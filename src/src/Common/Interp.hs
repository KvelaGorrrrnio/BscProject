{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Common.Interp (module Common.Interp, module Common.Log) where

import Common.Error
import Common.Log
import Common.AST

import Data.Bits (xor)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Loops (allM)

import Debug.Trace

import qualified Data.HashMap.Strict as M

-- ======================================
-- Monad transformer : The variable state
-- ======================================

type VarState = StateT VarTab (ExceptT Error (Writer [Message]))
execVarState :: VarTab -> VarState () -> (Either Error VarTab, [Message])
execVarState vtab = runWriter . runExceptT . flip execStateT vtab

rd :: Id -> Pos -> VarState Value
rd (Id id exps) p = gets (M.lookup id) >>= \case
  Just v  -> foldM getIdx v exps
  Nothing -> logError p $ NonDefinedId id

getIdx :: Value -> Exp -> VarState Value
getIdx (IntV n) idx = logError (getExpPos idx) IndexOnNonListExp
getIdx (ListV lst _) idx = eval idx >>= \case
  IntV i | i < 0     -> logError (getExpPos idx) NegativeIndex
    | otherwise -> case index lst i of
      Just v  -> return v
      Nothing -> logError (getExpPos idx) IndexOutOfBounds
  w -> logError (getExpPos idx) NonIntegerIndex
  where index :: [Value] -> Integer -> Maybe Value
        index lst i = if fromIntegral i >= length lst then Nothing else Just $ lst !! fromIntegral i

logStep :: Step -> VarState ()
logStep s = do
    exec s
    msg <- gets (MsgStep s)
    tell [msg]

logError :: Pos -> RuntimeError -> VarState a
logError p err' | err <- RuntimeError p err' = do
  tell [MsgError err]
  throwError err

adjust :: (Value -> Value) -> Id -> Pos -> VarState ()
adjust op (Id id []) _ = modify $ M.adjust op id
adjust op (Id id exps) p = do
  v <- rd (Id id []) p
  mn <- adjust' op exps v
  modify $ M.insert id mn

adjust' :: (Value -> Value) -> [Exp] -> Value -> VarState Value
adjust' op [] vo = return $ op vo
adjust' op (e:es) vo = do
  v <- getIdx vo e
  vi <- adjust' op es v
  case vo of
    ListV lst t -> eval e >>= \case
      IntV i -> return $ ListV (replace lst i vi) t
      w     -> logError (getExpPos e) NonIntegerIndex
    _ -> logError (getExpPos e) IndexOnNonListExp

  where replace :: [Value]  -> Integer -> Value -> [Value]
        replace (_:vs) 0 nv = nv:vs
        replace (v:vs) i nv = v : replace vs (i-1) nv

-- ==========
-- Statements
-- ==========
exec :: Step -> VarState ()

-- variable updates
exec (Update (Id id exps) op e p) = do
  cont <- contains e (Id id exps) []
  when cont $ logError p $ SelfAbuse (Id id exps)

  v1 <- rd (Id id exps) p
  n <- case v1 of
    IntV n -> return n
    w      -> logError p $ UpdateOnNonInteger (Id id exps) (getType w)

  v2 <- eval e
  m <- case v2 of
    IntV m -> return m
    w      -> logError p $ NonIntegerExp (getType w)

  case op of
    DivEq  | m == 0       -> logError p DivByZero
           | mod n m /= 0 -> logError p DivHasRest
           | otherwise    -> return ()
    MultEq | m == 0       -> logError p MultByZero
           | otherwise    -> return ()
    _ -> return ()

  res <- eval $ mapUpdOp op (Lit v1 p) (Lit v2 p) p
  adjust (const res) (Id id exps) p

  where contains :: Exp -> Id -> [Exp] -> VarState Bool
        contains Lit{} _ _ = return False
        contains (Var id2 _) (Id id1 exps1) exps2
          | id1 == id2 = (&&) (length exps1 == length exps2) <$>
                         ( (==) <$> mapM eval exps1 <*> mapM eval exps2 )
          | otherwise = return False
        contains (Binary _ e1 e2 _) id is = (||) <$> contains e1 id is <*> contains e2 id is
        contains (Unary Top e p) id is    = contains e id (Binary Minus (Unary Size e p) (Lit (IntV 1) p) p : is)
        contains (Index l exps _) id is   = contains l id (exps ++ is)
        contains (Unary Size _ _) _ _     = return False
        contains (Unary _ e _) id is      = contains e id is
        contains (Parens e _) id is       = contains e id is

-- list modification
exec (Push id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p
  case v2 of
    ListV ls (ListT t)
      | t == getType v1 -> do
        adjust clear id1 p
        adjust (push v1) id2 p
      | otherwise -> logError p $ ConflictingType t (getType v1)
    _ -> logError p $ PushToNonList id2
  where push v (ListV ls t) = ListV (ls++[v]) t
        clear (IntV _)      = IntV 0
        clear (ListV _ t)   = ListV [] t

exec (Pop id1 id2 p) = do
  v1 <- rd id1 p
  unless (isClear v1) $
    logError p $ PopToNonEmpty id1

  v2 <- rd id2 p
  case v2 of
    ListV [] _ -> logError p $ PopFromEmpty id2
    ListV ls (ListT t)
      | t == getType v1 -> do
        adjust (const $ last ls) id1 p
        adjust (const $ ListV (init ls) (ListT t)) id2 p
      | otherwise ->
        logError p $ ConflictingType t (getType v1)
    _  -> logError p $ PopFromNonList id2

-- swapping variables
exec (Swap id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p

  let t1 = getType v1
      t2 = getType v2
  unless (t1 == t2)
    $ logError p $ SwapNotSameType t1 t2

  adjust (const v2) id1 p >> adjust (const v1) id2 p

-- initialising a list
exec (Init id exps p) = do
  when (any (`dimContain` id) exps) $ logError p $ DimSelfAbuse id

  v <- rd (Id id []) p

  case v of
    IntV _ -> logError p $ InitOnNonList id
    ListV ls t
      | getDim t /= length exps ->
        logError p ConflictingDimensions
    _ -> return ()

  unless (isClear v) $ logError p $ InitNonEmptyList id

  nv <- foldrM repl (IntV 0) exps

  adjust (const nv) (Id id []) p

  where

    foldrM f e = foldr ((=<<) . f) (return e)

    repl e acc = eval e >>= \case
      IntV n
        | n >= 0 -> case acc of
          ListV ls t  -> return $ ListV (replicate (fromIntegral n) acc) (ListT t)
          IntV _      -> return $ ListV (replicate (fromIntegral n) acc) (ListT IntT)
        | otherwise -> logError (getExpPos e) NegativeDimension
      ListV _ t  -> logError (getExpPos e) $ NonIntegerDimension t

-- freeing a list
exec (Free id exps p) = do
  when (any (`dimContain` id) exps) $ logError p $ DimSelfAbuse id

  v <- rd (Id id []) p

  case v of
    IntV _ -> logError p $ FreeOnNonList id
    ListV ls t
      | getDim t /= length exps ->
        logError p ConflictingDimensions
    _ -> return ()

  unless (allZero v) $ logError p $ FreeNonEmptyList id

  eql <- equalLengths v exps
  unless eql
    $ logError p ConflictingLengths

  adjust (const . getDefaultValue . getType $ v) (Id id []) p

  where

    equalLengths v (e:exps) = eval e >>= \case
      IntV n
        | n >= 0 -> case v of
          ListV ls t  -> (&&) (length ls == fromIntegral n) <$> allM (`equalLengths` exps) ls
          IntV _      -> logError p $ FreeOnNonList id
        | otherwise -> logError (getExpPos e) NegativeDimension
      ListV _ t -> logError (getExpPos e) $ NonIntegerDimension t -- Maybe ListT t
    equalLengths ListV{} [] = return False
    equalLengths IntV{}  [] = return True

-- skip
exec _ = return ()

getDim t = case t of
  ListT t -> 1 + getDim t
  IntT    -> 0


-- ===========
-- Expressions
-- ===========

eval :: Exp -> VarState Value

-- terminals
eval (Lit v _)  = return v
eval (Var id p) = rd (Id id []) p

eval (Binary op l r p)

  -- binary arithmetic and relational
  | op <= Geq  = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m) -> return $ IntV (mapBinOp op n m)
      (ListV ls1 t1, ListV ls2 t2)
        | op == Equal && t1 == t2 -> return $ IntV (boolToInt $ ls1==ls2)
        | op == Neq   && t1 == t2 -> return $ IntV (boolToInt $ ls1/=ls2)
      (v,w) -> logError p $ ConflictingTypes [IntT,IntT] [getType v, getType w]

  -- binary div and mod
  | op <= Mod = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m)
        | m == 0    -> logError (getExpPos l) DivByZero
        | otherwise -> return $ IntV (mapBinOp op n m)
      (v,w)         -> logError (getExpPos l) $ ConflictingTypes [IntT,IntT] [getType v, getType w]

  -- binary logical
  | otherwise = eval l >>= \case
    IntV 0 | op==And        -> return $ IntV 0
    IntV v | v/=0 && op==Or -> return $ IntV 1
    IntV _ -> eval r >>= \case
        IntV n -> return $ IntV (if n==0 then 0 else 1)
        w      -> logError p $ NonIntegerExp (getType w)
    w -> logError p $ NonIntegerExp (getType w)

eval (Unary op exp p)

  -- unary arithmetic
  | op <= Sign = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    w      -> logError p $ NonIntegerExp (getType w)

  -- unary logical
  | op <= Not = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    w      -> logError p $ NonIntegerExp (getType w)

  -- unary list
  | op == Null = IntV . boolToInt . allZero <$> eval exp
  | otherwise = eval exp >>= \case
    ListV ls t -> case op of
      Top   -> case ls of
        []    -> logError p EmptyTop
        ls    -> return $ last ls
      Empty -> return $ IntV (boolToInt . null $ ls)
      Size  -> return $ IntV (fromIntegral . length $ ls)
    w  -> logError p $ NonListExp (getType w)

-- index
eval (Index l exps _) = do
  v <- eval l
  foldM getIdx v exps

-- parantheses
eval (Parens e _) = eval e


-- =======
-- Helpers
-- =======

-- helper for null and free
allZero :: Value -> Bool
allZero v = case v of
  ListV ls _ -> foldl (\acc e -> acc && allZero e) True ls
  IntV  n    -> n == 0

-- helper for RL and SRL interp
checkCond :: Exp -> VarState Bool
checkCond e = eval e >>= \case
  IntV q -> return $ q/=0
  w      -> logError (getExpPos e) $ ConflictingType IntT (getType w)

-- free and init
dimContain :: Exp -> String ->  Bool
dimContain Lit{} _               = False
dimContain (Var id' _) id        = id' == id
dimContain (Binary _ e1 e2 _) id = e1 `dimContain` id || e2 `dimContain` id
dimContain (Unary Top e p) id    = e  `dimContain` id
dimContain (Index e _ _) id      = e  `dimContain` id
dimContain (Unary Size e _) id   = e  `dimContain` id
dimContain (Unary _ e _) id      = e  `dimContain` id
dimContain (Parens e _) id       = e  `dimContain` id
