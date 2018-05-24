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
rd (Id id exps) p = gets (mLookup id) >>= \case
  Just v  -> foldM (\acc e -> getIdx (getExpPos e) acc e) v exps
  Nothing -> logError $ RuntimeError p $ NonDefinedId id

getIdx :: Pos -> Value -> Exp -> VarState Value
getIdx p (IntV n) _ = logError $ RuntimeError p IndexOnNonListExp
getIdx p (ListV lst _) idx = eval idx >>= \case
  IntV i | i < 0     -> logError $ RuntimeError (getExpPos idx) NegativeIndex
    | otherwise -> case index lst i of
      Just v  -> return v
      Nothing -> logError $ RuntimeError (getExpPos idx) IndexOutOfBounds
  w -> logError $ RuntimeError p NonIntegerIndex
  where index :: [Value] -> Integer -> Maybe Value
        index lst i = if fromIntegral i >= length lst then Nothing else Just $ lst !! fromIntegral i

logStmt :: Stmt -> VarState ()
logStmt s = do
    exec s
    msg <- gets (MsgStmt s)
    tell [msg]

logError :: Error -> VarState a
logError err = do
  tell [MsgError err]
  throwError err

adjust :: (Value -> Value) -> Id -> Pos -> VarState ()
adjust op (Id id []) p = modify $ M.adjust op id
adjust op (Id id exps) p = do
  v <- rd (Id id []) p
  mn <- adjust' op exps p v
  modify $ M.insert id mn

adjust' :: (Value -> Value) -> [Exp] -> Pos -> Value -> VarState Value
adjust' op [] p vo = return $ op vo
adjust' op (e:es) p vo = do
  v <- getIdx p vo e
  vi <- adjust' op es p v
  case vo of
    ListV lst t -> eval e >>= \case
      IntV i -> return $ ListV (replace lst i vi) t
      w     -> logError $ RuntimeError (getExpPos e) NonIntegerIndex
    _ -> logError $ RuntimeError (getExpPos e) IndexOnNonListExp
  where replace :: [Value] -> Integer -> Value -> [Value]
        replace lst i v | i' <- fromIntegral i = take i' lst ++ [v] ++ drop (i' + 1) lst

-- ==========
-- Statements
-- ==========
exec :: Stmt -> VarState ()

-- variable updates
exec (Update (Id id exps) op e p) = do
  cont <- contains e (Id id exps) []
  when cont $ logError $ RuntimeError p $ SelfAbuse (Id id exps)

  v1 <- rd (Id id exps) p
  n <- case v1 of
    IntV n -> return n
    w      -> logError $ RuntimeError p $ UpdateOnNonIntager (Id id exps) (getType w)

  v2 <- eval e
  m <- case v2 of
    IntV m -> return m
    w      -> logError $ RuntimeError p $ NonIntegerExp (getType w)

  case op of
    DivEq  | m == 0       -> logError $ RuntimeError p DivByZero
           | mod n m /= 0 -> logError $ RuntimeError p DivHasRest
           | otherwise    -> return ()
    MultEq | m == 0       -> logError $ RuntimeError p MultByZero
           | otherwise    -> return ()
    _ -> return ()

  res <- eval $ mapUpdOp op (Lit v1 p) (Lit v2 p) p
  adjust (const res) (Id id exps) p

  where contains :: Exp -> Id -> [Exp] -> VarState Bool
        contains Lit{} _ _ = return False
        contains (Var (Id id2 exps2) _) (Id id1 exps1) is
          | id1 == id2 = let exps2' = exps2 ++ is in
            (&&) (length exps1 == length exps2') <$>
            ( (==) <$> mapM eval exps1 <*> mapM eval exps2' )
          | otherwise = return False
        contains (Binary _ e1 e2 _) id is = (||) <$> contains e1 id is <*> contains e2 id is
        contains (Unary Top e p) (Id id exps) is = do
          id' <- getIdentifier (Unary Top e p)
          contains (Var id' p) (Id id exps) []
        contains (Unary Size _ _) _ _ = return False
        contains (Unary _ e _) id is = contains e id is
        contains (Parens e _) id is = contains e id is
        ----------------------------------------------
        getIdentifier :: Exp -> VarState Id
        getIdentifier (Var id p) = return id
        getIdentifier (Unary Top exp p) = do
          (Id id' exps') <- getIdentifier exp
          (ListV l _)    <- rd (Id id' exps') p
          return (Id id' (exps' ++ [Lit (IntV . fromIntegral $ length l - 1) p]))
        getIdentifier (Parens e p) = getIdentifier e
        getIdentifier e = return (Id (show e) [])




-- list modification
exec (Push id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p
  case v2 of
    ListV ls (ListT t)
      | t == getType v1 -> do
        adjust clear id1 p
        adjust (push v1) id2 p
      | otherwise -> logError $ RuntimeError p $ ConflictingType t (getType v1)
    _ -> logError $ RuntimeError p $ PushToNonList id2
  where push v (ListV ls t) = ListV (ls++[v]) t
        clear (IntV _)      = IntV 0
        clear (ListV _ t)   = ListV [] t

exec (Pop id1 id2 p) = do
  v1 <- rd id1 p
  unless (isClear v1) $
    logError $ RuntimeError p $ PopToNonEmpty id1

  v2 <- rd id2 p
  case v2 of
    ListV [] _ -> logError $ RuntimeError p $ PopFromEmpty id2
    ListV ls (ListT t)
      | t == getType v1 -> do
        adjust (const $ last ls) id1 p
        adjust (const $ ListV (init ls) (ListT t)) id2 p
      | otherwise ->
        logError $ RuntimeError p $ ConflictingType t (getType v1)
    _  -> logError $ RuntimeError p $ PopFromNonList id2

-- swapping variables
exec (Swap id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p

  unless (getType v1 == getType v2)
    $ logError $ RuntimeError p $ ConflictingType (getType v1) (getType v2)

  adjust (const v2) id1 p >> adjust (const v1) id2 p

-- initialising a list
exec (Init id exps p) = do
  v <- rd (Id id []) p

  case v of
    IntV _ -> logError $ RuntimeError p $ InitOnNonList id
    ListV ls t
      | getDim t /= length exps ->
        logError $ RuntimeError p ConflictingDimensions
    _ -> return ()

  unless (isClear v) $ logError $ RuntimeError p $ InitNonEmptyList id

  nv <- foldrM repl (IntV 0) exps

  adjust (const nv) (Id id []) p

  where

    foldrM f e = foldr ((=<<) . f) (return e)

    repl e acc = eval e >>= \case
      IntV n
        | n >= 0 -> case acc of
          ListV ls t  -> return $ ListV (replicate (fromIntegral n) acc) (ListT t)
          IntV _      -> return $ ListV (replicate (fromIntegral n) acc) (ListT IntT)
        | otherwise -> logError $ RuntimeError (getExpPos e) NegativeDimension
      ListV _ t  -> logError $ RuntimeError (getExpPos e) $ NonIntegerDimension t -- TODO: Maybe List t

-- freeing a list
exec (Free id exps p) = do
  v <- rd (Id id []) p

  case v of
    IntV _ -> logError $ RuntimeError p $ FreeOnNonList id
    ListV ls t
      | getDim t /= length exps ->
        logError $ RuntimeError p ConflictingDimensions
    _ -> return ()

  unless (allZero v) $ logError $ RuntimeError p $ FreeNonEmptyList id

  eql <- equalLengths v exps
  unless eql
    $ logError $ RuntimeError p ConflictingLengths

  adjust (const . getDefaultValue . getType $ v) (Id id []) p

  where

    equalLengths v (e:exps) = eval e >>= \case
      IntV n
        | n >= 0 -> case v of
          ListV ls t  -> (&&) (length ls == fromIntegral n) <$> allM (`equalLengths` exps) ls
          IntV _      -> logError $ RuntimeError p $ FreeOnNonList id
        | otherwise -> logError $ RuntimeError (getExpPos e) NegativeDimension
      ListV _ t -> logError $ RuntimeError (getExpPos e) $ NonIntegerDimension t -- Maybe ListT t
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
eval (Var id p) = rd id p

eval (Binary op l r p)

  -- binary arithmetic and relational
  | op <= Geq  = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m) -> return $ IntV (mapBinOp op n m)
      (v,w)            -> logError $ RuntimeError p $ ConflictingTypes [IntT,IntT] [getType v, getType w]

  -- binary div and mod
  | op <= Mod = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m)
        | m == 0    -> logError $ RuntimeError (getExpPos l) DivByZero
        | otherwise -> return $ IntV (mapBinOp op n m)
      (v,w)         -> logError $ RuntimeError (getExpPos l) $ ConflictingTypes [IntT,IntT] [getType v, getType w]

  -- binary logical
  | otherwise = eval l >>= \case
    IntV 0 | op==And        -> return $ IntV 0
    IntV v | v/=0 && op==Or -> return $ IntV 1
    IntV _ -> eval r >>= \case
        IntV n -> return $ IntV (norm n)
        w      -> logError $ RuntimeError p $ NonIntegerExp (getType w)
    w -> logError $ RuntimeError p $ NonIntegerExp (getType w)

  -- unary arithmetic
eval (Unary op exp p)
  | op <= Sign = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    w      -> logError $ RuntimeError p $ NonIntegerExp (getType w)

  -- unary logical
  | op < Size = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    w      -> logError $ RuntimeError p $ NonIntegerExp (getType w)

  | op == Null = IntV . boolToInt . allZero <$> eval exp
  -- unary list
  | otherwise = eval exp >>= \case
    ListV ls t -> case op of
      Top   -> case ls of
        []    -> logError $ RuntimeError p EmptyTop
        ls    -> return $ last ls
      Empty -> return $ IntV (boolToInt . null $ ls)
      Size  -> return $ IntV (fromIntegral . length $ ls)
    w  -> logError $ RuntimeError p $ NonListExp (getType w)

-- parantheses
eval (Parens e p) = eval e

allZero :: Value -> Bool
allZero v = case v of
  ListV ls _ -> foldl (\acc e -> acc && allZero e) True ls
  IntV  n    -> n == 0
