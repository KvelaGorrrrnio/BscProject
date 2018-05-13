{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Common.Interp (module Common.Interp, module Common.Log) where

import Common.Error
import Common.Log
import Common.AST

import Data.Bits (xor)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap.Strict as I

-- debugging
import Debug.Trace

-- ======================================
-- Monad transformer : The variable state
-- ======================================

type VarState = StateT VarTab (ExceptT Error (Writer Log))
execVarState :: VarTab -> VarState () -> (Either Error VarTab, Log)
execVarState vtab = runWriter . runExceptT . flip execStateT vtab

rd :: Id -> Pos -> VarState Value
rd (Id id exps) p = gets (mLookup id) >>= \case
  Just v  -> foldM (getIdx p) v exps
  Nothing -> logError $ RuntimeError p $ CustomRT ("Variable '" ++ id ++ "' is not declared.")

getIdx :: Pos -> Value -> Exp -> VarState Value
getIdx p (IntV _) _ = logError $ RuntimeError p $ CustomRT "Too many indices."
getIdx p (ListV lst _) idx = eval idx >>= \case
  IntV i | i < 0     -> logError $ RuntimeError p $ CustomRT "Index must be non-negative."
    | otherwise -> case index lst i of
      Just v  -> return v
      Nothing -> logError $ RuntimeError p $ CustomRT "Index out of bounds."
  _ -> logError $ RuntimeError p $ CustomRT "Index must be an integer."
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

logMsg :: MonadWriter Log w => String -> w ()
logMsg st = tell [MsgCustom st]

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
      _     -> logError $ RuntimeError p $ CustomRT "Index must be an integer."
    _ -> logError $ RuntimeError p $ CustomRT "Too many indices."
  where replace :: [Value] -> Integer -> Value -> [Value]
        replace lst i v = take (fromIntegral i) lst ++ [v] ++ drop (fromIntegral (i + 1)) lst

-- ==========
-- Statements
-- ==========
exec :: Stmt -> VarState ()

-- variable updates
exec (Update (Id id exps) op e p) = do
  abuse <- contains e (Id id exps) []
  when abuse $ logError $ RuntimeError p $ CustomRT "Self-abuse in update."

  v1 <- rd (Id id exps) p
  n <- case v1 of
    IntV n -> return n
    _      -> logError $ RuntimeError p $ CustomRT "Attempting to update list variable."

  v2 <- eval e
  m <- case v2 of
    IntV m -> return m
    _      -> logError $ RuntimeError p $ CustomRT "Attempting to update variable with a list expression."

  case op of
    DivEq  | mod n m == 0 -> return ()
           | otherwise -> logError $ RuntimeError p $ CustomRT "Division has rest in update."
    MultEq | n /= 0 && m /= 0 -> return ()
           | otherwise -> logError $ RuntimeError p $ CustomRT "An operand in multiplication update is zero."
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
        contains (Unary Top e p) id is = contains e id (Lit (IntV 0) p : is)
        contains (Unary Size _ _) _ _ = return False
        contains (Unary _ e _) id is = contains e id is
        contains (Parens e _) id is = contains e id is

-- list modification
exec (Push id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p
  case v2 of
    ListV ls (ListT t)
      | t == getType v1 -> do
        adjust clear id1 p
        adjust (push v1) id2 p
      | otherwise ->
        logError $ RuntimeError p $ CustomRT "Types do not match" -- TODO: mere nøjagtig
    _ -> logError $ RuntimeError p $ CustomRT "Pushing onto non-list. " -- TODO: mere nøjagtig
    where push v (ListV ls t) = ListV (v:ls) t
          clear (IntV _)    = IntV 0
          clear (ListV _ t) = ListV [] t

exec (Pop id1 id2 p) = do
  v1 <- rd id1 p
  unless (isClear v1) $
    logError $ RuntimeError p $ CustomRT ("Popping into non-clear variable '" ++ show id1 ++ "'.")

  v2 <- rd id2 p
  case v2 of
    ListV [] _ -> logError $ RuntimeError p $ CustomRT $ "Popping from empty list '" ++ show id2 ++ "'."
    ListV (v:ls) (ListT t)
      | t == getType v1 -> do
        adjust (const v) id1 p
        adjust (const $ ListV ls (ListT t)) id2 p
      | otherwise ->
        logError $ RuntimeError p $ CustomRT "Types do not match" -- TODO: mere nøjagtig
    _  -> logError $ RuntimeError p $ CustomRT "Popping from non-list"

-- swapping variables
exec (Swap id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p

  unless (getType v1 == getType v2)
    $ logError $ RuntimeError p $ CustomRT "Variables being swapped must be of the same type."

  adjust (const v2) id1 p >> adjust (const v1) id2 p

-- skip
exec _ = return ()


-- ===========
-- Expressions
-- ===========

eval :: Exp -> VarState Value

-- terminals
eval (Lit v _)  = return v
eval (Var id p) = rd id p


  -- binary arithmetic and relational
eval (Binary op l r p)
  | op <= Geq  = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m) -> return $ IntV (mapBinOp op n m)
      _                -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- binary div and mod
  | op <= Mod = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m)
        | m == 0    -> logError $ RuntimeError p $ CustomRT "Dividing by zero."
        | otherwise -> return $ IntV (mapBinOp op n m)
      _             -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig
  -- binary logical
  | otherwise = eval l >>= \case
    IntV 0 | op==And        -> return $ IntV 0
    IntV v | v/=0 && op==Or -> return $ IntV 1
    IntV _ -> eval r >>= \case
        IntV n -> return $ IntV (norm n)
        _      -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig
    _ -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- unary arithmetic
eval (Unary op exp p)
  | op <= Sign = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    _      -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- unary logical
  | op < Size = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    _      -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- unary list
  | otherwise = eval exp >>= \case
    ListV ls t -> case op of
      Top   -> case ls of
        []    -> logError $ RuntimeError p $ CustomRT "Accessing top of empty list."
        v:_   -> return v
      Empty -> return $ IntV (boolToInt . null $ ls)
      Size  -> return $ IntV (fromIntegral . length $ ls)
    _  -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

-- parantheses
eval (Parens e p) = eval e
