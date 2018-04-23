{-# LANGUAGE LambdaCase #-}

module Common.Interp (module Common.Interp, module Common.Log) where

import Common.Error
import Common.Log
import Common.AST

import Data.Bits (xor)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Loops

-- ======================================
-- Monad transformer : The variable state
-- ======================================

type VarState = StateT VarTab (ExceptT RuntimeError (Writer Log))
execVarState :: VarTab -> VarState () -> (Either RuntimeError VarTab, Log)
execVarState vtab = runWriter . runExceptT . flip execStateT vtab

rd :: Id -> VarState Value
rd id  = do
  ast <- get
  case lookup id ast of
    Just v  -> return v
    Nothing -> throwError $ CustomRT ("Variable '" ++ id ++ "' not defined.")

logStmt :: Stmt -> VarState ()
logStmt s = case s of
  Skip{}  -> tell [MsgStmt s]
  If{}    -> tell [MsgStmt s] >> exec s
  Until{} -> tell [MsgStmt s] >> exec s
  s    -> do
    tell [MsgStmt s] >> exec s
    vtab <- get
    tell [MsgState vtab]

logError :: RuntimeError -> VarState a
logError err = do
  tell [MsgError err]
  throwError err


-- ==========
-- Statements
-- ==========

execStmts :: [Stmt] -> VarState ()
execStmts = mapM_ logStmt

exec :: Stmt -> VarState ()

-- variable updates
exec (Update id op e p) = do
  v1 <- rd id
  v2 <- eval e
  case op of
    DivEq -> case (v1,v2) of
      (IntV n, IntV m) | mod n m == 0 -> return ()
        | otherwise -> logError $ CustomRT "Division has rest in update."
    MultEq -> case (v1,v2) of
      (IntV n, IntV m) | n /= 0 && m /= 0 -> return ()
        | otherwise -> logError $ CustomRT "An operand in mult update is zero."
    _      -> return ()
  res <- eval $ mapUpdOp op (Lit v1 p) (Lit v2 p) p
  modify $ insert id res

-- control flow : unique for SRL
exec (If t s1 s2 a p) = do
  t' <- valToBool <$> eval t
  if t' then execStmts s1
        else execStmts s2
  a' <- valToBool <$> eval a
  when (t' /= a')
    $ logError $ CustomRT "Assert and such"

exec (Until a s t p) = do
  aout <- valToBool <$> eval a
  unless aout $ logError $ CustomRT "Assert"
  execStmts s
  whileM_ (not . valToBool <$> eval t) $ do
      ain <- valToBool <$> eval a ; when ain
        $ throwError $ CustomRT "Assert not good"
      execStmts s;

-- list modification
exec (Push id1 id2 p) = do
  v1 <- rd id1
  v2 <- rd id2
  case v2 of
    ListV ls -> do
      modify $ adjust clear id1
      modify $ adjust (push v1) id2
    where push v (ListV ls) = ListV $ v : ls
          clear (IntV _)  = IntV 0
          clear (ListV _) = ListV []

exec (Pop id1 id2 p) = do
  v1 <- rd id1
  unless (isClear v1) $
    logError $ CustomRT ("Popping into non-clear variable '" ++ id1 ++ "'.")
  v2 <- rd id2
  case v2 of
    ListV (t:ls) -> do
      modify $ insert id1 t
      modify $ insert id2 $ ListV ls
    ListV [] -> logError $ CustomRT $ "Popping from empty list '" ++ id2 ++ "'."

-- swapping variables
exec (Swap id1 id2 p) = do
  v1 <- rd id1
  v2 <- rd id2
  modify $ insert id1 v2
  modify $ insert id2 v1

-- skip
exec _ = return ()


-- ===========
-- Expressions
-- ===========

eval :: Exp -> VarState Value

-- terminals
eval (Lit v _)  = return v
eval (Var id _) = rd id

-- binary arithmetic
eval (Binary op l r p) | op < Div = applyABinOp (mapABinOp op) <$> eval l <*> eval r
-- binary div and mod
                     | op <= Mod = do
  rv <- eval r
  case rv of
    IntV 0 -> logError $ CustomRT "Dividing by zero."
    IntV _ -> return ()
  lv <- eval l
  return $ applyABinOp (mapABinOp op) lv rv
-- binary relational
                     | op <= Geq = applyRBinOp (mapRBinOp op) <$> eval l <*> eval r
-- binary logical
                     | otherwise = eval l >>= \case
  IntV 0 | op==And        -> return $ IntV 0
  IntV v | v/=0 && op==Or -> return $ IntV 1
  IntV _ -> norm <$> eval r
-- unary arithmetic
eval (Unary op exp p) | op <= Sign  = applyAUnOp (mapAUnOp op) <$> eval exp
-- unary logical
                      | op < Size   = eval exp >>= \(IntV v)  -> return $ boolToVal $ mapLUnOp op (v/=0)
-- unary list
                      | otherwise   = eval exp >>= \(ListV lv) -> case op of
  Top   -> case lv of
    []   -> logError $ CustomRT "Accessing top of empty list."
    t:ts -> return t
  Empty -> return $ boolToVal . null  $ lv
  Size  -> return $ IntV . fromIntegral . length $ lv

-- paranthesis
eval (Parens e p) = eval e
