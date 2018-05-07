{-# LANGUAGE FlexibleContexts, LambdaCase #-}

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

type VarState = StateT VarTab (ExceptT Error (Writer Log))
execVarState :: VarTab -> VarState () -> (Either Error VarTab, Log)
execVarState vtab = runWriter . runExceptT . flip execStateT vtab

rd :: Id -> Pos -> VarState Value
rd id p = gets (mLookup id) >>= \case
  Just v  -> return v
  Nothing -> logError $ RuntimeError p $ CustomRT ("Variable '" ++ id ++ "' is not defined.")

logStmt :: Stmt -> VarState ()
logStmt s = case s of
  If{}    -> exec s
  Until{} -> exec s
  _ -> do
    exec s
    msg <- gets (MsgStmt s)
    tell [msg]

logError :: Error -> VarState a
logError err = do
  tell [MsgError err]
  throwError err

logMsg :: MonadWriter Log w => String -> w ()
logMsg st = tell [MsgCustom st]


-- ==========
-- Statements
-- ==========

execStmts :: [Stmt] -> VarState ()
execStmts = mapM_ logStmt

exec :: Stmt -> VarState ()

-- variable updates
exec (Update id op e p) = do
  v1 <- rd id p
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
           | otherwise -> logError $ RuntimeError p $ CustomRT "An operand in mult update is zero."
    _ -> return ()

  res <- eval $ mapUpdOp op (Lit v1 p) (Lit v2 p) p
  modify $ insert id res

-- control flow : unique for SRL
exec (If t s1 s2 a p) = do
  q  <- eval t >>= \case
    IntV q -> return $ intToBool q
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  logMsg $ show (If t s1 s2 a p) ++ " -> " ++ if q then "true" else "false"
  execStmts $ if q then s1 else s2
  logMsg $ (if q then "[s1]" else "[s2]") ++ " done"

  r <- eval a >>= \case
    IntV r -> return $ intToBool r
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  when (q /= r)
    $ logError $ RuntimeError p $ CustomRT "Assert and such"

exec (Until d a s t p) = do -- log this
  logMsg $ show (Until d a s t p)

  q <- eval a >>= \case
    IntV q -> return $ intToBool q
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  unless (q == d) $ logError (RuntimeError p $ CustomRT "Assert")

  execStmts s

  r <- eval t >>= \case
    IntV r -> return $ intToBool r
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  unless r $ exec (Until False a s t p)

  logMsg $ show t ++ " -> " ++ "true"

  -- q <- eval a >>= \case
  --   IntV q -> return $ intToBool q
  --   _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  -- unless q $ logError $ RuntimeError p $ CustomRT "Assert"

  -- execStmts s

  -- whileM_ (cond t) $ do
  --     logMsg $ show t ++ " -> " ++ "false"
  --     r <- eval a >>= \case
  --       IntV r -> return $ intToBool r
  --       _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig
  --     when r $ throwError $ RuntimeError p $ CustomRT "Assert not good"
  --     execStmts s;

  --   where cond t = eval t >>= \case
  --                     IntV q -> not . intToBool $ q
  --                     _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig


-- list modification
exec (Push id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p
  case v2 of
    ListV ls (ListT t)
      | t == getType v1 -> do
        modify $ adjust clear id1
        modify $ adjust (push v1) id2
      | otherwise ->
        logError $ RuntimeError p $ CustomRT "Types do not match" -- TODO: mere nøjagtig
    where push v (ListV ls t) = ListV (v : ls) t
          clear (IntV _)    = IntV 0
          clear (ListV _ t) = ListV [] t

exec (Pop id1 id2 p) = do
  v1 <- rd id1 p
  unless (isClear v1) $
    logError $ RuntimeError p $ CustomRT ("Popping into non-clear variable '" ++ id1 ++ "'.")

  v2 <- rd id2 p
  case v2 of
    ListV [] _ -> logError $ RuntimeError p $ CustomRT $ "Popping from empty list '" ++ id2 ++ "'."
    ListV (e:ls) (ListT t)
      | t == getType v1 -> do
        modify $ insert id1 e
        modify $ insert id2 $ ListV ls (ListT t)
      | otherwise ->
        logError $ RuntimeError p $ CustomRT "Types do not match" -- TODO: mere nøjagtig

-- swapping variables
exec (Swap id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p
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
eval (Var id p) = rd id p

  -- binary arithmetic and relational
eval (Binary op l r p)
  | op < Geq  = do
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
        e:es  -> return e
      Empty -> return $ IntV (boolToInt . null $ ls)
      Size  -> return $ IntV (fromIntegral . length $ ls)
    _  -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

-- parantheses
eval (Parens e p) = eval e
