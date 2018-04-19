module RL.Interp (module RL.Interp, module RL.AST) where

import RL.Error
import RL.AST

import Data.Bits (xor)
import Data.List (intercalate)

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- ==================
-- Monad transformers
-- ==================

-- ProgState is the outer state that must read from AST
type ProgState = ReaderT AST VarState
type VarState  = StateT VarTab (ExceptT Error (Writer Log))
rd :: Id -> VarState Value
rd id  = do
  ast <- get
  case lookup id ast of
    Just v  -> return v
    Nothing -> logError $ Debug $ "Variable '" ++ id ++ "' not defined."

logStmt :: Stmt -> VarState ()
logStmt (Skip p) = tell [MsgStmt (Skip p)]
logStmt s = do
  tell [MsgStmt s]
  exec s
  vtab <- get
  tell [MsgState vtab]

logError :: Error -> VarState a
logError err = do
  tell [MsgError err]
  throwError err


-- ==================
-- Running the program
-- ==================

runProgram :: AST -> (Either Error VarTab, Log)
runProgram ast = do
  let entry = getEntry  ast
      vtab  = buildVTab ast
  runWriter . runExceptT . flip execStateT vtab . runReaderT (interp entry) $ ast

-- ======
-- Blocks
-- ======

interp :: Label -> ProgState ()
interp l = do
  tell [MsgNewBlock l]
  ast <- ask
  case lookup l ast of
    Just (_,ss,t) -> do
      lift $ execStmts ss
      tell [MsgEndOfBlock t]
      case t of
        Exit       -> return ()
        Goto l     -> interp l
        IfTo t l1 l2 -> do
          t' <- lift $ valToBool <$> eval t
          if t' then interp l1 else interp l2
    Nothing -> lift $ logError $ Debug $ ("Label '" ++ l ++ "' not defined.")


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
        | otherwise -> logError $ Debug $ "Division has rest in update."
    MultEq -> case (v1,v2) of
      (IntV n, IntV m) | n /= 0 && m /= 0 -> return ()
        | otherwise -> logError $ Debug $ "An operand in mult update is zero."
    _      -> return ()
  res <- eval $ mapUpdOp op (Lit v1 p) (Lit v2 p) p
  modify $ insert id res

-- control flow
-- exec (If t s1 s2 a p) = do
--   t' <- valToBool <$> eval t
--   if t' then exec s1
--         else exec s2
--   when (t' /= valToBool <$> eval a)
--     $ logError TestAndAssertNotCoherent
--
-- exec (DoUntil d a s t p) = do -- if d is true we are coming from the outside
--   if (valToBool <$> eval a == d) then exec s
--   else logError AssertNotConsistent
--   when (valToBool <$> eval t) $ exec (DoUntil False a s t)

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
  unless (isClear v1) $ logError $ Debug $ ("Popping into non-clear variable '" ++ id1 ++ "'.")
  v2 <- rd id2
  case v2 of
    ListV (t:ls) -> do
      modify $ insert id1 t
      modify $ insert id2 $ ListV ls
    ListV [] -> logError $ Debug $ "Popping from empty list '" ++ id2 ++ "'."

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
                     | op <= Mod = eval r >>= \rv -> case rv of
  IntV 0 -> logError $ Debug $ "Dividing by zero."
  IntV _ -> eval l >>= \lv -> return $ applyABinOp (mapABinOp op) lv rv
-- binary relational
                     | op <= Geq = applyRBinOp (mapRBinOp op) <$> eval l <*> eval r
-- binary logical
                     | otherwise = eval l >>= \case
  IntV 0 | op==And        -> return $ IntV 0
  IntV v | v/=0 && op==Or -> return $ IntV 1
  IntV _ -> norm <$> eval r
-- unary arithmetic
eval (Unary op exp p) | op <= Sign  = eval exp >>= \v -> return $ applyAUnOp (mapAUnOp op) v
-- unary logical
                    | op < Size    = eval exp >>= \(IntV v) -> return $ boolToVal $ (mapLUnOp op) $ v/=0
-- unary list
                    | otherwise   = eval exp >>= \(ListV lv) -> case op of
  Top   -> case lv of
    []   -> logError $ Debug $ "Accessing top of empty list."
    t:ts -> return t
  Empty -> return $ boolToVal . null  $ lv
  Size  -> return $ intToVal . length $ lv

-- paranthesis
eval (Parens e p) = eval e

-- =======
-- helpers
-- =======
mapABinOp Plus    = (+)
mapABinOp Minus   = (-)
mapABinOp Xor     = xor
mapABinOp Pow     = (^)
mapABinOp Mult    = (*)
mapABinOp Div     = div
mapABinOp Mod     = mod

mapRBinOp Equal   = (==)
mapRBinOp Neq     = (/=)
mapRBinOp Less    = (<)
mapRBinOp Leq     = (<=)
mapRBinOp Greater = (>)
mapRBinOp Geq     = (>=)

mapAUnOp Neg  = negate
mapAUnOp Sign = signum

mapLUnOp Not  = not

-- apply arithmetic binary operator
applyABinOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
applyABinOp op (IntV n) (IntV m) = IntV $ op n m
-- apply relational operator
applyRBinOp :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
applyRBinOp op (IntV n) (IntV m) = boolToVal $ op n m
-- apply arithmetic unary operator
applyAUnOp :: (Integer -> Integer) -> Value -> Value
applyAUnOp op (IntV n) = IntV $ op n

-- normalise to bool
norm :: Value -> Value
norm (IntV 0) = IntV 0
norm (IntV _) = IntV 1

-- converting bool to val
boolToVal :: Bool -> Value
boolToVal b = IntV $ if b then 1 else 0
-- converting val to bool
valToBool :: Value -> Bool
valToBool (IntV p) = p /= 0
-- int to val
intToVal :: Int -> Value
intToVal = IntV . fromIntegral
