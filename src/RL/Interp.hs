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
  case varlookup id ast of
    Just v  -> return v
    Nothing -> logError $ "Variable '" ++ id ++ "' not defined."

logStmt :: Stmt -> VarState ()
logStmt Skip = tell [Stmt Skip]
logStmt s = do
  tell [Stmt s]
  exec s
  vtab <- get
  tell [State vtab]

logError :: Error -> VarState a
logError err = do
  tell [Error err]
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
  tell [NewBlock l]
  ast <- ask
  case blklookup l ast of
    Just (_,ss,t) -> do
      lift $ execStmts ss
      tell [EndOfBlock t]
      case t of
        Exit       -> return ()
        Goto l     -> interp l
        IfTo t l1 l2 -> do
          t' <- lift $ valToBool <$> eval t
          if t' then interp l1 else interp l2
    Nothing -> lift $ logError ("Label '" ++ l ++ "' not defined.")


-- ==========
-- Statements
-- ==========

execStmts :: [Stmt] -> VarState ()
execStmts = mapM_ logStmt

exec :: Stmt -> VarState ()

-- variable updates
exec (Update id op e) = do
  v1 <- rd id
  v2 <- eval e
  case op of
    DivEq -> case (v1,v2) of
      (IntV n, IntV m) | mod n m == 0 -> return ()
        | otherwise -> logError "Division has rest in update."
    MultEq -> case (v1,v2) of
      (IntV n, IntV m) | n /= 0 && m /= 0 -> return ()
        | otherwise -> logError "An operand in mult update is zero."
    _      -> return ()
  res <- eval $ mapUpdOp op (Lit v1) (Lit v2)
  modify $ insert id res

-- control flow
-- exec (If t s1 s2 a) = do
--   t' <- valToBool <$> eval t
--   if t' then exec s1
--         else exec s2
--   when (t' /= valToBool <$> eval a)
--     $ logError TestAndAssertNotCoherent
--
-- exec (DoUntil d a s t) = do -- if d is true we are coming from the outside
--   if (valToBool <$> eval a == d) then exec s
--   else logError AssertNotConsistent
--   when (valToBool <$> eval t) $ exec (DoUntil False a s t)

-- list modification
exec (Push id1 id2) = do
  v1 <- rd id1
  v2 <- rd id2
  case v2 of
    ListV ls -> do
      modify $ adjust clear id1
      modify $ adjust (push v1) id2
    where push v (ListV ls) = ListV $ v : ls
          clear (IntV _)  = IntV 0
          clear (ListV _) = ListV []

exec (Pop id1 id2) = do
  v1 <- rd id1
  unless (isClear v1) $ logError ("Popping into non-clear variable '" ++ id1 ++ "'.")
  v2 <- rd id2
  case v2 of
    ListV (t:ls) -> do
      modify $ insert id1 t
      modify $ insert id2 $ ListV ls
    ListV [] -> logError $ "Popping from empty list '" ++ id2 ++ "'."

-- swapping variables
exec (Swap id1 id2) = do
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
eval (Lit v)  = return v
eval (Var id) = rd id

-- binary arithmetic
eval (ABinary op l r) = applyABinOp (mapABinOp op) <$> eval l <*> eval r

-- div variations
eval (DivBinary op l r) = do
  vr <- eval r
  case vr of
    IntV 0 -> logError "Dividing by zero."
    IntV _ -> return ()
  flip (applyABinOp (mapDivOp op)) vr <$> eval l

-- unary arithmetic
eval (AUnary op e) = applyAUnOp (mapAUnOp op) <$> eval e

-- relational
eval (Relational op l r) = applyROp (mapROp op) <$> eval l <*> eval r

-- binary logical
eval (LBinary op l r) | b <- mapLBinOp op = eval l >>= \case
  IntV p | p == b -> return $ IntV b
  IntV _ -> norm <$> eval r

-- unary logical
eval (Not e) = --eval e >>= \(IntV p) -> IntV (boolToInt . not . intToBool $ p)
  applyABinOp xor (IntV 1) <$> (norm <$> eval e)

-- list expressions
eval (LstExp Top e) = do
  v <- eval e
  case v of
    ListV []     -> logError "Accessing top of empty list."
    ListV (t:ts) -> return t
eval (LstExp op e) = do
  let f = case op of
        Empty -> boolToVal . null
        Size  -> intToVal . length
  eval e >>= \(ListV ls) -> return $ f ls

-- paranthesis
eval (Parens e) = eval e

-- =======
-- helpers
-- =======

-- apply arithmetic binary operator
applyABinOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
applyABinOp op (IntV n) (IntV m) = IntV $ op n m
-- apply arithmetic unary operator
applyAUnOp :: (Integer -> Integer) -> Value -> Value
applyAUnOp op (IntV n) = IntV $ op n
-- apply relational operator
applyROp :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
applyROp op (IntV n) (IntV m) = boolToVal $ op n m

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
