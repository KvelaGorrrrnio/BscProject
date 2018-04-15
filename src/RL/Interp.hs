{-# LANGUAGE FlexibleContexts #-}

module Interp (module Interp, module AST) where

import Prelude hiding (log)
import Error
import AST

import Data.Bits (xor)
import Data.List (intercalate)
-- skal væk med hardcodede ast'er

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
logStmt Skip = log $ Stmt Skip
logStmt s = do
  log $ Stmt s
  exec s
  vtab <- get
  log $ State vtab

logError :: Error -> VarState a
logError err = do
  log $ Error err
  throwError err

log :: MonadWriter Log m => Message -> m ()
log msg = tell [msg]

-- ==================
-- Running the program
-- ==================

runProgram :: Label -> VarTab -> AST -> (Either Error VarTab, Log)
runProgram entry vtab =
  runWriter . runExceptT . flip execStateT vtab . runReaderT (interp entry)

-- ======
-- Blocks
-- ======

interp :: Label -> ProgState ()
interp l = do
  log $ NewBlock l
  ast <- ask
  case blklookup l ast of
    Just (Block (_,ss,t)) -> do
      lift $ execStmts ss
      log $ EndOfBlock t
      case t of
        Exit       -> return ()
        Goto l     -> interp l
        If t l1 l2 -> do
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

-- sequence of statements
-- exec (Seq ss) = foldr ((>>) . exec) (return ()) ss

-- skip and rest
exec _ = return ()


-- ===========
-- Expressions
-- ===========

eval :: Exp -> VarState Value

-- terminals
eval (Lit v)  = return v
eval (Var id) = rd id

-- binary arithmetic    - probably single group
eval (ABinary op l r) = applyABinOp (mapABinOp op) <$> eval l <*> eval r

-- div variations       - probably their own group
eval (DivBinary op l r) = do
  vr <- eval r
  case vr of
    IntV 0 -> logError "Dividing by zero."
    IntV _ -> return ()
  flip (applyABinOp (mapDivOp op)) vr <$> eval l

-- unary arithmetic      - probably single group
eval (AUnary op e) = applyAUnOp (mapAUnOp op) <$> eval e

-- relational            - also single group
eval (Relational op l r) = applyROp (mapROp op) <$> eval l <*> eval r

-- binary logical      - short cirquiting does not allow grouping
eval (LBinary op l r) = boolToVal <$> (mapLBinOp op <$> (valToBool <$> eval l) <*> (valToBool <$> eval r))

-- unary logical         - only one
eval (Not e) = --eval e >>= \(IntV p) -> IntV (boolToInt . not . intToBool $ p)
  applyABinOp xor (IntV 1) <$> (norm <$> eval e)

-- list expressions    - two of them can be grouped together
eval (Top e) = do
  v <- eval e
  case v of
    ListV []     -> logError "Accessing top of empty list."
    ListV (t:ts) -> return t
eval (Empty e) = eval e >>= \(ListV ls) -> return $ boolToVal (null ls)
eval (Size e)  = eval e >>= \(ListV ls) -> return $ IntV ((fromIntegral . length) ls)

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

-- vtab = VarTab $ M.fromList [("n", IntV 0), ("v", IntV 0), ("w", IntV 0)]
-- ast  = AST $ M.fromList [
--       ("init", Block (Entry,
--         [
--           Update "n" PlusEq (Lit $ IntV 70)
--         , Update "w" XorEq (Lit $ IntV 1)
--         ],
--         Goto "loop"))
--
--     , ("loop", Block (Fi (Relational Eq (Var "v") (Lit $ IntV 0)) "init" "loop",
--         [
--           Update "v" PlusEq (Var "w")
--         , Swap "v" "w"
--         , Update "n" MinusEq (Lit $ IntV 1)
--         ],
--         If (Var "n") "loop" "end"))
--
--     , ("end", Block (From "loop",
--         [Skip],
--         Exit))
--     ]
-- vtab = VarTab $ M.fromList [("n", IntV 0), ("v", IntV 0)]
-- ast  = AST $ M.fromList [
--       ("init", Block (Entry,
--         [Update "n" PlusEq (Lit $ IntV 32)],
--         Goto "loop"))
--
--     , ("loop", Block (Fi (Relational Eq (Var "v") (Lit $ IntV 0)) "init" "loop",
--         [
--           Update "v" PlusEq (Var "n")
--         , Update "n" MinusEq (Lit $ IntV 1)
--         ],
--         If (Var "n") "loop" "end"))
--
--     , ("end", Block (From "loop",
--         [Skip],
--         Exit))
--     ]
