{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (log)

import Data.List (intercalate)
import Data.Bool (bool)
import Data.Bits (xor)
import qualified Data.HashMap.Strict as M

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

-- labels and ids
type Label = String
type Id    = String

-- ===
-- Log
-- ===
type Log = [Message]
log :: MonadWriter Log m => Message -> m ()
log msg = tell [msg]

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

data Message = Stmt       Stmt
             | State      VarTab
             | EndOfBlock To
             | NewBlock   Label
             | Error      Error
instance Show Message where
  show (Stmt s)           = "> " ++ show s
  show (State vtab)       = show vtab
  show (EndOfBlock t)     = show t --case t of
                              -- Goto l -> "going to " ++ l
    -- If {}  -> "branch: "  ++ show t
    -- Exit   -> "program terminated sucessfully."
  show (NewBlock l)       = ">> " ++ l --">> entering block: " ++ l
  show (Error err)        = "*** Error: " ++ err

-- ======
-- VarTab
-- ======
newtype VarTab = VarTab (M.HashMap Id Value)
instance Show VarTab where
  show (VarTab vtab) = (
      (\vt -> bool ((intercalate "\n" . map (\(k,v) -> k ++ " -> " ++ show v)) vt) ("null") (null vt))
    . M.toList
    . M.filter (not . isClear)
    ) vtab
insert id val (VarTab vtab) = VarTab $ M.insert id val vtab
adjust op id (VarTab vtab)  = VarTab $ M.adjust op id vtab

-- =====
-- Errors
-- =====
type Error = String

-- ===
-- AST
-- ===

newtype AST = AST (M.HashMap Label Block)
instance Show AST where
  show (AST ast) = (intercalate "\n\n" . map (\(l,b) -> l ++ ": " ++ show b) . M.toList) ast

newtype Block = Block (From, [Stmt], To)
instance Show Block where
  show (Block (f,s,t)) = show f ++ "\n  "
    ++ (intercalate "\n  " . map show) s ++ "\n"
    ++ show t

data Value = IntV Integer | ListV [Value]
instance Show Value where
  show (IntV n)   = show n
  show (ListV ls) = show ls
isClear (IntV n)   = n == 0
isClear (ListV ls) = null ls

data From = From Label
          | Fi Exp Label Label
          | Entry
instance Show From where
  show (From l)     = "from " ++ l
  show (Fi e l1 l2) = case e of
    Parens _ -> "fi " ++ show e ++ " " ++ l1 ++ " " ++ l2
    _        -> "fi (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show Entry        = "entry"

data To = Goto Label
        | If Exp Label Label
        | Exit
instance Show To where
  show (Goto l)     = "goto " ++ l
  show (If e l1 l2) = case e of
    Parens _ -> "if "  ++ show e ++ " "  ++ l1 ++ " " ++ l2
    _        -> "if (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show Exit         = "exit"

data Stmt = Update Id UpdOp Exp
          | Push Id Id
          | Pop  Id Id
          | Swap Id Id
          | Skip
        -- NU!     | Seq [Stmt]
instance Show Stmt where
  show (Update id op e) = id ++ show op ++ show e
  show (Push id1 id2)   = "push " ++ id1 ++ " " ++ id2
  show (Pop id1 id2)    = "pop "  ++ id1 ++ " " ++ id2
  show (Swap id1 id2)   = "swap " ++ id1 ++ " " ++ id2
  show Skip             = "skip"
  -- show (Seq s)          = (intercalate "\n  " . map show) s


data UpdOp = PlusEq | MinusEq | XorEq| MultEq | DivEq
instance Show UpdOp where
  show PlusEq  = " += "
  show MinusEq = " -= "
  show XorEq   = " ^= "
  show MultEq  = " *= "
  show DivEq   = " /= "
mapUpdOp :: UpdOp -> Exp -> Exp -> Exp
mapUpdOp PlusEq  = ABinary   Plus
mapUpdOp MinusEq = ABinary   Minus
mapUpdOp XorEq   = ABinary   Xor
mapUpdOp MultEq  = ABinary   Mult
mapUpdOp DivEq   = DivBinary Div

data Exp = Lit Value
         | Var   Id

         | ABinary ABinOp Exp Exp

         | DivBinary DivOp Exp Exp

         | AUnary AUnOp Exp

         | Relational ROp Exp Exp

         | LBinary LBinOp Exp Exp

         | Not Exp

         | Top   Exp
         | Size  Exp
         | Empty Exp

         | Parens Exp

instance Show Exp where
  show (Lit v)             = show v
  show (Var id)            = id
  show (ABinary op l r)    = show l ++ show op ++ show r
  show (AUnary op e)       = case e of
    Parens _ -> show op ++ show e
    _        -> show op ++ "(" ++ show e ++ ")"
  show (Relational op l r) = show l ++ show op ++ show r
  show (LBinary op l r)    = show l ++ show op ++ show r
  show (Not e)             = case e of
    Parens _ -> "not " ++ show e
    _        -> "not (" ++ show e ++ ")"
  show (Top e)             = case e of
    Parens _ -> "top " ++ show e
    _        -> "top (" ++ show e ++ ")"
  show (Size e)            = case e of
    Parens _ -> "top " ++ show e
    _        -> "top (" ++ show e ++ ")"
  show (Empty e)           = case e of
    Parens _ -> "top " ++ show e
    _        -> "top (" ++ show e ++ ")"
  show (Parens e) = case e of
    Parens _ -> show e
    _        -> "(" ++ show e ++ ")"
-- ^
data ABinOp = Plus
            | Minus
            | Xor
            | Pow
            | Mult
instance Show ABinOp where
  show Plus  = " + "
  show Minus = " - "
  show Xor   = " ^ "
  show Pow   = " ** "
  show Mult  = " * "
mapABinOp op = case op of
  Plus  -> (+)
  Minus -> (-)
  Xor   -> xor
  Pow   -> (^)
  Mult  -> (*)
-- ^
data DivOp = Div | Mod
instance Show DivOp where
  show Div = " / "
  show Mod = " % "
mapDivOp op = case op of
  Div -> div
  Mod -> mod
-- ^
data AUnOp = Neg | Sign
instance Show AUnOp where
  show Neg  = "-"
  show Sign = "sign "
mapAUnOp op = case op of
  Neg  -> negate
  Sign -> signum
-- ^
data ROp = Eq | Less | Greater
instance Show ROp where
  show Eq      = " = "
  show Less    = " < "
  show Greater = " > "
mapROp op = case op of
  Eq      -> (==)
  Less    -> (<)
  Greater -> (>)
-- ^
data LBinOp = And | Or
instance Show LBinOp where
  show And = " && "
  show Or  = " || "
mapLBinOp op = case op of
  And -> (&&)
  Or  -> (||)

-- ==================
-- Monad transformers
-- ==================

-- ProgState is the outer state that must read from AST
type ProgState = ReaderT AST VarState
type VarState  = StateT VarTab (ExceptT Error (Writer Log))
rd :: Id -> VarState Value
rd id  = do
  VarTab ast <- get
  case M.lookup id ast of
          Just v  -> return v
          Nothing -> logError $ "Variable '" ++ id ++ "' not defined."
-- VarState is the innter state that does need to read from AST
-- type VarState  = StateT VarTab (Except Error (Writer Log))


-- ==================
-- Running the program
-- ==================

runProgram :: Label -> VarTab -> AST -> (Either Error VarTab, Log)
runProgram entry vtab =
  runWriter . runExceptT . flip execStateT vtab . runReaderT (interp entry)

-- pretty print the result
pprint :: (Either Error VarTab, Log) -> IO ()
pprint (result, log) = do
  case result of
    Left err   -> putStrLn $ "*** Error: "  ++ err
    Right vtab -> putStrLn $ "Result:\n" ++ show vtab
  writeFile "log.rlog" $ "file: filename.rl\n\n" ++ (intercalate "\n\n" . map show) log
  -- putStrLn $ "\nLog:\n" ++ (intercalate "\n\n" . map show) log

-- ======
-- Blocks
-- ======

interp :: Label -> ProgState ()
interp l = do
  log $ NewBlock l
  AST ast <- ask
  case M.lookup l ast of
    Just (Block (_,ss,t)) -> do
      lift $ execStmts ss
      log $ EndOfBlock t
      case t of
        Exit       -> return ()
        Goto l     -> interp l
        If t l1 l2 -> do
          t' <- lift $ valToBool <$> eval t
          bool (interp l2) (interp l1) t'
    Nothing -> lift $ logError ("Label '" ++ l ++ "' not defined.")


-- ==========
-- Statements
-- ==========
execStmts :: [Stmt] -> VarState ()
execStmts = foldr ((>>) . logStmt) (return ())

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
eval (Var id) = rd id
eval (Lit v)  = return v

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
boolToVal b = IntV $ bool 0 1 b
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
vtab = VarTab $ M.fromList [("n", IntV 0), ("v", IntV 0)]
ast  = AST $ M.fromList [
      ("init", Block (Entry,
        [Update "n" PlusEq (Lit $ IntV 10)],
        Goto "loop"))

    , ("loop", Block (Fi (Relational Eq (Var "v") (Lit $ IntV 0)) "init" "loop",
        [
          Update "v" PlusEq (Var "n")
        , Update "n" MinusEq (Lit $ IntV 1)
        ],
        If (Var "n") "loop" "end"))

    , ("end", Block (From "loop",
        [Skip, Push "a" "b"],
        Exit))
    ]

main = do
  putStrLn $ show ast ++ "\n"

  pprint $ runProgram "init" vtab ast
