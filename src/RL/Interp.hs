module RL.Interp ( runProgram, VarTab ) where
import RL.Error
import RL.AST

-- Working on lists
import Data.List
-- Bitwise XOR
import Data.Bits

-- StateT and Except monads
import Control.Monad.State
import Control.Monad.Except

-- When changing anything;
--   AST types
--   Reversion
--   toString
--   evaluation/interpretation

-- VarTab
type VarTab    = [(String, Value)]
type ProgState = StateT VarTab (Except ProgError)

update :: Identifier -> (Value -> Value -> Value) -> Value -> ProgState ()
update (Variable name) op val = do
  st <- get
  case lookup name st of
    Nothing -> do
      put $ (name, IntValue 0):st
      update (Variable name) op val
    Just _  -> put $ update' (Variable name) op val st
-- TODO: Implement Index variation

update' (Variable name) op val vtab = case vtab of
  (n,v):rst | n == name -> (n, op v val) : rst
            | otherwise -> (n,v) : update' (Variable name) op val rst
-- TODO: Implement Index variation

-- Read an identifier
rd :: Identifier -> ProgState Value
rd var = do
  st <- get
  case var of
    Variable name -> case lookup name st of
        Nothing -> do
          put $ (name, IntValue 0):st
          rd var
        Just v  -> return v
    -- TODO: Implement Index variation properly
    Index name e -> do
      ind <- eval e
      case ind of
        IntValue i -> case lookup name st of
          Nothing -> do
            put $ (name, ListValue $ replicate (i+1) (IntValue 0)):st
            rd var
          Just (ListValue lst) -> return (lst !! i)
          _ -> throwError $ IndexOnNonList name
        _ -> throwError IndexNotInteger

-- Swap two identifiers
swap :: Identifier -> Identifier -> ProgState ()
swap var1 var2 = do
  v1 <- rd var1
  v2 <- rd var2
  update var1 (\_ v -> v) v2
  update var2 (\_ v -> v) v1

-- Interpreting engine --
runProgram :: AST -> Either ProgError VarTab
runProgram ast = (runExcept . execStateT (interpAST ast)) []

-- interpreting a program
interpAST :: AST -> ProgState ()
interpAST ast = do
  let labels = genLabels ast
  interpAST' ast labels

interpAST' :: AST -> LabTab -> ProgState ()
interpAST' ast ltab = case ast of
  AST _ [] -> return ()
  AST _ (Block l _ insts t:_) -> do
    interpInsts insts
    case t of
      Exit    -> return ()
      Goto lt -> interpAST' (goto l lt ast ltab) ltab
      If exp ltt ltf -> do
        t <- eval exp
        case t of
          BoolValue True  -> interpAST' (goto l ltt ast ltab) ltab
          BoolValue False -> interpAST' (goto l ltf ast ltab) ltab
          _               -> throwError TestNotBoolean

-- interpreting list of instructions
interpInsts :: [Statement] -> ProgState ()
interpInsts (i:insts) = do
  interpInst i
  interpInsts insts
interpInsts [] = return ()

-- interpreting an instruction
interpInst :: Statement -> ProgState ()
interpInst i = case i of
  Assignment var op exp
    | exp `contains` var -> throwError AssignedVarIsOperand
    | otherwise -> do
      v <- eval exp -- TODO: Type check?
      update var (applyBinOp binop) v
      where binop = case op of
              PlusEq  -> (+)
              MinusEq -> (-)
              XorEq   -> xor
  Swap var1 var2 -> swap var1 var2
  Skip      -> return ()

contains :: Expression -> Identifier -> Bool
contains (Plus e1 e2)   var = contains e1 var || contains e2 var
contains (Minus e1 e2)  var = contains e1 var || contains e2 var
contains (Times e1 e2)  var = contains e1 var || contains e2 var
contains (Divide e1 e2) var = contains e1 var || contains e2 var
contains (Var (Variable n1)) (Variable n2) = n1==n2
-- TODO: Implement Index variation
contains (Constant v) (Variable n2) = False
contains (Parens e)     var = contains e var

applyBinOp :: (Int -> Int -> Int) -> Value -> Value -> Value
applyBinOp op (IntValue n) (IntValue m) = IntValue $ op n m

-- evaluating an expression
eval :: Expression -> ProgState Value
eval (Plus e1 e2)  = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (IntValue n,
     IntValue m) -> return $ IntValue (n + m)
    _            -> throwError $ WrongType "Int"
eval (Minus e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (IntValue n,
     IntValue m) -> return $ IntValue (n - m)
    _            -> throwError $ WrongType "Int"
eval (Times e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (IntValue n,
     IntValue m) -> return $ IntValue (n * m)
    _            -> throwError $ WrongType "Int"
eval (Divide e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (IntValue n,
     IntValue m) | m == 0      -> throwError DivByZero
                 | mod n m ==0 -> return $ IntValue (div n m)
                 | otherwise   -> throwError DivHasRest
    _            -> throwError $ WrongType "Int"
eval (Eq e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (IntValue n,
     IntValue m) -> return $ BoolValue (n == m)
    _            -> throwError $ WrongType "Int"
eval (Lth e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (IntValue n,
     IntValue m) -> return $ BoolValue (n < m)
    _            -> throwError $ WrongType "Int"
eval (Gth e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (IntValue n,
     IntValue m) -> return $ BoolValue (n > m)
    _            -> throwError $ WrongType "Int"
eval (And e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (BoolValue p,
     BoolValue q) -> return $ BoolValue (p && q)
    _             -> throwError $ WrongType "Bool"
eval (Or e1 e2) = do
  v1 <- eval e1 ; v2 <- eval e2
  case (v1,v2) of
    (BoolValue p,
     BoolValue q) -> return $ BoolValue (p || q)
    _             -> throwError $ WrongType "Bool"
eval (Var v) = rd v
eval (Constant v)  = return v
eval (Parens e) = eval e
