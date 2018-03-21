module RL.Interp ( runProgram, VarTab, varTabToString ) where
import RL.Error
import RL.AST

------- TEMP
import Data.Maybe

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

-- Split up in statements and expressions
-- TODO:
-- Maybe errors should be so too?
-- Expression errors:
--    type errors when applying operator
--    negative index?
--    division by zero
--    division has rest
--    indexing on non-lists? - can happen on both left- and right-hand side though
-- Instruction errors:
--    variable being assigned different type than expression
--    condition is not bool
--    assigned variable occurs in expression
--    index must be integer
--    indexing on non-lists? - can happen on both left- and right-hand side though
--    perhaps with 'checkIndexing (Index name ind) = v <- rd name; case v of (IntVal _) -> throwError; (ListVal _) -> return ()'

-- VarTab
type VarTab    = [(String, Value)]

varTabToString :: VarTab -> String
varTabToString []   = "Clear."
varTabToString vtab = (intercalate "\n" . map (\(n,v) -> n ++ " -> " ++ show v)) vtab

type ProgState = StateT VarTab (Except ProgError)

update :: Identifier -> (Value -> Value -> Value) -> Value -> ProgState ()
update name op val = do
  st <- get
  case lookup name st of
    Nothing -> do
      put $ (name, IntValue 0):st
      update name op val
    Just _  -> put $ update' name op val st
    where update' name op val vtab = case vtab of
            (n,v):rst | n == name -> (n, op v val) : rst
                      | otherwise -> (n,v) : update' name op val rst

--push :: Identifier -> Identifier -> ProgState ()
--push n1 n2 = update n1 (\val (StackValue st) -> StackValue (val:st))
--
--pop :: Identifier -> Identifier -> ProgState ()
--pop n1 n2 | isZero n1 = do
--  v <- pop n2
--  update n1 const v

-- TODO: Implement stack

-- Read an identifier
rd :: Identifier -> ProgState Value
rd var = state $ \st -> (fromMaybe (IntValue 0) (lookup var st),st)

-- Swap two identifiers
swap :: Identifier -> Identifier -> ProgState ()
swap a b = state $ \st -> return $ map (\(n,v) -> if n == a then (b,v) else if n == b then (a,v) else (n,v)) st

-- Interpreting engine --
runProgram :: AST -> (VarTab -> Either ProgError VarTab)
runProgram ast = runExcept . execStateT (interpAST ast)

-- interpreting a program
interpAST :: AST -> ProgState ()
interpAST ast = do
  let labels = genLabels ast
  interpAST' ast labels

-- stripping vtab from here
  state $ \st -> ((),strip st)

strip :: VarTab -> VarTab
strip = filter (\(n,v) -> (not . isZero) v)

isZero :: Value -> Bool
isZero (StackValue st) = null st
isZero (IntValue n)    = n==0
-- to here - just remove if need be

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

-- interpreting list of instructions
interpInsts :: [Statement] -> ProgState ()
interpInsts = foldr ((>>) . interpInst) (return ())

-- interpreting an instruction
interpInst :: Statement -> ProgState ()
interpInst i = case i of
  Update var op exp
    | otherwise -> do
      v1 <- rd var
      -- TODO: Type check?
      case v1 of
        IntValue _ -> do
          v2 <- eval exp
          case v2 of
            IntValue _ -> update var (applyBinOp binop) v2
                          where binop = case op of
                                  PlusEq  -> (+)
                                  MinusEq -> (-)
                                  XorEq   -> xor
            _          -> throwError $ WrongType "Int"
        _        -> throwError AssignedValNotScalarValue
      -- Type check done - but not in the prettiest way
  Swap var1 var2 -> swap var1 var2
  Skip      -> return ()

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
eval (Not e) = do
  v <- eval e
  case v of
    BoolValue q -> return $ BoolValue (not q)
    _           -> throwError $ WrongType "Bool"
eval (Var v) = rd v
eval (Constant v)  = return v
eval (Parens e) = eval e
