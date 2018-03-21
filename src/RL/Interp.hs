module RL.Interp ( runProgram, VarTab, varTabToString ) where
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
update (name) op val = do
  st <- get
  case lookup name st of
    Nothing -> do
      put $ (name, IntValue 0):st
      update (name) op val
    Just _  -> put $ update' (name) op val st
-- TODO: Implement Index variation

update' (name) op val vtab = case vtab of
  (n,v):rst | n == name -> (n, op v val) : rst
            | otherwise -> (n,v) : update' (name) op val rst
-- TODO: Implement Index variation

-- Read an identifier
rd :: Identifier -> ProgState Value
rd var = do
  st <- get
  case lookup var st of
    Nothing -> do
      put $ (var, IntValue 0):st
      rd var
    Just v  -> return v
    -- TODO: Implement Index variation properly

-- Swap two identifiers
swap :: Identifier -> Identifier -> ProgState ()
swap var1 var2 = do -- hvorfor ikke bare bytte om på navnene lol?
  v1 <- rd var1
  v2 <- rd var2
  update var1 (\_ v -> v) v2
  update var2 (\_ v -> v) v1

-- Interpreting engine --
runProgram :: AST -> (VarTab -> Either ProgError VarTab)
runProgram ast = runExcept . execStateT (interpAST ast)

-- interpreting a program
interpAST :: AST -> ProgState ()
interpAST ast = do
  let labels = genLabels ast
  interpAST' ast labels
-- stripping vtab from here
  vtab <- get
  put $ strip vtab

strip :: VarTab -> VarTab
strip [] = []
strip ((n,v):rst) = case v of
  IntValue 0   -> strip rst
  ListValue [] -> strip rst
  ListValue vs -> let vs' = (reverse . takeWhile (\(IntValue n) -> n==0) . reverse) vs
                    in if null vs' then strip rst else (n,ListValue vs'):strip rst
  _            -> (n,v):strip rst
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
  Update var op exp
    | exp `contains` var -> throwError UpdatedVarIsOperand
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
        _        -> throwError UpdatedValNotScalarValue
      -- Type check done - but not in the prettiest way
  Swap var1 var2 -> swap var1 var2
  Skip      -> return ()

contains :: Expression -> Identifier -> Bool
contains (Plus e1 e2)   var = contains e1 var || contains e2 var
contains (Minus e1 e2)  var = contains e1 var || contains e2 var
contains (Times e1 e2)  var = contains e1 var || contains e2 var
contains (Divide e1 e2) var = contains e1 var || contains e2 var
contains (Var n)        var = n == var
contains (Constant v)   var = False
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
eval (Not e) = do
  v <- eval e
  case v of
    BoolValue q -> return $ BoolValue (not q)
    _           -> throwError $ WrongType "Bool"
eval (Var v) = rd v
eval (Constant v)  = return v
eval (Parens e) = eval e
