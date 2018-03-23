{-# LANGUAGE LambdaCase #-}

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

-- VarTab
type VarTab    = [(String, Value)]

varTabToString :: VarTab -> String
varTabToString []   = "Clear."
varTabToString vtab = (intercalate "\n" . map (\(n,v) -> n ++ " -> " ++ valueToString v)) vtab

valueToString :: Value -> String
valueToString (IntValue  n)    = show n
valueToString (StackValue lst) = "[" ++ (intercalate ", " . map valueToString) lst ++ "]"

type ProgState = StateT VarTab (Except ProgError)

update :: Identifier -> Value -> ProgState ()
update n nv  = modify $ \st -> case () of
  _ | any (\(n',v) -> n'==n) st -> map (\(n',v) -> if n'==n then (n',nv) else (n',v)) st
    | otherwise                 -> (n,nv):st

-- Read an identifier
rd :: Identifier -> ProgState (Maybe Value)
rd var = lookup var <$> get

-- Swap two identifiers
swap :: Identifier -> Identifier -> ProgState ()
swap a b = modify $ \st -> map (\(n,v) -> if n == a then (b,v) else if n == b then (a,v) else (n,v)) st

-- Interpreting engine --
runProgram :: AST -> (VarTab -> Either ProgError VarTab)
runProgram ast = runExcept . execStateT (interpAST ast)

-- interpreting a program
interpAST :: AST -> ProgState ()
interpAST ast = (interpAST' ast . genLabels) ast
-- stripping starts here
  >> strip

strip :: ProgState ()
strip = modify $ \st -> filter (\(n,v) -> (not . isZero) v) st

isZero :: Value -> Bool
isZero (StackValue st) = null st
isZero (IntValue n)    = n == 0
-- to here - just remove if need be

interpAST' :: AST -> LabTab -> ProgState ()
interpAST' ast ltab = case ast of
  AST _ [] -> return ()
  AST _ (Block l _ insts t:_) -> interpInsts insts >> case t of
    Exit    -> return ()
    Goto lt -> interpAST' (goto l lt ast ltab) ltab
    If exp ltt ltf -> eval exp >>= \case
      BoolValue b | b     -> interpAST' (goto l ltt ast ltab) ltab
      BoolValue b | not b -> interpAST' (goto l ltf ast ltab) ltab

-- interpreting list of instructions
interpInsts :: [Statement] -> ProgState ()
interpInsts = foldr ((>>) . interpInst) (return ())

-- interpreting an instruction
interpInst :: Statement -> ProgState ()
interpInst i = case i of
  Update var op exp -> join $ update var <$> eval (updateOpMapper op (Var var) exp)
  Push var1 var2    -> error "Push not implemented yet." -- TODO
  Pop  var1 var2    -> error "Pop not implemented yet."  -- TODO
  Swap var1 var2    -> swap var1 var2
  Skip              -> return ()

updateOpMapper :: UpdateOperator -> (Expression -> Expression -> Expression)
updateOpMapper PlusEq   = BinOperation Plus
updateOpMapper MinusEq  = BinOperation Minus
updateOpMapper XorEq    = BinOperation Xor
updateOpMapper TimesEq  = BinOperation Times
updateOpMapper DivideEq = BinOperation Divide

-- evaluating an expression
eval :: Expression -> ProgState Value
eval (BinOperation Divide exp1 exp2) = eval exp2 >>= \case
  IntValue 0 -> throwError DivByZero
  v          -> flip (binopMapper Divide) v <$> eval exp1
eval (BinOperation Eq exp1 exp2)    = eval exp1 >>= \case
  IntValue n -> binIntToBool  (==) <$> eval exp1 <*> eval exp2
  v          -> binBoolToBool (==) <$> eval exp1 <*> eval exp2
eval (BinOperation binop exp1 exp2) = binopMapper binop <$> eval exp1 <*> eval exp2
eval (UnOperation unop exp)         = unopMapper  unop  <$> eval exp
eval (Top n)                        = error "Top not implemented yet."   -- TODO
eval (Empty n)                      = error "Empty not implemented yet." -- TODO
eval (Var v)                        = fromMaybe (IntValue 0) <$> rd v
eval (Constant v)                   = return v
eval (Parens e)                     = eval e

--
binopMapper :: BinOperator -> (Value -> Value -> Value)
binopMapper Plus   = binIntToInt (+)
binopMapper Minus  = binIntToInt (-)
binopMapper Xor    = binIntToInt xor
binopMapper Times  = binIntToInt (*)
binopMapper Divide = binIntToInt div

binopMapper Lth    = binIntToBool (<)
binopMapper Gth    = binIntToBool (>)

binopMapper And    = binBoolToBool (&&)
binopMapper Or     = binBoolToBool (||)

binIntToInt :: (Int -> Int -> Int) -> Value -> Value -> Value
binIntToInt op (IntValue n) (IntValue m) = IntValue (n `op` m)

binIntToBool :: (Int -> Int -> Bool) -> Value -> Value -> Value
binIntToBool op (IntValue n) (IntValue m) = BoolValue (n `op` m)

binBoolToBool :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
binBoolToBool op (BoolValue p) (BoolValue q) = BoolValue (p `op` q)
--

unopMapper :: UnOperator -> (Value -> Value)
unopMapper Not     = \(BoolValue p) -> BoolValue (not p)
unopMapper Negate  = \(IntValue n)  -> IntValue (-n)
