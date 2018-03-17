module Main where
import Data.List
import Data.Maybe
import Data.Bits

-- When changing anything;
-- AST types
-- Reversion
-- toString
-- evaluation/interpretation

-- AST
type Var   = String

type AST   = Block
type Block = [Inst]

data Exp
  = Plus   Exp Exp
  | Minus  Exp Exp
  | Times  Exp Exp
  | Divide Exp Exp
  | Eq     Exp Exp
  | Gth    Exp Exp
  | Lth    Exp Exp
  | And    Exp Exp
  | Or     Exp Exp
  | Not    Exp
  | Var    Var
  | Const  Value
  deriving Show

data Inst
  = Swap Var Var
  | PlusEq  Var  Exp
  | MinusEq Var  Exp
  | XOREq   Var  Exp
  | If Exp Block Block Exp
  | From Exp Block Exp
  | Skip
  deriving Show

data Value
  = IntVal   Int
  | FloatVal Float
  | BoolVal  Bool
  | ListVal  [Value]
  deriving Show

-- Converting AST to string for pretty printing
astToString :: Int -> AST -> String
astToString ind = intercalate "\n" . map (\i -> replicate (ind*2) ' ' ++ instToString ind i)

instToString :: Int -> Inst -> String
instToString _ (Swap n1 n2)   = n1 ++ " <=> " ++ n2
instToString _ (PlusEq  n e)  = n ++ " += " ++ expToString e
instToString _ (MinusEq n e)  = n ++ " -= " ++ expToString e
instToString _ (XOREq n e)    = n ++ " ^= " ++ expToString e
instToString ind (If t b1 b2 a) =
  "if (" ++ expToString t ++ ") then\n" ++
  astToString (ind+1) b1 ++ "\nelse\n" ++
  astToString (ind+1) b2 ++ "\nfi(" ++ expToString a ++ ")"
instToString ind (From a b t) =
  "from (" ++ expToString a ++ ")\ndo\n" ++
  astToString (ind+1) b ++ "\nuntil (" ++ expToString t ++ ")"
instToString _ Skip           = "skip"

expToString :: Exp -> String
expToString (Plus   e1 e2) = expToString e1 ++ " + "  ++ expToString e2
expToString (Minus  e1 e2) = expToString e1 ++ " - "  ++ expToString e2
expToString (Times  e1 e2) = expToString e1 ++ " * "  ++ expToString e2
expToString (Divide e1 e2) = expToString e1 ++ " / "  ++ expToString e2
expToString (Eq  e1 e2)    = expToString e1 ++ " == " ++ expToString e2
expToString (Lth e1 e2)    = expToString e1 ++ " < "  ++ expToString e2
expToString (Gth e1 e2)    = expToString e1 ++ " > "  ++ expToString e2
expToString (And e1 e2)    = expToString e1 ++ " && " ++ expToString e2
expToString (Or  e1 e2)    = expToString e1 ++ " || " ++ expToString e2
expToString (Not e1)       = "not (" ++ expToString e1 ++ ")"
expToString (Const v)      = valueToString v
expToString (Var n)        = n

valueToString :: Value -> String
valueToString (IntVal   n)  = show n
valueToString (FloatVal x)  = show x
valueToString (BoolVal b)   = show b
valueToString (ListVal lst) = concatMap valueToString lst

-- Reversion
reverseAST :: AST -> AST
reverseAST = map reverseInst . reverse

reverseInst :: Inst -> Inst
reverseInst (Swap  n1 n2)  = Swap n1 n2
reverseInst (PlusEq  n e)  = MinusEq n e
reverseInst (MinusEq n e)  = PlusEq  n e
reverseInst (XOREq   n e)  = XOREq   n e
reverseInst (If t b1 b2 a) = If a (reverseAST b1) (reverseAST b2) t
reverseInst (From a b t)   = From t (reverseAST b) a
reverseInst Skip           = Skip

-- VarTab

type VarTab = [(Var, Value)]

update :: String -> Value -> VarTab -> VarTab
update name vn vtab = case vtab of
      (n,vo) : bs
        | n == name  -> (n,vn) : bs
        | otherwise  -> (n,vo) : update name vn bs
      [] -> error $ "Variable not defined: " ++ name

bind :: String -> Value -> VarTab -> VarTab
bind name value vtab = (name,value):vtab

varTabToString :: VarTab -> String
varTabToString = intercalate "\n" . map (\(n,v) -> n ++ " -> " ++ show v)

-- Interpreting engine --

-- interpreting a program
interpAST :: AST -> VarTab -> VarTab
interpAST ast vtab = foldl (flip interpInst) vtab ast

-- interpreting an instruction
interpInst :: Inst -> VarTab -> VarTab
interpInst i vtab = case i of
  PlusEq name exp -> case (lookup name vtab, eval exp vtab) of
    (Just (IntVal   n), IntVal   m) -> update name (IntVal   $ n + m) vtab
    (Just (FloatVal n), FloatVal m) -> update name (FloatVal $ n + m) vtab
    (Nothing, IntVal   m)         -> bind   name (IntVal   m) vtab
    (Nothing, FloatVal m)         -> bind   name (FloatVal m) vtab
    _                           -> error "Operands not of correct type"
  MinusEq name exp -> case (lookup name vtab, eval exp vtab) of
    (Just (IntVal   n), IntVal   m) -> update name (IntVal   $ n - m) vtab
    (Just (FloatVal n), FloatVal m) -> update name (FloatVal $ n - m) vtab
    (Nothing, _)                -> error "No support for negative numbers"
    _                           -> error "Operands not of correct type"
  XOREq   name exp -> case (lookup name vtab, eval exp vtab) of
    (Just (IntVal n), IntVal m) -> update name (IntVal $ xor n m) vtab
    (Nothing, IntVal m)         -> bind   name (IntVal m) vtab
    _                           -> error "Operands not of correct type"
  Swap name1 name2 -> case (lookup name1 vtab, lookup name2 vtab) of
    (Just v1, Just v2) ->
      let vtab' = update name1 v2 vtab
        in update name2 v1 vtab'
    _ -> error "Variable is not defined"
  If t b1 b2 a     -> case eval t vtab of
    BoolVal bl1    -> do
      let vtab' = interpAST (if bl1 then b1 else b2) vtab
      case eval a vtab' of
        BoolVal bl2 | bl1 == bl2 -> vtab'
                    | bl1 /= bl2 -> error "if-then-else: Test does not match assertion."
        _                        -> error "if-then-else: Assertions is not boolean."
    _ -> error "if-then-else: Test is not boolean."
  From a b t       -> case eval a vtab of
    BoolVal True   -> doLoop a b t vtab
    BoolVal False  -> error "do-until: Assertion evaluated to false at initialization."
    _ -> error "do-until: Assertion is of wrong type."
  Skip             -> vtab

doLoop :: Exp -> Block -> Exp -> VarTab -> VarTab
doLoop a b t vtab = case eval t vtab' of
    BoolVal True  -> vtab'
    BoolVal False -> case eval a vtab' of
      BoolVal False -> doLoop a b t vtab'
      BoolVal True  -> error "do-until: Assertion evaluated to true mid-loop."
      _             -> error "do-until: Test is of wrong type."
    _ -> error "do-until: Assertion is of wrong type."
    where vtab' = interpAST b vtab

-- evaluating an expression
eval :: Exp -> VarTab -> Value
eval (Plus e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n + m
  (FloatVal x, FloatVal y) -> FloatVal $ x + y
  _                        -> error "Adding two incompatible types."
eval (Minus e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n - m
  (FloatVal x, FloatVal y) -> FloatVal $ x - y
  _                        -> error "Subtracting two incompatible types."
eval (Times e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n * m
  (FloatVal x, FloatVal y) -> FloatVal $ x * y
  _                        -> error "Subtracting two incompatible types."
eval (Divide e1 e2) vtab = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n `div` m
  (FloatVal x, FloatVal y) -> FloatVal $ x / y
  _                        -> error "Subtracting two incompatible types."
eval (Eq  e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> BoolVal $ n == m
  (FloatVal x, FloatVal y) -> BoolVal $ x == y
  _                        -> error "Comparing two incompatible types."
eval (Lth e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> BoolVal $ n < m
  (FloatVal x, FloatVal y) -> BoolVal $ x < y
  _                        -> error "Comparing two incompatible types."
eval (Gth e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> BoolVal $ n > m
  (FloatVal x, FloatVal y) -> BoolVal $ x > y
  _                        -> error "Comparing two incompatible types."
eval (And e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (BoolVal b1, BoolVal b2)     -> BoolVal $ b1 && b2
  _                        -> error "Comparing two incompatible types."
eval (Or e1 e2) vtab     = case (eval e1 vtab, eval e2 vtab) of
  (BoolVal b1, BoolVal b2)     -> BoolVal $ b1 || b2
  _                        -> error "Comparing two incompatible types."
eval (Not e) vtab        = case eval e vtab of
  BoolVal b                -> BoolVal $ not b
  _                        -> error "Comparing two incompatible types."
eval (Var  n) vtab = fromMaybe (IntVal 0) (lookup n vtab)
eval (Const v) _ = v

-- Helper to test implementation
testAST :: AST -> VarTab -> IO ()
testAST ast sstate = do
  let res  = interpAST ast sstate
      ast' = reverseAST ast
      res' = interpAST ast' res
  putStrLn "Starting state:"
  putStrLn $ varTabToString sstate ++ "\n"
  putStrLn $ astToString 0 ast
  putStrLn "\nResult:"
  putStrLn $ varTabToString res
  putStrLn "\n---- Reversed ----\n"
  putStrLn $ astToString 0 ast'
  putStrLn "\nResult:"
  putStrLn $ varTabToString res'

-- Main
main = do
-- A sample AST - calculate the nth fibonacci number
let ast =
      [
        XOREq "w" (Const $ IntVal 1),
        From
          (Eq (Var "v") (Const $ IntVal 0))
          [
            PlusEq "v" (Var "w"),
            Swap   "v" "w",
            MinusEq "n" (Const $ IntVal 1)
          ]
          (Or (Eq (Var "n") (Const $ IntVal 0)) (Gth (Var "v") (Var "w")))
      ]

testAST ast [("n", IntVal 16)]
