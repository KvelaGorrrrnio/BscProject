module SRL.ToString ( astToString ) where
import SRL.AST
import Data.List

-- Converting AST to string for pretty printing
astToString :: AST -> String
astToString = astToString' 0

astToString' :: Int -> AST -> String
astToString' ind = intercalate "\n" . map (\i -> replicate (ind*2) ' ' ++ instToString ind i)

instToString :: Int -> Statement -> String
instToString _ (Swap var1 var2) = "swap " ++ varToString var1 ++ " " ++ varToString var2
instToString _ (Assignment (Variable n) PlusEq  e) = n ++ " += " ++ expToString e
instToString _ (Assignment (Variable n) MinusEq e) = n ++ " -= " ++ expToString e
instToString _ (Assignment (Variable n) XorEq   e) = n ++ " ^= " ++ expToString e
instToString ind (If t b1 b2 a) =
  "if " ++ expToString t ++ "then\n" ++
  astToString' (ind+1) b1  ++ "\nelse\n" ++
  astToString' (ind+1) b2  ++ "\nfi " ++ expToString a
instToString ind (From a b t) =
  "from " ++ expToString a ++ "\n"  ++
  replicate (ind*2) ' ' ++ "do\n"  ++
  astToString' (ind+1) b ++ "\n"    ++
  replicate (ind*2) ' ' ++ "until" ++
  " (" ++ expToString t ++ ")"
instToString _ Skip          = "skip"

expToString :: Expression -> String
expToString (Plus   e1 e2) = expToString e1 ++ " + "  ++ expToString e2
expToString (Minus  e1 e2) = expToString e1 ++ " - "  ++ expToString e2
expToString (Xor    e1 e2) = expToString e1 ++ " / "  ++ expToString e2
expToString (Times  e1 e2) = expToString e1 ++ " * "  ++ expToString e2
expToString (Divide e1 e2) = expToString e1 ++ " / "  ++ expToString e2
expToString (Eq  e1 e2)    = expToString e1 ++ " = "  ++ expToString e2
expToString (Neq  e1 e2)   = expToString e1 ++ " != " ++ expToString e2
expToString (Lth e1 e2)    = expToString e1 ++ " < "  ++ expToString e2
expToString (Gth e1 e2)    = expToString e1 ++ " > "  ++ expToString e2
expToString (And e1 e2)    = expToString e1 ++ " && " ++ expToString e2
expToString (Or  e1 e2)    = expToString e1 ++ " || " ++ expToString e2
expToString (Not e1)       = "not ("   ++ expToString e1 ++ ")"
expToString (Top v)        = "top "    ++ varToString v
expToString (Empty v)      = "empty "  ++ varToString v
expToString (Constant v)   = valueToString v
expToString (Var v)        = varToString v
expToString (Parens -- Redundant brackets
              (Parens e)
            ) = expToString $ Parens e
expToString (Parens e)     = "(" ++ expToString e ++ ")"

varToString :: Identifier -> String
varToString (Variable x) = x
varToString (Index x i) = x ++ "[" ++ expToString i ++ "]"

valueToString :: Value -> String
valueToString (IntValue   n)  = show n
valueToString (FloatValue x)  = show x
valueToString (BoolValue b)
  | b     = "true"
  | not b = "false"
valueToString (ListValue lst) = concatMap valueToString lst
