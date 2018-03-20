module RL.ToString ( astToString ) where
import RL.AST

-- Converting AST to string for pretty printing
astToString :: AST -> String
astToString (AST _ blocks) = (intercalate "\n" . map blockToString) blocks

blockToString :: Block -> String
blockToString (Block l f insts t) =
  labelToString l     ++ " "  ++
  fromToString  f     ++ "\n  " ++
  instsToString insts ++ "\n  " ++
  toToString t

labelToString :: Label -> String
labelToString l = l++":"

fromToString :: From -> String
fromToString (From l)     = "from " ++ l
fromToString (Fi e l1 l2) = case e of
  Parens _ -> "fi " ++ expToString e ++ " " ++ l1 ++ " " ++ l2
  _        -> "fi " ++ expToString (Parens e) ++ " " ++ l1 ++ " " ++ l2
fromToString Entry = "entry"

instsToString :: [Statement] -> String
instsToString = intercalate "\n  " . map instToString

instToString :: Statement -> String
instToString (Swap var1 var2) = "swap " ++ varToString var1 ++ " " ++ varToString var2
instToString (Assignment (Variable n) PlusEq  e) = n ++ " += " ++ expToString e
instToString (Assignment (Variable n) MinusEq e) = n ++ " += " ++ expToString e
instToString (Assignment (Variable n) XorEq   e) = n ++ " += " ++ expToString e
instToString Skip          = "skip"

toToString :: Goto -> String
toToString (Goto l)     = "goto " ++ l
toToString (If e l1 l2) = case e of
  Parens _ -> "if " ++ expToString e ++ " " ++ l1 ++ " " ++ l2
  _        -> "if " ++ expToString (Parens e) ++ " " ++ l1 ++ " " ++ l2
toToString Exit         = "exit"

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
