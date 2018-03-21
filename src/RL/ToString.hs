module RL.ToString ( astToString ) where
import RL.AST
import Data.List

-- Converting AST to string for pretty printing
astToString :: AST -> String
astToString (AST _ blocks) = (intercalate "\n" . map blockToString) blocks

blockToString :: Block -> String
blockToString (Block l f insts t) =
  labelToString l     ++ " "  ++
  fromToString  f     ++ "\n" ++
  instsToString insts ++
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
instsToString [] = ""
instsToString insts = "  " ++ (intercalate "\n  " . map instToString) insts ++ "\n"

instToString :: Statement -> String
instToString (Swap var1 var2) = "swap " ++ varToString var1 ++ " " ++ varToString var2
instToString (Update n PlusEq  e) = n ++ " += " ++ expToString e
instToString (Update n MinusEq e) = n ++ " -= " ++ expToString e
instToString (Update n XorEq   e) = n ++ " ^= " ++ expToString e
instToString (Push var1 var2)     = "push " ++ var1 ++ " " ++ var2
instToString (Pop  var1 var2)     = "pop " ++ var1 ++ " " ++ var2
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
expToString (Lth e1 e2)    = expToString e1 ++ " < "  ++ expToString e2
expToString (Gth e1 e2)    = expToString e1 ++ " > "  ++ expToString e2
expToString (And e1 e2)    = expToString e1 ++ " && " ++ expToString e2
expToString (Or  e1 e2)    = expToString e1 ++ " || " ++ expToString e2
expToString (Parens -- Redundant brackets
              (Parens e)
            ) = expToString $ Parens e
expToString (Parens e)       = "(" ++ expToString e ++ ")"
expToString (Not (Not e))    = expToString e
expToString (Not (Parens e)) = "not "   ++ expToString e
expToString (Not e)          = "not " ++ "("  ++ expToString e ++ ")"
expToString (Top v)          = "top "    ++ varToString v
expToString (Empty v)        = "empty "  ++ varToString v
expToString (Constant v)     = valueToString v
expToString (Var v)          = varToString v

varToString :: Identifier -> String
varToString var = var

valueToString :: Value -> String
valueToString (IntValue  n)  = show n
valueToString (BoolValue b)
  | b     = "true"
  | not b = "false"
valueToString (StackValue lst) = concatMap valueToString lst
