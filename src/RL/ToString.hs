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
instToString (Update n op e) = n ++ updOpToString op ++ expToString e
instToString (Push var1 var2)     = "push " ++ var1 ++ " " ++ var2
instToString (Pop  var1 var2)     = "pop " ++ var1 ++ " " ++ var2
instToString Skip          = "skip"

updOpToString :: UpdateOperator -> String
updOpToString PlusEq   = " += "
updOpToString MinusEq  = " -= "
updOpToString XorEq    = " ^= "
updOpToString TimesEq  = " *= "
updOpToString DivideEq = " /= "

toToString :: Goto -> String
toToString (Goto l)     = "goto " ++ l
toToString (If e l1 l2) = case e of
  Parens _ -> "if " ++ expToString e ++ " " ++ l1 ++ " " ++ l2
  _        -> "if " ++ expToString (Parens e) ++ " " ++ l1 ++ " " ++ l2
toToString Exit         = "exit"

expToString :: Expression -> String
expToString (Var n)                    = n
expToString (BinOperation binop e1 e2) = expToString e1 ++ binopToString binop ++ expToString e2
expToString (UnOperation unop e)       = unopToString unop ++ expToString e
expToString (Constant v)     = valueToString v
expToString (Top n)          = "top " ++ n
expToString (Empty n)        = "empty " ++ n
expToString (Parens -- Redundant brackets
              (Parens e)
            ) = expToString $ Parens e
expToString (Parens e)       = "(" ++ expToString e ++ ")"

binopToString :: BinOperator -> String
binopToString Plus   = " + "
binopToString Minus  = " - "
binopToString Xor    = " ^ "
binopToString Times  = " * "
binopToString Divide = " / "
binopToString Eq     = " = "
binopToString Lth    = " < "
binopToString Gth    = " > "
binopToString And    = " && "
binopToString Or     = " || "

unopToString :: UnOperator -> String
unopToString Not    = "not "
unopToString Negate = "-"

varToString :: Identifier -> String
varToString var = var

valueToString :: Value -> String
valueToString (IntValue  n)  = show n
valueToString (BoolValue b)
  | b     = "true"
  | not b = "false"
valueToString (StackValue lst) = concatMap valueToString lst
