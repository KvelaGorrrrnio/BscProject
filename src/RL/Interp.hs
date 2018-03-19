module RL.Interp where
import Data.List
import Data.Maybe
import Data.Bits
import RL.Parser
import RL.AST

-- When changing anything;
-- AST types
-- Reversion
-- toString
-- evaluation/interpretation

toAST :: [Block] -> AST
toAST = AST []

next :: AST -> AST
next (AST ls (r:rs)) = AST (r:ls) rs

prev :: AST -> AST
prev (AST (l:ls) rs) = AST ls (l:rs)

goto :: Label -> Label -> AST -> LabTab -> AST
goto l1 l2 ast ltab = case (lookup l1 ltab, lookup l2 ltab) of
  (Just n, Just m) | m-n >  0 -> iterate next ast !! (m-n)
  (Just n, Just m) | m-n <  0 -> iterate prev ast !! (n-m)
  (Just n, Just m) | m-n == 0 -> ast
  _                           -> error "Label not defined."

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
instToString (Swap (Variable n1) (Variable n2))  = "swap " ++ n1 ++ " " ++ n2
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

-- Reversion
reverseAST :: AST -> AST
reverseAST (AST [] blocks) = AST [] $ (map reverseBlock . reverse) blocks

reverseBlock :: Block -> Block
reverseBlock (Block l f insts t) = do
  let f'     = reverseTo t
      t'     = reverseFrom f
      insts' = reverseInsts insts
  Block l f' insts' t'

reverseTo :: Goto -> From
reverseTo (Goto ln)      = From ln
reverseTo (If e ltt ltf) = Fi e ltt ltf
reverseTo  Exit          = Entry

reverseFrom :: From -> Goto
reverseFrom (From l)       = Goto l
reverseFrom (Fi e ltt ltf) = If e ltt ltf
reverseFrom Entry          = Exit

reverseInsts :: [Statement] -> [Statement]
reverseInsts = map reverseInst . reverse

reverseInst :: Statement -> Statement
reverseInst (Assignment var op e) = Assignment var (reverseOp op) e
reverseInst (Swap n1 n2)          = Swap n1 n2
reverseInst Skip                  = Skip

reverseOp :: AssignOperator -> AssignOperator
reverseOp PlusEq  = MinusEq
reverseOp MinusEq = PlusEq
reverseOp XorEq   = XorEq

-- VarTab
type VarTab = [(String, Value)]

update :: Identifier -> (Value -> Value -> Value) -> Value -> VarTab -> VarTab
update (Variable name) op val vtab = case vtab of
  (n,v):rst | n == name -> (n, op v val) : rst
            | otherwise -> (n,v) : update (Variable name) op val rst
  []                    -> error "Variable not defined."

bind :: String -> Value -> VarTab -> VarTab
bind name value vtab = (name,value):vtab

varTabToString :: VarTab -> String
varTabToString = intercalate "\n" . map (\(n,v) -> n ++ " -> " ++ show v)

swap :: String -> String -> VarTab -> VarTab
swap name1 name2 vtab = case (lookup name1 vtab, lookup name2 vtab) of
  (Just v1, Just v2) -> (update (Variable name1) (\_ v -> v) v2 . update (Variable name2) (\_ v -> v) v1) vtab
  (Nothing, Just _ ) -> error $ name1 ++ " not defined."
  (Just _,  Nothing) -> error $ name2 ++ " not defined."

-- LabTab
type LabTab = [(Label, Int)]

genLabels :: AST -> LabTab
genLabels (AST _ bs) =
  fst $ foldl
        (\(lst, n) (Block l _ _ _) -> (lst++[(l,n)],n+1))
        ([],0) bs

-- Interpreting engine --

-- interpreting a program
interpAST :: AST -> VarTab -> VarTab
interpAST ast vtab = do
  let labels = genLabels ast
  interpAST' ast vtab labels

interpAST' :: AST -> VarTab -> LabTab -> VarTab
interpAST' ast vtab ltab = case ast of
  AST _ [] -> vtab
  AST _ (Block l _ [] t:_) -> error $ "Empty block: '" ++ l ++ "'."
  AST _ (Block l _ insts t:_) -> case t of
    Exit    -> vtab'
    Goto lt -> interpAST' (goto l lt ast ltab) vtab' ltab
    If exp ltt ltf ->
      case eval exp vtab' of
        BoolValue True  -> interpAST' (goto l ltt ast ltab) vtab' ltab
        BoolValue False -> interpAST' (goto l ltf ast ltab) vtab' ltab
        _             -> error "Type error in if then else"
    where vtab' = interpInsts insts vtab

-- interpreting list of instructions
interpInsts :: [Statement] -> VarTab -> VarTab
interpInsts insts vtab = foldl (flip interpInst) vtab insts

-- interpreting an instruction
interpInst :: Statement -> VarTab -> VarTab
interpInst i vtab = case i of
  Assignment (Variable name) op exp -> case lookup name vtab of
    Nothing -> (interpInst i . bind name (IntValue 0)) vtab
    Just _  -> update (Variable name) (apply binop) (eval exp vtab) vtab
    where binop = case op of
            PlusEq  -> (+)
            MinusEq -> (-)
            XorEq   -> xor
  Swap (Variable name1) (Variable name2) -> swap name1 name2 vtab
  Skip      -> vtab

apply :: (Int -> Int -> Int) -> Value -> Value -> Value
apply op (IntValue n) (IntValue m) = IntValue $ op n m
apply op _ _                   = error "Operands must be integers."

-- evaluating an expression
eval :: Expression -> VarTab -> Value
eval (Plus e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntValue n, IntValue m)     -> IntValue   $ n + m
  _                        -> error "Adding two incompatible types."
eval (Minus e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntValue n, IntValue m)     -> IntValue   $ n - m
  _                        -> error "Subtracting two incompatible types."
eval (Times e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntValue n, IntValue m)     -> IntValue   $ n * m
  _                        -> error "Subtracting two incompatible types."
eval (Divide e1 e2) vtab = case (eval e1 vtab, eval e2 vtab) of
  (IntValue n, IntValue m)     -> IntValue   $ n `div` m
  _                        -> error "Subtracting two incompatible types."
eval (Eq  e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntValue n, IntValue m)     -> BoolValue $ n == m
  _                        -> error "Comparing two incompatible types."
eval (Lth e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntValue n, IntValue m)     -> BoolValue $ n < m
  _                        -> error "Comparing two incompatible types."
eval (Gth e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntValue n, IntValue m)     -> BoolValue $ n > m
  _                        -> error "Comparing two incompatible types."
eval (And e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (BoolValue b1, BoolValue b2)     -> BoolValue $ b1 && b2
  _                        -> error "Comparing two incompatible types."
eval (Or e1 e2) vtab     = case (eval e1 vtab, eval e2 vtab) of
  (BoolValue b1, BoolValue b2)     -> BoolValue $ b1 || b2
  _                        -> error "Comparing two incompatible types."
eval (Not e) vtab        = case eval e vtab of
  BoolValue b                -> BoolValue $ not b
  _                        -> error "Comparing two incompatible types."
eval (Var  (Variable n)) vtab = fromMaybe (IntValue 0) (lookup n vtab)
eval (Constant v) _ = v
eval (Parens e) vtab = eval e vtab

-- Helper to test implementation
testAST :: AST -> VarTab -> IO ()
testAST ast sstate = do
  let res  = interpAST ast sstate
      ast' = reverseAST ast
      res' = interpAST ast' res
  putStrLn "Starting state:"
  putStrLn $ varTabToString sstate ++ "\n"
  putStrLn $ astToString ast
  putStrLn "\nResult:"
  putStrLn $ varTabToString res
  putStrLn "\n---- Reversed ----\n"
  putStrLn $ astToString ast'
  putStrLn "\nResult:"
  putStrLn $ varTabToString res'
