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
type Label = String
type Var   = String

data AST = AST [Block] [Block] deriving Show

data Block = Block Label From [Inst] To deriving Show

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
  | Skip
  deriving Show

-- data Var    -- TODO: implement this everywhere
--   = Sgl String
--   | Lst String Exp

data Value
  = IntVal   Int
  | FloatVal Float
  | BoolVal  Bool
  | ListVal  [Value]
  deriving Show

data From
    = From Label
    | Fi Exp Label Label
    | Entry
    deriving Show

data To
    = Goto Label
    | If Exp Label Label
    | Exit
    deriving Show

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
  instsToString insts ++ "\n" ++
  toToString t

labelToString :: Label -> String
labelToString l = l++":"

fromToString :: From -> String
fromToString (From l)     = "from " ++ l
fromToString (Fi e l1 l2) = "fi (" ++ expToString e ++ ") " ++ l1 ++ " " ++ l2
fromToString Entry = "entry"

instsToString :: [Inst] -> String
instsToString = intercalate "\n  " . map instToString

instToString :: Inst -> String
instToString (Swap n1 n2)  = n1 ++ " <=> " ++ n2
instToString (PlusEq  n e) = n ++ " += " ++ expToString e
instToString (MinusEq n e) = n ++ " -= " ++ expToString e
instToString (XOREq n e)   = n ++ " ^= " ++ expToString e
instToString Skip          = "skip"

toToString :: To -> String
toToString (Goto l)     = "goto " ++ l
toToString (If e l1 l2) = "if (" ++ expToString e ++ ") " ++ l1 ++ " " ++ l2
toToString Exit         = "exit"

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
reverseAST (AST [] blocks) = AST [] $ (map reverseBlock . reverse) blocks

reverseBlock :: Block -> Block
reverseBlock (Block l f insts t) = do
  let f'     = reverseTo t
      t'     = reverseFrom f
      insts' = reverseInsts insts
  Block l f' insts' t'

reverseTo :: To -> From
reverseTo (Goto ln)      = From ln
reverseTo (If e ltt ltf) = Fi e ltt ltf
reverseTo  Exit          = Entry

reverseFrom :: From -> To
reverseFrom (From l)       = Goto l
reverseFrom (Fi e ltt ltf) = If e ltt ltf
reverseFrom Entry          = Exit

reverseInsts :: [Inst] -> [Inst]
reverseInsts = map reverseInst . reverse

reverseInst :: Inst -> Inst
reverseInst (Swap  n1 n2)  = Swap n1 n2
reverseInst (PlusEq  n e)  = MinusEq n e
reverseInst (MinusEq n e)  = PlusEq  n e
reverseInst (XOREq   n e)  = XOREq   n e
reverseInst Skip           = Skip

-- data Inst
--  = Swap Var Var
--  | PlusEq  Var  Exp
--  | MinusEq Var  Exp
--  | XOREq   Var  Exp
--  deriving Show

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
        BoolVal True  -> interpAST' (goto l ltt ast ltab) vtab' ltab
        BoolVal False -> interpAST' (goto l ltf ast ltab) vtab' ltab
        _             -> error "Type error in if then else"
    where vtab' = interpInsts insts vtab

-- interpreting list of instructions
interpInsts :: [Inst] -> VarTab -> VarTab
interpInsts insts vtab = foldl (flip interpInst) vtab insts

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
  Skip             -> vtab

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
  putStrLn $ astToString ast
  putStrLn "\nResult:"
  putStrLn $ varTabToString res
  putStrLn "\n---- Reversed ----\n"
  putStrLn $ astToString ast'
  putStrLn "\nResult:"
  putStrLn $ varTabToString res'

-- Main

main = do
-- A sample AST
-- data Block = Block Label From [Inst] To deriving Show
let ast = toAST
          [
            Block
            "init"
            Entry
            [ PlusEq "c" (Const $ IntVal 20) ]
            (Goto "test")
          ,
            Block
            "test"
            (Fi (Eq (Var "c") (Const $ IntVal 20)) "init" "loop_body")
            [ Skip ]
            (If (Eq (Var "c") (Const $ IntVal 0)) "end" "loop_body")
          ,
            Block
            "loop_body"
            (From "test")
            [
              PlusEq  "a" (Const $ IntVal 3),
              MinusEq "c"   (Const $ IntVal 1)
            ]
            (Goto "test")
          ,
            Block
            "end"
            (From "loop_body")
            [
              Swap "a" "c"
            ]
            Exit
          ]


testAST ast [("a", IntVal 11)]
