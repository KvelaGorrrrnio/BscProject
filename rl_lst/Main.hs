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
  = Swap String String
  | PlusEq  Var  Exp
  | MinusEq Var  Exp
  | XOREq   Var  Exp
  | Skip
  deriving Show

data Var    -- TODO: implement this everywhere
  = Scl String
  | Idx String Exp
  deriving Show

data Value
  = IntVal   Int
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
astToString (AST _ blocks) = (intercalate "\n\n" . map blockToString) blocks

blockToString :: Block -> String
blockToString (Block l f insts t) =
  labelToString l     ++ ": "  ++
  fromToString  f     ++ "\n  " ++
  instsToString insts ++ "\n" ++
  toToString t

labelToString :: Label -> String
labelToString = cyan

fromToString :: From -> String
fromToString (From l)     = green "from " ++ labelToString l
fromToString (Fi e l1 l2) = green "fi"    ++ " (" ++ expToString e ++ ") " ++ labelToString l1 ++ " " ++ labelToString l2
fromToString Entry        = green "entry"

instsToString :: [Inst] -> String
instsToString = intercalate "\n  " . map instToString

instToString :: Inst -> String
instToString (Swap n1 n2)  = n1 ++ " <=> " ++ n2
instToString (PlusEq  v e) = varToString v ++ " += " ++ expToString e
instToString (MinusEq v e) = varToString v ++ " -= " ++ expToString e
instToString (XOREq v e)   = varToString v ++ " ^= " ++ expToString e
instToString Skip          = yellow "skip"

varToString :: Var -> String
varToString (Scl n)   = n
varToString (Idx n e) = n ++ "[" ++ expToString e ++ "]"

toToString :: To -> String
toToString (Goto l)     = yellow "goto " ++ labelToString l
toToString (If e l1 l2) = yellow "if" ++ " (" ++ expToString e ++ ") " ++ labelToString l1 ++ " " ++ labelToString l2
toToString Exit         = yellow "exit"

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
expToString (Var v)        = varToString v

valueToString :: Value -> String
valueToString (IntVal   n)  = show n
valueToString (BoolVal b)   = show b
valueToString (ListVal lst) = "[" ++ (intercalate "," . map valueToString) lst ++ "]"

-- For color highlighting
black   s = "\x1b[30m" ++ s ++ "\x1b[0m"
red     s = "\x1b[31m" ++ s ++ "\x1b[0m"
green   s = "\x1b[32m" ++ s ++ "\x1b[0m"
yellow  s = "\x1b[33m" ++ s ++ "\x1b[0m"
blue    s = "\x1b[34m" ++ s ++ "\x1b[0m"
magenta s = "\x1b[35m" ++ s ++ "\x1b[0m"
cyan    s = "\x1b[36m" ++ s ++ "\x1b[0m"
white   s = "\x1b[37m" ++ s ++ "\x1b[0m"

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
reverseInst (PlusEq  v e)  = MinusEq v e
reverseInst (MinusEq v e)  = PlusEq  v e
reverseInst (XOREq   v e)  = XOREq   v e
reverseInst Skip           = Skip

-- VarTab
type VarTab = [(String, Value)]

update :: String -> Value -> VarTab -> VarTab
update name vn vtab = case vtab of
      (n,vo) : bs
        | n == name  -> (n,vn) : bs
        | otherwise  -> (n,vo) : update name vn bs
      [] -> error $ "Variable not defined: " ++ name

bind :: String -> Value -> VarTab -> VarTab
bind name value vtab = (name,value):vtab

varTabToString :: VarTab -> String
varTabToString = intercalate "\n" . map (\(n,v) -> n ++ " -> " ++ valueToString v)

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
  -- Scalar
  PlusEq (Scl name) exp -> case (lookup name vtab, eval exp vtab) of
    (Just (IntVal   n), IntVal m) -> update name (IntVal $ n + m) vtab
    (Nothing, IntVal   m)         -> bind   name (IntVal m) vtab
    _                           -> error "Operands not of correct type"
  MinusEq (Scl name) exp -> case (lookup name vtab, eval exp vtab) of
    (Just (IntVal   n), IntVal   m) -> update name (IntVal   $ n - m) vtab
    (Nothing, IntVal   m)         -> bind   name (IntVal (-m)) vtab
    _                           -> error "Operands not of correct type"
  XOREq   (Scl name) exp -> case (lookup name vtab, eval exp vtab) of
    (Just (IntVal n), IntVal m) -> update name (IntVal $ xor n m) vtab
    (Nothing, IntVal m)         -> bind   name (IntVal m) vtab
    _                           -> error "Operands not of correct type"
  -- Lists
  PlusEq (Idx name e) exp -> case (eval e vtab, eval exp vtab) of
    (IntVal ind, IntVal n)    -> case lookup name vtab of
      Just (ListVal lst)      -> case index name ind vtab of
        Nothing         -> update name (ListVal (updateIdx lst ind (IntVal n)))   vtab
        Just (IntVal m) -> update name (ListVal (updateIdx lst ind (IntVal (m+n)))) vtab
        _               -> error "List elements must be integers."
      Nothing                 -> (interpInst i . bind name (ListVal [])) vtab
      _                 -> error "Variable is not a list."
    _                   -> error "Index and expression must be integers."
  MinusEq (Idx name e) exp -> case (eval e vtab, eval exp vtab) of
    (IntVal ind, IntVal n)    -> case lookup name vtab of
      Just (ListVal lst)      -> case index name ind vtab of
        Nothing         -> update name (ListVal (updateIdx lst ind (IntVal (-n))))   vtab
        Just (IntVal m) -> update name (ListVal (updateIdx lst ind (IntVal (m-n)))) vtab
        _               -> error "List elements must be integers."
      Nothing                 -> (interpInst i . bind name (ListVal [])) vtab
      _                 -> error "Variable is not a list."
    _                   -> error "Index and expression must be integers."
  XOREq (Idx name e) exp -> case (eval e vtab, eval exp vtab) of
    (IntVal ind, IntVal n)    -> case lookup name vtab of
      Just (ListVal lst)      -> case index name ind vtab of
        Nothing         -> update name (ListVal (updateIdx lst ind (IntVal n)))   vtab
        Just (IntVal m) -> update name (ListVal (updateIdx lst ind (IntVal (xor m n)))) vtab
        _               -> error "List elements must be integers."
      Nothing                 -> (interpInst i . bind name (ListVal [])) vtab
      _                 -> error "Variable is not a list."
    _                   -> error "Index and expression must be integers."
  -- Rest
  Swap name1 name2 -> case (lookup name1 vtab, lookup name2 vtab) of
    (Just v1, Just v2) -> (update name2 v1 . update name1 v2) vtab
    _ -> error "Variable is not defined"
  Skip             -> vtab

updateIdx :: [Value] -> Int -> Value -> [Value]
updateIdx lst i v
  | length lst  > i = take i lst ++ [v] ++ drop (i + 1) lst
  | otherwise      = lst ++ replicate (i - length lst) (IntVal 0) ++ [v]

-- evaluating an expression
eval :: Exp -> VarTab -> Value
eval (Plus e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n + m
  _                        -> error "Adding two incompatible types."
eval (Minus e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n - m
  _                        -> error "Subtracting two incompatible types."
eval (Times e1 e2) vtab  = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n * m
  _                        -> error "Subtracting two incompatible types."
eval (Divide e1 e2) vtab = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> IntVal   $ n `div` m
  _                        -> error "Subtracting two incompatible types."
eval (Eq  e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> BoolVal $ n == m
  _                        -> error "Comparing two incompatible types."
eval (Lth e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> BoolVal $ n < m
  _                        -> error "Comparing two incompatible types."
eval (Gth e1 e2) vtab    = case (eval e1 vtab, eval e2 vtab) of
  (IntVal n, IntVal m)     -> BoolVal $ n > m
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
eval (Var  (Scl n))   vtab = fromMaybe (IntVal 0) (lookup n vtab)
eval (Var  (Idx n e)) vtab = case eval e vtab of
  IntVal i -> fromMaybe (IntVal 0) (index n i vtab)
  _        -> error "Index must be an integer."
eval (Const v) _ = v

index :: String -> Int -> VarTab -> Maybe Value
index name i vtab = case lookup name vtab of
  Nothing                             -> Nothing
  Just (ListVal lst) | length lst > i -> Just $ lst !! i
                     | otherwise      -> Nothing
  _                 -> error "Indexing on non-list."

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
            [
              Skip
            ]
            (Goto "test")
          ,
            Block
            "test"
            (Fi (Eq (Var (Scl "c")) (Const $ IntVal 0)) "init" "loop_body")
            [ Skip ]
            (If (Eq (Var (Scl "c")) (Const $ IntVal 10)) "end" "loop_body")
          ,
            Block
            "loop_body"
            (From "test")
            [
              PlusEq (Idx "lst" (Var $ Scl "c")) (Times (Var $ Scl "c") (Var $ Scl "c")),
              PlusEq (Scl "c")   (Const $ IntVal 1)
            ]
            (Goto "test")
          ,
            Block
            "end"
            (From "loop_body")
            [
              PlusEq (Idx "lst" (Var $ Scl "c")) (Times (Var $ Scl "c") (Var $ Scl "c"))
            ]
            Exit
          ]


testAST ast []
