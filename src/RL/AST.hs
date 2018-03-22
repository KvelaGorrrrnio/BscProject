module RL.AST
( AST             (..)
, Block           (..)
, Identifier      (..)
, From            (..)
, Goto            (..)
, Statement       (..)
, UpdateOperator  (..)
, BinOperator     (..)
, UnOperator      (..)
, Expression      (..)
, Value           (..)
, Label
, goto
, LabTab
, genLabels
) where

-- RL specific
data AST = AST [Block] [Block]
  deriving Show

data Block = Block Label From [Statement] Goto
  deriving Show

data From
  = From  Label
  | Fi    Expression Label Label
  | Entry
  deriving Show

data Goto
  = Goto  Label
  | If    Expression Label Label
  | Exit
  deriving Show

type Label = String

-- Common
type Identifier = String

data Statement
  = Update Identifier UpdateOperator Expression
  | Push   Identifier Identifier
  | Pop    Identifier Identifier
  | Swap   Identifier Identifier
  | Skip
  deriving Show

data UpdateOperator
  = PlusEq
  | MinusEq
  | XorEq
  | TimesEq
  | DivideEq
  deriving Show

data Expression
  -- Normal operations
  = Var       Identifier
  | Constant  Value
  | BinOperation BinOperator Expression Expression
  | UnOperation UnOperator Expression
  -- Stack
  | Top   Identifier
  | Empty Identifier
  -- For printing parantheses
  | Parens Expression
  deriving Show

data BinOperator
  = Plus
  | Minus
  | Xor
  | Times
  | Divide
  | Eq
  | Lth
  | Gth
  | And
  | Or
  deriving Show

data UnOperator
  = Not
  | Negate
  deriving Show

-- ændr Expression til kun at have
--    Variable Identifier
--    Literal Value
--    BinOperation
--    UnOperation
--    Top
--    Empty
--    Parens

data Value
  = IntValue    Int
  | BoolValue   Bool
  | StackValue  [Value]
  deriving (Show, Eq)

next :: AST -> AST
next (AST ls (r:rs)) = AST (r:ls) rs

prev :: AST -> AST
prev (AST (l:ls) rs) = AST ls (l:rs)

goto :: Label -> Label -> AST -> LabTab -> AST
goto l1 l2 ast ltab = case (lookup l1 ltab, lookup l2 ltab) of
  (Just n, Just m) | m-n >  0 -> iterate next ast !! (m-n)
  (Just n, Just m) | m-n <  0 -> iterate prev ast !! (n-m)
  (Just n, Just m) | m-n == 0 -> ast
  _                           -> error $ "Label not defined: " ++ l1 ++ " ; " ++ l2

-- LabTab
type LabTab = [(Label, Int)]

genLabels :: AST -> LabTab
genLabels (AST _ bs) =
  fst $ foldl
        (\(lst, n) (Block l _ _ _) -> (lst++[(l,n)],n+1))
        ([],0) bs
