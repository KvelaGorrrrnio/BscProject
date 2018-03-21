module RL.AST
( AST             (..)
, Block           (..)
, Identifier      (..)
, From            (..)
, Goto            (..)
, Statement       (..)
, UpdateOperator  (..)
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
  deriving Show

data Expression
  = Var       Identifier
  | Constant  Value
  | Plus      Expression Expression
  | Minus     Expression Expression
  | Xor       Expression Expression
  | Times     Expression Expression
  | Divide    Expression Expression
  | Eq        Expression Expression
  | Lth       Expression Expression
  | Gth       Expression Expression
  | And       Expression Expression
  | Or        Expression Expression
  | Not       Expression
  | Top       Identifier
  | Empty     Identifier
  | Parens    Expression
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
  deriving Show

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
