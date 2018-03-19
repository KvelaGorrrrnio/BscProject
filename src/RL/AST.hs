module RL.AST
( AST             (..)
, Block           (..)
, Identifier      (..)
, From            (..)
, Goto            (..)
, Statement       (..)
, AssignOperator  (..)
, Expression      (..)
, Value           (..)
, Label
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

-- Common
type Label = String

data Identifier
  = Variable String
  | Index String Expression
  deriving Show

data Statement
  = Assignment      Identifier AssignOperator Expression
  | Push Identifier Identifier
  | Pop  Identifier Identifier
  | Swap Identifier Identifier
  | Skip
  deriving Show

data AssignOperator
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
  | Neq       Expression Expression
  | Lth       Expression Expression
  | Gth       Expression Expression
  | And       Expression Expression
  | Or        Expression Expression
  | Not       Expression
  | Top       Identifier
  | Empty     Identifier
  deriving Show

data Value
  = IntValue    Int
  | FloatValue  Float
  | BoolValue   Bool
  | StringValue String
  | ListValue   [Value]
  deriving Show

