module RL.AST
( AST             (..)
, Block           (..)
, Identifier
, From            (..)
, Goto            (..)
, Statement       (..)
, AssignOperator  (..)
, Expression      (..)
, Value           (..)
) where

-- RL specific
data AST = AST [Block] [Block]
  deriving Show

data Block = Block Identifier From [Statement] Goto
  deriving Show

data From
  = From  Identifier
  | Fi    Expression Identifier Identifier
  | Entry
  deriving Show

data Goto
  = Goto  Identifier
  | If    Expression Identifier Identifier
  | Exit
  deriving Show

-- Common
type Identifier = String

data Statement
  = Assignment      Identifier AssignOperator Expression
  | IndexAssignment Identifier Expression AssignOperator Expression
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
  | Index     Identifier Expression
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
  | Not       Expression Expression
  | Top       Identifier
  | Empty     Identifier
  deriving Show

data Value
  = IntValue    Int
  | FloatValue  Float
  | BoolValue   Bool
  | StringValue String
  | ListVal     [Value]
  deriving Show

