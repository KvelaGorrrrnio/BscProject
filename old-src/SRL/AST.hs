module SRL.AST
( AST             (..)
, Identifier      (..)
, Statement       (..)
, AssignOperator  (..)
, Expression      (..)
, Value           (..)
) where

-- SRL specific
--data AST = AST [Statement] -- Change to just type synonym?
--  deriving Show
type AST   = [Statement]
type Block = [Statement]

-- Common
data Identifier
  = Variable String
  | Index String Expression
  deriving Show

data Statement
  = Assignment      Identifier AssignOperator Expression
  | If   Expression AST AST Expression
  | From Expression AST Expression
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
  | Parens    Expression
  deriving Show

data Value
  = IntValue    Int
  | FloatValue  Float
  | BoolValue   Bool
  | StringValue String
  | ListValue   [Value]
  deriving Show

