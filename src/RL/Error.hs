module RL.Error where

type VarName = String
type Type    = String

data ProgError
  = IndexOnNonList VarName
  | IndexNotInteger
  | TestNotBoolean
  | AssertionNotBoolan
  | AssignedVarIsOperand
  | WrongType Type
  | DivByZero
  | DivHasRest
  deriving Show

