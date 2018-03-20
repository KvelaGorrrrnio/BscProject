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

instance Show ProgError where
  show (IndexOnNonList n)   = "Tried to index on variable '" ++ n ++ "' which is not a list."
  show IndexNotInteger      = "Index must be an integer."
  show TestNotBoolean       = "Test must be boolean."
  show AssertionNotBoolan   = "Assertion must be boolean."
  show AssignedVarIsOperand = "The assigned variable occurs on the right-hand side of the binding."
  show (WrongType t)        = "Expected type " ++ t ++ "."
  show DivByZero            = "Tried to divide by zero."
  show DivHasRest           = "Division must not have a rest."
