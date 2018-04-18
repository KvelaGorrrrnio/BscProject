module Common.Error
( Error        (..)
, RuntimeError (..)
, TypeError    (..)
) where

import Common.AST

data Error e
  = RuntimeError RuntimeError
  | TypeError    TypeError
  | StaticError  e
  deriving Show

data RuntimeError = RTE deriving Show
data TypeError
  = IncompatibleTypes Type Type
  | BinOpTypes BinOp (Type,Type) (Type,Type)
  | UnOpType   UnOp Type Type
  deriving Show
