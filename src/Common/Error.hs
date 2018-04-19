module Common.Error
( CError       (..)
, RuntimeError (..)
, TypeError    (..)
) where

import Common.AST

data CError e
  = RuntimeError RuntimeError
  | TypeError    TypeError
  | StaticError  e
  | Debug        String
  deriving Show

data RuntimeError = RTE deriving Show
data TypeError
  = IncompatibleTypes Type Type
  | BinOpTypes BinOp (Type,Type) (Type,Type)
  | UnOpType   UnOp Type Type
  deriving Show
