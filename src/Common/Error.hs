module Common.Error
( Error (..)
) where

data Error e
  = RuntimeError RuntimeError
  | TypeError    TypeError
  | StaticError e
  deriving Show

data RuntimeError = RTE deriving Show
data TypeError    = TE deriving Show
