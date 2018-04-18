module Common.Error
( Error (..)
) where

Error e
  = RuntimeError RuntimeError
  | TypeError    TypeError
  | StaticError e
