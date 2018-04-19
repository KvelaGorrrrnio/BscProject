module RL.Error
( Error
, StaticError (..)
, module Common.Error
) where

import Common.AST
import Common.Error

-- =====
-- Errors
-- =====

type Error = CError StaticError

data StaticError
  = SelfAbuse Id
  | StaticVoid -- Used for converting between common and RL error in Type
  deriving Show
