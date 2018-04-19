module SRL.Error
( StaticError (..)
, module Common.Error
) where

import Common.AST
import Common.Error

-- =====
-- Errors
-- =====

data StaticError
  = SelfAbuse Id
  | StaticVoid -- Used for converting between common and RL error in Type
  deriving Show
