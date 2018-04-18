module SRL.AST
  ( module SRL.AST,
    module Common.AST
  ) where

import Data.Bits (xor)
import Data.List (intercalate)

import Common.AST

import SRL.Error

-- ===
-- Log
-- ===

type Log = [Message]

logToString :: Log -> String
logToString = intercalate "\n\n" . map show

logToJSON :: Log -> String
logToJSON = intercalate ",\n\n" . map show

data Message = Stmt       Stmt
             | State      VarTab
             | Error      Error
instance Show Message where
  show (Stmt s)           = "> " ++ show s
  show (State vtab)       = show vtab
  show (Error err)        = "*** Error: " ++ err


-- ===
-- AST
-- ===

type AST   = [Stmt]
type Block = [Stmt]
