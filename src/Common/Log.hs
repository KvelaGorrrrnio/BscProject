module Common.Log where

import Common.AST
import Common.Error

import Data.List (intercalate)

-- ===
-- Log
-- ===

type Log = [Message]

logToString :: Log -> String
logToString = intercalate "\n\n" . map show

logToJSON :: Log -> String
logToJSON = intercalate ",\n\n" . map show

data Message = MsgStmt       Stmt
             | MsgState      VarTab
             | MsgError      RuntimeError
instance Show Message where
  show (MsgStmt s)           = "> " ++ show s
  show (MsgState vtab)       = show vtab
  show (MsgError err)        = "*** Error: " ++show err
