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

data Message = MsgStmt    Stmt
             | MsgState   VarTab
             | MsgError   Error
instance Show Message where
  show (MsgStmt s)           = "> " ++ show s
  show (MsgState vtab)       = show vtab
  show (MsgError err)        = "*** Error: " ++show err


-- ===
-- AST
-- ===

type AST   = [Stmt]
showAST :: AST -> String
showAST = showAST' 0

doIndent lvl = replicate (2 * lvl) ' '

showAST' :: Int -> AST -> String
showAST' lvl = intercalate "\n" . map (showStmt lvl)

showStmt :: Int -> Stmt -> String
showStmt lvl s = case s of
  If t s1 s2 a -> indent ++ "if " ++ show t ++ " then\n"
               ++ showAST' (lvl+1) s1 ++ "\n"
               ++ indent ++ "else\n"
               ++ showAST' (lvl+1) s2 ++ "\n"
               ++ indent ++ "fi " ++ show a

  Until t s a  -> indent ++ "from "  ++ show t ++ " do\n"
               ++ showAST' (lvl + 1) s ++ "\n"
               ++ indent ++ "until " ++ show a

  s            -> indent ++ show s

  where indent = doIndent lvl

type Block = [Stmt]
