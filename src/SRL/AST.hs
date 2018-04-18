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
showAST :: AST -> String
showAST = showAST' 0

showAST' :: Int -> AST -> String
showAST' lvl ast = case ast of
  If t s1 s2 a : ss -> indent ++ "if " ++ show t ++ " then\n"
                    ++ showAST' (lvl+1) s1
                    ++ indent ++ "else\n"
                    ++ showAST' (lvl+1) s2
                    ++ indent ++ "fi " ++ show a ++ "\n"
                    ++ showAST' lvl ss
  Until t s a : ss  -> indent ++ "from "  ++ show t ++ " do\n"
                    ++ showAST' (lvl + 1) s
                    ++ indent ++ "until " ++ show a ++ "\n"
                    ++ showAST' lvl ss
  s : ss            -> indent ++ show s ++ "\n"
                    ++ showAST' lvl ss
  []                -> []

  where indent = replicate (2 * lvl) ' '

type Block = [Stmt]
