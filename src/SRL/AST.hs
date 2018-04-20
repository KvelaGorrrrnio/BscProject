module SRL.AST
  ( module SRL.AST,
    module Common.AST
  ) where

import Data.Bits (xor)
import Data.List (intercalate)

import Common.AST
import SRL.Error

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
  If t s1 s2 a _ -> indent ++ "if " ++ show t ++ " then\n"
               ++ showAST' (lvl+1) s1 ++ "\n"
               ++ indent ++ "else\n"
               ++ showAST' (lvl+1) s2 ++ "\n"
               ++ indent ++ "fi " ++ show a

  Until _ t s a _ -> indent ++ "from "  ++ show t ++ " do\n"
               ++ showAST' (lvl + 1) s ++ "\n"
               ++ indent ++ "until " ++ show a

  s            -> indent ++ show s

  where indent = doIndent lvl

type Block = [Stmt]
