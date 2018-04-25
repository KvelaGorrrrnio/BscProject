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
showAST' lvl ast = if null ast then showStmt lvl (Skip (0,0)) else intercalate "\n" . map (showStmt lvl) $ ast

showStmt :: Int -> Stmt -> String
showStmt lvl s = case s of
  If t s1 s2 a _ -> indent ++ "if " ++ showPar t ++ " then\n"
                 ++ showAST' (lvl+1) s1 ++ "\n"
                 ++ indent ++ "else\n"
                 ++ showAST' (lvl+1) s2 ++ "\n"
                 ++ indent ++ "fi " ++ showPar a

  Until t s a _ -> indent ++ "from "  ++ showPar t ++ " do\n"
                ++ showAST' (lvl + 1) s ++ "\n"
                ++ indent ++ "until " ++ showPar a

  s            -> indent ++ show s

  where indent = doIndent lvl

type Block = [Stmt]