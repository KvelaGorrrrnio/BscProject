module SRL.AST
  ( module SRL.AST,
    module Common.AST
  ) where

import Common.AST

-- ===
-- AST
-- ===

type AST = Block
showAST :: TypeTab -> AST -> String
showAST ttab ast = showTypeDecs ttab ++ showBlock 0 ast

doIndent lvl = replicate (2 * lvl) ' '

showBlock :: Int -> Block -> String
showBlock lvl b = case b of
  Step s            -> indent ++ show s
  If t b1 b2 a      -> indent ++ "if " ++ showPar t ++ " then\n"
                    ++ showBlock (lvl+1) b1 ++ "\n"
                    ++ indent ++ "else\n"
                    ++ showBlock (case b2 of If{} -> lvl ; _ -> lvl + 1)  b2 ++ "\n"
                    ++ indent ++ "fi " ++ showPar a

  Until _ t b1 b2 a -> indent ++ "from "  ++ showPar t ++ " do\n"
                    ++ showBlock (lvl + 1) b1 ++ "\n"
                    ++ indent ++ "loop\n"
                    ++ showBlock (lvl + 1) b2 ++ "\n"
                    ++ indent ++ "until " ++ showPar a

  Seq b1 b2         -> showBlock lvl b1 ++ "\n"
                    ++ showBlock lvl b2

  where indent = doIndent lvl

data Block = Step Stmt
           | If Exp Block Block Exp
           | Until Bool Exp Block Block Exp
           | Seq Block Block
