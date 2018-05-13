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
  Atom s          -> indent ++ show s
  If t b1 b2 a _  -> indent ++ "if " ++ showPar t ++ " then\n"
                  ++ showBlock (lvl+1) b1 ++ "\n"
                  ++ indent ++ "else\n"
                  ++ showBlock (lvl+1) b2 ++ "\n"
                  ++ indent ++ "fi " ++ showPar a

  Until _ t b a _ -> indent ++ "from "  ++ showPar t ++ " do\n"
                  ++ showBlock (lvl + 1) b ++ "\n"
                  ++ indent ++ "until " ++ showPar a

  Seq b1 b2       -> showBlock lvl b1 ++ "\n"
                  ++ showBlock lvl b2

  where indent = doIndent lvl

data Block = Atom Stmt
           | If Exp Block Block Exp Pos
           | Until Bool Exp Block Exp Pos -- add the loop part?
           | Seq Block Block -- [Block] -- Block Block
           deriving Show
