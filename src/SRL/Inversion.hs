module SRL.Inversion
( invert
) where

import SRL.AST

invert :: AST -> AST
invert = reverse . map invertStmt

invertStmt :: Stmt -> Stmt
invertStmt (Update id op e) = Update id (invertOp op) e
invertStmt (Push id1 id2)   = Pop  id1 id2
invertStmt (Pop  id1 id2)   = Push id1 id2
invertStmt (If t s1 s2 a)   = If a s1 s2 t
invertStmt (Until a s t)    = Until t s a
invertStmt s                = s

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
invertOp op      = op
