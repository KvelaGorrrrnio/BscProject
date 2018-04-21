module Common.Inversion where

import Common.AST

invertStmts :: [Stmt] -> [Stmt]
invertStmts = reverse . map invertStmt

invertStmt :: Stmt -> Stmt
invertStmt (Update id op e p) = Update id (invertOp op) e p
invertStmt (Push id1 id2 p)   = Pop  id1 id2 p
invertStmt (Pop  id1 id2 p)   = Push id1 id2 p
invertStmt (If t s1 s2 a p)   = If a (invertStmts s1) (invertStmts s2) t p
invertStmt (Until a s t p)    = Until t (invertStmts s) a p
invertStmt s                  = s

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
invertOp op      = op
