module Common.Inversion where

import Common.AST

invertStmt :: Stmt -> Stmt
invertStmt (Update id op e p) = Update id (invertOp op) e p
invertStmt (Push id1 id2 p)   = Pop  id1 id2 p
invertStmt (Pop  id1 id2 p)   = Push id1 id2 p
invertStmt (If t s1 s2 a p)   = If a s1 s2 t p
invertStmt (Until d a s t p)  = Until d t s a p
invertStmt s                  = s

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
invertOp op      = op
