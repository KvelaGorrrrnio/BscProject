module Inversion where

import AST

invert :: AST -> AST
invert = revAST . mapAST (\(l,b) -> (l,invertBlock b))

invertBlock :: Block -> Block
invertBlock (Block (f,s,t)) = Block (invertTo t, (map invertStmt . reverse) s, invertFrom f)

invertStmt :: Stmt -> Stmt
invertStmt (Update id op e) = Update id (invertOp op) e
invertStmt (Push id1 id2)   = Pop  id1 id2
invertStmt (Pop  id1 id2)   = Push id1 id2
invertStmt s                = s

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
invertOp op      = op


invertTo :: To -> From
invertTo (Goto l)     = From l
invertTo (If e l1 l2) = Fi e l1 l2
invertTo Exit         = Entry

invertFrom :: From -> To
invertFrom (From l)     = Goto l
invertFrom (Fi e l1 l2) = If e l1 l2
invertFrom Entry        = Exit
