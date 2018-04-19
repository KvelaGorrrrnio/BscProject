module RL.Inversion
( invert
) where

import RL.AST

invert :: AST -> AST
invert = reverse . map (\(l,b) -> (l, invertBlock b))

invertBlock :: Block -> Block
invertBlock (f,s,t) = (invertTo t, (map invertStmt . reverse) s, invertFrom f)

invertStmt :: Stmt -> Stmt
invertStmt (Update id op e p) = Update id (invertOp op) e p
invertStmt (Push id1 id2 p)   = Pop  id1 id2 p
invertStmt (Pop  id1 id2 p)   = Push id1 id2 p
invertStmt s                = s

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
invertOp op      = op

invertTo :: To -> From
invertTo (Goto l)     = From l
invertTo (IfTo e l1 l2) = Fi e l1 l2
invertTo Exit         = Entry

invertFrom :: From -> To
invertFrom (From l)     = Goto l
invertFrom (Fi e l1 l2) = IfTo e l1 l2
invertFrom Entry        = Exit
