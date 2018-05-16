module RL.Inversion
( invert
) where

import RL.AST

import Common.Inversion

invert :: AST -> AST
invert = reverse . map (\(l,b) -> (l, invertBlock b))

invertBlock :: Block -> Block
invertBlock (f,s,j) = (invertJump j, invertStmts s, invertFrom f)

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
invertOp op      = op

invertJump :: Jump -> From
invertJump (Goto l p)     = From l p
invertJump (If e l1 l2 p) = Fi e l1 l2 p
invertJump (Exit p)       = Entry p

invertFrom :: From -> Jump
invertFrom (From l p)     = Goto l p
invertFrom (Fi e l1 l2 p) = If e l1 l2 p
invertFrom (Entry p)      = Exit p

invertStmts :: [Stmt] -> [Stmt]
invertStmts = reverse . map invertStmt
