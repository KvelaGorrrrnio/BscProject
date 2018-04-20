module RL.Inversion
( invert
) where

import RL.AST

import Common.Inversion

invert :: AST -> AST
invert = reverse . map (\(l,b) -> (l, invertBlock b))

invertBlock :: Block -> Block
invertBlock (f,s,t) = (invertTo t, invertStmts s, invertFrom f)

invertOp :: UpdOp -> UpdOp
invertOp PlusEq  = MinusEq
invertOp MinusEq = PlusEq
invertOp MultEq  = DivEq
invertOp DivEq   = MultEq
invertOp op      = op

invertTo :: To -> From
invertTo (Goto l p)       = From l p
invertTo (IfTo e l1 l2 p) = Fi e l1 l2 p
invertTo (Exit p)         = Entry p

invertFrom :: From -> To
invertFrom (From l p)     = Goto l p
invertFrom (Fi e l1 l2 p) = IfTo e l1 l2 p
invertFrom (Entry p)      = Exit p
