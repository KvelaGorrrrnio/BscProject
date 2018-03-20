module RL.Reversion ( reverseAST ) where
import RL.AST

-- Reversion
reverseAST :: AST -> AST
reverseAST (AST [] blocks) = AST [] $ (map reverseBlock . reverse) blocks

reverseBlock :: Block -> Block
reverseBlock (Block l f insts t) = do
  let f'     = reverseTo t
      t'     = reverseFrom f
      insts' = reverseInsts insts
  Block l f' insts' t'

reverseTo :: Goto -> From
reverseTo (Goto ln)      = From ln
reverseTo (If e ltt ltf) = Fi e ltt ltf
reverseTo  Exit          = Entry

reverseFrom :: From -> Goto
reverseFrom (From l)       = Goto l
reverseFrom (Fi e ltt ltf) = If e ltt ltf
reverseFrom Entry          = Exit

reverseInsts :: [Statement] -> [Statement]
reverseInsts = map reverseInst . reverse

reverseInst :: Statement -> Statement
reverseInst (Assignment var op e) = Assignment var (reverseOp op) e
reverseInst (Swap n1 n2)          = Swap n1 n2
reverseInst Skip                  = Skip

reverseOp :: AssignOperator -> AssignOperator
reverseOp PlusEq  = MinusEq
reverseOp MinusEq = PlusEq
reverseOp XorEq   = XorEq
