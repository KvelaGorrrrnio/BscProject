module RL.Inversion ( inverseAST ) where
import RL.AST

-- Reversion
inverseAST :: AST -> AST
inverseAST (AST [] blocks) = AST [] $ (map inverseBlock . reverse) blocks

inverseBlock :: Block -> Block
inverseBlock (Block l f insts t) = do
  let f'     = inverseGoTo t
      t'     = inverseFrom f
      insts' = inverseInsts insts
  Block l f' insts' t'

inverseGoTo :: Goto -> From
inverseGoTo (Goto ln)      = From ln
inverseGoTo (If e ltt ltf) = Fi e ltt ltf
inverseGoTo  Exit          = Entry

inverseFrom :: From -> Goto
inverseFrom (From l)       = Goto l
inverseFrom (Fi e ltt ltf) = If e ltt ltf
inverseFrom Entry          = Exit

inverseInsts :: [Statement] -> [Statement]
inverseInsts = map inverseInst . reverse

inverseInst :: Statement -> Statement
inverseInst (Update var op e) = Update var (inverseOp op) e
inverseInst (Swap n1 n2)          = Swap n1 n2
inverseInst Skip                  = Skip

inverseOp :: UpdateOperator -> UpdateOperator
inverseOp PlusEq  = MinusEq
inverseOp MinusEq = PlusEq
inverseOp XorEq   = XorEq
