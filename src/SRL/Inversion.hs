module SRL.Inversion ( inverseAST ) where
import SRL.AST

-- Reversion
inverseAST :: AST -> AST
inverseAST = map inverseInst . reverse

inverseInst :: Statement -> Statement
inverseInst (Assignment var op e) = Assignment var (inverseOp op) e
inverseInst (If t b1 b2 a)        = If a (inverseAST b1) (inverseAST b2) t
inverseInst (From a b t)          = From t (inverseAST b) a
inverseInst (Swap  n1 n2)         = Swap n1 n2
inverseInst Skip                  = Skip

inverseOp :: AssignOperator -> AssignOperator
inverseOp PlusEq  = MinusEq
inverseOp MinusEq = PlusEq
inverseOp XorEq   = XorEq
