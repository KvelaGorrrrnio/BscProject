module RL.Optimise where

import RL.AST

import Common.Optimise

optimise :: AST -> AST
optimise ast | ast' <- optBlocks ast =
  if ast' == ast
  then ast'
  else optimise ast'

optBlocks :: AST -> AST
optBlocks = map $ \(l,b) -> (l,optBlock b)

optBlock :: Block -> Block
optBlock (f,s,t) = do
  let f' = case f of
        Fi a l1 l2 p -> Fi (rmPar . optCond . optExp $ a) l1 l2 p
        _ -> f
  let s' = optStmts s
  let t' = case t of
        IfTo t l1 l2 p -> case rmPar . optCond . optExp $ t of
          -- Lit (IntV 0) p -> Goto l2 p
          -- Lit (IntV _) p -> Goto l1 p
          t' -> IfTo t' l1 l2 p
        _ -> t
  (f',s',t')
