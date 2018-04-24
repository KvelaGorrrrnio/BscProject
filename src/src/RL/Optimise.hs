module RL.Optimise where

import Debug.Trace

import RL.AST
import RL.Error

import Common.Optimise

optimise :: Either Error AST -> Either Error AST
optimise (Left err) = Left err
optimise (Right ast) | ast' <- optBlocks ast =
  -- trace (showAST ast' ++ "\n\n") $
  if ast' == ast
  then Right ast'
  else optimise $ Right ast'

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
