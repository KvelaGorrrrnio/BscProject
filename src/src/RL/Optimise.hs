module RL.Optimise where

import Debug.Trace

import RL.AST
import RL.Error

import Common.Optimise

optimise :: Either Error (TypeTab,AST) -> Either Error (TypeTab,AST)
optimise (Left err) = Left err
optimise (Right (ttab,ast)) | ast' <- optBlocks ast =
  if ast' == ast
  then Right (ttab,ast')
  else optimise $ Right (ttab,ast')

optBlocks :: AST -> AST
optBlocks = map $ \(l,b) -> (l,optBlock b)

optBlock :: Block -> Block
optBlock (f,s,t) = do
  let f' = case f of
        Fi a l1 l2 p -> Fi (rmPar . optCond . optExp $ a) l1 l2 p
        _ -> f
  let s' = optStmts s
  let t' = case t of
        If t l1 l2 p -> If (rmPar . optCond . optExp $ t) l1 l2 p
        _ -> t
  (f',s',t')
