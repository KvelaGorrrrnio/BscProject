module SRL.Optimise where

import SRL.AST

import Common.Optimise

optimise :: AST -> AST
optimise ast | ast' <- optStmts ast =
  if ast' == ast
  then ast'
  else optStmts ast'
