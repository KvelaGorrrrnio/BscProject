module SRL.Optimise where

import SRL.AST (AST)
import SRL.Error

import Common.Optimise

optimise :: Either Error AST -> Either Error AST
optimise (Left err) = Left err
optimise (Right ast) | ast' <- optStmts ast =
  if ast' == ast
  then Right ast'
  else Right $ optStmts ast'
