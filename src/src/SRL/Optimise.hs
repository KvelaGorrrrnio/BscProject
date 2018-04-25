module SRL.Optimise where
import Debug.Trace

import SRL.AST -- (AST)
import SRL.Error

import Common.Optimise

optimise :: Either Error AST -> Either Error AST
optimise (Left err) = Left err
optimise (Right ast) | ast' <- optStmts ast =
  -- trace (showAST ast' ++ "\n\n") $
  if ast' == ast
  then Right ast'
  else optimise $ Right ast'