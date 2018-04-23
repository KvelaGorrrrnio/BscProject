module SRL.Optimise where
import Debug.Trace

import SRL.AST -- (AST)

import Common.Optimise

optimise :: AST -> AST
optimise ast | ast' <- optStmts ast =
  trace (showAST ast' ++ "\n\n") $
    if ast' == ast
    then ast'
    else optimise ast'
