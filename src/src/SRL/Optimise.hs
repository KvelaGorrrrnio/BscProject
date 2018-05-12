module SRL.Optimise where

import SRL.AST
import SRL.Error

import Common.Optimise

import Debug.Trace

optimise :: Either Error (TypeTab,AST) -> Either Error (TypeTab,AST)
optimise (Left err) = Left err
optimise (Right (ttab,ast)) | ast' <- optStmts ast =
  trace (showAST ttab ast' ++ "\n\n") $
  if ast' == ast
  then Right (ttab,ast')
  else optimise $ Right (ttab,ast')
