module SRL.Optimise where

import SRL.AST
import SRL.Error

import Common.Optimise

-- optimise :: Either Error (TypeTab,AST) -> Either Error (TypeTab,AST)
-- optimise (Left err) = Left err
-- optimise (Right (ttab,ast)) | ast' <- optStmts ast =
--   if ast' == ast
--   then Right (ttab,ast')
--   else optimise $ Right (ttab,ast')

-- optStmt (If t s1 s2 a p) = case rmPar . optExp $ t of
--   Lit (IntV 0) _ -> optStmts s2
--   Lit (IntV _) _ -> optStmts s1
--   t'             -> [If t' (optStmts s1) (optStmts s2) (rmPar . optExp $ a) p]
-- optStmt (Until d a s t p)  = case rmPar . optExp $ t of
--   Lit (IntV n) _ | n/=0 -> optStmts s
--   t'             -> [Until d (rmPar . optExp $ a) (optStmts s) t' p]
