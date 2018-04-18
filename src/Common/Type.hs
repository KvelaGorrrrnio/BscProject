module RL.Type
(
) where

import Error
import AST

-- =======
-- Helpers
-- =======
unify :: Type -> Type -> Maybe Type
unify UnknownT t            = Just t
unify t UnknownT            = Just t
unify (ListT t1) (ListT t2) = case unify t1 t2 of
  Nothing -> Nothing
  Just t  -> Just $ ListT t
unify t1 t2 | t1 == t2      = Just t1
            | otherwise     = Nothing
