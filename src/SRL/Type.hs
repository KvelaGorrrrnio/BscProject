{-# LANGUAGE LambdaCase #-}
module SRL.Type
( typecheck
) where

import SRL.AST
import SRL.Error
import Common.Error (CError(..))
import qualified Common.Type as T

typecheck :: AST -> Either Error T.TypeTab
typecheck ast = case T.typecheck ast T.typecheckStmts of
  Right tab            -> Right tab
  Left (TypeError p err) -> Left $ TypeError p $ err
  _                    -> Left $ StaticError $ StaticVoid

