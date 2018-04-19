{-# LANGUAGE LambdaCase #-}
module SRL.Type
( typecheck
) where

import SRL.AST
import SRL.Error
import Common.Error (Error(..))
import qualified Common.Type as T

typecheck :: AST -> Either SRL.Error.Error T.TypeTab
typecheck ast = case T.typecheck ast T.typecheckStmts of
  Right tab            -> Right tab
  Left (TypeError err) -> Left $ TypeError $ err
  _                    -> Left $ StaticError $ StaticVoid

