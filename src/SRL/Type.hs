{-# LANGUAGE LambdaCase #-}
module SRL.Type
( typecheck
) where

import SRL.AST
import SRL.Error
import qualified Common.Type as T

typecheck :: AST -> IO T.TypeTab
typecheck ast = case T.typecheck ast T.typecheckStmts of
  Left err  -> print err >> fail "type error"
  Right tab -> return tab

