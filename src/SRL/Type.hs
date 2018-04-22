{-# LANGUAGE LambdaCase #-}
module SRL.Type
( typecheck
, module Common.Type
) where

import SRL.AST
import SRL.Error
import Common.Type

typecheck :: AST -> IO TypeTab
typecheck ast = case runTypecheck ast typecheckStmts of
  Left err  -> print err >> fail "type error"
  Right tab -> case runTypecheckWith ast typecheckStmts tab of
    Left err  -> print err >> fail "type error"
    Right tab -> return tab

