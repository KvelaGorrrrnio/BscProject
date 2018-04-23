{-# LANGUAGE LambdaCase #-}
module SRL.Type
( typecheck
, module Common.Type
) where

import SRL.AST
import SRL.Error
import Common.Type

typecheck :: AST -> Either Error TypeTab
typecheck ast = case runTypecheck ast typecheckStmts of
  Left err  -> Left err
  Right tab -> case runTypecheckWith ast typecheckStmts tab of
    Left err  -> Left err
    Right tab -> Right tab

