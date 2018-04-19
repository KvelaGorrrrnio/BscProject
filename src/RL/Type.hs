{-# LANGUAGE LambdaCase #-}
module RL.Type
( typecheck
, module Common.Type
) where

import RL.AST
import RL.Error
import Common.Type
import Control.Monad.Except

typecheck :: AST -> IO TypeTab
typecheck ast = case runTypecheck ast typecheckBlocks of
  Left err  -> print err >> fail "type error"
  Right tab -> return tab

typecheckBlocks :: AST -> TypeState ()
typecheckBlocks [] = return ()
typecheckBlocks ((_,(f,stmts,t)):ast) = typecheckFrom f >> typecheckStmts stmts >> typecheckTo t

typecheckFrom :: From -> TypeState ()
typecheckFrom (Fi exp _ _ p) = typeof exp >>= \case
  IntT -> return ()
  t    -> throwError $ IncompatibleTypes IntT t p -- TODO: Exp not eval to Int
typecheckFrom _ = return ()

typecheckTo :: To -> TypeState ()
typecheckTo (IfTo exp _ _ p) = typeof exp >>= \case
  IntT -> return ()
  t    -> throwError $ IncompatibleTypes IntT t p
typecheckTo _ = return ()
