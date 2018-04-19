{-# LANGUAGE LambdaCase #-}
module RL.Type
( typecheck
) where

import RL.AST
import RL.Error
import qualified Common.Type as T
import Control.Monad.Except

typecheck :: AST -> IO T.TypeTab
typecheck ast = case T.typecheck ast typecheckBlocks of
  Left err  -> print err >> fail "type error"
  Right tab -> return tab

typecheckBlocks :: AST -> T.TypeState ()
typecheckBlocks [] = return ()
typecheckBlocks ((_,(f,stmts,t)):ast) = typecheckFrom f >> T.typecheckStmts stmts >> typecheckTo t

typecheckFrom :: From -> T.TypeState ()
typecheckFrom (Fi exp _ _ p) = T.typeof exp >>= \case
  IntT -> return ()
  t    -> throwError $ IncompatibleTypes IntT t p -- TODO: Exp not eval to Int
typecheckFrom _ = return ()

typecheckTo :: To -> T.TypeState ()
typecheckTo (IfTo exp _ _ p) = T.typeof exp >>= \case
  IntT -> return ()
  t    -> throwError $ IncompatibleTypes IntT t p
typecheckTo _ = return ()
