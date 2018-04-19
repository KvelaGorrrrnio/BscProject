{-# LANGUAGE LambdaCase #-}
module RL.Type
( typecheck
) where

import RL.AST
import RL.Error
import Common.Error (CError(..),TypeError(..))
import qualified Common.Type as T
import Control.Monad.Except

typecheck :: AST -> Either RL.Error.Error T.TypeTab
typecheck ast = case T.typecheck ast typecheckBlocks of
  Right tab            -> Right tab
  Left (TypeError p err) -> Left $ TypeError p $ err
  _                    -> Left $ StaticError $ StaticVoid

typecheckBlocks :: AST -> T.TypeState ()
typecheckBlocks [] = return ()
typecheckBlocks ((_,(f,stmts,t)):ast) = typecheckFrom f >> T.typecheckStmts stmts >> typecheckTo t

typecheckFrom :: From -> T.TypeState ()
typecheckFrom (Fi exp _ _ p) = T.typeof exp >>= \case
  IntT -> return ()
  t    -> throwError $ TypeError p $ IncompatibleTypes IntT t -- TODO: Exp not eval to Int
typecheckFrom _ = return ()

typecheckTo :: To -> T.TypeState ()
typecheckTo (IfTo exp _ _ p) = T.typeof exp >>= \case
  IntT -> return ()
  t    -> throwError $ TypeError p $ IncompatibleTypes IntT t
typecheckTo _ = return ()
