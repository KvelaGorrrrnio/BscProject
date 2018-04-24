{-# LANGUAGE LambdaCase #-}
module RL.Type
( typecheck
, module Common.Type
) where

import RL.AST
import RL.Error
import Common.Type
import Control.Monad.Except

typecheck :: AST -> Either Error TypeTab
typecheck ast = case runTypecheck ast typecheckBlocks of
  Left err  -> Left err
  Right tab -> case runTypecheckWith ast typecheckBlocks tab of
    Left err  -> Left err
    Right tab -> Right tab


typecheckBlocks :: AST -> TypeState ()
typecheckBlocks = mapM_ typecheckBlock

typecheckBlock (_,(f,stmts,t)) = typecheckFrom f >> typecheckStmts stmts >> typecheckTo t

typecheckFrom :: From -> TypeState ()
typecheckFrom (Fi exp _ _ p) = typeof exp >>= \et -> case unify IntT et of
  Just _ -> return ()
  Nothing   -> throwError $ TypeError p $ NonIntegerExp exp et
typecheckFrom _ = return ()

typecheckTo :: To -> TypeState ()
typecheckTo (IfTo exp _ _ p) = typeof exp >>= \et -> case unify IntT et of
  Just _ -> return ()
  Nothing   -> throwError $ TypeError p $ NonIntegerExp exp et
typecheckTo _ = return ()
