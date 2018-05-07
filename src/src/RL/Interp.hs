{-# LANGUAGE LambdaCase #-}
module RL.Interp (module RL.Interp, module Common.Log, module RL.AST) where

import RL.Error
import RL.AST

import Common.Interp
import Common.Log

import Control.Monad.Reader
import Control.Monad.Except

-- The program state
type ProgState = ReaderT AST VarState

-- ==================
-- Running the program
-- ==================

runProgram :: AST -> TypeTab -> (Either Error VarTab, Log)
runProgram ast ttab = do
  let entry = getEntry ast
      vtab  = buildVTab ttab
  execVarState vtab . runReaderT (interp [] entry) $ ast

-- ======
-- Blocks
-- ======

interp :: Label -> Label -> ProgState ()
interp from l = do

  Just (f,ss,t) <- asks (lookup l)

  case f of
    Entry _      -> return ()
    From l' p    -> unless (from == l') $
      lift (logError $ RuntimeError p (CustomRT $ "From-clause not consistent.\nComing from label: " ++ from ++ "\nExpecting label:   " ++ l' ))
    Fi a l1 l2 p -> do
      q <- lift $ eval a >>= \case
        IntV q -> return $ intToBool q
        _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

      let l' = if q then l1 else l2

      unless (from == l') $
        lift (logError $ RuntimeError p (CustomRT $ "From-clause not consistent.\nComing from label: " ++ from ++ "\nExpecting label:   " ++ l'))

  logMsg $ ">> " ++ l

  lift $ execStmts ss

  let msg = show t

  case t of
    Exit _         -> logMsg msg
    Goto l' _      -> logMsg msg >> interp l l'
    IfTo c l1 l2 p -> do
      q <- lift $ eval c >>= \case
        IntV q -> return $ intToBool q
        _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig
      logMsg $ msg ++ " -> " ++ if q then "true" else "false" -- we want lower case

      if q then interp l l1 else interp l l2
