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

runProgramWith :: AST -> VarTab -> (Either Error VarTab, Log)
runProgramWith ast vtab = do
  let entry = getEntry ast
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
      a' <- lift $ valToBool <$> eval a
      let l' = if a' then l1 else l2
      unless (from == l') $
        lift (logError $ RuntimeError p (CustomRT $ "From-clause not consistent.\nComing from label: " ++ from ++ "\nExpecting label:   " ++ l'))

  logMsg $ ">> " ++ l
  lift $ execStmts ss

  let msg = show t
  case t of
    Exit _         -> logMsg msg
    Goto l' _      -> logMsg msg >> interp l l'
    IfTo c l1 l2 _ -> do
      c' <- lift $ valToBool <$> eval c
      logMsg $ msg ++ " -> " ++ if c' then "true" else "false" -- we want lower case
      if c' then interp l l1 else interp l l2
