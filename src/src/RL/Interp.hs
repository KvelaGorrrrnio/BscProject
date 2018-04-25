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

  lift $ execStmts ss

  case t of
    Exit _         -> return ()
    Goto l' _      -> interp l l'
    IfTo t l1 l2 _ -> do
      t' <- lift $ valToBool <$> eval t
      if t' then interp l l1 else interp l l2
