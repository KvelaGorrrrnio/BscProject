{-# LANGUAGE LambdaCase #-}
module RL.Interp (module RL.Interp, module RL.AST, module Common.Error) where

import RL.AST

import Common.Interp
import Common.Error

import Control.Monad.Reader


-- The program state
type ProgState = ReaderT AST VarState


-- ==================
-- Running the program
-- ==================

runProgram :: AST -> TypeTab -> (Either Error VarTab, Log)
runProgram ast ttab =
  let entry   = (fst . head) ast
      vtab    = buildVTab ttab
      (vt,ms) = execVarState vtab . runReaderT (interp [] entry) $ ast
    in (vt, Log vtab ms)


-- ============
-- Interpreting
-- ============

interp :: Label -> Label -> ProgState ()
interp from l = do

  Just (f,ss,j) <- asks (lookup l)

  case f of
    Entry p      -> unless (null from)
      $ llogError $ RuntimeError p $ FromFail (show $ Entry p) from
    From l' p    -> unless (from == l')
      $ llogError $ RuntimeError p $ FromFail from l'
    Fi a l1 l2 p -> do
      q <- lcheckCond a
      let l' = if q then l1 else l2
      unless (from == l')
        $ llogError $ RuntimeError p $ FromFail from l'

  logSteps ss

  case j of
    Exit _       -> return ()
    Goto l' _    -> interp l l'
    If t l1 l2 p -> do
      q <- lcheckCond t
      if q then interp l l1 else interp l l2

  where logSteps   = lift . mapM_ logStep
        llogError  = lift . logError
        lcheckCond = lift . checkCond
