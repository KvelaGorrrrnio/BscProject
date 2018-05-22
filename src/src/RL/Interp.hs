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
runProgram ast ttab =
  let entry   = getEntry ast
      vtab    = buildVTab ttab
      (vt,ms) = execVarState vtab . runReaderT (interp [] entry) $ ast
    in (vt, Log vtab ms)

-- ======
-- Blocks
-- ======

interp :: Label -> Label -> ProgState ()
interp from l = do
  blks <- ask
  Just (f,ss,j) <- asks (lookup l)

  case f of
    Entry p      -> unless (null from)
      $ lift $ logError $ RuntimeError p $ FromFail (show $ Entry p) from -- (CustomRT $ "From-clause not consistent.\nComing from entry\nExpecting label: " ++ from ))
    From l' p    -> unless (from == l')
      $ lift $ logError $ RuntimeError p $ FromFail from l' -- CustomRT $ "From-clause not consistent.\nComing from label: " ++ from ++ "\nExpecting label:   " ++ l' ))
    Fi a l1 l2 p -> do
      q <- lift $ eval a >>= \case
        IntV q -> return $ q/=0
        w      -> logError $ RuntimeError (getExpPos a) $ ConflictingType IntT (getType w)

      let l' = if q then l1 else l2

      unless (from == l') $
        lift $logError $ RuntimeError p $ FromFail from l'

  lift $ execStmts ss

  let msg = show j

  case j of
    Exit _         -> return ()
    Goto l' _      -> interp l l'
    If c l1 l2 p -> do
      q <- lift $ eval c >>= \case
        IntV q -> return $ q/=0
        w      -> logError $ RuntimeError (getExpPos c) $ ConflictingType IntT (getType w)

      if q then interp l l1 else interp l l2

execStmts :: [Stmt] -> VarState ()
execStmts = mapM_ logStmt

