module RL.Interp (module RL.Interp, module Common.Log, module RL.AST) where

import RL.Error
import RL.AST

import Common.Interp
import Common.Log

import Control.Monad.Reader

-- The program state
type ProgState = ReaderT AST VarState

-- ==================
-- Running the program
-- ==================

runProgramWith :: AST -> VarTab -> (Either RuntimeError VarTab, Log)
runProgramWith ast vtab = do
  let entry = getEntry ast
  execVarState vtab . runReaderT (interp entry) $ ast

-- ======
-- Blocks
-- ======

interp :: Label -> ProgState ()
interp l = do
  ast <- ask
  case lookup l ast of
    Just (_,ss,t) -> do
      lift $ execStmts ss
      case t of
        Exit _     -> return ()
        Goto l _     -> interp l
        IfTo t l1 l2 _ -> do
          t' <- lift $ valToBool <$> eval t
          if t' then interp l1 else interp l2
    Nothing -> lift $ logError $ CustomRT $ ("Label '" ++ l ++ "' not defined.")
