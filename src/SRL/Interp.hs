module SRL.Interp (module SRL.Interp, module Common.Log, module SRL.AST) where

import SRL.Error
import SRL.AST

import Common.Interp
import Common.Log

import Control.Monad.Reader

-- ==================
-- Running the program
-- ==================

runProgramWith :: AST -> VarTab -> (Either RuntimeError VarTab, Log)
runProgramWith ast vtab = execVarState vtab . execStmts $ ast

