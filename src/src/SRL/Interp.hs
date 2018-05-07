module SRL.Interp (module SRL.Interp, module Common.Log, module SRL.AST) where

import SRL.Error
import SRL.AST

import Common.Interp
import Common.Log

import Control.Monad.Reader

-- ==================
-- Running the program
-- ==================

runProgram :: AST -> TypeTab -> (Either Error VarTab, Log)
runProgram ast ttab = execVarState vtab . execStmts $ ast
  where vtab = buildVTab ttab

