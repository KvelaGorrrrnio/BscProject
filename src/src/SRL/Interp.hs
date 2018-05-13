{-# LANGUAGE LambdaCase #-}
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
runProgram ast ttab = execVarState vtab . interp $ ast
  where vtab = buildVTab ttab


interp :: Block -> VarState ()
interp (Atom s)       = logStmt s
interp (If t b1 b2 a p) = do
  q  <- eval t >>= \case
    IntV q -> return $ intToBool q
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  logMsg $ show (If t b1 b2 a p) ++ " -> " ++ if q then "true" else "false"
  interp $ if q then b1 else b2
  logMsg $ (if q then "[b1]" else "[b2]") ++ " done"

  r <- eval a >>= \case
    IntV r -> return $ intToBool r
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  when (q /= r)
    $ logError $ RuntimeError p $ CustomRT "Assert and such"

interp (Until d a b t p) = do -- log this
  logMsg $ show (Until d a b t p)

  q <- eval a >>= \case
    IntV q -> return $ intToBool q
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  unless (q == d) $ logError (RuntimeError p $ CustomRT "Assert")

  interp b

  r <- eval t >>= \case
    IntV r -> return $ intToBool r
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  unless r $ interp (Until False a b t p)

  logMsg $ show t ++ " -> " ++ "true"

interp (Seq b1 b2) = interp b1 >> interp b2
