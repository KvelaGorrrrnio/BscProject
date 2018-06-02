{-# LANGUAGE LambdaCase #-}
module SRL.Interp (module SRL.Interp, module Common.Log, module SRL.AST) where

import SRL.AST

import Common.Error
import Common.Interp
import Common.Log

import Control.Monad.Reader

-- ==================
-- Running the program
-- ==================

runProgram :: AST -> TypeTab -> (Either Error VarTab, Log)
runProgram ast ttab = let (vt,ms) = (execVarState vtab . interp) ast in (vt, Log vtab ms)
  where vtab = buildVTab ttab


-- ============
-- Interpreting
-- ============

interp :: Block -> VarState ()
interp (Step s) = logStep s

interp (If t b1 b2 a) = do
  q  <- eval t >>= \case
    IntV q -> return $ q/=0
    w      -> logError $ RuntimeError (getExpPos t) $ ConflictingType IntT (getType w)

  interp $ if q then b1 else b2

  r <- eval a >>= \case
    IntV r -> return $ r/=0
    w      -> logError $ RuntimeError (getExpPos t) $ ConflictingType IntT (getType w)

  when (q /= r)
    $ logError $ RuntimeError (getExpPos a) $ AssertionFailed a (IntV . boolToInt $ q) (IntV $ boolToInt r)

interp (Until d a b1 b2 t) = do -- log this
  q <- eval a >>= \case
    IntV q -> return $ q/=0
    w      -> logError $ RuntimeError (getExpPos t) $ ConflictingType IntT (getType w)

  unless (q == d) $ logError $ RuntimeError (getExpPos a) $ AssertionFailed a (IntV . boolToInt $ q) (IntV . boolToInt $ d)

  interp b1

  r <- eval t >>= \case
    IntV r -> return $ r/=0
    w      -> logError $ RuntimeError (getExpPos t) $ ConflictingType IntT (getType w)

  unless r $ interp b2 >> interp (Until False a b1 b2 t)

interp (Seq b1 b2) = interp b1 >> interp b2
