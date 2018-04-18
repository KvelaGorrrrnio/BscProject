{-# LANGUAGE LambdaCase #-}
module Common.Type
( update
, unify
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Common.Error
import Common.AST

type TypeTab   = M.HashMap Id Type
type TypeState = StateT TypeTab (Except (Error ()))

-- ==========
-- Statements
-- ==========

-- Update
typeCheckStmt :: Stmt -> TypeState ()
typeCheckStmt (Update id op exp)         = do
  typeofId id >>= \case
    IntT     -> return ()
    UnknownT -> update id IntT
    t        -> case unify IntT t of
      Just _  -> update id IntT
      Nothing -> throwError $ TypeError $ IncompatibleTypes IntT t
  typeof exp >>= \case
    IntT -> return ()
    t    -> throwError $ TypeError $ IncompatibleTypes IntT t

-- Push
typeCheckStmt (Push id lid)              = return ()
-- Push
typeCheckStmt (Pop id lid)               = return ()
-- Pop
typeCheckStmt (Swap id1 id2)             = return ()
-- If
typeCheckStmt (If iexp tstmt fstmt fexp) = return ()
-- Until
typeCheckStmt (Until fexp stmt uexp)     = return ()
-- Skip
typeCheckStmt _                          = return ()

-- =======
-- Helpers
-- =======

-- Unify two types, may fail (with Nothing).
unify :: Type -> Type -> Maybe Type
unify UnknownT t            = Just t
unify t UnknownT            = Just t
unify (ListT t1) (ListT t2) = case unify t1 t2 of
  Nothing -> Nothing
  Just t  -> Just $ ListT t
unify t1 t2 | t1 == t2      = Just t1
            | otherwise     = Nothing

-- Assign type to id (if compatible with earlier assigned type)
update :: Id -> Type -> TypeState ()
update id t = get >>= \tab -> typeofId id >>= \case
  UnknownT -> put $ M.insert id t tab
  t' -> case unify t t' of
    Nothing -> throwError $ TypeError $ IncompatibleTypes t t'
    Just ct -> put $ M.insert id ct tab

-- Get type of exp
typeof :: Exp -> TypeState Type
typeof (Lit v)         = typeofVal v
typeof (Var id)        = typeofId id
typeof (Binary op l r) = typeofBinOp op >>= \(lit,rit,t) -> do
  lt <- typeof l
  rt <- typeof r
  case (unify lit lt, unify rit rt) of
    (Nothing,_) -> throwError $ TypeError $ BinOpTypes op (lit,rit) (lt,rt)
    (_,Nothing) -> throwError $ TypeError $ BinOpTypes op (lit,rit) (lt,rt)
    _           -> return t
typeof _     = return IntT

-- Operator types
typeofBinOp :: BinOp -> TypeState (Type,Type,Type)
typeofBinOp _ = return (IntT, IntT, IntT)

typeofUnOp :: UnOp -> TypeState (Type,Type)
typeofUnOp _ = return (IntT,IntT)

-- Get type of id
typeofId :: Id -> TypeState Type
typeofId id = get >>= \tab -> case M.lookup id tab of
  Just t  -> return t
  Nothing -> return UnknownT

-- Get type of value
typeofVal :: Value -> TypeState Type
typeofVal (IntV _)   = return IntT
typeofVal (ListV []) = return $ ListT UnknownT
typeofVal (ListV v)  = typeofVal (head v) >>= \t -> return $ ListT t
