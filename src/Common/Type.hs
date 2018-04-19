{-# LANGUAGE LambdaCase #-}
module Common.Type
( typecheck
, TypeTab
, TypeState
, typecheckStmts
, typecheckStmt
, typeof
, update
, unify
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Common.Error
import Common.AST

type TypeTab   = M.HashMap Id Type
type TypeState = StateT TypeTab (Except (CError ()))

typecheck :: a -> (a -> TypeState ()) -> Either (CError ()) TypeTab
typecheck ast init = runExcept . (flip execStateT (M.fromList [])) $ init ast

-- ==========
-- Statements
-- ==========
-- Iterate over statements
typecheckStmts :: [Stmt] -> TypeState ()
typecheckStmts [] = return ()
typecheckStmts (s:stmts) = typecheckStmt s >> typecheckStmts stmts

-- Update
typecheckStmt :: Stmt -> TypeState ()
typecheckStmt (Update id op exp)         = do
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
typecheckStmt (Push id lid)              = typeofId id >>= \t -> do
  typeofId lid >>= \case
    UnknownT -> update lid (ListT t)
    ListT lt -> case unify lt t of
      Nothing -> throwError $ TypeError $ IncompatibleTypes t lt -- TODO: Custom Push error
      Just t' -> update id t' >> update lid (ListT t')
    lt       -> throwError $ TypeError $ IncompatibleTypes t lt -- TODO: Custom Stack error
-- Pop
typecheckStmt (Pop id lid)               = typeofId id >>= \t -> do
  typeofId id >>= \case
    UnknownT -> update lid (ListT t)
    ListT lt -> case unify lt t of
      Nothing -> throwError $ TypeError $ IncompatibleTypes t lt -- TODO: Custom Push error
      Just t' -> update id t' >> update lid (ListT t')
    lt       -> throwError $ TypeError $ IncompatibleTypes t lt -- TODO: Custom Stack error
-- Pop
typecheckStmt (Swap id1 id2)             = do
  t1 <- typeofId id1
  t2 <- typeofId id2
  case unify t1 t2 of
    Nothing -> throwError $ TypeError $ IncompatibleTypes t1 t2
    _       -> return ()
-- If
typecheckStmt (If ifexp tstmts fstmts fiexp) = typeof ifexp >>= \case
  IntT -> typecheckStmts tstmts >> typecheckStmts fstmts >> typeof fiexp >>= \case
    IntT -> return ()
    fit  -> throwError $ TypeError $ IncompatibleTypes IntT fit
  ift  -> throwError $ TypeError $ IncompatibleTypes IntT ift
-- Until
typecheckStmt (Until fexp stmts uexp)        = typeof fexp >>= \case
  IntT -> typecheckStmts stmts >> typeof uexp >>= \case
    IntT -> return ()
    ut  -> throwError $ TypeError $ IncompatibleTypes IntT ut
  ft  -> throwError $ TypeError $ IncompatibleTypes IntT ft
-- Skip
typecheckStmt Skip                       = return ()

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
--  unary arithmetic and logical
typeof (Unary op exp) | op < Size  = typeofUnOp op >>= \(it,t) -> do
  typeof exp >>= \et -> case unify it et of
    Nothing -> throwError $ TypeError $ UnOpType op it et
    _       -> return t
-- unary stack operations
                      | otherwise = typeofUnOp op >>= \case
  (ListT it,t) -> typeof exp >>= \case
    ListT et -> case op of
      Top -> case unify t et of
        Nothing -> throwError $ TypeError $ IncompatibleTypes t et
        Just t' -> return t'
      _   -> return t
    et       -> throwError $ TypeError $ UnOpType op (ListT it) et

typeof (Parens exp)               = typeof exp

-- Operator types
typeofBinOp :: BinOp -> TypeState (Type,Type,Type)
typeofBinOp _ = return (IntT, IntT, IntT)

typeofUnOp :: UnOp -> TypeState (Type,Type)
typeofUnOp Size  = return (ListT UnknownT, IntT)
typeofUnOp Empty = return (ListT UnknownT, IntT)
typeofUnOp Top   = return (ListT UnknownT, UnknownT)
typeofUnOp _     = return (IntT,IntT)

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
