{-# LANGUAGE LambdaCase #-}
module Common.Type
( runTypecheck
, runTypecheckWith
, TypeTab
, TypeState
, typecheckStmts
, typecheckStmt
, typeof
, update
, unify
, typesToVarTab
, showTab
) where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Common.Error
import Common.AST
-- For showtab
import Data.List

type TypeTab   = M.HashMap Id Type
type TypeState = StateT TypeTab (Except TypeError)

runTypecheck :: a -> (a -> TypeState ()) -> Either TypeError TypeTab
runTypecheck ast init = runTypecheckWith ast init M.empty

runTypecheckWith :: a -> (a -> TypeState ()) -> TypeTab -> Either TypeError TypeTab
runTypecheckWith ast init tab = runExcept . flip execStateT tab $ init ast

-- ==========
-- Statements
-- ==========
-- Iterate over statements
typecheckStmts :: [Stmt] -> TypeState ()
typecheckStmts = mapM_ typecheckStmt

-- Update
typecheckStmt :: Stmt -> TypeState ()
typecheckStmt (Update id op exp p)         = do
  typeofId id >>= \case
    IntT     -> return ()
    UnknownT -> update id IntT p
    t        -> case unify IntT t of
      Just _  -> update id IntT p
      Nothing -> throwError $ IncompatibleTypes IntT t (-1,1)
  typeof exp >>= \case
    IntT     -> return ()
    UnknownT -> return ()
    t        -> throwError $ IncompatibleTypes IntT t (-1,2)
-- Push
typecheckStmt (Push id lid p)              = typeofId id >>= \t ->
  typeofId lid >>= \case
    UnknownT -> update lid (ListT t) p
    ListT lt -> case unify lt t of
      Nothing -> throwError $ IncompatibleTypes t lt p -- TODO: Custom Push error
      Just t' -> update id t' p >> update lid (ListT t') p
    lt       -> throwError $ IncompatibleTypes t lt p -- TODO: Custom Stack error
-- Pop
typecheckStmt (Pop id lid p)               = typeofId id >>= \t ->
  typeofId lid >>= \case
    UnknownT -> update lid (ListT t) p
    ListT lt -> case unify lt t of
      Nothing -> throwError $ IncompatibleTypes t lt (-1,1) -- TODO: Custom Push error
      Just t' -> update id t' p >> update lid (ListT t') p
    lt       -> throwError $ IncompatibleTypes t lt (-1,2) -- TODO: Custom Stack error
-- Pop
typecheckStmt (Swap id1 id2 p)             = do
  t1 <- typeofId id1
  t2 <- typeofId id2
  case unify t1 t2 of
    Nothing -> throwError $ IncompatibleTypes t1 t2 p
    _       -> return ()
-- If
typecheckStmt (If ifexp tstmts fstmts fiexp p) = typeof ifexp >>= \case
  IntT -> typecheckStmts tstmts >> typecheckStmts fstmts >> typeof fiexp >>= \case
    IntT -> return ()
    fit  -> throwError $ IncompatibleTypes IntT fit p
  ift  -> throwError $ IncompatibleTypes IntT ift p
-- Until
typecheckStmt (Until fexp stmts uexp p)      = typeof fexp >>= \case
  IntT -> typecheckStmts stmts >> typeof uexp >>= \case
    IntT -> return ()
    ut  -> throwError $ IncompatibleTypes IntT ut p
  ft  -> throwError $ IncompatibleTypes IntT ft p
-- Skip
typecheckStmt (Skip _)                         = return ()

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
update :: Id -> Type -> Pos -> TypeState ()
update id t p = typeofId id >>= \case
  UnknownT -> modify $ M.insert id t
  t' -> case unify t t' of
    Nothing -> throwError $ IncompatibleTypes t t' p
    Just ct -> modify $ M.insert id ct

-- Get type of exp
typeof :: Exp -> TypeState Type
typeof (Lit v _)         = typeofVal v
typeof (Var id p)        = update id UnknownT p >> typeofId id
typeof (Binary op l r p) = typeofBinOp op >>= \(lit,rit,t) -> do
  lt <- case l of
    Var id p' -> update id lit p' >> typeof l
    _         -> typeof l
  rt <- case r of
    Var id p' -> update id rit p' >> typeof r
    _         -> typeof r
  case (unify lit lt, unify rit rt) of
    (Nothing,_) -> throwError $ BinOpTypes op (lit,rit) (lt,rt) p
    (_,Nothing) -> throwError $ BinOpTypes op (lit,rit) (lt,rt) p
    _           -> return t
--  unary arithmetic and logical
typeof (Unary op exp p) | op < Size  = typeofUnOp op >>= \(it,t) -> do
  et <- case exp of
    Var id p' -> update id it p' >> typeof exp
    _         -> typeof exp
  case unify it et of
    Nothing -> throwError $ UnOpType op it et p
    _       -> return t
-- unary stack operations
                      | otherwise = typeofUnOp op >>= \case
  (ListT it,t) -> do
    case exp of
      Var id p' -> update id it p'
      _         -> return ()
    et <- typeof exp >>= \case
      ListT et -> return et
      UnknownT -> return UnknownT -- $ ListT UnknownT
      et       -> throwError $ UnOpType op (ListT it) et p
    case op of
      Top -> case unify t et of
        Nothing -> throwError $ IncompatibleTypes t et p
        Just t' -> return t'
      _   -> return t

typeof (Parens exp _)               = typeof exp

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
typeofVal (ListV v)  = ListT <$> typeofVal (head v)

-- Convert type table to variable table
typesToVarTab :: TypeTab -> VarTab
typesToVarTab ttab = map defaultVal (M.toList ttab)
  where defaultVal (n,ListT _) = (n,ListV [])
        defaultVal (n,_)       = (n,IntV 0)

-- Show hashmap
showTab :: Show a => M.HashMap Id a -> String
showTab = showVTab . M.toList
