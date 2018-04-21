module SRL.Optimise where

import SRL.AST

import Data.HashMap.Strict (HashMap, lookup, insert, adjust)

import Control.Monad.State

type ConstTab = HashMap Id Int

type ConstState = State (HashMap Id Int)

optimise :: AST -> AST
optimise = optStmts

optStmts :: [Stmt] -> [Stmt]
optStmts = concatMap optStmt

optStmt :: Stmt -> [Stmt]
optStmt Skip{}           = []
optStmt (If t s1 s2 a p) = do
  let t' = optExp t
  case rmPar t' of
    Lit (IntV 0) _ -> optStmts s2
    Lit (IntV _) _ -> optStmts s1
    _              -> [If (rmPar t') (optStmts s1) (optStmts s2) (optExp a) p]
optStmt (Until a s t p)  = case rmPar (optExp t) of
  Lit (IntV n) _ | n/=0 -> optStmts s
  t'             -> [Until (rmPar . optExp $ a) (optStmts s) t' p]
optStmt (Swap id1 id2 p) | id1 == id2 = []
  | otherwise = [Swap id1 id2 p]
optStmt (Update id op e p) = case (op, rmPar . optExp $ e) of
  (PlusEq, Unary Neg e' _)  -> [Update id MinusEq (rmPar e') p]
  (MinusEq, Unary Neg e' _) -> [Update id PlusEq (rmPar e') p]
  (_,e')                    -> [Update id op e' p]
optStmt s                    = [s]

isConst :: Id -> AST -> Bool
isConst id = not . any (\case Update id' _ _ _ -> id'==id
                              If _ s1 s2 _ _   -> isConst id s1 && isConst id s2
                              Until _ s _ _    -> isConst id s
                              Swap id1 id2 _   -> id==id1 || id==id2
                              _ -> False
                       )

optExp :: Exp -> Exp
optExp e = case e of
  Unary Not e' p -> case rmPar e' of
    Unary Not e'' _      -> optExp e''
    Binary Equal l r p   -> Binary Neq (optExp l) (optExp r) p
    Binary Neq l r p     -> Binary Equal (optExp l) (optExp r) p
    Binary Geq l r p     -> Binary Less (optExp l) (optExp r) p
    Binary Leq l r p     -> Binary Greater (optExp l) (optExp r) p
    Binary Greater l r p -> Binary Leq (optExp l) (optExp r) p
    Binary Less l r p    -> Binary Geq (optExp l) (optExp r) p
    _ -> Unary Not (optExp e') p
  Unary Sign e' p -> case rmPar e' of
    Unary Sign e'' _ -> optExp e'
    _                -> Unary Sign (optExp e') p
  Unary Neg e' p -> case rmPar e' of
    Unary Neg e'' _ -> rmPar $ optExp e''
    _               -> Unary Neg (optExp e') p
  Binary Neq l r p -> case (rmPar (optExp l), rmPar (optExp r)) of
    (e', Lit (IntV 0) _) -> e'
    (Lit (IntV 0) _, e') -> e'
    (l',r') -> Binary Neq l' r' p
  Binary op l r p -> Binary op (optExp l) (optExp r) p
  Parens e' p -> case e' of
    Parens{} -> optExp e'
    _        -> Parens (optExp e') p
  _ -> e

rmPar :: Exp -> Exp
rmPar (Parens e p) = rmPar e
rmPar e            = e
