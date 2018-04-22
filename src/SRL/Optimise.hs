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
    _              -> [If (rmPar t') (optStmts s1) (optStmts s2) (rmPar . optExp $ a) p]
optStmt (Until a s t p)  = case rmPar (optExp t) of
  Lit (IntV n) _ | n/=0 -> optStmts s
  t'             -> [Until (rmPar . optExp $ a) (optStmts s) t' p]
optStmt (Swap id1 id2 p) | id1 == id2 = []
  | otherwise = [Swap id1 id2 p]
optStmt (Update id op e p) = case (op, rmPar . optExp $ e) of
  (PlusEq, Unary Neg e' _)  -> [Update id MinusEq (rmPar e') p]
  (MinusEq, Unary Neg e' _) -> [Update id PlusEq (rmPar e') p]
  (PlusEq, Lit (IntV 0) _)  -> []
  (MinusEq, Lit (IntV 0) _) -> []
  (XorEq, Lit (IntV 0) _)   -> []
  (DivEq, Lit (IntV 1) _)   -> []
  (MultEq, Lit (IntV 1) _)  -> []
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
    _ -> case rmPar (optExp e') of
      Binary Equal l r p'   -> Binary Neq (optExp l) (optExp r) p'
      Binary Neq l r p'     -> Binary Equal (optExp l) (optExp r) p'
      Binary Geq l r p'     -> Binary Less (optExp l) (optExp r) p'
      Binary Leq l r p'     -> Binary Greater (optExp l) (optExp r) p'
      Binary Greater l r p' -> Binary Leq (optExp l) (optExp r) p'
      Binary Less l r p'    -> Binary Geq (optExp l) (optExp r) p'
      Lit (IntV 0) p'       -> Lit (IntV 1) p'
      Lit (IntV _) p'       -> Lit (IntV 0) p'
      _ -> Unary Not (optExp e') p
  Unary Sign e' p -> case rmPar e' of
    Unary Sign e'' _ -> optExp e'
    _                -> Unary Sign (optExp e') p
  Unary Neg e' p -> case rmPar e' of
    Unary Neg e'' _ -> rmPar $ optExp e''
    Lit (IntV 0) p' -> Lit (IntV 0) p'
    _               -> Unary Neg (optExp e') p
  Binary op l r p -> case op of
    Mult -> case (rmPar (optExp l), rmPar (optExp r)) of
      (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
      (Lit (IntV 1) _, _)  -> optExp r
      (_, Lit (IntV 0) p') -> Lit (IntV 0) p'
      (_, Lit (IntV 1) _)  -> optExp l
      _                    -> Binary op (optExp l) (optExp r) p
    Div -> case (rmPar (optExp l), rmPar (optExp r)) of
      (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
      (_, Lit (IntV 1) _)  -> optExp l
      _                    -> Binary op (optExp l) (optExp r) p
    Plus -> case (rmPar (optExp l), rmPar (optExp r)) of
      (_, Unary Neg e' p') -> Binary Minus (optExp l) e' p'
      (_, Lit (IntV 0) p') -> optExp l
      (Lit (IntV 0) p', _) -> optExp r
      _                    -> Binary op (optExp l) (optExp r) p
    Minus -> case (rmPar (optExp l), rmPar (optExp r)) of
      (_, Unary Neg e' p') -> Binary Plus (optExp l) e' p'
      (_, Lit (IntV 0) p') -> optExp l
      (Lit (IntV 0) p', _) -> optExp (Unary Neg r p')
      _                    -> Binary op (optExp l) (optExp r) p
    Pow -> case (rmPar (optExp l), rmPar (optExp r)) of
      (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
      (_, Lit (IntV 1) _)  -> optExp l
      (_, Lit (IntV 0) p') -> Lit (IntV 1) p'
      _                    -> Binary op (optExp l) (optExp r) p
    Neq -> case (rmPar (optExp l), rmPar (optExp r)) of
      (e', Lit (IntV 0) _) -> e'
      (Lit (IntV 0) _, e') -> e'
      _                    -> Binary Neq (optExp l) (optExp r) p
    Equal -> case (rmPar (optExp l), rmPar (optExp r)) of
      (Unary Size e p, Lit (IntV 0) _) -> Unary Empty e p
      (Lit (IntV 0) _, Unary Size e p) -> Unary Empty e p
      _                                -> Binary Equal (optExp l) (optExp r) p
    And -> case (rmPar (optExp l), rmPar (optExp r)) of
      (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
      (_, Lit (IntV 0) p') -> Lit (IntV 0) p'
      (Lit (IntV n) p', Lit (IntV m) _) | n/=0 && m/=0 -> Lit (IntV 1) p'
      _                    -> Binary op (optExp l) (optExp r) p
    Or -> case (rmPar (optExp l), rmPar (optExp r)) of
      (Lit (IntV 1) p', _) -> Lit (IntV 1) p'
      (_, Lit (IntV 1) p') -> Lit (IntV 1) p'
      (Lit (IntV 0) p', Lit (IntV 0) _) -> Lit (IntV 0) p'
      _                    -> Binary op (optExp l) (optExp r) p
    _ -> Binary op (optExp l) (optExp r) p
  Parens e' p -> case e' of
    Parens{} -> optExp e'
    _        -> Parens (optExp e') p
  _ -> e

rmPar :: Exp -> Exp
rmPar (Parens e p) = rmPar e
rmPar e            = e
