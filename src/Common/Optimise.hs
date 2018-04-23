module Common.Optimise where

import Common.AST

import Data.HashMap.Strict (HashMap, lookup, insert, adjust)

-- import Control.Monad.State
--
-- type ConstTab = HashMap Id Int
--
-- type ConstState = State (HashMap Id Int)


optStmts :: [Stmt] -> [Stmt]
-- optStmts (s1:s2:ss) = case (s1,s2) of
--   (Update id op1 e1 p, Update id2 op2 e2 _) | id==id2 ->
--     optStmts $ Update id op1 (mapUpdOp op2 (Parens e1 p) (Parens e2 p) p) p : ss -- TODO: please reduce expression
--   _ -> optStmt s1 ++ optStmts (s2:ss)
-- optStmts (s:ss) = optStmt s ++ optStmts ss
-- optStmts [] = []
optStmts  = concatMap optStmt

optStmt :: Stmt -> [Stmt]
optStmt (If t s1 s2 a p) = case rmPar . optCond . optExp $ t of
  Lit (IntV 0) _ -> optStmts s2
  Lit (IntV _) _ -> optStmts s1
  t'             -> [If t' (optStmts s1) (optStmts s2) (rmPar . optCond . optExp $ a) p]
optStmt (Until a s t p)  = case rmPar . optCond . optExp $ t of
  Lit (IntV n) _ | n/=0 -> optStmts s
  t'             -> [Until (rmPar . optCond . optExp $ a) (optStmts s) t' p]
optStmt (Swap id1 id2 p) | id1 == id2 = []
  | otherwise = [Swap id1 id2 p]
optStmt (Update id op e p) = case (op, rmPar . optExp $ e) of
  (PlusEq, Lit (IntV 0) _)  -> []
  (MinusEq, Lit (IntV 0) _) -> []
  (XorEq, Lit (IntV 0) _)   -> []
  (DivEq, Lit (IntV 1) _)   -> []
  (MultEq, Lit (IntV 1) _)  -> []
  (PlusEq, Unary Neg e' _)  -> [Update id MinusEq (rmPar e') p]
  (MinusEq, Unary Neg e' _) -> [Update id PlusEq (rmPar e') p]
  (_,e')                    -> [Update id op e' p]
optStmt Skip{}               = []
optStmt s                    = [s]

-- isConst :: Id -> AST -> Bool
-- isConst id = not . any (\case Update id' _ _ _ -> id'==id
--                               If _ s1 s2 _ _   -> isConst id s1 && isConst id s2
--                               Until _ s _ _    -> isConst id s
--                               Swap id1 id2 _   -> id==id1 || id==id2
--                               _ -> False
--                        )

optCond :: Exp -> Exp
optCond e = case e of
  Unary Not e' p  -> Unary Not e' p
  Binary And l r p -> case (rmPar l, rmPar r) of
    (Lit (IntV n) _, r') -> r'
    (l', Lit (IntV _) _) -> l'
    _                    -> Binary And l r p
  Binary Or l r p -> case (rmPar l, rmPar r) of
    (Lit (IntV n) _, r') -> r'
    (l', Lit (IntV _) _) -> l'
    _                    -> Binary Or l r p
  Binary op l r p -> Binary op (optCond l) (optCond r) p
  Parens e' p -> case e' of
    Parens{} -> optCond e'
    _        -> Parens (optCond e') p
  _ -> e


optExp :: Exp -> Exp
optExp e = case e of
  Unary Not e' p -> case rmPar e' of
    Unary Not e'' _      -> optExp e''
    Binary Neq l r p'    -> Binary Equal (optExp l) (optExp r) p'
    _ -> case rmPar (optCond . optExp $ e') of
      Binary Equal l r p'   -> Binary Neq (optExp l) (optExp r) p'
      Binary Geq l r p'     -> Binary Less (optExp l) (optExp r) p'
      Binary Leq l r p'     -> Binary Greater (optExp l) (optExp r) p'
      Binary Greater l r p' -> Binary Leq (optExp l) (optExp r) p'
      Binary Less l r p'    -> Binary Geq (optExp l) (optExp r) p'
      Lit (IntV 0) p'       -> Lit (IntV 1) p'
      Lit (IntV _) p'       -> Lit (IntV 0) p'
      _ -> Unary Not (optCond . optExp $ e') p
  Unary Sign e' p -> case rmPar e' of
    Unary Sign e'' _ -> optExp e'
    _                -> Unary Sign (optExp e') p
  Unary Neg e' p -> case rmPar e' of
    Unary Neg e'' _ -> rmPar $ optExp e''
    Lit (IntV 0) p' -> Lit (IntV 0) p'
    _               -> Unary Neg (optExp e') p
  Binary op l r p ->
    let l' = optExp l
        r' = optExp r in
    case op of
      Mult -> case (rmPar l', rmPar r') of
        (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
        (Lit (IntV 1) _, _)  -> optExp r
        (_, Lit (IntV 0) p') -> Lit (IntV 0) p'
        (_, Lit (IntV 1) _)  -> optExp l
        _                    -> Binary op l' r' p
      Div -> case (rmPar l', rmPar r') of
        (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
        (_, Lit (IntV 1) _)  -> optExp l
        _                    -> Binary op l' r' p
      Plus -> case (rmPar l', rmPar r') of
        (_, Unary Neg e' p') -> Binary Minus l' e' p'
        (_, Lit (IntV 0) p') -> optExp l
        (Lit (IntV 0) p', _) -> optExp r
        _                    -> Binary op l' r' p
      Minus -> case (rmPar l', rmPar r') of
        (_, Unary Neg e' p') -> Binary Plus l' e' p'
        (_, Lit (IntV 0) p') -> optExp l
        (Lit (IntV 0) p', _) -> optExp (Unary Neg r p')
        _                    -> Binary op l' r' p
      Pow -> case (rmPar l', rmPar r') of
        (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
        (_, Lit (IntV 1) _)  -> optExp l
        (_, Lit (IntV 0) p') -> Lit (IntV 1) p'
        _                    -> Binary op l' r' p
      Neq -> case (rmPar l', rmPar r') of
        (e', Lit (IntV 0) _) -> e'
        (Lit (IntV 0) _, e') -> e'
        _                    -> Binary Neq l' r' p
      Equal -> case (rmPar l', rmPar r') of
        (Unary Size e p, Lit (IntV 0) _) -> Unary Empty e p
        (Lit (IntV 0) _, Unary Size e p) -> Unary Empty e p
        _                                -> Binary Equal l' r' p
      And ->
        let l'' = optCond l'
            r'' = optCond r' in
        case (rmPar l'', rmPar r'') of
          (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
          (_, Lit (IntV 0) p') -> Lit (IntV 0) p'
          (Lit (IntV n) p', Lit (IntV m) _) | n/=0 && m/=0 -> Lit (IntV 1) p'
          _                    -> Binary op l'' r'' p
      Or ->
        let l'' = optCond l'
            r'' = optCond r' in
        case (rmPar l', rmPar r') of
          (Lit (IntV 1) p', _) -> Lit (IntV 1) p'
          (_, Lit (IntV 1) p') -> Lit (IntV 1) p'
          (Lit (IntV 0) p', Lit (IntV 0) _) -> Lit (IntV 0) p'
          _                    -> Binary op l'' r'' p
      _ -> Binary op l' r' p
  Parens e' p -> case e' of
    Parens{} -> optExp e'
    Var id _ -> Var id p
    Lit v p  -> Lit v p
    _        -> Parens (optExp e') p
  _ -> e

rmPar :: Exp -> Exp
rmPar (Parens e p) = rmPar e
rmPar e            = e
