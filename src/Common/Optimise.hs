module Common.Optimise where

import Common.AST

import Data.HashMap.Strict (HashMap, lookup, insert, adjust)

-- import Control.Monad.State
--
-- type ConstTab = HashMap Id Int
--
-- type ConstState = State (HashMap Id Int)


optStmts :: [Stmt] -> [Stmt]
optStmts  = concatMap optStmt

---------------------------------------------
optUpd (s1:s2:ss) = case (s1,s2) of
  (Update id op1 e1 p, Update id2 op2 e2 _) | (op1==PlusEq || op1==MinusEq)
                                           && (op2==PlusEq || op2==MinusEq)
                                           -- && not (containsVar e1 || containsVar e2)
                                           && id==id2 ->
    optUpd $ Update id op1 (mapUpdOp op2 (Parens e1 p) (Parens e2 p) p) p : ss
    -- if not (containsVar e2)
    -- then optUpd $ Update id op1 (mapUpdOp op2 (Parens e1 p) (Parens e2 p) p) p : ss
    -- else s2 : optUpd (s1:ss)
  _ -> s1 : optUpd (s2:ss)
optUpd (s:ss) = s : optUpd ss
optUpd [] = []

containsVar :: Exp -> Bool
containsVar (Var _ _)        = True
containsVar (Lit _ _)        = False
containsVar (Binary _ l r _) = containsVar l || containsVar r
containsVar (Unary _ e _)    = containsVar e
containsVar (Parens e _)     = containsVar e
---------------------------------------------

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
optExp = compExp . optExp'

optExp' :: Exp -> Exp
optExp' e = case e of
  Unary Not e' p -> case rmPar e' of
    Unary Not e'' _      -> optExp' e''
    Binary Neq l r p'    -> Binary Equal (optExp' l) (optExp' r) p'
    _ -> let e'' = optCond . optExp' $ e' in
      case rmPar e'' of
        Binary Equal l r p'   -> Binary Neq (optExp' l) (optExp' r) p'
        Binary Geq l r p'     -> Binary Less (optExp' l) (optExp' r) p'
        Binary Leq l r p'     -> Binary Greater (optExp' l) (optExp' r) p'
        Binary Greater l r p' -> Binary Leq (optExp' l) (optExp' r) p'
        Binary Less l r p'    -> Binary Geq (optExp' l) (optExp' r) p'
        Lit (IntV 0) p'       -> Lit (IntV 1) p'
        Lit (IntV _) p'       -> Lit (IntV 0) p'
        _ -> Unary Not e'' p
  Unary Sign e' p -> case rmPar e' of
    Unary Sign e'' _ -> optExp' e'
    _                -> Unary Sign (optExp' e') p
  Unary Neg e' p -> case rmPar e' of
    Unary Neg e'' p' -> optExp' e''
    _ -> let e'' = optExp' e' in
      case rmPar e'' of
        Lit (IntV 0) _ -> Lit (IntV 0) p'
        _              -> Unary Neg e'' p'
  Binary op l r p ->
    let l' = optExp' l
        r' = optExp' r in
    case op of
      Mult -> case (rmPar l', rmPar r') of
        (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
        (Lit (IntV 1) _, _)  -> r'
        (_, Lit (IntV 0) p') -> Lit (IntV 0) p'
        (_, Lit (IntV 1) _)  -> l'
        _                    -> Binary op l' r' p
      Div -> case (rmPar l', rmPar r') of
        (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
        (_, Lit (IntV 1) _)  -> l'
        _                    -> Binary op l' r' p
      Plus -> case (rmPar l', rmPar r') of
        (_, Unary Neg e' p') -> Binary Minus l' e' p'
        (Unary Neg e' p', _) -> Binary Minus r' e' p'
        (_, Lit (IntV 0) p') -> l'
        (Lit (IntV 0) p', _) -> r'
        _                    -> Binary op l' r' p
      Minus -> case (rmPar l', rmPar r') of
        (_, Unary Neg e' p') -> Binary Plus l' e' p'
        (Unary Neg e' p', _) -> Unary Neg (Binary Plus e' r' p') p'
        (_, Lit (IntV 0) p') -> l'
        (Lit (IntV 0) p', _) -> Unary Neg r' p'
        _                    -> Binary op l' r' p
      Pow -> case (rmPar l', rmPar r') of
        (Lit (IntV 0) p', _) -> Lit (IntV 0) p'
        (_, Lit (IntV 1) _)  -> l'
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
    Parens{} -> optExp' e'
    Var id _ -> Var id p
    Lit v p  -> Lit v p
    _        -> Parens (optExp' e') p
  _ -> e

compExp :: Exp -> Exp
compExp e = case e of
  Binary op l r p ->
    let l' = compExp l
        r' = compExp r in
    case (rmPar l', rmPar r') of
      (Lit v1 _, Lit v2 _)
        | op < Div  -> Lit (applyABinOp (mapABinOp op) v1 v2) p
        | op <= Mod -> case v2 of
               IntV 0 -> Binary op l' r' p
               _      -> Lit (applyABinOp (mapABinOp op) v1 v2) p
        | op <= Geq -> Lit (applyRBinOp (mapRBinOp op) v1 v2) p
      _ -> Binary op l' r' p
  Unary Sign e' p ->
    let e'' = compExp e' in
    case rmPar e'' of
      Lit v _ -> Lit (applyAUnOp (mapAUnOp Sign) v) p
      _       -> Unary Sign e'' p
  Parens e' p -> case e' of
    Parens{} -> compExp e'
    Var id _ -> Var id p
    Lit v p  -> Lit v p
    _        -> Parens (compExp e') p
  _ -> e

rmPar :: Exp -> Exp
rmPar (Parens e p) = rmPar e
rmPar e            = e
