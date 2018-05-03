module Common.AST where

import Data.Bits (xor)
import Data.List (intercalate)

-- values
data Value = IntV Integer | ListV [Value] deriving Eq
instance Show Value where
  show (IntV n)    = show n
  show (ListV ls)  = show ls
isClear (IntV n)   = n == 0
isClear (ListV ls) = null ls

-- ======
-- VarTab
-- ======

-- ids
data Id = Id String [Exp]
instance Show Id where
  show (Id id is) = id ++ concatMap (\e -> "[" ++ show e ++ "]") is
type Pos = (Int,Int)

type VarTab = [(Id,Value)]
showVTab tab = let m = maximum (map (\(n,_) -> length n) tab)
      in intercalate "\n" $ map (\(n,v) ->n++pad(m-length n+1)++" : "++show v) tab
   where pad n = replicate (n-1) ' '
insert id val = map (\(id',v) -> if id'==id then (id,val) else (id',v))
adjust op id  = map (\(id',v) -> if id'==id then (id,op v) else (id',v))

-- Statements
data Stmt = Update Id UpdOp Exp Pos
          | Push Id Id Pos
          | Pop  Id Id Pos
          | Swap Id Id Pos
          | Skip Pos
          -- unique for SRL
          | If Exp [Stmt] [Stmt] Exp Pos
          | Until Exp [Stmt] Exp Pos
          deriving Eq
instance Show Stmt where
  show (Update id op e _) = show id ++ show op ++ show e
  show (Push id1 id2 _)   = "push " ++ show id1 ++ " " ++ show id2
  show (Pop id1 id2 _)    = "pop "  ++ show id1 ++ " " ++ show id2
  show (Swap id1 id2 _)   = "swap " ++ show id1 ++ " " ++ show id2
  show (Skip _)           = "skip"
  -- unique for SRL
  show (If t s1 s2 a _)   = "if " ++ showPar t ++ " then [s1] else [s2]"
  show (Until a s t _)    = "from " ++ showPar a ++ " do [s] until " ++ showPar t
getStmtPos :: Stmt -> Pos
getStmtPos (Update _ _ _ p) = p
getStmtPos (Push _ _ p)     = p
getStmtPos (Pop _ _ p)      = p
getStmtPos (Swap _ _ p)     = p
getStmtPos (Skip p)         = p
getStmtPos (If _ _ _ _ p)   = p
getStmtPos (Until _ _ _ p)  = p

data UpdOp = PlusEq | MinusEq | XorEq| MultEq | DivEq deriving Eq
instance Show UpdOp where
  show PlusEq  = " += "
  show MinusEq = " -= "
  show XorEq   = " ^= "
  show MultEq  = " *= "
  show DivEq   = " /= "

data Exp
  = Lit    Value Pos
  | Var    Id Pos
  | Binary BinOp Exp Exp Pos
  | Unary  UnOp  Exp Pos
  | Parens Exp Pos
  deriving Eq
instance Show Exp where
  show (Lit v _)          = show v
  show (Var id _)         = show id
  show (Binary op l r _)  = show l ++ show op ++ show r
  show (Unary  op exp _)  = show op ++ show exp
  show (Parens exp _)     = case exp of
    Parens exp' _ -> show exp
    _           -> "("++show exp++")"
showPar :: Exp -> String
showPar e = case e of
  Parens _ _ -> show e
  _          -> "(" ++ show e ++ ")"

data BinOp
  = Plus
  | Minus
  | Xor
  | Pow
  | Mult
  -- v Non-zero right arithmetic
  | Div
  | Mod
  -- ^ Arithmetic
  -- v Relational
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  -- ^ Relational
  -- v Logical
  | Or
  | And
  deriving (Eq,Ord)
instance Show BinOp where
  show Plus    = " + "
  show Minus   = " - "
  show Xor     = " ^ "
  show Pow     = " ** "
  show Mult    = " * "
  show Div     = " / "
  show Mod     = " % "
  show Equal   = " = "
  show Neq     = " != "
  show Less    = " < "
  show Leq     = " <= "
  show Greater = " > "
  show Geq     = " >= "
  show Or      = " || "
  show And     = " && "



data UnOp
  = Neg
  | Sign
  -- ^ Arithmetic
  -- v Logical
  | Not
  -- ^ Logical
  -- v Stack
  | Size
  | Empty
  | Top
  deriving (Eq,Ord)
instance Show UnOp where
  show Neg   = "- "
  show Sign  = "~ "
  show Not   = "not "
  show Size  = "# "
  show Empty = "? "
  show Top   = "^ "

-- ====
-- Type
-- ====
data Type
  = IntT
  | ListT Type
  | UnknownT
  deriving Eq
instance Show Type where
  show IntT      = "int"
  show (ListT t) = "["++show t++"]"
  show UnknownT  = "?"


-- =======
-- helpers
-- =======
mapUpdOp :: UpdOp -> Exp -> Exp -> Pos -> Exp
mapUpdOp PlusEq  = Binary Plus
mapUpdOp MinusEq = Binary Minus
mapUpdOp XorEq   = Binary Xor
mapUpdOp MultEq  = Binary Mult
mapUpdOp DivEq   = Binary Div

mapABinOp Plus    = (+)
mapABinOp Minus   = (-)
mapABinOp Xor     = xor
mapABinOp Pow     = (^)
mapABinOp Mult    = (*)
mapABinOp Div     = div
mapABinOp Mod     = mod

mapRBinOp Equal   = (==)
mapRBinOp Neq     = (/=)
mapRBinOp Less    = (<)
mapRBinOp Leq     = (<=)
mapRBinOp Greater = (>)
mapRBinOp Geq     = (>=)

mapAUnOp Neg  = negate
mapAUnOp Sign = signum

mapLUnOp Not  = not

-- apply arithmetic binary operator
applyABinOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
applyABinOp op (IntV n) (IntV m) = IntV $ op n m
-- apply relational operator
applyRBinOp :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
applyRBinOp op (IntV n) (IntV m) = boolToVal $ op n m
-- apply arithmetic unary operator
applyAUnOp :: (Integer -> Integer) -> Value -> Value
applyAUnOp op (IntV n) = IntV $ op n

-- normalise to bool
norm :: Value -> Value
norm (IntV 0) = IntV 0
norm (IntV _) = IntV 1

-- converting bool to val
boolToVal :: Bool -> Value
boolToVal b = IntV $ if b then 1 else 0
-- converting val to bool
valToBool :: Value -> Bool
valToBool (IntV p) = p /= 0
