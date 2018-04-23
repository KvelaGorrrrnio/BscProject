module Common.AST where

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
type Id = String
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
  show (Update id op e _) = id ++ show op ++ show e
  show (Push id1 id2 _)   = "push " ++ id1 ++ " " ++ id2
  show (Pop id1 id2 _)    = "pop "  ++ id1 ++ " " ++ id2
  show (Swap id1 id2 _)   = "swap " ++ id1 ++ " " ++ id2
  show (Skip _)           = "skip"
  -- unique for SRL
  show (If t s1 s2 a _)   = "if " ++ showPar t ++ " then [s1] else [s2]"
  show (Until a s t _)    = "from " ++ showPar a ++ " do [s] until " ++ showPar t

data UpdOp = PlusEq | MinusEq | XorEq| MultEq | DivEq deriving Eq
instance Show UpdOp where
  show PlusEq  = " += "
  show MinusEq = " -= "
  show XorEq   = " ^= "
  show MultEq  = " *= "
  show DivEq   = " /= "
mapUpdOp :: UpdOp -> Exp -> Exp -> Pos -> Exp
mapUpdOp PlusEq  = Binary   Plus
mapUpdOp MinusEq = Binary   Minus
mapUpdOp XorEq   = Binary   Xor
mapUpdOp MultEq  = Binary   Mult
mapUpdOp DivEq   = Binary   Div

data Exp
  = Lit    Value Pos
  | Var    Id Pos
  | Binary BinOp Exp Exp Pos
  | Unary  UnOp  Exp Pos
  | Parens Exp Pos
  deriving Eq
instance Show Exp where
  show (Lit v _)          = show v
  show (Var id _)         = id
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
