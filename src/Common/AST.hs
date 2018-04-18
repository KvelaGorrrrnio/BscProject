module Common.AST where

import Data.List
import Data.Bits

-- values
data Value = IntV Integer | ListV [Value]
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

type VarTab = [(Id,Value)]
showVTab =
    (\vt -> if null vt
            then "null"
            else (intercalate "\n" . map (\(k,v) -> k ++ " -> " ++ show v)) vt
    )
  . filter (not . isClear . snd)
insert id val = map (\(id',v) -> if id'==id then (id,val) else (id',v))
adjust op id  = map (\(id',v) -> if id'==id then (id,op v) else (id',v))

-- Statements
data Stmt = Update Id UpdOp Exp
          | Push Id Id
          | Pop  Id Id
          | Swap Id Id
          | Skip
          -- unique for SRL
          | If Exp [Stmt] [Stmt] Exp
          | Until Exp [Stmt] Exp
instance Show Stmt where
  show (Update id op e) = id ++ show op ++ show e
  show (Push id1 id2)   = "push " ++ id1 ++ " " ++ id2
  show (Pop id1 id2)    = "pop "  ++ id1 ++ " " ++ id2
  show (Swap id1 id2)   = "swap " ++ id1 ++ " " ++ id2
  show Skip             = "skip"

data UpdOp = PlusEq | MinusEq | XorEq| MultEq | DivEq
instance Show UpdOp where
  show PlusEq  = " += "
  show MinusEq = " -= "
  show XorEq   = " ^= "
  show MultEq  = " *= "
  show DivEq   = " /= "
mapUpdOp :: UpdOp -> Exp -> Exp -> Exp
mapUpdOp PlusEq  = Binary   Plus
mapUpdOp MinusEq = Binary   Minus
mapUpdOp XorEq   = Binary   Xor
mapUpdOp MultEq  = Binary   Mult
mapUpdOp DivEq   = Binary   Div

data Exp
  = Lit    Value
  | Var    Id
  | Binary BinOp Exp Exp
  | Unary  UnOp  Exp
  | Parens Exp
instance Show Exp where
  show (Lit v)         = show v
  show (Var id)        = id
  show (Binary op l r) = ""
  show (Unary  op exp) = show op++show exp
  show (Parens exp)    = "("++show exp++")"

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
  deriving (Show,Eq,Ord)


data UnOp
  = Neg
  | Sign
  -- ^ Arithmetic
  -- v Logical
  | Not
  -- ^ Logical
  -- v Stack
  | Top
  | Empty
  | Size
  deriving (Show,Eq,Ord)

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
