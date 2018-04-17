module Extra where

import Data.List
import Data.Bits

data Value = IntV Integer | ListV [Value]
instance Show Value where
  show (IntV n)   = show n
  show (ListV ls) = show ls
isClear (IntV n)   = n == 0
isClear (ListV ls) = null ls

-- ======
-- VarTab
-- ======

-- ids
type Id = String

newtype VarTab = VarTab [(Id,Value)]
instance Show VarTab where
  show (VarTab vtab) = (
      (\vt -> if null vt
              then "null"
              else (intercalate "\n" . map (\(k,v) -> k ++ " -> " ++ show v)) vt
      )
    . filter (not . isClear . snd)
    ) vtab
insert id val (VarTab vtab)    = VarTab $ map (\(id',v) -> if id'==id then (id,val) else (id',v)) vtab
adjust op id (VarTab vtab) = VarTab $ map (\(id',v) -> if id'==id then (id,op v) else (id',v)) vtab
varlookup id (VarTab vtab)     = lookup id vtab

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
  show (If t s1 s2 a)   = "if " ++ show t ++ " then " ++ show a ++ " fi " ++ show a
  show (Until a s t)    = "from " ++ show a ++ " do " ++ (intercalate "\n" . map show) s ++ " until " ++ show t

data UpdOp = PlusEq | MinusEq | XorEq| MultEq | DivEq
instance Show UpdOp where
  show PlusEq  = " += "
  show MinusEq = " -= "
  show XorEq   = " ^= "
  show MultEq  = " *= "
  show DivEq   = " /= "
mapUpdOp :: UpdOp -> Exp -> Exp -> Exp
mapUpdOp PlusEq  = ABinary   Plus
mapUpdOp MinusEq = ABinary   Minus
mapUpdOp XorEq   = ABinary   Xor
mapUpdOp MultEq  = ABinary   Mult
mapUpdOp DivEq   = DivBinary Div

-- Exp

data Exp = Lit Value
         | Var   Id

         | ABinary ABinOp Exp Exp

         | DivBinary DivOp Exp Exp

         | AUnary AUnOp Exp

         | Relational ROp Exp Exp

         | LBinary LBinOp Exp Exp

         | Not Exp

         | LstExp LstOp Exp

         | Parens Exp

instance Show Exp where
  show (Lit v)             = show v
  show (Var id)            = id
  show (ABinary op l r)    = show l ++ show op ++ show r
  show (AUnary op e)       = case e of
    Parens _ -> show op ++ show e
    _        -> show op ++ "(" ++ show e ++ ")"
  show (Relational op l r) = show l ++ show op ++ show r
  show (LBinary op l r)    = show op
  show (Not e)             = case e of
    Parens _ -> "not " ++ show e
    _        -> "not (" ++ show e ++ ")"
  show (LstExp op e)       = case e of
    Parens _ -> show op ++ show e
    _        -> show op ++ "(" ++ show e ++ ")"
  show (Parens e) = case e of
    Parens _ -> show e
    _        -> "(" ++ show e ++ ")"
-- ^
data ABinOp = Plus
            | Minus
            | Xor
            | Pow
            | Mult
instance Show ABinOp where
  show Plus  = " + "
  show Minus = " - "
  show Xor   = " ^ "
  show Pow   = " ** "
  show Mult  = " * "
mapABinOp op = case op of
  Plus  -> (+)
  Minus -> (-)
  Xor   -> xor
  Pow   -> (^)
  Mult  -> (*)
-- ^
data DivOp = Div | Mod
instance Show DivOp where
  show Div = " / "
  show Mod = " % "
mapDivOp op = case op of
  Div -> div
  Mod -> mod
-- ^
data AUnOp = Neg | Sign
instance Show AUnOp where
  show Neg  = "-"
  show Sign = "sign "
mapAUnOp op = case op of
  Neg  -> negate
  Sign -> signum
-- ^
data ROp = Eq | NEq | Less | LEq | Greater | GEq
instance Show ROp where
  show Eq      = " = "
  show NEq     = " != "
  show Less    = " < "
  show LEq     = " <= "
  show Greater = " > "
  show GEq     = " >= "
mapROp op = case op of
  Eq      -> (==)
  NEq     -> (/=)
  Less    -> (<)
  LEq     -> (<=)
  Greater -> (>)
  GEq     -> (>=)
-- ^
data LBinOp = And | Or
instance Show LBinOp where
  show And = " && "
  show Or  = " || "
mapLBinOp op = case op of
  And -> 0
  Or  -> 1
-- ^
data LstOp = Top | Empty | Size
instance Show LstOp where
  show Top   = "top "
  show Empty = "empty "
  show Size  = "size "
