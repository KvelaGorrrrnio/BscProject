module AST where

import Data.Bits (xor)
import Data.List (intercalate)
import qualified Data.HashMap.Strict as M

import Error

-- ===
-- Log
-- ===

type Log = [Message]

logToString :: Log -> String
logToString = intercalate "\n\n" . map show

logToJSON :: Log -> String
logToJSON = intercalate ",\n\n" . map show

data Message = Stmt       Stmt
             | State      VarTab
             | EndOfBlock To
             | NewBlock   Label
             | Error      Error
instance Show Message where
  show (Stmt s)           = "> " ++ show s
  show (State vtab)       = show vtab
  show (EndOfBlock t)     = show t
  show (NewBlock l)       = ">> " ++ l
  show (Error err)        = "*** Error: " ++ err


-- ======
-- VarTab
-- ======

newtype VarTab = VarTab (M.HashMap Id Value)
instance Show VarTab where
  show (VarTab vtab) = (
      (\vt -> if null vt
              then "null"
              else (intercalate "\n" . map (\(k,v) -> k ++ " -> " ++ show v)) vt
      )
    . M.toList
    . M.filter (not . isClear)
    ) vtab
insert id val (VarTab vtab) = VarTab $ M.insert id val vtab
adjust op id (VarTab vtab)  = VarTab $ M.adjust op id vtab
varlookup id (VarTab vtab)  = M.lookup id vtab


-- labels and ids
type Label = String
type Id    = String


-- ===
-- AST
-- ===

newtype AST = AST (M.HashMap Label Block)
instance Show AST where
  show (AST ast) = (intercalate "\n\n" . map (\(l,b) -> l ++ ": " ++ show b) . M.toList) ast

mapAST f (AST ast) = AST $ M.map f ast
blklookup l (AST ast)  = M.lookup l ast

newtype Block = Block (From, [Stmt], To)
instance Show Block where
  show (Block (f,s,t)) = show f ++ "\n  "
    ++ (intercalate "\n  " . map show) s ++ "\n"
    ++ show t

data Value = IntV Integer | ListV [Value]
instance Show Value where
  show (IntV n)   = show n
  show (ListV ls) = show ls
isClear (IntV n)   = n == 0
isClear (ListV ls) = null ls

data From = From Label
          | Fi Exp Label Label
          | Entry
instance Show From where
  show (From l)     = "from " ++ l
  show (Fi e l1 l2) = case e of
    Parens _ -> "fi " ++ show e ++ " " ++ l1 ++ " " ++ l2
    _        -> "fi (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show Entry        = "entry"

data To = Goto Label
        | If Exp Label Label
        | Exit
instance Show To where
  show (Goto l)     = "goto " ++ l
  show (If e l1 l2) = case e of
    Parens _ -> "if "  ++ show e ++ " "  ++ l1 ++ " " ++ l2
    _        -> "if (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show Exit         = "exit"

data Stmt = Update Id UpdOp Exp
          | Push Id Id
          | Pop  Id Id
          | Swap Id Id
          | Skip
        -- NU!     | Seq [Stmt]
instance Show Stmt where
  show (Update id op e) = id ++ show op ++ show e
  show (Push id1 id2)   = "push " ++ id1 ++ " " ++ id2
  show (Pop id1 id2)    = "pop "  ++ id1 ++ " " ++ id2
  show (Swap id1 id2)   = "swap " ++ id1 ++ " " ++ id2
  show Skip             = "skip"
  -- show (Seq s)          = (intercalate "\n  " . map show) s


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

data Exp = Lit Value
         | Var   Id

         | ABinary ABinOp Exp Exp

         | DivBinary DivOp Exp Exp

         | AUnary AUnOp Exp

         | Relational ROp Exp Exp

         | LBinary LBinOp Exp Exp

         | Not Exp

         | Top   Exp
         | Size  Exp
         | Empty Exp

         | Parens Exp

instance Show Exp where
  show (Lit v)             = show v
  show (Var id)            = id
  show (ABinary op l r)    = show l ++ show op ++ show r
  show (AUnary op e)       = case e of
    Parens _ -> show op ++ show e
    _        -> show op ++ "(" ++ show e ++ ")"
  show (Relational op l r) = show l ++ show op ++ show r
  show (LBinary op l r)    = show l ++ show op ++ show r
  show (Not e)             = case e of
    Parens _ -> "not " ++ show e
    _        -> "not (" ++ show e ++ ")"
  show (Top e)             = case e of
    Parens _ -> "top " ++ show e
    _        -> "top (" ++ show e ++ ")"
  show (Size e)            = case e of
    Parens _ -> "top " ++ show e
    _        -> "top (" ++ show e ++ ")"
  show (Empty e)           = case e of
    Parens _ -> "top " ++ show e
    _        -> "top (" ++ show e ++ ")"
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
data ROp = Eq | Less | Greater
instance Show ROp where
  show Eq      = " = "
  show Less    = " < "
  show Greater = " > "
mapROp op = case op of
  Eq      -> (==)
  Less    -> (<)
  Greater -> (>)
-- ^
data LBinOp = And | Or
instance Show LBinOp where
  show And = " && "
  show Or  = " || "
mapLBinOp op = case op of
  And -> (&&)
  Or  -> (||)

