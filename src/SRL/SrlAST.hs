module SrlAST where

import Data.Bits (xor)
import Data.List (intercalate)

import Extra

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
             | Error      Error
instance Show Message where
  show (Stmt s)           = "> " ++ show s
  show (State vtab)       = show vtab
  show (Error err)        = "*** Error: " ++ err


-- ===
-- AST
-- ===

-- labels
type Label = String

newtype AST = AST [Stmt] deriving Show
-- instance Show AST where
--   show (AST ast) = (intercalate "\n\n" . map (\(l,b) -> l ++ ": " ++ show b)) ast

mapAST f (AST ast) = AST $ map f ast
revAST (AST ast)   = AST $ reverse ast

type Block = [Stmt]

data Stmt = Update Id UpdOp Exp
          | Push Id Id
          | Pop  Id Id
          | Swap Id Id
          | Skip
          -- unique for SRL
          | If Exp Block Block Exp
          | Until Exp Block Exp
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

