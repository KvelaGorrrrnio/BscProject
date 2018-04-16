{-# LANGUAGE LambdaCase #-}
module RlAST where

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
             | EndOfBlock To
             | NewBlock   Label
             | Error      Error
instance Show Message where
  show (Stmt s)           = "> " ++ show s
  show (State vtab)       = show vtab
  show (EndOfBlock t)     = show t
  show (NewBlock l)       = ">> " ++ l
  show (Error err)        = "*** Error: " ++ err



-- ===
-- AST
-- ===

-- labels
type Label = String

newtype AST = AST [(Label, Block)]
instance Show AST where
  show (AST ast) = (intercalate "\n\n" . map (\(l,b) -> l ++ ": " ++ show b)) ast

mapAST f (AST ast) = AST $ map f ast
revAST (AST ast)   = AST $ reverse ast
blklookup l (AST ast)  = lookup l ast
getEntry (AST ast) = do
  let entries = (map fst . filter
          (\case
              (_, Block (Entry,_,_)) -> True
              _                      -> False
          )) ast
  if length entries == 1 then head entries else error "Exactly one entry must be defined"

newtype Block = Block (From, [Stmt], To)
instance Show Block where
  show (Block (f,s,t)) = show f ++ "\n  "
    ++ (intercalate "\n  " . map show) s ++ "\n"
    ++ show t

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

