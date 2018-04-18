{-# LANGUAGE LambdaCase #-}
module RlAST where

import Data.Bits (xor)
import Data.List (intercalate)

import Common.AST

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
        | GIf Exp Label Label
        | Exit
instance Show To where
  show (Goto l)     = "goto " ++ l
  show (GIf e l1 l2) = case e of
    Parens _ -> "if "  ++ show e ++ " "  ++ l1 ++ " " ++ l2
    _        -> "if (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show Exit         = "exit"
