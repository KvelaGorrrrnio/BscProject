{-# LANGUAGE LambdaCase #-}
module RL.AST (module RL.AST, module Common.AST) where

import Data.Bits (xor)
import Data.List (intercalate)

import Common.AST
import RL.Error

-- Extra for VarTab
buildVTab :: AST -> VarTab
buildVTab ast = [("n", IntV 0), ("v", IntV 0), ("w",IntV 0), ("q",ListV []), ("p",ListV [])]

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

type AST = [(Label, Block)]
showAST :: AST -> String
showAST  = intercalate "\n\n" . map (\(l,b) -> l ++ ": " ++ showB b)
getEntry ast = do
  let entries = (map fst . filter
          (\case
              (_, (Entry,_,_)) -> True
              _                -> False
          )) ast
  if length entries == 1 then head entries else error "Exactly one entry must be defined"

type Block = (From, [Stmt], To)
showB (f,s,t) = show f ++ "\n  "
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
        | IfTo Exp Label Label
        | Exit
instance Show To where
  show (Goto l)     = "goto " ++ l
  show (IfTo e l1 l2) = case e of
    Parens _ -> "if "  ++ show e ++ " "  ++ l1 ++ " " ++ l2
    _        -> "if (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show Exit         = "exit"
