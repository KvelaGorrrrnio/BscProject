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

data Message = MsgStmt       Stmt
             | MsgState      VarTab
             | MsgEndOfBlock To
             | MsgNewBlock   Label
             | MsgError      Error
instance Show Message where
  show (MsgStmt s)           = "> " ++ show s
  show (MsgState vtab)       = show vtab
  show (MsgEndOfBlock t)     = show t
  show (MsgNewBlock l)       = ">> " ++ l
  show (MsgError err)        = "*** Error: " ++show err


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
              (_, (Entry _,_,_)) -> True
              _                -> False
          )) ast
  if length entries == 1 then head entries else error "Exactly one entry must be defined"

type Block = (From, [Stmt], To)
showB (f,s,t) = show f ++ "\n  "
  ++ (intercalate "\n  " . map show) s ++ "\n"
  ++ show t

data From = From Label Pos
          | Fi Exp Label Label Pos
          | Entry Pos
instance Show From where
  show (From l _)     = "from " ++ l
  show (Fi e l1 l2 _) = case e of
    Parens _ _ -> "fi " ++ show e ++ " " ++ l1 ++ " " ++ l2
    _        -> "fi (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show (Entry _)      = "entry"

data To = Goto Label Pos
        | IfTo Exp Label Label Pos
        | Exit Pos
instance Show To where
  show (Goto l _)      = "goto " ++ l
  show (IfTo e l1 l2 _) = case e of
    Parens _ _ -> "if "  ++ show e ++ " "  ++ l1 ++ " " ++ l2
    _        -> "if (" ++ show e ++ ") " ++ l1 ++ " " ++ l2
  show (Exit _)         = "exit"
