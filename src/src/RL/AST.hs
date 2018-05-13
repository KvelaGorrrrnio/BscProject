{-# LANGUAGE LambdaCase #-}
module RL.AST (module RL.AST, module Common.AST) where

import Data.Bits (xor)
import Data.List (intercalate)

import Common.AST
import RL.Error

-- ===
-- AST
-- ===

-- labels
type Label = String

type AST = [(Label, Block)]
showAST :: TypeTab -> AST -> String
showAST ttab ast = showTypeDecs ttab ++ (intercalate "\n\n" . map (\(l,b) -> l ++ ": " ++ showB b)) ast
getEntry ast = do
  let entries = (map fst . filter
          (\case
              (_, (Entry _,_,_)) -> True
              _                -> False
          )) ast
  if length entries == 1 then head entries else error "Exactly one entry must be defined"

type Block = (From, [Stmt], Jump)
showB (f,s,j) = show f ++ "\n  "
  ++ (if null s then show (Skip (0,0)) else (intercalate "\n  " . map show) s) ++ "\n"
  ++ show j

data From = From Label Pos
          | Fi Exp Label Label Pos
          | Entry Pos
          deriving Eq
instance Show From where
  show (From l _)     = "from " ++ l
  show (Fi e l1 l2 _) = "fi " ++ showPar e ++ " " ++ l1 ++ " " ++ l2
  show (Entry _)      = "entry"

data Jump = Goto Label Pos
          | If Exp Label Label Pos
          | Exit Pos
        deriving Eq
instance Show Jump where
  show (Goto l _)       = "goto " ++ l
  show (If e l1 l2 _) = "if "  ++ showPar e ++ " "  ++ l1 ++ " " ++ l2
  show (Exit _)         = "exit"
