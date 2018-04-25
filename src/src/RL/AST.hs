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
  ++ (if null s then show (Skip (0,0)) else (intercalate "\n  " . map show) s) ++ "\n"
  ++ show t

data From = From Label Pos
          | Fi Exp Label Label Pos
          | Entry Pos
          deriving Eq
instance Show From where
  show (From l _)     = "from " ++ l
  show (Fi e l1 l2 _) = "fi " ++ showPar e ++ " " ++ l1 ++ " " ++ l2
  show (Entry _)      = "entry"

data To = Goto Label Pos
        | IfTo Exp Label Label Pos
        | Exit Pos
        deriving Eq
instance Show To where
  show (Goto l _)       = "goto " ++ l
  show (IfTo e l1 l2 _) = "if "  ++ showPar e ++ " "  ++ l1 ++ " " ++ l2
  show (Exit _)         = "exit"