module SrlAST (module SrlAST, module Common.AST) where

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
