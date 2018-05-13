module SRL.Inversion where

import SRL.AST

import Common.Inversion

invert :: AST -> AST
invert (Atom s)          = Atom $ invertStmt s
invert (If t b1 b2 a p)  = If a (invert b1) (invert b2) t p
invert (Until d a b t p) = Until d t (invert b) a p
invert (Seq b1 b2)       = Seq (invert b2) (invert b1)
