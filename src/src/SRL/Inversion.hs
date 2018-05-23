module SRL.Inversion where

import SRL.AST

import Common.Inversion

invert :: AST -> AST
invert (Step s)            = Step $ invertStmt s
invert (If t b1 b2 a)      = If a (invert b1) (invert b2) t
invert (Until d a b1 b2 t) = Until d t (invert b1) (invert b2) a
invert (Seq b1 b2)         = Seq (invert b2) (invert b1)
