module SRL.Inversion where

import SRL.AST

import Common.Inversion

invert :: AST -> AST
invert = invertStmts
