module SRL.Inversion
( invert
) where

import SRL.AST

import Common.Inversion

invert :: AST -> AST
invert = reverse . map invertStmt
