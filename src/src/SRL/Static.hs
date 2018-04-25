module SRL.Static
( staticcheck
, module Common.Static
) where

import SRL.AST
import SRL.Error
import Common.Static

staticcheck :: Either Error AST -> Either Error AST
staticcheck (Left err)  = Left err
staticcheck (Right ast) = case runStaticcheck ast of
  Left err -> Left err
  Right _  -> Right ast
