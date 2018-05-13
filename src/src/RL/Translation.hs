module RL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer

translate :: TypeTab -> RL.AST -> String
translate ttab ast = showTypeDecs ttab ++ "hej"

--trlProg :: RL.AST -> SRL.AST

