module RL.Translation
( translateToSRLSource
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer

translateToSRLSource :: TypeTab -> RL.AST -> String
translateToSRLSource ttab ast = showTypeDecs ttab ++ "hej"
