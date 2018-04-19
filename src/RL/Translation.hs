module RL.Translation
( translateToSRLSource
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer

translateToSRLSource :: RL.AST -> String
translateToSRLSource ast = "hej"
