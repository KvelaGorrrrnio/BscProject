module RL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer

translate :: TypeTab -> RL.AST -> String
translate ttab ast = showTypeDecs ttab ++ "hej"

-- trlProg :: RL.AST -> SRL.AST
--
-- p = (0,0)
-- assert e = SRL.If e (Atom $ Skip p) (Atom $ Skip p) (Lit (IntV 1) p) p
-- false    = Lit (IntV 0) p
--
-- trlBlocks :: RL.AST -> SRL.AST
-- trlBlocks = foldr trlBlock (assert false)
--
-- trlBlock :: (RL.Label,RL.Block) -> SRL.Block
-- trlBlock (l,(f,s,j)) =
