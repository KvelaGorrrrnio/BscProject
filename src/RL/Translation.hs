{-# LANGUAGE FlexibleContexts #-}

module RL.Translation
( translateToSRLSource
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer

translateToSRLSource :: RL.AST -> String
translateToSRLSource ast = SRL.showAST $ flip evalState 1 . execWriterT $ translateS "init" Entry Exit ast
