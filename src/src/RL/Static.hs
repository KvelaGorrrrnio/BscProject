module RL.Static
( staticcheck
, module Common.Static
) where

import RL.AST
import RL.Error
import Common.Static
import Data.List (group,sort)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

type BlockState = StateT ([(Label, Pos)],Bool,Bool) (ReaderT AST (Except Error))

staticcheck :: Either Error AST -> Either Error AST
staticcheck (Left err)  = Left err
staticcheck (Right ast) = case runExcept . (flip runReaderT ast) . (flip runStateT ([],False,False)) $ staticcheckBlocks ast of
  Left err -> Left err
  Right _  -> Right ast

staticcheckBlocks :: AST -> BlockState ()
staticcheckBlocks = mapM_ staticcheckBlock

staticcheckBlock :: (Label, Block) -> BlockState ()
staticcheckBlock (l,(f,stmts,t)) = get >>= \(lbls,en,ex) -> do
  case lookup l lbls of
    Just p  -> throwError $ StaticError (getFromPos f) $ DuplicateLabel l p
    Nothing -> case f of
      Entry p | en -> throwError $ StaticError p $ DuplicateEntry
      _ -> return ()-- not done

getFromPos :: From -> Pos
getFromPos (From _ p)   = p
getFromPos (Fi _ _ _ p) = p
getFromPos (Entry p)    = p

