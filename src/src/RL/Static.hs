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

type BlockState = StateT ([(Label,Pos)],Bool,Bool) (ReaderT [(Label,Pos)] (Except Error))

staticcheck :: Either Error AST -> Either Error AST
staticcheck (Left err)  = Left err
staticcheck (Right ast) = case runExcept . (flip runReaderT $ getLabels ast) . (flip execStateT ([],False,False)) $ staticcheckBlocks ast of
  Left err -> Left err
  Right (_,True,True) -> Right ast
  Right (_,False,_)   -> Left $ StaticError (0,0) $ NoEntry
  Right (_,_,False)   -> Left $ StaticError (0,0) $ NoExit
  where getLabels = map (\(l,b) ->(l,getBlockPos b))

staticcheckBlocks :: AST -> BlockState ()
staticcheckBlocks = mapM_ staticcheckBlock

staticcheckBlock :: (Label, Block) -> BlockState ()
staticcheckBlock (l,(f,stmts,t)) = do
  lbls <- ask
  (sn,en,ex) <- get
  case lookup l sn of
    Just p  -> throwError $ StaticError (getFromPos f) $ DuplicateLabel l p
    Nothing -> put ((l,getFromPos f):sn,en,ex)
  case f of
    Entry p | en -> throwError $ StaticError p DuplicateEntry
    Entry _      -> put (sn,True,ex)
    From l p   | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
    Fi _ l _ p | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
    Fi _ _ l p | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
    _                                 -> return ()
  case runStaticcheck stmts of
    Left err -> throwError err
    Right _  -> return ()
  (sn,en,ex) <- get
  case t of
    Exit p | ex -> throwError $ StaticError p DuplicateExit
    Exit _      -> put (sn,en,True)
    Goto l p     | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
    IfTo _ l _ p | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
    IfTo _ _ l p | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
    _                                   -> return ()

getFromPos :: From -> Pos
getFromPos (From _ p)   = p
getFromPos (Fi _ _ _ p) = p
getFromPos (Entry p)    = p

getBlockPos :: Block -> Pos
getBlockPos (f,_,_) = getFromPos f
