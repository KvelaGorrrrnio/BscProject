module RL.Static
( staticcheck
) where

import RL.AST
import RL.Error

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import qualified Data.HashMap.Strict as M

type BlockReader = ReaderT AST (Except Error)

staticcheck :: Either Error (TypeTab,AST) -> Either Error (TypeTab,AST)
staticcheck (Left err)  = Left err
staticcheck (Right (ttab,ast)) = case runExcept (runReaderT staticcheck' ast) of
   Left err -> Left err
   Right _  -> Right (ttab,ast)

staticcheck' :: BlockReader ()
staticcheck' = do
  startWithEntry
  endWithExit
  dupEntry
  dupExit
  defLab
  dupLab

startWithEntry :: BlockReader ()
startWithEntry = do
  b <- asks (snd . head)
  case b of
    (Entry _,_,_) -> return ()
    (f',_,_)      -> throwError $ StaticError (getFromPos f') EntryNotStart

endWithExit :: BlockReader ()
endWithExit = do
  b <- asks (snd . last)
  case b of
    (_,_,Exit _) -> return ()
    (_,_,j')     -> throwError $ StaticError (getJumpPos j') ExitNotEnd

dupEntry :: BlockReader ()
dupEntry = do
  es <- asks $ filter (\(_,(f,_,_)) -> case f of Entry{} -> True; _ -> False)
  when (length es > 1) $ throwError (StaticError (0,0) DuplicateEntry)

dupExit :: BlockReader ()
dupExit = do
  es <- asks $ filter (\(_,(_,_,j)) -> case j of Exit{} -> True; _ -> False)
  when (length es > 1) $ throwError (StaticError (0,0) DuplicateExit)

dupLab :: BlockReader ()
dupLab = do
  mp <- asks $ M.fromList . map (\(l,b) -> (l,0))
  ast <- ask
  let occ = M.toList . execState (dupLab' ast) $ mp
  mapM_ (checkOcc ast) occ

  where

    dupLab' :: AST -> State (M.HashMap Label Int) ()
    dupLab' = mapM_ (incOcc . fst)

    incOcc :: Label -> State (M.HashMap Label Int) ()
    incOcc l = modify $ M.adjust (+1) l

    checkOcc :: AST -> (Label, Int) -> BlockReader ()
    checkOcc ast (l,i) = when (i > 1) $ do
      let Just b = lookup l ast
      throwError $ StaticError (getBlockPos b) (DuplicateLabel l)


defLab :: BlockReader ()
defLab = do
  ast <- ask
  mapM_ (\(_,b) -> defBlock b) ast

  where

    defBlock :: Block -> BlockReader ()
    defBlock (f,_,j) = do
      case f of
        From l p     -> def l p
        Fi _ l1 l2 p -> def l1 p >> def l2 p
        _            -> return ()
      case j of
        Goto l p     -> def l p
        If _ l1 l2 p -> def l1 p >> def l2 p
        _            -> return ()

    def :: Label -> Pos -> BlockReader ()
    def l p = do
      b <- asks (lookup l)
      when (null b) $ throwError (StaticError p $ NotDefinedLabel l)

getFromPos :: From -> Pos
getFromPos (From _ p)   = p
getFromPos (Fi _ _ _ p) = p
getFromPos (Entry p)    = p

getJumpPos :: Jump -> Pos
getJumpPos (Goto _ p)   = p
getJumpPos (If _ _ _ p) = p
getJumpPos (Exit p)     = p

getBlockPos :: Block -> Pos
getBlockPos (f,_,_) = getFromPos f

-- module RL.Static
-- ( staticcheck
-- ) where
--
-- import RL.AST
-- import RL.Error
-- import Data.List (group,sort)
-- import Control.Monad
-- import Control.Monad.State
-- import Control.Monad.Reader
-- import Control.Monad.Except
--
-- type BlockState = StateT ([(Label,Pos)],Bool,Bool) (ReaderT [(Label,Pos)] (Except Error))
--
-- staticcheck :: Either Error (TypeTab,AST) -> Either Error (TypeTab,AST)
-- staticcheck (Left err)  = Left err
-- staticcheck (Right (ttab,ast)) = case runExcept . (flip runReaderT . getLabels) ast . flip execStateT ([],False,False) $ staticcheckBlocks ast of
--   Left err -> Left err
--   Right (_,True,True) -> Right (ttab,ast)
--   Right (_,False,_)   -> Left $ StaticError (0,0) NoEntry
--   Right (_,_,False)   -> Left $ StaticError (0,0) NoExit
--   where getLabels = map (\(l,b) ->(l,getBlockPos b))
--
-- staticcheckBlocks :: AST -> BlockState ()
-- staticcheckBlocks = mapM_ staticcheckBlock
--
-- staticcheckBlock :: (Label, Block) -> BlockState ()
-- staticcheckBlock (l,(f,stmts,j)) = do
--   lbls <- ask
--   (sn,en,ex) <- get
--   case lookup l sn of
--     Just p  -> throwError $ StaticError (getFromPos f) $ DuplicateLabel l p
--     Nothing -> put ((l,getFromPos f):sn,en,ex)
--   case f of
--     Entry p | en -> throwError $ StaticError p DuplicateEntry
--     Entry _      -> put (sn,True,ex)
--     From l p   | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
--     Fi _ l _ p | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
--     Fi _ _ l p | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
--     _                                 -> return ()
--   (sn,en,ex) <- get
--   case j of
--     Exit p | ex -> throwError $ StaticError p DuplicateExit
--     Exit _      -> put (sn,en,True)
--     Goto l p     | null (lookup l lbls) -> throwError $ StaticError p $ NotDefinedLabel l
--     If _ l _ p | null (lookup l lbls)   -> throwError $ StaticError p $ NotDefinedLabel l
--     If _ _ l p | null (lookup l lbls)   -> throwError $ StaticError p $ NotDefinedLabel l
--     _                                   -> return ()
--
-- getFromPos :: From -> Pos
-- getFromPos (From _ p)   = p
-- getFromPos (Fi _ _ _ p) = p
-- getFromPos (Entry p)    = p
--
-- getBlockPos :: Block -> Pos
-- getBlockPos (f,_,_) = getFromPos f
