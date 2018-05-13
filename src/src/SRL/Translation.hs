{-# LANGUAGE FlexibleContexts #-}

module SRL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer
import Control.Monad.State

type Labels = (RL.Label, RL.Label, RL.Label, RL.Label)
p = (0,0)

type TrlMonad = WriterT RL.AST (State Int)

genLabel :: MonadState Int m => () -> m RL.Label
genLabel () = gets ((++) "lab" . show) <* modify (+1)

push :: MonadWriter RL.AST m => RL.Label -> RL.Block -> m ()
push l b = tell [(l, b)]

translate :: TypeTab -> SRL.AST -> String
translate ttab ast =
  RL.showAST ttab . evalState (execWriterT $ trlProg ast) $ 0

trlProg :: SRL.AST -> TrlMonad ()
trlProg ast = do
  l0 <- genLabel ()
  l1 <- genLabel ()
  l2 <- genLabel ()
  l3 <- genLabel ()

  let fs = Entry p
      ts = Goto l1 p

      fe = From l2 p
      te = Exit p

  push l0 (fs,[],ts)
  trlBlk ast (l0,l1,l2,l3)
  push l3 (fe,[],te)

trlBlk :: SRL.AST -> Labels -> TrlMonad ()
trlBlk blk (l0,l1,l4,l5) = case blk of
  s:blk -> do
    l2 <- genLabel ()
    l3 <- genLabel ()

    trlStmt s  (l0,l1,l2,l3)
    trlBlk blk (l2,l3,l4,l5)
  [] -> trlStmt (Skip p) (l0,l1,l4,l5)

trlStmt :: Stmt -> Labels -> TrlMonad ()
trlStmt (If t b1 b2 a _) (l0,l1,l6,l7) = do
  l2 <- genLabel ()
  l3 <- genLabel ()
  l4 <- genLabel ()
  l5 <- genLabel ()

  let fs = From l0 p
      ts = IfTo t l2 l4 p

      fe = Fi a l3 l5 p
      te = Goto l7 p

  push l1 (fs,[],ts)
  trlBlk b1 (l1,l2,l3,l6)
  trlBlk b2 (l1,l4,l5,l6)
  push l6 (fe,[],te)

trlStmt (Until d a b t _) (l0,l1,l4,l5) = do
  l2 <- genLabel ()
  l3 <- genLabel ()

  let fs = Fi a l0 l4 p
      ts = Goto l2 p

      fe = From l3 p
      te = IfTo t l5 l1 p

  push l1 (fs,[],ts)
  trlBlk b (l1,l2,l3,l4)
  push l4 (fe,[],te)

trlStmt s (l0,l1,l2,l3) = do
  let fs = From l0 p
      ts = Goto l2 p

      fe = From l1 p
      te = Goto l3 p

  push l1 (fs,[s],ts)
  push l2 (fe,[ ],te)
