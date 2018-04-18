{-# LANGUAGE FlexibleContexts #-}

module Translation where

import qualified RlAST as R
import qualified SrlAST as S

import Control.Monad.Writer
import Control.Monad.State

import Extra

genLabel :: MonadState Int m => String -> m R.Label
genLabel lab = (++) lab . show <$> get
genLabelI :: MonadState Int m => String -> m R.Label
genLabelI lab = genLabel lab <* modify (+1)

translate :: S.AST -> R.AST
translate (S.AST ast) = R.AST $ (flip evalState 1 . execWriterT) (translateS "init" R.Entry R.Exit ast)

translateS :: R.Label -> R.From -> R.To -> [Stmt] -> WriterT [(R.Label, R.Block)] (State Int) R.Label
translateS thisL thisF thisT ss | thisB <- if null stmts then [Skip] else stmts =

  -- the next must be either an if, a loop or nothing
  case r of
    [] ->

      -- push this block to the AST
      tell [(thisL , R.Block (thisF, thisB, thisT))]

      >> return thisL

    If t s1 s2 a : ss -> do

      -- generate the labels for the blocks to come
      thenL <- genLabel  "then"
      elseL <- genLabel  "else"
      endL  <- genLabelI "contif"

      -- push this block to the AST
      tell [(thisL , R.Block (thisF, thisB, R.GIf t thenL elseL))]

      -- translate the bodies and fetch the end block labels
      tl <- translateS thenL (R.From thisL) (R.Goto endL)  s1
      el <- translateS elseL (R.From thisL) (R.Goto endL)  s2

      -- generate the next sequence of blocks
      translateS endL  (R.Fi a tl el) thisT ss

    Until a s t : ss -> do

      -- generate the labels for the blocks to come
      loopL <- genLabel  "loop"
      endL  <- genLabelI "contloop"

      -- push this block to the AST
      tell [(thisL , R.Block (thisF, thisB, R.Goto loopL))]

      -- translate the body and fetch the end block label
      ll <- translateS loopL (R.Fi a thisL loopL) (R.GIf t endL loopL) s

      -- generate the next sequence of blocks
      translateS endL (R.From ll) thisT ss

    _ -> fail "Something went very wrong."
  where (stmts,r) = break isIfOrUntil ss

isIfOrUntil :: Stmt -> Bool
isIfOrUntil If{}    = True
isIfOrUntil Until{} = True
isIfOrUntil _       = False
