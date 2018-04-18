{-# LANGUAGE FlexibleContexts #-}

module RL.Translation
( translateToSRLSource
) where

import RL.AST as RL
import SRL.AST as SRL

import Control.Monad.Writer

translateToSRLSource :: SRL.AST -> String
translateToSRLSource ast = RL.showAST $ flip evalState 1 . execWriterT $ translateS "init" Entry Exit ast

translateS :: Label -> From -> To -> [Stmt] -> WriterT RL.AST (State Int) Label
translateS thisL thisF thisT ss | thisB <- if null stmts then [Skip] else stmts =

  -- the next must be either an if, a loop or nothing
  case r of
    [] ->

      -- push this block to the AST
      tell [(thisL , (thisF, thisB, thisT))]

      >> return thisL

    If t s1 s2 a : ss -> do

      -- generate the labels for the blocks to come
      thenL <- genLabel  "then"
      elseL <- genLabel  "else"
      endL  <- genLabelI "contif"

      -- push this block to the AST
      tell [(thisL , (thisF, thisB, IfTo t thenL elseL))]

      -- translate the bodies and fetch the end block labels
      tl <- translateS thenL (From thisL) (Goto endL)  s1
      el <- translateS elseL (From thisL) (Goto endL)  s2

      -- generate the next sequence of blocks
      translateS endL  (Fi a tl el) thisT ss

    Until a s t : ss -> do

      -- generate the labels for the blocks to come
      loopL <- genLabel  "loop"
      endL  <- genLabelI "contloop"

      -- push this block to the AST
      tell [(thisL , (thisF, thisB, Goto loopL))]

      -- translate the body and fetch the end block label
      ll <- translateS loopL (Fi a thisL loopL) (IfTo t endL loopL) s

      -- generate the next sequence of blocks
      translateS endL (From ll) thisT ss

    _ -> fail "Something went very wrong."
  where (stmts,r) = break isIfOrUntil ss

isIfOrUntil :: Stmt -> Bool
isIfOrUntil If{}    = True
isIfOrUntil Until{} = True
isIfOrUntil _       = False
