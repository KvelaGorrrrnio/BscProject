module Translate where

import qualified RlAST as R
import qualified SrlAST as S
import Control.Monad.State
import Data.List

import Extra


ast = S.AST
  [
    Update "n" XorEq (Lit $ IntV 16)
  , Update "w" XorEq (Lit $ IntV 1)
  , Until (Relational Eq (Var "v") (Lit $ IntV 0))
    [
      Update "v" PlusEq (Var "w")
    , Swap "v" "w"
    , Update "n" MinusEq (Lit $ IntV 1)
    ]
    (Relational Eq (Var "n") (Lit $ IntV 0))
-- extra
   , If (Relational Eq (Var "n") (Lit $ IntV 0))
     [
       Update "v" PlusEq (Var "w")
     , Swap "v" "w"
     ]
     [
       If (Relational Eq (Var "n") (Lit $ IntV 0))
       [
         Update "v" PlusEq (Var "w")
       , Swap "v" "w"
       ]
       [Skip]
       (Relational Eq (Var "n") (Lit $ IntV 0))
     ]
     (Relational Eq (Var "n") (Lit $ IntV 0))
   ]

genLabel :: () -> State Int String
genLabel () = (++) "lab" . show <$> get <* modify (+1)

translate :: S.AST -> R.AST
translate (S.AST ast) = R.AST $ evalState (translateS "init" R.Entry R.Exit ast) 0

translateS :: R.Label -> R.From -> R.To -> [Stmt] -> State Int [(R.Label, R.Block)]
translateS thisL thisF thisT ss = do
  let (stmts,r)  = break isIfOrUntil ss
      thisB      = if null stmts then [Skip] else stmts
  case r of
    [] -> do
      let block = (thisL , R.Block (thisF, thisB, thisT))
      return [block]
    If t s1 s2 a : ss -> do
      thenL <- genLabel ()
      elseL <- genLabel ()
      endL  <- genLabel ()

      let ifblock = (thisL , R.Block (thisF, thisB, R.GIf t thenL elseL))

      thenblocks <- translateS thenL (R.From thisL)       (R.Goto endL)  s1
      elseblocks <- translateS elseL (R.From thisL)       (R.Goto endL)  s2
      endblocks  <- translateS endL  (R.Fi a thenL elseL) thisT ss

      return $ ifblock : (thenblocks ++ elseblocks ++ endblocks)

    Until a s t : ss -> do
      loopL <- genLabel ()
      let firstblock = (thisL , R.Block (thisF, thisB, R.Goto loopL))
      endL       <- genLabel ()
      loopblocks <- translateS loopL (R.Fi a thisL loopL) (R.GIf t endL loopL) s
      endblocks  <- translateS endL  (R.From loopL)       thisT ss

      return $ firstblock : loopblocks ++ endblocks

isIfOrUntil :: Stmt -> Bool
isIfOrUntil If{}    = True
isIfOrUntil Until{} = True
isIfOrUntil _         = False

main = print $ translate ast
