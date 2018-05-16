module RL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL
import Data.HashMap.Strict (fromList, union)
import Debug.Trace

import Control.Monad.Writer

translate :: TypeTab -> RL.AST -> String
translate ttab ast = let ttab' = ttab `union` initTypeDec in SRL.showAST ttab' $ trlProg ast

p :: Pos
p = (0,0)

trlProg :: RL.AST -> SRL.AST
trlProg ast = let
  n  = fromIntegral . length $ ast
  b1 = SRL.If (Unary Null (Var (Id "S" []) p) p) skip skip (Lit (IntV 1) p) p
  b2 = Atom $ Update (Id "S" [Lit (IntV 0) p, Lit (IntV 1) p, Lit (IntV 0) p]) XorEq (Lit (IntV 1) p) p
  b3 = Until True (Var (Id "S" [Lit (IntV 0) p, Lit (IntV 1) p, Lit (IntV 0) p]) p) (trlBlocks ast) skip (Var (Id "S" [Lit (IntV n) p, Lit (IntV n) p, Lit (IntV 0) p]) p) p
  b4 = Atom $ Update (Id "S" [Lit (IntV n) p, Lit (IntV n) p, Lit (IntV 0) p]) XorEq (Lit (IntV 1) p) p
  b5 = SRL.If (Unary Null (Var (Id "S" []) p) p) skip skip (Lit (IntV 1) p) p
  in Seq (setupLoop (length ast)) $ Seq b1 $ Seq b2 $ Seq b3 $ Seq b4 b5

skip = Atom $ Skip p

initTypeDec :: TypeTab
initTypeDec = fromList
  [ ("S",  ListT (ListT (ListT IntT)))
  , ("S1", ListT (ListT IntT))
  , ("S2", ListT IntT)
  , ("s",  IntT)
  ]

setupLoop :: Int -> SRL.Block
setupLoop n | n' <- fromIntegral (n + 1) = let
  a  = Unary Empty (Var (Id "S"  []) p)  p
  a1 = Unary Empty (Var (Id "S1" []) p)  p
  a2 = Unary Empty (Var (Id "S2" []) p)  p
  t  = Binary Equal (Unary Size (Var (Id "S"  []) p) p) (Lit (IntV n') p) p
  t1 = Binary Equal (Unary Size (Var (Id "S1" []) p) p) (Lit (IntV n') p) p
  t2 = Binary Equal (Unary Size (Var (Id "S2" []) p) p) (Lit (IntV 3) p) p
  b  = SRL.Seq (Until True a1 b1 skip t1 p) (Atom $ Push (Id "S1" []) (Id "S"  []) p)
  b1 = SRL.Seq (Until True a2 b2 skip t2 p) (Atom $ Push (Id "S2" []) (Id "S1" []) p)
  b2 = Atom $ Push (Id "s" []) (Id "S2" []) p
  in Until True a b skip t p

trlBlocks :: RL.AST -> SRL.Block
trlBlocks ast = foldr trlBlock (ef (Lit (IntV 0) p)) $ indexLabels ast

trlBlock :: (Int,RL.Block) -> SRL.Block -> SRL.Block
trlBlock (i,(f,ss,j)) sb = trlFrom f i $ trlSteps ss i $ trlJump j i sb

trlSteps ss i b = skip
trlJump j i b = skip

trlFrom :: From -> Int -> SRL.Block -> SRL.Block
trlFrom (Entry _) 0 b = SRL.If (Var (Id "S" [Lit (IntV 0) p, Lit (IntV 1) p, Lit (IntV 0) p]) p) skip skip (Lit (IntV 1) p) p

-- Make sure Entry gets 0
indexLabels :: RL.AST -> [(Int,RL.Block)]
indexLabels = foldl (\acc (l,b) -> acc ++ [(length acc,b)]) []

ef t = SRL.If t skip skip (Lit (IntV 1) p) p

-- list list list int S
-- list list int      S1
-- list int           S2
-- int                s
--
-- from empty S do
--   from empty S1 do
--     from empty S2 do
--       push s S2
--     loop .
--     until size S2 = 3
--     push S2 S1
--   loop .
--   until size S1 = (Lit (IntV . (+1) . length $ ast) p)
-- loop .
-- until size S = (Lit (IntV . (+1) . length $ ast) p)
--
--
-- list list list int S


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
