module RL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL
import qualified Data.HashMap.Strict as M
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace

import Control.Monad.Writer

type LabelMap = (String,M.HashMap Label Int)
type LabelState = StateT (Int,Int,Int) (Reader LabelMap)

mapLabel :: Label -> LabelState Int
mapLabel l = asks ((M.! l) . snd)

vec :: LabelState String
vec = asks fst

-- TODO: Make sure Entry gets 0 Exit n
genLabelMap :: TypeTab -> RL.AST -> LabelMap
genLabelMap ttab ast = let lm = M.fromList $ foldl (\acc (l,b) -> (l,length acc + 1):acc) [] ast in (genVec ttab,lm)

genVec :: TypeTab -> String
genVec ttab = "S"

-- CODEGEN START
genId id idxs = let is = map (\idx -> Lit (IntV . fromIntegral $ idx) p) idxs in Id id is
genVarId id idxs = Var (genId id idxs) p

genEmpty id    = Unary Empty id p
genNull id     = Unary Null id p
genSize id     = Unary Size id p
genLit n       = Lit (IntV n) p
genEqual e1 e2 = Binary Equal e1 e2 p
genOr e1 e2    = Binary Or e1 e2 p
genIf t b1 b2 a = SRL.If t b1 b2 a p
genSwap id1 id2 = Atom $ Swap id1 id2 p
genUpdate id op e = Atom $ Update id op e p
genUntil a b1 b2 t = Until True a b1 b2 t p
genPush id1 id2 = Atom $ Push id1 id2 p
genSeq [] = genSkip
genSeq s  = foldl1 Seq s
genSkip = Atom $ Skip p
-- CODEGEN END

pMac :: Int -> LabelState SRL.Block
pMac j' = do
  x <- vec
  (i,j,k) <- get
  let k' = (k + 1) `mod` 3
  put (i,j',k')
  genSwap (genId x [i,j,k]) (genId x [i,j',k'])
rMac :: Int -> LabelState SRL.Block
rMac j = genSkip

p :: Pos
p = (0,0)

translate :: TypeTab -> RL.AST -> String
translate ttab ast = let (ttab',ast') = runReader (evalStateT (trlProg ast) (0,1,0)) (genLabelMap ttab ast) in SRL.showAST (ttab `M.union` ttab') ast'

trlProg :: RL.AST -> LabelState (TypeTab,SRL.AST)
trlProg ast = do
  blocks <- trlBlocks ast
  x <- vec
  let n  = fromIntegral . length $ ast
      b1 = genIf (genNull (genVarId x [])) genSkip genSkip (genLit 1)
      b2 = genUpdate (genId x [0,1,0]) XorEq (genLit 1)
      b3 = genUntil (genVarId x [0,1,0]) blocks genSkip (genVarId x [n,n,0])
      b4 = genUpdate (genId x [n,n,0]) XorEq (genLit 1)
      b5 = genIf (genNull (genVarId x [])) genSkip genSkip (genLit 1)
      seq = [b1,b2,b3,b4,b5]
  sl <- setupLoop (length ast)
  ttab <- initTypeDec
  return (ttab, genSeq (sl:seq))

initTypeDec :: LabelState TypeTab
initTypeDec = do
  x <- vec
  return $ M.fromList
    [ (x,  ListT (ListT (ListT IntT)))
    , ("S1", ListT (ListT IntT))
    , ("S2", ListT IntT)
    , ("s",  IntT)
    ]

setupLoop :: Int -> LabelState SRL.Block
setupLoop n = do
  x <- vec
  let a  = genEmpty $ genVarId x  []
      a1 = genEmpty $ genVarId "S1" []
      a2 = genEmpty $ genVarId "S2" []
      n' = genLit (fromIntegral (n + 2))
      t  = genEqual (genSize (genVarId x  [])) n'
      t1 = genEqual (genSize (genVarId "S1" [])) n'
      t2 = genEqual (genSize (genVarId "S2" [])) n'
      b  = genSeq [genUntil a1 b2 genSkip t1, genPush (genId "S1" []) (genId x  [])]
      b1 = genSeq [genUntil a2 b2 genSkip t2, genPush (genId "S2" []) (genId "S1" [])]
      b2 = genPush (genId "s" []) (genId "S2" [])
    in return $ genUntil a b genSkip t

trlBlocks :: RL.AST -> LabelState SRL.Block
trlBlocks ast = foldM trlBlock (ef (Lit (IntV 0) p)) (reverse ast)

trlBlock :: SRL.Block -> (Label,RL.Block) -> LabelState SRL.Block
trlBlock sb (l,(f,ss,j)) = do
  i <- mapLabel l
  trlFrom f i $ trlSteps ss i $ trlJump j i sb

trlFrom :: From -> Int -> SRL.Block -> LabelState SRL.Block
trlFrom (Entry _)  1 b = do
  x <- vec
  return $ SRL.If (genVarId x [0,1,0]) (pMac x 0 1) b (Lit (IntV 1) p) p
trlFrom (From l _) i b = do
  x <- vec
  j <- mapLabel l
  return $ SRL.If (genVarId x [j,i,0]) (pMac x j i) b (genVarId x [1,1,1]) p
trlFrom (Fi e lj lk _) i b = do
  x <- vec
  j <- mapLabel lj
  k <- mapLabel lk
  let t1 = genOr (genVarId x [j,i,2]) (genVarId x [k,i,0])
      t2 = genVarId x [j,i,0]
      a1 = genVarId x [i,i,1]
      a2 = e
      if1 = genIf t1 if2 b a1
      if2 = genIf t2 (pMac x j i) (pMac x k i) a2
  return if1

trlSteps ss i b = genSkip
trlJump j i b = genSkip

trace' :: Show a => a -> b -> b
trace' a = trace ("\n" ++ show a ++ "\n")

ef t = SRL.If t genSkip genSkip (Lit (IntV 1) p) p





