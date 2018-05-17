module RL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL
import qualified Data.HashMap.Strict as M
import Control.Monad.Reader
import Control.Monad.State

import Control.Monad.Writer

import Debug.Trace

type LabelMap = (String, M.HashMap Label Int)
type TrlState = StateT (Int,Int,Int) (Reader LabelMap)

mapLabel :: Label -> TrlState Int
mapLabel l = asks ((M.! l) . snd)

vec :: TrlState String
vec = asks fst

initTypeDec :: TrlState TypeTab
initTypeDec = do
  x <- vec
  return $ M.fromList
    [ (x,  ListT (ListT (ListT IntT)))
    , ("S1", ListT (ListT IntT))
    , ("S2", ListT IntT)
    , ("s",  IntT)
    ]

-- setting up the states
setupLoop :: Int -> TrlState SRL.Block
setupLoop n = do
  x <- vec

  let b  = genSkip

      -- inner loop
      a1 = genEmpty $ genVar "S2" []
      b1 = genPush  (genId "s" []) (genId "S2" [])
      t1 = genEqual (genSize (genVar "S2" [])) (genLit 3)

      s1 = genPush (genId "S2" []) (genId "S1" [])

      l1 = genSeq [genUntil a1 b1 b t1, s1]

      -- next outer loop
      n' = genLit (fromIntegral (n + 2))

      a2 = genEmpty $ genVar "S1" []
      b2 = l1
      t2 = genEqual (genSize (genVar "S1" [])) n'

      s2 = genPush (genId "S1" []) (genId x [])

      l2 = genSeq [genUntil a2 b2 b t2, s2]

      -- outermost loop
      a3 = genEmpty $ genVar x []
      b3 = l2
      t3 = genEqual (genSize (genVar x [])) n'

  return $ genUntil a3 b3 b t3

-- TODO: Make sure Entry gets 0 Exit n
genLabelMap :: TypeTab -> RL.AST -> LabelMap
genLabelMap ttab ast =
  let lm = (M.fromList . foldl (\acc (l,b) -> (l,length acc + 1):acc) []) ast
    in trace (show lm) (genVec ttab,lm)

genVec :: TypeTab -> String
genVec ttab = "S"

-- Code generation
genId id            = Id id . map (\idx -> Lit (IntV . fromIntegral $ idx) p)
genVar id idxs      = Var (genId id idxs) p
genEmpty id         = Unary Empty id p
genNull id          = Unary Null id p
genSize id          = Unary Size id p
genLit n            = Lit (IntV n) p
genEqual e1 e2      = Binary Equal e1 e2 p
genOr e1 e2         = Binary Or e1 e2 p
genIf t b1 b2 a     = SRL.If t b1 b2 a p
genSwap id1 id2     = Step $ Swap id1 id2 p
genUpdate id op e   = Step $ Update id op e p
genUntil a b1 b2 t  = Until True a b1 b2 t p
genPush id1 id2     = Step $ Push id1 id2 p
genSeq []           = genSkip
genSeq s            = foldl1 Seq s
genSkip             = Step $ Skip p

-- Macros
pMac :: Int -> Int -> TrlState SRL.Block
pMac oj j' = do
  x <- vec
  (j,i,k) <- get
  let k' = (k + 1) `mod` 3
  put (j',i,k')
  return $ genSwap (genId x [j,i,k]) (genId x [j',i,k'])

rMac :: Int -> Int -> TrlState SRL.Block
rMac oi i' = do
  x <- vec
  (j,i,k) <- get
  let k' = (k + 1) `mod` 3
  put (j,i',k')
  return $ genSwap (genId x [j,i,k]) (genId x [j,i',k'])

-- Sanity check
ef t = genIf t genSkip genSkip (genLit 1)

-- Generic position
p = (0,0)

-- ==================
-- Actual translation
-- ==================

translate :: TypeTab -> RL.AST -> String
translate ttab ast =
  let (ttab',ast') = runReader (evalStateT (trlProg ast) (0,0,0)) (genLabelMap ttab ast)
    in SRL.showAST (ttab `M.union` ttab') ast'

trlProg :: RL.AST -> TrlState (TypeTab,SRL.AST)
trlProg ast = do
  x  <- vec
  sl <- setupLoop (length ast)
  bs <- trlBlocks ast
  let n  = fromIntegral . length $ ast
      b1 = genIf (genNull (genVar x [])) genSkip genSkip (genLit 1)
      b2 = genUpdate (genId x [0,1,0]) XorEq (genLit 1)
      b3 = genUntil (genVar x [0,1,0]) bs genSkip (genVar x [n,n+1,0])
      b4 = genUpdate (genId x [n,n+1,0]) XorEq (genLit 1)
      b5 = b1
      b  = genSeq [sl,b1,b2,b3,b4,b5]
  ttab <- initTypeDec
  return (ttab, b)

trlBlocks :: RL.AST -> TrlState SRL.Block
trlBlocks = foldrM trlBlock (ef . genLit $ 0)
  where foldrM f e = foldr ((=<<) . f) (return e)

trlBlock :: (Label,RL.Block) -> SRL.Block -> TrlState SRL.Block
trlBlock (l,(f,ss,j)) sb = do
  i <- mapLabel l
  trlFrom f i =<< trlSteps ss i =<< trlJump j i sb

-- comefroms
trlFrom :: From -> Int -> SRL.Block -> TrlState SRL.Block
trlFrom (Entry _) s b2 = do -- TODO: s skal være 1?
  x <- vec
  b1 <- pMac 0 s
  return $ genIf (genVar x [0,1,0]) b1 b2 (genLit 1)

trlFrom (From l _) i b2 = do
  x  <- vec
  j  <- mapLabel l
  b1 <- pMac j i
  return $ genIf (genVar x [j,i,0]) b1 b2 (genVar x [i,i,1])

trlFrom (Fi e lj lk _) i b = do
  x <- vec

  j <- mapLabel lj
  k <- mapLabel lk

  b1 <- pMac j i
  b2 <- pMac k i

  let t1  = genVar x [j,i,0]
      a1  = e
      i1  = genIf t1 b1 b2 a1

      t2  = genOr (genVar x [j,i,0]) (genVar x [k,i,0])
      a2 = genVar x [i,i,1]

  return $ genIf t2 i1 b a2


-- steps
trlSteps :: [Stmt] -> Int -> SRL.Block -> TrlState SRL.Block
trlSteps ss i b2 = do
  x <- vec

  b1' <- pMac i i

  let t  = genVar x [i,i,1]
      b1 = genSeq (map Step ss ++ [b1'])
      a  = genVar x [i,i,2]
  return $ genIf t b1 b2 a

-- jumps
trlJump :: Jump -> Int -> SRL.Block -> TrlState SRL.Block
trlJump (RL.If e lj lk _) i b = do
  x <- vec

  j <- mapLabel lj
  k <- mapLabel lk

  b1 <- rMac i j
  b2 <- rMac i k

  let t1  = e
      a1  = genVar x [i,j,0]
      i1  = genIf t1 b1 b2 a1

      t2 = genVar x [i,i,2]
      a2 = genOr (genVar x [i,j,0]) (genVar x [i,k,0])

  return $ genIf t2 i1 b a2

trlJump (Goto lj _) i b2 = do
  x <- vec

  j <- mapLabel lj

  b1 <- rMac i j

  let t = genVar x [i,i,2]
      a = genVar x [i,j,0]
  return $ genIf t b1 b2 a

trlJump (Exit _) n b2 = do -- TODO: n skal være netop n (længden af AST'et)
  x <- vec

  b1 <- rMac n (n+1)

  let t = genVar x [n,n,2]
      a = genVar x [n,n+1,0]
  return $ genIf t b1 b2 a
