module RL.Translation
( translate
) where

import RL.AST as RL
import SRL.AST as SRL

import qualified Data.HashMap.Strict as M

import Control.Monad.Reader

type LabelMap = (String, M.HashMap Label Int)
type TrlReader = Reader LabelMap

mapLabel :: Label -> TrlReader Int
mapLabel l = asks ((M.! l) . snd)

vec :: TrlReader String
vec = asks fst

initTypeDec :: TrlReader TypeTab
initTypeDec = do
  x <- vec
  return $ M.fromList
    [ (x,  ListT (ListT (ListT IntT)))
    , ("S1", ListT (ListT IntT))
    , ("S2", ListT IntT)
    , ("s",  IntT)
    ]

-- TODO: Make sure Entry gets 0 Exit n
genLabelMap :: TypeTab -> RL.AST -> LabelMap
genLabelMap ttab ast =
  let lm = M.fromList . foldl (\acc (l,b) -> (l,length acc + 1):acc) [] $ ast
    in (genVec ttab,lm)

genVec :: TypeTab -> String
genVec ttab = "S"

-- Code generation
genInit id n        = Step $ Init id [genLit n, genLit n, genLit 3] p
genFree id n        = Step $ Free id [genLit n, genLit n, genLit 3] p
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
pMac :: Int -> (Int,Int,Int) -> TrlReader SRL.Block
pMac j' (j,i,k) = do
  x <- vec
  let k' = (k + 1) `mod` 3
  return $ genSwap (genId x [j,i,k]) (genId x [j',i,k'])

rMac :: Int -> (Int,Int,Int) -> TrlReader SRL.Block
rMac i' (j,i,k) = do
  x <- vec
  let k' = (k + 1) `mod` 3
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
  let (ttab',ast') = runReader (trlProg ast) (genLabelMap ttab ast)
    in SRL.showAST (ttab `M.union` ttab') ast'

trlProg :: RL.AST -> TrlReader (TypeTab,SRL.AST)
trlProg ast = do
  x  <- vec

  let n  = fromIntegral . length $ ast
      b0 = genInit x (n + 2)
      b1 = genIf (genNull (genVar x [])) genSkip genSkip (genLit 1)
      b2 = genUpdate (genId x [0,1,0]) XorEq (genLit 1)

  bs <- trlBlocks ast
  let b3 = genUntil (genVar x [0,1,0]) bs genSkip (genVar x [n,n+1,0])
      b4 = genUpdate (genId x [n,n+1,0]) XorEq (genLit 1)
      b5 = b1
      b6 = genFree x (n + 2)

  ttab <- initTypeDec
  return (ttab, genSeq [b0,b1,b2,b3,b4,b5,b6])

trlBlocks :: RL.AST -> TrlReader SRL.Block
trlBlocks = foldrM trlBlock (ef . genLit $ 0)
  where foldrM f e = foldr ((=<<) . f) (return e)

trlBlock :: (Label,RL.Block) -> SRL.Block -> TrlReader SRL.Block
trlBlock (l,(f,ss,j)) sb = do
  i <- mapLabel l
  -- trlJump j i sb >>= trlSteps ss i >>= trlFrom f i
  trlFrom f i =<< trlSteps ss i =<< trlJump j i sb

-- comefroms
trlFrom :: From -> Int -> SRL.Block -> TrlReader SRL.Block
trlFrom (Entry _) 1 b2 = do -- TODO: i skal være 1?
  x  <- vec

  let t = genVar x [0,1,0]
  b1 <- pMac 1 (0,1,0)
  let a = genVar x [1,1,1]

  return $ genIf t b1 b2 a

trlFrom (From l _) i b2 = do
  x  <- vec

  j  <- mapLabel l

  let t = genVar x [j,i,0]
  b1 <- pMac i (j,i,0)
  let a = genVar x [i,i,1]

  return $ genIf t b1 b2 a
trlFrom (Entry _) n _ = fail "Entry not the starting state."

trlFrom (Fi e lj lk _) i b2 = do
  x <- vec

  j <- mapLabel lj
  k <- mapLabel lk

  -- outer test
  let t2 = genOr (genVar x [j,i,0]) (genVar x [k,i,0])

  -- inner if-block
  b1 <- do
    let t1 = genVar x [j,i,0]
    b1 <- pMac i (j,i,0)
    b2 <- pMac i (k,i,0)
    let a1 = e
    return $ genIf t1 b1 b2 a1

  -- outer assertion
  let a2 = genVar x [i,i,1]

  return $ genIf t2 b1 b2 a2


-- steps
trlSteps :: [Stmt] -> Int -> SRL.Block -> TrlReader SRL.Block
trlSteps ss i b2 = do
  x <- vec

  let t  = genVar x [i,i,1]
  b1 <- do
    bm <- pMac i (i,i,1)
    return $ genSeq $ map Step ss ++ [bm]
  let a  = genVar x [i,i,2]

  return $ genIf t b1 b2 a


-- jumps
trlJump :: Jump -> Int -> SRL.Block -> TrlReader SRL.Block
trlJump (Exit _) n b2 = do
  n' <- asks length
  unless (n == n') $ fail "Exit not the ending state."

  x <- vec

  let t = genVar x [n,n,2]
  b1 <- rMac (n+1) (n,n,2)
  let a = genVar x [n,n+1,0]

  return $ genIf t b1 b2 a

trlJump (Goto lj _) i b2 = do
  x <- vec

  j <- mapLabel lj

  let t = genVar x [i,i,2]
  b1 <- rMac j (i,i,2)
  let a = genVar x [i,j,0]

  return $ genIf t b1 b2 a

trlJump (RL.If e lj lk _) i b2 = do
  x <- vec

  j <- mapLabel lj
  k <- mapLabel lk

  -- outer test
  let t2 = genVar x [i,i,2]

  -- inner if-block
  b1 <- do
    let t1 = e
    b1 <- rMac j (i,i,2)
    b2 <- rMac k (i,i,2)
    let a1 = genVar x [i,j,0]
    return $ genIf t1 b1 b2 a1

  -- outer assertion
  let a2 = genOr (genVar x [i,j,0]) (genVar x [i,k,0])

  return $ genIf t2 b1 b2 a2

