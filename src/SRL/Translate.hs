module Translate where

import qualified RlAST as R
import qualified SrlAST as S
import Control.Monad.Writer
import Data.List

import Extra


ast = S.AST
  [
    S.Update "n" S.XorEq (Lit $ IntV 16)
  , S.Update "w" S.XorEq (Lit $ IntV 1)
  , S.Until (Relational Eq (Var "v") (Lit $ IntV 0))
    [
      S.Update "v" S.PlusEq (Var "w")
    , S.Swap "v" "w"
    , S.Update "n" S.MinusEq (Lit $ IntV 1)
    ]
    (Relational Eq (Var "n") (Lit $ IntV 0))
  , S.If (Relational Eq (Var "n") (Lit $ IntV 0))
    [
      S.Update "v" S.PlusEq (Var "w")
    , S.Swap "v" "w"
    ]
    [
      S.If (Relational Eq (Var "n") (Lit $ IntV 0))
      [
        S.Update "v" S.PlusEq (Var "w")
      , S.Swap "v" "w"
      ]
      [S.Skip]
      (Relational Eq (Var "n") (Lit $ IntV 0))
    ]
    (Relational Eq (Var "n") (Lit $ IntV 0))
  ]

translate :: S.AST -> String
translate (S.AST ast) = (++"exit") $ execWriter $ translateS 0 "init" R.Entry ast

translateS :: Int -> String -> R.From -> [S.Stmt] -> Writer String ()
translateS num label from ss = do
  let (sblock,rest) = break isIfOrUntil ss
  tell $ label ++ show num ++ ": "
  tell $ show from ++ "\n"
  tell $ "  " ++ if null ss then "skip" else (intercalate "\n  " . map show) sblock
  tell "\n"
  case rest of
    [] -> return ()
    S.If t s1 s2 a : ss -> do
      tell $ "if " ++ show t ++ " then" ++ show (num + 1) ++ " else" ++ show (num + 1) ++ "\n\n"
      translateS (num+1) "then" (R.From label) s1
      tell "goto next\n\n"
      translateS (num+1) "else" (R.From label) s2
      tell "goto next\n\n"
      translateS (num+1) "next" (R.Fi a "then" "else") ss
    S.Until a s t : ss -> do
      tell $ "goto loop" ++ show (num+1) ++ "\n\n"
      translateS (num+1) "loop" (R.Fi a label "loop") s
      tell $ "if " ++ show t ++ " next" ++ show (num+1) ++ " loop" ++ show (num+1) ++ "\n\n"
      translateS (num+1) "next" (R.From "loop") ss


isIfOrUntil :: S.Stmt -> Bool
isIfOrUntil S.If{}    = True
isIfOrUntil S.Until{} = True
isIfOrUntil _         = False

main = putStrLn $ translate ast
