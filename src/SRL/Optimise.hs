module SRL.Optimise where

import SRL.AST


import Data.Maybe (mapMaybe)
import Data.HashMap.Strict (HashMap, lookup, insert, adjust)

import Control.Monad.State

type ConstTab = HashMap Id Int

type ConstState = State (HashMap Id Int)

optimise :: AST -> AST
optimise = optimiseStmts

optimiseStmts :: [Stmt] -> [Stmt]
optimiseStmts = mapMaybe optimiseStmt

optimiseStmt :: Stmt -> Maybe Stmt
optimiseStmt Skip{}           = Nothing
optimiseStmt (If t s1 s2 a p) = Just $ If t (optimiseStmts s1) (optimiseStmts s2) a p
optimiseStmt (Until a s t p)  = Just $ Until a (optimiseStmts s) t p
optimiseStmt (Swap id1 id2 p) | id1 == id2 = Nothing
  | otherwise = Just $ Swap id1 id2 p
optimiseStmt s                = Just s

isConst :: Id -> AST -> Bool
isConst id = not . any (\case Update id' _ _ _ -> id'==id
                              If _ s1 s2 _ _   -> isConst id s1 && isConst id s2
                              Until _ s _ _    -> isConst id s
                              Swap id1 id2 _   -> id==id1 || id==id2
                              _ -> False
                       )
