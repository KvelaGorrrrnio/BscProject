module Common.Static
( StaticState
, runStaticcheck
, staticcheckStmts
, staticcheckStmt
) where

import Common.AST
import Common.Error
import Control.Monad.Except

type StaticState = Except Error

runStaticcheck :: a -> (a -> StaticState ()) -> Either Error ()
runStaticcheck ast init = runExcept $ init ast

-- ==========
-- Statements
-- ==========
-- Iterate over statements
staticcheckStmts :: [Stmt] -> StaticState ()
staticcheckStmts = mapM_ staticcheckStmt

-- Statements
staticcheckStmt :: Stmt -> StaticState ()
staticcheckStmt (Update id op exp p) | exp `contains` id = throwError $ StaticError p $ SelfAbuse id
                                     | otherwise         = return ()
staticcheckStmt (Push id lid p) | id == lid = throwError $ StaticError p $ SelfAbuse id
                                | otherwise = return ()
staticcheckStmt (Pop id lid p)  | id == lid = throwError $ StaticError p $ SelfAbuse id
                                | otherwise = return ()
staticcheckStmt _ = return ()

contains :: Exp -> Id -> Bool
contains (Binary _ e1 e2 _) id    = contains e1 id && contains e2 id
contains (Unary  _ e _) id        = contains e id
contains (Parens e _) id          = contains e id
contains (Var n _) id | n==id     = True
                      | otherwise = False
contains (Lit _ _) _              = False
