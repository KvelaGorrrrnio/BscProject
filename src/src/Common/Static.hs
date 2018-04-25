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

runStaticcheck :: [Stmt] -> Either Error ()
runStaticcheck = runExcept . staticcheckStmts

-- ==========
-- Statements
-- ==========
-- Iterate over statements
staticcheckStmts :: [Stmt] -> StaticState ()
staticcheckStmts = mapM_ staticcheckStmt

-- Statements
staticcheckStmt :: Stmt -> StaticState ()
staticcheckStmt (Update id op exp p) = when (exp `contains` id) $ throwError $ StaticError p $ SelfAbuse id
staticcheckStmt (Push id lid p) = when (id == lid) $ throwError $ StaticError p $ SelfAbuse id
staticcheckStmt (Pop id lid p) = when (id == lid) $ throwError $ StaticError p $ SelfAbuse id
staticcheckStmt _ = return ()

contains :: Exp -> Id -> Bool
contains (Binary _ e1 e2 _) id = contains e1 id || contains e2 id
contains (Unary  _ e _) id     = contains e id
contains (Parens e _) id       = contains e id
contains (Var id' _) id        = id'==id
contains Lit{} id              = False
