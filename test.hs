{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Test where
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer

import qualified Data.HashMap.Strict as M

-- statements
data Stmt   = Assign Id Value
type Id     = String
type Value  = Int

-- other types
type AST    = M.HashMap Id (Stmt,Id)
type VarTab = M.HashMap Id Int
type Error  = String
type Log    = [String]

logStmt :: MonadWriter Log m => Stmt -> m ()
logStmt (Assign id n) = tell [id ++ " := " ++ show n]

-- outer state
type ProgState = ReaderT AST VarState
-- inner state
type VarState  = StateT VarTab (ExceptT Error (Writer Log))

-- interpret a program
interp :: AST -> (Either Error VarTab, Log)
interp ast = case M.lookup "ini" ast of
   Just ini -> (runWriter . runExceptT . flip execStateT M.empty . runReaderT (interpBlock ini)) ast
   Nothing  -> error "no ini"

-- interp block
interpBlock :: (Stmt,Id) -> ProgState ()
interpBlock (s,nxt) = do
  logStmt s
  exec s
  if nxt == "exit" then return ()
  else M.lookup nxt <$> ask >>= \case
      Just blk -> interpBlock blk
      Nothing  -> error $ nxt ++ " is not defined."

-- exec statement
exec :: MonadState VarTab m => Stmt -> m ()
exec (Assign id n) = do
  throwError "hej"
  modify $ M.insert id n

prog = M.fromList
  [
    ("ini", (Assign "a" 5,  "snd"  ))
  , ("snd", (Assign "b" 11, "trd"  ))
  , ("end", (Assign "d" 9,  "exit" ))
  , ("trd", (Assign "c" 2,  "end"  ))
  ]

main = print $ interp prog
