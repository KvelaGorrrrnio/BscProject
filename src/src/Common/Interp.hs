{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Common.Interp (module Common.Interp, module Common.Log) where

import Common.Error
import Common.Log
import Common.AST

import Data.Bits (xor)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Loops
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap.Strict as I

-- ======================================
-- Monad transformer : The variable state
-- ======================================

type VarState = StateT VarTab (ExceptT Error (Writer Log))
execVarState :: VarTab -> VarState () -> (Either Error VarTab, Log)
execVarState vtab = runWriter . runExceptT . flip execStateT vtab

rd :: Id -> Pos -> VarState Value
rd (Id id exps) p = gets (mLookup id) >>= \case
  Just v  -> foldM getIdx v exps
  Nothing -> logError $ RuntimeError p $ CustomRT ("Variable '" ++ id ++ "' is not defined.")
  where getIdx (IntV _) _              = logError $ RuntimeError p $ CustomRT "Too many indices."
        getIdx (ListV v (ListT t)) idx = eval idx >>= \case
          ListV _ _ -> logError $ RuntimeError p $ CustomRT "Cant't use list as index."
          IntV i    -> case I.lookup (fromIntegral i) v of
            Just v' -> return v'
            Nothing -> return $ getDefaultValue t


  --case mLookup id vtab of --case gets (mLookup id) of
  --  Just v  -> foldM getIdx v exps
  --  Nothing -> logError $ RuntimeError p $ CustomRT ("Variable '" ++ id ++ "' is not defined.")
  --  where getIdx (IntV _) _      = logError $ RuntimeError p $ CustomRT "Too many indices."
  --        getIdx (ListV v t) idx = eval idx >>= \case
  --          ListV _ _ -> logError $ RuntimeError p $ CustomRT "Cant't use list as index."
  --          IntV i    -> return v

  ----where getIdx (IntV _) idx = logError $ RuntimeError p $ CustomRT "Too many indices."
  --      getIdx (ListV v t) idx        = eval idx >>= \case
  --        ListV _ _ -> logError $ RuntimeError p $ CustomRT "Cant't use list as index."
  --        IntV i     -> case I.lookup (fromIntegral i) v of
  --          Just v' -> return v'
  --          Nothing -> return $ getDefaultValue t


logStmt :: Stmt -> VarState ()
logStmt s = case s of
  If{}    -> exec s
  Until{} -> exec s
  _ -> do
    exec s
    msg <- gets (MsgStmt s)
    tell [msg]

logError :: Error -> VarState a
logError err = do
  tell [MsgError err]
  throwError err

logMsg :: MonadWriter Log w => String -> w ()
logMsg st = tell [MsgCustom st]

replace :: [Exp] -> Value -> Value -> Pos -> VarState Value
replace [] vn vo p | getType vn /= getType vo = logError $ RuntimeError p $ CustomRT "Couldn't update different values"
                   | otherwise                = return vn
replace (e:es) vn vo p = eval e >>= \case
  IntV i -> case vo of
    IntV _ -> logError $ RuntimeError p $ CustomRT "Too many indices."
    ListV im t -> case I.lookup (fromIntegral i) im of
      Just vi -> do
        vi' <- replace es vn vi p
        return $ ListV (I.insert (fromIntegral i) vi' im) t
      Nothing  -> return $ getDefaultValue t
  _      -> logError $ RuntimeError p $ CustomRT "Couldn't index with non-scalar"

adjust :: (Value -> Value) -> Id -> Pos -> VarTab -> VarState VarTab
adjust op (Id id []) p m   = return $ M.adjust op id m
adjust op (Id id exps) p m = case mLookup id m of
  Just (IntV _) -> logError $ RuntimeError p $ CustomRT "Too many indices."
  Just vo -> do
    mn <- adjust' op exps vo p
    return $ M.insert id mn m
adjust' :: (Value -> Value) -> [Exp] -> Value -> Pos -> VarState Value
adjust' op [] vo p = return $ op vo
adjust' op (e:es) vo p =  eval e >>= \case
  IntV i -> case vo of
    IntV _ -> logError $ RuntimeError p $ CustomRT "Too many indices."
    ListV im t -> case I.lookup (fromIntegral i) im of
      Just vi -> do
        vi' <- adjust' op es vi p
        return $ ListV (I.insert (fromIntegral i) vi' im) t
      Nothing  -> let ls = ListV (I.insert (fromIntegral i) (getDefaultValue t) im) t
        in adjust' op (e:es) ls p
  _      -> logError $ RuntimeError p $ CustomRT "Couldn't index with non-scalar"


-- ==========
-- Statements
-- ==========

execStmts :: [Stmt] -> VarState ()
execStmts = mapM_ logStmt

exec :: Stmt -> VarState ()

-- variable updates
exec (Update (Id id exps) op e p) = do
  v1 <- rd (Id id exps) p
  n <- case v1 of
    IntV n -> return n
    _      -> logError $ RuntimeError p $ CustomRT "Attempting to update list variable."

  v2 <- eval e
  m <- case v2 of
    IntV m -> return m
    _      -> logError $ RuntimeError p $ CustomRT "Attempting to update variable with a list expression."

  case op of
    DivEq  | mod n m == 0 -> return ()
           | otherwise -> logError $ RuntimeError p $ CustomRT "Division has rest in update."
    MultEq | n /= 0 && m /= 0 -> return ()
           | otherwise -> logError $ RuntimeError p $ CustomRT "An operand in mult update is zero."
    _ -> return ()

  res <- eval $ mapUpdOp op (Lit v1 p) (Lit v2 p) p
  lst <- rd (Id id []) p
  rep <- replace exps res lst p
  modify $ insert id rep

-- control flow : unique for SRL
exec (If t s1 s2 a p) = do
  q  <- eval t >>= \case
    IntV q -> return $ intToBool q
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  logMsg $ show (If t s1 s2 a p) ++ " -> " ++ if q then "true" else "false"
  execStmts $ if q then s1 else s2
  logMsg $ (if q then "[s1]" else "[s2]") ++ " done"

  r <- eval a >>= \case
    IntV r -> return $ intToBool r
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  when (q /= r)
    $ logError $ RuntimeError p $ CustomRT "Assert and such"

exec (Until d a s t p) = do -- log this
  logMsg $ show (Until d a s t p)

  q <- eval a >>= \case
    IntV q -> return $ intToBool q
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  unless (q == d) $ logError (RuntimeError p $ CustomRT "Assert")

  execStmts s

  r <- eval t >>= \case
    IntV r -> return $ intToBool r
    _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  unless r $ exec (Until False a s t p)

  logMsg $ show t ++ " -> " ++ "true"

  -- q <- eval a >>= \case
  --   IntV q -> return $ intToBool q
  --   _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig

  -- unless q $ logError $ RuntimeError p $ CustomRT "Assert"

  -- execStmts s

  -- whileM_ (cond t) $ do
  --     logMsg $ show t ++ " -> " ++ "false"
  --     r <- eval a >>= \case
  --       IntV r -> return $ intToBool r
  --       _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig
  --     when r $ throwError $ RuntimeError p $ CustomRT "Assert not good"
  --     execStmts s;

  --   where cond t = eval t >>= \case
  --                     IntV q -> not . intToBool $ q
  --                     _      -> logError $ RuntimeError p $ CustomRT "Type does not match in conditional." -- TODO: mere nøjagtig


-- list modification
exec (Push id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p
  case v2 of
    ListV ls (ListT t)
      | t == getType v1 -> do
        vtab <- get
        vtab' <- adjust clear id1 p vtab
        put =<< adjust (push v1) id2 p vtab'
      | otherwise ->
        logError $ RuntimeError p $ CustomRT "Types do not match" -- TODO: mere nøjagtig
    where push v (ListV ls t) = let i = (+1) . maximum . I.keys $ ls
            in ListV (I.insert i v ls) t
          clear (IntV _)    = IntV 0
          clear (ListV _ t) = ListV I.empty t

exec (Pop id1 id2 p) = do
  v1 <- rd id1 p
  unless (isClear v1) $
    logError $ RuntimeError p $ CustomRT ("Popping into non-clear variable '" ++ show id1 ++ "'.")

  v2 <- rd id2 p
  case v2 of
    ListV ls (ListT t)
      | I.null ls -> logError $ RuntimeError p $ CustomRT $ "Popping from empty list '" ++ show id2 ++ "'."
      | t == getType v1 -> do
        let i = (maximum . I.keys) ls
            v = ls I.! i
        vtab <- get
        vtab' <- adjust (const v) id1 p vtab
        put =<<  adjust (const $ ListV (I.delete i ls) t) id2 p vtab'
      | otherwise ->
        logError $ RuntimeError p $ CustomRT "Types do not match" -- TODO: mere nøjagtig
    _  -> logError $ RuntimeError p $ CustomRT "Popping from non-list"

-- swapping variables
exec (Swap id1 id2 p) = do
  v1 <- rd id1 p
  v2 <- rd id2 p
  vtab <- get
  vtab' <- adjust (const v2) id1 p vtab
  put =<< adjust (const v1) id2 p vtab'

-- skip
exec _ = return ()


-- ===========
-- Expressions
-- ===========

eval :: Exp -> VarState Value

-- terminals
eval (Lit v _)  = return v
eval (Var id p) = rd id p


  -- binary arithmetic and relational
eval (Binary op l r p)
  | op < Geq  = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m) -> return $ IntV (mapBinOp op n m)
      _                -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- binary div and mod
  | op <= Mod = do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (IntV n, IntV m)
        | m == 0    -> logError $ RuntimeError p $ CustomRT "Dividing by zero."
        | otherwise -> return $ IntV (mapBinOp op n m)
      _             -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig
  -- binary logical
  | otherwise = eval l >>= \case
    IntV 0 | op==And        -> return $ IntV 0
    IntV v | v/=0 && op==Or -> return $ IntV 1
    IntV _ -> eval r >>= \case
        IntV n -> return $ IntV (norm n)
        _      -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig
    _ -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- unary arithmetic
eval (Unary op exp p)
  | op <= Sign = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    _      -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- unary logical
  | op < Size = eval exp >>= \case
    IntV n -> return $ IntV (mapUnOp op n)
    _      -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

  -- unary list
  | otherwise = eval exp >>= \case
    ListV ls t -> case op of
      Top   -> case ls of
        ls | I.null ls  -> logError $ RuntimeError p $ CustomRT "Accessing top of empty list."
           | otherwise  -> let i = (maximum . I.keys) ls in return $ ls I.! i
      Empty -> return $ IntV (boolToInt . I.null $ ls)
      Size  -> return $ IntV (fromIntegral . I.size $ ls)
    _  -> logError $ RuntimeError p $ CustomRT "Type error in expression." -- TODO: mere nøjagtig

-- parantheses
eval (Parens e p) = eval e
