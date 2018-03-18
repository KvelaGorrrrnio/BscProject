module Main where
import Data.List
import Data.Maybe
import Data.Bits
import Control.Monad.State

-- When changing anything;
-- AST types
-- Reversion
-- toString
-- evaluation/interpretation

-- AST
type Var   = String

type AST   = Block
type Block = [Inst]

data Exp
  = Plus   Exp Exp
  | Minus  Exp Exp
  | Times  Exp Exp
  | Divide Exp Exp
  | Eq     Exp Exp
  | Gth    Exp Exp
  | Lth    Exp Exp
  | And    Exp Exp
  | Or     Exp Exp
  | Not    Exp
  | Var    Var
  | Const  Value
  deriving Show

data Inst
  = Swap Var Var
  | PlusEq  Var  Exp
  | MinusEq Var  Exp
  | XOREq   Var  Exp
  | If Exp Block Block Exp
  | From Exp Block Exp
  | Skip
  deriving Show

data Value
  = IntVal   Int
  | FloatVal Float
  | BoolVal  Bool
  | ListVal  [Value]
  deriving Show

-- Converting AST to string for pretty printing
astToString :: Int -> AST -> String
astToString ind = intercalate "\n" . map (\i -> replicate (ind*2) ' ' ++ instToString ind i)

instToString :: Int -> Inst -> String
instToString _ (Swap n1 n2)   = n1 ++ " <=> " ++ n2
instToString _ (PlusEq  n e)  = n ++ " += " ++ expToString e
instToString _ (MinusEq n e)  = n ++ " -= " ++ expToString e
instToString _ (XOREq n e)    = n ++ " ^= " ++ expToString e
instToString ind (If t b1 b2 a) =
  cyan "if" ++ " (" ++ expToString t ++ ") " ++ cyan "then\n" ++
  astToString (ind+1) b1  ++ cyan "\nelse\n" ++
  astToString (ind+1) b2  ++ cyan "\nfi" ++ " (" ++ expToString a ++ ")"
instToString ind (From a b t) =
  green "from" ++ " ("   ++ expToString a ++ ")\n"  ++
  replicate (ind*2) ' ' ++ green "do\n"  ++
  astToString (ind+1) b ++ "\n"    ++
  replicate (ind*2) ' ' ++ green "until" ++
  " (" ++ expToString t ++ ")"
instToString _ Skip           = yellow "skip"

expToString :: Exp -> String
expToString (Plus   e1 e2) = expToString e1 ++ " + "  ++ expToString e2
expToString (Minus  e1 e2) = expToString e1 ++ " - "  ++ expToString e2
expToString (Times  e1 e2) = expToString e1 ++ " * "  ++ expToString e2
expToString (Divide e1 e2) = expToString e1 ++ " / "  ++ expToString e2
expToString (Eq  e1 e2)    = expToString e1 ++ " == " ++ expToString e2
expToString (Lth e1 e2)    = expToString e1 ++ " < "  ++ expToString e2
expToString (Gth e1 e2)    = expToString e1 ++ " > "  ++ expToString e2
expToString (And e1 e2)    = expToString e1 ++ " && " ++ expToString e2
expToString (Or  e1 e2)    = expToString e1 ++ " || " ++ expToString e2
expToString (Not e1)       = "not (" ++ expToString e1 ++ ")"
expToString (Const v)      = valueToString v
expToString (Var n)        = n

valueToString :: Value -> String
valueToString (IntVal   n)  = show n
valueToString (FloatVal x)  = show x
valueToString (BoolVal b)   = show b
valueToString (ListVal lst) = concatMap valueToString lst

-- For color highlighting
black   s = "\x1b[30m" ++ s ++ "\x1b[0m"
red     s = "\x1b[31m" ++ s ++ "\x1b[0m"
green   s = "\x1b[32m" ++ s ++ "\x1b[0m"
yellow  s = "\x1b[33m" ++ s ++ "\x1b[0m"
blue    s = "\x1b[34m" ++ s ++ "\x1b[0m"
magenta s = "\x1b[35m" ++ s ++ "\x1b[0m"
cyan    s = "\x1b[36m" ++ s ++ "\x1b[0m"
white   s = "\x1b[37m" ++ s ++ "\x1b[0m"

-- Reversion
reverseAST :: AST -> AST
reverseAST = map reverseInst . reverse

reverseInst :: Inst -> Inst
reverseInst (Swap  n1 n2)  = Swap n1 n2
reverseInst (PlusEq  n e)  = MinusEq n e
reverseInst (MinusEq n e)  = PlusEq  n e
reverseInst (XOREq   n e)  = XOREq   n e
reverseInst (If t b1 b2 a) = If a (reverseAST b1) (reverseAST b2) t
reverseInst (From a b t)   = From t (reverseAST b) a
reverseInst Skip           = Skip

-- VarTab

type VarTab = [(Var, Value)]

update :: Var -> (Value -> Value -> Value) -> Value -> State VarTab ()
update name op val = state $ \vtab -> ((), update' name op val vtab)
  where update' name op val vtab = case vtab of
          (n,v):rst | n == name -> (n, op v val) : rst
                    | otherwise -> (n,v) : update' name op val rst
          []                   -> error "Variable not defined."

swap :: Var -> Var -> State VarTab ()
swap name1 name2 = do
  lu1 <- rd name1
  lu2 <- rd name2
  case (lu1, lu2) of
    (Just v1, Just v2) -> do
      update name1 (\_ v -> v) v2
      update name2 (\_ v -> v) v1
    (Nothing, Just _ ) -> error $ name1 ++ " not defined."
    (Just _,  Nothing) -> error $ name2 ++ " not defined."

fail :: String -> State (Either String VarTab)
fail msg = state $ \_ -> Left msg

wr :: Var -> Value -> State VarTab ()
wr name value = state $ \vtab -> ((),(name,value):vtab)

rd :: Var -> State VarTab (Maybe Value)
rd name = state $ \vtab -> (lookup name vtab, vtab)

varTabToString :: VarTab -> String
varTabToString = intercalate "\n" . map (\(n,v) -> n ++ " -> " ++ show v)

-- Interpreting engine --

-- interpreting a program
interpAST :: AST -> State VarTab ()
interpAST []      = return ()
interpAST (i:rst) = do --state $ \vtab -> ((), foldl (flip interpInst) vtab ast)
  interpInst i
  interpAST rst

-- interpreting an instruction
interpInst :: Inst -> State VarTab ()
interpInst i = case i of
  PlusEq name exp  -> do
    lu <- rd name
    case lu of
      Nothing -> do
        wr name (IntVal 0)
        interpInst i
      Just v -> do
        res <- eval exp
        update name (apply (+)) res
  MinusEq name exp -> do
    lu <- rd name
    case lu of
      Nothing -> do
        wr name (IntVal 0)
        interpInst i
      Just v -> do
        res <- eval exp
        update name (apply (-)) res
  XOREq name exp   -> do
    lu <- rd name
    case lu of
      Nothing -> do
        wr name (IntVal 0)
        interpInst i
      Just v -> do
        res <- eval exp
        update name (apply xor) res
  Swap name1 name2 -> swap name1 name2
  If t b1 b2 a     -> do
    bt <- eval t
    case bt of
      BoolVal bl1   -> do
        interpAST (if bl1 then b1 else b2)
        ba <- eval a
        case ba of
          BoolVal bl2 | bl1==bl2   -> return ()
                      | otherwise  -> error "Test and assertion do not match."
          _                        -> error "Assertion is not boolean."
      _             -> error "Test is not boolean."
  From a b t -> do
    ba <- eval a
    case ba of
      BoolVal True  -> nextIteration a b t --interpAST b
      BoolVal False -> error "Assertion evaluated to false."
      _             -> error "Assertion is not boolean."
  Skip              -> return ()

nextIteration :: Exp -> Block -> Exp -> State VarTab ()
nextIteration a b t = do
  interpAST b
  bt <- eval t
  case bt of
    BoolVal True  -> return ()
    BoolVal False -> do
      ba <- eval a
      case ba of
        BoolVal False -> nextIteration a b t
        BoolVal True  -> error "Assertion evaluated to true mid-loop."
        _             -> error "Assertion is not boolean."
    _             -> error "Test is not boolean."

apply :: (Int -> Int -> Int) -> Value -> Value -> Value
apply op (IntVal n) (IntVal m) = IntVal $ op n m
apply op _ _                   = error "Operands must be integers."

-- evaluating an expression
eval :: Exp -> State VarTab Value
eval (Plus e1 e2)   = do --case (eval e1 , eval e2 ) of
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (IntVal n, IntVal m)   -> return $ IntVal (n + m)
    _                      -> error "Adding two incompatible types."
eval (Minus e1 e2)  = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (IntVal n, IntVal m)   -> return $ IntVal (n - m)
    _                      -> error "Subtracting two incompatible types."
eval (Times e1 e2)  = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (IntVal n, IntVal m)   -> return $ IntVal (n * m)
    _                      -> error "Multiplying two incompatible types."
eval (Divide e1 e2) = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (IntVal n, IntVal m)   -> return $ IntVal (div n m)
    _                      -> error "Dividing two incompatible types."
eval (Eq  e1 e2)    = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (IntVal n, IntVal m)   -> return $ BoolVal (n == m)
    _                      -> error "Comparing == two incompatible types."
eval (Lth e1 e2)    = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (IntVal n, IntVal m)   -> return $ BoolVal (n < m)
    _                      -> error "Comparing < two incompatible types."
eval (Gth e1 e2)    = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (IntVal n, IntVal m)   -> return $ BoolVal (n > m)
    _                      -> error "Comparing > two incompatible types."
eval (And e1 e2)    = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (BoolVal n, BoolVal m) -> return $ BoolVal (n && m)
    _                      -> error "Anding two incompatible types."
eval (Or e1 e2)     = do
  res1 <- eval e1
  res2 <- eval e2
  case (res1, res2) of
    (BoolVal n, BoolVal m) -> return $ BoolVal (n || m)
    _                      -> error "Oring two incompatible types."
eval (Not e)        = do
  res <- eval e
  case res of
    BoolVal b              -> return $ BoolVal (not b)
    _                      -> error "Notting an non-boolean."
eval (Var name) = do --fromMaybe (IntVal 0) (lookup n )
  lu <- rd name
  case lu of
    Nothing -> do
      wr name (IntVal 0)
      eval $ Var name
    Just v  -> return v
eval (Const v) = return v

-- Helper to test implementation
testAST :: AST -> VarTab -> IO ()
testAST ast sstate = do
  let res  = execState (interpAST ast) sstate
      ast' = reverseAST ast
      res' = execState (interpAST ast') res
  putStrLn "Starting state:"
  putStrLn $ varTabToString sstate ++ "\n"
  putStrLn $ astToString 0 ast
  putStrLn "\nResult:"
  putStrLn $ varTabToString res
  putStrLn "\n---- Reversed ----\n"
  putStrLn $ astToString 0 ast'
  putStrLn "\nResult:"
  putStrLn $ varTabToString res'

-- Sample AST
ast =
  [
    XOREq "w" (Const $ IntVal 1),
    From
      (Eq (Var "v") (Const $ IntVal 0))
      [
        PlusEq "v" (Var "w"),
        Swap   "v" "w",
        MinusEq "n" (Const $ IntVal 1)
      ]
    (Or (Eq (Var "n") (Const $ IntVal 0)) (Gth (Var "v") (Var "w"))),
    If (Gth (Var "w") (Const $ IntVal 0))
      [
        PlusEq "x" (Var "w"),
        From
          (Eq (Var "x") (Var "w"))
          [
            MinusEq "x" (Const $ IntVal 1)
          ]
        (Eq (Var "x") (Const $ IntVal 0)),
        Swap "x" "w"
      ]
      [
        Skip
      ]
    (Eq (Var "w") (Const $ IntVal 0))
  ]

-- Main
main = testAST ast [("n", IntVal 16)]
