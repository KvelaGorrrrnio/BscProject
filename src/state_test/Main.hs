module Main where
import Control.Monad.State
import SymTab
import AST

main = do

let ast = AST.toAST
          [
            Block
            (Label "init")
            Entry
            [ PlusEq "c" (Constant $ IntVal 20) ]
            (Goto (Label "test"))
          ,
            Block
            (Label "test")
            (From (Label "init"))
            [ ]
            (If (Eq (Var "c") (Constant $ IntVal 0)) (Label "end") (Label "loop_body"))
          ,
            Block
            (Label "loop_body")
            (From (Label "test"))
            [
              PlusEq "acc" (Constant $ IntVal 3),
              MinusEq "c"  (Constant $ IntVal 1)
            ]
            (Goto (Label "test"))
          ,
            Block
            (Label "end")
            (From (Label "loop_body"))
            [
              PlusEq "acc" (Constant $ IntVal 40),
              Swap "acc" "c",
              Swap "acc" "c"
            ]
            Exit
          ]

    labels = bindLabels ast

    interpAST :: AST -> State VarTab ()
    interpAST (AST _ []) = return ()
    interpAST ast =
      case ast of
        AST _ [] -> return ()
        AST _ (Block l _ insts to:bs) -> do
          interpInsts insts
          case to of
            Exit    -> return ()
            Goto lt -> interpAST $ AST.goto (labelDiff l lt labels) ast
            If ep ltt ltf -> do
              ve <- interpExp exp
              case ve of
                BoolVal True  -> interpAST $ AST.goto (labelDiff l ltt labels) ast
                BoolVal False -> interpAST $ AST.goto (labelDiff l ltf labels) ast
                _             -> error "Type error in if then else"

    interpInsts :: [Instruction] -> State VarTab ()
    interpInsts []     = return ()
    interpInsts (i:is) = do
      interpInst i
      interpInsts is

    interpInst :: Instruction -> State VarTab ()
    interpInst i = case i of
      PlusEq name exp -> do
        lv <- lookupVar name
        ev <- interpExp exp
        case (lv, ev) of --update name (+) $ interpExp exp
          (Just (IntVal n), IntVal m) -> update name (IntVal $ n + m)
          (Nothing, IntVal m)         -> update name (IntVal m)
          _                           -> error "Operands not of correct type"
      MinusEq name exp -> do
        lv <- lookupVar name
        ev <- interpExp exp
        case (lv, ev) of --update name (+) $ interpExp exp
          (Just (IntVal n), IntVal m) -> update name (IntVal $ n - m)
          (Nothing, IntVal m)         -> error "No support for negative numbers"
          _                           -> error "Operands not of correct type"
      Swap name1 name2 -> do
        lv1 <- lookupVar name1
        lv2 <- lookupVar name2
        case (lv1, lv2) of
          (Just v1, Just v2) -> do
            update name1 v2
            update name2 v1
          _                  -> error "Variable is not defined"
      _               -> return ()

    interpExp :: Expression -> State VarTab Value
    interpExp exp = case exp of
      Plus  e1 e2 -> do
        IntVal v1 <- interpExp e1
        IntVal v2 <- interpExp e2
        return $ IntVal $ v1 + v2
      Minus  e1 e2 -> do
        IntVal v1 <- interpExp e1
        IntVal v2 <- interpExp e2
        return $ IntVal $ v1 - v2
      Eq  e1 e2 -> do
        IntVal v1 <- interpExp e1
        IntVal v2 <- interpExp e2
        return $ BoolVal $ v1 == v2
      Lth e1 e2 -> do
        IntVal v1 <- interpExp e1
        IntVal v2 <- interpExp e2
        return $ BoolVal $ v1 < v2
      Gth e1 e2 -> do
        IntVal v1 <- interpExp e1
        IntVal v2 <- interpExp e2
        return $ BoolVal $ v1 > v2
      Constant v  -> return v
      Var name    -> do
        lu <- lookupVar name
        case lu of
          Just v -> return v
          Nothing -> error "hej"
      where applyBinOp binop (IntVal n) (IntVal m) = IntVal  $ binop n m
            applyRelOp relop (IntVal n) (IntVal m) = BoolVal $ relop n m

print $ runState (interpAST ast) []


