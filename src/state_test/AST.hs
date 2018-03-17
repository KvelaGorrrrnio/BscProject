module AST where

data AST = AST [Block] [Block]
    deriving Show

data Block = Block Label From [Instruction] Goto
    deriving Show

newtype Label = Label String
    deriving Show

data Instruction
    = Swap String String
    | PlusEq String Expression
    | MinusEq String Expression
    | XOREq String Expression
    deriving Show

data Expression
    = Plus Expression Expression
    | Minus Expression Expression
    | Eq Expression Expression
    | Lth Expression Expression
    | Gth Expression Expression
    | Var String
    | Constant Value
    deriving Show

data Value
    = IntVal Int
    | FloatVal Float
    | BoolVal Bool
    | ListVal [Value]
    deriving Show

data From
    = From Label
    | Fi Expression Label Label
    | Entry
    deriving Show

data Goto
    = Goto Label
    | If Expression Label Label
    | Exit
    deriving Show

empty :: () -> AST
empty () = AST [] []

toAST :: [Block] -> AST
toAST = AST []

next :: AST -> AST
next (AST ls (r:rs)) = AST (r:ls) rs

prev :: AST -> AST
prev (AST (l:ls) rs) = AST ls (l:rs)

goto :: Int -> AST -> AST
goto offset ast
    | offset > 0    = iterate next ast !! offset
    | offset < 0    = iterate prev ast !! (-offset)
    | otherwise     = ast

push :: Block -> AST -> AST
push el (AST ls rs) = AST ls $ rs++[el]

printAST :: AST -> IO ()
printAST (AST ls rs) = do
    putStrLn $ "l: " ++ show ls
    putStrLn $ "r: " ++ show rs
    putStrLn "\n"
