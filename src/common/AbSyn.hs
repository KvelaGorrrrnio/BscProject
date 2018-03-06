module ComAbSyn
( ComAbSyn(..)
) where


type Var = String

data Step = Assign Var Maybe Expr Operator Expr
          deriving Show

data Expr = Value Value
          | Variable Var
          | Array Var Expr
          deriving Show

data Value =  IntValue Int
            deriving show

data Operator = Plus
              deriving Show
