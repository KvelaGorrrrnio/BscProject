module RL.AST
(
) where

import qualified Data.HashMap.Strict as M

newtype AST = AST (M.HashMap Label Block)
type Label  = String
type Id     = String
type Pos    = (Int, Int)

newtype Block = Block (From, [Stmt], Goto)

data From
  = From  Label
  | Fi    Exp Label Label
  | Entry
  deriving Show

data Goto
  = Goto  Label
  | If    Exp Label Label
  | Exit
  deriving Show

data Stmt
  = Update Id UptOp Exp
  | Push   Id Id
  | Pop    Id Id
  | Swap   Id Id
  | Skip
  deriving Show

data UptOp
  = PlusEq
  | MinusEq
  | XorEq
  | TimesEq
  | DivideEq
  deriving Show

data Exp
  = Var    Id
  | Const  Value
  | BinOp  BinOp Exp Exp
  | UnOp   UnOp Exp
  | Top    Id
  | Empty  Id
  deriving Show

data BinOp
  = Plus
  | Minus
  | Xor
  | Times
  | Equal
  | Less
  | Greater
  | And
  | Or
  deriving Show

data UnOp
  = Not
  | Negate
  deriving Show

data Value
  = IntV   Integer
  | StackV [Value]
  deriving Show

