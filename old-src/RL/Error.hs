module RL.Error where

type Pos = (Int, Int)

data RLError
  = RuntimeError RuntimeError
  | TypeError TypeError
  deriving Show

data TypeError
  = StackOnScalar Pos
  deriving Show

data RuntimeError
  = DivByZero Pos
  deriving Show

--instance Show RLError where
