module Common.Error
( RuntimeError (..)
, TypeError    (..)
) where

import Common.AST

data RuntimeError
  = CustomRT String
data TypeError
  = IncompatibleTypes Type Type Pos
  | BinOpTypes BinOp (Type,Type) (Type,Type) Pos
  | UnOpType   UnOp Type Type Pos

runtimebase (l,c) = "A runtime error occured at (line "++show l++", column "++show c++"): "
instance Show RuntimeError where
  show (CustomRT s) = s

typebase (l,c) = "A type error occured at (line "++show l++", column "++show c++"): "
instance Show TypeError where
  show (IncompatibleTypes t t' p)          = typebase p++"Couldn't find common type for "++show t++" and "++show t'++"."
  show (BinOpTypes op (lt,rt) (lt',rt') p) = typebase p++show op++" expected input of "++show lt++" * "++show rt++", but recieved "++show lt'++" * "++show rt'++"."
  show (UnOpType op t t' p)                = typebase p++show op++" expected input of "++show t++", but recieved "++show t'++"."
