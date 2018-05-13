module Common.Error
( Error        (..)
, RuntimeError (..)
, TypeError    (..)
, StaticError  (..)
, convertParseError
, module Common.JSON
) where

import Common.AST
import Common.JSON
import qualified Text.Parsec as Parsec

data Error
  = RuntimeError Pos RuntimeError
  | ParseError   Pos String
  | TypeError    Pos TypeError
  | StaticError  Pos StaticError
  | Custom       String

data RuntimeError
  = CustomRT String

data TypeError
  = IncompatibleTypes Type Type
  | BinOpTypes BinOp (Type,Type) (Type,Type)
  | UnOpType   UnOp Type Type
  | NonIntegerExp   Exp  Type
  | PushToNonList   Id   Type
  | PopFromNonList  Id   Type

data StaticError
  = SelfAbuse Id
  | DuplicateLabel String Pos
  | DuplicateEntry
  | DuplicateExit
  | NotDefinedLabel String
  | NoEntry
  | NoExit

instance Show Error where
  show (ParseError (l,c) e)   = e
  show (RuntimeError (l,c) e) = "A runtime error occurred at (line "++show l++", column "++show c++"):\n" ++ show e
  show (TypeError (l,c) e)    = "A type error occurred at (line "++show l++", column "++show c++"): " ++ show e
  show (StaticError (l,c) e)  = "An error occurred at (line "++show l++", column "++show c++"): " ++ show e
  show (Custom e)             = e
instance JSON Error where
  stringify (ParseError p e)       = jsonError e p
  stringify (RuntimeError p e)     = jsonError (show $ RuntimeError p e) p
  stringify (TypeError p e)        = jsonError (show $ TypeError p e) p
  stringify (StaticError p e)      = jsonError (show e) p
  stringify (Custom e)             = jsonError e (0,0)

instance Show RuntimeError where
  show (CustomRT s) = s

instance Show StaticError where
  show (SelfAbuse id) = show id ++ " is abusing itself."
  show (DuplicateLabel lbl (l,_)) = " Can't redefine " ++ lbl ++ " again. First defined at line " ++ show l ++ "."
  show (NotDefinedLabel lbl) = "Label " ++ lbl ++ " is not defined."
  show DuplicateEntry        = "Only one entry-point is allowed."
  show DuplicateExit         = "Only one exit-point is allowed."
  show NoEntry               = "No entry-point is specified."
  show NoExit                = "No exit-point is specified."

instance Show TypeError where
  show (IncompatibleTypes t t')          = "Couldn't find common type for "++show t++" and "++show t'++"."
  show (BinOpTypes op (lt,rt) (lt',rt')) = show op++" expected input of "++show lt++" * "++show rt++", but recieved "++show lt'++" * "++show rt'++"."
  show (UnOpType op t t')                = show op++" expected input of "++show t++", but recieved "++show t'++"."
  show (NonIntegerExp exp t)             = show exp ++ " is of type " ++ show t ++ " instead of " ++ show IntT ++ "."
  show (PushToNonList id t)              = "Tried pushing to non-list identifier " ++ show id ++ " of type " ++ show t ++ "."
  show (PopFromNonList id t)             = "Tried popping from non-list identifier " ++ show id ++ " of type " ++ show t ++ "."

-- =======
-- Helpers
-- =======
convertParseError :: Parsec.ParseError -> Error
convertParseError e = ParseError ((pos . Parsec.errorPos) e) (show e)
  where pos s = (Parsec.sourceLine s, Parsec.sourceColumn s)
