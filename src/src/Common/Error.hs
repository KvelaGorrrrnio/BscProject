module Common.Error
( Error        (..)
, RuntimeError (..)
, StaticError  (..)
, convertParseError
, module Common.JSON
) where

import Common.AST
import Common.JSON
import qualified Text.Parsec as Parsec
import Data.List (intercalate)

data Error
  = RuntimeError Pos RuntimeError
  | ParseError   Pos String
  | StaticError  Pos StaticError
  | Custom       String

data RuntimeError
  = NonDefinedId String
  | IndexOnNonList String
  | IndexOnNonListExp Value
  | NonIntegerIndex Value
  | SelfAbuse Id
  | NegativeIndex Integer
  | IndexOutOfBounds Integer
  | PushToNonList Id
  | PopToNonEmpty Id
  | PopFromEmpty Id
  | PopFromNonList Id
  | ConflictingType Type Type
  | ConflictingTypes [Type] [Type]
  | EmptyTop
  | NonListExp Type
  | NonIntegerExp Type
  | DivByZero
  | DivHasRest
  | MultByZero
  | UpdateOnNonIntager Id Type
  | InitOnNonList String
  | InitNonEmptyList String
  | ConflictingDimensions
  | NegativeDimension Integer
  | NonIntegerDimension Type
  | FreeOnNonList String
  | FreeNonEmptyList String
  -- RL specific runtime errors
  | AssertionFailed Exp Value Value
  | FromFail String String

data StaticError
  = DuplicateLabel String
  | DuplicateEntry
  | DuplicateExit
  | NotDefinedLabel String
  | EntryNotStart
  | ExitNotEnd
  | NoEntry
  | NoExit

instance Show Error where
  show (ParseError (l,c) e)   = e
  show (RuntimeError (l,c) e) = "A runtime error occurred at (line "++show l++", column "++show c++"):\n" ++ show e
  show (StaticError (l,c) e)  = "An error occurred at (line "++show l++", column "++show c++"): " ++ show e
  show (Custom e)             = e
instance JSON Error where
  stringify (ParseError p e)       = jsonError e p
  stringify (RuntimeError p e)     = jsonError (show $ RuntimeError p e) p
  stringify (StaticError p e)      = jsonError (show e) p
  stringify (Custom e)             = jsonError e (0,0)

instance Show RuntimeError where
  show (NonDefinedId id) = id ++ " is not defined."
  show (IndexOnNonList id) = "Tried indexing on non-list identifier: " ++ id
  show (IndexOnNonListExp v) = "Tried indexing on non-list value:" ++ show v
  show (NonIntegerIndex v) = "Tried indexing with non-list value: " ++ show v
  show (SelfAbuse idx) = show idx ++ " references itself."
  show (NegativeIndex n) = "Tried indexing with negative index: " ++ show n
  show (IndexOutOfBounds n) = "Indexing with " ++ show n ++ " results in an out of bounds."
  show (PushToNonList idx) = "Tried pushing to non-list identifier: " ++ show idx
  show (PopToNonEmpty idx) = "Tried popping to non-empty identifier: " ++ show idx
  show (PopFromEmpty idx) = "Tried popping from empty identifier: " ++ show idx
  show (PopFromNonList idx) = "Tried popping from non-list identifier: " ++ show idx
  show (ConflictingType t1 t2) = "Expected " ++ show t1 ++ " as type, but got " ++ show t2
  show (ConflictingTypes tl1 tl2) = "Expected " ++ intercalate " -> " (map show tl1) ++ " as type, but got " ++ intercalate " -> " (map show tl2)
  show (EmptyTop) = "Tried reading top of empty list."
  show (NonListExp t) = "Expected list from expression, but received " ++ show t
  show (NonIntegerExp t) = "Expected " ++ show IntT ++ " from expression, but received " ++ show t
  show (DivByZero) = "Division by zero."
  show (DivHasRest) = "Division has rest."
  show (MultByZero) = "Multiplication update by zero."
  show (UpdateOnNonIntager idx t) = "Tried updating non-" ++ show IntT ++ " identifier " ++ show idx ++ " of type " ++ show t
  show (InitOnNonList id) = "Tried initializing non-list identifier " ++ id
  show (InitNonEmptyList id) = "Tried initliazing non-empty list identifier " ++ id
  show (ConflictingDimensions) = "The number of dimensions specified does not match depth of list type."
  show (NegativeDimension n) = "Encountered negative dimension: " ++ show n
  show (NonIntegerDimension t) = "Expected dimension size to be of type " ++ show IntT ++ ", received " ++ show t
  show (FreeOnNonList id) = "Tried freeing non-list identifier: " ++ id
  show (FreeNonEmptyList id) = "Tried freeing non-empty list identifier: " ++ id
  show (AssertionFailed exp e r) = "Assertion " ++ show exp ++ " expected " ++ show e ++ ", but received " ++ show r
  show (FromFail f e) = "From-clause not consistent.\n Coming from " ++ f ++ ", but expected " ++ e ++ "."

instance Show StaticError where
  show (DuplicateLabel l)  = "Label '" ++ l ++ "' has been defined multiple times."
  show (NotDefinedLabel l) = "Label '" ++ l ++ "' has not been defined."
  show DuplicateEntry      = "Only one entry-point is allowed."
  show DuplicateExit       = "Only one exit-point is allowed."
  show EntryNotStart       = "The entry must be at the beginning of the program."
  show ExitNotEnd          = "The exit must be at the end of the program."
  show NoEntry             = "No entry-point is specified."
  show NoExit              = "No exit-point is specified."

-- =======
-- Helpers
-- =======
convertParseError :: Parsec.ParseError -> Error
convertParseError e = ParseError ((pos . Parsec.errorPos) e) (show e)
  where pos s = (Parsec.sourceLine s, Parsec.sourceColumn s)
