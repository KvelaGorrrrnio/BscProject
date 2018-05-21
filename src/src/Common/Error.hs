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

data Error
  = RuntimeError Pos RuntimeError
  | ParseError   Pos String
  | StaticError  Pos StaticError
  | Custom       String

data RuntimeError
  = CustomRT String

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
  show (CustomRT s) = s

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
