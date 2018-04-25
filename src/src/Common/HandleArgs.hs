{-# LANGUAGE DeriveDataTypeable #-}
module Common.HandleArgs where

import Data.Char
import System.Console.CmdArgs
import Prelude hiding (log)

data Prog
  = Typeof    { file  :: FilePath, out  :: FilePath
              , json  :: Bool,     code :: Bool }
  | Translate { file  :: FilePath, out  :: FilePath
              , json  :: Bool,     code :: Bool }
  | Invert    { file  :: FilePath, out  :: FilePath
              , json  :: Bool,     code :: Bool }
  | Run       { file  :: FilePath, out  :: FilePath
              , log   :: Bool,     json :: Bool
              , quiet :: Bool,     code :: Bool }
  deriving (Data,Typeable,Show,Eq)

helpOutput    = help "Write the output to the specified file"
helpCode lang = help $ "String argument treated as " ++ lang ++ " code"
helpJSON      = help "Format outpus as JSON"

typeof lang = Typeof
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  , json   = def &= helpJSON
  , code   = def &= helpCode lang
  } &= help "Print the inferred types of the program"

translate lang clang = Translate
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  , json   = def &= helpJSON
  , code   = def &= helpCode lang
  } &= help ("Translate a " ++ lang ++ " program to its " ++ clang ++ " counterpart")

invert_ lang = Invert
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  , json   = def &= helpJSON
  , code   = def &= helpCode lang
  } &= help ("Invert a " ++ lang ++ " program")

interpret lang = Run
  { file    = def &= args &= typFile
  , log     = def &= help "Output log instead of final state"
  , json    = def &= helpJSON
  , out     = def &= typFile &= helpOutput
  , quiet   = def &= help "Hide the result of the program"
  , code    = def &= helpCode lang
  } &= help ("Interpret a " ++ lang ++ " program") &= auto

mode lang clang = cmdArgsMode $ modes [interpret lang, invert_ lang, translate lang clang, typeof lang]
  &= help    ("Interpret, invert or translate a " ++ lang ++ " program")
  &= summary ("The Glorious " ++ lang ++ " Interpreter System, version 1.0.0")
  &= program (map toLower lang)

handleArgs :: String -> String -> IO Prog
handleArgs lang clang = cmdArgsRun $ mode lang clang
