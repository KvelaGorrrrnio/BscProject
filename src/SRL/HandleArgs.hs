{-# LANGUAGE DeriveDataTypeable #-}
module SRL.HandleArgs where

import System.Console.CmdArgs
import Prelude hiding (log)

data SRevL
  = Typeof    { file  :: FilePath, out  :: FilePath
              , json  :: Bool }
  | Translate { file  :: FilePath, out :: FilePath
              , json  :: Bool }
  | Invert    { file  :: FilePath, out :: FilePath
              , json  :: Bool }
  | Run       { file  :: FilePath, out   :: FilePath
              , log   :: Bool,     json  :: Bool
              , quiet :: Bool }
  deriving (Data,Typeable,Show,Eq)

helpOutput = help "Output file"

typeof = Typeof
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  ,json    = def &= help "Format output as JSON."
  } &= help "Print the inferred types of the program."

translate = Translate
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  ,json    = def &= help "Format output as JSON."
  } &= help "Translate a SRevL program to its RevL counterpart."

invert_ = Invert
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  ,json    = def &= help "Format output as JSON."
  } &= help "Invert a SRevL program"

interpret = Run
  {log     = def &= help "Output is program interpretation log instead of final state."
  ,json    = def &= help "Format output as JSON."
  ,out     = def &= typFile &= helpOutput
  ,quiet   = def &= help "Hide the result of the program."
  ,file    = def &= args &= typFile
  } &= help "Interpret a SRevL program" &= auto

mode = cmdArgsMode $ modes [interpret, invert_, translate, typeof]
  &= help    "Interpret, invert or translate a SRevL program"
  &= summary "The Glorious SRevL Interpreter System, version 1.0.0"

handleArgs :: IO SRevL
handleArgs = cmdArgsRun mode
