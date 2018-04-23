{-# LANGUAGE DeriveDataTypeable #-}
module RL.HandleArgs where

import System.Console.CmdArgs
import Prelude hiding (log)

data RevL
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

helpOutput = help "Write the output to the specified file"

typeof = Typeof
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  ,json    = def &= help "Format output as JSON"
  } &= help "Print the inferred types of the program"

translate = Translate
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  ,json    = def &= help "Format output as JSON"
  } &= help "Translate a RevL program to its SRevL counterpart"

invert_ = Invert
  { file   = def &= args &= typFile
  , out    = def &= typFile &= helpOutput
  ,json    = def &= help "Format output as JSON"
  } &= help "Invert a RevL program"

interpret = Run
  {log     = def &= help "Output log instead of final state"
  ,json    = def &= help "Format output as JSON"
  ,out     = def &= typFile &= helpOutput
  ,quiet   = def &= help "Hide the result of the program"
  ,file    = def &= args &= typFile
  } &= help "Interpret a RevL program" &= auto

mode = cmdArgsMode $ modes [interpret, invert_, translate, typeof]
  &= help    "Interpret, invert or translate a RevL program"
  &= summary "The Glorious RevL Interpreter System, version 1.0.0"

handleArgs :: IO RevL
handleArgs = cmdArgsRun mode
