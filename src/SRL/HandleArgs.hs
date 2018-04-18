{-# LANGUAGE DeriveDataTypeable #-}
module SRL.HandleArgs where

import System.Console.CmdArgs
import Prelude hiding (log)

data RevL
  = Typeof    {file :: FilePath}
  | Translate {out :: FilePath, file :: FilePath}
  | Invert    {out :: FilePath, file :: FilePath}
  | Run       {log   :: Bool, logstd  :: Bool
              ,json  :: Bool, jsonstd :: Bool
              ,quiet :: Bool, file    :: FilePath}
  deriving (Data,Typeable,Show,Eq)

helpOutput = help "Output file"

typeof = Typeof
  { file   = def &= args &= typFile
  } &= help "Print the inferred types of the program"

translate = Translate
  {out    = def &= typFile &= helpOutput
  ,file   = def &= args &= typFile
  } &= help "Translate a RevL program to its SRevL counterpart"

invert_ = Invert
  {out    = def &= typFile &= helpOutput
  ,file   = def &= args &= typFile
  } &= help "Invert a RevL program"

interpret = Run
  {log     = def &= help "Log the program execution to an RLog file"
  ,logstd  = def &= explicit &= name "log-stdout"
                 &= help "Log the program execution to stdout in RLog format."
  ,json    = def &= help "Log the program execution to a JSON file"
  ,jsonstd = def &= explicit &= name "json-stdout"
                 &= help "Log the program execution to stdout in JSON format."
  ,quiet   = def &= help "Hide the result of the program"
  ,file    = def &= args &= typFile
  } &= help "Interpret a RevL program" &= auto

mode = cmdArgsMode $ modes [interpret, invert_, translate, typeof]
  &= help    "Interpret, invert or translate a RevL program"
  &= summary "The Glorious RevL Interpreter System, version 1.0.0"

handleArgs :: IO RevL
handleArgs = cmdArgsRun mode
