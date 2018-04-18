{-# LANGUAGE DeriveDataTypeable #-}

module HandleArgs where
import System.Console.CmdArgs
import Prelude hiding (log)

data RevL
  = Trl    {out :: FilePath, file :: FilePath}
  | Inv    {out :: FilePath, file :: FilePath}
  | Typeof {file :: FilePath}
  | Run    {log   :: Bool, logstd  :: Bool
           ,json  :: Bool, jsonstd :: Bool
           ,quiet :: Bool, file    :: FilePath}
  deriving (Data,Typeable,Show,Eq)

helpOutput = help "Output file"

translate = Trl
  {out    = def &= typFile &= helpOutput
  ,file   = def &= args &= typFile
  } &= help "Translate a RevL program to its SRevL counterpart"

invert_ = Inv
  {out    = def &= typFile &= helpOutput
  ,file   = def &= args &= typFile
  } &= help "Invert a RevL program"

typeof = Typeof
  { file   = def &= args &= typFile
  } &= help "Print the inferred types of the program"

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

mode = cmdArgsMode $ modes [interpret, typeof, invert_, translate]
  &= help    "Interpret, optimize or translate a RevL program"
  &= summary "The Glorious RevL Interpreter System, version 1.0.0"

handleArgs :: IO RevL
handleArgs = cmdArgsRun mode
