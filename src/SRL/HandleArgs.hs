{-# LANGUAGE DeriveDataTypeable #-}

module HandleArgs where
import System.Console.CmdArgs
import Prelude hiding (log)

data SRevL
  = Trl {out :: FilePath, file :: FilePath}
  | Inv {out :: FilePath, file :: FilePath}
  | Run {log   :: Bool, logstd  :: Bool
        ,json  :: Bool, jsonstd :: Bool
        ,types :: Bool, quiet   :: Bool, file :: FilePath}
  deriving (Data,Typeable,Show,Eq)

helpOutput = help "Output file"

translate_ = Trl
  {out    = def &= typFile &= helpOutput
  ,file   = def &= args &= typFile
  } &= help "Translate a SRevL program to its RevL counterpart"

invert_ = Inv
  {out    = def &= typFile &= helpOutput
  ,file   = def &= args &= typFile
  } &= help "Invert a SRevL program"

interpret = Run
  {log     = def &= help "Log the program execution to an SRLog file"
  ,logstd  = def &= explicit &= name "log-stdout"
                 &= help "Log the program execution to stdout in RLog format."
  ,json    = def &= help "Log the program execution to a JSON file"
  ,jsonstd = def &= explicit &= name "json-stdout"
                 &= help "Log the program execution to stdout in JSON format."
  ,types   = def &= help "Output the types to stdout"
  ,quiet   = def &= help "Hide the result of the program"
  ,file    = def &= args &= typFile
  } &= help "Interpret a SRevL program" &= auto

mode = cmdArgsMode $ modes [interpret, invert_, translate_]
  &= help    "Interpret, optimize or translate a SRevL program"
  &= summary "The Glorious SRevL Interpreter System, version 1.0.0"

handleArgs :: IO SRevL
handleArgs = cmdArgsRun mode
