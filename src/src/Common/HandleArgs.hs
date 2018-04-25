{-# LANGUAGE DeriveDataTypeable #-}
module Common.HandleArgs where

import Data.Char
import System.Console.CmdArgs
import Prelude hiding (log)

data Prog
  = Typeof    { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool }
  | Translate { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool }
  | Invert    { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool }
  | Run       { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool
              , log   :: Bool,     quiet :: Bool}
  deriving (Data,Typeable,Show,Eq)

helpOutput = help "Write the output to the specified file"
helpCode   = help "String argument treated as (S)RL code"
helpJSON   = help "Formats output as JSON"

typeof    = Typeof {
    file  = def &= args &= typFile
  , out   = def         &= typFile  &= helpOutput
  , json  = def                     &= helpJSON
  , code  = def                     &= helpCode
  } &= help "Print the inferred types of the program"

translate = Translate {
    file  = def &= args &= typFile
  , out   = def         &= typFile  &= helpOutput
  , json  = def                     &= helpJSON
  , code  = def                     &= helpCode
  } &= help "Translate an (S)RL program to its (S)RL counterpart"

invert_   = Invert {
    file  = def &= args &= typFile
  , out   = def         &= typFile  &= helpOutput
  , json  = def                     &= helpJSON
  , code  = def                     &= helpCode
  } &= help "Invert an (S)RL program"

interpret = Run {
    file  = def &= args &= typFile
  , out   = def         &= typFile  &= helpOutput
  , json  = def                     &= helpJSON
  , code  = def                     &= helpCode
  , log   = def &= help "Output log instead of final state"
  , quiet = def &= help "Hide the result of the program"
  } &= help "Interpret an (S)RL program" &= auto

mode = cmdArgsMode $ modes [interpret, invert_, translate, typeof]
  &= help    "Interpret, invert or translate a (S)RL program"
  &= summary "The Glorious (S)RL Interpreter System, version 1.0.0"
  &= program (map toLower "(s)rl")
  &= helpArg [explicit, name "help", name "h"]

handleArgs :: IO Prog
handleArgs = cmdArgsRun mode
