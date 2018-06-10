{-# LANGUAGE DeriveDataTypeable #-}
module Common.HandleArgs where

import Data.Char
import System.Console.CmdArgs
import Prelude hiding (log)

data Prog
  = Translate { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool }
  | Invert    { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool }
  | Run       { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool
              , log   :: Bool                    }
  | Blocks    { file  :: FilePath, out   :: FilePath
              , json  :: Bool,     code  :: Bool }
  deriving (Data,Typeable,Show,Eq)

helpOutput = help "Write the output to the specified file"
helpCode   = help "Give a string to be treated as [S]RL code"
helpJSON   = help "Format the output as JSON"

blocks = record Blocks{} [
    file  := def += args += typFile
  , out   := def         += typFile  += helpOutput
  , json  := def                     += helpJSON
  , code  := def                     += helpCode
  ] += help "Print the number of blocks in the program"

translate_ = record Translate{} [
    file  := def += args += typFile
  , out   := def         += typFile  += helpOutput
  , json  := def                     += helpJSON
  , code  := def                     += helpCode
  ] += help "Translate a program in one language to a program in the other language"

invert_   = record Invert{} [
    file  := def += args += typFile
  , out   := def         += typFile  += helpOutput
  , json  := def                     += helpJSON
  , code  := def                     += helpCode
  ] += help "Invert an [S]RL program"

interpret = record Run{} [
    file  := def += args += typFile
  , out   := def         += typFile  += helpOutput
  , json  := def                     += helpJSON
  , code  := def                     += helpCode
  , log   := def += help "Output log instead of final state"
  ] += help "Interpret an [S]RL program" += auto

mode = cmdArgsMode_ $ modes_ [interpret, invert_, translate_, blocks]
  += help    "Interpret, invert or translate an [S]RL program"
  += summary "The Glorious [S]RL Interpreter System, version 1.0.0"
  += program "[s]rl"
  += helpArg    [explicit, name "help", name "h"]
  += versionArg [explicit, name "version", name "v"]

handleArgs :: IO Prog
handleArgs = cmdArgsRun mode
