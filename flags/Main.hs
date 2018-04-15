{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Prelude hiding (log)
import System.Console.CmdArgs

data RevL = RevL
  {log   :: Bool
  ,optim :: Bool
  ,files :: [FilePath]
  }
  deriving (Data,Typeable,Show,Eq)

hlint = RevL
  {log   = def &= help "Log the program execution"
  ,optim = def &= help "Optimize the program"
  ,files = def &= args &= typ "FILES"
  } &=
  summary "RevL Interpreter, (C) Anders Jørgensen & Lars Vadgaard"

mode = cmdArgsMode hlint
main = print =<< cmdArgsRun mode
