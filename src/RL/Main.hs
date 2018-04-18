module Main where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import HandleArgs
import Interp
import Inversion
import Parser

noFile = putStrLn "No .rl file provided."

main = do
  args <- handleArgs
  case args of
    Run _ _ _ _ _ []  -> noFile
    Run l ls j js q f -> do

      -- parse file and run
      ast <- parseFile f

      let (res,log) = runProgram ast

      -- if -q is not set
      unless (q || ls) $ case res of
        Left  err  -> putStrLn $ "*** Error: " ++ err
        Right vtab -> putStrLn $ showVTab vtab

      -- if -l flag is set
      when l $ do
        let logname = f -<.> "rlog"
        writeFile logname . (++"\n") . logToString $ log
        unless q $ putStrLn ("\nThe log was written to " ++ logname)

      -- if --log-stdout flag is set
      when ls $ putStrLn (logToString log)

      -- if -j flag is set
      when j $ do
        let logname = f -<.> "json"
        writeFile logname . (++"\n") . logToJSON $ log
        unless q $ putStrLn ("\nThe log was written to " ++ logname)

      -- if --json-stdout flag is set
      when js $ putStrLn (logToJSON log)

    Invert _ [] -> noFile
    Invert o f  -> do
      ast <- parseFile f
      let out = if null o
                then replaceFileName f (takeBaseName f ++ "_inv.rl")
                else o
      writeFile out . (++"\n") . showAST . invert $ ast

    Translate o [] -> noFile
    Translate o f  -> print args
