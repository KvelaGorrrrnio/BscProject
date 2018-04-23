module Main where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import SRL.HandleArgs
import SRL.Parser
import SRL.Translation
import SRL.Inversion
import SRL.Type
import SRL.Interp
import SRL.Optimise

import SRL.AST

noFile = putStrLn "No .srl file provided."

main = do
  args <- handleArgs
  case args of
    Run _ _ _ _ _ []  -> noFile
    Run l ls j js q f -> do

      -- parse file and run
      ast  <- (if l || ls || j || js then id else optimise) <$> parseFile f
      ttab <- typecheck ast
      let (res,log) = runProgramWith ast (typesToVarTab ttab)

      -- if -q is not set
      unless (q || ls) $ case res of
        Left  err  -> putStrLn $ "*** Error: " ++ show err
        Right vtab -> putStrLn $ showVTab vtab

      -- if -l flag is set
      when l $ do
        let logname = f -<.> "srlog"
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
      ttab <- typecheck ast
      let out = if null o
                then replaceFileName f (takeBaseName f ++ "_inv.srl")
                else o
      writeFile out . (++"\n") . showAST . invert $ ast

    Translate _ [] -> noFile
    Translate o f  -> do
      ast <- parseFile f
      ttab <- typecheck ast
      let out = if null o
                then f -<.> "rl"
                else o
      writeFile out . (++"\n") . translateToRLSource $ ast

    Typeof f -> do
      ttab <- typecheck =<< parseFile f
      putStrLn $ showTab ttab
