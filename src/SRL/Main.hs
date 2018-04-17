module Main where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import HandleArgs
import Parser
import Translation

noFile = putStrLn "No .rl file provided."

main = do
  args <- handleArgs
  case args of
    Run {}  -> print args

    Inv _ []  -> print args
    Inv o f -> print args

    Trl _ [] -> noFile
    Trl o f  -> do
      ast <- parseFile f
      let out = if null o
                then f -<.> "rl"
                else o
      writeFile out . (++"\n") . show . translate $ ast
