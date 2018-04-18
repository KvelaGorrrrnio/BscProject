module Main where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import SRL.HandleArgs
import SRL.Parser
import SRL.Translation
import SRL.Inversion

noFile = putStrLn "No .srl file provided."

main = do
  args <- handleArgs
  case args of
    Run _ _ _ _ _ []  -> noFile
    Run l ls j js q f -> do

      -- parse file and run
      ast <- parseFile f
      putStrLn . show $ ast

    Invert _ [] -> noFile
    Invert o f  -> do
      ast <- parseFile f
      let out = if null o
                then replaceFileName f (takeBaseName f ++ "_inv.srl")
                else o
      writeFile out . (++"\n") . show . invert $ ast

    Translate _ [] -> noFile
    Translate o f  -> do
      ast <- parseFile f
      let out = if null o
                then f -<.> "rl"
                else o
      writeFile out . (++"\n") . translateToRLSource $ ast

    Typeof f -> print args
