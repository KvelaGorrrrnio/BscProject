module Main where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import SRL.HandleArgs
import SRL.Parser
import SRL.Translation
import SRL.Inversion
import SRL.Type
import SRL.Interp

noFile = putStrLn "No .srl file provided."

main = do
  args <- handleArgs
  case args of
    Run _ _ _ _ _ []  -> noFile
    Run l ls j js q f -> do

      -- parse file and run
      ast <- parseFile f
      ttab <- typecheck ast
      putStrLn . showAST $ ast

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
      ttab <- parseFile f >>= \ast -> typecheck ast
      putStrLn $ showTab ttab
