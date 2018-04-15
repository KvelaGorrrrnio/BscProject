module Main where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import HandleArgs
import Interp
import Inversion
import Parser

import qualified Data.HashMap.Strict as M

noFile = putStrLn "No .rl file provided."

main = do
  args <- handleArgs
  case args of
    Run l j t q [] -> noFile
    Run l j t q f -> do
      ast <- parseFile f
      let (res,log) = runProgram (getEntry ast) (buildVTab ast) ast
      unless q $ do -- if quiet
        putStrLn $ show ast ++ "\n"
        case res of
          Left  err  -> putStrLn $ "*** Error: " ++ err
          Right vtab -> putStrLn $ "Result:\n"   ++ show vtab
      when l (writeFile (f -<.> "rlog") $ logToString log) -- if log
      when j (writeFile (f -<.> "json") $ logToJSON   log)   -- if jlog
      -- when t $ putStLn ttab -- if types

    Opt o s [] -> noFile
    Opt o s f  ->
      mapM_ (\fi -> do
        ast <- parseFile fi
        let out = replaceFileName fi (takeBaseName fi ++ "_opt.rl")
        -- let ast' = if opt then optimize ast else ast
        (writeFile out . show) ast
      ) f

    Inv o s opt [] -> noFile
    Inv o s opt f  ->
      mapM_ (\fi -> do
        ast <- parseFile fi
        let out = replaceFileName fi (takeBaseName fi ++ "_inv.rl")
        -- let ast' = if opt then optimize ast else ast
        (writeFile out . show . invert) ast
      ) f

    Trl o s opt [] -> noFile
    Trl o s opt f  -> print args
