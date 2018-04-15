module Main where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceBaseName)

import Control.Monad (when, unless)

import HandleArgs
import Interp
import Inversion

noFile = putStrLn "No .rl file provided."

main = do
  args <- handleArgs
  case args of
    Run l j t q [] -> noFile
    Run l j t q f | (res,log) <- runProgram "init" vtab ast -> do
      unless q $ do -- if quiet
        putStrLn $ show ast ++ "\n"
        case res of
          Left  err  -> putStrLn $ "*** Error: " ++ err
          Right vtab -> putStrLn $ "Result:\n"   ++ show vtab
      when l (writeFile (f -<.> "rlog") $ logToString log) -- if log
      when j (writeFile (f -<.> "json") $ logToJSON   log)   -- if jlog
      -- when t $ putStLn ttab -- if types

    Opt o s []     -> noFile
    Opt o s f      -> print args

    Inv o s opt [] -> noFile
    Inv o s opt f  ->
      if length f > 1 then return ()
      else do
        let out = if null o then ((++"_inv.rl") . takeBaseName) (head f) else o
        -- let ast' = if opt then optimize ast else ast
        (writeFile out . show . invert) ast'

    Trl o s opt [] -> noFile
    Trl o s opt f  -> print args
