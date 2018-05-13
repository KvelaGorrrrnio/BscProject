{-# LANGUAGE LambdaCase #-}
module RL.Main (main) where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import RL.Parser
import RL.Translation
import RL.Inversion
import RL.Static
import RL.Interp
import RL.Optimise
import RL.Error

import Common.HandleArgs
import Common.JSON

noFile j o = eout j o $ Custom "No file provided."
noCode j o = eout j o $ Custom "No string provided."

eout :: Bool -> String -> Error -> IO ()
eout True [] msg = putStrLn $ stringify msg
eout True o  msg = writeFile o $ stringify msg
eout False _ msg = print msg

getAST c = if c then return . parseSrc else parseFile

main = do
  args <- handleArgs
  case args of
    Run [] o j c _ -> if c then noCode j o else noFile j o
    Run f o j c l  -> let eout' = eout j o in
      ((if l then id else optimise) . staticcheck <$> getAST c f) >>= \case
        Left err  -> eout' err
        Right (ttab,ast) -> case runProgram ast ttab of
          (_,log)        | l && j && null o -> putStrLn $ logToJSON log
                         | l && j           -> writeFile o $ (++"\n") (logToJSON log)
                         | l && null o      -> putStrLn $ logToString log
                         | l                -> writeFile o $ (++"\n") (logToString log)
          (Right vtab,_) | j && null o      -> putStrLn $ jsonTab "variable" vtab
                         | j                -> writeFile o $ (++"\n") (jsonTab "variable" vtab)
                         | null o           -> putStrLn $ showTab vtab
                         | otherwise        -> writeFile o $ (++"\n") (showTab vtab)
          (Left err,_)                      -> eout' err
    Invert [] o j c -> if c then noCode j o else noFile j o
    Invert f o j c -> let eout' = eout j o in
      (staticcheck <$> getAST c f) >>= \case
        Left err  -> eout' err
        Right (ttab,ast) -> case (showAST ttab . invert) ast of
            code | j && null o -> putStrLn $ jsonCode code
                 | j           -> writeFile o $ (++"\n") (jsonCode code)
                 | null o      -> putStrLn code
                 | otherwise   -> writeFile o $ (++"\n") code
    Translate [] o j c -> if c then noCode j o else noFile j o
    Translate f o j c -> let eout' = eout j o in
      (staticcheck <$> getAST c f) >>= \case
       Left err  -> eout' err
       Right (ttab,ast) | code <- translateToSRLSource ttab ast -> case ast of
         _ | j && null o -> putStrLn $ jsonCode code
           | j           -> writeFile o $ (++"\n") (jsonCode code)
           | null o      -> putStrLn code
           | otherwise   -> writeFile o $ (++"\n") code
    Typeof [] o j c -> if c then noCode j o else noFile j o
    Typeof f o j c -> let eout' = eout j o in
      getAST c f >>= \case
        Left err  -> eout' err
        Right (ttab,_)
          | j && null o -> putStrLn $ jsonTab "type" ttab
          | j           -> writeFile o $ (++"\n") (jsonTab "type" ttab)
          | null o      -> putStrLn $ showTab ttab
          | otherwise   -> writeFile o $ (++"\n") (showTab ttab)
