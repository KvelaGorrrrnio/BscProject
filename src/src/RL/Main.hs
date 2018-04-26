{-# LANGUAGE LambdaCase #-}
module RL.Main (main) where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)

import Control.Monad (when, unless)

import RL.Parser
import RL.Translation
import RL.Inversion
import RL.Type
import RL.Static
import RL.Interp
import RL.Optimise
import RL.Error

import Common.HandleArgs
import Common.JSON

noFile = putStrLn "No file provided."
noCode = putStrLn "No string provided."

eout :: Bool -> String -> Error -> IO ()
eout True [] msg = putStrLn $ stringify msg
eout True o  msg = writeFile o $ stringify msg
eout False _ msg = print msg

getAST c = if c then return . parseSrc else parseFile

main = do
  args <- handleArgs
  case args of
    Run [] _ _ c _ -> if c then noCode else noFile
    Run f o j c l  -> let eout' = eout j o in
      (if l then id else optimise . staticcheck) <$> getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> case typecheck ast of
        Left err   -> eout' err
        Right ttab -> case runProgramWith ast (typesToVarTab ttab) of
          (_,log)        | l && j && null o -> putStrLn $ logToJSON log
                         | l && j           -> writeFile o $ (++"\n") (logToJSON log)
                         | l && null o      -> putStrLn $ logToString log
                         | l                -> writeFile o $ (++"\n") (logToString log)
          (Right vtab,_) | j && null o      -> putStrLn $ jsonTabL "variable" vtab
                         | j                -> writeFile o $ (++"\n") (jsonTabL "variable" vtab)
                         | null o           -> putStrLn $ showVTab vtab
                         | otherwise        -> writeFile o $ (++"\n") (showVTab vtab)
          (Left err,_)                      -> eout' err
    Invert [] _ _ c -> if c then noCode else noFile
    Invert f o j c -> let eout' = eout j o in
      getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> do
        let code = showAST . invert $ ast
        case typecheck ast of
          Left err   -> eout' err
          Right _ | j && null o -> putStrLn $ jsonCode code
                  | j           -> writeFile o $ (++"\n") (jsonCode code)
                  | null o      -> putStrLn code
                  | otherwise   -> writeFile o $ (++"\n") code
    Translate [] _ _ c -> if c then noCode else noFile
    Translate f o j c -> let eout' = eout j o in
      getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> do
        let code = translateToSRLSource ast
        case typecheck ast of
          Left err   -> eout' err
          Right _ | j && null o -> putStrLn $ jsonCode code
                  | j           -> writeFile o $ (++"\n") (jsonCode code)
                  | null o      -> putStrLn code
                  | otherwise   -> writeFile o $ (++"\n") code
    Typeof [] _ _ c -> if c then noCode else noFile
    Typeof f o j c -> let eout' = eout j o in
      getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> case typecheck ast of
        Left err   -> eout' err
        Right ttab | j && null o -> putStrLn $ jsonTab "type" ttab
                   | j           -> writeFile o $ (++"\n") (jsonTab "type" ttab)
                   | null o      -> putStrLn $ showTab ttab
                   | otherwise   -> writeFile o $ (++"\n") (showTab ttab)
