{-# LANGUAGE LambdaCase #-}
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
import SRL.Error
import Common.JSON

import SRL.AST

noFile = putStrLn "No .srl file provided."

eout :: Bool -> String -> Error -> IO ()
eout True "" msg = putStrLn $ stringify msg
eout True o  msg = writeFile o $ stringify msg
eout False _ msg = print msg

getAST c = if c then (return . parseSrc) else parseFile

main = do
  args <- handleArgs
  case args of
    Run [] _ _ _ _ _ -> noFile
    Run f o l j q c  -> let eout' = eout j o in
      (if l then id else optimise) <$> getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> case typecheck ast of
        Left err   -> eout' err
        Right ttab -> case runProgramWith ast (typesToVarTab ttab) of
          (_,log)        | l && j && null o -> unless q $ putStrLn $ logToJSON log
          (_,log)        | l && j           -> writeFile o $ logToJSON log
          (_,log)        | l && null o      -> unless q $ putStrLn $ logToString log
          (_,log)        | l                -> writeFile o $ logToString log
          (Right vtab,_) | j && null o      -> unless q $ putStrLn $ jsonTabL "variable" vtab
          (Right vtab,_) | j                -> writeFile o $ jsonTabL "variable" vtab
          (Right vtab,_) | null o           -> unless q $ putStrLn $ showTabL vtab
          (Right vtab,_)                    -> writeFile o $ showTabL vtab
          (Left err,_)                      -> eout' err
    Invert f o j c -> let eout' = eout j o in
      getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> do
        let code = (++"\n") . showAST . invert $ ast
        case typecheck ast of
          Left err   -> eout' err
          Right _ | j && null o -> putStrLn $ jsonCode code
          Right _ | j           -> writeFile o $ jsonCode code
          Right _ | null o      -> putStrLn code
          Right _               -> writeFile o code
    Translate f o j c -> let eout' = eout j o in
      getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> do
        let code = (++"\n") . translateToRLSource $ ast
        case typecheck ast of
          Left err   -> eout' err
          Right _ | j && null o -> putStrLn $ jsonCode code
          Right _ | j           -> writeFile o $ jsonCode code
          Right _ | null o      -> putStrLn code
          Right _               -> writeFile o code
    Typeof f o j c -> let eout' = eout j o in
      getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> case typecheck ast of
        Left err   -> eout' err
        Right ttab | j && null o -> putStrLn $ jsonTab "type" ttab
        Right ttab | j           -> writeFile o $ jsonTab "type" ttab
        Right ttab | null o      -> putStrLn $ showTab ttab
        Right ttab               -> writeFile o $ showTab ttab

