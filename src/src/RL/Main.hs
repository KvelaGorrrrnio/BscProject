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

noFile = putStrLn "No .rl file provided."

eout :: Bool -> String -> Error -> IO ()
eout True [] msg = putStrLn $ stringify msg
eout True o  msg = writeFile o $ stringify msg
eout False _ msg = print msg

getAST c = if c then return . parseSrc else parseFile

main = do
  args <- handleArgs "RL" "SRL"
  case args of
    Run [] _ _ _ _ _ -> noFile
    Run f o l j q c  -> let eout' = eout j o in
      (if l then id else optimise . staticcheck) <$> getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> case typecheck ast of
        Left err   -> eout' err
        Right ttab -> case runProgramWith ast (typesToVarTab ttab) of
          (_,log)        | l && j && null o -> unless q $ putStrLn $ logToJSON log
                         | l && j           -> writeFile o $ (++"\n") (logToJSON log)
                         | l && null o      -> unless q $ putStrLn $ logToString log
                         | l                -> writeFile o $ (++"\n") (logToString log)
          (Right vtab,_) | j && null o      -> unless q $ putStrLn $ jsonTabL "variable" vtab
                         | j                -> writeFile o $ (++"\n") (jsonTabL "variable" vtab)
                         | null o           -> unless q $ putStrLn $ showTabL vtab
                         | otherwise        -> writeFile o $ (++"\n") (showTabL vtab)
          (Left err,_)                      -> eout' err
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
    Typeof f o j c -> let eout' = eout j o in
      getAST c f >>= \case
       Left err  -> eout' err
       Right ast -> case typecheck ast of
        Left err   -> eout' err
        Right ttab | j && null o -> putStrLn $ jsonTab "type" ttab
                   | j           -> writeFile o $ (++"\n") (jsonTab "type" ttab)
                   | null o      -> putStrLn $ showTab ttab
                   | otherwise   -> writeFile o $ (++"\n") (showTab ttab)
