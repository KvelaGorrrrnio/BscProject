{-# LANGUAGE LambdaCase #-}
module SRL.Main (main) where

import System.FilePath.Posix ((-<.>), takeBaseName, replaceFileName)
import System.Directory (doesFileExist)

import Control.Monad (when, unless)

import SRL.Parser
import SRL.Translation
import SRL.Inversion
import SRL.Interp

import Common.Log
import Common.HandleArgs
import Common.JSON

noFile j o        = eout j o $ Custom "No file provided."
noCode j o        = eout j o $ Custom "No string provided."
noFileExist j o f = eout j o $ Custom $ "'" ++ f ++ "' does not exist."

eout :: Bool -> String -> Error -> IO ()
eout True [] msg = putStrLn $ stringify msg
eout True o  msg = writeFile o $ stringify msg
eout False _ msg = print msg

getAST c = if c then return . parseSrc else parseFile

main = do
  args <- handleArgs
  case args of
    Run [] o j c _ -> if c then noCode j o else noFile j o
    Run f o j c l  -> do
      unless c $ do
        exists <- doesFileExist f
        unless exists $ noFileExist j o f

      let eout' = eout j o
      getAST c f >>= \case
        Left err  -> eout' err
        Right (ttab,ast) -> case hasDupDec ttab of
          Nothing -> case runProgram ast ttab of
            (_,log)        | l && j && null o -> putStrLn $ stringify log
                           | l && j           -> writeFile o $ (++"\n") (stringify log)
                           | l && null o      -> print log
                           | l                -> writeFile o $ (++"\n") (show log)
            (Right vtab,_) | j && null o      -> putStrLn $ jsonTab "variable" vtab
                           | j                -> writeFile o $ (++"\n") (jsonTab "variable" vtab)
                           | null o           -> putStrLn $ showTab vtab
                           | otherwise        -> writeFile o $ (++"\n") (showTab vtab)
            (Left err,_)                      -> eout' err
          Just n  -> eout' (StaticError (0,0) $ DuplicateVarDec n)

    Blocks f o j c -> do
      unless c $ do
        exists <- doesFileExist f
        unless exists $ noFileExist j o f

      let eout' = eout j o
      getAST c f >>= \case
        Left err  -> eout' err
        Right (ttab,ast) -> case getLen ast of
          len | j && null o -> print len
              | j           -> writeFile o $ (++"\n") (show len)
              | null o      -> print len
              | otherwise   -> writeFile o $ (++"\n") (show len)
          where getLen ast = case ast of
                  If _ b1 b2 _   -> getLen b1 + getLen b2
                  Loop _ b1 b2 _ -> getLen b1 + getLen b2
                  Seq b1 b2      -> getLen b1 + getLen b2
                  Step _         -> 1

    m -> do
      (f,o,j,c,transform) <- case m of
        Invert    f o j c -> return (f, o, j, c, \ttab -> showAST ttab . invert)
        Translate f o j c -> return (f, o, j, c, translate)

      unless c $ do
        exists <- doesFileExist f
        unless exists $ noFileExist j o f

      let eout' = eout j o
      getAST c f >>= \case
        Left err  -> eout' err
        Right (ttab,ast) -> case transform ttab ast of
          code | j && null o -> putStrLn $ jsonCode code
               | j           -> writeFile o $ (++"\n") (jsonCode code)
               | null o      -> putStrLn code
               | otherwise   -> writeFile o $ (++"\n") code
