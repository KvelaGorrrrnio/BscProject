module Lib
( listFiles
, listScripts
, getSuite
, getSuites
, Target (..)
, OutFile (..)
, runSuite
, runSuites
, scriptColor
) where

import System.FilePath.Glob (compile, globDir)
import System.FilePath.Posix (takeExtension)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (..))
import System.Directory (getCurrentDirectory)
import Data.List (stripPrefix,concat)
import Control.Monad (mapM,join, filterM)
import Control.Monad.Except
import Data.Char (isSpace)

data Target
  = All
  | SRL
  | RL
  deriving (Show,Eq)

data Mode --  FAILS JSON LOG
  = Run       Bool  Bool Bool
  | Invert    Bool  Bool
  | Translate Bool  Bool
  | Typeof    Bool  Bool
  deriving Show

type Suite  = (String,[OutFile])
data OutFile = OutFile String Mode -- File FAILS JSON LOG TYPE
  deriving Show

bdir = "test/suite"

listFiles :: String -> [String] -> IO [[String]]
listFiles dir pats = do
  paths <- globDir (map compile pats) dir
  dir'  <- if null dir then getCurrentDirectory else return dir
  return $ map (map (removePrefix dir)) paths
  where removePrefix pf str = case stripPrefix pf str of
          Just str' -> dropWhile (=='/') str'
          Nothing   -> dropWhile (=='/') str

listScripts :: Target -> IO [String]
listScripts All = concat <$> listFiles bdir ["*.srl","*.rl"]
listScripts RL  = concat <$> listFiles bdir ["*.rl"]
listScripts SRL = concat <$> listFiles bdir ["*.srl"]

getSuite :: String -> IO Suite
getSuite f = listFiles bdir (patterns f) >>=
  \[o,lo,io,tro,tyo,e,le,ie,tre,tye,jo,jlo,jio,jtro,jtyo] -> return (f,concat [
      map (flip OutFile $ Run False False False) o,
      map (flip OutFile $ Run False False True) lo,
      map (flip OutFile $ Invert False False) io,
      map (flip OutFile $ Translate False False) tro,
      map (flip OutFile $ Typeof False False) tyo,
      map (flip OutFile $ Run True False False) e,
      map (flip OutFile $ Run True False True) le,
      map (flip OutFile $ Invert True False) ie,
      map (flip OutFile $ Translate True False) tre,
      map (flip OutFile $ Typeof True False) tye,
      map (flip OutFile $ Run False True False) jo,
      map (flip OutFile $ Run False True True) jlo,
      map (flip OutFile $ Invert False True) jio,
      map (flip OutFile $ Translate False True) jtro,
      map (flip OutFile $ Typeof False True) jtyo
    ])

getSuites :: [String] -> IO [Suite]
getSuites = mapM getSuite

patterns f = map (f++)
  [ ".out"
  , ".log.out"
  , ".invert.out"
  , ".translate.out"
  , ".typeof.out"
  , ".err"
  , ".log.err"
  , ".invert.err"
  , ".translate.err"
  , ".typeof.err"
  , ".json.out"
  , ".json.log.out"
  , ".json.invert.out"
  , ".json.translate.out"
  , ".json.typeof.out"
  ]

type SuiteState = ExceptT String IO
runSuite :: Suite -> IO (String,[(Bool,String)])
runSuite (file,outfiles) = do
  result <- mapM (run file) outfiles
  return (file,map getDesc result)
  where run file = runExceptT . runSuite' file
        getDesc (Right _) = (True, trueColor ".")
        getDesc (Left e)  = (False,"\n  " ++ e)

runSuite' :: String -> OutFile -> SuiteState ()
runSuite' file (OutFile o m) = do
  let lng = getInterpreter file
      (mode, e, j, l) = getModeFlags m
      flags = (if j || l then "-" else "") ++ (hasF j "j" ++ hasF l "l")
  (exitcode,stdout,stderr) <- lift $ readProcessWithExitCode "stack" ["exec", lng, "--", mode, flags, bdir ++ "/" ++ file] ""
  exp <- lift $ readFile $ bdir ++ "/" ++ o
  out <- case exitcode of
    ExitSuccess   | e     -> throwError $ "'" ++ o ++ "' should have failed with:\n" ++ expColor exp ++ "  , but succeeded with:\n" ++ errColor stdout
    ExitSuccess   | not e -> return stdout
    ExitFailure _ | e     -> return stderr
    ExitFailure _ | not e -> throwError $ "'" ++ o ++ "' should have succeeded with:\n" ++ expColor exp ++ "  , but failed with:\n" ++ errColor stderr
  if trim out == trim exp then return ()
    else throwError $ "Output from '" ++ o ++ "' did not meet expectation:\n" ++ expColor (trim exp) ++ "\n" ++ replicate (max (length out) (length exp)) '-' ++ "\n" ++ errColor (trim out) ++ "\n"
  when (mode == "translate" && not j) $ do
    (ec1,so1,se1) <- lift $ readProcessWithExitCode "stack" ["exec", lng, "--", bdir ++ "/" ++ file] ""
    (ec2,so2,se2) <- lift $ readProcessWithExitCode "stack" ["exec", swapInterpreter lng, "--", bdir ++ "/" ++ o] ""
    case (ec1,ec2) of
      (ExitSuccess, ExitSuccess) -> if trim so1 == trim so2 then return ()
        else throwError $ "Execution of '" ++ file ++ "' and '" ++ o ++ "' have different output:\n" ++ expColor (trim so1) ++ "\n" ++ replicate (max (length out) (length exp)) '-' ++ "\n" ++ errColor (trim so2) ++ "\n"
      _                          -> throwError $ "Either '" ++ file ++ "' and/or '" ++ o ++ "' wasn't executed."
  where hasF f s = if f then s else ""
        getInterpreter = tail . takeExtension
        swapInterpreter "rl" = "srl"
        swapInterpreter "srl" = "rl"
        swapInterpreter s = s


runSuites :: [Suite] -> IO [(String,[(Bool,String)])]
runSuites = mapM runSuite

getModeFlags :: Mode -> (String,Bool,Bool,Bool)
getModeFlags (Run e j l)     = ("run",e,j,l)
getModeFlags (Invert e j)    = ("invert",e,j,False)
getModeFlags (Translate e j) = ("translate",e,j,False)
getModeFlags (Typeof e j)    = ("typeof",e,j,False)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
--
-- coloring
expColor i    = "\x1b[36m" ++ i ++ "\x1b[0m"
errColor i    = "\x1b[31m" ++ i ++ "\x1b[0m"
scriptColor i = "\x1b[35m" ++ i ++ "\x1b[0m"
trueColor i   = "\x1b[32m" ++ i ++ "\x1b[0m"
