{-# LANGUAGE DeriveDataTypeable #-}

module HandleArgs where
import System.Console.CmdArgs
import Prelude hiding (log)

data RevL
  = Trl {out :: FilePath, strict :: Bool, optim :: Bool, files :: [FilePath]}
  | Inv {out :: FilePath, strict :: Bool, optim :: Bool, files :: [FilePath]}
  | Opt {out :: FilePath, strict :: Bool, files :: [FilePath]}
  | Run {log :: Bool, jlog :: Bool, types :: Bool, quiet :: Bool, file :: FilePath}
  deriving (Data,Typeable,Show,Eq)

translate = Trl
  {out    = def &= typFile &= help "Output file"
  ,strict = def &= help "Keep the block order of the original program"
  ,optim  = def &= explicit &= name "optim" &= help "Optimize program before translation"
  ,files  = def &= args &= typ "FILES"
  } &= help "Translate a RevL program to its SRevL counterpart"

reverse_ = Inv
  {out    = def &= typFile &= help "Output file"
  ,strict = def &= help "Keep the block order of the original program"
  ,optim  = def &= explicit &= name "optim" &= help "Optimize program before translation"
  ,files  = def &= args &= typ "FILES"
  } &= help "Translate a RevL program to its SRevL counterpart"

optimize = Opt
  {out    = def &= typFile &= help "Output file"
  ,strict = def &= help "Keep the block order of the original program"
  ,files  = def &= args &= typ "FILES"
  } &= help "Optimize a RevL program"

interpret = Run
  {log   = def &= help "Log the program execution to an RLog file"
  ,jlog  = def &= help "Log the program execution to a JSON file"
  ,types = def &= help "Output the types to stdout"
  ,quiet = def &= help "Hide the result of the program"
  ,file  = def &= args &= typFile
  } &= help "Interpret a RevL program" &= auto

mode = cmdArgsMode $ modes [interpret, optimize, reverse_, translate]
  &= program "revl"
  &= help "Interpret, optimize or translate a RevL program"
  &= summary "RevL : Unstructured Reversible Programming Language"

handleArgs :: IO RevL
handleArgs = cmdArgsRun mode
