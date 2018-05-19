import Lib (listScripts, getSuites, runSuite, Target(..), trueColor, errColor)
import Data.List (intercalate)
import Control.Monad (mapM)
import System.Exit (exitFailure)

target = All

main :: IO ()
main = do
  files <- listScripts target
  suites <- getSuites files
  putStr "\n"
  passed <- mapM runSuites' suites
  let allPassed = foldl (&&) True passed
  if allPassed then putStrLn $ trueColor "\nAll tests passed."
  else putStrLn (errColor "\nNot all tests passed.") >> exitFailure
  where runSuites' suite = do
          result  <- runSuite suite
          let (passed,output) = processResult result
          putStr output
          return passed

processResult (file,outfiles) =
        ( all fst outfiles
        , file ++ ":" ++ concatMap snd outfiles ++ "\n" )
