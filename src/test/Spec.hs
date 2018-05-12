import Lib
import Data.List (intercalate)

target = All

main :: IO ()
main = do
  files <- listScripts target
  suites <- getSuites files
  results <- runSuites suites
  let (allPassed,output) = processResults results
  if allPassed then putStrLn $ output ++ "\nAll tests passed."
  else fail $ "\n" ++ output ++ "\nNot all tests passed."

processResults :: [(String, [(Bool,String)])] -> (Bool,String)
processResults results = let m = map processResult results in
  (all fst m, concatMap snd m)
  where processResult (file,outfiles) =
          ( all fst outfiles
          , file ++ ":" ++ concatMap snd outfiles ++ "\n" )

