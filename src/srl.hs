import System.Environment
import SRL.Parser
import SRL.Interp
import SRL.Inversion
import SRL.ToString
-- Main

main = do
  -- Get cli-arguments
  args <- getArgs
  -- Check if any given
  if null args
  then putStrLn "Please supply a .srl file to interpret."
  else do -- Do interpretation
    east <- fparse (head args)
    case east of
      Left parseErr  -> print parseErr
      Right ast | inv <- inverseAST ast,
                  res <- runProgram ast [] -> case res of
        Left progErr -> print progErr
        Right state  -> do
          putStrLn $ astToString ast ++ "\n"
          putStrLn "Results:"
          putStrLn $ varTabToString state
          case runProgram inv state of
            Left progErr' -> print progErr'
            Right state'  -> do
              putStrLn "\n---- inverse ----\n"
              putStrLn $ astToString inv ++ "\n"
              putStrLn "Results:"
              putStrLn $ varTabToString state'


