import System.Environment
import RL.Parser
import RL.Interp
import RL.Inversion
-- Main

main = do
  -- Get cli-arguments
  args <- getArgs
  -- Check if any given
  if null args
  then putStrLn "Please supply a .rl file to interpret."
  else do -- Do interpretation
    east <- fparse (head args)
    case east of
      Left parseErr  -> print parseErr
      Right ast | inv <- inverseAST ast,
                  res <- runProgram ast [] -> case res of
        Left progErr -> print progErr
        Right state  -> do
          putStrLn $ varTabToString state
          case runProgram inv state of
            Left progErr' -> print progErr'
            Right state'  -> putStrLn $ "---- inverse ----\n"
                                        ++ varTabToString state'


