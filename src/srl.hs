import System.Environment
import SRL.Parser
--import SRL.Interp
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
      Left parseErr -> print parseErr
      Right ast -> print ast


