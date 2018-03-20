import System.Environment
import RL.Parser
import RL.Interp
import RL.AST -- ideally, the parser returns a correct AST
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
    Left err -> print err
    Right ast -> do
      let ast' = toAST ast
      print $ runProgram ast'

