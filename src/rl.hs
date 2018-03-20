import System.Environment
import RL.Parser
import RL.Interp
-- Main

main = do
  -- Get cli-arguments
  args <- getArgs
  -- Check if any given
  if (length args) == 0
  then do
    putStrLn "Please supply a .rl file to interpret."
    return ()
  else do -- Do interpretation
  east <- fparse (head args)
  case east of
    Left err -> print err
    Right ast -> do
      let ast' = toAST ast
      writeFile "testpp.rl" $ astToString ast'
      print $ interpAST ast' []

