import System.Environment
import RL.Parser
import RL.Interp
import Control.Monad.State
import Control.Monad.Except
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
      writeFile "testpp.rl" $ astToString ast'
      print $ (runExcept . execStateT (interpAST ast')) []

