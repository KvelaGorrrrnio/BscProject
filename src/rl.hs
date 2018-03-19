import RL.Parser
import RL.Interp
-- Main

main = do
-- A sample AST
-- data Block = Block Label From [Inst] To deriving Show
  east <- fparse "RL/test.rl"
  case east of
    Left err -> print err
    Right ast -> do
      let ast' = toAST ast
      writeFile "testpp.rl" $ astToString ast'
      print $ interpAST ast' []

