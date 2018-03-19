import RL.Parser
import RL.Interp
-- Main

main = do
-- A sample AST
-- data Block = Block Label From [Inst] To deriving Show
  east <- fparse "test.rl"
  case east of
    Left err -> print err
    Right ast -> print $ interpAST (toAST ast) []

