import RL.Parser (sparse, parseIdentifier)
main = do
let parse = putStrLn . (sparse parseIdentifier)
parse "a"
parse "A"
parse "_"
parse "_a"
parse "some_variable"
parse "some_variable_1"
