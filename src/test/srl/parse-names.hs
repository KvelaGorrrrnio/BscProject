import SRL.Parser (sparse, parseName)
main = do
let parse =  putStrLn . (sparse parseName)
parse "a"
parse "A"
parse "_"
parse "_a"
parse "some_variable"
parse "some_variable_1"

