import SRL.Parser (sparse, parseExpression)
main = do
let parse = putStrLn . (sparse parseExpression)
parse "1+1"
parse "1 + 1"
parse "1-1"
parse "1 - 1"
parse "1^1"
parse "1 ^ 1"
parse "1*1"
parse "1 * 1"
parse "1/1"
parse "1 / 1"
parse "(1 + 2) * 3"
