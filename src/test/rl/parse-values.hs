import RL.Parser (sparse, parseValue)
main = do
putStrLn $ sparse parseValue "1234"
putStrLn $ sparse parseValue "1.234"
putStrLn $ sparse parseValue "true"
putStrLn $ sparse parseValue "false"
putStrLn $ sparse parseValue "\"some string\""
