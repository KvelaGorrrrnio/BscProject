import RL.Parser (sparse, parseValue)
main = do
putStrLn $ sparse parseValue "1234"
