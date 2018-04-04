import RL.Parser (sparse, parseBoolean)
main = do
putStrLn $ sparse parseBoolean "true"
putStrLn $ sparse parseBoolean "false"
putStrLn $ sparse parseBoolean "some"
putStrLn $ sparse parseBoolean "truex"
putStrLn $ sparse parseBoolean "falsex"
putStrLn $ sparse parseBoolean "True"
putStrLn $ sparse parseBoolean "False"
