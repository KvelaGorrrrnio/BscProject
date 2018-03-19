import RL.Parser (sparse, parseInteger)

main = do
putStrLn $ sparse parseInteger "1"
putStrLn $ sparse parseInteger "0"
putStrLn $ sparse parseInteger "9999"
putStrLn $ sparse parseInteger "123456789"
putStrLn $ sparse parseInteger "-12"
