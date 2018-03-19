import RL.Parser (sparse, parseFloat)
main = do
putStrLn $ sparse parseFloat "0.0"
putStrLn $ sparse parseFloat "1.0"
putStrLn $ sparse parseFloat "1.2434"
putStrLn $ sparse parseFloat "0.1234567"
putStrLn $ sparse parseFloat "1234567.0"
putStrLn $ sparse parseFloat "-1.0"

