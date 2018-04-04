import SRL.Parser (sparse, parseString)
main = do
putStrLn $ sparse parseString "not a string"
putStrLn $ sparse parseString "\"I'm a string\""
putStrLn $ sparse parseString "\"I'm almost a string"
putStrLn $ sparse parseString "I'm almost a string\""
putStrLn $ sparse parseString "\"123321 Calling out!\""
putStrLn $ sparse parseString "\"_-@$\""

