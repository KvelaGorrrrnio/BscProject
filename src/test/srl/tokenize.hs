import Lexer
main = do
print $tokenize "123"
print $tokenize "x"
print $tokenize "x5a"
print $tokenize "if a=b then 5 else 2"
