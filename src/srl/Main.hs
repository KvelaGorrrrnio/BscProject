module Main where
import Lexer
main = do
  let prog = "a ^= 5\nb += 7\nif a==b then a += b else b -= a fi a + b"
  print $tokenize prog
