module Parser (parseFile) where

import System.IO
import Text.ParserCombinators.Parsec

import CommonParser
import AST

rlParser :: Parser AST
rlParser = whiteSpace >> blocks

blocks :: Parser [(Label, Block)]
blocks = many1 block

block :: Parser (Label, Block)
block = do
  l <- identifier
  colon
  f <- from
  s <- statements
  t <- to
  return (l, (f,s,t))

from :: Parser From
from = (reserved "from"  >> From <$> identifier)
   <|> (reserved "fi"    >> Fi <$> expression <*> identifier <*> identifier)
   <|> (reserved "entry" >> return Entry)

to :: Parser To
to  = (reserved "goto"  >> Goto <$> identifier)
  <|> (reserved "if"    >> IfTo <$> expression <*> identifier <*> identifier)
  <|> (reserved "exit"  >> return Exit)

statements :: Parser [Stmt]
statements = many statement

statement :: Parser Stmt
statement = try updateStmt
        <|> swapStmt
        <|> skipStmt
        <|> pushStmt
        <|> popStmt

parseSrc :: String -> Either ParseError AST
parseSrc = parse rlParser ""

parseFile :: String -> IO AST
parseFile path = do
  program <- readFile path
  case parseSrc program of
    Left e  -> print e >> fail "parser error"
    Right r -> return r
