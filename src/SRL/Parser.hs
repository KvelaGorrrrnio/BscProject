module Parser (parseFile) where

import System.IO
import Text.ParserCombinators.Parsec

import CommonParser
import SrlAST
import Extra

srlParser :: Parser AST
srlParser = whiteSpace >> AST <$> statements

statements :: Parser [Stmt]
statements = many1 statement

statement :: Parser Stmt
statement = try updateStmt
        <|> swapStmt
        <|> skipStmt
        <|> pushStmt
        <|> popStmt
        -- srl
        <|> ifStmt
        <|> untilStmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  t <- expression
  reserved "then"
  s1 <- statements
  reserved "else"
  s2 <- statements
  reserved "fi"
  a <- expression
  return $ If t s1 s2 a

untilStmt :: Parser Stmt
untilStmt = do
  reserved "from"
  a <- expression
  reserved "do"
  s <- statements
  reserved "until"
  t <- expression
  return $ Until a s t

parseSrc :: String -> Either ParseError AST
parseSrc = parse srlParser ""

parseFile :: String -> IO AST
parseFile path = do
  program <- readFile path
  case parseSrc program of
    Left e  -> print e >> fail "parser error"
    Right r -> return r
