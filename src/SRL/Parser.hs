module SRL.Parser
( parseFile
, parseSrc
, errorPos
) where

import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)

import Common.Parser
import Common.Error
import SRL.AST

srlParser :: Parser AST
srlParser = whiteSpace >> statements

statements :: Parser [Stmt]
statements = many1 statement

statement :: Parser Stmt
statement = pos >>= \p -> (\s->s p)
        <$> (try updateStmt
        <|> swapStmt
        <|> skipStmt
        <|> pushStmt
        <|> popStmt
        -- srl
        <|> ifStmt
        <|> untilStmt)

ifStmt :: Parser (Pos -> Stmt)
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

untilStmt :: Parser (Pos -> Stmt)
untilStmt = do
  reserved "from"
  a <- expression
  reserved "do"
  s <- statements
  reserved "until"
  t <- expression
  return $ Until a s t

parseSrc :: String -> Either Error AST
parseSrc s = case parse srlParser "" s of
  Left err  -> Left $ convertParseError err
  Right ast -> Right ast

parseFile :: String -> IO (Either Error AST)
parseFile path = parseSrc <$> readFile path
