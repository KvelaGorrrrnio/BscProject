module RL.Parser
( parseFile
, parseSrc
, errorPos
) where

import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)

import Common.Parser
import Common.Error
import RL.AST

rlParser :: Parser (TypeTab,AST)
rlParser = do
  whiteSpace
  decs <- typedecs
  blks <- blocks
  return (decs,blks)

blocks :: Parser [(Label, Block)]
blocks = many1 block

block :: Parser (Label, Block)
block = do
  l <- identifier'
  colon
  f <- from
  s <- statements
  t <- to
  return (l, (f,s,t))

from :: Parser From
from = pos >>= \p-> (\s->s p)
   <$> ((reserved "from"  >> From <$> identifier')
   <|> (reserved "fi"    >> Fi <$> expression <*> identifier' <*> identifier')
   <|> (reserved "entry" >> return Entry))

to :: Parser To
to  = pos >>= \p -> (\s->s p)
  <$> ((reserved "goto"  >> Goto <$> identifier')
  <|> (reserved "if"    >> IfTo <$> expression <*> identifier' <*> identifier')
  <|> (reserved "exit"  >> return Exit))

statements :: Parser [Stmt]
statements = many statement

statement :: Parser Stmt
statement = pos >>= \p -> (\s->s p)
        <$> (try updateStmt
        <|> swapStmt
        <|> skipStmt
        <|> pushStmt
        <|> popStmt)


parseSrc :: String -> Either Error (TypeTab, AST)
parseSrc s = case parse rlParser "" s of
  Left err  -> Left $ convertParseError err
  Right ast -> Right ast

parseFile :: String -> IO (Either Error (TypeTab, AST))
parseFile path = parseSrc <$> readFile path
