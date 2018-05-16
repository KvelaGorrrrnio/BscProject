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

srlParser :: Parser (TypeTab,AST)
srlParser = do
  whiteSpace
  decs <- typedecs
  blk  <- block
  eof
  return (decs,blk)

block :: Parser Block
block = do
  (b:seq) <- many1 block'
  return $ foldl Seq b seq

block' :: Parser Block
block' = stmtBlock <|> ifBlock <|> untilBlock

stmtBlock :: Parser Block
stmtBlock = Atom <$> statement

ifBlock :: Parser Block
ifBlock = pos >>= \p -> (\s->s p) <$> do
  reserved "if"
  t <- expression
  reserved "then"
  b1 <- block
  -- optional v
  reserved "else"
  b2 <- block
  -- optional ^
  reserved "fi"
  a <- expression
  return $ If t b1 b2 a

untilBlock :: Parser Block
untilBlock = pos >>= \p -> (\s->s p) <$> do
  reserved "from"
  a <- expression
  reserved "do"
  b1 <- block
  reserved "loop"
  b2 <- block
  reserved "until"
  t <- expression
  return $ Until True a b1 b2 t

-- let until and let only have one body

parseSrc :: String -> Either Error (TypeTab, AST)
parseSrc s = case parse srlParser "" s of
  Left err  -> Left $ convertParseError err
  Right ast -> Right ast

parseFile :: String -> IO (Either Error (TypeTab, AST))
parseFile path = parseSrc <$> readFile path
