module SRL.Parser
( parse
, fparse
, sparse
, parseStatements
, parseStatement
, parseExpression
, parseExpressionValue
, parseIdentifier
, parseVariable
, parseIndex
, parseValue
, parseBoolean
, parseNumber
, parseFloat
, parseInteger
, parseString
, parseName
) where

-- TODO:
-- Kunne godt tænke mig, at man kan have frie linjer, hvor man har lyst. Ville eksempelvis gerne kunne lave et program:
--
--   ~ Fibonacci program
--   ~ Written in SRL
--
--   n ^= 16
--   w ^= 1
--   from (v = 0)
--   do
--     v += w
--     swap v w
--     n -= 1
--   until (n = 0 || v > w)
--
-- Hvilke begrænsninger er der ellers?

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser, parseFromFile)
import SRL.AST

-- Interface
fparse :: String -> IO (Either P.ParseError AST)
fparse = parseFromFile (P.skipMany eol *> parseStatements <* ws <* P.many eol <* P.eof)

parse :: String -> Either P.ParseError AST
parse = P.parse (P.skipMany eol *> parseStatements <* ws <* P.many eol <* P.eof) ""

-- Safe parse
sparse :: Show a => Parser a -> String -> String
sparse parser code = case P.parse (parser <* ws <* P.eof) "" code of
  Left err -> "Nothing"
  Right ast -> show ast

-- Multiple statements
parseStatements :: Parser AST
parseStatements = P.many (ws *> parseStatement <* eol)

-- Statements
parseStatement :: Parser Statement
parseStatement =  (P.try parseIf
                   P.<|> P.try parseFrom
                   P.<|> P.try parseAssignment P.<|> P.try parseSkip
                   P.<|> P.try parseSwap P.<|> P.try parsePush
                   P.<|> P.try parsePop)
  where parseIf   = If <$> (P.string "if"     *> ws *> parseExpression) <*> (wrapEol (P.string "then") *> ws *> parseStatements) <*> (wrapEol (P.string "else")  *> ws *> parseStatements) <*> (P.string "fi" *> ws *> parseExpression)
        parseFrom = From <$> (P.string "from" *> ws *> parseExpression) <*> (wrapEol (P.string "do") *> ws *> parseStatements) <*> (P.string "until" *> ws *> parseExpression)
        wrapEol a = P.optional eol *> ws *> a <* ws <* P.optional eol
        parseSkip = const Skip <$> P.string "skip"
        parseSwap = P.string "swap" *> ws *> (Swap <$> parseIdentifier <*> parseIdentifier)
        parsePush = P.string "push" *> ws *> (Push <$> parseIdentifier <*> parseIdentifier)
        parsePop  = P.string "pop"  *> ws *> (Pop  <$> parseIdentifier <*> parseIdentifier)

parseAssignment :: Parser Statement
parseAssignment = Assignment <$> (P.try parseIndex P.<|> parseVariable) <*> parseAssignOperator <*> (ws *> parseExpression)
  where parseAssignOperator = ws *> (parsePlusEq P.<|> parseMinusEq P.<|> parseXorEq)
        parsePlusEq  = const PlusEq  <$> P.string "+="
        parseMinusEq = const MinusEq <$> P.string "-="
        parseXorEq   = const XorEq   <$> P.string "^="

-- Comments
cmt :: Parser String
cmt = P.char '~' *> (P.manyTill P.anyChar P.newline)

-- End of Line
eol :: Parser ()
eol = do
  P.skipMany1 cmt P.<|> (const () <$>P.newline)
  return ()

-- Whitespace
ws :: Parser ()
ws = P.skipMany (P.oneOf " \t") P.<?> ""

-- Expression
parseExpression :: Parser Expression
parseExpression =  parseExpressionOperators

-- Expression keywords
parseExpressionKeyword :: Parser Expression
parseExpressionKeyword = parseTop P.<|> parseEmpty
  where parseTop   = Top   <$> (P.string "top"   *> ws *> parseIdentifier)
        parseEmpty = Empty <$> (P.string "empty" *> ws *> parseIdentifier)

-- Paranthesis
parens :: Parser Expression -> Parser Expression
parens e = P.char '(' *> e <* P.char ')'

-- Operators
parseExpressionOperators :: Parser Expression
parseExpressionOperators = E.buildExpressionParser operatorTable (P.try parseExpressionKeyword P.<|> parseExpressionValue)

operatorTable = [ [ binary "*"  SRL.AST.Times E.AssocLeft, binary "/" SRL.AST.Divide E.AssocLeft]
                , [ binary "+"  SRL.AST.Plus  E.AssocLeft, binary "-" SRL.AST.Minus  E.AssocLeft]
                , [ binary "^"  SRL.AST.Xor   E.AssocLeft]
                , [ binary "="  SRL.AST.Eq    E.AssocLeft, binary "<" SRL.AST.Lth    E.AssocLeft
                  , binary ">"  SRL.AST.Gth   E.AssocLeft]
                , [ binary "&&" SRL.AST.And   E.AssocLeft]
                , [ binary "||" SRL.AST.Or    E.AssocLeft] ]
  where binary name fun = E.Infix (fun <$ reservedOp name)
        reservedOp :: String -> Parser String
        reservedOp name = ws *> P.string name <* ws

-- Expression values (leafs)
parseExpressionValue :: Parser Expression
parseExpressionValue = ws *> ((Parens <$> parens parseExpression) P.<|> (parseConstant P.<|> (Var <$> P.try parseIndex)
                              P.<|> (Var <$> parseVariable))) <* ws

-- Identifiers
parseIdentifier :: Parser Identifier
parseIdentifier = ws *> (P.try parseIndex P.<|> parseVariable)

-- Index
parseIndex :: Parser Identifier
parseIndex = do
  id <- parseName
  P.char '['
  ws
  e <- parseExpression
  ws
  P.char ']'
  return (Index id e)

-- Variables
parseVariable :: Parser Identifier
parseVariable = Variable <$> parseName

-- Constants
parseConstant :: Parser Expression
parseConstant = Constant <$> parseValue

-- Variable names
parseName :: Parser String
parseName = ws *> ((:) <$> P.letter <*> P.many (P.alphaNum P.<|> P.oneOf ['_'])) P.<?> "identifier"

-- Values
parseValue :: Parser Value
parseValue = parseString P.<|> parseBoolean P.<|> parseNumber

-- Strings
parseString :: Parser Value
parseString = StringValue <$> (P.char '"' *> P.many (P.noneOf ['"']) <* P.char '"') P.<?> "string"

-- Booleans
parseBoolean :: Parser Value
parseBoolean = BoolValue <$> (parseTrue P.<|> parseFalse) P.<?> "boolean"
  where parseTrue  = const True  <$> P.string "true"
        parseFalse = const False <$> P.string "false"

-- Numbers
parseNumber :: Parser Value
parseNumber = P.try parseFloat P.<|> parseInteger P.<?> "number"

parseInteger :: Parser Value
parseInteger = IntValue . readInt <$> P.many1 P.digit P.<?> "integer"
  where readInt = read :: String -> Int

parseFloat :: Parser Value
parseFloat = FloatValue . readFloat <$> ((++) <$> integer <*> fraction) P.<?> "float"
  where readFloat = read :: String -> Float
        integer  = P.many1 P.digit
        fraction = (:) <$> P.char '.' <*> integer
