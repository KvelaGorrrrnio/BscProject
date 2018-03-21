module RL.Parser
( parse
, fparse
, sparse
, parseBlocks
, parseBlock
, parseGoto
, parseFrom
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

-- [SVÆR] Tillad arbitrær indentering af gotos
-- Tillad newlines i expressions

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser, parseFromFile)
import RL.AST

-- Interface
fparse :: String -> IO (Either P.ParseError AST)
fparse = parseFromFile (toAST <$> parseBlocks <* ws <* P.many eol <* P.eof)

parse :: String -> Either P.ParseError AST
parse = P.parse (toAST <$> parseBlocks <* ws <* P.many eol <* P.eof) ""

-- Safe parse
sparse :: Show a => Parser a -> String -> String
sparse parser code = case P.parse (parser <* ws <* P.eof) "" code of
  Left err -> "Nothing"
  Right ast -> show ast

-- Blocks
parseBlocks :: Parser [Block]
parseBlocks = P.skipMany eol *> P.many1 parseBlock

-- Block
parseBlock :: Parser Block
parseBlock =  Block <$> (parseName <* P.char ':') <*> (ws *> parseFrom <* ws <* eol) <*> parseStatements <*> parseGoto <* ws <* P.many1 eol

-- Goto
parseGoto :: Parser Goto
parseGoto = P.try parseGoto' P.<|> P.try parseIf P.<|> parseExit
  where parseGoto' = P.string "goto" *> (Goto <$> parseName)
        parseIf    = P.string "if"   *> (If <$> parseExpression <*> parseName <*> parseName)
        parseExit = const Exit <$> P.string "exit"

-- From
parseFrom :: Parser From
parseFrom = P.try parseFrom' P.<|> P.try parseFi P.<|> parseEntry
  where parseFrom' = P.string "from" *> ws *> (From <$> parseName)
        parseFi    = P.string "fi"   *> ws *> (Fi <$> parseExpression <*> parseName <*> parseName)
        parseEntry = const Entry <$> P.string "entry"

-- Multiple statements
parseStatements :: Parser [Statement]
parseStatements = P.many (ws *> parseStatement <* eol <* P.skipMany (P.try (ws *> eol)))

-- Statements
parseStatement :: Parser Statement
parseStatement =  (P.try parseAssignment P.<|> P.try parseSkip
                   P.<|> P.try parseSwap P.<|> P.try parsePush
                   P.<|> P.try parsePop)
  where parseSkip = const Skip <$> P.string "skip"
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
parens e = P.char '(' *> ws *> e  <* ws <* P.char ')'

-- Operators
parseExpressionOperators :: Parser Expression
parseExpressionOperators = E.buildExpressionParser operatorTable (P.try parseExpressionKeyword P.<|> parseExpressionValue)

operatorTable = [ [ binary "*"   RL.AST.Times E.AssocLeft, binary "/" RL.AST.Divide E.AssocLeft]
                , [ binary "+"   RL.AST.Plus  E.AssocLeft, binary "-" RL.AST.Minus  E.AssocLeft]
                , [ binary "^"   RL.AST.Xor   E.AssocLeft]
                , [ unary  "not" RL.AST.Not]
                , [ binary "="   RL.AST.Eq    E.AssocLeft, binary "!=" RL.AST.Neq   E.AssocLeft
                  , binary "<"   RL.AST.Lth   E.AssocLeft, binary ">"  RL.AST.Gth   E.AssocLeft]
                , [ binary "&&"  RL.AST.And   E.AssocLeft]
                , [ binary "||"  RL.AST.Or    E.AssocLeft] ]
  where binary name fun = E.Infix  (fun <$ reservedOp name)
        unary  name fun = E.Prefix (fun <$ reservedOp name)
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
