module RL.Parser
( parse
) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)
import RL.AST

-- Interface
parse :: String -> Either P.ParseError [Block]
parse code = P.parse (parseBlocks <* P.eof) "" code

-- Blocks
parseBlocks :: Parser [Block]
parseBlocks = P.many1 parseBlock

-- Block
parseBlock :: Parser Block
parseBlock =  Block <$> (parseIdentifier <* P.char ':') <*> (ws *> parseFrom) <*> (ws *> P.option [] parseStatements) <*> (ws *> parseGoto)

-- Goto
parseGoto :: Parser Goto
parseGoto = P.try parseGoto' P.<|> P.try parseIf P.<|> parseExit
  where parseGoto' = P.string "goto" *> (Goto <$> parseIdentifier)
        parseIf    = P.string "fi"   *> (If <$> parseExpression <*> parseIdentifier <*> parseIdentifier)
        parseExit = (\_ -> Exit) <$> P.string "exit"

-- From
parseFrom :: Parser From
parseFrom = P.try parseFrom' P.<|> P.try parseFi P.<|> parseEntry
  where parseFrom' = P.string "from" *> (From <$> parseIdentifier)
        parseFi    = P.string "fi"   *> (Fi <$> parseExpression <*> parseIdentifier <*> parseIdentifier)
        parseEntry = (\_ -> Entry) <$> P.string "entry"

-- Multiple statements
parseStatements :: Parser [Statement]
parseStatements = P.many parseStatement

-- Statements
parseStatement :: Parser Statement
parseStatement =  P.try parseSkip P.<|> P.try parseSwap
            P.<|> P.try parsePush P.<|> P.try parsePop
            P.<|> P.try parseAssignment
  where parseSkip = (\_ -> Skip) <$> P.string "skip" <* ws
        parseSwap = P.string "swap" *> ws *> (Swap <$> parseIdentifier <*> parseIdentifier) <* ws
        parsePush = P.string "push" *> ws *> (Push <$> parseIdentifier <*> parseIdentifier) <* ws
        parsePop  = P.string "pop"  *> ws *> (Pop  <$> parseIdentifier <*> parseIdentifier) <* ws

parseAssignment :: Parser Statement
parseAssignment = P.try parseIndexAssignment P.<|> parseVariableAssignment
  where parseIndexAssignment    = IndexAssignment <$> parseIdentifier <*> (P.char '[' *> parseExpression <* P.char ']') <*> parseAssignOperator <*> parseExpression
        parseVariableAssignment = Assignment <$> parseIdentifier <*> parseAssignOperator <*> parseExpression
        parseAssignOperator = ws *> (parsePlusEq P.<|> parseMinusEq P.<|> parseXorEq) <* ws
        parsePlusEq  = (\_ -> PlusEq) <$> P.string "+="
        parseMinusEq = (\_ -> MinusEq) <$> P.string "-="
        parseXorEq   = (\_ -> XorEq) <$> P.string "^="

-- Whitespace
ws :: Parser ()
ws = (P.skipMany P.space) P.<?> ""

-- Expression
parseExpression :: Parser Expression
parseExpression = parseExpressionKeyword P.<|> parseExpressionOperators

-- Expression keywords
parseExpressionKeyword :: Parser Expression
parseExpressionKeyword = parseTop P.<|> parseEmpty
  where parseTop   = Top   <$> (P.string "top"   *> ws *> parseIdentifier)
        parseEmpty = Empty <$> (P.string "empty" *> ws *> parseIdentifier)

-- Operators
parseExpressionOperators :: Parser Expression
parseExpressionOperators = E.buildExpressionParser operatorTable parseExpressionValue

operatorTable = [ [ binary "*" RL.AST.Times E.AssocLeft, binary "/" RL.AST.Divide E.AssocLeft]
                , [ binary "+" RL.AST.Plus  E.AssocLeft, binary "-" RL.AST.Minus  E.AssocLeft]
                , [ binary "^" RL.AST.Xor   E.AssocLeft] ]
  where binary name fun assoc = E.Infix (fun <$ reservedOp name) assoc
        reservedOp :: String -> Parser String
        reservedOp name = ws *> P.string name <* ws

-- Expression values (leafs)
parseExpressionValue :: Parser Expression
parseExpressionValue = ws *> (parseConstant P.<|> P.try parseIndex P.<|> parseVariable) <* ws

-- Index TODO: Remove value
parseIndex :: Parser Expression
parseIndex = do
  id <- parseIdentifier
  P.char '['
  ws
  e <- parseExpression
  ws
  P.char ']'
  ws
  return (Index id e)

-- Variables
parseVariable :: Parser Expression
parseVariable = Var <$> parseIdentifier

-- Constants
parseConstant :: Parser Expression
parseConstant = Constant <$> parseValue

-- Identifiers
parseIdentifier :: Parser Identifier
parseIdentifier = ws *> ((:) <$> P.letter <*> P.many (P.alphaNum P.<|> P.oneOf ['_'])) P.<?> "identifier"

-- Values
parseValue :: Parser Value
parseValue = parseString P.<|> parseBoolean P.<|> parseNumber

-- Strings
parseString :: Parser Value
parseString = StringValue <$> (P.char '"' *> P.many (P.noneOf ['"']) <* P.char '"') P.<?> "string"

-- Booleans
parseBoolean :: Parser Value
parseBoolean = BoolValue <$> (parseTrue P.<|> parseFalse) P.<?> "boolean"
  where parseTrue  = (\_ -> True)  <$> P.string "true"
        parseFalse = (\_ -> False) <$> P.string "false"

-- Numbers
parseNumber :: Parser Value
parseNumber = P.try parseFloat P.<|> parseInteger P.<?> "number"

parseInteger :: Parser Value
parseInteger = (IntValue . readInt) <$> P.many1 P.digit P.<?> "integer"
  where readInt = read :: String -> Int

parseFloat :: Parser Value
parseFloat = (FloatValue . readFloat) <$> ((++) <$> integer <*> fraction) P.<?> "float"
  where readFloat = read :: String -> Float
        integer  = P.many1 P.digit
        fraction = (:) <$> P.char '.' <*> integer
