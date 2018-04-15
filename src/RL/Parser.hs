module Parser (parseFile) where

-- Import necessary libraries
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos

import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.HashMap.Strict as M

import AST

-- specifying the language
languageDef =
  emptyDef { Token.commentStart     = "/*"
           , Token.commentEnd       = "*/"
           , Token.commentLine      = "~"
           , Token.identStart       = letter <|> char '_'
           , Token.identLetter      = alphaNum <|> char '\'' <|> char '_'
           , Token.reservedNames    = [ "entry"
                                      , "from"
                                      , "fi"
                                      , "exit"
                                      , "goto"
                                      , "if"
                                      -- statements
                                      , "skip" , "." -- for consice notation
                                      , "swap"
                                      , "push"
                                      , "pop"
                                      ]
           , Token.reservedOpNames  = [ "+=", "-=", "^=", "*=", "/="
                                      , "+", "-", "^", "*", "**", "/", "%", "sig"
                                      , "=", "<", ">", "&&", "||", "not"
                                      , "top", "empty", "size" ]
           }

lexer = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
integer       = Token.integer       lexer
colon         = Token.colon         lexer
whiteSpace    = Token.whiteSpace    lexer

rlParser :: Parser AST
rlParser =
  whiteSpace >> AST . M.fromList <$> blocks

blocks :: Parser [(Label, Block)]
blocks = many1 block

block :: Parser (Label, Block)
block = do
  l <- identifier
  colon
  f <- from
  s <- statements
  t <- to
  return (l, Block (f,s,t))

from :: Parser From
from = (reserved "from"  >> From <$> identifier)
   <|> (reserved "fi"    >> Fi <$> expression <*> identifier <*> identifier)
   <|> (reserved "entry" >> return Entry)

to :: Parser To
to  = (reserved "goto"  >> Goto <$> identifier)
  <|> (reserved "if"    >> If <$> expression <*> identifier <*> identifier)
  <|> (reserved "exit"  >> return Exit)

statements :: Parser [Stmt]
statements = many1 statement

statement :: Parser Stmt
statement = try updateStmt
        <|> swapStmt
        <|> skipStmt
        <|> pushStmt
        <|> popStmt

updateStmt :: Parser Stmt
updateStmt = do
  var <- identifier
  op  <- update
  expr <- expression
  return $ Update var op expr

update = (reservedOp "+=" >> return PlusEq)
     <|> (reservedOp "-=" >> return MinusEq)
     <|> (reservedOp "^=" >> return XorEq)
     <|> (reservedOp "*=" >> return MultEq)
     <|> (reservedOp "/=" >> return DivEq)

skipStmt :: Parser Stmt
skipStmt = (reserved "skip" <|> reserved ".") >> return Skip

swapStmt :: Parser Stmt
swapStmt = reserved "swap" >> Swap <$> identifier <*> identifier

pushStmt :: Parser Stmt
pushStmt = reserved "push" >> Push <$> identifier <*> identifier

popStmt :: Parser Stmt
popStmt = reserved "pop" >> Pop <$> identifier <*> identifier

expression :: Parser Exp
expression = buildExpressionParser operators term

operators = [ [Prefix (reservedOp "size"  >> return Size                  )           ]
            , [Prefix (reservedOp "top"   >> return Top                   )           ]
            , [Prefix (reservedOp "empty" >> return Empty                 )           ]
            , [Prefix (reservedOp "-"     >> return (AUnary     Neg      ))           ]
            , [Prefix (reservedOp "sig"   >> return (AUnary     Sign     ))           ]
            , [Prefix (reservedOp "not"   >> return Not                   )           ]
            , [Infix  (reservedOp "**"    >> return (ABinary    Mult     )) AssocRight]
            , [Infix  (reservedOp "%"     >> return (DivBinary  Mod      )) AssocLeft ]
            , [Infix  (reservedOp "*"     >> return (ABinary    Mult     )) AssocLeft ]
            , [Infix  (reservedOp "/"     >> return (DivBinary  Div      )) AssocLeft ]
            , [Infix  (reservedOp "+"     >> return (ABinary    Plus     )) AssocLeft ]
            , [Infix  (reservedOp "-"     >> return (ABinary    Minus    )) AssocLeft ]
            , [Infix  (reservedOp "<"     >> return (Relational Less     )) AssocNone ]
            , [Infix  (reservedOp ">"     >> return (Relational Greater  )) AssocNone ]
            , [Infix  (reservedOp "="     >> return (Relational Eq       )) AssocNone ]
            , [Infix  (reservedOp "&&"    >> return (LBinary    And      )) AssocLeft ]
            , [Infix  (reservedOp "||"    >> return (LBinary    Or       )) AssocLeft ]
            ]
term =  fmap Parens (parens expression)
     <|> fmap Var identifier
     <|> fmap Lit (IntV <$> integer)

parseSrc :: String -> Either ParseError AST
parseSrc = parse rlParser ""

parseFile :: String -> IO AST
parseFile path = do
  program <- readFile path
  case parseSrc program of
    Left e  -> print e >> fail "parser error"
    Right r -> return r
