module Common.Parser where

import qualified Data.HashMap.Strict as M

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Common.AST

-- whitespace
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- symbols
symbol :: String -> Parser String
symbol = L.symbol sc

-- symbols
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

colon :: Parser a -> Parser a
colon     = symbol ":"

dot :: Parser a -> Parser a
dot       = symbol "."

integer :: Parser Integer
integer = lexeme L.decimal

semi :: Parser String
semi = symbol ";"

-- reserved words
rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = [ "entry"
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
      , "then"
      , "else"
      , "do"
      , "until"
      -- expressions
      , "neg"
      , "sig"
      , "not"
      , "and"
      , "or"
      , "top"
      , "empty"
      , "size"
      ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

statements :: Parser Stmt
statements = many statement

statement :: Parser Stmt
statement = ifStmt
        <|> untilStmt
        <|> skipStmt
        <|> swapStmt
        <|> updateStmt

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  t  <- expression
  rword "then"
  s1 <- statements
  rword "else"
  s2 <- statements
  rword "fi"
  a  <- expression
  return (If t s1 s2 a)

untilStmt :: Parser (Pos -> Stmt)
untilStmt = do
  rword "from"
  a <- expression
  rword "do"
  s <- statements
  rword "until"
  t <- expression
  return $ Until True a s t

updateStmt :: Parser (Pos -> Stmt)
updateStmt = do
  var  <- identifier
  op   <- update
  expr <- expression
  return $ Update var op expr

update = (reservedOp "+=" >> return PlusEq)
     <|> (reservedOp "-=" >> return MinusEq)
     <|> (reservedOp "^=" >> return XorEq)
     <|> (reservedOp "*=" >> return MultEq)
     <|> (reservedOp "/=" >> return DivEq)

skipStmt :: Parser (Pos -> Stmt)
skipStmt = (reserved "skip" <|> reserved ".") >> return Skip

swapStmt :: Parser (Pos -> Stmt)
swapStmt = reserved "swap" >> Swap <$> identifier <*> identifier

pushStmt :: Parser (Pos -> Stmt)
pushStmt = reserved "push" >> Push <$> identifier <*> identifier

popStmt :: Parser (Pos -> Stmt)
popStmt = reserved "pop" >> Pop <$> identifier <*> identifier


-- -- specifying the language
-- languageDef =
--   emptyDef { Token.commentStart     = "/*"
--            , Token.commentEnd       = "*/"
--            , Token.commentLine      = "//"
--            , Token.identStart       = letter
--            , Token.identLetter      = alphaNum <|> oneOf ['_', '\'']
--            , Token.reservedNames    = [ "entry"
--                                       , "from"
--                                       , "fi"
--                                       , "exit"
--                                       , "goto"
--                                       , "if"
--                                       -- statements
--                                       , "skip" , "." -- for consice notation
--                                       , "swap"
--                                       , "push"
--                                       , "pop"
--                                       , "then"
--                                       , "else"
--                                       , "do"
--                                       , "until"
--                                       -- expressions
--                                       , "neg"
--                                       , "sig"
--                                       , "not"
--                                       , "and"
--                                       , "or"
--                                       , "top"
--                                       , "empty"
--                                       , "size"
--                                       ]
--            , Token.reservedOpNames  = [ "+=", "-=", "^=", "*=", "/="
--                                       , "+", "-", "^", "*", "**", "/", "%"
--                                       , "=", "!=", "<", "<=", ">", ">="
--                                       , "!", "&&", "||"
--                                       ]
--            }
--
-- -- get position
-- pos :: Parser Pos
-- pos = do
--   s <- getPosition
--   return (sourceLine s,sourceColumn s)
--
-- lexer = Token.makeTokenParser languageDef
--
-- identifier    = Token.identifier    lexer
-- reserved      = Token.reserved      lexer
-- reservedOp n  = Token.reservedOp    lexer n >> pos
-- parens        = Token.parens        lexer
-- brackets      = Token.brackets      lexer
-- integer       = Token.integer       lexer
-- colon         = Token.colon         lexer
-- whiteSpace    = Token.whiteSpace    lexer
--
-- -- type declarations
-- typedecs :: Parser TypeTab
-- typedecs = M.fromList <$> many typedec
--
-- typedec :: Parser (Id, Type)
-- typedec = do
--   tp  <- typet
--   var <- identifier --many1 identifier
--   return (var, tp) -- $ map (\id -> (id,tp)) vars
--
-- typet :: Parser Type
-- typet = (reserved "int" >> return IntT)
--     <|> (reserved "list" >> ListT <$> typet)
--     <|> (ListT <$> brackets typet)
--
-- -- statements
-- updateStmt :: Parser (Pos -> Stmt)
-- updateStmt = do
--   var  <- identifier
--   op   <- update
--   expr <- expression
--   return $ Update var op expr
--
-- update = (reservedOp "+=" >> return PlusEq)
--      <|> (reservedOp "-=" >> return MinusEq)
--      <|> (reservedOp "^=" >> return XorEq)
--      <|> (reservedOp "*=" >> return MultEq)
--      <|> (reservedOp "/=" >> return DivEq)
--
-- skipStmt :: Parser (Pos -> Stmt)
-- skipStmt = (reserved "skip" <|> reserved ".") >> return Skip
--
-- swapStmt :: Parser (Pos -> Stmt)
-- swapStmt = reserved "swap" >> Swap <$> identifier <*> identifier
--
-- pushStmt :: Parser (Pos -> Stmt)
-- pushStmt = reserved "push" >> Push <$> identifier <*> identifier
--
-- popStmt :: Parser (Pos -> Stmt)
-- popStmt = reserved "pop" >> Pop <$> identifier <*> identifier
--
-- -- expressions
-- expression :: Parser Exp
-- expression = buildExpressionParser operators term <?> "expression"
--
-- operators = [
--               [Prefix ((reservedOp "^"  <|> (reserved "top">>pos) )
--                                          >>= \p -> return (\e->  Unary     Top      e   p))           ]
--             , [Prefix ((reservedOp "#"  <|> (reserved "size">>pos) )
--                                          >>= \p -> return (\e->  Unary     Size     e   p))           ]
--             , [Prefix ((reservedOp "?"  <|> (reserved "empty">>pos) )
--                                          >>= \p -> return (\e->  Unary     Empty    e   p))           ]
--             , [Prefix ((reservedOp "-"  <|> (reserved "neg">>pos)  )
--                                          >>= \p -> return (\e->  Unary     Neg      e   p))           ]
--             , [Prefix ((reservedOp "~"  <|> (reserved "sig">>pos) )
--                                          >>= \p -> return (\e->  Unary     Sign     e   p))           ]
--             , [Prefix ((reservedOp "!"  <|> (reserved "not">>pos) )
--                                          >>= \p -> return (\e->  Unary     Not      e   p))           ]
--             , [Infix  ( reservedOp "**"  >>= \p -> return (\l r->Binary    Pow      l r p)) AssocRight]
--             , [Infix  ( reservedOp "%"   >>= \p -> return (\l r->Binary    Mod      l r p)) AssocLeft ]
--             , [Infix  ( reservedOp "*"   >>= \p -> return (\l r->Binary    Mult     l r p)) AssocLeft ]
--             , [Infix  ( reservedOp "/"   >>= \p -> return (\l r->Binary    Div      l r p)) AssocLeft ]
--             , [Infix  ( reservedOp "+"   >>= \p -> return (\l r->Binary    Plus     l r p)) AssocLeft ]
--             , [Infix  ( reservedOp "-"   >>= \p -> return (\l r->Binary    Minus    l r p)) AssocLeft ]
--             , [Infix  ( reservedOp "<"   >>= \p -> return (\l r->Binary    Less     l r p)) AssocNone ]
--             , [Infix  ( reservedOp "<="  >>= \p -> return (\l r->Binary    Leq      l r p)) AssocNone ]
--             , [Infix  ( reservedOp ">"   >>= \p -> return (\l r->Binary    Greater  l r p)) AssocNone ]
--             , [Infix  ( reservedOp ">="  >>= \p -> return (\l r->Binary    Geq      l r p)) AssocNone ]
--             , [Infix  ( reservedOp "="   >>= \p -> return (\l r->Binary    Equal    l r p)) AssocNone ]
--             , [Infix  ( reservedOp "!="  >>= \p -> return (\l r->Binary    Neq      l r p)) AssocNone ]
--             , [Infix  ((reservedOp "&&" <|> (reserved "and">>pos))
--                                          >>= \p -> return (\l r->Binary    And      l r p)) AssocLeft ]
--             , [Infix  ((reservedOp "||" <|> (reserved "or">>pos))
--                                          >>= \p -> return (\l r->Binary    Or       l r p)) AssocLeft ]
--             ]
-- term = pos >>= \p ->(\s->s p)
--    <$> (Parens <$> parens expression
--    <|> Var    <$> identifier
--    <|> Lit . IntV <$> integer)
--    <?> "expression, identifier or value"
