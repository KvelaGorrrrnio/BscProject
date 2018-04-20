module Common.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import qualified Text.ParserCombinators.Parsec.Token as Token

import Common.AST

-- specifying the language
languageDef =
  emptyDef { Token.commentStart     = "/*"
           , Token.commentEnd       = "*/"
           , Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
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
           , Token.reservedOpNames  = [ "+=", "-=", "^=", "*=", "/="
                                      , "+", "-", "^", "*", "**", "/", "%"
                                      , "=", "!=", "<", "<=", ">", ">="
                                      , "&&", "||"
                                      ]
           }

-- get position
pos :: Parser Pos
pos = getPosition >>= \s->return (sourceLine s,sourceColumn s)

lexer = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp n  = Token.reservedOp    lexer n >> pos
parens        = Token.parens        lexer
integer       = Token.integer       lexer
colon         = Token.colon         lexer
whiteSpace    = Token.whiteSpace    lexer

-- statements
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

-- expressions
expression :: Parser Exp
expression = buildExpressionParser operators term <?> "expression"

operators = [
              [Prefix ((reservedOp "^"  <|> (reserved "top">>pos) )
                                         >>= \p -> return (\e->  Unary     Top      e   p))           ]
            , [Prefix ((reservedOp "#"  <|> (reserved "size">>pos) )
                                         >>= \p -> return (\e->  Unary     Size     e   p))           ]
            , [Prefix ((reservedOp "?"  <|> (reserved "empty">>pos) )
                                         >>= \p -> return (\e->  Unary     Empty    e   p))           ]
            , [Prefix ((reservedOp "-"  <|> (reserved "neg">>pos)  )
                                         >>= \p -> return (\e->  Unary     Neg      e   p))           ]
            , [Prefix ((reservedOp "~"  <|> (reserved "sig">>pos) )
                                         >>= \p -> return (\e->  Unary     Sign     e   p))           ]
            , [Prefix ((reservedOp "!"  <|> (reserved "not">>pos) )
                                         >>= \p -> return (\e->  Unary     Not      e   p))           ]
            , [Infix  ( reservedOp "**"  >>= \p -> return (\l r->Binary    Pow      l r p)) AssocRight]
            , [Infix  ( reservedOp "%"   >>= \p -> return (\l r->Binary    Mod      l r p)) AssocLeft ]
            , [Infix  ( reservedOp "*"   >>= \p -> return (\l r->Binary    Mult     l r p)) AssocLeft ]
            , [Infix  ( reservedOp "/"   >>= \p -> return (\l r->Binary    Div      l r p)) AssocLeft ]
            , [Infix  ( reservedOp "+"   >>= \p -> return (\l r->Binary    Plus     l r p)) AssocLeft ]
            , [Infix  ( reservedOp "-"   >>= \p -> return (\l r->Binary    Minus    l r p)) AssocLeft ]
            , [Infix  ( reservedOp "<"   >>= \p -> return (\l r->Binary    Less     l r p)) AssocNone ]
            , [Infix  ( reservedOp "<="  >>= \p -> return (\l r->Binary    Leq      l r p)) AssocNone ]
            , [Infix  ( reservedOp ">"   >>= \p -> return (\l r->Binary    Greater  l r p)) AssocNone ]
            , [Infix  ( reservedOp ">="  >>= \p -> return (\l r->Binary    Geq      l r p)) AssocNone ]
            , [Infix  ( reservedOp "="   >>= \p -> return (\l r->Binary    Equal    l r p)) AssocNone ]
            , [Infix  ( reservedOp "!="  >>= \p -> return (\l r->Binary    Neq      l r p)) AssocNone ]
            , [Infix  ((reservedOp "&&" <|> (reserved "and">>pos))
                                         >>= \p -> return (\l r->Binary    And      l r p)) AssocLeft ]
            , [Infix  ((reservedOp "||" <|> (reserved "or">>pos))
                                         >>= \p -> return (\l r->Binary    Or       l r p)) AssocLeft ]
            ]
term = pos >>= \p ->(\s->s p)
   <$> (Parens <$> parens expression
   <|> Var    <$> identifier
   <|> Lit . IntV <$> integer)
   <?> "expression, identifier or value"
