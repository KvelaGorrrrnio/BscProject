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
                                      , "if"
                                      , "then"
                                      , "else"
                                      , "fi"
                                      , "from"
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

lexer = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
integer       = Token.integer       lexer
colon         = Token.colon         lexer
whiteSpace    = Token.whiteSpace    lexer

-- statements
updateStmt :: Parser Stmt
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

skipStmt :: Parser Stmt
skipStmt = (reserved "skip" <|> reserved ".") >> return Skip

swapStmt :: Parser Stmt
swapStmt = reserved "swap" >> Swap <$> identifier <*> identifier

pushStmt :: Parser Stmt
pushStmt = reserved "push" >> Push <$> identifier <*> identifier

popStmt :: Parser Stmt
popStmt = reserved "pop" >> Pop <$> identifier <*> identifier

-- expressions
expression :: Parser Exp
expression = buildExpressionParser operators term <?> "expression"

operators = [
              [Prefix ((reservedOp "^"  <|> reserved "top"  )
                                         >> return (Unary     Top      ))           ]
            , [Prefix ((reservedOp "#"  <|> reserved "size" )
                                         >> return (Unary     Size     ))           ]
            , [Prefix ((reservedOp "?"  <|> reserved "empty")
                                         >> return (Unary     Empty    ))           ]
            , [Prefix ((reservedOp "-"  <|> reserved "neg"  )
                                         >> return (Unary     Neg      ))           ]
            , [Prefix ((reservedOp "~"  <|> reserved "sig"  )
                                         >> return (Unary     Sign     ))           ]
            , [Prefix ((reservedOp "!"  <|> reserved "not"  )
                                         >> return (Unary     Not      ))           ]
            , [Infix  ( reservedOp "**"  >> return (Binary    Pow      )) AssocRight]
            , [Infix  ( reservedOp "%"   >> return (Binary    Mod      )) AssocLeft ]
            , [Infix  ( reservedOp "*"   >> return (Binary    Mult     )) AssocLeft ]
            , [Infix  ( reservedOp "/"   >> return (Binary    Div      )) AssocLeft ]
            , [Infix  ( reservedOp "+"   >> return (Binary    Plus     )) AssocLeft ]
            , [Infix  ( reservedOp "-"   >> return (Binary    Minus    )) AssocLeft ]
            , [Infix  ( reservedOp "<"   >> return (Binary    Less     )) AssocNone ]
            , [Infix  ( reservedOp "<="  >> return (Binary    Leq      )) AssocNone ]
            , [Infix  ( reservedOp ">"   >> return (Binary    Greater  )) AssocNone ]
            , [Infix  ( reservedOp ">="  >> return (Binary    Geq      )) AssocNone ]
            , [Infix  ( reservedOp "="   >> return (Binary    Equal    )) AssocNone ]
            , [Infix  ( reservedOp "!="  >> return (Binary    Neq      )) AssocNone ]
            , [Infix  ((reservedOp "&&" <|> reserved "and")
                                         >> return (Binary    And      )) AssocLeft ]
            , [Infix  ((reservedOp "||" <|> reserved "or")
                                         >> return (Binary    Or       )) AssocLeft ]
            ]
term = Parens <$> parens expression
   <|> Var    <$> identifier
   <|> Lit . IntV <$> integer
   <?> "expression, identifier or value"
