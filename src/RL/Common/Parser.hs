module Common.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import qualified Text.ParserCombinators.Parsec.Token as Token

import Common

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
                                         >> return (LstExp     Top      ))           ]
            , [Prefix ((reservedOp "#"  <|> reserved "size" )
                                         >> return (LstExp     Size     ))           ]
            , [Prefix ((reservedOp "?"  <|> reserved "empty")
                                         >> return (LstExp     Empty    ))           ]
            , [Prefix ((reservedOp "-"  <|> reserved "neg"  )
                                         >> return (AUnary     Neg      ))           ]
            , [Prefix ((reservedOp "~"  <|> reserved "sig"  )
                                         >> return (AUnary     Sign     ))           ]
            , [Prefix ((reservedOp "!"  <|> reserved "not"  )
                                         >> return Not                   )           ]
            , [Infix  ( reservedOp "**"  >> return (ABinary    Pow      )) AssocRight]
            , [Infix  ( reservedOp "%"   >> return (DivBinary  Mod      )) AssocLeft ]
            , [Infix  ( reservedOp "*"   >> return (ABinary    Mult     )) AssocLeft ]
            , [Infix  ( reservedOp "/"   >> return (DivBinary  Div      )) AssocLeft ]
            , [Infix  ( reservedOp "+"   >> return (ABinary    Plus     )) AssocLeft ]
            , [Infix  ( reservedOp "-"   >> return (ABinary    Minus    )) AssocLeft ]
            , [Infix  ( reservedOp "<"   >> return (Relational Less     )) AssocNone ]
            , [Infix  ( reservedOp "<="  >> return (Relational LEq      )) AssocNone ]
            , [Infix  ( reservedOp ">"   >> return (Relational Greater  )) AssocNone ]
            , [Infix  ( reservedOp ">="  >> return (Relational GEq      )) AssocNone ]
            , [Infix  ( reservedOp "="   >> return (Relational Eq       )) AssocNone ]
            , [Infix  ( reservedOp "!="  >> return (Relational NEq      )) AssocNone ]
            , [Infix  ((reservedOp "&&" <|> reserved "and")
                                         >> return (LBinary    And      )) AssocLeft ]
            , [Infix  ((reservedOp "||" <|> reserved "or")
                                         >> return (LBinary    Or       )) AssocLeft ]
            ]
term = Parens <$> parens expression
   <|> Var    <$> identifier
   <|> Lit . IntV <$> integer
   <?> "expression, identifier or value"
