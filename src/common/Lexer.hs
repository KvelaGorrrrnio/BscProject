-- Lexer for SLR
module Lexer where
import Token
--import SubToken
import Keywords
import Data.Char
-- import Text.Regex.Posix

lexer :: String -> [Token]
lexer [] = []
lexer ('+':s) = Plus   : lexer s
lexer ('-':s) = Minus  : lexer s
lexer ('^':s) = Bind   : lexer s
lexer ('*':s) = Times  : lexer s
lexer ('/':s) = Divide : lexer s
lexer ('=':s) = Eq     : lexer s
lexer ('(':s) = Lpar   : lexer s
lexer (')':s) = Rpar   : lexer s
lexer ('[':s) = Lbrack : lexer s
lexer (']':s) = Lbrack : lexer s
lexer ('~':s) = -- Comments
    let (_, rest) = span (/= '\n') s
        in lexer rest
lexer (c:s)

    | isSpace c = lexer s

    | isDigit c =
        let (n, rest) = span isDigit (c:s)
            in Num (read n :: Int) : lexer rest

    | isAlpha c = do
        let (v, rest) = span isAlphaNum (c:s)
        case v of
            "top"   -> Top            : lexer rest
            "empty" -> Empty          : lexer rest
            "push"  -> Push           : lexer rest
            "pop"   -> Pop            : lexer rest
            "skip"  -> Skip           : lexer rest
            word    ->
                case matchWord word of
                    Just token  -> Sub token  : lexer rest
                    Nothing     -> Var word   : lexer rest

    | otherwise = error "hej"
