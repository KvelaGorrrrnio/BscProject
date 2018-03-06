-- Lexer for SLR
module Lexer where
import Token
--import SubToken
import Keywords
import Data.Char
-- import Text.Regex.Posix

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('+':s) = Plus   : tokenize s
tokenize ('-':s) = Minus  : tokenize s
tokenize ('^':s) = Hat    : tokenize s
tokenize ('*':s) = Times  : tokenize s
tokenize ('/':s) = Divide : tokenize s
tokenize ('=':s) = Eq     : tokenize s
tokenize ('(':s) = Lpar   : tokenize s
tokenize (')':s) = Rpar   : tokenize s
tokenize ('[':s) = Lbrack : tokenize s
tokenize (']':s) = Lbrack : tokenize s
tokenize ('~':s) = -- Comments
    let (_, rest) = span (/= '\n') s
        in tokenize rest
tokenize (c:s)

    | isSpace c = tokenize s

    | isDigit c =
        let (n, rest) = span isDigit (c:s)
            in Num (read n :: Int) : tokenize rest
    | isAlpha c = do
        let (v, rest) = span isAlphaNum (c:s)
        case v of
            "top"   -> Top            : tokenize rest
            "empty" -> Empty          : tokenize rest
            "push"  -> Push           : tokenize rest
            "pop"   -> Pop            : tokenize rest
            "skip"  -> Skip           : tokenize rest
            word    -> case matchWord word of
                Just token  -> Sub token  : tokenize rest
                Nothing     -> Var word   : tokenize rest

    | isPunctuation c = case matchWord [c] of
        Just token  -> Sub token  : tokenize s
        Nothing     -> error $"Unknown symbol encountered '"++[c]++"'."

    | otherwise = error $"Unknown symbol encountered '"++[c]++"'."
