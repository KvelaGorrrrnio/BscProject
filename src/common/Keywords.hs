module Keywords where
import SubToken

matchWord :: String -> Maybe SubToken
matchWord "if"      = Just If
matchWord "then"    = Just Then
matchWord "else"    = Just Else
matchWord "fi"      = Just Fi
matchWord "from"    = Just From
matchWord "until"   = Just Until
matchWord _         = Nothing
