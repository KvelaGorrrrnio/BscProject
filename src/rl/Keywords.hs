module Keywords where
import SubToken

matchWord :: String -> Maybe SubToken
matchWord "if"      = Just If
matchWord "else"    = Just Else
matchWord "fi"      = Just Fi
matchWord "from"    = Just From
matchWord "goto"    = Just Goto
matchWord "entry"   = Just Entry
matchWord "exit"    = Just Exit
matchWord ":"       = Just Colon
matchWord _         = Nothing
