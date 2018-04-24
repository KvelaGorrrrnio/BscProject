module Common.JSON
( JSON
, stringify
, escStr
, esc
, jsonError
, jsonLog
, jsonTab
, jsonTabL
, jsonCode
, getStmtPos
) where

import Common.AST
import Data.List (intercalate)
import qualified Data.HashMap.Strict as M

class JSON a where
  stringify :: a -> String

escStr :: String -> String
escStr str = foldl (\a c-> a++esc c) "" str

esc :: Char -> String
esc '\\' = "\\\\"
esc '\"' = "\\\""
esc '\n' = "\\n"
esc c    = [c]

-- Error
jsonError :: String -> Pos -> String
jsonError msg (l,c) = "{ "++
  "\"type\" : \"error\", "++
  "\"position\" : { \"line\" : "++show l++", \"column\" : "++show c++" }, "++
  "\"message\" : \""++escStr msg++"\" "++
  "}"

-- Log
jsonLog :: String -> String
jsonLog l =  "{ "++
  "\"type\" : \"log\", "++
  "\"log\" : ["++l++"] "++
  "}"

-- JSON
jsonTab :: Show a => String -> M.HashMap Id a -> String
jsonTab t tab = "{ " ++
  "\"type\" : \"" ++ t ++ "\", " ++
  "\"table\" : [" ++ intercalate ", " (map f (M.toList tab)) ++ "] " ++
  "}"
  where f (n,t) = "{ \"id\" : \"" ++ n ++ "\", \"value\" : \"" ++ show t ++ "\" }"

jsonTabL :: Show a => String -> [(Id,a)] -> String
jsonTabL t tab = jsonTab t $ M.fromList tab

-- Code
jsonCode :: String -> String
jsonCode c = "{ \"type\" : \"code\", " ++
  "\"code\" : \"" ++ escStr c ++ "\"" ++
  " }"

-- Helpers
getStmtPos :: Stmt -> Pos
getStmtPos (Update _ _ _ p) = p
getStmtPos (Push _ _ p)     = p
getStmtPos (Pop _ _ p)      = p
getStmtPos (Swap _ _ p)     = p
getStmtPos (Skip p)         = p
getStmtPos (If _ _ _ _ p)   = p
getStmtPos (Until _ _ _ p)  = p
