module Common.JSON
( JSON
, stringify
, escStr
, esc
, jsonError
, jsonLog
, jsonTab
--, jsonTabL
, jsonCode
, getStmtPos
) where

import Common.AST
import Data.List (intercalate)
import qualified Data.HashMap.Strict as M

class JSON a where
  stringify :: a -> String

escStr :: String -> String
escStr = concatMap esc

esc :: Char -> String
esc '\\' = "\\\\"
esc '\"' = "\\\""
esc '\n' = "\\n"
esc c    = [c]

-- Error
jsonError :: String -> Pos -> String
jsonError msg (l,c) =
     "{ \"type\" : \"error\", "
  ++ "\"position\" : { \"line\" : " ++ show l ++ ", \"column\" : " ++ show c ++ " }, "
  ++ "\"message\" : \"" ++ escStr msg ++ "\" }"

-- Log
jsonLog :: String -> String
jsonLog l =
     "{ \"type\" : \"log\", "
  ++ "\"log\" : ["++l++"] }"

-- JSON
-- jsonTab :: Show a => String -> M.HashMap Id a -> String
-- jsonTab t = jsonTabL t . M.toList

jsonTab :: Show a => String -> M.HashMap String a -> String
jsonTab t tab =
     "{ \"type\" : \"" ++ t ++ "\", "
  ++ "\"table\" : [" ++ (intercalate ", " . map f . sort' . M.toList) tab ++ "] }"
  where f (n,t) = "{ \"id\" : \"" ++ n ++ "\", \"value\" : \"" ++ show t ++ "\" }"

-- Code
jsonCode :: String -> String
jsonCode c =
     "{ \"type\" : \"code\", "
  ++ "\"code\" : \"" ++ escStr c ++ "\" }"
