module Common.Log where

import Common.AST
import Common.JSON
import Common.Error

import Data.List (intercalate)
import Data.Char

-- ===
-- Log
-- ===

type Log = [Message]

logToString :: Log -> String
logToString = intercalate "\n\n" . map show

logToJSON :: Log -> String
logToJSON l = jsonLog $ intercalate ", " $ map jsonMsg l
  where jsonMsg (MsgStmt stmt vtab) = let (l,c) = getStmtPos stmt in
          "{ \"type\" : \"statement\", " ++
          "\"position\" : { \"line\" : "++show l++", \"column\" : "++show c++" }, "++
          "\"statement\" : \"" ++ (escStr.show) stmt ++ "\", " ++
          "\"state\" : " ++ jsonTabL "vartab" vtab ++ " " ++
          "}"
        jsonMsg (MsgCustom st) = "{ \"type\" : \"custom\", " ++
          "\"message\" : " ++ dropWhile (not . isLetter) st ++ " " ++
          "}"
        jsonMsg (MsgError e) = stringify e

data Message = MsgStmt   Stmt VarTab
             | MsgError  Error
             | MsgCustom String
instance Show Message where
  show (MsgStmt s vtab)  =
    "line " ++ (show . fst . getStmtPos) s ++ "\n> " ++
    case s of
      If{}    -> show s
      Until{} -> show s
      Skip{}  -> show s
      _       -> show s ++ "\n" ++ showVTab vtab
  show (MsgError err) = "*** Error: " ++ show err
  show (MsgCustom st) = st
