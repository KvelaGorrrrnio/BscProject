module Common.Log where

import Common.AST
import Common.JSON
import Common.Error

import Data.List (intercalate)

-- ===
-- Log
-- ===

data Log = Log VarTab [Message]

logToString :: Log -> String
logToString (Log _ ms) = intercalate "\n\n" . map show $ ms

instance JSON Log where
  stringify (Log vt ms) = 
    "{ \"type\" : \"log\", "
    ++ "\"state\" : " ++ jsonTab "vartab" vt ++ ", "
    ++ "\"table\" : [" ++ msgsToJSON ms ++ "]"
    ++ " }"
msgsToJSON :: [Message] -> String
msgsToJSON ms = intercalate ", " $ map jsonMsg ms
  where jsonMsg (MsgStmt stmt vtab) =
          let (l,c) = getStmtPos stmt in
             "{ \"type\" : \"statement\", "
          ++ "\"position\" : { \"line\" : "++ show l ++ ", \"column\" : " ++ show c ++ " }, "
          ++ "\"statement\" : \"" ++ (escStr . show) stmt ++ "\", "
          ++ "\"state\" : " ++ jsonTab "vartab" vtab ++ " }"
        jsonMsg (MsgError e) = stringify e

data Message = MsgStmt   Stmt VarTab
             | MsgError  Error
instance Show Message where
  show (MsgStmt s vtab)  =
    "line " ++ (show . fst . getStmtPos) s ++ "\n> " ++
    case s of
      Skip{}  -> show s
      _       -> show s ++ "\n" ++ showTab vtab
  show (MsgError err) = "*** Error: " ++ show err
