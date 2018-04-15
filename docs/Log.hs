module RL.Log where

type Log = [Message]

data Message
  = ExecMsg Stmt VarTab
  | ErrorMsg Error
