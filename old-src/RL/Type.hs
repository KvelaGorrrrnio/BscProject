module RL.Type
( check
) where

import RL.AST AST
import RL.Error ( RLError, TypeError )

check :: Either RLError AST -> Either RLError AST
check Left e    = Left e
check Right AST [] a  =
