-- Tokens for LR and SLR
module Token where
import SubToken

data Token

    -- Ops
    = Plus
    | Minus
    | Hat
    | Times
    | Divide
    | Eq

    -- Brackets
    | Lbrack
    | Rbrack
    -- Parentheses
    | Lpar
    | Rpar

    -- Keywords
    | Top
    | Empty
    --
    | Push
    | Pop
    | Skip

    -- Variables
    | Num Int
    | Var String
    | Sub SubToken
    deriving Show
