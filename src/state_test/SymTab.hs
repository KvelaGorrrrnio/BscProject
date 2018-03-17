module SymTab where
import Control.Monad.State
import AST

type SymTab a = [(String, a)]
type VarTab = SymTab Value
type LabTab = SymTab Int

update :: String -> Value -> State VarTab ()
update name value =
  state $ \st -> ((),update' name value st)
  where
    update' name vn ((n,vo):bs)
      | n == name  = (n,vn) : bs
      | otherwise  = (n,vo) : update' name vn bs
    update' name vn [] = [(name,vn)]

lookupVar :: String -> State VarTab (Maybe Value)
lookupVar name = state $ \st -> (lookup name st, st)

labelDiff :: Label -> Label -> LabTab -> Int
labelDiff (Label lf) (Label lt) ltab =
  case (lookup lf ltab, lookup lt ltab) of
    (Just f, Just t)  -> t-f
    _                 -> error "Label not defined"

bindLabels :: AST -> LabTab
bindLabels = bindLabels' 0
  where
  bindLabels' n ast =
    case ast of
      AST _ (Block (Label l) _ _ _:_) ->
        (l, n):bindLabels' (n+1) (AST.next ast)
      _ -> []
