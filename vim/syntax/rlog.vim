
" Vim syntax file
" Language: Reversible Language
" Maintainer: Lars Vadgaard
" Latest Revision: 19 March 2018

" syn keyword rlStatement
syn keyword rlGoTo         goto
syn keyword rlConditional  if fi
syn keyword rlStmt         skip swap
syn keyword rlOperator     not
syn keyword rlStmtMrker    exit

syn match rlVariable       "[a-zA-Z][a-zA-Z0-9_']*"
syn match rlStmtMrker      ">"
syn match rlOperator       "[!+\-\^\*/><%=&\|]"
syn match rlInteger        "\d\+"
syn match rlNewBlock       ">>.*$"
syn match rlError          "\*\*\* Error:.*$"

" hi def link rlStatement	   Statement
hi def link rlGoTo	       Structure
hi def link rlConditional  Conditional

hi def link rlNewBlock     Function
hi def link rlStmtMrker    Function
hi def link rlError        Error
hi def link rlVariable	   Underlined
hi def link rlStmt         Statement
hi def link rlOperator     Operator
hi def link rlInteger      Number

let b:current_syntax = "rlog"
