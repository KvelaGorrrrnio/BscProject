" Vim syntax file
" Language: Reversible Language
" Maintainer: Lars Vadgaard
" Latest Revision: 19 March 2018

" syn keyword rlStatement
syn keyword rlGoTo         goto exit
syn keyword rlComeFrom     entry from
syn keyword rlConditional  if fi
syn keyword rlFunctionlike skip swap
syn keyword rlOperator     not

syn match rlVariable   "[a-zA-Z][a-zA-Z0-9_']*"
syn match rlOperator   "[!+\-\^\*/><%=&\|]"
syn match rlBoolean    "\(\w\|\.\)\@<!\(true\|false\)\(\w\|\.\)\@!"
syn match rlInteger    "\d\+"
syn match rlComment    "\~.*$"
syn match rlBlockStart "[a-zA-Z][a-zA-Z0-9_']*:"

" hi def link rlStatement	   Statement
hi def link rlGoTo	   Structure
hi def link rlComeFrom	   Structure
hi def link rlConditional  Conditional

hi def link rlVariable	   Underlined
hi def link rlComment      Comment
hi def link rlOperator     Operator
hi def link rlFunctionlike Function
hi def link rlBoolean      Boolean
hi def link rlInteger      Number
hi def link rlBlockStart   Label

let b:current_syntax = "rl"

" Options for vi: ts=8 sw=2 sts=2 nowrap noexpandtab ft=vim
