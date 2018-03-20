" Vim syntax file
" Language: Reversible Language
" Maintainer: Lars Vadgaard
" Latest Revision: 19 March 2018

" syn keyword rlStatement
syn keyword rlConditional  if then else fi
syn keyword rlConditional  from do until
syn keyword rlFunctionlike skip swap

syn match rlVariable   "[a-zA-Z][a-zA-Z0-9_']*"
syn match rlOperator   "[+\-\^\*/><%=&\|]"
syn match rlBoolean    "\(\w\|\.\)\@<!\(true\|false\)\(\w\|\.\)\@!"
syn match rlInteger    "\d\+"
syn match rlComment    "\~.*$"

" hi def link rlStatement	   Statement
hi def link rlConditional  Conditional

hi def link rlComment      Comment
hi def link rlOperator     Operator
hi def link rlFunctionlike Function
hi def link rlBoolean      Boolean
hi def link rlInteger      Number

let b:current_syntax = "srl"

" Options for vi: ts=8 sw=2 sts=2 nowrap noexpandtab ft=vim
