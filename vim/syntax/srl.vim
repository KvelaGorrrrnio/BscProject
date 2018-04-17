" Vim syntax file
" Language: Reversible Language
" Maintainer: Lars Vadgaard
" Latest Revision: 19 March 2018

" syn keyword rlStatement
syn keyword rlConditional  if then else fi from do until
syn keyword rlFunctionlike skip swap
syn keyword rlOperator     neg sig not and or top empty size

syn match rlVariable   "[a-zA-Z][a-zA-Z0-9_']*"
syn match rlOperator   "[!+\-\^\*/><%=&\|\#\?\~]"
syn match rlInteger    "\d\+"
syn match rlComment    "\/\/.*$"

syn region rlComment start="\/\*"    end="\*\/"

hi def link rlConditional  Conditional

hi def link rlVariable	   Underlined
hi def link rlComment      Comment
hi def link rlOperator     Operator
hi def link rlFunctionlike Function
hi def link rlInteger      Number

let b:current_syntax = "srl"
