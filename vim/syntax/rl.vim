" Vim syntax file
" Language: Reversible Language
" Maintainer: Lars Vadgaard
" Latest Revision: 19 March 2018

" syn keyword rlStatement
syn keyword rlGoTo         goto exit
syn keyword rlComeFrom     entry from
syn keyword rlConditional  if fi
syn keyword rlFunctionlike skip swap free init
syn keyword rlOperator     neg sig not and or top empty size null
syn keyword rlType         int list

syn match rlVariable   "[a-zA-Z][a-zA-Z0-9_']*"
syn match rlOperator   "[!+\-\^\*/><%=&\|\#\?\~]"
syn match rlInteger    "\d\+"
syn match rlBlockStart "[a-zA-Z][a-zA-Z0-9_']*:"

syn match rlComment    "\/\/.*$"
syn region rlComment start="\/\*"    end="\*\/"

hi def link rlBlockStart   Label
hi def link rlComeFrom	   Structure
hi def link rlGoTo	       Structure

hi def link rlConditional  Conditional
hi def link rlVariable	   Underlined
hi def link rlComment      Comment
hi def link rlOperator     Operator
hi def link rlFunctionlike Function
hi def link rlInteger      Number
hi def link rlType	       Type

let b:current_syntax = "rl"
