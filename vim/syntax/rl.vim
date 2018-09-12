" Vim syntax file
" Language: Reversible Language
" Maintainer: Lars Vadgaard
" Latest Revision: 03 June 2018

syn keyword rlGoTo         goto exit
syn keyword rlComeFrom     entry from
syn keyword rlConditional  if fi
syn keyword rlFunctionlike skip swap free init reverse
syn keyword rlOperator     neg sig not and or top empty size null
syn keyword rlType         int string list

syn match   rlVariable     "[a-zA-Z][a-zA-Z0-9_']*"
syn match   rlOperator     "[!+\-\^\*/><%=&\|\#\?\~]"
syn match   rlInteger      "\d\+"

syn match   rlComment      "\/\/.*$"
syn region  rlComment      start="\/\*" end="\*\/"
syn region  rlString       start="\"" end="\""

hi def link rlComeFrom     Structure
hi def link rlGoTo         Structure

hi def link rlConditional  Conditional
hi def link rlVariable     Underlined
hi def link rlComment      Comment
hi def link rlOperator     Operator
hi def link rlFunctionlike Function
hi def link rlString       Number
hi def link rlInteger      Number
hi def link rlType         Type

let b:current_syntax = "rl"
