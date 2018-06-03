" Vim syntax file
" Language: Structured Reversible Language
" Maintainer: Lars Vadgaard
" Latest Revision: 03 June 2018

syn keyword srlConditional  if then else fi from do loop until
syn keyword srlFunctionlike skip swap free init
syn keyword srlOperator     neg sig not and or top empty size
syn keyword srlType         int list

syn match   srlVariable     "[a-zA-Z][a-zA-Z0-9_']*"
syn match   srlOperator     "[!+\-\^\*/><%=&\|\#\?\~]"
syn match   srlInteger      "\d\+"

syn match   srlComment      "\/\/.*$"
syn region  srlComment      start="\/\*" end="\*\/"

hi def link srlConditional  Conditional
hi def link srlVariable	    Underlined
hi def link srlComment      Comment
hi def link srlOperator     Operator
hi def link srlFunctionlike Function
hi def link srlInteger      Number
hi def link srlType	        Type

let b:current_syntax = "srl"
