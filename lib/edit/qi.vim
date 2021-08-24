if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
syn case match

syn match qiComment /^#.*$/
syn match qiInclude /%:.*$/
syn match qiNumber /\d\+/ contained

syn region qiNLine start=/^N:/ end=/$/ contains=qiNumber

syn keyword qiFlag RETAKE RANDOM TOWN GENERATE contained
syn region qiTLine matchgroup=qiLinePrefix start=/^T:/ end=/$/ contains=qiFlag

syn region qiWLine matchgroup=qiLinePrefix start=/^W:/ end=/$/

syn match qiFilename /q_[a-zA-Z0-9_]*\.txt/ contained
syn region qiFLine matchgroup=qiLinePrefix start=/^F:/ end=/$/ contains=qiFilename

syn region qiGExp matchgroup=qiOp start=/KILL(/ end=/)/ contains=qiNumber contained
syn region qiGExp matchgroup=qiOp start=/FIND(/ end=/)/ contained
syn region qiGLine matchgroup=qiLinePrefix start=/^G:/ end=/$/ contains=qiGExp

hi def link qiOp Operator
hi def link qiLinePrefix Type

hi def link qiComment Comment
hi def link qiFlag Identifier
hi def link qiFilename Identifier
hi def link qiInclude PreProc
hi def link qiNumber Number
hi def link qiNLine Special
let b:current_syntax = "qi"

