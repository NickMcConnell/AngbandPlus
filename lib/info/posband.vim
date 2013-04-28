" Vim syntax file
" Language:	PosBand data files
" Maintainer:	Alexander Ulyanov
" URL:		http://nowhere.invalid/included-in-posband-sources
" Last Change:	2004 Dec 19

" Quick and dirty highlighting for PosBand lib/edit/*.txt files.
" Actually every file has its own syntax, so this is just a generic support

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Comments
syn match posComment "^#.*"
" Version stamp
syn match posVerStamp "^V:[0-9]\+\.[0-9]\+\.[0-9]\+$"
" Entry beginning
syn match posEntry "^N:"
" Flags or spells
" syn match posFlags "^[FSO]:"
" Descriptions
syn match posDesc "^D:"
" Any other lines
syn match posLine "^[A-CE-MO-UW-Zcrx]:"
" Delimiters
syn match posDelimiter ":\||"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_config_syntax_inits")
  if version < 508
    let did_config_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink posComment	Comment
  HiLink posVerStamp	Special
  HiLink posEntry	Function
  HiLink posDesc	Number
  HiLink posLine	Keyword
  HiLink posDelimiter	Delimiter
  
  delcommand HiLink
endif
			      
let b:current_syntax = "posband"
