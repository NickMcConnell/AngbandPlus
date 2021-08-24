if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
syn case match

syn keyword qOops Rewarded contained
syn keyword qStatus Untaken Taken InProgress Completed Finished Failed FailedDone contained
syn keyword qFeature FLOOR GRASS GRANITE PERMANENT TREE OPEN_DOOR CLOSED_DOOR UP_STAIR DOWN_STAIR contained
syn keyword qFeature SHALLOW_WATER DEEP_WATER SHALLOW_LAVA DEEP_LAVA contained
syn keyword qFeature RUBBLE MOUNTAIN_WALL BROKEN_DOOR contained
syn keyword qFeature QUEST_ENTER ENTRANCE contained
syn match qFeature /BUILDING_[0-9]*/ contained
syn match qFeature /TRAP_[A-Z]*/ contained
syn match qFeature /LOCKED_DOOR_[0-9]*/ contained
syn keyword qExKeyword AND EQU LEQ GEQ NOT MOD contained
syn match qVariable /$[A-Z0-9_]*/ contained
syn match qNumber /\d\+/ contained

syn match qComment /^#.*$/
syn match qInclude /^%:.*$/

syn region qExp matchgroup=qParen start=/\[/ end=/\]/ oneline contains=qExp,qExKeyword,qVariable,qStatus,qNumber,qOops contained
syn region qObjExp matchgroup=qOp start=/OBJ(/ end=/)/ oneline contains=qNumber contained
syn region qEgoExp matchgroup=qOp start=/EGO(/ end=/)/ oneline contains=qNumber contained
syn region qArtExp matchgroup=qOp start=/ART(/ end=/)/ oneline contains=qNumber contained
syn region qMonExp matchgroup=qOp start=/MON(/ end=/)/ oneline contains=qNumber contained
syn region qTrapExp matchgroup=qOp start=/TRAP(/ end=/)/ oneline contains=qNumber,qFeature contained
syn region qCmdExp matchgroup=qOp start=/SCRAMBLE(/ end=/)/ oneline contained

syn region qExpLine matchgroup=qExpPrefix start=/^?:/ end=/$/ contains=qExp
syn region qCmdLine matchgroup=qCmdPrefix start=/^!:/ end=/$/ contains=qCmdExp
syn region qLetterLine matchgroup=qLetterPrefix start=/^L:./ end=/$/ contains=qObjExp,qEgoExp,qArtExp,qMonExp,qTrapExp,qCmdExp,qFeature,qNumber
syn match qMapWall /#\+/ contained
syn match qMapWall /%\+/ contained
syn match qMapWall /;\+/ contained
syn match qMapGrass /-\+/ contained
syn match qMapDoor /+/ contained
syn match qMapTrap /\^/ contained
syn match qMapLoot /\$/ contained
syn match qMapTree /T\+/ contained
syn match qMapWater /\~\+/ contained
syn region qMapLine matchgroup=qMapPrefix start=/^M:/ end=/$/ contains=qMapWall,qMapDoor,qMapTrap,qMapLoot,qMapTree,qMapWater,qMapGrass
syn region qDescLine matchgroup=qDescPrefix start=/^D:/ end=/$/
syn region qRewardLine matchgroup=qRewardPrefix start=/^R:/ end=/$/ contains=qObjExp,qEgoExp,qArtExp,qNumber
syn region qTypeLine start=/^T:/ end=/$/


hi def link qNumber Number
hi def link qStatus Identifier
hi def link qOops Error
hi def link qComment Comment
hi def link qFeature Identifier
hi def link qInclude PreProc
hi def link qVariable Define
hi def link qExKeyword Keyword
hi def link qParen PreProc
hi def link qOp Operator
hi def link qLetterPrefix Type
hi def link qExpPrefix Label
hi def link qMapPrefix PreProc
hi def link qDescPrefix PreProc
hi def link qTypeLine Special
hi def link qCmdPrefix Operator
hi def link qRewardPrefix Special

hi qMapWall term=bold ctermfg=Brown
hi qMapDoor term=bold ctermfg=Brown
hi qMapTrap term=bold ctermfg=Red
hi qMapLoot term=bold ctermfg=Yellow
hi qMapTree term=bold ctermfg=Green
hi qMapWater ctermfg=Blue
hi qMapGrass ctermfg=Green

let b:current_syntax = "q"

