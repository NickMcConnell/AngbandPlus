if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
syn case match

syn keyword riError DEPRECATED
syn match riComment /^#.*$/
syn match riInclude /%:.*$/
syn match riNumber /\d\+/ contained
syn match riBlows /x\d\+/ contained
syn match riWeight /\d\+lbs/ contained
syn match riDice /\d\+d\d\+/ contained 
syn match riDice /\d\+d\d\++\d\+/ contained 
syn match riPercent /\d\+%/ contained 
syn match riFrequency /x\d\+/ contained

syn region riNLine start=/^N:/ end=/$/ contains=riNumber

syn match riBMethod /HIT/ contained
syn match riBMethod /TOUCH/ contained
syn match riBMethod /PUNCH/ contained
syn match riBMethod /KICK/ contained
syn match riBMethod /CLAW/ contained
syn match riBMethod /BITE/ contained
syn match riBMethod /STING/ contained
syn match riBMethod /SLASH/ contained
syn match riBMethod /BUTT/ contained
syn match riBMethod /CRUSH/ contained
syn match riBMethod /ENGULF/ contained
syn match riBMethod /STRIKE/ contained
syn match riBMethod /KNEE/ contained
syn match riBMethod /ELBOW/ contained
syn match riBMethod /UPPERCUT/ contained
syn match riBMethod /DOUBLE_KICK/ contained
syn match riBMethod /ZOMBIE_CLAW/ contained
syn match riBMethod /JUMP_KICK/ contained
syn match riBMethod /GHOUL_TOUCH/ contained
syn match riBMethod /CIRCLE_KICK/ contained
syn match riBMethod /LICH_FIST/ contained
syn match riBMethod /FLYING_KICK/ contained
syn match riBMethod /REAVER_FIST/ contained
syn match riBMethod /HAND_OF_VECNA/ contained
syn match riBMethod /CATS_CLAW/ contained
syn match riBMethod /EAGLES_CLAW/ contained
syn match riBMethod /IRON_FIST/ contained
syn match riBMethod /DRAGON_FIST/ contained
syn match riBMethod /CRUSHING_BLOW/ contained
syn match riBMethod /CHARGE/ contained
syn match riBMethod /CRAWL/ contained
syn match riBMethod /DROOL/ contained
syn match riBMethod /SPIT/ contained
syn match riBMethod /EXPLODE/ contained
syn match riBMethod /GAZE/ contained
syn match riBMethod /WAIL/ contained
syn match riBMethod /SPORE/ contained
syn match riBMethod /BEG/ contained
syn match riBMethod /INSULT/ contained
syn match riBMethod /MOAN/ contained
syn match riBMethod /SHOW/ contained
syn match riBMethod /MONK/ contained
syn region riBMExp matchgroup=riMethodOp start=/HIT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/HIT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/TOUCH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/PUNCH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/BITE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/STING(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/SLASH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/BUTT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/CRUSH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/ENGULF(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/STRIKE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/KNEE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/ELBOW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/UPPERCUT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/DOUBLE_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/ZOMBIE_CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/JUMP_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/GHOUL_TOUCH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/CIRCLE_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/LICH_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/FLYING_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/REAVER_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/HAND_OF_VECNA(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/CATS_CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/EAGLES_CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/IRON_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/DRAGON_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/CRUSHING_BLOW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/CHARGE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/CRAWL(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/DROOL(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/SPIT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/EXPLODE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/GAZE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/WAIL(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/SPORE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/BEG(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/INSULT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/MOAN(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/SHOW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp matchgroup=riMethodOp start=/MONK(/ end=/)/ contains=riWeight,riBlows contained

syn match riBEffect /BLIND/ contained
syn match riBEffect /CONFUSE/ contained
syn match riBEffect /DISEASE/ contained
syn match riBEffect /DISENCHANT/ contained
syn match riBEffect /DRAIN_CHARGES/ contained
syn match riBEffect /EAT_GOLD/ contained
syn match riBEffect /EAT_ITEM/ contained
syn match riBEffect /LOSE_STR/ contained
syn match riBEffect /LOSE_INT/ contained
syn match riBEffect /LOSE_WIS/ contained
syn match riBEffect /LOSE_DEX/ contained
syn match riBEffect /LOSE_CON/ contained
syn match riBEffect /LOSE_CHR/ contained
syn match riBEffect /LOSE_ALL/ contained
syn match riBEffect /PARALYZE/ contained
syn match riBEffect /TERRIFY/ contained
syn match riBEffect /TIME/ contained
syn match riBEffect /SLOW_ANKLE/ contained
syn match riBEffect /STUN_MALE/ contained
syn match riBEffect /DRAINING_TOUCH/ contained

syn region riBExp matchgroup=riOp start=/STUN_MALE(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/HURT(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/DRAIN_CHARGES(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/DRAIN_CHARGES(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/EAT_GOLD(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/EAT_ITEM(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/EAT_FOOD(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/EAT_LITE(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/BLIND(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/CONFUSE(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/TERRIFY(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/PARALYZE(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/LOSE_STR(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/LOSE_INT(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/LOSE_WIS(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/LOSE_DEX(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/LOSE_CON(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/LOSE_CHR(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/LOSE_ALL(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/SHATTER(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/DRAIN_EXP(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/DISEASE(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/EXP_VAMP(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/DRAIN_MANA(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/UNLIFE(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/CUT(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/STUN(/ end=/)/ contains=riDice,riPercent,riNumber contained
syn region riBExp matchgroup=riOp start=/VAMP(/ end=/)/ contains=riDice,riPercent,riNumber contained

syn region riGFExp matchgroup=riOp start=/ACID(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/AMNESIA(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/BRAIN_SMASH(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/CAUSE_1(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/CAUSE_2(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/CAUSE_3(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/CHAOS(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/COLD(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/DAM(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/DARK(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/DISENCHANT(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/ELDRITCH(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/ELEC(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/FIRE(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/GRAVITY(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/HELL_FIRE(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/HOLY_FIRE(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/ICE(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/LITE(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/MIND_BLAST(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/NETHER(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/NEXUS(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/PLASMA(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/POISON(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/POLYMORPH(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/SOUND(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/SHARDS(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/STUN(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
syn region riGFExp matchgroup=riOp start=/TIME(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained

syn region riBLine matchgroup=riLinePrefix start=/^B:/ end=/$/ contains=riBMExp,riBMethod,riBExp,riGFExp,riBEffect,riNumber,riDice

syn region riALine matchgroup=riLinePrefix start=/^A:/ end=/$/ contains=riGFExp,riAEffect

syn region riWLine matchgroup=riLinePrefix start=/^W:/ end=/$/ contains=riNumber
syn region riPLine start=/^P:/ end=/$/

syn match riGFFlag /ACID/ contained
syn match riGFFlag /ELEC/ contained
syn match riGFFlag /FIRE/ contained
syn match riGFFlag /COLD/ contained
syn match riGFFlag /POISON/ contained
syn match riGFFlag /LITE/ contained
syn match riGFFlag /DARK/ contained
syn match riGFFlag /NETHER/ contained
syn match riGFFlag /NEXUS/ contained
syn match riGFFlag /CHAOS/ contained
syn match riGFFlag /DISENCHANT/ contained
syn match riGFFlag /SOUND/ contained
syn match riGFFlag /SHARDS/ contained
syn match riGFFlag /NUKE/ contained
syn match riGFFlag /DISINTEGRATE/ contained
syn match riGFFlag /CONFUSION/ contained
syn match riGFFlag /TIME/ contained
syn match riGFFlag /INERTIA/ contained
syn match riGFFlag /GRAVITY/ contained
syn match riGFFlag /PLASMA/ contained
syn match riGFFlag /FORCE/ contained
syn match riGFFlag /MANA/ contained
syn match riGFFlag /WATER/ contained
syn match riGFFlag /ICE/ contained
syn match riGFFlag /HELL_FIRE/ contained
syn match riGFFlag /HOLY_FIRE/ contained
syn region riSBRFlag start=/BR_/ end=/\>/ contains=riGFFlag contained
syn match riSInnateFlag /ROCKET/ contained
syn match riSInnateFlag /SHOOT/ contained
syn match riSInnateFlag /THROW/ contained
syn region riSBAFlag start=/BA_/ end=/\>/ contains=riGFFlag contained
syn region riSBOFlag start=/BO_/ end=/\>/ contains=riGFFlag contained
syn match riSSpell /MANA_STORM/ contained
syn match riSSpell /HAND_DOOM/ contained
syn match riSSpell /PSY_SPEAR/ contained
syn region riSFreq matchgroup=riOp start=/1_IN_/ end=/\>/ contains=riNumber contained
syn region riSFreq matchgroup=riOp start=/FREQ_/ end=/\>/ contains=riNumber contained
syn region riSLine matchgroup=riLinePrefix start=/^S:/ end=/$/ contains=riSBRFlag,riSFreq,riSInnateFlag,riSBAFlag,riSBOFlag,riSSpell

syn match riObjParm /GOLD/ contained
syn match riObjParm /GOLD_COPPER/ contained
syn match riObjParm /GOLD_SILVER/ contained
syn match riObjParm /GOLD_MITHRIL/ contained
syn match riObjParm /GOLD_ADAMANT/ contained
syn match riObjParm /GOOD/ contained
syn match riObjParm /GREAT/ contained
syn match riObjParm /RING/ contained
syn match riObjParm /SWORD/ contained
syn match riObjParm /TAILORED/ contained
syn match riDropThemeX /WARRIOR/ contained
syn region riDropTheme start=/DROP_/ end=/\>/ contains=riDropThemeX contained
syn region riObjExp matchgroup=riOp start=/OBJ(/ end=/)/ oneline contains=riNumber,riObjParm contained
syn region riEgoExp matchgroup=riOp start=/EGO(/ end=/)/ oneline contains=riNumber contained
syn region riArtExp matchgroup=riOp start=/ART(/ end=/)/ oneline contains=riNumber contained
syn region riOLine matchgroup=riLinePrefix start=/^O:/ end=/$/ contains=riDice,riPercent,riObjExp,riEgoExp,riArtExp,riDropTheme

hi def link riOp Operator
hi def link riMethodOp Identifier
hi def link riLinePrefix Type
hi def link riError Error
hi def link riComment Comment

hi def link riBMethod Identifier
hi def link riBMExp Identifier
hi def link riBEffect Type
hi def link riAEffect Type
hi def link riGFFlag Type
hi def link riObjParm Type
hi def link riFilename Identifier

hi riSBRFlag term=bold ctermfg=Brown
hi riSInnateFlag term=bold ctermfg=Brown
hi riSBAFlag term=bold ctermfg=Red
hi riDropTheme term=bold ctermfg=Blue
hi riDropThemeX term=bold ctermfg=Blue
hi riSSpell term=bold ctermfg=Magenta
hi riSBOFlag term=bold ctermfg=Blue
hi riPLine term=bold ctermfg=Brown

hi def link riInclude PreProc
hi def link riNumber Number
hi def link riBlows Number
hi def link riWeight Number
hi def link riFrequency Number
hi def link riDice Number
hi def link riPercent Number
hi def link riNLine Special
let b:current_syntax = "ri"

