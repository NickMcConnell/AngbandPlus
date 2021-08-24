if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
syn case match


syn match riError /^?:0$/
syn match riInclude /%:.*$/
syn match riNumber /\d\+/ contained
syn match riBlows /x\d\+/ contained
syn match riWeight /\d\+lbs/ contained
syn match riDice /\d\+d\d\+/ contained 
syn match riDice /\d\+d\d\++\d\+/ contained 
syn match riPercent /\d\+%/ contained 
syn match riRange /\d\+'/ contained 
syn match riFrequency /x\d\+/ contained
syn match riID /[^: ]\.[a-zA-Z.\- ]\+/ contained
syn match riTODO /\<XXX\>/ contained

syn region riNLine start=/^N:/ end=/$/ contains=riNumber,riID
syn region riTLine start=/^T:/ end=/$/ contains=riNumber,riID
syn region riComment start=/^#/ end=/$/ contains=riTODO
syn region riDLine start=/^D:/ end=/$/

syn match riColor /Dark/ contained
syn match riColor /White/ contained
syn match riColor /Slate/ contained
syn match riColor /Orange/ contained
syn match riColor /Red/ contained
syn match riColor /Green/ contained
syn match riColor /Blue/ contained
syn match riColor /Umber/ contained
syn match riColor /Light Dark/ contained
syn match riColor /Light White/ contained
syn match riColor /Violet/ contained
syn match riColor /Yellow/ contained
syn match riColor /Light Red/ contained
syn match riColor /Light Green/ contained
syn match riColor /Light Blue/ contained
syn match riColor /Light Umber/ contained
syn match riDisplayFlag /CHAR_CLEAR/ contained
syn match riDisplayFlag /ATTR_MULTI/ contained
syn match riDisplayFlag /ATTR_CLEAR/ contained
syn match riDisplayFlag /ATTR_ANY/ contained
syn match riDisplayFlag /ATTR_SEMIRAND/ contained
syn match riDisplayFlag /SHAPECHANGER/ contained
syn match riAlertParm /Ever Vigilant/ contained
syn match riAlertParm /Vigilant/ contained
syn match riAlertParm /Very Observant/ contained
syn match riAlertParm /Observant/ contained
syn match riAlertParm /Fairly Observant/ contained
syn match riAlertParm /Fairly Unseeing/ contained
syn match riAlertParm /Unseeing/ contained
syn match riAlertParm /Overlooks/ contained
syn match riAlertParm /Inattentive/ contained
syn match riAlertParm /Very Inattentive/ contained
syn match riAlertParm /Ignores/ contained
syn region riSpeedExp matchgroup=riOp start=/SPEED(/ end=/)/ contains=riNumber contained
syn region riHPExp matchgroup=riOp start=/HP(/ end=/)/ contains=riNumber,riDice contained
syn region riACExp matchgroup=riOp start=/AC(/ end=/)/ contains=riNumber contained
syn region riAlertExp matchgroup=riOp start=/ALERT(/ end=/)/ contains=riNumber,riRange,riAlertParm contained
syn region riDisplayExp matchgroup=riOp start=/DISPLAY(/ end=/)/ contains=riColor,riDisplayFlag contained
syn region riColorExp matchgroup=riOp start=/COLOR(/ end=/)/ contains=riColor contained
syn match riWildFlag /ONLY/ contained
syn match riWildFlag /ALL/ contained
syn match riWildFlag /TOWN/ contained
syn match riWildFlag /SHORE/ contained
syn match riWildFlag /OCEAN/ contained
syn match riWildFlag /WASTE/ contained
syn match riWildFlag /WOOD/ contained
syn match riWildFlag /VOLCANO/ contained
syn match riWildFlag /MOUNTAIN/ contained
syn match riWildFlag /GRASS/ contained
syn match riWildFlag /SWAMP/ contained
syn region riWildExp matchgroup=riOp start=/WILD(/ end=/)/ contains=riWildFlag contained
syn region riEvolveExp matchgroup=riOp start=/EVOLVE(/ end=/)/ contains=riNumber,riID contained
syn match riAllocFlag /FORCE_DEPTH/ contained
syn match riAllocFlag /ESCORT/ contained
syn match riAllocFlag /NO_QUEST/ contained
syn match riAllocFlag /NO_SUMMON/ contained
syn match riAllocFlag /FIXED_UNIQUE/ contained
syn match riAllocFlag /UNIQUE/ contained
syn match riAllocFlag /UNIQUE2/ contained
syn match riAllocFlag /GUARDIAN/ contained
syn match riAllocFlag /WEB/ contained
syn match riAllocFlag /\<D_[A-Z_]\+/ contained
syn match riAllocFlag /\<W_[A-Z_]\+/ contained
syn match riAllocFlag /\<MAX_\d\+/ contained
syn region riAllocExp matchgroup=riOp start=/ALLOC(/ end=/)/ contains=riAllocFlag contained
syn region riLvlExp matchgroup=riOp start=/LVL(/ end=/)/ contains=riNumber contained
syn region riRarityExp matchgroup=riOp start=/RARITY(/ end=/)/ contains=riNumber contained
syn region riExpExp matchgroup=riOp start=/EXP(/ end=/)/ contains=riNumber contained
syn match riMoveFlag /NEVER/ contained
syn match riMoveFlag /OPEN/ contained
syn match riMoveFlag /BASH/ contained
syn match riMoveFlag /PUSH/ contained
syn match riMoveFlag /TRAMPLE/ contained
syn match riMoveFlag /PICKUP/ contained
syn match riMoveFlag /DESTROY/ contained
syn match riMoveFlag /TUNNEL/ contained
syn match riMoveFlag /PASSWALL/ contained
syn match riMoveFlag /SWIM/ contained
syn match riMoveFlag /FLY/ contained
syn match riMoveFlag /CLIMB/ contained
syn match riMoveFlag /TRUMP/ contained
syn match riMoveFlag /QUICK/ contained
syn match riMoveFlag /WEB/ contained
syn match riMoveFlag /CLEARWEB/ contained
syn match riMoveFlag /\<RAND_\d\+/ contained
syn region riMoveExp matchgroup=riOp start=/MOVE(/ end=/)/ contains=riMoveFlag contained
syn match riGFFlag /ACID/ contained
syn match riGFFlag /ELEC/ contained
syn match riGFFlag /FIRE/ contained
syn match riGFFlag /COLD/ contained
syn match riGFFlag /POIS/ contained
syn match riGFFlag /POISON/ contained
syn match riGFFlag /LIGHT/ contained
syn match riGFFlag /DARK/ contained
syn match riGFFlag /NETHER/ contained
syn match riGFFlag /NEXUS/ contained
syn match riGFFlag /CHAOS/ contained
syn match riGFFlag /DISENCHANT/ contained
syn match riGFFlag /SOUND/ contained
syn match riGFFlag /SHARDS/ contained
syn match riGFFlag /NUKE/ contained
syn match riGFFlag /DISINTEGRATE/ contained
syn match riGFFlag /CONF/ contained
syn match riGFFlag /CONFUSION/ contained
syn match riGFFlag /TIME/ contained
syn match riGFFlag /INERTIA/ contained
syn match riGFFlag /GRAVITY/ contained
syn match riGFFlag /PLASMA/ contained
syn match riGFFlag /FORCE/ contained
syn match riGFFlag /MANA/ contained
syn match riGFFlag /WATER/ contained
syn match riGFFlag /SLOW/ contained
syn match riGFFlag /SLEEP/ contained
syn match riGFFlag /BLIND/ contained
syn match riGFFlag /FEAR/ contained
syn match riGFFlag /STUN/ contained
syn match riGFFlag /TELE/ contained
syn match riGFFlag /TELEPORT/ contained
syn match riGFFlag /ICE/ contained
syn match riGFFlag /HELL_FIRE/ contained
syn match riGFFlag /HOLY_FIRE/ contained
syn region riResistExp matchgroup=riOp start=/RESIST(/ end=/)/ contains=riGFFlag contained
syn region riImmuneExp matchgroup=riOp start=/IMMUNE(/ end=/)/ contains=riGFFlag contained
syn region riVulnExp matchgroup=riOp start=/VULN(/ end=/)/ contains=riGFFlag contained
syn match riPosFlag /CORPSE/ contained
syn match riPosFlag /SKELETON/ contained
syn match riPosFlag /GAIN_AC/ contained
syn match riPosFlag /SEE_INVIS/ contained
syn match riPosFlag /HOLD_LIFE/ contained
syn match riPosFlag /TELEPATHY/ contained
syn match riPosFlag /DISABLED/ contained
syn match riPosFlag /SUST_STR/ contained
syn match riPosFlag /SUST_INT/ contained
syn match riPosFlag /SUST_WIS/ contained
syn match riPosFlag /SUST_DEX/ contained
syn match riPosFlag /SUST_CON/ contained
syn match riPosFlag /SUST_CHR/ contained
syn match riPosFlag /SUST_ALL/ contained
syn match riPosFlag /BACKSTAB/ contained
syn region riPosExp matchgroup=riOp start=/POS(/ end=/)/ contains=riPosFlag contained
syn match riAlign /Lawful/ contained
syn match riAlign /Very Good/ contained
syn match riAlign /Good/ contained
syn match riAlign /Neutral Good/ contained
syn match riAlign /Neutral/ contained
syn match riAlign /Neutral Evil/ contained
syn match riAlign /Evil/ contained
syn match riAlign /Very Evil/ contained
syn match riAlign /Chaotic/ contained
syn region riAlignExp matchgroup=riOp start=/ALIGN(/ end=/)/ contains=riAlign contained
syn match riKindFlag /ORC/ contained
syn match riKindFlag /TROLL/ contained
syn match riKindFlag /GIANT/ contained
syn match riKindFlag /DRAGON/ contained
syn match riKindFlag /DEMON/ contained
syn match riKindFlag /UNDEAD/ contained
syn match riKindFlag /ANIMAL/ contained
syn match riKindFlag /HUMAN/ contained
syn match riKindFlag /ELF/ contained
syn match riKindFlag /DARK_ELF/ contained
syn match riKindFlag /HOBBIT/ contained
syn match riKindFlag /DWARF/ contained
syn match riKindFlag /AMBERITE/ contained
syn match riKindFlag /THIEF/ contained
syn match riKindFlag /KNIGHT/ contained
syn match riKindFlag /OLYMPIAN/ contained
syn match riKindFlag /NONLIVING/ contained
syn match riKindFlag /AQUATIC/ contained
syn match riKindFlag /NAZGUL/ contained
syn match riKindFlag /HORROR/ contained
syn region riKindExp matchgroup=riOp start=/KIND(/ end=/)/ contains=riKindFlag contained
syn match riAttrFlag /MALE/ contained
syn match riAttrFlag /FEMALE/ contained
syn match riAttrFlag /SMART/ contained
syn match riAttrFlag /STUPID/ contained
syn match riAttrFlag /WEIRD_MIND/ contained
syn match riAttrFlag /EMPTY_MIND/ contained
syn match riAttrFlag /COLD_BLOOD/ contained
syn match riAttrFlag /FRIENDLY/ contained
syn match riAttrFlag /RIDING/ contained
syn match riAttrFlag /KILL_EXP/ contained
syn match riAttrFlag /IM_ILLUSION/ contained
syn match riAttrFlag /DEPRECATED/ contained
syn region riAttrExp matchgroup=riOp start=/ATTR(/ end=/)/ contains=riAttrFlag contained
syn region riLiteExp matchgroup=riOp start=/LIGHT(/ end=/)/ contains=riNumber contained
syn region riDarkExp matchgroup=riOp start=/DARK(/ end=/)/ contains=riNumber contained
syn match riAbilityFlag /SPEAK/ contained
syn match riAbilityFlag /REFLECT/ contained
syn match riAbilityFlag /INVIS/ contained
syn match riAbilityFlag /MULTIPLY/ contained
syn match riAbilityFlag /REGEN/ contained
syn match riAbilityFlag /REVENGE/ contained
syn match riAbilityFlag /FEAR/ contained
syn region riAbilityExp matchgroup=riOp start=/CAN(/ end=/)/ contains=riAbilityFlag contained
syn region riILine matchgroup=riLinePrefix start=/^I:/ end=/$/ contains=riSpeedExp,riHPExp,riACExp,riAlertExp,riDisplayExp,riColorExp,riEvolveExp,riWildExp,riAllocExp,riLvlExp,riRarityExp,riExpExp,riMoveExp,riResistExp,riImmuneExp,riVulnExp,riPosExp,riAlignExp,riKindExp,riAttrExp,riLiteExp,riDarkExp,riAbilityExp

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
syn region riBMExp start=/HIT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/HIT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/TOUCH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/PUNCH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/BITE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/STING(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/SLASH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/BUTT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/CRUSH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/ENGULF(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/STRIKE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/KNEE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/ELBOW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/UPPERCUT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/DOUBLE_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/ZOMBIE_CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/JUMP_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/GHOUL_TOUCH(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/CIRCLE_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/LICH_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/FLYING_KICK(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/REAVER_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/HAND_OF_VECNA(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/CATS_CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/EAGLES_CLAW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/IRON_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/DRAGON_FIST(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/CRUSHING_BLOW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/CHARGE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/CRAWL(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/DROOL(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/SPIT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/EXPLODE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/GAZE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/WAIL(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/SPORE(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/BEG(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/INSULT(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/MOAN(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/SHOW(/ end=/)/ contains=riWeight,riBlows contained
syn region riBMExp start=/MONK(/ end=/)/ contains=riWeight,riBlows contained

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
syn region riGFExp matchgroup=riOp start=/LIGHT(/ end=/)/ contains=riDice,riPercent,riNumber,riFrequency contained
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

syn region riBLine start=/^B:/ end=/$/ contains=riBMExp,riBMethod,riBExp,riGFExp,riBEffect,riNumber,riDice

syn region riALine matchgroup=riLinePrefix start=/^A:/ end=/$/ contains=riGFExp,riAEffect

syn region riPLine start=/^P:/ end=/$/

syn match riSInnateFlag /ROCKET/ contained
syn match riSInnateFlag /SHOOT/ contained
syn match riSInnateFlag /THROW/ contained
syn match riSSpell /MANA_STORM/ contained
syn match riSSpell /HAND_DOOM/ contained
syn match riSSpell /PSY_SPEAR/ contained
syn region riSFreq matchgroup=riOp start=/1_IN_/ end=/\>/ contains=riNumber contained
syn region riSFreq matchgroup=riOp start=/FREQ_/ end=/\>/ contains=riNumber contained
syn region riSLine start=/^S:/ end=/$/ contains=riSBRFlag,riSFreq,riSInnateFlag,riSBAFlag,riSBOFlag,riSSpell

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
syn region riArtExp matchgroup=riOp start=/ART(/ end=/)/ oneline contains=riID contained
syn region riOLine matchgroup=riLinePrefix start=/^O:/ end=/$/ contains=riDice,riPercent,riObjExp,riEgoExp,riArtExp,riDropTheme

syn match riMonParm /STOP/ contained
syn match riMonParm /NO_UNIQUE/ contained
syn match riMonParm /NO_GROUP/ contained
syn match riMonParm /NO_SLEEP/ contained
syn match riMonParm /FRIENDLY/ contained
syn match riMonParm /HASTE/ contained
syn region riDepthParm matchgroup=riOp start=/DEPTH+/ end=/\>/ contains=riNumber contained
syn region riMonExp matchgroup=riOp start=/MON(/ end=/)/ contains=riPercent,riID,riMonParm,riDepthParm contained
syn region riMLine matchgroup=riLinePrefix start=/^M:/ end=/$/ contains=riDice,riPercent,riMonExp
syn region riKLine matchgroup=riLinePrefix start=/^K:/ end=/$/ contains=riDice,riPercent,riMonExp


hi def link riOp Operator
hi def link riMethodOp Identifier
hi def link riLinePrefix Type
hi def link riError Error
hi def link riTODO Error
hi def link riComment Comment

hi riBMethod ctermfg=Red
hi riBMExp ctermfg=Red
hi def link riID Identifier
hi def link riBEffect Type
hi def link riAEffect Type
hi def link riGFFlag PreProc
hi def link riPosFlag PreProc
hi def link riAlign Number
hi def link riWildFlag PreProc
hi def link riKindFlag PreProc
hi def link riAttrFlag PreProc
hi def link riAbilityFlag PreProc
hi def link riDisplayFlag PreProc
hi def link riAllocFlag PreProc
hi def link riMoveFlag PreProc
hi def link riObjParm Type
hi def link riMonParm Type
hi def link riAlertParm Number
hi def link riColor Number
hi def link riFilename Identifier

hi riSInnateFlag term=bold ctermfg=Brown
hi riDropTheme term=bold ctermfg=Blue
hi riDropThemeX term=bold ctermfg=Blue
hi riSSpell term=bold ctermfg=Magenta
hi riPLine term=bold ctermfg=DarkGray
hi riTLine term=bold ctermfg=Magenta
hi riBLine ctermfg=Red
hi riDLine ctermfg=DarkGray
hi riSLine term=bold ctermfg=Blue

hi def link riInclude PreProc
hi def link riNumber Number
hi def link riBlows Number
hi def link riWeight Number
hi def link riFrequency Number
hi def link riDice Number
hi def link riPercent Number
hi def link riRange Number
hi def link riNLine Special
let b:current_syntax = "ri"

