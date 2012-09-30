/* monsters.c: monster definitions ° 161   © 169 
      æ190
Ù
≠Á6ÎáàÏÌÓÔ“‘’ÙÎıˆPÿ◊.õú
       
    ÂÊËÈÍÎÏÌÓÔÒÚÛ∂ıˆ˜¯˘˙˚¸˝ ˛Üáàâçèêëíùûü°•®©™≥¥∏äŸÉÖæ›‡ﬁÑ·‚„‰
    ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈√«»… ÀÃÕŒœ–—»… ÀÃÕŒœ–—±≤Ωªæø¿¡¬√ƒ≈∆«»… À
    vÜáàâäçëíëîïóòôòôöúùûü†°ò¢ß£§•¶ß®©™´¨≠ÆØ∞±≤≤≤≥¥µ∂÷Ò˜¯˘˚¸˝˛
    ˛˝¸˚˙˘¯˜ˆııÙÛÚÒ
 ∫Õ…ª»ºŒ ÀπÃãå“é–ﬂ±≤ìîïñóòôöõú÷◊ÿ†£¢—§⁄¶ß€‹ø´¨ÆØ∞’6‘∆«µ∂∑œ”Ä¿¡Ω¬√Åƒ≈Ç
    
    öôõ

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "constant.h"
#include "config.h"
#include "types.h"
#include "monster.h"

/*
		Attack types:
		1       Normal attack
		2       Poison Strength
		3       Confusion attack
		4       Fear attack
		5       Fire attack
		6       Acid attack
		7       Cold attack
		8       Lightning attack
		9       Corrosion attack
		10      Blindness attack
		11      Paralysis attack
		12      Steal Money
		13      Steal Object
		14      Poison
		15      Lose dexterity
		16      Lose constitution
		17      Lose intelligence
		18      Lose wisdom
		19      Lose experience
		20      Aggravation
		21      Disenchant
		22      Eats food
		23      Eat light
		24      Energy drain from pack
		25      Drain all stats
		99      Blank

		Attack descriptions:
		1       hits you.
		2       bites you.
		3       claws you.
		4       stings you.
		5       touches you.
		6       kicks you.
		7       gazes at you.
		8       breathes on you.
		9       spits on you.
		10      makes a horrible wail.
		11      embraces you.
		12      crawls on you.
		13      releases a cloud of spores.
		14      begs you for money.
		15      You've been slimed.
		16      crushes you.
		17      tramples you.
		18      drools on you.
		19      insults you.

		20      butts you.
		21      charges you.
		22      engulfs you.
	23      sings 'I love you, you love me' 

		99      is repelled.

	Example:  For a creature which bites for 1d6, then stings for
		  2d4 and loss of dex you would use:
			{1,2,1,6},{15,4,2,4}

	Sleep (sleep)   :       A measure in turns of how fast creature
				will notice player (on the average).
	Area of affect (aaf) :  Max range that creature is able to "notice"
				the player.
									*/

creature_type c_list[MAX_CREATURES] = {

{"Paavo Vaaranen"     ,(MV_ATT_NORM|MV_20|THRO_DR|PICK_UP|HAS_60
		|CARRY_OBJ)
		,(NONE8),(EVIL|UNIQUE|CHARM_SLEEP|MAX_HP|GOOD|CAN_SPEAK)
		,(NONE8),(NONE8)
	,0,3,40,6,21,'p',{6,66},{268,268,0,0},0,5,'‡'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Filthy street urchin"     ,(MV_ATT_NORM|MV_20|THRO_DR|PICK_UP)
		,(NONE8),(GROUP|EVIL),(NONE8),(NONE8)
	,0,40,4,1,21,'p',{1,4},{72,148,0,0},0,3,'‡'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Scrawny cat"              ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI),(NONE8)
			    ,(NONE8)
	,0,10,30,1,25,'f',{1,2},{49,0,0,0},0,3,'Í'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Scruffy little dog"       ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI),(NONE8)
			    ,(NONE8)
	,0,5,20,1,19,'C',{1,3},{24,0,0,0},0,3,'Ë'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Martti Ihrasaari"            ,(MV_ATT_NORM|CARRY_OBJ|HAS_90),(NONE8)
		,(UNIQUE|MAX_HP|CHARM_SLEEP|GOOD|BREAK_WALL|CAN_SPEAK),
		(NONE8),(NONE8)
	,0,3,40,10,19,'P',{25,15},{279,279,0,0},0,5,'ë'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Blubbering idiot"         ,(MV_ATT_NORM|MV_20|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
	,0,0,6,1,20,'p',{1,2},{79,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Hobo"      ,(MV_ATT_NORM|MV_20|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
    ,0,0,6,1,20,'p',{1,2},{72,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Raving lunatic"        ,(MV_ATT_NORM|MV_20|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
	,0,0,6,1,35,'p',{4,4},{79,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Pitiful looking beggar"   ,(MV_ATT_NORM|MV_20|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
	,0,40,10,1,18,'p',{1,4},{72,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Mangy looking leper"      ,(MV_ATT_NORM|MV_20|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
    ,0,50,10,1,19,'p',{1,1},{79,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Black Market deliverer"        ,(CARRY_OBJ|CARRY_GOLD|HAS_60|
			      MV_ATT_NORM|THRO_DR|PICK_UP)
			    ,(NONE8),(EVIL),(NONE8),(NONE8)
    ,0,99,10,8,22,'7',{2,8},{5,149,0,0},0,1,'‡'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Singing, happy drunk"     ,(CARRY_GOLD|HAS_60|
			      MV_ATT_NORM|MV_40|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
	,0,0,10,1,24,'p',{2,3},{72,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Aimless looking merchant" ,(CARRY_GOLD|HAS_60|
			      MV_ATT_NORM|MV_40|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
	,0,255,10,1,20,'p',{3,3},{2,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Mean looking mercenary"   ,(CARRY_GOLD|CARRY_OBJ|HAS_90|
			      MV_ATT_NORM|MV_40|THRO_DR|PICK_UP)
			    ,(NONE8),(EVIL),(NONE8),(NONE8)
	,0,250,10,20,22,'p',{5,8},{9,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Battle scarred veteran"   ,(CARRY_GOLD|CARRY_OBJ|HAS_90|
			      MV_ATT_NORM|MV_40|THRO_DR|PICK_UP)
			    ,(NONE8),(NONE8),(NONE8),(NONE8)
	,0,250,10,30,21,'p',{7,8},{15,0,0,0},0,1,'‡'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Grey mold"               ,(MV_ONLY_ATT),(NONE8),(CHARM_SLEEP|ANIMAL|SEMI),(NONE8)
	   ,(NONE8),3,0,2,1,22,'m',{1,2},{3,3,0,0},1,1,'Ó'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Blinking dot"     ,(MV_ONLY_ATT),(0x3L|BLINK),(CHARM_SLEEP|ANIMAL),(NONE8)
	   ,(NONE8),2,20,10,1,25,',',{1,2},{91,0,0,0},1,1,'è'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Giant yellow centipede"   ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
	,2,30,8,12,20,'c',{2,6},{26,60,0,0},1,1,'û'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Millipede"    ,(MV_ATT_NORM|MV_40),(NONE8),(ANIMAL|SEMI),(NONE8)
	,(NONE8),2,40,7,10,20,'c',{3,5},{25,59,0,0},1,1,'û'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Newt"         ,(MV_ATT_NORM|MV_75),(NONE8),(ANIMAL|SEMI),(NONE8)
	,(NONE8),2,10,12,7,20,'R',{3,5},{25,0,0,0},1,1,'‚'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Transparent jelly"         ,(MV_ATT_NORM|MV_75|MV_INVIS),(NONE8),(ANIMAL|MINDLESS)
			    ,(NONE8),(NONE8)
	,1,10,12,6,18,'j',{2,5},{63,0,0,0},1,1,'Ê'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Greater hell-beast"        ,(MV_ATT_NORM),(0x9L|BLINK|TELE),(EVIL|MAX_HP
		    |IM_FIRE|IM_LIGHTNING|IM_FROST|IM_ACID|IM_POISON
		    |BREAK_WALL)
			    ,(NONE8),(NONE8)
	,1,99,4,88,24,'&',{15,100},{264,0,0,0},1,4,'Ñ'
#ifdef TC_COLOR  /* Give me one newbie who wasn't scared shitless by
		    this one... -TY*/
  , DARKGRAY
#endif
},

{"Large brown snake"        ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
	,3,99,4,35,15,'R',{4,6},{26,73,0,0},1,1,'¥'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Dog from space"        ,(MV_ATT_NORM|MV_40),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
	,2,99,4,30,18,'C',{3,6},{24,0,0,0},1,1,'Ë'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Grid bug"             ,(THRO_DR|MV_ATT_NORM),(NONE8),(ANIMAL
		|GROUP|IM_LIGHTNING)
		,(NONE8),(NONE8)
	,3,10,20,16,18,'i',{1,5},{131,0,0,0},1,1,'¸'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Squire"                   ,(CARRY_OBJ|CARRY_GOLD|HAS_60|
			     THRO_DR|MV_ATT_NORM),(NONE8),(EVIL)
			    ,(NONE8),(NONE8)
	,5,10,20,16,19,'k',{3,7},{5,0,0,0},1,1,'˝'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"White worm mass"          ,(MULTIPLY|MV_75|MV_ATT_NORM),(NONE8)
			    ,(ANIMAL|HURT_LIGHT|IM_POISON|SEMI)
			    ,(NONE8),(NONE8)
	,2,10,7,1,14,'w',{4,4},{173,0,0,0},1,1,'˚'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Floating eye"             ,(MV_ONLY_ATT),(NONE8),(ANIMAL|HURT_LIGHT)
			    ,(NONE8),(NONE8)
    ,1,10,2,6,20,'e',{3,6},{146,0,0,0},1,1,'È'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Rock lizard"              ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
	,2,15,20,4,20,'R',{3,4},{24,0,0,0},1,1,'‚'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Jackal"                   ,(MV_ATT_NORM),(NONE8),(ANIMAL|GROUP)
			    ,(NONE8),(NONE8)
	,1,10,10,3,20,'C',{1,4},{24,0,0,0},1,1,'Ë'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Soldier ant"              ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
	,3,10,10,3,20,'i',{2,5},{25,0,0,0},1,1,'Â'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Fruit bat"                ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
	,1,10,20,3,31,'b',{1,6},{24,0,0,0},1,1,'á'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Shrieker mushroom patch"  ,(MV_ONLY_ATT),(NONE8),(CHARM_SLEEP|ANIMAL|MINDLESS)
			    ,(NONE8),(NONE8)
	,1,0,2,1,23,',',{1,1},{203,0,0,0},2,1,'è'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Huge drooling insect"    ,(CARRY_GOLD|HAS_90|
			      PICK_UP|THRO_CREAT|MV_40|MV_ATT_NORM)
		,(NONE8),(ANIMAL|HURT_LIGHT|SEMI),(NONE8),(NONE8)
	,8,10,14,4,18,'i',{5,6},{79,174,210,79},2,1,'Â'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Metallic green centipede" ,(MV_40|MV_ATT_NORM),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
	,3,10,5,4,31,'c',{4,4},{68,0,0,0},2,1,'û'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Novice warrior"           ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			      MV_ATT_NORM),(NONE8),(NONE8),(NONE8),(NONE8)
	,6,5,20,16,21,'p',{9,4},{6,5,0,0},2,1,'‡'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Novice rogue"             ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|PICK_UP|
			      MV_ATT_NORM),(NONE8),(EVIL),(NONE8),(NONE8)
	,6,5,20,12,21,'p',{8,4},{5,148,0,0},2,1,'‡'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Novice priest"            ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			      MV_ATT_NORM),(0xCL|CAUSE_LIGHT|FEAR)
			    ,(NONE8),(HEAL),(NONE8)
	,7,10,20,10,19,'p',{7,4},{4,0,0,0},2,1,'¯'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Novice mage"              ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			      MV_ATT_NORM),(0xCL|CONFUSION|MAG_MISS|BLINK)
			    ,(NONE8),(NONE8),(NONE8)
	,7,5,20,6,19,'p',{6,4},{3,0,0,0},2,1,'¯'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Yellow mushroom patch"   ,(MV_ONLY_ATT),(NONE8),(CHARM_SLEEP|ANIMAL|MINDLESS),(NONE8)
	   ,(NONE8),2,0,2,1,20,',',{1,1},{100,0,0,0},2,1,'è'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"White jelly"              ,(MV_ONLY_ATT),(NONE8)
			    ,(CHARM_SLEEP|ANIMAL|HURT_LIGHT|IM_POISON|MINDLESS),(NONE8)
    ,(NONE8),10,99,2,1,30,'j',{8,8},{168,0,0,0},2,1,'Ê'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Stork"         ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL),(NONE8)
	,(NONE8),6,30,12,8,19,'B',{2,8},{26,0,0,0},2,1,'í'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Giant black ant"          ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI),(NONE8)
	,(NONE8),8,80,8,20,20,'i',{3,6},{27,0,0,0},2,1,'Â'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Salamander"               ,(MV_ATT_NORM|MV_20),(NONE8),(IM_FIRE|ANIMAL)
			    ,(NONE8),(NONE8)
	,10,80,8,20,21,'R',{4,6},{105,0,0,0},2,1,'™'
#ifdef TC_COLOR
  , RED
#endif
},

{"White harpy"              ,(MV_ATT_NORM|MV_40),(NONE8),(ANIMAL|EVIL),(NONE8)
	  ,(NONE8),5,10,16,17,22,'H',{2,5},{49,49,25,0},2,1,'ﬁ'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Weirdo from another planet"
		   ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			     MV_ATT_NORM),(NONE8),(ANIMAL|MINDLESS),(NONE8)
	,(NONE8),4,10,18,14,21,'j',{2,6},{4,0,0,0},2,2,'©'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Bat-Bat, the Dark Knight" ,(MV_ATT_NORM|MV_20),(NONE8),(UNIQUE|MAX_HP|
		  CHARM_SLEEP|ANIMAL|HURT_LIGHT|CAN_SPEAK),(NONE8),(NONE8)
	 ,30,0,30,30,31,'b',{5,5},{3,0,0,0},2,1,'á'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Garfield" ,(MV_ATT_NORM|MV_20),(NONE8),(UNIQUE|MAX_HP|
			      CHARM_SLEEP|ANIMAL|CAN_SPEAK),(NONE8),(NONE8)
	 ,30,0,30,30,29,'f',{5,5},{27,0,0,0},2,1,'Í'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Green worm mass"          ,(MULTIPLY|MV_75|MV_ATT_NORM),(NONE8)
			    ,(ANIMAL|HURT_LIGHT|IM_ACID|SEMI),(NONE8)
	,(NONE8),3,10,7,3,15,'w',{6,4},{140,0,0,0},2,1,'˚'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Robot probe sent from Jupiter"
		,(MV_ATT_NORM|MV_20),(NONE8),(NONE8),(NONE8)
    ,(NONE8),9,75,5,38,15,'g',{4,8},{3,209,0,0},2,2,''
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Cave spider"              ,(MV_ATT_NORM),(NONE8),(ANIMAL|GROUP|HURT_LIGHT|SEMI)
		,(NONE8)
	,(NONE8),7,80,8,16,29,'S',{2,6},{27,0,0,0},2,1,'˜'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Pink panther"                 ,(THRO_DR|MV_ATT_NORM),(NONE8),(ANIMAL),(NONE8)
	,(NONE8),8,0,40,12,32,'f',{3,5},{51,51,0,0},2,2,'Í'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Mickey Mouse"                  ,(HAS_1D2|CARRY_GOLD|HAS_60|THRO_DR|MV_75|
			      MV_ATT_NORM|MV_INVIS),(NONE8),(EVIL|UNIQUE|CAN_SPEAK)
			    ,(NONE8),(NONE8)
	,16,5,20,12,42,'r',{11,4},{3,148,0,0},3,2,'ˆ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Green ooze"               ,(HAS_90|CARRY_GOLD|CARRY_OBJ|MV_75|
			      MV_ATT_NORM),(NONE8),(ANIMAL|MINDLESS),(NONE8),(NONE8)
	,4,80,8,16,32,'j',{3,4},{140,0,0,0},3,2,'©'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Poltergeist"              ,(CARRY_OBJ|CARRY_GOLD|HAS_60|HAS_90|PICK_UP|
			     MV_INVIS|THRO_WALL|MV_40|MV_75|
			     MV_ATT_NORM),(BLINK|0xFL)
			    ,(CHARM_SLEEP|HURT_LIGHT|EVIL|NO_INFRA|UNDEAD|IM_FROST)
			    ,(NONE8),(NONE8)
	,8,10,8,15,45,'G',{2,5},{93,0,0,0},3,1,'Ò'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Metallic blue centipede"  ,(MV_40|MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8)
	,(NONE8),7,15,6,6,31,'c',{4,5},{69,0,0,0},3,1,'û'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Giant white louse"        ,(MULTIPLY|MV_ATT_NORM|MV_75)
			    ,(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
	,1,10,6,5,32,'F',{1,1},{24,0,0,0},3,1,'Ù'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Black naga"               ,(CARRY_OBJ|CARRY_GOLD|HAS_60|MV_ATT_NORM|
			      MV_20),(NONE8),(EVIL),(NONE8),(NONE8)
	,20,120,16,40,22,'n',{6,8},{75,0,0,0},3,1,'•'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Spotted mushroom patch"   ,(MV_ONLY_ATT),(NONE8)
			    ,(CHARM_SLEEP|ANIMAL|IM_POISON|MINDLESS),(NONE8)
	,(NONE8),3,0,2,1,21,',',{1,1},{175,0,0,0},3,1,'è'
#ifdef TC_COLOR
  , CYAN
#endif
},



{"Silver jelly"             ,(MV_ONLY_ATT),(0xFL|MANA_DRAIN),
			     (CHARM_SLEEP|ANIMAL|HURT_LIGHT|IM_POISON|MINDLESS),(NONE8)
      ,(NONE8),12,99,2,1,33,'j',{10,8},{213,213,0,0},3,2,'Ê'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Giant owl"         ,(MV_40|MV_ATT_NORM),(NONE8),
		 (ANIMAL),(NONE8)
    ,(NONE8),11,9,2,1,32,'B',{5,8},{5,0,0,0},3,1,'í'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Ewok"   ,(CARRY_GOLD|CARRY_OBJ|HAS_60|THRO_DR|
	  MV_20|MV_ATT_NORM),(0x8L),(GROUP),(NONE8),(ARROW)
    ,4,10,16,8,21,'h',{3,5},{3,3,0,0},5,2,'ê'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Giant white ant"          ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,7,80,8,16,22,'i',{3,6},{27,0,0,0},3,1,'Â'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Yellow mold"             ,(MV_ONLY_ATT),(NONE8),(CHARM_SLEEP|ANIMAL),(NONE8)
       ,(NONE8),9,99,2,10,19,'m',{8,8},{3,0,0,0},3,1,'Ó'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"My Little Pony"   ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL),(NONE8)
    ,(NONE8),11,20,8,10,33,'q',{5,8},{25,203,0,0},3,3,'ı'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Yellow worm mass"         ,(MULTIPLY|MV_75|MV_ATT_NORM),(NONE8)
			    ,(ANIMAL|HURT_LIGHT|SEMI),(NONE8),(NONE8)
    ,4,10,7,4,14,'w',{4,8},{182,0,0,0},3,2,'˚'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Clear worm mass"          ,(MULTIPLY|MV_INVIS|MV_75|MV_ATT_NORM),(NONE8)
			    ,(ANIMAL|HURT_LIGHT|IM_POISON|SEMI),(NONE8),(NONE8)
    ,4,10,7,1,14,'w',{4,4},{173,0,0,0},3,2,'˚'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Radiation eye"            ,(MV_ONLY_ATT),(0xBL|MANA_DRAIN)
			    ,(ANIMAL|HURT_LIGHT),(NONE8),(NONE8)
    ,6,10,2,6,18,'e',{3,6},{88,0,0,0},3,1,'È'
#ifdef TC_COLOR
  , RED
#endif
},

{"Cave lizard"              ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,8,80,8,16,21,'R',{3,6},{28,0,0,0},4,1,'‚'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Novice ranger"            ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			      MV_ATT_NORM),(0x9L|MAG_MISS)
			    ,(NONE8),(NONE8),(NONE8)
    ,18,5,20,6,22,'p',{6,8},{4,4,0,0},4,1,'‡'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Novice paladin"           ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			     MV_ATT_NORM),(0x9L|CAUSE_LIGHT|FEAR),(NONE8)
			    ,(NONE8),(NONE8)
    ,20,5,20,16,22,'k',{6,8},{6,6,0,0},4,2,'˝'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Blue jelly"               ,(MV_ONLY_ATT),(NONE8)
			    ,(CHARM_SLEEP|ANIMAL|HURT_LIGHT|IM_FROST|NO_INFRA|MINDLESS)
			    ,(NONE8),(NONE8)
    ,14,99,2,1,19,'j',{12,8},{125,0,0,0},4,1,'Ê'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Creeping copper coins"    ,(HAS_1D2|CARRY_GOLD|MV_ATT_NORM)
			    ,(NONE8),(CHARM_SLEEP|ANIMAL|IM_POISON|NO_INFRA)
			    ,(NONE8),(NONE8)
    ,9,10,3,24,16,'$',{7,8},{3,170,0,0},4,2,'∫'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Giant white rat"          ,(MULTIPLY|MV_20|MV_ATT_NORM),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
    ,1,30,8,7,19,'r',{2,2},{153,0,0,0},4,1,'ˆ'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Blue worm mass"           ,(MULTIPLY|MV_ATT_NORM|MV_75),(NONE8)
			    ,(ANIMAL|HURT_LIGHT|IM_FROST|NO_INFRA|SEMI),(NONE8)
    ,(NONE8),5,10,7,12,14,'w',{5,8},{129,0,0,0},4,1,'˚'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Large grey snake"         ,(MV_20|MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8)
    ,(NONE8),14,50,6,41,16,'R',{6,8},{28,75,0,0},4,1,'¥'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Mighty Mouse"    ,(CARRY_OBJ|HAS_2D2|THRO_DR|
		  MV_ATT_NORM),(NONE8),(UNIQUE|GOOD|MAX_HP|IM_FIRE|IM_FROST
		  |IM_LIGHTNING|CAN_SPEAK)
			    ,(NONE8),(NONE8)
    ,90,10,16,8,31,'r',{8,8},{7,7,98,0},6,3,'ˆ'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Novice mage"              ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			      MV_ATT_NORM),(0xCL|CONFUSION|MAG_MISS|BLINK|
			      BLINDNESS),(GROUP),(NONE8),(NONE8)
    ,7,20,20,6,21,'p',{6,4},{3,0,0,0},5,1,'¯'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Green naga"               ,(CARRY_OBJ|CARRY_GOLD|HAS_60|
			     MV_ATT_NORM|MV_20),(NONE8),(EVIL|IM_ACID),(NONE8)
     ,(NONE8),30,120,18,40,21,'n',{9,8},{75,118,0,0},5,1,'•'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Smurf"                ,(HAS_60|CARRY_GOLD|CARRY_OBJ|MV_75|MV_INVIS|
		  MV_ATT_NORM),(NONE8),(GROUP),(NONE8)
    ,(NONE8),7,80,8,16,19,'h',{3,4},{209,0,0,0},3,1,'ê'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Green glutton ghost"      ,(CARRY_GOLD|CARRY_OBJ|HAS_60|HAS_90|PICK_UP|
			      THRO_WALL|MV_INVIS|MV_ATT_NORM|MV_40|MV_75)
	       ,(0x9L|BLINK)
	       ,(CHARM_SLEEP|EVIL|NO_INFRA|UNDEAD|IM_FROST),(NONE8)
       ,(NONE8),15,10,10,20,44,'G',{3,4},{211,0,0,0},5,1,'Ò'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Green jelly"              ,(MV_ONLY_ATT),(NONE8)
			    ,(CHARM_SLEEP|ANIMAL|HURT_LIGHT|IM_ACID|MINDLESS),(NONE8)
    ,(NONE8),18,99,2,1,31,'j',{22,8},{136,0,0,0},5,1,'Ê'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Zog"             ,(CARRY_OBJ|CARRY_GOLD|HAS_90|THRO_DR|MV_ATT_NORM)
			    ,(NONE8),(EVIL),(NONE8),(NONE8)
    ,25,30,20,32,21,'h',{13,9},{9,0,0,0},5,1,'Ï'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Death sword"          ,(MV_ONLY_ATT|HAS_90|CARRY_OBJ),(NONE8)
		,(CHARM_SLEEP|EVIL|IM_FROST|NO_INFRA|IM_FIRE
		 |IM_LIGHTNING|IM_POISON|MAX_HP|MINDLESS)
			    ,(NONE8),(NONE8)
    ,30,40,20,40,42,'|',{6,6},{23,23,23,23},6,5,''
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Giddy goon"          ,(MV_ATT_NORM|MV_40|THRO_DR)
		,(NONE8),(DEMON|GROUP|EVIL),(NONE8)
    ,(NONE8),10,15,14,12,22,'I',{4,8},{4,0,0,0},5,1,'Î'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Disenchanter eye"         ,(MV_ONLY_ATT),(MANA_DRAIN|0x9L)
			    ,(ANIMAL|HURT_LIGHT),(NONE8),(NONE8)
    ,20,10,2,10,17,'e',{7,8},{207,0,0,0},5,2,'È'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Red worm mass"            ,(MULTIPLY|MV_ATT_NORM|MV_75),(NONE8)
			    ,(ANIMAL|HURT_LIGHT|IM_FIRE|SEMI),(NONE8),(NONE8)
    ,6,10,7,12,16,'w',{5,8},{111,0,0,0},5,1,'˚'
#ifdef TC_COLOR
  , RED
#endif
},



{"Copperhead snake"         ,(MV_ATT_NORM|MV_40),(NONE8),(ANIMAL|IM_POISON|SEMI)
			    ,(NONE8),(NONE8)
    ,15,1,6,20,21,'R',{4,6},{158,0,0,0},5,1,'¥'
#ifdef TC_COLOR
  , RED
#endif
},

{"Purple mushroom patch"    ,(MV_ONLY_ATT),(NONE8),(CHARM_SLEEP|ANIMAL|MINDLESS)
			    ,(NONE8),(NONE8)
    ,15,0,2,1,23,',',{1,1},{183,183,183,0},6,2,'è'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Novice priest"            ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
			     MV_ATT_NORM),(0xCL|CAUSE_LIGHT|FEAR),(GROUP)
			    ,(HEAL),(NONE8)
    ,7,5,20,10,19,'p',{7,4},{4,0,0,0},6,2,'¯'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Kid from space"           ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
		 MV_ATT_NORM),(NONE8),(EVIL|GROUP),(NONE8),(NONE8)
    ,6,5,20,16,19,'p',{3,8},{6,5,0,0},6,2,'ê'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Nibelung"         ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|PICK_UP|
		 MV_ATT_NORM),(NONE8),(EVIL|GROUP|HURT_LIGHT),(NONE8)
			    ,(NONE8)
    ,6,5,20,12,21,'h',{8,4},{5,148,0,0},6,1,'ê'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Brown mold"               ,(MV_ONLY_ATT),(NONE8),(CHARM_SLEEP|ANIMAL)
			    ,(NONE8),(NONE8)
    ,20,99,2,12,22,'m',{15,8},{89,0,0,0},6,1,'Ó'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Disembodied hand that strangled people"
		  ,(MV_40|MV_ATT_NORM),(NONE8),(EVIL|UNDEAD|CHARM_SLEEP
	  |NO_INFRA|IM_POISON|FEARLESS|MINDLESS)
			    ,(NONE8),(NONE8)
    ,10,30,10,15,42,'z',{3,8},{75,0,0,0},6,2,'„'
#ifdef TC_COLOR
  , GREEN
  #endif
},

{"Novice archer"            ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_1D2)
			    ,(0x3L),(NONE8),(NONE8),(ARROW)
    ,20,5,20,10,29,'p',{6,8},{3,3,0,0},6,2,'‡'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Creeping silver coins"    ,(HAS_1D2|CARRY_GOLD|HAS_60|MV_ATT_NORM)
			    ,(NONE8),(CHARM_SLEEP|ANIMAL|IM_POISON|NO_INFRA)
			    ,(NONE8),(NONE8)
    ,18,10,4,30,16,'$',{12,8},{5,171,0,0},6,2,'∫'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Snaga"                    ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|MV_ATT_NORM)
			    ,(NONE8),(ORC|EVIL|GROUP|HURT_LIGHT),(NONE8)
    ,(NONE8),15,30,20,32,19,'o',{8,8},{7,0,0,0},6,1,'Ô'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Cute little dinosaur"          ,(MV_ATT_NORM|MV_40),(NONE8)
		,(ANIMAL|SEMI|IM_POISON|GROUP) /* Argh! GROUP was in cmove
					    field! No wonder they never
					    appeared in groups! Instead,
					    they ate other monsters...
					     -TY */
			    ,(NONE8),(NONE8)
    ,20,1,6,24,19,'R',{6,7},{227,0,0,0},8,1,'‚'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Cave orc"                 ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|MV_ATT_NORM)
			    ,(NONE8),(ORC|EVIL|GROUP|HURT_LIGHT),(NONE8)
	,(NONE8),20,30,20,32,20,'o',{11,9},{7,0,0,0},7,1,'Û'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Wood spider"              ,(MV_ATT_NORM),(NONE8),(ANIMAL|GROUP|IM_POISON|SEMI)
			    ,(NONE8),(NONE8)
    ,15,80,8,16,31,'S',{3,6},{26,165,0,0},7,3,'˜'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Manes"                    ,(THRO_DR|MV_ATT_NORM)
			    ,(NONE8),(DEMON|EVIL|GROUP|IM_FIRE),(NONE8)
	,(NONE8),16,30,20,32,20,'I',{8,8},{7,0,0,0},7,2,'Î'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Bloodshot eye"            ,(MV_ONLY_ATT),(0x7L|MANA_DRAIN)
			    ,(ANIMAL|HURT_LIGHT),(NONE8),(NONE8)
    ,15,10,2,6,23,'e',{5,8},{143,0,0,0},7,3,'È'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Red naga"                 ,(CARRY_GOLD|CARRY_OBJ|HAS_60|MV_ATT_NORM|
			      MV_20),(NONE8),(EVIL),(NONE8),(NONE8)
    ,40,120,20,40,22,'n',{11,8},{76,82,0,0},7,2,'•'
#ifdef TC_COLOR
  , RED
#endif
},

{"Red jelly"                ,(MV_ONLY_ATT),(NONE8)
			    ,(CHARM_SLEEP|ANIMAL|HURT_LIGHT|MINDLESS),(NONE8),(NONE8)
    ,26,99,2,1,22,'j',{26,8},{87,0,0,0},7,1,'Ê'
#ifdef TC_COLOR
  , RED
#endif
},

{"Killer bee"           ,(MV_ATT_NORM|MV_40)
		,(NONE8),(ANIMAL|GROUP|SEMI),(NONE8)
    ,(NONE8),10,50,12,16,29,'i',{1,8},{85,164,0,0},7,3,'Â'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Crypt creep"         ,(MV_ATT_NORM|MV_40)
		,(0x9L|S_UNDEAD|CAUSE_LIGHT),(UNDEAD|EVIL|HURT_LIGHT
		|CHARM_SLEEP|IM_FROST|NO_INFRA|GROUP|IM_POISON)
		,(NONE8)
       ,(NONE8),25,20,14,12,19,'s',{5,8},{170,0,0,0},7,2,'µ'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Rotting corpse"            ,(THRO_DR|MV_ATT_NORM),(NONE8),
			     (CHARM_SLEEP|UNDEAD|EVIL|IM_FROST|NO_INFRA|
		 IM_POISON|GROUP|FEARLESS|MINDLESS),(NONE8),(NONE8)
	 ,14,30,20,14,20,'z',{5,5},{238,0,0,0},7,1,'›'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Lost soul"                ,(CARRY_OBJ|CARRY_GOLD|HAS_60|HAS_90|
			      PICK_UP|MV_INVIS|THRO_WALL|MV_ATT_NORM|
			      MV_20|MV_40),(0xFL|TELE|MANA_DRAIN)
			    ,(CHARM_SLEEP|UNDEAD|EVIL|IM_FROST|NO_INFRA)
			    ,(NONE8),(NONE8)
    ,18,10,12,10,24,'G',{2,8},{11,185,0,0},7,2,'Ò'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Dark elf"                 ,(CARRY_OBJ|CARRY_GOLD|HAS_90|THRO_DR|
			      MV_ATT_NORM),(0xAL|CONFUSION),(EVIL|HURT_LIGHT),
				(DARKNESS)
    ,(NONE8),25,20,20,16,19,'h',{7,10},{5,5,0,0},7,2,'ﬂ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Night lizard"             ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,35,30,20,16,21,'R',{4,8},{29,29,0,0},7,2,'‚'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"The Phantom of the Opera"  ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
		,(0x8L|FEAR|CAUSE_LIGHT),(EVIL|IM_POISON|IM_FIRE
		|MAX_HP|UNIQUE|GOOD|CAN_SPEAK)
			    ,(NONE8),(NONE8)
    ,120,20,20,20,22,'G',{13,13},{9,9,9,0},7,3,'Ò'
#ifdef TC_COLOR
  , RED
#endif
},

{"Robin Hood, the Outlaw",
		 (MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2|HAS_60|HAS_90)
	,(0x5L)
	,(MAX_HP|UNIQUE|SPECIAL|EVIL|CAN_SPEAK)
		,(TRAP_CREATE|HEAL),(ARROW)
    ,150,20,20,30,33,'p',{14,12},{4,4,148,149},8,1,'‡'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Gargamel, the Evil Wizard"        ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
		,(0x3L|CONFUSION|MAG_MISS|FROST_BOLT|FIRE_BOLT|MONSTER)
		,(EVIL|MAX_HP|UNIQUE|GOOD|CAN_SPEAK),(NONE8)
			    ,(NONE8)
    ,80,30,20,32,21,'p',{16,12},{8,8,8,0},8,2,'¯'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Dirty cannibal"               ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
		  MV_ATT_NORM),(NONE8),(GROUP|EVIL),(NONE8),(NONE8)
	,11,10,18,18,20,'p',{4,9},{5,0,0,0},8,1,'‡'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Novice ranger"            ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|
		 MV_ATT_NORM),(0x9L|MAG_MISS),(GROUP)
			    ,(NONE8),(NONE8)
	,18,5,20,6,20,'p',{6,8},{4,4,0,0},8,1,'‡'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Giant salamander"         ,(MV_ATT_NORM|MV_20),(0x9L|BREATH_FI)
			    ,(ANIMAL|IM_FIRE),(NONE8)
    ,(NONE8),50,1,6,40,22,'R',{6,7},{106,0,0,0},8,1,'™'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Space monster"               ,(MV_ATT_NORM| THRO_WALL),(NONE8),
		 (CHARM_SLEEP|ANIMAL|UNDEAD|IM_ACID),(NONE8),(NONE8)
    ,28,30,20,14,21,' ',{21,8},{94,0,0,0},8,2,' '
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Ghast"             ,(MV_ATT_NORM|THRO_DR),
		 (NONE8),(CHARM_SLEEP|UNDEAD|EVIL|IM_POISON|
		 IM_FROST|NO_INFRA|HURT_LIGHT),(NONE8),(NONE8)
    ,26,40,20,36,21,'G',{10,8},{266,26,0,0},8,1,'Ò'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Hunting hawk of Julian"   ,(MV_ATT_NORM),(NONE8),(ANIMAL|FEARLESS),(NONE8),(NONE8)
    ,22,10,20,26,32,'B',{8,8},{50,50,29,0},8,2,'í'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Gremlin"           ,(THRO_DR|MV_ATT_NORM|MULTIPLY|PICK_UP)
	,(NONE8),(DEMON|EVIL|IM_POISON|HURT_LIGHT)
			    ,(NONE8),(NONE8)
    ,6,30,20,32,19,'I',{5,5},{4,209,0,0},8,3,'Î'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Hill orc"                 ,(CARRY_OBJ|CARRY_GOLD|HAS_60|THRO_DR|MV_ATT_NORM)
			    ,(NONE8),(ORC|EVIL|GROUP),(NONE8),(NONE8)
    ,25,30,20,32,21,'o',{13,9},{9,0,0,0},8,1,'Û'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Tax collector"                   ,(HAS_1D2|CARRY_OBJ|CARRY_GOLD|THRO_DR|PICK_UP|
			      MV_ATT_NORM),(NONE8),(EVIL),(NONE8),(NONE8)
    ,30,10,20,24,22,'p',{8,8},{13,148,278,0},8,2,'‡'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Yeti"                     ,(THRO_DR|MV_ATT_NORM),(NONE8),(ANIMAL|IM_FROST|
				NO_INFRA),(NONE8),(NONE8)
    ,30,10,20,24,22,'Y',{11,9},{51,51,27,0},9,3,'æ'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Goat"     ,(MV_ATT_NORM|MV_40),(NONE8)
		,(ANIMAL),(NONE8),(NONE8)
    ,24,20,14,18,29,'q',{7,8},{228,228,0,0},9,1,'ı'
#ifdef TC_COLOR
  , WHITE
#endif
},



{"Giant grey rat"           ,(MULTIPLY|MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
    ,2,20,8,12,22,'r',{2,3},{154,0,0,0},9,1,'ˆ'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Black harpy"              ,(MV_ATT_NORM|MV_20),(NONE8),(EVIL|ANIMAL)
			    ,(NONE8),(NONE8)
    ,19,10,16,22,33,'H',{3,8},{50,50,26,0},9,1,'ﬁ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Orc shaman"               ,(CARRY_OBJ|CARRY_GOLD|HAS_90|THRO_DR|MV_ATT_NORM)
			    ,(0x8L|MAG_MISS|CAUSE_LIGHT|BLINK)
			    ,(EVIL|ORC|HURT_LIGHT)
			    ,(NONE8),(NONE8)
    ,30,20,20,15,24,'o',{9,8},{5,5,0,0},9,1,'Û'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Baby blue dragon"         ,(MV_ATT_NORM|HAS_1D2|CARRY_GOLD|HAS_60|THRO_DR)
			    ,(0xBL|BREATH_L),(IM_LIGHTNING|EVIL|DRAGON|
			     MAX_HP),(NONE8),(NONE8)
	,35,70,20,30,20,'d',{10,10},{51,51,28,0},9,2,''
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Baby white dragon"        ,(MV_ATT_NORM|HAS_1D2|CARRY_GOLD|HAS_60|THRO_DR)
			    ,(0xBL|BREATH_FR),(IM_FROST|EVIL|DRAGON|MAX_HP|
				NO_INFRA),(NONE8),(NONE8)
	,35,70,20,30,20,'d',{10,10},{51,51,28,0},9,2,''
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Baby green dragon"        ,(MV_ATT_NORM|HAS_1D2|CARRY_GOLD|HAS_60|THRO_DR)
			    ,(0xBL|BREATH_G),(IM_POISON|EVIL|DRAGON|MAX_HP)
			    ,(NONE8),(NONE8)
	,35,70,20,30,20,'d',{10,10},{51,51,28,0},9,2,''
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Baby black dragon"        ,(MV_ATT_NORM|HAS_1D2|CARRY_GOLD|HAS_60|THRO_DR)
			    ,(0xBL|BREATH_A),(IM_ACID|EVIL|DRAGON|MAX_HP)
			    ,(NONE8),(NONE8)
	,35,70,20,30,20,'d',{10,10},{51,51,28,0},9,2,''
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Baby red dragon"          ,(MV_ATT_NORM|HAS_1D2|CARRY_GOLD|HAS_60|THRO_DR)
			    ,(0xBL|BREATH_FI),(IM_FIRE|EVIL|DRAGON|MAX_HP)
			    ,(NONE8),(NONE8)
	,35,70,20,30,20,'d',{10,11},{51,51,28,0},9,2,''
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Giant red ant"            ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,22,60,12,34,22,'i',{4,8},{27,85,0,0},9,2,'Â'
#ifdef TC_COLOR
  , RED
#endif
},

{"The Lizard King"   ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
		,(0x6L|SLOW|FEAR|CONFUSION)
		,(IM_POISON|MAX_HP|UNIQUE|GOOD|EVIL|ANIMAL|CAN_SPEAK)
		,(TRAP_CREATE),(NONE8)
    ,120,20,20,25,23,'R',{30,8},{10,10,10,0},9,2,'à'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"King cobra"               ,(MV_ATT_NORM|MV_40),(NONE8),(ANIMAL|IM_POISON|SEMI)
			    ,(NONE8),(NONE8)
    ,28,1,8,30,25,'R',{8,10},{144,161,0,0},9,2,'¥'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Giant spider"             ,(MV_ATT_NORM),(NONE8),(ANIMAL|IM_POISON|SEMI),(NONE8)
			    ,(NONE8)
    ,35,80,8,16,25,'S',{10,10},{32,156,156,32},10,2,'˜'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Dark elven mage"          ,(CARRY_OBJ|HAS_1D2|THRO_DR|MV_ATT_NORM)
			    ,(0x5L|BLINDNESS|MAG_MISS|CONFUSION),(EVIL|
			     IM_POISON|HURT_LIGHT),(ST_CLOUD|DARKNESS),(NONE8)
    ,50,20,20,16,33,'h',{7,10},{5,5,0,0},10,1,'ﬂ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Calvin, the horror kid from hell"    ,(CARRY_OBJ|HAS_90|THRO_DR|MV_ATT_NORM)
			    ,(0x4L|MONSTER|TELE_TO|BLINK|CONFUSION|SLOW)
		,(INTELLIGENT|EVIL|UNIQUE|GOOD|MAX_HP|CAN_SPEAK)
			    ,(HEAL),(NONE8)
       ,80,10,18,20,29,'p',{12,12},{8,7,268,268},10,3,'ê'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Dark elven warrior"       ,(CARRY_OBJ|CARRY_GOLD|HAS_1D2|THRO_DR|
			      MV_ATT_NORM),(NONE),(EVIL|HURT_LIGHT),
				(NONE8),(NONE8)
    ,50,20,20,16,22,'h',{10,11},{7,7,0,0},10,1,'ﬂ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Quiver slot"     ,(MULTIPLY|MV_ONLY_ATT|MV_INVIS)
		,(0x7L),(CHARM_SLEEP|ANIMAL|NO_INFRA)
		,(NONE8),(ARROW)
    ,5,0,4,1,28,',',{1,1},{70,0,0,0},10,2,'è'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Hobbes the Tiger"  ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_60)
		,(NONE8),(ANIMAL|EVIL|IM_POISON|MAX_HP|UNIQUE|GOOD|CAN_SPEAK)
			    ,(NONE8),(NONE8)
    ,160,20,20,20,33,'f',{15,15},{36,36,0,0},10,3,'Í'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Shadow creature of Fiona"
	    ,(MV_ATT_NORM|THRO_DR|HAS_60|CARRY_GOLD|CARRY_OBJ),(NONE8)
	    ,(IM_POISON|CHARM_SLEEP|GROUP),(NONE8)
     ,(NONE8),35,3,12,40,22,'h',{9,8},{8,0,0,0},10,2,'Ï'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Undead mass"              ,(MV_ONLY_ATT|MULTIPLY)
	    ,(NONE8),(UNDEAD|EVIL|NO_INFRA|CHARM_SLEEP|IM_POISON
	    |IM_FROST|HURT_LIGHT|FEARLESS),(NONE8)
     ,(NONE8),32,70,2,12,19,'m',{8,8},{170,98,0,0},10,2,'Ó'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Disenchanter mold"        ,(MV_ONLY_ATT),(MANA_DRAIN|0xBL)
			    ,(ANIMAL|CHARM_SLEEP),(NONE8),(NONE8)
    ,40,70,2,20,22,'m',{16,8},{206,0,0,0},10,2,'Ó'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Pseudo dragon"            ,(MV_ATT_NORM|CARRY_OBJ|CARRY_GOLD|HAS_60)
			    ,(0xBL|FEAR|CONFUSION),(DRAGON|MAX_HP)
			    ,(NONE8),(BREATH_LT|BREATH_DA)
    ,150,40,20,30,22,'d',{22,9},{51,51,28,0},10,2,''
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Tengu"                    ,(THRO_DR|MV_ATT_NORM)
		,(0x4L|TELE|BLINK|TELE_TO),(DEMON|EVIL|IM_FIRE),(TELE_AWAY
		|TELE_LEV)
       ,(NONE8),55,30,20,32,32,'I',{16,9},{7,0,0,0},14,1,'Ì'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Creeping gold coins"      ,(MV_ATT_NORM|HAS_1D2|HAS_90|CARRY_GOLD),(NONE8)
		,(IM_POISON|NO_INFRA|CHARM_SLEEP)
			    ,(NONE8),(NONE8)
    ,32,10,5,36,17,'$',{18,8},{14,172,0,0},10,3,'∫'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Wolf"                   ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|GROUP),(NONE8)
      ,(NONE8),30,20,30,30,29,'C',{6,6},{29,0,0,0},10,1,'Ë'
#ifdef TC_COLOR
  , BROWN
#endif
},



{"Panther"                  ,(MV_ATT_NORM),(NONE8),(ANIMAL),(NONE8),(NONE8)
	,25,0,40,30,30,'f',{10,8},{54,54,0,0},10,2,'Í'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Brigand"                  ,(HAS_1D2|CARRY_OBJ|CARRY_GOLD|THRO_DR|PICK_UP|
			      MV_ATT_NORM),(NONE8),(EVIL),(NONE8),(NONE8)
    ,35,10,20,32,23,'p',{9,8},{13,149,0,0},10,2,'‡'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Baby multi-hued dragon"   ,(MV_ATT_NORM|HAS_1D2|CARRY_GOLD|HAS_60|THRO_DR)
			    ,(0xBL|BREATH_FI|BREATH_FR|BREATH_G|BREATH_A|
			     BREATH_L)
			    ,(IM_FIRE|IM_FROST|IM_POISON|IM_ACID|IM_LIGHTNING|
			     EVIL|DRAGON|MAX_HP)
			    ,(NONE8),(NONE8)
    ,45,70,20,30,21,'d',{10,13},{51,51,28,0},11,2,''
#ifdef TC_COLOR
  , MULTI
#endif
},

{"Chaos shapechanger"
    ,(MV_ATT_NORM|HAS_60
    |CARRY_OBJ|CARRY_GOLD|HAS_90),(0x5L|CONFUSION|FIRE_BOLT|FROST_BOLT)
    ,(EVIL|SHAPECHANGER),(NONE8),(NONE8)
    ,40,10,12,14,24,'H',{20,9},{14,16,16,0},11,2,'≤'
#ifdef TC_COLOR
  , ANY
#endif
},

{"Orc zombie"               ,(THRO_DR|MV_ATT_NORM),(NONE8)
			    ,(CHARM_SLEEP|EVIL|UNDEAD|ORC|IM_FROST|NO_INFRA|
			     IM_POISON|FEARLESS|MINDLESS),(NONE8),(NONE8)
	,30,25,20,24,20,'z',{11,8},{3,3,3,0},11,1,'›'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Gnome mage"               ,(HAS_1D2|CARRY_OBJ|CARRY_GOLD|MV_ATT_NORM|
			     THRO_DR)
			    ,(0x4L|BLINK|FROST_BOLT|MONSTER),(EVIL)
			    ,(DARKNESS),(NONE8)
    ,38,10,18,20,19,'h',{7,8},{4,0,0,0},11,2,'ê'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Black mamba"              ,(MV_ATT_NORM|MV_40),(NONE8),(ANIMAL|IM_POISON|SEMI)
			    ,(NONE8),(NONE8)
    ,40,1,10,32,29,'R',{10,8},{163,0,0,0},12,3,'¥'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Hellcat"               ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|GROUP|SEMI|
		  IM_FIRE|EVIL),(NONE8),(NONE8)
    ,30,20,30,30,32,'f',{7,7},{51,51,26,0},12,1,'Í'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Grape jelly"              ,(MV_ONLY_ATT),(MANA_DRAIN|0xBL)
			    ,(HURT_LIGHT|CHARM_SLEEP|IM_POISON|MINDLESS),(NONE8)
      ,(NONE8),60,99,2,1,23,'j',{52,8},{186,0,0,0},12,3,'Ê'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Abyss worm"     ,(MULTIPLY|MV_ATT_NORM|MV_75|MV_INVIS),(NONE8)
		,(ANIMAL|HURT_LIGHT|BREAK_WALL|NO_INFRA|SEMI),(NONE8),(NONE8)
    ,40,3,7,15,18,'w',{5,8},{186,0,0,208},13,3,'˚'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Count Duckula"   ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_2D2)
		,(0x4L|CAUSE_LIGHT|FEAR),(UNDEAD|IM_POISON|IM_FROST|
			     IM_LIGHTNING|MAX_HP|UNIQUE|GOOD|CAN_SPEAK)
			    ,(NONE8),(NONE8)
    ,230,20,20,60,24,'V',{30,9},{10,10,93,153},12,3,'Ÿ'
#ifdef TC_COLOR
  , MAGENTA
#endif
},



{"Priest"                   ,(HAS_1D2|CARRY_GOLD|CARRY_OBJ|MV_ATT_NORM|
			    THRO_DR),(0x3L|CAUSE_SERIOUS|MONSTER|FEAR)
			    ,(EVIL|INTELLIGENT)
			    ,(HEAL),(NONE8)
    ,36,40,20,22,23,'p',{12,8},{12,12,0,0},12,1,'¯'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Moon beast"        ,(CARRY_OBJ|HAS_1D2|THRO_DR|MV_ATT_NORM)
			    ,(0x5L|BLINDNESS|CAUSE_SERIOUS|CONFUSION)
		,(EVIL|IM_FIRE|ANIMAL)
			    ,(HEAL|DARKNESS),(NONE8)
    ,50,30,20,30,28,'q',{7,10},{8,9,0,0},12,1,'ı'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Air spirit"               ,(THRO_DR|MV_INVIS|MV_ATT_NORM|MV_75),(NONE8)
			    ,(EVIL|NO_INFRA|IM_POISON|MINDLESS|NONLIVING|
			      CHARM_SLEEP),(NONE8),(NONE8)
    ,40,20,12,40,45,'E',{8,8},{2,0,0,0},12,2,'ç'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Skeleton human"           ,(THRO_DR|MV_ATT_NORM),(NONE8),
			     (EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|NO_INFRA|
			     IM_POISON|FEARLESS|MINDLESS),(NONE8),(NONE8)
    ,38,30,20,30,21,'s',{10,8},{7,0,0,0},12,1,'µ'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Zombie human"             ,(THRO_DR|MV_ATT_NORM),(NONE8),
			     (EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|NO_INFRA|
			     IM_POISON|FEARLESS|MINDLESS),(NONE8),(NONE8)
    ,34,20,20,24,21,'z',{12,8},{3,3,0,0},12,1,'›'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Tiger"                    ,(MV_ATT_NORM),(NONE8),(ANIMAL),(NONE8),(NONE8)
    ,40,0,40,40,31,'f',{12,10},{54,54,29,0},12,2,'Í'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Moaning spirit"           ,(CARRY_GOLD|CARRY_OBJ|HAS_60|HAS_90|
			      THRO_WALL|MV_INVIS|MV_ATT_NORM|MV_20)
			    ,(0xFL|TELE|FEAR)
			    ,(CHARM_SLEEP|EVIL|UNDEAD|IM_FROST|NO_INFRA)
			    ,(NONE8),(NONE8)
    ,44,10,14,20,31,'G',{5,8},{99,178,0,0},12,2,'Ò'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Swordsman"                ,(HAS_1D2|CARRY_GOLD|CARRY_OBJ|THRO_DR|
			      MV_ATT_NORM),(NONE8),(NONE8)
			    ,(NONE8),(NONE8)
    ,40,20,20,34,23,'p',{12,8},{18,18,0,0},12,1,'‡'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Fruminous bandersnatch"           ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,40,30,12,30,33,'c',{13,8},{34,34,62,0},12,2,'û'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Spotted jelly"            ,(THRO_DR|MV_ONLY_ATT|PICK_UP),(NONE8)
			    ,(IM_ACID|IM_POISON|ANIMAL|CHARM_SLEEP|NO_INFRA|MINDLESS)
			    ,(NONE8),(NONE8)
    ,33,1,12,18,32,'j',{13,8},{115,138,138,0},12,3,'Ê'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Mongbat"           ,(MV_ATT_NORM|GROUP),(NONE8)
	,(EVIL|IM_POISON|IM_FROST|IM_LIGHTNING|GROUP|ANIMAL)
	,(NONE8),(NONE8)
    ,65,20,8,80,26,'b',{12, 12},{29,29,156,0},13,2,'á'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Killer brown beetle"      ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
	,38,30,10,40,20,'K',{13,8},{41,0,0,0},13,2,'õ'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"The Gooey Kablooie"   ,(CARRY_OBJ|HAS_90|HAS_1D2|THRO_DR|
			     MV_ATT_NORM),(0x3L|BLINK|TELE|MONSTER|
		 BLINDNESS),(MAX_HP|IM_ACID|IM_POISON|
			     INTELLIGENT|ANIMAL|EVIL|UNIQUE|GOOD|CAN_SPEAK),(HEAL)
      ,(NONE8),210,10,18,24,35,'j',{20,9},{118,140,170,0},13,2,'©'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Ogre"                     ,(HAS_60|CARRY_GOLD|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
			    ,(NONE8),(EVIL|GROUP|GIANT),(NONE8),(NONE8)
	   ,50,30,20,33,20,'O',{13,9},{16,0,0,0},13,2,'®'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Creeping mithril coins"   ,(MV_ATT_NORM|HAS_2D2|HAS_90|CARRY_GOLD),(NONE8)
		,(IM_POISON|NO_INFRA),(NONE8),(NONE8)
       ,45,10,5,50,21,'$',{20,8},{14,172,0,0},13,4,'∫'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Illusionist"              ,(HAS_1D2|CARRY_GOLD|CARRY_OBJ|THRO_DR|
			      MV_ATT_NORM),(0x3L|BLINK|TELE|BLINDNESS|
			      CONFUSION|SLOW|HOLD_PERSON),(EVIL|INTELLIGENT)
			    ,(HASTE|DARKNESS),(NONE8)
    ,50,10,20,10,22,'p',{12,8},{11,0,0,0},13,2,'¯'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Druid"                    ,(MV_ATT_NORM|CARRY_OBJ|CARRY_GOLD|HAS_1D2|
			     THRO_DR),(0x3L|BLINK|HOLD_PERSON|BLINDNESS|
			     SLOW|FIRE_BOLT),(EVIL|INTELLIGENT)
			    ,(HASTE|LIGHT_BOLT),(NONE8)
    ,50,10,20,10,22,'p',{12,12},{13,13,0,0},13,2,'¯'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Black orc"                ,(HAS_60|CARRY_GOLD|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
		,(0xCL),(EVIL|ORC|GROUP|HURT_LIGHT)
		,(NONE8),(ARROW)
    ,45,20,20,36,22,'o',{12,10},{17,17,0,0},13,2,'Û'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Ochre jelly"              ,(THRO_DR|MV_ATT_NORM|PICK_UP),(NONE8)
			    ,(IM_ACID|IM_POISON|ANIMAL|CHARM_SLEEP|NO_INFRA|MINDLESS)
			    ,(NONE8),(NONE8)
    ,40,1,12,18,33,'j',{13,8},{115,138,138,0},13,3,'©'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Software bug"           ,(MULTIPLY|MV_ATT_NORM|MV_75),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
    ,4,10,8,25,34,'F',{2,2},{25,0,0,0},14,1,'Ù'
#ifdef TC_COLOR
  , RED
#endif
},

{"Stupendous Man"   ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
		,(NONE8),(EVIL|IM_POISON|IM_FROST|MAX_HP|UNIQUE|GOOD
		|CAN_SPEAK),(NONE8),(NONE8)
    ,200,20,20,50,24,'p',{40,8},{17,17,17,17},14,3,'‡'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Giant white dragon fly"   ,(MV_ATT_NORM|MV_40),(BREATH_FR|0xAL)
			    ,(ANIMAL|IM_FROST|NO_INFRA|SEMI),(NONE8),(NONE8)
    ,60,50,20,20,22,'F',{5,8},{122,0,0,0},14,3,'Ù'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Gibbering mouther"          ,(MV_ONLY_ATT|MULTIPLY)
		,(0x7L|FEAR|CONFUSION)
		,(ANIMAL|IM_POISON|MINDLESS),(NONE8),(BREATH_LT)
    ,20,20,15,20,18,'j',{8,6},{90,90,0,0},14,4,'©'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},
{"Care bear"              ,(CARRY_GOLD|CARRY_OBJ|HAS_60|THRO_DR|
		 MV_ATT_NORM),(0x7L|BLINK|TELE|MONSTER|
		 BLINDNESS|SLOW),(ANIMAL|EVIL|GROUP),(ST_CLOUD),(NONE8)
    ,28,10,18,24,19,'h',{12,9},{7,0,0,0},14,2,'ê'
#ifdef TC_COLOR
  , YELLOW
#endif
},
{"Hill giant"               ,(MV_ATT_NORM|HAS_60|CARRY_GOLD|CARRY_OBJ|
			     THRO_DR|MV_ATT_NORM),(NONE8),(EVIL|GIANT)
			    ,(NONE8),(NONE8)
    ,60,50,20,45,19,'P',{16,10},{19,19,0,0},14,1,'ë'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Irish wolfhound of Flora"
	 ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|GROUP|FEARLESS)
			    ,(NONE8),(NONE8)
    ,40,8,20,20,32,'C',{8,8},{31,0,0,0},12,2,'Ë'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Cheerful leprechaun"        ,(MULTIPLY|HAS_60|CARRY_GOLD
		|MV_ATT_NORM|MV_40),(0x6L|BLINK),(NONE8)
			    ,(NONE8),(NONE8)
    ,23,8,6,7,31,'l',{2,5},{148,0,0,0},14,2,''
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Lurker"                   ,(MV_ONLY_ATT|MV_INVIS),(NONE8)
			    ,(NO_INFRA|CHARM_SLEEP|MAX_HP|MINDLESS),(NONE8),(NONE8)
    ,80,10,30,25,22,'.',{20,10},{7,7,0,0},14,3,''
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Vorpal bunny"                  ,(MV_ATT_NORM)
		,(0x6L|BLINK)
		,(EVIL|ANIMAL|SEMI),(NONE8),(NONE8)
    ,45,10,10,10,35,'r',{18,8},{36,36,0,0},15,2,'ˆ'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Black ogre"               ,(HAS_60|CARRY_GOLD|CARRY_OBJ|THRO_DR
			      |MV_ATT_NORM|MV_20)
			    ,(NONE8),(EVIL|GROUP|GIANT),(NONE8),(NONE8)
    ,75,30,20,33,21,'O',{20,9},{16,16,0,0},15,2,'®'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Magic mushroom patch"     ,(MV_ONLY_ATT),(0x1L|BLINK|FEAR|SLOW|MANA_DRAIN)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(DARKNESS),(NONE8)
    ,10,0,40,10,42,',',{1,1},{0,0,0,0},15,3,'è'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Guardian naga"            ,(HAS_1D2|CARRY_GOLD|CARRY_OBJ|HAS_60|
			     MV_ATT_NORM|MV_20),(NONE8),(EVIL),(NONE8)
			     ,(NONE8)
    ,80,120,20,65,24,'n',{24,11},{77,31,31,0},15,2,'•'
#ifdef TC_COLOR
  , YELLOW
#endif
},


{"Light hound"
		,(THRO_DR|MV_ATT_NORM),(0x5L),(ANIMAL|GROUP),(NONE8)
			    ,(BREATH_LT)
	,50,0,30,30,20,'Z',{6,6},{29,0,0,0},15,1,'≥'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Shadow hound"
		,(THRO_DR|MV_ATT_NORM),(0x5L),(ANIMAL|GROUP|HURT_LIGHT),(NONE8)
			    ,(BREATH_DA)
	,50,0,30,30,20,'Z',{6,6},{29,0,0,0},15,1,'≥'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Flying skull"                 ,(HAS_60|CARRY_GOLD|CARRY_OBJ
		|THRO_DR|MV_ATT_NORM)
		,(NONE8),(EVIL|UNDEAD|IM_POISON|IM_FROST
		|CHARM_SLEEP|GROUP|NO_INFRA|FEARLESS|SEMI),(NONE8),(NONE8)
    ,50,20,20,40,22,'s',{10,10},{83,156,0,0},15,3,'∂'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Giant tarantula"          ,(MV_ATT_NORM),(NONE8),(ANIMAL|IM_POISON|SEMI)
			    ,(NONE8),(NONE8)
    ,70,80,8,32,35,'S',{10,15},{156,156,156,0},15,3,'˜'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Roadrunner"    ,(MV_75|MV_ATT_NORM),(NONE8),(ANIMAL)
			    ,(NONE8),(NONE8)
    ,30,30,12,30,77,'B',{5,8},{34,34,0,0},15,2,'‘'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Mirkwood spider"          ,(MV_ATT_NORM),(NONE8),(ANIMAL|GROUP|IM_POISON|
		EVIL|HURT_LIGHT|SEMI),(NONE8),(NONE8)
    ,25,80,15,25,33,'S',{9,8},{31,156,156,0},15,2,'˜'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Frost giant"              ,(MV_ATT_NORM|HAS_60|CARRY_GOLD|CARRY_OBJ|
			     THRO_DR|MV_ATT_NORM),(NONE8)
			    ,(EVIL|IM_FROST|NO_INFRA|GIANT),(NONE8),(NONE8)
    ,75,50,20,50,22,'P',{17,10},{120,16,0,0},15,1,'ë'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Griffon"                  ,(MV_ATT_NORM),(NONE8),(ANIMAL),(NONE8),(NONE8)
	,70,10,12,15,20,'H',{30,8},{17,36,0,0},15,1,'ö'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Dweller on the threshold"
		,(MV_ONLY_ATT|HAS_60|CARRY_GOLD|CARRY_OBJ)
		,(0x6L|FEAR|ACID_BOLT|MANA_DRAIN|MONSTER)
		,(DEMON|EVIL|IM_FIRE|IM_POISON|IM_FROST|CHARM_SLEEP)
		,(NONE8),(NONE8)
    ,40,30,20,32,25,'Y',{30,8},{145,9,9,0},15,3,'æ'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Gnome mage"               ,(CARRY_OBJ|CARRY_GOLD|HAS_60|MV_ATT_NORM|THRO_DR)
			    ,(0x4L|BLINK|FROST_BOLT|MONSTER),(EVIL|GROUP)
			    ,(DARKNESS),(NONE8)
    ,40,20,20,20,19,'h',{7,8},{4,0,0,0},15,2,'ê'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Ethereal hound"
		,(THRO_DR|MV_ATT_NORM|MV_INVIS),(NONE8),(ANIMAL|GROUP)
			    ,(NONE8),(NONE8)
	,50,0,30,30,20,'Z',{10,6},{29,29,29,0},15,2,'≥'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Mi-Go"               ,(MV_ATT_NORM),(0x8L|CONFUSION|MONSTER|S_DEMON),
		 (IM_FROST|EVIL|ANIMAL|
			      IM_POISON|NO_INFRA|CHARM_SLEEP),(NONE8),(NONE8)
    ,80,10,12,30,35,'i',{13,8},{7,28,0,0},15,2,'Â'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Umber hulk"               ,(MV_ATT_NORM),(NONE8)
		,(EVIL|BREAK_WALL|HURT_ROCK|IM_POISON|
			     NO_INFRA),(NONE8),(NONE8)
    ,75,10,20,50,22,'U',{20,10},{92,5,5,36},16,1,'ä'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Orc captain"              ,(HAS_90|CARRY_GOLD|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
		,(0x8L),(EVIL|ORC),(NONE8),(ARROW)
    ,40,20,20,59,24,'o',{20,10},{17,17,17,0},16,3,'Û'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Gelatinous cube"          ,(THRO_DR|MV_ATT_NORM|PICK_UP|HAS_4D2|
			      CARRY_GOLD|CARRY_OBJ|HAS_60|HAS_90),(NONE8)
			    ,(IM_ACID|IM_FIRE|IM_LIGHTNING|IM_POISON|IM_FROST|
			      ANIMAL|CHARM_SLEEP|MAX_HP|NO_INFRA|MINDLESS)
			    ,(NONE8),(NONE8)
    ,80,1,12,18,21,'j',{45,8},{115,115,115,0},16,4,'Ê'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Giant green dragon fly"   ,(MV_ATT_NORM|MV_75),(BREATH_G|0xAL),
			     (IM_POISON|ANIMAL|SEMI),(NONE8),(NONE8)
    ,70,50,12,20,22,'F',{3,8},{156,0,0,0},16,2,'Ù'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Fire giant"               ,(MV_ATT_NORM|HAS_60|CARRY_GOLD|CARRY_OBJ|
			     THRO_DR|MV_ATT_NORM)
			    ,(NONE8),(EVIL|IM_FIRE|GIANT),(NONE8),(NONE8)
    ,54,50,20,60,22,'P',{20,8},{102,102,0,0},16,2,'ë'
#ifdef TC_COLOR
  , RED
#endif
},

{"The Smithess"     ,(HAS_90|CARRY_OBJ|THRO_DR|PICK_UP|
		  MV_ATT_NORM),(NONE8),(EVIL|UNIQUE|GOOD|MAX_HP
		  |CHARM_SLEEP|NO_INFRA|MINDLESS|FEARLESS|NONLIVING)
			     ,(NONE8),(NONE8)
     ,200,40,20,40,23,'g',{20,17},{18,18,18,18},16,3,''
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Quasit"                   ,(MV_ATT_NORM|MV_20|MV_INVIS|
			      CARRY_OBJ|HAS_1D2),(0xAL|BLINK|TELE_TO|TELE|FEAR
			     |CONFUSION|BLINDNESS)
			    ,(INTELLIGENT|DEMON|IM_FIRE|EVIL)
			    ,(TELE_LEV),(NONE8)
    ,50,20,20,30,21,'I',{6,8},{176,51,51,0},16,2,'Ì'
#ifdef TC_COLOR
  , RED
#endif
},

{"Deep one"                       ,(MV_ATT_NORM|MV_20|
			      CARRY_OBJ|HAS_1D2),(0xAL|BLINK|TELE_TO|TELE|FEAR
			     |CONFUSION|BLINDNESS),
		 (DEMON|IM_FIRE|EVIL|NO_INFRA|GROUP|HURT_LIGHT)
			    ,(TELE_LEV),(NONE8)
    ,50,20,20,30,21,'I',{6,6},{152,152,0,0},17,2,'Ì'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Forest troll"             ,(MV_ATT_NORM|THRO_DR|HAS_60|CARRY_GOLD|
			     CARRY_OBJ),(NONE8),(TROLL|EVIL|HURT_LIGHT|GROUP)
			    ,(NONE8),(NONE8)
	,70,40,20,50,20,'T',{20,10},{3,3,29,0},17,1,'∏'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Spaceman Spiff"           ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|CARRY_OBJ|
		 HAS_1D2),(0x6L|FIRE_BOLT|BLINDNESS|CONFUSION)
			    ,(CHARM_SLEEP|IM_POISON|IM_FIRE|IM_FROST|GOOD|
		 MAX_HP|UNIQUE|CAN_SPEAK),(MIND_BLAST|HEAL),(NUKE_BALL)
    ,250,25,25,70,24,'p',{45,10},{18,18,18,18},17,2,'‡'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Sphinx"           ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_1D2)
			    ,(0xBL|FEAR),(ANIMAL),(NONE8),(NONE8)
    ,80,20,20,60,22,'H',{100,3},{53,54,36,0},17,2,'ö'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Water spirit"             ,(MV_ATT_NORM|MV_20),(NONE8),
			     (EVIL|IM_POISON|NO_INFRA|CHARM_SLEEP|MINDLESS
			     |NONLIVING)
			    ,(NONE8),(NONE8)
    ,58,40,12,28,32,'E',{9,8},{13,13,0,0},17,1,'ç'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Giant brown scorpion"     ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,62,20,12,44,22,'S',{11,8},{34,86,0,0},17,1,'∑'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Earth spirit"             ,(MV_ATT_NORM|MV_20|THRO_WALL|THRO_DR|PICK_UP)
			    ,(NONE8),(EVIL|HURT_ROCK|IM_POISON|NO_INFRA|
			     IM_FIRE|IM_FROST|IM_LIGHTNING|CHARM_SLEEP|MINDLESS
			     |NONLIVING)
			    ,(NONE8),(NONE8)
    ,64,50,10,40,31,'E',{13,8},{7,7,0,0},17,2,'ç'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Fire spirit"              ,(MV_ATT_NORM|MV_20),
			     (NONE8),(EVIL|IM_POISON|IM_FIRE|CHARM_SLEEP
			     |MINDLESS|NONLIVING)
			    ,(NONE8),(NONE8)
    ,75,20,16,30,31,'E',{10,9},{101,101,0,0},18,2,'ç'
#ifdef TC_COLOR
  , RED
#endif
},

{"Fire hound"
		,(THRO_DR|MV_ATT_NORM),(0xAL|BREATH_FI)
			    ,(ANIMAL|GROUP|IM_FIRE),(NONE8)
			    ,(NONE8)
    ,70,0,30,30,21,'Z',{10,6},{105,105,105,0},18,1,'≥'
#ifdef TC_COLOR
  , RED
#endif
},

{"Cold hound"
		,(THRO_DR|MV_ATT_NORM),(0xAL|BREATH_FR)
			    ,(ANIMAL|GROUP|IM_FROST|NO_INFRA),(NONE8)
		,(NONE8)
    ,70,0,30,30,21,'Z',{10,6},{122,54,29,0},18,1,'≥'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Energy hound"
	    ,(THRO_DR|MV_ATT_NORM),(0xAL|BREATH_L)
			    ,(ANIMAL|GROUP|IM_LIGHTNING),(NONE8)
			    ,(NONE8)
    ,70,0,30,30,21,'Z',{10,6},{131,131,131,0},18,1,'≥'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Mimic"                    ,(MV_ONLY_ATT),(0x6L|FROST_BOLT|BLINDNESS|FEAR|
			     CONFUSION|CAUSE_SERIOUS),(NO_INFRA|CHARM_SLEEP)
			    ,(NONE8),(NONE8)
    ,60,0,25,30,22,'!',{10,10},{152,12,12,0},18,3,'≠'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Blink dog"                ,(MV_ATT_NORM|MV_20),(0x4L|BLINK|TELE_TO)
			    ,(ANIMAL|GROUP),(NONE8),(NONE8)
    ,50,10,20,20,33,'C',{8,8},{31,0,0,0},18,2,'Ë'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Uruk"             ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|CARRY_OBJ|HAS_60)
		,(0xAL),(ORC|EVIL|IM_POISON|MAX_HP|GROUP)
		,(NONE8),(ARROW)
    ,68,20,20,50,22,'o',{10,8},{18,18,0,0},18,1,'Û'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Giant bronze dragon fly"  ,(MV_ATT_NORM|MV_75),(0x9L),
			     (CHARM_SLEEP|ANIMAL|SEMI),(BREATH_CO),(NONE8)
    ,70,50,12,20,32,'F',{3,8},{0,0,0,0},18,1,'Ù'
#ifdef TC_COLOR
  , BROWN
#endif
},
{"Gobbledigook", (MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
			     ,(NONE8),(ORC|EVIL|IM_POISON|MAX_HP|UNIQUE|GOOD|CAN_SPEAK)
			     ,(NONE8),(NONE8)
     ,400,20,20,60,23,'o',{40,10},{22,22,18,268},18,2,'Ô'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Frankenstein's Monster", (MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
		 ,(NONE8),(EVIL|IM_POISON|MAX_HP|UNIQUE|GOOD|IM_LIGHTNING
		 )
			     ,(NONE8),(NONE8)
     ,400,20,20,60,25,'g',{40,10},{20,20,18,18},18,3,''
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Shambling mound"          ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_90)
			    ,(NONE8),(CHARM_SLEEP|ANIMAL|EVIL|MINDLESS),(NONE8),(NONE8)
    ,75,40,20,16,22,',',{20,6},{203,7,7,0},18,2,'è'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Stone giant"              ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|
			     CARRY_OBJ|HAS_60),(NONE8),(EVIL|GIANT)
			    ,(NONE8),(NONE8)
    ,90,50,20,75,22,'P',{24,8},{20,20,0,0},18,1,'ë'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Giant black dragon fly"   ,(MV_ATT_NORM|MV_75),(BREATH_A|0x9L),
			     (IM_ACID|ANIMAL|SEMI),(NONE8),(NONE8)
    ,68,50,12,20,32,'F',{3,8},{0,0,0,0},18,2,'Ù'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Stone golem"              ,(MV_ATT_NORM),(NONE8),(HURT_ROCK|IM_FIRE|
			     IM_FROST|IM_LIGHTNING|IM_POISON|CHARM_SLEEP|
			     NO_INFRA|FEARLESS|MINDLESS|NONLIVING),(NONE8),(NONE8)
    ,100,10,12,75,16,'g',{28,8},{9,9,0,0},19,2,''
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},



{"Red mold"                 ,(MV_ONLY_ATT),(NONE8),(IM_FIRE|IM_POISON|ANIMAL)
			    ,(NONE8),(NONE8)
    ,64,70,2,16,21,'m',{17,8},{108,0,0,0},19,1,'Ó'
#ifdef TC_COLOR
  , RED
#endif
},

{"Giant brass dragon fly"    ,(MV_ATT_NORM|MV_75),(0x9L),
			     (IM_FIRE|ANIMAL|SEMI),(BREATH_SD),(NONE8)
    ,78,50,12,20,28,'F',{3,8},{26,0,0,0},18,2,'Ù'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Bolg, Son of Azog"        ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_2D2)
			    ,(NONE8),(ORC|EVIL|IM_POISON|MAX_HP|UNIQUE|GOOD|CAN_SPEAK)
			    ,(NONE8),(NONE8)
    ,800,20,20,50,29,'o',{50,10},{19,19,19,19},20,4,'Û'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Phase spider"             ,(MV_ATT_NORM),(0x5L|BLINK|TELE_TO)
			    ,(ANIMAL|GROUP|IM_POISON|SEMI),(NONE8),(NONE8)
    ,60,80,15,25,33,'S',{6,8},{31,156,156,0},20,2,'˜'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Wyvern"           ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_60|HAS_90)
		,(NONE8),(ANIMAL|DRAGON|IM_POISON|EVIL),(NONE8),(NONE8)
    ,350,20,20,65,31,'d',{100,5},{53,53,33,166},20,2,'ù'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Livingstone"            ,(MV_ONLY_ATT|MULTIPLY)
		,(NONE8),(IM_FROST|NO_INFRA|IM_ACID|IM_FIRE|IM_LIGHTNING
	 |IM_POISON|CHARM_SLEEP|HURT_ROCK|GROUP),(NONE8),(NONE8)
    ,56,75,20,28,35,'±',{6,8},{13,13,0,0},20,4,'±'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Earth hound"              ,(THRO_DR|MV_ATT_NORM),(0xAL)
			    ,(ANIMAL|GROUP),(BREATH_SH)
			    ,(NONE8)
    ,200,0,30,30,22,'Z',{15,8},{31,31,58,58},20,1,'≥'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Air hound"                ,(THRO_DR|MV_ATT_NORM),(0xAL|BREATH_G)
			    ,(ANIMAL|GROUP|IM_POISON),(NONE8)
			    ,(NONE8)
    ,200,0,30,30,22,'Z',{15,8},{31,31,58,58},20,1,'≥'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Sabre-tooth tiger"        ,(MV_ATT_NORM),(NONE8),(ANIMAL),(NONE8),(NONE8)
    ,120,0,40,50,32,'f',{20,14},{56,56,32,32},20,2,'Í'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Water hound"              ,(THRO_DR|MV_ATT_NORM),(0xAL|BREATH_A)
			    ,(ANIMAL|GROUP|IM_ACID),(NONE8)
			    ,(NONE8)
    ,200,0,30,30,22,'Z',{15,8},{113,113,58,58},20,2,'≥'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Chimera"                  ,(MV_ATT_NORM),(0xAL|BREATH_FI),(IM_FIRE)
			    ,(NONE8),(NONE8)
    ,200,10,12,15,22,'H',{13,8},{32,105,105,0},20,1,'ö'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Quylthulg"                ,(MV_INVIS),(0x4L|BLINK|MONSTER),(CHARM_SLEEP
				|FEARLESS)
			    ,(NONE8),(NONE8)
    ,250,0,10,1,21,'Q',{6,8},{0,0,0,0},20,1,'ú'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Sasquatch"                ,(MV_ATT_NORM|THRO_DR),(NONE8),(ANIMAL|IM_FROST|
			    NO_INFRA),(NONE8),(NONE8)
    ,180,10,15,40,32,'Y',{20,19},{56,56,37,0},20,3,'æ'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Weir"                 ,(MV_ATT_NORM|MV_20|THRO_DR)
		,(NONE8),(ANIMAL|GROUP),(NONE8),(NONE8)
    ,150,70,15,30,21,'C',{20,22},{29,29,32,0},23,1,'Ë'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Dark elven lord"          ,(CARRY_OBJ|HAS_2D2|THRO_DR|MV_ATT_NORM)
			    ,(0x5L|BLINDNESS|FROST_BOLT|FIRE_BOLT|CONFUSION)
			    ,(EVIL|HURT_LIGHT),(HASTE|DARKNESS),(NONE8)
    ,500,30,20,40,33,'h',{18,15},{20,18,0,0},20,2,'ﬂ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Cloud giant"              ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|
			      CARRY_GOLD|HAS_90)
			    ,(NONE8),(EVIL|GIANT|IM_LIGHTNING),(NONE8),(NONE8)
    ,125,50,20,60,22,'P',{24,10},{130,130,0,0},20,1,'ë'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Ugluk, the Uruk"          ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
		 ,(0x9L),(ORC|EVIL|IM_POISON|MAX_HP|UNIQUE|GOOD|CAN_SPEAK)
		 ,(NONE8),(ARROW)
    ,550,20,20,90,22,'o',{40,16},{18,18,18,18},20,4,'Û'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Lockheed, the cute little dragon"
		,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
		,(0x4L|BREATH_FI),(DRAGON|IM_FIRE|
			     MAX_HP|GOOD|UNIQUE|CHARM_SLEEP)
			    ,(NONE8),(NONE8)
    ,550,20,20,95,23,'d',{30,10},{57,57,36,0},21,3,''
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Blue dragon bat"          ,(MV_ATT_NORM|MV_40),(BREATH_L|0x4L)
			    ,(ANIMAL|IM_LIGHTNING),(NONE8),(NONE8)
    ,54,50,12,26,38,'b',{4,4},{131,0,0,0},21,1,'á'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Pole axe of animated attack"
		       ,(MV_ATT_NORM|HAS_90|CARRY_OBJ|CARRY_GOLD),(0x6L|FEAR|
			     BLINDNESS|CONFUSION|MONSTER),(NO_INFRA|FEARLESS|
			     CHARM_SLEEP|NONLIVING|MINDLESS),(NONE8),(NONE8)
    ,60,0,30,40,29,'/',{10,14},{14,14,15,15},21,3,'Ω'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Fire vortex"              ,(MV_ATT_NORM|MV_40),(BREATH_FI|0x6L)
			    ,(IM_FIRE|CHARM_SLEEP|MINDLESS|NONLIVING),(NONE8),(NONE8)
    ,100,0,100,30,21,'v',{9,9},{239,0,0,0},21,1,'˙'
#ifdef TC_COLOR
  , RED
#endif
},

{"Water vortex"             ,(MV_ATT_NORM|MV_40),(BREATH_A|0x6L)
			    ,(IM_ACID|CHARM_SLEEP|MINDLESS|NONLIVING),(NONE8),(NONE8)
    ,100,0,100,30,21,'v',{9,9},{240,0,0,0},21,1,'˙'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Cold vortex"              ,(MV_ATT_NORM|MV_40),(BREATH_FR|0x6L)
			    ,(IM_FROST|CHARM_SLEEP|NO_INFRA|MINDLESS|NONLIVING),(NONE8),(NONE8)
    ,100,0,100,30,21,'v',{9,9},{241,0,0,0},21,1,'˙'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Energy vortex"            ,(MV_ATT_NORM|MV_40),(BREATH_L|0x6L)
			    ,(IM_LIGHTNING|CHARM_SLEEP|MINDLESS|NONLIVING),(NONE8),(NONE8)
    ,130,0,100,30,21,'v',{12,12},{242,0,0,0},21,1,'˙'
#ifdef TC_COLOR
  , YELLOW
#endif
},



{"Killer stag beetle"       ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
	,80,30,12,55,20,'K',{20,8},{41,10,0,0},22,1,'õ'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Autoroller"               ,(MV_ATT_NORM),(NONE8)
			    ,(IM_FIRE|IM_FROST|IM_LIGHTNING|IM_POISON|
			      NO_INFRA|CHARM_SLEEP|NONLIVING|FEARLESS
			      |MINDLESS),(NONE8),(NONE8)
    ,230,10,12,80,32,'g',{70,12},{73,73,73,0},22,2,'œ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Giant yellow scorpion"    ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8)
			    ,(NONE8)
    ,60,20,12,38,22,'S',{12,8},{31,167,0,0},22,1,'∑'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Black ooze"               ,(CARRY_GOLD|CARRY_OBJ|HAS_60|PICK_UP|MULTIPLY|
			      THRO_DR|THRO_CREAT|MV_ATT_NORM|MV_40)
			    ,(0xBL|MANA_DRAIN),(IM_POISON|ANIMAL|MINDLESS),(NONE8)
			    ,(NONE8)
    ,7,1,10,6,20,'j',{6,8},{138,0,0,0},23,1,'©'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Gug"         ,(HAS_1D2|CARRY_GOLD|CARRY_OBJ|THRO_DR|PICK_UP|
			      MV_ATT_NORM),(NONE8),(EVIL),(NONE8),(NONE8)
    ,60,40,20,40,25,'H',{15,11},{18,18,0,0},23,1,'≤'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Azog, King of the Uruk-Hai",(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_2D2)
			   ,(NONE8),(ORC|EVIL|IM_POISON|MAX_HP|UNIQUE|GOOD|CAN_SPEAK)
			   ,(NONE8),(NONE8)
       ,1111,20,20,80,33,'o',{60,15},{23,23,23,0},23,5,'Û'
#ifdef TC_COLOR
  , WHITE
#endif
},


{"Red dragon bat"           ,(MV_ATT_NORM|MV_40),(BREATH_FI|0x4L)
			    ,(IM_FIRE|ANIMAL),(NONE8),(NONE8)
	,60,50,12,28,40,'b',{3,8},{105,0,0,0},23,1,'á'
#ifdef TC_COLOR
  , RED
#endif
},

{"Killer blue beetle"       ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI),(NONE8)
      ,(NONE8),85,30,14,55,22,'K',{20,8},{44,0,0,0},23,1,'õ'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},



{"Kouko"             ,(MV_ATT_NORM|MV_20|THRO_DR|CARRY_GOLD|
			      CARRY_OBJ|HAS_60|HAS_90)
			    ,(0xAL|FEAR|MANA_DRAIN)
			    ,(EVIL|NO_INFRA|UNDEAD|IM_FROST|IM_POISON|
			      HURT_LIGHT|CHARM_SLEEP),(NONE8),(NONE8)
    ,140,30,20,30,21,'W',{12,8},{5,5,187,0},24,1,'Ø'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Mime, the Nibelung"     ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
		 HAS_2D2),(0x8L|FIRE_BOLT),(UNIQUE|IM_FROST|IM_LIGHTNING|
		 EVIL|GOOD|HURT_LIGHT|CHARM_SLEEP|MAX_HP|CAN_SPEAK),(HEAL|HASTE),
		 (NONE8)
    ,300,10,20,80,24,'h',{55,15},{19,19,204,152},24,2,'ê'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Hagen, Son of Alberich"         ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
			    ,(0x8L|SLOW|FIRE_BOLT),(UNIQUE|CAN_SPEAK|IM_FROST|IM_FIRE|
			     EVIL|GOOD|CHARM_SLEEP|MAX_HP),(HEAL),(NONE8)
    ,300,10,20,80,24,'h',{55,15},{19,19,19,204},24,2,'ê'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Tyrannosaurus"       ,(MV_ATT_NORM|THRO_DR)
		,(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,380,20,20,70,29,'R',{100,6},{57,57,36,37},24,2,'à'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Mummified human"          ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_90)
		,(NONE8),(EVIL|NO_INFRA|UNDEAD|IM_FROST|
			     IM_POISON|CHARM_SLEEP|FEARLESS|MINDLESS),(NONE8),(NONE8)
    ,70,60,20,34,22,'M',{17,9},{13,13,0,0},24,1,'°'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Vampire bat"              ,(MV_ATT_NORM|MV_40),(NONE8)
		,(EVIL|UNDEAD|IM_FROST|IM_POISON|HURT_LIGHT|
			     CHARM_SLEEP|NO_INFRA|SEMI),(NONE8),(NONE8)
    ,150,50,12,40,29,'b',{9,10},{236,236,0,0},24,2,'á'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"It"     ,(MV_ATT_NORM|CARRY_OBJ|HAS_90|HAS_1D2|THRO_DR|MV_INVIS)
		,(0x4L|MANA_DRAIN|FEAR|CONFUSION|MONSTER|S_UNDEAD
		    |BLINDNESS|BLINK),
		(CHARM_SLEEP|IM_FIRE|UNIQUE|EVIL|
		 IM_LIGHTNING|SPECIAL|MAX_HP|NO_INFRA|INTELLIGENT|CAN_SPEAK|MINDLESS)
		 ,(FORGET|TRAP_CREATE|DARKNESS|S_ANGEL|HEAL|TELE_AWAY),
		 (NONE8)
    ,380,25,25,80,33,'˘',{77,8},{196,183,84,203},24,3,'.'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Mean Machine"       ,(MV_ATT_NORM|CARRY_OBJ|HAS_1D2|HAS_90|THRO_DR)
		,(NONE8),(CHARM_SLEEP|IM_FIRE|IM_LIGHTNING|
		 GOOD|EVIL|MAX_HP|UNIQUE),(NONE8),(NONE8)
    ,400,25,25,80,31,'p',{90,14},{227,227,227,0},24,2,'‡'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Banshee"                  ,(MV_ATT_NORM|MV_20|MV_40|CARRY_GOLD|CARRY_OBJ|
			     HAS_1D2|THRO_WALL|MV_INVIS|PICK_UP)
			    ,(0xFL|TELE|MANA_DRAIN)
			    ,(NO_INFRA|UNDEAD|EVIL|IM_FROST|
			     CHARM_SLEEP|IM_POISON),(NONE8),(NONE8)
    ,60,10,20,24,32,'G',{6,8},{99,188,0,0},24,2,'Ò'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Silent Watcher"                 ,(MV_ONLY_ATT)
		,(ACID_BOLT|CONFUSION|0x6L|MONSTER|HOLD_PERSON)
		,(IM_FIRE|IM_FROST|IM_LIGHTNING|IM_POISON|EVIL|
		  NO_INFRA|CHARM_SLEEP|HURT_ROCK|HURT_LIGHT|FEARLESS|NONLIVING)
		  ,(SUMMON|DARKNESS|MIND_BLAST),(NONE8)
    ,600,5,12,80,22,'g',{80,20},{10,19,203,203},25,3,''
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Dark elven druid"         ,(MV_ATT_NORM|CARRY_OBJ|HAS_1D2|THRO_DR)
			    ,(0x6L|MONSTER|CONFUSION),(IM_POISON|
			     CHARM_SLEEP|EVIL|HURT_LIGHT),
				(HEAL|S_SPIDER|DARKNESS),(NONE8)
    ,500,10,15,75,31,'h',{20,20},{6,6,20,0},25,3,'ﬂ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},



{"Stone troll"              ,(MV_ATT_NORM|THRO_DR|HAS_60|CARRY_GOLD|
			     CARRY_OBJ),(NONE8)
			    ,(TROLL|EVIL|HURT_LIGHT|HURT_ROCK|GROUP)
			    ,(NONE8),(NONE8)
    ,85,50,20,40,22,'T',{23,10},{5,5,41,0},25,1,'∏'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"X-man"             ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|CARRY_OBJ|MV_20|
		 HAS_90),(0x8L|BLINK|FEAR|FIRE_BOLT|FROST_BOLT)
		,(EVIL|CHARM_SLEEP)
		,(LIGHT_BOLT|MIND_BLAST|PLASMA_BOLT|FORGET),(NONE8)
    ,160,30,20,50,21,'x',{23,11},{7,7,8,0},25,3,'◊'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Wereworm"                 ,(MV_ATT_NORM),(NONE8),(IM_ACID|IM_POISON|ANIMAL
		    |EVIL|SEMI)
			    ,(NONE8),(NONE8)
        ,300,20,15,70,22,'w',{100,11},{32,139,224,156},
				    25,3,'˚'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Carrion crawler"          ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|IM_POISON|SEMI)
			    ,(NONE8),(NONE8)
    ,60,10,15,40,19,'c',{20,12},{253,253,0,0},25,2,'û'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Killer red beetle"        ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|SEMI)
			    ,(NONE8),(NONE8)
    ,85,30,14,50,22,'K',{20,8},{84,0,0,0},25,2,'õ'
#ifdef TC_COLOR
  , RED
#endif
},

{"Triceratops"      ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL)
			    ,(NONE8),(NONE8)
    ,90,40,10,40,25,'q',{19,8},{228,228,0,0},26,1,'à'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Kharis the Mummy"      ,(HAS_90|HAS_1D2|CARRY_OBJ|THRO_DR|
		  MV_ATT_NORM),(0x5L|CAUSE_SERIOUS|SLOW|FEAR
		  |S_UNDEAD|MANA_DRAIN|HOLD_PERSON)
		  ,(EVIL|UNIQUE|UNDEAD|NO_INFRA|IM_POISON|IM_FROST|GOOD|CAN_SPEAK
		  |FEARLESS)
			    ,(NONE8),(NONE8)
    ,800,40,20,40,24,'M',{80,11},{183,172,172,0},26,4,'°'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Displacer beast"          ,(MV_ATT_NORM|MV_INVIS),(NONE8),(ANIMAL)
			    ,(NONE8),(NONE8)
    ,100,20,35,100,27,'f',{25,10},{37,9,9,9},26,2,'Í'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Agent of Benedict"          ,(MV_ATT_NORM|MV_20)
		,(0x8L|TELE|BLINDNESS|CONFUSION),(GROUP)
			    ,(NONE8),(NONE8)
    ,100,20,14,54,22,'p',{16,8},{8,8,0,0},26,3,'‡'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Ghoul"                ,(HAS_60|CARRY_GOLD|CARRY_OBJ|THRO_DR
			      |MV_ATT_NORM)
		,(0x9L|FEAR),(EVIL|GROUP|UNDEAD|HURT_LIGHT
		|NO_INFRA|IM_POISON|IM_FROST|CHARM_SLEEP),(NONE8),(NONE8)
    ,95,30,20,33,19,'G',{15,9},{145,145,160,0},26,1,'Ò'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"White wraith"             ,(CARRY_GOLD|CARRY_OBJ|HAS_1D2|THRO_DR|
		 MV_ATT_NORM),(0x8L|FEAR|CAUSE_SERIOUS)
			    ,(UNDEAD|EVIL|NO_INFRA|IM_FROST|IM_POISON|
			      CHARM_SLEEP|HURT_LIGHT),(DARKNESS),(NONE8)
    ,175,10,20,40,21,'W',{15,8},{5,5,189,0},26,1,'Ø'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Angel"         ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      HAS_2D2)
			    ,(0x3L|FEAR|BLINDNESS|CONFUSION)
			    ,(IM_POISON|IM_ACID|CHARM_SLEEP|MAX_HP
			    |FEARLESS)
			    ,(FORGET),(NONE8)
		,220,255,30,60,20,'A',{25,12}
		,{17,17,17,17},26,6,'Ü'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},


{"$crooge McDuck"      ,(HAS_90|HAS_60|HAS_1D2|HAS_2D2|HAS_4D2|CARRY_GOLD|
		THRO_DR|MV_ATT_NORM),(0x5L|FEAR|BLINDNESS|MONSTER
		|MANA_DRAIN|HOLD_PERSON|CAUSE_SERIOUS|CAUSE_CRIT)
		  ,(EVIL|UNIQUE|NO_INFRA|IM_POISON|IM_FROST|IM_FIRE|
		  IM_ACID|IM_LIGHTNING|IM_POISON|CAN_SPEAK)
		,(ST_CLOUD|TRAP_CREATE|FORGET|HEAL),(NONE8)
    ,800,40,20,40,25,'$',{80,11},{157,203,148,148},26,4,'∫'
#ifdef TC_COLOR
  , RED
#endif
},


{"Alberich, the Nibelung King"   ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|
		HAS_2D2|MV_INVIS)
		,(0x6L|TELE|FEAR|MONSTER|ACID_BOLT),(EVIL|MAX_HP|IM_FROST|
			     IM_FIRE|IM_POISON|IM_ACID|IM_LIGHTNING|UNIQUE|
	 GOOD|CHARM_SLEEP|CAN_SPEAK),(ACID_BALL|HEAL),(NONE8)
    ,1200,20,20,80,34,'h',{100,11},{20,20,204,148},
				   27,4,'ê'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Unicorn"       ,(MV_ATT_NORM|MV_75|HAS_60)
		,(0x5L|TELE|BLINK),(IM_FIRE|IM_FROST|IM_POISON|ANIMAL)
			    ,(NONE8),(NONE8)
    ,55,30,14,45,48,'u',{13,9},{266,228,0,0},27,3,'˘'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Creeping adamantite coins",(MV_ATT_NORM|HAS_2D2|HAS_90|CARRY_GOLD),(NONE8)
			    ,(ANIMAL|IM_POISON|NO_INFRA),(NONE8),(NONE8)
       ,45,10,5,50,33,'$',{20,25},{161,172,10,10},27,4,'∫'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Headless"                  ,(MV_ATT_NORM|THRO_DR|HAS_60|
		 CARRY_OBJ|CARRY_GOLD),(0x6L|FEAR),(EVIL|GROUP)
			    ,(NONE8),(NONE8)
    ,160,40,20,60,21,'H',{21,12},{238,238,29,0},27,1,'ô'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Vibration hound"            ,(THRO_DR|MV_ATT_NORM),(0x5L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(BREATH_SD)
			    ,(NONE8)
    ,250,0,30,30,22,'Z',{25,10},{36,36,58,58},27,3,'≥'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Nexus hound"            ,(THRO_DR|MV_ATT_NORM),(0x5L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(BREATH_NE)
			    ,(NONE8)
    ,250,0,30,30,22,'Z',{25,10},{37,37,58,58},27,3,'≥'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Ogre mage"            ,(CARRY_GOLD|CARRY_OBJ|HAS_1D2|THRO_DR|MV_ATT_NORM)
			    ,(0x4L|HOLD_PERSON|FROST_BALL|MONSTER)
			    ,(EVIL|GIANT),(HEAL|TRAP_CREATE),(NONE8)
      ,300,30,20,40,23,'O',{30,12},{20,20,20,20},27,2,'®'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Grendel"      ,(MV_ATT_NORM|CARRY_OBJ|HAS_2D2|THRO_DR)
			    ,(NONE8),(GIANT|EVIL|IM_POISON|MAX_HP|GOOD|UNIQUE
			    |CAN_SPEAK)
			    ,(NONE8),(NONE8)
    ,1500,20,20,100,33,'O',{90,16},{235,235,235,0},27,2,'®'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Vampire"                  ,(HAS_1D2|CARRY_GOLD|CARRY_OBJ|HAS_60|
			      THRO_DR|MV_ATT_NORM)
			    ,(0x9L|HOLD_PERSON|FEAR|TELE_TO|CAUSE_SERIOUS)
			    ,(UNDEAD|EVIL|NO_INFRA|IM_FROST|IM_POISON|
			     CHARM_SLEEP|HURT_LIGHT)
			    ,(MIND_BLAST|FORGET|DARKNESS),(NONE8)
	,175,10,20,45,20,'V',{25,12},{5,5,190,0},27,1,'Ÿ'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Shantak"                ,(MV_ATT_NORM),(NONE8),(IM_ACID|ANIMAL|EVIL)
			    ,(NONE8),(NONE8)
      ,230,10,12,55,31,'B',{25,20},{32,32,223,0},27,2,'í'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Colbran"                  ,(MV_ATT_NORM),(0x3L)
			    ,(IM_LIGHTNING|IM_POISON|NO_INFRA|CHARM_SLEEP
			    |FEARLESS|MINDLESS|NONLIVING)
			    ,(LIGHT_BOLT),(NONE8)
    ,900,10,12,80,29,'g',{80,12},{130,130,0,0},27,2,''
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Spirit naga"              ,(HAS_2D2|CARRY_OBJ|HAS_90|
			     MV_ATT_NORM|MV_INVIS)
			    ,(0x4L|BLINDNESS)
			    ,(EVIL|CHARM_SLEEP)
			    ,(MIND_BLAST|DARKNESS|HEAL),(NONE8)
    ,60,120,20,75,23,'n',{30,15},{77,77,31,31},28,2,'•'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"stairway to hell"          ,(MV_ONLY_ATT|HAS_90|CARRY_OBJ),
		 (0xFL|S_DEMON)
		,(CHARM_SLEEP|EVIL|IM_ACID|NO_INFRA|IM_FIRE
	 |IM_LIGHTNING|IM_POISON|MAX_HP|UNDEAD|MAX_HP|FEARLESS|MINDLESS)
			    ,(NONE8),(NONE8)
    ,80,90,20,40,39,'>',{15,8},{203,192,148,149},28,5,'î'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Barney, the Dinosaur"       ,(MV_ATT_NORM|HAS_1D2|HAS_60|CARRY_OBJ)
		,(0x3L|BLINDNESS|CONFUSION|SLOW|MANA_DRAIN)
		,(ANIMAL|IM_POISON|EVIL|CHARM_SLEEP|MAX_HP|UNIQUE|SPECIAL|CAN_SPEAK)
	,(ST_CLOUD|BREATH_CH|FORGET|DARKNESS),(S_REPTILE|NUKE_BREATH)
       ,500,10,20,90,33,'R',{110,9},{270,272,163,278},28,2,'à'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Black knight"            ,(HAS_1D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM|CARRY_GOLD)
			    ,(0x8L|CAUSE_CRIT|BLINDNESS|FEAR),(EVIL)
			    ,(DARKNESS),(NONE8)
    ,240,10,20,70,28,'k',{30,10},{23,23,23,0},28,1,'˝'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Professor X"     ,(HAS_1D2|CARRY_OBJ|THRO_DR|PICK_UP|
		  MV_ATT_NORM)
		  ,(0x3L|MONSTER|HOLD_PERSON|FEAR|MANA_DRAIN)
		  ,(UNIQUE|GOOD|CHARM_SLEEP|CAN_SPEAK)
		,(MIND_BLAST|FORGET|BRAIN_SMASH),(NONE8)
    ,800,40,20,70,24,'x',{47,20},{22,22,22,0},28,4,'◊'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Lesser wall monster"            ,(MV_ATT_NORM|MV_75|MULTIPLY)
		,(NONE8),(IM_FROST|NO_INFRA|IM_ACID|IM_FIRE|IM_LIGHTNING
	 |IM_POISON|CHARM_SLEEP|BREAK_WALL|HURT_ROCK|NONLIVING),(NONE8),(NONE8)
    ,75,75,20,28,22,'±',{13,8},{13,13,13,0},28,4,'±'
#ifdef TC_COLOR /* This was "Wall monster" in the earlier versions - TY */
  , LIGHTGRAY
#endif
},
{"Mage"                     ,(HAS_1D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
			    ,(0x3L|TELE|TELE_TO|BLINDNESS|FROST_BOLT|FIRE_BOLT
			     |CONFUSION|MONSTER),(EVIL|INTELLIGENT)
			    ,(LIGHT_BOLT|HASTE),(NONE8)
    ,150,10,20,40,24,'p',{15,8},{14,14,0,0},28,1,'¯'
#ifdef TC_COLOR
  , RED
#endif
},

{"Mind flayer"              ,(HAS_1D2|CARRY_OBJ|HAS_60|THRO_DR|MV_ATT_NORM)
			    ,(0x8L|HOLD_PERSON|FEAR|BLINDNESS)
		,(EVIL|CHARM_SLEEP|MAX_HP|INTELLIGENT)
			    ,(MIND_BLAST|BRAIN_SMASH|FORGET),(NONE8)
    ,200,10,20,60,23,'h',{18,8},{225,225,0,0},28,1,'Ï'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Father Dagon"         ,(MV_ATT_NORM|MV_20|MV_INVIS|
			     CARRY_OBJ|HAS_4D2),(0x5L|BLINK|TELE_TO|TELE|FEAR
		|CONFUSION|BLINDNESS|S_DEMON)
		,(DEMON|IM_FIRE|MAX_HP|EVIL|UNIQUE
			      |GOOD|INTELLIGENT|CAN_SPEAK)
			    ,(TELE_LEV|TELE_AWAY),(NONE8)
    ,750,20,20,50,29,'I',{40,12},{152,152,17,0},28,5,'Ì'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Basilisk"         ,(MV_ATT_NORM|CARRY_OBJ|HAS_60|HAS_1D2|THRO_DR)
	,(0x9L|BREATH_G),(ANIMAL|CHARM_SLEEP|IM_POISON|EVIL)
			    ,(NONE8),(NONE8)
    ,600,30,15,90,29,'R',{22,25},{146,156,156,146},29,3,'·'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Ice troll"                ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|CARRY_GOLD|
			     HAS_60),(NONE8),
			    (EVIL|TROLL|IM_FROST|HURT_LIGHT|GROUP|NO_INFRA)
			    ,(NONE8),(NONE8)
    ,160,50,20,56,23,'T',{24,10},{4,4,123,4},28,1,'∏'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Dhole"        ,(MV_ATT_NORM),(0x9L|BREATH_A),(IM_ACID|ANIMAL|BREAK_WALL
		    |EVIL)
		,(NONE8),(NONE8)
    ,500,30,14,65,24,'w',{65,8},{7,113,166,92},29,3,'˚'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Archangel"         ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      HAS_2D2)
			    ,(0x3L|FEAR|BLINDNESS|CONFUSION)
			    ,(IM_POISON|IM_FIRE|IM_FROST|CHARM_SLEEP|MAX_HP|
			      INTELLIGENT|FEARLESS)
			    ,(HEAL|HASTE),(NONE8)
        ,400,255,30,68,24,'A',{25,16}
		,{18,18,18,18},29,6,'Ü'
#ifdef TC_COLOR
  , BLUE
#endif
},



{"Mimic"                    ,(MV_ONLY_ATT),(0x4L|FEAR|CONFUSION|BLINDNESS|
			     FIRE_BOLT|FROST_BOLT|ACID_BOLT|CAUSE_SERIOUS|
			     MONSTER),(CHARM_SLEEP|NO_INFRA|MINDLESS),(LIGHT_BOLT|
			     FORGET),(NONE8)
    ,200,100,30,60,32,'?',{10,35},{152,152,152,152},29,3,''
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Young blue dragon"        ,(MV_ATT_NORM|HAS_1D2|HAS_60|HAS_90|THRO_DR|
			      CARRY_GOLD|CARRY_OBJ),(0xBL|FEAR|BREATH_L)
			    ,(IM_LIGHTNING|EVIL|DRAGON|MAX_HP)
			    ,(NONE8),(NONE8)
    ,300,70,20,50,22,'d',{33,8},{52,52,29,0},29,1,''
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Young white dragon"       ,(MV_ATT_NORM|HAS_1D2|HAS_60|HAS_90|THRO_DR|
			      CARRY_GOLD|CARRY_OBJ),(0xBL|FEAR|BREATH_FR)
			    ,(IM_FROST|EVIL|DRAGON|MAX_HP|NO_INFRA),(NONE8)
    ,(NONE8),275,70,20,50,22,'d',{32,8},{52,52,29,0},29,1,''
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Young green dragon"       ,(MV_ATT_NORM|HAS_1D2|HAS_60|HAS_90|THRO_DR|
			      CARRY_GOLD|CARRY_OBJ),(0xBL|FEAR|BREATH_G)
			    ,(IM_POISON|EVIL|DRAGON|MAX_HP),(NONE8),(NONE8)
    ,290,70,20,60,22,'d',{32,8},{52,52,29,0},29,1,''
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Young bronze dragon"      ,(MV_ATT_NORM|HAS_2D2|HAS_60|HAS_90|
			     CARRY_GOLD|CARRY_OBJ|THRO_DR),
			     (0xBL|FEAR)
			    ,(DRAGON|MAX_HP|CHARM_SLEEP)
			    ,(BREATH_CO),(NONE8)
    ,310,150,20,63,22,'d',{34,8},{52,52,29,0},29,3,''
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Mithril golem"            ,(MV_ATT_NORM|CARRY_GOLD|HAS_2D2),(NONE8)
			    ,(IM_FROST|IM_FIRE|IM_LIGHTNING|IM_POISON
			      |NO_INFRA|CHARM_SLEEP|FEARLESS|MINDLESS|NONLIVING)
			      ,(NONE8),(NONE8)
      ,500,10,12,100,21,'g',{80,15},{20,20,23,23},30,4,''
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Shadow drake"             ,(MV_ATT_NORM|MV_20|MV_INVIS|THRO_DR|HAS_2D2|
			    CARRY_OBJ|PICK_UP),(0x6L|FEAR|CONFUSION|SLOW)
		,(ANIMAL|EVIL|IM_FROST|DRAGON|HURT_LIGHT)
			    ,(HASTE|DARKNESS),(NONE8)
      ,700,30,25,50,22,'d',{20,10},{122,122,122,0},30,2,''
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Skeleton troll"           ,(MV_ATT_NORM|THRO_DR),(NONE8)
			    ,(UNDEAD|EVIL|TROLL|NO_INFRA|IM_FROST|CHARM_SLEEP|
			     IM_POISON|FEARLESS|MINDLESS),(NONE8),(NONE8)
    ,225,20,20,55,21,'s',{20,10},{5,5,41,0},30,1,'µ'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Manticore"                ,(MV_ATT_NORM),(0x5L),(EVIL|MAX_HP),(MISSILE)
			    ,(NONE8)
    ,300,10,12,15,31,'H',{25,10},{17,17,17,17},30,2,'ö'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Giant static ant"         ,(MV_ATT_NORM|MV_20),(NONE8),(ANIMAL|IM_LIGHTNING|SEMI)
			    ,(NONE8),(NONE8)
    ,80,60,10,50,23,'i',{8,8},{134,0,0,0},30,2,'Â'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Giant army ant"           ,(MV_ATT_NORM|MV_20|THRO_CREAT),(NONE8)
			    ,(ANIMAL|GROUP|SEMI)
			    ,(NONE8),(NONE8)
    ,90,40,10,40,28,'i',{19,6},{39,0,0,0},30,3,'Â'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Grave wight"              ,(MV_ATT_NORM|MV_20|THRO_DR|HAS_1D2|CARRY_OBJ)
			    ,(0x8L|CAUSE_CRIT|FEAR)
			    ,(UNDEAD|EVIL|NO_INFRA|IM_FROST|IM_POISON|
			      HURT_LIGHT|CHARM_SLEEP),(DARKNESS),(NONE8)
    ,325,30,20,50,22,'W',{12,10},{6,6,191,0},30,1,'Ø'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Killer slicer beetle"     ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,200,30,14,60,22,'K',{22,10},{48,48,0,0},30,2,'õ'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Ghost"                    ,(MV_ATT_NORM|MV_20|HAS_1D2|CARRY_OBJ|CARRY_GOLD|
			     HAS_60|THRO_WALL|PICK_UP|MV_INVIS)
			    ,(0xFL|HOLD_PERSON|MANA_DRAIN|BLINDNESS)
			    ,(UNDEAD|EVIL|IM_FROST|NO_INFRA|CHARM_SLEEP|
			     IM_POISON),(NONE8),(NONE8)
    ,350,10,20,30,29,'G',{13,8},{99,192,184,0},31,1,'Ò'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Death watch beetle"       ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,190,30,16,60,22,'K',{25,12},{47,67,0,0},31,3,'õ'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Knight who says NI!"              ,(MV_ATT_NORM|THRO_DR|HAS_90|CARRY_OBJ)
		,(0x5L|HOLD_PERSON|CONFUSION|
		 FEAR|MONSTER),(EVIL|GROUP)
			    ,(TRAP_CREATE),(NONE8)
    ,224,30,20,55,22,'k',{10,10},{19,19,19,0},32,2,'˝'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Nexus quylthulg"          ,(MV_INVIS),(0x1L|BLINK),(CHARM_SLEEP|FEARLESS)
		,(TELE_AWAY),(NONE8)
    ,300,0,10,1,24,'Q',{10,12},{0,0,0,0},32,1,'ú'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"The Amazing Spider-man",(MV_ATT_NORM|CARRY_OBJ|HAS_2D2|HAS_1D2)
	       ,(0x2L|FEAR|HOLD_PERSON|CONFUSION|SLOW)
		 ,(UNIQUE|CHARM_SLEEP|MAX_HP|GOOD|INTELLIGENT|CAN_SPEAK)
		 ,(HEAL|S_SPIDER|HASTE|TRAP_CREATE),(NONE8)
      ,1200,8,8,80,29,'S',{120,10},{266,145,266,149},32,3,'˜'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Chaos tile"
    ,(MULTIPLY|MV_ONLY_ATT),(0x4L|BLINK|TELE|BLINDNESS|TELE_TO
	    |CONFUSION|MONSTER|SLOW)
		,(EVIL|IM_POISON|NO_INFRA|MINDLESS)
			    ,(NONE8),(NONE8)
    ,45,5,8,14,28,'.',{5,3},{25,178,0,0},32,6,'.'
#ifdef TC_COLOR
  , ANY
#endif
},
{"Teenage Mutant Ninja Turtle"
		,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|CARRY_GOLD|HAS_1D2)
			    ,(NONE8),(EVIL|CHARM_SLEEP),(NONE8),(NONE8)
	,300,10,20,60,30,'h',{13,12},{152,80,80,0},32,2,'Ï'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Spectator"              ,(MV_ATT_NORM),(0x6L|CAUSE_SERIOUS|SLOW|HOLD_PERSON)
	,(CHARM_SLEEP|FEARLESS)
			    ,(FORGET),(NONE8)
    ,150,5,30,1,23,'e',{10,13},{146,207,98,0},32,3,'È'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Storm giant"              ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|
			      CARRY_GOLD|HAS_1D2)
			    ,(0x8L|FEAR|CONFUSION|BLINK|TELE_TO)
			    ,(EVIL|GIANT|IM_LIGHTNING|IM_FROST|MAX_HP)
			    ,(LIGHT_BOLT|LIGHT_BALL),(NONE8)
      ,1500,40,20,60,23,'P',{24,16},{215,215,215,0},32,1,'ë'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Cave troll"               ,(MV_ATT_NORM|THRO_DR|HAS_60|
			      CARRY_OBJ|CARRY_GOLD),(NONE8)
			    ,(TROLL|EVIL|IM_POISON|HURT_LIGHT|GROUP)
			    ,(NONE8),(NONE8)
    ,350,50,20,50,22,'T',{24,12},{18,7,7,7},33,1,'∏'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Anti-paladin"               ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2|PICK_UP)
		,(0x4L|HOLD_PERSON|FEAR|BLINDNESS|CAUSE_CRIT)
		,(EVIL|IM_POISON|IM_FROST|CHARM_SLEEP)
		,(HASTE|TRAP_CREATE|DARKNESS|FORGET),(NONE8)
    ,450,30,30,50,31,'k',{30,20},{18,19,19,93},33,2,'˝'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Logrus master"                   ,(MV_ATT_NORM|HAS_1D2|CARRY_OBJ|MV_INVIS)
		,(0x6L|S_DEMON),(IM_POISON|IM_ACID|MAX_HP|CHARM_SLEEP|EVIL)
		,(HEAL|S_SPIDER),(LOGRUS_BALL)
     ,520,5,30,50,32,'p',{10,35},{266,266,266,266},33,3,'¯'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Barrow wight"             ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|
			      CARRY_GOLD|HAS_60),(0x8L|CAUSE_SERIOUS|
			      HOLD_PERSON|FEAR)
			    ,(EVIL|UNDEAD|NO_INFRA|IM_FROST|IM_POISON|
			      HURT_LIGHT|CHARM_SLEEP|GROUP),(DARKNESS),(NONE8)
    ,375,10,20,40,22,'W',{15,10},{7,7,193,0},33,3,'Ø'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Giant skeleton troll"     ,(MV_ATT_NORM|THRO_DR),(NONE8)
			    ,(TROLL|EVIL|UNDEAD|IM_FROST|IM_POISON|
			      NO_INFRA|CHARM_SLEEP|MAX_HP|FEARLESS|MINDLESS)
			      ,(NONE8),(NONE8)
    ,325,20,20,50,21,'s',{45,10},{8,8,28,28},33,1,'µ'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Chaos drake"              ,(MV_ATT_NORM|THRO_DR|HAS_2D2|CARRY_OBJ)
			    ,(0x6L|FEAR|CONFUSION|SLOW),(EVIL|IM_FIRE|
			     CHARM_SLEEP|MAX_HP|DRAGON),(BREATH_DI)
		,(NONE8)
      ,700,30,25,100,22,'d',{50,10},{54,54,36,0},33,3,''
#ifdef TC_COLOR
  , ANY
#endif
},

{"Law drake"                ,(MV_ATT_NORM|THRO_DR|HAS_2D2|CARRY_OBJ)
			    ,(0x6L|FEAR|CONFUSION|SLOW),(IM_FROST|
			     CHARM_SLEEP|MAX_HP|DRAGON),(BREATH_SH|
			     BREATH_SD),(NONE8)
      ,700,30,25,100,22,'d',{50,11},{54,54,36,0},33,3,''
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Balance drake"            ,(MV_ATT_NORM|THRO_DR|HAS_2D2|CARRY_OBJ)
			    ,(0x6L|FEAR|CONFUSION|SLOW),(IM_FIRE|
			     IM_FROST|CHARM_SLEEP|MAX_HP|DRAGON)
			    ,(BREATH_DI|BREATH_SD|BREATH_SH),(NONE8)
      ,700,30,25,100,22,'d',{50,12},{54,54,36,0},33,3,''
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Ethereal drake"           ,(MV_ATT_NORM|THRO_DR|HAS_2D2|CARRY_OBJ|MV_INVIS|
			     THRO_WALL)
			    ,(0x6L|FEAR|CONFUSION|SLOW)
			    ,(EVIL|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(BREATH_LT|BREATH_DA)
      ,700,15,25,100,22,'d',{50,9},{54,54,36,0},33,3,''
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Fasolt the Giant"   ,(MV_ATT_NORM|PICK_UP|THRO_DR|HAS_1D2|
			      CARRY_OBJ),(NONE8)
		,(GIANT|EVIL|IM_POISON|MAX_HP|UNIQUE|CAN_SPEAK|
		GOOD|IM_FROST),(NONE8),(NONE8)
       ,2000,50,20,70,26,'P',{55,20},{23,23,23,0},33,6,'ë'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Freddy Krueger"   ,(MV_ATT_NORM|PICK_UP|THRO_DR|HAS_2D2|
		  CARRY_OBJ),(0x5L|FEAR|SLOW|HOLD_PERSON
		  |S_UNDEAD|MANA_DRAIN)
		,(UNDEAD|EVIL|IM_POISON|MAX_HP|UNIQUE|
		  HURT_LIGHT|GOOD|IM_FROST|CAN_SPEAK)
		  ,(DARKNESS|HASTE|HEAL|TRAP_CREATE),(NONE8)
      ,2500,50,20,70,25,'W',{50,20},{57,271,146,98},34,6,'Ø'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Morgenstern, Julian's steed"   ,(MV_ATT_NORM),(NONE8)
	,(ANIMAL|IM_POISON|MAX_HP|UNIQUE|FEARLESS
		|IM_FROST),(NONE8),(NONE8)
      ,2000,50,20,70,33,'q',{55,20},{266,266,33,0},33,7,'˘'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Shade"                    ,(MV_ATT_NORM|MV_20|CARRY_OBJ|HAS_2D2|
			     HAS_90|THRO_WALL|PICK_UP|MV_INVIS)
			    ,(0xFL|HOLD_PERSON|MANA_DRAIN|BLINDNESS)
			    ,(UNDEAD|EVIL|IM_FROST|NO_INFRA|CHARM_SLEEP|
			     IM_POISON),(FORGET),(NONE8)
      ,375,10,20,30,32,' ',{14,20},{99,146,192,184},33,3,' '
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Spectre"                  ,(MV_ATT_NORM|MV_20|CARRY_OBJ|HAS_2D2|
			     HAS_90|THRO_WALL|PICK_UP)
			    ,(0xFL|HOLD_PERSON|MANA_DRAIN|BLINDNESS)
			    ,(UNDEAD|EVIL|IM_FROST|NO_INFRA|CHARM_SLEEP|
			     IM_POISON),(FORGET),(NONE8)
      ,350,10,20,30,32,'G',{14,20},{99,192,237,0},33,3,'Ò'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Water troll"               ,(MV_ATT_NORM|THRO_DR|HAS_60|
			      CARRY_OBJ|CARRY_GOLD),(NONE8)
			    ,(TROLL|EVIL|IM_POISON|IM_FROST|HURT_LIGHT|
			      MAX_HP|GROUP),(NONE8),(NONE8)
    ,420,50,20,50,22,'T',{26,14},{8,8,11,11},33,1,'∏'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Fire elemental"           ,(MV_ATT_NORM|MV_20|THRO_CREAT)
			    ,(0x6L|FIRE_BOLT)
			    ,(EVIL|IM_POISON|CHARM_SLEEP|IM_FIRE|MINDLESS
			    |NONLIVING)
		,(PLASMA_BOLT),(NONE8)
    ,350,50,12,50,22,'E',{30,8},{103,103,0,0},33,2,'ç'
#ifdef TC_COLOR
  , RED
#endif
},

{"Cherub"          ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      HAS_2D2|HAS_1D2)
			    ,(0x3L|FEAR|BLINDNESS|FIRE_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|INTELLIGENT|
			      IM_ACID|IM_LIGHTNING|CHARM_SLEEP|MAX_HP
			      |FEARLESS)
			    ,(HEAL|HASTE|MIND_BLAST|SUMMON),(NONE8)
        ,400,255,30,68,29,'A',{25,18}
		,{21,20,21,20},33,6,'Ü'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Water elemental"          ,(MV_ATT_NORM|MV_20|THRO_CREAT)
			    ,(0x6L|FROST_BOLT)
			    ,(EVIL|IM_POISON|CHARM_SLEEP|NO_INFRA|MINDLESS
			    |NONLIVING)
		,(WATER_BOLT),(NONE8)
    ,325,50,12,40,22,'E',{25,8},{9,9,9,0},33,2,'ç'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Dimensional shambler"        ,(MV_ATT_NORM|THRO_DR|HAS_90|MV_40|MV_INVIS)
			    ,(NONE8),(IM_LIGHTNING|EVIL|IM_POISON|CHARM_SLEEP|
			     NO_INFRA),(NONE8),(NONE8)
    ,300,20,20,46,42,'E',{19,12},{5,5,5,0},34,3,'ç'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Gargoyle"          ,(MV_ATT_NORM|HAS_60|CARRY_GOLD)
		,(0xCL|BREATH_L|BREATH_FI),(DEMON|IM_POISON
		 |EVIL|IM_FIRE|IM_FROST|IM_LIGHTNING|GROUP|HURT_LIGHT)
		 ,(NONE8),(NONE8)
    ,100,10,15,50,22,'&',{18,12},{58,58,36,0},34,2,'Ñ'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Master thief"             ,(HAS_2D2|HAS_90|CARRY_GOLD|CARRY_OBJ
			     |THRO_DR|PICK_UP|MV_ATT_NORM),(NONE8)
			    ,(EVIL),(NONE8),(NONE8)
      ,350,40,20,30,42,'p',{18,10},{16,17,231,232},34,2,'‡'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Jurt, the Living Trump"         ,(HAS_2D2|CARRY_OBJ|THRO_DR|PICK_UP|
		  MV_ATT_NORM),(0x6L|TELE|BLINK),(EVIL|UNIQUE|GOOD|CAN_SPEAK)
		,(NONE8),(LOGRUS_BALL)
      ,1200,40,20,90,35,'p',{80,13},{23,23,23,23},34,5,'‡'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Lich"                     ,(MV_ATT_NORM|THRO_DR|HAS_1D2|CARRY_OBJ|
			      CARRY_GOLD)
			    ,(0x4L|BLINK|TELE_TO|CAUSE_CRIT|
			      HOLD_PERSON|BLINDNESS|MANA_DRAIN|
			      SLOW|FEAR)
			    ,(EVIL|UNDEAD|IM_FROST|IM_POISON|NO_INFRA|
			      MAX_HP|HURT_LIGHT|CHARM_SLEEP|INTELLIGENT)
		,(BRAIN_SMASH|TELE_AWAY|RAZOR),(NONE8)
	 ,880,60,20,60,20,'L',{25,12},{179,179,194,214},34,3,'ü'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Oriental vampire"
	,(MV_ATT_NORM|THRO_DR|HAS_4D2|CARRY_OBJ|CARRY_GOLD|MV_INVIS
	    |THRO_WALL)
			    ,(0x6L|TELE_TO|CAUSE_CRIT|HOLD_PERSON|
			      FEAR|CONFUSION)
			    ,(CHARM_SLEEP|HURT_LIGHT|EVIL|UNDEAD|IM_FROST|
			     MAX_HP|IM_POISON|NO_INFRA)
			    ,(DARKNESS|MIND_BLAST|FORGET|NETHER_BOLT),(NONE8)
    ,770,10,20,60,25,'V',{28,12},{5,5,195,0},34,3,'Ÿ'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Giant red scorpion"       ,(MV_ATT_NORM),(NONE8),(ANIMAL|SEMI),(NONE8),(NONE8)
    ,275,40,12,50,29,'S',{18,20},{29,165,0,0},34,4,'∑'
#ifdef TC_COLOR
  , RED
#endif
},

{"Earth elemental"          ,(THRO_WALL|PICK_UP|MV_ATT_NORM)
			    ,(0x8L|ACID_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|IM_LIGHTNING|
			      CHARM_SLEEP|HURT_ROCK|EVIL|NO_INFRA|MINDLESS
			      |NONLIVING)
			    ,(NONE8),(NONE8)
    ,375,90,10,60,17,'E',{30,10},{22,22,22,0},34,2,'ç'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Air elemental"            ,(MV_ATT_NORM|MV_20|THRO_CREAT),(0x8L)
			    ,(EVIL|IM_POISON|CHARM_SLEEP|IM_FIRE|IM_FROST|
			      IM_LIGHTNING|IM_ACID|IM_POISON|NO_INFRA|MINDLESS
			      |NONLIVING)
			    ,(LIGHT_BOLT),(NONE8)
    ,390,50,12,50,34,'E',{30,5},{9,89,9,0},34,2,'ç'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Multi-hued hound"               ,(THRO_DR|MV_ATT_NORM)
		,(0x5L|BREATH_FI|BREATH_FR|BREATH_G|BREATH_L|BREATH_A),
		 (ANIMAL|IM_FIRE|IM_FROST|IM_POISON|IM_ACID
		 |IM_LIGHTNING|GROUP),(NONE8),(NONE8)
      ,600,0,25,80,29,'Z',{30,10},{39,39,39,58},33,2,'≥'
#ifdef TC_COLOR
  , MULTI
#endif
},

{"Eog golem"                ,(MV_ATT_NORM|CARRY_GOLD|HAS_2D2),(NONE8)
			    ,(IM_FROST|IM_FIRE|IM_LIGHTNING|IM_POISON
			      |NO_INFRA|CHARM_SLEEP|FEARLESS|MINDLESS|NONLIVING)
			      ,(NONE8),(NONE8)
     ,1200,10,12,125,17,'g',{100,20},{218,218,235,235},35,4,''
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Malicious leprechaun"   ,(MULTIPLY|MV_INVIS|MV_40|PICK_UP)
		,(0x6L|BLINK|TELE|TELE_TO|CAUSE_LIGHT),(NO_INFRA
		|EVIL|HURT_LIGHT)
		,(NONE8),(NONE8)
    ,84,8,8,14,33,'l',{4,5},{149,148,0,0},35,4,''
#ifdef TC_COLOR
  , MAGENTA
#endif
},


{"Olog"             ,(MV_ATT_NORM|THRO_DR|HAS_60|
			      CARRY_OBJ|CARRY_GOLD),(NONE8)
			    ,(TROLL|EVIL|IM_POISON|MAX_HP|GROUP)
			    ,(NONE8),(NONE8)
    ,400,50,20,50,22,'T',{30,14},{10,10,33,33},35,1,'∏'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Predator"                 ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|CARRY_GOLD|HAS_1D2
		    |MV_INVIS)
		,(0x8L|FIRE_BALL)
		,(EVIL|CHARM_SLEEP|NO_INFRA|MAX_HP)
		,(PLASMA_BOLT|RAZOR),(NUKE_BALL)
      ,600,5,20,70,31,'h',{30,25},{152,80,80,152},35,4,'Ï'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Gravity hound"            ,(THRO_DR|MV_ATT_NORM),(0x5L)
			    ,(ANIMAL|GROUP),(NONE8)
			    ,(BREATH_GR)
    ,500,0,30,30,22,'Z',{35,10},{39,39,39,58},35,2,'≥'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Acidic cytoplasm"          ,(THRO_DR|MV_ATT_NORM|PICK_UP|HAS_4D2|
			      CARRY_GOLD|CARRY_OBJ|HAS_60|HAS_90),(NONE8)
			    ,(IM_ACID|IM_FIRE|IM_LIGHTNING|IM_POISON|IM_FROST|
			      ANIMAL|CHARM_SLEEP|MAX_HP|NO_INFRA|MINDLESS)
			    ,(NONE8),(NONE8)
    ,36,1,12,18,33,'j',{50,8},{115,115,115,115},35,5,'Ê'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Inertia hound"            ,(THRO_DR|MV_ATT_NORM),(0x5L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(NONE8)
			    ,(BREATH_SL)
    ,500,0,30,30,22,'Z',{35,10},{39,39,39,58},35,2,'≥'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Impact hound"            ,(THRO_DR|MV_ATT_NORM),(0x8L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(NONE8)
			    ,(BREATH_WA)
    ,500,0,30,30,22,'Z',{35,10},{39,39,39,58},35,2,'≥'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Gloom"                    ,(MV_ATT_NORM|MV_20|CARRY_OBJ|HAS_2D2|
			     HAS_60|THRO_WALL|PICK_UP|MV_INVIS)
			    ,(0xFL|HOLD_PERSON|MANA_DRAIN|BLINDNESS|CONFUSION)
			    ,(UNDEAD|EVIL|IM_FROST|NO_INFRA|CHARM_SLEEP|
			     IM_POISON),(NETHER_BOLT),(NONE8)
    ,600,10,20,30,31,'G',{25,20},{235,235,80,0},35,2,'Ò'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Ooze elemental"           ,(MV_ATT_NORM|THRO_DR)
			    ,(0x5L|ACID_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|IM_LIGHTNING|IM_ACID
			    |NONLIVING|
			      CHARM_SLEEP|EVIL|NO_INFRA|MINDLESS),(ACID_BALL),(NONE8)
      ,300,90,10,80,22,'E',{13,10},{115,115,115,0},35,3,'ç'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Smoke elemental"          ,(MV_ATT_NORM)
			    ,(0x5L|FIRE_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|IM_LIGHTNING|NONLIVING|
			      CHARM_SLEEP|EVIL|MINDLESS),(DARKNESS),(NONE8)
    ,375,90,10,80,32,'E',{15,10},{36,36,0,0},35,3,'ç'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Young black dragon"       ,(MV_ATT_NORM|HAS_1D2|HAS_60|HAS_90|
			     CARRY_GOLD|CARRY_OBJ|THRO_DR),
			     (BREATH_A|0xBL|FEAR)
			    ,(EVIL|IM_ACID|DRAGON|MAX_HP),(NONE8),(NONE8)
    ,620,50,20,60,22,'d',{32,8},{53,53,29,0},35,1,''
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Mumak"                    ,(MV_ATT_NORM),(NONE8),(ANIMAL),(NONE8),(NONE8)
     ,2100,100,20,55,25,'q',{90,10},{227,227,233,0},35,3,'ı'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Cyclops"       ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_60|HAS_1D2),(NONE8)
	,(GIANT|EVIL|MAX_HP),(NONE8),(NONE8)
    ,350,40,14,49,23,'P',{25,18},{19,19,19,19},35,1,'ë'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Mature white dragon"      ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|CARRY_OBJ|
			      HAS_2D2),(0xAL|BREATH_FR|FEAR)
			    ,(CHARM_SLEEP|IM_FROST|EVIL|DRAGON|MAX_HP|NO_INFRA)
			    ,(NONE8),(NONE8)
    ,1000,70,20,65,23,'d',{50,8},{54,54,37,0},35,1,''
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Xorn"                     ,(MV_ATT_NORM|THRO_WALL|THRO_DR|PICK_UP)
			    ,(NONE8),(IM_FIRE|IM_FROST|IM_POISON|NO_INFRA|NONLIVING|
			     IM_LIGHTNING|CHARM_SLEEP|HURT_ROCK|MAX_HP|MINDLESS)
			    ,(NONE8),(NONE8)
    ,650,10,20,80,21,'X',{20,8},{5,5,5,5},36,2,'Ö'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Shadow"                   ,(MV_ATT_NORM|THRO_WALL|MV_INVIS|CARRY_OBJ|
			     HAS_1D2),(0x8L|TELE_TO|SLOW),(UNDEAD|EVIL|
		 IM_FROST|IM_POISON|NO_INFRA|CHARM_SLEEP|HURT_LIGHT)
			    ,(NONE8),(NONE8)
		,400,20,30,30,30,'G',{10,20},{200,200,184,252},
				  36,3,'Ò'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Phantom"                  ,(MV_ATT_NORM|THRO_WALL|MV_INVIS|CARRY_OBJ|
			     HAS_1D2),(0x5L),(UNDEAD|EVIL|IM_FROST|IM_POISON|
			     NO_INFRA|CHARM_SLEEP),(FORGET),(NONE8)
        ,400,20,30,30,31,'G',{20,25},{200,200,184,252},
				  36,3,'Ò'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Grey wraith"              ,(MV_ATT_NORM|THRO_DR|HAS_60|HAS_90|
			      CARRY_GOLD|CARRY_OBJ)
			    ,(0x7L|CAUSE_CRIT|HOLD_PERSON|FEAR)
			   ,(UNDEAD|EVIL|NO_INFRA|CHARM_SLEEP|IM_FROST|MAX_HP|
			     IM_POISON),(DARKNESS),(NONE8)
    ,700,10,20,50,22,'W',{24,8},{9,9,196,0},36,1,'Ø'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Young multi-hued dragon"  ,(MV_ATT_NORM|HAS_4D2|CARRY_GOLD|CARRY_OBJ|
			      THRO_DR|HAS_60|HAS_90)
			    ,(0x5L|BREATH_G|BREATH_L|BREATH_A|BREATH_FR|
			      BREATH_FI|FEAR)
			    ,(IM_FROST|IM_ACID|IM_POISON|IM_LIGHTNING|
			      IM_FIRE|EVIL|DRAGON|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,1320,50,20,60,22,'d',{40,8},{55,55,38,0},36,1,''
#ifdef TC_COLOR
  , MULTI
#endif
},

{"Raal's Tome of Destruction"
		 ,(MV_ONLY_ATT|CARRY_OBJ|HAS_90)
		 ,(0x3L|ACID_BOLT|BREATH_FI|MANA_BOLT|BREATH_FR
		 |BREATH_G)
		 ,(MAX_HP|CHARM_SLEEP|IM_ACID|IM_POISON|
		 NO_INFRA|IM_FROST|IM_LIGHTNING|GOOD|EVIL|MINDLESS)
		,(WATER_BOLT|ST_CLOUD|BREATH_LD),(NONE8)
     ,1500,10,12,150,26,'?',{50,15},{103,115,120,130},36,4,''
#ifdef TC_COLOR
  , RED
#endif
},

{"Young brass dragon"        ,(MV_ATT_NORM|HAS_2D2|HAS_60|HAS_90|
			     CARRY_GOLD|CARRY_OBJ|THRO_DR),
			     (0xBL|FEAR)
			    ,(DRAGON|MAX_HP)
			    ,(BREATH_SD),(NONE8)
    ,950,150,20,63,22,'d',{38,8},{54,54,37,0},36,2,''
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Groo the Wanderer"   ,(MV_ATT_NORM|PICK_UP|THRO_DR|HAS_2D2|
			      CARRY_OBJ),(NONE8)
		,(TROLL|EVIL|IM_POISON|MAX_HP|UNIQUE|SEMI|
			      GOOD|IM_FROST),(NONE8),(NONE8)
      ,5000,50,20,70,33,'T',{55,28},{235,235,235,229},36,5,'∏'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Mature blue dragon"       ,(HAS_2D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x9L|BREATH_L|FEAR)
			    ,(EVIL|DRAGON|IM_LIGHTNING|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,1200,70,20,75,23,'d',{49,8},{54,54,38,0},36,1,''
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Mature green dragon"      ,(HAS_2D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x9L|BREATH_G|FEAR)
			    ,(EVIL|DRAGON|IM_POISON|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,1100,70,20,70,23,'d',{49,8},{52,52,29,0},36,1,''
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Mature bronze dragon"     ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x9L|FEAR|CONFUSION)
			    ,(DRAGON|CHARM_SLEEP|MAX_HP)
			    ,(BREATH_CO),(NONE8)
    ,1300,150,20,70,23,'d',{55,8},{54,54,38,0},36,2,''
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Young red dragon"         ,(MV_ATT_NORM|HAS_1D2|HAS_60|HAS_90|
			     CARRY_GOLD|CARRY_OBJ|THRO_DR),
			     (BREATH_FI|0xBL|FEAR)
			    ,(EVIL|IM_FIRE|DRAGON|MAX_HP)
			    ,(NONE8),(NONE8)
    ,640,50,20,63,22,'d',{36,8},{54,54,37,0},36,1,''
#ifdef TC_COLOR
  , RED
#endif
},

{"Trapper"                  ,(MV_ONLY_ATT|MV_INVIS),(NONE8)
			    ,(NO_INFRA|CHARM_SLEEP|MAX_HP|MINDLESS),(NONE8),(NONE8)
    ,580,10,30,75,31,'.',{50,12},{20,20,265,265},36,3,''
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Elder thing"                    ,(PICK_UP|MV_ATT_NORM|THRO_DR)
		,(0x4L|FIRE_BALL|FIRE_BOLT|S_DEMON|FEAR|CONFUSION)
		,(IM_POISON|IM_FIRE|CHARM_SLEEP|EVIL|DEMON)
			    ,(NONE8),(NONE8)
      ,750,90,10,68,26,'I',{35,10},{77,77,77,224},36,2,'Ì'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Whirling Eddie"            ,(PICK_UP|MV_ATT_NORM)
			    ,(0x5L|FROST_BALL)
			    ,(IM_POISON|IM_FROST|IM_LIGHTNING|NONLIVING|
		 CHARM_SLEEP|EVIL|NO_INFRA|MINDLESS),(WATER_BOLT|WATER_BALL),(NONE8)
       ,660,90,10,60,24,'E',{35,10},{121,22,121,0},36,2,'ç'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Warlock"                  ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
			    ,(0x3L|TELE|TELE_TO|CAUSE_CRIT|HOLD_PERSON|
			   S_UNDEAD|FEAR|BLINDNESS),(EVIL|MAX_HP|INTELLIGENT)
			    ,(NETHER_BOLT|HASTE),(NONE8)
    ,630,10,20,50,23,'p',{25,11},{15,15,0,0},36,2,'¯'
#ifdef TC_COLOR
  , RED
#endif
},

{"Lord Borel of Hendrake"
			    ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_2D2)
			    ,(0x4L|TELE_TO),(EVIL|MAX_HP|CHARM_SLEEP|IM_ACID|
			     IM_POISON|IM_FIRE|IM_FROST|IM_LIGHTNING|GOOD|
			     UNIQUE|CAN_SPEAK),(SUMMON),(NONE8)
       ,1200,10,25,100,32,'p',{50,35},{235,235,20,20},36,2,'‡'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Demonist"                 ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2)
			    ,(0x2L|TELE|HOLD_PERSON|
			     S_DEMON),(EVIL|MAX_HP|INTELLIGENT)
			    ,(NONE8),(NONE8)
    ,700,10,20,50,31,'p',{25,11},{15,15,14,0},36,2,'¯'
#ifdef TC_COLOR
  , RED
#endif
},

{"Mummified troll"          ,(HAS_60|CARRY_GOLD|CARRY_OBJ|
			     THRO_DR|MV_ATT_NORM),(NONE8)
			    ,(UNDEAD|IM_FROST|CHARM_SLEEP|IM_POISON|
			      TROLL|EVIL|NO_INFRA|MAX_HP|FEARLESS|MINDLESS),(NONE8),(NONE8)
    ,420,50,20,50,21,'M',{19,10},{15,15,0,0},37,1,'°'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"The Queen Ant"            ,(MV_ATT_NORM|CARRY_OBJ|HAS_2D2|THRO_DR)
			    ,(0x2L),(ANIMAL|MAX_HP|UNIQUE|CHARM_SLEEP|
			     GOOD|SEMI),(NONE8),(S_ANT)
     ,1000,10,30,100,32,'i',{120,12},{39,39,37,37},37,2,'Â'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Will o' the wisp"         ,(MV_ATT_NORM|MV_40|MV_INVIS|THRO_DR|THRO_WALL)
			    ,(0x2L|BLINK|CAUSE_SERIOUS|CONFUSION|TELE)
		,(CHARM_SLEEP|IM_FIRE|IM_FROST|IM_POISON|EVIL|NONLIVING|
			     IM_ACID|IM_LIGHTNING|MAX_HP|INTELLIGENT)
			    ,(NONE8),(NONE8)
    ,500,0,30,150,42,'E',{20,10},{8,8,8,8},37,4,'ç'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Time elemental"          ,(THRO_WALL|PICK_UP|MV_ATT_NORM|THRO_WALL|MV_40)
		,(0x7L|SLOW)
			    ,(IM_POISON|IM_FIRE|IM_LIGHTNING|NONLIVING|
		  CHARM_SLEEP|EVIL|MINDLESS),(HASTE),(BREATH_TI)
    ,1000,90,10,70,27,'E',{35,10},{200,87,225,185},37,2,'ç'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Black pudding"            ,(THRO_DR|MV_ATT_NORM|PICK_UP|HAS_1D2|
			      CARRY_GOLD|CARRY_OBJ|HAS_60|HAS_90),(NONE8)
			    ,(IM_ACID|IM_FIRE|IM_LIGHTNING|IM_POISON|IM_FROST|
			      ANIMAL|CHARM_SLEEP|MAX_HP|NO_INFRA|GROUP|MINDLESS)
			    ,(NONE8),(NONE8)
    ,36,1,12,18,22,'j',{50,8},{115,115,115,115},37,5,'©'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Iridescent beetle"        ,(MV_ATT_NORM),(NONE8)
			    ,(ANIMAL|IM_LIGHTNING|MAX_HP|SEMI),(NONE8),(NONE8)
    ,850,30,16,60,23,'K',{32,8},{45,10,146,0},37,2,'õ'
#ifdef TC_COLOR
  , MULTI /* Check a dictionary for the word "Iridescent" and you'll see why I
	     made it MULTI -TY */
#endif
},

{"Nexus vortex"             ,(MV_ATT_NORM|MV_75),(0x6L)
			    ,(CHARM_SLEEP|MINDLESS|NONLIVING),(BREATH_NE),(NONE8)
    ,800,0,100,40,29,'v',{32,10},{244,0,0,0},37,1,'˙'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Plasma vortex"            ,(MV_ATT_NORM|MV_75),(0x6L)
			    ,(IM_FIRE|CHARM_SLEEP|MINDLESS|NONLIVING),(NONE8),(BREATH_PL)
	,800,0,100,40,30,'v',{32,10},{243,0,0,0},37,1,'˙'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Mature red dragon"        ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x9L|BREATH_FI|FEAR|CONFUSION)
			    ,(EVIL|DRAGON|IM_FIRE|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,1400,30,20,80,23,'d',{60,8},{52,56,39,0},37,1,''
#ifdef TC_COLOR
  , RED
#endif
},

{"Mature brass dragon"       ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x9L|FEAR|CONFUSION)
			    ,(DRAGON|CHARM_SLEEP|MAX_HP)
			    ,(BREATH_SD),(NONE8)
    ,1500,150,20,80,23,'d',{70,8},{52,56,39,0},37,2,''
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Fire Angel"            ,(MV_ATT_NORM|THRO_DR|HAS_4D2|CARRY_OBJ)
		,(0x9L)
		,(EVIL|IM_FIRE|CHARM_SLEEP|MAX_HP|DRAGON|INTELLIGENT)
		,(NONE8),(BREATH_SL)
       ,1500,10,25,100,29,'d',{50,10},{52,52,35,0},37,3,'ù'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Mature black dragon"      ,(HAS_2D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x9L|BREATH_A|FEAR)
			    ,(EVIL|DRAGON|IM_ACID|CHARM_SLEEP|MAX_HP),(NONE8)
			    ,(NONE8)
    ,1350,30,20,55,23,'d',{58,8},{54,54,38,0},37,1,''
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Mature multi-hued dragon" ,(MV_ATT_NORM|HAS_4D2|CARRY_GOLD|CARRY_OBJ|
			      THRO_DR|HAS_60|HAS_90|HAS_2D2)
			    ,(0x5L|BREATH_G|BREATH_L|BREATH_A|BREATH_FR|
			      BREATH_FI|FEAR|CONFUSION|BLINDNESS)
			    ,(IM_FROST|IM_ACID|IM_POISON|IM_LIGHTNING|
			      IM_FIRE|EVIL|DRAGON|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,1700,50,20,65,23,'d',{81,8},{56,56,39,0},38,2,''
#ifdef TC_COLOR
  , MULTI
#endif
},

{"Death knight"             ,(HAS_2D2|HAS_1D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
			    ,(0x5L|CAUSE_CRIT|BLINDNESS|FEAR)
		,(EVIL|IM_FROST|NO_INFRA|MAX_HP|INTELLIGENT|UNDEAD)
			    ,(NETHER_BOLT|SUMMON),(NONE8)
      ,1000,10,20,100,31,'k',{30,20},{235,23,23,194},38,1,'˝'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Mandor, Master of the Logrus"     ,(HAS_2D2|CARRY_OBJ|THRO_DR|PICK_UP|
		  MV_ATT_NORM),(0x2L|FIRE_BOLT|FROST_BOLT|HOLD_PERSON
		  |MANA_BOLT|MONSTER)
			    ,(EVIL|MAX_HP|UNIQUE|GOOD|INTELLIGENT)
		,(TRAP_CREATE|ICE_BOLT|HEAL|LIGHT_BOLT|CAN_SPEAK),(LOGRUS_BALL)
    ,1600,40,20,90,35,'p',{80,11},{23,23,23,23},38,5,'¯'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Time vortex"              ,(MV_ATT_NORM|MV_75),(0x6L)
			    ,(CHARM_SLEEP|MINDLESS|NONLIVING),(NONE8),(BREATH_TI)
    ,900,0,100,40,42,'v',{32,10},{244,0,0,0},38,4,'˙'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Shimmering vortex"        ,(MV_ATT_NORM|MV_75),(0x6L)
			    ,(CHARM_SLEEP|MINDLESS|NONLIVING),(NONE8),(BREATH_LT)
    ,200,0,100,30,51,'v',{6,12},{203,203,0,0},38,4,'˙'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Ancient blue dragon"      ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x9L|BREATH_L|FEAR|BLINDNESS
			      |CONFUSION)
			    ,(EVIL|DRAGON|IM_LIGHTNING|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,1500,80,20,80,29,'D',{88,8},{54,54,37,134},38,1,'â'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Ancient bronze dragon"    ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x6L|FEAR|BLINDNESS|CONFUSION)
			    ,(DRAGON|CHARM_SLEEP|MAX_HP),(BREATH_CO),(NONE8)
    ,1700,200,20,100,29,'D',{92,8},{54,54,38,234},38,2,'â'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Cacodemon"                ,(MV_ATT_NORM)
		,(0x2L|BREATH_L|MANA_DRAIN|BLINDNESS)
	,(EVIL|CHARM_SLEEP|IM_POISON|IM_LIGHTNING|IM_FIRE
		|DEMON)
		,(LIGHT_BALL),(NONE8)
    ,6000,10,30,80,28,'e',{90,27},{223,224,225,226},38,4,'È'
#ifdef TC_COLOR
  , RED
#endif
},

{"Baron of hell" , (MV_ATT_NORM|CARRY_GOLD|CARRY_OBJ|HAS_60|HAS_90)
		    ,(0x2L)
		    ,(IM_POISON|IM_FIRE|CHARM_SLEEP|EVIL|DEMON|MAX_HP)
		    ,(PLASMA_BOLT), (NONE8)
        , 900,10,12,131,24,'&',{150,10},{271,271,271,0},38,3,'Ñ'
#ifdef TC_COLOR
  , BROWN
#endif
},


{"Emperor wight"            ,(HAS_4D2|CARRY_OBJ|HAS_90|THRO_DR|MV_ATT_NORM)
			    ,(0x6L|CAUSE_CRIT|HOLD_PERSON|FEAR)
			    ,(EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|MAX_HP|
			      IM_POISON|NO_INFRA|HURT_LIGHT)
			    ,(NETHER_BOLT),(NONE8)
    ,1600,10,20,40,29,'W',{48,8},{10,10,199,0},38,2,'Ø'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Seraph"         ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      HAS_2D2|HAS_1D2)
			    ,(0xBL|CONFUSION|MANA_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|INTELLIGENT|
			      IM_ACID|IM_LIGHTNING|CHARM_SLEEP|MAX_HP|FEARLESS)
			    ,(HEAL|HASTE|SUMMON|TELE_AWAY|PLASMA_BOLT|S_ANGEL)
			    ,(NONE8)
        ,1800,255,30,68,29,'A',{50,10}
		,{22,23,23,22},38,6,'Ü'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Loge, Elemental Spirit of Fire"    ,(MV_ATT_NORM|MV_20|THRO_CREAT)
			    ,(0x4L|FIRE_BALL)
			   ,(EVIL|IM_POISON|CHARM_SLEEP|IM_FIRE|MAX_HP|UNIQUE
			   |CAN_SPEAK|MINDLESS|NONLIVING)
			   ,(PLASMA_BOLT),(NONE8)
    ,3000,50,12,50,33,'E',{60,25},{103,103,103,103},38,3,'ç'
#ifdef TC_COLOR
  , RED
#endif
},

{"Lloigor"             ,(HAS_2D2|CARRY_OBJ|HAS_1D2|MV_ATT_NORM
		|THRO_WALL|MV_INVIS)
		,(0x7L|HOLD_PERSON|FEAR|BLINDNESS|MANA_DRAIN
		|TELE_TO)
		,(EVIL|CHARM_SLEEP|IM_FROST|MAX_HP|
		  IM_POISON|NO_INFRA|INTELLIGENT)
		,(NETHER_BOLT),(BREATH_GR)
	,2200,10,20,55,30,'R',{50,10},{57,57,236,0},38,3,'¥'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Fire vampire"                  ,(MV_ATT_NORM|HAS_60|CARRY_OBJ|THRO_DR)
		,(0x7L|BLINDNESS|FIRE_BOLT|MANA_DRAIN)
		,(EVIL|DEMON|CHARM_SLEEP|IM_FIRE|MAX_HP|IM_POISON)
		,(PLASMA_BOLT),(NONE8)
    ,1000,80,20,50,26,'&',{18,8},{102,87,0,0},38,2,'Ñ'
#ifdef TC_COLOR
  , RED
#endif
},

{"Nether wraith"            ,(HAS_4D2|CARRY_OBJ|HAS_90|THRO_DR|MV_ATT_NORM|
			      MV_INVIS|THRO_WALL)
			    ,(0x6L|CAUSE_CRIT|FEAR|BLINDNESS)
			    ,(EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|MAX_HP|
			      HURT_LIGHT|NO_INFRA|IM_POISON)
			    ,(NETHER_BOLT|MIND_BLAST|DARKNESS),(NONE8)
    ,1700,10,20,55,32,'W',{60,8},{10,10,202,0},39,2,'Ø'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Castle Amber guard"                 ,(MV_ATT_NORM|THRO_DR|HAS_60|
		  CARRY_OBJ|CARRY_GOLD),(0x9L|MONSTER)
	,(IM_POISON|MAX_HP|CHARM_SLEEP|FEARLESS)
		,(NONE8),(NONE8)
    ,500,50,20,80,23,'k',{30,30},{17,17,17,0},39,3,'˝'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Ettin"                    ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2),(NONE8)
			    ,(TROLL|EVIL|IM_POISON|MAX_HP|CHARM_SLEEP)
			    ,(NONE8),(NONE8)
    ,1000,30,20,100,22,'T',{50,30},{19,19,19,0},39,3,'∏'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Moire, Queen of Rebma"   ,(MV_ATT_NORM|MV_20|THRO_CREAT|HAS_2D2|CARRY_OBJ)
			    ,(0x4L|FROST_BALL)
			    ,(EVIL|IM_POISON|CHARM_SLEEP|MAX_HP|UNIQUE
		  |NO_INFRA|GOOD|CAN_SPEAK)
			    ,(WATER_BOLT|WATER_BALL|ICE_BOLT),(NONE8)
    ,3250,50,12,40,31,'p',{90,25},{23,23,23,23},39,3,'Ú'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Kavlax the Many-Headed"   ,(MV_ATT_NORM|HAS_4D2|CARRY_OBJ|THRO_DR)
			    ,(0x4L|BREATH_FI|BREATH_FR|BREATH_L|BREATH_A)
			    ,(EVIL|DRAGON|CHARM_SLEEP|MAX_HP|UNIQUE|GOOD|
			     IM_ACID|IM_FROST|IM_FIRE|IM_LIGHTNING|CAN_SPEAK)
			    ,(BREATH_CO|BREATH_SD|BREATH_SH|BREATH_NE)
			    ,(BREATH_GR)
       ,3000,30,20,85,32,'d',{130,10},{56,39,39,39},39,3,''
#ifdef TC_COLOR
  , ANY
#endif
},

{"Ancient white dragon"     ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x9L|BREATH_FR|FEAR|BLINDNESS|CONFUSION)
			    ,(EVIL|DRAGON|IM_FROST|CHARM_SLEEP|MAX_HP|NO_INFRA)
			    ,(NONE8),(NONE8)
    ,2500,80,20,90,29,'D',{88,8},{55,55,39,124},39,1,'â'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Ancient green dragon"     ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x9L|BREATH_G|FEAR|BLINDNESS|CONFUSION)
			    ,(EVIL|DRAGON|IM_POISON|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,2400,80,20,85,29,'D',{90,8},{54,54,38,162},39,1,'â'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Chthonian"           ,(MV_ATT_NORM|CARRY_GOLD|HAS_4D2|HAS_2D2)
		,(0x5L|FEAR|CONFUSION|HOLD_PERSON|S_DEMON)
		,(IM_POISON|IM_FIRE|IM_FROST|BREAK_WALL|DESTRUCT|EVIL)
		,(MIND_BLAST|FORGET|HEAL|HASTE),(NONE8)
     ,2300,20,20,90,27,'w',{100,10},{77,77,235,254},39,3,'˚'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Night mare"               ,(MV_ATT_NORM|THRO_DR|HAS_2D2|CARRY_GOLD)
		,(0xFL|FEAR|HOLD_PERSON),(UNDEAD|IM_POISON|IM_FROST|EVIL|
			     CHARM_SLEEP|NO_INFRA|MAX_HP|FEARLESS)
			    ,(NONE8),(NONE8)
    ,2900,0,30,85,29,'q',{150,10},{236,20,20,216},39,3,'ı'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Vampire lord"             ,(MV_ATT_NORM|THRO_DR|HAS_4D2|CARRY_GOLD|
			     CARRY_OBJ|HAS_60)
			    ,(0x7L|CAUSE_CRIT|MANA_DRAIN|FEAR|HOLD_PERSON|
			      BLINDNESS)
			    ,(UNDEAD|EVIL|MAX_HP|CHARM_SLEEP|HURT_LIGHT|
			      IM_FROST|HURT_LIGHT|NO_INFRA|IM_POISON)
			    ,(BRAIN_SMASH|NETHER_BOLT|RAZOR|DARKNESS),(NONE8)
    ,1800,10,20,70,32,'V',{62,25},{5,5,5,198},39,3,'Ÿ'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Ancient black dragon"     ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x9L|BREATH_A|FEAR|BLINDNESS|CONFUSION)
			    ,(EVIL|DRAGON|IM_ACID|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
    ,2500,70,20,90,29,'D',{90,8},{55,55,38,113},39,1,'â'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Disenchanter worm"        ,(MULTIPLY|MV_ATT_NORM|MV_40),(NONE8),
			     (ANIMAL|HURT_LIGHT|SEMI),(NONE8),(NONE8)
    ,30,10,7,5,17,'w',{10,8},{208,0,0,0},40,3,'˚'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Rotting quylthulg"        ,(MV_INVIS),(0x2L|S_UNDEAD|BLINK|TELE)
		,(EVIL|MAX_HP|UNDEAD|IM_FROST|NO_INFRA|
			     CHARM_SLEEP|FEARLESS),(NONE8),(NONE8)
    ,1500,0,20,1,31,'Q',{20,8},{0,0,0,0},40,1,'ú'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Model T-1000 Terminator"
		,(MV_INVIS|MV_ATT_NORM|THRO_DR|
		 CARRY_OBJ|CARRY_GOLD|HAS_60|HAS_90)
		 ,(0x5L|FIRE_BALL)
		,(EVIL|IM_POISON|IM_FROST|MAX_HP|INTELLIGENT|
		 CHARM_SLEEP|IM_LIGHTNING|NO_INFRA|FEARLESS|NONLIVING)
		 ,(BREATH_SH|LIGHT_BOLT|LIGHT_BALL|ACID_BALL|HEAL|
		 MISSILE|PLASMA_BOLT),(BREATH_GR|BREATH_SL|NUKE_BALL)
    ,4000,5,20,140,32,'g',{40,30},{20,20,21,21},44,3,''
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Lesser titan"             ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      CARRY_GOLD|HAS_4D2|HAS_2D2)
			    ,(0x3L|FEAR|TELE_TO)
			    ,(EVIL|GIANT|MAX_HP|INTELLIGENT)
			    ,(SUMMON|HEAL),(NONE8)
       ,3500,15,30,80,31,'P',{35,30},{216,216,216,216},40,3,'ë'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"9-headed hydra"           ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_4D2|HAS_2D2)
			    ,(0x4L|FEAR|FIRE_BOLT|BREATH_FI)
			    ,(ANIMAL|IM_FIRE|SEMI),(NONE8),(NONE8)
	   ,3000,20,20,95,30,'R',{100,12},{106,106,106,106},40,2,'·'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Enchantress"              ,(HAS_2D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
			    ,(0x2L|S_DRAGON|BLINDNESS)
			    ,(EVIL|CHARM_SLEEP|MAX_HP|GOOD),(NONE8),(NONE8)
    ,2100,10,20,60,42,'p',{40,13},{15,15,16,0},40,4,'Ú'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Archpriest"               ,(HAS_2D2|CARRY_OBJ|HAS_90|THRO_DR|MV_ATT_NORM)
			    ,(0x2L|HOLD_PERSON|BLINDNESS|CONFUSION|
			      MONSTER|S_UNDEAD|CAUSE_CRIT)
			    ,(INTELLIGENT|EVIL|CHARM_SLEEP|MAX_HP)
			    ,(HEAL),(NONE8)
    ,1800,10,20,60,31,'p',{40,13},{17,17,18,0},40,2,'¯'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Sorceror"                 ,(HAS_4D2|CARRY_OBJ|HAS_90|THRO_DR|MV_ATT_NORM)
			    ,(0x2L|BLINK|TELE_TO|BLINDNESS|CONFUSION|
			      MONSTER|S_UNDEAD|CAUSE_CRIT|S_DRAGON|
			      FIRE_BALL|FROST_BALL|ACID_BOLT)
			    ,(EVIL|CHARM_SLEEP|MAX_HP),(TRAP_CREATE),(NONE8)
    ,2150,10,20,60,42,'p',{40,13},{16,16,16,0},40,2,'¯'
#ifdef TC_COLOR
  , RED
#endif
},

{"Xaren"                     ,(MV_ATT_NORM|THRO_WALL|THRO_DR|PICK_UP)
			    ,(NONE8),(IM_FIRE|IM_FROST|IM_POISON|NO_INFRA|NONLIVING|
			     IM_LIGHTNING|CHARM_SLEEP|HURT_ROCK|MAX_HP|MINDLESS)
			    ,(NONE8),(NONE8)
    ,1200,10,20,80,32,'X',{40,8},{17,17,17,17},40,1,'Ö'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Giant roc"                ,(MV_ATT_NORM),(NONE8),(ANIMAL|IM_LIGHTNING)
			    ,(NONE8),(NONE8)
    ,1000,10,20,70,23,'B',{80,13},{78,78,284,0},40,3,'í'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Nazgul"      ,(HAS_2D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
		,(FEAR|HOLD_PERSON|MANA_DRAIN|0x6L)
		,(EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|MAX_HP
			      |HURT_LIGHT|GOOD|NO_INFRA|IM_POISON)
		,(BREATH_LD),(NONE8)
     ,9000,10,90,60,33,'W',{50,38},{235,23,23,199},45,3,'É'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Minotaur"                 ,(MV_ATT_NORM),(NONE8),(EVIL),(NONE8),(NONE8)
        ,2100,10,13,25,42,'H',{100,10}
		,{227,227,228,228},40,2,'≤'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Jasra, Mistress of Brand"
	  ,(MV_ATT_NORM|THRO_DR|HAS_2D2|HAS_1D2|CARRY_OBJ)
			    ,(0x2L|HOLD_PERSON|CAUSE_CRIT|FIRE_BOLT|FEAR)
			    ,(EVIL|IM_FIRE|CHARM_SLEEP|UNIQUE|INTELLIGENT|
			     GOOD|IM_POISON|MAX_HP|IM_ACID|CAN_SPEAK)
			    ,(PLASMA_BOLT|ACID_BALL),(S_REPTILE)
    ,9000,5,30,100,31,'n',{40,60},{246,267,235,235},40,3,'Ú'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Death drake"              ,(MV_ATT_NORM|MV_INVIS|THRO_DR|HAS_2D2|HAS_4D2|
			     CARRY_OBJ|PICK_UP|THRO_WALL)
			    ,(0x6L|FEAR|CONFUSION|SLOW)
			    ,(EVIL|IM_FROST|CHARM_SLEEP|MAX_HP|DRAGON)
			    ,(BREATH_LD),(NONE8)
     ,3500,30,25,100,32,'D',{105,10},{56,56,236,0},40,2,'â'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Ancient red dragon"       ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x6L|BREATH_FI|FEAR|BLINDNESS|CONFUSION)
			    ,(EVIL|DRAGON|IM_FIRE|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
     ,2750,70,20,100,29,'D',{105,10},{56,56,40,107},40,1,'â'
#ifdef TC_COLOR
  , RED
#endif
},

{"Ancient brass dragon"      ,(HAS_4D2|CARRY_GOLD|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x6L|FEAR|BLINDNESS|CONFUSION)
			    ,(DRAGON|CHARM_SLEEP|MAX_HP),(BREATH_SD),(NONE8)
     ,4000,200,20,100,29,'D',{150,10},{56,56,40,234},40,2,'â'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Great crystal drake"      ,(MV_ATT_NORM|MV_INVIS|THRO_DR|HAS_4D2|HAS_2D2|
			     CARRY_OBJ)
			    ,(0x6L|FEAR|CONFUSION|SLOW)
			    ,(EVIL|IM_FROST|CHARM_SLEEP|MAX_HP|DRAGON)
			    ,(BREATH_SH),(NONE8)
       ,3900,30,25,100,28,'D',{150,10},{55,55,39,0},40,2,'â'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Clubber demon"                    ,(MV_ATT_NORM|HAS_60|CARRY_OBJ|THRO_DR)
			    ,(0x8L|BLINDNESS|CONFUSION)
			    ,(EVIL|DEMON|CHARM_SLEEP|IM_FIRE|MAX_HP|GROUP)
			    ,(NONE8),(NONE8)
    ,1000,80,20,50,22,'&',{20,11},{6,6,78,78},40,2,'Ñ'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Death quasit"             ,(HAS_4D2|HAS_2D2|HAS_90|CARRY_OBJ|MV_INVIS|
			      THRO_WALL|MV_ATT_NORM)
			  ,(0xAL|FEAR|CONFUSION|BLINDNESS|CAUSE_CRIT|S_DEMON)
			    ,(EVIL|IM_POISON|CHARM_SLEEP|MAX_HP|DEMON|IM_FIRE
			      |INTELLIGENT),(FORGET),(NONE8)
    ,1000,0,20,80,44,'I',{55,8},{177,58,58,0},40,3,'Ì'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Strygalldwir"   ,(HAS_4D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM|THRO_WALL)
		,(0x3L|CAUSE_CRIT|HOLD_PERSON|FEAR|BLINDNESS|ACID_BOLT|S_DEMON)
		,(EVIL|DEMON|CHARM_SLEEP|IM_FROST|MAX_HP|UNIQUE
		  |GOOD|IM_POISON|CAN_SPEAK)
		,(FORGET|NETHER_BOLT|DARKNESS|MIND_BLAST),(NONE8)
      ,8000,10,90,60,33,'&',{35,35},{22,22,224,87},41,3,'Ñ'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Shoggoth"      ,(HAS_2D2|CARRY_OBJ|HAS_90|THRO_DR|MV_ATT_NORM)
		,(0x4L|BLINDNESS|CONFUSION|
		  MONSTER|CAUSE_CRIT|S_DEMON|
		  ACID_BOLT|SLOW)
		,(EVIL|CHARM_SLEEP|MAX_HP|HURT_LIGHT|IM_ACID|IM_POISON
		|IM_FROST)
			    ,(HEAL|DARKNESS),(NONE8)
    ,4500,10,20,80,39,'j',{40,20},{115,77,238,77},41,3,'Ó'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Monastic lich"              ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_4D2|HAS_2D2)
			    ,(0x3L|FEAR|CONFUSION|BLINDNESS|HOLD_PERSON|
			      CAUSE_CRIT|MANA_DRAIN|TELE_TO|BLINK|S_UNDEAD)
			    ,(UNDEAD|IM_POISON|IM_FROST|EVIL|MAX_HP|
			      CHARM_SLEEP|NO_INFRA|INTELLIGENT)
		,(BRAIN_SMASH),(NONE8)
       ,10000,50,20,80,32,'L',{42,42},{263,201,214,181},41,2,'ü'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Byakhee"                   ,(MV_ATT_NORM|HAS_2D2|CARRY_OBJ|THRO_DR)
		,(0x9L|S_DEMON|FIRE_BOLT|CONFUSION)
		,(EVIL|DEMON|CHARM_SLEEP|IM_POISON|IM_FROST|MAX_HP|GROUP)
			    ,(NONE8),(NONE8)
	,2000,80,20,40,30,'&',{20,15},{13,13,87,0},41,3,'Ñ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Judge Fire"      ,(HAS_4D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM|PICK_UP)
		,(0x3L|CAUSE_CRIT|FEAR|BLINDNESS
		  |MONSTER|FIRE_BALL|BREATH_FI|FIRE_BOLT|TELE|BLINK)
		,(EVIL|UNDEAD|CHARM_SLEEP|IM_FIRE|IM_ACID|MAX_HP|UNIQUE
			      |GOOD|IM_POISON|NO_INFRA|CAN_SPEAK)
		,(PLASMA_BOLT|DARKNESS),(NONE8)
      ,12000,10,90,70,32,'s',{35,50},{103,103,199,99},41,3,'µ'
#ifdef TC_COLOR
  , RED
#endif
},

{"Rinaldo, Son of Brand",(MV_ATT_NORM|HAS_2D2|CARRY_OBJ|THRO_DR)
			    ,(0x2L|CAUSE_CRIT|MANA_BOLT)
			    ,(GOOD|UNIQUE|MAX_HP|INTELLIGENT|CHARM_SLEEP|EVIL|
			     IM_POISON|IM_FROST|IM_ACID|IM_LIGHTNING|CAN_SPEAK)
		,(WATER_BOLT),(LOGRUS_BALL)
       ,7000,40,20,120,32,'a',{80,20},{218,218,230,230},41,3,'«'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Archon"            ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      CARRY_GOLD|HAS_4D2|HAS_2D2|HAS_1D2)
			    ,(0x3L|FEAR|BLINDNESS|TELE_TO
			      |CAUSE_SERIOUS|MANA_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|IM_ACID|IM_LIGHTNING|
			      GOOD|INTELLIGENT|FEARLESS)
			    ,(S_ANGEL|RAZOR),(NONE8)
        ,15000,255,30,140,45,'A',{120,30}
		,{217,217,218,218},41,6,'Ü'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Formless spawn of Tsathogghua"
		   ,(MV_ATT_NORM|HAS_90|CARRY_OBJ|THRO_DR)
			    ,(0x9L|S_DEMON|FIRE_BOLT)
		,(EVIL|DEMON|CHARM_SLEEP|IM_POISON|MAX_HP)
		,(HEAL),(NONE8)
    ,1750,80,20,40,21,'&',{22,15},{13,13,240,240},41,2,'Ñ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Judge Mortis"      ,(HAS_4D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM|PICK_UP)
			    ,(0x3L|CAUSE_CRIT|HOLD_PERSON|FEAR|BLINDNESS
		  |MONSTER|SLOW|TELE|BLINK|S_UNDEAD|BREATH_G)
		,(EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|MAX_HP|UNIQUE
		  |HURT_LIGHT|IM_ACID|GOOD|IM_POISON|NO_INFRA|CAN_SPEAK)
		,(NETHER_BOLT|BREATH_LD|NETHER_BALL|ST_CLOUD),(NONE8)
      ,13000,10,90,70,32,'z',{35,50},{152,152,199,99},41,3,'›'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Hunting horror"               ,(MV_ATT_NORM|HAS_1D2|CARRY_OBJ|THRO_DR)
		,(0x9L|BLINDNESS|CONFUSION|S_DEMON)
		,(EVIL|DEMON|CHARM_SLEEP|IM_POISON|MAX_HP|HURT_LIGHT)
			    ,(NONE8),(NONE8)
    ,2000,80,20,60,32,'&',{30,15},{77,77,160,0},42,2,'∞'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Undead beholder"          ,(MV_ATT_NORM)
			    ,(0x2L|S_UNDEAD|SLOW|MANA_DRAIN|MANA_BOLT)
			    ,(UNDEAD|EVIL|CHARM_SLEEP|MAX_HP|IM_POISON
			     |IM_FIRE|IM_LIGHTNING|IM_ACID|IM_FROST|NO_INFRA)
			    ,(FORGET|MIND_BLAST|RAZOR|BRAIN_SMASH),(NONE8)
     ,7400,10,30,100,31,'e',{90,30},{223,224,225,226},42,4,'È'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Dread"                    ,(MV_ATT_NORM|MV_20|CARRY_OBJ|
			     HAS_60|THRO_WALL|PICK_UP|MV_INVIS)
			    ,(0xFL|HOLD_PERSON|MANA_DRAIN|BLINDNESS|CONFUSION)
			    ,(UNDEAD|EVIL|IM_FROST|NO_INFRA|CHARM_SLEEP|GROUP|
			     IM_POISON),(NETHER_BOLT),(NONE8)
    ,600,10,20,30,31,'G',{25,20},{235,235,80,0},43,1,'Ò'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},



{"Ancient multi-hued dragon",(MV_ATT_NORM|HAS_4D2|CARRY_OBJ|
			      THRO_DR|HAS_60|HAS_90|HAS_2D2|HAS_1D2)
			    ,(0x5L|BREATH_G|BREATH_L|BREATH_A|BREATH_FR|
			      BREATH_FI|FEAR|CONFUSION|BLINDNESS)
			    ,(IM_FROST|IM_ACID|IM_POISON|IM_LIGHTNING|
			      IM_FIRE|EVIL|DRAGON|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
	  ,13000,70,20,100,30,
	      'D',{52,40},{57,57,42,163},43,1,'â'
#ifdef TC_COLOR
  , MULTI
#endif
},

{"Oriental dragon"         ,(MV_ATT_NORM|THRO_DR|HAS_60|HAS_90|HAS_4D2|
			    HAS_2D2|HAS_1D2|CARRY_OBJ|MV_INVIS|THRO_WALL)
			   ,(0x5L|CONFUSION|BLINDNESS)
			   ,(DRAGON|CHARM_SLEEP|MAX_HP)
			   ,(BREATH_CO),(BREATH_LT|BREATH_DA)
	,11000,15,25,100,30,'D',{52,40},{57,57,42,141},43,2,'â'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Judge Fear"      ,(HAS_4D2|CARRY_OBJ|THRO_DR|MV_ATT_NORM)
			    ,(0x3L|FIRE_BALL|CAUSE_CRIT|HOLD_PERSON|
		  FEAR|BLINDNESS|S_UNDEAD|MANA_DRAIN)
			    ,(EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|MAX_HP|UNIQUE
			      |HURT_LIGHT|IM_FIRE|GOOD|IM_POISON|NO_INFRA|CAN_SPEAK)
		,(NETHER_BALL|BREATH_LD)
		,(BREATH_DA)
      ,12000,10,90,70,32,'L',{35,50},{23,23,199,99},43,4,'ü'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Dark young of Shub-Niggurath"
		    ,(MV_ATT_NORM|HAS_1D2|CARRY_OBJ|THRO_DR)
			    ,(0x9L|CAUSE_SERIOUS|BLINDNESS|S_DEMON)
		,(EVIL|DEMON|CHARM_SLEEP|IM_FIRE|MAX_HP|IM_POISON|IM_ACID
		|HURT_LIGHT)
		,(HEAL),(NONE8)
    ,5000,80,20,75,31,'&',{40,15},{80,80,80,80},43,2,'Ñ'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"The Incredible Hulk"  ,(PICK_UP|MV_ATT_NORM),(0xCL)
		,(IM_POISON|IM_FIRE|IM_ACID|IM_LIGHTNING|
		  CHARM_SLEEP|HURT_ROCK|EVIL|MAX_HP|NO_INFRA|BREAK_WALL|
		  DESTRUCT|UNIQUE|GIANT),(MISSILE),(NONE8)
    ,6000,90,10,97,31,'U',{90,20},{212,235,235,235},43,4,'ä'
#ifdef TC_COLOR
  , GREEN
#endif
},


{"Ariel, Queen of Air"      ,(MV_ATT_NORM|MV_20|THRO_CREAT),(0x5L|FROST_BALL)
			    ,(EVIL|IM_POISON|CHARM_SLEEP|IM_FIRE|IM_FROST|
			      IM_LIGHTNING|IM_ACID|IM_POISON|MAX_HP|UNIQUE|NONLIVING|
			      NO_INFRA|CAN_SPEAK|MINDLESS),(LIGHT_BALL|LIGHT_BOLT),(NONE8)
    ,8000,50,12,50,43,'E',{60,45},{22,89,22,89},44,4,'ç'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"11-headed hydra"          ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_4D2|HAS_2D2)
			    ,(0x4L|FEAR|FIRE_BOLT|FIRE_BALL|BREATH_FI)
			    ,(ANIMAL|SEMI|IM_FIRE),(PLASMA_BOLT),(NONE8)
      ,6000,20,20,100,31,'R',{100,18},{107,107,107,107},44,2,'·'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"High priest"            ,(HAS_4D2|CARRY_OBJ|HAS_90|THRO_DR|MV_ATT_NORM)
			    ,(0x2L|HOLD_PERSON|BLINDNESS|S_UNDEAD)
			    ,(EVIL|CHARM_SLEEP|MAX_HP|INTELLIGENT)
			    ,(HEAL|BRAIN_SMASH|RAZOR|SUMMON),(NONE8)
    ,5000,10,20,60,32,'p',{40,20},{17,17,266,0},44,2,'¯'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Grim reaper"             ,(MV_ATT_NORM|MV_20|HAS_2D2|CARRY_OBJ|
			     THRO_WALL|PICK_UP|MV_INVIS)
			   ,(0x9L|HOLD_PERSON|MANA_DRAIN|BLINDNESS
			     |CONFUSION|S_UNDEAD)
	       ,(UNDEAD|EVIL|GOOD|IM_FROST|NO_INFRA|CHARM_SLEEP|MAX_HP|
			     INTELLIGENT|IM_POISON)
			   ,(NETHER_BOLT|RAZOR|TELE_LEV),(NONE8)
    ,8000,10,20,100,42,'G',{60,20},{235,235,201,80},44,2,'—'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Dark and stormy knight"           ,(MV_ATT_NORM|HAS_2D2|HAS_1D2
		|CARRY_OBJ|MV_INVIS)
	       ,(0x3L|TELE_TO|FEAR|BLINDNESS|HOLD_PERSON|MANA_DRAIN)
	       ,(IM_FROST|IM_FIRE|IM_POISON|IM_LIGHTNING|GOOD|
		IM_ACID|MAX_HP|CHARM_SLEEP|EVIL|HURT_LIGHT|INTELLIGENT)
	       ,(HEAL|LIGHT_BALL|HEAL|HASTE|TELE_AWAY|NETHER_BALL)
	       ,(S_WRAITH|DARK_STORM)
    ,20000,5,30,60,42,'k',{40,55},{23,23,199,214},46,3,'˝'
#ifdef TC_COLOR
  , BLUE
#endif
},

/*
 * Unique monsters have no word before them e.g Tiamat etc.. hits you
 * not The Tiamat etc.. hits you or A Tiamat etc... hits you,
 * But DONT use capital letters at the beginning unless it is a name
 * e.g. You hit a Balrog...
 * NOT You hit A Balrog...
 */

{"Drolem"                   ,(MV_ATT_NORM|THRO_DR),(0x6L|BLINDNESS|CONFUSION|
			     SLOW|BREATH_G)
			    ,(DRAGON|CHARM_SLEEP|IM_FROST|IM_FIRE|NONLIVING|
			     IM_POISON|IM_LIGHTNING|MAX_HP|NO_INFRA|FEARLESS|MINDLESS)
			    ,(MISSILE),(NONE8)
      ,12000,30,25,130,33,'g',{100,30},{48,48,238,238},44,3,'â'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Scatha the Worm"          ,(HAS_4D2|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x3L|BREATH_FR|CAUSE_CRIT|CONFUSION)
			    ,(EVIL|DRAGON|IM_FROST|CHARM_SLEEP|MAX_HP|NO_INFRA|
			      UNIQUE|GOOD|CAN_SPEAK),(NONE8),(NONE8)
    ,17000,70,20,130,33,'D',{150,12},{56,56,56,276},44,2,'â'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Judge Death"    ,(HAS_1D2|HAS_60|HAS_90|HAS_4D2|CARRY_OBJ|THRO_DR|
		    MV_ATT_NORM)
		,(0x3L|CAUSE_CRIT|HOLD_PERSON|
			      FEAR|BLINDNESS|S_UNDEAD)
	,(EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|MAX_HP|UNIQUE|IM_ACID
		  |HURT_LIGHT|GOOD|INTELLIGENT|IM_POISON|IM_LIGHTNING|
		 NO_INFRA|CAN_SPEAK),(NETHER_BALL|RAZOR),(S_GUNDEAD)
     ,14000,10,90,90,33,'W',{45,50},{271,271,199,99},44,3,'µ'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Smaug the Golden"         ,(HAS_4D2|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x3L|BREATH_FI|CAUSE_CRIT|CONFUSION)
			    ,(EVIL|DRAGON|IM_FIRE|CHARM_SLEEP|MAX_HP|
			      UNIQUE|GOOD|CAN_SPEAK),(NONE8),(NONE8)
     ,19000,70,20,100,33,'D',{150,13},{56,56,56,276},45,2,'â'
#ifdef TC_COLOR
  , RED
#endif
},

{"Dracolich"                ,(MV_ATT_NORM|THRO_DR|HAS_2D2|HAS_4D2|
			     CARRY_OBJ|PICK_UP)
			    ,(0x6L|FEAR|CONFUSION|MANA_BOLT|BREATH_FR)
			    ,(EVIL|IM_FROST|CHARM_SLEEP|UNDEAD|
			    MAX_HP|DRAGON|IM_POISON|NO_INFRA),(NONE8),(NONE8)
    ,18000,30,25,120,32,'D',{70,50},{57,57,236,236},46,2,'â'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Greater titan"            ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      CARRY_GOLD|HAS_4D2|HAS_2D2)
			    ,(0x3L|TELE_TO)
			    ,(EVIL|GIANT|MAX_HP|INTELLIGENT)
			    ,(SUMMON|HEAL),(NONE8)
      ,13500,15,30,125,32,'P',{75,50},{269,269,269,269},46,3,'ë'
#ifdef TC_COLOR
  , MAGENTA
#endif
},
{"Tankero"                    ,(MV_ATT_NORM),(NONE8),(ANIMAL|GROUP)
			    ,(NONE8),(NONE8)
     ,3100,100,20,60,32,'q',{90,12},{227,227,233,76},46,2,'ı'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},
{"Spectral tyrannosaur"                ,(MV_ATT_NORM|THRO_DR|HAS_4D2|CARRY_OBJ)
		,(0x6L|HOLD_PERSON|FEAR|BREATH_G)
		,(ANIMAL|EVIL|CHARM_SLEEP|MAX_HP|IM_POISON
		|IM_FROST|UNDEAD|IM_ACID),(BREATH_NE|BREATH_LD),(NONE8)
	,14000,30,25,120,30,'R',{70,50},{39,39,236,146},46,2,'à'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Death mold"               ,(MV_ONLY_ATT|THRO_DR),(NONE8),(IM_FIRE|IM_POISON|
			     IM_FROST|IM_ACID|IM_LIGHTNING|ANIMAL|EVIL)
			    ,(NONE8),(NONE8)
      ,1000,0,200,60,55,'m',{200,10},{257,257,257,202},47,1,'Ó'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Mordred, Betrayer of King Arthur"  ,(HAS_4D2|CARRY_OBJ|HAS_90|HAS_1D2|
		  HAS_60|MV_ATT_NORM|THRO_DR)
		,(0x3L|FEAR|HOLD_PERSON|CAUSE_CRIT|CONFUSION)
		,(EVIL|INTELLIGENT|IM_FROST|IM_POISON|IM_FIRE|CHARM_SLEEP
		|MAX_HP|UNIQUE|GOOD),(RAZOR|MIND_BLAST|HASTE|SUMMON
		|NETHER_BALL|CAN_SPEAK),(S_WRAITH|S_GUNDEAD)
       ,20000,40,20,100,31,'k',{150,15},{235,230,200,214},47,3,'˝'
#ifdef TC_COLOR
  , RED
#endif
},

{"Greater wall monster"            ,(MV_ATT_NORM|MV_75|MULTIPLY|THRO_WALL)
		,(NONE8),(IM_FROST|NO_INFRA|IM_ACID|IM_FIRE|IM_LIGHTNING
	 |IM_POISON|CHARM_SLEEP|HURT_ROCK|NONLIVING),(NONE8),(NONE8)
    ,500,75,20,78,34,'±',{40,15},{23,23,23,23},48,4,'±'
#ifdef TC_COLOR /* Probably THE nastiest monster there is... the genocide
		   canditate nr.1 */
  , LIGHTGRAY
#endif
},

{"Fafner the Dragon"
			    ,(HAS_4D2|CARRY_OBJ|HAS_90|HAS_2D2|
			      HAS_60|MV_ATT_NORM)
			    ,(0x5L|BREATH_FI|CAUSE_CRIT|CONFUSION|BREATH_G)
			    ,(EVIL|DRAGON|IM_FIRE|CHARM_SLEEP|MAX_HP|
			      UNIQUE|GOOD|IM_POISON|CAN_SPEAK),(NONE8),(NONE8)
      ,25000,70,20,120,33,'D',{110,25},{271,273,280,250},49,2,'â'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Balrog"            ,(MV_ATT_NORM|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|THRO_DR)
	       ,(0x4L|CONFUSION|BLINDNESS|S_DEMON|BREATH_FI|CAUSE_CRIT)
		,(EVIL|DEMON|CHARM_SLEEP|IM_FIRE|MAX_HP|GOOD)
		,(HASTE|NETHER_BALL),(S_GUNDEAD)
    ,10000,80,20,75,32,'&',{65,45},{101,78,101,23},49,3,'Ñ'
#ifdef TC_COLOR
  , RED
#endif
},

{"Muar, the Balrog",         (MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
			    ,(0x4L|FEAR|S_UNDEAD|BREATH_FI|CONFUSION|S_DEMON)
			    ,(EVIL|DEMON|CHARM_SLEEP|IM_FIRE|MAX_HP|
		  UNIQUE|SPECIAL|BREAK_WALL|CAN_SPEAK),(NONE8),(NONE8)
       ,38000L,80,20,100,33,'&',{60,60},{104,78,214,0},50,3,'Ñ'
#ifdef TC_COLOR
  , RED
#endif
},


{"Nether hound"             ,(MV_ATT_NORM|THRO_DR),(0x5L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(BREATH_LD)
			    ,(NONE8)
       ,5000,0,30,100,30,'Z',{60,10},{39,39,39,58},51,2,'≥'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Time hound"               ,(MV_ATT_NORM|THRO_DR),(0x8L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(NONE8)
			    ,(BREATH_TI)
    ,5000,0,30,100,38,'Z',{60,10},{39,39,39,58},51,4,'≥'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Plasma hound"             ,(MV_ATT_NORM|THRO_DR),(0x5L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP|IM_FIRE),(NONE8)
			    ,(BREATH_PL)
    ,5000,0,30,100,29,'Z',{60,10},{39,39,39,58},51,2,'≥'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Demonic quylthulg"        ,(MV_INVIS),(0x2L|S_DEMON|BLINK|TELE)
		,(MAX_HP|CHARM_SLEEP|EVIL|FEARLESS),(NONE8),(NONE8)
    ,3000,0,20,1,32,'Q',{60,8},{0,0,0,0},51,1,'ú'
#ifdef TC_COLOR
  , RED
#endif
},

{"Great storm wyrm"         ,(HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x6L|BREATH_L|FEAR|BLINDNESS
			      |CONFUSION)
			    ,(EVIL|DRAGON|IM_LIGHTNING|CHARM_SLEEP
			     |MAX_HP|GOOD),(NONE8),(NONE8)
      ,17000,80,30,150,31,'D',{50,60},{57,57,57,277},51,2,'â'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Baphomet the Minotaur Lord" ,(HAS_4D2|HAS_1D2|CARRY_OBJ|MV_ATT_NORM)
			    ,(0x6L|SLOW|MANA_BOLT),(EVIL|CHARM_SLEEP|UNIQUE|
			     MAX_HP|GOOD|IM_POISON|IM_FIRE|CAN_SPEAK),(PLASMA_BOLT|
			     MISSILE|LIGHT_BALL),(BREATH_WA)
      ,18000,30,30,120,42,'H',{70,50},{282,282,212,212},51,4,'≤'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Pak, Master of Sinanju"   ,(HAS_4D2|HAS_1D2|CARRY_OBJ|MV_ATT_NORM|PICK_UP
		|MV_INVIS)
		,(0x6L|FEAR),(CHARM_SLEEP|MAX_HP|UNIQUE|CAN_SPEAK|
		 GOOD|IM_POISON),(HASTE|TRAP_CREATE),(NONE8)
    ,22500,0,40,90,55,'p',{50,48},{263,261,258,259},52,3,'‡'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Santa Claus",   (HAS_4D2|HAS_2D2|CARRY_OBJ|THRO_DR|MV_20|PICK_UP|MV_ATT_NORM)
	,(0x3L|CAUSE_CRIT|HOLD_PERSON|
		  MANA_DRAIN|FEAR|BLINDNESS|S_UNDEAD)
		,(CHARM_SLEEP|IM_FROST|MAX_HP|CAN_SPEAK|UNIQUE
		  |SPECIAL|INTELLIGENT|IM_POISON|IM_FIRE|IM_LIGHTNING|
		 NO_INFRA),(RAZOR|NETHER_BALL|MIND_BLAST|TRAP_CREATE|FORGET)
		 ,(S_UNIQUE|S_GUNDEAD|S_ANCIENTD)
       ,50000U,70,90,100,58,'h',{50,50},{212,278,278,278},52,3,'Ï'
#ifdef TC_COLOR
  , RED
#endif
},

{"Lord of Chaos"    ,(MV_ATT_NORM|HAS_4D2|CARRY_OBJ|MV_INVIS)
	      ,(0x2L|S_DEMON),(IM_FROST|IM_FIRE|IM_POISON|IM_LIGHTNING|
	   IM_ACID|MAX_HP|CHARM_SLEEP|EVIL|SHAPECHANGER)
	      ,(HEAL|S_HOUND|S_SPIDER|S_REPTILE),(LOGRUS_BALL)
      ,16500,5,30,80,42,'p',{40,55},{235,235,230,200},53,3,'¯'
#ifdef TC_COLOR
  , ANY
#endif
},

{"Master of the Universe", (HAS_4D2|CARRY_OBJ|HAS_1D2|THRO_DR|
			  MV_ATT_NORM)
	    ,(0x3L|MANA_BOLT|CAUSE_CRIT|HOLD_PERSON|FEAR
	    |TELE_TO|MANA_DRAIN|BLINK|TELE)
	       ,(CHARM_SLEEP|IM_FROST|MAX_HP|IM_ACID|IM_LIGHTNING
	       |IM_FIRE|IM_POISON|IM_ACID|GOOD|GIANT
	     |INTELLIGENT)
	    ,(SUMMON|HEAL|HASTE|WATER_BALL),(S_ANCIENTD)
      ,20000,10,90,130,33,'P',{80,55},{269,269,269,262},53,3,'ë'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Astral hound"    ,(MV_ATT_NORM|THRO_DR|MV_INVIS|THRO_WALL)
			   ,(0x5L),(ANIMAL|GROUP|CHARM_SLEEP)
			   ,(BREATH_LD),(NONE8)
       ,5000,0,30,100,31,'Z',{60,15},{39,39,39,58},54,3,'≥'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Great ice wyrm"          ,(HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x6L|BREATH_FR|FEAR|BLINDNESS
			      |CONFUSION)
			    ,(EVIL|DRAGON|IM_FROST|CHARM_SLEEP|NO_INFRA
			     |MAX_HP|GOOD),(NONE8),(NONE8)
    ,20000,80,30,170,31,'D',{50,60},{57,57,271,277},54,2,'â'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"The Phoenix"          ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_2D2)
			,(0x3L|FIRE_BOLT|FIRE_BALL|BREATH_FI)
			,(ANIMAL|CHARM_SLEEP|IM_FIRE|IM_LIGHTNING|IM_POISON|
			 IM_ACID|GOOD|CAN_SPEAK|UNIQUE|MAX_HP),(PLASMA_BOLT)
			,(BREATH_LT|BREATH_PL)
      ,40000U,0,60,130,33,'B',{36,100},{251,251,220,220},54,3,'í'
#ifdef TC_COLOR
  , RED
#endif
},

{"Nightcrawler"         ,(MV_ATT_NORM|HAS_2D2|CARRY_OBJ|HAS_1D2|THRO_DR)
			,(0x4L|FEAR|S_UNDEAD|BLINDNESS|MANA_BOLT)
			,(EVIL|UNDEAD|IM_FROST|IM_POISON|IM_FIRE|CHARM_SLEEP|
	     INTELLIGENT|GOOD|NO_INFRA|BREAK_WALL),(BRAIN_SMASH|NETHER_BALL|
			 NETHER_BOLT|BREATH_LD),(NONE8)
      ,8000,10,20,160,31,'W',{80,60},{254,254,255,255},54,4,'˚'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Hand druj"            ,(MV_ONLY_ATT)
			,(0x1L|FEAR|BLINDNESS|CONFUSION|CAUSE_CRIT)
			,(EVIL|NO_INFRA|IM_FROST|CHARM_SLEEP|MAX_HP|UNDEAD
			  |INTELLIGENT|IM_POISON)
			,(DARKNESS|FORGET|TELE_AWAY),(NONE8)
    ,12000,10,20,110,42,'s',{30,20},{0,0,0,0},55,4,'„'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Eye druj"             ,(MV_ONLY_ATT)
			,(0x1L|S_UNDEAD|MANA_BOLT)
			,(EVIL|UNDEAD|CHARM_SLEEP|NO_INFRA|MAX_HP|IM_FROST
			  |IM_POISON|IM_FIRE|INTELLIGENT)
			,(NETHER_BOLT|NETHER_BALL),(NONE8)
    ,24000,10,20,90,42,'s',{40,25},{246,246,0,0},55,4,'È'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Skull druj"           ,(MV_ONLY_ATT)
			,(0x1L|S_UNDEAD|SLOW)
			,(EVIL|UNDEAD|CHARM_SLEEP|NO_INFRA|MAX_HP|IM_FROST
			  |IM_POISON|IM_FIRE|INTELLIGENT)
			,(MIND_BLAST|TRAP_CREATE|NETHER_BOLT|PLASMA_BOLT
			  |BRAIN_SMASH|RAZOR|WATER_BALL),(NONE8)
    ,25000,10,20,120,42,'s',{50,27},{247,236,248,249},55,4,'∂'

#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Chaos vortex"              ,(MV_ATT_NORM|MV_75),(0x6L)
			    ,(CHARM_SLEEP|MINDLESS|NONLIVING),(BREATH_CH),(NONE8)
    ,4000,0,100,80,49,'v',{32,20},{0,0,0,0},55,1,'˙'
#ifdef TC_COLOR
  , ANY
#endif
},

{"Aether vortex"            ,(MV_ATT_NORM|MV_75),(0x6L|BREATH_FI|BREATH_FR|
			    BREATH_G|BREATH_A|BREATH_L)
			    ,(CHARM_SLEEP|IM_FIRE|IM_FROST|IM_ACID|IM_POISON|
			      IM_LIGHTNING|MINDLESS|NONLIVING)
			    ,(BREATH_SH|BREATH_SD|BREATH_CH|BREATH_CO
			      |BREATH_LD|BREATH_NE)
			    ,(BREATH_TI|BREATH_WA|BREATH_SL|BREATH_LT
			      |BREATH_DA|BREATH_PL|BREATH_GR)
       ,4500,0,100,40,43,'v',{32,20},{242,239,240,241},55,2,'˙'
#ifdef TC_COLOR
  , ANY
#endif
},

{"The Lernean Hydra"        ,(MV_ATT_NORM|THRO_DR|CARRY_GOLD|HAS_4D2|HAS_2D2|
			     HAS_1D2|THRO_CREAT)
			    ,(0x3L|FEAR|FIRE_BALL|FIRE_BOLT|
			     BREATH_FI|BREATH_G)
			    ,(CHARM_SLEEP|IM_FIRE|IM_POISON|UNIQUE|ANIMAL|
		 INTELLIGENT|MAX_HP) /* I removed the SEMI - it is
					"Intelligent"  --TY */
			    ,(PLASMA_BOLT|ST_CLOUD),(S_REPTILE)
    ,20000,20,20,140,33,'R',{100,43},{250,250,251,251},55,2,'·'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Vlad Dracula, Prince of Darkness"
		,(MV_ATT_NORM|THRO_DR|HAS_4D2|HAS_2D2|HAS_1D2|
			     CARRY_OBJ|HAS_60|HAS_90)
			    ,(0x3L|CAUSE_CRIT|MANA_DRAIN|FEAR|HOLD_PERSON|
			     BLINDNESS)
			    ,(UNDEAD|EVIL|MAX_HP|CHARM_SLEEP|INTELLIGENT|
			     IM_FROST|HURT_LIGHT|NO_INFRA|UNIQUE|GOOD|CAN_SPEAK|
			    IM_POISON),(NETHER_BALL|RAZOR|BRAIN_SMASH),(NONE8)
      ,23000,10,20,145,42,'V',{100,40},{48,216,216,198},55,4,'Ÿ'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"Great hell wyrm"          ,(HAS_1D2|HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x6L|BREATH_FI|FEAR|BLINDNESS
			      |CONFUSION)
			    ,(EVIL|DRAGON|IM_FIRE|CHARM_SLEEP|MAX_HP|GOOD)
			    ,(NONE8),(NONE8)
    ,23000,40,40,170,31,'D',{90,60},{57,57,271,277},55,2,'â'
#ifdef TC_COLOR
  , RED
#endif
},

{"Dragonic quylthulg"        ,(MV_INVIS)
		,(0x2L|S_DRAGON|BLINK|TELE),(MAX_HP|EVIL|
			     CHARM_SLEEP|FEARLESS),(NONE8),(NONE8)
    ,5500,0,20,1,31,'Q',{90,8},{0,0,0,0},55,3,'ú'
#ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Dworkin Barimen"     ,(HAS_4D2|CARRY_OBJ|HAS_1D2|MV_ATT_NORM|THRO_DR)
	,(0x4L|BLINK|TELE|TELE_TO|CAUSE_CRIT|FEAR|BLINDNESS|CONFUSION|
		MANA_DRAIN|S_DEMON)
		,(CHARM_SLEEP|MAX_HP|GOOD|IM_POISON|IM_FROST|SHAPECHANGER|
		 IM_FIRE|IM_ACID|IM_LIGHTNING|CAN_SPEAK|UNIQUE|INTELLIGENT),(FORGET|
		 RAZOR|
		 HEAL|SUMMON|BRAIN_SMASH),(LOGRUS_BALL)
     ,22000,10,25,195,42,'a',{100,48},{212,145,218,249},56,2,'¯'
#ifdef TC_COLOR
  , ANY
#endif
},

{"Uriel, Angel of Fire"     ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      HAS_4D2|HAS_2D2|HAS_1D2)
			    ,(0x2L|BLINDNESS|TELE_TO|BREATH_FI
			      |FIRE_BALL|FIRE_BOLT|MANA_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|IM_ACID|IM_LIGHTNING|
			      GOOD|INTELLIGENT|MAX_HP|CAN_SPEAK|UNIQUE|FEARLESS)
			    ,(S_ANGEL),(NONE8)
        ,25000,10,40,160,41,'A',{220,25}
		,{220,103,212,212},56,3,'Ü'
#ifdef TC_COLOR
  , WHITE
#endif
},


{"Azriel, Angel of Death"   ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      HAS_4D2|HAS_2D2|HAS_1D2)
			    ,(0x2L|BLINDNESS|TELE_TO|MANA_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|IM_ACID|IM_LIGHTNING|
			      GOOD|INTELLIGENT|MAX_HP|CAN_SPEAK|UNIQUE|FEARLESS)
			    ,(S_ANGEL|BREATH_LD|NETHER_BOLT|NETHER_BALL)
			    ,(NONE8)
        ,30000,10,40,170,42,'A',{240,25}
		,{202,260,212,212},57,3,'Ü'
#ifdef TC_COLOR
  , WHITE
#endif
},
{"Lobo"                ,(MV_ATT_NORM|HAS_2D2|CARRY_OBJ|THRO_DR
		|HAS_60|HAS_90|MULTIPLY)
		,(0xBL|ACID_BOLT|FIRE_BOLT|MANA_BOLT)
		,(EVIL|IM_POISON|CHARM_SLEEP)
		,(LIGHT_BOLT|PLASMA_BOLT)
		,(NUKE_BALL)
      ,6000,10,20,100,31,'h',{50,20},{269,263,256,263},58,4,'Ï'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Ancalagon the Black"      ,(HAS_4D2|CARRY_OBJ|HAS_90|HAS_2D2|THRO_DR|
			      HAS_60|MV_ATT_NORM)
			    ,(0x2L|BREATH_A|FEAR|BLINDNESS|CONFUSION|S_DRAGON)
			    ,(EVIL|DRAGON|IM_FIRE|IM_ACID|
			      UNIQUE|CHARM_SLEEP|MAX_HP|CAN_SPEAK|GOOD)
			    ,(NONE8),(S_ANCIENTD)
      ,30000,70,20,125,34,'D',{110,70},{273,274,275,281},58,3,'â'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Nightwalker"              ,(MV_ATT_NORM|HAS_4D2|CARRY_OBJ|THRO_DR)
			    ,(0x4L|FEAR|S_UNDEAD|BLINDNESS|MANA_BOLT)
			    ,(EVIL|UNDEAD|IM_FROST|IM_POISON|IM_LIGHTNING|GOOD
			     |IM_FIRE|CHARM_SLEEP|INTELLIGENT|NO_INFRA)
			    ,(BRAIN_SMASH|NETHER_BALL|NETHER_BOLT),(NONE8)
      ,15000,10,20,175,42,'W',{50,65},{256,256,257,257},59,4,'ë'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Raphael, the Messenger"   ,(MV_ATT_NORM|THRO_DR|PICK_UP|CARRY_OBJ|
			      HAS_4D2|HAS_2D2|HAS_1D2)
			    ,(0x2L|BLINDNESS|TELE_TO|MANA_BOLT)
			    ,(IM_POISON|IM_FIRE|IM_FROST|IM_ACID|IM_LIGHTNING|
			      GOOD|INTELLIGENT|MAX_HP|CAN_SPEAK|UNIQUE|FEARLESS)
			    ,(S_ANGEL),(NONE8)
        ,35000L,10,40,180,43,'A',{140,55}
		,{230,103,212,212},59,3,'Ü'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Brand, Mad Visionary of Amber"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
			    ,(0x2L|FEAR|BLINDNESS|S_UNDEAD|S_DEMON|S_DRAGON
		 |CONFUSION|TELE|BLINK|FIRE_BALL|FROST_BALL)
			    ,(EVIL|CHARM_SLEEP|IM_FIRE|IM_LIGHTNING|IM_FROST
		  |MAX_HP|CAN_SPEAK|UNIQUE|GOOD|IM_POISON|INTELLIGENT)
		 ,(RAZOR|WATER_BALL|ACID_BALL|TELE_AWAY|FORGET|
		   ICE_BOLT|MIND_BLAST|TRAP_CREATE|HEAL|HASTE|BRAIN_SMASH)
	 ,(DARK_STORM|LOGRUS_BALL)
     ,35100U,0,100,100,33
		 ,'a',{100,50},{230,230,23,23},60,1,'«'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Shadowlord"              ,(MV_ATT_NORM|MV_20|HAS_4D2|CARRY_OBJ|HAS_2D2|
			   HAS_60|THRO_WALL|PICK_UP|MV_INVIS|HAS_1D2)
			   ,(0x4L|HOLD_PERSON|MANA_DRAIN|BLINDNESS|
		S_UNDEAD|CONFUSION|FEAR|TELE_TO|TELE)
			   ,(UNDEAD|EVIL|IM_FROST|NO_INFRA|CHARM_SLEEP|MAX_HP|
		IM_POISON|INTELLIGENT)
		,(MIND_BLAST|NETHER_BALL|DARKNESS),(NONE8)
      ,22500,10,20,150,32,'W',{117,27},{257,257,81,214},62,3,'Ø'
#ifdef TC_COLOR
  , BLUE
#endif
},

{"He-Man"           ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_2D2|HAS_4D2
		|HAS_1D2
		|PICK_UP|THRO_CREAT|HAS_60|HAS_90)
	      ,(0x3L|TELE_TO|TELE)
	      ,(CHARM_SLEEP|MAX_HP|SPECIAL|CAN_SPEAK|UNIQUE|EVIL|GIANT|BREAK_WALL
	       |IM_FIRE|IM_FROST|IM_LIGHTNING|DESTRUCT)
	       ,(TELE_AWAY|HASTE|SUMMON|HEAL),(S_WRAITH|DARK_STORM|S_GUNDEAD
		|S_ANCIENTD)
      ,54000U,0,100,190,42,'P',{80,65},{269,260,262,272},68,3,'ë'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Great wyrm of many colors"
     ,(HAS_1D2|HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_90|HAS_60|
		  MV_ATT_NORM),(0x5L|BREATH_G|BREATH_L|BREATH_A|BREATH_FR|
			      BREATH_FI|FEAR|CONFUSION|BLINDNESS)
			    ,(IM_FROST|IM_ACID|IM_POISON|IM_LIGHTNING|
		  IM_FIRE|GOOD|EVIL|DRAGON|CHARM_SLEEP|MAX_HP)
			    ,(NONE8),(NONE8)
       ,32000,20,40,170,32,'D',{80,80},{273,273,274,280},64,2,'â'
#ifdef TC_COLOR
  , MULTI
#endif
},


{"Jabberwock"               ,(CARRY_OBJ|HAS_90|HAS_60|MV_ATT_NORM)
			    ,(0x5L)
	,(MAX_HP|GOOD),(BREATH_CH|RAZOR) /* It's no longer an animal!
					    I don't want them in zoos! TY */
			    ,(NONE8)
       ,19000,255,35,125,42,'J',{80,40},{212,212,212,212},
	 65,3,'ù'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Chaos hound"              ,(THRO_DR|MV_ATT_NORM),(0x5L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP),(BREATH_CH)
			    ,(NONE8)
      ,10000,0,30,100,32,'Z',{60,30},{39,39,39,58},65,1,'≥'
#ifdef TC_COLOR
  , ANY
#endif
},

{"Great wyrm of chaos"      ,(HAS_1D2|HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x3L|FEAR|BLINDNESS|CONFUSION|
			     S_DRAGON),(EVIL|DRAGON|CHARM_SLEEP|MAX_HP|GOOD)
			    ,(BREATH_CH|BREATH_DI),(NONE8)
       ,29000,20,40,170,32,'D',{65,70},{273,273,274,280},67,2,'â'
#ifdef TC_COLOR
  , ANY
#endif
},

{"Great wyrm of law"        ,(HAS_1D2|HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x3L|FEAR|BLINDNESS|CONFUSION|
			     S_DRAGON),(DRAGON|CHARM_SLEEP|MAX_HP|GOOD)
			    ,(BREATH_SH|BREATH_SD),(NONE8)
      ,29000,255,40,170,32,'D',{70,65},{273,273,274,280},67,2,'â'
#ifdef TC_COLOR
  , WHITE
#endif
},

{"Great wyrm of balance"    ,(HAS_1D2|HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_90|HAS_60|
			      MV_ATT_NORM),(0x3L|FEAR|BLINDNESS|S_DRAGON
			      |CONFUSION)
			    ,(DRAGON|CHARM_SLEEP|MAX_HP|GOOD)
			    ,(BREATH_SH|BREATH_SD|BREATH_CH|BREATH_DI),
			     (S_ANCIENTD)
      ,31000,255,40,170,32,'D',{70,70},{273,273,274,280},67,4,'â'
#ifdef TC_COLOR
  , LIGHTGRAY
#endif
},

{"Tselakus, the Dreadlord" ,(MV_ATT_NORM|HAS_4D2|CARRY_OBJ|HAS_2D2|
			   THRO_WALL|MV_INVIS|HAS_1D2)
			   ,(0x3L|HOLD_PERSON|BLINDNESS|CONFUSION)
			   ,(UNDEAD|EVIL|IM_FROST|NO_INFRA|CHARM_SLEEP|MAX_HP|
			    IM_POISON|GOOD|CAN_SPEAK|UNIQUE)
	       ,(NETHER_BALL),(S_GUNDEAD|DARK_STORM)
	  ,35000U,10,20,150,40,'G',{100,67},{81,81,212,212},68,2,'Ò'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Bleys, the Trickster"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x3L|FEAR|BLINDNESS|CONFUSION|TELE|BLINK|
		FIRE_BALL|FROST_BALL)
		,(EVIL|CHARM_SLEEP|IM_FIRE|IM_LIGHTNING
	  |MAX_HP|CAN_SPEAK|UNIQUE|GOOD|INTELLIGENT)
	 ,(TRAP_CREATE|MIND_BLAST|TELE_AWAY|TELE_LEV)
	 ,(S_WRAITH)
     ,32100U,0,100,100,39
		 ,'a',{100,52},{269,145,214,0},68,1,'«'
#ifdef TC_COLOR
  , BROWN
#endif
},

{"Julian, the Master of Forest Arden"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x3L|FEAR)
		,(EVIL|CHARM_SLEEP|IM_LIGHTNING|IM_POISON|IM_FROST
	  |MAX_HP|CAN_SPEAK|UNIQUE|GOOD|INTELLIGENT)
	 ,(TRAP_CREATE|TELE_AWAY|S_HOUND|S_SPIDER|DARKNESS|FORGET|WATER_BOLT)
	 ,(ARROW|S_REPTILE|S_ANT)
     ,34100U,0,100,100,40
		 ,'a',{100,55},{98,230,230,214},69,1,'«'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"The Norsa",(MV_ATT_NORM|HAS_4D2|CARRY_OBJ|
			      THRO_DR|HAS_60|HAS_90|HAS_2D2|HAS_1D2)
			    ,(0x2L|BREATH_G|BREATH_L|BREATH_A|BREATH_FR|
			      BREATH_FI|FEAR|CONFUSION|BLINDNESS)
			    ,(IM_FROST|IM_ACID|IM_POISON|IM_LIGHTNING|
			      IM_FIRE|EVIL|DRAGON|CHARM_SLEEP|MAX_HP|
			      UNIQUE|CAN_SPEAK|SPECIAL),(NONE8),(S_ANCIENTD)
       ,45000U,70,20,125,41,'H',{100,100},{78,78,281,217},70,4,'ö'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Black reaver"             ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_1D2|HAS_2D2)
			    ,(0x3L|CONFUSION|BLINDNESS|HOLD_PERSON|
			      CAUSE_CRIT|MANA_DRAIN|TELE_TO|S_UNDEAD)
			    ,(UNDEAD|IM_POISON|IM_FROST|EVIL|MAX_HP|GOOD|
		  CHARM_SLEEP|NO_INFRA|INTELLIGENT|BREAK_WALL|FEARLESS)
			    ,(BRAIN_SMASH|RAZOR|NETHER_BALL),(MANA_STORM)
      ,23000,50,20,170,32,'L',{60,60},{230,230,81,81},71,3,'ü'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},


{"Caine, the Conspirator"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x3L|FEAR|CONFUSION|TELE|TELE_TO|ACID_BOLT)
		,(EVIL|CHARM_SLEEP|IM_LIGHTNING|IM_POISON|IM_FIRE
	  |MAX_HP|UNIQUE|CAN_SPEAK|GOOD|INTELLIGENT)
	 ,(TRAP_CREATE|TELE_AWAY|HEAL|FORGET|WATER_BOLT|MIND_BLAST|RAZOR
	 |S_ANGEL)
	 ,(ARROW|S_WRAITH)
     ,34100U,0,100,100,40
		 ,'a',{100,56},{171,230,230,150},71,1,'«'
#ifdef TC_COLOR
  , BLUE
#endif
},


{"Master quylthulg"         ,(MV_INVIS)
			,(0x2L|MONSTER|S_UNDEAD|S_DRAGON),(CHARM_SLEEP|
	      MAX_HP|EVIL|FEARLESS),(SUMMON),(S_GUNDEAD|S_ANCIENTD)
       ,12000,0,20,1,31,'Q',{100,20},{0,0,0,0},71,3,'ú'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},


{"Greater dragonic quylthulg" ,(MV_INVIS)
		,(0x2L|BLINK|TELE_TO),(EVIL|MAX_HP|
			     CHARM_SLEEP|FEARLESS) ,(NONE8),(S_ANCIENTD)
    ,10500,0,20,1,32,'Q',{100,14},{0,0,0,0},71,3,'ú'
#ifdef TC_COLOR
  , MULTI
#endif
},

{"Greater rotting quylthulg",(MV_INVIS)
		,(0x2L|BLINK|TELE_TO),(EVIL|UNDEAD|NO_INFRA|
			     CHARM_SLEEP|MAX_HP|IM_FROST|FEARLESS),(NONE8),(S_GUNDEAD)
    ,10500,0,20,1,32,'Q',{100,14},{0,0,0,0},71,3,'ú'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Vecna, the Emperor Lich"  ,(MV_ATT_NORM|THRO_DR|CARRY_OBJ|HAS_4D2|HAS_2D2)
			    ,(0x2L|FEAR|CONFUSION|BLINDNESS|HOLD_PERSON|
			      CAUSE_CRIT|MANA_BOLT|TELE_TO|BLINK|S_UNDEAD)
			    ,(UNDEAD|IM_POISON|IM_FROST|EVIL|MAX_HP|
			      CHARM_SLEEP|NO_INFRA|CAN_SPEAK|UNIQUE|GOOD|INTELLIGENT)
			   ,(NETHER_BALL|TRAP_CREATE|RAZOR|SUMMON|BRAIN_SMASH)
			    ,(MANA_STORM)
      ,30000,50,20,85,42,'L',{90,50},{181,201,214,181},72,2,'ü'
#ifdef TC_COLOR
  , YELLOW
#endif
},

{"Omarax the Eye Tyrant"    ,(MV_ATT_NORM)
			    ,(0x2L|FIRE_BOLT|FROST_BOLT|ACID_BOLT|
			      MANA_DRAIN|BLINDNESS|CONFUSION|FEAR|SLOW)
			    ,(ANIMAL|EVIL|CHARM_SLEEP|MAX_HP|
			      IM_POISON|UNIQUE|CAN_SPEAK|INTELLIGENT)
			    ,(FORGET|MIND_BLAST|DARKNESS),(DARK_STORM)
       ,16000,10,30,80,41,'e',{80,80},{223,224,225,226},73,4,'È'
#ifdef TC_COLOR
  , MAGENTA
#endif
},

{"Gerard, the Strongman of Amber"  ,(MV_ATT_NORM|HAS_2D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x4L|FEAR|TELE|TELE_TO)
		,(EVIL|CHARM_SLEEP|IM_LIGHTNING|IM_FIRE|IM_FROST
	  |MAX_HP|UNIQUE|SPECIAL|INTELLIGENT|BREAK_WALL|DESTRUCT|CAN_SPEAK)
	 ,(HEAL)
	 ,(S_WRAITH)
     ,35100U,0,100,100,41
		 ,'a',{100,66},{262,256,256,0},74,1,'˛'
#ifdef TC_COLOR
  , RED
#endif
},

{"Atlach-Nacha"   ,(MV_ATT_NORM|CARRY_OBJ|HAS_4D2)
		,(0x3L|FEAR|BLINDNESS|CONFUSION|HOLD_PERSON|BREATH_G)
			    ,(ANIMAL|EVIL|UNIQUE|HURT_LIGHT|MAX_HP|CAN_SPEAK|
			      CHARM_SLEEP|GOOD|INTELLIGENT|IM_POISON)
			    ,(HEAL|S_SPIDER|DARKNESS)
			    ,(DARK_STORM|BREATH_DA)
     ,35000U,80,8,160,31,'S',{130,100},{84,84,167,167},75,1,'˜'
#ifdef TC_COLOR
  , DARKGRAY
#endif
},

{"Hound of Tindalos"
	       ,(MV_ATT_NORM|THRO_DR),(0x5L|BREATH_FI|BREATH_FR|BREATH_G|
					    BREATH_A|BREATH_L)
			    ,(ANIMAL|GROUP|CHARM_SLEEP|IM_FIRE|
			      IM_FROST|IM_LIGHTNING|IM_POISON|IM_ACID)
			    ,(BREATH_CH|BREATH_SH|BREATH_SD|BREATH_CO
			      |BREATH_DI|BREATH_LD|BREATH_NE)
			    ,(BREATH_WA|BREATH_GR|BREATH_SL|
			      BREATH_PL|BREATH_TI|BREATH_LT|
			      BREATH_DA)
    ,10000,0,30,100,32,'Z',{60,30},{39,39,39,58},75,2,'≥'
#ifdef TC_COLOR
  , ANY
#endif
},


{"Eric, the Usurper"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x3L|FEAR|CONFUSION|TELE|TELE_TO|MONSTER|MANA_DRAIN
		|CAUSE_CRIT|ACID_BOLT|MANA_BOLT|HOLD_PERSON|FIRE_BALL)
		,(EVIL|CHARM_SLEEP|IM_LIGHTNING|IM_POISON|IM_FIRE|IM_ACID
	  |MAX_HP|UNIQUE|GOOD|INTELLIGENT|CAN_SPEAK)
	 ,(TRAP_CREATE|TELE_AWAY|HEAL|BRAIN_SMASH
	 |FORGET|WATER_BOLT|RAZOR|NETHER_BOLT|DARKNESS
	 |S_ANGEL)
	 ,(S_WRAITH)
     ,35100U,0,100,100,41
		 ,'a',{100,58},{143,230,230,23},76,1,'«'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},

{"Cyberdemon",(MV_ATT_NORM|MV_20|PICK_UP|THRO_DR|HAS_2D2
		|HAS_90|CARRY_OBJ)
		,(0x4L)
		,(DEMON|EVIL|IM_POISON|MAX_HP|GOOD|IM_FIRE|GOOD)
		,(NONE8),(ROCKET)
     ,35000U,90,80,90,32,'&',{100,70},{269,269,269,0},75,5,'Ñ'
	     #ifdef TC_COLOR
  , BROWN /* "You can hear heavy steps..." ;-) TY */
#endif
},


{"Corwin, the Lord of Avalon"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x3L|FEAR|CONFUSION|TELE|TELE_TO|MONSTER|MANA_DRAIN
		|CAUSE_CRIT|ACID_BOLT|MANA_BOLT|HOLD_PERSON|FIRE_BALL
		|FROST_BALL)
		,(EVIL|CHARM_SLEEP|IM_LIGHTNING|IM_POISON|IM_FROST|IM_ACID
	  |MAX_HP|UNIQUE|GOOD|INTELLIGENT|CAN_SPEAK)
	 ,(TRAP_CREATE|TELE_AWAY|HEAL|BRAIN_SMASH|WATER_BALL|NETHER_BALL
	 |FORGET|WATER_BOLT|RAZOR|NETHER_BOLT|DARKNESS|SUMMON|PLASMA_BOLT
	 |S_ANGEL)
	 ,(S_WRAITH|S_ANCIENTD)
     ,36100U,0,100,100,42
		 ,'a',{100,59},{230,230,23,23},77,1,'«'
#ifdef TC_COLOR
      , DARKGRAY
#endif
},

{"Fiona, the Sorceress of Amber"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x2L|FEAR|CONFUSION|TELE|TELE_TO|MONSTER|MANA_DRAIN
		|CAUSE_CRIT|ACID_BOLT|MANA_BOLT|HOLD_PERSON|FIRE_BALL
		|FROST_BALL)
		,(EVIL|CHARM_SLEEP|IM_LIGHTNING|IM_POISON|IM_FROST|IM_ACID
	  |MAX_HP|UNIQUE|GOOD|INTELLIGENT|CAN_SPEAK)
	 ,(TRAP_CREATE|TELE_AWAY|HEAL|BRAIN_SMASH|WATER_BALL|NETHER_BALL
	 |FORGET|WATER_BOLT|RAZOR|NETHER_BOLT|DARKNESS|SUMMON|TELE_LEV|
	  WATER_BALL|S_ANGEL|ST_CLOUD|PLASMA_BOLT)
	 ,(ARROW|S_WRAITH|S_ANCIENTD|S_GUNDEAD)
     ,37100U,0,100,100,42
		 ,'a',{100,60},{230,230,23,23},78,1,'Ú'
#ifdef TC_COLOR
  , YELLOW
#endif
},



{"Klingsor, the Evil Master of Magic"
	  ,(MV_ATT_NORM|MV_INVIS|THRO_DR|HAS_4D2|HAS_1D2|
			     CARRY_OBJ),(0x2L|TELE_TO|CAUSE_CRIT|FIRE_BALL|
			     HOLD_PERSON),(EVIL|MAX_HP|UNIQUE|GOOD|CHARM_SLEEP
			     |INTELLIGENT|IM_FROST|IM_FIRE|IM_LIGHTNING|CAN_SPEAK),
			     (TRAP_CREATE|WATER_BALL|PLASMA_BOLT|NETHER_BALL)
		,(MANA_STORM|DARK_STORM|S_GUNDEAD|LOGRUS_BALL)
    ,38000U,10,60,100,42,'p',{70,100},{230,230,214,214},78,3,'¯'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"The Emperor Quylthulg"    ,(MV_INVIS|CARRY_OBJ|HAS_4D2)
		,(0x2L),(EVIL|MAX_HP|UNIQUE|CHARM_SLEEP
		|FEARLESS|CAN_SPEAK|GOOD)
			    ,(BRAIN_SMASH),(S_GUNDEAD|S_ANCIENTD)
     ,20000,0,30,1,43,'Q',{50,100},{0,0,0,0},78,3,'ú'
#ifdef TC_COLOR
  , ANY
#endif
},

{"Qlzqqlzuup, the Lord of Flesh", (MV_INVIS|CARRY_OBJ|HAS_4D2)
			    ,(0x1L|S_UNDEAD|S_DEMON|S_DRAGON|MONSTER)
		,(EVIL|UNIQUE|GOOD|MAX_HP|CHARM_SLEEP|FEARLESS|CAN_SPEAK)
			    ,(SUMMON|S_ANGEL|S_SPIDER|S_HOUND)
	,(S_REPTILE|S_ANT|S_GUNDEAD|S_ANCIENTD|S_UNIQUE)
      ,20000,0,30,1,43,'Q',{50,100},{0,0,0,0},78,3,'ú'
#ifdef TC_COLOR
  , LIGHTMAGENTA
#endif
},

{"Benedict, the Ideal Warrior"  ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x3L|FEAR|CONFUSION|TELE|TELE_TO|MANA_DRAIN)
		,(EVIL|CHARM_SLEEP|IM_LIGHTNING|IM_POISON|IM_FROST|IM_ACID
	  |MAX_HP|UNIQUE|GOOD|INTELLIGENT|CAN_SPEAK)
	 ,(TRAP_CREATE|TELE_AWAY|HEAL|FORGET
	 |RAZOR|SUMMON|TELE_LEV|S_ANGEL)
	 ,(S_WRAITH|S_ANCIENTD)
     ,38100U,0,100,100,43
		 ,'a',{100,70},{256,262,262,214},79,1,'«'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},



{"Murazor, the Witch-King of Angmar",
		       (HAS_4D2|HAS_2D2|CARRY_OBJ|HAS_1D2|THRO_DR|MV_ATT_NORM)
			    ,(0x2L|MANA_BOLT|CAUSE_CRIT|HOLD_PERSON|
			      FEAR|BLINDNESS)
			      ,(EVIL|UNDEAD|CHARM_SLEEP|IM_FROST|MAX_HP
				|IM_POISON|HURT_LIGHT|UNIQUE|GOOD|NO_INFRA|CAN_SPEAK
				|INTELLIGENT)
			      ,(NETHER_BALL|BRAIN_SMASH|TELE_AWAY|SUMMON)
	  ,(S_ANCIENTD|S_GUNDEAD|LOGRUS_BALL)
     ,42000U,10,90,120,42,'W',{120,50},{212,23,199,199},80,3,'É'
#ifdef TC_COLOR
  , LIGHTBLUE
#endif
},

{"Ithaqua the Windwalker"     ,(MV_ATT_NORM|THRO_DR|MV_INVIS|CARRY_OBJ|HAS_4D2)
	       ,(0x3L|MANA_BOLT|FEAR),(EVIL|DEMON|CHARM_SLEEP|
			    MAX_HP|IM_FROST|IM_FIRE|IM_LIGHTNING|IM_ACID|CAN_SPEAK|
			    IM_POISON|GOOD|UNIQUE),(LIGHT_BOLT|LIGHT_BALL|
		MIND_BLAST|RAZOR),(LOGRUS_BALL)
      ,30000,10,40,125,52,'Y',{100,55},{269,284,269,275},82,2,'æ'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Hell hound of Julian"               ,(MV_ATT_NORM|MV_20),(0x5L|BREATH_FI),
	    (ANIMAL|EVIL|MAX_HP|IM_FIRE|GROUP|FEARLESS),(NONE8),(NONE8)
       ,600,30,25,80,33,'C',{30,16},{107,107,107,0},83,4,'Ë'
#ifdef TC_COLOR
  , RED
#endif
},

{"Skeletor",
			     (MV_ATT_NORM|HAS_4D2|CARRY_OBJ|HAS_60|HAS_90|
			      THRO_DR|HAS_2D2|HAS_1D2)
			    ,(0x1L|TELE_TO|MANA_BOLT|FEAR|SLOW)
			    ,(EVIL|UNIQUE|UNDEAD|CHARM_SLEEP|IM_FROST|NO_INFRA
			    |CAN_SPEAK
			      |IM_POISON|IM_FIRE|MAX_HP|INTELLIGENT|SPECIAL)
			    ,(WATER_BALL|RAZOR|BRAIN_SMASH|ICE_BOLT|
		  NETHER_BALL),(S_GUNDEAD|LOGRUS_BALL)
        ,45000U,80,20,120,52,'s',{150,45},{246,172,172,0},
				    84,2,'µ'
#ifdef TC_COLOR
  , CYAN
#endif
},

{"Godzilla"        ,(MV_ATT_NORM|HAS_4D2|HAS_2D2|THRO_DR|CARRY_OBJ)
		,(0x2L|BREATH_G|BREATH_A)
		,(EVIL|CHARM_SLEEP|IM_FIRE|IM_POISON|IM_ACID|MAX_HP|
		UNIQUE|GOOD)
	,(BREATH_DI),(BREATH_PL|NUKE_BREATH)
     ,35500U,20,50,185,44,'R',{85,95},{212,212,78,214},84,2,'à'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Surtur the Fire Demon",
			     (MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
			    ,(0x4L|FEAR|BLINDNESS|S_DEMON|
			      BREATH_FI|CONFUSION)
			    ,(EVIL|DEMON|CHARM_SLEEP|IM_FIRE|MAX_HP|
			      UNIQUE|GOOD|CAN_SPEAK|IM_FROST),(NONE8)
			      ,(BREATH_PL|S_GUNDEAD)
     ,37500U,80,20,125,43,'&',{85,95},{104,104,78,214},85,2,'Ñ'
#ifdef TC_COLOR
  , RED
#endif
},

{"Star-spawn of Cthulhu",(MV_ATT_NORM|MV_20|PICK_UP|THRO_DR|HAS_1D2|HAS_2D2
		|HAS_90|CARRY_OBJ)
		,(0x3L|FEAR|CONFUSION|S_DEMON|S_UNDEAD|MANA_DRAIN|BREATH_A|
		    BREATH_FI)
		,(DEMON|EVIL|IM_POISON|MAX_HP|GOOD|IM_FROST|IM_ACID|GOOD)
		,(SUMMON|BRAIN_SMASH|BREATH_LD),(NONE8)
     ,40000U,90,80,90,42,'&',{100,70},{78,57,160,160},85,2,'’'
	     #ifdef TC_COLOR
  , LIGHTGREEN
#endif
},

{"Kashchei the Immortal",(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
			    ,(0x3L|FEAR|BLINDNESS|S_DEMON
			      |TELE|FIRE_BALL|MANA_BOLT)
			    ,(EVIL|CHARM_SLEEP|IM_FIRE|IM_LIGHTNING|IM_FROST
			      |MAX_HP|CAN_SPEAK|UNIQUE|SPECIAL|IM_POISON|INTELLIGENT
			      |NO_INFRA|UNDEAD)
			     ,(BRAIN_SMASH|RAZOR|SUMMON),(MANA_STORM|
			      S_GUNDEAD)
         ,45000U,0,100,100,42
		 ,'L',{120,50},{230,230,23,23},90,3,'ü'
#ifdef TC_COLOR
  , LIGHTCYAN
#endif
},

{"Great wyrm of power",(MV_ATT_NORM|PICK_UP|THRO_DR
			|HAS_4D2|HAS_2D2|HAS_1D2|HAS_60|HAS_90|CARRY_OBJ)
		,(0x3L|BREATH_G|BREATH_A|FEAR|S_DRAGON)
			    ,(IM_POISON|IM_FIRE|IM_FROST|DRAGON
			    |IM_ACID|IM_LIGHTNING|GOOD|EVIL
			     |MAX_HP|CHARM_SLEEP)
			    ,(BREATH_CH|BREATH_SH|BREATH_SD
			    |BREATH_DI|BREATH_LD|BREATH_NE)
			    ,(BREATH_WA|BREATH_SL|BREATH_TI|BREATH_GR
		|BREATH_PL|S_ANCIENTD|NUKE_BREATH)
         ,51000U,10,80,110,42,'D',{110,110},{
	     274,275,275,281},90,2,'â'
#ifdef TC_COLOR  /* This is where PDSMs come from;) - TY */
  , YELLOW
#endif
},

{"Cerberus, Guardian of Hades",(MV_ATT_NORM|HAS_4D2|CARRY_OBJ|THRO_DR)
			    ,(0x3L|BREATH_FI),(ANIMAL|EVIL|UNIQUE|IM_FIRE|
			     MAX_HP|GOOD|CHARM_SLEEP)
			    ,(BREATH_LD|S_HOUND),(DARK_STORM)
      ,40000U,10,50,160,41,'C',{100,100},{220,220,220,220},94,1,'Ë'
#ifdef TC_COLOR
  , LIGHTRED
#endif
},


{"Great Cthulhu",
			     (MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
		,(0x3L|FEAR|BLINDNESS|S_DEMON|BREATH_A|
		  BREATH_FI|CONFUSION|MANA_DRAIN)
		,(EVIL|DEMON|CHARM_SLEEP|IM_FIRE|INTELLIGENT|
		  IM_LIGHTNING|MAX_HP|CAN_SPEAK|UNIQUE|SPECIAL|IM_ACID
		  |IM_POISON)
	,(DARKNESS|BRAIN_SMASH|FORGET|BREATH_LD|BREATH_CO|BREATH_DI|BREATH_CH)
	,(S_GUNDEAD|BREATH_PL|BREATH_DA|NUKE_BREATH)
     ,52500U,0,100,140,43,'&',{100,80},{220,119,78,214},95,1,'’'
#ifdef TC_COLOR
  , GREEN
#endif
},

{"Oberon, the Lord of Amber",     (MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
			      CARRY_OBJ|HAS_60|HAS_90|THRO_DR)
			    ,(0x2L|FEAR|BLINDNESS|S_DEMON|TELE_TO
			     |CONFUSION|TELE|MANA_BOLT|FIRE_BALL)
			    ,(CHARM_SLEEP|IM_FIRE|IM_LIGHTNING|IM_FROST
			      |MAX_HP|CAN_SPEAK|UNIQUE|SPECIAL|IM_POISON|QUESTOR|
			      INTELLIGENT)
		,(BRAIN_SMASH|NETHER_BALL|ICE_BOLT|PLASMA_BOLT|S_ANGEL|
		  WATER_BALL|TELE_LEV|TELE_AWAY|FORGET|DARKNESS|SUMMON|
		  RAZOR),(MANA_STORM|S_ANCIENTD|S_GUNDEAD|LOGRUS_BALL)
     ,55000U,0,100,160,44
	 ,'a',{99,111},{284,269,269,214},99,1,'«'
#ifdef TC_COLOR
  , MULTI
#endif
},

{"The Serpent of Chaos",(MV_ATT_NORM|HAS_4D2|HAS_2D2|HAS_1D2|
		  CARRY_OBJ|HAS_60|HAS_90|THRO_DR|WINNER|THRO_CREAT)
		,(0x3L|MANA_BOLT)
			    ,(EVIL|
			    CHARM_SLEEP|IM_FIRE|IM_LIGHTNING|IM_FROST|
			     IM_POISON|IM_ACID|MAX_HP|CAN_SPEAK|UNIQUE|SPECIAL|
			     BREAK_WALL|DESTRUCT|INTELLIGENT)
	,(SUMMON|BREATH_CH|BRAIN_SMASH|NETHER_BALL|HASTE)
	,(MANA_STORM|S_ANCIENTD|S_GUNDEAD|S_UNIQUE|ROCKET|LOGRUS_BALL|
	NUKE_BREATH)
    ,60000U,0,100,152,60
    ,'R',{180,99},{262,262,245,214},100,10,'“'
#ifdef TC_COLOR
  , ANY
#endif
},

{"                                                                                                    " /* Players Ghost! */    ,(NONE8)
			    ,(NONE8)
			    ,(EVIL|CHARM_SLEEP|UNDEAD|UNIQUE|GOOD
			     |IM_POISON|IM_FROST|NO_INFRA),(NONE8)
			    ,(NONE8)
			    ,0,0,100,0,11
		,'@',{0,0},{0,0,0,0},100,1,'‡'
#ifdef TC_COLOR
  , WHITE
#endif
}
};

/* Speed adj. checkpoint  (pointer for finetune/adjustments) */

/* ERROR: attack #35 is no longer used */
struct m_attack_type monster_attacks[N_MONS_ATTS] = {
/* 0 */ {0, 0, 0, 0},   {1, 1, 1, 2},   {1, 1, 1, 3},   {1, 1, 1, 4},
	{1, 1, 1, 5},   {1, 1, 1, 6},   {1, 1, 1, 7},   {1, 1, 1, 8},
	{1, 1, 1, 9},   {1, 1, 1, 10},  {1, 1, 1, 12},  {1, 1, 2, 2},
    {1, 1, 2, 3},   {1, 1, 2, 4},   {1, 1, 2, 5},   {1, 1, 2, 6},
	{1, 1, 2, 8},   {1, 1, 3, 4},   {1, 1, 3, 5},   {1, 1, 3, 6},
/* 20 */{1, 1, 3, 8},   {1, 1, 4, 3},   {1, 1, 4, 6},   {1, 1, 5, 5},
	{1, 2, 1, 1},   {1, 2, 1, 2},   {1, 2, 1, 3},   {1, 2, 1, 4},
	{1, 2, 1, 5},   {1, 2, 1, 6},   {1, 2, 1, 7},   {1, 2, 1, 8},
	{1, 2, 1, 10},  {1, 2, 2, 3},   {1, 2, 2, 4},   {1, 2, 2, 5},
	{1, 2, 2, 6},   {1, 2, 2, 8},   {1, 2, 2, 10},  {1, 2, 2, 12},
/* 40 */{1, 2, 2, 14},  {1, 2, 3, 4},   {1, 2, 3, 12},  {1, 2, 4, 4},
	{1, 2, 4, 5},   {1, 2, 4, 6},   {1, 2, 4, 8},   {1, 2, 5, 4},
	{1, 2, 5, 8},   {1, 3, 1, 1},   {1, 3, 1, 2},   {1, 3, 1, 3},
	{1, 3, 1, 4},   {1, 3, 1, 5},   {1, 3, 1, 8},   {1, 3, 1, 9},
	{1, 3, 1, 10},  {1, 3, 1, 12},  {1, 3, 3, 3},   {1, 4, 1, 2},
/* 60 */{1, 4, 1, 3},   {1, 4, 1, 4},   {1, 4, 2, 4},   {1, 5, 1, 2},
	{1, 5, 1, 3},   {1, 5, 1, 4},   {1, 5, 1, 5},   {1, 10, 5, 6},
	{1, 12, 1, 1},  {1, 12, 1, 2},  {1, 13, 1, 1},  {1, 13, 1, 3},
	{1, 14, 0, 0},  {1, 16, 1, 4},  {1, 16, 1, 6},  {1, 16, 1, 8},
	{1, 16, 1, 10}, {1, 16, 2, 8},  {1, 17, 8, 12}, {1, 18, 0, 0},
/* 80 */{2, 1, 3, 4},   {2, 1, 4, 6},   {2, 2, 1, 4},   {2, 2, 2, 4},
	{2, 2, 4, 4},   {2, 4, 1, 4},   {2, 4, 1, 7},   {2, 5, 1, 5},
	{2, 7, 1, 6},   {3, 1, 1, 4},   {3, 5, 1, 8},   {3, 13, 1, 4},
	{3, 7, 0, 0},   {4, 1, 1, 1},   {4, 1, 1, 4},   {4, 2, 1, 2},
	{4, 2, 1, 6},   {4, 5, 0, 0},   {4, 7, 0, 0},   {4, 10, 0, 0},
/*100 */{4, 13, 1, 6},  {5, 1, 2, 6},   {5, 1, 3, 7},   {5, 1, 4, 6},
	{5, 1, 8, 12},  {5, 2, 1, 3},   {5, 2, 3, 6},   {5, 2, 3, 12},
	{5, 5, 4, 4},   {5, 9, 3, 7},   {5, 9, 4, 5},   {5, 12, 1, 6},
	{6, 2, 1, 3},   {6, 2, 2, 8},   {6, 2, 4, 4},   {6, 5, 1, 10},
	{6, 5, 2, 3},   {6, 8, 1, 5},   {6, 9, 2, 6},   {6, 9, 3, 6},
/*120 */{7, 1, 3, 6},   {7, 2, 1, 3},   {7, 2, 1, 6},   {7, 2, 3, 6},
	{7, 2, 3, 10},  {7, 5, 1, 6},   {7, 5, 2, 3},   {7, 5, 2, 6},
	{7, 5, 4, 4},   {7, 12, 1, 4},  {8, 1, 3, 8},   {8, 2, 1, 3},
	{8, 2, 2, 6},   {8, 2, 3, 8},   {8, 2, 5, 5},   {8, 5, 5, 4},
	{9, 5, 1, 2},   {9, 5, 2, 5},   {9, 5, 2, 6},   {9, 8, 2, 4},
/*140 */{9, 12, 1, 3},  {10, 2, 1, 6},  {10, 4, 1, 1},  {10, 7, 2, 6},
	{10, 9, 1, 2},  {11, 1, 1, 2},  {11, 7, 0, 0},  {11, 13, 2, 4},
	{12, 5, 0, 0},  {13, 5, 0, 0},  {13, 19, 0, 0}, {14, 1, 1, 3},
	{14, 1, 3, 4},  {14, 2, 1, 3},  {14, 2, 1, 4},  {14, 2, 1, 5},
	{14, 2, 1, 6},  {14, 2, 1, 10}, {14, 2, 2, 4},  {14, 2, 2, 5},
/*160 */{14, 2, 2, 6},  {14, 2, 3, 4},  {14, 2, 3, 9},  {14, 2, 4, 4},
	{14, 4, 1, 2},  {14, 4, 1, 4},  {14, 4, 1, 8},  {14, 4, 2, 5},
	{14, 5, 1, 2},  {14, 5, 1, 3},  {14, 5, 2, 4},  {14, 5, 2, 6},
	{14, 5, 3, 5},  {14, 12, 1, 2}, {14, 12, 1, 4}, {14, 13, 2, 4},
	{15, 2, 1, 6},  {15, 2, 3, 6},  {15, 5, 1, 8},  {15, 5, 2, 8},
/*180 */{15, 5, 2, 10}, {15, 5, 2, 12}, {15, 12, 1, 3}, {16, 13, 1, 2},
	{17, 3, 1, 10}, {18, 5, 0, 0},  {19, 5, 5, 8},  {19, 5, 12, 8},
	{19, 5, 14, 8}, {19, 5, 15, 8}, {19, 5, 18, 8}, {19, 5, 20, 8},
	{19, 5, 22, 8}, {19, 5, 26, 8}, {19, 5, 30, 8}, {19, 5, 32, 8},
	{19, 5, 34, 8}, {19, 5, 36, 8}, {19, 5, 38, 8}, {19, 5, 42, 8},
/*200 */{19, 5, 44, 8}, {19, 5, 46, 8}, {19, 5, 52, 8}, {20, 10, 0, 0},
	{21, 1, 0, 0},  {21, 5, 0, 0},  {21, 5, 1, 6},  {21, 7, 0, 0},
    {21, 12, 1, 4}, {22, 5, 2, 3},  {22, 12, 0, 0}, {22, 15, 1, 1},
/*212 */{1, 1, 10, 10}, {23, 5, 1, 3},  {24,  5, 0, 0}, {8,   1, 3, 8},
	{3,  1, 6, 6},  {4,  7, 4, 4},  {1,   1, 8, 6}, {1,   5, 2, 5},
	{5,  1, 9,12},

/*221 */{ 4, 7, 2, 4},  {10, 7, 2, 4},  {11, 7, 2, 4},  {19, 7, 20, 8},
	{17, 7, 2, 6},  {24, 7, 2, 6},  /* Beholder */

/*227 */{ 1,20, 4, 6},  { 1,20, 2, 6},  /* Butts */

/*229 */{ 9, 9, 3, 8},  /* Spit */

/*230 */{21, 1, 6, 8},

/*231 */{12, 1, 4, 4},  {13, 1, 4, 5}, /* Master Rogue */

/*233 */{ 1,21, 4, 4},
/*234 */{ 3, 2, 2, 2},
/*235 */{ 1, 1, 6, 6},
/*236 */{19, 2,52, 8}, /* Bite for xp drain */
/*237 */{18, 3, 5, 5}, /* Claw to drain Wisdom */
/*238 */{14, 3, 3, 3}, /* Algroth poisonous claw attack */
/*239 */{5, 22, 3, 3}, /* Fire */ /* vortices */
/*240 */{6, 22, 3, 3}, /* Acid */
/*241 */{7, 22, 3, 3}, /* Cold */
/*242 */{8, 22, 5, 5}, /* Lightning */
/*243 */{5, 22, 8, 8}, /* Plasma/fire */ /* vortices */
/*244 */{1, 22, 5, 5}, /* Hit */ /* vortices */
/*245 */{25, 1,10,12}, /* Morgoths dex drain attack */
/*246 */{19, 7,30,20}, /* Eye druj drain */
/*247 */{11, 2, 4, 4}, /* Skull */
/*248 */{17, 2, 4, 4}, /* druj */
/*249 */{18, 2, 4, 4}, /* attacks */
/*250 */{14, 2, 8, 6}, /* Lernean */
/*251 */{5,  2, 12, 6}, /* Hydra */
/*252 */{18, 3, 1, 10}, /* Another drain wisdom attack */
/*253 */{11, 4, 2, 6}, /* Carrion crawler */
/*254 */{16, 4, 8, 8}, /* Nightcrawler */
/*255 */{9,  2,10,10}, /* Nightcrawler */
/*256 */{21, 1,10,10}, /* Nightwalker */
/*257 */{21, 1, 7, 7}, /* Nightwalker */
/*258 */{12, 5, 5, 5}, /* Harowen  */
/*259 */{13, 5, 5, 5}, /*   the Black Hand */
/*260 */{10, 1,10, 5}, /* Harowen  */
/*261 */{14, 1, 8, 5}, /* Harowen  */
/*262 */{ 1, 1,20,10}, /* Morgoth attacks */
/*263 */{ 1, 6,20, 2}, /* Mystic kick */
/*264 */{ 1, 7 ,0 ,0}, /* This one be funny */
/*265 */{11, 1,15, 1}, /* Mystic paralysis */
/*266 */{ 1, 6,10, 2}, /* Mystic kick */
/*267 */{11,11, 6, 6}, /* Medusa paralyse */
/*268 */{ 1,19, 0, 0}, /* Nermal insults */
/*269 */{ 3, 1,12, 12}, /* Greater titan */
/*270 */{17, 23, 5, 8}, /* Barney int drain */
/*271 */{ 1, 3, 3, 12}, 
/*272 */{18, 23, 5, 8}, /* Barney wis drain */
/*273 */{ 1, 3, 5, 12},
/*274 */{ 1, 3, 6, 12},
/*275 */{ 1, 3, 8, 12},
/*276 */{ 1, 2, 3, 14}, /* New claws and bites for those wimpy dragons! */
/*277 */{ 1, 2, 4, 14},
/*278 */{ 12, 21, 0, 0}, /* Barney steal money ("charges") */
/*279 */{ 1, 23, 0, 0}, /* a BIZARRE new attack type */
/*280 */{ 1, 2, 8, 14},
/*281 */{ 1, 2, 10,14},
/*282 */{ 1,20, 12,13},
/*283 */{ 1,23,  0, 0},
/*284 */{ 8, 1, 12,12},
};


monster_type m_list[MAX_MALLOC];
int16 m_level[MAX_MONS_LEVEL+1];

/* Blank monster values */
monster_type blank_monster = {0,0,0,0,0,0,0,FALSE,0,FALSE};
int16 mfptr;                    /* Cur free monster ptr */
int16 mon_tot_mult;             /* # of repro's of creature     */
