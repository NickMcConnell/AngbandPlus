/* File: raceflag.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Kenneth 'Bessarion' Boyd
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "bitflag.h"

/*
 * Moderate reorganization.  The current layout is:
 *  RF_ : the master list of defines for race bit flag indexes
 *  RF1_, RF2_, etc.: bitflags for flags[0], flags[1], etc.
 *  RFA_ : offset spell/ranged attack indexes  
 *
 *  The RFA series of macros includes "inverse macros" to facilitate reliable static array initialization.
 *  The undefined inverse is MACRO_NULL.
 */


/*
 * New monster race flag indexes.  Array bounds for following are in defines.h.
 */
#define RF_UNIQUE			 0	/* Unique Monster */
#define RF_QUESTOR			 1	/* Quest Monster */
#define RF_MALE				 2	/* Male gender */
#define RF_FEMALE			 3	/* Female gender */
#define RF_CHAR_CLEAR		 4	/* Absorbs symbol */
#define RF_CHAR_MULTI		 5	/* Changes symbol */
#define RF_ATTR_CLEAR		 6	/* Absorbs color */
#define RF_ATTR_MULTI		 7	/* Changes color */
#define RF_FORCE_DEPTH		 8	/* Start at "correct" depth */
#define RF_FORCE_MAXHP		 9	/* Start with max hitpoints */
#define RF_FORCE_SLEEP		10	/* Start out sleeping */
#define RF_FORCE_EXTRA		11	/* Start out something */
#define RF_FRIEND			12	/* Arrive with a friend */
#define RF_FRIENDS			13	/* Arrive with some friends */
#define RF_ESCORT			14	/* Arrive with an escort */
#define RF_ESCORTS			15	/* Arrive with some escorts */
#define RF_NEVER_BLOW		16	/* Never make physical blow */
#define RF_NEVER_MOVE		17	/* Never make physical move */
#define RF_RAND_25			18	/* Moves randomly (25%) */
#define RF_RAND_50			19	/* Moves randomly (50%) */
#define RF_ONLY_GOLD		20	/* Drop only gold */
#define RF_ONLY_ITEM		21	/* Drop only items */
#define RF_DROP_60			22	/* Drop an item/gold (60%) */
#define RF_DROP_90			23	/* Drop an item/gold (90%) */
#define RF_DROP_1D2			24	/* Drop 1d2 items/gold */
#define RF_DROP_2D2			25	/* Drop 2d2 items/gold */
#define RF_DROP_3D2			26	/* Drop 3d2 items/gold */
#define RF_DROP_4D2			27	/* Drop 4d2 items/gold */
#define RF_DROP_GOOD		28	/* Drop good items */
#define RF_DROP_GREAT		29	/* Drop great items */
#define RF_DROP_USEFUL		30	/* Drop "useful" items */
#define RF_DROP_CHOSEN		31	/* Drop "chosen" items */

#define RF_STUPID			(32+0)	/* Monster is stupid */
#define RF_SMART			(32+1)	/* Monster is smart */
#define RF_INVISIBLE		(32+4)	/* Monster avoids vision */
#define RF_COLD_BLOOD		(32+5)	/* Monster avoids infra */
#define RF_EMPTY_MIND		(32+6)	/* Monster avoids telepathy */
#define RF_WEIRD_MIND		(32+7)	/* Monster avoids telepathy? */
#define RF_MULTIPLY			(32+8)	/* Monster reproduces */
#define RF_REGENERATE		(32+9)	/* Monster regenerates */
#define RF_POWERFUL			(32+12)	/* Monster has strong breath */
#define RF_OPEN_DOOR		(32+16)	/* Monster can open doors */
#define RF_BASH_DOOR		(32+17)	/* Monster can bash doors */
#define RF_PASS_WALL		(32+18)	/* Monster can pass walls */
#define RF_KILL_WALL		(32+19)	/* Monster can destroy walls */
#define RF_MOVE_BODY		(32+20)	/* Monster can move monsters */
#define RF_KILL_BODY		(32+21)	/* Monster can kill monsters */
#define RF_TAKE_ITEM		(32+22)	/* Monster can pick up items */
#define RF_KILL_ITEM		(32+23)	/* Monster can crush items */

#define RF_ORC				(2*32+0)	/* Orc */
#define RF_TROLL			(2*32+1)	/* Troll */
#define RF_GIANT			(2*32+2)	/* Giant */
#define RF_DRAGON			(2*32+3)	/* Dragon */
#define RF_DEMON			(2*32+4)	/* Demon */
#define RF_UNDEAD			(2*32+5)	/* Undead */
#define RF_EVIL				(2*32+6)	/* Evil */
#define RF_ANIMAL			(2*32+7)	/* Animal */
#define RF_HURT_LITE		(2*32+12)	/* Hurt by lite */
#define RF_HURT_ROCK		(2*32+13)	/* Hurt by rock remover */
#define RF_HURT_FIRE		(2*32+14)	/* Hurt badly by fire */
#define RF_HURT_COLD		(2*32+15)	/* Hurt badly by cold */
#define RF_IM_ACID			(2*32+16)	/* Resist acid a lot */
#define RF_IM_ELEC			(2*32+17)	/* Resist elec a lot */
#define RF_IM_FIRE			(2*32+18)	/* Resist fire a lot */
#define RF_IM_COLD			(2*32+19)	/* Resist cold a lot */
#define RF_IM_POIS			(2*32+20)	/* Resist poison a lot */
#define RF_RES_NETH			(2*32+22)	/* Resist nether a lot */
#define RF_IM_WATER			(2*32+23)	/* Immune to water */
#define RF_RES_PLAS			(2*32+24)	/* Resist plasma */
#define RF_RES_NEXUS		(2*32+25)	/* Resist nexus */
#define RF_RES_DISE			(2*32+26)	/* Resist disenchantment */
#define RF_NO_FEAR			(2*32+28)	/* Cannot be scared */
#define RF_NO_STUN			(2*32+29)	/* Cannot be stunned */
#define RF_NO_CONF			(2*32+30)	/* Cannot be confused */
#define RF_NO_SLEEP			(2*32+31)	/* Cannot be slept */

/* start attacks */
#define RF_SHRIEK			(3*32+0)	/* Shriek for help */
#define RF_ARROW_1			(3*32+4)	/* Fire an arrow (light) */
#define RF_ARROW_2			(3*32+5)	/* Fire an arrow (heavy) */
#define RF_ARROW_3			(3*32+6) 	/* Fire missiles (light) */
#define RF_ARROW_4			(3*32+7)	/* Fire missiles (heavy) */
#define RF_BR_ACID			(3*32+8)	/* Breathe Acid */
#define RF_BR_ELEC     		(3*32+9)	/* Breathe Elec */
#define RF_BR_FIRE         	(3*32+10) 	/* Breathe Fire */
#define RF_BR_COLD         	(3*32+11) 	/* Breathe Cold */
#define RF_BR_POIS         	(3*32+12) 	/* Breathe Poison */
#define RF_BR_NETH         	(3*32+13) 	/* Breathe Nether */
#define RF_BR_LITE			(3*32+14) 	/* Breathe Lite */
#define RF_BR_DARK			(3*32+15) 	/* Breathe Dark */
#define RF_BR_CONF			(3*32+16) 	/* Breathe Confusion */
#define RF_BR_SOUN			(3*32+17) 	/* Breathe Sound */
#define RF_BR_CHAO         	(3*32+18) 	/* Breathe Chaos */
#define RF_BR_DISE         	(3*32+19) 	/* Breathe Disenchant */
#define RF_BR_NEXU			(3*32+20) 	/* Breathe Nexus */
#define RF_BR_TIME			(3*32+21) 	/* Breathe Time */
#define RF_BR_INER			(3*32+22) 	/* Breathe Inertia */
#define RF_BR_GRAV			(3*32+23) 	/* Breathe Gravity */
#define RF_BR_SHAR			(3*32+24) 	/* Breathe Shards */
#define RF_BR_PLAS			(3*32+25) 	/* Breathe Plasma */
#define RF_BR_WALL			(3*32+26) 	/* Breathe Force */
#define RF_BOULDER         	(3*32+31)	/* Throw a boulder */

#define RF_BA_ACID			(4*32+0)	/* Acid Ball */
#define RF_BA_ELEC			(4*32+1)	/* Elec Ball */
#define RF_BA_FIRE			(4*32+2)	/* Fire Ball */
#define RF_BA_COLD			(4*32+3)	/* Cold Ball */
#define RF_BA_POIS			(4*32+4)	/* Poison Ball */
#define RF_BA_NETH			(4*32+5)	/* Nether Ball */
#define RF_BA_WATE			(4*32+6)	/* Water Ball */
#define RF_BA_MANA			(4*32+7)	/* Mana Storm */
#define RF_BA_DARK			(4*32+8)	/* Darkness Storm */
#define RF_DRAIN_MANA		(4*32+9)	/* Drain Mana */
#define RF_MIND_BLAST		(4*32+10)	/* Blast Mind */
#define RF_BRAIN_SMASH		(4*32+11)	/* Smash Brain */
#define RF_CAUSE_1			(4*32+12)	/* Cause Light Wound */
#define RF_CAUSE_2			(4*32+13)	/* Cause Serious Wound */
#define RF_CAUSE_3			(4*32+14)	/* Cause Critical Wound */
#define RF_CAUSE_4			(4*32+15)	/* Cause Mortal Wound */
#define RF_BO_ACID			(4*32+16)	/* Acid Bolt */
#define RF_BO_ELEC			(4*32+17)	/* Elec Bolt */
#define RF_BO_FIRE			(4*32+18)	/* Fire Bolt */
#define RF_BO_COLD			(4*32+19)	/* Cold Bolt */
#define RF_BO_NETH			(4*32+21)	/* Nether Bolt */
#define RF_BO_WATE			(4*32+22)	/* Water Bolt */
#define RF_BO_MANA			(4*32+23)	/* Mana Bolt */
#define RF_BO_PLAS			(4*32+24)	/* Plasma Bolt */
#define RF_BO_ICEE			(4*32+25)	/* Ice Bolt */
#define RF_MISSILE			(4*32+26)	/* Magic Missile */
#define RF_SCARE			(4*32+27)	/* Frighten Player */
#define RF_BLIND			(4*32+28)	/* Blind Player */
#define RF_CONF				(4*32+29)	/* Confuse Player */
#define RF_SLOW				(4*32+30)	/* Slow Player */
#define RF_HOLD				(4*32+31)	/* Paralyze Player */

#define RF_HASTE			(5*32+0) /* Speed self */
#define RF_HEAL				(5*32+2) /* Heal self */
#define RF_BLINK			(5*32+4) /* Teleport Short */
#define RF_TPORT			(5*32+5) /* Teleport Long */
#define RF_TELE_TO			(5*32+8) /* Move player to monster */
#define RF_TELE_AWAY		(5*32+9) /* Move player far away */
#define RF_TELE_LEVEL		(5*32+10) /* Move player vertically */
#define RF_DARKNESS			(5*32+12) /* Create Darkness */
#define RF_TRAPS			(5*32+13) /* Create Traps */
#define RF_FORGET			(5*32+14) /* Cause amnesia */
#define RF_S_KIN			(5*32+16) /* Summon Kin */
#define RF_S_HI_DEMON		(5*32+17) /* Summon Greater Demons */
#define RF_S_MONSTER		(5*32+18) /* Summon Monster */
#define RF_S_MONSTERS		(5*32+19) /* Summon Monsters */
#define RF_S_ANIMAL			(5*32+20) /* Summon Animals */
#define RF_S_SPIDER			(5*32+21) /* Summon Spiders */
#define RF_S_HOUND			(5*32+22) /* Summon Hounds */
#define RF_S_HYDRA			(5*32+23) /* Summon Hydras */
#define RF_S_ANGEL			(5*32+24) /* Summon Angel */
#define RF_S_DEMON			(5*32+25) /* Summon Demon */
#define RF_S_UNDEAD			(5*32+26) /* Summon Undead */
#define RF_S_DRAGON			(5*32+27) /* Summon Dragon */
#define RF_S_HI_UNDEAD		(5*32+28) /* Summon Greater Undead */
#define RF_S_HI_DRAGON		(5*32+29) /* Summon Ancient Dragon */
#define RF_S_WRAITH			(5*32+30) /* Summon Unique Wraith */
#define RF_S_UNIQUE			(5*32+31) /* Summon Unique Monster */


/*
 * New monster race bit flags
 */
#define RF0_UNIQUE			FLAG_FROM_INDEX(RF_UNIQUE)
#define RF0_QUESTOR			FLAG_FROM_INDEX(RF_QUESTOR)
#define RF0_MALE			FLAG_FROM_INDEX(RF_MALE)
#define RF0_FEMALE			FLAG_FROM_INDEX(RF_FEMALE)
#define RF0_CHAR_CLEAR		FLAG_FROM_INDEX(RF_CHAR_CLEAR)
#define RF0_CHAR_MULTI		FLAG_FROM_INDEX(RF_CHAR_MULTI)
#define RF0_ATTR_CLEAR		FLAG_FROM_INDEX(RF_ATTR_CLEAR)
#define RF0_ATTR_MULTI		FLAG_FROM_INDEX(RF_ATTR_MULTI)
#define RF0_FORCE_DEPTH		FLAG_FROM_INDEX(RF_FORCE_DEPTH)
#define RF0_FORCE_MAXHP		FLAG_FROM_INDEX(RF_FORCE_MAXHP)
#define RF0_FORCE_SLEEP		FLAG_FROM_INDEX(RF_FORCE_SLEEP)
#define RF0_FORCE_EXTRA		FLAG_FROM_INDEX(RF_FORCE_EXTRA)
#define RF0_FRIEND			FLAG_FROM_INDEX(RF_FRIEND)
#define RF0_FRIENDS			FLAG_FROM_INDEX(RF_FRIENDS)
#define RF0_ESCORT			FLAG_FROM_INDEX(RF_ESCORT)
#define RF0_ESCORTS			FLAG_FROM_INDEX(RF_ESCORTS)
#define RF0_NEVER_BLOW		FLAG_FROM_INDEX(RF_NEVER_BLOW)
#define RF0_NEVER_MOVE		FLAG_FROM_INDEX(RF_NEVER_MOVE)
#define RF0_RAND_25			FLAG_FROM_INDEX(RF_RAND_25)
#define RF0_RAND_50			FLAG_FROM_INDEX(RF_RAND_50)
#define RF0_ONLY_GOLD		FLAG_FROM_INDEX(RF_ONLY_GOLD)
#define RF0_ONLY_ITEM		FLAG_FROM_INDEX(RF_ONLY_ITEM)
#define RF0_DROP_60			FLAG_FROM_INDEX(RF_DROP_60)
#define RF0_DROP_90			FLAG_FROM_INDEX(RF_DROP_90)
#define RF0_DROP_1D2		FLAG_FROM_INDEX(RF_DROP_1D2)
#define RF0_DROP_2D2		FLAG_FROM_INDEX(RF_DROP_2D2)
#define RF0_DROP_3D2		FLAG_FROM_INDEX(RF_DROP_3D2)
#define RF0_DROP_4D2		FLAG_FROM_INDEX(RF_DROP_4D2)
#define RF0_DROP_GOOD		FLAG_FROM_INDEX(RF_DROP_GOOD)
#define RF0_DROP_GREAT		FLAG_FROM_INDEX(RF_DROP_GREAT)
#define RF0_DROP_USEFUL		FLAG_FROM_INDEX(RF_DROP_USEFUL)
#define RF0_DROP_CHOSEN		FLAG_FROM_INDEX(RF_DROP_CHOSEN)

#define RF1_STUPID			FLAG_FROM_INDEX(RF_STUPID)
#define RF1_SMART			FLAG_FROM_INDEX(RF_SMART)
#define RF1_INVISIBLE		FLAG_FROM_INDEX(RF_INVISIBLE)
#define RF1_COLD_BLOOD		FLAG_FROM_INDEX(RF_COLD_BLOOD)
#define RF1_EMPTY_MIND		FLAG_FROM_INDEX(RF_EMPTY_MIND)
#define RF1_WEIRD_MIND		FLAG_FROM_INDEX(RF_WEIRD_MIND)
#define RF1_MULTIPLY		FLAG_FROM_INDEX(RF_MULTIPLY)
#define RF1_REGENERATE		FLAG_FROM_INDEX(RF_REGENERATE)
#define RF1_POWERFUL		FLAG_FROM_INDEX(RF_POWERFUL)
#define RF1_OPEN_DOOR		FLAG_FROM_INDEX(RF_OPEN_DOOR)
#define RF1_BASH_DOOR		FLAG_FROM_INDEX(RF_BASH_DOOR)
#define RF1_PASS_WALL		FLAG_FROM_INDEX(RF_PASS_WALL)
#define RF1_KILL_WALL		FLAG_FROM_INDEX(RF_KILL_WALL)
#define RF1_MOVE_BODY		FLAG_FROM_INDEX(RF_MOVE_BODY)
#define RF1_KILL_BODY		FLAG_FROM_INDEX(RF_KILL_BODY)
#define RF1_TAKE_ITEM		FLAG_FROM_INDEX(RF_TAKE_ITEM)
#define RF1_KILL_ITEM		FLAG_FROM_INDEX(RF_KILL_ITEM)


#define RF2_ORC			FLAG_FROM_INDEX(RF_ORC)
#define RF2_TROLL		FLAG_FROM_INDEX(RF_TROLL)
#define RF2_GIANT		FLAG_FROM_INDEX(RF_GIANT)
#define RF2_DRAGON		FLAG_FROM_INDEX(RF_DRAGON)
#define RF2_DEMON		FLAG_FROM_INDEX(RF_DEMON)
#define RF2_UNDEAD		FLAG_FROM_INDEX(RF_UNDEAD)
#define RF2_EVIL		FLAG_FROM_INDEX(RF_EVIL)
#define RF2_ANIMAL		FLAG_FROM_INDEX(RF_ANIMAL)
#define RF2_HURT_LITE	FLAG_FROM_INDEX(RF_HURT_LITE)
#define RF2_HURT_ROCK	FLAG_FROM_INDEX(RF_HURT_ROCK)
#define RF2_HURT_FIRE	FLAG_FROM_INDEX(RF_HURT_FIRE)
#define RF2_HURT_COLD	FLAG_FROM_INDEX(RF_HURT_COLD)
#define RF2_IM_ACID		FLAG_FROM_INDEX(RF_IM_ACID)
#define RF2_IM_ELEC		FLAG_FROM_INDEX(RF_IM_ELEC)
#define RF2_IM_FIRE		FLAG_FROM_INDEX(RF_IM_FIRE)
#define RF2_IM_COLD		FLAG_FROM_INDEX(RF_IM_COLD)
#define RF2_IM_POIS		FLAG_FROM_INDEX(RF_IM_POIS)
#define RF2_RES_NETH	FLAG_FROM_INDEX(RF_RES_NETH)
#define RF2_IM_WATER	FLAG_FROM_INDEX(RF_IM_WATER)
#define RF2_RES_PLAS	FLAG_FROM_INDEX(RF_RES_PLAS)
#define RF2_RES_NEXUS	FLAG_FROM_INDEX(RF_RES_NEXUS)
#define RF2_RES_DISE	FLAG_FROM_INDEX(RF_RES_DISE)
#define RF2_NO_FEAR		FLAG_FROM_INDEX(RF_NO_FEAR)
#define RF2_NO_STUN		FLAG_FROM_INDEX(RF_NO_STUN)
#define RF2_NO_CONF		FLAG_FROM_INDEX(RF_NO_CONF)
#define RF2_NO_SLEEP	FLAG_FROM_INDEX(RF_NO_SLEEP)

#define RF3_SHRIEK		FLAG_FROM_INDEX(RF_SHRIEK)
#define RF3_ARROW_1		FLAG_FROM_INDEX(RF_ARROW_1)
#define RF3_ARROW_2		FLAG_FROM_INDEX(RF_ARROW_2)
#define RF3_ARROW_3		FLAG_FROM_INDEX(RF_ARROW_3)
#define RF3_ARROW_4		FLAG_FROM_INDEX(RF_ARROW_4)
#define RF3_BR_ACID		FLAG_FROM_INDEX(RF_BR_ACID)
#define RF3_BR_ELEC		FLAG_FROM_INDEX(RF_BR_ELEC)
#define RF3_BR_FIRE		FLAG_FROM_INDEX(RF_BR_FIRE)
#define RF3_BR_COLD		FLAG_FROM_INDEX(RF_BR_COLD)
#define RF3_BR_POIS		FLAG_FROM_INDEX(RF_BR_POIS)
#define RF3_BR_NETH		FLAG_FROM_INDEX(RF_BR_NETH)
#define RF3_BR_LITE		FLAG_FROM_INDEX(RF_BR_LITE)
#define RF3_BR_DARK		FLAG_FROM_INDEX(RF_BR_DARK)
#define RF3_BR_CONF		FLAG_FROM_INDEX(RF_BR_CONF)
#define RF3_BR_SOUN		FLAG_FROM_INDEX(RF_BR_SOUN)
#define RF3_BR_CHAO		FLAG_FROM_INDEX(RF_BR_CHAO)
#define RF3_BR_DISE		FLAG_FROM_INDEX(RF_BR_DISE)
#define RF3_BR_NEXU		FLAG_FROM_INDEX(RF_BR_NEXU)
#define RF3_BR_TIME		FLAG_FROM_INDEX(RF_BR_TIME)
#define RF3_BR_INER		FLAG_FROM_INDEX(RF_BR_INER)
#define RF3_BR_GRAV		FLAG_FROM_INDEX(RF_BR_GRAV)
#define RF3_BR_SHAR		FLAG_FROM_INDEX(RF_BR_SHAR)
#define RF3_BR_PLAS		FLAG_FROM_INDEX(RF_BR_PLAS)
#define RF3_BR_WALL		FLAG_FROM_INDEX(RF_BR_WALL)
#define RF3_BOULDER		FLAG_FROM_INDEX(RF_BOULDER)

#define RF4_BA_ACID		FLAG_FROM_INDEX(RF_BA_ACID)
#define RF4_BA_ELEC		FLAG_FROM_INDEX(RF_BA_ELEC)
#define RF4_BA_FIRE		FLAG_FROM_INDEX(RF_BA_FIRE)
#define RF4_BA_COLD		FLAG_FROM_INDEX(RF_BA_COLD)
#define RF4_BA_POIS		FLAG_FROM_INDEX(RF_BA_POIS)
#define RF4_BA_NETH		FLAG_FROM_INDEX(RF_BA_NETH)
#define RF4_BA_WATE		FLAG_FROM_INDEX(RF_BA_WATE)
#define RF4_BA_MANA		FLAG_FROM_INDEX(RF_BA_MANA)
#define RF4_BA_DARK		FLAG_FROM_INDEX(RF_BA_DARK)
#define RF4_DRAIN_MANA	FLAG_FROM_INDEX(RF_DRAIN_MANA)
#define RF4_MIND_BLAST	FLAG_FROM_INDEX(RF_MIND_BLAST)
#define RF4_BRAIN_SMASH	FLAG_FROM_INDEX(RF_BRAIN_SMASH)
#define RF4_CAUSE_1		FLAG_FROM_INDEX(RF_CAUSE_1)
#define RF4_CAUSE_2		FLAG_FROM_INDEX(RF_CAUSE_2)
#define RF4_CAUSE_3		FLAG_FROM_INDEX(RF_CAUSE_3)
#define RF4_CAUSE_4		FLAG_FROM_INDEX(RF_CAUSE_4)
#define RF4_BO_ACID		FLAG_FROM_INDEX(RF_BO_ACID)
#define RF4_BO_ELEC		FLAG_FROM_INDEX(RF_BO_ELEC)
#define RF4_BO_FIRE		FLAG_FROM_INDEX(RF_BO_FIRE)
#define RF4_BO_COLD		FLAG_FROM_INDEX(RF_BO_COLD)
#define RF4_BO_NETH		FLAG_FROM_INDEX(RF_BO_NETH)
#define RF4_BO_WATE		FLAG_FROM_INDEX(RF_BO_WATE)
#define RF4_BO_MANA		FLAG_FROM_INDEX(RF_BO_MANA)
#define RF4_BO_PLAS		FLAG_FROM_INDEX(RF_BO_PLAS)
#define RF4_BO_ICEE		FLAG_FROM_INDEX(RF_BO_ICEE)
#define RF4_MISSILE		FLAG_FROM_INDEX(RF_MISSILE)
#define RF4_SCARE		FLAG_FROM_INDEX(RF_SCARE)
#define RF4_BLIND		FLAG_FROM_INDEX(RF_BLIND)
#define RF4_CONF		FLAG_FROM_INDEX(RF_CONF)
#define RF4_SLOW		FLAG_FROM_INDEX(RF_SLOW)
#define RF4_HOLD		FLAG_FROM_INDEX(RF_HOLD)

#define RF5_HASTE		FLAG_FROM_INDEX(RF_HASTE)
#define RF5_HEAL		FLAG_FROM_INDEX(RF_HEAL)
#define RF5_BLINK		FLAG_FROM_INDEX(RF_BLINK)
#define RF5_TPORT		FLAG_FROM_INDEX(RF_TPORT)
#define RF5_TELE_TO		FLAG_FROM_INDEX(RF_TELE_TO)
#define RF5_TELE_AWAY	FLAG_FROM_INDEX(RF_TELE_AWAY)
#define RF5_TELE_LEVEL	FLAG_FROM_INDEX(RF_TELE_LEVEL)
#define RF5_DARKNESS	FLAG_FROM_INDEX(RF_DARKNESS)
#define RF5_TRAPS		FLAG_FROM_INDEX(RF_TRAPS)
#define RF5_FORGET		FLAG_FROM_INDEX(RF_FORGET)
#define RF5_S_KIN		FLAG_FROM_INDEX(RF_S_KIN)
#define RF5_S_HI_DEMON	FLAG_FROM_INDEX(RF_S_HI_DEMON)
#define RF5_S_MONSTER	FLAG_FROM_INDEX(RF_S_MONSTER)
#define RF5_S_MONSTERS	FLAG_FROM_INDEX(RF_S_MONSTERS)
#define RF5_S_ANIMAL	FLAG_FROM_INDEX(RF_S_ANIMAL)
#define RF5_S_SPIDER	FLAG_FROM_INDEX(RF_S_SPIDER)
#define RF5_S_HOUND		FLAG_FROM_INDEX(RF_S_HOUND)
#define RF5_S_HYDRA		FLAG_FROM_INDEX(RF_S_HYDRA)
#define RF5_S_ANGEL		FLAG_FROM_INDEX(RF_S_ANGEL)
#define RF5_S_DEMON		FLAG_FROM_INDEX(RF_S_DEMON)
#define RF5_S_UNDEAD	FLAG_FROM_INDEX(RF_S_UNDEAD)
#define RF5_S_DRAGON	FLAG_FROM_INDEX(RF_S_DRAGON)
#define RF5_S_HI_UNDEAD	FLAG_FROM_INDEX(RF_S_HI_UNDEAD)
#define RF5_S_HI_DRAGON	FLAG_FROM_INDEX(RF_S_HI_DRAGON)
#define RF5_S_WRAITH	FLAG_FROM_INDEX(RF_S_WRAITH)
#define RF5_S_UNIQUE	FLAG_FROM_INDEX(RF_S_UNIQUE)


/*
 * Some flags are obvious
 */
#define RF0_OBVIOUS_MASK \
	(RF0_UNIQUE | RF0_QUESTOR | RF0_MALE | RF0_FEMALE | \
	 RF0_FRIEND | RF0_FRIENDS | RF0_ESCORT | RF0_ESCORTS)

/*
 * Zaiband: All breath weapons are obvious.  Newbies should have 
 * fair warning of instadeath monsters by l)ooking.
 *
 * (Breath weapon monsters have, ahem, weird-looking "steam" when exhaling.)
 *
 * This still doesn't handle "read scroll of Light, Drolem wakes up and 
 * gets double move, Drolem breathes gas, you die."
 */
#define RF3_OBVIOUS_MASK \
	(RF3_BR_ACID | RF3_BR_ELEC | RF3_BR_FIRE | RF3_BR_COLD | RF3_BR_POIS | \
	 RF3_BR_NETH | RF3_BR_LITE | RF3_BR_DARK | RF3_BR_CONF | RF3_BR_SOUN | \
	 RF3_BR_CHAO | RF3_BR_DISE | RF3_BR_NEXU | RF3_BR_TIME | RF3_BR_INER | \
	 RF3_BR_GRAV | RF3_BR_SHAR | RF3_BR_PLAS | RF3_BR_WALL)


/*
 * "race" flags
 */
#define RF2_RACE_MASK \
	(RF2_ORC | RF2_TROLL | RF2_GIANT | RF2_DRAGON | \
	 RF2_DEMON | RF2_UNDEAD | RF2_EVIL | RF2_ANIMAL)


/*
 * Hack -- Bit masks to control what spells are considered
 */

/*
 * Choose "intelligent" spells when desperate
 */

#define RF3_INT_MASK \
	(0L)

#define RF4_INT_MASK \
	(RF4_HOLD | RF4_SLOW | RF4_CONF | RF4_BLIND | RF4_SCARE)

#define RF5_INT_MASK \
	(RF5_BLINK |  RF5_TPORT | RF5_TELE_LEVEL | RF5_TELE_AWAY | \
	 RF5_HEAL | RF5_HASTE | RF5_TRAPS | \
	 RF5_S_ANIMAL | RF5_S_KIN | RF5_S_MONSTER | RF5_S_MONSTERS | \
	 RF5_S_SPIDER | RF5_S_HOUND | RF5_S_HYDRA | \
	 RF5_S_ANGEL | RF5_S_DRAGON | RF5_S_UNDEAD | RF5_S_DEMON | \
	 RF5_S_HI_DRAGON | RF5_S_HI_UNDEAD | RF5_S_HI_DEMON | \
	 RF5_S_WRAITH | RF5_S_UNIQUE)


/*
 * "Bolt" spells that may hurt fellow monsters
 */
#define RF3_BOLT_MASK \
	(RF3_ARROW_1 | RF3_ARROW_2 | RF3_ARROW_3 | RF3_ARROW_4 | \
	 RF3_BOULDER)

#define RF4_BOLT_MASK \
	(RF4_BO_ACID | RF4_BO_ELEC | RF4_BO_FIRE | RF4_BO_COLD | \
	 RF4_BO_NETH | RF4_BO_WATE | RF4_BO_MANA | \
	 RF4_BO_PLAS | RF4_BO_ICEE | RF4_MISSILE)

#define RF5_BOLT_MASK \
	(0L)

/*
 * Spells that allow the caster to escape
 */
#define RF3_ESCAPE_MASK \
	(0L)

#define RF4_ESCAPE_MASK \
	(0L)

#define RF5_ESCAPE_MASK \
	(RF5_BLINK | RF5_TPORT | RF5_TELE_AWAY | RF5_TELE_LEVEL)


/*
 * Spells that hurt the player directly
 */
#define RF3_ATTACK_MASK \
	(RF3_ARROW_1 | RF3_ARROW_2 | RF3_ARROW_3 | RF3_ARROW_4 | RF3_BOULDER | \
	 RF3_BR_ACID | RF3_BR_ELEC | RF3_BR_FIRE | RF3_BR_COLD | RF3_BR_POIS | \
	 RF3_BR_NETH | RF3_BR_LITE | RF3_BR_DARK | RF3_BR_CONF | RF3_BR_SOUN | \
	 RF3_BR_CHAO | RF3_BR_DISE | RF3_BR_NEXU | RF3_BR_TIME | RF3_BR_INER | \
	 RF3_BR_GRAV | RF3_BR_SHAR | RF3_BR_PLAS | RF3_BR_WALL)

#define RF4_ATTACK_MASK \
	(RF4_BA_ACID | RF4_BA_ELEC | RF4_BA_FIRE | RF4_BA_COLD | RF4_BA_POIS | \
	 RF4_BA_NETH | RF4_BA_WATE | RF4_BA_MANA | RF4_BA_DARK | \
	 RF4_MIND_BLAST | RF4_BRAIN_SMASH | RF4_CAUSE_1 | RF4_CAUSE_2 | \
	 RF4_CAUSE_3 | RF4_CAUSE_4 | RF4_BO_ACID | RF4_BO_ELEC | RF4_BO_FIRE | \
	 RF4_BO_COLD | RF4_BO_NETH | RF4_BO_WATE | RF4_BO_MANA | \
	 RF4_BO_PLAS | RF4_BO_ICEE | RF4_MISSILE)

#define RF5_ATTACK_MASK \
	(0L)


/*
 * Summoning spells
 */
#define RF3_SUMMON_MASK \
	(0L)

#define RF4_SUMMON_MASK \
	(0L)

#define RF5_SUMMON_MASK \
	(RF5_S_KIN | RF5_S_MONSTER | RF5_S_MONSTERS | RF5_S_ANIMAL | \
	 RF5_S_SPIDER | RF5_S_HOUND | RF5_S_HYDRA | RF5_S_ANGEL | \
	 RF5_S_DEMON | RF5_S_UNDEAD | RF5_S_DRAGON | RF5_S_HI_UNDEAD | \
	 RF5_S_HI_DEMON | RF5_S_HI_DRAGON | RF5_S_WRAITH | RF5_S_UNIQUE)


/*
 * Spells that improve the caster's tactical position
 */
#define RF3_TACTIC_MASK \
	(0L)

#define RF4_TACTIC_MASK \
	(0L)

#define RF5_TACTIC_MASK \
	(RF5_BLINK)


/*
 * Annoying spells
 */
#define RF3_ANNOY_MASK \
	(RF3_SHRIEK)

#define RF4_ANNOY_MASK \
	(RF4_DRAIN_MANA | RF4_MIND_BLAST | RF4_BRAIN_SMASH | RF4_SCARE | \
	 RF4_BLIND | RF4_CONF | RF4_SLOW | RF4_HOLD)

#define RF5_ANNOY_MASK \
	(RF5_TELE_TO | RF5_DARKNESS | RF5_TRAPS | RF5_FORGET)


/*
 * Spells that increase the caster's relative speed
 */
#define RF3_HASTE_MASK \
	(0L)

#define RF4_HASTE_MASK \
	(RF4_SLOW | RF4_HOLD)

#define RF5_HASTE_MASK \
	(RF5_HASTE)


/*
 * Healing spells
 */
#define RF3_HEAL_MASK \
	(0L)

#define RF4_HEAL_MASK \
	(0L)

#define RF5_HEAL_MASK \
	(RF5_HEAL)


/*
 * Innate spell-like effects
 */
#define RF3_INNATE_MASK \
	(RF3_SHRIEK | RF3_ARROW_1 | RF3_ARROW_2 | RF3_ARROW_3 | RF3_ARROW_4 | \
	 RF3_BR_ACID | RF3_BR_ELEC | RF3_BR_FIRE | RF3_BR_COLD | RF3_BR_POIS | \
	 RF3_BR_NETH | RF3_BR_LITE | RF3_BR_DARK | RF3_BR_CONF | RF3_BR_SOUN | \
	 RF3_BR_CHAO | RF3_BR_DISE | RF3_BR_NEXU | RF3_BR_TIME | RF3_BR_INER | \
	 RF3_BR_GRAV | RF3_BR_SHAR | RF3_BR_PLAS | RF3_BR_WALL | RF3_BR_MANA | \
	 RF3_BOULDER)

#define RF4_INNATE_MASK \
	(0L)

#define RF5_INNATE_MASK \
	(0L)

/*
 * Redundant define, master in define.h
 */
#define RACE_FLAG_ATTACK_START 3

/*
 * Offset attack flag indexes
 */

#define RFA_SHRIEK		(RF_SHRIEK-32*RACE_FLAG_ATTACK_START)
#define RFA_0			SHRIEK
#define RFA_1			MACRO_NULL
#define RFA_2			MACRO_NULL
#define RFA_3			MACRO_NULL
#define RFA_ARROW_1		(RF_ARROW_1-32*RACE_FLAG_ATTACK_START)
#define RFA_4			ARROW_1
#define RFA_ARROW_2		(RF_ARROW_2-32*RACE_FLAG_ATTACK_START)
#define RFA_5			ARROW_2
#define RFA_ARROW_3		(RF_ARROW_3-32*RACE_FLAG_ATTACK_START)
#define RFA_6			ARROW_3
#define RFA_ARROW_4		(RF_ARROW_4-32*RACE_FLAG_ATTACK_START)
#define RFA_7			ARROW_4
#define RFA_BR_ACID		(RF_BR_ACID-32*RACE_FLAG_ATTACK_START)
#define RFA_8			BR_ACID
#define RFA_BR_ELEC		(RF_BR_ELEC-32*RACE_FLAG_ATTACK_START)
#define RFA_9			BR_ELEC
#define RFA_BR_FIRE		(RF_BR_FIRE-32*RACE_FLAG_ATTACK_START)
#define RFA_10			BR_FIRE
#define RFA_BR_COLD		(RF_BR_COLD-32*RACE_FLAG_ATTACK_START)
#define RFA_11			BR_COLD
#define RFA_BR_POIS		(RF_BR_POIS-32*RACE_FLAG_ATTACK_START)
#define RFA_12			BR_POIS
#define RFA_BR_NETH		(RF_BR_NETH-32*RACE_FLAG_ATTACK_START)
#define RFA_13			BR_NETH
#define RFA_BR_LITE		(RF_BR_LITE-32*RACE_FLAG_ATTACK_START)
#define RFA_14			BR_LITE
#define RFA_BR_DARK		(RF_BR_DARK-32*RACE_FLAG_ATTACK_START)
#define RFA_15			BR_DARK
#define RFA_BR_CONF		(RF_BR_CONF-32*RACE_FLAG_ATTACK_START)
#define RFA_16			BR_CONF
#define RFA_BR_SOUN		(RF_BR_SOUN-32*RACE_FLAG_ATTACK_START)
#define RFA_17			BR_SOUND
#define RFA_BR_CHAO		(RF_BR_CHAO-32*RACE_FLAG_ATTACK_START)
#define RFA_18			BR_CHAOS
#define RFA_BR_DISE		(RF_BR_DISE-32*RACE_FLAG_ATTACK_START)
#define RFA_19			BR_DISE
#define RFA_BR_NEXU		(RF_BR_NEXU-32*RACE_FLAG_ATTACK_START)
#define RFA_20			BR_NEXU
#define RFA_BR_TIME		(RF_BR_TIME-32*RACE_FLAG_ATTACK_START)
#define RFA_21			BR_TIME
#define RFA_BR_INER		(RF_BR_INER-32*RACE_FLAG_ATTACK_START)
#define RFA_22			BR_INER
#define RFA_BR_GRAV		(RF_BR_GRAV-32*RACE_FLAG_ATTACK_START)
#define RFA_23			BR_GRAV
#define RFA_BR_SHAR		(RF_BR_SHAR-32*RACE_FLAG_ATTACK_START)
#define RFA_24			BR_SHAR
#define RFA_BR_PLAS		(RF_BR_PLAS-32*RACE_FLAG_ATTACK_START)
#define RFA_25			BR_PLAS
#define RFA_BR_WALL		(RF_BR_WALL-32*RACE_FLAG_ATTACK_START)
#define RFA_26			BR_WALL
#define RFA_27			MACRO_NULL
#define RFA_28			MACRO_NULL
#define RFA_29			MACRO_NULL
#define RFA_30			MACRO_NULL
#define RFA_BOULDER		(RF_BOULDER-32*RACE_FLAG_ATTACK_START)
#define RFA_31			BOULDER

#define RFA_BA_ACID		(RF_BA_ACID-32*RACE_FLAG_ATTACK_START)
#define RFA_32			BA_ACID
#define RFA_BA_ELEC		(RF_BA_ELEC-32*RACE_FLAG_ATTACK_START)
#define RFA_33			BA_ELEC
#define RFA_BA_FIRE		(RF_BA_FIRE-32*RACE_FLAG_ATTACK_START)
#define RFA_34			BA_FIRE
#define RFA_BA_COLD		(RF_BA_COLD-32*RACE_FLAG_ATTACK_START)
#define RFA_35			BA_COLD
#define RFA_BA_POIS		(RF_BA_POIS-32*RACE_FLAG_ATTACK_START)
#define RFA_36			BA_POIS
#define RFA_BA_NETH		(RF_BA_NETH-32*RACE_FLAG_ATTACK_START)
#define RFA_37			BA_NETH
#define RFA_BA_WATE		(RF_BA_WATE-32*RACE_FLAG_ATTACK_START)
#define RFA_38			BA_WATE
#define RFA_BA_MANA		(RF_BA_MANA-32*RACE_FLAG_ATTACK_START)
#define RFA_39			BA_MANA
#define RFA_BA_DARK		(RF_BA_DARK-32*RACE_FLAG_ATTACK_START)
#define RFA_40			BA_DARK
#define RFA_DRAIN_MANA	(RF_DRAIN_MANA-32*RACE_FLAG_ATTACK_START)
#define RFA_41			DRAIN_MANA
#define RFA_MIND_BLAST	(RF_MIND_BLAST-32*RACE_FLAG_ATTACK_START)
#define RFA_42			MIND_BLAST
#define RFA_BRAIN_SMASH	(RF_BRAIN_SMASH-32*RACE_FLAG_ATTACK_START)
#define RFA_43			BRAIN_SMASH
#define RFA_CAUSE_1		(RF_CAUSE_1-32*RACE_FLAG_ATTACK_START)
#define RFA_44			CAUSE_1
#define RFA_CAUSE_2		(RF_CAUSE_2-32*RACE_FLAG_ATTACK_START)
#define RFA_45			CAUSE_2
#define RFA_CAUSE_3		(RF_CAUSE_3-32*RACE_FLAG_ATTACK_START)
#define RFA_46			CAUSE_3
#define RFA_CAUSE_4		(RF_CAUSE_4-32*RACE_FLAG_ATTACK_START)
#define RFA_47			CAUSE_4
#define RFA_BO_ACID		(RF_BO_ACID-32*RACE_FLAG_ATTACK_START)
#define RFA_48			BO_ACID
#define RFA_BO_ELEC		(RF_BO_ELEC-32*RACE_FLAG_ATTACK_START)
#define RFA_49			BO_ELEC
#define RFA_BO_FIRE		(RF_BO_FIRE-32*RACE_FLAG_ATTACK_START)
#define RFA_50			BO_FIRE
#define RFA_BO_COLD		(RF_BO_COLD-32*RACE_FLAG_ATTACK_START)
#define RFA_51			BO_COLD
#define RFA_52			MACRO_NULL
#define RFA_BO_NETH		(RF_BO_NETH-32*RACE_FLAG_ATTACK_START)
#define RFA_53			BO_NETH
#define RFA_BO_WATE		(RF_BO_WATE-32*RACE_FLAG_ATTACK_START)
#define RFA_54			BO_WATE
#define RFA_BO_MANA		(RF_BO_MANA-32*RACE_FLAG_ATTACK_START)
#define RFA_55			BO_MANA
#define RFA_BO_PLAS		(RF_BO_PLAS-32*RACE_FLAG_ATTACK_START)
#define RFA_56			BO_PLAS
#define RFA_BO_ICEE		(RF_BO_ICEE-32*RACE_FLAG_ATTACK_START)
#define RFA_57			BO_ICEE
#define RFA_MISSILE		(RF_MISSILE-32*RACE_FLAG_ATTACK_START)
#define RFA_58			MISSILE
#define RFA_SCARE		(RF_SCARE-32*RACE_FLAG_ATTACK_START)
#define RFA_59			SCARE
#define RFA_BLIND		(RF_BLIND-32*RACE_FLAG_ATTACK_START)
#define RFA_60			BLIND
#define RFA_CONF		(RF_CONF-32*RACE_FLAG_ATTACK_START)
#define RFA_61			CONF
#define RFA_SLOW		(RF_SLOW-32*RACE_FLAG_ATTACK_START)
#define RFA_62			SLOW
#define RFA_HOLD		(RF_HOLD-32*RACE_FLAG_ATTACK_START)
#define RFA_63			HOLD

#define RFA_HASTE		(RF_HASTE-32*RACE_FLAG_ATTACK_START)
#define RFA_64			HASTE
#define RFA_65			MACRO_NULL
#define RFA_HEAL		(RF_HEAL-32*RACE_FLAG_ATTACK_START)
#define RFA_66			HEAL
#define RFA_67			MACRO_NULL
#define RFA_BLINK		(RF_BLINK-32*RACE_FLAG_ATTACK_START)
#define RFA_68			BLINK
#define RFA_TPORT		(RF_TPORT-32*RACE_FLAG_ATTACK_START)
#define RFA_69			TPORT
#define RFA_70			MACRO_NULL
#define RFA_71			MACRO_NULL
#define RFA_TELE_TO		(RF_TELE_TO-32*RACE_FLAG_ATTACK_START)
#define RFA_72			TELE_TO
#define RFA_TELE_AWAY	(RF_TELE_AWAY-32*RACE_FLAG_ATTACK_START)
#define RFA_73			TELE_AWAY
#define RFA_TELE_LEVEL	(RF_TELE_LEVEL-32*RACE_FLAG_ATTACK_START)
#define RFA_74			TELE_LEVEL
#define RFA_75			MACRO_NULL
#define RFA_DARKNESS	(RF_DARKNESS-32*RACE_FLAG_ATTACK_START)
#define RFA_76			DARKNESS
#define RFA_TRAPS		(RF_TRAPS-32*RACE_FLAG_ATTACK_START)
#define RFA_77			TRAPS
#define RFA_FORGET		(RF_FORGET-32*RACE_FLAG_ATTACK_START)
#define RFA_78			FORGET
#define RFA_79			MACRO_NULL
#define RFA_S_KIN		(RF_S_KIN-32*RACE_FLAG_ATTACK_START)
#define RFA_80			S_KIN
#define RFA_S_HI_DEMON	(RF_S_HI_DEMON-32*RACE_FLAG_ATTACK_START)
#define RFA_81			S_HI_DEMON
#define RFA_S_MONSTER	(RF_S_MONSTER-32*RACE_FLAG_ATTACK_START)
#define RFA_82			S_MONSTER
#define RFA_S_MONSTERS	(RF_S_MONSTERS-32*RACE_FLAG_ATTACK_START)
#define RFA_83			S_MONSTERS
#define RFA_S_ANIMAL	(RF_S_ANIMAL-32*RACE_FLAG_ATTACK_START)
#define RFA_84			S_ANIMAL
#define RFA_S_SPIDER	(RF_S_SPIDER-32*RACE_FLAG_ATTACK_START)
#define RFA_85			S_SPIDER
#define RFA_S_HOUND		(RF_S_HOUND-32*RACE_FLAG_ATTACK_START)
#define RFA_86			S_HOUND
#define RFA_S_HYDRA		(RF_S_HYDRA-32*RACE_FLAG_ATTACK_START)
#define RFA_87			S_HYDRA
#define RFA_S_ANGEL		(RF_S_ANGEL-32*RACE_FLAG_ATTACK_START)
#define RFA_88			S_ANGEL
#define RFA_S_DEMON		(RF_S_DEMON-32*RACE_FLAG_ATTACK_START)
#define RFA_89			S_DEMON
#define RFA_S_UNDEAD	(RF_S_UNDEAD-32*RACE_FLAG_ATTACK_START)
#define RFA_90			S_UNDEAD
#define RFA_S_DRAGON	(RF_S_DRAGON-32*RACE_FLAG_ATTACK_START)
#define RFA_91			S_DRAGON
#define RFA_S_HI_UNDEAD	(RF_S_HI_UNDEAD-32*RACE_FLAG_ATTACK_START)
#define RFA_92			S_HI_UNDEAD
#define RFA_S_HI_DRAGON	(RF_S_HI_DRAGON-32*RACE_FLAG_ATTACK_START)
#define RFA_93			S_HI_DRAGON
#define RFA_S_WRAITH	(RF_S_WRAITH-32*RACE_FLAG_ATTACK_START)
#define RFA_94			S_WRAITH
#define RFA_S_UNIQUE	(RF_S_UNIQUE-32*RACE_FLAG_ATTACK_START)
#define RFA_95			S_UNIQUE


