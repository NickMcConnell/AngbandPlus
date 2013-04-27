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
 *  The RFA series of macros includes "inverse macros" to facilitate 
 *  reliable static array initialization.
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
#define RSF_SHRIEK			0	/* Shriek for help */
#define RSF_ARROW_1			4	/* Fire an arrow (light) */
#define RSF_ARROW_2			5	/* Fire an arrow (heavy) */
#define RSF_ARROW_3			6 	/* Fire missiles (light) */
#define RSF_ARROW_4			7	/* Fire missiles (heavy) */
#define RSF_BR_ACID			8	/* Breathe Acid */
#define RSF_BR_ELEC     	9	/* Breathe Elec */
#define RSF_BR_FIRE         10 	/* Breathe Fire */
#define RSF_BR_COLD         11 	/* Breathe Cold */
#define RSF_BR_POIS         12 	/* Breathe Poison */
#define RSF_BR_NETH         13 	/* Breathe Nether */
#define RSF_BR_LITE			14 	/* Breathe Lite */
#define RSF_BR_DARK			15 	/* Breathe Dark */
#define RSF_BR_CONF			16 	/* Breathe Confusion */
#define RSF_BR_SOUN			17 	/* Breathe Sound */
#define RSF_BR_CHAO         18 	/* Breathe Chaos */
#define RSF_BR_DISE         19 	/* Breathe Disenchant */
#define RSF_BR_NEXU			20 	/* Breathe Nexus */
#define RSF_BR_TIME			21 	/* Breathe Time */
#define RSF_BR_INER			22 	/* Breathe Inertia */
#define RSF_BR_GRAV			23 	/* Breathe Gravity */
#define RSF_BR_SHAR			24 	/* Breathe Shards */
#define RSF_BR_PLAS			25 	/* Breathe Plasma */
#define RSF_BR_WALL			26 	/* Breathe Force */
#define RSF_BOULDER         31	/* Throw a boulder */

#define RSF_BA_ACID			(32+0)	/* Acid Ball */
#define RSF_BA_ELEC			(32+1)	/* Elec Ball */
#define RSF_BA_FIRE			(32+2)	/* Fire Ball */
#define RSF_BA_COLD			(32+3)	/* Cold Ball */
#define RSF_BA_POIS			(32+4)	/* Poison Ball */
#define RSF_BA_NETH			(32+5)	/* Nether Ball */
#define RSF_BA_WATE			(32+6)	/* Water Ball */
#define RSF_BA_MANA			(32+7)	/* Mana Storm */
#define RSF_BA_DARK			(32+8)	/* Darkness Storm */
#define RSF_DRAIN_MANA		(32+9)	/* Drain Mana */
#define RSF_MIND_BLAST		(32+10)	/* Blast Mind */
#define RSF_BRAIN_SMASH		(32+11)	/* Smash Brain */
#define RSF_CAUSE_1			(32+12)	/* Cause Light Wound */
#define RSF_CAUSE_2			(32+13)	/* Cause Serious Wound */
#define RSF_CAUSE_3			(32+14)	/* Cause Critical Wound */
#define RSF_CAUSE_4			(32+15)	/* Cause Mortal Wound */
#define RSF_BO_ACID			(32+16)	/* Acid Bolt */
#define RSF_BO_ELEC			(32+17)	/* Elec Bolt */
#define RSF_BO_FIRE			(32+18)	/* Fire Bolt */
#define RSF_BO_COLD			(32+19)	/* Cold Bolt */
#define RSF_BO_NETH			(32+21)	/* Nether Bolt */
#define RSF_BO_WATE			(32+22)	/* Water Bolt */
#define RSF_BO_MANA			(32+23)	/* Mana Bolt */
#define RSF_BO_PLAS			(32+24)	/* Plasma Bolt */
#define RSF_BO_ICEE			(32+25)	/* Ice Bolt */
#define RSF_MISSILE			(32+26)	/* Magic Missile */
#define RSF_SCARE			(32+27)	/* Frighten Player */
#define RSF_BLIND			(32+28)	/* Blind Player */
#define RSF_CONF			(32+29)	/* Confuse Player */
#define RSF_SLOW			(32+30)	/* Slow Player */
#define RSF_HOLD			(32+31)	/* Paralyze Player */

#define RSF_HASTE			(2*32+0) /* Speed self */
#define RSF_HEAL			(2*32+2) /* Heal self */
#define RSF_BLINK			(2*32+4) /* Teleport Short */
#define RSF_TPORT			(2*32+5) /* Teleport Long */
#define RSF_TELE_TO			(2*32+8) /* Move player to monster */
#define RSF_TELE_AWAY		(2*32+9) /* Move player far away */
#define RSF_TELE_LEVEL		(2*32+10) /* Move player vertically */
#define RSF_DARKNESS		(2*32+12) /* Create Darkness */
#define RSF_TRAPS			(2*32+13) /* Create Traps */
#define RSF_FORGET			(2*32+14) /* Cause amnesia */
#define RSF_S_KIN			(2*32+16) /* Summon Kin */
#define RSF_S_HI_DEMON		(2*32+17) /* Summon Greater Demons */
#define RSF_S_MONSTER		(2*32+18) /* Summon Monster */
#define RSF_S_MONSTERS		(2*32+19) /* Summon Monsters */
#define RSF_S_ANIMAL		(2*32+20) /* Summon Animals */
#define RSF_S_SPIDER		(2*32+21) /* Summon Spiders */
#define RSF_S_HOUND			(2*32+22) /* Summon Hounds */
#define RSF_S_HYDRA			(2*32+23) /* Summon Hydras */
#define RSF_S_ANGEL			(2*32+24) /* Summon Angel */
#define RSF_S_DEMON			(2*32+25) /* Summon Demon */
#define RSF_S_UNDEAD		(2*32+26) /* Summon Undead */
#define RSF_S_DRAGON		(2*32+27) /* Summon Dragon */
#define RSF_S_HI_UNDEAD		(2*32+28) /* Summon Greater Undead */
#define RSF_S_HI_DRAGON		(2*32+29) /* Summon Ancient Dragon */
#define RSF_S_WRAITH		(2*32+30) /* Summon Unique Wraith */
#define RSF_S_UNIQUE		(2*32+31) /* Summon Unique Monster */


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

#define RSF0_SHRIEK		FLAG_FROM_INDEX(RSF_SHRIEK)
#define RSF0_ARROW_1	FLAG_FROM_INDEX(RSF_ARROW_1)
#define RSF0_ARROW_2	FLAG_FROM_INDEX(RSF_ARROW_2)
#define RSF0_ARROW_3	FLAG_FROM_INDEX(RSF_ARROW_3)
#define RSF0_ARROW_4	FLAG_FROM_INDEX(RSF_ARROW_4)
#define RSF0_BR_ACID	FLAG_FROM_INDEX(RSF_BR_ACID)
#define RSF0_BR_ELEC	FLAG_FROM_INDEX(RSF_BR_ELEC)
#define RSF0_BR_FIRE	FLAG_FROM_INDEX(RSF_BR_FIRE)
#define RSF0_BR_COLD	FLAG_FROM_INDEX(RSF_BR_COLD)
#define RSF0_BR_POIS	FLAG_FROM_INDEX(RSF_BR_POIS)
#define RSF0_BR_NETH	FLAG_FROM_INDEX(RSF_BR_NETH)
#define RSF0_BR_LITE	FLAG_FROM_INDEX(RSF_BR_LITE)
#define RSF0_BR_DARK	FLAG_FROM_INDEX(RSF_BR_DARK)
#define RSF0_BR_CONF	FLAG_FROM_INDEX(RSF_BR_CONF)
#define RSF0_BR_SOUN	FLAG_FROM_INDEX(RSF_BR_SOUN)
#define RSF0_BR_CHAO	FLAG_FROM_INDEX(RSF_BR_CHAO)
#define RSF0_BR_DISE	FLAG_FROM_INDEX(RSF_BR_DISE)
#define RSF0_BR_NEXU	FLAG_FROM_INDEX(RSF_BR_NEXU)
#define RSF0_BR_TIME	FLAG_FROM_INDEX(RSF_BR_TIME)
#define RSF0_BR_INER	FLAG_FROM_INDEX(RSF_BR_INER)
#define RSF0_BR_GRAV	FLAG_FROM_INDEX(RSF_BR_GRAV)
#define RSF0_BR_SHAR	FLAG_FROM_INDEX(RSF_BR_SHAR)
#define RSF0_BR_PLAS	FLAG_FROM_INDEX(RSF_BR_PLAS)
#define RSF0_BR_WALL	FLAG_FROM_INDEX(RSF_BR_WALL)
#define RSF0_BOULDER	FLAG_FROM_INDEX(RSF_BOULDER)

#define RSF1_BA_ACID		FLAG_FROM_INDEX(RSF_BA_ACID)
#define RSF1_BA_ELEC		FLAG_FROM_INDEX(RSF_BA_ELEC)
#define RSF1_BA_FIRE		FLAG_FROM_INDEX(RSF_BA_FIRE)
#define RSF1_BA_COLD		FLAG_FROM_INDEX(RSF_BA_COLD)
#define RSF1_BA_POIS		FLAG_FROM_INDEX(RSF_BA_POIS)
#define RSF1_BA_NETH		FLAG_FROM_INDEX(RSF_BA_NETH)
#define RSF1_BA_WATE		FLAG_FROM_INDEX(RSF_BA_WATE)
#define RSF1_BA_MANA		FLAG_FROM_INDEX(RSF_BA_MANA)
#define RSF1_BA_DARK		FLAG_FROM_INDEX(RSF_BA_DARK)
#define RSF1_DRAIN_MANA		FLAG_FROM_INDEX(RSF_DRAIN_MANA)
#define RSF1_MIND_BLAST		FLAG_FROM_INDEX(RSF_MIND_BLAST)
#define RSF1_BRAIN_SMASH	FLAG_FROM_INDEX(RSF_BRAIN_SMASH)
#define RSF1_CAUSE_1		FLAG_FROM_INDEX(RSF_CAUSE_1)
#define RSF1_CAUSE_2		FLAG_FROM_INDEX(RSF_CAUSE_2)
#define RSF1_CAUSE_3		FLAG_FROM_INDEX(RSF_CAUSE_3)
#define RSF1_CAUSE_4		FLAG_FROM_INDEX(RSF_CAUSE_4)
#define RSF1_BO_ACID		FLAG_FROM_INDEX(RSF_BO_ACID)
#define RSF1_BO_ELEC		FLAG_FROM_INDEX(RSF_BO_ELEC)
#define RSF1_BO_FIRE		FLAG_FROM_INDEX(RSF_BO_FIRE)
#define RSF1_BO_COLD		FLAG_FROM_INDEX(RSF_BO_COLD)
#define RSF1_BO_NETH		FLAG_FROM_INDEX(RSF_BO_NETH)
#define RSF1_BO_WATE		FLAG_FROM_INDEX(RSF_BO_WATE)
#define RSF1_BO_MANA		FLAG_FROM_INDEX(RSF_BO_MANA)
#define RSF1_BO_PLAS		FLAG_FROM_INDEX(RSF_BO_PLAS)
#define RSF1_BO_ICEE		FLAG_FROM_INDEX(RSF_BO_ICEE)
#define RSF1_MISSILE		FLAG_FROM_INDEX(RSF_MISSILE)
#define RSF1_SCARE			FLAG_FROM_INDEX(RSF_SCARE)
#define RSF1_BLIND			FLAG_FROM_INDEX(RSF_BLIND)
#define RSF1_CONF			FLAG_FROM_INDEX(RSF_CONF)
#define RSF1_SLOW			FLAG_FROM_INDEX(RSF_SLOW)
#define RSF1_HOLD			FLAG_FROM_INDEX(RSF_HOLD)

#define RSF2_HASTE			FLAG_FROM_INDEX(RSF_HASTE)
#define RSF2_HEAL			FLAG_FROM_INDEX(RSF_HEAL)
#define RSF2_BLINK			FLAG_FROM_INDEX(RSF_BLINK)
#define RSF2_TPORT			FLAG_FROM_INDEX(RSF_TPORT)
#define RSF2_TELE_TO		FLAG_FROM_INDEX(RSF_TELE_TO)
#define RSF2_TELE_AWAY		FLAG_FROM_INDEX(RSF_TELE_AWAY)
#define RSF2_TELE_LEVEL		FLAG_FROM_INDEX(RSF_TELE_LEVEL)
#define RSF2_DARKNESS		FLAG_FROM_INDEX(RSF_DARKNESS)
#define RSF2_TRAPS			FLAG_FROM_INDEX(RSF_TRAPS)
#define RSF2_FORGET			FLAG_FROM_INDEX(RSF_FORGET)
#define RSF2_S_KIN			FLAG_FROM_INDEX(RSF_S_KIN)
#define RSF2_S_HI_DEMON		FLAG_FROM_INDEX(RSF_S_HI_DEMON)
#define RSF2_S_MONSTER		FLAG_FROM_INDEX(RSF_S_MONSTER)
#define RSF2_S_MONSTERS		FLAG_FROM_INDEX(RSF_S_MONSTERS)
#define RSF2_S_ANIMAL		FLAG_FROM_INDEX(RSF_S_ANIMAL)
#define RSF2_S_SPIDER		FLAG_FROM_INDEX(RSF_S_SPIDER)
#define RSF2_S_HOUND		FLAG_FROM_INDEX(RSF_S_HOUND)
#define RSF2_S_HYDRA		FLAG_FROM_INDEX(RSF_S_HYDRA)
#define RSF2_S_ANGEL		FLAG_FROM_INDEX(RSF_S_ANGEL)
#define RSF2_S_DEMON		FLAG_FROM_INDEX(RSF_S_DEMON)
#define RSF2_S_UNDEAD		FLAG_FROM_INDEX(RSF_S_UNDEAD)
#define RSF2_S_DRAGON		FLAG_FROM_INDEX(RSF_S_DRAGON)
#define RSF2_S_HI_UNDEAD	FLAG_FROM_INDEX(RSF_S_HI_UNDEAD)
#define RSF2_S_HI_DRAGON	FLAG_FROM_INDEX(RSF_S_HI_DRAGON)
#define RSF2_S_WRAITH		FLAG_FROM_INDEX(RSF_S_WRAITH)
#define RSF2_S_UNIQUE		FLAG_FROM_INDEX(RSF_S_UNIQUE)


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
#define RSF0_OBVIOUS_MASK \
	(RSF0_BR_ACID | RSF0_BR_ELEC | RSF0_BR_FIRE | RSF0_BR_COLD | RSF0_BR_POIS | \
	 RSF0_BR_NETH | RSF0_BR_LITE | RSF0_BR_DARK | RSF0_BR_CONF | RSF0_BR_SOUN | \
	 RSF0_BR_CHAO | RSF0_BR_DISE | RSF0_BR_NEXU | RSF0_BR_TIME | RSF0_BR_INER | \
	 RSF0_BR_GRAV | RSF0_BR_SHAR | RSF0_BR_PLAS | RSF0_BR_WALL)


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

#define RSF0_INT_MASK 0L

#define RSF1_INT_MASK \
	(RSF1_HOLD | RSF1_SLOW | RSF1_CONF | RSF1_BLIND | RSF1_SCARE)

#define RSF2_INT_MASK \
	(RSF2_BLINK |  RSF2_TPORT | RSF2_TELE_LEVEL | RSF2_TELE_AWAY | \
	 RSF2_HEAL | RSF2_HASTE | RSF2_TRAPS | \
	 RSF2_S_ANIMAL | RSF2_S_KIN | RSF2_S_MONSTER | RSF2_S_MONSTERS | \
	 RSF2_S_SPIDER | RSF2_S_HOUND | RSF2_S_HYDRA | \
	 RSF2_S_ANGEL | RSF2_S_DRAGON | RSF2_S_UNDEAD | RSF2_S_DEMON | \
	 RSF2_S_HI_DRAGON | RSF2_S_HI_UNDEAD | RSF2_S_HI_DEMON | \
	 RSF2_S_WRAITH | RSF2_S_UNIQUE)


/*
 * "Bolt" spells that may hurt fellow monsters
 */
#define RSF0_BOLT_MASK \
	(RSF0_ARROW_1 | RSF0_ARROW_2 | RSF0_ARROW_3 | RSF0_ARROW_4 | \
	 RSF0_BOULDER)

#define RSF1_BOLT_MASK \
	(RSF1_BO_ACID | RSF1_BO_ELEC | RSF1_BO_FIRE | RSF1_BO_COLD | \
	 RSF1_BO_NETH | RSF1_BO_WATE | RSF1_BO_MANA | \
	 RSF1_BO_PLAS | RSF1_BO_ICEE | RSF1_MISSILE)

#define RSF2_BOLT_MASK 0L

/*
 * Spells that allow the caster to escape
 */
#define RSF0_ESCAPE_MASK 0L

#define RSF1_ESCAPE_MASK 0L

#define RSF2_ESCAPE_MASK \
	(RSF2_BLINK | RSF2_TPORT | RSF2_TELE_AWAY | RSF2_TELE_LEVEL)


/*
 * Spells that hurt the player directly
 */
#define RSF0_ATTACK_MASK \
	(RSF0_ARROW_1 | RSF0_ARROW_2 | RSF0_ARROW_3 | RSF0_ARROW_4 | RSF0_BOULDER | \
	 RSF0_BR_ACID | RSF0_BR_ELEC | RSF0_BR_FIRE | RSF0_BR_COLD | RSF0_BR_POIS | \
	 RSF0_BR_NETH | RSF0_BR_LITE | RSF0_BR_DARK | RSF0_BR_CONF | RSF0_BR_SOUN | \
	 RSF0_BR_CHAO | RSF0_BR_DISE | RSF0_BR_NEXU | RSF0_BR_TIME | RSF0_BR_INER | \
	 RSF0_BR_GRAV | RSF0_BR_SHAR | RSF0_BR_PLAS | RSF0_BR_WALL)

#define RSF1_ATTACK_MASK \
	(RSF1_BA_ACID | RSF1_BA_ELEC | RSF1_BA_FIRE | RSF1_BA_COLD | RSF1_BA_POIS | \
	 RSF1_BA_NETH | RSF1_BA_WATE | RSF1_BA_MANA | RSF1_BA_DARK | \
	 RSF1_MIND_BLAST | RSF1_BRAIN_SMASH | RSF1_CAUSE_1 | RSF1_CAUSE_2 | \
	 RSF1_CAUSE_3 | RSF1_CAUSE_4 | RSF1_BO_ACID | RSF1_BO_ELEC | RSF1_BO_FIRE | \
	 RSF1_BO_COLD | RSF1_BO_NETH | RSF1_BO_WATE | RSF1_BO_MANA | \
	 RSF1_BO_PLAS | RSF1_BO_ICEE | RSF1_MISSILE)

#define RSF2_ATTACK_MASK 0L


/*
 * Summoning spells
 */
#define RSF0_SUMMON_MASK 0L

#define RSF1_SUMMON_MASK 0L

#define RSF2_SUMMON_MASK \
	(RSF2_S_KIN | RSF2_S_MONSTER | RSF2_S_MONSTERS | RSF2_S_ANIMAL | \
	 RSF2_S_SPIDER | RSF2_S_HOUND | RSF2_S_HYDRA | RSF2_S_ANGEL | \
	 RSF2_S_DEMON | RSF2_S_UNDEAD | RSF2_S_DRAGON | RSF2_S_HI_UNDEAD | \
	 RSF2_S_HI_DEMON | RSF2_S_HI_DRAGON | RSF2_S_WRAITH | RSF2_S_UNIQUE)


/*
 * Spells that improve the caster's tactical position
 */
#define RSF0_TACTIC_MASK 0L

#define RSF1_TACTIC_MASK 0L

#define RSF2_TACTIC_MASK \
	(RSF2_BLINK)


/*
 * Annoying spells
 */
#define RSF0_ANNOY_MASK \
	(RSF0_SHRIEK)

#define RSF1_ANNOY_MASK \
	(RSF1_DRAIN_MANA | RSF1_MIND_BLAST | RSF1_BRAIN_SMASH | RSF1_SCARE | \
	 RSF1_BLIND | RSF1_CONF | RSF1_SLOW | RSF1_HOLD)

#define RSF2_ANNOY_MASK \
	(RSF2_TELE_TO | RSF2_DARKNESS | RSF2_TRAPS | RSF2_FORGET)


/*
 * Spells that increase the caster's relative speed
 */
#define RSF0_HASTE_MASK 0L

#define RSF1_HASTE_MASK \
	(RSF1_SLOW | RSF1_HOLD)

#define RSF2_HASTE_MASK \
	(RSF2_HASTE)


/*
 * Healing spells
 */
#define RSF0_HEAL_MASK 0L

#define RSF1_HEAL_MASK 0L

#define RSF2_HEAL_MASK \
	(RSF2_HEAL)


/*
 * Innate spell-like effects
 */
#define RSF0_INNATE_MASK \
	(RSF0_SHRIEK | RSF0_ARROW_1 | RSF0_ARROW_2 | RSF0_ARROW_3 | RSF0_ARROW_4 | \
	 RSF0_BR_ACID | RSF0_BR_ELEC | RSF0_BR_FIRE | RSF0_BR_COLD | RSF0_BR_POIS | \
	 RSF0_BR_NETH | RSF0_BR_LITE | RSF0_BR_DARK | RSF0_BR_CONF | RSF0_BR_SOUN | \
	 RSF0_BR_CHAO | RSF0_BR_DISE | RSF0_BR_NEXU | RSF0_BR_TIME | RSF0_BR_INER | \
	 RSF0_BR_GRAV | RSF0_BR_SHAR | RSF0_BR_PLAS | RSF0_BR_WALL | RSF0_BR_MANA | \
	 RSF0_BOULDER)

#define RSF1_INNATE_MASK 0L

#define RSF2_INNATE_MASK 0L

/*
 * Offset attack flag indexes
 */

#define RFA_SHRIEK		RSF_SHRIEK
#define RFA_0			SHRIEK
#define RFA_1			MACRO_NULL
#define RFA_2			MACRO_NULL
#define RFA_3			MACRO_NULL
#define RFA_ARROW_1		RSF_ARROW_1
#define RFA_4			ARROW_1
#define RFA_ARROW_2		RSF_ARROW_2
#define RFA_5			ARROW_2
#define RFA_ARROW_3		RSF_ARROW_3
#define RFA_6			ARROW_3
#define RFA_ARROW_4		RSF_ARROW_4
#define RFA_7			ARROW_4
#define RFA_BR_ACID		RSF_BR_ACID
#define RFA_8			BR_ACID
#define RFA_BR_ELEC		RSF_BR_ELEC
#define RFA_9			BR_ELEC
#define RFA_BR_FIRE		RSF_BR_FIRE
#define RFA_10			BR_FIRE
#define RFA_BR_COLD		RSF_BR_COLD
#define RFA_11			BR_COLD
#define RFA_BR_POIS		RSF_BR_POIS
#define RFA_12			BR_POIS
#define RFA_BR_NETH		RSF_BR_NETH
#define RFA_13			BR_NETH
#define RFA_BR_LITE		RSF_BR_LITE
#define RFA_14			BR_LITE
#define RFA_BR_DARK		RSF_BR_DARK
#define RFA_15			BR_DARK
#define RFA_BR_CONF		RSF_BR_CONF
#define RFA_16			BR_CONF
#define RFA_BR_SOUN		RSF_BR_SOUN
#define RFA_17			BR_SOUND
#define RFA_BR_CHAO		RSF_BR_CHAO
#define RFA_18			BR_CHAOS
#define RFA_BR_DISE		RSF_BR_DISE
#define RFA_19			BR_DISE
#define RFA_BR_NEXU		RSF_BR_NEXU
#define RFA_20			BR_NEXU
#define RFA_BR_TIME		RSF_BR_TIME
#define RFA_21			BR_TIME
#define RFA_BR_INER		RSF_BR_INER
#define RFA_22			BR_INER
#define RFA_BR_GRAV		RSF_BR_GRAV
#define RFA_23			BR_GRAV
#define RFA_BR_SHAR		RSF_BR_SHAR
#define RFA_24			BR_SHAR
#define RFA_BR_PLAS		RSF_BR_PLAS
#define RFA_25			BR_PLAS
#define RFA_BR_WALL		RSF_BR_WALL
#define RFA_26			BR_WALL
#define RFA_27			MACRO_NULL
#define RFA_28			MACRO_NULL
#define RFA_29			MACRO_NULL
#define RFA_30			MACRO_NULL
#define RFA_BOULDER		RSF_BOULDER
#define RFA_31			BOULDER

#define RFA_BA_ACID		RSF_BA_ACID
#define RFA_32			BA_ACID
#define RFA_BA_ELEC		RSF_BA_ELEC
#define RFA_33			BA_ELEC
#define RFA_BA_FIRE		RSF_BA_FIRE
#define RFA_34			BA_FIRE
#define RFA_BA_COLD		RSF_BA_COLD
#define RFA_35			BA_COLD
#define RFA_BA_POIS		RSF_BA_POIS
#define RFA_36			BA_POIS
#define RFA_BA_NETH		RSF_BA_NETH
#define RFA_37			BA_NETH
#define RFA_BA_WATE		RSF_BA_WATE
#define RFA_38			BA_WATE
#define RFA_BA_MANA		RSF_BA_MANA
#define RFA_39			BA_MANA
#define RFA_BA_DARK		RSF_BA_DARK
#define RFA_40			BA_DARK
#define RFA_DRAIN_MANA	RSF_DRAIN_MANA
#define RFA_41			DRAIN_MANA
#define RFA_MIND_BLAST	RSF_MIND_BLAST
#define RFA_42			MIND_BLAST
#define RFA_BRAIN_SMASH	RSF_BRAIN_SMASH
#define RFA_43			BRAIN_SMASH
#define RFA_CAUSE_1		RSF_CAUSE_1
#define RFA_44			CAUSE_1
#define RFA_CAUSE_2		RSF_CAUSE_2
#define RFA_45			CAUSE_2
#define RFA_CAUSE_3		RSF_CAUSE_3
#define RFA_46			CAUSE_3
#define RFA_CAUSE_4		RSF_CAUSE_4
#define RFA_47			CAUSE_4
#define RFA_BO_ACID		RSF_BO_ACID
#define RFA_48			BO_ACID
#define RFA_BO_ELEC		RSF_BO_ELEC
#define RFA_49			BO_ELEC
#define RFA_BO_FIRE		RSF_BO_FIRE
#define RFA_50			BO_FIRE
#define RFA_BO_COLD		RSF_BO_COLD
#define RFA_51			BO_COLD
#define RFA_52			MACRO_NULL
#define RFA_BO_NETH		RSF_BO_NETH
#define RFA_53			BO_NETH
#define RFA_BO_WATE		RSF_BO_WATE
#define RFA_54			BO_WATE
#define RFA_BO_MANA		RSF_BO_MANA
#define RFA_55			BO_MANA
#define RFA_BO_PLAS		RSF_BO_PLAS
#define RFA_56			BO_PLAS
#define RFA_BO_ICEE		RSF_BO_ICEE
#define RFA_57			BO_ICEE
#define RFA_MISSILE		RSF_MISSILE
#define RFA_58			MISSILE
#define RFA_SCARE		RSF_SCARE
#define RFA_59			SCARE
#define RFA_BLIND		RSF_BLIND
#define RFA_60			BLIND
#define RFA_CONF		RSF_CONF
#define RFA_61			CONF
#define RFA_SLOW		RSF_SLOW
#define RFA_62			SLOW
#define RFA_HOLD		RSF_HOLD
#define RFA_63			HOLD

#define RFA_HASTE		RSF_HASTE
#define RFA_64			HASTE
#define RFA_65			MACRO_NULL
#define RFA_HEAL		RSF_HEAL
#define RFA_66			HEAL
#define RFA_67			MACRO_NULL
#define RFA_BLINK		RSF_BLINK
#define RFA_68			BLINK
#define RFA_TPORT		RSF_TPORT
#define RFA_69			TPORT
#define RFA_70			MACRO_NULL
#define RFA_71			MACRO_NULL
#define RFA_TELE_TO		RSF_TELE_TO
#define RFA_72			TELE_TO
#define RFA_TELE_AWAY	RSF_TELE_AWAY
#define RFA_73			TELE_AWAY
#define RFA_TELE_LEVEL	RSF_TELE_LEVEL
#define RFA_74			TELE_LEVEL
#define RFA_75			MACRO_NULL
#define RFA_DARKNESS	RSF_DARKNESS
#define RFA_76			DARKNESS
#define RFA_TRAPS		RSF_TRAPS
#define RFA_77			TRAPS
#define RFA_FORGET		RSF_FORGET
#define RFA_78			FORGET
#define RFA_79			MACRO_NULL
#define RFA_S_KIN		RSF_S_KIN
#define RFA_80			S_KIN
#define RFA_S_HI_DEMON	RSF_S_HI_DEMON
#define RFA_81			S_HI_DEMON
#define RFA_S_MONSTER	RSF_S_MONSTER
#define RFA_82			S_MONSTER
#define RFA_S_MONSTERS	RSF_S_MONSTERS
#define RFA_83			S_MONSTERS
#define RFA_S_ANIMAL	RSF_S_ANIMAL
#define RFA_84			S_ANIMAL
#define RFA_S_SPIDER	RSF_S_SPIDER
#define RFA_85			S_SPIDER
#define RFA_S_HOUND		RSF_S_HOUND
#define RFA_86			S_HOUND
#define RFA_S_HYDRA		RSF_S_HYDRA
#define RFA_87			S_HYDRA
#define RFA_S_ANGEL		RSF_S_ANGEL
#define RFA_88			S_ANGEL
#define RFA_S_DEMON		RSF_S_DEMON
#define RFA_89			S_DEMON
#define RFA_S_UNDEAD	RSF_S_UNDEAD
#define RFA_90			S_UNDEAD
#define RFA_S_DRAGON	RSF_S_DRAGON
#define RFA_91			S_DRAGON
#define RFA_S_HI_UNDEAD	RSF_S_HI_UNDEAD
#define RFA_92			S_HI_UNDEAD
#define RFA_S_HI_DRAGON	RSF_S_HI_DRAGON
#define RFA_93			S_HI_DRAGON
#define RFA_S_WRAITH	RSF_S_WRAITH
#define RFA_94			S_WRAITH
#define RFA_S_UNIQUE	RSF_S_UNIQUE
#define RFA_95			S_UNIQUE


