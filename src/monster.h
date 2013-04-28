/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* monster.h: monster-related definitions */

#ifndef MONSTER_H_INCLUDED
#define MONSTER_H_INCLUDED

/*
 * There is a 1/40 (2.5%) chance of inflating the requested monster_level
 * during the creation of a monsters (see "get_mon_num()" in "monster.c").
 * Lower values yield harder monsters more often.
 */
#define NASTY_MON	40		/* 1/chance of inflated monster level */

/*
 * A monster can only "multiply" (reproduce) if there are fewer than 100
 * monsters on the level capable of such spontaneous reproduction.  This
 * is a hack which prevents the "mon_list[]" array from exploding due to
 * reproducing monsters.  Messy, but necessary.
 */
#define MAX_REPRO	100

/* Important monsters (hack!) */
#define MON_SAURON			546
#define MON_VALAR_HEAD			646
#define MON_VALAR_TAIL			654

/*
 * Legal restrictions for "summon_specific()"
 */
enum
{
        SUMMON_KIN = 1,		/* Summons similar non-unique monsters */
        SUMMON_LAWFUL,		/* Summons random lawful monsters */
        SUMMON_NEUTRAL,		/* Summons random neutral monsters */
        SUMMON_CHAOTIC,		/* Summons random chaotic monsters */
        SUMMON_ULTIMATE,	/* Summons some [hard-coded] Really Cool Mobs */
        SUMMON_UNIQUE,		/* Summons uniques */
	/* All these cannot summon uniques */
        SUMMON_YEEK,		/* Yeek! Yeek! 'y' */
        SUMMON_ORC,		/* Orcs 'o' */
        SUMMON_OGRE, 		/* Ogres 'O' */
        SUMMON_DARK_ELF,	/* Dark elves 'h', name starting with "dark" */
        SUMMON_GOLEM,		/* Golems 'g' */
        SUMMON_VORTEX,		/* Vortices 'v' */
        SUMMON_ELEMENTAL,	/* Elementals 'E' */
        SUMMON_ANT,		/* Ants 'a' */
        SUMMON_SPIDER,		/* Spiders/etc 'S' */
        SUMMON_HOUND,		/* Dogs 'C', 'Z' */
        SUMMON_HYDRA,		/* Hydras 'M' */
	SUMMON_TROLL,		/* Trolls 'T' */
        SUMMON_ANIMAL,		/* Animals (flagged) */
        SUMMON_THIEF,		/* Thieves (everything that hits to steal gold or items */
        SUMMON_DEMON,		/* Demons (flagged) */
        SUMMON_UNDEAD,		/* Undeads (flagged) */
        SUMMON_DRAGON,		/* Dragons (flagged) */
        SUMMON_MATURE_DRAGON,	/* Mature Dragons 'd', with "mature" or "drake" in name */
        SUMMON_LICH,		/* Liches 'L' */
        SUMMON_WIGHT_WRAITH,	/* Wraiths 'W' */
        SUMMON_VROCK,		/* "Demon Troops" (flagged demons, with groups) */
        SUMMON_BALROG,		/* "Demon Commanders" (flagged demons, 'U', either summoners or escorted) */
        SUMMON_UNDEAD_DRAGON,	/* Undead Dragons (flagged as both) */
	SUMMON_AIR_ELEM,        /* Air Elementals (hard-coded) */
        SUMMON_WATER_ELEM,	/* Water Elementals (hard-coded) */
        SUMMON_FIRE_ELEM,	/* Fire Elementals (hard-coded) */
	/* xxx other elementals */
        SUMMON_ENT,		/* Ents '%' */
        SUMMON_DEMON_SUMM,	/* Everything that summons demons or high demons */
        SUMMON_UNDEAD_SUMM,	/* Everything that summons undeads or high undeads */
        SUMMON_DRAGON_SUMM,	/* Everything that summons dragons or high dragons */
        SUMMON_HI_DEMON,	/* High Demons 'U' */
        SUMMON_HI_UNDEAD,	/* High Undeads 'L', 'V', 'W' */
        SUMMON_HI_DRAGON,	/* High Dragons 'D' */
        SUMMON_AINU,		/* Ainur 'A' */
	/* Special unique summons */
        SUMMON_BERTBILLTOM,	/* The Stone Trolls (unique 'T', by name) */
        SUMMON_WRAITH		/* Ringwraiths (unique 'W') */
};

/*
 * Some constants for the "learn" code.  These generalized from the
 * old DRS constants.
 */
enum
{
        LRN_FREE_SAVE = 10,
        LRN_MANA,
        LRN_ACID,
        LRN_ELEC,
        LRN_FIRE,
        LRN_COLD,
        LRN_POIS,
        LRN_FEAR_SAVE,
        LRN_LITE,
        LRN_DARK,
        LRN_BLIND,
        LRN_CONFU,
        LRN_SOUND,
        LRN_SHARD,
        LRN_NEXUS,
        LRN_NETHR,
        LRN_CHAOS,
        LRN_DISEN,
        LRN_SAVE,
        LRN_ARCH,
        LRN_PARCH,
        LRN_ICE,
        LRN_PLAS,
        LRN_SOUND2,	/* attacks which aren't resisted, but res sound prevent stun */
        LRN_STORM,
        LRN_WATER,
        LRN_NEXUS_SAVE,	/* Both resist Nexus and Saves apply */
        LRN_BLIND_SAVE,	/* Both resist Blind and Saves apply */
        LRN_CONFU_SAVE,	/* Both resist Confusion and Saves apply */
        LRN_DARK_SAVE,
        LRN_HOLY_SAVE,
	LRN_MAX
};

/*
 * Bit flags for the "monster_desc" function
 */
#define MDESC_OBJE		0x01	/* Objective (or Reflexive) */
#define MDESC_POSS		0x02	/* Possessive (or Reflexive) */
#define MDESC_IND1		0x04	/* Indefinites for hidden monsters */
#define MDESC_IND2		0x08	/* Indefinites for visible monsters */
#define MDESC_PRO1		0x10	/* Pronominalize hidden monsters */
#define MDESC_PRO2		0x20	/* Pronominalize visible monsters */
#define MDESC_HIDE		0x40	/* Assume the monster is hidden */
#define MDESC_SHOW		0x80	/* Assume the monster is visible */

/*
 * Some bit-flags for the "smart" field".
 *
 * Most of these map to the "TR2_xxx" flags.
 */
#define SM_OPP_ACID	0x00000001
#define SM_OPP_ELEC	0x00000002
#define SM_OPP_FIRE	0x00000004
#define SM_OPP_COLD	0x00000008
#define SM_OPP_POIS	0x00000010
#define SM_OPP_XXX1	0x00000020
#define SM_OPP_XXX2	0x00000040
#define SM_OPP_XXX3	0x00000080
#define SM_GOOD_SAVE	0x00000100
#define SM_PERF_SAVE	0x00000200
#define SM_IMM_FREE	0x00000400
#define SM_IMM_MANA	0x00000800
#define SM_IMM_ACID	0x00001000
#define SM_IMM_ELEC	0x00002000
#define SM_IMM_FIRE	0x00004000
#define SM_IMM_COLD	0x00008000
#define SM_RES_ACID	0x00010000
#define SM_RES_ELEC	0x00020000
#define SM_RES_FIRE	0x00040000
#define SM_RES_COLD	0x00080000
#define SM_RES_POIS	0x00100000
#define SM_RES_FEAR	0x00200000
#define SM_RES_LITE	0x00400000
#define SM_RES_DARK	0x00800000
#define SM_RES_BLIND	0x01000000
#define SM_RES_CONFU	0x02000000
#define SM_RES_SOUND	0x04000000
#define SM_RES_SHARD	0x08000000
#define SM_RES_NEXUS	0x10000000
#define SM_RES_NETHR	0x20000000
#define SM_RES_CHAOS	0x40000000
#define SM_RES_DISEN	0x80000000

#define MONSTER_BLOW_MAX 4

/*** Monster blow constants ***/

/* Methods */
enum
{
        RBM_HIT = 1,
        RBM_TOUCH,
        RBM_PUNCH,
        RBM_KICK,
        RBM_CLAW,
        RBM_BITE,
        RBM_STING,
        RBM_PECK,
        RBM_BUTT,
        RBM_CRUSH,
        RBM_ENGULF,
        RBM_CRAWL,
        RBM_DROOL,
        RBM_SPIT,
        RBM_SLIME,
        RBM_GAZE,
        RBM_WAIL,
        RBM_SPORE,
        RBM_BEG,
        RBM_INSULT
};

/* Effects */
enum
{
        RBE_HURT = 1,	/* Normal damage */
        RBE_WOUND,	/* Wounds */
        RBE_BATTER,	/* Stuns */
        RBE_SHATTER,	/* Earthquake */

        RBE_UN_BONUS,	/* Disenchant items */
        RBE_UN_POWER,	/* Drain charges */
        RBE_LOSE_MANA,	/* Drain mana */
        RBE_EAT_GOLD,	/* Steal gold */
        RBE_EAT_ITEM,	/* Steal items */
        RBE_EAT_FOOD,	/* Eat food */
        RBE_EAT_LITE,	/* Drain fuel */
        RBE_HUNGER,	/* Cause hunger */

	/* Elemental physical attacks */
        RBE_POISON,
        RBE_ACID,
        RBE_ELEC,
        RBE_FIRE,
        RBE_COLD,

	/* Physical attacks with magic effects */
        RBE_BLIND,
        RBE_CONFUSE,
        RBE_TERRIFY,
        RBE_PARALYZE,
        RBE_HALLU,
        RBE_DISEASE,

	/* Drain stat */
        RBE_LOSE_STR,
        RBE_LOSE_INT,
        RBE_LOSE_WIS,
        RBE_LOSE_DEX,
        RBE_LOSE_CON,
        RBE_LOSE_CHR,
        RBE_LOSE_ALL,

	/* Drain experience */
        RBE_EXP_10,
        RBE_EXP_20,
        RBE_EXP_40,
        RBE_EXP_80
};

/*** Monster flags ***/

/*
 * Special Monster Flags
 * XXX Many are unused
 */
#define MFLAG_VIEW          0x00000001  /* Monster is in line of sight */
#define MFLAG_DLIM	    0x00000002	/* Knowledge of monster is limited */
#define MFLAG_MIMIC         0x00000004  /* A mimic detected as an object */
#define MFLAG_ACTV          0x00000008  /* Monster is in active mode */
#define MFLAG_OVER          0x00000010  /* Monster (presumably pet) has "overriding" target set */
#define MFLAG_TOWN          0x00000020  /* Monster is using "townsman" AI */
#define MFLAG_SHOW	    0x00000040	/* Monster is recently memorized */
#define MFLAG_MARK	    0x00000080	/* Monster is currently memorized */
#define MFLAG_WARY          0x00000100  /* Monster is wary (used for traps)*/
#define MFLAG_TEMP          0x00000200  /* Monster is temporarily marked */
#define MFLAG_FULL          0x00000400  /* Monster has full required visibility */
#define MFLAG_PERM          0x00000800  /* Monster (presumably pet) is "permanent" (moves with player between levels) */
#define MFLAG_STAY          0x00001000	/* Monster guards his position */
#define MFLAG_DOOM          0x00002000  /* Monster is doomed */
/* Pet arsenal limitations */
#define MFLAG_NO_RANGED	    0x00004000 /* Disable using ranged attacks (missiles, breathes) */
#define MFLAG_NO_OFFENSE    0x00008000 /* Disable using offensive spells */
#define MFLAG_NO_TELEPORT   0x00010000 /* Disable using teleportation spells */
#define MFLAG_NO_SAVING	    0x00020000 /* Disable using spells with a saving throw */
#define MFLAG_NO_SUMM	    0x00040000 /* Disable using single summons */
#define MFLAG_NO_SUMM_MANY  0x00080000 /* Disable using mass summons */

/* Initial pet orders */
#define MFLAG_PET_DEFAULT	(MFLAG_NO_TELEPORT | MFLAG_NO_SAVING)

/*
 * Monster race bit flags
 */
#define RF1_UNIQUE		0x00000001	/* Unique Monster */
#define RF1_QUESTOR		0x00000002	/* Quest Monster */
#define RF1_MALE		0x00000004	/* Male gender */
#define RF1_FEMALE		0x00000008	/* Female gender */
#define RF1_CHAR_CLEAR		0x00000010	/* Absorbs symbol */
#define RF1_CHAR_MIMIC		0x00000020	/* Monster can mimic a symbol */
#define RF1_ATTR_CLEAR		0x00000040	/* Absorbs color */
#define RF1_ATTR_MULTI		0x00000080	/* Changes color */
#define RF1_FORCE_DEPTH		0x00000100	/* Start at "correct" depth */
#define RF1_FORCE_MAXHP		0x00000200	/* Start with max hitpoints */
#define RF1_FORCE_SLEEP		0x00000400	/* Start out sleeping */
#define RF1_FORCE_EXTRA		0x00000800	/* Unused */
#define RF1_FRIEND		0x00001000	/* Arrive with 2-3 friends*/
#define RF1_FRIENDS		0x00002000	/* Arrive with 4-6 friends */
#define RF1_ESCORT		0x00004000	/* Arrive with 4-6 escorts  */
#define RF1_ESCORTS		0x00008000	/* Arrive with 12-18 large escorts  */
#define RF1_NEVER_BLOW		0x00010000	/* Never make physical blow */
#define RF1_NEVER_MOVE		0x00020000	/* Never make physical move */
#define RF1_RAND_25		0x00040000	/* Moves randomly (25%) */
#define RF1_RAND_50		0x00080000	/* Moves randomly (50%) */
#define RF1_ONLY_GOLD		0x00100000	/* Drop only gold */
#define RF1_ONLY_ITEM		0x00200000	/* Drop only items */
#define RF1_DROP_60		0x00400000	/* Drop an item/gold (60%) */
#define RF1_DROP_90		0x00800000	/* Drop an item/gold (90%) */
#define RF1_DROP_1D2		0x01000000	/* Drop 1d2 items/gold */
#define RF1_DROP_2D2		0x02000000	/* Drop 2d2 items/gold */
#define RF1_DROP_3D2		0x04000000	/* Drop 3d2 items/gold */
#define RF1_DROP_4D2		0x08000000	/* Drop 4d2 items/gold */
#define RF1_DROP_GOOD		0x10000000	/* Drop good items */
#define RF1_DROP_GREAT		0x20000000	/* Drop great items */
#define RF1_DROP_CHEST		0x40000000	/* Drop "useful" items */
#define RF1_DROP_CHOSEN		0x80000000	/* Drop "chosen" items */

/* RF1 uber-flags to cover multiple items */
#define RF1_DROP_UP_TO_10	(RF1_DROP_2D2 | RF1_DROP_3D2)
#define RF1_DROP_UP_TO_12	(RF1_DROP_1D2 | RF1_DROP_2D2 | RF1_DROP_3D2)
#define RF1_DROP_UP_TO_14	(RF1_DROP_4D2 | RF1_DROP_1D2 | RF1_DROP_2D2)
#define RF1_DROP_UP_TO_16	(RF1_DROP_4D2 | RF1_DROP_3D2 | RF1_DROP_1D2)
#define RF1_DROP_UP_TO_18	(RF1_DROP_4D2 | RF1_DROP_3D2 | RF1_DROP_2D2)
#define RF1_DROP_UP_TO_20	(RF1_DROP_4D2 | RF1_DROP_3D2 | RF1_DROP_2D2 | RF1_DROP_1D2)

/*
 * New monster race bit flags
 */
#define RF2_STUPID		0x00000001  /* Monster is stupid */
#define RF2_SMART		0x00000002  /* Monster is smart */
#define RF2_HAS_LITE    	0x00000004  /* Monster carries light */
#define RF2_RF2XXX2		0x00000008  /* (?) */
#define RF2_INVISIBLE		0x00000010  /* Monster avoids vision */
#define RF2_COLD_BLOOD		0x00000020  /* Monster avoids infra */
#define RF2_EMPTY_MIND		0x00000040  /* Monster avoids telepathy.  NOT mindless. */
#define RF2_WEIRD_MIND		0x00000080  /* Monster avoids telepathy sometimes */
#define RF2_MULTIPLY		0x00000100  /* Monster reproduces */
#define RF2_REGENERATE		0x00000200  /* Monster regenerates especially quickly */
#define RF2_RF2XXX3		0x00000400  /* (?) */
#define RF2_EVASIVE		0x00000800  /* Monster often avoids blows */
#define RF2_CLOUD_SURROUND	0x00001000  /* Surrounded by gas/spores/darkness */
#define RF2_RF2XXX5		0x00002000  /* (?) */
#define RF2_RF2XXX7		0x00004000  /* (?) */
#define RF2_RF2XXX6		0x00008000  /* (?) */
#define RF2_OPEN_DOOR		0x00010000  /* Monster can open doors */
#define RF2_BASH_DOOR		0x00020000  /* Monster can bash doors */
#define RF2_PASS_WALL		0x00040000  /* Monster can pass walls */
#define RF2_KILL_WALL		0x00080000  /* Monster can destroy walls */
#define RF2_RF2XXX8		0x00100000  /* (?) */
#define RF2_KILL_BODY		0x00200000  /* Monster can kill monsters */
#define RF2_TAKE_ITEM		0x00400000  /* Monster can pick up items */
#define RF2_KILL_ITEM		0x00800000  /* Monster can crush items */
#define RF2_BRAIN_1		0x01000000  /* (?) */
#define RF2_LOW_MANA_RUN	0x02000000  /* Runs away/teleports when low on mana */
#define RF2_BRAIN_2		0x04000000  /* (?) */
#define RF2_POWERFUL		0x08000000  /* Breath loses less power with distance */
#define RF2_RF2XXX1		0x10000000  /* Unused */
#define RF2_RF2XXX9		0x20000000  /* Unused */
#define RF2_RF2XX10      	0x40000000  /* Unused */
#define RF2_BRAIN_3		0x80000000  /* (?) */

/*
 * New monster race bit flags
 */
#define RF3_ORC			0x00000001	/* Orc */
#define RF3_TROLL		0x00000002	/* Troll */
#define RF3_GIANT		0x00000004	/* Giant */
#define RF3_DRAGON		0x00000008	/* Dragon */
#define RF3_DEMON		0x00000010	/* Demon */
#define RF3_UNDEAD		0x00000020	/* Undead */
#define RF3_EVIL		0x00000040	/* Evil */
#define RF3_ANIMAL		0x00000080	/* Animal */
#define RF3_LAWFUL		0x00000100	/* Lawful */
#define RF3_RF3XXX1		0x00000200	/* Unused (neutral) */
#define RF3_CHAOTIC		0x00000400	/* Chaotic */
#define RF3_VALA		0x00000800	/* Vala */
#define RF3_HURT_LITE		0x00001000	/* Hurt by lite */
#define RF3_HURT_ROCK		0x00002000	/* Hurt by rock remover */
#define RF3_HURT_FIRE		0x00004000	/* Hurt badly by fire */
#define RF3_HURT_COLD		0x00008000	/* Hurt badly by cold */
#define RF3_IM_ACID		0x00010000	/* Resist acid a lot */
#define RF3_IM_ELEC		0x00020000	/* Resist elec a lot */
#define RF3_IM_FIRE		0x00040000	/* Resist fire a lot */
#define RF3_IM_COLD		0x00080000	/* Resist cold a lot */
#define RF3_IM_POIS		0x00100000	/* Resist poison a lot */
#define RF3_RES_CHAOS		0x00200000	/* Immune to chaos */
#define RF3_RES_NETHR		0x00400000	/* Resist nether a lot */
#define RF3_RES_WATER		0x00800000	/* Resist water, move fast in water  */
#define RF3_RES_PLAS		0x01000000	/* Resist plasma */
#define RF3_RES_NEXUS		0x02000000	/* Resist nexus */
#define RF3_RES_DISEN		0x04000000	/* Resist disenchantment */
#define RF3_NO_SLOW		0x08000000	/* Cannot be slowed */
#define RF3_NO_FEAR		0x10000000	/* Cannot be scared */
#define RF3_NO_STUN		0x20000000	/* Cannot be stunned */
#define RF3_NO_CONF		0x40000000	/* Cannot be confused/blinded */
#define RF3_NO_SLEEP		0x80000000	/* Cannot be slept */

/* RF3 uber-flags to cover multiple resists */
#define RF3_IM_ELEM	(RF3_IM_ACID | RF3_IM_ELEC | RF3_IM_FIRE | RF3_IM_COLD | RF3_IM_POIS)
#define RF3_IM_ALL	(RF3_IM_ELEM | RF3_RES_PLAS| RF3_RES_NETHR | RF3_RES_NEXUS | \
					 RF3_RES_DISEN | RF3_RES_CHAOS | RF3_RES_WATER)
#define RF3_NO_CHARM (RF3_NO_FEAR | RF3_NO_STUN | RF3_NO_CONF | RF3_NO_SLEEP | RF3_NO_SLOW)

/*
 * Monster racial flags - innate or physical ranged attacks
 * these spells do not have a failure rate - JG
 * other spells have a chance of failing
 */
#define RF4_SHRIEK         0x00000001  /* Shriek for help */
#define RF4_LASH           0x00000002  /* Use a melee attack at range 2 or 3 */
#define RF4_BOULDER        0x00000004  /* Throw a boulder */
#define RF4_SHOT           0x00000008  /* Fire sling shot */
#define RF4_ARROW          0x00000010  /* Fire arrows */
#define RF4_BOLT           0x00000020  /* Fire crossbow quarrels */
#define RF4_MISSL          0x00000040  /* Fire other physical missiles */
#define RF4_PMISSL         0x00000080  /* Fire poisoned missiles */
#define RF4_BRTH_ACID      0x00000100  /* Breathe Acid */
#define RF4_BRTH_ELEC      0x00000200  /* Breathe Elec */
#define RF4_BRTH_FIRE      0x00000400  /* Breathe Fire */
#define RF4_BRTH_COLD      0x00000800  /* Breathe Cold */
#define RF4_BRTH_POIS      0x00001000  /* Breathe Poison */
#define RF4_BRTH_PLAS      0x00002000  /* Breathe Plasma */
#define RF4_BRTH_LITE      0x00004000  /* Breathe Light */
#define RF4_BRTH_DARK      0x00008000  /* Breathe Dark */
#define RF4_BRTH_CONFU     0x00010000  /* Breathe Confusion */
#define RF4_BRTH_SOUND     0x00020000  /* Breathe Sound */
#define RF4_BRTH_SHARD     0x00040000  /* Breathe Shards */
#define RF4_BRTH_INER      0x00080000  /* Breathe Inertia */
#define RF4_BRTH_GRAV      0x00100000  /* Breathe Gravity */
#define RF4_BRTH_WIND      0x00200000  /* Breathe Wind */
#define RF4_BRTH_FORCE     0x00400000  /* Breathe Force */
#define RF4_BRTH_NEXUS     0x00800000  /* Breathe Nexus */
#define RF4_BRTH_NETHR     0x01000000  /* Breathe Nether */
#define RF4_BRTH_CHAOS     0x02000000  /* Breathe Chaos */
#define RF4_BRTH_DISEN     0x04000000  /* Breathe Disenchant */
#define RF4_BRTH_TIME      0x08000000  /* Breathe Time */
#define RF4_BRTH_MANA      0x10000000  /* Breathe Mana */
#define RF4_RF4XXX1        0x20000000  /*  */
#define RF4_RF4XXX2        0x40000000  /*  */
#define RF4_RF4XXX3        0x80000000  /*  */

/* RF4 uber-flags to cover multiple breaths */
#define RF4_BRTH_ELEM		(RF4_BRTH_ACID | RF4_BRTH_ELEC | RF4_BRTH_FIRE | \
						 RF4_BRTH_COLD | RF4_BRTH_POIS)
#define RF4_BRTH_POWER	(RF4_BRTH_ELEM | RF4_BRTH_NETHR | RF4_BRTH_LITE | RF4_BRTH_DARK | \
						 RF4_BRTH_CONFU | RF4_BRTH_SOUND | RF4_BRTH_CHAOS | RF4_BRTH_DISEN | \
						 RF4_BRTH_NEXUS)
#define RF4_BRTH_ALL	(RF4_BRTH_POWER | RF4_BRTH_TIME | RF4_BRTH_INER | RF4_BRTH_GRAV | \
						 RF4_BRTH_SHARD | RF4_BRTH_PLAS | RF4_BRTH_FORCE | RF4_BRTH_WIND | \
						 RF4_BRTH_MANA)

#define RF4_RANGED	0xffffffff	/* All RF4_xxx abilities are innate attacks */

/*
 * Monster racial flags - defined-area projection spells
 */
#define RF5_BALL_ACID      0x00000001  /* Acid Ball -> Acid Storm */
#define RF5_BALL_ELEC      0x00000002  /* Elec Ball -> Elec Storm */
#define RF5_BALL_FIRE      0x00000004  /* Fire Ball -> Fire Storm */
#define RF5_BALL_COLD      0x00000008  /* Cold Ball -> Cold Storm */
#define RF5_BALL_POIS      0x00000010  /* Stinking Cloud -> Poison Storm */
#define RF5_BALL_LITE      0x00000020  /* Light Ball / Starburst */
#define RF5_BALL_DARK      0x00000040  /* Darkness Ball -> Dark Storm */
#define RF5_BALL_CONFU     0x00000080  /* Confusion Ball -> Conf Storm */
#define RF5_BALL_SOUND     0x00000100  /* Sound Ball -> Sound Storm */
#define RF5_BALL_SHARD     0x00000200  /* Shard Ball -> Shard Storm */
#define RF5_BALL_WIND      0x00000400  /* Wind Ball -> Cyclone */
#define RF5_BALL_STORM     0x00000800  /* Storm Ball -> Tempest */
#define RF5_BALL_NETHR     0x00001000  /* Nether Ball -> Nether Storm */
#define RF5_BALL_CHAOS     0x00002000  /* Chaos Ball -> Chaos Storm */
#define RF5_BALL_MANA      0x00004000  /* Mana Ball -> Mana Storm */
#define RF5_BALL_WATER     0x00008000  /* Water Ball */
#define RF5_BOLT_ACID      0x00010000  /* Acid Bolt */
#define RF5_BOLT_ELEC      0x00020000  /* Elec Bolt */
#define RF5_BOLT_FIRE      0x00040000  /* Fire Bolt */
#define RF5_BOLT_COLD      0x00080000  /* Cold Bolt */
#define RF5_BOLT_POIS      0x00100000  /* Poison Bolt */
#define RF5_BOLT_PLAS      0x00200000  /* Plasma Bolt */
#define RF5_BOLT_ICE       0x00400000  /* Ice Bolt */
#define RF5_BOLT_WATER     0x00800000  /* Water Bolt */
#define RF5_BOLT_NETHR     0x01000000  /* Nether Bolt */
#define RF5_BOLT_MANA      0x02000000  /* Magic Missile -> Mana Bolt */
#define RF5_RF5_XXX3       0x04000000  /* Unused */
#define RF5_BEAM_ELEC      0x08000000  /* Electric spark */
#define RF5_BEAM_ICE       0x10000000  /* Ice Lance */
#define RF5_BEAM_NETHR     0x20000000  /* Spear of Nether */
#define RF5_RF5_XXX4	   0x40000000  /* Unused */
#define RF5_HOLY_ORB       0x80000000  /* Orb of Draining */

#define RF5_OFFENSE	   0xffffffff	/* All RF5_xxx spells are offensive */

/*
 * Monster racial flags - help self, hinder character, and special magics
 */

#define RF6_HASTE          0x00000001  /* Speed self */
#define RF6_ADD_MANA       0x00000002  /* Regain Mana */
#define RF6_HEAL           0x00000004  /* Heal self */
#define RF6_CURE           0x00000008  /* Cure self */
#define RF6_BLINK          0x00000010  /* Teleport Short */
#define RF6_TPORT          0x00000020  /* Teleport Long */
#define RF6_RF6XXX1        0x00000040  /*  */
#define RF6_TELE_SELF_TO   0x00000080  /* Teleport Selt to Player */
#define RF6_TELE_TO        0x00000100  /* Move player to monster */
#define RF6_TELE_AWAY      0x00000200  /* Move player far away */
#define RF6_TELE_LEVEL     0x00000400  /* Move player vertically */
#define RF6_RF6XXX2        0x00000800  /*  */
#define RF6_DARKNESS       0x00001000  /* Create Darkness */
#define RF6_TRAPS          0x00002000  /* Create Traps */
#define RF6_FORGET         0x00004000  /* Cause amnesia */
#define RF6_DRAIN_MANA     0x00008000  /* Drain Mana */
#define RF6_RF6XXX4        0x00010000  /*  */
#define RF6_RF6XXX5        0x00020000  /*  */
#define RF6_MIND_BLAST     0x00040000  /* Blast Mind --> Brain Smash */
#define RF6_BRAIN_SMASH    0x00080000  /* */
#define RF6_WOUND          0x00100000  /* Cause Wounds */
#define RF6_RF6XXX6        0x00200000  /*  */
#define RF6_RF6XXX7        0x00400000  /*  */
#define RF6_RF6XXX8        0x00800000  /*  */
#define RF6_RF6XXX9        0x01000000  /*  */
#define RF6_HUNGER         0x02000000  /* Make Player Hungry */
#define RF6_RF6XX11        0x04000000  /*  */
#define RF6_SCARE          0x08000000  /* Frighten Player */
#define RF6_BLIND          0x10000000  /* Blind Player */
#define RF6_CONF           0x20000000  /* Confuse Player */
#define RF6_SLOW           0x40000000  /* Slow Player */
#define RF6_HOLD           0x80000000  /* Paralyze Player */

/* RF6 uber-flags to cover multiple spells */
#define RF6_TELEPORT	(RF6_BLINK | RF6_TPORT | RF6_TELE_SELF_TO | RF6_TELE_TO | RF6_TELE_AWAY | RF6_TELE_LEVEL)
#define RF6_SAVING	(RF6_DARKNESS | RF6_TRAPS | RF6_FORGET | RF6_MIND_BLAST | RF6_BRAIN_SMASH | \
		RF6_WOUND | RF6_HUNGER | RF6_SCARE | RF6_BLIND | RF6_CONF | RF6_SLOW | RF6_HOLD)

/*
 * Hack -- Bit masks to control what spells are considered
 * Monster racial flags - summons
 */

#define RF7_S_KIN          0x00000001  /* Summon Similar */
#define RF7_S_ELEM_WATER   0x00000002  /* Summon Water Elementals */
#define RF7_S_ELEM_FIRE    0x00000004  /* Summon Fire Elementals */
#define RF7_S_MONSTER      0x00000008  /* Summon Monster */
#define RF7_S_MONSTERS     0x00000010  /* Summon Monsters */
#define RF7_RF7XXX3        0x00000020  /*  */
#define RF7_RF7XXX4        0x00000040  /*  */
#define RF7_RF7XXX5	   0x00000080  /*  */
#define RF7_S_ANT          0x00000100  /* Summon Ants */
#define RF7_S_SPIDER       0x00000200  /* Summon Spiders */
#define RF7_S_HOUND        0x00000400  /* Summon Hounds */
#define RF7_S_ANIMAL       0x00000800  /* Summon Animals */
#define RF7_S_HYDRA        0x00001000  /* Summon Hydras */
#define RF7_S_ENT          0x00002000  /* Summon Ents */
#define RF7_S_THIEF        0x00004000  /* Summon Thieves */
#define RF7_S_BERTBILLTOM  0x00008000  /* Summon Bert, Bill, and Tom */
#define RF7_RF7XXX8        0x00010000  /* */
#define RF7_S_AINU         0x00020000  /* Summon Ainu */
#define RF7_S_ELEM_AIR     0x00040000  /* Summon Air Elementals */
#define RF7_S_ELEM_EARTH   0x00080000  /* Summon Earth Elementals */
#define RF7_S_DRAGON       0x00100000  /* Summon Dragon */
#define RF7_S_HI_DRAGON    0x00200000  /* Summon Ancient Dragons */
#define RF7_RF7XX12        0x00400000  /*  */
#define RF7_RF7XX13        0x00800000  /*  */
#define RF7_S_DEMON        0x01000000  /* Summon Demon(s) */
#define RF7_S_HI_DEMON     0x02000000  /* Summon Greater Demons */
#define RF7_RF7XX14        0x04000000  /*  */
#define RF7_RF7XX15        0x08000000  /*  */
#define RF7_S_UNDEAD       0x10000000  /* Summon Undead */
#define RF7_S_HI_UNDEAD    0x20000000  /* Summon Greater Undead */
#define RF7_S_WRAITH       0x40000000  /* Summon Unique Wraith */
#define RF7_S_UNIQUE       0x80000000  /* Summon Unique Monster */

/* RF7 uber-flags to cover multiple summons */
#define RF7_SUMM	(RF7_S_MONSTER | RF7_S_BERTBILLTOM | RF7_S_AINU | RF7_S_DRAGON | \
		RF7_S_DEMON | RF7_S_UNDEAD)
#define RF7_SUMM_MANY	~(RF7_SUMM)	/* Generally, all other summoning spells summon >1 monster */

/*
 * Monster alignment
 */
#define AL_HOSTILE	0x01  /* Hostile Neutral */
#define AL_PET		0x02  /* Pet Neutral */
#define AL_HOSTILE_L	0x04  /* Hostile Lawful */
#define AL_PET_L	0x08  /* Pet Lawful */
#define AL_HOSTILE_C	0x10  /* Hostile Chaotic */
#define AL_PET_C	0x20  /* Pet Chaotic */

/* Masks */
#define AL_PET_MASK	(AL_PET | AL_PET_L | AL_PET_C)
#define AL_HOSTILE_MASK	(AL_HOSTILE | AL_HOSTILE_L | AL_HOSTILE_C)
#define AL_NEUTRAL	(AL_HOSTILE | AL_PET)
#define AL_LAWFUL	(AL_HOSTILE_L | AL_PET_L)
#define AL_CHAOTIC	(AL_HOSTILE_C | AL_PET_C)

/*
 * Some flags are obvious
 */

#define RF1_OBVIOUS_MASK \
	(RF1_UNIQUE | RF1_QUESTOR | RF1_MALE | RF1_FEMALE | \
	 RF1_FRIEND | RF1_FRIENDS | RF1_ESCORT | RF1_ESCORTS)

/*
 * "race" flags
 */

#define RF3_RACE_MASK \
	(RF3_ORC | RF3_TROLL | RF3_GIANT | RF3_DRAGON | \
	 RF3_DEMON | RF3_UNDEAD | RF3_EVIL | RF3_ANIMAL | RF3_LAWFUL | RF3_CHAOTIC)

 /*
  * Hack -- Bit masks to control what spells are considered
  */


 /*
  * Attack spells.
  * Need special treatment in movement AI.
  */
#define RF4_ATTACK_MASK \
        (RF4_BOULDER | RF4_SHOT | RF4_ARROW | RF4_BOLT | RF4_MISSL | RF4_PMISSL)

#define RF5_ATTACK_MASK \
	(RF5_BALL_ACID | RF5_BALL_ELEC | RF5_BALL_FIRE | RF5_BALL_COLD | \
	 RF5_BALL_POIS | RF5_BALL_LITE | RF5_BALL_DARK | RF5_BALL_CONFU | \
	 RF5_BALL_SOUND | RF5_BALL_SHARD | RF5_BALL_WIND | RF5_BALL_STORM | RF5_BALL_NETHR | \
	 RF5_BALL_CHAOS | RF5_BALL_MANA | RF5_BALL_WATER | RF5_BOLT_ACID | RF5_BOLT_ELEC | \
	 RF5_BOLT_FIRE | RF5_BOLT_COLD | RF5_BOLT_POIS | RF5_BOLT_PLAS | \
	 RF5_BOLT_ICE | RF5_BOLT_WATER | RF5_BOLT_NETHR | RF5_BOLT_MANA | \
	 RF5_BEAM_ELEC | RF5_BEAM_ICE | RF5_BEAM_NETHR | RF5_HOLY_ORB)

#define RF6_ATTACK_MASK (RF6_WOUND | RF6_BRAIN_SMASH | RF6_MIND_BLAST)

#define RF7_ATTACK_MASK (0L)


/*
 * Breath attacks.
 * Need special treatment in movement AI.
 */

#define RF4_BREATH_MASK \
        (RF4_BRTH_ACID | RF4_BRTH_ELEC | RF4_BRTH_FIRE | RF4_BRTH_COLD | \
         RF4_BRTH_POIS | RF4_BRTH_PLAS | RF4_BRTH_LITE | RF4_BRTH_DARK | \
         RF4_BRTH_CONFU | RF4_BRTH_SOUND | RF4_BRTH_SHARD | RF4_BRTH_INER | \
         RF4_BRTH_GRAV | RF4_BRTH_FORCE | RF4_BRTH_NEXUS | RF4_BRTH_NETHR | \
         RF4_BRTH_CHAOS | RF4_BRTH_DISEN | RF4_BRTH_TIME | RF4_BRTH_WIND | \
         RF4_BRTH_MANA)

#define RF5_BREATH_MASK \
 	(0L)

#define RF6_BREATH_MASK \
	(0L)

#define RF7_BREATH_MASK \
	(0L)

/*
 * Harassment (not direct damage) attacks.
 * Need special treatment in AI.
 */
#define RF4_HARASS_MASK \
        (RF4_SHRIEK)

#define RF5_HARASS_MASK \
 	(0L)

#define RF6_HARASS_MASK \
	(RF6_DARKNESS | RF6_TRAPS | RF6_FORGET | RF6_HUNGER | RF6_DRAIN_MANA | \
         RF6_SCARE | RF6_BLIND | RF6_CONF | RF6_SLOW | RF6_HOLD)

#define RF7_HARASS_MASK \
	(0L)

/* Number of times harassment spells get special treatment */
#define BASE_HARASS 4
#define LOW_HARASS  2

/*
 * Hack -- "bolt" spells that may hurt fellow monsters
 * Need special treatment in AI.
 */
#define RF4_BOLT_MASK \
        (RF4_ARROW | RF4_BOLT | RF4_SHOT | RF4_MISSL | RF4_PMISSL | RF4_BOULDER)

#define RF5_BOLT_MASK \
	(RF5_BOLT_ACID | RF5_BOLT_ELEC | RF5_BOLT_FIRE | RF5_BOLT_COLD | \
	 RF5_BOLT_POIS | RF5_BOLT_NETHR | RF5_BOLT_WATER | RF5_BOLT_MANA | \
	 RF5_BOLT_PLAS | RF5_BOLT_ICE | RF5_BEAM_ELEC | RF5_BEAM_ICE | RF5_BEAM_NETHR)

#define RF6_BOLT_MASK \
   0L


#define RF7_BOLT_MASK \
   0L

 /*
 * Archery attacks
 * Need special treatment in AI.
  */
#define RF4_ARCHERY_MASK \
        (RF4_ARROW | RF4_BOLT | RF4_SHOT | RF4_MISSL | RF4_PMISSL | RF4_BOULDER)

#define RF5_ARCHERY_MASK \
	(0L)

#define RF6_ARCHERY_MASK \
	(0L)

#define RF7_ARCHERY_MASK \
	(0L)

/*
 * Spells that can be can cast without a player in sight
 * Need special treatment in AI.
 */
#define RF4_NO_PLAYER_MASK \
        (0L)

#define RF5_NO_PLAYER_MASK \
        (0L)

#define RF6_NO_PLAYER_MASK \
        (RF6_HEAL | RF6_ADD_MANA | RF6_TELE_SELF_TO | RF6_CURE)

#define RF7_NO_PLAYER_MASK \
        (0L)


/* Spell Desire Table Columns */
enum
{
        D_BASE,
        D_SUMM,
        D_HURT,
        D_MANA,
        D_ESC,
        D_TACT,
        D_RES,
        D_RANGE
};

/*
 * Monsters will run up to 25 grids away
 */
#define FLEE_RANGE      MAX_SIGHT + 5

/*
 * Hack -- Panicked monsters will run far, far away
 */
#define PANIC_RANGE      100

/*
 * Spells that improve the caster's tactical position
 */
#define RF4_TACTIC_MASK \
	(0L)

#define RF5_TACTIC_MASK \
	(0L)

#define RF6_TACTIC_MASK \
	(RF6_BLINK)

/*
 * Annoying spells
 */
#define RF4_ANNOY_MASK \
	(RF4_SHRIEK)

#define RF5_ANNOY_MASK \
	(RF5_DRAIN_MANA | RF5_SCARE | \
	 RF5_BLIND | RF5_CONF | RF5_SLOW | RF5_HOLD)

#define RF6_ANNOY_MASK \
	(RF6_TELE_TO | RF6_DARKNESS | RF6_TRAPS | RF6_FORGET)

/*
 * Spells that increase the caster's relative speed
 */
#define RF4_HASTE_MASK \
	(0L)

#define RF5_HASTE_MASK \
	(RF5_SLOW | RF5_HOLD)

#define RF6_HASTE_MASK \
	(RF6_HASTE)

/*
 * Healing spells
 */
#define RF4_HEAL_MASK \
	(0L)

#define RF5_HEAL_MASK \
	(0L)

#define RF6_HEAL_MASK \
	(RF6_HEAL)

/*
 * Innate spell-like effects
 */
#define RF4_INNATE_MASK \
	(0L)

#define RF5_INNATE_MASK \
	(0L)

#define RF6_INNATE_MASK \
	(0L)

/* Max number of body powers */
#define PWR_BODY_MAX		22

/*
 * Monster macros
 */

/*
 * Determine if a given monster is "non-living"
 *
 * Demons are not alive.  Neither vortexes nor elementals are alive.  
 * Storms and events are not alive.  Golems are not alive.  Some mimics
 * are assumed to be "animated" object, and therefore not alive; others
 * are living creatures that look like something non-living.  Oh,
 * and undeads are not alive, too. 
 */
#define monster_nonliving(M) \
	(((M)->flags3 & (RF3_DEMON))  || \
	 ((M)->flags3 & (RF3_UNDEAD)) || \
	 (strchr("Evg$+~-_", (M)->d_char)))

/*
 * Determine if the monster is fully visible.
 */
#define mon_fully_visible(M) \
(((M)->ml) && !((M)->mflag & (MFLAG_MIMIC | MFLAG_DLIM)))

/*
 * Monster experience. Borrowed from ToME. Slightly modified (x6 -> x2)
 * (alpha2: x2 -> x5)
 */
#define MONSTER_LEVEL_MAX       150
#define MONSTER_EXP(level)      ((((level) > MONSTER_LEVEL_MAX)?MONSTER_LEVEL_MAX:(level)) * \
				 (((level) > MONSTER_LEVEL_MAX)?MONSTER_LEVEL_MAX:(level)) * \
				 (((level) > MONSTER_LEVEL_MAX)?MONSTER_LEVEL_MAX:(level)) * 5)

#define OFFENSE_ONE		2500	/* One attack on pet */
#define OFFENSE_WARNING		5000	/* Warning issued */
#define OFFENSE_CRITICAL	8500	/* Pet turns hostile */


/**** Monster types ****/
    
/*
 * Monster blow structure
 *
 *	- Method (RBM_*)
 *	- Effect (RBE_*)
 *	- Damage Dice
 *	- Damage Sides
 */
struct monster_blow
{
	byte method;
	byte effect;
	byte d_dice;
	byte d_side;
};

/*
 * Monster "power" information
 */
struct monster_power
{
	byte level;
	byte sp;
	byte type;
	byte fail;
        s16b extra;
};
 
/*
 * Monster "body" information.
 *
 * Used by monster races
 */
struct monster_body
{
	byte base_level;			/* Min. body level */
	byte evolve_level;			/* Evolution level */
	u16b evolve_index[6];			/* Evolution race */

	byte class;				/* "Emulated" class */
	
	s16b to_stat[6];			/* Stat bonuses */
	s16b to_ac_base, to_ac_max;             /* Base and max natural armor */
	s16b to_spd_base, to_spd_max;		/* Base and max speed */
        s16b dis, dev, sav, stl, srh, fos;	/* Abilites */
	s16b thn_bare, thn_edged, thn_hafted, thn_polearm, thn_axe; /* Fighting (melee) */
	s16b thb_sling, thb_bow, thb_xbow, tht;	/* Fighting (ranged) */
	byte hitdie;				/* Hit die */
	byte infra;				/* Infravision */
	byte pwrstat;				/* Powers stat */

	/* "Masks" for various inventory slots */
	u32b weapon_mask, bow_mask, ring_mask, amulet_mask, light_mask,
	     body_mask, cloak_mask, shield_mask, helm_mask, glove_mask, boot_mask;
	u32b slot_mask;				/* "Uber"-mask */
	u32b flags1, flags2, flags3, flags4;	/* Racial flags */
	
	s32b weight;				/* Corpse base weight */
	/* XXX Should store this somewhere else */
	char corpsename[25];			/* Corpse base name */
	
	monster_power pwr[PWR_BODY_MAX];	/* Body powers */
};

/*
 * Monster "race" information, including racial memories
 *
 * Note that "d_attr" and "d_char" are used for MORE than "visual" stuff.
 *
 * Note that "x_attr" and "x_char" are used ONLY for "visual" stuff.
 *
 * Note that "cur_num" (and "max_num") represent the number of monsters
 * of the given race currently on (and allowed on) the current level.
 * This information yields the "dead" flag for Unique monsters.
 *
 * Note that "max_num" is reset when a new player is created.
 * Note that "cur_num" is reset when a new level is created.
 *
 * Maybe "x_attr", "x_char", "cur_num", and "max_num" should
 * be moved out of this array since they are not read from
 * "monster.txt".
 */
struct monster_race
{
	u32b name;				/* Name (offset) */
	u32b text;				/* Text (offset) */

	byte hdice;				/* Creatures hit dice count */
	byte hside;				/* Creatures hit dice sides */

	s16b ac;				/* Armour Class */

	s16b sleep;				/* Inactive counter (base) */
	byte aaf;				/* Area affect radius (1-100) */
	byte speed;				/* Speed (normally 110) */

	s32b mexp;				/* Exp value for kill */

	u16b extra;				/* Unique, which must be dead before
						 * that monster can be generated */

	byte freq_ranged;		/* Ranged attack frequency */
	byte mana;				/* Max mana */
	byte spell_power;		/* Power of (damage-dealing) spells */
	byte unused;        	/* Not currently used */


	u32b flags1;			/* Flags 1 (general) */
	u32b flags2;			/* Flags 2 (abilities) */
	u32b flags3;			/* Flags 3 (race/resist) */
	u32b flags4;			/* Flags 4 (inate/breath) */
	u32b flags5;			/* Flags 5 (normal spells) */
	u32b flags6;			/* Flags 6 (special spells) */
	u32b flags7;			/* Flags 7 (summon spells) */

	monster_blow blow[MONSTER_BLOW_MAX]; /* Up to four blows per round */

	monster_body body;		/* Body */
	
	s16b artifact;			/* Unique artifact */

	byte level;				/* Level of creature */
	byte rarity;			/* Rarity of creature */

	byte d_attr;			/* Default monster attribute */
	char d_char;			/* Default monster character */

	byte x_attr;			/* Desired monster attribute */
	char x_char;			/* Desired monster character */

	byte max_num;			/* Maximum population allowed per level */
	byte cur_num;			/* Monster population on current level */
};

/*
 * Monster "lore" information
 *
 * Note that these fields are related to the "monster recall" and can
 * be scrapped if space becomes an issue, resulting in less "complete"
 * monster recall (no knowledge of spells, etc). XXX XXX XXX
 *
 * ToDo: The "r_" prefix is no longer needed and should be removed.
 * Huh? -AU
 */
struct monster_lore
{
	s16b sights;			/* Count sightings of this monster */
	s16b deaths;			/* Count deaths from this monster */

	s16b pkills;			/* Count monsters killed in this life */
	s16b tkills;			/* Count monsters killed in all lives */

	byte wake;				/* Number of times woken up (?) */
	byte ignore;			/* Number of times ignored (?) */

	byte xtra1;				/* Something (unused) */
	byte xtra2;				/* Something (unused) */

	byte drop_gold;			/* Max number of gold dropped at once */
	byte drop_item;			/* Max number of item dropped at once */

	byte ranged;			/* Observed ranged attacks */
	byte mana;				/* Max mana */
	byte spell_power;		/* Power of (damage-dealing) spells */

	byte xtra3;				/* Something (unused) */

	byte blows[MONSTER_BLOW_MAX]; /* Number of times each blow type was seen */

	u32b flags1;			/* Observed racial flags */
	u32b flags2;			/* Observed racial flags */
	u32b flags3;			/* Observed racial flags */
	u32b flags4;			/* Observed racial flags */
	u32b flags5;			/* Observed racial flags */
	u32b flags6;			/* Observed racial flags */
	u32b flags7;			/* Observed racial flags */
};

/*
 * Monster information, for a specific monster.
 *
 * Note: fy, fx constrain dungeon size to 256x256
 *
 * The "hold_o_idx" field points to the first object of a stack
 * of objects (if any) being carried by the monster (see above).
 */
struct monster_type
{
	s16b r_idx;			/* Monster race index */

	byte fy;			/* Y location on map */
	byte fx;			/* X location on map */

	s16b hp;			/* Current Hit points */
	s16b maxhp;			/* Max Hit points */
	
	s16b ac;			/* AC */

	s16b csleep;		/* Inactive counter */

	byte mspeed;		/* Monster "speed" */
	s16b energy;		/* Monster "energy" */

	byte stunned;		/* Monster is stunned */
	byte confused;		/* Monster is confused */
	byte blinded;		/* Monster is blinded */
	byte monfear;		/* Monster is afraid */

	byte cdis;			/* Current dis from player */

	u32b mflag;			/* Extra monster flags */

	bool ml;			/* Monster is "visible" */

	s16b hold_o_idx;	/* Object being held (if any) */


	u32b smart;			/* Field for "smart_learn" */

	s16b target;
	byte ty;			/* Monster target */
	byte tx;

	/* Harrassment spells are more likely early in a battle */
	byte harass;

	byte min_range;		/* What is the closest we want to be? */  /* Not saved */
	byte best_range;	/* How close do we want to be? */  /* Not saved */

	s16b mana;          /* Current mana level */
	s16b maxmana;		/* Maximum mana level */
	s16b spell_power;	/* Spell power */

	s16b mimic_k_idx;	/*type of mimic code*/

	byte align;		/* Alignment */

	byte level;		/* Level */
	u32b exp;		/* Experience */
	
	u16b off;	/* Offense */
	
	u16b timeout;		/* Time left before the monster disappears */
};


/**** Monster variables ****/

/* Spell/ranged attacks-related tables */
extern byte mana_cost_RF4[32];
extern byte mana_cost_RF5[32];
extern byte mana_cost_RF6[32];
extern byte mana_cost_RF7[32];
extern byte spell_desire_RF4[32][8];
extern byte spell_desire_RF5[32][8];
extern byte spell_desire_RF6[32][8];
extern byte spell_desire_RF7[32][8];
extern byte spell_range_RF4[32];
extern byte spell_range_RF5[32];
extern byte spell_range_RF6[32];
extern byte spell_range_RF7[32];

extern s16b mon_max;
extern s16b mon_cnt;

extern byte (*cave_cost)[MAX_DUNGEON_WID];
extern byte (*cave_when)[MAX_DUNGEON_WID];
extern int scent_when;
extern int flow_center_y;
extern int flow_center_x;
extern int update_center_y;
extern int update_center_x;
extern int cost_at_center;

extern monster_type *mon_list;
extern monster_lore *l_list;
extern monster_race *r_info;
extern char *r_name;
extern char *r_text;

/* Hacks. Should be removed. */
extern bool mon_mon_nonlos_combat_msg;
extern bool summon_pets_hack;


/**** Monster functions ****/

/* mon-attack.c */
bool make_attack_normal(monster_type *m_ptr);
bool make_attack_normal_mon(monster_type *m_ptr, monster_type *t_ptr);
bool make_attack_ranged(monster_type *m_ptr, int attack);
bool make_attack_ranged_mon(monster_type *m_ptr, monster_type *t_ptr, int attack);
void mon_cloud(int m_idx, int typ, int dam, int rad);
void cloud_surround(int r_idx, int *typ, int *dam, int *rad);

/* mon-ai.c */
bool sees_player_mon(monster_type *m_ptr);
int get_scent(int y, int x);
int monster_pass_non_wall(int r_idx, int y, int x, int move_chance);
bool cave_exist_mon(monster_race *r_ptr, int y, int x, bool occupied_ok, bool can_dig);
void acquire_target(monster_type *m_ptr);
void process_monsters(s16b minimum_energy);

/* mon-info.c */
void describe_monster(int r_idx, bool spoilers);
void roff_top(int r_idx);
void screen_roff(monster_type *m_ptr, int r_idx);
void display_roff(int r_idx);

/* mon-misc.c */
s16b poly_r_idx(int r_idx);
void delete_monster_idx(int i);
void delete_monster(int y, int x);
void compact_monsters(int size);
void wipe_mon_list(void);
s16b mon_pop(void);
errr get_mon_num_prep(void);
s16b get_mon_num(int level);
void display_monlist(void);
void monster_desc(char *desc, size_t max, const monster_type *m_ptr, int mode);
void monster_desc_race(char *desc, size_t max, int r_idx);
void lore_do_probe(int m_idx);
void lore_treasure(int m_idx, int num_item, int num_gold);
void update_mon(int m_idx, bool full);
void update_monsters(bool full);
s16b monster_carry(int m_idx, object_type *j_ptr);
void monster_swap(int y1, int x1, int y2, int x2);
s16b player_place(int y, int x);
s16b monster_place(int y, int x, monster_type *n_ptr);
bool place_monster_one(int y, int x, int r_idx, bool slp, int nlev);
bool place_monster_group(int y, int x, int r_idx, bool slp, s16b group_size);
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp);
bool place_monster(int y, int x, bool slp, bool grp);
bool alloc_monster(int dis, bool slp);
void set_mon_fear(monster_type *m_ptr, int v, bool panic);
bool multiply_monster(int m_idx);
void message_pain(int m_idx, int dam);
void update_smart_learn(int m_idx, int what);
int get_coin_type(const monster_race *r_ptr);
void monster_death(int m_idx, int who);
bool mon_hurt_misc(monster_type *m_ptr, int dam);
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note, int who);
void ang_sort_aux(void *u, void *v, int p, int q);
void ang_sort(void *u, void *v, int n);

/* mon-summ.c */
bool summon_specific(int y1, int x1, int lev, int type);
bool summon_specific_pet(int y1, int x1, int lev, int type);

#endif /* MONSTER_H_INCLUDED */
