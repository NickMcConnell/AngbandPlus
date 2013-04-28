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

/* player.h: player-related definitions */

#ifndef PLAYER_H_INCLUDED
#define PLAYER_H_INCLUDED

/*
 * Misc constants
 */
#define TOWN_DAWN	10000		/* Number of turns from dawn to dawn XXX */
#define BREAK_GLYPH	400		/* Rune of protection resistance */
#define BTH_PLUS_ADJ    3       	/* Adjust BTH per plus-to-hit */
#define MON_MULT_ADJ	8		/* High value slows multiplication */
#define QUEST_TURNS	1200		/* Number of turns between quest failure checks */
#define MON_DRAIN_LIFE	2		/* Percent of player exp drained per hit */
#define USE_DEVICE      3		/* x> Harder devices x< Easier devices */

/*
 * Maximum number of player "sex" types (see "table.c", etc)
 */
#define MAX_SEXES            2

/*
 * Player sex constants (hard-coded by save-files, arrays, etc)
 */
enum
{
        SEX_FEMALE,
        SEX_MALE,
};

/*
 * Percentage of maximum noise you can make just walking around, given a
 * stealth of zero.
 */
#define WAKEUP_ADJ      20

/*
 * More maximum values
 */
#define MAX_SIGHT	20	/* Maximum view distance */
#define MAX_RANGE	20	/* Maximum range (spells, etc) */

/*
 * Player constants
 */
#define PY_MAX_EXP	99999999L	/* Maximum exp */
#define PY_MAX_GOLD	999999999L	/* Maximum gold */
#define PY_MAX_LEVEL	50		/* Maximum level */

/*
 * Player "food" crucial values
 */
#define PY_FOOD_MAX	15000		/* Food value (Bloated) */
#define PY_FOOD_FULL	10000		/* Food value (Normal) */
#define PY_FOOD_ALERT	2000		/* Food value (Hungry) */
#define PY_FOOD_WEAK	1000		/* Food value (Weak) */
#define PY_FOOD_FAINT	500		/* Food value (Fainting) */
#define PY_FOOD_STARVE	100		/* Food value (Starving) */

/*
 * Player regeneration constants
 */
#define PY_REGEN_NORMAL		197	/* Regen factor*2^16 when full */
#define PY_REGEN_WEAK		98	/* Regen factor*2^16 when weak */
#define PY_REGEN_FAINT		33	/* Regen factor*2^16 when fainting */
#define PY_REGEN_HPBASE		1442	/* Min amount hp regen*2^16 */
#define PY_REGEN_MNBASE		524	/* Min amount mana regen*2^16 */


/*
 * Maximum number of players spells
 */
#define PY_MAX_SPELLS		64

/*
 * Spells
 */
enum
{
        SPELL_MAGIC_MISSILE,
        SPELL_DETECT_MONSTERS,
        SPELL_PHASE_DOOR,
        SPELL_LIGHT_AREA,
        SPELL_FIND_TRAPS_DOORS,
        SPELL_CURE_LIGHT_WOUNDS,
        SPELL_TREASURE_DETECTION,
        SPELL_OBJECT_DETECTION,
        SPELL_IDENTIFY,
        SPELL_DETECT_INVISIBLE,
        SPELL_DETECT_ENCHANTMENT,
        SPELL_STINKING_CLOUD,
        SPELL_LIGHTNING_BOLT,
        SPELL_CONFUSE_MONSTER,
        SPELL_SLEEP_MONSTER,
        SPELL_WONDER,
        SPELL_FROST_BOLT,
        SPELL_ACID_BOLT,
        SPELL_FIRE_BOLT,
        SPELL_TRAP_DOOR_DESTRUCTION,
        SPELL_SPEAR_OF_LIGHT,
        SPELL_TURN_STONE_TO_MUD,
        SPELL_DOOR_CREATION,
        SPELL_EARTHQUAKE,
        SPELL_STAIR_CREATION,
        SPELL_CURE_POISON,
        SPELL_SATISFY_HUNGER,
        SPELL_HEROISM,
        SPELL_BERSERKER,
        SPELL_HASTE_SELF,
        SPELL_TELEPORT_SELF,
        SPELL_SLOW_MONSTER,
        SPELL_TELEPORT_OTHER,
        SPELL_TELEPORT_LEVEL,
        SPELL_WORD_OF_RECALL,
        SPELL_POLYMORPH_OTHER,
        SPELL_SHOCK_WAVE,
        SPELL_EXPLOSION,
        SPELL_CLOUD_KILL,
        SPELL_MASS_SLEEP,
        SPELL_BEDLAM,
        SPELL_REND_SOUL,
        SPELL_WORD_OF_DESTRUCTION,
        SPELL_CHAOS_STRIKE,
        SPELL_RESIST_COLD,
        SPELL_RESIST_FIRE,
        SPELL_RESIST_POISON,
        SPELL_RESISTANCE,
        SPELL_SHIELD,
        SPELL_RUNE_OF_PROTECTION,
        SPELL_RECHARGE_ITEM_I,
        SPELL_ENCHANT_ARMOR,
        SPELL_ENCHANT_WEAPON,
        SPELL_RECHARGE_ITEM_II,
        SPELL_ELEMENTAL_BRAND,
        SPELL_FROST_BALL,
        SPELL_ACID_BALL,
        SPELL_FIRE_BALL,
        SPELL_ICE_STORM,
        SPELL_BANISHMENT,
        SPELL_METEOR_SWARM,
        SPELL_MASS_BANISHMENT,
        SPELL_RIFT,
        SPELL_MANA_STORM
};

/*
 * Prayers
 */
enum
{
	/* Beginners Handbook */
        PRAYER_DETECT_EVIL,
        PRAYER_CURE_LIGHT_WOUNDS,
        PRAYER_BLESS,
        PRAYER_REMOVE_FEAR,
        PRAYER_CALL_LIGHT,
        PRAYER_FIND_TRAPS_DOORS_STAIRS,
        PRAYER_BOLT_OF_DRAINING,
        PRAYER_SLOW_POISON,
	/* Words of Wisdom */
        PRAYER_SCARE_MONSTER,
        PRAYER_PORTAL,
        PRAYER_CURE_SERIOUS_WOUNDS,
        PRAYER_CHANT,
        PRAYER_SANCTUARY,
        PRAYER_SATISFY_HUNGER,
        PRAYER_REMOVE_CURSE,
        PRAYER_RESIST_HEAT_COLD,
	/* Chants and Blessings */
        PRAYER_NEUTRALIZE_POISON,
        PRAYER_ORB_OF_DRAINING,
        PRAYER_CURE_CRITICAL_WOUNDS,
        PRAYER_SENSE_INVISIBLE,
        PRAYER_PROTECTION_FROM_EVIL,
        PRAYER_EARTHQUAKE,
        PRAYER_SENSE_SURROUNDINGS,
        PRAYER_CURE_MORTAL_WOUNDS,
        PRAYER_TURN_UNDEAD,
	/* Exorcism and Dispelling */
        PRAYER_PRAYER,
        PRAYER_DISPEL_UNDEAD,
        PRAYER_HEAL,
        PRAYER_DISPEL_EVIL,
        PRAYER_GLYPH_OF_WARDING,
        PRAYER_HOLY_WORD,
	/* Godly Insights */
        PRAYER_DETECT_MONSTERS,
        PRAYER_DETECTION,
        PRAYER_PERCEPTION,
        PRAYER_PROBING,
        PRAYER_CLAIRVOYANCE,
	/* Purifications and Healing */
        PRAYER_CURE_SERIOUS_WOUNDS2,
        PRAYER_CURE_MORTAL_WOUNDS2,
        PRAYER_HEALING,
        PRAYER_RESTORATION,
        PRAYER_REMEMBRANCE,
	/* Wrath of God */
        PRAYER_DISPEL_UNDEAD2,
        PRAYER_DISPEL_EVIL2,
        PRAYER_BANISH_EVIL,
        PRAYER_WORD_OF_DESTRUCTION,
        PRAYER_ANNIHILATION,
	/* Holy Infusions */
        PRAYER_UNBARRING_WAYS,
        PRAYER_RECHARGING,
        PRAYER_DISPEL_CURSE,
        PRAYER_ENCHANT_WEAPON,
        PRAYER_ENCHANT_ARMOUR,
        PRAYER_ELEMENTAL_BRAND,
	/* Ethereal openings */
        PRAYER_BLINK,
        PRAYER_TELEPORT_SELF,
        PRAYER_TELEPORT_OTHER,
        PRAYER_TELEPORT_LEVEL,
        PRAYER_WORD_OF_RECALL,
        PRAYER_ALTER_REALITY
};

/* For defining spellbooks */
#define BOOK1(x) (((x) < 0) ? 0 : (x) < 32 ? (1L << (x)) : 0)
#define BOOK2(x) (((x) < 0) ? 0 : (x) < 32 ? 0 : (1L << ((x) % 32)))

#define BOOK(a, b, c, d, e, f, g, h, i) \
{ \
	(BOOK1(a) | BOOK1(b) | BOOK1(c) | BOOK1(d) | BOOK1(e) | \
	 BOOK1(f) | BOOK1(g) | BOOK1(h) | BOOK1(i)), \
	(BOOK2(a) | BOOK2(b) | BOOK2(c) | BOOK2(d) | BOOK2(e) | \
	 BOOK2(f) | BOOK2(g) | BOOK2(h) | BOOK2(i)) \
}

/*
 * Flags for player_type.spell_flags[]
 */
#define PY_SPELL_LEARNED    0x01 /* Spell has been learned */
#define PY_SPELL_WORKED     0x02 /* Spell has been successfully tried */
#define PY_SPELL_FORGOTTEN  0x04 /* Spell has been forgotten */

/*
 * Maximum number of "normal" pack slots, and the index of the "overflow"
 * slot, which can hold an item, but only temporarily, since it causes the
 * pack to "overflow", dropping the "last" item onto the ground.  Since this
 * value is used as an actual slot, it must be less than "INVEN_WIELD" (below).
 * Note that "INVEN_PACK" is probably hard-coded by its use in savefiles, and
 * by the fact that the screen can only show 23 items plus a one-line prompt.
 */
#define INVEN_PACK		23

/*
 * First equipment slot.
 */
#define INVEN_EQUIP		(INVEN_PACK + 1)
							   
/*
 * Total number of inventory slots.  Hard-coded in savefiles.
 * Individual slots are no longer hard-coded.  Value chosen from
 * the same reasons as INVEN_PACK.
 */
#define INVEN_TOTAL		(INVEN_EQUIP + 23)

/*
 * A service macro
 */
#define EQUIP_SLOT(slot)	(1 << (slot - INVEN_EQUIP))

/*
 * Equipment categories
 */
enum
{
    	EQUIP_WEAPON,
	EQUIP_BOW,
	EQUIP_RING,
	EQUIP_AMULET,
	EQUIP_LIGHT,
	EQUIP_BODY,
	EQUIP_CLOAK,
	EQUIP_SHIELD,
	EQUIP_HELM,
	EQUIP_GLOVE,
	EQUIP_BOOT,
	EQUIP_ANY
};

/*
 * Indexes of the various "stats" (hard-coded by savefiles, etc).
 */
enum
{
        A_STR,
        A_INT,
        A_WIS,
        A_DEX,
        A_CON,
        A_CHR
};

/*
 * Total number of stats.
 */
#define A_MAX	6

/**** "What to do" flags ****/

/*
 * Bit flags for the "p_ptr->notice" variable
 */
#define PN_COMBINE	0x00000001L	/* Combine the pack */
#define PN_REORDER	0x00000002L	/* Reorder the pack */
/* xxx (many) */


/*
 * Bit flags for the "p_ptr->update" variable
 */
#define PU_BONUS	0x00000001L	/* Calculate bonuses */
#define PU_TORCH	0x00000002L	/* Calculate torch radius */
/* xxx (many) */
#define PU_HP		0x00000010L	/* Calculate chp and mhp */
#define PU_MANA		0x00000020L	/* Calculate csp and msp */
#define PU_SPELLS	0x00000040L	/* Calculate spells */
/* xxx (many) */
#define PU_FORGET_VIEW	0x00010000L	/* Forget field of view */
#define PU_UPDATE_VIEW	0x00020000L	/* Update field of view */
/* xxx (many) */
#define PU_MONSTERS	0x10000000L	/* Update monsters */
#define PU_DISTANCE	0x20000000L	/* Update distances */
/* xxx */
#define PU_PANEL	0x80000000L	/* Update panel */

/*
 * Bit flags for the "p_ptr->redraw" variable
 */
#define PR_MISC			0x00000001L	/* Display Race/Class */
#define PR_TITLE		0x00000002L	/* Display Title */
#define PR_LEV			0x00000004L	/* Display Level */
#define PR_EXP			0x00000008L	/* Display Experience */
#define PR_STATS		0x00000010L	/* Display Stats */
#define PR_ARMOR		0x00000020L	/* Display Armor */
#define PR_HP			0x00000040L	/* Display Hitpoints */
#define PR_MANA			0x00000080L	/* Display Mana */
#define PR_GOLD			0x00000100L	/* Display Gold */
#define PR_DEPTH		0x00000200L	/* Display Depth */
#define PR_EQUIPPY		0x00000400L	/* Display Equippy chars */
#define PR_HEALTH		0x00000800L	/* Display Monster Health Bar */
#define PR_CUT			0x00001000L	/* Display Extra (Cut) */
#define PR_STUN			0x00002000L	/* Display Extra (Stun) */
#define PR_HUNGER		0x00004000L	/* Display Extra (Hunger) */
#define PR_MON_MANA		0x00008000L	/* Display Monster Mana Bar */
#define PR_BLIND		0x00010000L	/* Display Extra (Blind) */
#define PR_CONFUSED		0x00020000L	/* Display Extra (Confused) */
#define PR_AFRAID		0x00040000L	/* Display Extra (Afraid) */
#define PR_POISONED		0x00080000L	/* Display Extra (Poisoned) */
#define PR_STATE		0x00100000L	/* Display Extra (State) */
#define PR_SPEED		0x00200000L	/* Display Extra (Speed) */
#define PR_STUDY		0x00400000L	/* Display Extra (Study) */
/* xxx */
#define PR_EXTRA		0x01000000L	/* Display Extra Info */
#define PR_BASIC		0x02000000L	/* Display Basic Info */
/* xxx */
#define PR_MAP			0x08000000L	/* Display Map */
/* xxx (many) */

/*
 * Bit flags for the "p_ptr->window" variable (etc)
 */
#define PW_INVEN            0x00000001L /* Display inven/equip */
#define PW_EQUIP            0x00000002L /* Display equip/inven */
#define PW_PLAYER_0         0x00000004L /* Display player (basic) */
#define PW_PLAYER_1         0x00000008L /* Display player (extra) */
/* xxx */
/* xxx */
#define PW_MESSAGE          0x00000040L /* Display messages */
#define PW_OVERHEAD         0x00000080L /* Display overhead view */
#define PW_MONSTER          0x00000100L /* Display monster recall */
#define PW_OBJECT           0x00000200L /* Display object recall */
#define PW_MONLIST          0x00000400L /* Display monster list */
#define PW_SNAPSHOT         0x00000800L /* Display snap-shot */
#define PW_SCRIPT_VARS      0x00001000L /* Display script variables */
#define PW_SCRIPT_SOURCE    0x00002000L /* Display script source */
#define PW_BORG_1           0x00004000L /* Display borg messages */
#define PW_BORG_2           0x00008000L /* Display borg status */

/**** Class flags ****/

#define CF_EXTRA_SHOT		0x00000001L /* Extra shots when using sling*/
#define CF_BRAVERY_30		0x00000002L /* Gains resist fear at plev 30 */
#define CF_BLESS_WEAPON		0x00000004L /* Requires blessed/hafted weapons */
#define CF_CUMBER_GLOVE		0x00000008L /* Gloves disturb spellcasting */
#define CF_ZERO_FAIL		0x00000010L /* Fail rates can reach 0% */
#define CF_BEAM			0x00000020L /* Higher chance of spells beaming */
#define CF_CHOOSE_SPELLS	0x00000040L /* Allow choice of spells */
#define CF_PSEUDO_ID_HEAVY	0x00000080L /* Allow heavy pseudo-id */
#define CF_PSEUDO_ID_IMPROV	0x00000100L /* Pseudo-id improves quicker with player-level */
#define CF_ROGUE_COMBAT		0x00000200L /* Permits such as steal & critical hit*/
#define CF_EXTRA_ARROW		0x00000400L /* Extra arrow when using bow*/
#define CF_SET_TRAPS		0x00000800L /* Can set traps for monsters to fall into*/
#define CF_EXTRA_ATTACK		0x00001000L /* extra attack at level 26*/

/**** Possession/monster races stuff ****/

enum
{
        PAL_NONE,
        PAL_CHAOS,
        PAL_LAW  /* XXX unused */
};

/*
 * Hack - min. monster race
 */
 
#define MON_RACE_MIN		11

/*
 * Hack - possessor race
 */
#define POSS_RACE		11

/*
 * Hack - possessor class
 */
#define POSS_CLASS		6

/*
 * Hack - monster class
 */
#define MON_CLASS               7

/*
 * Hack - default possessor body
 */
#define POSS_SOUL		633

/*
 * Bit flags for the "get_item" function
 */
#define USE_EQUIP		0x01	/* Allow equip items */
#define USE_INVEN		0x02	/* Allow inven items */
#define USE_FLOOR		0x04	/* Allow floor items */



/**** Player types ****/

/*
 * Player spell
 */
struct magic_type
{
	byte slevel;	/* Required level (to learn) */
	byte smana;	/* Required mana (to cast) */
	byte sfail;	/* Minimum chance of failure */
	byte sexp;	/* Encoded experience bonus */
};

/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 * Hack -- a player with a "spell_book" of "ninety-nine" is a monster!
 */
struct player_magic
{
	magic_type info[PY_MAX_SPELLS];	/* The available spells */
};

/*
 * Player sex info
 */
struct player_sex
{
	cptr title;		/* Type of sex */
	cptr winner;		/* Name of winner */
};

/*
 * Starting equipment entry
 */
struct start_item
{
	byte tval;	/* Item's tval */
	byte sval;	/* Item's sval */
	byte min;	/* Minimum starting amount */
	byte max;	/* Maximum starting amount */
}; 

/*
 * Player racial info
 */
struct player_race
{
	u32b name;		/* Name (offset) */
	u32b text;		/* Text (offset) */

	s16b r_adj[A_MAX];	/* Racial stat bonuses */

	s16b r_dis;		/* disarming */
	s16b r_dev;		/* magic devices */
	s16b r_sav;		/* saving throw */
	s16b r_stl;		/* stealth */
	s16b r_srh;		/* search ability */
	s16b r_fos;		/* search frequency */
	s16b r_thn_bare;	/* to hit (melee/barehanded) */
	s16b r_thn_edged;	/* to hit (melee/edged weapons) */
	s16b r_thn_hafted;	/* to hit (melee/hafted weapons) */
	s16b r_thn_polearm;	/* to hit (melee/polearms) */
	s16b r_thn_axe;		/* to hit (melee/axes) */
	s16b r_thb_sling;	/* to hit (ranged/slings) */
	s16b r_thb_bow;		/* to hit (ranged/bows) */
	s16b r_thb_xbow;	/* to hit (ranged/crossbows) */
	s16b r_tht;		/* to hit (throwing) */

	byte r_mhp;		/* Race hit-dice modifier */
	u16b r_exp;		/* Race experience factor */

	u16b b_age;		/* base age */
	u16b m_age;		/* mod age */

	u16b m_b_ht;		/* base height (males) */
	u16b m_m_ht;		/* mod height (males) */
	u16b m_b_wt;		/* base weight (males) */
	u16b m_m_wt;		/* mod weight (males) */

	u16b f_b_ht;		/* base height (females) */
	u16b f_m_ht;		/* mod height (females) */
	u16b f_b_wt;		/* base weight (females) */
	u16b f_m_wt;		/* mod weight (females) */

	byte infra;		/* Infra-vision	range */
	
	byte comp;		/* Complexity level (0...4, see p_race.txt) */

	byte choice;		/* Legal class choices */

	s16b hist;		/* Starting history index */

	u32b m_r_idx;		/* Monster body index */
	s32b king_r_idx;	/* King Unique index */
	s32b pet_r_idx;		/* Pet index */

	u32b flags1;		/* Racial Flags, set 1 */
	u32b flags2;		/* Racial Flags, set 2 */
	u32b flags3;		/* Racial Flags, set 3 */
	u32b flags4;		/* Racial Flags, set 4 (new) */

	start_item start_items[MAX_START_ITEMS]; /* The starting inventory */
};

/*
 * Player class info
 */
struct player_class
{
	u32b name;		/* Name (offset) */

	u32b title[10];		/* Titles - offset */

	s16b c_adj[A_MAX];	/* Class stat modifier */

	s16b c_dis;		/* class disarming */
	s16b c_dev;		/* class magic devices */
	s16b c_sav;		/* class saving throws */
	s16b c_stl;		/* class stealth */
	s16b c_srh;		/* class searching ability */
	s16b c_fos;		/* class searching frequency */
	s16b c_thn_bare;	/* class to hit (melee/barehanded) */
	s16b c_thn_edged;	/* class to hit (melee/edged weapons) */
	s16b c_thn_hafted;	/* class to hit (melee/hafted weapons) */
	s16b c_thn_polearm;	/* class to hit (melee/polearms) */
	s16b c_thn_axe;		/* class to hit (melee/axes) */
	s16b c_thb_sling;	/* class to hit (ranged/slings) */
	s16b c_thb_bow;		/* class to hit (ranged/bows) */
	s16b c_thb_xbow;	/* class to hit (ranged/crossbows) */
	s16b c_tht;		/* class to hit (throwing) */

	s16b x_dis;		/* extra disarming */
	s16b x_dev;		/* extra magic devices */
	s16b x_sav;		/* extra saving throws */
	s16b x_stl;		/* extra stealth */
	s16b x_srh;		/* extra searching ability */
	s16b x_fos;		/* extra searching frequency */
	s16b x_thn_bare;	/* extra to hit (melee/barehanded) */
	s16b x_thn_edged;	/* extra to hit (melee/edged weapons) */
	s16b x_thn_hafted;	/* extra to hit (melee/hafted weapons) */
	s16b x_thn_polearm;	/* extra to hit (melee/polearms) */
	s16b x_thn_axe;		/* extra to hit (melee/axes) */
	s16b x_thb_sling;	/* extra to hit (ranged/slings) */
	s16b x_thb_bow;		/* extra to hit (ranged/bows) */
	s16b x_thb_xbow;	/* extra to hit (ranged/crossbows) */
	s16b x_tht;		/* extra to hit (throwing) */

	s16b c_mhp;		/* Class hit-dice adjustment */
	s16b c_exp;		/* Class experience factor */

	u32b flags;		/* Class Flags */

	u16b max_attacks;	/* Maximum possible attacks */
	u16b min_weight;	/* Minimum weapon weight for calculations */
	u16b att_multiply;	/* Multiplier for attack calculations */

	byte spell_book;	/* Tval of spell books (if any) */
	u16b spell_stat;	/* Stat for spells (if any) */
	u16b spell_first;	/* Level of first spell */
	u16b spell_weight;	/* Weight that hurts spells */

	u32b sense_base;	/* Base pseudo-id value */
	u16b sense_div;		/* Pseudo-id divisor */

	start_item start_items[MAX_START_ITEMS]; /* The starting inventory */

	player_magic spells; /* Magic spells */
};

/*
 * Player background information
 */
struct hist_type
{
	u32b text;			    /* Text (offset) */

	byte roll;			    /* Frequency of this entry */
	byte chart;			    /* Chart index */
	byte next;			    /* Next chart index */
	byte bonus;			    /* Social Class Bonus + 50 */
};

/*
 * Some more player information
 *
 * This information is retained across player lives
 */
struct player_other
{
	char full_name[32];		/* Full name */
	char base_name[32];		/* Base name */

	bool opt[OPT_MAX];		/* Options */

	u32b window_flag[ANGBAND_TERM_MAX];	/* Window flags */

	byte hitpoint_warn;		/* Hitpoint warning (0 to 9) */

	byte delay_factor;		/* Delay factor (0 to 9) */
};


/*
 * Most of the "player" information goes here.
 *
 * This stucture gives us a large collection of player variables.
 *
 * This entire structure is wiped when a new character is born.
 *
 * This structure is more or less laid out so that the information
 * which must be saved in the savefile precedes all the information
 * which can be recomputed as needed.
 */
struct player_type
{
	s16b py;		/* Player location */
	s16b px;		/* Player location */

	byte psex;		/* Sex index */
	byte prace;		/* Race index */
	byte pclass;		/* Class index */
	byte prealclass;	/* Real (non-emulated) class index */
	byte oops;		/* Unused */

	byte hitdie;		/* Hit dice (sides) */
	byte expfact;		/* Experience factor */

	s16b age;		/* Characters age */
	s16b ht;		/* Height */
	s16b wt;		/* Weight */
	s16b sc;		/* Social Class */

	u16b fame;		/* Fame - used for quests */

	s32b au;		/* Current Gold */

	s16b max_depth;		/* Max depth */
	s16b depth;		/* Cur depth */
	s16b recall_depth;	/* recall depth*/

	s16b max_lev;		/* Max level */
	s16b lev;		/* Cur level */

	s32b max_exp;		/* Max experience */
	s32b exp;		/* Cur experience */
	u16b exp_frac;		/* Cur exp frac (times 2^16) */

	s16b mhp;		/* Max hit pts */
	s16b chp;		/* Cur hit pts */
	u16b chp_frac;		/* Cur hit frac (times 2^16) */

	s16b msp;		/* Max mana pts */
	s16b csp;		/* Cur mana pts */
	u16b csp_frac;		/* Cur mana frac (times 2^16) */	

	s16b stat_max[A_MAX];	/* Current "maximal" stat values */
	s16b stat_cur[A_MAX];	/* Current "natural" stat values */

	u32b m_r_idx;		/* Current monster body index */
	
	s16b fast;		/* Timed -- Fast */
	s16b slow;		/* Timed -- Slow */
	s16b blind;		/* Timed -- Blindness */
	s16b paralyzed;		/* Timed -- Paralysis */
	s16b confused;		/* Timed -- Confusion */
	s16b afraid;		/* Timed -- Fear */
	s16b image;		/* Timed -- Hallucination */
	s16b poisoned;		/* Timed -- Poisoned */
	s16b cut;		/* Timed -- Cut */
	s16b stun;		/* Timed -- Stun */

	s16b protevil;		/* Timed -- Protection */
	s16b invuln;		/* Timed -- Invulnerable */
	s16b hero;		/* Timed -- Heroism */
	s16b shero;		/* Timed -- Super Heroism */
	s16b shield;		/* Timed -- Shield Spell */
	s16b blessed;		/* Timed -- Blessed */
	s16b tim_invis;		/* Timed -- See Invisible */
	s16b tim_infra;		/* Timed -- Infra Vision */
	s16b tim_immaterial;	/* Timed -- immaterial */
	s16b tim_invisible;	/* Timed -- invisibility */

	s16b oppose_acid;	/* Timed -- oppose acid */
	s16b oppose_elec;	/* Timed -- oppose lightning */
	s16b oppose_fire;	/* Timed -- oppose heat */
	s16b oppose_cold;	/* Timed -- oppose cold */
	s16b oppose_pois;	/* Timed -- oppose poison */

	s16b project_elec;	/* Timed -- lightning cloud */

	s16b word_recall;	/* Word of recall counter */

	s16b energy;		/* Current energy */

	s16b food;		/* Current nutrition */

	byte confusing;		/* Glowing hands */
	byte searching;		/* Currently searching */

	s16b base_wakeup_chance;	/* Base amount of character noise */

	byte spell_flags[PY_MAX_SPELLS]; /* Spell flags */

	byte spell_order[PY_MAX_SPELLS]; /* Spell order */

	s16b player_hp[PY_MAX_LEVEL];	/* HP Array */

	char died_from[80];		/* Cause of death */
	char history[250];		/* Initial history */

	u16b total_winner;		/* Total winner */
	u16b panic_save;		/* Panic save */

	u16b noscore;			/* Cheating flags */

	bool is_dead;			/* Player is dead */

	bool wizard;			/* Player is in wizard mode */

	/*** Temporary fields ***/

	bool playing;			/* True if player is playing */

	bool leaving;			/* True if player is leaving */

	s16b create_stair;		/* Create a staircase on next level */

	s16b wy;			/* Dungeon panel */
	s16b wx;			/* Dungeon panel */

	byte cur_map_hgt;		/* Current dungeon level hight */
	byte cur_map_wid;		/* Current dungeon level width */

	s32b total_weight;		/* Total weight being carried */

	s16b inven_cnt;			/* Number of items in inventory */
	s16b equip_cnt;			/* Number of items in equipment */

	s16b target_set;		/* Target flag */
	s16b target_who;		/* Target identity */
	s16b target_row;		/* Target location */
	s16b target_col;		/* Target location */

	s16b health_who;		/* Health bar trackee */

	/* XXX This has nothing to do with "m_r_idx". I ought to choose
	   field names more carefully :-/ */
	s16b monster_race_idx;		/* Monster race trackee */

	s16b object_kind_idx;		/* Object kind trackee */

	s16b energy_use;		/* Energy use this turn */

	s16b resting;			/* Resting counter */
	s16b running;			/* Running counter */

	s16b run_cur_dir;		/* Direction we are running */
	s16b run_old_dir;		/* Direction we came from */
	bool run_unused;		/* Unused (padding field) */
	bool run_open_area;		/* Looking for an open area */
	bool run_break_right;		/* Looking for a break (right) */
	bool run_break_left;		/* Looking for a break (left) */

        bool in_wall;                   /* Currently in wall */

	s16b command_cmd;		/* Gives identity of current command */
	s16b command_arg;		/* Gives argument of current command */
	s16b command_rep;		/* Gives repetition of current command */
	s16b command_dir;		/* Gives direction of current command */

	s16b command_see;		/* See "cmd1.c" */
	s16b command_wrk;		/* See "cmd1.c" */

	s16b command_new;		/* Hack -- command chaining XXX XXX */

	s16b new_spells;		/* Number of spells available */

	bool cumber_armor;		/* Mana draining armor */
	bool cumber_glove;		/* Mana draining gloves */
	bool heavy_wield;		/* Heavy weapon */
	bool heavy_shoot;		/* Heavy shooter */
	bool icky_wield;		/* Icky weapon */

	s16b cur_lite;			/* Radius of lite (if any) */

	u32b notice;			/* Special Updates (bit flags) */
	u32b update;			/* Pending Updates (bit flags) */
	u32b redraw;			/* Normal Redraws (bit flags) */
	u32b window;			/* Window Redraws (bit flags) */

	s16b stat_use[A_MAX];		/* Current modified stats */
	s16b stat_top[A_MAX];		/* Maximal modified stats */

	/*** Extracted fields ***/

	s16b stat_add[A_MAX];		/* Equipment stat bonuses */
	s16b stat_ind[A_MAX];		/* Indexes into stat tables */

	bool immune_acid;		/* Immunity to acid */
	bool immune_elec;		/* Immunity to lightning */
	bool immune_fire;		/* Immunity to fire */
	bool immune_cold;		/* Immunity to cold */
	bool immune_pois;		/* Immunity to poison */

	bool resist_acid;		/* Resist acid */
	bool resist_elec;		/* Resist lightning */
	bool resist_fire;		/* Resist fire */
	bool resist_cold;		/* Resist cold */
	bool resist_pois;		/* Resist poison */

	bool resist_fear;		/* Resist fear */
	bool resist_lite;		/* Resist light */
	bool resist_dark;		/* Resist darkness */
	bool resist_blind;		/* Resist blindness */
	bool resist_confu;		/* Resist confusion */
	bool resist_sound;		/* Resist sound */
	bool resist_shard;		/* Resist shards */
	bool resist_nexus;		/* Resist nexus */
	bool resist_nethr;		/* Resist nether */
	bool resist_chaos;		/* Resist chaos */
	bool resist_disen;		/* Resist disenchant */

	bool sustain_str;		/* Keep strength */
	bool sustain_int;		/* Keep intelligence */
	bool sustain_wis;		/* Keep wisdom */
	bool sustain_dex;		/* Keep dexterity */
	bool sustain_con;		/* Keep constitution */
	bool sustain_chr;		/* Keep charisma */

	bool slow_digest;		/* Slower digestion */
	bool ffall;			/* Feather falling */
	bool lite;			/* Permanent light */
	bool regenerate;		/* Regeneration */
	bool s_regenerate;		/* Super-regeneration */
	bool telepathy;			/* Telepathy */
	bool see_inv;			/* See invisible */
	bool free_act;			/* Free action */
	bool hold_life;			/* Hold life */
	bool invisible;			/* Invisible */

        bool cold_touch;       		/* Cold touch */
        bool fire_touch;		/* Fire touch */
        bool acid_touch;		/* Acid touch */
        bool elec_touch;		/* Elec touch */
        
        bool pois_blood;		/* Poisonous blood */

	bool aura_fear;			/* Aura of fear */

	bool impact;			/* Earthquake blows */
	bool aggravate;			/* Aggravate monsters */
	bool teleport;			/* Random teleporting */
	bool exp_drain;			/* Experience draining */

	bool bless_blade;		/* Blessed blade */

        bool undead;          		/* Undead */
        bool pass_wall;			/* Immaterial */
        
        byte al_special;		/* Alignment (special) */

	s16b dis_to_h;			/* Known bonus to hit */
	s16b dis_to_d;			/* Known bonus to dam */
	s16b dis_to_a;			/* Known bonus to ac */

	s16b dis_ac;			/* Known base ac */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to dam */
	s16b to_a;			/* Bonus to ac */

	s16b ac;			/* Base ac */

	s16b see_infra;			/* Infravision range */

	s16b skill_dis;			/* Skill: Disarming */
	s16b skill_dev;			/* Skill: Magic Devices */
	s16b skill_sav;			/* Skill: Saving throw */
	s16b skill_stl;			/* Skill: Stealth factor */
	s16b skill_srh;			/* Skill: Searching ability */
	s16b skill_fos;			/* Skill: Searching frequency */
	s16b skill_thn_bare;		/* Skill: To hit (melee/barehanded) */
	s16b skill_thn_edged;		/* Skill: To hit (melee/edged weapons) */
	s16b skill_thn_hafted;		/* Skill: To hit (melee/hafted weapons) */
	s16b skill_thn_polearm;		/* Skill: To hit (melee/polearms) */
	s16b skill_thn_axe;		/* Skill: To hit (melee/axes) */
	s16b skill_thb_sling;		/* Skill: To hit (ranged/slings) */
	s16b skill_thb_bow;		/* Skill: To hit (ranged/bows) */
	s16b skill_thb_xbow;		/* Skill: To hit (ranged/crossbows) */
	s16b skill_tht;			/* Skill: To hit (throwing) */
	s16b skill_dig;			/* Skill: Digging */

	u32b noise;			/* Derived from stealth */

	s16b num_blow[INVEN_TOTAL];	/* Number of blows/shots */

	byte ammo_mult;			/* Ammo multiplier */

	byte ammo_tval;			/* Ammo variety */

	s16b pspeed;			/* Current speed */

	byte vulnerability;		/* Used to make animal packs charge and retreat */

	u16b cur_quest;			/* Current quest */
};

struct pwr_remap
{
	byte power;
	char name[35];
};


/**** Player variables ****/

extern const int adj_mag_study[];
extern const int adj_mag_mana[];
extern const byte adj_mag_fail[];
extern const int adj_mag_stat[];
extern const byte adj_chr_gold[];
extern const s16b adj_chr_charm[];
extern const byte adj_chr_comp[];
extern const byte adj_int_dev[];
extern const byte adj_wis_sav[];
extern const byte adj_dex_dis[];
extern const byte adj_int_dis[];
extern const byte adj_dex_ta[];
extern const byte adj_str_td[];
extern const byte adj_dex_th[];
extern const byte adj_str_th[];
extern const byte adj_str_wgt[];
extern const byte adj_str_hold[];
extern const byte adj_str_dig[];
extern const byte adj_str_blow[];
extern const byte adj_dex_blow[];
extern const byte adj_dex_safe[];
extern const byte adj_con_fix[];
extern const int adj_con_mhp[];
extern const byte blows_table[12][12];
extern const byte extract_energy[200];
extern const s32b player_exp[PY_MAX_LEVEL];
extern const player_sex sex_info[MAX_SEXES];
extern const u32b spell_table[2][9][2];
extern cptr spell_names[2][PY_MAX_SPELLS];
extern cptr stat_names[A_MAX];
extern cptr stat_names_reduced[A_MAX];
extern cptr stat_names_full[A_MAX];
extern cptr pwr_desc[];
extern pwr_remap pwr_desc_chaos[];

extern object_type *inventory;
extern const player_sex *sp_ptr;
extern player_race *rp_ptr; /* not const now :) */
extern const player_class *cp_ptr;
extern const player_magic *mp_ptr;
extern player_other *op_ptr;
extern player_type *p_ptr;
extern player_race *p_info;
extern char *p_name;
extern char *p_text;
extern player_class *c_info;
extern char *c_name;
extern char *c_text;
extern hist_type *h_info;
extern char *h_text;


/**** Player functions ****/

/* birth.c */
void player_birth(void);

/* cmd-move-attack.c */
bool test_hit(int chance, int ac, int vis);
int critical_shot(int weight, int plus, int dam);
int critical_norm(int weight, int plus, int dam);
int tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr);
void search(void);
void py_pickup(int pickup);
void hit_trap(int y, int x);
void py_attack(int y, int x);
void move_player(int dir, int jumping);
void run_step(int dir);

/* cmd-misc.c */
void do_cmd_go_up(void);
void do_cmd_go_down(void);
void do_cmd_search(void);
void do_cmd_toggle_search(void);
void do_cmd_open(void);
void do_cmd_close(void);
void do_cmd_tunnel(void);
void do_cmd_disarm(void);
void do_cmd_bash(void);
void do_cmd_make_trap(void);
void do_cmd_steal(void);
void do_cmd_alter(void);
void do_cmd_spike(void);
void do_cmd_walk(void);
void do_cmd_jump(void);
void do_cmd_run(void);
void do_cmd_hold(void);
void do_cmd_stay(void);
void do_cmd_rest(void);
void do_cmd_fire(void);
void do_cmd_throw(void);
void do_cmd_possess(void);	/* "Possess"/"Leave" the monster */
void do_cmd_pet(void);		/* Issue orders to pets */
void py_steal(int y, int x);
bool make_monster_trap(void);
void py_set_trap(int y, int x);
bool py_modify_trap(int y, int x);

/* cmd-object.c */
void do_cmd_inven(void);
void do_cmd_equip(void);
void do_cmd_wield(void);
void do_cmd_takeoff(void);
void do_cmd_drop(void);
void do_cmd_destroy(void);
void do_cmd_observe(void);
void do_cmd_uninscribe(void);
void do_cmd_inscribe(void);
void do_cmd_refill(void);
void do_cmd_target(void);
void do_cmd_look(void);
void do_cmd_locate(void);
void do_cmd_query_symbol(void);
/* Sorts monsters */
bool ang_sort_comp_hook(const void *u, const void *v, int a, int b);
/* Swaps monster */
void ang_sort_swap_hook(void *u, void *v, int a, int b);

/* menus.c */
void do_cmd_redraw(void);
void options_birth_menu(bool adult);
void do_cmd_change_name(void);
void do_cmd_message_one(void);
void do_cmd_messages(void);
void do_cmd_options(void);
void do_cmd_pref(void);
void do_cmd_macros(void);
void do_cmd_visuals(void);
void do_cmd_colors(void);
void do_cmd_note(char *note, int what_depth);
void do_cmd_version(void);
void do_cmd_feeling(void);
void do_cmd_quest(void);
void do_cmd_load_screen(void);
void do_cmd_save_screen(void);
void do_cmd_knowledge(void);

/* cmd-magic.c */
s16b spell_chance(int spell);
bool spell_okay(int spell, bool known);
void print_spells(const byte *spells, int num, int y, int x);
void display_koff(int k_idx);
cptr get_spell_name(int tval, int spell);
void do_cmd_browse_aux(const object_type *o_ptr);
void do_cmd_browse(void);
void do_cmd_study(void);
void do_cmd_cast(void);
void do_cmd_pray(void);

/* powers.c */
void do_cmd_monster_power(void);

/* player-misc.c */
void cnv_stat(int val, char *out_val);
s16b modify_stat_value(int value, int amount);
void notice_stuff(void);
void update_stuff(void);
void redraw_stuff(void);
void window_stuff(void);
void handle_stuff(void);

/* player-state.c */
bool set_blind(int v);
bool set_confused(int v);
bool set_poisoned(int v);
bool set_afraid(int v);
bool set_paralyzed(int v);
bool set_image(int v);
bool set_fast(int v);
bool set_slow(int v);
bool set_shield(int v);
bool set_blessed(int v);
bool set_hero(int v);
bool set_shero(int v);
bool set_protevil(int v);
bool set_invuln(int v);
bool set_tim_seeinvis(int v);
bool set_tim_infra(int v);
bool set_tim_immaterial(int v);
bool set_tim_invisibility(int v);
bool set_oppose_acid(int v);
bool set_oppose_elec(int v);
bool set_oppose_fire(int v);
bool set_oppose_cold(int v);
bool set_oppose_pois(int v);
bool set_stun(int v);
bool set_cut(int v);
bool set_food(int v);
void check_experience(void);
void gain_exp(s32b amount);
void lose_exp(s32b amount);

#endif /* PLAYER_H_INCLUDED */
