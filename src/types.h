/* File: types.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


/*
 * Note that "char" may or may not be signed, and that "signed char"
 * may or may not work on all machines.  So always use "s16b" or "s32b"
 * for signed values.  Also, note that unsigned values cause math problems
 * in many cases, so try to only use "u16b" and "u32b" for "bit flags",
 * unless you really need the extra bit of information, or you really
 * need to restrict yourself to a single byte for storage reasons.
 *
 * Also, if possible, attempt to restrict yourself to sub-fields of
 * known size (use "s16b" or "s32b" instead of "int", and "byte" instead
 * of "bool"), and attempt to align all fields along four-byte words, to
 * optimize storage issues on 32-bit machines.  Also, avoid "bit flags"
 * since these increase the code size and slow down execution.  When
 * you need to store bit flags, use one byte per flag, or, where space
 * is an issue, use a "byte" or "u16b" or "u32b", and add special code
 * to access the various bit flags.
 *
 * Many of these structures were developed to reduce the number of global
 * variables, facilitate structured program design, allow the use of ascii
 * template files, simplify access to indexed data, or facilitate efficient
 * clearing of many variables at once.
 *
 * Note that certain data is saved in multiple places for efficient access,
 * and when modifying the data in one place it must also be modified in the
 * other places, to prevent the creation of inconsistant data.
 */

#ifndef INCLUDED_TYPES_H
#define INCLUDED_TYPES_H


/**** Available Types ****/


/*
 * An array of 256 byte's
 */
typedef byte byte_256[256];

/*
 * An array of 256 u16b's
 */
typedef u16b u16b_256[256];


/*
 * An array of MAX_DUNGEON_WID byte's
 */
typedef byte byte_wid[MAX_DUNGEON_WID];

/*
 * An array of MAX_DUNGEON_WID s16b's
 */
typedef s16b s16b_wid[MAX_DUNGEON_WID];

/*
 * A matrix of MAX_DUNGEON_HGT * MAX_DUNGEON_WID u16b's
 */
typedef u16b u16b_dungeon[MAX_DUNGEON_HGT][MAX_DUNGEON_WID];




/**** Available Structs ****/


typedef struct maxima maxima;
typedef struct feature_state feature_state;
typedef struct feature_type feature_type;
typedef struct feature_lore feature_lore;
typedef struct object_kind object_kind;
typedef struct artifact_type artifact_type;
typedef struct ego_item_type ego_item_type;
typedef struct monster_blow monster_blow;
typedef struct monster_race monster_race;
typedef struct monster_race_message monster_race_message;
typedef struct monster_lore monster_lore;
typedef struct vault_type vault_type;
typedef struct effect_type effect_type;
typedef struct object_type object_type;
typedef struct monster_type monster_type;
typedef struct alloc_entry alloc_entry;
typedef struct quest_type quest_type;
typedef struct owner_type owner_type;
typedef struct store_type store_type;
typedef struct magic_type magic_type;
typedef struct player_magic player_magic;
typedef struct player_sex player_sex;
typedef struct player_race player_race;
typedef struct player_class player_class;
typedef struct hist_type hist_type;
typedef struct player_other player_other;
typedef struct player_type player_type;
typedef struct start_item start_item;
typedef struct names_type names_type;
typedef struct flavor_type flavor_type;
typedef struct autoinscription autoinscription;
typedef struct move_moment_type move_moment_type;
typedef struct dynamic_grid_type dynamic_grid_type;
typedef struct quiver_group_type quiver_group_type;
typedef struct option_type option_type;
typedef struct element_counter_type element_counter_type;
typedef struct dungeon_capabilities_type dungeon_capabilities_type;
typedef struct new_spell_info_type new_spell_info_type;


/**** Available structs ****/


/*
 * Information about maximal indices of certain arrays
 * Actually, these are not the maxima, but the maxima plus one
 */
struct maxima
{
	u32b fake_text_size;
	u32b fake_name_size;

	u16b f_max;		/* Max size for "f_info[]" */
	u16b k_max;		/* Max size for "k_info[]" */
	u16b art_max;		/* Max size for "a_info[]" */
	u16b e_max;		/* Max size for "e_info[]" */
	u16b r_max;		/* Max size for "r_info[]" */
	u16b v_max;		/* Max size for "v_info[]" */
	u16b p_max;		/* Max size for "p_info[]" */
	u16b h_max;		/* Max size for "h_info[]" */
	u16b b_max;		/* Max size per element of "b_info[]" */
	u16b c_max;		/* Max size for "c_info[]" */
	u16b q_max;		/* Max size for "q_info[]" */
	u16b flavor_max; /* Max size for "flavor_info[]" */
	u16b o_max;		/* Max size for "o_list[]" */
	u16b m_max;		/* Max size for "mon_list[]" */
	u16b x_max;		/* Max size for "x_info[]" */
	u16b ghost_other_max;  /* number of maintainer player ghost templates*/
	u16b art_spec_max; /* Max number of special artifacts*/

	u16b art_norm_max; /* Max number for normal artifacts (special - normal)*/
	u16b art_rand_max; /*max number of random artifacts*/
};

/*
 * Feature state structure
 *
 *    - Action (FS_*)
 *   - Result (FEAT_*)
 */
struct feature_state
{
	byte fs_action;
	s16b fs_result;
	u16b fs_power;
};



/*
 * Information about terrain "features"
 */
struct feature_type
{
	u32b name;			/* Name (offset) */
	u32b f_text;			/* Text (offset) */

	u16b f_mimic;	/* Feature to mimic */

	s16b f_edge;

	u32b f_flags1;
	u32b f_flags2;
	u32b f_flags3;

	u16b f_level;     	/* Minimum level */
	u16b f_rarity;    	/* 1/Rarity */

	u16b priority;  /* Map priority */
	s16b defaults;     /* Default state */

	feature_state state[MAX_FEAT_STATES];

	byte f_power;

	u16b unused;		/* Unused */

	byte d_attr;		/* Default feature attribute */
	char d_char;		/* Default feature character */

	byte x_attr;		/* Desired feature attribute */
	char x_char;		/* Desired feature character */

	bool f_everseen;	/* Used to despoilify knowledge screens */

	/* Fields use donly by effects. */
	u16b x_damage;				/* damage per 100 levels for a smart trap - or basic damage for an effect cloud*/
	byte x_gf_type;			/* the force type of the effect - numbers are hard coded in source */
	byte x_timeout_set; 	/*base time between effect instances */
	byte x_timeout_rand; /*is the random time between effects */

	/* Fields used only by terrain. */
	u16b dam_non_native;		/*damage to non-native creatures existing in grid */
	byte native_energy_move;	/*energy to move through for native creatures */
	byte non_native_energy_move;	/*energy to move through for non-native creatures */
	byte native_to_hit_adj;	/*combat bonus for being native (percentage)  */
	byte non_native_to_hit_adj;	/*combat bonus for being native (percentage)*/
	int f_stealth_adj;			/*Adjustment to stealth depending on terrain*/

};


/*
 * Feature "lore" information
 *
 * Note that these fields are related to the "feature recall".
 *
 */
struct feature_lore
{
	byte f_l_sights;		/*Number of times seeing this terrain*/

	u32b f_l_flags1;
	u32b f_l_flags2;
	u32b f_l_flags3;

	byte f_l_defaults;     /* Default state */

	byte f_l_state[MAX_FEAT_STATES];

	byte f_l_power;		/*Number of observed usages of power (unlock, trap power, etc)*/

	byte f_l_dam_non_native;		/*Number of observed damage to non-native creatures existing in grid*/
	byte f_l_native_moves;		/* Number of observed native moves for this terrain */
	byte f_l_non_native_moves;	/* Number of observed non-native moves for this terrain */
	byte f_l_native_to_hit_adj;	/*Number of observed  for being native (percentage)*/
	byte f_l_non_native_to_hit_adj;	/*Number of observed combat penalties for being non-native (percentage)*/
	byte f_l_stealth_adj;			/*Number of observed adjustments to stealth depending on terrain*/

};



/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */
struct object_kind
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte tval;			/* Object type */
	byte sval;			/* Object sub type */

	s16b pval;			/* Object extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	byte dd, ds;		/* Damage dice/sides */

	s16b weight;		/* Weight */

	s32b cost;			/* Object "base cost" */

	u32b k_flags1;		/* Flags, set 1 */
	u32b k_flags2;		/* Flags, set 2 */
	u32b k_flags3;		/* Flags, set 3 */
	u32b k_native;		/* Flags, native */

	byte locale[4];		/* Allocation level(s) */
	byte chance[4];		/* Allocation chance(s) */

	byte k_level;			/* Level */
	byte extra;			/* Something */


	byte d_attr;		/* Default object attribute */
	char d_char;		/* Default object character */


	byte x_attr;		/* Desired object attribute */
	char x_char;		/* Desired object character */


	u16b flavor;		/* Special object flavor (or zero) */

	bool aware;			/* The player is "aware" of the item's effects */

	bool tried;			/* The player has "tried" one of the items */

	byte squelch;		/* Squelch setting for the particular item */

	bool everseen;		/* Used to despoilify squelch menus */
};



/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile,
 * except for the random artifacts
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */
struct artifact_type
{
	char name[MAX_LEN_ART_NAME];	/* Name */
	u32b text;			/* Text (offset) */

	byte tval;			/* Artifact type */
	byte sval;			/* Artifact sub type */

	s16b pval;			/* Artifact extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	byte dd, ds;		/* Damage when hits */

	s16b weight;		/* Weight */

	s32b cost;			/* Artifact "cost" */

	u32b a_flags1;		/* Artifact Flags, set 1 */
	u32b a_flags2;		/* Artifact Flags, set 2 */
	u32b a_flags3;		/* Artifact Flags, set 3 */
	u32b a_native;		/* Flags, native */

	byte a_level;			/* Artifact level */
	byte a_rarity;		/* Artifact rarity */

	byte a_cur_num;		/* Number created (0 or 1) */
	byte a_max_num;		/* Unused (should be "1") */

	byte activation;	/* Activation to use */
	u16b time;			/* Activation time */
	u16b randtime;		/* Activation time dice */
};


/*
 * Information about "ego-items".
 */
struct ego_item_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	s32b cost;			/* Ego-item "cost" */

	u32b flags1;		/* Ego-Item Flags, set 1 */
	u32b flags2;		/* Ego-Item Flags, set 2 */
	u32b flags3;		/* Ego-Item Flags, set 3 */
	u32b e_native;		/* Flags, native */

	byte level;			/* Minimum level */
	byte rarity;		/* Object rarity */
	byte rating;		/* Level rating boost */

	byte tval[EGO_TVALS_MAX]; /* Legal tval */
	byte min_sval[EGO_TVALS_MAX];	/* Minimum legal sval */
	byte max_sval[EGO_TVALS_MAX];	/* Maximum legal sval */

	byte max_to_h;		/* Maximum to-hit bonus */
	byte max_to_d;		/* Maximum to-dam bonus */
	byte max_to_a;		/* Maximum to-ac bonus */
	byte max_pval;		/* Maximum pval */

	byte xtra;			/* Extra sustain/resist/power */

	bool everseen;			/* Do not spoil squelch menus */
	bool squelch;			/* Squelch this ego-item */
};




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

	s16b extra;				/* Unused (for now) */

	byte freq_ranged;		/* Ranged attack frequency */
	byte mana;				/* Max mana */
	byte spell_power;		/* Power of (damage-dealing) spells */
	u32b mon_power;        		/* Monster Power Rating */

#ifdef ALLOW_DATA_DUMP

	u32b mon_eval_hp;		/*evaluated hitpoint power of monster*/
	u32b mon_eval_dam;		/*evaluated damage power of monster*/

#endif /*ALLOW_DATA_DUMP*/


	u32b flags1;			/* Flags 1 (general) */
	u32b flags2;			/* Flags 2 (abilities) */
	u32b flags3;			/* Flags 3 (race/resist) */
	u32b flags4;			/* Flags 4 (inate/breath) */
	u32b flags5;			/* Flags 5 (normal spells) */
	u32b flags6;			/* Flags 6 (special spells) */
	u32b flags7;			/* Flags 7 (summon spells) */

	u32b r_native;			/*Terrains where monster is native*/

	monster_blow blow[MONSTER_BLOW_MAX]; /* Up to four blows per round */

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

	u32b r_l_flags1;			/* Observed racial flags */
	u32b r_l_flags2;			/* Observed racial flags */
	u32b r_l_flags3;			/* Observed racial flags */
	u32b r_l_flags4;			/* Observed racial flags */
	u32b r_l_flags5;			/* Observed racial flags */
	u32b r_l_flags6;			/* Observed racial flags */
	u32b r_l_flags7;			/* Observed racial flags */

	u32b r_l_native;			/* Observed Nativity Flags*/
};

/*
 * Information about "vault generation"
 */
struct vault_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte typ;			/* Vault type */

	byte rat;			/* Vault rating */

	byte hgt;			/* Vault height */
	byte wid;			/* Vault width */
};

struct effect_type
{
	byte x_type;            /* Effect Type */

	u16b x_f_idx;           /* Effect Feature IDX */

	byte x_cur_y;			/* Current y location, or countdown_base */
	byte x_cur_x;			/* Current x location, or countdown_rand */

	byte x_countdown;       /* Number of turns effect has left */
	byte x_repeats;			/* Number of times the effect repeats*/

	u16b x_power;           /* Strength of effect */

	s16b x_source;          /* Source of effect - THIS MUST BE THE RACE of the monster, not the mon_idx of the creature. */

	u16b x_flags;           /* Effect "memory" bitflags */

	s16b next_x_idx;		/* Idx of next effect at this square. */

	s16b x_r_idx;           /* Some monster race index. Used for inscriptions */
};


/*
 * Object information, for a specific object.
 *
 * Note that a "discount" on an item is permanent and never goes away.
 *
 * Note that inscriptions are now handled via the "quark_str()" function
 * applied to the "note" field, which will return NULL if "note" is zero.
 *
 * Note that "object" records are "copied" on a fairly regular basis,
 * and care must be taken when handling such objects.
 *
 * Note that "object flags" must now be derived from the object kind,
 * the artifact and ego-item indexes, and the two "xtra" fields.
 *
 * Each cave grid points to one (or zero) objects via the "o_idx"
 * field (above).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a "stack" of objects in the same grid.
 *
 * Each monster points to one (or zero) objects via the "hold_o_idx"
 * field (below).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a pile of objects held by the monster.
 *
 * The "held_m_idx" field is used to indicate which monster, if any,
 * is holding the object.  Objects being held have "ix=0" and "iy=0".
 */
struct object_type
{
	s16b k_idx;			/* Kind index (zero if "dead") */

	byte iy;			/* Y-position on map, or zero */
	byte ix;			/* X-position on map, or zero */

	byte tval;			/* Item type (from kind) */
	byte sval;			/* Item sub-type (from kind) */

	s16b pval;			/* Item extra-parameter */

	byte discount;		/* Discount (if any) */

	byte number;		/* Number of items */

	s16b weight;		/* Item weight */

	byte art_num;		/* Artifact type, if any */
	byte ego_num;		/* Ego-Item type, if any */

	byte xtra1;			/* Extra info type */
	u32b xtra2;			/* Extra info index */

	s16b to_h;			/* Plusses to hit */
	s16b to_d;			/* Plusses to damage */
	s16b to_a;			/* Plusses to AC */

	s16b ac;			/* Normal AC */

	byte dd, ds;		/* Damage dice/sides */

	s16b timeout;		/* Timeout Counter */

	u32b ident;			/* Special flags (was byte) */

	byte marked;		/* Object is marked */

	u16b obj_note;		/* Inscription index */

	s16b next_o_idx;	/* Next object in stack (if any) */

	s16b held_m_idx;	/* Monster holding us (if any) */

        /* Object history - DRS */

	byte origin_nature;	/* ORIGIN_* */
	s16b origin_dlvl;	/* Depth */
	s16b origin_r_idx;	/* Monster race */
	s16b origin_m_name;	/* Index of monster name quark. Used only for player ghosts */
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

	s16b csleep;		/* Inactive counter */

	byte mspeed;		/* Monster "speed" */
	s16b m_energy;		/* Monster "energy" */

	byte stunned;		/* Monster is stunned */
	byte confused;		/* Monster is confused */
	byte monfear;		/* Monster is afraid */
	s16b slowed;		/* Monster is slowed */
	s16b hasted;		/* Monster is hasted */

	byte cdis;			/* Current dis from player */

	u32b mflag;			/* Extra monster flags */

	bool ml;			/* Monster is "visible" */

	s16b hold_o_idx;	/* Object being held (if any) */

	u32b smart;			/* Field for "smart_learn" */

	byte target_y;		/* Monster target */
	byte target_x;

	byte min_range;		/* What is the closest we want to be? */  /* Not saved */
	byte best_range;	/* How close do we want to be? */  /* Not saved */

	byte mana;          /* Current mana level */

	s16b mimic_k_idx;	/*type of mimic code*/

	byte using_flow;	/*Which movement flow is the creature using?*/

};




/*
 * An entry for the object/monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */
struct alloc_entry
{
	s16b index;		/* The actual index */

	byte level;		/* Base dungeon level */
	byte prob1;		/* Probability, pass 1 */
	byte prob2;		/* Probability, pass 2 */
	byte prob3;		/* Probability, pass 3 */

	u16b total;		/* Unused for now */
};



/*
 * Structure for the "quests"
 */
struct quest_type
{
	u32b name;			/* Name (offset) */
	byte type;			/* Quest Type */
	byte reward;		/* Quest Reward */
	byte theme;			/* Monster Theme for themed levels and nests/pits*/

	byte active_level;	/* Equals dungeon level if not completed, 0 if completed */
	byte base_level;	/* The dungeon level on which the quest was assigned*/

	s16b mon_idx;		/* Monster race/unique */

	s16b cur_num;		/* Number killed */
	s16b max_num;		/* Number required */

	bool started;		/* Has the player start the quest */
};


/*
 * A store owner
 */
struct owner_type
{
	u32b owner_name;	/* Name (offset) */

	s16b max_cost;		/* Purse limit */

	byte max_inflate;	/* Inflation (max) */
	byte min_inflate;	/* Inflation (min) */

	byte haggle_per;	/* Haggle unit */

	byte insult_max;	/* Insult limit */

	byte owner_race;	/* Owner race */
};




/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store_type
{
	byte owner;				/* Owner index */

	s16b insult_cur;		/* Insult counter */

	s16b good_buy;			/* Number of "good" buys */
	s16b bad_buy;			/* Number of "bad" buys */

	s32b store_open;		/* Closed until this turn */

	byte stock_num;			/* Stock -- Number of entries */
	s16b stock_size;		/* Stock -- Total Size of Array */
	object_type *stock;		/* Stock -- Actual stock items */

};


struct magic_type
{
	byte slevel;		/* Required level (to learn) */
	byte smana;			/* Required mana (to cast) */
	byte sfail;			/* Minimum chance of failure */
	byte sexp;			/* Encoded experience bonus */
};


/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
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
	cptr title;			/* Type of sex */

	cptr winner;		/* Name of winner */
};


/*
 * Player racial info
 */
struct player_race
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	s16b r_adj[A_MAX];	/* Racial stat bonuses */

	s16b r_dis;			/* disarming */
	s16b r_dev;			/* magic devices */
	s16b r_sav;			/* saving throw */
	s16b r_srh;			/* search ability */
	s16b r_fos;			/* search frequency */
	s16b r_thn;			/* combat (normal) */
	s16b r_thb;			/* combat (shooting) */

	byte r_mhp;			/* Race hit-dice modifier */
	byte r_mmp;			/* Race mana modifier */
	byte r_exp;			/* Race experience factor */

	byte b_age;			/* base age */
	byte m_age;			/* mod age */

	byte m_b_ht;		/* base height (males) */
	byte m_m_ht;		/* mod height (males) */
	byte m_b_wt;		/* base weight (males) */
	byte m_m_wt;		/* mod weight (males) */

	byte f_b_ht;		/* base height (females) */
	byte f_m_ht;		/* mod height (females) */
	byte f_b_wt;		/* base weight (females) */
	byte f_m_wt;		/* mod weight (females) */

	byte infra;			/* Infra-vision	range */

	byte choice;		/* Legal class choices */

	s16b hist;			/* Starting history index */

	u32b pr_flags1;		/* Racial Flags, set 1 */
	u32b pr_flags2;		/* Racial Flags, set 2 */
	u32b pr_flags3;		/* Racial Flags, set 3 */
	u32b pr_native;		/* Player Native Flags, set 3 */
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
 * Player class info
 */
struct player_class
{
	u32b name;			/* Name (offset) */

	u32b title[10];		/* Titles - offset */

	s16b c_adj[A_MAX];	/* Class stat modifier */

	s16b c_dis;			/* class disarming */
	s16b c_dev;			/* class magic devices */
	s16b c_sav;			/* class saving throws */
	s16b c_srh;			/* class searching ability */
	s16b c_fos;			/* class searching frequency */
	s16b c_thn;			/* class to hit (normal) */
	s16b c_thb;			/* class to hit (bows) */

	s16b x_dis;			/* extra disarming */
	s16b x_dev;			/* extra magic devices */
	s16b x_sav;			/* extra saving throws */
	s16b x_stl;			/* extra stealth */
	s16b x_srh;			/* extra searching ability */
	s16b x_fos;			/* extra searching frequency */
	s16b x_thn;			/* extra to hit (normal) */
	s16b x_thb;			/* extra to hit (bows) */

	s16b c_mhp;			/* Class hit-dice adjustment */
	s16b c_exp;			/* Class experience factor */

	u32b flags;			/* Class Flags */
	u32b c_native;		/* Class Native Flags*/

	u16b max_attacks;	/* Maximum possible attacks */
	u16b min_weight;	/* Minimum weapon weight for calculations */
	u16b att_multiply;	/* Multiplier for attack calculations */

	byte spell_book;	/* Tval of spell books (if any) */
	u16b spell_first;	/* Level of first spell */
	u16b spell_weight;	/* Weight that hurts spells */

	u32b sense_base;	/* Base pseudo-id value */
	u16b sense_div;		/* Pseudo-id divisor */

	start_item start_items[MAX_START_ITEMS];/* The starting inventory */

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
	byte py;			/* Player location */
	byte px;			/* Player location */

	byte flow_center_y;  /* Centerpoints of the last full flow rebuild. */
	byte flow_center_x;

	byte update_center_y; /* Centerpoints of the last partial flow rebuild. */
	byte update_center_x;

	byte psex;			/* Sex index */
	byte prace;			/* Race index */
	byte pclass;		/* Class index */
	byte oops;			/* Unused */

	byte mpdie;			/* mana dice (sides) */
	byte hitdie;		/* Hit dice (sides) */
	byte expfact;		/* Experience factor */

	s16b statgain1,statgain2,statgain3,statgain4,statgain5,statgain6,statgain7,statgain8,statgain9; /* from 0 to A_MAX-1, or set to A_MAX once change effected */

	s16b age;			/* Characters age */
	s16b ht;			/* Height */
	s16b wt;			/* Weight */
	s16b sc;			/* Social Class */

	u16b fame;			/* Fame - used for quests */

	s32b au;			/* Current Gold */

	s16b max_depth;		/* Max depth */
	s16b depth;			/* Cur depth */
	s16b recall_depth;		/* recall depth*/

	s16b max_lev;		/* Max level */
	s16b lev;			/* Cur level */

	s32b max_exp;		/* Max experience */
	s32b exp;			/* Cur experience */
	u16b exp_frac;		/* Cur exp frac (times 2^16) */

	s16b mhp;			/* Max hit pts */
	s16b chp;			/* Cur hit pts */
	u16b chp_frac;		/* Cur hit frac (times 2^16) */

	s16b msp;			/* Max mana pts */
	s16b csp;			/* Cur mana pts */
	u16b csp_frac;		/* Cur mana frac (times 2^16) */

	s16b stat_max[A_MAX];	/* Current "maximal" stat values */
	s16b stat_cur[A_MAX];	/* Current "natural" stat values */

	s16b fast;			/* Timed -- Fast */
	s16b slow;			/* Timed -- Slow */
	s16b blind;			/* Timed -- Blindness */
	s16b paralyzed;		/* Timed -- Paralysis */
	s16b confused;		/* Timed -- Confusion */
	s16b afraid;		/* Timed -- Fear */
	s16b image;			/* Timed -- Hallucination */
	s16b poisoned;		/* Timed -- Poisoned */
	s16b cut;			/* Timed -- Cut */
	s16b stun;			/* Timed -- Stun */

	s16b protevil;		/* Timed -- Protection */
	s16b invuln;		/* Timed -- Invulnerable */
	s16b hero;			/* Timed -- Heroism */
	s16b shero;			/* Timed -- Super Heroism */
	s16b shield;		/* Timed -- Shield Spell */
	s16b megashield;		/* Timed -- Shield Spell */
	s16b blessed;		/* Timed -- Blessed */
	s16b tim_invis;		/* Timed -- See Invisible */
	s16b tim_infra;		/* Timed -- Infra Vision */
	s16b slay_elements; /* Timed -- Temporary elemental damage */

	s16b oppose_acid;	/* Timed -- oppose acid */
	s16b oppose_elec;	/* Timed -- oppose lightning */
	s16b oppose_fire;	/* Timed -- oppose heat */
	s16b oppose_cold;	/* Timed -- oppose cold */
	s16b oppose_pois;	/* Timed -- oppose poison */
	s16b oppose_conf;	/* Timed -- oppose confusion */

	s16b temp_native_lava;	/* Timed -- native to lava */
	s16b temp_native_oil;	/* Timed -- native to oil */
	s16b temp_native_sand;	/* Timed -- native to sand */
	s16b temp_native_forest;	/* Timed -- native to forest */
	s16b temp_native_water;	/* Timed -- native to water */
	s16b temp_native_mud;	/* Timed -- native to mud */

	s16b word_recall;	/* Word of recall counter */
	s16b teleport_delay; /* teleport counter */
	s16b teleport_range; /* teleport range when it happens */

	s16b p_energy;		/* Current energy */

	s16b food;			/* Current nutrition */

	byte confusing;		/* Glowing hands */
	byte searching;		/* Currently searching */
	byte flying;		/* Currently flying */

	s16b base_wakeup_chance;	/* Base amount of character noise */

	byte spell_flags[PY_MAX_SPELLS]; /* Spell flags */

	byte spell_order[PY_MAX_SPELLS];	/* Spell order */

	s16b player_hp[PY_MAX_LEVEL];	/* HP Array */
	s16b player_sp[PY_MAX_LEVEL];	/* SP Array */

	char died_from[80];		/* Cause of death */
	char history[250];	/* Initial history */

	u16b total_winner;		/* Total winner */
	u16b panic_save;		/* Panic save */

	u16b noscore;			/* Cheating flags */

	bool is_dead;			/* Player is dead */

	bool wizard;			/* Player is in wizard mode */


	/*** Temporary fields ***/

	bool playing;			/* True if player is playing */

	bool leaving;			/* True if player is leaving */

	s16b create_stair;		/* Create a staircase on next level */

	byte cur_map_hgt;		/* Current dungeon level hight */
	byte cur_map_wid;		/* Current dungeon level width */

	s32b total_weight;		/* Total weight being carried */

	s16b inven_cnt;			/* Number of items in inventory */
	s16b equip_cnt;			/* Number of items in equipment */
	s16b pack_size_reduce;		/* Number of inventory slots used by
					   the quiver */

	s16b target_set;		/* Target flag */
	s16b target_who;		/* Target identity */
	s16b target_row;		/* Target location */
	s16b target_col;		/* Target location */

	s16b health_who;		/* Health bar trackee */

	s16b monster_race_idx;	/* Monster race trackee */

	s16b object_kind_idx;	/* Object kind trackee */

	s16b feature_kind_idx;	/* Feature kind tracker*/

	byte p_energy_use;		/* Energy use this turn */

	s16b resting;			/* Resting counter */
	s16b running;			/* Running counter */

	s16b run_cur_dir;		/* Direction we are running */
	s16b run_old_dir;		/* Direction we came from */
	bool run_unused;		/* Unused (padding field) */
	bool run_open_area;		/* Looking for an open area */
	bool run_break_right;	/* Looking for a break (right) */
	bool run_break_left;	/* Looking for a break (left) */

	s16b command_cmd;		/* Gives identity of current command */
	s16b command_arg;		/* Gives argument of current command */
	s16b command_rep;		/* Gives repetition of current command */
	s16b command_dir;		/* Gives direction of current command */

	s16b command_see;		/* See "cmd1.c" */
	s16b command_wrk;		/* See "cmd1.c" */

	s16b command_new;		/* Hack -- command chaining XXX XXX */

	s16b new_spells;		/* Number of spells available */

	bool cumber_armor;	/* Mana draining armor */
	bool cumber_glove;	/* Mana draining gloves */
	bool heavy_wield;	/* Heavy weapon */
	bool heavy_shoot;	/* Heavy shooter */
	bool icky_wield;	/* Icky weapon */

	s16b cur_lite;		/* Radius of lite (if any) */

	u32b notice;		/* Special Updates (bit flags) */
	u32b update;		/* Pending Updates (bit flags) */
	u32b redraw;		/* Normal Redraws (bit flags) */
	u32b window;		/* Window Redraws (bit flags) */

	s16b stat_use[A_MAX];	/* Current modified stats */
	s16b stat_top[A_MAX];	/* Maximal modified stats */

	/*** Extracted fields ***/

	s16b stat_add[A_MAX];	/* Equipment stat bonuses */
	s16b stat_ind[A_MAX];	/* Indexes into stat tables */

	bool immune_acid;	/* Immunity to acid */
	bool immune_elec;	/* Immunity to lightning */
	bool immune_fire;	/* Immunity to fire */
	bool immune_cold;	/* Immunity to cold */
	bool immune_pois;	/* immunity to poison*/

	bool resist_acid;	/* Resist acid */
	bool resist_elec;	/* Resist lightning */
	bool resist_fire;	/* Resist fire */
	bool resist_cold;	/* Resist cold */
	bool resist_pois;	/* Resist poison */

	bool resist_fear;	/* Resist fear */
	bool resist_lite;	/* Resist light */
	bool resist_dark;	/* Resist darkness */
	bool resist_blind;	/* Resist blindness */
	bool resist_confu;	/* Resist confusion */
	bool resist_sound;	/* Resist sound */
	bool resist_shard;	/* Resist shards */
	bool resist_nexus;	/* Resist nexus */
	bool resist_nethr;	/* Resist nether */
	bool resist_chaos;	/* Resist chaos */
	bool resist_disen;	/* Resist disenchant */

	u32b p_native;  /* Lists terrains the player is native to*/
	u32b p_native_known;  /* Lists terrains the player is known to be native to*/

	bool sustain_str;	/* Keep strength */
	bool sustain_int;	/* Keep intelligence */
	bool sustain_wis;	/* Keep wisdom */
	bool sustain_dex;	/* Keep dexterity */
	bool sustain_con;	/* Keep constitution */
	bool sustain_agi;	/* Keep agility */
	bool sustain_ste;	/* Keep stealth */
	bool sustain_per;	/* Keep perception */
	bool sustain_luc;	/* Keep luck */

	bool slow_digest;	/* Slower digestion */
	bool ffall;			/* Feather falling */
	bool lite;			/* Permanent light */
	bool regenerate;	/* Regeneration */
	bool telepathy;		/* Telepathy */
	bool sr_telepathy_1;		/* Short-range telepathy from high Perception */
	bool sr_telepathy_2;		/* Medium-range telepathy from high Perception */
	bool see_inv;		/* See invisible */
	bool free_act;		/* Free action */
	bool hold_life;		/* Hold life */
	bool vampire;       /* Draining */
	bool twoh_weapon;   /* crits bonus, but no shield */

	bool impact;		/* Earthquake blows */
	bool aggravate;		/* Aggravate monsters */
	int temp_aggravate; /* Stumbled in the last couple of turns! */
	bool teleport;		/* Random teleporting */
	bool exp_drain;		/* Experience draining */

	bool bless_blade;	/* Blessed blade */

	s16b dis_to_h_melee, dis_to_h_missile;		/* Known bonus to hit */
	s16b dis_to_d_melee, dis_to_d_missile;		/* Known bonus to dam */
	s16b dis_to_a;		/* Known bonus to ac */

	s16b dis_ac;		/* Known base ac */

	s16b to_h_melee, to_h_missile;			/* Bonus to hit */
	s16b to_d_melee, to_d_missile;			/* Bonus to dam */
	s16b to_a;			/* Bonus to ac */

	s16b ac;			/* Base ac */

	s16b see_infra;		/* Infravision range */

	s16b skill_dis;		/* Skill: Disarming */
	s16b skill_dev;		/* Skill: Magic Devices */
	s16b skill_sav;		/* Skill: Saving throw */
	s16b skill_stl;		/* Skill: Stealth factor */
	s16b skill_srh;		/* Skill: Searching ability */
	s16b skill_fos;		/* Skill: Searching frequency */
	s16b skill_thn;		/* Skill: To hit (normal) */
	s16b skill_thb;		/* Skill: To hit (shooting) */
	s16b skill_tht;		/* Skill: To hit (throwing) */
	s16b skill_dig;		/* Skill: Digging */

	s16b num_blows_times_ten;		/* Number of blows */
	s16b num_fire_times_ten;		/* Number of shots */

	byte ammo_mult;		/* Ammo multiplier */

	byte ammo_tval;		/* Ammo variety */

	s16b pspeed;		/* Current speed */

	byte vulnerability;	/* Used to make animal packs charge and retreat */

	u16b cur_quest;		/* Current quest */

	u16b cumulative_terrain_damage; /* How much damage we are taking from
					   					terrain (multiplied by 10) */

	s32b p_turn; /* Player turn */

	bool cursed_quiver;	/* The quiver is cursed */

	u16b dungeon_type;	/* One of the DUNGEON_TYPE_* constants */

	int n_woken;

};

struct new_spell_info_type
{
	s16b code; /* a NEWSPELL_ code */

	s16b school1; /* a SCHOOL_ code */
	s16b school2; /* a SCHOOL_ code or -1 */

	s16b level; 
	s16b cost;

	char name[80];
	s16b book; /* currently from 0 to 7 */
};

/*
 * Semi-Portable High Score List Entry (128 bytes)
 *
 * All fields listed below are null terminated ascii strings.
 *
 * In addition, the "number" fields are right justified, and
 * space padded, to the full available length (minus the "null").
 *
 * Note that "string comparisons" are thus valid on "pts".
 */

typedef struct high_score high_score;

struct high_score
{
	char what[8];		/* Version info (string) */

	char pts[10];		/* Total Score (number) */

	char gold[10];		/* Total Gold (number) */

	char turns[10];		/* Turns Taken (number) */

	char day[10];		/* Time stamp (string) */

	char who[16];		/* Player Name (string) */

	char uid[8];		/* Player UID (number) */

	char sex[2];		/* Player Sex (string) */
	char p_r[3];		/* Player Race (number) */
	char p_c[3];		/* Player Class (number) */

	char cur_lev[4];		/* Current Player Level (number) */
	char cur_dun[4];		/* Current Dungeon Level (number) */
	char max_lev[4];		/* Max Player Level (number) */
	char max_dun[4];		/* Max Dungeon Level (number) */

	char how[32];		/* Method of death (string) */
};




struct flavor_type
{
	u32b text;      /* Text (offset) */

	byte tval;      /* Associated object type */
	byte sval;      /* Associated object sub-type */

	byte d_attr;    /* Default flavor attribute */
	char d_char;    /* Default flavor character */

	byte x_attr;    /* Desired flavor attribute */
	char x_char;    /* Desired flavor character */
};




/*structure of letter probabilitiesfor the random name generator*/
struct names_type
{
	u16b lprobs[S_WORD+1][S_WORD+1][S_WORD+1];
	u16b ltotal[S_WORD+1][S_WORD+1];
};

/*Information for object auto-inscribe*/
struct autoinscription
{
	s16b	kindIdx;
	s16b	inscriptionIdx;
};

/*
 * Structure for building monster "lists"
 */
struct move_moment_type
{
	s16b m_idx;
	int moment;
};


/*
 * Simple structure to hold a map location
 */
typedef struct coord coord;

struct coord
{
	byte y;
	byte x;
};


/*
 * A circular queue of map locations.
 * Check out defines.h and util.c for usage
 */
typedef struct
{
	/* Maximum number of grids in the queue */
	size_t max_size;
	/* Grid data */
	coord *data;
	/* Head and tail of the queue */
	size_t head, tail;
} grid_queue_type;


/*
 * The type definition of the entries of the "dyna_g" array
 */
struct dynamic_grid_type
{
	/* Coordinates */
	byte y;
	byte x;

	/* DF1_* flags */
	byte flags;

	/*
	 * Timed features use a counter. Each time the features are
	 * proccesed this counter is decremented. The effect is applied when
	 * the counter becomes 0.
	 */
	byte counter;
};

/*
 * A stacked monster message entry
 */
struct monster_race_message
{
	s16b mon_race;		/* The race of the monster */
	byte mon_flags;		/* Flags: 0x01 means hidden monster, 0x02 means offscreen monster */
 	byte msg_code;		/* The coded message */
	byte mon_count;		/* How many monsters triggered this message */
};

/*
 * Info used to manage quiver groups
 */
struct quiver_group_type
{
	char cmd;		/* The command used to perform an action with the objects in the group */
	byte color;		/* The color of the pseudo-tag used for the group */
};

/*
 * The descriptions and default values of the in-game options
 */
struct option_type
{
	cptr text;
	cptr desc;
	bool norm;
};

/*
 * Counts the number of elemental terrain types in the dungeon
 * Note individual values for lava, bmud and bwater
 */
struct element_counter_type
{
	u16b fire;
	u16b acid;
	u16b ice;
	u16b oil;
	u16b water;
	u16b forest;
	u16b lava;
	u16b sand;
	u16b mud;
	u16b bmud;
	u16b bwater;
};

/*
 * Set of custom predicates that modify the behavior of the game,
 * specially dungeon generation. The predicates are assigned in generate.c
 */
struct dungeon_capabilities_type
{
	/*
	 * Check if a monster of the given race can have escorts
	 * Used only in alloc_monster
	 */
	bool (*can_place_escorts)(s16b r_idx);

	/*
	 * Check if the player must be placed over grids that use CAVE_ROOM
	 */
	bool (*can_place_player_in_rooms)(void);

	/*
	 * Check if stairs can be placed on the given location
	 */
	bool (*can_place_stairs)(int y, int x);

	/*
	 * Adjust the number of stairs in a level
	 */
	int (*adjust_stairs_number)(int initial_amount);


	/*
	 * Check if fog must be placed on rooms
	 */
	bool (*can_place_fog_in_rooms)(void);

	/*
	 * Check if the look command can stop in the given feature
	 */
	bool (*can_target_feature)(int f_idx);

	/*
	 * Check if regions and walls of the current dungeon can be transformed
	 */
	bool (*can_be_transformed)(void);

	/*
	 * Check if a non-native monsters can be placed in an elemental grid
	 * Used in get_mon_num
	 */
	bool (*can_place_non_native_monsters)(void);

	/*
	 * Get the initial number of monsters in the level
	 */
	int (*get_monster_count)(void);

	/*
	 * Get the initial number of objects in the level (rooms only)
	 */
	int (*get_object_count)(void);

	/*
	 * Get the initial number of gold objects in the level (rooms and corridors)
	 */
	int (*get_gold_count)(void);

	/*
	 * Get the initial number of extra objects in the level (rooms and corridors)
	 */
	int (*get_extra_object_count)(void);
};

#endif /* INCLUDED_TYPES_H */

