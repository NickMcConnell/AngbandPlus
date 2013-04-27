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



/**** Available Types ****/


/*
 * An array of 256 byte's
 */
typedef byte byte_256[256];


/*
 * An array of DUNGEON_WID byte's
 */
typedef byte byte_wid[DUNGEON_WID];

/*
 * An array of DUNGEON_WID s16b's
 */
typedef s16b s16b_wid[DUNGEON_WID];



/**** Available Structs ****/

/* these typedefs are unnecessary in C++ */
#ifndef __cplusplus
typedef struct _grid _grid;
typedef struct dice_sides dice_sides;

typedef struct maxima maxima;
typedef struct feature_type feature_type;
typedef struct object_kind object_kind;
typedef struct artifact_type artifact_type;
typedef struct ego_item_type ego_item_type;
typedef struct monster_blow monster_blow;
typedef struct monster_race monster_race;
typedef struct monster_lore monster_lore;
typedef struct vault_type vault_type;
typedef struct object_type object_type;
typedef struct monster_type monster_type;
typedef struct alloc_entry alloc_entry;
typedef struct quest quest;
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
typedef struct flavor_type flavor_type;
#endif


/**** Available structs ****/

/* Get the Boost-licensed types */
#include "types2.h"

/*
 * hook types for AI
 */
/* figure out how to disconnect this type from using Koeneke license code (problem is z-rand.h, damroll()) */
struct dice_sides
{
	byte dice;	/* dice */
	byte sides;	/* size */

	bool operator==(dice_sides RHS) const {return dice==RHS.dice && sides==RHS.sides;};
	bool operator!=(dice_sides RHS) const {return !(*this==RHS);};

	int maxroll() const {return (int)dice*(int)sides;};
	/* these two are technically wrong when d!=0, s=0, but that's a degenerate case */
	int minroll() const {return dice;};
	int medianroll() const {return (int)dice*((int)sides+1)/2;};

	/* STL interfaces */
	void clear() {dice = 0; sides = 0;};

	/* util.c */
	int damroll() const;
};


/*
 * Information about maximal indices of certain arrays
 * Actually, these are not the maxima, but the maxima plus one
 *
 * In spite of most of these variables being natural for static variables elsewhere,
 * they are needed in this structure for restoration from binary cached data.
 */
struct maxima
{
	u32b fake_text_size;
	u32b fake_name_size;

	u16b f_max;		/* Max size for "f_info[]" */
	u16b k_max;		/* Max size for "k_info[]" */
	u16b a_max;		/* Max size for "a_info[]" */
	u16b e_max;		/* Max size for "e_info[]" */
	u16b r_max;		/* Max size for "r_info[]" */
	u16b v_max;		/* Max size for "v_info[]" */
	u16b p_max;		/* Max size for "p_info[]" */
	u16b h_max;		/* Max size for "h_info[]" */
	u16b b_max;		/* Max size per element of "b_info[]" */
	u16b c_max;		/* Max size for "c_info[]" */
	u16b flavor_max; /* Max size for "flavor_info[]" */

	u16b o_max;		/* Max size for "o_list[]" */
	u16b m_max;		/* Max size for "mon_list[]" */
};


/*
 * Information about terrain "features"
 */
struct feature_type
{
	static feature_type *f_info;
	static char *f_name;
	static char *f_text;

	u32b _name;			/* Name (offset) */
	u32b _text;			/* Text (offset) */

	byte mimic;			/* Feature to mimic */

	byte extra;			/* Extra byte (unused) */

	s16b unused;		/* Extra bytes (unused) */


	byte d_attr;		/* Default feature attribute */
	char d_char;		/* Default feature character */


	byte x_attr;		/* Desired feature attribute */
	char x_char;		/* Desired feature character */

	const char* name() const {return f_name+_name;};
	const char* text() const {return f_text+_text;};
};

struct flavor_type
{
	static char *flavor_name;
	static char *flavor_text;

	u32b _text;      /* Text (offset) */
	
	byte tval;      /* Associated object type */
	byte sval;      /* Associated object sub-type */

	byte d_attr;    /* Default flavor attribute */
	char d_char;    /* Default flavor character */

	byte x_attr;    /* Desired flavor attribute */
	char x_char;    /* Desired flavor character */

	const char* text() const {return flavor_text+_text;};
};


/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */
struct object_kind
{
/*
 * The object flavor arrays
 */
	static flavor_type *flavor_info;
	static char *k_name;
	static char *k_text;

	u32b _name;			/* Name (offset) */
	u32b _text;			/* Text (offset) */

	byte tval;			/* Object type */
	byte sval;			/* Object sub type */

	s16b pval;			/* Object extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	dice_sides d;		/* Damage dice/sides */

	s16b weight;		/* Weight */

	s32b cost;			/* Object "base cost" */

	u32b flags1;		/* Flags, set 1 */
	u32b flags2;		/* Flags, set 2 */
	u32b flags3;		/* Flags, set 3 */

	byte locale[4];		/* Allocation level(s) */
	byte chance[4];		/* Allocation chance(s) */

	byte level;			/* Level */
	byte extra;			/* Something */


	byte d_attr;		/* Default object attribute */
	char d_char;		/* Default object character */


	byte x_attr;		/* Desired object attribute */
	char x_char;		/* Desired object character */

	u16b time_base;		/* Recharge time (if appropriate) */
	dice_sides time;	/* dice defaults to 1 right now, redo later */

	u16b flavor;		/* Special object flavor (or zero) */

	/* next two could be per-character ... */
	bool aware;			/* The player is "aware" of the item's effects */
	bool tried;			/* The player has "tried" one of the items */

	const char* name() const {return k_name+_name;};
	const char* text() const {return k_text+_text;};

	/* display interpretation */
	byte attr_user() const {return use_flavor_glyph() ? flavor_info[flavor].x_attr : x_attr;};
	char char_user() const {return use_flavor_glyph() ? flavor_info[flavor].x_char : x_char;};
	byte attr_default() const {return use_flavor_glyph() ? flavor_info[flavor].d_attr : d_attr;};
	char char_default() const {return use_flavor_glyph() ? flavor_info[flavor].d_char : d_char;};

	/* more complicated functions */
/*
 * Determine if the attr and char should consider the item's flavor
 *
 * Identified scrolls should use their own tile.
 */
	bool use_flavor_glyph() const {return flavor &&  !(TV_SCROLL==tval && aware);};
};



/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile.
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */
struct artifact_type
{
	static char* a_name;
	static char* a_text;

	u32b _name;			/* Name (offset) */
	u32b _text;			/* Text (offset) */

	byte tval;			/* Artifact type */
	byte sval;			/* Artifact sub type */

	s16b pval;			/* Artifact extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	dice_sides d;		/* Damage when hits */

	s16b weight;		/* Weight */

	s32b cost;			/* Artifact "cost" */

	u32b flags1;		/* Artifact Flags, set 1 */
	u32b flags2;		/* Artifact Flags, set 2 */
	u32b flags3;		/* Artifact Flags, set 3 */

	byte level;			/* Artifact level */
	byte rarity;		/* Artifact rarity */

	byte cur_num;		/* Number created (0 or 1) */
	byte max_num;		/* Unused (should be "1") */

	byte activation;	/* Activation to use */
	u16b time_base;		/* Recharge time (if appropriate) */
	dice_sides time;	/* dice defaults to 1 right now, redo later */

	const char* name() const {return a_name+_name;};
	const char* text() const {return a_text+_text;};
};


/*
 * Information about "ego-items".
 */
struct ego_item_type
{
	static char* e_name;
	static char* e_text;

	u32b _name;			/* Name (offset) */
	u32b _text;			/* Text (offset) */

	s32b cost;			/* Ego-item "cost" */

	u32b flags1;		/* Ego-Item Flags, set 1 */
	u32b flags2;		/* Ego-Item Flags, set 2 */
	u32b flags3;		/* Ego-Item Flags, set 3 */

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

	const char* name() const {return e_name+_name;};
	const char* text() const {return e_text+_text;};
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
	dice_sides d;
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
	static char *r_name;
	static char *r_text;

	u32b _name;				/* Name (offset) */
	u32b _text;				/* Text (offset) */

	dice_sides h;				/* Creatures hit dice/sides count */

	s16b ac;				/* Armour Class */

	s16b sleep;				/* Inactive counter (base) */
	byte aaf;				/* Area affect radius (1-100) */
	byte speed;				/* Speed (normally 110) */

	s32b mexp;				/* Exp value for kill */

	s16b extra;				/* Unused (for now) */

	byte freq_innate;		/* Innate spell frequency */
	byte freq_spell;		/* Other spell frequency */

	u32b flags1;			/* Flags 1 (general) */
	u32b flags2;			/* Flags 2 (abilities) */
	u32b flags3;			/* Flags 3 (race/resist) */
	u32b flags4;			/* Flags 4 (inate/breath) */
	u32b flags5;			/* Flags 5 (normal spells) */
	u32b flags6;			/* Flags 6 (special spells) */

	monster_blow blow[MONSTER_BLOW_MAX]; /* Up to four blows per round */

	byte level;				/* Level of creature */
	byte rarity;			/* Rarity of creature */

	byte d_attr;			/* Default monster attribute */
	char d_char;			/* Default monster character */

	byte x_attr;			/* Desired monster attribute */
	char x_char;			/* Desired monster character */

	byte max_num;			/* Maximum population allowed per level */
	byte cur_num;			/* Monster population on current level */

	const char* name() const {return r_name+_name;};
	const char* text() const {return r_text+_text;};
};


/*
 * Monster "lore" information
 *
 * Note that these fields are related to the "monster recall" and can
 * be scrapped if space becomes an issue, resulting in less "complete"
 * monster recall (no knowledge of spells, etc). XXX XXX XXX
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

	byte cast_innate;		/* Max number of innate spells seen */
	byte cast_spell;		/* Max number of other spells seen */

	byte blows[MONSTER_BLOW_MAX]; /* Number of times each blow type was seen */

	u32b flags1;			/* Observed racial flags */
	u32b flags2;			/* Observed racial flags */
	u32b flags3;			/* Observed racial flags */
	u32b flags4;			/* Observed racial flags */
	u32b flags5;			/* Observed racial flags */
	u32b flags6;			/* Observed racial flags */
};



/*
 * Information about "vault generation"
 */
struct vault_type
{
	static vault_type *v_info;
	static char *v_name;
	static char *v_text;

	u32b _name;			/* Name (offset) */
	u32b _text;			/* Text (offset) */

	byte typ;			/* Vault type */

	byte rat;			/* Vault rating */

	byte hgt;			/* Vault height */
	byte wid;			/* Vault width */

	const char* name() const {return v_name+_name;};
	const char* text() const {return v_text+_text;};
};

/*
 * Object information, for a specific object.
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
/*
 * The object kind arrays
 */
	static object_kind *k_info;
	static artifact_type *a_info;
	static ego_item_type *e_info;

	s16b k_idx;			/* Kind index (zero if "dead") */

	coord loc;			/* position on map, or (0,0) */

	byte tval;			/* Item type (from kind) */
	byte sval;			/* Item sub-type (from kind) */

	s16b pval;			/* Item extra-parameter */

	byte pseudo;		/* Pseudo-ID marker */
	byte number;		/* Number of items */

	s16b weight;		/* Item weight */

	byte name1;			/* Artifact type, if any */
	byte name2;			/* Ego-Item type, if any */

	byte xtra1;			/* Extra info type */
	byte xtra2;			/* Extra info index */

	s16b to_h;			/* Plusses to hit */
	s16b to_d;			/* Plusses to damage */
	s16b to_a;			/* Plusses to AC */

	s16b ac;			/* Normal AC */

	dice_sides d;		/* Damage dice/sides */

	s16b timeout;		/* Timeout Counter */

	byte ident;			/* Special flags */

	byte marked;		/* Object is marked */

	u16b note;			/* Inscription index */

	s16b next_o_idx;	/* Next object in stack (if any) */

	s16b held_m_idx;	/* Monster holding us (if any) */

	/* member functions */
	bool is_artifact() const {return name1;};
	bool is_ego_item() const {return name2;};

	bool is_broken() const {return ident & (IDENT_BROKEN);};
	bool is_cursed() const {return ident & (IDENT_CURSED);};
	bool is_broken_or_cursed() const {return ident & (IDENT_BROKEN | IDENT_CURSED);};

	void sense(byte new_pseudo) { pseudo = new_pseudo; ident |= (IDENT_SENSE);};

	/* k_info reflection functions */
	bool aware() const {return k_info[k_idx].aware;};
	bool tried() const {return k_info[k_idx].tried;};
	byte level() const {return k_info[k_idx].level;};
	
	byte attr_user() const {return k_info[k_idx].attr_user();};
	char char_user() const {return k_info[k_idx].char_user();};
	byte attr_default() const {return k_info[k_idx].attr_default();};
	char char_default() const {return k_info[k_idx].char_default();};

	/* more complicated functions */
/*
 * Determine if a given inventory item is "known"
 * Test One -- Check for special "known" tag
 * Test Two -- Check for "Easy Know" + "Aware"
 */
	bool known() const {return (ident & IDENT_KNOWN) || ((k_info[k_idx].flags3 & TR3_EASY_KNOW) && aware());};
};

typedef bool object_action(object_type& o);


/*
 * Monster information, for a specific monster.
 *
 * The "hold_o_idx" field points to the first object of a stack
 * of objects (if any) being carried by the monster (see above).
 */
struct monster_type
{
	static monster_race *r_info;
	static monster_lore *l_list;


	s16b r_idx;			/* Monster race index */

	coord loc;	/* location on map */

	s16b hp;			/* Current Hit points */
	s16b maxhp;			/* Max Hit points */

	s16b csleep;		/* Inactive counter */

	byte mspeed;		/* Monster "speed" */
	byte energy;		/* Monster "energy" */

	byte stunned;		/* Monster is stunned */
	byte confused;		/* Monster is confused */
	byte monfear;		/* Monster is afraid */

	byte cdis;			/* Current dis from player */

	byte mflag;			/* Extra monster flags */

	bool ml;			/* Monster is "visible" */

	s16b hold_o_idx;	/* Object being held (if any) */

	u32b smart;			/* Field for "smart_learn" */


	monster_race* race() const {return &r_info[r_idx];};
	monster_lore* lore() const {return &l_list[r_idx];};

	/* melee1.c */
	void melee_analyze(int& min_dam, int& median_dam, int& max_dam,coord g);

	/* melee2.c */
	void wake_up();
	void disturb(int d);
};

typedef bool monster_action(monster_type& m);


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
 *
 * Hack -- currently, only the "level" parameter is set, with the
 * semantics that "one (QUEST) monster of that level" must be killed,
 * and then the "level" is reset to zero, meaning "all done".  Later,
 * we should allow quests like "kill 100 fire hounds", and note that
 * the "quest level" is then the level past which progress is forbidden
 * until the quest is complete.  Note that the "QUESTOR" flag then could
 * become a more general "never out of depth" flag for monsters.
 */
struct quest
{
	byte level;		/* Dungeon level */
	int r_idx;		/* Monster race */

	int cur_num;	/* Number killed (unused) */
	int max_num;	/* Number required (unused) */
};




/*
 * A store owner
 */
struct owner_type
{
	u32b owner_name;	/* Name (offset) */

	s32b max_cost;		/* Purse limit */

	byte inflate;		/* Inflation */

	byte owner_race;	/* Owner race */
};




/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store_type
{
	byte owner;				/* Owner index */

	byte stock_num;			/* Stock -- Number of entries */
	s16b stock_size;		/* Stock -- Total Size of Array */
	object_type *stock;		/* Stock -- Actual stock items */

	s16b table_num;     /* Table -- Number of entries */
	s16b table_size;    /* Table -- Total Size of Array */
	s16b *table;        /* Table -- Legal item kinds */
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
	char terse;			/* abbreviation */
};

struct base_mod
{
	byte b;	/* base */
	byte m;	/* mod */

	byte linear_rand() const {return b+randint(m); };
	byte normal_rand() const {return Rand_normal(b,m); };
};

/*
 * Player racial info
 */
struct player_race
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	s16b r_adj[A_MAX];	/* Racial stat bonuses */

	s16b r_skills[SKILL_MAX-2];	/* racial skills */

	byte r_mhp;			/* Race hit-dice modifier */
	byte r_exp;			/* Race experience factor */

	base_mod age;		/* age */

	base_mod height[MAX_SEXES];	/* height */
	base_mod weight[MAX_SEXES];	/* weight */
	
	byte infra;			/* Infra-vision	range */

	byte choice;		/* Legal class choices */

	s16b hist;			/* Starting history index */

	u32b flags1;		/* Racial Flags, set 1 */
	u32b flags2;		/* Racial Flags, set 2 */
	u32b flags3;		/* Racial Flags, set 3 */
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

	s16b c_skills[SKILL_MAX-2];	/* class skills */
	s16b x_skills[SKILL_MAX-2];	/* extra skills */

	s16b c_mhp;			/* Class hit-dice adjustment */
	s16b c_exp;			/* Class experience factor */

	u32b flags;			/* Class Flags */

	u16b max_attacks;	/* Maximum possible attacks */
	u16b min_weight;	/* Minimum weapon weight for calculations */
	u16b att_multiply;	/* Multiplier for attack calculations */

	byte spell_book;	/* Tval of spell books (if any) */
	u16b spell_stat;	/* Stat for spells (if any) */
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
	static player_race *p_info;
	static char *p_name;
	static char *p_text;
	static player_class *c_info;
	static char *c_name;
	static char *c_text;
	static hist_type *h_info;
	static char *h_text;
	static const player_sex sex_info[MAX_SEXES];

	static const char* race_name(size_t i) { return p_name + p_info[i].name; };
	static const char* class_name(size_t i) { return c_name + c_info[i].name; };

	coord loc;	/* Player location */

	byte psex;			/* Sex index */
	byte prace;			/* Race index */
	byte pclass;		/* Class index */
	byte oops;			/* Unused */

	byte hitdie;		/* Hit dice (sides) */
	byte expfact;		/* Experience factor */

	s16b age;			/* Characters age */
	s16b ht;			/* Height */
	s16b wt;			/* Weight */
	s16b sc;			/* Social Class */

	s32b au;			/* Current Gold */

	s16b max_depth;		/* Max depth */
	s16b depth;			/* Cur depth */

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

	s16b timed[TMD_MAX];	/* Timed effects */

	s16b word_recall;	/* Word of recall counter */

	s16b energy;		/* Current energy */

	s16b food;			/* Current nutrition */

	byte confusing;		/* Glowing hands */
	byte searching;		/* Currently searching */

	byte spell_flags[PY_MAX_SPELLS]; /* Spell flags */

	byte spell_order[PY_MAX_SPELLS];	/* Spell order */

	s16b player_hp[PY_MAX_LEVEL];	/* HP Array */

	char died_from[80];		/* Cause of death */
	char history[250];	/* Initial history */

	u16b total_winner;		/* Total winner */
	u16b panic_save;		/* Panic save */

	u16b noscore;			/* Cheating flags */

	bool is_dead;			/* Player is dead */

	bool wizard;			/* Player is in wizard mode */

	object_type* inventory;	/* Array[INVEN_TOTAL] of objects in the player's inventory */

	/*** Temporary fields ***/

	bool playing;			/* True if player is playing */

	bool leaving;			/* True if player is leaving */

	bool create_up_stair;	/* Create up stair on next level */
	bool create_down_stair;	/* Create down stair on next level */

	s32b total_weight;		/* Total weight being carried */

	s16b inven_cnt;			/* Number of items in inventory */
	s16b equip_cnt;			/* Number of items in equipment */

	s16b target_set;		/* Target flag */
	s16b target_who;		/* Target identity */
	coord target;			/* Target location */

	s16b health_who;		/* Health bar trackee */

	s16b monster_race_idx;	/* Monster race trackee */

	s16b object_kind_idx;	/* Object kind trackee */

	s16b energy_use;		/* Energy use this turn */

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

	bool sustain[A_MAX];/* Keep strength, etc. (cf. stat_index enumeration) */

	bool slow_digest;	/* Slower digestion */
	bool ffall;			/* Feather falling */
	bool lite;			/* Permanent light */
	bool regenerate;	/* Regeneration */
	bool telepathy;		/* Telepathy */
	bool see_inv;		/* See invisible */
	bool free_act;		/* Free action */
	bool hold_life;		/* Hold life */

	bool impact;		/* Earthquake blows */
	bool aggravate;		/* Aggravate monsters */
	bool teleport;		/* Random teleporting */
	bool exp_drain;		/* Experience draining */

	bool bless_blade;	/* Blessed blade */

	s16b dis_to_h;		/* Known bonus to hit */
	s16b dis_to_d;		/* Known bonus to dam */
	s16b dis_to_a;		/* Known bonus to ac */

	s16b dis_ac;		/* Known base ac */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to dam */
	s16b to_a;			/* Bonus to ac */

	s16b ac;			/* Base ac */

	s16b see_infra;		/* Infravision range */

	s16b skills[SKILL_MAX];	/* Skills */

	u32b noise;			/* Derived from stealth */

	s16b num_blow;		/* Number of blows */
	s16b num_fire;		/* Number of shots */

	byte ammo_mult;		/* Ammo multiplier */

	byte ammo_tval;		/* Ammo variety */

	s16b pspeed;		/* Current speed */

	/* pointers to player information */
	const player_race *rp_ptr;
	const player_class *cp_ptr;

	const magic_type* spell_info(size_t spell) const { assert(PY_MAX_SPELLS>spell); return &mp_ptr->info[spell]; };
	byte spell_book() const {return cp_ptr->spell_book; };
	const char* gender() const { return sp_ptr->title; };
	char terse_gender() const { return sp_ptr->terse; };
	const char* win_rank() const { return sp_ptr->winner; };
	const char* racename() const { return p_name + rp_ptr->name; };
	const char* classname() const { return c_name + cp_ptr->name; };

	void set_race(byte new_race)
	{
		prace = new_race;
		rp_ptr = &p_info[new_race];
	}

	void set_sex(byte new_sex)
	{
		assert(MAX_SEXES>new_sex);
		psex = new_sex;
		sp_ptr = &sex_info[new_sex];	
	};

	void set_class(byte new_class)
	{
		pclass = new_class;
		cp_ptr = &c_info[new_class];	// initialize class
		mp_ptr = &cp_ptr->spells;		// initialize magic
	};

	bool std_save() const {return rand_int(100) < skills[SKILL_SAVE];};
	int total_ac() const {return ac+to_a;};

	/* attack.c */
	void melee_analyze(monster_type* m_ptr, int& min_dam, int& median_dam, int& max_dam);
	void missile_analyze(monster_type* m_ptr, int& min_dam, int& median_dam, int& max_dam);

	/* cmd2.c */
	int disarm_skill() const;
	bool disarm_trap(int power) const;

	/* cmd6.c */
	int item_chance(int lev) const;

	/* files.c */
	const char* title() const;
	void flags(u32b& f1, u32b& f2, u32b& f3) const;

	/* xtra2.c */
private:
	bool set_timed_clean(int idx, int v);
public:
	bool dec_timed(int idx, int v);
#ifdef ZAIBAND_STATIC_ASSERT
	/* compile-time checking */
	template<timed_effects idx> bool set_timed(int v) {ZAIBAND_STATIC_ASSERT((0 <= idx) && (TMD_MAX>idx)); return set_timed_clean(idx,v);}
	template<timed_effects idx> bool inc_timed(int v) {ZAIBAND_STATIC_ASSERT((0 <= idx) && (TMD_MAX>idx)); return set_timed_clean(idx,timed[idx]+v);}
	template<timed_effects idx> bool dec_timed(int v) {ZAIBAND_STATIC_ASSERT((0 <= idx) && (TMD_MAX>idx)); return set_timed_clean(idx,timed[idx]-v);}
	template<timed_effects idx> bool clear_timed(void) {ZAIBAND_STATIC_ASSERT((0 <= idx) && (TMD_MAX>idx)); return set_timed_clean(idx,0);}
#else
	bool set_timed(int idx, int v);
	bool inc_timed(int idx, int v);
	bool clear_timed(int idx) { return set_timed(idx, 0); }

	template<timed_effects idx> bool set_timed(int v) {return set_timed(idx,v);}
	template<timed_effects idx> bool inc_timed(int v) {return inc_timed(idx,timed[idx]+v);}
	template<timed_effects idx> bool dec_timed(int v) {return dec_timed(idx,timed[idx]-v);}
	template<timed_effects idx> bool clear_timed(void) {return set_timed(idx,0);}
#endif

	/* xtra3.c */
	bool allow_moron() const;

private:
	/* pointers to player information */
	const player_sex *sp_ptr;
	const player_magic *mp_ptr;
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

/* ============= Defines a visual grouping ============ */
typedef struct
{
	byte tval;
	cptr name;
} grouper;


enum grid_light_level
{
	LIGHT_TORCH,
	LIGHT_GLOW,
	LIGHT_DARK
};

typedef struct
{
	u32b m_idx;		/* Monster index */
	u32b f_idx;		/* Feature index */
	u32b first_k_idx;	/* The "Kind" of the first item on the grid */
	bool multiple_objects;	/* Is there more than one item there? */

	enum grid_light_level lighting; /* Light level */
	bool in_view; /* TRUE when the player can currently see the grid. */
	bool is_player;
	bool hallucinate;
} grid_data;

