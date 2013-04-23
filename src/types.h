#define TYPES_H
/* File: types.h */

/* Purpose: global type declarations */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


/*
 * This file should ONLY be included by "angband.h"
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
 * Certain data is saved in multiple places for efficient access, currently,
 * this includes the tval/weight fields in "object_type", various fields
 * in "header_type", and the "m_idx" and "o_idx" fields in "cave_type".  All
 * of these could be removed, but this would, in general, slow down the game
 * and increase the complexity of the code.
 */





/*
 * Template file header information (see "init.c").  16 bytes.
 *
 * Note that the sizes of many of the "arrays" are between 32768 and
 * 65535, and so we must use "unsigned" values to hold the "sizes" of
 * these arrays below.  Normally, I try to avoid using unsigned values,
 * since they can cause all sorts of bizarre problems, but I have no
 * choice here, at least, until the "race" array is split into "normal"
 * and "unique" monsters, which may or may not actually help.
 *
 * Note that, on some machines, for example, the Macintosh, the standard
 * "read()" and "write()" functions cannot handle more than 32767 bytes
 * at one time, so we need replacement functions, see "util.c" for details.
 *
 * Note that, on some machines, for example, the Macintosh, the standard
 * "malloc()" function cannot handle more than 32767 bytes at one time,
 * but we may assume that the "ralloc()" function can handle up to 65535
 * butes at one time.  We should not, however, assume that the "ralloc()"
 * function can handle more than 65536 bytes at a time, since this might
 * result in segmentation problems on certain older machines, and in fact,
 * we should not assume that it can handle exactly 65536 bytes at a time,
 * since the internal functions may use an unsigned short to specify size.
 *
 * In general, these problems occur only on machines (such as most personal
 * computers) which use 2 byte "int" values, and which use "int" for the
 * arguments to the relevent functions.
 */

typedef struct header header;

typedef errr (*parse_info_txt_func)(char *buf, header *head, vptr *extra);

struct header
{
	char version[6];	/* Version */

	u16b info_num;		/* Number of "info" records */

	u16b info_len;		/* Size of each "info" record */

	u32b info_size;		/* Size of the "info" array in bytes */

	u32b name_size;		/* Size of the "name" array in bytes */

	u32b text_size;		/* Size of the "text" array in bytes */

	cptr file_name;		/* Base name of the file to be parsed. */

	void *info_ptr; 
	char *name_ptr; 
	char *text_ptr; 
	void *fake_info_ptr;
	char *fake_name_ptr;
	char *fake_text_ptr;

	parse_info_txt_func parse_info_txt;

	byte header_num;
};

/*
 * A set of macros used to turn a relatively pretty text file into something
 * the file parser understands.
 *
 * This is the only way to insert non-printable or 8-bit characters into text
 * files.
 */
typedef struct init_macro_type init_macro_type;

struct init_macro_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte file;			/* File restriction */
	char pref;			/* Prefix restriction */
	byte field;			/* (colon-delimited) field */

	byte conv;			/* Type of conversion to be made. */
};

/*
 * Information about maximal indices of certain arrays
 * Actually, these are not the maxima, but the maxima plus one
 */
typedef struct maxima maxima;
struct maxima
{
#ifdef ALLOW_TEMPLATES
	u32b fake_text_size;
	u32b fake_name_size;
	u32b fake_info_size;
#else /* ALLOW_TEMPLATES */
	u32b unused[3];
#endif /* ALLOW_TEMPLATES */

	u16b o_max;		/* Max size for "o_list[]" */
	u16b m_max;		/* Max size for "m_list[]" */
	u16b oname;		/* Maximum length of object_desc() strings. */
	u16b mname;		/* Usual maximum length of monster_desc() strings. */
	u16b ar_delay;	/* Delay between rolls of the auto-roller. */

	u16b macros;	/* Total size of "macro_info" */
	u16b v_max;		/* Total size of "v_info[]" */
	u16b f_max;		/* Total size of "f_info[]" */
	u16b k_max;		/* Total size of "k_info[]" */
	u16b u_max;		/* Total size of "u_info[]" */
	u16b ob_max;	/* Total size of "o_base[]" */
	u16b a_max;		/* Total size of "a_info[]" */
	u16b e_max;		/* Total size of "e_info[]" */
	u16b r_max;		/* Total size of "r_info[]" */
	u16b event_max;	/* Total size of "death_events[]" */
	u16b p_max;		/* Total size of "p_info[]" */
	u16b h_max;		/* Total size of "h_info[]" */
	u16b b_max;		/* Total size per element of "b_info[]" */
	u16b flavor_max; /* Total size of "flavor_info[]" */
	u16b quests; /* Total size of "q_list[]" */
	u16b dungeons; /* Total size of "dun_defs[]" */
	u16b towns; /* Total size of "town_defs[]" */
	u16b owners; /* Total size of "owners[]" */
};


/*
 * Information about terrain "features"
 */

typedef struct feature_type feature_type;

struct feature_type
{
	u16b name;			/* Name (offset) */
	u16b text;			/* Text (offset) */

	byte mimic;			/* Feature to mimic */
	byte priority;		/* Priority for small-scale map. See priority(). */

	byte d_attr;		/* Object "attribute" */
	char d_char;		/* Object "symbol" */

	byte x_attr;		/* The desired attr for this feature */
	char x_char;		/* The desired char for this feature */
};


/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */

typedef struct object_kind object_kind;

struct object_kind
{
	u32b text;			/* Text (offset) */

	u16b name;			/* Name (offset) */
	byte tval;			/* Object type */
	byte extra;			/* Extra information related to the tval. */

	s16b pval;			/* Object extra info */
	s16b to_h;			/* Bonus to hit */

	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */
	byte dd, ds;		/* Damage dice/sides */

	u32b flags1;		/* Flags, set 1 */
	u32b flags2;		/* Flags, set 2 */
	u32b flags3;		/* Flags, set 3 */

	byte locale[4];		/* Allocation level(s) */
	byte chance[4];		/* Allocation chance(s) */

	s32b cost;			/* Object "base cost" */
	s16b weight;		/* Weight */

	byte d_attr;		/* Default object attribute */
	char d_char;		/* Default object character */
	byte x_attr;		/* Desired object attribute */
	char x_char;		/* Desired object character */

	u16b u_idx;	/* The u_info[] entry which represents this item. */
	bool aware;			/* The player is "aware" of the item's effects */
	bool tried;			/* The player has "tried" one of the items */

	/* u16b blank; */	/* Nothing */
};



/*
 * Information about the unidentified forms of object "kinds"
 */
typedef struct unident_type unident_type;

struct unident_type
{
	u16b name;	/* Name (offset) */

	byte p_id;	/* Primary index */
	byte s_id;	/* Secondary index (internally generated) */

	byte d_attr;		/* Default colour */
	char d_char;		/* Default symbol */

	byte x_attr;		/* The desired attr for this object */
	char x_char;		/* The desired char for this object */
};

typedef struct o_base_type o_base_type;

struct o_base_type
{
	u32b name;	/* Name (offset) */
	s32b cost;	/* Unaware cost */
	u32b flags1;	/* Expected flags, set 1 */
	u32b flags2;	/* Expected flags, set 2 */
	u32b flags3;	/* Expected flags, set 3 */
	byte tval;	/* The tval for this base type, TV_UNKNOWN if none. */
};

/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile.
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */

typedef struct artifact_type artifact_type;

struct artifact_type
{
	u16b name;			/* Name (offset) */
	u16b text;			/* Text (offset) */

	s16b k_idx;			/* Artifact type */

	s16b pval;			/* Artifact extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	byte dd, ds;		/* Damage when hits */

	s16b weight;		/* Weight */

	s32b cost;			/* Artifact "cost" */

	u32b flags1;		/* Artifact Flags, set 1 */
	u32b flags2;		/* Artifact Flags, set 2 */
	u32b flags3;		/* Artifact Flags, set 3 */

	byte level;			/* Artifact level */
	byte level2;		/* Secondary level for "special" artifacts */
	byte rarity;		/* Artifact rarity */

	byte cur_num;		/* Number created (0 or 1) */
};


/*
 * Information about "ego-items".
 */

typedef struct ego_item_type ego_item_type;

struct ego_item_type
{
	u16b name;			/* Name (offset) */
	u16b text;			/* Text (offset) */

	byte slot;			/* Standard slot value */
	byte rating;		/* Rating boost */

	byte level;			/* Minimum level */
	byte rarity;		/* Object rarity */

	byte max_to_h;		/* Maximum to-hit bonus */
	byte max_to_d;		/* Maximum to-dam bonus */
	byte max_to_a;		/* Maximum to-ac bonus */

	byte max_pval;		/* Maximum pval */

	s32b cost;			/* Ego-item "cost" */

	u32b flags1;		/* Ego-Item Flags, set 1 */
	u32b flags2;		/* Ego-Item Flags, set 2 */
	u32b flags3;		/* Ego-Item Flags, set 3 */
};




/*
 * Monster blow structure
 *
 *	- Method (RBM_*)
 *	- Effect (RBE_*)
 *	- Damage Dice
 *	- Damage Sides
 */

typedef struct monster_blow monster_blow;

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
 * Note that several of these fields, related to "recall", can be
 * scrapped if space becomes an issue, resulting in less "complete"
 * monster recall (no knowledge of spells, etc).  All of the "recall"
 * fields have a special prefix to aid in searching for them.
 */


typedef struct monster_race monster_race;

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

	byte freq_inate;		/* Inate spell frequency */
	byte freq_spell;		/* Other spell frequency */

	u32b flags1;			/* Flags 1 (general) */
	u32b flags2;			/* Flags 2 (abilities) */
	u32b flags3;			/* Flags 3 (race/resist) */
	u32b flags4;			/* Flags 4 (inate/breath) */
	u32b flags5;			/* Flags 5 (normal spells) */
	u32b flags6;			/* Flags 6 (special spells) */

	monster_blow blow[4];	/* Up to four blows per round */


	byte level;				/* Level of creature */
	byte rarity;			/* Rarity of creature */


	byte d_attr;			/* Default monster attribute */
	char d_char;			/* Default monster character */


	byte x_attr;			/* Desired monster attribute */
	char x_char;			/* Desired monster character */


	byte max_num;			/* Maximum population allowed per level */

	byte cur_num;			/* Monster population on current level */


	s16b r_sights;			/* Count sightings of this monster */
	s16b r_deaths;			/* Count deaths from this monster */

	s16b r_pkills;			/* Count monsters killed in this life */
	s16b r_tkills;			/* Count monsters killed in all lives */

	byte r_wake;			/* Number of times woken up (?) */
	byte r_ignore;			/* Number of times ignored (?) */

	byte r_drop_gold;		/* Max number of gold dropped at once */
	byte r_drop_item;		/* Max number of item dropped at once */

	byte r_cast_inate;		/* Max number of inate spells seen */
	byte r_cast_spell;		/* Max number of other spells seen */

	byte num_blows;        /* Attack speed (equates to p_ptr->num_blows) */
	byte r_blows[4];		/* Number of times each blow type was seen */

	u32b r_flags1;			/* Observed racial flags */
	u32b r_flags2;			/* Observed racial flags */
	u32b r_flags3;			/* Observed racial flags */
	u32b r_flags4;			/* Observed racial flags */
	u32b r_flags5;			/* Observed racial flags */
	u32b r_flags6;			/* Observed racial flags */
};



/*
 * Information about "vault generation"
 */

typedef struct vault_type vault_type;

struct vault_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte typ;			/* Vault type */

	byte rat;			/* Vault rating */

	byte hgt;			/* Vault height */
	byte wid;			/* Vault width */
};





/*
 * A single "grid" in a Cave
 *
 * Note that several aspects of the code restrict the actual cave
 * to a max size of 256 by 256.  In partcular, locations are often
 * saved as bytes, limiting each coordinate to the 0-255 range.
 *
 * The "o_idx" and "m_idx" fields are very interesting.  There are
 * many places in the code where we need quick access to the actual
 * monster or object(s) in a given cave grid.  The easiest way to
 * do this is to simply keep the index of the monster and object
 * (if any) with the grid, but this takes 198*66*4 bytes of memory.
 * Several other methods come to mind, which require only half this
 * amound of memory, but they all seem rather complicated, and would
 * probably add enough code that the savings would be lost.  So for
 * these reasons, we simply store an index into the "o_list" and
 * "m_list" arrays, using "zero" when no monster/object is present.
 *
 * Note that "o_idx" is the index of the top object in a stack of
 * objects, using the "next_o_idx" field of objects (see below) to
 * create the singly linked list of objects.  If "o_idx" is zero
 * then there are no objects in the grid.
 *
 * Note the special fields for the "MONSTER_FLOW" code.
 */

typedef struct cave_type cave_type;

struct cave_type
{
	u16b info;		/* Hack -- cave flags */

	byte feat;		/* Hack -- feature type */

	s16b o_idx;		/* Object in this grid */

	s16b m_idx;		/* Monster in this grid */

#ifdef MONSTER_FLOW

	byte cost;		/* Hack -- cost of flowing */
	byte when;		/* Hack -- when cost was computed */

#endif

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

typedef struct object_type object_type;

struct object_type
{
	s16b k_idx;			/* Kind index (zero if "dead") */
	byte iy;			/* Y-position on map, or zero */
	byte ix;			/* X-position on map, or zero */

	byte tval;			/* Item type (from kind) */
	byte discount;		/* Discount (if any) */
	byte number;		/* Number of items */
	byte marked;		/* Object is marked */

	byte name1;			/* Artifact type, if any */
	byte name2;			/* Ego-Item type, if any */
	byte xtra1;			/* Extra info type */
	byte xtra2;			/* Extra info index */

	s16b to_h;			/* Plusses to hit */
	s16b to_d;			/* Plusses to damage */

	s16b to_a;			/* Plusses to AC */
	s16b ac;			/* Normal AC */

	s16b pval;			/* Item extra-parameter */
	s16b timeout;		/* Timeout Counter */

	s16b weight;		/* Item weight */
	byte dd, ds;		/* Damage dice/sides */

	u16b ident;			/* Special flags  */
	u16b note;			/* Inscription index */

    u16b art_name;      /* Artifact name (random artifacts) */
	/* u16b nothing; */	/* Unused */

    u32b flags1;        /* Flags, set 1 */
    u32b flags2;        /* Flags, set 2 */
    u32b flags3;        /* Flags, set 3 */

	
	s16b next_o_idx;	/* Next object in stack (if any) */
	s16b held_m_idx;	/* Monster holding us (if any) */
};



/*
 * Monster information, for a specific monster.
 *
 * Note: fy, fx constrain dungeon size to 256x256
 *
 * The "hold_o_idx" field points to the first object of a stack
 * of objects (if any) being carried by the monster (see above).
 */

typedef struct monster_type monster_type;

struct monster_type
{
	s16b r_idx;			/* Monster race index */

	byte fy;			/* Y location on map */
	byte fx;			/* X location on map */

	byte generation; /* Generation if a breeder */

	s16b hp;			/* Current Hit points */
	s16b maxhp;			/* Max Hit points */

	s16b csleep;		/* Inactive counter */

	byte mspeed;		/* Monster "speed" */
	s16b energy;		/* Monster "energy" */

	byte stunned;		/* Monster is stunned */
	byte confused;		/* Monster is confused */
	byte monfear;		/* Monster is afraid */

	byte cdis;			/* Current dis from player */

	byte mflag;			/* Extra monster flags */
	
	s16b gold;        /* Gold stolen from player */

	bool ml;			/* Monster is "visible" */

	s16b hold_o_idx;	/* Object being held (if any) */

#ifdef WDT_TRACK_OPTIONS

	byte ty;			/* Y location of target */
	byte tx;			/* X location of target */

	byte t_dur;			/* How long are we tracking */

	byte t_bit;			/* Up to eight bit flags */

#endif

#ifdef DRS_SMART_OPTIONS

	u32b smart;			/* Field for "smart_learn" */

#endif

};




/*
 * An entry for the object/monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */

typedef struct alloc_entry alloc_entry;

struct alloc_entry
{
	s16b index;		/* The actual index */

	byte level;		/* Base dungeon level */
	byte prob1;		/* Probability, pass 1 */
	byte prob2;		/* Probability, pass 2 */
	byte prob3;		/* Probability, pass 3 */
};



/*
 * Available "options"
 *
 *	- Address of actual option variable (or NULL)
 *
 *	- Normal Value (TRUE or FALSE)
 *
 *	- Option Page Number (or zero)
 *
 *	- Savefile Set (or zero)
 *	- Savefile Bit in that set
 *
 *	- Textual name (or NULL)
 *	- Textual description
 */

typedef struct option_type option_type;

struct option_type
{
	char	*o_var;

	byte	o_norm;

	byte	o_page;

	byte	o_set;
	byte	o_bit;

	cptr	o_text;
	cptr	o_desc;
};


/*
 * Structure to specify how some options can override other options.
 */
typedef struct force_type force_type;

struct force_type
{
	bool *forcing_opt;
	bool forcing_value;
	bool *forced_opt;
};


/*
 * Structure for monster memory colour map.
 */
typedef struct moncol_type moncol_type;

struct moncol_type
{
	cptr	name;
	byte	attr;
};

/*
 * Structure for the "quests"
 */

typedef struct quest_type quest_type;

struct quest_type
{
	s16b r_idx;		/* Monster race */
	byte level;		/* Dungeon level */
	byte dungeon; /* Dungeon containing quest */

	byte cur_num;	/* Number killed */
	byte cur_num_known;	/* Number known by the player to have been killed */
	byte max_num;	/* Number required */
	bool known;	/* The player has entered this quest. */
};




/*
 * A store owner
 */

typedef struct owner_type owner_type;

struct owner_type
{
	s16b name;	/* Name */

	s16b max_cost;		/* Purse limit */

	byte max_inflate;	/* Inflation (max) */
	byte min_inflate;	/* Inflation (min) */

	byte haggle_per;	/* Haggle unit */

	byte insult_max;	/* Insult limit */

	byte owner_race;	/* Owner race */

	byte shop_type;	/* The category of shop this shopkeeper owns. */
	byte town;	/* The town restriction, if any. */
};




/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */

typedef struct store_type store_type;

struct store_type
{
	byte x;                    /* Coords of store in town */
	byte y;

	byte type;               /* Type of store */ 
	byte bought;             /* Flag for player purchase (only used on houses) */
	s16b owner;			  /* Owner index */

	s16b insult_cur;		/* Insult counter */

	s16b good_buy;			/* Number of "good" buys */
	s16b bad_buy;			/* Number of "bad" buys */

	s32b store_open;		/* Closed until this turn */

	s16b stock_num;			/* Stock -- Number of entries */
	s16b stock_size;		/* Stock -- Total Size of Array */
	object_type *stock;		/* Stock -- Actual stock items */
};





typedef struct magic_type magic_type;

struct magic_type
{
	byte minskill;		/* Required skill (to learn) */
	byte smana;			/* Required mana (to cast) */
	byte sfail;			/* Minimum chance of failure */
	byte sexp;			/* Encoded experience bonus */
	byte sschool;        /* School of spell */
	byte stype;         /* Type of spell */
};

typedef struct favour_type favour_type;
struct favour_type
{
	byte minskill; /* Required skill to persuade */
	byte annoy_inc; /* Annoyance increase for spirit */
	byte sfail; /* Base chance of refusal */
};

typedef struct cantrip_type cantrip_type;
struct cantrip_type
{
	byte minskill; /* Required skill to cast */
	byte mana; /* Mana cost */
	byte sfail; /* Base chance of failure */
};


/*
 * Structure for the display windows.
 */
typedef struct window_type window_type;
struct window_type
{
	term *term;	/* The window in question */
	char name[16]; /* Name of the window */
	byte pri[32]; /* Priority of the specified display type for this window. */
	byte rep[32]; /* Maximum priority of display which this display type can replace. */
	byte current; /* Current display for this window */
	u32b mask;
};

/*
 * Information about the player's "magic"
 */

typedef struct player_magic player_magic;

struct player_magic
{
	s16b spell_weight;		/* Weight that hurts spells */
    magic_type info[MAX_SCHOOL][32];    /* The available spells */
};



/*
 * Player sex info
 */

typedef struct player_sex player_sex;

struct player_sex
{
	cptr title;			/* Type of sex */
	
	cptr winner;		/* Name of winner */
};


/*
 * Player racial info
 */

typedef struct player_race player_race;

struct player_race
{
	cptr title;			/* Type of race */

	s16b r_adj[A_MAX];		/* Racial stat bonuses */

	byte disarm_bonus;			/* disarming */
	byte device_bonus;			/* magic devices */
	byte save_bonus;			/* saving throw */
	byte stealth_bonus;			/* stealth */
	byte search_bonus;			/* search ability */
	byte perception_bonus;			/* search frequency */
	byte melee_bonus;			/* combat (normal) */
	byte missile_bonus;			/* combat (shooting) */

	byte r_mhp;			/* Race hit-dice modifier */
	byte r_exp;			/* Race experience factor */

	byte b_age;			/* base age */
	byte m_age;			/* mod age */

	byte m_b_ht;		/* base height (males) */
	byte m_m_ht;		/* mod height (males) */
	byte m_b_wt;		/* base weight (males) */
	byte m_m_wt;		/* mod weight (males) */

	byte f_b_ht;		/* base height (females) */
	byte f_m_ht;		/* mod height (females)	  */
	byte f_b_wt;		/* base weight (females) */
	byte f_m_wt;		/* mod weight (females) */

	byte infra;			/* Infra-vision	range */

    u16b choice;        /* Legal template choices */
	byte chart;		/* Initial chart for get_history() */
/*    byte choice_xtra;   */
};

/* 
 *Player Skill Info
 */

typedef struct player_skill player_skill;
struct player_skill {
		cptr name; /* Skill name */
		cptr increase; /* Message printed on an increase */
		u16b experience; /* Amount of times the skill has been used */
		byte value; /* The current skill level */
		byte max_value; /* Maximum value the skill has had */
		byte base; /* The skill level that may be raised to at dungeon level zero */
		byte ceiling; /* The absolute maximum the skill may be raised to */
		u16b exp_to_raise; /* The number of times the skill needs to be used for each raise */
};

/*
 * Player template info
 */

typedef struct player_template player_template;

struct player_template
{
	cptr title;			/* Type of template */

	int choices;		/* Number of choices for hermetic skills */

	s16b c_adj[A_MAX];		/* Template stat modifier */

	s16b skill[19];		/* Skill improvements */

};


/*
 * Most of the "player" information goes here.
 *
 * This stucture gives us a large collection of player variables.
 *
 * This structure contains several "blocks" of information.
 *   (1) the "permanent" info
 *   (2) the "variable" info
 *   (3) the "transient" info
 *
 * All of the "permanent" info, and most of the "variable" info,
 * is saved in the savefile.  The "transient" info is recomputed
 * whenever anything important changes.
 */

typedef struct player_type player_type;

struct player_type
{
	byte psex;			/* Sex index */
	byte prace;			/* Race index */
	byte ptemplate;		/* Template index */

	byte hitdie;		/* Hit dice (sides) */
    u16b expfact;       /* Experience factor
                            Note: was byte, causing overflow for Great One
                            characters (such as Great One Paladins) */

	byte ritual;			  /* Flag for recall ritual */

	s16b age;			/* Characters age */
	s16b ht;			/* Height */
	s16b wt;			/* Weight */
	s16b sc;			/* Social Class */
	s16b birthday;    /* Player's Birthday */
	s16b startdate;   /* The start date of the adventure */


	s32b au;			/* Current Gold */

	s32b exp;			/* Cur experience */
	u16b exp_frac;		/* Cur exp frac (times 2^16) */


	s16b mhp;			/* Max hit pts */
	s16b chp;			/* Cur hit pts */
	u16b chp_frac;		/* Cur hit frac (times 2^16) */

	s16b msp;			/* Max mana pts */
	s16b csp;			/* Cur mana pts */
	u16b csp_frac;		/* Cur mana frac (times 2^16) */

	s16b mchi; /* Max chi points */
	s16b cchi; /* Cur chi points */
	u16b chi_frac; /* Cur chi frac (times 2^16) */

	s16b max_dlv;		/* Max levels explored in current dungeon. */

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
	s16b blessed;		/* Timed -- Blessed */
	s16b tim_invis;		/* Timed -- See Invisible */
	s16b tim_infra;		/* Timed -- Infra Vision */

	s16b oppose_acid;	/* Timed -- oppose acid */
	s16b oppose_elec;	/* Timed -- oppose lightning */
	s16b oppose_fire;	/* Timed -- oppose heat */
	s16b oppose_cold;	/* Timed -- oppose cold */
	s16b oppose_pois;	/* Timed -- oppose poison */


    s16b tim_esp;       /* Timed ESP */
    s16b wraith_form;   /* Timed wraithform */

    s16b chaos_patron;
    u32b muta1;
    u32b muta2;
    u32b muta3;

	s16b word_recall;	/* Word of recall counter */

	s16b energy;		/* Current energy */

	s16b food;			/* Current nutrition */

	byte confusing;		/* Glowing hands */
	byte sneaking;		/* Currently searching */

	s16b new_spells;	/* Number of spells available */


	bool ma_cumber_armour; /* Martial arts limiting armour */
	bool cumber_armor;	/* Mana draining armor */
	bool cumber_glove;	/* Mana draining gloves */
	bool cumber_helm; /* Chi draining helm */
	bool heavy_wield;	/* Heavy weapon */
	bool heavy_shoot;	/* Heavy shooter */
	int wield_skill;	/* Weapon skill used for current weapon*/

	s16b cur_lite;		/* Radius of lite (if any) */


	u32b notice;		/* Special Updates (bit flags) */
	u32b update;		/* Pending Updates (bit flags) */
	u32b redraw;		/* Normal Redraws (bit flags) */
	u32b window;		/* Window Redraws (bit flags) */

	s16b stat_use[A_MAX];	/* Current modified stats */
	s16b stat_top[A_MAX];	/* Maximal modified stats */

	s16b stat_add[A_MAX];	/* Modifiers to stat values */
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

	bool resist_conf;	/* Resist confusion */
	bool resist_sound;	/* Resist sound */
	bool resist_lite;	/* Resist light */
	bool resist_dark;	/* Resist darkness */
	bool resist_chaos;	/* Resist chaos */
	bool resist_disen;	/* Resist disenchant */
	bool resist_shard;	/* Resist shards */
	bool resist_nexus;	/* Resist nexus */
	bool resist_blind;	/* Resist blindness */
	bool resist_neth;	/* Resist nether */
	bool resist_fear;	/* Resist fear */

    bool reflect;       /* Reflect 'bolt' attacks */
    bool sh_fire;       /* Fiery 'immolation' effect */
    bool sh_elec;       /* Electric 'immolation' effect */

    bool anti_magic;    /* Anti-magic */
    bool anti_tele;     /* Prevent teleportation */

	bool sustain_str;	/* Keep strength */
	bool sustain_int;	/* Keep intelligence */
	bool sustain_wis;	/* Keep wisdom */
	bool sustain_dex;	/* Keep dexterity */
	bool sustain_con;	/* Keep constitution */
	bool sustain_chr;	/* Keep charisma */

	bool aggravate;		/* Aggravate monsters */
	bool teleport;		/* Random teleporting */

	bool exp_drain;		/* Experience draining */

	bool ffall;			/* No damage falling */
	bool lite;			/* Permanent light */
	bool free_act;		/* Never paralyzed */
	bool see_inv;		/* Can see invisible */
	bool regenerate;	/* Regenerate hit pts */
	bool hold_life;		/* Resist life draining */
	bool telepathy;		/* Telepathy */
	bool slow_digest;	/* Slower digestion */
	bool bless_blade;	/* Blessed blade */
	bool xtra_might;	/* Extra might bow */
	bool impact;		/* Earthquake blows */

	s16b dis_to_h;		/* Known bonus to hit */
	s16b dis_to_d;		/* Known bonus to dam */
	s16b dis_to_a;		/* Known bonus to ac */

	s16b dis_ac;		/* Known base ac */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to dam */

	s16b ac;			/* Armour class (base + bonus) */

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

	s16b num_blow;		/* Number of blows */
	s16b num_fire;		/* Number of shots */

	byte tval_xtra;		/* Correct xtra tval */

	byte tval_ammo;		/* Correct ammo tval */

	s16b pspeed;		/* Current speed */
};


/* For Martial arts */

typedef struct martial_arts martial_arts;

struct martial_arts
{
    cptr    desc;    /* A verbose attack description */
    int     min_level;  /* Minimum skill to use */
    int     chance;     /* Chance of 'success' */
    int     dd;        /* Damage dice */
    int     ds;        /* Damage sides */
    int     effect;     /* Special effects */
};



/* Mindcrafters */

typedef struct mindcraft_power mindcraft_power;
struct mindcraft_power {
      int min_lev;
      int mana_cost;
      int fail;
      cptr name;
};

/* Spirit for shamanic magic */
typedef struct spirit_type spirit_type;
struct spirit_type {
	char name[20]; /* The name of the spirit */
	cptr desc; /* The description of the spirit */
	u16b pact; /* Whether the player has a pact with this spirit */
	u32b annoyance; /* How annoyed the spirit is with the player */
	u32b favour_flags; /* Like the 'spell_flags' array */
	byte sphere; /* sphere of influence */
	byte minskill; /* Minimum skill to form a pact (= min skill of easiest favour) */
};

/* Stat defaults */
typedef struct stat_default_type stat_default_type;
struct stat_default_type {
	byte	sex;	/* Sex */
	byte	race;	/* Race */
	byte	template;	/* Template */
	bool	maximise;	/* Whether maximise mode is used in this stat set */
	byte	stat[A_MAX];	/* The stats used */
	s16b	name;	/* The quark containing the name */
};

/* Towns */

typedef struct town_type town_type;
struct town_type
{
	s16b name; /* Town name. */

	byte x,y; /* Coords on map */
		
	u32b seed; /* Seed for RNG */

	u32b house_price; /* Price of the house in town (if any) */

	byte store[MAX_STORES_PER_TOWN];
	byte numstores;
};

/* Dungeons */

typedef struct dun_type dun_type;
struct dun_type
{
	byte x,y; /* The location of the dungeon on the max. */

	byte flags; /* Is this dungeon a tower */
	char sym; /* A letter used to represent the dungeon on the map. */

	byte  offset; /* Offset to level to apply to Monster/Object creation */
	byte  max_level; /* Maximum dungeon level allowed */
	u16b  bias; /* summon type used for biasing random creature generation */

	s16b name; /* Full name for entering/leaving (offset) */
	s16b shortname; /* 11 character name for depth display (offset) */
};

typedef struct wild_type wild_type;
struct wild_type {
		byte terrain; /* Terrain type for grid */
		byte dungeon; /* Dungeon contained in this wilderness grid */
		u32b seed; /* Seed for random number generator */
		byte road_map; /* Flags for road generation */
		s16b dun_min; /* Minimum depth to have caves */
		s16b dun_max; /* Maximum depth to have caves */
};

/*
 * The various types of event used by death_event_type events below.
 */

/* Items of all kinds. */
typedef struct make_item_type make_item_type;
struct make_item_type {
	u16b	name;	/* Name (offset) */
	byte	flags;	/* EI_* flags */
	s16b	k_idx;	/* k_idx of item */
	byte	x_idx;	/* Artefact or ego number where appropriate. */
	byte	min;	/* Minimum number which can be created. */
	byte	max;	/* Maximum number which can be created. */
};

/* Monsters */
typedef struct make_monster_type make_monster_type;
struct make_monster_type {
	s16b	num;	/* r_idx of monster. */
	byte	min;	/* Minimum number which can be created. */
	byte	max;	/* Maximum number which can be created. */
	byte	radius;	/* Distance between the dead monster and the resulting ones */
	bool	strict;	/* Only place monsters within radius, rather than merely preferring to. */
};

/* Explosions centred on the deceased monster. */
typedef struct make_explosion_type make_explosion_type;
struct make_explosion_type {
	byte	dice;	/* Number of die rolls to be summed for damage. */
	byte	sides;	/* Number of sides the above "dice" should have. */
	byte	method;	/* Type of explosion to be attempted. */
	s16b	radius;	/* Radius of explosion*/
};

/* Coins (amounts handled by the normal flags as you can't get 1d3 from
 * Bernoulli trials) */
typedef struct make_coin_type make_coin_type;
struct make_coin_type {
	s16b	metal; /* Coin_type. */
};

/* Keep the parameters within death_event_type to simplify things. */
typedef union de_par de_par;
union de_par {
	make_item_type item;
	make_monster_type monster;
	make_explosion_type explosion;
	make_coin_type coin;
};

/* Control interraction between events and control the structure. */
typedef struct death_event_type death_event_type;
struct death_event_type {
	u16b	text;	/* Text (offset) */
	s16b	r_idx;	/* The number of the monster responsible for the event. */
	u16b	num;	/* The numerator in the probability of causing the event */
	u16b	denom;	/* The denominator in the probability of causing the event */
	byte	flags;	/* EF_* flags */
	byte	type;	/* event type */
	de_par	par;	/* The parameters for this event type (see above) */
};

typedef struct tval_ammo_type tval_ammo_type;
struct tval_ammo_type {
	s16b	bow_kidx;	/* k_idx of the bow being used. */
	byte	ammo_tval;	/* tval of the ammunition it uses. */
};



/* Hack - provide a structure for things about an object which may be unknown,
 * but are needed by various functions. */
typedef struct object_extra object_extra;

#if 0 /* Only currently used in object1.c, so defined there for now. */
struct object_extra {
};
#endif

/*
 * Semi-Portable High Score List Entry (128 bytes) -- BEN
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
	char p_c[3];		/* Player Template (number) */

	char cur_lev[4];		/* Current Player Level (number) */
	char cur_dun[4];		/* Current Dungeon Level (number) */
	char max_lev[4];		/* Max Player Level (number) */
	char max_dun[4];		/* Max Dungeon Level (number) */

	char how[32];		/* Method of death (string) */
};

/*
 * List the variable elements of a monster blow method.
 */
typedef const struct blow_method_type blow_method_type;

struct blow_method_type
{
	byte flags; /* RBF_* flags. */
	cptr name; /* Descriptive name. */
	cptr hitmsg; /* Message on hitting the target. */
	cptr *hitplayer; /* Null-terminated list of random messages for hitting the
		* player. Overrides hitmsg. */
	cptr missmsg; /* Message on missing the target, if any. */
	cptr flagname; /* The name used to describe this flag in a text file.
		* Unused withose ALLOW_TEMPLATES. */
};

