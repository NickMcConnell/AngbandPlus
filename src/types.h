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
	char version[MAX_VERSION_LEN]; /* Version */

	u16b info_num; /* Number of "info" records */

	u16b info_len; /* Size of each "info" record */

	u32b info_size; /* Size of the "info" array in bytes */

	u32b name_size; /* Size of the "name" array in bytes */

	u32b text_size; /* Size of the "text" array in bytes */

	cptr file_name; /* Base name of the file to be parsed. */

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
	u32b name; /* Name (offset) */
	u32b text; /* Text (offset) */

	byte file; /* File restriction */
	char pref; /* Prefix restriction */
	byte field; /* (colon-delimited) field */

	byte conv; /* Type of conversion to be made. */
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

	u16b o_max; /* Max size for "o_list[]" */
	u16b m_max; /* Max size for "m_list[]" */
	u16b oname; /* Maximum length of object_desc() strings. */
	u16b mname; /* Usual maximum length of monster_desc() strings. */
	u16b ar_delay; /* Delay between rolls of the auto-roller. */

	u16b macros; /* Elements in "macro_info" */
	u16b v_max; /* Elements in "v_info[]" */
	u16b f_max; /* Elements in "f_info[]" */
	u16b k_max; /* Elements in "k_info[]" */
	u16b u_max; /* Elements in "u_info[]" */
	u16b ob_max; /* Elements in "o_base[]" */
	u16b a_max; /* Elements in "a_info[]" */
	u16b e_max; /* Elements in "e_info[]" */
	u16b r_max; /* Elements in "r_info[]" */
	u16b event_max; /* Elements in "death_events[]" */
	u16b p_max; /* Elements in "p_info[]" */
	u16b h_max; /* Elements in "h_info[]" */
	u16b b_max; /* Total size per element of "b_info[]" */
	u16b flavor_max; /* Elements in "flavor_info[]" */
	u16b quests; /* Elements in "q_list[]" */
	u16b dungeons; /* Elements in "dun_defs[]" */
	u16b towns; /* Elements in "town_defs[]" */
	u16b owners; /* Elements in "owners[]" */
	u16b templates; /* Elements in template_info[] */
};

/*
 * Graphical representation for various on-screen things.
 */
typedef struct gfx_type gfx_type;
struct gfx_type
{
	byte da;
	char dc;

	byte xa;
	char xc;
};

/*
 * Information about terrain "features"
 */

typedef struct feature_type feature_type;

struct feature_type
{
	u16b name; /* Name (offset) */
	u16b text; /* Text (offset) */

	byte mimic; /* Feature to mimic */
	byte priority; /* Priority for small-scale map. See priority(). */

	gfx_type gfx; /* On-screen representation. */
};


/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */

typedef struct object_kind object_kind;

struct object_kind
{
	u32b text; /* Text (offset) */

	u16b name; /* Name (offset) */
	byte tval; /* Object type */
	byte extra; /* Extra information related to the tval. */

	s16b pval; /* Object extra info */
	s16b to_h; /* Bonus to hit */

	s16b to_d; /* Bonus to damage */
	s16b to_a; /* Bonus to armor */

	s16b ac; /* Base armor */
	byte dd, ds; /* Damage dice/sides */

	u32b flags1; /* Flags, set 1 */
	u32b flags2; /* Flags, set 2 */
	u32b flags3; /* Flags, set 3 */

	byte locale[4]; /* Allocation level(s) */
	byte chance[4]; /* Allocation chance(s) */

	s32b cost; /* Object "base cost" */
	s16b weight; /* Weight */
	u16b u_idx; /* The u_info[] entry which represents this item. */

	gfx_type gfx; /* On-screen representation. */

	byte i_attr; /* Desired equipment list colour */

	bool aware; /* The player is "aware" of the item's effects */
	bool tried; /* The player has "tried" one of the items */
	bool seen; /* The player has encountered at least one of these. */

	byte rating; /* Bonus to level rating. */
	byte squelch; /* Default squelch setting. */
	u16b note; /* Default inscription for this object_kind. */
};



/*
 * Information about the unidentified forms of object "kinds"
 */
typedef struct unident_type unident_type;

struct unident_type
{
	u16b name; /* Name (offset) */

	byte p_id; /* Primary index */
	byte s_id; /* Secondary index (internally generated) */

	gfx_type gfx; /* On-screen representation. */
};

typedef struct o_base_type o_base_type;

struct o_base_type
{
	u16b name; /* Name (offset) */
	u16b note; /* Default inscription. */
	s32b cost; /* Unaware cost */
	u32b flags1; /* Expected flags, set 1 */
	u32b flags2; /* Expected flags, set 2 */
	u32b flags3; /* Expected flags, set 3 */
	byte tval; /* The tval for this base type, TV_UNKNOWN if none. */
	byte i_attr; /* The colour of such an item in the player's inventory. */
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
	u16b name; /* Name (offset) */
	u16b text; /* Text (offset) */

	s16b k_idx; /* Artifact type */

	s16b pval; /* Artifact extra info */

	s16b to_h; /* Bonus to hit */
	s16b to_d; /* Bonus to damage */
	s16b to_a; /* Bonus to armor */

	s16b ac; /* Base armor */

	byte dd, ds; /* Damage when hits */

	s16b weight; /* Weight */

	s32b cost; /* Artifact "cost" */

	u32b flags1; /* Artifact Flags, set 1 */
	u32b flags2; /* Artifact Flags, set 2 */
	u32b flags3; /* Artifact Flags, set 3 */

	byte level; /* Artifact level */
	byte level2; /* Secondary level for "special" artifacts */
	byte rarity; /* Artifact rarity */

	byte cur_num; /* Number created (0 or 1) */
};


/*
 * Information about "ego-items".
 */

typedef struct ego_item_type ego_item_type;

struct ego_item_type
{
	u16b name; /* Name (offset) */
	u16b text; /* Text (offset) */

	byte rating; /* Rating boost */
	byte special; /* Index into random item modifications on creation. */

	byte chance; /* Chance of being created. */

	s16b min_obj; /* Minimum legal object. */
	s16b max_obj; /* Maximum legal object. */

	byte max_to_h; /* Maximum to-hit bonus */
	byte max_to_d; /* Maximum to-dam bonus */
	byte max_to_a; /* Maximum to-ac bonus */

	byte max_pval; /* Maximum pval */

	s32b cost; /* Ego-item "cost" */

	u32b flags1; /* Ego-Item Flags, set 1 */
	u32b flags2; /* Ego-Item Flags, set 2 */
	u32b flags3; /* Ego-Item Flags, set 3 */
};




/*
 * Monster blow structure
 *
 * - Method (RBM_*)
 * - Effect (RBE_*)
 * - Damage Dice
 * - Damage Sides
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
	u32b name; /* Name (offset) */
	u32b text; /* Text (offset) */

	byte hdice; /* Creatures hit dice count */
	byte hside; /* Creatures hit dice sides */

	s16b ac; /* Armour Class */

	s16b sleep; /* Inactive counter (base) */
	byte aaf; /* Area affect radius (1-100) */
	byte speed; /* Speed (normally 110) */

	s32b mexp; /* Exp value for kill */

	u32b flags1; /* Flags 1 (general) */
	u32b flags2; /* Flags 2 (abilities) */
	u32b flags3; /* Flags 3 (race/resist) */
	u32b flags4; /* Flags 4 (inate/breath) */
	u32b flags5; /* Flags 5 (normal spells) */
	u32b flags6; /* Flags 6 (special spells) */

	monster_blow blow[4]; /* Up to four blows per round */

	byte freq_spell; /* Spell frequency */
	byte level; /* Level of creature */
	byte rarity; /* Rarity of creature */

	gfx_type gfx; /* On-screen representation. */

	byte max_num; /* Maximum population allowed per level */
	byte cur_num; /* Monster population on current level */


	s16b r_sights; /* Count sightings of this monster */
	s16b r_deaths; /* Count deaths from this monster */

	s16b r_pkills; /* Count monsters killed in this life */
	s16b r_tkills; /* Count monsters killed in all lives */

	byte r_wake; /* Number of times woken up (?) */
	byte r_ignore; /* Number of times ignored (?) */

	byte r_drop_gold; /* Max number of gold dropped at once */
	byte r_drop_item; /* Max number of item dropped at once */

	byte r_cast_spell; /* Max number of spells seen */

	byte num_blows;        /* Attack speed (equates to p_ptr->num_blows) */
	byte r_blows[4]; /* Number of times each blow type was seen */

	u32b r_flags1; /* Observed racial flags */
	u32b r_flags2; /* Observed racial flags */
	u32b r_flags3; /* Observed racial flags */
	u32b r_flags4; /* Observed racial flags */
	u32b r_flags5; /* Observed racial flags */
	u32b r_flags6; /* Observed racial flags */
};



/*
 * Information about "vault generation"
 */

typedef struct vault_type vault_type;

struct vault_type
{
	u32b name; /* Name (offset) */
	u32b text; /* Text (offset) */

	byte typ; /* Vault type */

	byte rat; /* Vault rating */

	byte hgt; /* Vault height */
	byte wid; /* Vault width */
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
	u16b info; /* Hack -- cave flags */

	byte feat; /* Hack -- feature type */
	byte r_feat; /* Observed feature type */

	s16b o_idx; /* Object in this grid */

	s16b m_idx; /* Monster in this grid */

#ifdef MONSTER_FLOW

	byte cost; /* Hack -- cost of flowing */
	byte when; /* Hack -- when cost was computed */

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

/* Where was this object found? */
typedef struct object_found object_found;
struct object_found
{
	byte how; /* Found under this set of circumstances. */
	s16b idx; /* Shop, vault, etc., index for this situation. */
	byte dungeon; /* Found in/near this dungeon (255 for wilderness). */
	byte level; /* Found on this level within the dungeon. */
};

typedef struct object_type object_type;
typedef const object_type object_ctype;

struct object_type
{
	s16b k_idx; /* Kind index (zero if "dead") */
	byte iy; /* Y-position on map, or zero */
	byte ix; /* X-position on map, or zero */

	byte tval; /* Item type (from kind) */
	byte discount; /* Discount (if any) */
	byte number; /* Number of items */
	bool marked; /* Object is marked */

	byte name1; /* Artifact type, if any */
	byte name2; /* Ego-Item type, if any */
	byte activation; /* Object activation (e.g. as randart) */
	byte stack;         /* The stack of objects this was part of. */

	s16b to_h; /* Plusses to hit */
	s16b to_d; /* Plusses to damage */

	s16b to_a; /* Plusses to AC */
	s16b ac; /* Normal AC */

	s16b pval; /* Item extra-parameter */
	s16b timeout; /* Timeout Counter */

	s16b weight; /* Item weight */
	byte dd, ds; /* Damage dice/sides */

	u16b ident; /* Special flags  */
	u16b note; /* Inscription index */

	u16b art_name;      /* Artifact name (random artifacts) */
	/* u16b nothing; */ /* Unused */

	u32b flags1;        /* Flags, set 1 */
	u32b flags2;        /* Flags, set 2 */
	u32b flags3;        /* Flags, set 3 */


	s16b next_o_idx; /* Next object in stack (if any) */
	s16b held_m_idx; /* Monster holding us (if any) */

	object_found found; /* Describe where the object was found. */
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
	s16b r_idx; /* Monster race index */

	byte fy; /* Y location on map */
	byte fx; /* X location on map */

	byte generation; /* Generation if a breeder */

	s16b hp; /* Current Hit points */
	s16b maxhp; /* Max Hit points */

	s16b csleep; /* Inactive counter */

	byte mspeed; /* Monster "speed" */
	s16b energy; /* Monster "energy" */

	byte stunned; /* Monster is stunned */
	byte confused; /* Monster is confused */
	byte monfear; /* Monster is afraid */

	byte cdis; /* Current dis from player */

	byte mflag; /* Extra monster flags */

	s16b gold;        /* Gold stolen from player */

	bool ml; /* Monster is "visible" */

	s16b hold_o_idx; /* Object being held (if any) */

#ifdef WDT_TRACK_OPTIONS

	byte ty; /* Y location of target */
	byte tx; /* X location of target */

	byte t_dur; /* How long are we tracking */

	byte t_bit; /* Up to eight bit flags */

#endif

#ifdef DRS_SMART_OPTIONS

	u32b smart; /* Field for "smart_learn" */

#endif

	s16b pl_mdam; /* Maximum damage the player has caused in a turn. */
	s16b pl_cdam; /* Damage the player caused in this turn. */
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
	s16b index; /* The actual index */

	byte level; /* Base dungeon level */
	byte prob1; /* Probability, pass 1 */
	byte prob2; /* Probability, pass 2 */
	byte prob3; /* Probability, pass 3 */
};



/*
 * Available "options"
 *
 * - Address of actual option variable (or NULL)
 *
 * - Normal Value (TRUE or FALSE)
 *
 * - Option Page Number (or zero)
 *
 * - Savefile Set (or zero)
 * - Savefile Bit in that set
 *
 * - Textual name (or NULL)
 * - Textual description
 */

typedef struct option_type option_type;

struct option_type
{
	bool *o_var;

	byte o_norm;

	byte o_page;

	byte o_set;
	byte o_bit;

	cptr o_text;
	cptr o_desc;
};


/*
 * A more complex option structure.
 * This includes an array and a function to print out the current value.
 */
typedef struct option_special option_special;
struct option_special
{
	void (*print_f1)(char *, uint, cptr, va_list *); /* value->string */
	bool (*parse)(void *, cptr); /* string->value */
	const s16b *vals; /* A list of the values this option can take. */
	u16b nvals; /* N_ELEMENTS(vals) */
	void *var; /* A pointer to the variable being set. Normally a s16b. */
	cptr text; /* The name by which the option may be referred. */
	cptr desc; /* A short description of the option. */
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
	cptr name;

	gfx_type gfx; /* N.B. Only the attr fields are actually read. */
};

/*
 * Structure for the "quests"
 */

typedef struct quest_type quest_type;

struct quest_type
{
	s16b r_idx; /* Monster race */
	byte level; /* Dungeon level */
	byte dungeon; /* Dungeon containing quest */

	byte cur_num; /* Number killed */
	byte cur_num_known; /* Number known by the player to have been killed */
	byte max_num; /* Number required */
	bool known; /* The player has entered this quest. */
};




/*
 * A store owner
 */

typedef struct owner_type owner_type;

struct owner_type
{
	s16b name; /* Name */

	s16b max_cost; /* Purse limit */

	byte max_inflate; /* Inflation (max) */
	byte min_inflate; /* Inflation (min) */

	byte haggle_per; /* Haggle unit */

	byte insult_max; /* Insult limit */

	byte owner_race; /* Owner race */

	byte shop_type; /* The category of shop this shopkeeper owns. */
	byte town; /* The town restriction, if any. */
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
	s16b owner;   /* Owner index */

	s16b insult_cur; /* Insult counter */

	s16b good_buy; /* Number of "good" buys */
	s16b bad_buy; /* Number of "bad" buys */

	s32b store_open; /* Closed until this turn */

	s16b stock_num; /* Stock -- Number of entries */
	s16b stock_size; /* Stock -- Total Size of Array */
	object_type *stock; /* Stock -- Actual stock items */
};





typedef struct magic_type magic_type;

struct magic_type
{
	cptr name; /* Name listed in spell book, etc. */
	cptr desc; /* Information about the spell. */

	byte min; /* Required skill (to learn) */
	byte mana; /* Required mana (to cast) */
	byte fail; /* Minimum chance of failure */
	byte flags; /* (Variable) MAGIC_* flags. */

	byte skill1; /* School of spell */
	byte skill2; /* Type of spell (or NONE) */
	s16b power; /* The index of the spell effect. */
};

typedef struct book_type book_type;
struct book_type
{
#ifdef CHECK_ARRAYS
	int idx;
#endif /* CHECK_ARRAYS */

	magic_type *info; /* A list of the spells available in this book. */

	byte max; /* The number of spells in this book. */
	byte learn; /* Index offset for learning (or 0). */
};

/*
 * Structure for the display windows.
 */
typedef struct window_type window_type;
struct window_type
{
	term *term; /* The window in question */
	char name[16]; /* Name of the window */
	byte pri[32]; /* Priority of the specified display type for this window. */
	byte rep[32]; /* Maximum priority of display which this one can replace. */
	byte current; /* Current display for this window */
	u32b mask;
};

/*
 * Player sex info
 */

typedef struct player_sex player_sex;

struct player_sex
{
	cptr title; /* Type of sex */

	cptr winner; /* Name of winner */
};


/*
 * A template to create an item with.
 */
typedef struct make_item_type make_item_type;
struct make_item_type {
	s16b k_idx; /* k_idx of item */
	byte x_idx; /* Artefact or ego number where appropriate. */
	byte flags; /* EI_* flags */
	u16b name; /* Name (offset) */
	byte min; /* Minimum number which can be created. */
	byte max; /* Maximum number which can be created. */
};

/*
 * Race-specific bonuses (resistances, etc.).
 */
typedef struct race_bonus race_bonus;
struct race_bonus
{
	byte type; /* Mutation needed for this bonus (unused for races). */
	byte skill; /* The skill needed to access it, if any. */
	byte min; /* The level of skill needed to access it. */
	byte set; /* The flag set involved in the skill. */
	byte flag; /* The flag involved (log 2). */
	s16b value; /* A description of how the bonus affects the flag. */
};

typedef struct race_power race_power;
struct race_power
{
	s16b idx; /* The index used to identify this power in do_power(). */
	s16b min_level; /* A penalty for racial_success_chance() which depends partly on level. */
	s16b cost; /* Cost of power. Negative values refer to skill-based values */
	byte use_stat;  /* The stat considered by racial_success_chance() */
	byte difficulty; /* Another penalty for racial_success_chance(). */
	cptr text; /* The description of the power given to the player. */
	cptr text2; /* An additional phrase to be inserted after the cost is given. */
	cptr atext; /* Text to print as the power is used. */
};

/*
 * Player racial info
 */

typedef struct player_race player_race;

struct player_race
{
	cptr title; /* Type of race */

	s16b r_adj[A_MAX]; /* Racial stat bonuses */

	byte disarm_bonus; /* disarming */
	byte device_bonus; /* magic devices */
	byte save_bonus; /* saving throw */
	byte stealth_bonus; /* stealth */
	byte search_bonus; /* search ability */
	byte perception_bonus; /* search frequency */
	byte melee_bonus; /* combat (normal) */
	byte missile_bonus; /* combat (shooting) */

	byte r_mhp; /* Race hit-dice modifier */
	byte r_exp; /* Race experience factor */

	byte b_age; /* base age */
	byte m_age; /* mod age */

	byte m_b_ht; /* base height (males) */
	byte m_m_ht; /* mod height (males) */
	byte m_b_wt; /* base weight (males) */
	byte m_m_wt; /* mod weight (males) */

	byte f_b_ht; /* base height (females) */
	byte f_m_ht; /* mod height (females)   */
	byte f_b_wt; /* base weight (females) */
	byte f_m_wt; /* mod weight (females) */

	u32b flags[4]; /* Various flags to give the race unconditionally. */

	race_bonus *bonus; /* A pointer to the extra things this race gets. */
	race_power *power; /* A pointer to the powers the race can use. */
	byte bonuses; /* The number of elements in bonus. */
	byte powers; /* The number of elements in power. */

	byte chaos; /* A chaos feature this race is likely to get. */
	byte chaos_chance; /* THe chance of gaining the above chaos feature. */
	byte infra; /* Infra-vision range */

	byte chart; /* Initial chart for get_history() */
	byte grace; /* Race restriction for ghosts. */
	byte eat; /* Proportion of sustenance gained from eating. */

	cptr **name_syls; /* Syllable list for random name generation. */
	make_item_type items[MAX_RACE_ITEMS]; /* Objects to be given at birth. */
};

/*
 *Player Skill Info
 */

typedef struct player_skill player_skill;
struct player_skill
{
#ifdef CHECK_ARRAYS
	int idx;
#endif /* CHECK_ARRAYS */
	cptr name; /* Skill name */
	cptr increase; /* Message printed on an increase */
	u16b experience; /* Amount of times the skill has been used */
	byte value; /* The current skill level */
	byte max_value; /* Maximum value the skill has had */
	byte base; /* The skill level that may be raised to at dungeon level zero */
	byte ceiling; /* The absolute maximum the skill may be raised to */
	u16b exp_to_raise; /* The number of times the skill must be used to raise */
	byte x; /* The x co-ordinate used in player_skills(). */
	byte y; /* The y co-ordinate used in player_skills(). */
};

/*
 * Player template info
 */

typedef struct player_template player_template;

struct player_template
{
	u16b name; /* Type of template. */

	byte choices; /* Number of choices for hermetic skills */

	byte art1_bias; /* Possible bias for artefact_scroll(). */
	byte art2_bias; /* Alternative bias for artefact_scroll(). */
	byte art2_chance; /* Chance of choosing art2 if a bias is used. */

	s16b c_adj[A_MAX]; /* Template stat modifier */

	s16b skill[MAX_SKILLS]; /* Skill improvements */

	/* Objects to be given at the start (including backups for items which
	 * are useless for the chosen race). */
	make_item_type items[MAX_TPL_ITEMS*2];
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
	byte psex; /* Sex index */
	byte prace; /* Race index */
	byte ptemplate; /* Template index */

	byte hitdie; /* Hit dice (sides) */
	u16b expfact;       /* Experience factor */

	byte ritual;   /* Flag for recall ritual */
	byte resist_eldritch; /* % resistance to sanity blasting. */

	s16b age; /* Characters age */
	s16b ht; /* Height */
	s16b wt; /* Weight */
	s16b sc; /* Social Class */
	s16b birthday;    /* Player's Birthday */
	s16b startdate;   /* The start date of the adventure */


	s32b au; /* Current Gold */

	s32b exp; /* Cur experience */
	u16b exp_frac; /* Cur exp frac (times 2^16) */


	s16b mhp; /* Max hit pts */
	s16b chp; /* Cur hit pts */
	s16b min_hp; /* Minimum hp (used at level start) */
	u16b chp_frac; /* Cur hit frac (times 2^16) */

	s16b msp; /* Max mana pts */
	s16b csp; /* Cur mana pts */
	u16b csp_frac; /* Cur mana frac (times 2^16) */

	s16b mchi; /* Max chi points */
	s16b cchi; /* Cur chi points */
	u16b chi_frac; /* Cur chi frac (times 2^16) */

	s16b max_dlv; /* Max levels explored in current dungeon. */

	s16b stat_max[A_MAX]; /* Current "maximal" stat values */
	s16b stat_cur[A_MAX]; /* Current "natural" stat values */

	s16b fast; /* Timed -- Fast */
	s16b slow; /* Timed -- Slow */
	s16b blind; /* Timed -- Blindness */
	s16b paralyzed; /* Timed -- Paralysis */
	s16b confused; /* Timed -- Confusion */
	s16b afraid; /* Timed -- Fear */
	s16b image; /* Timed -- Hallucination */
	s16b poisoned; /* Timed -- Poisoned */
	s16b cut; /* Timed -- Cut */
	s16b stun; /* Timed -- Stun */

	s16b protevil; /* Timed -- Protection */
	s16b invuln; /* Timed -- Invulnerable */
	s16b hero; /* Timed -- Heroism */
	s16b shero; /* Timed -- Super Heroism */
	s16b shield; /* Timed -- Shield Spell */
	s16b blessed; /* Timed -- Blessed */
	s16b tim_invis; /* Timed -- See Invisible */
	s16b tim_infra; /* Timed -- Infra Vision */

	s16b oppose_acid; /* Timed -- oppose acid */
	s16b oppose_elec; /* Timed -- oppose lightning */
	s16b oppose_fire; /* Timed -- oppose heat */
	s16b oppose_cold; /* Timed -- oppose cold */
	s16b oppose_pois; /* Timed -- oppose poison */


	s16b tim_esp;       /* Timed ESP */
	s16b wraith_form;   /* Timed wraithform */
	s16b vamp_drain; /* Vampiric drain during last round. */

	s16b chaos_patron;
	u32b muta[3];

	s16b word_recall; /* Word of recall counter */

	s16b energy; /* Current energy */

	s16b food; /* Current nutrition */

	byte confusing; /* Glowing hands */
	byte sneaking; /* Currently searching */

	s16b new_spells; /* Number of spells available */

	byte ma_armour; /* Armour bonus gained through martial arts. */

	bool ma_cumber_armour; /* Martial arts limiting armour */
	bool cumber_armor; /* Mana draining armor */
	bool cumber_glove; /* Mana draining gloves */
	bool cumber_helm; /* Chi draining helm */
	bool heavy_wield; /* Heavy weapon */
	bool heavy_shoot; /* Heavy shooter */

	byte wield_skill; /* Weapon skill used for current weapon*/
	s16b cur_lite; /* Radius of lite (if any) */


	u32b notice; /* Special Updates (bit flags) */
	u32b update; /* Pending Updates (bit flags) */
	u32b redraw; /* Normal Redraws (bit flags) */
	u32b window; /* Window Redraws (bit flags) */

	s16b stat_use[A_MAX]; /* Current modified stats */
	s16b stat_top[A_MAX]; /* Maximal modified stats */

	s16b stat_add[A_MAX]; /* Modifiers to stat values */
	s16b stat_ind[A_MAX]; /* Indexes into stat tables */

	bool immune_acid; /* Immunity to acid */
	bool immune_elec; /* Immunity to lightning */
	bool immune_fire; /* Immunity to fire */
	bool immune_cold; /* Immunity to cold */

	bool resist_acid; /* Resist acid */
	bool resist_elec; /* Resist lightning */
	bool resist_fire; /* Resist fire */
	bool resist_cold; /* Resist cold */
	bool resist_pois; /* Resist poison */

	bool resist_conf; /* Resist confusion */
	bool resist_sound; /* Resist sound */
	bool resist_lite; /* Resist light */
	bool resist_dark; /* Resist darkness */
	bool resist_chaos; /* Resist chaos */
	bool resist_disen; /* Resist disenchant */
	bool resist_shard; /* Resist shards */
	bool resist_nexus; /* Resist nexus */
	bool resist_blind; /* Resist blindness */
	bool resist_neth; /* Resist nether */
	bool resist_fear; /* Resist fear */

	bool heal_nether; /* Be healed by nether attacks. */
	bool immune_dark; /* Immunity to dark. */
	bool hurt_light; /* Take extra damage from light. */
	bool weak_wraith; /* Move through walls, taking no damage if low on HP. */

	bool reflect;       /* Reflect 'bolt' attacks */
	bool sh_fire;       /* Fiery 'immolation' effect */
	bool sh_elec;       /* Electric 'immolation' effect */

	bool anti_magic;    /* Anti-magic */
	bool anti_tele;     /* Prevent teleportation */

	bool sustain[A_MAX]; /* Keep various stats. */

	bool aggravate; /* Aggravate monsters */
	bool teleport; /* Random teleporting */

	bool exp_drain; /* Experience draining */

	bool ffall; /* No damage falling */
	bool lite; /* Permanent light */
	bool free_act; /* Never paralyzed */
	bool see_inv; /* Can see invisible */
	bool regenerate; /* Regenerate hit pts */
	bool hold_life; /* Resist life draining */
	bool telepathy; /* Telepathy */
	bool slow_digest; /* Slower digestion */
	bool xtra_might; /* Extra might bow */
	bool impact; /* Earthquake blows */
	bool no_cut; /* Immune to cuts. */
	bool no_stun; /* Immune to stunning. */

	s16b dis_to_h; /* Known bonus to hit */
	s16b dis_to_d; /* Known bonus to dam */
	s16b dis_to_a; /* Known bonus to ac */

	s16b dis_ac; /* Known base ac */

	s16b to_h; /* Bonus to hit */
	s16b to_d; /* Bonus to dam */

	s16b ac; /* Armour class (base + bonus) */

	s16b see_infra; /* Infravision range */

	s16b skill_dis; /* Skill: Disarming */
	s16b skill_dev; /* Skill: Magic Devices */
	s16b skill_sav; /* Skill: Saving throw */
	s16b skill_stl; /* Skill: Stealth factor */
	s16b skill_srh; /* Skill: Searching ability */
	s16b skill_fos; /* Skill: Searching frequency */
	s16b skill_thn; /* Skill: To hit (normal) */
	s16b skill_thb; /* Skill: To hit (shooting) */
	s16b skill_tht; /* Skill: To hit (throwing) */
	s16b skill_dig; /* Skill: Digging */

	s16b num_blow; /* Number of blows */
	s16b num_fire; /* Number of shots */

	byte tval_ammo; /* Correct ammo tval */

	s16b pspeed; /* Current speed */
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
	u32b annoyance; /* How annoyed the spirit is with the player */

	bool pact; /* Whether the player has a pact with this spirit */
	byte sphere; /* sphere of influence */
	byte minskill; /* Minimum skill to form a pact */
	byte punish_chance; /* How likely the spirit is to punish the player. */

	s16b cost; /* The base cost of associating with this spirit. */
	byte stat; /* The stats drained by associating with this spirit. */
	byte book; /* The index of the book_type associated with this spirit. */
};

/* Stat defaults */
typedef struct stat_default_type stat_default_type;
struct stat_default_type {
	byte sex; /* Sex */
	byte race; /* Race */
	byte template; /* Template */
	bool maximise; /* Whether maximise mode is used in this stat set */
	byte stat[A_MAX]; /* The stats used */
	u16b name; /* The quark containing the name */
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

/* Monsters */
typedef struct make_monster_type make_monster_type;
struct make_monster_type {
	s16b num; /* r_idx of monster. */
	byte min; /* Minimum number which can be created. */
	byte max; /* Maximum number which can be created. */
	byte radius; /* Distance between the dead monster and the resulting ones */
	bool strict; /* Never place monsters outside radius. */
};

/* Explosions centred on the deceased monster. */
typedef struct make_explosion_type make_explosion_type;
struct make_explosion_type {
	byte dice; /* Number of die rolls to be summed for damage. */
	byte sides; /* Number of sides the above "dice" should have. */
	byte method; /* Type of explosion to be attempted. */
	s16b radius; /* Radius of explosion*/
};

/* Coins (amounts handled by the normal flags as you can't get 1d3 from
 * Bernoulli trials) */
typedef struct make_coin_type make_coin_type;
struct make_coin_type {
	s16b metal; /* Coin_type. */
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
	u16b text; /* Text (offset) */
	s16b r_idx; /* The number of the monster responsible for the event. */
	u16b num; /* The numerator in the probability of causing the event */
	u16b denom; /* The denominator in the probability of causing the event */
	byte flags; /* EF_* flags */
	byte type; /* event type */
	de_par par; /* The parameters for this event type (see above) */
};

typedef struct tval_ammo_type tval_ammo_type;
struct tval_ammo_type {
	s16b bow_kidx; /* k_idx of the bow being used. */
	byte ammo_tval; /* tval of the ammunition it uses. */
};



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
	char what[MAX_VERSION_LEN]; /* Version info (string) */

	char pts[10]; /* Total Score (number) */

	char gold[10]; /* Total Gold (number) */

	char turns[10]; /* Turns Taken (number) */

	char day[10]; /* Time stamp (string) */

	char who[16]; /* Player Name (string) */

	char uid[8]; /* Player UID (number) */

	char sex[2]; /* Player Sex (string) */
	char p_r[3]; /* Player Race (number) */
	char p_c[3]; /* Player Template (number) */

	char cur_lev[4]; /* Current Player Level (number) */
	char cur_dun[4]; /* Current Dungeon Level (number) */
	char max_lev[4]; /* Max Player Level (number) */
	char max_dun[4]; /* Max Dungeon Level (number) */

	char how[32]; /* Method of death (string) */
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

/*
 * Describe a display function used for an external window.
 */
typedef struct display_func_type display_func_type;

struct display_func_type
{
	u32b flag; /* PW_* flag for this display. */
	cptr name; /* Name (used in preferences) for this display. */
	bool (*good)(void); /* Check if this display is interesting. */
	void (*display)(void); /* Display this display. */
};

/* Describe a set of co-ordinates. */
typedef struct co_ord co_ord;
struct co_ord
{
	int x;
	int y;
};

/*
 * Describe the information needed to explain how to redraw a string display.
 * Anything not marked as fixed can be changed in the redraw prefs.
 */
typedef struct redraw_type redraw_type;
struct redraw_type
{
#ifdef CHECK_ARRAYS
	int idx; /* Index (fixed). */
#endif /* CHECK_ARRAYS */
	cptr name; /* Description (fixed). */
	s16b x, y; /* Actual co-ordinates. */
	s16b l; /* Maximum length of the string (0 disables it). */
};


/* A set of strings in a particular order. */
typedef struct cptr_ch cptr_ch;
struct cptr_ch
{
#ifdef CHECK_ARRAYS
	int idx;
#endif /* CHECK_ARRAYS */
	cptr str;
};

#ifdef HANDLE_SIGNALS

/* Define a signal handler here where everything can see it. */
typedef void (*Signal_Handler_t)(int);

#endif

/*
 * Associate numbers with names for one reason or another.
 */
typedef const struct name_entry name_centry;
typedef struct name_entry name_entry;
struct name_entry
{
	int idx;
	cptr str;
};

typedef struct natural_attack natural_attack;
struct natural_attack
{
	int mut; /* The chaos feature required for this attack. */
	byte dd; /* The dice for the attack. */
	byte ds; /* The sides of each die in the attack. */
	byte wgt; /* The weight used in the attack (for criticals). */
	byte typ; /* The type of attack for project(), or GF_HIT for physical. */
	cptr desc; /* The name of the body part used. */
};

/* Some basic information about a GF_* flag. */
typedef struct gf_type gf_type;
struct gf_type
{
	byte idx;

	/* Graphics stuff */
	byte bolt_graf_attr;
	byte ball_graf_char;
	byte base_bolt_char;

	/* Colours used. */
	char colour[16];

	cptr flag; /* The name of the flag, used in text files. */
	cptr desc; /* A textual description of this effect. */
};
