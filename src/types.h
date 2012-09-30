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
 * this includes the tval/sval/weight fields in "object_type", various fields
 * in "header_type", and the "m_idx" and "o_idx" fields in "cave_type".  All
 * of these could be removed, but this would, in general, slow down the game
 * and increase the complexity of the code.
 */





/*
 * Information about maximal indices of certain arrays
 * Actually, these are not the maxima, but the maxima plus one
 */

typedef struct maxima maxima;

struct maxima
{
	u32b fake_text_size;
	u32b fake_name_size;

	u16b f_max;	/* Max size for "f_info[]" */
	u16b k_max;	/* Max size for "k_info[]" */
	u16b a_max;	/* Max size for "a_info[]" */
	u16b e_max;	/* Max size for "e_info[]" */
	u16b r_max;	/* Max size for "r_info[]" */
	u16b v_max;	/* Max size for "v_info[]" */

	u16b q_max;	/* Max size for quest array */

	u16b t_max;	/* Max size for field types array */
	u16b fld_max;	/* Max size for field list */
	u16b rg_max;	/* Max size for region list */

	u16b wn_max;	/* Max size for wilderness tree nodes */
	u16b wt_max;	/* Max size for wilderness gen types */
	u16b wp_max;	/* Max places in the wilderness */

	u16b o_max;	/* Max size for "o_list[]" */
	u16b m_max;	/* Max size for "m_list[]" */
};


/*
 * Structure used to store information required for LOS
 * calculatations.  The same data can be inverted to
 * get the squares affected by a projection.  Those squares
 * can be iterated over to get a 'flight path' for arrows
 * and thrown items...
 */

typedef struct project_type project_type;

struct project_type
{
	/* Offset of square */
	byte x;
	byte y;

	/* Index into array if this square is a wall */
	byte slope;
	byte square;
};


/*
 * Information about terrain "features"
 */

typedef struct feature_type feature_type;

struct feature_type
{
	u32b name;	/* Name (offset) */
	u32b text;	/* Text (offset) */

	byte d_attr;	/* Default feature attribute */
	char d_char;	/* Default feature character */


	byte x_attr;	/* Desired feature attribute */
	char x_char;	/* Desired feature character */

	byte w_attr;	/* Desired extra feature attribute */
	char w_char;	/* Desired extra feature character */

	byte flags;	/* Properties of the feature */
};


/*
 * Hack - a type for 'object flags'
 */
typedef struct object_flags object_flags;

struct object_flags
{
	u32b flags[4];
};


/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */

typedef struct object_kind object_kind;

struct object_kind
{
	u32b name;	/* Name (offset) */
	u32b text;	/* Text (offset) */

	byte tval;	/* Object type */
	byte sval;	/* Object sub type */

	s16b pval;	/* Object extra info */

	s16b to_h;	/* Bonus to hit */
	s16b to_d;	/* Bonus to damage */
	s16b to_a;	/* Bonus to armor */

	s16b ac;	/* Base armor */

	byte dd, ds;	/* Damage dice/sides */

	s16b weight;	/* Weight */

	s32b cost;	/* Object "base cost" */

	u32b flags[4];	/* Flags */

	byte locale[4];	/* Allocation level(s) */
	byte chance[4];	/* Allocation chance(s) */

	byte level;	/* Level */

	byte extra;	/* Rarity (for special randarts) */

	u32b trigger[MAX_TRIGGER]; /* Special object scripts */

	byte d_attr;	/* Default object attribute */
	char d_char;	/* Default object character */


	byte x_attr;	/* Desired object attribute */
	char x_char;	/* Desired object character */


	byte flavor;	/* Special object flavor (or zero) */

	bool easy_know;	/* This object is always known (if aware) */


	bool aware;	/* The player is "aware" of the item's effects */

	bool tried;	/* The player has "tried" one of the items */
};


/*
 * "Themed" objects.
 * Probability in percent for each class of objects to be dropped.
 * This could perhaps be an array - but that wouldn't be as clear.
 */
typedef struct obj_theme obj_theme;

struct obj_theme
{
	byte treasure;
	byte combat;
	byte magic;
	byte tools;
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
	u32b name;	/* Name (offset) */
	u32b text;	/* Text (offset) */

	byte tval;	/* Artifact type */
	byte sval;	/* Artifact sub type */

	s16b pval;	/* Artifact extra info */

	s16b to_h;	/* Bonus to hit */
	s16b to_d;	/* Bonus to damage */
	s16b to_a;	/* Bonus to armor */

	s16b ac;	/* Base armor */

	byte dd, ds;	/* Damage when hits */

	s16b weight;	/* Weight */

	s32b cost;	/* Artifact "cost" */

	u32b flags[4];	/* Artifact Flags */

	byte level;	/* Artifact level */
	byte rarity;	/* Artifact rarity */

	u32b trigger[MAX_TRIGGER]; /* Special object scripts */

	byte cur_num;	/* Number created (0 or 1) */
	byte max_num;	/* Unused (should be "1") */
};


/*
 * Information about "ego-items".
 */

typedef struct ego_item_type ego_item_type;

struct ego_item_type
{
	u32b name;	/* Name (offset) */
	u32b text;	/* Text (offset) */

	byte slot;	/* Standard slot value */
	byte rating;	/* Rating boost */

	byte level;	/* Minimum level */
	byte rarity;	/* Object rarity */

	s16b max_to_h;	/* Maximum to-hit bonus */
	s16b max_to_d;	/* Maximum to-dam bonus */
	s16b max_to_a;	/* Maximum to-ac bonus */

	s16b max_pval;	/* Maximum pval */

	s32b cost;	/* Ego-item "cost" */

	u32b flags[4];	/* Ego-Item Flags */

	u32b trigger[MAX_TRIGGER]; /* Special object scripts */
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
	u32b name;	/* Name (offset) */
	u32b text;	/* Text (offset) */

	s16b hdice;	/* Creatures hit dice count */
	s16b hside;	/* Creatures hit dice sides */

	s16b ac;	/* Armour Class */

	s16b sleep;	/* Inactive counter (base) */
	byte aaf;	/* Area affect radius (1-100) */
	byte speed;	/* Speed (normally 110) */

	s32b mexp;	/* Exp value for kill */

	s16b extra;	/* Unused (for now) */

	byte freq_inate;	/* Inate spell frequency */
	byte freq_spell;	/* Other spell frequency */

	u32b flags[9];	/* Flags 1 (general) */
			/* Flags 2 (abilities) */
			/* Flags 3 (race/resist) */
			/* Flags 4 (inate/breath) */
			/* Flags 5 (normal spells) */
			/* Flags 6 (special spells) */
			/* Flags 7 (movement related abilities) */
			/* Flags 8 (wilderness info) */
			/* Flags 9 (drops info) */

	monster_blow blow[4];	/* Up to four blows per round */


	byte level;	/* Level of creature */
	byte rarity;	/* Rarity of creature */


	byte d_attr;	/* Default monster attribute */
	char d_char;	/* Default monster character */


	byte x_attr;	/* Desired monster attribute */
	char x_char;	/* Desired monster character */


	byte max_num;	/* Maximum population allowed per level */

	byte cur_num;	/* Monster population on current level */


	s16b r_sights;	/* Count sightings of this monster */
	s16b r_deaths;	/* Count deaths from this monster */

	s16b r_pkills;	/* Count monsters killed in this life */
	s16b r_tkills;	/* Count monsters killed in all lives */

	byte r_wake;	/* Number of times woken up (?) */
	byte r_ignore;	/* Number of times ignored (?) */

	byte r_xtra1;	/* Something (unused) */
	byte r_xtra2;	/* Something (unused) */

	byte r_drop_gold;	/* Max number of gold dropped at once */
	byte r_drop_item;	/* Max number of item dropped at once */

	byte r_cast_inate;	/* Max number of inate spells seen */
	byte r_cast_spell;	/* Max number of other spells seen */

	byte r_blows[4];	/* Number of times each blow type was seen */

	u32b r_flags[9];	/* Observed racial flags */

	obj_theme obj_drop;	/* Type of objects to drop when killed */

	u16b r_see;	/* Number of monsters of this type visible */
};



/*
 * Information about "vault generation"
 */

typedef struct vault_type vault_type;

struct vault_type
{
	u32b name;	/* Name (offset) */
	u32b text;	/* Text (offset) */

	byte typ;	/* Vault type */

	byte rat;	/* Vault rating */

	byte hgt;	/* Vault height */
	byte wid;	/* Vault width */
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
	byte info;	/* Hack -- cave flags */

	byte feat;	/* Hack -- feature type */

	s16b o_idx;	/* Object in this grid */

	s16b m_idx;	/* Monster in this grid */

	s16b fld_idx;	/* Field in this grid */

	byte cost;	/* Hack -- cost of flowing */
	byte when;	/* Hack -- when cost was computed */
};


/*
 * Cave data that is player-specific
 *
 * The feat is the memorized feat on this square
 */

typedef struct pcave_type pcave_type;

struct pcave_type
{
	byte player;	/* Player-specific flags */
	byte feat;	/* Memorized feature */
};


/*
 * Simple structure to hold a map location
 */
typedef struct coord coord;

struct coord
{
	u16b y;
	u16b x;
};


/*
 * Pointer to a 16x16 block of cave grids.
 * The grids are allocated and deallocated in large
 * blocks for speed.
 *
 * Note - blocks also do not use the region code
 * because of speed.  (We don't need to constantly
 * allocate and deallocate everything.)
 */
typedef cave_type **blk_ptr;

/*
 * Alias of block pointer - region type
 *
 * Whilst these are the same type as blk_ptr, they
 * are used in a different way.  (Allocating large
 * chucks at a time.)
 */
typedef cave_type **region_type;

/*
 * Pointer to a 16x16 block of grids of player information.
 * The grids are allocated and deallocated in large
 * blocks for speed.
 */
typedef pcave_type **pblk_ptr;


/*
 * Region equivalent of the above.
 */
typedef pcave_type **pregion_type;


/*
 * Region information
 *
 * Note - most of this stuff is dungeon specific.
 *
 * Note - the reason why this isn't part of region_type
 * is because we don't want to have to add offsets all
 * the time for every cave access.  That would be slow.
 */
typedef struct region_info region_info;

struct region_info
{
	u16b xsize;
	u16b ysize;

	u16b refcount;

	byte flags;
};

/*
 * Structure used to generate the wilderness.
 * This stores the "height", "population" and "law" results
 * after the initial plasma fractal routines.
 * These values are then converted into simple look up numbers
 * for the wilderness generation type and wandering monster type.
 */
typedef struct wild_gen1_type wild_gen1_type;

struct wild_gen1_type
{
	u16b hgt_map;
	u16b pop_map;
	u16b law_map;
};

/*
 * Structure used during creation of wilderness.
 * It contains the transition state between the above
 * structure and the one that follows.
 */
typedef struct wild_gen2_type wild_gen2_type;

struct wild_gen2_type
{
	byte hgt_map;
	byte pop_map;
	byte law_map;
	byte place;
	byte dummy;
	byte info;
};


/* Structure used to hold the completed wilderness */

typedef struct wild_done_type wild_done_type;

struct wild_done_type
{
	u16b wild;
	byte place;
	byte info;
	byte mon_gen;
	byte mon_prob;
};

/*
 * To save room, the above three structures are combined to form the completed
 * wilderness type.
 */

typedef union wild_type wild_type;
union wild_type
{
	wild_gen1_type gen;
	wild_gen2_type trans;
	wild_done_type done;
};


/*
 * An array of this structure is used to work out what wilderness type
 * is at each 16x16 block.
 */
typedef struct wild_choice_tree_type wild_choice_tree_type;

struct wild_choice_tree_type
{
	/*
	 * Stores what type of node this is -
	 * both what type of cutoff (hgt,pop,law)
	 * and whether the pointers reference
	 * another tree node- or a wilderness
	 * generation type.
	 */
	byte info;

	/* cutoff for the split of the virtual BSP tree */
	byte cutoff;

	/*
	 * chance1/(chance1+chance2) = prob. of going down
	 * the "left" branch.  (This is used when several
	 * wilderness generation functions inhabit the same
	 * area of parameter space.  This is used to select
	 * between the possibilities randomly.
	 */
	byte chance1;
	byte chance2;

	/*
	 * These point to the left and right branches of the tree.
	 * Note - that since these also need to reference a wild.gen.type.
	 * these are index numbers of the "choice" or "gen" arrays.
	 * (depending on the value of info)
	 */
	u16b ptrnode1;
	u16b ptrnode2;
};


/*
 * This type is used to describe a region in parameter space
 * for wilderness generation.
 */
typedef struct wild_bound_box_type wild_bound_box_type;

struct wild_bound_box_type
{
	/* Min and max values for the cuboid in the parameter space */

	byte hgtmin;
	byte hgtmax;

	byte popmin;
	byte popmax;

	byte lawmin;
	byte lawmax;
};
/*
 * This data type stores the information on a particular
 * wilderness generation type for 16x16 blocks, so the
 * blocks can be
 * 1) Made.
 * 2) Looked at on the overhead map.
 */
typedef struct wild_gen_data_type wild_gen_data_type;

struct wild_gen_data_type
{
	byte feat;	/* The feature to look like on the overhead map */

	byte gen_routine;	/* Generation routine number */

	/*
	 * Course type - used in testing to see where monsters go
	 * in the wilderness.
	 */
	byte rough_type;

	byte chance;	/* Chance for this type vs others */

	byte data[8];	/* data for generation routine */
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
 * "object flags" are now stored in the object data type, as are the
 * known flags.  This allows the identification status of each flag
 * to be stored seperately.
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
	s16b k_idx;	/* Kind index (zero if "dead") */

	s16b ix;	/* X-position on map, or zero */
	s16b iy;	/* Y-position on map, or zero */

	s16b weight;	/* Item weight */

	byte tval;	/* Item type (from kind) */
	byte sval;	/* Item sub-type (from kind) */

	s16b pval;	/* Item extra-parameter */

	byte discount;	/* Discount (if any) */

	byte number;	/* Number of items */

	s16b to_h;	/* Plusses to hit */
	s16b to_d;	/* Plusses to damage */
	s16b to_a;	/* Plusses to AC */

	s16b ac;	/* Normal AC */

	s16b timeout;	/* Timeout Counter */

	byte dd, ds;	/* Damage dice/sides */

	s16b next_o_idx;	/* Next object in stack (if any) */

	s16b inscription;	/* Inscription index */
	s16b xtra_name;	/* Extra Name (Artifacts and ego items) */

	u32b flags[4];	/* Flags */

	u32b kn_flags[4];	/* Known Flags */

	s32b cost;	/* Object "base cost" */
	s32b temp_cost;	/* Cost including shopkeeper effects */

	s16b region;	/* Region */

	byte feeling;	/* Game generated inscription number (eg, pseudo-id) */

	byte a_idx;	/* Artifact type */

	byte info;	/* Special flags */

	s16b trigger[MAX_TRIGGER]; /* Special object scripts */

	bool allocated;	/* Held in the o_list[] array */
};



/*
 * Monster information, for a specific monster.
 *
 * The "hold_o_idx" field points to the first object of a stack
 * of objects (if any) being carried by the monster (see above).
 */

typedef struct monster_type monster_type;

struct monster_type
{
	s16b r_idx;	/* Monster race index */

	s16b csleep;	/* Inactive counter */

	s16b fx;	/* X location on map */
	s16b fy;	/* Y location on map */


	s16b tx;	/* Target X location on map */
	s16b ty;	/* Target Y location on map */

	s16b hp;	/* Current Hit points */
	s16b maxhp;	/* Max Hit points */


	byte mspeed;	/* Monster "speed" */
	byte energy;	/* Monster "energy" */

	byte stunned;	/* Monster is stunned */
	byte confused;	/* Monster is confused */

	byte monfear;	/* Monster is afraid */
	byte invulner;	/* Monster is temporarily invulnerable */

	s16b hold_o_idx;	/* Object being held (if any) */

	u32b smart;	/* Field for "smart_learn" */

	s16b region;	/* Region */

	byte cdis;	/* Current dis from player */

	byte mflag;	/* Extra monster flags */

	bool ml;	/* Monster is "visible" */
};

/*
 * Monster Race blow-method types
 */
typedef struct rbm_type rbm_type;
struct rbm_type
{
	cptr name;
	cptr action;

	u16b sound;

	bool touched;
	bool cut;
	bool stun;
};


/*
 * The thaumaturgical list of fields.
 *
 * (Equivalent to monster races, or object kinds.
 *  They had to be called something. ;-) )
 *
 * Eventually most of this, and the following struct
 * will be wrapped inside a python object.  Only things
 * that need to be accessed quickly will be left as is.
 */

typedef struct field_thaum field_thaum;
struct field_thaum
{
	char *name;	/* The name of the field */

	byte f_attr;	/* attribute */
	char f_char;	/* character */

	byte d_attr;	/* Default attribute */
	char d_char;	/* Default char */

	byte priority;	/* LOS priority higher = more visible */

	byte type;	/* Type of field */

	s16b count_init;	/* Counter for timed effects */

	s16b action[FIELD_ACTION_MAX];	/* Action scripts */

	/* Storage space for the actions to interact with. */
	byte data_init[8];

	u16b info;	/* Information flags */
};


/*
 * The field structure.
 *
 * Fields will be used to create a variety of effects from
 * the ability to place traps on _all_ terrains (not just 
 * dungeon floor), to the nightmare mode automatic corpse raising.
 *
 * The new building / store code will use this structure.
 *
 */
typedef struct field_type field_type;
struct field_type
{
	byte f_attr;	/* attribute */
	char f_char;	/* character */

	s16b t_idx;	/* field type index */

	s16b fy;	/* Y location on map */
	s16b fx;	/* X location on map */

	s16b next_f_idx;	/* Pointer to next field in list */

	u16b info;	/* quick access flags */

	/* Storage space for the actions to interact with. */
	byte data[8];

	s16b counter;	/* Counter for timed effects */

	s16b region;	/* Region */

	byte priority;	/* LOS priority higher = more visible */
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
	s16b index;	/* The actual index */

	byte level;	/* Base dungeon level */
	byte prob1;	/* Probability, pass 1 */
	byte prob2;	/* Probability, pass 2 */
	byte prob3;	/* Probability, pass 3 */
};


/*
 * Available "options"
 *
 *	- Normal and Current Value (TRUE or FALSE)
 *
 *	- Option Page Number (or zero)
 *
 *	- Textual name (or NULL)
 *	- Textual description
 */

typedef struct option_type option_type;

struct option_type
{
	bool o_val;

	byte o_page;

	cptr o_text;
	cptr o_desc;
};


/*
 * There are three types of quest:
 * General quests, Dungeon and wilderness quests.
 * Each has its own type of data it needs to store
 */

/*
 * Quest type-specific data.
 */

/* Bounty */
typedef struct quest_bnt quest_bnt;

struct quest_bnt
{
	u16b r_idx;	/* Monster */
	u16b cur_num;	/* Number killed */
	u16b max_num;	/* Number required */
};

/* Dungeon */
typedef struct quest_dun quest_dun;

struct quest_dun
{
	u16b r_idx;	/* Monster race */
	u16b level;	/* Dungeon level */

	s16b cur_num;	/* Number killed */
	s16b max_num;	/* Number required */
	s16b num_mon;	/* Number on the level */
};

/* Wilderness */
typedef struct quest_wld quest_wld;

struct quest_wld
{
	u16b refcount;	/* Refcount for wilderness code */
	u16b place;	/* Equivalent "place number" */

	u16b data;	/* Data so can choose completion */
	byte depth;	/* Power of monsters */
};

/* Message */
typedef struct quest_msg quest_msg;

struct quest_msg
{
	u16b place;	/* Town to go to */
	u16b shop;	/* Person to give it to */
};


/* Find item */
typedef struct quest_fit quest_fit;

struct quest_fit
{
	u16b a_idx;	/* Artifact index */
	u16b place;	/* Dungeon it is in */
};


/* Find place */
typedef struct quest_fpl quest_fpl;

struct quest_fpl
{
	u16b place;	/* Place to find */
};


/* The union holding the quest-specific data */
typedef union quest_data_type quest_data_type;
union quest_data_type
{
	quest_bnt bnt;
	quest_dun dun;
	quest_wld wld;
	quest_msg msg;
	quest_fit fit;
	quest_fpl fpl;
};

/*
 * Structure for the "quests"
 */
typedef struct quest_type quest_type;

struct quest_type
{
	byte status;	/* Is the quest taken, completed, finished? */

	byte flags;	/* Quest flags */
	byte type;	/* The quest type (gen/dun/wild) */

	byte item;	/* Artificial quest item number */

	u16b place;	/* Town where given */
	u16b shop;	/* Quest-giver */
	u16b reward;	/* Reward level */

	byte c_type;	/* Type of creation trigger */
	byte x_type;	/* Type of action trigger */

	u32b timeout;	/* Time limits */

	char name[128];	/* Quest name */

	quest_data_type data;	/* Quest-specific data */
};


typedef struct magic_type magic_type;

struct magic_type
{
	byte slevel;	/* Required level (to learn) */
	byte smana;	/* Required mana (to cast) */
};


/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */

typedef struct player_magic player_magic;

struct player_magic
{
	int spell_book;	/* Tval of spell books (if any) */
	int spell_xtra;	/* Something for later */

	int spell_stat;	/* Stat for spells (if any) */
	int spell_type;	/* Spell type (mage/priest) */

	int spell_first;	/* Level of first spell */
	int spell_weight;	/* Weight that hurts spells */

	magic_type info[MAX_REALM][32];	/* The available spells */
};



/*
 * Player sex info
 */

typedef struct player_sex player_sex;

struct player_sex
{
	cptr title;	/* Type of sex */

	cptr winner;	/* Name of winner */
};


/*
 * Player racial info
 */

typedef struct player_race player_race;

struct player_race
{
	cptr title;	/* Type of race */

	s16b r_adj[A_MAX];	/* Racial stat bonuses */

	s16b r_dis;	/* disarming */
	s16b r_dev;	/* magic devices */
	s16b r_sav;	/* saving throw */
	s16b r_stl;	/* stealth */
	s16b r_sns;	/* sensing ability */
	s16b r_fos;	/* search frequency */
	s16b r_thn;	/* combat (normal) */
	s16b r_thb;	/* combat (shooting) */

	byte r_mhp;	/* Race hit-dice modifier */
	byte r_exp;	/* Race experience factor */

	byte b_age;	/* base age */
	byte m_age;	/* mod age */

	byte m_b_ht;	/* base height (males) */
	byte m_m_ht;	/* mod height (males) */
	byte m_b_wt;	/* base weight (males) */
	byte m_m_wt;	/* mod weight (males) */

	byte f_b_ht;	/* base height (females) */
	byte f_m_ht;	/* mod height (females) */
	byte f_b_wt;	/* base weight (females) */
	byte f_m_wt;	/* mod weight (females) */

	byte infra;	/* Infra-vision range */
};


/*
 * Player class info
 */

typedef struct player_class player_class;

struct player_class
{
	cptr title;	/* Type of class */

	s16b c_adj[A_MAX];	/* Class stat modifier */

	s16b c_dis;	/* class disarming */
	s16b c_dev;	/* class magic devices */
	s16b c_sav;	/* class saving throws */
	s16b c_stl;	/* class stealth */
	s16b c_sns;	/* class sensing ability */
	s16b c_fos;	/* class searching frequency */
	s16b c_thn;	/* class to hit (normal) */
	s16b c_thb;	/* class to hit (bows) */

	s16b x_dis;	/* extra disarming */
	s16b x_dev;	/* extra magic devices */
	s16b x_sav;	/* extra saving throws */
	s16b x_stl;	/* extra stealth */
	s16b x_sns;	/* extra sensing ability */
	s16b x_fos;	/* extra searching frequency */
	s16b x_thn;	/* extra to hit (normal) */
	s16b x_thb;	/* extra to hit (bows) */

	s16b c_mhp;	/* Class hit-dice adjustment */
	s16b c_exp;	/* Class experience factor */

	byte pet_upkeep_div;	/* Pet upkeep divider */
	bool heavy_sense;
};


/*
 * Data describing the character.
 *
 * (Mostly roll-playing information)
 */
typedef struct player_data player_data;

struct player_data
{
	s16b age;	/* Characters age */
	s16b ht;	/* Height */
	s16b wt;	/* Weight */
	s16b sc;	/* Social Class */
	
	byte hitdie;	/* Hit dice (sides) */

	byte psex;	/* Sex index */
	byte prace;	/* Race index */
	byte pclass;	/* Class index */
};


/*
 * Player spell data
 */
typedef struct player_realm player_realm;

struct player_realm
{
	u32b learned;	/* Spell flags */
	u32b worked;	/* Spell flags */
	u32b forgotten;	/* Spell flags */
	
	byte realm;	/* Realm number */
};

typedef struct player_spell player_spell;

struct player_spell
{
	player_realm r[2];	/* Magic realms */
	byte order[PY_MAX_SPELLS];	/* Spell order */
};

/*
 * Timed effects acting on the player
 */
typedef struct player_timed player_timed;

struct player_timed
{
	s16b fast;	/* Timed -- Fast */
	s16b slow;	/* Timed -- Slow */
	s16b blind;	/* Timed -- Blindness */
	s16b paralyzed;	/* Timed -- Paralysis */
	s16b confused;	/* Timed -- Confusion */
	s16b afraid;	/* Timed -- Fear */
	s16b image;	/* Timed -- Hallucination */
	s16b poisoned;	/* Timed -- Poisoned */
	s16b cut;	/* Timed -- Cut */
	s16b stun;	/* Timed -- Stun */

	s16b protevil;	/* Timed -- Protection */
	s16b invuln;	/* Timed -- Invulnerable */
	s16b hero;	/* Timed -- Heroism */
	s16b shero;	/* Timed -- Super Heroism */
	s16b shield;	/* Timed -- Shield Spell */
	s16b blessed;	/* Timed -- Blessed */
	s16b invis;	/* Timed -- See Invisible */
	s16b infra;	/* Timed -- Infra Vision */

	s16b oppose_acid;	/* Timed -- oppose acid */
	s16b oppose_elec;	/* Timed -- oppose lightning */
	s16b oppose_fire;	/* Timed -- oppose heat */
	s16b oppose_cold;	/* Timed -- oppose cold */
	s16b oppose_pois;	/* Timed -- oppose poison */
	s16b esp;	/* Timed ESP */
	s16b wraith_form;	/* Timed wraithform */
	s16b resist_magic;	/* Timed Resist Magic (later) */
	
	s16b word_recall;	/* Word of recall counter */
};

/*
 * The state of the player
 */
typedef struct player_state player_state;

struct player_state
{
	char died_from[80];	/* Cause of death */
	
	s16b resting;	/* Resting counter */
	s16b running;	/* Running counter */

	byte confusing;	/* Glowing hands */
	byte searching;	/* Currently searching */

	u16b total_winner;	/* Total winner */

	u16b panic_save;	/* Panic save */
	u16b noscore;	/* Cheating flags */

	bool is_dead;	/* Player is dead */
	bool wizard;	/* Player is in wizard mode */
	
	bool playing;	/* True if player is playing */
	bool leaving;	/* True if player is leaving */

	bool create_up_stair;	/* Create up stair on next level */
	bool create_down_stair;	/* Create down stair on next level */
	
	byte feeling;	/* Most recent feeling */
	
	s16b energy_use;	/* Energy use this turn */
	
	bool cumber_armor;	/* Mana draining armor */
	bool cumber_glove;	/* Mana draining gloves */
	bool heavy_wield;	/* Heavy weapon */
	bool heavy_shoot;	/* Heavy shooter */
	bool icky_wield;	/* Icky weapon */
	bool detected;	/* Detected for traps? */
	
	bool skip_more;	/* Skip the --more-- prompt */
	bool mon_fight;	/* Monster fighting indicator */

	bool monk_armour_stat;	/* Status of monk armour */
	
	byte noise_level;	/* Amount of noise since last update */
	
	u16b store_top;		/* Top of store inventory list */
};

/*
 * Information about the player state for the running code
 */
typedef struct player_run player_run;

struct player_run
{
	s16b cur_dir;	/* Direction we are running */
	s16b old_dir;	/* Direction we came from */
	int  mode;      /* Current running algorithm */
};

/*
 * The current state of the command being executed
 * and the keys being pressed
 */
typedef struct player_command player_command;

struct player_command
{
	s16b cmd;	/* Gives identity of current command */
	s16b arg;	/* Gives argument of current command */
	s16b rep;	/* Gives repetition of current command */
	s16b dir;	/* Gives direction of current command */

	s16b new;	/* Hack -- command chaining XXX XXX */
	
	/* Inkey status */
	bool inkey_base;	/* See the "inkey()" function */
	bool inkey_xtra;	/* See the "inkey()" function */
	bool inkey_scan;	/* See the "inkey()" function */
	bool inkey_flag;	/* See the "inkey()" function */
};

/*
 * The player stats
 */
typedef struct player_stat player_stat;

struct player_stat
{
	s16b max;	/* Current "maximal" stat values */
	s16b cur;	/* Current "natural" stat values */

	s16b use;	/* Current modified stats */
	s16b top;	/* Maximal modified stats */

	s16b add;	/* Equipment stat bonuses */
	s16b ind;	/* Indexes into stat tables */
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
	s16b px;	/* Player location */
	s16b py;	/* Player location */
	
	player_data rp;	/* Role-play information */
	
	s16b depth;	/* Cur depth */

	s16b max_lev;	/* Max level */
	s16b lev;	/* Cur level */

	u16b exp_frac;	/* Cur exp frac (times 2^16) */

	s32b max_exp;	/* Max experience */
	s32b exp;	/* Cur experience */

	s32b au;	/* Current Gold */

	s16b place_num;	/* Current place number in the wilderness */

	s32b wilderness_x;	/* Coordinates in the wilderness */
	s32b wilderness_y;

	s16b old_wild_x;	/* Previous block coords in the wilderness */
	s16b old_wild_y;

	s16b panel_x1;	/* Coordinates of top left hand corner of panel */
	s16b panel_y1;

	s16b panel_x2;	/* Coordinates of bottom right hand corner of panel */
	s16b panel_y2;

	s16b mhp;	/* Max hit pts */
	s16b chp;	/* Cur hit pts */
	u16b chp_frac;	/* Cur hit frac (times 2^16) */

	s16b msp;	/* Max mana pts */
	s16b csp;	/* Cur mana pts */
	u16b csp_frac;	/* Cur mana frac (times 2^16) */

	player_spell spell;	/* Spell information */

	u32b muta1;	/* Mutations */
	u32b muta2;	/* Mutations */
	u32b muta3;	/* Mutations */

	s16b virtues[MAX_PLAYER_VIRTUES];
	s16b vir_types[MAX_PLAYER_VIRTUES];

	s16b energy;	/* Current energy */
	s16b food;	/* Current nutrition */

	s16b player_hp[PY_MAX_LEVEL];	/* HP Array */

	u16b expfact;	/* Experience factor
					 * Note: was byte, causing overflow for Amberite
					 * characters (such as Amberite Paladins)
					 */
	s16b chaos_patron;	/* Players Chaos Patron */

	player_timed tim;	/* Timed effects */
	
	player_state state;	/* Internal state of the player */
	
	s16b skills[MAX_SKILL];	/* Player skills */
	
	player_command cmd;	/* The current command status */
	
	player_stat stat[A_MAX];	/* The player's stats */

	/*** Pointers to player grid information ***/

	pcave_type *pcave[MAX_HGT];
	pblk_ptr **pwild;

	/*** Boundary of player-known area ****/
	u16b max_hgt;
	u16b min_hgt;
	u16b max_wid;
	u16b min_wid;

	/*** Temporary fields ***/

	s32b align;	/* Good/evil/neutral */

	s16b total_weight;	/* Total weight being carried */

	s16b inventory;	/* Index to inventory item list */
	object_type equipment[EQUIP_MAX];	/* Equipment */

	s16b target_set;	/* Target flag */
	s16b target_who;	/* Target identity */
	s16b target_row;	/* Target location */
	s16b target_col;	/* Target location */

	s16b health_who;	/* Health bar trackee */

	s16b monster_race_idx;	/* Monster race trackee */
	u16b max_seen_r_idx;	/* Most powerful monster visible */

	s16b object_kind_idx;	/* Object kind trackee */
	
	player_run run;		/* Current stat of the running routine */

	s16b new_spells;	/* Number of spells available */

	s16b cur_lite;	/* Radius of lite (if any) */

	u32b notice;	/* Special Updates (bit flags) */
	u32b update;	/* Pending Updates (bit flags) */
	u32b redraw;	/* Normal Redraws (bit flags) */
	u32b window;	/* Window Redraws (bit flags) */
	u32b change;	/* Once per turn (bit flags) */
	
	/*
	 * Flags on equipment items and the racial/class
	 * effects logical-ored together.
	 */
	u32b flags[4];

	/*** Extracted fields ***/
	s16b dis_to_h;	/* Known bonus to hit */
	s16b dis_to_d;	/* Known bonus to dam */
	s16b dis_to_a;	/* Known bonus to ac */

	s16b dis_ac;	/* Known base ac */

	s16b to_h;	/* Bonus to hit */
	s16b to_d;	/* Bonus to dam */
	s16b to_a;	/* Bonus to ac */

	s16b ac;	/* Base ac */

	s16b see_infra;	/* Infravision range */

	u32b noise;	/* Derived from stealth */

	s16b num_blow;	/* Number of blows */
	s16b num_fire;	/* Number of shots */

	byte ammo_mult;	/* Ammo multiplier */
	byte ammo_tval;	/* Ammo variety */
	byte bow_energy;	/* shooter speed */

	s16b pspeed;	/* Current speed */

	s16b sp_bonus;  /* Extra mana per level */

	/*** Pet commands ***/

	s16b pet_follow_distance;	/* Length of the imaginary "leash" for pets */
	byte pet_open_doors;	/* flag - allow pets to open doors */
	byte pet_pickup_items;	/* flag - allow pets to pickup items */

	/* Options */
	bool options[OPT_PLAYER];
	bool birth[OPT_BIRTH];
};


/*
 * For multiplayer use.
 *
 * Various information must be stored on the server
 * when a multiplayer version is created.
 *
 * This structure contains such information.
 * At the moment, this only has dungeon-specific options.
 * (Which are not already "birth" options.)
 */
typedef struct server_type server_type;

struct server_type
{
	bool options[OPT_SERVER];
};



/* For Monk martial arts */

typedef struct martial_arts martial_arts;

struct martial_arts
{
	cptr desc;	/* A verbose attack description */
	int min_level;	/* Minimum level to use */
	int chance;	/* Chance of 'success' */
	int dd;	/* Damage dice */
	int ds;	/* Damage sides */
	int effect;	/* Special effects */
};



/* Mindcrafters */

typedef struct mindcraft_power mindcraft_power;
struct mindcraft_power
{
	int min_lev;
	int mana_cost;
	int fail;
	cptr name;
};

#if 0
/*
 * A store owner
 */
typedef struct owner_type owner_type;

struct owner_type
{
	cptr owner_name;	/* Name */

	s16b max_cost;	/* Purse limit / 100 */

	byte greed;	/* Greed level */
};


/*
 * A building owner
 */
typedef struct b_own_type b_own_type;

struct b_own_type
{
	cptr owner_name;	/* Name */

	byte inflate;	/* Inflation */
};
#endif /* 0 */


/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
typedef struct store_type store_type;

struct store_type
{
	byte type;	/* Store type */
	
	byte greed;	/* Greed value */
	s16b max_cost;	/* Purse limit / 100 */
	s16b owner_name;	/* Owner name */
	
	s16b data;	/* Data used for various things */

	s32b last_visit;	/* Last visited on this turn */

	s16b stock;	/* Stock -- list of items in o_list[] */

	u16b x;	/* Location x coord. */
	u16b y;	/* Location y coord. */

	byte max_stock;	/* Stock -- Max number of entries */
};


/* Dungeons */
typedef struct dun_type dun_type;
struct dun_type
{
	obj_theme theme;	/* Dungeon object theme */
	
	u32b habitat;	/* Flags describing habitat */

	byte min_level;	/* Minimum level in the dungeon */
	byte max_level;	/* Maximum dungeon level allowed */

	s16b rating;	/* Level's current rating */

	s16b region;	/* Hack - Region for current level */
	
	u16b rooms;		/* Room types available */
	
	byte recall_depth;	/* Recall depth */
	
	bool good_item_flag;	/* True if "Artifact" on this level */
	
	byte floor;		/* Floor terrain type */
	
	byte liquid;	/* Liquid type for lakes/ rivers etc. */
	
	byte flags;		/* Extra flags */
};


/* Type holding dungeon type information */
typedef struct dun_gen_type dun_gen_type;

struct dun_gen_type
{
	/* Theme information */
	obj_theme theme;
	u32b habitat;
	
	/* Level bounds for fixed dungeons */
	int min_level;
	int max_level;

	/* Probability (inverse rarity) */
	int chance;
	
	/* Wilderness location */
	byte pop;
	byte height;
	
	/* Room types available */
	u16b rooms;
	
	/* Floor terrain type */
	byte floor;
	
	/* Liquid type for lakes/ rivers etc. */
	byte liquid;
	
	/* Extra flags */
	byte flags;
};


/*
 * A structure describing a place with
 * stores and buildings.
 */
typedef struct place_type place_type;
struct place_type
{
	u32b seed;	/* Seed for RNG */
	store_type *store;	/* The stores[numstores] */

	dun_type *dungeon;

	byte type;	/* Type of place */

	byte numstores;
	u16b quest_num;	/* Quest number if is special */

	byte x;	/* Location mod 16 in wilderness */
	byte y;

	byte xsize;	/* Size in wilderness */
	byte ysize;	/* Size in wilderness */

	byte data;	/* pop for towns, generic for quests */
	byte monst_type;	/* Type of population (monsters/people etc.) */

	s16b region;	/* Region */

	byte gates_x[MAX_GATES];	/* Position of the town gates */
	byte gates_y[MAX_GATES];

	char name[T_NAME_LEN];	/* Town name */
};


/* Various function pointer types */
typedef bool (*monster_hook_type) (int r_idx);
typedef byte (*object_hook_type) (int k_idx);
typedef int (*inven_func) (object_type *);
typedef bool (*cave_hook_type) (const cave_type *c_ptr);
typedef bool (*object_comp) (const object_type *, const object_type *);



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
	char what[8];	/* Version info (string) */

	char pts[10];	/* Total Score (number) */

	char gold[10];	/* Total Gold (number) */

	char turns[10];	/* Turns Taken (number) */

	char day[10];	/* Time stamp (string) */

	char who[16];	/* Player Name (string) */

	char uid[8];	/* Player UID (number) */

	char sex[2];	/* Player Sex (string) */
	char p_r[3];	/* Player Race (number) */
	char p_c[3];	/* Player Class (number) */

	char cur_lev[4];	/* Current Player Level (number) */
	char cur_dun[4];	/* Current Dungeon Level (number) */
	char max_lev[4];	/* Max Player Level (number) */
	char max_dun[4];	/* Max Dungeon Level (number) */

	char how[32];	/* Method of death (string) */
};

/*
 * Struct for mutations and racial powers
 */

typedef struct mutation_type mutation_type;

struct mutation_type
{
	u32b which;	/* Actual mutation (mask) */

	cptr desc_text;	/* Text describing mutation */
	cptr gain_text;	/* Text displayed on gaining the mutation */
	cptr lose_text;	/* Text displayed on losing the mutation */

	char name[39];	/* Short description (activatable mutations) */
	byte level;	/* Minimum level (activatable mutations) */

	int cost;	/* Mana/HP Cost (activatable mutations) */
	int stat;	/* Stat dependency (activatable mutations) */
	int diff;	/* Difficulty (activatable mutations) */
	int chance;	/* Chance of occuring (random mutations) / 100 */

};


/*
 * A function pointer used in displaying menus
 *
 * The function takes a number for the option chosen
 * and will return TRUE if the selection works, and FALSE
 * if the menu should stay up.
 */
typedef bool (*menu_select_type) (int option);

typedef struct menu_type menu_type;

struct menu_type
{
	cptr text;					/* Option text */
	cptr help;					/* Help file to use */
	menu_select_type action;	/* Action to do */

	byte flags;					/* Flags controling option behaviour */
};


/*
 * Object bonuses to various stuff
 */
typedef struct bonuses_type bonuses_type;

struct bonuses_type
{
	int stat[6];
	int sp_bonus;
	int skills[MAX_SKILL];
	int see_infra;
	int pspeed;
	int extra_blows;
	int extra_shots;
};
