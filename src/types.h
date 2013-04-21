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
 * this includes the tval/sval/weight fields in "object_type", various fields
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

struct header
{
	byte	v_major;		/* Version -- major */
	byte	v_minor;		/* Version -- minor */
	byte	v_patch;		/* Version -- patch */
	byte	v_extra;		/* Version -- extra */


	u16b	info_num;		/* Number of "info" records */

	u16b	info_len;		/* Size of each "info" record */


	u32b	head_size;		/* Size of the "header" in bytes */

	u32b	info_size;		/* Size of the "info" array in bytes */

	u32b	name_size;		/* Size of the "name" array in bytes */

	u32b	text_size;		/* Size of the "text" array in bytes */
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
 * Information about terrain "features"
 */

typedef struct feature_type feature_type;

struct feature_type
{
        u32b name;              /* Name (offset) */
        u32b text;              /* Text (offset) */

        byte mimic;             /* Feature to mimic */

        u32b flags1;            /* First set of flags */

        byte extra;             /* Extra byte (unused) */

	s16b unused;		/* Extra bytes (unused) */

	byte d_attr;		/* Default feature attribute */
	char d_char;		/* Default feature character */


	byte x_attr;		/* Desired feature attribute */
	char x_char;		/* Desired feature character */
};


/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */

typedef struct object_kind object_kind;

struct object_kind
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

	byte tval;			/* Object type */
	byte sval;			/* Object sub type */

        s32b pval;                      /* Object extra info */

	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to damage */
	s16b to_a;			/* Bonus to armor */

	s16b ac;			/* Base armor */

	byte dd, ds;		/* Damage dice/sides */

        s32b weight;            /* Weight */

	s32b cost;			/* Object "base cost" */

	u32b flags1;		/* Flags, set 1 */
	u32b flags2;		/* Flags, set 2 */
	u32b flags3;		/* Flags, set 3 */
        u32b flags4;            /* Flags, set 4 */

	byte locale[4];		/* Allocation level(s) */
	byte chance[4];		/* Allocation chance(s) */

	byte level;			/* Level */
	byte extra;			/* Something */


	byte d_attr;		/* Default object attribute */
	char d_char;		/* Default object character */


	byte x_attr;		/* Desired object attribute */
	char x_char;		/* Desired object character */


	byte flavor;			/* Special object flavor (or zero) */

	bool easy_know;		/* This object is always known (if aware) */


	bool aware;			/* The player is "aware" of the item's effects */

	bool tried;			/* The player has "tried" one of the items */

        bool know;                      /* extractable flag for the alchemist */

        byte squeltch;                  /* Should we destroy it ? */

        u32b esp;                       /* ESP flags */
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
	u32b name;			/* Name (offset) */
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

	u32b flags1;		/* Artifact Flags, set 1 */
	u32b flags2;		/* Artifact Flags, set 2 */
	u32b flags3;		/* Artifact Flags, set 3 */
        u32b flags4;            /* Artifact Flags, set 4 */

	byte level;			/* Artifact level */
	byte rarity;		/* Artifact rarity */

	byte cur_num;		/* Number created (0 or 1) */
	byte max_num;		/* Unused (should be "1") */

        u32b esp;                       /* ESP flags */
};


/*
 * Information about "ego-items".
 */

typedef struct ego_item_type ego_item_type;

struct ego_item_type
{
	u32b name;			/* Name (offset) */
	u32b text;			/* Text (offset) */

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
        u32b flags4;            /* Ego-Item Flags, set 4 */

        u32b esp;                       /* ESP flags */
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
        u32b text;                              /* Text (offset) */

	byte hdice;				/* Creatures hit dice count */
	byte hside;				/* Creatures hit dice sides */

	s16b ac;				/* Armour Class */

	s16b sleep;				/* Inactive counter (base) */
	byte aaf;				/* Area affect radius (1-100) */
	byte speed;				/* Speed (normally 110) */

	s32b mexp;				/* Exp value for kill */

        s32b weight;                            /* Weight of the monster */

	byte freq_inate;		/* Inate spell frequency */
	byte freq_spell;		/* Other spell frequency */

	u32b flags1;			/* Flags 1 (general) */
	u32b flags2;			/* Flags 2 (abilities) */
	u32b flags3;			/* Flags 3 (race/resist) */
	u32b flags4;			/* Flags 4 (inate/breath) */
	u32b flags5;			/* Flags 5 (normal spells) */
	u32b flags6;			/* Flags 6 (special spells) */
	u32b flags7;			/* Flags 7 (movement related abilities) */
	u32b flags8;			/* Flags 8 (wilderness info) */
	u32b flags9;			/* Flags 9 (drops info) */

        monster_blow blow[4];           /* Up to four blows per round */

        byte body_parts[BODY_MAX];      /* To help to decide what to use when body changing */

        byte level;                     /* Level of creature */
	byte rarity;			/* Rarity of creature */


	byte d_attr;			/* Default monster attribute */
	char d_char;			/* Default monster character */


	byte x_attr;			/* Desired monster attribute */
	char x_char;			/* Desired monster character */


        s16b max_num;                   /* Maximum population allowed per level */

	byte cur_num;			/* Monster population on current level */


	s16b r_sights;			/* Count sightings of this monster */
	s16b r_deaths;			/* Count deaths from this monster */

	s16b r_pkills;			/* Count monsters killed in this life */
	s16b r_tkills;			/* Count monsters killed in all lives */

	byte r_wake;			/* Number of times woken up (?) */
	byte r_ignore;			/* Number of times ignored (?) */

	byte r_xtra1;			/* Something (unused) */
	byte r_xtra2;			/* Something (unused) */

	byte r_drop_gold;		/* Max number of gold dropped at once */
	byte r_drop_item;		/* Max number of item dropped at once */

	byte r_cast_inate;		/* Max number of inate spells seen */
	byte r_cast_spell;		/* Max number of other spells seen */

	byte r_blows[4];		/* Number of times each blow type was seen */

	u32b r_flags1;			/* Observed racial flags */
	u32b r_flags2;			/* Observed racial flags */
	u32b r_flags3;			/* Observed racial flags */
	u32b r_flags4;			/* Observed racial flags */
	u32b r_flags5;			/* Observed racial flags */
	u32b r_flags6;			/* Observed racial flags */
	u32b r_flags7;			/* Observed racial flags */
        u32b r_flags8;                  /* Observed racial flags */
        u32b r_flags9;                  /* Observed racial flags */

        bool on_saved;                  /* Is the (unique) on a saved level ? */

        byte total_visible;             /* Amount of this race that are visible */

        obj_theme drops;                /* The drops type */
};


typedef struct monster_ego monster_ego;

struct monster_ego
{
	u32b name;				/* Name (offset) */

        s16b hdice;                             /* Creatures hit dice count */
        s16b hside;                             /* Creatures hit dice sides */

	s16b ac;				/* Armour Class */

	s16b sleep;				/* Inactive counter (base) */
        s16b aaf;                               /* Area affect radius (1-100) */
        s16b speed;                             /* Speed (normally 110) */

	s32b mexp;				/* Exp value for kill */

        s32b weight;                            /* Weight of the monster */

        byte freq_inate;                /* Inate spell frequency */
	byte freq_spell;		/* Other spell frequency */

        /* Ego flags */
        u32b flags1;                    /* Flags 1 */

        /* Monster flags */
        u32b mflags1;                    /* Flags 1 (general) */
        u32b mflags2;                    /* Flags 2 (abilities) */
        u32b mflags3;                    /* Flags 3 (race/resist) */
        u32b mflags4;                    /* Flags 4 (inate/breath) */
        u32b mflags5;                    /* Flags 5 (normal spells) */
        u32b mflags6;                    /* Flags 6 (special spells) */
        u32b mflags7;                    /* Flags 7 (movement related abilities) */
        u32b mflags8;                    /* Flags 8 (wilderness info) */
        u32b mflags9;                    /* Flags 9 (drops info) */

        /* Negative Flags, to be removed from the monster flags */
        u32b nflags1;                    /* Flags 1 (general) */
        u32b nflags2;                    /* Flags 2 (abilities) */
        u32b nflags3;                    /* Flags 3 (race/resist) */
        u32b nflags4;                    /* Flags 4 (inate/breath) */
        u32b nflags5;                    /* Flags 5 (normal spells) */
        u32b nflags6;                    /* Flags 6 (special spells) */
        u32b nflags7;                    /* Flags 7 (movement related abilities) */
        u32b nflags8;                    /* Flags 8 (wilderness info) */
        u32b nflags9;                    /* Flags 9 (drops info) */

        s16b level;                     /* Level of creature */
        s16b rarity;                    /* Rarity of creature */


	byte d_attr;			/* Default monster attribute */
	char d_char;			/* Default monster character */
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

        s16b lvl;                       /* level of special (if any) */
        byte dun_type;                  /* Dungeon type where the level will show up */

        s16b mon[10];                   /* special monster */
        int item[3];                   /* number of item (usually artifact) */
};


/* jk */
/* name and description are in some other arrays */
typedef struct trap_type trap_type;
struct trap_type {
  s16b probability; /* probability of existence */
  s16b another;     /* does this trap easily combine */
  s16b p1valinc;     /* how much does this trap attribute to p1val */
  byte difficulty;  /* how difficult to disarm */
  byte minlevel;    /* what is the minimum level on which the traps should be */
  byte color;       /* what is the color on screen */
  u32b flags;       /* where can these traps go - and perhaps other flags */
  bool ident;       /* do we know the name */
  s16b known;       /* how well is this trap known */
  s16b name;        /* normal name like weakness */
  s16b dd, ds;      /* base damage */
  s16b text;        /* longer description once you've met this trap */
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
	s16b info;		/* Hack -- cave flags */

	byte feat;		/* Hack -- feature type */

	s16b o_idx;		/* Object in this grid */

	s16b m_idx;		/* Monster in this grid */

	s16b t_idx;		/* trap index (in t_list) or zero       */
	
        s16b special, special2; /* Special cave info */

        s16b inscription;       /* Inscription of the grid */

        byte mana;              /* Magical energy of the grid */

	byte mimic;		/* Feature to mimic */

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
	byte sval;			/* Item sub-type (from kind) */

        s32b pval;                      /* Item extra-parameter */
        s16b pval2;                     /* Item extra-parameter for some special
                                           items*/
        s16b pval3;                     /* Item extra-parameter for some special
                                           items*/

	byte discount;		/* Discount (if any) */

	byte number;		/* Number of items */

        s32b weight;            /* Item weight */

        byte elevel;            /* Item exp level */
        s32b exp;               /* Item exp */

	byte name1;			/* Artifact type, if any */
	byte name2;			/* Ego-Item type, if any */

	byte xtra1;			/* Extra info type */
	byte xtra2;			/* Extra info index */

	s16b to_h;			/* Plusses to hit */
	s16b to_d;			/* Plusses to damage */
	s16b to_a;			/* Plusses to AC */

	s16b ac;			/* Normal AC */

	byte dd, ds;		/* Damage dice/sides */

	s16b timeout;		/* Timeout Counter */

	byte ident;			/* Special flags  */

	byte marked;		/* Object is marked */

	u16b note;			/* Inscription index */
    u16b art_name;      /* Artifact name (random artifacts) */

    u32b art_flags1;        /* Flags, set 1  Alas, these were necessary */
    u32b art_flags2;        /* Flags, set 2  for the random artifacts of*/
    u32b art_flags3;        /* Flags, set 3  Zangband */
    u32b art_flags4;        /* Flags, set 4  PernAngband */
    u32b art_esp;           /* Flags, set esp  PernAngband */

	
	s16b next_o_idx;	/* Next object in stack (if any) */

	s16b held_m_idx;	/* Monster holding us (if any) */

        byte sense;             /* Pseudo-id status */
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

        u16b ego;                       /* Ego monster type */

	byte fy;			/* Y location on map */
	byte fx;			/* X location on map */

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

#ifdef WDT_TRACK_OPTIONS

	byte ty;			/* Y location of target */
	byte tx;			/* X location of target */

	byte t_dur;			/* How long are we tracking */

	byte t_bit;			/* Up to eight bit flags */

#endif

#ifdef DRS_SMART_OPTIONS

	u32b smart;			/* Field for "smart_learn" */

#endif

        bool imprinted;                  /* Is the monster imprinted(if he can)? */
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

	u16b total;		/* Unused for now */
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
	bool	*o_var;

	byte	o_norm;

	byte	o_page;

	byte	o_set;
	byte	o_bit;

	cptr	o_text;
	cptr	o_desc;
};


/*
 * Structure for the "quests"
 */
typedef struct quest_type quest_type;
                      
struct quest_type
{
	s16b status;            /* Is the quest taken, completed, finished? */

	s16b type;              /* The quest type */

	char name[60];          /* Quest name */
	s16b level;             /* Dungeon level */
	s16b r_idx;             /* Monster race */

	s16b cur_num;           /* Number killed */
	s16b max_num;           /* Number required */

	s16b k_idx;             /* object index */
	s16b num_mon;           /* number of monsters on level */

        byte flags;             /* Quest flags */
};


/*
 * A store owner
 */
typedef struct owner_type owner_type;

struct owner_type
{
        u32b name;                      /* Name (offset) */

        s16b max_cost;                  /* Purse limit */

        byte max_inflate;               /* Inflation (max) */
        byte min_inflate;               /* Inflation (min) */

        byte haggle_per;                /* Haggle unit */

        byte insult_max;                /* Insult limit */

        s32b races[2];                  /* Liked/hated races */
        s32b classes[2];                /* Liked/hated classes */
        s32b realms[2];                 /* Liked/hated realms */

        s16b costs[3];                  /* Costs for liked people */
};




/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
typedef struct store_type store_type;

struct store_type
{
        u16b st_idx;

        u16b owner;                     /* Owner index */

	s16b insult_cur;		/* Insult counter */

	s16b good_buy;			/* Number of "good" buys */
	s16b bad_buy;			/* Number of "bad" buys */

	s32b store_open;		/* Closed until this turn */

	s32b last_visit;		/* Last visited on this turn */

	byte stock_num;			/* Stock -- Number of entries */
	s16b stock_size;		/* Stock -- Total Size of Array */
	object_type *stock;		/* Stock -- Actual stock items */
};

/*
 * A store/building type
 */
typedef struct store_info_type store_info_type;

struct store_info_type
{
        u32b name;                      /* Name (offset) */

        s16b table[STORE_CHOICES][2];   /* Table -- Legal item kinds */
        byte table_num;                 /* Number of items */

        u16b owners[4];                 /* List of owners(refers to ow_info) */

        u16b actions[6];                /* Actions(refers to ba_info) */

        char chr;                       /* Char of the building */
        byte attr;                      /* Attr of the building */
};

/*
 * Stores/buildings actions
 */
typedef struct store_action_type store_action_type;

struct store_action_type
{
        u32b name;                      /* Name (offset) */

        s16b costs[3];                  /* Costs for liked people */
        char letter;                    /* Action letter */
        s16b action;                    /* Action code */
        s16b action_restr;              /* Action restriction */
};

/*
 * The "name" of spell 'N' is stored as spell_names[X][N],
 * where X is 0 for mage-spells and 1 for priest-spells.
 */
typedef struct magic_type magic_type;

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

typedef struct player_magic player_magic;

struct player_magic
{
	int spell_book;		/* Tval of spell books (if any) */
	int spell_xtra;		/* Something for later */

	int spell_stat;		/* Stat for spells (if any)  */
	int spell_type;		/* Spell type (mage/priest) */

        int spell_lev;          /* The higher it is the higher the spells level are */
        int spell_fail;         /* The higher it is the higher the spells failure are */
        int spell_mana;         /* The higher it is the higher the spells mana are */
        int spell_first;        /* Level of first spell */
        int spell_weight;       /* Weight that hurts spells */
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

	s16b r_adj[6];		/* Racial stat bonuses */

	s16b r_dis;			/* disarming */
	s16b r_dev;			/* magic devices */
	s16b r_sav;			/* saving throw */
	s16b r_stl;			/* stealth */
	s16b r_srh;			/* search ability */
	s16b r_fos;			/* search frequency */
	s16b r_thn;			/* combat (normal) */
	s16b r_thb;			/* combat (shooting) */

	byte r_mhp;			/* Race hit-dice modifier */
        u16b r_exp;                     /* Race experience factor */

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

        u32b choice;        /* Legal class choices */
/*    byte choice_xtra;   */
};


/*
 * Player class info
 */

typedef struct player_class player_class;

struct player_class
{
	cptr title;			/* Type of class */

	s16b c_adj[6];		/* Class stat modifier */

	s16b c_dis;			/* class disarming */
	s16b c_dev;			/* class magic devices */
	s16b c_sav;			/* class saving throws */
	s16b c_stl;			/* class stealth */
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
	s16b oldpy;		/* Previous player location -KMW- */
	s16b oldpx;		/* Previous player location -KMW- */

	byte psex;			/* Sex index */
	byte prace;			/* Race index */
	byte pclass;		/* Class index */
        u16b realm1;        /* First magic realm */
        u16b realm2;        /* Second magic realm */
        byte mimic_form;        /* Actualy transformation */
	byte oops;			/* Unused */

	byte hitdie;		/* Hit dice (sides) */
        u16b expfact;       /* Experience factor */                            

	byte maximize;		/* Maximize stats */
	byte preserve;		/* Preserve artifacts */
        byte special;           /* Special levels */
        byte allow_one_death;          /* Blood of live */

	s16b age;			/* Characters age */
	s16b ht;			/* Height */
	s16b wt;			/* Weight */
	s16b sc;			/* Social Class */


	s32b au;			/* Current Gold */

	s32b max_exp;		/* Max experience */
	s32b exp;			/* Cur experience */
	u16b exp_frac;		/* Cur exp frac (times 2^16) */

	s16b lev;			/* Level */

	s16b town_num;			/* Current town number */
	s16b arena_number;		/* monster number in arena -KMW- */
	s16b inside_arena;		/* Is character inside arena? */
	s16b inside_quest;		/* Inside quest level */
	bool exit_bldg;			/* Goal obtained in arena? -KMW- */

        s16b rewards[MAX_BACT];         /* Status of rewards in town */

        s32b wilderness_x;              /* Coordinates in the wilderness */
	s32b wilderness_y;
        bool wild_mode;                 /* TRUE = Small map, FLASE = Big map */

	s16b mhp;			/* Max hit pts */
	s16b chp;			/* Cur hit pts */
	u16b chp_frac;		/* Cur hit frac (times 2^16) */

	s16b msp;			/* Max mana pts */
	s16b csp;			/* Cur mana pts */
	u16b csp_frac;		/* Cur mana frac (times 2^16) */

        s16b msane;                   /* Max sanity */
        s16b csane;                   /* Cur sanity */
        u16b csane_frac;              /* Cur sanity frac */

        s16b mtp;                       /* Max tank pts */
        s16b ctp;                       /* Cur tank pts */
        s16b tp_aux1;                   /* aux1 tank pts */
        s16b tp_aux2;                   /* aux2 tank pts */

        s32b grace;                   /* Your God's appreciation factor. */
        s32b god_favor;               /* Last time you asked for a favor. */
        byte pgod;                    /* Your God. */

	s16b max_plv;		/* Max Player Level */

	s16b stat_max[6];	/* Current "maximal" stat values */
	s16b stat_cur[6];	/* Current "natural" stat values */

	s16b fast;			/* Timed -- Fast */
        s16b lightspeed;                /* Timed -- Light Speed */
	s16b slow;			/* Timed -- Slow */
	s16b blind;			/* Timed -- Blindness */
	s16b paralyzed;		/* Timed -- Paralysis */
	s16b confused;		/* Timed -- Confusion */
	s16b afraid;		/* Timed -- Fear */
	s16b image;			/* Timed -- Hallucination */
	s16b poisoned;		/* Timed -- Poisoned */
	s16b cut;			/* Timed -- Cut */
	s16b stun;			/* Timed -- Stun */

        s16b protevil;          /* Timed -- Protection from Evil*/
        s16b protgood;          /* Timed -- Protection from Good*/
        s16b protundead;        /* Timed -- Protection from Undead*/
	s16b invuln;		/* Timed -- Invulnerable */
	s16b hero;			/* Timed -- Heroism */
	s16b shero;			/* Timed -- Super Heroism */
	s16b shield;		/* Timed -- Shield Spell */
        s16b shield_power;      /* Timed -- Shield Spell Power */
	s16b blessed;		/* Timed -- Blessed */
	s16b tim_invis;		/* Timed -- See Invisible */
	s16b tim_infra;		/* Timed -- Infra Vision */

	s16b oppose_acid;	/* Timed -- oppose acid */
	s16b oppose_elec;	/* Timed -- oppose lightning */
	s16b oppose_fire;	/* Timed -- oppose heat */
	s16b oppose_cold;	/* Timed -- oppose cold */
	s16b oppose_pois;	/* Timed -- oppose poison */
        s16b oppose_ld;         /* Timed -- oppose light & dark */
        s16b oppose_cc;         /* Timed -- oppose chaos & confusion */
        s16b oppose_ss;         /* Timed -- oppose sound & shards */
        s16b oppose_nex;        /* Timed -- oppose nexus */


        s16b tim_esp;       /* Timed ESP */
        s16b wraith_form;   /* Timed wraithform */
        s16b tim_ffall;     /* Timed Levitation */
        s16b tim_fire_aura; /* Timed Fire Aura */

        s16b resist_magic;  /* Timed Resist Magic (later) */
        s16b tim_invisible; /* Timed Invisibility */
        s16b tim_inv_pow;   /* Power of timed invisibility */
        s16b tim_mimic;     /* Timed Mimic */
        s16b tim_lite;      /* Timed Lite */
        s16b holy;          /* Holy Aura */
        s16b walk_water;    /* Walk over water as a god */
        s16b tim_mental_barrier; /* Sustain Int&Wis */
        s16b strike;        /* True Strike(+25 hit) */
        s16b meditation;    /* Meditation(+50 mana -25 to hit/to dam) */
        s16b tim_reflect;   /* Timed Reflection */
        s16b tim_res_time;  /* Timed Resistance to Time */

        s16b immov_cntr;    /* Timed -- Last ``immovable'' command. */

        s16b chaos_patron;
        u32b muta1;
        u32b muta2;
        u32b muta3;

        s16b recall_dungeon;    /* Recall in which dungeon */
	s16b word_recall;	/* Word of recall counter */

        s32b energy;            /* Current energy */

	s16b food;			/* Current nutrition */

	byte confusing;		/* Glowing hands */
	byte searching;		/* Currently searching */

	s16b new_spells;	/* Number of spells available */

	s16b old_spells;

        s16b xtra_spells;       /* Number of xtra spell learned(via potion) */

	bool old_cumber_armor;
	bool old_cumber_glove;
	bool old_heavy_wield;
	bool old_heavy_shoot;
	bool old_icky_wield;

	s16b old_lite;		/* Old radius of lite (if any) */
	s16b old_view;		/* Old radius of view (if any) */

	s16b old_food_aux;	/* Old value of food */


	bool cumber_armor;	/* Mana draining armor */
	bool cumber_glove;	/* Mana draining gloves */
	bool heavy_wield;	/* Heavy weapon */
	bool heavy_shoot;	/* Heavy shooter */
	bool icky_wield;	/* Icky weapon */
        bool immovable;         /* Immovable character */

	s16b cur_lite;		/* Radius of lite (if any) */


	u32b notice;		/* Special Updates (bit flags) */
	u32b update;		/* Pending Updates (bit flags) */
	u32b redraw;		/* Normal Redraws (bit flags) */
	u32b window;		/* Window Redraws (bit flags) */

	s16b stat_use[6];	/* Current modified stats */
	s16b stat_top[6];	/* Maximal modified stats */

	s16b stat_add[6];	/* Modifiers to stat values */
	s16b stat_ind[6];	/* Indexes into stat tables */
	s16b stat_cnt[6];	/* Counter for temporary drains */
	s16b stat_los[6];	/* Amount of temporary drains */

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
        bool resist_continuum;  /* Resist space-time continuum disruption */

        bool sensible_fire;     /* Fire does more damage on the player */
/* Not used and NOT coded now */
/*        bool sensible_cold;*/   /* Cold does more damage on the player */
/*        bool sensible_elec;*/   /* Lightning does more damage on the player */
/*        bool sensible_acid;*/   /* Acid does more damage on the player */

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

        bool climb;             /* Can climb mountains */
        bool fly;               /* Can fly over some features */
        bool ffall;             /* No damage falling */
        bool lite;              /* Permanent light */
	bool free_act;		/* Never paralyzed */
	bool see_inv;		/* Can see invisible */
	bool regenerate;	/* Regenerate hit pts */
	bool hold_life;		/* Resist life draining */
        u32b telepathy;         /* Telepathy */
	bool slow_digest;	/* Slower digestion */
	bool bless_blade;	/* Blessed blade */
	bool xtra_might;	/* Extra might bow */
	bool impact;		/* Earthquake blows */

        s16b invis;             /* Invisibility */

	s16b dis_to_h;		/* Known bonus to hit */
	s16b dis_to_d;		/* Known bonus to dam */
	s16b dis_to_a;		/* Known bonus to ac */

	s16b dis_ac;		/* Known base ac */

        s16b to_m;                      /* Bonus to mana */
        s16b to_s;                      /* Bonus to spell */
	s16b to_h;			/* Bonus to hit */
	s16b to_d;			/* Bonus to dam */
	s16b to_a;			/* Bonus to ac */

	s16b ac;			/* Base ac */

        byte antimagic;         /* Power of the anti magic field */
        byte antimagic_dis;     /* Radius of the anti magic field */

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

        u32b class_extra1;      /* Variable for classe */
        u32b class_extra2;      /* Variable for classe */
        u32b class_extra3;      /* Variable for classe */
        u32b class_extra4;      /* Variable for classe */
        u32b class_extra5;      /* Variable for classe */
        u32b class_extra6;      /* Variable for classe */
        u32b class_extra7;      /* Variable for classe */

        u32b race_extra1;       /* Variable for race */
        u32b race_extra2;       /* Variable for race */
        u32b race_extra3;       /* Variable for race */
        u32b race_extra4;       /* Variable for race */
        u32b race_extra5;       /* Variable for race */
        u32b race_extra6;       /* Variable for race */
        u32b race_extra7;       /* Variable for race */

        cptr mimic_name;

        byte music;             /* Current music */

        char tactic;                  /* from 128-4 extremely coward to */
                                      /* 128+4 berserker */
        char movement;                /* base movement way */

        bool no_mortal;         /* Fated to never die by the hand of a mortal being */

        bool black_breath;      /* The Tolkien's Black Breath */

        bool precognition;      /* Like the cheat mode */

	/*** Pet commands ***/
	byte pet_follow_distance; /* Length of the imaginary "leash" for pets */
	byte pet_open_doors;      /* flag - allow pets to open doors */
	byte pet_pickup_items;    /* flag - allow pets to pickup items */

        /*** Body changing variables ***/
        u16b body_monster;        /* In which body is the player */
        bool disembodied;         /* Is the player in a body ? */
        byte body_parts[INVEN_TOTAL - INVEN_WIELD]; /* Which body parts does he have ? */

	/*** Temporary fields ***/

        bool leaving;                   /* True if player is leaving */
};


/* For Monk martial arts */

typedef struct martial_arts martial_arts;

struct martial_arts
{
    cptr    desc;    /* A verbose attack description */
    int     min_level;  /* Minimum level to use */
    int     chance;     /* Chance of 'success' */
    int     dd;        /* Damage dice */
    int     ds;        /* Damage sides */
    int     effect;     /* Special effects */
};



/* Powers - used by Mindcrafters and Necromancers */
typedef struct magic_power magic_power;
struct magic_power
{
	int     min_lev;
	int     mana_cost;
	int     fail;
	cptr    name;
};

/* Border */
typedef struct border_type border_type;
struct border_type 
{
	byte 	north[MAX_WID];
	byte 	south[MAX_WID];
	byte 	east[MAX_HGT];
	byte 	west[MAX_HGT];
	byte	north_west;
	byte	north_east;
	byte	south_west;
	byte	south_east;
};


/*
 * A structure describing a wilderness area
 * with a terrain, a town or a dungeon entrance
 */
typedef struct wilderness_type_info wilderness_type_info;
struct wilderness_type_info
{
        u32b    name;                   /* Name (offset) */
        u32b    text;                   /* Text (offset) */
        u16b    entrance;               /* Which town is there(<1000 i's a town, >=1000 it a dungeon) */
        byte    road;                   /* Flags of road */
        int     level;                  /* Difficulty level */
        s32b    flags1;                 /* Some flags */
        byte    feat;                   /* The feature of f_info.txt that is used to allow passing, ... and to get a char/color/graph */
        byte    terrain_idx;            /* Terrain index(defined in defines.h) */

        byte    terrain[MAX_WILD_TERRAIN];/* Feature types for the plasma generator */
};

/*
 * A structure describing a wilderness map
 */
typedef struct wilderness_map wilderness_map;
struct wilderness_map
{
        int     feat;                   /* Wilderness feature */
        u32b    seed;                   /* Seed for the RNG */
        u16b    entrance;               /* Entrance for dungeons */

        bool    known;                  /* Is it seen by the player ? */
};

/*
 * A structure describing a town with
 * stores and buildings
 */
typedef struct town_type town_type;
struct town_type
{
	cptr name;
	u32b seed; /* Seed for RNG */
        store_type *store;      /* The stores [max_st_idx] */
	byte numstores;

        bool real;              /* Is the town actualy defined ? */
};

/* Alchemists */
typedef struct alchemist_recipe_ego alchemist_recipe_ego;
struct alchemist_recipe_ego
{
        int     ego,which,enchant,ego_num;
};

typedef struct alchemist_recipe_item alchemist_recipe_item;
struct alchemist_recipe_item
{
        int     ctval,csval,num;
        int     etval,esval;
};

typedef struct alchemist_recipe alchemist_recipe;
struct alchemist_recipe
{
        int     sval_baterie;
        alchemist_recipe_ego ego[9];

        alchemist_recipe_item item[9];
};

/* A structure for deity information. */
typedef struct deity deity;
struct deity {
  cptr name;
  cptr god_of;
  byte grace_deduction;
  byte rarity;
  byte race1, race2;
};

/* A structure for tactics */
typedef struct tactic_info_type tactic_info_type;
struct tactic_info_type {
  s16b to_hit;
  s16b to_dam;
  s16b to_ac;
  s16b to_stealth;
  s16b to_disarm;
  s16b to_saving;
  cptr name;
};

/* A structure to describe a random artifact. */
typedef struct random_artifact random_artifact;
struct random_artifact {
  char name_full[80];     /* Full name for the artifact */
  char name_short[80];    /* Un-Id'd name */
  byte level;             /* Level of the artifact */
  byte attr;              /* Color that is used on the screen */
  u32b cost;              /* Object's value */
  byte activation;        /* Activation. */
  s16b timeout;           /* Timeout. */
  byte generated;         /* Does it exist already? */
};

/* A structure to describe an activation. */
typedef struct activation activation;
struct activation {
  char desc[80];          /* Desc of the activation */
  u32b cost;              /* costs value */
  s16b spell;             /* Spell. */
};

/* A structure to describe a music. */
typedef struct music music;
struct music {
  char desc[80];          /* Desc of the music */
  s16b music;             /* Music. */
  s16b dur;               /* Duration(if any) */
  s16b init_recharge;     /* Minimal recharge time */
  s16b turn_recharge;     /* Recharge time for each more turn */
  byte min_inst;          /* Minimum instrument for the music */
  byte rarity;            /* Rarity of the music(use 100 to unallow to be randomly generated) */
};

/* A structure to describe the random spells of the Power Mages */
typedef struct random_spell random_spell;
struct random_spell {
  char desc[30];          /* Desc of the spell */
  char name[30];          /* Name of the spell */
  s16b mana;              /* Mana cost */
  s16b fail;              /* Failure rate */
  u32b proj_flags;        /* Project function flags */
  byte GF;                /* Type of the projection */
  byte radius;
  byte dam_sides;
  byte dam_dice;
  byte level;             /* Level needed */
  bool untried;           /* Is the spell was tried? */
};

/* A structure to describe the fate of the player */
typedef struct fate fate;
struct fate {
        byte fate;      /* Which fate */
        byte level;     /* On which level */
        byte serious;   /* Is it sure? */
        s16b o_idx;     /* Object to find */
        s16b e_idx;     /* Ego-Item to find */
        s16b a_idx;     /* Artifact to find */
        s16b v_idx;     /* Vault to find */
        s16b r_idx;     /* Monster to find */
        s16b count;     /* Number of things */
        s16b time;      /* Turn before */
        bool know;      /* Has it been predicted? */
};

/* A structure for movements */
typedef struct move_info_type move_info_type;
struct move_info_type {
   s16b to_speed;
   s16b to_search;
   s16b to_stealth;
   s16b to_percep;
   cptr name;
};

/* A structure for the != dungeon types */
typedef struct dungeon_info_type dungeon_info_type;
struct dungeon_info_type {
        u32b name;                      /* Name */
        u32b text;                      /* Description */
        s16b floor1;                    /* Floor tile 1 */
        byte floor_percent1;            /* Chance of type 1 */
        s16b floor2;                    /* Floor tile 2 */
        byte floor_percent2;            /* Chance of type 2 */
        s16b floor3;                    /* Floor tile 3 */
        byte floor_percent3;            /* Chance of type 3 */
        s16b outer_wall;                /* Outer wall tile */
        s16b inner_wall;                /* Inner wall tile */
        s16b fill_type1;                /* Cave tile 1 */
        byte fill_percent1;             /* Chance of type 1 */
        s16b fill_type2;                /* Cave tile 2 */
        byte fill_percent2;             /* Chance of type 2 */
        s16b fill_type3;                /* Cave tile 3 */
        byte fill_percent3;             /* Chance of type 3 */
        s16b mindepth;                  /* Minimal depth */
        s16b maxdepth;                  /* Maximal depth */
        bool principal;                 /* If it's a part of the main dungeon */
        byte next;                      /* The next part of the main dungeon */
        byte min_plev;                  /* Minimal plev needed to enter -- it's an anti-cheating mesure */
        byte mode;                      /* Mode of combinaison of the monster flags */

        int min_m_alloc_level;          /* Minimal number of monsters per level */
        int max_m_alloc_chance;         /* There is a 1/max_m_alloc_chance chance per round of creating a new monster */

        s32b flags1;                    /* Flags 1 */

        s32b mflags1;                   /* The monster flags that are allowed */
        s32b mflags2;
        s32b mflags3;
        s32b mflags4;
        s32b mflags5;
        s32b mflags6;
        s32b mflags7;
        s32b mflags8;
        s32b mflags9;

        char r_char[5];                 /* Monster race allowed */
        int final_artifact;             /* The artifact you'll find at the bottom */
        int final_guardian;             /* The artifact's guardian. If an artifact is specified, then it's NEEDED */

        int ix, iy, ox, oy;             /* Wilderness coordinates of the entrance/output of the dungeon */

        byte special_percent;           /* % of monsters affected by the flags/races allowed, to add some variety */

        obj_theme objs;                 /* The drops type */
};

/* A structure for inscriptions */
typedef struct inscription_info_type inscription_info_type;
struct inscription_info_type {
        char text[40];                  /* The inscription itself */
        byte when;                      /* When it is executed */
        bool know;                      /* Is the inscription know ? */
        byte mana;                      /* Grid mana needed */
};
