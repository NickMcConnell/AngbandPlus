/* File: types.h */

/*
 * Global structures used in the game.  Monster, object, artifact,
 * character, etc.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
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
 * variables, facilitate structured program design, allow the use of ASCII
 * template files, simplify access to indexed data, or facilitate efficient
 * clearing of many variables at once.
 *
 * Note that certain data is saved in multiple places for efficient access,
 * and when modifying the data in one place it must also be modified in the
 * other places, to prevent the creation of inconsistent data.
 */


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
 * An array of DUNGEON_WID_MAX byte's
 */
typedef byte byte_wid[DUNGEON_WID_MAX];

/*
 * An array of DUNGEON_WID_MAX s16b's
 */
typedef s16b s16b_wid[DUNGEON_WID_MAX];

/**** Available Structs ****/


typedef struct maxima maxima;
typedef struct feature_type feature_type;
typedef struct trap_kind trap_kind;
typedef struct trap_type trap_type;
typedef struct object_kind object_kind;
typedef struct object_type object_type;
typedef struct artifact_type artifact_type;
typedef struct ego_item_type ego_item_type;
typedef struct tval_desc tval_desc;
typedef struct flag_data flag_data;
typedef struct set_element set_element;
typedef struct set_type set_type;
typedef struct monster_blow monster_blow;
typedef struct monster_race monster_race;
typedef struct monster_lore monster_lore;
typedef struct monster_type monster_type;
typedef struct vault_type vault_type;
typedef struct effect_grid_type effect_grid_type;
typedef struct effect_type effect_type;
typedef struct alloc_entry alloc_entry;
typedef struct quest_type quest_type;
typedef struct quest_memory_type quest_memory_type;
typedef struct owner_type owner_type;
typedef struct stock_type stock_type;
typedef struct store_type store_type;
typedef struct magic_type magic_type;
typedef struct player_magic player_magic;
typedef struct skill_type skill_type;
typedef struct skill_data skill_data;
typedef struct talent_type talent_type;
typedef struct talent_data talent_data;
typedef struct player_sex player_sex;
typedef struct player_race player_race;
typedef struct player_other player_other;
typedef struct trap_set_type trap_set_type;
typedef struct player_type player_type;
typedef struct color_type color_type;
typedef struct flavor_type flavor_type;
typedef struct move_moment_type move_moment_type;
typedef struct proj_graphics_type proj_graphics_type;
typedef struct graphics_data_type graphics_data_type;
typedef struct history_info history_info;

/**** Available structs ****/


/*
 * Information about maximal indices of certain arrays.
 *
 * These values may be adjusted by editing the file "limits.txt".
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

	u16b q_max;		/* Max size for "q_info[]" */

	u16b x_max;		/* Max size for "x_info[]" */
	u16b t_max;		/* Max size for "t_info[]" */

	u16b o_max;		/* Max size for "o_list[]" */
	u16b m_max;		/* Max size for "m_list[]" */

	u16b flavor_max; /* Max size for "flavor_info[]" */
};



/*
 * Information about terrain "features"
 */
struct feature_type
{
	u16b name;                  /* Name (offset) */
	u32b text;                  /* Text (offset) */

	byte mimic;                 /* Feature to mimic */
	byte priority;              /* Mini-map priority */

	u32b flags;                 /* Bitflags */

	byte d_attr;                /* Object "attribute" */
	char d_char;                /* Object "symbol" */

	byte x_attr;                /* The desired attr for this feature */
	char x_char;                /* The desired char for this feature */

	byte x_attr_lit;            /* Attr when brightly lit */
	char x_char_lit;            /* Char when brightly lit */

	byte x_attr_dim;            /* Attr when in shadow */
	char x_char_dim;            /* Char when in shadow */
};


/*
 * A trap template.
 */
struct trap_kind
{
	byte d_attr;            /* Default trap attribute */
	char d_char;            /* Default trap character */

	byte x_attr;            /* Desired trap attribute */
	char x_char;            /* Desired trap character */

	byte min_depth;         /* Minimum depth */
	byte xtra;              /* Unused */

	u16b flags;             /* Special trap flags (all traps of this kind) */
	cptr name;
};

/*
 * An actual trap.
 */
struct trap_type
{
	byte t_idx;             /* Trap kind index */

	byte fy;                /* Location of trap */
	byte fx;

	byte xtra;

	s16b hold_o_idx;        /* Index of the first object held */
	u16b flags;             /* Special trap flags (only this particular trap) */
};


/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware", "tried", and "known_effect" are saved in the savefile
 */
struct object_kind
{
	u32b name;              /* Name (offset) */
	u32b text;              /* Text (offset) */

	byte tval;              /* Object type */
	byte sval;              /* Object sub type */

	s16b pval;              /* Object bonuses, etc. */
	u32b flags_pval;        /* Pval-dependant object flags */

	s16b to_h;              /* Bonus to hit */
	s16b to_d;              /* Bonus to damage */
	s16b to_a;              /* Bonus to armor */

	s16b ac;                /* Base armor */

	byte dd, ds;            /* Damage dice/sides */

	s16b weight;            /* Weight */

	s32b cost;              /* Object "base cost" */

	u32b flags1;            /* Flags, set 1 */
	u32b flags2;            /* Flags, set 2 */
	u32b flags3;            /* Flags, set 3 */

	byte level;             /* Level */

	byte locale[4];         /* Allocation level(s) */
	byte chance[4];         /* Allocation chance(s) */

	byte gen_mult_prob;     /* Probability of generating more than one */
	byte gen_dice;          /* Average number to generate - dice rolled */
	byte gen_side;          /* Average number to generate - dice sides */

	byte e_type[4];         /* Essence cost (type) */
	byte e_num[4];          /* Essence cost (quantity) */

	byte activate;          /* Activation */

	byte d_attr;            /* Default object attribute */
	char d_char;            /* Default object character */

	byte x_attr;            /* Desired object attribute */
	char x_char;            /* Desired object character */

	s16b flavor;            /* Special object flavor (or zero) */

	u16b xtra;              /* Random object flags */

	byte special;           /* Special object bit flags */
};



/*
 * Object information, for a specific object (or stack of objects).
 *
 * Note that a "discount" on an item is permanent and never goes away.
 *
 * Objects may have up to three pvals.  The first, called simply "pval"
 * may control just about anything from food value to maximum timeout, or
 * govern any pval-dependant flag.  The second and third pval are far
 * less often used; they come into play if some pval-dependant qualities
 * have different values than others.
 *
 * Note that inscriptions are now handled via the "quark_str()" function
 * applied to the "note" field, which will return NULL if "note" is zero.
 *
 * Each cave grid points to one (or zero) objects via the "o_idx"
 * field (above).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a stack of objects in the same grid.
 *
 * Each monster points to one (or zero) objects via the "hold_o_idx"
 * field (below).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a stack of objects held by the monster.
 *
 * The "held_m_idx" field is used to indicate which monster or trap,
 * if any, is holding the object.  Objects being held have "ix=0" and
 * "iy=0".
 */
struct object_type
{
	s16b k_idx;               /* Kind index (zero if "dead") */

	byte iy;                  /* Y-position on map, or zero */
	byte ix;                  /* X-position on map, or zero */

	byte tval;                /* Item type (from kind) */
	byte sval;                /* Item sub-type (from kind) */

	s16b pval;                /* Special numeric value # 1 */
	u32b flags_pval1;         /* Flags affected by this value */

	s16b pval2;               /* Special numeric value # 2 */
	u32b flags_pval2;         /* Flags affected by this value */

	s16b pval3;               /* Special numeric value # 3 */
	u32b flags_pval3;         /* Flags affected by this value */

	u32b flags1;              /* Non pval-dependant flags, set 1 */
	u32b flags2;              /* Non pval-dependant flags, set 2 */
	u32b flags3;              /* Non pval-dependant flags, set 3 */

	s16b to_h;                /* Skill Bonus */
	s16b to_d;                /* Deadliness Bonus */
	s16b to_a;                /* Armor Bonus */

	s16b ac;                  /* Normal AC */

	byte dd, ds;              /* Damage dice/sides (dice are also damage mult) */

	s16b weight;              /* Item weight */
	byte number;              /* Number of items */

	byte artifact_index;      /* Index of artifact template, if any */
	byte ego_item_index;      /* Index of ego_item template, if any */

	s16b timeout;             /* Timeout counter (for recharging objects) */
	byte activate;            /* Activation */

	s32b b_cost;              /* Base cost */
	byte cost_adjust;         /* Cost Adjustment */

	byte ident;               /* Special identification flags  */
	byte marked;              /* Object is memorized for map display purposes */

	u16b note;                /* User-created inscription index */
	byte inscrip;             /* Special game-generated inscription index */

	s16b next_o_idx;          /* Next object in stack (if any) */
	s16b held_m_idx;          /* Monster (or trap) holding us  (if any) */

	s16b drop_depth;           /* Depth where item dropped */

	byte quivered;             /* Has item been added to quiver? */
};



/*
 * Information contained in an "artifact" template.  All values for
 * artifacts override all other information about an object.
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */
struct artifact_type
{
	u16b name;                /* Name (offset) */
	u32b text;                /* Text (offset) */

	byte level;               /* Artifact level */
	byte rarity;              /* Artifact rarity */

	byte tval;                /* Artifact type */
	byte sval;                /* Artifact sub type */

	s16b to_h;                /* Skill Bonus */
	s16b to_d;                /* Deadliness Bonus */
	s16b to_a;                /* Armor Bonus */

	s16b ac;                  /* Base armor */

	byte dd, ds;              /* Damage dice */

	s16b weight;              /* Weight */

	s32b cost;                /* Artifact cost (exact price) */

	s16b pval1;               /* Special numeric value # 1 */
	u32b flags_pval1;         /* Artifact flags affected by this value */
	s16b pval2;               /* Special numeric value # 2 */
	u32b flags_pval2;         /* Artifact flags affected by this value */
	s16b pval3;               /* Special numeric value # 3 */
	u32b flags_pval3;         /* Artifact flags affected by this value */

	u32b flags1;              /* Artifact flags, set 1 */
	u32b flags2;              /* Artifact flags, set 2 */
	u32b flags3;              /* Artifact flags, set 3 */

	u16b xtra;                /* Random attribute flags */

	byte cur_num;             /* Number created (0 or 1) */
	byte max_num;             /* Number of items comprising the artifact */
	byte lost;                /* Artifact is forever lost */
	byte activate;            /* Artifact activation */

	byte set_index;           /* Set to which this artifact belongs */
	byte set_bonus;           /* Is the set bonus currently applied? */
};


/*
 * Information contained in an "ego-item" template.
 */
struct ego_item_type
{
	u32b name;                /* Name (offset) */
	u32b text;                /* Text (offset) */

	byte level;               /* Minimum level */
	byte rarity;              /* Ego-item rarity */
	byte rating;              /* Rating boost */

	s32b cost;                /* Ego-item bonus to object value */

	s16b mod_to_h;            /* Maximum Skill bonus */
	s16b mod_to_d;            /* Maximum Deadliness bonus */
	s16b mod_to_a;            /* Maximum armor bonus */

	byte activate;            /* Activation */

	byte tval[EGO_TVALS_MAX];      /* Legal tval */
	byte min_sval[EGO_TVALS_MAX];  /* Minimum legal sval */
	byte max_sval[EGO_TVALS_MAX];  /* Maximum legal sval */

	s16b max_pval1;           /* Special numeric value # 1 */
	u32b flags_pval1;         /* Ego-item flags affected by this value */
	s16b max_pval2;           /* Special numeric value # 2 */
	u32b flags_pval2;         /* Ego-item flags affected by this value */

	u32b flags1;              /* Ego-item flags, set 1 */
	u32b flags2;              /* Ego-item flags, set 2 */
	u32b flags3;              /* Ego-item flags, set 3 */

	u16b xtra;                /* Random attribute flags */
};


/*
 * A structure to hold a tval, its description and its possibility
 * for becoming an artifact.
 */
struct tval_desc
{
	byte tval;
	cptr desc;
	byte can_be_artifact;
};



/*
 * Creation-related data for object flags.
 *
 * Essence type (sval) that it uses and generates.
 * Minimum cost in essences.  If flag is pval-dependant, then cost is
 *   when asking for a pval of 10.
 * Probability of essence generation:  100 is normal.
 * Description of flag.
 */
struct flag_data
{
	byte essence_sval;
	byte cost;
	s16b prob;
	cptr desc;
};



/* Item sets */

/* Information about an item in a set  -GS- */
struct set_element
{
	byte a_idx;               /* Artifact Index */

	s16b to_h;                /* Skill Bonus */
	s16b to_d;                /* Deadliness Bonus */
	s16b to_a;                /* Armor Bonus */

	s16b ac;                  /* Base armor */
	byte dd, ds;              /* Damage dice */

	s16b pval1;               /* Special numeric value # 1 */
	u32b flags_pval1;         /* Artifact flags affected by this value */
	s16b pval2;               /* Special numeric value # 2 */
	u32b flags_pval2;         /* Artifact flags affected by this value */
	s16b pval3;               /* Special numeric value # 3 */
	u32b flags_pval3;         /* Artifact flags affected by this value */

	u32b flags1;              /* Artifact flags, set 1 */
	u32b flags2;              /* Artifact flags, set 2 */
	u32b flags3;              /* Artifact flags, set 3 */
};

/* Information about artifact sets  -GS- */
struct set_type
{
	cptr name;                 /* Name of the set */
	byte no_of_items;          /* The number of items in the set */
	cptr set_desc;             /* Description of set */
	set_element set_items[2];  /* Artifact index and extra powers. */
};



/*
 * Monster blow structure
 *
 *   - Method (RBM_*)
 *   - Effect (RBE_*)
 *   - Damage Dice
 *   - Damage Sides
 */
struct monster_blow
{
	byte method;
	byte effect;
	byte d_dice;
	byte d_side;
};

/*
 * Monster "race" information
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
 */
struct monster_race
{
	u16b name;					/* Name (offset) */
	u32b text;					/* Text (offset) */

	u16b hitpoints;         /* Monster hitpoints */

	byte ac;						/* Armor Class */

	s16b sleep;					/* Inactive counter (base) */
	byte aaf;					/* Area affect radius (1-100) */
	byte noise;					/* How noisy the monster is */

	byte speed;					/* Speed (normally 110) */

	s16b mexp;					/* Exp value for kill */

	byte freq_ranged;			/* Ranged attack frequency */

	byte mana;					/* Max mana */
	byte spell_power;			/* Power of (damage-dealing) spells */
	byte combat_range;      /* Combat range - how the monster fights */

	u32b flags1;			/* Flags 1 (general) */
	u32b flags2;			/* Flags 2 (abilities) */
	u32b flags3;			/* Flags 3 (race/resist) */
	u32b flags4;			/* Flags 4 (innate/breath) */
	u32b flags5;			/* Flags 5 (normal spells) */
	u32b flags6;			/* Flags 6 (special spells) */
	u32b flags7;			/* Flags 7 (summon spells) */

	monster_blow blow[MONSTER_BLOW_MAX]; /* Monster blows */

	byte level;					/* Level of creature */
	byte rarity;				/* Rarity of creature */

	byte d_attr;				/* Default monster attribute */
	char d_char;				/* Default monster character */

	byte x_attr;				/* Desired monster attribute */
	char x_char;				/* Desired monster character */

	byte max_num;				/* Maximum population allowed per level */
	byte cur_num;				/* Monster population on current level */
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
	s16b sights;				/* Count sightings of this monster */
	s16b deaths;				/* Count deaths from this monster */

	s16b pkills;				/* Count monsters killed in this life */
	s16b tkills;				/* Count monsters killed in all lives */

	byte wake;				/* Number of times woken up (approximate) */
	byte ignore;				/* Number of times ignored (approximate) */

	byte flags;				/* Special monster lore flags */
	byte xtra2;				/* Something (unused) */

	byte drop_gold;			/* Max number of gold dropped at once */
	byte drop_item;			/* Max number of item dropped at once */

	byte xtra3;				/* Something (unused) */
	byte ranged;			/* Observed ranged attack frequency */

	byte blows[MONSTER_BLOW_MAX];  /* Number of times each blow type was seen */

	u32b flags1;				/* Observed racial flags */
	u32b flags2;				/* Observed racial flags */
	u32b flags3;				/* Observed racial flags */
	u32b flags4;				/* Observed racial flags */
	u32b flags5;				/* Observed racial flags */
	u32b flags6;				/* Observed racial flags */
	u32b flags7;				/* Observed racial flags */
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
	s16b r_idx;					/* Monster race index */
	byte fy;					/* Y location on map */
	byte fx;					/* X location on map */

	s16b hp;					/* Current Hit points */
	s16b maxhp;					/* Max Hit points */

	byte csleep;                /* Inactive counter */
	byte mspeed;                /* Monster "speed" */
	byte energy;                /* Monster "energy" */
	byte mana;                  /* Current mana level */

	byte stunned;				/* Monster is stunned */
	byte confused;				/* Monster is confused */
	byte monfear;				/* Monster is afraid (or panicking) */
	byte slowed;				/* Monster is slowed down */

	byte hasted;				/* Monster is hasted */
	byte harass;                /* Cast harassment spells early */
	s16b hold_o_idx;			/* Object being held (if any) */

	byte cdis;					/* Current distance from character  */
	u16b mflag;					/* Extra monster flags */
	byte ml;					/* Monster is visible or partially visible */

	u32b smart;				/* Field for "smart_learn" */

	byte ty;				/* Monster target */
	byte tx;
	byte min_range;		/* What is the closest we want to be? */  /* Not saved */
	byte best_range;		/* How close do we want to be? */  /* Not saved */
};

/*
 * Information about "vault generation"
 */
struct vault_type
{
	u16b name;					/* Name (offset) */
	u32b text;					/* Text (offset) */

	byte typ;					/* Vault type */

	byte rat;					/* Vault rating */

	byte hgt;					/* Vault height */
	byte wid;					/* Vault width */

	byte min_lev;		/* Minimum allowable level, if specified. */
	byte max_lev;		/* Maximum allowable level, if specified. */
};


/*
 * Information about grids within a lingering effect.
 */
struct effect_grid_type
{
	byte y;        /* Y-coordinate */
	byte x;        /* X-coordinate */
	byte x_idx;    /* Effect index */
	byte xtra;     /* Bitflags or other information */
};


/*
 * Information about effects.
 *
 *   The "index" is used to trigger any effect-type-specific code.  It is
 *      /not/ the same as the order of this effect in the effect array.
 *      The index need not be unique - you can have multiple "wall of fire"
 *      spells going at the same time.
 *   The "type" can be either the index of the projection type used in
 *      this effect, or a terrain type that this effect temporarily
 *      creates, or even correspond to multiple projection types and/or
 *      terrains.
 *   "y0" and "x0" are the coordinates of the source grid.
 *   The "time_count" is how many game turns have elapsed since this
 *      effect was last processed.  When it reaches zero, the effect is
 *      processed again, the "age" is incremented by one, and the "time_
 *      count" reset to the value stored in "time_delay".
 *   The "age" is how old this effect is; special functions use this
 *      variable to make the effect do different things over time,
 *      including "dying".
 *   The "angle_dir" is the angular direction from the source of the effect
 *      to the target grid, expressed in degrees divided by 2.  See the
 *      table "get_angle_to_grid".
 *   The "power" is how strong this effect is.  This is usually, but
 *      not always, the same as base damage.
 *   The "power2" is often used for radius or effectiveness.
 *   The "flags" store bitflags corresponding to effect state; they serve
 *      (with the "age") as the effect memory.  Unlike monster or object
 *      bitflags, these need not have fixed meanings.
 */
struct effect_type
{
	byte index;             /* Numerical index or classifier */
	byte type;              /* Projection (or terrain) type */

	byte y0;                /* Source location */
	byte x0;

	s16b y1;                /* Target location -- needs to be */
	s16b x1;                /* negative sometimes */

	char time_count;        /* Game turns until effect takes its turn */
	char time_delay;        /* Number of game turns between effect turns */

	byte age;               /* Number of turns effect has lasted */
	byte lifespan;          /* Number of turns effect can last */

	s16b power;             /* Strength of effect */
	s16b power2;

	byte practice_skill;    /* Skill this effect practices when it hurts or
	                           kills monsters */
	byte pval;              /* Various things (may need to become "who") */

	u16b flags;             /* Effect "memory" bitflags */
};

/*
 * An entry for the monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */
struct alloc_entry
{
	s16b index;					/* The actual index */

	byte level;					/* Base dungeon level */
	byte prob1;					/* Probability, pass 1 */
	byte prob2;					/* Probability, pass 2 */
	byte prob3;					/* Probability, pass 3 */
};


/*
 * Structure for the "quests"
 *
 * From EyAngband.
 */
struct quest_type
{
	u16b name;			/* Name (offset) */

	byte type;			/* Quest Type */
	byte reward;		/* Quest Reward */

	byte active_level;	/* Equals dungeon level if not completed, 0 if completed */
	byte base_level;	/* The dungeon level on which the quest was assigned */

	s16b r_idx;			/* Monster race */

	s16b cur_num;		/* Number killed */
	s16b max_num;		/* Number required */

	byte started;		/* Has the character started the quest? */
	byte slack;			/* How much "slack" we're giving before quest fail */
	byte diff;          /* Difficulty rating of quest */
	byte flags;         /* Quest flags */
};


/*
 * Structure for the quest history (current and previous quests)
 */
struct quest_memory_type
{
	byte type;			/* Quest Type */
	byte level;			/* Dungeon level */

	s16b r_idx;			/* Monster race */
	s16b max_num;		/* Number of monsters assigned */

	byte succeeded;	/* Did the character succeed in the quest? */
	byte extra;			/* Unused space */
};


/*
 * A store owner
 */
struct owner_type
{
	char owner_name[80];		/* Name */

	u16b max_cost;				/* Purse limit */

	byte max_inflate;			/* Inflation (max) */
	byte min_inflate;			/* Inflation (min) */

	byte haggle_per;			/* Haggle unit */

	byte insult_max;			/* Insult limit */

	byte owner_race;			/* Owner race */
};

/*
 * A store stock item
 */
struct stock_type
{
	s16b k_idx;

	byte prob;                /* Chance of item actually being in stock */
	byte num;                 /* Approximate number to keep on hand */
};


/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store_type
{
	byte owner;					/* Owner index */

	s16b insult_cur;			/* Insult counter */

	s16b good_buy;				/* Number of "good" buys */
	s16b bad_buy;				/* Number of "bad" buys */

	s32b store_open;			/* Closed until this turn */

	s32b total_buy;         /* Total purchases at this store */

	s16b stock_start;
	s16b stock_end;
	byte stock_num;				/* Stock -- Number of entries */
	s16b stock_size;			/* Stock -- Total Size of Array */
	object_type *stock;			/* Stock -- Actual stock items */
};




/*
 * Spell information.  Index controls effects and name; position in
 * spellbook is controlled by values in the array "book_start_index".
 */
struct magic_type
{
	byte index;         /* The internal spell index */
	byte slevel;        /* Required level (to learn) */
	byte smana;         /* Required mana (to cast) */
	byte sfail;         /* Base chance of failure */
	byte sexp;          /* Encoded experience bonus */
	cptr sname;         /* Spell name */
};


/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */
struct player_magic
{
	cptr title;                /* Name of realm */
	byte spell_book;           /* Tval of spell books (if any) */
	byte spell_stat;           /* Primary stat (used for spells, if any) */
	byte spell_skill;          /* Skill that improves magic */
	s16b spell_weight;         /* Max armor weight that avoids mana penalties */
	byte spell_number;         /* Total available spells in that realm */
	byte book_start_index[11]; /* Index of 1st spell for all books */
	magic_type info[PY_MAX_SPELLS];       /* The available spells */
};

/*
 * Information about skills
 */

/*
 * General skill information.
 */
struct skill_type
{
	cptr name;            /* Name */
	cptr desc;            /* Brief description */
	s16b cost_adj;        /* Cost adjustment */
};

/*
 * Character-specific skills data
 */
struct skill_data
{
	u16b cur;					/* Current skill level */
	u16b max;					/* Maximum skill level */

	s32b practice_exp;      /* Amount of practice in the skill */
};


/*
 * General talent information
 */
struct talent_type
{
	cptr name;              /* Name of this talent */
	char index;             /* Index of talent (character used to access) */
	byte skill_count;       /* Number of skills for this talent */
	byte skill[5];          /* Skill(s) this talent is based on */

	byte min_level;         /* Value of skill needed to use */
	s16b timeout;           /* Time between uses of this talent */
	byte oath;              /* Required oath(s) */
	int type;               /* Type of talent (warrior or utility) */
	int form;               /* Associated shapechange form */
};

/*
 * Character-specific talent data
 */
struct talent_data
{
	byte marked;            /* Player wants updates about this talent */
	byte empty;             /* Available for later use */
	s16b count;             /* Time until you can use this talent again */
};



/*
 * Player sex info
 */
struct player_sex
{
	cptr title;					/* Type of sex */
	cptr pronoun;				/* Pronoun of sex */
	cptr winner;				/* Name of winner */
};


/*
 * Player racial info
 */
struct player_race
{
	cptr title;			/* Type of race */

	s16b r_adj[A_MAX];	/* Racial stat adjustments */

	s16b r_dis;			/* base disarming modifier */
	s16b r_dev;			/* base magic devices modifier */
	s16b r_sav;			/* base saving throw modifier */
	s16b r_stl;			/* base stealth modifier */
	s16b r_srh;			/* base perception ability modifier */
	s16b r_thn;			/* base combat (melee) modifier */
	s16b r_thb;			/* base combat (missile) modifier */
	s16b r_tht;			/* base combat (throwing) modifier */

	byte r_mhp;			/* Race hit-dice modifier */

	u16b b_age;			/* base age */
	u16b m_age;			/* mod age */

	byte m_b_ht;		/* base height (males) */
	byte m_m_ht;		/* mod height (males) */
	u16b m_b_wt;		/* base weight (males) */
	u16b m_m_wt;		/* mod weight (males) */

	byte f_b_ht;		/* base height (females) */
	byte f_m_ht;		/* mod height (females)	  */
	u16b f_b_wt;		/* base weight (females) */
	u16b f_m_wt;		/* mod weight (females) */

	byte infra;			/* Infra-vision	range */

	u32b flags1;            /* Racial Flags, set 1 */
	u32b flags2;            /* Racial Flags, set 2 */
	u32b flags3;            /* Racial Flags, set 3 */
};



/*
 * Some more player information
 *
 * This information is retained across player lives
 */
struct player_other
{
	char full_name[32];			/* Full name */
	char base_name[32];			/* Base name */

	bool opt[OPT_MAX];			/* Options */

	u32b window_flag[TERM_MAX];		/* Window flags */

	byte hitpoint_warn;			/* Hitpoint warning (0 to 9) */

	byte delay_factor;			/* Delay factor (0 to 9) */
};

/*
 * Data about trap setting
 */
struct trap_set_type
{
	byte time;         /* Amount of time invested */
	byte y;            /* Location of work - row */
	byte x;            /* Location of work - col */
	byte tmp;          /* Not currently used */
};

/*
 * Most of the "player" information goes here.
 *
 * This structure gives us a large collection of player variables.
 *
 * This entire structure is wiped when a new character is born.
 *
 * This structure is more or less laid out so that the information
 * which must be saved in the savefile precedes all the information
 * which can be recomputed as needed.
 */
struct player_type
{
	s16b py;					/* Character location */
	s16b px;					/* Character location */

	byte psex;					/* Sex index */
	byte prace;					/* Race index */
	byte realm;					/* Magical realm */

	byte hitdie;				/* Hit dice (sides) */

	s16b age;					/* Character age */
	s16b ht;					/* Height */
	s16b wt;					/* Weight */
	s16b sc;					/* Social Class */

	s16b stat_birth[A_MAX];  /* Starting birth stats */
	s16b stat_max[A_MAX];    /* Maximum (undrained) intrinsic stats */
	s16b stat_cur[A_MAX];    /* Current intrinsic stats */

	byte power;					/* Character power (current) */
	s32b final_score;       /* Final score */

	s32b au;					/* Current Gold */
	s16b au_birth;			/* Starting Gold */

	s16b max_depth;			/* Max depth */
	s16b depth;					/* Cur depth */

	s32b exp;					/* Cur experience */
	u16b exp_frac;				/* Cur exp frac (in 1/1000ths of an exp point) */

	s16b mhp;					/* Max hit pts */
	s16b chp;					/* Cur hit pts */
	u16b chp_frac;				/* Cur hit frac (in 1/10000ths of a hit point) */

	s16b msp;					/* Max mana pts */
	s16b csp;					/* Cur mana pts */
	u16b csp_frac;				/* Cur mana frac (in 1/10000ths of a mana point) */

	s32b total_kills;       /* Total number of kills */

	s16b unsanctified;          /* Cursed by the Divine */
	s16b luck;					/* Luck */

	s16b fast;					/* Timed -- Fast */
	s16b slow;					/* Timed -- Slow */
	s16b blind;					/* Timed -- Blindness */
	s16b paralyzed;				/* Timed -- Paralysis */
	s16b confused;				/* Timed -- Confusion */
	s16b afraid;				/* Timed -- Fear */
	s16b image;					/* Timed -- Hallucination */
	s16b poisoned;				/* Timed -- Poisoned */
	s16b diseased;				/* Timed -- Diseased */
	s16b cut;					/* Timed -- Cut */
	s16b stun;					/* Timed -- Stun */

	s16b protevil;				/* Timed -- Protection */
	s16b bold;					/* Timed -- Resist fear */
	s16b hero;					/* Timed -- Heroism */
	s16b berserk;				/* Timed -- Berserkergang */
	s16b necro_rage;			/* Timed -- Necromantic rage */
	s16b shield;				/* Timed -- Shield Spell */
	s16b steelskin;			/* Timed -- Armored skin (same as shield) */
	s16b blessed;				/* Timed -- Blessed */
	s16b holy;					/* Timed -- Holy Aura */
	s16b tim_invis;			/* Timed -- Invisibility */
	s16b tim_inv_pow;			/* Power of timed invisibility */
	s16b tim_infra;			/* Timed -- Infra Vision */
	s16b detect_inv;			/* Timed -- See Invisible */
	s16b esp_evil;				/* Awareness of evil creatures */
	s16b tim_esp;				/* Timed -- full ESP */
	s16b regen_hp;				/* Timed -- Regenerate HP */
	s16b regen_mana;			/* Timed -- Regenerate Mana */
	s16b vitality;			/* Timed -- Extra recovery */
	s16b mania;				/* Timed -- Manic-depressive fits */
	s16b res_dam;				/* Timed -- Resistance to damage */
	s16b self_knowledge;		/* Timed -- Self Knowledge */

	s16b forbid_summoning;  /* Forbid most summoning */

	s16b oppose_acid;			/* Timed -- oppose acid */
	s16b oppose_elec;			/* Timed -- oppose lightning */
	s16b oppose_fire;			/* Timed -- oppose heat */
	s16b oppose_cold;			/* Timed -- oppose cold */
	s16b oppose_pois;			/* Timed -- oppose poison */
	s16b oppose_ethereal;   /* Timed -- oppose ethereal */
	s16b word_recall;			/* Word of recall counter */

	s16b wraithform;           /* Can go through walls */
	s16b form_dur;             /* Handle temporary forms -- note: no longer any dragonform or trollform variable */

	s16b dancing_feet;         /* Player blinks every turn */
	bool dancing_feet_safe;    /* Are these blinks safe or unsafe? */
	s16b phasing_foes;         /* Opponents blink during their turns */

	s16b blink_away;         	/* Try to blink away if hit */
	s16b evasion;              /* Your dodging skill increases greatly */

	s16b aura_cold;            /* Frosty aura */
	s16b aura_fire;            /* Fiery aura */
	s16b wiz_prot;             /* Wizardly protection */

	s16b mental_barrier;       /* Enhances the "Dominion" spell, saving throw */
	s16b pois_power;           /* Enhances the "Noxious Fumes" spell */
	s16b pois_power_dur;       /* Enhances the "Noxious Fumes" spell */
	s16b chaos_power;          /* Enhances the "Call Chaos" spell */
	s16b chaos_power_dur;      /* Enhances the "Call Chaos" spell */

	s16b nexus_field;           /* Player is surrounded by a nexus field */
	s16b nexus_field_strength;

	bool being_crushed;         /* Being crushed */


	trap_set_type trap_set;    /* Data about monster trap setting */

	s16b avg_dam;           /* Average recent damage */
	s16b avg_dam_offhand;   /* Average damage for the off-hand weapon */

	s16b avg_hit;           /* Average recent hit rate */
	s16b avg_hit_offhand;   /* Average hit rate for the off-hand weapon */

	s16b energy;			/* Current energy */

	s32b food;				/* Current nutrition */

	byte schange;			/* Current shapechange */
	byte schange_skill;     /* Which skill increases power of shapechange */
	byte schange_min_skill; /* The minimum skill required to use the skill */

	s16b soul_reserve;		/* Xp your weapon has stored */
	bool feed_weapon;			/* We can feed our weapon */

	u16b special_attack;			/* Special attack flags */

	s16b acid_attack;				/* Acid attack */
	s16b elec_attack;				/* Elec attack */
	s16b fire_attack;				/* Fire attack */
	s16b cold_attack;				/* Cold attack */
	s16b pois_attack;				/* Pois attack */

	s16b tim_weath;			/* Time until weather changes */
	s16b hold_weath;			/* Timed -- Hold Weather */

	s16b humid;				/* Current humidity */
	s16b wind;				/* Current windiness */
	s16b temp;				/* Current temperature */
	s16b humid_forecast;				/* Forecast humidity */
	s16b wind_forecast;				/* Forecast windiness */
	s16b temp_forecast;				/* Forecast temperature */


	byte barehand;			/* Skill we use when have no weapon */
	byte barehanded;		/* Temporary -- note that we are barehanded */
	byte lastadv;			/* Skill last advanced */

	skill_data pskills[NUM_SKILLS];	/* The player's skills */
	talent_data ptalents[NUM_TALENTS];	/* The player's talents */
	byte oath;				/* Commitments the character has made */

	byte essence[NUM_ESSENCE];		/* Stored essences */

	byte sneaking;			/* Currently sneaking */

	s16b base_wakeup_chance;	/* Base amount of character noise */

	/* Quests */
	bool special_quest;     /* In a special quest */
	s16b fame;              /* Character fame */
	s16b inn_name;          /* Variable controlling the name of the Inn */
	byte cur_quest;         /* Current quest */
	quest_memory_type quest_memory[MAX_QM_IDX];  /* Quest memory */

	byte spell_flags[PY_MAX_SPELLS]; /* Spell flags */

	s16b player_hp[PY_MAX_POWER];	/* HP Array */

	char died_from[80];		/* Cause of death */
	char history[250];		/* Initial history */

	s32b score;             /* Game score */
	u16b total_winner;		/* Total winner */
	u16b panic_save;			/* Panic save */

	u16b noscore;			/* Cheating flags */

	bool is_dead;			/* Player is dead */

	bool wizard;			/* Player is in wizard mode */

	s16b deaths;          /* Number of deaths */

	s16b create_stair;		/* Create a staircase on the next level */

	byte last_set_options_screen;  /* Last screen displayed */


	/*** Temporary fields ***/

	bool playing;			/* True if player is playing */

	bool leaving;			/* True if player is leaving */

	s16b wy;				/* Window panel - top */
	s16b wx;				/* Window panel - left */

	s16b total_weight;		/* Total weight being carried */

	s16b inven_cnt;			/* Number of items in inventory */
	s16b equip_cnt;			/* Number of items in equipment */
	s16b pack_size_reduce;	  /* Amount of space the quiver uses up */


	s16b target_set;			/* Target flag */
	s16b target_who;			/* Target identity */
	s16b target_row;			/* Target location */
	s16b target_col;			/* Target location */
	s16b max_dist;				/* Maximum projection distance with current weapon (fired or thrown) */

	s16b health_who;			/* Health bar trackee */

	s16b monster_race_idx;		/* Monster race trackee */

	s16b object_kind_idx;		/* Object kind trackee */

	s16b energy_use;			/* Energy use this turn */

	s16b resting;			/* Resting counter */
	s16b running;			/* Running counter */

	s32b resting_turns;     /* Number of turns spent resting */
	s32b total_turns;      /* Number of turns spent active */

	s16b run_cur_dir;			/* Direction we are running */
	s16b run_old_dir;			/* Direction we came from */
	bool run_unused;			/* Unused (padding field) */
	bool run_open_area;			/* Looking for an open area */
	bool run_break_right;		/* Looking for a break (right) */
	bool run_break_left;		/* Looking for a break (left) */

	byte move_dir;				/* Direction of movement (for adjusting panels) */

	u32b proj_mon_flags;		/* Type of monsters certain spells affect */
	u32b proj_temp_flags;   /* Temporary projection flags */
	u32b proj_temp_flags_cancel;   /* Temporary projection cancel flags */
	s16b came_hither;		/* Did the "come hither" spell work? */

	bool auto_pickup_okay;      /* Allow automatic pickup */

	s16b command_cmd;			/* Gives identity of current command */
	s16b command_arg;			/* Gives argument of current command */
	s16b command_rep;			/* Gives repetition of current command */
	s16b command_dir;			/* Gives direction of current command */
	bool using_keymap;      /* Using a keymap */

	s16b command_see;			/* See "cmd1.c" */
	s16b command_wrk;			/* See "cmd1.c" */

	s16b command_new;			/* Hack -- command chaining  (ignore disturbance) XXX XXX */

	s16b uncast_spells;			/* Available but uncast spells */
	s16b spell_level;			/* Maximum level of spell character can cast */

	bool cumber_armor;			/* Mana draining armor */
	bool cumber_glove;			/* Mana draining gloves */
	bool heavy_wield;			/* Heavy weapon */
	bool heavy_shoot;			/* Heavy shooter */
	bool icky_wield;			/* Icky weapon */
	bool shield_on_back;	/* Player carrying a shield on his back. -LM- */
	bool suppress_bottle;  /* Stop saving bottles and parchments */

	s16b cur_lite;				/* Radius of light (if any) */

	s16b extrahp;		/* Hit points to be added on for heroism, berserk */

	s32b food_bloated;			/* Bloated  when food is at this value */
	s32b food_full;				/* Full     when food is at this value */
	s32b food_hungry;				/* Hungry   when food is at this value */
	s32b food_weak;				/* Weak     when food is at this value */
	s32b food_fainting;			/* Fainting when food is at this value */
	s32b food_starving;			/* Starving when food is at this value */

	s32b birth_roll_requirement;   /* Number of times character was rolled up */

	s16b specialty;		/* Character skill specialty (for title and icon display) */

	byte suppress;				/* Stop auto-display of specific things */

	byte get_help_index;    /* Context-specific help */

	u32b notice;				/* Special Updates (bit flags) */
	u32b update;				/* Pending Updates (bit flags) */
	u32b redraw;				/* Normal Redraws (bit flags) */
	u32b window;				/* Window Redraws (bit flags) */

	u32b dungeon_flags;         /* Special "dungeon environment" conditions */

	byte character_type;        /* Type of player character (normal, ironman, etc.) */


	/*** Extracted fields ***/

	s16b stat_use[A_MAX];    /* Actual stats (intrinsic + modifiers) */
	s16b stat_ind[A_MAX];    /* Stat table indexes of these stats */
	s16b stat_add[A_MAX];    /* Modifiers to stats */

	bool immune_acid;			/* Immunity to acid */
	bool immune_elec;			/* Immunity to lightning */
	bool immune_fire;			/* Immunity to fire */
	bool immune_cold;			/* Immunity to cold */

	bool resist_acid;			/* Resist acid */
	bool resist_elec;			/* Resist lightning */
	bool resist_fire;			/* Resist fire */
	bool resist_cold;			/* Resist cold */
	bool resist_pois;			/* Resist poison */

	bool resist_fear;			/* Resist fear */
	bool resist_lite;			/* Resist light */
	bool resist_dark;			/* Resist darkness */
	bool resist_blind;			/* Resist blindness */
	bool resist_confu;			/* Resist confusion */
	bool resist_sound;			/* Resist sound */
	bool resist_shard;			/* Resist shards */
	bool resist_nexus;			/* Resist nexus */
	bool resist_nethr;			/* Resist nether */
	bool resist_chaos;			/* Resist chaos */
	bool resist_disen;			/* Resist disenchant */
	bool resist_mana_drain;    /* Resist mana draining */

	bool sustain_str;			/* Keep strength */
	bool sustain_int;			/* Keep intelligence */
	bool sustain_wis;			/* Keep wisdom */
	bool sustain_dex;			/* Keep dexterity */
	bool sustain_con;			/* Keep constitution */
	bool sustain_chr;			/* Keep charisma */

	bool slow_digest;			/* Slower digestion */
	bool ffall;					/* No damage falling */
	bool glowing;				/* Permanent light */
	bool regenerate;			/* Regenerate faster */
	bool telepathy;				/* Telepathy */
	bool see_inv;				/* Can see invisible */
	bool free_act;				/* Never paralyzed */
	bool hold_life;				/* Resist life draining */

	bool vuln_fire;             /* Take extra damage from fire */
	bool vuln_cold;             /* Take extra damage from cold */
	bool vuln_acid;             /* Take extra damage from acid */
	bool vuln_elec;             /* Take extra damage from electricity */
	bool vuln_pois;             /* Take extra damage from poison */
	bool vuln_lite;             /* Take extra damage from light */
	bool vuln_dark;             /* Take extra damage from dark */
	bool vuln_confu;			/* Take extra damage from confusion */
	bool vuln_sound;			/* Take extra damage from sound */
	bool vuln_shard;			/* Take extra damage from shards */
	bool vuln_nexus;			/* Take extra damage from nexus */
	bool vuln_nethr;			/* Take extra damage from nether */
	bool vuln_chaos;			/* Take extra damage from chaos */
	bool vuln_disen;			/* Take extra damage from disenchant */

	bool aggravate;				/* Aggravate monsters */
	bool teleport;				/* Random teleporting */
	bool drain_exp;				/* Experience draining (permanent, light) */
	bool black_breath;			/* Experience draining (temporary, heavy) */
	bool drain_light;			/* Drain light */

	bool bless_blade;			/* Blessed blade */

	s16b dis_to_d;				/* Known bonus to dam */
	s16b dis_to_a;				/* Known bonus to ac */

	s16b dis_ac;				/* Known base ac */

	s16b to_d;					/* Bonus to dam */
	s16b to_a;					/* Bonus to ac */

	s16b ac;					/* Base ac */

	s16b see_infra;				/* Infravision range */

	s16b skill_dis;				/* Skill: Disarming */
	s16b skill_dev;				/* Skill: Magic Devices */
	s16b skill_sav;				/* Skill: Saving throw */
	s16b skill_stl;				/* Skill: Stealth factor */
	s16b skill_srh;				/* Skill: Perception */
	s16b skill_awr;				/* Skill: Awareness */
	s16b skill_thn;				/* Skill: To hit (melee) */
	s16b skill_thn2;			/* Skill: To hit (offhand) */
	s16b skill_thb;				/* Skill: To hit (shooting) */
	s16b skill_tht;				/* Skill: To hit (throwing) */
	s16b skill_dig;				/* Skill: Digging */

	u32b noise;					/* Derived from stealth */

	s16b num_blow;				/* Number of blows */
	s16b num_blow2;				/* Number of blows from the secondary weapon */
	s16b num_fire;				/* Number of shots */

	byte ammo_mult;				/* Ammo multiplier */

	byte ammo_tval;				/* Ammo variety */

	s16b pspeed;				/* Current speed */

	s16b mana_bonus;       /* Bonus to mana */

	s16b life_recovery_value;  /* Speed of hitpoint recovery */
	s16b mana_recovery_value;  /* Speed of mana recovery */

	byte vulnerability;	/* Used to make animal packs charge and retreat */

	s16b invisible;				/* Total level of invisibility */

	bool soulsteal;				/* Your weapon needs feeding */
	bool nomagic;				/* Dispelled magic -- No spell-casting */
	bool twoweap;				/* Currently wielding two weapons */
	bool hitpoint_warning;      /* Display a hitpoint warning later */

	byte crossing_dir;			/* Direction of terrain crossing movement */
	byte crossing_moves;		/* Number of turns spent crossing */

};



/*
 * Semi-Portable High Score List Entry (128 bytes)
 *
 * All fields listed below are null terminated ASCII strings.
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
	char p_mag[2];		/* Player magic type (number) */
	char title[25];   /* Player title */
	char kill_desc[80];    /* Best kill */

	char cur_lev[4];		/* Current Player Power (number) */
	char cur_dun[4];		/* Current Dungeon Level (number) */
	char max_lev[4];		/* Max Player Power (number) */
	char max_dun[4];		/* Max Dungeon Level (number) */

	char how[32];		/* Method of death (string) */

	char pad[3];				/* Padding */
};


/*
 * A game color.
 */
struct color_type
{
	char index_char;            /* Character index:  'r' = red, etc. */

	char name[32];              /* Color name */

	s16b color_translate;       /* Index used in 16-color mode */

	byte kv;                    /* (unknown) */
	byte rv;                    /* Red */
	byte gv;                    /* Green */
	byte bv;                    /* Blue */
};


/*
 * An object flavour type
 */
struct flavor_type
{
	u32b text;      /* Text (offset) */

	byte tval;      /* Associated object type */
	byte sval;      /* Associated object sub-type */

	byte level;     /* Minimum level for flavour */
	byte unused;    /* unused */

	byte d_attr;    /* Default flavor attribute */
	char d_char;    /* Default flavor character */

	byte x_attr;    /* Desired flavor attribute */
	char x_char;    /* Desired flavor character */
};


/*
 * A projection graphics type.
 *
 * Among the possible future uses for "flags" is to allow transparency.
 * Among the possible future uses for "unused" is to allow multiple
 * user-editable colors (using a different array, indexed by this value).
 */
struct proj_graphics_type
{
	byte attr_vert;      /* Spell graphics */
	char char_vert;

	byte attr_horiz;
	char char_horiz;

	byte attr_rdiag;
	char char_rdiag;

	byte attr_ldiag;
	char char_ldiag;

	byte attr_ball;
	char char_ball;

	byte flags;          /* Special flags */
	byte unused;         /* Padding to make this array 32 bit-compliant */
};


/*
 * A graphics type
 *
 * Width of tiles (if constant), height of tiles (if constant),
 * unused, flags (such as masking),
 * file name (8 character max + '\0'), full name (24 character max)
 */
struct graphics_data_type
{
	byte tile_wid;       /* width of tiles */
	char tile_hgt;       /* height of tiles */

	byte unused;         /* unused */
	byte flags;          /* flags (such as masking) */

	char file_name[9];   /* file name (used to choose bitmap files and such) */
	char full_name[24];  /* full name (used for display) */
	char desc[256];      /* description */
};


/*
 * A list of monster and character movement moments.
 *
 * A "movement moment" is the exact time within the course of a game
 * turn in which the monster or character has exactly 100 energy, and
 * may move.  Other "entities", such as effects, may easily be added.
 */
struct move_moment_type
{
	s16b m_idx;
	s16b moment;
};

struct history_info
{
	u16b type;			/* Kind of history item */
	s16b dlev;			/* Dungeon level when this item was recorded */
	s16b clev;			/* Character level when this item was recorded */
	byte a_idx;			/* Artifact this item relates to */
	s32b turn;			/* Turn this item was recorded on */
	char event[80];	    /* The text of the item */
};
