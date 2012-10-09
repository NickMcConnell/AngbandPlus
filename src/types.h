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
 * in "header_type", and the "m_idx" and "i_idx" fields in "cave_cell_type".  All
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

/*
 * Information about terrain "features"
 */

typedef struct feature_type feature_type;

struct feature_type {

    u16b name;          /* Name (offset)                */
    u16b text;          /* Text (offset)                */

    u16b mtyp;          /* Object type                  */
    u16b styp;          /* Object sub type              */

    s16b mim_m;         /* which number of feature type to mimic */
    s16b mim_s;         /* which number of feature type to mimic */

    byte priority;      /* priority in the 'M'ap function */

    byte d_attr;        /* Object "attribute"            */
    char d_char;        /* Object "symbol"               */

/* jk */
    u32b flags;         /* some flags */

    byte x_attr;        /* The desired attr for this feature    */
    char x_char;        /* The desired char for this feature    */
};

/*
 * this is used to reconstruct where something was found 
 */
typedef struct item_log_type item_log_type;

struct item_log_type {
   u32b where;        /* floor, vault, shop?          */
   u32b found_when;         /* when did we find this        */
   u16b whose;        /* shopkeeper, monster?         */
   s16b mlevel;       /* where                        */
   s16b slevel;       /* where                        */
};

/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */

typedef struct object_kind object_kind;

struct object_kind {

   u16b name;          /* Name (offset)                                */
   u16b text;          /* Text (offset)                                */

   byte tval;          /* Object type                                  */
   s16b sval;          /* Object sub type                              */

   s32b p1val;         /* Object extra info                            */
   s16b p2val;         /* Object extra extra info                      */

   s16b to_h;          /* Bonus to hit                                 */
   s16b to_d;          /* Bonus to damage                              */
   s16b to_a;          /* Bonus to armor                               */

   s16b ac;            /* Base armor                                   */

   byte dd, ds;        /* Damage dice/sides                            */

   s16b weight;        /* Weight                                       */

   s32b cost;          /* Object "base cost"                           */

   u64b flags1;        /* Flags, set 1                                 */
   u64b flags2;        /* Flags, set 2                                 */
   u64b flags3;        /* Flags, set 3                                 */

   byte locale[4];     /* Allocation level(s)                          */
   byte chance[4];     /* Allocation chance(s)                         */

   byte level;         /* Level                                        */
   byte extra;         /* Something                                    */

   byte d_attr;        /* Object "attribute"                           */
   char d_char;        /* Object "symbol"                              */

   byte x_attr;        /* The desired attr for this object             */
   char x_char;        /* The desired char for this object             */

   s16b flavor;        /* This object has a flavor                     */
   bool easy_know;     /* This object is always known (if aware)       */
   bool aware;         /* The player is "aware" of the item's effects  */
   bool tried;         /* The player has "tried" one of the items      */
   item_log_type log;
};

/*
 * describes a flavor-type
 */
typedef struct flavor_type flavor_type;

struct flavor_type
{
   char name[MAX_FLAVOR_LENGTH];
   byte color;
};

/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile.
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */

typedef struct artifact_type artifact_type;

struct artifact_type {

   u16b name;                  /* Name (offset)                */
   u16b text;                  /* Text (offset)                */

   byte tval;                  /* Artifact type                */
   s16b sval;                  /* Artifact sub type            */

   s16b p1val;                 /* Artifact extra info          */
   s16b p2val;                 /* Object extra extra info :-)  */

   s16b to_h;                  /* Bonus to hit                 */
   s16b to_d;                  /* Bonus to damage              */
   s16b to_a;                  /* Bonus to armor               */

   s16b ac;                    /* Base armor                   */

   byte dd, ds;                /* Damage when hits             */

   s16b weight;                /* Weight                       */

   s32b cost;                  /* Artifact "cost"              */

   u64b flags1;                /* Artifact Flags, set 1        */
   u64b flags2;                /* Artifact Flags, set 2        */
   u64b flags3;                /* Artifact Flags, set 3        */

   byte level;                 /* Artifact level               */
   byte rarity;                /* Artifact rarity              */

   byte cur_num;               /* Number created (0 or 1)      */
   byte max_num;               /* Unused (should be "1")       */
   u16b ident;                 /* Special flags                */
/* note that ident is necessary because we create items for monsters
 * when creating a level. Imagine being on a new level with an art-
 * ifact in a monster inventory. That should not be seen, but cur_num
 * is already 1. Checking if each artifact is in a monsters possession,
 * on the floor, in your inventory, etc. doesn't work because stores
 * may be stored on other levels that are on disk, not in memory.
 * so we need to keep it here */
   item_log_type log;
};

/*
 * Information about "ego-items".
 */

typedef struct ego_item_type ego_item_type;

struct ego_item_type {

   u16b name;                  /* Name (offset)                */
   u16b text;                  /* Text (offset)                */

   byte slot;                  /* Standard slot value          */
   byte rating;                /* Rating boost                 */
/* jk */
   s16b weightfactor;          /* weightfactor in % of normal item kind */

   byte level;                 /* Minimum level                */
   byte rarity;                /* Object rarity                */

   byte min_to_h;              /* Minimum to-hit bonus         */
   byte min_to_d;              /* Minimum to-dam bonus         */
   byte min_to_a;              /* Minimum to-ac bonus          */
   byte max_to_h;              /* Maximum to-hit bonus         */
   byte max_to_d;              /* Maximum to-dam bonus         */
   byte max_to_a;              /* Maximum to-ac bonus          */
/* jk */
   byte ds, dd;                /* overrule normal item damage dice */

   byte min_p1val;              /* Minimum p1val                 */
   byte max_p1val;              /* Maximum p1val                 */
   s16b min_p2val;              /* Object extra extra info :-)  */
   s16b max_p2val;              /* Object extra extra info :-)  */

   s32b cost;                  /* Ego-item "cost"              */

   u64b flags1;                /* Ego-Item Flags, set 1        */
   u64b flags2;                /* Ego-Item Flags, set 2        */
   u64b flags3;                /* Ego-Item Flags, set 3        */
   item_log_type log;
};

/*
 * simple definition for coordinate structures
 */
typedef struct coord coord;

struct coord
{
   s16b x;
   s16b y;
};

/*
 * debugflags
 */
typedef struct debug_flag_type debug_flag_type;

struct debug_flag_type
{
   cptr name;
   cptr explanation;
};




/*
 * Monster move prediction structure
 *
 */
typedef struct monster_move_type monster_move_type;

struct monster_move_type
{
   s16b x;           /* coordinates of this spot */
   s16b y;
   s16b dist;        /* absolute distance from player              */
   s16b reldist;     /* relative distance from player (min 0 max 3 */
   s16b exitdist;    /* distance from this square to best exit     */
   s16b suitable;    /* highest number indicates best move         */
   u32b result;      /* actions that should happen if this move is */
                     /* chosen (removing walls etc)                */
   bool player_view; /* can the player view this square            */
   bool possible;    /* for spotting impossible moves              */
   bool in_room;     /* is this move in a room?                    */
};

/*
 * Monster blow structure
 *
 *      - Method (RBM_*)
 *      - Effect (RBE_*)
 *      - Damage Dice
 *      - Damage Sides
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

struct monster_race {

    u32b name;                  /* Name (offset)                */
    u32b text;                  /* Text (offset)                */

    byte hdice;                 /* Creatures hit dice count     */
    byte hside;                 /* Creatures hit dice sides     */

    s16b ac;                    /* Armour Class                 */

    s16b sleep;                 /* Inactive counter (base)      */
    byte aaf;                   /* Area affect radius (1-100)   */
    byte speed;                 /* Speed (normally 110)         */

    s32b mexp;                  /* Exp value for kill           */

    s16b extra;                 /* Unused (for now)             */

    byte freq_inate;            /* Inate spell frequency        */
    byte freq_spell;            /* Other spell frequency        */

    u64b flags1;                /* Flags 1 (general)            */
    u64b flags2;                /* Flags 2 (abilities)          */
    u64b flags3;                /* Flags 3 (race/resist)        */
    u64b flags4;                /* Flags 4 (inate/breath)       */
    u64b flags5;                /* Flags 5 (normal spells)      */
    u64b flags6;                /* Flags 6 (special spells)     */

    /* corpse properties */
    u32b corpse_gives_alw;       /* Flags 1 effects given always             */
    u32b corpse_takes_alw;       /* Flags 2 effects removed always           */
    u32b corpse_gives_smt;       /* Flags 3 effects given sometimes          */
    u32b corpse_takes_smt;       /* Flags 4 effects removed sometimes        */

    s16b corpse_chance;          /* chance at corpse creation */
    s16b corpse_nutrition;       /* nutritional value */
    s16b corpse_weight;          /* corpse weight */
    s32b corpse_spoiling;        /* how long before nothing is left? */
    s16b corpse_chance_bones;    /* how big is the chance bones remain? */
    byte corpse_chance_gives;    /* what chance that these effects occur */
    byte corpse_chance_takes;    /* what chance that these effects occur */

    monster_blow blow[4];       /* Up to four blows per round   */

    byte level;                 /* Level of creature            */
    byte rarity;                /* Rarity of creature           */

    byte d_attr;                /* Racial "color"               */
    char d_char;                /* Racial "symbol"              */

    byte x_attr;                /* Desired monster attribute    */
    char x_char;                /* Desired monster character    */

    byte max_num;               /* Maximum population allowed per level */
    byte cur_num;               /* Monster population on current level */

/* jk */
    byte max_gen;               /* max generations when breeding        */
    s16b r_sights;              /* Count sightings of this monster      */
    s16b r_deaths;              /* Count deaths from this monster       */

    s16b r_pkills;              /* Count monsters killed in this life   */
    s16b r_tkills;              /* Count monsters killed in all lives   */

    byte r_wake;                /* Number of times woken up (?)         */
    byte r_ignore;              /* Number of times ignored (?)          */

    byte r_xtra1;               /* Something (unused)                   */
    byte r_xtra2;               /* Something (unused)                   */

    byte r_drop_gold;           /* Max number of gold dropped at once   */
    byte r_drop_item;           /* Max number of item dropped at once   */

    byte r_cast_inate;          /* Max number of inate spells seen      */
    byte r_cast_spell;          /* Max number of other spells seen      */

    byte r_blows[4];            /* Number of times each blow type was seen */
    u32b first_kill;            /* turn when this monster was first killed */

    u64b r_flags1;              /* Observed racial flags */
    u64b r_flags2;              /* Observed racial flags */
    u64b r_flags3;              /* Observed racial flags */
    u64b r_flags4;              /* Observed racial flags */
    u64b r_flags5;              /* Observed racial flags */
    u64b r_flags6;              /* Observed racial flags */
};

/*
 * Information about "vault generation"
 */

typedef struct vault_type vault_type;

struct vault_type {

   u16b name;                  /* Name (offset)                */
   u16b text;                  /* Text (offset)                */

   byte typ;                   /* Vault type                   */
   byte id;                    /* Vault id - same for multi-level vaults! */

   s16b sublevel;              /* On which sublevel is this supposed to go? */

   byte rat;                   /* Vault rating                 */

   s16b hgt;                   /* Vault height                 */
   s16b wid;                   /* Vault width                  */
   byte min_lev;               /* Minimum allowable level, if specified. */
   byte max_lev;               /* Maximum allowable level, if specified. */
   u64b flags1;                /* want: Flags 1 (general)        */
   u64b flags2;                /*       Flags 2 (abilities)      */
   u64b flags3;                /*       Flags 3 (race/resist)    */
   u64b flags4;                /*       Flags 4 (inate/breath)   */
   u64b flags5;                /*       Flags 5 (normal spells)  */
   u64b flags6;                /*       Flags 6 (special spells) */
   u64b nflags1;               /* not:  Flags 1 (general)        */
   u64b nflags2;               /*       Flags 2 (abilities)      */
   u64b nflags3;               /*       Flags 3 (race/resist)    */
   u64b nflags4;               /*       Flags 4 (inate/breath)   */
   u64b nflags5;               /*       Flags 5 (normal spells)  */
   u64b nflags6;               /*       Flags 6 (special spells) */
   u32b options;               /* options for this vault       */
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

typedef struct trap_item_type trap_item_type;

struct trap_item_type {
  bool inuse;  /* to allow a fast t_pop routine */
  /* room for some types of trap on this item at the same time*/
  byte type[MAX_TRAPS_IN_SET];
  /* and flags for the visibility of those */
  bool found[MAX_TRAPS_IN_SET];
  s16b tx;     /* -1 means it's on a chest */
  s16b ty;
  s16b tz;
};

typedef struct item_set_type item_set_type;

struct item_set_type {
  bool inuse;
  s16b x;      /* -1 signifies belonging to monster y */
  s16b y;
  s16b z;
  s16b index[ITEM_SET_SIZE];
};

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

typedef struct move_info_type move_info_type;

struct move_info_type {
   s16b to_speed;
   s16b to_search;
   s16b to_stealth;
   s16b to_percep;
   cptr name;
};

/*
 * A single "grid" in a Cave
 *
 * Note that several aspects of the code restrict the actual cave
 * to a max size of 256 by 256.  In partcular, locations are often
 * saved as bytes, limiting each coordinate to the 0-255 range.
 *
 * jk - no longer, all locations are saved as 16 bit ints now!
 *
 * The "i_idx" and "m_idx" fields are very interesting.  There are
 * many places in the code where we need quick access to the actual
 * monster or object(s) in a given cave grid.  The easiest way to
 * do this is to simply keep the index of the monster and object
 * (if any) with the grid, but takes a lot of memory.  Several other
 * methods come to mind, but they all seem rather complicated.
 *
 * Note the special fields for the simple "monster flow" code,
 * and for the "tracking" code.
 */

typedef struct cave_cell_type cave_cell_type;

struct cave_cell_type {
  u32b fdat;                   /* Hack -- cave flags */
  u16b mtyp;                   /* Hack -- main feature type */
  u16b styp;                   /* feature subtype */
  s16b i_idx;                  /* Item index (in i_list/item_set_list) or 0 */
  s16b m_idx;                  /* Monster index (in mn_list) or zero    */
  s16b t_idx;                  /* trap index (in t_list) or zero       */
/* jk extra currently is in use for what level a certain stair leads to */
/* and for the difficulty level of a closed door */
/* and for the index into the store array of a given store */
  s32b extra;                  /* whatever we need */
  u16b memory_mtyp;            /* in players memory, it looks like this */
  u16b memory_styp;
#ifdef MONSTER_FLOW
  byte cost;                   /* Hack -- cost of flowing              */
  byte when;                   /* Hack -- when cost was computed       */
#endif
};

/*
 * jk - this is handy for making tables of objects
 * note that tval is a s16b instead of a byte here, because the tables
 * denoting which is acceptable for stores and which isn't use values <0
 */

typedef struct store_table_type store_table_type;

struct store_table_type {
   s16b tval;
   s16b sval;
   byte chance;
};

/*
 * this identifies an array used to describe items
 */
typedef struct obj_info_type obj_info_type;

struct obj_info_type {
   char str[128][2048];
};

/*
 * Structure for an object. (32 bytes)
 *
 * Note that a "discount" on an item is permanent and never goes away.
 *
 * Note that inscriptions are now handled via the "quark_str()" function
 * applied to the "note" field, which will return NULL if "note" is zero.
 *
 * Note that "object" records are "copied" on a fairly regular basis.
 *
 * Note that "object flags" must now be derived from the object kind,
 * the artifact and ego-item indexes, and the two "xtra" fields.
 */

typedef struct object_type object_type;

struct object_type {

   s16b k_idx;              /* Kind index (zero if "dead")  */
   s16b ix;                 /* X-position on map, or zero   */
                            /* x -1 means in monster m_idx y's inventory */
                            /* when are they zero?          */
   s16b iy;                 /* Y-position on map, or zero   */
   s16b iz;                 /* which sub-level? 0 is main level */

   byte tval;               /* Item type (from kind)        */
   s16b sval;               /* Item sub-type (from kind)    */

   s32b p1val;              /* Item extra-parameter         */
   s16b p2val;              /* Object extra extra info :-)  */

   byte discount;           /* Discount (if any)            */

   byte number;             /* Number of items              */

   s16b weight;             /* Item weight                  */

   byte name1;              /* Artifact type, if any        */
   byte name2;              /* Ego-Item type, if any        */

   byte xtra1;              /* Extra info type              */
   s32b xtra2;              /* Extra info index             */

   s16b to_h;               /* Plusses to hit               */
   s16b to_d;               /* Plusses to damage            */
   s16b to_a;               /* Plusses to AC                */

   s16b ac;                 /* Normal AC                    */

   byte dd, ds;             /* Damage dice/sides            */

   s16b timeout;            /* Timeout Counter              */

   u16b ident;              /* Special flags                */

   byte marked;             /* Object is marked             */

   u16b note;               /* Inscription index            */
   s16b spell_set;          /* pointer to spell set         */
   item_log_type log;
};

typedef struct spell_set_type spell_set_type;

struct spell_set_type {
   bool inuse;
   u16b spells[MAX_SPELLS_PER_ITEM/16+1]; /* spells stored      */
};

/*
 * Monster information, for a specific monster.
 *
 * Note: fy, fx constrain dungeon size to 256x256
 */

typedef struct monster_type monster_type;

struct monster_type {

  s16b r_idx;                   /* Monster race index           */

/* jk - was byte */
  s16b fx;                      /* X location on map            */
  s16b fy;                      /* Y location on map            */
  s16b fz;                      /* Z location on map            */

  s16b hp;                      /* Current Hit points           */
  s16b maxhp;                   /* Max Hit points               */

  s16b csleep;                  /* Inactive counter             */

  byte mspeed;                  /* Monster "speed"              */
  byte energy;                  /* Monster "energy"             */

  s16b confused;                /* Timed -- Confusion           */
  s16b afraid;                  /* Timed -- Fear                */
  s16b stun;                    /* Timed -- Stun                */

  u16b attacked;                /* how many turns ago           */
  s16b escaping;                /* did we just nick something?  */
  byte oldspeed;                /* and what speed were we then? */
  byte breed_counter;           /* n'th generation by now       */

  byte mflag;                   /* Monster state                */
  s16b cdis;                    /* Current dis from player      */
  bool has_drop;                /* Monster carries objects      */

  bool los;                     /* Monster is "in sight"        */
  bool ml;                      /* Monster is "visible"         */
  byte last_hit;                /* how much did the last hit take off the player */

#ifdef WDT_TRACK_OPTIONS
  s16b tx;                      /* X location of target         */
  s16b ty;                      /* Y location of target         */
  byte t_dur;                   /* How long are we tracking     */
  byte t_bit;                   /* Up to eight bit flags        */
#endif

#ifdef DRS_SMART_OPTIONS
  u32b smart;                   /* Field for "smart_learn"      */
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
   s16b index;    /* The actual index */

   byte level;    /* Base dungeon level */
   byte prob1;    /* Probability, pass 1 */
   byte prob2;    /* Probability, pass 2 */
   byte prob3;    /* Probability, pass 3 */

   u16b total;    /* Unused for now */
};

/*
 * Available "options"
 *
 *      - Address of actual option variable (or NULL)
 *
 *      - Normal Value (TRUE or FALSE)
 *
 *      - Option Page Number (or zero)
 *
 *      - Savefile Set (or zero)
 *      - Savefile Bit in that set
 *
 *      - Textual name (or NULL)
 *      - Textual description
 */

typedef struct option_type option_type;

struct option_type {

    bool        *variable;
    bool        stdval;
    byte        page;
    cptr        descr;
};

/*
 * structure for menu hooks in help-files
 */
typedef struct hook_type hook_type;

struct hook_type {
   char hook[10][32];
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
 *
 * Actually, in Angband 2.8.0 it will probably prove easier to restrict
 * the concept of quest monsters to specific unique monsters, and to
 * actually scan the dead unique list to see what quests are left.
 */

typedef struct quest quest;

struct quest {

    int level;          /* Dungeon level */
    int r_idx;          /* Monster race */

    int cur_num;        /* Number killed (unused) */
    int max_num;        /* Number required (unused) */
};

/*
 * A store owner
 */

typedef struct owner_type owner_type;

struct owner_type {

  cptr owner_name;      /* Name */

  s32b max_cost;        /* Purse limit */

  byte max_inflate;     /* Inflation (max) */
  byte min_inflate;     /* Inflation (min) */

  byte haggle_per;      /* Haggle unit */

  byte insult_max;      /* Insult limit */

  byte owner_race;      /* Owner race */

  byte unused;          /* Unused */
};

/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */

typedef struct store_type store_type;

struct store_type {

  byte owner;                               /* Owner index                  */
  byte store_type;                          /* What type of store is this   */
  s16b sx;                                  /* where is this store on the map */
  s16b sy;
  byte extra;                               /* Unused for now               */

  s16b insult_cur;                          /* Insult counter               */

  s16b good_buy;                            /* Number of "good" buys        */
  s16b bad_buy;                             /* Number of "bad" buys         */

  s32b store_open;                          /* Closed until this turn       */

  s16b stock_num;                           /* Stock -- Number of entries   */
  object_type stock[STORE_INVEN_MAX];       /* Stock -- Actual stock items  */
};

/*
 * sub-levels are levels reached via stairs in vaults for example
 * the idea is to allow towers or cellars on the same level, consisting
 * of a series of sublevels, each max 66x20 (one screen).
 * on screen, something like 'cellar 200 ft below main dungeon, 100 ft.'
 * multiple sub-level-stacks per level should be possible, as should perhaps
 * be stairs from sub-levels to other main-levels etc.
 * the extra byte in cave_cell_type is used to direct where stairs lead to.
 */

typedef struct level_info_type level_info_type;

struct level_info_type {
   bool saved;                                     /* has this level already been stored on disk */
   u32b first_visit;                               /* how long ago were we last there */
   u32b last_visit;                                /* how long ago were we last there */
};

typedef struct dungeon_info_type dungeon_info_type;

/* jk - we now have different dungeon-sets */
struct dungeon_info_type
{
   cptr name;                                      /* what name has this level                */
   cave_cell_type *level[MAX_SUB_LEVELS][MAX_HGT]; /* the subquest-levels                     */
   bool level_used[MAX_SUB_LEVELS];                /* are they used?                          */
   u16b level_name[MAX_SUB_LEVELS];                /* how are they called? offset from v_name */
   s16b level_depth[MAX_SUB_LEVELS];               /* where are they?                         */
   s16b level_wid[MAX_SUB_LEVELS];                 /* how big is this dungeon                 */
   s16b level_hgt[MAX_SUB_LEVELS];                 /*                                         */
};

/*
 * The "name" of spell 'N' is stored as spell_names[X][N],
 * where X is 0 for mage-spells and 1 for priest-spells.
 */

typedef struct magic_type magic_type;

struct magic_type {

  byte slevel;                  /* Required level (to learn)    */
  byte smana;                   /* Required mana (to cast)      */
  byte sfail;                   /* Minimum chance of failure    */
  byte sexp;                    /* Encoded experience bonus     */
  u16b flag;                    /* what type of spell is this   */
};

/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */

typedef struct spell_type spell_type;

struct spell_type
{
  u16b name;         /* Name (offset)                */
  u16b text;         /* Text (offset)                */

  s16b sclass;       /* what classes can cast this                        */
  byte scale;        /* what scale is this spell small/normal/big/super   */
  u16b type;         /* what type of spell is this                        */
  s16b level;        /* at what level can the player cast this            */
  s16b mana;         /* how much mana does it cost                        */
  s16b chance;       /* what is the initial fail-rate                     */
  s16b minfail;      /* what is the minimum fail-rate                     */
  u32b numcast;      /* how many times cast                               */
  char chr[8];       /* how does the beam look on screen                  */
  byte col[8];       /* beam color                                        */
};

/*
 * Player sex info
 */

typedef struct player_sex player_sex;

struct player_sex
{
        cptr title;             /* Type of sex */
        cptr winner;            /* Name of winner */
};

typedef struct ghost_type ghost_type;

struct ghost_type
{
   object_type inv[ITEM_SET_SIZE];
   s16b        r_idx;              /* in what monster race are we stored */
};

/*
 * Player racial info
 */

typedef struct player_race player_race;

struct player_race {

  cptr title;                   /* Type of race                 */

  s16b r_adj[6];                /* Racial stat bonuses          */

  s16b r_dis;                   /* disarming                    */
  s16b r_dev;                   /* magic devices                */
  s16b r_sav;                   /* saving throw                 */
  s16b r_stl;                   /* stealth                      */
  s16b r_srh;                   /* search ability               */
  s16b r_pcp;                   /* search frequency             */
  s16b r_thn;                   /* combat (normal)              */
  s16b r_thb;                   /* combat (shooting)            */
/* jk */
  byte tactic;                  /* base tactic                  */

  byte r_mhp;                   /* Race hit-dice modifier       */
  byte r_exp;                   /* Race experience factor       */

  byte b_age;                   /* base age                     */
  byte m_age;                   /* mod age                      */

  byte m_b_ht;                  /* base height (males)          */
  byte m_m_ht;                  /* mod height (males)           */
  byte m_b_wt;                  /* base weight (males)          */
  byte m_m_wt;                  /* mod weight (males)           */

  byte f_b_ht;                  /* base height (females)        */
  byte f_m_ht;                  /* mod height (females)         */
  byte f_b_wt;                  /* base weight (females)        */
  byte f_m_wt;                  /* mod weight (females)         */

  byte infra;                   /* Infra-vision range           */
};

/*
 * Player class info
 */

typedef struct player_class player_class;

struct player_class {

  cptr title;                   /* Type of class                */

  s16b c_adj[6];                /* Class stat modifier          */
  byte spell_stat;              /* do the spells depend on INT or WIS  */
  s16b spell_encumbrance;       /* at what weight spells suffer?       */
  s16b spell_level;             /* what's the level of the first spell */

  s16b c_dis;                   /* class disarming              */
  s16b c_dev;                   /* class magic devices          */
  s16b c_sav;                   /* class saving throws          */
  s16b c_stl;                   /* class stealth                */
  s16b c_srh;                   /* class searching ability      */
  s16b c_pcp;                   /* class searching frequency    */
  s16b c_thn;                   /* class to hit (normal)        */
  s16b c_thb;                   /* class to hit (bows)          */
/* jk */
  s16b tactic;                  /* class tactic */

  s16b x_dis;                   /* extra disarming              */
  s16b x_dev;                   /* extra magic devices          */
  s16b x_sav;                   /* extra saving throws          */
  s16b x_stl;                   /* extra stealth                */
  s16b x_srh;                   /* extra searching ability      */
  s16b x_pcp;                   /* extra searching frequency    */
  s16b x_thn;                   /* extra to hit (normal)        */
  s16b x_thb;                   /* extra to hit (bows)          */

  s16b c_mhp;                   /* Class hit-dice adjustment    */
  s16b c_exp;                   /* Class experience factor      */

  s16b c_num;                   /* max blows per round          */
  s16b c_wgt;                   /* minimum weapon weight        */
  s16b c_mul;                   /* str multiplicator for blows  */
};

/* jk - this is snatched from spells1.c, now it can be used in monster3.c */
typedef bool (*inven_func)(object_type *);

/*
 * Some more player information
 *
 * This information is retained across player lives
 */
typedef struct player_other player_other;

struct player_other
{
   u32b window_flag[8]; /* Window flags */
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
   byte prace;                   /* Race index           */
   s16b pclass;                  /* Class index          */
   byte psex;                    /* Sex index */
   byte oops;                    /* Unused               */

   byte hitdie;                  /* Hit dice (sides)     */
   s16b expfact;                 /* Experience factor    */

   byte maximize;                /* Maximize stats       */
   bool teach_birth;             /* Used teaching as char generation method */

   s16b monster_race_idx;        /* Monster race trackee */
   s16b health_who;              /* Health bar trackee   */
   s16b object_kind_idx;         /* Object kind trackee  */
   s16b age;                     /* Characters age       */
   s16b ht;                      /* Height               */
   s16b wt;                      /* Weight               */
   s16b sc;                      /* Social Class         */
   s16b wx, wy;                  /* place on panel       */

   u32b au;                      /* Current Gold                  */

   s32b max_exp;                 /* Max experience                */
   s32b exp;                     /* Cur experience                */
   u16b exp_frac;                /* Cur exp frac (times 2^16)     */

   s16b lev;                     /* Level                         */

   s16b mhp;                     /* Max hit pts                   */
   s16b chp;                     /* Cur hit pts                   */
   u16b chp_frac;                /* Cur hit frac (times 2^16)     */

   s16b msp;                     /* Max mana pts                  */
   s16b csp;                     /* Cur mana pts                  */
   u16b csp_frac;                /* Cur mana frac (times 2^16)    */

   s16b max_plv;                 /* Max Player Level              */
   s16b max_dlv;                 /* Max level explored            */

   s16b stat_max[6];             /* Current "maximal" stat values */
   s16b stat_cur[6];             /* Current "natural" stat values */

   s16b fast;                    /* Timed -- Fast                 */
   s16b slow;                    /* Timed -- Slow                 */
   s16b blind;                   /* Timed -- Blindness            */
   s16b paralyzed;               /* Timed -- Paralysis            */
   s16b confused;                /* Timed -- Confusion            */
   s16b afraid;                  /* Timed -- Fear                 */
   s16b image;                   /* Timed -- Hallucination        */
   s16b poisoned;                /* Timed -- Poisoned             */
   s16b cut;                     /* Timed -- Cut                  */
   s16b stun;                    /* Timed -- Stun                 */
   byte arena_state;             /* in what state are we w.r.t the arena? */
/* jk */
   s16b total_weight;
   s16b equip_weight;
   s16b mdepth;
   s16b sdepth;
   s16b new_mdepth;
   s16b new_sdepth;
   bool crossing;                /* is crossing trees/rubble     */
   s16b running;                 /* is running */
   s16b resting;                 /* is resting -1 until healed / -2 until done */
   s16b sliding;                 /* Timed -- Sliding             */
   s16b throat;                  /* Timed -- Throat shut         */
   s16b fire;                    /* Timed -- fire skin           */
   s16b cold;                    /* Timed -- cold skin           */
   s16b acid;                    /* Timed -- acid skin           */
   s16b elec;                    /* Timed -- elec skin           */
   s16b lift;                    /* Timed -- increased lifting   */
   s16b reading;                 /* Timed -- reading page/prayer */

   bool sliding_now;             /* On a slide now ?             */
   byte tactic;                  /* from 128-4 extremely coward to */
                                 /* 128+4 berserker */
   byte movement;                /* base movement way            */

   s16b protevil;                /* Timed -- Protection          */
   u16b invuln;                  /* Timed -- Invulnerable        */
   s16b hero;                    /* Timed -- Heroism             */
   s16b shero;                   /* Timed -- Super Heroism       */
   s16b shield;                  /* Timed -- Shield Spell        */
   s16b blessed;                 /* Timed -- Blessed             */
   s16b tim_invis;               /* Timed -- See Invisible       */
   s16b tim_infra;               /* Timed -- Infra Vision        */

   s16b oppose_acid;             /* Timed -- oppose acid         */
   s16b oppose_elec;             /* Timed -- oppose lightning    */
   s16b oppose_fire;             /* Timed -- oppose heat         */
   s16b oppose_cold;             /* Timed -- oppose cold         */
   s16b oppose_pois;             /* Timed -- oppose poison       */

   s16b word_recall;             /* Word of recall counter       */

   s16b energy;                  /* Current energy               */

   s32b food;                    /* Current nutrition            */

   byte confusing;               /* Glowing hands                */
   byte searching;               /* Currently searching          */
   byte reflecting;              /* currently reflecting attacks */

   s16b new_spells;              /* Number of spells available   */

   s16b old_spells;

   bool twohands;                /* wielding a two-handed weapon? */
   bool old_cumber_armor;
   bool old_cumber_glove;
   bool old_heavy_wield1, old_heavy_wield2;
   bool old_heavy_shoot;
   bool old_icky_wield;

   s16b old_lite;                /* Old radius of lite (if any)  */
   s16b old_view;                /* Old radius of view (if any)  */

   s16b old_food_aux;            /* Old value of food            */

   bool cumber_armor;            /* Mana draining armor          */
   bool cumber_glove;            /* Mana draining gloves         */
   bool heavy_wield1, heavy_wield2; /* Heavy weapon                 */
   bool heavy_shoot;             /* Heavy shooter                */
   bool icky_wield;              /* Icky weapon                  */

   s16b cur_lite;                /* Radius of lite (if any)      */

   u32b notice;                  /* Special Updates (bit flags)  */

   u32b update;                  /* Pending Updates (bit flags)  */
   u32b redraw1;                 /* Desired Redraws (bit flags)  */
   u32b redraw2;                 /* Desired Redraws (bit flags)  */
   u32b window;                  /* Desired Window actions       */

   s16b stat_use[6];             /* Current modified stats       */
   s16b stat_top[6];             /* Maximal modified stats       */

   s16b stat_add[6];             /* Modifiers to stat values     */
   s16b stat_ind[6];             /* Indexes into stat tables     */
   u16b stat_cnt[6];             /* counter for temporary drains */
   u16b stat_los[6];             /* amount of temporary drains   */

   bool immune_acid;             /* Immunity to acid             */
   bool immune_elec;             /* Immunity to lightning        */
   bool immune_fire;             /* Immunity to fire             */
   bool immune_cold;             /* Immunity to cold             */

   bool resist_acid;             /* Resist acid          */
   bool resist_elec;             /* Resist lightning     */
   bool resist_fire;             /* Resist fire          */
   bool resist_cold;             /* Resist cold          */
   bool resist_pois;             /* Resist poison        */

   bool resist_conf;             /* Resist confusion     */
   bool resist_sound;            /* Resist sound         */
   bool resist_lite;             /* Resist light         */
   bool resist_dark;             /* Resist darkness      */
   bool resist_chaos;            /* Resist chaos         */
   bool resist_disen;            /* Resist disenchant    */
   bool resist_shard;            /* Resist shards        */
   bool resist_nexus;            /* Resist nexus         */
   bool resist_blind;            /* Resist blindness     */
   bool resist_neth;             /* Resist nether        */
   bool resist_fear;             /* Resist fear          */

   bool sustain_str;             /* Keep strength        */
   bool sustain_int;             /* Keep intelligence    */
   bool sustain_wis;             /* Keep wisdom          */
   bool sustain_dex;             /* Keep dexterity       */
   bool sustain_con;             /* Keep constitution    */
   bool sustain_chr;             /* Keep charisma        */

   bool aggravate;               /* Aggravate monsters   */
   bool teleport;                /* Random teleporting   */

   bool exp_drain;               /* Experience draining  */

   bool ffall;                   /* No damage falling    */
   bool lite;                    /* Permanent light      */
   bool free_act;                /* Never paralyzed      */
   bool see_inv;                 /* Can see invisible    */
   bool regenerate;              /* Regenerate hit pts   */
   bool hold_life;               /* Resist life draining */
   s16b telepathy;               /* Telepathy            */
   bool slow_digest;             /* Slower digestion     */
   bool bless_blade;             /* Blessed blade        */

   s16b dis_to_h;                /* Known bonus to hit   */
   s16b dis_to_d;                /* Known bonus to dam   */
   s16b dis_ring_to_d;           /* Known bonus to dam from ring */
   s16b dis_to_a;                /* Known bonus to ac    */

   s16b dis_ac;                  /* Known base ac        */

   s16b to_h;                    /* Bonus to hit         */
   s16b to_d;                    /* Bonus to dam         */
   s16b ring_to_d;               /* Plusses to damage from rings */
   s16b to_a;                    /* Bonus to ac          */

   s16b ac;                      /* Base ac              */

   s16b see_infra;               /* Infravision range    */

   s16b skill_dis;               /* Skill: Disarming             */
   s16b skill_dev;               /* Skill: Magic Devices         */
   s16b skill_sav;               /* Skill: Saving throw          */
   s16b skill_stl;               /* Skill: Stealth factor        */
   s16b skill_srh;               /* Skill: Searching ability     */
   s16b skill_pcp;               /* Skill: Searching frequency   */
   s16b skill_thn;               /* Skill: To hit (normal)       */
   s16b skill_thb;               /* Skill: To hit (shooting)     */
   s16b skill_tht;               /* Skill: To hit (throwing)     */
   s16b skill_dig;               /* Skill: Digging               */

   s16b teach_dis;               /* Skill: Disarming                */
   s16b teach_dev;               /* Skill: Magic Devices            */
   s16b teach_sav;               /* Skill: Saving throw             */
   s16b teach_stl;               /* Skill: Stealth factor           */
   s16b teach_srh;               /* Skill: Searching ability        */
   s16b teach_pcp;               /* Skill: Searching frequency      */
   s16b teach_thn;               /* Skill: To hit (normal)          */
   s16b teach_thb;               /* Skill: To hit (shooting)        */
   s16b teach_tht;               /* Skill: To hit (throwing)        */
   s16b teach_exp;               /* experience factor from teaching */
   s16b teach_stat[6];           /* Teached stat advances           */

   s16b num_blow1, num_blow2;    /* Number of blows      */
   s16b num_fire;                /* Number of shots      */

   byte tval_xtra;               /* Correct xtra tval    */

   byte tval_ammo;               /* Correct ammo tval    */
   byte xtra_blows1;             /* Extra hits */
   byte xtra_blows2;             /* Extra hits */
   byte xtra_shots;              /* Extra shots */
   byte xtra_might;              /* Extra might */

   s16b pspeed;                  /* Current speed        */

   s16b command_arg;    /* Gives argument of current command */
   s16b command_cmd;    /* Gives identity of current command */
   s16b command_dir;    /* Gives direction of current command */
   s16b command_new;    /* Hack -- command chaining XXX XXX */
   s16b command_rep;    /* Gives repetition of current command */
   s16b command_see;    /* See "cmd1.c" */
   s16b command_wrk;    /* See "cmd1.c" */
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
   char what[8];           /* Version info (string) */

   char pts[10];           /* Total Score (number) */

   char gold[10];          /* Total Gold (number) */

   char turns[10];         /* Turns Taken (number) */

   char day[10];           /* Time stamp (string) */

   char who[16];           /* Player Name (string) */

   char uid[8];            /* Player UID (number) */

   char sex[2];            /* Player Sex (string) */
   char p_r[3];            /* Player Race (number) */
   char p_c[3];            /* Player Class (number) */

   char cur_lev[4];        /* Current Player Level (number) */
   char cur_dun[4];        /* Current Dungeon Level (number) (main level) */
   char max_lev[4];        /* Max Player Level (number) */
   char max_dun[4];        /* Max Dungeon Level (number) */

   char how[32];           /* Method of death (string) */
};
#if 0
typedef struct addr_pair addr_pair;

struct addr_pair
{
  unsigned long address;
  cptr name;
};
#endif

/*
 * events, such as meeting your first vault
 */
typedef struct event_type event_type;

struct event_type {
   u32b turn;
   byte type;
   s16b index;
};

/*
 * the cause of an project() call
 */
typedef struct project_who_type project_who_type;

struct project_who_type {
   byte type;               /* WHO_MONSTER / WHO_PLAYER / WHO_TRAPBYPLAYER / WHO_TRAPBYMONSTER */
   u16b trigger;            /* what monster triggered that trap?                               */
   u16b trigger_race;       /* what monster triggered that trap? sometimes the monster has been*/
                            /* deleted before you can do anything with it, so the race is saved*/
                            /* separately                                                      */
   u16b index;              /* what monster hit you directly / what trap was triggered         */
   u16b index_race;         /* see above                                                       */
};

/*
 * spells that a monster can cast
 */
typedef struct cast_spell_type cast_spell_type;

struct cast_spell_type {
   byte flag;
   u64b spell;
};

