/* File: types.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: global type declarations */


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

typedef struct {
    cptr name;
    cptr desc;
} name_desc_t, *name_desc_ptr;

/*
 * Feature state structure
 *
 * - Action (FF_*)
 * - Result (f_info ID)
 */
typedef struct feature_state feature_state;

struct feature_state
{
    byte action;
    s16b result;
};


/*
 * Information about terrain "features"
 */

typedef struct feature_type feature_type;
typedef struct feature_type feat_t, *feat_ptr;

struct feature_type
{
    u32b name;                /* Name (offset) */
    u32b text;                /* Text (offset) */
    s16b tag;                 /* Tag (offset) */

    s16b mimic;               /* Feature to mimic */

    u32b flags[FF_FLAG_SIZE]; /* Flags */

    u16b priority;            /* Map priority */
    s16b destroyed;           /* Default destroyed state */

    feature_state state[MAX_FEAT_STATES];

    byte subtype;
    byte power;

    byte d_attr[F_LIT_MAX];   /* Default feature attribute */
    byte d_char[F_LIT_MAX];   /* Default feature character */

    byte x_attr[F_LIT_MAX];   /* Desired feature attribute */
    byte x_char[F_LIT_MAX];   /* Desired feature character */
};

/* Effects are used for activations on objects (e.g. DSM) as well as devices (wand, rod, staff) */
struct effect_s
{
    s16b type;         /* See effect_e in defines.h. */
    byte power;        /* Power Level (1-100): Determines damage, etc */
    byte difficulty;   /* Difficulty Level (1-100): Determines fail rate. */
                       /* Think of power as the player level and difficulty as the spell level. */
    s16b cost;         /* Timeout for activations. SP for devices */
    s16b extra;        /* Optionally override normal damage calc. See do_effect for details on a per effect basis. */
};
typedef struct effect_s effect_t;

struct counts_s
{
    s32b generated;
    s32b found;
    s32b bought;
    s32b used;
    s32b destroyed;
};
typedef struct counts_s counts_t, *counts_ptr;

typedef struct {
    s32b found;
    s32b selling;
    s32b buying;
    s32b services;
    s32b winnings;
    s32b stolen;
} gold_counts_t;

/*
 * Information about object "kinds", including player knowledge.
 *
 */

typedef struct object_kind object_kind;

struct object_kind
{
    u32b idx;
    u32b name;            /* Name (offset) */
    u32b text;            /* Text (offset) */
    u32b flavor_name;        /* Flavor name (offset) */

    byte tval;            /* Object type */
    byte sval;            /* Object sub type */
    byte stack;           /* Max Stack Size (0 => use tv_info_t settings) */

    s16b pval;            /* Object extra info */

    s16b to_h;            /* Bonus to hit */
    s16b to_d;            /* Bonus to damage */
    s16b to_a;            /* Bonus to armor */

    s16b ac;            /* Base armor */

    byte dd, ds;        /* Damage dice/sides for melee weapons */
    s16b mult;          /* Damage multiplier (scaled by 100) for bows */

    s16b weight;        /* Weight */

    s32b cost;            /* Object "base cost" */

    u32b flags[OF_ARRAY_SIZE];    /* Flags */

    u32b gen_flags;        /* flags for generate */
    byte stack_chance;
    byte stack_dice, stack_sides;

    byte locale[4];        /* Allocation level(s) */
    byte chance[4];        /* Allocation chance(s) */

    byte level;            /* Level */
    byte max_level;        /* Level */

    effect_t activation;
    u32b     activation_msg;

    byte d_attr;        /* Default object attribute */
    byte d_char;        /* Default object character */


    byte x_attr;        /* Desired object attribute */
    byte x_char;        /* Desired object character */


    s16b flavor;        /* Special object flavor (or zero) */

    bool easy_know;        /* This object is always known (if aware) */


    bool aware;            /* The player is "aware" of the item's effects */
    bool tried;            /* The player has "tried" one of the items */
    counts_t counts;
};



/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile.
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */

typedef struct artifact_type artifact_type, *art_ptr;

struct artifact_type
{
    u32b name;            /* Name (offset) */
    u32b text;            /* Text (offset) */

    byte tval;            /* Artifact type */
    byte sval;            /* Artifact sub type */

    s16b pval;            /* Artifact extra info */

    s16b to_h;            /* Bonus to hit */
    s16b to_d;            /* Bonus to damage */
    s16b to_a;            /* Bonus to armor */

    s16b ac;            /* Base armor */

    byte dd, ds;        /* Damage when hits */
    s16b mult;          /* Damage multiplier (scaled by 100) for bows */

    s16b weight;        /* Weight */

    s32b cost;            /* Artifact "cost" */

    u32b     flags[OF_ARRAY_SIZE];       /* Artifact Flags */
    u32b     known_flags[OF_ARRAY_SIZE];

    effect_t activation;
    u32b     activation_msg;

    u32b gen_flags;        /* flags for generate */

    byte level;            /* Artifact level */
    byte rarity;        /* Artifact rarity */

    bool generated; /* Artifact has been created, but possibly yet to be discovered */
    bool found;     /* Player has found and identified this artifact */

    s16b tries;     /* Debug: Number of times the game tried to roll this art (statistics runs only) */
};


/*
 * Information about "ego-items".
 */

typedef struct ego_type ego_type, *ego_ptr;

struct ego_type
{
    u32b id;
    u32b name;            /* Name (offset) */
    u32b text;            /* Text (offset) */

    u32b type;            /* Type Flags (Bow, Weapon, Gloves, Helmet, Crown, Harp, etc)
                             Note: Due to object lore, it is very useful to share the same
                             ego type across different equipment slots. For example,
                             Elemental Protection can appear on multiple armor types. */

    byte level;           /* Minimum level */
    byte rarity;          /* Object rarity */
    byte max_level;       /* Maximum level. 0 => No restriction */

    s16b max_to_h;        /* Maximum to-hit bonus */
    s16b max_to_d;        /* Maximum to-dam bonus */
    s16b max_to_a;        /* Maximum to-ac bonus */

    byte max_pval;        /* Maximum pval */

    u32b flags[OF_ARRAY_SIZE];    /* Ego-Item Flags */
    u32b known_flags[OF_ARRAY_SIZE];
    u32b xtra_flags[OF_ARRAY_SIZE];

    u32b gen_flags;        /* flags for generate */
    effect_t activation;

    counts_t counts;

    vec_ptr  art_names;
    vec_ptr  art_names_high;
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
typedef bool (*object_p)(object_type *o_ptr);

struct obj_loc_s {
    byte where; /* INV_* code (see inv.h) */
    union {
        int slot; /* INV_PACK, INV_EQUIP, etc */
        struct { u16b dun_id, obj_id; s16b x, y; } floor;  /* INV_FLOOR */
        struct { u16b dun_id, obj_id, mon_id; } mon_pack; /* INV_MON_PACK */
    } v;
};
typedef struct obj_loc_s obj_loc_t, *obj_loc_ptr;

struct object_type
{
    s16b k_idx;            /* Kind index (zero if "dead") */

    byte tval;            /* Item type (from kind) */
    byte sval;            /* Item sub-type (from kind) */

    s16b pval;            /* Item extra-parameter */

    byte discount;        /* Discount (if any) */

    byte number;        /* Number of items */

    s16b weight;        /* Item weight in decipounds */

    s16b name1;            /* Artifact type, if any */
    s16b name2;            /* Ego-Item type, if any */
    s16b name3;         /* Random replacement for a fixed art */

    byte xtra1;            /* Extra info: Weaponsmith */
    byte xtra2;            /* Extra info index */
    byte xtra3;            /* Extra info: Chests and Weaponsmith. Device Power. */
    s16b xtra4;            /* Extra info: Lights, Capture, Quiver Capacity, Device MaxSP. */
    s32b xtra5;            /* Extra info: Device CSP */

    s16b to_h;            /* Plusses to hit */
    s16b to_d;            /* Plusses to damage */
    s16b to_a;            /* Plusses to AC */

    s16b ac;            /* Normal AC */

    byte dd, ds;        /* Damage dice/sides */
    s16b mult;          /* Damage multiplier (scaled by 100) for bows */

    s16b timeout;        /* Timeout Counter */

    byte ident;            /* Special flags  */

    u32b marked;        /* Object is marked */

    u16b inscription;    /* Inscription index */
    u16b art_name;      /* Artifact name (random artifacts) */

    byte feeling;          /* Game generated inscription number (eg, pseudo-id) */

    u32b flags[OF_ARRAY_SIZE];        /* Extra Flags for ego and artifacts */

    u32b curse_flags;        /* Flags for curse */

    u32b known_flags[OF_ARRAY_SIZE];
    u32b known_curse_flags;
    u32b known_xtra;

    u32b rune;

    object_type *next;  /* piles (floor or monster) */

    effect_t activation;
    obj_loc_t loc;

    s16b level;         /* object level on generation for my statistical pleasures */
    int  scratch;
};
#define object_is_(O, T, S) ((O) && (O)->tval == (T) && (O)->sval == (S))

#define MAX_MON_AURAS        3

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

typedef struct {
    s16b dis;            /* disarming */
    s16b dev;            /* magic devices */
    s16b sav;            /* saving throw */
    s16b stl;            /* stealth */
    s16b srh;            /* search ability */
    s16b fos;            /* search frequency */
    s16b thn;            /* combat (normal) */
    s16b thb;            /* combat (shooting) */
} skills_t, *skills_ptr;

#define SKILL_DESC_LEN 50
typedef struct {
    char dis[SKILL_DESC_LEN];
    char dev[SKILL_DESC_LEN];
    char sav[SKILL_DESC_LEN];
    char stl[SKILL_DESC_LEN];
    char srh[SKILL_DESC_LEN];
    char fos[SKILL_DESC_LEN];
    char thn[SKILL_DESC_LEN];
    char thb[SKILL_DESC_LEN];
} skills_desc_t, *skills_desc_ptr;

struct monster_body_s
{
    s16b     stats[MAX_STATS];
    skills_t skills;
    skills_t extra_skills;
    s16b     life;
    s16b     infra;
    s16b     spell_stat;
    s16b     body_idx;
    s16b     class_idx;
    s16b     speed;
    blows_calc_t blows_calc;
};
typedef struct monster_body_s monster_body_t;


typedef struct monster_race monster_race;

struct monster_race
{
    u32b name;                /* Name (offset) */
    u32b text;                /* Text (offset) */

    byte hdice;               /* Creatures hit dice count */
    byte hside;               /* Creatures hit dice sides */
    s16b ac;                  /* Armour Class: Always use mon_ac(mon) instead! */

    s16b sleep;               /* Inactive counter (base) */
    byte aaf;                 /* Area affect radius (1-100) */
    byte speed;               /* Speed (normally 110) */

    s32b mexp;                /* Exp value for kill */

    s16b weight;
    byte drop_theme;

    mon_spells_ptr spells;
    mon_drop_ptr drops;

    u32b flags1;              /* Flags 1 (general) */
    u32b flags2;              /* Flags 2 (abilities) */
    u32b flags3;              /* Flags 3 (race/resist) */
    u32b flags7;              /* Flags 7 (movement related abilities) */
    u32b flags8;              /* Flags 8 (wilderness info) */
    u32b flags9;              /* Flags 9 (drops info; possessor info) */
    u32b flagsr;              /* Flags R (resistances info) */
    u32b flagsx;              /* Temp Flags valid only for a single game. Written to savefile.
                                 For example, this unique is a questor. Or this
                                 unique is suppressed and won't appear in this game. */

    vec_ptr blows;
    mon_effect_t auras[MAX_MON_AURAS];

    s16b next_r_idx;          /* Evolution */
    u32b next_exp;

    byte pack_pct;           /* FRIENDS has become FRIENDS(XdY[,Z%]) if desired */
    byte pack_dice;
    byte pack_sides;

    s16b dun_type_id;         /* restricted to specified dungeon (e.g. Olympians to Mt Olympus; Amberites to Amber) */
    byte level;               /* Level of creature */
    byte rarity;              /* Rarity of creature */

    s16b max_level;
    s16b id;


    byte d_attr;              /* Default monster attribute */
    byte d_char;              /* Default monster character */
    byte x_attr;              /* Desired monster attribute */
    byte x_char;              /* Desired monster character */


    s16b max_num;             /* Maximum population allowed per level */
    s16b cur_num;             /* Monster population on current level */


    s16b r_sights;            /* Count sightings of this monster */
    s16b r_deaths;            /* Count deaths from this monster */

    s16b r_pkills;            /* Count visible monsters killed in this life */
    s16b r_akills;            /* Count all monsters killed in this life */
    s16b r_tkills;            /* Count monsters killed in all lives */

    s16b r_skills;            /* Count all summons killed in this life */

    byte r_wake;              /* Number of times woken up (?) */
    byte r_ignore;            /* Number of times ignored (?) */

    byte r_xtra1;             /* Flags for Evolution and Possessor Body Info */
    byte r_xtra2;             /* Something (unused) */

    byte r_drop_gold;         /* Max number of gold dropped at once */
    byte r_drop_item;         /* Max number of item dropped at once */

    u32b r_spell_turns;       /* Number of spells cast or failed (so may exceed sum(spell->lore)) */
    u32b r_move_turns;        /* Includes attacking the player */
                              /* Now we can report accurate observed spell frequencies! */

    u32b r_flags1;            /* Observed racial flags */
    u32b r_flags2;            /* Observed racial flags */
    u32b r_flags3;            /* Observed racial flags */
    u32b r_flagsr;            /* Observed racial resistance flags */

    byte stolen_ct;           /* For uniques in this lifetime only. Prevents PickPocket scumming of excellent drop uniques */

    monster_body_t body;      /* For The Possessor */
    int scratch;
};



/*
 * Information about "skill"
 */

typedef struct skill_table skill_table;

struct skill_table
{
    s16b w_start[5][64];      /* start weapon exp */
    s16b w_max[5][64];        /* max weapon exp */
    s16b s_start[10];      /* start skill */
    s16b s_max[10];           /* max skill */
};


/*
 * A single "grid" in a Cave
 */

typedef struct cave_type cave_type;
typedef struct cave_type cave_t, *cave_ptr;

struct cave_type
{
    u16b info;        /* Hack -- cave flags */

    s16b feat;        /* Hack -- feature type */

    s16b special;    /* Special cave info */

    s16b mimic;        /* Feature to mimic */
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
enum {
    SM_REFLECTION = RES_MAX,
    SM_FREE_ACTION,
    SM_GUARDIAN,   /* XXX This is being checked, but is never set XXX */
    SM_CLONED,
    SM_PET,
    SM_FRIENDLY,
    SM_SUMMONED,
    SM_NO_S_SPECIAL,
    SM_MAX
};

struct monster_type
{
    u16b id;
    s16b r_idx;        /* Monster race index */
    s16b ap_r_idx;        /* Monster race appearance index */
    byte sub_align;        /* Sub-alignment for a neutral monster */
    byte mspeed;            /* Monster "speed" */

    u16b dun_id;
    bool ml;        /* Monster is "visible" */

    point_t pos;
    point_t target;

    s16b hp;        /* Current Hit points */
    s16b maxhp;        /* Max Hit points */
    s16b max_maxhp;        /* Max Max Hit points */
    s16b pain;
    s16b ac_adj;
    s16b mpower;    /* Monster power scales various things like melee skill, damage, AC, etc.
                       This field is a per mill value, just like p_ptr->clp */

    mon_tim_ptr timers;

    s16b energy_need;    /* Monster "energy" */
    s16b cdis;        /* Current dis from player */

    u32b mflag;        /* Extra monster flags. (Note: Not saved!) */
    u32b mflag2;        /* Extra monster flags. (This one *is* saved)  */


    object_type *obj;


    u16b nickname;        /* Monster's Nickname */

    u32b exp;

    u32b smart;            /* Field for "smart_learn" */

    s16b parent_m_idx;
    s16b pack_idx;

    byte drop_ct;
    byte stolen_ct;
    u16b turns;
    /* XXX Digression: The game used to have MFLAG_NICE, which was set on monster
     * birth along with "repair_monsters". For summoned monsters, this prevented them
     * casting a spell before the player had a chance to react to the initial summons, which
     * can be nasty with chain summoning and brain smashes. However, repair_monsters doesn't
     * work correctly with multiple levels as it only repairs monsters on the player's level.
     * (e.g. Nodens would come up the stairs and never cast spells since he never had
     * his MFLAG_NICE flag repaired).
     * Instead of this, we'll use a counter to track the number of moves each monster takes
     * (might be interesting for other things) and block spells on the monster's initial
     * move. This will allow a spell if a summoned monster gets a double move, but that is rare.
     * cf dun_process_monsters (dun.c) and _can_cast (mon_spell.c)
     * nb this "nice behaviour" requires RF1_FORCE_SLEEP XXX */

    byte anti_magic_ct;
    byte anger;
    s16b mana;

    s32b pexp;    /* player experience gained (x100). kept <= r_ptr->mexp */
};

enum {
    AI_SEEK = 0,
    AI_LURE = 1,
    AI_GUARD_MON = 2,
    AI_GUARD_POS = 3,
    AI_FEAR = 4,
    AI_SHOOT = 5,
    AI_MAINTAIN_DISTANCE = 6,
};

typedef struct pack_info_s pack_info_t;
struct pack_info_s
{
    u16b pack_idx;        /* Set when allocated */
    u16b leader_idx;
    s16b count;
    s16b ai;            /* How is the pack behaving? */
    u16b guard_idx;     /* Pack is guarding another monster, perhaps the leader. Or, this is a dungeon entrance being guarded. */
    s16b guard_x;       /* Pack is defending a specific location */
    s16b guard_y;
    s16b distance;
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
    s16b index;        /* The actual index */

    s16b level;        /* Base dungeon level */
    s16b max_level;
    byte prob1;        /* Probability, pass 1 */
    byte prob2;        /* Probability, pass 2 */
    byte prob3;        /* Probability, pass 3 */

    u16b total;        /* Unused for now */
};



/*
 * Available "options"
 *
 *    - Address of actual option variable (or NULL)
 *
 *    - Normal Value (TRUE or FALSE)
 *
 *    - Option Page Number (or zero)
 *
 *    - Savefile Set (or zero)
 *    - Savefile Bit in that set
 *
 *    - Textual name (or NULL)
 *    - Textual description
 */

typedef struct option_type option_type;

struct option_type
{
    bool    *o_var;

    byte    o_norm;

    byte    o_page;

    byte    o_set;
    byte    o_bit;

    cptr    o_text;
    cptr    o_desc;
};


typedef struct magic_type magic_type;

struct magic_type
{
    byte slevel;        /* Required level (to learn) */
    byte smana;            /* Required mana (to cast) */
    byte sfail;            /* Minimum chance of failure */
    byte sexp;            /* Encoded experience bonus */
};

/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */

typedef struct player_magic player_magic;

struct player_magic
{
    int spell_book;        /* Tval of spell books (if any) */
    int spell_xtra;        /* Something for later */

    int spell_stat;        /* Stat for spells (if any)  */
    int spell_type;        /* Spell type (mage/priest) */

    int spell_first;        /* Level of first spell */
    int spell_weight;        /* Weight that hurts spells */

    magic_type info[MAX_MAGIC][32];    /* The available spells */
};



/*
 * Player sex info
 */

typedef struct player_sex player_sex;

struct player_sex
{
    cptr title;            /* Type of sex */
    cptr winner;        /* Name of winner */
};

typedef struct player_pact player_pact;

struct player_pact
{
    cptr title;
    cptr alliance;
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
#define MAX_HANDS 6
#define MAX_ARMS  3
#define HAND_NONE -1
#define BLOW_NONE -1

/* XXX Player melee is being refactored.  Checkout plr_attack.h */

#define NUM_BLOWS(h) (p_ptr->attack_info[h].base_blow + p_ptr->attack_info[h].xtra_blow)

struct shooter_info_s
{
    int slot;
    int to_h;
    int to_d;
    int dis_to_h;
    int dis_to_d;
    int to_mult;
    int base_shot;
    int xtra_shot;
    byte tval_ammo;
    int breakage; /* pct of normal breakage odds ... default is 100 */
    bool heavy_shoot;
    u32b flags[OF_ARRAY_SIZE];
};

#define NUM_SHOTS (p_ptr->shooter_info.base_shot + p_ptr->shooter_info.xtra_shot)

typedef struct player_type player_type;

struct player_type
{
    s32b id;

    u16b initial_world_id; /* W_SMAUG for middle earth; W_AMBER for random world */
    u16b world_id;     /* worlds may be sequenced (W_SMAUG->W_SARUMAN->W_SAURON) */
    u16b dun_id;       /* may differ from cave->dun_id */
    point_t pos;       /* current position in dun_id */
    point_t old_pos;   /* last position in D_SURFACE for recall */
    point_t new_pos;   /* D_QUEST position: setting pos too early can break savefiles on panic_save */
    u32b turn;
    dun_bmp_ptr los;

    byte psex;            /* Sex index */
    s16b prace;            /* Race index */
    byte pclass;        /* Class index */
    byte personality;        /* Seikaku index */
    byte realm1;        /* First magic realm */
    byte realm2;        /* Second magic realm */
    byte dragon_realm;
    byte psubclass;        /* e.g. Pacts on Warlocks. Type of Weaponmaster.*/
    byte psubrace;      /* e.g. Parentage on Demigods */
    s16b current_r_idx;


    u16b expfact;

    s32b au;            /* Current Gold */
    s16b fame;

    s32b max_max_exp;    /* Max max experience (only to calculate score) */
    s32b max_exp;        /* Max experience */
    s32b exp;            /* Cur experience */
    u32b exp_frac;        /* Cur exp frac (times 2^16) */

    s16b lev;            /* Level */

    s32b mmhp;           /* Max Max hit pts */
    s32b mhp;            /* Max hit pts */
    s32b chp;            /* Cur hit pts */
    u32b chp_frac;       /* Cur hit frac (times 2^16) */

    s32b msp;            /* Max mana pts */
    s32b csp;            /* Cur mana pts */
    u32b csp_frac;       /* Cur mana frac (times 2^16) */

    s16b clp;            /* Cur life pts (per mill) */

    s16b max_plv;        /* Max Player Level */

    s16b stat_max[6];    /* Current "maximal" stat values */
    s16b stat_max_max[6];    /* Maximal "maximal" stat values */
    s16b stat_cur[6];    /* Current "natural" stat values */

    s16b learned_spells;
    s16b add_spells;

    u32b count;

    s16b afraid;        /* Timed -- Fear */
    s16b mimic_form;
    s16b tim_mimic;

    bool spell_turned;
    bool fasting;
    s16b free_turns;
    bool sense_artifact;
    s16b duelist_target_idx;

    bool return_ammo;
    bool big_shot;
    bool painted_target;
    int  painted_target_idx;
    int  painted_target_ct;
    bool easy_2weapon;
    bool speciality_equip;
    s16b crit_freq_mul;  /* per cent; so 150 makes 10%->15% and 20%->30%; 0 disables crits; 100 default */
    s16b crit_freq_add;  /* per mil; so 25 makes 10%->12.5% and 20%->22.5% (ninja); 1000 forces every hit critical */
    s16b crit_qual_mul;  /* per cent; scale the base of critical roll (i.e., the weight) */
    s16b crit_qual_add;  /* add to quality roll (see _plr_attack_crit_aux) */
    bool cleave;
    bool vorpal;
    bool whirlwind;
    s16b elaborate_defense;
    s16b cloak_of_shadows;
    bool lightning_reflexes;
    bool clear_mind;

    s16b ambush;     /* Non-zero allows ambush on sleeping monster. Pct damage multiplier */
    s16b backstab;   /* Non-zero allows backstab on fleeing monster. Pct damage multiplier */
    bool peerless_stealth;
    s16b open_terrain_ct;

    s16b entrench_x;
    s16b entrench_y;
    s16b entrench_ct;
    bool entrenched;
    bool inven_prot;
    bool quick_walk;

    /* Rune Knight: Some Rune effects might become general game mechanics, like Magic Resistance
       and Magic Absorption.  Also, let's consolidate the White Aura mutation with the Rune of
       Good Fortune into a cached Good Luck value */
    s16b magic_resistance;
    bool good_luck;

    bool rune_elem_prot;

    s16b chaos_patron;
    u32b muta[MUT_FLAG_SIZE];
    u32b muta_lock[MUT_FLAG_SIZE];   /* Mutations that can not be removed! */
    s16b demigod_power[MAX_DEMIGOD_POWERS];
    s16b draconian_power;

    s16b virtues[8];
    s16b vir_types[8];

    s16b energy_need;      /* Energy needed for next move */

    s16b food;          /* Current nutrition */

    u32b special_attack;      /* Special attack capacity -LM- */
    u32b special_defense;      /* Special block capacity -LM- */
    byte action;          /* Currently action */

    u32b spell_learned1;      /* bit mask of spells learned */
    u32b spell_learned2;      /* bit mask of spells learned */
    u32b spell_worked1;      /* bit mask of spells tried and worked */
    u32b spell_worked2;      /* bit mask of spells tried and worked */
    u32b spell_forgotten1;      /* bit mask of spells learned but forgotten */
    u32b spell_forgotten2;      /* bit mask of spells learned but forgotten */
    byte spell_order[64];      /* order spells learned/remembered/forgotten */

    s16b spell_exp[64];       /* Proficiency of spells */
    s16b weapon_exp[5][64];   /* Proficiency of weapons */
    s16b skill_exp[10];       /* Proficiency of misc. skill */
    s16b spells_per_round;    /* 175 = 1.75 spells per round, etc. Calculated in calc_bonuses(). Only works for book casters (do_cmd_cast) at the moment. */

    s32b magic_num1[MAX_MAGIC_NUM];     /* Array for non-spellbook type magic */
    byte magic_num2[MAX_MAGIC_NUM];     /* Flags for non-spellbook type magics */

    s16b concent;      /* Sniper's concentration level */

    s16b player_hp[PY_MAX_LEVEL];
    char died_from[80];         /* What killed the player */
    cptr last_message;        /* Last message on death or retirement */

    u16b total_winner;      /* Total winner */
    u16b panic_save;      /* Panic save */

    u16b noscore;          /* Cheating flags */

    bool is_dead;          /* Player is dead */

    bool wizard;          /* Player is in wizard mode */

    u16b riding;              /* Riding on a monster of this index */
    byte knowledge;           /* Knowledge about yourself */

    s16b start_race;          /* Race at birth */
    s32b old_race1;           /* Record of race changes */
    s32b old_race2;           /* Record of race changes */
    s16b old_realm;           /* Record of realm changes */

    s16b pet_follow_distance; /* Length of the imaginary "leash" for pets */
    s16b pet_extra_flags;     /* Various flags for controling pets */

    s16b today_mon;           /* Wanted monster */

    bool autopick_autoregister; /* auto register is in-use or not */

    /*** Temporary fields ***/

    bool playing;            /* True if player is playing */
    bool leaving;            /* True if player is leaving */

    u16b health_who;    /* Health bar trackee */

    s16b monster_race_idx;    /* Monster race trackee */

    s16b object_kind_idx;    /* Object kind trackee */

    s16b new_spells;    /* Number of spells available */
    s16b old_spells;

    s16b old_food_aux;    /* Old value of food */

    bool old_cumber_armor;
    bool old_cumber_glove;
    bool old_heavy_wield[MAX_HANDS];
    bool old_heavy_shoot;
    bool old_icky_wield[MAX_HANDS];
    bool old_riding_wield[MAX_HANDS];
    bool old_riding_ryoute;
    bool old_monlite;

    s16b old_lite;        /* Old radius of lite (if any) */

    bool cumber_armor;    /* Mana draining armor */
    int  cumber_armor_amt;
    bool cumber_glove;    /* Mana draining gloves */
    bool riding_ryoute;    /* Riding weapon */
    bool monlite;

    s16b cur_lite;        /* Radius of lite (if any) */


    u32b notice;        /* Special Updates (bit flags) */
    u32b update;        /* Pending Updates (bit flags) */
    u32b redraw;        /* Normal Redraws (bit flags) */
    u32b window;        /* Window Redraws (bit flags) */

    s16b stat_use[6];    /* Current modified stats */
    s16b stat_top[6];    /* Maximal modified stats */

    bool sutemi;
    bool counter;

    s32b align;                /* Good/evil/neutral */
    s16b run_py;
    s16b run_px;


    /*** Extracted fields ***/
    s16b stat_add[6];    /* Modifiers to stat values */
    s16b stat_ind[6];    /* Indexes into stat tables */

    s16b resist[RES_MAX];
    s16b life;

    bool reflect;
    bool sh_fire;
    bool sh_elec;
    bool sh_cold;
    bool sh_shards;
    bool sh_retaliation;
    bool sh_fear;

    bool no_eldritch;
    bool no_stun;
    bool no_cut;
    bool no_slow;
    bool no_passwall_dam;
    bool no_charge_drain;
    bool melt_armor;

    bool anti_magic;    /* Anti-magic */
    bool res_magic;
    bool vuln_magic;
    bool anti_tele;     /* Prevent teleportation */
    bool anti_summon;
    bool stealthy_snipe; /* MUT_PEERLESS_SNIPER and scout's Stealthy Snipe */
    bool nimble_dodge;

    bool sustain_str;    /* Keep strength */
    bool sustain_int;    /* Keep intelligence */
    bool sustain_wis;    /* Keep wisdom */
    bool sustain_dex;    /* Keep dexterity */
    bool sustain_con;    /* Keep constitution */
    bool sustain_chr;    /* Keep charisma */

    u32b cursed;         /* Player is cursed */

    bool can_swim;       /* No damage falling */
    bool levitation;     /* No damage falling */
    bool lite;           /* Permanent light */
    s16b free_act;       /* Resist paralysis; perhaps slowing */
    s16b see_inv;        /* Can see invisible */
    s16b regen;          /* Rate of regeneration: 100 = 100%, 200 = 200%, etc. */
    s16b hold_life;      /* Resist life draining */

    bool auto_id;
    bool auto_pseudo_id;
    int  auto_id_sp;
    bool cult_of_personality;
    bool fairy_stealth;

    bool telepathy;      /* Telepathy */
    bool esp_animal;
    bool esp_undead;
    bool esp_demon;
    bool esp_orc;
    bool esp_troll;
    bool esp_giant;
    bool esp_dragon;
    bool esp_human;
    bool esp_evil;
    bool esp_good;
    bool esp_nonliving;
    bool esp_unique;
    bool esp_magical;
    bool esp_living;
    bool wizard_sight;

    bool slow_digest;    /* Slower digestion */
    bool pass_wall;     /* Permanent wraithform */
    bool kill_wall;
    s16b dec_mana;
    s16b spell_power;
    s16b device_power;
    s16b spell_cap;
    s16b easy_spell;
    bool heavy_spell;
    bool warning;
    bool mighty_throw;
    bool see_nocto;        /* Noctovision */
    bool easy_capture;

    byte easy_realm1;   /* Magic Stones give realm specific boosts */

    bool move_random;   /* Cyberdemons and Possessors ... */

    s16b monk_lvl;
    cptr monk_tbl;

    vec_ptr           innate_blows; /* vec<mon_blow_ptr> (cf PU_INNATE) */

    int               weapon_ct;
    plr_attack_info_t attack_info[MAX_HANDS];
    plr_attack_info_t innate_attack_info;
    shooter_info_t    shooter_info;

    s16b dis_to_a;        /* Known bonus to ac */

    s16b dis_ac;        /* Known base ac */

    s16b to_h_m;            /* Bonus to hit (misc) */
    s16b to_d_m;            /* Bonus to dam (misc) */
    s16b to_a;            /* Bonus to ac */
    s16b bonus_to_a;   /* bless < stone skin < ult. (plr_bonus_ac) */
    s16b bonus_speed;  /* quicken vs !Speed vs psion */

    s16b to_d_spell;

    s16b to_m_chance;        /* Minusses to cast chance */

    bool ryoute;

    bool no_flowed;

    int birth_mutation;

    s16b ac;            /* Base ac */

    s16b see_infra;        /* Infravision range */

    skills_t skills;

    s16b skill_tht;        /* Skill: To hit (throwing) */
    s16b skill_dig;        /* Skill: Digging */

    s16b pspeed;        /* Current speed */
};


/*
 * A structure to hold "rolled" information
 */
typedef struct birther birther;

struct birther
{
    s16b initial_world_id;
    byte game_mode;
    byte psex;         /* Sex index */
    s16b prace;        /* Race index */
    byte psubrace;
    byte pclass;       /* Class index */
    byte psubclass;       /* Subclass index */
    byte personality;     /* Seikaku index */
    byte realm1;       /* First magic realm */
    byte realm2;       /* Second magic realm */
    byte dragon_realm;

    s32b au;

    s16b stat_max[6];        /* Current "maximal" stat values */
    bool quick_ok;
};


typedef struct kamae kamae;

struct kamae
{
    cptr    desc;       /* A verbose kamae description */
    int     min_level;  /* Minimum level to use */
    cptr    info;
};

/*
 * Sort-array element
 */
typedef struct tag_type tag_type;

struct tag_type
{
    int     tag;
    int     value;
};

typedef bool (*monster_hook_type)(int r_idx);


/*
 * This seems like a pretty standard "typedef"
 */
typedef int (*inven_func)(object_type *);

/*
 *  A structure type for entry of auto-picker/destroyer
 */
typedef struct {
    cptr name;          /* Items which have 'name' as part of its name match */
    cptr insc;          /* Items will be auto-inscribed as 'insc' */
    u32b flag[2];       /* Misc. keyword to be matched */
    byte action;        /* Auto-pickup or Destroy or Leave items */
    byte dice;          /* Weapons which have more than 'dice' dice match */
    byte bonus;         /* Items which have more than 'bonus' magical bonus match */
    byte level;
    byte weight;
    int  value;
} autopick_type;

/*
 * A structure type for doors
 */
typedef struct
{
    s16b open;
    s16b broken;
    s16b closed;
    s16b locked[MAX_LJ_DOORS];
    s16b num_locked;
    s16b jammed[MAX_LJ_DOORS];
    s16b num_jammed;
} door_type;


/*
 *  A structure type for travel command
 */
#define TRAVEL_MODE_NORMAL   0
#define TRAVEL_MODE_AMMO     1
#define TRAVEL_MODE_AUTOPICK 2
typedef struct {
    point_t pos;
    int mode;
    int run;
    /* travel either by a direct path (This is an optimization for "auto_get") */
    point_vec_ptr path;
    int path_idx;
    /* or by a more expensive "flow" (Calculating this on D_SURFACE can be expensive) */
    dun_flow_ptr flow;
    int dir;
} travel_type;

typedef struct {
    int  id;
    cptr name;
    byte color;
    cptr desc;
    cptr parse;
    int  xtra;
} parse_tbl_t, *parse_tbl_ptr;

/*
 * A new spell system, and some half baked ideas for refactoring
 * Below here, is under construction, to be cleaned up later!
 */

typedef void (*ang_spell)(int cmd, var_ptr res);

typedef struct {
    int level;
    int cost;
    int fail;
    ang_spell fn;
} spell_info;

typedef void (*ang_spell_action)(const spell_info *spell);
typedef int (*calc_fail_fn)(int fail);

typedef struct {
int            stat;
spell_info    spell;
} power_info;

#define SPELL_FLAG_LEARNED   0x0001
#define SPELL_FLAG_FORGOTTEN 0x0002
#define SPELL_FLAG_NOTICED   0x0004
#define SPELL_FLAG_CONFIRM   0x0008
#define SPELL_FLAG_HIDE      0x0010

struct spell_stats_s
{
    int flags;
    int ct_cast;
    int ct_fail;
    int skill;      /* CASTER_GAIN_SKILL */
    int max_skill;
};

typedef struct spell_stats_s  spell_stats_t;
typedef spell_stats_t        *spell_stats_ptr;


/* TODO: This needs some work ... I just hacked this together for now.
   I'm shooting for a single unified interface for choosing, browsing
   and casting spells */

#define CASTER_ALLOW_DEC_MANA       0x0001 /* Wizardstaff and Mage Egos/Artifacts. cf equip.c for more details */
#define CASTER_GLOVE_ENCUMBRANCE    0x0002
#define CASTER_NO_SPELL_FAIL        0x0008
#define CASTER_USE_HP               0x0010
#define CASTER_GAIN_SKILL           0x0020
#define CASTER_USE_AU               0x0040 /* Leprechauns */
#define CASTER_SUPERCHARGE_MANA     0x0080
#define CASTER_USE_CONCENTRATION    0x0100 /* Sniper */

typedef struct {
    int max_wgt;    /* max weight before encumbrance */
    int weapon_pct; /* how much do melee weapons matter for encumbrance? */
    int enc_wgt;    /* how much weight over the max before 0sp */
} encumbrance_info;

typedef struct {
    cptr magic_desc;    /* spell, mindcraft, brutal power, ninjitsu, etc */
    int  min_fail;
    encumbrance_info encumbrance;
    int  which_stat;
    int  min_level;
    u32b options;
    ang_spell_action on_fail;    /* Hallucinate, Temporal Inversion, etc. */
    ang_spell_action on_cast;    /* Blood Knights take cuts, etc. */
    u32b realm1_choices;         /* XXX replace this after realm re-write */
    u32b realm2_choices;
} caster_info;

/************************************************************************
 * Player Race/Class Hooks. Because of Monster Mode, the plr_race_t needs
 * to be just as informative as plr_class_t.
 * Note: Hooks are typically named after their system point of origin.
 * For example, prt_effects() prints the player's current effect status,
 * so the hook is called prt_effects. Ditto with process_world, process_player,
 * move_player, etc. At some point, I will try for consistent system-wide
 * naming ...
 ************************************************************************/
typedef void (*birth_f)(void);
typedef int  (*birth_ui_f)(doc_ptr doc);
typedef void (*register_timers_f)(void);
typedef void (*load_f)(savefile_ptr file);
typedef void (*save_f)(savefile_ptr file);

typedef void (*process_world_f)(void);
typedef void (*process_player_f)(void);
typedef void (*player_action_f)(void);
typedef void (*move_player_f)(void);
typedef void (*move_monster_f)(mon_ptr mon);
typedef void (*kill_monster_f)(mon_ptr mon);
typedef int  (*obj_alloc_f)(obj_kind_ptr kind, int prob);

typedef void (*timer_on_f)(plr_tim_ptr timer);
typedef void (*timer_off_f)(plr_tim_ptr timer);

typedef void (*calc_bonuses_f)(void);
typedef void (*stats_f)(s16b stats[MAX_STATS]);
typedef void (*calc_weapon_bonuses_f)(obj_ptr obj, plr_attack_info_ptr info);
typedef void (*calc_shooter_bonuses_f)(obj_ptr obj, shooter_info_t *info_ptr);
typedef void (*flags_f)(u32b flgs[OF_ARRAY_SIZE]);
typedef status_display_t (*status_display_f)(void);

typedef caster_info* (*caster_info_f)(void);
typedef int  (*get_spells_f)(spell_info* spells, int max);

typedef void (*gain_level_f)(int new_level);
typedef void (*change_level_f)(int old_level, int new_level);
typedef void (*doc_f)(doc_ptr doc);

typedef void (*calc_innate_bonuses_f)(mon_blow_ptr blow);
typedef void (*calc_innate_attacks_f)(void);

typedef struct plr_hooks_s plr_hooks_t, *plr_hooks_ptr;
struct plr_hooks_s
{
    /* startup and savefiles */
    birth_f                 birth;          /* After py_birth() ... grant starting gear, etc */
    birth_ui_f              birth_ui;       /* Used during py_birth() ... choose a subclass */ 
    register_timers_f       register_timers;
    load_f                  load_player;
    save_f                  save_player;

    /* game processing */
    process_world_f         process_world;  /* Called every 10 game turns */
    process_player_f        process_player; /* Called from process_player ... but player take 0 or more actions per call */
    player_action_f         player_action;  /* Called once per player action, so long as the action consumes energy */
    move_player_f           move_player;    /* Called every time the player actually moves */
    move_monster_f          move_monster;    /* Called whenever a monster moves */
    kill_monster_f          kill_monster;
    obj_p                   destroy_object;
    obj_f                   get_object;
    obj_alloc_f             obj_alloc;

    /* timers */
    timer_on_f              timer_on;
    timer_off_f             timer_off;

    /* bonuses and status display */
    calc_bonuses_f          calc_bonuses;    /* Do flag related bonuses here ... */
    stats_f                 calc_stats;      /* ... and stat related stuff here */
    calc_weapon_bonuses_f   calc_weapon_bonuses;
    calc_shooter_bonuses_f  calc_shooter_bonuses;
    flags_f                 get_flags;
    status_display_f        status_display;
    doc_f                   prt_effects;

    /* spellcasting */
    caster_info_f           caster_info;
    get_spells_f            get_spells;
    get_spells_f            get_powers;

    /* character sheet and levelling up */
    gain_level_f            gain_level;
    change_level_f          change_level;
    doc_f                   character_dump;

    /* melee attacks */
    plr_attack_init_f       attack_init;
    mon_attack_init_f       mon_attack_init;
    calc_innate_attacks_f   calc_innate_attacks;   /* build the attacks: PU_INNATE */
    calc_innate_bonuses_f   calc_innate_bonuses;   /* tweak attacks: PU_BONUS (e.g. calculate blow->blows) */
};

typedef struct class_s class_t, *class_ptr;
struct class_s
{
    int         id;
    int         subid;
    cptr        name;
    cptr        subname;
    cptr        desc;
    cptr        subdesc;
    s16b        stats[MAX_STATS];
    skills_t    skills;
    skills_t    extra_skills; /* Prorata every 10 levels */
    s16b        life;
    s16b        base_hp;
    s16b        exp;
    byte        pets;
    u32b        flags;

    plr_hooks_t hooks;
};

struct equip_template_s;
typedef struct equip_template_s equip_template_t, *equip_template_ptr;

typedef struct race_s race_t, *race_ptr;
struct race_s
{
    int         id;
    int         subid;
    cptr        name;
    cptr        subname;
    cptr        desc;
    cptr        subdesc;
    s16b        stats[MAX_STATS];
    skills_t    skills;
    skills_t    extra_skills; /* Prorata every 10 levels (Monster Races) */
    s16b        life;
    s16b        base_hp;
    s16b        exp;
    s16b        infra;
    u32b        flags;
    bool        mimic;
    equip_template_ptr
                equip_template;
    int         boss_r_idx;
    s16b        pseudo_class_idx; /* For the "Monster" class ... */
    s16b        shop_adjust;

    plr_hooks_t hooks;
};

typedef struct {
    int  type;
    s16b tag;
    int  hand;
} equip_slot_t;

struct equip_template_s
{
    int          max;
    u32b         name;
    equip_slot_t slots[EQUIP_MAX + 1];
};

typedef struct {
    int                     id;
    cptr                    name;
    cptr                    desc;
    s16b                    stats[MAX_STATS];
    skills_t                skills;
    s16b                    life;
    s16b                    exp;
    s16b                    attack;
    s16b                    breath;
    s16b                    spell_stat;
} dragon_realm_t, *dragon_realm_ptr;

struct device_effect_info_s
{
    int      type;
    int      level;
    int      cost;
    int      rarity;
    int      max_depth;
    int      difficulty_base;
    int      difficulty_xtra;
    int      flags;
    counts_t counts;
    int      prob;
};

typedef struct device_effect_info_s  device_effect_info_t;
typedef struct device_effect_info_s *device_effect_info_ptr;

struct personality_s
{
    int            id;
    cptr           name;
    cptr           desc;
    s16b           stats[MAX_STATS];
    skills_t       skills;
    s16b           life;
    s16b           exp;
    int            flags;
    birth_f        birth;
    calc_bonuses_f calc_bonuses;
    flags_f        get_flags;
};

typedef struct personality_s personality_t, *personality_ptr;
