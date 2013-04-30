/*
 * File: types.h
 * Purpose: Global type declarations
 */

#ifndef INCLUDED_TYPES_H
#define INCLUDED_TYPES_H

#include "spells.h"

/*
 * This file contains various defined types used by the game.
 *
 * TODO: Most of these should be elsewhere, in their own header files.
 * For example, the object structs should be in object.h.
 *
 * Be careful when creating data structures; most of these are designed
 * to be serialised to file, so be careful to use exact-size data types
 * (like u32b and s32b) and not just "int"s.
 */

#include "h-option.h"
#include "z-color.h"

/**** Available Types ****/

/** An array of 256 bytes */
typedef byte byte_256[256];

/** An array of DUNGEON_WID bytes */
typedef byte byte_wid[DUNGEON_WID];

/** An array of DUNGEON_WID short integers */
typedef s16b s16b_wid[DUNGEON_WID];

/**** MAngband specific structs ****/

/* The setup data that the server transmits to the client */
typedef struct
{
    s16b frames_per_second;
    byte min_col;
    byte max_col;
    byte min_row;
    byte max_row;
    bool ready;
    bool wait;
    bool initialized;

    /* Static arrays to hold text screen loaded from TEXTFILEs */
    char text_screen[MAX_TEXTFILES][TEXTFILE__WID * TEXTFILE__HGT];
} server_setup_t;

enum
{
    SETTING_USE_GRAPHICS = 0,
    SETTING_SCREEN_COLS,
    SETTING_SCREEN_ROWS,
    SETTING_TILE_WID,
    SETTING_TILE_HGT,
    SETTING_TILE_DISTORTED,
    SETTING_MAX_HGT,
    SETTING_WINDOW_FLAG,
    SETTING_HITPOINT_WARN,
    SETTING_DELAY_FACTOR,
    SETTING_SQUELCH_JEWELRY,
    SETTING_SQUELCH_DRAG_ARMOR,
    SETTING_SQUELCH_WEARABLE,
    SETTING_SQUELCH_BOOK,
    SETTING_SQUELCH_CONSUMABLE,

    SETTING_MAX
};

typedef enum
{
    FEAT_LIGHTING_BRIGHT = 0,
    FEAT_LIGHTING_LIT,
    FEAT_LIGHTING_DARK,

    FEAT_LIGHTING_MAX
} grid_light_level;

typedef byte byte_lit[FEAT_LIGHTING_MAX];
typedef char char_lit[FEAT_LIGHTING_MAX];

/* The setup data that the client transmits to the server */
typedef struct
{
    s16b settings[SETTING_MAX];
    bool options[OPT_MAX];
    byte *flvr_x_attr;
    char *flvr_x_char;
    byte (*f_attr)[FEAT_LIGHTING_MAX];
    char (*f_char)[FEAT_LIGHTING_MAX];
    byte *k_attr;
    char *k_char;
    byte *r_attr;
    char *r_char;
    byte gf_attr[GF_MAX][BOLT_MAX];
    char gf_char[GF_MAX][BOLT_MAX];
    byte tval_attr[128];
} client_setup_t;

/*
 * Adding this structure so we can have different creatures generated
 * in different types of wilderness... this will probably be completly
 * redone when I do a proper landscape generator.
 */
typedef struct
{
    int world_x;    /* The world coordinates (transient) */
    int world_y;
    int type;       /* What kind of terrain we are in (transient) */
    int monst_lev;  /* Monster level (transient) */
    bool generated; /* Level is generated */
} wilderness_type;

/* The information needed to show a single "grid" */
typedef struct
{
    byte a; /* Color attribute */
    char c; /* ASCII character */
} cave_view_type;

/* Information about a "party" */
typedef struct
{
    char name[NORMAL_WID];  /* Name of the party */
    char owner[20];         /* Owner's name */
    s32b num;               /* Number of people in the party */
    hturn created;          /* Creation (or disband) time */
} party_type;

/* Information about a "house" */
typedef struct
{
    byte x_1;
    byte y_1;
    byte x_2;
    byte y_2;
    byte door_y;                /* Location of door */
    byte door_x;
    s32b depth;
    s32b price;                 /* Cost of buying */
    s32b ownerid;               /* Owner ID */
    char ownername[NORMAL_WID]; /* Owner name */
    byte color;                 /* Door color */
} house_type;

/* Information about a "hostility" */
typedef struct _hostile_type
{
    s32b id;                    /* ID of player we are hostile to */
    struct _hostile_type *next; /* Next in list */
} hostile_type;

/* Information about a "chat channel" */
typedef struct
{
    char name[MAX_CHAN_LEN];
    s32b id;
    s32b num;
    byte mode;
} channel_type;

/* Information about "Arena" (special building for pvp) */
typedef struct
{
    byte x_1;
    byte y_1;
    byte x_2;
    byte y_2;
    s32b depth;
    int player1;
    int player2;
} arena_type;

/* A structure to hold misc information on spells */
typedef struct
{
    int flag;       /* Actual spell flag */
    byte line_attr; /* "Color" of the spell (learned, worked, forgotten) */
    byte dir_attr;  /* Directional info */
    byte proj_attr; /* Can be projected */
} spell_flags;

/**** Available Structs ****/

/*
 * Information about maximal indices of certain arrays.
 *
 * These are actually not the maxima, but the maxima plus one, because of
 * 0-based indexing issues.
 */
typedef struct maxima
{
    u16b f_max;             /* Maximum number of terrain features */
    u16b k_max;             /* Maximum number of object base kinds */
    u16b a_max;             /* Maximum number of artifact kinds */
    u16b e_max;             /* Maximum number of ego item kinds */
    u16b r_max;             /* Maximum number of monster races */
    u16b mp_max;            /* Maximum number of monster pain message sets */
    u16b s_max;             /* Maximum number of magic spells */
    u16b pit_max;           /* Maximum number of monster pit types */
    u16b soc_max;           /* Maximum number of socials */
    u16b o_max;             /* Maximum number of objects on a given level */
    u16b m_max;             /* Maximum number of monsters on a given level */
} maxima;

/*
 * Information about terrain features.
 *
 * At the moment this isn't very much, but eventually a primitive flag-based
 * information system will be used here.
 */
typedef struct feature
{
    char *name;     /* Name */
    int fidx;       /* Index */
    struct feature *next;
    byte mimic;     /* Feature to mimic */
    byte priority;  /* Display priority */
    byte locked;    /* How locked is it? */
    byte jammed;    /* How jammed is it? */
    byte shopnum;   /* Which shop does it take you to? */
    byte dig;       /* How hard is it to dig through? */
    u32b effect;    /* Effect on entry to grid */
    u32b flags;     /* Terrain flags */
    byte d_attr;    /* Default feature attribute */
    char d_char;    /* Default feature character */
    byte x_attr[FEAT_LIGHTING_MAX]; /* Desired feature attribute (set by user/pref file) */
    char x_char[FEAT_LIGHTING_MAX]; /* Desired feature character (set by user/pref file) */
} feature_type;

/*
 * Information about "vault generation"
 */
struct vault
{
    char *name;         /* Name */
    char *text;         /* Text */
    unsigned int vidx;  /* Index */
    struct vault *next;
    byte typ;           /* Vault type */
    byte rat;           /* Vault rating */
    byte hgt;           /* Vault height */
    byte wid;           /* Vault width */
};

/*
 * An entry for the object/monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */
typedef struct
{
    s16b index;     /* The actual index */
    s16b level;     /* Base dungeon level */
    byte prob1;     /* Probability, pass 1 */
    byte prob2;     /* Probability, pass 2 */
    byte prob3;     /* Probability, pass 3 */
    u16b total;     /* Unused for now */
} alloc_entry;

/*
 * And here's the structure for the "fixed" spell information
 */
typedef struct spell
{
    char *name;         /* Name */
    char *text;         /* Text */
    unsigned int sidx;  /* Index */
    struct spell *next;
    byte realm;         /* tval - TV_MAGIC_BOOK */
    byte tval;          /* Item type for book this spell is in */
    byte sval;          /* Item sub-type for book (= book number) */
    byte snum;          /* Position of spell within book */
    byte spell_index;   /* Index into player_magic array */
    byte sdir;          /* Requires a direction */
    byte sproj;         /* Can be projected */
} spell_type;

/*
 * Socials
 */
typedef struct social
{
    char *name;         /* Name */
    char *text;         /* Text */
    unsigned int sidx;  /* Index */
    struct social *next;
    byte target;        /* Target type (target/no target) */
    byte max_dist;      /* Max distance of target allowed */
} social_type;

/* Defines a (value, name) pairing.  Variable names used are historical. */
typedef struct
{
	byte tval;
	const char *name;
} grouper;

typedef struct
{
    s16b m_idx;                 /* Monster index */
    u32b f_idx;                 /* Feature index */
    s16b first_o_idx;           /* The first item on the grid */
    bool multiple_objects;      /* Multiple objects in the grid */
    bool unseen_object;         /* Is there an unaware object there? */
    bool unseen_money;          /* Is there some unaware money there? */
    grid_light_level lighting;  /* Light level */
    bool in_view;               /* Grid can be seen */
    bool is_player;             /* Grid contains the player */
    bool hallucinate;           /* Hallucinatory grid */
    bool trapborder;            /* Trap detection boundary */
} grid_data;

/*
 * A game color.
 */
typedef struct
{
    char index_char;                /* Character index:  'r' = red, etc. */
    char name[32];                  /* Color name */
    byte color_translate[MAX_ATTR]; /* Index for various in-game translations */
} color_type;

/*
 * A hint.
 */
struct hint
{
    char *hint;
    struct hint *next;
};

#endif /* INCLUDED_TYPES_H */
