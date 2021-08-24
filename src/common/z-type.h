/*
 * File: z-type.h
 * Purpose: Helper classes for the display of typed data
 */

#ifndef INCLUDED_ZTYPE_H
#define INCLUDED_ZTYPE_H

/* Defines a rectangle on the screen that is bound to a Panel or subpanel */
typedef struct
{
    int col;        /* x-coordinate of upper right corner */
    int row;        /* y-coordinate of upper right corner */
    int width;      /* width of display area. 1 - use system default. */
                    /* non-positive - rel to right of screen */
    int page_rows;  /* non-positive value is relative to the bottom of the screen */
} region;

struct loc
{
    int x;
    int y;
};

struct cmp_loc
{
    int x;
    int y;
    void *data;
};

/*
 * Defines a (value, name) pairing. Variable names used are historical.
 */
typedef struct
{
	byte tval;
	const char *name;
} grouper;

/*
 * A set of points that can be constructed to apply a set of changes to
 */
struct point_set
{
    int n;
    int allocated;
    struct cmp_loc *pts;
};

extern struct point_set *point_set_new(int initial_size);
extern void point_set_dispose(struct point_set *ps);
extern void add_to_point_set(struct point_set *ps, void *data, int y, int x);
extern int point_set_size(struct point_set *ps);
extern int point_set_contains(struct point_set *ps, int y, int x);

/**** MAngband specific ****/

/*
 * Buffers for ".txt" text files
 */
#define MAX_TEXTFILES   3
#define TEXTFILE__WID   140
#define TEXTFILE__HGT   23
#define TEXTFILE_MOTD   0
#define TEXTFILE_TOMB   1
#define TEXTFILE_CRWN   2

/* The setup data that the server transmits to the client */
typedef struct
{
    s16b frames_per_second;
    byte min_col;
    byte max_col;
    byte min_row;
    byte max_row;
    bool initialized;

    /* Static arrays to hold text screen loaded from TEXTFILEs */
    char text_screen[MAX_TEXTFILES][TEXTFILE__WID * TEXTFILE__HGT];
} server_setup_t;

extern server_setup_t Setup;

/* The setting data that the client transmits to the server */
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

    SETTING_MAX
};

enum grid_light_level
{
    LIGHTING_LOS = 0,  /* line of sight */
    LIGHTING_TORCH,    /* torchlight */
    LIGHTING_LIT,      /* permanently lit (when not in line of sight) */
    LIGHTING_DARK,     /* dark */

    LIGHTING_MAX
};

typedef byte byte_lit[LIGHTING_MAX];
typedef char char_lit[LIGHTING_MAX];

/* The setup data that the client transmits to the server */
typedef struct
{
    s16b settings[SETTING_MAX];
    byte *flvr_x_attr;
    char *flvr_x_char;
    byte (*f_attr)[LIGHTING_MAX];
    char (*f_char)[LIGHTING_MAX];
    byte (*t_attr)[LIGHTING_MAX];
    char (*t_char)[LIGHTING_MAX];
    byte *k_attr;
    char *k_char;
    byte *r_attr;
    char *r_char;
    byte proj_attr[PROJ_MAX][BOLT_MAX];
    char proj_char[PROJ_MAX][BOLT_MAX];
} client_setup_t;

extern client_setup_t Client_setup;

/*
 * Maximum channel name length
 */
#define MAX_CHAN_LEN 12

/* Information about a "chat channel" */
typedef struct
{
    char name[MAX_CHAN_LEN];
    s32b id;
    s32b num;
    byte mode;
} channel_type;

/*
 * Maximum number of channels.
 */
#define MAX_CHANNELS 255

extern channel_type channels[MAX_CHANNELS];

/* A structure to hold misc information on spells */
typedef struct
{
    int flag;       /* Actual spell flag */
    byte line_attr; /* "Color" of the spell (learned, worked, forgotten) */
    byte dir_attr;  /* Directional info */
    byte proj_attr; /* Can be projected */
} spell_flags;

/*
 * Information about maximal indices of certain arrays.
 */
struct angband_constants
{
    /* Array bounds, set on parsing edit files */
    u16b f_max;                 /* Maximum number of terrain features */
    u16b trap_max;              /* Maximum number of trap kinds */
    u16b k_max;                 /* Maximum number of object base kinds */
    u16b a_max;                 /* Maximum number of artifact kinds */
    u16b e_max;                 /* Maximum number of ego item kinds */
    u16b r_max;                 /* Maximum number of monster races */
    u16b mp_max;                /* Maximum number of monster pain message sets */
    u16b s_max;                 /* Maximum number of magic spells */
    u16b pit_max;               /* Maximum number of monster pit types */
    u16b act_max;               /* Maximum number of activations */
    u16b curse_max;             /* Maximum number of curses */
    u16b slay_max;              /* Maximum number of slays */
    u16b brand_max;             /* Maximum number of brands */
    u16b mon_blows_max;         /* Maximum number of monster blows */
    u16b blow_methods_max;      /* Maximum number of monster blow methods */
    u16b blow_effects_max;      /* Maximum number of monster blow effects */
    u16b equip_slots_max;       /* Maximum number of player equipment slots */
    u16b profile_max;           /* Maximum number of cave_profiles */
    u16b quest_max;             /* Maximum number of quests */
    u16b projection_max;        /* Maximum number of projection types */
    u16b calculation_max;       /* Maximum number of object power calculations */
    u16b property_max;          /* Maximum number of object properties */
    u16b summon_max;            /* Maximum number of summon types */
    u16b soc_max;               /* Maximum number of socials */
    u16b wf_max;                /* Maximum number of wilderness terrain features */
    u16b tf_max;                /* Maximum number of town terrain features */
    u16b town_max;              /* Maximum number of towns */
    u16b dungeon_max;           /* Maximum number of dungeons */

    /* Maxima of things on a given level, read from constants.txt */
    u16b level_monster_max;     /* Maximum number of monsters on a given level */

    /* Monster generation constants, read from constants.txt */
    u16b alloc_monster_chance;  /* 1/per-turn-chance of generation */
    u16b level_monster_min;     /* Minimum number generated */
    u16b town_monsters_day;     /* Townsfolk generated - day */
    u16b town_monsters_night;   /* Townsfolk generated  - night */
    u16b repro_monster_max;     /* Maximum breeders on a level */
    u16b ood_monster_chance;    /* Chance of OoD monster is 1 in this */
    u16b ood_monster_amount;    /* Max number of levels OoD */

    /* Monster gameplay constants, read from constants.txt */
    u16b glyph_hardness;        /* How hard for a monster to break a glyph */
    u16b repro_monster_rate;    /* Monster reproduction rate-slower */
    u16b life_drain_percent;    /* Percent of player life drained */
    u16b flee_range;            /* Monsters run this many grids out of view */
    u16b turn_range;            /* Monsters turn to fight closer than this */

    /* Dungeon generation constants, read from constants.txt */
    u16b level_room_max;        /* Maximum number of rooms on a level */
    u16b level_door_max;        /* Maximum number of potential doors on a level */
    u16b wall_pierce_max;       /* Maximum number of potential wall piercings */
    u16b tunn_grid_max;         /* Maximum number of tunnel grids */
    u16b room_item_av;          /* Average number of items in rooms */
    u16b both_item_av;          /* Average number of items in random places */
    u16b both_gold_av;          /* Average number of money items */
    u16b level_pit_max;         /* Maximum number of pits on a level */

    /* World shape constants, read from constants.txt */
    u16b max_depth;             /* Maximum dungeon level */
    u16b day_length;            /* Number of turns from dawn to dawn */
    u16b dungeon_hgt;           /* Maximum number of vertical grids on a level */
    u16b dungeon_wid;           /* Maximum number of horizontal grids on a level */
    u16b town_hgt;              /* Number of features in the starting town (vertically) */
    u16b town_wid;              /* Number of features in the starting town (horizontally) */
    u16b feeling_total;         /* Total number of feeling squares per level */
    u16b feeling_need;          /* Squares needed to see to get first feeling */
    u16b stair_skip;            /* Number of levels to skip for each down stair */
    u16b move_energy;           /* Energy the player or monster needs to move */

    /* Carrying capacity constants, read from constants.txt */
    u16b pack_size;             /* Maximum number of pack slots */
    u16b quiver_size;           /* Maximum number of quiver slots */
    u16b quiver_slot_size;      /* Maximum number of missiles per quiver slot */
    u16b floor_size;            /* Maximum number of items per floor grid */

    /* Store parameters, read from constants.txt */
    u16b store_inven_max;       /* Maximum number of objects in store inventory */
    u16b store_turns;           /* Number of turns between turnovers */
    u16b store_shuffle;         /* 1/per-day-chance of owner changing */
    u16b store_magic_level;     /* Level for apply_magic() in normal stores */

    /* Object creation constants, read from constants.txt */
    u16b max_obj_depth;         /* Maximum depth used in object allocation */
    u16b great_obj;             /* 1/chance of inflating the requested object level */
    u16b great_ego;             /* 1/chance of inflating the requested ego item level */
    u16b fuel_torch;            /* Maximum amount of fuel in a torch */
    u16b fuel_lamp;             /* Maximum amount of fuel in a lamp */
    u16b default_lamp;          /* Default amount of fuel in a lamp */

    /* Player constants, read from constants.txt */
    u16b max_sight;             /* Maximum visual range */
    u16b max_range;             /* Maximum missile and spell range */
    u16b start_gold;            /* Amount of gold the player starts with */
};

extern struct angband_constants *z_info;

extern const char *ANGBAND_SYS;

extern char *ANGBAND_DIR_GAMEDATA;
extern char *ANGBAND_DIR_CUSTOMIZE;
extern char *ANGBAND_DIR_HELP;
extern char *ANGBAND_DIR_SCREENS;
extern char *ANGBAND_DIR_FONTS;
extern char *ANGBAND_DIR_TILES;
extern char *ANGBAND_DIR_SOUNDS;
extern char *ANGBAND_DIR_ICONS;
extern char *ANGBAND_DIR_USER;
extern char *ANGBAND_DIR_SAVE;
extern char *ANGBAND_DIR_SCORES;

/*
 * Socials
 */
struct social
{
    char *name;         /* Name */
    char *text;         /* Text */
    unsigned int sidx;  /* Index */
    struct social *next;
    byte target;        /* Target type (target/no target) */
    byte max_dist;      /* Max distance of target allowed */
};

extern struct social *soc_info;

#endif /* INCLUDED_ZTYPE_H */
