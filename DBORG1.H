
/* File: borg1.h */
/* Purpose: Header file for "borg1.c" -BEN- */

#ifndef INCLUDED_BORG1_H
#define INCLUDED_BORG1_H

#include "angband.h"



#ifdef ALLOW_BORG


/*
 * This file provides support for "borg1.c".
 */



/*** Some constants ***/


/*
 * Maximum possible dungeon size
 */
#define AUTO_MAX_X  DUNGEON_WID
#define AUTO_MAX_Y  DUNGEON_HGT



/*
 * Flags for the "info" field of grids
 *
 * Note that some of the flags below are not "perfect", in particular,
 * several of the flags should be treated as "best guesses", see below.
 *
 * The "BORG_MARK" flag means that the grid has been "observed", though
 * the terrain feature may or may not be memorized.  Note the use of the
 * "FEAT_NONE", "FEAT_FLOOR", and "FEAT_INVIS" feature codes below.
 *
 * The "BORG_GLOW" flag means that a grid is probably "perma-lit", but
 * sometimes it is really only "recently" perma-lit, but was then made
 * dark with a darkness attack, and it is now torch-lit or off-screen.
 *
 * The "BORG_DARK" flag means that a grid is probably not "perma-lit",
 * but sometimes it is really only "recently" dark, but was then made
 * "lite" with a "call lite" spell, and it is now torch-lit or off-screen.
 *
 * The "BORG_LITE" flag means that a grid is probably lit by the player
 * torch, but this may not be true if the nearby "BORG_VIEW" flags are
 * not correct, or if the "lite radius" has changed recently.
 *
 * The "BORG_VIEW" flag means that a grid is probably in line of sight
 * of the player, but this may not be true if some of the grids between
 * the player and the grid contain previously unseen walls/doors/etc.
 *
 * The "BORG_TEMP" flag means that a grid has been added to the array
 * "auto_temp_x"/"auto_temp_y", though normally we ignore this flag.
 *
 * The "BORG_XTRA" flag is used for various "extra" purposes, primarily
 * to assist with the "update_view()" code.
 */
#define BORG_MARK   0x01    /* observed grid */
#define BORG_GLOW   0x02    /* probably perma-lit */
#define BORG_DARK   0x04    /* probably not perma-lit */
#define BORG_OKAY   0x08    /* on the current panel */
#define BORG_LITE   0x10    /* lit by the torch */
#define BORG_VIEW   0x20    /* in line of sight */
#define BORG_TEMP   0x40    /* temporary flag */
#define BORG_XTRA   0x80    /* extra flag */


/*
 * Maximum size of the "lite" array
 */
#define AUTO_LITE_MAX 1536

/*
 * Some assistance with the borg_attack and magic arrows
 */
#define GF_ARROW_SEEKER  77
#define GF_ARROW_FLAME   70
#define GF_ARROW_FROST   71
#define GF_ARROW_ANIMAL  72
#define GF_ARROW_DRAGON  73
#define GF_ARROW_EVIL    74
#define GF_ARROW_WOUNDING 75
#define GF_HOLY_WORD      76

/*
 * Player race constants (hard-coded by save-files, arrays, etc)
 */
#define RACE_HUMAN      0
#define RACE_HALF_ELF   1
#define RACE_ELF        2
#define RACE_HOBBIT     3
#define RACE_GNOME      4
#define RACE_DWARF      5
#define RACE_HALF_ORC   6
#define RACE_HALF_TROLL 7
#define RACE_DUNADAN    8
#define RACE_HIGH_ELF   9


/*
 * Maximum size of the "view" array
 */
/*#define AUTO_VIEW_MAX 1536*/
#define AUTO_VIEW_MAX 9000


/*
 * Number of grids in the "temp" array
 */
#define AUTO_TEMP_MAX 1536


/*
 * Number of grids in the "flow" array
 */
#define AUTO_FLOW_MAX 1536



/*
 * Enable the "borg_note()" usage of the Recall Window
 * Also specify the number of "rolling rows" to use
 */
#define BORG_NOTE_ROWS      12



/*
 * Size of Keypress buffer
 */
#define KEY_SIZE 8192




/*
 * Object information
 */

typedef struct auto_take auto_take;

struct auto_take
{
    s16b    k_idx;      /* Kind index */

    bool    known;      /* Verified kind */

    bool    seen;       /* Assigned motion */

    bool    extra;      /* Unused */

    byte    x, y;       /* Location */

    s16b    when;       /* When last seen */
};


/*
 * Monster information
 */

typedef struct auto_kill auto_kill;

struct auto_kill
{
    s16b    r_idx;      /* Race index */

    bool    known;      /* Verified race */
    bool    awake;      /* Probably awake */

    bool    confused;   /* Probably confused */
    bool    afraid;     /* Probably afraid */
    bool    quiver;     /* Probably quivering */
    bool    stunned;
    bool    poisoned;   /* Probably poisoned */
    bool    invuln;     /* in Stasis */

    bool    seen;       /* Assigned motion */
    bool    used;       /* Assigned message */

    byte    x, y;       /* Location */

    byte    ox, oy;     /* Old location */

    byte    speed;      /* Estimated speed */
    byte    moves;      /* Estimates moves */
    bool    ranged_attack; /* can attack from a dx */
    s16b    power;      /* Estimated hit-points */
    s16b    other;      /* Estimated something */
    s16b    level;      /* Monsters Level */

    s16b    when;       /* When last seen */
};



/*
 * OPTION: Use the "room" support
 *
 * Note -- this option causes slight bugs in "borg-ext.c" that
 * I have not yet tracked down.  You have been warned...
 */
/* #define BORG_ROOMS */


#ifdef BORG_ROOMS


/*
 * OPTION: Allow rooms to intersect.  This is "pretty" but inefficient.
 */
/* #define CROSS_ROOMS */


/*
 * Maximum number of rooms.  This may be too small.
 * But if AUTO_ROOMS * sizeof(auto_room) > 64K then some
 * machines may not be able to allocate the room array.
 */
#define AUTO_ROOMS  (AUTO_MAX_X * AUTO_MAX_Y / 8)



/*
 * Forward declare
 */
typedef struct auto_room auto_room;


/*
 * A room in the dungeon.  20 bytes.
 *
 * The "rooms" are designed to (1) optimize the storage of information
 * about the map and (2) simplify navigation.  Thus, we create rooms by
 * grouping relevant rectangles of grids together.  This includes all
 * "normal" rooms in the dungeon, most "corridors", occasional "thick"
 * corridors, all left-over floor grids, and many regions in the town.
 *
 * A room index of "zero" means that the grid is not in any room yet
 * A room index N such that 0<N<MAX_ROOMS refers to a single room
 * A room index MAX_ROOMS+N means the grid is inside "N" different rooms
 *
 * A few options in the code itself determine if two rooms can overlap.
 * Likewise, the code handles issues like one room absorbing another,
 * and destroying any rooms that are suddenly found to include a grid
 * which is actually a wall.  This allows the Borg to recover from
 * earthquakes, and also lets it assume that all monsters and objects
 * are actually on floors, since if they move, revealing a wall, the
 * Borg will simply take back all previous assumptions.
 *
 * The rooms are maintained in a "room array" of a size AUTO_ROOMS.
 * The first and last rooms in this array are used to maintain two
 * in-place lists, a stack of "free" rooms, and a priority queue
 * of rooms used to propagate the current "flow".
 *
 * The "self" field gives the index of a room.
 *
 * The "free" field is either zero, meaning that the room is being used,
 * or it holds the index of the next free room.  Note that the first
 * room always points to the next free room, and the last room always
 * points to itself (indicating we have run out of rooms).
 *
 * The "when" field allows us to make decisions based on the actual
 * "time" at which a room was visited.  Currently this field is used
 * only by the "revisit" function to choose "interesting" rooms.  We
 * can probably delete this field and still function.
 *
 * The "location" of the corners of the room are encoded in x1/y1/x2/y2.
 */
struct auto_room
{
    s16b self;      /* Self index */

    s16b free;      /* Index of next room in free room list */

    s16b when;      /* When last visited */

    byte x1, y1;    /* Upper left corner */
    byte x2, y2;    /* Bottom right corner */
};


#endif



/*
 * Forward declare
 */
typedef struct auto_grid auto_grid;

/*
 * A grid in the dungeon.  Several bytes.
 *
 * There is a set of eight bit flags (see below) called "info".
 *
 * There is a terrain feature type, which may be incorrect.  It is
 * more or less based on the standard "feature" values, but some of
 * the legal values are never used, such as "secret door", and others
 * are used in bizarre ways, such as "invisible trap".
 *
 * There is an object index into the "object tracking" array.
 *
 * There is a monster index into the "monster tracking" array.
 *
 * There is a byte "hmmm" which is currently unused.
 *
 * There is a byte "xtra" which tracks how much "searching" has been done
 * in the grid or in any grid next to the grid.
 *
 * To perform "navigation" from one place to another, the "flow" routines
 * are used, which place "cost" information into the "cost" fields.  Then,
 * if the path is clear, the "cost" information is copied into the "flow"
 * fields, which are used for the actual navigation.  This allows multiple
 * routines to check for "possible" flowing, without hurting the current
 * flow, which may have taken a long time to construct.  We also assume
 * that the Borg never needs to follow a path longer than 250 grids long.
 * Note that the "cost" fields have been moved into external arrays.
 *
 * Hack -- note that the "char" zero will often crash the system!
 */
struct auto_grid
{
    byte feat;      /* Grid type */
    byte info;      /* Grid flags */

    byte take;      /* Object index */
    byte kill;      /* Monster index */

#ifdef BORG_ROOMS
    s16b room;      /* Room index */
#endif

    byte hmmm;      /* Extra field (unused) */

    byte xtra;      /* Extra field (search count) */
};


/*
 * Forward declare
 */
typedef struct auto_data auto_data;

/*
 * Hack -- one byte of info per grid
 *
 * We use a structure to encapsulate the data into a "typed" form.
 */
struct auto_data
{
    byte data[AUTO_MAX_Y][AUTO_MAX_X];
};




/*** Some macros ***/


/*
 * Determine "twice" the distance between two points
 * This results in "diagonals" being "correctly" ranged,
 * that is, a diagonal appears "furthur" than an adjacent.
 */
#define double_distance(Y1,X1,Y2,X2) \
    (distance(((int)(Y1))<<1,((int)(X1))<<1,((int)(Y2))<<1,((int)(X2))<<1))



/*** Some variables ***/


/*
 * Some variables
 */
extern bool auto_active;        /* Actually active */
extern bool borg_resurrect;     /* Continous play mode */
extern bool auto_cancel;        /* Being cancelled */

extern char genocide_target;    /* Identity of the poor unsuspecting soul */
extern int zap_slot;            /* to avoid a nasty game bug with amnesia */
extern bool borg_casted_glyph;  /* we dont have a launch messages anymore */
extern int auto_stop_dlevel;
extern int auto_stop_clevel;
extern bool auto_stop_king;
extern bool borg_dont_react;
extern int successful_target;
extern int sold_item_tval;
extern int sold_item_sval;
extern int sold_item_pval;
extern int sold_item_store;
extern int borg_chest_fail_tolerance;

/* options from the borg.txt file */
extern int auto_respawn;
extern int auto_respawn_race;
extern int auto_respawn_class;
extern int auto_respawn_str;
extern int auto_respawn_int;
extern int auto_respawn_wis;
extern int auto_respawn_dex;
extern int auto_respawn_con;
extern int auto_respawn_chr;
extern int auto_dump_level;
extern bool auto_respawn_winners;

extern bool borg_worships_damage;
extern bool borg_worships_speed;
extern bool borg_worships_hp;
extern bool borg_worships_mana;
extern bool borg_worships_ac;
extern bool borg_worships_gold;
extern bool borg_plays_risky;
extern bool borg_uses_swaps;
extern bool borg_uses_calcs;
extern bool borg_slow_optimizehome;
extern bool borg_scums_uniques;
extern bool borg_engage_cloak;


/* HACK... this should really be a parm into borg_prepared */
/*         I am just being lazy */
extern bool borg_slow_return;

/* dynamic required items */
/* dynamic required items */
typedef struct req_item
{
   int depth;
   int item;
   int number;

} req_item;

extern req_item *borg_required_item[MAX_CLASS];
extern int n_req[MAX_CLASS];
typedef struct power_item
{
   int depth;
   int cnd;
   int item;
   int from;
   int to;
   int power;
   bool each;
} power_item;

extern power_item *borg_power_item[MAX_CLASS];
extern int n_pwr[MAX_CLASS];
extern int *borg_has;
extern int *borg_has_on;
extern int *borg_artifact;
extern int *borg_skill;
extern int size_depth;
extern int size_obj;

/* NOTE: This must exactly match the prefix_pref enums in BORG1.c */
enum
{
    BI_STR,
    BI_INT,
    BI_WIS,
    BI_DEX,
    BI_CON,
    BI_CHR,
    BI_CSTR,
    BI_CINT,
    BI_CWIS,
    BI_CDEX,
    BI_CCON,
    BI_CCHR,
    BI_SSTR,
    BI_SINT,
    BI_SWIS,
    BI_SDEX,
    BI_SCON,
    BI_SCHR,
    BI_LITE,
    BI_HP,
    BI_HP_ADJ,
    BI_SP,
    BI_SP_ADJ,
    BI_FAIL1,
    BI_FAIL2,
    BI_CLEVEL,
    BI_ESP,
    BI_CUR_LITE,
    BI_RECALL,
    BI_FOOD,
    BI_SPEED,
    BI_SDIG,
    BI_FEATH,
    BI_REG,
    BI_SINV,
    BI_INVIS,
    BI_INFRA,
    BI_DIS,
    BI_DEV,
    BI_SAV,
    BI_STL,
    BI_SRCH,
    BI_SRCHFREQ,
    BI_THN,
    BI_THB,
    BI_THT,
    BI_DIG,
    BI_IFIRE,
    BI_IACID,
    BI_ICOLD,
    BI_IELEC,
    BI_RFIRE,
    BI_RCOLD,
    BI_RELEC,
    BI_RACID,
    BI_RPOIS,
    BI_RFEAR,
    BI_RLITE,
    BI_RDARK,
    BI_RBLIND,
    BI_RCONF,
    BI_RSND,
    BI_RSHRD,
    BI_RNXUS,
    BI_RNTHR,
    BI_RKAOS,
    BI_RDIS,
    BI_HLIFE,
    BI_FRACT,
    BI_SRFIRE,
    BI_SRCOLD,
    BI_SRELEC,
    BI_SRACID,
    BI_SRPOIS,
    BI_SRFEAR,
    BI_SRLITE,
    BI_SRDARK,
    BI_SRBLIND,
    BI_SRCONF,
    BI_SRSND,
    BI_SRSHRD,
    BI_SRNXUS,
    BI_SRNTHR,
    BI_SRKAOS,
    BI_SRDIS,
    BI_SHLIFE,
    BI_SFRACT,
    BI_DEPTH,
    BI_CDEPTH,
    BI_ARMOR,
    BI_TOHIT,
    BI_TODAM,
    BI_WTOHIT,
    BI_WTODAM,
    BI_BTOHIT,
    BI_BTODAM,
    BI_BLOWS,
    BI_SHOTS,
    BI_WMAXDAM,
    BI_WBASEDAM,
    BI_BMAXDAM,
    BI_HEAVYWEPON,
    BI_HEAVYBOW,
    BI_CRSTELE,
    BI_CRSAGRV,
    BI_WS_ANIMAL,
    BI_WS_EVIL,
    BI_WS_UNDEAD,
    BI_WS_DEMON,
    BI_WS_ORC,
    BI_WS_TROLL,
    BI_WS_GIANT,
    BI_WS_DRAGON,
    BI_WK_DRAGON,
    BI_W_IMPACT,
    BI_WB_ACID,
    BI_WB_ELEC,
    BI_WB_FIRE,
    BI_WB_COLD,
    BI_ATELEPORT,
    BI_AESCAPE,
    BI_AFUEL,
    BI_AHEAL,
    BI_AEZHEAL,
    BI_AID,
    BI_ASPEED,
    BI_ASTFMAGI,
    BI_ASTFDEST,
    BI_AMISSILES,
    BI_ACUREPOIS,
    BI_ADETTRAP,
    BI_ADETDOOR,
    BI_ADETEVIL,
    BI_AMAGICMAP,
    BI_ARECHARGE,
    BI_APFE,
    BI_AGLYPH,
    BI_ACCW,
    BI_ACSW,
    BI_ARESHEAT,
    BI_ARESCOLD,
    BI_ATELEPORTLVL,  /* scroll of teleport level */
    BI_AXGOI,            /* reliable GOI spell */
    BI_AGOI,            /* GOI spell */
    BI_AHWORD,            /* Holy Word prayer */

    BI_MAX
};

#define MAX_FORMULA_ELEMENTS 60
enum
{
    BFO_DONE, /* just to make sure we end fast if there is no formula */
    BFO_NUMBER,
    BFO_VARIABLE,
    BFO_EQ,
    BFO_NEQ,
    BFO_NOT,
    BFO_LT,
    BFO_LTE,
    BFO_GT,
    BFO_GTE,
    BFO_AND,
    BFO_OR,
    BFO_PLUS,
    BFO_MINUS,
    BFO_DIVIDE,
    BFO_MULT
};

extern int *formula[1000];
extern cptr prefix_pref[];

/*
 * Hack -- optional cheating flags
 */

extern bool auto_cheat_equip;       /* Cheat for "equip mode" */

extern bool auto_cheat_inven;       /* Cheat for "inven mode" */

extern bool auto_cheat_spell;       /* Cheat for "browse mode" */

extern bool auto_cheat_panel;       /* Cheat for "panel mode" */

extern bool auto_do_star_id;


/*
 * Various silly flags
 */

extern bool auto_flag_save;     /* Save savefile at each level */

extern bool auto_flag_dump;     /* Save savefile at each death */

extern bool borg_save; /* do a save next time we get to press a key! */

extern bool auto_borg_message;      /* List borg messages or not */
extern bool borg_graphics;          /* List borg messages or not */
extern bool borg_confirm_target;

extern char borg_engine_date[];       /* last update */

/*
 * Use a simple internal random number generator
 */

extern bool auto_rand_quick;        /* Save system setting */

extern u32b auto_rand_value;        /* Save system setting */

extern u32b auto_rand_local;        /* Save personal setting */


/*
 * Hack -- time variables
 */

extern s16b c_t;        /* Current "time" */
extern s16b need_see_inviso;        /* To tell me to cast it */
extern s16b borg_see_inv;
extern bool need_shift_panel;        /* to spot offscreeners */
extern s16b when_shift_panel;
extern s16b time_this_panel;        /* Current "time" for current panel*/
extern bool vault_on_level;     /* borg will search for a vault */
extern bool unique_on_level;
extern bool scaryguy_on_level;

extern bool breeder_level;      /* Borg will shut doors */
extern s16b old_depth;
extern s16b borg_respawn;       /* to prevent certain crashes */
extern bool genocide_level_hounds; /* used against hounds */
extern s16b borg_hound_count;
extern s16b borg_no_retreat;

/*
 * Hack -- Other time variables
 */

extern s16b when_call_lite; /* When we last did call light */
extern s16b when_wizard_lite;   /* When we last did wizard light */

extern s16b when_detect_traps;  /* When we last detected traps */
extern s16b when_detect_doors;  /* When we last detected doors */
extern s16b when_detect_walls;  /* When we last detected walls */
extern s16b when_detect_evil;
extern s16b when_last_kill_mult;   /* When a multiplier was last killed */

extern bool my_need_alter;     /* incase of walls/doors */
extern bool my_no_alter;     /* incase of walls/doors */
extern bool my_need_redraw;     /* incase of walls/doors */
extern bool borg_attempting_refresh;  /* for the goi spell */

/*
 * Some information
 */

extern s16b goal;       /* Flowing (goal type) */

extern bool goal_rising;    /* Currently returning to town */

extern bool goal_leaving;   /* Currently leaving the level */

extern bool goal_fleeing;   /* Currently fleeing the level */

extern bool goal_ignoring;  /* Currently ignoring monsters */

extern bool goal_recalling; /* Currently waiting for recall */

extern s16b borg_times_twitch; /* how often twitchy on this level */
extern s16b borg_escapes; /* how often teleported on this level */

extern bool stair_less;     /* Use the next "up" staircase */
extern bool stair_more;     /* Use the next "down" staircase */

extern s32b auto_began;     /* When this level began */
extern s32b auto_time_town; /* how long it has been since I was in town */

extern s16b avoidance;      /* Current danger thresh-hold */

extern bool auto_failure;   /* Notice failure */

extern bool auto_simulate;  /* Simulation flag */
extern bool borg_attacking; /* Are we attacking a monster? */
extern bool borg_offsetting; /* Are we attacking a monster? with offsett balls*/

extern bool auto_completed; /* Completed the level */
extern bool borg_on_upstairs;      /* used when leaving a level */
extern bool borg_on_dnstairs;      /* used when leaving a level */

/* defence flags */
extern bool borg_prot_from_evil;
extern bool borg_speed;
extern bool borg_bless;
extern bool borg_hero;
extern bool borg_berserk;
extern bool my_oppose_fire;
extern bool my_oppose_cold;
extern bool my_oppose_acid;
extern bool my_oppose_pois;
extern bool my_oppose_elec;
extern s16b borg_goi;
extern s16b borg_game_ratio;

extern bool borg_yeeked;       /* Yeek Flee */
extern s16b borg_lock;         /* Ethereal Lock */
extern bool my_inviso;

extern bool borg_shield;
extern bool borg_on_glyph; /* borg is standing on a glyph of warding */
extern bool borg_create_door; /* borg is going to create doors */
extern bool borg_sleep_spell;
extern bool borg_sleep_spell_ii;
extern bool borg_slow_spell;
extern bool borg_confuse_spell;
extern bool borg_fear_mon_spell;


/*
 * Shop goals
 */

extern s16b goal_shop;      /* Next shop to visit */
extern s16b goal_ware;      /* Next item to buy there */
extern s16b goal_item;      /* Next item to sell there */

/*
 * Other variables
 */

extern int w_x;         /* Current panel offset (X) */
extern int w_y;         /* Current panel offset (Y) */

extern int c_x;         /* Current location (X) */
extern int c_y;         /* Current location (Y) */

extern int g_x;         /* Goal location (X) */
extern int g_y;         /* Goal location (Y) */

extern int bad_obj_x[50];   /* Dropped cursed artifact at location (X) */
extern int bad_obj_y[50];   /* Dropped cursed artifact at location (Y) */


/*
 * State variables extracted from the screen
 */

extern bool do_weak;        /* Currently Weak */
extern bool do_hungry;      /* Currently Hungry */

extern bool do_full;        /* Currently Full */
extern bool do_gorged;      /* Currently Gorged */

extern bool do_blind;       /* Currently Blind */
extern bool do_afraid;      /* Currently Afraid */
extern bool do_confused;    /* Currently Confused */
extern bool do_poisoned;    /* Currently Poisoned */
extern bool do_nasty_pois; /* TM's curse */

extern bool do_cut;         /* Currently bleeding */
extern bool do_stun;        /* Currently stunned */
extern bool do_heavy_stun;      /* Currently stunned */

extern bool do_image;       /* May be hallucinating */
extern bool do_study;       /* May learn spells */

extern bool do_fix_lev;     /* Drained LEV */
extern bool do_fix_exp;     /* Drained EXP */

extern bool do_fix_stat[6]; /* Drained Stats */



/*
 * Some estimated state variables
 */

extern s16b my_stat_max[6]; /* Current "maximal" stat values    */
extern s16b my_stat_cur[6]; /* Current "natural" stat values    */
extern s16b my_stat_use[6]; /* Current "resulting" stat values  */
extern s16b my_stat_ind[6]; /* Current "additions" to stat values   */
extern bool my_need_stat_check[6];  /* do I need to check my stats */

extern s16b my_stat_add[6];  /* aditions to stats */

extern s16b home_stat_add[6];

extern int  weapon_swap;   /* location of my swap weapon   */
extern s32b weapon_swap_value;   /* value of my swap weapon   */
extern int  armour_swap;   /* location of my swap weapon   */
extern s32b armour_swap_value;   /* value of my swap weapon   */

/* a 3 state boolean */
/*-1 = not cursed, no help needed for it */
/* 0 = light curse, needs light remove curse spell */
/* 1 = heavy curse, needs heavy remove curse spell */
extern int decurse_weapon_swap;  /* my swap is great, except its cursed */
extern int enchant_weapon_swap_to_h;  /* my swap is great, except its cursed */
extern int enchant_weapon_swap_to_d;  /* my swap is great, except its cursed */
extern int decurse_armour_swap;  /* my swap is great, except its cursed */
extern int enchant_armour_swap_to_a;  /* my swap is great, except its cursed */
extern bool borg_wearing_cursed;

extern s16b weapon_swap_digger;
extern byte  weapon_swap_slay_animal;
extern byte  weapon_swap_slay_evil;
extern byte  weapon_swap_slay_undead;
extern byte  weapon_swap_slay_demon;
extern byte  weapon_swap_slay_orc;
extern byte  weapon_swap_slay_troll;
extern byte  weapon_swap_slay_giant;
extern byte  weapon_swap_slay_dragon;
extern byte  weapon_swap_kill_dragon;
extern byte  weapon_swap_impact;
extern byte  weapon_swap_brand_acid;
extern byte  weapon_swap_brand_elec;
extern byte  weapon_swap_brand_fire;
extern byte  weapon_swap_brand_cold;
extern byte  weapon_swap_see_infra;
extern byte  weapon_swap_slow_digest;
extern byte  weapon_swap_aggravate;
extern byte  weapon_swap_teleport;
extern byte  weapon_swap_regenerate;
extern byte  weapon_swap_telepathy;
extern byte  weapon_swap_lite;
extern byte  weapon_swap_see_invis;
extern byte  weapon_swap_ffall;
extern byte  weapon_swap_free_act;
extern byte  weapon_swap_hold_life;
extern byte  weapon_swap_immune_fire;
extern byte  weapon_swap_immune_acid;
extern byte  weapon_swap_immune_cold;
extern byte  weapon_swap_immune_elec;
extern byte  weapon_swap_resist_acid;
extern byte  weapon_swap_resist_elec;
extern byte  weapon_swap_resist_fire;
extern byte  weapon_swap_resist_cold;
extern byte  weapon_swap_resist_pois;
extern byte  weapon_swap_resist_conf;
extern byte  weapon_swap_resist_sound;
extern byte  weapon_swap_resist_lite;
extern byte  weapon_swap_resist_dark;
extern byte  weapon_swap_resist_chaos;
extern byte  weapon_swap_resist_disen;
extern byte  weapon_swap_resist_shard;
extern byte  weapon_swap_resist_nexus;
extern byte  weapon_swap_resist_blind;
extern byte  weapon_swap_resist_neth;
extern byte  weapon_swap_resist_fear;
extern byte  armour_swap_slay_animal;
extern byte  armour_swap_slay_evil;
extern byte  armour_swap_slay_undead;
extern byte  armour_swap_slay_demon;
extern byte  armour_swap_slay_orc;
extern byte  armour_swap_slay_troll;
extern byte  armour_swap_slay_giant;
extern byte  armour_swap_slay_dragon;
extern byte  armour_swap_kill_dragon;
extern byte  armour_swap_impact;
extern byte  armour_swap_brand_acid;
extern byte  armour_swap_brand_elec;
extern byte  armour_swap_brand_fire;
extern byte  armour_swap_brand_cold;
extern byte  armour_swap_see_infra;
extern byte  armour_swap_slow_digest;
extern byte  armour_swap_aggravate;
extern byte  armour_swap_teleport;
extern byte  armour_swap_regenerate;
extern byte  armour_swap_telepathy;
extern byte  armour_swap_lite;
extern byte  armour_swap_see_invis;
extern byte  armour_swap_ffall;
extern byte  armour_swap_free_act;
extern byte  armour_swap_hold_life;
extern byte  armour_swap_immune_fire;
extern byte  armour_swap_immune_acid;
extern byte  armour_swap_immune_cold;
extern byte  armour_swap_immune_elec;
extern byte  armour_swap_resist_acid;
extern byte  armour_swap_resist_elec;
extern byte  armour_swap_resist_fire;
extern byte  armour_swap_resist_cold;
extern byte  armour_swap_resist_pois;
extern byte  armour_swap_resist_conf;
extern byte  armour_swap_resist_sound;
extern byte  armour_swap_resist_lite;
extern byte  armour_swap_resist_dark;
extern byte  armour_swap_resist_chaos;
extern byte  armour_swap_resist_disen;
extern byte  armour_swap_resist_shard;
extern byte  armour_swap_resist_nexus;
extern byte  armour_swap_resist_blind;
extern byte  armour_swap_resist_neth;
extern byte  armour_swap_resist_fear;

extern byte my_ammo_tval;   /* Ammo -- "tval"   */
extern byte my_ammo_sides;  /* Ammo -- "sides"  */
extern s16b my_ammo_power;  /* Shooting multipler   */
extern s16b my_ammo_range;  /* Shooting range   */

extern s16b my_need_enchant_to_a;   /* Need some enchantment */
extern s16b my_need_enchant_to_h;   /* Need some enchantment */
extern s16b my_need_enchant_to_d;   /* Need some enchantment */
extern s16b my_need_brand_weapon;  /* apw brand bolts */


/*
 * Hack -- basic "power"
 */

extern s32b my_power;


/*
 * Various "amounts" (for the player)
 */

extern s16b amt_phase;
extern s16b amt_food_lowcal;
extern s16b amt_food_hical;

extern s16b amt_ez_heal_life;
extern s16b amt_ez_heal_star;
extern s16b amt_rod_heal;       /* used by warriors and rogues */
extern s16b amt_pot_heal;       /* need real potions for morgy */
extern s16b amt_mana;
extern s16b amt_slow_poison;
extern s16b amt_cure_confusion;
extern s16b amt_cure_blind;

extern s16b amt_cool_staff;  /* holiness-power staff */

extern s16b amt_book[9];

extern s16b amt_add_stat[6];
extern s16b amt_fix_stat[7];

extern s16b amt_fix_exp;

extern s16b amt_enchant_to_a;
extern s16b amt_enchant_to_d;
extern s16b amt_enchant_to_h;
extern s16b amt_brand_weapon;  /* cubragol and bolts */
extern s16b amt_enchant_weapon;
extern s16b amt_enchant_armor;
extern s16b amt_digger;

/*
 * Various "amounts" (for the home)
 */

extern s16b num_food;
extern s16b num_mold;
extern s16b num_ident;
extern s16b num_star_ident;
extern s16b num_recall;
extern s16b num_phase;
extern s16b num_escape;
extern s16b num_teleport;
extern s16b num_berserk;
extern s16b num_teleport_level;

extern s16b num_cure_critical;
extern s16b num_cure_serious;

extern s16b num_pot_rheat;
extern s16b num_pot_rcold;

extern s16b num_missile;

extern s16b num_book[9];

extern s16b num_fix_stat[7];

extern s16b num_fix_exp;
extern s16b num_mana;
extern s16b num_heal;
extern s16b num_heal_true;
extern s16b num_ez_heal;
extern s16b num_ez_heal_true;
extern s16b num_pfe;
extern s16b num_glyph;
extern s16b num_speed;

extern s16b num_enchant_to_a;
extern s16b num_enchant_to_d;
extern s16b num_enchant_to_h;
extern s16b num_brand_weapon;  /* apw crubragol and bolts */
extern s16b num_genocide;
extern s16b num_mass_genocide;

extern s16b num_artifact;

extern s16b home_slot_free;
extern s16b home_damage;
extern s16b num_duplicate_items;
extern s16b num_slow_digest;
extern s16b num_regenerate;
extern s16b num_telepathy;
extern s16b num_lite;
extern s16b num_see_inv;

extern s16b num_invisible; /*apw*/

extern s16b num_ffall;
extern s16b num_free_act;
extern s16b num_hold_life;
extern s16b num_immune_acid;
extern s16b num_immune_elec;
extern s16b num_immune_fire;
extern s16b num_immune_cold;
extern s16b num_resist_acid;
extern s16b num_resist_elec;
extern s16b num_resist_fire;
extern s16b num_resist_cold;
extern s16b num_resist_pois;
extern s16b num_resist_conf;
extern s16b num_resist_sound;
extern s16b num_resist_lite;
extern s16b num_resist_dark;
extern s16b num_resist_chaos;
extern s16b num_resist_disen;
extern s16b num_resist_shard;
extern s16b num_resist_nexus;
extern s16b num_resist_blind;
extern s16b num_resist_neth;
extern s16b num_sustain_str;
extern s16b num_sustain_int;
extern s16b num_sustain_wis;
extern s16b num_sustain_dex;
extern s16b num_sustain_con;
extern s16b num_sustain_all;

extern s16b num_speed;
extern s16b num_edged_weapon;
extern s16b num_bad_gloves;
extern s16b num_weapons;
extern s16b num_bow;
extern s16b num_rings;
extern s16b num_neck;
extern s16b num_armor;
extern s16b num_cloaks;
extern s16b num_shields;
extern s16b num_hats;
extern s16b num_gloves;
extern s16b num_boots;


/*
 * Hack -- extra state variables
 */

extern int auto_feeling;    /* Current level "feeling" */

extern int auto_max_level;  /* Maximum player level */
extern int auto_max_depth;  /* Maximum dungeon depth */



/*
 * Hack -- current shop index
 */

extern s16b shop_num;       /* Current shop index */



/*
 * State variables extracted from the screen
 */

extern int auto_depth;      /* Current dungeon "level" */

extern bool borg_king;      /* We won! */

extern s32b auto_exp;       /* Current experience */

extern s32b auto_gold;      /* Current gold */

extern int auto_speed;      /* Current speed */

extern int auto_ac;     /* Current armor */

extern int auto_mhp;        /* Maximum hitpoints */
extern int auto_chp;        /* Current hitpoints */

extern int auto_msp;        /* Maximum spell points */
extern int auto_csp;        /* Current spell points */

extern int auto_stat[6];    /* Current stats */

extern int auto_book[9];    /* Current book slots */


/*
 * State variables extracted from the inventory/equipment
 */

extern int auto_cur_wgt;    /* Current weight */


/*
 * Constant state variables
 */

extern int auto_race;       /* Current race */
extern int auto_class;      /* Current class */



/*
 * Constant state structures
 */

extern player_race *rb_ptr; /* Player race info */
extern player_class *cb_ptr;    /* Player class info */
extern player_magic *mb_ptr;    /* Player magic info */


extern void mmove2(int *y, int *x, int y1, int x1, int y2, int x2);

/*
 * Number of turns to step for (zero means forever)
 */
extern u16b auto_step;      /* Step count (if any) */


/*
 * Status message search string
 */
extern char auto_match[128];    /* Search string */


/*
 * Log file
 */
extern FILE *auto_fff;      /* Log file */


/*
 * Hack -- single character constants
 */

extern const char p1, p2, c1, c2, b1, b2;


/*
 * Hack -- the detection arrays
 */

extern bool auto_detect_wall[6][6];

extern bool auto_detect_trap[6][6];

extern bool auto_detect_door[6][6];

extern bool auto_detect_evil[6][6];

/*
 * Locate the store doors
 */

extern byte *track_shop_x;
extern byte *track_shop_y;


/*
 * Track "stairs up"
 */

extern s16b track_less_num;
extern s16b track_less_size;
extern byte *track_less_x;
extern byte *track_less_y;


/*
 * Track "stairs down"
 */

extern s16b track_more_num;
extern s16b track_more_size;
extern byte *track_more_x;
extern byte *track_more_y;

/*
 * Track glyphs
 */
extern s16b track_glyph_num;
extern s16b track_glyph_size;
extern byte *track_glyph_x;
extern byte *track_glyph_y;

/*
 * Track steps
 */
extern s16b track_step_num;
extern s16b track_step_size;
extern byte *track_step_x;
extern byte *track_step_y;

/*
 * Track closed doors
 */
extern s16b track_door_num;
extern s16b track_door_size;
extern byte *track_door_x;
extern byte *track_door_y;

/*
 * The object list.  This list is used to "track" objects.
 */

extern s16b auto_takes_cnt;

extern s16b auto_takes_nxt;

extern auto_take *auto_takes;


/*
 * The monster list.  This list is used to "track" monsters.
 */

extern s16b auto_kills_cnt;
extern s16b auto_kills_summoner;   /* index of a summoning guy */
extern s16b auto_kills_nxt;

extern auto_kill *auto_kills;


/*
 * Hack -- depth readiness
 */
extern int borg_ready_morgoth;

/*
 * Hack -- extra fear per "region"
 */

extern u16b auto_fear_region[6][18];


/*
 * Hack -- count racial appearances per level
 */

extern s16b *auto_race_count;


/*
 * Hack -- count racial kills (for uniques)
 */

extern s16b *auto_race_death;


/*
 * Classification of map symbols
 */

extern bool auto_is_take[256];      /* Symbol may be an object */

extern bool auto_is_kill[256];      /* Symbol may be a monster */


/*
 * Current "grid" list
 */

extern auto_grid *auto_grids[AUTO_MAX_Y];   /* Current "grid list" */


#ifdef BORG_ROOMS

/*
 * Some "room" variables
 */

extern int auto_room_max;       /* First totally free room */

extern auto_room *auto_rooms;       /* Current "room list" */

extern auto_room *auto_room_head;   /* &auto_rooms[0] */

extern auto_room *auto_room_tail;   /* &auto_rooms[AUTO_MAX_ROOMS-1] */


#endif


/*
 * Maintain a set of grids (liteable grids)
 */

extern s16b auto_lite_n;
extern byte auto_lite_y[AUTO_LITE_MAX];
extern byte auto_lite_x[AUTO_LITE_MAX];

/*
 * Maintain a set of glow grids (liteable grids)
 */

extern s16b auto_glow_n;
extern byte auto_glow_y[AUTO_LITE_MAX];
extern byte auto_glow_x[AUTO_LITE_MAX];


/*
 * Maintain a set of grids (viewable grids)
 */

extern s16b auto_view_n;
extern byte auto_view_y[AUTO_VIEW_MAX];
extern byte auto_view_x[AUTO_VIEW_MAX];


/*
 * Maintain a set of grids (scanning arrays)
 */

extern s16b auto_temp_n;
extern byte auto_temp_y[AUTO_TEMP_MAX];
extern byte auto_temp_x[AUTO_TEMP_MAX];

extern byte offset_y;
extern byte offset_x;


/*
 * Maintain a set of grids (flow calculations)
 */

extern s16b auto_flow_n;
extern byte auto_flow_y[AUTO_FLOW_MAX];
extern byte auto_flow_x[AUTO_FLOW_MAX];


/*
 * Hack -- use "flow" array as a queue
 */

extern int flow_head;
extern int flow_tail;


/*
 * Some variables
 */

extern auto_data *auto_data_flow;   /* Current "flow" data */

extern auto_data *auto_data_cost;   /* Current "cost" data */

extern auto_data *auto_data_hard;   /* Constant "hard" data */

extern auto_data *auto_data_know;   /* Current "know" flags */

extern auto_data *auto_data_icky;   /* Current "icky" flags */


/*
 * Strategy flags -- recalculate things
 */

extern bool auto_danger_wipe;       /* Recalculate danger */

extern bool auto_update_view;       /* Recalculate view */

extern bool auto_update_lite;       /* Recalculate lite */


/*
 * Strategy flags -- examine the world
 */

extern bool auto_do_inven;      /* Acquire "inven" info */

extern bool auto_do_equip;      /* Acquire "equip" info */

extern bool auto_do_panel;      /* Acquire "panel" info */

extern bool auto_do_frame;      /* Acquire "frame" info */

extern bool auto_do_spell;      /* Acquire "spell" info */

extern byte auto_do_spell_aux;      /* Hack -- book for "auto_do_spell" */

extern bool auto_do_browse;     /* Acquire "store" info */

extern byte auto_do_browse_what;    /* Hack -- store for "auto_do_browse" */

extern byte auto_do_browse_more;    /* Hack -- pages for "auto_do_browse" */


/*
 * Strategy flags -- run certain functions
 */

extern bool auto_do_crush_junk;

extern bool auto_do_crush_hole;

extern bool auto_do_crush_slow;

/* am I fighting a unique */
extern int borg_fighting_unique;
extern bool borg_fighting_evil_unique;

/* am I fighting a summoner */
extern bool borg_fighting_summoner;



/*** Some functions ***/

/*
 * Queue a keypress
 */
extern errr borg_keypress(char k);

/*
 * Queue several keypresses
 */
extern errr borg_keypresses(cptr str);

/*
 * Dequeue a keypress
 */
extern char borg_inkey(bool take);

/*
 * Flush the keypresses
 */
extern void borg_flush(void);


/*
 * Obtain some text from the screen (single character)
 */
extern errr borg_what_char(int x, int y, byte *a, char *c);

/*
 * Obtain some text from the screen (multiple characters)
 */
extern errr borg_what_text(int x, int y, int n, byte *a, char *s);


/*
 * Log a message to a file
 */
extern void borg_info(cptr what);

/*
 * Log a message, Search it, and Show/Memorize it in pieces
 */
extern void borg_note(cptr what);


/*
 * Abort the Borg, noting the reason
 */
extern void borg_oops(cptr what);


/*
 * Take a "memory note"
 */
extern bool borg_tell(cptr what);

/*
 * Change the player name
 */
extern bool borg_change_name(cptr str);

/*
 * Dump a character description
 */
extern bool borg_dump_character(cptr str);

/*
 * Save the game (but do not quit)
 */
extern bool borg_save_game(void);


/*
 * Update the "frame" info from the screen
 */
extern void borg_update_frame(void);

/*
 * Calc a formula out in RPN
 */
extern int borg_calc_formula(int *);
/*
 * check out a formula in RPN
 */
extern int borg_check_formula(int *);
/*
 * return a string for the formula
 */
extern cptr borg_prt_formula(int *formula);

/*
 * Print the string for an item
 */
extern cptr borg_prt_item(int item);

/*
 * Initialize this file
 */
extern void borg_init_1(void);

#ifdef ALLOW_BORG_GRAPHICS

typedef struct glyph
{
   byte d_attr;        /* Attribute */
   char d_char;        /* Character */
} glyph;


extern glyph translate_visuals[255][255];

#endif /* ALLOW_BORG_GRAPHICS */

#ifdef BORG_TK
extern cptr BORG_DIR_ROOT;
extern cptr BORG_DIR_DATA;
#endif /* BORG_TK */

#endif

#endif

