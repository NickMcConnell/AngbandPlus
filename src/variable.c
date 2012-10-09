/* File: variable.c */

/* Purpose: Angband variables */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Hack -- Link a copyright message into the executable
 */
cptr copyright[5] =
{
    "Copyright (c) 1989 James E. Wilson, Robert A. Keoneke",
    "",
    "This software may be copied and distributed for educational, research,",
    "and not for profit purposes provided that this copyright and statement",
    "are included in all such copies."
};

/*
 * how many debug messages do we want?
 */
u32b debuglevel;

/*
 * Hack -- Link the "version" into the executable
 */
byte version_major = VERSION_MAJOR;
byte version_minor = VERSION_MINOR;
byte version_patch = VERSION_PATCH;
byte version_extra = VERSION_EXTRA;

/*
 * Hack -- Savefile version
 */
byte sf_major;                  /* Savefile's "version_major" */
byte sf_minor;                  /* Savefile's "version_minor" */
byte sf_patch;                  /* Savefile's "version_patch" */
byte sf_extra;                  /* Savefile's "version_extra" */

/* jk */
bool force_templates = FALSE;   /* Hack -- force loading *.txt and */


/*
 * Hack -- Savefile information
 */
u32b sf_xtra;                   /* Operating system info */
u32b sf_when;                   /* Time when savefile created */
u16b sf_lives;                  /* Number of past "lives" with this file */
u16b sf_saves;                  /* Number of "saves" during this life */
char sf_lastfile_name[1024];    /* the name of the file with the last written level */

/*
 * Hack -- Run-time arguments
 */
bool arg_wizard;                /* Command arg -- Enter wizard mode */
bool arg_force_original;        /* Command arg -- Force original keyset */
bool arg_force_roguelike;       /* Command arg -- Force roguelike keyset */

bool character_generated;       /* The character exists */
bool character_dungeon;         /* The character has a dungeon */
bool character_loaded = FALSE;  /* The character was loaded from a savefile */
bool character_saved;           /* The character was just saved to a savefile */

bool character_icky;            /* The game is in an icky full screen mode */
bool character_xtra;            /* The game is in an icky startup mode */

/* jk */
char cmd_sliding;
s16b sliding_dir = -1;
s16b max_reading;

s16b energy_use;                /* Energy use this turn */

s16b choose_default;            /* Default contents of choice window */

bool create_up_stair;           /* Auto-create "up stairs" */
bool create_down_stair;         /* Auto-create "down stairs" */

bool msg_flag;                  /* Used in msg_print() for "buffering" */

bool alive;                     /* True if game is running */

bool death;                     /* True if player has died */
bool suicide;                   /* True if player has quit (make no ghost!) */
/* jk */
bool used_stairs;               /* did we use a stairway to get to this level */

s16b cur_hgt;                   /* Current dungeon height */
s16b cur_wid;                   /* Current dungeon width */

s16b num_repro;                 /* Current reproducer count */
s16b object_level;              /* Current object creation level */
s16b monster_level;             /* Current monster creation level */
s16b baselevel;                /* current main dungeon level */
s16b sublevel;                 /* current dungeon sub level */

s32b turn;                      /* Current game turn */
s32b old_turn;                  /* Turn when level began (feelings) */

bool wizard;                    /* Is the player currently in Wizard mode?  */
bool can_be_wizard;             /* Does the player have wizard permissions? */
u32b wizard_target = 0L;        /* used in forced level generation targets  */

u16b total_winner;              /* Semi-Hack -- Game has been won */

u16b panic_save;                /* Track some special "conditions" */
u16b noscore;                   /* Track various "cheating" conditions */

s16b signal_count = 0;          /* Hack -- Count interupts */

bool inkey_base;                /* See the "inkey()" function */
bool inkey_xtra;                /* See the "inkey()" function */
bool inkey_scan;                /* See the "inkey()" function */
bool inkey_flag;                /* See the "inkey()" function */

s16b coin_type;                 /* Hack -- force coin type */
bool opening_chest;             /* Hack -- prevent chest generation */

bool arg_graphics;              /* Command arg -- Request graphics mode */
bool arg_sound;                 /* Command arg -- Request special sounds */
bool use_sound;                 /* The "sound" mode is enabled */
bool use_graphics;              /* The "graphics" mode is enabled */

bool scan_monsters;             /* Hack -- optimize multi-hued code, etc */
bool scan_objects;              /* Hack -- optimize multi-hued code, etc */

s16b inven_nxt;                 /* Hack -- unused */

s16b inven_cnt;                 /* Number of items in inventory */
s16b equip_cnt;                 /* Number of items in equipment */

s16b i_nxt = 1;                 /* Object free scanner */
s16b mn_nxt = 1;                 /* Monster free scanner */

s16b i_max = 1;                 /* Object heap size */
s16b mn_max = 1;                 /* Monster heap size */

s16b i_top = 0;                 /* Object top size */
s16b mn_top = 0;                 /* Monster top size */
/* jk - t_list is kept differently from i_list or mn_list - better I think :-) */
s16b t_max = 1;                 /* trap heap size - start at 1 !! */
s16b is_max = 0;                /* inventory set max */

/* Software options (set via the '=' command).  See "tables.c" */

/* General options */

bool rogue_like_commands;       /* Use the rogue-like keyset */
bool quick_messages;            /* Clear "-more-" with any key */
bool other_query_flag;          /* Prompt before various actions */
/* jk */
bool display_coords;            /* display coordinates on screen */
bool print_experience_advance;  /* print the experience needed to advance */
bool carry_cursed_flag;         /* Never pick up cursed things */
bool always_pickup;             /* Pick things up by default */
bool always_throw;              /* Throw things without asking */
bool always_repeat;             /* Auto-repeat some commands */
bool use_old_target;            /* Use old target when possible */

bool show_equip_label;          /* Shop labels in equipment list */
bool depth_in_feet;             /* Display the depth in "feet" */
bool notice_seams;              /* Highlight mineral seams */

bool use_color;                 /* Use color if possible */

/* jk */
bool compress_savefile;         /* Compress the savefile as possible */
bool kill_savefile_interrupt;   /* Kill the savefile after pressing ^C */
bool remove_levelfiles;         /* Remove levelfiles if killed */
bool smooth_scroll_panels;      /* scroll panels by 1 cell every move */
bool number_hit_messages;       /* 1: you hit xxx 2: you hit xxx */
bool show_key_help;             /* show key help if you press a wrong key */

bool hilite_player;             /* Hilite the player */
bool ring_bell;                 /* Ring the bell */
bool view_yellow_lite;          /* Use "yellow" for "torch lite" */
bool view_bright_lite;          /* Use "bright" for (viewable) "perma-lite" */
/* jk */
bool view_all_squares;

bool view_granite_lite;         /* Use special colors for wall grids (slow) */
bool view_special_lite;         /* Use special colors for floor grids (slow) */

/* Option Set 2 -- Disturbance */

bool find_ignore_stairs;        /* Run past stairs */
bool find_ignore_doors;         /* Run through doors */
bool find_cut;                  /* Cut corners */
bool find_examine;              /* Examine corners */

bool disturb_near;              /* Disturbed by "local" motion */
bool disturb_move;              /* Disturbed by monster movement */
bool disturb_enter;             /* Disturbed by monster appearing */
bool disturb_leave;             /* Disturbed by monster disappearing */

bool disturb_panel;             /* Disturbed by map panel changing */
bool disturb_other;             /* Disturbed by various things happening */

bool flush_disturb;             /* Flush input on disturbance */
bool flush_failure;             /* Flush input on any failure */

bool fresh_before;              /* Flush output before normal commands */
bool fresh_after;               /* Flush output after normal commands */
bool fresh_message;             /* Flush output after all messages */
bool save_messages;             /* Save/append messages to disk */
bool alert_hitpoint;            /* Alert user to critical hitpoints */
bool alert_failure;             /* Alert user to various failures */
bool corpse_messages;           /* Alert the user to corpses decomposing etc */
bool fear_messages;             /* Alert the user to monster fear */
bool drop_messages;             /* Alert the user to objects dropped */
bool ask_before_traps;          /* always ask before walking on a known trap */


/* Gameplay options */

bool scum_always;               /* Auto-scum for good levels (always) */
bool scum_sometimes;            /* Auto-scum for good levels (sometimes) */
/* jk */
bool scum_verygood;             /* if scumming, get very good levels */

bool dungeon_align;             /* Generate dungeons with align rooms */
bool dungeon_connected;         /* Generate dungeons with align rooms */
bool generate_large_levels;     /* Don't generate extra-large levels */

bool view_perma_grids;          /* Map "remembers" perma-lit grids */
bool view_torch_grids;          /* Map "remembers" torch-lit grids */

bool flow_by_sound;             /* Monsters track new player location */
bool flow_by_smell;             /* Monsters track old player location */

bool track_follow;              /* Monsters follow the player */
bool track_target;              /* Monsters target the player */

bool smart_learn;               /* Monsters learn from their mistakes */
bool smart_cheat;               /* Monsters exploit player weaknesses */
bool monster_flee_exits;       /* Monsters flee towards exits from a room */
bool monster_know_exits;       /* Monsters flee toward exits they can't see */

bool no_haggle_flag;            /* Cancel haggling */
/* jk */
bool kill_cursed_floor;         /* TRUE: kills cursed items (fl) without conf */
bool kill_cursed_pack;          /* TRUE: kills cursed items (pk) without conf */
bool color_known_items;         /* TRUE: The Violet Speckled Potion of ... */
bool pickup_add_to_ammo;        /* add items automatically to the invem_ammo */
bool ask_for_other_ammo;        /* ask what ammo to use when firing */
bool stacking_wipes_logs;       /* wipe log when stacking items or not */
bool show_spell_numbers;        /* show spell numbers in books */
bool show_full_name_on_destroy; /* Show the full name after you've destroyed something */
bool stack_ignore_logs;         /* ignores logs when stacking items */
bool auto_open;                 /* TRUE: auto-open closed doors when walking */
                                /* against them */
bool auto_target_only_monster;  /* auto-target only monster in sight */
bool create_corpses;            /* create corpses on monster death */
bool good_store_items;          /* use extra CPU cycles to create good store items */
bool pick_up_gold;
bool pick_up_absorbable;

bool shuffle_owners;            /* Shuffle store owners occasionally */

bool show_health_bar;           /* Show monster health bar */

bool show_inven_weight;         /* Show weights in inven */
bool show_equip_weight;         /* Show weights in equip */
bool show_floor_weight;
bool show_store_weight;         /* Show weights in store */

bool stack_allow_items;         /* Allow weapons and armor and such to stack */
bool stack_allow_corpses;       /* Allow corpses to stack */
bool stack_force_notes;         /* Force items with different notes to stack */
bool stack_force_costs;         /* Force items with different costs to stack */


/* Efficiency options */

bool view_reduce_lite;          /* Reduce lite radius when running */
bool view_reduce_view;          /* Reduce view radius in town */

bool optimize_display;          /* Optimize various things (visual display) */
bool optimize_various;          /* Optimize various things (message recall) */
bool save_levels;               /* persistent savefiles? */

/* Special options */

bool use_mirror_debug;          /* Use "mirror" window -- debug messages */

bool use_mirror_around;         /* Use "mirror" window -- auto-mapping */

bool use_mirror_recent;         /* Use "mirror" window -- recent monsters */

bool use_mirror_normal;         /* Use "mirror" window -- current stuff */
bool use_mirror_choose;         /* Use "mirror" window -- show "choices" */
bool use_mirror_spells;         /* Use "mirror" window -- show "spells" */

bool use_recall_recent;         /* Use "recall" window -- recent monsters */

bool use_choice_normal;         /* Use "choice" window -- current stuff */
bool use_choice_choose;         /* Use "choice" window -- show "choices" */
bool use_choice_spells;         /* Use "choice" window -- show "spells" */

bool show_choose_info;          /* Show info in windows when "choosing" */
bool show_choose_prompt;        /* Show prompt in windows when "choosing" */
bool show_choose_weight;        /* Show weights in windows when "choosing" */
bool show_choose_label;         /* Show labels in windows when "choosing"  */

bool recall_show_desc;          /* Show monster descriptions when "recalling" */
bool recall_show_kill;          /* Show monster kill info when "recalling" */
bool show_unkilled;             /* Show unkilled monsters in kill list */

/* Cheating options */

bool cheat_spell_info;          /* Cheat -- view more spell info */
bool cheat_peek;                /* Cheat -- note object creation */
bool cheat_hear;                /* Cheat -- note monster creation */
bool cheat_room;                /* Cheat -- note dungeon creation */
bool cheat_xtra;                /* Cheat -- note something else */
bool cheat_know;                /* Cheat -- complete monster recall */
bool cheat_live;                /* Cheat -- allow death avoidance */
bool cheat_hitpoints;           /* Cheat -- allow unlimited hitpoints */
bool cheat_mode;                /* Cheat -- allow extra 'C' screens */
bool cheat_numeric_skills;      /* Cheat -- allow player to see numeric skill values */

s16b hitpoint_warn;             /* Hitpoint warning (0 to 9) */

s16b delay_spd;                 /* Delay factor (0 to 9) */

bool term_initialized;          /* is term interaction possible? */

s16b feeling;                   /* Most recent feeling */
bool store_built;               /* is there a store on the level */
s16b rating;                    /* Level's current rating */
bool monsters_with_artifacts;   /* Are there any monsters with artifacts on this level */
bool good_item_flag;            /* True if "Artifact" on this level */
bool new_level_flag;            /* Start a new level */
bool closing_flag;              /* Dungeon is closing */

/*
 * Dungeon size info
 */
s16b panel_max_rows, panel_max_cols;
s16b panel_min_row, panel_max_row;
s16b panel_min_col, panel_max_col;
s16b panel_prt_col, panel_prt_row;

/* Player location in dungeon */
s16b py;
s16b px;

/* Targetting variables */
s16b target_who;
s16b target_col;
s16b target_row;

/* Health bar variable -DRS- */
s16b health_who;

/*
 * Monster moves prediction array
 *
 */
monster_move_type mmove[9];

/*
 * The player's UID and GID
 */
s16b player_uid = 0;
s16b player_euid = 0;
s16b player_egid = 0;

/* the exp factor obtained from fiddling with the starting stats.. */
s16b birth_stat_exp = 0;

/* Current player's character name */
char player_name[32];

/* Stripped version of "player_name" */
char player_base[32];

/* What killed the player */
char died_from[80];

/* What are the latest kills */
coord last_kills[25];

/* Hack -- Textual "history" for the Player */
char history[MAX_HIST][70];

/*
 * teaching gets you something
 */
s16b teach_dis, teach_dev, teach_sav, teach_stl;
s16b teach_srh, teach_pcp, teach_thn, teach_thb;
s16b teach_stat[6];
char teacher[4][80];

/* Buffer to hold the current savefile name */
char savefile[1024];

/* jk - buffer to hold current level savefile name */
char levelfile[1024];
ghost_type ghost_info[MAX_GHOSTS];


/* jk - variables needed in the arena - defined here because saving */
/* a game in the middle should be possible                          */
bool been_in_arena = FALSE;
s32b arena_reward = -1;
s32b arena_previous_monsters = 0;
s32b arena_monsters = 0;
s32b arena_monster_level;
s16b arena_visit_level[PY_MAX_LEVEL+1];
u32b compress_length;

/* Array of grids lit by player lite (see "cave.c") [LITE_MAX] */
s16b lite_n;
/* jk - made s16b of these */
s16b lite_y[LITE_MAX];
s16b lite_x[LITE_MAX];

/* Array of grids viewable to the player (see "cave.c") [VIEW_MAX] */
s16b view_n;
/* jk - made s16b of these */
s16b view_y[VIEW_MAX];
s16b view_x[VIEW_MAX];
u16b view_g[VIEW_MAX];

/* Array of grids for use by various functions (see "cave.c") [TEMP_MAX] */
s16b temp_n;
s16b old_lite_n;
s16b temp_y[TEMP_MAX];
s16b temp_x[TEMP_MAX];
u16b old_lite_g[TEMP_MAX];

/* jk */
s16b tmp_x[TEMP_MAX];
s16b tmp_y[TEMP_MAX];
s16b tmp_i[TEMP_MAX];

/*
 * Number of active macros.
 */
s16b macro__num;

/*
 * Array of macro patterns [MACRO_MAX]
 */
cptr *macro__pat;

/*
 * Array of macro actions [MACRO_MAX]
 */
cptr *macro__act;

/*
 * Array of macro types [MACRO_MAX]
 */
bool *macro__cmd;

/*
 * Current (or recent) macro action
 */
char macro_buffer[1024];

/*
 * Keymaps for each "mode" associated with each keypress.
 */
cptr keymap_act[KEYMAP_MODES][256];

/*
 * The number of quarks
 */
s16b quark__num;

/*
 * The pointers to the quarks [QUARK_MAX]
 */
cptr *quark__str;


/*
 * The next "free" index to use
 */
u32b message__next;

/*
 * The index of the oldest message (none yet)
 */
u32b message__last;

/*
 * The next "free" offset
 */
u32b message__head;

/*
 * The offset to the previously stored message
 */
u32b message__prev;

/*
 * The offset to the oldest used char (none yet)
 */
u32b message__tail;

/*
 * The array of offsets, by index [MESSAGE_MAX]
 */
u32b *message__ptr;

/*
 * The array of chars, by offset [MESSAGE_BUF]
 */
char *message__buf;

/*
 * The array of counts, by offset [MESSAGE_BUF]
 */
byte *message__cnt;

/*
 * The array[8] of window pointers
 */
term *angband_term[8];


/*
 * The array[8] of window names (modifiable?)
 */
char angband_term_name[8][16] =
{
   "Angband",
   "Term-1",
   "Term-2",
   "Term-3",
   "Term-4",
   "Term-5",
   "Term-6",
   "Term-7"
};


/*
 * Global table of color definitions (mostly zeros)
 */
byte angband_color_table[256][4] =
{
   {0x00, 0x00, 0x00, 0x00},  /* TERM_DARK */
   {0x00, 0xFF, 0xFF, 0xFF},  /* TERM_WHITE */
   {0x00, 0x80, 0x80, 0x80},  /* TERM_SLATE */
   {0x00, 0xFF, 0x80, 0x00},  /* TERM_ORANGE */
   {0x00, 0xC0, 0x00, 0x00},  /* TERM_RED */
   {0x00, 0x00, 0x80, 0x40},  /* TERM_GREEN */
   {0x00, 0x00, 0x40, 0xFF},  /* TERM_BLUE */
   {0x00, 0x80, 0x40, 0x00},  /* TERM_BROWN */
   {0x00, 0x60, 0x60, 0x60},  /* TERM_L_DARK */
   {0x00, 0xC0, 0xC0, 0xC0},  /* TERM_L_WHITE */
   {0x00, 0xFF, 0x00, 0xFF},  /* TERM_VIOLET */
   {0x00, 0xFF, 0xFF, 0x00},  /* TERM_YELLOW */
   {0x00, 0xFF, 0x00, 0x00},  /* TERM_L_RED */
   {0x00, 0x00, 0xFF, 0x00},  /* TERM_L_GREEN */
   {0x00, 0x00, 0xFF, 0xFF},  /* TERM_L_BLUE */
   {0x00, 0xC0, 0x80, 0x40}   /* TERM_L_BROWN */
};


/*
 * Standard sound names (modifiable?)
 */
char angband_sound_name[SOUND_MAX][16] =
{
   "",
   "hit",
   "miss",
   "flee",
   "drop",
   "kill",
   "level",
   "death",
   "study",
   "teleport",
   "shoot",
   "quaff",
   "zap",
   "walk",
   "tpother",
   "hitwall",
   "eat",
   "store1",
   "store2",
   "store3",
   "store4",
   "dig",
   "opendoor",
   "shutdoor",
   "tplevel"
};

/*
 * The array of indexes of "live" objects
 */
s16b i_fast[MAX_I_IDX];

/*
 * The array of indexes of "live" monsters
 */
s16b mn_fast[MAX_M_IDX];

/* jk - the array of index of "live" traps */
s16b t_fast[MAX_TR_IDX];

/* jk - the array of level information */
level_info_type level_info[MAX_LEVEL];

/* jk - the array of dungeon information */
dungeon_info_type dungeon;

/*
 * The array of dungeon items [MAX_I_IDX]
 */
object_type *i_list;
spell_set_type *s_list;

/* jk */
trap_item_type *t_list;
item_set_type  *is_list;

/*
 * The array of dungeon monsters [MAX_M_IDX]
 */
monster_type *mn_list;

/*
 * Hack -- Quest array
 */
quest q_list[MAX_Q_IDX];


/*
 * The stores [MAX_STORES]
 */
/* jk - how many stores are active on this level */
u16b num_stores = 0;
store_type store[MAX_STORES_LEVEL];

/*
 * The player's inventory [INVEN_TOTAL]
 */
object_type *inventory;

/*
 * The size of "alloc_kind_table" (at most MAX_K_IDX * 4)
 */
s16b alloc_kind_size;

/*
 * The array[alloc_kind_size] of entries in the "kind allocator table"
 */
alloc_entry *alloc_kind_table;


/*
 * The size of "alloc_race_table" (at most MAX_R_IDX)
 */
s16b alloc_race_size;

/*
 * The array[alloc_race_size] of entries in the "race allocator table"
 */
alloc_entry *alloc_race_table;



/*
 * Specify attr/char pairs for visual special effects
 * Be sure to use "index & 0xFF" to avoid illegal access
 */
byte misc_to_attr[MAX_FLAVORS];
char misc_to_char[MAX_FLAVORS];

/*
 * Specify attr/char pairs for inventory items (by tval)
 * XXX XXX XXX Note the implied maximum "tval" of 128
 */
byte tval_to_attr[128];
char tval_to_char[128];

/*
 * Simple keymap method, see "init.c" and "cmd6.c".
 * XXX XXX XXX Note the implied maximum "key" of 128
 */
byte keymap_cmds[128];
byte keymap_dirs[128];


/*
 * Global table of color definitions
 */
byte color_table[256][4];


/*** Player information ***/

/*
 * The player other record (static)
 */
static player_other player_other_body;

/*
 * Pointer to the player other record
 */
player_other *op_ptr = &player_other_body;

/*
 * Static player info record
 */
static player_type p_body;

/*
 * Pointer to the player info
 */
player_type *p_ptr = &p_body;

/*
 * Pointer to the player tables (race, class, magic)
 */
player_sex *sp_ptr;
player_race *rp_ptr;
player_class *cp_ptr;

/*
 * Calculated base hp values for player at each level,
 * store them so that drain life + restore life does not
 * affect hit points.  Also prevents shameless use of backup
 * savefiles for hitpoint acquirement.
 */
s16b player_hp[PY_MAX_LEVEL];
u32b level_reached[PY_MAX_LEVEL];


/*
 * The vault generation arrays
 */
vault_type *v_info;
char *v_name;
char *v_text;
u32b v_name_size;
u32b v_text_size;
u16b v_number;

/*
 * The terrain feature arrays
 */
feature_type *f_info;
char *f_name;
char *f_text;
u32b f_name_size;
u32b f_text_size;
u16b f_number;
/* jk */
u16b f_info_index[MAX_F_IDX];

/*
 * The object kind arrays
 */
object_kind *k_info;
char *k_name;
char *k_text;
u32b k_name_size;
u32b k_text_size;
u16b k_number;

/*
 * The artifact arrays
 */
artifact_type *a_info;
char *a_name;
char *a_text;
u32b a_name_size;
u32b a_text_size;
u16b a_number;

/*
 * The ego-item arrays
 */
ego_item_type *e_info;
char *e_name;
char *e_text;
u32b e_name_size;
u32b e_text_size;
u16b e_number;

/* jk */
/* the trap-arrays */
trap_type *t_info;
char *t_name;
char *t_text;
u32b t_name_size;
u32b t_text_size;
u16b t_number;

/* jk */
/* the trap-arrays */
spell_type *s_info;
char *s_name;
char *s_text;
u32b s_name_size;
u32b s_text_size;
u16b s_number;

/*
 * The monster race arrays
 */
monster_race *r_info;
char *r_name;
char *r_text;
u32b r_name_size;
u32b r_text_size;
u32b r_name_size_total;
u32b r_text_size_total;
u16b r_number;       /* 0 ... rnumber-1 =             normal monsters */
u16b r_number_total; /* rnumber ... rnumber_total-1 = ghosts          */

/*
 * Hack -- The special Angband "System Suffix"
 * This variable is used to choose an appropriate "pref-xxx" file
 */
cptr ANGBAND_SYS = "xxx";

/*
 * Path name: The main "lib" directory
 * This variable is not actually used anywhere in the code
 */
cptr ANGBAND_DIR;

/*
 * High score files (binary)
 * These files may be portable between platforms
 */
cptr ANGBAND_DIR_APEX;

/*
 * Bone files for player ghosts (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_BONE;

/*
 * Binary image files for the "*_info" arrays (binary)
 * These files are not portable between platforms
 */
cptr ANGBAND_DIR_DATA;

/*
 * Textual template files for the "*_info" arrays (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_EDIT;

/*
 * Various extra files (ascii)
 * These files may be portable between platforms
 */
cptr ANGBAND_DIR_FILE;

/*
 * Help files (normal) for the online help (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_HELP;

/*
 * Help files (spoilers) for the online help (ascii)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_INFO;

/*
 * Savefiles for current characters (binary)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_SAVE;

/*
 * User "preference" files (ascii)
 * These files are rarely portable between platforms
 */
cptr ANGBAND_DIR_USER;

/*
 * Various extra files (binary)
 * These files are rarely portable between platforms
 */
cptr ANGBAND_DIR_XTRA;


/* jk */
cptr ANGBAND_DIR_TEMP;
char ANGBAND_ERRLOG_FILE[1024];
FILE *errlog = NULL;
FILE *wizardfile = NULL;
bool read_options;
char ANGBAND_OPTION_FILE[1024];
char prev_message[250];
u16b prev_message_cnt;


/*
 * Hack -- error tracking in template reading
 */
s16b error_idx;
s16b error_line;

/*
 * Total Hack -- allow all items to be listed (even empty ones)
 * This is only used by "do_cmd_inven_e()" and is cleared there.
 */
bool item_tester_full;


/*
 * Here is a "pseudo-hook" used during calls to "get_item()" and
 * "show_inven()" and "show_equip()", and the choice window routines.
 */
byte item_tester_tval;


/*
 * Here is a "hook" used during calls to "get_item()" and
 * "show_inven()" and "show_equip()", and the choice window routines.
 */
bool (*item_tester_hook)(object_type*);

/*
 * Current "comp" function for ang_sort()
 */
bool (*ang_sort_comp)(vptr u, vptr v, s16b a, s16b b);


/*
 * Current "swap" function for ang_sort()
 */
void (*ang_sort_swap)(vptr u, vptr v, s16b a, s16b b);

/*
 * Hack -- function hook to check "validity" of given race
 */
bool (*get_mon_num_hook)(s16b r_idx);
/* jk what SUMMON_ type is okay */
s16b summon_specific_type = 0;

/*
 * Hack -- function hook to check "validity" of given kind
 */
bool (*get_obj_num_hook)(s16b k_idx);
/* jk */
bool (*get_obj_num_hook2)(s16b k_idx);

/*
 * The type of object the item generator should make, if specified. -LM-
 */
byte required_tval = 0;

/*
 * The racial type of monster that the monster generator should
 * make, if specified.  -LM-
 */
char required_race = '\0';

event_type event[MAX_EVENTS];


