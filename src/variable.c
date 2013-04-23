#define VARIABLE_C
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

/* The name of the help index file */
cptr syshelpfile;

/*
 * Run-time arguments
 */
bool arg_fiddle = FALSE;			/* Command arg -- Request fiddle mode */
bool arg_wizard = FALSE;			/* Command arg -- Request wizard mode (unused) */
bool arg_sound = FALSE;				/* Command arg -- Request special sounds */
bool arg_graphics = FALSE;			/* Command arg -- Request graphics mode */
bool arg_force_original = FALSE;	/* Command arg -- Request original keyset */
bool arg_force_roguelike = FALSE;	/* Command arg -- Request roguelike keyset */

/*
 * Various things
 */

bool character_generated;	/* The character exists */
bool character_dungeon;		/* The character has a dungeon */
bool character_loaded;		/* The character was loaded from a savefile */
bool character_saved;		/* The character was just saved to a savefile */

bool character_icky;		/* The game is in an icky full screen mode */
bool character_xtra;		/* The game is in an icky startup mode */

u32b seed_flavor;		/* Hack -- consistent object colors */
u32b seed_wild;			/* Hack -- consistent wilderness layout */

s16b command_cmd;		/* Current "Angband Command" */

s16b command_arg;		/* Gives argument of current command */
s16b command_rep;		/* Gives repetition of current command */
s16b command_dir;		/* Gives direction of current command */

s16b command_see;		/* See "cmd1.c" */
s16b command_wrk;		/* See "cmd1.c" */

s16b command_gap = 50;	/* See "cmd1.c" */

s16b command_new;		/* Command chaining from inven/equip view */

s16b energy_use;		/* Energy use this turn */
s16b old_energy_use;		/* Energy use last turn */

bool create_up_stair;	/* Auto-create "up stairs" */
bool create_down_stair;	/* Auto-create "down stairs" */

bool msg_flag;			/* Used in msg_print() for "buffering" */

bool alive;				/* True if game is running */

bool death;				/* True if player has died */

s16b running;			/* Current counter for running, if any */
s16b resting;			/* Current counter for resting, if any */

s16b cur_hgt;			/* Current dungeon height */
s16b cur_wid;			/* Current dungeon width */
s16b dun_level;			/* Current dungeon level */
s16b dun_offset;       /* Monster/Object offset for current dungeon */
u16b dun_bias;			/* Summon flag used to give the dungeon a bias */
byte cur_town;          /* Current Town */
byte cur_dungeon;    /* Current Dungeon */
byte recall_dungeon; /* Last dungeon recalled from */
byte came_from;       /* Location player has come from onto this level */
s16b num_repro;			/* Current reproducer count */
s16b object_level;		/* Current object creation level */
s16b monster_level;		/* Current monster creation level */

s32b turn;				/* Current game turn */
s32b old_turn;			/* Turn when level began (feelings) */
s32b curse_turn;		/* Turn when autocurse activates */

bool cheat_wzrd;			/* Is the player currently in Wizard mode? */

bool use_sound;			/* The "sound" mode is enabled */
bool use_graphics;		/* The "graphics" mode is enabled */

u16b total_winner;		/* Semi-Hack -- Game has been won */

u16b panic_save;		/* Track some special "conditions" */
u16b noscore;			/* Track various "cheating" conditions */

bool use_transparency = FALSE; /* Use transparent tiles */

s16b signal_count;		/* Hack -- Count interupts */

bool inkey_base;		/* See the "inkey()" function */
bool inkey_scan;		/* See the "inkey()" function */
bool inkey_flag;		/* See the "inkey()" function */

s16b coin_type;			/* Hack -- force coin type */

bool opening_chest;		/* Hack -- prevent chest generation */

bool shimmer_monsters;	/* Hack -- optimize multi-hued monsters */
bool shimmer_objects;	/* Hack -- optimize multi-hued objects */

bool repair_monsters;	/* Hack -- optimize detect monsters */
bool repair_objects;	/* Hack -- optimize detect objects */

s16b total_weight;		/* Total weight being carried */

bool hack_mind;
bool hack_chaos_feature;
int artifact_bias;

s16b o_max = 1;			/* Number of allocated objects */
s16b o_cnt = 0;			/* Number of live objects */

s16b m_max = 1;			/* Number of allocated monsters */
s16b m_cnt = 0;			/* Number of live monsters */

s16b hack_m_idx = 0;	/* Hack -- see "process_monsters()" */
s16b hack_m_idx_ii = 0;
bool multi_rew = FALSE;

int total_friends = 0;
s32b total_friend_levels = 0;

/*
 * Software options (set via the '=' command).  See "tables.c"
 */


/* Option Set 1 -- User Interface */

bool inscribe_depth;			/* Inscribe a new object with the depth made at. */
bool rogue_like_commands;	/* Rogue-like commands */
bool quick_messages;		/* Activate quick messages */
bool quick_prompt;		/* Activate quick [y/n] prompts */
bool other_query_flag;		/* Prompt for various information */
bool carry_query_flag;		/* Prompt before picking things up */
bool use_old_target;		/* Use old target by default */
bool always_pickup;			/* Pick things up by default */
bool always_repeat;			/* Repeat obvious commands */
bool depth_in_feet;			/* Show dungeon level in feet */

bool stack_force_notes;		/* Merge inscriptions when stacking */
bool stack_force_notes_all;		/* Merge all inscriptions when stacking (inc. dissimilar ones) */
bool stack_force_costs;		/* Merge discounts when stacking */

bool show_labels;			/* Show labels in object listings */
bool show_weights;			/* Show weights in object listings */
bool show_choices;			/* Show choices in certain sub-windows */
bool show_details;			/* Show more detailed monster descriptons */
bool show_choices_main;			/* Show choices in main window */

bool ring_bell;				/* Ring the bell (on errors, etc) */
bool use_color;				/* Use color if possible (slow) */
bool verbose_haggle;		/* Verbose messages if auto_haggle is set */
bool scroll_edge;	/* Scroll until detection reaches the edge. */
bool show_piles; /* Show stacks with a special colour/character. */


/* Option Set 2 -- Disturbance */

bool find_ignore_stairs;	/* Run past stairs */
bool find_ignore_doors;		/* Run through open doors */
bool find_cut;				/* Run past known corners */
bool stop_corner;				/* Stop at corners */
bool find_examine;			/* Run into potential corners */

bool disturb_move;			/* Disturb whenever any monster moves */
bool disturb_near;			/* Disturb whenever viewable monster moves */
bool disturb_panel;			/* Disturb whenever map panel changes */
bool disturb_state;			/* Disturn whenever player state changes */
bool disturb_dawn;	/* Disturb at sunrise or sunset on the surface. */
bool disturb_minor;			/* Disturb whenever boring things happen */


bool alert_failure;		/* Alert user to various failures */
bool last_words;		/* Get last words upon dying */
bool small_levels;		/* Allow unusually small dungeon levels */
bool empty_levels;		/* Allow empty 'arena' levels */
bool player_symbols;		/* Use varying symbols for the player char */
bool equippy_chars;		/* Back by popular demand... */
bool skip_chaos_features;		/* Skip chaos feature screen even if we have it */
bool plain_descriptions;	/* Plain object descriptions */
bool stupid_monsters;		/* Monsters use old AI */
bool auto_destroy;		/* Known worthless items are destroyed without confirmation */
bool confirm_stairs;		/* Prompt before staircases... */
bool wear_confirm;		/* Confirm before putting on known cursed items */
bool confirm_wear_all;		/* Confirm before wearing items with unknown cursed status */
bool disturb_allies;		/* Allies moving nearby disturb us */
bool multi_stair;         /* Multiple level staircases */
bool unify_commands; /* Combine object commands into a single 'u'se command */
bool centre_view; /* Centre view on player */
bool macro_edit; /* Use macros as edit keys in string prompts */
bool no_centre_run; /* Stop centring when running */
bool track_mouse; /* Track the cursor in extra windows */
bool auto_more;

/* Option Set B -- Birth Options */

bool preserve_mode_w; 
bool preserve_mode; /* Don't lose missed artifacts */
bool maximise_mode_w;
bool maximise_mode; /* Unify stat bonuses */
bool use_autoroller; /* Autoroll characters */
bool spend_points; /* Spend points on stats */
bool ironman_shop_w;
bool ironman_shop; /* Not allowed in shops */
bool ironman_feeling_w;
bool ironman_feeling;	/* Only give real feeling after 2500 turns. */
bool speak_unique_w;
bool speak_unique;		/* Speaking uniques + shopkeepers */
#ifdef SCORE_QUITTERS
bool score_quitters_w;
bool score_quitters; /* Quitting can give a high score */
#endif /* SCORE_QUITTERS */

/* Option Set 3 -- Game-Play */

bool auto_haggle;			/* Auto-haggle in stores */

bool auto_scum;				/* Auto-scum for good levels */

bool stack_allow_items;		/* Allow weapons and armor to stack */
bool stack_allow_wands;		/* Allow wands/staffs/rods to stack */

bool expand_look;			/* Expand the power of the look command */
bool expand_list;			/* Expand the power of the list commands */

bool view_perma_grids;		/* Map remembers all perma-lit grids */
bool view_torch_grids;		/* Map remembers all torch-lit grids */

bool dungeon_align;			/* Generate dungeons with aligned rooms */
bool dungeon_stair;			/* Generate dungeons with connected stairs */
bool dungeon_small;         /* Generate dungeons with small levels always */

bool flow_by_sound;			/* Monsters track new player location */
bool flow_by_smell;			/* Monsters track old player location */

bool smart_learn;			/* Monsters learn from their mistakes */
bool smart_cheat;			/* Monsters exploit player weaknesses */


/* Option Set 4 -- Efficiency */

bool view_reduce_lite;		/* Reduce lite-radius when running */
bool view_reduce_view;		/* Reduce view-radius in town */

bool avoid_abort;			/* Avoid checking for user abort */
bool avoid_other;			/* Avoid processing special colors */

bool flush_error;			/* Flush input on incorrect keypresses. */
bool flush_failure;			/* Flush input on any failure */
bool flush_disturb;			/* Flush input on disturbance */
bool flush_command;			/* Flush input before every command */

bool fresh_before;			/* Flush output before normal commands */
bool fresh_after;			/* Flush output after normal commands */
bool fresh_message;			/* Flush output after all messages */

bool compress_savefile;		/* Compress messages in savefiles */

bool hilite_player;			/* Hilite the player with the cursor */

bool view_yellow_lite;		/* Use special colors for torch-lit grids */
bool view_bright_lite;		/* Use special colors for 'viewable' grids */

bool view_granite_lite;		/* Use special colors for wall grids (slow) */
bool view_special_lite;		/* Use special colors for floor grids (slow) */

/* Option set 5 -- Testing */

bool testing_stack;			/* Test the stacking code */

bool testing_carry;			/* Test the carrying code */


/* Spoiler options */
bool spoil_art;				/* Benefit from artefact spoilers */
bool spoil_mon;		/* Know complete monster info */
bool spoil_ego;		/* Know complete ego item info */
bool spoil_value;	/* Know the apparent prices of items */
bool spoil_base;	/* Know complete info about base items */
bool spoil_stat;	/* Know the significance of stat values */
bool spoil_dam;	/* Know the damage done by a melee weapon */
bool spoil_flag; /* Know the effects of various flags */

/* Cheating options */

bool cheat_peek;		/* Peek into object creation */
bool cheat_hear;		/* Peek into monster creation */
bool cheat_room;		/* Peek into dungeon creation */
bool cheat_xtra;		/* Peek into something else */
bool cheat_item;		/* Know complete item info */
bool cheat_live;		/* Allow player to avoid death */
bool cheat_skll;        /* Peek into skill rolls */

/* Special options */

bool allow_quickstart;	/* Allow Quick-Start */
#ifdef USE_MAIN_C
bool display_credits;	/* Require a keypress to clear the initial credit screen. */
#endif
bool allow_pickstats;	/* Allow the player to choose a stat template. */

s16b hitpoint_warn = 2;		/* Hitpoint warning (0 to 9) */

s16b delay_factor = 4;		/* Delay factor (0 to 9) */

bool autosave_l;        /* Autosave before entering new levels */
bool autosave_t;        /* Timed autosave */
bool autosave_q;        /* Quiet autosave */
s16b autosave_freq;     /* Autosave frequency */


/*
 * Dungeon variables
 */

s16b feeling;			/* Most recent feeling */
s16b rating;			/* Level's current rating */

bool good_item_flag;		/* True if "Artifact" on this level */

bool new_level_flag;		/* Start a new level */

int full_grid;	/* Monsters can't be created outside a circle of radius full_grid around the player */


/*
 * Dungeon size info
 */

s16b max_panel_rows;
s16b max_panel_cols;
s16b panel_row;
s16b panel_col;
s16b panel_row_min;
s16b panel_row_max;
s16b panel_col_min;
s16b panel_col_max;
s16b panel_col_prt;
s16b panel_row_prt;

/*
 * Player location in dungeon
 */
s16b py;
s16b px;

/*
 * Player location in wilderness
  */
s16b wildx;
s16b wildy;

/*
 * Targetting variables
 */
s16b target_who;
s16b target_col;
s16b target_row;

/*
 * Health bar variable -DRS-
 */
s16b health_who;

/*
 * Monster race to track
 */
s16b monster_race_idx;

/*
 * Object kind to track
 */
s16b object_kind_idx;

/*
 * Object to track
 */
object_type *tracked_o_ptr;

/*
 * Floor square to track
 */
co_ord tracked_co_ord;

/*
 * User info
 */
int player_uid;
int player_euid;
int player_egid;

/*
 * Current player's character name
 */
char player_name[NAME_LEN];

/*
 * Stripped version of "player_name"
 */
char player_base[NAME_LEN];

/*
 * What killed the player
 */
cptr died_from;

/*
 * Hack -- Textual "history" for the Player
 */
char history[4][60];

/*
 * Buffer to hold the current savefile name
 */
char savefile[1024]="";


/*
 * Array of grids lit by player lite (see "cave.c")
 */
s16b lite_n;
byte lite_y[LITE_MAX];
byte lite_x[LITE_MAX];

/*
 * Array of grids viewable to the player (see "cave.c")
 */
s16b view_n;
byte view_y[VIEW_MAX];
byte view_x[VIEW_MAX];

/*
 * Array of grids for use by various functions (see "cave.c")
 */
s16b temp_n;
byte temp_y[TEMP_MAX];
byte temp_x[TEMP_MAX];


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
 * Current macro action [1024]
 */
char *macro__buf;


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
u16b message__next;

/*
 * The index of the oldest message (none yet)
 */
u16b message__last;

/*
 * The next "free" offset
 */
u16b message__head;

/*
 * The offset to the oldest used char (none yet)
 */
u16b message__tail;

/*
 * The array of offsets, by index [MESSAGE_MAX]
 */
u16b *message__ptr;

/*
 * The array of chars, by offset [MESSAGE_BUF]
 */
char *message__buf;


/*
 * The array of normal options
 */
u32b option_flag[8];
u32b option_mask[8];

/*
 * Global table of color definitions
 */
byte angband_color_table[256][4] =
{
	{0x00, 0x00, 0x00, 0x00},	/* TERM_DARK */
	{0x00, 0xFF, 0xFF, 0xFF},	/* TERM_WHITE */
	{0x00, 0x80, 0x80, 0x80},	/* TERM_SLATE */
	{0x00, 0xFF, 0x80, 0x00},	/* TERM_ORANGE */
	{0x00, 0xC0, 0x00, 0x00},	/* TERM_RED */
	{0x00, 0x00, 0x80, 0x40},	/* TERM_GREEN */
	{0x00, 0x00, 0x00, 0xFF},	/* TERM_BLUE */
	{0x00, 0x80, 0x40, 0x00},	/* TERM_UMBER */
	{0x00, 0x40, 0x40, 0x40},	/* TERM_L_DARK */
	{0x00, 0xC0, 0xC0, 0xC0},	/* TERM_L_WHITE */
	{0x00, 0xFF, 0x00, 0xFF},	/* TERM_VIOLET */
	{0x00, 0xFF, 0xFF, 0x00},	/* TERM_YELLOW */
	{0x00, 0xFF, 0x00, 0x00},	/* TERM_L_RED */
	{0x00, 0x00, 0xFF, 0x00},	/* TERM_L_GREEN */
	{0x00, 0x00, 0xFF, 0xFF},	/* TERM_L_BLUE */
	{0x00, 0xC0, 0x80, 0x40}	/* TERM_L_UMBER */
};


/*
 * Standard sound names
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
	"tplevel",
	"scroll",
	"buy",
	"sell",
	"warn"
};


/*
 * The array of "cave grids" [MAX_WID][MAX_HGT].
 * Not completely allocated, that would be inefficient
 * Not completely hardcoded, that would overflow memory
 */
cave_type *cave[MAX_HGT];

/*
 * The array of dungeon items [MAX_O_IDX]
 */
object_type *o_list;

/*
 * The array of dungeon monsters [MAX_M_IDX]
 */
monster_type *m_list;

/*
 * The stores [MAX_STORES_TOTAL]
 */
store_type *store;

/*
 * The player's inventory [INVEN_TOTAL]
 */
object_type *inventory;


/*
 * The size of "alloc_kind_table" (at most MAX_K_IDX * 4)
 */
s16b alloc_kind_size;

/*
 * The entries in the "kind allocator table"
 */
alloc_entry *alloc_kind_table;


/*
 * The size of "alloc_race_table" (at most MAX_R_IDX)
 */
s16b alloc_race_size;

/*
 * The entries in the "race allocator table"
 */
alloc_entry *alloc_race_table;


/*
 * Specify attr/char pairs for visual special effects
 * Be sure to use "index & 0x7F" to avoid illegal access
 */
/* byte misc_to_attr[128]; */
/* char misc_to_char[128]; */


/*
 * Specify attr/char pairs for inventory items (by tval)
 * Be sure to use "index & 0x7F" to avoid illegal access
 */
byte tval_to_attr[128];
char tval_to_char[128];


/*
 * Keymaps for each "mode" associated with each keypress.
 */
cptr keymap_act[KEYMAP_MODES][256];



/*** Player information ***/

/*
 * Player info record
 */
player_type p_body;

/*
 * Pointer to the player info
 */
player_type *p_ptr = &p_body;


/*
 * Pointer to the player tables
 * (sex, race, template, magic)
 */
player_sex *sp_ptr;
player_race *rp_ptr;
player_template *cp_ptr;


/*
 * More spell info
 */
byte spell_order[128];	/* order spells learned/remembered/forgotten */


/*
 * Calculated base hp values for player at each level,
 * store them so that drain life + restore life does not
 * affect hit points.  Also prevents shameless use of backup
 * savefiles for hitpoint acquirement.
 */
s16b player_hp[100];


/* Various maxima */
maxima *z_info = NULL;

/*
 * The vault generation arrays
 */
vault_type *v_info;
cptr v_name;
cptr v_text;

/*
 * The terrain feature arrays
 */
feature_type *f_info;
cptr f_name;
/* cptr f_text; */

/*
 * The object kind arrays
 */
object_kind *k_info;
cptr k_name;
cptr k_text;

/*
 * The unidentified object arrays
 */
unident_type *u_info;
char *u_name; /* This is written to in name_scrolls(). */

/*
 * The base object arrays
 */
o_base_type *o_base;
cptr ob_name;

/*
 * The artifact arrays
 */
artifact_type *a_info;
cptr a_name;
/* char *a_text; */

/*
 * The ego-item arrays
 */
ego_item_type *e_info;
cptr e_name;
/* char *e_text; */


/*
 * The monster race arrays
 */
monster_race *r_info;
char *r_name; /* This is written to during ghost creation. */
cptr r_text;

/*
 * The death event arrays
 */
death_event_type *death_event;
cptr event_name;
cptr event_text;

/*
 * The dungeon arrays.
 */
dun_type *dun_defs;
cptr dun_name;

/*
 * The town arrays.
 */
town_type *town_defs;
cptr town_name;

/*
 * The quest array.
 */
quest_type *q_list;

/*
 * The shopkeeper array.
 */
owner_type *owners;
cptr s_name;

/*
 * Hack -- The special Angband "System Suffix"
 * This variable is used to choose an appropriate "pref-xxx" file
 */
cptr ANGBAND_SYS = "xxx";

/*
 * Hack -- The special Angband "Graphics Suffix"
 * This variable is used to choose an appropriate "graf-xxx" file
 */
cptr ANGBAND_GRAF = "old";

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
 * Default user "preference" files (ascii)
 * These files are rarely portable between platforms
 */
cptr ANGBAND_DIR_PREF;

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
bool (*item_tester_hook)(object_ctype*);



/*
 * Current "comp" function for ang_sort()
 */
bool (*ang_sort_comp)(vptr u, vptr v, int a, int b);


/*
 * Current "swap" function for ang_sort()
 */
void (*ang_sort_swap)(vptr u, vptr v, int a, int b);




/* Hack, violet uniques */
bool violet_uniques = FALSE;

 #ifdef ALLOW_EASY_OPEN
 bool easy_open = TRUE;
 #endif /* ALLOW_EASY_OPEN -- TNB */
 
 #ifdef ALLOW_EASY_DISARM
 bool easy_disarm = TRUE;
 #endif /* ALLOW_EASY_DISARM -- TNB */

/*
 * Player stat defaults
 */
stat_default_type *stat_default;
s16b stat_default_total;

/* 
 * Initialisation macros
 */
#ifdef ALLOW_TEMPLATES

init_macro_type *macro_info = NULL;
char *macro_name;
char *macro_text;

u16b rebuild_raw = 0;

#endif /* ALLOW_TEMPLATES */

