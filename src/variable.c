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
 * Executable version
 */
byte version_major = VER_MAJOR;
byte version_minor = VER_MINOR;
byte version_patch = VER_PATCH;
byte version_extra = VER_EXTRA;

/*
 * Savefile version
 */
byte sf_extra;	/* Savefile's "version_extra" */
u32b sf_version;	/* Savefile's "version" */

byte z_major;	/* Savefile version for Zangband */
byte z_minor;
byte z_patch;

/*
 * Savefile information
 */
u32b sf_xtra;	/* Operating system info */
u32b sf_when;	/* Time when savefile created */
u16b sf_lives;	/* Number of past "lives" with this file */
u16b sf_saves;	/* Number of "saves" during this life */

/*
 * Run-time arguments
 */
bool arg_fiddle;	/* Command arg -- Request fiddle mode */
bool arg_wizard;	/* Command arg -- Request wizard mode */
bool arg_sound;	/* Command arg -- Request special sounds */
byte arg_graphics;	/* Command arg -- Request graphics mode */
bool arg_bigtile;	/* Command arg -- Request bigtile mode */
bool arg_monochrome;	/* Command arg -- Request monochrome mode */
bool arg_force_original;	/* Command arg -- Request original keyset */
bool arg_force_roguelike;	/* Command arg -- Request roguelike keyset */

/*
 * Various things
 */
bool character_generated = FALSE;	/* The character exists */
bool character_dungeon = FALSE;	/* The character has a dungeon */
bool character_loaded = FALSE;	/* The character was loaded from a savefile */
bool character_saved = FALSE;	/* The character was just saved to a savefile */

bool character_icky = FALSE;	/* The game is in an icky full screen mode */
bool character_xtra = FALSE;	/* The game is in an icky startup mode */

u32b seed_flavor;	/* Hack -- consistent object colors */

bool msg_flag;	/* Used in msg_print() for "buffering" */

s16b num_repro;	/* Current reproducer count */

s32b turn;	/* Current game turn */
s32b old_turn;	/* Turn when level began (feelings) */

bool use_sound;	/* The "sound" mode is enabled */
byte use_graphics;	/* The "graphics" mode enabled (0 is none) */
bool use_bigtile = FALSE;	/* Use square map tiles */

bool use_transparency = FALSE;	/* Use transparent tiles */

s16b signal_count;	/* Hack -- Count interupts */

s16b o_max = 1;	/* Number of allocated objects */
s16b o_cnt = 0;	/* Number of live objects */

s16b m_max = 1;	/* Number of allocated monsters */
s16b m_cnt = 0;	/* Number of live monsters */

s16b fld_max = 1;	/* Number of allocated fields */
s16b fld_cnt = 0;	/* Number of live fields */

s16b rg_max = 1;	/* Number of allocated regions */
s16b rg_cnt = 0;	/* Number of live regions */

s16b q_max = 1;	/* Number of allocated quests */

s16b hack_m_idx = 0;	/* Hack -- see "process_monsters()" */

/* Can we get rid of this at all? */
char summon_kin_type;	/* Hack, by Julian Lighton: summon 'relatives' */

/* This probably can be moved to player_type */
int total_friends = 0;
s32b total_friend_levels = 0;
s32b friend_align = 0;

s16b store_cache_num = 0;	/* Number of stores with stock */
store_type **store_cache;	/* The cache of store stocks */



/* Special options */

byte hitpoint_warn;	/* Hitpoint warning (0 to 9) */

byte delay_factor;	/* Delay factor (0 to 9) */

byte autosave_l;	/* Autosave before entering new levels */
byte autosave_t;	/* Timed autosave */
s16b autosave_freq;	/* Autosave frequency */

/* Cheating options */
bool cheat_peek;
bool cheat_hear;
bool cheat_room;
bool cheat_xtra;
bool cheat_know;
bool cheat_live;

bool fake_monochrome;	/* Use fake monochrome for effects */


/*
 * Dungeon size info
 */

s16b panel_row_min, panel_row_max;
s16b panel_col_min, panel_col_max;

byte *mp_a = NULL;
char *mp_c = NULL;
byte *mp_ta = NULL;
char *mp_tc = NULL;


/*
 * User info
 */
int player_uid;
int player_euid;
int player_egid;

/*
 * Current player's character name
 */
char player_name[32];

/*
 * Stripped version of "player_name"
 */
char player_base[32];


/*
 * Buffer to hold the current savefile name
 */
char savefile[1024];



/*
 * Array of grids viewable to the player (see "cave.c")
 */
s16b view_n;
s16b view_y[VIEW_MAX];
s16b view_x[VIEW_MAX];

/*
 * Array of grids for use by various functions (see "cave.c")
 */
s16b temp_n;
s16b temp_y[TEMP_MAX];
s16b temp_x[TEMP_MAX];


/*
 * Array of grids for use in monster lighting effects (see "cave.c")
 */
s16b lite_n = 0;
s16b lite_y[LITE_MAX];
s16b lite_x[LITE_MAX];



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
 * The array of window options
 */
u32b window_flag[ANGBAND_TERM_MAX];
u32b window_mask[ANGBAND_TERM_MAX];

/* Normal option masks */
u32b option_mask[8];


/*
 * The array of window pointers
 */
term *angband_term[ANGBAND_TERM_MAX];


/*
 * Standard window names
 */
char angband_term_name[ANGBAND_TERM_MAX][16] =
{
	VERSION_NAME,
	"Term-1",
	"Term-2",
	"Term-3",
	"Term-4",
	"Term-5",
	"Term-6",
	"Term-7"
};


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
	"warn",
	"rocket",
	"n_kill",
	"u_kill",
	"quest",
	"heal",
	"x_heal",
	"bite",
	"claw",
	"m_spell",
	"summon",
	"breath",
	"ball",
	"m_heal",
	"atkspell",
	"evil",
	"touch",
	"sting",
	"crush",
	"slime",
	"wail",
	"winner",
	"fire",
	"acid",
	"elec",
	"cold",
	"illegal",
	"fail",
	"wakeup",
	"invuln",
	"fall",
	"pain",
	"destitem",
	"moan",
	"show",
	"unused",
	"explode",
};


/*
 * The function pointer that is used to access the dungeon / wilderness.
 * It points to a simple function when in the dungeon, that evaluates
 * cave[y][x]
 * In the wilderness, things are more complicated.
 */

cave_type *(*area_aux) (int, int);

/*
 * Equivalent function pointer used to get player information
 * for each grid.
 */

pcave_type *(*parea_aux) (int, int);

/*
 * Variables used to access the scrollable wilderness.
 * This is designed to be as fast as possible - whilst using as little
 * RAM as possible to store a massive wilderness.
 *
 * The wilderness is generated "on the fly" as the player moves around it.
 * To save time - blocks of 16x16 squares are saved in a cache so they
 * don't need to be redone if the player moves back and forth.
 */

/* block used to generate plasma fractal for random wilderness */
u16b *temp_block[WILD_BLOCK_SIZE + 1];

/* List of 16x16 blocks in the wilderness */
blk_ptr *wild_cache;

/* Reference count of each 16x16 block in the wilderness */
int **wild_refcount;

/* Counter of where in the list of cache blocks we are */
u32b wc_cnt = 0;

/* The wilderness itself - grid of 16x16 blocks*/
blk_ptr **wild_grid;

/* The data used to generate the wilderness */
wild_type **wild;

/* The seed for the wilderness */
u32b wild_seed;

/* Description of wilderness block types */
wild_gen_data_type *wild_gen_data;

/* The decision tree for working out what block type to pick */
wild_choice_tree_type *wild_choice_tree;

/* Bounds checking function pointers */
bool (*in_bounds) (int, int);
bool (*in_bounds2) (int, int);
bool (*in_boundsp) (int, int);

/*
 * Current size of the wilderness
 */
s32b max_wild;

/*
 * The current global region.
 */
region_type cave_data;

/*
 * Index of current global region
 */
int cur_region;

/*
 * The array of dungeon items [z_info->o_max]
 */
object_type *o_list;

/*
 * The array of dungeon monsters [z_info->m_max]
 */
monster_type *m_list;

/*
 * The array of fields [z_info->fld_max]
 */
field_type *fld_list;

/*
 * The array of regions [z_info->rg_max]
 */
region_type *rg_list;

/*
 * The array of region information [z_info->rg_max]
 */
region_info *ri_list;

/*
 * Number of towns used.
 */
u16b place_count;

/*
 * Places in the wilderness [z_info->wp_max]
 */
place_type *place;


/*
 * The size of "alloc_kind_table" (at most z_info->k_max * 4)
 */
s16b alloc_kind_size;

/*
 * The entries in the "kind allocator table"
 */
alloc_entry *alloc_kind_table;


/*
 * The size of "alloc_race_table" (at most z_info->r_max)
 */
s16b alloc_race_size;

/*
 * The entries in the "race allocator table"
 */
alloc_entry *alloc_race_table;


/*
 * The size of the "alloc_ego_table" (at most z_info->e_max)
 */
s16b alloc_ego_size;

/*
 * The entries in the "ego item allocator table"
 */
alloc_entry *alloc_ego_table;


/*
 * Specify attr/char pairs for visual special effects
 * Be sure to use "index & 0x7F" to avoid illegal access
 *
 * Can we decrease the size to 128??
 */
byte misc_to_attr[256];
char misc_to_char[256];


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
 * Static player info record
 */
player_type p_body;

/*
 * Pointer to the player info
 */
player_type *p_ptr = &p_body;

/*
 * Pointer to the player tables
 * (sex, race, class, magic)
 */
player_sex *sp_ptr;
player_race *rp_ptr;
player_class *cp_ptr;
player_magic *mp_ptr;

/**** Server Information ****/

server_type s_body;

/*
 * Pointer to the server information
 */
server_type *svr_ptr = &s_body;



/*
 * Structure (not array) of size limits
 */
maxima *z_info;

/*
 * The vault generation arrays
 */
vault_type *v_info;
char *v_name;
char *v_text;

/*
 * The terrain feature arrays
 */
feature_type *f_info;
char *f_name;
char *f_text;

/*
 * The object kind arrays
 */
object_kind *k_info;
char *k_name;
char *k_text;

/*
 * The artifact arrays
 */
artifact_type *a_info;
char *a_name;
char *a_text;

/*
 * The ego-item arrays
 */
ego_item_type *e_info;
char *e_name;
char *e_text;


/*
 * The monster race arrays
 */
monster_race *r_info;
char *r_name;
char *r_text;

/*
 * The field thaumatergical array
 */
field_thaum *t_info;

/*
 * Quest data array
 */
quest_type *quest;

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
 * Script files
 * These files are portable between platforms.
 */
cptr ANGBAND_DIR_SCRIPT;

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


/* Can these inventory hacks be cleaned up somehow? */

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
bool (*item_tester_hook) (const object_type *);


/*
 * Current "comp" function for ang_sort()
 */
bool (*ang_sort_comp) (const vptr u, const vptr v, int a, int b);


/*
 * Current "swap" function for ang_sort()
 */
void (*ang_sort_swap) (const vptr u, const vptr v, int a, int b);


/*
 * Default spell color table (quark index)
 */
cptr gf_color[MAX_GF];


/*
 * Store owner table sizes
 */
int owner_names_max;
int owner_suffix_max;

/* Get rid of this? */

/*
 * Flags for initialization
 */
int init_flags;

