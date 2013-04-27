/* File: variable.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Hack -- Link a copyright message into the executable
 */
const char* const copyright =
	"Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Keoneke\n"
	"\n"
	"This software may be copied and distributed for educational, research,\n"
	"and not for profit purposes provided that this copyright and statement\n"
	"are included in all such copies.  Other copyrights may also apply.\n";


/*
 * Savefile information
 */
u32b sf_xtra;			/* Operating system info */
u32b sf_when;			/* Time when savefile created */
u16b sf_lives;			/* Number of past "lives" with this file */
u16b sf_saves;			/* Number of "saves" during this life */

/*
 * Run-time arguments
 */
bool arg_fiddle;			/* Command arg -- Request fiddle mode */
bool arg_wizard;			/* Command arg -- Request wizard mode */
bool arg_sound;				/* Command arg -- Request special sounds */
int arg_graphics;			/* Command arg -- Request graphics mode */

/*
 * Various things
 */

bool character_generated;	/* The character exists */
bool character_dungeon;		/* The character has a dungeon */
bool character_loaded;		/* The character was loaded from a savefile */
bool character_saved;		/* The character was just saved to a savefile */

s16b character_icky;		/* Depth of the game in special mode */
s16b character_xtra;		/* Depth of the game in startup mode */

u32b seed_randart;		/* Hack -- consistent random artifacts */

u32b seed_flavor;		/* Hack -- consistent object colors */
u32b seed_town;			/* Hack -- consistent town layout */

s16b num_repro;			/* Current reproducer count */
s16b object_level;		/* Current object creation level */
s16b monster_level;		/* Current monster creation level */

char summon_kin_type;		/* Hack -- See summon_specific() */

s32b turn;				/* Current game turn */

s32b old_turn;			/* Hack -- Level feeling counter */

bool use_sound;			/* The "sound" mode is enabled */
int use_graphics;		/* The "graphics" mode is enabled */
bool use_bigtile = FALSE;

s16b signal_count;		/* Hack -- Count interrupts */

bool msg_flag = FALSE;	/* Player has pending message */

bool inkey_base;		/* See the "inkey()" function */
bool inkey_xtra;		/* See the "inkey()" function */
bool inkey_scan;		/* See the "inkey()" function */
bool inkey_flag;		/* See the "inkey()" function */

bool opening_chest = FALSE;		/* Hack -- prevent chest generation */

bool shimmer_monsters;	/* Hack -- optimize multi-hued monsters */
bool shimmer_objects;	/* Hack -- optimize multi-hued objects */

bool repair_mflag_nice;	/* Hack -- repair monster flags (nice) */
bool repair_mflag_show;	/* Hack -- repair monster flags (show) */
bool repair_mflag_mark;	/* Hack -- repair monster flags (mark) */

/*
 * TRUE if process_command() is a repeated call.
 */
bool command_repeating = FALSE;


/*
 * Dungeon variables
 */

byte feeling;			/* Most recent feeling */
s16b rating;			/* Level's current rating */

bool good_item_flag;	/* True if "Artifact" on this level */

bool closing_flag;		/* Dungeon is closing */


/*
 * Player info
 */
int player_uid;
int player_euid;
int player_egid;


/*
 * Buffer to hold the current savefile name
 */
char savefile[1024];


/*
 * The array[ANGBAND_TERM_MAX] of window pointers
 */
term* angband_term[ANGBAND_TERM_MAX];


/*
 * The array[ANGBAND_TERM_MAX] of window names (modifiable?)
 *
 * ToDo: Make the names independent of ANGBAND_TERM_MAX.
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
 * Global table of color definitions (mostly zeros)
 */
byte angband_color_table[MAX_COLORS][4] =
{
	{0x00, 0x00, 0x00, 0x00},	/* TERM_DARK */
	{0x00, 0xFF, 0xFF, 0xFF},	/* TERM_WHITE */
	{0x00, 0x80, 0x80, 0x80},	/* TERM_SLATE */
	{0x00, 0xFF, 0x80, 0x00},	/* TERM_ORANGE */
	{0x00, 0xC0, 0x00, 0x00},	/* TERM_RED */
	{0x00, 0x00, 0x80, 0x40},	/* TERM_GREEN */
	{0x00, 0x00, 0x40, 0xFF},	/* TERM_BLUE */
	{0x00, 0x80, 0x40, 0x00},	/* TERM_UMBER */
	{0x00, 0x60, 0x60, 0x60},	/* TERM_L_DARK */
	{0x00, 0xC0, 0xC0, 0xC0},	/* TERM_L_WHITE */
	{0x00, 0xFF, 0x00, 0xFF},	/* TERM_VIOLET */
	{0x00, 0xFF, 0xFF, 0x00},	/* TERM_YELLOW */
	{0x00, 0xFF, 0x40, 0x40},	/* TERM_L_RED */
	{0x00, 0x00, 0xFF, 0x00},	/* TERM_L_GREEN */
	{0x00, 0x00, 0xFF, 0xFF},	/* TERM_L_BLUE */
	{0x00, 0xC0, 0x80, 0x40}	/* TERM_L_UMBER */
};


/*
 * Arrays[TEMP_MAX] used for various things
 *
 * Note that temp_g shares memory with temp_x and temp_y.
 */
int temp_n = 0;
coord *temp_g;
byte *temp_y;
byte *temp_x;

/*
 * start level-specific information
 */


/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid info flags
 */
byte (*cave_info)[DUNGEON_WID];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid feature codes
 */
byte (*cave_feat)[DUNGEON_WID];


/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid object indexes
 *
 * Note that this array yields the index of the top object in the stack of
 * objects in a given grid, using the "next_o_idx" field in that object to
 * indicate the next object in the stack, and so on, using zero to indicate
 * "nothing".  This array replicates the information contained in the object
 * list, for efficiency, providing extremely fast determination of whether
 * any object is in a grid, and relatively fast determination of which objects
 * are in a grid.
 */
s16b (*cave_o_idx)[DUNGEON_WID];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid monster indexes
 *
 * Note that this array yields the index of the monster or player in a grid,
 * where negative numbers are used to represent the player, positive numbers
 * are used to represent a monster, and zero is used to indicate "nobody".
 * This array replicates the information contained in the monster list and
 * the player structure, but provides extremely fast determination of which,
 * if any, monster or player is in any given grid.
 */
s16b (*cave_m_idx)[DUNGEON_WID];


/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid flow "cost" values
 */
byte (*cave_cost)[DUNGEON_WID];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid flow "when" stamps
 */
byte (*cave_when)[DUNGEON_WID];


object_type *o_list;	/**< Array[z_info->o_max] of dungeon objects */
s16b o_max = 1;			/**< Number of allocated objects */
s16b o_cnt = 0;			/**< Number of live objects */

monster_type *mon_list; /**< Array[z_info->m_max] of dungeon monsters */
s16b mon_max = 1;		/**< Number of allocated monsters */
s16b mon_cnt = 0;		/**< Number of live monsters */

/*
 * end level-specific information
 */


/*
 * Array[z_info->r_max] of monster lore
 */
monster_lore* monster_type::l_list;


/*
 * Hack -- Array[MAX_Q_IDX] of quests
 */
quest *q_list;


/*
 * Array[MAX_STORES] of stores
 */
store_type *store;


/*
 * The size of "alloc_kind_table" (at most z_info->k_max * 4)
 */
s16b alloc_kind_size;

/*
 * The array[alloc_kind_size] of entries in the "kind allocator table"
 */
alloc_entry *alloc_kind_table;


/*
 * The size of the "alloc_ego_table"
 */
s16b alloc_ego_size;

/*
 * The array[alloc_ego_size] of entries in the "ego allocator table"
 */
alloc_entry *alloc_ego_table;


/*
 * The size of "alloc_race_table" (at most z_info->r_max)
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
byte misc_to_attr[256];
char misc_to_char[256];


/*
 * Specify color for inventory item text display (by tval)
 * Be sure to use "index & 0x7F" to avoid illegal access
 */
byte tval_to_attr[128];


/*
 * Keymaps for each "mode" associated with each keypress.
 */
const char* keymap_act[KEYMAP_MODES][256];



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
 * The player info record (static)
 */
static player_type player_type_body;

/*
 * Pointer to the player info record
 */
player_type *p_ptr = &player_type_body;


/*
 * Structure (not array) of size limits
 */
maxima *z_info;

/*
 * The vault generation arrays
 */
vault_type* vault_type::v_info;
char* vault_type::v_name;
char* vault_type::v_text;

/*
 * The terrain feature arrays
 */
feature_type* feature_type::f_info;
char* feature_type::f_name;
char* feature_type::f_text;

/*
 * The object kind arrays
 */
object_kind* object_type::k_info;
char* object_kind::k_name;
char* object_kind::k_text;

/*
 * The artifact arrays
 */
artifact_type* object_type::a_info;
char* artifact_type::a_name;
char* artifact_type::a_text;

/*
 * The ego-item arrays
 */
ego_item_type* object_type::e_info;
char* ego_item_type::e_name;
char* ego_item_type::e_text;


/*
 * The monster race arrays
 */
monster_race* monster_type::r_info;
char* monster_race::r_name;
char* monster_race::r_text;


/*
 * The player race arrays
 */
player_race* player_type::p_info;
char* player_type::p_name;
char* player_type::p_text;

/*
 * The player class arrays
 */
player_class* player_type::c_info;
char* player_type::c_name;
char* player_type::c_text;

/*
 * The player history arrays
 */
hist_type* player_type::h_info;
char* player_type::h_text;

/*
 * The shop owner arrays
 */
owner_type *b_info;
char *b_name;
char *b_text;

/*
 * The racial price adjustment arrays
 */
byte *g_info;
char *g_name;
char *g_text;


/*
 * The object flavor arrays
 */
flavor_type* object_kind::flavor_info;
char* flavor_type::flavor_name;
char* flavor_type::flavor_text;


/*
 * Hack -- The special Angband "System Suffix"
 * This variable is used to choose an appropriate "pref-xxx" file
 */
const char* ANGBAND_SYS = "xxx";

/*
 * Hack -- The special Angband "Graphics Suffix"
 * This variable is used to choose an appropriate "graf-xxx" file
 */
const char* ANGBAND_GRAF = "old";

/*
 * Path name: The main "lib" directory
 * This variable is not actually used anywhere in the code
 */
const char* ANGBAND_DIR;

/*
 * High score files (binary)
 * These files may be portable between platforms
 */
const char* ANGBAND_DIR_APEX;

/*
 * Bone files for player ghosts (ascii)
 * These files are portable between platforms
 */
const char* ANGBAND_DIR_BONE;

/*
 * Binary image files for the "*_info" arrays (binary)
 * These files are not portable between platforms
 */
const char* ANGBAND_DIR_DATA;

/*
 * Textual template files for the "*_info" arrays (ascii)
 * These files are portable between platforms
 */
const char* ANGBAND_DIR_EDIT;

/*
 * Various extra files (ascii)
 * These files may be portable between platforms
 */
const char* ANGBAND_DIR_FILE;

/*
 * Help files (normal) for the online help (ascii)
 * These files are portable between platforms
 */
const char* ANGBAND_DIR_HELP;

/*
 * Help files (spoilers) for the online help (ascii)
 * These files are portable between platforms
 */
const char* ANGBAND_DIR_INFO;

/*
 * Savefiles for current characters (binary)
 * These files are portable between platforms
 */
const char* ANGBAND_DIR_SAVE;

/*
 * Default user "preference" files (ascii)
 * These files are rarely portable between platforms
 */
const char* ANGBAND_DIR_PREF;

/*
 * User defined "preference" files (ascii)
 * These files are rarely portable between platforms
 */
const char* ANGBAND_DIR_USER;

/*
 * Various extra files (binary)
 * These files are rarely portable between platforms
 */
const char* ANGBAND_DIR_XTRA;

/*
 * Total Hack -- allow all items to be listed (even empty ones)
 * This is only used by "do_cmd_inven_e()" and is cleared there.
 */
bool item_tester_full = FALSE;


/*
 * Here is a "pseudo-hook" used during calls to "get_item()" and
 * "show_inven()" and "show_equip()", and the choice window routines.
 */
byte item_tester_tval;

/*
 * Here is a "hook" used during calls to "get_item()" and
 * "show_inven()" and "show_equip()", and the choice window routines.
 */
bool (*item_tester_hook)(const object_type*);


/*
 * Current "comp" function for ang_sort()
 */
bool (*ang_sort_comp)(const void *u, const void *v, int a, int b);


/*
 * Current "swap" function for ang_sort()
 */
void (*ang_sort_swap)(void *u, void *v, int a, int b);



void (*object_info_out_flags)(const object_type *o_ptr, u32b* f);


/*
 * Hack - the destination file for text_out_to_file.
 */
FILE *text_out_file = NULL;


/*
 * Hack -- function hook to output (colored) text to the
 * screen or to a file.
 */
void (*text_out_hook)(byte a, const char* str);


/*
 * Hack -- Where to wrap the text when using text_out().  Use the default
 * value (for example the screen width) when 'text_out_wrap' is 0.
 */
int text_out_wrap = 0;


/*
 * Hack -- Indentation for the text when using text_out().
 */
int text_out_indent = 0;


/*
 * Use transparent tiles
 */
bool use_transparency = FALSE;
