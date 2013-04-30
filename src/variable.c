/* File: variable.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.6
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"


/*
 * Hack -- Link a copyright message into the executable
 */
cptr copyright =
	"Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Keoneke\n"
	"\n"
	"This software may be copied and distributed for educational, research,\n"
	"and not for profit purposes provided that this copyright and statement\n"
	"are included in all such copies.  Other copyrights may also apply.\n"
	"\n"
	"UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.6\n"
	"source code are released under the Gnu Public License. See www.fsf.org\n"
	"for current GPL license details. Addition permission granted to\n"
	"incorporate modifications in all Angband variants as defined in the\n"
	"Angband variants FAQ. See rec.games.roguelike.angband for FAQ.\n";


/*
 * Executable version
 */
byte version_major = VERSION_MAJOR;
byte version_minor = VERSION_MINOR;
byte version_patch = VERSION_PATCH;
byte version_extra = VERSION_EXTRA;

/*
 * Savefile version
 */
byte sf_major;		  /* Savefile's "version_major" */
byte sf_minor;		  /* Savefile's "version_minor" */
byte sf_patch;		  /* Savefile's "version_patch" */
byte sf_extra;		  /* Savefile's "version_extra" */

/*
 * Savefile information
 */
u32b sf_xtra;		   /* Operating system info */
u32b sf_when;		   /* Time when savefile created */
u16b sf_lives;		  /* Number of past "lives" with this file */
u16b sf_saves;		  /* Number of "saves" during this life */

/*
 * Run-time arguments
 */
bool arg_fiddle;			/* Command arg -- Request fiddle mode */
bool arg_wizard;			/* Command arg -- Request wizard mode */
bool arg_sound;			 /* Command arg -- Request special sounds */
bool arg_graphics;		      /* Command arg -- Request graphics mode */
bool arg_force_original;	/* Command arg -- Request original keyset */
bool arg_force_roguelike;       /* Command arg -- Request roguelike keyset */

/*
 * Various things
 */

bool character_generated;       /* The character exists */
bool character_dungeon;	 /* The character has a dungeon */
bool character_loaded;	  /* The character was loaded from a savefile */
bool character_saved;	   /* The character was just saved to a savefile */

s16b character_icky;	    /* Depth of the game in special mode */
s16b character_xtra;	    /* Depth of the game in startup mode */

u32b seed_randart;	      /* Hack -- consistent random artifacts */

u32b seed_flavor;	       /* Hack -- consistent object colors */
u32b seed_town;		 /* Hack -- consistent town layout */

s16b num_repro;		 /* Current reproducer count */
s16b object_level;	      /* Current object creation level */
s16b monster_level;	     /* Current monster creation level */

char summon_kin_type;	   /* Hack -- See summon_specific() */

s32b turn;			      /* Current game turn */

s32b old_turn;		  /* Hack -- Level feeling counter */

bool surface;
bool daytime;

bool use_sound;		 /* The "sound" mode is enabled */
bool use_graphics;	      /* The "graphics" mode is enabled */
bool use_trptile = FALSE;
bool use_dbltile = FALSE;
bool use_bigtile = FALSE;

s16b signal_count;	      /* Hack -- Count interrupts */

bool msg_flag;		  /* Player has pending message */

bool inkey_base;		/* See the "inkey()" function */
bool inkey_xtra;		/* See the "inkey()" function */
bool inkey_scan;		/* See the "inkey()" function */
bool inkey_flag;		/* See the "inkey()" function */

s16b coin_type;		 /* Hack -- force coin type */

s16b food_type;		 /* Hack -- force food type */

s16b race_drop_idx;	     /* Hack -- force race drop */

bool opening_chest;	     /* Hack -- prevent chest generation */

bool shimmer_monsters;  /* Hack -- optimize multi-hued monsters */
bool shimmer_objects;   /* Hack -- optimize multi-hued objects */

bool repair_mflag_show; /* Hack -- repair monster flags (show) */
bool repair_mflag_mark; /* Hack -- repair monster flags (mark) */

s16b o_max = 1;		 /* Number of allocated objects */
s16b o_cnt = 0;		 /* Number of live objects */

s16b m_max = 1;		 /* Number of allocated monsters */
s16b m_cnt = 0;		 /* Number of live monsters */

/*
 * Hack - Trackees for term windows
 */
object_type term_object;
bool term_obj_real;

/*
 * TRUE if process_command() is a repeated call.
 */
bool command_repeating = FALSE;


/*
 * Dungeon variables
 */

byte feeling;		   /* Most recent feeling */
s16b rating;		    /* Level's current rating */

byte level_flag;		/* Level type */


bool good_item_flag;    /* True if "Artifact" on this level */

bool closing_flag;	      /* Dungeon is closing */


/*
 * The character generates both directed (extra) noise (by doing noisy 
 * things) and ambiant noise (the combination of directed and innate 
 * noise).  Directed noise can immediately wake up monsters in LOS. 
 * Ambient noise determines how quickly monsters wake up and how often 
 * they get new information on the current character position.
 *
 * Each player turn, more noise accumulates.  Every time monster 
 * temporary conditions are processed, all non-innate noise is cleared.
 */
int add_wakeup_chance = 0;
u32b total_wakeup_chance = 0;


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
 * The number of quarks (first quark is NULL)
 */
s16b quark__num = 1;

/*
 * The array[QUARK_MAX] of pointers to the quarks
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
 * The array[MESSAGE_MAX] of offsets, by index
 */
u16b *message__ptr;

/*
 * The array[MESSAGE_BUF] of chars, by offset
 */
char *message__buf;

/*
 * The array[MESSAGE_MAX] of u16b for the types of messages
 */
u16b *message__type;

/*
 * Hack --- context sensitive help
 * XXX - probably should make them pointers to elsewhere
 */

/*
 * Help file name
 */
char context_help_file[16];

/*
 * Help file name
 */
char context_help_match[132];



/*
 * Table of colors associated to message-types
 */
byte message__color[MSG_MAX];


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
	{0x00, 0x00, 0x00, 0x00},       /* TERM_DARK */
	{0x00, 0xFF, 0xFF, 0xFF},       /* TERM_WHITE */
	{0x00, 0x80, 0x80, 0x80},       /* TERM_SLATE */
	{0x00, 0xFF, 0x80, 0x00},       /* TERM_ORANGE */
	{0x00, 0xC0, 0x00, 0x00},       /* TERM_RED */
	{0x00, 0x00, 0x80, 0x40},       /* TERM_GREEN */
	{0x00, 0x00, 0x40, 0xFF},       /* TERM_BLUE */
	{0x00, 0x80, 0x40, 0x00},       /* TERM_UMBER */
	{0x00, 0x60, 0x60, 0x60},       /* TERM_L_DARK */
	{0x00, 0xC0, 0xC0, 0xC0},       /* TERM_L_WHITE */
	{0x00, 0xFF, 0x00, 0xFF},       /* TERM_VIOLET */
	{0x00, 0xFF, 0xFF, 0x00},       /* TERM_YELLOW */
	{0x00, 0xFF, 0x40, 0x40},       /* TERM_L_RED */
	{0x00, 0x00, 0xFF, 0x00},       /* TERM_L_GREEN */
	{0x00, 0x00, 0xFF, 0xFF},       /* TERM_L_BLUE */
	{0x00, 0xC0, 0x80, 0x40}	/* TERM_L_UMBER */
};


/*
 * Standard sound (and message) names
 */
const cptr angband_sound_name[SOUND_MAX] =
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
	"bell",
	"nothing_to_open",
	"lockpick_fail",
	"stairs",
	"hitpoint_warn",
};


/*
 * Array[VIEW_MAX] used by "update_view()"
 */
sint view_n = 0;
u16b *view_g;

/*
 * Arrays[TEMP_MAX] used for various things
 */
sint temp_n = 0;
u16b *temp_g;
byte *temp_y;
byte *temp_x;

/*
 * Arrays[DYNA_MAX] used for various things
 */
sint dyna_n = 0;
u16b *dyna_g;
byte dyna_cent_y;
byte dyna_cent_x;
bool dyna_full;

/*
 * Array[DUNGEON_HGT][256] of cave grid info flags (padded)
 *
 * This array is padded to a width of 256 to allow fast access to elements
 * in the array via "grid" values (see the GRID() macros).
 */
byte (*cave_info)[256];

/*
 * Array[DUNGEON_HGT][256] of player grid info flags (padded)
 *
 * This array is padded to a width of 256 to allow fast access to elements
 * in the array via "grid" values (see the GRID() macros).
 */
byte (*play_info)[256];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid feature codes
 */
s16b (*cave_feat)[DUNGEON_WID];



/*
 * Array of room information
 *
 */
room_info_type room_info[DUN_ROOMS];

/*
 * Array[MAX_ROOMS_ROW][MAX_ROOMS_COL] of room information
 *
 * This indexes into the above room information
 *
 */
byte dun_room[MAX_ROOMS_ROW][MAX_ROOMS_COL];





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



#ifdef MONSTER_FLOW

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid flow "cost" values
 * Used to simulate character noise.
 */
byte (*cave_cost)[DUNGEON_WID];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid flow "when" stamps.
 * Used to store character scent trails.
 */
byte (*cave_when)[DUNGEON_WID];

/*
 * Current scent age marker.  Counts down from 250 to 0 and then loops.
 */
int scent_when = 250;


/*
 * Centerpoints of the last flow (noise) rebuild and the last flow update.
 */
int flow_center_y;
int flow_center_x;
int update_center_y;
int update_center_x;

/*
 * Flow cost at the center grid of the current update.
 */
int cost_at_center = 0;

#endif	/* MONSTER_FLOW */



/*
 * Array[z_info->o_max] of dungeon objects
 */
object_type *o_list;

/*
 * Array[z_info->m_max] of dungeon monsters
 */
monster_type *m_list;

/*
 * Array[z_info->r_max] of monster lore
 */
monster_lore *l_list;


/*
 * Array[z_info->a_max] of artifact lore
 */
object_lore *a_list;

/*
 * Array[z_info->e_max] of ego item lore
 */
object_lore *e_list;

/*
 * Array[z_info->k_max] of kind lore
 */
object_lore *k_list;


/*
 * Hack -- Array[MAX_Q_IDX] of quests
 */
quest *q_list;


/*
 * Array[MAX_STORES] of stores
 */
store_type *store;

/*
 * Array[INVEN_TOTAL] of objects in the player's inventory
 */
object_type *inventory;


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
 * The size of "alloc_feat_table" (at most MAX_F_IDX * 4)
 */
s16b alloc_feat_size;

/*
 * The array[alloc_feat_size] of entries in the "feat allocator table"
 */
alloc_entry *alloc_feat_table;


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
 * Current (or recent) macro action
 */
char macro_buffer[1024];


/*
 * Keymaps for each "mode" associated with each keypress.
 */
cptr keymap_act[KEYMAP_MODES][256];



/*** Player information ***/

/*
 * Pointer to the player tables (sex, race, class, magic)
 */
const player_sex *sp_ptr;
const player_race *rp_ptr;
const player_class *cp_ptr;

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
 * The room description information arrays
 */
desc_type *d_info;
char *d_name;
char *d_text;

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
 * The flavor arrays
 */
flavor_type *x_info;
char *x_name;
char *x_text;

/*
 * The monster race arrays
 */
monster_race *r_info;
char *r_name;
char *r_text;


/*
 * The player race arrays
 */
player_race *p_info;
char *p_name;
char *p_text;

/*
 * The player class arrays
 */
player_class *c_info;
char *c_name;
char *c_text;

/*
 * The weapon style arrays
 */
weapon_style *w_info;

/*
 * The spell arrays
 */
spell_type *s_info;
char *s_name;
char *s_text;

/*
 * The rune arrays
 */
rune_type *y_info;
char *y_name;
char *y_text;


/*
 * The town/dungeon arrays
 */
town_type *t_info;
char *t_name;
char *t_text;

/*
 * The store arrays
 */
store_type *u_info;
char *u_name;
char *u_text;


/*
 * The player history arrays
 */
hist_type *h_info;
char *h_text;

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
 * Savefiles for current characters (binary)
 * These files are portable between platforms
 */
cptr ANGBAND_DIR_SAVE;

/*
 * User "preference" files (ascii)
 * These files are rarely portable between platforms
 */
cptr ANGBAND_DIR_PREF;


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
bool (*item_tester_hook)(const object_type*);



/*
 * Current "comp" function for ang_sort()
 */
bool (*ang_sort_comp)(vptr u, vptr v, int a, int b);


/*
 * Current "swap" function for ang_sort()
 */
void (*ang_sort_swap)(vptr u, vptr v, int a, int b);



/*
 * Hack -- function hook to restrict "get_mon_num_prep()" function
 */
bool (*get_mon_num_hook)(int r_idx);



/*
 * Hack -- function hook to restrict "get_obj_num_prep()" function
 */
bool (*get_obj_num_hook)(int k_idx);


/*
 * Hack -- function hook to restrict "get_feat_num_prep()" function
 */
bool (*get_feat_num_hook)(int f_idx);


/*
 * Hack -- File hundle for output used within the text_out_to_file()
 */
FILE *text_out_file = NULL;


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
 * Hack -- Where to stop text output in number of lines. The function will
 * write at most this many lines before aborting. Use the default
 * value (for example the screen height) when 'text_out_wrap' is 0. Note
 * that the starting y position in screen output will assume that already
 * (y - 1) lines have been written (Similiar to text_out_indent).
 */
int text_out_lines = 0;

/*
 * Hack -- function hook for text_out()
 *
 * Returns 0 if all characters in str have been output, or the next position
 * in the string if not completely output.
 */
void (*text_out_hook)(byte a, cptr str);


/*
 * The "highscore" file descriptor, if available.
 */
int highscore_fd = -1;


/*
 * Use transparent tiles
 */
bool use_transparency = FALSE;

/*
 * Game can be saved
 */
bool can_save = TRUE;
