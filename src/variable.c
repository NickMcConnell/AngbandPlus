
/* File: variable.c */

/*
 * The copyright.  Global variables and arrays.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
	"are included in all such copies.  Other copyrights may also apply.\n";


/*
 * Sangband version
 */
byte version_major = VERSION_MAJOR;
byte version_minor = VERSION_MINOR;
byte version_patch = VERSION_PATCH;
byte version_extra = VERSION_EXTRA;


/*
 * Savefile version
 */
byte sf_major;			/* Savefile's "version_major" */
byte sf_minor;			/* Savefile's "version_minor" */
byte sf_patch;			/* Savefile's "version_patch" */
byte sf_extra;			/* Savefile's "version_extra". */
                     /* Used for encryption (currently always zero) */

	/* Hack -- Savefile information */
u32b sf_xtra;					/* Operating system info */
u32b sf_when;					/* Time when savefile created */
u16b sf_lives;					/* Number of past "lives" with this file */
u16b sf_saves;					/* Number of "saves" during this life */


	/* Run-time arguments */

bool arg_fiddle;				/* Command arg -- Request fiddle mode */
bool arg_wizard;				/* Command arg -- Request wizard mode */
bool arg_sound;					/* Command arg -- Request special sounds */
bool arg_graphics;				/* Command arg -- Request graphics mode */
bool arg_force_original;		/* Command arg -- Request original keyset */
bool arg_force_roguelike;		/* Command arg -- Request roguelike keyset */


	/* Various things */

bool character_generated;		/* The character exists */
bool character_dungeon;			/* The character has a dungeon */
bool character_loaded;			/* The character was loaded from a savefile */
bool character_saved;			/* The character was just saved to a savefile */
bool character_existed;			/* A character once existed on this savefile */

s16b character_icky;			/* Depth of the game in special mode */
s16b character_silent;			/* No messages when updating certain things */

u32b seed_flavor;				/* Hack -- consistent object colors */
u32b seed_town;					/* Hack -- consistent town layout */
u16b seed_detection = 0;      /* Hack -- efficient randomized detection */

s16b object_level;				/* Current object creation level */
s16b old_object_level;				/* Old object creation level */
s16b monster_level;				/* Current monster creation level */
s16b old_monster_level;				/* Old monster creation level */

s16b project_immune = 0;		/* Hack -- special immunity to projections */

s32b turn;						/* Current game turn */
s32b old_turn;					/* Turn when level began (feelings) */
s32b player_turn;               /* Number of turns the player has taken (including resting) */
s32b resting_turn;              /* Number of turns spent resting */


bool use_sound;					/* The "sound" mode is enabled */
bool use_graphics;				/* The "graphics" mode is enabled */

bool use_transparency = FALSE; /* Use transparent tiles */


/*
 * Autosave-related global variables.
 */
s16b autosave_freq = 0;			/* Autosave frequency */


/*
 * We assume that the game is running in VGA 16-color text mode.  If a
 * port enables a graphics mode capable of displaying more text colors,
 * then it needs to change this value.
 *
 * Should never be greater than 128, because graphics are defined as
 * PICTs with attrs (and chars) >= 128.
 */
s16b max_system_colors = 16;



/*
 * Hack -- Array of customized left panel slots.
 */
byte *custom_display;


/*
 * Number of rows shown on screen.  Set by "Term_rows()".
 */
s16b screen_rows;

/*
 * Number of rows used to display the dungeon.
 * Should always be a multiple of BLOCK_HGT (currently 11).  Should be
 * between 22 and 44.
 */
s16b map_rows = 22;

/*
 * Require most displays, the map screen in particular, to be shown using
 * 25 rows.
 */
bool force_25_rows = TRUE;

/*
 * Allow more displays to show 50 lines, the help system in particular.
 */
bool text_50_rows = FALSE;


/*
 * Allow player to precisely control how close to the edge of the map
 * the character must move before the screen automatically shifts.
 */
s16b clear_y = 2;
s16b clear_x = 4;


s16b image_count;  		/* Grids until next random image    */
                  		/* Optimizes the hallucination code */

s16b signal_count;		/* Hack -- Count interrupts */

bool msg_flag;					/* Player has pending message */

bool inkey_base;				/* See the "inkey()" function */
bool inkey_xtra;				/* See the "inkey()" function */
bool inkey_scan;				/* See the "inkey()" function */
bool inkey_flag;				/* See the "inkey()" function */

s16b coin_type;					/* Hack -- force coin type */

bool repair_mflag_show;       /* Hack -- optimize monster detection */

bool shimmer_objects;			/* Hack -- optimize multi-hued objects */

s16b o_max = 1;					/* Number of allocated objects */
s16b o_cnt = 0;					/* Number of live objects */

s16b m_max = 1;					/* Number of allocated monsters */
s16b m_cnt = 0;					/* Number of live monsters */

s16b t_max = 0;					/* Number of allocated traps */

char summon_kin_type;			/* Hack -- see summon_specific() */
int summon_index_type;			/* Hack -- see summon_specific() */

int detect_y = -1;                 /* Center of detection */
int detect_x = -1;                 /* Center of detection */




/* Messages can automatically move to a sub-window */
bool message_to_window_active = FALSE;


/*
 * Prevent potion smashing causing potion smashing causing ...
 */
bool allow_activate = TRUE;


/*
 * Dungeon variables
 */
byte feeling;					/* Most recent feeling */
bool no_feeling_yet;			/* Feeling is not yet known */
s16b level_rating;			/* Level's current rating */

byte dungeon_hgt = DUNGEON_HGT_MAX;   /* Current height and width of dungeon */
byte dungeon_wid = DUNGEON_WID_MAX;

bool good_item_flag;			/* True if ghost or quest level */

bool closing_flag;				/* Dungeon is closing */


/*
 * Player info
 */
int player_uid;
int player_euid;
int player_egid;


/*
 * Buffer to hold the current savefile name
 */
char savefile[256] = "";


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
 * The array of window pointers
 */
term *angband_term[ANGBAND_TERM_MAX];



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
 * Variables for the macro trigger patch
 */
int max_macrotrigger = 0;
cptr macro_template = NULL;
cptr macro_modifier_chr;
cptr macro_modifier_name[MAX_MACRO_MOD];
cptr macro_trigger_name[MAX_MACRO_TRIGGER];
cptr macro_trigger_keycode[2][MAX_MACRO_TRIGGER];


/*
 * Standard sound (and message) names
 */
cptr angband_sound_name[SOUND_MAX] =
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
	"hit_soft",
	"hit_medium",
	"hit_hard",
	"hit_deadly",
	"summon"
};


/*
 * Array[VIEW_MAX] used by "update_view()"
 */
int view_n = 0;
u16b *view_g;

/*
 * Arrays[TEMP_MAX] used for various things
 */
int temp_n = 0;
u16b *temp_g;  /* Note:  this duplicates temp_y and temp_x */
byte *temp_y;
byte *temp_x;

/*
 * Array[LITE_MAX] used by "process_player()"
 */
int lite_n = 0;
u16b *lite_g;

/*
 * Arrays[EFFECT_GRID_MAX] used for effects
 */
int effect_grid_n = 0;
effect_grid_type *effect_grid;


/*
 * Array[DUNGEON_HGT_MAX][256] of cave grid info flags (padded)
 *
 * This array is padded to a width of 256 to allow fast access to elements
 * in the array via "grid" values (see the GRID() macros).
 */
u16b(*cave_info)[256];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID_MAX] of cave grid feature codes
 */
byte(*cave_feat)[DUNGEON_WID_MAX];


/*
 * Array[DUNGEON_HGT_MAX][DUNGEON_WID_MAX] of cave grid object indexes
 *
 * Note that this array yields the index of the top object in the stack of
 * objects in a given grid, using the "next_o_idx" field in that object to
 * indicate the next object in the stack, and so on, using zero to indicate
 * "nothing".  This array replicates the information contained in the object
 * list, for efficiency, providing extremely fast determination of whether
 * any object is in a grid, and relatively fast determination of which objects
 * are in a grid.
 */
s16b(*cave_o_idx)[DUNGEON_WID_MAX];

/*
 * Array[DUNGEON_HGT_MAX][DUNGEON_WID_MAX] of cave grid monster indexes
 *
 * Note that this array yields the index of the monster or player in a grid,
 * where negative numbers are used to represent the player, positive numbers
 * are used to represent a monster, and zero is used to indicate "nobody".
 * This array replicates the information contained in the monster list and
 * the player structure, but provides extremely fast determination of which,
 * if any, monster or player is in any given grid.
 */
s16b(*cave_m_idx)[DUNGEON_WID_MAX];



/*
 * Array[DUNGEON_HGT_MAX][DUNGEON_WID_MAX] of cave grid flow "cost" values
 * Used to simulate character noise.
 */
byte (*cave_cost)[DUNGEON_WID_MAX];

/*
 * Array[DUNGEON_HGT_MAX][DUNGEON_WID_MAX] of cave grid flow "when" stamps.
 * Used to store character scent trails.
 */
byte (*cave_when)[DUNGEON_WID_MAX];

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


/*
 * Projection path and information
 */
u16b path_g[120];  /* Grids in the projection path */
byte path_gx[120];  /* Special information about each grid */
int path_n = 0;   /* Number of grids in the path */


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
 * Array[z_info->x_max] of dungeon effects
 */
effect_type *x_list;

/*
 * The trap arrays
 */
trap_type *t_list;

/*
 * Array[MAX_STORES] of stores
 */
store_type *store;

/*
 * Array[INVEN_TOTAL] of objects in the player's inventory
 */
object_type *inventory;

/*
 * The arrays of entries in the "kind allocator tables".
 * The first is used for allocation permissions, and the second for final
 * allocation chances.
 */
bool *permit_kind_table;
byte *chance_kind_table;

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
 * Number of entries in the movement moment table
 */
s16b move_moment_num = 0;


/*
 * The movement moment table
 */
move_moment_type *move_moment;

/*
 * The projection graphics table
 */
proj_graphics_type *proj_graphics;


/*
 * Specify color for inventory item text display (by tval).  Be sure
 * to use "index % ((N_ELEMENTS(tval_to_attr))" to avoid illegal access
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
 * Pointer to the player tables (sex, race, magic)
 */
const player_sex *sp_ptr;
const player_race *rp_ptr;
const player_magic *mp_ptr;

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
 * The character generates both directed (extra) noise (by doing noisy
 * things) and ambient noise (the combination of directed and innate
 * noise).
 *
 * Noise builds up as the character does certain things, and diminishes
 * over time.
 */
s32b add_wakeup_chance = 0;
s32b total_wakeup_chance = 0;



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
 * The player race arrays
 */
player_race *p_info;
char *p_name;
char *p_text;

/*
 * The shop owner arrays
 */
owner_type *b_info;
char *b_name;
char *b_text;

/*
 * The quest arrays
 */
quest_type *q_info;
char *q_name;
char *q_text;


/*
 * The object flavor arrays
 */
flavor_type *flavor_info;
char *flavor_name;
char *flavor_text;



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
 * Fat binary support.
 *
 * Something should set this before the code gets to init_file_paths,
 * or it won't have any effect.  This gets appended to the end of
 * ANGBAND_DIR_DATA, and lets us support fat binaries, which may require
 * multiple sets of data files.
 *
 * Hack -- allow a default to be set via a macro.
 */
#ifdef FAT_SUFFIX_DEFAULT
char *fat_data_suffix = FAT_SUFFIX_DEFAULT;
#else
char *fat_data_suffix = 0;
#endif

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
bool(*item_tester_hook)(const object_type*);

/*
 * A function hook, used during calls to "get_item()", that tests against
 * slot indexes.
 */
bool (*slot_tester_hook)(const int slot);


/*
 * Current "comp" function for ang_sort()
 */
bool (*ang_sort_comp)(const void *u, const void *v, int a, int b);


/*
 * Current "swap" function for ang_sort()
 */
void (*ang_sort_swap)(void *u, void *v, int a, int b);




/*
 * Hack -- variable to restrict "get_mon_num_prep()" function.
 *
 * Is 0 when we don't know how much space we have, 1 when only one grid
 * is available (we assume at least one), 2 when 4 or fewer are, and 3
 * otherwise.
 */
int monster_space = SPACE_UNKNOWN;

/*
 * Hack -- function hooks to restrict "get_mon_num_prep()" function
 */
bool(*get_mon_num_hook)(int r_idx);
bool(*old_get_mon_num_hook)(int r_idx);


/*
 * Hack -- function hooks to restrict "get_obj_num_prep()" function
 */
bool(*get_obj_num_hook)(int k_idx);
bool(*old_get_obj_num_hook)(int k_idx);


/*
 * The type of object the item generator should make, if specified.
 */
byte required_tval = 0;
byte old_required_tval = 0;


/*
 * Set of bitflags adjusting various things about object generation.
 * See "defines.h" for flags.
 */
u32b obj_gen_flags = 0L;

/*
 * Modifiers for the "object_desc()" function.
 */
s16b object_desc_flavour = 0;
s16b object_desc_plural = 0;


/*
 * The total of all final monster and object generation probabilities
 */
u32b alloc_race_total;
u32b alloc_kind_total;


/*
 * The default quantity passed to the "get_quantity()" function.
 */
s32b get_quantity_default = 1;


/*
 * Hack - the destination file for text_out_to_file.
 */
FILE *text_out_file = NULL;

/*
 * Hack -- function hook to output (colored) text to the
 * screen or to a file.
 */
void (*text_out_hook)(byte a, cptr str);

/*
 * Hack -- Where to wrap the text when using text_out().  Use the default
 * value (for example the screen width) when 'text_out_wrap' is 0.
 *
 */
int text_out_wrap = 0;

/*
 * Hack -- Indentation for the text when using text_out().
 *
 */
int text_out_indent = 0;

/*
 * The highscore file descriptor, if available.
 */
int highscore_fd = -1;

/*
 * A file descriptor (for saving screen info), if available.
 */
int dump_file_fd = -1;


/*
 * Game can be saved
 */
bool can_save = TRUE;


/*
 * Hack -- which skill is being used now?
 *
 * Used to assign practice experience to the right skill.
 */
s16b skill_being_used;



/*
 * The bones file a restored player ghost should use to collect extra
 * flags, a sex, and a unique name.  This also indicates that there is
 * a ghost active.
 */
byte bones_selector;

/*
 * The player ghost template index.
 */
int r_ghost;

/*
 * The player ghost name is stored here for quick reference by the
 * description function.
 */
char ghost_name[80];

/*
 * Is the player partly through trees, rubble, or water and, if so, in which
 * direction is he headed?  Monsters are handled more simply:  They have
 * a 33% or 50% chance of walking through.
 */
byte player_is_crossing;


/*
 * Prevent the "sea of runes" tactic
 */
byte num_glyph_on_level;

/*
 * Limit traps
 */
byte num_trap_on_level;

/*
 * Limit thefts
 */
byte num_recent_thefts;
