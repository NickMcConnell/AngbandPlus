/* File: variable.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Angband variables */

#include "angband.h"

bool initialized = FALSE;

int game_mode = GAME_MODE_NORMAL;

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
cptr warranty = 
    "THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY "
    "APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT "
    "HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM 'AS IS' WITHOUT WARRANTY "
    "OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, "
    "THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR "
    "PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM "
    "IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF "
    "ALL NECESSARY SERVICING, REPAIR OR CORRECTION.";
cptr liability = 
    "IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING "
    "WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS "
    "THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY "
    "GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE "
    "USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF "
    "DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD "
    "PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), "
    "EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF "
    "SUCH DAMAGES.";

int max_macrotrigger = 0;
cptr macro_template = NULL;
cptr macro_modifier_chr;
cptr macro_modifier_name[MAX_MACRO_MOD];
cptr macro_trigger_name[MAX_MACRO_TRIG];
cptr macro_trigger_keycode[2][MAX_MACRO_TRIG];

/*
 *  List for auto-picker/destroyer entries
 */
int max_autopick = 0;
int max_max_autopick = 0;
autopick_type *autopick_list = NULL;

/*
 * Savefile version
 */
byte h_ver_major;       /* Savefile version for Hengband 1.1.1 and later */
byte h_ver_minor;
byte h_ver_patch;
byte h_ver_extra;

byte sf_extra;        /* Savefile's encoding key */

byte z_major;           /* Savefile version for Hengband */
byte z_minor;
byte z_patch;

/*
 * Savefile information
 */
u32b sf_system;            /* Operating system info */
u32b sf_when;            /* Time when savefile created */
u16b sf_lives;            /* Number of past "lives" with this file */
u16b sf_saves;            /* Number of "saves" during this life */

/*
 * Run-time arguments
 */
bool arg_fiddle;            /* Command arg -- Request fiddle mode */
bool arg_wizard;            /* Command arg -- Request wizard mode */
bool arg_sound;                /* Command arg -- Request special sounds */
byte arg_graphics;            /* Command arg -- Request graphics mode */
bool arg_monochrome;        /* Command arg -- Request monochrome mode */
bool arg_force_original;    /* Command arg -- Request original keyset */
bool arg_force_roguelike;    /* Command arg -- Request roguelike keyset */
bool arg_bigtile = FALSE;    /* Command arg -- Request big tile mode */

/*
 * Various things
 */
bool character_generated;    /* The character exists */
bool character_loaded;        /* The character was loaded from a savefile */
bool character_saved;        /* The character was just saved to a savefile */

bool character_icky;        /* The game is in an icky full screen mode */
bool character_xtra;        /* The game is in an icky startup mode */

bool creating_savefile;        /* New savefile is currently created */

u32b seed_flavor;        /* Hack -- consistent object colors */

s16b command_cmd;        /* Current "Angband Command" */

s16b command_arg;        /* Gives argument of current command */
s16b command_rep;        /* Gives repetition of current command */
s16b command_dir;        /* Gives direction of current command */

s16b command_see;        /* See "object1.c" */
s16b command_wrk;        /* See "object1.c" */

s16b command_gap = 999;         /* See "object1.c" */

s16b command_new;        /* Command chaining from inven/equip view */

s16b energy_use;        /* Energy use this turn */

s16b running;            /* Current counter for running, if any */
s16b resting;            /* Current counter for resting, if any */

bool use_sound;            /* The "sound" mode is enabled */
bool use_graphics;        /* The "graphics" mode is enabled */
bool use_bigtile = FALSE;

s16b signal_count;        /* Hack -- Count interupts */

bool inkey_base;        /* See the "inkey()" function */
bool inkey_xtra;        /* See the "inkey()" function */
bool inkey_scan;        /* See the "inkey()" function */
bool inkey_flag;        /* See the "inkey()" function */
bool get_com_no_macros = FALSE;    /* Expand macros in "get_com" or not */

bool opening_chest;        /* Hack -- prevent chest generation */

bool shimmer_monsters;    /* Hack -- optimize multi-hued monsters */
bool shimmer_objects;    /* Hack -- optimize multi-hued objects */

bool repair_monsters;    /* Hack -- optimize detect monsters */
bool repair_objects;    /* Hack -- optimize detect objects */

bool hack_mind;

char summon_kin_type;   /* Hack, by Julian Lighton: summon 'relatives' */

/*
 * Software options (set via the '=' command). See "tables.c"
 */

/*** Input Options ***/

bool rogue_like_commands;    /* Rogue-like commands */
bool always_pickup;    /* Pick things up by default */
bool quick_messages;    /* Activate quick messages */
bool command_menu;    /* Enable command selection menu */
bool use_old_target;    /* Use old target by default */
bool auto_target;    /* Automatically target nearest monster */
bool always_repeat;    /* Repeat obvious commands */
bool confirm_destroy;    /* Prompt for destruction of known worthless items */
bool confirm_wear;    /* Confirm to wear/wield known cursed items */
bool target_pet;    /* Allow targetting pets */

#ifdef ALLOW_EASY_OPEN
bool easy_open;    /* Automatically open doors */
#endif

#ifdef ALLOW_EASY_DISARM
bool easy_disarm;    /* Automatically disarm traps */
#endif

bool auto_get_ammo;
bool auto_get_objects;
bool auto_detect_traps;

bool numpad_as_cursorkey;    /* Use numpad keys as cursor key in editor mode */
bool use_pack_slots;


/*** Map Screen Options ***/

bool center_player;    /* Center map while walking (*slow*) */
bool center_running;    /* Centering even while running */
bool view_light;
bool view_daylight;
bool view_gridlight;
bool view_perma_grids;    /* Map remembers all perma-lit grids */
bool view_torch_grids;    /* Map remembers all torch-lit grids */
bool view_unsafe_grids;    /* Map marked by detect traps */
bool fresh_before;    /* Flush output while continuous command */
bool fresh_after;    /* Flush output after monster's move */
bool fresh_message;    /* Flush output after every message */
bool hilite_player;    /* Hilite the player with the cursor */
bool display_path;    /* Display actual path before shooting */


/*** Text Display Options ***/

bool plain_descriptions;    /* Plain object descriptions */
bool always_show_list;    /* Always show list when choosing items */
bool depth_in_feet;    /* Show dungeon level in feet */
bool show_labels;    /* Show labels in object listings */
bool show_weights;    /* Show weights in object listings */
bool show_discounts;
bool show_item_graph;    /* Show items graphics */
bool equippy_chars;    /* Display 'equippy' chars */
bool display_food_bar;
bool display_hp_bar;
bool display_sp_bar;
bool display_light_bar;
bool compress_savefile;    /* Compress messages in savefiles */
bool abbrev_extra;    /* Describe obj's extra resistances by abbreviation */
bool abbrev_all;    /* Describe obj's all resistances by abbreviation */
bool exp_need;    /* Show the experience needed for next level */
bool ignore_unview;    /* Ignore whenever any monster does */
bool display_distance;
bool display_race; /* Display monster races with their racial char */


/*** Game-Play Options ***/

bool stack_force_notes;    /* Merge inscriptions when stacking */
bool stack_force_costs;    /* Merge discounts when stacking */
bool expand_list;    /* Expand the power of the list commands */
bool last_words;    /* Leave last words when your character dies */

bool allow_debug_opts;    /* Allow use of debug/cheat options */


/*** Disturbance Options ***/

bool find_ignore_stairs;    /* Run past stairs */
bool find_ignore_doors;    /* Run through open doors */
bool find_cut;    /* Run past known corners */
bool check_abort;    /* Check for user abort while continuous command */
bool flush_failure;    /* Flush input on various failures */
bool flush_disturb;    /* Flush input whenever disturbed */
bool disturb_move;    /* Disturb whenever any monster moves */
bool disturb_high;    /* Disturb whenever high-level monster moves */
bool disturb_near;    /* Disturb whenever viewable monster moves */
bool disturb_pets;    /* Disturb when visible pets move */
bool disturb_panel;    /* Disturb whenever map panel changes */
bool disturb_state;    /* Disturb whenever player state changes */
bool disturb_minor;    /* Disturb whenever boring things happen */
bool town_no_disturb;
bool ring_bell;    /* Audible bell (on errors, etc) */
bool disturb_trap_detect;    /* Disturb when leaving trap detected area */
bool alert_trap_detect;    /* Alert when leaving trap detected area */


/*** Birth Options ***/

bool smart_learn;    /* Monsters learn from their mistakes (*) */
bool smart_cheat;    /* Monsters exploit players weaknesses (*) */
bool allow_friendly_monster; /* Allow monsters friendly to player */
bool allow_hostile_monster; /* Allow monsters hostile to each other */
bool allow_pets; /* Allow pets: Note, this makes some classes unplayable. */
bool quest_unique; /* Random quests for unique monsters only */
bool random_artifacts;
byte random_artifact_pct = 100;
bool no_artifacts;

/*** Easy Object Auto-Destroyer ***/

bool destroy_items;    /* Use easy auto-destroyer */
bool destroy_debug;
bool destroy_feeling;    /* Apply auto-destroy as sense feeling */
bool destroy_identify;    /* Apply auto-destroy as identify an item */
bool leave_worth;    /* Auto-destroyer leaves known worthy items */
bool leave_equip;    /* Auto-destroyer leaves weapons and armour */
bool leave_chest;    /* Auto-destroyer leaves closed chests */
bool leave_wanted;    /* Auto-destroyer leaves wanted corpses */
bool leave_corpse;    /* Auto-destroyer leaves corpses and skeletons */
bool leave_junk;    /* Auto-destroyer leaves junk */
bool leave_special;    /* Auto-destroyer leaves items your race/class needs */

/* Special options */

byte hitpoint_warn = 3;    /* Hitpoint warning (0 to 9) */
byte mana_warn;    /* Mana color (0 to 9) */

int delay_animation = 10;
int delay_run = 5;
int delay_rest = 0;

bool autosave_l;    /* Autosave before entering new levels */
bool autosave_t;    /* Timed autosave */
s16b autosave_freq;     /* Autosave frequency */


/*
 * Dungeon variables
 */

bool closing_flag;        /* Dungeon is closing */


/*
 * Dungeon size info
 */
point_t viewport_origin;

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
 * 'savefile' holds full path name. 'savefile_base' holds only base name.
 */
char savefile[1024];
char savefile_base[40];

/*
 * Array of grids for use by various functions (see "cave.c")
 */
point_vec_ptr temp_pts;

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
 * The array of normal options
 */
u32b option_flag[8];
u32b option_mask[8];


/*
 * The array of window options
 */
u32b window_flag[8];
u32b window_mask[8];


/*
 * The array of window pointers
 */
term *angband_term[8];


/*
 * Standard window names
 */
char angband_term_name[8][16] =
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
    {0x00, 0x00, 0x00, 0x00},    /* TERM_DARK */
    {0x00, 0xFF, 0xFF, 0xFF},    /* TERM_WHITE */
    {0x00, 0x80, 0x80, 0x80},    /* TERM_SLATE */
    {0x00, 0xFF, 0x80, 0x00},    /* TERM_ORANGE */
    {0x00, 0xC0, 0x00, 0x00},    /* TERM_RED */
    {0x00, 0x00, 0x80, 0x40},    /* TERM_GREEN */
    {0x00, 0x00, 0x00, 0xFF},    /* TERM_BLUE */
    {0x00, 0x80, 0x40, 0x00},    /* TERM_UMBER */
    {0x00, 0x40, 0x40, 0x40},    /* TERM_L_DARK */
    {0x00, 0xC0, 0xC0, 0xC0},    /* TERM_L_WHITE */
    {0x00, 0xFF, 0x00, 0xFF},    /* TERM_VIOLET */
    {0x00, 0xFF, 0xFF, 0x00},    /* TERM_YELLOW */
    {0x00, 0xFF, 0x00, 0x00},    /* TERM_L_RED */
    {0x00, 0x00, 0xFF, 0x00},    /* TERM_L_GREEN */
    {0x00, 0x00, 0xFF, 0xFF},    /* TERM_L_BLUE */
    {0x00, 0xC0, 0x80, 0x40}    /* TERM_L_UMBER */
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
    "glass",
};


/*
 * The size of "alloc_kind_table" (at most max_k_idx * 4)
 */
s16b alloc_kind_size;

/*
 * The entries in the "kind allocator table"
 */
alloc_entry *alloc_kind_table;

/*
 * Specify attr/char pairs for visual special effects
 * Be sure to use "index & 0x7F" to avoid illegal access
 */
byte misc_to_attr[256];
char misc_to_char[256];


/*
 * Keymaps for each "mode" associated with each keypress.
 */
cptr keymap_act[KEYMAP_MODES][256];



/*** Player information ***/

/*
 * Pointer to the player tables
 * (sex, race, class, magic)
 */
player_magic *mp_ptr;


/*
 * The last character rolled,
 * holded for quick start
 */
birther previous_char;


/*
 * Room Templates (Vaults, Special Rooms, Wilderness Encounters)
 */
vec_ptr     room_info = NULL;
int_map_ptr room_letters = NULL;

/*
 * The magic info
 */
player_magic *m_info;

/*
 * The object kind arrays
 */
object_kind *k_info;

/*
 * The ego-item arrays
 */
ego_type *e_info;

/*
 * Hack -- The special Angband "System Suffix"
 * This variable is used to choose an appropriate "pref-xxx" file
 */
cptr ANGBAND_SYS = "xxx";

/*
 * Hack -- The special Angband "Keyboard Suffix"
 * This variable is used to choose an appropriate macro-trigger definition
 */
cptr ANGBAND_KEYBOARD = "0";

/*
 * Hack -- The special Angband "Graphics Suffix"
 * This variable is used to choose an appropriate "graf-xxx" file
 */
cptr ANGBAND_GRAF = "ascii";

/*
 * Path name: The main "lib" directory
 * This variable is not actually used anywhere in the code
 */
cptr ANGBAND_DIR;

/*
 * High score files (binary)
 * These files may be portable between platforms
 */
cptr ANGBAND_DIR_SCORES;

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


/*
 * Hack -- function hook to restrict "get_obj_num_prep()" function
 */
bool (*get_obj_num_hook)(int k_idx);
int obj_drop_theme;


/* Hack, monk armour */
bool monk_armour_aux;
bool monk_notify_aux;

#ifdef ALLOW_EASY_OPEN /* TNB */
bool easy_open;
#endif /* ALLOW_EASY_OPEN -- TNB */

#ifdef ALLOW_EASY_DISARM /* TNB */
bool easy_disarm;
#endif /* ALLOW_EASY_DISARM -- TNB */

bool center_player;
bool center_running;

/* Auto-destruction options */
bool destroy_items;
bool destroy_feeling;
bool destroy_identify;
bool leave_worth;
bool leave_equip;
bool leave_wanted;
bool leave_corpse;
bool leave_junk;
bool leave_chest;
bool leave_special;

/*
 * Maximum number of items in k_info.txt
 */
u16b max_k_idx;

/*
 * Maximum number of terrain features in f_info.txt
 */
u16b max_f_idx;

/*
 * Maximum number of ego-items in e_info.txt
 */
u16b max_e_idx;

/*
 * Default spell color table (quark index)
 */
s16b gf_color[GF_COUNT];

int mutant_regenerate_mod = 100;

bool can_save = FALSE;        /* Game can be saved */

mon_ptr world_monster;
bool world_player;

int cap_mon;
int cap_mspeed;
int cap_hp;
int cap_maxhp;
u16b cap_nickname;

sym_t kubi_r_idx[MAX_KUBI];
sym_t today_mon;

int tsuri_dir;

bool new_mane;

bool mon_fight;


s32b now_turn;
bool use_menu;


travel_type travel;

/* for snipers */
bool reset_concent = FALSE;   /* Concentration reset flag */
