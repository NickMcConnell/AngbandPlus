/* File: variable.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
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
cptr copyright =
	"Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Keoneke\n"
	"\n"
	"This software may be copied and distributed for educational, research,\n"
	"and not for profit purposes provided that this copyright and statement\n"
	"are included in all such copies.  Other copyrights may also apply.\n";


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
byte sf_major;			/* Savefile's "version_major" */
byte sf_minor;			/* Savefile's "version_minor" */
byte sf_patch;			/* Savefile's "version_patch" */
byte sf_extra;			/* Savefile's "version_extra" */

/* Game mode */
byte game_mode;			/* What game are we playing */

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
bool arg_rebalance;			/* Command arg -- Rebalance monsters */
bool arg_sound;				/* Command arg -- Request special sounds */
int arg_graphics;			/* Command arg -- Request graphics mode */
bool arg_force_original;	/* Command arg -- Request original keyset */
bool arg_force_roguelike;	/* Command arg -- Request roguelike keyset */

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
u32b seed_ghost;			/* Hack -- consistent player_ghosts */

s16b num_repro;			/* Current reproducer count */
s16b object_level;		/* Current object creation level */
s16b monster_level;		/* Current monster creation level */

char summon_kin_type;		/* Hack -- See summon_specific() */

monster_type *summoner; 	/*Track the current summoner*/

s32b turn;				/* Current game turn */

bool do_feeling;			/* Hack -- Level feeling counter */

int use_graphics;		/* The "graphics" mode is enabled */
bool use_bigtile = FALSE;

s16b image_count;  		/* Grids until next random image    */
                  		/* Optimizes the hallucination code */

s16b signal_count;		/* Hack -- Count interrupts */

bool msg_flag;			/* Player has pending message */

bool do_playtesting;

bool inkey_base;		/* See the "inkey()" function */
bool inkey_xtra;		/* See the "inkey()" function */
u32b inkey_scan;		/* See the "inkey()" function */
bool inkey_flag;		/* See the "inkey()" function */

s16b coin_type;			/* Hack -- force coin type */

byte object_generation_mode;/* Hack -- use different depth check, prevent embedded chests */

bool shimmer_monsters;	/* Hack -- optimize multi-hued monsters */
bool shimmer_objects;	/* Hack -- optimize multi-hued objects */
bool repair_mflag_show;	/* Hack -- repair monster flags (show) */
bool repair_mflag_mark;	/* Hack -- repair monster flags (mark) */

s16b o_max = 1;			/* Number of allocated objects */
s16b o_cnt = 0;			/* Number of live objects */

s16b mon_max = 1;	/* Number of allocated monsters */
s16b mon_cnt = 0;	/* Number of live monsters */

s16b x_max = 1;	/* Number of allocated effects */
s16b x_cnt = 0;	/* Number of live effects */


/*
 * TRUE if process_command() is a repeated call.
 */
bool command_repeating = FALSE;


/*
 * Dungeon variables
 */

byte feeling;			/* Most recent feeling */
s16b rating;			/* Level's current rating */

u32b  level_flag;		/* Level type */

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
 * Number of active macros.
 */
s16b macro__num;

/*
 * Array of macro patterns [MACRO_MAX]
 */
char **macro__pat;

/*
 * Array of macro actions [MACRO_MAX]
 */
char **macro__act;


/*
 * The array[ANGBAND_TERM_MAX] of window pointers
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

int max_macrotrigger = 0;
char *macro_template = NULL;
char *macro_modifier_chr;
char *macro_modifier_name[MAX_MACRO_MOD];
char *macro_trigger_name[MAX_MACRO_TRIGGER];
char *macro_trigger_keycode[2][MAX_MACRO_TRIGGER];


/*
 * Global table of color definitions (mostly zeros)
 */
byte angband_color_table[256][4] =
{
	{0x00, 0x00, 0x00, 0x00},	/* TERM_DARK - (black) - d */
	{0x00, 0xFF, 0xFF, 0xFF},	/* TERM_WHITE - w */
	{0x00, 0x80, 0x80, 0x80},	/* TERM_SLATE (Dark Gray)- s */
	{0x00, 0xFF, 0x80, 0x00},	/* TERM_ORANGE - o */
	{0x00, 0xC0, 0x00, 0x00},	/* TERM_RED - r*/
	{0x00, 0x00, 0x80, 0x40},	/* TERM_GREEN - g */
	{0x00, 0x00, 0x40, 0xFF},	/* TERM_BLUE - b */
	{0x00, 0x80, 0x40, 0x00},	/* TERM_UMBER - u */
	{0x00, 0x60, 0x60, 0x60},	/* TERM_L_DARK - D */
	{0x00, 0xC0, 0xC0, 0xC0},	/* TERM_L_WHITE - W */
	{0x00, 0xFF, 0x00, 0xFF},	/* TERM_VIOLET - v */
	{0x00, 0xFF, 0xFF, 0x00},	/* TERM_YELLOW - Y*/
	{0x00, 0xFF, 0x40, 0x40},	/* TERM_L_RED - R */
	{0x00, 0x00, 0xFF, 0x00},	/* TERM_L_GREEN  - G*/
	{0x00, 0x00, 0xFF, 0xFF},	/* TERM_L_BLUE - B */
	{0x00, 0xC0, 0x80, 0x40},	/* TERM_L_UMBER - U*/

	/*
	 * Values for shades at compile time, taken from shades.prf
	 */
	{0x00, 0x00, 0x00, 0x00},	/* 	16 - Unused */
	{0x00, 0xFF, 0xFA, 0xFA},	/* TERM_SNOW_WHITE 	(Shade 1 - w1) */
	{0x00, 0x70, 0x80, 0x90},	/* TERM_SLATE_GRAY 	(Shade 1 - s1) */
	{0x00, 0xFF, 0x9F, 0x00},	/* TERM_ORANGE_PEEL	(Shade 1 - o1) */
	{0x00, 0xCF, 0x10, 0x20},	/* TERM_RED_LAVA 	(Shade 1 - r1) */
	{0x00, 0x29, 0xAB, 0x87},	/* TERM_JUNGLE_GREEN (Shade 1 - g1) */
	{0x00, 0x4C, 0x4C, 0xA6},	/* TERM_NAVY_BLUE 	(Shade 1 - b1 */
	{0x00, 0x6D, 0x35, 0x1A},	/* TERM_AUBURN 		(Shade 1 - u1) */
	{0x00, 0x8B, 0x85, 0x89},	/* TERM_TAUPE 		(Shade 1)- D1 */
	{0x00, 0xE8, 0xD0, 0xC0},	/* TERM_L_WHITE_2	(Shade 1)- W1 */
	{0x00, 0xA5, 0x00, 0xFF},	/* TERM_PURPLE	 	(Shade 1)- v1*/
	{0x00, 0xFB, 0xEC, 0x5D},	/* TERM_MAIZE 		(Shade 1 - Y1) */
	{0x00, 0xE3, 0x0B, 0x5C},	/* TERM_RASPBERRY 	(Shade 1 - R1) */
	{0x00, 0xBF, 0xFF, 0x00},	/* TERM_LIME_GREEN  (Shade 1 - G1) */
	{0x00, 0x00, 0xBF, 0xFF},	/* TERM_SKY_BLUE  	(Shade 1 - B1 */
	{0x00, 0xC1, 0x9A, 0x6B}, 	/* TERM_L_BROWN		(Shade 1 - Fallow - U1) */

	{0x00, 0x00, 0x00, 0x00},	/* 	32 - Unused */
	{0x00, 0x00, 0x00, 0x00},	/* 	33 - Unused */
	{0x00, 0xC0, 0xC0, 0xC0},	/* TERM_SILVER 		(shade 2 - s2) */
	{0x00, 0xC0, 0x40, 0x00},	/* TERM_MAHAGONY 	(Shade 2 - o2) */
	{0x00, 0xB7, 0x41, 0x0E},	/* TERM_RED_RUST 	(Shade 2 - r2) */
	{0x00, 0x00, 0x00, 0x00},	/* 	37 - Unused */
	{0x00, 0x00, 0x00, 0x00},	/* 	38 - Unused */
	{0x00, 0xB8, 0x73, 0x33},	/* TERM_COPPER 		(Shade 2 - u2) */
	{0x00, 0x00, 0x00, 0x00},	/* 	40 - Unused */
	{0x00, 0x00, 0x00, 0x00},	/* 	41 - Unused */
	{0x00, 0x00, 0x00, 0x00},	/* 	42 - Unused */
	{0x00, 0xFF, 0xD7, 0x00},	/* TERM_GOLD 		(Shade 2 - Y2) */
	{0x00, 0xFF, 0x14, 0x93},	/* TERM_PINK 		(Shade 2 - R2) */
	{0x00, 0x00, 0x00, 0x00},	/* 	45 - Unused */
	{0x00, 0x00, 0x00, 0x00},	/* 	46 - Unused */
	{0x00, 0xE1, 0xA9, 0x5F} 	/* TERM_EARTH_YELLOW (Shade 2 - U2) */
};

/*
 * Standard sound (and message) names
 */
const cptr angband_sound_name[MSG_MAX] =
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
	"zap_rod",
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
	"stairs_down",
	"hitpoint_warn",
	"act_artifact",
	"use_staff",
	"destroy",
	"mon_hit",
	"mon_touch",
	"mon_punch",
	"mon_kick",
	"mon_claw",
	"mon_bite",
	"mon_sting",
	"mon_butt",
	"mon_crush",
	"mon_engulf",
	"mon_crawl",
	"mon_drool",
	"mon_spit",
	"mon_gaze",
	"mon_wail",
	"mon_spore",
	"mon_beg",
	"mon_insult",
	"mon_moan",
	"recover",
	"blind",
	"confused",
	"poisoned",
	"afraid",
	"paralyzed",
	"drugged",
	"speed",
	"slow",
	"shield",
	"blessed",
	"hero",
	"berserk",
	"prot_evil",
	"invuln",
	"see_invis",
	"infrared",
	"res_acid",
	"res_elec",
	"res_fire",
	"res_cold",
	"res_pois",
	"stun",
	"cut",
	"stairs_up",
	"store_enter",
	"store_leave",
	"store_home",
	"money1",
	"money2",
	"money3",
	"shoot_hit",
	"store5",
	"lockpick",
	"disarm",
	"identify_bad",
	"identify_ego",
	"identify_art",
	"breathe_elements",
	"breathe_frost",
	"breathe_elec",
	"breathe_acid",
	"breathe_gas",
	"breathe_fire",
	"breathe_confusion",
	"breathe_disenchant",
	"breathe_chaos",
	"breathe_shards",
	"breathe_sound",
	"breathe_light",
	"breathe_dark",
	"breathe_nether",
	"breathe_nexus",
	"breathe_time",
	"breathe_inertia",
	"breathe_gravity",
	"breathe_plasma",
	"breathe_force",
	"summon_monster",
	"summon_angel",
	"summon_undead",
	"summon_animal",
	"summon_spider",
	"summon_hound",
	"summon_hydra",
	"summon_demon",
	"summon_dragon",
	"summon_gr_undead",
	"summon_gr_dragon",
	"summon_gr_demon",
	"summon_ringwraith",
	"summon_unique",
	"wield",
	"cursed",
	"pseudo_id",
	"hungry",
	"notice",
	"ambient_day",
	"ambient_nite",
	"ambient_dng1",
	"ambient_dng2",
	"ambient_dng3",
	"ambient_dng4",
	"ambient_dng5",
	"mon_create_trap",
	"mon_shriek",
	"mon_cast_fear",
	"hit_good",
	"hit_great",
	"hit_superb",
	"hit_hi_great",
	"hit_hi_superb",
	"cast_spell",
	"pray_prayer",
	"kill_unique",
	"kill_king",
	"drain_stat",
	"multiply",
	"losing_nativity",
	"losing_flying",
	"hide_unhide",
};



/*
 * Array[VIEW_MAX] used by "update_view()"
 */
int view_n = 0;
u16b *view_g;


/*
 * Array[VIEW_MAX] used by "update_view()"
 */
int fire_n = 0;
u16b *fire_g;

/*
 * Arrays[TEMP_MAX] used for various things
 *
 * Note that temp_g shares memory with temp_x and temp_y.
 */
int temp_n = 0;
u16b *temp_g;
byte *temp_y;
byte *temp_x;

/* The counter for monster energy & the order in which it should be processed*/
int *move_moment;

/*
 * Array[z_info->x_max] of dungeon effects
 */
effect_type *x_list;


/*
 * Array[DUNGEON_HGT][256] of cave grid info flags (padded)
 *
 * This array is padded to a width of 256 to allow fast access to elements
 * in the array via "grid" values (see the GRID() macros).
 */
u16b (*cave_info)[256];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid feature codes
 */
byte (*cave_feat)[MAX_DUNGEON_WID];


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
s16b (*cave_o_idx)[MAX_DUNGEON_WID];

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
s16b (*cave_m_idx)[MAX_DUNGEON_WID];

/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid effect indexes
 *
 * Note that this array yields the index of the top object in the stack of
 * effects in a given grid, using the "next_x_idx" field in that effect to
 * indicate the next effect in the stack, and so on, using zero to indicate
 * "nothing".  This array replicates the information contained in the effect
 * list, for efficiency, providing extremely fast determination of whether
 * any effect is in a grid, and relatively fast determination of which effects
 * are in a grid.
 */

s16b (*cave_x_idx)[MAX_DUNGEON_WID];

/*
 * Table of avergae monster power.
 * Used to hep determine a suitable quest monster.
 */

u32b mon_power_ave[MAX_DEPTH_ALL][CREATURE_TYPE_MAX];




/*
 * Arrays[NUM_FLOWS][DUNGEON_HGT][DUNGEON_WID] of cave grid flow "cost" values
 */
u16b cave_cost[MAX_FLOWS][MAX_DUNGEON_HGT][MAX_DUNGEON_WID];


/*
 * Flow cost at the center grid of the current update.
 */
int cost_at_center[MAX_FLOWS];




#ifdef	MONSTER_SMELL
/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cave grid flow "when" stamps
 */
byte (*cave_when)[MAX_DUNGEON_WID];

/*
 * Current scent age marker.  Counts down from 250 to 0 and then loops.
 */
int scent_when = 250;

#endif	/* MONSTER_SMELL */

/*
 * The character generates both directed (extra) noise (by doing noisy
 * things) and ambient noise (the combination of directed and innate
 * noise).
 *
 * Noise builds up as the character does certain things, and diminishes
 * over time.
 */
s16b add_wakeup_chance = 0;
s16b total_wakeup_chance = 0;


/*
 * Array[z_info->o_max] of dungeon objects
 */
object_type *o_list;

/*
 * Array[z_info->m_max] of dungeon monsters
 */
monster_type *mon_list;

/*
 * Array[z_info->r_max] of monster lore
 */
monster_lore *l_list;


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
 * for ball spells and bolt spells
 */
byte color_to_attr[2][MAX_COLOR_USED];
char color_to_char[2][MAX_COLOR_USED];


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
char *keymap_act[KEYMAP_MODES][256];



/*** Player information ***/

/*
 * Pointer to the player tables (sex, race, class, magic)
 */
const player_sex *sp_ptr;
const player_race *rp_ptr;
player_class *cp_ptr;
player_magic *mp_ptr;

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
 * The terrain lore arrays
 */
feature_lore *f_l_list;

/*
 * The object kind arrays
 */
object_kind *k_info;
char *k_name;
char *k_text;

/*
 * The ghost template arrays
 */
ghost_template *t_info;
char *t_name;
char *t_text;


/*
 * The artifact arrays
 */
artifact_type *a_info;
artifact_lore *a_l_list;
char *a_text;

/*
 * The random name generator tables
 */
names_type *n_info;

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
 * The quest arrays
 */
quest_type *q_info;
char *q_name;



/*
 * The object flavor arrays
 */
flavor_type *flavor_info;
char *flavor_name;
char *flavor_text;

/*Monster_movement energy info*/
move_moment_type *mon_moment_info;
u16b move_moment_num;

/*
 * Hack -- The special Angband "System Suffix"
 * This variable is used to choose an appropriate "pref-xxx" file
 */
const char *ANGBAND_SYS = "xxx";

/*
 * Hack -- The special Angband "Graphics Suffix"
 * This variable is used to choose an appropriate "graf-xxx" file
 */
const char *ANGBAND_GRAF = "old";

/*
 * Path name: The main "lib" directory
 * This variable is not actually used anywhere in the code
 */

char *ANGBAND_DIR;

/*
 * High score files (binary)
 * These files may be portable between platforms
 */
char *ANGBAND_DIR_APEX;
char *ANGBAND_DIR_BONE;
char *ANGBAND_DIR_DATA;
char *ANGBAND_DIR_EDIT;
char *ANGBAND_DIR_FILE;
char *ANGBAND_DIR_HELP;
char *ANGBAND_DIR_INFO;
char *ANGBAND_DIR_SAVE;
char *ANGBAND_DIR_PREF;
char *ANGBAND_DIR_USER;
char *ANGBAND_DIR_XTRA;

/*
 * Various xtra/ subdirectories.
 */
char *ANGBAND_DIR_XTRA_FONT;
char *ANGBAND_DIR_XTRA_GRAF;
char *ANGBAND_DIR_XTRA_SOUND;
char *ANGBAND_DIR_XTRA_HELP;
char *ANGBAND_DIR_XTRA_ICON;

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
 * Specifies whether the item tester should allow the swap weapon or not.
 * For example, the player shouldn't be allowed to activate a swap weapon.
 */
bool item_tester_swap;


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



/*
 * Hack -- function hook to restrict "get_mon_num_prep()" function
 */
bool (*get_mon_num_hook)(int r_idx);



/*
 * Hack -- function hook to restrict "get_obj_num_prep()" function
 */
bool (*get_obj_num_hook)(int k_idx);


void (*object_info_out_flags)(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native);

/*
 * Hack -- function hook to restrict "get_feat_num_prep()" function
 */
bool (*get_feat_num_hook)(int f_idx);



/*
 * Hack - the destination file for text_out_to_file.
 */
ang_file *text_out_file = NULL;


/*
 * Hack -- function hook to output (colored) text to the
 * screen or to a file.
 */
void (*text_out_hook)(byte a, cptr str);


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


/*
 * Sound hook (for playing FX).
 */
void (*sound_hook)(int sound);


/*
 * Buffer to hold the current notes file name
 */

char notes_fname[1024];

/*
 * File for taking notes
 */
ang_file *notes_file;


 /* Two variables that limit rogue stealing and creation of traps.
 * Cleared when a level is created. {From Oangband} -JG
 */
byte num_trap_on_level;
u16b altered_inventory_counter;
bool allow_altered_inventory;


/*
 * For summon spells, which summon spells have failed on levels where summoning is restricted
 * to current level monsters and respawning is prohibited.
 */
u32b dungeon_summon_mask_f7;

autoinscription* inscriptions = 0;
u16b inscriptionsCount = 0;


/* The entry in a restored player ghost should use to collect extra
 * flags, a sex, and a unique name.  Any value above -1 indicates that there is
 * a ghost active.
 */
s16b player_ghost_num;

/*
 * The r_idx of the active player ghost template, if any.
 */
s16b ghost_r_idx;

/*
 * The player ghost name is stored here for quick reference by the
 * description function.  -LM-
 */
char player_ghost_name[80];



/*
 * The name of the current greater vault, if any. -DG-
 */
char g_vault_name[80];


/*
 * The array of dynamic grids. -DG-
 */
dynamic_grid_type *dyna_g = NULL;

/*
 * Current number of grids in dyna_g
 */
u16b dyna_cnt = 0;

/*
 * The index of the entry that is after the last entry of dyna_g
 */
u16b dyna_next = 0;

/*
 * If dyna_full is FALSE, all the dynamic grids on the level are stored in
 * dyna_g. If it's TRUE, the contents of dyna_g are updated every certain
 * number of steps (see process_dynamic_terrain for this)
 */
bool dyna_full = FALSE;

/*
 * When dyna_full is TRUE, we need to track the center of the last update
 */
byte dyna_center_y = 255;
byte dyna_center_x = 255;

/*
 * The array used to store stacked monster messages
 */
monster_race_message *mon_msg;

monster_message_history *mon_message_hist;

/*
 * The current size of that array
 */
u16b size_mon_msg;
u16b size_mon_hist;

/*
 * Some static info used to manage quiver groups
 */
quiver_group_type quiver_group[MAX_QUIVER_GROUPS] =
{
	{'f', TERM_L_BLUE},
	{'f', TERM_L_GREEN},
	{'f', TERM_YELLOW},
	{'v', TERM_ORANGE},
};


/*
 * Number of player turns the quest indicator is displayed
 * when the player fails or wins a quest.
 */
u16b quest_indicator_timer = 0;

/*
 * Remember what is being displayed on each row on the side of the screen.
 */
int sidebar_details[SIDEBAR_MAX_TYPES];


/*
 * Remember what is being displayed on each row on the side of the screen.
 */
int sidebar_monsters[SIDEBAR_MONSTER_MAX];

/*
 * It's TRUE if the player won a quest.
 */
byte quest_indicator_complete = FALSE;

/*
 * Panel change offsets. See verify_panel
 */
u16b panel_change_offset_y = MIN_PANEL_CHANGE_OFFSET_Y;
u16b panel_change_offset_x = MIN_PANEL_CHANGE_OFFSET_X;



/*
 * The current capabilities of the dungeon
 */
dungeon_capabilities_type *dun_cap = NULL;

/* Delay in centiseconds before moving to allow another keypress */
/* Zero means normal instant movement. */
u16b lazymove_delay =0;


