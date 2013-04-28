/* File: variable.c */

/*
 * Brief version of copyright.  Global variables and arrays.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"

/*
 * Hack -- Link a copyright message into the executable
 */
cptr copyright =
	"Copyright (c) 2007 Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke\n"
	"\n"
	"This program is free software; you can redistribute it and/or modify it\n"
	"under the terms of the GNU General Public License.  Some parts may\n"
	"also be available under the terms of the Moria license.  Other copyrights\n"
	"may apply.\n";


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
int arg_sound;					/* Command arg -- Request special sounds */
int arg_graphics = GRAPHICS_FONT;	/* Command arg -- Request graphics */
bool arg_force_original;		/* Command arg -- Request original keyset */
bool arg_force_roguelike;		/* Command arg -- Request roguelike keyset */



	/* Game interface variables */

int use_sound = 0;				/* Does the game play sounds?  What kind? */
bool use_mouse = FALSE;			/* Is Mouse input recognized? */
int use_graphics;				/* What graphics display are we showing? */
int cursor_shape;				/* Cursor shape */

bool use_special_map = FALSE;  /* Use a dedicated map display term */


	/* Various things */

bool character_generated;		/* The character exists */
bool character_dungeon;			/* The character has a dungeon */
bool character_loaded;			/* The character was loaded from a savefile */
bool character_saved;			/* The character was just saved to a savefile */
bool character_existed;			/* A character once existed on this savefile */

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
 * Whether we are showing a full-screen view.
 */
bool use_fullscreen_view = FALSE;

/*
 * Whether we are using the tall display term.  Set by "switch_display()".
 */
bool use_tall_display;

/*
 * The current screen depth
 */
int screen_depth = 0;

/*
 * Whether it is safe to perform any updates to the main view (map, left panel,
 * status bar, etc.).  Values other than 0 forbid updates.  If the screen is
 * locked, it is always -1.  Otherwise, the screen saving and loading code
 * toggle this value between 0 and 1.
 *
 * (was "character_icky")
 */
int main_screen_inactive = 0;

/*
 * Number of rows used to display the dungeon.
 * Must be at least 22, and should be a multiple of BLOCK_HGT (currently 11).
 */
s16b map_rows = 22;

/*
 * Number of columns used to display the dungeon.
 * Must be at least 22, and should be a multiple of BLOCK_WID (currently 11).
 */
s16b map_cols = 22;

/*
 * Option:  Allow more interfaces to use the tall display (more useable
 * rows), the show file system in particular.
 */
bool more_tall_display = FALSE;


/*
 * Option:  Precisely fit the map display to screen.  Has both advantages and
 * disadvantages.
 */
bool map_display_precise_fit = FALSE;

/*
 * Allow player to precisely control how close to the edge of the map
 * the character must move before the screen automatically shifts.
 */
s16b clear_y = 2;
s16b clear_x = 4;


s16b image_count = 0;          /* Grids until next random image    */
                               /* Optimizes the hallucination code */

s16b signal_count;		/* Hack -- Count interrupts */

bool msg_flag;					/* Player has pending message */

bool inkey_base;				/* See the "inkey()" function */
bool inkey_xtra;				/* See the "inkey()" function */
bool inkey_scan;				/* See the "inkey()" function */
bool inkey_flag;				/* See the "inkey()" function */

/* A brute-force solution to the problem of cursor visibility */
bool inkey_cursor_hack[TERM_MAX];		/* See the "inkey()" function */

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
 * The array of pointers to display terminals
 */
term *angband_term[TERM_MAX];



/*
 * The array[TERM_MAX] of window names (modifiable?)
 *
 * ToDo: Make the names independent of TERM_MAX.
 */
char angband_term_name[TERM_MAX+1][40] =
{
	"Main screen",
	"Map",
	"Win-1",
	"Win-2",
	"Win-3",
	"Win-4",
	"Win-5",
	"Win-6",
	"Win-7",
	"Win-8",
	"Display"
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
cptr angband_sound_name[MSG_MAX] =
{
	"",                    /* MSG_GENERIC */
	"hit",                 /* MSG_HIT */
	"miss",                /* MSG_MISS */
	"flee",                /* MSG_FLEE */
	"drop",                /* MSG_DROP */
	"kill",                /* MSG_KILL */
	"level",               /* MSG_LEVEL */
	"death",               /* MSG_DEATH */
	"study",               /* MSG_STUDY */
	"teleport",            /* MSG_TELEPORT */
	"shoot",               /* MSG_SHOOT */
	"quaff",               /* MSG_QUAFF */
	"",
	"walk",                /* MSG_WALK */
	"tpother",             /* MSG_TPOTHER */
	"hitwall",             /* MSG_HITWALL */
	"eat",                 /* MSG_EAT */
	"store1",              /* MSG_STORE1 */
	"store2",              /* MSG_STORE2 */
	"store3",              /* MSG_STORE3 */
	"store4",              /* MSG_STORE4 */
	"dig",                 /* MSG_DIG */
	"opendoor",            /* MSG_OPENDOOR */
	"shutdoor",            /* MSG_SHUTDOOR */
	"tplevel",             /* MSG_TPLEVEL */
	"bell",                /* MSG_BELL */
	"nothing_to_open",     /* MSG_NOTHING_TO_OPEN */
	"lockpick_fail",       /* MSG_LOCKPICK_FAIL */
	"stairs_down",         /* MSG_STAIRS_DOWN */
	"hitpoint_warn",       /* MSG_HITPOINT_WARN */
	"act_artifact",        /* MSG_ACT_ARTIFACT */
	"",
	"destroy",             /* MSG_DESTROY */
	"mon_hit",             /* MSG_MON_HIT */
	"mon_touch",           /* MSG_MON_TOUCH */
	"mon_punch",           /* MSG_MON_PUNCH */
	"mon_kick",            /* MSG_MON_KICK */
	"mon_claw",            /* MSG_MON_CLAW */
	"mon_bite",            /* MSG_MON_BITE */
	"mon_sting",           /* MSG_MON_STING */
	"mon_butt",            /* MSG_MON_BUTT */
	"mon_crush",           /* MSG_MON_CRUSH */
	"mon_engulf",          /* MSG_MON_ENGULF */
	"mon_crawl",           /* MSG_MON_CRAWL */
	"mon_drool",           /* MSG_MON_DROOL */
	"mon_spit",            /* MSG_MON_SPIT */
	"mon_gaze",            /* MSG_MON_GAZE */
	"mon_wail",            /* MSG_MON_WAIL */
	"mon_spore",           /* MSG_MON_SPORE */
	"mon_beg",             /* MSG_MON_BEG */
	"mon_insult",          /* MSG_MON_INSULT */
	"mon_moan",            /* MSG_MON_MOAN */
	"recover",             /* MSG_RECOVER */
	"blind",               /* MSG_BLIND */
	"confused",            /* MSG_CONFUSED */
	"poisoned",            /* MSG_POISONED */
	"afraid",              /* MSG_AFRAID */
	"paralyzed",           /* MSG_PARALYZED */
	"drugged",             /* MSG_DRUGGED */
	"speed",               /* MSG_SPEED */
	"slow",                /* MSG_SLOW */
	"shield",              /* MSG_SHIELD */
	"blessed",             /* MSG_BLESSED */
	"hero",                /* MSG_HERO */
	"berserk",             /* MSG_BERSERK */
	"prot_evil",           /* MSG_PROT_EVIL */
	"invuln",              /* MSG_INVULN */
	"see_invis",           /* MSG_SEE_INVIS */
	"infrared",            /* MSG_INFRARED */
	"res_acid",            /* MSG_RES_ACID */
	"res_elec",            /* MSG_RES_ELEC */
	"res_fire",            /* MSG_RES_FIRE */
	"res_cold",            /* MSG_RES_COLD */
	"res_pois",            /* MSG_RES_POIS */
	"stun",                /* MSG_STUN */
	"cut",                 /* MSG_CUT */
	"stairs_up",           /* MSG_STAIRS_UP */
	"store_enter",         /* MSG_STORE_ENTER */
	"store_leave",         /* MSG_STORE_LEAVE */
	"store_home",          /* MSG_STORE_HOME */
	"money1",              /* MSG_MONEY1 */
	"money2",              /* MSG_MONEY2 */
	"money3",              /* MSG_MONEY3 */
	"shoot_hit",           /* MSG_SHOOT_HIT */
	"store5",              /* MSG_STORE5 */
	"lockpick",            /* MSG_LOCKPICK */
	"disarm",              /* MSG_DISARM */
	"identify_bad",        /* MSG_IDENT_BAD */
	"identify_ego",        /* MSG_IDENT_EGO */
	"identify_art",        /* MSG_IDENT_ART */
	"breathe_elements",    /* MSG_BR_ELEMENTS */
	"breathe_frost",       /* MSG_BR_FROST */
	"breathe_elec",        /* MSG_BR_ELEC */
	"breathe_acid",        /* MSG_BR_ACID */
	"breathe_gas",         /* MSG_BR_GAS */
	"breathe_fire",        /* MSG_BR_FIRE */
	"breathe_confusion",   /* MSG_BR_CONF */
	"breathe_disenchant",  /* MSG_BR_DISENCHANT */
	"breathe_chaos",       /* MSG_BR_CHAOS */
	"breathe_shards",      /* MSG_BR_SHARDS */
	"breathe_sound",       /* MSG_BR_SOUND */
	"breathe_light",       /* MSG_BR_LIGHT */
	"breathe_dark",        /* MSG_BR_DARK */
	"breathe_nether",      /* MSG_BR_NETHER */
	"breathe_nexus",       /* MSG_BR_NEXUS */
	"breathe_time",        /* MSG_BR_TIME */
	"breathe_inertia",     /* MSG_BR_INERTIA */
	"breathe_gravity",     /* MSG_BR_GRAVITY */
	"breathe_plasma",      /* MSG_BR_PLASMA */
	"breathe_force",       /* MSG_BR_FORCE */
	"summon_monster",      /* MSG_SUM_MONSTER */
	"summon_angel",        /* MSG_SUM_ANGEL */
	"summon_undead",       /* MSG_SUM_UNDEAD */
	"summon_animal",       /* MSG_SUM_ANIMAL */
	"summon_spider",       /* MSG_SUM_SPIDER */
	"summon_hound",        /* MSG_SUM_HOUND */
	"summon_hydra",        /* MSG_SUM_HYDRA */
	"summon_demon",        /* MSG_SUM_DEMON */
	"summon_dragon",       /* MSG_SUM_DRAGON */
	"summon_gr_undead",    /* MSG_SUM_HI_UNDEAD */
	"summon_gr_dragon",    /* MSG_SUM_HI_DRAGON */
	"summon_gr_demon",     /* MSG_SUM_HI_DEMON */
	"summon_ringwraith",   /* MSG_SUM_WRAITH */
	"summon_unique",       /* MSG_SUM_UNIQUE */
	"wield",               /* MSG_WIELD */
	"cursed",              /* MSG_CURSED */
	"pseudo_id",           /* MSG_PSEUDOID */
	"hungry",              /* MSG_HUNGRY */
	"notice",              /* MSG_NOTICE */
	"ambient_day",         /* MSG_AMBIENT_DAY */
	"ambient_nite",        /* MSG_AMBIENT_NITE */
	"ambient_dng1",        /* MSG_AMBIENT_DNG1 */
	"ambient_dng2",        /* MSG_AMBIENT_DNG2 */
	"ambient_dng3",        /* MSG_AMBIENT_DNG3 */
	"ambient_dng4",        /* MSG_AMBIENT_DNG4 */
	"ambient_dng5",        /* MSG_AMBIENT_DNG5 */
	"mon_create_trap",     /* MSG_CREATE_TRAP */
	"mon_shriek",          /* MSG_SHRIEK */
	"mon_cast_fear",       /* MSG_CAST_FEAR */
	"hit_good",            /* MSG_HIT_GOOD */
	"hit_great",           /* MSG_HIT_GREAT */
	"hit_superb",          /* MSG_HIT_SUPERB */
	"hit_hi_great",        /* MSG_HIT_HI_GREAT */
	"hit_hi_superb",       /* MSG_HIT_HI_SUPERB */
	"cast_spell",          /* MSG_SPELL */
	"pray_prayer",         /* MSG_PRAYER */
	"kill_unique",         /* MSG_KILL_UNIQUE */
	"kill_king",           /* MSG_KILL_KING */
	"drain_stat",          /* MSG_DRAIN_STAT */
	"multiply",            /* MSG_MULTIPLY */
	"mon_blow_soft",       /* MSG_HIT_SOFT */
	"mon_blow_medium",     /* MSG_HIT_MEDIUM */
	"mon_blow_hard",       /* MSG_HIT_HARD */
	"mon_blow_deadly",     /* MSG_HIT_DEADLY */
	"",                    /* MSG_ */
	"use_staff",           /* MSG_USE_STAFF */
	"aim_wand",            /* MSG_AIM_WAND */
	"zap_rod",             /* MSG_ZAP_ROD */
	"yell_for_help",       /* MSG_YELL_FOR_HELP */
	"spit",                /* MSG_SPIT */
	"whip",                /* MSG_WHIP */
	"boulder",             /* MSG_BOULDER */
	"shot",                /* MSG_SHOT */
	"arrow",               /* MSG_ARROW */
	"bolt",                /* MSG_BOLT */
	"missl",               /* MSG_MISSL */
	"pmissl",              /* MSG_PMISSL */
	"br_wind",             /* MSG_BR_WIND */
	"br_mana",             /* MSG_BR_MANA */
	"diseased",            /* MSG_DISEASED */
	"steelskin",           /* MSG_STEELSKIN */
	"holy",                /* MSG_HOLY */
	"necro_rage",          /* MSG_NECRO_RAGE */
	"wiz_prot",            /* MSG_WIZ_PROT */
	"invis",               /* MSG_INVIS */
	"mania",               /* MSG_MANIA */
	"res_dam",             /* MSG_RES_DAM */
	"res_ethereal",        /* MSG_RES_ETHEREAL */
	"",                    /*  */
	"",                    /*  */
	"",                    /*  */
	"",                    /*  */
	"",   "",   "",   "",   "",
	"",   "",   "",   "",   "",
	"",   "",   "",   "",   "",
	"",   "",
	"",                    /* MSG_DARK */
	"",                    /* MSG_WHITE */
	"",                    /* MSG_SLATE */
	"",                    /* MSG_ORANGE */
	"",                    /* MSG_RED */
	"",                    /* MSG_GREEN */
	"",                    /* MSG_BLUE */
	"",                    /* MSG_UMBER */
	"",                    /* MSG_L_DARK */
	"",                    /* MSG_L_WHITE */
	"",                    /* MSG_L_PURPLE */
	"",                    /* MSG_YELLOW */
	"",                    /* MSG_L_RED */
	"",                    /* MSG_L_GREEN */
	"",                    /* MSG_L_BLUE */
	"",                    /* MSG_L_UMBER */
	""
};


/*
 * Musical theme names
 */
cptr angband_music_name[MUSIC_MAX] =
{
	"town",                /* MUSIC_TOWN */
	"peaceful",            /* MUSIC_PEACEFUL */
	"light",               /* MUSIC_LIGHT */
	"medium",              /* MUSIC_MEDIUM */
	"heavy",               /* MUSIC_HEAVY */
	"deadly",              /* MUSIC_DEADLY */
	"death",               /* MUSIC_DEATH */
	"",                    /* MUSIC_XXX1 */
	"",                    /* MUSIC_XXX2 */
	"",                    /* MUSIC_XXX3 */
	"",                    /* MUSIC_XXX4 */
	"",                    /* MUSIC_XXX5 */
	"",                    /* MUSIC_XXX6 */
	"",                    /* MUSIC_XXX7 */
	"",                    /* MUSIC_XXX8 */
	""
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
u16b path_g[128];  /* Grids in the projection path */
byte path_gx[128];  /* Special information about each grid */
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
 * The character generates both directed (extra) noise (by doing noisy things)
 * and overall noise (the combination of directed and innate noise).
 *
 * Noise builds up as the character does certain things and diminishes over
 * time.
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
 * Hack -- function hook to switch between the main and the map display.
 */
errr (*switch_display_hook)(int display) = NULL;

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
 * Values for use with "text_out()".  Line wrapping (0 wraps to term area),
 * indenting, and left margin (note that this function always retains a 1
 * space right margin).
 */
int text_out_wrap = 0;
int text_out_indent = 0;
int text_border_left = 0;


/*
 * An interface-configurable hook to reset the display to show a given size, if possible.
 *
 * This hook must always be optional.
 */
void (*special_view_hook)(int cols, int rows, bool activate) = NULL;



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
char ghost_name[DESC_LEN];


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



byte player_graphics[MAX_RACES][MAX_SPECIALTIES][2];   /* Array of player graphics */


