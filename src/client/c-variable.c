/*
 * File: c-variable.c
 * Purpose: Various global variables
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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

#include "c-angband.h"

/*
 * The array[ANGBAND_TERM_MAX] of window pointers
 */
term *angband_term[ANGBAND_TERM_MAX];

/*
 * The array[ANGBAND_TERM_MAX] of window names
 */
const char *angband_term_name[ANGBAND_TERM_MAX] =
{
    "Main window",
    "Term-1",
    "Term-2",
    "Term-3",
    "Chat window",
    "Term-5",
    "Term-6",
    "Term-7"
};

/*
 * Global table of color definitions (mostly zeros)
 */
byte angband_color_table[MAX_COLORS][4] =
{
    {0x00, 0x00, 0x00, 0x00}, /* 0  TERM_DARK */
    {0x00, 0xff, 0xff, 0xff}, /* 1  TERM_WHITE */
    {0x00, 0x80, 0x80, 0x80}, /* 2  TERM_SLATE */
    {0x00, 0xff, 0x80, 0x00}, /* 3  TERM_ORANGE */
    {0x00, 0xc0, 0x00, 0x00}, /* 4  TERM_RED */
    {0x00, 0x00, 0x80, 0x40}, /* 5  TERM_GREEN */
    {0x00, 0x00, 0x40, 0xff}, /* 6  TERM_BLUE */
    {0x00, 0x80, 0x40, 0x00}, /* 7  TERM_UMBER */
    {0x00, 0x60, 0x60, 0x60}, /* 8  TERM_L_DARK */
    {0x00, 0xc0, 0xc0, 0xc0}, /* 9  TERM_L_WHITE */
    {0x00, 0xff, 0x00, 0xff}, /* 10 TERM_L_PURPLE */
    {0x00, 0xff, 0xff, 0x00}, /* 11 TERM_YELLOW */
    {0x00, 0xff, 0x40, 0x40}, /* 12 TERM_L_RED */
    {0x00, 0x00, 0xff, 0x00}, /* 13 TERM_L_GREEN */
    {0x00, 0x00, 0xff, 0xff}, /* 14 TERM_L_BLUE */
    {0x00, 0xc0, 0x80, 0x40}, /* 15 TERM_L_UMBER */
    {0x00, 0x90, 0x00, 0x90}, /* 16 TERM_PURPLE */
    {0x00, 0x90, 0x20, 0xff}, /* 17 TERM_VIOLET */
    {0x00, 0x00, 0xa0, 0xa0}, /* 18 TERM_TEAL */
    {0x00, 0x6c, 0x6c, 0x30}, /* 19 TERM_MUD */
    {0x00, 0xff, 0xff, 0x90}, /* 20 TERM_L_YELLOW */
    {0x00, 0xff, 0x00, 0xa0}, /* 21 TERM_MAGENTA */
    {0x00, 0x20, 0xff, 0xdc}, /* 22 TERM_L_TEAL */
    {0x00, 0xb8, 0xa8, 0xff}, /* 23 TERM_L_VIOLET */
    {0x00, 0xff, 0x80, 0x80}, /* 24 TERM_L_PINK */
    {0x00, 0xb4, 0xb4, 0x00}, /* 25 TERM_MUSTARD */
    {0x00, 0xa0, 0xc0, 0xd0}, /* 26 TERM_BLUE_SLATE */
    {0x00, 0x00, 0xb0, 0xff}, /* 27 TERM_DEEP_L_BLUE */
};

/*
 * Standard sound (and message) names
 */
const char *angband_sound_name[MSG_MAX] =
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
    "bold", 
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
    "breathe_water",
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
    "summon_ainu", 
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
    "wild_shore",
    "wild_grass",
    "wild_wood",
    "wild_swamp",
    "wild_waste",
    "wild_mountain",
    "wild_volcano",
    "ambient_sauron",
    "ambient_morgoth",
    "ambient_dng6",
    "ambient_senya",
    "ambient_xakaze",
    "hit_hi_critical"
};

/*** Player information ***/

player_type player;
player_type *p_ptr = &player;
char title[NORMAL_WID];

/*
 * Structure (not array) of size limits
 */
maxima z_info_struct;

/*
 * Maximum number of flavors
 */
u16b flavor_max;

/*
 * Maximum number of vaults
 */
u16b v_max;

/*
 * Hack -- The special Angband "System Suffix"
 * This variable is used to choose an appropriate "pref-xxx" file
 */
const char *ANGBAND_SYS;

/*
 * Hack -- The special Angband "Graphics Suffix"
 * This variable is used to choose an appropriate "graf-xxx" file
 */
const char *ANGBAND_GRAF = "none";

/*
 * Various directories. These are no longer necessarily all subdirs of "lib"
 */
char *ANGBAND_DIR_XTRA;

/*
 * Various xtra/ subdirectories.
 */
char *ANGBAND_DIR_XTRA_FONT;
char *ANGBAND_DIR_XTRA_GRAF;
char *ANGBAND_DIR_XTRA_ICON;
char *ANGBAND_DIR_XTRA_SOUND;

/*
 * Sound hook (for playing FX).
 */
void (*sound_hook)(int);

/* Delay in centiseconds before moving to allow another keypress */
/* Zero means normal instant movement. */
byte lazymove_delay = 0;

/*** MAngband specifics ***/

/* Connection parameters */
char meta_address[NORMAL_WID];
char nick[NORMAL_WID];
char pass[NORMAL_WID];
char stored_pass[NORMAL_WID];
char real_name[NORMAL_WID];
char server_name[NORMAL_WID];
int server_port;

/* Inventory */
char **inventory_name;
byte inven_pack;
byte inven_wield;
byte inven_bow;
byte inven_left;
byte inven_light;
byte inven_total;
byte quiver_start;
byte all_inven_total;
byte max_stack_size;

/* Equipment */
char **eq_name;

/* The general info about the current store */
struct store current_store;

/* Store name for current store */
char store_name[NORMAL_WID];

/* The names of the stuff in the store */
char store_names[STORE_INVEN_MAX][NORMAL_WID];

/* Floor item */
object_type floor_item[MAX_FLOOR_STACK];
char floor_name[MAX_FLOOR_STACK][NORMAL_WID];
byte floor_num;

/*
 * Spell information and description
 */
char spell_info[BOOKS_PER_REALM][SPELLS_PER_BOOK][NORMAL_WID];
spell_flags spell_flag[BOOKS_PER_REALM][SPELLS_PER_BOOK];
char spell_desc[BOOKS_PER_REALM][SPELLS_PER_BOOK][MSG_LEN];

/* Party information */
char party_info[160];

/* The information given to us by the server */
server_setup_t Setup;

/* The information we give to the server */
client_setup_t Client_setup;

/* Are we in a store? */
bool shopping;

/* Are we looking at the full map? */
bool map_active;

/* Last line of info we've received */
s16b last_line_info = -1;

/* Maximum amount of "special" info */
s16b max_line;

/* Current displayed line of "special" info */
s16b cur_line;

/* Health bar parameters */
int health_amt;
byte health_attr;

/* Lag bar parameters */
u32b lag_mark;

/* Trap detection indicator */
byte trap_indicator;

/* Special info display */
int special_line_type;
char special_line_header[ANGBAND_TERM_MAX][NORMAL_WID];

/* Escape command */
bool first_escape = FALSE;

/* Window flags */
u32b window_flag[ANGBAND_TERM_MAX];

/* Roller */
s16b stat_roll[A_MAX + 1];

/* Top line is icky */
bool topline_icky;

/* Party mode */
bool party_mode;

/* Section is icky */
s16b section_icky_col;
byte section_icky_row;

/* Melee/missile to-hit/to-dam */
int dis_to_mhit;
int dis_to_mdam;
int dis_to_shit;
int dis_to_sdam;

/* Chat channels */
channel_type channels[MAX_CHANNELS];
s16b view_channel = 0;

/* Remote info display */
cave_view_type remote_info[ANGBAND_TERM_MAX][MAX_TXT_INFO][NORMAL_WID];
s16b last_remote_line[ANGBAND_TERM_MAX];

/* Character list */
u16b char_num;
char **char_name;
char *char_expiry;

/* Hack -- Icky screen */
bool full_icky_screen;
bool target_icky_screen;
