/*
 * File: s-variable.c
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

#include "s-angband.h"
#include "../common/spells.h"

/*
 * Number of connected players
 */
int NumPlayers;

/*
 * Various things
 */
bool server_generated;      /* The server exists */
bool server_state_loaded;   /* The server state was loaded from a savefile */
u32b seed_flavor;           /* Hack -- consistent object colors */
u32b seed_town;             /* Hack -- consistent town layout */
hturn turn;                 /* Current game turn */
s16b o_max = 1;             /* Number of allocated objects */
s16b o_top = 0;             /* Number of live objects */
s16b o_nxt = 1;             /* Next object */

/*
 * Array[MAX_STORES] of stores
 */
struct store *stores;

/*
 * The size of "alloc_ego_table"
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
 * Specify attr/char pairs for visual special effects for project()
 */
byte gf_to_attr[GF_MAX][BOLT_MAX];
char gf_to_char[GF_MAX][BOLT_MAX];

/*
 * Specify color for inventory item text display (by tval)
 * Be sure to use "index & 0x7F" to avoid illegal access
 */
byte tval_to_attr[128];

/*** Player information ***/

/* Current player ID */
s32b player_id;

/*
 * The info arrays
 */
artifact_type *a_info;
feature_type *f_info;
object_base *kb_info;
ego_item_type *e_info;
monster_base *rb_info;
monster_pain *pain_messages;
struct vault *vaults;
struct flavor *flavors;
spell_type *s_info;
struct pit_profile *pit_info;

/*
 * Various directories. These are no longer necessarily all subdirs of "lib"
 */
char *ANGBAND_DIR_APEX;
char *ANGBAND_DIR_HELP;
char *ANGBAND_DIR_SAVE;

/*
 * Hack -- function hook to restrict "get_mon_num_prep()" function
 */
bool (*get_mon_num_hook)(int r_idx);

/*** MAngband specifics ***/

/* Number of trees in town */
byte trees_in_town;

/* Where do players start if coming up? */
byte level_up_y[MAX_DEPTH];
byte level_up_x[MAX_DEPTH];

/* Where do players start if going down? */
byte level_down_y[MAX_DEPTH];
byte level_down_x[MAX_DEPTH];

/* Where do players start if they tele level? */
byte level_rand_y[MAX_DEPTH];
byte level_rand_x[MAX_DEPTH];

/* How many players are at each depth */
s16b players_on_world[MAX_DEPTH + MAX_WILD];
s16b *players_on_depth = &(players_on_world[MAX_WILD]);

/* List of depths which are special static levels */
s16b special_levels[MAX_SPECIAL_LEVELS];

/* Player images for graphic mode */
cave_view_type player_presets[MAX_PRESETS][MAX_CLASS][MAX_RACES][MAX_SEXES];
cave_view_type player_numbers[MAX_PRESETS][7];

/*
 * Server options, set in mangband.cfg
 */
bool cfg_report_to_meta = FALSE;
char *cfg_meta_address = NULL;
char *cfg_bind_name = NULL;
char *cfg_report_address = NULL;
char *cfg_console_password = NULL;
char *cfg_dungeon_master = NULL;
bool cfg_secret_dungeon_master = TRUE;
bool cfg_no_steal = TRUE;
bool cfg_newbies_cannot_drop = TRUE;
s32b cfg_level_unstatic_chance = 60;
bool cfg_random_artifacts = FALSE;
s32b cfg_retire_timer = -1;
s16b cfg_limit_stairs = 0;
bool cfg_no_recall = FALSE;
bool cfg_no_ghost = FALSE;
bool cfg_more_towns = FALSE;
bool cfg_artifact_drop_shallow = TRUE;
bool cfg_limit_player_connections = TRUE;
s16b cfg_max_townies = -1;
s16b cfg_max_trees = -1;
s32b cfg_tcp_port = 18346;
bool cfg_chardump_color = FALSE;
bool cfg_town_wall = FALSE;
s16b cfg_pvp_hostility = PVP_SAFE;
bool cfg_base_monsters = TRUE;
bool cfg_extra_monsters = FALSE;
s16b cfg_max_houses = 0;
bool cfg_ghost_diving = FALSE;
bool cfg_console_local_only = FALSE;
char *cfg_load_pref_file = NULL;
s16b cfg_preserve_artifacts = 3;
bool cfg_safe_recharge = FALSE;
s16b cfg_party_sharelevel = -1;
bool cfg_small_range = FALSE;
bool cfg_ai_packs = TRUE;
bool cfg_ai_smart = FALSE;
bool cfg_ai_smell = TRUE;
bool cfg_ai_learn = FALSE;
bool cfg_ai_cheat = FALSE;

/* The party information */
party_type parties[MAX_PARTIES];

/* Chat channels */
channel_type channels[MAX_CHANNELS];
int chan_audit;
int chan_debug;
int chan_cheat;

/* The information about arenas */
arena_type arenas[MAX_ARENAS];
u16b num_arenas = 0;

/* The information about houses */
house_type houses[MAX_HOUSES];
u16b num_houses = 0;

/*
 * The array of indexes of "live" monsters
 */
s16b *m_fast;

/*
 * The information about wilderness levels
 */
wilderness_type world_info[MAX_WILD + 1];
wilderness_type *wild_info = &(world_info[MAX_WILD]);

/* Extra terrain feature arrays */
char (*f_char_s)[FEAT_LIGHTING_MAX];
byte (*f_attr_s)[FEAT_LIGHTING_MAX];

/* Extra monster race arrays */
char *r_char_s;
byte *r_attr_s;

/*
 * The dungeon master movement hook, called whenever he moves
 * (to make building large buildings / summoning hoards of monsters easier)
 */
void (*master_move_hook)(int Ind, char *args) = NULL;
