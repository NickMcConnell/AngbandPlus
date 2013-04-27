// File: variable.cpp

// Purpose: Utumno variables

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"


// Hack -- Link a copyright message into the executable
char *copyright = {
    "Copyright (c) 1989 James E. Wilson, Robert A. Koeneke\n\n"
    "This software may be copied and distributed for educational, research,\n"
    "and not for profit purposes provided that this copyright and statement\n"
    "are included in all such copies."
};


// Hack -- Savefile version
byte sf_major;                  // Savefile's "version_major"
byte sf_minor;                  // Savefile's "version_minor"
byte sf_patch;                  // Savefile's "version_patch"
byte sf_extra;                  // Savefile's "version_extra"


// Hack -- Savefile information
bool character_generated;       // The character exists
bool character_dungeon;         // The character has a dungeon
bool character_loaded;          // The character was loaded from a savefile
bool character_saved;           // The character was just saved to a savefile

bool character_icky;            // The game is in an icky full screen mode
bool character_xtra;            // The game is in an icky startup mode

u32b seed_flavor;               // Hack -- consistent object colors
u32b seed_town;                 // Hack -- consistent town layout

bool create_up_stair;           // Auto-create "up stairs"
bool create_down_stair;         // Auto-create "down stairs"

bool force_enter_store;         // Enter a store

bool alive;                     // True if game is running
bool death;                     // True if player has died

s16b cur_hgt;                   // Current dungeon height
s16b cur_wid;                   // Current dungeon width
s16b dun_level;                 // Current dungeon level
s16b num_repro;                 // Current reproducer count

s32b game_turn;                 // Current game turn

bool wizard;                    // Is the player currently in Wizard mode?

u16b total_winner;              // Semi-Hack -- Game has been won

u16b panic_save;                // Track some special "conditions"

s16b signal_count = 0;          // Hack -- Count interupts

s16b coin_type;                 // Hack -- force coin type
bool opening_chest;             // Hack -- prevent chest generation

s16b inven_cnt;                 // Number of items in inventory
s16b equip_cnt;                 // Number of items in equipment

CItem *inventory;               // The inventory


// Software options (set via the '=' command).  See "tables.c"

// General options
bool other_query_flag;          // Prompt before various actions
bool carry_query_flag;          // Prompt when picking up things

bool show_equip_label;          // Shop labels in equipment list
bool notice_seams;              // Highlight mineral seams

bool ring_bell;                 // Ring the bell


// Option Set 2 -- Disturbance
bool alert_hitpoint;            // Alert user to critical hitpoints
bool alert_failure;             // Alert user to various failures


// Gameplay options
bool show_inven_weight;         // Show weights in inven
bool show_equip_weight;         // Show weights in equip
bool show_store_weight;         // Show weights in store

bool stack_allow_items;         // Allow weapons and armor and such to stack
bool stack_allow_wands;         // Allow wands and staffs and rods to stack
bool stack_force_notes;         // Force items with different notes to stack
bool stack_force_costs;         // Force items with different costs to stack


byte game_speed;                // Game speed (1 to 100)
byte hitpoint_warn;             // Hitpoint warning (0 to 9)


bool new_level_flag;            // Start a new level


// Current player's character name
char player_name[32];

// What killed the player
char died_from[80];

// Player index
int char_idx;


// Array of grids lit by player lite (see "cave.c") [LITE_MAX]
s16b lite_n;
CCoord lite[LITE_MAX];

// Array of grids viewable to the player (see "cave.c") [VIEW_MAX]
s16b view_n;
CCoord view[VIEW_MAX];

// Array of grids for use by various functions (see "cave.c") [TEMP_MAX]
s16b temp_n;
byte temp_x[TEMP_MAX];
byte temp_y[TEMP_MAX];


// The array of "cave grids" [MAX_WID][MAX_HGT].
// Not completely allocated, that would be inefficient
// Not completely hardcoded, that would overflow memory
CGrid *cave[MAX_HGT];

// Hack -- Quest array
quest q_list[MAX_Q_IDX];


// The stores [MAX_STORES]
store_type *store;


// The size of the "kind allocator table"
s16b alloc_kind_size;

// The indexes into the "kind allocator table" by level [MAX_DEPTH]
s16b *alloc_kind_index;

// The "kind allocator table" [alloc_kind_size]
kind_entry *alloc_kind_table;


// The size of the "race allocator table"
s16b alloc_race_size;

// The indexes into the "race allocator table" by level [MAX_DEPTH]
s16b *alloc_race_index;

// The "race allocator table" [alloc_race_size]
race_entry *alloc_race_table;


/*** Player information ***/

// Pointer to the player info
CPlayer *p_ptr;

// Pointer to the player tables (race, class, magic)
player_race *rp_ptr;
player_class *cp_ptr;
player_magic *mp_ptr;


// More spell info
bool spell_learned[64];   // spells learned
bool spell_worked[64];    // spells tried and worked
bool spell_forgotten[64]; // spells learned but forgotten
byte spell_order[64];     // order spells learned/remembered/forgotten


// The vault generation arrays
header *v_head;
vault_type *v_info;
char *v_name;
char *v_text;

// The terrain feature arrays
header *f_head;
feature_type *f_info;
char *f_name;

// The object kind arrays
CObjectKind *k_info;

// The artifact arrays
header *a_head;
artifact_type *a_info;
char *a_name;

// The ego-item arrays
header *e_head;
ego_item_type *e_info;
char *e_name;
char *e_text;


// The monster race arrays
header *r_head;
CMonsterRace *r_info;
char *r_name;
char *r_text;


// Total Hack -- allow all items to be listed (even empty ones)
// This is only used by do_cmd_inven() and such and is cleared there.
bool item_tester_full;


// Here is a "pseudo-hook" used during calls to "get_item()" and
// "show_inven()" and "show_equip()", and the choice window routines.
byte item_tester_tval;


// The pointer to the last get_item item
CItem *gi_i_ptr;


// Here is a "hook" used during calls to "get_item()" and
// "show_inven()" and "show_equip()", and the choice window routines.
bool (*item_tester_hook)(CItem *);



// Current "comp" function for ang_sort()
bool (*ang_sort_comp)(vptr u, vptr v, int a, int b);


// Current "swap" function for ang_sort()
void (*ang_sort_swap)(vptr u, vptr v, int a, int b);



// Hack -- function hook to check "validity" of given race
bool (*get_mon_num_hook)(int r_idx);



// Hack -- function hook to check "validity" of given kind
bool (*get_obj_num_hook)(int k_idx);


// Other
bool show_minimap;
int current_spell, current_spell_type;
int show_stuff, show_param;
u32b frames, fps_ticker;
char cur_console_line[128];
CArrow *arrows[MAX_PROJECTILES];
bool resting;