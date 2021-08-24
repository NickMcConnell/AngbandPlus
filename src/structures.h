#ifndef STRUCTURES_H
#define STRUCTURES_H

#include <src/defines.h>
#include "src/monster.h"
#include "src/terrain.h"
#include "src/player.h"
#include "src/object.h"
#include <QChar>
#include <QString>
#include <QColor>


typedef struct maxima maxima;
typedef struct colors_preset colors_preset;
typedef struct alloc_entry alloc_entry;
typedef struct alloc_entry_new alloc_entry_new;
typedef struct quest_type quest_type;
typedef struct owner_type owner_type;
typedef struct store_type store_type;
//typedef struct magic_type magic_type;
//typedef struct start_item start_item;
typedef struct names_type names_type;
typedef struct autoinscription autoinscription;
//typedef struct move_moment_type move_moment_type;
typedef struct coord coord;
typedef struct quiver_group_type quiver_group_type;
typedef struct option_entry option_entry;
typedef struct dungeon_capabilities_type dungeon_capabilities_type;
typedef struct slays_structure slays_structure;
typedef struct brands_structure brands_structure;
typedef struct mon_susceptibility_struct mon_susceptibility_struct;

enum
{
    INPUT_MODE_NONE = 0,
    INPUT_MODE_KEY,
    INPUT_MODE_MOUSE_SINGLE_CLICK,
    INPUT_MODE_MOUSE_DOUBLE_CLICK,
    INPUT_MODE_MOUSE_WHEEL,
};


class UserInput
{
public:
    int mode;
    int key;
    QString text;
    int x, y;
};




/*
 * Information about maximal indices of certain arrays
 * Actually, these are not the maxima, but the maxima plus one
 */
struct maxima
{

    u16b f_max;		/* Max size for "f_info[]" */
    u16b k_max;		/* Max size for "k_info[]" */
    u16b art_max;		/* Max size for "a_info[]" */
    u16b e_max;		/* Max size for "e_info[]" */
    u16b r_max;		/* Max size for "r_info[]" */
    u16b v_max;		/* Max size for "v_info[]" */
    u16b p_max;		/* Max size for "p_info[]" */
    u16b h_max;		/* Max size for "h_info[]" */
    u16b b_max;		/* Max size per element of "b_info[]" */
    u16b c_max;		/* Max size for "c_info[]" */
    u16b q_max;		/* Max size for "q_info[]" */
    u16b flavor_max; /* Max size for "flavor_info[]" */
    u16b o_max;		/* Max size for "o_list[]" */
    u16b m_max;		/* Max size for "mon_list[]" */
    u16b x_max;		/* Max size for "x_info[]" */
    u16b ghost_template_max;  /* number of maintainer maintainer + player ghost templates*/
    u16b ghost_player_max;
    u16b ghost_maint_max;
    u16b art_spec_max; /* Max number of special artifacts*/

    u16b art_norm_max; /* Max number for normal artifacts (special + normal)*/
    u16b art_rand_max; /*max number of random artifacts*/

    byte max_level;
    byte max_titles;
};



struct colors_preset
{
    QString color_name;
    int red;
    int green;
    int blue;
};








/*
 * An entry for the object/monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */
struct alloc_entry
{
    s16b index;		/* The actual index */

    byte level;		/* Base dungeon level */
    byte prob1;		/* Probability, pass 1 */
    byte prob2;		/* Probability, pass 2 */
    byte prob3;		/* Probability, pass 3 */

    u16b total;		/* Unused for now */
};

/*
 * An entry for the object/monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */
struct alloc_entry_new
{
    s16b index;		/* The actual index */

    byte level;		/* Base dungeon level */
    byte base_probability;		/* Probability, pass 1 */
    byte hook_probability;      // Probability after the functional hook
    byte final_probability;     // Probability after the level (or other) filter
};



/*
 * Structure for the "quests"
 */
struct quest_type
{
    QString name;			/* Name */
    byte q_type;		/* Quest Type */
    u16b q_reward;		/* Quest Reward */
    u16b q_fame_inc;	/* Amount fame will be increased when the quest is finished */
    byte q_theme;		/* Monster Theme for themed levels and nests/pits*/

    byte base_level;	/* The dungeon level on which the quest is assigned*/

    s16b mon_idx;		/* Monster race/unique */
    s32b turn_counter;	/* Mark when the quest began */

    s16b q_num_killed;	/* Number killed */
    s16b q_max_num;		/* Number required */

    byte q_flags;		/* Various quest flags */
};








/*structure of letter probabilitiesfor the random name generator*/
struct names_type
{
    u16b lprobs[S_WORD+1][S_WORD+1][S_WORD+1];
    u16b ltotal[S_WORD+1][S_WORD+1];
};



/*
 * Simple structure to hold a map location
 */
struct coord
{
    byte y;
    byte x;
};




/*
 * Info used to manage quiver groups
 */
struct quiver_group_type
{
    QChar cmd;		/* The command used to perform an action with the objects in the group */
    byte color;		/* The color of the pseudo-tag used for the group */
};

/*
 * The descriptions and default values of the in-game options
 */
struct option_entry
{
    QString name;
    QString description;
    bool normal;
};

/*
 * Set of custom predicates that modify the behavior of the game,
 * specially dungeon generation. The predicates are assigned in generate.c
 */
struct dungeon_capabilities_type
{
    /*
     * Check if a monster of the given race can have escorts
     * Used only in alloc_monster
     */
    bool (*can_place_escorts)(s16b r_idx);

    /*
     * Check if the player must be placed over grids that use CAVE_ROOM
     */
    bool (*can_place_player_in_rooms)(void);

    /*
     * Check if stairs can be placed on the given location
     */
    bool (*can_place_stairs)(int y, int x);

    /*
     * Adjust the number of stairs in a level
     */
    int (*adjust_stairs_number)(int initial_amount);


    /*
     * Check if fog must be placed on rooms
     */
    bool (*can_place_fog_in_rooms)(void);

    /*
     * Check if the look command can stop in the given feature
     */
    bool (*can_target_feature)(int f_idx);

    /*
     * Check if regions and walls of the current dungeon can be transformed
     */
    bool (*can_be_transformed)(void);

    /*
     * Check if a non-native monsters can be placed in an elemental grid
     * Used in get_mon_num
     */
    bool (*can_place_non_native_monsters)(void);

    /*
     * Check if monsters get re-populated while the player is on the level
     */
    bool (*allow_level_repopulation)(void);

    /*
     * Check if summoning is limited to creatures on the level
     */
    bool (*limited_level_summoning)(void);

    /*
     * Check if breeders are allowed to spread on the level
     */
    bool (*allow_monster_multiply)(void);

    /*
     * Check if earthquakes and destruction are allowed
     */
    bool (*prevent_destruction)(void);

    /*
     * Get the initial number of monsters in the level
     */
    int (*get_monster_count)(void);

    /*
     * Get the initial number of objects in the level (rooms only)
     */
    int (*get_object_count)(void);

    /*
     * Get the initial number of gold objects in the level (rooms and corridors)
     */
    int (*get_gold_count)(void);

    /*
     * Get the initial number of extra objects in the level (rooms and corridors)
     */
    int (*get_extra_object_count)(void);
};



/* Currently assumes all flags are in TR1 (object) and RF1 (monster flags) */
struct slays_structure
{
  u32b slay_flag;  /* Assumes in object flag TR1_ */
  byte multiplier;
  u32b mon_flag; /* Assumes in monster flag RF3 */
  QString slay_race;
};

/* Currently assumes all flags are in TR1 (object) and RF1 (monster flags) */
struct brands_structure
{
  u32b brand_flag;  /* Assumes in object flag TR1_ */
  byte multiplier;
  u32b mon_flag; /* Assumes in monster flag RF3 */
  u32b element;
  byte divisor;
  QString brand_resist;
};

/* Currently assumes all flags are in TR1 (object) and RF1 (monster flags) */
struct mon_susceptibility_struct
{
  u32b brand_flag;  /* Assumes in object flag TR1_ */
  u32b mon_flag; /* Assumes in monster flag RF3 */
  QString brand_susceptibility;
};


#endif // STRUCTURES_H
