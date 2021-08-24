#ifndef DUN_CLASSES_H
#define DUN_CLASSES_H

#include <QColor>
#include <QChar>
#include "src/terrain.h"


enum
{
    FLOOR_LIGHT_DIM = 0,
    FLOOR_LIGHT_NORMAL,
    FLOOR_LIGHT_BRIGHT,
};

enum
{
    OBJ_SYMBOL_NONE = 0,
    OBJ_SYMBOL_SQUELCH,
    OBJ_SYMBOL_PILE,
};

#define UI_TRANSPARENT_MONSTER 0x0001
#define UI_TRANSPARENT_EFFECT  0x0002
#define UI_LIGHT_DIM           0x0004
#define UI_LIGHT_BRIGHT        0x0008
#define UI_LIGHT_TORCH         0x0010
#define UI_COSMIC_TORCH        0x0020


class dungeon_type
{
public:
    dungeon_type();

    //  the 4 layers of the dungeon, the last 3 are indexes to lists which store the information
    s16b feature_idx;
    s16b object_idx;
    u16b effect_idx;
    s16b monster_idx;

    u16b cave_info;
    byte special_lighting;
    bool dtrap;
    byte obj_special_symbol;

    //display information for the 4 layers
    QColor dun_color;
    QChar  dun_char;
    QColor object_color;
    QChar  object_char;
    QColor effect_color;
    QChar  effect_char;
    QColor monster_color;
    QChar  monster_char;

    // Tiles for the 4 layers
    QString dun_tile;
    QString object_tile;
    QString effect_tile;
    QString monster_tile;

    bool double_height_monster;

    u16b ui_flags;

    u16b path_cost;
    bool path_flow;

    void clear_path_flow();

    bool has_object();
    bool has_effect();
    bool has_monster();

    bool has_visible_terrain();
    bool has_visible_object();
    bool has_visible_effect();
    bool has_visible_monster();

    bool has_visible_artifact();

    void mark_square();

    bool is_wall(bool known);
    bool is_door(void);
    bool is_secret_door(void);
    bool is_known_door(void);
    bool is_closed_door(void);
    bool is_known_closed_door(void);
    bool is_jammed_door(void);
    bool is_wall(void);
    bool is_stairs(void);
    bool projectable();


    // All variables above should be included in this method.
    void dungeon_square_wipe();
};

class effect_type
{
public:
    effect_type();

    byte x_type;            /* Effect Type */

    u16b x_f_idx;           /* Effect Feature IDX */

    byte x_cur_y;			/* Current y location, or countdown_base */
    byte x_cur_x;			/* Current x location, or countdown_rand */

    byte x_countdown;       /* Number of turns effect has left */
    byte x_repeats;			/* Number of times the effect repeats*/

    u16b x_power;           /* Strength of effect */

    s16b x_source;          /* Source of effect - THIS MUST BE THE RACE of the monster, not the mon_idx of the creature. */

    u16b x_flags;           /* Effect "memory" bitflags */

    s16b next_x_idx;		/* Idx of next effect at this square. */

    s16b x_r_idx;           /* Some monster race index. Used for inscriptions */

    // All variables above should be included in this method.
    void effect_wipe();
};

/*
 * Feature state structure
 *
 *    - Action (FS_*)
 *   - Result (FEAT_*)
 */
class feature_state
{
public:

    byte fs_action;
    s16b fs_result;
    u16b fs_power;
};

/*
 * Information about terrain "features"
 */
class feature_type
{
public:
    feature_type();

    QString f_name;			/* Name (offset) */
    QString f_text;			/* Text (offset) */

    u16b f_mimic;	/* Feature to mimic */

    s16b f_edge;

    u32b f_flags1;
    u32b f_flags2;
    u32b f_flags3;

    u16b f_level;     	/* Minimum level */
    u16b f_rarity;    	/* 1/Rarity */

    u16b priority;  /* Map priority */
    s16b defaults;     /* Default state */

    feature_state state[MAX_FEAT_STATES];

    byte f_power;

    u16b unused;		/* Unused */

    byte color_num;     //The number of any default color.  CUSTOM_COLOR for all others.
    QColor d_color;		/* Default feature color */
    QChar d_char;		/* Default feature character */

    bool f_everseen;	/* Used to despoilify knowledge screens */

    /* Fields use donly by effects. */
    u16b x_damage;				/* damage per 100 levels for a smart trap - or basic damage for an effect cloud*/
    byte x_gf_type;			/* the force type of the effect - numbers are hard coded in source */
    byte x_timeout_set; 	/*base time between effect instances */
    byte x_timeout_rand; /*is the random time between effects */

    /* Fields used only by terrain. */
    u16b dam_non_native;		/*damage to non-native creatures existing in grid */
    byte native_energy_move;	/*energy to move through for native creatures */
    byte non_native_energy_move;	/*energy to move through for non-native creatures */
    byte native_to_hit_adj;	/*combat bonus for being native (percentage)  */
    byte non_native_to_hit_adj;	/*combat bonus for being native (percentage)*/
    s32b f_stealth_adj;			/*Adjustment to stealth depending on terrain*/

    QString tile_id;

    void feature_wipe();
    bool is_door(void);
    bool is_secret_door(void);
    bool is_known_door(void);
    bool is_closed_door(void);
    bool is_known_closed_door(void);
    bool is_jammed_door(void);
    bool is_wall(void);
    bool is_stairs(void);
};

/*
 * Feature "lore" information
 *
 * Note that these fields are related to the "feature recall".
 *
 */
class feature_lore
{
public:

    feature_lore();

    byte f_l_sights;		/*Number of times seeing this terrain*/

    u32b f_l_flags1;
    u32b f_l_flags2;
    u32b f_l_flags3;

    byte f_l_defaults;     /* Default state */

    byte f_l_state[MAX_FEAT_STATES];

    byte f_l_power;		/*Number of observed usages of power (unlock, trap power, etc)*/

    byte f_l_dam_non_native;		/*Number of observed damage to non-native creatures existing in grid*/
    byte f_l_native_moves;		/* Number of observed native moves for this terrain */
    byte f_l_non_native_moves;	/* Number of observed non-native moves for this terrain */
    byte f_l_native_to_hit_adj;	/*Number of observed  for being native (percentage)*/
    byte f_l_non_native_to_hit_adj;	/*Number of observed combat penalties for being non-native (percentage)*/
    byte f_l_stealth_adj;			/*Number of observed adjustments to stealth depending on terrain*/

    // All variables above should be included in this method.
    void feature_lore_wipe();

};

/*
 * Information about "vault generation"
 */
class vault_type
{
public:

    vault_type();
    void vault_wipe();

    QString vault_name;			/* Name (offset) */
    QString vault_text;			/* Text (offset) */

    byte typ;			/* Vault type */

    byte rat;			/* Vault rating */

    byte hgt;			/* Vault height */
    byte wid;			/* Vault width */
};

/*
 * The type definition of the entries of the "dyna_g" array
 */
class dynamic_grid_type
{
public:

    dynamic_grid_type();

    /* Coordinates */
    byte y;
    byte x;

    /* DF1_* flags */
    bool new_grid;

    /*
     * Timed features use a counter. Each time the features are
     * proccesed this counter is decremented. The effect is applied when
     * the counter becomes 0.
     */
    byte counter;

    // All variables above should be included in this method.
    void dynamic_grid_wipe();
};

extern void reset_dungeon_info();

#endif // DUN_CLASSES_H
