/* File: dun_classes.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "src/npp.h"

bool dungeon_type::has_visible_artifact()
{
    //if (!(cave_info & CAVE_SEEN)) return false;

    object_type *o_ptr;

    for (int o_idx = object_idx; o_idx > 0; o_idx = o_ptr->next_o_idx) {
        o_ptr = &o_list[o_idx];

        if (!o_ptr->marked || !o_ptr->is_artifact()) continue;

        if (o_ptr->is_known()) return true;

        if (o_ptr->discount == INSCRIP_INDESTRUCTIBLE ||
                o_ptr->discount == INSCRIP_TERRIBLE ||
                o_ptr->discount == INSCRIP_SPECIAL) return true;
    }

    return false;
}

void dungeon_type::mark_square()
{
    cave_info |= (CAVE_MARK | CAVE_EXPLORED);
}

void dungeon_type::clear_path_flow()
{
    path_cost = 0;
    path_flow = FALSE;
}

//Verify if the dungeon square has an object on it
bool dungeon_type::has_object()
{
    if (object_idx) return (TRUE);
    return (FALSE);
}

//Verify if the dungeon square has an effect on it
bool dungeon_type::has_effect()
{
    if (effect_idx) return (TRUE);
    return (FALSE);
}

//Verify if the dungeon square has a monster object on it
bool dungeon_type::has_monster()
{
    if (monster_idx) return (TRUE);
    return (FALSE);
}

bool dungeon_type::has_visible_object()
{
    if (object_char != ' ') return true;
    if (object_color != QColor("black")) return true;
    if (use_graphics && (object_tile.length() > 0)) return true;
    return false;
}

bool dungeon_type::has_visible_effect()
{
    if (effect_char != ' ') return true;
    if (effect_color != QColor("black")) return true;
    if (use_graphics && (effect_tile.length() > 0)) return true;
    return false;
}

bool dungeon_type::has_visible_monster()
{
    if (monster_char != ' ') return true;
    if (monster_color != QColor("black")) return true;
    if (use_graphics && (monster_tile.length() > 0)) return true;
    return false;
}

bool dungeon_type::is_wall(bool known)
{
    int this_feat = feature_idx;
    if (known) this_feat = f_info[feature_idx].f_mimic;
    return (f_info[this_feat].is_wall());
}

bool dungeon_type::is_door(void)
{
    return (f_info[feature_idx].is_door());
}

bool dungeon_type::is_secret_door(void)
{
    return (f_info[feature_idx].is_secret_door());
}

bool dungeon_type::is_known_door(void)
{
    return (f_info[feature_idx].is_known_door());
}

bool dungeon_type::is_known_closed_door(void)
{
    return (f_info[feature_idx].is_known_closed_door());
}

bool dungeon_type::is_closed_door(void)
{
    return (f_info[feature_idx].is_closed_door());
}

bool dungeon_type::is_jammed_door(void)
{
    return (f_info[feature_idx].is_jammed_door());
}

dungeon_type::dungeon_type()
{
    dungeon_square_wipe();
}

bool dungeon_type::is_stairs(void)
{
    return (f_info[feature_idx].is_stairs());
}

/*
 * Wipe the dungeon_type class.
 * This function shoudld be used instead of WIPE command.
 * All variables in dungeon_type should be re-set in this function.
 * This function does not clear the object, effect, and monster lists.
 * It should not be called without first removing any
 * monsters, effects, or objects from their respective lists.
 */
void dungeon_type::dungeon_square_wipe()
{
    feature_idx = effect_idx = monster_idx = object_idx = 0;
    path_cost = cave_info = 0;
    special_lighting = obj_special_symbol = 0;
    ui_flags = 0;
    path_flow = dtrap = FALSE;
    wall_right = wall_southeast = wall_below = double_height_monster = FALSE;
    dun_color = Qt::black;
    dun_char = ' ';
    object_color = Qt::black;
    object_char = ' ';
    effect_color = Qt::black;
    effect_char = ' ';
    monster_color = Qt::black;
    monster_char = ' ';

    monster_tile.clear();
    object_tile.clear();
    dun_tile.clear();
    effect_tile.clear();
}


effect_type::effect_type()
{
    effect_wipe();
}

/*
 * Wipe the effect_type class.
 * This function shoudld be used instead of WIPE command.
 * This function does not clear the effect pointer from dungeon_type.
 * It should be called directly only under unusual circumstances.
 */
void effect_type::effect_wipe()
{
    x_type = 0;
    x_f_idx = 0;
    x_cur_y = 0;
    x_cur_x = 0;
    x_countdown = 0;
    x_repeats = 0;
    x_power = 0;
    x_source = 0;
    x_flags = 0;
    next_x_idx = 0;
    x_r_idx = 0;
}





feature_type::feature_type()
{
    feature_wipe();
}

void feature_type::feature_wipe()
{
    f_name = QString("");
    f_text = QString("");

    f_mimic = f_edge = f_flags1 = f_flags2 = f_flags3 = 0;
    f_level = f_rarity = priority = defaults = f_power = unused = 0;

    for (int i = 0; i < MAX_FEAT_STATES; i++) {
        state[i].fs_action = state[i].fs_power = state[i].fs_result = 0;
    }

    color_num = 0;
    d_color = QColor("black");
    d_char = QChar(' ');

    f_everseen = false;

    x_damage = x_gf_type = x_timeout_rand = x_timeout_set = 0;

    dam_non_native = native_energy_move = non_native_energy_move = native_to_hit_adj =
            non_native_to_hit_adj = f_stealth_adj = 0;

    tile_id.clear();
}

bool feature_type::is_door(void)
{
    if (f_flags1 & (FF1_DOOR)) return (TRUE);
    return (FALSE);
}

bool feature_type::is_secret_door(void)
{
    if (!is_door()) return (FALSE);
    if (f_flags1 & (FF1_SECRET))  return (TRUE);
    return (FALSE);
}

bool feature_type::is_closed_door(void)
{
    if (!is_door()) return (FALSE);
    if (f_flags3 & (FF3_DOOR_CLOSED))  return (TRUE);
    return (FALSE);
}

bool feature_type::is_known_door(void)
{
    if (is_secret_door())   return (FALSE);
    return (TRUE);
}

bool feature_type::is_known_closed_door(void)
{
    if (!is_closed_door())  return (FALSE);
    if (is_secret_door())   return (FALSE);
    return (TRUE);
}

bool feature_type::is_jammed_door(void)
{
    if (!is_door()) return (FALSE);

    /* Jammed doors */
    if (f_flags3 & (FF3_DOOR_JAMMED)) return (TRUE);
    return (FALSE);
}

bool feature_type::is_wall(void)
{
    if (f_flags1 & (FF1_WALL)) return (TRUE);
    return (FALSE);
}

bool feature_type::is_stairs(void)
{
    if (f_flags1 & (FF1_STAIRS)) return (TRUE);
    return (FALSE);
}

vault_type::vault_type()
{
    vault_wipe();
}

void vault_type::vault_wipe()
{
    vault_name.clear();
    vault_text.clear();
    typ = rat = hgt = wid = 0;
}

feature_lore::feature_lore()
{
    feature_lore_wipe();
}


/*
 * Wipe the feature_lore class.
 * This function shoudld be used instead of WIPE command.
 * All variables in feature should be re-set in this function.
 */
void feature_lore::feature_lore_wipe()
{
    f_l_sights = 0;
    f_l_flags1 = f_l_flags2 = f_l_flags3 = 0;
    f_l_defaults = f_l_power = f_l_dam_non_native = f_l_native_moves = 0;
    f_l_non_native_moves = f_l_native_to_hit_adj = f_l_non_native_to_hit_adj = f_l_stealth_adj = 0;
    for (int i = 0; i < MAX_FEAT_STATES; i++)  f_l_state[i] = 0;
}


dynamic_grid_type::dynamic_grid_type()
{
    dynamic_grid_wipe();
}


/*
 * Wipe the feature_lore class.
 * This function shoudld be used instead of WIPE command.
 * All variables in feature should be re-set in this function.
 */
void dynamic_grid_type::dynamic_grid_wipe()
{
    y = x = counter = 0;
    new_grid = FALSE;
}

void reset_dungeon_info()
{
    for (int y = 0; y < MAX_DUNGEON_HGT; y++) {
        for (int x = 0; x < MAX_DUNGEON_WID; x++) {
            dungeon_info[y][x].dungeon_square_wipe();
        }
    }

    redraw_coords.clear();
}

