/* File: player_classes.cpp */

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
#include "src/player_command.h"
#include "src/utilities.h"
#include <src/messages.h>

player_race::player_race()
{
    player_race_wipe();
}

void player_race::player_race_wipe()
{
    pr_name.clear();
    pr_text.clear();
    C_WIPE(r_adj, A_MAX, s16b);
    r_dis = r_dev = r_sav = r_stl = r_srh = r_fos = r_thn = r_thb = 0;
    r_mhp = r_exp = b_age = m_age = 0;
    m_b_ht = m_m_ht = m_b_wt = m_m_wt = 0;
    f_b_ht = f_m_ht = f_b_wt = f_m_wt = 0;
    infra = choice = 0;
    hist = 0;
    pr_flags1 = pr_flags2 = pr_flags3 = pr_native = 0;

}

player_class::player_class()
{
    player_class_wipe();
}

void player_class::player_class_wipe()
{
    cl_name.clear();
    for (int i = 0; i < PY_MAX_LEVEL; i++)
    {
        cl_title[i].clear();
    }
    C_WIPE(c_adj, A_MAX, s16b);
    c_dis = c_dev = c_sav = c_stl = c_srh = c_fos = c_thn = c_thb = 0;
    x_dis = x_dev = x_sav = x_stl = x_srh = x_fos = x_thn = x_thb = 0;
    c_mhp = c_exp = flags = c_native = max_attacks = min_weight = att_multiply = 0;
    spell_book = spell_first = spell_weight = sense_base = sense_div = 0;
    for (int i = 0; i < MAX_START_ITEMS; i++)
    {
        start_items[i].max = start_items[i].min = start_items[i].sval = start_items[i].tval = 0;
    }
    for (int i = 0; i < PY_MAX_SPELLS; i++)
    {
        magic_type *m = spells.info + i;
        m->sexp = m->sfail = m->slevel = m->smana = 0;
    }
}

bool player_class::pseudo_id_heavy()
{
    if (flags & (CF_PSEUDO_ID_HEAVY)) return (TRUE);
    return (FALSE);
}



/*
 * Wipe the player state class.
 * This function shoudld be used instead of WIPE command.
 * All variables in player state should be re-set in this function.
 */
void player_state::player_state_wipe()
{
    int i;
    p_speed = 0;
    num_blow = num_fire = 0;
    ammo_mult = ammo_tval = 0;
    for (i = 0; i < A_MAX; i++)
    {
        stat_equip[i] = 0;
        stat_index[i] = 0;
        stat_loaded_cur[i] = 0;
        stat_loaded_max[i] = 0;
    }
    known_ac = ac = known_to_a = to_a = to_h = known_to_h = to_d = known_to_d = 0;
    see_infra = 0;
    for (i = 0; i < SKILL_MAX; i++) skills[i] = 0;
    noise = cur_light = 0;
    p_flags_native_with_temp = p_flags_native_no_temp = 0;
    sustain_str = sustain_int = sustain_wis = sustain_dex = sustain_con = sustain_chr = FALSE;
    immune_acid = immune_elec = immune_fire = immune_cold = immune_pois = FALSE;
    resist_acid = resist_elec = resist_fire = resist_cold = resist_pois = FALSE;
    resist_fear = resist_light = resist_dark = resist_blind = resist_confu = resist_sound = FALSE;
    resist_shard = resist_nexus = resist_nethr = resist_chaos = resist_disen = FALSE;
    native_lava = native_ice = native_oil = native_fire = native_sand = native_boiling_water = FALSE;
    native_forest = native_water = native_acid = native_mud = native_boiling_mud = FALSE;
    slow_digest = ffall = regenerate = telepathy = see_inv = see_inv_perm = free_act = hold_life = afraid = light = FALSE;
    impact = aggravate = teleport = exp_drain = bless_blade = cursed_quiver = FALSE;
    cumber_armor = cumber_glove = heavy_wield = heavy_shoot = icky_wield = FALSE;
}



/*
 * Wipe the player other class.
 * This function shoudld be used instead of WIPE command.
 * All variables in player other should be re-set in this function.
 */
void player_other::player_other_wipe()
{            
    full_name.clear();
    for (int i = 0; i < OPT_MAX; i++) op_ptr->opt[i] = options[i].normal;
    hitpoint_warn = delay_factor = 0;
}

void player_type::player_command_wipe()
{
    command_current = CMD_NONE;
    player_args.wipe();
    p_ptr->running_withpathfind = FALSE;
}

void player_type::player_previous_command_wipe()
{
    command_previous = CMD_NONE;
    command_previous_args.wipe();
}

void player_type::player_previous_command_update(s16b command, cmd_arg args)
{
    player_previous_command_wipe();
    command_previous = command;
    command_previous_args = args;
}


bool player_type::is_running()
{
    if (p_ptr->command_current == CMD_RUNNING) return (TRUE);
    return (FALSE);
}

bool player_type::is_resting()
{
    if (p_ptr->command_current == CMD_RESTING) return (TRUE);
    return (FALSE);
}

/* Determine if the player should stop resting
 */
bool player_type::should_stop_resting()
{
    if (!is_resting()) return(FALSE);

    if (player_args.choice == REST_BOTH_SP_HP)
    {
        if (chp < mhp) return (FALSE);
        if (csp < msp) return (FALSE);
        return (TRUE);
    }

    if (player_args.choice == REST_HP)
    {
        if (chp < mhp) return (FALSE);
        return (TRUE);
    }
    if (player_args.choice == REST_SP)
    {
        if (csp < msp) return (FALSE);
        return (TRUE);
    }

    if (player_args.choice == REST_COMPLETE)
    {
        if (chp < mhp) return (FALSE);
        if (csp < msp) return (FALSE);
        if (timed[TMD_BLIND])  return (FALSE);
        if (timed[TMD_CONFUSED])  return (FALSE);
        if (timed[TMD_AFRAID])  return (FALSE);
        if (timed[TMD_STUN])  return (FALSE);
        if (timed[TMD_SLOW])  return (FALSE);
        if (timed[TMD_PARALYZED])  return (FALSE);
        if (timed[TMD_IMAGE])  return (FALSE);
        if (word_recall) return (FALSE);
        if (food > PY_FOOD_UPPER)  return (TRUE);

        return (TRUE);
    }

    if (player_args.choice == REST_TURNCOUNT)
    {
        if (player_args.repeats) return (FALSE);
        return (TRUE);
    }

    // Oops!  Code shouldn't get this far.
    return TRUE;
}

/* Start appending messages
 */
void player_type::message_append_start()
{
    message_append = TRUE;
}

/* Stop appending messages
 */
void player_type::message_append_stop()
{
    message_append = FALSE;
    if (!message_list.empty()) message_list[0].append = FALSE;

    stop_message_window_append();
}

u16b player_type::cut_status()
{
    if (!timed[TMD_CUT])                    return (CUT_NONE);
    if (timed[TMD_CUT] > CUT_MORTAL_WOUND)  return (CUT_MORTAL_WOUND);
    if (timed[TMD_CUT] > CUT_DEEP_GASH)     return (CUT_DEEP_GASH);
    if (timed[TMD_CUT] > CUT_SEVERE)        return (CUT_SEVERE);
    if (timed[TMD_CUT] > CUT_NASTY)         return (CUT_NASTY);
    if (timed[TMD_CUT] > CUT_BAD)           return (CUT_BAD);
    if (timed[TMD_CUT] > CUT_LIGHT)        return (CUT_LIGHT);
    return (CUT_GRAZE);
}

u16b player_type::stun_status()
{
    if (!timed[TMD_STUN])                    return (STUN_NONE);
    if (timed[TMD_STUN] >= STUN_KNOCKED_OUT)  return (STUN_KNOCKED_OUT);
    if (timed[TMD_STUN] > STUN_HEAVY)     return (STUN_HEAVY);
    return (STUN_LIGHT);
}

void player_type::player_type_wipe()
{
    int i;
    py = px = 0;
    flow_center_y = flow_center_x = 0;
    update_center_y = update_center_x = 0;
    psex = prace = pclass = 0;
    hitdie = expfact = 0;
    age = ht = wt = sc = au = 0;
    q_fame = deferred_rewards = quest_depth = max_depth = depth = recall_depth = 0;
    max_lev = lev = max_exp = exp = exp_frac = 0;
    mhp = chp = chp_frac = msp = csp = csp_frac = 0;
    for (i = 0; i < A_MAX; i++)
    {
        stat_base_max[i] = 0;
        stat_base_cur[i] = 0;
        stat_quest_add[i] = 0;
    }

    for (i = 0; i < TMD_MAX; i++) timed[i] = 0;
    word_recall = p_energy = food = 0;
    confusing = searching = 0;
    base_wakeup_chance = 0;
    for (i = 0; i < PY_MAX_SPELLS; i++)     spell_flags[i] = spell_order[i] = 0;
    for (i = 0; i < PY_MAX_LEVEL; i++)      player_hp[i] = 0;
    died_from.clear();
    history.clear();
    total_winner = panic_save = 0;
    is_dead = terminated = player_turn = is_wizard = FALSE;
    playing =  in_store = message_append = leaving_level = autosave = FALSE;
    create_stair = cur_map_hgt = cur_map_wid = FALSE;
    total_weight = 0;
    inven_cnt = equip_cnt = pack_size_reduce = quiver_remainder = quiver_slots = 0;
    target_set = target_who = target_row = target_col = health_who = monster_race_idx = 0;
    object_idx = object_kind_idx = feature_kind_idx = 0;
    running_withpathfind = FALSE;
    run_cur_dir = run_old_dir = 0;
    run_unused = run_open_area = run_break_right = run_break_left = FALSE;
    player_command_wipe();
    player_previous_command_wipe();
    new_spells = 0;
    notice = update = redraw = window = 0;
    au_birth = 0;
    for (i = 0; i < A_MAX; i++) stat_birth[i] = 0;
    ht_birth = wt_birth = sc_birth = 0;
    current_hotkey = 0;

    state.player_state_wipe();

    vulnerability = next_quest = cumulative_terrain_damage = 0;
    game_turn = p_turn = 0;
    dungeon_type = 0;

    tile_id.clear();
}

bool player_type::has_learned_spells(void)
{
    // Hasn't learned any spells.
    if (p_ptr->spell_order[0] == 99)
    {
        QString noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, DIR_UNKNOWN);
        message(QString("You have not learned any %1s.") .arg(noun));
        return (FALSE);
    }

    return (TRUE);
}

/*
 * Is the player capable of casting a spell?
 */
bool player_type::can_cast(void)
{
    if (!cp_ptr->spell_book)
    {
        message(QString("You cannot cast spells!"));
        return (FALSE);
    }

    if (p_ptr->timed[TMD_BLIND] || no_light())
    {
        message(QString("You cannot see!"));
        return (FALSE);
    }

    if (p_ptr->timed[TMD_CONFUSED])
    {
        message(QString("You are too confused!"));
        return (FALSE);
    }

    return (TRUE);
}

/*
 * Is the player capable of studying?
 */
bool player_type::can_study(void)
{
    if (!can_cast()) return (FALSE);

    if (!new_spells)
    {
        QString p = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
        message(QString("You cannot learn any new %1s!") .arg(p));
        return (FALSE);
    }

    return (TRUE);
}

/*
 * Is the player capable of studying?
 */
bool player_type::chooses_spells(void)
{
    if (!cp_ptr->spell_book) return (FALSE);

    if (cp_ptr->flags & (CF_CHOOSE_SPELLS)) return TRUE;

    return (FALSE);
}

// Calculate the maximum possible score for each player ability.
// Called once after the game is initialized
void player_attribute_maximums::calculate_maximums()
{
    s16b current_calcs[SKILL_MAX];
    int i;

    for (i = 0; i < SKILL_MAX; i++) max_skills[i] = 1;

    for(int r = 0; r < z_info->p_max; r++)
    {

        for(int c = 0; c < z_info->c_max; c++)
        {
            for (i = 0; i < SKILL_MAX; i++) current_calcs[i] = 0;

            // All these calcs need to be kept consistent with calcs.cpp
            current_calcs[SKILL_DISARM] = p_info[r].r_dis + c_info[c].c_dis;
            current_calcs[SKILL_DEVICE] = p_info[r].r_dev + c_info[c].c_dev;
            current_calcs[SKILL_SAVE] = p_info[r].r_sav + c_info[c].c_sav;
            current_calcs[SKILL_STEALTH] = p_info[r].r_stl + c_info[c].c_stl + 1;
            current_calcs[SKILL_SEARCH_CHANCE] = p_info[r].r_srh + c_info[c].c_srh;
            current_calcs[SKILL_SEARCH_FREQUENCY] = p_info[r].r_fos + c_info[c].c_fos;
            current_calcs[SKILL_TO_HIT_MELEE] = p_info[r].r_thn + c_info[c].c_thn;
            current_calcs[SKILL_TO_HIT_BOW] = p_info[r].r_thb + c_info[c].c_thb;
            current_calcs[SKILL_TO_HIT_THROW] = p_info[r].r_thb + c_info[c].c_thb;
            current_calcs[SKILL_DIGGING] = 0;

            if (game_mode == GAME_NPPMORIA)
            {

                current_calcs[SKILL_DEVICE] += (moria_class_level_adj[c][MORIA_CLA_DEVICE] * z_info->max_level / 3);
                current_calcs[SKILL_DISARM] += (moria_class_level_adj[c][MORIA_CLA_DISARM] * z_info->max_level / 3);
                current_calcs[SKILL_TO_HIT_MELEE] += (moria_class_level_adj[c][MORIA_CLA_BTH] * z_info->max_level);
                current_calcs[SKILL_SAVE] += (moria_class_level_adj[c][MORIA_CLA_SAVE] * z_info->max_level / 3);
                current_calcs[SKILL_TO_HIT_BOW] += (moria_class_level_adj[c][MORIA_CLA_BTHB] * z_info->max_level);
                current_calcs[SKILL_TO_HIT_THROW] += (moria_class_level_adj[c][MORIA_CLA_BTHB] * z_info->max_level);
            }
            else // GAME_NPPANGBAND
            {
                // assume max
                current_calcs[SKILL_DISARM] += adj_dex_dis[STAT_TABLE_MAX_VALUE];
                current_calcs[SKILL_DISARM] += adj_int_dis[STAT_TABLE_MAX_VALUE];

                /* Affect Skill -- magic devices (INT) */
                current_calcs[SKILL_DEVICE] += adj_int_dev[STAT_TABLE_MAX_VALUE];

                /* Affect Skill -- saving throw (WIS) */
                current_calcs[SKILL_SAVE] += adj_wis_sav[STAT_TABLE_MAX_VALUE];
            }

            current_calcs[SKILL_DIGGING] += adj_str_dig[STAT_TABLE_MAX_VALUE];
            current_calcs[SKILL_DISARM] += (c_info[c].x_dis * z_info->max_level / 10);
            current_calcs[SKILL_DEVICE] += (c_info[c].x_dev * z_info->max_level / 10);
            current_calcs[SKILL_SAVE] += (c_info[c].x_sav * z_info->max_level / 10);
            current_calcs[SKILL_SEARCH_CHANCE] += (c_info[c].x_srh * z_info->max_level / 10);
            current_calcs[SKILL_SEARCH_FREQUENCY] += (c_info[c].x_fos * z_info->max_level / 10);
            current_calcs[SKILL_TO_HIT_MELEE] += (c_info[c].x_thn * z_info->max_level / 10);
            current_calcs[SKILL_TO_HIT_BOW] += (c_info[c].x_thb * z_info->max_level / 10);
            current_calcs[SKILL_TO_HIT_THROW] += (c_info[c].x_thb * z_info->max_level / 10);
            current_calcs[SKILL_STEALTH] += (c_info[c].c_stl * z_info->max_level / 10);

            if (c_info[c].flags & (CF_ROGUE_COMBAT)) current_calcs[SKILL_TO_HIT_THROW] += 20 +  z_info->max_level / 3;
            if (c_info[c].flags & (CF_BRIGAND_COMBAT)) current_calcs[SKILL_TO_HIT_THROW] += 20 +  z_info->max_level / 3;

            // Now see if we have a new maximum;
            for (i = 0; i < SKILL_MAX; i++)
            {
                if (max_skills[i] < current_calcs[i]) max_skills[i] = current_calcs[i];
            }
        }
    }

    // Now work on stealth calculations;
    if (max_skills[SKILL_STEALTH] > 30) max_skills[SKILL_STEALTH] = 30;

    max_wakeup_chance = WAKEUP_MAX;

    for (i = 0; i < max_skills[SKILL_STEALTH]; i++)
    {
        max_wakeup_chance = 4 * max_wakeup_chance / 5;

        /* Always make at least some innate noise */
        if (max_wakeup_chance < WAKEUP_MIN)
        {
            max_wakeup_chance = WAKEUP_MIN;
            break;
        }
    }

    // Saving throw tops out at 100%
    if (max_skills[SKILL_SAVE] > 100) max_skills[SKILL_SAVE] = 100;

    if (game_mode == GAME_NPPMORIA) max_p_speed = calc_energy_gain(NPPMORIA_MAX_SPEED);
    // GAME_NPPANGBAND
    else max_p_speed = calc_energy_gain(NPPANGBAND_MAX_SPEED);

}
