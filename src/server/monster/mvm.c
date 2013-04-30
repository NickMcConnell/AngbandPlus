/*
 * File: mvm.c
 * Purpose: MvM combat
 *
 * Copyright (c) 2011 PWMAngband Developers
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


#include "../s-angband.h"
#include "../attack.h"
#include "../init.h"
#include "mon-make.h"
#include "mon-msg.h"
#include "mon-spell.h"
#include "mon-timed.h"
#include "mon-util.h"
#include "../s-spells.h"


/*
 * Choose "logical" directions for monster movement in MvM combat
 */
static bool get_moves_MvM(int target_m_idx, int depth, int m_idx, int mm[5])
{
    monster_type *target_m_ptr = cave_monster(cave_get(depth), target_m_idx);
    monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);
    int y, x;
    int y2 = target_m_ptr->fy;
    int x2 = target_m_ptr->fx;

    /* Extract the "pseudo-direction" */
    y = m_ptr->fy - y2;
    x = m_ptr->fx - x2;

    /* Check for no move */
    if (!x && !y) return (FALSE);

    /* Extract some directions */
    compute_moves(x, y, mm);

    /* Want to move */
    return (TRUE);
}


/*
 * Darken all rooms containing the given location
 */
static void unlight_room_MvM(int depth, int y1, int x1)
{
    struct point_set *ps;

    ps = point_set_new(200);

    light_room_aux(ps, depth, y1, x1);

    /* Now, darken them all at once */
    cave_unlight_aux(depth, ps);

    point_set_dispose(ps);
}


/*
 * Hack -- call darkness around the monster
 * Affect all monsters in the projection radius
 */
bool unlight_area_MvM(int target_m_idx, int depth, int m_idx)
{
    monster_type *target_m_ptr = cave_monster(cave_get(depth), target_m_idx);
    int flg = PROJECT_GRID | PROJECT_KILL;
    int rad = 3;

    /* No effect outside of the dungeon during day */
    if ((depth <= 0) && is_daytime()) return FALSE;

    /* No effect on special levels */
    if (check_special_level(depth)) return FALSE;

    /* Hook into the "project()" function */
    project(m_idx, rad, depth, target_m_ptr->fy, target_m_ptr->fx, 0, GF_DARK_WEAK, flg, "killed");

    /* Darken the whole room if any */
    unlight_room_MvM(depth, target_m_ptr->fy, target_m_ptr->fx);

    /* Assume seen */
    return (TRUE);
}


bool trap_creation_MvM(int target_m_idx, int depth, int m_idx)
{
    monster_type *target_m_ptr = cave_monster(cave_get(depth), target_m_idx);
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

    /* Only on random levels */
    if (!random_level(depth)) return FALSE;

    return project(m_idx, 1, depth, target_m_ptr->fy, target_m_ptr->fx, 0, GF_MAKE_TRAP, flg,
        "killed");
}


/*
 * Creatures can cast spells, shoot missiles, and breathe in MvM combat
 */
static bool make_attack_spell_MvM(int Ind, int m_idx, int target_m_idx, int target_m_dis)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *target_m_ptr = cave_monster(cave_get(p_ptr->depth), target_m_idx);
    int thrown_spell;
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
    char m_name[NORMAL_WID], m_poss[NORMAL_WID];

    /* Monster position */
    int px = target_m_ptr->fx;
    int py = target_m_ptr->fy;

    /* Extract the blind-ness */
    bool blind = (p_ptr->timed[TMD_BLIND]? TRUE: FALSE);

    /* Extract the "see-able-ness" */
    bool seen = (!blind && p_ptr->mon_vis[m_idx]);

    /* Choose a spell to cast */
    thrown_spell = get_thrown_spell(Ind, 0, p_ptr->depth, m_idx, target_m_dis, py, px);

    /* Abort if no spell was chosen */
    if (thrown_spell < 0) return ((thrown_spell == -1)? FALSE: TRUE);

    /* If we see an unaware monster try to cast a spell, become aware of it */
    if (m_ptr->unaware) become_aware(p_ptr, m_ptr);

    /* Reveal mimics */
    if (target_m_ptr->unaware) become_aware(p_ptr, target_m_ptr);

    /* Get the monster name (or "it") */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(p_ptr, m_poss, sizeof(m_poss), m_ptr, MDESC_PRO2 | MDESC_POSS);

    /* Special case RSF_HASTE */
    if (thrown_spell == RSF_HASTE)
    {
        if (!check_antimagic(p_ptr, m_ptr))
        {
            if (blind)
                msg(p_ptr, "%s mumbles.", m_name);
            else
                msg(p_ptr, "%s concentrates on %s body.", m_name, m_poss);

            mon_inc_timed(p_ptr, m_ptr, MON_TMD_FAST, 50, 0, FALSE);
        }
    }

    /* Cast the spell. */
    else
        do_mon_spell_MvM(Ind, target_m_idx, thrown_spell, m_idx, seen);

    /* Remember what the monster did */
    if (seen)
    {
        rsf_on(l_ptr->spell_flags, thrown_spell);

        /* Innate spell */
        if (thrown_spell < MIN_NONINNATE_SPELL)
        {
            if (l_ptr->cast_innate < MAX_UCHAR) l_ptr->cast_innate++;
        }

        /* Bolt or Ball, or Special spell */
        else
        {
            if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
        }
    }

    /* A spell was cast */
    return TRUE;
}


/*
 * Find the closest target
 */
static void get_closest_target(int depth, int m_idx, int *target_idx, int *target_dis,
    bool *target_los)
{
    int i;
    monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);
    int target_m_idx = -1, target_m_dis = 9999, target_m_hp = 99999;
    bool target_m_los = FALSE, new_los;

    /* Process the monsters */
    for (i = cave_monster_max(cave_get(depth)) - 1; i >= 1; i--)
    {
        int j;
        monster_type *target_m_ptr;

        /* Skip the origin */
        if (i == m_idx) continue;

        /* Access the monster */
        target_m_ptr = cave_monster(cave_get(depth), i);

        /* Skip "dead" monsters */
        if (!target_m_ptr->r_idx) continue;

        /* Skip if the monster is not hostile */
        if (master_in_party(target_m_ptr->master, m_ptr->master)) continue;

        /* Compute distance */
        j = distance(target_m_ptr->fy, target_m_ptr->fx, m_ptr->fy, m_ptr->fx);

        /* Check if monster has LOS to the target */
        new_los = los(m_ptr->depth, m_ptr->fy, m_ptr->fx, target_m_ptr->fy, target_m_ptr->fx);

        /* Check that the closest VISIBLE target gets selected */
        /* If no visible one available just take the closest */
        if (((target_m_los >= new_los) && (j > target_m_dis)) || (target_m_los > new_los))
            continue;

        /* Skip if same distance and stronger and same visibility */
        if ((j == target_m_dis) && (target_m_ptr->hp > target_m_hp) && (target_m_los == new_los))
            continue;

        /* Remember this target */
        target_m_los = new_los;
        target_m_dis = j;
        target_m_idx = i;
        target_m_hp = target_m_ptr->hp;
    }

    /* Forget player status */
    m_ptr->smart = 0L;

    /* Always track closest target */
    (*target_idx) = target_m_idx;
    (*target_dis) = target_m_dis;
    (*target_los) = target_m_los;
}


/*
 * Process a monster, make it attack other monsters if possible
 */
static void process_monster_MvM(int Ind, int m_idx, int target_m_idx, int target_m_dis)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    int oy, ox;
    int mm[5];
    bool stagger;
    bool did_what[3];
    bool do_turn;
    bool do_move;

    /* Handle "sleep" */
    if (m_ptr->m_timed[MON_TMD_SLEEP])
    {
        /* Wake the monster */
        mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);

        /* Notice the "waking up" */
        if (p_ptr->mon_vis[m_idx] && !m_ptr->unaware)
        {
            char m_name[NORMAL_WID];

            /* Get the monster name */
            monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

            /* Dump a message */
            msg(p_ptr, "%s wakes up.", m_name);
        }

        /* Efficiency XXX XXX */
        return;
    }

    if (m_ptr->m_timed[MON_TMD_FAST])
        mon_dec_timed(p_ptr, m_ptr, MON_TMD_FAST, 1, 0, FALSE);

    if (m_ptr->m_timed[MON_TMD_SLOW])
        mon_dec_timed(p_ptr, m_ptr, MON_TMD_SLOW, 1, 0, FALSE);

    /* Handle "stun" */
    if (monster_stunned(Ind, m_idx)) return;

    /* Handle confusion, fear, poison, bleeding */
    monster_effects(Ind, m_idx);

    /* Get the origin */
    oy = m_ptr->fy;
    ox = m_ptr->fx;

    /* Attempt to cast a spell */
    if (make_attack_spell_MvM(Ind, m_idx, target_m_idx, target_m_dis)) return;

    /* Stagger */
    stagger = is_staggering(Ind, m_idx);

    /* Normal movement */
    if (!stagger)
    {
        /* Logical moves, may do nothing */
        if (!get_moves_MvM(target_m_idx, p_ptr->depth, m_idx, mm)) return;
    }

    /* Movement */
    process_move(Ind, m_idx, oy, ox, mm, stagger, target_m_idx, did_what);
    do_turn = did_what[0];
    do_move = did_what[1];

    /* If we haven't done anything, try casting a spell again */
    if (cfg_ai_smart && !do_turn && !do_move)
    {
        /* Cast spell */
        if (make_attack_spell_MvM(Ind, m_idx, target_m_idx, target_m_dis)) return;
    }

    process_move_end(Ind, m_idx, did_what);
}


/*
 * MvM code
 */
void check_monster_MvM(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr;
    int target_m_idx, target_m_dis;
    bool target_m_los;

    /* Find the closest target */
    get_closest_target(p_ptr->depth, m_idx, &target_m_idx, &target_m_dis, &target_m_los);

    /* Paranoia -- Make sure we found a closest target */
    if (target_m_idx == -1) return;

    /* Get the race */
    r_ptr = &r_info[m_ptr->r_idx];

    /*
     * Process the monster if the monster either:
     * - can "sense" the target
     * - can "see" the target
     */
    if ((target_m_dis <= (cfg_small_range? (r_ptr->aaf / 2): r_ptr->aaf)) || target_m_los)
    {
        /* Process the monster */
        process_monster_MvM(Ind, m_idx, target_m_idx, target_m_dis);
    }
}


/*
 * Determine if a monster attack against another monster succeeds.
 * Always miss 5% of the time, always hit 12% of the time.
 * Otherwise, match monster power against monster armor.
 */
int check_hit_MvM(int ac, int power, int level)
{
    int chance;

    /* Calculate the "attack quality" */
    chance = (power + (level * 3));

    /* Check if the monster was hit */
    return test_hit(chance, ac, TRUE);
}


/*
 * Attack a monster via physical attacks.
 */
bool make_attack_normal_MvM(int Ind, int target_m_idx, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
    monster_type *target_m_ptr = cave_monster(cave_get(p_ptr->depth), target_m_idx);
    monster_race *target_r_ptr = &r_info[target_m_ptr->r_idx];
    monster_lore *target_l_ptr = &p_ptr->lore[target_m_ptr->r_idx];
    int ac, rlev;
    char m_name[NORMAL_WID];
    char target_m_name[NORMAL_WID];
    int ap_cnt;
    int do_cut, do_stun, do_conf, do_fear, do_blind, do_para;
    int sound_msg;

    /* Assume a default death */
    byte note_dies = MON_MSG_DIE;

    /* Some monsters get "destroyed" */
    if (monster_is_unusual(r_ptr))
    {
        /* Special note at death */
        note_dies = MON_MSG_DESTROYED;
    }

    /* Not allowed to attack */
    if (rf_has(r_ptr->flags, RF_NEVER_BLOW)) return (FALSE);

    /* Reveal mimics */
    if (is_mimicking(target_m_ptr)) become_aware(p_ptr, target_m_ptr);

    /* Total armor */
    ac = target_m_ptr->ac;

    /* Extract the effective monster level */
    rlev = ((m_ptr->level >= 1) ? m_ptr->level : 1);

    /* Get the monster name (or "it") */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);
    monster_desc(p_ptr, target_m_name, sizeof(target_m_name), target_m_ptr, 0);

    /* Scan through all blows */
    for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
    {
        bool visible = FALSE;
        int power = 0;
        const char *act = NULL;
        bool obvious = FALSE;
        int damage = 0;
        bool dead = FALSE;

        /* Extract the attack infomation */
        int effect = m_ptr->blow[ap_cnt].effect;
        int method = m_ptr->blow[ap_cnt].method;
        int d_dice = m_ptr->blow[ap_cnt].d_dice;
        int d_side = m_ptr->blow[ap_cnt].d_side;

        /* Hack -- no more attacks */
        if (!method) break;

        /* Extract visibility (before blink) */
        if (p_ptr->mon_vis[m_idx]) visible = TRUE;

        /* Extract the attack "power" */
        power = get_power(effect);

        /* Monster hits */
        if (!effect || check_hit_MvM(ac, power, rlev))
        {
            /* Assume no effects */
            do_cut = do_stun = do_conf = do_fear = do_blind = do_para = 0;

            /* Assume no sound */
            sound_msg = MSG_GENERIC;

            /* Describe the attack method */
            switch (method)
            {
                case RBM_HIT:
                {
                    act = "hits %s";
                    do_cut = do_stun = 1;
                    sound_msg = MSG_MON_HIT;
                    break;
                }

                case RBM_TOUCH:
                {
                    act = "touches %s";
                    sound_msg = MSG_MON_TOUCH;
                    break;
                }

                case RBM_PUNCH:
                {
                    act = "punches %s";
                    do_stun = 1;
                    sound_msg = MSG_MON_PUNCH;
                    break;
                }

                case RBM_KICK:
                {
                    act = "kicks %s";
                    do_stun = 1;
                    sound_msg = MSG_MON_KICK;
                    break;
                }

                case RBM_CLAW:
                {
                    act = "claws %s";
                    do_cut = 1;
                    sound_msg = MSG_MON_CLAW;
                    break;
                }

                case RBM_BITE:
                {
                    act = "bites %s";
                    do_cut = 1;
                    sound_msg = MSG_MON_BITE;
                    break;
                }

                case RBM_STING:
                {
                    act = "stings %s";
                    sound_msg = MSG_MON_STING;
                    break;
                }

                case RBM_BUTT:
                {
                    act = "butts %s";
                    do_stun = 1;
                    sound_msg = MSG_MON_BUTT;
                    break;
                }

                case RBM_CRUSH:
                {
                    act = "crushes %s";
                    do_stun = 1;
                    sound_msg = MSG_MON_CRUSH;
                    break;
                }

                case RBM_ENGULF:
                {
                    act = "engulfs %s";
                    sound_msg = MSG_MON_ENGULF;
                    break;
                }

                case RBM_CRAWL:
                {
                    act = "crawls on %s";
                    sound_msg = MSG_MON_CRAWL;
                    break;
                }

                case RBM_DROOL:
                {
                    act = "drools on %s";
                    sound_msg = MSG_MON_DROOL;
                    break;
                }

                case RBM_SPIT:
                {
                    act = "spits on %s";
                    sound_msg = MSG_MON_SPIT;
                    break;
                }

                case RBM_GAZE:
                {
                    act = "gazes at %s";
                    sound_msg = MSG_MON_GAZE;
                    break;
                }

                case RBM_WAIL:
                {
                    act = "wails at %s";
                    sound_msg = MSG_MON_WAIL;
                    break;
                }

                case RBM_SPORE:
                {
                    act = "releases spores at %s";
                    sound_msg = MSG_MON_SPORE;
                    break;
                }

                case RBM_BEG:
                {
                    act = "begs %s for money.";
                    sound_msg = MSG_MON_BEG;
                    break;
                }

                case RBM_INSULT:
                {
                    act = "insults %s";
                    sound_msg = MSG_MON_INSULT;
                    break;
                }

                case RBM_MOAN:
                {
                    act = "curses %s";
                    sound_msg = MSG_MON_MOAN;
                    break;
                }
            }

            /* Message */
            if (act) msgt(p_ptr, sound_msg, "%s %s.", m_name, format(act, target_m_name));

            /* Roll out the damage */
            damage = damroll(d_dice, d_side);

            /* Apply appropriate damage */
            switch (effect)
            {
                case 0:
                {
                    /* Hack -- Assume obvious */
                    obvious = TRUE;

                    /* Hack -- No damage */
                    damage = 0;

                    break;
                }

                case RBE_HURT:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Hack -- monster armor reduces total damage */
                    damage -= (damage * ((ac < 240)? ac: 240) / 400);

                    /* Take damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage, note_dies);

                    break;
                }

                case RBE_POISON:
                case RBE_DISEASE:
                {
                    int mult = 3;

                    /* Obvious */
                    obvious = TRUE;

                    /* Notice immunity */
                    if (rf_has(target_r_ptr->flags, RF_IM_POIS)) mult = 1;

                    /* Take some damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage * mult, note_dies);
                    if (dead) break;

                    /* Take "poison" effect */
                    mon_inc_timed(p_ptr, target_m_ptr, MON_TMD_POIS, randint1(rlev) + 5,
                        MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, FALSE);

                    break;
                }

                case RBE_UN_BONUS:
                case RBE_UN_POWER:
                case RBE_EAT_GOLD:
                case RBE_EAT_ITEM:
                case RBE_EAT_FOOD:
                case RBE_EAT_LIGHT:
                case RBE_LOSE_STR:
                case RBE_LOSE_INT:
                case RBE_LOSE_WIS:
                case RBE_LOSE_DEX:
                case RBE_LOSE_CON:
                case RBE_LOSE_CHR:
                case RBE_LOSE_ALL:
                case RBE_EXP_10:
                case RBE_EXP_20:
                case RBE_EXP_40:
                case RBE_EXP_80:
                case RBE_FORGET:
                case RBE_TIME:
                case RBE_DISARM:
                case RBE_FAMINE:
                {
                    /* Take some damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage, note_dies);

                    break;
                }  

                case RBE_ACID:
                {
                    int mult = 3;

                    /* Obvious */
                    obvious = TRUE;

                    /* Notice immunity */
                    if (rf_has(target_r_ptr->flags, RF_IM_ACID))
                    {
                        mult = 1;
                        if (visible) rf_on(target_l_ptr->flags, RF_IM_ACID);
                    }

                    /* Take some damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage * mult, note_dies);

                    break;
                }

                case RBE_ELEC:
                {
                    int mult = 3;

                    /* Obvious */
                    obvious = TRUE;

                    /* Notice immunity */
                    if (rf_has(target_r_ptr->flags, RF_IM_ELEC))
                    {
                        mult = 1;
                        if (visible) rf_on(target_l_ptr->flags, RF_IM_ELEC);
                    }

                    /* Take some damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage * mult, note_dies);

                    break;
                }

                case RBE_FIRE:
                {
                    int mult = 3;

                    /* Obvious */
                    obvious = TRUE;

                    /* Notice immunity */
                    if (rf_has(target_r_ptr->flags, RF_IM_FIRE))
                    {
                        mult = 1;
                        if (visible) rf_on(target_l_ptr->flags, RF_IM_FIRE);
                    }

                    /* Notice susceptibility */
                    else if (rf_has(target_r_ptr->flags, RF_HURT_FIRE))
                    {
                        mult = 6;
                        if (visible) rf_on(target_l_ptr->flags, RF_HURT_FIRE);
                    }

                    /* Take some damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage * mult, note_dies);

                    break;
                }    

                case RBE_COLD:
                {
                    int mult = 3;

                    /* Obvious */
                    obvious = TRUE;

                    /* Notice immunity */
                    if (rf_has(target_r_ptr->flags, RF_IM_COLD))
                    {
                        mult = 1;
                        if (visible) rf_on(target_l_ptr->flags, RF_IM_COLD);
                    }

                    /* Notice susceptibility */
                    else if (rf_has(target_r_ptr->flags, RF_HURT_COLD))
                    {
                        mult = 6;
                        if (visible) rf_on(target_l_ptr->flags, RF_HURT_COLD);
                    }

                    /* Take some damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage * mult, note_dies);

                    break;
                }

                case RBE_BLIND:
                {
                    /* Take damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage, note_dies);
                    if (dead) break;

                    /* Obvious */
                    obvious = TRUE;

                    /* Blinding attack */
                    do_blind = 1;

                    break;
                }

                case RBE_CONFUSE:
                case RBE_HALLU:
                {
                    /* Take damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage, note_dies);
                    if (dead) break;

                    /* Obvious */
                    obvious = TRUE;

                    /* Confusing attack */
                    do_conf = 1;

                    break;
                }

                case RBE_TERRIFY:
                {
                    /* Take damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage, note_dies);
                    if (dead) break;

                    /* Obvious */
                    obvious = TRUE;

                    /* Fear attack */
                    do_fear = 1;

                    break;
                }

                case RBE_PARALYZE:
                {
                    /* Take damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage, note_dies);
                    if (dead) break;

                    /* Obvious */
                    obvious = TRUE;

                    /* Paralyzing attack */
                    do_para = 1;

                    break;
                }

                case RBE_SHATTER:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Hack -- Reduce damage based on the monster armor class */
                    damage -= (damage * ((ac < 240)? ac: 240) / 400);

                    /* Take damage */
                    dead = take_hit_MvM(m_idx, target_m_ptr, damage, note_dies);
                    if (dead) break;

                    /* Radius 8 earthquake centered at the monster */
                    if (damage > 23)
                        earthquake(NULL, m_ptr->depth, m_ptr->fy, m_ptr->fx, 8);

                    break;
                }
            }

            /* Handle effects */
            if (!dead)
            {
                /* Hack -- only one of cut or stun */
                if (do_cut && do_stun)
                {
                    /* Cancel cut */
                    if (magik(50))
                        do_cut = 0;

                    /* Cancel stun */
                    else
                        do_stun = 0;
                }

                /* Handle cut (Shard breathers resist) */
                if (do_cut) do_cut = get_cut(d_dice, d_side, damage);
                if (do_cut)
                {
                    mon_inc_timed(p_ptr, target_m_ptr, MON_TMD_CUT, do_cut,
                        MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, FALSE);
                }

                /* Handle stun */
                if (do_stun) do_stun = get_stun(d_dice, d_side, damage);
                if (do_stun)
                {
                    mon_inc_timed(p_ptr, target_m_ptr, MON_TMD_STUN, do_stun,
                        MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, FALSE);
                }

                /* Apply fear */
                if (do_fear)
                {
                    mon_inc_timed(p_ptr, target_m_ptr, MON_TMD_FEAR, 3 + randint1(rlev),
                        MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, FALSE);
                }

                /* Apply confusion */
                if (do_conf)
                {
                    mon_inc_timed(p_ptr, target_m_ptr, MON_TMD_CONF, 3 + randint1(rlev),
                        MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, FALSE);
                }

                /* Apply blindness */
                if (do_blind)
                {
                    mon_inc_timed(p_ptr, target_m_ptr, MON_TMD_BLIND, 10 + randint1(rlev),
                        MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, FALSE);
                }

                /* Handle paralysis */
                if (do_para)
                {
                    mon_inc_timed(p_ptr, target_m_ptr, MON_TMD_HOLD, 3 + randint1(rlev),
                        MON_TMD_FLG_NOTIFY | MON_TMD_MON_SOURCE, FALSE);
                }
            }
        }

        /* Monster missed */
        else
        {
            /* Analyze failed attacks */
            switch (method)
            {
                case RBM_HIT:
                case RBM_TOUCH:
                case RBM_PUNCH:
                case RBM_KICK:
                case RBM_CLAW:
                case RBM_BITE:
                case RBM_STING:
                case RBM_BUTT:
                case RBM_CRUSH:
                case RBM_ENGULF:

                /* Visible monsters */
                if (visible)
                {
                    /* Message */
                    msgt(p_ptr, MSG_MISS, "%s misses %s.", m_name, target_m_name);
                }

                break;
            }
        }

        /* Analyze "visible" monsters only */
        if (visible)
        {
            /* Count "obvious" attacks (and ones that cause damage) */
            if (obvious || damage || (l_ptr->blows[ap_cnt] > 10))
            {
                /* Count attacks of this type */
                if (l_ptr->blows[ap_cnt] < MAX_UCHAR) l_ptr->blows[ap_cnt]++;
            }
        }

        if (dead) break;
    }

    /* Assume we attacked */
    return (TRUE);
}


/*
 * Decreases a monster's hit points by `dam` and handle monster death.
 *
 * Returns TRUE if the monster has been killed (and deleted).
 */
bool take_hit_MvM(int who, struct monster *m_ptr, int dam, byte note)
{
    monster_type *who_ptr = cave_monster(cave_get(m_ptr->depth), who);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Redraw (later) if needed */
    update_health(m_ptr->midx);

    /* Wake the monster up */
    mon_clear_timed(NULL, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);

    /* "Unique" monsters can only be "killed" by the player */
    if (rf_has(r_ptr->flags, RF_UNIQUE) && (dam > m_ptr->hp)) dam = m_ptr->hp;

    /* Hurt the monster */
    m_ptr->hp -= dam;

    /* Dead monster */
    if (m_ptr->hp < 0)
    {
        int i;
        char m_name[NORMAL_WID];

        for (i = 1; i < NumPlayers + 1; i++)
        {
            player_type *p_ptr = player_get(i);
            byte note_dies = note;
            bool seen = p_ptr->mon_vis[m_ptr->midx];

            /* If he's not here, skip him */
            if (p_ptr->depth != m_ptr->depth) continue;

            /* Give detailed messages if destroyed */
            if (!seen) note_dies = MON_MSG_MORIA_DEATH;

            /* Dump the note */
            monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);
            add_monster_message(p_ptr, m_name, m_ptr, note_dies, TRUE);

            /* Reward the master with some experience */
            if (p_ptr->id == who_ptr->master) monster_give_xp(i, m_ptr, TRUE);
        }

        /* Drop objects being carried */
        monster_drop_carried(0, m_ptr, -1, FALSE, NULL, NULL);

        /* Drop a corpse */
        monster_drop_corpse(0, m_ptr);

        /* Delete the monster */
        delete_monster_idx(cave_get(m_ptr->depth), m_ptr->midx);

        /* Monster is dead */
        return (TRUE);
    }

    /* Not dead yet */
    return (FALSE);
}
