/* File: spells3.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Spell code (part 3) */

#include "angband.h"
#include <assert.h>

/* Maximum number of tries for teleporting */
#define MAX_TRIES 100

/* 1/x chance of reducing stats (for elemental attacks) */
#define HURT_CHANCE 16


static bool cave_monster_teleportable_bold(mon_ptr mon, int y, int x, u32b mode)
{
    point_t   dest_pos = point_create(x, y);
    cave_ptr  dest_grid = cave_at(dest_pos);
    mon_ptr   dest_mon = mon_at(dest_pos);
    feat_ptr  dest_feat = &f_info[dest_grid->feat];

    if (!have_flag(dest_feat->flags, FF_TELEPORTABLE)) return FALSE;
    if (dest_mon && dest_mon != mon) return FALSE;
    if (plr_at(dest_pos)) return FALSE;
    if (is_glyph_grid(dest_grid)) return FALSE;
    if (is_mon_trap_grid(dest_grid)) return FALSE;

    if (!(mode & TELEPORT_PASSIVE))
    {
        if (!monster_can_cross_terrain(dest_grid->feat, mon_race(mon), 0)) return FALSE;
    }

    return TRUE;
}


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
bool teleport_away(int m_idx, int dis, u32b mode)
{
    int d, i, min;
    int tries = 0;

    bool look = TRUE;

    monster_type *m_ptr = dun_mon(cave, m_idx);
    point_t old_pos = m_ptr->pos;
    point_t new_pos = {0};

    /* Paranoia */
    if (mon_is_dead(m_ptr)) return FALSE;

    /* Minimum distance */
    min = dis / 2;

    if ((mode & TELEPORT_DEC_VALOUR) &&
        (((p_ptr->chp * 10) / p_ptr->mhp) > 5) &&
        (4+randint1(5) < ((p_ptr->chp * 10) / p_ptr->mhp)))
    {
        virtue_add(VIRTUE_VALOUR, -1);
    }

    /* Look until done */
    while (look)
    {
        tries++;

        /* Verify max distance */
        if (dis > 200) dis = 200;

        /* Try several locations */
        for (i = 0; i < 500; i++)
        {
            /* Pick a (possibly illegal) location
             * XXX We must bounds check outside the loop. For example,
             * in the Thieves' Quest, it is impossible to get a tile
             * farther than min that is also legal. */
            while (1)
            {
                new_pos = point_random_jump(old_pos, dis);
                d = point_distance(old_pos, new_pos);
                if (min <= d && d <= dis) break;
            }
            if (!dun_pos_interior(cave, new_pos)) continue;
            if (!cave_monster_teleportable_bold(m_ptr, new_pos.y, new_pos.x, mode)) continue;

            /* No teleporting into vaults and such */
            if (!quests_get_current() && (cave_at(new_pos)->info & CAVE_ICKY)) continue;

            /* This grid looks good */
            look = FALSE;

            /* Stop looking */
            break;
        }

        /* Increase the maximum distance */
        dis = dis * 2;

        /* Decrease the minimum distance */
        min = min / 2;

        /* Stop after MAX_TRIES tries */
        if (tries > MAX_TRIES) return (FALSE);
    }
    
    dun_move_mon(cave, m_ptr, new_pos);

    reset_target(m_ptr);
    if (mon_race(m_ptr)->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
        p_ptr->update |= (PU_MON_LITE);

    return (TRUE);
}



/*
 * Teleport monster next to a grid near the given location
 */
void teleport_monster_to(int m_idx, int ty, int tx, int power, u32b mode)
{
    int d, i, min;
    int attempts = 500;
    int dis = 2;
    bool look = TRUE;
    monster_type *m_ptr = dun_mon(cave, m_idx);
    point_t tgt_pos = point_create(tx, ty);
    point_t new_pos = {0};

    /* Paranoia */
    if (mon_is_dead(m_ptr)) return;

    /* "Skill" test */
    if (randint1(100) > power) return;

    /* Minimum distance */
    min = dis / 2;

    /* Look until done */
    while (look && --attempts)
    {
        /* Verify max distance */
        if (dis > 200) dis = 200;

        /* Try several locations */
        for (i = 0; i < 500; i++)
        {
            /* Pick a (possibly illegal) location */
            while (1)
            {
                new_pos = point_random_jump(tgt_pos, dis);
                if (!dun_pos_interior(cave, new_pos)) continue;
                d = point_distance(tgt_pos, new_pos);
                if (min <= d && d <= dis) break;
            }

            if (!cave_monster_teleportable_bold(m_ptr, new_pos.y, new_pos.x, mode)) continue;

            /* No teleporting into vaults and such */
            /* if (cave_at_xy(nx, ny)->info & (CAVE_ICKY)) continue; */

            /* This grid looks good */
            look = FALSE;

            /* Stop looking */
            break;
        }

        /* Increase the maximum distance */
        dis = dis * 2;

        /* Decrease the minimum distance */
        min = min / 2;
    }

    if (attempts < 1) return;

    dun_move_mon(cave, m_ptr, new_pos);

    if (mon_race(m_ptr)->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
        p_ptr->update |= (PU_MON_LITE);
}


bool cave_player_teleportable_bold(int y, int x, u32b mode)
{
    point_t  pos = point_create(x, y);
    cave_ptr grid = cave_at(pos);
    mon_ptr  mon = mon_at(pos);
    feat_ptr feat = &f_info[grid->feat];

    if (!have_flag(feat->flags, FF_TELEPORTABLE)) return FALSE;
    if (!(mode & TELEPORT_NONMAGICAL) && (grid->info & CAVE_ICKY)) return FALSE; /* no vaults */
    if (mon && mon->id != p_ptr->riding) return FALSE;
    if (have_flag(feat->flags, FF_HIT_TRAP)) return FALSE;

    if (!(mode & TELEPORT_PASSIVE))
    {
        if (!player_can_enter(grid->feat, 0)) return FALSE;

        if (have_flag(feat->flags, FF_WATER) && have_flag(feat->flags, FF_DEEP))
        {
            if (!p_ptr->levitation && !p_ptr->can_swim) return FALSE;
        }

        if (have_flag(feat->flags, FF_LAVA) && res_pct(RES_FIRE) < 100 && !plr_tim_find(T_INVULN))
        {
            /* (Almost) always forbid deep lava */
            if (have_flag(feat->flags, FF_DEEP) && !elemental_is_(ELEMENTAL_FIRE)) return FALSE;

            /* Forbid shallow lava when the player don't have levitation */
            if (!p_ptr->levitation) return FALSE;
        }

    }

    return TRUE;
}


/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 *
 * There was a nasty tendency for a long time; which was causing the
 * player to "bounce" between two or three different spots because
 * these are the only spots that are "far enough" way to satisfy the
 * algorithm.
 *
 * But this tendency is now removed; in the new algorithm, a list of
 * candidates is selected first, which includes at least 50% of all
 * floor grids within the distance, and any single grid in this list
 * of candidates has equal possibility to be choosen as a destination.
 */

#define MAX_TELEPORT_DISTANCE 200

bool teleport_player_aux(int dis, u32b mode)
{
    int candidates_at[MAX_TELEPORT_DISTANCE + 1];
    int total_candidates, cur_candidates;
    int min_dis, pick, i;
    rect_t rect;
    point_t min, max, p;

    if (p_ptr->anti_tele && !(mode & TELEPORT_NONMAGICAL))
    {
        msg_print("A mysterious force prevents you from teleporting!");
        equip_learn_flag(OF_NO_TELE);
        return FALSE;
    }

    /* calculate a search rect centered on the player */
    rect = rect_create_centered(p_ptr->pos, dis, dis);

    /* clip search rect to dungeon and get iteration bounds */
    rect = rect_intersect(rect_interior(cave->rect), rect);
    min = rect_top_left(rect);
    max = rect_bottom_right(rect);

    /* Initialize counters */
    total_candidates = 0;
    for (i = 0; i <= MAX_TELEPORT_DISTANCE; i++)
        candidates_at[i] = 0;

    /* Limit the distance */
    if (dis > MAX_TELEPORT_DISTANCE) dis = MAX_TELEPORT_DISTANCE;

    /* Search valid locations */
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            int d;

            /* Skip illegal locations */
            if (!cave_player_teleportable_bold(p.y, p.x, mode)) continue;

            /* Calculate distance */
            d = plr_distance(p);

            /* Skip too far locations */
            if (d > dis) continue;

            /* Strafing ... Maintains LoS with old location */
            if ((mode & TELEPORT_LINE_OF_SIGHT) && !plr_los(p)) continue;

            /* Count the total number of candidates */
            total_candidates++;

            /* Count the number of candidates in this circumference */
            candidates_at[d]++;
        }
    }

    /* No valid location! */
    if (0 == total_candidates) return FALSE;

    /* Fix the minimum distance */
    for (cur_candidates = 0, min_dis = dis; min_dis >= 0; min_dis--)
    {
        cur_candidates += candidates_at[min_dis];

        /* 50% of all candidates will have an equal chance to be choosen. */
        if (cur_candidates && (cur_candidates >= total_candidates / 2)) break;
    }

    /* Pick up a single location randomly */
    pick = randint1(cur_candidates);

    /* Search again the choosen location */
    for (p.y = min.y; p.y <= max.y; p.y++)
    {
        for (p.x = min.x; p.x <= max.x; p.x++)
        {
            int d;

            /* Skip illegal locations */
            if (!cave_player_teleportable_bold(p.y, p.x, mode)) continue;

            /* Calculate distance */
            d = plr_distance(p);

            /* Skip too far locations */
            if (d > dis) continue;

            /* Skip too close locations */
            if (d < min_dis) continue;

            /* Strafing ... Maintains LoS with old location */
            if ((mode & TELEPORT_LINE_OF_SIGHT) && !plr_los(p)) continue;

            /* This grid was picked up? */
            pick--;
            if (!pick) break;
        }

        /* Exit the loop */
        if (!pick) break;
    }

    if (plr_at(p)) return FALSE;

    /* Move the player */
    move_player_effect(p, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
    return TRUE;
}

void teleport_player(int dis, u32b mode)
{
    point_t old_pos = p_ptr->pos;
    point_t d;

    if (!teleport_player_aux(dis, mode)) return;

    /* Monsters with teleport ability may follow the player */
    for (d.x = -2; d.x < 3; d.x++)
    {
        for (d.y = -2; d.y < 3; d.y++)
        {
            point_t pos = point_add(old_pos, d);
            mon_ptr mon;

            if (!dun_pos_interior(cave, pos)) continue;
            mon = mon_at(pos);

            /* A monster except your mount may follow */
            if (mon && mon->id != p_ptr->riding)
            {
                mon_race_ptr race = mon_race(mon);

                /* Hack: Duelist Disengage. Marked foe can never follow */
                if ((mode & TELEPORT_DISENGAGE) && mon->id == p_ptr->duelist_target_idx)
                    continue;

                if (!point_project(old_pos, pos)) continue;

                /* The latter limitations are to avoid totally unkillable suckers...  */
                if (mon_race_can_teleport(race) && !(race->flagsr & RFR_RES_TELE) && !(mon->mflag2 & MFLAG2_VAULT))
                {
                    if (!mon_tim_find(mon, MT_SLEEP))
                        teleport_monster_to(mon->id, p_ptr->pos.y, p_ptr->pos.x, race->level, 0);
                }

                if (mon->r_idx == MON_MONKEY_CLONE)
                    teleport_monster_to(mon->id, p_ptr->pos.y, p_ptr->pos.x, 10000, 0);
            }
        }
    }
}


void teleport_player_away(int m_idx, int dis)
{
    point_t old_pos = p_ptr->pos;
    point_t d;

    if (!teleport_player_aux(dis, TELEPORT_PASSIVE)) return;

    /* Monsters with teleport ability may follow the player */
    for (d.x = -2; d.x < 3; d.x++)
    {
        for (d.y = -2; d.y < 3; d.y++)
        {
            point_t pos = point_add(old_pos, d);
            mon_ptr mon;

            if (!dun_pos_interior(cave, pos)) continue;
            mon = mon_at(pos);

            /* A monster except your mount or caster may follow */
            if (mon && mon->id != p_ptr->riding && mon->id != m_idx)
            {
                mon_race_ptr race = mon_race(mon);

                if (!point_project(old_pos, pos)) continue;

                /* The latter limitations are to avoid totally unkillable suckers...  */
                if (mon_race_can_teleport(race) && !(race->flagsr & RFR_RES_TELE) && !(mon->mflag2 & MFLAG2_VAULT))
                {
                    if (!mon_tim_find(mon, MT_SLEEP))
                        teleport_monster_to(mon->id, p_ptr->pos.y, p_ptr->pos.x, race->level, 0);
                }
            }
        }
    }
}


/*
 * Teleport player to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_to(int ny, int nx, u32b mode)
{
    point_t tgt_pos = point_create(nx, ny);
    point_t new_pos;
    int dis = 0, ctr = 0;
    int attempt = 0;
    const int max_attempts = 10 * 1000;

    if (p_ptr->anti_tele && !(mode & TELEPORT_NONMAGICAL))
    {
        msg_print("A mysterious force prevents you from teleporting!");
        equip_learn_flag(OF_NO_TELE);
        return;
    }

    /* Find a usable location */
    while (1)
    {
        /* Pick a nearby legal location */
        while (1)
        {
            new_pos = point_random_jump(tgt_pos, dis);
            if (dun_pos_interior(cave, new_pos)) break;
        }

        ++attempt;
        if (attempt >= max_attempts)
        {
            msg_print("There is no effect!");
            return;
        }

        /* Accept teleportable floor grids */
        if (cave_player_teleportable_bold(new_pos.y, new_pos.x, mode)) break;

        /* Occasionally advance the distance */
        if (++ctr > (4 * dis * dis + 4 * dis + 1))
        {
            ctr = 0;
            dis++;
        }
    }

    /* Sound */
    sound(SOUND_TELEPORT);

    /* Move the player */
    move_player_effect(new_pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
}

static u32b _flag = 0;
static bool _has_flag(object_type *o_ptr) {
    if (!obj_is_cursed(o_ptr))
        return obj_has_flag(o_ptr, _flag);
    return FALSE;
}
void teleport_away_followable(int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    int          oldfy = m_ptr->pos.y;
    int          oldfx = m_ptr->pos.x;
    bool         old_ml = m_ptr->ml;
    int          old_cdis = m_ptr->cdis;

    teleport_away(m_idx, MAX_SIGHT * 2 + 5, 0L);

    if (old_ml && (old_cdis <= MAX_SIGHT) && !world_monster && los(p_ptr->pos.y, p_ptr->pos.x, oldfy, oldfx))
    {
        bool follow = FALSE;

        if (mut_present(MUT_TELEPORT)) follow = TRUE;
        else if (p_ptr->pclass == CLASS_DUELIST
              && p_ptr->duelist_target_idx == m_idx
              && p_ptr->lev >= 30 )
        {
            follow = TRUE;
        }
        else
        {
            _flag = OF_TELEPORT;
            if (equip_find_first(_has_flag))
                follow = TRUE;
        }

        if (follow)
        {
            if (get_check("Do you follow it? "))
            {
                if (one_in_(3))
                {
                    teleport_player(200, TELEPORT_PASSIVE);
                    msg_print("Failed!");
                }
                else
                {
                    if (p_ptr->pclass == CLASS_DUELIST
                     && p_ptr->duelist_target_idx == m_idx
                     && p_ptr->lev >= 30 )
                    {
                        msg_print("You invoke Unending Pursuit ... The duel continues!");
                    }
                    teleport_player_to(m_ptr->pos.y, m_ptr->pos.x, 0L);
                }
                p_ptr->energy_need += ENERGY_NEED();
            }
        }
    }
}


/*
 * Teleport the player one level up or down (random when legal)
 * Note: If m_idx <= 0, target is player.
 */
void teleport_level(int m_idx)
{
    bool see_m = TRUE;

    if (m_idx <= 0)
    {
    }
    else /* To monster */
    {
        monster_type *m_ptr = dun_mon(cave, m_idx);
        see_m = mon_show_msg(m_ptr);
    }

    /* No effect in some case */
    if (TELE_LEVEL_IS_INEFF(m_idx))
    {
        if (see_m) msg_print("There is no effect.");
        return;
    }

    if (m_idx <= 0 && p_ptr->anti_tele) /* To player */
    {
        msg_print("A mysterious force prevents you from teleporting!");
        equip_learn_flag(OF_NO_TELE);
        return;
    }

    if (m_idx <= 0)
    {
        assert(p_ptr->dun_id == cave->dun_id);
        dun_teleport_level_plr(cave);
    }
    else
    {
        dun_teleport_level_mon(cave, dun_mon(cave, m_idx));
    }
}

bool reset_recall(void)
{
    int  dummy = 0;
    char ppp[80];
    char tmp_val[160];
    dun_type_ptr type = dun_types_choose("Reset Which Dungeon?", FALSE);

    if (!type) return FALSE;
    if (!dun_pos_interior(dun_mgr()->world, type->world_pos))
    {
        msg_format("%s does not exist in this world.", type->name);
        return FALSE;
    }

    sprintf(ppp, "Reset to which level (%d-%d): ", type->min_dun_lvl, type->plr_max_lvl);

    /* Default */
    sprintf(tmp_val, "%d", MAX(cave->dun_lvl, 1));

    /* Ask for a level */
    if (get_string(ppp, tmp_val, 10))
    {
        /* Extract request */
        dummy = atoi(tmp_val);

        /* Paranoia */
        if (dummy < 1) dummy = 1;

        /* Paranoia */
        if (dummy > type->plr_max_lvl) dummy = type->plr_max_lvl;
        if (dummy < type->min_dun_lvl) dummy = type->min_dun_lvl;

        type->plr_max_lvl = dummy;

        msg_format("Recall depth set to level %d (%d').", dummy, dummy * 50);
    }
    else
    {
        return FALSE;
    }
    return TRUE;
}


/*
 * Apply disenchantment to the player's stuff
 *
 * XXX XXX XXX This function is also called from the "melee" code
 *
 * Return "TRUE" if the player notices anything
 */
bool apply_disenchant(int mode)
{
    int slot = equip_random_slot(object_is_weapon_armour_ammo);

    if (slot)
    {
        object_type     *o_ptr = equip_obj(slot);
        char            o_name[MAX_NLEN];
        int to_h, to_d, to_a, pval;

        if (o_ptr->to_h <= 0 && o_ptr->to_d <= 0 && o_ptr->to_a <= 0 && o_ptr->pval <= 1)
            return FALSE;

        object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        if (demigod_is_(DEMIGOD_HEPHAESTUS))
        {
            return TRUE;
        }
        if (obj_is_art(o_ptr) && (randint0(100) < 71))
        {
            msg_format("Your %s resists disenchantment!", o_name);
            return TRUE;
        }

        if (obj_has_flag(o_ptr, OF_RES_DISEN))
        {
            msg_format("Your %s resists disenchantment!", o_name);
            obj_learn_flag(o_ptr, OF_RES_DISEN);
            return TRUE;
        }

        /* Memorize old value */
        to_h = o_ptr->to_h;
        to_d = o_ptr->to_d;
        to_a = o_ptr->to_a;
        pval = o_ptr->pval;

        /* Disenchant tohit */
        if (o_ptr->to_h > 0) o_ptr->to_h--;
        if ((o_ptr->to_h > 5) && (randint0(100) < 20)) o_ptr->to_h--;

        /* Disenchant todam */
        if (o_ptr->to_d > 0) o_ptr->to_d--;
        if ((o_ptr->to_d > 5) && (randint0(100) < 20)) o_ptr->to_d--;

        /* Disenchant toac */
        if (o_ptr->to_a > 0) o_ptr->to_a--;
        if ((o_ptr->to_a > 5) && (randint0(100) < 20)) o_ptr->to_a--;

        /* Disenchant pval (occasionally) */
        /* Unless called from wild_magic() */
        if ((o_ptr->pval > 1) && one_in_(13) && !(mode & 0x01)) o_ptr->pval--;

        if ((to_h != o_ptr->to_h) || (to_d != o_ptr->to_d) ||
            (to_a != o_ptr->to_a) || (pval != o_ptr->pval))
        {
            msg_format("Your %s was disenchanted!", o_name);
            virtue_add(VIRTUE_HARMONY, 1);
            virtue_add(VIRTUE_ENCHANTMENT, -2);

            p_ptr->update |= (PU_BONUS);
            p_ptr->window |= (PW_EQUIP);
            android_calc_exp();
        }
        return TRUE;
    }
    return FALSE;
}


void mutate_player(void)
{
    int max1, cur1, max2, cur2, ii, jj, i;
    bool sustains[6] = {0};

    /* Pick a pair of stats. Sustains give players a chance to protect
     * key stats in the early game (e.g. mages are forced to hyper focus
     * stat boosts on Int and swapping 18/50 with 12 might as well just
     * kill the player). Nexus is common around DL30 while stat potions
     * are not.*/
    sustains[A_STR] = p_ptr->sustain_str;
    sustains[A_INT] = p_ptr->sustain_int;
    sustains[A_WIS] = p_ptr->sustain_wis;
    sustains[A_DEX] = p_ptr->sustain_dex;
    sustains[A_CON] = p_ptr->sustain_con;
    sustains[A_CHR] = p_ptr->sustain_chr;

    for (;;)
    {
        ii = randint0(6);
        if (!sustains[ii]) break;
        if (one_in_(6)) break;
    }
    for (;;)
    {
        jj = randint0(6);
        if (jj == ii) continue;
        if (!sustains[jj]) break;
        if (one_in_(6)) break;
    }

    max1 = p_ptr->stat_max[ii];
    cur1 = p_ptr->stat_cur[ii];
    max2 = p_ptr->stat_max[jj];
    cur2 = p_ptr->stat_cur[jj];

    p_ptr->stat_max[ii] = max2;
    p_ptr->stat_cur[ii] = cur2;
    p_ptr->stat_max[jj] = max1;
    p_ptr->stat_cur[jj] = cur1;

    for (i=0;i<6;i++)
    {
        if(p_ptr->stat_max[i] > p_ptr->stat_max_max[i]) p_ptr->stat_max[i] = p_ptr->stat_max_max[i];
        if(p_ptr->stat_cur[i] > p_ptr->stat_max_max[i]) p_ptr->stat_cur[i] = p_ptr->stat_max_max[i];
    }

    if (p_ptr->wizard)
    {
        msg_format("<color:v>Swapped <color:R>%s</color> with <color:R>%s</color>.</color>",
            stat_abbrev_true[ii], stat_abbrev_true[jj]);
    }
    p_ptr->update |= (PU_BONUS);
}


/*
 * Apply Nexus
 */
static void _nexus_travel(void)
{
    int          level = 10 + MAX(20, plr_prorata_level(100));
    dun_type_ptr type;
    if (!plr_on_surface() && !plr_in_dungeon())
    {
        msg_print("There is no effect.");
        return; /* paranoia wrt arena or town quests */
    }
    type = dun_types_random(level);
    if (type)
    {
        if (level > type->max_dun_lvl) level = type->max_dun_lvl;
        if (level < type->min_dun_lvl) level = type->min_dun_lvl;
        level = rand_range(type->min_dun_lvl, level);
        dun_mgr_wizard_jump(type->id, level);
    }
}

void apply_nexus(monster_type *m_ptr)
{
    if (!m_ptr) /* wizard testing */
    {
        _nexus_travel();
        return;
    }
    switch (randint1(7))
    {
        case 1: case 2: case 3:
        {
            teleport_player(200, TELEPORT_PASSIVE);
            break;
        }

        case 4: case 5:
        {
            teleport_player_to(m_ptr->pos.y, m_ptr->pos.x, TELEPORT_PASSIVE);
            break;
        }

        case 6:
        {
            if (randint0(100) < p_ptr->skills.sav)
            {
                msg_print("You resist the effects!");

                break;
            }

            /* Teleport Level */
            teleport_level(0);
            break;
        }

        case 7:
        {
            if (randint0(100) < p_ptr->skills.sav)
            {
                msg_print("You resist the effects!");
                break;
            }
            if (p_ptr->pclass == CLASS_WILD_TALENT)
            {
                msg_print("Your body starts to scramble...");
                wild_talent_scramble();
            }
            else
            {
                msg_print("The world around you starts to scramble...");
                _nexus_travel();
            }
            break;
        }
    }
}


/*
 * Charge a lite (torch or latern)
 */
void phlogiston(void)
{
    int slot = equip_find_obj(TV_LITE, SV_ANY);
    if (slot)
    {
        int max_flog = 0;
        object_type *o_ptr = equip_obj(slot);

        if (o_ptr->sval == SV_LITE_LANTERN)
            max_flog = FUEL_LAMP;
        else if (o_ptr->sval == SV_LITE_TORCH)
            max_flog = FUEL_TORCH;

        if (o_ptr->xtra4 >= max_flog || !max_flog)
        {
            msg_print("No more phlogiston can be put in this item.");
            return;
        }

        o_ptr->xtra4 += (max_flog / 2);
        msg_print("You add phlogiston to your light item.");
        if (o_ptr->xtra4 >= max_flog)
        {
            o_ptr->xtra4 = max_flog;
            msg_print("Your light item is full.");
        }

        p_ptr->update |= PU_TORCH;
    }
    else
        msg_print("Nothing happens.");
}

/*
 * Brand the current weapon
 */
static bool _can_brand_weapon(obj_ptr obj)
{
    if (!obj_is_weapon(obj)) return FALSE;
    if (!object_is_nameless(obj)) return FALSE;
    if (object_is_(obj, TV_SWORD, SV_POISON_NEEDLE)) return FALSE;
    /* Hengband: if (object_is_(obj, TV_SWORD, SV_DIAMOND_EDGE)) return FALSE;*/
    if (have_flag(obj->flags, OF_NO_REMOVE)) return FALSE;
    return TRUE;
}

bool brand_weapon(int brand_type)
{
    obj_prompt_t prompt = {0};
    bool         result = FALSE;

    prompt.prompt = "Enchant which weapon?";
    prompt.error = "You have nothing to enchant.";
    prompt.filter = _can_brand_weapon;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    if (brand_type == -1)
    {
        result = brand_weapon_aux(prompt.obj);
    }
    else
    {
        ego_brand_weapon(prompt.obj, brand_type);
        result = TRUE;
    }
    if (result)
    {
        enchant(prompt.obj, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);
        prompt.obj->discount = 99;

        virtue_add(VIRTUE_ENCHANTMENT, 2);
        obj_identify_fully(prompt.obj);
        obj_display(prompt.obj);
        obj_release(prompt.obj, OBJ_RELEASE_ENCHANT);
    }
    else
    {
        if (flush_failure) flush();
        msg_print("The Branding failed.");
        virtue_add(VIRTUE_ENCHANTMENT, -2);
    }
    return TRUE;
}
/* Hack for old branding spells attempting to make now non-existent ego types! */
bool brand_weapon_slaying(int brand_flag, int res_flag)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Enchant which weapon?";
    prompt.error = "You have nothing to enchant.";
    prompt.filter = _can_brand_weapon;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    prompt.obj->name2 = EGO_WEAPON_SLAYING;
    add_flag(prompt.obj->flags, brand_flag);
    if (res_flag != OF_INVALID)
        add_flag(prompt.obj->flags, res_flag);

    enchant(prompt.obj, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);
    prompt.obj->discount = 99;

    virtue_add(VIRTUE_ENCHANTMENT, 2);
    obj_identify_fully(prompt.obj);
    obj_display(prompt.obj);
    obj_release(prompt.obj, OBJ_RELEASE_ENCHANT);
    return TRUE;
}
static int _crafting_lvl(void)
{
    return plr_prorata_level(75);
}
bool brand_weapon_aux(object_type *o_ptr)
{
    if (have_flag(o_ptr->flags, OF_NO_REMOVE))
        return FALSE;
    apply_magic(o_ptr, _crafting_lvl(), AM_GOOD | AM_GREAT | AM_NO_FIXED_ART | AM_CRAFTING);
    return TRUE;
}
bool brand_armour_aux(object_type *o_ptr)
{
    if (have_flag(o_ptr->flags, OF_NO_REMOVE))
        return FALSE;
    apply_magic(o_ptr, _crafting_lvl(), AM_GOOD | AM_GREAT | AM_NO_FIXED_ART | AM_CRAFTING);
    return TRUE;
}

/*
 * Vanish all walls in this floor
 */
static void _vanish_interior(point_t pos, dun_grid_ptr grid)
{
    feat_ptr f_ptr = &f_info[grid->feat];
    mon_ptr  mon = mon_at(pos);

    grid->info &= ~(CAVE_ROOM | CAVE_ICKY);
    if (mon) mon_tim_remove(mon, MT_SLEEP);
    if (have_flag(f_ptr->flags, FF_HURT_DISI)) cave_alter_feat(pos.y, pos.x, FF_HURT_DISI);
}
static void _vanish_boundary(point_t pos, dun_grid_ptr grid)
{
    feat_ptr f_ptr = &f_info[grid->mimic];

    grid->info &= ~(CAVE_ROOM | CAVE_ICKY);

    /* Set boundary mimic if needed */
    if (grid->mimic && have_flag(f_ptr->flags, FF_HURT_DISI))
    {
        grid->mimic = feat_state(grid->mimic, FF_HURT_DISI);

        /* Check for change to boring grid */
        if (!have_flag(f_info[grid->mimic].flags, FF_REMEMBER)) grid->info &= ~(CAVE_MARK);
    }
}
static bool vanish_dungeon(void)
{
    /* Prevent vasishing of quest levels and town */
    if (!plr_in_dungeon())
        return FALSE;

    dun_iter_interior(cave, _vanish_interior);
    dun_iter_boundary(cave, _vanish_boundary);

    p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);
    p_ptr->update |= (PU_MONSTERS);
    p_ptr->redraw |= (PR_MAP);
    p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

    return TRUE;
}


void call_the_(void)
{
    int i;
    cave_type *c_ptr;
    bool do_call = TRUE;

    for (i = 0; i < 9; i++)
    {
        c_ptr = cave_at_xy(p_ptr->pos.x + ddx_ddd[i], p_ptr->pos.y + ddy_ddd[i]);

        if (!cave_have_flag_grid(c_ptr, FF_PROJECT))
        {
            if (!c_ptr->mimic || !have_flag(f_info[c_ptr->mimic].flags, FF_PROJECT) ||
                !permanent_wall(&f_info[c_ptr->feat]))
            {
                do_call = FALSE;
                break;
            }
        }
    }

    if (do_call)
    {
        for (i = 1; i < 10; i++)
        {
            if (i - 5) fire_ball(GF_ROCKET, i, 175, 2);
        }

        for (i = 1; i < 10; i++)
        {
            if (i - 5) fire_ball(GF_MANA, i, 175, 3);
        }

        for (i = 1; i < 10; i++)
        {
            if (i - 5) fire_ball(GF_NUKE, i, 175, 4);
        }
    }

    /* Prevent destruction of quest levels and town */
    else if (!plr_in_dungeon())
    {
        msg_print("The ground trembles.");
    }

    else
    {
        msg_format("You %s the %s too close to a wall!",
            ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
            ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell"));
        msg_print("There is a loud explosion!");

        if (one_in_(666))
        {
            if (!vanish_dungeon()) msg_print("The dungeon silences a moment.");
        }
        else
        {
            if (destroy_area(p_ptr->pos.y, p_ptr->pos.x, 15 + p_ptr->lev + randint0(11), 8 * p_ptr->lev))
                msg_print("The dungeon collapses...");

            else
                msg_print("The dungeon trembles.");
        }

        take_hit(DAMAGE_NOESCAPE, 100 + randint1(150), "a suicidal Call the Void");
    }
}


/*
 * Fetch an item (teleport it right underneath the caster)
 */
void fetch(int dir, int wgt, bool require_los)
{
    point_t tgt;
    obj_ptr obj;
    char    o_name[MAX_NLEN];

    /* Check to see if an object is already there */
    if (obj_at(p_ptr->pos))
    {
        msg_print("You can't fetch when you're already standing on something.");
        return;
    }

    /* Use a target */
    if (dir == 5 && target_okay())
    {
        tgt = point_create(target_col, target_row);

        if (point_distance(p_ptr->pos, tgt) > MAX_RANGE)
        {
            msg_print("You can't fetch something that far away!");
            return;
        }
        if (!obj_at(tgt))
        {
            msg_print("There is no object at this place.");
            return;
        }
        if (cave_at(tgt)->info & CAVE_ICKY)
        {
            msg_print("The item slips from your control.");
            return;
        }
        if (require_los)
        {
            if (!player_has_los_bold(tgt.y, tgt.x))
            {
                msg_print("You have no direct line of sight to that location.");
                return;
            }
            else if (!projectable(p_ptr->pos.y, p_ptr->pos.x, tgt.y, tgt.x))
            {
                msg_print("You have no direct line of sight to that location.");
                return;
            }
        }
    }
    else
    {
        tgt = p_ptr->pos;
        do
        {
            tgt = point_step(tgt, dir);
            if (!dun_pos_interior(cave, tgt)) return;
            if (point_distance(p_ptr->pos, tgt) > MAX_RANGE || !cave_have_flag_at(tgt, FF_PROJECT)) return;
        }
        while (!obj_at(tgt));
    }

    obj = obj_at(tgt);
    assert(obj);

    if (obj->weight > wgt)
    {
        msg_print("The object is too heavy.");
        return;
    }

    object_desc(o_name, obj, OD_NAME_ONLY);
    msg_format("%^s flies through the air to your feet.", o_name);
    dun_fetch_obj(cave, tgt, p_ptr->pos);
    note_pos(p_ptr->pos);
    p_ptr->redraw |= PR_MAP;
}


void alter_reality(void)
{
    if (cave->dun_type_id == D_SURFACE)
    {
        msg_print("Nothing happens.");
        return;
    }

    msg_print("The world around you changes!");
    dun_mgr_wizard_jump(cave->dun_type_id, cave->dun_lvl);
}


/*
 * Leave a "glyph of warding" which prevents monster movement
 */
bool warding_glyph(void)
{
    /* XXX XXX XXX */
    if (!cave_clean_bold(p_ptr->pos.y, p_ptr->pos.x))
    {
        msg_print("The object resists the spell.");

        return FALSE;
    }

    /* Create a glyph */
    cave_at(p_ptr->pos)->info |= CAVE_OBJECT;
    cave_at(p_ptr->pos)->mimic = feat_glyph;

    note_pos(p_ptr->pos);
    lite_pos(p_ptr->pos);

    return TRUE;
}


bool set_trap(int y, int x, int feature)
{
    if (!in_bounds(y, x)) return FALSE;

    if (!cave_clean_bold(y, x))
    {
        if (y == p_ptr->pos.y && x == p_ptr->pos.x) /* Hack: I only want the message sometimes ... */
            msg_print("The object resists the spell.");
        return FALSE;
    }

    cave_at_xy(x, y)->info |= CAVE_OBJECT;
    cave_at_xy(x, y)->mimic = feature;
    note_spot(y, x);
    lite_spot(y, x);

    return TRUE;
}

bool explosive_rune(void)
{
    return set_trap(p_ptr->pos.y, p_ptr->pos.x, feat_explosive_rune);
}


/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
static void _identify_obj(obj_ptr obj)
{
    identify_item(obj);
    autopick_alter_obj(obj, FALSE);
}

void identify_pack(void)
{
    pack_for_each(_identify_obj);
    equip_for_each(_identify_obj);
    quiver_for_each(_identify_obj);
}


/*
 * Used by the "enchant" function (chance of failure)
 * (modified for Zangband, we need better stuff there...) -- TY
 */
static int enchant_table[16] =
{
      5,  10,  50, 100, 200,
    300, 400, 500, 650, 800,
    950, 987, 993, 995, 998,
    1000
};


/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */

static int remove_curse_aux(int all)
{
    int slot;
    int ct = 0;

    for (slot = equip_find_first(obj_is_cursed);
            slot;
            slot = equip_find_next(obj_is_cursed, slot))
    {
        object_type *o_ptr = equip_obj(slot);

        if (!all && (o_ptr->curse_flags & OFC_HEAVY_CURSE)) continue;

        if (o_ptr->curse_flags & OFC_PERMA_CURSE)
        {
            o_ptr->curse_flags &= (OFC_CURSED | OFC_HEAVY_CURSE | OFC_PERMA_CURSE);
            o_ptr->known_curse_flags &= (OFC_CURSED | OFC_HEAVY_CURSE | OFC_PERMA_CURSE);
            continue;
        }

        o_ptr->curse_flags = 0;
        o_ptr->known_curse_flags = 0; /* Forget lore in preparation for next cursing */
        o_ptr->ident  |= IDENT_SENSE;
        o_ptr->feeling = FEEL_NONE;
        p_ptr->update |= PU_BONUS;
        p_ptr->window |= PW_EQUIP;
        p_ptr->redraw |= PR_EFFECTS;
        ct++;
    }

    return ct;
}


/*
 * Remove most curses
 */
bool remove_curse(void)
{
    return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
    return (remove_curse_aux(TRUE));
}


/*
 * Turns an object into gold, gain some of its value in a shop
 */
static bool _alchemy_aux(obj_ptr obj, bool force)
{
    char o_name[MAX_NLEN];
    char out_val[MAX_NLEN+40];
    int price;

    object_desc(o_name, obj, OD_COLOR_CODED);

    /* Verify unless quantity given */
    if (!force)
    {
        if (confirm_destroy || (obj_value(obj) > 0))
        {
            sprintf(out_val, "Really turn %s to gold? ", o_name);
            if (!get_check(out_val)) return FALSE;
        }
    }

    /* Artifacts cannot be destroyed */
    if (!can_player_destroy_object(obj))
    {
        msg_format("You fail to turn %s to gold!", o_name);
        return FALSE;
    }

    price = obj_value_real(obj);

    if (price <= 0)
    {
        msg_format("You turn %s to fool's gold.", o_name);
    }
    else
    {
        price /= 3;
        if (price > 30000) price = 30000;
        price *= obj->number;

        msg_format("You turn %s to %d coins worth of gold.", o_name, price);

        p_ptr->au += price;
        stats_on_gold_selling(price); /* ? */

        p_ptr->redraw |= (PR_GOLD);
        if (prace_is_(RACE_MON_LEPRECHAUN))
            p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

    }
    return TRUE;
}

bool alchemy(void)
{
    obj_prompt_t prompt = {0};
    int amt = 1;
    bool force = FALSE;

    /* Hack -- force destruction */
    if (command_arg > 0) force = TRUE;

    prompt.prompt = "Turn which item to gold?";
    prompt.error = "You have nothing to turn to gold.";
    prompt.filter = obj_exists; 
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    if (prompt.obj->number > 1)
    {
        amt = get_quantity(NULL, prompt.obj->number);
        if (amt <= 0) return FALSE;
    }

    if (amt < prompt.obj->number)
    {
        obj_t copy = *prompt.obj;
        copy.number = amt;
        if (_alchemy_aux(&copy, force))
        {
            prompt.obj->number -= amt;
            obj_release(prompt.obj, 0);
            return TRUE;
        }
    }
    if (_alchemy_aux(prompt.obj, force))
    {
        prompt.obj->number = 0;
        obj_release(prompt.obj, 0);
        return TRUE;
    }
    return FALSE;
}


/*
 * Break the curse of an item
 */
void break_curse(object_type *o_ptr)
{
    if (obj_is_cursed(o_ptr) && !(o_ptr->curse_flags & OFC_PERMA_CURSE) && !(o_ptr->curse_flags & OFC_HEAVY_CURSE) && (randint0(100) < 25))
    {
        msg_print("The curse is broken!");

        o_ptr->curse_flags = 0L;

        o_ptr->ident |= (IDENT_SENSE);

        o_ptr->feeling = FEEL_NONE;
    }
}


/*
 * Enchants a plus onto an item. -RAK-
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting. Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item. -CFT-
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time. Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
    int     i, chance, prob;
    bool    res = FALSE;
    bool    a = obj_is_art(o_ptr);
    bool    force = BOOL(eflag & ENCH_FORCE);
    int     minor_limit = 2 + p_ptr->lev/5; /* This matches the town service ... */

    /* Large piles resist enchantment */
    prob = o_ptr->number * 100;

    /* Some objects cannot be enchanted */
    if (obj_has_flag(o_ptr, OF_NO_ENCHANT))
        return FALSE;


    /* Missiles are easy to enchant */
    if ((o_ptr->tval == TV_BOLT) ||
        (o_ptr->tval == TV_ARROW) ||
        (o_ptr->tval == TV_SHOT))
    {
        prob = prob / 20;
    }

    /* Try "n" times */
    for (i = 0; i < n; i++)
    {
        /* Hack -- Roll for pile resistance */
        if (!force && randint0(prob) >= 100) continue;

        /* Enchant to hit */
        if (eflag & ENCH_TOHIT)
        {
            int idx = o_ptr->to_h;
            if (eflag & ENCH_PSI_HACK)
            {
                idx -= 2*(psion_enchant_power() - 1);
            }

            if (idx < 0) chance = 0;
            else if (idx > 15) chance = 1000;
            else chance = enchant_table[idx];

            if ((eflag & ENCH_MINOR_HACK) && idx >= minor_limit)
                chance = 1000;

            if (force || ((randint1(1000) > chance) && (!a || (randint0(100) < 50))))
            {
                o_ptr->to_h++;
                res = TRUE;

                /* only when you get it above -1 -CFT */
                if (o_ptr->to_h >= 0)
                    break_curse(o_ptr);
            }
        }

        /* Enchant to damage */
        if (eflag & ENCH_TODAM)
        {
            int idx = o_ptr->to_d;
            if (eflag & ENCH_PSI_HACK)
            {
                idx -= 2*(psion_enchant_power() - 1);
            }

            if (idx < 0) chance = 0;
            else if (idx > 15) chance = 1000;
            else chance = enchant_table[idx];

            if ((eflag & ENCH_MINOR_HACK) && idx >= minor_limit)
                chance = 1000;

            if (force || ((randint1(1000) > chance) && (!a || (randint0(100) < 50))))
            {
                o_ptr->to_d++;
                res = TRUE;

                /* only when you get it above -1 -CFT */
                if (o_ptr->to_d >= 0)
                    break_curse(o_ptr);
            }
        }

        /* Enchant to armor class */
        if (eflag & ENCH_TOAC)
        {
            int idx = o_ptr->to_a;

            if (eflag & ENCH_PSI_HACK)
            {
                idx -= 2*(psion_enchant_power() - 1);
            }

            if (idx < 0) chance = 0;
            else if (idx > 15) chance = 1000;
            else chance = enchant_table[idx];

            if ((eflag & ENCH_MINOR_HACK) && idx >= minor_limit)
                chance = 1000;

            if (force || ((randint1(1000) > chance) && (!a || (randint0(100) < 50))))
            {
                o_ptr->to_a++;
                res = TRUE;

                /* only when you get it above -1 -CFT */
                if (o_ptr->to_a >= 0)
                    break_curse(o_ptr);
            }
        }
    }

    if (!res) return FALSE;

    gear_notice_enchant(o_ptr);
    return TRUE;
}



/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
    obj_prompt_t prompt = {0};
    bool         okay = FALSE;
    char         o_name[MAX_NLEN];

    prompt.prompt = "Enchant which item?";
    prompt.error = "You have nothing to enchant.";
    prompt.filter = num_ac ? obj_is_armor : object_allow_enchant_weapon;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

    msg_format("%s %s glow%s brightly!",
           (prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The",
           o_name,
           (prompt.obj->number > 1) ? "" : "s");


    if (enchant(prompt.obj, num_hit, ENCH_TOHIT)) okay = TRUE;
    if (enchant(prompt.obj, num_dam, ENCH_TODAM)) okay = TRUE;
    if (enchant(prompt.obj, num_ac, ENCH_TOAC)) okay = TRUE;

    if (!okay)
    {
        if (flush_failure) flush();
        msg_print("The enchantment failed.");

        if (one_in_(3) && virtue_current(VIRTUE_ENCHANTMENT) < 100)
            virtue_add(VIRTUE_ENCHANTMENT, -1);
    }
    else
        virtue_add(VIRTUE_ENCHANTMENT, 1);

    return TRUE;
}


/*
 * Check if an object is nameless weapon or armour
 */
bool item_tester_hook_nameless_weapon_armour(object_type *o_ptr)
{
    if ( !object_is_weapon_armour_ammo(o_ptr)
      && !(o_ptr->tval == TV_LITE && o_ptr->sval == SV_LITE_FEANOR)
      && !(o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET) /* Testing ... */
      && !(prace_is_(RACE_SNOTLING) && object_is_mushroom(o_ptr)) )
    {
        return FALSE;
    }

    /* Require nameless object if the object is well known
     * XXX Egos are now OK. */
    if (obj_is_known(o_ptr) && obj_is_art(o_ptr))
        return FALSE;

    if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_POISON_NEEDLE) return FALSE;

    return TRUE;
}

bool artifact_scroll(void)
{
    obj_prompt_t prompt = {0};
    bool         okay = FALSE;
    char         o_name[MAX_NLEN];

    prompt.prompt = "Enchant which item?";
    prompt.error = "You have nothing to enchant.";
    prompt.filter = item_tester_hook_nameless_weapon_armour;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

    /* Describe */
    msg_format("The %s radiate%s a blinding light!", o_name,
          ((prompt.obj->number > 1) ? "" : "s"));

    if (obj_is_art(prompt.obj))
    {
        msg_format("The %s %s already %s!",
            o_name, ((prompt.obj->number > 1) ? "are" : "is"),
            ((prompt.obj->number > 1) ? "artifacts" : "an artifact"));
    }

    else if (prompt.obj->xtra3)
    {
        msg_format("The %s %s already %s!",
            o_name, ((prompt.obj->number > 1) ? "are" : "is"),
            ((prompt.obj->number > 1) ? "customized items" : "a customized item"));
    }

    else if (have_flag(prompt.obj->flags, OF_NO_REMOVE))
    {
        msg_print("You are quite special already!");
    }
    else
    {
        if (prompt.obj->number > 1)
        {
            msg_print("Not enough enough energy to enchant more than one object!");
            msg_format("%d of your %s %s destroyed!",(prompt.obj->number)-1, o_name, (prompt.obj->number>2?"were":"was"));
            prompt.obj->number = 1;
        }

        if (object_is_mushroom(prompt.obj)) /* Hack for Snotlings ... */
        {
            prompt.obj->art_name = quark_add("(Eternal Mushroom)");
            okay = TRUE;
        }
        else
        {
            int lvl = plr_prorata_level(75);
            if (obj_is_ego(prompt.obj))
                art_create_ego(prompt.obj, lvl, AM_CRAFTING);
            else
                art_create_random(prompt.obj, lvl, AM_CRAFTING);
            art_remember_name(quark_str(prompt.obj->art_name));
            okay = TRUE;
        }
    }

    /* Failure */
    if (!okay)
    {
        /* Flush */
        if (flush_failure) flush();

        /* Message */
        msg_print("The enchantment failed.");

        if (one_in_(3)) virtue_add(VIRTUE_ENCHANTMENT, -1);
    }
    else
        virtue_add(VIRTUE_ENCHANTMENT, 1);

    android_calc_exp();

    /* Something happened */
    return (TRUE);
}


/*
 * Identify an object
 */
bool identify_item(object_type *o_ptr)
{
    bool old_known = FALSE;

    if (obj_is_identified(o_ptr))
        old_known = TRUE;

    if (!spoiler_hack && !old_known)
    {
        if (obj_is_art(o_ptr) || one_in_(5))
            virtue_add(VIRTUE_KNOWLEDGE, 1);
    }

    obj_identify(o_ptr);
    stats_on_identify(o_ptr);
    o_ptr->marked |= OM_TOUCHED;
    gear_notice_id(o_ptr);
    return old_known;
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
static object_p _hack_obj_p = NULL;
static bool item_tester_hook_identify(object_type *o_ptr)
{
    if ( !obj_is_known(o_ptr)
      && (!_hack_obj_p || _hack_obj_p(o_ptr)) )
    {
        return TRUE;
    }
    return FALSE;
}
bool ident_spell(object_p p)
{
    obj_prompt_t prompt = {0};
    char         o_name[MAX_NLEN];
    bool         old_known;

    _hack_obj_p = p;
    prompt.prompt = "Identify which item?";
    prompt.error = "All items are identified.";
    prompt.filter = item_tester_hook_identify;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    old_known = identify_item(prompt.obj);

    object_desc(o_name, prompt.obj, OD_COLOR_CODED);

    msg_format("You identify %s.", o_name);
    autopick_alter_obj(prompt.obj, destroy_identify && !old_known);
    obj_release(prompt.obj, OBJ_RELEASE_QUIET | OBJ_RELEASE_ID);
    return TRUE;
}


/*
 * Mundanify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was mundanified, else FALSE.
 */
bool mundane_spell(bool only_equip)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Use which item?";
    prompt.error = "You have nothing you can use.";
    if (only_equip)
        prompt.filter = object_is_weapon_armour_ammo;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    if (prompt.obj->name1 == ART_HAND_OF_VECNA || prompt.obj->name1 == ART_EYE_OF_VECNA)
    {
        msg_print("There is no effect.");
        return TRUE;
    }

    if (have_flag(prompt.obj->flags, OF_NO_REMOVE))
    {
        msg_print("Failed! You will never be average!");
        return TRUE;
    }

    msg_print("There is a bright flash of light!");
    {
        obj_loc_t loc = prompt.obj->loc;
        byte marked = prompt.obj->marked;
        u16b inscription = prompt.obj->inscription;
        int  number = prompt.obj->number;
        obj_ptr next = prompt.obj->next;

        /* Wipe it clean ... note this erases info that must
         * not be erased. Thus, all the code to remember and restore ... sigh. */
        object_prep(prompt.obj, prompt.obj->k_idx);

        prompt.obj->loc = loc;
        prompt.obj->marked = marked;
        prompt.obj->inscription = inscription;
        prompt.obj->number = number;
        prompt.obj->next = next;
    }
    p_ptr->update |= PU_BONUS;
    android_calc_exp();

    return TRUE;
}



static bool item_tester_hook_identify_fully(object_type *o_ptr)
{
    if (_hack_obj_p && !_hack_obj_p(o_ptr)) return FALSE;
    if (!obj_is_known(o_ptr) || !obj_is_identified_fully(o_ptr))
    {
        return TRUE;
    }
    if ( obj_is_identified_fully(o_ptr)
      && o_ptr->curse_flags != o_ptr->known_curse_flags )
    {
        return TRUE;
    }
    return FALSE;
}

/*
 * Fully "identify" an object in the inventory  -BEN-
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(object_p p)
{
    obj_prompt_t prompt = {0};
    bool         old_known;

    _hack_obj_p = p;
    prompt.prompt = "*Identify* which item?";
    prompt.error = "You have nothing to *identify*.";
    prompt.filter = item_tester_hook_identify_fully;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    old_known = identify_item(prompt.obj); /* For the stat tracking and old_known ... */
    obj_identify_fully(prompt.obj);

    if ( p_ptr->prace == RACE_MON_POSSESSOR
      && prompt.obj->tval == TV_CORPSE
      && prompt.obj->sval == SV_CORPSE )
    {
        if (!(mon_race_lookup(prompt.obj->pval)->r_xtra1 & MR1_POSSESSOR))
            msg_print("You learn more about this body.");
        lore_do_probe(prompt.obj->pval);
    }

    obj_display(prompt.obj);
    autopick_alter_obj(prompt.obj, destroy_identify && !old_known);
    obj_release(prompt.obj, OBJ_RELEASE_QUIET | OBJ_RELEASE_ID);
    return TRUE;
}

/* Recharging
 * Move mana from either the player or another device into a target device.
 * The source device is destroyed (but not the player :) but the target never is.
 * The effect may fail, however, wasting either the source device or the player's sp.
 */

static object_type *_obj_recharge_src_ptr = NULL;
static bool _obj_recharge_dest(object_type *o_ptr)
{
    /* dest must be different from source */
    if (o_ptr == _obj_recharge_src_ptr)
        return FALSE;

    switch (o_ptr->tval)
    {
    case TV_WAND: case TV_ROD: case TV_STAFF:
        if (device_sp(o_ptr) < device_max_sp(o_ptr))
            return TRUE;
        break;
    }
    return FALSE;
}

static bool _obj_recharge_src(object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
    case TV_WAND: case TV_ROD: case TV_STAFF:
        if (device_sp(o_ptr) > 0)
            return TRUE;
        break;
    }
    return FALSE;
}

static void _recharge_aux(object_type *o_ptr, int amt, int power)
{
    int fail_odds = 0;
    int lev = o_ptr->activation.difficulty;
    int div = 15;

    if (devicemaster_is_speciality(o_ptr))
        div = 8;

    if (power > lev/2)
        fail_odds = (power - lev/2) / div;

    if (one_in_(fail_odds))
    {
        /* Do nothing for now. My experience is that players get too conservative
         * using recharge in the dungeon if there is any chance of failure at all.
         * Remember, you're lucky if you find just one wand of rockets all game long!
         * I plan on removing the town recharging service to compensate for this
         * generosity, though. */
         msg_print("Failed!");
         return;
    }

    device_increase_sp(o_ptr, amt);
}

bool recharge_from_player(int power)
{
    obj_prompt_t prompt = {0};
    int          amt, max;
    cptr         resource = NULL;

    amt = power;
    if (p_ptr->prace == RACE_MON_LEPRECHAUN)
    {
        if (amt > p_ptr->au / 100)
            amt = p_ptr->au / 100;
        resource = "money";
    }
    else
    {
        if (amt > p_ptr->csp)
            amt = p_ptr->csp;
        resource = "mana";
    }

    if (amt == 0) 
    {
        msg_format("Failed! You don't have enough %s!", resource);
        return FALSE;
    }

    /* Get destination device */
    _obj_recharge_src_ptr = NULL;
    prompt.prompt = "Recharge which item?";
    prompt.error = "You have nothing to recharge.";
    prompt.filter = _obj_recharge_dest;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    max = device_max_sp(prompt.obj) - device_sp(prompt.obj);
    if (amt > max)
        amt = max;

    if (p_ptr->prace == RACE_MON_LEPRECHAUN)
    {
        p_ptr->au -= amt * 100;
        stats_on_gold_services(amt * 100); /* ? */
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);
    }
    else
        sp_player(-amt);

    _recharge_aux(prompt.obj, amt, power);
    obj_release(prompt.obj, 0);
    return TRUE;
}

static obj_ptr _get_recharge_src(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Pick a source device?";
    prompt.error = "You need a source device to power the recharging.";
    prompt.filter = _obj_recharge_src;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    return prompt.obj;
}

static obj_ptr _get_recharge_dest(obj_ptr src)
{
    obj_prompt_t prompt = {0};

    _obj_recharge_src_ptr = src;

    prompt.prompt = "Recharge which item?";
    prompt.error = "You have nothing to recharge.";
    prompt.filter = _obj_recharge_dest;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    return prompt.obj;
}

bool recharge_from_device(int power)
{
    int          amt, max;
    bool         destroy = TRUE;
    object_type *src_ptr, *dest_ptr;

    /* Get source device */
    src_ptr = _get_recharge_src();
    if (!src_ptr) return FALSE;

    /* Get destination device */
    dest_ptr = _get_recharge_dest(src_ptr);
    if (!dest_ptr) return FALSE;

    amt = device_sp(src_ptr);
    max = device_max_sp(dest_ptr) - device_sp(dest_ptr);
    if (max > power)
        max = power;

    if (amt > max)
        amt = max;

    if (!one_in_(3) || devicemaster_is_speciality(src_ptr))
    {
        device_decrease_sp(src_ptr, amt);
        destroy = FALSE;
    }

    _recharge_aux(dest_ptr, amt, power);

    if (destroy)
    {
        if (obj_is_std_art(src_ptr))
            device_decrease_sp(src_ptr, amt);
        else
        {
            char name[MAX_NLEN];

            object_desc(name, src_ptr, OD_OMIT_PREFIX | OD_COLOR_CODED);
            msg_format("Recharging consumes your %s!", name);

            src_ptr->number = 0;
            obj_release(src_ptr, OBJ_RELEASE_QUIET);
        }
    }

    return TRUE;
}

/*
 * Bless a weapon
 */
bool bless_weapon(void)
{
    obj_prompt_t prompt = {0};
    char         o_name[MAX_NLEN];

    prompt.prompt = "Bless which weapon?";
    prompt.error = "You have weapon to bless.";
    prompt.filter = obj_is_weapon;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

    if (obj_is_cursed(prompt.obj))
    {
        if (((prompt.obj->curse_flags & OFC_HEAVY_CURSE) && (randint1(100) < 33)) ||
            (prompt.obj->curse_flags & OFC_PERMA_CURSE))
        {
            msg_format("The black aura on %s %s disrupts the blessing!",
                (prompt.obj->loc.where != INV_FLOOR) ? "your" : "the", o_name);

            return TRUE;
        }

        msg_format("A malignant aura leaves %s %s.",
            (prompt.obj->loc.where != INV_FLOOR) ? "your" : "the", o_name);

        /* Uncurse it */
        prompt.obj->curse_flags = 0;

        /* Hack -- Assume felt */
        prompt.obj->ident |= (IDENT_SENSE);

        /* Take note */
        prompt.obj->feeling = FEEL_NONE;

        /* Recalculate the bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Window stuff */
        p_ptr->window |= (PW_EQUIP);
    }

    /*
     * Next, we try to bless it. Artifacts have a 1/3 chance of
     * being blessed, otherwise, the operation simply disenchants
     * them, godly power negating the magic. Ok, the explanation
     * is silly, but otherwise priests would always bless every
     * artifact weapon they find. Ego weapons and normal weapons
     * can be blessed automatically.
     */
    if (obj_has_flag(prompt.obj, OF_BLESSED))
    {
        msg_format("%s %s %s blessed already.",
            ((prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The"), o_name,
            ((prompt.obj->number > 1) ? "were" : "was"));

        return TRUE;
    }

    if (!(obj_is_art(prompt.obj) || obj_is_ego(prompt.obj)) || one_in_(3))
    {
        /* Describe */
        msg_format("%s %s shine%s!",
            ((prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The"), o_name,
            ((prompt.obj->number > 1) ? "" : "s"));

        add_flag(prompt.obj->flags, OF_BLESSED);
        add_flag(prompt.obj->known_flags, OF_BLESSED);
        if (object_is_nameless(prompt.obj))
            prompt.obj->discount = 99;
    }
    else
    {
        bool dis_happened = FALSE;

        msg_print("The weapon resists your blessing!");


        /* Disenchant tohit */
        if (prompt.obj->to_h > 0)
        {
            prompt.obj->to_h--;
            dis_happened = TRUE;
        }

        if ((prompt.obj->to_h > 5) && (randint0(100) < 33)) prompt.obj->to_h--;

        /* Disenchant todam */
        if (prompt.obj->to_d > 0)
        {
            prompt.obj->to_d--;
            dis_happened = TRUE;
        }

        if ((prompt.obj->to_d > 5) && (randint0(100) < 33)) prompt.obj->to_d--;

        /* Disenchant toac */
        if (prompt.obj->to_a > 0)
        {
            prompt.obj->to_a--;
            dis_happened = TRUE;
        }

        if ((prompt.obj->to_a > 5) && (randint0(100) < 33)) prompt.obj->to_a--;

        if (dis_happened)
        {
            msg_print("There is a static feeling in the air...");

            msg_format("%s %s %s disenchanted!",
                ((prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The"), o_name,
                ((prompt.obj->number > 1) ? "were" : "was"));

        }
    }

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Window stuff */
    p_ptr->window |= PW_EQUIP;

    android_calc_exp();

    return TRUE;
}


/*
 * polish shield
 */
bool polish_shield(void)
{
    obj_prompt_t prompt = {0};
    char         o_name[MAX_NLEN];

    prompt.prompt = "Polish which shield?";
    prompt.error = "You have no shield to polish.";
    prompt.filter = obj_is_shield; 
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

    if (!obj_is_art(prompt.obj) && !obj_is_ego(prompt.obj) &&
        !obj_is_cursed(prompt.obj) && (prompt.obj->sval != SV_MIRROR_SHIELD))
    {
        msg_format("%s %s shine%s!",
            ((prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The"), o_name,
            ((prompt.obj->number > 1) ? "" : "s"));
        prompt.obj->name2 = EGO_SHIELD_REFLECTION;
        enchant(prompt.obj, randint0(3) + 4, ENCH_TOAC);

        prompt.obj->discount = 99;
        virtue_add(VIRTUE_ENCHANTMENT, 2);
    }
    else
    {
        if (flush_failure) flush();
        msg_print("Failed.");
        virtue_add(VIRTUE_ENCHANTMENT, -2);
    }
    android_calc_exp();
    return TRUE;
}


/*
 * Potions "smash open" and cause an area effect when
 * (1) they are shattered while in the player's inventory,
 * due to cold (etc) attacks;
 * (2) they are thrown at a monster, or obstacle;
 * (3) they are shattered by a "cold ball" or other such spell
 * while lying on the floor.
 *
 * Arguments:
 *    who   ---  who caused the potion to shatter (0=player)
 *          potions that smash on the floor are assumed to
 *          be caused by no-one (who = 1), as are those that
 *          shatter inside the player inventory.
 *          (Not anymore -- I changed this; TY)
 *    y, x  --- coordinates of the potion (or player if
 *          the potion was in her inventory);
 *    o_ptr --- pointer to the potion object.
 */
bool potion_smash_effect(int who, int y, int x, int k_idx)
{
    int     radius = 2;
    int     dt = 0;
    int     dam = 0;
    bool    angry = FALSE;

    object_kind *k_ptr = &k_info[k_idx];

    switch (k_ptr->sval)
    {
        case SV_POTION_SALT_WATER:
        case SV_POTION_SLIME_MOLD:
        case SV_POTION_LOSE_MEMORIES:
        case SV_POTION_DEC_STR:
        case SV_POTION_DEC_INT:
        case SV_POTION_DEC_WIS:
        case SV_POTION_DEC_DEX:
        case SV_POTION_DEC_CON:
        case SV_POTION_DEC_CHR:
        case SV_POTION_WATER:   /* perhaps a 'water' attack? */
        case SV_POTION_APPLE_JUICE:
            return TRUE;

        case SV_POTION_INFRAVISION:
        case SV_POTION_DETECT_INVIS:
        case SV_POTION_SLOW_POISON:
        case SV_POTION_CURE_POISON:
        case SV_POTION_BOLDNESS:
        case SV_POTION_RESIST_HEAT:
        case SV_POTION_RESIST_COLD:
        case SV_POTION_HEROISM:
        case SV_POTION_BERSERK_STRENGTH:
        case SV_POTION_RES_STR:
        case SV_POTION_RES_INT:
        case SV_POTION_RES_WIS:
        case SV_POTION_RES_DEX:
        case SV_POTION_RES_CON:
        case SV_POTION_RES_CHR:
        case SV_POTION_INC_STR:
        case SV_POTION_INC_INT:
        case SV_POTION_INC_WIS:
        case SV_POTION_INC_DEX:
        case SV_POTION_INC_CON:
        case SV_POTION_INC_CHR:
        case SV_POTION_AUGMENTATION:
        case SV_POTION_ENLIGHTENMENT:
        case SV_POTION_STAR_ENLIGHTENMENT:
        case SV_POTION_SELF_KNOWLEDGE:
        case SV_POTION_EXPERIENCE:
        case SV_POTION_RESISTANCE:
        case SV_POTION_INVULNERABILITY:
        case SV_POTION_NEW_LIFE:
            /* All of the above potions have no effect when shattered */
            return FALSE;
        case SV_POTION_SLOWNESS:
            dt = GF_OLD_SLOW;
            dam = 5;
            angry = TRUE;
            break;
        case SV_POTION_POISON:
            dt = GF_POIS;
            dam = 3;
            angry = TRUE;
            break;
        case SV_POTION_BLINDNESS:
            dt = GF_DARK;
            angry = TRUE;
            break;
        case SV_POTION_CONFUSION: /* Booze */
            dt = GF_OLD_CONF;
            angry = TRUE;
            break;
        case SV_POTION_SLEEP:
            dt = GF_OLD_SLEEP;
            angry = TRUE;
            break;
        case SV_POTION_RUINATION:
        case SV_POTION_DETONATIONS:
            dt = GF_SHARDS;
            dam = damroll(25, 25);
            angry = TRUE;
            break;
        case SV_POTION_DEATH:
            dt = GF_DEATH_RAY;    /* !! */
            dam = k_ptr->level * 10;
            angry = TRUE;
            radius = 1;
            break;
        case SV_POTION_SPEED:
            dt = GF_OLD_SPEED;
            break;
        case SV_POTION_CURE_LIGHT:
            dt = GF_OLD_HEAL;
            dam = damroll(2, 3);
            break;
        case SV_POTION_CURE_SERIOUS:
            dt = GF_OLD_HEAL;
            dam = damroll(4, 3);
            break;
        case SV_POTION_CURE_CRITICAL:
        case SV_POTION_CURING:
            dt = GF_OLD_HEAL;
            dam = damroll(6, 3);
            break;
        case SV_POTION_HEALING:
            dt = GF_OLD_HEAL;
            dam = damroll(10, 10);
            break;
        case SV_POTION_RESTORE_EXP:
            dt = GF_STAR_HEAL;
            dam = 0;
            radius = 1;
            break;
        case SV_POTION_LIFE:
            dt = GF_STAR_HEAL;
            dam = damroll(50, 50);
            radius = 1;
            break;
        case SV_POTION_STAR_HEALING:
            dt = GF_OLD_HEAL;
            dam = damroll(50, 50);
            radius = 1;
            break;
        case SV_POTION_RESTORE_MANA:   /* MANA */
            dt = GF_MANA;
            dam = damroll(10, 10);
            radius = 1;
            break;
        default:
            /* Do nothing */  ;
    }

    (void)project(who, radius, y, x, dam, dt,
        (PROJECT_JUMP | PROJECT_ITEM | PROJECT_KILL));

    /* XXX  those potions that explode need to become "known" */
    return angry;
}


/*
 * Returns experience of a spell
 */
s16b experience_of_spell(int spell, int use_realm)
{
    if (p_ptr->pclass == CLASS_SORCERER) return SPELL_EXP_MASTER;
    else if (p_ptr->pclass == CLASS_RED_MAGE) return SPELL_EXP_SKILLED;
    else if (use_realm == p_ptr->realm1) return p_ptr->spell_exp[spell];
    else if (use_realm == p_ptr->realm2) return p_ptr->spell_exp[spell + 32];
    else return 0;
}

/* Multiple sources of DEC_MANA now matter. Code is shared with skillmaster,
 * mon_spell, and bookless spellcasting systems */
int dec_mana_cost(int dec_mana)
{
    int pct[5] = { 100, 75, 65, 57, 50 };
    int i = MIN(4, MAX(0, dec_mana));
    return pct[i];
}

int dec_mana_fail1(int dec_mana, int easy_spell)
{
    if (!dec_mana)
    {
        if (easy_spell) return -2;
        return 0;
    }
    else
    {
        int i = dec_mana + easy_spell;
        return -2 - i;
    }
}

int dec_mana_fail2(int dec_mana, int easy_spell)
{
    if (dec_mana > 2) return -2;
    if (dec_mana) return -1;
    return 0;
}

/*
 * Modify mana consumption rate using spell exp and p_ptr->dec_mana
 */
int mod_need_mana(int need_mana, int spell, int realm)
{
    int dec_mana = p_ptr->dec_mana;

    if (p_ptr->easy_realm1 && realm == p_ptr->easy_realm1)
        dec_mana++;

    /* Realm magic */
    if (realm > REALM_NONE && realm <= MAX_REALM)
    {
        need_mana = need_mana * (2400 + SPELL_EXP_EXPERT - experience_of_spell(spell, realm)) + 2399;
        need_mana *= dec_mana_cost(dec_mana);
        need_mana /= 2400 * 100;
    }

    /* Non-realm magic */
    else
    {
        need_mana = (need_mana + 1) * dec_mana_cost(dec_mana) / 100;
    }
    if (need_mana < 1) need_mana = 1;

    return need_mana;
}


/*
 * Modify spell fail rate
 * Using p_ptr->to_m_chance, p_ptr->dec_mana, p_ptr->easy_spell and p_ptr->heavy_spell
 */
int mod_spell_chance_1(int chance, int realm)
{
    bool dec_mana = p_ptr->dec_mana;

    if (realm && realm == p_ptr->easy_realm1)
        dec_mana++;

    chance += p_ptr->to_m_chance;

    if (p_ptr->heavy_spell) chance += 20;

    chance += dec_mana_fail1(dec_mana, p_ptr->easy_spell);

    if (mut_present(MUT_ARCANE_MASTERY))
        chance -= 3;
    if (prace_is_(RACE_DEMIGOD) && p_ptr->psubrace == DEMIGOD_ATHENA)
        chance -= 2;

    return chance;
}


/*
 * Modify spell fail rate (as "suffix" process)
 * Using p_ptr->dec_mana, p_ptr->easy_spell and p_ptr->heavy_spell
 * Note: variable "chance" cannot be negative.
 */
int mod_spell_chance_2(int chance, int realm)
{
    bool dec_mana = p_ptr->dec_mana;

    if (realm && realm == p_ptr->easy_realm1)
        dec_mana++;

    chance += dec_mana_fail2(dec_mana, p_ptr->easy_spell);

    if (p_ptr->heavy_spell) chance += 5;

    return MAX(chance, 0);
}


/*
 * Returns spell chance of failure for spell -RAK-
 */
s16b spell_chance(int spell, int use_realm)
{
    int             chance, minfail;
    magic_type      *s_ptr;
    int             need_mana;
    caster_info        *caster_ptr = get_caster_info();

    /* Paranoia -- must be literate */
    if (!mp_ptr->spell_book) return (100);

    if (use_realm == REALM_HISSATSU) return 0;

    /* Access the spell */
    if (!is_magic(use_realm))
    {
        s_ptr = &technic_info[use_realm - MIN_TECHNIC][spell];
    }
    else
    {
        s_ptr = &mp_ptr->info[use_realm - 1][spell];
    }

    /* Extract the base spell failure rate */
    chance = s_ptr->sfail;

    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (p_ptr->lev - s_ptr->slevel);

    /* Reduce failure rate by INT/WIS adjustment */
    chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[caster_ptr->which_stat]] - 1);

    if (p_ptr->riding)
        chance += MAX(plr_riding_lvl() - skills_riding_current() / 100 - 10, 0);

    /* Extract mana consumption rate */
    need_mana = mod_need_mana(s_ptr->smana, spell, use_realm);

    /* Not enough mana to cast */
    if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
    {
        /* Spells can't be cast without enough hp, so just leave the fail rate alone! */
    }
    else if (need_mana > p_ptr->csp)
    {
        chance += 5 * (need_mana - p_ptr->csp);
    }

    if ( use_realm != p_ptr->realm1
      && ( p_ptr->pclass == CLASS_MAGE
        || p_ptr->pclass == CLASS_PRIEST
        || p_ptr->pclass == CLASS_YELLOW_MAGE ) )
    {
        chance += 5;
    }

    /* Extract the minimum failure rate */
    minfail = adj_mag_fail[p_ptr->stat_ind[caster_ptr->which_stat]];

    /*
     * Non mage/priest characters never get too good
     * (added high mage, mindcrafter)
     */
    if (caster_ptr && minfail < caster_ptr->min_fail)
        minfail = caster_ptr->min_fail;

    if (prace_is_(RACE_DEMIGOD) && p_ptr->psubrace == DEMIGOD_ATHENA && minfail > 0)
        minfail -= 1;

    /* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
    if (p_ptr->pclass == CLASS_PRIEST || p_ptr->pclass == CLASS_SORCERER)
    {
        if (have_flag(p_ptr->attack_info[0].paf_flags, PAF_ICKY)) chance += 25;
        if (have_flag(p_ptr->attack_info[1].paf_flags, PAF_ICKY)) chance += 25;
    }

    chance = mod_spell_chance_1(chance, use_realm);
    chance = virtue_mod_spell_fail(use_realm, chance);

    /* Minimum failure rate */
    if (chance < minfail) chance = minfail;

    /* Stunning makes spells harder */
    if (plr_tim_find(T_STUN))
        chance += 50 * plr_tim_amount(T_STUN) / 100;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    if ((use_realm == p_ptr->realm1) || (use_realm == p_ptr->realm2)
        || (p_ptr->pclass == CLASS_SORCERER) || (p_ptr->pclass == CLASS_RED_MAGE))
    {
        s16b exp = experience_of_spell(spell, use_realm);
        if (exp >= SPELL_EXP_EXPERT) chance--;
        if (exp >= SPELL_EXP_MASTER) chance--;
    }

    /* Return the chance */
    return mod_spell_chance_2(chance, use_realm);
}



/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool learned, bool study_pray, int use_realm)
{
    magic_type *s_ptr;

    /* Access the spell */
    if (!is_magic(use_realm))
    {
        s_ptr = &technic_info[use_realm - MIN_TECHNIC][spell];
    }
    else
    {
        s_ptr = &mp_ptr->info[use_realm - 1][spell];
    }

    /* Spell is illegal */
    if (s_ptr->slevel > p_ptr->lev) return (FALSE);

    /* Spell is forgotten */
    if ((use_realm == p_ptr->realm2) ?
        (p_ptr->spell_forgotten2 & (1L << spell)) :
        (p_ptr->spell_forgotten1 & (1L << spell)))
    {
        /* Never okay */
        return (FALSE);
    }

    if (p_ptr->pclass == CLASS_SORCERER) return (TRUE);
    if (p_ptr->pclass == CLASS_RED_MAGE) return (TRUE);

    /* Spell is learned */
    if ((use_realm == p_ptr->realm2) ?
        (p_ptr->spell_learned2 & (1L << spell)) :
        (p_ptr->spell_learned1 & (1L << spell)))
    {
        /* Always true */
        return (!study_pray);
    }

    /* Okay to study, not to cast */
    return (!learned);
}


/*
 * Print a list of spells (for browsing or casting or viewing)
 */
void print_spells(int target_spell, byte *spells, int num, rect_t display, int use_realm)
{
    int             i, spell, exp_level, increment = 64;
    magic_type     *s_ptr;
    cptr            comment;
    char            info[80];
    char            out_val[160];
    byte            line_attr;
    int             need_mana;
    char            ryakuji[15];
    char            buf[256];
    bool            max = FALSE;
    caster_info    *caster_ptr = get_caster_info();


    if (((use_realm <= REALM_NONE) || (use_realm > MAX_REALM)) && p_ptr->wizard)
        msg_print("Warning! print_spells called with null realm");


    /* Title the list */
    if (use_realm == REALM_HISSATSU)
        strcpy(buf,"  Lvl  SP");
    else
    {
        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
            strcpy(buf,"Profic Lvl  HP Fail Desc");
        else
            strcpy(buf,"Profic Lvl  SP Fail Desc");
    }

    Term_erase(display.x, display.y, display.cx);
    put_str("Name", display.y, display.x + 5);
    put_str(buf, display.y, display.x + 29);

    if ((p_ptr->pclass == CLASS_SORCERER) || (p_ptr->pclass == CLASS_RED_MAGE)) increment = 0;
    else if (use_realm == p_ptr->realm1) increment = 0;
    else if (use_realm == p_ptr->realm2) increment = 32;

    /* Dump the spells */
    for (i = 0; i < num; i++)
    {
        Term_erase(display.x, display.y + i + 1, display.cx);

        /* Access the spell */
        spell = spells[i];

        /* Access the spell */
        if (!is_magic(use_realm))
        {
            s_ptr = &technic_info[use_realm - MIN_TECHNIC][spell];
        }
        else
        {
            s_ptr = &mp_ptr->info[use_realm - 1][spell];
        }

        if (use_realm == REALM_HISSATSU)
            need_mana = s_ptr->smana;
        else
        {
            s16b exp = experience_of_spell(spell, use_realm);

            /* Extract mana consumption rate */
            need_mana = mod_need_mana(s_ptr->smana, spell, use_realm);

            if ((increment == 64) || (s_ptr->slevel >= 99)) exp_level = EXP_LEVEL_UNSKILLED;
            else exp_level = spell_exp_level(exp);

            max = FALSE;
            if (!increment && (exp_level == EXP_LEVEL_MASTER)) max = TRUE;
            else if ((increment == 32) && (exp_level >= EXP_LEVEL_EXPERT)) max = TRUE;
            else if (s_ptr->slevel >= 99) max = TRUE;
            else if ((p_ptr->pclass == CLASS_RED_MAGE) && (exp_level >= EXP_LEVEL_SKILLED)) max = TRUE;

            strncpy(ryakuji, exp_level_str[exp_level], 4);
            ryakuji[3] = ']';
            ryakuji[4] = '\0';
        }

        if (use_menu && target_spell)
        {
            if (i == (target_spell-1))
                strcpy(out_val, "  >  ");
            else
                strcpy(out_val, "     ");
        }
        else sprintf(out_val, "  %c) ", I2A(i));
        /* Skip illegible spells */
        if (s_ptr->slevel >= 99)
        {
                strcat(out_val, format("%-30s", "(illegible)"));
                c_put_str(TERM_L_DARK, out_val, display.y + i + 1, display.x);
                continue;
        }

        /* XXX XXX Could label spells above the players level */

        /* Get extra info */
        strcpy(info, do_spell(use_realm, spell, SPELL_INFO));

        /* Use that info */
        comment = info;

        /* Assume spell is known and tried */
        line_attr = TERM_WHITE;

        /* Analyze the spell */
        if ((p_ptr->pclass == CLASS_SORCERER) || (p_ptr->pclass == CLASS_RED_MAGE))
        {
            if (s_ptr->slevel > p_ptr->max_plv)
            {
                comment = "unknown";

                line_attr = TERM_L_BLUE;
            }
            else if (s_ptr->slevel > p_ptr->lev)
            {
                comment = "forgotten";

                line_attr = TERM_YELLOW;
            }
        }
        else if ((use_realm != p_ptr->realm1) && (use_realm != p_ptr->realm2))
        {
            comment = "unknown";

            line_attr = TERM_L_BLUE;
        }
        else if ((use_realm == p_ptr->realm1) ?
            ((p_ptr->spell_forgotten1 & (1L << spell))) :
            ((p_ptr->spell_forgotten2 & (1L << spell))))
        {
            comment = "forgotten";

            line_attr = TERM_YELLOW;
        }
        else if (!((use_realm == p_ptr->realm1) ?
            (p_ptr->spell_learned1 & (1L << spell)) :
            (p_ptr->spell_learned2 & (1L << spell))))
        {
            comment = "unknown";

            line_attr = TERM_L_BLUE;
        }
        else if (!((use_realm == p_ptr->realm1) ?
            (p_ptr->spell_worked1 & (1L << spell)) :
            (p_ptr->spell_worked2 & (1L << spell))))
        {
            comment = "untried";

            line_attr = TERM_L_GREEN;
        }

        /* Dump the spell --(-- */
        if (use_realm == REALM_HISSATSU)
        {
            strcat(out_val, format("%-25s %3d %3d",
                do_spell(use_realm, spell, SPELL_NAME),
                s_ptr->slevel, need_mana));
        }
        else
        {
            strcat(out_val, format("%-25s%c%-4s %3d %3d %3d%% %s",
                do_spell(use_realm, spell, SPELL_NAME),
                (max ? '!' : ' '), ryakuji,
                s_ptr->slevel, need_mana, spell_chance(spell, use_realm), comment));
        }
        c_put_str(line_attr, out_val, display.y + i + 1, display.x);
    }

    /* Clear the bottom line */
    Term_erase(display.x, display.y + i + 1, display.cx);
}


/*
 * Note that amulets, rods, and high-level spell books are immune
 * to "inventory damage" of any kind. Also sling ammo and shovels.
 */


/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool hates_acid(object_type *o_ptr)
{
    /* Analyze the type */
    switch (o_ptr->tval)
    {
        /* Wearable items */
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        {
            return (TRUE);
        }

        /* Staffs/Scrolls are wood/paper */
        case TV_STAFF:
        case TV_SCROLL:
        {
            return (TRUE);
        }

        /* Ouch */
        case TV_CHEST:
        {
            return (TRUE);
        }

        /* Junk is useless */
        case TV_SKELETON:
        case TV_BOTTLE:
        case TV_JUNK:
        {
            return (TRUE);
        }
    }

    return (FALSE);
}


/*
 * Does a given object (usually) hate electricity?
 */
bool hates_elec(object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
        case TV_RING:
        case TV_WAND:
        {
            return (TRUE);
        }
    }

    return (FALSE);
}


/*
 * Does a given object (usually) hate fire?
 * Hafted/Polearm weapons have wooden shafts.
 * Arrows/Bows are mostly wooden.
 */
bool hates_fire(object_type *o_ptr)
{
    /* Analyze the type */
    switch (o_ptr->tval)
    {
        /* Wearable */
        case TV_LITE:
        case TV_ARROW:
        case TV_BOW:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        {
            return (TRUE);
        }

        /* Books */
        case TV_LIFE_BOOK:
        case TV_SORCERY_BOOK:
        case TV_NATURE_BOOK:
        case TV_CHAOS_BOOK:
        case TV_DEATH_BOOK:
        case TV_TRUMP_BOOK:
        case TV_ARCANE_BOOK:
        case TV_CRAFT_BOOK:
        case TV_DAEMON_BOOK:
        case TV_CRUSADE_BOOK:
        case TV_NECROMANCY_BOOK:
        case TV_ARMAGEDDON_BOOK:
        case TV_MUSIC_BOOK:
        case TV_HISSATSU_BOOK:
        case TV_HEX_BOOK:
        {
            return (TRUE);
        }

        /* Chests */
        case TV_CHEST:
        {
            return (TRUE);
        }

        /* Staffs/Scrolls burn */
        case TV_STAFF:
        case TV_SCROLL:
        {
            return (TRUE);
        }
    }

    return (FALSE);
}


/*
 * Does a given object (usually) hate cold?
 */
bool hates_cold(object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
        case TV_POTION:
        case TV_FLASK:
        case TV_BOTTLE:
        {
            return (TRUE);
        }
    }

    return (FALSE);
}


/*
 * Melt something
 */
int set_acid_destroy(object_type *o_ptr)
{
    if (!hates_acid(o_ptr)) return FALSE;
    if (obj_has_flag(o_ptr, OF_IGNORE_ACID)) return FALSE;
    return TRUE;
}


/*
 * Electrical damage
 */
int set_elec_destroy(object_type *o_ptr)
{
    if (!hates_elec(o_ptr)) return FALSE;
    if (obj_has_flag(o_ptr, OF_IGNORE_ELEC)) return FALSE;
    return TRUE;
}


/*
 * Burn something
 */
int set_fire_destroy(object_type *o_ptr)
{
    if (!hates_fire(o_ptr)) return FALSE;
    if (obj_has_flag(o_ptr, OF_IGNORE_FIRE)) return FALSE;
    return TRUE;
}


/*
 * Freeze things
 */
int set_cold_destroy(object_type *o_ptr)
{
    if (!hates_cold(o_ptr)) return FALSE;
    if (obj_has_flag(o_ptr, OF_IGNORE_COLD)) return FALSE;
    return TRUE;
}


/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 * Destruction taken from "melee.c" code for "stealing".
 * New-style wands and rods handled correctly. -LM-
 * Returns number of items destroyed.
 */
static void _damage_obj(obj_ptr obj, int p1, int p2, int which_res)
{
    int i, amt = 0;

    /* Test each and every object in the pile for destruction */
    for (i = 0; i < obj->number; i++)
    {
        if ( randint0(100) < p1 /* Effects of Breath Quality */
          && randint0(100) < p2 /* Effects of Inventory Protection (Rune or Spell) */
          && !res_save_inventory(which_res) ) /* Effects of Resistance */
        {
            amt++;
        }
    }

    if (amt)
    {
        char o_name[MAX_NLEN];
        object_desc(o_name, obj, OD_OMIT_PREFIX | OD_COLOR_CODED);

        if (amt < obj->number)
            msg_format("%d of your %s %s destroyed!",
                        amt, o_name, (amt > 1) ? "were" : "was");
        else if (obj->number == 1)
            msg_format("Your %s was destroyed!", o_name);
        else
            msg_format("All of your %s were destroyed!", o_name);

        if (obj_is_potion(obj))
            potion_smash_effect(0, p_ptr->pos.y, p_ptr->pos.x, obj->k_idx);

        stats_on_m_destroy(obj, amt);

        obj->number -= amt;
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}
void inven_damage(inven_func typ, int p1, int which)
{
    slot_t slot;
    int    p2 = 100;

    if (CHECK_MULTISHADOW()) return;

    if (p_ptr->rune_elem_prot) p2 = 50;
    if (p_ptr->inven_prot) p2 = 50;

    /* Pack */
    for (slot = 1; slot <= pack_max(); slot++)
    {
        obj_ptr obj = pack_obj(slot);

        if (!obj) continue;
        if (obj_is_art(obj)) continue;
        if (!typ(obj)) continue;

        _damage_obj(obj, p1, p2, which);
    }

    /* Quiver */
    slot = equip_find_obj(TV_QUIVER, SV_ANY);
    if (slot)
    {
        obj_ptr quiver = equip_obj(slot);
        if (quiver->name2 != EGO_QUIVER_PROTECTION)
        {
            for (slot = 1; slot <= quiver_max(); slot++)
            {
                obj_ptr obj = quiver_obj(slot);

                if (!obj) continue;
                if (obj_is_art(obj)) continue;
                if (!typ(obj)) continue;

                _damage_obj(obj, p1, p2, which);
            }
        }
    }
}


/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
int minus_ac(void)
{
    int slot = equip_random_slot(obj_is_armor);

    if (slot)
    {
        object_type *o_ptr = equip_obj(slot);
        char         o_name[MAX_NLEN];

        if (o_ptr->ac + o_ptr->to_a <= 0) return FALSE;

        object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        if (demigod_is_(DEMIGOD_HEPHAESTUS))
        {
            obj_learn_flag(o_ptr, OF_IGNORE_ACID);
            return TRUE;
        }
        if (obj_has_flag(o_ptr, OF_IGNORE_ACID))
        {
            if (disturb_minor)
                msg_format("Your %s is unaffected!", o_name);
            obj_learn_flag(o_ptr, OF_IGNORE_ACID);
            return TRUE;
        }

        msg_format("Your %s is damaged!", o_name);
        o_ptr->to_a--;
        p_ptr->update |= PU_BONUS;
        p_ptr->window |= PW_EQUIP;
        android_calc_exp();
        return TRUE;
    }
    return FALSE;
}

bool rustproof(void)
{
    obj_prompt_t prompt = {0};
    char         o_name[MAX_NLEN];

    prompt.prompt = "Rustproof which piece of armour?";
    prompt.error = "You have nothing to rustproof.";
    prompt.filter = obj_is_armor;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

    add_flag(prompt.obj->flags, OF_IGNORE_ACID);
    add_flag(prompt.obj->known_flags, OF_IGNORE_ACID);

    if ((prompt.obj->to_a < 0) && !obj_is_cursed(prompt.obj))
    {
        msg_format("%s %s look%s as good as new!",
            ((prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The"), o_name,
            ((prompt.obj->number > 1) ? "" : "s"));
        prompt.obj->to_a = 0;
    }

    msg_format("%s %s %s now protected against corrosion.",
        ((prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The"), o_name,
        ((prompt.obj->number > 1) ? "are" : "is"));

    android_calc_exp();
    return TRUE;
}

/*
 * Helper for Cursing Equipment (?Curse Armor and ?Curse Weapn)
 * Also used when sacrificing a worn piece of equipment.
 */

void blast_object(object_type *o_ptr)
{
    bool is_armor = obj_is_armor(o_ptr);
    bool is_weapon = obj_is_weapon(o_ptr);
    int i;

    if (have_flag(o_ptr->flags, OF_NO_REMOVE))
        return;

    o_ptr->name1 = 0;
    o_ptr->name2 = EGO_SPECIAL_BLASTED;
    o_ptr->name3 = 0;
    o_ptr->to_a = 0;
    o_ptr->to_h = 0;
    o_ptr->to_d = 0;
    o_ptr->ac = 0;
    o_ptr->dd = 0;
    o_ptr->ds = 0;

    if (is_armor)
        o_ptr->to_a -= randint1(5) + randint1(5);

    if (is_weapon)
    {
        o_ptr->to_h -= randint1(5) + randint1(5);
        o_ptr->to_d -= randint1(5) + randint1(5);
    }

    for (i = 0; i < OF_ARRAY_SIZE; i++)
        o_ptr->flags[i] = 0;

    o_ptr->rune = 0;

    o_ptr->ident |= (IDENT_BROKEN);

    p_ptr->update |= (PU_BONUS);
    p_ptr->update |= (PU_MANA);
    p_ptr->window |= (PW_INVEN | PW_EQUIP);
}

/*
 * Curse the players armor
 */
bool curse_armor(int slot)
{
    object_type *o_ptr;
    char o_name[MAX_NLEN];

    if (!slot) return FALSE;
    o_ptr = equip_obj(slot);
    if (!o_ptr) return FALSE;

    object_desc(o_name, o_ptr, OD_OMIT_PREFIX);

    if (obj_is_art(o_ptr) && (randint0(100) < 50))
    {
        msg_format("A %s tries to %s, but your %s resists the effects!",
               "terrible black aura", "surround your armor", o_name);
    }
    else
    {
        msg_format("A terrible black aura blasts your %s!", o_name);
        virtue_add(VIRTUE_ENCHANTMENT, -5);
        blast_object(o_ptr);
        o_ptr->curse_flags |= OFC_CURSED;
    }

    return TRUE;
}


/*
 * Curse the players weapon
 */
bool curse_weapon(bool force, int slot)
{
    object_type *o_ptr;
    char o_name[MAX_NLEN];

    if (!slot) return FALSE;
    o_ptr = equip_obj(slot);
    if (!o_ptr) return FALSE;

    object_desc(o_name, o_ptr, OD_OMIT_PREFIX);

    /* Attempt a saving throw */
    if (obj_is_art(o_ptr) && (randint0(100) < 50) && !force)
    {
        msg_format("A %s tries to %s, but your %s resists the effects!",
               "terrible black aura", "surround your weapon", o_name);
    }
    else
    {
        if (!force) msg_format("A terrible black aura blasts your %s!", o_name);
        virtue_add(VIRTUE_ENCHANTMENT, -5);
        blast_object(o_ptr);
        o_ptr->curse_flags |= OFC_CURSED;
    }
    return TRUE;
}

/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * Note that this function is one of the more "dangerous" ones...
 */
static s16b poly_r_idx(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);

    int i, lev1, lev2;

    /* Hack -- Uniques never polymorph */
    if (r_ptr->flags1 & RF1_UNIQUE) return r_idx;

    /* Allowable range of "levels" for resulting monster */
    if (r_ptr->level < 30)
    {
        /* 1d20/1d9 Distribution (AVG 2.96):
         *  0  1  2  3  4  5  6  7  8  9 10 11 ...
         * 36 45 36 18 11  7  6  3  3  3  2  1 ... */
        lev1 = r_ptr->level - ((randint1(20) / randint1(9)) + 1);
        lev2 = r_ptr->level + ((randint1(20) / randint1(9)) + 1);
    }
    else
    {
        /* However, polymorph gives the same small set of monsters
         * once you get deeper. You'll need to consult r_info.txt,
         * sorted by level (try MonsterDam.csv in lib/help). So,
         * let's try something else. This is for the Chaos Vortex,
         * btw, who polymorphs in melee as a way of life! */
        lev1 = r_ptr->level - r_ptr->level * randnor(100, 20) / 1000;
        lev2 = r_ptr->level + r_ptr->level * randnor(100, 20) / 1000;
    }

    /* Pick a (possibly new) non-unique race */
    for (i = 0; i < 1000; i++)
    {
        /* Pick a new race, using a level calculation */
        r_ptr = mon_alloc_choose((cave->difficulty + r_ptr->level) / 2 + 5);

        /* Handle failure */
        if (!r_ptr) break;

        /* Ignore unique monsters */
        if (r_ptr->flags1 & RF1_UNIQUE) continue;

        /* Ignore monsters with incompatible levels */
        if ((r_ptr->level < lev1) || (r_ptr->level > lev2)) continue;

        /* Use that index */
        r_idx = r_ptr->id;

        /* Done */
        break;
    }

    /* Result */
    return (r_idx);
}


bool polymorph_monster(mon_ptr mon)
{
    int r_idx;

    if (mon->mflag2 & MFLAG2_QUESTOR) return FALSE;
    r_idx = poly_r_idx(mon->r_idx);
    if (r_idx != mon->r_idx || p_ptr->wizard)
        mon_change_race(mon, r_idx, "polymorphed");
    return TRUE;
}


/*
 * Dimension Door
 */
bool dimension_door_aux(int x, int y, int rng)
{
    int    plev = p_ptr->lev;

    if (!mut_present(MUT_ASTRAL_GUIDE))
        p_ptr->energy_need += (s16b)((s32b)(60 - plev) * ENERGY_NEED() / 100L);

    if (p_ptr->wizard)
    {
        teleport_player_to(y, x, 0L);
        return TRUE;
    }
    else if ( !cave_player_teleportable_bold(y, x, 0L)
           || distance(y, x, p_ptr->pos.y, p_ptr->pos.x) > rng
           || !randint0(plev / 10 + 10) )
    {
        if (!mut_present(MUT_ASTRAL_GUIDE))
            p_ptr->energy_need += (s16b)((s32b)(60 - plev) * ENERGY_NEED() / 100L);
        teleport_player((plev + 2) * 2, TELEPORT_PASSIVE);
        return FALSE;
    }
    else
    {
        teleport_player_to(y, x, 0L);
        return TRUE;
    }
}


/*
 * Dimension Door
 */
bool dimension_door(int rng)
{
    int x = 0, y = 0;

    /* Rerutn FALSE if cancelled */
    if (!tgt_pt(&x, &y, rng)) return FALSE;

    if (dimension_door_aux(x, y, rng)) return TRUE;

    if (p_ptr->pclass == CLASS_TIME_LORD)
        msg_print("You fail to exit the temporal plane correctly!");
    else
        msg_print("You fail to exit the astral plane correctly!");

    return TRUE;
}


bool eat_magic(int power)
{
    obj_prompt_t prompt = {0};
    int          amt;
    int          fail_odds = 0, lev;

    if (p_ptr->pclass == CLASS_RUNE_KNIGHT)
    {
        msg_print("You are not allowed to Eat Magic!");
        return FALSE;
    }

    prompt.prompt = "Drain which item?";
    prompt.error = "You have nothing to drain.";
    prompt.filter = _obj_recharge_src;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    amt = prompt.obj->activation.difficulty;
    if (amt > device_sp(prompt.obj))
        amt = device_sp(prompt.obj);

    lev = prompt.obj->activation.difficulty;
    if (power > lev/2)
        fail_odds = (power - lev/2) / 5;

    if (one_in_(fail_odds))
    {
        char name[MAX_NLEN];
        bool drain = FALSE;


        if (obj_is_std_art(prompt.obj) || !one_in_(10))
            drain = TRUE;

        if (drain)
        {
            device_decrease_sp(prompt.obj, device_sp(prompt.obj));
            object_desc(name, prompt.obj, OD_OMIT_PREFIX | OD_COLOR_CODED);
            msg_format("Failed! Your %s is completely drained.", name);
        }
        else
        {
            object_desc(name, prompt.obj, OD_OMIT_PREFIX | OD_COLOR_CODED);
            msg_format("Failed! Your %s is destroyed.", name);
            prompt.obj->number--;
            obj_release(prompt.obj, OBJ_RELEASE_QUIET);
        }
    }
    else
    {
        device_decrease_sp(prompt.obj, amt);
        sp_player(amt);
        obj_release(prompt.obj, OBJ_RELEASE_DELAYED_MSG);
    }

    p_ptr->window |= PW_INVEN;
    return TRUE;
}


bool summon_kin_player(int level, int y, int x, u32b mode)
{
    point_t pos = point_create(x, y);
    bool pet = BOOL(mode & PM_FORCE_PET);
    if (!pet) mode |= PM_NO_PET;

    switch (p_ptr->mimic_form)
    {
    case MIMIC_NONE:
        switch (p_ptr->prace)
        {
            case RACE_HUMAN:
            case RACE_AMBERITE:
            case RACE_BARBARIAN:
            case RACE_BEASTMAN:
            case RACE_DUNADAN:
            case RACE_DEMIGOD:
                summon_kin_type = 'p';
                break;
            case RACE_DARK_ELF:
            case RACE_HIGH_ELF:
            case RACE_WATER_ELF:
            case RACE_WOOD_ELF:
            case RACE_HOBBIT:
            case RACE_GNOME:
            case RACE_DWARF:
            case RACE_NIBELUNG:
            case RACE_MIND_FLAYER:
            case RACE_KUTAR:
            case RACE_SHADOW_FAIRY:
                summon_kin_type = 'h';
                break;
            case RACE_SNOTLING:
                summon_kin_type = 'o';
                break;
            case RACE_HALF_TROLL:
                summon_kin_type = 'T';
                break;
            case RACE_HALF_OGRE:
                summon_kin_type = 'O';
                break;
            case RACE_HALF_GIANT:
            case RACE_HALF_TITAN:
            case RACE_CYCLOPS:
                summon_kin_type = 'P';
                break;
            case RACE_YEEK:
                summon_kin_type = 'y';
                break;
            case RACE_KLACKON:
                summon_kin_type = 'K';
                break;
            case RACE_KOBOLD:
                summon_kin_type = 'k';
                break;
            case RACE_IMP:
                if (one_in_(13)) summon_kin_type = 'U';
                else summon_kin_type = 'u';
                break;
            case RACE_DRACONIAN:
                summon_kin_type = 'd';
                if (p_ptr->lev >= 40)
                    summon_kin_type = 'D';
                break;
            case RACE_GOLEM:
            case RACE_ANDROID:
                summon_kin_type = 'g';
                break;
            case RACE_SKELETON:
                if (one_in_(13)) summon_kin_type = 'L';
                else summon_kin_type = 's';
                break;
            case RACE_ZOMBIE:
                summon_kin_type = 'z';
                break;
            case RACE_VAMPIRE:
                summon_kin_type = 'V';
                break;
            case RACE_SPECTRE:
                summon_kin_type = 'G';
                break;
            case RACE_SPRITE:
                summon_kin_type = 'I';
                break;
            case RACE_ENT:
                summon_kin_type = '#';
                break;
            case RACE_ARCHON:
                summon_kin_type = 'A';
                break;
            case RACE_BALROG:
                summon_kin_type = 'U';
                break;
            default:
                summon_kin_type = 'p';
                break;
        }
        break;
    case MIMIC_DEMON:
        if (one_in_(13)) summon_kin_type = 'U';
        else summon_kin_type = 'u';
        break;
    case MIMIC_DEMON_LORD:
        summon_kin_type = 'U';
        break;
    case MIMIC_VAMPIRE:
        summon_kin_type = 'V';
        break;
    case MIMIC_CLAY_GOLEM:
    case MIMIC_IRON_GOLEM:
    case MIMIC_MITHRIL_GOLEM:
    case MIMIC_COLOSSUS:
        summon_kin_type = 'g';
        break;
    }

    if (warlock_is_(WARLOCK_GIANTS))
        summon_kin_type = 'P';

    if (p_ptr->current_r_idx)
        summon_kin_type = mon_race_lookup(p_ptr->current_r_idx)->d_char;

    return summon_specific((pet ? -1 : 0), pos, level, SUMMON_KIN, mode);
}
