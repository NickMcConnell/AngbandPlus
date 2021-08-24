/* File: melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Monster spells and movement */

/*
* This file has several additions to it by Keldon Jones (keldon@umr.edu)
* to improve the general quality of the AI (version 0.1.1).
*/

#include "angband.h"
#include <assert.h>

#define SPEAK_CHANCE 8
#define GRINDNOISE 20
#define CYBERNOISE 20

/*
 * Calculate the direction to the next enemy
 */
static bool get_enemy_dir(mon_ptr mon, int *mm)
{
    point_t pos = point_create(-1, -1);

    assert(cave->dun_id == mon->dun_id);

    /* careful: target monsters might not be on the current level!
     * this happens when the plr changes level and pet stragglers
     * have yet to follow the plr to the new level */
    if (riding_t_m_idx && p_ptr->riding == mon->id)
    {
        mon_ptr tgt_mon = dun_mon(cave, riding_t_m_idx);
        if (tgt_mon) pos = tgt_mon->pos;
    }
    if (is_pet(mon) && pet_t_m_idx && !dun_pos_interior(cave, pos))
    {
        mon_ptr tgt_mon = dun_mon(cave, pet_t_m_idx);
        if (tgt_mon) pos = tgt_mon->pos;
    }

    if (!dun_pos_interior(cave, pos))
    {
        mon_race_ptr race = mon_race(mon);
        point_map_iter_ptr iter;

        for (iter = point_map_iter_alloc(cave->mon_pos);
                point_map_iter_is_valid(iter);
                point_map_iter_next(iter))
        {
            mon_ptr tgt_mon = point_map_iter_current(iter);
            
            if (tgt_mon == mon) continue;
            if (is_pet(mon))
            {
                if (p_ptr->pet_follow_distance < 0) /* Only fight away from player */
                {
                    int rng = -p_ptr->pet_follow_distance;
                    if (tgt_mon->cdis <= rng) continue;
                }
                else if (mon->cdis < tgt_mon->cdis && tgt_mon->cdis > p_ptr->pet_follow_distance)
                    continue; /* No fighting away from player */
                if (race->aaf < tgt_mon->cdis) continue; /* XXX */
            }
            if (!are_enemies(mon, tgt_mon)) continue;

            if ( ((race->flags2 & RF2_PASS_WALL) && (mon->id != p_ptr->riding || p_ptr->pass_wall))
              || ((race->flags2 & RF2_KILL_WALL) && mon->id != p_ptr->riding) )
            {
                if (!in_disintegration_range(mon->pos.y, mon->pos.x, tgt_mon->pos.y, tgt_mon->pos.x))
                    continue;
            }
            else if (!point_project(mon->pos, tgt_mon->pos)) continue;

            /* XXX Take first available target */
            pos = tgt_mon->pos;
            break;
        }
        point_map_iter_free(iter);
        if (!dun_pos_interior(cave, pos)) return FALSE;
    }

    /* Extract the direction */
    pos = point_subtract(pos, mon->pos);

    /* North */
    if (pos.y < 0 && pos.x == 0)
    {
        mm[0] = 8;
        mm[1] = 7;
        mm[2] = 9;
    }
    /* South */
    else if (pos.y > 0 && pos.x == 0)
    {
        mm[0] = 2;
        mm[1] = 1;
        mm[2] = 3;
    }
    /* East */
    else if (pos.x > 0 && pos.y == 0)
    {
        mm[0] = 6;
        mm[1] = 9;
        mm[2] = 3;
    }
    /* West */
    else if (pos.x < 0 && pos.y == 0)
    {
        mm[0] = 4;
        mm[1] = 7;
        mm[2] = 1;
    }
    /* North-West */
    else if (pos.y < 0 && pos.x < 0)
    {
        mm[0] = 7;
        mm[1] = 4;
        mm[2] = 8;
    }
    /* North-East */
    else if (pos.y < 0 && pos.x > 0)
    {
        mm[0] = 9;
        mm[1] = 6;
        mm[2] = 8;
    }
    /* South-West */
    else if (pos.y > 0 && pos.x < 0)
    {
        mm[0] = 1;
        mm[1] = 4;
        mm[2] = 2;
    }
    /* South-East */
    else if (pos.y > 0 && pos.x > 0)
    {
        mm[0] = 3;
        mm[1] = 6;
        mm[2] = 2;
    }

    /* Found a monster */
    return TRUE;
}


/*
 * Hack, based on mon_take_hit... perhaps all monster attacks on
 * other monsters should use this?
 */
void mon_take_hit_mon(int m_idx, int dam, bool *fear, cptr note, int who)
{
    monster_type    *m_ptr = dun_mon(cave, m_idx);
    monster_race    *r_ptr = mon_race(m_ptr);
    bool             who_is_pet = FALSE;

    char m_name[160];

    bool seen = mon_show_msg(m_ptr);

    /* Can the player be aware of this attack? */
    bool known = (m_ptr->cdis <= MAX_SIGHT);

    if (who && is_pet(dun_mon(cave, who)))
        who_is_pet = TRUE;

    /* Extract monster name */
    monster_desc(m_name, m_ptr, 0);

    /* Redraw (later) if needed */
    if (m_ptr->ml)
    {
        check_mon_health_redraw(m_idx);
    }

    /* Wake it up */
    mon_tim_delete(m_ptr, MT_SLEEP);

    if (p_ptr->riding && (m_idx == p_ptr->riding)) disturb(1, 0);

    if (mon_tim_find(m_ptr, T_INVULN) && randint0(PENETRATE_INVULNERABILITY))
    {
        if (seen)
            msg_format("%^s is unharmed.", m_name);

        return;
    }

    if (r_ptr->flagsr & RFR_RES_ALL)
    {
        if(dam > 0)
        {
            dam /= 100;
            if((dam == 0) && one_in_(3)) dam = 1;
        }
        if (dam==0)
        {
            if (seen)
                msg_format("%^s is unharmed.", m_name);

            return;
        }
    }

    /* Hurt it */
    m_ptr->hp -= dam;

    /* It is dead now... or is it? */
    if (m_ptr->hp < 0)
    {
        if ( ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL) || (m_ptr->mflag2 & MFLAG2_QUESTOR))
          && !prace_is_(RACE_MON_QUYLTHULG))
        {
            m_ptr->hp = 1;
        }
        else
        {
            /* Make a sound */
            if (!monster_living(r_ptr))
            {
                sound(SOUND_N_KILL);
            }
            else
            {
                sound(SOUND_KILL);
            }

            if (known)
            {
                monster_desc(m_name, m_ptr, MD_TRUE_NAME);
                /* Unseen death by normal attack */
                if (!seen)
                {
                    mon_fight = TRUE;
                }
                /* Death by special attack */
                else if (note)
                {
                    msg_format("%^s%s", m_name, note);
                }
                /* Death by normal attack -- nonliving monster */
                else if (!monster_living(r_ptr))
                {
                    msg_format("%^s is destroyed.", m_name);
                }
                /* Death by normal attack -- living monster */
                else
                {
                    msg_format("%^s is killed.", m_name);
                }
            }

            monster_gain_exp(who, m_ptr->r_idx);

            mon_check_kill_unique(m_idx);

            /* Generate treasure */
            monster_death(m_ptr, who_is_pet);

            /* Delete the monster */
            delete_monster(m_ptr);

            /* Not afraid */
            (*fear) = FALSE;

            /* Monster is dead */
            return;
        }
    }

#ifdef ALLOW_FEAR

    /* Mega-Hack -- Pain cancels fear */
    if (mon_tim_find(m_ptr, T_FEAR) && dam > 0) m_ptr->pain += dam;

    /* Sometimes a monster gets scared by damage */
    if (!mon_tim_find(m_ptr, T_FEAR) && !(r_ptr->flags3 & RF3_NO_FEAR) && !mon_tim_find(m_ptr, T_BERSERK))
    {
        /* Percentage of fully healthy */
        int percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

        /*
        * Run (sometimes) if at 10% or less of max hit points,
        * or (usually) when hit for half its current hit points
         */
        if (((percentage <= 10) && (randint0(10) < percentage)) ||
            ((dam >= m_ptr->hp) && (randint0(100) < 80)))
        {
            int amt = randint1(10);
            if (dam >= m_ptr->hp && percentage > 7) amt += 20;
            else amt += (11 - percentage)*5;
            mon_tim_add(m_ptr, T_FEAR, amt);
            (*fear) = TRUE;
        }
    }

#endif /* ALLOW_FEAR */

    if ((dam > 0) && !is_pet(m_ptr) && !is_friendly(m_ptr) && (who != m_idx))
    {
        if (is_pet(dun_mon(cave, who)) && !plr_at(m_ptr->target))
        {
            set_target(m_ptr, dun_mon(cave, who)->pos);
        }
    }

    if (p_ptr->riding && (p_ptr->riding == m_idx) && (dam > 0))
    {
        char m_name[80];

        /* Extract monster name */
        monster_desc(m_name, m_ptr, 0);

        if (m_ptr->hp > m_ptr->maxhp/3) dam = (dam + 1) / 2;
        if (rakuba((dam > 200) ? 200 : dam, FALSE))
        {
            msg_format("You have thrown off from %s!", m_name);
        }
    }

    /* Not dead yet */
    return;
}


/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players. See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency. Like not using "mod" or "div" when possible. And
 * attempting to check the conditions in an optimal order. Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
static int mon_will_run(mon_ptr m_ptr)
{
#ifdef ALLOW_TERROR

    monster_race *r_ptr = mon_race(m_ptr);

    u16b p_lev, m_lev;
    u16b p_chp, p_mhp;
    u16b m_chp, m_mhp;
    u32b p_val, m_val;

#endif

    /* Friends can be commanded to avoid the player */
    if (is_pet(m_ptr))
    {
        /* Are we trying to avoid the player? */
        return ((p_ptr->pet_follow_distance < 0) &&
                  (m_ptr->cdis <= (0 - p_ptr->pet_follow_distance)));
    }

    /* Keep monsters from running too far away */
    if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

    /* All "afraid" monsters will run away */
    if (mon_tim_find(m_ptr, T_FEAR)) return (TRUE);

#ifdef ALLOW_TERROR

    /* Nearby monsters will not become terrified */
    if (m_ptr->cdis <= 5) return (FALSE);

    if ( p_ptr->prace == RACE_MON_RING
      && !p_ptr->riding
      && !is_aware(m_ptr)
      && mon_is_type(m_ptr->r_idx, SUMMON_RING_BEARER) )
    {
        return FALSE;
    }

    /* Examine player power (level) */
    p_lev = p_ptr->lev;

    /* Examine monster power (level plus morale) */
    m_lev = r_ptr->level + (m_ptr->id & 0x08) + 25;

    /* Optimize extreme cases below */
    if (m_lev > p_lev + 4) return (FALSE);
    if (m_lev + 4 <= p_lev) return (TRUE);

    /* Examine player health */
    p_chp = p_ptr->chp;
    p_mhp = p_ptr->mhp;

    /* Examine monster health */
    m_chp = m_ptr->hp;
    m_mhp = m_ptr->maxhp;

    /* Prepare to optimize the calculation */
    p_val = (p_lev * p_mhp) + (p_chp << 2); /* div p_mhp */
    m_val = (m_lev * m_mhp) + (m_chp << 2); /* div m_mhp */

    /* Strong players scare strong monsters */
    if (p_val * m_mhp > m_val * p_mhp) return (TRUE);

#endif

    /* Assume no terror */
    return (FALSE);
}




/*
 * Search for a spell castable grid
 */
static bool get_moves_aux2(mon_ptr mon, point_ptr result)
{
    mon_race_ptr race = mon_race(mon);
    int i, best = 999;
    bool can_open_door = FALSE;
    int now_cost;

    /* Monster can already cast spell to player */
    if (point_project(mon->pos, p_ptr->pos)) return FALSE;

    /* Set current grid cost */
    now_cost = dun_flow_at(cave->flow, mon->pos).cost;
    if (now_cost == 0) now_cost = 999;

    /* Can monster bash or open doors? */
    if (race->flags2 & (RF2_BASH_DOOR | RF2_OPEN_DOOR))
        can_open_door = TRUE;

    /* Check nearby grids, diagonals first */
    for (i = 7; i >= 0; i--)
    {
        point_t tmp_pos = point_step(mon->pos, ddd[i]);
        cave_ptr c_ptr;
        int cost;

        if (!dun_pos_interior(cave, tmp_pos)) continue;
        if (dun_plr_at(cave, tmp_pos)) return FALSE; /* simply attack plr */

        c_ptr = cave_at(tmp_pos);
        cost = dun_flow_at(cave->flow, tmp_pos).cost;

        /* Monster cannot kill or pass walls */
        if (!( ((race->flags2 & RF2_PASS_WALL) && (mon->id != p_ptr->riding || p_ptr->pass_wall))
            || ((race->flags2 & RF2_KILL_WALL) && mon->id != p_ptr->riding) ) )
        {
            if (cost == 0) continue;
            if (!can_open_door && is_closed_door(c_ptr->feat)) continue;
        }

        /* Hack -- for kill or pass wall monster.. */
        if (cost == 0) cost = 998;

        if (now_cost < cost) continue;
        if (!point_project(tmp_pos, p_ptr->pos)) continue;

        /* Accept louder sounds */
        if (best < cost) continue;
        best = cost;
        (*result) = tmp_pos;
    }

    /* No legal move (?) */
    if (best == 999) return FALSE;

    /* Success */
    return TRUE;
}

/*
 * Choose the "best" direction for "flowing"
 *
 * Note that ghosts and rock-eaters are never allowed to "flow",
 * since they should move directly towards the player.
 *
 * Prefer "non-diagonal" directions, but twiddle them a little
 * to angle slightly towards the player's actual location.
 *
 * Allow very perceptive monsters to track old "spoor" left by
 * previous locations occupied by the player. This will tend
 * to have monsters end up either near the player or on a grid
 * recently occupied by the player (and left via "teleport").
 *
 * Note that if "smell" is turned on, all monsters get vicious.
 *
 * Also note that teleporting away from a location will cause
 * the monsters who were chasing you to converge on that location
 * as long as you are still near enough to "annoy" them without
 * being close enough to chase directly. I have no idea what will
 * happen if you combine "smell" with low "aaf" values.
 */
static bool get_moves_aux(mon_ptr mon, point_ptr result, bool no_flow)
{
    int i, best;
    int rng = 16; /* <== I don't understand this value! */

    bool use_scent = FALSE;

    monster_race *r_ptr = mon_race(mon);

    if ( p_ptr->action == ACTION_GLITTER
      && mon_is_type(mon->r_idx, SUMMON_RING_BEARER) )
    {
        rng = AAF_LIMIT_RING;
    }
    else if (cave->dun_type_id == D_SURFACE)
        rng = AAF_LIMIT;

    if (mon_has_attack_spell(mon) && p_ptr->dun_id == cave->dun_id)
    {
        /* Can move onto spell castable grid? */
        if (get_moves_aux2(mon, result)) return TRUE;
    }

    /* Monster can't flow */
    if (no_flow && p_ptr->dun_id == cave->dun_id) return (FALSE);

    /* Monster can go through rocks: Note that in D_MOUNTAIN, fill_type[] is all
     * feat_mountain_wall, which is permanent. Use normal flow instead ... */
    if (cave->dun_type_id != D_MOUNTAIN)
    {
        if ((r_ptr->flags2 & RF2_PASS_WALL) && (mon->id != p_ptr->riding || p_ptr->pass_wall))
            return FALSE;
        if ((r_ptr->flags2 & RF2_KILL_WALL) && mon->id != p_ptr->riding)
            return FALSE;
    }

    /* Hack -- Player can see us, run towards him */
    if (point_los(mon->pos, cave->flow_pos) && point_project(mon->pos, cave->flow_pos))
        return FALSE;

    /* If we can hear noises, advance towards them */
    if (dun_flow_at(cave->flow, mon->pos).cost)
        best = 999;

    /* Otherwise, try to follow a scent trail */
    #if 0
    else if (c_ptr->when)
    {
        /* Too old smell */
        if (p_ptr->dun_id != cave->dun_id) return FALSE;
        if (cave_at(p_ptr->pos)->when - c_ptr->when > 127) return FALSE;
        use_scent = TRUE;
        best = 0;
    }
    #endif

    /* Otherwise, advance blindly */
    else
    {
        return FALSE;
    }

    /* Check nearby grids, diagonals first */
    for (i = 7; i >= 0; i--)
    {
        point_t tmp_pos = point_step(mon->pos, ddd[i]);

        if (!dun_pos_interior(cave, tmp_pos)) continue;

        /* We're following a scent trail */
        if (use_scent)
        {
            #if 0
            int when = c_ptr->when;

            /* Accept younger scent */
            if (best > when) continue;
            best = when;
            #endif
        }

        /* We're using sound */
        else
        {
            int cost;
            flow_data_t flow = dun_flow_at(cave->flow, tmp_pos);

            if (r_ptr->flags2 & (RF2_BASH_DOOR | RF2_OPEN_DOOR))
                cost = flow.dist;
            else cost = flow.cost;

            /* Accept louder sounds */
            if (cost == 0 || best < cost) continue;
            /* Hack: Don't flow into occupied squares
               TODO: Don't flow into squares that the monster can't move into.
            */
            if (mon_at(tmp_pos) && !(r_ptr->flags2 & (RF2_MOVE_BODY|RF2_KILL_BODY))) continue;

            best = cost;
        }

        /* Hack -- Save the "twiddled" location */
        /* XXX why not tmp_pos? look at get_moves_aux2 */
        (*result) = point_jump(cave->flow_pos, ddd[i], MAX(rng, r_ptr->aaf));
    }

    /* No legal move (?) */
    if (best == 999 || best == 0) return FALSE;

    /* Success */
    return TRUE;
}


/*
* Provide a location to flee to, but give the player a wide berth.
*
* A monster may wish to flee to a location that is behind the player,
* but instead of heading directly for it, the monster should "swerve"
* around the player so that he has a smaller chance of getting hit.
*/
static bool get_fear_moves_aux(mon_ptr mon, point_ptr result)
{
    int y, x, y1, x1, fy, fx, gy = 0, gx = 0;
    int score = -1;
    int i;

    /* Monster location */
    fy = mon->pos.y;
    fx = mon->pos.x;

    /* Desired destination */
    y1 = fy - result->y;
    x1 = fx - result->x;

    /* Check nearby grids, diagonals first */
    for (i = 7; i >= 0; i--)
    {
        int dis, s;

        /* Get the location */
        y = fy + ddy_ddd[i];
        x = fx + ddx_ddd[i];

        /* Ignore locations off of edge */
        if (!in_bounds2(y, x)) continue;

        /* Don't move toward player */
        /* if (cave_at_xy(x, y)->dist < 3) continue; */ /* Hmm.. Need it? */

        /* Calculate distance of this grid from our destination */
        dis = distance(y, x, y1, x1);

        /* Score this grid */
        s = 5000 / (dis + 3) - 500 / (dun_flow_at(cave->flow, point_create(x, y)).dist + 1);

        /* No negative scores */
        if (s < 0) s = 0;

        /* Ignore lower scores */
        if (s < score) continue;

        /* Save the score and time */
        score = s;

        /* Save the location */
        gy = y;
        gx = x;
    }

    /* No legal move (?) */
    if (score == -1) return (FALSE);

    /* Find deltas */
    result->y = fy - gy;
    result->x = fx - gx;

    /* Success */
    return (TRUE);
}

/*
 * Hack -- Precompute a bunch of calls to distance() in find_safety() and
 * find_hiding().
 *
 * The pair of arrays dist_offsets_y[n] and dist_offsets_x[n] contain the
 * offsets of all the locations with a distance of n from a central point,
 * with an offset of (0,0) indicating no more offsets at this distance.
 *
 * This is, of course, fairly unreadable, but it eliminates multiple loops
 * from the previous version.
 *
 * It is probably better to replace these arrays with code to compute
 * the relevant arrays, even if the storage is pre-allocated in hard
 * coded sizes. At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()"). XXX XXX XXX
 *
 * These arrays could be combined into two big arrays, using sub-arrays
 * to hold the offsets and lengths of each portion of the sub-arrays, and
 * this could perhaps also be used somehow in the "look" code. XXX XXX XXX
 */


static sint d_off_y_0[] =
{ 0 };

static sint d_off_x_0[] =
{ 0 };


static sint d_off_y_1[] =
{ -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static sint d_off_x_1[] =
{ -1, 0, 1, -1, 1, -1, 0, 1, 0 };


static sint d_off_y_2[] =
{ -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static sint d_off_x_2[] =
{ -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };


static sint d_off_y_3[] =
{ -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
  3, 3, 3, 0 };

static sint d_off_x_3[] =
{ -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
  -1, 0, 1, 0 };


static sint d_off_y_4[] =
{ -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
  0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0 };

static sint d_off_x_4[] =
{ -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
  -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0 };


static sint d_off_y_5[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
  -5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 0 };

static sint d_off_x_5[] =
{ -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
  -5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
  0, 1, 0 };


static sint d_off_y_6[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
  5, 5, 6, 6, 6, 0 };

static sint d_off_x_6[] =
{ -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
  0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
  -3, 2, 3, -1, 0, 1, 0 };


static sint d_off_y_7[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
  3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0 };

static sint d_off_x_7[] =
{ -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
  -3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
  6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
  1, 0 };


static sint d_off_y_8[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
  2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
  8, 8, 8, 0 };

static sint d_off_x_8[] =
{ -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
  5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
  7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
  2, 3, -1, 0, 1, 0 };


static sint d_off_y_9[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
  0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
  7, 8, 8, 8, 8, 9, 9, 9, 0 };

static sint d_off_x_9[] =
{ -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
  -5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
  9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
  4, 5, -2, -3, 2, 3, -1, 0, 1, 0 };


static sint *dist_offsets_y[10] =
{
    d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
    d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

static sint *dist_offsets_x[10] =
{
    d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
    d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};

/*
* Choose a "safe" location near a monster for it to run toward.
*
* A location is "safe" if it can be reached quickly and the player
* is not able to fire into it (it isn't a "clean shot"). So, this will
* cause monsters to "duck" behind walls. Hopefully, monsters will also
* try to run towards corridor openings if they are in a room.
*
* This function may take lots of CPU time if lots of monsters are
* fleeing.
*
* Return TRUE if a safe location is available.
*/
static bool find_safety(mon_ptr mon, point_ptr result)
{
    int r, dis, i;
    int gdis = 0;
    point_t gp, dp;

    sint *y_offsets;
    sint *x_offsets;

    cave_type *c_ptr;

    /* Start with adjacent locations, spread further */
    for (r = 1; r < 10; r++)
    {
        /* Get the lists of points with a distance d from (fx, fy) */
        y_offsets = dist_offsets_y[r];
        x_offsets = dist_offsets_x[r];

        /* Check the locations */
        for (i = 0, dp.x = x_offsets[0], dp.y = y_offsets[0];
             dp.x != 0 || dp.y != 0;
             i++, dp.x = x_offsets[i], dp.y = y_offsets[i])
        {
            point_t p = point_add(mon->pos, dp);

            if (!dun_pos_interior(cave, p)) continue;
            c_ptr = cave_at(p);

            /* Skip locations in a wall */
            if (!monster_can_cross_terrain(c_ptr->feat, mon_race(mon), (mon->id == p_ptr->riding) ? CEM_RIDING : 0)) continue;

            /* Check for "availability" (if monsters can flow) */
            if (!(mon->mflag2 & MFLAG2_NOFLOW))
            {
                int d = dun_flow_at(cave->flow, p).dist;
                int d2 = dun_flow_at(cave->flow, mon->pos).dist;

                /* Ignore grids very far from the player */
                if (d == 0) continue;

                /* Ignore too-distant grids */
                if (d > d2 + 2*r) continue;
            }

            /* Check for absence of shot (more or less) */
            if (!plr_project(p))
            {
                /* Calculate distance from player */
                dis = plr_distance(p);

                /* Remember if further than previous */
                if (dis > gdis)
                {
                    gp = p;
                    gdis = dis;
                }
            }
        }

        /* Check for success */
        if (gdis > 0)
        {
            /* Good location */
            *result = point_subtract(mon->pos, gp);

            /* Found safe place */
            return TRUE;
        }
    }

    /* No safe place */
    return FALSE;
}


/*
 * Choose a good hiding place near a monster for it to run toward.
 *
 * Pack monsters will use this to "ambush" the player and lure him out
 * of corridors into open space so they can swarm him.
 *
 * Return TRUE if a good location is available.
 */
static bool find_hiding(mon_ptr mon, point_ptr pos)
{
    monster_race *r_ptr = mon_race(mon);

    int fy = mon->pos.y;
    int fx = mon->pos.x;

    int y, x, dy, dx, d, dis, i;
    int gy = 0, gx = 0, gdis = 999;

    sint *y_offsets, *x_offsets;

    assert(p_ptr->dun_id == cave->dun_id);

    /* Start with adjacent locations, spread further */
    for (d = 1; d < 10; d++)
    {
        /* Get the lists of points with a distance d from (fx, fy) */
        y_offsets = dist_offsets_y[d];
        x_offsets = dist_offsets_x[d];

        /* Check the locations */
        for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
             dx != 0 || dy != 0;
             i++, dx = x_offsets[i], dy = y_offsets[i])
        {
            y = fy + dy;
            x = fx + dx;

            /* Skip illegal locations */
            if (!in_bounds(y, x)) continue;

            /* Skip occupied locations */
            if (!monster_can_enter(y, x, r_ptr, 0)) continue;

            /* Check for hidden, available grid */
            if (!projectable(p_ptr->pos.y, p_ptr->pos.x, y, x) && clean_shot(fy, fx, y, x, FALSE))
            {
                /* Calculate distance from player */
                dis = distance(y, x, p_ptr->pos.y, p_ptr->pos.x);

                /* Remember if closer than previous */
                if (dis < gdis && dis >= 2)
                {
                    gy = y;
                    gx = x;
                    gdis = dis;
                }
            }
        }

        /* Check for success */
        if (gdis < 999)
        {
            /* Good location */
            pos->y = fy - gy;
            pos->x = fx - gx;

            /* Found good place */
            return TRUE;
        }
    }

    /* No good place */
    return FALSE;
}


/*
 * Choose "logical" directions for monster movement
 */
static bool get_moves(mon_ptr mon, int *mm)
{
    mon_race_ptr race = mon_race(mon);
    mon_pack_ptr pack = NULL;
    point_t      pos = point_create(0, 0);
    int          ay, ax;
    int          move_val = 0;
    bool         done = FALSE;
    bool         will_run = mon_will_run(mon);
    bool         no_flow = (mon->mflag2 & MFLAG2_NOFLOW) && dun_flow_at(cave->flow, mon->pos).cost > 2;
    bool         can_pass_wall = (race->flags2 & RF2_PASS_WALL) && (mon->id != p_ptr->riding || p_ptr->pass_wall);

    if (mon->pack_idx)
    {
        pack = dun_mgr_pack(mon->pack_idx);
        if (mon_tim_find(mon, T_BERSERK)) {}
        else if (pack->ai == AI_FEAR)
        {
            int odds = 500 - mon->cdis * mon->cdis;

            /* Recover from fear? */
            if (odds <= 1 || one_in_(odds))
                pack->ai = AI_SEEK;
            else if (mon->cdis > 5)
                will_run = TRUE;
        }
        else if (pack->ai == AI_MAINTAIN_DISTANCE)
        {
            if ( 1 < mon->cdis && mon->cdis <= pack->distance
              && p_ptr->chp >= p_ptr->mhp * 4 / 5 ) /* If @ wounded, pursue! */
            {
                will_run = TRUE;
            }
        }
    }

    /* Counter attack to an enemy monster */
    if (!will_run && dun_pos_interior(cave, mon->target))
    {
        mon_ptr tgt_mon = mon_at(mon->target);
        if (tgt_mon && are_enemies(mon, tgt_mon) && point_project(mon->pos, tgt_mon->pos))
        {
            pos = point_subtract(mon->pos, tgt_mon->pos);
            done = TRUE;
        }
    }

    /* Handle Packs ... (only if player is present) */
    if (!done && !will_run && !mon_tim_find(mon, T_BERSERK) && is_hostile(mon) && pack && p_ptr->dun_id == cave->dun_id)
    {
        if (pack->ai == AI_LURE && !can_pass_wall && !(race->flags2 & RF2_KILL_WALL))
        {
            int i, room = 0, score;

            /* Count room grids next to player */
            for (i = 0; i < 8; i++)
            {
                point_t tmp_pos = point_step(p_ptr->pos, ddd[i]);

                if (!dun_pos_interior(cave, tmp_pos)) continue;
                if (monster_can_cross_terrain(cave_at(tmp_pos)->feat, race, 0))
                    room++;
            }
            if (cave_at(p_ptr->pos)->info & CAVE_ROOM) room -= 2;
            if (!race->spells) room -= 2;

            /* Not in a room and strong player */
            score = 8 * (p_ptr->chp + p_ptr->csp)/(p_ptr->mhp + p_ptr->msp);
            if (room < score)
            {
                if (find_hiding(mon, &pos)) done = TRUE;
            }
        }

        if (pack->ai == AI_SHOOT && mon->cdis > 1)
        {
            pos.x = 0; /* don't move */
            pos.y = 0;
            done = TRUE;
        }
        if ( pack->ai == AI_GUARD_POS
          && mon->cdis > 3    /* abandon guarding if the player gets close ... */
          && mon->anger < 10 ) /* ... or if we get ticked off. */
        {
            pos = point_subtract(mon->pos, point_create(pack->guard_x, pack->guard_y)); /* XXX pack->guard_pos */
            done = TRUE;
        }

        if (pack->ai == AI_GUARD_MON)
        {
            mon_ptr guard_mon = dun_mon(cave, pack->guard_idx);
            if (!guard_mon) pack->ai = AI_SEEK;    /* detect a dead guardian */
            else if ( mon->id != pack->guard_idx
                   && mon->cdis > 3 )
            {
                pos = point_subtract(mon->pos, guard_mon->pos);
                done = TRUE;
            }
        }

        /* Monster groups try to surround the player */
        if (!done && dun_flow_at(cave->flow, mon->pos).dist < 3)
        {
            if (pack->leader_idx == mon->id || pack->count < 5)
            {
                /* Skip pack processing and flow normally towards player ... */
            }
            else
            {
                int i;
                point_t pos2;

                /* Find an empty square near the player to fill */
                for (i = 0; i < 8; i++)
                {
                    int j = (mon->id + i) & 7; /* semi-random direction */
                    pos2 = point_step(p_ptr->pos, ddd[j]);
                    if (point_equals(mon->pos, pos2))
                    {
                        pos2 = p_ptr->pos; /* attack plr */
                        break;
                    }
                    if (!dun_pos_interior(cave, pos2)) continue;

                    /* Ignore filled grids */
                    if (!monster_can_enter(pos2.y, pos2.x, race, 0)) continue;

                    /* Try to fill this hole */
                    break;
                }

                /* Extract the new "pseudo-direction" */
                pos = point_subtract(mon->pos, pos2);
                done = TRUE;
            }
        }
    }

    if (!done)
    {
        /* Flow towards the player */
        point_t pos2 = cave->flow_pos;
        get_moves_aux(mon, &pos2, no_flow);

        /* Extract the "pseudo-direction" */
        pos = point_subtract(mon->pos, pos2);

        /* Not done */
    }

    /* Apply fear if possible and necessary */
    if ((is_pet(mon) || p_ptr->dun_id != cave->dun_id) && will_run)
    {
        /* XXX XXX Not very "smart" */
        pos = point_create(-pos.x, -pos.y);
    }
    else if (!done && will_run)
    {
        /* Try to find safe place */
        if (find_safety(mon, &pos))
        {
            /* Attempt to avoid the player */
            if (!no_flow)
            {
                /* Adjust movement */
                if (get_fear_moves_aux(mon, &pos)) done = TRUE; /* returns pseudo-direction */
            }
        }

        if (!done)
        {
            /* This is not a very "smart" method XXX XXX */
            pos = point_create(-pos.x, -pos.y);
        }
    }


    /* Check for no move */
    if (!pos.x && !pos.y) return (FALSE);

    /* Extract the "absolute distances" */
    ax = ABS(pos.x);
    ay = ABS(pos.y);

    /* Do something weird */
    if (pos.y < 0) move_val += 8;
    if (pos.x > 0) move_val += 4;

    /* Prevent the diamond maneuvre */
    if (ay > (ax << 1)) move_val += 2;
    else if (ax > (ay << 1)) move_val++;

    /* Extract some directions */
    switch (move_val)
    {
    case 0:
        mm[0] = 9;
        if (ay > ax)
        {
            mm[1] = 8;
            mm[2] = 6;
            mm[3] = 7;
            mm[4] = 3;
        }
        else
        {
            mm[1] = 6;
            mm[2] = 8;
            mm[3] = 3;
            mm[4] = 7;
        }
        break;
    case 1:
    case 9:
        mm[0] = 6;
        if (pos.y < 0)
        {
            mm[1] = 3;
            mm[2] = 9;
            mm[3] = 2;
            mm[4] = 8;
        }
        else
        {
            mm[1] = 9;
            mm[2] = 3;
            mm[3] = 8;
            mm[4] = 2;
        }
        break;
    case 2:
    case 6:
        mm[0] = 8;
        if (pos.x < 0)
        {
            mm[1] = 9;
            mm[2] = 7;
            mm[3] = 6;
            mm[4] = 4;
        }
        else
        {
            mm[1] = 7;
            mm[2] = 9;
            mm[3] = 4;
            mm[4] = 6;
        }
        break;
    case 4:
        mm[0] = 7;
        if (ay > ax)
        {
            mm[1] = 8;
            mm[2] = 4;
            mm[3] = 9;
            mm[4] = 1;
        }
        else
        {
            mm[1] = 4;
            mm[2] = 8;
            mm[3] = 1;
            mm[4] = 9;
        }
        break;
    case 5:
    case 13:
        mm[0] = 4;
        if (pos.y < 0)
        {
            mm[1] = 1;
            mm[2] = 7;
            mm[3] = 2;
            mm[4] = 8;
        }
        else
        {
            mm[1] = 7;
            mm[2] = 1;
            mm[3] = 8;
            mm[4] = 2;
        }
        break;
    case 8:
        mm[0] = 3;
        if (ay > ax)
        {
            mm[1] = 2;
            mm[2] = 6;
            mm[3] = 1;
            mm[4] = 9;
        }
        else
        {
            mm[1] = 6;
            mm[2] = 2;
            mm[3] = 9;
            mm[4] = 1;
        }
        break;
    case 10:
    case 14:
        mm[0] = 2;
        if (pos.x < 0)
        {
            mm[1] = 3;
            mm[2] = 1;
            mm[3] = 6;
            mm[4] = 4;
        }
        else
        {
            mm[1] = 1;
            mm[2] = 3;
            mm[3] = 4;
            mm[4] = 6;
        }
        break;
    case 12:
        mm[0] = 1;
        if (ay > ax)
        {
            mm[1] = 2;
            mm[2] = 4;
            mm[3] = 3;
            mm[4] = 7;
        }
        else
        {
            mm[1] = 4;
            mm[2] = 2;
            mm[3] = 7;
            mm[4] = 3;
        }
        break;
    }

    /* invisible monsters, currently undetected, attempt to fool the
     * player by moving somewhat unpredictably (but not erratically) */
    if ((race->flags2 & RF2_INVISIBLE) && !mon->ml && one_in_(2))
    {
        int i;
        /* permute mm[0..2] (first three primary directions) */
        for (i = 0; i < 2; i++)
        {
            int j = rand_range(i, 2);
            int t = mm[i];
            mm[i] = mm[j];
            mm[j] = t;
        }
    }

    /* Wants to move... */
    return TRUE;
}


static bool check_hp_for_feat_destruction(feature_type *f_ptr, monster_type *m_ptr)
{
    return !have_flag(f_ptr->flags, FF_GLASS) ||
           (mon_race(m_ptr)->flags2 & RF2_STUPID) ||
           (m_ptr->hp >= MAX(m_ptr->maxhp / 3, 200));
}

/*
 * Process a monster
 *
 * The monster is known to be within 100 grids of the player
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level. This should prevent the level from being "swamped" by
 * reproducing monsters. It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it. Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * XXX XXX XXX In addition, monsters which *cannot* open or bash
 * down a door will still stand there trying to open it...
 *
 * XXX Technically, need to check for monster in the way
 * combined with that monster being in a wall (or door?)
 *
 * A "direction" of "5" means "pick a random direction".
 */
void process_monster(mon_ptr mon)
{
    monster_race    *r_ptr = mon_race(mon);
    monster_race    *ap_r_ptr = mon_apparent_race(mon);

    int             i, d;

    int             mm[8] = {0};

    cave_type       *c_ptr;
    feature_type    *f_ptr;


    bool            do_turn;
    bool            do_move;
    bool            do_view;
    bool            asc_nerf = FALSE;
    bool            could_splash = FALSE;
    bool            must_alter_to_move;

    bool            did_open_door;
    bool            did_bash_door;
    bool            did_take_item;
    bool            did_kill_item;
    bool            did_move_body;
    bool            did_pass_wall;
    bool            did_kill_wall;
    bool            gets_angry = FALSE;
    bool            can_cross;
    bool            aware = TRUE;

    bool            fear;

    bool            is_riding_mon = (mon->id == p_ptr->riding);

    bool            see_m = mon_show_msg(mon);

    /* Hack: Trump monsters blink continually for free.
       Note, if you move this code below, the monster actually spawns???  Probably,
       he is just in N places at once.
    */
    if ((r_ptr->flags1 & RF1_TRUMP) && one_in_(2))
    {
        if (!mon_tim_find(mon, MT_SLEEP) && !is_riding_mon) /* TODO: Currently broken for riding! */
            teleport_away(mon->id, 5, 0L);
    }

    if (is_riding_mon)
    {
        if (p_ptr->prace == RACE_MON_RING)
        {
            ring_process_m(mon->id);
        }
        else if (!(r_ptr->flags7 & RF7_RIDING))
        {
            if (rakuba(0, TRUE))
            {
                char m_name[80];
                monster_desc(m_name, dun_mon(cave, p_ptr->riding), 0);
                msg_format("You have fallen from %s.", m_name);
            }
        }
    }

    if ((mon->mflag2 & MFLAG2_CHAMELEON) && one_in_(13) && !mon_tim_find(mon, MT_SLEEP))
    {
        choose_new_monster(mon, FALSE, 0);
        r_ptr = mon_race(mon);
    }

    /* Players hidden in shadow are almost imperceptible. -LM- */
    if (p_ptr->special_defense & NINJA_S_STEALTH && !is_pet(mon))
    {
        int tmp = p_ptr->lev*6+(p_ptr->skills.stl+10)*4;
        if (p_ptr->monlite) tmp /= 3;
        if (p_ptr->cursed & OFC_AGGRAVATE) tmp /= 2;
        if (r_ptr->level > (p_ptr->lev*p_ptr->lev/20+10)) tmp /= 3;
        /* Low-level monsters will find it difficult to locate the player. */
        if (randint0(tmp) > (r_ptr->level+20)) aware = FALSE;

        /* Note: The aware flag will induce random monster movement since
           the player is hiding in the shadows. The is_aware() attribute
           means the monster is unaware that a mimic is not an object. In
           this case, movement should still be towards the object (player). */
    }

    /* Are there its parent? */
    if (mon->parent_m_idx && !dun_mon(cave, mon->parent_m_idx))
    {
        /* Its parent have gone, it also goes away.
           Hack: Only for pets.
        */
        if (!is_pet(mon))
        {
            mon_set_parent(mon, 0);
        }
        else
        {
            bool keep = FALSE;
            /* XXX See if master has taken nearby stairs (e.g. player's mount).
             * We'll head for the stairs to follow the player in this case. */
            if (mon->dun_id != p_ptr->dun_id)
            {
                if (dun_mon(plr_dun(), mon->parent_m_idx))
                    keep = TRUE;
            }
            if (!keep)
            {
                if (see_m)
                {
                    char m_name[80];
                    monster_desc(m_name, mon, 0);
                    msg_format("%^s disappears!", m_name);
                }

                /* Delete the monster */
                delete_monster(mon);

                return;
            }
        }
    }

    /* follow player up/down stairs */
    if (point_equals(mon->pos, cave->flow_pos) && p_ptr->dun_id != cave->dun_id && !(r_ptr->flags2 & RF2_MULTIPLY))
    {
        if (dun_take_stairs_mon(cave, mon))
            return;
    }

    if (mon->r_idx == MON_SHURYUUDAN)
    {
        mon_take_hit_mon(mon->id, 1, &fear, " explodes into tiny shreds.", mon->id);
        return;
    }

    if ((is_pet(mon) || is_friendly(mon)) && ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL)))
    {
        static int riding_pinch = 0;

        if (mon->hp < mon->maxhp/3)
        {
            char m_name[80];
            monster_desc(m_name, mon, 0);

            if (is_riding_mon && riding_pinch < 2)
            {
                msg_format("%^s seems to be in so much pain, and trying to escape from your restriction.", m_name);
                riding_pinch++;
                disturb(1, 0);
            }
            else
            {
                if (is_riding_mon)
                {
                    msg_format("%^s succeeded to escape from your restriction!", m_name);
                    if (rakuba(-1, FALSE))
                        msg_print("You have fallen from riding pet.");
                }

                if (see_m)
                {
                    if ((r_ptr->flags2 & RF2_CAN_SPEAK) && (mon->r_idx != MON_GRIP) && (mon->r_idx != MON_WOLF_U) && (mon->r_idx != MON_FANG) &&
                        plr_los(mon->pos) && plr_project(mon->pos))
                    {
                        msg_format("%^s says 'It is the pinch! I will retreat'.", m_name);
                    }
                    msg_format("%^s read a scroll of teleport level.", m_name);
                    msg_format("%^s disappears.", m_name);
                }

                if (is_riding_mon && rakuba(-1, FALSE))
                    msg_print("You have fallen from riding pet.");

                quests_on_kill_mon(mon);
                delete_monster(mon);
                return;
            }
        }
        else
        {
            /* Reset the counter */
            if (is_riding_mon) riding_pinch = 0;
        }
    }

    /* Hack: Rings wake up potential ring bearers */
    if ( mon_tim_find(mon, MT_SLEEP)
      && p_ptr->action == ACTION_GLITTER
      && mon_is_type(mon->r_idx, SUMMON_RING_BEARER) )
    {
        mon_tim_remove(mon, MT_SLEEP);
    }

    /* Handle "sleep" */
    if (mon_tim_find(mon, MT_SLEEP))
    {
        /* Handle non-aggravation - Still sleeping */
        if (!(p_ptr->cursed & OFC_AGGRAVATE)) return;

        /* Handle aggravation */
        mon_tim_remove(mon, MT_SLEEP);

        /* Hack -- Count the wakings
         * XXX Why should aggravation learn the monster's alertness?
        if (is_original_ap_and_seen(mon) && (r_ptr->r_wake < MAX_UCHAR))
        {
            r_ptr->r_wake++;
        } */
    }

    if (is_riding_mon)
    {
        p_ptr->update |= (PU_BONUS);
    }

    /* No one wants to be your friend if you're aggravating */
    if (is_friendly(mon) && (p_ptr->cursed & OFC_AGGRAVATE))
        gets_angry = TRUE;

    /* Paranoia... no pet uniques outside wizard mode -- TY */
    if (is_pet(mon) &&
        ((((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL)) &&
          monster_has_hostile_align(NULL, 10, -10, r_ptr))))
    {
        if (p_ptr->prace != RACE_MON_RING)
            gets_angry = TRUE;
    }

    if (gets_angry)
    {
        if (is_pet(mon) || see_m)
        {
            char m_name[80];
            monster_desc(m_name, mon, is_pet(mon) ? MD_ASSUME_VISIBLE : 0);
            msg_format("%^s suddenly becomes hostile!", m_name);
        }

        set_hostile(mon);
    }

    /* Attempt to "multiply" if able and allowed */
    if ( (r_ptr->flags2 & RF2_MULTIPLY)
      && cave->breed_ct < MAX_REPRO
      && p_ptr->dun_id == cave->dun_id
      && randint1(375) > virtue_current(VIRTUE_HARMONY)
      /* XXX Try to limit breeding (e.g. Spawns of Ubbo) */
      && randint1(MAX_REPRO - cave->breed_ct) > randint1(5*cave->breed_kill_ct) )
    {
        int ct = 0, dir;
        for (dir = 0; dir < 8; dir++)
        {
            point_t p = point_step(mon->pos, ddd[dir]);
            if (!dun_pos_interior(cave, p)) continue;
            if (mon_at(p)) ct++;
        }

        /* Hex */
        if (multiply_barrier(mon->id)) ct = 8;

        /* Hack -- multiply slower in crowded areas */
        if (ct < 4 && one_in_(8 + 8*ct))
        {
            bool allow = TRUE;
            /* Try to multiply */
            if (is_pet(mon))
            {
                int upkeep = calculate_upkeep();
                if (upkeep > 80)
                    allow = FALSE;
                if (p_ptr->pet_extra_flags & PF_NO_BREEDING)
                    allow = FALSE;
            }
            if (allow && multiply_monster(mon->id, FALSE, (is_pet(mon) ? PM_FORCE_PET : 0)))
            {
                /* Take note if visible */
                if (dun_mon(cave, hack_m_idx_ii)->ml)
                {
                    mon_lore_2(mon, RF2_MULTIPLY);
                }

                /* Multiplying takes energy */
                return;
            }
        }
    }


    /* Hack -- Ohmu scatters molds! This is no longer a spell ... just do it! */
    if (mon->r_idx == MON_OHMU)
    {
        if (one_in_(3))
        {
            int  k;
            int  flags = PM_ALLOW_GROUP;
            if (is_pet(mon)) flags |= PM_FORCE_PET;
            for (k = 0; k < 6; k++)
                summon_specific(mon->id, mon->pos, r_ptr->level, SUMMON_BIZARRE1, flags);
        }
    }


    /* Hack! "Cyber" monster makes noise... */
    if (mon->ap_r_idx == MON_CYBER &&
        one_in_(CYBERNOISE) &&
        !mon->ml && (mon->cdis <= MAX_SIGHT))
    {
        if (disturb_minor)
        {
            msg_print("You hear heavy steps.");
            disturb(0, 0);
        }
    }

    /* Some monsters can speak */
    if ((ap_r_ptr->flags2 & RF2_CAN_SPEAK) && aware && is_aware(mon) &&
        mon->id != p_ptr->riding &&
        one_in_(SPEAK_CHANCE) &&
        plr_los(mon->pos) &&
        plr_project(mon->pos) )
    {
        char m_name[80];
        char monmessage[1024];
        cptr filename;

        /* Acquire the monster name/poss */
        if (mon->ml)
            monster_desc(m_name, mon, 0);
        else
            strcpy(m_name, "It");

        /* Select the file for monster quotes */
        if (mon_tim_find(mon, T_FEAR))
            filename = "monfear.txt";
        else if (is_friendly(mon))
            filename = "monfrien.txt";
        else
            filename = "monspeak.txt";

        if (get_rnd_line(filename, mon->ap_r_idx, monmessage) == 0)
        {
            msg_format("<color:g>%^s</color> %s", m_name, monmessage);
            msg_boundary();
        }
    }

    /* Try to cast spell occasionally */
    if (r_ptr->spells)
    {
        int freq = r_ptr->spells->freq;
        pack_info_t *pack_ptr = pack_info_ptr(mon->id);
        bool blocked = FALSE;

        /* XXX Block spells occasionally if the monster just cast (EXPERIMENTAL)
         * Here, were are attempting to prevent long runs of consecutive casts for
         * melee characters (anger=0). Of course, Nodens should still spell every
         * turn! See ^A"F for analysis ... This is approach II. */
        if (!mon->anger && mon->mana > 0 && freq <= 50 && !one_in_(1 + mon->mana))
            blocked = TRUE;

        /* Increase spell frequency for pack AI or player glyphs of warding */
        if (p_ptr->dun_id == cave->dun_id && is_glyph_grid(cave_at(p_ptr->pos)))
        {
            freq = MAX(30, freq + 10);
        }
        else if (pack_ptr)
        {
            switch (pack_ptr->ai)
            {
            case AI_SHOOT:
                freq = MAX(30, freq + 15);
                break;
            case AI_MAINTAIN_DISTANCE:
                freq += MIN(freq/2, 15);
                break;
            case AI_LURE:
            case AI_FEAR:
            case AI_GUARD_POS:
                freq += MIN(freq/2, 10);
                break;
            }
        }

        /* Angry monsters will eventually spell if they get too pissed off.
         * Monsters are angered by distance attacks (spell casters/archers) */
        freq += mon->anger;
        if (freq > 100) freq = 100;

        /* XXX Adapt spell frequency down if monster is stunned (EXPERIMENTAL)
         * Sure, stunning effects fail rates, but not on innate spells (breaths).
         * In fact, distance stunning gives no benefit against big breathers ...
         * Try a sprite mindcrafter and you'll see what I mean. */
        if (mon_tim_find(mon, T_STUN) && freq < 100)
        {
            int s = mon_tim_amount(mon, T_STUN);
            int p = MAX(0, 100 - s);
            freq = freq * p / 100;
            if (freq < 1) freq = 1;
        }

        /* Hack for Rage Mage Anti-magic Ray ... */
        if (!blocked && mon->anti_magic_ct)
        {
            char m_name[80];
            monster_desc(m_name, mon, 0);

            /* Monsters that spell 1 in 1 get a save on each action. Make the
               save and the spell is broken. Fail, and a turn is lost. */
            if (r_ptr->spells->freq == 100)
            {
                if (!mon_save_p(mon->r_idx, A_STR))
                {
                    if (mon_show_msg(mon))
                        msg_format("%^s tries to break your anti-magic ray but fails.", m_name);
                    mon->anti_magic_ct--;
                    return;
                }
                else
                {
                    if (mon_show_msg(mon))
                        msg_format("%^s breaks your anti-magic ray.", m_name);
                    mon->anti_magic_ct = 0;
                }
            }
            /* Other monsters continue to take a move, but can't cast spells */
            else
            {
                blocked = TRUE;
                mon->anti_magic_ct--;
            }
        }

        if (!blocked && randint1(100) <= freq)
        {
            bool counterattack = FALSE;

            /* Give priority to counter attack? */
            if (dun_pos_interior(cave, mon->target))
            {
                mon_ptr tgt_mon = mon_at(mon->target);

                /* The monster must be an enemy, and projectable */
                if (tgt_mon && are_enemies(mon, tgt_mon) && point_project(mon->pos, tgt_mon->pos))
                    counterattack = TRUE;
            }

            if (!counterattack)
            {
                /* Attempt to cast a spell */
                if (aware && mon_spell_cast(mon, NULL))
                {
                    mon->anger = 0;
                    mon->mana++;
                    return;
                }
                /*
                 * Attempt to cast a spell at an enemy other than the player
                 * (may slow the game a smidgeon, but I haven't noticed.)
                 */
                if (mon_spell_cast_mon(mon, NULL))
                    return;
            }
            else
            {
                /* Attempt to do counter attack at first */
                if (mon_spell_cast_mon(mon, NULL))
                    return;

                if (aware && mon_spell_cast(mon, NULL))
                    return;
            }
        }
    }

    /* XXX Regain mana (EXPERIMENTAL) */
    if (mon->mana)
        mon->mana--;

    /* Really, a non-spell turn. The monster may never move, or may be blocked, and we still
       want to track those turns for accurate reporting of spell frequency. Also, don't count
       unless the monster could spell.*/
    if (plr_project(mon->pos))
        mon_lore_move(mon);

    /* Confused -- 100% random */
    if (mon_tim_find(mon, T_CONFUSED) || !aware)
    {
        /* Try four "random" directions */
        mm[0] = mm[1] = mm[2] = mm[3] = 5;
    }

    /* 75% random movement */
    else if (((r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25)) == (RF1_RAND_50 | RF1_RAND_25)) &&
         (randint0(100) < 75))
    {
        /* Memorize flags */
        mon_lore_1(mon, RF1_RAND_50 | RF1_RAND_25);

        /* Try four "random" directions */
        mm[0] = mm[1] = mm[2] = mm[3] = 5;
    }

    /* 50% random movement */
    else if ((r_ptr->flags1 & RF1_RAND_50) &&
                (randint0(100) < 50))
    {
        /* Memorize flags */
        mon_lore_1(mon, RF1_RAND_50);

        /* Try four "random" directions */
        mm[0] = mm[1] = mm[2] = mm[3] = 5;
    }

    /* 25% random movement */
    else if ((r_ptr->flags1 & RF1_RAND_25) &&
                (randint0(100) < 25))
    {
        /* Memorize flags */
        mon_lore_1(mon, RF1_RAND_25);

        /* Try four "random" directions */
        mm[0] = mm[1] = mm[2] = mm[3] = 5;
    }

    /* Can't reach player - find something else to hit */
    else if ((r_ptr->flags1 & RF1_NEVER_MOVE) && (mon->cdis > 1))
    {
        /* Try four "random" directions */
        mm[0] = mm[1] = mm[2] = mm[3] = 5;

        /* Look for an enemy */
#if 0  /* Hack - Too slow. Mimic pits are horrible with this on. */
        get_enemy_dir(mon->id, mm);
#endif /* 0 */
    }

    /* Pets will follow the player */
    else if (is_pet(mon))
    {
        /* Are we trying to avoid the player? */
        bool avoid = ((p_ptr->pet_follow_distance < 0) &&
                          (mon->cdis <= (0 - p_ptr->pet_follow_distance)));

        /* Do we want to find the player? */
        bool lonely = (!avoid && (mon->cdis > p_ptr->pet_follow_distance));

        /* Should we find the player if we can't find a monster? */
        bool distant = (mon->cdis > PET_SEEK_DIST);

        if (mon->dun_id != p_ptr->dun_id)
        {
            avoid = FALSE;
            lonely = TRUE;
        }

        /* by default, move randomly */
        mm[0] = mm[1] = mm[2] = mm[3] = 5;

        /* Look for an enemy */
        if (!get_enemy_dir(mon, mm))
        {
            /* Find the player if necessary */
            if (avoid || lonely || distant)
            {
                /* Remember the leash length */
                int dis = p_ptr->pet_follow_distance;

                /* Hack -- adjust follow distance temporarily */
                if (p_ptr->pet_follow_distance > PET_SEEK_DIST)
                {
                    p_ptr->pet_follow_distance = PET_SEEK_DIST;
                }

                /* Find the player */
                get_moves(mon, mm);

                /* Restore the leash */
                p_ptr->pet_follow_distance = dis;
            }
        }
    }

    /* Friendly monster movement */
    else if (!is_hostile(mon))
    {
        /* XXX Perhaps friendly monsters should join up with the
         * player? This would make them more interesting/useful
         * (e.g. Gandalf). The random stuff just looks silly
         * when there are no enemies around. */
        if (!get_enemy_dir(mon, mm)/* && !get_moves(mon, mm) */)
            mm[0] = mm[1] = mm[2] = mm[3] = 5;
    }
    /* Normal movement */
    else
    {
        /* Logical moves, may do nothing */
        if (!get_moves(mon, mm)) return;
    }

    /* Assume nothing */
    do_turn = FALSE;
    do_move = FALSE;
    do_view = FALSE;
    must_alter_to_move = FALSE;

    /* Assume nothing */
    did_open_door = FALSE;
    did_bash_door = FALSE;
    did_take_item = FALSE;
    did_kill_item = FALSE;
    did_move_body = FALSE;
    did_pass_wall = FALSE;
    did_kill_wall = FALSE;


    /* Take a zero-terminated array of "directions" */
    for (i = 0; mm[i]; i++)
    {
        bool    diagonal; /* diagonal movement will cost more energy */
        point_t old_pos = mon->pos;
        point_t new_pos;
        mon_ptr y_ptr;

        /* Get the direction */
        d = mm[i];
        if (d == 5) d = ddd[randint0(8)];

        /* Get the destination */
        new_pos = point_step(old_pos, d);
        diagonal = ddx[d] && ddy[d];

        /* Ignore locations off of edge */
        if (!dun_pos_interior(cave, new_pos)) continue;

        /* Access that cave grid */
        c_ptr = cave_at(new_pos);
        f_ptr = &f_info[c_ptr->feat];
        can_cross = monster_can_cross_terrain(c_ptr->feat, r_ptr, is_riding_mon ? CEM_RIDING : 0);

        /* Access that cave grid's contents */
        y_ptr = mon_at(new_pos);

        /* Hack -- player 'in' wall */
        if (plr_at(new_pos))
            do_move = TRUE;

        /* Possibly a monster to attack */
        else if (y_ptr)
            do_move = TRUE;

        /* Monster destroys walls (and doors) */
        else if ((r_ptr->flags2 & RF2_KILL_WALL) &&
                 (can_cross ? !have_flag(f_ptr->flags, FF_LOS) : !is_riding_mon) &&
                 have_flag(f_ptr->flags, FF_HURT_DISI) && !have_flag(f_ptr->flags, FF_PERMANENT) &&
                 check_hp_for_feat_destruction(f_ptr, mon))
        {
            /* Eat through walls/doors/rubble */
            do_move = TRUE;
            if (!can_cross) must_alter_to_move = TRUE;

            /* Monster destroyed a wall (later) */
            did_kill_wall = TRUE;
        }

        /* Floor is open? (XXX Passwall monsters as well, right?) */
        else if (can_cross)
        {
            /* Nerf ASC a bit */
            if (mon_has_summon_spell(mon) && p_ptr->dun_id == cave->dun_id)
            {
                if ( p_ptr->chp > p_ptr->mhp * 4 / 5 /* If @ wounded, pursue! */
                  && !plr_at(new_pos)
                  && plr_los(new_pos) /* plr stepping into los */
                  && plr_project(new_pos) 
                  && !plr_project(mon->pos) )
                {
                    int ct_open = 0;
                    int ct_enemy = 0;
                    point_t d;

                    /* Inspect @'s surroundings */
                    for (d.y = -2; d.y <= 2; d.y++)
                    {
                        for (d.x = -2; d.x <= 2; d.x++)
                        {
                            point_t p = point_add(p_ptr->pos, d);
                            mon_ptr tmon;

                            if (!dun_pos_interior(cave, p)) continue;
                            if (plr_distance(p) > 2) continue;
                            if (cave_have_flag_at(p, FF_PATTERN)) continue;
                            if (point_equals(p, new_pos)) continue;
                            if (point_equals(p, mon->pos)) continue;

                            tmon = mon_at(p);
                            if (tmon && is_hostile(tmon)) ct_enemy++;
                            if (cave_empty_at(p) && plr_project(p)) ct_open++;
                        }
                    }

                    if (ct_enemy)
                    {
                        /* If @ is in battle, join the fray! */
                    }
                    else if (ct_open < 1 + randint1(4))
                    {
                        /* not enough summoning opportunities, so hold off unless angered */
                        if (!mon->anger || !one_in_(3))
                        {
                            asc_nerf = TRUE;
                            could_splash = mon_could_splash(mon, p_ptr->pos);
                            continue;
                        }
                    }
                }
            }

            /* Go ahead and move */
            do_move = TRUE;

            /* Monster moves through walls (and doors) */
            if ((r_ptr->flags2 & RF2_PASS_WALL) && (!is_riding_mon || p_ptr->pass_wall) &&
                have_flag(f_ptr->flags, FF_CAN_PASS))
            {
                /* Monster went through a wall */
                did_pass_wall = TRUE;
            }
        }

        /* Handle doors and secret doors */
        else if (is_closed_door(c_ptr->feat))
        {
            bool may_bash = TRUE;

            /* Assume no move allowed */
            do_move = FALSE;

            /* Creature can open doors. */
            if ((r_ptr->flags2 & RF2_OPEN_DOOR) && have_flag(f_ptr->flags, FF_OPEN) &&
                 (!is_pet(mon) || (p_ptr->pet_extra_flags & PF_OPEN_DOORS)))
            {
                /* Closed doors */
                if (!f_ptr->power)
                {
                    /* The door is open */
                    did_open_door = TRUE;

                    /* Do not bash the door */
                    may_bash = FALSE;

                    /* Take a turn */
                    do_turn = TRUE;
                }

                /* Locked doors (not jammed) */
                else
                {
                    /* Try to unlock it XXX XXX XXX */
                    if (randint0(mon->hp / 10) > f_ptr->power || p_ptr->action == ACTION_GLITTER)
                    {
                        /* Unlock the door */
                        cave_alter_feat(new_pos.y, new_pos.x, FF_DISARM);

                        /* Do not bash the door */
                        may_bash = FALSE;

                        /* Take a turn */
                        do_turn = TRUE;
                    }
                }
            }

            /* Stuck doors -- attempt to bash them down if allowed */
            if (may_bash && (r_ptr->flags2 & RF2_BASH_DOOR) && have_flag(f_ptr->flags, FF_BASH) &&
                (!is_pet(mon) || (p_ptr->pet_extra_flags & PF_OPEN_DOORS)))
            {
                /* Attempt to Bash XXX XXX XXX */
                if (check_hp_for_feat_destruction(f_ptr, mon) && (randint0(mon->hp / 10) > f_ptr->power))
                {
                    /* Disturb (sometimes) */
                    if (disturb_minor)
                    {
                        if (have_flag(f_ptr->flags, FF_GLASS))
                            msg_print("You hear a glass was crashed!");
                        else
                            msg_print("You hear a door burst open!");

                        disturb(0, 0);
                    }

                    /* The door was bashed open */
                    did_bash_door = TRUE;

                    /* Hack -- fall into doorway */
                    do_move = TRUE;
                    must_alter_to_move = TRUE;
                }
            }


            /* Deal with doors in the way */
            if (did_open_door || did_bash_door)
            {
                /* Break down the door */
                if (did_bash_door && ((randint0(100) < 50) || (feat_state(c_ptr->feat, FF_OPEN) == c_ptr->feat) || have_flag(f_ptr->flags, FF_GLASS)))
                {
                    cave_alter_feat(new_pos.y, new_pos.x, FF_BASH);

                    if (mon_is_dead(mon)) /* Killed by shards of glass, etc. */
                    {
                        /* Update some things */
                        p_ptr->update |= (PU_FLOW);
                        p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
                        mon_lore_2(mon, RF2_BASH_DOOR);

                        return;
                    }
                }

                /* Open the door */
                else
                {
                    cave_alter_feat(new_pos.y, new_pos.x, FF_OPEN);
                }

                f_ptr = &f_info[c_ptr->feat];

                /* Handle viewable doors */
                do_view = TRUE;
            }
        }

        /* Hack -- check for Glyph of Warding */
        if (do_move && is_glyph_grid(c_ptr) &&
            !((r_ptr->flags1 & RF1_NEVER_BLOW) && plr_at(new_pos)))
        {
            /* Assume no move allowed */
            do_move = FALSE;

            /* Break the ward */
            if (!is_pet(mon) && (randint1(BREAK_GLYPH) < r_ptr->level))
            {
                /* Describe observable breakage */
                if (c_ptr->info & CAVE_MARK)
                    msg_print("The rune of protection is broken!");

                /* Forget the rune */
                c_ptr->info &= ~(CAVE_MARK);

                /* Break the rune */
                c_ptr->info &= ~(CAVE_OBJECT);
                c_ptr->mimic = 0;

                /* Allow movement */
                do_move = TRUE;

                /* Notice */
                dun_note_pos(cave, new_pos);
            }
        }
        else if (do_move && is_mon_trap_grid(c_ptr) &&
             !((r_ptr->flags1 & RF1_NEVER_BLOW) && plr_at(new_pos)))
        {
            do_move = FALSE;

            if (!is_pet(mon))
            {
                point_t p = mon->pos;
                hit_mon_trap(new_pos.y, new_pos.x, mon->id);

                /* Killed Monster? */
                if (mon_is_dead(mon)) return;

                /* Teleported Monster? */
                if (!point_equals(mon->pos, p)) return;

                /* Slept/Confused/Stunned monster? */
                if (mon_tim_find(mon, T_CONFUSED) || mon_tim_find(mon, T_STUN) || mon_tim_find(mon, MT_SLEEP)) return;

                /* Bug with traps that summon?? Better to cancel the move until I can figure out the issue.  */
                do_move = FALSE;
            }
        }

        /* The player is in the way */
        if (do_move && plr_at(new_pos))
        {
            /* Some monsters never attack */
            if (r_ptr->flags1 & RF1_NEVER_BLOW)
            {
                /* Hack -- memorize lack of attacks */
                mon_lore_1(mon, RF1_NEVER_BLOW);

                /* Do not move */
                do_move = FALSE;
            }

            /* In anti-melee dungeon, stupid or confused monster takes useless turn */
            if (do_move && (cave->flags & DF_NO_MELEE))
            {
                if (!mon_tim_find(mon, T_CONFUSED))
                {
                    if (!(r_ptr->flags2 & RF2_STUPID)) do_move = FALSE;
                    else
                    {
                        mon_lore_2(mon, RF2_STUPID);
                    }
                }
            }

            /* The player is in the way. Attack him. */
            if (do_move)
            {
                /* XXX if (!p_ptr->riding || one_in_(2))
                 * mon_attack_begin now makes this decision */
                {
                    /* Do the attack */
                    mon_attack(mon, new_pos);
                    if ((r_ptr->flags2 & RF2_INVISIBLE) && p_ptr->see_inv && !mon->ml)
                        update_mon(mon, FALSE);

                    /* Do not move */
                    do_move = FALSE;

                    /* Took a turn */
                    do_turn = TRUE;
                }
            }
        }

        /* A monster is in the way */
        if (do_move && y_ptr)
        {
            monster_race *z_ptr = mon_race(y_ptr);

            /* Assume no movement */
            do_move = FALSE;

            /* Attack 'enemies' */
            if (((r_ptr->flags2 & RF2_KILL_BODY) && !(r_ptr->flags1 & RF1_NEVER_BLOW) &&
                 (r_ptr->mexp * r_ptr->level > z_ptr->mexp * z_ptr->level) &&
                 can_cross && (y_ptr->id != p_ptr->riding)) ||
                are_enemies(mon, y_ptr) || mon_tim_find(mon, T_CONFUSED))
            {
                if (!(r_ptr->flags1 & RF1_NEVER_BLOW))
                {
                    if (r_ptr->flags2 & RF2_KILL_BODY)
                    {
                        mon_lore_2(mon, RF2_KILL_BODY);
                    }

                    /* attack */
                    if (y_ptr->r_idx && (y_ptr->hp >= 0))
                    {
                        if (mon_attack(mon, new_pos)) return;

                        /* In anti-melee dungeon, stupid or confused monster takes useless turn */
                        else if (cave->flags & DF_NO_MELEE)
                        {
                            if (mon_tim_find(mon, T_CONFUSED)) return;
                            else if (r_ptr->flags2 & RF2_STUPID)
                            {
                                mon_lore_2(mon, RF2_STUPID);
                                return;
                            }
                        }
                    }
                }
            }

            /* Push past weaker monsters (unless leaving a wall) */
            else if ((r_ptr->flags2 & RF2_MOVE_BODY) && !(r_ptr->flags1 & RF1_NEVER_MOVE) &&
                (r_ptr->mexp > z_ptr->mexp) &&
                can_cross && (y_ptr->id != p_ptr->riding) &&
                monster_can_cross_terrain(cave_at(mon->pos)->feat, z_ptr, 0))
            {
                /* Allow movement */
                do_move = TRUE;

                /* Monster pushed past another monster */
                did_move_body = TRUE;

                /* Wake up the moved monster */
                mon_tim_remove(y_ptr, MT_SLEEP);
            }
        }

        if (is_riding_mon)
        {
            if (!p_ptr->riding_ryoute && !mon_tim_find(dun_mon(cave, p_ptr->riding), T_FEAR)) do_move = FALSE;
        }

        /* radius 1 disintegration effect for powerful monsters (and lava for Gothmog) */
        if (r_ptr->level > 70 && do_move && r_ptr->flags2 & RF2_KILL_WALL)
        {
            int dir;
            cave_type       *c_ptr;
            feature_type    *f_ptr;
            for (dir = 0; dir < 8; dir++)
            {
                point_t p = point_step(new_pos, ddd[dir]);
                if (!dun_pos_interior(cave, p)) continue;

                c_ptr = cave_at(p);
                f_ptr = &f_info[c_ptr->feat];

                if (have_flag(f_ptr->flags, FF_PERMANENT)) continue;

                cave_alter_feat(p.y, p.x, FF_HURT_DISI);
                if (mon->r_idx == MON_GOTHMOG)
                {
                    if (c_ptr->feat != feat_deep_lava)
                        cave_set_feat(p.y, p.x, feat_shallow_lava);
                }
            }

            if (mon->r_idx == MON_GOTHMOG && !cave_have_flag_at(new_pos, FF_PERMANENT))
                cave_set_feat(new_pos.y, new_pos.x, feat_deep_lava);
        }
        /* Fangorn and Ents */
        if (do_move && (mon->r_idx == MON_FANGORN || mon->r_idx == MON_ENT))
        {
            int dir;
            cave_type       *c_ptr;
            feature_type    *f_ptr;
            for (dir = 0; dir < 8; dir++)
            {
                point_t p = point_step(new_pos, ddd[dir]);
                if (!dun_pos_interior(cave, p)) continue;

                c_ptr = cave_at(p);
                f_ptr = &f_info[c_ptr->feat];

                if (!have_flag(f_ptr->flags, FF_FLOOR)) continue;
                if ( c_ptr->feat != feat_tree
                  && c_ptr->feat != feat_flower
                  && c_ptr->feat != feat_brake )
                {
                    switch (randint1(5))
                    {
                    case 1: cave_set_feat(p.y, p.x, feat_flower); break;
                    case 2: cave_set_feat(p.y, p.x, feat_brake); break;
                    default: cave_set_feat(p.y, p.x, feat_grass);
                    }
                }
            }
            if (cave_have_flag_at(new_pos, FF_FLOOR))
                cave_set_feat(new_pos.y, new_pos.x, feat_tree);
        }
        /* Spiders */
        if (do_move && (mon->r_idx == MON_UNGOLIANT || mon->r_idx == MON_ATLACH_NACHA))
        {
            int dir;
            cave_type       *c_ptr;
            feature_type    *f_ptr;
            for (dir = 0; dir < 8; dir++)
            {
                point_t p = point_step(new_pos, ddd[dir]);
                if (!dun_pos_interior(cave, p)) continue;

                c_ptr = cave_at(p);
                f_ptr = &f_info[c_ptr->feat];

                if (!have_flag(f_ptr->flags, FF_FLOOR)) continue;
                if (c_ptr->feat != feat_web && randint0(150) < r_ptr->level)
                    cave_set_feat(p.y, p.x, feat_web);
            }
            if (cave_have_flag_at(new_pos, FF_FLOOR))
                cave_set_feat(new_pos.y, new_pos.x, feat_web);
        }

        if (did_kill_wall && do_move)
        {
            if (one_in_(GRINDNOISE))
            {
                if (have_flag(f_ptr->flags, FF_GLASS))
                    msg_print("There is a crashing sound.");
                else
                    msg_print("There is a grinding sound.");
            }


            cave_alter_feat(new_pos.y, new_pos.x, FF_HURT_DISI);

            if (mon_is_dead(mon)) /* Killed by shards of glass, etc. */
            {
                /* Update some things */
                p_ptr->update |= (PU_FLOW);
                p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
                mon_lore_2(mon, RF2_KILL_WALL);

                return;
            }

            f_ptr = &f_info[c_ptr->feat];

            /* Note changes to viewable region */
            do_view = TRUE;

            /* Take a turn */
            do_turn = TRUE;
        }

        if (must_alter_to_move && (r_ptr->flags7 & RF7_AQUATIC))
        {
            if (!monster_can_cross_terrain(c_ptr->feat, r_ptr, is_riding_mon ? CEM_RIDING : 0))
            {
                /* Assume no move allowed */
                do_move = FALSE;
            }
        }

        /*
         * Check if monster can cross terrain
         * This is checked after the normal attacks
         * to allow monsters to attack an enemy,
         * even if it can't enter the terrain.
         */
        if (do_move && !can_cross && !did_kill_wall && !did_bash_door)
        {
            /* Assume no move allowed */
            do_move = FALSE;
        }

        /* Some monsters never move */
        if (do_move && (r_ptr->flags1 & RF1_NEVER_MOVE))
        {
            /* Hack -- memorize lack of moves */
            mon_lore_1(mon, RF1_NEVER_MOVE);

            /* Do not move */
            do_move = FALSE;
            if ((r_ptr->flags2 & RF2_INVISIBLE) && p_ptr->see_inv)
                update_mon(mon, FALSE);
        }

        /* Creature has been allowed move */
        if (do_move)
        {
            /* Take a turn */
            do_turn = TRUE;

            if (have_flag(f_ptr->flags, FF_TREE))
            {
                if (!(r_ptr->flags7 & RF7_CAN_FLY) && !(r_ptr->flags8 & RF8_WILD_WOOD))
                {
                    mon->energy_need += ENERGY_NEED();
                }
            }
            if (have_flag(f_ptr->flags, FF_WEB) && r_ptr->d_char != 'S')
            {
                mon->energy_need += ENERGY_NEED();
                /* Monster hacks web to bits? We assume at the moment that only
                 * the player (in particual, a Spider player monster race) can
                 * produce webs. */
                if (!(r_ptr->flags2 & RF2_PASS_WALL) && mon_save_p(mon->r_idx, A_NONE))
                {
                    cave_set_feat(new_pos.y, new_pos.x, dun_type()->floor_type[randint0(100)]);
                }
            }

            if (!is_riding_mon)
            {
                dun_move_mon(cave, mon, new_pos);
                dun_lite_pos(cave, old_pos);
                dun_lite_pos(cave, new_pos);

                /* XXX (Proposed): Diagonal movement costs 50% more energy */
                if (diagonal)
                    mon->energy_need += ENERGY_NEED() / 2;

                /* Some classes have techniques to apply whenever a monster moves too close */
                plr_hook_move_monster(mon);
                if (mon_is_dead(mon)) return;
            }
            else
            {
                /* Moving the player will handle moving mon as well */
                if (!move_player_effect(new_pos, MPE_DONT_PICKUP)) break;
            }

            /* Possible disturb */
            if (mon->ml &&
                (disturb_move ||
                 (mon->cdis <= 2 && plr_project(mon->pos)) ||
                 (disturb_near && plr_project(mon->pos)) ||
                 (disturb_high && ap_r_ptr->r_tkills && ap_r_ptr->level >= p_ptr->lev)))
            {
                if (town_no_disturb && plr_in_town() && r_ptr->level == 0)
                {
                }
                else if (is_hostile(mon))
                    disturb(0, 0);
            }

            /* Take or Kill objects on the floor */
            if (r_ptr->flags2 & RF2_KILL_ITEM)
            {
                if (!is_pet(mon))
                    dun_mon_destroy(cave, mon);
            }
            else if (r_ptr->flags2 & RF2_TAKE_ITEM)
            {
                if (!is_pet(mon) || (p_ptr->pet_extra_flags & PF_PICKUP_ITEMS))
                    dun_mon_pickup(cave, mon);
            }
        }

        /* Stop when done */
        if (do_turn) break;
    }

    /*
     *  Forward movements failed, but now received LOS attack!
     *  Try to flow by smell.
     */
    if (p_ptr->no_flowed && i > 2 &&  dun_pos_interior(cave, mon->target))
        mon->mflag2 &= ~MFLAG2_NOFLOW;

    /* Lore Issues wrt ASC: Monsters often hang back out of los until they choose
     * a splash spell. We aren't counting the moves, but we are counting the spells,
     * so the reported frequency is too high. */
    if (asc_nerf && could_splash && do_turn && do_move && !plr_project(mon->pos))
    {
        /*msg_print("<color:v>asc mon_lore_move</color>");*/
        mon_lore_move(mon);
    }

    /* Notice changes in view */
    if (do_view)
    {
        /* Update some things */
        p_ptr->update |= (PU_FLOW);

        /* Window stuff */
        p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
    }

    /* Notice changes in view */
    if (do_move && ((r_ptr->flags7 & (RF7_SELF_LD_MASK | RF7_HAS_DARK_1 | RF7_HAS_DARK_2))
        || ((r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)))))
    {
        /* Update some things */
        p_ptr->update |= (PU_MON_LITE);
    }

    /* Learn things from observable monster */
    if (is_original_ap_and_seen(mon))
    {
        /* Monster opened a door */
        if (did_open_door) mon_lore_aux_2(r_ptr, RF2_OPEN_DOOR);

        /* Monster bashed a door */
        if (did_bash_door) mon_lore_aux_2(r_ptr, RF2_BASH_DOOR);

        /* Monster tried to pick something up */
        if (did_take_item) mon_lore_aux_2(r_ptr, RF2_TAKE_ITEM);

        /* Monster tried to crush something */
        if (did_kill_item) mon_lore_aux_2(r_ptr, RF2_KILL_ITEM);

        /* Monster pushed past another monster */
        if (did_move_body) mon_lore_aux_2(r_ptr, RF2_MOVE_BODY);

        /* Monster passed through a wall */
        if (did_pass_wall) mon_lore_aux_2(r_ptr, RF2_PASS_WALL);

        /* Monster destroyed a wall */
        if (did_kill_wall) mon_lore_aux_2(r_ptr, RF2_KILL_WALL);
    }


    /* Hack -- get "bold" if out of options */
    if (!do_turn && !do_move && mon_tim_find(mon, T_FEAR) && aware)
    {
        /* No longer afraid */
        mon_tim_delete(mon, T_FEAR);

        /* Message if seen */
        if (see_m)
        {
            char m_name[80];

            /* Acquire the monster name */
            monster_desc(m_name, mon, 0);

            /* Dump a message */
            msg_format("%^s turns to fight!", m_name);
        }

        if (mon->ml) virtue_add(VIRTUE_COMPASSION, -1);

        /* XXX XXX XXX Actually do something now (?) */
    }
}

void dispel_monster_status(int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    mon_tim_dispel(m_ptr);
}

bool process_the_world(int num, int who, bool vs_player)
{
    monster_type *m_ptr = dun_mon(cave, hack_m_idx);  /* the world monster */

    if (world_monster) return (FALSE);

    if(vs_player)
    {
        char m_name[80];
        monster_desc(m_name, m_ptr, 0);

        if (who == 1)
            msg_format("%s yells 'The World! Time has stopped!'", m_name);
        else if (who == 3)
            msg_format("%s yells 'Time!'", m_name);
        else msg_print("hek!");

        if (p_ptr->pclass == CLASS_TIME_LORD && !mon_save_p(m_ptr->r_idx, A_WIS))
        {
            msg_format("You grin as time begins to flow once again!");
            return TRUE;
        }

        msg_print(NULL);
    }

    /* This monster cast spells */
    world_monster = hack_m_idx;

    if (vs_player) do_cmd_redraw();

    while(num--)
    {
        if(mon_is_dead(m_ptr)) break;
        process_monster(m_ptr);

        reset_target(m_ptr);

        /* Notice stuff */
        if (p_ptr->notice) notice_stuff();

        /* Update stuff */
        if (p_ptr->update) update_stuff();

        /* Redraw stuff */
        if (p_ptr->redraw) redraw_stuff();

        /* Redraw stuff */
        if (p_ptr->window) window_stuff();

        /* Delay */
        if (vs_player) Term_xtra(TERM_XTRA_DELAY, 500);
    }

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

    /* Update monsters */
    p_ptr->update |= (PU_MONSTERS);

    /* Window stuff */
    p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

    world_monster = 0;
    if (vs_player || (plr_los(m_ptr->pos) && plr_project(m_ptr->pos)))
    {
        msg_print("You feel time flowing around you once more.");
        msg_print(NULL);
    }

    handle_stuff();

    return (TRUE);
}

void mon_gain_exp(mon_ptr mon, int amt)
{
    mon_race_ptr race = mon_race(mon);
    if (!race->next_exp) return;
    mon->exp += amt;
    if (0 || p_ptr->wizard)
        msg_format("<color:D>mon_gain_exp %d (needs %d).</color>", mon->exp, race->next_exp);

    if (mon->mflag2 & MFLAG2_CHAMELEON) return;

    if (mon->exp >= race->next_exp)
    {
        char m_name[80];
        int old_hp = mon->hp;
        int old_maxhp = mon->max_maxhp;
        int old_r_idx = mon->r_idx;
        byte old_sub_align = mon->sub_align;

        /* Hack -- Reduce the racial counter of previous monster */
        assert(real_r_ptr(mon)->cur_num > 0);
        real_r_ptr(mon)->cur_num--;

        monster_desc(m_name, mon, 0);
        mon->r_idx = race->next_r_idx;

        /* Count the monsters on the level */
        real_r_ptr(mon)->cur_num++;

        mon->ap_r_idx = mon->r_idx;
        race = mon_race(mon);

        if (race->flags1 & RF1_FORCE_MAXHP)
        {
            mon->max_maxhp = maxroll(race->hdice, race->hside);
        }
        else
        {
            mon->max_maxhp = damroll(race->hdice, race->hside);
        }
        mon->maxhp = mon->max_maxhp;
        mon->hp = old_hp * mon->maxhp / old_maxhp;

        /* Extract the monster base speed */
        mon->mspeed = get_mspeed(race);

        /* Sub-alignment of a monster */
        if (!is_pet(mon) && !(race->flags3 & (RF3_EVIL | RF3_GOOD)))
            mon->sub_align = old_sub_align;
        else
        {
            mon->sub_align = SUB_ALIGN_NEUTRAL;
            if (race->flags3 & RF3_EVIL) mon->sub_align |= SUB_ALIGN_EVIL;
            if (race->flags3 & RF3_GOOD) mon->sub_align |= SUB_ALIGN_GOOD;
        }

        mon->exp = 0;

        if (is_pet(mon) || mon->ml)
        {
            if (!ignore_unview || plr_can_see(mon->pos))
            {
                if (plr_tim_find(T_HALLUCINATE))
                {
                    monster_race *hallu_race;

                    do
                    {
                        hallu_race = &r_info[randint1(max_r_idx - 1)];
                    }
                    while (!hallu_race->name || (hallu_race->flags1 & RF1_UNIQUE));

                    msg_format("%^s evolved into %s.", m_name, r_name + hallu_race->name);
                }
                else
                {
                    msg_format("%^s evolved into %s.", m_name, r_name + race->name);
                    if (race->r_sights < MAX_SHORT) race->r_sights++;
                }
            }

            if (!plr_tim_find(T_HALLUCINATE)) r_info[old_r_idx].r_xtra1 |= MR1_SINKA;

            /* Now you feel very close to this pet. */
            mon_set_parent(mon, 0);
        }
        update_mon(mon, FALSE);
        lite_pos(mon->pos);
    }
    if (mon->id == p_ptr->riding) p_ptr->update |= PU_BONUS;
}

void monster_gain_exp(int m_idx, int s_idx)
{
    monster_type *m_ptr;
    monster_race *r_ptr;
    monster_race *s_ptr;
    int new_exp;

    /* Paranoia */
    if (m_idx <= 0 || s_idx <= 0) return;

    m_ptr = dun_mon(cave, m_idx);

    /* Paranoia -- Skip dead monsters */
    if (mon_is_dead(m_ptr)) return;

    r_ptr = mon_race(m_ptr);
    s_ptr = mon_race_lookup(s_idx);

    new_exp = s_ptr->mexp * s_ptr->level / (r_ptr->level + 2);
    if (m_idx == p_ptr->riding) new_exp = (new_exp + 1) / 2;
    if (cave->dun_type_id == D_SURFACE) new_exp /= 5;

    /* Experimental: Share the xp with the player */
    if (is_pet(m_ptr))
    {
        int  div = 5;
        bool penalty = TRUE;
        int  exp;

        if ( prace_is_(RACE_MON_QUYLTHULG)
          || (prace_is_(RACE_MON_RING) && p_ptr->riding == m_idx) )
        {
            div = 1;
            penalty = FALSE;
        }

        exp = new_exp / div;
        gain_exp(exp);
        if (penalty)
            new_exp -= exp;
        if (new_exp < 0) new_exp = 0;
    }

    mon_gain_exp(m_ptr, new_exp);
}
