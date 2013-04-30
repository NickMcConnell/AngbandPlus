/*
 * File: melee.c
 * Purpose: Monster attacking code and AI routines
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke (attacking code)
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones (AI routines).
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


#include "../s-angband.h"
#include "../../common/tvalsval.h"
#include "../attack.h"
#include "../init.h"
#include "mon-make.h"
#include "mon-msg.h"
#include "mon-spell.h"
#include "mon-timed.h"
#include "mon-util.h"
#include "../object/slays.h"
#include "../s-spells.h"
#include "../squelch.h"


/*
 * Determine if a bolt will arrive, checking that no monsters are in the way
 */
#define clean_shot(DPTH, Y1, X1, Y2, X2) \
    projectable(DPTH, Y1, X1, Y2, X2, PROJECT_STOP)


/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Give monsters more intelligent attack/spell selection based on
 * observations of previous attacks on the player, and/or by allowing
 * the monster to "cheat" and know the player status.
 *
 * Maintain an idea of the player status, and use that information
 * to occasionally eliminate "ineffective" spell attacks.  We could
 * also eliminate ineffective normal attacks, but there is no reason
 * for the monster to do this, since he gains no benefit.
 * Note that MINDLESS monsters are not allowed to use this code.
 * And non-INTELLIGENT monsters only use it partially effectively.
 *
 * Actually learn what the player resists, and use that information
 * to remove attacks or spells before using them.
 *
 * This has the added advantage that attacks and spells are related.
 * The "smart_learn" option means that the monster "learns" the flags
 * that should be set, and "smart_cheat" means that he "knows" them.
 * So "smart_cheat" means that the "smart" field is always up to date,
 * while "smart_learn" means that the "smart" field is slowly learned.
 * Both of them have the same effect on the "choose spell" routine.
 */


/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(int who, int depth, int m_idx, bitflag f[RSF_SIZE])
{
    monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    bitflag f2[RSF_SIZE], ai_flags[OF_SIZE];
    size_t i;
    u32b smart = 0L;

    /* Hack -- MvM */
    if (!who) return;

    /* Stupid monsters act randomly */
    if (rf_has(r_ptr->flags, RF_STUPID)) return;

    /* Take working copy of spell flags */
    rsf_copy(f2, f);

    /* Don't heal if full */
    if (m_ptr->hp >= m_ptr->maxhp) rsf_off(f2, RSF_HEAL);

    /* Don't haste if hasted with time remaining */
    if (m_ptr->m_timed[MON_TMD_FAST] > 10) rsf_off(f2, RSF_HASTE);

    /* Don't teleport to if the player is already next to us */
    if (m_ptr->cdis == 1) rsf_off(f2, RSF_TELE_TO);

    /* Update acquired knowledge */
    of_wipe(ai_flags);
    if (cfg_ai_learn)
    {
        /* Occasionally forget player status */
        if (one_in_(100)) of_wipe(m_ptr->known_pflags);

        /* Use the memorized flags */
        smart = m_ptr->smart;
        of_copy(ai_flags, m_ptr->known_pflags);
    }

    /* Cheat if requested */
    if (cfg_ai_cheat)
    {
        for (i = 0; i < OF_MAX; i++)
        {
            if (check_state(player_get(who), i)) of_on(ai_flags, i);
        }
        if (!player_get(who)->msp) smart |= SM_IMM_MANA;
    }

    /* Cancel out certain flags based on knowledge */
    if (!of_is_empty(ai_flags)) unset_spells(who, f2, ai_flags, r_ptr);
    if ((smart & SM_IMM_MANA) && magik(rf_has(r_ptr->flags, RF_SMART)? 100: 50))
        rsf_off(f2, RSF_DRAIN_MANA);

    /* Use working copy of spell flags */
    rsf_copy(f, f2);
}


/*
 * Determine if there is a space near the selected spot in which
 * a summoned creature can appear
 */
static bool summon_possible(int depth, int y1, int x1)
{
    int y, x;

    /* Start at the location, and check 2 grids in each dir */
    for (y = y1 - 2; y <= y1 + 2; y++)
    {
        for (x = x1 - 2; x <= x1 + 2; x++)
        {
            /* Ignore illegal locations */
            if (!in_bounds(y, x)) continue;

            /* Only check a circular area */
            if (distance(y1, x1, y, x) > 2) continue;

            /* No summon on glyph of warding */
            if (cave_get(depth)->feat[y][x] == FEAT_GLYPH) continue;

            /* Require empty floor grid in line of sight */
            if (cave_empty_bold(depth, y, x) && los(depth, y1, x1, y, x))
                return (TRUE);
        }
    }

    return (FALSE);
}


/*
 * Have a monster choose a spell to cast.
 *
 * Note that the monster's spell list has already had "useless" spells
 * (bolts that won't hit the player, summons without room, etc.) removed.
 * Perhaps that should be done by this function.
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * This function could be an efficiency bottleneck.
 */
static int choose_attack_spell(int depth, int m_idx, int y, int x, bitflag f[RSF_SIZE])
{
    monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    int num = 0;
    byte spells[RSF_MAX];
    int i;
    bool has_escape, has_attack, has_summon, has_tactic;
    bool has_annoy, has_haste, has_heal;

    /* Smart monsters restrict their spell choices. */
    if (cfg_ai_smart && !rf_has(r_ptr->flags, RF_STUPID))
    {
        /* What have we got? */
        has_escape = test_spells(f, RST_ESCAPE);
        has_attack = test_spells(f, RST_ATTACK | RST_BOLT | RST_BALL | RST_BREATH);
        has_summon = test_spells(f, RST_SUMMON);
        has_tactic = test_spells(f, RST_TACTIC);
        has_annoy = test_spells(f, RST_ANNOY);
        has_haste = test_spells(f, RST_HASTE);
        has_heal = test_spells(f, RST_HEAL);

        /*** Try to pick an appropriate spell type ***/

        /* Hurt badly or afraid, attempt to flee */
        if (has_escape && ((m_ptr->hp < m_ptr->maxhp / 4) || m_ptr->m_timed[MON_TMD_FEAR]))
        {
            /* Choose escape spell */
            set_spells(f, RST_ESCAPE);
        }

        /* Still hurt badly, couldn't flee, attempt to heal */
        else if (has_heal && (m_ptr->hp < m_ptr->maxhp / 4))
        {
            /* Choose heal spell */
            set_spells(f, RST_HEAL);
        }

        /* Target is close and we have attack spells, blink away */
        else if (has_tactic && (distance(y, x, m_ptr->fy, m_ptr->fx) < 4) &&
            has_attack && magik(75))
        {
            /* Choose tactical spell */
            set_spells(f, RST_TACTIC);
        }

        /* We're hurt (not badly), try to heal */
        else if (has_heal && (m_ptr->hp < m_ptr->maxhp * 3 / 4) && magik(60))
        {
            /* Choose heal spell */
            set_spells(f, RST_HEAL);
        }

        /* Summon if possible (sometimes) */
        else if (has_summon && magik(50))
        {
            /* Choose summon spell */
            set_spells(f, RST_SUMMON);
        }

        /* Attack spell (most of the time) */
        else if (has_attack && magik(85))
        {
            /* Choose attack spell */
            set_spells(f, RST_ATTACK | RST_BOLT | RST_BALL | RST_BREATH);
        }

        /* Try another tactical spell (sometimes) */
        else if (has_tactic && magik(50))
        {
            /* Choose tactic spell */
            set_spells(f, RST_TACTIC);
        }

        /* Haste self if we aren't already somewhat hasted (rarely) */
        else if (has_haste && magik(20 - m_ptr->m_timed[MON_TMD_FAST]))
        {
            /* Choose haste spell */
            set_spells(f, RST_HASTE);
        }

        /* Annoy player (most of the time) */
        else if (has_annoy && magik(85))
        {
            /* Choose annoyance spell */
            set_spells(f, RST_ANNOY);
        }

        /* Else choose no spell */
        else rsf_wipe(f);

        /* Anything left? */
        if (rsf_is_empty(f)) return (FLAG_END);
    }

    /* Extract all spells: "innate", "normal", "bizarre" */
    for (i = FLAG_START; i < RSF_MAX; i++)
    {
        if (rsf_has(f, i)) spells[num++] = i;
    }

    /* Paranoia */
    if (num == 0) return 0;

    /* Pick at random */
    return (spells[randint0(num)]);
}


/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "TRUE" if a spell (or whatever) was (successfully) cast.
 *
 * XXX XXX XXX This function could use some work, but remember to
 * keep it as optimized as possible, while retaining generic code.
 *
 * Verify the various "blind-ness" checks in the code.
 *
 * XXX XXX XXX Note that several effects should really not be "seen"
 * if the player is blind.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 */
static bool make_attack_spell(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    int thrown_spell;
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
    char m_name[NORMAL_WID], m_poss[NORMAL_WID];

    /* Player position */
    int px = p_ptr->px;
    int py = p_ptr->py;

    /* Extract the blind-ness */
    bool blind = (p_ptr->timed[TMD_BLIND]? TRUE: FALSE);

    /* Extract the "see-able-ness" */
    bool seen = (!blind && p_ptr->mon_vis[m_idx]);

    /* Stop if player is dead or gone */
    if (!p_ptr->alive || p_ptr->is_dead || p_ptr->new_level_flag) return (FALSE);

    /* Choose a spell to cast */
    thrown_spell = get_thrown_spell(Ind, Ind, p_ptr->depth, m_idx, m_ptr->cdis, py, px);

    /* Abort if no spell was chosen */
    if (thrown_spell < 0) return ((thrown_spell == -1)? FALSE: TRUE);

    /* If we see an unaware monster try to cast a spell, become aware of it */
    if (m_ptr->unaware) become_aware(p_ptr, m_ptr);

    /* Get the monster name (or "it") */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(p_ptr, m_poss, sizeof(m_poss), m_ptr, MDESC_PRO2 | MDESC_POSS);

    /* Cast the spell. */
    disturb(p_ptr, 1, 0);

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
    else
        do_mon_spell(Ind, thrown_spell, m_idx, seen);

    /* Remember what the monster did to us */
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

    /* Always take note of monsters that kill you */
    if (p_ptr->is_dead && (l_ptr->pdeaths < MAX_SHORT)) l_ptr->pdeaths++;
    if (p_ptr->is_dead && (r_ptr->lore.tdeaths < MAX_SHORT)) r_ptr->lore.tdeaths++;

    /* A spell was cast */
    return TRUE;
}


/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players.  See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency.  Like not using "mod" or "div" when possible.  And
 * attempting to check the conditions in an optimal order.  Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
static int mon_will_run(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    u16b p_lev, m_lev;
    u16b p_chp, p_mhp;
    u16b m_chp, m_mhp;
    u32b p_val, m_val;

    /* Skip if the monster is not hostile */
    if (master_in_party(m_ptr->master, p_ptr->id)) return (FALSE);

    /* Keep monsters from running too far away */
    if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

    /* All "afraid" monsters will run away */
    if (m_ptr->m_timed[MON_TMD_FEAR]) return (TRUE);

    /* Nearby monsters will not become terrified */
    if (m_ptr->cdis <= 5) return (FALSE);

    /* Examine player power (level) */
    p_lev = p_ptr->lev;

    /* Examine monster power (level plus morale) */
    m_lev = m_ptr->level + (m_idx & 0x08) + 25;

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

    /* Assume no terror */
    return (FALSE);
}


/*
 * Find whether a monster is near a permanent wall.
 * This decides whether PASS_WALL & KILL_WALL monsters use the monster flow code.
 */
static bool near_permwall(struct player *p, const monster_type *m_ptr, struct cave *c)
{
    int y, x;
    int my = m_ptr->fy;
    int mx = m_ptr->fx;

    /* If PC is in LOS, there's no need to go around walls */
    if (projectable_wall(p->depth, my, mx, p->py, p->px)) return FALSE;

    /* PASS_WALL & KILL_WALL monsters occasionally flow for a turn anyway */
    if (randint0(99) < 5) return TRUE;

    /* Search the nearby grids, which are always in bounds */
    for (y = (my - 2); y <= (my + 2); y++)
    {
        for (x = (mx - 2); x <= (mx + 2); x++)
        {
            if (!cave_in_bounds_fully(c, y, x)) continue;

            /* Vault walls are always FEAT_PERM_BASIC */
            if (cave_isperm(c, y, x)) return TRUE;
        }
    }

    return FALSE;
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
 * previous locations occupied by the player.  This will tend
 * to have monsters end up either near the player or on a grid
 * recently occupied by the player (and left via "teleport").
 *
 * Note that if "smell" is turned on, all monsters get vicious.
 *
 * Also note that teleporting away from a location will cause
 * the monsters who were chasing you to converge on that location
 * as long as you are still near enough to "annoy" them without
 * being close enough to chase directly.  I have no idea what will
 * happen if you combine "smell" with low "aaf" values.
 */
static bool get_moves_aux(int Ind, int m_idx, int *yp, int *xp)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    int i, y, x, y1, x1;
    int when = 0;
    int cost = 999;
    struct cave *c = cave_get(p_ptr->depth);
    monster_type *m_ptr = cave_monster(c, m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Monster can go through rocks */
    if (flags_test(r_ptr->flags, RF_SIZE, RF_PASS_WALL, RF_KILL_WALL, FLAG_END))
    {
        /* If monster is near a permwall, use normal pathfinding */
        if (!near_permwall(p_ptr, m_ptr, c)) return (FALSE);
    }

    /* Monster location */
    y1 = m_ptr->fy;
    x1 = m_ptr->fx;

    /* The player is not currently near the monster grid */
    if (p_ptr->cave->when[y1][x1] < p_ptr->cave->when[py][px])
    {
        /* The player has never been near the monster grid */
        if (p_ptr->cave->when[y1][x1] == 0) return (FALSE);

        /* The monster is not allowed to track the player */
        if (!cfg_ai_smell) return (FALSE);
    }

    /* Monster is too far away to notice the player */
    if (p_ptr->cave->cost[y1][x1] > MONSTER_FLOW_DEPTH) return (FALSE);
    if (p_ptr->cave->cost[y1][x1] > (cfg_small_range? (r_ptr->aaf / 2): r_ptr->aaf)) return (FALSE);

    /* Hack -- Player can see us, run towards him */
    if (player_has_los_bold(p_ptr, y1, x1)) return (FALSE);

    /* Check nearby grids, diagonals first */
    for (i = 7; i >= 0; i--)
    {
        /* Get the location */
        y = y1 + ddy_ddd[i];
        x = x1 + ddx_ddd[i];

        /* Ignore illegal locations */
        if (p_ptr->cave->when[y][x] == 0) continue;

        /* Ignore ancient locations */
        if (p_ptr->cave->when[y][x] < when) continue;

        /* Ignore distant locations */
        if (p_ptr->cave->cost[y][x] > cost) continue;

        /* Save the cost and time */
        when = p_ptr->cave->when[y][x];
        cost = p_ptr->cave->cost[y][x];

        /* Hack -- Save the "twiddled" location */
        (*yp) = py + 16 * ddy_ddd[i];
        (*xp) = px + 16 * ddx_ddd[i];
    }

    /* No legal move (?) */
    if (!when) return (FALSE);

    /* Success */
    return (TRUE);
}


/*
 * Provide a location to flee to, but give the player a wide berth.
 *
 * A monster may wish to flee to a location that is behind the player,
 * but instead of heading directly for it, the monster should "swerve"
 * around the player so that he has a smaller chance of getting hit.
 */
static bool get_fear_moves_aux(int Ind, int m_idx, int *yp, int *xp)
{
    player_type *p_ptr = player_get(Ind);
    int y, x, y1, x1, fy, fx, py, px, gy = 0, gx = 0;
    int when = 0, score = -1;
    int i;
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Player location */
    py = p_ptr->py;
    px = p_ptr->px;

    /* Monster location */
    fy = m_ptr->fy;
    fx = m_ptr->fx;

    /* Desired destination */
    y1 = fy - (*yp);
    x1 = fx - (*xp);

    /* The player is not currently near the monster grid */
    if (p_ptr->cave->when[fy][fx] < p_ptr->cave->when[py][px])
    {
        /* No reason to attempt flowing */
        return (FALSE);
    }

    /* Monster is too far away to use flow information */
    if (p_ptr->cave->cost[fy][fx] > MONSTER_FLOW_DEPTH) return (FALSE);
    if (p_ptr->cave->cost[fy][fx] > (cfg_small_range? (r_ptr->aaf / 2): r_ptr->aaf)) return (FALSE);

    /* Check nearby grids, diagonals first */
    for (i = 7; i >= 0; i--)
    {
        int dis, s;

        /* Get the location */
        y = fy + ddy_ddd[i];
        x = fx + ddx_ddd[i];

        /* Ignore illegal locations */
        if (p_ptr->cave->when[y][x] == 0) continue;

        /* Ignore ancient locations */
        if (p_ptr->cave->when[y][x] < when) continue;

        /* Calculate distance of this grid from our destination */
        dis = distance(y, x, y1, x1);

        /* Score this grid */
        s = 5000 / (dis + 3) - 500 / (p_ptr->cave->cost[y][x] + 1);

        /* No negative scores */
        if (s < 0) s = 0;

        /* Ignore lower scores */
        if (s < score) continue;

        /* Save the score and time */
        when = p_ptr->cave->when[y][x];
        score = s;

        /* Save the location */
        gy = y;
        gx = x;
    }

    /* No legal move (?) */
    if (!when) return (FALSE);

    /* Find deltas */
    (*yp) = fy - gy;
    (*xp) = fx - gx;

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
 * coded sizes.  At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()").  XXX XXX XXX
 *
 * Also, the storage needs could be reduced by using char.  XXX XXX XXX
 *
 * These arrays could be combined into two big arrays, using sub-arrays
 * to hold the offsets and lengths of each portion of the sub-arrays, and
 * this could perhaps also be used somehow in the "look" code.  XXX XXX XXX
 */

static const int d_off_y_0[] =
{ 0 };

static const int d_off_x_0[] =
{ 0 };

static const int d_off_y_1[] =
{ -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static const int d_off_x_1[] =
{ -1, 0, 1, -1, 1, -1, 0, 1, 0 };

static const int d_off_y_2[] =
{ -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static const int d_off_x_2[] =
{ -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };

static const int d_off_y_3[] =
{ -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
  3, 3, 3, 0 };

static const int d_off_x_3[] =
{ -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
  -1, 0, 1, 0 };

static const int d_off_y_4[] =
{ -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
  0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0 };

static const int d_off_x_4[] =
{ -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
  -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0 };

static const int d_off_y_5[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
  -5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 0 };

static const int d_off_x_5[] =
{ -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
  -5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
  0, 1, 0 };

static const int d_off_y_6[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
  5, 5, 6, 6, 6, 0 };

static const int d_off_x_6[] =
{ -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
  0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
  -3, 2, 3, -1, 0, 1, 0 };

static const int d_off_y_7[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
  3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0 };

static const int d_off_x_7[] =
{ -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
  -3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
  6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
  1, 0 };

static const int d_off_y_8[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
  2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
  8, 8, 8, 0 };

static const int d_off_x_8[] =
{ -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
  5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
  7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
  2, 3, -1, 0, 1, 0 };

static const int d_off_y_9[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
  0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
  7, 8, 8, 8, 8, 9, 9, 9, 0 };

static const int d_off_x_9[] =
{ -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
  -5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
  9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
  4, 5, -2, -3, 2, 3, -1, 0, 1, 0 };

static const int *dist_offsets_y[10] =
{
    d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
    d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

static const int *dist_offsets_x[10] =
{
    d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
    d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};


/*
 * Choose a "safe" location near a monster for it to run toward.
 *
 * A location is "safe" if it can be reached quickly and the player
 * is not able to fire into it (it isn't a "clean shot").  So, this will
 * cause monsters to "duck" behind walls.  Hopefully, monsters will also
 * try to run towards corridor openings if they are in a room.
 *
 * This function may take lots of CPU time if lots of monsters are fleeing.
 *
 * Return TRUE if a safe location is available.
 */
static bool find_safety(int Ind, int m_idx, int *yp, int *xp)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    int fy = m_ptr->fy;
    int fx = m_ptr->fx;
    int py = p_ptr->py;
    int px = p_ptr->px;
    int i, y, x, dy, dx, d, dis;
    int gy = 0, gx = 0, gdis = 0;
    const int *y_offsets;
    const int *x_offsets;

    /* Start with adjacent locations, spread further */
    for (d = 1; d < 10; d++)
    {
        /* Get the lists of points with a distance d from (fx, fy) */
        y_offsets = dist_offsets_y[d];
        x_offsets = dist_offsets_x[d];

        /* Check the locations */
        for (i = 0, dx = x_offsets[0], dy = y_offsets[0]; dx != 0 || dy != 0;
             i++, dx = x_offsets[i], dy = y_offsets[i])
        {
            y = fy + dy;
            x = fx + dx;

            /* Skip illegal locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Skip locations in a wall */
            if (!cave_floor_bold(p_ptr->depth, y, x)) continue;

            /* Ignore grids very far from the player */
            if (p_ptr->cave->when[y][x] < p_ptr->cave->when[py][px]) continue;

            /* Ignore too-distant grids */
            if (p_ptr->cave->cost[y][x] > p_ptr->cave->cost[fy][fx] + 2 * d)
                continue;

            /* Check for absence of shot (more or less) */
            if (!player_has_los_bold(p_ptr, y, x))
            {
                /* Calculate distance from player */
                dis = distance(y, x, py, px);

                /* Remember if further than previous */
                if (dis > gdis)
                {
                    gy = y;
                    gx = x;
                    gdis = dis;
                }
            }
        }

        /* Check for success */
        if (gdis > 0)
        {
            /* Good location */
            (*yp) = fy - gy;
            (*xp) = fx - gx;

            /* Found safe place */
            return (TRUE);
        }
    }

    /* No safe place */
    return (FALSE);
}


/*
 * Choose a good hiding place near a monster for it to run toward.
 *
 * Pack monsters will use this to "ambush" the player and lure him out
 * of corridors into open space so they can swarm him.
 *
 * Return TRUE if a good location is available.
 */
static bool find_hiding(int Ind, int m_idx, int *yp, int *xp)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    int fy = m_ptr->fy;
    int fx = m_ptr->fx;
    int py = p_ptr->py;
    int px = p_ptr->px;
    int i, y, x, dy, dx, d, dis;
    int gy = 0, gx = 0, gdis = 999, min;
    const int *y_offsets, *x_offsets;

    /* Closest distance to get */
    min = distance(py, px, fy, fx) * 3 / 4 + 2;

    /* Start with adjacent locations, spread further */
    for (d = 1; d < 10; d++)
    {
        /* Get the lists of points with a distance d from (fx, fy) */
        y_offsets = dist_offsets_y[d];
        x_offsets = dist_offsets_x[d];

        /* Check the locations */
        for (i = 0, dx = x_offsets[0], dy = y_offsets[0]; dx != 0 || dy != 0;
             i++, dx = x_offsets[i], dy = y_offsets[i])
        {
            y = fy + dy;
            x = fx + dx;

            /* Skip illegal locations */
            if (!in_bounds_fully(y, x)) continue;

            /* Skip occupied locations */
            if (!cave_empty_bold(p_ptr->depth, y, x)) continue;

            /* Check for hidden, available grid */
            if (!player_has_los_bold(p_ptr, y, x) && clean_shot(p_ptr->depth, fy, fx, y, x))
            {
                /* Calculate distance from player */
                dis = distance(y, x, py, px);

                /* Remember if closer than previous */
                if (dis < gdis && dis >= min)
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
            (*yp) = fy - gy;
            (*xp) = fx - gx;

            /* Found good place */
            return (TRUE);
        }
    }

    /* No good place */
    return (FALSE);
}


/*
 * Choose "logical" directions for monster movement
 *
 * We store the directions in a special "mm" array
 */
static bool get_moves(int Ind, int m_idx, int mm[5])
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    int y, x;
    int y2 = py;
    int x2 = px;
    bool done = FALSE;

    /* Flow towards the player */
    get_moves_aux(Ind, m_idx, &y2, &x2);

    /* Extract the "pseudo-direction" */
    y = m_ptr->fy - y2;
    x = m_ptr->fx - x2;

    /* Normal animal packs try to get the player out of corridors. */
    if (cfg_ai_packs && rf_has(r_ptr->flags, RF_FRIENDS) && rf_has(r_ptr->flags, RF_ANIMAL) &&
        !flags_test(r_ptr->flags, RF_SIZE, RF_KILL_WALL, RF_PASS_WALL, FLAG_END))
    {
        int i, open = 0;

        /*
         * Check grid around the player for room interior (room walls count)
         * or other empty space
         */
        for (i = 0; i < 8; i++)
        {
            /* Check grid */
            if ((cave_get(p_ptr->depth)->feat[py + ddy_ddd[i]][px + ddx_ddd[i]] <= FEAT_MORE) ||
                (cave_get(p_ptr->depth)->info[py + ddy_ddd[i]][px + ddx_ddd[i]] & (CAVE_ROOM)))
            {
                /* One more open grid */
                open++;
            }
        }

        /* Not in an empty space and strong player */
        if ((open < 7) && (p_ptr->chp > p_ptr->mhp / 2))
        {
            /* Find hiding place */
            if (find_hiding(Ind, m_idx, &y, &x)) done = TRUE;
        }
    }

    /* Apply fear */
    if (!done && mon_will_run(Ind, m_idx))
    {
        /* Try to find safe place */
        if (!(cfg_ai_smart && find_safety(Ind, m_idx, &y, &x)))
        {
            /* This is not a very "smart" method XXX XXX */
            y = (-y);
            x = (-x);
        }
        else
        {
            /* Adjust movement */
            get_fear_moves_aux(Ind, m_idx, &y, &x);
        }

        done = TRUE;
    }

    /* Monster groups try to surround the player */
    if (!done && cfg_ai_packs && rf_has(r_ptr->flags, RF_FRIENDS))
    {
        int i, tmp;

        /* If we are not already adjacent */
        if (m_ptr->cdis > 1)
        {
            /* Find an empty square near the player to fill */
            tmp = randint0(8);
            for (i = 0; i < 8; i++)
            {
                /* Pick squares near player (pseudo-randomly) */
                y2 = py + ddy_ddd[(tmp + i) & 7];
                x2 = px + ddx_ddd[(tmp + i) & 7];

                /* Ignore filled grids */
                if (!cave_empty_bold(p_ptr->depth, y2, x2)) continue;

                /* Try to fill this hole */
                break;
            }
        }

        /* Extract the new "pseudo-direction" */
        y = m_ptr->fy - y2;
        x = m_ptr->fx - x2;
    }

    /* Check for no move */
    if (!x && !y) return (FALSE);

    /* Extract some directions */
    compute_moves(x, y, mm);

    /* Want to move */
    return (TRUE);
}


/*
 * Hack -- compare the "strength" of two monsters XXX XXX XXX
 */
static int compare_monsters(const monster_type *m_ptr, const monster_type *n_ptr)
{
    monster_race *r_ptr;
    u32b mexp1, mexp2;

    /* Race 1 */
    r_ptr = &r_info[m_ptr->r_idx];

    /* Extract mexp */
    mexp1 = r_ptr->mexp;

    /* Race 2 */
    r_ptr = &r_info[n_ptr->r_idx];

    /* Extract mexp */
    mexp2 = r_ptr->mexp;

    /* Compare */
    if (mexp1 < mexp2) return (-1);
    if (mexp1 > mexp2) return (1);

    /* Assume equal */
    return (0);
}


/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(int dice, int sides, int dam)
{
    int max = 0;
    int total = dice * sides;

    /* Must do at least 95% of perfect */
    if (dam < total * 19 / 20) return (0);

    /* Weak blows rarely work */
    if ((dam < 20) && !magik(dam)) return (0);

    /* Perfect damage */
    if (dam == total) max++;

    /* Super-charge */
    if (dam >= 20)
    {
        while (magik(2)) max++;
    }

    /* Critical damage */
    if (dam > 45) return (6 + max);
    if (dam > 33) return (5 + max);
    if (dam > 25) return (4 + max);
    if (dam > 18) return (3 + max);
    if (dam > 11) return (2 + max);
    return (1 + max);
}


/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, always hit 12% of the time.
 * Otherwise, match monster power against player armor.
 */
bool check_hit(struct player *p, int power, int level)
{
    int chance, ac;

    /* Calculate the "attack quality" */
    chance = (power + (level * 3));

    /* Total armor */
    ac = p->state.ac + p->state.to_a;

    /* If the monster checks vs ac, the player learns ac bonuses */
    object_notice_on_defend(p);

    /* Check if the player was hit */
    return test_hit(chance, ac, TRUE);
}


#define MAX_DESC_INSULT 8


/*
 * Hack -- Possible "insult" messages
 */
static const char *desc_insult[MAX_DESC_INSULT] =
{
    "insults you!",
    "insults your mother!",
    "gives you the finger!",
    "humiliates you!",
    "defiles you!",
    "dances around you!",
    "makes obscene gestures!",
    "moons you!!!"
};


#define MAX_DESC_MOAN 8


/*
 * Hack -- Possible "moan" messages
 */
static const char *desc_moan[MAX_DESC_MOAN] =
{
    "wants his mushrooms back.",
    "tells you to get off his land.",
    "looks for his dogs.",
    "says 'Did you kill my Fang?'",
    "asks 'Do you want to buy any mushrooms?'",
    "seems sad about something.",
    "asks if you have seen his dogs.",
    "mumbles something about mushrooms."
};


/*
 * Attack a player via physical attacks.
 */
static bool make_attack_normal(struct monster *m_ptr, struct player *p)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p->lore[m_ptr->r_idx];
    int ap_cnt;
    int ac, rlev;
    int do_cut, do_stun;
    char m_name[NORMAL_WID];
    char ddesc[NORMAL_WID];
    int blinked;
    bool flav = FALSE;
    int sound_msg;

    /* Hack -- Don't attack shoppers */
    if (in_store(p)) return (FALSE);

    /* Not allowed to attack */
    if (rf_has(r_ptr->flags, RF_NEVER_BLOW)) return (FALSE);

    /* Total armor */
    ac = p->state.ac + p->state.to_a;

    /* Extract the effective monster level */
    rlev = ((m_ptr->level >= 1) ? m_ptr->level : 1);

    /* Get the monster name (or "it") */
    monster_desc(p, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

    /* Get the "died from" information (i.e. "a kobold") */
    monster_desc(p, ddesc, sizeof(ddesc), m_ptr, MDESC_SHOW | MDESC_IND2);

    /* Assume no blink */
    blinked = 0;

    /* Scan through all blows */
    for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
    {
        bool visible = FALSE;
        bool obvious = FALSE;
        int power = 0;
        int damage = 0;
        const char *act = NULL;

        /* Extract the attack infomation */
        int effect = m_ptr->blow[ap_cnt].effect;
        int method = m_ptr->blow[ap_cnt].method;
        int d_dice = m_ptr->blow[ap_cnt].d_dice;
        int d_side = m_ptr->blow[ap_cnt].d_side;

        /* Hack -- No more attacks */
        if (!method) break;

        /* Stop if player is dead or gone */
        if (!p->alive || p->is_dead || p->new_level_flag) break;

        /* Extract visibility (before blink) */
        if (p->mon_vis[m_ptr->midx]) visible = TRUE;

        /* Extract visibility from carrying light */
        if (rf_has(r_ptr->flags, RF_HAS_LIGHT)) visible = TRUE;

        /* Extract the attack "power" */
        power = get_power(effect);

        /* Monster hits player */
        if (!effect || check_hit(p, power, rlev))
        {
            /* Always disturbing */
            disturb(p, 1, 0);

            /* Hack -- Apply "protection from evil" */
            if (p->timed[TMD_PROTEVIL] > 0)
            {
                /* Learn about the evil flag */
                if (visible) rf_on(l_ptr->flags, RF_EVIL);

                if (rf_has(r_ptr->flags, RF_EVIL) && (p->lev >= rlev) &&
                    !magik(PY_MAX_LEVEL - p->lev))
                {
                    /* Message */
                    msg(p, "%s is repelled.", m_name);

                    /* Hack -- Next attack */
                    continue;
                }
            }

            /* Assume no cut or stun */
            do_cut = do_stun = 0;

            /* Assume no sound */
            sound_msg = MSG_GENERIC;

            /* Describe the attack method */
            switch (method)
            {
                case RBM_HIT:
                {
                    act = "hits you.";
                    do_cut = do_stun = 1;
                    sound_msg = MSG_MON_HIT;
                    break;
                }

                case RBM_TOUCH:
                {
                    act = "touches you.";
                    sound_msg = MSG_MON_TOUCH;
                    break;
                }

                case RBM_PUNCH:
                {
                    act = "punches you.";
                    do_stun = 1;
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was punched to death by %s",
                        ddesc);
                    flav = TRUE;
                    sound_msg = MSG_MON_PUNCH;
                    break;
                }

                case RBM_KICK:
                {
                    act = "kicks you.";
                    do_stun = 1;
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was kicked to death by %s",
                        ddesc);
                    flav = TRUE;
                    sound_msg = MSG_MON_KICK;
                    break;
                }

                case RBM_CLAW:
                {
                    act = "claws you.";
                    do_cut = 1;
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was eviscerated by %s", ddesc);
                    flav = TRUE;
                    sound_msg = MSG_MON_CLAW;
                    break;
                }

                case RBM_BITE:
                {
                    act = "bites you.";
                    do_cut = 1;
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was shred to pieces by %s",
                        ddesc);
                    flav = TRUE;
                    sound_msg = MSG_MON_BITE;
                    break;
                }

                case RBM_STING:
                {
                    act = "stings you.";
                    sound_msg = MSG_MON_STING;
                    break;
                }

                case RBM_BUTT:
                {
                    act = "butts you.";
                    do_stun = 1;
                    sound_msg = MSG_MON_BUTT;
                    break;
                }

                case RBM_CRUSH:
                {
                    act = "crushes you.";
                    do_stun = 1;
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was crushed to pulp by %s",
                        ddesc);
                    flav = TRUE;
                    sound_msg = MSG_MON_CRUSH;
                    break;
                }

                case RBM_ENGULF:
                {
                    act = "engulfs you.";
                    sound_msg = MSG_MON_ENGULF;
                    break;
                }

                case RBM_CRAWL:
                {
                    act = "crawls on you.";
                    sound_msg = MSG_MON_CRAWL;
                    break;
                }

                case RBM_DROOL:
                {
                    act = "drools on you.";
                    sound_msg = MSG_MON_DROOL;
                    break;
                }

                case RBM_SPIT:
                {
                    act = "spits on you.";
                    sound_msg = MSG_MON_SPIT;
                    break;
                }

                case RBM_GAZE:
                {
                    act = "gazes at you.";
                    sound_msg = MSG_MON_GAZE;
                    break;
                }

                case RBM_WAIL:
                {
                    act = "wails at you.";
                    sound_msg = MSG_MON_WAIL;
                    break;
                }

                case RBM_SPORE:
                {
                    act = "releases spores at you.";
                    sound_msg = MSG_MON_SPORE;
                    break;
                }

                case RBM_BEG:
                {
                    act = "begs you for money.";
                    sound_msg = MSG_MON_BEG;
                    break;
                }

                case RBM_INSULT:
                {
                    act = desc_insult[randint0(MAX_DESC_INSULT)];
                    sound_msg = MSG_MON_INSULT;
                    break;
                }

                case RBM_MOAN:
                {
                    act = desc_moan[randint0(MAX_DESC_MOAN)];
                    sound_msg = MSG_MON_MOAN;
                    break;
                }
            }

            /* Message */
            if (act) msgt(p, sound_msg, "%s %s", m_name, act);

            /* Hack -- Assume all attacks are obvious */
            obvious = TRUE;

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

                    /* Hack -- Player armor reduces total damage */
                    damage -= (damage * ((ac < 240)? ac: 240) / 400);

                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    take_hit(p, damage, ddesc, FALSE);

                    break;
                }

                case RBE_POISON:
                {
                    /* Take some damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    damage = adjust_dam(p, GF_POIS, damage, RANDOMISE,
                        check_for_resist(p, GF_POIS, TRUE));
                    if (damage)
                    {
                        if (!take_hit(p, damage, ddesc, FALSE))
                        {
                            /* Take "poison" effect */
                            obvious = player_inc_timed(p, TMD_POISONED, randint1(rlev) + 5, TRUE,
                                TRUE);
                        }
                    }

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_POIS);

                    break;
                }

                case RBE_UN_BONUS:
                {
                    /* Take some damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Allow complete resist */
                    if (!check_state(p, OF_RES_DISEN))
                    {
                        /* Apply disenchantment */
                        if (apply_disenchant(p, 0)) obvious = TRUE;
                    }

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_DISEN);

                    break;
                }

                case RBE_UN_POWER:
                {
                    /* Take some damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    un_power(p, m_ptr->midx, &obvious);

                    break;
                }

                case RBE_EAT_GOLD:
                {
                    /* Take some damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Obvious */
                    obvious = TRUE;

                    /* Saving throw (unless paralyzed) based on dex and level */
                    if (!p->timed[TMD_PARALYZED] &&
                        magik(adj_dex_safe[p->state.stat_ind[A_DEX]] + p->lev))
                    {
                        /* Saving throw message */
                        msg(p, "You quickly protect your money pouch!");

                        /* Occasional blink anyway */
                        if (randint0(3)) blinked = 2;
                    }

                    /* Eat gold */
                    else
                    {
                        eat_gold(p, m_ptr->midx);

                        /* Blink away */
                        blinked = 2;
                    }

                    break;
                }

                case RBE_EAT_ITEM:
                {
                    /* Take some damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Saving throw (unless paralyzed) based on dex and level */
                    if (!p->timed[TMD_PARALYZED] &&
                        magik(adj_dex_safe[p->state.stat_ind[A_DEX]] + p->lev))
                    {
                        /* Saving throw message */
                        msg(p, "You grab hold of your backpack!");

                        /* Occasional "blink" anyway */
                        blinked = 2;

                        /* Obvious */
                        obvious = TRUE;

                        /* Done */
                        break;
                    }

                    eat_item(p, m_ptr->midx, &obvious, &blinked);

                    break;
                }

                case RBE_EAT_FOOD:
                {
                    /* Take some damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    eat_fud(p, 0, &obvious);

                    break;
                }

                case RBE_EAT_LIGHT:
                {
                    /* Take some damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    eat_light(p, &obvious);

                    break;
                }

                case RBE_ACID:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg(p, "You are covered in acid!");

                    /* Special damage */
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was dissolved by %s", ddesc);
                    damage = adjust_dam(p, GF_ACID, damage, RANDOMISE,
                        check_for_resist(p, GF_ACID, TRUE));
                    if (damage)
                    {
                        if (!take_hit(p, damage, ddesc, FALSE))
                            inven_damage(p, GF_ACID, MIN(damage * 5, 300));
                    }

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_ACID);

                    break;
                }

                case RBE_ELEC:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg(p, "You are struck by lightning!");

                    /* Special damage */
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was electrocuted by %s", ddesc);
                    damage = adjust_dam(p, GF_ELEC, damage, RANDOMISE,
                        check_for_resist(p, GF_ELEC, TRUE));
                    if (damage)
                    {
                        if (!take_hit(p, damage, ddesc, FALSE))
                            inven_damage(p, GF_ELEC, MIN(damage * 5, 300));
                    }

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_ELEC);

                    break;
                }

                case RBE_FIRE:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg(p, "You are enveloped in flames!");

                    /* Special damage */
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was fried by %s", ddesc);
                    damage = adjust_dam(p, GF_FIRE, damage, RANDOMISE,
                        check_for_resist(p, GF_FIRE, TRUE));
                    if (damage)
                    {
                        if (!take_hit(p, damage, ddesc, FALSE))
                            inven_damage(p, GF_FIRE, MIN(damage * 5, 300));
                    }

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_FIRE);

                    break;
                }

                case RBE_COLD:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Message */
                    msg(p, "You are covered with frost!");

                    /* Special damage */
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was frozen by %s", ddesc);
                    damage = adjust_dam(p, GF_COLD, damage, RANDOMISE,
                        check_for_resist(p, GF_COLD, TRUE));
                    if (damage)
                    {
                        if (!take_hit(p, damage, ddesc, FALSE))
                            inven_damage(p, GF_COLD, MIN(damage * 5, 300));
                    }

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_COLD);

                    break;
                }

                case RBE_BLIND:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Increase "blind" */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You resist the effects!");
                        obvious = TRUE;
                    }
                    else
                        obvious = player_inc_timed(p, TMD_BLIND, 10 + randint1(rlev), TRUE, TRUE);

                    /* Learn about the player */
                    update_smart_learn(m_ptr, p, OF_RES_BLIND);

                    break;
                }

                case RBE_CONFUSE:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Increase "confused" */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You resist the effects!");
                        obvious = TRUE;
                    }
                    else
                        obvious = player_inc_timed(p, TMD_CONFUSED, 3 + randint1(rlev), TRUE, TRUE);

                    /* Learn about the player */
                    update_smart_learn(m_ptr, p, OF_RES_CONFU);

                    break;
                }

                case RBE_TERRIFY:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Increase "afraid" */
                    if (magik(p->state.skills[SKILL_SAVE]) ||
                        resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You stand your ground!");
                        obvious = TRUE;
                    }
                    else
                        obvious = player_inc_timed(p, TMD_AFRAID, 3 + randint1(rlev), TRUE, TRUE);

                    /* Learn about the player */
                    update_smart_learn(m_ptr, p, OF_RES_FEAR);

                    break;
                }

                case RBE_PARALYZE:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Increase "paralyzed" */
                    if (magik(p->state.skills[SKILL_SAVE]) ||
                        resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You resist the effects!");
                        obvious = TRUE;
                    }
                    else
                    {
                        if (player_inc_timed(p, TMD_PARALYZED, 3 + randint1(rlev), TRUE, TRUE))
                        {
                            obvious = TRUE;

                            /* Hack - Make level 1 monsters who paralyze also blink */
                            if (r_ptr->level == 1) blinked = 1;
                        }
                    }

                    /* Learn about the player */
                    update_smart_learn(m_ptr, p, OF_FREE_ACT);

                    break;
                }

                case RBE_LOSE_STR:
                {
                    /* Damage (physical) */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Damage (stat) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_STR]);
                        wieldeds_notice_flag(p, OF_SUST_STR);
                        obvious = TRUE;
                    }
                    else if (do_dec_stat(p, A_STR, FALSE)) obvious = TRUE;

                    break;
                }

                case RBE_LOSE_INT:
                {
                    /* Damage (physical) */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Damage (stat) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_INT]);
                        wieldeds_notice_flag(p, OF_SUST_INT);
                        obvious = TRUE;
                    }
                    else if (do_dec_stat(p, A_INT, FALSE)) obvious = TRUE;

                    break;
                }

                case RBE_LOSE_WIS:
                {
                    /* Damage (physical) */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Damage (stat) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_WIS]);
                        wieldeds_notice_flag(p, OF_SUST_WIS);
                        obvious = TRUE;
                    }
                    else if (do_dec_stat(p, A_WIS, FALSE)) obvious = TRUE;

                    break;
                }

                case RBE_LOSE_DEX:
                {
                    /* Damage (physical) */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Damage (stat) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_DEX]);
                        wieldeds_notice_flag(p, OF_SUST_DEX);
                        obvious = TRUE;
                    }
                    else if (do_dec_stat(p, A_DEX, FALSE)) obvious = TRUE;

                    break;
                }

                case RBE_LOSE_CON:
                {
                    /* Damage (physical) */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Damage (stat) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_CON]);
                        wieldeds_notice_flag(p, OF_SUST_CON);
                        obvious = TRUE;
                    }
                    else if (do_dec_stat(p, A_CON, FALSE)) obvious = TRUE;

                    break;
                }

                case RBE_LOSE_CHR:
                {
                    /* Damage (physical) */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Damage (stat) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_CHR]);
                        wieldeds_notice_flag(p, OF_SUST_CHR);
                        obvious = TRUE;
                    }
                    else if (do_dec_stat(p, A_CHR, FALSE)) obvious = TRUE;

                    break;
                }

                case RBE_LOSE_ALL:
                {
                    /* Damage (physical) */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Damage (stats) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_STR]);
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_DEX]);
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_CON]);
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_INT]);
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_WIS]);
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_CHR]);
                        wieldeds_notice_flag(p, OF_SUST_STR);
                        wieldeds_notice_flag(p, OF_SUST_INT);
                        wieldeds_notice_flag(p, OF_SUST_WIS);
                        wieldeds_notice_flag(p, OF_SUST_DEX);
                        wieldeds_notice_flag(p, OF_SUST_CON);
                        wieldeds_notice_flag(p, OF_SUST_CHR);
                        obvious = TRUE;
                    }
                    else
                    {
                        if (do_dec_stat(p, A_STR, FALSE)) obvious = TRUE;
                        if (do_dec_stat(p, A_DEX, FALSE)) obvious = TRUE;
                        if (do_dec_stat(p, A_CON, FALSE)) obvious = TRUE;
                        if (do_dec_stat(p, A_INT, FALSE)) obvious = TRUE;
                        if (do_dec_stat(p, A_WIS, FALSE)) obvious = TRUE;
                        if (do_dec_stat(p, A_CHR, FALSE)) obvious = TRUE;
                    }

                    break;
                }

                case RBE_SHATTER:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Hack -- Reduce damage based on the player armor class */
                    damage -= (damage * ((ac < 240)? ac: 240) / 400);

                    /* Take damage */
                    strnfmt(p->died_flavor, sizeof(p->died_flavor), "was splattered by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Radius 8 earthquake centered at the monster */
                    if (damage > 23)
                        earthquake(NULL, p->depth, m_ptr->fy, m_ptr->fx, 8);

                    break;
                }

                case RBE_EXP_10:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;
                    update_smart_learn(m_ptr, p, OF_HOLD_LIFE);

                    if (resist_undead_attacks(p, r_ptr))
                        msg(p, "You keep hold of your life force!");
                    else
                        drain_xp(p, 10);

                    break;
                }

                case RBE_EXP_20:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;
                    update_smart_learn(m_ptr, p, OF_HOLD_LIFE);

                    if (resist_undead_attacks(p, r_ptr))
                        msg(p, "You keep hold of your life force!");
                    else
                        drain_xp(p, 20);

                    break;
                }

                case RBE_EXP_40:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;
                    update_smart_learn(m_ptr, p, OF_HOLD_LIFE);

                    if (resist_undead_attacks(p, r_ptr))
                        msg(p, "You keep hold of your life force!");
                    else
                        drain_xp(p, 40);

                    break;
                }

                case RBE_EXP_80:
                {
                    /* Obvious */
                    obvious = TRUE;

                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;
                    update_smart_learn(m_ptr, p, OF_HOLD_LIFE);

                    if (resist_undead_attacks(p, r_ptr))
                        msg(p, "You keep hold of your life force!");
                    else
                        drain_xp(p, 80);

                    break;
                }

                case RBE_HALLU:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Increase "image" */
                    obvious = player_inc_timed(p, TMD_IMAGE, 3 + randint1(rlev / 2), TRUE, TRUE);

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_CHAOS);

                    break;
                }

                case RBE_FORGET:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Increase "amnesia" */
                    if (magik(p->state.skills[SKILL_SAVE]) ||
                        resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You resist the effects!");
                        obvious = TRUE;
                    }
                    else
                        obvious = player_inc_timed(p, TMD_AMNESIA, 4, TRUE, TRUE);

                    break;
                }

                case RBE_DISEASE:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    damage = adjust_dam(p, GF_POIS, damage, RANDOMISE,
                        check_for_resist(p, GF_POIS, TRUE));
                    if (damage)
                    {
                        if (!take_hit(p, damage, ddesc, FALSE))
                        {
                            /* Take "poison" effect */
                            obvious = player_inc_timed(p, TMD_POISONED, randint1(rlev) + 5, TRUE,
                                TRUE);
                        }
                    }

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_POIS);

                    /* Damage (stat) */
                    if (resist_undead_attacks(p, r_ptr))
                    {
                        msg(p, "You feel %s for a moment, but the feeling passes.",
                            desc_stat_neg[A_CON]);
                        wieldeds_notice_flag(p, OF_SUST_CON);
                        obvious = TRUE;
                    }
                    else if (do_dec_stat(p, A_CON, FALSE)) obvious = TRUE;

                    break;
                }

                case RBE_TIME:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Take "time" effect */
                    do_side_effects(p, RSF_BR_TIME, 0, m_ptr->midx, FALSE, TRUE);
                    obvious = TRUE;

                    /* Learn about the player */
                    monster_learn_resists(m_ptr, p, GF_TIME);

                    break;
                }

                case RBE_DISARM:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    drop_weapon(p, damage);
                    obvious = TRUE;

                    break;
                }

                case RBE_FAMINE:
                {
                    /* Take damage */
                    if (!flav)
                        strnfmt(p->died_flavor, sizeof(p->died_flavor), "was killed by %s", ddesc);
                    if (take_hit(p, damage, ddesc, FALSE)) break;

                    /* Take "hunger" effect */
                    if (!p->ghost)
                    {
                        msg(p, "You have a sudden attack of hunger!");
                        player_set_food(p, p->food / 2);
                        obvious = TRUE;
                    }

                    break;
                }
            }     

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

            /* Handle cut */
            if (do_cut) do_cut = get_cut(d_dice, d_side, damage);
            if (do_cut) player_inc_timed(p, TMD_CUT, do_cut, TRUE, TRUE);

            /* Handle stun */
            if (do_stun) do_stun = get_stun(d_dice, d_side, damage);
            if (do_stun) player_inc_timed(p, TMD_STUN, do_stun, TRUE, TRUE);
        }

        /* Monster missed player */
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
                    /* Disturbing */
                    disturb(p, 1, 0);

                    /* Message */
                    msgt(p, MSG_MISS, "%s misses you.", m_name);
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

        /* Handle freezing aura */
        if (p->timed[TMD_ICY_AURA] && damage)
        {
            if (magik(50))
                 fire_ball(p, GF_ICE, 0, 1, 1);
            else
                 fire_ball(p, GF_COLD, 0, 1 + p->lev / 5, 1);

            /* Stop if monster is dead */
            if (m_ptr->hp < 0) break;

            /* Stop if monster is stunned */
            if (m_ptr->m_timed[MON_TMD_STUN] || m_ptr->m_timed[MON_TMD_HOLD]) break;
        }
    }

    /* Blink away */
    if (blinked == 2)
    {
        if (teleport_away(m_ptr, MAX_SIGHT_LGE * 2 + 5))
            msg(p, "There is a puff of smoke!");
    }
    else if (blinked == 1)
    {
        if (teleport_away(m_ptr, 10))
            msg(p, "%s blinks away.", m_name);
    }

    /* Always notice cause of death */
    if (p->is_dead && (l_ptr->pdeaths < MAX_SHORT)) l_ptr->pdeaths++;
    if (p->is_dead && (r_ptr->lore.tdeaths < MAX_SHORT)) r_ptr->lore.tdeaths++;

    /* Assume we attacked */
    return (TRUE);
}


/*
 * Process a monster
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * In addition, monsters which *cannot* open or bash down a door
 * will still stand there trying to open it...  XXX XXX XXX
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void process_monster(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
    int oy, ox;
    int mm[5];
    bool stagger;
    bool did_what[3];
    bool do_turn;
    bool do_move;
    bool woke_up = FALSE;
    int k, y, x;
    bool allow_breed = FALSE;
    char m_name[NORMAL_WID];

    /* Get the monster name */
    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

    /* Generate monster drops */
    mon_create_drops(p_ptr, m_idx);

    /* Handle "sleep" */
    if (m_ptr->m_timed[MON_TMD_SLEEP])
    {
        u32b notice;

        /* Aggravation */
        if (check_state(p_ptr, OF_AGGRAVATE))
        {
            /* Wake the monster */
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);

            /* Notice the "waking up" */
            if (p_ptr->mon_vis[m_idx] && !m_ptr->unaware)
            {
                /* Dump a message */
                msg(p_ptr, "%s wakes up.", m_name);
            }

            /* Efficiency XXX XXX */
            return;
        }

        /* Anti-stealth */
        notice = randint0(1024);

        /* Hack -- See if monster "notices" player */
        if ((notice * notice * notice) <= p_ptr->state.noise)
        {
            /* Hack -- Amount of "waking" */
            int d = 1;

            /* Hack -- Make sure the distance isn't zero */
            if (m_ptr->cdis == 0) m_ptr->cdis = 1;

            /* Wake up faster near the player */
            if (m_ptr->cdis < 50) d = (100 / m_ptr->cdis);

            /* Still asleep */
            if (m_ptr->m_timed[MON_TMD_SLEEP] > d)
            {
                /* Monster wakes up "a little bit" */
                mon_dec_timed(p_ptr, m_ptr, MON_TMD_SLEEP, d, MON_TMD_FLG_NOMESSAGE, FALSE);

                /* Notice the "not waking up" */
                if (p_ptr->mon_vis[m_idx] && !m_ptr->unaware)
                {
                    /* Hack -- Count the ignores */
                    if (l_ptr->ignore < MAX_UCHAR) l_ptr->ignore++;
                }
            }

            /* Just woke up */
            else
            {
                /* Reset sleep counter */
                woke_up = mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);

                /* Notice the "waking up" */
                if (p_ptr->mon_vis[m_idx] && !m_ptr->unaware)
                {
                    /* Dump a message */
                    msg(p_ptr, "%s wakes up.", m_name);

                    /* Hack -- Count the wakings */
                    if (l_ptr->wake < MAX_UCHAR) l_ptr->wake++;
                }
            }
        }

        /* Still sleeping */
        if (m_ptr->m_timed[MON_TMD_SLEEP]) return;
    }

    /* If the monster just woke up, then it doesn't act */
    if (woke_up) return;

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

    /*
     * Attempt to breed (all monsters are allowed an attempt for lore
     * purposes, even non-breeders)
     */

    /* Count the adjacent monsters */
    for (k = 0, y = oy - 1; y <= oy + 1; y++)
    {
        for (x = ox - 1; x <= ox + 1; x++)
        {
            if (cave_get(p_ptr->depth)->m_idx[y][x] > 0) k++;
        }
    }

    /* Breed slower in crowded areas */
    /* Hack -- Breed even more slowly on no_recall servers */
    if (cfg_no_recall)
        allow_breed = ((k < 4) && one_in_((k + 1) * MON_MULT_ADJ * 2));
    else
        allow_breed = ((k < 4) && (!k || one_in_(k * MON_MULT_ADJ)));
    if (allow_breed)
    {
        /* Successful breeding attempt, learn about that now */
        if (p_ptr->mon_vis[m_idx]) rf_on(l_ptr->flags, RF_MULTIPLY);

        /* Try to breed (only breeders allowed) */
        if (rf_has(r_ptr->flags, RF_MULTIPLY) && clone_mon(Ind, p_ptr->depth, m_idx))
        {
            /* Make a sound */
            if (p_ptr->mon_vis[m_idx]) sound(p_ptr, MSG_MULTIPLY);

            /* Breeding takes energy */
            return;
        }
    }

    /* Mimics lie in wait */
    if (is_mimicking(m_ptr)) return;

    /* Attempt to cast a spell (skip if non hostile) */
    if (pvm_check(Ind, m_idx))
    {
        if (make_attack_spell(Ind, m_idx)) return;
    }

    /* Stagger */
    stagger = is_staggering(Ind, m_idx);

    /* Normal movement */
    if (!stagger)
    {
        /* Logical moves, may do nothing */
        if (!get_moves(Ind, m_idx, mm)) return;
    }

    /* Movement */
    process_move(Ind, m_idx, oy, ox, mm, stagger, 0 - Ind, did_what);
    do_turn = did_what[0];
    do_move = did_what[1];

    /* If we haven't done anything, try casting a spell again */
    if (cfg_ai_smart && !do_turn && !do_move)
    {
        /* Cast spell (skip if non hostile) */
        if (pvm_check(Ind, m_idx))
        {
            if (make_attack_spell(Ind, m_idx)) return;
        }
    }

    process_move_end(Ind, m_idx, did_what);
}


static bool monster_can_flow(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Monster location */
    int fy = m_ptr->fy;
    int fx = m_ptr->fx;

    /* Check the flow (normal aaf is about 20) */
    if ((p_ptr->cave->when[fy][fx] == p_ptr->cave->when[p_ptr->py][p_ptr->px]) &&
        (p_ptr->cave->cost[fy][fx] < MONSTER_FLOW_DEPTH) &&
        (p_ptr->cave->cost[fy][fx] < (cfg_small_range? (r_ptr->aaf / 2): r_ptr->aaf)))
    {
        return TRUE;
    }

    return FALSE;
}


/*
 * Get the player closest to a monster and update the distance to that player.
 *
 * Code from update_mon()... maybe we should simply call update_mon() instead.
 *
 * Note that this is necessary because process_monsters() is called after
 * process_players() which sets new_level_flag for players leaving a level...
 * but before generate_new_level() which actually creates the new level for the
 * players, thus making closest_player obsolete in the meantime.
 */
static void get_closest_player(struct cave *c, int m_idx)
{
    monster_type *m_ptr = cave_monster(c, m_idx);
    int Ind;
    int closest = 0, dis_to_closest = 9999, lowhp = 9999;
    bool blos = FALSE, new_los;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Current location */
    int fy = m_ptr->fy;
    int fx = m_ptr->fx;

    /* Check for each player */
    for (Ind = 1; Ind < NumPlayers + 1; Ind++)
    {
        player_type *p_ptr = player_get(Ind);
        int d;

        /* Make sure he's on the same dungeon level */
        if (p_ptr->depth != m_ptr->depth) continue;

        /* Hack -- Skip him if he's shopping */
        if (in_store(p_ptr)) continue;

        /* Hack -- Make the dungeon master invisible to monsters */
        if (p_ptr->dm_flags & DM_MONSTER_FRIEND) continue;

        /* Skip player if dead or gone */
        if (!p_ptr->alive || p_ptr->is_dead || p_ptr->new_level_flag) continue;

        /* Wanderers ignore level 1 players unless hurt or aggravated */
        if (rf_has(r_ptr->flags, RF_WANDERER) && (p_ptr->lev == 1) &&
            !check_state(p_ptr, OF_AGGRAVATE) && (m_ptr->hp == m_ptr->maxhp))
        {
            continue;
        }

        /* Compute distance */
        d = distance(p_ptr->py, p_ptr->px, fy, fx);

        /* Restrict distance */
        if (d > 255) d = 255;

        /* Check if monster has LOS to the player */
        new_los = los(p_ptr->depth, fy, fx, p_ptr->py, p_ptr->px);

        /* Remember this player if closest */
        if (is_closest(Ind, m_ptr, blos, new_los, d, dis_to_closest, lowhp))
        {
            blos = new_los;
            dis_to_closest = d;
            closest = Ind;
            lowhp = p_ptr->chp;
        }
    }

    /* Forget player status */
    if (closest != m_ptr->closest_player) m_ptr->smart = 0L;

    /* Always track closest player */
    m_ptr->closest_player = closest;

    /* Paranoia -- Make sure we found a closest player */
    if (closest) m_ptr->cdis = dis_to_closest;
}


/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Note that monsters can never move in the monster array (except when the
 * "compact_monsters()" function is called by "dungeon()" or "save_player()").
 *
 * This function is responsible for at least half of the processor time
 * on a normal system with a "normal" amount of monsters and a player doing
 * normal things.
 *
 * When the player is resting, virtually 90% of the processor time is spent
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "cave_light_spot()",
 * especially when the player is running.
 *
 * Note the special "MFLAG_NICE" flag, which prevents "nasty" monsters from
 * using any of their spell attacks until the player gets a turn.
 */
void process_monsters(struct cave *c)
{
    int i, j;
    monster_type *m_ptr;
    monster_race *r_ptr;

    /* Process the monsters (backwards) */
    for (i = cave_monster_max(c) - 1; i >= 1; i--)
    {
        /* Get the monster */
        m_ptr = cave_monster(c, i);

        /* Ignore "dead" monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip "unconscious" monsters */
        if (m_ptr->hp == 0) continue;

        /* Not enough energy to move */
        if (m_ptr->energy < level_speed(m_ptr->depth)) continue;

        /* Use up "some" energy */
        m_ptr->energy -= level_speed(m_ptr->depth);

        /* Hack -- Controlled monsters have a limited lifespan */
        if (m_ptr->master && m_ptr->lifespan)
        {
            m_ptr->lifespan--;

            /* Delete the monster */
            if (!m_ptr->lifespan)
            {
                update_monlist(m_ptr);
                delete_monster_idx(c, i);
                continue;
            }
        }

        /* Get closest player */
        get_closest_player(c, i);

        /* Paranoia -- Make sure we have a closest player */
        if (!m_ptr->closest_player) continue;

        /* Hack -- MvM */
        if (m_ptr->status == MSTATUS_ATTACK)
        {
            check_monster_MvM(m_ptr->closest_player, i);
            continue;
        }

        /* Get the race */
        r_ptr = &r_info[m_ptr->r_idx];

        /*
         * Process the monster if the monster either:
         * - can "sense" the player
         * - is hurt
         * - can "see" the player (checked backwards)
         * - can "smell" the player from far away (flow)
         */
        if ((m_ptr->cdis <= (cfg_small_range? (r_ptr->aaf / 2): r_ptr->aaf)) ||
            (m_ptr->hp < m_ptr->maxhp) ||
            player_has_los_bold(player_get(m_ptr->closest_player), m_ptr->fy, m_ptr->fx) ||
            monster_can_flow(m_ptr->closest_player, i))
        {
            /* Process the monster */
            process_monster(m_ptr->closest_player, i);
        }
    }

    /* Efficiency */
    if (!c->scan_monsters) return;

    /* Every 5 game turns */
    if (turn.turn % 5) return;

    /* Shimmer multi-hued monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        m_ptr = cave_monster(c, i);
        if (!m_ptr->r_idx) continue;
        r_ptr = &r_info[m_ptr->r_idx];
        if (!monster_shimmer(r_ptr)) continue;

        /* Check everyone */
        for (j = 1; j < NumPlayers + 1; j++)
        {
            player_type *q_ptr = player_get(j);

            /* If he's not here, skip him */
            if (q_ptr->depth != m_ptr->depth) continue;

            /* Actually light that spot for that player */
            if (allow_shimmer(q_ptr)) cave_light_spot_aux(q_ptr, c, m_ptr->fy, m_ptr->fx);
        }
    }
}


bool resist_undead_attacks(player_type *p_ptr, monster_race *r_ptr)
{
    return (rf_has(r_ptr->flags, RF_UNDEAD) && player_has(p_ptr, PF_UNDEAD_POWERS) &&
        (p_ptr->lev >= 6) && magik(40 + p_ptr->lev));
}


int get_power(int effect)
{
    int power = 0;

    switch (effect)
    {
        case RBE_HURT:      power = 40; break;
        case RBE_POISON:    power = 20; break;
        case RBE_ACID:      power = 20; break;
        case RBE_ELEC:      power = 40; break;
        case RBE_FIRE:      power = 40; break;
        case RBE_COLD:      power = 40; break;
        case RBE_BLIND:     power =  0; break;
        case RBE_CONFUSE:   power = 20; break;
        case RBE_TERRIFY:   power =  0; break;
        case RBE_PARALYZE:  power =  0; break;
        case RBE_HALLU:     power =  0; break;
        case RBE_EXP_10:    power = 20; break;
        case RBE_EXP_20:    power = 20; break;
        case RBE_EXP_40:    power = 20; break;
        case RBE_EXP_80:    power = 20; break;
        case RBE_UN_BONUS:  power = 10; break;
        case RBE_UN_POWER:  power = 10; break;
        case RBE_EAT_GOLD:  power =  0; break;
        case RBE_EAT_ITEM:  power =  0; break;
        case RBE_EAT_FOOD:  power =  0; break;
        case RBE_EAT_LIGHT: power =  0; break;
        case RBE_LOSE_STR:  power =  0; break;
        case RBE_LOSE_DEX:  power =  0; break;
        case RBE_LOSE_CON:  power =  0; break;
        case RBE_LOSE_INT:  power =  0; break;
        case RBE_LOSE_WIS:  power =  0; break;
        case RBE_LOSE_CHR:  power =  0; break;
        case RBE_LOSE_ALL:  power =  0; break;
        case RBE_SHATTER:   power = 60; break;
        case RBE_FORGET:    power = 60; break;
        case RBE_DISEASE:   power =  5; break;
        case RBE_TIME:      power =  5; break;
        case RBE_DISARM:    power = 60; break;
        case RBE_FAMINE:    power = 20; break;
    }

    return power;
}


int get_cut(int d_dice, int d_side, int d_dam)
{
    int k = 0;

    /* Critical hit (zero if non-critical) */
    int tmp = monster_critical(d_dice, d_side, d_dam);

    /* Roll for damage */
    switch (tmp)
    {
        case 0: k = 0; break;
        case 1: k = randint1(5); break;
        case 2: k = randint1(5) + 5; break;
        case 3: k = randint1(20) + 20; break;
        case 4: k = randint1(50) + 50; break;
        case 5: k = randint1(100) + 100; break;
        case 6: k = 300; break;
        default: k = 500; break;
    }

    return k;
}


int get_stun(int d_dice, int d_side, int d_dam)
{
    int k = 0;

    /* Critical hit (zero if non-critical) */
    int tmp = monster_critical(d_dice, d_side, d_dam);

    /* Roll for damage */
    switch (tmp)
    {
        case 0: k = 0; break;
        case 1: k = randint1(5); break;
        case 2: k = randint1(10) + 10; break;
        case 3: k = randint1(20) + 20; break;
        case 4: k = randint1(30) + 30; break;
        case 5: k = randint1(40) + 40; break;
        case 6: k = 100; break;
        default: k = 200; break;
    }

    return k;
}


/*
 * Have a monster choose a spell to cast (remove all "useless" spells).
 */
int get_thrown_spell(int Ind, int who, int depth, int m_idx, int target_m_dis, int py, int px)
{
    player_type *p_ptr = player_get(Ind);
    int chance, thrown_spell, rlev, failrate;
    bitflag f[RSF_SIZE];
    monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Assume "normal" target */
    bool normal = TRUE;

    /* Cannot cast spells when confused */
    if (m_ptr->m_timed[MON_TMD_CONF] || m_ptr->m_timed[MON_TMD_BLIND]) return -1;

    /* Hack -- Extract the spell probability */
    chance = r_ptr->freq_spell;

    /* Not allowed to cast spells */
    if (!chance) return -1;

    /* Only do spells occasionally */
    if (!magik(chance)) return -1;

    /* Hack -- Require projectable player */
    if (normal)
    {
        /* Check range */
        if (target_m_dis > MAX_RANGE) return -1;

        /* Check path (destination could be standing on a wall) */
        if (!projectable_wall(depth, m_ptr->fy, m_ptr->fx, py, px)) return -1;
    }

    /* Extract the monster level */
    rlev = ((m_ptr->level >= 1)? m_ptr->level: 1);

    /* Extract the racial spell flags */
    rsf_copy(f, r_ptr->spell_flags);

    /* Allow "desperate" spells */
    if (rf_has(r_ptr->flags, RF_SMART) && (m_ptr->hp < m_ptr->maxhp / 10) && magik(50))
    {
        /* Require intelligent spells */
        set_spells(f, RST_HASTE | RST_ANNOY | RST_ESCAPE | RST_HEAL | RST_TACTIC | RST_SUMMON);
    }

    /* Remove the "ineffective" spells */
    remove_bad_spells(who, depth, m_idx, f);

    /* Check whether summons and bolts are worth it. */
    if (!rf_has(r_ptr->flags, RF_STUPID))
    {
        /* Check for a clean bolt shot */
        if (test_spells(f, RST_BOLT) && !clean_shot(depth, m_ptr->fy, m_ptr->fx, py, px))
        {
            /* Remove spells that will only hurt friends */
            set_spells(f, ~RST_BOLT);
        }

        /* Check for a possible summon */
        if (!(summon_possible(depth, m_ptr->fy, m_ptr->fx)))
        {
            /* Remove summoning spells */
            set_spells(f, ~RST_SUMMON);
        }
    }

    /* No spells left */
    if (rsf_is_empty(f)) return -1;

    /* Choose a spell to cast */
    thrown_spell = choose_attack_spell(depth, m_idx, py, px, f);

    /* Abort if no spell was chosen */
    if (!thrown_spell) return -1;

    /* Calculate spell failure rate */
    failrate = 25 - (rlev + 3) / 4;
    if (m_ptr->m_timed[MON_TMD_FEAR]) failrate += 20;
    if (failrate < 0) failrate = 0;

    /* Stupid monsters will never fail (for jellies and such) */
    if (cfg_ai_smart || rf_has(r_ptr->flags, RF_STUPID)) failrate = 0;

    /* Hack -- Monsters will be unlikely to summon in MvM */
    if (!who && is_spell_summon(thrown_spell)) failrate = 95;

    /* Check for spell failure (innate attacks never fail) */
    if ((thrown_spell >= MIN_NONINNATE_SPELL) && magik(failrate))
    {
        char m_name[NORMAL_WID];

        /* Get the monster name (or "it") */
        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);

        /* Message */
        msg(p_ptr, "%s tries to cast a spell, but fails.", m_name);

        return -2;
    }

    return thrown_spell;
}


/*
 * Extract some directions
 */
void compute_moves(int x, int y, int mm[5])
{
    int ay, ax;
    int move_val = 0;

    /* Extract the "absolute distances" */
    ax = ABS(x);
    ay = ABS(y);

    /* Do something weird */
    if (y < 0) move_val += 8;
    if (x > 0) move_val += 4;

    /* Prevent the diamond maneuvre */
    if (ay > (ax << 1))
    {
        move_val++;
        move_val++;
    }
    else if (ax > (ay << 1))
        move_val++;

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
            if (y < 0)
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
            if (x < 0)
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
            if (y < 0)
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
            if (x < 0)
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
}


/*
 * Choose "erratic" directions for monster movement
 */
bool is_staggering(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
    int roll = randint0(100);

    /* Confused */
    if (m_ptr->m_timed[MON_TMD_CONF] || m_ptr->m_timed[MON_TMD_BLIND])
    {
        /* Stagger */
        return TRUE;
    }

    /* Random movement - always attempt for lore purposes */

    /* Random movement (25%) */
    if (roll < 25)
    {
        /* Learn about small random movement */
        if (p_ptr->mon_vis[m_idx]) rf_on(l_ptr->flags, RF_RAND_25);

        /* Stagger */
        if (flags_test(r_ptr->flags, RF_SIZE, RF_RAND_25, RF_RAND_50, FLAG_END))
            return TRUE;
    }

    /* Random movement (50%) */
    else if (roll < 50)
    {
        /* Learn about medium random movement */
        if (p_ptr->mon_vis[m_idx]) rf_on(l_ptr->flags, RF_RAND_50);

        /* Stagger */
        if (rf_has(r_ptr->flags, RF_RAND_50)) return TRUE;
    }

    /* Random movement (75%) */
    else if (roll < 75)
    {
        /* Stagger */
        if (flags_test_all(r_ptr->flags, RF_SIZE, RF_RAND_25, RF_RAND_50, FLAG_END))
            return TRUE;
    }

    /* Normal movement */
    return FALSE;
}


/*
 * Handle "stun"
 */
bool monster_stunned(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

    /* Handle "stun" */
    if (m_ptr->m_timed[MON_TMD_STUN])
    {
        int d = 1;

        /* Make a "saving throw" against stun */
        if (CHANCE(m_ptr->level * m_ptr->level, 5000))
        {
            /* Recover fully */
            d = m_ptr->m_timed[MON_TMD_STUN];
        }

        /* Hack -- Recover from stun */
        if (m_ptr->m_timed[MON_TMD_STUN] > d)
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_STUN, d, MON_TMD_FLG_NOMESSAGE, FALSE);
        else
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_STUN, MON_TMD_FLG_NOTIFY, FALSE);

        /* Still stunned */
        if (m_ptr->m_timed[MON_TMD_STUN]) return TRUE;
    }

    /* Handle "paralysis" */
    if (m_ptr->m_timed[MON_TMD_HOLD])
    {
        int d = 1;

        /* Make a "saving throw" against paralysis */
        if (CHANCE(m_ptr->level * m_ptr->level, 5000))
        {
            /* Recover fully */
            d = m_ptr->m_timed[MON_TMD_HOLD];
        }

        /* Hack -- Recover from paralysis */
        if (m_ptr->m_timed[MON_TMD_HOLD] > d)
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_HOLD, d, MON_TMD_FLG_NOMESSAGE, FALSE);
        else
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_HOLD, MON_TMD_FLG_NOTIFY, FALSE);

        /* Still paralyzed */
        if (m_ptr->m_timed[MON_TMD_HOLD]) return TRUE;
    }

    /* Not stunned */
    return FALSE;
}


/*
 * Handle confusion, fear, poison, bleeding
 */
void monster_effects(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

    /* Handle confusion */
    if (m_ptr->m_timed[MON_TMD_CONF])
    {
        /* Amount of "boldness" */
        int d = randint1(m_ptr->level / 10 + 1);

        if (m_ptr->m_timed[MON_TMD_CONF] > d)
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_CONF, d, MON_TMD_FLG_NOMESSAGE, FALSE);
        else
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_CONF, MON_TMD_FLG_NOTIFY, FALSE);
    }

    /* Handle "fear" */
    if (m_ptr->m_timed[MON_TMD_FEAR])
    {
        /* Amount of "boldness" */
        int d = randint1(m_ptr->level / 10 + 1);

        if (m_ptr->m_timed[MON_TMD_FEAR] > d)
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_FEAR, d, MON_TMD_FLG_NOMESSAGE, FALSE);
        else
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_FEAR, MON_TMD_FLG_NOTIFY, FALSE);
    }

    /* Handle poison */
    if (m_ptr->m_timed[MON_TMD_POIS])
    {
        /* Amount of "boldness" */
        int d = randint1(m_ptr->level / 10 + 1);

        if (m_ptr->m_timed[MON_TMD_POIS] > d)
        {
            /* Reduce the poison */
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_POIS, d, MON_TMD_FLG_NOMESSAGE, FALSE);

            /* Take damage */
            if (m_ptr->hp > 0)
            {
                m_ptr->hp--;

                /* Unconscious state - message if visible */
                if ((m_ptr->hp == 0) && p_ptr->mon_vis[m_idx])
                {
                    char m_name[NORMAL_WID];

                    /* Acquire the monster name */
                    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

                    /* Dump a message */
                    add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_CROAK, TRUE);
                }

                /* Hack -- Update the health bar */
                if (p_ptr->mon_vis[m_idx]) update_health(m_idx);
            }
        }
        else
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_POIS, MON_TMD_FLG_NOTIFY, FALSE);
    }       

    /* Handle bleeding */
    if (m_ptr->m_timed[MON_TMD_CUT])
    {
        /* Amount of "boldness" */
        int d = randint1(m_ptr->level / 10 + 1);

        if (m_ptr->m_timed[MON_TMD_CUT] > d)
        {
            /* Reduce the bleeding */
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_CUT, d, MON_TMD_FLG_NOMESSAGE, FALSE);

            /* Take damage */
            if (m_ptr->hp > 0)
            {
                m_ptr->hp--;

                /* Unconscious state - message if visible */
                if ((m_ptr->hp == 0) && p_ptr->mon_vis[m_idx])
                {
                    char m_name[NORMAL_WID];

                    /* Acquire the monster name */
                    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

                    /* Dump a message */
                    add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_CROAK, TRUE);
                }

                /* Hack -- Update the health bar */
                if (p_ptr->mon_vis[m_idx]) update_health(m_idx);
            }
        }
        else
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_CUT, MON_TMD_FLG_NOTIFY, FALSE);
    }

    /* Handle blindness */
    if (m_ptr->m_timed[MON_TMD_BLIND])
    {
        /* Amount of "boldness" */
        int d = randint1(m_ptr->level / 10 + 1);

        if (m_ptr->m_timed[MON_TMD_BLIND] > d)
            mon_dec_timed(p_ptr, m_ptr, MON_TMD_BLIND, d, MON_TMD_FLG_NOMESSAGE, FALSE);
        else
            mon_clear_timed(p_ptr, m_ptr, MON_TMD_BLIND, MON_TMD_FLG_NOTIFY, FALSE);
    }
}


/*
 * Monster movement
 */
void process_move(int Ind, int m_idx, int oy, int ox, int mm[5], bool stagger, int target_m_idx,
    bool did_what[3])
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];
    int i, d, ny, nx;
    bool do_turn;
    bool do_move;
    bool do_view;
    bool did_open_door;
    bool did_bash_door;
    int y, x;

    /* Target info */
    if (target_m_idx < 0)
    {
        y = p_ptr->py;
        x = p_ptr->px;
    }
    else
    {
        monster_type *target_m_ptr = cave_monster(cave_get(p_ptr->depth), target_m_idx);

        y = target_m_ptr->fy;
        x = target_m_ptr->fx;
    }

    /* Assume nothing */
    do_turn = FALSE;
    do_move = FALSE;
    do_view = FALSE;

    /* Assume nothing */
    did_open_door = FALSE;
    did_bash_door = FALSE;

    /* Process moves */
    for (i = 0; i < 5; i++)
    {
        int cur_m_idx;

        /* Get the direction (or stagger) */
        d = (stagger? ddd[randint0(8)]: mm[i]);

        /* Get the destination */
        ny = oy + ddy[d];
        nx = ox + ddx[d];

        cur_m_idx = cave_get(p_ptr->depth)->m_idx[ny][nx];

        /* Safe floor */
        if (cave_issafefloor(cave_get(p_ptr->depth), ny, nx))
        {
            /* Nothing */
        }

        /* Floor is open? */
        else if (cave_floor_bold(p_ptr->depth, ny, nx))
        {
            /* Go ahead and move */
            do_move = TRUE;
        }

        /* Permanent wall in the way */
        else if (cave_isperm(cave_get(p_ptr->depth), ny, nx) ||
            (cave_get(p_ptr->depth)->feat[ny][nx] == FEAT_PERM_CLEAR))
        {
            /* Nothing */
        }

        /* Normal wall, door, or secret door in the way */
        else
        {
            /*
             * There's some kind of feature in the way, so learn about
             * kill-wall and pass-wall now
             */
            if (p_ptr->mon_vis[m_idx])
            {
                rf_on(l_ptr->flags, RF_PASS_WALL);
                rf_on(l_ptr->flags, RF_KILL_WALL);
            }

            /* Monster moves through walls (and doors) */
            if (rf_has(r_ptr->flags, RF_PASS_WALL))
            {
                /* Pass through walls/doors/rubble */
                do_move = TRUE;
            }

            /* Monster destroys walls (and doors) */
            else if (rf_has(r_ptr->flags, RF_KILL_WALL))
            {
                /* Eat through walls/doors/rubble */
                do_move = TRUE;

                /* Forget the wall */
                forget_spot(p_ptr->depth, ny, nx);

                /* Notice */
                cave_set_feat(cave_get(p_ptr->depth), ny, nx, FEAT_FLOOR);

                /* Note changes to viewable region */
                if (player_has_los_bold(p_ptr, ny, nx)) do_view = TRUE;
            }

            /* Handle doors and secret doors */
            else if (cave_iscloseddoor(cave_get(p_ptr->depth), ny, nx) ||
                cave_issecretdoor(cave_get(p_ptr->depth), ny, nx))
            {
                bool may_bash = TRUE;

                /* Take a turn */
                do_turn = TRUE;

                /* Learn about door abilities */
                if (p_ptr->mon_vis[m_idx])
                {
                    rf_on(l_ptr->flags, RF_OPEN_DOOR);
                    rf_on(l_ptr->flags, RF_BASH_DOOR);
                }

                /* Creature can open doors. */
                if (rf_has(r_ptr->flags, RF_OPEN_DOOR))
                {
                    /* Closed doors and secret doors */
                    if ((cave_get(p_ptr->depth)->feat[ny][nx] == FEAT_DOOR_HEAD) ||
                        cave_issecretdoor(cave_get(p_ptr->depth), ny, nx))
                    {
                        /* The door is open */
                        did_open_door = TRUE;

                        /* Do not bash the door */
                        may_bash = FALSE;
                    }

                    /* Locked doors (not jammed) */
                    else if (cave_get(p_ptr->depth)->feat[ny][nx] < FEAT_DOOR_HEAD + 0x08)
                    {
                        int k;

                        /* Door power */
                        k = ((cave_get(p_ptr->depth)->feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);

                        /* Try to unlock it */
                        if (!CHANCE(k, m_ptr->hp / 10))
                        {
                            char m_name[NORMAL_WID];

                            /* Print a message */
                            if (p_ptr->mon_vis[m_idx])
                            {
                                monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);
                                msg(p_ptr, "%s fiddles with the lock.", m_name);
                            }
                            else
                                msg(p_ptr, "Something fiddles with a lock.");

                            /* Reduce the power of the door by one */
                            cave_set_feat(cave_get(p_ptr->depth), ny, nx,
                                cave_get(p_ptr->depth)->feat[ny][nx] - 1);

                            /* Do not bash the door */
                            may_bash = FALSE;
                        }
                    }
                }

                /* Stuck doors -- attempt to bash them down if allowed */
                if (may_bash && rf_has(r_ptr->flags, RF_BASH_DOOR))
                {
                    int k;

                    /* Door power */
                    k = ((cave_get(p_ptr->depth)->feat[ny][nx] - FEAT_DOOR_HEAD) & 0x07);

                    /* Attempt to bash */
                    if (!CHANCE(k, m_ptr->hp / 10))
                    {
                        char m_name[NORMAL_WID];

                        /* Print a message */
                        if (p_ptr->mon_vis[m_idx])
                        {
                            monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);
                            msg(p_ptr, "%s slams against the door.", m_name);
                        }
                        else
                            msg(p_ptr, "Something slams against a door.");

                        /* Reduce the power of the door by one */
                        cave_set_feat(cave_get(p_ptr->depth), ny, nx,
                            cave_get(p_ptr->depth)->feat[ny][nx] - 1);

                        /* If the door is no longer jammed */
                        if (cave_get(p_ptr->depth)->feat[ny][nx] < FEAT_DOOR_HEAD + 0x09)
                        {
                            msg(p_ptr, "You hear a door burst open!");

                            /* Disturb (sometimes) */
                            if (target_m_idx < 0) disturb(p_ptr, 0, 0);

                            /* The door was bashed open */
                            did_bash_door = TRUE;

                            /* Hack -- fall into doorway */
                            do_move = TRUE;
                        }
                    }
                }

                /* Deal with doors in the way */
                if (did_open_door || did_bash_door)
                {
                    /* Break down the door */
                    if (did_bash_door && magik(50))
                        cave_set_feat(cave_get(p_ptr->depth), ny, nx, FEAT_BROKEN);

                    /* Open the door */
                    else
                        cave_set_feat(cave_get(p_ptr->depth), ny, nx, FEAT_OPEN);

                    /* Handle viewable doors */
                    if (player_has_los_bold(p_ptr, ny, nx)) do_view = TRUE;
                }
            }
        }

        /* Hack -- A player is in the way */
        if (cur_m_idx < 0) do_move = TRUE;

        /* Check for Glyph of Warding */
        if (do_move && (cave_get(p_ptr->depth)->feat[ny][nx] == FEAT_GLYPH))
        {
            /* Assume no move allowed */
            do_move = FALSE;

            /* Break the ward */
            if (CHANCE(m_ptr->level, BREAK_GLYPH))
            {
                /* Describe observable breakage */
                if (p_ptr->cave->info[ny][nx] & CAVE_MARK)
                    msg(p_ptr, "The rune of protection is broken!");

                /* Forget the rune */
                forget_spot(p_ptr->depth, ny, nx);

                /* Break the rune */
                cave_set_feat(cave_get(p_ptr->depth), ny, nx, FEAT_FLOOR);

                /* Allow movement */
                do_move = TRUE;
            }
        }

        /* A player is in the way. */
        if (do_move && (cur_m_idx < 0))
        {
            /* Learn about if the monster attacks */
            if (p_ptr->mon_vis[m_idx]) rf_on(l_ptr->flags, RF_NEVER_BLOW);

            /* Some monsters never attack */
            if ((ny == y) && (nx == x) && rf_has(r_ptr->flags, RF_NEVER_BLOW))
            {
                /* Do not move */
                do_move = FALSE;
            }

            /* Otherwise, attack the player */
            else
            {
                /* Skip if non hostile */
                if (pvm_check(0 - cur_m_idx, m_idx))
                {
                    /* Do the attack */
                    make_attack_normal(m_ptr, player_get(0 - cur_m_idx));

                    /* Took a turn */
                    do_turn = TRUE;
                }

                /* Do not move */
                do_move = FALSE;
            }
        }

        /* Some monsters never move */
        if (do_move && rf_has(r_ptr->flags, RF_NEVER_MOVE))
        {
            /* Learn about lack of movement */
            if (p_ptr->mon_vis[m_idx]) rf_on(l_ptr->flags, RF_NEVER_MOVE);

            /* Do not move */
            do_move = FALSE;
        }

        /* A monster is in the way */
        if (do_move && (cur_m_idx > 0))
        {
            monster_type *n_ptr = cave_monster(cave_get(p_ptr->depth), cur_m_idx);

            /* Kill weaker monsters */
            int kill_ok = rf_has(r_ptr->flags, RF_KILL_BODY);

            /* Push past weaker monsters (unless leaving a wall) */
            int move_ok = (rf_has(r_ptr->flags, RF_MOVE_BODY) &&
                cave_floor_bold(p_ptr->depth, m_ptr->fy, m_ptr->fx));

            /* Assume no movement */
            do_move = FALSE;

            /* Always attack if this is our target */
            if ((target_m_idx > 0) && (target_m_idx == cur_m_idx) &&
                !master_in_party(m_ptr->master, n_ptr->master))
            {
                /* Do the attack */
                make_attack_normal_MvM(Ind, cur_m_idx, m_idx);

                /* Took a turn */
                do_turn = TRUE;
            }

            else if (compare_monsters(m_ptr, n_ptr) > 0)
            {
                /* Learn about pushing and shoving */
                if (p_ptr->mon_vis[m_idx])
                {
                    rf_on(l_ptr->flags, RF_KILL_BODY);
                    rf_on(l_ptr->flags, RF_MOVE_BODY);
                }

                if (kill_ok || move_ok)
                {
                    char m_name[NORMAL_WID];
                    char n_name[NORMAL_WID];

                    /* Get the names of the monsters involved */
                    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_IND1 | MDESC_CAPITAL);
                    monster_desc(p_ptr, n_name, sizeof(n_name), n_ptr, MDESC_IND1);

                    /* Allow movement */
                    do_move = TRUE;

                    /* Monster ate another monster */
                    if (kill_ok)
                    {
                        /* Note if visible */
                        if (p_ptr->mon_vis[m_idx] && p_ptr->mon_los[m_idx])
                            msg(p_ptr, "%s tramples over %s.", m_name, n_name);

                        delete_monster(p_ptr->depth, ny, nx);
                    }

                    /* Monster pushed past another monster */
                    else
                    {
                        /* Note if visible */
                        if (p_ptr->mon_vis[m_idx] && p_ptr->mon_los[m_idx])
                            msg(p_ptr, "%s pushes past %s.", m_name, n_name);
                    }
                }

                /* Otherwise, attack the monster (skip if non hostile) */
                else if ((target_m_idx > 0) && !master_in_party(m_ptr->master, n_ptr->master))
                {
                    /* Do the attack */
                    make_attack_normal_MvM(Ind, cur_m_idx, m_idx);

                    /* Took a turn */
                    do_turn = TRUE;
                }
            }

            /* Always attack stronger monsters */
            else if ((target_m_idx > 0) && !master_in_party(m_ptr->master, n_ptr->master))
            {
                /* Do the attack */
                make_attack_normal_MvM(Ind, cur_m_idx, m_idx);

                /* Took a turn */
                do_turn = TRUE;
            }
        }

        /* Creature has been allowed move */
        if (do_move)
        {
            s16b this_o_idx, next_o_idx = 0;

            /* Learn about no lack of movement */
            if (p_ptr->mon_vis[m_idx]) rf_on(l_ptr->flags, RF_NEVER_MOVE);

            /* Take a turn */
            do_turn = TRUE;

            /* Move the monster */
            monster_swap(p_ptr->depth, oy, ox, ny, nx);

            /* Possible disturb */
            if ((target_m_idx < 0) && p_ptr->mon_vis[m_idx] &&
                (OPT_P(p_ptr, disturb_move) || (p_ptr->mon_los[m_idx] && OPT_P(p_ptr, disturb_near))))
            {
                /* Disturb (except townies, friendlies and unaware mimics) */
                if ((m_ptr->level > 0) && pvm_check(Ind, m_idx) && !is_mimicking(m_ptr))
                    disturb(p_ptr, 0, 0);
            }

            /* Scan all objects in the grid */
            for (this_o_idx = cave_get(p_ptr->depth)->o_idx[ny][nx]; this_o_idx;
                this_o_idx = next_o_idx)
            {
                object_type *o_ptr;

                /* Get the object */
                o_ptr = object_byid(this_o_idx);

                /* Get the next object */
                next_o_idx = o_ptr->next_o_idx;

                /* Skip gold */
                if (o_ptr->tval == TV_GOLD) continue;

                /* Learn about item pickup behavior */
                if (p_ptr->mon_vis[m_idx])
                {
                    rf_on(l_ptr->flags, RF_TAKE_ITEM);
                    rf_on(l_ptr->flags, RF_KILL_ITEM);
                }

                /* Take or Kill objects on the floor */
                if (rf_has(r_ptr->flags, RF_TAKE_ITEM) || rf_has(r_ptr->flags, RF_KILL_ITEM))
                {
                    bitflag obj_flags[OF_SIZE];
                    bitflag mon_flags[RF_SIZE];
                    char m_name[NORMAL_WID];
                    char o_name[NORMAL_WID];

                    rf_wipe(mon_flags);

                    /* Extract some flags */
                    object_flags(o_ptr, obj_flags);

                    /* Get the object name */
                    object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

                    /* Get the monster name */
                    monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_IND1 | MDESC_CAPITAL);

                    /* React to objects that hurt the monster */
                    react_to_slay(obj_flags, mon_flags);

                    /* The object cannot be picked up by the monster */
                    if (o_ptr->artifact || rf_is_inter(r_ptr->flags, mon_flags))
                    {
                        /* Only give a message for "take_item" */
                        if (rf_has(r_ptr->flags, RF_TAKE_ITEM))
                        {
                            /* Describe observable situations */
                            if (p_ptr->mon_vis[m_idx] && player_has_los_bold(p_ptr, ny, nx) &&
                                !squelch_item_ok(p_ptr, o_ptr))
                            {
                                /* Dump a message */
                                msg(p_ptr, "%s tries to pick up %s, but fails.", m_name, o_name);
                            }
                        }
                    }

                    /* Pick up the item */
                    else if (rf_has(r_ptr->flags, RF_TAKE_ITEM))
                    {
                        object_type *i_ptr;
                        object_type object_type_body;

                        /* Describe observable situations */
                        if (player_has_los_bold(p_ptr, ny, nx) &&
                            !squelch_item_ok(p_ptr, o_ptr))
                        {
                            /* Dump a message */
                            msg(p_ptr, "%s picks up %s.", m_name, o_name);
                        }

                        /* Get local object */
                        i_ptr = &object_type_body;

                        /* Obtain local object */
                        object_copy(i_ptr, o_ptr);

                        /* Carry the object */
                        monster_carry(m_ptr, i_ptr, FALSE);

                        /* Delete the object */
                        delete_object_idx(this_o_idx);
                    }

                    /* Destroy the item */
                    else
                    {
                        /* Describe observable situations */
                        if (player_has_los_bold(p_ptr, ny, nx) && !squelch_item_ok(p_ptr, o_ptr))
                        {
                            /* Dump a message */
                            msgt(p_ptr, MSG_DESTROY, "%s crushes %s.", m_name, o_name);
                        }

                        /* Delete the object */
                        delete_object_idx(this_o_idx);
                    }
                }
            }
        }

        /* Stop when done */
        if (do_turn) break;
    }

    did_what[0] = do_turn;
    did_what[1] = do_move;
    did_what[2] = do_view;
}


void process_move_end(int Ind, int m_idx, bool did_what[3])
{
    player_type *p_ptr = player_get(Ind);
    bool do_turn = did_what[0];
    bool do_move = did_what[1];
    bool do_view = did_what[2];
    monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];

    if (rf_has(r_ptr->flags, RF_HAS_LIGHT)) do_view = TRUE;

    /* Notice changes in view */
    if (do_view)
    {
        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

        /* Fully update the flow XXX XXX XXX */
        p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);
    }

    /* Hack -- Get "bold" if out of options */
    if (!do_turn && !do_move && m_ptr->m_timed[MON_TMD_FEAR])
        mon_clear_timed(p_ptr, m_ptr, MON_TMD_FEAR, MON_TMD_FLG_NOTIFY, FALSE);

    /* If we see an unaware monster do something, become aware of it */
    if (do_turn && m_ptr->unaware) become_aware(p_ptr, m_ptr);
}


/*
 * Determine whether the player is invisible to a monster
 */
bool player_invis(int Ind, monster_type *m_ptr)
{
    player_type *p_ptr = player_get(Ind);
    s16b mlv;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    /* Player should be invisible */
    if (!p_ptr->timed[TMD_INVIS]) return FALSE;

    /* Aggravation breaks invisibility */
    if (check_state(p_ptr, OF_AGGRAVATE)) return FALSE;

    /* Can't fool a monster once injured */
    if (m_ptr->hp < m_ptr->maxhp) return FALSE;

    /* Questor monsters see invisible */
    if (rf_has(r_ptr->flags, RF_QUESTOR)) return FALSE;

    /* Invisible monsters see invisible */
    if (rf_has(r_ptr->flags, RF_INVISIBLE)) return FALSE;

    mlv = (s16b)m_ptr->level;
    if (rf_has(r_ptr->flags, RF_NO_SLEEP)) mlv += 5;
    if (rf_has(r_ptr->flags, RF_DRAGON)) mlv += 10;
    if (rf_has(r_ptr->flags, RF_UNDEAD)) mlv += 12;
    if (rf_has(r_ptr->flags, RF_DEMON)) mlv += 10;
    if (rf_has(r_ptr->flags, RF_ANIMAL)) mlv += 3;
    if (rf_has(r_ptr->flags, RF_ORC)) mlv -= 15;
    if (rf_has(r_ptr->flags, RF_TROLL)) mlv -= 10;
    if (rf_has(r_ptr->flags, RF_STUPID)) mlv /= 2;
    if (rf_has(r_ptr->flags, RF_SMART)) mlv = (mlv * 5) / 4;
    if (rf_has(r_ptr->flags, RF_UNIQUE)) mlv *= 2;
    if (mlv < 0) mlv = 0;

    /* High level monsters can't be fooled */
    if (mlv > p_ptr->lev) return FALSE;

    /* Monsters can sometimes see invisible players */
    /* 1 every 100 game turns at max */
    /* This will act like a super slow monster effect */
    if (CHANCE(mlv, p_ptr->lev * 100)) return FALSE;

    /* Player is invisible */
    return TRUE;
}


/*
 * Determine whether the player is closest to a monster
 */
bool is_closest(int Ind, monster_type *m_ptr, bool blos, bool new_los, int j,
    int dis_to_closest, int lowhp)
{
    player_type *p_ptr = player_get(Ind);

    /* Followers will try to reach their master */
    if ((m_ptr->status == MSTATUS_FOLLOW) && (m_ptr->master != p_ptr->id)) return FALSE;

    /* Skip guards */
    if (master_in_party(m_ptr->master, p_ptr->id) && (m_ptr->status == MSTATUS_GUARD))
        return FALSE;

    /* Skip if the monster can't see the player */
    if (player_invis(Ind, m_ptr)) return FALSE;

    /* Check that the closest VISIBLE target gets selected */
    /* If no visible one available just take the closest */
    if (((blos >= new_los) && (j > dis_to_closest)) || (blos > new_los))
        return FALSE;

    /* Skip if same distance and stronger and same visibility */
    if ((j == dis_to_closest) && (p_ptr->chp > lowhp) && (blos == new_los))
        return FALSE;

    return TRUE;
}
