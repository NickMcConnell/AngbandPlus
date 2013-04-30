/*
 * File: target.c
 * Purpose: Targeting code
 *
 * Copyright (c) 1997-2007 Angband contributors
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
#include "../common/tvalsval.h"
#include "cmds.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "s-spells.h"
#include "squelch.h"
#include "target.h"


#define TS_INITIAL_SIZE  20


/*** Functions ***/


/*
 * Health description (unhurt, wounded, etc)
 */
static const char *look_health_desc(bool living, s16b chp, s16b mhp)
{
    int perc;

    /* Healthy */
    if (chp >= mhp)
    {
        /* No damage */
        return (living? "unhurt": "undamaged");
    }

    /* Calculate a health "percentage" */
    perc = 100L * chp / mhp;

    if (perc >= 60)
        return (living? "somewhat wounded": "somewhat damaged");

    if (perc >= 25)
        return (living? "wounded": "damaged");

    if (perc >= 10)
        return (living? "badly wounded": "badly damaged");

    return (living? "almost dead": "almost destroyed");
}


/*
 * Monster health description
 */
static void look_mon_desc(char *buf, size_t max, int depth, int m_idx)
{
    monster_type *m_ptr = cave_monster(cave_get(depth), m_idx);
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    bool living = TRUE;

    /* Determine if the monster is "living" (vs "undead") */
    if (monster_is_nonliving(r_ptr)) living = FALSE;

    /* Apply health description */
    my_strcpy(buf, look_health_desc(living, m_ptr->hp, m_ptr->maxhp), max);

    /* Apply condition descriptions */
    if (m_ptr->m_timed[MON_TMD_SLEEP]) my_strcat(buf, ", asleep", max);
    if (m_ptr->m_timed[MON_TMD_CONF]) my_strcat(buf, ", confused", max);
    if (m_ptr->m_timed[MON_TMD_BLIND]) my_strcat(buf, ", blind", max);
    if (m_ptr->m_timed[MON_TMD_FEAR]) my_strcat(buf, ", afraid", max);
    if (m_ptr->m_timed[MON_TMD_STUN]) my_strcat(buf, ", stunned", max);
    if (m_ptr->m_timed[MON_TMD_HOLD]) my_strcat(buf, ", paralyzed", max);

    /* Monster-specific conditions */
    if (m_ptr->m_timed[MON_TMD_POIS]) my_strcat(buf, ", poisoned", max);
    if (m_ptr->m_timed[MON_TMD_CUT]) my_strcat(buf, ", bleeding", max);

    /* Monster-specific conditions */
    switch (m_ptr->status)
    {
        case MSTATUS_GUARD: my_strcat(buf, ", guarding", max); break;
        case MSTATUS_FOLLOW: my_strcat(buf, ", following", max); break;
        case MSTATUS_ATTACK: my_strcat(buf, ", attacking", max); break;
    }
}


/*
 * Player health Description
 */
static void look_player_desc(int Ind, char *buf, size_t max)
{
    player_type *p_ptr = player_get(Ind);
    bool living = TRUE;

    /* Determine if the player is alive */
    if (p_ptr->ghost) living = FALSE;

    /* Apply health description */
    my_strcpy(buf, look_health_desc(living, p_ptr->chp, p_ptr->mhp), max);

    /* Apply condition descriptions */
    if (p_ptr->resting) my_strcat(buf, ", resting", max);
    if (p_ptr->timed[TMD_CONFUSED]) my_strcat(buf, ", confused", max);
    if (check_state(p_ptr, OF_AFRAID)) my_strcat(buf, ", afraid", max);
    if (p_ptr->timed[TMD_STUN]) my_strcat(buf, ", stunned", max);

    /* Player-specific conditions */
    if (p_ptr->timed[TMD_PARALYZED]) my_strcat(buf, ", paralyzed", max);
}


/*
 * Determine if a monster/player makes a reasonable target
 *
 * The concept of "targeting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster/player.
 *
 * Currently, a monster/player is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(struct player *p, int m_idx)
{
    int pz = p->depth;
    int py = p->py;
    int px = p->px;
    monster_type *m_ptr;

    /* No target */
    if (!m_idx) return (FALSE);

    /* Target is a player */
    if (m_idx < 0)
    {
        player_type *q_ptr;

        /* Paranoia */
        if ((0 - m_idx) >= MAX_PLAYERS) return (FALSE);

        /* Get player */
        q_ptr = player_get(0 - m_idx);

        /* Paranoia */
        if (!q_ptr) return (FALSE);

        /* Player must be on same depth */
        if (p->depth != q_ptr->depth) return (FALSE);

        /* Player must be visible */
        if (!p->play_vis[0 - m_idx]) return (FALSE);

        /* Player must be aware this is a player */
        if (q_ptr->k_idx) return (FALSE);

        /* Player must be projectable */
        if (!projectable(pz, py, px, q_ptr->py, q_ptr->px, PROJECT_NONE))
            return (FALSE);

        /* Hack -- No targeting hallucinations */
        if (p->timed[TMD_IMAGE]) return (FALSE);

        /* Assume okay */
        return (TRUE);
    }

    /* Get monster */
    m_ptr = cave_monster(cave_get(pz), m_idx);

    /* Monster must be alive */
    if (!m_ptr->r_idx) return (FALSE);

    /* Monster must be visible */
    if (!p->mon_vis[m_idx]) return (FALSE);

    /* Player must be aware this is a monster */
    if (m_ptr->unaware) return (FALSE);

    /* Monster must be projectable */
    if (!projectable(pz, py, px, m_ptr->fy, m_ptr->fx, PROJECT_NONE))
        return (FALSE);

    /* Hack -- No targeting hallucinations */
    if (p->timed[TMD_IMAGE]) return (FALSE);

    /* Assume okay */
    return (TRUE);
}


/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(struct player *p)
{
    /* No target */
    if (!p->target_set) return (FALSE);

    /* Accept "location" targets */
    if (!p->target_who) return (TRUE);

    /* Check "monster" targets */
    if (p->target_who > 0)
    {
        int m_idx = p->target_who;

        /* Accept reasonable targets */
        if (target_able(p, m_idx))
        {
            monster_type *m_ptr = cave_monster(cave_get(p->depth), m_idx);

            /* Get the monster location */
            p->target_y = m_ptr->fy;
            p->target_x = m_ptr->fx;

            /* Good target */
            return (TRUE);
        }
    }

    /* Check "player" targets */
    if (p->target_who < 0)
    {
        int p_idx = 0 - p->target_who;

        /* Accept reasonable targets */
        if (target_able(p, 0 - p_idx))
        {
            player_type *q_ptr;

            /* Paranoia */
            if (p_idx >= MAX_PLAYERS) return (FALSE);

            /* Get player */
            q_ptr = player_get(p_idx);

            /* Paranoia */
            if (!q_ptr) return (FALSE);

            /* Get the player location */
            p->target_y = q_ptr->py;
            p->target_x = q_ptr->px;

            /* Good target */
            return (TRUE);
        }
    }

    /* Assume no target */
    return (FALSE);
}


/*
 * Set the target to a monster/player (or nobody)
 */
void target_set_monster(int Ind, int m_idx)
{
    player_type *p_ptr = player_get(Ind);

    /* Acceptable target */
    if (target_able(p_ptr, m_idx))
    {
        /* Save target info */
        p_ptr->target_set = TRUE;
        p_ptr->target_who = m_idx;
        if (m_idx > 0)
        {
            monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

            p_ptr->target_y = m_ptr->fy;
            p_ptr->target_x = m_ptr->fx;
        }
        else
        {
            player_type *q_ptr = player_get(0 - m_idx);

            p_ptr->target_y = q_ptr->py;
            p_ptr->target_x = q_ptr->px;
        }
    }

    /* Clear target */
    else
    {
        /* Reset target info */
        p_ptr->target_set = FALSE;
        p_ptr->target_who = 0;
        p_ptr->target_y = 0;
        p_ptr->target_x = 0;
    }
}


/*
 * Set the target to a location
 */
void target_set_location(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Legal target */
    if (in_bounds_fully(y, x))
    {
        /* Save target info */
        p_ptr->target_set = TRUE;
        p_ptr->target_who = 0;
        p_ptr->target_y = y;
        p_ptr->target_x = x;
    }

    /* Clear target */
    else
    {
        /* Reset target info */
        p_ptr->target_set = FALSE;
        p_ptr->target_who = 0;
        p_ptr->target_y = 0;
        p_ptr->target_x = 0;
    }
}


static int cmp_distance(const void *a, const void *b)
{
    const struct cmp_loc *pa = a;
    const struct cmp_loc *pb = b;
    int da, db, kx, ky;

    /* Absolute distance components */
    kx = pa->x; kx -= player_get(pa->Ind)->px; kx = ABS(kx);
    ky = pa->y; ky -= player_get(pa->Ind)->py; ky = ABS(ky);

    /* Approximate Double Distance to the first point */
    da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

    /* Absolute distance components */
    kx = pb->x; kx -= player_get(pb->Ind)->px; kx = ABS(kx);
    ky = pb->y; ky -= player_get(pb->Ind)->py; ky = ABS(ky);

    /* Approximate Double Distance to the first point */
    db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

    /* Compare the distances */
    if (da < db) return -1;
    if (da > db) return 1;
    return 0;
}


/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx, struct point_set *targets)
{
    int i, v;
    int x2, y2, x3, y3, x4, y4;
    int b_i = -1, b_v = 9999;

    /* Scan the locations */
    for (i = 0; i < point_set_size(targets); i++)
    {
        /* Point 2 */
        x2 = targets->pts[i].x;
        y2 = targets->pts[i].y;

        /* Directed distance */
        x3 = (x2 - x1);
        y3 = (y2 - y1);

        /* Verify quadrant */
        if (dx && (x3 * dx <= 0)) continue;
        if (dy && (y3 * dy <= 0)) continue;

        /* Absolute distance */
        x4 = ABS(x3);
        y4 = ABS(y3);

        /* Verify quadrant */
        if (dy && !dx && (x4 > y4)) continue;
        if (dx && !dy && (y4 > x4)) continue;

        /* Approximate Double Distance */
        v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

        /* Penalize location XXX XXX XXX */

        /* Track best */
        if ((b_i >= 0) && (v >= b_v)) continue;

        /* Track best */
        b_i = i; b_v = v;
    }

    /* Result */
    return (b_i);
}


/*
 * Hack -- Determine if a given location is "interesting"
 */
bool target_set_interactive_accept(struct player *p, int y, int x)
{
    int Ind = get_player_index(get_connection(p->conn));
    object_type *o_ptr;
    s16b this_o_idx, next_o_idx = 0;
    int m_idx = cave_get(p->depth)->m_idx[y][x];

    /* Player grids are always interesting */
    if ((m_idx < 0) && (Ind == (0 - m_idx))) return (TRUE);

    /* Handle hallucination */
    if (p->timed[TMD_IMAGE]) return (FALSE);

    /* Visible players */
    if ((m_idx < 0) && p->play_vis[0 - m_idx] && !player_get(0 - m_idx)->k_idx) return (TRUE);

    /* Visible monsters */
    if ((m_idx > 0) && p->mon_vis[m_idx] && !cave_monster(cave_get(p->depth), m_idx)->unaware)
        return (TRUE);

    /* Scan all objects in the grid */
    for (this_o_idx = cave_get(p->depth)->o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
        /* Get the object */
        o_ptr = object_byid(this_o_idx);

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Memorized object */
        if (object_marked(p, this_o_idx) && !squelch_item_ok(p, o_ptr)) return (TRUE);
    }

    /* Interesting memorized features */
    if (p->cave->info[y][x] & CAVE_MARK)
    {
        /* Notice glyphs */
        if (cave_get(p->depth)->feat[y][x] == FEAT_GLYPH) return (TRUE);

        /* Notice doors */
        if (cave_isopendoor(cave_get(p->depth), y, x)) return (TRUE);
        if (cave_get(p->depth)->feat[y][x] == FEAT_BROKEN) return (TRUE);

        /* Notice stairs */
        if (cave_isupstairs(cave_get(p->depth), y, x)) return (TRUE);
        if (cave_isdownstairs(cave_get(p->depth), y, x)) return (TRUE);

        /* Notice shops */
        if (cave_isshop(cave_get(p->depth), y, x)) return (TRUE);

        /* Notice traps */
        if (cave_isknowntrap(cave_get(p->depth), y, x)) return (TRUE);

        /* Notice doors */
        if (cave_iscloseddoor(cave_get(p->depth), y, x)) return (TRUE);

        /* Notice rubble */
        if (cave_isrubble(cave_get(p->depth), y, x)) return (TRUE);

        /* Notice veins with treasure */
        if (cave_get(p->depth)->feat[y][x] == FEAT_MAGMA_K) return (TRUE);
        if (cave_get(p->depth)->feat[y][x] == FEAT_QUARTZ_K) return (TRUE);

        /* Notice fountains */
        if (cave_fountain_basic(cave_get(p->depth)->feat[y][x])) return (TRUE);

        /* Notice home doors */
        if (cave_get(p->depth)->feat[y][x] == FEAT_HOME_OPEN) return (TRUE);
        if (cave_ishomedoor(cave_get(p->depth), y, x)) return (TRUE);
    }

    /* Nope */
    return (FALSE);
}


static int player_wounded(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    return p_ptr->chp * 1000 / p_ptr->mhp;
}


static int cmp_wounded(const void *a, const void *b)
{
    const struct cmp_loc *pa = a;
    const struct cmp_loc *pb = b;
    int idx1 = 0 - cave_get(player_get(pa->Ind)->depth)->m_idx[pa->y][pa->x];
    int idx2 = 0 - cave_get(player_get(pb->Ind)->depth)->m_idx[pb->y][pb->x];
    int w1 = player_wounded(idx1);
    int w2 = player_wounded(idx2);

    if (w1 < w2) return -1;
    if (w1 > w2) return 1;
    return 0;
}


/*
 * Return a target set of target_able monsters.
 */
static struct point_set *target_set_interactive_prepare(int Ind, int mode)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    struct point_set *targets = point_set_new(TS_INITIAL_SIZE);
    int screen_hgt, screen_wid;

    screen_hgt = p_ptr->screen_rows / p_ptr->tile_hgt;
    screen_wid = p_ptr->screen_cols / p_ptr->tile_wid;

    /* Scan the current panel */
    for (y = p_ptr->offset_y; y < p_ptr->offset_y + screen_hgt; y++)
    {
        for (x = p_ptr->offset_x; x < p_ptr->offset_x + screen_wid; x++)
        {
            int m_idx;

            /* Check bounds */
            if (!in_bounds_fully(y, x)) continue;

            /* Require line of sight */
            if (!player_has_los_bold(p_ptr, y, x)) continue;

            /* Require "interesting" contents */
            if (!target_set_interactive_accept(p_ptr, y, x)) continue;

            m_idx = cave_get(p_ptr->depth)->m_idx[y][x];

            /* Special modes */
            if (mode & (TARGET_KILL))
            {
                /* Must be a targetable monster/player */
                if (!target_able(p_ptr, m_idx)) continue;

                /* Skip non hostile monsters */
                if ((m_idx > 0) && !pvm_check(Ind, m_idx)) continue;

                /* Don't target yourself */
                if ((m_idx < 0) && ((0 - m_idx) == Ind)) continue;

                /* Ignore players we aren't hostile to */
                if ((m_idx < 0) && !pvp_check(p_ptr, player_get(0 - m_idx), PVP_CHECK_BOTH, TRUE,
                    cave_get(p_ptr->depth)->feat[y][x]))
                {
                    continue;
                }
            }
            else if (mode & (TARGET_HELP))
            {
                /* Must contain a player */
                if (m_idx > 0) continue;

                /* Must be a targetable player */
                if (!target_able(p_ptr, m_idx)) continue;

                /* Don't target yourself */
                if ((0 - m_idx) == Ind) continue;

                /* Ignore players we aren't friends with */
                if (pvp_check(p_ptr, player_get(0 - m_idx), PVP_CHECK_BOTH, TRUE, FEAT_NONE))
                    continue;
            }

            /* Save the location */
            add_to_point_set(targets, Ind, y, x);
        }
    }

    /* Sort the positions */
    sort(targets->pts, point_set_size(targets), sizeof(*(targets->pts)),
        ((mode & TARGET_HELP)? cmp_wounded: cmp_distance));

    return targets;
}


/*
 * Display targeting help at the bottom of the screen
 */
static void target_display_help(char *help, size_t len, bool monster, bool free)
{
    /* Display help */
    my_strcpy(help, "[Press <dir>, 'p', 'q', 'r'", len);
    if (free) my_strcat(help, ", 'm'", len);
    else my_strcat(help, ", '+', '-', 'o'", len);
    if (monster || free) my_strcat(help, ", 't'", len);
    my_strcat(help, ", Return, or Space]", len);
}


/*
 * Describe a location relative to the player position.
 * e.g. "12 S, 35 W" or "0 N, 33 E" or "0 N, 0 E"
 */
static void coords_desc(int Ind, char *buf, int size, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    const char *east_or_west;
    const char *north_or_south;
    int py = p_ptr->py;
    int px = p_ptr->px;

    if (y > py) north_or_south = "S";
    else north_or_south = "N";

    if (x < px) east_or_west = "W";
    else east_or_west = "E";

    strnfmt(buf, size, "%d %s, %d %s", ABS(y - py), north_or_south, ABS(x - px), east_or_west);
}


/*
 * Do we need to inform client about target info?
 */
static bool need_target_info(int Ind, u32b query, byte step)
{
    player_type *p_ptr = player_get(Ind);
    bool need_info = FALSE;

    /* Acknowledge */
    if (query == '\0') need_info = TRUE;

    /* Next step */
    if (p_ptr->tt_step < step) need_info = TRUE;

    /* Print help */
    if ((query == KC_ENTER) && (p_ptr->tt_step == step) && p_ptr->tt_help)
        need_info = TRUE;

    /* Advance step */
    if (need_info) p_ptr->tt_step = step;

    /* Clear help */
    else p_ptr->tt_help = FALSE;

    return need_info;
}


/*
 * Inform client about target info
 */
static bool target_info(int Ind, int y, int x, const char *info, const char *help, u32b query)
{
    player_type *p_ptr = player_get(Ind);
    int col = x - p_ptr->offset_x;
    int row = y - p_ptr->offset_y + 1;
    bool dble = TRUE;

    /* Do nothing on quit */
    if ((query == 'q') || (query == ESCAPE)) return FALSE;

    /* Hack -- Is there something targetable above our position? */
    if (in_bounds_fully(y - 1, x) && target_set_interactive_accept(p_ptr, y - 1, x))
        dble = FALSE;

    /* Display help info */
    if (p_ptr->tt_help)
        Send_target_info(Ind, col, row, dble, help);

    /* Display target info */
    else
        Send_target_info(Ind, col, row, dble, info);

    /* Toggle help */
    p_ptr->tt_help = !p_ptr->tt_help;

    return TRUE;
}


/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * This function correctly handles multiple objects per grid, and objects
 * and terrain features in the same grid, though the latter never happens.
 *
 * This function must handle blindness/hallucination.
 */
static bool target_set_interactive_aux(int Ind, int y, int x, int mode, const char *help, u32b query)
{
    player_type *p_ptr = player_get(Ind);
    const char *s1, *s2, *s3;
    bool boring;
    int feat;
    int floor_list[MAX_FLOOR_STACK];
    int floor_num;
    char out_val[256];
    char coords[20];
    int tries = 200;

    /* Describe the square location */
    coords_desc(Ind, coords, sizeof(coords), y, x);

    /* Repeat forever */
    while (tries--)
    {
        /* Assume boring */
        boring = TRUE;

        /* Default */
        s1 = "You see ";
        s2 = "";
        s3 = "";

        /* The player */
        if ((cave_get(p_ptr->depth)->m_idx[y][x] < 0) &&
            ((0 - cave_get(p_ptr->depth)->m_idx[y][x]) == Ind))
        {
            /* Description */
            s1 = "You are ";

            /* Preposition */
            s2 = "on ";
        }

        /* Hallucination messes things up */
        if (p_ptr->timed[TMD_IMAGE])
        {
            const char *name = "something strange";

            /* Display a message */
            strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, name, coords);

            /* Inform client */
            if (need_target_info(Ind, query, TARGET_NONE))
                return target_info(Ind, y, x, out_val, help, query);

            /* Stop on everything but "return" */
            if (query == KC_ENTER)
            {
                query = '\0';
                continue;
            }

            return FALSE;
        }

        /* Actual players */
        if ((cave_get(p_ptr->depth)->m_idx[y][x] < 0) &&
            ((0 - cave_get(p_ptr->depth)->m_idx[y][x]) != Ind))
        {
            player_type *q_ptr = player_get(0 - cave_get(p_ptr->depth)->m_idx[y][x]);

            /* Visible */
            if (p_ptr->play_vis[0 - cave_get(p_ptr->depth)->m_idx[y][x]])
            {
                bool recall = FALSE;
                char player_name[NORMAL_WID];

                /* Not boring */
                boring = FALSE;

                /* Unaware players get a pseudo description */
                if (q_ptr->k_idx)
                {
                    /* Acting as an object: get a pseudo object description */
                    if (q_ptr->k_idx > 0)
                    {
                        object_kind *kind = &k_info[q_ptr->k_idx];
                        object_type obj_fake;

                        object_prep(&obj_fake, kind, 0, MINIMISE);
                        if (kind->tval == TV_GOLD) obj_fake.pval[DEFAULT_PVAL] = 1;
                        object_desc(p_ptr, player_name, sizeof(player_name), &obj_fake,
                            ODESC_PREFIX | ODESC_BASE);
                    }

                    /* Acting as a feature: get a pseudo feature description */
                    else
                    {
                        monster_race *r_ptr = &r_info[q_ptr->r_idx];

                        switch (r_ptr->d_char)
                        {
                            case '+': feat = FEAT_DOOR_HEAD; break;
                            case '<': feat = FEAT_LESS; break;
                            case '>': feat = FEAT_MORE; break;
                            default: feat = FEAT_FLOOR;
                        }
                        my_strcpy(player_name, f_info[feat].name, sizeof(player_name));
                        s3 = (is_a_vowel(player_name[0])? "an ": "a ");
                    }

                    /* Describe the player */
                    strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, player_name,
                        coords);

                    /* Inform client */
                    if (need_target_info(Ind, query, TARGET_MON))
                        return target_info(Ind, y, x, out_val, help, query);

                    /* Stop on everything but "return" */
                    if (query != KC_ENTER) break;

                    /* Paranoia */
                    return TRUE;
                }

                /* Get the player name */
                strnfmt(player_name, sizeof(player_name), "%s the %s %s", q_ptr->name,
                    q_ptr->race->name, q_ptr->clazz->name);

                /* Hack -- Track this player */
                monster_race_track(Ind, -1);

                /* Hack -- Health bar for this player */
                health_track(p_ptr, cave_get(p_ptr->depth)->m_idx[y][x]);

                /* Hack -- Track cursor for this player */
                cursor_track(Ind, cave_get(p_ptr->depth)->m_idx[y][x]);

                /* Hack -- Handle stuff */
                handle_stuff(p_ptr);

                /* Interact */
                if ((query == 'r') && (p_ptr->tt_step == TARGET_MON))
                    recall = TRUE;

                /* Recall */
                if (recall)
                {
                    /* Recall on screen */
                    do_cmd_describe(Ind);
                    return FALSE;
                }

                /* Normal */
                else
                {
                    char buf[NORMAL_WID];

                    /* Describe the player */
                    look_player_desc(0 - cave_get(p_ptr->depth)->m_idx[y][x], buf, sizeof(buf));

                    /* Describe, and prompt for recall */
                    strnfmt(out_val, sizeof(out_val), "%s%s%s%s (%s), %s.",
                        s1, s2, s3, player_name, buf, coords);

                    /* Inform client */
                    if (need_target_info(Ind, query, TARGET_MON))
                        return target_info(Ind, y, x, out_val, help, query);
                }

                /* Stop on everything but "return"/"space" */
                if ((query != KC_ENTER) && (query != ' ')) break;

                /* Sometimes stop at "space" key */
                if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

                /* Take account of gender */
                if (q_ptr->psex == SEX_FEMALE) s1 = "She is ";
                else if (q_ptr->psex == SEX_MALE) s1 = "He is ";
                else s1 = "It is ";

                /* Use a preposition */
                s2 = "on ";
            }
        }

        /* Actual monsters */
        if (cave_get(p_ptr->depth)->m_idx[y][x] > 0)
        {
            monster_type *m_ptr = cave_monster_at(cave_get(p_ptr->depth), y, x);
            monster_race *r_ptr = &r_info[m_ptr->r_idx];

            /* Visible */
            if (p_ptr->mon_vis[cave_get(p_ptr->depth)->m_idx[y][x]] && !m_ptr->unaware)
            {
                bool recall = FALSE;
                char m_name[NORMAL_WID];

                /* Not boring */
                boring = FALSE;

                /* Get the monster name ("a kobold") */
                monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_IND2);

                /* Hack -- Track this monster race */
                monster_race_track(Ind, m_ptr->r_idx);

                /* Hack -- Health bar for this monster */
                health_track(p_ptr, cave_get(p_ptr->depth)->m_idx[y][x]);

                /* Hack -- Track cursor for this monster */
                cursor_track(Ind, cave_get(p_ptr->depth)->m_idx[y][x]);

                /* Hack -- Handle stuff */
                handle_stuff(p_ptr);

                /* Interact */
                if ((query == 'r') && (p_ptr->tt_step == TARGET_MON))
                    recall = TRUE;

                /* Recall */
                if (recall)
                {
                    /* Recall on screen */
                    do_cmd_describe(Ind);
                    return FALSE;
                }

                /* Normal */
                else
                {
                    char buf[NORMAL_WID];

                    /* Describe the monster */
                    look_mon_desc(buf, sizeof(buf), p_ptr->depth,
                        cave_get(p_ptr->depth)->m_idx[y][x]);

                    /* Describe, and prompt for recall */
                    strnfmt(out_val, sizeof(out_val), "%s%s%s%s (%s), %s.", s1, s2, s3, m_name, buf,
                        coords);

                    /* Inform client */
                    if (need_target_info(Ind, query, TARGET_MON))
                        return target_info(Ind, y, x, out_val, help, query);
                }

                /* Stop on everything but "return"/"space" */
                if ((query != KC_ENTER) && (query != ' ')) break;

                /* Sometimes stop at "space" key */
                if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

                /* Take account of gender */
                if (rf_has(r_ptr->flags, RF_FEMALE)) s1 = "She is ";
                else if (rf_has(r_ptr->flags, RF_MALE)) s1 = "He is ";
                else s1 = "It is ";

                /* Disabled for non-DMs since monsters now carry their drops */
                if (is_dm_p(p_ptr))
                {
                    /* Use a verb */
                    s2 = "carrying ";

                    /* Change the intro */
                    if (p_ptr->tt_o) s2 = "also carrying ";

                    /* Scan all objects being carried */
                    if (!p_ptr->tt_o) p_ptr->tt_o = m_ptr->hold_o_idx;
                    else p_ptr->tt_o = object_byid(p_ptr->tt_o)->next_o_idx;
                    if (p_ptr->tt_o)
                    {
                        char o_name[NORMAL_WID];
                        object_type *o_ptr;

                        /* Get the object */
                        o_ptr = object_byid(p_ptr->tt_o);

                        /* Obtain an object description */
                        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

                        /* Describe the object */
                        strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, o_name, coords);

                        /* Inform client */
                        return target_info(Ind, y, x, out_val, help, query);
                    }
                }

                /* Use a preposition */
                s2 = "on ";
            }
        }

        floor_num = scan_floor(p_ptr, floor_list, N_ELEMENTS(floor_list), p_ptr->depth, y, x, 0x02);

        /* Scan all marked objects in the grid */
        if ((floor_num > 0) && (!(p_ptr->timed[TMD_BLIND]) || ((y == p_ptr->py) && (x == p_ptr->px))))
        {
            /* Not boring */
            boring = FALSE;

            /* If there is more than one item... */
            if (floor_num > 1)
            {
                /* Describe the pile */
                strnfmt(out_val, sizeof(out_val), "%s%s%sa pile of %d objects, %s.",
                    s1, s2, s3, floor_num, coords);

                /* Inform client */
                if (need_target_info(Ind, query, TARGET_OBJ))
                    return target_info(Ind, y, x, out_val, help, query);

                /* Display objects */
                if (query == 'r')
                {
                    display_floor(p_ptr, floor_list, floor_num);
                    show_floor(Ind, OLIST_WEIGHT | OLIST_GOLD);
                    return FALSE;
                }

                /* Done */
                break;
            }

            /* Only one object to display */
            else
            {
                char o_name[NORMAL_WID];

                /* Get the single object in the list */
                object_type *o_ptr = object_byid(floor_list[0]);

                /* Not boring */
                boring = FALSE;

                /* Obtain an object description */
                object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

                /* Describe the object */
                strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, o_name, coords);

                /* Inform client */
                if (need_target_info(Ind, query, TARGET_OBJ))
                    return target_info(Ind, y, x, out_val, help, query);

                /* Stop on everything but "return"/"space" */
                if ((query != KC_ENTER) && (query != ' ')) break;

                /* Sometimes stop at "space" key */
                if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

                /* Change the intro */
                s1 = "It is ";

                /* Plurals */
                if (o_ptr->number != 1) s1 = "They are ";

                /* Preposition */
                s2 = "on ";
            }
        }

        /* Feature (apply "mimic") */
        feat = f_info[cave_get(p_ptr->depth)->feat[y][x]].mimic;

        /* Require knowledge about grid, or ability to see grid */
        if (!is_memorized(p_ptr, p_ptr->cave->info[y][x]) && !player_can_see_bold(p_ptr, y, x))
        {
            /* Forget feature */
            feat = FEAT_NONE;
        }

        /* Terrain feature if needed */
        if (boring || (feat > FEAT_INVIS))
        {
            const char *name = f_info[feat].name;

            /* Hack -- handle unknown grids */
            if (feat == FEAT_NONE) name = "unknown grid";

            /* Pick a prefix */
            if (*s2 && (((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_PERM_SOLID)) ||
                ((feat >= FEAT_TREE) && (feat <= FEAT_PERM_CLEAR)) ||
                ((feat >= FEAT_HOME_HEAD) && (feat <= FEAT_HOME_TAIL))))
            {
                s2 = "in ";
            }

            /* Pick proper indefinite article */
            s3 = (is_a_vowel(name[0])? "an ": "a ");

            /* Hack -- special introduction for store doors */
            if ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL))
                s3 = "the entrance to the ";

            /* Message */
            strnfmt(out_val, sizeof(out_val), "%s%s%s%s, %s.", s1, s2, s3, name, coords);

            /* Inform client */
            if (need_target_info(Ind, query, TARGET_FEAT))
                return target_info(Ind, y, x, out_val, help, query);

            /* Stop on everything but "return"/"space" */
            if ((query != KC_ENTER) && (query != ' ')) break;
        }

        /* Stop on everything but "return" */
        if (query != KC_ENTER) break;

        /* Paranoia */
        return TRUE;
    }

    /* Paranoia */
    if (!tries)
        plog_fmt("Infinite loop in target_set_interactive_aux: %c", query);

    /* Keep going */
    return FALSE;
}


/*
 * Extract a direction (or zero) from a character
 */
static int target_dir(u32b ch)
{
    int d = 0;

    /* Already a direction? */
    if (isdigit((unsigned char)ch))
        d = D2I(ch);
    else if (isarrow(ch))
    {
        switch (ch)
        {
            case ARROW_DOWN:  d = 2; break;
            case ARROW_LEFT:  d = 4; break;
            case ARROW_RIGHT: d = 6; break;
            case ARROW_UP:    d = 8; break;
        }
    }

    /* Paranoia */
    if (d == 5) d = 0;

    /* Return direction */
    return (d);
}


static void set_tt_m(int Ind, s16b m)
{
    player_type *p_ptr = player_get(Ind);

    p_ptr->tt_m = m;
    p_ptr->tt_o = 0;
}


/*
 * Target closest monster
 */
bool target_set_closest(int Ind, int mode)
{
    player_type *p_ptr = player_get(Ind);
    int y, x, m_idx;
    monster_type *m_ptr;
    char m_name[NORMAL_WID];
    struct point_set *targets;

    /* Paranoia */
    if (!cave_get(p_ptr->depth)) return FALSE;

    /* Cancel old target */
    target_set_monster(Ind, 0);

    /* Get ready to do targeting */
    targets = target_set_interactive_prepare(Ind, mode);

    /* If nothing was prepared, then return */
    if (point_set_size(targets) < 1)
    {
        msg(p_ptr, "No available target.");
        point_set_dispose(targets);
        return FALSE;
    }

    /* Find the first monster in the queue */
    y = targets->pts[0].y;
    x = targets->pts[0].x;
    m_idx = cave_get(p_ptr->depth)->m_idx[y][x];

    /* Target the monster, if possible */
    if (!target_able(p_ptr, m_idx))
    {
        msg(p_ptr, "No available target.");
        point_set_dispose(targets);
        return FALSE;
    }

    /* Target the monster */
    if (m_idx > 0)
    {
        m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);
        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);
    }

    /* Target the player */
    else
        player_desc(p_ptr, m_name, sizeof(m_name), player_get(0 - m_idx), TRUE);

    if (!(mode & TARGET_QUIET)) msg(p_ptr, "%s is targeted.", m_name);

    /* Set up target information */
    if (m_idx > 0) monster_race_track(Ind, m_ptr->r_idx);
    health_track(p_ptr, m_idx);
    target_set_monster(Ind, m_idx);

    point_set_dispose(targets);
    return TRUE;
}


/*
 * Draw a visible path over the squares between (x1,y1) and (x2,y2).
 *
 * The path consists of "*", which are white except where there is a
 * monster, object or feature in the grid.
 *
 * This routine has (at least) three weaknesses:
 * - remembered objects/walls which are no longer present are not shown,
 * - squares which (e.g.) the player has walked through in the dark are
 *   treated as unknown space.
 * - walls which appear strange due to hallucination aren't treated correctly.
 *
 * The first two result from information being lost from the dungeon arrays,
 * which requires changes elsewhere.
 */
int draw_path(struct player *p, u16b path_n, u16b *path_g, int y1, int x1)
{
    int i;
    bool on_screen;
    struct cave *c = cave_get(p->depth);

    /* No path, so do nothing. */
    if (path_n < 1) return 0;

    /*
     * The starting square is never drawn, but notice if it is being
     * displayed. In theory, it could be the last such square.
     */
    on_screen = panel_contains(p, y1, x1);

    /* Draw the path. */
    for (i = 0; i < path_n; i++)
    {
        byte colour;

        /* Find the co-ordinates on the level. */
        int y = GRID_Y(path_g[i]);
        int x = GRID_X(path_g[i]);

        /*
         * As path[] is a straight line and the screen is oblong,
         * there is only section of path[] on-screen.
         * If the square being drawn is visible, this is part of it.
         * If none of it has been drawn, continue until some of it
         * is found or the last square is reached.
         * If some of it has been drawn, finish now as there are no
         * more visible squares to draw.
         */
        if (panel_contains(p, y, x)) on_screen = TRUE;
        else if (on_screen) break;
        else continue;

        /* Choose a colour (monsters). */
        if ((c->m_idx[y][x] > 0) && p->mon_vis[c->m_idx[y][x]])
        {
            /* Visible monsters are red. */
            monster_type *m_ptr = cave_monster_at(c, y, x);

            /* Mimics act as objects */
            if (m_ptr->unaware && m_ptr->mimicked_o_idx)
                colour = TERM_YELLOW;
            else
                colour = TERM_L_RED;
        }

        /* Choose a colour (players). */
        else if ((c->m_idx[y][x] < 0) && p->play_vis[0 - c->m_idx[y][x]])
        {
            /* Visible players are red. */
            player_type *q_ptr = player_get(0 - c->m_idx[y][x]);

            /* Player mimics act as objects (or features) */
            if (q_ptr->k_idx > 0)
                colour = TERM_YELLOW;
            else if (q_ptr->k_idx < 0)
                colour = TERM_WHITE;
            else
                colour = TERM_L_RED;
        }

        /* Known objects are yellow. */
        else if (c->o_idx[y][x] && object_marked(p, c->o_idx[y][x]))
            colour = TERM_YELLOW;

        /* Known walls are blue. */
        else if (!cave_floor_bold(p->depth, y, x) &&
            ((p->cave->info[y][x] & CAVE_MARK) || player_can_see_bold(p, y, x)))
        {
            colour = TERM_BLUE;
        }

        /* Unknown squares are grey. */
        else if (!(p->cave->info[y][x] & CAVE_MARK) && !player_can_see_bold(p, y, x))
            colour = TERM_L_DARK;

        /* Unoccupied squares are white. */
        else
            colour = TERM_WHITE;

        /* Draw the path segment */
        draw_path_grid(p, y, x, colour, '*');
    }

    /* Flush and wait (delay for consistency) */
    if (i) Send_flush(p, TRUE, TRUE);
    else Send_flush(p, TRUE, FALSE);

    return i;
}


/*
 * Load the attr/char at each point along "path" which is on screen.
 */
void load_path(struct player *p, u16b path_n, u16b *path_g)
{
    int i;

    for (i = 0; i < path_n; i++)
    {
        int y = GRID_Y(path_g[i]);
        int x = GRID_X(path_g[i]);

        if (!panel_contains(p, y, x)) continue;
        cave_light_spot_aux(p, cave_get(p->depth), y, x);
    }

    Send_flush(p, TRUE, FALSE);
    p->path_drawn = FALSE;
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
 *
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 *
 * 'mode' is one of TARGET_LOOK or TARGET_KILL.
 * 'x' and 'y' are the initial position of the target to be highlighted,
 * or -1 if no location is specified.
 * Returns TRUE if a target has been successfully set, FALSE otherwise.
 */
bool target_set_interactive(int Ind, int mode, u32b query)
{
    player_type *p_ptr = player_get(Ind);
    int py = p_ptr->py;
    int px = p_ptr->px;
    int i, d, t, bd;
    bool done = FALSE;
    struct point_set *targets;
    bool good_target;
    char help[NORMAL_WID];
    s16b old_target_who, old_target_x, old_target_y;
    bool auto_target = FALSE;
    int tries = 200;

    /* Paranoia */
    if (!cave_get(p_ptr->depth)) return FALSE;

    /* Remove old targeting path */
    if (p_ptr->path_drawn) load_path(p_ptr, p_ptr->path_n, p_ptr->path_g);

    /* Hack -- Auto-target if requested */
    if ((mode & (TARGET_AIM)) && OPT_P(p_ptr, use_old_target) && target_okay(p_ptr))
    {
        old_target_who = p_ptr->target_who;
        old_target_x = p_ptr->target_x;
        old_target_y = p_ptr->target_y;
        auto_target = TRUE;
    }

    if (query == '\0')
    {
        p_ptr->tt_flag = TRUE;
        p_ptr->tt_step = TARGET_NONE;
        p_ptr->tt_help = FALSE;
    }

    /* Start on the player */
    if (query == '\0')
    {
        p_ptr->tt_x = p_ptr->px;
        p_ptr->tt_y = p_ptr->py;

        /* Hack -- Auto-target if requested */
        if (auto_target)
        {
            p_ptr->tt_x = old_target_x;
            p_ptr->tt_y = old_target_y;
        }
    }

    /* Cancel target */
	target_set_monster(Ind, 0);

    /* Cancel tracking */
    cursor_track(Ind, 0);

    /* Prepare the "temp" array */
    targets = target_set_interactive_prepare(Ind, mode);

    /* Start near the player */
    if (query == '\0')
    {
        set_tt_m(Ind, 0);

        /* Hack -- Auto-target if requested */
        if (auto_target)
        {
            /* Find the old target */
            for (i = 0; i < point_set_size(targets); i++)
            {
                int temp_y = targets->pts[i].y;
                int temp_x = targets->pts[i].x;

                if (cave_get(p_ptr->depth)->m_idx[temp_y][temp_x] == old_target_who)
                {
                    set_tt_m(Ind, i);
                    break;
                }
            }
        }
    }

    /* Interact */
    while (tries-- && !done)
    {
        bool ret;

        /* Paranoia: grids could have changed! */
        if (p_ptr->tt_m >= point_set_size(targets)) set_tt_m(Ind, point_set_size(targets) - 1);
        if (p_ptr->tt_m < 0) set_tt_m(Ind, 0);

#ifdef NOTARGET_PROMPT
        /* No targets */
        if (p_ptr->tt_flag && !point_set_size(targets))
        {
            /* Analyze */
            switch (query)
            {
                case ESCAPE:
                case 'q': break;

                case 'p':
                {
                    p_ptr->tt_flag = FALSE;
                    break;
                }

                default:
                {
                    int col = p_ptr->px - p_ptr->offset_x;
                    int row = p_ptr->py - p_ptr->offset_y + 1;
                    bool dble = TRUE;

                    /* Hack -- Is there something targetable above our position? */
                    if (in_bounds_fully(p_ptr->py - 1, p_ptr->px) &&
                        target_set_interactive_accept(p_ptr, p_ptr->py - 1, p_ptr->px))
                    {
                        dble = FALSE;
                    }

                    Send_target_info(Ind, col, row, dble, "Nothing to target. [p,q]");
                    point_set_dispose(targets);
                    return FALSE;
                }
            }
        }
#endif

        /* Interesting grids */
        if (p_ptr->tt_flag && point_set_size(targets))
        {
            p_ptr->tt_y = targets->pts[p_ptr->tt_m].y;
            p_ptr->tt_x = targets->pts[p_ptr->tt_m].x;

            /* Adjust panel if needed */
            if (adjust_panel(Ind, p_ptr->tt_y, p_ptr->tt_x)) handle_stuff(p_ptr);

            /* Update help */
            good_target = target_able(p_ptr, cave_get(p_ptr->depth)->m_idx[p_ptr->tt_y][p_ptr->tt_x]);
            target_display_help(help, sizeof(help), good_target, FALSE);

            /* Find the path. */
            p_ptr->path_n = project_path(p_ptr->path_g, MAX_RANGE, p_ptr->depth, py, px, p_ptr->tt_y,
                p_ptr->tt_x, PROJECT_THRU);

            /* Draw the path in "target" mode. If there is one */
            if (mode & TARGET_KILL)
                p_ptr->path_drawn = draw_path(p_ptr, p_ptr->path_n, p_ptr->path_g, py, px);

            /* Describe and Prompt */
            ret = target_set_interactive_aux(Ind, p_ptr->tt_y, p_ptr->tt_x, mode, help, query);

            if (ret)
            {
                point_set_dispose(targets);
                return FALSE;
            }

            /* Remove the path */
            if (p_ptr->path_drawn) load_path(p_ptr, p_ptr->path_n, p_ptr->path_g);

            /* Assume no "direction" */
            d = 0;

            /* Analyze */
            switch (query)
            {
                case ESCAPE:
                case 'q':
                case 'r':
                {
                    done = TRUE;
                    break;
                }

                case ' ':
                case '(':
                case '*':
                case '+':
                {
                    set_tt_m(Ind, p_ptr->tt_m + 1);
                    if (p_ptr->tt_m == point_set_size(targets)) set_tt_m(Ind, 0);

                    /* Hack -- Acknowledge */
                    query = '\0';

                    break;
                }

                case '-':
                {
                    set_tt_m(Ind, p_ptr->tt_m - 1);
                    if (p_ptr->tt_m == -1) set_tt_m(Ind, point_set_size(targets) - 1);

                    /* Hack -- Acknowledge */
                    query = '\0';

                    break;
                }

                case 'p':
                {
                    /* Recenter around player */
                    verify_panel(p_ptr);

                    /* Handle stuff */
                    handle_stuff(p_ptr);

                    p_ptr->tt_y = p_ptr->py;
                    p_ptr->tt_x = p_ptr->px;
                }

                case 'o':
                {
                    p_ptr->tt_flag = FALSE;

                    /* Hack -- Acknowledge */
                    query = '\0';

                    break;
                }

                case 'm':
                {
                    /* Hack -- Acknowledge */
                    query = '\0';

                    break;
                }

                case 't':
                case '5':
                case '0':
                case '.':
                {
                    int m_idx = cave_get(p_ptr->depth)->m_idx[p_ptr->tt_y][p_ptr->tt_x];

                    if (target_able(p_ptr, m_idx))
                    {
                        health_track(p_ptr, m_idx);
                        target_set_monster(Ind, m_idx);
                    }

                    done = TRUE;

                    break;
                }

                default:
                {
                    /* Extract direction */
                    d = target_dir(query);

                    /* Oops */
                    if (!d)
                    {
                        /* Hack -- Acknowledge */
                        if (query != KC_ENTER) query = '\0';
                    }

                    break;
                }
            }

            /* Hack -- Move around */
            if (d)
            {
                int old_y = targets->pts[p_ptr->tt_m].y;
                int old_x = targets->pts[p_ptr->tt_m].x;

                /* Find a new monster */
                i = target_pick(old_y, old_x, ddy[d], ddx[d], targets);

                /* Scroll to find interesting grid */
                if (i < 0)
                {
                    int old_wy = p_ptr->offset_y;
                    int old_wx = p_ptr->offset_x;

                    /* Change if legal */
                    if (change_panel(p_ptr, d))
                    {
                        /* Recalculate interesting grids */
                        point_set_dispose(targets);
                        targets = target_set_interactive_prepare(Ind, mode);

                        /* Find a new monster */
                        i = target_pick(old_y, old_x, ddy[d], ddx[d], targets);

                        /* Restore panel if needed */
                        if ((i < 0) && modify_panel(p_ptr, old_wy, old_wx))
                        {
                            /* Recalculate interesting grids */
                            point_set_dispose(targets);
                            targets = target_set_interactive_prepare(Ind, mode);
                        }

                        /* Handle stuff */
                        handle_stuff(p_ptr);
                    }
                }

                /* Use interesting grid if found */
                if (i >= 0) set_tt_m(Ind, i);

                /* Hack -- Acknowledge */
                query = '\0';
            }
        }

        /* Arbitrary grids */
        else
        {
            /* Update help */
            good_target = target_able(p_ptr, cave_get(p_ptr->depth)->m_idx[p_ptr->tt_y][p_ptr->tt_x]);
            target_display_help(help, sizeof(help), good_target, TRUE);

            /* Find the path. */
            p_ptr->path_n = project_path(p_ptr->path_g, MAX_RANGE, p_ptr->depth, py, px, p_ptr->tt_y,
                p_ptr->tt_x, PROJECT_THRU);

            /* Draw the path in "target" mode. If there is one */
            if (mode & TARGET_KILL)
                p_ptr->path_drawn = draw_path(p_ptr, p_ptr->path_n, p_ptr->path_g, py, px);

            /* Describe and Prompt (enable "TARGET_LOOK") */
            ret = target_set_interactive_aux(Ind, p_ptr->tt_y, p_ptr->tt_x, mode | TARGET_LOOK,
                help, query);

            if (ret)
            {
                point_set_dispose(targets);
                return FALSE;
            }

            /* Remove the path */
            if (p_ptr->path_drawn) load_path(p_ptr, p_ptr->path_n, p_ptr->path_g);

            /* Assume no "direction" */
            d = 0;

            /* Analyze */
            switch (query)
            {
                case ESCAPE:
                case 'q':
                case 'r':
                {
                    done = TRUE;
                    break;
                }

                case ' ':
                case '(':
                case '*':
                case '+':
                case '-':
                {
                    /* Hack -- Acknowledge */
                    query = '\0';

                    break;
                }

                case 'p':
                {
                    /* Recenter around player */
                    verify_panel(p_ptr);

                    /* Handle stuff */
                    handle_stuff(p_ptr);

                    p_ptr->tt_y = p_ptr->py;
                    p_ptr->tt_x = p_ptr->px;
                }

                case 'o':
                {
                    /* Hack -- Acknowledge */
                    query = '\0';

                    break;
                }

                case 'm':
                {
                    p_ptr->tt_flag = TRUE;

                    set_tt_m(Ind, 0);
                    bd = 999;

                    /* Pick a nearby monster */
                    for (i = 0; i < point_set_size(targets); i++)
                    {
                        t = distance(p_ptr->tt_y, p_ptr->tt_x, targets->pts[i].y, targets->pts[i].x);

                        /* Pick closest */
                        if (t < bd)
                        {
                            set_tt_m(Ind, i);
                            bd = t;
                        }
                    }

                    /* Nothing interesting */
                    if (bd == 999) p_ptr->tt_flag = FALSE;

                    /* Hack -- Acknowledge */
                    query = '\0';

                    break;
                }

                case 't':
                case '5':
                case '0':
                case '.':
                {
                    target_set_location(Ind, p_ptr->tt_y, p_ptr->tt_x);
                    done = TRUE;

                    break;
                }

                default:
                {
                    /* Extract a direction */
                    d = target_dir(query);

                    /* Oops */
                    if (!d)
                    {
                        /* Hack -- Acknowledge */
                        if (query != KC_ENTER) query = '\0';
                    }

                    break;
                }
            }

            /* Handle "direction" */
            if (d)
            {
                /* Move */
                p_ptr->tt_x += ddx[d];
                p_ptr->tt_y += ddy[d];

                /* Slide into legality */
                if (p_ptr->tt_x >= DUNGEON_WID - 1) p_ptr->tt_x--;
                else if (p_ptr->tt_x <= 0) p_ptr->tt_x++;

                /* Slide into legality */
                if (p_ptr->tt_y >= DUNGEON_HGT - 1) p_ptr->tt_y--;
                else if (p_ptr->tt_y <= 0) p_ptr->tt_y++;

                /* Adjust panel if needed */
                if (adjust_panel(Ind, p_ptr->tt_y, p_ptr->tt_x))
                {
                    /* Handle stuff */
                    handle_stuff(p_ptr);

                    /* Recalculate interesting grids */
                    point_set_dispose(targets);
                    targets = target_set_interactive_prepare(Ind, mode);
                }

                /* Hack -- Acknowledge */
                query = '\0';
            }
        }
    }

    /* Paranoia */
    if (!tries) plog_fmt("Infinite loop in target_set_interactive: %lu", query);

    /* Forget */
    point_set_dispose(targets);

    /* Recenter around player */
    verify_panel(p_ptr);

    /* Handle stuff */
    handle_stuff(p_ptr);

    /* Failure to set target */
    if (!p_ptr->target_set) return (FALSE);

    /* Success */
    return (TRUE);
}


/*
 * Obtains the location the player currently targets.
 *
 * Both `col` and `row` must point somewhere, and on function termination,
 * contain the X and Y locations respectively.
 */
void target_get(struct player *p, s16b *col, s16b *row)
{
    *col = p->target_x;
    *row = p->target_y;
}


/*
 * Returns the currently targeted monster index.
 */
s16b target_get_monster(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    return p_ptr->target_who;
}


void draw_path_grid(struct player *p, int y, int x, byte a, char c)
{
    int dispy, dispx;

    /* Draw, Highlight, Fresh, Pause, Erase */
    dispx = x - p->offset_x;
    dispy = y - p->offset_y + 1;

    /* Remember the projectile */
    p->scr_info[dispy][dispx].c = c;
    p->scr_info[dispy][dispx].a = a;

    /* Tell the client */
    Send_char(p, dispx, dispy, a, c, p->trn_info[dispy][dispx].a, p->trn_info[dispy][dispx].c);
}


void flush_path_grid(struct player *p, int depth, int y, int x, byte a, char c)
{
    /* Draw, Highlight, Fresh, Pause, Erase */
    draw_path_grid(p, y, x, a, c);

    /* Flush and wait */
    Send_flush(p, TRUE, TRUE);

    /* Restore */
    cave_light_spot_aux(p, cave_get(depth), y, x);

    Send_flush(p, TRUE, FALSE);
}
