/*
 * File: spells2.c
 * Purpose: Various assorted spell effects
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "generate.h"
#include "monster/mon-lore.h"
#include "monster/mon-make.h"
#include "monster/mon-msg.h"
#include "monster/mon-spell.h"
#include "monster/mon-timed.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "object/inventory.h"
#include "object/slays.h"
#include "s-spells.h"
#include "squelch.h"
#include "target.h"


/*
 * Increase players hit points, notice effects
 */
bool hp_player_safe(struct player *p, int num)
{
    /* Healing needed */
    if (p->chp < p->mhp)
    {
        int old_num = get_player_num(p);

        /* Gain hitpoints */
        p->chp += num;

        /* Enforce maximum */
        if (p->chp >= p->mhp)
        {
            p->chp = p->mhp;
            p->chp_frac = 0;
        }

        /* Hack -- Redraw picture */
        redraw_picture(p, old_num);

        /* Redraw */
        p->redraw |= (PR_HP);

        /* Print a nice message */
        if (num < 5) msg(p, "You feel a little better.");
        else if (num < 15) msg(p, "You feel better.");
        else if (num < 35) msg(p, "You feel much better.");
        else msg(p, "You feel very good.");

        /* Notice */
        return (TRUE);
    }

    /* Ignore */
    return (FALSE);
}


bool hp_player(struct player *p, int num)
{
    if (player_undead(p))
    {
        take_hit(p, num, "a bad healing medicine", FALSE);
        return TRUE;
    }

    return hp_player_safe(p, num);
}


/*
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(struct player *p)
{
    int py = p->py;
    int px = p->px;
    int depth = p->depth;

    /* Only on random levels */
    if (!random_level(depth))
    {
        msg(p, "You cannot create glyphs here...");
        return;
    }

    /* Require clean space */
    if (!cave_isfloor(cave_get(depth), py, px))
    {
        msg(p, "There is no clear floor on which to cast the spell.");
        return;
    }

    /* Push objects off the grid */
    if (cave_get(depth)->o_idx[py][px]) push_object(p, py, px);

    /* Create a glyph of warding */
    cave_set_feat(cave_get(depth), py, px, FEAT_GLYPH);
    msg_misc(p, " lays down a glyph of protection.");
}


/*
 * Array of stat "descriptions"
 */
static const char *desc_stat_pos[] =
{
    "strong",
    "smart",
    "wise",
    "dextrous",
    "healthy",
    "cute"
};


/*
 * Array of stat "descriptions"
 */
const char *desc_stat_neg[] =
{
    "weak",
    "stupid",
    "naive",
    "clumsy",
    "sickly",
    "ugly"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(struct player *p, int stat, bool perma)
{
    bool sust = FALSE;

    /* Get the "sustain" */
    switch (stat)
    {
        case A_STR:
            wieldeds_notice_flag(p, OF_SUST_STR);
            if (check_state(p, OF_SUST_STR)) sust = TRUE;
            break;
        case A_INT:
            wieldeds_notice_flag(p, OF_SUST_INT);
            if (check_state(p, OF_SUST_INT)) sust = TRUE;
            break;
        case A_WIS:
            wieldeds_notice_flag(p, OF_SUST_WIS);
            if (check_state(p, OF_SUST_WIS)) sust = TRUE;
            break;
        case A_DEX:
            wieldeds_notice_flag(p, OF_SUST_DEX);
            if (check_state(p, OF_SUST_DEX)) sust = TRUE;
            break;
        case A_CON:
            wieldeds_notice_flag(p, OF_SUST_CON);
            if (check_state(p, OF_SUST_CON)) sust = TRUE;
            break;
        case A_CHR:
            wieldeds_notice_flag(p, OF_SUST_CHR);
            if (check_state(p, OF_SUST_CHR)) sust = TRUE;
            break;
    }

    /* Sustain */
    if (sust && !perma)
    {
        /* Message */
        msg(p, "You feel very %s for a moment, but the feeling passes.",
            desc_stat_neg[stat]);

        /* Notice effect */
        return (TRUE);
    }

    /* Attempt to reduce the stat */
    if (player_stat_dec(p, stat, perma))
    {
        /* Message */
        msgt(p, MSG_DRAIN_STAT, "You feel very %s.", desc_stat_neg[stat]);

        /* Notice effect */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(struct player *p, int stat)
{
    /* Attempt to increase */
    if (res_stat(p, stat))
    {
        /* Message */
        msg(p, "You feel less %s.", desc_stat_neg[stat]);

        /* Notice */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}


/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(struct player *p, int stat)
{
    bool res;

    /* Restore stat */
    res = res_stat(p, stat);

    /* Attempt to increase */
    if (player_stat_inc(p, stat))
    {
        /* Message */
        msg(p, "Wow!  You feel very %s!", desc_stat_pos[stat]);

        /* Notice */
        return (TRUE);
    }

    /* Restoration worked */
    if (res)
    {
        /* Message */
        msg(p, "You feel less %s.", desc_stat_neg[stat]);

        /* Notice */
        return (TRUE);
    }

    /* Nothing obvious */
    return (FALSE);
}


/*
 * Identify everything being carried.
 */
void identify_pack(struct player *p)
{
    int i;

    /* Simply identify and know every item */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &p->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Aware and Known */
        if (object_is_known(p, o_ptr)) continue;

        /* Identify it */
        do_ident_item(p, i, o_ptr);

        /* Repeat with same slot (in case pack got reordered) */
        i--;
    }
}


/*
 * Hack -- Removes curse from an object.
 */
static void uncurse_object(object_type *o_ptr)
{
    bitflag f[OF_SIZE];

    create_mask(f, FALSE, OFT_CURSE, OFT_MAX);

    of_diff(o_ptr->flags, f);
}


/*
 * Removes curses from items in inventory.
 *
 * "heavy" removes heavy curses if true
 *
 * Returns number of items uncursed
 */
static int remove_curse_aux(struct player *p, bool heavy)
{
    int i, cnt = 0;

    /* Attempt to uncurse items */
    /* Do it backwards to start with worn items first */
    for (i = ALL_INVEN_TOTAL - 1; i >= 0; i--)
    {
        object_type *o_ptr = &p->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Uncursed already */
        if (!cursed_p(o_ptr->flags)) continue;

        /* Heavily cursed items need a special spell */
        if (of_has(o_ptr->flags, OF_HEAVY_CURSE) && !heavy) continue;

        /* Perma-cursed items can never be uncursed */
        if (of_has(o_ptr->flags, OF_PERMA_CURSE)) continue;

        /* Uncurse, and update things */
        uncurse_object(o_ptr);
        p->update |= (PU_BONUS);
        p->redraw |= (PR_INVEN | PR_EQUIP);

        /* Count the uncursings */
        cnt++;

        /* Remove curse only works once */
        if (!heavy) break;
    }

    /* Return "something uncursed" */
    return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(struct player *p)
{
    return (remove_curse_aux(p, FALSE));
}


/*
 * Remove all curses
 */
bool remove_all_curse(struct player *p)
{
    return (remove_curse_aux(p, TRUE));
}


/*
 * Restores any drained experience
 */
bool restore_level(struct player *p)
{
    /* Restore experience */
    if (p->exp < p->max_exp)
    {
        /* Message */
        msg(p, "You feel your life energies returning.");

        /* Restore the experience */
        player_exp_gain(p, p->max_exp - p->exp);

        /* Did something */
        return (TRUE);
    }

    /* No effect */
    return (FALSE);
}


/*** Detection spells ***/

/*
 * Useful constants for the area around the player to detect.
 * This is instead of using circular detection spells.
 */
#define DETECT_DIST_X   40  /* Detect 40 grids to the left & right */
#define DETECT_DIST_Y   22  /* Detect 22 grids to the top & bottom */

/*
 * Map an area around the player.
 *
 * We must never attempt to map the outer dungeon walls, or we
 * might induce illegal cave grid references.
 */
void map_area(struct player *p)
{
    int i, x, y;
    int x1, x2, y1, y2;

    /* Pick an area to map */
    y1 = p->py - DETECT_DIST_Y;
    y2 = p->py + DETECT_DIST_Y;
    x1 = p->px - DETECT_DIST_X;
    x2 = p->px + DETECT_DIST_X;

    /* Speed -- shrink to fit legal bounds */
    if (y1 < 1) y1 = 1;
    if (y2 > DUNGEON_HGT - 2) y2 = DUNGEON_HGT - 2;
    if (x1 < 1) x1 = 1;
    if (x2 > DUNGEON_WID - 2) x2 = DUNGEON_WID - 2;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* All non-walls are "checked" */
            if (!cave_wall_basic(cave_get(p->depth)->feat[y][x]))
            {
                /* Memorize normal features */
                if (!cave_floor_basic(cave_get(p->depth)->feat[y][x]))
                {
                    /* Memorize the object */
                    p->cave->info[y][x] |= CAVE_MARK;
                    cave_light_spot_aux(p, cave_get(p->depth), y, x);
                }

                /* Memorize known walls */
                for (i = 0; i < 8; i++)
                {
                    int yy = y + ddy_ddd[i];
                    int xx = x + ddx_ddd[i];

                    /* Memorize walls (etc) */
                    if (cave_wall_basic(cave_get(p->depth)->feat[yy][xx]))
                    {
                        /* Memorize the walls */
                        p->cave->info[yy][xx] |= CAVE_MARK;
                        cave_light_spot_aux(p, cave_get(p->depth), yy, xx);
                    }
                }
            }
        }
    }
}


/*
 * Detect traps around the player.
 */
bool detect_traps(struct player *p)
{
    int y, x;
    int x1, x2, y1, y2;
    bool detect = FALSE;
    object_type *o_ptr;

    /* Pick an area to map */
    y1 = p->py - DETECT_DIST_Y;
    y2 = p->py + DETECT_DIST_Y;
    x1 = p->px - DETECT_DIST_X;
    x2 = p->px + DETECT_DIST_X;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!in_bounds_fully(y, x)) continue;

            /* Detect invisible traps */
            if (cave_issecrettrap(cave_get(p->depth), y, x))
            {
                /* Pick a trap */
                pick_trap(p->depth, y, x);
            }

            /* Detect traps */
            if (cave_isknowntrap(cave_get(p->depth), y, x))
            {
                /* Hack -- Memorize */
                p->cave->info[y][x] |= CAVE_MARK;

                /* We found something to detect */
                detect = TRUE;
            }

            /* Scan all objects in the grid to look for traps on chests */
            for (o_ptr = get_first_object(p->depth, y, x); o_ptr; o_ptr = get_next_object(o_ptr))
            {
                /* Skip anything not a trapped chest */
                if (!is_trapped_chest(o_ptr)) continue;

                /* Identify once */
                if (!object_is_known(p, o_ptr))
                {
                    /* Know the trap */
                    object_notice_everything(p, o_ptr, TRUE);

                    /* Notice it */
                    disturb(p, 0, 0);

                    /* We found something to detect */
                    detect = TRUE;
                }
            }

            /* Mark as trap-detected */
            p->cave->info[y][x] |= CAVE_DTRAP;
        }
    }        

    /* Rescan the map for the new dtrap edge */
    for (y = y1 - 1; y <= y2 + 1; y++)
    {
        for (x = x1 - 1; x <= x2 + 1; x++)
        {
            if (!in_bounds_fully(y, x)) continue;

            /* See if this grid is on the edge */
            if (dtrap_edge(p, y, x))
                p->cave->info[y][x] |= CAVE_DEDGE;
            else
                p->cave->info[y][x] &= ~CAVE_DEDGE;

            /* Redraw */
            cave_light_spot_aux(p, cave_get(p->depth), y, x);
        }
    }

    /* Describe */
    if (detect)
    {
        msg(p, "You sense the presence of traps!");
        party_msg_near(p, " senses the presence of traps!");
    }

    /* Trap detection always makes you aware, even if no traps are present */
    else
        msg(p, "You sense no traps.");

    /* Mark the redraw flag */
    p->redraw |= (PR_DTRAP);

    return (TRUE);
}


/*
 * Detect doors and stairs around the player.
 */
bool detect_doorstairs(struct player *p, bool aware)
{
    int y, x;
    int x1, x2, y1, y2;
    bool doors = FALSE, stairs = FALSE;

    /* Pick an area to map */
    y1 = p->py - DETECT_DIST_Y;
    y2 = p->py + DETECT_DIST_Y;
    x1 = p->px - DETECT_DIST_X;
    x2 = p->px + DETECT_DIST_X;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!in_bounds_fully(y, x)) continue;

            /* Detect secret doors */
            if (cave_issecretdoor(cave_get(p->depth), y, x))
            {
                /* Pick a door */
                place_closed_door(cave_get(p->depth), y, x);
            }

            /* Detect doors */
            if (cave_iscloseddoor(cave_get(p->depth), y, x) ||
                cave_isopendoor(cave_get(p->depth), y, x) ||
                (cave_get(p->depth)->feat[y][x] == FEAT_BROKEN))
            {
                /* Memorize the door */
                p->cave->info[y][x] |= CAVE_MARK;

                /* Redraw */
                cave_light_spot_aux(p, cave_get(p->depth), y, x);

                /* Obvious */
                doors = TRUE;
            }

            /* Detect stairs */
            if (cave_isstairs(cave_get(p->depth), y, x))
            {
                /* Memorize the stairs */
                p->cave->info[y][x] |= CAVE_MARK;

                /* Redraw */
                cave_light_spot_aux(p, cave_get(p->depth), y, x);

                /* Obvious */
                stairs = TRUE;
            }
        }
    }

    /* Describe */
    if (doors && !stairs)
    {
        msg(p, "You sense the presence of doors!");
        party_msg_near(p, " senses the presence of doors!");
    }
    else if (!doors && stairs)
    {
        msg(p, "You sense the presence of stairs!");
        party_msg_near(p, " senses the presence of stairs!");
    }
    else if (doors && stairs)
    {
        msg(p, "You sense the presence of doors and stairs!");
        party_msg_near(p, " senses the presence of doors and stairs!");
    }
    else if (aware)
        msg(p, "You sense no doors or stairs.");

    return (doors || stairs);
}


/*
 * Detect all treasure around the player.
 */
bool detect_treasure(struct player *p, bool aware, bool full)
{
    int i;
    int y, x;
    int x1, x2, y1, y2;
    bool gold_buried = FALSE;
    bool objects = FALSE;

    /* Hack -- DM has full detection */
    if (p->dm_flags & DM_SEE_LEVEL) full = TRUE;

    /* Pick an area to map */
    y1 = p->py - DETECT_DIST_Y;
    y2 = p->py + DETECT_DIST_Y;
    x1 = p->px - DETECT_DIST_X;
    x2 = p->px + DETECT_DIST_X;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!in_bounds_fully(y, x)) continue;

            /* Notice embedded gold */
            if ((cave_get(p->depth)->feat[y][x] == FEAT_MAGMA_H) ||
                (cave_get(p->depth)->feat[y][x] == FEAT_QUARTZ_H))
            {
                /* Expose the gold */
                cave_get(p->depth)->feat[y][x] += 0x02;
            }

            /* Magma/Quartz + Known Gold */
            if ((cave_get(p->depth)->feat[y][x] == FEAT_MAGMA_K) ||
                (cave_get(p->depth)->feat[y][x] == FEAT_QUARTZ_K))
            {
                /* Hack -- Memorize */
                p->cave->info[y][x] |= CAVE_MARK;

                /* Redraw */
                cave_light_spot_aux(p, cave_get(p->depth), y, x);

                /* Detect */
                gold_buried = TRUE;
            }
        }
    }

    /* Scan objects */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = object_byid(i);

        /* Skip dead objects */
        if (!o_ptr->kind) continue;

        /* Skip held objects */
        if (o_ptr->held_m_idx) continue;

        /* Location */
        y = o_ptr->iy;
        x = o_ptr->ix;

        /* Only detect nearby objects */
        if (o_ptr->depth != p->depth) continue;
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Memorize it */
        if (p->obj_marked[i] < MARK_SEEN)
            p->obj_marked[i] = (full? MARK_SEEN: MARK_AWARE);

        /* Redraw */
        p->redraw |= PR_ITEMLIST;

        /* Redraw */
        cave_light_spot_aux(p, cave_get(p->depth), y, x);

        /* Detect */
        if (!squelch_item_ok(p, o_ptr) || !full) objects = TRUE;
    }

    if (gold_buried)
    {
        msg(p, "You sense the presence of buried treasure!");
        party_msg_near(p, " senses the presence of buried treasure!");
    }
    if (objects)
    {
        msg(p, "You sense the presence of objects!");
        party_msg_near(p, " senses the presence of objects!");
    }
    if (aware && !gold_buried && !objects)
        msg(p, "You sense no treasure or objects.");

    return (gold_buried || objects);
}


/*
 * Quietly detect all buried treasure near the player.
 */
bool detect_close_buried_treasure(struct player *p)
{
    int y, x;
    int x1, x2, y1, y2;
    bool gold_buried = FALSE;

    /* Pick a small area to map */
    y1 = p->py - 3;
    y2 = p->py + 3;
    x1 = p->px - 3;
    x2 = p->px + 3;

    /* Scan the dungeon */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            if (!in_bounds_fully(y, x)) continue;

            /* Notice embedded gold */
            if ((cave_get(p->depth)->feat[y][x] == FEAT_MAGMA_H) ||
                (cave_get(p->depth)->feat[y][x] == FEAT_QUARTZ_H))
            {
                /* Expose the gold */
                cave_get(p->depth)->feat[y][x] += 0x02;
            }

            /* Magma/Quartz + Known Gold */
            if ((cave_get(p->depth)->feat[y][x] == FEAT_MAGMA_K) ||
                (cave_get(p->depth)->feat[y][x] == FEAT_QUARTZ_K))
            {
                /* Hack -- Memorize */
                p->cave->info[y][x] |= CAVE_MARK;

                /* Redraw */
                cave_light_spot_aux(p, cave_get(p->depth), y, x);

                /* Detect */
                gold_buried = TRUE;
            }
        }
    }

    return (gold_buried);
}


/*
 * Increment magical detection counter for a monster/player
 */
static void give_detect(struct player *p, int m_idx)
{
    /* Players */
    if (m_idx < 0)
    {
        if (p->play_det[0 - m_idx] < 255) p->play_det[0 - m_idx]++;
    }

    /* Monsters */
    else
    {
        if (p->mon_det[m_idx] < 255) p->mon_det[m_idx]++;
    }
}


/*
 * Detect "normal" monsters around the player.
 */
bool detect_monsters_normal(struct player *p, bool pause, bool aware)
{
    int i, y, x;
    int x1, x2, y1, y2;
    bool flag = FALSE;

    /* Pick an area to map */
    y1 = p->py - DETECT_DIST_Y;
    y2 = p->py + DETECT_DIST_Y;
    x1 = p->px - DETECT_DIST_X;
    x2 = p->px + DETECT_DIST_X;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip visible monsters */
        if (p->mon_vis[i]) continue;

        /* Location */
        y = m_ptr->fy;
        x = m_ptr->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all non-invisible, obvious monsters */
        if (!rf_has(r_ptr->flags, RF_INVISIBLE) && !m_ptr->unaware)
        {
            /* Increment detection counter */
            give_detect(p, i);

            /* Detect */
            flag = TRUE;
        }
    }

    /* Scan players */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *q_ptr = player_get(i);

        /* Skip visible players */
        if (p->play_vis[i]) continue;

        /* Skip the dungeon master if hidden */
        if (q_ptr->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this depth */
        if (p->depth != q_ptr->depth) continue;

        /* Skip ourself */
        if (q_ptr == p) continue;

        /* Location */
        y = q_ptr->py;
        x = q_ptr->px;

        /* Only detect nearby players */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all non-invisible, obvious players */
        if (!q_ptr->timed[TMD_INVIS] && !q_ptr->k_idx)
        {
            /* Increment detection counter */
            give_detect(p, 0 - i);

            /* Detect */
            flag = TRUE;
        }
    }

    /* Describe and clean up */
    if (flag && pause)
    {
        /* Mega-Hack -- Fix the monsters and players */
        update_monsters(p->depth, FALSE);
        update_players();

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = TRUE;

        /* Handle Window stuff */
        handle_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = FALSE;

        /* Describe, and wait for acknowledgement */
        msg(p, "You sense the presence of creatures!");
        party_msg_near(p, " senses the presence of creatures!");
        message_flush(p);

        /* Hack -- Pause */
        if (OPT_P(p, pause_after_detect)) Send_pause(p);
    }
    else if (aware && !flag)
        msg(p, "You sense no monsters.");

    /* Result */
    return (flag);
}


/*
 * Detect "invisible" monsters around the player.
 */
bool detect_monsters_invis(struct player *p, bool pause, bool aware)
{
    int i, y, x;
    int x1, x2, y1, y2;
    bool flag = FALSE;

    /* Pick an area to map */
    y1 = p->py - DETECT_DIST_Y;
    y2 = p->py + DETECT_DIST_Y;
    x1 = p->px - DETECT_DIST_X;
    x2 = p->px + DETECT_DIST_X;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        monster_lore *l_ptr = &p->lore[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip visible monsters */
        if (p->mon_vis[i]) continue;

        /* Location */
        y = m_ptr->fy;
        x = m_ptr->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all invisible monsters */
        if (rf_has(r_ptr->flags, RF_INVISIBLE))
        {
            /* Take note that they are invisible */
            rf_on(l_ptr->flags, RF_INVISIBLE);

            /* Update monster recall window */
            if (p->monster_race_idx == m_ptr->r_idx)
                p->redraw |= (PR_MONSTER);

            /* Increment detection counter */
            give_detect(p, i);

            /* Detect */
            flag = TRUE;
        }
    }

    /* Scan players */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *q_ptr = player_get(i);

        /* Skip visible players */
        if (p->play_vis[i]) continue;

        /* Skip the dungeon master if hidden */
        if (q_ptr->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this depth */
        if (p->depth != q_ptr->depth) continue;

        /* Skip ourself */
        if (q_ptr == p) continue;

        /* Location */
        y = q_ptr->py;
        x = q_ptr->px;

        /* Only detect nearby players */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect all invisible players */
        if (q_ptr->timed[TMD_INVIS])
        {
            /* Increment detection counter */
            give_detect(p, 0 - i);

            /* Detect */
            flag = TRUE;
        }
    }

    /* Describe result, and clean up */
    if (flag && pause)
    {
        /* Mega-Hack -- Fix the monsters and players */
        update_monsters(p->depth, FALSE);
        update_players();

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = TRUE;

        /* Handle Window stuff */
        handle_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = FALSE;

        /* Describe, and wait for acknowledgement */
        msg(p, "You sense the presence of invisible creatures!");
        party_msg_near(p, " senses the presence of invisible creatures!");
        message_flush(p);

        /* Hack -- Pause */
        if (OPT_P(p, pause_after_detect)) Send_pause(p);
    }
    else if (aware && !flag)
        msg(p, "You sense no invisible creatures.");

    /* Result */
    return (flag);
}


/*
 * Detect "evil" monsters around the player.
 */
bool detect_monsters_evil(struct player *p, bool aware)
{
    int i, y, x;
    int x1, x2, y1, y2;
    bool flag = FALSE;

    /* Pick an area to map */
    y1 = p->py - DETECT_DIST_Y;
    y2 = p->py + DETECT_DIST_Y;
    x1 = p->px - DETECT_DIST_X;
    x2 = p->px + DETECT_DIST_X;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        monster_lore *l_ptr = &p->lore[m_ptr->r_idx];

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip visible monsters */
        if (p->mon_vis[i]) continue;

        /* Location */
        y = m_ptr->fy;
        x = m_ptr->fx;

        /* Only detect nearby monsters */
        if ((x < x1) || (y < y1) || (x > x2) || (y > y2)) continue;

        /* Detect evil monsters */
        if (rf_has(r_ptr->flags, RF_EVIL))
        {
            /* Take note that they are evil */
            rf_on(l_ptr->flags, RF_EVIL);

            /* Update monster recall window */
            if (p->monster_race_idx == m_ptr->r_idx)
                p->redraw |= (PR_MONSTER);

            /* Increment detection counter */
            give_detect(p, i);

            /* Detect */
            flag = TRUE;
        }
    }

    /* Note effects and clean up */
    if (flag)
    {
        /* Mega-Hack -- Fix the monsters */
        update_monsters(p->depth, FALSE);

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = TRUE;

        /* Handle Window stuff */
        handle_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = FALSE;

        /* Describe, and wait for acknowledgement */
        msg(p, "You sense the presence of evil creatures!");
        party_msg_near(p, " senses the presence of evil creatures!");
        message_flush(p);

        /* Hack -- Pause */
        if (OPT_P(p, pause_after_detect)) Send_pause(p);
    }
    else if (aware)
        msg(p, "You sense no evil creatures.");

    /* Result */
    return (flag);
}


/*
 * Detect all monsters on the level (used for *enlightenment* only)
 */
bool detect_monsters_entire_level(struct player *p)
{
    int i;
    bool detect = FALSE;

    /* Scan monsters */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);

        /* Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Increment detection counter */
        give_detect(p, i);

        /* Detect */
        detect = TRUE;
    }

    /* Scan players */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *q_ptr = player_get(i);

        /* Skip the dungeon master if hidden */
        if (q_ptr->dm_flags & DM_SECRET_PRESENCE) continue;

        /* Skip players not on this depth */
        if (p->depth != q_ptr->depth) continue;

        /* Skip ourself */
        if (q_ptr == p) continue;

        /* Increment detection counter */
        give_detect(p, 0 - i);

        /* Detect */
        detect = TRUE;
    }

    /* Describe result, and clean up */
    if (detect)
    {
        /* Mega-Hack -- Fix the monsters and players */
        update_monsters(p->depth, FALSE);
        update_players();

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = TRUE;

        /* Handle Window stuff */
        handle_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = FALSE;

        /* Describe, and wait for acknowledgement */
        msg(p, "An image of all nearby life-forms appears in your mind!");
        party_msg_near(p, " senses the presence of all nearby life-forms!");
        message_flush(p);

        /* Hack -- Pause */
        if (OPT_P(p, pause_after_detect)) Send_pause(p);
    }
    else
        msg(p, "The level is devoid of life.");

    /* Result */
    return (detect);
}


/*
 * Detect everything
 */
bool detect_all(struct player *p, bool aware)
{
    bool detect = FALSE;

    /* Detect everything */
    if (detect_traps(p)) detect = TRUE;
    if (detect_doorstairs(p, aware)) detect = TRUE;
    if (detect_treasure(p, aware, FALSE)) detect = TRUE;
    if (reveal_monsters(p, aware)) detect = TRUE;

    /* Result */
    return (detect);
}


/*
 * Create stairs at the player location
 */
void stair_creation(struct player *p)
{
    int py = p->py;
    int px = p->px;
    int depth = p->depth;
    static byte count = 0xFF;
    static byte feat = FEAT_MORE;
    byte desired_feat;

    /* Only on random levels */
    if (!random_level(depth))
    {
        msg(p, "You cannot create stairs here...");
        return;
    }

    /* Require clean space */
    if (!cave_isfloor(cave_get(depth), py, px))
    {
        msg(p, "There is no clear floor on which to cast the spell.");
        return;
    }

    /* Push objects off the grid */
    if (cave_get(depth)->o_idx[py][px]) push_object(p, py, px);

    /* Choose staircase direction */
    if (depth == MAX_DEPTH - 1)
    {
        /* Bottom: always up */
        desired_feat = FEAT_LESS;

        /* Forbidden */
        if (cfg_limit_stairs == 2)
        {
            msg(p, "Nothing happens.");
            return;
        }
    }
    else if (cfg_limit_stairs == 2)
    {
        /* Always down */
        desired_feat = FEAT_MORE;
    }
    else if (count == 0)
    {
        /* Un-bias the RNG: no more creation of 10 up staircases in a row... */
        count = rand_range(3, 5);
        feat = ((feat == FEAT_MORE)? FEAT_LESS: FEAT_MORE);
        desired_feat = feat;
    }
    else
    {
        /* Random choice */
        desired_feat = (magik(50)? FEAT_MORE: FEAT_LESS);

        /* Check current feature */
        if ((count == 0xFF) || (desired_feat != feat)) count = rand_range(3, 5);
        if (desired_feat == feat) count--;
    }

    /* Create a staircase */
    cave_set_feat(cave_get(depth), py, px, desired_feat);
}


/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(object_type *o_ptr)
{
    return (wieldable_p(o_ptr) || (o_ptr->tval == TV_MSTAFF));
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(object_type *o_ptr)
{
    return armor_p(o_ptr);
}


/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[16] =
{
    0, 10, 20, 40, 80, 160, 280, 400,
    550, 700, 800, 900, 950, 970, 990, 1000
};


/*
 * Tries to increase an items bonus score, if possible
 *
 * Returns true if the bonus was increased
 */
static bool enchant_score(s16b *score, bool is_artifact)
{
    int chance;

    /* Artifacts resist enchantment half the time */
    if (is_artifact && magik(50)) return FALSE;

    /* Figure out the chance to enchant */
    if (*score < 0) chance = 0;
    else if (*score > 15) chance = 1000;
    else chance = enchant_table[*score];

    /* If we roll less-than-or-equal to chance, it fails */
    if (CHANCE(chance, 1000)) return FALSE;

    /* Increment the score */
    ++*score;

    return TRUE;
}


/*
 * Tries to uncurse a cursed item, if possible
 *
 * Returns true if a curse was broken
 */
static bool enchant_curse(struct player *p, object_type *o_ptr, bool is_artifact)
{
    bitflag f[OF_SIZE];

    /* Extract the flags */
    object_flags(o_ptr, f);

    /* If the item isn't cursed (or is perma-cursed) this doesn't work */
    if (!cursed_p(o_ptr->flags) || of_has(f, OF_PERMA_CURSE)) return FALSE;

    /* Artifacts resist enchanting curses away half the time */
    if (is_artifact && magik(50)) return FALSE;

    /* Normal items are uncursed 25% of the time */
    if (magik(75)) return FALSE;

    /* Uncurse the item */
    msg(p, "The curse is broken!");
    uncurse_object(o_ptr);
    return TRUE;
}


/*
 * Helper function for enchant() which tries to do the two things that
 * enchanting an item does, namely increasing its bonuses and breaking curses
 *
 * Returns true if a bonus was increased or a curse was broken
 */
static bool enchant_aux(struct player *p, object_type *o_ptr, s16b *score)
{
    bool result = FALSE;
    bool is_artifact = (o_ptr->artifact? TRUE: FALSE);

    if (enchant_score(score, is_artifact)) result = TRUE;
    if (enchant_curse(p, o_ptr, is_artifact)) result = TRUE;
    return result;
}


/*
 * Enchant an item
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting, and a
 * flag of what to try enchanting.  Artifacts resist enchantment some of the
 * time. Also, any enchantment attempt (even unsuccessful) kicks off a parallel
 * attempt to uncurse a cursed item.
 *
 * Note that an item can technically be enchanted all the way to +15 if you
 * wait a very, very, long time.  Going from +9 to +10 only works about 5% of
 * the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and the larger
 * the pile, the lower the chance of success.
 *
 * Returns true if the item was changed in some way
 */
bool enchant(struct player *p, object_type *o_ptr, int n, int eflag)
{
    int i, prob;
    bool res = FALSE;

    /* Magic ammo are always +0 +0 */
    if (magic_ammo_p(o_ptr)) return FALSE;

    /* Artifact ammo cannot be enchanted */
    if (obj_is_ammo(p, o_ptr) && o_ptr->artifact) return FALSE;

    /* Mage weapons cannot be enchanted */
    if (o_ptr->tval == TV_MSTAFF) return FALSE;

    /* Large piles resist enchantment */
    prob = o_ptr->number * 100;

    /* Missiles are easy to enchant */
    if (obj_is_ammo(p, o_ptr)) prob = prob / 20;

    /* Try "n" times */
    for (i = 0; i < n; i++)
    {
        /* Roll for pile resistance */
        if (!CHANCE(100, prob)) continue;

        /* Try the three kinds of enchantment we can do */
        if ((eflag & ENCH_TOHIT) && enchant_aux(p, o_ptr, &o_ptr->to_h))
            res = TRUE;
        if ((eflag & ENCH_TODAM) && enchant_aux(p, o_ptr, &o_ptr->to_d))
            res = TRUE;
        if ((eflag & ENCH_TOAC) && enchant_aux(p, o_ptr, &o_ptr->to_a))
            res = TRUE;
    }

    /* Failure */
    if (!res) return (FALSE);

    /* Recalculate bonuses */
    p->update |= (PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw */
    p->redraw |= (PR_INVEN | PR_EQUIP | PR_PLUSSES);

    /* Success */
    return (TRUE);
}


/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(struct player *p, int num_hit, int num_dam, int num_ac)
{
    bool okay = FALSE;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    bool discounted = TRUE;
    int n_hit = num_hit, n_dam = num_dam, n_ac = num_ac;
    bool (*item_tester)(object_type *o_ptr);
    int item;

    /* No item -> get one */
    if (p->current_value == ITEM_REQUEST)
    {
        get_item(p, 0, (num_ac? HOOK_ARMOR: HOOK_WEAPON));
        return FALSE;
    }

    /* Use current */
    item = p->current_value;

    /* Assume enchant weapon */
    item_tester = item_tester_hook_weapon;

    /* Enchant armor if requested */
    if (num_ac) item_tester = item_tester_hook_armour;

    /* Get the item */
    o_ptr = object_from_item_idx(p, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return FALSE;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p, o_ptr))
    {
        msg(p, "This item belongs to someone else!");
        return FALSE;
    }

    /* Paranoia: requires proper item */
    if (!item_tester(o_ptr)) return FALSE;

    /* Description */
    object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

    /* Describe */
    msg(p, "%s %s glow%s brightly!", ((item >= 0) ? "Your" : "The"), o_name, SINGULAR(o_ptr->number));

    /* Check for enchant spell */
    if (num_hit < 0)
    {
        n_hit = 0 - num_hit;
        discounted = FALSE;
    }
    if (num_dam < 0)
    {
        n_dam = 0 - num_dam;
        discounted = FALSE;
    }
    if (num_ac < 0)
    {
        n_ac = 0 - num_ac;
        discounted = FALSE;
    }

    /* Enchant */
    if (enchant(p, o_ptr, n_hit, ENCH_TOHIT)) okay = TRUE;
    if (enchant(p, o_ptr, n_dam, ENCH_TODAM)) okay = TRUE;
    if (enchant(p, o_ptr, n_ac, ENCH_TOAC)) okay = TRUE;

    /* Failure */
    if (!okay)
    {
        /* Message */
        msg(p, "The enchantment failed.");
    }
    else if (discounted)
    {
        /* Endless source of cash? No way... make them worthless */
        o_ptr->ident |= IDENT_WORTHLESS;
        if (object_was_sensed(o_ptr) || object_is_known(p, o_ptr))
            p->notice |= PN_SQUELCH;
    }

    /* Redraw */
    if (item < 0) p->redraw |= (PR_FLOOR | PR_ITEMLIST);

    /* Something happened */
    return (TRUE);
}


/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(struct player *p)
{
    object_type *o_ptr;
    int item;

    /* No item -> get one */
    if (p->current_value == ITEM_REQUEST)
    {
        get_item(p, 0, HOOK_UNDEFINED);
        return FALSE;
    }

    /* Use current */
    item = p->current_value;

    /* Get the item */
    o_ptr = object_from_item_idx(p, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return FALSE;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p, o_ptr))
    {
        msg(p, "This item belongs to someone else!");
        return FALSE;
    }

    /* Identify the object */
    do_ident_item(p, item, o_ptr);

    /* Something happened */
    return (TRUE);
}


/*
 * Hook to specify rechargeable items
 */
static bool item_tester_hook_recharge(object_type *o_ptr)
{
    /* Recharge staves */
    if (o_ptr->tval == TV_STAFF) return (TRUE);

    /* Recharge wands */
    if (o_ptr->tval == TV_WAND) return (TRUE);

    /* Nope */
    return (FALSE);
}


/*
 * Recharge a wand or staff from the pack or on the floor.
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staves/wands in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 */
bool recharge(struct player *p, int spell_strength)
{
    int i, t, lev;
    object_type *o_ptr;
    int item;

    /* No item -> get one */
    if (p->current_value == ITEM_REQUEST)
    {
        get_item(p, 0, HOOK_RECHARGE);
        return FALSE;
    }

    /* Use current */
    item = p->current_value;

    /* Get the item */
    o_ptr = object_from_item_idx(p, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return FALSE;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p, o_ptr))
    {
        msg(p, "This item belongs to someone else!");
        return FALSE;
    }

    /* Paranoia: requires rechargeable item */
    if (!item_tester_hook_recharge(o_ptr)) return FALSE;

    /* Extract the object "level" */
    lev = o_ptr->kind->level;

    /*
     * Chance of failure = 1 time in
     * [Spell_strength + 100 - item_level - 10 * charge_per_item] / 15
     */
    i = (spell_strength + 100 - lev - (10 * (o_ptr->pval[DEFAULT_PVAL] / o_ptr->number))) / 15;

    /* Back-fire */
    if ((i <= 1) || one_in_(i))
    {
        msg(p, "The recharge backfires!");
        msg(p, "There is a bright flash of light.");

        /* Safe recharge: drain all charges */
        if (cfg_safe_recharge)
        {
            /* Drain all charges */
            o_ptr->pval[DEFAULT_PVAL] = 0;

            /* We know that the item is empty */
            o_ptr->ident |= IDENT_EMPTY;
        }

        /* Normal recharge: destroy one item */
        else
        {
            /* Reduce the charges of rods/wands/staves */
            reduce_charges(o_ptr, 1);

            /* Reduce and describe */
            item_decrease(p, item, 1, TRUE);
        }
    }

    /* Recharge */
    else
    {
        /* Extract a "power" */
        t = (spell_strength / (lev + 2)) + 1;

        /* Recharge based on the power */
        if (t > 0) o_ptr->pval[DEFAULT_PVAL] += 2 + randint1(t);

        /* We no longer think the item is empty */
        o_ptr->ident &= ~IDENT_EMPTY;
    }

    /* Combine / Reorder the pack (later) */
    p->notice |= (PN_COMBINE | PN_REORDER);

    /* Redraw */
    p->redraw |= (PR_INVEN);
    if (item < 0) p->redraw |= (PR_FLOOR | PR_ITEMLIST);

    /* Something was done */
    return (TRUE);
}


/*
 * Apply a "project()" directly to all viewable monsters
 */
bool project_los(struct player *p, int typ, int dam, bool obvious)
{
    int i, x, y;
    int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
    int Ind = get_player_index(get_connection(p->conn));

    if (obvious) flg |= PROJECT_AWARE;

    /* Affect all (nearby) monsters */
    p->current_sound = -2;
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Location */
        y = m_ptr->fy;
        x = m_ptr->fx;

        /* Require line of sight */
        if (!player_has_los_bold(p, y, x)) continue;

        /* Jump directly to the target monster */
        if (project(0 - Ind, 0, p->depth, y, x, dam, typ, flg, "killed"))
            obvious = TRUE;
    }
    p->current_sound = -1;

    /* Result */
    return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(struct player *p)
{
    return project_los(p, GF_OLD_SPEED, 50, FALSE);
}


/*
 * Slow monsters
 */
bool slow_monsters(struct player *p, bool aware)
{
    return project_los(p, GF_OLD_SLOW, 20, aware);
}


/*
 * Sleep monsters
 */
bool sleep_monsters(struct player *p, bool aware)
{
    return project_los(p, GF_OLD_SLEEP, p->lev, aware);
}


/*
 * Confuse monsters
 */
bool confuse_monsters(struct player *p, bool aware)
{
    return project_los(p, GF_OLD_CONF, p->lev, aware);
}


/*
 * Banish evil monsters
 */
bool banish_evil(struct player *p, int dist)
{
    return project_los(p, GF_AWAY_EVIL, dist, TRUE);
}


/*
 * Turn undead
 */
bool turn_undead(struct player *p)
{
    return project_los(p, GF_TURN_UNDEAD, p->lev, TRUE);
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(struct player *p, int dam, bool aware)
{
    return project_los(p, GF_DISP_UNDEAD, dam, aware);
}


/*
 * Dispel evil monsters
 */
bool dispel_evil(struct player *p, int dam, bool aware)
{
    return project_los(p, GF_DISP_EVIL, dam, aware);
}


/*
 * Dispel all monsters
 */
bool dispel_monsters(struct player *p, int dam)
{
    return project_los(p, GF_DISP_ALL, dam, FALSE);
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(struct player *p, int who)
{
    int i;
    bool sleep = FALSE;

    /* Aggravate everyone nearby */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        int d;

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip aggravating monster (or player) */
        if (i == who) continue;

        /* Wake up nearby sleeping monsters */
        d = distance(p->py, p->px, m_ptr->fy, m_ptr->fx);
        if (d < MAX_SIGHT * 2)
        {
            /* Wake up */
            if (m_ptr->m_timed[MON_TMD_SLEEP])
            {
                /* Wake up */
                mon_clear_timed(p, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);
                sleep = TRUE;
            }
        }

        /* Speed up monsters in line of sight */
        if (player_has_los_bold(p, m_ptr->fy, m_ptr->fx))
            mon_inc_timed(p, m_ptr, MON_TMD_FAST, 25, MON_TMD_FLG_NOTIFY, FALSE);
    }

    /* Messages */
    if (sleep) msg(p, "You hear a sudden stirring in the distance!");
}


/*
 * Delete all non-unique monsters of a given "type" from the level
 *
 * This is different from normal Angband now -- the closest non-unique
 * monster is chosen as the designed character to banish.
 */
bool banishment(struct player *p)
{
    int i;
    char typ;
    bool result = FALSE;
    int d = 999, tmp;
    const char *pself = player_self(p);
    unsigned dam = 0;

    /* Search all monsters and find the closest */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Hack -- Skip Unique Monsters */
        if (rf_has(r_ptr->flags, RF_UNIQUE)) continue;

        /* Check distance */
        if ((tmp = distance(p->py, p->px, m_ptr->fy, m_ptr->fx)) < d)
        {
            /* Set closest distance */
            d = tmp;

            /* Set char */
            typ = r_ptr->d_char;
        }
    }

    /* Check to make sure we found a monster */
    if (d == 999) return FALSE;

    /* Delete the monsters of that "type" */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Hack -- Skip Unique Monsters */
        if (rf_has(r_ptr->flags, RF_UNIQUE)) continue;

        /* Skip "wrong" monsters */
        if (r_ptr->d_char != typ) continue;

        /* Delete the monster */
        delete_monster_idx(cave_get(p->depth), i);

        /* Take some damage */
        dam += randint1(4);
    }

    /* Hurt the player */
    strnfmt(p->died_flavor, sizeof(p->died_flavor), "exhausted %s with Banishment", pself);
    take_hit(p, dam, "the strain of casting Banishment", FALSE);

    /* Calculate result */
    result = (dam > 0)? TRUE: FALSE;

    /* Redraw */
    if (result) p->redraw |= (PR_MONLIST);

    return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_banishment(struct player *p)
{
    int i;
    bool result = FALSE;
    const char *pself = player_self(p);
    unsigned dam = 0;

    /* Delete the (nearby) monsters */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        int d;

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Hack -- Skip unique monsters */
        if (rf_has(r_ptr->flags, RF_UNIQUE)) continue;

        /* Skip distant monsters */
        d = distance(p->py, p->px, m_ptr->fy, m_ptr->fx);
        if (d > MAX_SIGHT) continue;

        /* Delete the monster */
        delete_monster_idx(cave_get(p->depth), i);

        /* Take some damage */
        dam += randint1(3);
    }

    /* Hurt the player */
    strnfmt(p->died_flavor, sizeof(p->died_flavor), "exhausted %s with Mass Banishment", pself);
    take_hit(p, dam, "the strain of casting Mass Banishment", FALSE);

    /* Calculate result */
    result = (dam > 0)? TRUE: FALSE;

    /* Redraw */
    if (result) p->redraw |= (PR_MONLIST);

    return (result);
}


/*
 * Probe nearby monsters
 */
bool probing(struct player *p)
{
    int i;
    bool probe = FALSE;
    bool blows;

    /* Probe all (nearby) monsters */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);

        blows = FALSE;

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Skip monsters too far */
        if (distance(p->py, p->px, m_ptr->fy, m_ptr->fx) > PROBE_RADIUS) continue;

        /* Probe visible monsters */
        if (p->mon_vis[i])
        {
            char m_name[NORMAL_WID];
            char buf[NORMAL_WID];
            int j;

            /* Start the message */
            if (!probe) msg(p, "Probing...");

            /* Get "the monster" or "something" */
            monster_desc(p, m_name, sizeof(m_name), m_ptr, MDESC_IND1 | MDESC_CAPITAL);

            strnfmt(buf, sizeof(buf), "blows");
            for (j = 0; j < 4; j++)
            {
                if (m_ptr->blow[j].d_dice)
                {
                    if (!blows) blows = TRUE;
                    my_strcat(buf, format(" %dd%d", m_ptr->blow[j].d_dice, m_ptr->blow[j].d_side),
                        sizeof(buf));
                }
            }

            /* Describe the monster */
            msg(p, "%s (%d) has %d hp, %d ac, %d speed.", m_name, m_ptr->level, m_ptr->hp,
                m_ptr->ac, m_ptr->mspeed);
            if (blows)
                msg(p, "%s (%d) %s.", m_name, m_ptr->level, buf);

            /* Learn all of the non-spell, non-treasure flags */
            lore_do_probe(p, i);

            /* Probe worked */
            probe = TRUE;
        }
    }

    /* Done */
    if (probe)
        msg(p, "That's all.");

    /* Result */
    return (probe);
}


/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
bool destroy_area(struct player *p, int depth, int y1, int x1, int r, bool full)
{
    int y, x, k, t, j;
    int hurt[MAX_PLAYERS];
    int count = 0;

    /* Only on random levels */
    if (!random_level(depth))
    {
        if (p) msg(p, "The ground shakes for a moment.");
        return FALSE;
    }

    if (p) msg_misc(p, " unleashes great power!");

    /* Big area of affect */
    for (y = (y1 - r); y <= (y1 + r); y++)
    {
        for (x = (x1 - r); x <= (x1 + r); x++)
        {
            /* Skip illegal grids */
            if (!in_bounds_fully(y, x)) continue;

            /* Extract the distance */
            k = distance(y1, x1, y, x);

            /* Stay in the circle of death */
            if (k > r) continue;

            /* Lose room and vault */
            cave_get(depth)->info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_NOTELE);

            /* Lose light */
            cave_get(depth)->info[y][x] &= ~(CAVE_GLOW);

            cave_light_spot(cave_get(depth), y, x);

            /* Hack -- Notice player affect */
            if (cave_get(depth)->m_idx[y][x] < 0)
            {
                /* Hurt the player later */
                hurt[count] = 0 - cave_get(depth)->m_idx[y][x];
                count++;

                /* Do not hurt this grid */
                continue;
            }

            /* Hack -- Skip the epicenter */
            if ((y == y1) && (x == x1)) continue;

            /* Delete the monster (if any) */
            delete_monster(depth, y, x);

            /* Don't remove stairs */
            if (cave_isstairs(cave_get(depth), y, x)) continue;

            /* Lose knowledge (keeping knowledge of stairs) */
            forget_spot(depth, y, x);

            /* Destroy any grid that isn't a permanent wall */
            if (!cave_isperm(cave_get(depth), y, x))
            {
                int feat = FEAT_FLOOR;

                /* Delete the object (if any) */
                delete_object(depth, y, x);

                /* Wall (or floor) type */
                t = randint0(200);

                /* Granite */
                if (t < 20)
                {
                    /* Create granite wall */
                    feat = FEAT_WALL_EXTRA;
                }

                /* Quartz */
                else if (t < 70)
                {
                    /* Create quartz vein */
                    feat = FEAT_QUARTZ;
                }

                /* Magma */
                else if (t < 100)
                {
                    /* Create magma vein */
                    feat = FEAT_MAGMA;
                }

                /* Change the feature */
                cave_set_feat(cave_get(depth), y, x, feat);
            }
        }
    }

    /* Hack -- Affect players */
    for (j = 0; j < count; j++)
    {
        player_type *p_ptr = player_get(hurt[j]);

        /* Message */
        msg(p_ptr, "There is a searing blast of light!");

        /* Blind the player */
        wieldeds_notice_flag(p_ptr, OF_RES_LIGHT);
        if (!check_state(p_ptr, OF_RES_LIGHT))
            player_inc_timed(p_ptr, TMD_BLIND, 10 + randint1(10), TRUE, TRUE);

        /* Fully update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Fully update the flow */
        p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

        /* Redraw */
        p_ptr->redraw |= (PR_MONLIST | PR_ITEMLIST);
    }

    return TRUE;
}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when banished.
 *
 * Note that players and monsters (except eaters of walls and passers
 * through walls) will never occupy the same grid as a wall (or door).
 */
bool earthquake(struct player *p, int depth, int cy, int cx, int r)
{
    int i, t, y, x, yy, xx, dy, dx, j;
    int damage;
    int sn, sy, sx;
    int hurt[MAX_PLAYERS];
    bool map[32][32];
    int count = 0;

    /* Only on random levels */
    if (!random_level(depth))
    {
        if (p) msg(p, "The ground shakes for a moment.");
        return FALSE;
    }

    if (p) msg_misc(p, " causes the ground to shake!");

    /* Paranoia -- Enforce maximum range */
    if (r > 12) r = 12;

    /* Clear the "maximal blast" area */
    for (y = 0; y < 32; y++)
    {
        for (x = 0; x < 32; x++) map[y][x] = FALSE;
    }

    /* Check around the epicenter */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds_fully(yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cy, cx, yy, xx) > r) continue;

            /* Take note of any player */
            if (cave_get(depth)->m_idx[yy][xx] < 0)
            {
                hurt[count] = cave_get(depth)->m_idx[yy][xx];
                count++;
            }

            /* Lose room and vault */
            cave_get(depth)->info[yy][xx] &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_NOTELE);

            /* Lose light and knowledge */
            cave_get(depth)->info[yy][xx] &= ~(CAVE_GLOW);
            forget_spot(depth, yy, xx);

            /* Skip the epicenter */
            if (!dx && !dy) continue;

            /* Skip most grids */
            if (magik(85)) continue;

            /* Damage this grid */
            map[16 + dy][16 + dx] = TRUE;

            /* Hack -- Take note of player damage */
            if (cave_get(depth)->m_idx[yy][xx] < 0) hurt[count - 1] = 0 - hurt[count - 1];
        }
    }

    /* First, affect the players (if necessary) */
    for (j = 0; j < count; j++)
    {
        player_type *p_ptr;

        /* Skip undamaged players */
        if (hurt[j] < 0) continue;

        p_ptr = player_get(hurt[j]);

        sn = 0; sy = 0; sx = 0; damage = 0;

        /* Check around the player */
        for (i = 0; i < 8; i++)
        {
            /* Get the location */
            y = p_ptr->py + ddy_ddd[i];
            x = p_ptr->px + ddx_ddd[i];

            /* Skip illegal grids */
            if (!in_bounds_fully(y, x)) continue;

            /* Skip non-empty grids */
            if (!cave_empty_bold(depth, y, x)) continue;

            /* Important -- Skip "quake" grids */
            if (map[16 + y - cy][16 + x - cx]) continue;

            /* Count "safe" grids, apply the randomizer */
            if ((++sn > 1) && randint0(sn)) continue;

            /* Save the safe location */
            sy = y; sx = x;
        }

        /* Random message */
        switch (randint1(3))
        {
            case 1:
            {
                msg(p_ptr, "The cave ceiling collapses!");
                break;
            }
            case 2:
            {
                msg(p_ptr, "The cave floor twists in an unnatural way!");
                break;
            }
            default:
            {
                msg(p_ptr, "The cave quakes!");
                msg(p_ptr, "You are pummeled with debris!");
                break;
            }
        }

        /* Hurt the player a lot */
        if (!sn)
        {
            /* Message and damage */
            msg(p_ptr, "You are severely crushed!");
            damage = 300;
        }

        /* Destroy the grid, and push the player to safety */
        else
        {
            /* Calculate results */
            switch (randint1(3))
            {
                case 1:
                {
                    msg(p_ptr, "You nimbly dodge the blast!");
                    damage = 0;
                    break;
                }
                case 2:
                {
                    msg(p_ptr, "You are bashed by rubble!");
                    damage = damroll(10, 4);
                    player_inc_timed(p_ptr, TMD_STUN, randint1(50), TRUE, TRUE);
                    break;
                }
                case 3:
                {
                    msg(p_ptr, "You are crushed between the floor and ceiling!");
                    damage = damroll(10, 4);
                    player_inc_timed(p_ptr, TMD_STUN, randint1(50), TRUE, TRUE);
                    break;
                }
            }

            /* Move player */
            monster_swap(depth, p_ptr->py, p_ptr->px, sy, sx);
        }

        /* Take some damage */
        if (damage)
        {
            my_strcpy(p_ptr->died_flavor, "was crushed by tons of falling rocks",
                sizeof(p_ptr->died_flavor));
            take_hit(p_ptr, damage, "an earthquake", FALSE);
        }
    }

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds_fully(yy, xx)) continue;

            /* Skip unaffected grids */
            if (!map[16 + dy][16 + dx]) continue;

            /* Process monsters */
            if (cave_get(depth)->m_idx[yy][xx] > 0)
            {
                monster_type *m_ptr = cave_monster_at(cave_get(depth), yy, xx);
                monster_race *r_ptr = &r_info[m_ptr->r_idx];

                /* Most monsters cannot co-exist with rock */
                if (!rf_has(r_ptr->flags, RF_KILL_WALL) && !rf_has(r_ptr->flags, RF_PASS_WALL))
                {
                    char m_name[NORMAL_WID];

                    /* Assume not safe */
                    sn = 0;

                    /* Monster can move to escape the wall */
                    if (!rf_has(r_ptr->flags, RF_NEVER_MOVE))
                    {
                        /* Look for safety */
                        for (i = 0; i < 8; i++)
                        {
                            /* Get the grid */
                            y = yy + ddy_ddd[i];
                            x = xx + ddx_ddd[i];

                            /* Skip illegal grids */
                            if (!in_bounds_fully(y, x)) continue;

                            /* Skip non-empty grids */
                            if (!cave_empty_bold(depth, y, x)) continue;

                            /* No safety on glyph of warding */
                            if (cave_get(depth)->feat[y][x] == FEAT_GLYPH) continue;

                            /* Important -- Skip "quake" grids */
                            if (map[16 + y - cy][16 + x - cx]) continue;

                            /* Count "safe" grids, apply the randomizer */
                            if ((++sn > 1) && randint0(sn)) continue;

                            /* Save the safe grid */
                            sy = y;
                            sx = x;
                        }
                    }

                    /* Give players a message */
                    for (j = 0; j < count; j++)
                    {
                        /* Get player */
                        player_type *p_ptr = player_get(abs(hurt[j]));

                        /* Describe the monster */
                        monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

                        /* Scream in pain */
                        add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_WAIL, TRUE);
                    }

                    /* Take damage from the quake */
                    damage = (sn? damroll(4, 8): (m_ptr->hp + 1));

                    /* Monster is certainly awake */
                    mon_clear_timed(p, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);

                    /* If the quake finished the monster off, show message */
                    if ((m_ptr->hp < damage) && (m_ptr->hp >= 0))
                    {
                        /* Give players a message */
                        for (j = 0; j < count; j++)
                        {
                            /* Get player */
                            player_type *p_ptr = player_get(abs(hurt[j]));

                            /* Describe the monster */
                            monster_desc(p_ptr, m_name, sizeof(m_name), m_ptr, 0);

                            /* Message */
                            add_monster_message(p_ptr, m_name, m_ptr, MON_MSG_EMBEDDED, TRUE);
                        }
                    }

                    /* Apply damage directly */
                    m_ptr->hp -= damage;

                    /* Delete (not kill) "dead" monsters */
                    if (m_ptr->hp < 0)
                    {
                        /* Delete the monster */
                        delete_monster(depth, yy, xx);

                        /* No longer safe */
                        sn = 0;
                    }

                    /* Hack -- Escape from the rock */
                    if (sn)
                    {
                        /* Move the monster */
                        monster_swap(depth, yy, xx, sy, sx);
                    }
                }
            }
        }
    }

    /* Important -- No wall on players */
    for (j = 0; j < count; j++)
    {
        /* Get player */
        player_type *p_ptr = player_get(abs(hurt[j]));

        map[16 + p_ptr->py - cy][16 + p_ptr->px - cx] = FALSE;
    }

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds_fully(yy, xx)) continue;

            /* Note unaffected grids for light changes, etc. */
            if (!map[16 + dy][16 + dx]) cave_light_spot(cave_get(depth), yy, xx);

            /* Destroy location (if valid) */
            else if (cave_valid_bold(depth, yy, xx))
            {
                int feat = FEAT_FLOOR;
                bool floor = cave_floor_bold(depth, yy, xx);

                /* Delete any object that is still there */
                delete_object(depth, yy, xx);

                /* Wall (or floor) type */
                t = (floor? randint0(100): 200);

                /* Granite */
                if (t < 20)
                {
                    /* Create granite wall */
                    feat = FEAT_WALL_EXTRA;
                }

                /* Quartz */
                else if (t < 70)
                {
                    /* Create quartz vein */
                    feat = FEAT_QUARTZ;
                }

                /* Magma */
                else if (t < 100)
                {
                    /* Create magma vein */
                    feat = FEAT_MAGMA;
                }

                /* Change the feature */
                cave_set_feat(cave_get(depth), yy, xx, feat);
            }
        }
    }

    for (j = 0; j < count; j++)
    {
        /* Get player */
        player_type *p_ptr = player_get(abs(hurt[j]));

        /* Fully update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Fully update the flow */
        p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

        /* Redraw */
        p_ptr->redraw |= (PR_HEALTH | PR_MONLIST | PR_ITEMLIST);
    }

    return TRUE;
}


/*
 * This routine will Perma-Light all grids in the set passed in.
 *
 * This routine is used (only) by "light_room(..., LIGHT)"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_light(struct player *p, struct point_set *ps)
{
    int i;

    /* Apply flag changes */
    for (i = 0; i < ps->n; i++)
    {
        int y = ps->pts[i].y;
        int x = ps->pts[i].x;

        /* Paranoia */
        if (!in_bounds(y, x)) continue;

        /* No longer in the array */
        cave_get(p->depth)->info[y][x] &= ~CAVE_TEMP;

        /* Perma-Light */
        cave_get(p->depth)->info[y][x] |= CAVE_GLOW;
    }

    /* Check everyone */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *q_ptr = player_get(i);

        /* If he's not here, skip him */
        if (q_ptr->depth != p->depth) continue;

        /* Fully update the visuals */
        q_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Update stuff */
        update_stuff(q_ptr);
    }

    /* Process the grids */
    for (i = 0; i < ps->n; i++)
    {
        int y = ps->pts[i].y;
        int x = ps->pts[i].x;

        /* Paranoia */
        if (!in_bounds(y, x)) continue;

        /* Redraw the grid */
        cave_light_spot(cave_get(p->depth), y, x);

        /* Process affected monsters */
        if (cave_get(p->depth)->m_idx[y][x] > 0)
        {
            int chance = 25;
            monster_type *m_ptr = cave_monster_at(cave_get(p->depth), y, x);
            monster_race *r_ptr = &r_info[m_ptr->r_idx];

            /* Stupid monsters rarely wake up */
            if (rf_has(r_ptr->flags, RF_STUPID)) chance = 10;

            /* Smart monsters always wake up */
            if (rf_has(r_ptr->flags, RF_SMART)) chance = 100;

            /* Sometimes monsters wake up */
            if (m_ptr->m_timed[MON_TMD_SLEEP] && magik(chance))
            {
                /* Wake up! */
                mon_clear_timed(p, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOTIFY, FALSE);
            }
        }
    }
}


void cave_unlight_aux(int depth, struct point_set *ps)
{
    int i;

    /* Apply flag changes */
    for (i = 0; i < ps->n; i++)
    {
        int y = ps->pts[i].y;
        int x = ps->pts[i].x;

        /* No longer in the array */
        cave_get(depth)->info[y][x] &= ~CAVE_TEMP;

        /* Darken the grid */
        cave_get(depth)->info[y][x] &= ~CAVE_GLOW;

        /* Hack -- Forget "boring" grids */
        if (cave_floor_basic(cave_get(depth)->feat[y][x]))
        {
            /* Forget the grid */
            forget_spot(depth, y, x);
        }
    }

    /* Check everyone */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *q_ptr = player_get(i);

        /* If he's not here, skip him */
        if (q_ptr->depth != depth) continue;

        /* Fully update the visuals */
        q_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        /* Update stuff */
        update_stuff(q_ptr);
    }

    /* Process the grids */
    for (i = 0; i < ps->n; i++)
    {
        int y = ps->pts[i].y;
        int x = ps->pts[i].x;

        /* Redraw the grid */
        cave_light_spot(cave_get(depth), y, x);
    }
}


/*
 * This routine will "darken" all grids in the set passed in.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "light_room(..., UNLIGHT)"
 */
static void cave_unlight(struct player *p, struct point_set *ps)
{
    cave_unlight_aux(p->depth, ps);
}


/*
 * Aux function -- See below
 */
void cave_room_aux(struct point_set *seen, int depth, int y, int x)
{
    /* Avoid infinite recursion */
    if (cave_get(depth)->info[y][x] & CAVE_TEMP) return;

    /* Do not "leave" the current room */
    if (!(cave_get(depth)->info[y][x] & CAVE_ROOM)) return;

    /* Mark the grid as "seen" */
    cave_get(depth)->info[y][x] |= CAVE_TEMP;

    /* Add it to the "seen" set */
    add_to_point_set(seen, 0, y, x);
}


void light_room_aux(struct point_set *ps, int depth, int y1, int x1)
{
    int i, x, y;

    /* Add the initial grid */
    cave_room_aux(ps, depth, y1, x1);

    /* While grids are in the queue, add their neighbors */
    for (i = 0; i < ps->n; i++)
    {
        x = ps->pts[i].x, y = ps->pts[i].y;

        /* Walls get lit, but stop light */
        if (!cave_floor_bold(depth, y, x)) continue;

        /* Spread adjacent */
        cave_room_aux(ps, depth, y + 1, x);
        cave_room_aux(ps, depth, y - 1, x);
        cave_room_aux(ps, depth, y, x + 1);
        cave_room_aux(ps, depth, y, x - 1);

        /* Spread diagonal */
        cave_room_aux(ps, depth, y + 1, x + 1);
        cave_room_aux(ps, depth, y - 1, x - 1);
        cave_room_aux(ps, depth, y - 1, x + 1);
        cave_room_aux(ps, depth, y + 1, x - 1);
    }
}


#define LIGHT TRUE
#define UNLIGHT FALSE


/*
 * Illuminate any room containing the given location.
 */
static void light_room(struct player *p, int y1, int x1, bool light)
{
    struct point_set *ps;

    ps = point_set_new(200);

    light_room_aux(ps, p->depth, y1, x1);

    /* Now, lighten or darken them all at once */
    if (light) cave_light(p, ps);
    else cave_unlight(p, ps);

    point_set_dispose(ps);
}


/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool light_area(struct player *p, int dam, int rad)
{
    int pz = p->depth;
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_GRID;
    int Ind = get_player_index(get_connection(p->conn));

    /* Hurt monsters in the projection radius */
    if (dam > 0) flg |= PROJECT_KILL;

    /* Hack -- Message */
    if (!p->timed[TMD_BLIND])
        msg(p, "You are surrounded by a white light.");

    /* Hook into the "project()" function */
    p->current_sound = -2;
    project(0 - Ind, rad, pz, py, px, dam, GF_LIGHT_WEAK, flg, "killed");
    p->current_sound = -1;

    /* Light up the room */
    light_room(p, py, px, LIGHT);

    /* Assume seen */
    return (TRUE);
}


/*
 * Hack -- Call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlight_area(struct player *p, int dam, int rad)
{
    int pz = p->depth;
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_GRID | PROJECT_KILL;
    int Ind = get_player_index(get_connection(p->conn));

    /* No effect outside of the dungeon during day */
    if ((pz <= 0) && is_daytime()) return FALSE;

    /* No effect on special levels */
    if (check_special_level(pz)) return FALSE;

    /* Hack -- Message */
    if (!p->timed[TMD_BLIND])
        msg(p, "Darkness surrounds you.");

    /* Hook into the "project()" function */
    project(0 - Ind, rad, pz, py, px, dam, GF_DARK_WEAK, flg, "killed");

    /* Darken the room */
    light_room(p, py, px, UNLIGHT);

    /* Assume seen */
    return (TRUE);
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
static bool fire_ball_aux(struct player *p, int typ, int dir, int dam, int rad, bool obvious)
{
    s16b ty, tx;
    bool result;
    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    int Ind = get_player_index(get_connection(p->conn));

    if (obvious) flg |= PROJECT_AWARE;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return FALSE;

    /* Use the given direction */
    tx = p->px + 99 * ddx[dir];
    ty = p->py + 99 * ddy[dir];

    /* Hack -- Use an actual "target" */
    if ((dir == 5) && target_okay(p))
    {
        flg &= ~PROJECT_STOP;
        target_get(p, &tx, &ty);
    }

    /* Analyze the "dir" and the "target".  Hurt items on floor. */
    p->current_sound = -2;
    result = project(0 - Ind, rad, p->depth, ty, tx, dam, typ, flg, "annihilated");
    p->current_sound = -1;
    return result;
}


bool fire_ball(struct player *p, int typ, int dir, int dam, int rad)
{
    return fire_ball_aux(p, typ, dir, dam, rad, FALSE);
}


bool fire_ball_hack(struct player *p, int typ, int dir, int dam, int rad)
{
    return fire_ball_aux(p, typ, dir, dam, rad, TRUE);
}


/*
 * Cast multiple non-jumping ball spells at the same target.
 *
 * Targets absolute coordinates instead of a specific monster, so that
 * the death of the monster doesn't change the target's location.
 */
bool fire_swarm(struct player *p, int num, int typ, int dir, int dam, int rad)
{
    bool noticed = FALSE;
    int py = p->py;
    int px = p->px;
    s16b ty, tx;
    int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    int Ind = get_player_index(get_connection(p->conn));

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return FALSE;

    /* Use the given direction */
    ty = py + 99 * ddy[dir];
    tx = px + 99 * ddx[dir];

    /* Hack -- Use an actual "target" */
    if ((dir == 5) && target_okay(p))
        target_get(p, &tx, &ty);

    p->current_sound = -2;
    while (num--)
    {
        /* Analyze the "dir" and the "target".  Hurt items on floor. */
        if (project(0 - Ind, rad, p->depth, ty, tx, dam, typ, flg, "annihilated"))
            noticed = TRUE;
    }
    p->current_sound = -1;

    return noticed;
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
bool project_hook(struct player *p, int typ, int dir, int dam, int flg, const char *what)
{
    s16b ty, tx;
    int Ind = get_player_index(get_connection(p->conn));

    /* Pass through the target if needed */
    flg |= (PROJECT_THRU);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return FALSE;

    /* Use the given direction */
    tx = p->px + ddx[dir];
    ty = p->py + ddy[dir];

    /* Hack -- Use an actual "target" */
    if ((dir == 5) && target_okay(p)) target_get(p, &tx, &ty);

    /* Analyze the "dir" and the "target", do NOT explode */
    return (project(0 - Ind, 0, p->depth, ty, tx, dam, typ, flg, what));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
static bool fire_bolt_aux(struct player *p, int typ, int dir, int dam, bool obvious)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    if (obvious) flg |= PROJECT_AWARE;
    return (project_hook(p, typ, dir, dam, flg, "annihilated"));
}


bool fire_bolt(struct player *p, int typ, int dir, int dam)
{
    return fire_bolt_aux(p, typ, dir, dam, FALSE);
}


bool fire_bolt_hack(struct player *p, int typ, int dir, int dam)
{
    return fire_bolt_aux(p, typ, dir, dam, TRUE);
}


/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
static bool fire_beam_aux(struct player *p, int typ, int dir, int dam, bool obvious)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_KILL;

    if (obvious) flg |= PROJECT_AWARE;
    p->current_sound = -2;
    result = project_hook(p, typ, dir, dam, flg, "annihilated");
    p->current_sound = -1;
    return result;
}


bool fire_beam(struct player *p, int typ, int dir, int dam)
{
    return fire_beam_aux(p, typ, dir, dam, FALSE);
}


bool fire_beam_hack(struct player *p, int typ, int dir, int dam)
{
    return fire_beam_aux(p, typ, dir, dam, TRUE);
}


/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(struct player *p, int prob, int typ, int dir, int dam)
{
    if (magik(prob)) return fire_beam(p, typ, dir, dam);
    return fire_bolt(p, typ, dir, dam);
}


/*
 * Some of the old functions
 */
bool light_line(struct player *p, int dir)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;

    p->current_sound = -2;
    result = project_hook(p, GF_LIGHT_WEAK, dir, damroll(6, 8), flg, "killed");
    p->current_sound = -1;
    return result;
}


bool strong_light_line(struct player *p, int dir)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;

    p->current_sound = -2;
    result = project_hook(p, GF_LIGHT, dir, damroll(10, 8), flg, "killed");
    p->current_sound = -1;
    return result;
}


bool drain_life(struct player *p, int dir, int dam)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    return (project_hook(p, GF_OLD_DRAIN, dir, dam, flg, "killed"));
}


bool wall_to_mud(struct player *p, int dir)
{
    bool result;
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

    p->current_sound = -2;
    result = project_hook(p, GF_KILL_WALL, dir, 20 + randint1(30), flg, "killed");
    p->current_sound = -1;
    return result;
}


bool destroy_door(struct player *p, int dir)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;

    return (project_hook(p, GF_KILL_DOOR, dir, 0, flg, "killed"));
}


bool disarm_trap(struct player *p, int dir)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;

    return (project_hook(p, GF_KILL_TRAP, dir, 0, flg, "killed"));
}


bool heal_monster(struct player *p, int dir)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    return (project_hook(p, GF_OLD_HEAL, dir, damroll(4, 6), flg, "killed"));
}


bool speed_monster(struct player *p, int dir)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    return (project_hook(p, GF_OLD_SPEED, dir, 100, flg, "killed"));
}


bool slow_monster(struct player *p, int dir, bool aware)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    if (aware) flg |= PROJECT_AWARE;
    return (project_hook(p, GF_OLD_SLOW, dir, 20, flg, "killed"));
}


bool sleep_monster(struct player *p, int dir, bool aware)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    if (aware) flg |= PROJECT_AWARE;
    return (project_hook(p, GF_OLD_SLEEP, dir, p->lev, flg, "killed"));
}


bool confuse_monster(struct player *p, int dir, int plev, bool aware)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    if (aware) flg |= PROJECT_AWARE;
    return (project_hook(p, GF_OLD_CONF, dir, plev, flg, "killed"));
}


bool poly_monster(struct player *p, int dir, bool aware)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    if (aware) flg |= PROJECT_AWARE;

    /* Forbid in the town and on special levels */
    if (forbid_special(p->depth)) return (FALSE);

    return (project_hook(p, GF_OLD_POLY, dir, p->lev, flg, "killed"));
}


bool clone_monster(struct player *p, int dir)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    return (project_hook(p, GF_OLD_CLONE, dir, 0, flg, "killed"));
}


bool fear_monster(struct player *p, int dir, int plev, bool aware)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    if (aware) flg |= PROJECT_AWARE;
    return (project_hook(p, GF_TURN_ALL, dir, plev, flg, "killed"));
}


bool teleport_monster(struct player *p, int dir)
{
    bool result;
    int flg = PROJECT_STOP | PROJECT_KILL;

    /* Sound */
    sound(p, MSG_TPOTHER);

    p->current_sound = -2;
    result = project_hook(p, GF_AWAY_ALL, dir, MAX_SIGHT_LGE * 5, flg, "killed");
    p->current_sound = -1;
    return result;
}


/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */
bool door_creation(struct player *p)
{
    int pz = p->depth;
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    int Ind = get_player_index(get_connection(p->conn));

    /* Only on random levels */
    if (!random_level(pz))
    {
        msg(p, "You cannot create doors here...");
        return (FALSE);
    }

    return (project(0 - Ind, 1, pz, py, px, 0, GF_MAKE_DOOR, flg, "killed"));
}


bool trap_creation(struct player *p, bool silent)
{
    int pz = p->depth;
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    int Ind = get_player_index(get_connection(p->conn));

    /* Only on random levels */
    if (!random_level(pz))
    {
        msg(p, (silent? "You hear a brief whistling sound.": "You cannot create traps here..."));
        return FALSE;
    }

    project(0 - Ind, 1, pz, py, px, 0, GF_MAKE_TRAP, flg, "killed");
    return TRUE;
}


bool destroy_doors_touch(struct player *p)
{
    int pz = p->depth;
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    int Ind = get_player_index(get_connection(p->conn));

    return (project(0 - Ind, 1, pz, py, px, 0, GF_KILL_DOOR, flg, "killed"));
}


bool sleep_monsters_touch(struct player *p, bool aware)
{
    int pz = p->depth;
    int py = p->py;
    int px = p->px;
    int flg = PROJECT_KILL | PROJECT_HIDE;
    int Ind = get_player_index(get_connection(p->conn));

    if (aware) flg |= PROJECT_AWARE;
    return (project(0 - Ind, 1, pz, py, px, p->lev, GF_OLD_SLEEP, flg, "killed"));
}


/*
 * Curse the players armor
 */
bool curse_armor(struct player *p)
{
    object_type *o_ptr;
    char o_name[NORMAL_WID];

    /* Curse the body armor */
    o_ptr = &p->inventory[INVEN_BODY];

    /* Nothing to curse */
    if (!o_ptr->kind) return (FALSE);

    /* Describe */
    object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_FULL);

    /* Attempt a saving throw for artifacts */
    if (o_ptr->artifact && magik(50))
    {
        /* Cool */
        msg(p, "A terrible black aura tries to %s, but your %s resists the effects!",
            "surround your armor", o_name);
    }

    /* Not artifact or failed save... */
    else
    {
        /* Oops */
        msg(p, "A terrible black aura blasts your %s!", o_name);

        /* Damage the armor */
        o_ptr->to_a -= randint1(3);

        /* Curse it */
        flags_set(o_ptr->flags, OF_SIZE, OF_LIGHT_CURSE, OF_HEAVY_CURSE, FLAG_END);

        /* Recalculate bonuses */
        p->update |= (PU_BONUS);

        /* Recalculate mana */
        p->update |= (PU_MANA);

        /* Redraw */
        p->redraw |= (PR_EQUIP);
    }

    return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(struct player *p)
{
    object_type *o_ptr;
    char o_name[NORMAL_WID];

    /* Curse the weapon */
    o_ptr = &p->inventory[INVEN_WIELD];

    /* Nothing to curse */
    if (!o_ptr->kind) return (FALSE);  

    /* Describe */
    object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_FULL);

    /* Attempt a saving throw */
    if (o_ptr->artifact && magik(50))
    {
        /* Cool */
        msg(p, "A terrible black aura tries to %s, but your %s resists the effects!",
            "surround your weapon", o_name);
    }

    /* Not artifact or failed save... */
    else
    {
        /* Oops */
        msg(p, "A terrible black aura blasts your %s!", o_name);

        /* Damage the weapon */
        o_ptr->to_h -= randint1(3);
        o_ptr->to_d -= randint1(3);

        /* Curse it */
        flags_set(o_ptr->flags, OF_SIZE, OF_LIGHT_CURSE, OF_HEAVY_CURSE, FLAG_END);

        /* Recalculate bonuses */
        p->update |= (PU_BONUS);

        /* Recalculate mana */
        p->update |= (PU_MANA);

        /* Redraw */
        p->redraw |= (PR_EQUIP);
    }

    /* Notice */
    return (TRUE);
}


/*  
 * Brand weapons (or ammo)
 *
 * Turns the (non-magical) object into an ego-item of 'brand_type'.
 */
static void brand_object(struct player *p, object_type *o_ptr, int brand_type)
{
    int i, j;
    ego_item_type *e_ptr;
    bool ok = FALSE;

    /* You can never modify artifacts/ego-items */
    /* You can never modify worthless/cursed items */
    if (o_ptr->kind && !cursed_p(o_ptr->flags) && o_ptr->kind->cost && !o_ptr->artifact && !o_ptr->ego)
    {
        char o_name[NORMAL_WID];
        bitflag f[OF_SIZE];
        const char *brand[SL_MAX];

        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

        of_wipe(f);
        of_on(f, brand_type);
        i = list_slays(f, f, NULL, brand, NULL, FALSE);

        /* Describe */
        msg(p, "The %s %s surrounded with an aura of %s.", o_name,
            ((o_ptr->number > 1)? "are": "is"), brand[0]);

        /* Get the right ego type for the object */
        for (i = 0; i < z_info->e_max; i++)
        {
            e_ptr = &e_info[i];
            if (of_has(e_ptr->flags, brand_type))
            {
                for (j = 0; j < EGO_TVALS_MAX; j++)
                {
                    if ((o_ptr->tval == e_ptr->tval[j]) && (o_ptr->sval >= e_ptr->min_sval[j]) &&
                        (o_ptr->sval <= e_ptr->max_sval[j]))
                    {
                        ok = TRUE;
                    }
                }
            }
            if (ok) break;
        }
        o_ptr->ego = &e_info[i];
        ego_apply_magic(o_ptr, 0);
        object_notice_ego(p, o_ptr);

        /* Combine / Reorder the pack (later) */
        p->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

        /* Redraw */
        p->redraw |= (PR_INVEN | PR_EQUIP);

        /* Enchant */
        enchant(p, o_ptr, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Endless source of cash? No way... make them worthless */
        o_ptr->ident |= IDENT_WORTHLESS;
        if (object_was_sensed(o_ptr) || object_is_known(p, o_ptr))
            p->notice |= PN_SQUELCH;
    }
    else
        msg(p, "The branding failed.");
}


/*
 * Brand the current weapon
 */
void brand_weapon(struct player *p, bool with_fire)
{
    object_type *o_ptr;
    bitflag f[OF_SIZE];
    const struct slay *s_ptr;

    o_ptr = &p->inventory[INVEN_WIELD];

    /* Select a brand */
    if (with_fire)
        flags_init(f, OF_SIZE, OF_BRAND_FIRE, OF_BRAND_COLD, FLAG_END);
    else
        flags_init(f, OF_SIZE, OF_BRAND_COOL, OF_BRAND_COLD, FLAG_END);
    s_ptr = random_slay(f);

    brand_object(p, o_ptr, s_ptr->object_flag);
}


/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(object_type *o_ptr)
{
    /* Ammo */
    if (!obj_is_ammo(NULL, o_ptr)) return FALSE;

    /* Magic ammo are always +0 +0 */
    if (o_ptr->sval == SV_AMMO_MAGIC) return FALSE;

    return TRUE;
}


/*
 * Brand chosen ammo
 */
bool brand_ammo(struct player *p)
{
    object_type *o_ptr;
    const struct slay *s_ptr;
    bitflag f[OF_SIZE];
    int item;

    /* No item -> get one */
    if (p->current_value == ITEM_REQUEST)
    {
        get_item(p, 0, HOOK_AMMO);
        return FALSE;
    }

    /* Use current */
    item = p->current_value;

    /* Get the item */
    o_ptr = object_from_item_idx(p, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return FALSE;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p, o_ptr))
    {
        msg(p, "This item belongs to someone else!");
        return FALSE;
    }

    /* Paranoia: requires ammo */
    if (!item_tester_hook_ammo(o_ptr)) return FALSE;

    /* Select the brand */
    flags_init(f, OF_SIZE, OF_BRAND_FIRE, OF_BRAND_COLD, OF_BRAND_POIS, FLAG_END);
    s_ptr = random_slay(f);

    /* Brand the ammo */
    brand_object(p, o_ptr, s_ptr->object_flag);

    /* Redraw */
    if (item < 0) p->redraw |= (PR_FLOOR | PR_ITEMLIST);

    return TRUE;
}


/*
 * Identify an item.
 *
 * `item` is used to print the slot occupied by an object in equip/inven.
 * Any negative value assigned to "item" can be used for specifying an object
 * on the floor.
 */
void do_ident_item(struct player *p, int item, object_type *o_ptr)
{
    char o_name[NORMAL_WID];
    u32b msg_type = 0;

    /* Identify it */
    object_notice_everything(p, o_ptr, FALSE);

    /* Set squelch flag */
    p->notice |= PN_SQUELCH;

    /* Recalculate bonuses */
    p->update |= (PU_BONUS);

    /* Combine / Reorder the pack (later) */
    p->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw */
    p->redraw |= (PR_INVEN | PR_EQUIP | PR_PLUSSES);
    if (item < 0) p->redraw |= (PR_FLOOR | PR_ITEMLIST);

    /* Description */
    object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Determine the message type. */
    if (cursed_p(o_ptr->flags) || worthless_p(o_ptr)) msg_type = MSG_IDENT_BAD;
    else if (o_ptr->artifact) msg_type = MSG_IDENT_ART;
    else if (o_ptr->ego) msg_type = MSG_IDENT_EGO;
    else msg_type = MSG_GENERIC;

    /* Describe */
    if (item >= INVEN_WIELD)
    {
        /* Format and capitalise */
        char *msg = format("%s: %s (%c).", describe_use(p, item), o_name, index_to_label(item));

        msgt(p, msg_type, msg);
    }
    else if (item >= 0)
        msgt(p, msg_type, "In your pack: %s (%c).", o_name, index_to_label(item));
    else
        msgt(p, msg_type, "On the ground: %s.", o_name);
}


/*
 * Get player "number"
 */
int get_player_num(struct player *p)
{
    int num;

    num = (p->chp * 95) / (p->mhp * 10);
    if (p->timed[TMD_MANASHIELD])
        num = (p->csp * 95) / (p->msp * 10);
    if (num >= 7) num = 10;

    return num;
}


/*
 * Update player picture after HP/SP change
 */
void redraw_picture(struct player *p, int old_num)
{
    int new_num;
    int Ind = get_player_index(get_connection(p->conn));

    /* Figure out of if the player's "number" has changed */
    new_num = get_player_num(p);

    /* If so then refresh everyone's view of this player */
    if (new_num != old_num)
        cave_light_spot(cave_get(p->depth), p->py, p->px);

    /* Update health bars */
    update_health(0 - Ind);
}


/* Wipe everything */
bool wipe_spell(int depth, int cy, int cx, int r)
{
    int yy, xx, dy, dx;

    /* Only on random levels */
    if (!random_level(depth)) return FALSE;

    /* Paranoia -- Enforce maximum range */
    if (r > 12) r = 12;

    /* Check around the epicenter */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds_fully(yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cy, cx, yy, xx) > r) continue;

            /* Lose room and vault */
            cave_get(depth)->info[yy][xx] &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_NOTELE);

            /* Lose light and knowledge */
            cave_get(depth)->info[yy][xx] &= ~(CAVE_GLOW);
            forget_spot(depth, yy, xx);

            cave_light_spot(cave_get(depth), yy, xx);

            /* Delete monsters */
            delete_monster(depth, yy, xx);

            /* Destroy "valid" grids */
            if (cave_valid_bold(depth, yy, xx))
            {
                /* Delete objects */
                delete_object(depth, yy, xx);

                /* Turn into basic floor */
                cave_set_feat(cave_get(depth), yy, xx, FEAT_FLOOR);
            }
        }
    }

    return TRUE;
}


/*
 * Increase players hit points to max and notice effects.
 */
void restore_hp(struct player *p)
{
    int old_num = get_player_num(p);

    /* Heal the player */
    p->chp = p->mhp;
    p->chp_frac = 0;

    /* Hack -- Redraw picture */
    redraw_picture(p, old_num);

    /* Redraw */
    p->redraw |= PR_HP;
}


/*
 * Increase players mana points to max and notice effects.
 */
void restore_sp(struct player *p)
{
    int old_num = get_player_num(p);

    /* Restore mana */
    p->csp = p->msp;
    p->csp_frac = 0;

    /* Hack -- Redraw picture */
    redraw_picture(p, old_num);

    /* Redraw */
    p->redraw |= PR_MANA;
}


/*
 * Increase players mana, notice effects
 */
bool sp_player(struct player *p, int num)
{
    /* Healing needed */
    if (p->csp < p->msp)
    {
        int old_num = get_player_num(p);

        /* Gain mana */
        p->csp += num;

        /* Enforce maximum */
        if (p->csp >= p->msp)
        {
            p->csp = p->msp;
            p->csp_frac = 0;
        }

        /* Hack -- Redraw picture */
        redraw_picture(p, old_num);

        /* Redraw */
        p->redraw |= (PR_MANA);

        /* Print a nice message */
        msg(p, "Your feel your head clear.");

        /* Notice */
        return (TRUE);
    }

    /* Ignore */
    return (FALSE);
}


/*
 * Reveal monsters
 */
bool reveal_monsters(struct player *p, bool aware)
{
    bool detect = FALSE;
    bool detected_invis, detected_creatures;

    /* Reveal monsters */
    detected_creatures = detect_monsters_normal(p, FALSE, aware);
    detected_invis = detect_monsters_invis(p, FALSE, aware);

    /* Describe result, and clean up */
    if (detected_creatures || detected_invis)
    {
        /* Mega-Hack -- Fix the monsters and players */
        update_monsters(p->depth, FALSE);
        update_players();

        /* Full refresh (includes monster/object lists) */
        p->full_refresh = TRUE;

        /* Handle Window stuff */
        handle_stuff(p);

        /* Normal refresh (without monster/object lists) */
        p->full_refresh = FALSE;

        detect = TRUE;

        /* Describe, and wait for acknowledgement */
        msg(p, "You sense the presence of creatures!");
        party_msg_near(p, " senses the presence of creatures!");
        message_flush(p);

        /* Hack -- Pause */
        if (OPT_P(p, pause_after_detect)) Send_pause(p);
    }

    /* Result */
    return (detect);
}


/*
 * Fear monsters
 */
bool fear_monsters(struct player *p)
{
    return project_los(p, GF_TURN_ALL, p->lev, TRUE);
}


/*
 * Stun monsters
 */
bool stun_monsters(struct player *p)
{
    return project_los(p, GF_STUN, p->lev, TRUE);
}


/*
 * Cast a breath attack
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_breath(struct player *p, bitflag mon_breath[RSF_SIZE], int dir, int dam)
{
    s16b ty, tx;
    bool result;
    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    int flag, thrown_breath, typ, rad;
    int breath[20], num = 0;
    int Ind = get_player_index(get_connection(p->conn));

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return FALSE;

    /* Use the given direction */
    tx = p->px + 99 * ddx[dir];
    ty = p->py + 99 * ddy[dir];

    /* Hack -- Use an actual "target" */
    if ((dir == 5) && target_okay(p))
    {
        flg &= ~PROJECT_STOP;
        target_get(p, &tx, &ty);
    }

    /* No breath attacks */
    if (rsf_is_empty(mon_breath))
    {
        msg(p, "You are not able to breathe anything but air...");
        return (FALSE);
    }

    /* Extract the breath attacks */
    for (flag = rsf_next(mon_breath, FLAG_START); flag != FLAG_END;
        flag = rsf_next(mon_breath, flag + 1))
    {
        breath[num++] = flag;
    }

    /* Choose a breath attack */
    thrown_breath = breath[randint0(num)];

    /* Cast the breath attack */
    switch (thrown_breath)
    {
        case RSF_BR_ACID:
            msgt(p, MSG_BR_ACID, "You breathe acid."); typ = GF_ACID; break;
        case RSF_BR_ELEC:
            msgt(p, MSG_BR_ELEC, "You breathe lightning."); typ = GF_ELEC; break;
        case RSF_BR_FIRE:
            msgt(p, MSG_BR_FIRE, "You breathe fire."); typ = GF_FIRE; break;
        case RSF_BR_COLD:
            msgt(p, MSG_BR_FROST, "You breathe frost."); typ = GF_COLD; break;
        case RSF_BR_POIS:
            msgt(p, MSG_BR_GAS, "You breathe gas."); typ = GF_POIS; break;
        case RSF_BR_NETH:
            msgt(p, MSG_BR_NETHER, "You breathe nether."); typ = GF_NETHER; break;
        case RSF_BR_LIGHT:
            msgt(p, MSG_BR_LIGHT, "You breathe light."); typ = GF_LIGHT; break;
        case RSF_BR_DARK:
            msgt(p, MSG_BR_DARK, "You breathe darkness."); typ = GF_DARK; break;
        case RSF_BR_SOUN:
            msgt(p, MSG_BR_SOUND, "You breathe sound."); typ = GF_SOUND; break;
        case RSF_BR_CHAO:
            msgt(p, MSG_BR_CHAOS, "You breathe chaos."); typ = GF_CHAOS; break;
        case RSF_BR_DISE:
            msgt(p, MSG_BR_DISEN, "You breathe disenchantment."); typ = GF_DISEN; break;
        case RSF_BR_NEXU:
            msgt(p, MSG_BR_NEXUS, "You breathe nexus."); typ = GF_NEXUS; break;
        case RSF_BR_TIME:
            msgt(p, MSG_BR_TIME, "You breathe time."); typ = GF_TIME; break;
        case RSF_BR_INER:
            msgt(p, MSG_BR_INERTIA, "You breathe inertia."); typ = GF_INERT; break;
        case RSF_BR_GRAV:
            msgt(p, MSG_BR_GRAVITY, "You breathe gravity."); typ = GF_GRAVITY; break;
        case RSF_BR_SHAR:
            msgt(p, MSG_BR_SHARDS, "You breathe shards."); typ = GF_SHARD; break;
        case RSF_BR_PLAS:
            msgt(p, MSG_BR_PLASMA, "You breathe plasma."); typ = GF_PLASMA; break;
        case RSF_BR_WALL:
            msgt(p, MSG_BR_FORCE, "You breathe force."); typ = GF_FORCE; break;
        case RSF_BR_MANA:
            msg(p, "You breathe magical energy."); typ = GF_MANA; break;
        case RSF_BR_WATE:
            msgt(p, MSG_BR_WATE, "You breathe water."); typ = GF_WATER; break;
    }

    /* Determine the radius of the blast */
    rad = 2;

    /* Handle polymorphed players */
    if (p->r_idx && (dam == 0))
    {
        monster_race *r_ptr = &r_info[p->r_idx];
        const char *pself = player_self(p);

        /* Damage */
        dam = p->chp / 3;

        /* Breathing damages health instead of costing mana */
        strnfmt(p->died_flavor, sizeof(p->died_flavor), "exhausted %s with breathing", pself);
        take_hit(p, p->mhp / 20, "the strain of breathing", FALSE);
        if (p->is_dead) return (FALSE);

        /* Powerful breath */
        rad = (rf_has(r_ptr->flags, RF_POWERFUL) ? 3 : 2);
    }

    /* Analyze the "dir" and the "target".  Hurt items on floor. */
    p->current_sound = -2;
    result = project(0 - Ind, rad, p->depth, ty, tx, dam, typ, flg, "vaporized");
    p->current_sound = -1;
    return result;
}


/*
 * Cast a melee range spell
 */
bool fire_melee(struct player *p, int typ, int dir, int dam)
{
    s16b ty, tx;
    int Ind = get_player_index(get_connection(p->conn));

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return FALSE;

    /* Use the given direction */
    tx = p->px + ddx[dir];
    ty = p->py + ddy[dir];

    /* Hack -- Use an actual "target" */
    if ((dir == 5) && target_okay(p))
    {
        target_get(p, &tx, &ty);

        /* Check distance */
        if (distance(p->py, p->px, ty, tx) > 1)
        {
            msg(p, "Target out of range.");
            return FALSE;
        }
    }

    /* Analyze the "dir" and the "target", do NOT explode */
    return (project(0 - Ind, 0, p->depth, ty, tx, dam, typ, PROJECT_KILL, "annihilated"));
}    


bool blind_monster(struct player *p, int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_AWARE;

    return (project_hook(p, GF_BLIND, dir, plev, flg, "killed"));
}


void current_clear(struct player *p)
{
    p->current_spell = -1;
    p->current_item = ITEM_REQUEST;
    p->current_value = ITEM_REQUEST;
}


void sea_runes(struct player *p)
{
    int x, y, rad = 2 + (p->lev / 20);

    /* Only on random levels */
    if (!random_level(p->depth))
    {
        msg(p, "You cannot create glyphs here...");
        return;
    }

    msg_misc(p, " lays down some glyphs of protection.");

    for (x = p->px - rad; x <= p->px + rad; x++)
    {
        for (y = p->py - rad; y <= p->py + rad; y++)
        {
            /* First we must be in the dungeon */
            if (!in_bounds_fully(y, x)) continue;

            /* Is it a naked grid? */
            if (!cave_naked_bold(p->depth, y, x)) continue;

            /* Now we want a circle */
            if (distance(y, x, p->py, p->px) != rad) continue;

            /* Everything ok... then put a glyph */
            cave_set_feat(cave_get(p->depth), y, x, FEAT_GLYPH);
        }
    }
}


void wall_creation(struct player *p, int dir)
{
    int Ind = get_player_index(get_connection(p->conn));

    /* Only on random levels */
    if (!random_level(p->depth))
    {
        msg(p, "You cannot create walls here...");
        return;
    }

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    if (dir)
    {
        project(0 - Ind, 0, p->depth, p->py + ddy[dir], p->px + ddx[dir], 0, GF_STONE_WALL,
            PROJECT_GRID, "killed");
    }
    else
        fire_ball(p, GF_STONE_WALL, 0, 1, 1);
}


static struct player *get_inscribed_player(struct player *p, quark_t note)
{
    struct player *q = NULL;
    char* inscription = (char*)quark_str(note);

    /* Check for a valid inscription */
    if (inscription == NULL)
    {
        msg(p, "Nobody to use the power with.");
        return NULL;
    }

    /* Scan the inscription for @P */
    while ((*inscription != '\0') && !q)
    {
        if (*inscription == '@')
        {
            inscription++;

            /* A valid @P has been located */
            if (*inscription == 'P')
            {
                inscription++;
                q = player_lookup(inscription);
            }
        }
        inscription++;
    }

    if (!q) msg(p, "Player is not on.");

    return q;
}


void mind_vision(struct player *p, quark_t note)
{
    struct player *q = get_inscribed_player(p, note);

    if (!q) return;

    if (p == q)
    {
        msg(p, "You cannot link to your own mind.");
        return;
    }
    if (p->esp_link)
    {
        msg(p, "Your mind is already linked.");
        return;
    }
    if (q->esp_link)
    {
        msg(p, "%s's mind is already linked.", q->name);
        return;
    }

    /* Not if hostile */
    if (pvp_check(p, q, PVP_CHECK_ONE, TRUE, FEAT_NONE))
    {
        /* Message */
        msg(p, "%s's mind is not receptive.", q->name);
        return;
    }

    msg(q, "%s infiltrates your mind.", p->name);
    msg(p, "You infiltrate %s's mind.", q->name);
    p->esp_link = q->id;
    p->esp_link_type = LINK_DOMINANT;

    q->esp_link = p->id;
    q->esp_link_type = LINK_DOMINATED;
    q->redraw |= PR_MAP;
}


bool telekinesis(struct player *p, quark_t note)
{
    object_type *o_ptr;
    int item, amt;
    struct player *q;

    /* No item -> get one */
    if (p->current_value == ITEM_REQUEST)
    {
        get_item(p, 0, HOOK_UNDEFINED);
        return FALSE;
    }

    /* Use current */
    item = p->current_value;

    /* Get the item */
    o_ptr = object_from_item_idx(p, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return FALSE;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p, o_ptr))
    {
        msg(p, "This item belongs to someone else!");
        return FALSE;
    }

    /* Forbid artifacts */
    if (o_ptr->artifact)
    {
        msg(p, "The object is too powerful to be sent...");
        return FALSE;
    }

    q = get_inscribed_player(p, note);
    if (!q) return FALSE;

    /* Note that the pack is too full */
    if (!inven_carry_okay(q, o_ptr))
    {
        msg(p, "%s has no room for another object.", q->name);
        return FALSE;
    }

    /* Note that the pack is too heavy */
    if (!weight_okay(q, o_ptr))
    {
        msg(p, "%s is already too burdened to carry another object.", q->name);
        return FALSE;
    }

    /* Restricted by choice */
    if (OPT_P(q, birth_no_stores))
    {
        msg(p, "%s cannot be reached.", q->name);
        return FALSE;
    }

    amt = o_ptr->number;

    /* Actually teleport the object to the player inventory */
    inven_carry(q, o_ptr, TRUE);

    /* Combine the pack */
    q->notice |= (PN_COMBINE);

    /* Redraw */
    q->redraw |= (PR_INVEN | PR_EQUIP);

    /* Wipe it */
    item_decrease(p, item, amt, FALSE);

    /* Combine the pack */
    p->notice |= (PN_COMBINE);

    /* Redraw */
    p->redraw |= (PR_INVEN | PR_EQUIP);

    msg(q, "You are hit by a powerful magic wave from %s.", p->name);
    return TRUE;
}


void elemental_brand(struct player *p, int tries)
{
    object_type *o_ptr = &p->inventory[INVEN_WIELD];
    int i;
    const char *act;
    int brand;

    /* You can never modify artifacts/ego-items */
    /* You can never modify worthless/cursed items */
    if (o_ptr->kind && !cursed_p(o_ptr->flags) && o_ptr->kind->cost && !o_ptr->artifact &&
        !o_ptr->ego)
    {
        char o_name[NORMAL_WID];
        bitflag note_f[OF_SIZE];

        object_desc(p, o_name, sizeof(o_name), o_ptr, ODESC_BASE);

        /* Brand the object */
        o_ptr->ego = &e_info[EGO_ELEMENTAL];
        ego_apply_magic(o_ptr, 0);
        object_notice_ego(p, o_ptr);

        /* Enchant */
        enchant(p, o_ptr, randint0(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Add some brands */
        for (i = 0; i < tries; i++)
        {
            /* Select a brand */
            switch (randint0(5))
            {
                case 0: act = "fiery"; brand = OF_BRAND_FIRE; break;
                case 1: act = "chilling"; brand = OF_BRAND_COLD; break;
                case 2: act = "electric"; brand = OF_BRAND_ELEC; break;
                case 3: act = "acidic"; brand = OF_BRAND_ACID; break;
                case 4: act = "toxic"; brand = OF_BRAND_POIS; break;
            }

            /* Check brand */
            if (of_has(o_ptr->flags, brand)) continue;

            /* Describe */
            msg(p, "A %s aura surrounds the %s.", act, o_name);

            /* Brand the object */
            of_on(o_ptr->flags, brand);
            if (object_was_sensed(o_ptr) || object_is_known(p, o_ptr))
            {
                of_wipe(note_f);
                of_on(note_f, brand);
                object_notice_slays(p, o_ptr, note_f);
            }
        }

        /* Endless source of cash? No way... make them worthless */
        o_ptr->ident |= IDENT_WORTHLESS;
        if (object_was_sensed(o_ptr) || object_is_known(p, o_ptr))
            p->notice |= PN_SQUELCH;
    }
    else
        msg(p, "The branding failed.");
}


/*
 * Create potions of poison from any potion
 */
bool create_poison(struct player *p)
{
    object_type *o_ptr;
    int amt;
    object_type *i_ptr;
    object_type object_type_body;
    int item;

    /* No item -> get one */
    if (p->current_value == ITEM_REQUEST)
    {
        get_item(p, TV_POTION, HOOK_UNDEFINED);
        return FALSE;
    }

    /* Use current */
    item = p->current_value;

    /* Get the item */
    o_ptr = object_from_item_idx(p, item, 0, TRUE);

    /* Paranoia: requires an item */
    if (!o_ptr || !o_ptr->kind) return FALSE;

    /* Restricted by choice */
    if ((item < 0) && !is_owner(p, o_ptr))
    {
        msg(p, "This item belongs to someone else!");
        return FALSE;
    }

    /* Paranoia: requires a potion */
    if (o_ptr->tval != TV_POTION) return FALSE;

    /* Amount */
    amt = o_ptr->number;

    /* Message */
    msg(p, "You create %d potions of poison.", amt);

    /* Eliminate the item */
    item_decrease(p, item, amt, FALSE);

    /* Get local object */
    i_ptr = &object_type_body;

    /* Create the potions */
    object_prep(i_ptr, lookup_kind(TV_POTION, SV_POTION_POISON), 0, MINIMISE);
    i_ptr->number = amt;

    /* Set origin */
    set_origin(i_ptr, ORIGIN_ACQUIRE, p->depth, 0);

    drop_near(p, cave_get(p->depth), i_ptr, 0, p->py, p->px, TRUE);

    return TRUE;
}
