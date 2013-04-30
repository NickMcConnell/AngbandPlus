/*
 * File: cmd2.c
 * Purpose: Chest and door opening/closing, disarming, running, resting, ...
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
#include "attack.h"
#include "cmds.h"
#include "files.h"
#include "generate.h"
#include "monster/mon-timed.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "s-spells.h"
#include "squelch.h"


/*
 * Go up one level
 */
void do_cmd_go_up(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    byte new_level_method;

    /* Check preventive inscription '^<' */
    __trap(p_ptr, CPI(p_ptr, '<'))

    /* Verify stairs */
    if (!cave_isupstairs(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px) && !p_ptr->ghost &&
        !p_ptr->timed[TMD_PROBTRAVEL])
    {
        msg(p_ptr, "I see no up staircase here.");
        return;
    }

    /* Can't go up outside of the dungeon */
    if (p_ptr->depth <= 0)
    {
        msg(p_ptr, "There is nothing above you.");
        return;
    }

    /* Ironman */
    if ((cfg_limit_stairs == 2) || OPT_P(p_ptr, birth_ironman))
    {
        /* Going up is forbidden (except ghosts) */
        if (!p_ptr->ghost)
        {
            msg(p_ptr, "Morgoth awaits you in the darkness below.");
            return;
        }
    }

    /* Hack -- DM redesigning the level */
    if (players_on_depth[p_ptr->depth - 1] == INHIBIT_DEPTH)
    {
        msg(p_ptr, "Something prevents you from going up...");
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Success */
    if (cave_isupstairs(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px))
    {
        msgt(p_ptr, MSG_STAIRS_UP, "You enter a maze of up staircases.");
        new_level_method = LEVEL_UP;
    }
    else
    {
        msg(p_ptr, "You float upwards.");
        new_level_method = LEVEL_GHOST;
    }

    /* Change level */
    dungeon_change_level(p_ptr, p_ptr->depth - 1, new_level_method);
}


/*
 * Go down one level
 */
void do_cmd_go_down(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    byte new_level_method;

    /* Check preventive inscription '^>' */
    __trap(p_ptr, CPI(p_ptr, '>'))

    /* Ghosts who are not dungeon masters can't go down if ghost_diving is off */
    if (p_ptr->ghost && !cfg_ghost_diving && !is_dm_p(p_ptr))
    {
        msg(p_ptr, "You seem unable to go down. Try going up.");
        return;
    }

    /* Verify stairs */
    if (!cave_isdownstairs(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px) && !p_ptr->ghost &&
        !p_ptr->timed[TMD_PROBTRAVEL])
    {
        msg(p_ptr, "I see no down staircase here.");
        return;
    }

    /* Can't go down in the wilderness */
    if (p_ptr->depth < 0)
    {
        msg(p_ptr, "There is nothing below you.");
        return;
    }

    /* Verify maximum depth */
    if (p_ptr->depth == MAX_DEPTH - 1)
    {
        msg(p_ptr, "You are at the bottom of the dungeon.");
        return;
    }

    /* Verify basic quests */
    if (!quest_done(p_ptr, p_ptr->depth))
    {
        msg(p_ptr, "Something prevents you from going down...");
        return;
    }

    /* Hack -- DM redesigning the level */
    if (players_on_depth[p_ptr->depth + 1] == INHIBIT_DEPTH)
    {
        msg(p_ptr, "Something prevents you from going down...");
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Success */
    if (cave_isdownstairs(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px))
    {
        msgt(p_ptr, MSG_STAIRS_DOWN, "You enter a maze of down staircases.");
        new_level_method = LEVEL_DOWN;
    }
    else
    {
        msg(p_ptr, "You float downwards.");
        new_level_method = LEVEL_GHOST;
    }

    /* Change level */
    dungeon_change_level(p_ptr, p_ptr->depth + 1, new_level_method);
}


/*
 * Simple command to "search" for some turns
 */
void do_cmd_search(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Repeat searching 10 times */
    p_ptr->search_request = 10;
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Stop searching */
    if (p_ptr->searching)
    {
        /* Clear the searching flag */
        p_ptr->searching = FALSE;

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);
    }

    /* Start searching */
    else
    {
        /* Set the searching flag */
        p_ptr->searching = TRUE;

        /* Update stuff */
        p_ptr->update |= (PU_BONUS);

        /* Redraw stuff */
        p_ptr->redraw |= (PR_STATE | PR_SPEED);
    }
}


/*
 * Return the number of doors/traps around (or under) the character.
 */
static int count_feats(int Ind, int *y, int *x, bool (*test)(struct cave *c, int y, int x),
    bool under)
{
    player_type *p_ptr = player_get(Ind);
    int d;
    int xx, yy;
    int count = 0; /* Count how many matches */

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        /* If not searching under player continue */
        if ((d == 8) && !under) continue;

        /* Extract adjacent (legal) location */
        yy = p_ptr->py + ddy_ddd[d];
        xx = p_ptr->px + ddx_ddd[d];

        /* Paranoia */
        if (!in_bounds_fully(yy, xx)) continue;

        /* Must have knowledge */
        if (!(p_ptr->cave->info[yy][xx] & CAVE_MARK)) continue;

        /* Not looking for this feature */
        if (!((*test)(cave_get(p_ptr->depth), yy, xx))) continue;

        /* Count it */
        ++count;

        /* Remember the location of the last feature found */
        *y = yy;
        *x = xx;
    }

    /* All done */
    return count;
}


/*
 * Extract a "direction" which will move one step from the player location
 * towards the given "target" location (or "5" if no motion necessary).
 */
static int coords_to_dir(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    return (motion_dir(p_ptr->py, p_ptr->px, y, x));
}


/*
 * Determine if a given grid may be "opened"
 */
static bool do_cmd_open_test(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Ghosts cannot open things */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        msg(p_ptr, "You cannot open things!");
        return FALSE;
    }

    /* Handle polymorphed players */
    if (p_ptr->r_idx && !OPT_P(p_ptr, birth_fruit_bat))
    {
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        if (!rf_has(r_ptr->flags, RF_OPEN_DOOR))
        {
            msg(p_ptr, "You cannot open things!");
            return FALSE;
        }
    }

    /* Must have knowledge */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK))
    {
        msg(p_ptr, "You see nothing there.");
        return FALSE;
    }

    /* Must be a closed door (or permawall for house doors) */
    if (!cave_isbasicdoor(cave_get(p_ptr->depth), y, x) &&
        (cave_get(p_ptr->depth)->feat[y][x] != FEAT_PERM_EXTRA))
    {
        msgt(p_ptr, MSG_NOTHING_TO_OPEN, "You see nothing there to open.");
        return FALSE;
    }

    /* Okay */
    return TRUE;
}


/* Forward declaration */
static bool create_house_door(int Ind, int x, int y);


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    int i, j, k;
    bool more = FALSE;

    /* Verify legality */
    if (!do_cmd_open_test(Ind, y, x)) return (FALSE);

    /* Player Houses */
    if (cave_ishomedoor(cave_get(p_ptr->depth), y, x))
    {
        i = pick_house(p_ptr->depth, y, x);

        /* Tell the DM who owns the house */
        if (p_ptr->dm_flags & DM_HOUSE_CONTROL)
        {
            /* Message */
            if (houses[i].ownerid > 0)
                msg(p_ptr, "This house belongs to %s.", houses[i].ownername);
            else
                msg(p_ptr, "This house is not owned.");
        }

        /* Do we own this house? */
        else if (house_owned_by(p_ptr, i))
        {
            /* If someone is in our store, we eject them (anti-exploit) */
            for (k = 1; k <= NumPlayers; k++)
            {
                player_type *q_ptr = player_get(k);

                if (Ind == k) continue;

                /* Eject any shopper */
                if ((q_ptr->player_store_num == i) && (q_ptr->store_num == STORE_PLAYER))
                {
                    msg(q_ptr, "The shopkeeper locks the doors.");
                    Send_store_leave(q_ptr);
                }
            }

            /* Open the door */
            cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_HOME_OPEN);

            /* Update the visuals */
            update_visuals(p_ptr->depth);

            /* Sound */
            sound(p_ptr, MSG_OPENDOOR);
        }

        /* He's not the owner, check if owned */
        else if (houses[i].ownerid > 0)
        {
            /* Player owned store! */

            /* Disturb */
            disturb(p_ptr, 0, 0);

            /* Hack -- Enter store */
            do_cmd_store(Ind, i);
        }

        /* Not owned */
        else
        {
            int factor;
            s32b price;

            /* Take CHR into account */
            factor = adj_chr_gold[p_ptr->state.stat_ind[A_CHR]];
            price = (s32b)((double)houses[i].price * factor / 100);

            /* Tell him the price */
            msg(p_ptr, "This house costs %ld gold.", price);
        }
    }

    /* Open a perma wall */
    else if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_PERM_EXTRA)
    {
        /*
         * Opening a wall? Either the player has lost his mind or he
         * is trying to create a door!
         */
        create_house_door(Ind, x, y);
    }

    /* Jammed door */
    else if (cave_isjammeddoor(cave_get(p_ptr->depth), y, x))
    {
        /* Stuck */
        msg(p_ptr, "The door appears to be stuck.");
    }

    /* Locked door */
    else if (cave_islockeddoor(cave_get(p_ptr->depth), y, x))
    {
        /* Disarm factor */
        i = p_ptr->state.skills[SKILL_DISARM];

        /* Penalize some conditions */
        if (p_ptr->timed[TMD_BLIND] || no_light(p_ptr)) i = i / 10;
        if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

        /* Extract the lock power */
        j = cave_get(p_ptr->depth)->feat[y][x] - FEAT_DOOR_HEAD;

        /* Extract the difficulty XXX XXX XXX */
        j = i - (j * 4);

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success */
        if (magik(j))
        {
            /* Message */
            msgt(p_ptr, MSG_LOCKPICK, "You have picked the lock.");

            /* Open the door */
            cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_OPEN);

            /* Update the visuals */
            update_visuals(p_ptr->depth);
        }

        /* Failure */
        else
        {
            /* Message */
            msgt(p_ptr, MSG_LOCKPICK_FAIL, "You failed to pick the lock.");

            /* We may keep trying */
            more = TRUE;
        }
    }

    /* Closed door */
    else
    {
        /* Open the door */
        cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_OPEN);

        /* Update the visuals */
        update_visuals(p_ptr->depth);

        /* Sound */
        sound(p_ptr, MSG_OPENDOOR);
    }

    /* Result */
    return (more);
}


/*
 * Attack monster or player in the way
 */
static void attack_blocker(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    int m_idx = cave_get(p_ptr->depth)->m_idx[y][x];

    /* Monster in the way */
    if (m_idx > 0)
    {
        monster_type *m_ptr = cave_monster(cave_get(p_ptr->depth), m_idx);

        /* Mimics surprise the player */
        if (is_mimicking(m_ptr))
        {
            become_aware(p_ptr, m_ptr);

            /* Mimic wakes up */
            if (pvm_check(Ind, m_idx))
                mon_clear_timed(p_ptr, m_ptr, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE, FALSE);
        }
        else
        {
            /* Message */
            msg(p_ptr, "There is a monster in the way!");

            /* Attack */
            if (pvm_check(Ind, m_idx)) py_attack(Ind, y, x);
        }
    }

    /* Player in the way */
    else
    {
        player_type *q_ptr = player_get(0 - m_idx);

        /* Reveal mimics */
        if (q_ptr->k_idx)
            aware_player(p_ptr, q_ptr);

        /* Message */
        msg(p_ptr, "There is a player in the way!");

        /* Attack */
        if (pvp_check(p_ptr, q_ptr, PVP_DIRECT, TRUE, cave_get(p_ptr->depth)->feat[y][x]))
            py_attack(Ind, y, x);
    }
}


/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(int Ind, int dir, bool easy)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    s16b o_idx;
    bool more = FALSE;

    /* Easy Open */
    if (easy)
    {
        int num_doors, num_chests;

        /* Count closed doors */
        num_doors = count_feats(Ind, &y, &x, cave_isbasicdoor, FALSE);

        /* Count chests (locked) */
        num_chests = count_chests(p_ptr, &y, &x, CHEST_OPENABLE);

        /* Use the last target found */
        if ((num_doors + num_chests) >= 1)
            dir = coords_to_dir(Ind, y, x);
    }

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Check for chests */
    o_idx = chest_check(p_ptr, y, x, CHEST_OPENABLE);

    /* Verify legality */
    if (!o_idx && !do_cmd_open_test(Ind, y, x))
    {
        /* Cancel repeat */
        disturb(p_ptr, 0, 0);
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    if (player_confuse_dir(p_ptr, &dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];

        /* Check for chests */
        o_idx = chest_check(p_ptr, y, x, CHEST_OPENABLE);
    }

    /* Something in the way */
    if (cave_get(p_ptr->depth)->m_idx[y][x] != 0)
    {
        /* Attack */
        attack_blocker(Ind, y, x);
    }

    /* Chest */
    else if (o_idx)
    {
        /* Open the chest */
        more = do_cmd_open_chest(p_ptr, y, x, o_idx);
    }

    /* Door */
    else
    {
        /* Open the door */
        more = do_cmd_open_aux(Ind, y, x);
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(p_ptr, 0, 0);
}


/*
 * Determine if a given grid may be "closed"
 */
static bool do_cmd_close_test(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Ghosts cannot close things */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        /* Message */
        msg(p_ptr, "You cannot close things!");

        /* Nope */
        return (FALSE);
    }

    /* Handle polymorphed players */
    if (p_ptr->r_idx && !OPT_P(p_ptr, birth_fruit_bat))
    {
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        if (!rf_has(r_ptr->flags, RF_OPEN_DOOR))
        {
            /* Message */
            msg(p_ptr, "You cannot close things!");

            /* Nope */
            return (FALSE);
        }
    }

    /* Check preventive inscription '^c' */
    __trapR(p_ptr, CPI(p_ptr, 'c'), FALSE)

    /* Must have knowledge */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK))
    {
        /* Message */
        msg(p_ptr, "You see nothing there.");

        /* Nope */
        return (FALSE);
    }

    /* Require open/broken door */
    if (!cave_isbasicopen(cave_get(p_ptr->depth), y, x) &&
        (cave_get(p_ptr->depth)->feat[y][x] != FEAT_BROKEN))
    {
        /* Message */
        msg(p_ptr, "You see nothing there to close.");

        /* Nope */
        return (FALSE);
    }

    /* Okay */
    return (TRUE);
}


/*
 * Perform the basic "close" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    bool more = FALSE;
    int i;

    /* Verify legality */
    if (!do_cmd_close_test(Ind, y, x)) return (FALSE);

    /* Broken door */
    if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_BROKEN)
    {
        /* Message */
        msg(p_ptr, "The door appears to be broken.");
    }

    /* House door, close it */
    else if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_HOME_OPEN)
    {
        /* Find this house */
        i = pick_house(p_ptr->depth, y, x);

        /* Close the door */
        cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_HOME_HEAD + houses[i].color);

        /* Update the visuals */
        update_visuals(p_ptr->depth);

        /* Sound */
        sound(p_ptr, MSG_SHUTDOOR);
    }

    /* Close the door */
    else
    {
        /* Close the door */
        cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_DOOR_HEAD);

        /* Update the visuals */
        update_visuals(p_ptr->depth);

        /* Sound */
        sound(p_ptr, MSG_SHUTDOOR);
    }

    /* Result */
    return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(int Ind, int dir, bool easy)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    bool more = FALSE;

    /* Easy Close */
    if (easy)
    {
        /* Count open doors */
        if (count_feats(Ind, &y, &x, cave_isbasicopen, FALSE) >= 1)
            dir = coords_to_dir(Ind, y, x);
    }

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!do_cmd_close_test(Ind, y, x))
    {
        /* Cancel repeat */
        disturb(p_ptr, 0, 0);
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    if (player_confuse_dir(p_ptr, &dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    /* Something in the way */
    if (cave_get(p_ptr->depth)->m_idx[y][x] != 0)
    {
        /* Attack */
        attack_blocker(Ind, y, x);
    }

    /* Door */
    else
    {
        /* Close door */
        more = do_cmd_close_aux(Ind, y, x);
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(p_ptr, 0, 0);
}


/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Ghosts cannot tunnel */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        /* Message */
        msg(p_ptr, "You cannot tunnel.");

        /* Nope */
        return (FALSE);
    }

    /* Check preventive inscription '^T' */
    __trapR(p_ptr, CPI(p_ptr, 'T'), FALSE)

    /* Must have knowledge */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK))
    {
        /* Message */
        msg(p_ptr, "You see nothing there.");

        /* Nope */
        return (FALSE);
    }

    /* Must be a wall/door/etc */
    if (cave_floor_bold(p_ptr->depth, y, x) ||
        (cave_get(p_ptr->depth)->feat[y][x] == FEAT_PERM_CLEAR))
    {
        /* Message */
        msg(p_ptr, "You see nothing there to tunnel.");

        /* Nope */
        return (FALSE);
    }

    /* No tunneling through house doors */
    if ((cave_get(p_ptr->depth)->feat[y][x] >= FEAT_HOME_OPEN) &&
        (cave_get(p_ptr->depth)->feat[y][x] <= FEAT_HOME_TAIL))
    {
        /* Message */
        msg(p_ptr, "You cannot tunnel through house doors.");

        /* Nope */
        return (FALSE);
    }

    /* No tunneling on special levels */
    if (check_special_level(p_ptr->depth))
    {
        msg(p_ptr, "Nothing happens.");
        return (FALSE);
    }

    /* Okay */
    return (TRUE);
}


/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * Attempting to do so will produce floor grids which are not part
 * of the room, and whose "illumination" status do not change with
 * the rest of the room.
 */
static bool twall(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    byte feat = ((p_ptr->depth > 0)? FEAT_FLOOR: FEAT_DIRT);

    /* Paranoia -- Require a wall or door or some such */
    if (cave_floor_bold(p_ptr->depth, y, x)) return (FALSE);

    /* Sound */
    sound(p_ptr, MSG_DIG);

    /* Forget the wall */
    forget_spot(p_ptr->depth, y, x);

    /* Remove the feature */
    cave_set_feat(cave_get(p_ptr->depth), y, x, feat);

    /* Update the visuals */
    update_visuals(p_ptr->depth);

    /* Fully update the flow */
    fully_update_flow(p_ptr->depth);

    /* Result */
    return (TRUE);
}


/*
 * Perform the basic "tunnel" command
 *
 * Assume there is no monster blocking the destination
 *
 * Uses "twall" (above) to do all "terrain feature changing".
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    bool more = FALSE;

    /* Verify legality */
    if (!do_cmd_tunnel_test(Ind, y, x)) return (FALSE);

    /* Tree */
    if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_TREE)
    {
        /* Mow down the vegetation */
        if (CHANCE(p_ptr->state.skills[SKILL_DIGGING] + wielding_cut(p_ptr) * 10, 400) &&
            twall(Ind, y, x))
        {
            if (!p_ptr->depth) trees_in_town--;

            /* Message */
            msg(p_ptr, "You hack your way through the vegetation.");
        }
        else
        {
            /* Message, keep digging */
            msg(p_ptr, "You attempt to clear a path.");
            more = TRUE;
        }
    }

    /* Dead tree */
    else if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_EVIL_TREE)
    {
        /* Mow down the vegetation */
        if (CHANCE(p_ptr->state.skills[SKILL_DIGGING] + wielding_cut(p_ptr) * 10, 600) &&
            twall(Ind, y, x))
        {
            /* Message */
            msg(p_ptr, "You hack your way through the vegetation.");
        }
        else
        {
            /* Message, keep digging */
            msg(p_ptr, "You attempt to clear a path.");
            more = TRUE;
        }
    }

    /* Hack -- Pit walls (to fool the player) */
    else if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_PERM_FAKE)
    {
        msg(p_ptr, "You tunnel into the granite wall.");
        more = TRUE;
    }

    /* Permanent */
    else if (cave_isperm(cave_get(p_ptr->depth), y, x))
        msg(p_ptr, "This seems to be permanent rock.");

    /* Mountain */
    else if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_MOUNTAIN)
        msg(p_ptr, "Digging a tunnel into that mountain would take forever!");

    /* Granite */
    else if (cave_isrock(cave_get(p_ptr->depth), y, x))
    {
        /* Tunnel */
        if (CHANCE(p_ptr->state.skills[SKILL_DIGGING] - 40, 1600) && twall(Ind, y, x))
            msg(p_ptr, "You have finished the tunnel.");

        /* Keep trying */
        else
        {
            /* We may continue tunelling */
            msg(p_ptr, "You tunnel into the granite wall.");
            more = TRUE;
        }
    }

    /* Quartz / Magma */
    else if (cave_ismagma(cave_get(p_ptr->depth), y, x) || cave_isquartz(cave_get(p_ptr->depth), y, x))
    {
        bool okay = FALSE;
        bool gold = FALSE;
        bool hard = FALSE;

        /* Found gold */
        if (cave_get(p_ptr->depth)->feat[y][x] >= FEAT_MAGMA_H) gold = TRUE;

        /* Extract "quartz" flag XXX XXX XXX */
        if ((cave_get(p_ptr->depth)->feat[y][x] - FEAT_MAGMA) & 0x01) hard = TRUE;

        /* Quartz */
        if (hard)
            okay = CHANCE(p_ptr->state.skills[SKILL_DIGGING] - 20, 800);

        /* Magma */
        else
            okay = CHANCE(p_ptr->state.skills[SKILL_DIGGING] - 10, 400);

        /* Success */
        if (okay && twall(Ind, y, x))
        {
            /* Found treasure */
            if (gold)
            {
                /* Place some gold */
                place_gold(p_ptr, cave_get(p_ptr->depth), y, x, object_level(p_ptr->depth),
                    SV_GOLD_ANY, ORIGIN_FLOOR);

                /* Message */
                msg(p_ptr, "You have found something!");
            }

            /* Found nothing */
            else
            {
                /* Message */
                msg(p_ptr, "You have finished the tunnel.");
            }
        }

        /* Failure (quartz) */
        else if (hard)
        {
            /* Message, continue digging */
            msg(p_ptr, "You tunnel into the quartz vein.");
            more = TRUE;
        }

        /* Failure (magma) */
        else
        {
            /* Message, continue digging */
            msg(p_ptr, "You tunnel into the magma vein.");
            more = TRUE;
        }
    }

    /* Rubble */
    else if (cave_isrubble(cave_get(p_ptr->depth), y, x))
    {
        /* Remove the rubble */
        if (CHANCE(p_ptr->state.skills[SKILL_DIGGING], 200) && twall(Ind, y, x))
        {
            /* Message */
            msg(p_ptr, "You have removed the rubble.");

            /* Hack -- Place an object */
            if (magik(10))
            {
                /* Create a simple object */
                place_object(p_ptr, cave_get(p_ptr->depth), y, x, object_level(p_ptr->depth), FALSE,
                    FALSE, ORIGIN_RUBBLE, 0);

                /* Observe the new object */
                if (!squelch_item_ok(p_ptr, object_byid(cave_get(p_ptr->depth)->o_idx[y][x])) &&
                    player_can_see_bold(p_ptr, y, x))
                {
                    msg(p_ptr, "You have found something!");
                }
            }
        }

        else
        {
            /* Message, keep digging */
            msg(p_ptr, "You dig in the rubble.");
            more = TRUE;
        }
    }

    /* Secret doors */
    else if (cave_issecretdoor(cave_get(p_ptr->depth), y, x))
    {
        /* Tunnel */
        if (CHANCE(p_ptr->state.skills[SKILL_DIGGING] - 30, 1200) && twall(Ind, y, x))
            msg(p_ptr, "You have finished the tunnel.");

        /* Keep trying */
        else
        {
            /* We may continue tunelling */
            msg(p_ptr, "You tunnel into the granite wall.");
            more = TRUE;

            /* Occasional Search XXX XXX */
            if (magik(25)) search(Ind, FALSE);
        }
    }

    /* Doors */
    else
    {
        /* Tunnel */
        if (CHANCE(p_ptr->state.skills[SKILL_DIGGING] - 30, 1200) && twall(Ind, y, x))
            msg(p_ptr, "You have finished the tunnel.");

        /* Keep trying */
        else
        {
            /* We may continue tunelling */
            msg(p_ptr, "You tunnel into the door.");
            more = TRUE;
        }
    }

    /* Result */
    return (more);
}


/*
 * Tunnels through "walls" (including rubble and closed doors)
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    bool more = FALSE;

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Oops */
    if (!do_cmd_tunnel_test(Ind, y, x))
    {
        /* Cancel repeat */
        disturb(p_ptr, 0, 0);
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    if (player_confuse_dir(p_ptr, &dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    /* Something in the way */
    if (cave_get(p_ptr->depth)->m_idx[y][x] != 0)
    {
        /* Attack */
        attack_blocker(Ind, y, x);
    }

    /* Walls */
    else
    {
        /* Tunnel through walls */
        more = do_cmd_tunnel_aux(Ind, y, x);
    }

    /* Cancel repetition unless we can continue */
    if (!more) disturb(p_ptr, 0, 0);
}


/*
 * Determine if a given grid may be "disarmed"
 */
static bool do_cmd_disarm_test(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Ghosts cannot disarm */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        msg(p_ptr, "You cannot disarm things!");
        return FALSE;
    }

    /* Check preventive inscription '^D' */
    __trapR(p_ptr, CPI(p_ptr, 'D'), FALSE)

    /* Must have knowledge */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK))
    {
        msg(p_ptr, "You see nothing there.");
        return FALSE;
    }

    /* Look for a closed, unlocked door to lock */
    if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_DOOR_HEAD) return TRUE;

    /* Look for a trap */
    if (!cave_isknowntrap(cave_get(p_ptr->depth), y, x))
    {
        msg(p_ptr, "You see nothing there to disarm.");
        return FALSE;
    }

    /* Okay */
    return TRUE;
}


/*
 * Perform the command "lock door"
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_lock_door(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    int i, j, power;
    bool more = FALSE;

    /* Verify legality */
    if (!do_cmd_disarm_test(Ind, y, x)) return FALSE;

    /* Get the "disarm" factor */
    i = p_ptr->state.skills[SKILL_DISARM];

    /* Penalize some conditions */
    if (p_ptr->timed[TMD_BLIND] || no_light(p_ptr)) i = i / 10;
    if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

    /* Calculate lock "power" */
    power = m_bonus(7, p_ptr->depth);

    /* Extract the difficulty */
    j = i - power;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Success */
    if (magik(j))
    {
        msg(p_ptr, "You lock the door.");
        cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_DOOR_HEAD + power);
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && !CHANCE(5, i))
    {
        msg(p_ptr, "You failed to lock the door.");

        /* We may keep trying */
        more = TRUE;
    }

    /* Failure */
    else
        msg(p_ptr, "You failed to lock the door.");

    /* Result */
    return more;
}


/*
 * Perform the basic "disarm" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_aux(int Ind, int y, int x, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int i, j, power;
    const char *name;
    bool more = FALSE;

    /* Verify legality */
    if (!do_cmd_disarm_test(Ind, y, x)) return (FALSE);

    /* Get the trap name */
    name = f_info[cave_get(p_ptr->depth)->feat[y][x]].name;

    /* Get the "disarm" factor */
    i = p_ptr->state.skills[SKILL_DISARM];

    /* Penalize some conditions */
    if (p_ptr->timed[TMD_BLIND] || no_light(p_ptr)) i = i / 10;
    if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

    /* XXX XXX XXX Variable power? */

    /* Extract trap "power" */
    power = 5;

    /* Extract the difficulty */
    j = i - power;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Success */
    if (magik(j))
    {
        /* Message */
        msgt(p_ptr, MSG_DISARM, "You have disarmed the %s.", name);

        /* Reward */
        player_exp_gain(p_ptr, power);

        /* Forget the trap */
        forget_spot(p_ptr->depth, y, x);

        /* Remove the trap */
        cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_FLOOR);
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && !CHANCE(5, i))
    {
        /* Message */
        msg(p_ptr, "You failed to disarm the %s.", name);

        /* We may keep trying */
        more = TRUE;
    }

    /* Failure -- Set off the trap */
    else
    {
        /* Message */
        msg(p_ptr, "You set off the %s!", name);

        /* Hit the trap */
        move_player(Ind, dir, FALSE, FALSE, TRUE);
    }

    /* Result */
    return (more);
}


/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(int Ind, int dir, bool easy)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    s16b o_idx;
    bool more = FALSE;

    /* Easy Disarm */
    if (easy)
    {
        int num_traps, num_chests;

        /* Count visible traps */
        num_traps = count_feats(Ind, &y, &x, cave_isknowntrap, TRUE);

        /* Count chests (trapped) */
        num_chests = count_chests(p_ptr, &y, &x, CHEST_TRAPPED);

        /* Use the last target found */
        if ((num_traps + num_chests) >= 1)
            dir = coords_to_dir(Ind, y, x);
    }

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Check for chests */
    o_idx = chest_check(p_ptr, y, x, CHEST_TRAPPED);

    /* Verify legality */
    if (!o_idx && !do_cmd_disarm_test(Ind, y, x))
    {
        /* Cancel repeat */
        disturb(p_ptr, 0, 0);
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    if (player_confuse_dir(p_ptr, &dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];

        /* Check for chests */
        o_idx = chest_check(p_ptr, y, x, CHEST_TRAPPED);
    }

    /* Something in the way */
    if (cave_get(p_ptr->depth)->m_idx[y][x] != 0)
        attack_blocker(Ind, y, x);

    /* Chest */
    else if (o_idx)
        more = do_cmd_disarm_chest(p_ptr, y, x, o_idx);

    /* Door to lock */
    else if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_DOOR_HEAD)
        more = do_cmd_lock_door(Ind, y, x);

    /* Disarm trap */
    else
        more = do_cmd_disarm_aux(Ind, y, x, dir);

    /* Cancel repeat unless told not to */
    if (!more) disturb(p_ptr, 0, 0);
}


/*
 * Determine if a given grid may be "bashed"
 */
static bool do_cmd_bash_test(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Ghosts cannot bash */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        msg(p_ptr, "You cannot bash things!");
        return FALSE;
    }

    /* Handle polymorphed players */
    if (p_ptr->r_idx && !OPT_P(p_ptr, birth_fruit_bat))
    {
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        if (!rf_has(r_ptr->flags, RF_BASH_DOOR))
        {
            msg(p_ptr, "You cannot bash things!");
            return FALSE;
        }
    }

    /* Check preventive inscription '^B' */
    __trapR(p_ptr, CPI(p_ptr, 'B'), FALSE)

    /* Must have knowledge */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK))
    {
        msg(p_ptr, "You see nothing there.");
        return FALSE;
    }

    /* Require a door */
    if (!cave_iscloseddoor(cave_get(p_ptr->depth), y, x))
    {
        msg(p_ptr, "You see nothing there to bash.");
        return FALSE;
    }

    /* Okay */
    return TRUE;
}


/*
 * Perform the basic "bash" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);
    int bash, temp;
    bool more = FALSE;

    /* Verify legality */
    if (!do_cmd_bash_test(Ind, y, x)) return FALSE;

    /* Message */
    msg(p_ptr, "You smash into the door!");

    /* Hack -- Bash power based on strength */
    /* (Ranges from 3 to 20 to 100 to 200) */
    bash = adj_str_blow[p_ptr->state.stat_ind[A_STR]];

    /* Extract door power */
    temp = ((cave_get(p_ptr->depth)->feat[y][x] - FEAT_DOOR_HEAD) & 0x07);

    /* Compare bash power to door power */
    temp = (bash - (temp * 10));

    /* Hack -- always have a chance */
    if (temp < 1) temp = 1;

    /* Hack -- attempt to bash down the door */
    if (magik(temp))
    {
        /* Break down the door */
        if (magik(50))
            cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_BROKEN);

        /* Open the door */
        else
            cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_OPEN);

        msgt(p_ptr, MSG_OPENDOOR, "The door crashes open!");

        /* Update the visuals */
        update_visuals(p_ptr->depth);
    }

    /* Saving throw against stun */
    else if (magik(adj_dex_safe[p_ptr->state.stat_ind[A_DEX]] + p_ptr->lev))
    {
        msg(p_ptr, "The door holds firm.");

        /* Allow repeated bashing */
        more = TRUE;
    }

    /* Low dexterity has bad consequences */
    else
    {
        msg(p_ptr, "You are off-balance.");

        /* Lose balance */
        player_inc_timed(p_ptr, TMD_STUN, 2 + randint0(2), TRUE, FALSE);
    }

    /* Result */
    return more;
}


/*
 * Bash open a door, success based on character strength
 *
 * For a closed door, pval is positive if locked; negative if stuck.
 *
 * For an open door, pval is positive for a broken door.
 *
 * A closed door can be opened - harder if locked. Any door might be
 * bashed open (and thereby broken). Bashing a door is (potentially)
 * faster! You move into the door way. To open a stuck door, it must
 * be bashed. A closed door can be jammed (see do_cmd_spike()).
 *
 * Creatures can also open or bash doors, see elsewhere.
 */
void do_cmd_bash(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    bool more = FALSE;

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!do_cmd_bash_test(Ind, y, x))
    {
        /* Cancel repeat */
        disturb(p_ptr, 0, 0);
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    if (player_confuse_dir(p_ptr, &dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    /* Something in the way */
    if (cave_get(p_ptr->depth)->m_idx[y][x] != 0)
    {
        /* Attack */
        attack_blocker(Ind, y, x);
    }

    /* Door */
    else
    {
        /* Bash the door */
        more = do_cmd_bash_aux(Ind, y, x);
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(p_ptr, 0, 0);
}     


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 *
 * The "semantics" of this command must be chosen before the player
 * is confused, and it must be verified against the new grid.
 */
void do_cmd_alter(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    bool more = FALSE;
    struct cave *c = cave_get(p_ptr->depth);

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Check preventive inscription '^+' */
    __trap(p_ptr, CPI(p_ptr, '+'))

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    if (player_confuse_dir(p_ptr, &dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    /* Something in the way */
    if (c->m_idx[y][x] != 0)
    {
        /* Attack */
        attack_blocker(Ind, y, x);
    }

    /* MAngband-specific: Open walls (House Creation) */
    else if (c->feat[y][x] == FEAT_PERM_EXTRA)
        more = do_cmd_open_aux(Ind, y, x);

    /* Tunnel through walls, trees and rubble (allow pit walls to fool the player) */
    else if (cave_isdiggable(c, y, x) || cave_istree(c, y, x) || (c->feat[y][x] == FEAT_PERM_FAKE))
        more = do_cmd_tunnel_aux(Ind, y, x);

    /* Open closed doors */
    else if (cave_iscloseddoor(c, y, x) || cave_ishomedoor(c, y, x))
        more = do_cmd_open_aux(Ind, y, x);

    /* Disarm traps */
    else if (cave_isknowntrap(c, y, x))
        more = do_cmd_disarm_aux(Ind, y, x, dir);

    /* Oops */
    else
        msg(p_ptr, "You spin around.");

    /* Cancel repetition unless we can continue */
    if (!more) disturb(p_ptr, 0, 0);
}


/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
static bool get_spike(int Ind, int *ip)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Check every item in the pack */
    for (i = 0; i < INVEN_PACK; i++)
    {
        object_type *o_ptr = &p_ptr->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Check the "tval" code */
        if (o_ptr->tval == TV_SPIKE)
        {
            /* Save the spike index */
            (*ip) = i;

            /* Success */
            return (TRUE);
        }
    }

    /* Oops */
    return (FALSE);
}


/*
 * Determine if a given grid may be "spiked"
 */
static bool do_cmd_spike_test(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Ghosts cannot spike */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_HANDS))
    {
        /* Message */
        msg(p_ptr, "You cannot spike doors!");

        /* Nope */
        return (FALSE);
    }

    /* Check preventive inscription '^j' */
    __trapR(p_ptr, CPI(p_ptr, 'j'), FALSE)

    /* Must have knowledge */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK))
    {
        msg(p_ptr, "You see nothing there.");
        return FALSE;
    }

    /* Check if door is closed */
    if (!cave_iscloseddoor(cave_get(p_ptr->depth), y, x))
    {
        msg(p_ptr, "You see nothing there to spike.");
        return FALSE;
    }

    /* Check that the door is not fully spiked */
    if (!(cave_get(p_ptr->depth)->feat[y][x] < FEAT_DOOR_TAIL))
    {
        msg(p_ptr, "You can't use more spikes on this door.");
        return FALSE;
    }

    /* Okay */
    return TRUE;
}


/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int y, x, item;

    /* Get a spike */
    if (!get_spike(Ind, &item))
    {
        /* Message */
        msg(p_ptr, "You have no spikes!");

        /* Done */
        return;
    }

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!do_cmd_spike_test(Ind, y, x)) return;

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    if (player_confuse_dir(p_ptr, &dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    /* Something in the way */
    if (cave_get(p_ptr->depth)->m_idx[y][x] != 0)
    {
        /* Attack */
        attack_blocker(Ind, y, x);
    }

    /* Go for it */
    else
    {
        /* Verify legality */
        if (!do_cmd_spike_test(Ind, y, x)) return;

        /* Successful jamming */
        msg(p_ptr, "You jam the door with a spike.");

        /* Convert "locked" to "stuck" XXX XXX XXX */
        if (cave_get(p_ptr->depth)->feat[y][x] < FEAT_DOOR_HEAD + 0x08)
            cave_get(p_ptr->depth)->feat[y][x] += 0x08;

        /* Add one spike to the door */
        if (cave_get(p_ptr->depth)->feat[y][x] < FEAT_DOOR_TAIL)
            cave_get(p_ptr->depth)->feat[y][x]++;

        /* Use up, and describe, a single spike, from the bottom */
        inven_item_increase(p_ptr, item, -1);
        inven_item_describe(p_ptr, item);
        inven_item_optimize(p_ptr, item);
    }
}


/*
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Check preventive inscription '^;' */
    __trapR(p_ptr, CPI(p_ptr, ';'), FALSE)

    /* Tests are done in move_player() */

    /* Okay */
    return (TRUE);
}


/*
 * Apply erratic movement, if needed, to a direction
 *
 * Return TRUE if direction changes.
 */
static bool erratic_dir(int Ind, int *dp)
{
    player_type *p_ptr = player_get(Ind);
    int dir;
    int erratic = 0;

    /* Default */
    dir = (*dp);

    /* Handle polymorphed players */
    if (p_ptr->r_idx)
    {
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        if (rf_has(r_ptr->flags, RF_RAND_25)) erratic += 10;
        if (rf_has(r_ptr->flags, RF_RAND_50)) erratic += 20;
    }

    /* Apply "erratic movement" */
    if (magik(erratic))
    {
        /* Random direction */
        dir = ddd[randint0(8)];
    }

    /* Notice erratic movement */
    if ((*dp) != dir)
    {
        /* Save direction */
        (*dp) = dir;

        /* Erratic movement */
        return (TRUE);
    }

    /* Normal movement */
    return (FALSE);
}


/*
 * Walk in the given direction.
 */
void do_cmd_walk(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);

    /* Get a direction (or abort) */
    if (!dir) return;

    /* Verify legality */
    if (!do_cmd_walk_test(Ind)) return;

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion/erratic movement */
    player_confuse_dir(p_ptr, &dir);
    erratic_dir(Ind, &dir);

    /* Move the player */
    move_player(Ind, dir, TRUE, TRUE, FALSE);
}


/*
 * Walk into a trap
 */
void do_cmd_jump(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);

    /* Get a direction (or abort) */
    if (!dir) return;

    /* Verify legality */
    if (!do_cmd_walk_test(Ind)) return;

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion/erratic movement */
    player_confuse_dir(p_ptr, &dir);
    erratic_dir(Ind, &dir);

    /* Move the player */
    move_player(Ind, dir, FALSE, TRUE, FALSE);
}


/*
 * Determine if a given grid may be "run"
 */
static bool do_cmd_run_test(int Ind, int y, int x)
{
    player_type *p_ptr = player_get(Ind);

    /* Check preventive inscription '^.' */
    __trapR(p_ptr, CPI(p_ptr, '.'), FALSE)

    /* Ghosts run right through everything */
    if (player_passwall(p_ptr)) return (TRUE);

    /* Do wilderness hack, keep running from one outside level to another */
    if (!in_bounds_fully(y, x) && (p_ptr->depth <= 0)) return (TRUE);

    /* Illegal grids are not known walls XXX XXX XXX */
    if (!in_bounds(y, x)) return (TRUE);

    /* Hack -- walking obtains knowledge XXX XXX */
    if (!(p_ptr->cave->info[y][x] & CAVE_MARK)) return (TRUE);

    /* Require open space */
    if (!cave_floor_bold(p_ptr->depth, y, x))
    {
        /* Rubble */
        if (cave_isrubble(cave_get(p_ptr->depth), y, x))
            msgt(p_ptr, MSG_HITWALL, "There is a pile of rubble in the way!");

        /* Door */
        else if (cave_isbasicdoor(cave_get(p_ptr->depth), y, x))
            return (TRUE);

        /* Tree */
        else if (cave_tree_basic(cave_get(p_ptr->depth)->feat[y][x]))
            msgt(p_ptr, MSG_HITWALL, "There is a tree in the way!");

        /* Wall */
        else
            msgt(p_ptr, MSG_HITWALL, "There is a wall in the way!");

        /* Cancel repeat */
        disturb(p_ptr, 0, 0);

        /* Nope */
        return (FALSE);
    }

    /* Okay */
    return (TRUE);
}


/*
 * Start running.
 *
 * Note that running while confused is not allowed
 */
void do_cmd_run(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;

    /* Not while confused */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        msg(p_ptr, "You are too confused!");
        return;
    }

    /* Handle polymorphed players */
    if (p_ptr->r_idx)
    {
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        if (rf_has(r_ptr->flags, RF_RAND_25) || rf_has(r_ptr->flags, RF_RAND_50))
        {
            msg(p_ptr, "Your nature prevents you from running straight.");
            return;
        }
    }

    /* Ignore if we are already running in this direction */
    if (p_ptr->running && (dir == p_ptr->run_cur_dir)) return;

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!do_cmd_run_test(Ind, y, x)) return;

    /* Start run */
    p_ptr->run_request = dir;
    p_ptr->running = TRUE;
}


/*
 * Pick up objects on the floor beneath you.
 */
void do_cmd_pickup(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Pick up floor objects, forcing a menu for multiple objects. */
    current_clear(p_ptr);
    py_pickup(Ind, 2);
}


/*
 * Pick up objects on the floor beneath you.
 */
void do_cmd_autopickup(int Ind)
{
    do_autopickup(Ind, 2);
}


/*
 * Rest (restores hit points and mana and such)
 */
bool do_cmd_rest(int Ind, s16b resting)
{
    player_type *p_ptr = player_get(Ind);

    /*
     * A little sanity checking on the input - only the specified negative
     * values are valid.
     */
    if ((resting < 0) && ((resting != REST_COMPLETE) && (resting != REST_ALL_POINTS) &&
        (resting != REST_SOME_POINTS)))
            return TRUE;

    /* Truncate overlarge values */
    if (resting > 9999) resting = 9999;

    /* Cancel the rest if already resting (costs no energy) */
    if (p_ptr->resting)
    {
        disturb(p_ptr, 0, 0);
        return TRUE;
    }

    /* Check energy */
    if (!has_energy(Ind)) return FALSE;

    /* Disturb us: reset resting, searching, running if needed */
    disturb(p_ptr, 1, 0);

    /* Set resting counter */
    p_ptr->resting = resting;

    /* Take a turn */
    use_energy(Ind);

    /* Redraw the state */
    p_ptr->redraw |= (PR_STATE);

    return TRUE;
}


/*
 * Hack -- Commit suicide
 */
void do_cmd_suicide(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Mark as suicide */
    p_ptr->alive = FALSE;

    /* Hack -- Set the cause of death */
    if (!p_ptr->ghost)
    {
        /* Set the cause of death */
        player_death_info(p_ptr, "self-inflicted wounds");

        /* Mark as quitter */
        if (!p_ptr->total_winner) p_ptr->noscore = 1;
    }

    /* Hack -- Clear ghost */
    set_ghost_flag(p_ptr, 0, TRUE);

    if (p_ptr->total_winner) kingly(Ind);

    /* Kill him */
    player_death(Ind);
}


static void set_house_owner(struct player *p, int house)
{
    my_strcpy(houses[house].ownername, p->name, sizeof(houses[0].ownername));
    houses[house].ownerid = p->id;
    houses[house].color = PLAYER_STORE_BM;
}


/*
 * Buy a house. It is assumed that the player already knows the price.
 * Hacked to sell houses for half price.
 */
void do_cmd_purchase_house(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    int y, x, i, j, factor, n;
    s32b price;

    /* Ghosts cannot buy houses */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_HOUSE_CONTROL))
    {
        /* Message */
        msg(p_ptr, "You cannot buy a house.");
        return;
    }

    /* Restricted by choice */
    if (OPT_P(p_ptr, birth_no_stores))
    {
        /* Message */
        msg(p_ptr, "You cannot buy a house.");
        return;
    }

    /* Check preventive inscription '^h' */
    __trap(p_ptr, CPI(p_ptr, 'h'))

    /* Check for no-direction -- Confirmation (when selling house) */
    if (!dir)
    {
        i = p_ptr->current_house;
        p_ptr->current_house = -1;

        if (i == -1)
        {
            /* No house, message */
            msg(p_ptr, "You see nothing to sell there.");
            return;
        }

        /* Take player's CHR into account */
        factor = adj_chr_gold[p_ptr->state.stat_ind[A_CHR]];
        price = (s32b)((double)houses[i].price * factor / 100);

        /* Check for already-owned house */
        if (houses[i].ownerid > 0)
        {
            /* See if he owns the house */
            if (house_owned_by(p_ptr, i))
            {
                /* House is no longer owned */
                reset_house(i, p_ptr->depth, houses[i].door_y, houses[i].door_x);
                msg(p_ptr, "You sell your house for %ld gold.", price / 2);

                /* Get the money */
                p_ptr->au += price / 2;

                /* Redraw */
                p_ptr->redraw |= (PR_INVEN | PR_GOLD);

                /* Done */
                return;
            }

            /* The DM can reset a house */
            if (p_ptr->dm_flags & DM_HOUSE_CONTROL)
            {
                /* House is no longer owned */
                reset_house(i, p_ptr->depth, houses[i].door_y, houses[i].door_x);
                msg(p_ptr, "The house has been reset.");

                /* Done */
                return;
            }
        }

        /* Message */
        msg(p_ptr, "You don't own this house.");

        /* No sale */
        return;
    }

    /* Be sure we have a direction */
    if (VALID_DIR(dir))
    {
        /* Get requested direction */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];

        /* Check for a house */
        if ((i = pick_house(p_ptr->depth, y, x)) == -1)
        {
            /* No house, message */
            msg(p_ptr, "You see nothing to buy there.");
            return;
        }

        /* The door needs to be closed! */
        if (cave_get(p_ptr->depth)->feat[y][x] == FEAT_HOME_OPEN)
        {
            /* Open door, message */
            msg(p_ptr, "You need to close the door first...");
            return;
        }

        /* Not inside the house! */
        if (house_inside(p_ptr, i))
        {
            /* Instead we allow players to look inside their own stores */
            if (house_owned_by(p_ptr, i))
                do_cmd_store(Ind, i);
            else
                msg(p_ptr, "You need to stand outside of the house first...");
            return;
        }

        /* Take player's CHR into account */
        factor = adj_chr_gold[p_ptr->state.stat_ind[A_CHR]];
        price = (s32b)((double)houses[i].price * factor / 100);

        /* Check for already-owned house */
        if (houses[i].ownerid > 0)
        {
            /* See if he owns the house */
            if (house_owned_by(p_ptr, i))
            {
                /* Delay house transaction */
                p_ptr->current_house = i;

                /* Tell the client about the price */
                Send_store_sell(Ind, price / 2, FALSE);

                /* Done */
                return;
            }

            /* The DM can reset a house */
            if (p_ptr->dm_flags & DM_HOUSE_CONTROL)
            {
                /* Delay house transaction */
                p_ptr->current_house = i;

                /* Tell the client about the reset */
                Send_store_sell(Ind, 0, TRUE);

                /* Done */
                return;
            }

            /* Message */
            msg(p_ptr, "That house is already owned.");

            /* No sale */
            return;
        }

        /* The DM cannot buy houses! */
        if (p_ptr->dm_flags & DM_HOUSE_CONTROL)
        {
            /* Message */
            msg(p_ptr, "You cannot buy a house.");
            return;
        }

        /* Check for enough funds */
        if (price > p_ptr->au)
        {
            /* Not enough money, message */
            msg(p_ptr, "You do not have enough money.");
            return;
        }

        /* Check already owned houses */
        n = 0;
        for (j = 0; j < num_houses; j++)
        {
            if (house_owned_by(p_ptr, j)) n++;
        }

        /* Can we buy a house? */
        if (n == (p_ptr->total_winner? 10: p_ptr->max_lev / 10 + 1))
        {
            /* Too many houses, message */
            msg(p_ptr, "You cannot buy more houses.");
            return;
        }
        if (cfg_max_houses && (n == cfg_max_houses))
        {
            /* Too many houses, message */
            msg(p_ptr, "You cannot buy more houses.");
            return;
        }

        /* Open the door */
        cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_HOME_OPEN);

        /* Take some of the player's money */
        p_ptr->au -= price;

        /* The house is now owned */
        set_house_owner(p_ptr, i);

        /* Redraw */
        p_ptr->redraw |= (PR_GOLD);
    }
}


/*
 * Get a quest
 */
void do_cmd_quest(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    monster_race *r_ptr;
    int min_depth = ((p_ptr->max_depth < 6)? 1: (p_ptr->max_depth - 5));
    int max_depth = ((p_ptr->max_depth > 95)? 100: (p_ptr->max_depth + 5));
    int r_idx, max_num;

    /* Quest already taken? */
    if (p_ptr->quest.r_idx)
    {
        r_ptr = &r_info[p_ptr->quest.r_idx];
        msg(p_ptr, "You still need to kill %d of the %s race!",
            p_ptr->quest.max_num - p_ptr->quest.cur_num, r_ptr->name);
        return;
    }

    /* Get a quest - try to roughly match player max depth */
    /* Skip uniques and monsters that can't be generated */
    do
    {
        r_idx = randint1(z_info->r_max - 1);
        r_ptr = &r_info[r_idx];
    }
    while ((r_ptr->level < min_depth) || (r_ptr->level > max_depth) ||
        rf_has(r_ptr->flags, RF_UNIQUE) ||
        (rf_has(r_ptr->flags, RF_PWMANG_BASE) && !cfg_base_monsters) ||
        (rf_has(r_ptr->flags, RF_PWMANG_EXTRA) && !cfg_extra_monsters));

    /* Monster race */
    p_ptr->quest.r_idx = r_idx;

    /* Set a base required number */
    max_num = 2 + randint1(5);

    /* Increase it for groups */
    if (rf_has(r_ptr->flags, RF_FRIEND)) max_num = max_num + 2 + randint1(5);
    if (rf_has(r_ptr->flags, RF_FRIENDS)) max_num = max_num + 11 + randint1(7);

    /* Number required */
    p_ptr->quest.max_num = max_num;

    /* Set the timer */
    p_ptr->quest.timer = 10000;

    msg(p_ptr, "Find%s and kill %d of the %s race!",
        (rf_has(r_ptr->flags, RF_FRIENDS)? " some groups": ""),
        p_ptr->quest.max_num, r_ptr->name);
}


/*
 * Given coordinates return a house to which they belong.
 * Houses can be overlapping, so a single coordinate pair may match several
 * houses. The offset parameter allows searching for the next match.
 */
static int find_house(struct player *p, int x, int y, int offset)
{
    int i;

    for (i = offset; i < num_houses; i++)
    {
        /* Check the house position *including* the walls */
        if ((houses[i].depth == p->depth) &&
            (x >= houses[i].x_1 - 1) && (x <= houses[i].x_2 + 1) &&
            (y >= houses[i].y_1 - 1) && (y <= houses[i].y_2 + 1))
        {
            /* We found the house this section of wall belongs to */
            return i;
        }
    }
    return -1;
}


/*  
 * Determine if the given location is ok to use as part of the foundation
 * of a house.
 */
static bool is_valid_foundation(struct player *p, int x, int y)
{
    /* Foundation stones are always valid */
    if (cave_get(p->depth)->o_idx[y][x])
    {
        object_type *o_ptr = object_byid(cave_get(p->depth)->o_idx[y][x]);

        if ((o_ptr->tval == TV_STONE) && !o_ptr->next_o_idx) return TRUE;
        return FALSE;
    }

    /*
     * Perma walls and doors are valid if they are part of a house owned
     * by this player
     */
    if ((cave_get(p->depth)->feat[y][x] == FEAT_PERM_EXTRA) ||
        cave_ishomedoor(cave_get(p->depth), y, x))
    {
        int house;

        /* Looks like part of a house, which house? */
        house = find_house(p, x, y, 0);
        if (house >= 0)
        {
            /* Do we own this house? */
            if (house_owned_by(p, house))
            {
                /* Valid, a wall or door in our own house. */
                return TRUE;
            }
        }
    }

    return FALSE;
}


/*  
 * Create a new house door at the given location.
 * Due to the fact that houses can overlap (i.e. share a common wall) it
 * may not be possible to identify the house to which the door should belong.
 *
 * For example, on the left below, we have two houses overlapping, neither
 * have doors. On the right the player creates a door, but to which house does
 * it belong?
 *
 *   ####                        ####
 *   #  #@                       #  #@
 *   #  ###                      #  +##
 *   #### #                      #### #
 *      # #                         # #
 *      ###                         ###
 *
 * It is therefore possible to create a complex of houses such that the player
 * owned shop mechanism becomes confused.  When a player bumps one door they
 * see the contents of a different room listed.
 *
 * FIXME Therefore the player owned shop mechanism should treat overlapping
 * player created houses as a *single* house and present all goods in all
 * attached houses.
 */
static bool create_house_door(int Ind, int x, int y)
{
    player_type *p_ptr = player_get(Ind);
    int house, lastmatch;

    /* Which house is the given location part of? */
    lastmatch = 0;
    house = find_house(p_ptr, x, y, lastmatch);
    while (house >= 0)
    {
        /* Do we own this house? */
        if (!house_owned_by(p_ptr, house))
        {
            /* If we don't own this one, we can't own any overlapping ones */
            msg(p_ptr, "You do not own this house.");

            return FALSE;
        }

        /* Does it already have a door? */
        if (!houses[house].door_y && !houses[house].door_x)
        {
            /* No door, so create one! */
            houses[house].door_y = y;
            houses[house].door_x = x;
            cave_set_feat(cave_get(p_ptr->depth), y, x, FEAT_HOME_HEAD);
            msg(p_ptr, "You create a door for your house!");

            return TRUE;
        }

        /* Check next house */
        lastmatch = house + 1;
        house = find_house(p_ptr, x, y, lastmatch);
    }

    /* We searched all matching houses and none needed a door */
    msg(p_ptr, "You can't do that here.");

    return FALSE;
}


/*  
 * Determine the area for a house foundation.
 *
 * Although an individual house must be rectangular, a foundation
 * can be non-rectangular.  This is because we allow existing walls to
 * form part of our foundation, and therefore allow complex shaped houses
 * to be constructed.
 *                                              ~~~
 * For example this is a legal foundation:   ~~~~~~
 *                                           ~~~~~~
 * In this sitation:
 *
 *   #####                               #####
 *   #   #                               #   #
 *   #   #      Forming a final shape:   #   #
 *   #####~~~                            ###+####
 *     ~~~~~~                              #    #
 *     ~~~~~~                              ######
 *
 * This function is also responsible for rejecting illegal shapes and sizes.
 *
 * We start from the player location (who must be stood on a foundation stone)
 * and work our way outwards to find the bounding rectange of the foundation.
 * Conceptually imagine a box radiating out from the player, we keep extending
 * the box in each dimension for as long as all points on the perimeter are
 * either foundation stones or walls of houses the player owns.
 *
 */
static bool get_house_foundation(struct player *p, int *px1, int *py1, int *px2, int *py2)
{
    int x, y, x1, y1, x2, y2;
    bool done = FALSE;
    bool n, s, e, w, ne, nw, se, sw;

    /* We must be standing on a house foundation */
    if (!is_valid_foundation(p, p->px, p->py))
    {
        msg(p, "There is no house foundation here.");
        return FALSE;
    }

    /* Start from the players position */
    x1 = p->px;
    x2 = p->px;
    y1 = p->py;
    y2 = p->py;

    while (!done)
    {
        n = s = e = w = ne = nw = se = sw = FALSE;

        /* Could we expand north? */
        n = TRUE;
        for (x = x1; x <= x2; x++)
        {
            /* Is this a valid location for part of our house? */
            if (!is_valid_foundation(p, x, y1 - 1))
            {
                /* Not a valid perimeter */
                n = FALSE;

                break;
            }
        }

        /* Could we expand east? */
        e = TRUE;
        for (y = y1; y <= y2; y++)
        {
            /* Is this a valid location for part of our house? */
            if (!is_valid_foundation(p, x2 + 1, y))
            {
                /* Not a valid perimeter */
                e = FALSE;

                break;
            }
        }

        /* Could we expand south? */
        s = TRUE;
        for (x = x1; x <= x2; x++)
        {
            /* Is this a valid location for part of our house? */
            if (!is_valid_foundation(p, x, y2 + 1))
            {
                /* Not a valid perimeter */
                s = FALSE;

                break;
            }
        }

        /* Could we expand west? */
        w = TRUE;
        for (y = y1; y <= y2; y++)
        {
            /* Is this a valid location for part of our house? */
            if (!is_valid_foundation(p, x1 - 1, y))
            {
                /* Not a valid perimeter */
                w = FALSE;

                break;
            }
        }

        /* Could we expand the corners? */
        ne = is_valid_foundation(p, x2 + 1, y1 - 1);
        nw = is_valid_foundation(p, x1 - 1, y1 - 1);
        se = is_valid_foundation(p, x2 + 1, y2 + 1);
        sw = is_valid_foundation(p, x1 - 1, y2 + 1);

        /*
         * Only permit expansion in a way that maintains a rectangle, we don't
         * want to create fancy polygons.
         */
        if (n) n = (!e && !w) || (e && ne) || (w && nw);
        if (e) e = (!n && !s) || (n && ne) || (s && se);
        if (s) s = (!e && !w) || (e && se) || (w && sw);
        if (w) w = (!n && !s) || (n && nw) || (s && sw);

        /* Actually expand the boundary */
        if (n) y1--;
        if (s) y2++;
        if (w) x1--;
        if (e) x2++;

        /* Stop if we couldn't expand */
        done = !(n || s || w || e);
    }

    /* Paranoia */
    x = x2 - x1 - 1;
    y = y2 - y1 - 1;
    if ((x <= 0) || (y <= 0))
    {
        msg(p, "The foundation should have positive dimensions!");
        return FALSE;
    }

    /* No 1x1 house foundation */
    if ((x + y) < 3)
    {
        msg(p, "The foundation is too small!");
        return FALSE;
    }

    /* Return the area */
    *px1 = x1;
    *px2 = x2;
    *py1 = y1;
    *py2 = y2;

    return TRUE;
}


/*
 * Create a new house.
 * The creating player owns the house.
 */
bool create_house(struct player *p)
{
    int x1, x2, y1, y2, x, y;

    /* Houses can only be created in the wilderness */
    if (p->depth >= 0)
    {
        msg(p, "This location is not suited for a house.");
        return FALSE;
    }

    /* Check maximum number of houses */
    if (num_houses == MAX_HOUSES)
    {
        msg(p, "The maximum number of houses has been reached.");
        return FALSE;
    }

    /* Determine the area of the house foundation */
    if (!get_house_foundation(p, &x1, &y1, &x2, &y2)) return FALSE;

    /* Is the location allowed? */
    /* XXX We should check if too near other houses, roads, level edges, etc */

    /* Add a house to our houses list */
    houses[num_houses].price = 0;   /* XXX */
    houses[num_houses].x_1 = x1 + 1;
    houses[num_houses].y_1 = y1 + 1;
    houses[num_houses].x_2 = x2 - 1;
    houses[num_houses].y_2 = y2 - 1;
    houses[num_houses].depth = p->depth;
    houses[num_houses].door_y = 0;
    houses[num_houses].door_x = 0;
    set_house_owner(p, num_houses);
    num_houses++;

    /* Render into the terrain */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Delete any object */
            delete_object(p->depth, y, x);

            /* Build a wall, but don't destroy any existing door */
            if (!cave_ishomedoor(cave_get(p->depth), y, x))
                cave_set_feat(cave_get(p->depth), y, x, FEAT_PERM_EXTRA);
        }
    }
    for (y = y1 + 1; y < y2; y++)
    {
        for (x = x1 + 1; x < x2; x++)
        {
            /* Fill with safe floor */
            cave_set_feat(cave_get(p->depth), y, x, FEAT_FLOOR_SAFE);

            /* Make it "icky" */
            cave_get(p->depth)->info[y][x] |= CAVE_ICKY;
        }
    }

    return TRUE;
}


/*
 * Return the index of a house given a coordinate pair
 */
int pick_house(int depth, int y, int x)
{
    int i;

    /* Check each house */
    for (i = 0; i < num_houses; i++)
    {
        /* Check this one */
        if ((houses[i].door_x == x) && (houses[i].door_y == y) &&
            (houses[i].depth == depth))
        {
            /* Return */
            return i;
        }
    }

    /* Failure */
    return -1;
}


/*
 * "Cutting" bonus based on weapon efficiency against trees
 */
int wielding_cut(struct player *p)
{
    object_type *o_ptr = &p->inventory[INVEN_WIELD];

    /* Skip empty weapon slot */
    if (!o_ptr->kind) return 0;

    /* Weapon type */
    switch (o_ptr->tval)
    {
        /* Polearms and axes */
        case TV_POLEARM:
        {
            /* Weapon type */
            switch (o_ptr->sval)
            {
                /* Scythes of slicing */
                case SV_SCYTHE_OF_SLICING: return 4;

                /* Scythes */
                case SV_SCYTHE: return 3;

                /* Axes */
                case SV_BEAKED_AXE:
                case SV_BROAD_AXE:
                case SV_BATTLE_AXE:
                case SV_GREAT_AXE:
                case SV_LOCHABER_AXE: return 2;

                /* Polearms */
                default: return 0;
            }
        }

        /* Swords */
        case TV_SWORD: return 1;

        /* Hafted and other weapons */
        default: return 0;
    }
}
