/*
 * File: cmd-cave.c
 * Purpose: Chest and door opening/closing, disarming, running, resting, ...
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


/*
 * Go up one level
 */
void do_cmd_go_up(struct player *p)
{
    int ascend_to;
    byte new_level_method;
    struct chunk *c = chunk_get(&p->wpos);
    struct worldpos wpos;

    /* Can't go up on the surface */
    if (p->wpos.depth == 0)
    {
        msg(p, "There is nothing above you.");
        return;
    }

    /* Check preventive inscription '^<' */
    if (check_prevent_inscription(p, INSCRIPTION_UP))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Verify stairs */
    if (!square_isupstairs(c, &p->grid) && !p->ghost && !p->timed[TMD_PROBTRAVEL])
    {
        msg(p, "I see no up staircase here.");
        return;
    }

    /* Force down */
    if ((cfg_limit_stairs >= 2) || OPT(p, birth_force_descend))
    {
        /* Going up is forbidden (except ghosts) */
        if (!p->ghost)
        {
            msg(p, "Morgoth awaits you in the darkness below.");
            return;
        }
    }

    /* No going up from quest levels with force_descend while the quest is active */
    if (((cfg_limit_stairs == 3) || OPT(p, birth_force_descend)) &&
        is_quest_active(p, p->wpos.depth))
    {
        msg(p, "Something prevents you from going up...");
        return;
    }

    ascend_to = dungeon_get_next_level(p, p->wpos.depth, -1);

    wpos_init(&wpos, &p->wpos.grid, ascend_to);

    /* Hack -- DM redesigning the level */
    if (chunk_inhibit_players(&wpos))
    {
        msg(p, "Something prevents you from going up...");
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Success */
    if (square_isupstairs(c, &p->grid))
    {
        msgt(p, MSG_STAIRS_UP, "You enter a maze of up staircases.");
        new_level_method = LEVEL_UP;
    }
    else
    {
        msg(p, "You float upwards.");
        new_level_method = LEVEL_GHOST;
    }

    /* Change level */
    dungeon_change_level(p, c, &wpos, new_level_method);
}


/*
 * Go down one level
 */
void do_cmd_go_down(struct player *p)
{
    int descend_to;
    byte new_level_method;
    struct chunk *c = chunk_get(&p->wpos);
    struct wild_type *w_ptr = get_wt_info_at(&p->wpos.grid);
    struct worldpos wpos;

    /* Can't go down if no dungeon */
    if (w_ptr->max_depth == 1)
    {
        msg(p, "There is nothing below you.");
        return;
    }

    /* Ghosts who are not dungeon masters can't go down if ghost_diving is off */
    if (p->ghost && !cfg_ghost_diving && !is_dm_p(p))
    {
        msg(p, "You seem unable to go down. Try going up.");
        return;
    }

    /* Check preventive inscription '^>' */
    if (check_prevent_inscription(p, INSCRIPTION_DOWN))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Verify stairs */
    if (!square_isdownstairs(c, &p->grid) && !p->ghost && !p->timed[TMD_PROBTRAVEL])
    {
        msg(p, "I see no down staircase here.");
        return;
    }

    /* Paranoia, no descent from max_depth - 1 */
    if (p->wpos.depth == w_ptr->max_depth - 1)
    {
        msg(p, "The dungeon does not appear to extend deeper.");
        return;
    }

    /* Verify basic quests */
    if (is_quest_active(p, p->wpos.depth))
    {
        msg(p, "Something prevents you from going down...");
        return;
    }

    /* Winner-only dungeons */
    if (forbid_entrance_weak(p))
    {
        msg(p, "You're not powerful enough to enter!");
        return;
    }

    /* Shallow dungeons */
    if (forbid_entrance_strong(p))
    {
        msg(p, "You're too powerful to enter!");
        return;
    }

    descend_to = dungeon_get_next_level(p, p->wpos.depth, 1);

    /* Warn a force_descend player if they're going to a quest level */
    if ((cfg_limit_stairs == 3) || OPT(p, birth_force_descend))
    {
        descend_to = dungeon_get_next_level(p, p->max_depth, 1);

        /* Ask for confirmation */
        if (is_quest_active(p, descend_to) && !p->current_action)
        {
            p->current_action = ACTION_GO_DOWN;
            get_item(p, HOOK_DOWN, "");
            return;
        }
    }

    wpos_init(&wpos, &p->wpos.grid, descend_to);

    /* Hack -- DM redesigning the level */
    if (chunk_inhibit_players(&wpos))
    {
        msg(p, "Something prevents you from going down...");
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Success */
    if (square_isdownstairs(c, &p->grid))
    {
        msgt(p, MSG_STAIRS_DOWN, "You enter a maze of down staircases.");
        new_level_method = LEVEL_DOWN;
    }
    else
    {
        msg(p, "You float downwards.");
        new_level_method = LEVEL_GHOST;
    }

    /* Change level */
    dungeon_change_level(p, c, &wpos, new_level_method);
}


/*
 * Toggle stealth mode
 */
void do_cmd_toggle_stealth(struct player *p)
{
    /* Stop stealth mode */
    if (p->stealthy)
    {
        p->stealthy = false;
        p->upkeep->update |= (PU_BONUS);
        p->upkeep->redraw |= (PR_STATE);
    }

    /* Start stealth mode */
    else
    {
        p->stealthy = true;
        p->upkeep->update |= (PU_BONUS);
        p->upkeep->redraw |= (PR_STATE | PR_SPEED);
    }
}


/*
 * Determine if a given grid may be "opened"
 */
static bool do_cmd_open_test(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Ghosts cannot open things */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS))
    {
        msg(p, "You cannot open things!");
        return false;
    }

    /* Handle polymorphed players */
    if (p->poly_race && !OPT(p, birth_fruit_bat))
    {
        if (!rf_has(p->poly_race->flags, RF_OPEN_DOOR))
        {
            msg(p, "You cannot open things!");
            return false;
        }
    }

    /* Must have knowledge */
    if (!square_isknown(p, grid))
    {
        msg(p, "You see nothing there.");
        return false;
    }

    /* Must be a closed door */
    if (!square_iscloseddoor(c, grid))
    {
        msgt(p, MSG_NOTHING_TO_OPEN, "You see nothing there to open.");
        return false;
    }

    return true;
}


/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns true if repeated commands may continue
 */
static bool do_cmd_open_aux(struct player *p, struct chunk *c, struct loc *grid)
{
    int i, j, k;
    bool more = false;
    struct house_type *house;

    /* Verify legality */
    if (!do_cmd_open_test(p, c, grid)) return false;

    /* Player Houses */
    if (square_home_iscloseddoor(c, grid))
    {
        i = pick_house(&p->wpos, grid);
        house = house_get(i);

        /* Tell the DM who owns the house */
        if (p->dm_flags & DM_HOUSE_CONTROL)
        {
            /* Message */
            if (house->ownerid > 0)
                msg(p, "This house belongs to %s.", house->ownername);
            else
                msg(p, "This house is not owned.");
        }

        /* Do we own this house? */
        else if (house_owned_by(p, i))
        {
            /* If someone is in our store, we eject them (anti-exploit) */
            for (k = 1; k <= NumPlayers; k++)
            {
                struct player *q = player_get(k);

                if (p == q) continue;

                /* Eject any shopper */
                if (q->player_store_num == i)
                {
                    struct store *s = store_at(q);

                    if (s && (s->type == STORE_PLAYER))
                    {
                        msg(q, "The shopkeeper locks the doors.");
                        Send_store_leave(q);
                    }
                }
            }

            /* Open the door */
            square_open_homedoor(c, grid);

            /* Update the visuals */
            square_memorize(p, c, grid);
            square_light_spot_aux(p, c, grid);
            update_visuals(&p->wpos);

            /* Sound */
            sound(p, MSG_STORE_HOME);
        }

        /* He's not the owner, check if owned */
        else if (house->ownerid > 0)
        {
            /* Player owned store! */

            /* Disturb */
            disturb(p, 0);

            /* Hack -- enter store */
            do_cmd_store(p, i);
        }

        /* Not owned */
        else
        {
            /* Tell him the price */
            msg(p, "This house costs %ld gold.", house->price);
        }
    }

    /* Locked door */
    else if (square_islockeddoor(c, grid))
    {
        /* Disarm factor */
        i = p->state.skills[SKILL_DISARM_PHYS];

        /* Penalize some conditions */
        if (p->timed[TMD_BLIND] || no_light(p) || p->timed[TMD_CONFUSED] || p->timed[TMD_IMAGE])
            i = i / 10;

        /* Extract the door power */
        j = square_door_power(c, grid);

        /* Extract the difficulty XXX XXX XXX */
        j = i - (j * 4);

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success */
        if (magik(j))
        {
            /* Message */
            msgt(p, MSG_LOCKPICK, "You have picked the lock.");

            /* Open the door */
            square_open_door(c, grid);

            /* Update the visuals */
            square_memorize(p, c, grid);
            square_light_spot_aux(p, c, grid);
            update_visuals(&p->wpos);
        }

        /* Failure */
        else
        {
            /* Message */
            msgt(p, MSG_LOCKPICK_FAIL, "You failed to pick the lock.");

            /* We may keep trying */
            more = true;
        }
    }

    /* Closed door */
    else
    {
        /* Open the door */
        square_open_door(c, grid);

        /* Update the visuals */
        square_memorize(p, c, grid);
        square_light_spot_aux(p, c, grid);
        update_visuals(&p->wpos);

        /* Sound */
        sound(p, MSG_OPENDOOR);
    }

    /* Result */
    return (more);
}


/*
 * Monster or player in the way
 */
static bool is_blocker(struct player *p, struct chunk *c, struct loc *grid, bool allow_5)
{
    if (!square(c, grid)->mon) return false;
    if (!player_is_at(p, grid)) return true;
    return !allow_5;
}


/*
 * Attack monster or player in the way
 */
static void attack_blocker(struct player *p, struct chunk *c, struct loc *grid)
{
    struct source who_body;
    struct source *who = &who_body;

    square_actor(c, grid, who);

    /* Monster in the way */
    if (who->monster)
    {
        /* Mimics surprise the player */
        if (monster_is_camouflaged(who->monster))
        {
            become_aware(p, c, who->monster);

            /* Mimic wakes up */
            if (pvm_check(p, who->monster))
                mon_clear_timed(p, who->monster, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);
        }
        else
        {
            /* Message */
            msg(p, "There is a monster in the way!");

            /* Attack */
            if (pvm_check(p, who->monster)) py_attack(p, c, grid);
        }
    }

    /* Player in the way */
    else
    {
        /* Reveal mimics */
        if (who->player->k_idx)
            aware_player(p, who->player);

        /* Message */
        msg(p, "There is a player in the way!");

        /* Attack */
        if (pvp_check(p, who->player, PVP_DIRECT, true, square(c, grid)->feat))
            py_attack(p, c, grid);
    }
}


/*
 * Open a closed/locked door or a closed/locked chest.
 *
 * Unlocking a locked chest is worth one experience point; since doors are
 * player lockable, there is no experience for unlocking doors.
 */
void do_cmd_open(struct player *p, int dir, bool easy)
{
    struct loc grid;
    struct object *obj;
    bool more = false, allow_5 = false;
    struct chunk *c = chunk_get(&p->wpos);

    /* Easy Open */
    if (easy)
    {
        int n_closed_doors, n_locked_chests;

        /* Count closed doors */
        n_closed_doors = count_feats(p, c, &grid, square_iscloseddoor, false);

        /* Count chests (locked) */
        n_locked_chests = count_chests(p, c, &grid, CHEST_OPENABLE);

        /* Use the last target found */
        if ((n_closed_doors + n_locked_chests) >= 1)
            dir = motion_dir(&p->grid, &grid);

        /* If there are chests to open, allow 5 as a direction */
        if (n_locked_chests) allow_5 = true;
    }

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    next_grid(&grid, &p->grid, dir);

    /* Check for chest */
    obj = chest_check(p, c, &grid, CHEST_OPENABLE);

    /* Check for door */
    if (!obj && !do_cmd_open_test(p, c, &grid))
    {
        /* Cancel repeat */
        disturb(p, 0);
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Apply confusion */
    if (player_confuse_dir(p, &dir))
    {
        /* Get location */
        next_grid(&grid, &p->grid, dir);

        /* Check for chests */
        obj = chest_check(p, c, &grid, CHEST_OPENABLE);
    }

    /* Something in the way */
    if (is_blocker(p, c, &grid, allow_5))
        attack_blocker(p, c, &grid);

    /* Chest */
    else if (obj)
        more = do_cmd_open_chest(p, c, &grid, obj);

    /* Door */
    else
        more = do_cmd_open_aux(p, c, &grid);

    /* Cancel repeat unless we may continue */
    if (!more) disturb(p, 0);
}


/*
 * Determine if a given grid may be "closed"
 */
static bool do_cmd_close_test(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Ghosts cannot close things */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS))
    {
        /* Message */
        msg(p, "You cannot close things!");

        /* Nope */
        return false;
    }

    /* Handle polymorphed players */
    if (p->poly_race && !OPT(p, birth_fruit_bat))
    {
        if (!rf_has(p->poly_race->flags, RF_OPEN_DOOR))
        {
            /* Message */
            msg(p, "You cannot close things!");

            /* Nope */
            return false;
        }
    }

    /* Check preventive inscription '^c' */
    if (check_prevent_inscription(p, INSCRIPTION_CLOSE))
    {
        msg(p, "The item's inscription prevents it.");
        return false;
    }

    /* Must have knowledge */
    if (!square_isknown(p, grid))
    {
        /* Message */
        msg(p, "You see nothing there.");

        /* Nope */
        return false;
    }

    /* Require open/broken door */
    if (!square_isopendoor(c, grid) && !square_isbrokendoor(c, grid))
    {
        /* Message */
        msg(p, "You see nothing there to close.");

        /* Nope */
        return false;
    }

    /* Okay */
    return true;
}


/*
 * Perform the basic "close" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns true if repeated commands may continue
 */
static bool do_cmd_close_aux(struct player *p, struct chunk *c, struct loc *grid)
{
    bool more = false;
    int i;

    /* Verify legality */
    if (!do_cmd_close_test(p, c, grid)) return false;

    /* Broken door */
    if (square_isbrokendoor(c, grid))
        msg(p, "The door appears to be broken.");

    /* House door, close it */
    else if (square_home_isopendoor(c, grid))
    {
        /* Find this house */
        i = pick_house(&p->wpos, grid);

        /* Close the door */
        square_colorize_door(c, grid, house_get(i)->color);

        /* Update the visuals */
        square_memorize(p, c, grid);
        square_light_spot_aux(p, c, grid);
        update_visuals(&p->wpos);

        /* Sound */
        sound(p, MSG_SHUTDOOR);
    }

    /* Close the door */
    else
    {
        /* Close the door */
        square_close_door(c, grid);

        /* Update the visuals */
        square_memorize(p, c, grid);
        square_light_spot_aux(p, c, grid);
        update_visuals(&p->wpos);

        /* Sound */
        sound(p, MSG_SHUTDOOR);
    }

    /* Result */
    return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(struct player *p, int dir, bool easy)
{
    bool more = false;
    struct chunk *c = chunk_get(&p->wpos);
    struct loc grid;

    /* Easy Close */
    if (easy)
    {
        /* Count open doors */
        if (count_feats(p, c, &grid, square_isopendoor, false) >= 1)
            dir = motion_dir(&p->grid, &grid);
    }

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    next_grid(&grid, &p->grid, dir);

    /* Verify legality */
    if (!do_cmd_close_test(p, c, &grid))
    {
        /* Cancel repeat */
        disturb(p, 0);
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Apply confusion */
    if (player_confuse_dir(p, &dir))
    {
        /* Get location */
        next_grid(&grid, &p->grid, dir);
    }

    /* Something in the way */
    if (square(c, &grid)->mon != 0)
        attack_blocker(p, c, &grid);

    /* Door - close it */
    else
        more = do_cmd_close_aux(p, c, &grid);

    /* Cancel repeat unless we may continue */
    if (!more) disturb(p, 0);
}


/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Ghosts cannot tunnel */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS))
    {
        msg(p, "You cannot tunnel.");
        return false;
    }

    /* Check preventive inscription '^T' */
    if (check_prevent_inscription(p, INSCRIPTION_TUNNEL))
    {
        msg(p, "The item's inscription prevents it.");
        return false;
    }

    /* Must have knowledge */
    if (!square_isknown(p, grid))
    {
        msg(p, "You see nothing there.");
        return false;
    }

    /* Must be a wall/door/etc */
    if (!square_seemsdiggable(c, grid))
    {
        msg(p, "You see nothing there to tunnel.");
        return false;
    }

    /* No tunneling on special levels and towns */
    if (special_level(&p->wpos) || in_town(&p->wpos))
    {
        msg(p, "Nothing happens.");
        return false;
    }

    /* Okay */
    return true;
}


/*
 * Tunnel through wall. Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * Attempting to do so will produce floor grids which are not part
 * of the room, and whose "illumination" status do not change with
 * the rest of the room.
 */
static bool twall(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Paranoia -- require a wall or door or some such */
    if (!square_seemsdiggable(c, grid)) return false;

    /* Sound */
    sound(p, MSG_DIG);

    /* Remove the feature */
    square_tunnel_wall(c, grid);

    /* Update the visuals */
    update_visuals(&p->wpos);

    /* Fully update the flow */
    fully_update_flow(&p->wpos);

    /* Result */
    return true;
}


/* Forward declaration */
static bool create_house_door(struct player *p, struct chunk *c, struct loc *grid);


/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination.
 * Uses "twall" (above) to do all "terrain feature changing".
 * Returns true if repeated commands may continue.
 */
static bool do_cmd_tunnel_aux(struct player *p, struct chunk *c, struct loc *grid)
{
    bool more = false;
    int digging_chances[DIGGING_MAX];
    bool okay = false;
    bool gold, rubble, tree, web;
    int digging;

    gold = square_hasgoldvein(c, grid);
    rubble = square_isrubble(c, grid);
    tree = square_istree(c, grid);
    web = square_isweb(c, grid);

    /* Verify legality */
    if (!do_cmd_tunnel_test(p, c, grid)) return false;

    calc_digging_chances(p, &p->state, digging_chances);

    /* Do we succeed? */
    digging = square_digging(c, grid);
    if (digging > 0) okay = CHANCE(digging_chances[digging - 1], 1600);

    /* Hack -- pit walls (to fool the player) */
    if (square_ispermfake(c, grid))
    {
        msg(p, "You tunnel into the granite wall.");
        more = true;
    }

    /* Hack -- house walls */
    else if (square_ispermhouse(c, grid))
    {
        /* Either the player has lost his mind or he is trying to create a door! */
        create_house_door(p, c, grid);
    }

    /* Permanent */
    else if (square_isperm(c, grid))
    {
        /* Hack -- logs */
        if (!feat_is_wall(square(c, grid)->feat))
            msg(p, "You cannot tunnel through that.");
        else
            msg(p, "This seems to be permanent rock.");
    }

    /* Mountain */
    else if (square_ismountain(c, grid))
        msg(p, "Digging a tunnel into that mountain would take forever!");

    /* Success */
    else if (okay && twall(p, c, grid))
    {
        /* Mow down the vegetation */
        if (tree)
        {
            /* Message */
            msg(p, "You hack your way through the vegetation.");
        }

        /* Clear some web */
        else if (web)
            msg(p, "You hack your way through the web.");

        /* Remove the rubble */
        else if (rubble)
        {
            /* Message */
            msg(p, "You have removed the rubble.");

            /* Place an object */
            if (magik(10))
            {
                /* Create a simple object */
                place_object(p, c, grid, object_level(&p->wpos), false, false, ORIGIN_RUBBLE, 0);

                /* Observe the new object */
                if (!ignore_item_ok(p, square_object(c, grid)) && square_isseen(p, grid))
                    msg(p, "You have found something!");
            }
        }

        /* Found treasure */
        else if (gold)
        {
            /* Place some gold */
            place_gold(p, c, grid, object_level(&p->wpos), ORIGIN_FLOOR);

            /* Message */
            msg(p, "You have found something!");
        }

        /* Found nothing */
        else
            msg(p, "You have finished the tunnel.");
    }

    /* Failure, continue digging */
    else
    {
        if (tree || web)
            msg(p, "You attempt to clear a path.");
        else if (rubble)
            msg(p, "You dig in the rubble.");
        else
            msg(p, "You tunnel into the %s.", square_apparent_name(p, c, grid));
         more = true;
    }

    /* Result */
    return (more);
}


/*
 * Tunnel through "walls" (including rubble and doors, secret or otherwise)
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
bool do_cmd_tunnel(struct player *p)
{
    int dir = (int)p->digging_dir;
    bool more = false;
    struct chunk *c = chunk_get(&p->wpos);
    struct loc grid;

    /* Cancel repeat */
    if (!p->digging_request) return true;

    /* Check energy */
    if (!has_energy(p, true)) return false;

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir))
    {
        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Get location */
    next_grid(&grid, &p->grid, dir);

    /* Oops */
    if (!do_cmd_tunnel_test(p, c, &grid))
    {
        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Take a turn */
    use_energy(p);

    /* Apply confusion */
    if (player_confuse_dir(p, &dir))
    {
        /* Get location */
        next_grid(&grid, &p->grid, dir);
    }

    /* Attack anything we run into */
    if (square(c, &grid)->mon != 0)
        attack_blocker(p, c, &grid);

    /* Tunnel through walls */
    else
        more = do_cmd_tunnel_aux(p, c, &grid);

    /* Cancel repetition unless we can continue */
    if (!more) disturb(p, 0);

    /* Repeat */
    if (p->digging_request > 0) p->digging_request--;
    if (p->digging_request > 0) cmd_tunnel(p);
    return true;
}


/*
 * Determine if a given grid may be "disarmed"
 */
static bool do_cmd_disarm_test(struct player *p, struct chunk *c, struct loc *grid)
{
    /* Ghosts cannot disarm */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS))
    {
        msg(p, "You cannot disarm things!");
        return false;
    }

    /* Check preventive inscription '^D' */
    if (check_prevent_inscription(p, INSCRIPTION_DISARM))
    {
        msg(p, "The item's inscription prevents it.");
        return false;
    }

    /* Must have knowledge */
    if (!square_isknown(p, grid))
    {
        msg(p, "You see nothing there.");
        return false;
    }

    /* Look for a closed, unlocked door to lock */
    if (square_basic_iscloseddoor(c, grid) && !square_islockeddoor(c, grid))
        return true;

    /* Look for a trap */
    if (!square_isdisarmabletrap(c, grid))
    {
        msg(p, "You see nothing there to disarm.");
        return false;
    }

    /* Okay */
    return true;
}


/*
 * Perform the command "lock door"
 *
 * Assume there is no monster blocking the destination
 *
 * Returns true if repeated commands may continue
 */
static bool do_cmd_lock_door(struct player *p, struct chunk *c, struct loc *grid)
{
    int i, j, power;
    bool more = false;

    /* Verify legality */
    if (!do_cmd_disarm_test(p, c, grid)) return false;

    /* Get the "disarm" factor */
    i = p->state.skills[SKILL_DISARM_PHYS];

    /* Penalize some conditions */
    if (p->timed[TMD_BLIND] || no_light(p) || p->timed[TMD_CONFUSED] || p->timed[TMD_IMAGE])
        i = i / 10;

    /* Calculate lock "power" */
    power = m_bonus(7, p->wpos.depth);

    /* Extract the difficulty */
    j = i - power;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Success */
    if (magik(j))
    {
        msg(p, "You lock the door.");
        square_set_door_lock(c, grid, power);
    }

    /* Failure -- keep trying */
    else if (magik(j))
    {
        msg(p, "You failed to lock the door.");

        /* We may keep trying */
        more = true;
    }

    /* Failure */
    else
        msg(p, "You failed to lock the door.");

    /* Result */
    return more;
}


/*
 * Perform the basic "disarm" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns true if repeated commands may continue
 */
static bool do_cmd_disarm_aux(struct player *p, struct chunk *c, struct loc *grid, int dir)
{
    int skill, power, chance;
    struct trap *trap = square(c, grid)->trap;
    bool more = false;

    /* Verify legality */
    if (!do_cmd_disarm_test(p, c, grid)) return false;

    /* Choose first player trap */
    while (trap)
    {
        if (trf_has(trap->flags, TRF_TRAP)) break;
        trap = trap->next;
    }
    if (!trap) return false;

    /* Get the base disarming skill */
    if (trf_has(trap->flags, TRF_MAGICAL))
        skill = p->state.skills[SKILL_DISARM_MAGIC];
    else
        skill = p->state.skills[SKILL_DISARM_PHYS];

    /* Penalize some conditions */
    if (p->timed[TMD_BLIND] || no_light(p) || p->timed[TMD_CONFUSED] || p->timed[TMD_IMAGE])
        skill = skill / 10;

    /* Extract trap power */
    power = MAX(c->wpos.depth, 0) / 5;

    /* Extract the percentage success */
    chance = skill - power;

    /* Always have a small chance of success */
    if (chance < 2) chance = 2;

    /* Two chances - one to disarm, one not to set the trap off */
    if (magik(chance))
    {
        msgt(p, MSG_DISARM, "You have disarmed the %s.", trap->kind->name);
        player_exp_gain(p, 1 + power);

        /* Trap is gone */
        square_destroy_trap(c, grid);
    }
    else if (magik(chance))
    {
        msg(p, "You failed to disarm the %s.", trap->kind->name);

        /* Player can try again */
        more = true;
    }
    else
    {
        msg(p, "You set off the %s!", trap->kind->name);
        move_player(p, c, dir, false, false, true);
    }

    /* Result */
    return (more);
}


static bool square_hack_iscloseddoor(struct chunk *c, struct loc *grid)
{
    return (square_basic_iscloseddoor(c, grid) && !square_islockeddoor(c, grid));
}


/*
 * Disarms a trap, or chest
 *
 * Traps must be visible, chests must be known trapped
 */
void do_cmd_disarm(struct player *p, int dir, bool easy)
{
    struct object *obj;
    bool more = false, allow_5 = false;
    struct chunk *c = chunk_get(&p->wpos);
    struct loc grid;

    /* Easy Disarm */
    if (easy)
    {
        int num_doors, n_traps, n_chests;

        /* Hack -- count closed doors (for door locking) */
        num_doors = count_feats(p, c, &grid, square_hack_iscloseddoor, false);

        /* Count visible traps */
        n_traps = count_feats(p, c, &grid, square_isdisarmabletrap, true);

        /* Count chests (trapped) */
        n_chests = count_chests(p, c, &grid, CHEST_TRAPPED);

        /* Use the last target found */
        if ((num_doors + n_traps + n_chests) >= 1)
            dir = motion_dir(&p->grid, &grid);

        /* If there are trap or chests to open, allow 5 as a direction */
        if (n_traps || n_chests) allow_5 = true;
    }

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Get location */
    next_grid(&grid, &p->grid, dir);

    /* Check for chests */
    obj = chest_check(p, c, &grid, CHEST_TRAPPED);

    /* Verify legality */
    if (!obj && !do_cmd_disarm_test(p, c, &grid))
    {
        /* Cancel repeat */
        disturb(p, 0);
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Apply confusion */
    if (player_confuse_dir(p, &dir))
    {
        /* Get location */
        next_grid(&grid, &p->grid, dir);

        /* Check for chests */
        obj = chest_check(p, c, &grid, CHEST_TRAPPED);
    }

    /* Something in the way */
    if (is_blocker(p, c, &grid, allow_5))
        attack_blocker(p, c, &grid);

    /* Chest */
    else if (obj)
        more = do_cmd_disarm_chest(p, c, &grid, obj);

    /* Door to lock */
    else if (square_basic_iscloseddoor(c, &grid))
        more = do_cmd_lock_door(p, c, &grid);

    /* Disarm trap */
    else
        more = do_cmd_disarm_aux(p, c, &grid, dir);

    /* Cancel repeat unless told not to */
    if (!more) disturb(p, 0);
}     


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * This command must always take energy, to prevent free detection
 * of invisible monsters.
 * REVISED FOR MAngband-specific reasons: we don't care if someone
 * detects a monster by tunneling into it, and treat "tunnel air" as an
 * error, which DOES NOT spend player's energy.
 *
 * The "semantics" of this command must be chosen before the player
 * is confused, and it must be verified against the new grid.
 */
void do_cmd_alter(struct player *p, int dir)
{
    bool more = false;
    bool spend = true;
    struct chunk *c = chunk_get(&p->wpos);
    struct loc grid;

    /* Get a direction (or abort) */
    if (!dir || !VALID_DIR(dir)) return;

    /* Check preventive inscription '^+' */
    if (check_prevent_inscription(p, INSCRIPTION_ALTER))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Get location */
    next_grid(&grid, &p->grid, dir);

    /* Apply confusion */
    if (player_confuse_dir(p, &dir))
    {
        /* Get location */
        next_grid(&grid, &p->grid, dir);
    }

    /* Something in the way */
    if (square(c, &grid)->mon != 0)
    {
        /* Attack */
        attack_blocker(p, c, &grid);
    }

    /* MAngband-specific: open house walls (House Creation) */
    else if (square_ispermhouse(c, &grid))
        more = do_cmd_tunnel_aux(p, c, &grid);

    /* Tunnel through walls, trees and rubble (allow pit walls to fool the player) */
    else if (square_isdiggable(c, &grid) || square_ispermfake(c, &grid))
        more = do_cmd_tunnel_aux(p, c, &grid);

    /* Open closed doors */
    else if (square_iscloseddoor(c, &grid))
        more = do_cmd_open_aux(p, c, &grid);

    /* Disarm traps */
    else if (square_isdisarmabletrap(c, &grid))
        more = do_cmd_disarm_aux(p, c, &grid, dir);

    /* Oops */
    else
    {
        msg(p, "You spin around.");

        /* Do not spend energy. */
        spend = false;
    }

    /* Take a turn */
    if (spend) use_energy(p);

    /* Cancel repetition unless we can continue */
    if (!more) disturb(p, 0);
}


static const char *comment_ironman[] =
{
    "You don't feel like going to pick flowers right now.",
    "Where do you think you are going?",
    "Morgoth the potato farmer? - get real!",
    "Morgoth awaits you in the depths not in the fields.",
    "Something draws your attention back to the stairs."
};


/* Do a probability travel in a wall */
static void do_prob_travel(struct player *p, struct chunk *c, int dir)
{
    bool do_move = true;
    struct loc grid, next;

    loc_copy(&grid, &p->grid);

    /* Paranoia */
    if ((dir == DIR_TARGET) || !dir) return;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    next_grid(&next, &grid, dir);
    loc_copy(&grid, &next);

    while (true)
    {
        /* Do not get out of the level */
        if (!square_in_bounds_fully(c, &grid))
        {
            do_move = false;
            break;
        }

        /* Require a "naked" floor grid */
        if (!square_isempty(c, &grid) || square_isvault(c, &grid))
        {
            next_grid(&next, &grid, dir);
            loc_copy(&grid, &next);
            continue;
        }

        /* Everything is ok */
        do_move = true;
        break;
    }

    if (do_move) monster_swap(c, &p->grid, &grid);
}


/*
 * Move player in the given direction.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
void move_player(struct player *p, struct chunk *c, int dir, bool disarm, bool check_pickup,
    bool force)
{
    bool old_dtrap, new_dtrap, old_pit, new_pit;
    bool do_move = true;
    struct source who_body;
    struct source *who = &who_body;
    bool trapsafe = player_is_trapsafe(p);
    bool alterable;
    struct loc grid;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    next_grid(&grid, &p->grid, dir);
    alterable = (square_isdisarmabletrap(c, &grid) || square_iscloseddoor(c, &grid));

    /* Handle polymorphed players */
    if (p->poly_race)
    {
        if (rf_has(p->poly_race->flags, RF_NEVER_MOVE)) do_move = false;

        /* Unaware players trying to move reveal themselves */
        if (p->k_idx) aware_player(p, p);
    }

    /* New player location on the world map */
    if ((p->wpos.depth == 0) && !square_in_bounds_fully(c, &grid))
    {
        struct wild_type *w_ptr;
        struct loc new_world_grid, new_grid;

        loc_copy(&new_world_grid, &p->wpos.grid);
        loc_copy(&new_grid, &p->grid);

        /* Handle polymorphed players */
        if (!do_move)
        {
            msg(p, "You cannot move!");
            return;
        }

        /* Leaving base town */
        if (in_base_town(&p->wpos))
        {
            /* Forbid */
            if ((cfg_diving_mode > 1) || OPT(p, birth_no_recall))
            {
                if (cfg_diving_mode > 1)
                    msg(p, "There is a wall blocking your way.");
                else
                    msg(p, ONE_OF(comment_ironman));
                disturb(p, 1);
                return;
            }

            /* Warn */
            if (p->lev == 1)
                msg(p, "Really enter the wilderness? The dungeon entrance is in the town!");
        }

        /* Find his new location */
        if (grid.y <= 0)
        {
            new_world_grid.y++;
            new_grid.y = c->height - 2;
        }
        if (grid.y >= c->height - 1)
        {
            new_world_grid.y--;
            new_grid.y = 1;
        }
        if (grid.x <= 0)
        {
            new_world_grid.x--;
            new_grid.x = c->width - 2;
        }
        if (grid.x >= c->width - 1)
        {
            new_world_grid.x++;
            new_grid.x = 1;
        }

        /* New location */
        w_ptr = get_wt_info_at(&new_world_grid);

        /* Check to make sure he hasn't hit the edge of the world */
        if (!w_ptr)
        {
            switch (randint0(2))
            {
                case 0: msg(p, "You have reached the Walls of the World. You can not pass."); break;
                case 1: msg(p, "You cannot go beyond the Walls of the World."); break;
            }

            disturb(p, 1);
            return;
        }

        /* Hack -- DM redesigning the level */
        if (chunk_inhibit_players(&w_ptr->wpos))
        {
            msg(p, "Something prevents you from going this way...");
            return;
        }

        /* Change location */
        dungeon_change_level(p, c, &w_ptr->wpos, LEVEL_OUTSIDE);

        /* Hack -- replace the player */
        loc_copy(&p->old_grid, &new_grid);
        loc_copy(&p->grid, &new_grid);

        /* Update the wilderness map */
        wild_set_explored(p, &w_ptr->wpos);

        /* Disturb if necessary */
        if (OPT(p, disturb_panel)) disturb(p, 0);

        return;
    }

    /* Save "last direction moved" */
    p->last_dir = dir;

    square_actor(c, &grid, who);

    /* Bump into other players */
    if (who->player)
    {
        /* Don't bump into self! */
        if (who->player != p)
        {
            /* Reveal mimics */
            if (who->player->k_idx)
                aware_player(p, who->player);

            /* Check for an attack */
            if (pvp_check(p, who->player, PVP_DIRECT, true, square(c, &grid)->feat))
                py_attack(p, c, &grid);

            /* Handle polymorphed players */
            else if (!do_move)
                msg(p, "You cannot move!");

            /* Switch places */
            else if ((!player_passwall(p) && !player_passwall(who->player) &&
                (ddy[who->player->last_dir] == (0 - ddy[dir])) &&
                (ddx[who->player->last_dir] == (0 - ddx[dir]))) ||
                (who->player->dm_flags & DM_SECRET_PRESENCE))
            {
                monster_swap(c, &p->grid, &who->player->grid);

                /* Don't tell people they bumped into the Dungeon Master */
                if (!(who->player->dm_flags & DM_SECRET_PRESENCE))
                {
                    char p_name[NORMAL_WID], q_name[NORMAL_WID];

                    player_desc(p, q_name, sizeof(q_name), who->player, false);
                    player_desc(who->player, p_name, sizeof(p_name), p, false);

                    /* Tell both of them */
                    msg(p, "You switch places with %s.", q_name);
                    msg(who->player, "You switch places with %s.", p_name);

                    /* Disturb both of them */
                    disturb(p, 1);
                    disturb(who->player, 1);
                }

                /* Unhack both of them */
                who->player->last_dir = p->last_dir = 5;

                player_know_floor(p, c);
                player_know_floor(who->player, c);
            }

            /* Bump into other players */
            else if (!(p->dm_flags & DM_SECRET_PRESENCE))
            {
                char p_name[NORMAL_WID], q_name[NORMAL_WID];

                player_desc(p, q_name, sizeof(q_name), who->player, false);
                player_desc(who->player, p_name, sizeof(p_name), p, true);

                /* Tell both about it */
                msg(p, "You bump into %s.", q_name);
                msg(who->player, "%s bumps into you.", p_name);

                /* Disturb both parties */
                disturb(p, 1);
                disturb(who->player, 1);
            }
        }

        return;
    }

    /* Bump into monsters */
    if (who->monster)
    {
        /* Check for an attack */
        if (pvm_check(p, who->monster))
        {
            /* Attack monsters */
            if (monster_is_camouflaged(who->monster))
            {
                become_aware(p, c, who->monster);

                /* Mimic wakes up */
                mon_clear_timed(p, who->monster, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);
            }
            else
                py_attack(p, c, &grid);
        }

        /* Handle polymorphed players */
        else if (!do_move)
            msg(p, "You cannot move!");

        /* Reveal mimics */
        else if (monster_is_camouflaged(who->monster))
            become_aware(p, c, who->monster);

        /* Switch places */
        else
        {
            monster_swap(c, &p->grid, &who->monster->grid);
            player_know_floor(p, c);
        }

        return;
    }

    /* Arena */
    if (square_ispermarena(c, &grid))
    {
        access_arena(p, &grid);
        return;
    }

    /* Prob travel */
    if (p->timed[TMD_PROBTRAVEL] && !square_ispassable(c, &grid))
    {
        do_prob_travel(p, c, dir);
        return;
    }

    /* Optionally alter traps/doors on movement */
    if (alterable && disarm && square_isknown(p, &grid))
    {
        do_cmd_alter(p, dir);
        return;
    }

    /* Stop running before known traps */
    if (p->upkeep->running && square_isdisarmabletrap(c, &grid) && !trapsafe)
    {
        disturb(p, 0);
        return;
    }

    /* Normal players can not walk through "walls" */
    if (!player_passwall(p) && !square_ispassable(c, &grid))
    {
        disturb(p, 0);

        /* Notice unknown obstacles */
        if (!square_isknown(p, &grid))
        {
            /* Rubble */
            if (square_isrubble(c, &grid))
            {
                msgt(p, MSG_HITWALL, "You feel a pile of rubble blocking your way.");
                square_memorize(p, c, &grid);
                square_light_spot_aux(p, c, &grid);
            }

            /* Closed door */
            else if (square_iscloseddoor(c, &grid))
            {
                msgt(p, MSG_HITWALL, "You feel a door blocking your way.");
                square_memorize(p, c, &grid);
                square_light_spot_aux(p, c, &grid);
            }

            /* Tree */
            else if (square_istree(c, &grid))
            {
                msgt(p, MSG_HITWALL, "You feel a tree blocking your way.");
                square_memorize(p, c, &grid);
                square_light_spot_aux(p, c, &grid);
            }

            /* Wall (or secret door) */
            else
            {
                msgt(p, MSG_HITWALL, "You feel a wall blocking your way.");
                square_memorize(p, c, &grid);
                square_light_spot_aux(p, c, &grid);
            }
        }

        /* Mention known obstacles */
        else
        {
            /* Rubble */
            if (square_isrubble(c, &grid))
                msgt(p, MSG_HITWALL, "There is a pile of rubble blocking your way.");

            /* Closed doors */
            else if (square_iscloseddoor(c, &grid))
                msgt(p, MSG_HITWALL, "There is a door blocking your way.");

            /* Tree */
            else if (square_istree(c, &grid))
                msgt(p, MSG_HITWALL, "There is a tree blocking your way.");

            /* Wall (or secret door) */
            else
                msgt(p, MSG_HITWALL, "There is a wall blocking your way.");
        }

        return;
    }

    /* Permanent walls */
    if (player_passwall(p) && square_isperm(c, &grid))
    {
        /* Forbid in most cases */
        if (p->timed[TMD_WRAITHFORM] || player_can_undead(p) || !square_in_bounds_fully(c, &grid))
        {
            /* Message */
            msg(p, "The wall blocks your movement.");

            disturb(p, 0);
            return;
        }
    }

    /* Wraith trying to run inside a house */
    if (p->timed[TMD_WRAITHFORM] && square_home_iscloseddoor(c, &grid))
    {
        do_cmd_open(p, dir, false);
        return;
    }

    /* Handle polymorphed players */
    if (!do_move && !force)
    {
        msg(p, "You cannot move!");
        return;
    }

    /* See if trap detection status will change */
    old_dtrap = square_isdtrap(p, &p->grid);
    new_dtrap = square_isdtrap(p, &grid);

    /* Note the change in the detect status */
    if (old_dtrap != new_dtrap) p->upkeep->redraw |= (PR_DTRAP);

    /* Disturb if the player is about to leave the area */
    if (p->upkeep->running && !p->upkeep->running_firststep && old_dtrap && !new_dtrap &&
        random_level(&p->wpos))
    {
        disturb(p, 0);
        return;
    }

    /* PWMAngband: display a message when entering a pit */
    old_pit = square_ispitfloor(c, &p->grid);
    new_pit = square_ispitfloor(c, &grid);
    if (new_pit && !old_pit)
        msgt(p, MSG_ENTER_PIT, "The floor is very dusty and the air feels very still!");

    /* Move player */
    monster_swap(c, &p->grid, &grid);

    /* Handle store doors, or notice objects */
    if (!p->ghost && square_isshop(c, &grid))
    {
        disturb(p, 0);

        /* Hack -- enter store */
        do_cmd_store(p, -1);
    }
    if (square(c, &grid)->obj)
    {
        p->ignore = 1;
        player_know_floor(p, c);
        do_autopickup(p, c, check_pickup);
        current_clear(p);
        player_pickup_item(p, c, check_pickup, NULL);
    }

    /* Handle resurrection */
    else if (p->ghost && square_isshop(c, &grid))
    {
        struct store *s = &stores[square_shopnum(c, &grid)];

        if (s->type == STORE_TEMPLE)
        {
            /* Resurrect him */
            resurrect_player(p, c);

            /* Give him some gold */
            if (!is_dm_p(p) && !player_can_undead(p) && (p->lev >= 5))
                p->au = 100 * (p->lev - 4) / p->lives;
        }
    }

    /* Discover invisible traps */
    else if (square_issecrettrap(c, &grid))
    {
        disturb(p, 0);
        hit_trap(p);
    }

    /* Set off a visible trap */
    else if (square_isdisarmabletrap(c, &grid) && !trapsafe)
    {
        disturb(p, 0);
        hit_trap(p);
    }

    /* Mention fountains */
    else if (square_isfountain(c, &grid))
    {
        disturb(p, 0);
        msg(p, "A fountain is located at this place.");
    }

    p->upkeep->running_firststep = false;

    /* Hack -- we're done if player is gone (trap door) */
    if (p->upkeep->new_level_method) return;

    /* Update view and search */
    update_view(p, c);
    search(p, c);

    /*
     * Hack -- if we are the dungeon master, and our movement hook
     * is set, call it.  This is used to make things like building walls
     * and summoning monster armies easier.
     */
    if (is_dm_p(p) && master_move_hook)
        master_move_hook(p, NULL);
}


/*
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(struct player *p)
{
    /* Check preventive inscription '^;' */
    if (check_prevent_inscription(p, INSCRIPTION_WALK))
    {
        msg(p, "The item's inscription prevents it.");
        return false;
    }

    /* Tests are done in move_player() */

    /* Okay */
    return true;
}


/*
 * Apply erratic movement, if needed, to a direction
 *
 * Return true if direction changes.
 */
static bool erratic_dir(struct player *p, int *dp)
{
    int dir;
    int erratic = 0;

    /* Default */
    dir = (*dp);

    /* Handle polymorphed players */
    if (p->poly_race)
    {
        if (rf_has(p->poly_race->flags, RF_RAND_25)) erratic += 10;
        if (rf_has(p->poly_race->flags, RF_RAND_50)) erratic += 20;
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
        return true;
    }

    /* Normal movement */
    return false;
}


/*
 * Walk in the given direction.
 */
void do_cmd_walk(struct player *p, int dir)
{
    struct chunk *c = chunk_get(&p->wpos);
    bool trapsafe = player_is_trapsafe(p);
    struct loc grid;

    /* Get a direction (or abort) */
    if (!dir) return;

    /* Verify legality */
    if (!do_cmd_walk_test(p)) return;

    /* Take a turn */
    use_energy(p);

    /* Apply confusion/erratic movement */
    player_confuse_dir(p, &dir);
    erratic_dir(p, &dir);

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    next_grid(&grid, &p->grid, dir);

    /* Attempt to disarm unless it's a trap and we're trapsafe */
    move_player(p, c, dir, !(square_isdisarmabletrap(c, &grid) && trapsafe), true, false);
}


/*
 * Walk into a trap
 */
void do_cmd_jump(struct player *p, int dir)
{
    /* Get a direction (or abort) */
    if (!dir) return;

    /* Verify legality */
    if (!do_cmd_walk_test(p)) return;

    /* Take a turn */
    use_energy(p);

    /* Apply confusion/erratic movement */
    player_confuse_dir(p, &dir);
    erratic_dir(p, &dir);

    /* Move the player */
    move_player(p, chunk_get(&p->wpos), dir, false, true, false);
}


/*
 * Determine if a given grid may be "run"
 */
static bool do_cmd_run_test(struct player *p, struct loc *grid)
{
    struct chunk *c = chunk_get(&p->wpos);

    /* Check preventive inscription '^.' */
    if (check_prevent_inscription(p, INSCRIPTION_RUN))
    {
        msg(p, "The item's inscription prevents it.");
        return false;
    }

    /* Ghosts run right through everything */
    if (player_passwall(p)) return true;

    /* Do wilderness hack, keep running from one outside level to another */
    if (!square_in_bounds_fully(c, grid) && (p->wpos.depth == 0)) return true;

    /* Illegal grids are not known walls XXX XXX XXX */
    if (!square_in_bounds(c, grid)) return true;

    /* Hack -- walking obtains knowledge XXX XXX */
    if (!square_isknown(p, grid)) return true;

    /* Require open space */
    if (!square_ispassable(c, grid))
    {
        /* Rubble */
        if (square_isrubble(c, grid))
            msgt(p, MSG_HITWALL, "There is a pile of rubble in the way!");

        /* Door */
        else if (square_iscloseddoor(c, grid))
            return true;

        /* Tree */
        else if (square_istree(c, grid))
            msgt(p, MSG_HITWALL, "There is a tree in the way!");

        /* Wall */
        else
            msgt(p, MSG_HITWALL, "There is a wall in the way!");

        /* Cancel repeat */
        disturb(p, 0);

        /* Nope */
        return false;
    }

    /* Okay */
    return true;
}


/*
 * Start running.
 *
 * Note that running while confused is not allowed
 */
void do_cmd_run(struct player *p, int dir)
{
    struct loc grid;

    /* Not while confused */
    if (p->timed[TMD_CONFUSED])
    {
        msg(p, "You are too confused!");
        return;
    }

    /* Handle polymorphed players */
    if (p->poly_race)
    {
        if (rf_has(p->poly_race->flags, RF_RAND_25) ||
            rf_has(p->poly_race->flags, RF_RAND_50))
        {
            msg(p, "Your nature prevents you from running straight.");
            return;
        }
    }

    /* Ignore invalid directions */
    if ((dir == DIR_TARGET) || !VALID_DIR(dir)) return;

    /* Ignore non-direction if we are not running */
    if (!p->upkeep->running && !dir) return;

    /* Continue running if we are already running in this direction */
    if (p->upkeep->running && (dir == p->run_cur_dir)) dir = 0;

    /* Get location */
    if (dir)
    {
        next_grid(&grid, &p->grid, dir);

        /* Verify legality */
        if (!do_cmd_run_test(p, &grid)) return;
    }

    /* Start run */
    run_step(p, dir);
}


/*
 * Rest (restores hit points and mana and such)
 */
bool do_cmd_rest(struct player *p, s16b resting)
{
    /*
     * A little sanity checking on the input - only the specified negative
     * values are valid.
     */
    if ((resting < 0) && !player_resting_is_special(resting))
        return true;

    /* Check energy */
    if (!has_energy(p, true)) return false;

    /* Do some upkeep on the first turn of rest */
    if (!player_is_resting(p))
    {
        /* Disturb us: reset running if needed */
        disturb(p, 1);

        /* Redraw the state */
        p->upkeep->redraw |= (PR_STATE);
    }

    /* Set the counter, and stop if told to */
    player_resting_set_count(p, resting);
    if (!player_is_resting(p)) return true;

    /* Take a turn */
    player_resting_step_turn(p);

    /* Redraw the state if requested */
    handle_stuff(p);

    /* Prepare to continue, or cancel and clean up */
    if (player_resting_count(p) > 0)
        cmd_rest(p, resting - 1);
    else if (player_resting_is_special(resting))
        cmd_rest(p, resting);
    else
        player_resting_cancel(p, false);

    return true;
}


/*
 * Spend a turn doing nothing
 */
void do_cmd_sleep(struct player *p)
{
    /* Take a turn */
    if (has_energy(p, true)) use_energy(p);
}


/*
 * Array of feeling strings for object feelings.
 * Keep strings at 36 or less characters to keep the
 * combined feeling on one row.
 */
static const char *obj_feeling_text[][2] =
{
    {"this looks like any other level.", "this place looks familiar."},
    {"you sense an item of wondrous power!", "there was an item of wondrous power here!"},
    {"there are superb treasures here.", "there were superb treasures here."},
    {"there are excellent treasures here.", "there were excellent treasures here."},
    {"there are very good treasures here.", "there were very good treasures here."},
    {"there are good treasures here.", "there were good treasures here."},
    {"there may be something worthwhile here.", "there was something worthwhile here."},
    {"there may not be much interesting here.", "there was nothing interesting here."},
    {"there aren't many treasures here.", "there weren't many treasures here."},
    {"there are only scraps of junk here.", "there were only scraps of junk here."},
    {"there are naught but cobwebs here.", "there were naught but cobwebs here."}
};


/*
 * Array of feeling strings for monster feelings.
 * Keep strings at 36 or less characters to keep the
 * combined feeling on one row.
 */
static const char *mon_feeling_text[][2] =
{
    {"You are still uncertain about this place", "You are still uncertain about this place"},
    {"Omens of death haunt this place", "Omens of death haunted this place"},
    {"This place seems murderous", "This place seemed murderous"},
    {"This place seems terribly dangerous", "This place seemed terribly dangerous"},
    {"You feel anxious about this place", "You were anxious about this place"},
    {"You feel nervous about this place", "You were nervous about this place"},
    {"This place does not seem too risky", "This place did not seem too risky"},
    {"This place seems reasonably safe", "This place seemed reasonably safe"},
    {"This seems a tame, sheltered place", "This seemed a tame, sheltered place"},
    {"This seems a quiet, peaceful place", "This seemed a quiet, peaceful place"}
};


/*
 * Display the feeling. Players always get a monster feeling.
 * Object feelings are delayed until the player has explored some
 * of the level.
 *
 * Players entering a static level will get a different message, since
 * the feeling may not be accurate anymore.
 */
void display_feeling(struct player *p, bool obj_only)
{
    s16b obj_feeling;
    s16b mon_feeling;
    const char *join;
    byte set = 0;

    /* Don't show feelings for cold-hearted characters */
    if (!OPT(p, birth_feelings)) return;

    /* No feeling in towns */
    if (forbid_town(&p->wpos))
    {
        msg(p, "Looks like a typical town.");
        return;
    }

    /* Hack -- special levels */
    if (special_level(&p->wpos))
    {
        msg(p, "Looks like a special level.");
        return;
    }

    /* No feeling in the wilderness */
    if (p->wpos.depth == 0)
    {
        msg(p, "Looks like a typical wilderness level.");
        return;
    }

    /* Hack -- player entering a static level */
    if (p->feeling < 0)
    {
        int i, obj_f, mon_f;

        obj_feeling = 0;
        mon_feeling = 0;

        /* Get a feeling from other players */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *q = player_get(i);

            if (q == p) continue;
            if (!wpos_eq(&q->wpos, &p->wpos)) continue;
            if (q->feeling < 0) continue;

            obj_f = q->feeling / 10;
            mon_f = q->feeling - (10 * obj_f);

            if (obj_f > obj_feeling) obj_feeling = obj_f;
            if (mon_f > mon_feeling) mon_feeling = mon_f;
        }

        /* Display a different message */
        set = 1;
    }
    else
    {
        obj_feeling = p->feeling / 10;
        mon_feeling = p->feeling - (10 * obj_feeling);
    }

    /* Verify the feeling */
    if (obj_feeling >= N_ELEMENTS(obj_feeling_text))
        obj_feeling = N_ELEMENTS(obj_feeling_text) - 1;

    /* Display only the object feeling when it's first discovered. */
    if (obj_only)
    {
        disturb(p, 0);
        msg(p, "You feel that %s", obj_feeling_text[obj_feeling][set]);
        p->obj_feeling = obj_feeling;
        return;
    }

    /* Verify the feeling */
    if (mon_feeling >= N_ELEMENTS(mon_feeling_text))
        mon_feeling = N_ELEMENTS(mon_feeling_text) - 1;

    /* Players automatically get a monster feeling. */
    if (p->cave->feeling_squares < z_info->feeling_need)
    {
        msg(p, "%s.", mon_feeling_text[mon_feeling][set]);
        p->mon_feeling = mon_feeling;
        return;
    }

    /* Decide the conjunction */
    if (((mon_feeling <= 5) && (obj_feeling > 6)) || ((mon_feeling > 5) && (obj_feeling <= 6)))
        join = ", yet";
    else
        join = ", and";

    /* Display the feeling */
    msg(p, "%s%s %s", mon_feeling_text[mon_feeling][set], join, obj_feeling_text[obj_feeling][set]);
    p->obj_feeling = obj_feeling;
    p->mon_feeling = mon_feeling;
}


/*
 * Check if the player has enough funds to buy a house.
 * The player is allowed to buy a small house (value < 10000 au) with a deed of property.
 */
static int can_buy_house(struct player *p, int price)
{
    int i;

    /* Check for deeds of property */
    if (price < 10000)
    {
        for (i = 0; i < z_info->pack_size; i++)
        {
            struct object *obj = p->upkeep->inven[i];

            if (!obj) continue;

            if (tval_is_deed(obj))
            {
                use_object(p, obj, 1, true);

                /* Used a deed of property */
                return 1;
            }
        }
    }

    /* Check for enough funds */
    if (price > p->au)
    {
        /* Not enough money, message */
        msg(p, "You do not have enough money.");
        return 0;
    }

    /* Actually bought the house */
    return 2;
}


/*
 * Buy a house. It is assumed that the player already knows the price.
 * Hacked to sell houses for half price.
 */
void do_cmd_purchase_house(struct player *p, int dir)
{
    int i, n, check;
    struct chunk *c = chunk_get(&p->wpos);

    /* Ghosts cannot buy houses */
    if (p->ghost && !(p->dm_flags & DM_HOUSE_CONTROL))
    {
        /* Message */
        msg(p, "You cannot buy a house.");
        return;
    }

    /* Restricted by choice */
    if (cfg_no_stores || OPT(p, birth_no_stores))
    {
        /* Message */
        msg(p, "You cannot buy a house.");
        return;
    }

    /* Check preventive inscription '^h' */
    if (check_prevent_inscription(p, INSCRIPTION_HOUSE))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Check for no-direction -- confirmation (when selling house) */
    if (!dir)
    {
        struct house_type *house;

        i = p->current_house;
        p->current_house = -1;

        if (i == -1)
        {
            /* No house, message */
            msg(p, "You see nothing to sell there.");
            return;
        }

        house = house_get(i);

        /* Check for already-owned house */
        if (house->ownerid > 0)
        {
            /* See if he owns the house */
            if (house_owned_by(p, i))
            {
                /* Get the money */
                if (house->free)
                    msg(p, "You reset your house.");
                else
                {
                    msg(p, "You sell your house for %ld gold.", house->price / 2);
                    p->au += house->price / 2;
                }

                /* House is no longer owned */
                reset_house(i);

                /* Redraw */
                p->upkeep->redraw |= (PR_INVEN | PR_GOLD);

                /* Done */
                return;
            }

            /* The DM can reset a house */
            if (p->dm_flags & DM_HOUSE_CONTROL)
            {
                /* House is no longer owned */
                reset_house(i);
                msg(p, "The house has been reset.");

                /* Done */
                return;
            }
        }

        /* Message */
        msg(p, "You don't own this house.");

        /* No sale */
        return;
    }

    /* Be sure we have a direction */
    if (VALID_DIR(dir))
    {
        struct house_type *house;
        struct loc grid;

        /* Get requested direction */
        next_grid(&grid, &p->grid, dir);

        /* Check for a house */
        if ((i = pick_house(&p->wpos, &grid)) == -1)
        {
            /* No house, message */
            msg(p, "You see nothing to buy there.");
            return;
        }

        /* The door needs to be closed! */
        if (square_home_isopendoor(c, &grid))
        {
            /* Open door, message */
            msg(p, "You need to close the door first...");
            return;
        }

        /* Not inside the house! */
        if (house_inside(p, i))
        {
            /* Instead we allow players to look inside their own stores */
            if (house_owned_by(p, i))
                do_cmd_store(p, i);
            else
                msg(p, "You need to stand outside of the house first...");
            return;
        }

        house = house_get(i);

        /* Check for already-owned house */
        if (house->ownerid > 0)
        {
            /* See if he owns the house */
            if (house_owned_by(p, i))
            {
                /* Delay house transaction */
                p->current_house = i;

                /* Tell the client about the price */
                Send_store_sell(p, (house->free? 0: house->price / 2), false);

                /* Done */
                return;
            }

            /* The DM can reset a house */
            if (p->dm_flags & DM_HOUSE_CONTROL)
            {
                /* Delay house transaction */
                p->current_house = i;

                /* Tell the client about the reset */
                Send_store_sell(p, 0, true);

                /* Done */
                return;
            }

            /* Message */
            msg(p, "That house is already owned.");

            /* No sale */
            return;
        }

        /* The DM cannot buy houses! */
        if (p->dm_flags & DM_HOUSE_CONTROL)
        {
            /* Message */
            msg(p, "You cannot buy a house.");
            return;
        }

        /* Check already owned houses */
        n = houses_owned(p);

        /* Can we buy a house? */
        if (n == 2)
        {
            /* Too many houses, message */
            msg(p, "You cannot buy more houses.");
            return;
        }

        /* Check for funds or deed of property */
        check = can_buy_house(p, house->price);
        if (!check) return;

        /* Open the door */
        square_open_homedoor(c, &grid);

        /* Take some of the player's money (if the house was bought) */
        if (check == 2) p->au -= house->price;
        else house->free = 1;

        /* The house is now owned */
        set_house_owner(p, house);

        /* Redraw */
        if (check == 2) p->upkeep->redraw |= (PR_GOLD);
    }
}


/*
 * "Cutting" bonus based on weapon efficiency against trees
 */
int wielding_cut(struct player *p)
{
    struct object *obj = equipped_item_by_slot_name(p, "weapon");

    /* Skip empty weapon slot */
    if (!obj) return 0;

    /* Weapon type */
    return tval_wielding_cut(obj);
}


/*
 * MAngband house creation code
 *
 * Disabled for now, as it is incomplete:
 *   - confuses the player owned shop mechanism
 *   - allows awkward door placements
 *   - doesn't check for other houses proximity (especially house doors!)
 *   - sets a price of 0 on new houses
 */


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
static bool create_house_door(struct player *p, struct chunk *c, struct loc *grid)
{
    int house, lastmatch;

    /* Which house is the given location part of? */
    lastmatch = 0;
    house = find_house(p, grid, lastmatch);
    while (house >= 0)
    {
        struct house_type *h_ptr = house_get(house);

        /* Do we own this house? */
        if (!house_owned_by(p, house))
        {
            /* If we don't own this one, we can't own any overlapping ones */
            msg(p, "You do not own this house.");

            return false;
        }

        /* Does it already have a door? */
        if (loc_is_zero(&h_ptr->door))
        {
            /* No door, so create one! */
            loc_copy(&h_ptr->door, grid);
            square_colorize_door(c, grid, 0);
            msg(p, "You create a door for your house!");

            return true;
        }

        /* Check next house */
        lastmatch = house + 1;
        house = find_house(p, grid, lastmatch);
    }

    /* We searched all matching houses and none needed a door */
    msg(p, "You can't do that here.");

    return false;
}


/*
 * Determine if the given location is ok to use as part of the foundation
 * of a house.
 */
static bool is_valid_foundation(struct player *p, struct chunk *c, struct loc *grid)
{
    struct object *obj = square_object(c, grid);

    /* Foundation stones are always valid */
    if (obj)
    {
        if (tval_is_stone(obj) && !obj->next) return true;
        return false;
    }

    /*
     * Perma walls and doors are valid if they are part of a house owned
     * by this player
     */
    if (square_ispermhouse(c, grid) || square_home_iscloseddoor(c, grid))
    {
        int house;

        /* Looks like part of a house, which house? */
        house = find_house(p, grid, 0);
        if (house >= 0)
        {
            /* Do we own this house? */
            if (house_owned_by(p, house))
            {
                /* Valid, a wall or door in our own house. */
                return true;
            }
        }
    }

    return false;
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
 * In this situation:
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
 * and work our way outwards to find the bounding rectangle of the foundation.
 * Conceptually imagine a box radiating out from the player, we keep extending
 * the box in each dimension for as long as all points on the perimeter are
 * either foundation stones or walls of houses the player owns.
 *
 */
static bool get_house_foundation(struct player *p, struct chunk *c, struct loc *grid1,
    struct loc *grid2)
{
    int x, y;
    bool done = false;
    bool n, s, e, w, ne, nw, se, sw;

    /* We must be standing on a house foundation */
    if (!is_valid_foundation(p, c, &p->grid))
    {
        msg(p, "There is no house foundation here.");
        return false;
    }

    /* Start from the players position */
    loc_copy(grid1, &p->grid);
    loc_copy(grid2, &p->grid);

    while (!done)
    {
        struct loc grid;

        n = s = e = w = ne = nw = se = sw = false;

        /* Could we expand north? */
        n = true;
        for (x = grid1->x; x <= grid2->x; x++)
        {
            /* Is this a valid location for part of our house? */
            loc_init(&grid, x, grid1->y - 1);
            if (!is_valid_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                n = false;

                break;
            }
        }

        /* Could we expand east? */
        e = true;
        for (y = grid1->y; y <= grid2->y; y++)
        {
            /* Is this a valid location for part of our house? */
            loc_init(&grid, grid2->x + 1, y);
            if (!is_valid_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                e = false;

                break;
            }
        }

        /* Could we expand south? */
        s = true;
        for (x = grid1->x; x <= grid2->x; x++)
        {
            /* Is this a valid location for part of our house? */
            loc_init(&grid, x, grid2->y + 1);
            if (!is_valid_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                s = false;

                break;
            }
        }

        /* Could we expand west? */
        w = true;
        for (y = grid1->y; y <= grid2->y; y++)
        {
            /* Is this a valid location for part of our house? */
            loc_init(&grid, grid1->x - 1, y);
            if (!is_valid_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                w = false;

                break;
            }
        }

        /* Could we expand the corners? */
        loc_init(&grid, grid2->x + 1, grid1->y - 1);
        ne = is_valid_foundation(p, c, &grid);
        loc_init(&grid, grid1->x - 1, grid1->y - 1);
        nw = is_valid_foundation(p, c, &grid);
        loc_init(&grid, grid2->x + 1, grid2->y + 1);
        se = is_valid_foundation(p, c, &grid);
        loc_init(&grid, grid1->x - 1, grid2->y + 1);
        sw = is_valid_foundation(p, c, &grid);

        /*
         * Only permit expansion in a way that maintains a rectangle, we don't
         * want to create fancy polygons.
         */
        if (n) n = (!e && !w) || (e && ne) || (w && nw);
        if (e) e = (!n && !s) || (n && ne) || (s && se);
        if (s) s = (!e && !w) || (e && se) || (w && sw);
        if (w) w = (!n && !s) || (n && nw) || (s && sw);

        /* Actually expand the boundary */
        if (n) grid1->y--;
        if (s) grid2->y++;
        if (w) grid1->x--;
        if (e) grid2->x++;

        /* Stop if we couldn't expand */
        done = !(n || s || w || e);
    }

    /* Paranoia */
    x = grid2->x - grid1->x - 1;
    y = grid2->y - grid1->y - 1;
    if ((x <= 0) || (y <= 0))
    {
        msg(p, "The foundation should have positive dimensions!");
        return false;
    }

    /* No 1x1 house foundation */
    if ((x + y) < 3)
    {
        msg(p, "The foundation is too small!");
        return false;
    }

    /* Return the area */
    return true;
}


/*
 * Create a new house.
 * The creating player owns the house.
 */
bool create_house(struct player *p)
{
    int house;
    struct house_type h_local;
    struct chunk *c = chunk_get(&p->wpos);
    struct loc begin, end;
    struct loc_iterator iter;

    /* The DM cannot create houses! */
    if (p->dm_flags & DM_HOUSE_CONTROL)
    {
        msg(p, "You cannot create or extend houses.");
        return false;
    }

    /* Restricted by choice */
    if (cfg_no_stores || OPT(p, birth_no_stores))
    {
        msg(p, "You cannot create or extend houses.");
        return false;
    }

    /* Houses can only be created in the wilderness */
    if (!in_wild(&p->wpos))
    {
        msg(p, "This location is not suited for a house.");
        return false;
    }

    /* Determine the area of the house foundation */
    if (!get_house_foundation(p, c, &begin, &end)) return false;

    /* Is the location allowed? */
    /* XXX We should check if too near other houses, roads, level edges, etc */

    /* Get an empty house slot */
    house = house_add(true);

    /* Check maximum number of houses */
    if (house == -1)
    {
        msg(p, "The maximum number of houses has been reached.");
        return false;
    }

    /* Setup house info */
    loc_init(&h_local.grid_1, begin.x + 1, begin.y + 1);
    loc_init(&h_local.grid_2, end.x - 1, end.y - 1);
    loc_init(&h_local.door, 0, 0);
    memcpy(&h_local.wpos, &p->wpos, sizeof(struct worldpos));
    h_local.price = 0;   /* XXX */
    set_house_owner(p, &h_local);
    h_local.state = HOUSE_CUSTOM;

    /* Add a house to our houses list */
    house_set(house, &h_local);

    loc_iterator_first(&iter, &begin, &end);

    /* Render into the terrain */
    do
    {
        /* Delete any object */
        square_excise_pile(c, &iter.cur);

        /* Build a wall, but don't destroy any existing door */
        if (!square_home_iscloseddoor(c, &iter.cur))
            square_build_permhouse(c, &iter.cur);
    }
    while (loc_iterator_next(&iter));

    begin.x++;
    begin.y++;
    loc_iterator_first(&iter, &begin, &end);

    do
    {
        /* Fill with safe floor */
        square_add_safe(c, &iter.cur);

        /* Declare this to be a room */
        sqinfo_on(square(c, &iter.cur)->info, SQUARE_VAULT);
        sqinfo_on(square(c, &iter.cur)->info, SQUARE_ROOM);
    }
    while (loc_iterator_next_strict(&iter));

    return true;
}


/*
 * PWMAngband house creation code
 *
 * A simplified version of the MAngband house creation code:
 *   - only rectangular houses allowed with automatic (random) door placement
 *   - any foundation near an existing owned house extends that house automatically
 *   - house creation/extension forbidden near other houses
 *   - price set (or adjusted) according to house dimensions
 *   - player pays local tax equal to this price minus the amount already paid
 */


/*
 * Determine if the given location contains a house foundation stone.
 */
static bool is_foundation(struct player *p, struct chunk *c, struct loc *grid)
{
    struct object *obj;

    /* Paranoia */
    if (!square_in_bounds(c, grid)) return false;

    obj = square_object(c, grid);

    if (obj && tval_is_stone(obj) && !obj->next) return true;
    return false;
}


/*
 * Determine the area for a house foundation.
 */
static bool get_foundation_area(struct player *p, struct chunk *c, struct loc *begin,
    struct loc *end)
{
    int x, y, x1, x2, y1, y2, d;
    bool done = true;
    bool n, s, e, w, ne, nw, se, sw;

    /* We must NOT be standing on a house foundation stone */
    if (is_foundation(p, c, &p->grid))
    {
        msg(p, "You must stand outside the foundation perimeter.");
        return false;
    }

    /* Find a house foundation stone next to the player */
    for (d = 0; d < 8; d++)
    {
        struct loc grid;

        loc_sum(&grid, &p->grid, &ddgrid_ddd[d]);

        /* Oops */
        if (!square_in_bounds(c, &grid)) continue;

        if (is_foundation(p, c, &grid))
        {
            x1 = grid.x;
            x2 = grid.x;
            y1 = grid.y;
            y2 = grid.y;
            done = false;
            break;
        }
    }

    /* Didn't find any foundation stone next to the player */
    if (done)
    {
        msg(p, "You must stand next to the foundation perimeter.");
        return false;
    }

    do
    {
        struct loc grid;

        /* Could we expand north? */
        n = true;
        for (x = x1; x <= x2; x++)
        {
            loc_init(&grid, x, y1 - 1);

            /* Is this a valid location for part of our house? */
            if (!is_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                n = false;

                break;
            }
        }

        /* Could we expand east? */
        e = true;
        for (y = y1; y <= y2; y++)
        {
            loc_init(&grid, x2 + 1, y);

            /* Is this a valid location for part of our house? */
            if (!is_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                e = false;

                break;
            }
        }

        /* Could we expand south? */
        s = true;
        for (x = x1; x <= x2; x++)
        {
            loc_init(&grid, x, y2 + 1);

            /* Is this a valid location for part of our house? */
            if (!is_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                s = false;

                break;
            }
        }

        /* Could we expand west? */
        w = true;
        for (y = y1; y <= y2; y++)
        {
            loc_init(&grid, x1 - 1, y);

            /* Is this a valid location for part of our house? */
            if (!is_foundation(p, c, &grid))
            {
                /* Not a valid perimeter */
                w = false;

                break;
            }
        }

        /* Could we expand the corners? */
        loc_init(&grid, x2 + 1, y1 - 1);
        ne = is_foundation(p, c, &grid);
        loc_init(&grid, x1 - 1, y1 - 1);
        nw = is_foundation(p, c, &grid);
        loc_init(&grid, x2 + 1, y2 + 1);
        se = is_foundation(p, c, &grid);
        loc_init(&grid, x1 - 1, y2 + 1);
        sw = is_foundation(p, c, &grid);

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
    while (!done);

    /* Paranoia */
    x = x2 - x1 - 1;
    y = y2 - y1 - 1;
    if ((x <= 0) || (y <= 0))
    {
        msg(p, "The foundation should have positive dimensions!");
        return false;
    }

    /* No 1x1 house foundation */
    if ((x + y) < 3)
    {
        msg(p, "The foundation is too small!");
        return false;
    }

    /* Return the area */
    loc_init(begin, x1, y1);
    loc_init(end, x2, y2);

    return true;
}


/*
 * Determine if the area for a house foundation is allowed.
 */
static bool allowed_foundation_area(struct player *p, struct chunk *c, struct loc *begin,
    struct loc *end)
{
    int x, y;
    struct loc grid1, grid2, grid3, grid4;

    loc_init(&grid1, begin->x - 1, begin->y - 1);
    loc_init(&grid2, end->x + 1, begin->y - 1);
    loc_init(&grid3, begin->x - 1, end->y + 1);
    loc_init(&grid4, end->x + 1, end->y + 1);

    /* Check bounds (fully) */
    if (!square_in_bounds_fully(c, &grid1) || !square_in_bounds_fully(c, &grid2) ||
        !square_in_bounds_fully(c, &grid3) || !square_in_bounds_fully(c, &grid4))
    {
        msg(p, "You cannot create or extend houses near the level border.");
        return false;
    }

    /* Check north and south */
    for (x = begin->x; x <= end->x; x++)
    {
        loc_init(&grid1, x, begin->y - 1);
        loc_init(&grid2, x, end->y + 1);

        /* Check for house doors */
        if (square_home_iscloseddoor(c, &grid1) || square_home_iscloseddoor(c, &grid2))
        {
            msg(p, "You cannot create or extend houses near other house doors.");
            return false;
        }
    }

    /* Check east and west */
    for (y = begin->y; y <= end->y; y++)
    {
        loc_init(&grid1, begin->x - 1, y);
        loc_init(&grid2, end->x + 1, y);

        /* Check for house doors */
        if (square_home_iscloseddoor(c, &grid1) || square_home_iscloseddoor(c, &grid2))
        {
            msg(p, "You cannot create or extend houses near other house doors.");
            return false;
        }
    }

    return true;
}


/*
 * Create or extend a house.
 */
bool build_house(struct player *p)
{
    int x1, x2, y1, y2, house, area, price = 0, tax;
    struct house_type *h_ptr = NULL;
    struct chunk *c = chunk_get(&p->wpos);
    struct loc begin, end;
    struct loc_iterator iter;

    /* The DM cannot create or extend houses! */
    if (p->dm_flags & DM_HOUSE_CONTROL)
    {
        msg(p, "You cannot create or extend houses.");
        return false;
    }

    /* Restricted by choice */
    if (cfg_no_stores || OPT(p, birth_no_stores))
    {
        msg(p, "You cannot create or extend houses.");
        return false;
    }

    /* Houses can only be created in the wilderness */
    if (!in_wild(&p->wpos))
    {
        msg(p, "This location is not suited for a house.");
        return false;
    }

    /* PWMAngband: no house expansion in immediate suburbs */
    if (town_suburb(&p->wpos))
    {
        msg(p, "This location is not suited for a house.");
        return false;
    }

    /* Determine the area of the house foundation */
    if (!get_foundation_area(p, c, &begin, &end)) return false;

    /* Is the location allowed? */
    if (!allowed_foundation_area(p, c, &begin, &end)) return false;

    /* Is it near a house we own? */
    house = house_near(p, &begin, &end);
    switch (house)
    {
        /* Invalid dimensions */
        case -3:
        {
            msg(p, "You need a foundation with proper dimensions to extend that house.");
            return false;
        }

        /* House not owned */
        case -2:
        {
            msg(p, "You cannot create or extend houses near houses you don't own.");
            return false;
        }

        /* No house near: we can create one */
        case -1: break;

        /* A house has been found: we can extend it */
        default: h_ptr = house_get(house);
    }

    /* Local tax: we already paid this amount (cost of foundation) */
    tax = 0 - 1000 * (x2 - x1 + 1) * (y2 - y1 + 1);

    /* New dimensions */
    if (h_ptr)
    {
        x1 = MIN(x1, h_ptr->grid_1.x - 1);
        x2 = MAX(x2, h_ptr->grid_2.x + 1);
        y1 = MIN(y1, h_ptr->grid_1.y - 1);
        y2 = MAX(y2, h_ptr->grid_2.y + 1);
    }

    /* Remember price */
    area = (x2 - x1 - 1) * (y2 - y1 - 1);
    if (area > 40) price = (area - 40) * (area - 40) * (area - 40) * 3;
    price += area * area * 33;
    price += area * (900 + randint0(200));

    /* Local tax: price minus amount already paid */
    tax += price;
    if (h_ptr) tax -= h_ptr->price;
    if (tax < 0) tax = 0;

    /* Check for enough funds */
    if (tax > p->au)
    {
        /* Not enough money, message */
        msg(p, "You do not have enough money to pay the local tax of %ld gold.", tax);
        return false;
    }

    /* Extend an existing house */
    if (h_ptr)
    {
        /* Check maximum number of houses */
        if (!house_extend())
        {
            msg(p, "The maximum number of houses has been reached.");
            return false;
        }
    }

    /* Create a new house */
    else
    {
        /* Get an empty house slot */
        house = house_add(true);

        /* Check maximum number of houses */
        if (house == -1)
        {
            msg(p, "The maximum number of houses has been reached.");
            return false;
        }
    }

    /* Take some of the player's money */
    p->au -= tax;

    /* Redraw */
    p->upkeep->redraw |= (PR_GOLD);

    loc_init(&begin, x1, y1);
    loc_init(&end, x2, y2);
    loc_iterator_first(&iter, &begin, &end);

    /* Build a rectangular building */
    do
    {
        /* Delete any object */
        square_excise_pile(c, &iter.cur);

        /* Build a wall, but don't destroy any existing door */
        if (!square_home_iscloseddoor(c, &iter.cur))
            square_build_permhouse(c, &iter.cur);
    }
    while (loc_iterator_next(&iter));

    loc_init(&begin, x1 + 1, y1 + 1);
    loc_iterator_first(&iter, &begin, &end);

    /* Make it hollow */
    do
    {
        /* Fill with safe floor */
        square_add_safe(c, &iter.cur);

        /* Declare this to be a room */
        sqinfo_on(square(c, &iter.cur)->info, SQUARE_VAULT);
        sqinfo_on(square(c, &iter.cur)->info, SQUARE_ROOM);
    }
    while (loc_iterator_next_strict(&iter));

    /* Finish house creation */
    if (!h_ptr)
    {
        struct house_type h_local;
        int tmp;
        struct loc door;

        /* Pick a door direction (S,N,E,W) */
        tmp = randint0(4);

        /* Extract a "door location" */
        switch (tmp)
        {
            /* Bottom side */
            case DIR_SOUTH:
            {
                door.y = y2;
                door.x = rand_range(x1, x2);
                break;
            }

            /* Top side */
            case DIR_NORTH:
            {
                door.y = y1;
                door.x = rand_range(x1, x2);
                break;
            }

            /* Right side */
            case DIR_EAST:
            {
                door.y = rand_range(y1, y2);
                door.x = x2;
                break;
            }

            /* Left side */
            default:
            {
                door.y = rand_range(y1, y2);
                door.x = x1;
                break;
            }
        }

        /* Add the door */
        square_colorize_door(c, &door, 0);

        /* Setup house info */
        loc_init(&h_local.grid_1, x1 + 1, y1 + 1);
        loc_init(&h_local.grid_2, x2 - 1, y2 - 1);
        loc_copy(&h_local.door, &door);
        memcpy(&h_local.wpos, &p->wpos, sizeof(struct worldpos));
        h_local.price = price;
        set_house_owner(p, &h_local);
        h_local.state = HOUSE_CUSTOM;

        /* Add a house to our houses list */
        house_set(house, &h_local);

        /* Update the visuals */
        update_visuals(&p->wpos);

        return true;
    }

    /* Adjust some house info */
    loc_init(&h_ptr->grid_1, x1 + 1, y1 + 1);
    loc_init(&h_ptr->grid_2, x2 - 1, y2 - 1);
    h_ptr->price = price;
    h_ptr->state = HOUSE_EXTENDED;

    /* Update the visuals */
    update_visuals(&p->wpos);

    return true;
}


/*
 * Display current time (of the day).
 */
void display_time(struct player *p)
{
    int day = 5 * z_info->day_length;
    int hour = 1 + (turn.turn % day) * 12 / day;
    const char *suffix = "th";

    if (hour == 1) suffix = "st";
    else if (hour == 2) suffix = "nd";
    else if (hour == 3) suffix = "rd";

    msg(p, "It is the %d%s hour of the %s.", hour, suffix, (is_daytime()? "day": "night"));
}
