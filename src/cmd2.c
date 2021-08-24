/* File: cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Movement commands (part 2) */

#include "angband.h"
#include "dun.h"

#include <assert.h>

/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
    dun_cell_ptr cell = dun_cell_at(cave, plr->pos);

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    if (!stairs_go_up(cell))
    {
        msg_print("I see no up staircase here.");
        return;
    }

    if (!quests_check_leave()) return;
    energy_use = 100;
    if (!dun_take_stairs_plr(cave))
        msg_print("The stairs are blocked!");
}

/*
 * Enter quest level
 */
void do_cmd_quest(void)
{
    dun_cell_ptr cell = dun_cell_at(cave, plr->pos);

    energy_use = 100;
    if (!stairs_enter_quest(cell))
    {
        msg_print("You see no quest level here.");
        return;
    }
    else
    {
        int quest_id = stairs_quest_id(cell);
        quest_ptr quest = quests_get(quest_id);

        if (quest->status != QS_TAKEN) /* paranoia */
        {
            msg_print("The entrance is blocked!");
            return;
        }

        msg_format("This is the entrance to the quest: <color:B>%s</color>.",
            quests_get_name(quest_id));
        if (!get_check("Do you enter? ")) return;

        dun_take_stairs_plr(cave);
    }
}

/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
    dun_cell_ptr cell = dun_cell_at(cave, plr->pos);
    bool fall_trap = floor_has_trapdoor(cell);

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    if (!stairs_go_down(cell) && !fall_trap)
    {
        msg_print("I see no down staircase here.");
        return;
    }

    if (stairs_enter_quest(cell))
        do_cmd_quest();

    else
    {
        int target_dungeon = 0;
        if (cave->type->id == D_SURFACE)
        {
            dun_type_ptr dun_type;
            assert(stairs_enter_dungeon(cell));
            target_dungeon = stairs_dun_type_id(cell);
            dun_type = dun_types_lookup(target_dungeon);
            if (!dun_type->plr_max_lvl)
            {
                if (dun_type->flags.info & DF_RANDOM)
                    msg_format("This is the entrance of %s (Danger level: ?)", dun_type->name);
                else
                    msg_format("This is the entrance of %s (Danger level: %d)", dun_type->name, dun_type->min_dun_lvl);
                if (!get_check("Do you really go into this dungeon? ")) return;
            }
            plr->old_pos = plr->pos; /* remember for return recall */
        }

        energy_use = 100;

        if (fall_trap)
        {
            msg_print("You deliberately jump through the trap door.");
            dun_trap_door_plr(cave);
        }
        else if (dun_take_stairs_plr(cave))
        {
            if (target_dungeon)
            {
                assert(cave->type->id == target_dungeon);
                if (cave->type->enter_f)
                    cave->type->enter_f(cave->type);
                else msg_format("You entered %s.", cave->type->desc);
            }
        }
        else msg_print("The stairs are blocked!");
    }
}


/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        plr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Take a turn */
    energy_use = 100;

    /* Search */
    if (search() != ACTION_CONTINUE)
        disturb(0, 0);
}

#if defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) /* TNB */

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_adjacent(point_ptr out_pos, dun_cell_p test, bool under)
{
    int d, count = 0;

    for (d = 0; d < 9; d++)
    {
        point_t pos = point_step(plr->pos, ddd[d]);
        dun_cell_ptr cell = dun_cell_at(cave, pos);

        /* if not searching under player continue */
        if (!under && point_equals(pos, plr->pos)) continue;

        /* Must have knowledge */
        if (!(cell->flags & CELL_MAP)) continue;

        assert(test);
        if (!test(cell)) continue;

        ++count;
        *out_pos = pos;
    }

    /* All done */
    return count;
}


/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
static obj_ptr next_chest(point_t pos, obj_p filter)
{ /* cf chest_is_locked or chest_has_known_trap ... or chest_is_not_empty */
    obj_ptr obj;
    for (obj = dun_obj_at(cave, pos); obj; obj = obj->next)
    {
        if (!obj_is_chest(obj)) continue;
        if (!filter || filter(obj)) return obj;
    }
    return NULL;
}
static int count_chests(point_ptr out_pos, bool trapped)
{
    int d, count = 0;

    for (d = 0; d < 9; d++)
    {
        point_t pos = point_step(plr->pos, ddd[d]);
        obj_ptr chest = next_chest(pos, chest_is_not_empty);

        if (!chest) continue;
        if (trapped && !chest_has_known_trap(chest))
            continue;

        ++count;
        *out_pos = pos;
    }
    return count;
}

#endif /* defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) -- TNB */


/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
    int dir, rc = ACTION_SUCCESS;
    point_t pos = {0};

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

#ifdef ALLOW_EASY_OPEN /* TNB */
    /* Option: Pick a direction */
    if (easy_open)
    {
        int num_doors = count_adjacent(&pos, door_is_closed, FALSE);
        int num_chests = count_chests(&pos, FALSE);

        if (num_doors + num_chests == 1)
            command_dir = point_step_dir(plr->pos, pos);
    }
#endif /* ALLOW_EASY_OPEN -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        plr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir, TRUE))
    {
        mon_ptr mon;
        obj_ptr chest;

        pos = point_step(plr->pos, dir);
        chest = next_chest(pos, chest_is_not_empty);
        mon = dun_mon_at(cave, pos);

        if (mon && mon->id != plr->riding) /* XXX trying to open chest under foot with dir = 5 */
        {
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }
        else if (chest)
        {
            rc = chest_open(chest);
            if (rc != ACTION_ABORT)
                energy_use = 100;
        }
        else
        {
            rc = dun_open(cave, pos, 0);
            if (rc != ACTION_ABORT)
                energy_use = 100;
        }
    }

    /* Cancel repeat unless we may continue */
    if (rc != ACTION_CONTINUE)
        disturb(0, 0);
}

/*
 * Close an open door.
 */
static bool _door_can_close(dun_cell_ptr cell) { return door_is_open(cell) || curtain_is_open(cell); }
void do_cmd_close(void)
{
    int dir, rc = ACTION_SUCCESS;
    point_t pos = {0};

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

#ifdef ALLOW_EASY_OPEN /* TNB */
    /* Option: Pick a direction */
    if (easy_open)
    {
        if (count_adjacent(&pos, _door_can_close, FALSE) == 1)
            command_dir = point_step_dir(plr->pos, pos);
    }
#endif /* ALLOW_EASY_OPEN -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        plr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        point_t pos = point_step(plr->pos, dir);
        mon_ptr mon = dun_mon_at(cave, pos);

        if (mon)
        {
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }
        else
        {
            rc = dun_close(cave, pos, 0);
            if (rc != ACTION_ABORT)
                energy_use = 100;
        }
    }

    /* Cancel repeat unless we may continue */
    if (rc != ACTION_CONTINUE)
        disturb(0, 0);
}

/*
 * Tunnels through "walls" (including rubble and closed doors)
 *
 * Note that you must tunnel in order to hit invisible monsters
 * in walls, though moving into walls still takes a turn anyway.
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void do_cmd_tunnel(void)
{
    int  dir, rc = ACTION_ABORT;

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1;
        plr->redraw |= (PR_STATE);
        command_arg = 0;
    }

    /* Get a direction to tunnel, or Abort */
    if (get_rep_dir(&dir, FALSE))
    {
        point_t pos = point_step(plr->pos, dir);
        mon_ptr mon = dun_mon_at(cave, pos);

        if (mon)
        {
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }
        else
        {
            rc = dun_tunnel(cave, pos, 0);
            if (rc != ACTION_ABORT)
                energy_use = 100;
        }
    }

    /* Cancel repetition unless we can continue */
    if (rc != ACTION_CONTINUE)
        disturb(0, 0);
}

/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(void)
{
    int dir, rc = ACTION_ABORT;

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

#ifdef ALLOW_EASY_DISARM /* TNB */
    /* Option: Pick a direction */
    if (easy_disarm)
    {
        point_t pos = {0};
        int num_traps = count_adjacent(&pos, floor_has_known_trap, FALSE);
        int num_chests = count_chests(&pos, TRUE);

        if (num_traps + num_chests == 1)
            command_dir = point_step_dir(plr->pos, pos);
    }
#endif /* ALLOW_EASY_DISARM -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1;
        plr->redraw |= (PR_STATE);
        command_arg = 0;
    }

    /* Get a direction (or abort) */
    if (get_rep_dir(&dir, TRUE))
    {
        point_t pos = point_step(plr->pos, dir);
        mon_ptr mon = dun_mon_at(cave, pos);
        obj_ptr chest = next_chest(pos, chest_has_known_trap);

        if (mon && mon->id != plr->riding)
        {
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }
        else if (chest)
        {
            rc = chest_disarm(chest);
            if (rc != ACTION_ABORT)
                energy_use = 100;
        }
        else
        {
            rc = dun_disarm(cave, pos, 0);
            if (rc != ACTION_ABORT)
            {
                energy_use = 100;
                if (rc != ACTION_CONTINUE && cell_allow_plr(dun_cell_at(cave, pos)))
                    move_player_effect(pos, 0);
            }
        }
    }

    /* Cancel repeat unless told not to */
    if (rc != ACTION_CONTINUE)
        disturb(0, 0);
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
void do_cmd_bash(void)
{
    int  dir, rc = ACTION_ABORT;

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1;
        plr->redraw |= (PR_STATE);
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        point_t pos = point_step(plr->pos, dir);
        mon_ptr mon = dun_mon_at(cave, pos);

        if (mon)
        {
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }
        else
        {
            rc = dun_bash(cave, pos, 0);
            if (rc != ACTION_ABORT)
                energy_use = 100;
        }
    }

    /* Unless valid action taken, cancel bash */
    if (rc != ACTION_CONTINUE)
        disturb(0, 0);
}


/*
 * Manipulate an adjacent grid in some way
 *
 * Attack monsters, tunnel through walls, disarm traps, open doors.
 *
 * Consider confusion XXX XXX XXX
 *
 * This command must always take a turn, to prevent free detection
 * of invisible monsters.
 */
void do_cmd_alter(void)
{
    int  dir, rc = ACTION_ABORT;

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1;
        plr->redraw |= (PR_STATE);
        command_arg = 0;
    }

    /* Get a direction */
    if (get_rep_dir(&dir,TRUE))
    {
        point_t      pos = point_step(plr->pos, dir);
        dun_cell_ptr cell = dun_cell_at(cave, pos);
        mon_ptr      mon = dun_mon_at(cave, pos);

        energy_use = 100;
        rc = ACTION_SUCCESS;

        /* Attack monsters */
        if (mon)
            plr_attack_normal(pos);

        /* Bash jammed doors ... note jammed counts as 'closed' but dun_open will warn */
        else if (door_is_jammed(cell))
            rc = dun_bash(cave, pos, 0);

        /* Locked|closed doors */
        else if (door_is_closed(cell) || curtain_is_closed(cell))
            rc = dun_open(cave, pos, 0);
            
        /* Tunnel through walls */
        else if (cell->type == FEAT_WALL || cell->type == FEAT_TREE)
            rc = dun_tunnel(cave, pos, 0);

        /* Close open doors */
        else if (_door_can_close(cell))
            rc = dun_close(cave, pos, 0);

        /* Disarm traps */
        else if (floor_has_known_trap(cell))
        {
            rc = dun_disarm(cave, pos, 0);
            if (rc == ACTION_SUCCESS || rc == ACTION_FAIL)
            {
                if (cell_allow_plr(dun_cell_at(cave, pos)))
                    move_player_effect(pos, 0);
            }
        }
        /* Oops */
        else
            msg_print("You attack the empty air.");

        if (rc == ACTION_ABORT) /* not sure this is possible ... */
            energy_use = 0;
    }

    /* Cancel repetition unless we can continue */
    if (rc != ACTION_CONTINUE)
        disturb(0, 0);
}

/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike(void)
{
    int dir, rc = ACTION_ABORT;

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    if (get_rep_dir(&dir,FALSE))
    {
        point_t pos = point_step(plr->pos, dir);
        mon_ptr mon = dun_mon_at(cave, pos);

        if (mon)
        {
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }
        else
        {
            rc = dun_jam(cave, pos, 0);
            if (rc != ACTION_ABORT)
                energy_use = 100;
        }
    }
}


/*
 * Support code for the "Walk" and "Jump" commands
 */
static void do_cmd_walk_aux(int dir, bool pickup)
{
    /* Take a turn */
    energy_use = 100;

    if (dir != 5 && (plr->special_defense & KATA_MUSOU))
        set_action(ACTION_NONE);

    if (plr->action == ACTION_QUICK_WALK) energy_use = energy_use * (45-(plr->lev/2)) / 100;
    if (plr->action == ACTION_STALK) energy_use = energy_use * (150 - plr->lev) / 100;
    if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
        energy_use = energy_use * (45-(plr->lev/2)) / 100;

    if (plr->quick_walk)
        energy_use = energy_use * 60 / 100;

    if (prace_is_(RACE_MON_GOLEM))
        energy_use *= 2;

    move_player(dir, pickup, FALSE);
}

void do_cmd_walk(bool pickup)
{
    int dir;
    bool more = FALSE;


    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1;
        plr->redraw |= (PR_STATE);
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        do_cmd_walk_aux(dir, pickup);
        more = TRUE;
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(0, 0);
}

/*
 * Start running. For confused or randomly moving
 * players, we disallow running in a random direction.
 * They will simply stumble a single step instead.
 */
void do_cmd_run(void)
{
    int dir;

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    switch (get_rep_dir(&dir,FALSE))
    {
    case GET_DIR_OK:
        running = (command_arg ? command_arg : 5000);
        run_step(dir);
        break;
    case GET_DIR_RANDOM:
        do_cmd_walk_aux(dir, FALSE);
        break;
    }
}

/*
 * Stay still. Search. Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_stay(bool pickup)
{
    u32b mpe_mode = MPE_STAYING | MPE_ENERGY_USE;

    /* Allow repeated command */
    if (command_arg)
    {
        command_rep = command_arg - 1;
        plr->redraw |= (PR_STATE);
        command_arg = 0;
    }

    /* Take a turn */
    energy_use = 100;

    if (pickup) mpe_mode |= MPE_DO_PICKUP;
    move_player_effect(plr->pos, mpe_mode);
}

/* Get Object(s).
 * Historically, 'g' was 'stay still (flip pickup)' which makes little sense */
static bool _travel_next_obj(int mode)
{
    point_map_iter_ptr iter;
    int best_idx = -1, best_dist = 0, dist;
    point_t best_pos = {0};

    for (iter = point_map_iter_alloc(cave->obj_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        obj_ptr pile = point_map_iter_current(iter);
        point_t pos = point_map_iter_current_key(iter);
        obj_ptr obj;
        for (obj = pile; obj; obj = obj->next)
        {
            if (!(obj->marked & OM_FOUND)) continue;
            if (mode == TRAVEL_MODE_AMMO)
            {
                if (!obj->inscription) continue;
                if (!strstr(quark_str(obj->inscription), "=g")) continue;
                if (point_equals(plr->pos, pos))
                {
                    /* Full pack aborts the travel sequence */
                    return FALSE;
                }
            }
            else if (mode == TRAVEL_MODE_AUTOPICK && obj->tval != TV_GOLD)
            {
                int j = is_autopick(obj);

                if (j < 0) continue;
                if (!(autopick_list[j].action & (DO_AUTODESTROY | DO_AUTOPICK))) continue;
                if (point_equals(plr->pos, pos))
                {
                    /* Full pack aborts the travel sequence */
                    if (autopick_list[j].action & DO_AUTOPICK)
                        return FALSE;
                    continue; /* paranoia ... we should have destroyed this object */
                }
            }

            dist = point_distance(plr->pos, pos);
            if (!plr_project(pos))
            {
                if (mode == TRAVEL_MODE_AUTOPICK) continue;
                else if (dist > 18) continue;
            }

            if (best_idx == -1 || dist < best_dist)
            {
                best_idx = obj->loc.v.floor.obj_id;
                best_dist = dist;
                best_pos = pos;
            }
        }
    }
    point_map_iter_free(iter);
    if (best_idx == -1)
        return FALSE;
    travel_begin(mode, best_pos);
    return TRUE;
}
void do_cmd_get(void)
{
    if (!dun_obj_at(cave, plr->pos))
        msg_print("You see no objects here. Try <color:keypress>^G</color> to auto-get nearby objects.");
    if (pack_get_floor())
        energy_use = 100;
}
void do_cmd_autoget(void)
{
    /* Get any objects under foot first ... this is the old
     * 'g' behavior sans interaction with features (e.g. re-
     * enter a shop) */
    if (dun_obj_at(cave, plr->pos))
    {
        if (pack_get_floor())
            energy_use = 100;
        else /* Pack is full or the user canceled the easy_floor menu */
            return;
    }
    /* Now, auto pickup nearby objects by iterating
     * the travel command */
    if (auto_get_objects)
        _travel_next_obj(TRAVEL_MODE_AUTOPICK);
    else if (auto_get_ammo)
        _travel_next_obj(TRAVEL_MODE_AMMO);
    else
    {
        msg_print("<color:B>Warning:</color> You have specified neither the "
            "<color:keyword>auto_get_ammo</color> nor the <color:keyword>"
            "auto_get_objects</color> options. With neither option set, "
            "<color:keypress>^G</color> behaves just like the normal "
            "<color:keypress>g</color>et command.");
    }
}

/*
 * Resting allows a player to safely restore his hp    -RAK-
 */
void do_cmd_rest(void)
{
    int tmp;
    if (REPEAT_PULL(&tmp))
        command_arg = tmp;

    set_action(ACTION_NONE);
    if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
        weaponmaster_set_toggle(TOGGLE_NONE);

    stop_mouth();

    /* Prompt for time if needed */
    if (command_arg == 0)
    {
        cptr p = "<color:y>Rest</color> (0-9999, '*' for HP/SP, '&' as needed): ";


        char out_val[80];

        /* Default */
        strcpy(out_val, "&");

        /* Ask for duration
        if (!get_string(p, out_val, 4)) return;*/
        if (!msg_input(p, out_val, 4)) return;

        /* Rest until done */
        if (out_val[0] == '&')
        {
            command_arg = (-2);
        }

        /* Rest a lot */
        else if (out_val[0] == '*')
        {
            command_arg = (-1);
        }

        /* Rest some */
        else
        {
            command_arg = atoi(out_val);
            if (command_arg <= 0) return;
        }
        REPEAT_PUSH(command_arg);
    }


    /* Paranoia */
    if (command_arg > 9999) command_arg = 9999;

    /* Mimickry blocks all regeneration (hp and sp)!
       We can either block this command since resting will stop
       immediately (cf process_player() in dungeon.c) or, more
       conveniently, we can stop mimicry, though this will tend
       to expose the player!
     */
    if (mimic_no_regen() && command_arg < 0)
    {
        bool clear = TRUE;
        if (command_arg == -2)
        {
            if (plr_tim_find(T_BLIND) ||
                plr_tim_find(T_CONFUSED) ||
                plr_tim_find(T_POISON) ||
                plr->afraid ||
                plr_tim_find(T_STUN) ||
                plr_tim_find(T_CUT) ||
                plr_tim_find(T_SLOW) ||
                plr_tim_find(T_PARALYZED) ||
                plr_tim_find(T_HALLUCINATE))
            {
                clear = FALSE;
            }
        }

        /* XXX race_mimic? */
        if (clear)
            mimic_race(MIMIC_NONE, "You cannot rest while maintaining your current form.");
    }

    /* XXX We have issues with the new lighting system. plr->hooks.move_player might seem
     * a good place to handle ninja movement, but lighting is not up to date when this is
     * called. If we exit stealth here, then the next call to plr->hooks.update_light will
     * go back into stealth and abort the rest. This happens when updating monster light.
     * This needs thought. For now, allow the ninja to rest in the shadows.
     * cf PU_LIGHT | PU_MON_LIGHT as well as _update_light in ninja.c XXX */
    if (plr->special_defense & NINJA_S_STEALTH) set_superstealth(FALSE);

    /* Take a turn XXX XXX XXX (?) */
    energy_use = 100;

    /* The sin of sloth */
    if (command_arg > 100)
        virtue_add(VIRTUE_DILIGENCE, -1);

    /* Why are you sleeping when there's no need?  WAKE UP!*/
    if ((plr->chp == plr->mhp) &&
        (plr->csp == plr->msp) &&
        !plr_tim_find(T_BLIND) && !plr_tim_find(T_CONFUSED) &&
        !plr_tim_find(T_POISON) && !plr->afraid &&
        !plr_tim_find(T_STUN) && !plr_tim_find(T_CUT) &&
        !plr_tim_find(T_SLOW) && !plr_tim_find(T_PARALYZED) &&
        !plr_tim_find(T_HALLUCINATE) &&
        !magic_eater_can_regen())
    {
        virtue_add(VIRTUE_DILIGENCE, -1);
    }
    /* Save the rest code */
    resting = command_arg;
    plr->action = ACTION_REST;

    /* Recalculate bonuses */
    plr->update |= (PU_BONUS);

    /* Redraw the state */
    plr->redraw |= (PR_STATE);

    /* Handle stuff */
    handle_stuff();

    /* Refresh */
    Term_fresh();
}


/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
int breakage_chance(object_type *o_ptr)
{
    if (obj_is_art(o_ptr)) return 0;

    if (o_ptr->name2 == EGO_AMMO_ENDURANCE) return 0;
    if (o_ptr->name2 == EGO_AMMO_EXPLODING) return 100;

    /* Examine the item type */
    switch (o_ptr->tval)
    {
        /* Always break */
        case TV_FLASK:
        case TV_POTION:
        case TV_BOTTLE:
        case TV_FOOD:
        case TV_JUNK:
            return 100;

        /* Often break */
        case TV_LIGHT:
        case TV_SCROLL:
        case TV_SKELETON:
            return 50;

        /* Sometimes break */
        case TV_WAND:
        case TV_SPIKE:
            return 25;
        case TV_ARROW:
            return 20;

        /* Rarely break */
        case TV_SHOT:
        case TV_BOLT:
            return 10;
        default:
            return 10;
    }
}

/*
 * Player Travel to Fixed Location
 */
static bool _travel_flow_p(point_t pos, dun_grid_ptr grid)
{
    if (!(grid->flags & CELL_AWARE)) return FALSE; /* non-spoiling */
    if (floor_has_known_trap(grid)) return FALSE;      /* avoid unnecessary risks */
    /* XXX if (door_is_jammed(grid)) return FALSE; */

    if (grid->type == FEAT_WALL) return FALSE;
    if (cell_is_chasm(grid) && !plr->levitation) return FALSE;
    if (grid->type == FEAT_PATTERN) return FALSE;
    return TRUE;
}
static bool _travel_begin_direct(point_t pos)
{
    if (!travel.path) travel.path = point_vec_alloc();
    else point_vec_clear(travel.path);
    travel.path_idx = 0;

    if (point_fast_distance(plr->pos, pos) <= MAX_SIGHT)
    {
        point_t path[MAX_SIGHT];
        int     ct = project_path(path, MAX_SIGHT, plr->pos, pos, 0), i;

        if (ct > 0 && point_equals(path[ct - 1], pos))
        {
            /* A projection path never includes the start position. We'll include it
             * in our travel path for more robust error checking. travel.path[.path_idx]
             * should always be the the plr's position, and .path[.path_idx+1] is the 
             * next position in the travel. */
            assert(!point_equals(plr->pos, path[0]));
            point_vec_add(travel.path, plr->pos);

            for (i = 0; i < ct; i++)
            {
                point_t p = path[i];
                dun_grid_ptr g = dun_grid_at(cave, p);
                if (!_travel_flow_p(p, g))
                {
                    point_vec_clear(travel.path);
                    return FALSE;
                }
                point_vec_add(travel.path, p);
            }
            return TRUE;
        }
    }
    return FALSE;
}
static void _travel_remember(int mode, point_t new_pos)
{
    /* never erase last_pos during auto travel sequences */
    if (mode != TRAVEL_MODE_NORMAL)
    {
        /* stash current pos on first auto-travel leg
         * until entire auto-travel sequence is finished (travel_end) */
        if (travel.mode == TRAVEL_MODE_NORMAL)
            travel.temp_pos = travel.pos;
        return;
    }

    /* never remember auto-travel locations */
    if (travel.mode != TRAVEL_MODE_NORMAL) return;

    /* don't remember invalid locations */
    if (!dun_pos_interior(cave, travel.pos))
    {
        /* but first normal travel remembers start location! */
        if (!dun_pos_interior(cave, travel.last_pos))
            travel.last_pos = plr->pos;
        return;
    }
    if (point_equals(travel.last_pos, travel.pos)) return; /* XXX can't remember why ... */

    /* resumed an interrupted travel ... don't clobber last_pos */
    if (point_equals(travel.pos, new_pos)) return;

    travel.last_pos = travel.pos;
}
void travel_begin(int mode, point_t pos)
{
    assert(dun_pos_interior(cave, pos));

    if (point_equals(pos, plr->pos))
        return;

    if (_travel_begin_direct(pos))
    {
        assert(travel.path);
        assert(point_vec_length(travel.path));

        if (travel.flow) dun_flow_free(travel.flow);
        travel.flow = NULL;

        _travel_remember(mode, pos);
        travel.mode = mode;
        travel.pos = pos;
        travel.run = 255;
        travel.dir = 0;
        travel.path_idx = 0;
    }
    else
    {
        int i;
        int dx, dy, sx, sy;
        dun_grid_ptr grid;
        int rng = plr_on_surface() ? MAX(50, point_fast_distance(plr->pos, pos)) : 200;
        rect_t rect;


        rect = rect_create_centered(pos, rng, rng);
        if (!rect_contains_point(rect, plr->pos))
        {
            msg_print("That location is too far away.");
            return;
        }

        grid = dun_grid_at(cave, pos);
        if ((grid->flags & CELL_MAP) && grid->type == FEAT_WALL)
        {
            msg_print("You cannot travel there!");
            return;
        }

        if (travel.flow) dun_flow_free(travel.flow);
        travel.flow = NULL;

        _travel_remember(mode, pos);
        travel.mode = mode;
        travel.run = 0;
        travel.pos = pos;
        travel.flow = dun_flow_calc(cave, pos, rng, _travel_flow_p);
        travel.run = 255;
        travel.dir = 0;

        /* Decides first direction */
        dx = abs(plr->pos.x - pos.x);
        dy = abs(plr->pos.y - pos.y);
        sx = ((pos.x == plr->pos.x) || (dx < dy)) ? 0 : ((pos.x > plr->pos.x) ? 1 : -1);
        sy = ((pos.y == plr->pos.y) || (dy < dx)) ? 0 : ((pos.y > plr->pos.y) ? 1 : -1);

        for (i = 1; i <= 9; i++)
        {
            if ((sx == ddx[i]) && (sy == ddy[i])) travel.dir = i;
        }
    }
}

void travel_cancel(void)
{
    travel.run = 0;
    /* Don't twiddle with the mode here ... If you examine travel_step
     * you will see it manually undoes the effects of disturb() during
     * the move_player_effect(). It does this by remembering travel.run
     * and resetting it when all is done. So, if we were to clear the
     * mode here, then we would abort an intermediate stage in a travel
     * sequence (e.g. TRAVEL_MODE_AUTOPICK). It's OK to leave the mode
     * set after the travel is legitimately completed since nobody can
     * start a new travel without calling travel_begin(mode, ...).
     *
     * BTW, the autopicker is triggering the disturb during travel.
     * In our case, we are travelling just so the autopicker can do
     * its thing, and aborting would be a gross error.
     *
     * travel.mode = TRAVEL_MODE_NORMAL;*/
}

void travel_end(void)
{
    travel.run = 0;
    if (travel.mode != TRAVEL_MODE_NORMAL && _travel_next_obj(travel.mode))
    {
        /* paranoia ... but don't get stuck */
        if (point_equals(travel.pos, plr->pos))
            travel_cancel();
    }
    else
    {
        if (travel.mode != TRAVEL_MODE_NORMAL)
        {
            /* restore and forget "stashed" travel.pos */
            travel.pos = travel.temp_pos;
            travel.temp_pos = point_create(0, 0);
            travel.mode = TRAVEL_MODE_NORMAL;
        }
        if (center_player && !center_running) viewport_verify();
    }
}

void do_cmd_travel(void)
{
    point_t pos;
    if (dun_cell_at(cave, plr->pos)->type == FEAT_PATTERN)
    {
        /* XXX travel flow does not support walking the pattern. instead,
         * you will receive 255 messages saying you cannot leave the pattern! */
        msg_print("You may not travel while walking the pattern.");
        return;
    }
    pos = target_pos(-1);
    if (!dun_pos_interior(cave, pos)) return;
    travel_begin(TRAVEL_MODE_NORMAL, pos);
}


