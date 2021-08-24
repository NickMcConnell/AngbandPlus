/* File: was cmd2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "player_command.h"
#include "nppdialog.h"
#include "storedialog.h"
#include "src/cmds.h"
#include <QPushButton>

/*
 * Check if action permissible here.
 */
bool do_cmd_test(int y, int x, int action, bool do_message)
{
    u32b bitzero = 0x01;
    u32b flag;

    QString act = "";

    QString here = ((p_ptr->px == x ) && (p_ptr->py == y)) ? "here": "there";

    feature_type *f_ptr;

    /* Must have knowledge */
    if (!(dungeon_info[y][x].cave_info & (CAVE_MARK)))
    {
        /* Message */
        if (do_message) message(QString("You see nothing %1.") .arg(here));

        /* Nope */
        return (FALSE);
    }

    /* Get memorised feature */
    f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    switch (action)
    {
        case FS_SECRET:							break;
        case FS_OPEN:	act = " to open";		break;
        case FS_CLOSE:	act = " to close";		break;
        case FS_BASH:	act = " to bash";		break;
        case FS_SPIKE:	act = " to spike";		break;
        case FS_TUNNEL:	act = " to tunnel";		break;
        case FS_FLOOR:	act = " to set a trap on";	break;
        default: break;
    }

    if (action < FS_FLAGS2)
    {
        flag = bitzero << (action - FS_FLAGS1);
        if (!(f_ptr->f_flags1 & flag))
        {
            if (do_message) message(QString("You see nothing %1%2.") .arg(here) .arg(act));
            return (FALSE);
        }
    }

    else if (action < FS_FLAGS3)
    {
        flag = bitzero << (action - FS_FLAGS2);
        if (!(f_ptr->f_flags2 & flag))
        {
            if (do_message) message(QString("You see nothing %1%2.") .arg(here) .arg(act));
            return (FALSE);
        }
    }

    else if (action < FS_FLAGS_END)
    {
        flag = bitzero << (action - FS_FLAGS3);
        if (!(f_ptr->f_flags2 & flag))
        {
            if (do_message) message(QString("You see nothing %1%2.") .arg(here) .arg(act));
            return (FALSE);
        }
    }

    return (TRUE);
}


/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
    QString out_val;
    byte quest;

    int decrease = 0;

    // Paranoia
    if (!character_dungeon) return;

    feature_type *f_ptr= &f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx];

    /* Verify stairs */
    if (!cave_up_stairs(p_ptr->py, p_ptr->px))
    {
        message(QString("I see no up staircase here."));

        return;
    }

    /* Ironman */
    if (birth_ironman)
    {
        message(QString("Nothing happens!"));
        return;
    }

    /* Verify leaving normal quest level */
    if ((verify_leave_quest) && quest_might_fail_if_leave_level())
    {
        out_val = "Really risk failing your quest? ";
        if (!get_check(out_val)) return;
    }

    /* Verify leaving normal quest level */
    if ((verify_leave_quest) && quest_shall_fail_if_leave_level())
    {
        out_val = "Really fail your quest? ";
        if (!get_check(out_val)) return;
    }

    /* Success */
    message(QString("You enter a maze of up staircases."));
    if (game_mode == GAME_NPPMORIA) message(QString("You pass through a one-way door."));

    /* Create a way back */
    if (birth_connected_stairs) p_ptr->create_stair = FEAT_STAIRS_DOWN;

    /* New depth */
    decrease++;

    /*find out of entering a quest level (unusual going up)*/
    quest = quest_check(p_ptr->depth);

    /*go up another level if it is a shaft*/
    if ((f_ptr->f_flags2 & (FF2_SHAFT)) &&
        (!quest) && (p_ptr->depth > 0))
    {
        decrease++;

        /* Create a way back (usually) */
        if (birth_connected_stairs) p_ptr->create_stair = FEAT_SHAFT_DOWN;
    }

    /* Change level */
    dungeon_change_level(p_ptr->depth - decrease);

    process_player_energy(BASE_ENERGY_MOVE);
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
    byte quest;
    QString out_val;

    int increase = 0;

    // Paranoia
    if (!character_dungeon) return;

    feature_type *f_ptr= &f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx];

    /*find out if entering a quest level*/
    quest = quest_check(p_ptr->depth);

    /* Verify stairs */
    if (!cave_down_stairs(p_ptr->py, p_ptr->px))
    {
        message(QString("I see no down staircase here."));
        return;
    }

    /* Verify leaving normal quest level */
    if ((verify_leave_quest) && quest_might_fail_if_leave_level())
    {
        out_val = "Really risk failing your quest? ";
        if (!get_check(out_val)) return;
    }

    /* Verify leaving normal quest level */
    if ((verify_leave_quest) && quest_shall_fail_if_leave_level())
    {
        out_val = "Really fail your quest? ";
        if (!get_check(out_val)) return;
    }

    /* Success */
    message(QString("You enter a maze of down staircases."));
    if (game_mode == GAME_NPPMORIA) message(QString("You pass through a one-way door."));

    /* Create a way back (usually) */
    if (birth_connected_stairs) p_ptr->create_stair = FEAT_STAIRS_UP;

    /* New level */
    increase++;

    /*find out if entering a quest level*/
    quest = quest_check(p_ptr->depth);

    /* Go down a shaft if allowed */
    if ((f_ptr->f_flags2 & (FF2_SHAFT)) &&
        (!quest) && (p_ptr->depth < MAX_DEPTH - 1))
    {
        increase++;

        /* Create a way back (usually) */
        if (birth_connected_stairs) p_ptr->create_stair = FEAT_SHAFT_UP;
    }

    /* Change level */
    dungeon_change_level(p_ptr->depth + increase);

    process_player_energy(BASE_ENERGY_MOVE);
}

/*
 * Given a "source" and "target" location, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
static int motion_dir(int y1, int x1, int y2, int x2)
{
    /* No movement required */
    if ((y1 == y2) && (x1 == x2)) return (5);

    /* South or North */
    if (x1 == x2) return ((y1 < y2) ? 2 : 8);

    /* East or West */
    if (y1 == y2) return ((x1 < x2) ? 6 : 4);

    /* South-east or South-west */
    if (y1 < y2) return ((x1 < x2) ? 3 : 1);

    /* North-east or North-west */
    if (y1 > y2) return ((x1 < x2) ? 9 : 7);

    /* Paranoia */
    return (5);
}

/*
 * Extract a "direction" which will move one step from the player location
 * towards the given "target" location (or "5" if no motion necessary).
 */
static int coords_to_dir(int y, int x)
{
    return (motion_dir(p_ptr->py, p_ptr->px, y, x));
}

/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 * ANDY - Counts features that allow action.
 */
static int count_feats(int *y, int *x, int action)
{
    int d, count;

    feature_type *f_ptr;

    u32b flag, bitzero = 0x01;

    /* Count how many matches */
    count = 0;

    /* Check around the character */
    for (d = 0; d < 8; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = p_ptr->py + ddy_ddd[d];
        int xx = p_ptr->px + ddx_ddd[d];

        /* Must have knowledge */
        if (!(dungeon_info[yy][xx].cave_info & (CAVE_MARK))) continue;

        /* Get the mimiced feature */
        f_ptr = &f_info[dungeon_info[yy][xx].feature_idx];

        if (action < FS_FLAGS2)
        {
            flag = bitzero << (action - FS_FLAGS1);
            if (!(f_ptr->f_flags1 & flag)) continue;
        }

        else if (action < FS_FLAGS3)
        {
            flag = bitzero << (action - FS_FLAGS2);
            if (!(f_ptr->f_flags2 & flag)) continue;
        }

        else if (action < FS_FLAGS_END)
        {
            flag = bitzero << (action - FS_FLAGS3);
            if (!(f_ptr->f_flags3 & flag)) continue;
        }

        /* Count it */
        ++count;

        /* Remember the location of the last door found */
        *y = yy;
        *x = xx;
    }

    /* All done */
    return count;
}

/*
 * Perform the basic "open" command on doors
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool command_open_aux(int y, int x)
{
    int i, j;

    bool more = FALSE;

    int feat = dungeon_info[y][x].feature_idx;

    int door_power;

    /* Verify legality */
    if (!do_cmd_test(y, x, FS_OPEN, TRUE)) return (FALSE);

    /* Secrets on doors */
    if (feat_ff1_match(feat, FF1_DOOR | FF1_SECRET) == (FF1_DOOR | FF1_SECRET))
    {
        /* Reveal */
        find_secret(y, x);

        /* Get the new door */
        feat = dungeon_info[y][x].feature_idx;

        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }

    /* Jammed door */
    if (feat_ff3_match(feat, FF3_DOOR_JAMMED))
    {
        /* Stuck */
        message("The door appears to be stuck.");
    }

    /* Locked door */
    else if (feat_ff3_match(feat, FF3_DOOR_LOCKED) &&
        ((door_power = feat_state_power(feat, FS_OPEN)) > 0))
    {
        /*Mark the feature lore*/
        feature_lore *f_l_ptr = &f_l_list[feat];
        f_l_ptr->f_l_flags1 |= (FF1_CAN_OPEN);

        /* Disarm factor */
        i = p_ptr->state.skills[SKILL_DISARM];

        /* Penalize some conditions */
        if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
        if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

        /* Extract the lock power */
        /* door_power must be between 1 and 7 */
        j = i - (door_power * 4);

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success */
        if (rand_int(100) < j)
        {
            /* Message */
            message("You have picked the lock.");

            /* Open the door */
            cave_alter_feat(y, x, FS_OPEN);

            /* Update the visuals */
            p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

            /* Experience */
            gain_exp(1);
        }

        /* Failure */
        else
        {

            /* Message */
            message("You failed to pick the lock.");

            /* We may keep trying */
            more = TRUE;
        }
    }

    /* Closed door */
    else
    {
        /* Open the door */
        cave_alter_feat(y, x, FS_OPEN);

        /* Update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);

        /* Sound */
        //sound(MSG_OPENDOOR);
    }

    update_stuff();

    /* Result */
    return (more);
}

/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x, bool check_locked)
{
    s16b this_o_idx, next_o_idx = 0;


    /* Scan all objects in the grid */
    for (this_o_idx = dungeon_info[y][x].object_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &o_list[this_o_idx];

        /* Get the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Check for chest */
        if (o_ptr->tval != TV_CHEST) continue;

        /* Don't count special quest items */
        if (o_ptr->is_mimic()) continue;
        if (o_ptr->is_quest_object()) continue;

        /* Handle the option to check if it is locked*/
        if (check_locked)
        {
            /* Ignore disarmed chests or chests with no traps. */
            if ((o_ptr->pval <= 0) || (!chest_traps[o_ptr->pval])) continue;
        }

        /*Success*/
        return (this_o_idx);
    }

    /* No chest */
    return (0);
}

/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
int count_chests(int *y, int *x, bool trapped)
{
    int d, count, o_idx;

    object_type *o_ptr;

    /* Count how many matches */
    count = 0;

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = p_ptr->py + ddy_ddd[d];
        int xx = p_ptr->px + ddx_ddd[d];

        /* No (visible) chest is there */
        if ((o_idx = chest_check(yy, xx, trapped)) == 0) continue;

        /* Grab the object */
        o_ptr = &o_list[o_idx];

        /* Hack - Don't open mimic chests */
        /* Don't count special quest items */
        if (o_ptr->is_mimic()) continue;
        if (o_ptr->is_quest_object()) continue;

        /* No (known) traps here */
        if (trapped &&
            (!o_ptr->is_known() ||
             (o_ptr->pval < 0) ||
             !chest_traps[o_ptr->pval]))
        {
            continue;
        }

        /* Count it */
        ++count;

        /* Remember the location of the last chest found */
        *y = yy;
        *x = xx;
    }

    /* All done */
    return count;
}

/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, s16b o_idx)
{
    int i, trap;

    object_type *o_ptr = &o_list[o_idx];

    /* Ignore disarmed chests */
    if (o_ptr->pval <= 0) return;

    /* Obtain the traps */
    trap = chest_traps[o_ptr->pval];

    /* Lose strength */
    if (trap & (CHEST_LOSE_STR))
    {
        message("A small needle has pricked you!");
        take_hit(damroll(1, 4), "a poison needle");
        (void)do_dec_stat(A_STR);
    }

    /* Lose constitution */
    if (trap & (CHEST_LOSE_CON))
    {
        message("A small needle has pricked you!");
        take_hit(damroll(1, 4), "a poison needle");
        (void)do_dec_stat(A_CON);
    }

    /* Poison */
    if (trap & (CHEST_POISON))
    {
        message("A puff of green gas surrounds you!");
        if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
        {
            (void)inc_timed(TMD_POISONED, 10 + randint(20), TRUE);
        }
    }

    /* Paralyze */
    if (trap & (CHEST_PARALYZE))
    {
        message("A puff of yellow gas surrounds you!");
        if (!p_ptr->state.free_act)
        {
            (void)inc_timed(TMD_PARALYZED, 10 + randint(20), TRUE);
        }
    }

    /* Summon monsters */
    if (trap & (CHEST_SUMMON))
    {
        int num = 2 + randint(3);
        message("You are enveloped in a cloud of smoke!");
        sound(MSG_SUM_MONSTER);
        for (i = 0; i < num; i++)
        {
            (void)summon_specific(y, x, p_ptr->depth, 0, MPLACE_OVERRIDE);
        }
        disturb(TRUE, TRUE);
    }

    /* Explode */
    if (trap & (CHEST_EXPLODE))
    {
        message("There is a sudden explosion!");
        message("Everything inside the chest is destroyed!");
        o_ptr->pval = 0;
        o_ptr->xtra1 = 0;
        take_hit(damroll(5, 8), "an exploding chest");

        /* squelch chest */
        delete_object_idx(o_idx);
        message("The chest is destroyed.");
    }
}

/*
 * Allocate objects upon opening a chest
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 */
static void chest_death(int y, int x, s16b o_idx)
{
    int number, quality, num;

    int chesttheme = 0;

    int minlevel = 0;

    object_type *o_ptr;

    object_type *i_ptr;

    object_type object_type_body;

    /* Get the chest */
    o_ptr = &o_list[o_idx];

    /* Determine how much to drop (see above)
     *
     * Small chests get 3-5 objects */

    number = 2 + randint (3);

    /* large chests get 5-7*/
    if (o_ptr->sval >= SV_CHEST_MIN_LARGE) number += 2;

    /*Jeweled chests get 7-10*/
    if (o_ptr->sval == SV_CHEST_JEWELED_LARGE) number += randint (3);

    /* Zero pval means empty chest */
    if (!o_ptr->pval) return;

    /* Opening a chest */
    object_generation_mode = OB_GEN_MODE_CHEST;

    /* Determine the "value" of the items */
    object_level = ABS(o_ptr->pval);

    /*paranoia*/
    if (object_level < 1) object_level = 1;

    /*the theme of the chest is created during object generation*/
    chesttheme = (o_ptr->xtra1);

    /*large chests have a better chance of great times*/
    if (o_ptr->sval >= SV_CHEST_MIN_LARGE) minlevel = object_level / 4;

    /*Hack - don't wan't results over 100*/
    if ((object_level + minlevel) > 100) num = 100 - minlevel;

    else num = object_level;

    /* Drop some objects (non-chests) */
    for (; number > 0; --number)
    {
        /* Get local object */
        i_ptr = &object_type_body;

        /*used to determine quality of item, gets more likely
         *to be great as you get deeper.
         */
        quality = randint (num) + minlevel;

        /* Moria has less levels */
        if (game_mode == GAME_NPPMORIA) quality += quality / 5;

        /* Wipe the object */
        i_ptr->object_wipe();

        /*theme 1 is gold, themes 2-15 are objects*/

        if (chesttheme == DROP_TYPE_GOLD) make_gold(i_ptr);

        else if (chesttheme >= 2)
        {
            bool good, great;

            /* Regular objects in chests will become quite
             * rare as depth approaches 5000'.
             * All items with i > 50 are guaranteed great,
             * all items with i > 80  get 4 chances
             * to become an artifact.
             * Chests should be extremely lucrative
             * as a player approaches 5000'.
             * For potions, scrolls, and wands, having the
             * good and great flags checked increase the
             * max object generation level, but have no
             * other effect.  JG
             */
            if (quality < 26)
            {
                good = FALSE;
                great = FALSE;
            }
            else if (quality < 51)
            {
                good = TRUE;
                great = FALSE;
            }
            else if (quality < 81)
            {
                good = FALSE;
                great = TRUE;
            }
            else
            {
                good = TRUE;
                great = TRUE;
            }

            while (!make_object(i_ptr, good, great, chesttheme, FALSE)) continue;

            /* Remember history */
            object_history(i_ptr, ORIGIN_CHEST, 0);

            /* Hack -- Remember depth of the chest */
            i_ptr->origin_dlvl = o_ptr->origin_dlvl;
        }

        /* Drop it in the dungeon */
        drop_near(i_ptr, -1, y, x);
    }

    /* Reset the object level */
    object_level = p_ptr->depth;

    /* No longer opening a chest */
    object_generation_mode = OB_GEN_MODE_NORMAL;

    /* Empty */
    o_ptr->pval = 0;

    /*Paranoia, delete chest theme*/
    o_ptr->xtra1 = 0;
}

/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(int y, int x, s16b o_idx)
{
    int i, j;

    bool flag = TRUE;

    bool more = FALSE;

    object_type *o_ptr = &o_list[o_idx];

    /* paranoia - make sure it is a chest */
    if (o_ptr->tval != TV_CHEST)
    {
        message("This object is not a chest!");
        return (FALSE);
    }

    // Paranoia - should never happen
    if (o_ptr->is_mimic()) return (FALSE);

    if (o_ptr->is_quest_object())
    {
        message("This chest cannot be opened!");
        return (FALSE);
    }

    /* Attempt to unlock it */
    if (o_ptr->pval > 0)
    {
        /* Assume locked, and thus not open */
        flag = FALSE;

        /* Get the "disarm" factor */
        i = p_ptr->state.skills[SKILL_DISARM];

        /* Penalize some conditions */
        if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
        if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

        /* Extract the difficulty */
        j = i - o_ptr->pval;

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success -- May still have traps */
        if (rand_int(100) < j)
        {
            message("You have picked the lock.");
            ui_clear_message_label();
            gain_exp(1);
            flag = TRUE;
        }

        /* Failure -- Keep trying */
        else
        {
            /* We may continue repeating */
            more = TRUE;
            ui_update_message_label(color_string("You failed to pick the lock.", TERM_ORANGE_PEEL));
        }
    }

    /* Allowed to open */
    if (flag)
    {
        /* Apply chest traps, if any */
        chest_trap(y, x, o_idx);

        /* Let the Chest drop items */
        chest_death(y, x, o_idx);

        /* squelch chest */
        delete_object_idx(o_idx);
        message("Chest squelched after it was opened.");

    }

    /* Result */
    return (more);
}

/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void command_open(cmd_arg args)
{
    int cy, cx, y, x;
    int dir = args.direction;

    /* Count chests (locked) */
    int num_chests = 0, o_idx = 0;

    bool more = FALSE;

    /* Get location */
    cy = y = p_ptr->py + ddy[dir];
    cx = x = p_ptr->px + ddx[dir];

    /* Check for chests */
    num_chests = count_chests(&y, &x, FALSE);
    o_idx = chest_check(y, x, FALSE);

    /* Verify legality */
    if (!o_idx && !do_cmd_test(y, x, FS_OPEN, TRUE)) return;

    /* Apply confusion */
    if (confuse_dir(&dir))
    {
        /* Get location */
        cy = y = p_ptr->py + ddy[dir];
        cy = x = p_ptr->px + ddx[dir];

        /* Check for chest */
        num_chests = count_chests(&y, &x, FALSE);
        o_idx = chest_check(y, x, FALSE);
    }


    /* Set up repeated command, if necessary */
    if (!p_ptr->command_current && easy_open)
    {
        p_ptr->player_command_wipe();
        p_ptr->command_current = CMD_OPEN;
        p_ptr->player_args.direction = dir;
        command_type *command_ptr = &command_info[CMD_OPEN];
        p_ptr->player_args.repeats = command_ptr->repeat_num;
    }

    p_ptr->player_previous_command_update(CMD_OPEN, args);

    /* Monster */
    if (dungeon_info[y][x].monster_idx > 0)
    {
        /* Message */
        message("There is a monster in the way!");

        /* Attack */
        py_attack(y, x);
        // Energy expeneded in py_attack
        return;
    }

    /* Chest */
    else if (num_chests)
    {
        /* Get top chest */
        o_idx = chest_check(y, x, FALSE);

        /* Open the chest if confused, or only one */
        if ((p_ptr->timed[TMD_CONFUSED]) || (num_chests == 1))  more = do_cmd_open_chest(y, x, o_idx);

        /* More than one */
        else
        {
            bool do_open = FALSE;
            // See if we have already selected the chest
            if (p_ptr->command_current == CMD_OPEN)
            {
                o_idx = p_ptr->player_args.item;
                do_open = TRUE;
            }
            else
            {

                QString q, s;

                o_idx = 0;

                /* Get an item */
                q = "Open which chest? ";
                s = "There are no chests in that direction!";

                /*clear the restriction*/
                item_tester_hook = obj_is_chest;

                /*player chose escape*/
                if (!get_item_beside(&o_idx, q, s, cy, cx)) more = 0;
                else
                {
                    do_open = TRUE;
                    p_ptr->player_args.item = o_idx;
                }
            }

            /* Open the chest */
            if (do_open) more = do_cmd_open_chest(cy, cx, -o_idx);
        }
    }

    /* Door */
    else
    {
        /* Open the door */
        more = command_open_aux(cy, cx);
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(TRUE, FALSE);

    process_player_energy(BASE_ENERGY_MOVE);
}

void do_cmd_open(int dir)
{
    if (!character_dungeon) return;

    int door_y, door_x;
    int chest_y, chest_x;

    int num_doors = count_feats(&door_y, &door_x, FS_OPEN);
    int num_chests = count_chests(&chest_y, &chest_x, FALSE);

    // Nothing to open
    if(!num_doors && !num_chests) return;

    /* Easy Open */
    if (easy_open)
    {
        /* See if only one target */
        if ((num_doors + num_chests) == 1)
        {
            if (num_doors) dir = coords_to_dir(door_y, door_x);
            else /* num_chests */ dir = coords_to_dir(chest_y, chest_x);
        }
    }

    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    cmd_arg args;
    args.wipe();
    args.direction = dir;

    command_open(args);
}

/*
 * Attempt to disarm the chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_disarm_chest(int y, int x, s16b o_idx)
{
    int i, j;

    bool more = FALSE;

    object_type *o_ptr = &o_list[o_idx];

    /* Get the "disarm" factor */
    i = p_ptr->state.skills[SKILL_DISARM];

    /* Penalize some conditions */
    if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
    if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

    /* Extract the difficulty */
    j = i - o_ptr->pval;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Must find the trap first. */
    if (!o_ptr->is_known())
    {
        message("I don't see any traps.");
    }

    /* Already disarmed/unlocked */
    else if (o_ptr->pval <= 0)
    {
        message("The chest is not trapped.");
    }

    /* No traps to find. */
    else if (!chest_traps[o_ptr->pval])
    {
        message("The chest is not trapped.");
    }

    /* Success (get a lot of experience) */
    else if (rand_int(100) < j)
    {
        message("You have disarmed the chest.");
        gain_exp(o_ptr->pval);
        o_ptr->pval = (0 - o_ptr->pval);
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && (randint(i) > 5))
    {
        /* We may keep trying */
        more = TRUE;

        ui_update_message_label(color_string("You failed to disarm the chest.", TERM_ORANGE_PEEL));
    }

    /* Failure -- Set off the trap */
    else
    {
        message("You set off a trap!");
        chest_trap(y, x, o_idx);
    }

    /* Result */
    return (more);
}

/*
 * Perform the basic "disarm" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool command_disarm_aux(int y, int x, bool disarm)
{
    int i, j, power;

    QString act;

    QString name;

    bool more = FALSE;

    int feat;

    feature_lore *f_l_ptr;

    /* Arm or disarm */
    if (disarm) act = "disarm";
    else act = "arm";

    /* Verify legality */
    if (!cave_any_trap_bold(y, x)) return (FALSE);

    feat = x_list[dungeon_info[y][x].effect_idx].x_f_idx;

    f_l_ptr = &f_l_list[feat];

    /* Get the trap name */
    name = feature_desc(feat, FALSE, TRUE);

    /* Get the "disarm" factor */
    i = p_ptr->state.skills[SKILL_DISARM];

    /* Penalize some conditions */
    if (p_ptr->timed[TMD_BLIND] || no_light()) i = i / 10;
    if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) i = i / 10;

    /* XXX XXX XXX Variable power? */

    /* Extract trap "power" */
    power = 5 + p_ptr->depth / 4;

    /* Prevent the player's own traps granting exp. */
    if (feat_ff2_match(feat, FF2_TRAP_MON)) power = 0;

    /* Prevent glyphs of warding granting exp. */
    if  (feat_ff1_match(feat, FF1_GLYPH)) power = 0;

    /* Extract the difficulty */
    j = i - power;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /*Mark the feature lore*/
    f_l_ptr->f_l_flags1 |= (FF1_CAN_DISARM);

    /* Success, always succeed with player trap or glyphs */
    if (feat_ff2_match(feat, FF2_TRAP_MON) ||
        feat_ff1_match(feat, FF1_GLYPH) || (rand_int(100) < j))
    {

        /* Special message for glyphs. */
        if  (feat_ff1_match(feat, FF1_GLYPH))
            message(QString("You have desanctified the %1.").arg(name));

        /* Normal message otherwise */
        else ui_update_message_label(color_string((QString("You have %1ed the %2.").arg(act).arg(name)), TERM_ORANGE));

        /* If a Rogue's monster trap, decrement the trap count. */
        if (feat_ff2_match(feat, FF2_TRAP_MON)) num_trap_on_level--;

        /* Reward */
        gain_exp(power);

        /* Disarm */
        delete_effect_idx(dungeon_info[y][x].effect_idx);

        /* Forget the trap */
        dungeon_info[y][x].cave_info &= ~(CAVE_MARK);

        /* Check if the grid is still viewable */
        note_spot(y, x);

        light_spot(y, x);
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && (randint(i) > 5))
    {

        /* Message */
        message(QString("You failed to %1 the %2.").arg(act).arg(name));

        /* We may keep trying */
        more = TRUE;
    }

    /* Failure -- Set off the trap */
    else if (cave_passive_trap_bold(y, x))
    {
        /* Message */
        message(QString("You set off the %1!").arg(name));

        /* Hit the trap */
        hit_trap(feat, y, x, MODE_ACTION);
    }

    /* Result */
    return (more);
}

/*
 * Return the number of traps or glyphs around (or under) the character.
 * If requested, count only known traps.
 */
static int count_traps(int *y, int *x, bool known)
{
    int d, count;

    effect_type *x_ptr;

    /* Count how many matches */
    count = 0;

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = p_ptr->py + ddy_ddd[d];
        int xx = p_ptr->px + ddx_ddd[d];

        /* No trap effect is there */
        if (!cave_any_trap_bold(yy, xx)) continue;

        /* Grab the object */
        x_ptr = &x_list[dungeon_info[yy][xx].effect_idx];

        /* Hidden */
        if ((known) && (x_ptr->x_flags & (EF1_HIDDEN))) continue;

        /* Count it */
        ++count;

        /* Remember the location of the last trap found */
        *y = yy;
        *x = xx;
    }

    /* All done */
    return (count);
}

/*
 * Disarms a trap, or a chest
 */
void command_disarm(cmd_arg args)
{
    int dir = args.direction;
    int dir_y, dir_x, chest_y, chest_x, trap_y, trap_x;

    int num_traps, o_idx, num_chests;

    bool more = FALSE;

    /* Get location */
    chest_y = trap_y = dir_y = p_ptr->py + ddy[dir];
    chest_x = trap_x = dir_x = p_ptr->px + ddx[dir];

    /* Count visible traps */
    num_traps = count_traps(&trap_y, &trap_x, TRUE);

    /* Count chests (trapped) */
    num_chests = count_chests(&chest_y, &chest_x, TRUE);

    /* Check for trapped chests */
    o_idx = chest_check(chest_y, chest_x, TRUE);

    /* Verify legality */
    if (!num_traps && !num_chests) return;

    /* Apply confusion */
    if (confuse_dir(&dir))
    {
        /* Get location */
        chest_y = trap_y = dir_y = p_ptr->py + ddy[dir];
        chest_x = trap_x = dir_x = p_ptr->px + ddx[dir];

        /* re-count the chests and traps */
        num_traps = count_traps(&trap_y, &trap_x, TRUE);

        num_chests= count_chests(&chest_y, &chest_x, TRUE);
        o_idx = chest_check(chest_y, chest_x, TRUE);

        /* Verify legality */
        if (!num_traps && !num_chests)
        {
            message("You are too confused!");
            process_player_energy(BASE_ENERGY_MOVE);
            return;
        }

        if (num_chests)
        {
            dir_y = chest_y;
            dir_x = chest_x;
        }
        else  /* (num_traps) */
        {
            dir_y = trap_y;
            dir_x = trap_x;
        }
    }
    /* One final check to see if we are opening a chest or a trap, if both are present */
    else if ((num_chests) && (num_traps))
    {
        /* Did the player initially specify a trap or a chest? */
        if (cave_any_trap_bold(dir_y, dir_x))
        {
            num_chests = 0;
            trap_y = dir_y;
            trap_x = dir_x;
        }
    }

    // Repeat commands if necessary
    if (easy_open && !p_ptr->command_current)
    {
        p_ptr->player_command_wipe();
        p_ptr->command_current = CMD_DISARM;
        p_ptr->player_args.direction = dir;
        command_type *command_ptr = &command_info[CMD_DISARM];
        p_ptr->player_args.repeats = command_ptr->repeat_num;
    }

     p_ptr->player_previous_command_update(CMD_DISARM, args);

    /* Monster */
    if (dungeon_info[dir_y][dir_x].monster_idx > 0)
    {
        /* Message */
        message("There is a monster in the way!");

        /* Attack */
        py_attack(dir_y, dir_x);
        //Energy burned by py_attack.
        return;
    }

    /* Chest */
    else if (num_chests)
    {

        /* Disarm the chest if confused, or only one */
        if ((p_ptr->timed[TMD_CONFUSED]) || (num_chests == 1))
        {
            more = do_cmd_disarm_chest(chest_y, chest_x, o_idx);
        }

        /* More than one */
        else
        {
            QString q, s;
            o_idx = 0;

            /* Get an item */
            q = "Disarm which chest? ";
            s = "There are no trapped chests in that direction!";

            /*clear the restriction*/
            item_tester_hook = chest_requires_disarming;

            /*player chose escape*/
            if (!get_item_beside(&o_idx, q, s, chest_y, chest_x)) more = 0;

            /* Disarm the chest */
            else more = do_cmd_disarm_chest(chest_y, chest_x, -o_idx);
        }
    }

    /* Disarm trap */
    else
    {
        /* Disarm the trap */
        more = command_disarm_aux(trap_y, trap_x, TRUE);
    }

    /* Cancel repeat unless told not to */
    if (!more) disturb(FALSE, FALSE);

    process_player_energy(BASE_ENERGY_MOVE);
}

void do_cmd_disarm(int dir)
{
    if (!character_dungeon) return;

    int chest_y, chest_x, trap_y, trap_x;

    /* Count visible traps */
    int num_traps = count_traps(&trap_y, &trap_x, TRUE);

    /* Count chests (trapped) */
    int num_chests = count_chests(&chest_y, &chest_x, TRUE);

    /* Easy Disarm */
    if (easy_open)
    {
        /* See if only one target */
        if ((num_traps + num_chests) == 1)
        {
            if (num_traps)
            {
                dir = coords_to_dir(trap_y, trap_x);
            }
            else  /* (num_chests) */
            {
                dir = coords_to_dir(chest_y, chest_x);
            }
        }
    }

    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    cmd_arg args;
    args.wipe();
    args.direction = dir;

    command_disarm(args);
}

/*
 * Search for hidden things
 * Process energy is not called from this function.
 */
void do_search(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int y, x, chance;

    object_type *o_ptr;


    /* Start with base search ability */
    chance = p_ptr->state.skills[SKILL_SEARCH_CHANCE];

    /* Penalize various conditions */
    if (p_ptr->timed[TMD_BLIND] || no_light()) chance = chance / 10;
    if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) chance = chance / 10;

    /* Search the nearby grids, which are always in bounds */
    for (y = (py - 1); y <= (py + 1); y++)
    {
        for (x = (px - 1); x <= (px + 1); x++)
        {
            /* Sometimes, notice things */
            if (rand_int(100) < chance)
            {
                /* Get the feature */
                int feat = dungeon_info[y][x].feature_idx;

                /* Reveal interesting secret features */
                if (feat_ff1_match(feat, FF1_SECRET) &&
                    (feat_ff3_match(feat, FF3_PICK_TRAP |
                        FF3_PICK_DOOR) ||
                     !feat_ff1_match(feat, FF1_MOVE)))
                {
                    find_secret(y, x);
                }

                /* Find hidden player traps */
                if (cave_player_trap_bold(y, x))
                {
                    /* Get the trap */
                    effect_type *x_ptr = &x_list[dungeon_info[y][x].effect_idx];

                    /* Ignore known traps */
                    if (x_ptr->x_flags & (EF1_HIDDEN))
                    {
                        /* Reveal the trap */
                        x_ptr->x_flags &= ~(EF1_HIDDEN);

                        /* Show the trap */
                        note_spot(y, x);

                        light_spot(y, x);

                        /* Message */
                        message(QString("You have found a trap!"));

                        /* Disturb the player */
                        disturb(FALSE, FALSE);
                    }
                }

                /* Scan all objects in the grid */
                for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
                {
                    /* Skip non-chests */
                    if (!o_ptr->is_chest()) continue;

                    /* Skip disarmed chests */
                    if (o_ptr->pval <= 0) continue;

                    /* Skip non-trapped chests */
                    if (!chest_traps[o_ptr->pval]) continue;
                    if (o_ptr->is_mimic()) continue;
                    if (o_ptr->is_quest_object()) continue;

                    /* Identify once */
                    if (!o_ptr->is_known())
                    {
                        /* Message */
                        message(QString("You have discovered a trap on the chest!"));

                        /* Know the trap */
                        o_ptr->mark_known(FALSE);

                        /* Notice it */
                        disturb(FALSE, FALSE);
                    }
                }
            }
        }
    }
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{
    /* Stop searching */
    if (p_ptr->searching)
    {
        /* Clear the searching flag */
        p_ptr->searching = FALSE;
    }

    /* Start searching */
    else
    {
        /* Set the searching flag */
        p_ptr->searching = TRUE;
    }

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Redraw the state */
    p_ptr->redraw |= (PR_SIDEBAR_PL | PR_STATUSBAR);

    handle_stuff();
}

void command_search(cmd_arg args)
{
    p_ptr->player_previous_command_update(CMD_SEARCH, args);

    do_search();
    process_player_energy(BASE_ENERGY_MOVE);
}

/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
    if (!character_dungeon) return;

    cmd_arg args;
    args.wipe();

    /* Search */
    command_search(args);
}

/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Needs to first tunnel effects such as rubble or loose rock
 *
 * Uses "twall" (above) to do all "terrain feature changing".
 *
 * Returns TRUE if repeated commands may continue
 */
static bool command_tunnel_aux(int y, int x)
{
    bool more = FALSE;
    bool tunneling_effect = FALSE;

    int this_x_idx, next_x_idx;
    effect_type *x_ptr;

    int feat;

    QString name;

    int j;

    feat = dungeon_info[y][x].feature_idx;

    /* Scan all effects in the grid */
    for (this_x_idx = dungeon_info[y][x].effect_idx; this_x_idx; this_x_idx = next_x_idx)
    {
        /* Get the effect */
        x_ptr = &x_list[this_x_idx];

        /* Get the next effect */
        next_x_idx = x_ptr->next_x_idx;

        /* Accept this item */
        if (!feat_ff1_match(x_ptr->x_f_idx, FF1_CAN_TUNNEL)) continue;

        tunneling_effect = TRUE;
        feat = x_ptr->x_f_idx;
        break;
    }

    /* Verify legality */
    if (!tunneling_effect)
    {
        if (!do_cmd_test(y, x, FS_TUNNEL, TRUE)) return (FALSE);
    }

    if (!tunneling_effect) j = feat_state_power(feat, FS_TUNNEL);
    else j = 1;

    /* Sound XXX XXX XXX */
    sound(MSG_DIG);

    /* Make some noise. */
    add_wakeup_chance = 1000;

    /* Permanent doors/rock */
    if (!tunneling_effect && cave_ff1_match(y, x, FF1_PERMANENT))
    {
        /* Stuck */
        find_secret(y, x);

        // Get the feature again
        feat = dungeon_info[y][x].feature_idx;

        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }

    /* Silent watchers */
    else if (feat == FEAT_SILENT_WATCHER)
    {
        /* Hurt the player */
        hit_silent_watcher(y, x);
    }

    /* Dig or tunnel */
    else if (feat_ff1_match(feat, FF1_CAN_TUNNEL))
    {
        /*Mark the feature lore*/
        feature_lore *f_l_ptr = &f_l_list[feat];
        f_l_ptr->f_l_flags1 |= (FF1_CAN_TUNNEL);

        /* Dig */
        if (tunneling_effect || p_ptr->state.skills[SKILL_DIGGING] > rand_int(40* j))
        {
            sound(MSG_DIG);

            bool put_object = FALSE;

            /* Get the name */
            name = feature_desc(feat, FALSE, TRUE);

            /* Give the message */
            message(QString("You have removed the " + name + "."));

            if (!tunneling_effect) cave_alter_feat(y, x, FS_TUNNEL);
            else
            {
                if (cave_hidden_object_bold(y, x)) put_object = TRUE;
                delete_effect_idx(this_x_idx);
            }

            if (put_object)
            {
                message(QString("You have found something!"));
                place_object(y, x, FALSE, FALSE, DROP_TYPE_UNTHEMED);
            }

            /* Forget the square if marked */
            dungeon_info[y][x].cave_info &= ~(CAVE_MARK);

            /* Check if the grid is still viewable */
            note_spot(y, x);

            light_spot(y, x);

            /* Update the visuals */
            p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
        }

        /* Take damage from the feature sometimes */
        else if ((f_info[feat].dam_non_native > 0) &&
            !is_player_native(y, x) && one_in_(50))
        {
            QString kb("digging ");

            /* Get the name */
            name = feature_desc(feat, TRUE, TRUE);

            /* Format the killer string */
            kb += name;

            /* Take the hit */
            take_terrain_hit(f_info[feat].dam_non_native, feat, kb);
        }

        /* Keep trying */
        else
        {
            /* Get the name */
            name = feature_desc(feat, FALSE, TRUE);

            /* We may continue tunneling */
            ui_update_message_label(color_string(QString("You dig into the %1.") .arg(name), TERM_ORANGE_PEEL));

            more = TRUE;
        }

    }

    /* Result */
    return (more);
}



/*
 * Tunnel through "walls" (including rubble and secret doors)
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
void command_tunnel(cmd_arg args)
{
    int y, x, dir;

    bool more = FALSE;

    dir = args.direction;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Apply confusion */
    if (confuse_dir(&dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    // Repeat commands if necessary
    if (easy_open && !p_ptr->command_current)
    {
        p_ptr->player_command_wipe();
        p_ptr->command_current = CMD_TUNNEL;
        p_ptr->player_args.direction = dir;
        command_type *command_ptr = &command_info[CMD_TUNNEL];
        p_ptr->player_args.repeats = command_ptr->repeat_num;
    }

    p_ptr->player_previous_command_update(CMD_TUNNEL, args);

    /* Monster */
    if (dungeon_info[y][x].monster_idx > 0)
    {
        /* Message */
        message("There is a monster in the way!");

        /* Attack Energy burned by py_attack. */
        py_attack(y, x);
        return;
    }

    /* Walls */
    else
    {  
        /* Tunnel through walls */
        more = command_tunnel_aux(y, x);
    }

    /* Cancel repetition unless we can continue */
    if (!more) disturb(FALSE, FALSE);

    /* Take a turn */
    process_player_energy(BASE_ENERGY_MOVE);
}

void do_cmd_tunnel(int dir)
{
    if (!character_dungeon) return;

    int y, x;

    // Nothing to tunnel
    if (!count_feats(&y, &x, FS_TUNNEL)) return;

    /* Easy Close */
    if (easy_open)
    {
        /* Handle a single open door */
        if (count_feats(&y, &x, FS_TUNNEL) == 1)
        {
            /* Don't close door player is on */
            if ((y != p_ptr->py) || (x != p_ptr->px))
            {
                dir = coords_to_dir(y, x);
            }
        }
    }
    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    cmd_arg args;
    args.wipe();
    args.direction = dir;

    command_tunnel(args);
}


/*
 * Perform the basic "close" command
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
    bool more = FALSE;

    int feat = dungeon_info[y][x].feature_idx;

    /* Verify legality */
    if (!do_cmd_test(y, x, FS_CLOSE, TRUE)) return (FALSE);

    /* Broken door */
    if (feat_ff3_match(feat, FF3_DOOR_BROKEN))
    {
        /* Message */
        message("The door appears to be broken.");
    }

    /* Secrets on door/permanent doors */
    else if (feat_ff1_match(feat, FF1_SECRET | FF1_PERMANENT))
    {
        /* Stuck */
        find_secret(y,x);

        feat = dungeon_info[y][x].feature_idx;

        /* Update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);
    }

    /* Open door */
    else
    {
        /*Mark the feature lore*/
        feature_lore *f_l_ptr = &f_l_list[feat];
        f_l_ptr->f_l_flags1 |= (FF1_CAN_CLOSE);

        /* Close the door */
        cave_alter_feat(y, x, FS_CLOSE);

        /* Update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS | PU_FLOW_NO_DOORS | PU_FLOW_DOORS);

        /* Sound */
        sound(MSG_SHUTDOOR);
    }

    /* Result */
    return (more);
}

/*
 * Close an open door.
 */
void command_close(cmd_arg args)
{
    int y, x, dir;

    bool more = FALSE;

    dir = args.direction;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!do_cmd_test(y, x, FS_CLOSE, TRUE))
    {
        /* Cancel repeat */
        disturb(FALSE, FALSE);
        return;
    }

    /* Apply confusion */
    if (confuse_dir(&dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    p_ptr->player_previous_command_update(CMD_CLOSE, args);

    /* Monster */
    if (dungeon_info[y][x].has_monster())
    {
        /* Message */
        message("There is a monster in the way!");

        /* Attack Energy burned by py_attack.*/
        py_attack(y, x);
        return;
    }

    /* Door */
    else
    {
        /* Close door */
        more = do_cmd_close_aux(y, x);
    }

    /* Cancel repeat unless told not to */
    if (!more) disturb(FALSE, FALSE);


    /* Take a turn */
    process_player_energy(BASE_ENERGY_MOVE);
}

void do_cmd_close(int dir)
{
    if (!character_dungeon) return;

    int y, x;

    // Nothing to close
    if (!count_feats(&y, &x, FS_CLOSE)) return;

    /* Easy Close */
    if (easy_open)
    {
        /* Count open doors */
        if (count_feats(&y, &x, FS_CLOSE) == 1)
        {
            dir = coords_to_dir(y, x);
        }
    }

    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }


    cmd_arg args;
    args.wipe();
    args.direction = dir;

    command_close(args);
}

/*
 * The alter command tries to figure the most logical
 * command to do to the given square.
 * Most functions called apply their own confusion and
 * burn their own energy.
 */

void command_alter(cmd_arg args)
{
    int y, x;

    u16b feat;

    int dir = args.direction;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    if (!in_bounds_fully(y, x)) return;

    /* Original feature */
    feat = dungeon_info[y][x].feature_idx;

    /* Must have knowledge to know feature XXX XXX */
    if (!(dungeon_info[y][x].cave_info & (CAVE_MARK))) feat = FEAT_NONE;

     p_ptr->player_previous_command_update(CMD_ALTER, args);

    /*Is there a monster on the space?*/
    if (dungeon_info[y][x].monster_idx > 0)
    {
        /* Apply confusion */
        if (confuse_dir(&dir))
        {
            /* Get location */
            y = p_ptr->py + ddy[dir];
            x = p_ptr->px + ddx[dir];
        }
        py_attack(y, x);
        //Energy burned by py_attack.
        return;
    }

    /* Open closed doors */
    else if (feat_ff1_match(feat, FF1_CAN_OPEN))
    {
        /* Open */
        command_open(args);
        //Energy burned by open.
        return;
    }

    /* Disarm traps */
    else if (cave_any_trap_bold(y, x))
    {
        /* Open */
        command_disarm(args);
        //Energy burned by function.
        return;
    }

    /* Bash jammed doors */
    else if (feat_ff1_match(feat, FF1_CAN_BASH))
    {
        /* Bash */
        command_bash(args);
        return;
    }

    /* Tunnel through walls */
    else if (feat_ff1_match(feat, FF1_DOOR | FF1_CAN_TUNNEL) ==	(FF1_CAN_TUNNEL))
    {
        /* Tunnel */
        command_tunnel(args);
        return;
    }

    /* Close open doors */
    else if (feat_ff1_match(feat, FF1_CAN_CLOSE))
    {
        /* Close */
        command_close(args);
        //Energy burned by function.
        return;
    }

    /* Oops */
    else
    {
        /* Oops */
        message("You spin around.");
    }

    // Take a turn
    process_player_energy(BASE_ENERGY_MOVE);
}

void do_cmd_alter(int dir)
{
    if (!character_dungeon) return;

    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    cmd_arg args;
    args.wipe();
    args.direction = dir;

    command_alter(args);
}

/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
static bool get_spike(int *ip)
{
    int i;

    /* Check every item in the pack */
    for (i = 0; i < INVEN_PACK; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

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
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void command_spike(cmd_arg args)
{
    int y, x, dir, item = 0;

    dir = args.direction;

    /* Get a spike */
    if (!get_spike(&item))
    {
        /* Message */
        message(QString("You have no spikes!"));

        /* Done */
        return;
    }

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!do_cmd_test(y, x, FS_SPIKE, TRUE)) return;

    /* Confuse direction */
    if (confuse_dir(&dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

     p_ptr->player_previous_command_update(CMD_SPIKE, args);

    /* Monster */
    if (dungeon_info[y][x].monster_idx > 0)
    {
        /* Message */
        message(QString("There is a monster in the way!"));

        /* Attack */
        py_attack(y, x);
        //Energy burned by py_attack.
        return;
    }

    /* Go for it */
    int feat = dungeon_info[y][x].feature_idx;

    /*Mark the feature lore*/
    feature_lore *f_l_ptr = &f_l_list[feat];
    f_l_ptr->f_l_flags1 |= (FF1_CAN_SPIKE);

    /* Verify legality */
    if (!do_cmd_test(y, x, FS_SPIKE, TRUE)) return;

    /* Secrets on door/permanent doors */
    if (feat_ff1_match(feat, FF1_SECRET | FF1_PERMANENT))
    {
        /* Stuck */
        find_secret(y,x);

        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

    }

    //The door is already jammed
    if (f_info[feat].is_jammed_door())
    {
        message(QString("The %1 is already jammed.") .arg(feature_desc(feat, FALSE, TRUE)));
        return;
    }

    /* Successful jamming */
    message(QString("You jam the %1 with a spike.") .arg(feature_desc(feat, FALSE, TRUE)));

    /*Spike the door*/
    cave_alter_feat(y, x, FS_SPIKE);

    /* Use up, and describe, a single spike, from the bottom */
    inven_item_increase(item, -1);
    inven_item_describe(item);
    inven_item_optimize(item);

    /* Take a turn */
    process_player_energy(BASE_ENERGY_MOVE);
}


void do_cmd_spike(int dir)
{
    if (!character_dungeon) return;

    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    cmd_arg args;
    args.wipe();
    args.direction = dir;

    command_spike(args);
}

// Just burn some energy
void command_rest(cmd_arg args)
{
    if (!p_ptr->is_resting())
    {
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }
    if (args.repeats) ui_update_message_label(color_string(QString("Resting %1") .arg(args.repeats), TERM_BLUE));
    else ui_update_message_label(color_string("Resting", TERM_BLUE));

    p_ptr->player_previous_command_update(CMD_RESTING, args);

    /* Cancel searching */
    p_ptr->searching = FALSE;

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    if (p_ptr->should_stop_resting())
    {
        disturb(FALSE, FALSE);
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }

    /* Handle stuff */
    handle_stuff();

    process_player_energy(BASE_ENERGY_MOVE);
}

void RestDialog::on_clicked()
{
    QObject *obj = QObject::sender();
    choice = obj->property("choice").toInt();
    this->accept();
}

void RestDialog::keyPressEvent(QKeyEvent *event)
{
    QString txt = event->text();

    // Handle escape key
    if (event->key() == Qt::Key_Escape)
    {
        this->reject();
        return;
    }

    if (txt.at(0).isLetter())
    {
        QList<QPushButton *> list = this->findChildren<QPushButton *>();
        for (int i = 0; i < list.size(); i++)
        {
            if (list.at(i)->text().startsWith(txt))
            {
                list.at(i)->click();
                return;
            }
        }
    }

}

RestDialog::RestDialog(int *_choice)
{
    choice = *_choice = 0;

    QVBoxLayout *lay1 = new QVBoxLayout;
    this->setLayout(lay1);
    //lay1->setContentsMargins(0, 0, 0, 0);

    QLabel *lb = new QLabel("Pick the rest type");
    lb->setStyleSheet("font-weight: bold;");
    lay1->addWidget(lb);

    struct
    {
        QString name;
        int value;
    } choices[] =
    {
        {"Complete", REST_COMPLETE},
        {"Hit points and Spell points", REST_BOTH_SP_HP},
        {"Hit points", REST_HP},
        {"Spell points", REST_SP},
        {"Rest Turncount", REST_TURNCOUNT},
        {"", 0}
    };

    for (int i = 0; !choices[i].name.isEmpty(); i++) {
        QString lb = number_to_letter(i);
        lb += ") ";
        lb += choices[i].name;
        QPushButton *btn = new QPushButton(lb);
        btn->setProperty("choice", choices[i].value);
        btn->setStyleSheet("text-align: left");
        connect(btn, SIGNAL(clicked()), this, SLOT(on_clicked()));

        lay1->addWidget(btn);
    }

    QDialogButtonBox *buttons = new QDialogButtonBox(QDialogButtonBox::Cancel);
    connect(buttons, SIGNAL(rejected()), this, SLOT(close()));
    lay1->addWidget(buttons);

    this->exec();

    *_choice = choice;

    if (choice == REST_TURNCOUNT)
    {
        int repeats = get_quantity(QString("Enter rest turncount"), 9999, 1, FALSE);

        if (!repeats) *_choice = 0;

        p_ptr->player_args.repeats = repeats;
    }
}

void do_cmd_rest(void)
{
    if (!character_dungeon) return;

    int choice;

    /* Cancel the command */
    p_ptr->player_command_wipe();

    RestDialog dlg(&choice);

    if (choice == 0) return;

    p_ptr->command_current = CMD_RESTING;
    p_ptr->player_args.choice = choice;

    command_rest(p_ptr->player_args);
}

void do_cmd_rest_specific(int choice)
{
    if (!character_dungeon) return;

    /* Cancel the command */
    p_ptr->player_command_wipe();

    p_ptr->command_current = CMD_RESTING;
    p_ptr->player_args.choice = choice;

    command_rest(p_ptr->player_args);
}


/*
 * Determine if a given grid may be "walked"
 */
static bool do_cmd_walk_test(int y, int x)
{
    int feat;

    QString name;

    /* Get feature */
    feat = dungeon_info[y][x].feature_idx;

    /* Get mimiced feature Playtesting */
    /*feat = f_info[feat].f_mimic;*/

    /* Hack -- walking obtains knowledge XXX XXX */
    if (!(dungeon_info[y][x].cave_info & (CAVE_MARK))) return (TRUE);

    /* Allow attack on visible monsters */
    if ((dungeon_info[y][x].monster_idx > 0) && (mon_list[dungeon_info[y][x].monster_idx].ml))
    {
        return TRUE;
    }

    /* Known unpassable grids */
    else if (!feat_ff1_match(feat, FF1_MOVE))
    {
        /* Check presence of interesting walls */
        if (hit_wall(y, x, FALSE)) return (TRUE);

        /* Some doors are allowed */
        if (easy_alter)
        {
            if (feat_ff1_match(feat, FF1_CAN_OPEN)) return (TRUE);
        }

        /* Get the name */
        name = feature_desc(feat, TRUE, TRUE);

        /* Message */
        message("There is " + name + " in the way.");

        /* Nope */
        return (FALSE);
    }

    /* Okay */
    return (TRUE);
}

void command_run(cmd_arg args)
{
    // Args.verify should be true if we are starting a run
    // and false if we are continuing one.
    int dir = 0;
    if (args.verify) dir = args.direction;

    int energy = run_step(dir);

    ui_update_message_label(color_string("Running", TERM_GREEN));

    if (energy > 0) process_player_energy(energy);

    // Re-check if the player is on-screen if they have stopped running
    else if (!panel_contains(p_ptr->py, p_ptr->px)) p_ptr->update |= (PU_PANEL);
}

void do_cmd_run(int dir)
{
    if (!character_dungeon) return;

    // Already running (for excessive mouseclicks)
    if (p_ptr->is_running()) return;

    if (p_ptr->timed[TMD_CONFUSED])
    {
        message("You are too confused!");
        return;
    }

    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    /* Get location */
    int y = p_ptr->py + ddy[dir];
    int x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!do_cmd_walk_test(y, x)) return;

    cmd_arg args;
    args.wipe();

    args.direction = dir;
    p_ptr->player_command_wipe();
    p_ptr->command_current = CMD_RUNNING;

    command_type *command_ptr = &command_info[CMD_RUNNING];
    p_ptr->player_args.repeats = command_ptr->repeat_num;

    //This means this will be the first step of the run
    args.verify = TRUE;

    command_run(args);
}

/*
 * Return TRUE if the feature located in the given location is dangerous for the
 * player and he/she doesn't want to walk over/touch it.
 * Return FALSE if a monster occupies that grid.
 */
static bool found_dangerous_grid(int y, int x)
{
    u16b feat = dungeon_info[y][x].feature_idx;
    int gf_type;

    /* Grid is occupied by a monster. Perform a melee attack */
    if (dungeon_info[y][x].monster_idx > 0) return (FALSE);

    /* Flying entities aren't affected by terrain */
    if (p_ptr->timed[TMD_FLYING] && feat_ff2_match(feat, FF2_CAN_FLY)) return (FALSE);

    /* Player is native to that feature or feature is harmless */
    if ((f_info[feat].dam_non_native < 1) || is_player_native(y, x)) return (FALSE);

    /* Get the spell type */
    get_spell_type_from_feature(feat, &gf_type);

    /* Player is harmless to that spell type */
    if (is_player_immune(gf_type)) return (FALSE);

    /* Stop running */
    disturb(FALSE, TRUE);

    /* Ask the player for confirmation */
    QString action(feat_ff1_match(feat, FF1_MOVE) ? "walk over": "touch");

    if (get_check("It seems dangerous. Do you want to " + action + " that grid?")) return (FALSE);

    /* Dangerous */
    return (TRUE);
}

/*
 * Helper function for the "walk" and "jump" commands.
 */
void command_walk(cmd_arg args)
{
    int dir = args.direction;
    bool jumping = args.verify;

    int y, x;

    /* Get a direction (or abort) */
    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    /* Verify legality */
    if (!p_ptr->timed[TMD_CONFUSED])
    {
        /* Can the player walk over there? */
        if (!do_cmd_walk_test(y, x)) return;

        /* Dangerous grid? */
        if (found_dangerous_grid(y, x)) return;
    }

    /* Confuse direction */
    if (confuse_dir(&dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];

        /* Verify legality */
        if (!do_cmd_walk_test(y, x)) return;
    }

    p_ptr->player_previous_command_update(CMD_WALK, args);

    /* Move the player, record energy used */
    int energy = move_player(dir, jumping);

    if (energy > 0) process_player_energy(energy);
}

void do_cmd_walk(int dir, bool jumping)
{
    if (!character_dungeon) return;

    /* Get a direction (or abort) */
    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    cmd_arg args;
    args.wipe();

    args.direction = dir;
    args.verify = jumping;

    /* Move (normal) */
    command_walk(args);
}

// Does not burn player energy
static bool player_bash(int y, int x)
{
    int m_idx = dungeon_info[y][x].monster_idx;
    monster_type *m_ptr = &mon_list[m_idx];
    monster_race *r_ptr;
    QString m_name;
    object_type *o_ptr = &inventory[INVEN_ARM];

    /* Chance to hit based on strength and weight */
    int base_to_hit = p_ptr->state.stat_index[A_STR] + o_ptr->weight / 2 + p_ptr->total_weight/10;

    /* Paranoia */
    if (!(m_idx > 0)) return (FALSE);

    m_ptr = &mon_list[m_idx];
    r_ptr = &r_info[m_ptr->r_idx];
    m_name = monster_desc(m_ptr, 0);

    /* Boundry control */
    if (base_to_hit < 4)  base_to_hit = 4;

    if (test_hit(base_to_hit, r_ptr->ac, m_ptr->ml))
    {
        int dd = 4;
        int ds = (base_to_hit / 4);
        int plus = p_ptr->state.to_d;
        int damage;
        bool fear = FALSE;

        message("You bash " + m_name + ".");

        /* Allow for a critical hit */
        (void)critical_hit_check(o_ptr, &dd, &plus);

        damage = damroll(dd, ds) + plus;

        /* Monster is still alive */
        if (!mon_take_hit(m_idx, damage, &fear, NULL, SOURCE_PLAYER, FALSE))
        {
            /* Reduce its energy (half-paralysis) */
            m_ptr->m_energy = BASE_ENERGY_MOVE / 2;
            if (!m_ptr->m_timed[MON_TMD_STUN])
            {
                (void)mon_inc_timed(m_idx, MON_TMD_STUN, (randint(3) + 1), MON_TMD_FLG_NOTIFY);
            }
        }

        return (TRUE);
    }

    else message("You miss " + m_name + ".");

    /* High dexterity yields coolness */
    if (randint1(150) < p_ptr->state.stat_index[A_DEX])
    {
        /* Message */
        message("You retain your balance.");
    }
    else
    {
        /* Message */
        message("You are off-balance.");

        /* Hack -- Lose balance aka paralysis */
        (void)set_timed(TMD_PARALYZED, 2 + randint(2), FALSE);
    }

    return (FALSE);
}

/*
 * Perform the basic "bash" command
 *
 * Assume there is no monster blocking the destination
 * returns FALSE if the command should be cancelled.
 */
static bool do_cmd_bash_aux(int y, int x)
{
    int bash, temp;

    int feat = dungeon_info[y][x].feature_idx;

    QString name;

    feature_lore *f_l_ptr = &f_l_list[feat];

    /* Verify legality */
    if (!do_cmd_test(y, x, FS_BASH, TRUE)) return (FALSE);

    /* Get the name */
    name = feature_desc(feat, FALSE, TRUE);

    /* Message */
    message("You smash into the " + name + "!");

    /* Make a lot of noise. */
    add_wakeup_chance = 9000;

    /* Secrets on doors */
    if (feat_ff1_match(feat, FF1_DOOR | FF1_SECRET) == (FF1_DOOR | FF1_SECRET))
    {
        /* Reveal */
        find_secret(y, x);

        /* Get the new door */
        feat = dungeon_info[y][x].feature_idx;

        /* Update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
    }

    /* Hack -- Bash power based on strength */
    /* (Ranges from 3-20 with step=1 and 20-240 with step=10) */
    /* (A character with 18/00 STR gets 20)*/
    bash = adj_str_blow[p_ptr->state.stat_index[A_STR]];

    /* Extract door power (must be between 0 and 6) */
    temp = feat_state_power(feat, FS_BASH);

    /* Compare bash power to door power XXX XXX XXX */
    temp = (bash - (temp * 10));

    /* Hack -- always have a chance */
    if (temp < 1) temp = 1;

    /*Mark the feature lore*/
    f_l_ptr->f_l_flags1 |= (FF1_CAN_BASH);

    /* Hack -- attempt to bash down the door */
    if (rand_int(100) < temp)
    {

        /* Break down the door */
        if (!feat_ff1_match(feat, FF1_CAN_OPEN) || one_in_(2))
        {
            cave_alter_feat(y, x, FS_BASH);
        }

        /* Open the door */
        else
        {
            cave_alter_feat(y, x, FS_OPEN);
        }

        /* Message */
        if (feat_ff1_match(f_info[feat].f_mimic, FF1_DOOR))
        {
            message("The door crashes open!");
        }
        else
        {
            message("The " + name + " crashes!");
        }

        /* Update the visuals */
        p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

        return (FALSE);
    }

    /* Take a hit from the feature, sometimes */
    else if ((f_info[feat].dam_non_native > 0) &&
        !is_player_native(y, x) && one_in_(3))
    {
        QString kb_str;

        /* Get the feature name */
        name = feature_desc(feat, TRUE, TRUE);

        /* Format the killer string */
        kb_str = "bashing " + name;

        /* Take the hit */
        take_terrain_hit(f_info[feat].dam_non_native, feat, kb_str);
    }

    /* Some features can't stun the player */
    else if (cave_passable_bold(y, x))
    {
        /* Message */
        message("The " + name + " remains intact.");
    }

    /* Saving throw against stun */
    else if (rand_int(100) < adj_dex_safe[p_ptr->state.stat_index[A_DEX]] +
             p_ptr->lev)
    {
        /* Message */
        message("The " + name + " holds firm.");
    }

    /* High dexterity yields coolness */
    else
    {
        /* Message */
        message("You are off-balance.");

        /* Hack -- Lose balance ala paralysis */
        (void)inc_timed(TMD_PARALYZED, 2 + rand_int(2), TRUE);

        return (FALSE);
    }

    return (TRUE);
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
void command_bash(cmd_arg args)
{
    int y, x, dir;

    dir = args.direction;

    /* Get location */
    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];

    int m_idx = dungeon_info[y][x].monster_idx;

    /* In Moria, possibly bash a monster */
    if ((m_idx > 0) && (game_mode == GAME_NPPMORIA))
    {
        /* Do nothing except avoid the next check */
    }

    /* Verify legality */
    else if (!do_cmd_test(y, x, FS_BASH, TRUE)) return;

    // Repeat commands if necessary
    if (easy_open && !p_ptr->command_current)
    {
        p_ptr->player_command_wipe();
        p_ptr->command_current = CMD_BASH;
        p_ptr->player_args.direction = dir;
        command_type *command_ptr = &command_info[CMD_BASH];
        p_ptr->player_args.repeats = command_ptr->repeat_num;
    }

    p_ptr->player_previous_command_update(CMD_BASH, args);

    /* Apply confusion */
    if (confuse_dir(&dir))
    {
        /* Get location */
        y = p_ptr->py + ddy[dir];
        x = p_ptr->px + ddx[dir];
    }

    /* Monster */
    if (m_idx > 0)
    {
        player_bash(y, x);
    }

    /* Door */
    else
    {
        /* Bash the door */
        if (!do_cmd_bash_aux(y, x))
        {
            /* Cancel repeat */
            disturb(FALSE, FALSE);
        }
    }

    process_player_energy(BASE_ENERGY_MOVE);
}

void do_cmd_bash(int dir)
{
    if (!character_dungeon) return;

    if (dir == DIR_UNKNOWN)
    {
        if (!get_rep_dir(&dir)) return;
    }

    cmd_arg args;
    args.wipe();
    args.direction = dir;

    command_bash(args);
}

void command_hold(cmd_arg args)
{
    /* Take a turn */
    int energy = BASE_ENERGY_MOVE;

    /* Spontaneous Searching */
    if ((p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] >= SEARCH_CHANCE_MAX) ||
        (0 == rand_int(SEARCH_CHANCE_MAX - p_ptr->state.skills[SKILL_SEARCH_FREQUENCY])))
    {
        do_search();
    }

    /* Continuous Searching */
    if (p_ptr->searching)
    {
        do_search();
    }

    /* Handle "objects" */
    py_pickup(always_pickup);

    /* Hack -- enter a store if we are on one */
    if (cave_shop_bold(p_ptr->py,p_ptr->px))
    {
        /* Disturb */
        disturb(TRUE, TRUE);

        int feat = dungeon_info[p_ptr->py][p_ptr->px].feature_idx;
        int store_idx = f_info[feat].f_power;
        launch_store(store_idx);

        /* Energy already burned */
        energy = 0;
    }

    p_ptr->player_previous_command_update(CMD_HOLD, args);

    if (energy > 0) process_player_energy(energy);
}

/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
void do_cmd_hold()
{
    if (!character_dungeon) return;

    cmd_arg args;
    args.wipe();

    command_hold(args);
}

//Run using pathfind to a specific spot
void do_cmd_findpath(int y, int x)
{
    if (!character_dungeon) return;

    // For excessive mouseclicks
    if (p_ptr->is_running()) return;

    // If running fails, at least try a direction
    if (!buildpath(y, x))
    {
        do_cmd_walk(ui_get_dir_from_slope(p_ptr->py, p_ptr->px, y, x), FALSE);
        return;
    }

    cmd_arg args;
    args.wipe();

    args.direction = 0;
    p_ptr->player_command_wipe();
    p_ptr->command_current = CMD_RUNNING;
    p_ptr->running_withpathfind = TRUE;

    command_type *command_ptr = &command_info[CMD_RUNNING];
    p_ptr->player_args.repeats = command_ptr->repeat_num;

    //So we know we are running with pathfind
    args.verify = FALSE;

    command_run(args);
}
