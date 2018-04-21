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

/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
    bool go_up = FALSE;

    /* Player grid */
    cave_type *c_ptr = &cave[py][px];
    feature_type *f_ptr = &f_info[c_ptr->feat];

    int up_num = 0;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Verify stairs */
    if (!have_flag(f_ptr->flags, FF_LESS))
    {
        msg_print("I see no up staircase here.");

        return;
    }

    /* Quest up stairs */
    if (have_flag(f_ptr->flags, FF_QUEST))
    {
        /* Success */
        msg_print("You enter the up staircase.");

        leave_quest_check();

        p_ptr->inside_quest = c_ptr->special;

        /* Activate the quest */
        if (!quest[p_ptr->inside_quest].status)
        {
            quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
        }

        /* Leaving a quest */
        if (!p_ptr->inside_quest)
        {
            dun_level = 0;
        }

        /* Leaving */
        p_ptr->leaving = TRUE;

        p_ptr->oldpx = 0;
        p_ptr->oldpy = 0;

        /* End the command */
        return;
    }

    if (!dun_level)
    {
        go_up = TRUE;
    }
    else
    {
        quest_type *q_ptr = &quest[p_ptr->inside_quest];

        /* Confirm leaving from once only quest */
        if (confirm_quest && p_ptr->inside_quest &&
            (q_ptr->type == QUEST_TYPE_RANDOM ||
             (q_ptr->flags & QUEST_FLAG_ONCE &&
              q_ptr->status != QUEST_STATUS_COMPLETED)))
        {
            if (q_ptr->type == QUEST_TYPE_RANDOM && ironman_quests)
            {
                go_up = TRUE;
            }
            else
            {
                msg_print("You can't come back here once you leave this floor.");
                if (get_check("Really leave this floor? ")) go_up = TRUE;
            }
        }
        else
        {
            go_up = TRUE;
        }
    }

    /* Cancel the command */
    if (!go_up) return;

    /* Hack -- take a turn */
    energy_use = 100;

    if (autosave_l) do_cmd_save_game(TRUE);

    /* For a random quest */
    if (p_ptr->inside_quest &&
        quest[p_ptr->inside_quest].type == QUEST_TYPE_RANDOM)
    {
        leave_quest_check();

        p_ptr->inside_quest = 0;
    }

    /* For a fixed quest */
    if (p_ptr->inside_quest &&
        quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM)
    {
        leave_quest_check();

        p_ptr->inside_quest = c_ptr->special;
        dun_level = 0;
        up_num = 0;
    }

    /* For normal dungeon and random quest */
    else
    {
        /* New depth */
        if (have_flag(f_ptr->flags, FF_SHAFT))
        {
            /* Create a way back */
            prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_UP | CFM_SHAFT);

            up_num = 2;
        }
        else
        {
            /* Create a way back */
            prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_UP);

            up_num = 1;
        }

        /* Get out from current dungeon */
        if (dun_level - up_num < d_info[dungeon_type].mindepth)
            up_num = dun_level;

        if (d_info[dungeon_type].flags1 & DF1_RANDOM)
        {
            up_num = dun_level;
            prepare_change_floor_mode(CFM_NO_RETURN);
        }
    }

    /* Success */
    if (up_num == dun_level)
        msg_print("You go back to the surface.");
    else
        msg_print("You enter a maze of up staircases.");

    /* Leaving */
    p_ptr->leaving = TRUE;
}


/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
    /* Player grid */
    cave_type *c_ptr = &cave[py][px];
    feature_type *f_ptr = &f_info[c_ptr->feat];

    bool fall_trap = FALSE;
    int down_num = 0;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Verify stairs */
    if (!have_flag(f_ptr->flags, FF_MORE))
    {
        msg_print("I see no down staircase here.");

        return;
    }

    if (have_flag(f_ptr->flags, FF_TRAP)) fall_trap = TRUE;

    /* Quest entrance */
    if (have_flag(f_ptr->flags, FF_QUEST_ENTER))
    {
        do_cmd_quest();
    }

    /* Quest down stairs */
    else if (have_flag(f_ptr->flags, FF_QUEST))
    {
        msg_print("You enter the down staircase.");

        leave_quest_check();

        p_ptr->inside_quest = c_ptr->special;

        /* Activate the quest */
        if (!quest[p_ptr->inside_quest].status)
        {
            quest[p_ptr->inside_quest].status = QUEST_STATUS_TAKEN;
        }

        /* Leaving a quest */
        if (!p_ptr->inside_quest)
        {
            dun_level = 0;
        }

        /* Leaving */
        p_ptr->leaving = TRUE;

        p_ptr->oldpx = 0;
        p_ptr->oldpy = 0;
    }

    else
    {
        int target_dungeon = 0;

        if (!dun_level)
        {
            target_dungeon = have_flag(f_ptr->flags, FF_ENTRANCE) ? c_ptr->special : DUNGEON_ANGBAND;

            if (ironman_downward && (target_dungeon != DUNGEON_ANGBAND))
            {
                msg_print("The entrance of this dungeon is closed!");
                return;
            }
            if (!max_dlv[target_dungeon])
            {
                if (d_info[target_dungeon].flags1 & DF1_RANDOM)
                    msg_format("This is the entrance of %s (Danger level: ?)", d_name+d_info[target_dungeon].name);
                else
                    msg_format("This is the entrance of %s (Danger level: %d)", d_name+d_info[target_dungeon].name, d_info[target_dungeon].mindepth);
                if (!get_check("Do you really go into this dungeon? ")) return;
            }

            /* Save old player position */
            p_ptr->oldpx = px;
            p_ptr->oldpy = py;
            dungeon_type = (byte)target_dungeon;

            /*
             * Clear all saved floors
             * and create a first saved floor
             */
            prepare_change_floor_mode(CFM_FIRST_FLOOR);
        }

        /* Hack -- take a turn */
        energy_use = 100;

        if (autosave_l) do_cmd_save_game(TRUE);

        /* Go down */
        if (have_flag(f_ptr->flags, FF_SHAFT)) down_num += 2;
        else down_num += 1;

        if (!dun_level)
        {
            /* Enter the dungeon just now */
            p_ptr->enter_dungeon = TRUE;
            down_num = d_info[dungeon_type].mindepth;
        }

        if (fall_trap)
        {
            msg_print("You deliberately jump through the trap door.");
        }
        else
        {
            /* Success */
            if (target_dungeon)
            {
                msg_format("You entered %s.", d_text + d_info[dungeon_type].text);
            }
            else
            {
                msg_print("You enter a maze of down staircases.");
            }
        }


        /* Leaving */
        p_ptr->leaving = TRUE;

        if (fall_trap)
        {
            prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_RAND_PLACE | CFM_RAND_CONNECT);
        }
        else
        {
            if (have_flag(f_ptr->flags, FF_SHAFT))
            {
                /* Create a way back */
                prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_SHAFT);
            }
            else
            {
                /* Create a way back ... maybe */
                if ( p_ptr->enter_dungeon 
                  && down_num >= 20 
                  && !ironman_rooms 
                  && !(d_info[dungeon_type].flags1 & DF1_RANDOM) 
                  && !(d_info[dungeon_type].initial_guardian && !(dungeon_flags[dungeon_type] & DUNGEON_NO_GUARDIAN))
                  && one_in_(14) )
                {
                    /* Hack:  No stair scum */
                    msg_print("The stairs collapse behind you! You are trapped!!");
                    dungeon_flags[target_dungeon] |= DUNGEON_NO_ENTRANCE;
                    prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN | CFM_RAND_PLACE);
                }
                else
                {
                    prepare_change_floor_mode(CFM_SAVE_FLOORS | CFM_DOWN);
                }
            }
        }
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
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Take a turn */
    energy_use = 100;

    /* Search */
    search();
}


/*
 * Determine if a grid contains a chest
 */
static s16b chest_check(int y, int x)
{
    cave_type *c_ptr = &cave[y][x];

    s16b this_o_idx, next_o_idx = 0;


    /* Scan all objects in the grid */
    for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[this_o_idx];

        /* Acquire next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Skip unknown chests XXX XXX */
        /* if (!(o_ptr->marked & OM_FOUND)) continue; */

        /* Check for chest */
        if (o_ptr->tval == TV_CHEST) return (this_o_idx);
    }

    /* No chest */
    return (0);
}


/*
 * Allocates objects upon opening a chest    -BEN-
 *
 * Disperse treasures from the given chest, centered at (x,y).
 *
 * Chests are now a good means of getting gold  -CHRIS-
 *
 */
static bool _chest_scatter(object_type *o_ptr)
{
    int i;
    for (i = 0; i < 200; i++)
    {
        int y = randint0(MAX_HGT);
        int x = randint0(MAX_WID);

        if (!cave_empty_bold(y, x)) continue;
        drop_near(o_ptr, -1, y, x);
        return TRUE;
    }
    return FALSE;
}
static void chest_death(bool scatter, int y, int x, s16b o_idx)
{
    int         ct_objects = 0, ct_gold = 0;
    int         i;
    u32b        mode = AM_GOOD;
    object_type *chest_ptr = &o_list[o_idx];

    if (chest_ptr->sval == SV_CHEST_KANDUME) /* Can of Toys */
    {
        ct_objects = 5;
        ct_gold = 0;
        mode |= AM_GREAT;
        object_level = chest_ptr->xtra3;
    }
    else
    {        /* v~~~~~~~~~ You'll need to look a k_info.txt to understand this ... */
        int num = chest_ptr->sval % SV_CHEST_MIN_LARGE;

        object_level = ABS(chest_ptr->pval) + 10;
        if (chest_ptr->sval < SV_CHEST_MIN_LARGE)
        {
            ct_gold = damroll(2, num);
            ct_objects = damroll(1, num);
        }
        else
        {
            ct_gold = damroll(3, num);
            ct_objects = damroll(2, num);
        }
    }

    /* Zero pval means empty chest */
    if (!chest_ptr->pval)
    {
        ct_gold = 0;
        ct_objects = 0;
    }

    /* Opening a chest */
    opening_chest = TRUE; /* <==== This hack prevents getting chests from inside chests! */

    for (i = 0; i < ct_objects; i++)
    {
        object_type forge = {0};
        if (!make_object(&forge, mode)) continue;
        if (scatter) _chest_scatter(&forge);
        else drop_near(&forge, -1, y, x);
    }

    for (i = 0; i < ct_gold; i++)
    {
        object_type forge = {0};
        if (!make_gold(&forge, TRUE)) continue;
        if (scatter) _chest_scatter(&forge);
        else drop_near(&forge, -1, y, x);
    }

    /* Reset the object level */
    object_level = base_level;

    /* No longer opening a chest */
    opening_chest = FALSE;

    /* Empty */
    chest_ptr->pval = 0;

    /* Known */
    obj_identify(chest_ptr);
}


/*
 * Chests have traps too.
 *
 * Exploding chest destroys contents (and traps).
 * Note that the chest itself is never destroyed.
 */
static void chest_trap(int y, int x, s16b o_idx)
{
    int  i, trap;

    object_type *o_ptr = &o_list[o_idx];

    int mon_level = o_ptr->xtra3;

    /* Ignore disarmed chests */
    if (o_ptr->pval <= 0) return;

    /* Obtain the traps */
    trap = chest_traps[o_ptr->pval];

    /* Lose strength */
    if (trap & (CHEST_LOSE_STR))
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle", -1);

        (void)do_dec_stat(A_STR);
    }

    /* Lose constitution */
    if (trap & (CHEST_LOSE_CON))
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle", -1);

        (void)do_dec_stat(A_CON);
    }

    /* Poison */
    if (trap & (CHEST_POISON))
    {
        msg_print("A puff of green gas surrounds you!");
        if (!res_save_default(RES_POIS))
            (void)set_poisoned(p_ptr->poisoned + 10 + randint1(20), FALSE);
    }

    /* Paralyze */
    if (trap & (CHEST_PARALYZE))
    {
        msg_print("A puff of yellow gas surrounds you!");


        if (!p_ptr->free_act)
        {
            (void)set_paralyzed(randint1(4), FALSE);
        }
        else equip_learn_flag(OF_FREE_ACT);
    }

    /* Summon monsters */
    if (trap & (CHEST_SUMMON))
    {
        int num = 2 + randint1(3);
        msg_print("You are enveloped in a cloud of smoke!");


        for (i = 0; i < num; i++)
        {
            if (randint1(100)<dun_level)
                activate_hi_summon(py, px, FALSE);
            else
                (void)summon_specific(0, y, x, mon_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Elemental summon. */
    if (trap & (CHEST_E_SUMMON))
    {
        msg_print("Elemental beings appear to protect their treasures!");
        for (i = 0; i < randint1(3) + 5; i++)
        {
            (void)summon_specific(0, y, x, mon_level, SUMMON_ELEMENTAL, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Force clouds, then summon birds. */
    if (trap & (CHEST_BIRD_STORM))
    {
        msg_print("A storm of birds swirls around you!");

        for (i = 0; i < randint1(3) + 3; i++)
            (void)fire_meteor(-1, GF_FORCE, y, x, o_ptr->pval / 5, 7);

        for (i = 0; i < randint1(5) + o_ptr->pval / 5; i++)
        {
            (void)summon_specific(0, y, x, mon_level, SUMMON_BIRD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Various colorful summonings. */
    if (trap & (CHEST_H_SUMMON))
    {
        /* Summon demons. */
        if (one_in_(4))
        {
            msg_print("Demons materialize in clouds of fire and brimstone!");

            for (i = 0; i < randint1(3) + 2; i++)
            {
                (void)fire_meteor(-1, GF_FIRE, y, x, 10, 5);
                (void)summon_specific(0, y, x, mon_level, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }

        /* Summon dragons. */
        else if (one_in_(3))
        {
            msg_print("Draconic forms loom out of the darkness!");

            for (i = 0; i < randint1(3) + 2; i++)
            {
                (void)summon_specific(0, y, x, mon_level, SUMMON_DRAGON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }

        /* Summon hybrids. */
        else if (one_in_(2))
        {
            msg_print("Creatures strange and twisted assault you!");

            for (i = 0; i < randint1(5) + 3; i++)
            {
                (void)summon_specific(0, y, x, mon_level, SUMMON_HYBRID, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }

        /* Summon vortices (scattered) */
        else
        {
            msg_print("Vortices coalesce and wreak destruction!");

            for (i = 0; i < randint1(3) + 2; i++)
            {
                (void)summon_specific(0, y, x, mon_level, SUMMON_VORTEX, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }
    }

    /* Dispel player. */
    if ((trap & (CHEST_RUNES_OF_EVIL)) && o_ptr->k_idx)
    {
        /* Determine how many nasty tricks can be played. */
        int nasty_tricks_count = 4 + randint0(3);

        /* Message. */
        msg_print("Hideous voices bid:  'Let the darkness have thee!'");

        /* This is gonna hurt... */
        for (; nasty_tricks_count > 0; nasty_tricks_count--)
        {
            /* ...but a high saving throw does help a little. */
            if (randint1(100+o_ptr->pval*2) > p_ptr->skills.sav)
            {
                if (one_in_(6)) take_hit(DAMAGE_NOESCAPE, damroll(5, 20), "a chest dispel-player trap", -1);
                else if (one_in_(5)) (void)set_cut(p_ptr->cut + 200, FALSE);
                else if (one_in_(4))
                {
                    if (!p_ptr->free_act) 
                        (void)set_paralyzed(randint1(4), FALSE);
                    else 
                    {
                        equip_learn_flag(OF_FREE_ACT);
                        (void)set_stun(p_ptr->stun + 10 + randint0(100), FALSE);
                    }
                }
                else if (one_in_(3)) apply_disenchant(0);
                else if (one_in_(2))
                {
                    (void)do_dec_stat(A_STR);
                    (void)do_dec_stat(A_DEX);
                    (void)do_dec_stat(A_CON);
                    (void)do_dec_stat(A_INT);
                    (void)do_dec_stat(A_WIS);
                    (void)do_dec_stat(A_CHR);
                }
                else (void)fire_meteor(-1, GF_NETHER, y, x, 150, 1);
            }
        }
    }

    /* Aggravate monsters. */
    if (trap & (CHEST_ALARM))
    {
        msg_print("An alarm sounds!");
        aggravate_monsters(0);
    }

    /* Explode */
    if ((trap & (CHEST_EXPLODE)) && o_ptr->k_idx)
    {
        msg_print("There is a sudden explosion!");
        msg_print("Everything inside the chest is destroyed!");

        o_ptr->pval = 0;
        sound(SOUND_EXPLODE);
        take_hit(DAMAGE_ATTACK, damroll(5, 8), "an exploding chest", -1);

    }
    /* Scatter contents. */
    if ((trap & (CHEST_SCATTER)) && o_ptr->k_idx)
    {
        msg_print("The contents of the chest scatter all over the dungeon!");
        chest_death(TRUE, y, x, o_idx);
        o_ptr->pval = 0;
    }
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


    /* Take a turn */
    energy_use = 100;

    /* Attempt to unlock it */
    if (o_ptr->pval > 0)
    {
        /* Assume locked, and thus not open */
        flag = FALSE;

        /* Get the "disarm" factor */
        i = p_ptr->skills.dis;

        /* Penalize some conditions */
        if (p_ptr->blind || no_lite()) i = i / 10;
        if (p_ptr->confused || p_ptr->image) i = i / 10;

        /* Extract the difficulty */
        j = i - o_ptr->pval;

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success -- May still have traps */
        if (randint0(100) < j)
        {
            msg_print("You have picked the lock.");

            gain_exp(1);
            flag = TRUE;
        }

        /* Failure -- Keep trying */
        else
        {
            /* We may continue repeating */
            more = TRUE;
            if (flush_failure) flush();
            msg_print("You failed to pick the lock.");

        }
    }

    /* Allowed to open */
    if (flag)
    {
        /* Apply chest traps, if any */
        chest_trap(y, x, o_idx);

        /* Let the Chest drop items */
        chest_death(FALSE, y, x, o_idx);
    }

    /* Result */
    return (more);
}


#if defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) /* TNB */

/*
 * Return TRUE if the given feature is an open door
 */
static bool is_open(int feat)
{
    return have_flag(f_info[feat].flags, FF_CLOSE) && (feat != feat_state(feat, FF_CLOSE));
}


/*
 * Return the number of features around (or under) the character.
 * Usually look for doors and floor traps.
 */
static int count_dt(int *y, int *x, bool (*test)(int feat), bool under)
{
    int d, count, xx, yy;

    /* Count how many matches */
    count = 0;

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        cave_type *c_ptr;
        s16b feat;

        /* if not searching under player continue */
        if ((d == 8) && !under) continue;

        /* Extract adjacent (legal) location */
        yy = py + ddy_ddd[d];
        xx = px + ddx_ddd[d];

        /* Get the cave */
        c_ptr = &cave[yy][xx];

        /* Must have knowledge */
        if (!(c_ptr->info & (CAVE_MARK))) continue;

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Not looking for this feature */
        if (!((*test)(feat))) continue;

        /* OK */
        ++count;

        /* Remember the location. Only useful if only one match */
        *y = yy;
        *x = xx;
    }

    /* All done */
    return count;
}


/*
 * Return the number of chests around (or under) the character.
 * If requested, count only trapped chests.
 */
static int count_chests(int *y, int *x, bool trapped)
{
    int d, count, o_idx;

    object_type *o_ptr;

    /* Count how many matches */
    count = 0;

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = py + ddy_ddd[d];
        int xx = px + ddx_ddd[d];

        /* No (visible) chest is there */
        if ((o_idx = chest_check(yy, xx)) == 0) continue;

        /* Grab the object */
        o_ptr = &o_list[o_idx];

        /* Already open */
        if (o_ptr->pval == 0) continue;

        /* No (known) traps here
           CTK: pval is negative if chest is disarmed. Don't read out of bounds!! */
        if (trapped && (!object_is_known(o_ptr) || o_ptr->pval < 0 ||
            !chest_traps[o_ptr->pval])) continue;

        /* OK */
        ++count;

        /* Remember the location. Only useful if only one match */
        *y = yy;
        *x = xx;
    }

    /* All done */
    return count;
}


/*
 * Convert an adjacent location to a direction.
 */
static int coords_to_dir(int y, int x)
{
    int d[3][3] = { {7, 4, 1}, {8, 5, 2}, {9, 6, 3} };
    int dy, dx;

    dy = y - py;
    dx = x - px;

    /* Paranoia */
    if (ABS(dx) > 1 || ABS(dy) > 1) return (0);

    return d[dx + 1][dy + 1];
}

#endif /* defined(ALLOW_EASY_OPEN) || defined(ALLOW_EASY_DISARM) -- TNB */


/*
 * Perform the basic "open" command on doors
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_aux(int y, int x)
{
    int i, j;

    /* Get requested grid */
    cave_type *c_ptr = &cave[y][x];

    feature_type *f_ptr = &f_info[c_ptr->feat];

    bool more = FALSE;


    /* Take a turn */
    energy_use = 100;

    /* Seeing true feature code (ignore mimic) */

    /* Jammed door */
    if (!have_flag(f_ptr->flags, FF_OPEN))
    {
        /* Stuck */
        msg_format("The %s appears to be stuck.", f_name + f_info[get_feat_mimic(c_ptr)].name);

    }

    /* Locked door */
    else if (f_ptr->power)
    {
        /* Disarm factor */
        i = p_ptr->skills.dis;

        /* Penalize some conditions */
        if (p_ptr->blind || no_lite()) i = i / 10;
        if (p_ptr->confused || p_ptr->image) i = i / 10;

        /* Extract the lock power */
        j = f_ptr->power;

        /* Extract the difficulty XXX XXX XXX */
        j = i - (j * 4);

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success */
        if (randint0(100) < j)
        {
            /* Message */
            msg_print("You have picked the lock.");

            /* Open the door */
            cave_alter_feat(y, x, FF_OPEN);

            /* Sound */
            sound(SOUND_OPENDOOR);

            /* Experience */
            gain_exp(1);
        }

        /* Failure */
        else
        {
            /* Failure */
            if (flush_failure) flush();

            /* Message */
            msg_print("You failed to pick the lock.");


            /* We may keep trying */
            more = TRUE;
        }
    }

    /* Closed door */
    else
    {
        /* Open the door */
        cave_alter_feat(y, x, FF_OPEN);

        /* Sound */
        sound(SOUND_OPENDOOR);
    }

    /* Result */
    return (more);
}



/*
 * Open a closed/locked/jammed door or a closed/locked chest.
 *
 * Unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open(void)
{
    int y, x, dir;

    s16b o_idx;

    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

#ifdef ALLOW_EASY_OPEN /* TNB */

    /* Option: Pick a direction */
    if (easy_open)
    {
        int num_doors, num_chests;

        /* Count closed doors (locked or jammed) */
        num_doors = count_dt(&y, &x, is_closed_door, FALSE);

        /* Count chests (locked) */
        num_chests = count_chests(&y, &x, FALSE);

        /* See if only one target */
        if (num_doors || num_chests)
        {
            bool too_many = (num_doors && num_chests) || (num_doors > 1) ||
                (num_chests > 1);
            if (!too_many) command_dir = coords_to_dir(y, x);
        }
    }

#endif /* ALLOW_EASY_OPEN -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir, TRUE))
    {
        s16b feat;
        cave_type *c_ptr;

        /* Get requested location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get requested grid */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Check for chest */
        o_idx = chest_check(y, x);

        /* Nothing useful */
        if (!have_flag(f_info[feat].flags, FF_OPEN) && !o_idx)
        {
            /* Message */
            msg_print("You see nothing there to open.");

        }

        /* Monster in the way */
        else if (c_ptr->m_idx && p_ptr->riding != c_ptr->m_idx)
        {
            /* Take a turn */
            energy_use = 100;

            /* Message */
            msg_print("There is a monster in the way!");


            /* Attack */
            py_attack(y, x, 0);
        }

        /* Handle chests */
        else if (o_idx)
        {
            /* Open the chest */
            more = do_cmd_open_chest(y, x, o_idx);
        }

        /* Handle doors */
        else
        {
            /* Open the door */
            more = do_cmd_open_aux(y, x);
        }
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(0, 0);
}



/*
 * Perform the basic "close" command
 *
 * Assume destination is an open/broken door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_close_aux(int y, int x)
{
    /* Get grid and contents */
    cave_type *c_ptr = &cave[y][x];
    s16b      old_feat = c_ptr->feat;
    bool      more = FALSE;

    /* Take a turn */
    energy_use = 100;

    /* Seeing true feature code (ignore mimic) */

    /* Open door */
    if (have_flag(f_info[old_feat].flags, FF_CLOSE))
    {
        s16b closed_feat = feat_state(old_feat, FF_CLOSE);

        /* Hack -- object in the way */
        if ((c_ptr->o_idx || (c_ptr->info & CAVE_OBJECT)) &&
            (closed_feat != old_feat) && !have_flag(f_info[closed_feat].flags, FF_DROP))
        {
            /* Message */
            msg_print("There seems stuck.");
        }
        else
        {
            /* Close the door */
            cave_alter_feat(y, x, FF_CLOSE);

            /* Broken door */
            if (old_feat == c_ptr->feat)
            {
                /* Message */
                msg_print("The door appears to be broken.");
            }
            else
            {
                /* Sound */
                sound(SOUND_SHUTDOOR);
            }
        }
    }

    /* Result */
    return (more);
}


/*
 * Close an open door.
 */
void do_cmd_close(void)
{
    int y, x, dir;

    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

#ifdef ALLOW_EASY_OPEN /* TNB */

    /* Option: Pick a direction */
    if (easy_open)
    {
        /* Count open doors */
        if (count_dt(&y, &x, is_open, FALSE) == 1)
        {
            command_dir = coords_to_dir(y, x);
        }
    }

#endif /* ALLOW_EASY_OPEN -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        cave_type *c_ptr;
        s16b feat;

        /* Get requested location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get grid and contents */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Require open/broken door */
        if (!have_flag(f_info[feat].flags, FF_CLOSE))
        {
            /* Message */
            msg_print("You see nothing there to close.");
        }

        /* Monster in the way */
        else if (c_ptr->m_idx)
        {
            /* Take a turn */
            energy_use = 100;

            /* Message */
            msg_print("There is a monster in the way!");

            /* Attack */
            py_attack(y, x, 0);
        }

        /* Close the door */
        else
        {
            /* Close the door */
            more = do_cmd_close_aux(y, x);
        }
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(0, 0);
}


/*
 * Determine if a given grid may be "tunneled"
 */
static bool do_cmd_tunnel_test(int y, int x)
{
    cave_type *c_ptr = &cave[y][x];

    /* Must have knowledge */
    if (!(c_ptr->info & CAVE_MARK))
    {
        /* Message */
        msg_print("You see nothing there.");

        /* Nope */
        return (FALSE);
    }

    /* Must be a wall/door/etc */
    if (!cave_have_flag_grid(c_ptr, FF_TUNNEL))
    {
        /* Message */
        msg_print("You see nothing there to tunnel.");

        /* Nope */
        return (FALSE);
    }

    /* Okay */
    return (TRUE);
}


/*
 * Perform the basic "tunnel" command
 *
 * Assumes that no monster is blocking the destination
 *
 * Do not use twall anymore
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_tunnel_aux(int y, int x)
{
    cave_type *c_ptr;
    feature_type *f_ptr, *mimic_f_ptr;
    int power;
    cptr name;
    bool more = FALSE;

    /* Verify legality */
    if (!do_cmd_tunnel_test(y, x)) return (FALSE);

    /* Take a turn */
    energy_use = 100;

    /* Get grid */
    c_ptr = &cave[y][x];
    f_ptr = &f_info[c_ptr->feat];
    power = f_ptr->power;

    /* Feature code (applying "mimic" field) */
    mimic_f_ptr = &f_info[get_feat_mimic(c_ptr)];

    name = f_name + mimic_f_ptr->name;

    /* Sound */
    sound(SOUND_DIG);

    if (have_flag(f_ptr->flags, FF_PERMANENT))
    {
        /* Titanium */
        if (have_flag(mimic_f_ptr->flags, FF_PERMANENT))
        {
            msg_print("This seems to be permanent rock.");
        }

        /* Map border (mimiccing Permanent wall) */
        else
        {
            msg_print("You can't tunnel through that!");
        }
    }

    /* Dig or tunnel */
    else if (have_flag(f_ptr->flags, FF_CAN_DIG))
    {
        /* Dig */
        if (p_ptr->skill_dig > randint0(20 * power))
        {
            /* Message */
            msg_format("You have removed the %s.", name);

            /* Remove the feature */
            cave_alter_feat(y, x, FF_TUNNEL);

            /* Update some things */
            p_ptr->update |= (PU_FLOW);
        }
        else
        {
            /* Message, keep digging */
            msg_format("You dig into the %s.", name);

            more = TRUE;
        }
    }

    else
    {
        bool tree = have_flag(mimic_f_ptr->flags, FF_TREE);

        /* Tunnel */
        if (p_ptr->skill_dig > power + randint0(40 * power))
        {
            if (tree) msg_format("You have cleared away the %s.", name);
            else
            {
                msg_print("You have finished the tunnel.");
                p_ptr->update |= (PU_FLOW);
            }

            /* Sound */
            if (have_flag(f_ptr->flags, FF_GLASS)) sound(SOUND_GLASS);

            /* Remove the feature */
            cave_alter_feat(y, x, FF_TUNNEL);

            virtue_add(VIRTUE_DILIGENCE, 1);
            virtue_add(VIRTUE_NATURE, -1);
        }

        /* Keep trying */
        else
        {
            if (tree)
            {
                /* We may continue chopping */
                msg_format("You chop away at the %s.", name);
                /* Occasional Search XXX XXX */
                if (randint0(100) < 25) search();
            }
            else
            {
                /* We may continue tunelling */
                msg_format("You tunnel into the %s.", name);
            }

            more = TRUE;
        }
    }

    if (is_hidden_door(c_ptr))
    {
        /* Occasional Search XXX XXX */
        if (randint0(100) < 25) search();
    }

    /* Result */
    return more;
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
    int            y, x, dir;

    cave_type    *c_ptr;
    s16b feat;

    bool        more = FALSE;


    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a direction to tunnel, or Abort */
    if (get_rep_dir(&dir,FALSE))
    {
        /* Get location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get grid */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* No tunnelling through doors */
        if (have_flag(f_info[feat].flags, FF_DOOR))
        {
            /* Message */
            msg_print("You cannot tunnel through doors.");
        }

        /* No tunnelling through most features */
        else if (!have_flag(f_info[feat].flags, FF_TUNNEL))
        {
            msg_print("You can't tunnel through that.");
        }

        /* A monster is in the way */
        else if (c_ptr->m_idx)
        {
            /* Take a turn */
            energy_use = 100;

            /* Message */
            msg_print("There is a monster in the way!");

            /* Attack */
            py_attack(y, x, 0);
        }

        /* Try digging */
        else
        {
            /* Tunnel through walls */
            more = do_cmd_tunnel_aux(y, x);
        }
    }

    /* Cancel repetition unless we can continue */
    if (!more) disturb(0, 0);
}


#ifdef ALLOW_EASY_OPEN /* TNB */

/*
 * easy_open_door --
 *
 *    If there is a jammed/closed/locked door at the given location,
 *    then attempt to unlock/open it. Return TRUE if an attempt was
 *    made (successful or not), otherwise return FALSE.
 *
 *    The code here should be nearly identical to that in
 *    do_cmd_open_test() and do_cmd_open_aux().
 */
bool easy_open_door(int y, int x)
{
    int i, j;

    cave_type *c_ptr = &cave[y][x];
    feature_type *f_ptr = &f_info[c_ptr->feat];

    /* Must be a closed door */
    if (!is_closed_door(c_ptr->feat))
    {
        /* Nope */
        return (FALSE);
    }

    /* Jammed door */
    if (!have_flag(f_ptr->flags, FF_OPEN))
    {
        /* Stuck */
        msg_format("The %s appears to be stuck.", f_name + f_info[get_feat_mimic(c_ptr)].name);

    }

    /* Locked door */
    else if (f_ptr->power)
    {
        /* Disarm factor */
        i = p_ptr->skills.dis;

        /* Penalize some conditions */
        if (p_ptr->blind || no_lite()) i = i / 10;
        if (p_ptr->confused || p_ptr->image) i = i / 10;

        /* Extract the lock power */
        j = f_ptr->power;

        /* Extract the difficulty XXX XXX XXX */
        j = i - (j * 4);

        /* Always have a small chance of success */
        if (j < 2) j = 2;

        /* Success */
        if (randint0(100) < j)
        {
            /* Message */
            msg_print("You have picked the lock.");

            /* Open the door */
            cave_alter_feat(y, x, FF_OPEN);

            /* Sound */
            sound(SOUND_OPENDOOR);

            /* Experience */
            gain_exp(1);
        }

        /* Failure */
        else
        {
            /* Failure */
            if (flush_failure) flush();

            /* Message */
            msg_print("You failed to pick the lock.");

        }
    }

    /* Closed door */
    else
    {
        /* Open the door */
        cave_alter_feat(y, x, FF_OPEN);

        /* Sound */
        sound(SOUND_OPENDOOR);
    }

    /* Result */
    return (TRUE);
}

#endif /* ALLOW_EASY_OPEN -- TNB */


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
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


    /* Take a turn */
    energy_use = 100;

    /* Get the "disarm" factor */
    i = p_ptr->skills.dis;

    /* Penalize some conditions */
    if (p_ptr->blind || no_lite()) i = i / 10;
    if (p_ptr->confused || p_ptr->image) i = i / 10;

    /* Extract the difficulty */
    j = i - o_ptr->pval;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Must find the trap first. */
    if (!object_is_known(o_ptr))
    {
        msg_print("I don't see any traps.");

    }

    /* Already disarmed/unlocked */
    else if (o_ptr->pval <= 0)
    {
        msg_print("The chest is not trapped.");

    }

    /* No traps to find. */
    else if (!chest_traps[o_ptr->pval])
    {
        msg_print("The chest is not trapped.");

    }

    /* Success (get a lot of experience) */
    else if (randint0(100) < j)
    {
        msg_print("You have disarmed the chest.");

        gain_exp(o_ptr->pval);
        o_ptr->pval = (0 - o_ptr->pval);
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && (randint1(i) > 5))
    {
        /* We may keep trying */
        more = TRUE;
        if (flush_failure) flush();
        msg_print("You failed to disarm the chest.");

    }

    /* Failure -- Set off the trap */
    else
    {
        msg_print("You set off a trap!");

        sound(SOUND_FAIL);
        chest_trap(y, x, o_idx);
    }

    /* Result */
    return (more);
}


/*
 * Perform the basic "disarm" command
 *
 * Assume destination is a visible trap
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
#ifdef ALLOW_EASY_DISARM /* TNB */

bool do_cmd_disarm_aux(int y, int x, int dir)

#else /* ALLOW_EASY_DISARM -- TNB */

static bool do_cmd_disarm_aux(int y, int x, int dir)

#endif /* ALLOW_EASY_DISARM -- TNB */
{
    /* Get grid and contents */
    cave_type *c_ptr = &cave[y][x];

    /* Get feature */
    feature_type *f_ptr = &f_info[c_ptr->feat];

    /* Access trap name */
    cptr name = (f_name + f_ptr->name);

    /* Extract trap "power" */
    int power = f_ptr->power;

    bool more = FALSE;

    /* Get the "disarm" factor */
    int i = p_ptr->skills.dis;

    int j;

    /* Take a turn */
    energy_use = 100;

    /* Penalize some conditions */
    if (p_ptr->blind || no_lite()) i = i / 10;
    if (p_ptr->confused || p_ptr->image) i = i / 10;

    /* Extract the difficulty */
    j = i - power;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Success */
    if (randint0(100) < j)
    {
        /* Message */
        msg_format("You have disarmed the %s.", name);

        /* Reward */
        gain_exp(power);

        /* Remove the trap */
        cave_alter_feat(y, x, FF_DISARM);

#ifdef ALLOW_EASY_DISARM /* TNB */

        /* Move the player onto the trap */
        move_player(dir, easy_disarm, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

        /* move the player onto the trap grid */
        move_player(dir, FALSE, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
    }

    /* Failure -- Keep trying */
    else if ((i > 5) && (randint1(i) > 5))
    {
        /* Failure */
        if (flush_failure) flush();

        /* Message */
        msg_format("You failed to disarm the %s.", name);

        /* We may keep trying */
        more = TRUE;
    }

    /* Failure -- Set off the trap */
    else
    {
        /* Message */
        msg_format("You set off the %s!", name);

#ifdef ALLOW_EASY_DISARM /* TNB */

        /* Move the player onto the trap */
        move_player(dir, easy_disarm, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

        /* Move the player onto the trap */
        move_player(dir, FALSE, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */
    }

    /* Result */
    return (more);
}


/*
 * Disarms a trap, or chest
 */
void do_cmd_disarm(void)
{
    int y, x, dir;

    s16b o_idx;

    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

#ifdef ALLOW_EASY_DISARM /* TNB */

    /* Option: Pick a direction */
    if (easy_disarm)
    {
        int num_traps, num_chests;

        /* Count visible traps */
        num_traps = count_dt(&y, &x, is_trap, TRUE);

        /* Count chests (trapped) */
        num_chests = count_chests(&y, &x, TRUE);

        /* See if only one target */
        if (num_traps || num_chests)
        {
            bool too_many = (num_traps && num_chests) || (num_traps > 1) ||
                (num_chests > 1);
            if (!too_many) command_dir = coords_to_dir(y, x);
        }
    }

#endif /* ALLOW_EASY_DISARM -- TNB */

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a direction (or abort) */
    if (get_rep_dir(&dir,TRUE))
    {
        cave_type *c_ptr;
        s16b feat;

        /* Get location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get grid and contents */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Check for chests */
        o_idx = chest_check(y, x);

        /* Disarm a trap */
        if (!is_trap(feat) && !o_idx)
        {
            /* Message */
            msg_print("You see nothing there to disarm.");

        }

        /* Monster in the way */
        else if (c_ptr->m_idx && p_ptr->riding != c_ptr->m_idx)
        {
            /* Message */
            msg_print("There is a monster in the way!");


            /* Attack */
            py_attack(y, x, 0);
        }

        /* Disarm chest */
        else if (o_idx)
        {
            /* Disarm the chest */
            more = do_cmd_disarm_chest(y, x, o_idx);
        }

        /* Disarm trap */
        else
        {
            /* Disarm the trap */
            more = do_cmd_disarm_aux(y, x, dir);
        }
    }

    /* Cancel repeat unless told not to */
    if (!more) disturb(0, 0);
}


/*
 * Perform the basic "bash" command
 *
 * Assume destination is a closed/locked/jammed door
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_bash_aux(int y, int x, int dir)
{
    /* Get grid */
    cave_type    *c_ptr = &cave[y][x];

    /* Get feature */
    feature_type *f_ptr = &f_info[c_ptr->feat];

    /* Hack -- Bash power based on strength */
    /* (Ranges from 3 to 20 to 100 to 200) */
    int bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

    /* Extract door power */
    int temp = f_ptr->power;

    bool        more = FALSE;

    cptr name = f_name + f_info[get_feat_mimic(c_ptr)].name;

    /* Take a turn */
    energy_use = 100;

    /* Message */
    msg_format("You smash into the %s!", name);

    /* Compare bash power to door power XXX XXX XXX */
    temp = (bash - (temp * 10));

    if (p_ptr->pclass == CLASS_BERSERKER) temp *= 2;

    /* Hack -- always have a chance */
    if (temp < 1) temp = 1;

    /* Hack -- attempt to bash down the door */
    if (randint0(100) < temp)
    {
        /* Message */
        msg_format("The %s crashes open!", name);

        /* Sound */
        sound(have_flag(f_ptr->flags, FF_GLASS) ? SOUND_GLASS : SOUND_OPENDOOR);

        /* Break down the door */
        if ((randint0(100) < 50) || (feat_state(c_ptr->feat, FF_OPEN) == c_ptr->feat) || have_flag(f_ptr->flags, FF_GLASS))
        {
            cave_alter_feat(y, x, FF_BASH);
        }

        /* Open the door */
        else
        {
            cave_alter_feat(y, x, FF_OPEN);
        }

        /* Hack -- Fall through the door */
        move_player(dir, FALSE, FALSE);
    }

    /* Saving throw against stun */
    else if (randint0(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
         p_ptr->lev)
    {
        /* Message */
        msg_format("The %s holds firm.", name);


        /* Allow repeated bashing */
        more = TRUE;
    }

    /* High dexterity yields coolness */
    else
    {
        /* Message */
        msg_print("You are off-balance.");


        /* Hack -- Lose balance ala paralysis */
        (void)set_paralyzed(randint1(4), FALSE);
    }

    /* Result */
    return (more);
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
    int            y, x, dir;

    cave_type    *c_ptr;

    bool        more = FALSE;


    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        s16b feat;

        /* Bash location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get grid */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Nothing useful */
        if (!have_flag(f_info[feat].flags, FF_BASH))
        {
            /* Message */
            msg_print("You see nothing there to bash.");

        }

        /* Monster in the way */
        else if (c_ptr->m_idx)
        {
            /* Take a turn */
            energy_use = 100;

            /* Message */
            msg_print("There is a monster in the way!");


            /* Attack */
            py_attack(y, x, 0);
        }

        /* Bash a closed door */
        else
        {
            /* Bash the door */
            more = do_cmd_bash_aux(y, x, dir);
        }
    }

    /* Unless valid action taken, cancel bash */
    if (!more) disturb(0, 0);
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
    int            y, x, dir;

    cave_type    *c_ptr;

    bool        more = FALSE;


    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a direction */
    if (get_rep_dir(&dir,TRUE))
    {
        s16b feat;
        feature_type *f_ptr;

        /* Get location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get grid */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);
        f_ptr = &f_info[feat];

        /* Take a turn */
        energy_use = 100;

        /* Attack monsters */
        if (c_ptr->m_idx)
        {
            /* Attack */
            py_attack(y, x, 0);
        }

        /* Locked doors */
        else if (have_flag(f_ptr->flags, FF_OPEN))
        {
            more = do_cmd_open_aux(y, x);
        }

        /* Bash jammed doors */
        else if (have_flag(f_ptr->flags, FF_BASH))
        {
            more = do_cmd_bash_aux(y, x, dir);
        }

        /* Tunnel through walls */
        else if (have_flag(f_ptr->flags, FF_TUNNEL))
        {
            more = do_cmd_tunnel_aux(y, x);
        }

        /* Close open doors */
        else if (have_flag(f_ptr->flags, FF_CLOSE))
        {
            more = do_cmd_close_aux(y, x);
        }

        /* Disarm traps */
        else if (have_flag(f_ptr->flags, FF_DISARM))
        {
            more = do_cmd_disarm_aux(y, x, dir);
        }

        /* Oops */
        else
        {
            /* Oops */
            msg_print("You attack the empty air.");

        }
    }

    /* Cancel repetition unless we can continue */
    if (!more) disturb(0, 0);
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
void do_cmd_spike(void)
{
    int dir;

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        int y, x, item;
        cave_type *c_ptr;
        s16b feat;

        /* Get location */
        y = py + ddy[dir];
        x = px + ddx[dir];

        /* Get grid and contents */
        c_ptr = &cave[y][x];

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Require closed door */
        if (!have_flag(f_info[feat].flags, FF_SPIKE))
        {
            /* Message */
            msg_print("You see nothing there to spike.");

        }

        /* Get a spike */
        else if (!get_spike(&item))
        {
            /* Message */
            msg_print("You have no spikes!");
        }

        /* Is a monster in the way? */
        else if (c_ptr->m_idx)
        {
            /* Take a turn */
            energy_use = 100;

            /* Message */
            msg_print("There is a monster in the way!");

            /* Attack */
            py_attack(y, x, 0);
        }

        /* Go for it */
        else
        {
            /* Take a turn */
            energy_use = 100;

            /* Successful jamming */
            msg_format("You jam the %s with a spike.", f_name + f_info[feat].name);

            cave_alter_feat(y, x, FF_SPIKE);

            /* Use up, and describe, a single spike, from the bottom */
            inven_item_increase(item, -1);
            inven_item_describe(item);
            inven_item_optimize(item);
        }
    }
}



/*
 * Support code for the "Walk" and "Jump" commands
 */
void do_cmd_walk(bool pickup)
{
    int dir;

    bool more = FALSE;


    /* Allow repeated command */
    if (command_arg)
    {
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        /* Take a turn */
        energy_use = 100;

        if ((dir != 5) && (p_ptr->special_defense & KATA_MUSOU))
        {
            set_action(ACTION_NONE);
        }

        /* Hack -- In small scale wilderness it takes MUCH more time to move */
        if (p_ptr->wild_mode) energy_use *= ((MAX_HGT + MAX_WID) / 2);
        
        if (p_ptr->action == ACTION_QUICK_WALK) energy_use = energy_use * (45-(p_ptr->lev/2)) / 100;
        if (p_ptr->action == ACTION_STALK) energy_use = energy_use * (175 - p_ptr->lev) / 100;
        if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
            energy_use = energy_use * (45-(p_ptr->lev/2)) / 100;
        
        if (!p_ptr->wild_mode && p_ptr->tim_shrike) 
            energy_use /= 3;
        else if (p_ptr->quick_walk)
            energy_use = energy_use * 60 / 100;

        if (prace_is_(RACE_MON_GOLEM))
            energy_use *= 2;

        /* Actually move the character */
        move_player(dir, pickup, FALSE);

        /* Allow more walking */
        more = TRUE;
    }

    /* Hack again -- Is there a special encounter ??? */
    if (p_ptr->wild_mode && !cave_have_flag_bold(py, px, FF_TOWN))
    {
        int lvl = wilderness_level(px, py);
        int tmp = MAX(1, 120 + p_ptr->lev*10 - lvl + 5);

        if (wilderness[py][px].road)
            tmp *= 3;

        if (!is_daytime())
            tmp /= 2;

#if 0
        msg_format("Ambush=%.2f%%", (double)(21 - p_ptr->skills.stl) * 100.0/(double)tmp);
#endif

        if ( lvl + 5 > p_ptr->lev / 2 
          && randint0(tmp) < 21 - p_ptr->skills.stl )
        {
            /* Inform the player of his horrible fate :=) */
            msg_print("You are ambushed!");

            /* Go into large wilderness view */
            p_ptr->oldpy = rand_range(15, MAX_HGT - 15);
            p_ptr->oldpx = rand_range(15, MAX_WID - 15);
            change_wild_mode();

            /* Give first move to monsters */
            energy_use = 100;

            /* Hack -- set the encouter flag for the wilderness generation */
            generate_encounter = TRUE;
        }
    }

    /* Cancel repeat unless we may continue */
    if (!more) disturb(0, 0);
}



/*
 * Start running.
 */
void do_cmd_run(void)
{
    int dir;

    /* Hack -- no running when confused */
    if (p_ptr->confused)
    {
        msg_print("You are too confused!");

        return;
    }

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        /* Hack -- Set the run counter */
        running = (command_arg ? command_arg : 1000);

        /* First step */
        run_step(dir);
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
        /* Set repeat count */
        command_rep = command_arg - 1;

        /* Redraw the state */
        p_ptr->redraw |= (PR_STATE);

        /* Cancel the arg */
        command_arg = 0;
    }

    /* Take a turn */
    energy_use = 100;

    if (pickup) mpe_mode |= MPE_DO_PICKUP;
    (void)move_player_effect(py, px, mpe_mode);
}



/*
 * Resting allows a player to safely restore his hp    -RAK-
 */
void do_cmd_rest(void)
{

    set_action(ACTION_NONE);
    if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
        weaponmaster_set_toggle(TOGGLE_NONE);

    if ((p_ptr->pclass == CLASS_BARD) && (p_ptr->magic_num1[0] || p_ptr->magic_num1[1]))
    {
        bard_stop_singing();
    }

    /* Hex */
    if (hex_spelling_any()) stop_hex_spell_all();

    warlock_stop_singing();

    /* Prompt for time if needed */
    if (command_arg <= 0)
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
            if (p_ptr->blind || 
                p_ptr->confused ||
                p_ptr->poisoned ||
                p_ptr->afraid ||
                p_ptr->stun ||
                p_ptr->cut ||
                p_ptr->slow || 
                p_ptr->paralyzed ||
                p_ptr->image ||
                p_ptr->word_recall ||
                p_ptr->alter_reality)
            {
                clear = FALSE;
            }
        }

        if (clear)
            mimic_race(MIMIC_NONE, "You cannot rest while maintaining your current form.");
    }

    if (p_ptr->special_defense & NINJA_S_STEALTH) set_superstealth(FALSE);

    /* Take a turn XXX XXX XXX (?) */
    energy_use = 100;

    /* The sin of sloth */
    if (command_arg > 100)
        virtue_add(VIRTUE_DILIGENCE, -1);
    
    /* Why are you sleeping when there's no need?  WAKE UP!*/
    if ((p_ptr->chp == p_ptr->mhp) &&
        (p_ptr->csp == p_ptr->msp) &&
        !p_ptr->blind && !p_ptr->confused &&
        !p_ptr->poisoned && !p_ptr->afraid &&
        !p_ptr->stun && !p_ptr->cut &&
        !p_ptr->slow && !p_ptr->paralyzed &&
        !p_ptr->image && !p_ptr->word_recall &&
        !p_ptr->alter_reality &&
        !magic_eater_can_regen())
    {
        virtue_add(VIRTUE_DILIGENCE, -1);
    }
    /* Save the rest code */
    resting = command_arg;
    p_ptr->action = ACTION_REST;

    /* Recalculate bonuses */
    p_ptr->update |= (PU_BONUS);

    /* Redraw the state */
    p_ptr->redraw |= (PR_STATE);

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
static int breakage_chance(object_type *o_ptr)
{
    int archer_bonus = (p_ptr->pclass == CLASS_ARCHER ? (p_ptr->lev-1)/7 + 4: 0);

    /* Hack: Bowmasters should have 75% protection ... */
    if (weaponmaster_is_(WEAPONMASTER_BOWS) && p_ptr->lev >= 20)
        archer_bonus = 7;

    /* Examine the snipe type */
    if (snipe_type)
    {
        if (snipe_type == SP_KILL_WALL) return (100);
        if (snipe_type == SP_EXPLODE) return (100);
        if (snipe_type == SP_PIERCE) return (100);
        if (snipe_type == SP_FINAL) return (100);
        if (snipe_type == SP_NEEDLE) return (100);
        if (snipe_type == SP_EVILNESS) return (40);
        if (snipe_type == SP_HOLYNESS) return (40);
    }

    if (shoot_hack == SHOOT_SHATTER) return 100;
    if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT) return 100;
    if (shoot_hack == SHOOT_ELEMENTAL) return 100;
    if (weaponmaster_get_toggle() == TOGGLE_OVERDRAW) return 100;

    /* Examine the item type */
    switch (o_ptr->tval)
    {
        /* Always break */
        case TV_FLASK:
        case TV_POTION:
        case TV_BOTTLE:
        case TV_FOOD:
        case TV_JUNK:
            return (100);

        /* Often break */
        case TV_LITE:
        case TV_SCROLL:
        case TV_SKELETON:
            return (50);

        /* Sometimes break */
        case TV_WAND:
        case TV_SPIKE:
            return (25);
        case TV_ARROW:
            return (20 - archer_bonus * 2);

        /* Rarely break */
        case TV_SHOT:
        case TV_BOLT:
            return (10 - archer_bonus);
        default:
            return (10);
    }
}


static s16b tot_dam_aux_shot(object_type *o_ptr, int tdam, monster_type *m_ptr)
{
    int mult = 10;
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    u32b flgs[OF_ARRAY_SIZE];

    /* Extract the flags */
    missile_flags(o_ptr, flgs);

    /* Some "weapons" and "ammo" do extra damage */
    switch (o_ptr->tval)
    {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            /* Slay Animal */
            if ((have_flag(flgs, OF_SLAY_ANIMAL)) &&
                (r_ptr->flags3 & RF3_ANIMAL))
            {
                mon_lore_3(m_ptr, RF3_ANIMAL);
                obj_learn_slay(o_ptr, OF_SLAY_ANIMAL, "slays <color:g>Animals</color>");
                if (mult < 17) mult = 17;
            }

            /* Kill Animal */
            if ((have_flag(flgs, OF_KILL_ANIMAL)) &&
                (r_ptr->flags3 & RF3_ANIMAL))
            {
                mon_lore_3(m_ptr, RF3_ANIMAL);
                obj_learn_slay(o_ptr, OF_KILL_ANIMAL, "slays <color:g>*Animals*</color>");
                if (mult < 27) mult = 27;
            }

            /* Slay Evil */
            if ((have_flag(flgs, OF_SLAY_EVIL)) &&
                (r_ptr->flags3 & RF3_EVIL))
            {
                mon_lore_3(m_ptr, RF3_EVIL);
                obj_learn_slay(o_ptr, OF_SLAY_EVIL, "slays <color:y>Evil</color>");
                if (mult < 15) mult = 15;
            }

            /* Kill Evil */
            if ((have_flag(flgs, OF_KILL_EVIL)) &&
                (r_ptr->flags3 & RF3_EVIL))
            {
                mon_lore_3(m_ptr, RF3_EVIL);
                obj_learn_slay(o_ptr, OF_KILL_EVIL, "slays <color:y>*Evil*</color>");
                if (mult < 25) mult = 25;
            }

            /* Slay Human */
            if ((have_flag(flgs, OF_SLAY_HUMAN)) &&
                (r_ptr->flags2 & RF2_HUMAN))
            {
                mon_lore_2(m_ptr, RF2_HUMAN);
                obj_learn_slay(o_ptr, OF_SLAY_HUMAN, "slays <color:s>Humans</color>");
                if (mult < 17) mult = 17;
            }

            /* Kill Human */
            if ((have_flag(flgs, OF_KILL_HUMAN)) &&
                (r_ptr->flags2 & RF2_HUMAN))
            {
                mon_lore_2(m_ptr, RF2_HUMAN);
                obj_learn_slay(o_ptr, OF_KILL_HUMAN, "slays <color:s>*Humans*</color>");
                if (mult < 27) mult = 27;
            }

            /* Slay Undead */
            if ((have_flag(flgs, OF_SLAY_UNDEAD)) &&
                (r_ptr->flags3 & RF3_UNDEAD))
            {
                mon_lore_3(m_ptr, RF3_UNDEAD);
                obj_learn_slay(o_ptr, OF_SLAY_UNDEAD, "slays <color:D>Undead</color>");
                if (mult < 20) mult = 20;
            }

            /* Kill Undead */
            if ((have_flag(flgs, OF_KILL_UNDEAD)) &&
                (r_ptr->flags3 & RF3_UNDEAD))
            {
                mon_lore_3(m_ptr, RF3_UNDEAD);
                obj_learn_slay(o_ptr, OF_KILL_UNDEAD, "slays <color:D>*Undead*</color>");
                if (mult < 30) mult = 30;
            }

            /* Slay Demon */
            if ((have_flag(flgs, OF_SLAY_DEMON)) &&
                (r_ptr->flags3 & RF3_DEMON))
            {
                mon_lore_3(m_ptr, RF3_DEMON);
                obj_learn_slay(o_ptr, OF_SLAY_DEMON, "slays <color:R>Demons</color>");
                if (mult < 20) mult = 20;
            }

            /* Kill Demon */
            if ((have_flag(flgs, OF_KILL_DEMON)) &&
                (r_ptr->flags3 & RF3_DEMON))
            {
                mon_lore_3(m_ptr, RF3_DEMON);
                obj_learn_slay(o_ptr, OF_KILL_DEMON, "slays <color:R>*Demons*</color>");
                if (mult < 30) mult = 30;
            }

            /* Slay Orc */
            if ((have_flag(flgs, OF_SLAY_ORC)) &&
                (r_ptr->flags3 & RF3_ORC))
            {
                mon_lore_3(m_ptr, RF3_ORC);
                obj_learn_slay(o_ptr, OF_SLAY_ORC, "slays <color:U>Orcs</color>");
                if (mult < 20) mult = 20;
            }

            /* Kill Orc */
            if ((have_flag(flgs, OF_KILL_ORC)) &&
                (r_ptr->flags3 & RF3_ORC))
            {
                mon_lore_3(m_ptr, RF3_ORC);
                obj_learn_slay(o_ptr, OF_KILL_ORC, "slays <color:U>*Orcs*</color>");
                if (mult < 30) mult = 30;
            }

            /* Slay Troll */
            if ((have_flag(flgs, OF_SLAY_TROLL)) &&
                (r_ptr->flags3 & RF3_TROLL))
            {
                mon_lore_3(m_ptr, RF3_TROLL);
                obj_learn_slay(o_ptr, OF_SLAY_TROLL, "slays <color:g>Trolls</color>");
                if (mult < 20) mult = 20;
            }

            /* Kill Troll */
            if ((have_flag(flgs, OF_KILL_TROLL)) &&
                (r_ptr->flags3 & RF3_TROLL))
            {
                mon_lore_3(m_ptr, RF3_TROLL);
                obj_learn_slay(o_ptr, OF_KILL_TROLL, "slays <color:g>*Trolls*</color>");
                if (mult < 30) mult = 30;
            }

            /* Slay Giant */
            if ((have_flag(flgs, OF_SLAY_GIANT)) &&
                (r_ptr->flags3 & RF3_GIANT))
            {
                mon_lore_3(m_ptr, RF3_GIANT);
                obj_learn_slay(o_ptr, OF_SLAY_GIANT, "slays <color:u>Giants</color>");
                if (mult < 20) mult = 20;
            }

            /* Kill Giant */
            if ((have_flag(flgs, OF_KILL_GIANT)) &&
                (r_ptr->flags3 & RF3_GIANT))
            {
                mon_lore_3(m_ptr, RF3_GIANT);
                obj_learn_slay(o_ptr, OF_KILL_GIANT, "slays <color:u>*Giants*</color>");
                if (mult < 30) mult = 30;
            }

            /* Slay Dragon  */
            if ((have_flag(flgs, OF_SLAY_DRAGON)) &&
                (r_ptr->flags3 & RF3_DRAGON))
            {
                mon_lore_3(m_ptr, RF3_DRAGON);
                obj_learn_slay(o_ptr, OF_SLAY_DRAGON, "slays <color:r>Dragons</color>");
                if (mult < 20) mult = 20;
            }

            /* Execute Dragon */
            if ((have_flag(flgs, OF_KILL_DRAGON)) &&
                (r_ptr->flags3 & RF3_DRAGON))
            {
                mon_lore_3(m_ptr, RF3_DRAGON);
                obj_learn_slay(o_ptr, OF_KILL_DRAGON, "slays <color:r>*Dragons*</color>");
                if (mult < 30) mult = 30;

                if ( o_ptr->name1 == ART_BARD_ARROW 
                  && m_ptr->r_idx == MON_SMAUG 
                  && equip_find_artifact(ART_BARD) )
                {
                    mult *= 5;
                }
            }

            /* Brand (Acid) */
            if (have_flag(flgs, OF_BRAND_ACID))
            {
                if (r_ptr->flagsr & RFR_EFF_IM_ACID_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_IM_ACID_MASK);
                }
                else
                {
                    obj_learn_slay(o_ptr, OF_BRAND_ACID, "is <color:g>Acid Branded</color>");
                    if (mult < 17) mult = 17;
                }
            }

            /* Brand (Elec) */
            if (have_flag(flgs, OF_BRAND_ELEC))
            {
                if (r_ptr->flagsr & RFR_EFF_IM_ELEC_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_IM_ELEC_MASK);
                }
                else
                {
                    obj_learn_slay(o_ptr, OF_BRAND_ELEC, "is <color:b>Lightning Branded</color>");
                    if (mult < 17) mult = 17;
                }
            }

            /* Brand (Fire) */
            if (have_flag(flgs, OF_BRAND_FIRE))
            {
                if (r_ptr->flagsr & RFR_EFF_IM_FIRE_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_IM_FIRE_MASK);
                }
                else
                {
                    obj_learn_slay(o_ptr, OF_BRAND_FIRE, "has <color:r>Flame Tongue</color>");
                    if (r_ptr->flags3 & RF3_HURT_FIRE)
                    {
                        mon_lore_3(m_ptr, RF3_HURT_FIRE);
                        if (mult < 25) mult = 25;
                    }
                    else if (mult < 17) mult = 17;
                }
            }

            /* Brand (Cold) */
            if (have_flag(flgs, OF_BRAND_COLD))
            {
                if (r_ptr->flagsr & RFR_EFF_IM_COLD_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_IM_COLD_MASK);
                }
                else
                {
                    obj_learn_slay(o_ptr, OF_BRAND_COLD, "is <color:W>Frost Branded</color>");
                    if (r_ptr->flags3 & RF3_HURT_COLD)
                    {
                        if (mult < 25) mult = 25;
                        mon_lore_3(m_ptr, RF3_HURT_COLD);
                    }
                    else if (mult < 17) mult = 17;
                }
            }

            /* Brand (Poison) */
            if (have_flag(flgs, OF_BRAND_POIS))
            {
                if (r_ptr->flagsr & RFR_EFF_IM_POIS_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_IM_POIS_MASK);
                }
                else
                {
                    obj_learn_slay(o_ptr, OF_BRAND_POIS, "has <color:G>Viper's Fang</color>");
                    if (mult < 17) mult = 17;
                }
            }

            if ( (have_flag(flgs, OF_BRAND_MANA) || p_ptr->tim_force) 
              && (p_ptr->csp > (p_ptr->msp / 30)))
            {
                p_ptr->csp -= (1+(p_ptr->msp / 30));
                p_ptr->redraw |= (PR_MANA);
                mult = mult * 3 / 2;
                obj_learn_slay(o_ptr, OF_BRAND_MANA, "is <color:B>Mana Branded</color>");
            }

            break;
        }
    }

    /* Sniper */
    if (snipe_type) mult = tot_dam_aux_snipe(mult, m_ptr);

    /* Return the total damage */
    return (tdam * mult / 10);
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
#define SIGN(x) ((x) > 0 ? 1 : ((x) < 0 ? -1: 0))
bool do_cmd_fire_aux1(int item, object_type *bow)
{
    int dir;
    int tdis, tx, ty;

    tdis = bow_range(bow);
    project_length = tdis + 1;

    if (shoot_hack == SHOOT_DISINTEGRATE)
    {
        if (target_set(TARGET_DISI))
        {
            if (target_who > 0)
            {
                tx = m_list[target_who].fx;
                ty = m_list[target_who].fy;
            }
            else
            {
                tx = target_col;
                ty = target_row;
            }
        }
        else
        {
            energy_use = 0;
            return FALSE;
        }
    }
    else
    {
        if (!get_aim_dir(&dir))
        {
            energy_use = 0;
            if (snipe_type == SP_AWAY) snipe_type = SP_NONE;
            return FALSE;
        }

        /* Predict the "target" location */
        tx = px + 99 * ddx[dir];
        ty = py + 99 * ddy[dir];

        /* Check for "target request" */
        if ((dir == 5) && target_okay())
        {
            tx = target_col;
            ty = target_row;
        }
    }

    /* Don't shoot at my feet */
    if (tx == px && ty == py)
    {
        energy_use = 0;
        /* project_length is already reset to 0 */
        return FALSE;
    }

    if (!fear_allow_shoot())
    {
        msg_print("You are too scared!");
        energy_use = bow_energy(bow->sval)/p_ptr->shooter_info.num_fire;
        if (snipe_type == SP_AWAY) snipe_type = SP_NONE;
        return FALSE;
    }

    do_cmd_fire_aux2(item, bow, px, py, tx, ty);

    return TRUE;
}
void do_cmd_fire_aux2(int item, object_type *bow, int sx, int sy, int tx, int ty)
{
    int i, j, y, x, ny, nx, prev_y, prev_x, dd;
    int tdam_base, tdis, thits, tmul;
    int bonus, chance;
    int cur_dis, visible;
    bool no_energy = FALSE;
    int num_shots = 1;

    object_type forge, forge2;
    object_type *q_ptr;

    object_type *o_ptr;

    bool hit_body = FALSE;
    bool return_ammo = FALSE;

    char o_name[MAX_NLEN];

    u16b path_g[512];    /* For calcuration of path length */
    int flgs = PROJECT_PATH | PROJECT_THRU;

    int msec = delay_factor * delay_factor * delay_factor;

    bool stick_to = FALSE;

    /* Access the item (if in the pack) */
    if (item >= 0)
    {
        if (item == INVEN_UNLIMITED_QUIVER)
        {
            int k_idx = lookup_kind(p_ptr->shooter_info.tval_ammo, SV_AMMO_NORMAL);
            object_prep(&forge2, k_idx);
            o_ptr = &forge2;
        }
        else
            o_ptr = &inventory[item];
    }
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Sniper - Cannot shot a single arrow twice */
    if ((snipe_type == SP_DOUBLE) && (o_ptr->number < 2)) snipe_type = SP_NONE;
    if (snipe_type == SP_DOUBLE) num_shots = 2;

    /* Describe the object */
    object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_NO_PLURAL | OD_OMIT_INSCRIPTION);

    /* Use the proper number of shots */
    thits = p_ptr->shooter_info.num_fire;

    /* Base damage from thrown object plus launcher bonus */
    dd = o_ptr->dd;
    if (p_ptr->big_shot)
        dd *= 2;
    tdam_base = damroll(dd, o_ptr->ds) + o_ptr->to_d + bow->to_d;
    if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS) && p_ptr->lev >= 15)
        tdam_base += 1 + p_ptr->lev/10;

    /* Actually "fire" the object */
    bonus = (p_ptr->shooter_info.to_h + o_ptr->to_h + bow->to_h);

    switch (shoot_hack)
    {
    case SHOOT_BOUNCE:
        bonus -= 20 * shoot_count;
        break;
    case SHOOT_RUN:
        bonus -= 10;
        no_energy = TRUE;
        break;
    case SHOOT_MANY:
        bonus -= 10;
        no_energy = TRUE;
        break;
    case SHOOT_ALL:
        bonus -= 20;
        no_energy = TRUE;
        break;
    case SHOOT_RETALIATE:
        no_energy = TRUE;
        break;
    case SHOOT_VOLLEY:
    case SHOOT_TRANQUILIZE:
    case SHOOT_NEEDLE:
    case SHOOT_DISINTEGRATE:
    case SHOOT_SHATTER:
    case SHOOT_KNOCKBACK:
    case SHOOT_ELEMENTAL:
        no_energy = TRUE;
        energy_use = 100;
        break;
    }
    
    if (weaponmaster_get_toggle() == TOGGLE_PIERCING_ARROW || shoot_hack == SHOOT_PIERCE)
        shoot_count = 0;

    if (weaponmaster_get_toggle() == TOGGLE_RAPID_SHOT)
    {
        int  frac;
        s16b energy_fire = bow_energy(bow->sval);

        bonus -= 20;
        /* In this mode, whenever the player fires, all of their shots go
           at a single target in rapid succession. Full energy is consumed, and
           the player gets a slight bonus to the number of shots. Think of
           a rapid fire machine gun :) */
        no_energy = TRUE;
        energy_use = 100;

        /* Calculate shots per round 
           CTK: energy_fire has four decimal places implied
                p_ptr->num_fire only has two decimal places implied */
        num_shots = p_ptr->shooter_info.num_fire * 100;  /* rescale to 4 decimal places */
        num_shots = num_shots * 120 / 100;  /* rapid fire gives 1.2x the number of shots */
        frac = (num_shots * 100 / energy_fire) % 100;
        num_shots /= energy_fire;

        if (randint1(100) < frac)
            num_shots++;
    }

    if (bow->sval == SV_LIGHT_XBOW || bow->sval == SV_HEAVY_XBOW)
        chance = (p_ptr->skills.thb + (p_ptr->weapon_exp[0][bow->sval] / 400 + bonus) * BTH_PLUS_ADJ);
    else
        chance = (p_ptr->skills.thb + ((p_ptr->weapon_exp[0][bow->sval] - (WEAPON_EXP_MASTER / 2)) / 200 + bonus) * BTH_PLUS_ADJ);

    if (!no_energy)
        energy_use = bow_energy(bow->sval);

    /* Calculate the Multiplier */
    tmul = bow_mult(bow);

    /* Boost the damage */
    tdam_base *= tmul;
    tdam_base /= 100;

    /* Base range */
    tdis = bow_range(bow);
    project_length = tdis + 1;

    /* Get projection path length */
    if (shoot_hack == SHOOT_DISINTEGRATE)
        flgs |= PROJECT_DISI;

    tdis = project_path(path_g, project_length, sy, sx, ty, tx, flgs) - 1;

    project_length = 0; /* reset to default */

    /* Take a (partial) turn */
    if (!no_energy)
        energy_use = (energy_use / thits); 

    is_fired = TRUE;

    /* Sniper - Difficult to shot twice at 1 turn */
    if (snipe_type == SP_DOUBLE)  p_ptr->concent = (p_ptr->concent + 1) / 2;

    /* Sniper - Repeat shooting when double shots */
    for (i = 0; i < num_shots; i++)
    {
        /* Make sure there is ammo left over for this shot */
        if (item >= 0 && item != INVEN_UNLIMITED_QUIVER)
        {
            if (inventory[item].tval != p_ptr->shooter_info.tval_ammo)
            {
                msg_print("Your ammo has run out. Time to reload!");
                break;
            }
        }

        /* Start at the source */
        y = sy;
        x = sx;

        /* Weaponmaster power:  Ammo is not consumed */
        if ( (p_ptr->return_ammo || o_ptr->name2 == EGO_AMMO_RETURNING)
          && randint1(100) <= 50 + p_ptr->lev/2 )
        {
            return_ammo = TRUE;
        }

        /* Get local object */
        q_ptr = &forge;

        /* Obtain a local object */
        object_copy(q_ptr, o_ptr);

        /* Single object */
        q_ptr->number = 1;

        /* Reduce and describe inventory */
        if (return_ammo)
        {
        }
        else if (item >= 0)
        {
            if (item != INVEN_UNLIMITED_QUIVER)
            {
                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
            }
        }

        /* Reduce and describe floor item */
        else
        {
            floor_item_increase(0 - item, -1);
            floor_item_optimize(0 - item);
        }

        /* Sound */
        sound(SOUND_SHOOT);

        /* Hack -- Handle stuff */
        handle_stuff();

        /* Save the old location */
        prev_y = y;
        prev_x = x;

        /* The shot does not hit yet */
        hit_body = FALSE;

        /* Travel until stopped */
        for (cur_dis = 0; cur_dis <= tdis; )
        {
            cave_type *c_ptr;

            if (weaponmaster_get_toggle() != TOGGLE_PIERCING_ARROW && shoot_hack != SHOOT_PIERCE)
            {
                /* Hack -- Stop at the target */
                if ((y == ty) && (x == tx)) break;
            }

            /* Calculate the new location (see "project()") */
            ny = y;
            nx = x;
            /* Why, after having calculated a projection path, do we not use it?????
            mmove2(&ny, &nx, sy, sx, ty, tx);*/
            ny = GRID_Y(path_g[cur_dis]);
            nx = GRID_X(path_g[cur_dis]);

            /* Shatter Arrow */
            if (snipe_type == SP_KILL_WALL)
            {
                c_ptr = &cave[ny][nx];
                if (cave_have_flag_grid(c_ptr, FF_HURT_ROCK) && !c_ptr->m_idx)
                {
                    if (c_ptr->info & (CAVE_MARK)) msg_print("Wall rocks were shattered.");
                    c_ptr->info &= ~(CAVE_MARK);
                    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);
                    cave_alter_feat(ny, nx, FF_HURT_ROCK);
                    hit_body = TRUE;
                    break;
                }
            }

            if (shoot_hack == SHOOT_DISINTEGRATE)
            {
                c_ptr = &cave[ny][nx];
                if (cave_have_flag_grid(c_ptr, FF_HURT_ROCK) && !c_ptr->m_idx)
                {
                    c_ptr->info &= ~(CAVE_MARK);
                    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);
                    cave_alter_feat(ny, nx, FF_HURT_ROCK);
                }
            }

            /* Stopped by walls/doors */
            if (!cave_have_flag_bold(ny, nx, FF_PROJECT) && !cave[ny][nx].m_idx) break;

            /* Advance the distance */
            cur_dis++;

            /* Sniper */
            if (snipe_type == SP_LITE)
            {
                cave[ny][nx].info |= (CAVE_GLOW);
                note_spot(ny, nx);
                lite_spot(ny, nx);
            }

            /* The player can see the (on screen) missile */
            if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
            {
                char c = object_char(q_ptr);
                byte a = object_attr(q_ptr);

                /* Draw, Hilite, Fresh, Pause, Erase */
                print_rel(c, a, ny, nx);
                move_cursor_relative(ny, nx);
                Term_fresh();
                Term_xtra(TERM_XTRA_DELAY, msec);
                lite_spot(ny, nx);
                Term_fresh();
            }
            /* The player cannot see the missile */
            else
            {
                /* Pause anyway, for consistancy */
                Term_xtra(TERM_XTRA_DELAY, msec);
            }

            /* Sniper */
            if (snipe_type == SP_KILL_TRAP)
            {
                project(0, 0, ny, nx, 0, GF_KILL_TRAP,
                    (PROJECT_JUMP | PROJECT_HIDE | PROJECT_GRID | PROJECT_ITEM), -1);
            }
            if (snipe_type == SP_EVILNESS)
            {
                cave[ny][nx].info &= ~(CAVE_GLOW | CAVE_MARK);
                note_spot(ny, nx);
                lite_spot(ny, nx);
            }

            /* Save the old location */
            prev_y = y;
            prev_x = x;

            /* Save the new location */
            x = nx;
            y = ny;

            /* Hack: Shoot all monsters fires over the heads of intervening monsters */
            if (shoot_hack == SHOOT_ALL && (x != tx || y != ty)) continue;
            if (shoot_hack == SHOOT_VOLLEY && (x != tx || y != ty)) continue;

            /* Monster here, Try to hit it */
            if (cave[y][x].m_idx)
            {
                int armour;
                bool hit = FALSE;
                cave_type *c_ptr = &cave[y][x];

                monster_type *m_ptr = &m_list[c_ptr->m_idx];
                monster_race *r_ptr = &r_info[m_ptr->r_idx];

                /* Check the visibility */
                visible = m_ptr->ml;

                /* Note the collision */
                hit_body = TRUE;

                if (MON_CSLEEP(m_ptr))
                {
                    if (!(r_ptr->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_COMPASSION, -1);
                    if (!(r_ptr->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_HONOUR, -1);
                }

                if ((r_ptr->level + 10) > p_ptr->lev)
                    skills_bow_gain(bow->sval);

                if (p_ptr->riding)
                    skills_riding_gain_archery(r_ptr);

                 armour = MON_AC(r_ptr, m_ptr);
                if (p_ptr->concent)
                {
                    armour *= (10 - p_ptr->concent);
                    armour /= 10;
                }

                if ( p_ptr->painted_target 
                  && p_ptr->painted_target_idx == c_ptr->m_idx 
                  && p_ptr->painted_target_ct >= 3)
                {
                    if (randint1(100) <= 95)
                        hit = TRUE;
                }
                else if (weaponmaster_is_(WEAPONMASTER_BOWS) && cur_dis == 1)
                {
                    hit = TRUE;
                }
                else
                {
                    int chance2 = chance;

                    if (weaponmaster_is_(WEAPONMASTER_BOWS) && p_ptr->lev >= 15)
                        chance2 += 2*(bow_range(bow) - cur_dis);

                    hit = test_hit_fire(chance2 - cur_dis, armour, m_ptr->ml);
                }

                if (p_ptr->painted_target)
                {
                    if (shoot_hack == SHOOT_BOUNCE && shoot_count > 0)
                    {
                        /* A richochet from bouncing pebble should not reset the
                            painted target */
                    }
                    else if (!hit)
                    {
                        p_ptr->painted_target_idx = 0;
                        p_ptr->painted_target_ct = 0;
                    }
                    else if (p_ptr->painted_target_idx == c_ptr->m_idx)
                    {
                        p_ptr->painted_target_ct++;
                    }
                    else
                    {
                        p_ptr->painted_target_idx = c_ptr->m_idx;
                        p_ptr->painted_target_ct = 1;
                    }
                }

                if (hit)
                {
                    bool fear = FALSE;
                    int tdam = tdam_base;

                    /* Get extra damage from concentration */
                    if (p_ptr->concent) tdam = boost_concentration_damage(tdam);

                    /* Handle unseen monster */
                    if (!visible)
                    {
                        msg_format("The %s finds a mark.", o_name);
                    }
                    /* Handle visible monster */
                    else
                    {
                        char m_name[80];
                        monster_desc(m_name, m_ptr, 0);
                        msg_format("The %s hits %s.", o_name, m_name);

                        if (m_ptr->ml)
                        {
                            if (!p_ptr->image) monster_race_track(m_ptr->ap_r_idx);
                            health_track(c_ptr->m_idx);
                        }
                    }

                    if (shoot_hack == SHOOT_NEEDLE)
                    {
                        if ((randint1(randint1(r_ptr->level/7)+5) == 1) && (!(r_ptr->flags1 & RF1_UNIQUE) || m_ptr->r_idx == MON_HAGURE2) && !(r_ptr->flags7 & RF7_UNIQUE2))
                        {
                            char m_name[80];
                            monster_desc(m_name, m_ptr, 0);
                            tdam = m_ptr->hp + 1;
                            msg_format("Your shot hit a fatal spot of %s!", m_name);
                        }
                        else 
                            tdam = 1;
                    }
                    else if (snipe_type == SP_NEEDLE)
                    {
                        if ((randint1(randint1(r_ptr->level / (3 + p_ptr->concent)) + (8 - p_ptr->concent)) == 1)
                            && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flags7 & RF7_UNIQUE2))
                        {
                            char m_name[80];
                            monster_desc(m_name, m_ptr, 0);
                            tdam = m_ptr->hp + 1;
                            msg_format("Your shot hit a fatal spot of %s!", m_name);
                        }
                        else 
                            tdam = 1;
                    }
                    else
                    {
                        critical_t crit = {0};
                        if (shoot_hack != SHOOT_SHATTER && shoot_hack != SHOOT_ELEMENTAL)
                            tdam = tot_dam_aux_shot(q_ptr, tdam, m_ptr);

                        crit = critical_shot(q_ptr->weight, q_ptr->to_h);
                        if (crit.desc)
                        {
                            tdam = tdam * crit.mul/100 + crit.to_d;
                            msg_print(crit.desc);
                        }

                        tdam += p_ptr->shooter_info.to_d;
                        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
                        {
                            if (p_ptr->lev >= 20)
                                tdam += bow_range(bow) - cur_dis;

                            if (p_ptr->lev >= 45)
                            {
                                int mult = 100 + (m_ptr->maxhp - m_ptr->hp)*100/(2*m_ptr->maxhp);
                                tdam = tdam * mult / 100;
                            }
                        }
                        if (tdam < 0) tdam = 0;
                        tdam = mon_damage_mod(m_ptr, tdam, FALSE);
                    }

                    if (snipe_type == SP_EXPLODE)
                    {
                        u16b flg = (PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID);
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, ((p_ptr->concent + 1) / 2 + 1), ny, nx, tdam, GF_MISSILE, flg, -1);
                        break;
                    }
                    if (shoot_hack == SHOOT_ELEMENTAL)
                    {
                        int rad = 0;
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                            rad = randint1(2+p_ptr->lev/40);
                        project(0, rad, ny, nx, tdam, GF_FIRE, flg, -1);
                        project(0, rad, ny, nx, tdam, GF_COLD, flg, -1);
                        project(0, rad, ny, nx, tdam, GF_ELEC, flg, -1);
                        project(0, rad, ny, nx, tdam, GF_ACID, flg, -1);
                        project(0, rad, ny, nx, tdam, GF_POIS, flg, -1);
                        break;
                    }
                    if (shoot_hack == SHOOT_SHATTER)
                    {
                        int rad = 0;
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                            rad = randint1(2+p_ptr->lev/40);
                        tdam = tdam * (100 + p_ptr->lev*2)/100;
                        project(0, rad, ny, nx, tdam, GF_SHARDS, flg, -1);
                        break;
                    }
                    if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                    {
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, randint1(2+p_ptr->lev/40), ny, nx, tdam, GF_MISSILE, flg, -1);
                        break;
                    }
                    if (snipe_type == SP_HOLYNESS)
                    {
                        cave[ny][nx].info |= (CAVE_GLOW);
                        note_spot(ny, nx);
                        lite_spot(ny, nx);
                    }
                    if (mon_take_hit(c_ptr->m_idx, tdam, &fear, NULL))
                    {
                        /* Dead monster ... abort firing additional shots */
                        i = num_shots;
                        p_ptr->painted_target_idx = 0;
                        p_ptr->painted_target_ct = 0;
                    }
                    /* No death */
                    else
                    {
                        bool anger = TRUE;

                        message_pain(c_ptr->m_idx, tdam);

                        if (shoot_hack == SHOOT_TRANQUILIZE)
                        {
                            if (MON_CSLEEP(m_ptr))
                            {
                                if (!one_in_(5))
                                    anger = FALSE;
                            }
                            else if (one_in_(3))
                            {
                                char m_name[80];
                                monster_desc(m_name, m_ptr, 0);
                                msg_format("%^s is put to sleep!", m_name);
                                set_monster_csleep(c_ptr->m_idx, 500);
                                anger = FALSE;
                            }

                            /* mon_take_hit() in xtra2.c hacked to not auto wakeup for SHOOT_TRANQUILIZE */
                            if (anger)
                                (void)set_monster_csleep(c_ptr->m_idx, 0);
                        }

                        if (anger && tdam > 0 && m_ptr->cdis > 1 && allow_ticked_off(r_ptr))
                        {
                            if (mut_present(MUT_PEERLESS_SNIPER))
                            {
                            }
                            else if (p_ptr->tim_stealthy_snipe)
                            {
                            }
                            else if (one_in_(2))
                            {
                                m_ptr->anger_ct++;
                            }
                        }

                        if (anger && tdam > 0) 
                            anger_monster(m_ptr);

                        /* Artifact arrows stick to target. Note, we now do this
                           after hurting/angering the monster since Cupid's Arrow
                           might charm the target, while anger_monster() would set
                           it back to being hostile. */
                        if (object_is_fixed_artifact(q_ptr))
                        {
                            char m_name[80];
                            monster_desc(m_name, m_ptr, 0);

                            stick_to = TRUE;

                            /* If Cupid's Arrow charms the monster,
                               having the arrow stick is highly annoying since
                               you can't command your pets to drop objects they
                               are carrying. */
                            if (q_ptr->name1 == ART_CUPIDS_ARROW && !(r_ptr->flags1 & RF1_UNIQUE))
                            {
                                if (!mon_save_p(m_ptr->r_idx, A_CHR))
                                {
                                    if (!mon_save_p(m_ptr->r_idx, A_CHR))
                                    {
                                        if (!is_pet(m_ptr))
                                        {
                                            set_pet(m_ptr);
                                            msg_format("%^s is charmed!", m_name);
                                            stick_to = FALSE;
                                        }
                                        else if (!is_friendly(m_ptr))
                                        {
                                            set_friendly(m_ptr);
                                            msg_format("%^s suddenly becomes friendly.", m_name);
                                            stick_to = FALSE;
                                        }
                                    }
                                    else if (!is_pet(m_ptr) && !is_friendly(m_ptr))
                                    {
                                        set_friendly(m_ptr);
                                        msg_format("%^s suddenly becomes friendly.", m_name);
                                        stick_to = FALSE;
                                    }
                                }
                            }
                            
                            if (stick_to)
                                msg_format("%^s have stuck into %s!",o_name, m_name);
                        }

                        if (fear && m_ptr->ml)
                        {
                            char m_name[80];
                            sound(SOUND_FLEE);
                            monster_desc(m_name, m_ptr, 0);
                            msg_format("%^s flees in terror!", m_name);
                        }

                        set_target(m_ptr, py, px);

                        if (snipe_type == SP_RUSH || shoot_hack == SHOOT_KNOCKBACK)
                        {
                            int n = randint1(5) + 3;
                            int m_idx = c_ptr->m_idx;

                            for ( ; cur_dis <= tdis; )
                            {
                                int ox = nx;
                                int oy = ny;

                                if (!n) break;

                                /* Calculate the new location (see "project()") */
                                mmove2(&ny, &nx, sy, sx, ty, tx);

                                /* Stopped by wilderness boundary */
                                if (!in_bounds2(ny, nx)) break;

                                /* Stopped by walls/doors */
                                if (!player_can_enter(cave[ny][nx].feat, 0)) break;

                                /* Stopped by monsters */
                                if (!cave_empty_bold(ny, nx)) break;

                                cave[ny][nx].m_idx = m_idx;
                                cave[oy][ox].m_idx = 0;

                                m_ptr->fx = nx;
                                m_ptr->fy = ny;

                                /* Update the monster (new location) */
                                update_mon(c_ptr->m_idx, TRUE);

                                lite_spot(ny, nx);
                                lite_spot(oy, ox);

                                Term_fresh();
                                Term_xtra(TERM_XTRA_DELAY, msec);

                                x = nx;
                                y = ny;
                                cur_dis++;
                                n--;
                            }
                        }
                    }
                }
                else
                {
                    char m_name[80];
                    monster_desc(m_name, m_ptr, 0);
                    msg_format("The %s misses %s.", o_name, m_name);
                }

                /* The following effects (piercing and bouncing) should
                   not take place when artifact ammo has stuck to a unique! */
                if (!stick_to)
                {
                    if (snipe_type == SP_PIERCE)
                    {
                        if(p_ptr->concent < 1) break;
                        p_ptr->concent--;
                        continue;
                    }

                    if (hit && shoot_count < 5 && (shoot_hack == SHOOT_PIERCE || weaponmaster_get_toggle() == TOGGLE_PIERCING_ARROW))
                    {
                        /*  @.....+......o
                            Is there a ghost standing on the door? Double check the terrain
                            before allowing a pierce to continue towards that orc.
                        */
                        if (!cave_have_flag_bold(ny, nx, FF_PROJECT)) break;
                        chance -= 20 * BTH_PLUS_ADJ;
                        shoot_count++;
                        continue;
                    }

                    if (shoot_hack == SHOOT_BOUNCE)
                    {
                        int dir = randint1(9);
                        if (dir != 5)
                        {
                            /* For now, only one bounce ... Consider allowing a chance
                               of multiple bounces (e.g. one_in_(shoot_count)) or just
                               letting the pebble bounce until it fails to hit a monster */
                            shoot_count++;
                            if (shoot_count <= 5)
                            {
                                tx = x + 99 * ddx[dir];
                                ty = y + 99 * ddy[dir];
                                do_cmd_fire_aux2(item, bow, x, y, tx, ty);
                                return;
                            }
                        }
                    }
                }

                /* Stop looking */
                break;
            }
        }

        /* Chance of breakage (during attacks) */
        j = (hit_body ? breakage_chance(q_ptr) : 0);
        if (shoot_hack == SHOOT_DISINTEGRATE) j = 100;

        if (item == INVEN_UNLIMITED_QUIVER)
        {
            if (disturb_minor)
                msg_print("Your quiver seems endless.");
        }
        else if (return_ammo)
        {
            if (disturb_minor)
                msg_format("The %s returns to your pack.", o_name);
        }
        else if (stick_to)
        {
            int m_idx = cave[y][x].m_idx;
            monster_type *m_ptr = &m_list[m_idx];
            int o_idx = o_pop();

            if (!o_idx)
            {
                msg_format("The %s have gone to somewhere.", o_name);
                if (object_is_fixed_artifact(q_ptr))
                    a_info[q_ptr->name1].generated = FALSE;
                if (random_artifacts && q_ptr->name3)
                    a_info[q_ptr->name3].generated = FALSE;
                return;
            }

            o_ptr = &o_list[o_idx];
            object_copy(o_ptr, q_ptr);

            o_ptr->marked &= (OM_TOUCHED | OM_COUNTED | OM_EFFECT_COUNTED | OM_EGO_COUNTED | OM_ART_COUNTED);
            o_ptr->iy = o_ptr->ix = 0;
            o_ptr->held_m_idx = m_idx;
            o_ptr->next_o_idx = m_ptr->hold_o_idx;
            m_ptr->hold_o_idx = o_idx;
        }
        else if (cave_have_flag_bold(y, x, FF_PROJECT))
        {
            /* Drop (or break) near that location */
            (void)drop_near(q_ptr, j, y, x);
        }
        else
        {
            /* Drop (or break) near that location */
            (void)drop_near(q_ptr, j, prev_y, prev_x);
        }

    /* Sniper - Repeat shooting when double shots */
    }

    /* Sniper - Loose his/her concentration after any shot */
    if (p_ptr->concent) reset_concentration(FALSE);
}


bool do_cmd_fire(void)
{
    bool result = FALSE;
    int item;
    int slot = equip_find_first(object_is_bow);
    object_type *bow = NULL;
    cptr q, s;

    if (!slot)
    {
        msg_print("You have nothing to fire with.");
        flush();
        return FALSE;
    }

    if (prace_is_(MIMIC_MIST))
    {
        msg_print("You cannot shoot while incorporeal.");
        flush();
        return FALSE;
    }

    bow = equip_obj(slot);

    if (bow->sval == SV_CRIMSON || bow->sval == SV_RAILGUN)
    {
        msg_print("You should activate your Gun instead.");
        flush();
        return FALSE;
    }

    if (bow->sval == SV_HARP)
    {
        msg_print("You play a soothing melody, but not much else happens.");
        flush();
        return FALSE;
    }

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    item_tester_tval = p_ptr->shooter_info.tval_ammo;
    q = "Fire which item? ";
    s = "You have nothing to fire.";
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_QUIVER)))
    {
        flush();
        return FALSE;
    }

    result = do_cmd_fire_aux1(item, bow);

    if (p_ptr->pclass == CLASS_SNIPER)
    {
        if (snipe_type == SP_AWAY)
            teleport_player(10 + (p_ptr->concent * 2), 0L);
        if (snipe_type == SP_FINAL)
        {
            msg_print("You experience a powerful recoil!");
            (void)set_slow(p_ptr->slow + randint0(7) + 7, FALSE);
            (void)set_stun(p_ptr->stun + randint1(25), FALSE);
        }
    }

    return result;
}


static bool item_tester_hook_boomerang(object_type *o_ptr)
{
    if ((o_ptr->tval==TV_DIGGING) || (o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM) || (o_ptr->tval == TV_HAFTED)) return (TRUE);

    /* Assume not */
    return (FALSE);
}


/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
bool do_cmd_throw_aux(int mult, bool boomerang, int shuriken)
{
    int dir, item;
    int i, j, y, x, ty, tx, prev_y, prev_x;
    int ny[19], nx[19];
    int chance, tdam, tdis;
    int mul, div;
    int cur_dis, visible;

    object_type forge;
    object_type *q_ptr;

    object_type *o_ptr;

    bool hit_body = FALSE;
    bool hit_wall = FALSE;
    bool equiped_item = FALSE;
    bool return_when_thrown = FALSE;

    char o_name[MAX_NLEN];

    int msec = delay_factor * delay_factor * delay_factor;

    u32b flgs[OF_ARRAY_SIZE];
    cptr q, s;
    bool come_back = FALSE;
    bool do_drop = TRUE;


    if (p_ptr->special_defense & KATA_MUSOU)
    {
        set_action(ACTION_NONE);
    }

    if (shuriken)
    {
        item = shuriken;
    }
    else if (boomerang)
    {
        if (p_ptr->weapon_ct > 1)
        {
            item_tester_hook = item_tester_hook_boomerang;
            q = "Throw which item? ";
            s = "You have nothing to throw.";
            if (!get_item(&item, q, s, USE_EQUIP))
            {
                flush();
                return FALSE;
            }
        }
        else
            item = equip_find_first(object_is_melee_weapon);
    }
    else
    {
        /* Get an item */
        q = "Throw which item? ";
        s = "You have nothing to throw.";

        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_EQUIP)))
        {
            flush();
            return FALSE;
        }
    }

    /* Access the item (if in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];

        /* Ugly hack! */
        if ( object_is_melee_weapon(o_ptr) 
          && equip_is_valid_slot(item)
          && p_ptr->pclass == CLASS_PSION
          && psion_weapon_graft() )
        {
            msg_print("Failed!  Your weapon is currently grafted to your arm!");
            return FALSE;
        }
    }
    else
    {
        o_ptr = &o_list[0 - item];
    }

    if (!fear_allow_shoot())
    {
        msg_print("You are too scared!");
        energy_use = 100;
        if ((p_ptr->pclass == CLASS_ROGUE) || (p_ptr->pclass == CLASS_NINJA))
            energy_use -= p_ptr->lev;
        return FALSE;
    }

    if (have_flag(o_ptr->flags, OF_NO_REMOVE))
    {
        msg_print("And how exactly do you propose to throw yourself?");
        return FALSE;
    }

    /* Item is cursed */
    if (object_is_cursed(o_ptr) && equip_is_valid_slot(item))
    {
        msg_print("Hmmm, it seems to be cursed.");
        return FALSE;
    }

    if (o_ptr->tval == TV_POTION && o_ptr->sval == SV_POTION_BLOOD)
    {
        msg_print("You can't do that!  Your blood will go sour!");
        return FALSE;
    }

    if (p_ptr->inside_arena && !boomerang)
    {
        if (o_ptr->tval != TV_SPIKE)
        {
            msg_print("You're in the arena now. This is hand-to-hand!");
            msg_print(NULL);

            /* Nope */
            return FALSE;
        }
    }

    /* Get local object */
    q_ptr = &forge;

    /* Obtain a local object */
    object_copy(q_ptr, o_ptr);

    /* Extract the thrown object's flags. */
    obj_flags(q_ptr, flgs);

    /* Distribute the charges of rods/wands between the stacks */
    distribute_charges(o_ptr, q_ptr, 1);

    /* Single object */
    q_ptr->number = 1;

    /* Description */
    object_desc(o_name, q_ptr, OD_OMIT_PREFIX);

    if (p_ptr->mighty_throw) mult += 3;

    /* Extract a "distance multiplier" */
    /* Changed for 'launcher' mutation */
    mul = 10 + 2 * (mult - 1);

    /* Enforce a minimum "weight" of one pound */
    div = ((q_ptr->weight > 10) ? q_ptr->weight : 10);
    if ((have_flag(flgs, OF_THROWING)) || boomerang) div /= 2;

    /* Hack -- Distance -- Reward strength, penalize weight */
    tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

    /* Max distance of 10-18 */
    if (tdis > mul) tdis = mul;

    if (shuriken)
    {
        ty = randint0(101)-50+py;
        tx = randint0(101)-50+px;
    }
    else
    {
        project_length = tdis + 1;

        /* Get a direction (or cancel) */
        if (!get_aim_dir(&dir)) return FALSE;

        /* Predict the "target" location */
        tx = px + 99 * ddx[dir];
        ty = py + 99 * ddy[dir];

        /* Check for "target request" */
        if ((dir == 5) && target_okay())
        {
            tx = target_col;
            ty = target_row;
        }

        project_length = 0;  /* reset to default */
    }

    if ((q_ptr->name1 == ART_MJOLLNIR) ||
        (q_ptr->name1 == ART_AEGISFANG) || boomerang)
        return_when_thrown = TRUE;

    /* Reduce and describe inventory */
    if (item >= 0)
    {
        inven_item_increase(item, -1);
        if (!return_when_thrown)
            inven_item_describe(item);
        inven_item_optimize(item);
    }

    /* Reduce and describe floor item */
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_optimize(0 - item);
    }
    if (equip_is_valid_slot(item))
    {
        equiped_item = TRUE;
        p_ptr->redraw |= (PR_EQUIPPY);
    }

    /* Take a turn */
    energy_use = 100;

    /* Rogue and Ninja gets bonus */
    if ((p_ptr->pclass == CLASS_ROGUE) || (p_ptr->pclass == CLASS_NINJA))
        energy_use -= p_ptr->lev;

    /* Start at the player */
    y = py;
    x = px;

    /* Hack -- Handle stuff */
    notice_stuff(); /* Hack: combine_pack before calc_bonuses */
    handle_stuff();

    if ((p_ptr->pclass == CLASS_NINJA) && ((q_ptr->tval == TV_SPIKE) || ((have_flag(flgs, OF_THROWING)) && (q_ptr->tval == TV_SWORD)))) shuriken = TRUE;
    else shuriken = FALSE;

    /* Chance of hitting */
    if (have_flag(flgs, OF_THROWING)) chance = ((p_ptr->skill_tht) +
        ((p_ptr->shooter_info.to_h + q_ptr->to_h) * BTH_PLUS_ADJ));
    else chance = (p_ptr->skill_tht + (p_ptr->shooter_info.to_h * BTH_PLUS_ADJ));

    if (shuriken) chance *= 2;

    /* Save the old location */
    prev_y = y;
    prev_x = x;

    /* Travel until stopped */
    for (cur_dis = 0; cur_dis <= tdis; )
    {
        /* Hack -- Stop at the target */
        if ((y == ty) && (x == tx)) break;

        /* Calculate the new location (see "project()") */
        ny[cur_dis] = y;
        nx[cur_dis] = x;
        mmove2(&ny[cur_dis], &nx[cur_dis], py, px, ty, tx);

        /* Stopped by walls/doors */
        if (!cave_have_flag_bold(ny[cur_dis], nx[cur_dis], FF_PROJECT))
        {
            hit_wall = TRUE;
            if ((q_ptr->tval == TV_FIGURINE) || object_is_potion(q_ptr) || !cave[ny[cur_dis]][nx[cur_dis]].m_idx) break;
        }

        /* The player can see the (on screen) missile */
        if (panel_contains(ny[cur_dis], nx[cur_dis]) && player_can_see_bold(ny[cur_dis], nx[cur_dis]))
        {
            char c = object_char(q_ptr);
            byte a = object_attr(q_ptr);

            /* Draw, Hilite, Fresh, Pause, Erase */
            print_rel(c, a, ny[cur_dis], nx[cur_dis]);
            move_cursor_relative(ny[cur_dis], nx[cur_dis]);
            Term_fresh();
            Term_xtra(TERM_XTRA_DELAY, msec);
            lite_spot(ny[cur_dis], nx[cur_dis]);
            Term_fresh();
        }

        /* The player cannot see the missile */
        else
        {
            /* Pause anyway, for consistancy */
            Term_xtra(TERM_XTRA_DELAY, msec);
        }

        /* Save the old location */
        prev_y = y;
        prev_x = x;

        /* Save the new location */
        x = nx[cur_dis];
        y = ny[cur_dis];

        /* Advance the distance */
        cur_dis++;

        /* Monster here, Try to hit it */
        if (cave[y][x].m_idx)
        {
            cave_type *c_ptr = &cave[y][x];

            monster_type *m_ptr = &m_list[c_ptr->m_idx];
            monster_race *r_ptr = &r_info[m_ptr->r_idx];

            /* Check the visibility */
            visible = m_ptr->ml;

            /* Note the collision */
            hit_body = TRUE;

            /* Did we hit it (penalize range) */
            if (test_hit_fire(chance - cur_dis, MON_AC(r_ptr, m_ptr), m_ptr->ml))
            {
                bool fear = FALSE;

                /* Handle unseen monster */
                if (!visible)
                {
                    /* Invisible monster */
                    msg_format("The %s finds a mark.", o_name);

                }

                /* Handle visible monster */
                else
                {
                    char m_name[80];

                    /* Get "the monster" or "it" */
                    monster_desc(m_name, m_ptr, 0);

                    /* Message */
                    msg_format("The %s hits %s.", o_name, m_name);

                    if (m_ptr->ml)
                    {
                        /* Hack -- Track this monster race */
                        if (!p_ptr->image) monster_race_track(m_ptr->ap_r_idx);

                        /* Hack -- Track this monster */
                        health_track(c_ptr->m_idx);
                    }
                }

                /* Hack -- Base damage from thrown object */
                tdam = damroll(q_ptr->dd, q_ptr->ds);
                /* Apply special damage XXX XXX XXX */
                tdam = tot_dam_aux(q_ptr, tdam, m_ptr, 0, 0, TRUE);
                tdam = critical_throw(q_ptr->weight, q_ptr->to_h, tdam);
                if (q_ptr->to_d > 0)
                    tdam += q_ptr->to_d;
                else
                    tdam += -q_ptr->to_d;

                if (boomerang)
                {
                    tdam *= mult + NUM_BLOWS(0)/100; /* TODO */
                    tdam += p_ptr->to_d_m;
                }
                else if (have_flag(flgs, OF_THROWING))
                {
                    tdam *= (3+mult);
                    tdam += p_ptr->to_d_m;
                }
                else
                {
                    tdam *= mult;
                }
                if (shuriken)
                {
                    tdam += ((p_ptr->lev+30)*(p_ptr->lev+30)-900)/55;
                }

                /* No negative damage */
                if (tdam < 0) tdam = 0;

                /* Modify the damage */
                tdam = mon_damage_mod(m_ptr, tdam, FALSE);

                /* Hit the monster, check for death */
                if (mon_take_hit(c_ptr->m_idx, tdam, &fear, NULL))
                {
                    /* Dead monster */
                }

                /* No death */
                else
                {
                    /* Message */
                    message_pain(c_ptr->m_idx, tdam);

                    /* Anger the monster */
                    if ((tdam > 0) && !object_is_potion(q_ptr))
                        anger_monster(m_ptr);

                    /* Take note */
                    if (fear && m_ptr->ml)
                    {
                        char m_name[80];

                        /* Sound */
                        sound(SOUND_FLEE);

                        /* Get the monster name (or "it") */
                        monster_desc(m_name, m_ptr, 0);

                        /* Message */
                        msg_format("%^s flees in terror!", m_name);

                    }
                }
            }

            /* Stop looking */
            break;
        }
    }

    /* Chance of breakage (during attacks) */
    j = (hit_body ? breakage_chance(q_ptr) : 0);

    /* Figurines transform */
    if ((q_ptr->tval == TV_FIGURINE) && !(p_ptr->inside_arena))
    {
        j = 100;

        if (!(summon_named_creature(0, y, x, q_ptr->pval,
                        !(object_is_cursed(q_ptr)) ? PM_FORCE_PET : 0L)))
            msg_print("The Figurine writhes and then shatters.");

        else if (object_is_cursed(q_ptr))
            msg_print("You have a bad feeling about this.");

    }


    /* Potions smash open */
    if (object_is_potion(q_ptr))
    {
        if (hit_body || hit_wall || (randint1(100) < j))
        {
            /* Message */
            msg_format("The %s shatters!", o_name);


            if (potion_smash_effect(0, y, x, q_ptr->k_idx))
            {
                monster_type *m_ptr = &m_list[cave[y][x].m_idx];

                /* ToDo (Robert): fix the invulnerability */
                if (cave[y][x].m_idx &&
                    is_friendly(&m_list[cave[y][x].m_idx]) &&
                    !MON_INVULNER(m_ptr))
                {
                    char m_name[80];
                    monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
                    msg_format("%^s gets angry!", m_name);

                    set_hostile(&m_list[cave[y][x].m_idx]);
                }
            }
            do_drop = FALSE;
        }
        else
        {
            j = 0;
        }
    }

    if (return_when_thrown)
    {
        int back_chance = randint1(30)+20+((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
        char o2_name[MAX_NLEN];
        bool super_boomerang = (((q_ptr->name1 == ART_MJOLLNIR) || (q_ptr->name1 == ART_AEGISFANG)) && boomerang);

        j = -1;
        if (boomerang) back_chance += 4+randint1(5);
        if (super_boomerang) back_chance += 100;
        object_desc(o2_name, q_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        if((back_chance > 30) && (!one_in_(100) || super_boomerang))
        {
            for (i = cur_dis - 1; i > 0; i--)
            {
                if (panel_contains(ny[i], nx[i]) && player_can_see_bold(ny[i], nx[i]))
                {
                    char c = object_char(q_ptr);
                    byte a = object_attr(q_ptr);

                    /* Draw, Hilite, Fresh, Pause, Erase */
                    print_rel(c, a, ny[i], nx[i]);
                    move_cursor_relative(ny[i], nx[i]);
                    Term_fresh();
                    Term_xtra(TERM_XTRA_DELAY, msec);
                    lite_spot(ny[i], nx[i]);
                    Term_fresh();
                }
                else
                {
                    /* Pause anyway, for consistancy */
                    Term_xtra(TERM_XTRA_DELAY, msec);
                }
            }
            if((back_chance > 37) && !p_ptr->blind && (item >= 0))
            {
                msg_format("%s comes back to you.", o2_name);
                come_back = TRUE;
            }
            else
            {
                if (item >= 0)
                {
                    msg_format("%s comes back to you, but you can't catch!", o2_name);
                }
                else
                {
                    msg_format("%s comes back.", o2_name);
                }
                y = py;
                x = px;
            }
        }
        else
        {
            msg_format("%s doesn't back!", o2_name);
        }
    }

    if (come_back)
    {
        if (equip_is_valid_slot(item))
        {
            /* Access the wield slot */
            o_ptr = &inventory[item];

            /* Wear the new stuff */
            object_copy(o_ptr, q_ptr);

            /* Increase the weight */
            p_ptr->total_weight += q_ptr->weight;

            p_ptr->update |= (PU_BONUS);
            p_ptr->update |= (PU_TORCH);
            p_ptr->update |= (PU_MANA);
            p_ptr->redraw |= (PR_EQUIPPY);
        }
        else
        {
            inven_carry(q_ptr);
        }
        do_drop = FALSE;
    }
    else if (equiped_item)
    {
        android_calc_exp();
    }

    /* Drop (or break) near that location */
    if (do_drop)
    {
        if (cave_have_flag_bold(y, x, FF_PROJECT))
        {
            /* Drop (or break) near that location */
            (void)drop_near(q_ptr, j, y, x);
        }
        else
        {
            /* Drop (or break) near that location */
            (void)drop_near(q_ptr, j, prev_y, prev_x);
        }
    }

    return TRUE;
}


/*
 * Throw an object from the pack or floor.
 */
void do_cmd_throw(void)
{
    do_cmd_throw_aux(1, FALSE, 0);
}


/*
 * Hack: travel command
 */
#define TRAVEL_UNABLE 9999

static int flow_head = 0;
static int flow_tail = 0;
static s16b temp2_x[MAX_SHORT];
static s16b temp2_y[MAX_SHORT];

/* Hack: forget the "flow" information */
void forget_travel_flow(void)
{
    int x, y;

    /* Check the entire dungeon */
    for (y = 0; y < cur_hgt; y++)
    {
        for (x = 0; x < cur_wid; x++)
        {
            /* Forget the old data */
            travel.cost[y][x] = TRAVEL_UNABLE;
        }
    }
}

static bool travel_flow_aux(int y, int x, int n, bool wall)
{
    cave_type *c_ptr = &cave[y][x];
    feature_type *f_ptr = &f_info[c_ptr->feat];
    int old_head = flow_head;

    n = n % TRAVEL_UNABLE;

    /* Ignore out of bounds */
    if (!in_bounds(y, x)) return wall;

    /* Ignore "pre-stamped" entries */
    if (travel.cost[y][x] != TRAVEL_UNABLE) return wall;

    /* Ignore "walls" and "rubble" (include "secret doors") */
    if (have_flag(f_ptr->flags, FF_WALL) ||
        have_flag(f_ptr->flags, FF_CAN_DIG) ||
        (have_flag(f_ptr->flags, FF_DOOR) && cave[y][x].mimic) ||
        (!have_flag(f_ptr->flags, FF_MOVE) && have_flag(f_ptr->flags, FF_CAN_FLY) && !p_ptr->levitation))
    {
        if (!wall) return wall;
    }
    else
    {
        wall = FALSE;
    }

    /* Save the flow cost */
    travel.cost[y][x] = n;
    if (wall) travel.cost[y][x] += TRAVEL_UNABLE;

    /* Enqueue that entry */
    temp2_y[flow_head] = y;
    temp2_x[flow_head] = x;

    /* Advance the queue */
    if (++flow_head == MAX_SHORT) flow_head = 0;

    /* Hack -- notice overflow by forgetting new entry */
    if (flow_head == flow_tail) flow_head = old_head;

    return wall;
}


static void travel_flow(int ty, int tx)
{
    int x, y, d;
    bool wall = FALSE;
    feature_type *f_ptr = &f_info[cave[ty][tx].feat];

    /* Reset the "queue" */
    flow_head = flow_tail = 0;

    if (!have_flag(f_ptr->flags, FF_MOVE)) wall = TRUE;

    /* Add the player's grid to the queue */
    wall = travel_flow_aux(ty, tx, 0, wall);

    /* Now process the queue */
    while (flow_head != flow_tail)
    {
        /* Extract the next entry */
        y = temp2_y[flow_tail];
        x = temp2_x[flow_tail];

        /* Forget that entry */
        if (++flow_tail == MAX_SHORT) flow_tail = 0;

        /* Add the "children" */
        for (d = 0; d < 8; d++)
        {
            /* Add that child if "legal" */
            wall = travel_flow_aux(y + ddy_ddd[d], x + ddx_ddd[d], travel.cost[y][x] + 1, wall);
        }
    }

    /* Forget the flow info */
    flow_head = flow_tail = 0;
}

void do_cmd_travel_xy(int x, int y)
{
    int i;
    int dx, dy, sx, sy;
    feature_type *f_ptr;

    travel.run = 0;

    /* Bug with wilderness scrolling yet to be located ... */
    if (!in_bounds2(y, x))
    {
        forget_travel_flow();
        travel.y = py;
        travel.x = px;
        return;
    }

    if ((x == px) && (y == py))
    {
        msg_print("You are already there!!");
        return;
    }

    f_ptr = &f_info[cave[y][x].feat];

    if ((cave[y][x].info & CAVE_MARK) &&
        (have_flag(f_ptr->flags, FF_WALL) ||
            have_flag(f_ptr->flags, FF_CAN_DIG) ||
            (have_flag(f_ptr->flags, FF_DOOR) && cave[y][x].mimic)))
    {
        msg_print("You cannot travel there!");
        return;
    }

    travel.x = x;
    travel.y = y;

    forget_travel_flow();
    travel_flow(y, x);

    /* Travel till 255 steps */
    travel.run = 255;

    /* Paranoia */
    travel.dir = 0;

    /* Decides first direction */
    dx = abs(px - x);
    dy = abs(py - y);
    sx = ((x == px) || (dx < dy)) ? 0 : ((x > px) ? 1 : -1);
    sy = ((y == py) || (dy < dx)) ? 0 : ((y > py) ? 1 : -1);

    for (i = 1; i <= 9; i++)
    {
        if ((sx == ddx[i]) && (sy == ddy[i])) travel.dir = i;
    }
}

void do_cmd_travel(void)
{
    int x, y;
    if (!tgt_pt(&x, &y, -1)) return;
    do_cmd_travel_xy(x, y);
}
