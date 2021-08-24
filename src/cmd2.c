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

static bool do_cmd_bash_aux(int y, int x, int dir);

/*
 * Go up one level
 */
void do_cmd_go_up(void)
{
    cave_type *c_ptr = cave_at(p_ptr->pos);
    feature_type *f_ptr = &f_info[c_ptr->feat];

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    /* Verify stairs */
    if (!have_flag(f_ptr->flags, FF_LESS))
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
    energy_use = 100;

    if (!cave_have_flag_at(p_ptr->pos, FF_QUEST_ENTER))
    {
        msg_print("You see no quest level here.");
        return;
    }
    else
    {
        int quest_id = cave_at(p_ptr->pos)->special;
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
    cave_type *c_ptr = cave_at(p_ptr->pos);
    feature_type *f_ptr = &f_info[c_ptr->feat];

    bool fall_trap = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    /* Verify stairs */
    if (!have_flag(f_ptr->flags, FF_MORE))
    {
        msg_print("I see no down staircase here.");
        return;
    }

    if (have_flag(f_ptr->flags, FF_TRAP)) fall_trap = TRUE;

    /* Quest entrance */
    if (have_flag(f_ptr->flags, FF_QUEST_ENTER))
        do_cmd_quest();

    else
    {
        int target_dungeon = 0;
        if (cave->dun_type_id == D_SURFACE)
        {
            dun_type_ptr dun_type;
            assert(have_flag(f_ptr->flags, FF_ENTRANCE));
            target_dungeon = c_ptr->special;
            dun_type = dun_types_lookup(target_dungeon);
            if (!dun_type->plr_max_lvl)
            {
                if (dun_type->flags & DF_RANDOM)
                    msg_format("This is the entrance of %s (Danger level: ?)", dun_type->name);
                else
                    msg_format("This is the entrance of %s (Danger level: %d)", dun_type->name, dun_type->min_dun_lvl);
                if (!get_check("Do you really go into this dungeon? ")) return;
            }
            p_ptr->old_pos = p_ptr->pos; /* remember for return recall */
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
                dun_type_ptr type = dun_type();
                if (type->enter_f) type->enter_f(type);
                else msg_format("You entered %s.", dun_type()->desc);
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
static obj_ptr chest_check(int y, int x)
{
    obj_ptr obj;
    for (obj = obj_at_xy(x, y); obj; obj = obj->next)
        if (obj->tval == TV_CHEST) return obj;
    return NULL;
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
    rect_t r = rect_interior(cave->rect);
    int i;
    for (i = 0; i < 200; i++)
    {
        point_t p = rect_random_point(r);
        if (!cave_empty_at(p)) continue;
        drop_near(o_ptr, p, -1);
        return TRUE;
    }
    return FALSE;
}
static void chest_death(bool scatter, int y, int x, obj_ptr chest_ptr)
{
    point_t pos = point_create(x, y);
    int     ct_objects = 0, ct_gold = 0;
    int     i;
    int     lvl = cave->difficulty;
    u32b    mode = AM_GOOD;

    if (chest_ptr->sval == SV_CHEST_KANDUME) /* Can of Toys */
    {
        ct_objects = 5;
        ct_gold = 0;
        mode |= AM_GREAT;
        lvl = chest_ptr->xtra3;
    }
    else
    {        /* v~~~~~~~~~ You'll need to look a k_info.txt to understand this ... */
        int num = chest_ptr->sval % SV_CHEST_MIN_LARGE;

        /* lvl = ABS(chest_ptr->pval) + 10;
         * i.e., 1dL+10. Finding an OL99 chest and getting L22 objects is just
         * plain insulting. Chests should be like ?Acquirement, only AM_GOOD rather
         * than AM_GREAT. */
        lvl = chest_ptr->xtra3;
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
        if (!make_object(&forge, lvl, mode)) continue;
        if (scatter) _chest_scatter(&forge);
        else drop_near(&forge, pos, -1);
    }

    for (i = 0; i < ct_gold; i++)
    {
        object_type forge = {0};
        if (!make_gold(&forge, lvl)) continue;
        if (scatter) _chest_scatter(&forge);
        else drop_near(&forge, pos, -1);
    }

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
static void chest_trap(int y, int x, obj_ptr obj)
{
    point_t pos = point_create(x, y);
    int  i, trap;
    int mon_level = obj->xtra3;

    /* Ignore disarmed chests */
    if (obj->pval <= 0) return;

    /* Obtain the traps */
    trap = chest_traps[obj->pval];

    /* Lose strength */
    if (trap & (CHEST_LOSE_STR))
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle");

        (void)do_dec_stat(A_STR);
    }

    /* Lose constitution */
    if (trap & (CHEST_LOSE_CON))
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, damroll(1, 4), "a poison needle");

        (void)do_dec_stat(A_CON);
    }

    /* Poison */
    if (trap & (CHEST_POISON))
    {
        msg_print("A puff of green gas surrounds you!");
        if (!res_save_default(RES_POIS))
            plr_tim_add(T_POISON, 10 + randint1(20));
    }

    /* Paralyze */
    if (trap & (CHEST_PARALYZE))
    {
        msg_print("A puff of yellow gas surrounds you!");
        if (!free_act_save_p(0))
            plr_tim_add(T_PARALYZED, randint1(4));
    }

    /* Summon monsters */
    if (trap & (CHEST_SUMMON))
    {
        int num = 2 + randint1(3);
        msg_print("You are enveloped in a cloud of smoke!");


        for (i = 0; i < num; i++)
        {
            if (randint1(100)<cave->dun_lvl)
                activate_hi_summon(p_ptr->pos.y, p_ptr->pos.x, FALSE);
            else
                summon_specific(0, pos, mon_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Elemental summon. */
    if (trap & (CHEST_E_SUMMON))
    {
        msg_print("Elemental beings appear to protect their treasures!");
        for (i = 0; i < randint1(3) + 5; i++)
            summon_specific(0, pos, mon_level, SUMMON_ELEMENTAL, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
    }

    /* Force clouds, then summon birds. */
    if (trap & (CHEST_BIRD_STORM))
    {
        msg_print("A storm of birds swirls around you!");

        for (i = 0; i < randint1(3) + 3; i++)
            fire_meteor(-1, GF_FORCE, y, x, obj->pval / 5, 7);

        for (i = 0; i < randint1(5) + obj->pval / 5; i++)
            summon_specific(0, pos, mon_level, SUMMON_BIRD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
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
                fire_meteor(-1, GF_FIRE, y, x, 10, 5);
                summon_specific(0, pos, mon_level, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            }
        }

        /* Summon dragons. */
        else if (one_in_(3))
        {
            msg_print("Draconic forms loom out of the darkness!");
            for (i = 0; i < randint1(3) + 2; i++)
                summon_specific(0, pos, mon_level, SUMMON_DRAGON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }

        /* Summon hybrids. */
        else if (one_in_(2))
        {
            msg_print("Creatures strange and twisted assault you!");
            for (i = 0; i < randint1(5) + 3; i++)
                summon_specific(0, pos, mon_level, SUMMON_HYBRID, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }

        /* Summon vortices (scattered) */
        else
        {
            msg_print("Vortices coalesce and wreak destruction!");
            for (i = 0; i < randint1(3) + 2; i++)
                summon_specific(0, pos, mon_level, SUMMON_VORTEX, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        }
    }

    /* Dispel player. */
    if ((trap & (CHEST_RUNES_OF_EVIL)) && obj->k_idx)
    {
        /* Determine how many nasty tricks can be played. */
        int nasty_tricks_count = 4 + randint0(3);

        /* Message. */
        msg_print("Hideous voices bid:  'Let the darkness have thee!'");

        /* This is gonna hurt... */
        for (; nasty_tricks_count > 0; nasty_tricks_count--)
        {
            /* ...but a high saving throw does help a little. */
            if (randint1(100+obj->pval*2) > p_ptr->skills.sav)
            {
                if (one_in_(6)) take_hit(DAMAGE_NOESCAPE, damroll(5, 20), "a chest dispel-player trap");
                else if (one_in_(5))
                {
                    if (!p_ptr->no_cut)
                        plr_tim_add(T_CUT, 200);
                }
                else if (one_in_(4))
                {
                    if (!free_act_save_p(0))
                        plr_tim_add(T_PARALYZED, randint1(4));
                    else if (!p_ptr->no_stun)
                        plr_tim_add(T_STUN, 10 + randint0(100));
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
    if ((trap & (CHEST_EXPLODE)) && obj->k_idx)
    {
        msg_print("There is a sudden explosion!");
        msg_print("Everything inside the chest is destroyed!");

        obj->pval = 0;
        sound(SOUND_EXPLODE);
        take_hit(DAMAGE_ATTACK, damroll(5, 8), "an exploding chest");

    }
    /* Scatter contents. */
    if ((trap & (CHEST_SCATTER)) && obj->k_idx)
    {
        msg_print("The contents of the chest scatter all over the dungeon!");
        chest_death(TRUE, y, x, obj);
        obj->pval = 0;
    }
}


/*
 * Attempt to open the given chest at the given location
 *
 * Assume there is no monster blocking the destination
 *
 * Returns TRUE if repeated commands may continue
 */
static bool do_cmd_open_chest(int y, int x, obj_ptr obj)
{
    int i, j;

    bool flag = TRUE;

    bool more = FALSE;


    /* Take a turn */
    energy_use = 100;

    /* Attempt to unlock it */
    if (obj->pval > 0)
    {
        /* Assume locked, and thus not open */
        flag = FALSE;

        /* Get the "disarm" factor */
        i = p_ptr->skills.dis;

        /* Penalize some conditions */
        if (plr_tim_find(T_BLIND) || no_lite()) i = i / 10;
        if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE)) i = i / 10;

        /* Extract the difficulty */
        j = i - obj->pval;

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
        chest_trap(y, x, obj);

        /* Let the Chest drop items */
        chest_death(FALSE, y, x, obj);
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
        yy = p_ptr->pos.y + ddy_ddd[d];
        xx = p_ptr->pos.x + ddx_ddd[d];

        /* Get the cave */
        c_ptr = cave_at_xy(xx, yy);

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
    int d, count;

    object_type *o_ptr;

    /* Count how many matches */
    count = 0;

    /* Check around (and under) the character */
    for (d = 0; d < 9; d++)
    {
        /* Extract adjacent (legal) location */
        int yy = p_ptr->pos.y + ddy_ddd[d];
        int xx = p_ptr->pos.x + ddx_ddd[d];

        o_ptr = chest_check(yy, xx);

        /* No (visible) chest is there */
        if (!o_ptr) continue;

        /* Already open */
        if (o_ptr->pval == 0) continue;

        /* No (known) traps here
           CTK: pval is negative if chest is disarmed. Don't read out of bounds!! */
        if (trapped && (!obj_is_known(o_ptr) || o_ptr->pval < 0 ||
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

    dy = y - p_ptr->pos.y;
    dx = x - p_ptr->pos.x;

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
    cave_type *c_ptr = cave_at_xy(x, y);

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
        if (plr_tim_find(T_BLIND) || no_lite()) i = i / 10;
        if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE)) i = i / 10;

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
        mon_ptr mon;
        obj_ptr obj;

        /* Get requested location */
        y = p_ptr->pos.y + ddy[dir];
        x = p_ptr->pos.x + ddx[dir];

        /* Get requested grid */
        c_ptr = cave_at_xy(x, y);

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Check for chest */
        obj = chest_check(y, x);
        mon = mon_at_xy(x, y);

        /* Nothing useful */
        if (!have_flag(f_info[feat].flags, FF_OPEN) && !obj)
        {
            /* Message */
            msg_print("You see nothing there to open.");

        }

        /* Monster in the way */
        else if (mon && mon->id != p_ptr->riding)
        {
            /* Take a turn */
            energy_use = 100;

            /* Message */
            msg_print("There is a monster in the way!");

            /* Attack */
            plr_attack_normal(point_create(x, y));
        }

        /* Handle chests */
        else if (obj)
        {
            /* Open the chest */
            more = do_cmd_open_chest(y, x, obj);
        }

        /* Handle doors */
        else
        {
            /* Open the door */
            if (p_ptr->prace == RACE_MON_VORTEX)
                more = do_cmd_bash_aux(y, x, dir);
            else
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
    cave_type *c_ptr = cave_at_xy(x, y);
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
        if ((obj_at_xy(x, y) || (c_ptr->info & CAVE_OBJECT)) &&
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
        point_t pos = point_step(p_ptr->pos, dir);
        dun_grid_ex_t grid = dun_grid_ex_at(cave, pos);

        /* Require open/broken door */
        if (!have_flag(grid.feat_mimic->flags, FF_CLOSE))
            msg_print("You see nothing there to close.");

        /* Monster in the way */
        else if (grid.mon)
        {
            energy_use = 100;
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }

        /* Close the door */
        else
        {
            /* Close the door */
            more = do_cmd_close_aux(pos.y, pos.x);
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
    cave_type *c_ptr = cave_at_xy(x, y);

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
    c_ptr = cave_at_xy(x, y);
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
    int  dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

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
        point_t pos = point_step(p_ptr->pos, dir);
        dun_grid_ex_t grid = dun_grid_ex_at(cave, pos);

        /* No tunnelling through doors */
        if (have_flag(grid.feat_mimic->flags, FF_DOOR))
            msg_print("You cannot tunnel through doors.");

        /* No tunnelling through most features */
        else if (!have_flag(grid.feat_mimic->flags, FF_TUNNEL))
            msg_print("You can't tunnel through that.");

        /* A monster is in the way */
        else if (grid.mon)
        {
            energy_use = 100;
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }

        /* Try digging */
        else
            more = do_cmd_tunnel_aux(pos.y, pos.x);
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
bool easy_open_door(int y, int x, int dir)
{
    int i, j;

    cave_type *c_ptr = cave_at_xy(x, y);
    feature_type *f_ptr = &f_info[c_ptr->feat];

    /* Must be a closed door */
    if (!is_closed_door(c_ptr->feat))
    {
        /* Nope */
        return (FALSE);
    }

    if (p_ptr->prace == RACE_MON_VORTEX)
    {
        do_cmd_bash_aux(y, x, dir);
        return TRUE;
    }

    /* Jammed door */
    if (!have_flag(f_ptr->flags, FF_OPEN))
    {
        msg_format("The %s appears to be stuck.", f_name + f_info[get_feat_mimic(c_ptr)].name);
    }

    /* Locked door */
    else if (f_ptr->power)
    {
        /* Disarm factor */
        i = p_ptr->skills.dis;

        /* Penalize some conditions */
        if (plr_tim_find(T_BLIND) || no_lite()) i = i / 10;
        if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE)) i = i / 10;

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
static bool do_cmd_disarm_chest(int y, int x, obj_ptr obj)
{
    int i, j;

    bool more = FALSE;


    /* Take a turn */
    energy_use = 100;

    /* Get the "disarm" factor */
    i = p_ptr->skills.dis;

    /* Penalize some conditions */
    if (plr_tim_find(T_BLIND) || no_lite()) i = i / 10;
    if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE)) i = i / 10;

    /* Extract the difficulty */
    j = i - obj->pval;

    /* Always have a small chance of success */
    if (j < 2) j = 2;

    /* Must find the trap first. */
    if (!obj_is_known(obj))
    {
        msg_print("I don't see any traps.");

    }

    /* Already disarmed/unlocked */
    else if (obj->pval <= 0)
    {
        msg_print("The chest is not trapped.");

    }

    /* No traps to find. */
    else if (!chest_traps[obj->pval])
    {
        msg_print("The chest is not trapped.");

    }

    /* Success (get a lot of experience) */
    else if (randint0(100) < j)
    {
        msg_print("You have disarmed the chest.");

        gain_exp(obj->pval);
        obj->pval = (0 - obj->pval);
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
        chest_trap(y, x, obj);
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
    cave_type *c_ptr = cave_at_xy(x, y);

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
    if (plr_tim_find(T_BLIND) || no_lite()) i = i / 10;
    if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE)) i = i / 10;

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
        obj_ptr obj;
        mon_ptr mon;

        /* Get location */
        y = p_ptr->pos.y + ddy[dir];
        x = p_ptr->pos.x + ddx[dir];

        /* Get grid and contents */
        c_ptr = cave_at_xy(x, y);

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);

        /* Check for chests */
        obj = chest_check(y, x);
        mon = mon_at_xy(x, y);

        /* Disarm a trap */
        if (!is_trap(feat) && !obj)
        {
            msg_print("You see nothing there to disarm.");
        }

        /* Monster in the way */
        else if (mon && mon->id != p_ptr->riding)
        {
            msg_print("There is a monster in the way!");
            plr_attack_normal(point_create(x, y));
        }

        /* Disarm chest */
        else if (obj)
        {
            more = do_cmd_disarm_chest(y, x, obj);
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
    cave_type    *c_ptr = cave_at_xy(x, y);

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

    if (p_ptr->prace == RACE_MON_VORTEX) temp = 100;

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
        plr_tim_add(T_PARALYZED, randint1(4));
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
    int  dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

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
        point_t pos = point_step(p_ptr->pos, dir);
        dun_grid_ex_t grid = dun_grid_ex_at(cave, pos);

        /* Nothing useful */
        if (!have_flag(grid.feat_mimic->flags, FF_BASH))
            msg_print("You see nothing there to bash.");

        /* Monster in the way */
        else if (grid.mon)
        {
            energy_use = 100;
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }

        /* Bash a closed door */
        else
            more = do_cmd_bash_aux(pos.y, pos.x, dir);
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
    int  dir;
    bool more = FALSE;

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

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
        point_t pos = point_step(p_ptr->pos, dir);
        dun_grid_ex_t grid = dun_grid_ex_at(cave, pos);

        /* Take a turn */
        energy_use = 100;

        /* Attack monsters */
        if (grid.mon)
            plr_attack_normal(pos);

        /* Locked doors */
        else if (have_flag(grid.feat_mimic->flags, FF_OPEN))
            more = do_cmd_open_aux(pos.y, pos.x);

        /* Bash jammed doors */
        else if (have_flag(grid.feat_mimic->flags, FF_BASH))
            more = do_cmd_bash_aux(pos.y, pos.x, dir);

        /* Tunnel through walls */
        else if (have_flag(grid.feat_mimic->flags, FF_TUNNEL))
            more = do_cmd_tunnel_aux(pos.y, pos.x);

        /* Close open doors */
        else if (have_flag(grid.feat_mimic->flags, FF_CLOSE))
            more = do_cmd_close_aux(pos.y, pos.x);

        /* Disarm traps */
        else if (have_flag(grid.feat_mimic->flags, FF_DISARM))
            more = do_cmd_disarm_aux(pos.y, pos.x, dir);

        /* Oops */
        else
            msg_print("You attack the empty air.");
    }

    /* Cancel repetition unless we can continue */
    if (!more) disturb(0, 0);
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
        set_action(ACTION_NONE);

    /* Get a "repeated" direction */
    if (get_rep_dir(&dir,FALSE))
    {
        point_t pos = point_step(p_ptr->pos, dir);
        dun_grid_ex_t grid = dun_grid_ex_at(cave, pos);

        /* Require closed door */
        if (!have_flag(grid.feat_mimic->flags, FF_SPIKE))
            msg_print("You see nothing there to spike.");

        /* Is a monster in the way? */
        else if (grid.mon)
        {
            energy_use = 100;
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
        }

        /* Go for it */
        else
        {
            slot_t slot = pack_find_obj(TV_SPIKE, SV_ANY);
            obj_ptr spike;
            if (!slot)
            {
                msg_print("You have no spikes.");
                return;
            }

            /* Take a turn */
            energy_use = 100;

            /* Successful jamming */
            msg_format("You jam the %s with a spike.", f_name + grid.feat_mimic->name);

            cave_alter_feat(pos.y, pos.x, FF_SPIKE);

            spike = pack_obj(slot);
            spike->number--;
            obj_release(spike, 0);
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

    if ((dir != 5) && (p_ptr->special_defense & KATA_MUSOU))
    {
        set_action(ACTION_NONE);
    }

    if (p_ptr->action == ACTION_QUICK_WALK) energy_use = energy_use * (45-(p_ptr->lev/2)) / 100;
    if (p_ptr->action == ACTION_STALK) energy_use = energy_use * (150 - p_ptr->lev) / 100;
    if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
        energy_use = energy_use * (45-(p_ptr->lev/2)) / 100;

    if (p_ptr->quick_walk)
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

    if (p_ptr->special_defense & KATA_MUSOU)
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
    move_player_effect(p_ptr->pos, mpe_mode);
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
                if (point_equals(p_ptr->pos, pos))
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
                if (point_equals(p_ptr->pos, pos))
                {
                    /* Full pack aborts the travel sequence */
                    if (autopick_list[j].action & DO_AUTOPICK)
                        return FALSE;
                    continue; /* paranoia ... we should have destroyed this object */
                }
            }

            dist = point_distance(p_ptr->pos, pos);
            if (!projectable(p_ptr->pos.y, p_ptr->pos.x, pos.y, pos.x))
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
    if (!obj_at(p_ptr->pos))
        msg_print("You see no objects here. Try <color:keypress>^G</color> to auto-get nearby objects.");
    if (pack_get_floor())
        energy_use = 100;
}
void do_cmd_autoget(void)
{
    /* Get any objects under foot first ... this is the old
     * 'g' behavior sans interaction with features (e.g. re-
     * enter a shop) */
    if (obj_at(p_ptr->pos))
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

    if ((p_ptr->pclass == CLASS_BARD) && (p_ptr->magic_num1[0] || p_ptr->magic_num1[1]))
    {
        bard_stop_singing();
    }

    /* Hex */
    if (hex_spelling_any()) stop_hex_spell_all();

    warlock_stop_singing();

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
                p_ptr->afraid ||
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

    if (p_ptr->special_defense & NINJA_S_STEALTH) set_superstealth(FALSE);

    /* Take a turn XXX XXX XXX (?) */
    energy_use = 100;

    /* The sin of sloth */
    if (command_arg > 100)
        virtue_add(VIRTUE_DILIGENCE, -1);

    /* Why are you sleeping when there's no need?  WAKE UP!*/
    if ((p_ptr->chp == p_ptr->mhp) &&
        (p_ptr->csp == p_ptr->msp) &&
        !plr_tim_find(T_BLIND) && !plr_tim_find(T_CONFUSED) &&
        !plr_tim_find(T_POISON) && !p_ptr->afraid &&
        !plr_tim_find(T_STUN) && !plr_tim_find(T_CUT) &&
        !plr_tim_find(T_SLOW) && !plr_tim_find(T_PARALYZED) &&
        !plr_tim_find(T_HALLUCINATE) &&
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
int breakage_chance(object_type *o_ptr)
{
    if (obj_is_art(o_ptr)) return 0;
    if (shoot_hack == SP_KILL_WALL) return 100;
    if (shoot_hack == SP_EXPLODE) return 100;
    if (shoot_hack == SP_PIERCE) return 100;
    if (shoot_hack == SP_FINAL) return 100;
    if (shoot_hack == SP_NEEDLE) return 100;

    if (shoot_hack == SHOOT_SHATTER) return 100;
    if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT) return 100;
    if (shoot_hack == SHOOT_ELEMENTAL) return 100;
    if (weaponmaster_get_toggle() == TOGGLE_OVERDRAW) return 100;

    if (o_ptr->name2 == EGO_AMMO_ENDURANCE) return 0;
    if (shoot_hack == SP_EVILNESS) return 40;
    if (shoot_hack == SP_HOLYNESS) return 40;
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
        case TV_LITE:
        case TV_SCROLL:
        case TV_SKELETON:
            return 50;

        /* Sometimes break */
        case TV_WAND:
        case TV_SPIKE:
            return 25;
        case TV_ARROW:
            return 20 * MAX(0, p_ptr->shooter_info.breakage) / 100;

        /* Rarely break */
        case TV_SHOT:
        case TV_BOLT:
            return 10 * MAX(0, p_ptr->shooter_info.breakage) / 100;
        default:
            return 10;
    }
}


static void _shot_learn_slay(obj_ptr ammo, int which, cptr msg)
{
    slot_t slot = equip_find_first(obj_is_bow);
    if (slot)
        obj_learn_slay(equip_obj(slot), which, msg);
    obj_learn_slay(ammo, which, msg); 
}

static s16b tot_dam_aux_shot(object_type *o_ptr, int tdam, monster_type *m_ptr)
{
    int mult = 100;
    monster_race *r_ptr = mon_race(m_ptr);

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
            if (monster_living(r_ptr) && have_flag(flgs, OF_SLAY_LIVING))
            {
                _shot_learn_slay(o_ptr, OF_SLAY_LIVING, "slays <color:o>Living</color>");
                if (mult < 143) mult = 143;
            }

            /* Slay Animal */
            if ((have_flag(flgs, OF_SLAY_ANIMAL)) &&
                (r_ptr->flags3 & RF3_ANIMAL))
            {
                mon_lore_3(m_ptr, RF3_ANIMAL);
                _shot_learn_slay(o_ptr, OF_SLAY_ANIMAL, "slays <color:g>Animals</color>");
                if (mult < 162) mult = 162;
            }

            /* Kill Animal */
            if ((have_flag(flgs, OF_KILL_ANIMAL)) &&
                (r_ptr->flags3 & RF3_ANIMAL))
            {
                mon_lore_3(m_ptr, RF3_ANIMAL);
                _shot_learn_slay(o_ptr, OF_KILL_ANIMAL, "slays <color:g>*Animals*</color>");
                if (mult < 224) mult = 224;
            }

            /* Slay Evil */
            if ((have_flag(flgs, OF_SLAY_EVIL)) &&
                (r_ptr->flags3 & RF3_EVIL))
            {
                mon_lore_3(m_ptr, RF3_EVIL);
                _shot_learn_slay(o_ptr, OF_SLAY_EVIL, "slays <color:y>Evil</color>");
                if (mult < 143) mult = 143;
            }

            /* Kill Evil */
            if ((have_flag(flgs, OF_KILL_EVIL)) &&
                (r_ptr->flags3 & RF3_EVIL))
            {
                mon_lore_3(m_ptr, RF3_EVIL);
                _shot_learn_slay(o_ptr, OF_KILL_EVIL, "slays <color:y>*Evil*</color>");
                if (mult < 186) mult = 186;
            }

            /* Slay Good */
            if ((have_flag(flgs, OF_SLAY_GOOD)) &&
                (r_ptr->flags3 & RF3_GOOD))
            {
                mon_lore_3(m_ptr, RF3_GOOD);
                _shot_learn_slay(o_ptr, OF_SLAY_GOOD, "slays <color:W>Good</color>");
                if (mult < 143) mult = 143;
            }

            /* Slay Human */
            if ((have_flag(flgs, OF_SLAY_HUMAN)) &&
                (r_ptr->flags2 & RF2_HUMAN))
            {
                mon_lore_2(m_ptr, RF2_HUMAN);
                _shot_learn_slay(o_ptr, OF_SLAY_HUMAN, "slays <color:s>Humans</color>");
                if (mult < 162) mult = 162;
            }

            /* Kill Human */
            if ((have_flag(flgs, OF_KILL_HUMAN)) &&
                (r_ptr->flags2 & RF2_HUMAN))
            {
                mon_lore_2(m_ptr, RF2_HUMAN);
                _shot_learn_slay(o_ptr, OF_KILL_HUMAN, "slays <color:s>*Humans*</color>");
                if (mult < 234) mult = 234;
            }

            /* Slay Undead */
            if ((have_flag(flgs, OF_SLAY_UNDEAD)) &&
                (r_ptr->flags3 & RF3_UNDEAD))
            {
                mon_lore_3(m_ptr, RF3_UNDEAD);
                _shot_learn_slay(o_ptr, OF_SLAY_UNDEAD, "slays <color:D>Undead</color>");
                if (mult < 190) mult = 190;
            }

            /* Kill Undead */
            if ((have_flag(flgs, OF_KILL_UNDEAD)) &&
                (r_ptr->flags3 & RF3_UNDEAD))
            {
                mon_lore_3(m_ptr, RF3_UNDEAD);
                _shot_learn_slay(o_ptr, OF_KILL_UNDEAD, "slays <color:D>*Undead*</color>");
                if (mult < 280) mult = 280;
            }

            /* Slay Demon */
            if ((have_flag(flgs, OF_SLAY_DEMON)) &&
                (r_ptr->flags3 & RF3_DEMON))
            {
                mon_lore_3(m_ptr, RF3_DEMON);
                _shot_learn_slay(o_ptr, OF_SLAY_DEMON, "slays <color:R>Demons</color>");
                if (mult < 190) mult = 190;
            }

            /* Kill Demon */
            if ((have_flag(flgs, OF_KILL_DEMON)) &&
                (r_ptr->flags3 & RF3_DEMON))
            {
                mon_lore_3(m_ptr, RF3_DEMON);
                _shot_learn_slay(o_ptr, OF_KILL_DEMON, "slays <color:R>*Demons*</color>");
                if (mult < 280) mult = 280;
            }

            /* Slay Orc */
            if ((have_flag(flgs, OF_SLAY_ORC)) &&
                (r_ptr->flags3 & RF3_ORC))
            {
                mon_lore_3(m_ptr, RF3_ORC);
                _shot_learn_slay(o_ptr, OF_SLAY_ORC, "slays <color:U>Orcs</color>");
                if (mult < 190) mult = 190;
            }

            /* Kill Orc */
            if ((have_flag(flgs, OF_KILL_ORC)) &&
                (r_ptr->flags3 & RF3_ORC))
            {
                mon_lore_3(m_ptr, RF3_ORC);
                _shot_learn_slay(o_ptr, OF_KILL_ORC, "slays <color:U>*Orcs*</color>");
                if (mult < 280) mult = 280;
            }

            /* Slay Troll */
            if ((have_flag(flgs, OF_SLAY_TROLL)) &&
                (r_ptr->flags3 & RF3_TROLL))
            {
                mon_lore_3(m_ptr, RF3_TROLL);
                _shot_learn_slay(o_ptr, OF_SLAY_TROLL, "slays <color:g>Trolls</color>");
                if (mult < 190) mult = 190;
            }

            /* Kill Troll */
            if ((have_flag(flgs, OF_KILL_TROLL)) &&
                (r_ptr->flags3 & RF3_TROLL))
            {
                mon_lore_3(m_ptr, RF3_TROLL);
                _shot_learn_slay(o_ptr, OF_KILL_TROLL, "slays <color:g>*Trolls*</color>");
                if (mult < 280) mult = 280;
            }

            /* Slay Giant */
            if ((have_flag(flgs, OF_SLAY_GIANT)) &&
                (r_ptr->flags3 & RF3_GIANT))
            {
                mon_lore_3(m_ptr, RF3_GIANT);
                _shot_learn_slay(o_ptr, OF_SLAY_GIANT, "slays <color:u>Giants</color>");
                if (mult < 190) mult = 190;
            }

            /* Kill Giant */
            if ((have_flag(flgs, OF_KILL_GIANT)) &&
                (r_ptr->flags3 & RF3_GIANT))
            {
                mon_lore_3(m_ptr, RF3_GIANT);
                _shot_learn_slay(o_ptr, OF_KILL_GIANT, "slays <color:u>*Giants*</color>");
                if (mult < 280) mult = 280;
            }

            /* Slay Dragon  */
            if ((have_flag(flgs, OF_SLAY_DRAGON)) &&
                (r_ptr->flags3 & RF3_DRAGON))
            {
                mon_lore_3(m_ptr, RF3_DRAGON);
                _shot_learn_slay(o_ptr, OF_SLAY_DRAGON, "slays <color:r>Dragons</color>");
                if (mult < 190) mult = 190;
            }

            /* Execute Dragon */
            if ((have_flag(flgs, OF_KILL_DRAGON)) &&
                (r_ptr->flags3 & RF3_DRAGON))
            {
                mon_lore_3(m_ptr, RF3_DRAGON);
                _shot_learn_slay(o_ptr, OF_KILL_DRAGON, "slays <color:r>*Dragons*</color>");
                if (mult < 280) mult = 280;

                if ( o_ptr->name1 == ART_BARD_ARROW
                  && m_ptr->r_idx == MON_SMAUG
                  && equip_find_art(ART_BARD) )
                {
                    mult *= 5;
                }
            }

            /* Brand (Acid) */
            if (have_flag(flgs, OF_BRAND_ACID))
            {
                if (r_ptr->flagsr & RFR_EFF_RES_ACID_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_RES_ACID_MASK);
                }
                else
                {
                    _shot_learn_slay(o_ptr, OF_BRAND_ACID, "is <color:g>Acid Branded</color>");
                    if (mult < 162) mult = 162;
                }
            }

            /* Brand (Elec) */
            if (have_flag(flgs, OF_BRAND_ELEC))
            {
                if (r_ptr->flagsr & RFR_EFF_RES_ELEC_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_RES_ELEC_MASK);
                }
                else
                {
                    _shot_learn_slay(o_ptr, OF_BRAND_ELEC, "is <color:b>Lightning Branded</color>");
                    if (mult < 162) mult = 162;
                }
            }

            /* Brand (Fire) */
            if (have_flag(flgs, OF_BRAND_FIRE))
            {
                if (r_ptr->flagsr & RFR_EFF_RES_FIRE_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_RES_FIRE_MASK);
                }
                else
                {
                    _shot_learn_slay(o_ptr, OF_BRAND_FIRE, "has <color:r>Flame Tongue</color>");
                    if (r_ptr->flags3 & RF3_HURT_FIRE)
                    {
                        mon_lore_3(m_ptr, RF3_HURT_FIRE);
                        if (mult < 234) mult = 234;
                    }
                    else if (mult < 162) mult = 162;
                }
            }

            /* Brand (Cold) */
            if (have_flag(flgs, OF_BRAND_COLD))
            {
                if (r_ptr->flagsr & RFR_EFF_RES_COLD_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_RES_COLD_MASK);
                }
                else
                {
                    _shot_learn_slay(o_ptr, OF_BRAND_COLD, "is <color:W>Frost Branded</color>");
                    if (r_ptr->flags3 & RF3_HURT_COLD)
                    {
                        if (mult < 234) mult = 234;
                        mon_lore_3(m_ptr, RF3_HURT_COLD);
                    }
                    else if (mult < 162) mult = 162;
                }
            }

            /* Brand (Poison) */
            if (have_flag(flgs, OF_BRAND_POIS))
            {
                if (r_ptr->flagsr & RFR_EFF_RES_POIS_MASK)
                {
                    mon_lore_r(m_ptr, RFR_EFF_RES_POIS_MASK);
                }
                else
                {
                    _shot_learn_slay(o_ptr, OF_BRAND_POIS, "has <color:G>Viper's Fang</color>");
                    if (mult < 162) mult = 162;
                }
            }

            if (have_flag(flgs, OF_BRAND_MANA))
            {
                int cost = 1 + o_ptr->dd * o_ptr->ds / 2;
                if (cost <= p_ptr->csp)
                {
                    sp_player(-cost);
                    mult += 50;
                    _shot_learn_slay(o_ptr, OF_BRAND_MANA, "is <color:B>Mana Branded</color>");
                }
            }

            break;
        }
    }

    /* Sniper */
    if (p_ptr->pclass == CLASS_SNIPER && shoot_hack)
    {
        int n = sniper_multiplier(shoot_hack, o_ptr, m_ptr) * 10; /* XXX */
        if (n > mult)
            mult = n;
    }

    /* Return the total damage */
    return (tdam * mult / 100);
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
bool do_cmd_fire_aux1(obj_ptr bow, obj_ptr arrows)
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
                tx = dun_mon(cave, target_who)->pos.x;
                ty = dun_mon(cave, target_who)->pos.y;
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
        if (!get_fire_dir(&dir))
        {
            energy_use = 0;
            if (shoot_hack == SP_AWAY) shoot_hack = SP_NONE;
            return FALSE;
        }

        /* Predict the "target" location */
        tx = p_ptr->pos.x + 99 * ddx[dir];
        ty = p_ptr->pos.y + 99 * ddy[dir];

        /* Check for "target request" */
        if ((dir == 5) && target_okay())
        {
            tx = target_col;
            ty = target_row;
        }
    }

    /* Don't shoot at my feet */
    if (tx == p_ptr->pos.x && ty == p_ptr->pos.y)
    {
        energy_use = 0;
        /* project_length is already reset to 0 */
        return FALSE;
    }

    if (!fear_allow_shoot())
    {
        msg_print("You are too scared!");
        energy_use = bow_energy(bow->sval)/NUM_SHOTS;
        if (shoot_hack == SP_AWAY) shoot_hack = SP_NONE;
        return FALSE;
    }

    do_cmd_fire_aux2(bow, arrows, p_ptr->pos.x, p_ptr->pos.y, tx, ty);
    obj_release(arrows, OBJ_RELEASE_QUIET);
    return TRUE;
}
void do_cmd_fire_aux2(obj_ptr bow, obj_ptr arrows, int sx, int sy, int tx, int ty)
{
    int  i, break_chance, y, x, ny, nx, prev_y, prev_x, dd, ds;
    int  tdis, tmul;
    int  bonus, chance;
    int  cur_dis, visible;
    bool no_energy = FALSE;
    int  num_shots = 1;
    bool hit_body = FALSE;
    bool return_ammo = FALSE;
    char o_name[MAX_NLEN];
    point_t path[MAX_SIGHT];
    int  flgs = /*PROJECT_PATH |*/ PROJECT_THRU;
    bool stick_to = FALSE;

    /* Sniper - Cannot shoot a single arrow twice */
    if ((shoot_hack == SP_DOUBLE) && (arrows->number < 2)) shoot_hack = SP_NONE;
    if (shoot_hack == SP_DOUBLE) num_shots = 2;

    /* Describe the object */
    object_desc(o_name, arrows, OD_OMIT_PREFIX | OD_NO_PLURAL | OD_OMIT_INSCRIPTION);

    /* Base damage from thrown object plus launcher bonus */
    dd = arrows->dd;
    ds = arrows->ds;
    if (p_ptr->big_shot)
        ds += 2;

    /* Actually "fire" the object */
    bonus = (p_ptr->shooter_info.to_h + arrows->to_h + bow->to_h);

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
        num_shots = NUM_SHOTS * 100;  /* rescale to 4 decimal places */
        num_shots = num_shots * 120 / 100;  /* rapid fire gives 1.2x the number of shots */
        frac = (num_shots * 100 / energy_fire) % 100;
        num_shots /= energy_fire;

        if (randint1(100) < frac)
            num_shots++;
    }

    chance = p_ptr->skills.thb + bonus * BTH_PLUS_ADJ;
    chance += skills_bow_calc_bonus(bow->sval) * BTH_PLUS_ADJ;

    if (plr_tim_find(T_STUN))
        chance -= chance * MIN(100, plr_tim_amount(T_STUN)) / 150;

    /* Calculate the Multiplier */
    tmul = bow_mult(bow);

    /* Base range */
    tdis = bow_range(bow);
    project_length = tdis + 1;

    /* Get projection path length */
    if (shoot_hack == SHOOT_DISINTEGRATE)
        flgs |= PROJECT_DISI;

    tdis = project_path(path, project_length, p_ptr->pos, point_create(tx, ty), flgs) - 1;

    project_length = 0; /* reset to default */

    /* Take a (partial) turn */
    if (!no_energy)
        energy_use = bow_energy(bow->sval) / NUM_SHOTS;

    /* Sniper - Difficult to shot twice at 1 turn */
    if (shoot_hack == SP_DOUBLE)  p_ptr->concent = (p_ptr->concent + 1) / 2;

    /* Sniper - Repeat shooting when double shots */
    for (i = 0; i < num_shots; i++)
    {
        obj_t arrow;

        /* Make sure there is ammo left over for this shot */
        if (!arrows->number)
        {
            msg_print("Your ammo has run out. Time to reload!");
            break;
        }

        /* Start at the source */
        y = sy;
        x = sx;

        /* Weaponmaster power: Ammo is not consumed */
        if ( (p_ptr->return_ammo || arrows->name2 == EGO_AMMO_RETURNING)
          && randint1(100) <= 50 + p_ptr->lev/2 )
        {
            return_ammo = TRUE;
        }

        arrow = *arrows;
        arrow.number = 1;
        if (!return_ammo)
            arrows->number--;

        stats_on_use(arrows, 1);

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
            cave_ptr c_ptr;
            mon_ptr mon;

            if (weaponmaster_get_toggle() != TOGGLE_PIERCING_ARROW && shoot_hack != SHOOT_PIERCE)
            {
                /* Hack -- Stop at the target */
                if ((y == ty) && (x == tx)) break;
            }
            nx = path[cur_dis].x;
            ny = path[cur_dis].y;

            c_ptr = cave_at_xy(nx, ny);
            mon = mon_at_xy(nx, ny);

            /* Shatter Arrow */
            if (shoot_hack == SP_KILL_WALL)
            {
                if (cave_have_flag_grid(c_ptr, FF_HURT_ROCK) && !mon)
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
                if (cave_have_flag_grid(c_ptr, FF_HURT_ROCK) && !mon)
                {
                    c_ptr->info &= ~(CAVE_MARK);
                    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);
                    cave_alter_feat(ny, nx, FF_HURT_ROCK);
                }
            }

            /* Stopped by walls/doors */
            if (!cave_have_flag_grid(c_ptr, FF_PROJECT) && !mon) break;

            /* Advance the distance */
            cur_dis++;

            /* Sniper */
            if (shoot_hack == SP_LITE)
            {
                c_ptr->info |= (CAVE_GLOW);
                note_spot(ny, nx);
                lite_spot(ny, nx);
            }

            /* The player can see the (on screen) missile */
            if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
            {
                char c = object_char(&arrow);
                byte a = object_attr(&arrow);

                /* Draw, Hilite, Fresh, Pause, Erase */
                print_rel(c, a, ny, nx);
                move_cursor_relative(point_create(nx, ny));
                Term_fresh();
                Term_xtra(TERM_XTRA_DELAY, delay_animation);
                lite_spot(ny, nx);
                Term_fresh();
            }
            /* The player cannot see the missile */
            else
            {
                /* Pause anyway, for consistancy */
                Term_xtra(TERM_XTRA_DELAY, delay_animation);
            }

            /* Sniper */
            if (shoot_hack == SP_KILL_TRAP)
            {
                project(0, 0, ny, nx, 0, GF_KILL_TRAP,
                    (PROJECT_JUMP | PROJECT_HIDE | PROJECT_GRID | PROJECT_ITEM));
            }
            if (shoot_hack == SP_EVILNESS)
            {
                c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);
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
            if (mon)
            {
                int armour;
                bool hit = FALSE;
                monster_race *r_ptr = mon_race(mon);

                /* Check the visibility */
                visible = mon->ml;

                /* Note the collision */
                hit_body = TRUE;

                if (mon_tim_find(mon, MT_SLEEP))
                {
                    if (!(r_ptr->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_COMPASSION, -1);
                    if (!(r_ptr->flags3 & RF3_EVIL) || one_in_(5)) virtue_add(VIRTUE_HONOUR, -1);
                }

                skills_bow_gain(bow->sval, r_ptr->level);
                if (p_ptr->riding)
                    skills_riding_gain_archery(r_ptr);

                armour = mon_ac(mon);
                if (p_ptr->concent)
                {
                    armour *= (10 - p_ptr->concent);
                    armour /= 10;
                }

                if ( p_ptr->painted_target
                  && p_ptr->painted_target_idx == mon->id
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

                    hit = test_hit_fire(chance2 - cur_dis, armour, mon->ml);
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
                    else if (p_ptr->painted_target_idx == mon->id)
                    {
                        p_ptr->painted_target_ct++;
                    }
                    else
                    {
                        p_ptr->painted_target_idx = mon->id;
                        p_ptr->painted_target_ct = 1;
                    }
                }

                if (hit)
                {
                    bool fear = FALSE;
                    int  tdam;
                    bool ambush = FALSE;

                    if (mon_tim_find(mon, MT_SLEEP) && mon->ml && !plr_tim_find(T_CONFUSED) && !plr_tim_find(T_HALLUCINATE) && !plr_tim_find(T_STUN))
                    {
                        if (shoot_hack == SHOOT_SNIPING || (p_ptr->pclass == CLASS_SKILLMASTER && p_ptr->ambush))
                        {
                            ambush = TRUE;
                        }
                    }
                    /* Handle unseen monster */
                    if (!visible)
                    {
                        msg_format("The %s finds a mark.", o_name);
                    }
                    /* Handle visible monster */
                    else
                    {
                        char m_name[80];
                        monster_desc(m_name, mon, 0);
                        if (ambush)
                            cmsg_format(TERM_VIOLET, "You cruelly shoot %s!", m_name);
                        else
                            msg_format("The %s hits %s.", o_name, m_name);

                        if (mon->ml)
                        {
                            if (!plr_tim_find(T_HALLUCINATE)) mon_track(mon);
                            health_track(mon->id);
                        }
                    }

                    if (shoot_hack == SHOOT_NEEDLE)
                    {
                        if ((randint1(randint1(r_ptr->level/7)+5) == 1) && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flags7 & RF7_UNIQUE2))
                        {
                            char m_name[80];
                            monster_desc(m_name, mon, 0);
                            tdam = mon->hp + 1;
                            msg_format("Your shot hit a fatal spot of %s!", m_name);
                        }
                        else
                            tdam = 1;
                    }
                    else if (shoot_hack == SP_NEEDLE)
                    {
                        if ((randint1(randint1(r_ptr->level / (3 + p_ptr->concent)) + (8 - p_ptr->concent)) == 1)
                            && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flags7 & RF7_UNIQUE2))
                        {
                            char m_name[MAX_NLEN];
                            monster_desc(m_name, mon, 0);
                            tdam = mon->hp + 1;
                            msg_format("Your shot hit a fatal spot of %s!", m_name);
                        }
                        else
                            tdam = 1;
                    }
                    else
                    {
                        critical_t crit = {0};

                        /* The Damage Calculation (Changed) */
                        tdam = damroll(dd, ds);
                        tdam += arrow.to_d;
                        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS) && p_ptr->lev >= 15)
                            tdam += 1 + p_ptr->lev/10;

                        if (shoot_hack != SHOOT_SHATTER && shoot_hack != SHOOT_ELEMENTAL)
                            tdam = tot_dam_aux_shot(&arrow, tdam, mon);
                        if (ambush) tdam *= 2;
                        crit = critical_shot(arrow.weight, arrow.to_h);
                        if (crit.desc)
                        {
                            tdam = tdam * crit.mul/100 + crit.to_d;
                            msg_print(crit.desc);
                        }
                        if (p_ptr->concent) tdam = boost_concentration_damage(tdam);

                        tdam *= tmul;
                        tdam /= 100;
                        tdam += bow->to_d;

                        tdam += p_ptr->shooter_info.to_d;
                        /* End of Damage Calculation (Changed) */

                        if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
                        {
                            if (p_ptr->lev >= 20)
                                tdam += bow_range(bow) - cur_dis;

                            if (p_ptr->lev >= 45)
                            {
                                int mult = 100 + (mon->maxhp - mon->hp)*100/(2*mon->maxhp);
                                tdam = tdam * mult / 100;
                            }
                        }
                        if (0) msg_format("<color:B>%d damage</color>", tdam);
                        if (tdam < 0) tdam = 0;
                        tdam = mon_damage_mod(mon, tdam, FALSE);
                    }

                    if (shoot_hack == SP_EXPLODE)
                    {
                        u16b flg = (PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID);
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, ((p_ptr->concent + 1) / 2 + 1), ny, nx, tdam, GF_MISSILE, flg);
                        break;
                    }
                    if (arrow.name2 == EGO_AMMO_EXPLODING)
                    {
                        u16b flg = (PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID);
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, 3, ny, nx, tdam, GF_MISSILE, flg);
                        break;
                    }
                    if (shoot_hack == SHOOT_ELEMENTAL)
                    {
                        int rad = 0;
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                            rad = randint1(2+p_ptr->lev/40);
                        project(0, rad, ny, nx, tdam, GF_FIRE, flg);
                        project(0, rad, ny, nx, tdam, GF_COLD, flg);
                        project(0, rad, ny, nx, tdam, GF_ELEC, flg);
                        project(0, rad, ny, nx, tdam, GF_ACID, flg);
                        project(0, rad, ny, nx, tdam, GF_POIS, flg);
                        break;
                    }
                    if (shoot_hack == SHOOT_SHATTER)
                    {
                        int rad = 0;
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                            rad = randint1(2+p_ptr->lev/40);
                        tdam = tdam * (100 + p_ptr->lev*2)/100;
                        project(0, rad, ny, nx, tdam, GF_SHARDS, flg);
                        break;
                    }
                    if (weaponmaster_get_toggle() == TOGGLE_EXPLODING_BOLT)
                    {
                        int flg = PROJECT_STOP | PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID;
                        sound(SOUND_EXPLODE); /* No explode sound - use breath fire instead */
                        project(0, randint1(2+p_ptr->lev/40), ny, nx, tdam, GF_MISSILE, flg);
                        break;
                    }
                    if (shoot_hack == SP_HOLYNESS)
                    {
                        cave_at_xy(nx, ny)->info |= (CAVE_GLOW);
                        note_spot(ny, nx);
                        lite_spot(ny, nx);
                    }
                    if (plr_tim_find(T_STUN))
                        tdam -= tdam * MIN(100, plr_tim_amount(T_STUN)) / 150;
                    if (mon_take_hit(mon->id, tdam, &fear, NULL))
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

                        message_pain(mon->id, tdam);

                        if (shoot_hack == SHOOT_TRANQUILIZE)
                        {
                            if (mon_tim_find(mon, MT_SLEEP))
                            {
                                if (!one_in_(5))
                                    anger = FALSE;
                            }
                            else if (one_in_(3))
                            {
                                mon_tim_add(mon, MT_SLEEP, 500);
                                anger = FALSE;
                            }

                            /* mon_take_hit() in xtra2.c hacked to not auto wakeup for SHOOT_TRANQUILIZE */
                            if (anger)
                                mon_tim_delete(mon, MT_SLEEP);
                        }

                        if (anger && tdam > 0 && mon->cdis > 1 && allow_ticked_off(r_ptr))
                        {
                            if (!p_ptr->stealthy_snipe)
                                mon_anger_shoot(mon, tdam);
                        }

                        if (anger && tdam > 0)
                            anger_monster(mon);

                        /* Artifact arrows stick to target. Note, we now do this
                           after hurting/angering the monster since Cupid's Arrow
                           might charm the target, while anger_monster() would set
                           it back to being hostile. */
                        if (obj_is_std_art(&arrow))
                        {
                            char m_name[80];
                            monster_desc(m_name, mon, 0);

                            stick_to = one_in_(2);

                            /* If Cupid's Arrow charms the monster,
                               having the arrow stick is highly annoying since
                               you can't command your pets to drop objects they
                               are carrying. */
                            if (arrow.name1 == ART_CUPIDS_ARROW && !(r_ptr->flags1 & RF1_UNIQUE))
                            {
                                if (!mon_save_p(mon->r_idx, A_CHR))
                                {
                                    if (!mon_save_p(mon->r_idx, A_CHR))
                                    {
                                        if (!is_pet(mon))
                                        {
                                            set_pet(mon);
                                            msg_format("%^s is charmed!", m_name);
                                            stick_to = FALSE;
                                        }
                                        else if (!is_friendly(mon))
                                        {
                                            set_friendly(mon);
                                            msg_format("%^s suddenly becomes friendly.", m_name);
                                            stick_to = FALSE;
                                        }
                                    }
                                    else if (!is_pet(mon) && !is_friendly(mon))
                                    {
                                        set_friendly(mon);
                                        msg_format("%^s suddenly becomes friendly.", m_name);
                                        stick_to = FALSE;
                                    }
                                }
                            }

                            if (stick_to)
                                msg_format("%^s have stuck into %s!",o_name, m_name);
                        }

                        if (fear && mon->ml)
                        {
                            char m_name[80];
                            sound(SOUND_FLEE);
                            monster_desc(m_name, mon, 0);
                            msg_format("%^s flees in terror!", m_name);
                        }

                        set_target(mon, p_ptr->pos);

                        if (shoot_hack == SP_RUSH || shoot_hack == SHOOT_KNOCKBACK)
                        {
                            int n = randint1(5) + 3;

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
                                if (!monster_can_cross_terrain(cave_at_xy(nx, ny)->feat, r_ptr, 0)) break;

                                /* Stopped by monsters */
                                if (!cave_empty_bold(ny, nx)) break;

                                dun_move_mon(cave, mon, point_create(nx, ny));

                                lite_spot(ny, nx);
                                lite_spot(oy, ox);

                                Term_fresh();
                                Term_xtra(TERM_XTRA_DELAY, delay_animation);

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
                    monster_desc(m_name, mon, 0);
                    msg_format("The %s misses %s.", o_name, m_name);
                }

                /* The following effects (piercing and bouncing) should
                   not take place when artifact ammo has stuck to a unique! */
                if (!stick_to)
                {
                    if (shoot_hack == SP_PIERCE)
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
                                do_cmd_fire_aux2(bow, arrows, x, y, tx, ty);
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
        break_chance = (hit_body ? breakage_chance(&arrow) : 0);
        if (shoot_hack == SHOOT_DISINTEGRATE) break_chance = 100;

        if (return_ammo)
        {
            if (disturb_minor)
                msg_format("The %s returns to your pack.", o_name);
        }
        else if (stick_to)
        {
            mon_ptr mon = dun_mon_at(cave, point_create(x, y));
            dun_mon_steal_plr(cave, mon, &arrow); 
        }
        else if (cave_have_flag_bold(y, x, FF_PROJECT))
        {
            /* Drop (or break) near that location */
            drop_near(&arrow, point_create(x, y), break_chance);
        }
        else
        {
            /* Drop (or break) near that location */
            drop_near(&arrow, point_create(prev_x, prev_y), break_chance);
        }

    /* Sniper - Repeat shooting when double shots */
    }

    /* Sniper - Loose his/her concentration after any shot */
    if (p_ptr->concent) reset_concentration(FALSE);
}


bool do_cmd_fire(void)
{
    bool         result = FALSE;
    obj_prompt_t prompt = {0};
    int          slot = equip_find_first(obj_is_bow);
    obj_ptr      bow = NULL;

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

    prompt.prompt = "Fire which item?";
    prompt.error = "You have nothing to fire.";
    prompt.filter = obj_can_shoot;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    result = do_cmd_fire_aux1(bow, prompt.obj);
    if (result && p_ptr->pclass == CLASS_SNIPER)
    {
        if (shoot_hack == SP_AWAY)
            teleport_player(10 + (p_ptr->concent * 2), 0L);
        if (shoot_hack == SP_FINAL)
        {
            msg_print("You experience a powerful recoil!");
            plr_tim_add(T_SLOW, randint0(7) + 7);
            if (!p_ptr->no_stun) plr_tim_add(T_STUN, randint1(25));
        }
    }

    return result;
}


/*
 * Player Travel to Fixed Location
 */
static bool _travel_flow_p(point_t pos, dun_grid_ptr grid)
{
    dun_feat_ptr feat;

    if (!(grid->info & CAVE_AWARE)) return FALSE; /* non-spoiling */
    if (is_known_trap(grid)) return FALSE;      /* avoid unnecessary risks */
    /* XXX if (is_jammed_door(grid->feat)) return FALSE; */

    feat = dun_grid_feat_mimic(grid); /* non-spoiling */
    if (have_flag(feat->flags, FF_WALL) || have_flag(feat->flags, FF_CAN_DIG)) return FALSE;
    if (!have_flag(feat->flags, FF_MOVE) && have_flag(feat->flags, FF_CAN_FLY) && !p_ptr->levitation)
        return FALSE;
    if (have_flag(feat->flags, FF_PATTERN))
        return FALSE;
    /*if (have_flag(feat->flags, FF_LAVA) && !elemental_is_(ELEMENTAL_FIRE) && res_pct(RES_FIRE) < 100)
        return FALSE;*/
    return TRUE;
}
static bool _travel_begin_direct(point_t pos)
{
    if (!travel.path) travel.path = point_vec_alloc();
    else point_vec_clear(travel.path);
    travel.path_idx = 0;

    if (point_fast_distance(p_ptr->pos, pos) <= MAX_SIGHT)
    {
        point_t path[MAX_SIGHT];
        int     ct = project_path(path, MAX_SIGHT, p_ptr->pos, pos, 0), i;

        if (ct > 0 && point_equals(path[ct - 1], pos))
        {
            /* A projection path never includes the start position. We'll include it
             * in our travel path for more robust error checking. travel.path[.path_idx]
             * should always be the the plr's position, and .path[.path_idx+1] is the 
             * next position in the travel. */
            assert(!point_equals(p_ptr->pos, path[0]));
            point_vec_add(travel.path, p_ptr->pos);

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
void travel_begin(int mode, point_t pos)
{
    assert(dun_pos_interior(cave, pos));

    if (point_equals(pos, p_ptr->pos))
        return;

    if (_travel_begin_direct(pos))
    {
        assert(travel.path);
        assert(point_vec_length(travel.path));

        if (travel.flow) dun_flow_free(travel.flow);
        travel.flow = NULL;

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
        dun_feat_ptr feat;
        int rng = plr_on_surface() ? MAX(50, point_fast_distance(p_ptr->pos, pos)) : 200;
        rect_t rect;


        rect = rect_create_centered(pos, rng, rng);
        if (!rect_contains_point(rect, p_ptr->pos))
        {
            msg_print("That location is too far away.");
            return;
        }

        grid = dun_grid_at(cave, pos);
        feat = dun_grid_feat_mimic(grid);
        if ( (grid->info & CAVE_MARK)
          && (have_flag(feat->flags, FF_WALL) || have_flag(feat->flags, FF_CAN_DIG)) )
        {
            msg_print("You cannot travel there!");
            return;
        }

        if (travel.flow) dun_flow_free(travel.flow);
        travel.flow = NULL;

        travel.mode = mode;
        travel.run = 0;
        travel.pos = pos;
        travel.flow = dun_flow_calc(cave, pos, rng, _travel_flow_p);
        travel.run = 255;
        travel.dir = 0;

        /* Decides first direction */
        dx = abs(p_ptr->pos.x - pos.x);
        dy = abs(p_ptr->pos.y - pos.y);
        sx = ((pos.x == p_ptr->pos.x) || (dx < dy)) ? 0 : ((pos.x > p_ptr->pos.x) ? 1 : -1);
        sy = ((pos.y == p_ptr->pos.y) || (dy < dx)) ? 0 : ((pos.y > p_ptr->pos.y) ? 1 : -1);

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
        if (point_equals(travel.pos, p_ptr->pos))
            travel_cancel();
    }
    else
    {
        travel.mode = TRAVEL_MODE_NORMAL;
        if (center_player && !center_running) viewport_verify();
    }
}

void do_cmd_travel(void)
{
    int x, y;
    if (cave_have_flag_at(p_ptr->pos, FF_PATTERN))
    {
        /* XXX travel flow does not support walking the pattern. instead,
         * you will receive 255 messages saying you cannot leave the pattern! */
        msg_print("You may not travel while walking the pattern.");
        return;
    }
    if (!tgt_pt(&x, &y, -1)) return;
    travel_begin(TRAVEL_MODE_NORMAL, point_create(x, y));
}


