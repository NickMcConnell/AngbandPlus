/*
 * File: cmd3.c
 * Purpose: Miscellaneous queries
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
#include "cmds.h"
#include "monster/mon-lore.h"
#include "monster/mon-util.h"
#include "object/inventory.h"
#include "s-spells.h"
#include "squelch.h"


/*
 * Drop some gold
 */
void do_cmd_drop_gold(int Ind, s32b amt)
{
    player_type *p_ptr = player_get(Ind);
    object_type tmp_obj;

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to drop gold!");
        return;
    }

    /* Handle the newbies_cannot_drop option */
    if (newbies_cannot_drop(p_ptr))
    {
        msg(p_ptr, "You are not experienced enough to drop gold.");
        return;
    }

    /* Error checks */
    if (amt <= 0) return;
    if (amt > p_ptr->au)
    {
        msg(p_ptr, "You do not have that much gold.");
        return;
    }

    /* Setup the object */
    object_prep(&tmp_obj, lookup_kind(TV_GOLD, SV_GOLD), 0, MINIMISE);

    /* Setup the "worth" */
    tmp_obj.pval[DEFAULT_PVAL] = amt;

    /* Pile of gold is now owned */
    tmp_obj.owner = p_ptr->id;

    /* Drop it */
    drop_near(p_ptr, cave_get(p_ptr->depth), &tmp_obj, 0, p_ptr->py, p_ptr->px, FALSE);

    /* Subtract from the player's gold */
    p_ptr->au -= amt;

    /* Message */
    msg(p_ptr, "You drop %ld gold pieces.", amt);

    /* Redraw gold */
    p_ptr->redraw |= (PR_GOLD);

    /* Take a turn */
    use_energy(Ind);
}


/*
 * Attempt to steal from another player
 */
void do_cmd_steal(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind), *q_ptr;
    int success, notice;
    bool fail = TRUE;
    int y, x, m_idx;

    /* Paranoia */
    if ((dir == 5) || !dir) return;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    y = p_ptr->py + ddy[dir];
    x = p_ptr->px + ddx[dir];
    m_idx = cave_get(p_ptr->depth)->m_idx[y][x];

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to steal items!");
        return;
    }

    /* Not when confused */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        msg(p_ptr, "You are too confused!");
        return;
    }

    /* Restricted by choice */
    if (OPT_P(p_ptr, birth_no_stores))
    {
        msg(p_ptr, "You cannot steal from players.");
        return;
    }

    /* Check preventive inscription '^J' */
    __trap(p_ptr, CPI(p_ptr, 'J'))

    /* May only steal from visible players */
    if ((m_idx >= 0) || !p_ptr->play_vis[0 - m_idx])
    {
        /* Message */
        msg(p_ptr, "You see nothing there to steal from.");
        return;
    }

    /* Examine target */
    q_ptr = player_get(0 - m_idx);

    /* May not steal if hostile */
    if (pvp_check(p_ptr, q_ptr, PVP_CHECK_ONE, TRUE, FEAT_NONE))
    {
        /* Message */
        msg(p_ptr, "%s is on guard against you.", q_ptr->name);
        return;
    }

    /* May not steal if the target cannot retaliate */
    if ((cfg_pvp_hostility >= PVP_SAFE) || in_party(q_ptr, p_ptr->party) ||
        cave_issafefloor(cave_get(p_ptr->depth), y, x))
    {
        /* Message */
        msg(p_ptr, "You cannot steal from that player.");
        return;
    }

    /* Compute chance of success */
    success = 3 * (adj_dex_safe[p_ptr->state.stat_ind[A_DEX]] -
        adj_dex_safe[q_ptr->state.stat_ind[A_DEX]]);

    /* Compute base chance of being noticed */
    notice = 5 * (adj_mag_stat[q_ptr->state.stat_ind[A_INT]] - p_ptr->state.skills[SKILL_STEALTH]);

    /* Hack -- Rogues get bonuses to chances */
    if (player_has(p_ptr, PF_STEALING_IMPROV))
    {
        /* Increase chance by level */
        success += 3 * p_ptr->lev;
        notice -= 3 * p_ptr->lev;
    }

    /* Hack -- Always small chance to succeed */
    if (success < 2) success = 2;

    /* Check for success */
    if (magik(success))
    {
        /* Steal gold 25% of the time */
        if (magik(25))
        {
            int amt = q_ptr->au / 10;

            /* Transfer gold */
            if (amt)
            {
                /* Move from target to thief */
                q_ptr->au -= amt;
                p_ptr->au += amt;

                /* Redraw */
                p_ptr->redraw |= (PR_GOLD);
                q_ptr->redraw |= (PR_GOLD);

                /* Tell thief */
                msg(p_ptr, "You steal %ld gold.", amt);

                /* Check for target noticing */
                if (magik(notice))
                {
                    /* Make target hostile */
                    pvp_check(q_ptr, p_ptr, PVP_ADD, TRUE, FEAT_NONE);

                    /* Message */
                    msg(q_ptr, "You notice %s stealing %ld gold!", p_ptr->name, amt);
                }
                fail = FALSE;
            }
        }
        else
        {
            int item;
            object_type *o_ptr, forge;
            char o_name[NORMAL_WID];

            /* Steal an item */
            item = randint0(q_ptr->inven_cnt);

            /* Get object */
            o_ptr = &q_ptr->inventory[item];

            /* Don't steal (nothing)s or artifacts */
            if (o_ptr->kind && !o_ptr->artifact)
            {
                /* Get a copy with the right "amt" */
                object_distribute_amt(&forge, o_ptr, 1);

                /* Note that the pack is too full */
                if (!inven_carry_okay(p_ptr, &forge))
                {
                    msg(p_ptr, "You cannot carry that many items.");
                    return;
                }

                /* Note that the pack is too heavy */
                if (!weight_okay(p_ptr, &forge))
                {
                    msg(p_ptr, "You are already too burdened to carry another object.");
                    return;
                }
                
                inven_carry(p_ptr, &forge, TRUE);

                /* Take one from target */
                inven_item_increase(q_ptr, item, -1);
                inven_item_optimize(q_ptr, item);

                /* Tell thief what he got */
                object_desc(p_ptr, o_name, sizeof(o_name), &forge, ODESC_PREFIX | ODESC_FULL);
                msg(p_ptr, "You stole %s.", o_name);

                /* Easier to notice heavier objects */
                notice += forge.weight;

                /* Check for target noticing */
                if (magik(notice))
                {
                    /* Make target hostile */
                    pvp_check(q_ptr, p_ptr, PVP_ADD, TRUE, FEAT_NONE);

                    /* Message */
                    msg(q_ptr, "You notice %s stealing %s!", p_ptr->name, o_name);
                }
                fail = FALSE;
            }
        }
    }
    
    if (fail)
    {
        /* Message */
        msg(p_ptr, "You fail to steal anything.");

        /* Easier to notice a failed attempt */
        if (magik(notice + 50))
        {
            /* Make target hostile */
            pvp_check(q_ptr, p_ptr, PVP_ADD, TRUE, FEAT_NONE);

            /* Message */
            msg(q_ptr, "You notice %s trying to steal from you!", p_ptr->name);
        }
    }

    /* Take a turn */
    use_energy(Ind);
}


/*
 * Display known info about a monster or group of monsters specified by name or symbol
 */
void do_cmd_query_symbol(int Ind, const char *buf)
{
    player_type *p_ptr = player_get(Ind);
    int i;
    bool found = FALSE;
    char* str;

    /* Let the player scroll through this info */
    p_ptr->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p_ptr);

    /* Lowercase our search string */
    if (strlen(buf) > 1)
    {
        for (str = (char*)buf; *str; str++) *str = tolower((unsigned char)*str);
    }

    /* Collect matching monsters */
    for (i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];
        bool ok;

        /* Skip non-entries */
        if (!r_ptr->name) continue;

        /* Check if the input is a symbol */
        if (strlen(buf) == 1) ok = (r_ptr->d_char == buf[0]);

        /* The input is a partial monster name */
        else
        {
            char monster[NORMAL_WID];
            char* dst;

            /* Clean up monster name */
            dst = monster;
            for (str = r_ptr->name; *str; str++)
            {
                if (isalpha(*str) || (*str == 32)) *dst++ = tolower((unsigned char)*str);
            }
            *dst++ = '\0';

            /* Check if cleaned name matches our search string */
            ok = (strstr(monster, buf) != NULL);
        }

        /* Dump info */
        if (ok)
        {
            /* Describe it */
            if (found) text_out(p_ptr, "\n\n\n");
            monster_info_screen(p_ptr, i);

            found = TRUE;
        }
    }
    if (!found) text_out(p_ptr, "You fail to remember any monsters of this kind.");

    /* Restore height and width of current dungeon level */
    text_out_done(p_ptr);

    /* Notify player */
    notify_player(Ind, format("Monster Recall ('%s')", buf), NTERM_WIN_MONSTER, FALSE);
}


/*
 * Display known info about a monster/player
 */
void do_cmd_describe(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    monster_type *m_ptr;
    int r_idx;

    /* We need something */
    if (p_ptr->cursor_who == 0) return;

    /* Get the monster */
    if (p_ptr->cursor_who > 0)
    {
        m_ptr = cave_monster(cave_get(p_ptr->depth), p_ptr->cursor_who);
        r_idx = m_ptr->r_idx;
    }

    /* Get the player */
    else
        r_idx = p_ptr->cursor_who;

    /* Describe it */
    if (r_idx < 0) describe_player(p_ptr, 0 - r_idx);
    else describe_monster(p_ptr, r_idx);

    /* Notify player */
    notify_player(Ind, "Monster Recall", NTERM_WIN_MONSTER, FALSE);
}


/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(struct player *p, int dir)
{
    int y1, x1, y2, x2;
    char tmp_val[NORMAL_WID];
    char out_val[160];
    int panel_wid, panel_hgt;

    panel_wid = BLOCK_WID / p->tile_wid;
    panel_hgt = BLOCK_HGT / p->tile_hgt;

    /* No direction, recenter */
    if (!dir)
    {
        /* Forget current panel */
        p->offset_y_old = p->offset_x_old = -1;

        /* Recenter map around the player */
        verify_panel(p);

        return;
    }

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    /* Initialize */
    if (dir == 5)
    {
        /* Remember current panel */
        p->offset_y_old = p->offset_y;
        p->offset_x_old = p->offset_x;
    }

    /* Start at current panel */
    y1 = p->offset_y_old;
    x1 = p->offset_x_old;

    /* Apply the motion */
    change_panel(p, dir);

    /* Handle stuff */
    handle_stuff(p);

    /* Get the current panel */
    y2 = p->offset_y;
    x2 = p->offset_x;

    /* Describe the location */
    if ((y2 == y1) && (x2 == x1))
        tmp_val[0] = '\0';
    else
    {
        strnfmt(tmp_val, sizeof(tmp_val), "%s%s of", ((y2 < y1)? " north": (y2 > y1)? " south": ""),
            ((x2 < x1)? " west": (x2 > x1)? " east": ""));
    }

    /* Prepare to ask which way to look */
    strnfmt(out_val, sizeof(out_val), "Map sector [%d,%d], which is%s your sector.  Direction?",
        (y2 / panel_hgt), (x2 / panel_wid), tmp_val);

    /* More detail */
    if (OPT_P(p, center_player))
    {
        strnfmt(out_val, sizeof(out_val),
            "Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
            (y2 / panel_hgt), (y2 % panel_hgt), (x2 / panel_wid), (x2 % panel_wid), tmp_val);
    }

    msg(p, out_val);
}


/*
 * Return the object kind with the given `tval` and `sval`, or 0.
 * No message logged.
 */
static object_kind *lookup_kind_silent(int tval, int sval)
{
    int k;

    /* Look for it */
    for (k = 1; k < z_info->k_max; k++)
    {
        object_kind *k_ptr = &k_info[k];

        /* Found a match */
        if ((k_ptr->tval == tval) && (k_ptr->sval == sval)) return k_ptr;
    }

    /* Failure */
    return 0;
}


/*
 * Drink/fill an empty bottle from a fountain
 */
void do_cmd_fountain(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    object_type *i_ptr;
    object_type object_type_body;
    object_kind *kind;

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to interact with fountains!");
        return;
    }

    /* Check preventive inscription '^_' */
    __trap(p_ptr, CPI(p_ptr, '_'))

    /* We must stand on a fountain */
    if (!cave_fountain_basic(cave_get(p_ptr->depth)->feat[p_ptr->py][p_ptr->px]))
    {
        msg(p_ptr, "There is no fountain here.");
        return;
    }

    /* Dried out */
    if (cave_get(p_ptr->depth)->feat[p_ptr->py][p_ptr->px] == FEAT_FNT_DRIED)
    {
        msg(p_ptr, "The fountain is dried out.");
        return;
    }

    /* Fruit bat! */
    if (item == -1)
    {
        s16b race_fruit_bat = get_r_idx("Fruit bat");
        bool poly = FALSE;

        /* Try (very rarely) to turn the player into a fruit bat */
        if ((p_ptr->r_idx != race_fruit_bat) && one_in_(200)) poly = TRUE;

        /* Try (very hard) to restore characters back to normal */
        if ((p_ptr->r_idx == race_fruit_bat) && one_in_(3)) poly = TRUE;

        if (poly)
        {
            msg(p_ptr, "You drink from the fountain.");
            poly_bat(p_ptr, 100, NULL);

            /* Done */
            return;
        }
    }

    /* Summon water creature */
    if (one_in_(20))
    {
        static const struct summon_chance_t
        {
            const char *race;
            byte minlev;
            byte chance;
        } summon_chance[] =
        {
            {"Giant green frog", 0, 100},
            {"Giant red frog", 7, 90},
            {"Water spirit", 17, 80},
            {"Water vortex", 21, 70},
            {"Water elemental", 33, 60},
            {"Water hound", 43, 50}
        };
        int i;
        s16b r_idx;

        msg(p_ptr, "Something pops out of the water!");
        do {i = randint0(N_ELEMENTS(summon_chance));}
        while ((p_ptr->depth < summon_chance[i].minlev) || !magik(summon_chance[i].chance));
        r_idx = get_r_idx(summon_chance[i].race);
        summon_specific_race(Ind, p_ptr->depth, p_ptr->py, p_ptr->px, r_idx, 1);

        /* Done */
        return;
    }

    /* Fall in */
    if (one_in_(20))
    {
        msg(p_ptr, "You slip and fall in the water.");
        if (!player_passwall(p_ptr) && !can_swim(p_ptr))
        {
            my_strcpy(p_ptr->died_flavor, "slipped and fell in a fountain",
                sizeof(p_ptr->died_flavor));
            take_hit(p_ptr, damroll(4, 5), "drowning", FALSE);
        }

        /* Done */
        return;
    }

    /* Message */
    if (item == -1) msg(p_ptr, "You drink from the fountain.");

    /* Ale */
    if (one_in_(10))
    {
        msg(p_ptr, "Wow! Pure dwarven ale!");
        kind = lookup_kind(TV_FOOD, SV_FOOD_PINT_OF_ALE);
    }

    /* Plain water */
    else if (one_in_(2))
    {
        msg(p_ptr, "The water is clear and fresh.");
        kind = lookup_kind(TV_POTION, SV_POTION_WATER);
    }

    /* Random potion effect */
    else
    {
        int sval;

        /* Generate a potion */
        while (TRUE)
        {
            /* Pick a random potion */
            sval = randint1(SV_POTION_MAX);

            /* Access the kind index */
            kind = lookup_kind_silent(TV_POTION, sval);
            if (!kind) continue;

            /* No out of depth effect */
            if (p_ptr->depth < kind->alloc_min) continue;

            /* Less chance to get the effect deeper */
            if ((p_ptr->depth > kind->alloc_max) && magik(50)) continue;

            /* Apply rarity */
            if (!magik(kind->alloc_prob)) continue;

            /* Success */
            break;
        }

        /* Message */
        if (!kind->cost)
            msg(p_ptr, "The water is dark and muddy.");
        else
            msg(p_ptr, "The water sparkles gently.");
    }

    /* Get local object */
    i_ptr = &object_type_body;

    /* Prepare the object */
    object_prep(i_ptr, kind, p_ptr->depth, RANDOMISE);

    /* Set origin */
    set_origin(i_ptr, ORIGIN_FOUNTAIN, p_ptr->depth, 0);

    /* Get an empty bottle */
    if (item >= 0)
    {
        object_type *o_ptr = &p_ptr->inventory[item];

        /* Paranoia: requires a bottle */
        if (!o_ptr->kind || (o_ptr->tval != TV_BOTTLE)) return;

        /* The inscription prevents it */
        __trap(p_ptr, CGI(o_ptr, '_', FALSE))

        /* Eliminate the item */
        inven_item_increase(p_ptr, item, -1);
        inven_item_describe(p_ptr, item);
        inven_item_optimize(p_ptr, item);

        /* Create the object */
        apply_magic(p_ptr, p_ptr->depth, i_ptr, p_ptr->depth, FALSE, FALSE, FALSE);

        /* Drop it in the dungeon */
        drop_near(p_ptr, cave_get(p_ptr->depth), i_ptr, 0, p_ptr->py, p_ptr->px, TRUE);
    }

    /* Drink from a fountain */
    else
    {
        bool dummy = FALSE;

        p_ptr->was_aware = object_flavor_is_aware(p_ptr, i_ptr);
        use_object(Ind, i_ptr, &dummy, 0);
    }

    /* Fountain dries out */
    if (one_in_(3))
    {
        msg(p_ptr, "The fountain suddenly dries up.");
        cave_set_feat(cave_get(p_ptr->depth), p_ptr->py, p_ptr->px, FEAT_FNT_DRIED);
    }
}


/*
 * Centers the map on the player
 */
void do_cmd_center_map(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Not while shopping */
    if (in_store(p_ptr)) return;

    center_panel(Ind);
}
