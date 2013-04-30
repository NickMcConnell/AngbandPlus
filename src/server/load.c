/*
 * File: load.c
 * Purpose: Savefile loading functions
 *
 * Copyright (c) 1997 Ben Harrison, and others
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
#include "history.h"
#include "monster/mon-make.h"
#include "monster/mon-spell.h"
#include "savefile.h"


/* Shorthand function pointer for rd_item version */
typedef int (*rd_item_t)(object_type *o_ptr);


/*
 * Find an ego item from its index
 */
static struct ego_item *lookup_ego(int idx)
{
    if ((idx > 0) && (idx < z_info->e_max))
        return &e_info[idx];

    return NULL;
}


/*
 * Read an object
 *
 * This function no longer attempts to "repair" old savefiles - the info
 * held in o_ptr is now authoritative.
 */
static int rd_item(object_type *o_ptr)
{
    byte ego_idx;
    byte art_idx;
    size_t i, j;
    char buf[128];
    s16b tmp16s;

    /* Skip version */
    strip_bytes(1);

    /* Skip description */
    strip_string(NORMAL_WID);

    /* Location */
    rd_byte(&o_ptr->iy);
    rd_byte(&o_ptr->ix);
    rd_s16b(&o_ptr->depth);

    /* Type/Subtype */
    rd_byte(&o_ptr->tval);
    rd_byte(&o_ptr->sval);
    for (i = 0; i < MAX_PVALS; i++) rd_s32b(&o_ptr->pval[i]);
    rd_byte(&o_ptr->num_pvals);

    rd_byte(&o_ptr->number);
    rd_s16b(&o_ptr->weight);

    rd_byte(&art_idx);
    rd_byte(&ego_idx);
    rd_s32b(&o_ptr->randart_seed);

    rd_s16b(&o_ptr->timeout);

    rd_s16b(&o_ptr->to_h);
    rd_s16b(&o_ptr->to_d);
    rd_s16b(&o_ptr->to_a);

    rd_s16b(&o_ptr->ac);

    rd_byte(&o_ptr->dd);
    rd_byte(&o_ptr->ds);

    rd_u16b(&o_ptr->ident);

    rd_byte(&o_ptr->origin);
    rd_s16b(&o_ptr->origin_depth);
    rd_u16b(&o_ptr->origin_xtra);

    rd_byte(&o_ptr->ignore);

    for (i = 0; i < OF_SIZE; i++) rd_byte(&o_ptr->flags[i]);

    of_wipe(o_ptr->known_flags);
    for (i = 0; i < OF_SIZE; i++) rd_byte(&o_ptr->known_flags[i]);

    for (j = 0; j < MAX_PVALS; j++)
        for (i = 0; i < OF_SIZE; i++)
            rd_byte(&o_ptr->pval_flags[j][i]);

    /* Monster holding object */
    rd_s16b(&o_ptr->held_m_idx);

    rd_s16b(&o_ptr->mimicking_m_idx);

    /* Save the inscription */
    rd_string(buf, sizeof(buf));
    if (buf[0]) o_ptr->note = quark_add(buf);

    /* PWMAngband */
    rd_s32b(&o_ptr->creator);
    rd_s32b(&o_ptr->owner);
    rd_byte(&o_ptr->squelch);
    rd_byte(&o_ptr->ordered);
    rd_u16b(&o_ptr->effect);
    rd_s16b(&tmp16s);
    o_ptr->time.base = tmp16s;
    rd_s16b(&tmp16s);
    o_ptr->time.dice = tmp16s;
    rd_s16b(&tmp16s);
    o_ptr->time.sides = tmp16s;

    /* Lookup item kind */
    o_ptr->kind = lookup_kind(o_ptr->tval, o_ptr->sval);
    if (!o_ptr->kind) return 0;

    o_ptr->ego = lookup_ego(ego_idx);

    if (art_idx >= z_info->a_max + 9) return -1;
    if (!o_ptr->randart_seed && (art_idx >= z_info->a_max)) return -1;
    if (art_idx > 0) o_ptr->artifact = &a_info[art_idx];

    /* Success */
    return (0);
}


int rd_monster_memory(struct player *p)
{
    int r_idx;
    u16b tmp16u;

    /* Monster Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->r_max)
    {
        plog_fmt("Too many (%u) monster races!", tmp16u);
        return (-1);
    }

    /* Read the available records */
    for (r_idx = 0; r_idx < tmp16u; r_idx++)
    {
        size_t i;
        monster_race *r_ptr = &r_info[r_idx];
        monster_lore* l_ptr = (p? &p->lore[r_idx]: &r_info[r_idx].lore);

        /* Count sights/deaths/kills */
        rd_byte(&l_ptr->spawned);
        rd_byte(&l_ptr->seen);
        rd_byte(&l_ptr->pseen);
        rd_s16b(&l_ptr->pdeaths);
        rd_s16b(&l_ptr->tdeaths);
        rd_s16b(&l_ptr->pkills);
        rd_s16b(&l_ptr->tkills);

        /* Count wakes and ignores */
        rd_byte(&l_ptr->wake);
        rd_byte(&l_ptr->ignore);

        /* Count drops */
        rd_byte(&l_ptr->drop_gold);
        rd_byte(&l_ptr->drop_item);

        /* Count spells */
        rd_byte(&l_ptr->cast_innate);
        rd_byte(&l_ptr->cast_spell);

        /* Count blows of each type */
        for (i = 0; i < MONSTER_BLOW_MAX; i++)
            rd_byte(&l_ptr->blows[i]);

        /* Memorize flags */
        for (i = 0; i < RF_SIZE; i++)
            rd_byte(&l_ptr->flags[i]);
        for (i = 0; i < RSF_SIZE; i++)
            rd_byte(&l_ptr->spell_flags[i]);

        /* Repair the spell lore flags */
        rsf_inter(l_ptr->spell_flags, r_ptr->spell_flags);
    }

    /* Success */
    return (0);
}


int rd_object_memory(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Object Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->k_max)
    {
        plog_fmt("Too many (%u) object kinds!", tmp16u);
        return (-1);
    }

    /* Read the object memory */
    for (i = 0; i < tmp16u; i++)
    {
        byte tmp8u;

        rd_byte(&tmp8u);

        p->obj_aware[i] = ((tmp8u & 0x01)? TRUE: FALSE);
        p->obj_tried[i] = ((tmp8u & 0x02)? TRUE: FALSE);
        p->kind_everseen[i] = ((tmp8u & 0x04)? 1: 0);
    }

    /* Ego Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->e_max)
    {
        plog_fmt("Too many (%u) ego items!", tmp16u);
        return (-1);
    }

    /* Read the ego memory */
    for (i = 1; i < tmp16u; i++) rd_byte(&p->ego_everseen[i]);

    /* Success */
    return (0);
}


int rd_player_artifacts(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Read the character artifact info */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->a_max)
    {
        plog_fmt("Too many (%u) artifacts!", tmp16u);
        return (-1);
    }

    /* Read the artifact flags */
    for (i = 0; i < tmp16u; i++) rd_byte(&p->art_info[i]);

    /* Read the randart flags */
    for (i = 0; i < tmp16u + 9; i++)
    {
        rd_byte(&p->randart_info[i]);
        rd_byte(&p->randart_created[i]);
    }

    /* Success */
    return (0);
}


int rd_artifacts(struct player *unused)
{
    int i;
    u16b tmp16u;

    /* Load the Artifacts */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->a_max)
    {
        plog_fmt("Too many (%u) artifacts!", tmp16u);
        return (-1);
    }

    /* Read the artifact flags */
    for (i = 0; i < tmp16u; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        rd_byte(&a_ptr->created);
        rd_byte(&a_ptr->owned);
    }

    /* Success */
    return (0);
}


/*
 * Read the "extra" information
 */
int rd_player(struct player *p)
{
    int i;
    byte num;

    rd_s32b(&p->id);

    /* Verify player ID */
    if (!p->id) p->id = player_id++;

    rd_string(p->died_from, NORMAL_WID);
    rd_string(p->died_flavor, 160);

    rd_string(p->death_info.title, NORMAL_WID);
    rd_s16b(&p->death_info.max_lev);
    rd_s16b(&p->death_info.lev);
    rd_s32b(&p->death_info.max_exp);
    rd_s32b(&p->death_info.exp);
    rd_s32b(&p->death_info.au);
    rd_s16b(&p->death_info.max_depth);
    rd_s16b(&p->death_info.depth);
    rd_string(p->death_info.died_from, NORMAL_WID);
    rd_s32b((s32b*)&p->death_info.time);
    rd_string(p->death_info.ctime, NORMAL_WID);

    for (i = 0; i < N_HIST_LINES; i++) rd_string(p->history[i], N_HIST_WRAP);
    rd_string(p->descrip, N_HIST_LINES * N_HIST_WRAP);

    /* Special Race/Class info */
    rd_byte(&p->hitdie);
    rd_s16b(&p->expfact);

    /* Age/Height/Weight */
    rd_s16b(&p->age);
    rd_s16b(&p->ht);
    rd_s16b(&p->wt);

    /* Read the stat info */
    for (i = 0; i < A_MAX; ++i) rd_s16b(&p->stat_max[i]);
    for (i = 0; i < A_MAX; ++i) rd_s16b(&p->stat_cur[i]);
    for (i = 0; i < A_MAX; ++i) rd_s16b(&p->stat_birth[i]);

    rd_s32b(&p->au);

    rd_s32b(&p->max_exp);
    rd_s32b(&p->exp);
    rd_u16b(&p->exp_frac);

    rd_s16b(&p->lev);

    /* Verify player level */
    if ((p->lev < 1) || (p->lev > PY_MAX_LEVEL))
    {
        plog_fmt("Invalid player level (%d).", p->lev);
        return (-1);
    }

    rd_s16b(&p->mhp);
    rd_s16b(&p->chp);
    rd_u16b(&p->chp_frac);

    rd_s16b(&p->msp);
    rd_s16b(&p->csp);
    rd_u16b(&p->csp_frac);

    /* Max Player and Dungeon Levels */
    rd_s16b(&p->max_lev);
    rd_s16b(&p->max_depth);

    /* Hack -- Repair maximum player level */
    if (p->max_lev < p->lev) p->max_lev = p->lev;

    /* Hack -- Repair maximum dungeon level */
    if (p->max_depth < 0) p->max_depth = 0;

    p->recall_depth = p->max_depth;

    /* More info */
    rd_s16b(&p->sc);

    /* Read the flags */
    rd_s16b(&p->food);
    rd_s32b(&p->energy);
    rd_s16b(&p->word_recall);
    rd_s16b(&p->deep_descent);
    rd_byte(&p->confusing);
    rd_byte(&p->searching);

    /* Find the number of timed effects */
    rd_byte(&num);

    if (num <= TMD_MAX)
    {
        /* Read all the effects */
        for (i = 0; i < num; i++)
            rd_s16b(&p->timed[i]);

        /* Initialize any entries not read */
        if (num < TMD_MAX)
            C_WIPE(p->timed + num, TMD_MAX - num, s16b);
    }
    else
    {
        /* Probably in trouble anyway */
        for (i = 0; i < TMD_MAX; i++)
            rd_s16b(&p->timed[i]);

        /* Discard unused entries */
        strip_bytes(2 * (num - TMD_MAX));
        plog("Discarded unsupported timed effects.");
    }

    /* Success */
    return (0);
}


int rd_player_misc(struct player *p)
{
    /* Special stuff */
    rd_u16b(&p->total_winner);
    rd_byte(&p->noscore);

    /* Read "death" */
    rd_bool(&p->is_dead);

    /* Read "feeling" */
    rd_s16b(&p->feeling);
    rd_u16b(&p->cave->feeling_squares);

    /* PWMAngband */
    rd_hturn(&p->game_turn);
    rd_hturn(&p->player_turn);
    rd_hturn(&p->active_turn);
    rd_hturn(&p->quit_turn);
    rd_s16b(&p->ghost);
    rd_byte(&p->lives);
    rd_bool(&OPT_P(p, birth_ironman));
    rd_bool(&OPT_P(p, birth_no_stores));
    rd_bool(&OPT_P(p, birth_no_artifacts));
    rd_bool(&OPT_P(p, birth_no_feelings));
    rd_bool(&OPT_P(p, birth_no_selling));
    rd_bool(&OPT_P(p, birth_no_ghost));
    rd_bool(&OPT_P(p, birth_fruit_bat));
    rd_s16b(&p->quest.r_idx);
    rd_s16b(&p->quest.cur_num);
    rd_s16b(&p->quest.max_num);
    rd_s16b(&p->quest.timer);
    rd_byte(&p->party);
    rd_u16b(&p->retire_timer);
    rd_s16b(&p->tim_mimic_what);
    rd_s16b(&p->r_idx);
    rd_s16b(&p->k_idx);

    /* Success */
    return (0);
}


int rd_misc(struct player *unused)
{
    /* Hack -- The two "special seeds" */
    rd_u32b(&seed_flavor);
    rd_u32b(&seed_town);

    /* Current turn */
    rd_hturn(&turn);

    /* PWMAngband */
    rd_s32b(&player_id);

    /* Success */
    return (0);
}


int rd_player_hp(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Read the player_hp array */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > PY_MAX_LEVEL)
    {
        plog_fmt("Too many (%u) hitpoint entries!", tmp16u);
        return (-1);
    }

    /* Read the player_hp array */
    for (i = 0; i < tmp16u; i++) rd_s16b(&p->player_hp[i]);

    /* Success */
    return (0);
}


int rd_player_spells(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Read the number of spells */
    rd_u16b(&tmp16u);
    if (tmp16u > PY_MAX_SPELLS)
    {
        plog_fmt("Too many player spells (%d).", tmp16u);
        return (-1);
    }

    /* Read the spell flags */
    for (i = 0; i < tmp16u; i++) rd_byte(&p->spell_flags[i]);

    /* Read the spell order */
    for (i = 0; i < tmp16u; i++) rd_byte(&p->spell_order[i]);

    /* Read spell power */
    for (i = 0; i < tmp16u; i++) rd_byte(&p->spell_power[i]);

    /* Success */
    return (0);
}


/*
 * Read the player inventory
 *
 * Note that the inventory is re-sorted later by dungeon().
 */
static int rd_inventory_aux(struct player *p, rd_item_t rd_item_version)
{
    int slot = 0;
    object_type *i_ptr;
    object_type object_type_body;

    /* No weight */
    p->total_weight = 0;

    /* No items */
    p->inven_cnt = 0;

    /* Read until done */
    while (1)
    {
        u16b n;

        /* Get the next item index */
        rd_u16b(&n);

        /* Nope, we reached the end */
        if (n == 0xFFFF) break;

        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        object_wipe(i_ptr);

        /* Read the item */
        if ((*rd_item_version)(i_ptr))
        {
            plog("Error reading item");
            return (-1);
        }

        /* Hack -- verify item */
        if (!i_ptr->kind) continue;

        /* Verify slot */
        if (n >= ALL_INVEN_TOTAL) return (-1);

        /* Repair artifacts */
        if (i_ptr->artifact)
        {
            if (true_artifact_p(i_ptr))
            {
                if (!i_ptr->artifact->created) i_ptr->artifact->created = 1;
                if (i_ptr->owner) i_ptr->artifact->owned = 1;
            }
            else
            {
                p->randart_created[i_ptr->artifact->aidx] = 1;
                if (!i_ptr->creator) i_ptr->creator = p->id;
            }
        }

        /* Wield equipment */
        if (n >= INVEN_WIELD)
        {
            /* Copy object */
            object_copy(&p->inventory[n], i_ptr);

            /* Add the weight */
            p->total_weight += (i_ptr->number * i_ptr->weight);
        }

        /* Warning -- backpack is full */
        else if (p->inven_cnt == INVEN_PACK)
        {
            /* Oops */
            plog("Too many items in the inventory!");

            /* Fail */
            return (-1);
        }

        /* Carry inventory */
        else
        {
            /* Get a slot */
            n = slot++;

            /* Copy object */
            object_copy(&p->inventory[n], i_ptr);

            /* Add the weight */
            p->total_weight += (i_ptr->number * i_ptr->weight);

            /* One more item */
            p->inven_cnt++;
        }
    }

    save_quiver_size(p, FALSE);

    /* Success */
    return (0);
}


/*
 * Read the player inventory - wrapper function
 */
int rd_inventory(struct player *p) {return rd_inventory_aux(p, rd_item);}


/* Read store contents */
static int rd_stores_aux(rd_item_t rd_item_version)
{
    int i;
    u16b tmp16u;

    /* Read the stores */
    rd_u16b(&tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        struct store *st_ptr = &stores[i];
        int j;
        byte own;
        s16b num;

        /* Read the basic info */
        rd_byte(&own);
        rd_s16b(&num);

        st_ptr->owner = store_ownerbyidx(st_ptr, own);

        /* Read the items */
        for (j = 0; j < num; j++)
        {
            object_type *i_ptr;
            object_type object_type_body;

            /* Get local object */
            i_ptr = &object_type_body;

            /* Wipe the object */
            object_wipe(i_ptr);

            /* Read the item */
            if ((*rd_item_version)(i_ptr))
            {
                plog("Error reading item");
                return (-1);
            }

            /* Accept any valid items */
            if ((st_ptr->stock_num < STORE_INVEN_MAX) && i_ptr->kind)
            {
                int k = st_ptr->stock_num++;

                /* Accept the item */
                object_copy(&st_ptr->stock[k], i_ptr);
            }
        }
    }

    /* Read the store orders */
    for (i = 0; i < STORE_MIN_KEEP; i++) rd_string(store_orders[i], NORMAL_WID);

    /* Success */
    return (0);
}


/*
 * Read the stores - wrapper function
 */
int rd_stores(struct player *unused) {return rd_stores_aux(rd_item);}


/*
 * Read the dungeon (player)
 */
int rd_player_dungeon(struct player *p)
{
    int i, y, x;
    u16b ymax, xmax;
    byte count;
    byte tmp8u;

    /* Only if the player's alive */
    if (p->is_dead) return 0;

    /*** Basic info ***/

    /* Header info */
    rd_s16b(&p->depth);
    rd_s16b(&p->py);
    rd_s16b(&p->px);
    rd_u16b(&ymax);
    rd_u16b(&xmax);

    /* PWMAngband */
    rd_s16b(&p->world_y);
    rd_s16b(&p->world_x);

    /*** Run length decoding ***/

    /* Load the dungeon data */
    for (x = y = 0; y < ymax; )
    {
        /* Grab RLE info */
        rd_byte(&count);
        rd_byte(&tmp8u);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            /* Extract "info" */
            p->cave->info[y][x] = tmp8u;

            /* Advance/Wrap */
            if (++x >= xmax)
            {
                /* Wrap */
                x = 0;

                /* Advance/Wrap */
                if (++y >= ymax) break;
            }
        }
    }

    /* Success */
    return (0);
}


/*
 * Read the dungeon (depth)
 */
int rd_depth_dungeon(struct player *unused)
{
    int i, y, x;
    s16b depth;
    u16b ymax, xmax;
    byte count;
    byte tmp8u;
    hturn generated;

    /*** Basic info ***/

    /* Header info */
    rd_s16b(&depth);
    rd_u16b(&ymax);
    rd_u16b(&xmax);

    /* Ignore illegal dungeons */

    /* How many players are on this depth and turn of creation */
    rd_s16b(&players_on_depth[depth]);
    rd_hturn(&generated);

    /* The staircase locations on this depth */
    /* Hack -- this information is currently not present for the wilderness levels. */
    if (depth >= 0)
    {
        rd_byte(&level_up_y[depth]);
        rd_byte(&level_up_x[depth]);
        rd_byte(&level_down_y[depth]);
        rd_byte(&level_down_x[depth]);
        rd_byte(&level_rand_y[depth]);
        rd_byte(&level_rand_x[depth]);
    }

    /*
     * Allocate the memory for the dungeon if it has not already
     * been allocated - which it might have been if we are loading
     * a special static level file
     */
    if (!cave_get(depth)) alloc_dungeon_level(depth);

    cave_get(depth)->width = xmax;
    cave_get(depth)->height = ymax;

    /*** Run length decoding ***/

    /* Load the dungeon data */
    for (x = y = 0; y < DUNGEON_HGT; )
    {
        /* Grab RLE info */
        rd_byte(&count);
        rd_byte(&tmp8u);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            /* Extract "feat" */
            cave_get(depth)->feat[y][x] = tmp8u;

            /* Advance/Wrap */
            if (++x >= DUNGEON_WID)
            {
                /* Wrap */
                x = 0;

                /* Advance/Wrap */
                if (++y >= DUNGEON_HGT) break;
            }
        }
    }

    /*** Run length decoding ***/

    /* Load the dungeon data */
    for (x = y = 0; y < DUNGEON_HGT; )
    {
        /* Grab RLE info */
        rd_byte(&count);
        rd_byte(&tmp8u);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            int feat = cave_get(depth)->feat[y][x];

            /* Extract "info" */
            cave_get(depth)->info[y][x] = tmp8u;

            /* Handle "wall/door" grids */
            if (((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_PERM_SOLID)) ||
                ((feat >= FEAT_TREE) && (feat <= FEAT_PERM_CLEAR)) ||
                ((feat >= FEAT_HOME_HEAD) && (feat <= FEAT_HOME_TAIL)))
            {
                cave_get(depth)->info[y][x] |= (CAVE_WALL);
            }

            /* Advance/Wrap */
            if (++x >= DUNGEON_WID)
            {
                /* Wrap */
                x = 0;

                /* Advance/Wrap */
                if (++y >= DUNGEON_HGT) break;
            }
        }
    }

    /*** Success ***/

    /* The dungeon is ready */
    ht_copy(&cave_get(depth)->generated, &generated);

    return 0;
}


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now hard-coded to
 * DUNGEON_HGT by DUNGEON_WID, and any dungeon with another
 * size will be silently discarded by this routine.
 *
 * Note that dungeon objects, including objects held by monsters, are
 * placed directly into the dungeon, using "object_copy()", which will
 * copy "iy", "ix", and "held_m_idx", leaving "next_o_idx" blank for
 * objects held by monsters, since it is not saved in the savefile.
 *
 * After loading the monsters, the objects being held by monsters are
 * linked directly into those monsters.
 */
int rd_dungeon(struct player *unused)
{
    int i;
    u32b tmp32u;

    /* Read the number of levels to be loaded */
    rd_u32b(&tmp32u);

    /* Read the levels */
    for (i = 0; i < tmp32u; i++)
    {
        if (rd_depth_dungeon(NULL)) return (-1);
    }

    /* Success */
    return (0);
}


/* Read the floor object list */
static int rd_objects_aux(rd_item_t rd_item_version)
{
    int i;
    s16b limit;

    /* Read the item count */
    rd_s16b(&limit);

    /* Verify maximum */
    if (limit > z_info->o_max)
    {
        plog_fmt("Too many (%d) object entries!", limit);
        return (-1);
    }

    /* Read the dungeon items */
    for (i = 1; i < limit; i++)
    {
        object_type *i_ptr;
        object_type object_type_body;
        s16b o_idx;
        object_type *o_ptr;

        /* Get the object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        object_wipe(i_ptr);

        /* Read the item */
        if ((*rd_item_version)(i_ptr))
        {
            plog("Error reading item");
            return (-1);
        }

        /* Make an object */
        o_idx = o_pop();

        /* Paranoia */
        if (o_idx != i)
        {
            plog_fmt("Cannot place object %d!", i);
            return (-1);
        }

        /* Get the object */
        o_ptr = object_byid(o_idx);

        /* Structure Copy */
        object_copy(o_ptr, i_ptr);
    }

    /* Success */
    return (0);
}


/*
 * Read the object list - wrapper function
 */
int rd_objects(struct player *unused) {return rd_objects_aux(rd_item);}


static int rd_depth_monsters(void)
{
    int i;
    size_t j;
    s16b limit;

    /* Read the monster count */
    rd_s16b(&limit);

    /* Hack -- Verify */
    if (limit > z_info->m_max)
    {
        plog_fmt("Too many (%d) monster entries!", limit);
        return (-1);
    }

    /* Read the monsters */
    for (i = 1; i < limit; i++)
    {
        monster_type *m_ptr;
        monster_type monster_type_body;
        byte flags;
        byte tmp8u;

        /* Get local monster */
        m_ptr = &monster_type_body;
        WIPE(m_ptr, monster_type);

        /* Read in record */
        rd_s16b(&m_ptr->r_idx);
        rd_byte(&m_ptr->fy);
        rd_byte(&m_ptr->fx);
        rd_s16b(&m_ptr->depth);
        rd_s16b(&m_ptr->hp);
        rd_s16b(&m_ptr->maxhp);
        rd_byte(&m_ptr->mspeed);
        rd_s32b(&m_ptr->energy);
        rd_byte(&tmp8u);

        for (j = 0; j < tmp8u; j++) rd_s16b(&m_ptr->m_timed[j]);

        /* Read and extract the flag */
        rd_byte(&flags);
        m_ptr->unaware = ((flags & 0x01)? TRUE: FALSE);

        for (j = 0; j < OF_SIZE; j++) rd_byte(&m_ptr->known_pflags[j]);

        rd_s16b(&m_ptr->mimicked_k_idx);

        rd_s16b(&m_ptr->ac);
        for (j = 0; j < MONSTER_BLOW_MAX; j++)
        {
            rd_byte(&m_ptr->blow[j].method);
            rd_byte(&m_ptr->blow[j].effect);
            rd_byte(&m_ptr->blow[j].d_dice);
            rd_byte(&m_ptr->blow[j].d_side);
        }
        rd_s16b(&m_ptr->level);
        rd_s16b(&m_ptr->master);
        rd_byte(&m_ptr->lifespan);
        rd_byte(&m_ptr->status);
        rd_byte(&m_ptr->clone);
        rd_byte(&m_ptr->origin);

        /* Place monster in dungeon (skip if failure) */
        place_monster(NULL, m_ptr, 0);
    }

    /* Success */
    return (0);
}


int rd_monsters(struct player *unused)
{
    int i;
    u32b tmp32u;

    /* Read the number of levels to be loaded */
    rd_u32b(&tmp32u);

    /* Read the monsters */
    for (i = 0; i < tmp32u; i++)
    {
        if (rd_depth_monsters()) return (-1);
    }

    /* Reacquire objects */
    for (i = 1; i < o_max; ++i)
    {
        object_type *o_ptr;
        monster_type *m_ptr;

        /* Get the object */
        o_ptr = object_byid(i);

        /* Check for mimics */
        if (o_ptr->mimicking_m_idx)
        {
            /* Verify monster index */
            if (o_ptr->mimicking_m_idx >= cave_monster_max(cave_get(o_ptr->depth)))
            {
                char base_name[NORMAL_WID];

                plog_fmt("Invalid monster index: %d", o_ptr->mimicking_m_idx);

                object_desc(NULL, base_name, sizeof(base_name), o_ptr, ODESC_BASE);
                plog_fmt("Deleting: %s", base_name);

                object_wipe(o_ptr);

                continue;
            }

            /* Get the monster */
            m_ptr = cave_monster(cave_get(o_ptr->depth), o_ptr->mimicking_m_idx);

            /* Link the monster to the object */
            m_ptr->mimicked_o_idx = i;
        }
        else if (o_ptr->held_m_idx)
        {
            /* Verify monster index */
            if (o_ptr->held_m_idx >= cave_monster_max(cave_get(o_ptr->depth)))
            {
                char base_name[NORMAL_WID];

                plog_fmt("Invalid monster index: %d", o_ptr->held_m_idx);

                object_desc(NULL, base_name, sizeof(base_name), o_ptr, ODESC_BASE);
                plog_fmt("Deleting: %s", base_name);

                object_wipe(o_ptr);

                continue;
            }

            /* Get the monster */
            m_ptr = cave_monster(cave_get(o_ptr->depth), o_ptr->held_m_idx);

            /* Skip already acquired objects */
            if (m_ptr->hold_o_idx == i) continue;

            /* Link the object to the pile */
            o_ptr->next_o_idx = m_ptr->hold_o_idx;

            /* Link the monster to the object */
            m_ptr->hold_o_idx = i;
        }

        /* Ignore dungeon objects */
        else continue;
    }

    /* Success */
    return (0);
}


int rd_history(struct player *p)
{
    int i;

    /* Paranoia */
    history_wipe(p);

    /* Read the character event history */
    rd_s16b(&p->history_size);
    for (i = 0; i < p->history_size; i++)
    {
        history_info entry;

        rd_u16b(&entry.type);
        rd_hturn(&entry.turn);
        rd_s16b(&entry.dlev);
        rd_s16b(&entry.clev);
        rd_byte(&entry.a_idx);
        rd_string(entry.name, sizeof(entry.name));
        rd_string(entry.event, sizeof(entry.event));

        COPY(&p->history_list[i], &entry, history_info);
    }
    rd_s16b(&p->history_ctr);

    /* Success */
    return (0);
}


/*** PWMAngband ***/


/*
 * Hack - Read basic player info
 */
int rd_header(struct player *p)
{
    byte num;

    rd_string(p->name, NORMAL_WID);

    /* Skip password */
    strip_string(NORMAL_WID);

    /* Player race */
    rd_byte(&num);
    p->race = player_id2race(num);

    /* Verify player race */
    if (!p->race)
    {
        plog_fmt("Invalid player race (%d).", num);
        return (-1);
    }

    /* Player class */
    rd_byte(&num);
    p->clazz = player_id2class(num);

    /* Verify player class */
    if (!p->clazz)
    {
        plog_fmt("Invalid player class (%d).", num);
        return (-1);
    }

    /* Player gender */
    rd_byte(&p->psex);
    p->sex = &sex_info[p->psex];

    /* Success */
    return (0);
}


int rd_wild_map(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Get the map size */
    rd_u16b(&tmp16u);

    /* If too many map entries */
    if (tmp16u > MAX_WILD / 8)
    {
        plog_fmt("Too many (%u) map entries!", tmp16u);
        return (-1);
    }

    /* Read in the map */
    for (i = 0; i < tmp16u; i++) rd_byte(&p->wild_map[i]);

    /* Success */
    return (0);
}


/*
 * Read some party info
 */
static void rd_party(int n)
{
    party_type *party_ptr = &parties[n];

    rd_string(party_ptr->name, NORMAL_WID);
    rd_string(party_ptr->owner, 20);
    rd_s32b(&party_ptr->num);
    rd_hturn(&party_ptr->created);
}


int rd_parties(struct player *unused)
{
    int i;
    u16b tmp16u;

    /* Read the parties */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > MAX_PARTIES)
    {
        plog_fmt("Too many (%u) party entries!", tmp16u);
        return (-1);
    }

    /* Read the available records */
    for (i = 0; i < tmp16u; i++) rd_party(i);

    /* Success */
    return (0);
}


/*
 * Read some house info
 */
static void rd_house(int n)
{
    house_type *house_ptr = &houses[n];

    rd_byte(&house_ptr->x_1);
    rd_byte(&house_ptr->y_1);
    rd_byte(&house_ptr->x_2);
    rd_byte(&house_ptr->y_2);
    rd_byte(&house_ptr->door_y);
    rd_byte(&house_ptr->door_x);
    rd_s32b(&house_ptr->depth);
    rd_s32b(&house_ptr->price);
    rd_s32b(&house_ptr->ownerid);
    rd_string(house_ptr->ownername, NORMAL_WID);
    rd_byte(&house_ptr->color);
}


int rd_houses(struct player *unused)
{
    int i;

    /* Read house info */
    rd_u16b(&num_houses);

    /* Incompatible save files */
    if (num_houses > MAX_HOUSES)
    {
        plog_fmt("Too many (%u) house entries!", num_houses);
        return (-1);
    }

    /* Read the available records */
    for (i = 0; i < num_houses; i++) rd_house(i);

    /* Success */
    return (0);
}


/*
 * Read some arena info
 */
static void rd_arena(int n)
{
    arena_type *arena_ptr = &arenas[n];

    rd_byte(&arena_ptr->x_1);
    rd_byte(&arena_ptr->y_1);
    rd_byte(&arena_ptr->x_2);
    rd_byte(&arena_ptr->y_2);
    rd_s32b(&arena_ptr->depth);
}


int rd_arenas(struct player *unused)
{
    int i;

    /* Read arena info */
    rd_u16b(&num_arenas);

    /* Incompatible save files */
    if (num_arenas > MAX_ARENAS)
    {
        plog_fmt("Too many (%u) arena entries!", num_arenas);
        return (-1);
    }

    /* Read the available records */
    for (i = 0; i < num_arenas; i++) rd_arena(i);

    /* Success */
    return (0);
}


static void rd_wild(int n)
{
    wilderness_type *w_ptr = &wild_info[0 - n];

    rd_bool(&w_ptr->generated);
}


int rd_wilderness(struct player *unused)
{
    int i;
    u32b tmp32u;

    /* Read wilderness info */
    rd_u32b(&tmp32u);

    /* Incompatible save files */
    if (tmp32u > MAX_WILD)
    {
        plog_fmt("Too many (%u) wilderness entries!", tmp32u);
        return (-1);
    }

    /* Read the available records */
    for (i = 1; i <= tmp32u; i++) rd_wild(i);

    /* Success */
    return (0);
}


int rd_player_names(struct player *unused)
{
    size_t i;
    u32b tmp32u;
    char name[NORMAL_WID];

    /* Read the player name database */
    rd_u32b(&tmp32u);

    /* Read the available records */
    for (i = 0; i < tmp32u; i++)
    {
        s32b id;
        u32b account;
        hturn death_turn;

        /* Read the ID */
        rd_s32b(&id);

        /* Read the account ID */
        rd_u32b(&account);

        /* Read the player name */
        rd_string(name, sizeof(name));

        /* Read the time of death */
        rd_hturn(&death_turn);

        /* Store the player name */
        add_player_name(id, account, name, &death_turn);
    }

    /* Success */
    return (0);
}
