/*
 * File: save.c
 * Purpose: Savefile saving
 *
 * Copyright (c) 1997 Ben Harrison
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
#include "monster/mon-make.h"
#include "party.h"
#include "savefile.h"


/*
 * Write an "item" record
 */
static void wr_item(object_type *o_ptr)
{
    char obj_name[NORMAL_WID];
    size_t i, j;

    wr_byte(ITEM_VERSION);

    /* Description */
    object_desc(NULL, obj_name, sizeof(obj_name), o_ptr, ODESC_PREFIX | ODESC_FULL);
    wr_string(obj_name);

    /* Location */
    wr_byte(o_ptr->iy);
    wr_byte(o_ptr->ix);
    wr_s16b(o_ptr->depth);

    wr_byte(o_ptr->tval);
    wr_byte(o_ptr->sval);
    for (i = 0; i < MAX_PVALS; i++) wr_s32b(o_ptr->pval[i]);
    wr_byte(o_ptr->num_pvals);

    wr_byte(o_ptr->number);
    wr_s16b(o_ptr->weight);

    wr_byte(o_ptr->artifact? o_ptr->artifact->aidx: 0);
    wr_byte(o_ptr->ego? o_ptr->ego->eidx: 0);
    wr_s32b(o_ptr->randart_seed);

    wr_s16b(o_ptr->timeout);

    wr_s16b(o_ptr->to_h);
    wr_s16b(o_ptr->to_d);
    wr_s16b(o_ptr->to_a);
    wr_s16b(o_ptr->ac);
    wr_byte(o_ptr->dd);
    wr_byte(o_ptr->ds);

    wr_u16b(o_ptr->ident);

    /* Origin */
    wr_byte(o_ptr->origin);
    wr_s16b(o_ptr->origin_depth);
    wr_u16b(o_ptr->origin_xtra);

    wr_byte(o_ptr->ignore);

    for (i = 0; i < OF_SIZE; i++) wr_byte(o_ptr->flags[i]);

    for (i = 0; i < OF_SIZE; i++) wr_byte(o_ptr->known_flags[i]);

    for (j = 0; j < MAX_PVALS; j++)
        for (i = 0; i < OF_SIZE; i++)
            wr_byte(o_ptr->pval_flags[j][i]);

    /* Held by monster index */
    wr_s16b(o_ptr->held_m_idx);

    wr_s16b(o_ptr->mimicking_m_idx);

    /* Save the inscription (if any) */
    if (o_ptr->note)
        wr_string(quark_str(o_ptr->note));
    else
        wr_string("");

    /* PWMAngband */
    wr_s32b(o_ptr->creator);
    wr_s32b(o_ptr->owner);
    wr_byte(o_ptr->squelch);
    wr_byte(o_ptr->ordered);
    wr_u16b(o_ptr->effect);
    wr_s16b(o_ptr->time.base);
    wr_s16b(o_ptr->time.dice);
    wr_s16b(o_ptr->time.sides);
}


void wr_monster_memory(int Ind)
{
    int r_idx;

    /* Dump the monster lore */
    wr_u16b(z_info->r_max);
    for (r_idx = 0; r_idx < z_info->r_max; r_idx++)
    {
        int i;
        monster_lore* l_ptr = ((Ind > 0)? &player_get(Ind)->lore[r_idx]: &r_info[r_idx].lore);

        /* Count sights/deaths/kills */
        wr_byte(l_ptr->spawned);
        wr_byte(l_ptr->seen);
        wr_byte(l_ptr->pseen);
        wr_s16b(l_ptr->pdeaths);
        wr_s16b(l_ptr->tdeaths);
        wr_s16b(l_ptr->pkills);
        wr_s16b(l_ptr->tkills);

        /* Count wakes and ignores */
        wr_byte(l_ptr->wake);
        wr_byte(l_ptr->ignore);

        /* Count drops */
        wr_byte(l_ptr->drop_gold);
        wr_byte(l_ptr->drop_item);

        /* Count spells */
        wr_byte(l_ptr->cast_innate);
        wr_byte(l_ptr->cast_spell);

        /* Count blows of each type */
        for (i = 0; i < MONSTER_BLOW_MAX; i++)
            wr_byte(l_ptr->blows[i]);

        /* Memorize flags */
        for (i = 0; i < RF_SIZE; i++)
            wr_byte(l_ptr->flags[i]);
        for (i = 0; i < RSF_SIZE; i++)
            wr_byte(l_ptr->spell_flags[i]);
    }
}


void wr_object_memory(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Dump the object memory */
    wr_u16b(z_info->k_max);
    for (i = 0; i < z_info->k_max; i++)
    {
        byte tmp8u = 0;

        if (p_ptr->obj_aware[i]) tmp8u |= 0x01;
        if (p_ptr->obj_tried[i]) tmp8u |= 0x02;
        if (p_ptr->kind_everseen[i]) tmp8u |= 0x04;

        wr_byte(tmp8u);
    }

    /* Dump the ego memory */
    wr_u16b(z_info->e_max);
    for (i = 1; i < z_info->e_max; i++) wr_byte(p_ptr->ego_everseen[i]);
}


void wr_player_artifacts(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Write the artifact sold list */
    wr_u16b(z_info->a_max);
    for (i = 0; i < z_info->a_max; i++) wr_byte(p_ptr->art_info[i]);

    /* Write the randart info */
    for (i = 0; i < z_info->a_max + 9; i++)
    {
        wr_byte(p_ptr->randart_info[i]);
        wr_byte(p_ptr->randart_created[i]);
    }
}


void wr_artifacts(int unused)
{
    int i;

    /* Hack -- Dump the artifacts */
    wr_u16b(z_info->a_max);
    for (i = 0; i < z_info->a_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        wr_byte(a_ptr->created);
        wr_byte(a_ptr->owned);
    }
}


/*
 * Write some "extra" info
 */
void wr_player(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    wr_s32b(p_ptr->id);

    wr_string(p_ptr->died_from);
    wr_string(p_ptr->died_flavor);

    wr_string(p_ptr->death_info.title);
    wr_s16b(p_ptr->death_info.max_lev);
    wr_s16b(p_ptr->death_info.lev);
    wr_s32b(p_ptr->death_info.max_exp);
    wr_s32b(p_ptr->death_info.exp);
    wr_s32b(p_ptr->death_info.au);
    wr_s16b(p_ptr->death_info.max_depth);
    wr_s16b(p_ptr->death_info.depth);
    wr_string(p_ptr->death_info.died_from);
    wr_s32b((s32b)p_ptr->death_info.time);
    wr_string(p_ptr->death_info.ctime);

    for (i = 0; i < N_HIST_LINES; i++) wr_string(p_ptr->history[i]);
    wr_string(p_ptr->descrip);

    wr_byte(p_ptr->hitdie);
    wr_s16b(p_ptr->expfact);

    wr_s16b(p_ptr->age);
    wr_s16b(p_ptr->ht);
    wr_s16b(p_ptr->wt);

    /* Dump the stats (maximum and current and birth) */
    for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_max[i]);
    for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_cur[i]);
    for (i = 0; i < A_MAX; ++i) wr_s16b(p_ptr->stat_birth[i]);

    wr_s32b(p_ptr->au);

    wr_s32b(p_ptr->max_exp);
    wr_s32b(p_ptr->exp);
    wr_u16b(p_ptr->exp_frac);
    wr_s16b(p_ptr->lev);

    wr_s16b(p_ptr->mhp);
    wr_s16b(p_ptr->chp);
    wr_u16b(p_ptr->chp_frac);

    wr_s16b(p_ptr->msp);
    wr_s16b(p_ptr->csp);
    wr_u16b(p_ptr->csp_frac);

    /* Max Player and Dungeon Levels */
    wr_s16b(p_ptr->max_lev);
    wr_s16b(p_ptr->max_depth);

    /* More info */
    wr_s16b(p_ptr->sc);
    wr_s16b(p_ptr->food);
    wr_s32b(p_ptr->energy);
    wr_s16b(p_ptr->word_recall);
    wr_s16b(p_ptr->deep_descent);
    wr_byte(p_ptr->confusing);
    wr_byte(p_ptr->searching);

    /* Find the number of timed effects */
    wr_byte(TMD_MAX);

    /* Read all the effects, in a loop */
    for (i = 0; i < TMD_MAX; i++) wr_s16b(p_ptr->timed[i]);
}


void wr_player_misc(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Special stuff */
    wr_u16b(p_ptr->total_winner);
    wr_byte(p_ptr->noscore);

    /* Write death */
    wr_byte(p_ptr->is_dead);

    /* Write feeling */
    wr_s16b(p_ptr->feeling);
    wr_u16b(p_ptr->cave->feeling_squares);

    /* PWMAngband */
    wr_hturn(&p_ptr->game_turn);
    wr_hturn(&p_ptr->player_turn);
    wr_hturn(&p_ptr->active_turn);
    wr_hturn(&turn);
    wr_s16b(p_ptr->ghost);
    wr_byte(p_ptr->lives);
    wr_byte(OPT_P(p_ptr, birth_ironman));
    wr_byte(OPT_P(p_ptr, birth_no_stores));
    wr_byte(OPT_P(p_ptr, birth_no_artifacts));
    wr_byte(OPT_P(p_ptr, birth_no_feelings));
    wr_byte(OPT_P(p_ptr, birth_no_selling));
    wr_byte(OPT_P(p_ptr, birth_no_ghost));
    wr_byte(OPT_P(p_ptr, birth_fruit_bat));
    wr_s16b(p_ptr->quest.r_idx);
    wr_s16b(p_ptr->quest.cur_num);
    wr_s16b(p_ptr->quest.max_num);
    wr_s16b(p_ptr->quest.timer);
    wr_byte(p_ptr->party);
    wr_u16b(p_ptr->retire_timer);
    wr_s16b(p_ptr->tim_mimic_what);
    wr_s16b(p_ptr->r_idx);
    wr_s16b(p_ptr->k_idx);
}


void wr_misc(int unused)
{
    /* Write the "object seeds" */
    wr_u32b(seed_flavor);
    wr_u32b(seed_town);

    /* Current turn */
    wr_hturn(&turn);

    /* PWMAngband */
    wr_s32b(player_id);
}


void wr_player_hp(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Dump the "player hp" entries */
    wr_u16b(PY_MAX_LEVEL);
    for (i = 0; i < PY_MAX_LEVEL; i++) wr_s16b(p_ptr->player_hp[i]);
}


void wr_player_spells(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Write spell data */
    wr_u16b(PY_MAX_SPELLS);
    for (i = 0; i < PY_MAX_SPELLS; i++) wr_byte(p_ptr->spell_flags[i]);

    /* Dump the ordered spells */
    for (i = 0; i < PY_MAX_SPELLS; i++) wr_byte(p_ptr->spell_order[i]);

    /* Dump spell power */
    for (i = 0; i < PY_MAX_SPELLS; i++) wr_byte(p_ptr->spell_power[i]);
}


void wr_inventory(int Ind)
{
    int i;

    /* Write the inventory */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &player_get(Ind)->inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Dump index */
        wr_u16b((u16b)i);

        /* Dump object */
        wr_item(o_ptr);
    }

    /* Add a sentinel */
    wr_u16b(0xFFFF);
}


void wr_stores(int unused)
{
    int i;

    /* Note the stores */
    wr_u16b(MAX_STORES);

    /* Dump the stores */
    for (i = 0; i < MAX_STORES; i++)
    {
        const struct store *st_ptr = &stores[i];
        int j;

        /* Save the current owner */
        wr_byte(st_ptr->owner->oidx);

        /* Save the stock size */
        wr_s16b(st_ptr->stock_num);

        /* Save the stock */
        for (j = 0; j < st_ptr->stock_num; j++)
        {
            /* Save each item in stock */
            wr_item(&st_ptr->stock[j]);
        }
    }

    /* Dump the store orders */
    for (i = 0; i < STORE_MIN_KEEP; i++) wr_string(store_orders[i]);
}


/*
 * Write the current dungeon (player)
 */
void wr_player_dungeon(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int y, x;
    byte tmp8u;
    byte count;
    byte prev_char;

    if (p_ptr->is_dead) return;

    /*** Basic info ***/

    /* Dungeon specific info follows */
    wr_s16b(p_ptr->depth);
    wr_s16b(p_ptr->py);
    wr_s16b(p_ptr->px);
    wr_u16b(DUNGEON_HGT);
    wr_u16b(DUNGEON_WID);

    /* PWMAngband */
    wr_s16b(p_ptr->world_y);
    wr_s16b(p_ptr->world_x);

    /*** Simple "Run-Length-Encoding" of cave ***/

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Dump the cave */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        for (x = 0; x < DUNGEON_WID; x++)
        {
            /* Extract the important player_cave->info flags */
            tmp8u = (p_ptr->cave->info[y][x] & (CAVE_MARK | CAVE_DTRAP));

            /* If the run is broken, or too full, flush it */
            if ((tmp8u != prev_char) || (count == MAX_UCHAR))
            {
                wr_byte((byte)count);
                wr_byte((byte)prev_char);
                prev_char = tmp8u;
                count = 1;
            }

            /* Continue the run */
            else
                count++;
        }
    }

    /* Flush the data (if any) */
    if (count)
    {
        wr_byte((byte)count);
        wr_byte((byte)prev_char);
    }
}


/*
 * Write the current dungeon (depth)
 */
void wr_depth_dungeon(int depth)
{
    int y, x;
    byte tmp8u;
    byte count;
    byte prev_char;
    struct cave *c = cave_get(depth);

    /*** Basic info ***/

    /* Depth */
    wr_s16b(depth);

    /* Dungeon size */
    wr_u16b(c->height);
    wr_u16b(c->width);

    /* How many players are on this depth + turn of creation */
    wr_s16b(players_on_depth[depth]);
    wr_hturn(&c->generated);

    /* The staircase locations on this depth */
    /* Hack -- this information is currently not present for the wilderness levels. */
    if (depth >= 0)
    {
        wr_byte(level_up_y[depth]);
        wr_byte(level_up_x[depth]);
        wr_byte(level_down_y[depth]);
        wr_byte(level_down_x[depth]);
        wr_byte(level_rand_y[depth]);
        wr_byte(level_rand_x[depth]);
    }

    /*** Simple "Run-Length-Encoding" of cave ***/

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Dump the cave */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        for (x = 0; x < DUNGEON_WID; x++)
        {
            /* Extract a byte */
            tmp8u = c->feat[y][x];

            /* If the run is broken, or too full, flush it */
            if ((tmp8u != prev_char) || (count == MAX_UCHAR))
            {
                wr_byte((byte)count);
                wr_byte((byte)prev_char);
                prev_char = tmp8u;
                count = 1;
            }

            /* Continue the run */
            else
                count++;
        }
    }

    /* Flush the data (if any) */
    if (count)
    {
        wr_byte((byte)count);
        wr_byte((byte)prev_char);
    }

    /*** Simple "Run-Length-Encoding" of cave ***/

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Dump the cave */
    for (y = 0; y < DUNGEON_HGT; y++)
    {
        for (x = 0; x < DUNGEON_WID; x++)
        {
            /* Extract the important cave->info flags */
            tmp8u = (c->info[y][x] & (CAVE_GLOW | CAVE_ICKY | CAVE_ROOM | CAVE_FEEL | CAVE_NOTELE));

            /* If the run is broken, or too full, flush it */
            if ((tmp8u != prev_char) || (count == MAX_UCHAR))
            {
                wr_byte((byte)count);
                wr_byte((byte)prev_char);
                prev_char = tmp8u;
                count = 1;
            }

            /* Continue the run */
            else
                count++;
        }
    }

    /* Flush the data (if any) */
    if (count)
    {
        wr_byte((byte)count);
        wr_byte((byte)prev_char);
    }
}


/*
 * Write the current dungeon
 */
void wr_dungeon(int unused)
{
    int i;
    u32b tmp32u;

    /* Get the number of levels to dump */
    tmp32u = 0;
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        /* Make sure the level has been allocated */
        if ((players_on_depth[i] || check_special_level(i)) && cave_get(i))
            tmp32u++;
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the levels players are actually on - and special levels */
    /* Note that this saves the players_on_depth information */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        if ((players_on_depth[i] || check_special_level(i)) && cave_get(i))
            wr_depth_dungeon(i);
    }

    /*** Compact ***/

    /* Compact the objects */
    compact_objects(0);

    /* Compact the monsters */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        if (cave_get(i)) compact_monsters(cave_get(i), 0);
    }
}


void wr_objects(int unused)
{
    int i;

    /* Total objects */
    wr_s16b(o_max);

    /* Dump the objects */
    for (i = 1; i < o_max; i++) wr_item(object_byid(i));
}


static void wr_depth_monsters(int depth)
{
    int i, j;

    /* Total monsters */
    wr_s16b(cave_monster_max(cave_get(depth)));

    /* Dump the monsters */
    for (i = 1; i < cave_monster_max(cave_get(depth)); i++)
    {
        byte unaware = 0;
        const monster_type *m_ptr = cave_monster(cave_get(depth), i);

        wr_s16b(m_ptr->r_idx);
        wr_byte(m_ptr->fy);
        wr_byte(m_ptr->fx);
        wr_s16b(m_ptr->depth);
        wr_s16b(m_ptr->hp);
        wr_s16b(m_ptr->maxhp);
        wr_byte(m_ptr->mspeed);
        wr_s32b(m_ptr->energy);

        wr_byte(MON_TMD_MAX);

        for (j = 0; j < MON_TMD_MAX; j++)
            wr_s16b(m_ptr->m_timed[j]);

        if (m_ptr->unaware) unaware |= 0x01;
        wr_byte(unaware);

        for (j = 0; j < OF_SIZE; j++) wr_byte(m_ptr->known_pflags[j]);

        wr_s16b(m_ptr->mimicked_k_idx);

        /* New level-related data */
        wr_s16b(m_ptr->ac);
        for (j = 0; j < MONSTER_BLOW_MAX; j++)
        {
            wr_byte(m_ptr->blow[j].method);
            wr_byte(m_ptr->blow[j].effect);
            wr_byte(m_ptr->blow[j].d_dice);
            wr_byte(m_ptr->blow[j].d_side);
        }
        wr_s16b(m_ptr->level);
        wr_s16b(m_ptr->master);
        wr_byte(m_ptr->lifespan);
        wr_byte(m_ptr->status);
        wr_byte(m_ptr->clone);
        wr_byte(m_ptr->origin);
    }
}


void wr_monsters(int unused)
{
    int i;
    u32b tmp32u = 0;

    /* Get the number of levels to dump */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        /* Make sure the level has been allocated */
        if (cave_get(i)) tmp32u++;
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the monsters */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        if (cave_get(i)) wr_depth_monsters(i);
    }
}


void wr_history(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Write character event history */
    wr_s16b(p_ptr->history_size);
    for (i = 0; i < p_ptr->history_size; i++)
    {
        wr_u16b(p_ptr->history_list[i].type);
        wr_hturn(&p_ptr->history_list[i].turn);
        wr_s16b(p_ptr->history_list[i].dlev);
        wr_s16b(p_ptr->history_list[i].clev);
        wr_byte(p_ptr->history_list[i].a_idx);
        wr_string(p_ptr->history_list[i].name);
        wr_string(p_ptr->history_list[i].event);
    }
    wr_s16b(p_ptr->history_ctr);
}


/*** PWMAngband ***/


/*
 * Hack - Save basic player info
 */
void wr_header(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    wr_string(p_ptr->name);
    wr_string(p_ptr->pass);

    /* Race/Class/Gender/Spells */
    wr_byte(p_ptr->race->ridx);
    wr_byte(p_ptr->clazz->cidx);
    wr_byte(p_ptr->psex);
}


void wr_wild_map(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Write the wilderness map */
    wr_u16b(MAX_WILD / 8);
    for (i = 0; i < MAX_WILD / 8; i++) wr_byte(p_ptr->wild_map[i]);
}


static void wr_party(party_type *party_ptr)
{
    wr_string(party_ptr->name);
    wr_string(party_ptr->owner);
    wr_s32b(party_ptr->num);
    wr_hturn(&party_ptr->created);
}


void wr_parties(int unused)
{
    int i;

    /* Note the parties */
    wr_u16b(MAX_PARTIES);

    /* Dump the parties */
    for (i = 0; i < MAX_PARTIES; i++) wr_party(&parties[i]);
}


/*
 * Write the information about a house
 */
static void wr_house(house_type *house)
{
    wr_byte(house->x_1);
    wr_byte(house->y_1);
    wr_byte(house->x_2);
    wr_byte(house->y_2);
    wr_byte(house->door_y);
    wr_byte(house->door_x);
    wr_s32b(house->depth);
    wr_s32b(house->price);
    wr_s32b(house->ownerid);
    wr_string(house->ownername);
    wr_byte(house->color);
}


void wr_houses(int unused)
{
    int i;

    /* Note the number of houses */
    wr_u16b(num_houses);

    /* Dump the houses */
    for (i = 0; i < num_houses; i++) wr_house(&houses[i]);
}


/*
 * Write the information about an arena
 */
static void wr_arena(arena_type *arena)
{
    wr_byte(arena->x_1);
    wr_byte(arena->y_1);
    wr_byte(arena->x_2);
    wr_byte(arena->y_2);
    wr_s32b(arena->depth);
}


void wr_arenas(int unused)
{
    int i;

    /* Note the number of arenas */
    wr_u16b(num_arenas);

    /* Dump the arenas */
    for (i = 0; i < num_arenas; i++) wr_arena(&arenas[i]);
}


static void wr_wild(int n)
{
    wilderness_type *w_ptr = &wild_info[0 - n];

    wr_byte(w_ptr->generated);
}


void wr_wilderness(int unused)
{
    int i;

    /* Note the size of the wilderness... change this to num_wild ? */
    wr_u32b(MAX_WILD);

    /* Dump the wilderness */
    for (i = 1; i <= MAX_WILD; i++) wr_wild(i);
}


/*
 * Write the player name hash table.
 */
void wr_player_names(int unused)
{
    int *id_list = NULL;
    u32b num;
    size_t i;
    hash_entry *ptr;

    /* Get the list of player ID's */
    num = player_id_list(&id_list, 0L);

    /* Store the number of entries */
    wr_u32b(num);

    /* Store each entry */
    for (i = 0; i < num; i++)
    {
        /* Search for the entry */
        ptr = lookup_player(id_list[i]);

        /* Store the ID */
        wr_s32b(ptr->id);

        /* Store the account ID */
        wr_u32b(ptr->account);

        /* Store the player name */
        wr_string(ptr->name);

        /* Store the time of death */
        wr_hturn(&ptr->death_turn);
    }

    /* Free the memory in the list */
    mem_free(id_list);
}
