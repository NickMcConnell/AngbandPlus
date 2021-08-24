/*
 * File: save.c
 * Purpose: Individual saving functions
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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
 * Write a description of the character
 */
void wr_description(void *data)
{
    struct player *p = (struct player *)data;
    char buf[MSG_LEN];

    if (p->is_dead)
    {
        strnfmt(buf, sizeof(buf), "%s, dead (%s)", p->other.full_name,
            p->death_info.died_from);
    }
    else
    {
        strnfmt(buf, sizeof(buf), "%s, L%d %s %s, at DL%d", p->other.full_name, p->lev,
            p->race->name, p->clazz->name, p->depth);
    }

    wr_string(buf);
}


/*
 * Write an "item" record
 */
static void wr_item(struct object *obj)
{
    size_t i;
    struct brand *b;
    struct slay *s;

    wr_byte(ITEM_VERSION);

    /* Location */
    wr_byte(obj->iy);
    wr_byte(obj->ix);
    wr_s16b(obj->depth);

    wr_byte(obj->tval);
    wr_byte(obj->sval);
    wr_s32b(obj->pval);

    wr_byte(obj->number);
    wr_s16b(obj->weight);

    if (obj->artifact)
    {
        if (obj->artifact != (struct artifact *)1)
            wr_byte(obj->artifact->aidx);
        else
            wr_byte(EGO_ART_KNOWN);
    }
    else
        wr_byte(0);

    if (obj->ego)
    {
        if (obj->ego != (struct ego_item *)1)
            wr_byte(obj->ego->eidx);
        else
            wr_byte(EGO_ART_KNOWN);
    }
    else
        wr_byte(0);

    if (obj->effect)
    {
        if (obj->effect != (struct effect *)1)
            wr_byte(2);
        else
            wr_byte(1);
    }
    else
        wr_byte(0);

    wr_s32b(obj->randart_seed);

    wr_s16b(obj->timeout);

    wr_s16b(obj->to_h);
    wr_s16b(obj->to_d);
    wr_s16b(obj->to_a);
    wr_s16b(obj->ac);
    wr_byte(obj->dd);
    wr_byte(obj->ds);

    /* Origin */
    wr_byte(obj->origin);
    wr_s16b(obj->origin_depth);
    wr_u16b(obj->origin_xtra);

    wr_byte(obj->notice);

    for (i = 0; i < OF_SIZE; i++)
        wr_byte(obj->flags[i]);

    for (i = 0; i < OBJ_MOD_MAX; i++)
        wr_s32b(obj->modifiers[i]);

    /* Save brands */
    wr_byte(obj->brands? 1: 0);
    for (b = obj->brands; b; b = b->next)
    {
        wr_string(b->name);
        wr_s16b(b->element);
        wr_s16b(b->multiplier);
        wr_byte(b->next? 1: 0);
    }

    /* Save slays */
    wr_byte(obj->slays? 1: 0);
    for (s = obj->slays; s; s = s->next)
    {
        wr_string(s->name);
        wr_s16b(s->race_flag);
        wr_s16b(s->multiplier);
        wr_byte(s->next? 1: 0);
    }

    for (i = 0; i < ELEM_MAX; i++)
    {
        wr_s16b(obj->el_info[i].res_level);
        wr_byte(obj->el_info[i].flags);
    }

    /* Held by monster index */
    wr_s16b(obj->held_m_idx);

    wr_s16b(obj->mimicking_m_idx);

    /* Activation */
    if (obj->activation)
        wr_s16b(obj->activation->index);
    else
        wr_s16b(-1);
    wr_s16b(obj->time.base);
    wr_s16b(obj->time.dice);
    wr_s16b(obj->time.sides);

    /* Save the inscription (if any) */
    if (obj->note)
        wr_string(quark_str(obj->note));
    else
        wr_string("");

    /* PWMAngband */
    wr_s32b(obj->creator);
    wr_s32b(obj->owner);
    wr_byte(obj->allow_ignore);
    wr_byte(obj->ordered);
    wr_s16b(obj->decay);
    wr_byte(obj->bypass_aware);
}


void wr_monster_memory(void *data)
{
    struct player *p = (struct player *)data;
    int race;

    /* Dump the monster lore */
    wr_u16b(z_info->r_max);
    wr_byte(RF_SIZE);
    wr_byte(RSF_SIZE);
    wr_byte(z_info->mon_blows_max);
    for (race = 0; race < z_info->r_max; race++)
    {
        int i;
        struct monster_race *r = &r_info[race];
        struct monster_lore* lore = (p? get_lore(p, r): &r->lore);

        /* Count sights/deaths/kills */
        wr_byte(lore->spawned);
        wr_byte(lore->seen);
        wr_byte(lore->pseen);
        wr_s16b(lore->pdeaths);
        wr_s16b(lore->tdeaths);
        wr_s16b(lore->pkills);
        wr_s16b(lore->tkills);

        /* Count wakes and ignores */
        wr_byte(lore->wake);
        wr_byte(lore->ignore);

        /* Count spells */
        wr_byte(lore->cast_innate);
        wr_byte(lore->cast_spell);

        /* Count blows of each type */
        for (i = 0; i < z_info->mon_blows_max; i++) wr_byte(lore->blows[i]);

        /* Memorize flags */
        for (i = 0; i < RF_SIZE; i++) wr_byte(lore->flags[i]);
        for (i = 0; i < RSF_SIZE; i++) wr_byte(lore->spell_flags[i]);
    }
}


void wr_object_memory(void *data)
{
    struct player *p = (struct player *)data;
    int i, j;

    wr_byte(OF_SIZE);
    wr_byte(OBJ_MOD_MAX);
    wr_byte(ELEM_MAX);

    if (!p) return;

    /* Dump the object memory */
    wr_u16b(z_info->k_max);
    for (i = 0; i < z_info->k_max; i++)
    {
        byte flags = 0;

        /* Figure out and write the flags */
        if (p->obj_aware[i]) flags |= 0x01;
        if (p->obj_tried[i]) flags |= 0x02;
        if (p->kind_everseen[i]) flags |= 0x04;
        if (p->kind_ignore[i]) flags |= 0x08;
        wr_byte(flags);
    }

    /* Dump the ego memory */
    wr_u16b(z_info->e_max);
    wr_u16b(ITYPE_SIZE);
    for (i = 1; i < z_info->e_max; i++)
    {
        bitflag everseen = 0, itypes[ITYPE_SIZE];

        /* Figure out and write the everseen flag */
        everseen = p->ego_everseen[i];
        wr_byte(everseen);

        /* Figure out and write the ignore flags */
        itype_wipe(itypes);
        for (j = ITYPE_NONE; j < ITYPE_MAX; j++)
        {
            if (p->ego_ignore_types[i][j]) itype_on(itypes, j);
        }

        for (j = 0; j < ITYPE_SIZE; j++) wr_byte(itypes[j]);
    }
}


void wr_player_artifacts(void *data)
{
    struct player *p = (struct player *)data;
    int i;

    /* Write the artifact sold list */
    wr_u16b(z_info->a_max);
    for (i = 0; i < z_info->a_max; i++) wr_byte(p->art_info[i]);

    /* Write the randart info */
    for (i = 0; i < z_info->a_max + 9; i++)
    {
        wr_byte(p->randart_info[i]);
        wr_byte(p->randart_created[i]);
    }
}


void wr_artifacts(void *unused)
{
    int i;

    /* Hack -- dump the artifacts */
    wr_u16b(z_info->a_max);
    for (i = 0; i < z_info->a_max; i++)
    {
        struct artifact *art = &a_info[i];

        wr_byte(art->created);
        wr_byte(art->owned);
    }
}


/*
 * Write some "extra" info
 */
void wr_player(void *data)
{
    struct player *p = (struct player *)data;
    int i;

    wr_s32b(p->id);

    wr_string(p->died_from);
    wr_string(p->died_flavor);

    wr_string(p->death_info.title);
    wr_s16b(p->death_info.max_lev);
    wr_s16b(p->death_info.lev);
    wr_s32b(p->death_info.max_exp);
    wr_s32b(p->death_info.exp);
    wr_s32b(p->death_info.au);
    wr_s16b(p->death_info.max_depth);
    wr_s16b(p->death_info.depth);
    wr_string(p->death_info.died_from);
    wr_s32b((s32b)p->death_info.time);
    wr_string(p->death_info.ctime);

    for (i = 0; i < N_HIST_LINES; i++) wr_string(p->history[i]);

    wr_byte(p->hitdie);
    wr_s16b(p->expfact);

    wr_s16b(p->age);
    wr_s16b(p->ht);
    wr_s16b(p->wt);

    /* Dump the stats (maximum and current and birth) */
    for (i = 0; i < STAT_MAX; ++i) wr_s16b(p->stat_max[i]);
    for (i = 0; i < STAT_MAX; ++i) wr_s16b(p->stat_cur[i]);
    for (i = 0; i < STAT_MAX; ++i) wr_s16b(p->stat_birth[i]);

    /* PWMAngband: don't save body, use race body instead */

    wr_s32b(p->au);

    wr_s32b(p->max_exp);
    wr_s32b(p->exp);
    wr_u16b(p->exp_frac);
    wr_s16b(p->lev);

    wr_s16b(p->mhp);
    wr_s16b(p->chp);
    wr_u16b(p->chp_frac);

    wr_s16b(p->msp);
    wr_s16b(p->csp);
    wr_u16b(p->csp_frac);

    /* Max Player and Dungeon Levels */
    wr_s16b(p->max_lev);
    wr_s16b(p->max_depth);

    /* More info */
    wr_byte(p->unignoring);
    wr_s16b(p->deep_descent);

    wr_s16b(p->food);
    wr_s32b(p->energy);
    wr_s16b(p->word_recall);
    wr_byte(p->confusing);
    wr_byte(p->searching);

    /* Find the number of timed effects */
    wr_byte(TMD_MAX);

    /* Read all the effects, in a loop */
    for (i = 0; i < TMD_MAX; i++) wr_s16b(p->timed[i]);

    /* Write the brand info */
    wr_byte(p->brand.type);
    wr_byte(p->brand.blast);
    wr_s16b(p->brand.dam);
}


void wr_ignore(void *data)
{
    struct player *p = (struct player *)data;
    size_t i;

    /* Write number of ignore bytes */
    wr_byte(ITYPE_MAX);

    for (i = ITYPE_NONE; i < ITYPE_MAX; i++) wr_byte(p->other.ignore_lvl[i]);
}


void wr_player_misc(void *data)
{
    struct player *p = (struct player *)data;
    struct quest *quest = &p->quest;

    /* Special stuff */
    wr_u16b(p->total_winner);
    wr_byte(p->noscore);

    /* Write death */
    wr_byte(p->is_dead);

    /* Write feeling */
    wr_s16b(p->feeling);
    wr_u16b(p->cave->feeling_squares);

    /* PWMAngband */
    wr_hturn(&p->game_turn);
    wr_hturn(&p->player_turn);
    wr_hturn(&p->active_turn);
    wr_hturn(&turn);
    wr_s16b(p->ghost);
    wr_byte(p->lives);
    wr_byte(OPT_P(p, birth_force_descend));
    wr_byte(OPT_P(p, birth_no_recall));
    wr_byte(OPT_P(p, birth_no_artifacts));
    wr_byte(OPT_P(p, birth_feelings));
    wr_byte(OPT_P(p, birth_no_selling));
    wr_byte(OPT_P(p, birth_start_kit));
    wr_byte(OPT_P(p, birth_no_stores));
    wr_byte(OPT_P(p, birth_no_ghost));
    wr_byte(OPT_P(p, birth_fruit_bat));
    wr_s16b(quest->race? quest->race->ridx: 0);
    wr_s16b(quest->cur_num);
    wr_s16b(quest->max_num);
    wr_s16b(quest->timer);
    wr_byte(p->party);
    wr_u16b(p->retire_timer);
    wr_s16b(p->tim_mimic_what);
    wr_s16b(p->poly_race? p->poly_race->ridx: 0);
    wr_s16b(p->k_idx);
}


void wr_misc(void *unused)
{
    /* Write the "object seeds" */
    wr_u32b(seed_flavor);
    wr_u32b(seed_wild);

    /* Current turn */
    wr_hturn(&turn);

    /* PWMAngband */
    wr_s32b(player_id);
}


void wr_player_hp(void *data)
{
    struct player *p = (struct player *)data;
    int i;

    /* Dump the "player hp" entries */
    wr_u16b(PY_MAX_LEVEL);
    for (i = 0; i < PY_MAX_LEVEL; i++) wr_s16b(p->player_hp[i]);
}


void wr_player_spells(void *data)
{
    struct player *p = (struct player *)data;
    int i;

    /* Write spell data */
    wr_u16b(p->clazz->magic.total_spells);
    for (i = 0; i < p->clazz->magic.total_spells; i++) wr_byte(p->spell_flags[i]);

    /* Dump the ordered spells */
    for (i = 0; i < p->clazz->magic.total_spells; i++) wr_byte(p->spell_order[i]);

    /* Dump spell power */
    for (i = 0; i < p->clazz->magic.total_spells; i++) wr_byte(p->spell_power[i]);
}


static void wr_dummy_item(void)
{
    struct object *dummy = object_new();

    wr_item(dummy);
    object_delete(&dummy);
}


void wr_gear(void *data)
{
    struct player *p = (struct player *)data;
    struct object *obj;

    /* Write the gear */
    for (obj = p->gear; obj; obj = obj->next)
    {
        /* Write code for equipment or other gear */
        wr_byte(equipped_item_slot(p->body, obj));

        /* Dump object */
        wr_item(obj);

        /* Dump known object */
        wr_item(obj->known);
    }

    /* Write finished code */
    wr_byte(FINISHED_CODE);
}


void wr_stores(void *unused)
{
    int i;

    /* Note the stores */
    wr_u16b(MAX_STORES);

    /* Dump the stores */
    for (i = 0; i < MAX_STORES; i++)
    {
        const struct store *store = &stores[i];
        struct object *obj;

        /* Save the current owner */
        wr_byte(store->owner->oidx);

        /* Save the stock size */
        wr_s16b(store->stock_num);

        /* Save the stock */
        for (obj = store->stock; obj; obj = obj->next)
            wr_item(obj);
    }

    /* Dump the store orders */
    for (i = 0; i < STORE_ORDERS; i++) wr_string(store_orders[i]);
}


/*
 * Write the current dungeon terrain features and info flags (player)
 *
 * Note that the cost and when fields of cave->squares[y][x] are not saved
 */
void wr_player_dungeon(void *data)
{
    struct player *p = (struct player *)data;
    int y, x;
    size_t i;
    byte tmp8u;
    byte count;
    byte prev_char;
    bitflag cave_info[SQUARE_SIZE], important_flags[SQUARE_SIZE];

    if (p->is_dead) return;

    /* Dungeon specific info follows */
    wr_s16b(p->depth);
    wr_s16b(p->py);
    wr_s16b(p->px);
    wr_u16b(p->cave->height);
    wr_u16b(p->cave->width);

    /* PWMAngband */
    wr_s16b(p->world_y);
    wr_s16b(p->world_x);

    /*** Simple "Run-Length-Encoding" of cave ***/

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Run length encoding of cave->squares[y][x].feat */
    for (y = 0; y < p->cave->height; y++)
    {
        for (x = 0; x < p->cave->width; x++)
        {
            /* Extract a byte */
            tmp8u = p->cave->squares[y][x].feat;

            /* If the run is broken, or too full, flush it */
            if ((tmp8u != prev_char) || (count == UCHAR_MAX))
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

    sqinfo_wipe(important_flags);
    sqinfo_on(important_flags, SQUARE_DTRAP);

    /* Run length encoding of cave->squares[y][x].info */
    for (i = 0; i < SQUARE_SIZE; i++)
    {
        count = 0;
        prev_char = 0;

        /* Dump for each grid */
        for (y = 0; y < p->cave->height; y++)
        {
            for (x = 0; x < p->cave->width; x++)
            {
                sqinfo_wipe(cave_info);
                sqinfo_copy(cave_info, p->cave->squares[y][x].info);
                sqinfo_inter(cave_info, important_flags);

                /* Extract the important cave->squares[y][x].info flags */
                tmp8u = cave_info[i];

                /* If the run is broken, or too full, flush it */
                if ((tmp8u != prev_char) || (count == UCHAR_MAX))
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
}


/*
 * Write the current dungeon terrain features and info flags (depth)
 */
void wr_depth_dungeon(void *data)
{
    int depth = (int)data;
    int y, x;
    size_t i;
    byte tmp8u;
    byte count;
    byte prev_char;
    struct chunk *c = chunk_get(depth);
    bitflag cave_info[SQUARE_SIZE], important_flags[SQUARE_SIZE];

    /* Dungeon specific info follows */

    /* Depth */
    wr_s16b(depth);

    /* Dungeon size */
    wr_u16b(c->height);
    wr_u16b(c->width);

    /* Player count + turn of creation */
    wr_s16b(chunk_get_player_count(depth));
    wr_hturn(&c->generated);

    /* The staircase locations on this depth */
    /* Hack -- this information is currently not present for the wilderness levels. */
    if (depth >= 0)
    {
        wr_byte(c->level_up_y);
        wr_byte(c->level_up_x);
        wr_byte(c->level_down_y);
        wr_byte(c->level_down_x);
        wr_byte(c->level_rand_y);
        wr_byte(c->level_rand_x);
    }

    /*** Simple "Run-Length-Encoding" of cave ***/

    /* Note that this will induce two wasted bytes */
    count = 0;
    prev_char = 0;

    /* Run length encoding of cave->squares[y][x].feat */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            /* Extract a byte */
            tmp8u = c->squares[y][x].feat;

            /* If the run is broken, or too full, flush it */
            if ((tmp8u != prev_char) || (count == UCHAR_MAX))
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

    sqinfo_wipe(important_flags);
    sqinfo_on(important_flags, SQUARE_GLOW);
    sqinfo_on(important_flags, SQUARE_VAULT);
    sqinfo_on(important_flags, SQUARE_ROOM);
    sqinfo_on(important_flags, SQUARE_FEEL);
    sqinfo_on(important_flags, SQUARE_NO_TELEPORT);
    sqinfo_on(important_flags, SQUARE_TRAP);
    /*sqinfo_on(important_flags, SQUARE_INVIS);*/

    /* Run length encoding of cave->squares[y][x].info */
    for (i = 0; i < SQUARE_SIZE; i++)
    {
        count = 0;
        prev_char = 0;

        /* Dump for each grid */
        for (y = 0; y < c->height; y++)
        {
            for (x = 0; x < c->width; x++)
            {
                sqinfo_wipe(cave_info);
                sqinfo_copy(cave_info, c->squares[y][x].info);
                sqinfo_inter(cave_info, important_flags);

                /* Extract the important cave->squares[y][x].info flags */
                tmp8u = cave_info[i];

                /* If the run is broken, or too full, flush it */
                if ((tmp8u != prev_char) || (count == UCHAR_MAX))
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
}


/*
 * Write the current dungeon
 */
void wr_dungeon(void *unused)
{
    int i;
    u32b tmp32u;

    wr_byte(SQUARE_SIZE);

    /* Get the number of levels to dump */
    tmp32u = 0;
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        /* Make sure the level has been allocated */
        if (chunk_get(i) && level_keep_allocated(i)) tmp32u++;
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the levels players are actually on - and special levels */
    /* Note that this saves the player count */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        if (chunk_get(i) && level_keep_allocated(i))
            wr_depth_dungeon((void *)i);
    }

    /*** Compact ***/

    /* Compact the monsters */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        struct chunk *c = chunk_get(i);

        if (c) compact_monsters(c, 0);
    }
}


/*
 * Write the dungeon floor objects
 */
static void wr_objects_aux(struct chunk *c)
{
    int y, x;

    /* Write the objects */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            struct object *obj = c->squares[y][x].obj;

            while (obj)
            {
                wr_item(obj);

                /* Dump known object */
                wr_item(obj->known);

                obj = obj->next;
            }
        }
    }

    /* Write a dummy record as a marker */
    wr_dummy_item();
}


/*
 * Write the player objects
 */
void wr_player_objects(void *data)
{
    struct player *p = (struct player *)data;
    int y, x;

    if (p->is_dead) return;

    /* Write the objects */
    for (y = 0; y < p->cave->height; y++)
    {
        for (x = 0; x < p->cave->width; x++)
        {
            struct object *obj = p->cave->squares[y][x].obj;

            while (obj)
            {
                wr_item(obj);

                /* Dump known object */
                wr_item(obj->known);

                obj = obj->next;
            }
        }
    }

    /* Write a dummy record as a marker */
    wr_dummy_item();
}


void wr_objects(void *unused)
{
    int i;
    u32b tmp32u = 0;

    /* Get the number of levels to dump */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        /* Make sure the level has been allocated */
        if (chunk_get(i) && level_keep_allocated(i)) tmp32u++;
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the objects */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        struct chunk *c = chunk_get(i);

        if (c && level_keep_allocated(i))
        {
            /* Write the depth */
            wr_s16b(c->depth);

            wr_objects_aux(c);
        }
    }
}


/*
 * Write a monster record (including held or mimicked objects)
 */
static void wr_monster(const struct monster *mon)
{
    int j;
    struct object *obj = mon->held_obj;

    wr_s16b(mon->race->ridx);
    wr_byte(mon->fy);
    wr_byte(mon->fx);
    wr_s16b(mon->depth);
    wr_s16b(mon->hp);
    wr_s16b(mon->maxhp);
    wr_byte(mon->mspeed);
    wr_s32b(mon->energy);

    wr_byte(MON_TMD_MAX);
    for (j = 0; j < MON_TMD_MAX; j++)
        wr_s16b(mon->m_timed[j]);

    for (j = 0; j < OF_SIZE; j++)
        wr_byte(mon->known_pstate.flags[j]);

    for (j = 0; j < ELEM_MAX; j++)
        wr_s16b(mon->known_pstate.el_info[j].res_level);

    /* Mimic stuff */
    wr_byte(mon->unaware);
    wr_s16b(mon->mimicked_k_idx);
    wr_byte(mon->feat);

    /* New level-related data */
    wr_s16b(mon->ac);
    for (j = 0; j < z_info->mon_blows_max; j++)
    {
        wr_byte(mon->blow[j].method);
        wr_byte(mon->blow[j].effect);
        wr_byte(mon->blow[j].dice.dice);
        wr_byte(mon->blow[j].dice.sides);
    }
    wr_s16b(mon->level);

    wr_byte(mon->clone);
    wr_byte(mon->origin);

    /* Write mimicked object marker, if any */
    if (mon->mimicked_obj) wr_u16b(mon->midx);
    else wr_u16b(0);

    /* Write all held objects, followed by a dummy as a marker */
    while (obj)
    {
        wr_item(obj);

        /* Dump known object */
        wr_item(obj->known);

        obj = obj->next;
    }
    wr_dummy_item();
}


/*
 * Write the monster list
 */
static void wr_monsters_aux(struct chunk *c)
{
    int i;
    u16b limit = 1;

    /* Total monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        const struct monster *mon = cave_monster(c, i);

        if (mon->race) limit++;
    }
    wr_u16b(limit);

    /* Dump the monsters */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        const struct monster *mon = cave_monster(c, i);

        /* Paranoia */
        if (!mon->race) continue;

        wr_monster(mon);
    }
}


void wr_monsters(void *unused)
{
    int i;
    u32b tmp32u = 0;

    /* Get the number of levels to dump */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        /* Make sure the level has been allocated */
        if (chunk_get(i) && level_keep_allocated(i)) tmp32u++;
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the monsters */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        struct chunk *c = chunk_get(i);

        if (c && level_keep_allocated(i))
        {
            /* Write the depth */
            wr_s16b(c->depth);

            wr_monsters_aux(c);
        }
    }
}


/*
 * Write a trap record
 */
static void wr_trap(struct trap *trap)
{
    size_t i;

    wr_byte(trap->t_idx);
    wr_byte(trap->fy);
    wr_byte(trap->fx);
    wr_byte(trap->xtra);

    for (i = 0; i < TRF_SIZE; i++)
        wr_byte(trap->flags[i]);
}


void wr_player_traps(void *data)
{
    struct player *p = (struct player *)data;
    int x, y;
    struct trap *dummy;

    if (p->is_dead) return;

    /* Write the traps */
    for (y = 0; y < p->cave->height; y++)
    {
        for (x = 0; x < p->cave->width; x++)
        {
            struct trap *trap = p->cave->squares[y][x].trap;

            while (trap)
            {
                wr_trap(trap);
                trap = trap->next;
            }
        }
    }

    /* Write a dummy record as a marker */
    dummy = mem_zalloc(sizeof(*dummy));
    wr_trap(dummy);
    mem_free(dummy);
}


static void wr_depth_traps(struct chunk *c)
{
    int x, y;
    struct trap *dummy;

    /* Write the depth */
    wr_s16b(c->depth);

    /* Write the traps */
    for (y = 0; y < c->height; y++)
    {
        for (x = 0; x < c->width; x++)
        {
            struct trap *trap = c->squares[y][x].trap;

            while (trap)
            {
                wr_trap(trap);
                trap = trap->next;
            }
        }
    }

    /* Write a dummy record as a marker */
    dummy = mem_zalloc(sizeof(*dummy));
    wr_trap(dummy);
    mem_free(dummy);
}


void wr_traps(void *unused)
{
    int i;
    u32b tmp32u = 0;

    wr_byte(TRF_SIZE);

    /* Get the number of levels to dump */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        /* Make sure the level has been allocated */
        if (chunk_get(i) && level_keep_allocated(i)) tmp32u++;
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the traps */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        struct chunk *c = chunk_get(i);

        if (c && level_keep_allocated(i)) wr_depth_traps(c);
    }
}


void wr_history(void *data)
{
    struct player *p = (struct player *)data;
    int i, j;

    wr_byte(HIST_SIZE);

    /* Write character event history */
    wr_s16b(p->history_size);
    for (i = 0; i < p->history_size; i++)
    {
        for (j = 0; j < HIST_SIZE; j++)
            wr_byte(p->history_list[i].type[j]);
        wr_hturn(&p->history_list[i].turn);
        wr_s16b(p->history_list[i].dlev);
        wr_s16b(p->history_list[i].clev);
        wr_byte(p->history_list[i].a_idx);
        wr_string(p->history_list[i].name);
        wr_string(p->history_list[i].event);
    }
    wr_s16b(p->history_ctr);
}


/*** PWMAngband ***/


/*
 * Hack -- save basic player info
 */
void wr_header(void *data)
{
    struct player *p = (struct player *)data;

    wr_string(p->name);
    wr_string(p->pass);

    /* Race/Class/Gender/Spells */
    wr_byte(p->race->ridx);
    wr_byte(p->clazz->cidx);
    wr_byte(p->psex);
}


void wr_wild_map(void *data)
{
    struct player *p = (struct player *)data;
    int i;

    /* Write the wilderness map */
    wr_u16b(MAX_WILD / 8);
    for (i = 0; i < MAX_WILD / 8; i++) wr_byte(p->wild_map[i]);
}


static void wr_party(party_type *party_ptr)
{
    wr_string(party_ptr->name);
    wr_string(party_ptr->owner);
    wr_s32b(party_ptr->num);
    wr_hturn(&party_ptr->created);
}


void wr_parties(void *unused)
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
    wr_byte(house->state);
    wr_byte(house->free);
}


void wr_houses(void *unused)
{
    u16b i, count = (u16b)houses_count();

    /* Note the number of houses */
    wr_u16b(count);

    /* Dump the houses */
    for (i = 0; i < count; i++) wr_house(house_get(i));
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


void wr_arenas(void *unused)
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

    wr_byte((byte)w_ptr->generated);
}


void wr_wilderness(void *unused)
{
    int i;

    /* Note the size of the wilderness... change this to num_wild? */
    wr_u32b(MAX_WILD);

    /* Dump the wilderness */
    for (i = 1; i <= MAX_WILD; i++) wr_wild(i);
}


/*
 * Write the player name hash table.
 */
void wr_player_names(void *unused)
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
