/*
 * File: save.c
 * Purpose: Individual saving functions
 *
 * Copyright (c) 1997 Ben Harrison
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
 * Write a description of the character
 */
void wr_description(void *data)
{
    struct player *p = (struct player *)data;
    char buf[MSG_LEN];

    if (p->is_dead)
        strnfmt(buf, sizeof(buf), "%s, dead (%s)", p->full_name, p->death_info.died_from);
    else
    {
        strnfmt(buf, sizeof(buf), "%s, L%d %s %s, at DL%d", p->full_name, p->lev, p->race->name,
            p->clazz->name, p->wpos.depth);
    }

    wr_string(buf);
}


static void wr_tval_sval(byte tval, byte sval)
{
#ifdef SAVE_AS_STRINGS
    wr_string(tval_find_name(tval));
    if (sval)
    {
        char name[MSG_LEN];

        obj_desc_name_format(name, sizeof(name), NULL, lookup_kind(tval, sval)->name, 0, false);
        wr_string(name);
    }
    else
        wr_string("");
#else
    wr_byte(tval);
    wr_byte(sval);
#endif
}


static void wr_artifact(struct artifact *art)
{
#ifdef SAVE_AS_STRINGS
    if (!art) wr_string("");
    else if (art == (struct artifact *)1) wr_string("1");
    else if ((art->aidx >= (u32b)z_info->a_max) && (art->aidx < (u32b)z_info->a_max + 9))
        wr_string(format("%d", art->aidx));
    else wr_string(art->name);
#else
    if (!art) wr_byte(0);
    else if (art == (struct artifact *)1) wr_byte(EGO_ART_KNOWN);
    else wr_byte(art->aidx + 1);
#endif
}


static void wr_ego(struct ego_item *ego)
{
#ifdef SAVE_AS_STRINGS
    if (!ego) wr_string("");
    else if (ego == (struct ego_item *)1) wr_string("1");
    else wr_string(ego->name);
#else
    if (!ego) wr_byte(0);
    else if (ego == (struct ego_item *)1) wr_byte(EGO_ART_KNOWN);
    else wr_byte(ego->eidx + 1);
#endif
}


static void wr_activation(struct activation *act)
{
    if (act) wr_u16b(act->index + 1);
    else wr_u16b(0);
}


/*
 * Write an "item" record
 */
static void wr_item(struct object *obj)
{
    size_t i;

    wr_byte(ITEM_VERSION);

    /* Location */
    wr_byte(obj->iy);
    wr_byte(obj->ix);
    wr_s16b(obj->wpos.wy);
    wr_s16b(obj->wpos.wx);
    wr_s16b(obj->wpos.depth);

    wr_tval_sval(obj->tval, obj->sval);
    wr_s32b(obj->pval);

    wr_byte(obj->number);
    wr_s16b(obj->weight);

    wr_s32b(obj->randart_seed);
    wr_artifact(obj->artifact);
    wr_ego(obj->ego);

    if (obj->effect)
    {
        if (obj->effect != (struct effect *)1)
            wr_byte(2);
        else
            wr_byte(1);
    }
    else
        wr_byte(0);

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
    if (obj->origin_race) wr_string(obj->origin_race->name);
    else wr_string("");

    wr_byte(obj->notice);

    for (i = 0; i < OF_SIZE; i++)
        wr_byte(obj->flags[i]);

    for (i = 0; i < OBJ_MOD_MAX; i++)
        wr_s32b(obj->modifiers[i]);

    /* Save brands */
    wr_byte(obj->brands? 1: 0);
    for (i = 0; obj->brands && (i < (size_t)z_info->brand_max); i++)
        wr_byte(obj->brands[i]? 1: 0);

    /* Save slays */
    wr_byte(obj->slays? 1: 0);
    for (i = 0; obj->slays && (i < (size_t)z_info->slay_max); i++)
        wr_byte(obj->slays[i]? 1: 0);

    /* Save curses */
    wr_byte(obj->curses? 1: 0);
    for (i = 0; obj->curses && (i < (size_t)z_info->curse_max); i++)
    {
        int j;

        wr_s16b(obj->curses[i].power);
        wr_s16b(obj->curses[i].timeout);
        wr_s16b(obj->curses[i].to_a);
        wr_s16b(obj->curses[i].to_h);
        wr_s16b(obj->curses[i].to_d);
        for (j = 0; j < OBJ_MOD_MAX; j++)
            wr_s16b(obj->curses[i].modifiers[j]);
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
    wr_activation(obj->activation);
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
    wr_byte(obj->ignore_protect);
    wr_byte(obj->ordered);
    wr_s16b(obj->decay);
    wr_byte(obj->bypass_aware);
}


void wr_monster_memory(void *data)
{
    struct player *p = (struct player *)data;
    int r;

    /* Dump the monster lore */
    wr_u16b(z_info->r_max);
    wr_byte(RF_SIZE);
    wr_byte(RSF_SIZE);
    wr_byte(z_info->mon_blows_max);
    for (r = 0; r < z_info->r_max; r++)
    {
        int i;
        struct monster_race *race = &r_info[r];
        struct monster_lore* lore = (p? get_lore(p, race): &race->lore);

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
    wr_byte(z_info->brand_max);
    wr_byte(z_info->slay_max);
    wr_byte(z_info->curse_max);

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
    for (i = 0; i < z_info->e_max; i++)
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
    wr_s16b(p->death_info.wpos.wy);
    wr_s16b(p->death_info.wpos.wx);
    wr_s16b(p->death_info.wpos.depth);
    wr_string(p->death_info.died_from);
    wr_s32b((s32b)p->death_info.time);
    wr_string(p->death_info.ctime);

    for (i = 0; i < N_HIST_LINES; i++) wr_string(p->history[i]);

    wr_byte(p->hitdie);
    wr_s16b(p->expfact);

    wr_s16b(p->age);
    wr_s16b(p->ht);
    wr_s16b(p->wt);

    /* Dump the stats (maximum and current and birth and swap-mapping) */
    for (i = 0; i < STAT_MAX; ++i) wr_s16b(p->stat_max[i]);
    for (i = 0; i < STAT_MAX; ++i) wr_s16b(p->stat_cur[i]);
    for (i = 0; i < STAT_MAX; ++i) wr_s16b(p->stat_map[i]);
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
    wr_byte(p->stealthy);

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

    for (i = ITYPE_NONE; i < ITYPE_MAX; i++) wr_byte(p->opts.ignore_lvl[i]);
}


static void wr_race(struct monster_race *race)
{
#ifdef SAVE_AS_STRINGS
    wr_string(race? race->name: "");
#else
    wr_u16b(race? race->ridx: 0);
#endif
}


void wr_player_misc(void *data)
{
    struct player *p = (struct player *)data;
    struct quest *quest = &p->quest;
    size_t i;

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
    wr_byte(OPT(p, birth_force_descend));
    wr_byte(OPT(p, birth_no_recall));
    wr_byte(OPT(p, birth_no_artifacts));
    wr_byte(OPT(p, birth_feelings));
    wr_byte(OPT(p, birth_no_selling));
    wr_byte(OPT(p, birth_start_kit));
    wr_byte(OPT(p, birth_no_stores));
    wr_byte(OPT(p, birth_no_ghost));
    wr_byte(OPT(p, birth_fruit_bat));
    wr_race(quest->race);
    wr_s16b(quest->cur_num);
    wr_s16b(quest->max_num);
    wr_s16b(quest->timer);
    wr_byte(p->party);
    wr_u16b(p->retire_timer);
    wr_s16b(p->tim_mimic_what);
    wr_race(p->poly_race);
    wr_s16b(p->k_idx);

    if (p->is_dead) return;

    /* Property knowledge */

    /* Flags */
    for (i = 0; i < OF_SIZE; i++)
        wr_byte(p->obj_k->flags[i]);

    /* Modifiers */
    for (i = 0; i < OBJ_MOD_MAX; i++)
        wr_s32b(p->obj_k->modifiers[i]);

    /* Elements */
    for (i = 0; i < ELEM_MAX; i++)
    {
        wr_s16b(p->obj_k->el_info[i].res_level);
        wr_byte(p->obj_k->el_info[i].flags);
    }

    /* Brands */
    for (i = 0; i < (size_t)z_info->brand_max; i++)
        wr_byte(p->obj_k->brands[i]? 1: 0);

    /* Slays */
    for (i = 0; i < (size_t)z_info->slay_max; i++)
        wr_byte(p->obj_k->slays[i]? 1: 0);

    /* Curses */
    for (i = 0; i < (size_t)z_info->curse_max; i++)
        wr_s16b(p->obj_k->curses[i].power);

    /* Combat data */
    wr_s16b(p->obj_k->ac);
    wr_s16b(p->obj_k->to_a);
    wr_s16b(p->obj_k->to_h);
    wr_s16b(p->obj_k->to_d);
    wr_byte(p->obj_k->dd);
    wr_byte(p->obj_k->ds);
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


static void wr_store(struct store *store)
{
    struct object *obj;

    /* Save the current owner */
    wr_byte(store->owner->oidx);

    /* Save the stock size */
    wr_s16b(store->stock_num);

    /* Save the stock */
    for (obj = store->stock; obj; obj = obj->next)
    {
        wr_item(obj);

        /* Dump known object */
        wr_item(obj->known);
    }
}


void wr_stores(void *unused)
{
    int i;

    /* Note the stores */
    wr_u16b(MAX_STORES);

    /* Dump the stores */
    for (i = 0; i < MAX_STORES; i++)
    {
        struct store *store = &stores[i];

        wr_store(store);
    }

    /* Note the store orders */
    wr_u16b(STORE_ORDERS);

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

    if (p->is_dead) return;

    /* Dungeon specific info follows */
    wr_s16b(p->wpos.wy);
    wr_s16b(p->wpos.wx);
    wr_s16b(p->wpos.depth);
    wr_s16b(p->py);
    wr_s16b(p->px);
    wr_u16b(p->cave->height);
    wr_u16b(p->cave->width);

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
                /* Extract the important cave->squares[y][x].info flags */
                tmp8u = p->cave->squares[y][x].info[i];

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
 * Write the current dungeon terrain features and info flags (level)
 */
void wr_level(void *data)
{
    int y, x;
    size_t i;
    byte tmp8u;
    byte count;
    byte prev_char;
    struct chunk *c = chunk_get((struct worldpos *)data);

    /* Dungeon specific info follows */

    /* Coordinates */
    wr_s16b(c->wpos.wy);
    wr_s16b(c->wpos.wx);
    wr_s16b(c->wpos.depth);

    /* Dungeon size */
    wr_u16b(c->height);
    wr_u16b(c->width);

    /* Player count + turn of creation */
    wr_s16b(chunk_get_player_count(&c->wpos));
    wr_hturn(&c->generated);

    /* Write connector info */
    wr_loc(c->join->up);
    wr_loc(c->join->down);
    wr_loc(c->join->rand);

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
                /* Extract the important cave->squares[y][x].info flags */
                tmp8u = c->squares[y][x].info[i];

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
    int i, x, y;
    u32b tmp32u = 0;

    wr_byte(SQUARE_SIZE);

    /* Get the number of levels to dump */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                /* Make sure the level has been allocated */
                if (c && level_keep_allocated(c)) tmp32u++;
            }
        }
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the levels players are actually on - and special levels */
    /* Note that this saves the player count */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c && level_keep_allocated(c))
                    wr_level((void *)&c->wpos);
            }
        }
    }

    /*** Compact ***/

    /* Compact the monsters */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c) compact_monsters(c, 0);
            }
        }
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
    int i, x, y;
    u32b tmp32u = 0;

    /* Get the number of levels to dump */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                /* Make sure the level has been allocated */
                if (c && level_keep_allocated(c)) tmp32u++;
            }
        }
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the objects */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c && level_keep_allocated(c))
                {
                    /* Write the coordinates */
                    wr_s16b(c->wpos.wy);
                    wr_s16b(c->wpos.wx);
                    wr_s16b(c->wpos.depth);

                    wr_objects_aux(c);
                }
            }
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

    wr_race(mon->race);
    wr_byte(mon->fy);
    wr_byte(mon->fx);
    wr_s16b(mon->wpos.wy);
    wr_s16b(mon->wpos.wx);
    wr_s16b(mon->wpos.depth);
    wr_s32b(mon->hp);
    wr_s32b(mon->maxhp);
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
    wr_byte(mon->camouflage);
    wr_s16b(mon->mimicked_k_idx);
    wr_byte(mon->feat);

    /* New level-related data */
    wr_s16b(mon->ac);
    for (j = 0; j < z_info->mon_blows_max; j++)
    {
        if (mon->blow[j].method)
        {
            wr_byte(blow_method_index(mon->blow[j].method->name) + 1);
            wr_byte(blow_effect_index(mon->blow[j].effect->name));
            wr_byte(mon->blow[j].dice.dice);
            wr_byte(mon->blow[j].dice.sides);
        }
        else
            wr_byte(0);
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
    int i, x, y;
    u32b tmp32u = 0;

    /* Get the number of levels to dump */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                /* Make sure the level has been allocated */
                if (c && level_keep_allocated(c)) tmp32u++;
            }
        }
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the monsters */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c && level_keep_allocated(c))
                {
                    /* Write the coordinates */
                    wr_s16b(c->wpos.wy);
                    wr_s16b(c->wpos.wx);
                    wr_s16b(c->wpos.depth);

                    wr_monsters_aux(c);
                }
            }
        }
    }
}


static void wr_trap_kind(struct trap_kind *kind)
{
#ifdef SAVE_AS_STRINGS
    wr_string(kind? kind->desc: "");
#else
    wr_byte(kind? kind->tidx: 0);
#endif
}


/*
 * Write a trap record
 */
static void wr_trap(struct trap *trap)
{
    size_t i;

    wr_trap_kind(trap->kind);
    wr_byte(trap->fy);
    wr_byte(trap->fx);
    wr_byte(trap->power);
    wr_byte(trap->timeout);

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


static void wr_level_traps(struct chunk *c)
{
    int x, y;
    struct trap *dummy;

    /* Write the coordinates */
    wr_s16b(c->wpos.wy);
    wr_s16b(c->wpos.wx);
    wr_s16b(c->wpos.depth);

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
    int i, x, y;
    u32b tmp32u = 0;

    wr_byte(TRF_SIZE);

    /* Get the number of levels to dump */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                /* Make sure the level has been allocated */
                if (c && level_keep_allocated(c)) tmp32u++;
            }
        }
    }

    /* Write the number of levels */
    wr_u32b(tmp32u);

    /* Write the traps */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                struct chunk *c = w_ptr->chunk_list[i];

                if (c && level_keep_allocated(c)) wr_level_traps(c);
            }
        }
    }
}


void wr_history(void *data)
{
    struct player *p = (struct player *)data;
    int i, j;

    wr_byte(HIST_SIZE);

    /* Write character event history */
    wr_s16b(p->hist.next);
    for (i = 0; i < p->hist.next; i++)
    {
        for (j = 0; j < HIST_SIZE; j++)
            wr_byte(p->hist.entries[i].type[j]);
        wr_hturn(&p->hist.entries[i].turn);
        wr_s16b(p->hist.entries[i].dlev);
        wr_s16b(p->hist.entries[i].clev);
        wr_artifact(p->hist.entries[i].art);
        wr_string(p->hist.entries[i].name);
        wr_string(p->hist.entries[i].event);
    }
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
    wr_string(p->race->name);
    wr_string(p->clazz->name);
    wr_byte(p->psex);
}


void wr_wild_map(void *data)
{
    struct player *p = (struct player *)data;
    int x, y;

    /* Write the wilderness map */
    wr_u16b(radius_wild);
    for (y = radius_wild; y >= 0 - radius_wild; y--)
        for (x = 0 - radius_wild; x <= radius_wild; x++)
            wr_byte(p->wild_map[radius_wild - y][radius_wild + x]);
}


void wr_home(void *data)
{
    struct player *p = (struct player *)data;

    wr_store(p->home);
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
static void wr_house(struct house_type *house)
{
    wr_byte(house->x_1);
    wr_byte(house->y_1);
    wr_byte(house->x_2);
    wr_byte(house->y_2);
    wr_byte(house->door_y);
    wr_byte(house->door_x);
    wr_s16b(house->wpos.wy);
    wr_s16b(house->wpos.wx);
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
static void wr_arena(struct arena_type *arena)
{
    wr_byte(arena->x_1);
    wr_byte(arena->y_1);
    wr_byte(arena->x_2);
    wr_byte(arena->y_2);
    wr_s16b(arena->wpos.wy);
    wr_s16b(arena->wpos.wx);
    wr_s16b(arena->wpos.depth);
}


void wr_arenas(void *unused)
{
    int i;

    /* Note the number of arenas */
    wr_u16b(num_arenas);

    /* Dump the arenas */
    for (i = 0; i < num_arenas; i++) wr_arena(&arenas[i]);
}


void wr_wilderness(void *unused)
{
    int x, y;

    /* Note the size of the wilderness */
    wr_u16b(radius_wild);

    /* Dump the wilderness */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            wr_byte((byte)w_ptr->generated);
        }
    }
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
