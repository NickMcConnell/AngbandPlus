/*
 * File: load.c
 * Purpose: Savefile loading functions
 *
 * Copyright (c) 1997 Ben Harrison, and others
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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
 * Dungeon constants
 */
static byte square_size = 0;


/*
 * Player constants
 */
static byte hist_size = 0;


/*
 * Object constants
 */
static byte obj_mod_max = OBJ_MOD_MAX;
static byte of_size = OF_SIZE;
static byte elem_max = ELEM_MAX;
static byte brand_max;
static byte slay_max;
static byte curse_max;


/*
 * Monster constants
 */
static byte mflag_size = 0;


/*
 * Trap constants
 */
static byte trf_size = 0;


/*
 * Shorthand function pointer for rd_item version
 */
typedef struct object *(*rd_item_t)(void);


static void rd_tval_sval(u16b *tval, u16b *sval)
{
#ifdef SAVE_AS_STRINGS
    char buf[MSG_LEN];

    rd_string(buf, sizeof(buf));
    if (buf[0]) *tval = tval_find_idx(buf);
    rd_string(buf, sizeof(buf));
    if (buf[0]) *sval = lookup_sval(*tval, buf);
#else
    rd_u16b(tval);
    rd_u16b(sval);
#endif
}


static struct artifact *rd_artifact(bool randart)
{
#ifdef SAVE_AS_STRINGS
    char buf[NORMAL_WID];
    int aux;

    rd_string(buf, sizeof(buf));
    aux = atoi(buf);
    if (aux == 1) return (struct artifact *)1;
    if (randart && (aux >= z_info->a_max) && (aux < z_info->a_max + 9)) return &a_info[aux];
    if (buf[0]) return lookup_artifact_name(buf);
#else
    byte art_idx;

    rd_byte(&art_idx);
    if (art_idx == EGO_ART_KNOWN) return (struct artifact *)1;
    if ((art_idx > 0) && (art_idx <= z_info->a_max)) return &a_info[art_idx - 1];
    if (randart && (art_idx > z_info->a_max) && (art_idx <= z_info->a_max + 9))
        return &a_info[art_idx - 1];
#endif
    return NULL;
}


static struct ego_item *rd_ego(struct object_kind *kind)
{
#ifdef SAVE_AS_STRINGS
    char buf[NORMAL_WID];

    rd_string(buf, sizeof(buf));
    if (streq(buf, "1")) return (struct ego_item *)1;
    if (buf[0]) return lookup_ego_item(buf, kind);
#else
    byte ego_idx;

    rd_byte(&ego_idx);
    if (ego_idx == EGO_ART_KNOWN) return (struct ego_item *)1;
    if ((ego_idx > 0) && (ego_idx <= z_info->e_max)) return &e_info[ego_idx - 1];
#endif
    return NULL;
}


static struct activation *rd_activation(void)
{
    u16b tmp16u;

    rd_u16b(&tmp16u);
    if ((tmp16u > 0) && (tmp16u <= z_info->act_max)) return &activations[tmp16u - 1];
    return NULL;
}


/*
 * Read an object.
 */
static struct object *rd_item(void)
{
    struct object *obj = object_new();
    byte tmp8u, tmp8x, tmp8y;
    s16b tmp16s, tmp16x, tmp16y;
    byte effect;
    size_t i;
    char buf[128];

    /* Skip version */
    strip_bytes(1);

    /* Location */
    rd_byte(&tmp8y);
    rd_byte(&tmp8x);
    loc_init(&obj->grid, tmp8x, tmp8y);
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&obj->wpos.grid, tmp16x, tmp16y);
    rd_s16b(&obj->wpos.depth);

    /* Type/Subtype */
    rd_tval_sval(&obj->tval, &obj->sval);
    rd_s32b(&obj->pval);

    /* Lookup item kind (can be NULL) */
    obj->kind = lookup_kind_silent(obj->tval, obj->sval);

    rd_byte(&obj->number);
    rd_s16b(&obj->weight);

    rd_s32b(&obj->randart_seed);
    obj->artifact = rd_artifact(obj->randart_seed? true: false);
    obj->ego = rd_ego(obj->kind);
    rd_byte(&effect);

    rd_s16b(&obj->timeout);

    rd_s16b(&obj->to_h);
    rd_s16b(&obj->to_d);
    rd_s16b(&obj->to_a);

    rd_s16b(&obj->ac);

    rd_byte(&obj->dd);
    rd_byte(&obj->ds);

    rd_byte(&obj->origin);
    rd_s16b(&obj->origin_depth);
    rd_string(buf, sizeof(buf));
    if (buf[0]) obj->origin_race = lookup_monster(buf);

    rd_byte(&obj->notice);

    for (i = 0; i < (size_t)of_size; i++)
        rd_byte(&obj->flags[i]);

    for (i = 0; i < (size_t)obj_mod_max; i++)
        rd_s32b(&obj->modifiers[i]);

    /* Read brands */
    rd_byte(&tmp8u);
    if (tmp8u)
    {
        obj->brands = mem_zalloc(z_info->brand_max * sizeof(bool));

        for (i = 0; i < (size_t)brand_max; i++)
        {
            rd_byte(&tmp8u);
            obj->brands[i] = (tmp8u? true: false);
        }
    }

    /* Read slays */
    rd_byte(&tmp8u);
    if (tmp8u)
    {
        obj->slays = mem_zalloc(z_info->slay_max * sizeof(bool));

        for (i = 0; i < (size_t)slay_max; i++)
        {
            rd_byte(&tmp8u);
            obj->slays[i] = (tmp8u? true: false);
        }
    }

    /* Read curses */
    rd_byte(&tmp8u);
    if (tmp8u)
    {
        obj->curses = mem_zalloc(z_info->curse_max * sizeof(struct curse_data));

        for (i = 0; i < (size_t)curse_max; i++)
        {
            int j;

            rd_s16b(&tmp16s);
            obj->curses[i].power = tmp16s;
            rd_s16b(&tmp16s);
            obj->curses[i].timeout = tmp16s;
            rd_s16b(&tmp16s);
            obj->curses[i].to_a = tmp16s;
            rd_s16b(&tmp16s);
            obj->curses[i].to_h = tmp16s;
            rd_s16b(&tmp16s);
            obj->curses[i].to_d = tmp16s;
            for (j = 0; j < obj_mod_max; j++)
            {
                rd_s16b(&tmp16s);
                obj->curses[i].modifiers[j] = tmp16s;
            }
        }
    }

    for (i = 0; i < (size_t)elem_max; i++)
    {
        rd_s16b(&obj->el_info[i].res_level);
        rd_byte(&obj->el_info[i].flags);
    }

    /* Monster holding object */
    rd_s16b(&obj->held_m_idx);

    rd_s16b(&obj->mimicking_m_idx);

    /* Activation */
    obj->activation = rd_activation();
    rd_s16b(&tmp16s);
    obj->time.base = tmp16s;
    rd_s16b(&tmp16s);
    obj->time.dice = tmp16s;
    rd_s16b(&tmp16s);
    obj->time.sides = tmp16s;

    /* Save the inscription */
    rd_string(buf, sizeof(buf));
    if (buf[0]) obj->note = quark_add(buf);

    /* PWMAngband */
    rd_s32b(&obj->creator);
    rd_s32b(&obj->owner);
    rd_byte(&obj->level_req);
    rd_byte(&obj->ignore_protect);
    rd_byte(&obj->ordered);
    rd_s16b(&obj->decay);
    rd_byte(&obj->bypass_aware);

    /* Dummy item */
    if (!obj->tval && !obj->sval)
    {
        object_delete(&obj);
        return NULL;
    }

    /* Check we have a kind */
    if (!obj->kind)
    {
        plog_fmt("No object: %d:%d (%s)", obj->tval, obj->sval, tval_find_name(obj->tval));
        object_delete(&obj);
        return NULL;
    }

    /* Set effect */
    switch (effect)
    {
        case 1: obj->effect = (struct effect *)1; break;
        case 2: obj->effect = obj->kind->effect; break;
        default: break;
    }

    /* Success */
    return obj;
}


/*
 * Read monster memory.
 */
int rd_monster_memory(struct player *p)
{
    int r;
    u16b tmp16u;
    byte monster_blow_max;
    byte rf_size;
    byte rsf_size;

    /* Monster Memory */
    rd_u16b(&tmp16u);
    if (tmp16u > z_info->r_max)
    {
        plog_fmt("Too many (%u) monster races!", tmp16u);
        return (-1);
    }

    /* Monster flags */
    rd_byte(&rf_size);
    if (rf_size > RF_SIZE)
    {
        plog_fmt("Too many (%u) monster flags!", rf_size);
        return (-1);
    }

    /* Monster spell flags */
    rd_byte(&rsf_size);
    if (rsf_size > RSF_SIZE)
    {
        plog_fmt("Too many (%u) monster spell flags!", rsf_size);
        return (-1);
    }

    /* Monster blows */
    rd_byte(&monster_blow_max);
    if (monster_blow_max != z_info->mon_blows_max)
    {
        plog_fmt("Invalid number of monster blows (%u)!", monster_blow_max);
        return (-1);
    }

    /* Read the available records */
    for (r = 0; r < tmp16u; r++)
    {
        size_t i;
        struct monster_race *race = &r_info[r];
        struct monster_lore* lore = (p? get_lore(p, race): &race->lore);

        /* Count sights/deaths/kills */
        rd_byte(&lore->spawned);
        rd_byte(&lore->seen);
        rd_byte(&lore->pseen);
        rd_s16b(&lore->pdeaths);
        rd_s16b(&lore->tdeaths);
        rd_s16b(&lore->pkills);
        rd_s16b(&lore->thefts);
        rd_s16b(&lore->tkills);

        /* Count wakes and ignores */
        rd_byte(&lore->wake);
        rd_byte(&lore->ignore);

        /* Count spells */
        rd_byte(&lore->cast_innate);
        rd_byte(&lore->cast_spell);

        /* Count blows of each type */
        for (i = 0; i < (size_t)z_info->mon_blows_max; i++) rd_byte(&lore->blows[i]);

        /* Memorize flags */
        for (i = 0; i < (size_t)rf_size; i++) rd_byte(&lore->flags[i]);
        for (i = 0; i < (size_t)rsf_size; i++) rd_byte(&lore->spell_flags[i]);

        /* Repair the spell lore flags */
        rsf_inter(lore->spell_flags, race->spell_flags);

        /* Update any derived values */
        if (p) lore_update(race, lore);
    }

    /* Success */
    return (0);
}


/*
 * Read object memory.
 */
int rd_object_memory(struct player *p)
{
    int i, j;
    u16b tmp16u;
    u16b itype_size;

    /* Object flags */
    rd_byte(&of_size);
    if (of_size > OF_SIZE)
    {
        plog_fmt("Too many (%u) object flags!", of_size);
        return (-1);
    }

    /* Object modifiers */
    rd_byte(&obj_mod_max);
    if (obj_mod_max > OBJ_MOD_MAX)
    {
        plog_fmt("Too many (%u) object modifiers allowed!", obj_mod_max);
        return (-1);
    }

    /* Elements */
    rd_byte(&elem_max);
    if (elem_max > ELEM_MAX)
    {
        plog_fmt("Too many (%u) elements allowed!", elem_max);
        return (-1);
    }

    /* Brands */
    rd_byte(&brand_max);
    if (brand_max > z_info->brand_max)
    {
        plog_fmt("Too many (%u) brands allowed!", brand_max);
        return (-1);
    }

    /* Slays */
    rd_byte(&slay_max);
    if (slay_max > z_info->slay_max)
    {
        plog_fmt("Too many (%u) slays allowed!", slay_max);
        return (-1);
    }

    /* Curses */
    rd_byte(&curse_max);
    if (curse_max > z_info->curse_max)
    {
        plog_fmt("Too many (%u) curses allowed!", curse_max);
        return (-1);
    }

    if (!p) return (0);

    /* Object Memory */
    rd_u16b(&tmp16u);
    if (tmp16u > z_info->k_max)
    {
        plog_fmt("Too many (%u) object kinds!", tmp16u);
        return (-1);
    }

    /* Read the object memory */
    for (i = 0; i < tmp16u; i++)
    {
        byte flags;

        /* Read and extract the flags */
        rd_byte(&flags);
        p->obj_aware[i] = ((flags & 0x01)? true: false);
        p->obj_tried[i] = ((flags & 0x02)? true: false);
        p->kind_everseen[i] = ((flags & 0x04)? 1: 0);
        p->kind_ignore[i] = ((flags & 0x08)? 1: 0);
    }

    /* Ego Memory */
    rd_u16b(&tmp16u);
    rd_u16b(&itype_size);
    if (tmp16u > z_info->e_max)
    {
        plog_fmt("Too many (%u) ego items!", tmp16u);
        return (-1);
    }
    if (itype_size > ITYPE_SIZE)
    {
        plog_fmt("Too many (%u) ignore bytes!", itype_size);
        return (-1);
    }

    /* Read the ego memory */
    for (i = 0; i < tmp16u; i++)
    {
        bitflag flags, itypes[ITYPE_SIZE];

        /* Read and extract the everseen flag */
        rd_byte(&flags);
        p->ego_everseen[i] = flags;

        /* Read and extract the ignore flags */
        for (j = 0; j < itype_size; j++) rd_byte(&itypes[j]);

        /* If number of ignore types has changed, don't set anything */
        if (itype_size == ITYPE_SIZE)
        {
            for (j = ITYPE_NONE; j < ITYPE_MAX; j++)
            {
                if (itype_has(itypes, j))
                    p->ego_ignore_types[i][j] = 1;
            }
        }
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
    s16b tmp16s, tmp16x, tmp16y;

    rd_s32b(&p->id);

    /* Verify player ID */
    if (!p->id) p->id = player_id++;
    else
    {
        hash_entry *ptr = lookup_player_by_name(p->name);

        /* If character exists, ids must match */
        if (ptr)
        {
            if (ptr->id != p->id) p->id = player_id++;
        }

        /* If character doesn't exist, don't steal the id of someone else */
        else if (lookup_player(p->id)) p->id = player_id++;
    }

    rd_string(p->died_from, NORMAL_WID);
    rd_string(p->died_flavor, 160);

    rd_string(p->death_info.title, NORMAL_WID);
    rd_s16b(&p->death_info.max_lev);
    rd_s16b(&p->death_info.lev);
    rd_s32b(&p->death_info.max_exp);
    rd_s32b(&p->death_info.exp);
    rd_s32b(&p->death_info.au);
    rd_s16b(&p->death_info.max_depth);
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&p->death_info.wpos.grid, tmp16x, tmp16y);
    rd_s16b(&p->death_info.wpos.depth);
    rd_string(p->death_info.died_from, NORMAL_WID);
    rd_s32b((s32b*)&p->death_info.time);
    rd_string(p->death_info.ctime, NORMAL_WID);

    for (i = 0; i < N_HIST_LINES; i++) rd_string(p->history[i], N_HIST_WRAP);

    /* Special Race/Class info */
    rd_byte(&p->hitdie);
    rd_s16b(&p->expfact);

    /* Age/Height/Weight */
    rd_s16b(&p->age);
    rd_s16b(&p->ht);
    rd_s16b(&p->wt);

    /* Read the stat info */
    for (i = 0; i < STAT_MAX; ++i) rd_s16b(&p->stat_max[i]);
    for (i = 0; i < STAT_MAX; ++i) rd_s16b(&p->stat_cur[i]);
    for (i = 0; i < STAT_MAX; ++i) rd_s16b(&p->stat_map[i]);
    for (i = 0; i < STAT_MAX; ++i) rd_s16b(&p->stat_birth[i]);

    /* PWMAngband: don't load body, use race body instead */
    player_embody(p);

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

    /* Hack -- repair maximum player level */
    if (p->max_lev < p->lev) p->max_lev = p->lev;

    /* Hack -- repair maximum dungeon level */
    if (p->max_depth < 0) p->max_depth = 0;

    wpos_init(&p->recall_wpos, &p->wpos.grid, p->max_depth);

    /* More info */
    rd_byte(&p->unignoring);
    rd_s16b(&p->deep_descent);

    /* Read the flags */
    rd_s32b(&p->energy);
    rd_s16b(&p->word_recall);
    rd_byte(&p->stealthy);

    /* Find the number of timed effects */
    rd_byte(&num);

    if (num <= TMD_MAX)
    {
        /* Read all the effects */
        for (i = 0; i < num; i++)
            rd_s16b(&p->timed[i]);

        /* Initialize any entries not read */
        if (num < TMD_MAX)
            memset(p->timed + num, 0, (TMD_MAX - num) * sizeof(s16b));
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

    /* Read the brand info */
    rd_byte(&p->brand.type);
    rd_bool(&p->brand.blast);
    rd_s16b(&tmp16s);
    p->brand.dam = tmp16s;

    /* Success */
    return (0);
}


/*
 * Read ignore info
 */
int rd_ignore(struct player *p)
{
    size_t i;
    byte tmp8u;

    /* Read how many ignore bytes we have */
    rd_byte(&tmp8u);

    if (tmp8u <= ITYPE_MAX)
    {
        /* Read all the entries */
        for (i = 0; i < (size_t)tmp8u; i++)
            rd_byte(&p->opts.ignore_lvl[i]);

        /* Initialize any entries not read */
        for (i = tmp8u; i < ITYPE_MAX; i++)
            p->opts.ignore_lvl[i] = IGNORE_BAD;
    }
    else
    {
        /* Probably in trouble anyway */
        for (i = ITYPE_NONE; i < ITYPE_MAX; i++)
            rd_byte(&p->opts.ignore_lvl[i]);

        /* Discard unused entries */
        strip_bytes(tmp8u - ITYPE_MAX);
        plog("Discarded unsupported ignore entries.");
    }

    /* Success */
    return (0);
}


static struct monster_race *rd_race(void)
{
#ifdef SAVE_AS_STRINGS
    char race_name[NORMAL_WID];

    rd_string(race_name, sizeof(race_name));
    if (strcmp(race_name, "none")) return lookup_monster(race_name);
#else
    u16b race;

    rd_u16b(&race);
    if (race >= z_info->r_max) race = 0;
    if (race) return &r_info[race];
#endif
    return NULL;
}


int rd_player_misc(struct player *p)
{
    struct quest *quest = &p->quest;
    size_t i;
    byte tmp8u;
    s16b tmp16s;

    /* Special stuff */
    rd_u16b(&p->total_winner);
    rd_byte(&p->noscore);

    /* Read "death" */
    rd_bool(&p->is_dead);

    /* Hack -- reset cause of death */
    if (!p->is_dead && (p->chp >= 0))
        my_strcpy(p->died_from, "(alive and well)", sizeof(p->died_from));

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
    rd_bool(&OPT(p, birth_force_descend));
    rd_bool(&OPT(p, birth_no_recall));
    rd_bool(&OPT(p, birth_no_artifacts));
    rd_bool(&OPT(p, birth_feelings));
    rd_bool(&OPT(p, birth_no_selling));
    rd_bool(&OPT(p, birth_start_kit));
    rd_bool(&OPT(p, birth_no_stores));
    rd_bool(&OPT(p, birth_no_ghost));
    rd_bool(&OPT(p, birth_fruit_bat));
    quest->race = rd_race();
    rd_s16b(&quest->cur_num);
    rd_s16b(&quest->max_num);
    rd_s16b(&quest->timer);
    rd_byte(&p->party);
    rd_u16b(&p->retire_timer);
    rd_s16b(&p->tim_mimic_what);
    p->poly_race = rd_race();
    rd_s16b(&p->k_idx);

    if (p->is_dead) return 0;

    /* Property knowledge */

    /* Flags */
    for (i = 0; i < (size_t)of_size; i++)
        rd_byte(&p->obj_k->flags[i]);

    /* Modifiers */
    for (i = 0; i < (size_t)obj_mod_max; i++)
        rd_s32b(&p->obj_k->modifiers[i]);

    /* Elements */
    for (i = 0; i < (size_t)elem_max; i++)
    {
        rd_s16b(&p->obj_k->el_info[i].res_level);
        rd_byte(&p->obj_k->el_info[i].flags);
    }

    /* Brands */
    for (i = 0; i < (size_t)brand_max; i++)
    {
        rd_byte(&tmp8u);
        p->obj_k->brands[i] = (tmp8u? true: false);
    }

    /* Slays */
    for (i = 0; i < (size_t)slay_max; i++)
    {
        rd_byte(&tmp8u);
        p->obj_k->slays[i] = (tmp8u? true: false);
    }

    /* Curses */
    for (i = 0; i < (size_t)curse_max; i++)
    {
        rd_s16b(&tmp16s);
        p->obj_k->curses[i].power = tmp16s;
    }

    /* Combat data */
    rd_s16b(&p->obj_k->ac);
    rd_s16b(&p->obj_k->to_a);
    rd_s16b(&p->obj_k->to_h);
    rd_s16b(&p->obj_k->to_d);
    rd_byte(&p->obj_k->dd);
    rd_byte(&p->obj_k->ds);

    /* Success */
    return (0);
}


int rd_misc(struct player *unused)
{
    /* Read the flavors seed */
    rd_u32b(&seed_flavor);
    flavor_init();

    /* Seed for wilderness layout */
    rd_u32b(&seed_wild);

    /* Current turn */
    rd_hturn(&turn);

    /* Success */
    return (0);
}


int rd_player_artifacts(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Read the character artifact info */
    rd_u16b(&tmp16u);
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
    if (tmp16u > z_info->a_max)
    {
        plog_fmt("Too many (%u) artifacts!", tmp16u);
        return (-1);
    }

    /* Read the artifact flags */
    for (i = 0; i < tmp16u; i++)
    {
        struct artifact *art = &a_info[i];

        rd_byte(&art->created);
        rd_byte(&art->owned);
    }

    /* Success */
    return (0);
}


int rd_player_hp(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Read the player_hp array */
    rd_u16b(&tmp16u);
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


/*
 * Read the player spells
 */
int rd_player_spells(struct player *p)
{
    int i;
    u16b tmp16u;

    /* Read the number of spells */
    rd_u16b(&tmp16u);
    if (tmp16u > p->clazz->magic.total_spells)
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

    /* Read spell cooldown */
    for (i = 0; i < tmp16u; i++) rd_byte(&p->spell_cooldown[i]);

    /* Success */
    return (0);
}


/*
 * Read the player gear
 */
static int rd_gear_aux(struct player *p, rd_item_t rd_item_version)
{
    byte code;
    struct object *last_gear_obj = NULL;

    /* Get the first item code */
    rd_byte(&code);

    /* Read until done */
    while (code != FINISHED_CODE)
    {
        /* Read the item */
        struct object *obj = (*rd_item_version)();

        if (!obj)
        {
            plog("Error reading item");
            return (-1);
        }

        /* Repair artifacts */
        if (obj->artifact)
        {
            if (true_artifact_p(obj))
            {
                if (!obj->artifact->created) obj->artifact->created = 1;
                if (obj->owner) obj->artifact->owned = 1;
            }
            else
            {
                p->randart_created[obj->artifact->aidx] = 1;
                if (!obj->creator) obj->creator = p->id;
            }
        }

        /* Read the known item */
        obj->known = (*rd_item_version)();

        /* Append the object */
        obj->prev = last_gear_obj;
        if (last_gear_obj) last_gear_obj->next = obj;
        else p->gear = obj;
        last_gear_obj = obj;

        /* Add the weight */
        p->upkeep->total_weight += (obj->number * obj->weight);

        /* If it's equipment, wield it */
        if (code < p->body.count)
        {
            obj->oidx = z_info->pack_size + code;
            p->body.slots[code].obj = obj;
            p->upkeep->equip_cnt++;
        }

        /* Get the next item code */
        rd_byte(&code);
    }

    calc_inventory(p);

    /* Success */
    return (0);
}


/*
 * Read the player gear - wrapper function
 */
int rd_gear(struct player *p)
{
    return rd_gear_aux(p, rd_item);
}


typedef struct object *(*store_carry_t)(struct player *p, struct store *store, struct object *obj);


static int rd_store(struct player *p, struct store **store, store_carry_t store_carry_fn,
    rd_item_t rd_item_version)
{
    byte own;
    s16b num;

    /* Read the basic info */
    rd_byte(&own);
    rd_s16b(&num);

    (*store)->owner = store_ownerbyidx(*store, own);

    /* Read the items */
    for (; num; num--)
    {
        /* Read the item */
        struct object *obj = (*rd_item_version)();

        if (!obj)
        {
            plog("Error reading item");
            return (-1);
        }

        /* Read the known item */
        obj->known = (*rd_item_version)();

        /* Accept any valid items */
        if (((*store)->stock_num < z_info->store_inven_max) && obj->kind)
            (*store_carry_fn)(p, *store, obj);
        else
            object_delete(&obj);
    }

    return 0;
}


/*
 * Read store contents
 */
static int rd_stores_aux(rd_item_t rd_item_version)
{
    int i;
    u16b tmp16u;

    /* Read the stores */
    rd_u16b(&tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        struct store *store = &stores[i];
        int res = rd_store(NULL, &store, store_carry, rd_item_version);

        if (res) return res;

    }

    /* Read the store orders */
    rd_u16b(&tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        rd_string(store_orders[i].order, NORMAL_WID);
        rd_hturn(&store_orders[i].turn);
    }

    /* Success */
    return 0;
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
    int i, n;
    u16b height, width;
    byte count;
    byte tmp8u;
    u16b tmp16u;
    struct loc grid;
    s16b tmp16x, tmp16y;

    /* Only if the player's alive */
    if (p->is_dead) return 0;

    /* Header info */
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&p->wpos.grid, tmp16x, tmp16y);
    rd_s16b(&p->wpos.depth);
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&p->grid, tmp16x, tmp16y);
    rd_u16b(&height);
    rd_u16b(&width);

    /* Allocate the memory */
    player_cave_new(p, height, width);

    /* Run length decoding of cave->squares[y][x].feat */
    for (grid.x = grid.y = 0; grid.y < height; )
    {
        /* Grab RLE info */
        rd_byte(&count);
        rd_u16b(&tmp16u);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            /* Extract "feat" */
            square_p(p, &grid)->feat = tmp16u;

            /* Advance/Wrap */
            if (++grid.x >= width)
            {
                /* Wrap */
                grid.x = 0;

                /* Advance/Wrap */
                if (++grid.y >= height) break;
            }
        }
    }

    /* Run length decoding of cave->squares[y][x].info */
    for (n = 0; n < square_size; n++)
    {
        for (grid.x = grid.y = 0; grid.y < height; )
        {
            /* Grab RLE info */
            rd_byte(&count);
            rd_byte(&tmp8u);

            /* Apply the RLE info */
            for (i = count; i > 0; i--)
            {
                /* Extract "info" */
                square_p(p, &grid)->info[n] = tmp8u;

                /* Advance/Wrap */
                if (++grid.x >= width)
                {
                    /* Wrap */
                    grid.x = 0;

                    /* Advance/Wrap */
                    if (++grid.y >= height) break;
                }
            }
        }
    }

    return 0;
}


/*
 * Read the dungeon (level)
 */
int rd_level(struct player *unused)
{
    int i, n;
    s16b tmp16s, tmp16x, tmp16y;
    u16b height, width;
    byte count;
    byte tmp8u;
    u16b tmp16u;
    hturn generated;
    struct worldpos wpos;
    struct chunk *c;
    struct loc grid;

    /* Header info */
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&wpos.grid, tmp16x, tmp16y);
    rd_s16b(&wpos.depth);
    rd_u16b(&height);
    rd_u16b(&width);

    /* PWMAngband */

    /* Ignore illegal dungeons */

    /* Player count and turn of creation */
    rd_s16b(&tmp16s);
    rd_hturn(&generated);

    /*
     * Allocate the memory for the dungeon if it has not already
     * been allocated - which it might have been if we are loading
     * a special static level file
     */
    c = chunk_get(&wpos);
    if (!c)
    {
        c = cave_new(height, width);
        memcpy(&c->wpos, &wpos, sizeof(struct worldpos));
        chunk_list_add(c);
    }
    chunk_set_player_count(&c->wpos, tmp16s);

    /* Read connector info */
    rd_loc(&c->join->up);
    rd_loc(&c->join->down);
    rd_loc(&c->join->rand);

    /* Run length decoding of cave->squares[y][x].feat */
    for (grid.x = grid.y = 0; grid.y < c->height; )
    {
        /* Grab RLE info */
        rd_byte(&count);
        rd_u16b(&tmp16u);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            /* Extract "feat" */
            square(c, &grid)->feat = tmp16u;

            /* Advance/Wrap */
            if (++grid.x >= c->width)
            {
                /* Wrap */
                grid.x = 0;

                /* Advance/Wrap */
                if (++grid.y >= c->height) break;
            }
        }
    }

    /* Run length decoding of cave->squares[y][x].info */
    for (n = 0; n < square_size; n++)
    {
        for (grid.x = grid.y = 0; grid.y < c->height; )
        {
            /* Grab RLE info */
            rd_byte(&count);
            rd_byte(&tmp8u);

            /* Apply the RLE info */
            for (i = count; i > 0; i--)
            {
                /* Extract "info" */
                square(c, &grid)->info[n] = tmp8u;

                /* Advance/Wrap */
                if (++grid.x >= c->width)
                {
                    /* Wrap */
                    grid.x = 0;

                    /* Advance/Wrap */
                    if (++grid.y >= c->height) break;
                }
            }
        }
    }

    /* The dungeon is ready */
    ht_copy(&c->generated, &generated);

    return 0;
}


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now the current dimensions of the
 * cave global variable.
 *
 * Note that dungeon objects, including objects held by monsters, are
 * placed directly into the dungeon, using "object_copy()", which will
 * copy "iy", "ix", and "held_m_idx", leaving "next" blank for
 * objects held by monsters, since it is not saved in the savefile.
 *
 * After loading the monsters, the objects being held by monsters are
 * linked directly into those monsters.
 */
int rd_dungeon(struct player *unused)
{
    u32b i, tmp32u;

    /* Header info */
    rd_byte(&square_size);

    /* Read the number of levels to be loaded */
    rd_u32b(&tmp32u);

    /* Read the levels */
    for (i = 0; i < tmp32u; i++)
    {
        if (rd_level(NULL)) return (-1);
    }

    /* Success */
    return (0);
}


/*
 * Read the floor object list
 */
static int rd_objects_aux(rd_item_t rd_item_version, struct chunk *c)
{
    /* Read the dungeon items until one isn't returned */
    while (true)
    {
        struct object *obj = (*rd_item_version)();

        if (!obj) break;

        /* Read the known item */
        obj->known = (*rd_item_version)();

        /* The dungeon is ready: place object in dungeon */
        if (c && !ht_zero(&c->generated))
        {
            if (!floor_add(c, &obj->grid, obj))
            {
                plog_fmt("Cannot place object at row %d, column %d!", obj->grid.y, obj->grid.x);
                object_delete(&obj);
                if (!beta_version()) return -1;
            }
        }
        else
            object_delete(&obj);
    }

    return 0;
}


/*
 * Read the player object list
 */
static int rd_player_objects_aux(struct player *p, rd_item_t rd_item_version)
{
    /* Read the player items until one isn't returned */
    while (true)
    {
        struct object *obj = (*rd_item_version)();

        if (!obj) break;

        /* Read the known item */
        obj->known = (*rd_item_version)();

        /* Place object in player object list */
        if (player_square_in_bounds_fully(p, &obj->grid))
            pile_insert_end(&square_p(p, &obj->grid)->obj, obj);
    }

    return 0;
}


/*
 * Read the player object list - wrapper function
 */
int rd_player_objects(struct player *p)
{
    /* Only if the player's alive */
    if (p->is_dead) return 0;

    return rd_player_objects_aux(p, rd_item);
}


/*
 * Read the objects - wrapper function
 */
int rd_objects(struct player *unused)
{
    u32b num, tmp32u;
    struct worldpos wpos;
    s16b tmp16x, tmp16y;

    /* Read the number of levels to be loaded */
    rd_u32b(&tmp32u);

    /* Read the objects */
    for (num = 0; num < tmp32u; num++)
    {
        /* Read the coordinates */
        rd_s16b(&tmp16y);
        rd_s16b(&tmp16x);
        loc_init(&wpos.grid, tmp16x, tmp16y);
        rd_s16b(&wpos.depth);

        if (rd_objects_aux(rd_item, chunk_get(&wpos))) return (-1);
    }

    /* Success */
    return (0);
}


/*
 * Read a monster
 */
static bool rd_monster_aux(struct chunk *c, struct monster *mon, rd_item_t rd_item_version)
{
    byte tmp8u;
    u16b tmp16u;
    size_t j;
    bool remove = false;
    s16b tmp16x, tmp16y;

    /* Read the monster race */
    rd_u16b(&tmp16u);
    mon->midx = tmp16u;
    mon->race = rd_race();
    if (!mon->race)
    {
        plog("Monster race no longer exists!");
        return false;
    }
    mon->original_race = rd_race();

    /* Read the other information */
    rd_byte(&tmp8u);
    mon->grid.y = tmp8u;
    rd_byte(&tmp8u);
    mon->grid.x = tmp8u;
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&mon->wpos.grid, tmp16x, tmp16y);
    rd_s16b(&mon->wpos.depth);
    rd_s32b(&mon->hp);
    rd_s32b(&mon->maxhp);
    rd_byte(&mon->mspeed);
    rd_s32b(&mon->energy);

    /* Hack -- save previous monster location */
    loc_copy(&mon->old_grid, &mon->grid);

    rd_byte(&tmp8u);
    for (j = 0; j < (size_t)tmp8u; j++)
        rd_s16b(&mon->m_timed[j]);

    /* Read and extract the flag */
    for (j = 0; j < (size_t)mflag_size; j++)
        rd_byte(&mon->mflag[j]);

    for (j = 0; j < (size_t)of_size; j++)
        rd_byte(&mon->known_pstate.flags[j]);

    for (j = 0; j < (size_t)elem_max; j++)
        rd_s16b(&mon->known_pstate.el_info[j].res_level);

    /* Mimic stuff */
    rd_s16b(&mon->mimicked_k_idx);
    rd_u16b(&mon->feat);

    rd_s16b(&mon->ac);
    mon->blow = mem_zalloc(z_info->mon_blows_max * sizeof(struct monster_blow));
    for (j = 0; j < (size_t)z_info->mon_blows_max; j++)
    {
        rd_byte(&tmp8u);
        if (tmp8u)
        {
            if (tmp8u <= z_info->blow_methods_max)
                mon->blow[j].method = &blow_methods[tmp8u - 1];
            rd_byte(&tmp8u);
            if (tmp8u < z_info->blow_effects_max)
                mon->blow[j].effect = &blow_effects[tmp8u];
            rd_byte(&tmp8u);
            mon->blow[j].dice.dice = tmp8u;
            rd_byte(&tmp8u);
            mon->blow[j].dice.sides = tmp8u;
        }
    }
    rd_s16b(&mon->level);

    rd_byte(&mon->clone);
    rd_byte(&mon->origin);

    /* Read mimicked object marker */
    rd_u16b(&tmp16u);

    /* The dungeon has to be ready */
    if (tmp16u && c && !ht_zero(&c->generated))
    {
        /* Find and set the mimicked object */
        struct object *square_obj = square_object(c, &mon->grid);

        /* Try and find the mimicked object; if we fail, delete the monster */
        while (square_obj)
        {
            if (square_obj->mimicking_m_idx == tmp16u) break;
            square_obj = square_obj->next;
        }
        if (square_obj)
            mon->mimicked_obj = square_obj;
        else
            remove = true;
    }

    /* Read all the held objects (order is unimportant) */
    while (true)
    {
        struct object *obj = (*rd_item_version)();

        if (!obj) break;

        /* Read the known item */
        obj->known = (*rd_item_version)();

        pile_insert(&mon->held_obj, obj);
    }

    /* Read group info */
    rd_u16b(&tmp16u);
    mon->group_info[PRIMARY_GROUP].index = tmp16u;
    rd_byte(&tmp8u);
    mon->group_info[PRIMARY_GROUP].role = tmp8u;
    rd_u16b(&tmp16u);
    mon->group_info[SUMMON_GROUP].index = tmp16u;
    rd_byte(&tmp8u);
    mon->group_info[SUMMON_GROUP].role = tmp8u;

    /* Now delete the monster if necessary */
    if (remove) delete_monster(c, &mon->grid);

    return true;
}


/*
 * Read a monster - wrapper function
 */
static bool rd_monster(struct chunk *c, struct monster *mon)
{
    return rd_monster_aux(c, mon, rd_item);
}


/*
 * Read monsters
 */
static int rd_monsters_aux(struct chunk *c)
{
    int i;
    u16b limit;

    /* Read the monster count */
    rd_u16b(&limit);

    /* Hack -- verify */
    if (limit > z_info->level_monster_max)
    {
        plog_fmt("Too many (%d) monster entries!", limit);
        return (-1);
    }

    /* Read the monsters */
    for (i = 1; i < limit; i++)
    {
        struct monster *mon;
        struct monster monster_body;

        /* Get local monster */
        mon = &monster_body;
        memset(mon, 0, sizeof(*mon));

        /* Read the monster */
        if (!rd_monster(c, mon))
        {
            plog_fmt("Cannot read monster %d", i);
            return -1;
        }

        /* The dungeon is ready: place monster in dungeon */
        if (c && !ht_zero(&c->generated))
        {
            if (!place_monster(NULL, c, mon, 0))
            {
                plog_fmt("Cannot place monster %d", i);
                return -1;
            }
        }
    }

    return 0;
}


/*
 * Read the monster list - wrapper function
 */
int rd_monsters(struct player *unused)
{
    u32b num, tmp32u;
    struct worldpos wpos;
    s16b tmp16x, tmp16y;

    /* Monster temporary flags */
    rd_byte(&mflag_size);

    /* Incompatible save files */
    if (mflag_size > MFLAG_SIZE)
    {
        plog_fmt("Too many (%u) monster temporary flags!", mflag_size);
        return (-1);
    }

    /* Read the number of levels to be loaded */
    rd_u32b(&tmp32u);

    /* Read the monsters */
    for (num = 0; num < tmp32u; num++)
    {
        /* Read the coordinates */
        rd_s16b(&tmp16y);
        rd_s16b(&tmp16x);
        loc_init(&wpos.grid, tmp16x, tmp16y);
        rd_s16b(&wpos.depth);

        if (rd_monsters_aux(chunk_get(&wpos))) return (-1);
    }

    /* Success */
    return (0);
}


static struct trap_kind *rd_trap_kind(void)
{
#ifdef SAVE_AS_STRINGS
    char buf[NORMAL_WID];

    rd_string(buf, sizeof(buf));
    if (buf[0]) return lookup_trap(buf);
#else
    byte tidx;

    rd_byte(&tidx);
    if (tidx >= z_info->trap_max) tidx = 0;
    if (tidx) return &trap_info[tidx];
#endif
    return NULL;
}


/*
 * Read a trap record
 */
static void rd_trap(struct trap *trap)
{
    int i;
    byte tmp8x, tmp8y;

    trap->kind = rd_trap_kind();
    rd_byte(&tmp8y);
    rd_byte(&tmp8x);
    loc_init(&trap->grid, tmp8x, tmp8y);
    rd_byte(&trap->power);
    rd_byte(&trap->timeout);

    for (i = 0; i < trf_size; i++)
        rd_byte(&trap->flags[i]);
}


int rd_player_traps(struct player *p)
{
    struct trap *trap;

    /* Only if the player's alive */
    if (p->is_dead) return 0;

    /* Read traps until one has no location */
    while (true)
    {
        trap = mem_zalloc(sizeof(*trap));
        rd_trap(trap);
        if (loc_is_zero(&trap->grid)) break;

        /* Put the trap at the front of the grid trap list */
        if (player_square_in_bounds_fully(p, &trap->grid))
        {
            trap->next = square_p(p, &trap->grid)->trap;
            square_p(p, &trap->grid)->trap = trap;
        }
    }

    mem_free(trap);
    return 0;
}


static int rd_level_traps(void)
{
    struct trap *trap;
    struct worldpos wpos;
    struct chunk *c;
    s16b tmp16x, tmp16y;

    /* Read the coordinates */
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&wpos.grid, tmp16x, tmp16y);
    rd_s16b(&wpos.depth);

    c = chunk_get(&wpos);

    /* Paranoia: dungeon must be ready */
    if (!c || ht_zero(&c->generated))
    {
        plog_fmt("Dungeon level (%d) not ready!", wpos.depth);
        return (-1);
    }

    /* Read traps until one has no location */
    while (true)
    {
        trap = mem_zalloc(sizeof(*trap));
        rd_trap(trap);
        if (loc_is_zero(&trap->grid)) break;

        /* Put the trap at the front of the grid trap list */
        trap->next = square(c, &trap->grid)->trap;
        square_set_trap(c, &trap->grid, trap);

        /* Set decoy if appropriate */
        if (trap->kind == lookup_trap("decoy")) loc_copy(&c->decoy, &trap->grid);
    }

    mem_free(trap);
    return 0;
}


/*
 * Read the traps - wrapper function
 */
int rd_traps(struct player *unused)
{
    u32b i, tmp32u;

    rd_byte(&trf_size);

    /* Read the number of levels to be loaded */
    rd_u32b(&tmp32u);

    /* Read the traps */
    for (i = 0; i < tmp32u; i++)
    {
        if (rd_level_traps()) return (-1);
    }

    /* Success */
    return (0);
}


int rd_history(struct player *p)
{
    s16b tmp16s;
    int i, j;

    history_clear(p);

    /* History type flags */
    rd_byte(&hist_size);
    if (hist_size > HIST_SIZE)
    {
        plog_fmt("Too many (%u) history types!", hist_size);
        return (-1);
    }

    /* Read the character event history */
    rd_s16b(&tmp16s);
    for (i = 0; i < tmp16s; i++)
    {
        struct history_info entry;

        memset(&entry, 0, sizeof(entry));

        for (j = 0; j < hist_size; j++) rd_byte(&entry.type[j]);
        rd_hturn(&entry.turn);
        rd_s16b(&entry.dlev);
        rd_s16b(&entry.clev);
        entry.art = rd_artifact(true);
        rd_string(entry.name, sizeof(entry.name));
        rd_string(entry.event, sizeof(entry.event));

        history_add_full(p, &entry);
    }

    /* Success */
    return (0);
}


/*
 * For blocks that don't need loading anymore.
 */
int rd_null(struct player *unused)
{
    return 0;
}


/*** PWMAngband ***/


/*
 * Hack -- read basic player info
 */
int rd_header(struct player *p)
{
    char buf[NORMAL_WID];

    rd_string(p->name, NORMAL_WID);
    rd_string(p->pass, NORMAL_WID);

    /* Player race */
    rd_string(buf, sizeof(buf));
    p->race = lookup_player_race(buf);

    /* Verify player race */
    if (!p->race)
    {
        plog_fmt("Invalid player race (%s).", buf);
        return -1;
    }

    /* Player class */
    rd_string(buf, sizeof(buf));
    p->clazz = lookup_player_class(buf);

    /* Verify player class */
    if (!p->clazz)
    {
        plog_fmt("Invalid player class (%s).", buf);
        return -1;
    }

    /* Initialize the spells */
    player_spells_init(p);

    /* Player gender */
    rd_byte(&p->psex);
    p->sex = &sex_info[p->psex];

    /* Success */
    return 0;
}


int rd_wild_map(struct player *p)
{
    int x, y;
    u16b tmp16u;

    /* Get the map size */
    rd_u16b(&tmp16u);
    if (tmp16u != radius_wild)
    {
        plog_fmt("Incompatible map size: (%u).", tmp16u);
        return (-1);
    }

    /* Read in the map */
    for (y = radius_wild; y >= 0 - radius_wild; y--)
        for (x = 0 - radius_wild; x <= radius_wild; x++)
            rd_byte(&p->wild_map[radius_wild - y][radius_wild + x]);

    /* Success */
    return (0);
}


int rd_home(struct player *p)
{
    struct store *store = p->home;

    return rd_store(p, &store, home_carry, rd_item);
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
static int rd_house(void)
{
    int house;
    struct house_type h_local;
    byte tmpx, tmpy;
    s16b tmp16x, tmp16y;

    memset(&h_local, 0, sizeof(struct house_type));

    /* Read house info */
    rd_byte(&tmpx);
    rd_byte(&tmpy);
    loc_init(&h_local.grid_1, tmpx, tmpy);
    rd_byte(&tmpx);
    rd_byte(&tmpy);
    loc_init(&h_local.grid_2, tmpx, tmpy);
    rd_byte(&tmpy);
    rd_byte(&tmpx);
    loc_init(&h_local.door, tmpx, tmpy);
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&h_local.wpos.grid, tmp16x, tmp16y);
    rd_s32b(&h_local.price);
    rd_s32b(&h_local.ownerid);
    rd_string(h_local.ownername, NORMAL_WID);
    rd_byte(&h_local.color);
    rd_byte(&h_local.state);
    rd_byte(&h_local.free);

    /* Get an empty house slot */
    house = house_add((h_local.state >= HOUSE_EXTENDED)? true: false);
    if (house == -1) return (-1);

    /* Add a house to our houses list */
    house_set(house, &h_local);

    /* Success */
    return (0);
}


int rd_houses(struct player *unused)
{
    u16b i, count;

    /* Read house info */
    rd_u16b(&count);

    /* Read the available records */
    for (i = 0; i < count; i++)
    {
        if (rd_house() == -1)
        {
            plog("Too many custom house entries!");
            return (-1);
        }
    }

    /* Success */
    return (0);
}


/*
 * Read some arena info
 */
static void rd_arena(int n)
{
    struct arena_type *arena_ptr = &arenas[n];
    byte tmpx, tmpy;
    s16b tmp16x, tmp16y;

    rd_byte(&tmpx);
    rd_byte(&tmpy);
    loc_init(&arena_ptr->grid_1, tmpx, tmpy);
    rd_byte(&tmpx);
    rd_byte(&tmpy);
    loc_init(&arena_ptr->grid_2, tmpx, tmpy);
    rd_s16b(&tmp16y);
    rd_s16b(&tmp16x);
    loc_init(&arena_ptr->wpos.grid, tmp16x, tmp16y);
    rd_s16b(&arena_ptr->wpos.depth);
}


int rd_arenas(struct player *unused)
{
    int i;

    /* Read arena info */
    rd_u16b(&num_arenas);
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


int rd_wilderness(struct player *unused)
{
    struct loc grid;
    u16b tmp16u;

    /* Read wilderness info */
    rd_u16b(&tmp16u);
    if (tmp16u != radius_wild)
    {
        plog_fmt("Incompatible wilderness size: (%u).", tmp16u);
        return (-1);
    }

    /* Read the available records */
    for (grid.y = radius_wild; grid.y >= 0 - radius_wild; grid.y--)
    {
        for (grid.x = 0 - radius_wild; grid.x <= radius_wild; grid.x++)
        {
            byte tmp;
            struct wild_type *w_ptr = get_wt_info_at(&grid);

            /* Read wilderness info */
            rd_byte(&tmp);

            w_ptr->generated = (enum wild_gen)tmp;
        }
    }

    /* Success */
    return (0);
}


int rd_player_names(struct player *unused)
{
    size_t i;
    u32b tmp32u;
    char name[NORMAL_WID];

    /* Current player ID */
    rd_s32b(&player_id);

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

        /* Remove duplicates from the player name database */
        delete_player_name(name);

        /* Store the player name */
        add_player_name(id, account, name, &death_turn);
    }

    /* Success */
    return (0);
}
