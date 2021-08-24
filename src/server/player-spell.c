/*
 * File: player-spell.c
 * Purpose: Spell and prayer casting/praying
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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
 * Stat Table (INT/WIS) -- minimum failure rate (percentage)
 */
static const int adj_mag_fail[] =
{
    99  /* 3 */,
    99  /* 4 */,
    99  /* 5 */,
    99  /* 6 */,
    99  /* 7 */,
    50  /* 8 */,
    30  /* 9 */,
    20  /* 10 */,
    15  /* 11 */,
    12  /* 12 */,
    11  /* 13 */,
    10  /* 14 */,
    9   /* 15 */,
    8   /* 16 */,
    7   /* 17 */,
    6   /* 18/00-18/09 */,
    6   /* 18/10-18/19 */,
    5   /* 18/20-18/29 */,
    5   /* 18/30-18/39 */,
    5   /* 18/40-18/49 */,
    4   /* 18/50-18/59 */,
    4   /* 18/60-18/69 */,
    4   /* 18/70-18/79 */,
    4   /* 18/80-18/89 */,
    3   /* 18/90-18/99 */,
    3   /* 18/100-18/109 */,
    2   /* 18/110-18/119 */,
    2   /* 18/120-18/129 */,
    2   /* 18/130-18/139 */,
    2   /* 18/140-18/149 */,
    1   /* 18/150-18/159 */,
    1   /* 18/160-18/169 */,
    1   /* 18/170-18/179 */,
    1   /* 18/180-18/189 */,
    1   /* 18/190-18/199 */,
    0   /* 18/200-18/209 */,
    0   /* 18/210-18/219 */,
    0   /* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- failure rate adjustment
 */
const int adj_mag_stat[] =
{
    -5  /* 3 */,
    -4  /* 4 */,
    -3  /* 5 */,
    -3  /* 6 */,
    -2  /* 7 */,
    -1  /* 8 */,
     0  /* 9 */,
     0  /* 10 */,
     0  /* 11 */,
     0  /* 12 */,
     0  /* 13 */,
     1  /* 14 */,
     2  /* 15 */,
     3  /* 16 */,
     4  /* 17 */,
     5  /* 18/00-18/09 */,
     6  /* 18/10-18/19 */,
     7  /* 18/20-18/29 */,
     8  /* 18/30-18/39 */,
     9  /* 18/40-18/49 */,
    10  /* 18/50-18/59 */,
    11  /* 18/60-18/69 */,
    12  /* 18/70-18/79 */,
    15  /* 18/80-18/89 */,
    18  /* 18/90-18/99 */,
    21  /* 18/100-18/109 */,
    24  /* 18/110-18/119 */,
    27  /* 18/120-18/129 */,
    30  /* 18/130-18/139 */,
    33  /* 18/140-18/149 */,
    36  /* 18/150-18/159 */,
    39  /* 18/160-18/169 */,
    42  /* 18/170-18/179 */,
    45  /* 18/180-18/189 */,
    48  /* 18/190-18/199 */,
    51  /* 18/200-18/209 */,
    54  /* 18/210-18/219 */,
    57  /* 18/220+ */
};


/*
 * Initialize player spells
 */
void player_spells_init(struct player *p)
{
    int i, num_spells = p->clazz->magic.total_spells;

    /* None */
    if (!num_spells) return;

    /* Allocate */
    p->spell_flags = mem_zalloc(num_spells * sizeof(byte));
    p->spell_order = mem_zalloc(num_spells * sizeof(byte));
    p->spell_power = mem_zalloc(num_spells * sizeof(byte));

    /* None of the spells have been learned yet */
    for (i = 0; i < num_spells; i++) p->spell_order[i] = 99;
}


/*
 * Free player spells
 */
void player_spells_free(struct player *p)
{
    mem_free(p->spell_flags);
    mem_free(p->spell_order);
    mem_free(p->spell_power);
}


/*
 * Get the spellbook structure from an object which is a book the player can
 * cast from
 */
const struct class_book *object_to_book(struct player *p, const struct object *obj)
{
    int i;

    for (i = 0; i < p->clazz->magic.num_books; i++)
    {
        if ((obj->tval == p->clazz->magic.books[i].tval) &&
            (obj->sval == p->clazz->magic.books[i].sval))
        {
            return &p->clazz->magic.books[i];
        }
    }

    return NULL;
}


/*
 * Get the spellbook structure index from an object which is a book the player can
 * cast from
 */
int object_to_book_index(struct player *p, const struct object *obj)
{
    int i;

    for (i = 0; i < p->clazz->magic.num_books; i++)
    {
        if ((obj->tval == p->clazz->magic.books[i].tval) &&
            (obj->sval == p->clazz->magic.books[i].sval))
        {
            return i;
        }
    }

    return -1;
}


const struct class_spell *spell_by_index(const struct class_magic *magic, int index)
{
    int book = 0, count = 0;

    /* Check index validity */
    if ((index < 0) || (index >= magic->total_spells)) return NULL;

    /* Find the book, count the spells in previous books */
    while (count + magic->books[book].num_spells - 1 < index)
        count += magic->books[book++].num_spells;

    /* Find the spell */
    return &magic->books[book].spells[index - count];
}


/*
 * Spell failure adjustment by casting stat level
 */
static int fail_adjust(struct player *p)
{
    int stat = p->clazz->magic.spell_realm->stat;

    return adj_mag_stat[p->state.stat_ind[stat]];
}


/*
 * Spell minimum failure by casting stat level
 */
static int min_fail(struct player *p)
{
    int stat = p->clazz->magic.spell_realm->stat;

    return adj_mag_fail[p->state.stat_ind[stat]];
}


/*
 * Returns chance of failure for a spell
 */
s16b spell_chance(struct player *p, int spell_index)
{
    int chance, minfail;
    const struct class_spell *spell;

    /* Paranoia -- must be literate */
    if (!p->clazz->magic.spell_realm) return (100);

    /* Get the spell */
    spell = spell_by_index(&p->clazz->magic, spell_index);

    /* Extract the base spell failure rate */
    chance = spell->sfail;

    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (p->lev - spell->slevel);

    /* Reduce failure rate by realm adjustment */
    chance -= fail_adjust(p);

    /* Extract the minimum failure rate */
    minfail = min_fail(p);

    /* Non mage/priest characters never get better than 5 percent */
    if (!player_has(p, PF_ZERO_FAIL) && (minfail < 5)) minfail = 5;

    /* Priest prayer penalty for "edged" weapons (before minfail) */
    if (p->state.icky_wield) chance += 25;

    /* Fear makes spells harder (before minfail) */
    if (player_of_has(p, OF_AFRAID)) chance += 20;

    /* Minimal and maximal failure rate */
    if (chance < minfail) chance = minfail;
    if (chance > 50) chance = 50;

    /* Stunning makes spells harder */
    if (p->timed[TMD_STUN] > 50) chance += 25;
    else if (p->timed[TMD_STUN]) chance += 15;

    /* Amnesia doubles failure change */
    if (p->timed[TMD_AMNESIA]) chance = 50 + chance / 2;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    /* Return the chance */
    return (chance);
}


bool spell_is_identify(struct player *p, int spell_index)
{
    const struct class_spell *spell = spell_by_index(&p->clazz->magic, spell_index);

    return (spell->effect->index == EF_IDENTIFY);
}


static size_t append_random_value_string(char *buffer, size_t size, random_value *rv)
{
    size_t offset = 0;

    if (rv->base > 0)
    {
        offset += strnfmt(buffer + offset, size - offset, "%d", rv->base);

        if (rv->dice > 0 || rv->sides > 0)
            offset += strnfmt(buffer + offset, size - offset, "+");
    }

    if (rv->dice == 1)
        offset += strnfmt(buffer + offset, size - offset, "d%d", rv->sides);
    else if (rv->dice > 1)
        offset += strnfmt(buffer + offset, size - offset, "%dd%d", rv->dice, rv->sides);

    return offset;
}


static void spell_append_value_info(struct player *p, int spell_index, char *buf, size_t len)
{
    const struct player_class *c = p->clazz;
    const struct magic_realm *realm;
    const char *name;
    const struct class_spell *spell;
    random_value rv;
    const char *type = NULL;
    const char *special = NULL;
    size_t offset = 0;
    bool first = true;
    struct effect *effect;
    struct source actor_body;
    struct source *data = &actor_body;

    source_player(data, 0, p);

    if (p->ghost && !player_can_undead(p)) c = player_id2class(CLASS_GHOST);

    realm = c->magic.spell_realm;
    name = (realm? realm->name: "");

    spell = spell_by_index(&c->magic, spell_index);

    while (true)
    {
        s16b current_spell;

        if (first) effect = spell->effect;
        else effect = effect->next;

        type = effect_info(effect);

        /* Hack -- teleport other (show nothing) */
        if ((effect->index == EF_BOLT) && (effect->params[0] == PROJ_AWAY_ALL)) return;

        /* Hack -- non-explosive branded shots (show nothing) */
        if ((effect->index == EF_BOW_BRAND) && (effect->params[1] == 0)) return;

        /* Hack -- non-damaging LOS effects (show nothing) */
        if ((effect->index == EF_PROJECT_LOS_AWARE) && (effect->params[1] == 0)) return;

        /* Hack -- illumination ("damage" value is used for radius, so change the tip accordingly) */
        if ((effect->index == EF_LIGHT_AREA) && streq(name, "elemental"))
            type = "range";

        /* Hack -- mana drain ("damage" value is used for healing, so change the tip accordingly) */
        if ((effect->index == EF_BOLT_AWARE) && (effect->params[0] == PROJ_DRAIN_MANA))
            type = "heal";

        /* Hack -- set current spell (for spell_value_base_by_name) */
        current_spell = p->current_spell;
        p->current_spell = spell_index;

        memset(&rv, 0, sizeof(rv));

        /* Hack -- mana drain (show real value) */
        if ((effect->index == EF_BOLT_AWARE) && (effect->params[0] == PROJ_DRAIN_MANA))
        {
            rv.base = 6;
            rv.dice = 3;
            rv.sides = p->lev;
        }

        /* Normal case -- use dice */
        else if (effect->dice != NULL)
            dice_roll(effect->dice, (void *)data, &rv);

        /* Hack -- reset current spell */
        p->current_spell = current_spell;

        /* Handle some special cases where we want to append some additional info. */
        switch (effect->index)
        {
            case EF_HEAL_HP:
            {
                /* Append percentage only, as the fixed value is always displayed */
                if (rv.m_bonus) special = format("/%d%%", rv.m_bonus);
                break;
            }
            case EF_BALL:
            case EF_BOLT_OR_BEAM:
            case EF_STAR:
            case EF_STAR_BALL:
            case EF_SWARM:
            {
                /* Append number of projectiles. */
                if (rv.m_bonus) special = format("x%d", rv.m_bonus);
                break;
            }
            case EF_BOW_BRAND_SHOT:
            {
                /* Append "per shot" */
                special = "/shot";
                break;
            }
            case EF_TIMED_INC:
            {
                if (rv.m_bonus)
                {
                    /* Append percentage only, as the fixed value is always displayed */
                    if (effect->params[0] == TMD_EPOWER)
                        special = format("/+%d%%", rv.m_bonus);

                    /* Append the bonus only, since the duration is always displayed. */
                    else
                        special = format("/+%d", rv.m_bonus);
                }
                break;
            }
        }

        if (type == NULL) return;

        if (first) offset += strnfmt(buf, len, " %s ", type);
        else offset += strnfmt(buf + offset, len - offset, "+");
        offset += append_random_value_string(buf + offset, len - offset, &rv);

        if (special != NULL)
            strnfmt(buf + offset, len - offset, "%s", special);

        first = false;

        /* Hack -- if next effect has the same tip, also append that info */
        if (!effect->next) return;
        if (!effect_info(effect->next)) return;
        if (strcmp(effect_info(effect->next), type)) return;
    }
}


void get_spell_info(struct player *p, int spell_index, char *buf, size_t len)
{
    /* Blank 'buf' first */
    buf[0] = '\0';

    spell_append_value_info(p, spell_index, buf, len);
}


static int spell_value_base_spell_power(void *data)
{
    int power = 0;

    /* Check the reference race first */
    if (ref_race)
        power = ref_race->spell_power;

    /* Otherwise the current monster if there is one */
    else
    {
        struct source *who = (struct source *)data;

        if (who->monster)
            power = who->monster->race->spell_power;
    }

    return power;
}


static int spell_value_base_player_level(void *data)
{
    struct source *who = (struct source *)data;

    return who->player->lev;
}


static int spell_value_base_dungeon_level(void *data)
{
    struct source *who = (struct source *)data;

    return who->player->wpos.depth;
}


static int spell_value_base_max_sight(void *data)
{
    return z_info->max_sight;
}


static int spell_value_base_food_faint(void *data)
{
    return PY_FOOD_FAINT;
}


static int spell_value_base_food_starve(void *data)
{
    return PY_FOOD_STARVE;
}


static int spell_value_base_weapon_damage(void *data)
{
    struct source *who = (struct source *)data;
    struct object *obj = who->player->body.slots[slot_by_name(who->player, "weapon")].obj;

    if (!obj) return 0;
    return (damroll(obj->dd, obj->ds) + obj->to_d);
}


static int spell_value_base_food_max(void *data)
{
    return PY_FOOD_MAX;
}


static int spell_value_base_player_spell_power(void *data)
{
    struct source *who = (struct source *)data;

    return who->player->spell_power[who->player->current_spell];
}


static int spell_value_base_ball_element(void *data)
{
    struct source *who = (struct source *)data;
    int power = who->player->spell_power[who->player->current_spell];

    return who->player->lev + power * 10;
}


static int spell_value_base_xball_element(void *data)
{
    struct source *who = (struct source *)data;
    int power = who->player->spell_power[who->player->current_spell];

    return who->player->lev + power * 5;
}


static int spell_value_base_blast_element(void *data)
{
    struct source *who = (struct source *)data;
    int power = who->player->spell_power[who->player->current_spell];

    return who->player->lev * 2 + power * 20;
}


static int spell_value_base_xblast_element(void *data)
{
    struct source *who = (struct source *)data;
    int power = who->player->spell_power[who->player->current_spell];

    return who->player->lev * 2 + power * 10;
}


expression_base_value_f spell_value_base_by_name(const char *name)
{
    static const struct value_base_s
    {
        const char *name;
        expression_base_value_f function;
    } value_bases[] =
    {
        {"SPELL_POWER", spell_value_base_spell_power},
        {"PLAYER_LEVEL", spell_value_base_player_level},
        {"DUNGEON_LEVEL", spell_value_base_dungeon_level},
        {"MAX_SIGHT", spell_value_base_max_sight},
        {"FOOD_FAINT", spell_value_base_food_faint},
        {"FOOD_STARVE", spell_value_base_food_starve},
        {"WEAPON_DAMAGE", spell_value_base_weapon_damage},
        {"FOOD_MAX", spell_value_base_food_max},
        {"PLAYER_SPELL_POWER", spell_value_base_player_spell_power},
        {"BALL_ELEMENT", spell_value_base_ball_element},
        {"XBALL_ELEMENT", spell_value_base_xball_element},
        {"BLAST_ELEMENT", spell_value_base_blast_element},
        {"XBLAST_ELEMENT", spell_value_base_xblast_element},
        {NULL, NULL}
    };
    const struct value_base_s *current = value_bases;

    while (current->name != NULL && current->function != NULL)
    {
        if (my_stricmp(name, current->name) == 0)
            return current->function;

        current++;
    }

    return NULL;
}


void cast_spell_end(struct player *p)
{
    int spell_index = p->current_spell;
    const struct class_spell *spell;
    const struct player_class *c = p->clazz;

    if (p->ghost && !player_can_undead(p)) c = player_id2class(CLASS_GHOST);

    /* Access the spell */
    spell = spell_by_index(&c->magic, spell_index);

    /* A spell was cast */
    if (!(p->spell_flags[spell_index] & PY_SPELL_WORKED))
    {
        int e = spell->sexp;

        /* The spell worked */
        p->spell_flags[spell_index] |= PY_SPELL_WORKED;

        /* Gain experience */
        player_exp_gain(p, e * spell->slevel);

        /* Redraw */
        p->upkeep->redraw |= (PR_SPELL);
    }
}


/*
 * Send the ghost spell info to the client.
 */
void show_ghost_spells(struct player *p)
{
    struct player_class *c = player_id2class(CLASS_GHOST);
    const struct class_book *book = &c->magic.books[0];
    struct class_spell *spell;
    int i;
    char out_val[NORMAL_WID];
    char out_desc[MSG_LEN];
    byte line_attr;
    char help[20];
    const char *comment = help;
    spell_flags flags;

    flags.line_attr = COLOUR_WHITE;
    flags.flag = RSF_NONE;
    flags.dir_attr = 0;
    flags.proj_attr = 0;

    /* Wipe the spell array */
    Send_spell_info(p, 0, 0, "", &flags);

    /* Check each spell */
    for (i = 0; i < book->num_spells; i++)
    {
        /* Access the spell */
        spell = &book->spells[i];

        /* Get extra info */
        get_spell_info(p, spell->sidx, help, sizeof(help));

        /* Assume spell is known and tried */
        comment = help;
        line_attr = COLOUR_WHITE;

        /* Format information */
        strnfmt(out_val, sizeof(out_val), "%-30s%2d %4d %3d%%%s", spell->name, spell->slevel,
            spell->smana, 0, comment);
        my_strcpy(out_desc, spell->text, sizeof(out_desc));

        flags.line_attr = line_attr;
        flags.flag = RSF_NONE;
        flags.dir_attr = effect_aim(spell->effect);
        flags.proj_attr = spell->sproj;

        /* Send it */
        Send_spell_info(p, 0, i, out_val, &flags);
        Send_spell_desc(p, 0, i, out_desc);
    }
}


/*
 * Get antimagic field from an object
 */
int antimagic_field(const struct object *obj, bitflag flags[OF_SIZE])
{
    /* Base antimagic field */
    return 10 * obj->modifiers[OBJ_MOD_ANTI_MAGIC];
}


/*
 * Check if the antimagic field around a player will disrupt the caster's spells.
 */
bool check_antimagic(struct player *p, struct chunk *c, struct monster *who)
{
    s16b id;
    int y, x, i, amchance, amrad, dist;

    /* The caster is a monster */
    if (who)
    {
        id = who->master;
        y = who->fy;
        x = who->fx;
    }

    /* The caster is the player */
    else
    {
        id = p->id;
        y = p->py;
        x = p->px;
    }

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);
        struct object *obj;

        /* Skip players not on this level */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

        /* Compute the probability of an unbeliever to disrupt any magic attempts */
        if (player_has(q, PF_ANTIMAGIC))
        {
            amchance = q->lev;
            amrad = 1 + q->lev / 10;
        }
        else
        {
            amchance = 0;
            amrad = 0;
        }

        /* Handle polymorphed players */
        if (q->poly_race)
        {
            if (rf_has(q->poly_race->flags, RF_ANTI_MAGIC))
            {
                amchance = q->poly_race->level / 2;
                amrad = 1 + q->poly_race->level / 20;
            }
        }

        /* Dark swords can disrupt magic attempts too */
        obj = equipped_item_by_slot_name(q, "weapon");
        if (obj)
        {
            int field = antimagic_field(obj, obj->flags);

            /* Apply field */
            amchance = amchance + field;
            amrad = amrad + field / 10;
        }

        /* Paranoia */
        if (amchance < 0) amchance = 0;
        if (amrad < 0) amrad = 0;

        /* Own antimagic field */
        if (p == q)
        {
            /* Antimagic field is capped at 90% */
            if (amchance > 90) amchance = 90;

            /* Check antimagic */
            if (magik(amchance))
            {
                if (who)
                {
                    char m_name[NORMAL_WID];

                    monster_desc(p, m_name, sizeof(m_name), who, MDESC_CAPITAL);
                    msg(p, "%s fails to cast a spell.", m_name);
                }
                else
                    msg(p, "Your anti-magic field disrupts your attempt.");
                return true;
            }
        }

        /* Antimagic field from other players */
        else
        {
            /* Lower effect from party mates (greatly) */
            if (master_in_party(id, q->id)) amchance >>= 2;

            /* Antimagic field is capped at 90% */
            if (amchance > 90) amchance = 90;

            /* Compute distance */
            dist = distance(y, x, q->py, q->px);
            if (dist > amrad) amchance = 0;

            /* Check antimagic */
            if (magik(amchance))
            {
                if (who)
                {
                    char m_name[NORMAL_WID];

                    monster_desc(p, m_name, sizeof(m_name), who, MDESC_CAPITAL);
                    msg(p, "%s fails to cast a spell.", m_name);
                }
                else
                {
                    if (player_is_visible(p, i))
                        msg(p, "%s's anti-magic field disrupts your attempt.", q->name);
                    else
                        msg(p, "An anti-magic field disrupts your attempt.");
                }
                return true;
            }
        }
    }

    /* Monsters don't disrupt other monsters' spells, that would be cheezy */
    if (who) return false;

    /* Check each monster */
    for (i = 1; i < cave_monster_max(c); i++)
    {
        struct monster *mon = cave_monster(c, i);
        struct monster_lore *lore;

        /* Paranoia -- skip dead monsters */
        if (!mon->race) continue;

        /* Learn about antimagic field */
        lore = get_lore(p, mon->race);
        if (monster_is_visible(p, i)) rf_on(lore->flags, RF_ANTI_MAGIC);

        /* Skip monsters without antimagic field */
        if (!rf_has(mon->race->flags, RF_ANTI_MAGIC)) continue;

        /* Compute the probability of a monster to disrupt any magic attempts */
        amchance = 25 + mon->level;
        amrad = 3 + (mon->level / 10);

        /* Lower effect from party mates (greatly) */
        if (master_in_party(id, mon->master)) amchance >>= 2;

        /* Antimagic field is capped at 90% */
        if (amchance > 90) amchance = 90;

        /* Compute distance */
        dist = distance(y, x, mon->fy, mon->fx);
        if (dist > amrad) amchance = 0;

        /* Check antimagic */
        if (magik(amchance))
        {
            if (monster_is_visible(p, i))
            {
                char m_name[NORMAL_WID];

                monster_desc(p, m_name, sizeof(m_name), mon, MDESC_CAPITAL);
                msg(p, "%s's anti-magic field disrupts your attempt.", m_name);
            }
            else
                msg(p, "An anti-magic field disrupts your attempt.");

            return true;
        }
    }

    /* Assume no antimagic */
    return false;
}


/*
 * Check if the antisummon field around a player will disrupt the caster's summoning spells.
 */
bool check_antisummon(struct player *p, struct monster *mon)
{
    s16b id;
    int y, x, i, amchance, amrad, dist;

    /* The caster is a monster */
    if (mon)
    {
        id = mon->master;
        y = mon->fy;
        x = mon->fx;
    }

    /* The caster is the player */
    else
    {
        id = p->id;
        y = p->py;
        x = p->px;
    }

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Skip players not on this level */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

        /* No antisummon */
        if (!q->timed[TMD_ANTISUMMON]) continue;

        /* Compute the probability of a summoner to disrupt any summon attempts */
        /* This value ranges from 60% (clvl 35) to 90% (clvl 50) */
        amchance = q->lev * 2 - 10;

        /* Range of the antisummon field (8-11 squares for a max sight of 20 squares) */
        amrad = 1 + z_info->max_sight * q->lev / 100;

        /* Own antisummon field */
        if (p == q)
        {
            /* Check antisummon */
            if (magik(amchance))
            {
                if (mon)
                {
                    char m_name[NORMAL_WID];

                    monster_desc(p, m_name, sizeof(m_name), mon, MDESC_CAPITAL);
                    msg(p, "%s fails to cast a spell.", m_name);
                }
                else
                    msg(p, "Your anti-summon field disrupts your attempt.");
                return true;
            }
        }

        /* Antisummon field from other players */
        else
        {
            /* Lower effect from party mates (greatly) */
            if (master_in_party(id, q->id)) amchance >>= 2;

            /* Compute distance */
            dist = distance(y, x, q->py, q->px);
            if (dist > amrad) amchance = 0;

            /* Check antisummon */
            if (magik(amchance))
            {
                if (mon)
                {
                    char m_name[NORMAL_WID];

                    monster_desc(p, m_name, sizeof(m_name), mon, MDESC_CAPITAL);
                    msg(p, "%s fails to cast a spell.", m_name);
                }
                else
                {
                    if (player_is_visible(p, i))
                        msg(p, "%s's anti-summon field disrupts your attempt.", q->name);
                    else
                        msg(p, "An anti-summon field disrupts your attempt.");
                }
                return true;
            }
        }
    }

    /* Assume no antisummon */
    return false;
}


/*
 * Send the mimic spell info to the client.
 */
void show_mimic_spells(struct player *p)
{
    const struct class_book *book = &p->clazz->magic.books[0];
    struct class_spell *spell;
    int i, j = 0, k = 0;
    char out_val[NORMAL_WID];
    char out_desc[MSG_LEN];
    byte line_attr;
    char help[20];
    const char *comment = help;
    int flag;
    spell_flags flags;

    flags.line_attr = COLOUR_WHITE;
    flags.flag = RSF_NONE;
    flags.dir_attr = 0;
    flags.proj_attr = 0;

    /* Wipe the spell array */
    Send_spell_info(p, 0, 0, "", &flags);

    /* Check each spell */
    for (i = 0; i < book->num_spells; i++)
    {
        /* Access the spell */
        spell = &book->spells[i];

        /* Access the spell flag */
        flag = spell->effect->flag;

        /* Check spell availability */
        if (!(p->poly_race && rsf_has(p->poly_race->spell_flags, flag))) continue;

        /* Get extra info */
        get_spell_info(p, spell->sidx, help, sizeof(help));

        /* Assume spell is known and tried */
        comment = help;
        line_attr = COLOUR_WHITE;

        /* Format information */
        strnfmt(out_val, sizeof(out_val), "%-30s%2d %4d %3d%%%s", spell->name, 0, spell->smana,
            spell->sfail, comment);
        strnfmt(out_desc, sizeof(out_desc), spell->text, flag);

        flags.line_attr = line_attr;
        flags.flag = flag;
        flags.dir_attr = effect_aim(spell->effect);
        flags.proj_attr = spell->sproj;

        /* Send it */
        Send_spell_info(p, k, j, out_val, &flags);
        Send_spell_desc(p, k, j, out_desc);

        /* Next spell */
        j++;
        if (j == MAX_SPELLS_PER_PAGE)
        {
            j = 0;
            k++;
        }
    }
}


/*
 * Project a spell on someone.
 *
 * p is the target of the spell
 * cidx is the class index of the caster of the spell
 * spell is the spell index
 * silent is true when no message is displayed
 */
bool cast_spell_proj(struct player *p, int cidx, int spell_index, bool silent)
{
    const struct player_class *c = player_id2class(cidx);
    const struct class_spell *spell = spell_by_index(&c->magic, spell_index);
    const struct magic_realm *realm = c->magic.spell_realm;
    const char *name = (realm? realm->name: "");
    bool pious = streq(name, "divine");
    bool ident = false;
    struct source who_body;
    struct source *who = &who_body;

    /* Clear current */
    current_clear(p);

    /* Set current spell */
    p->current_spell = spell_index;

    /* Hack -- save the class of the caster */
    p->current_item = 0 - cidx;

    /* Message */
    if (spell->effect && spell->effect->other_msg && !silent)
    {
        /* Hack -- formatted message */
        switch (spell->effect->flag)
        {
            case RSF_HEAL:
            case RSF_TELE_TO:
            case RSF_TELE_LEVEL:
            case RSF_FORGET:
            case RSF_S_KIN:
            {
                msg_format_near(p, MSG_PY_SPELL, spell->effect->other_msg, player_poss(p));
                break;
            }
            default:
            {
                msg_print_near(p, (pious? MSG_PY_PRAYER: MSG_PY_SPELL), spell->effect->other_msg);
                break;
            }
        }
    }

    source_player(who, get_player_index(get_connection(p->conn)), p);
    return effect_do(spell->effect, who, &ident, true, 0, NULL, 0, 0, NULL);
}


/*
 * Return the chance of an effect beaming, given a tval.
 */
static int beam_chance_tval(int tval)
{
    switch (tval)
    {
        case TV_WAND: return 20;
        case TV_ROD:  return 10;
    }

    return 0;
}


static int beam_chance(struct player* p)
{
    int plev = p->lev;

    return (player_has(p, PF_BEAM)? plev: plev / 2);
}


void fill_beam_info(struct player *p, int spell_index, struct beam_info *beam)
{
    const struct player_class *c;
    const struct class_spell *spell;
    const struct magic_realm *realm;
    const char *name;

    /* Initialize */
    memset(beam, 0, sizeof(struct beam_info));

    /* Use the spell parameter as a tval */
    if (!p)
    {
        beam->beam = beam_chance_tval(spell_index);
        return;
    }

    /* Use the spell parameter as a spell */
    beam->beam = beam_chance(p);

    c = p->clazz;
    if (p->ghost && !player_can_undead(p)) c = player_id2class(CLASS_GHOST);
    spell = spell_by_index(&c->magic, spell_index);

    realm = c->magic.spell_realm;
    name = (realm? realm->name: "");

    /* Hack -- elemental spells */
    if (streq(name, "elemental"))
    {
        int i, j;

        /* Spell power */
        beam->spell_power = p->spell_power[spell_index];

        /* Beam chance */
        if (spell->bidx < c->magic.num_books - 1)
            beam->beam += beam->spell_power * 10;
        else
            beam->beam += beam->spell_power * 5;

        /* Elemental power */
        if (p->timed[TMD_EPOWER])
        {
            for (i = 0; i < c->magic.num_books; i++)
            {
                for (j = 0; j < c->magic.books[i].num_spells; j++)
                {
                    spell = &c->magic.books[i].spells[j];

                    if ((spell->effect->index == EF_TIMED_INC) &&
                        (spell->effect->params[0] == TMD_EPOWER))
                    {
                        beam->elem_power = p->spell_power[spell->sidx];
                        break;
                    }
                }
            }
        }
    }
}
