/*
 * File: mon-init.c
 * Purpose: Monster initialization routines.
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2011 noz
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


struct monster_pain *pain_messages;
struct monster_spell *monster_spells;
const struct monster_race *ref_race = NULL;


const char *r_info_flags[] =
{
    #define RF(a, b, c) #a,
    #include "../common/list-mon-race-flags.h"
    #undef RF
    NULL
};


const char *r_info_spell_flags[] =
{
    #define RSF(a, b, c) #a,
    #include "../common/list-mon-spells.h"
    #undef RSF
    NULL
};


static const char *effect_list[] = {
    NULL,
    #define EFFECT(x, a, b, c, d, e) #x,
    #include "list-effects.h"
    #undef EFFECT
    NULL
};


/* Parsing functions for monster_spell.txt */
static enum parser_error parse_mon_spell_name(struct parser *p)
{
    struct monster_spell *h = parser_priv(p);
    struct monster_spell *s = mem_zalloc(sizeof *s);
    const char *name = parser_getstr(p, "name");
    int index;

    s->next = h;
    if (grab_name("monster spell", name, r_info_spell_flags, N_ELEMENTS(r_info_spell_flags), &index))
        return PARSE_ERROR_INVALID_SPELL_NAME;
    s->index = index;
    parser_setpriv(p, s);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_message_type(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);
    int msg_index;
    const char *type;

    my_assert(s);

    type = parser_getsym(p, "type");

    msg_index = message_lookup_by_name(type);

    if (msg_index < 0)
        return PARSE_ERROR_INVALID_MESSAGE;

    s->msgt = msg_index;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_message(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    my_assert(s);
    s->message = string_append(s->message, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_blind_message(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    my_assert(s);
    s->blind_message = string_append(s->blind_message, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_miss_message(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    my_assert(s);
    s->miss_message = string_append(s->miss_message, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_save_message(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    my_assert(s);
    s->save_message = string_append(s->save_message, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_lore_desc(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    my_assert(s);
    s->lore_desc = string_append(s->lore_desc, parser_getstr(p, "text"));
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_hit(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    my_assert(s);
    s->hit = parser_getuint(p, "hit");
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_effect(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);
    struct effect *new_effect = mem_zalloc(sizeof(*new_effect));
    const char *type;
    int val;

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    if (grab_name("effect", parser_getsym(p, "eff"), effect_list, N_ELEMENTS(effect_list), &val))
        return PARSE_ERROR_INVALID_EFFECT;
    new_effect->index = val;

    if (parser_hasval(p, "type"))
    {
        type = parser_getsym(p, "type");

        if (type == NULL) return PARSE_ERROR_UNRECOGNISED_PARAMETER;

        /* Check for a value */
        val = effect_param(new_effect->index, type);
        if (val < 0) return PARSE_ERROR_INVALID_VALUE;
        new_effect->params[0] = val;
    }

    if (parser_hasval(p, "xtra"))
        new_effect->params[1] = parser_getint(p, "xtra");

    new_effect->next = s->effect;
    s->effect = new_effect;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_param(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (s->effect == NULL) return PARSE_ERROR_NONE;

    s->effect->params[1] = parser_getint(p, "p2");

    if (parser_hasval(p, "p3"))
        s->effect->params[2] = parser_getint(p, "p3");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_dice(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);
    dice_t *dice = NULL;
    const char *string = NULL;

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (s->effect == NULL) return PARSE_ERROR_NONE;

    dice = dice_new();

    if (dice == NULL) return PARSE_ERROR_INVALID_DICE;

    string = parser_getstr(p, "dice");

    if (dice_parse_string(dice, string))
        s->effect->dice = dice;
    else
    {
        dice_free(dice);
        return PARSE_ERROR_INVALID_DICE;
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_expr(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);
    expression_t *expression = NULL;
    expression_base_value_f function = NULL;
    const char *name;
    const char *base;
    const char *expr;

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (s->effect == NULL) return PARSE_ERROR_NONE;

    /* If there are no dice, assume that this is human and not parser error. */
    if (s->effect->dice == NULL) return PARSE_ERROR_NONE;

    name = parser_getsym(p, "name");
    base = parser_getsym(p, "base");
    expr = parser_getstr(p, "expr");
    expression = expression_new();

    if (expression == NULL) return PARSE_ERROR_INVALID_EXPRESSION;

    function = spell_value_base_by_name(base);
    expression_set_base_value(expression, function);

    if (expression_add_operations_string(expression, expr) < 0)
        return PARSE_ERROR_BAD_EXPRESSION_STRING;

    if (dice_bind_expression(s->effect->dice, name, expression) < 0)
        return PARSE_ERROR_UNBOUND_EXPRESSION;

    /* The dice object makes a deep copy of the expression, so we can free it */
    expression_free(expression);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_spell_power(struct parser *p)
{
    struct monster_spell *s = parser_priv(p);

    my_assert(s);
    s->power = parser_getrand(p, "power");
    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_mon_spell(void)
{
    struct parser *p = parser_new();
    parser_setpriv(p, NULL);

    parser_reg(p, "name str name", parse_mon_spell_name);
    parser_reg(p, "msgt sym type", parse_mon_spell_message_type);
    parser_reg(p, "message-vis str text", parse_mon_spell_message);
    parser_reg(p, "message-invis str text", parse_mon_spell_blind_message);
    parser_reg(p, "message-miss str text", parse_mon_spell_miss_message);
    parser_reg(p, "message-save str text", parse_mon_spell_save_message);
    parser_reg(p, "lore str text", parse_mon_spell_lore_desc);
    parser_reg(p, "hit uint hit", parse_mon_spell_hit);
    parser_reg(p, "effect sym eff ?sym type ?int xtra", parse_mon_spell_effect);
    parser_reg(p, "param int p2 ?int p3", parse_mon_spell_param);
    parser_reg(p, "dice str dice", parse_mon_spell_dice);
    parser_reg(p, "expr sym name sym base str expr", parse_mon_spell_expr);
    parser_reg(p, "power rand power", parse_mon_spell_power);
    return p;
}


static errr run_parse_mon_spell(struct parser *p)
{
    return parse_file_quit_not_found(p, "monster_spell");
}


static errr finish_parse_mon_spell(struct parser *p)
{
    monster_spells = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_mon_spell(void)
{
    struct monster_spell *rs, *next;

    rs = monster_spells;
    while (rs)
    {
        next = rs->next;
        free_effect(rs->effect);
        string_free(rs->message);
        string_free(rs->blind_message);
        string_free(rs->miss_message);
        string_free(rs->save_message);
        string_free(rs->lore_desc);
        mem_free(rs);
        rs = next;
    }
}


struct file_parser mon_spell_parser =
{
    "monster_spell",
    init_parse_mon_spell,
    run_parse_mon_spell,
    finish_parse_mon_spell,
    cleanup_mon_spell
};


/* Parsing functions for monster_base.txt */
static enum parser_error parse_mon_base_name(struct parser *p)
{
    struct monster_base *h = parser_priv(p);
    struct monster_base *rb = mem_zalloc(sizeof(*rb));

    rb->next = h;
    rb->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, rb);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_base_glyph(struct parser *p)
{
    struct monster_base *rb = parser_priv(p);

    if (!rb) return PARSE_ERROR_MISSING_RECORD_HEADER;

    rb->d_char = parser_getchar(p, "glyph");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_base_pain(struct parser *p)
{
    struct monster_base *rb = parser_priv(p);
    int pain_idx;

    if (!rb) return PARSE_ERROR_MISSING_RECORD_HEADER;

    pain_idx = parser_getuint(p, "pain");
    if (pain_idx >= z_info->mp_max) return PARSE_ERROR_OUT_OF_BOUNDS;

    rb->pain = &pain_messages[pain_idx];

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_base_flags(struct parser *p)
{
    struct monster_base *rb = parser_priv(p);
    char *flags;
    char *s;

    if (!rb) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(rb->flags, RF_SIZE, r_info_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_base_spells(struct parser *p)
{
    struct monster_base *rb = parser_priv(p);
    char *flags;
    char *s;

    if (!rb) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "spells")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "spells"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(rb->spell_flags, RSF_SIZE, r_info_spell_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_mon_base_desc(struct parser *p)
{
    struct monster_base *rb = parser_priv(p);

    if (!rb) return PARSE_ERROR_MISSING_RECORD_HEADER;
    rb->text = string_append(rb->text, parser_getstr(p, "desc"));
    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_mon_base(void)
{
    struct parser *p = parser_new();
    parser_setpriv(p, NULL);

    parser_reg(p, "name str name", parse_mon_base_name);
    parser_reg(p, "glyph char glyph", parse_mon_base_glyph);
    parser_reg(p, "pain uint pain", parse_mon_base_pain);
    parser_reg(p, "flags ?str flags", parse_mon_base_flags);
    parser_reg(p, "spells ?str spells", parse_mon_base_spells);
    parser_reg(p, "desc str desc", parse_mon_base_desc);
    return p;
}


static errr run_parse_mon_base(struct parser *p)
{
    return parse_file_quit_not_found(p, "monster_base");
}


static errr finish_parse_mon_base(struct parser *p)
{
    rb_info = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_mon_base(void)
{
    struct monster_base *rb, *next;

    rb = rb_info;
    while (rb)
    {
        next = rb->next;
        string_free(rb->text);
        string_free(rb->name);
        mem_free(rb);
        rb = next;
    }
}


struct file_parser mon_base_parser =
{
    "monster_base",
    init_parse_mon_base,
    run_parse_mon_base,
    finish_parse_mon_base,
    cleanup_mon_base
};


/* Parsing functions for monster.txt */
static enum parser_error parse_monster_name(struct parser *p)
{
    struct monster_race *h = parser_priv(p);
    struct monster_race *r = mem_zalloc(sizeof(*r));

    r->next = h;
    r->ridx = parser_getuint(p, "index");
    r->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, r);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_base(struct parser *p)
{
    struct monster_race *r = parser_priv(p);

    r->base = lookup_monster_base(parser_getsym(p, "base"));
    if (r->base == NULL)
        /* Todo: make new error for this */
        return PARSE_ERROR_UNRECOGNISED_TVAL;

    /* The template sets the default display character */
    r->d_char = r->base->d_char;

    /* Give the monster its default flags */
    rf_union(r->flags, r->base->flags);
    rsf_union(r->spell_flags, r->base->spell_flags);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_glyph(struct parser *p)
{
    struct monster_race *r = parser_priv(p);

    /* If the display character is specified, it overrides any template */
    r->d_char = parser_getchar(p, "glyph");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_color(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    const char *color;
    int attr;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    color = parser_getsym(p, "color");
    if (strlen(color) > 1)
        attr = color_text_to_attr(color);
    else
        attr = color_char_to_attr(color[0]);
    if (attr < 0) return PARSE_ERROR_INVALID_COLOR;
    r->d_attr = attr;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_info(struct parser *p)
{
    struct monster_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->speed = parser_getint(p, "speed");
    r->avg_hp = parser_getint(p, "hp");

    /* Area of action assumes max_sight is 20, so we adjust in case it isn't */
    r->aaf = parser_getint(p, "aaf") * 20 / z_info->max_sight;

    r->ac = parser_getint(p, "ac");
    r->sleep = parser_getint(p, "sleep");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_power(struct parser *p)
{
    struct monster_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->level = parser_getint(p, "level");
    r->rarity = parser_getint(p, "rarity");
    r->extra = parser_getint(p, "extra");
    r->power = parser_getuint(p, "power");
    r->scaled_power = parser_getuint(p, "scaled");
    r->mexp = parser_getint(p, "mexp");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_blow(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    struct monster_blow *b = r->blow;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* Go to the last valid blow, then allocate a new one */
    if (!b)
    {
        r->blow = mem_zalloc(sizeof(struct monster_blow));
        b = r->blow;
    }
    else
    {
        while (b->next) b = b->next;
        b->next = mem_zalloc(sizeof(struct monster_blow));
        b = b->next;
    }

    /* Now read the data */
    b->method = blow_method_name_to_idx(parser_getsym(p, "method"));
    if (!monster_blow_method_is_valid(b->method)) return PARSE_ERROR_UNRECOGNISED_BLOW;
    if (parser_hasval(p, "effect"))
    {
        b->effect = blow_effect_name_to_idx(parser_getsym(p, "effect"));
        if (!monster_blow_effect_is_valid(b->effect)) return PARSE_ERROR_INVALID_EFFECT;
    }
    if (parser_hasval(p, "damage"))
        b->dice = parser_getrand(p, "damage");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_flags(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    char *flags;
    char *s;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(r->flags, RF_SIZE, r_info_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_flags_off(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    char *flags;
    char *s;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (remove_flag(r->flags, RF_SIZE, r_info_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_desc(struct parser *p)
{
    struct monster_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->text = string_append(r->text, parser_getstr(p, "desc"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_spell_freq(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    int pct;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    pct = parser_getint(p, "freq");
    if (pct < 1 || pct > 100)
        return PARSE_ERROR_INVALID_SPELL_FREQ;
    r->freq_spell = 100 / pct;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_spells(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    char *flags;
    char *s;
    int ret = PARSE_ERROR_NONE;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    flags = string_make(parser_getstr(p, "spells"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(r->spell_flags, RSF_SIZE, r_info_spell_flags, s))
        {
            ret = PARSE_ERROR_INVALID_FLAG;
            break;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return ret;
}


static enum parser_error parse_monster_drop(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    struct monster_drop *d;
    struct object_kind *k;
    int tval, sval;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;
    sval = lookup_sval(tval, parser_getsym(p, "sval"));
    if (sval < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;

    if ((parser_getuint(p, "min") > (unsigned int)z_info->stack_size) ||
        (parser_getuint(p, "max") > (unsigned int)z_info->stack_size))
    {
        return PARSE_ERROR_INVALID_ITEM_NUMBER;
    }

    k = lookup_kind(tval, sval);
    if (!k) return PARSE_ERROR_UNRECOGNISED_SVAL;

    d = mem_zalloc(sizeof(*d));
    d->kind = k;
    d->percent_chance = parser_getuint(p, "chance");
    d->min = parser_getuint(p, "min");
    d->max = parser_getuint(p, "max");
    d->next = r->drops;
    r->drops = d;
    return PARSE_ERROR_NONE;
}


/*
 * Return the a_idx of the artifact with the given name
 */
static int lookup_artifact_name(const char *name)
{
    int i;
    int a_idx = -1;

    /* Look for it */
    for (i = 1; i < z_info->a_max; i++)
    {
        struct artifact *art = &a_info[i];

        /* Test for equality */
        if (art->name && streq(name, art->name)) return i;

        /* Test for close matches */
        if ((strlen(name) >= 3) && art->name && my_stristr(art->name, name) && (a_idx == -1))
            a_idx = i;
    }

    /* Return our best match */
    return a_idx;
}


static enum parser_error parse_monster_drop_artifact(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    struct monster_drop *d;
    int art;
    struct artifact *a;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    art = lookup_artifact_name(parser_getstr(p, "name"));
    if (art < 0) return PARSE_ERROR_NO_ARTIFACT_NAME;
    a = &a_info[art];

    d = mem_zalloc(sizeof(*d));
    d->artifact = a;
    d->min = 1;
    d->max = 1;
    d->percent_chance = 100;
    d->next = r->drops;
    r->drops = d;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_friends(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    struct monster_friends *f;
    struct random number;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f = mem_zalloc(sizeof(*f));
    number = parser_getrand(p, "number");
    f->number_dice = number.dice;
    f->number_side = number.sides;
    f->percent_chance = parser_getuint(p, "chance");
    f->name = string_make(parser_getstr(p, "name"));
    f->next = r->friends;
    r->friends = f;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_friends_base(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    struct monster_friends_base *f;
    struct random number;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f = mem_zalloc(sizeof(*f));
    number = parser_getrand(p, "number");
    f->number_dice = number.dice;
    f->number_side = number.sides;
    f->percent_chance = parser_getuint(p, "chance");
    f->base = lookup_monster_base(parser_getstr(p, "name"));
    if (!f->base) return PARSE_ERROR_UNRECOGNISED_TVAL;

    f->next = r->friends_base;
    r->friends_base = f;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_mimic(struct parser *p)
{
    struct monster_race *r = parser_priv(p);
    struct monster_mimic *m;
    int tval, sval;
    struct object_kind *kind;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;
    sval = lookup_sval(tval, parser_getsym(p, "sval"));
    if (sval < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;

    kind = lookup_kind(tval, sval);
    if (!kind) return PARSE_ERROR_NO_KIND_FOUND;
    m = mem_zalloc(sizeof(*m));
    m->kind = kind;
    m->next = r->mimic_kinds;
    r->mimic_kinds = m;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_monster_plural(struct parser *p)
{
    struct monster_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (parser_hasval(p, "plural"))
    {
        const char *plural = parser_getstr(p, "plural");

        if (strlen(plural) > 0)
            r->plural = string_make(plural);
        else
            r->plural = NULL;
    }

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_monster(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);

    parser_reg(p, "name uint index str name", parse_monster_name);
    parser_reg(p, "plural ?str plural", parse_monster_plural);
    parser_reg(p, "base sym base", parse_monster_base);
    parser_reg(p, "glyph char glyph", parse_monster_glyph);
    parser_reg(p, "color sym color", parse_monster_color);
    parser_reg(p, "info int speed int hp int aaf int ac int sleep", parse_monster_info);
    parser_reg(p, "power int level int rarity int extra uint power uint scaled int mexp",
        parse_monster_power);
    parser_reg(p, "blow sym method ?sym effect ?rand damage", parse_monster_blow);
    parser_reg(p, "flags ?str flags", parse_monster_flags);
    parser_reg(p, "flags-off ?str flags", parse_monster_flags_off);
    parser_reg(p, "desc str desc", parse_monster_desc);
    parser_reg(p, "spell-freq int freq", parse_monster_spell_freq);
    parser_reg(p, "spells str spells", parse_monster_spells);
    parser_reg(p, "drop sym tval sym sval uint chance uint min uint max", parse_monster_drop);
    parser_reg(p, "drop-artifact str name", parse_monster_drop_artifact);
    parser_reg(p, "friends uint chance rand number str name", parse_monster_friends);
    parser_reg(p, "friends-base uint chance rand number str name", parse_monster_friends_base);
    parser_reg(p, "mimic sym tval sym sval", parse_monster_mimic);

    return p;
}


static errr run_parse_monster(struct parser *p)
{
    return parse_file_quit_not_found(p, "monster");
}


static errr finish_parse_monster(struct parser *p)
{
    struct monster_race *r, *n;
    size_t i;

    /* Scan the list for the max id and max blows */
    z_info->r_max = 0;
    z_info->mon_blows_max = 0;
    r = parser_priv(p);
    while (r)
    {
        int max_blows = 0;
        struct monster_blow *b = r->blow;

        if (r->ridx > (unsigned int)z_info->r_max) z_info->r_max = r->ridx;
        while (b)
        {
            b = b->next;
            max_blows++;
        }
        if (max_blows > z_info->mon_blows_max) z_info->mon_blows_max = max_blows;
        r = r->next;
    }
    z_info->r_max++;

    /* Allocate the direct access list and copy the race records to it */
    r_info = mem_zalloc(z_info->r_max * sizeof(*r));
    for (r = parser_priv(p); r; r = n)
    {
        struct monster_blow *b, *bn = NULL;

        /* Main record */
        memcpy(&r_info[r->ridx], r, sizeof(*r));
        n = r->next;
        if (n) r_info[r->ridx].next = &r_info[n->ridx];
        else r_info[r->ridx].next = NULL;

        /* Blows */
        r_info[r->ridx].blow = mem_zalloc(z_info->mon_blows_max * sizeof(struct monster_blow));
        for (i = 0, b = r->blow; b; i++, b = bn)
        {
            memcpy(&r_info[r->ridx].blow[i], b, sizeof(*b));
            r_info[r->ridx].blow[i].next = NULL;
            bn = b->next;
            mem_free(b);
        }

        mem_free(r);
    }

    /* Convert friend names into race pointers */
    for (i = 0; i < (size_t)z_info->r_max; i++)
    {
        struct monster_race *r = &r_info[i];
        struct monster_friends *f;

        for (f = r->friends; f; f = f->next)
        {
            if (!my_stricmp(f->name, "same"))
                f->race = r;
            else
                f->race = lookup_monster(f->name);

            if (!f->race)
                quit_fmt("Couldn't find friend named '%s' for monster '%s'", f->name, r->name);

            string_free(f->name);
        }
    }

    /* Allocate space for the monster lore */
    for (i = 0; i < (size_t)z_info->r_max; i++)
    {
        struct monster_lore *l = &r_info[i].lore;

        l->blows = mem_zalloc(z_info->mon_blows_max * sizeof(byte));
        l->blow_known = mem_zalloc(z_info->mon_blows_max * sizeof(bool));
    }

    /* Write new monster.txt file if requested */
    if (arg_power || arg_rebalance) eval_monster_power(r_info);

    parser_destroy(p);
    return 0;
}


static void cleanup_monster(void)
{
    int ridx;

    /* Paranoia */
    if (!r_info) return;

    for (ridx = 0; ridx < z_info->r_max; ridx++)
    {
        struct monster_race *r = &r_info[ridx];
        struct monster_drop *d = NULL;
        struct monster_friends *f = NULL;
        struct monster_friends_base *fb = NULL;
        struct monster_mimic *m = NULL;

        d = r->drops;
        while (d)
        {
            struct monster_drop *dn = d->next;

            mem_free(d);
            d = dn;
        }
        f = r->friends;
        while (f)
        {
            struct monster_friends *fn = f->next;

            mem_free(f);
            f = fn;
        }
        fb = r->friends_base;
        while (fb)
        {
            struct monster_friends_base *fbn = fb->next;

            mem_free(fb);
            fb = fbn;
        }
        m = r->mimic_kinds;
        while (m)
        {
            struct monster_mimic *mn = m->next;

            mem_free(m);
            m = mn;
        }
        string_free(r->plural);
        string_free(r->text);
        string_free(r->name);
        mem_free(r->blow);
        mem_free(r->lore.blows);
        mem_free(r->lore.blow_known);
    }

    mem_free(r_info);
}


struct file_parser monster_parser =
{
    "monster",
    init_parse_monster,
    run_parse_monster,
    finish_parse_monster,
    cleanup_monster
};