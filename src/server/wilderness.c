/*
 * File: wilderness.c
 * Purpose: Wilderness generation
 *
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
 * The information about wilderness terrain features
 */
struct wild_feat *wf_info;


/*
 * The information about wilderness
 */
struct wilderness *wild_info;


/*
 * The information about town terrain features
 */
struct town_feat *tf_info;


/*
 * The information about towns
 */
struct location *towns = NULL;


/*
 * The information about dungeons
 */
struct location *dungeons = NULL;


/*
 * Wilderness radius.
 */
u16b radius_wild;


/*
 * Hack -- consistent wilderness layout
 */
u32b seed_wild;


/*
 * The information about arenas
 */
struct arena_type arenas[MAX_ARENAS];
u16b num_arenas = 0;


/*
 * The information about wilderness levels
 */
static struct wild_type **wt_info;


/*
 * Parsing functions for wild_feat.txt
 */
static const char *sound_list[] =
{
    #define MSG(x, s) #x,
    #include "../common/list-message.h"
    #undef MSG
};


static enum parser_error parse_wild_feat_name(struct parser *p)
{
    struct wild_feat *h = parser_priv(p);
    struct wild_feat *f = mem_zalloc(sizeof(*f));
    const char *name = parser_getstr(p, "name");

    f->next = h;
    f->name = string_make(name);
    parser_setpriv(p, f);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_wild_feat_feat(struct parser *p)
{
    struct wild_feat *f = parser_priv(p);
    int lvl = parser_getuint(p, "level");
    int idx = lookup_feat(parser_getsym(p, "index"));
    char sym = parser_getchar(p, "sym");

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->feat_lvl = lvl;
    f->feat_idx = idx;
    f->symbol = sym;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_wild_feat_chance(struct parser *p)
{
    struct wild_feat *f = parser_priv(p);
    unsigned int idx = parser_getuint(p, "index");
    int feat = lookup_feat(parser_getsym(p, "feat"));
    int chance = parser_getuint(p, "chance");

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (idx >= TERRAIN_TYPE_MAX) return PARSE_ERROR_INVALID_VALUE;
    f->chance[idx].feat = feat;
    f->chance[idx].chance = chance;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_wild_feat_sound(struct parser *p)
{
    struct wild_feat *f = parser_priv(p);
    const char *name = parser_getstr(p, "name");
    int index;

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (grab_name("sound", name, sound_list, N_ELEMENTS(sound_list), &index))
        return PARSE_ERROR_INVALID_VALUE;
    f->sound_idx = index;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_wild_feat(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_wild_feat_name);
    parser_reg(p, "feat uint level sym index char sym", parse_wild_feat_feat);
    parser_reg(p, "chance uint index sym feat uint chance", parse_wild_feat_chance);
    parser_reg(p, "sound str name", parse_wild_feat_sound);

    return p;
}


static errr run_parse_wild_feat(struct parser *p)
{
    return parse_file_quit_not_found(p, "wild_feat");
}


static errr finish_parse_wild_feat(struct parser *p)
{
    struct wild_feat *f, *n;
    int count;

    /* Scan the list for the max id */
    z_info->wf_max = 0;
    f = parser_priv(p);
    while (f)
    {
        z_info->wf_max++;
        f = f->next;
    }

    /* Allocate the direct access list and copy the data to it */
    wf_info = mem_zalloc(z_info->wf_max * sizeof(*f));
    count = z_info->wf_max - 1;
    for (f = parser_priv(p); f; f = n, count--)
    {
        memcpy(&wf_info[count], f, sizeof(*f));
        n = f->next;
        wf_info[count].next = NULL;
        mem_free(f);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_wild_feat(void)
{
    int i;

    /* Paranoia */
    if (!wf_info) return;

    for (i = 0; i < z_info->wf_max; i++)
        string_free(wf_info[i].name);
    mem_free(wf_info);
}


struct file_parser wild_feat_parser =
{
    "wild_feat",
    init_parse_wild_feat,
    run_parse_wild_feat,
    finish_parse_wild_feat,
    cleanup_wild_feat
};


/*
 * Parsing functions for wild_info.txt
 */
static enum parser_error parse_wild_info_radius(struct parser *p)
{
    struct wilderness *w = mem_zalloc(sizeof(*w));

    radius_wild = parser_getuint(p, "rad");
    parser_setpriv(p, w);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_wild_info_desc(struct parser *p)
{
    struct wilderness *w = parser_priv(p);
    const char *desc;

    if (!w) return PARSE_ERROR_MISSING_RECORD_HEADER;
    desc = parser_getstr(p, "text");
    if (strlen(desc) != (size_t)(2 * radius_wild + 1)) return PARSE_ERROR_INVALID_VALUE;
    w->text = string_append(w->text, desc);

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_wild_info(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "radius uint rad", parse_wild_info_radius);
    parser_reg(p, "D str text", parse_wild_info_desc);

    return p;
}


static errr run_parse_wild_info(struct parser *p)
{
    return parse_file_quit_not_found(p, "wild_info");
}


static errr finish_parse_wild_info(struct parser *p)
{
    wild_info = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_wild_info(void)
{
    if (wild_info)
    {
        string_free(wild_info->text);
        mem_free(wild_info);
    }
}


struct file_parser wild_info_parser =
{
    "wild_info",
    init_parse_wild_info,
    run_parse_wild_info,
    finish_parse_wild_info,
    cleanup_wild_info
};


/*
 * Parsing functions for town_feat.txt
 */
static enum parser_error parse_town_feat_feat(struct parser *p)
{
    struct town_feat *h = parser_priv(p);
    struct town_feat *f = mem_zalloc(sizeof(*f));
    char sym = parser_getchar(p, "sym");
    char spec = parser_getchar(p, "spec");
    int idx = lookup_feat(parser_getsym(p, "index"));

    f->next = h;
    f->symbol = sym;
    f->special = spec;
    f->feat_idx = idx;
    parser_setpriv(p, f);

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_town_feat(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "feat char sym char spec sym index", parse_town_feat_feat);

    return p;
}


static errr run_parse_town_feat(struct parser *p)
{
    return parse_file_quit_not_found(p, "town_feat");
}


static errr finish_parse_town_feat(struct parser *p)
{
    struct town_feat *f, *n;
    int count;

    /* Scan the list for the max id */
    z_info->tf_max = 0;
    f = parser_priv(p);
    while (f)
    {
        z_info->tf_max++;
        f = f->next;
    }

    /* Allocate the direct access list and copy the data to it */
    tf_info = mem_zalloc(z_info->tf_max * sizeof(*f));
    count = z_info->tf_max - 1;
    for (f = parser_priv(p); f; f = n, count--)
    {
        memcpy(&tf_info[count], f, sizeof(*f));
        n = f->next;
        tf_info[count].next = NULL;
        mem_free(f);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_town_feat(void)
{
    mem_free(tf_info);
}


struct file_parser town_feat_parser =
{
    "town_feat",
    init_parse_town_feat,
    run_parse_town_feat,
    finish_parse_town_feat,
    cleanup_town_feat
};


/*
 * Parsing functions for locations
 */
static enum parser_error parse_location_info_name(struct parser *p)
{
    struct location *h = parser_priv(p);
    struct location *t = mem_zalloc(sizeof(*t));
    const char *name = parser_getstr(p, "name");

    t->next = h;
    t->name = string_make(name);
    parser_setpriv(p, t);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_wpos(struct parser *p)
{
    struct location *t = parser_priv(p);
    int wx = parser_getint(p, "wx");
    int wy = parser_getint(p, "wy");

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    COORDS_SET(&t->wpos, wy, wx, 0);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_min_depth(struct parser *p)
{
    struct location *t = parser_priv(p);
    int depth = parser_getint(p, "depth");

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    t->min_depth = depth;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_max_depth(struct parser *p)
{
    struct location *t = parser_priv(p);
    int depth = parser_getint(p, "depth");

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    t->max_depth = depth;

    return PARSE_ERROR_NONE;
}


static const char *dungeon_flags[] =
{
    #define DF(a, b) #a,
    #include "list-dungeon-flags.h"
    #undef DF
    NULL
};


static enum parser_error parse_location_info_flags(struct parser *p)
{
    struct location *t = parser_priv(p);
    char *flags;
    char *s;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));

    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(t->flags, DF_SIZE, dungeon_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_floor(struct parser *p)
{
    struct location *t = parser_priv(p);
    struct dun_feature *f;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f = t->floors;

    /* Go to the last valid feature, then allocate a new one */
    if (!f)
    {
        t->floors = mem_zalloc(sizeof(struct dun_feature));
        f = t->floors;
    }
    else
    {
        while (f->next) f = f->next;
        f->next = mem_zalloc(sizeof(struct dun_feature));
        f = f->next;
    }

    /* Now read the data */
    f->feat = lookup_feat(parser_getsym(p, "feat"));
    f->percent = parser_getint(p, "percent");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_wall(struct parser *p)
{
    struct location *t = parser_priv(p);
    struct dun_feature *f;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f = t->walls;

    /* Go to the last valid feature, then allocate a new one */
    if (!f)
    {
        t->walls = mem_zalloc(sizeof(struct dun_feature));
        f = t->walls;
    }
    else
    {
        while (f->next) f = f->next;
        f->next = mem_zalloc(sizeof(struct dun_feature));
        f = f->next;
    }

    /* Now read the data */
    f->feat = lookup_feat(parser_getsym(p, "feat"));
    f->percent = parser_getint(p, "percent");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_rule(struct parser *p)
{
    struct location *t = parser_priv(p);
    struct dun_rule *r;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r = t->rules;

    /* Go to the last valid rule, then allocate a new one */
    if (!r)
    {
        t->rules = mem_zalloc(sizeof(struct dun_rule));
        r = t->rules;
    }
    else
    {
        while (r->next) r = r->next;
        r->next = mem_zalloc(sizeof(struct dun_rule));
        r = r->next;
    }

    /* Now read the data */
    r->percent = parser_getint(p, "percent");
    r->all = parser_getuint(p, "all");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_rule_flags(struct parser *p)
{
    struct location *t = parser_priv(p);
    struct dun_rule *r;
    char *flags;
    char *s;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r = t->rules;
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


static enum parser_error parse_location_info_rule_spells(struct parser *p)
{
    struct location *t = parser_priv(p);
    struct dun_rule *r;
    char *flags;
    char *s;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r = t->rules;
    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));

    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(r->spell_flags, RSF_SIZE, r_info_spell_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_location_info_rule_symbols(struct parser *p)
{
    struct location *t = parser_priv(p);
    struct dun_rule *r;
    const char *symbols = parser_getstr(p, "symbols");

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r = t->rules;
    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    my_strcpy(r->sym, symbols, sizeof(r->sym));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_location_info(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_location_info_name);
    parser_reg(p, "wpos int wx int wy", parse_location_info_wpos);
    parser_reg(p, "min-depth int depth", parse_location_info_min_depth);
    parser_reg(p, "max-depth int depth", parse_location_info_max_depth);
    parser_reg(p, "flags ?str flags", parse_location_info_flags);
    parser_reg(p, "floor sym feat int percent", parse_location_info_floor);
    parser_reg(p, "wall sym feat int percent", parse_location_info_wall);
    parser_reg(p, "rule int percent uint all", parse_location_info_rule);
    parser_reg(p, "rule-flags ?str flags", parse_location_info_rule_flags);
    parser_reg(p, "rule-spells ?str flags", parse_location_info_rule_spells);
    parser_reg(p, "rule-symbols str symbols", parse_location_info_rule_symbols);

    return p;
}


/*
 * Parsing functions for town.txt
 */
static errr run_parse_town_info(struct parser *p)
{
    return parse_file_quit_not_found(p, "town");
}


static errr finish_parse_town_info(struct parser *p)
{
    struct location *t, *n;
    int count;

    /* Scan the list for the max id */
    z_info->town_max = 0;
    t = parser_priv(p);
    while (t)
    {
        z_info->town_max++;
        t = t->next;
    }

    /* Allocate the direct access list and copy the data to it */
    towns = mem_zalloc(z_info->town_max * sizeof(*t));
    count = z_info->town_max - 1;
    for (t = parser_priv(p); t; t = n, count--)
    {
        struct dun_rule *r, *rn = NULL;
        struct dun_feature *f, *fn = NULL;
        int i;

        /* Main record */
        memcpy(&towns[count], t, sizeof(*t));
        n = t->next;
        towns[count].next = NULL;

        /* No rules */
        for (i = 0, r = t->rules; r; i++, r = rn)
        {
            rn = r->next;
            mem_free(r);
        }

        /* No features */
        for (i = 0, f = t->floors; f; i++, f = fn)
        {
            fn = f->next;
            mem_free(f);
        }
        for (i = 0, f = t->walls; f; i++, f = fn)
        {
            fn = f->next;
            mem_free(f);
        }

        mem_free(t);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_town_info(void)
{
    int i;

    /* Paranoia */
    if (!towns) return;

    for (i = 0; i < z_info->town_max; i++)
        string_free(towns[i].name);
    mem_free(towns);
}


struct file_parser town_info_parser =
{
    "town",
    init_parse_location_info,
    run_parse_town_info,
    finish_parse_town_info,
    cleanup_town_info
};


/*
 * Parsing functions for dungeon.txt
 */
static errr run_parse_dungeon_info(struct parser *p)
{
    return parse_file_quit_not_found(p, "dungeon");
}


static errr finish_parse_dungeon_info(struct parser *p)
{
    struct location *t, *n;
    int count;

    /* Scan the list for the max id */
    z_info->dungeon_max = 0;
    t = parser_priv(p);
    while (t)
    {
        z_info->dungeon_max++;
        t = t->next;
    }

    /* Allocate the direct access list and copy the data to it */
    dungeons = mem_zalloc(z_info->dungeon_max * sizeof(*t));
    count = z_info->dungeon_max - 1;
    for (t = parser_priv(p); t; t = n, count--)
    {
        struct dun_rule *r, *rn = NULL;
        struct dun_feature *f, *fn = NULL;
        int i;

        /* Main record */
        memcpy(&dungeons[count], t, sizeof(*t));
        n = t->next;
        dungeons[count].next = NULL;

        /* Rules */
        dungeons[count].rules = mem_zalloc(5 * sizeof(struct dun_rule));
        for (i = 0, r = t->rules; r; i++, r = rn)
        {
            /* Keep five rules at max */
            if (i < 5)
            {
                memcpy(&dungeons[count].rules[i], r, sizeof(*r));
                dungeons[count].rules[i].next = NULL;
            }
            rn = r->next;
            mem_free(r);
        }

        /* Features */
        dungeons[count].floors = mem_zalloc(3 * sizeof(struct dun_feature));
        for (i = 0, f = t->floors; f; i++, f = fn)
        {
            /* Keep three features at max */
            if (i < 3)
            {
                memcpy(&dungeons[count].floors[i], f, sizeof(*f));
                dungeons[count].floors[i].next = NULL;
            }
            fn = f->next;
            mem_free(f);
        }
        dungeons[count].walls = mem_zalloc(3 * sizeof(struct dun_feature));
        for (i = 0, f = t->walls; f; i++, f = fn)
        {
            /* Keep three features at max */
            if (i < 3)
            {
                memcpy(&dungeons[count].walls[i], f, sizeof(*f));
                dungeons[count].walls[i].next = NULL;
            }
            fn = f->next;
            mem_free(f);
        }

        mem_free(t);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_dungeon_info(void)
{
    int i;

    /* Paranoia */
    if (!dungeons) return;

    for (i = 0; i < z_info->dungeon_max; i++)
    {
        string_free(dungeons[i].name);
        mem_free(dungeons[i].rules);
    }
    mem_free(dungeons);
}


struct file_parser dungeon_info_parser =
{
    "dungeon",
    init_parse_location_info,
    run_parse_dungeon_info,
    finish_parse_dungeon_info,
    cleanup_dungeon_info
};


/*
 * Get wilderness level given by (y, x) coordinates
 */
struct wild_type *get_wt_info_at(int world_y, int world_x)
{
    /* Check bounds */
    if ((radius_wild - world_y < 0) || (radius_wild - world_y > 2 * radius_wild)) return NULL;
    if ((radius_wild + world_x < 0) || (radius_wild + world_x > 2 * radius_wild)) return NULL;

    return &wt_info[radius_wild - world_y][radius_wild + world_x];
}


/*
 * Get monster level given by depth
 */
int monster_level(struct worldpos *wpos)
{
    struct wild_type *w_ptr;

    if (wpos->depth > 0) return wpos->depth;

    /* Tougher at night */
    if (!is_daytime() && !town_area(wpos)) return 100;

    w_ptr = get_wt_info_at(wpos->wy, wpos->wx);
    return MIN(wf_info[w_ptr->type].feat_lvl, 2 * w_ptr->distance);
}


/*
 * Get object level given by depth
 */
int object_level(struct worldpos *wpos)
{
    struct wild_type *w_ptr;

    if (wpos->depth > 0) return wpos->depth;

    w_ptr = get_wt_info_at(wpos->wy, wpos->wx);
    return MIN(wf_info[w_ptr->type].feat_lvl, 2 * w_ptr->distance);
}


/*
 * Is this level the base level of a dungeon?
 */
bool surface_of_dungeon(struct worldpos *wpos)
{
    if (wpos->depth > 0) return false;
    return (get_wt_info_at(wpos->wy, wpos->wx)->max_depth > 1);
}


/*
 * Return the starting town coordinates.
 */
struct worldpos *start_wpos(void)
{
    /* Assume index of 0 */
    return &towns[0].wpos;
}


/*
 * Are we in the starting town?
 */
bool in_start_town(struct worldpos *wpos)
{
    return (COORDS_EQUAL(wpos, start_wpos()));
}


/*
 * Return the base town coordinates.
 */
struct worldpos *base_wpos(void)
{
    /* Assume index of 1 */
    return &towns[1].wpos;
}


/*
 * Are we in the base town?
 */
bool in_base_town(struct worldpos *wpos)
{
    return (COORDS_EQUAL(wpos, base_wpos()));
}


/*
 * Return the current town (structure).
 */
struct location *get_town(struct worldpos *wpos)
{
    int i;

    for (i = 0; i < z_info->town_max; i++)
    {
        if (COORDS_EQUAL(wpos, &towns[i].wpos)) return &towns[i];
    }

    return NULL;
}


/*
 * Is this level a town?
 */
bool in_town(struct worldpos *wpos)
{
    int i;

    for (i = 0; i < z_info->town_max; i++)
    {
        if (COORDS_EQUAL(wpos, &towns[i].wpos)) return true;
    }

    return false;
}


/*
 * Is this level a wilderness level?
 */
bool in_wild(struct worldpos *wpos)
{
    return ((wpos->depth == 0) && !in_town(wpos));
}


/*
 * Is this level in immediate suburb of a town?
 */
bool town_suburb(struct worldpos *wpos)
{
    if (wpos->depth > 0) return false;
    return (get_wt_info_at(wpos->wy, wpos->wx)->distance <= 1);
}


/*
 * Is this level close to a town?
 */
bool town_area(struct worldpos *wpos)
{
    if (wpos->depth > 0) return false;
    return (get_wt_info_at(wpos->wy, wpos->wx)->distance <= 2);
}


/*
 * Restrict to this location.
 * Returns the coordinates of this location (town, dungeon or special coordinates).
 */
struct worldpos *restrict_location(const char *location)
{
    int i, x, y;

    /* Browse the towns */
    for (i = 0; i < z_info->town_max; i++)
    {
        if (streq(towns[i].name, location))
        {
            struct worldpos* wpos = mem_zalloc(sizeof(struct worldpos));

            memcpy(wpos, &towns[i].wpos, sizeof(struct worldpos));
            return wpos;
        }
    }

    /* Browse the dungeons */
    for (i = 0; i < z_info->dungeon_max; i++)
    {
        if (streq(dungeons[i].name, location))
        {
            struct worldpos* wpos = mem_zalloc(sizeof(struct worldpos));

            memcpy(wpos, &dungeons[i].wpos, sizeof(struct worldpos));
            return wpos;
        }
    }

    /* Simply use the given coordinates */
    if (2 == sscanf(location, "%d,%d", &x, &y))
    {
        struct worldpos* wpos = mem_zalloc(sizeof(struct worldpos));

        COORDS_SET(wpos, y, x, 0);
        return wpos;
    }

    return NULL;
}


/*
 * Return the current dungeon (structure).
 */
struct location *get_dungeon(struct worldpos *wpos)
{
    int i;

    for (i = 0; i < z_info->dungeon_max; i++)
    {
        if (COORDS_EQUAL(wpos, &dungeons[i].wpos)) return &dungeons[i];
    }

    return NULL;
}


/*
 * List visited dungeons and towns in a file
 */
void dungeon_list(struct player *p, ang_file *fff)
{
    int i;
    char buf[160];

    /* Browse the towns */
    for (i = 0; i < z_info->town_max; i++)
    {
        if (wild_is_explored(p, &towns[i].wpos))
        {
            strnfmt(buf, sizeof(buf), "%s: (%d, %d)\n", towns[i].name, towns[i].wpos.wx,
                towns[i].wpos.wy);
            file_put(fff, buf);
        }
    }

    file_put(fff, "\n");

    /* Browse the dungeons */
    for (i = 0; i < z_info->dungeon_max; i++)
    {
        if (wild_is_explored(p, &dungeons[i].wpos))
        {
            strnfmt(buf, sizeof(buf), "%s: (%d, %d)\n", dungeons[i].name, dungeons[i].wpos.wx,
                dungeons[i].wpos.wy);
            file_put(fff, buf);
        }
    }
}


static int distance_from_towns(struct worldpos *wpos)
{
    int i, dist_min = -1;

    for (i = 0; i < z_info->town_max; i++)
    {
        int dist = MAX(abs(wpos->wx - towns[i].wpos.wx), abs(wpos->wy - towns[i].wpos.wy));

        if ((dist_min == -1) || (dist < dist_min)) dist_min = dist;
    }

    return dist_min;
}


/*
 * Initialize the wt_info array.
 * Note that the "generated" flag for these structures are loaded from the server savefile.
 */
void init_wild_info(void)
{
    int i, x, y;
    const char *data = wild_info->text;
    const char *t;

    /* Allocate */
    wt_info = mem_zalloc((2 * radius_wild + 1) * sizeof(struct wild_type *));
    for (i = 0; i <= 2 * radius_wild; i++)
        wt_info[i] = mem_zalloc((2 * radius_wild + 1) * sizeof(struct wild_type));

    /* Initialize */
    for (t = data, y = radius_wild; (y >= 0 - radius_wild) && *t; y--)
    {
        for (x = 0 - radius_wild; (x <= radius_wild) && *t; x++, t++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);
            struct location *dungeon;
            int size;

            /* Coordinates */
            COORDS_SET(&w_ptr->wpos, y, x, 0);

            /* Min/max depth */
            dungeon = get_dungeon(&w_ptr->wpos);
            if (dungeon)
            {
                w_ptr->min_depth = (dungeon->min_depth? dungeon->min_depth: 1);
                w_ptr->max_depth = (dungeon->max_depth? dungeon->max_depth + 1: z_info->max_depth);
            }
            else
            {
                w_ptr->min_depth = 1;
                w_ptr->max_depth = 1;
            }

            /* Chunks */
            size = w_ptr->max_depth - w_ptr->min_depth + 1;
            w_ptr->chunk_list = mem_zalloc(size * sizeof(struct chunk *));
            w_ptr->players_on_depth = mem_zalloc(size * sizeof(s16b));

            /* Type */
            w_ptr->type = WILD_UNDEFINED;
            for (i = 0; i < z_info->wf_max; i++)
            {
                if (*t == wf_info[i].symbol)
                {
                    w_ptr->type = i;
                    break;
                }
            }

            /* Distance */
            w_ptr->distance = distance_from_towns(&w_ptr->wpos);
        }
    }
}


/*
 * Free the wt_info array.
 */
void free_wild_info(void)
{
    int i, x, y;

    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            /* Caves */
            for (i = 0; i <= w_ptr->max_depth - w_ptr->min_depth; i++)
            {
                if (!w_ptr->chunk_list[i]) continue;

                /* Deallocate the level */
                wipe_mon_list(w_ptr->chunk_list[i]);
                cave_free(w_ptr->chunk_list[i]);
                w_ptr->chunk_list[i] = NULL;
            }

            mem_free(w_ptr->chunk_list);
            mem_free(w_ptr->players_on_depth);
        }
    }

    for (i = 0; i <= 2 * radius_wild; i++)
        mem_free(wt_info[i]);
    mem_free(wt_info);
}


/*
 * Add wilderness position in "03N, 12E" format to string "buf"
 */
void wild_cat_depth(struct worldpos *wpos, char *buf, int len)
{
    struct location *town = get_town(wpos);

    if (town)
        my_strcat(buf, town->name, len);
    else
    {
        my_strcat(buf, "[", len);
        if (wpos->wy)
            my_strcat(buf, format("%d%c", abs(wpos->wy), ((wpos->wy > 0)? 'N': 'S')), len);
        if (wpos->wy && wpos->wx) my_strcat(buf, ", ", len);
        if (wpos->wx)
            my_strcat(buf, format("%d%c", abs(wpos->wx), ((wpos->wx > 0)? 'E': 'W')), len);
        my_strcat(buf, "]", len);
    }
}


bool wild_is_explored(struct player *p, struct worldpos *wpos)
{
    /* Hack -- DM has knowledge of the full world */
    if (p->dm_flags & DM_SEE_LEVEL) return true;

    return (p->wild_map[radius_wild - wpos->wy][radius_wild + wpos->wx]? true: false);
}


void wild_set_explored(struct player *p, struct worldpos *wpos)
{
    p->wild_map[radius_wild - wpos->wy][radius_wild + wpos->wx] = 1;
}


void wild_deserted_message(struct player *p)
{
    /* Not in dungeons */
    if (p->wpos.depth > 0) return;

    /* Not in towns or special levels */
    if (forbid_special(&p->wpos)) return;

    /* Not on levels which contain (other) players */
    if (chunk_get_player_count(&p->wpos) > 1) return;

    /* Not on levels which contain owned houses */
    if (level_has_owned_houses(&p->wpos)) return;

    /* Add message */
    if (get_wt_info_at(p->wpos.wy, p->wpos.wx)->generated == WILD_DESERTED)
        msg(p, "This seems to be a deserted area...");
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_shallow_water(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_SHORE)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_waste(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_WASTE)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_volcano(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_VOLCANO)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_grass(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_GRASS)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_wood(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_WOOD)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_desert(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_DESERT)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_glacier(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_GLACIER)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_swamp(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_SWAMP)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Helper function for wild_add_monster
 */
static bool wild_monst_aux_hill(struct monster_race *race)
{
    if (rf_has(race->flags, RF_WILD_MOUNTAIN)) return true;
    if (rf_has(race->flags, RF_WILD_ALL)) return true;

    return false;
}


/*
 * Place a monster in the wilderness
 */
void wild_add_monster(struct player *p, struct chunk *c)
{
    int monst_x, monst_y;
    int tries = 50;
    struct monster_race *race;
    struct wild_type *w_ptr = get_wt_info_at(p->wpos.wy, p->wpos.wx);

    /* Prepare allocation table */
    switch (w_ptr->type)
    {
        case WILD_RIVER: get_mon_num_prep(wild_monst_aux_shallow_water); break;
        case WILD_WASTE: get_mon_num_prep(wild_monst_aux_waste); break;
        case WILD_VOLCANO: get_mon_num_prep(wild_monst_aux_volcano); break;
        case WILD_GRASS: get_mon_num_prep(wild_monst_aux_grass); break;
        case WILD_WOOD: get_mon_num_prep(wild_monst_aux_wood); break;
        case WILD_MOUNTAIN: return;
        case WILD_DESERT: get_mon_num_prep(wild_monst_aux_desert); break;
        case WILD_GLACIER: get_mon_num_prep(wild_monst_aux_glacier); break;
        case WILD_SWAMP: get_mon_num_prep(wild_monst_aux_swamp); break;
        case WILD_DEEPWATER: return;
        case WILD_HILL: get_mon_num_prep(wild_monst_aux_hill); break;
        case WILD_SHORE: get_mon_num_prep(wild_monst_aux_shallow_water); break;
    }

    /* Find a legal, unoccupied space */
    while (true)
    {
        tries--;

        /* Handle failure */
        if (!tries) return;

        monst_x = randint0(c->width);
        monst_y = randint0(c->height);

        /* Hack -- don't place monster in an arena */
        if (pick_arena(&c->wpos, monst_y, monst_x) != -1) continue;

        if (square_isempty(c, monst_y, monst_x)) break;
    }

    /* Get the monster */
    race = get_mon_num(c, monster_level(&p->wpos), false);

    /* Prepare allocation table */
    get_mon_num_prep(NULL);

    /* Handle failure */
    if (!race) return;

    /* Place the monster */
    place_new_monster(p, c, monst_y, monst_x, race, MON_GROUP, ORIGIN_DROP);
}


/*
 * Adds crop to a given location.
 */
void wild_add_crop(struct chunk *c, int x, int y, int type)
{
    struct object *food = object_new();

    /* Food choice */
    switch (type)
    {
        case WILD_CROP_POTATO:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Potato"), 0, RANDOMISE);
            break;

        case WILD_CROP_CABBAGE:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Head of Cabbage"), 0, RANDOMISE);
            break;

        case WILD_CROP_CARROT:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Carrot"), 0, RANDOMISE);
            break;

        case WILD_CROP_BEET:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Beet"), 0, RANDOMISE);
            break;

        case WILD_CROP_MUSHROOM:
        {
            /* Hack -- mushrooms are rare */
            static struct mushroom_crop
            {
                int tval;
                char *name;
                int chance;
            } shroom_chance[] =
            {
                {TV_MUSHROOM, "Second Sight", 2},
                {TV_MUSHROOM, "Fast Recovery", 5},
                {TV_MUSHROOM, "Vigor", 1},
                {TV_MUSHROOM, "Clear Mind", 5},
                {TV_MUSHROOM, "Emergency", 5},
                {TV_MUSHROOM, "Terror", 5},
                {TV_MUSHROOM, "Stoneskin", 5},
                {TV_MUSHROOM, "Debility", 5},
                {TV_MUSHROOM, "Sprinting", 5},
                {TV_MUSHROOM, "Purging", 5},
                {TV_FOOD, "Slime Mold", 100}
            };
            int i;

            do {i = randint0(N_ELEMENTS(shroom_chance));}
            while (!magik(shroom_chance[i].chance));

            object_prep(NULL, food,
                lookup_kind_by_name(shroom_chance[i].tval, shroom_chance[i].name), 0, RANDOMISE);
            break;
        }

        case WILD_CROP_SQUASH:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Squash"), 0, RANDOMISE);
            break;

        case WILD_CROP_CORN:
            object_prep(NULL, food, lookup_kind_by_name(TV_CROP, "Ear of Corn"), 0, RANDOMISE);
            break;
    }

    /* Drop food */
    set_origin(food, ORIGIN_FLOOR, c->wpos.depth, NULL);
    drop_near(NULL, c, &food, 0, y, x, false, DROP_FADE);
}


/*
 * Returns a random terrain feature on a given wilderness sector
 */
static byte terrain_spot(int type)
{
    int idx;

    while (true)
    {
        idx = randint0(TERRAIN_TYPE_MAX);
        if (magik(wf_info[type].chance[idx].chance)) break;
    }

    return wf_info[type].chance[idx].feat;
}


/*
 * Returns the neighbor.
 */
struct wild_type *get_neighbor(struct wild_type *origin, char dir)
{
    struct wild_type *neighbor = NULL;

    switch (dir)
    {
        case DIR_NORTH: neighbor = get_wt_info_at(origin->wpos.wy + 1, origin->wpos.wx); break;
        case DIR_EAST:  neighbor = get_wt_info_at(origin->wpos.wy, origin->wpos.wx + 1); break;
        case DIR_SOUTH: neighbor = get_wt_info_at(origin->wpos.wy - 1, origin->wpos.wx); break;
        case DIR_WEST:  neighbor = get_wt_info_at(origin->wpos.wy, origin->wpos.wx - 1); break;
    }

    /* Hack -- skip the towns */
    if (neighbor && in_town(&neighbor->wpos)) return NULL;

    return neighbor;
}


/*
 * This function takes the (x, y) level world coordinate and uses it to
 * calculate an unique index. The levels are stored in a series of "rings"
 * radiating out from the center, as shown below.
 *
 *         Indexes (-)             Ring #                         world_y
 *
 *           [05]                   [2]                            [ 2]
 *       [12][01][06]            [2][1][2]                         [ 1]
 *   [11][04][00][02][07]     [2][1][0][1][2]     world_x  [-2][-1][ 0][ 1][ 2]
 *       [10][03][08]            [2][1][2]                         [-1]
 *           [09]                   [2]                            [-2]
 *
 * PWMAngband: this uses the old "depth" algorithm, and should only be used to seed the RNG
 * using "seed_wild".
 */
int world_index(struct worldpos *wpos)
{
    int ring, base, offset, idx;

    /* Calculate which "ring" the level is in */
    ring = abs(wpos->wx) + abs(wpos->wy);
    if (!ring) return 0;

    /* Calculate the base offset of this ring */
    base = 2 * ring * (ring - 1) + 1;

    /* Calculate the offset within this ring */
    if (wpos->wx >= 0) offset = ring - wpos->wy;
    else offset = (3 * ring) + wpos->wy;

    idx = 0 - (base + offset);

    return idx;
}


/*
 * Determines whether or not to bleed from a given depth in a given direction.
 * Useful for initial determination, as well as shared bleed points.
 */
static bool should_we_bleed(struct wild_type *origin, char dir)
{
    int tmp, origin_idx, neighbor_idx;

    /* Get our neighbor */
    struct wild_type *neighbor = get_neighbor(origin, dir);

    if (!neighbor) return false;

    /* Check if our neighbor is of a different type */
    if (origin->type == neighbor->type) return false;

    origin_idx = world_index(&origin->wpos);
    neighbor_idx = world_index(&neighbor->wpos);

    /* Determine whether to bleed or not */
    Rand_value = seed_wild + (origin_idx + neighbor_idx) * 93754;
    tmp = randint0(2);
    if (tmp && (origin_idx < neighbor_idx)) return true;
    if (!tmp && (origin_idx > neighbor_idx)) return true;
    return false;
}


/*
 * Helper function to wild_gen_bleedmap
 */
static void wild_gen_bleedmap_aux(int *bleedmap, int span, char dir, int width, int height)
{
    int c = 0, above, below, noise_mag, rand_noise, bleedmag;

    /* Make a pass of the bleedmap */
    while (c < width)
    {
        /* Check that its clear */
        if (bleedmap[c] == 0xFFFF)
        {
            /* If these are aligned right, they shouldn't overflow */
            if (bleedmap[c - span] != 0xFFFF) above = bleedmap[c - span];
            else above = 0;
            if (bleedmap[c + span] != 0xFFFF) below = bleedmap[c + span];
            else below = 0;

            noise_mag = (dir % 2)? 70: 25;

            /* Randomness proportional to span */
            rand_noise = ((randint0(noise_mag * 2) - noise_mag) * span) / 64;
            bleedmag = ((above + below) / 2) + rand_noise;

            /* Bounds checking */
            if (bleedmag < 0) bleedmag = 0;
            if (bleedmag > (height - 1) / 2) bleedmag = (height - 1) / 2;

            /* Set the bleed magnitude */
            bleedmap[c] = bleedmag;
        }

        c += span;
    }

    span /= 2;

    /* Do the next level of recursion */
    if (span) wild_gen_bleedmap_aux(bleedmap, span, dir, width, height);
}


/*
 * Using a simple fractal algorithm, generates the bleedmap used by the function below.
 * Hack -- for this algorithm to work nicely, an initial span of a power of 2 is required.
 */
static void wild_gen_bleedmap(int *bleedmap, char dir, int start, int end, int width, int height)
{
    int c = 0, bound;

    /* Initialize the bleedmap */
    for (c = 0; c <= 256; c++) bleedmap[c] = 0xFFFF;

    /* Initialize the "top" and "bottom" */
    if (start < 0) bleedmap[0] = randint0(((dir % 2)? 70: 25));
    else bleedmap[0] = start;
    if (end < 0) bleedmap[256] = randint0(((dir % 2)? 70: 25));
    else
    {
        bound = (dir % 2)? height - 3: width - 3;
        for (c = bound; c <= 256; c++) bleedmap[c] = end;
    }

    /*
     * Hack -- if the start and end are zeroed, add something in the middle
     * to make exciting stuff happen.
     */
    if (!start && !end)
    {
        /* East or west */
        if (dir % 2) bleedmap[32] = randint0(40) + 15;

        /* North or south */
        else
        {
            bleedmap[64] = randint0(20) + 8;
            bleedmap[128] = randint0(20) + 8;
        }
    }

    /* Generate the bleedmap */
    wild_gen_bleedmap_aux(bleedmap, 128, dir, width, height);

    /* Hack -- no bleedmags less than 8 except near the edges */
    bound = (dir % 2)? height - 1: width - 1;

    /* Beginning to middle */
    for (c = 0; c < 8; c++)
    {
        if (bleedmap[c] < c) bleedmap[c] = c;
    }

    /* Middle */
    for (c = 8; c < bound - 8; c++)
    {
        if (bleedmap[c] < 8) bleedmap[c] = randint0(3) + 8;
    }

    /* Middle to end */
    for (c = bound - 8; c < bound; c++)
    {
        if (bleedmap[c] < bound - c) bleedmap[c] = bound - c;
    }
}


/*
 * This function "bleeds" the terrain type of bleed_from to the side of c->depth
 * specified by dir.
 *
 * First, a bleedmap array is initialized using a simple fractal algorithm.
 * This map specifies the magnitude of the bleed at each point along the edge.
 * After this, the two structures bleed_begin and bleed_end are initialized.
 *
 * After this structure is initialized, for each point along the bleed edge,
 * up until the bleedmap[point] edge of the bleed, the terrain is set to
 * that of bleed_from.
 *
 * We should hack this to add interesting features near the bleed edge.
 * Such as ponds near shoreline to make it more interesting and
 * groves of trees near the edges of forest.
 */
static void wild_bleed_level(struct chunk *c, struct wild_type *bleed_from, struct wild_type *w_ptr,
    char dir, int start, int end)
{
    int x, y;
    int bleedmap[257], *bleed_begin, *bleed_end;

    /* Hack -- bleed hills for mountain type */
    int type = ((bleed_from->type == WILD_MOUNTAIN)? WILD_HILL: bleed_from->type);

    /* Sanity check */
    if (type == w_ptr->type) return;

    memset(bleedmap, 0, 257 * sizeof(int));

    /* Generate the bleedmap */
    wild_gen_bleedmap(bleedmap, dir, start, end, c->width, c->height);

    bleed_begin = mem_zalloc(z_info->dungeon_wid * sizeof(int));
    bleed_end = mem_zalloc(z_info->dungeon_wid * sizeof(int));

    /* Initialize the bleedruns */
    switch (dir)
    {
        case DIR_EAST:
        {
            for (y = 1; y < c->height - 1; y++)
            {
                bleed_begin[y] = c->width - bleedmap[y];
                bleed_end[y] = c->width - 1;
            }
            break;
        }
        case DIR_WEST:
        {
            for (y = 1; y < c->height - 1; y++)
            {
                bleed_begin[y] = 1;
                bleed_end[y] = bleedmap[y];
            }
            break;
        }
        case DIR_NORTH:
        {
            for (x = 1; x < c->width - 1; x++)
            {
                bleed_begin[x] = 1;
                bleed_end[x] = bleedmap[x];
            }
            break;
        }
        case DIR_SOUTH:
        {
            for (x = 1; x < c->width - 1; x++)
            {
                bleed_begin[x] = c->height - bleedmap[x];
                bleed_end[x] = c->height - 1;
            }
            break;
        }
        default: break;
    }

    if ((dir == DIR_EAST) || (dir == DIR_WEST))
    {
        for (y = 1; y < c->height - 1; y++)
            for (x = bleed_begin[y]; x < bleed_end[y]; x++)
                square_set_feat(c, y, x, terrain_spot(type));
    }
    else if ((dir == DIR_NORTH) || (dir == DIR_SOUTH))
    {
        for (x = 1; x < c->width - 1; x++)
            for (y = bleed_begin[x]; y < bleed_end[x]; y++)
                square_set_feat(c, y, x, terrain_spot(type));
    }

    mem_free(bleed_begin);
    mem_free(bleed_end);
}


/*
 * To determine whether we bleed into our neighbor or whether our neighbor
 * bleeds into us, we seed the random number generator with our combined
 * depth. If the resulting number is 0, we bleed into the greater (negative
 * wise) level. Other wise we bleed into the lesser (negative wise) level.
 *
 * I added in shared points... turning this function into something extremly
 * gross. This will be extremly annoying to get working. I wish I had a simpler
 * way of doing this.
 */
static void bleed_with_neighbors(struct chunk *c)
{
    struct wild_type *w_ptr = get_wt_info_at(c->wpos.wy, c->wpos.wx);
    struct wild_type *neighbor[4];
    int dir, d, tmp, side[2], start, end, opposite;
    bool do_bleed[4], bleed_zero[4];
    int share_point[4][2];
    u32b old_seed = Rand_value;
    bool rand_old = Rand_quick;

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Get our neighbors */
    for (dir = 0; dir < 4; dir++)
        neighbor[dir] = get_neighbor(w_ptr, dir);

    /* For each neighbor, determine whether to bleed or not */
    for (dir = 0; dir < 4; dir++)
        do_bleed[dir] = should_we_bleed(w_ptr, dir);

    /* Calculate the bleed_zero values */
    for (dir = 0; dir < 4; dir++)
    {
        tmp = dir - 1;
        if (tmp < 0) tmp = 3;

        bleed_zero[dir] = false;
        if (neighbor[tmp] && neighbor[dir])
        {
            bleed_zero[dir] = true;
            if (neighbor[tmp]->type == neighbor[dir]->type)
            {
                bleed_zero[dir] = false;

                /* Calculate special case bleed zero values. */
                if (do_bleed[dir])
                {
                    /* If get the opposite direction from tmp */
                    opposite = tmp - 2;
                    if (opposite < 0) opposite += 4;

                    /* If the other one is bleeding towards us */
                    if (should_we_bleed(neighbor[tmp], opposite))
                        bleed_zero[dir] = true;

                }
                else if (do_bleed[tmp])
                {
                    /* Get the opposite direction from dir */
                    opposite = dir - 2;
                    if (opposite < 0) opposite += 4;

                    /* If the other one is bleeding towards us */
                    if (should_we_bleed(neighbor[dir], opposite))
                        bleed_zero[dir] = true;
                }
            }
        }
    }

    /* Calculate bleed shared points */
    for (dir = 0; dir < 4; dir++)
    {
        side[0] = dir - 1;
        if (side[0] < 0) side[0] = 3;
        side[1] = dir + 1;
        if (side[1] > 3) side[1] = 0;

        share_point[dir][0] = 0;
        share_point[dir][1] = 0;

        /* If this direction is bleeding */
        if (do_bleed[dir])
        {
            /* For the left and right sides */
            for (d = 0; d <= 1; d++)
            {
                /* If we have a valid neighbor */
                if (neighbor[side[d]])
                {
                    /* If our neighbor is bleeding in a similar way */
                    if (should_we_bleed(neighbor[side[d]], dir))
                    {
                        /* Are we a similar type of terrain */
                        if (neighbor[side[d]]->type == w_ptr->type)
                        {
                            int origin_idx = world_index(&w_ptr->wpos);
                            int neighbor_idx = world_index(&neighbor[side[d]]->wpos);

                            /* Seed the number generator */
                            Rand_value = seed_wild + (origin_idx + neighbor_idx) * 89791;

                            /* Share a point */
                            share_point[dir][d] = randint0(((dir % 2)? 70: 25));
                        }
                    }
                }
            }
        }
    }

    /* Do the bleeds */
    for (dir = 0; dir < 4; dir++)
    {
        tmp = dir + 1;
        if (tmp > 3) tmp = 0;
        if (do_bleed[dir])
        {
            start = 0;
            end = 0;

            if (!share_point[dir][0] && !bleed_zero[dir]) start = -1;
            else if (share_point[dir][0]) start = share_point[dir][0];

            if (!share_point[dir][1] && !bleed_zero[tmp]) end = -1;
            else if (share_point[dir][1]) end = share_point[dir][1];

            if (dir < 2)
                wild_bleed_level(c, neighbor[dir], w_ptr, dir, start, end);
            else
                wild_bleed_level(c, neighbor[dir], w_ptr, dir, end, start);
        }
    }

    /* Hack -- restore the random number generator */
    Rand_value = old_seed;
    Rand_quick = rand_old;
}


/*
 * Add a patch of grass on other terrain
 */
static void wild_add_grass_hotspot(int magnitude, int *type, bool *add_dwelling)
{
    *type = WILD_GRASS;

    /* Sometimes a dwelling */
    if (magnitude > 8)
    {
        if (magik(25)) *add_dwelling = true;
    }
}


static bool plot_clear(struct chunk *c, byte **plot, int *x1, int *y1, int *x2, int *y2)
{
    int x, y;

    /* Check if its clear */
    for (y = *y1; y <= *y2; y++)
    {
        for (x = *x1; x <= *x2; x++)
        {
            /* Don't build on other buildings or farms */
            if (square_isplot(c, y, x)) return false;

            /* Any ickiness on the plot is NOT allowed */
            if (square_isvault(c, y, x)) return false;

            /* Spaces that have already been reserved are NOT allowed */
            if (plot[y][x] == 1) return false;
        }
    }

    /* Buildings and farms can partially, but not completely, be built on water. */
    if (square_iswater(c, *y1, *x1) && square_iswater(c, *y2, *x2))
        return false;

    return true;
}


/*
 * Choose a clear building location, possibly specified by (xcen, ycen),
 * and "reserves" it so nothing else can choose any of its squares for building again.
 */
static void reserve_building_plot(struct chunk *c, byte **plot, int *x1, int *y1, int *x2, int *y2,
    int xlen, int ylen, int xcen, int ycen)
{
    int x, y, attempts = 0;

    while (attempts < 20)
    {
        /* If xcen, ycen have not been specified */
        if (!square_in_bounds_fully(c, ycen, xcen))
        {
            /* The upper left corner */
            *x1 = randint0(c->width - xlen - 4) + 2;
            *y1 = randint0(c->height - ylen - 4) + 2;

            /* The lower right corner */
            *x2 = *x1 + xlen - 1;
            *y2 = *y1 + ylen - 1;
        }
        else
        {
            *x1 = xcen - xlen / 2;
            *y1 = ycen - ylen / 2;
            *x2 = *x1 + xlen - 1;
            *y2 = *y1 + ylen - 1;
        }

        /* Add a 'border' (reserve 1 tile more than needed) */
        --*x1; --*y1; ++*x2; ++*y2;

        /* Check acquired x1, y1, x2, y2 */
        if (!square_in_bounds_fully(c, *y1, *x1) || !square_in_bounds_fully(c, *y2, *x2))
        {
            *x1 = *y1 = *x2 = *y2 = -1;
            return;
        }

        /* If we have a clear plot, reserve it and return */
        if (plot_clear(c, plot, x1, y1, x2, y2))
        {
            for (y = *y1; y <= *y2; y++)
                for (x = *x1; x <= *x2; x++)
                    plot[y][x] = 1;
            ++*x1; ++*y1; --*x2; --*y2;
            return;
        }

        attempts++;
    }

    /* Plot allocation failed */
    *x1 = *y1 = *x2 = *y2 = -1;
}


/*
 * Adds a garden at a reasonable distance from a building.
 */
static void wild_add_garden(struct chunk *c, byte **plot, int xmin, int ymin, int xmax, int ymax,
    int *x1, int *y1, int *x2, int *y2, int *type)
{
    int xlen, ylen, xcen, ycen, orientation;

    /* Choose a 'good' size for the garden */
    xlen = randint0(15) + 15;
    ylen = randint0(7) + 7;

    /* Choose a 'good' location for the garden */
    while (true)
    {
        xcen = rand_range(xmin - xlen, xmax + xlen);
        ycen = rand_range(ymin - ylen, ymax + ylen);
        if (square_in_bounds_fully(c, ycen, xcen) &&
            ((xcen < xmin - xlen / 2) || (xcen > xmax + xlen / 2) ||
            (ycen < ymin - ylen / 2) || (ycen > ymax + ylen / 2))) break;
    }

    reserve_building_plot(c, plot, x1, y1, x2, y2, xlen, ylen, xcen, ycen);

    /* If we failed to obtain a valid plot */
    if (*x1 < 0) return;

    /* Choose which type of garden it is */
    *type = randint0(7);

    /* Whether the crop rows are horizontal or vertical */
    orientation = randint0(2);

    /* Initially fill with a layer of dirt */
    fill_dirt(c, *y1, *x1, *y2, *x2);

    /* Alternating rows of crops */
    add_crop(c, *y1 + 1, *x1 + 1, *y2 - 1, *x2 - 1, orientation);
}


/*
 * Helper function for wild_furnish_dwelling
 */
static bool wild_monst_aux_home_owner(struct monster_race *race)
{
    /* Is the monster humanoid? */
    return is_humanoid(race);
}


/*
 * Helper function for wild_furnish_dwelling
 */
static bool wild_monst_aux_invaders(struct monster_race *race)
{
    /* Is the monster humanoid? */
    if (is_humanoid(race)) return true;

    /* Is the monster half-humanoid? */
    if (is_half_humanoid(race)) return true;

    return false;
}


/*
 * Adds crops to a given garden.
 */
static void wild_add_crops(struct chunk *c, int x1, int y1, int x2, int y2, int type)
{
    int x, y;

    /* Alternating rows of crops */
    for (y = y1 + 1; y <= y2 - 1; y++)
    {
        for (x = x1 + 1; x <= x2 - 1; x++)
        {
            /* Different orientations */
            if (!square_iscrop(c, y, x)) continue;

            /* Random chance of food */
            if (magik(60)) continue;

            wild_add_crop(c, x, y, type);
        }
    }
}


/*
 * Make a dwelling 'interesting'
 */
static void wild_furnish_dwelling(struct player *p, struct chunk *c, byte **plot, int x1, int y1,
    int x2, int y2)
{
    struct wild_type *w_ptr = get_wt_info_at(p->wpos.wy, p->wpos.wx);
    bool at_home = false, taken_over = false;
    int num_food = 0, cash = 0, num_objects = 0;
    int x, y, trys;
    int size = (x2 - x1) * (y2 - y1);
    int xmin = -1, ymin, xmax, ymax, type;
    u32b old_seed;
    struct object_kind *kind;
    struct monster_race *race;

    /* Is the building deserted? */
    if (magik(25)) return;

    /* Possibly add a farm */
    if (magik(50))
        wild_add_garden(c, plot, x1, y1, x2, y2, &xmin, &ymin, &xmax, &ymax, &type);

    /* Hack -- if we have created this level before, do not add anything more to it. */
    if (w_ptr->generated != WILD_NONE) return;

    /* Mark level as furnished (objects + inhabitants) */
    w_ptr->generated = WILD_FURNISHED;

    /* Save the RNG */
    old_seed = Rand_value;

    /* Is someone to be found at this house? */
    if (magik(80)) at_home = true;

    /* Taken over! */
    else if (magik(50)) taken_over = true;

    /* Is there any cash inside? */
    if (magik(50)) cash = object_level(&p->wpos) + randint0(20);

    /* Are there objects to be found? */
    if (magik(50)) num_objects = randint0(randint0(size));

    /* Is there any food inside? */
    if (magik(50)) num_food = randint0(randint0(size));

    /* Add the cash */
    if (cash)
    {
        /* Try to place the cash */
        while (trys < 50)
        {
            x = rand_range(x1, x2);
            y = rand_range(y1, y2);

            if (square_canputitem(c, y, x))
            {
                place_gold(p, c, y, x, cash, ORIGIN_FLOOR);
                break;
            }
            trys++;
        }
    }

    /* Add the objects */
    trys = 0;
    while (num_objects && (trys < 300))
    {
        x = rand_range(x1, x2);
        y = rand_range(y1, y2);

        if (square_canputitem(c, y, x))
        {
            place_object(p, c, y, x, object_level(&p->wpos), false, false, ORIGIN_FLOOR, 0);
            num_objects--;
        }
        trys++;
    }

    /* Add the food */
    trys = 0;
    while (num_food && (trys < 100))
    {
        x = rand_range(x1, x2);
        y = rand_range(y1, y2);

        if (square_canputitem(c, y, x))
        {
            struct object *food = object_new();

            if (magik(50))
                kind = lookup_kind(TV_FOOD, randint1(kb_info[TV_FOOD].num_svals));
            else
                kind = lookup_kind(TV_CROP, randint1(kb_info[TV_CROP].num_svals));
            object_prep(NULL, food, kind, 0, RANDOMISE);

            set_origin(food, ORIGIN_FLOOR, c->wpos.depth, NULL);

            drop_near(NULL, c, &food, 0, y, x, false, DROP_FADE);

            num_food--;
        }
        trys++;
    }

    /* Add the inhabitants */
    if (at_home)
    {
        int mlvl = MIN(wf_info[w_ptr->type].feat_lvl, 2 * w_ptr->distance) + 10;

        /* Determine the home owner's species */
        get_mon_num_prep(wild_monst_aux_home_owner);

        /* Home owners can be tough */
        race = get_mon_num(c, mlvl, false);

        /* Prepare allocation table */
        get_mon_num_prep(NULL);

        /* Handle failure */
        if (race)
        {
            /* Get the owner's location */
            while (true)
            {
                x = rand_range(x1 - 5, x2 + 5);
                y = rand_range(y1 - 5, y2 + 5);

                if (!square_in_bounds_fully(c, y, x)) continue;

                /* Hack -- don't place monster in an arena */
                if (pick_arena(&c->wpos, y, x) != -1) continue;

                if ((x < x1 - 1) || (x > x2 + 1) || (y < y1 - 1) || (y > y2 + 1))
                    break;
            }

            /* Place the owner */
            place_new_monster(p, c, y, x, race, 0, ORIGIN_DROP);
        }
    }

    /* Add the invaders */
    if (taken_over)
    {
        int mlvl = MIN(wf_info[w_ptr->type].feat_lvl, 2 * w_ptr->distance);

        /* Determine the invaders species */
        get_mon_num_prep(wild_monst_aux_invaders);
        race = get_mon_num(c, mlvl, false);

        /* Prepare allocation table */
        get_mon_num_prep(NULL);

        /* Handle failure */
        if (race)
        {
            /* Add the monsters */
            for (y = y1; y <= y2; y++)
            {
                for (x = x1; x <= x2; x++)
                {
                    if (magik(50)) continue;
                    place_new_monster(p, c, y, x, race, 0, ORIGIN_DROP);
                }
            }
        }
    }

    /* Restore the RNG */
    Rand_value = old_seed;

    /* No farm has been added */
    if (xmin < 0) return;

    /* Save the RNG (state should not be affected by farm generation) */
    old_seed = Rand_value;

    /* Add crops to the farm */
    wild_add_crops(c, xmin, ymin, xmax, ymax, type);

    /* Restore the RNG */
    Rand_value = old_seed;
}


/*
 * Adds a building to the wilderness. If the coordinate is not given, find it randomly.
 */
static void wild_add_dwelling(struct player *p, struct chunk *c, byte **plot, int x, int y)
{
    int h_x1, h_y1, h_x2, h_y2, p_x1, p_y1, p_x2, p_y2, plot_xlen, plot_ylen;
    int house_xlen, house_ylen, door_x, door_y, drawbridge_x[3], drawbridge_y[3];
    int tmp, type, area, price, num_door_attempts, i;
    byte door_feature, has_moat = 0;
    struct wild_type *w_ptr = get_wt_info_at(p->wpos.wy, p->wpos.wx);
    bool rand_old = Rand_quick;

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Find the dimensions of the house */

    /* PWMAngband: 50% "small" and 50% "medium" houses in immediate suburbs */
    if (town_suburb(&p->wpos))
    {
        /* Chance of being a "small" house */
        if (one_in_(2))
        {
            house_xlen = randint0(4) + 3;
            house_ylen = randint0(2) + 3;
        }

        /* A "normal" house */
        else
        {
            house_xlen = randint0(10) + 6;
            house_ylen = randint0(5) + 4;
        }
    }

    /* PWMAngband: 50% "large" and 50% "medium" houses/buildings elsewhere */
    else
    {
        /* Chance of being a "large" house */
        if (one_in_(2))
        {
            house_xlen = randint0(10) + randint0(randint0(10)) + 9;
            house_ylen = randint0(5) + randint0(randint0(5)) + 6;
        }

        /* A "normal" house */
        else
        {
            house_xlen = randint0(10) + 6;
            house_ylen = randint0(5) + 4;
        }
    }

    /* Houses are at least 2x2 */
    if (house_xlen == 3) house_xlen++;
    if (house_ylen == 3) house_ylen++;

    area = (house_xlen - 2) * (house_ylen - 2);

    /* Find the dimensions of the "lawn" the house is built on */
    if (area < 30)
    {
        plot_xlen = house_xlen;
        plot_ylen = house_ylen;
    }
    else if (area < 60)
    {
        plot_xlen = house_xlen + (area / 15) * 2;
        plot_ylen = house_ylen + (area / 25) * 2;
    }
    else
    {
        plot_xlen = house_xlen + (area / 8) * 2;
        plot_ylen = house_ylen + (area / 14) * 2;
    }

    /* Hack -- sometimes large buildings get moats */
    if ((area >= 70) && one_in_(16)) has_moat = 1;
    if ((area >= 80) && one_in_(6)) has_moat = 1;
    if ((area >= 100) && one_in_(2)) has_moat = 1;
    if ((area >= 130) && CHANCE(3, 4)) has_moat = 1;
    if (has_moat) plot_xlen += 8;
    if (has_moat) plot_ylen += 8;

    /* Hack -- one chance at an arena in desert */
    if (w_ptr->type == WILD_DESERT)
    {
        /* Ensure standard dimensions */
        house_xlen = randint0(7) + 12;
        house_ylen = randint0(5) + 8;
        area = (house_xlen - 2) * (house_ylen - 2);
        plot_xlen = house_xlen + (area / 8) * 2;
        plot_ylen = house_ylen + (area / 14) * 2;
        has_moat = 0;
    }

    /* Determine the plot's boundaries */
    reserve_building_plot(c, plot, &p_x1, &p_y1, &p_x2, &p_y2, plot_xlen, plot_ylen, x, y);

    /* Determine the building's boundaries */
    h_x1 = p_x1 + ((plot_xlen - house_xlen) / 2);
    h_y1 = p_y1 + ((plot_ylen - house_ylen) / 2);
    h_x2 = p_x2 - ((plot_xlen - house_xlen) / 2);
    h_y2 = p_y2 - ((plot_ylen - house_ylen) / 2);

    /* Return if we didn't get a plot */
    if (p_x1 < 0)
    {
        Rand_quick = rand_old;
        return;
    }

    /* Initialize x and y, which may not be specified at this point */
    x = (h_x1 + h_x2) / 2;
    y = (h_y1 + h_y2) / 2;

    /* Create log cabins by default */
    type = WILD_LOG_CABIN;

    /* Add extra houses near the towns */
    if (town_area(&p->wpos)) type = WILD_TOWN_HOME;

    /* Hack -- one chance at an arena in desert */
    if (w_ptr->type == WILD_DESERT) type = WILD_ARENA;

    /* Select the door location... */
    /* Done here so we can try to prevent it from being put on water. */
    num_door_attempts = 0;
    do
    {
        /* Pick a door direction (S,N,E,W) */
        tmp = randint0(4);

        /* Extract a "door location" */
        switch (tmp)
        {
            /* Bottom side */
            case DIR_SOUTH:
            {
                door_y = h_y2;
                door_x = rand_range(h_x1, h_x2);
                if (has_moat)
                {
                    drawbridge_y[0] = h_y2 + 1; drawbridge_y[1] = h_y2 + 2;
                    drawbridge_y[2] = h_y2 + 3;
                    drawbridge_x[0] = door_x; drawbridge_x[1] = door_x;
                    drawbridge_x[2] = door_x;
                }
                break;
            }

            /* Top side */
            case DIR_NORTH:
            {
                door_y = h_y1;
                door_x = rand_range(h_x1, h_x2);
                if (has_moat)
                {
                    drawbridge_y[0] = h_y1 - 1; drawbridge_y[1] = h_y1 - 2;
                    drawbridge_y[2] = h_y1 - 3;
                    drawbridge_x[0] = door_x; drawbridge_x[1] = door_x;
                    drawbridge_x[2] = door_x;
                }
                break;
            }

            /* Right side */
            case DIR_EAST:
            {
                door_y = rand_range(h_y1, h_y2);
                door_x = h_x2;
                if (has_moat)
                {
                    drawbridge_y[0] = door_y; drawbridge_y[1] = door_y;
                    drawbridge_y[2] = door_y;
                    drawbridge_x[0] = h_x2 + 1; drawbridge_x[1] = h_x2 + 2;
                    drawbridge_x[2] = h_x2 + 3;
                }
                break;
            }

            /* Left side */
            default:
            {
                door_y = rand_range(h_y1, h_y2);
                door_x = h_x1;
                if (has_moat)
                {
                    drawbridge_y[0] = door_y; drawbridge_y[1] = door_y;
                    drawbridge_y[2] = door_y;
                    drawbridge_x[0] = h_x1 - 1; drawbridge_x[1] = h_x1 - 2;
                    drawbridge_x[2] = h_x1 - 3;
                }
                break;
            }
        }

        /* Access the grid */
        num_door_attempts++;
    }
    while (square_iswater(c, door_y, door_x) && (num_door_attempts < 30));

    /* Build a rectangular building and make it hollow */
    door_feature = add_building(c, h_y1, h_x1, h_y2, h_x2, type);

    /* Add the door */
    square_set_feat(c, door_y, door_x, door_feature);

    /* Build the moat */
    if (has_moat)
        add_moat(c, h_y1, h_x1, h_y2, h_x2, drawbridge_y, drawbridge_x);

    /* Finish making the building */
    switch (type)
    {
        case WILD_LOG_CABIN:
        {
            /* Make the building interesting */
            wild_furnish_dwelling(p, c, plot, h_x1 + 1, h_y1 + 1, h_x2 - 1, h_y2 - 1);

            break;
        }

        case WILD_TOWN_HOME:
        {
            /* This is the dominant term for large houses */
            if (area > 40) price = (area - 40) * (area - 40) * (area - 40) * 3;
            else price = 0;

            /* This is the dominant term for medium houses */
            price += area * area * 33;

            /* This is the dominant term for small houses */
            price += area * (900 + randint0(200));

            /* Hack -- only add a house if it is not already in memory */
            i = pick_house(&p->wpos, door_y, door_x);
            if (i == -1)
            {
                struct house_type h_local;

                square_set_feat(c, door_y, door_x, door_feature);

                /* Get an empty house slot */
                i = house_add(false);

                /* Setup house info */
                h_local.x_1 = h_x1 + 1;
                h_local.y_1 = h_y1 + 1;
                h_local.x_2 = h_x2 - 1;
                h_local.y_2 = h_y2 - 1;
                h_local.door_y = door_y;
                h_local.door_x = door_x;
                memcpy(&h_local.wpos, &p->wpos, sizeof(struct worldpos));
                h_local.price = price;
                h_local.ownerid = 0;
                h_local.ownername[0] = '\0';
                h_local.color = 0;
                h_local.state = HOUSE_NORMAL;
                h_local.free = 0;

                /* Add a house to our houses list */
                house_set(i, &h_local);
            }
            else
            {
                /* Tag owned house door */
                square_set_feat(c, door_y, door_x, door_feature + house_get(i)->color);
            }

            break;
        }

        case WILD_ARENA:
        {
            /* Hack -- only add arena if it is not already in memory */
            i = pick_arena(&c->wpos, door_y, door_x);
            if (i == -1)
            {
                arenas[num_arenas].x_1 = h_x1;
                arenas[num_arenas].y_1 = h_y1;
                arenas[num_arenas].x_2 = h_x2;
                arenas[num_arenas].y_2 = h_y2;
                memcpy(&arenas[num_arenas].wpos, &c->wpos, sizeof(struct worldpos));
                num_arenas++;
            }

            break;
        }
    }

    /* Hack -- use the "complex" RNG */
    Rand_quick = rand_old;
}


/*
 * Adds hotspots.
 * Done to make the levels a bit more interesting.
 *
 * Chopiness defines the randomness of the circular shape.
 */
static void wild_add_hotspot(struct player *p, struct chunk *c, byte **plot)
{
    int x_cen, y_cen, max_mag, magnitude = 0, magsqr, chopiness, x, y;
    bool add_dwelling = false;
    int type = get_wt_info_at(p->wpos.wy, p->wpos.wx)->type;

    /* Hack -- minimum hotspot radius of 3 */
    while (magnitude < 3)
    {
        /* Determine the rough "coordinates" of the feature */
        x_cen = randint0(c->width - 11) + 5;
        y_cen = randint0(c->height - 11) + 5;

        /*
         * Determine the maximum size of the feature, which is its distance to
         * its closest edge.
         */
        max_mag = y_cen;
        if (x_cen < max_mag) max_mag = x_cen;
        if ((c->height - y_cen) < max_mag) max_mag = c->height - y_cen;
        if ((c->width - x_cen) < max_mag) max_mag = c->width - x_cen;

        /*
         * Determine the magnitude of the feature.  the triple rand is done to
         * keep most features small, but have a rare large one.
         */
        magnitude = randint0(randint0(randint0(max_mag)));
    }

    /* Hack -- take the square to avoid square roots */
    magsqr = magnitude * magnitude;

    /* The "roughness" of the hotspot */
    chopiness = 2 * magsqr / (randint0(5) + 1);

    /* Initialize the terrain type */
    switch (type)
    {
        /* River */
        case WILD_RIVER:
        {
            /* Shore or land */
            if (magik(50)) type = WILD_SHORE;
            else wild_add_grass_hotspot(magnitude, &type, &add_dwelling);

            break;
        }

        /* Waste */
        case WILD_WASTE:
        {
            /* Sometimes a scorched forest */
            if (magik(50)) type = WILD_SCORCHED;

            /* Otherwise some hills */
            else type = WILD_HILL;

            break;
        }

        /* Volcano */
        case WILD_VOLCANO:
        {
            /* Sometimes some hills */
            if (magik(50)) type = WILD_HILL;

            /* Otherwise some wasteland */
            else type = WILD_WASTE;

            break;
        }

        /* Grass */
        case WILD_GRASS:
        {
            /* Sometimes a pond */
            if (magik(50)) type = WILD_RIVER;

            /* Otherwise a glade */
            else type = WILD_WOOD;

            break;
        }

        /* Wood */
        case WILD_WOOD:
        {
            /* Sometimes a pond */
            if (magik(50)) type = WILD_RIVER;

            /* Otherwise a clearing */
            else wild_add_grass_hotspot(magnitude, &type, &add_dwelling);

            break;
        }

        /* Desert */
        case WILD_DESERT:
        {
            /* Sometimes a scorched forest */
            if (magik(50)) type = WILD_SCORCHED;

            /* Otherwise some wasteland */
            else type = WILD_WASTE;

            break;
        }

        /* Glacier */
        case WILD_GLACIER:
        {
            /* Sometimes a scorched forest */
            if (magik(50)) type = WILD_SCORCHED;

            /* Otherwise a mud pit */
            else type = WILD_MUDPIT;

            break;
        }

        /* Swamp */
        case WILD_SWAMP:
        {
            /* Sometimes a pond */
            if (magik(50)) type = WILD_RIVER;

            /* Otherwise a mud pit */
            else type = WILD_MUDPIT;

            break;
        }

        /* Hill */
        case WILD_HILL:
        {
            /* Sometimes a forest */
            if (magik(50)) type = WILD_WOOD;

            /* Otherwise some wasteland */
            else type = WILD_WASTE;

            break;
        }

        /* Shore */
        case WILD_SHORE:
        {
            /* Big pool of water or land */
            if (magik(50)) type = WILD_DEEPWATER;
            else wild_add_grass_hotspot(magnitude, &type, &add_dwelling);

            break;
        }
    }

    /* Create the hotspot */
    for (y = y_cen - magnitude; y <= y_cen + magnitude; y++)
    {
        for (x = x_cen - magnitude; x <= x_cen + magnitude; x++)
        {
            /* a^2 + b^2 = c^2... the rand makes the edge less defined */
            /* Hack -- multiply the y's by 4 to "squash" the shape */
            if (((x - x_cen) * (x - x_cen) + (y - y_cen) * (y - y_cen) * 4) <
                (magsqr + randint0(chopiness)))
            {
                square_set_feat(c, y, x, terrain_spot(type));
            }
        }
    }

    /* Add inhabitants */
    if (add_dwelling) wild_add_dwelling(p, c, plot, x_cen, y_cen);
}


static void wilderness_gen_basic(struct chunk *c)
{
    int y, x;
    struct wild_type *w_ptr = get_wt_info_at(c->wpos.wy, c->wpos.wx);

    for (y = 1; y < c->height - 1; y++)
    {
        for (x = 1; x < c->width - 1; x++)
        {
            int type = w_ptr->type;

            /* Hack -- surround mountain type with hills to keep a passable border */
            if ((y == 1) || (y == c->height - 2) || (x == 1) || (x == c->width - 2))
            {
                if (type == WILD_MOUNTAIN) type = WILD_HILL;
            }

            square_set_feat(c, y, x, terrain_spot(type));
        }
    }
}


/*
 * Generate the wilderness for the first time
 *
 * p is the player
 * c is the current chunk
 */
static void wilderness_gen_layout(struct player *p, struct chunk *c)
{
    int y, x1, x2, y1, y2;
    u32b tmp_seed = Rand_value;
    bool rand_old = Rand_quick;
    struct wild_type *w_ptr = get_wt_info_at(p->wpos.wy, p->wpos.wx);
    int dwelling = 0;
    byte **plot;
    bool add_park = false;

    plot = mem_zalloc(c->height * sizeof(byte *));
    for (y = 0; y < c->height; y++)
        plot[y] = mem_zalloc(c->width * sizeof(byte));

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Hack -- induce consistant wilderness */
    Rand_value = seed_wild + world_index(&c->wpos) * 600;

    /* Create boundary */
    draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_PERM_CLEAR, SQUARE_NONE);

    /* Hack -- start with basic floors */
    wilderness_gen_basic(c);

    /* To make the borders between wilderness levels more seamless, "bleed" the levels together */
    bleed_with_neighbors(c);

    /* Hack -- reseed, just to make sure everything stays consistent. */
    Rand_value = seed_wild + world_index(&c->wpos) * 287 + 490836;

    /* To make the level more interesting, add some "hotspots" */
    /* Only if not close to towns to preserve houses */
    if (!town_area(&p->wpos))
    {
        for (y = 0; y < randint0(11); y++) wild_add_hotspot(p, c, plot);
    }

    /* Add some dwellings */
    switch (w_ptr->type)
    {
        /* Add dwellings on grass */
        case WILD_GRASS:
        {
            dwelling = 250;

            /* Hack -- if close to the towns, make dwellings more likely */
            if (w_ptr->distance == 1)
            {
                dwelling *= 80;
                add_park = true;
            }
            if (w_ptr->distance == 2) dwelling *= 25;
            if (w_ptr->distance == 3) dwelling *= 8;
            if (w_ptr->distance == 4) dwelling *= 2;

            break;
        }

        /* Allow houses on waste and glacier terrains (for base town) */
        case WILD_WASTE:
        case WILD_GLACIER:
        {
            if (w_ptr->distance == 1)
            {
                dwelling = 20000;
                add_park = true;
            }
            if (w_ptr->distance == 2) dwelling = 6250;

            break;
        }

        /* Hack -- one chance at an arena in desert */
        case WILD_DESERT:
        {
            if (num_arenas < MAX_ARENAS) dwelling = 50;

            break;
        }
    }

    /*
     * Hack -- 50% of the time in immediate suburbs there will be a "park" which will make
     * the rest of the level more densely packed together
     */
    if (add_park && one_in_(2))
    {
        reserve_building_plot(c, plot, &x1, &y1, &x2, &y2, randint0(30) + 15, randint0(20) + 10,
            -1, -1);
    }

    /* Add wilderness dwellings */
    while (dwelling > 0)
    {
        /* Hack -- the number of dwellings is proportional to their chance of existing */
        if (CHANCE(dwelling, 1000)) wild_add_dwelling(p, c, plot, -1, -1);
        dwelling -= 50;
    }

    /* For dungeon base levels, add down stairs */
    if (get_dungeon(&w_ptr->wpos) != NULL) add_down_stairs(c);

    /* Hack -- use the "complex" RNG */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;

    for (y = 0; y < c->height; y++)
        mem_free(plot[y]);
    mem_free(plot);
}


/*
 * Information for town parsing.
 */
struct parse_town
{
    int special_feat[2];    /* Special terrain features */
    int chance;             /* Chance of generating the second feature */
    char *map;              /* Symbols */
    char *mask;             /* Special symbols */
};


static enum parser_error parse_town_special(struct parser *p)
{
    struct parse_town *t = mem_zalloc(sizeof(*t));

    t->special_feat[0] = lookup_feat(parser_getsym(p, "i1"));
    t->special_feat[1] = lookup_feat(parser_getsym(p, "i2"));
    t->chance = parser_getuint(p, "chance");
    parser_setpriv(p, t);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_town_map(struct parser *p)
{
    struct parse_town *t = parser_priv(p);
    const char *map;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    map = parser_getstr(p, "text");
    if (strlen(map) != (size_t)z_info->dungeon_wid) return PARSE_ERROR_INVALID_VALUE;
    t->map = string_append(t->map, map);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_town_mask(struct parser *p)
{
    struct parse_town *t = parser_priv(p);
    const char *mask;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    mask = parser_getstr(p, "text");
    if (strlen(mask) != (size_t)z_info->dungeon_wid) return PARSE_ERROR_INVALID_VALUE;
    t->mask = string_append(t->mask, mask);

    return PARSE_ERROR_NONE;
}


void get_town_file(char *buf, size_t len, const char *name)
{
    char *str;

    strnfmt(buf, len, "town_%s", name);
    for (str = buf; *str; str++)
    {
        /* Lowercase our string */
        *str = tolower((unsigned char)*str);

        /* Replace spaces with underscores */
        if (*str == ' ') *str = '_';
    }
}


/*
 * Generate wilderness towns for the first time
 *
 * c is the current chunk
 */
static void wild_town_gen_layout(struct chunk *c)
{
    /* Get town info */
    struct location *town = get_town(&c->wpos);

    /* Parse town file */
    struct parser *p = parser_new();
    errr r;
    char town_file[20];
    struct parse_town *helper;
    const char *sym, *spec;
    int i, x, y;

    u32b tmp_seed = Rand_value;
    bool rand_old = Rand_quick;

    parser_setpriv(p, NULL);
    parser_reg(p, "feat sym i1 sym i2 uint chance", parse_town_special);
    parser_reg(p, "map str text", parse_town_map);
    parser_reg(p, "mask str text", parse_town_mask);
    if (!p) quit_fmt("Cannot initialize town of %s.", town->name);
    get_town_file(town_file, sizeof(town_file), town->name);
    r = parse_file_quit_not_found(p, town_file);
    if (r) quit_fmt("Cannot initialize town of %s.", town->name);
    helper = parser_priv(p);
    parser_destroy(p);

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Hack -- induce consistant town */
    Rand_value = seed_wild + world_index(&c->wpos) * 600;

    /* Initialize */
    sym = helper->map;
    spec = helper->mask;
    for (y = 0; (y < z_info->dungeon_hgt) && *sym && *spec; y++)
    {
        for (x = 0; (x < z_info->dungeon_wid) && *sym && *spec; x++, sym++, spec++)
        {
            int feat = 0;

            for (i = 0; i < z_info->tf_max; i++)
            {
                if ((*sym == tf_info[i].symbol) && (*spec == tf_info[i].special))
                {
                    feat = tf_info[i].feat_idx;
                    break;
                }
            }

            /* Hack -- special feat */
            if (feat == 0)
            {
                feat = helper->special_feat[0];
                if ((y > 1) && (y < z_info->dungeon_hgt - 2) &&
                    (x > 1) && (x < z_info->dungeon_wid - 2) && magik(helper->chance))
                {
                    feat = helper->special_feat[1];
                }
            }

            /* Hack -- stairs */
            if (feat == FEAT_MORE)
            {
                /* Place a staircase */
                square_set_downstairs(c, y, x);

                /* Hack -- the players start on the stairs while recalling */
                square_set_join_rand(c, y, x);
            }

            /* Hack -- safe floor */
            else if (feat == FEAT_FLOOR_SAFE)
            {
                /* Create the tavern, make it PvP-safe */
                square_add_safe(c, y, x);

                /* Declare this to be a room */
                sqinfo_on(c->squares[y][x].info, SQUARE_GLOW);
                sqinfo_on(c->squares[y][x].info, SQUARE_VAULT);
                sqinfo_on(c->squares[y][x].info, SQUARE_ROOM);

                /* Hack -- have everyone start in the tavern */
                if (*sym == 'x') square_set_join_down(c, y, x);
            }

            else
                square_set_feat(c, y, x, feat);
        }
    }

    /* Cleanup */
    if (helper)
    {
        string_free(helper->map);
        string_free(helper->mask);
        mem_free(helper);
    }

    /* Create boundary */
    draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_PERM_CLEAR, SQUARE_NONE);

    /* Hack -- use the "complex" RNG */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;
}


/*
 * Main function for generation of wilderness levels.
 *
 * p is the player
 * wpos is the position on the world map
 *
 * Returns a pointer to the generated chunk.
 *
 * Hack -- this function also generates all other towns except starting and base towns,
 * which use their own cave profiles.
 */
struct chunk *wilderness_gen(struct player *p, struct worldpos *wpos, int min_height, int min_width)
{
    int i, residents;

    /* Make a new chunk */
    struct chunk *c = cave_new(z_info->dungeon_hgt, z_info->dungeon_wid);

    memcpy(&c->wpos, wpos, sizeof(struct worldpos));

    /* Towns */
    if (in_town(wpos))
    {
        residents = (is_daytime()? z_info->town_monsters_day: z_info->town_monsters_night);

        player_cave_new(p, z_info->dungeon_hgt, z_info->dungeon_wid);

        /* Build stuff */
        wild_town_gen_layout(c);

        /* Apply illumination */
        player_cave_clear(p, true);
        cave_illuminate(p, c, is_daytime());

        /* Make some residents */
        for (i = 0; i < residents; i++)
            pick_and_place_distant_monster(p, c, 0, MON_ASLEEP);
    }

    /* Wilderness levels */
    else
    {
        struct wild_type *w_ptr = get_wt_info_at(wpos->wy, wpos->wx);
        enum wild_gen state = w_ptr->generated;

        residents = object_level(wpos);

        player_cave_new(p, z_info->dungeon_hgt, z_info->dungeon_wid);

        /* Build stuff */
        wilderness_gen_layout(p, c);

        /* Apply illumination */
        player_cave_clear(p, true);
        cave_illuminate(p, c, is_daytime());

        /* Make some residents */
        for (i = 0; i < residents; i++)
            wild_add_monster(p, c);

        /* Mark levels without dwellings as generated */
        if (w_ptr->generated == WILD_NONE) w_ptr->generated = WILD_GENERATED;

        /* Mark regenerated levels with dwellings as deserted */
        if (state == WILD_FURNISHED) w_ptr->generated = WILD_DESERTED;
    }

    return c;
}


void wilderness_gen_basic_layout(struct chunk *c)
{
    u32b tmp_seed = Rand_value;
    bool rand_old = Rand_quick;

    /* Hack -- use the "simple" RNG */
    Rand_quick = true;

    /* Hack -- induce consistant wilderness */
    Rand_value = seed_wild + world_index(&c->wpos) * 600;

    /* Create boundary */
    draw_rectangle(c, 0, 0, c->height - 1, c->width - 1, FEAT_PERM_CLEAR, SQUARE_NONE);

    /* Hack -- start with basic floors */
    wilderness_gen_basic(c);

    /* To make the borders between wilderness levels more seamless, "bleed" the levels together */
    bleed_with_neighbors(c);

    /* Hack -- use the "complex" RNG */
    Rand_value = tmp_seed;
    Rand_quick = rand_old;
}
