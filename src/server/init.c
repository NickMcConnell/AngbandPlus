/*
 * File: init.c
 * Purpose: Various game initialisation routines
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
#include "../common/parser.h"
#include "../common/randname.h"
#include "../common/tvalsval.h"
#include "files.h"
#include "generate.h"
#include "init.h"
#include "monster/mon-init.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "object/slays.h"
#include "wilderness.h"


static struct history_chart *histories;


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.  Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "lib/edit" directory.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass.  Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 */


static const char *k_info_flags[] =
{
    #define OF(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) #a,
    #include "../common/list-object-flags.h"
    #undef OF
    NULL
};


static const char *effect_list[] = {
    #define EFFECT(x, y, r, z) #x,
    #include "list-effects.h"
    #undef EFFECT
    NULL
};


static u32b grab_one_effect(const char *what)
{
    size_t i;

    /* Scan activations */
    for (i = 0; i < N_ELEMENTS(effect_list); i++)
    {
        if (effect_list[i] && streq(what, effect_list[i])) return i;
    }

    /* Oops */
    plog_fmt("Unknown effect '%s'.", what);

    /* Error */
    return 0;
}


static struct history_chart *findchart(struct history_chart *hs, unsigned int idx)
{
    for (; hs; hs = hs->next)
    {
        if (hs->idx == idx) break;
    }
    return hs;
}


static void free_file_paths()
{
    /* Free the sub-paths */
    string_free(ANGBAND_DIR_APEX);
    string_free(ANGBAND_DIR_EDIT);
    string_free(ANGBAND_DIR_FILE);
    string_free(ANGBAND_DIR_HELP);
    string_free(ANGBAND_DIR_SAVE);
    string_free(ANGBAND_DIR_USER);
    string_free(ANGBAND_DIR_PREF);
}


/*
 * Find the default paths to all of our important sub-directories.
 *
 * All of the sub-directories should, by default, be located inside
 * the main directory, whose location is very system dependant. (On multi-
 * user systems such as Linux this is not the default - see config.h for
 * more info.)
 *
 * This function takes a writable buffer, initially containing the
 * "path" to the "config", "lib" and "data" directories, for example,
 * "/etc/angband/", "/usr/share/angband" and "/var/games/angband" -
 * or a system dependant string, for example, ":lib:".  The buffer
 * must be large enough to contain at least 32 more characters.
 *
 * Various command line options may allow some of the important
 * directories to be changed to user-specified directories, most
 * importantly, the "apex" and "user" and "save" directories,
 * but this is done after this function, see "main.c".
 *
 * In general, the initial path should end in the appropriate "PATH_SEP"
 * string.  All of the "sub-directory" paths (created below or supplied
 * by the user) will NOT end in the "PATH_SEP" string, see the special
 * "path_build()" function in "util.c" for more information.
 *
 * Hack -- first we free all the strings, since this is known
 * to succeed even if the strings have not been allocated yet,
 * as long as the variables start out as "NULL".  This allows
 * this function to be called multiple times, for example, to
 * try several base "path" values until a good one is found.
 */
void init_file_paths(const char *configpath, const char *libpath, const char *datapath)
{
    /*** Free everything ***/

    free_file_paths();

    /*** Prepare the "path" ***/

    /* Build path names */
    ANGBAND_DIR_APEX = string_make(format("%sapex", datapath));
    ANGBAND_DIR_EDIT = string_make(format("%sedit", configpath));
    ANGBAND_DIR_FILE = string_make(format("%sfile", libpath));
    ANGBAND_DIR_HELP = string_make(format("%shelp", libpath));
    ANGBAND_DIR_SAVE = string_make(format("%ssave", datapath));
    ANGBAND_DIR_USER = string_make(format("%suser", datapath));
    ANGBAND_DIR_PREF = string_make(format("%spref", configpath));
}


/*
 * Create any missing directories.
 *
 * We create only those dirs which may be empty. The others are assumed
 * to contain required files and therefore must exist at startup.
 */
void create_needed_dirs(void)
{
    char dirpath[MSG_LEN];

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_APEX, "");
    if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);
}


/* Parsing functions for limits.txt */
static enum parser_error parse_z(struct parser *p)
{
    maxima *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "O"))
        z->o_max = value;
    else if (streq(label, "M"))
        z->m_max = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return 0;
}


static struct parser *init_parse_z(void)
{
    struct maxima *z = mem_zalloc(sizeof(*z));
    struct parser *p = parser_new();

    parser_setpriv(p, z);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "M sym label int value", parse_z);

    return p;
}


static errr run_parse_z(struct parser *p)
{
    return parse_file(p, "limits");
}


static errr finish_parse_z(struct parser *p)
{
    z_info = parser_priv(p);
    parser_destroy(p);

    return 0;
}


static void cleanup_z(void)
{
    mem_free(z_info);
}


static struct file_parser z_parser =
{
    "limits",
    init_parse_z,
    run_parse_z,
    finish_parse_z,
    cleanup_z
};


/* Parsing functions for object_base.txt */
struct kb_parsedata
{
    object_base defaults;
    object_base *kb;
};


static enum parser_error parse_kb_d(struct parser *p)
{
    const char *label;
    int value;
    struct kb_parsedata *d = parser_priv(p);

    my_assert(d);

    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (streq(label, "B"))
        d->defaults.break_perc = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_kb_n(struct parser *p)
{
    struct object_base *kb;
    struct kb_parsedata *d = parser_priv(p);

    my_assert(d);

    kb = mem_alloc(sizeof(*kb));
    memcpy(kb, &d->defaults, sizeof(*kb));
    kb->next = d->kb;
    d->kb = kb;

    kb->tval = tval_find_idx(parser_getsym(p, "tval"));
    if (kb->tval == -1) return PARSE_ERROR_UNRECOGNISED_TVAL;

    kb->name = string_make(parser_getstr(p, "name"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_kb_b(struct parser *p)
{
    struct object_base *kb;
    struct kb_parsedata *d = parser_priv(p);

    my_assert(d);

    kb = d->kb;
    my_assert(kb);

    kb->break_perc = parser_getint(p, "breakage");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_kb_f(struct parser *p)
{
    struct object_base *kb;
    char *s, *t;
    struct kb_parsedata *d = parser_priv(p);

    my_assert(d);

    kb = d->kb;
    my_assert(kb);

    s = string_make(parser_getstr(p, "flags"));
    t = strtok(s, " |");
    while (t)
    {
        if (grab_flag(kb->flags, OF_SIZE, k_info_flags, t)) break;
        t = strtok(NULL, " |");
    }
    mem_free(s);

    return (t? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static struct parser *init_parse_kb(void)
{
    struct parser *p = parser_new();
    struct kb_parsedata *d = mem_zalloc(sizeof(*d));

    parser_setpriv(p, d);

    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "D sym label int value", parse_kb_d);
    parser_reg(p, "N sym tval str name", parse_kb_n);
    parser_reg(p, "B int breakage", parse_kb_b);
    parser_reg(p, "F str flags", parse_kb_f);

    return p;
}


static errr run_parse_kb(struct parser *p)
{
    return parse_file(p, "object_base");
}


static errr finish_parse_kb(struct parser *p)
{
    struct object_base *kb;
    struct object_base *next = NULL;
    struct kb_parsedata *d = parser_priv(p);

    my_assert(d);

    kb_info = mem_zalloc(TV_MAX * sizeof(*kb_info));

    for (kb = d->kb; kb; kb = next)
    {
        if (kb->tval >= TV_MAX) continue;
        memcpy(&kb_info[kb->tval], kb, sizeof(*kb));
        next = kb->next;
        mem_free(kb);
    }

    mem_free(d);
    parser_destroy(p);
    return 0;
}


static void cleanup_kb(void)
{
    int i;

    /* Paranoia */
    if (!kb_info) return;

    for (i = 0; i < TV_MAX; i++) string_free(kb_info[i].name);
    mem_free(kb_info);
}


static struct file_parser kb_parser =
{
    "object_base",
    init_parse_kb,
    run_parse_kb,
    finish_parse_kb,
    cleanup_kb
};


/* Parsing functions for object.txt */
static enum parser_error parse_k_n(struct parser *p)
{
    int idx = parser_getint(p, "index");
    const char *name = parser_getstr(p, "name");
    struct object_kind *h = parser_priv(p);
    struct object_kind *k = mem_zalloc(sizeof(*k));

    k->next = h;
    parser_setpriv(p, k);
    k->kidx = idx;
    k->name = string_make(name);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_g(struct parser *p)
{
    char glyph = parser_getchar(p, "glyph");
    const char *color = parser_getsym(p, "color");
    struct object_kind *k = parser_priv(p);

    my_assert(k);
    k->d_char = glyph;
    if (strlen(color) > 1)
        k->d_attr = color_text_to_attr(color);
    else
        k->d_attr = color_char_to_attr(color[0]);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_i(struct parser *p)
{
    struct object_kind *k = parser_priv(p);
    int tval;

    my_assert(k);
    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    k->tval = tval;
    k->sval = parser_getint(p, "sval");

    k->base = &kb_info[k->tval];

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_w(struct parser *p)
{
    struct object_kind *k = parser_priv(p);

    my_assert(k);
    k->level = parser_getint(p, "level");
    k->weight = parser_getint(p, "weight");
    k->cost = parser_getint(p, "cost");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_a(struct parser *p)
{
    struct object_kind *k = parser_priv(p);
    const char *tmp = parser_getstr(p, "minmax");
    int amin, amax;

    my_assert(k);
    k->alloc_prob = parser_getint(p, "common");
    if (sscanf(tmp, "%d to %d", &amin, &amax) != 2) return PARSE_ERROR_GENERIC;

    k->alloc_min = amin;
    k->alloc_max = amax;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_p(struct parser *p)
{
    struct object_kind *k = parser_priv(p);
    struct random hd = parser_getrand(p, "hd");

    my_assert(k);
    k->ac = parser_getint(p, "ac");
    k->dd = hd.dice;
    k->ds = hd.sides;
    k->to_h = parser_getrand(p, "to-h");
    k->to_d = parser_getrand(p, "to-d");
    k->to_a = parser_getrand(p, "to-a");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_c(struct parser *p)
{
    struct object_kind *k = parser_priv(p);

    my_assert(k);
    k->charge = parser_getrand(p, "charges");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_m(struct parser *p)
{
    struct object_kind *k = parser_priv(p);

    my_assert(k);
    k->gen_mult_prob = parser_getint(p, "prob");
    k->stack_size = parser_getrand(p, "stack");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_f(struct parser *p)
{
    struct object_kind *k = parser_priv(p);
    char *s = string_make(parser_getstr(p, "flags"));
    char *t;

    my_assert(k);
    t = strtok(s, " |");
    while (t)
    {
        if (grab_flag(k->flags, OF_SIZE, k_info_flags, t)) break;
        t = strtok(NULL, " |");
    }
    string_free(s);

    return (t? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_k_e(struct parser *p)
{
    struct object_kind *k = parser_priv(p);

    my_assert(k);
    k->effect = grab_one_effect(parser_getsym(p, "name"));
    if (parser_hasval(p, "time")) k->time = parser_getrand(p, "time");
    if (!k->effect) return PARSE_ERROR_GENERIC;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_k_l(struct parser *p)
{
    struct object_kind *k = parser_priv(p);
    char *s;
    char *t;

    my_assert(k);
    k->pval[k->num_pvals] = parser_getrand(p, "pval");

    if (!parser_hasval(p, "flags"))
    {
        k->num_pvals++;
        return PARSE_ERROR_NONE;
    }

    s = string_make(parser_getstr(p, "flags"));
    t = strtok(s, " |");

    while (t)
    {
        if (grab_flag(k->flags, OF_SIZE, k_info_flags, t) ||
            grab_flag(k->pval_flags[k->num_pvals], OF_SIZE, k_info_flags, t))
                break;
        t = strtok(NULL, " |");
    }

    k->num_pvals++;
    if (k->num_pvals > MAX_PVALS) return PARSE_ERROR_TOO_MANY_ENTRIES;

    string_free(s);
    return (t? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_k_d(struct parser *p)
{
    struct object_kind *k = parser_priv(p);

    my_assert(k);
    k->text = string_append(k->text, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_k(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N int index str name", parse_k_n);
    parser_reg(p, "G char glyph sym color", parse_k_g);
    parser_reg(p, "I sym tval int sval", parse_k_i);
    parser_reg(p, "W int level int extra int weight int cost", parse_k_w);
    parser_reg(p, "A int common str minmax", parse_k_a);
    parser_reg(p, "P int ac rand hd rand to-h rand to-d rand to-a", parse_k_p);
    parser_reg(p, "C rand charges", parse_k_c);
    parser_reg(p, "M int prob rand stack", parse_k_m);
    parser_reg(p, "F str flags", parse_k_f);
    parser_reg(p, "E sym name ?rand time", parse_k_e);
    parser_reg(p, "L rand pval ?str flags", parse_k_l);
    parser_reg(p, "D str text", parse_k_d);

    return p;
}


static errr run_parse_k(struct parser *p)
{
    return parse_file(p, "object");
}


static errr finish_parse_k(struct parser *p)
{
    struct object_kind *k, *next = NULL;

    /* Scan the list for the max id */
    z_info->k_max = 0;
    k = parser_priv(p);
    while (k)
    {
        if (k->kidx > z_info->k_max) z_info->k_max = k->kidx;
        k = k->next;
    }
    z_info->k_max++;

    /* Allocate the direct access list and copy the data to it */
    k_info = mem_zalloc(z_info->k_max * sizeof(*k));
    for (k = parser_priv(p); k; k = next)
    {
        memcpy(&k_info[k->kidx], k, sizeof(*k));
        next = k->next;
        if (next) k_info[k->kidx].next = &k_info[next->kidx];
        else k_info[k->kidx].next = NULL;
        mem_free(k);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_k(void)
{
    int i;

    /* Paranoia */
    if (!k_info) return;

    for (i = 0; i < z_info->k_max; i++)
    {
        string_free(k_info[i].name);
        string_free(k_info[i].text);
    }
    mem_free(k_info);
}


static struct file_parser k_parser =
{
    "object",
    init_parse_k,
    run_parse_k,
    finish_parse_k,
    cleanup_k
};


/* Parsing functions for artifact.txt */
static enum parser_error parse_a_n(struct parser *p)
{
    bitflag f[OF_SIZE];
    int idx = parser_getint(p, "index");
    const char *name = parser_getstr(p, "name");
    struct artifact *h = parser_priv(p);
    struct artifact *a = mem_zalloc(sizeof(*a));

    a->next = h;
    parser_setpriv(p, a);
    a->aidx = idx;
    a->name = string_make(name);

    /* Ignore all elements */
    create_mask(f, FALSE, OFT_IGNORE, OFT_MAX);
    of_union(a->flags, f);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_a_i(struct parser *p)
{
    struct artifact *a = parser_priv(p);
    int tval, sval;

    my_assert(a);
    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;
    a->tval = tval;

    sval = lookup_sval(a->tval, parser_getsym(p, "sval"));
    if (sval < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;
    a->sval = sval;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_a_w(struct parser *p)
{
    struct artifact *a = parser_priv(p);

    my_assert(a);
    a->level = parser_getint(p, "level");
    a->weight = parser_getint(p, "weight");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_a_a(struct parser *p)
{
    struct artifact *a = parser_priv(p);
    const char *tmp = parser_getstr(p, "minmax");
    int amin, amax;

    my_assert(a);
    a->alloc_prob = parser_getint(p, "common");
    if (sscanf(tmp, "%d to %d", &amin, &amax) != 2) return PARSE_ERROR_GENERIC;

    if ((amin > 255) || (amax > 255) || (amin < 0) || (amax < 0)) return PARSE_ERROR_OUT_OF_BOUNDS;

    a->alloc_min = amin;
    a->alloc_max = amax;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_a_p(struct parser *p)
{
    struct artifact *a = parser_priv(p);
    struct random hd = parser_getrand(p, "hd");

    my_assert(a);
    a->ac = parser_getint(p, "ac");
    a->dd = hd.dice;
    a->ds = hd.sides;
    a->to_h = parser_getint(p, "to-h");
    a->to_d = parser_getint(p, "to-d");
    a->to_a = parser_getint(p, "to-a");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_a_f(struct parser *p)
{
    struct artifact *a = parser_priv(p);
    char *s;
    char *t;

    my_assert(a);
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    s = string_make(parser_getstr(p, "flags"));

    t = strtok(s, " |");
    while (t)
    {
        if (grab_flag(a->flags, OF_SIZE, k_info_flags, t)) break;
        t = strtok(NULL, " |");
    }
    string_free(s);

    return (t? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_a_e(struct parser *p)
{
    struct artifact *a = parser_priv(p);

    my_assert(a);
    a->effect = grab_one_effect(parser_getsym(p, "name"));
    a->time = parser_getrand(p, "time");
    if (!a->effect) return PARSE_ERROR_GENERIC;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_a_m(struct parser *p)
{
    struct artifact *a = parser_priv(p);

    my_assert(a);
    a->effect_msg = string_append(a->effect_msg, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_a_l(struct parser *p)
{
    struct artifact *a = parser_priv(p);
    char *s;
    char *t;

    my_assert(a);
    a->pval[a->num_pvals] = parser_getint(p, "pval");

    if (!parser_hasval(p, "flags")) return PARSE_ERROR_MISSING_FIELD;

    s = string_make(parser_getstr(p, "flags"));
    t = strtok(s, " |");

    while (t)
    {
        if (grab_flag(a->flags, OF_SIZE, k_info_flags, t) ||
            grab_flag(a->pval_flags[a->num_pvals], OF_SIZE, k_info_flags, t))
                break;
        t = strtok(NULL, " |");
    }

    a->num_pvals++;
    if (a->num_pvals > MAX_PVALS) return PARSE_ERROR_TOO_MANY_ENTRIES;

    string_free(s);
    return (t? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_a_d(struct parser *p)
{
    struct artifact *a = parser_priv(p);

    my_assert(a);
    a->text = string_append(a->text, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_a(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N int index str name", parse_a_n);
    parser_reg(p, "I sym tval sym sval", parse_a_i);
    parser_reg(p, "W int level int rarity int weight int cost", parse_a_w);
    parser_reg(p, "A int common str minmax", parse_a_a);
    parser_reg(p, "P int ac rand hd int to-h int to-d int to-a", parse_a_p);
    parser_reg(p, "F ?str flags", parse_a_f);
    parser_reg(p, "E sym name rand time", parse_a_e);
    parser_reg(p, "M str text", parse_a_m);
    parser_reg(p, "L int pval str flags", parse_a_l);
    parser_reg(p, "D str text", parse_a_d);
    return p;
}


static errr run_parse_a(struct parser *p)
{
    return parse_file(p, "artifact");
}


static errr finish_parse_a(struct parser *p)
{
    struct artifact *a, *n;
    int i;

    /* Scan the list for the max id */
    z_info->a_max = 0;
    a = parser_priv(p);
    while (a)
    {
        if (a->aidx > z_info->a_max) z_info->a_max = a->aidx;
        a = a->next;
    }
    z_info->a_max++;

    /* Allocate the direct access list and copy the data to it */
    a_info = mem_zalloc((z_info->a_max + 9) * sizeof(*a));
    for (a = parser_priv(p); a; a = n)
    {
        memcpy(&a_info[a->aidx], a, sizeof(*a));
        n = a->next;
        if (n) a_info[a->aidx].next = &a_info[n->aidx];
        else a_info[a->aidx].next = NULL;
        mem_free(a);
    }

    /* Hack -- Create 9 empty shelves for Rings of Power */
    for (i = z_info->a_max; i < z_info->a_max + 9; i++) a_info[i].aidx = i;

    parser_destroy(p);
    return 0;
}


static void cleanup_a(void)
{
    int i;

    /* Paranoia */
    if (!a_info) return;

    for (i = 0; i < z_info->a_max; i++)
    {
        string_free(a_info[i].name);
        string_free(a_info[i].text);
        string_free(a_info[i].effect_msg);
    }
    mem_free(a_info);
}


static struct file_parser a_parser =
{
    "artifact",
    init_parse_a,
    run_parse_a,
    finish_parse_a,
    cleanup_a
};


/* Parsing functions for names.txt (random name fragments) */
struct name
{
    struct name *next;
    char *str;
};


struct names_parse
{
    unsigned int section;
    unsigned int nnames[RANDNAME_NUM_TYPES];
    struct name *names[RANDNAME_NUM_TYPES];
};


static enum parser_error parse_names_n(struct parser *p)
{
    unsigned int section = parser_getint(p, "section");
    struct names_parse *s = parser_priv(p);

    if (s->section >= RANDNAME_NUM_TYPES) return PARSE_ERROR_GENERIC;
    s->section = section;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_names_d(struct parser *p)
{
    const char *name = parser_getstr(p, "name");
    struct names_parse *s = parser_priv(p);
    struct name *ns = mem_zalloc(sizeof(*ns));

    s->nnames[s->section]++;
    ns->next = s->names[s->section];
    ns->str = string_make(name);
    s->names[s->section] = ns;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_names(void)
{
    struct parser *p = parser_new();
    struct names_parse *n = mem_zalloc(sizeof(*n));

    n->section = 0;
    parser_setpriv(p, n);
    parser_reg(p, "N int section", parse_names_n);
    parser_reg(p, "D str name", parse_names_d);

    return p;
}


static errr run_parse_names(struct parser *p)
{
    return parse_file(p, "names");
}


static errr finish_parse_names(struct parser *p)
{
    int i;
    unsigned int j;
    struct names_parse *n = parser_priv(p);
    struct name *nm;

    num_names = C_ZNEW(RANDNAME_NUM_TYPES, u32b);
    name_sections = mem_zalloc(sizeof(char**) * RANDNAME_NUM_TYPES);
    for (i = 0; i < RANDNAME_NUM_TYPES; i++)
    {
        num_names[i] = n->nnames[i];
        name_sections[i] = mem_alloc(sizeof(char*) * (n->nnames[i] + 1));
        for (nm = n->names[i], j = 0; nm && j < n->nnames[i]; nm = nm->next, j++)
            name_sections[i][j] = nm->str;
        name_sections[i][n->nnames[i]] = NULL;
        while (n->names[i])
        {
            nm = n->names[i]->next;
            mem_free(n->names[i]);
            n->names[i] = nm;
        }
    }
    mem_free(n);
    parser_destroy(p);

    return 0;
}


static void cleanup_names(void)
{
    strings_free(name_sections, num_names, RANDNAME_NUM_TYPES);
}


static struct file_parser names_parser =
{
    "names",
    init_parse_names,
    run_parse_names,
    finish_parse_names,
    cleanup_names
};


/* Parsing functions for terrain.txt */
static enum parser_error parse_f_n(struct parser *p)
{
    int idx = parser_getuint(p, "index");
    const char *name = parser_getstr(p, "name");
    struct feature *h = parser_priv(p);
    struct feature *f = mem_zalloc(sizeof(*f));

    f->next = h;
    f->fidx = idx;
    f->mimic = idx;
    f->name = string_make(name);
    parser_setpriv(p, f);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_f_g(struct parser *p)
{
    char glyph = parser_getchar(p, "glyph");
    const char *color = parser_getsym(p, "color");
    int attr = 0;
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->d_char = glyph;
    if (strlen(color) > 1)
        attr = color_text_to_attr(color);
    else
        attr = color_char_to_attr(color[0]);
    if (attr < 0) return PARSE_ERROR_INVALID_COLOR;
    f->d_attr = attr;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_f_m(struct parser *p)
{
    unsigned int idx = parser_getuint(p, "index");
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->mimic = idx;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_f_p(struct parser *p)
{
    unsigned int priority = parser_getuint(p, "priority");
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->priority = priority;

    return PARSE_ERROR_NONE;
}


static const char *f_info_flags[] =
{
    "PWALK",
    "PPASS",
    "MWALK",
    "MPASS",
    "LOOK",
    "DIG",
    "DOOR",
    "EXIT_UP",
    "EXIT_DOWN",
    "PERM",
    "TRAP",
    "SHOP",
    "HIDDEN",
    "BORING",
    NULL
};


static errr grab_one_flag(u32b *flags, const char *names[], const char *what)
{
    int i;

    /* Check flags */
    for (i = 0; (i < 32) && names[i]; i++)
    {
        if (streq(what, names[i]))
        {
            *flags |= (1L << i);
            return (0);
        }
    }

    return (-1);
}


static enum parser_error parse_f_f(struct parser *p)
{
    struct feature *f = parser_priv(p);
    char *flags;
    char *s;

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));

    s = strtok(flags, " |");
    while (s)
    {
        if (grab_one_flag(&f->flags, f_info_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_f_x(struct parser *p)
{
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->locked = parser_getint(p, "locked");
    f->jammed = parser_getint(p, "jammed");
    f->shopnum = parser_getint(p, "shopnum");
    f->dig = parser_getint(p, "dig");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_f_e(struct parser *p)
{
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->effect = grab_one_effect(parser_getstr(p, "effect"));
    if (!f->effect) return PARSE_ERROR_INVALID_EFFECT;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_f(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index str name", parse_f_n);
    parser_reg(p, "G char glyph sym color", parse_f_g);
    parser_reg(p, "M uint index", parse_f_m);
    parser_reg(p, "P uint priority", parse_f_p);
    parser_reg(p, "F ?str flags", parse_f_f);
    parser_reg(p, "X int locked int jammed int shopnum int dig", parse_f_x);
    parser_reg(p, "E str effect", parse_f_e);

    return p;
}


static errr run_parse_f(struct parser *p)
{
    return parse_file(p, "terrain");
}


static errr finish_parse_f(struct parser *p)
{
    struct feature *f, *n;

    /* Scan the list for the max id */
    z_info->f_max = 0;
    f = parser_priv(p);
    while (f)
    {
        if (f->fidx > z_info->f_max) z_info->f_max = f->fidx;
        f = f->next;
    }
    z_info->f_max++;

    /* Allocate the direct access list and copy the data to it */
    f_info = mem_zalloc(z_info->f_max * sizeof(*f));
    for (f = parser_priv(p); f; f = n)
    {
        memcpy(&f_info[f->fidx], f, sizeof(*f));
        n = f->next;
        if (n) f_info[f->fidx].next = &f_info[n->fidx];
        else f_info[f->fidx].next = NULL;
        mem_free(f);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_f(void)
{
    int i;

    /* Paranoia */
    if (!f_info) return;

    for (i = 0; i < z_info->f_max; i++) string_free(f_info[i].name);
    mem_free(f_info);
}


static struct file_parser f_parser =
{
    "terrain",
    init_parse_f,
    run_parse_f,
    finish_parse_f,
    cleanup_f
};


/* Parsing functions for ego-item.txt */
static enum parser_error parse_e_n(struct parser *p)
{
    int idx = parser_getint(p, "index");
    const char *name = parser_getstr(p, "name");
    struct ego_item *h = parser_priv(p);
    struct ego_item *e = mem_zalloc(sizeof(*e));

    e->next = h;
    parser_setpriv(p, e);
    e->eidx = idx;
    e->name = string_make(name);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_e_w(struct parser *p)
{
    int level = parser_getint(p, "level");
    int rarity = parser_getint(p, "rarity");
    struct ego_item *e = parser_priv(p);

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;
    e->level = level;
    e->rarity = rarity;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_e_x(struct parser *p)
{
    int rating = parser_getint(p, "rating");
    int xtra = parser_getint(p, "xtra");
    struct ego_item *e = parser_priv(p);

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;
    e->rating = rating;
    e->xtra = xtra;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_e_a(struct parser *p)
{
    struct ego_item *e = parser_priv(p);
    const char *tmp = parser_getstr(p, "minmax");
    int amin, amax;

    e->alloc_prob = parser_getint(p, "common");
    if (sscanf(tmp, "%d to %d", &amin, &amax) != 2) return PARSE_ERROR_GENERIC;

    if ((amin > 255) || (amax > 255) || (amin < 0) || (amax < 0)) return PARSE_ERROR_OUT_OF_BOUNDS;

    e->alloc_min = amin;
    e->alloc_max = amax;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_e_t(struct parser *p)
{
    int i;
    int tval;
    int min_sval, max_sval;
    struct ego_item *e = parser_priv(p);

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;

    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    min_sval = parser_getint(p, "min-sval");
    max_sval = parser_getint(p, "max-sval");

    for (i = 0; i < EGO_TVALS_MAX; i++)
    {
        if (!e->tval[i])
        {
            e->tval[i] = tval;
            e->min_sval[i] = min_sval;
            e->max_sval[i] = max_sval;
            break;
        }
    }

    if (i == EGO_TVALS_MAX) return PARSE_ERROR_GENERIC;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_e_c(struct parser *p)
{
    struct random th = parser_getrand(p, "th");
    struct random td = parser_getrand(p, "td");
    struct random ta = parser_getrand(p, "ta");
    struct ego_item *e = parser_priv(p);

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;

    e->to_h = th;
    e->to_d = td;
    e->to_a = ta;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_e_m(struct parser *p)
{
    int th = parser_getint(p, "th");
    int td = parser_getint(p, "td");
    int ta = parser_getint(p, "ta");
    struct ego_item *e = parser_priv(p);

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;

    e->min_to_h = th;
    e->min_to_d = td;
    e->min_to_a = ta;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_e_f(struct parser *p)
{
    struct ego_item *e = parser_priv(p);
    char *s;
    char *t;

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    s = string_make(parser_getstr(p, "flags"));
    t = strtok(s, " |");
    while (t)
    {
        if (grab_flag(e->flags, OF_SIZE, k_info_flags, t)) break;
        t = strtok(NULL, " |");
    }
    string_free(s);

    return (t? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_e_l(struct parser *p)
{
    struct ego_item *e = parser_priv(p);
    char *s;
    char *t;

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_MISSING_FIELD;

    e->pval[e->num_pvals] = parser_getrand(p, "pval");
    e->min_pval[e->num_pvals] = parser_getint(p, "min");

    s = string_make(parser_getstr(p, "flags"));
    t = strtok(s, " |");

    while (t)
    {
        if (grab_flag(e->flags, OF_SIZE, k_info_flags, t) ||
            grab_flag(e->pval_flags[e->num_pvals], OF_SIZE, k_info_flags, t))
                break;
        t = strtok(NULL, " |");
    }

    e->num_pvals++;
    if (e->num_pvals > MAX_PVALS) return PARSE_ERROR_TOO_MANY_ENTRIES;

    string_free(s);
    return (t? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_e_d(struct parser *p)
{
    struct ego_item *e = parser_priv(p);

    if (!e) return PARSE_ERROR_MISSING_RECORD_HEADER;
    e->text = string_append(e->text, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_e(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N int index str name", parse_e_n);
    parser_reg(p, "W int level int rarity int pad int cost", parse_e_w);
    parser_reg(p, "X int rating int xtra", parse_e_x);
    parser_reg(p, "A int common str minmax", parse_e_a);
    parser_reg(p, "T sym tval int min-sval int max-sval", parse_e_t);
    parser_reg(p, "C rand th rand td rand ta", parse_e_c);
    parser_reg(p, "M int th int td int ta", parse_e_m);
    parser_reg(p, "F ?str flags", parse_e_f);
    parser_reg(p, "L rand pval int min str flags", parse_e_l);
    parser_reg(p, "D str text", parse_e_d);

    return p;
}


static errr run_parse_e(struct parser *p)
{
    return parse_file(p, "ego_item");
}


static errr finish_parse_e(struct parser *p)
{
    struct ego_item *e, *n;

    /* Scan the list for the max id */
    z_info->e_max = 0;
    e = parser_priv(p);
    while (e)
    {
        if (e->eidx > z_info->e_max) z_info->e_max = e->eidx;
        e = e->next;
    }
    z_info->e_max++;

    /* Allocate the direct access list and copy the data to it */
    e_info = mem_zalloc(z_info->e_max * sizeof(*e));
    for (e = parser_priv(p); e; e = n)
    {
        memcpy(&e_info[e->eidx], e, sizeof(*e));
        n = e->next;
        if (n) e_info[e->eidx].next = &e_info[n->eidx];
        else e_info[e->eidx].next = NULL;
        mem_free(e);
    }

    create_slay_cache();

    parser_destroy(p);
    return 0;
}


static void cleanup_e(void)
{
    int i;

    /* Paranoia */
    if (!e_info) return;

    for (i = 0; i < z_info->e_max; i++)
    {
        string_free(e_info[i].name);
        string_free(e_info[i].text);
    }
    mem_free(e_info);
    free_slay_cache();
}


static struct file_parser e_parser =
{
    "ego_item",
    init_parse_e,
    run_parse_e,
    finish_parse_e,
    cleanup_e
};


/* Parsing functions for prace.txt */
static enum parser_error parse_p_n(struct parser *p)
{
    struct player_race *h = parser_priv(p);
    struct player_race *r = mem_zalloc(sizeof(*r));

    r->next = h;
    r->ridx = parser_getuint(p, "index");
    r->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, r);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_s(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_adj[A_STR] = parser_getint(p, "str");
    r->r_adj[A_DEX] = parser_getint(p, "dex");
    r->r_adj[A_CON] = parser_getint(p, "con");
    r->r_adj[A_INT] = parser_getint(p, "int");
    r->r_adj[A_WIS] = parser_getint(p, "wis");
    r->r_adj[A_CHR] = parser_getint(p, "chr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_r(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_DISARM] = parser_getint(p, "dis");
    r->r_skills[SKILL_DEVICE] = parser_getint(p, "dev");
    r->r_skills[SKILL_SAVE] = parser_getint(p, "sav");
    r->r_skills[SKILL_STEALTH] = parser_getint(p, "stl");
    r->r_skills[SKILL_SEARCH] = parser_getint(p, "srh");
    r->r_skills[SKILL_SEARCH_FREQUENCY] = parser_getint(p, "fos");
    r->r_skills[SKILL_TO_HIT_MELEE] = parser_getint(p, "thm");
    r->r_skills[SKILL_TO_HIT_BOW] = parser_getint(p, "thb");
    r->r_skills[SKILL_TO_HIT_THROW] = r->r_skills[SKILL_TO_HIT_BOW];
    r->r_skills[SKILL_DIGGING] = parser_getint(p, "dig");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_x(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_mhp = parser_getint(p, "mhp");
    r->r_exp = parser_getint(p, "exp");
    r->infra = parser_getint(p, "infra");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_i(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->history = findchart(histories, parser_getuint(p, "hist"));
    r->b_age = parser_getint(p, "b-age");
    r->m_age = parser_getint(p, "m-age");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_h(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->m_b_ht = parser_getint(p, "mbht");
    r->m_m_ht = parser_getint(p, "mmht");
    r->f_b_ht = parser_getint(p, "fbht");
    r->f_m_ht = parser_getint(p, "fmht");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_w(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->m_b_wt = parser_getint(p, "mbwt");
    r->m_m_wt = parser_getint(p, "mmwt");
    r->f_b_wt = parser_getint(p, "fbwt");
    r->f_m_wt = parser_getint(p, "fmwt");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_f(struct parser *p)
{
    struct player_race *r = parser_priv(p);
    char *flags;
    char *s;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(r->flags, OF_SIZE, k_info_flags, s)) break;
        s = strtok(NULL, " |");
    }
    string_free(flags);

    return (s? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static const char *player_info_flags[] =
{
    #define PF(a, b) #a,
    #include "../common/list-player-flags.h"
    #undef PF
    NULL
};


static enum parser_error parse_p_y(struct parser *p)
{
    struct player_race *r = parser_priv(p);
    char *flags;
    char *s;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(r->pflags, PF_SIZE, player_info_flags, s)) break;
        s = strtok(NULL, " |");
    }
    string_free(flags);

    return (s? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_p_c(struct parser *p)
{
    struct player_race *r = parser_priv(p);
    char *classes;
    char *s;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "classes")) return PARSE_ERROR_NONE;
    classes = string_make(parser_getstr(p, "classes"));
    s = strtok(classes, " |");
    while (s)
    {
        r->choice |= (1 << atoi(s));
        s = strtok(NULL, " |");
    }
    string_free(classes);

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_p(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index str name", parse_p_n);
    parser_reg(p, "S int str int int int wis int dex int con int chr", parse_p_s);
    parser_reg(p,
        "R int dis int dev int sav int stl int srh int fos int thm int thb int dig",
        parse_p_r);
    parser_reg(p, "X int mhp int exp int infra", parse_p_x);
    parser_reg(p, "I uint hist int b-age int m-age", parse_p_i);
    parser_reg(p, "H int mbht int mmht int fbht int fmht", parse_p_h);
    parser_reg(p, "W int mbwt int mmwt int fbwt int fmwt", parse_p_w);
    parser_reg(p, "F ?str flags", parse_p_f);
    parser_reg(p, "Y ?str flags", parse_p_y);
    parser_reg(p, "C ?str classes", parse_p_c);

    return p;
}


static errr run_parse_p(struct parser *p)
{
    return parse_file(p, "p_race");
}


static errr finish_parse_p(struct parser *p)
{
    races = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_p(void)
{
    struct player_race *p = races;
    struct player_race *next;

    while (p)
    {
        next = p->next;
        string_free(p->name);
        mem_free(p);
        p = next;
    }
}


static struct file_parser p_parser =
{
    "p_race",
    init_parse_p,
    run_parse_p,
    finish_parse_p,
    cleanup_p
};


/* Parsing functions for pclass.txt */
static enum parser_error parse_c_n(struct parser *p)
{
    struct player_class *h = parser_priv(p);
    struct player_class *c = mem_zalloc(sizeof(*c));

    c->cidx = parser_getuint(p, "index");
    c->name = string_make(parser_getstr(p, "name"));
    c->next = h;
    parser_setpriv(p, c);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_g(struct parser *p)
{
    const char *color = parser_getsym(p, "color");
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->attr = color_char_to_attr(color[0]);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_s(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    c->c_adj[A_STR] = parser_getint(p, "str");
    c->c_adj[A_INT] = parser_getint(p, "int");
    c->c_adj[A_WIS] = parser_getint(p, "wis");
    c->c_adj[A_DEX] = parser_getint(p, "dex");
    c->c_adj[A_CON] = parser_getint(p, "con");
    c->c_adj[A_CHR] = parser_getint(p, "chr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_c(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_DISARM] = parser_getint(p, "dis");
    c->c_skills[SKILL_DEVICE] = parser_getint(p, "dev");
    c->c_skills[SKILL_SAVE] = parser_getint(p, "sav");
    c->c_skills[SKILL_STEALTH] = parser_getint(p, "stl");
    c->c_skills[SKILL_SEARCH] = parser_getint(p, "srh");
    c->c_skills[SKILL_SEARCH_FREQUENCY] = parser_getint(p, "fos");
    c->c_skills[SKILL_TO_HIT_MELEE] = parser_getint(p, "thm");
    c->c_skills[SKILL_TO_HIT_BOW] = parser_getint(p, "thb");
    c->c_skills[SKILL_TO_HIT_THROW] = c->c_skills[SKILL_TO_HIT_BOW];
    c->c_skills[SKILL_DIGGING] = parser_getint(p, "dig");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_x(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->x_skills[SKILL_DISARM] = parser_getint(p, "dis");
    c->x_skills[SKILL_DEVICE] = parser_getint(p, "dev");
    c->x_skills[SKILL_SAVE] = parser_getint(p, "sav");
    c->x_skills[SKILL_STEALTH] = parser_getint(p, "stl");
    c->x_skills[SKILL_SEARCH] = parser_getint(p, "srh");
    c->x_skills[SKILL_SEARCH_FREQUENCY] = parser_getint(p, "fos");
    c->x_skills[SKILL_TO_HIT_MELEE] = parser_getint(p, "thm");
    c->x_skills[SKILL_TO_HIT_BOW] = parser_getint(p, "thb");
    c->x_skills[SKILL_TO_HIT_THROW] = c->x_skills[SKILL_TO_HIT_BOW];
    c->x_skills[SKILL_DIGGING] = parser_getint(p, "dig");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_i(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_mhp = parser_getint(p, "mhp");
    c->c_exp = parser_getint(p, "exp");
    c->sense_base = parser_getint(p, "sense-base");
    c->sense_div = parser_getint(p, "sense-div");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_a(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->max_attacks = parser_getint(p, "max-attacks");
    c->min_weight = parser_getint(p, "min-weight");
    c->att_multiply = parser_getint(p, "att-multiply");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_m(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->spell_book = parser_getuint(p, "book");
    c->spell_stat = parser_getuint(p, "stat");
    c->spell_first = parser_getuint(p, "first");
    c->spell_weight = parser_getuint(p, "weight");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_b(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    unsigned int spell;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    spell = parser_getuint(p, "spell");
    if (spell >= PY_MAX_SPELLS) return PARSE_ERROR_OUT_OF_BOUNDS;
    c->spells.info[spell].slevel = parser_getint(p, "level");
    c->spells.info[spell].smana = parser_getint(p, "mana");
    c->spells.info[spell].sfail = parser_getint(p, "fail");
    c->spells.info[spell].sexp = parser_getint(p, "exp");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_t(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    int i;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    for (i = 0; i < PY_MAX_LEVEL / 5; i++)
    {
        if (!c->title[i])
        {
            c->title[i] = string_make(parser_getstr(p, "title"));
            break;
        }
    }

    if (i >= PY_MAX_LEVEL / 5) return PARSE_ERROR_TOO_MANY_ENTRIES;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_e_aux(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct start_item *si;
    int tval, sval;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    sval = lookup_sval(tval, parser_getsym(p, "sval"));
    if (sval < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;

    si = mem_zalloc(sizeof(*si));
    si->kind = lookup_kind(tval, sval);
    si->min = parser_getuint(p, "min");
    si->max = parser_getuint(p, "max");

    if ((si->min >= MAX_STACK_SIZE) || (si->max >= MAX_STACK_SIZE))
    {
        mem_free(si);
        return PARSE_ERROR_INVALID_ITEM_NUMBER;
    }

    si->next = c->start_items;
    c->start_items = si;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_c_e(struct parser *p)
{
    if (cfg_no_recall) return PARSE_ERROR_NONE;
    return parse_c_e_aux(p);
}


static enum parser_error parse_c_y(struct parser *p)
{
    if (!cfg_no_recall) return PARSE_ERROR_NONE;
    return parse_c_e_aux(p);
}


static enum parser_error parse_c_f(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    char *flags;
    char *s;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(c->pflags, PF_SIZE, player_info_flags, s)) break;
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return (s? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static struct parser *init_parse_c(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index str name", parse_c_n);
    parser_reg(p, "G sym color", parse_c_g);
    parser_reg(p, "S int str int int int wis int dex int con int chr", parse_c_s);
    parser_reg(p,
        "C int dis int dev int sav int stl int srh int fos int thm int thb int dig",
        parse_c_c);
    parser_reg(p,
        "X int dis int dev int sav int stl int srh int fos int thm int thb int dig",
        parse_c_x);
    parser_reg(p, "I int mhp int exp int sense-base int sense-div", parse_c_i);
    parser_reg(p, "A int max-attacks int min-weight int att-multiply", parse_c_a);
    parser_reg(p, "M uint book uint stat uint first uint weight", parse_c_m);
    parser_reg(p, "B uint spell int level int mana int fail int exp", parse_c_b);
    parser_reg(p, "T str title", parse_c_t);
    parser_reg(p, "E sym tval sym sval uint min uint max", parse_c_e);
    parser_reg(p, "Y sym tval sym sval uint min uint max", parse_c_y);
    parser_reg(p, "F ?str flags", parse_c_f);

    return p;
}


static errr run_parse_c(struct parser *p)
{
    return parse_file(p, "p_class");
}


static errr finish_parse_c(struct parser *p)
{
    classes = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_c(void)
{
    struct player_class *c = classes;
    struct player_class *next;
    struct start_item *item, *item_next;
    int i;

    while (c)
    {
        next = c->next;
        string_free(c->name);
        for (i = 0; i < PY_MAX_LEVEL / 5; i++) string_free(c->title[i]);
        item = c->start_items;
        while (item)
        {
            item_next = item->next;
            mem_free(item);
            item = item_next;
        }
        mem_free(c);
        c = next;
    }
}


static struct file_parser c_parser =
{
    "p_class",
    init_parse_c,
    run_parse_c,
    finish_parse_c,
    cleanup_c
};


/* Parsing functions for vault.txt */
static enum parser_error parse_v_n(struct parser *p)
{
    struct vault *h = parser_priv(p);
    struct vault *v = mem_zalloc(sizeof(*v));

    v->vidx = parser_getuint(p, "index");
    v->name = string_make(parser_getstr(p, "name"));
    v->next = h;
    parser_setpriv(p, v);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_v_x(struct parser *p)
{
    struct vault *v = parser_priv(p);

    if (!v) return PARSE_ERROR_MISSING_RECORD_HEADER;
    v->typ = parser_getuint(p, "type");
    v->rat = parser_getint(p, "rating");
    v->hgt = parser_getuint(p, "height");
    v->wid = parser_getuint(p, "width");

    /* Check for maximum vault sizes */
    if ((v->typ == 7) && ((v->wid > 33) || (v->hgt > 22)))
        return PARSE_ERROR_VAULT_TOO_BIG;
    if ((v->typ == 8) && ((v->wid > 66) || (v->hgt > 44)))
        return PARSE_ERROR_VAULT_TOO_BIG;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_v_d(struct parser *p)
{
    struct vault *v = parser_priv(p);

    if (!v) return PARSE_ERROR_MISSING_RECORD_HEADER;
    v->text = string_append(v->text, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_v(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index str name", parse_v_n);
    parser_reg(p, "X uint type int rating uint height uint width", parse_v_x);
    parser_reg(p, "D str text", parse_v_d);

    return p;
}


static errr run_parse_v(struct parser *p)
{
    return parse_file(p, "vault");
}


static errr finish_parse_v(struct parser *p)
{
    vaults = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_v(void)
{
    struct vault *v, *next;

    for (v = vaults; v; v = next)
    {
        next = v->next;
        string_free(v->name);
        string_free(v->text);
        mem_free(v);
    }
}


static struct file_parser v_parser =
{
    "vault",
    init_parse_v,
    run_parse_v,
    finish_parse_v,
    cleanup_v
};


/* Parsing functions for p_hist.txt */
static enum parser_error parse_h_n(struct parser *p)
{
    struct history_chart *oc = parser_priv(p);
    struct history_chart *c;
    struct history_entry *e = mem_zalloc(sizeof(*e));
    unsigned int idx = parser_getuint(p, "chart");

    c = findchart(oc, idx);
    if (!c)
    {
        c = mem_zalloc(sizeof(*c));
        c->next = oc;
        c->idx = idx;
        parser_setpriv(p, c);
    }

    e->isucc = parser_getint(p, "next");
    e->roll = parser_getint(p, "roll");
    e->bonus = parser_getint(p, "bonus");

    e->next = c->entries;
    c->entries = e;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_h_d(struct parser *p)
{
    struct history_chart *h = parser_priv(p);

    if (!h) return PARSE_ERROR_MISSING_RECORD_HEADER;
    my_assert(h->entries);
    h->entries->text = string_append(h->entries->text, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_h(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint chart int next int roll int bonus", parse_h_n);
    parser_reg(p, "D str text", parse_h_d);

    return p;
}


static errr run_parse_h(struct parser *p)
{
    return parse_file(p, "p_hist");
}


static errr finish_parse_h(struct parser *p)
{
    struct history_chart *c;
    struct history_entry *e, *prev, *next;

    histories = parser_priv(p);

    /*
     * Go fix up the entry successor pointers. We can't compute them at
     * load-time since we may not have seen the successor history yet. Also,
     * we need to put the entries in the right order; the parser actually
     * stores them backwards, which is not desirable.
     */
    for (c = histories; c; c = c->next)
    {
        e = c->entries;
        prev = NULL;
        while (e)
        {
            next = e->next;
            e->next = prev;
            prev = e;
            e = next;
        }
        c->entries = prev;
        for (e = c->entries; e; e = e->next)
        {
            if (!e->isucc) continue;
            e->succ = findchart(histories, e->isucc);
            if (!e->succ) return -1;
        }
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_h(void)
{
    struct history_chart *c, *next_c;
    struct history_entry *e, *next_e;

    c = histories;
    while (c)
    {
        next_c = c->next;
        e = c->entries;
        while (e)
        {
            next_e = e->next;
            string_free(e->text);
            mem_free(e);
            e = next_e;
        }
        mem_free(c);
        c = next_c;
    }
}


static struct file_parser h_parser =
{
    "p_hist",
    init_parse_h,
    run_parse_h,
    finish_parse_h,
    cleanup_h
};


/* Parsing functions for flavor.txt */
static enum parser_error parse_flavor_n(struct parser *p)
{
    struct flavor *h = parser_priv(p);
    struct flavor *f = mem_zalloc(sizeof(*f));

    f->next = h;
    f->fidx = parser_getuint(p, "index");
    f->tval = tval_find_idx(parser_getsym(p, "tval"));
    if (parser_hasval(p, "sval"))
        f->sval = lookup_sval(f->tval, parser_getsym(p, "sval"));
    else
        f->sval = SV_UNKNOWN;
    parser_setpriv(p, f);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_flavor_g(struct parser *p)
{
    struct flavor *f = parser_priv(p);
    int d_attr;
    const char *attr;

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;

    f->d_char = parser_getchar(p, "glyph");
    attr = parser_getsym(p, "attr");
    if (strlen(attr) == 1)
        d_attr = color_char_to_attr(attr[0]);
    else
        d_attr = color_text_to_attr(attr);
    if (d_attr < 0) return PARSE_ERROR_GENERIC;
    f->d_attr = d_attr;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_flavor_d(struct parser *p)
{
    struct flavor *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->text = string_append(f->text, parser_getstr(p, "desc"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_flavor(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index sym tval ?sym sval", parse_flavor_n);
    parser_reg(p, "G char glyph sym attr", parse_flavor_g);
    parser_reg(p, "D str desc", parse_flavor_d);

    return p;
}


static errr run_parse_flavor(struct parser *p)
{
    return parse_file(p, "flavor");
}


static errr finish_parse_flavor(struct parser *p)
{
    flavors = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_flavor(void)
{
    struct flavor *f, *next;

    f = flavors;
    while (f)
    {
        next = f->next;
        string_free(f->text);
        mem_free(f);
        f = next;
    }
}


static struct file_parser flavor_parser =
{
    "flavor",
    init_parse_flavor,
    run_parse_flavor,
    finish_parse_flavor,
    cleanup_flavor
};


/* Parsing functions for spell.txt */
static enum parser_error parse_s_n(struct parser *p)
{
    struct spell *s = mem_zalloc(sizeof(*s));

    s->next = parser_priv(p);
    s->sidx = parser_getuint(p, "index");
    s->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, s);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_s_i(struct parser *p)
{
    struct spell *s = parser_priv(p);

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    s->tval = parser_getuint(p, "tval");
    s->sval = parser_getuint(p, "sval");
    s->snum = parser_getuint(p, "snum");
    s->sdir = parser_getuint(p, "sdir");
    s->sproj = parser_getuint(p, "sproj");

    s->realm = s->tval - TV_MAGIC_BOOK;
    s->spell_index = s->sidx - (s->realm * PY_MAX_SPELLS);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_s_d(struct parser *p)
{
    struct spell *s = parser_priv(p);

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    s->text = string_append(s->text, parser_getstr(p, "desc"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_s(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index str name", parse_s_n);
    parser_reg(p, "I uint tval uint sval uint snum uint sdir uint sproj", parse_s_i);
    parser_reg(p, "D str desc", parse_s_d);

    return p;
}


static errr run_parse_s(struct parser *p)
{
    return parse_file(p, "spell");
}


static errr finish_parse_s(struct parser *p)
{
    struct spell *s, *n, *ss;
    struct object_kind *k;

    /* Scan the list for the max id */
    z_info->s_max = 0;
    s = parser_priv(p);
    while (s)
    {
        if (s->sidx > z_info->s_max) z_info->s_max = s->sidx;
        s = s->next;
    }
    z_info->s_max++;

    /* Allocate the direct access list and copy the data to it */
    s_info = mem_zalloc(z_info->s_max * sizeof(*s));
    for (s = parser_priv(p); s; s = n)
    {
        n = s->next;
        ss = &s_info[s->sidx];
        memcpy(ss, s, sizeof(*s));
        k = lookup_kind(s->tval, s->sval);
        if (k)
        {
            ss->next = k->spells;
            k->spells = ss;
        }
        else ss->next = NULL;
        mem_free(s);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_s(void)
{
    int i;

    /* Paranoia */
    if (!s_info) return;

    for (i = 0; i < z_info->s_max; i++)
    {
        string_free(s_info[i].name);
        string_free(s_info[i].text);
    }
    mem_free(s_info);
}


static struct file_parser s_parser =
{
    "spell",
    init_parse_s,
    run_parse_s,
    finish_parse_s,
    cleanup_s
};


/* Parsing functions for socials.txt */
static enum parser_error parse_soc_n(struct parser *p)
{
    struct social *s = mem_zalloc(sizeof(*s));

    s->next = parser_priv(p);
    s->sidx = parser_getuint(p, "index");
    s->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, s);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_soc_i(struct parser *p)
{
    struct social *s = parser_priv(p);

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    s->target = parser_getuint(p, "target");
    s->max_dist = parser_getuint(p, "max-dist");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_soc_d(struct parser *p)
{
    struct social *s = parser_priv(p);

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    s->text = string_append(s->text, parser_getstr(p, "desc"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_soc(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "V sym version", ignored);
    parser_reg(p, "N uint index str name", parse_soc_n);
    parser_reg(p, "I uint target uint max-dist", parse_soc_i);
    parser_reg(p, "D str desc", parse_soc_d);

    return p;
}


static errr run_parse_soc(struct parser *p)
{
    return parse_file(p, "socials");
}


static errr finish_parse_soc(struct parser *p)
{
    struct social *s, *n;

    /* Scan the list for the max id */
    z_info->soc_max = 0;
    s = parser_priv(p);
    while (s)
    {
        if (s->sidx > z_info->soc_max) z_info->soc_max = s->sidx;
        s = s->next;
    }
    z_info->soc_max++;

    /* Allocate the direct access list and copy the data to it */
    soc_info = mem_zalloc(z_info->soc_max * sizeof(*s));
    for (s = parser_priv(p); s; s = n)
    {
        memcpy(&soc_info[s->sidx], s, sizeof(*s));
        n = s->next;
        if (n) soc_info[s->sidx].next = &soc_info[n->sidx];
        else soc_info[s->sidx].next = NULL;
        mem_free(s);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_soc(void)
{
    int i;

    /* Paranoia */
    if (!soc_info) return;

    for (i = 0; i < z_info->soc_max; i++)
    {
        string_free(soc_info[i].name);
        string_free(soc_info[i].text);
    }
    mem_free(soc_info);
}


static struct file_parser soc_parser =
{
    "socials",
    init_parse_soc,
    run_parse_soc,
    finish_parse_soc,
    cleanup_soc
};


/* Initialise hints */


static enum parser_error parse_hint(struct parser *p)
{
    struct hint *h = parser_priv(p);
    struct hint *n = mem_zalloc(sizeof(*n));

    n->hint = string_make(parser_getstr(p, "text"));
    n->next = h;

    parser_setpriv(p, n);
    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_hints(void)
{
    struct parser *p = parser_new();

    parser_reg(p, "H str text", parse_hint);
    return p;
}


static errr run_parse_hints(struct parser *p)
{
    return parse_file(p, "hints");
}


static errr finish_parse_hints(struct parser *p)
{
    hints = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_hints(void)
{
    struct hint *h, *next;

    h = hints;
    while (h)
    {
        next = h->next;
        string_free(h->hint);
        mem_free(h);
        h = next;
    }
}


static struct file_parser hints_parser =
{
    "hints",
    init_parse_hints,
    run_parse_hints,
    finish_parse_hints,
    cleanup_hints
};


/* Initialise monster pain messages */
static enum parser_error parse_mp_n(struct parser *p)
{
	struct monster_pain *h = parser_priv(p);
	struct monster_pain *mp = mem_zalloc(sizeof(*mp));

	mp->next = h;
	mp->pain_idx = parser_getuint(p, "index");
	parser_setpriv(p, mp);

	return PARSE_ERROR_NONE;
}


static enum parser_error parse_mp_m(struct parser *p)
{
	struct monster_pain *mp = parser_priv(p);
	int i;

	if (!mp) return PARSE_ERROR_MISSING_RECORD_HEADER;
	for (i = 0; i < 7; i++)
    {
		if (!mp->messages[i]) break;
    }
	if (i == 7) return PARSE_ERROR_TOO_MANY_ENTRIES;
	mp->messages[i] = string_make(parser_getstr(p, "message"));

	return PARSE_ERROR_NONE;
}


static struct parser *init_parse_mp(void)
{
	struct parser *p = parser_new();
	parser_setpriv(p, NULL);

	parser_reg(p, "N uint index", parse_mp_n);
	parser_reg(p, "M str message", parse_mp_m);
	return p;
}


static errr run_parse_mp(struct parser *p)
{
	return parse_file(p, "pain");
}


static errr finish_parse_mp(struct parser *p)
{
	struct monster_pain *mp, *n;

    /* Scan the list for the max id */
    z_info->mp_max = 0;
    mp = parser_priv(p);
    while (mp)
    {
        if (mp->pain_idx > z_info->mp_max) z_info->mp_max = mp->pain_idx;
        mp = mp->next;
    }
    z_info->mp_max++;

    /* Allocate the direct access list and copy the data to it */
	pain_messages = mem_zalloc(z_info->mp_max * sizeof(*mp));
	for (mp = parser_priv(p); mp; mp = n)
    {
		memcpy(&pain_messages[mp->pain_idx], mp, sizeof(*mp));
        n = mp->next;
        if (n) pain_messages[mp->pain_idx].next = &pain_messages[n->pain_idx];
        else pain_messages[mp->pain_idx].next = NULL;
		mem_free(mp);
	}

	parser_destroy(p);
	return 0;
}


static void cleanup_mp(void)
{
	int idx, i;

    /* Paranoia */
    if (!pain_messages) return;

	for (idx = 0; idx < z_info->mp_max; idx++)
    {
		for (i = 0; i < 7; i++)
			string_free((char *)pain_messages[idx].messages[i]);
	}
	mem_free(pain_messages);
}


static struct file_parser mp_parser =
{
	"pain messages",
	init_parse_mp,
	run_parse_mp,
	finish_parse_mp,
	cleanup_mp
};


/* Initialise monster pits */


static enum parser_error parse_pit_n(struct parser *p)
{
    struct pit_profile *h = parser_priv(p);
    struct pit_profile *pit = mem_zalloc(sizeof(*pit));

    pit->next = h;
    pit->pit_idx = parser_getuint(p, "index");
    pit->name = string_make(parser_getstr(p, "name"));
    pit->colors = NULL;
    pit->forbidden_monsters = NULL;
    parser_setpriv(p, pit);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_r(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;

    pit->room_type = parser_getuint(p, "type");
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_a(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;

    pit->rarity = parser_getuint(p, "rarity");
    pit->ave = parser_getuint(p, "level");
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_o(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;

    pit->obj_rarity = parser_getuint(p, "obj_rarity");
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_t(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);
    struct monster_base *base = lookup_monster_base(parser_getsym(p, "base"));

    if (!pit)
        return PARSE_ERROR_MISSING_RECORD_HEADER;
    else if (pit->n_bases == MAX_RVALS)
        return PARSE_ERROR_TOO_MANY_ENTRIES;
    else if (base == NULL)
        /* Todo: make new error for this */
        return PARSE_ERROR_UNRECOGNISED_TVAL;
    else
    {
        pit->base[pit->n_bases++] = base;
        return PARSE_ERROR_NONE;
    }
}


static enum parser_error parse_pit_c(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);
    struct pit_color_profile *colors;
    const char *color;
    int attr;

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;
    color = parser_getsym(p, "color");
    if (strlen(color) > 1)
        attr = color_text_to_attr(color);
    else
        attr = color_char_to_attr(color[0]);
    if (attr < 0) return PARSE_ERROR_INVALID_COLOR;

    colors = mem_zalloc(sizeof(*colors));
    colors->color = attr;
    colors->next = pit->colors;
    pit->colors = colors;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_f(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);
    char *flags;
    char *s;

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(pit->flags, RF_SIZE, r_info_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_f2(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);
    char *flags;
    char *s;

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(pit->forbidden_flags, RF_SIZE, r_info_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_s(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);
    char *flags;
    char *s;

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "spells")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "spells"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(pit->spell_flags, RSF_SIZE, r_info_spell_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pit_s2(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);
    char *flags;
    char *s;

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "spells")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "spells"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(pit->forbidden_spell_flags, RSF_SIZE, r_info_spell_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


/*
 * Return the r_idx of the monster with the given name.
 * If no monster has the exact name given, returns the r_idx
 * of the first monster having the given name as a prefix.
 */
static int lookup_monster(const char *name)
{
    int i;
    int r_idx = -1;

    /* Look for it */
    for (i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Test for equality */
        if (r_ptr->name && streq(name, r_ptr->name)) return i;

        /* Test for close matches */
        if (r_ptr->name && my_stristr(r_ptr->name, name) && (r_idx == -1)) r_idx = i;
    }

    /* Return our best match */
    return r_idx;
}


static enum parser_error parse_pit_e(struct parser *p)
{
    struct pit_profile *pit = parser_priv(p);
    struct pit_forbidden_monster *monsters;
    int r_idx = lookup_monster(parser_getsym(p, "race"));

    if (!pit) return PARSE_ERROR_MISSING_RECORD_HEADER;

    monsters = mem_zalloc(sizeof(*monsters));
    monsters->r_idx = r_idx;
    monsters->next = pit->forbidden_monsters;
    pit->forbidden_monsters = monsters;
    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_pit(void)
{
    struct parser *p = parser_new();
    parser_setpriv(p, NULL);

    parser_reg(p, "N uint index str name", parse_pit_n);
    parser_reg(p, "R uint type", parse_pit_r);
    parser_reg(p, "A uint rarity uint level", parse_pit_a);
    parser_reg(p, "O uint obj_rarity", parse_pit_o);
    parser_reg(p, "T sym base", parse_pit_t);
    parser_reg(p, "C sym color", parse_pit_c);
    parser_reg(p, "F ?str flags", parse_pit_f);
    parser_reg(p, "f ?str flags", parse_pit_f2);
    parser_reg(p, "S ?str spells", parse_pit_s);
    parser_reg(p, "s ?str spells", parse_pit_s2);
    parser_reg(p, "E sym race", parse_pit_e);
    return p;
}


static errr run_parse_pit(struct parser *p)
{
    return parse_file(p, "pit");
}


static errr finish_parse_pit(struct parser *p)
{
    struct pit_profile *pit, *n;

    /* Scan the list for the max id */
    z_info->pit_max = 0;
    pit = parser_priv(p);
    while (pit)
    {
        if (pit->pit_idx > z_info->pit_max) z_info->pit_max = pit->pit_idx;
        pit = pit->next;
    }
    z_info->pit_max++;

    /* Allocate the direct access list and copy the data to it */
    pit_info = mem_zalloc(z_info->pit_max * sizeof(*pit));
    for (pit = parser_priv(p); pit; pit = n)
    {
        memcpy(&pit_info[pit->pit_idx], pit, sizeof(*pit));
        n = pit->next;
        if (n) pit_info[pit->pit_idx].next = &pit_info[n->pit_idx];
        else pit_info[pit->pit_idx].next = NULL;
        mem_free(pit);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_pits(void)
{
    int i;

    /* Paranoia */
    if (!pit_info) return;

    for (i = 0; i < z_info->pit_max; i++)
    {
        struct pit_profile *pit = &pit_info[i];
        struct pit_color_profile *c, *cn;
        struct pit_forbidden_monster *m, *mn;

        c = pit->colors;
        while (c)
        {
            cn = c->next;
            mem_free(c);
            c = cn;
        }
        m = pit->forbidden_monsters;
        while (m)
        {
            mn = m->next;
            mem_free(m);
            m = mn;
        }

        string_free(pit->name);
    }
    mem_free(pit_info);
}


static struct file_parser pit_parser =
{
    "pits",
    init_parse_pit,
    run_parse_pit,
    finish_parse_pit,
    cleanup_pits
};


/*** Initialize others ***/


/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
    int i;

    /*** Prepare "vinfo" array ***/

    /* Used by "update_view()" */
    vinfo_init();

    /*** Prepare the "dungeon" information ***/

    /* Allocate and Wipe the object list */
    objects_init();

    /* Allocate "permanent" space for the town */
    alloc_dungeon_level(0);

    /*** Init the wild_info array... for more information see wilderness.c ***/
    init_wild_info();

    /*** Prepare the various "bizarre" arrays ***/

    /* Quark variables */
    quarks_init();

    /*** Prepare chat channels ***/
    for (i = 0; i < MAX_CHANNELS; i++)
    {
        channels[i].name[0] = '\0';
        channels[i].id = 0;
        channels[i].num = 0;
        channels[i].mode = 0;
        chan_audit = chan_debug = chan_cheat = 0;
    }
    my_strcpy(channels[0].name, DEFAULT_CHANNEL, sizeof(channels[0].name));
    channels[0].mode = CM_SERVICE | CM_PLOG;
    for (i = 1; i < 4; i++)
    {
        channels[i].id = i;
        channels[i].mode = (CM_SECRET | CM_KEYLOCK | CM_SERVICE);
        switch (i)
        {
            case 1:
                chan_audit = i;
                my_strcpy(channels[i].name, "#audit", sizeof(channels[0].name));
                break;
            case 2:
                chan_debug = i;
                my_strcpy(channels[i].name, "#debug", sizeof(channels[0].name));
                break;
            case 3:
                chan_cheat = i;
                my_strcpy(channels[i].name, "#cheat", sizeof(channels[0].name));
                break;
        }
    }

    /* Success */
    return (0);
}


static byte get_artifact_rarity(s32b value, object_kind *kind)
{
    s32b alloc = 50000000 / (value * (kind->alloc_prob? kind->alloc_prob: 100));

    if (alloc > 990) return 99;
    if (alloc < 10) return 1;

    return (byte)(((alloc - (alloc / 10) * 10) >= 5)? alloc / 10 + 1: alloc / 10);
}


/*
 * Initialize some other arrays
 */
static errr init_alloc(void)
{
    int i;
    monster_race *r_ptr;
    ego_item_type *e_ptr;
    alloc_entry *table;
    s16b num[MAX_DEPTH];
    s16b aux[MAX_DEPTH];

    /*** Initialize object allocation info ***/

    init_obj_alloc();

    /* Hack -- Automatically compute art rarities for PWMAngband's artifacts */
    for (i = ART_MAX_STATIC + 1; i < z_info->a_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        object_type object_type_body;
        s32b value;

        /* Ignore "empty" artifacts */
        if (!a_ptr->tval) continue;

        /* Create a "forged" artifact */
        if (!make_fake_artifact(&object_type_body, a_ptr)) continue;

        /* Get the value */
        value = object_value_real(NULL, &object_type_body, 1);

        /* Allocation probability */
        a_ptr->alloc_prob = get_artifact_rarity(value, object_type_body.kind);
    }

    /*** Analyze monster allocation info ***/

    /* Clear the "aux" array */
    C_WIPE(&aux, MAX_DEPTH, s16b);

    /* Clear the "num" array */
    C_WIPE(&num, MAX_DEPTH, s16b);

    /* Size of "alloc_race_table" */
    alloc_race_size = 0;

    /* Scan the monsters */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Get the i'th race */
        r_ptr = &r_info[i];

        /* Legal monsters */
        if (r_ptr->rarity)
        {
            /* Count the entries */
            alloc_race_size++;

            /* Group by level */
            num[r_ptr->level]++;
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < MAX_DEPTH; i++)
    {
        /* Group by level */
        num[i] += num[i - 1];
    }

    /* Paranoia */
    if (!num[0]) quit("No town monsters!");

    /*** Initialize monster allocation info ***/

    /* Allocate the alloc_race_table */
    alloc_race_table = C_ZNEW(alloc_race_size, alloc_entry);

    /* Access the table entry */
    table = alloc_race_table;

    /* Scan the monsters */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Get the i'th race */
        r_ptr = &r_info[i];

        /* Count valid pairs */
        if (r_ptr->rarity)
        {
            int p, x, y, z;

            /* Extract the base level */
            x = r_ptr->level;

            /* Extract the base probability */
            p = (100 / r_ptr->rarity);

            /* Skip entries preceding our locale */
            y = ((x > 0)? num[x - 1]: 0);

            /* Skip previous entries at this locale */
            z = y + aux[x];

            /* Load the entry */
            table[z].index = i;
            table[z].level = x;
            table[z].prob1 = p;
            table[z].prob2 = p;
            table[z].prob3 = p;

            /* Another entry complete for this locale */
            aux[x]++;
        }
    }

    /*** Analyze ego item allocation info ***/

    /* Clear the "aux" array */
    C_WIPE(&aux, MAX_DEPTH, s16b);

    /* Clear the "num" array */
    C_WIPE(&num, MAX_DEPTH, s16b);

    /* Size of "alloc_ego_table" */
    alloc_ego_size = 0;

    /* Scan the ego items */
    for (i = 1; i < z_info->e_max; i++)
    {
        /* Get the i'th ego item */
        e_ptr = &e_info[i];

        /* Legal items */
        if (e_ptr->rarity)
        {
            /* Count the entries */
            alloc_ego_size++;

            /* Group by level */
            num[e_ptr->level]++;
        }
    }

    /* Collect the level indexes */
    for (i = 1; i < MAX_DEPTH; i++)
    {
        /* Group by level */
        num[i] += num[i - 1];
    }

    /*** Initialize ego item allocation info ***/

    /* Allocate the alloc_ego_table */
    alloc_ego_table = C_ZNEW(alloc_ego_size, alloc_entry);

    /* Access the table entry */
    table = alloc_ego_table;

    /* Scan the ego items */
    for (i = 1; i < z_info->e_max; i++)
    {
        /* Get the i'th ego item */
        e_ptr = &e_info[i];

        /* Count valid pairs */
        if (e_ptr->rarity)
        {
            int p, x, y, z;

            /* Extract the base level */
            x = e_ptr->level;

            /* Extract the base probability */
            p = (100 / e_ptr->rarity);

            /* Skip entries preceding our locale */
            y = ((x > 0)? num[x - 1]: 0);

            /* Skip previous entries at this locale */
            z = y + aux[x];

            /* Load the entry */
            table[z].index = i;
            table[z].level = x;
            table[z].prob1 = p;
            table[z].prob2 = p;
            table[z].prob3 = p;

            /* Another entry complete for this locale */
            aux[x]++;
        }
    }

    /* Success */
    return (0);
}


/*
 * Initialize the "player_presets" array
 */
static errr init_player_presets(void)
{
    /* Read player presets from pref files */
    process_pref_file_xtra("presets.prf");

    return 0;
}


/*
 * Initialize various Angband variables and arrays.
 *
 * This initialization involves the parsing of special files
 * in the "lib/edit" directory.
 *
 * Note that the "template" files are initialized first, since they
 * often contain errors.  This means that macros and message recall
 * and things like that are not available until after they are done.
 */
void init_angband(void)
{
    /* Initialize size info */
    plog("[Initializing array sizes...]");
    if (run_parser(&z_parser)) quit("Cannot initialize sizes");

    /* Initialize feature info */
    plog("[Initializing arrays... (features)]");
    if (run_parser(&f_parser)) quit("Cannot initialize features");

    /* Initialize object base info */
    plog("[Initializing arrays... (object bases)]");
    if (run_parser(&kb_parser)) quit("Cannot initialize object bases");

    /* Initialize object info */
    plog("[Initializing arrays... (objects)]");
    if (run_parser(&k_parser)) quit("Cannot initialize objects");

    /* Initialize ego-item info */
    plog("[Initializing arrays... (ego-items)]");
    if (run_parser(&e_parser)) quit("Cannot initialize ego-items");

    /* Initialize artifact info */
    plog("[Initializing arrays... (artifacts)]");
    if (run_parser(&a_parser)) quit("Cannot initialize artifacts");

    /* Initialize monster pain messages */
    plog("[Initializing arrays... (pain messages)]");
    if (run_parser(&mp_parser)) quit("Cannot initialize monster pain messages");

    /* Initialize monster base info */
    plog("[Initializing arrays... (monster bases)]");
    if (run_parser(&rb_parser)) quit("Cannot initialize monsters bases");

    /* Initialize monster info */
    plog("[Initializing arrays... (monsters)]");
    if (run_parser(&r_parser)) quit("Cannot initialize monsters");

    /* Initialize monster pits */
    plog("[Initializing arrays... (monster pits)]");
    if (run_parser(&pit_parser)) quit("Cannot initialize monster pits");

    /* Initialize feature info */
    plog("[Initializing arrays... (vaults)]");
    if (run_parser(&v_parser)) quit("Cannot initialize vaults");

    /* Initialize history info */
    plog("[Initializing arrays... (histories)]");
    if (run_parser(&h_parser)) quit("Cannot initialize histories");

    /* Initialize race info */
    plog("[Initializing arrays... (races)]");
    if (run_parser(&p_parser)) quit("Cannot initialize races");

    /* Initialize class info */
    plog("[Initializing arrays... (classes)]");
    if (run_parser(&c_parser)) quit("Cannot initialize classes");

    /* Initialize flavor info */
    plog("[Initializing arrays... (flavors)]");
    if (run_parser(&flavor_parser)) quit("Cannot initialize flavors");
    
    /* Initialize spell info */
    plog("[Initializing arrays... (spells)]");
    if (run_parser(&s_parser)) quit("Cannot initialize spells");
    
    /* Initialize social info */
    plog("[Initializing arrays... (socials)]");
    if (run_parser(&soc_parser)) quit("Cannot initialize socials");

    /* Initialize hint text */
    plog("[Initializing arrays... (hints)]");
    if (run_parser(&hints_parser)) quit("Cannot initialize hints");

    /* Initialise store stocking data */
    plog("[Initializing arrays... (store stocks)]");
    store_init();

    /* Initialise random name data */
    plog("[Initializing arrays... (random names)]");
    run_parser(&names_parser);

    /* Initialize player presets */
    plog("[Initializing arrays... (player presets)]");
    if (init_player_presets()) quit("Cannot initialize player presets");

    /* Initialize some other arrays */
    plog("[Initializing arrays... (other)]");
    if (init_other()) quit("Cannot initialize other stuff");

    /* Initialize some other arrays */
    plog("[Initializing arrays... (alloc)]");
    if (init_alloc()) quit("Cannot initialize alloc stuff");

    /* Initialize randart info */
    if (cfg_random_artifacts)
    {
        plog("[Initializing arrays... (randarts)]");
        init_randart_generator();
    }

    /* Hack -- all done */
    plog("[Initializing arrays... done]");
}


/*
 * Dirty hack -- unset server options
 */
static void unload_server_cfg(void)
{
    string_free(cfg_meta_address);
    string_free(cfg_bind_name);
    string_free(cfg_report_address);
    string_free(cfg_console_password);
    string_free(cfg_dungeon_master);
    string_free(cfg_load_pref_file);
}


void cleanup_angband(void)
{
    int i;

    /* Stop the main loop */
    remove_timer_tick();

    /* Caves */
    for (i = 0 - MAX_WILD; i < MAX_DEPTH; i++)
    {
        if (cave_get(i))
            dealloc_dungeon_level(i);
    }

    /* Stop the network server */
    Stop_net_server();

    free_randart_generator();

    /* Free options from mangband.cfg */
    unload_server_cfg();

    /* Misc */
    wipe_player_names();

    /* Free the allocation tables */
    free_obj_alloc();
    mem_free(alloc_race_table);
    mem_free(alloc_ego_table);

    /* Free the stores */
    if (stores) free_stores();

    /* Free the random name fragments */
    cleanup_parser(&names_parser);

    /* Free attr/chars used for dumps */
    mem_free(f_char_s);
    mem_free(f_attr_s);
    mem_free(r_char_s);
    mem_free(r_attr_s);

    /* Free the object list */
    objects_destroy();

    /* Free the "quarks" */
    quarks_free();

    /* Free the info arrays */
    cleanup_parser(&k_parser);
    cleanup_parser(&a_parser);
    cleanup_parser(&f_parser);
    cleanup_parser(&kb_parser);
    cleanup_parser(&e_parser);
    cleanup_parser(&rb_parser);
    cleanup_parser(&r_parser);
    cleanup_parser(&mp_parser);
    cleanup_parser(&p_parser);
    cleanup_parser(&c_parser);
    cleanup_parser(&v_parser);
    cleanup_parser(&h_parser);
    cleanup_parser(&flavor_parser);
    cleanup_parser(&soc_parser);
    cleanup_parser(&hints_parser);
    cleanup_parser(&s_parser);
    cleanup_parser(&pit_parser);
    cleanup_parser(&z_parser);

    /* Free the format() buffer */
    vformat_kill();

    /* Free the directories */
    free_file_paths();
}


static bool str_to_boolean(char * str)
{
    /* false by default */
    return !(strcasecmp(str, "true"));
}


/*
 * Try to set a server option.  This is handled very sloppily right now,
 * since server options are manually mapped to global variables.  Maybe
 * the handeling of this will be unified in the future with some sort of
 * options structure.
 */
static void set_server_option(char *option, char *value)
{
    /* Due to the lame way that C handles strings, we can't use a switch statement */
    if (!strcmp(option, "REPORT_TO_METASERVER"))
        cfg_report_to_meta = str_to_boolean(value);
    else if (!strcmp(option, "META_ADDRESS"))
    {
        string_free(cfg_meta_address);
        cfg_meta_address = string_make(value);
    }
    else if (!strcmp(option, "BIND_NAME"))
    {
        string_free(cfg_bind_name);
        cfg_bind_name = string_make(value);
    }
    else if (!strcmp(option, "REPORT_ADDRESS"))
    {
        string_free(cfg_report_address);
        cfg_report_address = string_make(value);
    }
    else if (!strcmp(option, "CONSOLE_PASSWORD"))
    {
        string_free(cfg_console_password);
        cfg_console_password = string_make(value);
    }
    else if (!strcmp(option, "DUNGEON_MASTER_NAME"))
    {
        string_free(cfg_dungeon_master);
        cfg_dungeon_master = string_make(value);
        plog_fmt("Dungeon Master Set as [%s]", cfg_dungeon_master);
    }
    else if (!strcmp(option, "SECRET_DUNGEON_MASTER"))
        cfg_secret_dungeon_master = str_to_boolean(value);
    else if (!strcmp(option, "FPS"))
    {
        cfg_fps = atoi(value);

        /* Hack -- Reinstall the timer handler to match the new FPS */
        install_timer_tick(dungeon, cfg_fps);
    }
    else if (!strcmp(option, "NO_STEAL"))
        cfg_no_steal = str_to_boolean(value);
    else if (!strcmp(option, "NEWBIES_CANNOT_DROP"))
        cfg_newbies_cannot_drop = str_to_boolean(value);
    else if (!strcmp(option, "LEVEL_UNSTATIC_CHANCE"))
        cfg_level_unstatic_chance = atoi(value);
    else if (!strcmp(option, "RETIRE_TIMER"))
        cfg_retire_timer = atoi(value);
    else if (!strcmp(option, "ALLOW_RANDOM_ARTIFACTS"))
        cfg_random_artifacts = str_to_boolean(value);
    else if (!strcmp(option, "LIMIT_STAIRS"))
    {
        cfg_limit_stairs = atoi(value);

        /* Sanity checks */
        if (cfg_limit_stairs < 0) cfg_limit_stairs = 0;
        if (cfg_limit_stairs > 2) cfg_limit_stairs = 2;
    }
    else if (!strcmp(option, "NO_RECALL"))
        cfg_no_recall = str_to_boolean(value);
    else if (!strcmp(option, "NO_GHOST"))
        cfg_no_ghost = str_to_boolean(value);
    else if (!strcmp(option, "MORE_TOWNS"))
        cfg_more_towns = str_to_boolean(value);
    else if (!strcmp(option, "ARTIFACT_DROP_SHALLOW"))
        cfg_artifact_drop_shallow = str_to_boolean(value);
    else if (!strcmp(option, "LIMIT_PLAYER_CONNECTIONS"))
        cfg_limit_player_connections = str_to_boolean(value);
    else if (!strcmp(option, "MAX_TOWNIES"))
        cfg_max_townies = atoi(value);
    else if (!strcmp(option, "MAX_TREES"))
        cfg_max_trees = atoi(value);
    else if (!strcmp(option, "TCP_PORT"))
    {
        cfg_tcp_port = atoi(value);

        /* We probably ought to do some sanity check here */
        if (cfg_tcp_port & 0x01) /* Odd number */
            cfg_tcp_port++;
        if ((cfg_tcp_port > 65535) || (cfg_tcp_port < 1))
            cfg_tcp_port = 18346;
    }
    else if (!strcmp(option, "CHARACTER_DUMP_COLOR"))
        cfg_chardump_color = str_to_boolean(value);
    else if (!strcmp(option, "TOWN_WALL"))
        cfg_town_wall = str_to_boolean(value);
    else if (!strcmp(option, "PVP_HOSTILITY"))
        cfg_pvp_hostility = atoi(value);
    else if (!strcmp(option, "BASE_MONSTERS"))
        cfg_base_monsters = str_to_boolean(value);
    else if (!strcmp(option, "EXTRA_MONSTERS"))
        cfg_extra_monsters = str_to_boolean(value);
    else if (!strcmp(option, "MAX_HOUSES"))
        cfg_max_houses = atoi(value);
    else if (!strcmp(option, "GHOST_DIVING"))
        cfg_ghost_diving = str_to_boolean(value);
    else if (!strcmp(option, "CONSOLE_LOCAL_ONLY"))
        cfg_console_local_only = str_to_boolean(value);
    else if (!strcmp(option, "LOAD_PREF_FILE"))
    {
        string_free(cfg_load_pref_file);
        cfg_load_pref_file = string_make(value);
    }
    else if (!strcmp(option, "PRESERVE_ARTIFACTS"))
        cfg_preserve_artifacts = atoi(value);
    else if (!strcmp(option, "SAFE_RECHARGE"))
        cfg_safe_recharge = str_to_boolean(value);
    else if (!strcmp(option, "PARTY_SHARELEVEL"))
        cfg_party_sharelevel = atoi(value);
    else if (!strcmp(option, "SMALL_RANGE"))
        cfg_small_range = str_to_boolean(value);
    else if (!strcmp(option, "AI_PACKS"))
        cfg_ai_packs = str_to_boolean(value);
    else if (!strcmp(option, "AI_SMART"))
        cfg_ai_smart = str_to_boolean(value);
    else if (!strcmp(option, "AI_SMELL"))
        cfg_ai_smell = str_to_boolean(value);
    else if (!strcmp(option, "AI_LEARN"))
        cfg_ai_learn = str_to_boolean(value);
    else if (!strcmp(option, "AI_CHEAT"))
        cfg_ai_cheat = str_to_boolean(value);
    else plog_fmt("Error : unrecognized mangband.cfg option %s", option);
}


/*
 * Parse the loaded mangband.cfg file, and if a valid expression was found
 * try to set it using set_server_option.
 */
static void load_server_cfg_aux(ang_file *cfg)
{
    char line[256];
    char *newword;
    char *option;
    char *value;
    bool first_token;

    /* Read in lines until we hit EOF */
    while (file_getl(cfg, line, sizeof(line)))
    {
        /* Parse the line that has been read in */
        /* If the line begins with a # or is empty, ignore it */
        if ((line[0] == '#') || (line[0] == '\0')) continue;

        /* Reset option and value */
        option = NULL;
        value = NULL;

        /*
         * Split the line up into words
         * We pass the string to be parsed to strsep on the first call,
         * and subsequently pass it null.
         */
        first_token = 1;
        newword = strtok(first_token ? line : NULL, " ");
        while (newword)
        {
            first_token = 0;

            /* Set the option or value */
            if (!option) option = newword;
            else if ((!value) && (newword[0] != '='))
            {
                value = newword;

                /* Hack -- ignore "" around values */
                if (value[0] == '"') value++;
                if (value[strlen(value) - 1] == '"')
                    value[strlen(value) - 1] = '\0';
            }

            /* If we have a completed option and value, then try to set it */
            if (option && value)
            {
                set_server_option(option, value);
                break;
            }

            newword = strtok(first_token? line: NULL, " ");
        }
    }
}


/*
 * Load in the mangband.cfg file.  This is a file that holds many
 * options that have historically been #defined in config.h.
 */
void load_server_cfg(void)
{
    ang_file *cfg;

    /* Attempt to open the file */
    cfg = file_open("mangband.cfg", MODE_READ, FTYPE_TEXT);

    /* Failure */
    if (!cfg)
    {
        plog("Error : cannot open file mangband.cfg");
        return;
    }

    /* Default */
    cfg_fps = 12;

    /* Actually parse the file */
    load_server_cfg_aux(cfg);

    /* Close it */
    file_close(cfg);
}


/*
 * This table allows quick conversion from "speed" to "energy"
 * The basic function WAS ((S>=110) ? (S-110) : (100 / (120-S)))
 * Note that table access is *much* quicker than computation.
 *
 * Note that the table has been changed at high speeds.  From
 * "Slow (-40)" to "Fast (+30)" is pretty much unchanged, but
 * at speeds above "Fast (+30)", one approaches an asymptotic
 * effective limit of 50 energy per turn.  This means that it
 * is relatively easy to reach "Fast (+30)" and get about 40
 * energy per turn, but then speed becomes very "expensive",
 * and you must get all the way to "Fast (+50)" to reach the
 * point of getting 45 energy per turn.  After that point,
 * furthur increases in speed are more or less pointless,
 * except to balance out heavy inventory.
 *
 * Note that currently the fastest monster is "Fast (+30)".
 */
u16b extract_energy[200] =
{
    /* Slow */     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,
    /* Slow */     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,
    /* Slow */     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,
    /* Slow */     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,
    /* Slow */     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,
    /* Slow */     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,
    /* S-50 */     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,
    /* S-40 */     200,  200,  200,  200,  200,  200,  200,  200,  200,  200,
    /* S-30 */     200,  200,  200,  200,  200,  200,  200,  300,  300,  300,
    /* S-20 */     300,  300,  300,  300,  300,  400,  400,  400,  400,  400,
    /* S-10 */     500,  500,  500,  500,  600,  600,  700,  700,  800,  900,
    /* Norm */    1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900,
    /* F+10 */    2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900,
    /* F+20 */    3000, 3100, 3200, 3300, 3400, 3500, 3600, 3600, 3700, 3700,
    /* F+30 */    3800, 3800, 3900, 3900, 4000, 4000, 4000, 4100, 4100, 4100,
    /* F+40 */    4200, 4200, 4200, 4300, 4300, 4300, 4400, 4400, 4400, 4400,
    /* F+50 */    4500, 4500, 4500, 4500, 4500, 4600, 4600, 4600, 4600, 4600,
    /* F+60 */    4700, 4700, 4700, 4700, 4700, 4800, 4800, 4800, 4800, 4800,
    /* F+70 */    4900, 4900, 4900, 4900, 4900, 4900, 4900, 4900, 4900, 4900,
    /* Fast */    4900, 4900, 4900, 4900, 4900, 4900, 4900, 4900, 4900, 4900
};


int get_energy(int speed)
{
    return extract_energy[speed] / 100;
}
