/*
 * File: files.c
 * Purpose: Various file-related activities, poorly organised
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
#include "../common/buildid.h"
#include "../common/display.h"
#include "../common/parser.h"
#include "../common/spells.h"
#include "files.h"
#include "history.h"
#include "netserver.h"


/*** Pref file parser ***/


/*
 * Private data for pref file parsing.
 */
typedef struct prefs_data
{
    bool bypass;
    bool skip;
    int mode;
    int nrace;
    int nclass;
} prefs_data;


/*
 * Load another file.
 */
static enum parser_error parse_prefs_load(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);
    const char *file;

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    file = parser_getstr(p, "file");
    process_pref_file(file, TRUE);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_expr(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);

    /* Hack - Do not load any Evaluated Expressions */
    parser_getstr(p, "expr");

    /* Set flag */
    d->bypass = d->skip;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_k(struct parser *p)
{
    int tvi, svi;
    object_kind *kind;
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    tvi = tval_find_idx(parser_getsym(p, "tval"));
    if (tvi < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    svi = lookup_sval(tvi, parser_getsym(p, "sval"));
    if (svi < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;

    kind = lookup_kind(tvi, svi);
    if (!kind) return PARSE_ERROR_UNRECOGNISED_SVAL;

    kind->x_attr = (byte)parser_getint(p, "attr");
    kind->x_char = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_r(struct parser *p)
{
    int idx;
    monster_race *monster;
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    if (idx >= z_info->r_max) return PARSE_ERROR_OUT_OF_BOUNDS;

    monster = &r_info[idx];
    monster->x_attr = (byte)parser_getint(p, "attr");
    monster->x_char = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_f(struct parser *p)
{
    int idx;
    feature_type *feature;
    const char *lighting;
    int light_idx;
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    if (idx >= z_info->f_max) return PARSE_ERROR_OUT_OF_BOUNDS;

    lighting = parser_getsym(p, "lighting");
    if (streq(lighting, "bright"))
        light_idx = FEAT_LIGHTING_BRIGHT;
    else if (streq(lighting, "lit"))
        light_idx = FEAT_LIGHTING_LIT;
    else if (streq(lighting, "dark"))
        light_idx = FEAT_LIGHTING_DARK;
    else if (streq(lighting, "all"))
        light_idx = FEAT_LIGHTING_MAX;
    else
        return PARSE_ERROR_GENERIC;

    if (light_idx < FEAT_LIGHTING_MAX)
    {
        feature = &f_info[idx];
        feature->x_attr[light_idx] = (byte)parser_getint(p, "attr");
        feature->x_char[light_idx] = (char)parser_getint(p, "char");
    }
    else
    {
        for (light_idx = 0; light_idx < FEAT_LIGHTING_MAX; light_idx++)
        {
            feature = &f_info[idx];
            feature->x_attr[light_idx] = (byte)parser_getint(p, "attr");
            feature->x_char[light_idx] = (char)parser_getint(p, "char");
        }
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_gf(struct parser *p)
{
    bool types[GF_MAX] = { 0 };
    const char *direction;
    int motion, motion2 = 0;
    char *s, *t;
    size_t i;
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    /* Parse the type, which is a | seperated list of GF_ constants */
    s = string_make(parser_getsym(p, "type"));
    t = strtok(s, "| ");
    while (t)
    {
        if (streq(t, "*"))
            memset(types, TRUE, sizeof(types));
        else
        {
            int idx = gf_name_to_idx(t);

            if (idx == -1) return PARSE_ERROR_INVALID_VALUE;

            types[idx] = TRUE;
        }

        t = strtok(NULL, "| ");
    }

    string_free(s);

    direction = parser_getsym(p, "direction");
    if (streq(direction, "static"))
        motion = BOLT_NO_MOTION;
    else if (streq(direction, "0"))
    {
        motion = BOLT_0;
        motion2 = BOLT_180;
    }
    else if (streq(direction, "45"))
    {
        motion = BOLT_45;
        motion2 = BOLT_225;
    }
    else if (streq(direction, "90"))
    {
        motion = BOLT_90;
        motion2 = BOLT_270;
    }
    else if (streq(direction, "135"))
    {
        motion = BOLT_135;
        motion2 = BOLT_315;
    }
    else if (streq(direction, "180"))
        motion = BOLT_180;
    else if (streq(direction, "225"))
        motion = BOLT_225;
    else if (streq(direction, "270"))
        motion = BOLT_270;
    else if (streq(direction, "315"))
        motion = BOLT_315;
    else
        return PARSE_ERROR_INVALID_VALUE;

    for (i = 0; i < GF_MAX; i++)
    {
        if (!types[i]) continue;

        gf_to_attr[i][motion] = (byte)parser_getuint(p, "attr");
        gf_to_char[i][motion] = (char)parser_getuint(p, "char");

        /* Default values */
        if (motion2)
        {
            gf_to_attr[i][motion2] = (byte)parser_getuint(p, "attr");
            gf_to_char[i][motion2] = (char)parser_getuint(p, "char");
        }
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_l(struct parser *p)
{
    unsigned int idx;
    struct flavor *flavor;
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    for (flavor = flavors; flavor; flavor = flavor->next)
    {
        if (flavor->fidx == idx) break;
    }

    if (flavor)
    {
        flavor->x_attr = (byte)parser_getint(p, "attr");
        flavor->x_char = (char)parser_getint(p, "char");
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_e(struct parser *p)
{
    int tvi, a;
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    tvi = tval_find_idx(parser_getsym(p, "tval"));
    if ((tvi < 0) || (tvi >= (long)N_ELEMENTS(tval_to_attr)))
        return PARSE_ERROR_UNRECOGNISED_TVAL;

    a = parser_getint(p, "attr");
    if (a) tval_to_attr[tvi] = (byte) a;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_rf(struct parser *p)
{
    /* Hack -- Parser hook for female player presets */
    return parse_prefs_r(p);
}


static struct parser *init_parse_prefs(void)
{
    struct parser *p = parser_new();
    struct prefs_data *d = mem_zalloc(sizeof(struct prefs_data));

    d->skip = TRUE;

    parser_setpriv(p, d);
    parser_reg(p, "% str file", parse_prefs_load);
    parser_reg(p, "? str expr", parse_prefs_expr);
    parser_reg(p, "K sym tval sym sval int attr int char", parse_prefs_k);
    parser_reg(p, "R uint idx int attr int char", parse_prefs_r);
    parser_reg(p, "F uint idx sym lighting int attr int char", parse_prefs_f);
    parser_reg(p, "GF sym type sym direction uint attr uint char", parse_prefs_gf);
    parser_reg(p, "L uint idx int attr int char", parse_prefs_l);
    parser_reg(p, "E sym tval int attr", parse_prefs_e);

    /* Hack -- Parser hook for female player presets */
    parser_reg(p, "RF uint idx int attr int char", parse_prefs_rf);

    return p;
}


static void print_error(const char *name, struct parser *p)
{
    struct parser_state s;

    parser_getstate(p, &s);
    plog_fmt("Parse error in %s line %d column %d: %s: %s", name,
        s.line, s.col, s.msg, parser_error_str[s.error]);
}


/*
 * Process the "user pref file" with the given name
 * "quiet" means "don't complain about not finding the file.
 *
 * Returns TRUE if everything worked OK, false otherwise
 */
bool process_pref_file(const char *name, bool quiet)
{
    char buf[MSG_LEN];
    ang_file *f;
    struct parser *p;
    errr e = 0;
    int line_no = 0;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_PREF, name);
    if (!file_exists(buf)) path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

    f = file_open(buf, MODE_READ, FTYPE_TEXT);
    if (!f)
    {
        if (!quiet) plog_fmt("Cannot open '%s'.", buf);
    }
    else
    {
        char line[MSG_LEN];

        p = init_parse_prefs();

        while (file_getl(f, line, sizeof(line)))
        {
            line_no++;

            e = parser_parse(p, line);
            if (e != PARSE_ERROR_NONE)
            {
                print_error(buf, p);
                break;
            }
        }

        file_close(f);
        mem_free(parser_priv(p));
        parser_destroy(p);
    }

    /* Result */
    return (e == PARSE_ERROR_NONE);
}


static enum parser_error parse_xprefs_r(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);
    int i;

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    parser_getuint(p, "idx");

    d->nrace++;
    if (d->nrace == MAX_RACES)
    {
        d->nrace = 0;
        d->nclass++;
    }

    /* Hack -- Default player presets */
    for (i = 0; i < MAX_SEXES; i++)
    {
        player_presets[d->mode][d->nclass][d->nrace][i].a = (byte)parser_getint(p, "attr");
        player_presets[d->mode][d->nclass][d->nrace][i].c = (char)parser_getint(p, "char");
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_xprefs_rf(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);

    my_assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    parser_getuint(p, "idx");

    /* Hack -- Player presets for female characters */
    player_presets[d->mode][d->nclass][d->nrace][SEX_FEMALE].a = (byte)parser_getint(p, "attr");
    player_presets[d->mode][d->nclass][d->nrace][SEX_FEMALE].c = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_xprefs(int mode)
{
    struct parser *p = parser_new();
    struct prefs_data *d = mem_zalloc(sizeof(struct prefs_data));

    d->mode = mode;
    d->nrace = MAX_RACES - 1;
    d->nclass = -1;

    parser_setpriv(p, d);
    parser_reg(p, "? str expr", parse_prefs_expr);
    parser_reg(p, "R uint idx int attr int char", parse_xprefs_r);
    parser_reg(p, "RF uint idx int attr int char", parse_xprefs_rf);

    return p;
}


/*
 * Process the "user pref file" with the given name
 *
 * Returns TRUE if everything worked OK, false otherwise
 */
static bool process_pref_file_xtra_aux(int mode, const char *name)
{
    char buf[MSG_LEN];
    ang_file *f;
    struct parser *p;
    errr e = 0;
    int line_no = 0;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_PREF, name);
    if (!file_exists(buf)) path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

    f = file_open(buf, MODE_READ, FTYPE_TEXT);
    if (!f)
        plog_fmt("Cannot open '%s'.", buf);
    else
    {
        char line[MSG_LEN];

        p = init_parse_xprefs(mode);

        while (file_getl(f, line, sizeof(line)))
        {
            line_no++;

            e = parser_parse(p, line);
            if (e != PARSE_ERROR_NONE)
            {
                print_error(buf, p);
                break;
            }
        }

        file_close(f);
        mem_free(parser_priv(p));
        parser_destroy(p);
    }

    /* Result */
    return (e == PARSE_ERROR_NONE);
}


static enum parser_error parse_pprefs_p(struct parser *p)
{
    int *pint = parser_priv(p);

    my_assert(pint != NULL);

    *pint = parser_getint(p, "mode");

    /* Read player presets from pref files */
    process_pref_file_xtra_aux(*pint, parser_getstr(p, "file"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_pprefs_n(struct parser *p)
{
    int *pint = parser_priv(p);
    int idx;

    my_assert(pint != NULL);

    idx = parser_getint(p, "idx");

    player_numbers[*pint][idx].a = (byte)parser_getint(p, "attr");
    player_numbers[*pint][idx].c = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_pprefs(void)
{
    struct parser *p = parser_new();
    int *pint = mem_zalloc(sizeof(int));

    parser_setpriv(p, pint);
    parser_reg(p, "P int mode str file", parse_pprefs_p);
    parser_reg(p, "N int idx int attr int char", parse_pprefs_n);

    return p;
}


bool process_pref_file_xtra(const char *name)
{
    char buf[MSG_LEN];
    ang_file *f;
    struct parser *p;
    errr e = 0;
    int line_no = 0;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_PREF, name);
    if (!file_exists(buf)) path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

    f = file_open(buf, MODE_READ, FTYPE_TEXT);
    if (!f)
        plog_fmt("Cannot open '%s'.", buf);
    else
    {
        char line[MSG_LEN];

        p = init_parse_pprefs();

        while (file_getl(f, line, sizeof(line)))
        {
            line_no++;

            e = parser_parse(p, line);
            if (e != PARSE_ERROR_NONE)
            {
                print_error(buf, p);
                break;
            }
        }

        file_close(f);
        mem_free(parser_priv(p));
        parser_destroy(p);
    }

    /* Result */
    return (e == PARSE_ERROR_NONE);
}


/*** Status line display functions ***/


/*
 * Print the status line.
 */
static void update_statusline(int Ind)
{
    int row = 1;
    int col = 0;

    /* Set the hooks */
    put_str_hook = buffer_put_str;

    /* Clear the buffer */
    buffer_clear();

    col += display_depth(Ind, row, col);

    /* Print the status line */
    display_statusline(Ind, row, col);
}


/*** Utility functions for character dumps ***/


/*
 * Display the character in a file (two different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank.
 *
 * Mode FALSE = standard display with skills/history
 * Mode TRUE = special display with equipment flags
 *
 * Similar to the function in c-xtra but modified to work server-side.
 * This is used for server-side character dumps.
 */
static void display_player_file(int Ind, bool mode)
{
    /* Set the hooks */
    clear_hook = buffer_clear;
    region_erase_hook = NULL;
    put_ch_hook = buffer_put_ch;
    put_str_hook = buffer_put_str;
    use_bigtile_hook = FALSE;

    /* Display the character in a file */
    display_player(Ind, mode);
}


/*
 * Hack -- Dump a character description file
 * This is for server-side character dumps
 */
errr file_character_server(int Ind, char *base, char *name, bool ladder)
{
    player_type *p_ptr = player_get(Ind);
    int i, x, y;
    byte a;
    char c;
    ang_file *fp;
    char o_name[NORMAL_WID];
    char buf[MSG_LEN];
    time_t ct = time((time_t*)0);
    char today[10];
    int j;
    int x1, x2, y1, y2;
    char attr;
    grid_data g;
    char sx;
    bool victory = streq(p_ptr->death_info.died_from, "winner");

    /* Only record the original death */
    if (p_ptr->ghost) return (-1);

    /* Build the filename */
    path_build(buf, MSG_LEN, base, name);

    /* Open the file for writing */
    fp = file_open(buf, MODE_WRITE, FTYPE_TEXT);
    if (!fp) return (-1);

    switch (p_ptr->psex)
    {
        case SEX_FEMALE: sx = 'f'; break;
        case SEX_MALE: sx = 'm'; break;
        default: sx = 'n'; break;
    }

    /*
     * Add ladder information, this line is used by the online ladder and
     * not displayed when viewing a character dump online.
     */
    if (ladder)
    {
        strftime(today, 9, "%m/%d/%y", localtime(&ct));
        x_file_putf(fp,
            "# %lu|%lu|%-.8s|%-.25s|%c|%2d|%2d|%3d|%3d|%3d|%3d|%-.31s|%s\n",
            (long)total_points(p_ptr, p_ptr->max_exp, p_ptr->max_depth), (long)p_ptr->au, today,
            p_ptr->name, sx, p_ptr->race->ridx, p_ptr->clazz->cidx, p_ptr->lev, p_ptr->depth,
            p_ptr->max_lev, p_ptr->max_depth, p_ptr->death_info.died_from, get_buildver());
    }

    /* Leave it at that for characters lower than level 20 */
    if (ladder && (p_ptr->lev < 20))
    {
        /* Close dump file */
        file_close(fp);

        /* Success */
        return (0);
    }

    /* Begin dump */
    file_putf(fp, "  [%s Character Dump]\n\n", get_buildid(FALSE));

    /* Display player */
    display_player_file(Ind, FALSE);

    /* Dump part of the screen */
    for (y = 0; y < 21; y++) x_file_putf(fp, "%s\n", buffer_line(y));

    /* Skip a line */
    file_putf(fp, "\n");

    /* Display player */
    display_player_file(Ind, TRUE);

    /* Dump part of the screen */
    for (y = 10; y < 19; y++) x_file_putf(fp, "%s\n", buffer_line(y));

    /* Skip some lines */
    file_putf(fp, "\n\n");

    /* Dump last messages */
    file_put(fp, "  [Last Messages]\n\n");
    i = p_ptr->msg_hist_ptr;
    for (j = 0; j < MAX_MSG_HIST; j++)
    {
        if (i >= MAX_MSG_HIST) i = 0;
        if (p_ptr->msg_log[i])
            x_file_putf(fp, "%s\n", p_ptr->msg_log[i]);
        i++;
    }
    if (victory) x_file_putf(fp, "\nAll Hail the Mighty %s!\n\n\n", p_ptr->sex->winner);
    else x_file_putf(fp, "\nKilled by %s.\n\n\n", p_ptr->death_info.died_from);

    /* Dump the equipment */
    file_put(fp, "  [Character Equipment]\n\n");
    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        if (i == INVEN_TOTAL)
        {
            file_putf(fp, "\n\n  [Character Quiver]\n\n");
            continue;
        }
        object_desc(NULL, o_name, sizeof(o_name), &p_ptr->inventory[i], ODESC_PREFIX | ODESC_FULL);
        x_file_putf(fp, "%c) %s\n", index_to_label(i), o_name);
    }

    /* Dump the inventory */
    file_putf(fp, "\n\n  [Character Inventory]\n\n");
    for (i = 0; i < INVEN_PACK; i++)
    {
        object_desc(NULL, o_name, sizeof(o_name), &p_ptr->inventory[i], ODESC_PREFIX | ODESC_FULL);
        x_file_putf(fp, "%c) %s\n", index_to_label(i), o_name);
    }
    file_put(fp, "\n\n");

    /* Dump the Home if anything there */
    file_put(fp, "  [Home Inventory]\n");
    for (i = 0; i < num_houses; i++)
    {
        if (house_owned_by(p_ptr, i))
        {
            file_put(fp, "\n");
            j = 0;
            for (y = houses[i].y_1; y <= houses[i].y_2; y++)
            {
                for (x = houses[i].x_1; x <= houses[i].x_2; x++)
                {
                    object_type *o_ptr;

                    for (o_ptr = get_first_object(houses[i].depth, y, x); o_ptr;
                        o_ptr = get_next_object(o_ptr))
                    {
                        if (j > 12)
                        {
                            file_put(fp, "\n");
                            j = 0;
                        }
                        object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);
                        x_file_putf(fp,"%c) %s\n", index_to_label(j), o_name);
                        j++;
                    }
                }
            }
        }
    }
    file_put(fp, "\n\n");

    /* Dump character history */
    file_put(fp, "  [Character History]\n\n");
    history_unmask_unknown(p_ptr);
    dump_history(p_ptr, fp);

    /* Dump the scene of death */
    if (victory) file_put(fp, "  [Scene of Victory]\n\n");
    else file_put(fp, "  [Scene of Death]\n\n");

    /* Get an in bounds area */
    x1 = p_ptr->px - 39;
    x2 = p_ptr->px + 39;
    y1 = p_ptr->py - 10;
    y2 = p_ptr->py + 10;
    if (y1 < 0)
    {
        y2 = y2 - y1;
        y1 = 0;
    }
    if (x1 < 0)
    {
        x2 = x2 - x1;
        x1 = 0;
    }
    if (y2 > DUNGEON_HGT - 1)
    {
        y1 = y1 - (y2 - (DUNGEON_HGT - 1));
        y2 = DUNGEON_HGT - 1;
    }
    if (x2 > DUNGEON_WID - 1)
    {
        x1 = x1 - (x2 - (DUNGEON_WID - 1));
        x2 = DUNGEON_WID - 1;
    }

    /* Describe each row */
    for (y = y1; y <= y2; y++)
    {
        for (x = x1; x <= x2; x++)
        {
            /* Get the features */
            map_info(p_ptr, y, x, &g);
            grid_data_as_text(p_ptr, TRUE, &g, &a, &c, &a, &c);

            /* Hack for the player who is already dead and gone */
            if ((x == p_ptr->px) && (y == p_ptr->py))
            {
                c = '@';
                a = 'W';
            }

            /* Translate the attr */
            attr = color_attr_to_char(a);

            /* Config file controls if we output with color codes */
            if (cfg_chardump_color)
            {
                /* Output with attr colour code */
                file_putf(fp, "%c%c", attr, c);
            }
            else
            {
                /* Output plain ASCII */
                file_putf(fp, "%c", c);
            }
        }
        file_put(fp, "\n");
    }

    /* Prepare status line */
    update_statusline(Ind);

    /* Dump status line */
    file_putf(fp, "%s\n", buffer_line(0));
    file_put(fp, "\n\n");

    /* Close it */
    file_close(fp);

    /* Success */
    return (0);
}


/*
 * File perusal
 */
static bool do_cmd_help_aux(int Ind, const char *name, const char *what, int line, int color)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Number of "real" lines passed by */
    int next = 0;

    /* Number of "real" lines in the file */
    int size;

    /* Current help file */
    ang_file *fff = NULL;

    /* Hold a string to show */
    char shower[NORMAL_WID] = "";

    /* Describe this thing */
    char caption[128] = "";

    /* Path buffer */
    char path[MSG_LEN];

    /* General buffer */
    char buf[MSG_LEN];

    /* Hack XXX XXX XXX */
    if (what)
    {
        my_strcpy(caption, what, sizeof(caption));

        my_strcpy(path, name, sizeof(path));

        fff = file_open(path, MODE_READ, FTYPE_TEXT);
    }

    /* Look in "help" */
    if (!fff)
    {
        strnfmt(caption, sizeof(caption), "Help file '%s'", name);

        path_build(path, MSG_LEN, ANGBAND_DIR_HELP, name);

        fff = file_open(path, MODE_READ, FTYPE_TEXT);
    }

    /* Oops */
    if (!fff)
    {
        /* Message */
        msg(p_ptr, "Cannot open '%s'.", name);
        message_flush(p_ptr);

        /* Oops */
        return (TRUE);
    }

    /* Pre-Parse the file */
    while (TRUE)
    {
        /* Read a line or stop */
        if (!file_getl(fff, buf, sizeof(buf))) break;

        /* Count the "real" lines */
        next++;
    }

    /* Save the number of "real" lines */
    size = next;

    /* Restart when necessary */
    if (line >= size) player_get(Ind)->interactive_line = line = 0;

    /* Re-open the file if needed */
    if (next > line)
    {
        /* Close it */
        file_close(fff);

        /* Hack -- Re-Open the file */
        fff = file_open(path, MODE_READ, FTYPE_TEXT);
        if (!fff) return (FALSE);

        /* File has been restarted */
        next = 0;
    }

    /* Goto the selected line */
    while (next < line)
    {
        /* Get a line */
        if (!file_getl(fff, buf, sizeof(buf))) break;

        /* Count the lines */
        next++;
    }

    /* Dump the next 20 lines of the file */
    for (i = 0; i < 20; )
    {
        byte attr = TERM_WHITE;

        /* Hack -- Track the "first" line */
        if (!i) line = next;

        /* Get a line of the file or stop */
        if (!file_getl(fff, buf, sizeof(buf))) break;

        /* Count the "real" lines */
        next++;

        /* Extract color */
        if (color) attr = color_char_to_attr(buf[0]);

        /* Highlight "shower" */
        if (shower[0] && strstr(buf, shower)) attr = TERM_YELLOW;

        /* Dump the line */
        Send_special_line(Ind, size - 1, size - 1 - line, i, attr, &buf[color]);

        /* Count the printed lines */
        i++;
    }

    /* Inform about empty file/list */
    if (!i) Send_special_line(Ind, 0, 0, 0, TERM_WHITE, "  (nothing)");

    /* Close the file */
    file_close(fff);

    /* Normal return */
    return (TRUE);
}


/*
 * Hack -- display the contents of a file on the screen
 *
 * XXX XXX XXX Use this function for commands such as the
 * "examine object" command.
 */
errr show_file(int Ind, const char *name, const char *what, int line, int color)
{
    player_type *p_ptr = player_get(Ind);

    /* Protect our header */
    alloc_header_icky(p_ptr, what);

    /* Peruse the requested file */
    do_cmd_help_aux(Ind, name, get_header(p_ptr, what), line, color);

    /* Success */
    return (0);
}


/*
 * Process the player name
 */
int process_player_name_aux(const char *name, const char *base, const char *save, bool sf)
{
    int i, k = 0;
    char local_base[NORMAL_WID];
    char *basename = (char*)base;

    /* Cannot be too long */
    if (strlen(name) > 15)
    {
        /* Abort */
        return -1;
    }

    /* Cannot contain "icky" characters */
    for (i = 0; name[i]; i++)
    {
        /* No control characters */
        if (iscntrl(name[i]))
        {
            /* Abort */
            return -2;
        }
    }

    if (base == NULL) basename = &local_base[0];

    /* Extract "useful" letters */
    for (i = 0; name[i]; i++)
    {
        char c = name[i];

        /* Accept some letters */
        if (isalpha(c) || isdigit((unsigned char)c)) basename[k++] = c;

        /* Convert space, dot, and underscore to underscore */
        else if (strchr(". _", c)) basename[k++] = '_';
    }

    /* Terminate */
    basename[k] = '\0';

    /* Require a "base" name */
    if (!basename[0])
        my_strcpy(basename, "PLAYER", sizeof(basename));

    /* Accept */
    sf = TRUE;

    /* Change the savefile name */
    if (sf)
    {
        char temp[128];
        memset(temp, 0, 128);

        /* Rename the savefile, using the player_base */
        my_strcpy(temp, basename, sizeof(temp));

        /* Build the filename */
        path_build((char*)save, MSG_LEN, ANGBAND_DIR_SAVE, temp);
    }

    /* Success */
    return 0;
}


/*
 * Process the player name.
 * Extract a clean "base name".
 * Build the savefile name if needed.
 */
bool process_player_name(struct player *p, bool sf)
{
    int ret;

    ret = process_player_name_aux(p->name, p->other.base_name, p->savefile, sf);

    /* Cannot be too long */
    if (ret == -1)
    {
        /* Name too long */
        Destroy_connection(p->conn, "Your name is too long!");

        /* Abort */
        return FALSE;
    }

    /* Cannot contain "icky" characters */
    if (ret == -2)
    {
        /* Illegal characters */
        Destroy_connection(p->conn, "Your name contains control chars!");

        /* Abort */
        return FALSE;
    }

    /* Success */
    return TRUE;
}


/*
 * Change a player into a King!
 */
void kingly(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Hack -- Retire in town */
    p_ptr->depth = 0;

    /* Fake death */
    my_strcpy(p_ptr->death_info.died_from, "winner", sizeof(p_ptr->death_info.died_from));

    /* Restore the experience */
    p_ptr->exp = p_ptr->max_exp;

    /* Restore the level */
    p_ptr->lev = p_ptr->max_lev;

    /* Hack -- Player gets an XP bonus for beating the game */
    p_ptr->exp = p_ptr->max_exp += 10000000L;

    /* Hack -- Ensure we are retired */
    p_ptr->retire_timer = 0;
}


/*
 * Save a character
 */
static void save_game(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Disturb the player */
    disturb(p_ptr, 1, 0);

    /* Clear messages */
    message_flush(p_ptr);

    /* Handle stuff */
    handle_stuff(p_ptr);

    /* Message */
    msg(p_ptr, "Saving game...");

    /* The player is not dead */
    my_strcpy(p_ptr->died_from, "(saved)", sizeof(p_ptr->died_from));

    /* Save the player */
    if (save_player(Ind))
        msg(p_ptr, "Saving game... done.");

    /* Save failed (oops) */
    else
        msg(p_ptr, "Saving game... failed!");

    /* Note that the player is not dead */
    my_strcpy(p_ptr->died_from, "(alive and well)", sizeof(p_ptr->died_from));
}


/*
 * Close up the current game (player may or may not be dead)
 *
 * This function is called only from "main.c"
 *
 * In here we try to save everybody's game, as well as save the server state.
 */
void close_game(void)
{
    int i;

    for (i = 0; i < NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Handle stuff */
        handle_stuff(p_ptr);

        /* Flush the messages */
        message_flush(p_ptr);

        /* Handle death */
        if (p_ptr->is_dead)
        {
            /* Handle retirement */
            if (p_ptr->total_winner) kingly(i);

            /* Save memories */
            if (!save_player(i)) msg(p_ptr, "death save failed!");

            /* Handle score, show Top scores */
            enter_score(i, &p_ptr->death_info.time);
        }

        /* Still alive */
        else
        {
            /* Save the game */
            save_game(i);
        }
    }

    /* Try to save the server information */
    save_server_info();
}


/*
 * Handle a fatal crash.
 *
 * Here we try to save every player's state, and the state of the server
 * in general.  Note that we must be extremely careful not to have a crash
 * in this function, or some things may not get saved.  Also, this function
 * may get called because some data structures are not in a "correct" state.
 * For this reason many paranoia checks are done to prevent bad pointer
 * dereferences.
 *
 * Note that this function would not be needed at all if there were no bugs.
 */
void exit_game_panic()
{
    int i = 1;

    /* If nothing important has happened, just return */
    if (!server_generated) return;

    while (NumPlayers > (i - 1))
    {
        player_type *p_ptr = player_get(i);

        /* Don't dereference bad pointers */
        if (!p_ptr)
        {
            /* Skip to next player */
            i++;

            continue;
        }

        /* Hack -- Turn off some things */
        disturb(p_ptr, 1, 0);

        /* Hack -- Delay death */
        if (p_ptr->chp < 0) p_ptr->is_dead = FALSE;

        /* Indicate panic save */
        my_strcpy(p_ptr->died_from, "(panic save)", sizeof(p_ptr->died_from));

        /* Hack -- Unstatic if the DM left while manually designing a dungeon level */
        if (players_on_depth[p_ptr->depth] == INHIBIT_DEPTH) players_on_depth[p_ptr->depth] = 0;

        /*
         * Try to save the player, don't worry if this fails because there
         * is nothing we can do now anyway
         */
        save_player(i++);
    }

    /* Preserve artifacts on the ground */
    for (i = 1; i < MAX_DEPTH; i++) preserve_artifacts(i);

    /* Try to save the server information */
    if (!save_server_info()) plog("server panic info save failed!");

    /* Successful panic save of server info */
    else plog("server panic info save succeeded!");
}


/*
 * Windows specific replacement for signal handling
 */
LPTOP_LEVEL_EXCEPTION_FILTER old_handler;


/*
 * Callback to be called by Windows when our term closes, the user
 * logs off, the system is shutdown, etc.
 */
BOOL ctrl_handler(DWORD fdwCtrlType)
{
    /* Save everything and quit the game */
    shutdown_server();
    return TRUE;
}


/*
 * Global unhandled exception handler
 *
 * If the server crashes under Windows, this is where we end up
 */
LONG WINAPI UnhandledExceptionFilter(struct _EXCEPTION_POINTERS* ExceptionInfo)
{
    /*
     * We don't report to the meta server in this case, the meta
     * server will detect that we've gone anyway
     */

    /*
     * Call the previous exception handler, which we are assuming
     * is the MinGW exception handler which should have been implicitly
     * setup when we loaded the exchndl.dll library.
     */
    if (old_handler != NULL) old_handler(ExceptionInfo);

    /* Save everything and quit the game */
    exit_game_panic();

    /* We don't expect to ever get here... but for what it's worth... */
    return (EXCEPTION_EXECUTE_HANDLER);
}


void setup_exit_handler(void)
{
    /* Trap CTRL+C, Logoff, Shutdown, etc */
    if (SetConsoleCtrlHandler((PHANDLER_ROUTINE)ctrl_handler, TRUE))
        plog("Initialised exit save handler.");
    else
        plog("ERROR: Could not set panic save handler!");

    /* Trap unhandled exceptions, i.e. server crashes */
    old_handler = SetUnhandledExceptionFilter(UnhandledExceptionFilter);
}


/*
 * On-Line help.
 *
 * Process user commands, access sub-menu entries and browse files.
 * This function manages a virtual 'window' which buffers file
 * contents using "copy_file_info" function.
 */
void common_file_peruse(int Ind, u32b query)
{
    player_type *p_ptr = player_get(Ind);
    int next = p_ptr->interactive_next;

    /* Enter sub-menu */
    if (isalpha(query))
    {
        /* Extract the requested menu item */
        int k = A2I(query);

        /* Verify the menu item */
        if ((k >= 0) && (k < 26) && !STRZERO(p_ptr->interactive_hook[k]))
        {
            /* Select that file */
            p_ptr->interactive_file = string_make(p_ptr->interactive_hook[k]);

            /* Hack: enforce update */
            p_ptr->interactive_next = -1;
            next = 0;

            /* Query processed */
            query = 0;
        }
    }

    /* Use default file */
    if (!p_ptr->interactive_file)
    {
        p_ptr->interactive_file = string_make("help.hlp");

        /* Hack: enforce update */
        p_ptr->interactive_next = -1;
        next = 0;

        /* Query processed */
        query = 0;
    }

    /* We're just starting. Reset counter */
    if (!query) p_ptr->interactive_line = 0;

    /* We're done. Clear file, exit */
    if (query == ESCAPE)
    {
        string_free(p_ptr->interactive_file);
        p_ptr->interactive_file = NULL;
        free_info_icky(Ind);
        free_header_icky(Ind);
        return;
    }

    /* Process query */
    if (query)
    {
        switch (query)
        {
            /* Back up one line */
            case ARROW_UP:
            case '8':
            case '=':
                p_ptr->interactive_line--;
                break;

            /* Back up one full page */
            case '9':
            case '-':
                p_ptr->interactive_line -= 20;
                break;

            /* Back to the top */
            case '7':
                p_ptr->interactive_line = 0;
                break;

            /* Advance one line */
            case ARROW_DOWN:
            case '2':
            case KC_ENTER:
                p_ptr->interactive_line++;
                break;

            /* Advance one full page */
            case '3':
            case ' ':
                p_ptr->interactive_line += 20;
                break;

            /* Advance to the bottom */
            case '1':
                p_ptr->interactive_line = p_ptr->interactive_size - 20;
                break;
        }

        /* Adjust viewport boundaries */
        if (p_ptr->interactive_line > p_ptr->interactive_size - 20)
            p_ptr->interactive_line = p_ptr->interactive_size - 20;
        if (p_ptr->interactive_line < 0) p_ptr->interactive_line = 0;

        /* Shift window! */
        if ((p_ptr->interactive_line + 20 > p_ptr->interactive_next + MAX_TXT_INFO) ||
                (p_ptr->interactive_line < p_ptr->interactive_next))
            next = p_ptr->interactive_line - MAX_TXT_INFO / 2;

        /* Adjust window boundaries */
        if (next > p_ptr->interactive_size - MAX_TXT_INFO)
            next = p_ptr->interactive_size - MAX_TXT_INFO;
        if (next < 0) next = 0;
    }

    /* Update file */
    if (next != p_ptr->interactive_next)
    {
        p_ptr->interactive_next = next;
        copy_file_info(Ind, p_ptr->interactive_file, next, 0);
    }
}


static void strrep(char *s, const char c, const char r)
{
    while (*s)
    {
        if (*s == c) *s = r;
        s++;
    }
}


/*
 * Read a file and copy a portion of it into player's "info[]" array.
 *
 * TODO: Add 'search' from do_cmd_help_aux()
 */
void copy_file_info(int Ind, const char *name, int line, int color)
{
    player_type *p_ptr = player_get(Ind);
    int i = 0, k;

    /* Current help file */
    ang_file *fff = NULL;

    /* Number of "real" lines passed by */
    int next = 0;

    /* Path buffer */
    char path[MSG_LEN];

    /* General buffer */
    char buf[MSG_LEN];

    /* TRUE if we are inside a RST block that should be skipped */
    bool skip_lines = FALSE;

    /* Build the filename */
    path_build(path, MSG_LEN, ANGBAND_DIR_HELP, name);

    /* Open the file */
    fff = file_open(path, MODE_READ, FTYPE_TEXT);

    /* Oops */
    if (!fff)
    {
        /* Message */
        msg(p_ptr, "Cannot open '%s'.", name);
        message_flush(p_ptr);

        /* Oops */
        return;
    }

    /* Wipe the hooks */
    for (k = 0; k < 26; k++) p_ptr->interactive_hook[k][0] = '\0';

    /* Parse the file */
    while (TRUE)
    {
        byte attr = TERM_WHITE;

        /* Read a line or stop */
        if (!file_getl(fff, buf, sizeof(buf))) break;

        /* Skip lines if we are inside a RST directive */
        if (skip_lines)
        {
            if (contains_only_spaces(buf)) skip_lines = FALSE;
            continue;
        }

        /* Parse a very small subset of RST */
        if (prefix(buf, ".. "))
        {
            /* Parse ".. menu:: [x] filename.txt" (with exact spacing) */
            if (prefix(buf + strlen(".. "), "menu:: [") && (buf[strlen(".. menu:: [x")] == ']'))
            {
                /* Extract the menu item */
                k = A2I(buf[strlen(".. menu:: [")]);

                /* Store the menu item (if valid) */
                if ((k >= 0) && (k < 26))
                {
                    my_strcpy(p_ptr->interactive_hook[k], buf + strlen(".. menu:: [x] "),
                        sizeof(p_ptr->interactive_hook[0]));
                }
            }

            /* Skip this and enter skip mode */
            skip_lines = TRUE;
            continue;
        }

        /* Skip '|' characters */
        if (strchr(buf, '|') && !strstr(buf, "``|``"))
        {
            if (strstr(buf, "|`|"))
                strrep(buf, '|', ' ');
            else
                strskip(buf, '|');
        }

        /* Count the "real" lines */
        next++;

        /* Wait for needed one */
        if (next <= line) continue;

        /* Too much */
        if (line + MAX_TXT_INFO < next) continue;

        /* Extract color */
        if (color) attr = color_char_to_attr(buf[0]);

        /* Dump the line */
        for (k = color; k < NORMAL_WID; k++)
        {
            p_ptr->info[i][k - color].a = attr;
            p_ptr->info[i][k - color].c = buf[k];
        }

        /* Count the "info[]" lines */
        i++;
    }

    /* Save last "real" line */
    p_ptr->interactive_size = next;

    /* Save last dumped line */
    p_ptr->last_info_line = i - 1;

    /* Protect our info area */
    free_info_icky(Ind);
    free_header_icky(Ind);
    alloc_info_icky(p_ptr);
    alloc_header_icky(p_ptr, "Help");

    /* Close the file */
    file_close(fff);
}
