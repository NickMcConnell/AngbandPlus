/*
 * File: ui-prefs.c
 * Purpose: Pref file handling code
 *
 * Copyright (c) 2003 Takeshi Mogami, Robert Ruehlmann
 * Copyright (c) 2007 Pete Mack
 * Copyright (c) 2010 Andi Sidwell
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


#include "c-angband.h"


int use_graphics;   /* The "graphics" mode is enabled */


/*
 * Pref file saving code
 */


/*
 *  Header and footer marker string for pref file dumps
 */
static const char *dump_separator = "#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#";


/*
 * Remove old lines from pref files
 */
static void remove_old_dump(const char *cur_fname, const char *mark)
{
    bool between_marks = false;
    bool changed = false;
    int skip_one = 0;
    char buf[MSG_LEN];
    char start_line[MSG_LEN];
    char end_line[MSG_LEN];
    char new_fname[MSG_LEN];
    ang_file *new_file;
    ang_file *cur_file;

    /* Format up some filenames */
    strnfmt(new_fname, sizeof(new_fname), "%s.new", cur_fname);

    /* Work out what we expect to find */
    strnfmt(start_line, sizeof(start_line), "%s begin %s", dump_separator, mark);
    strnfmt(end_line, sizeof(end_line), "%s end %s", dump_separator, mark);

    /* Open current file */
    cur_file = file_open(cur_fname, MODE_READ, FTYPE_TEXT);
    if (!cur_file) return;

    /* Open new file */
    new_file = file_open(new_fname, MODE_WRITE, FTYPE_TEXT);
    if (!new_file)
    {
        c_msg_print(format("Failed to create file %s", new_fname));
        return;
    }

    /* Loop for every line */
    while (file_getl(cur_file, buf, sizeof(buf)))
    {
        /* Turn on at the start line, turn off at the finish line */
        if (!strcmp(buf, start_line))
            between_marks = true;
        else if (!strcmp(buf, end_line))
        {
            between_marks = false;
            skip_one = 3;
            changed = true;
        }

        /* Copy original line */
        if (!between_marks && !skip_one)
            file_putf(new_file, "%s\n", buf);

        if (skip_one) skip_one--;
    }

    /* Close files */
    file_close(cur_file);
    file_close(new_file);

    /* If there are changes use the new file. otherwise just destroy it */
    if (changed)
    {
        char old_fname[MSG_LEN];

        strnfmt(old_fname, sizeof(old_fname), "%s.old", cur_fname);

        if (file_move(cur_fname, old_fname))
        {
            file_move(new_fname, cur_fname);
            file_delete(old_fname);
        }
    }
    else
        file_delete(new_fname);
}


/*
 * Output the header of a pref-file dump
 */
static void pref_header(ang_file *fff, const char *mark)
{
    /* Start of dump */
    file_putf(fff, "%s begin %s\n", dump_separator, mark);

    file_put(fff, "# *Warning!*  The lines below are an automatic dump.\n");
    file_put(fff, "# Don't edit them; changes will be deleted and replaced automatically.\n");
}


/*
 * Output the footer of a pref-file dump
 */
static void pref_footer(ang_file *fff, const char *mark)
{
    file_put(fff, "# *Warning!*  The lines above are an automatic dump.\n");
    file_put(fff, "# Don't edit them; changes will be deleted and replaced automatically.\n");

    /* End of dump */
    file_putf(fff, "%s end %s\n", dump_separator, mark);
}


/*
 * Dump colors
 */
void dump_colors(ang_file *fff)
{
    int i;

    for (i = 0; i < MAX_COLORS; i++)
    {
        int kv = angband_color_table[i][0];
        int rv = angband_color_table[i][1];
        int gv = angband_color_table[i][2];
        int bv = angband_color_table[i][3];
        const char *name = "unknown";

        /* Skip non-entries */
        if (!kv && !rv && !gv && !bv) continue;

        /* Extract the color name */
        if (i < BASIC_COLORS) name = color_table[i].name;

        file_putf(fff, "# Color: %s\n", name);
        file_putf(fff, "color:%d:%d:%d:%d:%d\n\n", i, kv, rv, gv, bv);
    }
}


/*
 * Write all current options to a user preference file.
 */
void option_dump(ang_file *f)
{
    int i;

    /* Dump options */
    for (i = 0; i < OPT_MAX; i++)
    {
        const char *name = option_name(i);

        if (!name) continue;

        /* Comment */
        file_putf(f, "# Option '%s'\n", option_desc(i));

        /* Dump the option */
        file_putf(f, "%c:%s\n", (Client_setup.options[i]? 'Y': 'X'), name);

        /* Skip a line */
        file_put(f, "\n");
    }

    file_putf(f, "# Hitpoint warning (0-9)\nO:hp_warn_factor:%d\n\n", player->other.hitpoint_warn);
    file_putf(f, "# Base delay factor (0-255)\nO:delay_factor:%d\n\n", player->other.delay_factor);
    file_putf(f, "# Movement delay factor (0-9)\nO:lazymove_delay:%d\n\n",
        player->other.lazymove_delay);
}


/*
 * Write all current window settings to a user preference file.
 */
void window_dump(ang_file *f)
{
    int i, j;

    /* Dump window flags */
    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        /* Require a real window */
        if (!angband_term[i]) continue;

        /* Check each flag */
        for (j = 0; j < (int)N_ELEMENTS(window_flag_desc); j++)
        {
            /* Require a real flag */
            if (!window_flag_desc[j]) continue;

            /* Comment */
            file_putf(f, "# Window '%s', Flag '%s'\n",
                angband_term_name[i], window_flag_desc[j]);

            /* Dump the flag */
            if (window_flag[i] & (1L << j))
                file_putf(f, "window:%d:%d:1\n", i, j);
            else
                file_putf(f, "window:%d:%d:0\n", i, j);

            /* Skip a line */
            file_put(f, "\n");
        }
    }
}


/*
 * Save a set of preferences to file, overwriting any old preferences with the
 * same title.
 *
 * - "path" is the filename to dump to
 * - "dump" is a pointer to the function that does the writing to file
 * - "title" is the name of this set of preferences
 *
 * Returns true on success, false otherwise
 */
bool prefs_save(const char *path, void (*dump)(ang_file *), const char *title)
{
    ang_file *fff;

    /* Remove old keymaps */
    remove_old_dump(path, title);

    fff = file_open(path, MODE_APPEND, FTYPE_TEXT);
    if (!fff) return false;

    /* Append the header */
    pref_header(fff, title);
    file_put(fff, "\n");

    dump(fff);

    /* Append the footer */
    pref_footer(fff, title);
    file_put(fff, "\n\n");

    file_close(fff);

    return true;
}


/*
 * Pref file parser
 */


/*
 * Load another file.
 */
static enum parser_error parse_prefs_load(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);
    const char *file;

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    file = parser_getstr(p, "file");
    process_pref_file(file, true, d->user);

    return PARSE_ERROR_NONE;
}


/*
 * Helper function for "process_pref_file()"
 *
 * Input:
 *   v: output buffer array
 *   f: final character
 *
 * Output:
 *   result
 */
static const char *process_pref_file_expr(char **sp, char *fp)
{
    const char *v;
    char *b;
    char *s;
    char f = ' ';

    /* Initial */
    s = (*sp);

    /* Skip spaces */
    while (isspace((unsigned char)*s)) s++;

    /* Save start */
    b = s;

    /* Default */
    v = "?o?o?";

    /* Either the string starts with a [ or it doesn't */
    if (*s == '[')
    {
        const char *p;
        const char *t;

        /* Skip [ */
        s++;

        /* First */
        t = process_pref_file_expr(&s, &f);

        /* Check all the different types of connective */
        if (!*t)
        {
            /* Nothing */
        }
        else if (streq(t, "IOR"))
        {
            v = "0";
            while (*s && (f != ']'))
            {
                t = process_pref_file_expr(&s, &f);
                if (*t && !streq(t, "0")) v = "1";
            }
        }
        else if (streq(t, "AND"))
        {
            v = "1";
            while (*s && (f != ']'))
            {
                t = process_pref_file_expr(&s, &f);
                if (*t && streq(t, "0")) v = "0";
            }
        }
        else if (streq(t, "NOT"))
        {
            v = "1";
            while (*s && (f != ']'))
            {
                t = process_pref_file_expr(&s, &f);
                if (*t && !streq(t, "0")) v = "0";
            }
        }
        else if (streq(t, "EQU"))
        {
            v = "1";
            if (*s && (f != ']'))
                t = process_pref_file_expr(&s, &f);
            while (*s && (f != ']'))
            {
                p = t;
                t = process_pref_file_expr(&s, &f);
                if (*t && !streq(p, t)) v = "0";
            }
        }
        else if (streq(t, "LEQ"))
        {
            v = "1";
            if (*s && (f != ']'))
                t = process_pref_file_expr(&s, &f);
            while (*s && (f != ']'))
            {
                p = t;
                t = process_pref_file_expr(&s, &f);
                if (*t && (strcmp(p, t) >= 0)) v = "0";
            }
        }
        else if (streq(t, "GEQ"))
        {
            v = "1";
            if (*s && (f != ']'))
                t = process_pref_file_expr(&s, &f);
            while (*s && (f != ']'))
            {
                p = t;
                t = process_pref_file_expr(&s, &f);
                if (*t && (strcmp(p, t) <= 0)) v = "0";
            }
        }
        else
        {
            while (*s && (f != ']'))
                t = process_pref_file_expr(&s, &f);
        }

        /* Verify ending */
        if (f != ']') v = "?x?x?";

        /* Extract final and Terminate */
        if ((f = *s) != '\0') *s++ = '\0';
    }
    else
    {
        /* Accept all printables except spaces and brackets */
        while (isprint((unsigned char)*s) && !strchr(" []", *s)) ++s;

        /* Extract final and Terminate */
        if ((f = *s) != '\0') *s++ = '\0';

        /* Variables start with $, otherwise it's a constant */
        if (*b == '$')
        {
            /* System */
            if (streq(b+1, "SYS")) v = ANGBAND_SYS;

            /* Race */
            else if (streq(b+1, "RACE")) v = player->race->name;

            /* Class */
            else if (streq(b+1, "CLASS")) v = player->clazz->name;

            /* Player */
            else if (streq(b+1, "PLAYER")) v = strip_suffix(nick);

            /* Gender */
            else if (streq(b+1, "GENDER")) v = player->sex->title;
        }
        else
            v = b;
    }

    /* Save */
    (*fp) = f;
    (*sp) = s;

    /* Result */
    return (v);
}


/*
 * Parse one of the prefix-based logical expressions used in pref files
 */
static enum parser_error parse_prefs_expr(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);
    const char *v;
    char *str;
    char *expr;
    char f;

    assert(d != NULL);

    /* XXX this can be avoided with a rewrite of process_pref_file_expr */
    str = expr = string_make(parser_getstr(p, "expr"));

    /* Parse the expr */
    v = process_pref_file_expr(&expr, &f);

    /* Set flag */
    d->bypass = streq(v, "0");

    string_free(str);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_object(struct parser *p)
{
    int tvi, svi;
    struct object_kind *kind;
    const char *tval, *sval;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    tval = parser_getsym(p, "tval");
    sval = parser_getsym(p, "sval");

    /* object:*:* means handle all objects and flavors */
    if (!strcmp(tval, "*"))
    {
        byte attr = (byte)parser_getint(p, "attr");
        char chr = (char)parser_getint(p, "char");
        int i;

        if (strcmp(sval, "*")) return PARSE_ERROR_UNRECOGNISED_SVAL;

        for (i = 0; i < z_info->k_max; i++)
        {
            struct object_kind *kind = &k_info[i];

            Client_setup.k_attr[kind->kidx] = attr;
            Client_setup.k_char[kind->kidx] = chr;

            if (!kind->ac) continue;

            Client_setup.flvr_x_attr[kind->ac] = attr;
            Client_setup.flvr_x_char[kind->ac] = chr;
        }
    }
    else
    {
        tvi = tval_find_idx(tval);
        if (tvi < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

        /* object:tval:* means handle all objects and flavors with this tval */
        if (!strcmp(sval, "*"))
        {
            byte attr = (byte)parser_getint(p, "attr");
            char chr = (char)parser_getint(p, "char");
            int i;

            for (i = 0; i < z_info->k_max; i++)
            {
                struct object_kind *kind = &k_info[i];

                if (kind->tval != tvi) continue;

                Client_setup.k_attr[kind->kidx] = attr;
                Client_setup.k_char[kind->kidx] = chr;

                if (!kind->ac) continue;

                Client_setup.flvr_x_attr[kind->ac] = attr;
                Client_setup.flvr_x_char[kind->ac] = chr;
            }
        }
        else
        {
            svi = lookup_sval(tvi, parser_getsym(p, "sval"));
            if (svi < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;

            kind = lookup_kind(tvi, svi);
            if (!kind) return PARSE_ERROR_UNRECOGNISED_SVAL;

            Client_setup.k_attr[kind->kidx] = (byte)parser_getint(p, "attr");
            Client_setup.k_char[kind->kidx] = (char)parser_getint(p, "char");
        }
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_monster(struct parser *p)
{
    const char *name;
    struct monster_race *monster;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    name = parser_getsym(p, "name");
    monster = lookup_monster(name);
    if (!monster) return PARSE_ERROR_NO_KIND_FOUND;

    Client_setup.r_attr[monster->ridx] = (byte)parser_getint(p, "attr");
    Client_setup.r_char[monster->ridx] = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_monster_base(struct parser *p)
{
    const char *name;
    struct monster_base *mb;
    int i;
    struct prefs_data *d = parser_priv(p);
    byte a;
    char c;

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    name = parser_getsym(p, "name");
    mb = lookup_monster_base(name);
    if (!mb) return PARSE_ERROR_NO_KIND_FOUND;

    a = (byte)parser_getint(p, "attr");
    c = (char)parser_getint(p, "char");

    for (i = 0; i < z_info->r_max; i++)
    {
        struct monster_race *race = &r_info[i];

        if (race->base != mb) continue;

        Client_setup.r_attr[race->ridx] = a;
        Client_setup.r_char[race->ridx] = c;
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_feat(struct parser *p)
{
    int idx;
    const char *lighting;
    int light_idx;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    if (idx >= z_info->f_max) return PARSE_ERROR_OUT_OF_BOUNDS;

    lighting = parser_getsym(p, "lighting");
    if (streq(lighting, "torch"))
        light_idx = LIGHTING_TORCH;
    else if (streq(lighting, "los"))
        light_idx = LIGHTING_LOS;
    else if (streq(lighting, "lit"))
        light_idx = LIGHTING_LIT;
    else if (streq(lighting, "dark"))
        light_idx = LIGHTING_DARK;
    else if (streq(lighting, "*"))
        light_idx = LIGHTING_MAX;
    else
        return PARSE_ERROR_INVALID_LIGHTING;

    if (light_idx < LIGHTING_MAX)
    {
        Client_setup.f_attr[idx][light_idx] = (byte)parser_getint(p, "attr");
        Client_setup.f_char[idx][light_idx] = (char)parser_getint(p, "char");
    }
    else
    {
        for (light_idx = 0; light_idx < LIGHTING_MAX; light_idx++)
        {
            Client_setup.f_attr[idx][light_idx] = (byte)parser_getint(p, "attr");
            Client_setup.f_char[idx][light_idx] = (char)parser_getint(p, "char");
        }
    }

    return PARSE_ERROR_NONE;
}


static void set_trap_graphic(int trap_idx, int light_idx, byte attr, char ch)
{
    if (light_idx < LIGHTING_MAX)
    {
        Client_setup.t_attr[trap_idx][light_idx] = attr;
        Client_setup.t_char[trap_idx][light_idx] = ch;
    }
    else
    {
        for (light_idx = 0; light_idx < LIGHTING_MAX; light_idx++)
        {
            Client_setup.t_attr[trap_idx][light_idx] = attr;
            Client_setup.t_char[trap_idx][light_idx] = ch;
        }
    }
}


static enum parser_error parse_prefs_trap(struct parser *p)
{
    const char *idx_sym;
    const char *lighting;
    int trap_idx;
    int light_idx;
    struct prefs_data *d = parser_priv(p);
    byte attr;
    char chr;

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    /* idx can be "*" or a number */
    idx_sym = parser_getsym(p, "idx");

    if (!strcmp(idx_sym, "*"))
        trap_idx = -1;
    else
    {
        char *z = NULL;

        trap_idx = strtoul(idx_sym, NULL, 0);
        if ((z == idx_sym) || (*idx_sym == '-')) return PARSE_ERROR_NOT_NUMBER;
        if (trap_idx >= z_info->trap_max) return PARSE_ERROR_OUT_OF_BOUNDS;
    }

    lighting = parser_getsym(p, "lighting");
    if (streq(lighting, "torch"))
        light_idx = LIGHTING_TORCH;
    else if (streq(lighting, "los"))
        light_idx = LIGHTING_LOS;
    else if (streq(lighting, "lit"))
        light_idx = LIGHTING_LIT;
    else if (streq(lighting, "dark"))
        light_idx = LIGHTING_DARK;
    else if (streq(lighting, "*"))
        light_idx = LIGHTING_MAX;
    else
        return PARSE_ERROR_INVALID_LIGHTING;

    attr = (byte)parser_getint(p, "attr");
    chr = (char)parser_getint(p, "char");

    if (trap_idx == -1)
    {
        int i;

        for (i = 0; i < z_info->trap_max; i++)
            set_trap_graphic(i, light_idx, attr, chr);
    }
    else
        set_trap_graphic(trap_idx, light_idx, attr, chr);

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

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    /* Parse the type, which is a | separated list of GF_ constants */
    s = string_make(parser_getsym(p, "type"));
    t = strtok(s, "| ");
    while (t)
    {
        if (streq(t, "*"))
            memset(types, true, sizeof(types));
        else
        {
            int idx = gf_name_to_idx(t);

            if (idx == -1) return PARSE_ERROR_INVALID_VALUE;

            types[idx] = true;
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

        Client_setup.gf_attr[i][motion] = (byte)parser_getuint(p, "attr");
        Client_setup.gf_char[i][motion] = (char)parser_getuint(p, "char");

        /* Default values */
        if (motion2)
        {
            Client_setup.gf_attr[i][motion2] = (byte)parser_getuint(p, "attr");
            Client_setup.gf_char[i][motion2] = (char)parser_getuint(p, "char");
        }
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_flavor(struct parser *p)
{
    unsigned int idx;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    if (idx >= (unsigned int)flavor_max) return PARSE_ERROR_OUT_OF_BOUNDS;

    Client_setup.flvr_x_attr[idx] = (byte)parser_getint(p, "attr");
    Client_setup.flvr_x_char[idx] = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_keymap_action(struct parser *p)
{
    const char *act = "";
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    if (parser_hasval(p, "act")) act = parser_getstr(p, "act");
    keypress_from_text(d->keymap_buffer, N_ELEMENTS(d->keymap_buffer), act);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_keymap_input(struct parser *p)
{
    int mode;
    struct keypress tmp[2];
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    mode = parser_getint(p, "mode");
    if ((mode < 0) || (mode >= KEYMAP_MODE_MAX)) return PARSE_ERROR_OUT_OF_BOUNDS;

    keypress_from_text(tmp, N_ELEMENTS(tmp), parser_getstr(p, "key"));
    if ((tmp[0].type != EVT_KBRD) || (tmp[1].type != EVT_NONE))
        return PARSE_ERROR_FIELD_TOO_LONG;

    keymap_add(mode, tmp[0], d->keymap_buffer, d->user);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_message(struct parser *p)
{
    int a, msg_index;
    const char *attr;
    const char *type;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    type = parser_getsym(p, "type");
    attr = parser_getsym(p, "attr");

    msg_index = message_lookup_by_name(type);

    if (msg_index < 0)
        return PARSE_ERROR_INVALID_MESSAGE;

    if (strlen(attr) > 1)
        a = color_text_to_attr(attr);
    else
        a = color_char_to_attr(attr[0]);

    if ((a < COLOUR_DARK) || (a > COLOUR_DEEP_L_BLUE))
        return PARSE_ERROR_INVALID_COLOR;

    message_color_define(msg_index, (byte)a);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_color(struct parser *p)
{
    int idx;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    if (idx >= MAX_COLORS) return PARSE_ERROR_OUT_OF_BOUNDS;

    angband_color_table[idx][0] = parser_getint(p, "k");
    angband_color_table[idx][1] = parser_getint(p, "r");
    angband_color_table[idx][2] = parser_getint(p, "g");
    angband_color_table[idx][3] = parser_getint(p, "b");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_window(struct parser *p)
{
    int window;
    size_t flag, value;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    window = parser_getint(p, "window");
    if ((window < 0) || (window >= ANGBAND_TERM_MAX)) return PARSE_ERROR_OUT_OF_BOUNDS;

    flag = parser_getuint(p, "flag");
    if (flag >= PW_MAX_FLAGS) return PARSE_ERROR_OUT_OF_BOUNDS;

    value = parser_getuint(p, "value");

    /* Hack -- the main window is fixed: PW_PLAYER_2 + PW_STATUS */
    if (window == 0)
    {
        bool good_flag = (((1L << flag) == PW_PLAYER_2) || ((1L << flag) == PW_STATUS));

        if ((good_flag && !value) || (!good_flag && value))
            return PARSE_ERROR_OUT_OF_BOUNDS;
    }

    /* Hack -- the chat window is fixed: PW_MESSAGE_CHAT */
    if (window == PMSG_TERM)
    {
        bool good_flag = ((1L << flag) == PW_MESSAGE_CHAT);

        if ((good_flag && !value) || (!good_flag && value))
            return PARSE_ERROR_OUT_OF_BOUNDS;
    }

    if (window_flag_desc[flag])
    {
        if (value)
            d->window_flags[window] |= (1L << flag);
        else
            d->window_flags[window] &= ~(1L << flag);
    }

    d->loaded_window_flag[window] = true;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_x(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    option_set(Client_setup.options, parser_getstr(p, "option"), false);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_y(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    option_set(Client_setup.options, parser_getstr(p, "option"), true);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_o(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    option_set(NULL, parser_getsym(p, "name"), parser_getuint(p, "value"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_rf(struct parser *p)
{
    /* Hack -- parser hook for female player presets */
    return parse_prefs_monster(p);
}


static struct parser *init_parse_prefs(bool user)
{
    struct parser *p = parser_new();
    struct prefs_data *pd = mem_zalloc(sizeof(*pd));
    int i;

    parser_setpriv(p, pd);
    pd->user = user;
    for (i = 0; i < ANGBAND_TERM_MAX; i++) pd->loaded_window_flag[i] = false;

    parser_reg(p, "% str file", parse_prefs_load);
    parser_reg(p, "? str expr", parse_prefs_expr);
    parser_reg(p, "object sym tval sym sval int attr int char", parse_prefs_object);
    parser_reg(p, "monster sym name int attr int char", parse_prefs_monster);
    parser_reg(p, "monster-base sym name int attr int char", parse_prefs_monster_base);
    parser_reg(p, "feat uint idx sym lighting int attr int char", parse_prefs_feat);
    parser_reg(p, "trap sym idx sym lighting int attr int char", parse_prefs_trap);
    parser_reg(p, "GF sym type sym direction uint attr uint char", parse_prefs_gf);
    parser_reg(p, "flavor uint idx int attr int char", parse_prefs_flavor);
    parser_reg(p, "keymap-act ?str act", parse_prefs_keymap_action);
    parser_reg(p, "keymap-input int mode str key", parse_prefs_keymap_input);
    parser_reg(p, "message sym type sym attr", parse_prefs_message);
    parser_reg(p, "color uint idx int k int r int g int b", parse_prefs_color);
    parser_reg(p, "window int window uint flag uint value", parse_prefs_window);
    register_sound_pref_parser(p);
    parser_reg(p, "X str option", parse_prefs_x);
    parser_reg(p, "Y str option", parse_prefs_y);
    parser_reg(p, "O sym name uint value", parse_prefs_o);

    /* Hack -- parser hook for female player presets */
    parser_reg(p, "RF sym name int attr int char", parse_prefs_rf);

    return p;
}


static errr finish_parse_prefs(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);
    int i;

    /*
     * Update sub-windows based on the newly read-in prefs.
     *
     * The window_flag[] array cannot be updated directly during
     * parsing since the changes between the existing flags and the new
     * are used to set/unset the event handlers that update the windows.
     *
     * Build a complete set to pass to subwindows_set_flags() by loading
     * any that weren't read in by the parser from the existing set.
     */
     for (i = 0; i < ANGBAND_TERM_MAX; i++)
     {
        if (!d->loaded_window_flag[i])
            d->window_flags[i] = window_flag[i];
     }
     subwindows_set_flags(d->window_flags, ANGBAND_TERM_MAX);

     return PARSE_ERROR_NONE;
}


errr process_pref_file_command(const char *s)
{
    struct parser *p = init_parse_prefs(true);
    errr e = parser_parse(p, s);

    mem_free(parser_priv(p));
    parser_destroy(p);
    return e;
}


static void print_error(const char *name, struct parser *p)
{
    struct parser_state s;

    parser_getstate(p, &s);
    plog_fmt("Parse error in %s line %d column %d: %s: %s", name,
        s.line, s.col, s.msg, parser_error_str[s.error]);
}


/*
 * Process the user pref file with a given path.
 *
 * name is the name of the pref file.
 * quiet means "don't complain about not finding the file".
 * user should be true if the pref file is user-specific and not a game default.
 */
static bool process_pref_file_named(const char *path, bool quiet, bool user)
{
    ang_file *f = file_open(path, MODE_READ, FTYPE_TEXT);
    errr e = 0;

    if (!f)
    {
        if (!quiet) plog_fmt("Cannot open '%s'.", path);

        /* Signal failure to callers */
        e = PARSE_ERROR_INTERNAL;
    }
    else
    {
        char line[MSG_LEN];
        int line_no = 0;
        struct parser *p = init_parse_prefs(user);

        while (file_getl(f, line, sizeof(line)))
        {
            line_no++;

            e = parser_parse(p, line);
            if (e != PARSE_ERROR_NONE)
            {
                print_error(path, p);
                break;
            }
        }
        finish_parse_prefs(p);

        file_close(f);
        mem_free(parser_priv(p));
        parser_destroy(p);
    }

    /* Result */
    return (e == PARSE_ERROR_NONE);
}


/*
 * Process the user pref file with a given name and search paths.
 *
 * name is the name of the pref file.
 * quiet means "don't complain about not finding the file".
 * user should be true if the pref file is user-specific and not a game default.
 * base_search_path is the first path that should be checked for the file.
 * fallback_search_path is the path that should be checked if the file
 *   couldn't be found at the base path.
 * used_fallback will be set on return to true if the fallback path was used, false otherwise.
 * returns true if everything worked OK, false otherwise.
 */
static bool process_pref_file_layered(const char *name, bool quiet, bool user,
    const char *base_search_path, const char *fallback_search_path, bool *used_fallback)
{
    char buf[MSG_LEN];

    assert(base_search_path != NULL);

    /* Build the filename */
    path_build(buf, sizeof(buf), base_search_path, name);

    if (used_fallback != NULL) *used_fallback = false;

    if (!file_exists(buf) && (fallback_search_path != NULL))
    {
        path_build(buf, sizeof(buf), fallback_search_path, name);

        if (used_fallback != NULL) *used_fallback = true;
    }

    return process_pref_file_named(buf, quiet, user);
}


/*
 * Look for a pref file at its base location (falling back to another path if
 * needed) and then in the user location. This effectively will layer a user
 * pref file on top of a default pref file.
 *
 * Because of the way this function works, there might be some unexpected
 * effects when a pref file triggers another pref file to be loaded.
 * For example, customize/pref.prf causes message.prf to load. This means that the
 * game will load customize/pref.prf, then customize/message.prf, then user/message.prf,
 * and finally user/pref.prf.
 *
 * name is the name of the pref file.
 * quiet means "don't complain about not finding the file".
 * user should be true if the pref file is user-specific and not a game default.
 * returns true if everything worked OK, false otherwise.
 */
bool process_pref_file(const char *name, bool quiet, bool user)
{
    bool root_success = false;
    bool user_success = false;
    bool used_fallback = false;

    /*
     * This supports the old behavior: look for a file first in 'customize/', and
     * if not found there, then 'user/'.
     */
    root_success = process_pref_file_layered(name, quiet, user, ANGBAND_DIR_CUSTOMIZE,
        ANGBAND_DIR_USER, &used_fallback);

    /* If not found, do a check of the current graphics directory */
    if (!root_success && use_graphics)
    {
        graphics_mode *mode = get_graphics_mode(use_graphics, true);

        if (mode)
            root_success = process_pref_file_layered(name, quiet, user, mode->path, NULL, NULL);
    }

    /*
     * Next, we want to force a check for the file in the user/ directory.
     * However, since we used the user directory as a fallback in the previous
     * check, we only want to do this if the fallback wasn't used. This cuts
     * down on unnecessary parsing.
     */
    if (!used_fallback)
    {
        /*
         * Force quiet (since this is an optional file) and force user
         * (since this should always be considered user-specific).
         */
        user_success = process_pref_file_layered(name, true, true, ANGBAND_DIR_USER, NULL,
            &used_fallback);
    }

    /* If only one load was successful, that's okay; we loaded something. */
    return root_success || user_success;
}


void process_pref_options(void)
{
    char name[MSG_LEN];
    char buf[MSG_LEN];
    ang_file *f;
    struct parser *p = parser_new();
    struct prefs_data *pd = mem_zalloc(sizeof(*pd));
    char line[MSG_LEN];

    /* Initialize default option values */
    init_options(Client_setup.options);

    /* Build the filename */
    strnfmt(name, sizeof(name), "%s.prf", strip_suffix(nick));
    path_build(buf, sizeof(buf), ANGBAND_DIR_CUSTOMIZE, name);
    if (!file_exists(buf))
        path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

    f = file_open(buf, MODE_READ, FTYPE_TEXT);
    if (!f) return;

    parser_setpriv(p, pd);
    parser_reg(p, "X str option", parse_prefs_x);
    parser_reg(p, "Y str option", parse_prefs_y);

    /* Parse the file (skip errors) */
    while (file_getl(f, line, sizeof(line)))
        parser_parse(p, line);

    file_close(f);
    mem_free(parser_priv(p));
    parser_destroy(p);
}


/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is true, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 */
void reset_visuals(bool load_prefs)
{
    /* Nothing to do */
    if (!z_info) return;

    /* Reset the Client_setup info */
    memset(Client_setup.flvr_x_attr, 0, flavor_max * sizeof(byte));
    memset(Client_setup.flvr_x_char, 0, flavor_max * sizeof(char));
    memset(Client_setup.f_attr, 0, z_info->f_max * sizeof(byte_lit));
    memset(Client_setup.f_char, 0, z_info->f_max * sizeof(char_lit));
    memset(Client_setup.t_attr, 0, z_info->trap_max * sizeof(byte_lit));
    memset(Client_setup.t_char, 0, z_info->trap_max * sizeof(char_lit));
    memset(Client_setup.k_attr, 0, z_info->k_max * sizeof(byte));
    memset(Client_setup.k_char, 0, z_info->k_max * sizeof(char));
    memset(Client_setup.r_attr, 0, z_info->r_max * sizeof(byte));
    memset(Client_setup.r_char, 0, z_info->r_max * sizeof(char));
    memset(Client_setup.gf_attr, 0, sizeof(Client_setup.gf_attr));
    memset(Client_setup.gf_char, 0, sizeof(Client_setup.gf_char));

    /* Don't load the user pref file */
    if (!load_prefs) return;

    /* Graphic symbols */
    if (use_graphics)
    {
        /* If we have a graphics mode, see if the mode has a pref file name */
        graphics_mode *mode = get_graphics_mode(use_graphics, true);
        char buf[MSG_LEN];

        /* Build path to the pref file */
        path_build(buf, sizeof(buf), mode->path, mode->pref);

        process_pref_file_named(buf, false, false);
    }

    /* Normal symbols */
    else
    {
        /* Process "font.prf" */
        process_pref_file("font.prf", false, false);
    }
}


/*
 * Ask for a "user pref line" and process it
 */
void do_cmd_pref(void)
{
    char buf[NORMAL_WID];

    buf[0] = '\0';
    if (get_string("Action: ", buf, sizeof(buf)))
        process_pref_file_command(buf);
}
