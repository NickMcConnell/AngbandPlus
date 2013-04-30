/*
 * File: prefs.c
 * Purpose: Pref file handling code
 *
 * Copyright (c) 2003 Takeshi Mogami, Robert Ruehlmann
 * Copyright (c) 2007 Pete Mack
 * Copyright (c) 2010 Andi Sidwell
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


#include "c-angband.h"
#include "../common/parser.h"
#include "../common/spells.h"
#include "keymap.h"
#include "prefs.h"


/*** Pref file saving code ***/


/*
 *  Header and footer marker string for pref file dumps
 */
static const char *dump_separator = "#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#";


/*
 * Remove old lines from pref files
 */
static void remove_old_dump(const char *cur_fname, const char *mark)
{
    int between_marks = 0;
    bool changed = FALSE;
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
        /* If we find the start line, turn on */
        if (!strcmp(buf, start_line)) between_marks = -1;

        /* If we find the finish line, turn off */
        else if (!strcmp(buf, end_line))
        {
            between_marks = 3;
            changed = TRUE;
        }

        /* Take into account two empty lines after the finish line */
        else if (between_marks > 0) between_marks--;

        if (!between_marks)
        {
            /* Copy orginal line */
            file_putf(new_file, "%s\n", buf);
        }
    }

    /* Close files */
    file_close(cur_file);
    file_close(new_file);

    /* If there are changes, move things around */
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

    /* Otherwise just destroy the new file */
    else file_delete(new_fname);
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
 * Write all current options to a user preference file.
 */
void option_dump(ang_file *fff)
{
    int i, j;

    /* Dump options */
    for (i = 0; i < OPT_MAX; i++)
    {
        const char *name = option_name(i);
        if (!name) continue;

        /* Comment */
        file_putf(fff, "# Option '%s'\n", option_desc(i));

        /* Dump the option */
        if (Client_setup.options[i])
            file_putf(fff, "Y:%s\n", name);
        else
            file_putf(fff, "X:%s\n", name);

        /* Skip a line */
        file_putf(fff, "\n");
    }

    /* Dump other options */
    for (i = 0; i < TYPE_MAX; i++)
    {
        file_putf(fff, "# Auto-squelch level (0 to 6)\nH:%d:squelch_lvl_%d\n\n",
            p_ptr->other.squelch_lvl[i], i);
    }
    file_putf(fff, "# Base delay factor (0 to 255)\nH:%d:delay_factor\n\n", p_ptr->other.delay_factor);
    file_putf(fff, "# Hitpoint warning (0 to 9)\nH:%d:hitpoint_warn\n\n", p_ptr->other.hitpoint_warn);
    file_putf(fff, "# Movement delay factor (0 to 9)\nH:%d:lazymove_delay\n\n", lazymove_delay);

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
            file_putf(fff, "# Window '%s', Flag '%s'\n",
                angband_term_name[i], window_flag_desc[j]);

            /* Dump the flag */
            if (window_flag[i] & (1L << j))
                file_putf(fff, "W:%d:%d:1\n", i, j);
            else
                file_putf(fff, "W:%d:%d:0\n", i, j);

            /* Skip a line */
            file_putf(fff, "\n");
        }
    }
}


/* Dump colors */
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
        file_putf(fff, "V:%d:%d:%d:%d:%d\n\n", i, kv, rv, gv, bv);
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
 * Returns TRUE on success, FALSE otherwise
 */
bool prefs_save(const char *path, void (*dump)(ang_file *), const char *title)
{
    ang_file *fff;

    /* Remove old keymaps */
    remove_old_dump(path, title);

    fff = file_open(path, MODE_APPEND, FTYPE_TEXT);
    if (!fff) return FALSE;

    /* Append the header */
    pref_header(fff, title);
    file_put(fff, "\n\n");
    file_putf(fff, "# %s definitions\n\n", strstr(title, " "));
    
    dump(fff);

    /* Append the footer */
    file_put(fff, "\n");
    pref_footer(fff, title);
    file_put(fff, "\n\n");

    file_close(fff);

    return TRUE;
}


/*** Pref file parser ***/


/*
 * Private data for pref file parsing.
 */
typedef struct prefs_data
{
    bool bypass;
    struct keypress keymap_buffer[KEYMAP_ACTION_MAX];
    bool user;
    bool loaded_window_flag[ANGBAND_TERM_MAX];
    u32b window_flags[ANGBAND_TERM_MAX];
} prefs_data;


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
    process_pref_file(file, TRUE, d->user);

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

    /* Analyze */
    if (*s == '[')
    {
        const char *p;
        const char *t;

        /* Skip [ */
        s++;

        /* First */
        t = process_pref_file_expr(&s, &f);

        /* Oops */
        if (!*t)
        {
            /* Nothing */
        }

        /* Function: IOR */
        else if (streq(t, "IOR"))
        {
            v = "0";
            while (*s && (f != ']'))
            {
                t = process_pref_file_expr(&s, &f);
                if (*t && !streq(t, "0")) v = "1";
            }
        }

        /* Function: AND */
        else if (streq(t, "AND"))
        {
            v = "1";
            while (*s && (f != ']'))
            {
                t = process_pref_file_expr(&s, &f);
                if (*t && streq(t, "0")) v = "0";
            }
        }

        /* Function: NOT */
        else if (streq(t, "NOT"))
        {
            v = "1";
            while (*s && (f != ']'))
            {
                t = process_pref_file_expr(&s, &f);
                if (*t && !streq(t, "0")) v = "0";
            }
        }

        /* Function: EQU */
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

        /* Function: LEQ */
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

        /* Function: GEQ */
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

        /* Oops */
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

    /* Other */
    else
    {
        /* Accept all printables except spaces and brackets */
        while (isprint((unsigned char)*s) && !strchr(" []", *s)) ++s;

        /* Extract final and Terminate */
        if ((f = *s) != '\0') *s++ = '\0';

        /* Variable */
        if (*b == '$')
        {
            /* System */
            if (streq(b+1, "SYS")) v = ANGBAND_SYS;

            /* Graphics */
            else if (streq(b+1, "GRAF")) v = ANGBAND_GRAF;

            /* Race */
            else if (streq(b+1, "RACE")) v = p_ptr->race->name;

            /* Class */
            else if (streq(b+1, "CLASS")) v = p_ptr->clazz->name;

            /* Player */
            else if (streq(b+1, "PLAYER")) v = nick;

            /* Gender */
            else if (streq(b+1, "GENDER")) v = p_ptr->sex->title;
        }

        /* Constant */
        else
            v = b;
    }

    /* Save */
    (*fp) = f;
    (*sp) = s;

    /* Result */
    return (v);
}


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


static enum parser_error parse_prefs_k(struct parser *p)
{
    int tvi, svi;
    object_kind *kind;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    tvi = tval_find_idx(parser_getsym(p, "tval"));
    if (tvi < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    svi = lookup_sval(tvi, parser_getsym(p, "sval"));
    if (svi < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;

    kind = lookup_kind(tvi, svi);
    if (!kind) return PARSE_ERROR_UNRECOGNISED_SVAL;

    Client_setup.k_attr[kind->kidx] = (byte)parser_getint(p, "attr");
    Client_setup.k_char[kind->kidx] = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_r(struct parser *p)
{
    int idx;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    if (idx >= z_info->r_max) return PARSE_ERROR_OUT_OF_BOUNDS;

    Client_setup.r_attr[idx] = (byte)parser_getint(p, "attr");
    Client_setup.r_char[idx] = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_f(struct parser *p)
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
        Client_setup.f_attr[idx][light_idx] = (byte)parser_getint(p, "attr");
        Client_setup.f_char[idx][light_idx] = (char)parser_getint(p, "char");
    }
    else
    {
        for (light_idx = 0; light_idx < FEAT_LIGHTING_MAX; light_idx++)
        {
            Client_setup.f_attr[idx][light_idx] = (byte)parser_getint(p, "attr");
            Client_setup.f_char[idx][light_idx] = (char)parser_getint(p, "char");
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

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    /* Parse the type, which is a | separated list of GF_ constants */
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


static enum parser_error parse_prefs_l(struct parser *p)
{
    unsigned int idx;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    idx = parser_getuint(p, "idx");
    if (idx >= flavor_max) return PARSE_ERROR_OUT_OF_BOUNDS;

    Client_setup.flvr_x_attr[idx] = (byte)parser_getint(p, "attr");
    Client_setup.flvr_x_char[idx] = (char)parser_getint(p, "char");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_e(struct parser *p)
{
    int tvi, a;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    tvi = tval_find_idx(parser_getsym(p, "tval"));
    if ((tvi < 0) || (tvi >= (long)N_ELEMENTS(Client_setup.tval_attr)))
        return PARSE_ERROR_UNRECOGNISED_TVAL;

    a = parser_getint(p, "attr");
    if (a) Client_setup.tval_attr[tvi] = (byte) a;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_a(struct parser *p)
{
    const char *act;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    act = parser_getstr(p, "act");
    keypress_from_text(d->keymap_buffer, N_ELEMENTS(d->keymap_buffer), act);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_c(struct parser *p)
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


static enum parser_error parse_prefs_m(struct parser *p)
{
    int a, type;
    const char *attr;
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    type = parser_getint(p, "type");
    attr = parser_getsym(p, "attr");

    if (strlen(attr) > 1)
        a = color_text_to_attr(attr);
    else
        a = color_char_to_attr(attr[0]);

    if ((a < TERM_DARK) || (a > TERM_DEEP_L_BLUE))
        return PARSE_ERROR_INVALID_COLOR;

    message_color_define((u16b)type, (byte)a);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_v(struct parser *p)
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


static enum parser_error parse_prefs_w(struct parser *p)
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

    /* Hack -- The main window is fixed: PW_PLAYER_2 + PW_STATUS */
    if (window == 0)
    {
        bool good_flag = (((1L << flag) == PW_PLAYER_2) || ((1L << flag) == PW_STATUS));

        if ((good_flag && !value) || (!good_flag && value))
            return PARSE_ERROR_OUT_OF_BOUNDS;
    }

    /* Hack -- The chat window is fixed: PW_MESSAGE_CHAT */
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

    d->loaded_window_flag[window] = TRUE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_x(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    /* XXX check for valid option */
    option_set(Client_setup.options, parser_getstr(p, "option"), FALSE);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_y(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    option_set(Client_setup.options, parser_getstr(p, "option"), TRUE);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_h(struct parser *p)
{
    const char *option;
    size_t value;
    struct prefs_data *d = parser_priv(p);
    int i;
    char tmp[20];

    assert(d != NULL);
    if (d->bypass) return PARSE_ERROR_NONE;

    option = parser_getstr(p, "option");
    value = parser_getuint(p, "value");

    for (i = 0; i < TYPE_MAX; i++)
    {
        strnfmt(tmp, sizeof(tmp), "squelch_lvl_%d", i);
        if (streq(option, tmp))
        {
            /* Bounds */
            if (value > SQUELCH_ALL) value = SQUELCH_ALL;
            p_ptr->other.squelch_lvl[i] = value;
        }
    }
    if (streq(option, "delay_factor"))
    {
        /* Bounds */
        if (value > 255) value = 255;
        p_ptr->other.delay_factor = value;
    }
    if (streq(option, "hitpoint_warn"))
    {
        /* Bounds */
        if (value > 9) value = 9;
        p_ptr->other.hitpoint_warn = value;
    }
    if (streq(option, "lazymove_delay"))
    {
        /* Bounds */
        if (value > 9) value = 9;
        lazymove_delay = value;
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_prefs_rf(struct parser *p)
{
    /* Hack -- Parser hook for female player presets */
    return parse_prefs_r(p);
}


static struct parser *init_parse_prefs(bool user)
{
    struct parser *p = parser_new();
    struct prefs_data *pd = mem_zalloc(sizeof(*pd));
    int i;

    parser_setpriv(p, pd);
    pd->user = user;
    for (i = 0; i < ANGBAND_TERM_MAX; i++) pd->loaded_window_flag[i] = FALSE;

    parser_reg(p, "% str file", parse_prefs_load);
    parser_reg(p, "? str expr", parse_prefs_expr);
    parser_reg(p, "K sym tval sym sval int attr int char", parse_prefs_k);
    parser_reg(p, "R uint idx int attr int char", parse_prefs_r);
    parser_reg(p, "F uint idx sym lighting int attr int char", parse_prefs_f);
    parser_reg(p, "GF sym type sym direction uint attr uint char", parse_prefs_gf);
    parser_reg(p, "L uint idx int attr int char", parse_prefs_l);
    parser_reg(p, "E sym tval int attr", parse_prefs_e);
    parser_reg(p, "A str act", parse_prefs_a);
    parser_reg(p, "C int mode str key", parse_prefs_c);
    parser_reg(p, "M int type sym attr", parse_prefs_m);
    parser_reg(p, "V uint idx int k int r int g int b", parse_prefs_v);
    parser_reg(p, "W int window uint flag uint value", parse_prefs_w);
    parser_reg(p, "X str option", parse_prefs_x);
    parser_reg(p, "Y str option", parse_prefs_y);
    parser_reg(p, "H uint value str option", parse_prefs_h);

    /* Hack -- Parser hook for female player presets */
    parser_reg(p, "RF uint idx int attr int char", parse_prefs_rf);

    return p;
}


static errr finish_parse_prefs(struct parser *p)
{
    struct prefs_data *d = parser_priv(p);
    int i;

    /*
     * Update sub-windows based on the newly read-in prefs.
     *
     * The op_ptr->window_flag[] array cannot be updated directly during
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
    struct parser *p = init_parse_prefs(TRUE);
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
 * Process the "user pref file" with the given name
 * "quiet" means "don't complain about not finding the file.
 *
 * 'user' should be TRUE if the pref file loaded is user-specific and not
 * a game default.
 *
 * Returns TRUE if everything worked OK, false otherwise
 */
bool process_pref_file(const char *name, bool quiet, bool user)
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

        p = init_parse_prefs(user);

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
        finish_parse_prefs(p);

        file_close(f);
        mem_free(parser_priv(p));
        parser_destroy(p);
    }

    /* Result */
    return (e == PARSE_ERROR_NONE);
}