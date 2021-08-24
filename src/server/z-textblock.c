/*
 * File: z-textblock.c
 * Purpose: Text output bugger code
 *
 * Copyright (c) 2010 Andi Sidwell
 * Copyright (c) 2011 Peter Denison
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


void text_out_init(struct player *p)
{
    loc_init(&p->info_grid, 0, 0);
}


static void text_out_line_end(struct player *p)
{
    /* Fill the rest of the line with spaces */
    while (p->info_grid.x < NORMAL_WID)
    {
        p->info[p->info_grid.y][p->info_grid.x].a = COLOUR_WHITE;
        p->info[p->info_grid.y][p->info_grid.x].c = ' ';
        p->info_grid.x++;
    }
}


void text_out_done_no_newline(struct player *p)
{
    if (p->info_grid.y == MAX_TXT_INFO)
        p->last_info_line = MAX_TXT_INFO - 1;
    else
    {
        text_out_line_end(p);
        p->last_info_line = p->info_grid.y;
    }
}


void text_out_done(struct player *p)
{
    /* Flush */
    text_out(p, "\n");
    text_out_done_no_newline(p);
}


/*
 * Write text and apply line-wrapping.
 *
 * Long lines will be wrapped at column 74 or at a newline character.
 * Note that punctuation can sometimes be placed one column beyond the wrap limit.
 *
 * You must be careful to end all file output with a newline character
 * to "flush" the stored line position.
 */
static void text_out_aux(struct player *p, byte a, const char *str)
{
    const char *s;
    char buf[MSG_LEN];

    /* Check limit */
    if (p->info_grid.y == MAX_TXT_INFO) return;

    /* Copy to a rewriteable string */
    my_strcpy(buf, str, MSG_LEN);

    /* Current location within "buf" */
    s = buf;

    /* Process the string */
    while (*s)
    {
        int n = 0;
        int len = NORMAL_WID - 6 - p->info_grid.x;
        int l_space = -1;

        /* Paranoia */
        if (len < 0) len = 0;

        /* Find length of line up to next newline or end-of-string */
        while ((n < len) && !((s[n] == '\n') || (s[n] == '\0')))
        {
            /* Mark the most recent space in the string */
            if (s[n] == ' ') l_space = n;

            /* Increment */
            n++;
        }

        /* If we have encountered no spaces */
        if ((l_space == -1) && (n == len))
        {
            /* If we are at the start of a new line */
            if (p->info_grid.x == 0) len = n;

            /* Hack -- output punctuation at the end of the line */
            else if ((s[0] == ' ') || (s[0] == ',') || (s[0] == '.')) len = 1;

            else
            {
                /* Begin a new line */
                text_out_line_end(p);
                p->info_grid.y++;

                /* Reset */
                p->info_grid.x = 0;

                /* Check limit */
                if (p->info_grid.y == MAX_TXT_INFO) return;

                continue;
            }
        }
        else
        {
            /* Wrap at the newline */
            if ((s[n] == '\n') || (s[n] == '\0')) len = n;

            /* Wrap at the last space */
            else len = l_space;
        }

        /* Write that line to file */
        for (n = 0; n < len; n++)
        {
            /* Write out the character */
            p->info[p->info_grid.y][p->info_grid.x].a = a;
            p->info[p->info_grid.y][p->info_grid.x].c = s[n];

            /* Increment */
            p->info_grid.x++;
        }

        /* Move 's' past the stuff we've written */
        s += len;

        /* If we are at the end of the string, end */
        if (*s == '\0') return;

        /* Skip newlines */
        if (*s == '\n') s++;

        /* Begin a new line */
        text_out_line_end(p);
        p->info_grid.y++;

        /* Reset */
        p->info_grid.x = 0;

        /* Check limit */
        if (p->info_grid.y == MAX_TXT_INFO) return;

        /* Skip whitespace */
        while (*s == ' ') s++;
    }
}


/*
 * Output text to the screen or to a file depending on the selected
 * text_out hook.
 */
void text_out(struct player *p, const char *fmt, ...)
{
    char buf[MSG_LEN];
    va_list vp;

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Do the va_arg fmt to the buffer */
    vstrnfmt(buf, sizeof(buf), fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Output now */
    text_out_aux(p, COLOUR_WHITE, buf);
}


/*
 * Output text to the screen (in color) or to a file depending on the
 * selected hook.
 */
void text_out_c(struct player *p, byte a, const char *fmt, ...)
{
    char buf[MSG_LEN];
    va_list vp;

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Do the va_arg fmt to the buffer */
    vstrnfmt(buf, sizeof(buf), fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Output now */
    text_out_aux(p, a, buf);
}


/*
 * Write a text file from given input.
 *
 * path the path to write to
 * writer the text-writing function
 */
errr text_lines_to_file(const char *path, text_writer writer, void *data)
{
    char new_fname[MSG_LEN];
    char old_fname[MSG_LEN];
    ang_file *new_file;

    /* Format filenames */
    strnfmt(new_fname, sizeof(new_fname), "%s.new", path);
    strnfmt(old_fname, sizeof(old_fname), "%s.old", path);

    /* Write new file */
    new_file = file_open(new_fname, MODE_WRITE, FTYPE_TEXT);
    if (!new_file) return -1;

    writer(new_file, data);
    file_close(new_file);

    /* Move files around */
    if (!file_exists(path))
        file_move(new_fname, path);
    else if (file_move(path, old_fname))
    {
        file_move(new_fname, path);
        file_delete(old_fname);
    }
    else
        file_delete(new_fname);

    return 0;
}
