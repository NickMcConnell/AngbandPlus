/*
 * File: help-ui.c
 * Purpose: In-game help
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
 * Recursive file perusal.
 */
bool show_file(struct player *p, const char *name, const char *what, int line, int color)
{
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

    /* Protect our header */
    alloc_header_icky(p, what);

    /* Currently unused facility to show and describe arbitrary files */
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

        path_build(path, sizeof(path), ANGBAND_DIR_HELP, name);
        fff = file_open(path, MODE_READ, FTYPE_TEXT);
    }

    /* Oops */
    if (!fff)
    {
        /* Message */
        msg(p, "Cannot open '%s'.", name);
        message_flush(p);

        /* Oops */
        return true;
    }

    /* Pre-Parse the file */
    while (true)
    {
        /* Read a line or stop */
        if (!file_getl(fff, buf, sizeof(buf))) break;

        /* Count the "real" lines */
        next++;
    }

    /* Save the number of "real" lines */
    size = next;

    /* Restart when necessary */
    if (line >= size) p->interactive_line = line = 0;

    /* Re-open the file if needed */
    if (next > line)
    {
        /* Close it */
        file_close(fff);

        /* Hack -- re-open the file */
        fff = file_open(path, MODE_READ, FTYPE_TEXT);
        if (!fff) return false;

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
        byte attr = COLOUR_WHITE;

        /* Hack -- track the "first" line */
        if (!i) line = next;

        /* Get a line of the file or stop */
        if (!file_getl(fff, buf, sizeof(buf))) break;

        /* Count the "real" lines */
        next++;

        /* Extract color */
        if (color) attr = color_char_to_attr(buf[0]);

        /* Highlight "shower" */
        if (shower[0] && strstr(buf, shower)) attr = COLOUR_YELLOW;

        /* Dump the line */
        Send_special_line(p, size - 1, size - 1 - line, i, attr, &buf[color]);

        /* Count the printed lines */
        i++;
    }

    /* Inform about empty file/list */
    if (!i) Send_special_line(p, 0, 0, 0, COLOUR_WHITE, "  (nothing)");

    /* Close the file */
    file_close(fff);

    /* Normal return */
    return true;
}


/*
 * Read a file and copy a portion of it into player's "info[]" array.
 */
static void copy_file_info(struct player *p, const char *name, int line, int color)
{
    int i = 0, k;

    /* Current help file */
    ang_file *fff = NULL;

    /* Number of "real" lines passed by */
    int next = 0;

    /* Path buffer */
    char path[MSG_LEN];

    /* General buffer */
    char buf[MSG_LEN];

    /* true if we are inside a RST block that should be skipped */
    bool skip_lines = false;

    /* Build the filename */
    path_build(path, sizeof(path), ANGBAND_DIR_HELP, name);

    /* Open the file */
    fff = file_open(path, MODE_READ, FTYPE_TEXT);

    /* Oops */
    if (!fff)
    {
        /* Message */
        msg(p, "Cannot open '%s'.", name);
        message_flush(p);

        /* Oops */
        return;
    }

    /* Wipe the hooks */
    for (k = 0; k < 26; k++) p->interactive_hook[k][0] = '\0';

    /* Parse the file */
    while (true)
    {
        byte attr = COLOUR_WHITE;

        /* Read a line or stop */
        if (!file_getl(fff, buf, sizeof(buf))) break;

        /* Skip lines if we are inside a RST directive */
        if (skip_lines)
        {
            if (contains_only_spaces(buf)) skip_lines = false;
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
                    my_strcpy(p->interactive_hook[k], buf + strlen(".. menu:: [x] "),
                        sizeof(p->interactive_hook[0]));
                }
            }

            /* Skip this and enter skip mode */
            skip_lines = true;
            continue;
        }

        /* Skip '|' characters */
        if (strchr(buf, '|') && !strstr(buf, "'|'")) strskip(buf, '|', '\\');

        /* Escape backslashes */
        strescape(buf, '\\');

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
            p->info[i][k - color].a = attr;
            p->info[i][k - color].c = buf[k];
        }

        /* Count the "info[]" lines */
        i++;
    }

    /* Save last "real" line */
    p->interactive_size = next;

    /* Save last dumped line */
    p->last_info_line = i - 1;

    /* Protect our info area */
    free_info_icky(p);
    free_header_icky(p);
    alloc_info_icky(p);
    alloc_header_icky(p, "Help");

    /* Close the file */
    file_close(fff);
}


/*
 * On-Line help.
 *
 * Process user commands, access sub-menu entries and browse files.
 * This function manages a virtual 'window' which buffers file
 * contents using "copy_file_info" function.
 */
void common_file_peruse(struct player *p, u32b query)
{
    int next = p->interactive_next;

    /* Enter sub-menu */
    if (isalpha(query))
    {
        /* Extract the requested menu item */
        int k = A2I(query);

        /* Verify the menu item */
        if ((k >= 0) && (k < 26) && !STRZERO(p->interactive_hook[k]))
        {
            /* Paranoia -- free string */
            string_free(p->interactive_file);

            /* Select that file */
            p->interactive_file = string_make(p->interactive_hook[k]);

            /* Hack -- enforce update */
            p->interactive_next = -1;
            next = 0;

            /* Query processed */
            query = 0;
        }
    }

    /* Use default file */
    if (!p->interactive_file)
    {
        p->interactive_file = string_make("help.hlp");

        /* Hack -- enforce update */
        p->interactive_next = -1;
        next = 0;

        /* Query processed */
        query = 0;
    }

    /* We're just starting. Reset counter */
    if (!query) p->interactive_line = 0;

    /* We're done. Clear file, exit */
    if (query == ESCAPE)
    {
        string_free(p->interactive_file);
        p->interactive_file = NULL;
        free_info_icky(p);
        free_header_icky(p);
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
                p->interactive_line--;
                break;

            /* Back up one full page */
            case '9':
            case '-':
                p->interactive_line -= 20;
                break;

            /* Back to the top */
            case '7':
                p->interactive_line = 0;
                break;

            /* Advance one line */
            case ARROW_DOWN:
            case '2':
            case KC_ENTER:
                p->interactive_line++;
                break;

            /* Advance one full page */
            case '3':
            case ' ':
                p->interactive_line += 20;
                break;

            /* Advance to the bottom */
            case '1':
                p->interactive_line = p->interactive_size - 20;
                break;
        }

        /* Adjust viewport boundaries */
        if (p->interactive_line > p->interactive_size - 20)
            p->interactive_line = p->interactive_size - 20;
        if (p->interactive_line < 0) p->interactive_line = 0;

        /* Shift window! */
        if ((p->interactive_line + 20 > p->interactive_next + MAX_TXT_INFO) ||
                (p->interactive_line < p->interactive_next))
            next = p->interactive_line - MAX_TXT_INFO / 2;

        /* Adjust window boundaries */
        if (next > p->interactive_size - MAX_TXT_INFO)
            next = p->interactive_size - MAX_TXT_INFO;
        if (next < 0) next = 0;
    }

    /* Update file */
    if (next != p->interactive_next)
    {
        p->interactive_next = next;
        copy_file_info(p, p->interactive_file, next, 0);
    }
}
