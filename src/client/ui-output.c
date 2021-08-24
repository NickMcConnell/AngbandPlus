/*
 * File: ui-output.c
 * Purpose: Putting text on the screen, screen saving and loading, panel handling
 *
 * Copyright (c) 2007 Pete Mack and others.
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


#include "c-angband.h"


/* Hack -- icky screen */
bool full_icky_screen;
bool target_icky_screen;


const region SCREEN_REGION = {0, 0, 0, 0};


/*
 * Regions
 */


/*
 * These functions are used for manipulating regions on the screen, used
 * mostly (but not exclusively) by the menu functions.
 */


region region_calculate(region loc)
{
    int w, h;

    Term_get_size(&w, &h);

    if (loc.col < 0) loc.col += w;
    if (loc.row < 0) loc.row += h;
    if (loc.width <= 0) loc.width += w - loc.col;
    if (loc.page_rows <= 0) loc.page_rows += h - loc.row;

    return loc;
}


void region_erase_bordered(const region *loc)
{
    region calc = region_calculate(*loc);
    int i;

    calc.col = MAX(calc.col - 1, 0);
    calc.row = MAX(calc.row - 1, 0);
    calc.width += 2;
    calc.page_rows += 2;

    for (i = 0; i < calc.page_rows; i++)
        Term_erase(calc.col, calc.row + i, calc.width);
}


void region_erase(const region *loc)
{
    region calc = region_calculate(*loc);
    int i;

    for (i = 0; i < calc.page_rows; i++)
        Term_erase(calc.col, calc.row + i, calc.width);
}


/*
 * text_out hook for screen display
 */


/*
 * Print some (colored) text to the screen at the current cursor position,
 * automatically "wrapping" existing text (at spaces) when necessary to
 * avoid placing any text into the last column, and clearing every line
 * before placing any text in that line.  Also, allow "newline" to force
 * a "wrap" to the next line.  Advance the cursor as needed so sequential
 * calls to this function will work correctly.
 *
 * Once this function has been called, the cursor should not be moved
 * until all the related "text_out()" calls to the window are complete.
 *
 * This function will correctly handle any width up to the maximum legal
 * value of 256, though it works best for a standard 80 character width.
 */
void text_out_to_screen(byte a, const char *str)
{
    int text_out_wrap = 0, text_out_indent, text_out_pad = 1;
    int x, y;
    int wid, h;
    int wrap;
    const char *s;
    char buf[MSG_LEN];

    /* Obtain the size */
    Term_get_size(&wid, &h);

    /* Obtain the cursor */
    Term_locate(&x, &y);
    text_out_indent = x - 1;

    /* Copy to a rewriteable string */
    my_strcpy(buf, str, MSG_LEN);

    /* Use special wrapping boundary? */
    if ((text_out_wrap > 0) && (text_out_wrap < wid))
        wrap = text_out_wrap;
    else
        wrap = wid;

    /* Process the string */
    for (s = buf; *s; s++)
    {
        char ch;

        /* Force wrap */
        if (*s == '\n')
        {
            /* Wrap */
            x = text_out_indent;
            y++;

            /* Clear line, move cursor */
            Term_erase(x, y, 255);

            x += text_out_pad;
            Term_gotoxy(x, y);

            continue;
        }

        /* Clean up the char */
        ch = (isprint((unsigned char)*s)? *s: ' ');

        /* Wrap words as needed */
        if ((x >= wrap - 1) && (ch != ' '))
        {
            int i, n = 0;
            u16b av[256];
            char cv[256];

            /* Wrap word */
            if (x < wrap)
            {
                /* Scan existing text */
                for (i = wrap - 2; i >= 0; i--)
                {
                    /* Grab existing attr/char */
                    Term_what(i, y, &av[i], &cv[i]);

                    /* Break on space */
                    if (cv[i] == ' ') break;

                    /* Track current word */
                    n = i;
                }
            }

            /* Special case */
            if (n == 0) n = wrap;

            /* Clear line */
            Term_erase(n, y, 255);

            /* Wrap */
            x = text_out_indent;
            y++;

            /* Clear line, move cursor */
            Term_erase(x, y, 255);

            x += text_out_pad;
            Term_gotoxy(x, y);

            /* Wrap the word (if any) */
            for (i = n; i < wrap - 1; i++)
            {
                /* Dump */
                Term_addch(av[i], cv[i]);

                /* Advance (no wrap) */
                if (++x > wrap) x = wrap;
            }
        }

        /* Dump */
        Term_addch(a, ch);

        /* Advance */
        if (++x > wrap) x = wrap;
    }
}


/*
 * Simple text display
 */


/*
 * Display a string on the screen using an attribute.
 *
 * At the given location, using the given attribute, if allowed,
 * add the given string. Do not clear the line.
 */
void c_put_str(byte attr, const char *str, int row, int col)
{
    /* Position cursor, Dump the attr/text */
    Term_putstr(col, row, -1, attr, str);
}


/*
 * As above, but in "white"
 */
void put_str(const char *str, int row, int col)
{
    /* Spawn */
    Term_putstr(col, row, -1, COLOUR_WHITE, str);
}


/*
 * Display a string on the screen using an attribute, and clear
 * to the end of the line.
 */
void c_prt(byte attr, const char *str, int row, int col)
{
    /* Clear line, position cursor */
    Term_erase(col, row, 255);

    /* Dump the attr/text */
    Term_addstr(-1, attr, str);
}


/*
 * As above, but in "white"
 */
void prt(const char *str, int row, int col)
{
    /* Spawn */
    c_prt(COLOUR_WHITE, str, row, col);
}


/*
 * Screen loading/saving
 */


/*
 * Screen loading and saving can be done to an arbitrary depth but it's
 * important that every call to screen_save() is balanced by a call to
 * screen_load() later on.
 */


/*
 * Save the screen, and increase the "icky" depth.
 *
 * This function must match exactly one call to "screen_load()".
 */
void screen_save(void)
{
    /* Increase "icky" depth */
    player->screen_save_depth++;
    Send_icky();

    /* Save the screen (if legal) */
    Term_save();
}


/*
 * Load the screen, and decrease the "icky" depth.
 *
 * This function must match exactly one call to "screen_save()".
 */
void screen_load(bool flush)
{
    /* Load the screen (if legal) */
    Term_load();

    /* Decrease "icky" depth */
    player->screen_save_depth--;
    Send_icky();

    /* Flush any queued events */
    if (flush) Flush_queue();

    /* Redraw distorted graphics */
    if (!player->screen_save_depth && (tile_distorted || full_icky_screen)) Term_redraw();
}


/*
 * Miscellaneous things
 */


/*
 * A Hengband-like 'window' function, that draws a surround box in ASCII art.
 */
void window_make(int origin_x, int origin_y, int end_x, int end_y)
{
    int n;
    region to_clear;

    to_clear.col = origin_x;
    to_clear.row = origin_y;
    to_clear.width = end_x - origin_x;
    to_clear.page_rows = end_y - origin_y;

    region_erase(&to_clear);

    Term_putch(origin_x, origin_y, COLOUR_WHITE, '+');
    Term_putch(end_x, origin_y, COLOUR_WHITE, '+');
    Term_putch(origin_x, end_y, COLOUR_WHITE, '+');
    Term_putch(end_x, end_y, COLOUR_WHITE, '+');

    for (n = 1; n < (end_x - origin_x); n++)
    {
        Term_putch(origin_x + n, origin_y, COLOUR_WHITE, '-');
        Term_putch(origin_x + n, end_y, COLOUR_WHITE, '-');
    }

    for (n = 1; n < (end_y - origin_y); n++)
    {
        Term_putch(origin_x, origin_y + n, COLOUR_WHITE, '|');
        Term_putch(end_x, origin_y + n, COLOUR_WHITE, '|');
    }
}


/*
 * Given a "formatted" chunk of text (i.e. one including tags like {red}{/})
 * in 'source', with starting point 'init', this finds the next section of
 * text and any tag that goes with it, return true if it finds something to
 * print.
 *
 * If it returns true, then it also fills 'text' with a pointer to the start
 * of the next printable section of text, and 'len' with the length of that
 * text, and 'end' with a pointer to the start of the next section.  This
 * may differ from "text + len" because of the presence of tags.  If a tag
 * applies to the section of text, it returns a pointer to the start of that
 * tag in 'tag' and the length in 'taglen'.  Otherwise, 'tag' is filled with
 * NULL.
 *
 * See text_out_e for an example of its use.
 */
static bool next_section(const char *source, size_t init, const char **text,
    size_t *len, const char **tag, size_t *taglen, const char **end)
{
    const char *next;

    *tag = NULL;
    *text = source + init;
    if (*text[0] == '\0') return false;

    next = strchr(*text, '{');
    while (next)
    {
        const char *s = next + 1;

        while (*s && (isalpha((unsigned char)*s) || isspace((unsigned char)*s))) s++;

        /* Woo!  valid opening tag thing */
        if (*s == '}')
        {
            const char *close = strstr(s, "{/}");

            /* There's a closing thing, so it's valid. */
            if (close)
            {
                /* If this tag is at the start of the fragment */
                if (next == *text)
                {
                    *tag = *text + 1;
                    *taglen = s - *text - 1;
                    *text = s + 1;
                    *len = close - *text;
                    *end = close + 3;
                    return true;
                }

                /* Otherwise return the chunk up to this */
                else
                {
                    *len = next - *text;
                    *end = *text + *len;
                    return true;
                }
            }

            /* No closing thing, therefore all one lump of text. */
            else
            {
                *len = strlen(*text);
                *end = *text + *len;
                return true;
            }
        }

        /* End of the string, that's fine. */
        else if (*s == '\0')
        {
            *len = strlen(*text);
            *end = *text + *len;
            return true;
        }

        /* An invalid tag, skip it. */
        else
            next = next + 1;

        next = strchr(next, '{');
    }

    /* Default to the rest of the string */
    *len = strlen(*text);
    *end = *text + *len;

    return true;
}


/*
 * Output text to the screen or to a file depending on the
 * selected hook. Takes strings with "embedded formatting",
 * such that something within {red}{/} will be printed in red.
 *
 * Note that such formatting will be treated as a "breakpoint"
 * for the printing, so if used within words may lead to part of the
 * word being moved to the next line.
 */
void text_out_e(const char *buf, int y, int xoffset)
{
    char smallbuf[MSG_LEN];
    const char *start;
    const char *next, *text, *tag;
    size_t textlen, taglen = 0;
    int x = 0;

    start = buf;
    while (next_section(start, 0, &text, &textlen, &tag, &taglen, &next))
    {
        int a = -1;

        memcpy(smallbuf, text, textlen);
        smallbuf[textlen] = 0;

        if (tag)
        {
            char tagbuffer[16];

            /* Colour names are less than 16 characters long. */
            assert(taglen < 16);

            memcpy(tagbuffer, tag, taglen);
            tagbuffer[taglen] = '\0';

            a = color_text_to_attr(tagbuffer);
        }

        if (a == -1) a = COLOUR_WHITE;

        /* Output now */
        c_put_str(a, smallbuf, y, x + xoffset);

        /* Advance */
        x += strlen(smallbuf);

        start = next;
    }
}
