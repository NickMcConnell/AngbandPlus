/*
 * File: ui-spell.c
 * Purpose: Spell UI handing
 *
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
#include "../common/tvalsval.h"
#include "c-cmds.h"
#include "netclient.h"
#include "ui-menu.h"


/* Hack -- Last row of text on the screen */
static int last_row;


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
static void text_out_to_screen(byte a, const char *str)
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
            byte av[256];
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
 * Spell menu data struct
 */
struct spell_menu_data
{
    int book;
    int n_spells;

    bool browse;
    bool (*is_valid)(int book, int spell);

    int selected_spell;
};


/*
 * Is item oid valid?
 */
static int spell_menu_valid(menu_type *m, int oid)
{
    struct spell_menu_data *d = menu_priv(m);

    return d->is_valid(d->book, oid);
}


/*
 * Display a row of the spell menu
 */
static void spell_menu_display(menu_type *m, int oid, bool cursor, int row, int col, int wid)
{
    struct spell_menu_data *d = menu_priv(m);

    /* Dump the spell --(-- */
    c_prt(spell_flag[d->book][oid].line_attr, spell_info[d->book][oid], row, col);
}


/*
 * Handle an event on a menu row
 */
static bool spell_menu_handler(menu_type *m, const ui_event *e, int oid)
{
    struct spell_menu_data *d = menu_priv(m);

    if (e->type == EVT_SELECT)
    {
        d->selected_spell = oid;
        return d->browse;
    }

    return FALSE;
}


/*
 * Show spell long description when browsing
 */
static void spell_menu_browser(int oid, void *data, const region *loc)
{
    struct spell_menu_data *d = data;
    char desc[MSG_LEN];
    int x, y;

    /* FIXME -- Redisplay the header */
    Term_putstr(loc->col, loc->row - 1, loc->width, TERM_WHITE,
        "Name                             Lv Mana Fail Info");

    /* Redirect output to the screen */
    Term_gotoxy(loc->col, loc->row + loc->page_rows);
    strnfmt(desc, sizeof(desc), "\n%s\n", spell_desc[d->book][oid]);
    text_out_to_screen(TERM_WHITE, desc);
    Term_locate(&x, &y);

    /* Hack -- Always finish at the end of a tile in bigtile mode */
    if (tile_height > 1)
    {
        int ymax = ((y - ROW_MAP) / tile_height) * tile_height + ROW_MAP + tile_height - 1;

        while (++y <= ymax) Term_erase(x - 1, y, 255);
    }

    /* Hack -- If we use a distorted display, don't refresh the last rows */
    if (Term->max_hgt != Term->hgt)
    {
        if (--y > last_row)
            last_row = y;
        else if (y < last_row)
        {
            while (++y <= last_row) Term_erase(x - 1, y, 255);
        }
    }
}


static const menu_iter spell_menu_iter =
{
    NULL,   /* get_tag = NULL, just use lowercase selections */
    spell_menu_valid,
    spell_menu_display,
    spell_menu_handler,
    NULL    /* no resize hook */
};


/*
 * Collect spells from a book
 */
static int spell_collect_from_book(int book)
{
    int i;
    int n_spells = 0;

    for (i = 0; i < SPELLS_PER_BOOK; i++)
    {
        /* Check for end of the book */
        if (spell_info[book][i][0] == '\0') break;

        /* Spell is available */
        n_spells++;
    }

    return n_spells;
}


/*
 * True if at least one spell is OK according to spell_test
 */
static bool spell_okay_list(bool (*spell_test)(int book, int spell), int book, int n_spells)
{
    int i;

    for (i = 0; i < n_spells; i++)
    {
        if (spell_test(book, i)) return TRUE;
    }

    return FALSE;
}


/*
 * Create and initialise a spell menu, given a book and a validity hook
 */
static menu_type *spell_menu_new(int book, bool (*is_valid)(int book, int spell))
{
    menu_type *m = menu_new(MN_SKIN_SCROLL, &spell_menu_iter);
    struct spell_menu_data *d = mem_alloc(sizeof(*d));
    region loc = { -60, 1, 60, -99 };

    /* Collect spells from book */
    d->book = book;
    d->n_spells = spell_collect_from_book(book);
    if ((d->n_spells == 0) || !spell_okay_list(is_valid, d->book, d->n_spells))
    {
        mem_free(m);
        mem_free(d);
        return NULL;
    }

    /* Copy across private data */
    d->is_valid = is_valid;
    d->selected_spell = -1;
    d->browse = FALSE;

    menu_setpriv(m, d->n_spells, d);

    /* Set flags */
    m->header = "Name                             Lv Mana Fail Info";
    m->flags = MN_CASELESS_TAGS;
    m->selections = lower_case;
    m->browse_hook = spell_menu_browser;

    /* Set size */
    loc.page_rows = d->n_spells + 1;
    menu_layout(m, &loc);

    /* Hack -- Reset last row */
    last_row = 0;

    return m;
}


/*
 * Clean up a spell menu instance
 */
static void spell_menu_destroy(menu_type *m)
{
    struct spell_menu_data *d = menu_priv(m);

    /* Hack -- Reset last row */
    last_row = 0;

    mem_free(d);
    mem_free(m);
}


/*
 * Run the spell menu to select a spell
 */
static int spell_menu_select(menu_type *m, const char *noun, const char *verb)
{
    struct spell_menu_data *d = menu_priv(m);
    char buf[NORMAL_WID];

    screen_save();

    region_erase_bordered(&m->active);

    /* Format, capitalise and display */
    strnfmt(buf, sizeof(buf), "%s which %s? ", verb, noun);
    my_strcap(buf);
    prt(buf, 0, 0);

    menu_select(m, 0, TRUE);
    screen_load(TRUE);

    return d->selected_spell;
}


/*
 * Run the spell menu, without selections
 */
static void spell_menu_browse(menu_type *m, const char *noun)
{
    struct spell_menu_data *d = menu_priv(m);

    screen_save();

    region_erase_bordered(&m->active);
    prt(format("Browsing %ss. Press Escape to exit.", noun), 0, 0);

    d->browse = TRUE;
    menu_select(m, 0, TRUE);

    screen_load(TRUE);
}


/*
 * Interactively select a spell.
 *
 * Returns the spell selected, or -1.
 *
 * "inkey_next" is used to disable spell selection during keymaps.
 */
static int get_spell(int book, const char *verb, bool (*spell_test)(int book, int spell))
{
    bool hidden = (inkey_next? TRUE: FALSE);
    int spell = -1;

    /* The top line is icky */
    topline_icky = TRUE;

    /* Disable spell selection during keymaps */
    if (hidden)
    {
        int num;
        struct keypress which;

        /* Collect spells from book */
        num = spell_collect_from_book(book);
        if (num && spell_okay_list(spell_test, book, num))
        {
            /* Get a key */
            which = inkey();

            /* Accept lowercase letters */
            if (islower(which.code))
            {
                spell = A2I(which.code);
                if (spell >= num) spell = -1;
            }

            /* Macros are supposed to be accurate */
            if (spell == -1) bell("Illegal spell choice!");
        }
    }

    /* Interactively select a spell */
    else
    {
        menu_type *m;
        const char *noun;

        if (p_ptr->ghost && !player_can_undead(p_ptr)) noun = "power";
        else if (p_ptr->clazz->spell_book == TV_PRAYER_BOOK) noun = "prayer";
        else noun = "spell";

        m = spell_menu_new(book, spell_test);

        if (m)
        {
            spell = spell_menu_select(m, noun, verb);
            spell_menu_destroy(m);
        }
    }

    /* Fix the top line */
    topline_icky = FALSE;

    /* Flush any events */
    Flush_queue();

    /* Clear the prompt line */
    prt("", 0, 0);

    /* Result */
    return (spell);
}


/*
 * True if the spell is browsable
 */
static bool spell_okay_to_browse(int book, int spell)
{
    int attr = spell_flag[book][spell].line_attr;

    return (attr != TERM_L_DARK);
}


/*
 * Browse the given book
 */
void textui_spell_browse(int book)
{
    menu_type *m;
    const char *noun;

    if (p_ptr->ghost && !player_can_undead(p_ptr)) noun = "power";
    else if (p_ptr->clazz->spell_book == TV_PRAYER_BOOK) noun = "prayer";
    else noun = "spell";

    m = spell_menu_new(book, spell_okay_to_browse);
    if (m)
    {
        spell_menu_browse(m, noun);
        spell_menu_destroy(m);
    }
    else
        c_msg_print("You cannot browse that.");
}


/*
 * True if the spell can be studied
 */
bool spell_okay_to_study(int book, int spell)
{
    int attr = spell_flag[book][spell].line_attr;

    return ((attr == TERM_L_BLUE) ||
        ((attr == TERM_WHITE) && (p_ptr->clazz->spell_book == TV_ELEM_BOOK)));
}


/*
 * Study a book to gain a new spell
 * Return -1 to abort, -2 to learn a random spell
 */
int textui_obj_study(int book)
{
    /* Learn random spell */
    int spell = -2;

    /* Learn a selected spell */
    if (player_has(p_ptr, PF_CHOOSE_SPELLS))
    {
        /* Ask for a spell, allow cancel */
        spell = get_spell(book, "study", spell_okay_to_study);
    }

    return spell;
}


/*
 * True if the spell is castable
 */
bool spell_okay_to_cast(int book, int spell)
{
    int attr = spell_flag[book][spell].line_attr;

    return ((attr == TERM_WHITE) || (attr == TERM_L_GREEN));
}


/*
 * Cast/project a spell from a book
 */
static void textui_obj_cast_aux(int book, bool project)
{
    int spell;
    const char *verb;
    int dir = 0;

    if (p_ptr->ghost && !player_can_undead(p_ptr)) verb = "use";
    else if (p_ptr->clazz->spell_book == TV_PRAYER_BOOK) verb = "recite";
    else verb = "cast";

    /* Cast a spell directly by using spell flag */
    if (book == -1)
    {
        int i, num;
        int cur_page = 0;
        int min_flag = RSF_MAX, max_flag = RSF_NONE;
        spell_flags flags[RSF_MAX];
        int flag_count = 0;
        char tmp[NORMAL_WID];

        /* Get available spell flags */
        do
        {
            /* Check for available spells */
            for (i = 0, num = 0; i < SPELLS_PER_BOOK; i++)
            {
                int cur_flag;

                /* Check for end of the book */
                if (spell_info[cur_page][i][0] == '\0') break;

                /* Spell is available */
                num++;
                flags[flag_count].flag = spell_flag[cur_page][i].flag;
                flags[flag_count].dir_attr = spell_flag[cur_page][i].dir_attr;
                flags[flag_count].proj_attr = spell_flag[cur_page][i].proj_attr;
                flag_count++;

                /* Note min and max flags */
                cur_flag = spell_flag[cur_page][i].flag;
                if (cur_flag < min_flag) min_flag = cur_flag;
                if (cur_flag > max_flag) max_flag = cur_flag;
            }
            if (num > 0) cur_page++;
        }
        while ((num > 0) && (cur_page < BOOKS_PER_REALM));

        /* Ask for a spell, allow cancel */
        strnfmt(tmp, sizeof(tmp), "Select a spell (%d-%d): ", min_flag, max_flag);
        spell = get_quantity(tmp, max_flag);
        if ((spell < min_flag) || (spell > max_flag)) return;

        /* Check for available spell flags */
        for (i = 0; i < flag_count; i++)
        {
            if (spell == flags[i].flag) break;
        }
        if (i == flag_count)
        {
            c_msg_print("You don't know this monster spell.");
            return;
        }

        /* Projectable */
        if (project && flags[i].proj_attr) spell = 0 - spell;

        /* Needs a direction */
        if (flags[i].dir_attr || (project && flags[i].proj_attr))
        {
            /* Allow direction to be cancelled for free */
            if (!get_aim_dir(&dir)) return;
        }
    }

    /* Cast a spell by using book/spell number */
    else
    {
        spell_flags flag;

        /* Ask for a spell, allow cancel */
        spell = get_spell(book, verb, spell_okay_to_cast);
        if (spell == -1) return;

        /* Projectable */
        flag = spell_flag[book][spell];
        if (project && flag.proj_attr) spell += PY_MAX_SPELLS;

        /* Needs a direction */
        if (flag.dir_attr || (project && flag.proj_attr))
        {
            /* Allow direction to be cancelled for free */
            if (!get_aim_dir(&dir)) return;
        }
    }

    /* Tell the server */
    if (p_ptr->ghost && !player_can_undead(p_ptr)) Send_ghost(spell, dir);
    else if (player_has(p_ptr, PF_MONSTER_SPELLS)) Send_mimic(book, spell, dir);
    else if (p_ptr->clazz->spell_book == TV_PRAYER_BOOK) Send_pray(book, spell, dir);
    else Send_cast(book, spell, dir);
}


/*
 * Cast a spell from a book
 */
void textui_obj_cast(int book)
{
    textui_obj_cast_aux(book, FALSE);
}


/*
 * Project a spell from a book
 */
void textui_obj_project(int book)
{
    textui_obj_cast_aux(book, TRUE);
}
