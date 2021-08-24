/*
 * File: ui-spell.c
 * Purpose: Spell UI handing
 *
 * Copyright (c) 2010 Andi Sidwell
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


#include "c-angband.h"


/*
 * Spell information and description
 */
struct book_info book_info[MAX_PAGES];


/* Hack -- last row of text on the screen */
static int last_row;


/*
 * Spell menu data struct
 */
struct spell_menu_data
{
    int book;
    int n_spells;

    bool browse;
    bool (*is_valid)(int, int);
    bool show_description;

    int selected_spell;
};


/*
 * Is item oid valid?
 */
static int spell_menu_valid(struct menu *m, int oid)
{
    struct spell_menu_data *d = menu_priv(m);

    return d->is_valid(d->book, oid);
}


/*
 * Display a row of the spell menu
 */
static void spell_menu_display(struct menu *m, int oid, bool cursor, int row, int col, int wid)
{
    struct spell_menu_data *d = menu_priv(m);

    /* Dump the spell --(-- */
    c_prt(book_info[d->book].spell_info[oid].flag.line_attr,
        book_info[d->book].spell_info[oid].info, row, col);
}


/*
 * Handle an event on a menu row
 */
static bool spell_menu_handler(struct menu *m, const ui_event *e, int oid)
{
    struct spell_menu_data *d = menu_priv(m);

    if (e->type == EVT_SELECT)
    {
        d->selected_spell = oid;
        return d->browse;
    }
    if (e->type == EVT_KBRD)
    {
        if (e->key.code == '?')
            d->show_description = !d->show_description;
    }

    return false;
}


static void spell_menu_erase(void)
{
    int x, y;

    Term_locate(&x, &y);

    /* Hack -- always finish at the end of a tile in bigtile mode */
    if (tile_height > 1)
    {
        int ymax = ((y - ROW_MAP) / tile_height) * tile_height + ROW_MAP + tile_height - 1;

        while (++y <= ymax) Term_erase(x - 1, y, 255);
    }

    /* Hack -- if we use a distorted display, don't refresh the last rows */
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


/*
 * Show spell long description when browsing
 */
static void spell_menu_browser(int oid, void *data, const region *loc)
{
    struct spell_menu_data *d = data;
    char desc[MSG_LEN];

    /* FIXME -- redisplay the header */
    Term_putstr(loc->col, loc->row - 1, loc->width, COLOUR_WHITE,
        "Name                             Lv Mana Fail Info");

    if (!d->show_description) return;

    /* Redirect output to the screen */
    Term_gotoxy(loc->col, loc->row + loc->page_rows);
    strnfmt(desc, sizeof(desc), "\n%s\n\n", book_info[d->book].spell_info[oid].desc);
    text_out_to_screen(COLOUR_WHITE, desc);

    spell_menu_erase();
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
    int i = 0, n_spells = 0;
    const struct player_class *c = player->clazz;

    if (player->ghost && !player_can_undead(player)) c = lookup_player_class("Ghost");

    /* Paranoia */
    if (book < 0) return 0;
    if (book >= c->magic.num_books) return 0;

    /* Check for end of the book */
    while (book_info[book].spell_info[i].info[0] != '\0')
    {
        /* Spell is available */
        n_spells++;

        i++;
    }

    return n_spells;
}


/*
 * True if at least one spell is OK according to spell_test
 */
static bool spell_okay_list(bool (*spell_test)(int, int), int book, int n_spells)
{
    int i;

    for (i = 0; i < n_spells; i++)
    {
        if (spell_test(book, i)) return true;
    }

    return false;
}


/*
 * Create and initialize a spell menu, given a book and a validity hook
 */
static struct menu *spell_menu_new(int book, bool (*is_valid)(int, int))
{
    struct menu *m = menu_new(MN_SKIN_SCROLL, &spell_menu_iter);
    struct spell_menu_data *d = mem_alloc(sizeof(*d));
    region loc = { -62, 1, 62, -99 };

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
    d->browse = false;
    d->show_description = false;

    menu_setpriv(m, d->n_spells, d);

    /* Set flags */
    m->header = "Name                             Lv Mana Fail Info";
    m->flags = MN_CASELESS_TAGS;
    m->selections = lower_case;
    m->browse_hook = spell_menu_browser;
    m->cmd_keys = "?";

    /* Set size */
    loc.page_rows = d->n_spells + 1;
    menu_layout(m, &loc);

    /* Hack -- reset last row */
    last_row = 0;

    return m;
}


/*
 * Clean up a spell menu instance
 */
static void spell_menu_destroy(struct menu *m)
{
    struct spell_menu_data *d = menu_priv(m);

    /* Hack -- reset last row */
    last_row = 0;

    mem_free(d);
    mem_free(m);
}


/*
 * Run the spell menu to select a spell
 */
static int spell_menu_select(struct menu *m, const char *noun, const char *verb)
{
    struct spell_menu_data *d = menu_priv(m);
    char buf[NORMAL_WID];

    screen_save();
    region_erase_bordered(&m->active);

    /* Format, capitalise and display */
    strnfmt(buf, sizeof(buf), "%s which %s? ('?' to toggle description)", verb, noun);
    my_strcap(buf);
    prt(buf, 0, 0);

    menu_select(m, 0, true);
    screen_load(true);

    return d->selected_spell;
}


/*
 * Run the spell menu, without selections
 */
static void spell_menu_browse(struct menu *m, const char *noun)
{
    struct spell_menu_data *d = menu_priv(m);

    screen_save();

    region_erase_bordered(&m->active);
    prt(format("Browsing %ss. ('?' to toggle description)", noun), 0, 0);

    d->browse = true;
    menu_select(m, 0, true);

    screen_load(true);
}


/*
 * True if the spell is browsable
 */
static bool spell_okay_to_browse(int book, int spell_index)
{
    int attr = book_info[book].spell_info[spell_index].flag.line_attr;

    return (attr != COLOUR_L_DARK);
}


/*
 * Browse a given book
 */
void textui_book_browse(int book)
{
    struct menu *m;

    m = spell_menu_new(book, spell_okay_to_browse);
    if (m)
    {
        spell_menu_browse(m, book_info[book].realm->spell_noun);
        spell_menu_destroy(m);
    }
    else
        c_msg_print("You cannot browse that.");
}


/*
 * Get a spell from specified book.
 */
static int textui_get_spell_from_book(int book, const char *verb, bool (*spell_filter)(int, int))
{
    struct menu *m;

    m = spell_menu_new(book, spell_filter);
    if (m)
    {
        int spell_index = spell_menu_select(m, book_info[book].realm->spell_noun, verb);

        spell_menu_destroy(m);
        return spell_index;
    }

    return -1;
}


/*
 * Interactively select a spell.
 *
 * Returns the spell selected, or -1.
 *
 * "inkey_next" is used to disable spell selection during keymaps.
 */
int textui_get_spell(int book, const char *verb, bool (*spell_filter)(int, int))
{
    bool hidden = (inkey_next? true: false);
    int spell = -1;

    /* The top line is icky */
    topline_icky = true;

    /* Disable spell selection during keymaps */
    if (hidden)
    {
        int num;
        struct keypress which;

        /* Collect spells from book */
        num = spell_collect_from_book(book);
        if (num && spell_okay_list(spell_filter, book, num))
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
        spell = textui_get_spell_from_book(book, verb, spell_filter);

    /* Fix the top line */
    topline_icky = false;

    /* Flush any events */
    Flush_queue();

    /* Clear the prompt line */
    prt("", 0, 0);

    /* Result */
    return spell;
}


/*
 * True if the spell can be studied
 */
bool spell_okay_to_study(int book, int spell_index)
{
    int attr = book_info[book].spell_info[spell_index].flag.line_attr;
    const char *name = book_info[book].realm->name;

    return ((attr == COLOUR_L_BLUE) || ((attr == COLOUR_WHITE) && streq(name, "elemental")));
}


/*
 * True if the spell is castable
 */
bool spell_okay_to_cast(int book, int spell)
{
    int attr = book_info[book].spell_info[spell].flag.line_attr;

    return ((attr == COLOUR_WHITE) || (attr == COLOUR_L_GREEN));
}


/*
 * Cast/project a spell from a book
 */
static int textui_obj_cast_aux(int book, bool project, int *dir)
{
    const struct player_class *c = player->clazz;
    int spell;

    if (player->ghost && !player_can_undead(player)) c = lookup_player_class("Ghost");

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
            i = 0;
            num = 0;

            /* Check for end of the book */
            while (book_info[cur_page].spell_info[i].info[0] != '\0')
            {
                int cur_flag;

                /* Spell is available */
                num++;
                flags[flag_count].flag = book_info[cur_page].spell_info[i].flag.flag;
                flags[flag_count].dir_attr = book_info[cur_page].spell_info[i].flag.dir_attr;
                flags[flag_count].proj_attr = book_info[cur_page].spell_info[i].flag.proj_attr;
                flag_count++;

                /* Note min and max flags */
                cur_flag = book_info[cur_page].spell_info[i].flag.flag;
                if (cur_flag < min_flag) min_flag = cur_flag;
                if (cur_flag > max_flag) max_flag = cur_flag;

                i++;
            }
            if (num > 0) cur_page++;
        }
        while ((num > 0) && (cur_page < MAX_PAGES));

        /* Ask for a spell, allow cancel */
        strnfmt(tmp, sizeof(tmp), "Select a spell (%d-%d): ", min_flag, max_flag);
        spell = get_quantity(tmp, max_flag);
        if ((spell < min_flag) || (spell > max_flag)) return -1;

        /* Check for available spell flags */
        for (i = 0; i < flag_count; i++)
        {
            if (spell == flags[i].flag) break;
        }
        if (i == flag_count)
        {
            c_msg_print("You don't know this monster spell.");
            return -1;
        }

        /* Projectable */
        if (project && flags[i].proj_attr) spell = 0 - spell;

        /* Needs a direction */
        if (flags[i].dir_attr || (project && flags[i].proj_attr))
        {
            /* Allow direction to be cancelled for free */
            if (!get_aim_dir(dir)) return -1;
        }
    }

    /* Cast a spell by using book/spell number */
    else
    {
        spell_flags flag;

        /* Ask for a spell, allow cancel */
        spell = get_spell(book, book_info[book].realm->verb, spell_okay_to_cast);
        if (spell == -1) return -1;

        /* Projectable */
        flag = book_info[book].spell_info[spell].flag;
        if (project && flag.proj_attr) spell += c->magic.total_spells;

        /* Needs a direction */
        if (flag.dir_attr || (project && flag.proj_attr))
        {
            /* Allow direction to be cancelled for free */
            if (!get_aim_dir(dir)) return -1;
        }
    }

    return spell;
}


/*
 * Cast a spell from a book
 */
int textui_obj_cast(int book, int *dir)
{
    int result;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    result = textui_obj_cast_aux(book, false, dir);

    allow_disturb_icky = true;
    return result;
}


/*
 * Project a spell from a book
 */
int textui_obj_project(int book, int *dir)
{
    return textui_obj_cast_aux(book, true, dir);
}
