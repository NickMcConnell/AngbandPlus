/*
 * File: ui-options.c
 * Purpose: Text UI options handling code (everything accessible from '=')
 *
 * Copyright (c) 1997-2000 Robert A. Koeneke, James E. Wilson, Ben Harrison
 * Copyright (c) 2007 Pete Mack
 * Copyright (c) 2010 Andi Sidwell
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


/*
 * Prompt the user for a filename to save the pref file to.
 */
static bool get_pref_path(const char *what, int row, char *buf, size_t max)
{
    char ftmp[NORMAL_WID];
    int res;
    ui_event ea = EVENT_ABORT;

    screen_save();

    /* Prompt */
    prt(format("%s to a pref file", what), row, 0);
    prt("File: ", row + 2, 0);

    /* Default filename */
    strnfmt(ftmp, sizeof(ftmp), "%s.prf", strip_suffix(nick));

    /* Get a filename */
    res = askfor_ex(ftmp, sizeof(ftmp), NULL, false);

    /* Build the filename */
    if (!res)
        path_build(buf, max, ANGBAND_DIR_USER, ftmp);
    else if (res == 1)
        Term_event_push(&ea);

    screen_load(false);

    return (res == 0);
}


/*
 * Interactively dump preferences to a file.
 *
 * - Title must have the form "Dump <pref-type>"
 * - dump(ang_file *) needs to emit only the raw data for the dump.
 *   Comments are generated automatically
 */
static void dump_pref_file(void (*dump)(ang_file*), const char *title, int row)
{
    char buf[MSG_LEN];

    /* Get filename from user */
    if (!get_pref_path(title, row, buf, sizeof(buf))) return;

    /* Try to save */
    if (prefs_save(buf, dump, title))
        c_msg_print(format("Saved %s.", strstr(title, " ") + 1));
    else
        c_msg_print(format("Failed to save %s.", strstr(title, " ") + 1));

    c_msg_print(NULL);
}


static void do_cmd_pref_file_hack(long row);


/*
 * Options display and setting
 */


/*
 * Displays an option entry
 */
static void option_toggle_display(struct menu *m, int oid, bool cursor, int row, int col, int width)
{
    byte attr = curs_attrs[CURS_KNOWN][cursor != 0];
    bool *options = menu_priv(m);

    c_prt(attr, format("%-45s: %s  (%s)", option_desc(oid),
        options[oid] ? "yes" : "no ", option_name(oid)), row, col);
}


/*
 * Handle keypresses for an option entry
 */
static bool option_toggle_handle(struct menu *m, const ui_event *event, int oid)
{
    bool next = false;

    if (event->type == EVT_SELECT)
    {
        /* Hack -- birth options can not be toggled after birth */
        if (!(m->flags == MN_NO_TAGS))
            option_set(player->opts.opt, option_name(oid), !player->opts.opt[oid]);
    }
    else if (event->type == EVT_KBRD)
    {
        if ((event->key.code == 'y') || (event->key.code == 'Y'))
        {
            option_set(player->opts.opt, option_name(oid), true);
            next = true;
        }
        else if ((event->key.code == 'n') || (event->key.code == 'N'))
        {
            option_set(player->opts.opt, option_name(oid), false);
            next = true;
        }
        else if (event->key.code == 't' || event->key.code == 'T')
        {
            /* Hack -- birth options can not be toggled after birth */
            if (!(m->flags == MN_NO_TAGS))
                option_set(player->opts.opt, option_name(oid), !player->opts.opt[oid]);
        }
        else if (event->key.code == '?')
        {
            /*
                screen_save();
                show_file(format("option.txt#%s", option_name(oid)), NULL, 0, 0);
                screen_load();
            */
            plog("Not implemented... yet.");
        }
        else
            return false;
    }
    else
        return false;

    if (next)
    {
        m->cursor++;
        m->cursor = (m->cursor + m->filter_count) % m->filter_count;
    }

    return true;
}


/*
 * Toggle option menu display and handling functions
 */
static const menu_iter option_toggle_iter =
{
    NULL,
    NULL,
    option_toggle_display,
    option_toggle_handle,
    NULL
};


/*
 * Interact with some options
 */
static void option_toggle_menu(const char *name, int page)
{
    int i;
    struct menu *m = menu_new(MN_SKIN_SCROLL, &option_toggle_iter);
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* For all menus */
    m->prompt = "Set option (y/n/t), '?' for information";
    m->cmd_keys = "?YyNnTt";
    m->selections = "abcdefghijklmopqrsuvwxz";
    m->flags = MN_DBL_TAP;

    /* We add 10 onto the page amount to indicate we're at birth */
    if (page == OP_BIRTH)
    {
        m->prompt = "You can only modify these options at character birth. '?' for information";
        m->cmd_keys = "?";
        m->flags = MN_NO_TAGS;
    }
    else if (page == OP_BIRTH + 10)
        page -= 10;

    /* For this particular menu */
    m->title = name;

    /* Find the number of valid entries */
    for (i = 0; i < OPT_PAGE_PER; i++)
    {
        if (option_page[page][i] == OPT_none) break;
    }

    /* Set the data to the player's options */
    menu_setpriv(m, OPT_MAX, &player->opts.opt);
    menu_set_filter(m, option_page[page], i);
    menu_layout(m, &SCREEN_REGION);

    /* Run the menu */
    screen_save();

    clear_from(0);
    ke = menu_select(m, 0, false);
    if (is_abort(ke)) Term_event_push(&ea);

    screen_load(false);

    mem_free(m);
}


/*
 * Edit birth options.
 */
void do_cmd_options_birth(void)
{
    option_toggle_menu("Birth options", OP_BIRTH + 10);
}


/*
 * Modify the "window" options
 */
static void do_cmd_options_win(const char *name, int row)
{
    int i, j, d;
    int y = 0;
    int x = 0;
    ui_event ke;
    u32b new_flags[ANGBAND_TERM_MAX];
    ui_event ea = EVENT_ABORT;

    /* Set new flags to the old values */
    for (j = 0; j < ANGBAND_TERM_MAX; j++)
        new_flags[j] = window_flag[j];

    /* Clear screen */
    screen_save();
    clear_from(0);

    /* Interact */
    while (1)
    {
        /* Prompt */
        prt("Window flags (<dir> to move, 't'/Enter to toggle, or ESC)", 0, 0);

        /* Display the windows */
        for (j = 0; j < ANGBAND_TERM_MAX; j++)
        {
            byte a = COLOUR_WHITE;
            const char *s = angband_term_name[j];

            /* Use color */
            if (j == x) a = COLOUR_L_BLUE;

            /* Window name, staggered, centered */
            Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
        }

        /* Display the options */
        for (i = 0; i < PW_MAX_FLAGS; i++)
        {
            byte a = COLOUR_WHITE;
            const char *str = window_flag_desc[i];

            /* Use color */
            if (i == y) a = COLOUR_L_BLUE;

            /* Unused option */
            if (!str) str = "(Unused option)";

            /* Flag name */
            Term_putstr(0, i + 5, -1, a, str);

            /* Display the windows */
            for (j = 0; j < ANGBAND_TERM_MAX; j++)
            {
                char c = '.';

                a = COLOUR_WHITE;

                /* Use color */
                if ((i == y) && (j == x)) a = COLOUR_L_BLUE;

                /* Active flag */
                if (new_flags[j] & (1L << i)) c = 'X';

                /* Flag value */
                Term_putch(35 + j * 5, i + 5, a, c);
            }
        }

        /* Place Cursor */
        Term_gotoxy(35 + x * 5, y + 5);

        /* Get key */
        ke = inkey_ex();

        /* Exit */
        if (is_exit(ke)) break;

        /* Keyboard interaction */
        else if (ke.type == EVT_KBRD)
        {
            /* Allow escape */
            if (ke.key.code == 'q') break;

            /* Toggle */
            else if ((ke.key.code == '5') || (ke.key.code == 't') || (ke.key.code == KC_ENTER))
            {
                /* Can never toggle main and chat windows */
                if ((x == 0) || (x == PMSG_TERM))
                    bell("Cannot toggle window flags for this term!");

                /* Can never toggle PW_MESSAGE_CHAT */
                else if ((1L << y) == PW_MESSAGE_CHAT)
                    bell("Cannot toggle chat messages flag!");

                /* Toggle flag (off) */
                else if (new_flags[x] & (1L << y)) new_flags[x] &= ~(1L << y);

                /* Toggle flag (on) */
                else new_flags[x] = (1L << y);

                /* Continue */
                continue;
            }

            /* Extract direction */
            d = target_dir(ke.key);

            /* Move */
            if (d != 0)
            {
                x = (x + ddx[d] + ANGBAND_TERM_MAX) % ANGBAND_TERM_MAX;
                y = (y + ddy[d] + PW_MAX_FLAGS) % PW_MAX_FLAGS;
            }
        }
    }

    if (is_abort(ke)) Term_event_push(&ea);

    /* Notice changes */
    subwindows_set_flags(new_flags, ANGBAND_TERM_MAX);

    screen_load(false);
}


/*
 * Interact with keymaps
 */


/*
 * Current (or recent) keymap action
 */
static struct keypress keymap_buffer[KEYMAP_ACTION_MAX + 1];


/*
 * Ask for, and display, a keymap trigger.
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
static int keymap_get_trigger(struct keypress *c)
{
    char tmp[MSG_LEN];
    struct keypress buf[2];
    ui_event ke;

    memset(buf, 0, 2 * sizeof(struct keypress));

    /* Flush */
    event_signal(EVENT_INPUT_FLUSH);

    /* Get a key */
    ke = inkey_ex();
    return_on_abort(ke);
    buf[0] = ke.key;

    /* Convert to ascii */
    keypress_to_text(tmp, sizeof(tmp), buf, false);

    /* Hack -- display the trigger */
    Term_addstr(-1, COLOUR_WHITE, tmp);

    /* Flush */
    event_signal(EVENT_INPUT_FLUSH);

    /* Return trigger */
    *c = ke.key;
    return 0;
}


/*
 * Keymap menu action functions
 */


static void ui_keymap_pref_load(const char *title, int row)
{
    do_cmd_pref_file_hack(16);
}


static void ui_keymap_pref_append(const char *title, int row)
{
    dump_pref_file(keymap_dump, "Dump keymaps", 13);
}


static void ui_keymap_query(const char *title, int row)
{
    char tmp[MSG_LEN];
    int mode = (OPT(player, rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    struct keypress c;
    const struct keypress *act;
    int res;
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* Prompt */
    prt(title, 13, 0);
    prt("Key: ", 14, 0);

    /* Get a keymap trigger & mapping */
    res = keymap_get_trigger(&c);
    if (res)
    {
        if (res == 1) Term_event_push(&ea);
        return;
    }
    act = keymap_find(mode, c);

    /* Keymap found? */
    if (!act)
    {
        /* Prompt */
        prt("No keymap with that trigger.  Press any key to continue.", 16, 0);
    }
    else
    {
        /* Analyze the current action */
        keypress_to_text(tmp, sizeof(tmp), act, false);

        /* Display the current action */
        prt("Found: ", 15, 0);
        Term_addstr(-1, COLOUR_WHITE, tmp);

        /* Prompt */
        prt("Press any key to continue.", 17, 0);
    }

    ke = inkey_ex();
    if (is_abort(ke)) Term_event_push(&ea);
}


static void ui_keymap_create(const char *title, int row)
{
    bool done = false;
    size_t n = 0;
    struct keypress c, esc;
    char tmp[MSG_LEN];
    int mode = (OPT(player, rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    int res;
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* Prompt */
    prt(title, 13, 0);
    prt("Key: ", 14, 0);

    /* Get a keymap trigger */
    res = keymap_get_trigger(&c);
    if (res)
    {
        if (res == 1) Term_event_push(&ea);
        return;
    }

    if (c.code == '$')
    {
        c_prt(COLOUR_L_RED, "The '$' key is reserved.", 16, 2);
        prt("Press any key to continue.", 18, 0);
        ke = inkey_ex();
        if (is_abort(ke)) Term_event_push(&ea);
        return;
    }

    memset(keymap_buffer, 0, (KEYMAP_ACTION_MAX + 1) * sizeof(struct keypress));

    /* PWMAngband: always start with ESCAPE */
    esc.type = EVT_KBRD;
    esc.code = ESCAPE;
    esc.mods = 0;
    keymap_buffer[n++] = esc;

    /* Get an encoded action, with a default response */
    while (!done)
    {
        struct keypress kp = {EVT_NONE, 0, 0};
        int color = COLOUR_WHITE;

        if (n == 0) color = COLOUR_YELLOW;
        if (n == KEYMAP_ACTION_MAX) color = COLOUR_L_RED;

        keypress_to_text(tmp, sizeof(tmp), keymap_buffer, false);
        c_prt(color, format("Action: %s", tmp), 15, 0);

        c_prt(COLOUR_L_BLUE, "  Press '$' when finished.", 17, 0);
        c_prt(COLOUR_L_BLUE, "  Use 'CTRL-U' to reset.", 18, 0);
        c_prt(COLOUR_L_BLUE, format("(Maximum keymap length is %d keys.)", KEYMAP_ACTION_MAX), 19, 0);

        ke = inkey_ex();
        kp = ke.key;

        if (is_abort(ke))
        {
            Term_event_push(&ea);
            return;
        }

        if (ke.type != EVT_KBRD) continue;

        if (kp.code == '$')
        {
            done = true;
            continue;
        }

        switch (kp.code)
        {
            case KC_DELETE:
            case KC_BACKSPACE:
            {
                if (n > 0)
                {
                    n -= 1;
                    keymap_buffer[n].type = 0;
                    keymap_buffer[n].code = 0;
                    keymap_buffer[n].mods = 0;
                }
                break;
            }
            case KTRL('U'):
            {
                memset(keymap_buffer, 0, (KEYMAP_ACTION_MAX + 1) * sizeof(struct keypress));
                n = 0;
                break;
            }
            default:
            {
                if (n == KEYMAP_ACTION_MAX) continue;

                keymap_buffer[n++] = kp;
                break;
            }
        }
    }

    if (c.code)
    {
        res = get_check_ex("Save this keymap? ");
        if (res)
        {
            if (res == 1) Term_event_push(&ea);
            return;
        }
        keymap_add(mode, c, keymap_buffer, true);
        prt("Keymap added.  Press any key to continue.", 17, 0);
        ke = inkey_ex();
        if (is_abort(ke)) Term_event_push(&ea);
    }
}


static void ui_keymap_remove(const char *title, int row)
{
    struct keypress c;
    int mode = (OPT(player, rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    int res;
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* Prompt */
    prt(title, 13, 0);
    prt("Key: ", 14, 0);

    /* Get a keymap trigger */
    res = keymap_get_trigger(&c);
    if (res)
    {
        if (res == 1) Term_event_push(&ea);
        return;
    }

    if (keymap_remove(mode, c))
        prt("Removed.", 16, 0);
    else
        prt("No keymap to remove!", 16, 0);

    /* Prompt */
    prt("Press any key to continue.", 17, 0);
    ke = inkey_ex();
    if (is_abort(ke)) Term_event_push(&ea);
}


/*
 * Display keymaps as a list and allow user to navigate through it.
 */
static void ui_keymap_browse(const char *title, int row)
{
    int total;
    int o = 0;
    int hgt = Term->max_hgt - 4;
    int j = 0;
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    screen_save();

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 0, -1, COLOUR_WHITE, "Browse Macros");

        /* Dump them */
        total = keymap_browse(o, &j);

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        if (is_exit(ke)) break;

        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                /* Down */
                case ARROW_DOWN:
                case '2':
                case KC_ENTER:
                {
                    j++;
                    if (j > total - 1) j = total - 1;
                    else if ((j - o > hgt / 2) && (j < total)) o++;
                    break;
                }

                /* Home */
                case KC_HOME:
                case '7': o = j = 0; break;

                /* Page up */
                case KC_PGUP:
                case '9':
                case '-':
                {
                    j -= hgt;
                    if (j < 0) j = 0;
                    o = j;
                    break;
                }

                /* Page down */
                case KC_PGDOWN:
                case '3':
                case ' ':
                {
                    j += Term->max_hgt;
                    if (j > total - 1) j = total - 1;
                    o = j - hgt / 2;
                    break;
                }

                /* End */
                case KC_END:
                case '1':
                {
                    j = total - 1;
                    o = j - hgt / 2;
                    break;
                }

                /* Up */
                case ARROW_UP:
                case '8':
                {
                    j--;
                    if (j < 0) j = 0;
                    else if (o && (j - o < hgt / 2)) o--;
                    break;
                }
            }
        }
    }

    if (is_abort(ke)) Term_event_push(&ea);

    screen_load(false);
}


static void keymap_browse_hook(int oid, void *db, const region *loc)
{
    char tmp[MSG_LEN];

    clear_from(13);

    /* Show current action */
    prt("Current action (if any) shown below:", 13, 0);
    keypress_to_text(tmp, sizeof(tmp), keymap_buffer, false);
    prt(tmp, 14, 0);
}


static struct menu *keymap_menu;
static menu_action keymap_actions[] =
{
    {0, 0, "Load a user pref file", ui_keymap_pref_load},
    {0, 0, "Save keymaps to file", ui_keymap_pref_append},
    {0, 0, "Query a keymap", ui_keymap_query},
    {0, 0, "Create a keymap", ui_keymap_create},
    {0, 0, "Remove a keymap", ui_keymap_remove},
    {0, 0, "Browse keymaps", ui_keymap_browse}
};


/*
 * Interact with "keymaps"
 */
static void do_cmd_keymaps(const char *title, int row)
{
    region loc = {0, 0, 0, 12};
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    screen_save();
    clear_from(0);

    if (!keymap_menu)
    {
        keymap_menu = menu_new_action(keymap_actions, N_ELEMENTS(keymap_actions));

        keymap_menu->title = "Interact with keymaps";
        keymap_menu->selections = lower_case;
        keymap_menu->browse_hook = keymap_browse_hook;
    }

    menu_layout(keymap_menu, &loc);
    ke = menu_select(keymap_menu, 0, false);
    if (title && is_abort(ke)) Term_event_push(&ea);

    screen_load(true);
}


/*
 * Interact with colours
 */


static void colors_pref_load(const char *title, int row)
{
    /* Ask for and load a user pref file */
    do_cmd_pref_file_hack(8);

    /* Should probably be a cleaner way to tell UI about colour changes */
    Term_xtra(TERM_XTRA_REACT, use_graphics);
    Term_redraw();
}


static void colors_pref_dump(const char *title, int row)
{
    dump_pref_file(dump_colors, title, 15);
}


static void colors_modify(const char *title, int row)
{
    int i;
    ui_event ke;
    static byte a = 0;
    ui_event ea = EVENT_ABORT;

    /* Prompt */
    prt("Command: Modify colors", 8, 0);

    /* Hack -- query until done */
    while (1)
    {
        const char *name;
        char index;

        /* Clear */
        clear_from(10);

        /* Exhibit the normal colors */
        for (i = 0; i < BASIC_COLORS; i++)
        {
            int pos = ((i < 10)? i * 2: i * 3 - 9);

            /* Exhibit this color */
            Term_putstr(pos, 20, -1, a, ((i < 10)? " #": "##"));

            /* Exhibit character letter */
            Term_putstr(pos, 21, -1, (byte)i, format(" %c", color_table[i].index_char));

            /* Exhibit all colors */
            Term_putstr(pos, 22, -1, (byte)i, format("%2d", i));
        }

        /* Describe the color */
        name = ((a < BASIC_COLORS)? color_table[a].name: "undefined");
        index = ((a < BASIC_COLORS)? color_table[a].index_char: '?');

        /* Describe the color */
        Term_putstr(5, 10, -1, COLOUR_WHITE,
            format("Color = %d, Name = %s, Index = %c", a, name, index));

        /* Label the Current values */
        Term_putstr(5, 12, -1, COLOUR_WHITE, format("K = 0x%02x / R,G,B = 0x%02x,0x%02x,0x%02x",
            angband_color_table[a][0], angband_color_table[a][1], angband_color_table[a][2],
            angband_color_table[a][3]));

        /* Prompt */
        Term_putstr(0, 14, -1, COLOUR_WHITE, "Command (n/N/k/K/r/R/g/G/b/B): ");

        /* Get a command */
        ke = inkey_ex();

        /* All done */
        if (is_exit(ke)) break;

        /* Analyze */
        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                case 'n': a = (byte)(a + 1); break;
                case 'N': a = (byte)(a - 1); break;
                case 'k':
                    angband_color_table[a][0] = (byte)(angband_color_table[a][0] + 1);
                    break;
                case 'K':
                    angband_color_table[a][0] = (byte)(angband_color_table[a][0] - 1);
                    break;
                case 'r':
                    angband_color_table[a][1] = (byte)(angband_color_table[a][1] + 1);
                    break;
                case 'R':
                    angband_color_table[a][1] = (byte)(angband_color_table[a][1] - 1);
                    break;
                case 'g':
                    angband_color_table[a][2] = (byte)(angband_color_table[a][2] + 1);
                    break;
                case 'G':
                    angband_color_table[a][2] = (byte)(angband_color_table[a][2] - 1);
                    break;
                case 'b':
                    angband_color_table[a][3] = (byte)(angband_color_table[a][3] + 1);
                    break;
                case 'B':
                    angband_color_table[a][3] = (byte)(angband_color_table[a][3] - 1);
                    break;
            }
        }

        /* Hack -- react to changes */
        Term_xtra(TERM_XTRA_REACT, use_graphics);

        /* Hack -- redraw */
        Term_redraw();
    }

    if (is_abort(ke)) Term_event_push(&ea);
}


static void colors_browse_hook(int oid, void *db, const region *loc)
{
    clear_from(1);
}


static struct menu *color_menu;


static menu_action color_events[] =
{
    {0, 0, "Load a user pref file", colors_pref_load},
    {0, 0, "Dump colors", colors_pref_dump},
    {0, 0, "Modify colors", colors_modify}
};


/*
 * Interact with "colors"
 */
static void do_cmd_colors(const char *title, int row)
{
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    screen_save();
    clear_from(0);

    if (!color_menu)
    {
        color_menu = menu_new_action(color_events, N_ELEMENTS(color_events));

        color_menu->title = title;
        color_menu->selections = lower_case;
        color_menu->browse_hook = colors_browse_hook;
    }

    menu_layout(color_menu, &SCREEN_REGION);
    ke = menu_select(color_menu, 0, false);
    if (title && is_abort(ke)) Term_event_push(&ea);

    screen_load(true);
}


/*
 * Non-complex menu actions
 */


static bool askfor_aux_numbers(char *buf, size_t buflen, size_t *curs,
    size_t *len, struct keypress keypress, bool firsttime)
{
    switch (keypress.code)
    {
        case ESCAPE:
        case KC_ENTER:
        case ARROW_LEFT:
        case ARROW_RIGHT:
        case KC_BACKSPACE:
        case KC_DELETE:
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            return askfor_aux_keypress(buf, buflen, curs, len, keypress, firsttime);
    }

    return false;
}


/*
 * Set base delay factor
 */
static void do_cmd_delay(const char *name, int row)
{
    int res;
    char tmp[4] = "";
    ui_event ea = EVENT_ABORT;

    strnfmt(tmp, sizeof(tmp), "%i", player->opts.delay_factor);

    screen_save();

    /* Prompt */
    prt("Command: Base Delay Factor", 20, 0);

    prt(format("Current base delay factor: %d msec", player->opts.delay_factor), 22, 0);
    prt("New base delay factor (0-255): ", 21, 0);

    /* Ask for a numeric value */
    res = askfor_ex(tmp, sizeof(tmp), askfor_aux_numbers, false);
    if (!res)
    {
        u16b val = (u16b)strtoul(tmp, NULL, 0);

        player->opts.delay_factor = MIN(val, 255);
    }
    else if (res == 1)
        Term_event_push(&ea);

    screen_load(false);
}


/*
 * Set hitpoint warning level
 */
static void do_cmd_hp_warn(const char *name, int row)
{
    int res;
    char tmp[2] = "";
    ui_event ea = EVENT_ABORT;

    strnfmt(tmp, sizeof(tmp), "%i", player->opts.hitpoint_warn);

    screen_save();

    /* Prompt */
    prt("Command: Hitpoint Warning", 20, 0);

    prt(format("Current hitpoint warning: %d (%d%%)",
        player->opts.hitpoint_warn, player->opts.hitpoint_warn * 10), 22, 0);
    prt("New hitpoint warning (0-9): ", 21, 0);

    /* Ask the user for a string */
    res = askfor_ex(tmp, sizeof(tmp), askfor_aux_numbers, false);

    /* Process input */
    if (!res) player->opts.hitpoint_warn = (byte)strtoul(tmp, NULL, 0);
    else if (res == 1) Term_event_push(&ea);

    screen_load(false);
}


/*
 * Set "lazy-movement" delay
 */
static void do_cmd_lazymove_delay(const char *name, int row)
{
    int res;
    char tmp[2] = "";
    ui_event ea = EVENT_ABORT;

    strnfmt(tmp, sizeof(tmp), "%i", player->opts.lazymove_delay);

    screen_save();

    /* Prompt */
    prt("Command: Movement Delay Factor", 20, 0);

    prt(format("Current movement delay factor: %d (%d msec)",
        player->opts.lazymove_delay, player->opts.lazymove_delay * 100), 22, 0);
    prt("New movement delay factor (0-9): ", 21, 0);

    /* Ask the user for a string */
    res = askfor_ex(tmp, sizeof(tmp), askfor_aux_numbers, false);

    /* Process input */
    if (!res) player->opts.lazymove_delay = (byte)strtoul(tmp, NULL, 0);
    else if (res == 1) Term_event_push(&ea);

    screen_load(false);
}


/*
 * Ask for a "user pref file" and process it.
 *
 * This function should only be used by standard interaction commands,
 * in which a standard "Command:" prompt is present on the given row.
 *
 * Allow absolute file names?  XXX XXX XXX
 */
static void do_cmd_pref_file_hack(long row)
{
    char ftmp[NORMAL_WID];
    int res;
    ui_event ea = EVENT_ABORT;

    screen_save();

    /* Prompt */
    prt("Command: Load a user pref file", row, 0);

    /* Prompt */
    prt("File: ", row + 2, 0);

    /* Default filename */
    strnfmt(ftmp, sizeof(ftmp), "%s.prf", strip_suffix(nick));

    /* Ask for a file (or cancel) */
    res = askfor_ex(ftmp, sizeof(ftmp), NULL, false);
    if (!res)
    {
        /* Process the given filename */
        if (!process_pref_file(ftmp, false, true))
        {
            /* Mention failure */
            c_msg_print(format("Failed to load '%s'!", ftmp));
        }
        else
        {
            /* Mention success */
            c_msg_print(format("Loaded '%s'.", ftmp));
        }
    }
    else if (res == 1) Term_event_push(&ea);

    screen_load(false);
}


/*
 * Write options to a file.
 */
static void do_dump_options(const char *title, int row)
{
    char path[MSG_LEN];

    /* Save the window prefs */
    path_build(path, sizeof(path), ANGBAND_DIR_USER, "window.prf");
    if (!prefs_save(path, window_dump, "Dump window settings"))
        c_msg_print("Failed to save subwindow preferences.");

    dump_pref_file(option_dump, "Dump options", 20);
}


/*
 * Write autoinscriptions to a file.
 */
static void do_dump_autoinsc(const char *title, int row)
{
    dump_pref_file(dump_autoinscriptions, "Dump autoinscriptions", 20);
}


/*
 * Load a pref file.
 */
static void options_load_pref_file(const char *n, int row)
{
    do_cmd_pref_file_hack(20);

    /* Notice changes */
    subwindows_reinit_flags();
    subwindows_init_flags();
}


/*
 * Quality ignore menu
 */


/*
 * Menu struct for differentiating aware from unaware ignoring
 */
typedef struct
{
    struct object_kind *kind;
    bool aware;
} ignore_choice;


/*
 * Ordering function for ignore choices.
 */
static int cmp_ignore(const void *a, const void *b)
{
    char bufa[NORMAL_WID];
    char bufb[NORMAL_WID];
    const ignore_choice *x = a;
    const ignore_choice *y = b;

    /* XXX: would require kind->flavor and kind->flavor->text if not aware */

    object_kind_name(bufa, sizeof(bufa), x->kind, x->aware);
    object_kind_name(bufb, sizeof(bufb), y->kind, y->aware);

    return strcmp(bufa, bufb);
}


/*
 * Used for mapping the values below to names.
 */
typedef struct
{
    unsigned int enum_val;
    const char *name;
} quality_name_struct;


static quality_name_struct quality_choices[] =
{
    #define ITYPE(a, b) {ITYPE_##a, b},
    #include "../common/list-ignore-types.h"
    #undef ITYPE
    {ITYPE_MAX, ""}
};


/*
 * The names for the various kinds of quality
 */
static quality_name_struct quality_values[IGNORE_MAX] =
{
    {IGNORE_NONE, "no ignore"},
    {IGNORE_BAD, "worthless"},
    {IGNORE_AVERAGE, "average"},
    {IGNORE_GOOD, "good"},
    {IGNORE_ALL, "non-artifact"}
};


/*
 * Display an entry in the menu.
 */
static void quality_display(struct menu *menu, int oid, bool cursor, int row, int col, int width)
{
    /* Choice is [1..ITYPE_MAX], level is [0..ITYPE_MAX] */
    const char *name = quality_choices[oid].name;
    byte level = player->opts.ignore_lvl[oid + 1];
    const char *level_name = quality_values[level].name;
    byte attr = (cursor? COLOUR_L_BLUE: COLOUR_WHITE);

    c_put_str(attr, format("%-30s : %s", name, level_name), row, col);
}


/*
 * Display the quality ignoring subtypes.
 */
static void quality_subdisplay(struct menu *menu, int oid, bool cursor, int row, int col, int width)
{
    const char *name = quality_values[oid].name;
    byte attr = (cursor? COLOUR_L_BLUE: COLOUR_WHITE);

    c_put_str(attr, name, row, col);
}


/*
 * Handle keypresses.
 */
static bool quality_action(struct menu *m, const ui_event *event, int oid)
{
    struct menu menu;
    menu_iter menu_f = {NULL, NULL, quality_subdisplay, NULL, NULL};
    region area = {37, 2, 29, IGNORE_MAX};
    ui_event evt;
    int count;
    ui_event ea = EVENT_ABORT;

    /* Display at the right point */
    area.row += oid;

    /* Save */
    screen_save();

    /* Work out how many options we have */
    count = IGNORE_MAX;
    /*if ((oid == ITYPE_RING) || (oid == ITYPE_AMULET))
        count = area.page_rows = IGNORE_BAD + 1;*/

    /* Run menu */
    menu_init(&menu, MN_SKIN_SCROLL, &menu_f);
    menu_setpriv(&menu, count, quality_values);

    /* Stop menus from going off the bottom of the screen */
    if (area.row + menu.count > Term->hgt - 1)
        area.row += Term->hgt - 1 - area.row - menu.count;

    menu_layout(&menu, &area);

    window_make(area.col - 2, area.row - 1, area.col + area.width + 2, area.row + area.page_rows);

    evt = menu_select(&menu, 0, true);
    if (is_abort(evt)) Term_event_push(&ea);

    /* Set the new value appropriately */
    if (evt.type == EVT_SELECT) player->opts.ignore_lvl[oid + 1] = menu.cursor;

    /* Load and finish */
    screen_load(false);
    return true;
}


/*
 * Display quality ignoring menu.
 */
static void quality_menu(void)
{
    struct menu menu;
    menu_iter menu_f = {NULL, NULL, quality_display, quality_action, NULL};
    region area = {0, 0, 0, 0};
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* Save screen */
    screen_save();
    clear_from(0);

    /* Set up the menu */
    menu_init(&menu, MN_SKIN_SCROLL, &menu_f);
    menu.title = "Quality ignoring menu";
    menu_setpriv(&menu, ITYPE_MAX - 1, quality_values);
    menu_layout(&menu, &area);

    /* Select an entry */
    ke = menu_select(&menu, 0, false);
    if (is_abort(ke)) Term_event_push(&ea);

    /* Load screen */
    screen_load(false);
}


/*
 * Ego item ignore menu
 */


/*
 * Skip common prefixes in ego-item names.
 */
static const char *strip_ego_name(const char *name)
{
    if (prefix(name, "of the ")) return name + 7;
    if (prefix(name, "of ")) return name + 3;
    return name;
}


/*
 * Display an ego-item type on the screen.
 */
int ego_item_name(char *buf, size_t buf_size, struct ego_desc *desc)
{
    size_t i;
    int end;
    size_t prefix_size;
    const char *long_name;
    struct ego_item *ego = &e_info[desc->e_idx];

    /* Find the ignore type */
    for (i = ITYPE_NONE; i < ITYPE_MAX; i++)
    {
        if ((size_t)desc->itype == i) break;
    }

    if ((i == ITYPE_NONE) || (i == ITYPE_MAX)) return 0;

    /* Initialize the buffer */
    end = my_strcat(buf, "[ ] ", buf_size);

    /* Append the name (choice is [1..ITYPE_MAX]) */
    end += my_strcat(buf, quality_choices[i - 1].name, buf_size);

    /* Append an extra space */
    end += my_strcat(buf, " ", buf_size);

    /* Get the full ego-item name */
    long_name = ego->name;

    /* Get the length of the common prefix, if any */
    prefix_size = (desc->short_name - long_name);

    /* Found a prefix? */
    if (prefix_size > 0)
    {
        char prefix[100];

        /* Get a copy of the prefix */
        my_strcpy(prefix, long_name, prefix_size + 1);

        /* Append the prefix */
        end += my_strcat(buf, prefix, buf_size);
    }

    /* Set the name to the right length */
    return end;
}


/*
 * Utility function used for sorting an array of ego-item indices by
 * ego-item name.
 */
static int ego_comp_func(const void *a_ptr, const void *b_ptr)
{
    const struct ego_desc *a = a_ptr;
    const struct ego_desc *b = b_ptr;

    /* Note the removal of common prefixes */
    return (strcmp(a->short_name, b->short_name));
}


/*
 * Display an entry on the ego menu
 */
static void ego_display(struct menu *menu, int oid, bool cursor, int row, int col, int width)
{
    char buf[NORMAL_WID] = "";
    struct ego_desc *choice = (struct ego_desc *)menu->menu_data;
    bool ignored = player->ego_ignore_types[choice[oid].e_idx][choice[oid].itype];
    byte attr = (cursor? COLOUR_L_BLUE: COLOUR_WHITE);
    byte sq_attr = (ignored? COLOUR_L_RED: COLOUR_L_GREEN);

    /* Acquire the "name" of object "i" */
    ego_item_name(buf, sizeof(buf), &choice[oid]);

    /* Print it */
    c_put_str(attr, format("%s", buf), row, col);

    /* Show ignore mark, if any */
    if (ignored)
        c_put_str(COLOUR_L_RED, "*", row, col + 1);

    /* Show the stripped ego-item name using another colour */
    c_put_str(sq_attr, choice[oid].short_name, row, col + strlen(buf));
}


/*
 * Deal with events on the ego menu
 */
static bool ego_action(struct menu *menu, const ui_event *event, int oid)
{
    struct ego_desc *choice = menu->menu_data;

    /* Toggle */
    if (event->type == EVT_SELECT)
    {
        player->ego_ignore_types[choice[oid].e_idx][choice[oid].itype] =
            !player->ego_ignore_types[choice[oid].e_idx][choice[oid].itype];

        return true;
    }

    return false;
}


static void text_out_c(byte attr, const char *text, int y, int* px)
{
    int x;

    /* Check line break */
    x = (*px) + strlen(text);
    if (x > NORMAL_WID - 2) return;

    /* Display text */
    c_put_str(attr, text, y, *px);

    /* Advance */
    (*px) += strlen(text);
}


/*
 * Display list of ego items to be ignored.
 */
static void ego_menu(void)
{
    int max_num = 0;
    struct ego_item *ego;
    struct ego_desc *choice;
    struct menu menu;
    menu_iter menu_f = {NULL, NULL, ego_display, ego_action, NULL};
    region area = {1, 5, -1, -1};
    int i, x;
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* Create the array */
    choice = mem_zalloc(z_info->e_max * ITYPE_MAX * sizeof(struct ego_desc));

    /* Get the valid ego-items */
    for (i = 0; i < z_info->e_max; i++)
    {
        int itype;

        ego = &e_info[i];

        /* Only valid known ego-items allowed */
        if (!ego->name || !player->ego_everseen[i])
            continue;

        /* Find appropriate ignore types */
        for (itype = ITYPE_NONE; itype < ITYPE_MAX; itype++)
        {
            if (ego_has_ignore_type(ego, itype))
            {
                /* Fill in the details */
                choice[max_num].e_idx = i;
                choice[max_num].itype = itype;
                choice[max_num].short_name = strip_ego_name(ego->name);

                ++max_num;
            }
        }
    }

    /* Quickly sort the array by ego-item name */
    sort(choice, max_num, sizeof(choice[0]), ego_comp_func);

    /* Return here if there are no objects */
    if (!max_num)
    {
        mem_free(choice);
        return;
    }

    /* Save the screen and clear it */
    screen_save();
    clear_from(0);

    /* Help text */
    prt("Ego item ignoring menu", 0, 0);

    /* Display some helpful information */
    x = 1;
    text_out_c(COLOUR_L_GREEN, "Movement keys", 1, &x);
    text_out_c(COLOUR_WHITE, " scroll the list", 1, &x);
    x = 1;
    text_out_c(COLOUR_L_RED, "ESC", 2, &x);
    text_out_c(COLOUR_WHITE, " returns to the previous menu", 2, &x);
    x = 1;
    text_out_c(COLOUR_L_BLUE, "Enter", 3, &x);
    text_out_c(COLOUR_WHITE, " toggles the current setting.", 3, &x);

    /* Set up the menu */
    menu_init(&menu, MN_SKIN_SCROLL, &menu_f);
    menu_setpriv(&menu, max_num, choice);
    menu_layout(&menu, &area);

    /* Select an entry */
    ke = menu_select(&menu, 0, false);
    if (is_abort(ke)) Term_event_push(&ea);

    /* Free memory */
    mem_free(choice);

    /* Load screen */
    screen_load(false);
}


/*
 * Sval ignore menu
 */


/*
 * Structure to describe tval/description pairings.
 */
typedef struct
{
    int tval;
    const char *desc;
} tval_desc;


/*
 * Categories for sval-dependent ignore.
 */
static tval_desc sval_dependent[] =
{
    {TV_STAFF, "Staves"},
    {TV_WAND, "Wands"},
    {TV_ROD, "Rods"},
    {TV_SCROLL, "Scrolls"},
    {TV_POTION, "Potions"},
    {TV_RING, "Rings"},
    {TV_AMULET, "Amulets"},
    {TV_FOOD, "Food"},
    {TV_MUSHROOM, "Mushrooms"},
    {TV_MAGIC_BOOK, "Magic Books"},
    {TV_PRAYER_BOOK, "Prayer Books"},
    {TV_NATURE_BOOK, "Nature Books"},
    {TV_SHADOW_BOOK, "Shadow Books"},
    {TV_PSI_BOOK, "Psi Books"},
    {TV_ELEM_BOOK, "Elemental Books"},
    {TV_FLASK, "Flasks of Oil"}
};


/*
 * Determines whether a MAngband tval is eligible for sval-ignoring.
 */
static bool ignore_tval_extra(int tval)
{
    /* PWMAngband: allow crops and junk to be ignored */
    switch (tval)
    {
        case TV_SKELETON:
        case TV_BOTTLE:
        case TV_CORPSE:
        case TV_CROP:
        case TV_COOKIE: return true;
    }

    return false;
}


/*
 * Determines whether a tval is eligible for sval-ignoring.
 */
bool ignore_tval(int tval)
{
    size_t i;

    /* Only ignore if the tval's allowed */
    for (i = 0; i < N_ELEMENTS(sval_dependent); i++)
    {
        if (tval == sval_dependent[i].tval)
            return true;
    }

    return ignore_tval_extra(tval);
}


/*
 * Display an entry on the sval menu
 */
static void ignore_sval_menu_display(struct menu *menu, int oid, bool cursor,
    int row, int col, int width)
{
    char buf[NORMAL_WID];
    const ignore_choice *choice = menu_priv(menu);
    struct object_kind *kind = choice[oid].kind;
    bool aware = choice[oid].aware;
    byte attr = curs_attrs[(int)aware][0 != cursor];

    /* Acquire the "name" of object "i" */
    object_kind_name(buf, sizeof(buf), kind, aware);

    /* Print it */
    c_put_str(attr, format("[ ] %s", buf), row, col);
    /*if ((aware && (kind->ignore & IGNORE_IF_AWARE)) || (!aware && (kind->ignore & IGNORE_IF_UNAWARE)))*/
    if (player->kind_ignore[kind->kidx])
        c_put_str(COLOUR_L_RED, "*", row, col + 1);
}


/*
 * Deal with events on the sval menu
 */
static bool ignore_sval_menu_action(struct menu *m, const ui_event *event, int oid)
{
    const ignore_choice *choice = menu_priv(m);

    if (event->type == EVT_SELECT ||
        (event->type == EVT_KBRD && tolower(event->key.code) == 't'))
    {
        struct object_kind *kind = choice[oid].kind;

        /* Toggle the appropriate flag */
        if (choice[oid].aware)
            /*kind->ignore ^= IGNORE_IF_AWARE;*/
            player->kind_ignore[kind->kidx] = !player->kind_ignore[kind->kidx];
        /*else
            kind->ignore ^= IGNORE_IF_UNAWARE;*/

        Send_ignore();
        return true;
    }

    return false;
}


static const menu_iter ignore_sval_menu =
{
    NULL,
    NULL,
    ignore_sval_menu_display,
    ignore_sval_menu_action,
    NULL
};


/*
 * Collect all tvals in the big ignore_choice array
 */
static int ignore_collect_kind(int tval, ignore_choice **ch)
{
    ignore_choice *choice;
    int num = 0;
    int i;

    /*choice = mem_alloc(2 * z_info->k_max * sizeof(*choice));*/
    choice = mem_alloc(z_info->k_max * sizeof(*choice));

    for (i = 0; i < z_info->k_max; i++)
    {
        struct object_kind *kind = &k_info[i];
        bool artifact;

        /* Skip empty objects, unseen objects, and incorrect tvals */
        if (!kind->name || kind->tval != tval) continue;

        /* Can unaware ignore anything */
        /*if (!kind->aware)
        {
            choice[num].kind = kind;
            choice[num++].aware = false;
        }*/

        /* Do not display the artifact base kinds in this list */
        artifact = (kf_has(kind->kind_flags, KF_INSTA_ART) || kf_has(kind->kind_flags, KF_QUEST_ART));
        if (player->obj_aware[kind->kidx] && player->kind_everseen[kind->kidx] && !artifact)
        {
            choice[num].kind = kind;
            choice[num++].aware = true;
        }
    }

    if (num == 0)
        mem_free(choice);
    else
        *ch = choice;

    return num;
}


/*
 * Display list of svals to be ignored
 */
static bool sval_menu(int tval, const char *desc)
{
    struct menu *menu;
    region area = {1, 2, -1, -1};
    ui_event ke;
    ui_event ea = EVENT_ABORT;
    ignore_choice *choices;
    int n_choices = ignore_collect_kind(tval, &choices);

    if (!n_choices) return false;

    /* Sort by name in ignore menus except for categories of items that are aware from the start */
    switch (tval)
    {
        /* Leave sorted by sval */
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_NATURE_BOOK:
        case TV_SHADOW_BOOK:
        case TV_PSI_BOOK:
        case TV_ELEM_BOOK: break;

        /* Sort by name */
        default: sort(choices, n_choices, sizeof(*choices), cmp_ignore);
    }

    /* Save the screen and clear it */
    screen_save();
    clear_from(0);

    /* Help text */
    prt(format("Ignore the following %s:", desc), 0, 0);

    /* Run menu */
    menu = menu_new(MN_SKIN_COLUMNS, &ignore_sval_menu);
    menu_setpriv(menu, n_choices, choices);
    menu->cmd_keys = "Tt";
    menu_layout(menu, &area);
    menu_set_cursor_x_offset(menu, 1); /* Place cursor in brackets. */
    ke = menu_select(menu, 0, false);
    if (is_abort(ke)) Term_event_push(&ea);

    /* Free memory */
    mem_free(choices);

    /* Load screen */
    screen_load(false);
    return true;
}


/*
 * Returns true if there's anything to display a menu of
 */
static bool seen_tval(int tval)
{
    int i;

    for (i = 0; i < z_info->k_max; i++)
    {
        struct object_kind *kind = &k_info[i];

        /* Skip empty objects, unseen objects, and incorrect tvals */
        if (!kind->name) continue;
        if (!player->kind_everseen[kind->kidx]) continue;
        if (kind->tval != tval) continue;

        return true;
    }

    return false;
}


/*
 * Collect all other MAngband tvals in the big ignore_choice array
 */
static int ignore_collect_kind_extra(ignore_choice **ch)
{
    ignore_choice *choice;
    int num = 0;
    int i;

    /*choice = mem_alloc(2 * z_info->k_max * sizeof(*choice));*/
    choice = mem_alloc(z_info->k_max * sizeof(*choice));

    for (i = 0; i < z_info->k_max; i++)
    {
        struct object_kind *kind = &k_info[i];
        bool artifact;

        /* Skip empty objects, unseen objects, and incorrect tvals */
        if (!kind->name || !ignore_tval_extra(kind->tval)) continue;

        /* Can unaware ignore anything */
        /*if (!kind->aware)
        {
            choice[num].kind = kind;
            choice[num++].aware = false;
        }*/

        /* Do not display the artifact base kinds in this list */
        artifact = (kf_has(kind->kind_flags, KF_INSTA_ART) || kf_has(kind->kind_flags, KF_QUEST_ART));
        if (player->obj_aware[kind->kidx] && player->kind_everseen[kind->kidx] && !artifact)
        {
            choice[num].kind = kind;
            choice[num++].aware = true;
        }
    }

    if (num == 0)
        mem_free(choice);
    else
        *ch = choice;

    return num;
}


/*
 * Display list of MAngband svals to be ignored
 */
static void sval_menu_extra(void)
{
    struct menu *menu;
    region area = {1, 2, -1, -1};
    ui_event ke;
    ui_event ea = EVENT_ABORT;
    ignore_choice *choices;
    int n_choices = ignore_collect_kind_extra(&choices);

    if (!n_choices) return;

    /* Sort by name */
    sort(choices, n_choices, sizeof(*choices), cmp_ignore);

    /* Save the screen and clear it */
    screen_save();
    clear_from(0);

    /* Help text */
    prt("Ignore the following:", 0, 0);

    /* Run menu */
    menu = menu_new(MN_SKIN_COLUMNS, &ignore_sval_menu);
    menu_setpriv(menu, n_choices, choices);
    menu->cmd_keys = "Tt";
    menu_layout(menu, &area);
    menu_set_cursor_x_offset(menu, 1); /* Place cursor in brackets. */
    ke = menu_select(menu, 0, false);
    if (is_abort(ke)) Term_event_push(&ea);

    /* Free memory */
    mem_free(choices);

    /* Load screen */
    screen_load(false);
}


/*
 * Extra options on the "item options" menu
 */
static struct
{
    char tag;
    const char *name;
    void (*action)(void);
} extra_item_options[] = {
    {'Q', "Quality ignoring options", quality_menu},
    {'E', "Ego ignoring options", ego_menu},
    {'T', "Other MAngband tval-ignoring options", sval_menu_extra}
};


static char tag_options_item(struct menu *menu, int oid)
{
    size_t line = (size_t)oid;

    if (line < N_ELEMENTS(sval_dependent))
        return I2A(oid);

    /* Separator - blank line. */
    if (line == N_ELEMENTS(sval_dependent))
        return 0;

    line = line - N_ELEMENTS(sval_dependent) - 1;

    if (line < N_ELEMENTS(extra_item_options))
        return extra_item_options[line].tag;

    return 0;
}


static int valid_options_item(struct menu *menu, int oid)
{
    size_t line = (size_t)oid;

    if (line < N_ELEMENTS(sval_dependent))
        return 1;

    /* Separator - blank line. */
    if (line == N_ELEMENTS(sval_dependent))
        return 0;

    line = line - N_ELEMENTS(sval_dependent) - 1;

    if (line < N_ELEMENTS(extra_item_options))
        return 1;

    return 0;
}


static void display_options_item(struct menu *menu, int oid, bool cursor, int row, int col, int width)
{
    size_t line = (size_t)oid;

    /* Most of the menu is svals, with a small "extra options" section below */
    if (line < N_ELEMENTS(sval_dependent))
    {
        bool known = seen_tval(sval_dependent[line].tval);
        byte attr = curs_attrs[known? CURS_KNOWN: CURS_UNKNOWN][(int)cursor];

        c_prt(attr, sval_dependent[line].desc, row, col);
    }
    else
    {
        byte attr = curs_attrs[CURS_KNOWN][(int)cursor];

        line = line - N_ELEMENTS(sval_dependent) - 1;

        if (line < N_ELEMENTS(extra_item_options))
            c_prt(attr, extra_item_options[line].name, row, col);
    }
}


static bool handle_options_item(struct menu *menu, const ui_event *event, int oid)
{
    if (event->type == EVT_SELECT)
    {
        if ((size_t)oid < N_ELEMENTS(sval_dependent))
            sval_menu(sval_dependent[oid].tval, sval_dependent[oid].desc);
        else
        {
            oid = oid - (int)N_ELEMENTS(sval_dependent) - 1;
            assert((size_t)oid < N_ELEMENTS(extra_item_options));
            extra_item_options[oid].action();
        }

        return true;
    }

    return false;
}


static const menu_iter options_item_iter =
{
    tag_options_item,
    valid_options_item,
    display_options_item,
    handle_options_item,
    NULL
};


/*
 * Display and handle the main ignoring menu.
 */
static void do_cmd_options_item(const char *title, int row)
{
    struct menu menu;
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    menu_init(&menu, MN_SKIN_SCROLL, &options_item_iter);
    menu_setpriv(&menu, N_ELEMENTS(sval_dependent) + N_ELEMENTS(extra_item_options) + 1, NULL);

    menu.title = title;
    menu_layout(&menu, &SCREEN_REGION);

    screen_save();
    clear_from(0);
    ke = menu_select(&menu, 0, false);
    if (is_abort(ke)) Term_event_push(&ea);
    screen_load(false);
}


/*
 * Main menu definitions and display
 */


static struct menu *option_menu;


static menu_action option_actions[] =
{
    {0, 'a', "User interface options", option_toggle_menu},
    {0, 'b', "MAngband options", option_toggle_menu},
    {0, 'c', "Birth (difficulty) options", option_toggle_menu},
    {0, 'w', "Subwindow setup", do_cmd_options_win},
    {0, 'i', "Item ignoring setup", do_cmd_options_item},
    {0, 0, NULL, NULL},
    {0, 'd', "Set base delay factor", do_cmd_delay},
    {0, 'h', "Set hitpoint warning", do_cmd_hp_warn},
    {0, 'm', "Set movement delay", do_cmd_lazymove_delay},
    {0, 0, NULL, NULL},
    {0, 'l', "Load a user pref file", options_load_pref_file},
    {0, 's', "Save options to pref file", do_dump_options},
    {0, 't', "Save autoinscriptions to pref file", do_dump_autoinsc},
    {0, 0, NULL, NULL},
    {0, 'k', "Edit keymaps (advanced)", do_cmd_keymaps},
    {0, 'v', "Edit colours (advanced)", do_cmd_colors}
};


/*
 * Display the options main menu.
 */
void do_cmd_options(void)
{
    if (!option_menu)
    {
        /* Main option menu */
        option_menu = menu_new_action(option_actions, N_ELEMENTS(option_actions));

        option_menu->title = "Options Menu";
        option_menu->flags = MN_CASELESS_TAGS;
    }

    screen_save();
    clear_from(0);

    menu_layout(option_menu, &SCREEN_REGION);
    menu_select(option_menu, 0, false);

    screen_load(true);

    /* Send event */
    Term_xtra(TERM_XTRA_REACT, use_graphics);

    /* Resend options to server */
    Send_options(gather_settings());
    Send_ignore();

    /* Redraw */
    player->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_MESSAGE | PR_MESSAGE_CHAT |
        PR_OTHER | PR_EXTRA | PR_DTRAP);
    redraw_stuff();
}


/*** Cleanup ***/


void cleanup_options(void)
{
    mem_free(keymap_menu);
    mem_free(color_menu);
    mem_free(option_menu);
}


/*
 * Return the name of an ignore type.
 */
const char *ignore_name_for_type(ignore_type_t type)
{
    size_t i;

    for (i = ITYPE_NONE + 1; i < ITYPE_MAX; i++)
    {
        /* Choice is [1..ITYPE_MAX] */
        if (quality_choices[i - 1].enum_val == (unsigned int)type)
            return quality_choices[i - 1].name;
    }

    return "unknown";
}


const char *quality_name_for_value(byte value)
{
    return quality_values[value].name;
}
