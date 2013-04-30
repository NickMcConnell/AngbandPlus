/*
 * File: ui-options.c
 * Purpose: Text UI options handling code (everything accessible from '=')
 *
 * Copyright (c) 1997-2000 Robert A. Koeneke, James E. Wilson, Ben Harrison
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
#include "keymap.h"
#include "netclient.h"
#include "prefs.h"
#include "ui-menu.h"


/*
 * Interactively dump preferences to a file.
 *
 * - Title must have the form "Dump <pref-type>"
 * - dump(ang_file *) needs to emit only the raw data for the dump.
 *   Comments are generated automatically
 */
static void dump_pref_file(void (*dump)(ang_file*), const char *title, int row, char *buf, size_t len)
{
    char ftmp[NORMAL_WID];
    int res;
    ui_event ea = EVENT_ABORT;

    screen_save();

    /* Prompt */
    prt(format("%s to a pref file", title), row, 0);

    /* Prompt */
    prt("File: ", row + 2, 0);

    /* Default filename */
    strnfmt(ftmp, sizeof(ftmp), "%s.prf", nick);

    /* Get a filename */
    res = askfor_ex(ftmp, sizeof(ftmp), NULL, FALSE);
    if (!res)
    {
        /* Build the filename */
        path_build(buf, len, ANGBAND_DIR_USER, ftmp);

        if (prefs_save(buf, dump, title))
            c_msg_print(format("Dumped %s", strstr(title, " ") + 1));
        else
            c_msg_print("Failed");
    }
    else if (res == 1) Term_event_push(&ea);

    screen_load(FALSE);
}


static void do_cmd_pref_file_hack(long row);


/*** Options display and setting ***/


/*** Boolean option menu code ***/


/*
 * Displays an option entry
 */
static void option_toggle_display(menu_type *m, int oid, bool cursor, int row, int col, int width)
{
    byte attr = curs_attrs[CURS_KNOWN][cursor != 0];
    bool *options = menu_priv(m);

    c_prt(attr, format("%-45s: %s  (%s)", option_desc(oid),
        options[oid] ? "yes" : "no ", option_name(oid)), row, col);
}


/*
 * Handle keypresses for an option entry
 */
static bool option_toggle_handle(menu_type *m, const ui_event *event, int oid)
{
    bool next = FALSE;

    if (event->type == EVT_SELECT)
    {
        /* Hack -- Birth options can not be toggled after birth */
        if (!(m->flags == MN_NO_TAGS))
            option_set(Client_setup.options, option_name(oid), !Client_setup.options[oid]);
    }
    else if (event->type == EVT_KBRD)
    {
        if ((event->key.code == 'y') || (event->key.code == 'Y'))
        {
            option_set(Client_setup.options, option_name(oid), TRUE);
            next = TRUE;
        }
        else if ((event->key.code == 'n') || (event->key.code == 'N'))
        {
            option_set(Client_setup.options, option_name(oid), FALSE);
            next = TRUE;
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
            return FALSE;
    }
    else
        return FALSE;

    if (next)
    {
        m->cursor++;
        m->cursor = (m->cursor + m->filter_count) % m->filter_count;
    }

    return TRUE;
}


/* Toggle option menu display and handling functions */
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
    menu_type *m = menu_new(MN_SKIN_SCROLL, &option_toggle_iter);
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* For all menus */
    m->prompt = "Set option (y/n/t), '?' for information";
    m->cmd_keys = "?YyNnTt";
    m->selections = "abcdefghijklmopqrsuvwxz";
    m->flags = MN_DBL_TAP;

    if ((page == OPT_PAGE_BIRTH) && (p_ptr->max_exp > 0))
    {
        m->prompt = "You can only modify these options at character birth. '?' for information";
        m->cmd_keys = "?";
        m->flags = MN_NO_TAGS;
    }

    /* For this particular menu */
    m->title = name;

    /* Find the number of valid entries */
    for (i = 0; i < OPT_PAGE_PER; i++)
    {
        if (option_page[page][i] == OPT_NONE) break;
    }

    /* Set the data to the player's options */
    menu_setpriv(m, OPT_MAX, &Client_setup.options);
    menu_set_filter(m, option_page[page], i);
    menu_layout(m, &SCREEN_REGION);

    /* Run the menu */
    screen_save();

    clear_from(0);
    ke = menu_select(m, 0, FALSE);
    if (is_abort(ke)) Term_event_push(&ea);

    screen_load(FALSE);

    mem_free(m);
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
            byte a = TERM_WHITE;
            const char *s = angband_term_name[j];

            /* Use color */
            if (j == x) a = TERM_L_BLUE;

            /* Window name, staggered, centered */
            Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
        }

        /* Display the options */
        for (i = 0; i < PW_MAX_FLAGS; i++)
        {
            byte a = TERM_WHITE;
            const char *str = window_flag_desc[i];

            /* Use color */
            if (i == y) a = TERM_L_BLUE;

            /* Unused option */
            if (!str) str = "(Unused option)";

            /* Flag name */
            Term_putstr(0, i + 5, -1, a, str);

            /* Display the windows */
            for (j = 0; j < ANGBAND_TERM_MAX; j++)
            {
                char c = '.';

                a = TERM_WHITE;

                /* Use color */
                if ((i == y) && (j == x)) a = TERM_L_BLUE;

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

    screen_load(FALSE);
}


/*** Interact with keymaps ***/


/*
 * Current (or recent) keymap action
 */
static struct keypress keymap_buffer[KEYMAP_ACTION_MAX];


/*
 * Ask for, and display, a keymap trigger.
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
static int keymap_get_trigger(struct keypress *c)
{
    char tmp[MSG_LEN];
    struct keypress buf[2] = {{0}, {0}};
    ui_event ke;

    /* Flush */
    flush();

    /* Get a key */
    ke = inkey_ex();
    return_on_abort(ke);
    buf[0] = ke.key;

    /* Convert to ascii */
    keypress_to_text(tmp, sizeof(tmp), buf, FALSE);

    /* Hack -- display the trigger */
    Term_addstr(-1, TERM_WHITE, tmp);

    /* Flush */
    flush();

    /* Return trigger */
    *c = ke.key;
    return 0;
}


/*
 * Macro menu action functions
 */


static void ui_keymap_pref_load(const char *title, int row)
{
    do_cmd_pref_file_hack(16);
}


static void ui_keymap_pref_append(const char *title, int row)
{
    char buf[MSG_LEN];

    dump_pref_file(keymap_dump, "Dump keymaps", 13, buf, sizeof(buf));
}


static void ui_keymap_query(const char *title, int row)
{
    char tmp[MSG_LEN];
    int mode = (OPT(rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
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

    /* Nothing found */
    if (!act)
    {
        /* Prompt */
        prt("No keymap with that trigger.  Press any key to continue.", 16, 0);
    }

    /* Found one */
    else
    {
        /* Analyze the current action */
        keypress_to_text(tmp, sizeof(tmp), act, FALSE);

        /* Display the current action */
        prt("Found: ", 15, 0);
        Term_addstr(-1, TERM_WHITE, tmp);

        /* Prompt */
        prt("Press any key to continue.", 17, 0);
    }

    ke = inkey_ex();
    if (is_abort(ke)) Term_event_push(&ea);
}


static void ui_keymap_create(const char *title, int row)
{
    bool done = FALSE;
    size_t n = 0;
    struct keypress c;
    char tmp[MSG_LEN];
    int mode = (OPT(rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
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
        c_prt(TERM_L_RED, "The '$' key is reserved.", 16, 2);
        prt("Press any key to continue.", 18, 0);
        ke = inkey_ex();
        if (is_abort(ke)) Term_event_push(&ea);
        return;
    }

    /* Get an encoded action, with a default response */
    while (!done)
    {
        struct keypress kp = {EVT_NONE, 0, 0};
        int color = TERM_WHITE;

        if (n == 0) color = TERM_YELLOW;
        if (n == KEYMAP_ACTION_MAX) color = TERM_L_RED;

        keypress_to_text(tmp, sizeof(tmp), keymap_buffer, FALSE);
        c_prt(color, format("Action: %s", tmp), 15, 0);

        c_prt(TERM_L_BLUE, "  Press '$' when finished.", 17, 0);
        c_prt(TERM_L_BLUE, "  Use 'CTRL-U' to reset.", 18, 0);
        c_prt(TERM_L_BLUE, format("(Maximum keymap length is %d keys.)", KEYMAP_ACTION_MAX), 19, 0);

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
            done = TRUE;
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
                memset(keymap_buffer, 0, sizeof(keymap_buffer));
                n = 0;
                break;
            }
            default:
            {
                if (n == KEYMAP_ACTION_MAX) continue;

                if (n == 0) memset(keymap_buffer, 0, sizeof(keymap_buffer));
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
        keymap_add(mode, c, keymap_buffer, TRUE);
        prt("Keymap added.  Press any key to continue.", 17, 0);
        ke = inkey_ex();
        if (is_abort(ke)) Term_event_push(&ea);
    }
}


static void ui_keymap_remove(const char *title, int row)
{
    struct keypress c;
    int mode = (OPT(rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
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
        Term_putstr(0, 0, -1, TERM_WHITE, "Browse Macros");

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

    screen_load(FALSE);
}


static void keymap_browse_hook(int oid, void *db, const region *loc)
{
    char tmp[MSG_LEN];

    clear_from(13);

    /* Show current action */
    prt("Current action (if any) shown below:", 13, 0);
    keypress_to_text(tmp, sizeof(tmp), keymap_buffer, FALSE);
    prt(tmp, 14, 0);
}


static menu_type *keymap_menu;
static menu_action keymap_actions[] =
{
    {0, 0, "Load a user pref file", ui_keymap_pref_load},
    {0, 0, "Append keymaps to a file", ui_keymap_pref_append},
    {0, 0, "Query a keymap", ui_keymap_query},
    {0, 0, "Create a keymap", ui_keymap_create},
    {0, 0, "Remove a keymap", ui_keymap_remove},
    {0, 0, "Browse keymaps", ui_keymap_browse}
};


/*
 * Interact with "keymaps"
 */
void do_cmd_keymaps(const char *title, int row)
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
    ke = menu_select(keymap_menu, 0, FALSE);
    if (title && is_abort(ke)) Term_event_push(&ea);

    screen_load(TRUE);
}


/*** Interact with colours ***/


static void colors_pref_load(const char *title, int row)
{
    /* Ask for and load a user pref file */
    do_cmd_pref_file_hack(8);

    /* Should probably be a cleaner way to tell UI about colour changes */
    Term_xtra(TERM_XTRA_REACT, p_ptr->use_graphics);
    Term_redraw();
}


static void colors_pref_dump(const char *title, int row)
{
    char buf[MSG_LEN];

    dump_pref_file(dump_colors, title, 15, buf, sizeof(buf));
}


static void colors_modify(const char *title, int row)
{
    int i;
    ui_event ke;
    static byte a = 0;
    ui_event ea = EVENT_ABORT;

    /* Prompt */
    prt("Command: Modify colors", 8, 0);

    /* Hack -- Query until done */
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
        Term_putstr(5, 10, -1, TERM_WHITE,
            format("Color = %d, Name = %s, Index = %c", a, name, index));

        /* Label the Current values */
        Term_putstr(5, 12, -1, TERM_WHITE, format("K = 0x%02x / R,G,B = 0x%02x,0x%02x,0x%02x",
            angband_color_table[a][0], angband_color_table[a][1], angband_color_table[a][2],
            angband_color_table[a][3]));

        /* Prompt */
        Term_putstr(0, 14, -1, TERM_WHITE, "Command (n/N/k/K/r/R/g/G/b/B): ");

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

        /* Hack -- React to changes */
        Term_xtra(TERM_XTRA_REACT, p_ptr->use_graphics);

        /* Hack -- Redraw */
        Term_redraw();
    }

    if (is_abort(ke)) Term_event_push(&ea);
}


static void colors_browse_hook(int oid, void *db, const region *loc)
{
    clear_from(1);
}


static menu_type *color_menu;


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
    ke = menu_select(color_menu, 0, FALSE);
    if (title && is_abort(ke)) Term_event_push(&ea);

    screen_load(TRUE);
}


/*** Non-complex menu actions ***/


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

    return FALSE;
}


/*
 * Set base delay factor
 */
static void do_cmd_delay(const char *name, int row)
{
    int res;
    char tmp[4] = "";
    ui_event ea = EVENT_ABORT;

    strnfmt(tmp, sizeof(tmp), "%i", p_ptr->other.delay_factor);

    screen_save();

    /* Prompt */
    prt("Command: Base Delay Factor", 20, 0);

    prt(format("Current base delay factor: %d msec", p_ptr->other.delay_factor), 22, 0);
    prt("New base delay factor (0-255): ", 21, 0);

    /* Ask for a numeric value */
    res = askfor_ex(tmp, sizeof(tmp), askfor_aux_numbers, FALSE);
    if (!res)
    {
        u16b val = (u16b)strtoul(tmp, NULL, 0);

        p_ptr->other.delay_factor = MIN(val, 255);
    }
    else if (res == 1)
        Term_event_push(&ea);

    screen_load(FALSE);
}


/*
 * Set hitpoint warning level
 */
static void do_cmd_hp_warn(const char *name, int row)
{
    int res;
    char tmp[2] = "";
    ui_event ea = EVENT_ABORT;

    strnfmt(tmp, sizeof(tmp), "%i", p_ptr->other.hitpoint_warn);

    screen_save();

    /* Prompt */
    prt("Command: Hitpoint Warning", 20, 0);

    prt(format("Current hitpoint warning: %d (%d%%)",
        p_ptr->other.hitpoint_warn, p_ptr->other.hitpoint_warn * 10), 22, 0);
    prt("New hitpoint warning (0-9): ", 21, 0);

    /* Ask the user for a string */
    res = askfor_ex(tmp, sizeof(tmp), askfor_aux_numbers, FALSE);

    /* Process input */
    if (!res) p_ptr->other.hitpoint_warn = (byte)strtoul(tmp, NULL, 0);
    else if (res == 1) Term_event_push(&ea);

    screen_load(FALSE);
}


/*
 * Set "lazy-movement" delay
 */
static void do_cmd_lazymove_delay(const char *name, int row)
{
    int res;
    char tmp[2] = "";
    ui_event ea = EVENT_ABORT;

    strnfmt(tmp, sizeof(tmp), "%i", lazymove_delay);

    screen_save();

    /* Prompt */
    prt("Command: Movement Delay Factor", 20, 0);

    prt(format("Current movement delay factor: %d (%d msec)",
        lazymove_delay, lazymove_delay * 100), 22, 0);
    prt("New movement delay factor (0-9): ", 21, 0);

    /* Ask the user for a string */
    res = askfor_ex(tmp, sizeof(tmp), askfor_aux_numbers, FALSE);

    /* Process input */
    if (!res) lazymove_delay = (byte)strtoul(tmp, NULL, 0);
    else if (res == 1) Term_event_push(&ea);

    screen_load(FALSE);
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
    strnfmt(ftmp, sizeof(ftmp), "%s.prf", nick);

    /* Ask for a file (or cancel) */
    res = askfor_ex(ftmp, sizeof(ftmp), NULL, FALSE);
    if (!res)
    {
        /* Process the given filename */
        if (!process_pref_file(ftmp, FALSE, TRUE))
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

    screen_load(FALSE);
}


/*
 * Write options to a file.
 */
static void do_dump_options(const char *title, int row)
{
    char buf[MSG_LEN];

    dump_pref_file(option_dump, "Dump options", 20, buf, sizeof(buf));
    prefs_save(buf, keymap_dump, "Dump keymaps");
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


/*** Quality-squelch menu ***/


/*
 * Used for mapping the values below to names.
 */
typedef struct
{
    int enum_val;
    const char *name;
} quality_name_struct;


static quality_name_struct quality_choices[TYPE_MAX] =
{
    {TYPE_JEWELRY, "Jewelry"},
    {TYPE_DRAG_ARMOR, "Dragon Armor"},
    {TYPE_WEARABLE, "Other Wearable Items"},
    {TYPE_BOOK, "Books"},
    {TYPE_CONSUMABLE, "Other Consumable Items"}
};


/*
 * The names for the various kinds of quality
 */
static quality_name_struct quality_values[SQUELCH_MAX] =
{
    {SQUELCH_NONE, "no squelch"},
    {SQUELCH_WORTHLESS, "worthless"},
    {SQUELCH_AVERAGE, "average"},
    {SQUELCH_GOOD, "good"},
    {SQUELCH_EXCELLENT_NO_HI, "excellent with no high resists"},
    {SQUELCH_EXCELLENT_NO_SPL, "excellent but not splendid"},
    {SQUELCH_ALL, "non-artifact"}
};


/*
 * Display an entry in the menu.
 */
static void quality_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
    const char *name = quality_choices[oid].name;
    byte level = p_ptr->other.squelch_lvl[oid];
    const char *level_name = quality_values[level].name;
    byte attr = (cursor? TERM_L_BLUE: TERM_WHITE);

    c_put_str(attr, format("%-25s : %s", name, level_name), row, col);
}


/*
 * Display the quality squelch subtypes.
 */
static void quality_subdisplay(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
    const char *name = quality_values[oid].name;
    byte attr = (cursor? TERM_L_BLUE: TERM_WHITE);

    c_put_str(attr, name, row, col);
}


/*
 * Handle keypresses.
 */
static bool quality_action(menu_type *m, const ui_event *event, int oid)
{
    menu_type menu;
    menu_iter menu_f = {NULL, NULL, quality_subdisplay, NULL, NULL};
    region area = {32, 2, 29, SQUELCH_MAX};
    ui_event evt;
    int count;
    ui_event ea = EVENT_ABORT;

    /* Display at the right point */
    area.row += oid;

    /* Save */
    screen_save();

    /* Work out how many options we have */
    count = SQUELCH_MAX;
    /*if ((oid == TYPE_RING) || (oid == TYPE_AMULET))
        count = area.page_rows = SQUELCH_BAD + 1;*/

    /* Run menu */
    menu_init(&menu, MN_SKIN_SCROLL, &menu_f);
    menu_setpriv(&menu, count, quality_values);

    /* Stop menus from going off the bottom of the screen */
    if (area.row + menu.count > Term->hgt - 1)
        area.row += Term->hgt - 1 - area.row - menu.count;

    menu_layout(&menu, &area);

    window_make(area.col - 2, area.row - 1, area.col + area.width + 2, area.row + area.page_rows);

    evt = menu_select(&menu, 0, TRUE);
    if (is_abort(evt)) Term_event_push(&ea);

    /* Set the new value appropriately */
    if (evt.type == EVT_SELECT) p_ptr->other.squelch_lvl[oid] = menu.cursor;

    /* Load and finish */
    screen_load(FALSE);
    return TRUE;
}


/*
 * Display and handle the main squelching menu.
 */
static void do_cmd_options_item(const char *title, int row)
{
    menu_type menu;
    menu_iter menu_f = {NULL, NULL, quality_display, quality_action, NULL};
    region area = {0, 0, 0, 0};
    ui_event ke;
    ui_event ea = EVENT_ABORT;

    /* Save screen */
    screen_save();
    clear_from(0);

    /* Set up the menu */
    menu_init(&menu, MN_SKIN_SCROLL, &menu_f);
    menu.title = title;
    menu_setpriv(&menu, TYPE_MAX, quality_values);
    menu_layout(&menu, &area);

    /* Select an entry */
    ke = menu_select(&menu, 0, FALSE);
    if (is_abort(ke)) Term_event_push(&ea);

    /* Load screen */
    screen_load(FALSE);
}


/*** Main menu definitions and display ***/


static menu_type *option_menu;


static menu_action option_actions[] =
{
    {0, 'a', "Interface options", option_toggle_menu},
    {0, 'b', "Display options", option_toggle_menu},
    {0, 'e', "Warning and disturbance options", option_toggle_menu},
    {0, 'f', "Birth (difficulty) options", option_toggle_menu},
    {0, 0, 0, 0}, /* Load and append */
    {0, 'w', "Subwindow display settings", do_cmd_options_win},
    {0, 's', "Item squelch settings", do_cmd_options_item},
    {0, 'd', "Set base delay factor", do_cmd_delay},
    {0, 'h', "Set hitpoint warning", do_cmd_hp_warn},
    {0, 'i', "Set movement delay factor", do_cmd_lazymove_delay},
    {0, 'l', "Load a user pref file", options_load_pref_file},
    {0, 'o', "Save options", do_dump_options},
    {0, 0, 0, 0}, /* Interact with */
    {0, 'm', "Interact with keymaps (advanced)", do_cmd_keymaps},
    {0, 'c', "Interact with colours (advanced)", do_cmd_colors}
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
    menu_select(option_menu, 0, FALSE);

    screen_load(TRUE);

    /* Send event */
    Term_xtra(TERM_XTRA_REACT, p_ptr->use_graphics);

    /* Resend options to server */
    Send_options(gather_settings());

    /* Redraw */
    p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_MESSAGE | PR_MESSAGE_CHAT |
        PR_OTHER | PR_EXTRA | PR_DTRAP);
    redraw_stuff();
}


/*** Cleanup ***/


void free_option_menus(void)
{
    mem_free(keymap_menu);
    mem_free(color_menu);
    mem_free(option_menu);
}
