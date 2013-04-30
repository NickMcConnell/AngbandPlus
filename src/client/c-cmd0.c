/*
 * File: c-cmd0.c
 * Purpose: Deal with command processing
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
#include "c-cmds.h"
#include "keymap.h"
#include "netclient.h"
#include "textui.h"
#include "ui-menu.h"


/*
 * This file contains (several) big lists of commands, so that they can be
 * easily manipulated for e.g. help displays, or if a port wants to provide a
 * native menu containing a command list.
 *
 * Consider a two-paned layout for the command menus. XXX
 *
 * This file still needs some clearing up. XXX
 */


/*** Misc commands ***/


/*
 * Quit the game.
 */
static void do_cmd_quit(void)
{
    quit(NULL);
}


/*
 * Display the options and redraw afterward.
 */
static void do_cmd_xxx_options(void)
{
    do_cmd_options();
    Send_redraw();
}


/*
 * Escape command
 */
static void do_cmd_escape(void)
{
    if (first_escape) Send_clear();
    first_escape = FALSE;
}


/*
 * Invoked when the command isn't recognized.
 */
static void do_cmd_unknown(void)
{
    prt("Type '?' for help.", 0, 0);
}


/*
 * Interact with keymaps
 */
static void do_cmd_keymaps_hack(void)
{
    do_cmd_keymaps(NULL, 0);
}


/*** Handling bits ***/


/*
 * Holds a generic command.
 */
struct cmd_info
{
    const char *desc;
    keycode_t key[2];
    cmd_code cmd;
    void (*hook)(void);
    bool (*prereq)(void);
};


/* Magic use */
static struct cmd_info cmd_magic[] =
{
    {"Use polymorphing ability", {'V'}, CMD_POLY, textui_cmd_poly, NULL},
    {"Use breath attack", {'y', KTRL('A')}, CMD_BREATH, NULL, NULL}
};


/* Item commands */
static struct cmd_info cmd_item[] =
{
    {"Inscribe an object", {'{'}, CMD_INSCRIBE, NULL, NULL},
    {"Uninscribe an object", {'}'}, CMD_UNINSCRIBE, NULL, NULL},
    {"Wear/wield an item", {'w'}, CMD_WIELD, NULL, NULL},
    {"Take off/unwield an item", {'t', 'T'}, CMD_TAKEOFF, NULL, NULL},
    {"Examine an item", {'I'}, CMD_EXAMINE, NULL, NULL},
    {"Drop an item", {'d'}, CMD_DROP, NULL, NULL},
    {"Fire your missile weapon", {'f', 't'}, CMD_FIRE, NULL, NULL},
    {"Use a staff", {'u', 'Z'}, CMD_USE_STAFF, NULL, NULL},
    {"Aim a wand", {'a', 'z'}, CMD_USE_WAND, NULL, NULL},
    {"Zap a rod", {'z', 'a'}, CMD_USE_ROD, NULL, NULL},
    {"Activate an object", {'A'}, CMD_ACTIVATE, NULL, NULL},
    {"Eat some food", {'E'}, CMD_EAT, NULL, NULL},
    {"Quaff a potion", {'q'}, CMD_QUAFF, NULL, NULL},
    {"Read a scroll", {'r'}, CMD_READ_SCROLL, NULL, NULL},
    {"Fuel your light source", {'F'}, CMD_REFILL, NULL, obj_refill_pre},
    {"Use an item", {'U', KTRL('V')}, CMD_USE_ANY, NULL, NULL}
};


/* General actions */
static struct cmd_info cmd_action[] =
{
    {"Search for traps/doors", {'s'}, CMD_SEARCH, NULL, NULL},
    {"Disarm a trap or chest", {'D'}, CMD_DISARM, NULL, NULL},
    {"Rest for a while", {'R'}, CMD_NULL, textui_cmd_rest, NULL},
    {"Look around", {'l', 'x'}, CMD_NULL, do_cmd_look, NULL},
    {"Target monster or location", {'*'}, CMD_NULL, do_cmd_target, NULL},
    {"Target non hostile player", {'('}, CMD_NULL, do_cmd_target_friendly, NULL},
    {"Target closest monster", {'\''}, CMD_NULL, do_cmd_target_closest, NULL},
    {"Dig a tunnel", {'T', KTRL('T')}, CMD_TUNNEL, NULL, NULL},
    {"Go up staircase", {'<'}, CMD_GO_UP, NULL, NULL},
    {"Go down staircase", {'>'}, CMD_GO_DOWN, NULL, NULL},
    {"Toggle search mode", {'S', '#'}, CMD_TOGGLE_SEARCH, NULL, NULL},
    {"Open a door or a chest", {'o'}, CMD_OPEN, NULL, NULL},
    {"Close a door", {'c'}, CMD_CLOSE, NULL, NULL},
    {"Jam a door shut", {'j', 'S'}, CMD_JAM, NULL, NULL},
    {"Bash a door open", {'B', 'f'}, CMD_BASH, NULL, NULL},
    {"Fire at nearest target", {'h', KC_TAB}, CMD_NULL, textui_cmd_fire_at_nearest, NULL},
    {"Throw an item", {'v'}, CMD_THROW, textui_cmd_throw, NULL},
    {"Walk into a trap", {'W', '-'}, CMD_JUMP, NULL, NULL},
    {"Interact with a fountain", {'_'}, CMD_FOUNTAIN, do_cmd_fountain, NULL}
};


/* Item management commands */
static struct cmd_info cmd_item_manage[]  =
{
    {"Display equipment listing", {'e'}, CMD_NULL, do_cmd_equip, NULL},
    {"Display inventory listing", {'i'}, CMD_NULL, do_cmd_inven, NULL},
    {"Pick up objects", {'g'}, CMD_PICKUP, NULL, NULL},
    {"Ignore/destroy an item", {'k', KTRL('D')}, CMD_DESTROY, textui_cmd_destroy, NULL},
    {"Drop gold", {'$'}, CMD_DROP_GOLD, textui_cmd_drop_gold, NULL},
    {"Steal an item", {'J', 'X'}, CMD_STEAL, NULL, NULL}
};


/* Information access commands */
static struct cmd_info cmd_info[] =
{
    {"Browse a book", {'b', 'P'}, CMD_BROWSE_SPELL, NULL, obj_browse_pre},
    {"Gain new spells or prayers", {'G'}, CMD_STUDY_BOOK, NULL, obj_study_pre},
    {"Cast a spell", {'m'}, CMD_CAST, NULL, obj_cast_pre},
    {"Project a spell", {'p'}, CMD_PROJECT, NULL, obj_cast_pre},
    {"Full dungeon map", {'M'}, CMD_NULL, do_cmd_view_map, NULL},
    {"Toggle ignoring of items", {'K', 'O'}, CMD_NULL, textui_cmd_toggle_ignore, NULL},
    {"Display visible item list", {']'}, CMD_NULL, do_cmd_itemlist, NULL},
    {"Display visible monster list", {'['}, CMD_NULL, do_cmd_monlist, NULL},
    {"Locate player on map", {'L', 'W'}, CMD_NULL, do_cmd_locate, NULL},
    {"Help", {'?'}, CMD_NULL, do_cmd_help, NULL},
    {"Identify symbol", {'/'}, CMD_NULL, do_cmd_query_symbol, NULL},
    {"Character description", {'C'}, CMD_NULL, do_cmd_change_name, NULL},
    {"Check knowledge", {'~'}, CMD_NULL, textui_browse_knowledge, NULL},
    {"Repeat level feeling", {KTRL('F')}, CMD_NULL, do_cmd_feeling, NULL},
    {"Show previous messages", {KTRL('P')}, CMD_NULL, do_cmd_messages, NULL},
    {"Show chat messages", {':'}, CMD_NULL, do_cmd_message, NULL},
    {"Chat commands", {KTRL('Z')}, CMD_NULL, do_cmd_chat, NULL},
    {"Access party menu", {'P', '!'}, CMD_NULL, do_cmd_party, NULL},
    {"Display connected players", {'@', '|'}, CMD_NULL, do_cmd_players, NULL},
    {"Describe an item in chat window", {KTRL('D'), KTRL('O')}, CMD_NULL, do_cmd_describe, NULL},
    {"Full wilderness map", {KTRL('W')}, CMD_NULL, do_cmd_wild_map, NULL}
};


/* Utility/assorted commands */
static struct cmd_info cmd_util[] =
{
    {"Interact with options", {'='}, CMD_NULL, do_cmd_xxx_options, NULL},
    {"Interact with keymaps", {'%'}, CMD_NULL, do_cmd_keymaps_hack, NULL},
    {"Save and quit", {KTRL('X')}, CMD_QUIT, do_cmd_quit, NULL},
    {"Quit (commit suicide)", {'Q'}, CMD_NULL, textui_cmd_suicide, NULL},
    {"Redraw the screen", {KTRL('R')}, CMD_NULL, do_cmd_redraw, NULL},
    {"Save \"screen dump\"", {')'}, CMD_NULL, do_cmd_save_screen, NULL},
    {"Purchase a house", {KTRL('E')}, CMD_NULL, do_cmd_purchase_house, NULL},
    {"Take a quest", {KTRL('Q')}, CMD_NULL, do_cmd_quest, NULL},
    {"Socials", {KTRL('S')}, CMD_NULL, do_cmd_social, NULL}
};


/* Commands that shouldn't be shown to the user */
static struct cmd_info cmd_hidden[] =
{
    {"Load a single pref line", {'"'}, CMD_NULL, do_cmd_pref, NULL},
    {"Alter a grid", {'+'}, CMD_ALTER, NULL, NULL},
    {"Walk", {';'}, CMD_WALK, NULL, NULL},
    {"Start running", {'.', ','}, CMD_RUN, NULL, NULL},
    {"Stand still", {',', '.'}, CMD_HOLD, NULL, NULL},
    {"Center map", {KTRL('L'), '@'}, CMD_NULL, do_cmd_center_map, NULL},
    {"Do autopickup", {KTRL('G')}, CMD_AUTOPICKUP, NULL, NULL},
    {"Access dungeon master menu", {'&'}, CMD_NULL, do_cmd_master, NULL}
};


/*
 * A categorised list of all the command lists.
 */
typedef struct
{
    const char *name;
    struct cmd_info *list;
    size_t len;
} command_list;


static command_list cmds_all[] =
{
    {"Misc magic", cmd_magic, N_ELEMENTS(cmd_magic)},
    {"Items", cmd_item, N_ELEMENTS(cmd_item)},
    {"Action commands", cmd_action, N_ELEMENTS(cmd_action)},
    {"Manage items", cmd_item_manage, N_ELEMENTS(cmd_item_manage)},
    {"Information", cmd_info, N_ELEMENTS(cmd_info)},
    {"Utility", cmd_util, N_ELEMENTS(cmd_util)},
    {"Hidden", cmd_hidden, N_ELEMENTS(cmd_hidden)}
};


/*** Menu functions ***/


/* Display an entry on a command menu */
static void cmd_sub_entry(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
    byte attr = (cursor? TERM_L_BLUE: TERM_WHITE);
    const struct cmd_info *commands = menu_priv(menu);
    int mode = (OPT(rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    struct keypress kp;
    char buf[16];

    kp.type = EVT_KBRD;
    kp.code = commands[oid].key[mode];
    kp.mods = 0;

    /* Write the description */
    Term_putstr(col, row, -1, attr, commands[oid].desc);

    /* Include keypress */
    Term_addch(attr, ' ');
    Term_addch(attr, '(');

    /* Get readable version */
    keypress_to_readable(buf, sizeof(buf), kp);
    Term_addstr(-1, attr, buf);

    Term_addch(attr, ')');
}


/*
 * Display a list of commands.
 */
static bool cmd_menu(command_list *list, void *selection_p)
{
    menu_type menu;
    menu_iter commands_menu = {NULL, NULL, cmd_sub_entry, NULL, NULL};
    region area = {23, 4, 37, 13};
    ui_event evt;
    struct cmd_info **selection = selection_p;
    ui_event ea = EVENT_ABORT;

    /* Set up the menu */
    menu_init(&menu, MN_SKIN_SCROLL, &commands_menu);
    menu_setpriv(&menu, list->len, list->list);
    menu_layout(&menu, &area);

    /* Set up the screen */
    screen_save();
    window_make(21, 3, 62, 17);

    /* Select an entry */
    evt = menu_select(&menu, 0, TRUE);

    /* Load the screen */
    screen_load(FALSE);

    if (evt.type == EVT_SELECT) *selection = &list->list[menu.cursor];
    else if (is_abort(evt)) Term_event_push(&ea);

    return FALSE;
}


static bool cmd_list_action(menu_type *m, const ui_event *event, int oid)
{
    if (event->type == EVT_SELECT)
        return cmd_menu(&cmds_all[oid], menu_priv(m));
    return FALSE;
}


static void cmd_list_entry(menu_type *menu, int oid, bool cursor, int row,
    int col, int width)
{
    byte attr = (cursor? TERM_L_BLUE: TERM_WHITE);

    Term_putstr(col, row, -1, attr, cmds_all[oid].name);
}


static menu_type *command_menu;


static menu_iter command_menu_iter =
{
    NULL,
    NULL,
    cmd_list_entry,
    cmd_list_action,
    NULL
};


/*
 * Display a list of command types, allowing the user to select one.
 */
static struct cmd_info *textui_action_menu_choose(void)
{
    region area = {21, 5, 37, 6};
    struct cmd_info *chosen_command = NULL;

    if (!command_menu) command_menu = menu_new(MN_SKIN_SCROLL, &command_menu_iter);

    menu_setpriv(command_menu, N_ELEMENTS(cmds_all) - 1, &chosen_command);
    menu_layout(command_menu, &area);

    /* Set up the screen */
    screen_save();
    window_make(19, 4, 58, 11);

    /* Select an entry */
    menu_select(command_menu, 0, TRUE);

    /* Load the screen */
    screen_load(TRUE);

    return chosen_command;
}


/*** Exported functions ***/


/* List indexed by char */
static struct cmd_info *converted_list[2][MAX_UCHAR + 1];


/*
 * Initialise the command list.
 */
void cmd_init(void)
{
    size_t i, j;

    memset(converted_list, 0, sizeof(converted_list));

    /* Go through all generic commands */
    for (j = 0; j < N_ELEMENTS(cmds_all); j++)
    {
        struct cmd_info *commands = cmds_all[j].list;

        /* Fill everything in */
        for (i = 0; i < cmds_all[j].len; i++)
        {
            /* If a roguelike key isn't set, use default */
            if (!commands[i].key[1]) commands[i].key[1] = commands[i].key[0];

            converted_list[0][commands[i].key[0]] = &commands[i];
            converted_list[1][commands[i].key[1]] = &commands[i];
        }
    }
}


cmd_code cmd_lookup(unsigned char key, int mode)
{
    assert((mode == KEYMAP_MODE_ROGUE) || (mode == KEYMAP_MODE_ORIG));

    if (!converted_list[mode][key]) return CMD_NULL;
    return converted_list[mode][key]->cmd;
}


/*** Input processing ***/


/*
 * Hack -- special buffer to hold the action of the current keymap
 */
static struct keypress request_command_buffer[256];


/*
 * Request a command from the user.
 *
 * Note that "caret" ("^") is treated specially, and is used to
 * allow manual input of control characters.  This can be used
 * on many machines to request repeated tunneling (Ctrl-H) and
 * on the Macintosh to request "Control-Caret".
 *
 * Note that "backslash" is treated specially, and is used to bypass any
 * keymap entry for the following character.  This is useful for keymaps.
 */
static ui_event textui_get_command(void)
{
    int mode = (OPT(rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    ui_event ke = EVENT_EMPTY;
    const struct keypress *act = NULL;

    /* Reset command */
    ui_event ke0 = EVENT_EMPTY;

    /* Activate "command mode" */
    inkey_flag = TRUE;

    /* Activate "scan mode" */
    inkey_scan = SCAN_INSTANT;

    /* Get a command */
    ke = inkey_ex();

    /* Paranoia */
    if ((ke.type == EVT_NONE) || ((ke.type == EVT_KBRD) && (ke.key.code == '\0')))
        return ke0;

    /* Flush messages */
    c_msg_print(NULL);

    if (ke.type == EVT_KBRD)
    {
        bool keymap_ok = TRUE;

        switch (ke.key.code)
        {
            /* Allow "keymaps" to be bypassed */
            case '\\':
            {
                get_com_ex("Command: ", &ke);
                keymap_ok = FALSE;
                break;
            }

            /* Allow "control chars" to be entered */
            case '^':
            {
                if (get_com("Control: ", &ke.key)) ke.key.code = KTRL(ke.key.code);
                break;
            }
        }

        /* Find any relevant keymap */
        if (keymap_ok) act = keymap_find(mode, ke.key);
    }

    /* Erase the message line */
    prt("", 0, 0);

    /* Apply keymap if not inside a keymap already */
    if (ke.key.code && act && !inkey_next)
    {
        size_t n = 0;

        while (act[n].type) n++;

        /* Make room for the terminator */
        n += 1;

        /* Install the keymap */
        memcpy(request_command_buffer, act, n * sizeof(struct keypress));

        /* Start using the buffer */
        inkey_next = request_command_buffer;

        /* Continue */
        return ke0;
    }

    /* Paranoia */
    if ((ke.type == EVT_KBRD) && !ke.key.code) ke.key.code = ESCAPE;

    return ke;
}


/*
 * Process a textui keypress.
 */
static bool textui_process_key(struct keypress kp)
{
    struct cmd_info *cmd;
    int mode = (OPT(rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    keycode_t c = kp.code;

    if ((c == '\0') || (c == ESCAPE) || (c == ' ') || (c == '\a'))
    {
        do_cmd_escape();
        return TRUE;
    }

    if (c == KC_ENTER)
        cmd = textui_action_menu_choose();
    else
    {
        if (c > UCHAR_MAX) return FALSE;
        cmd = converted_list[mode][c];
    }

    if (!cmd) return FALSE;

    if (!cmd->prereq || cmd->prereq())
    {
        if (cmd->hook)
            cmd->hook();
        else if (cmd->cmd)
            process_command(cmd->cmd);
    }

    return TRUE;
}


/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
void textui_process_command()
{
    bool done;
    ui_event e;

    /* See if we have a command waiting */
    e = textui_get_command();

    /* Process any commands we got */
    while (e.type != EVT_NONE)
    {
        done = TRUE;

        /* Process it */
        if (e.type == EVT_KBRD) done = textui_process_key(e.key);
        if (!done) do_cmd_unknown();

        /* Ask for another command */
        e = textui_get_command();
    }
}


/*** Cleanup ***/


void free_command_menu(void)
{
    mem_free(command_menu);
}


/*** Misc commands ***/


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
 *
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 *
 * 'mode' is one of TARGET_LOOK or TARGET_KILL.
 * 'x' and 'y' are the initial position of the target to be highlighted,
 * or -1 if no location is specified.
 * Returns TRUE if a target has been successfully set, FALSE otherwise.
 */
bool cmd_target_interactive(int mode)
{
    bool done = FALSE;
    struct keypress query;

    target_icky_screen = TRUE;

    Term->cursor_icky = TRUE;

    /* Tell the server to init targeting */
    Send_target_interactive(mode, '\0');

    /* Interact */
    while (!done)
    {
        /* Describe and Prompt */
        query = inkey();
        if (!query.code) continue;

        Send_target_interactive(mode, query.code);

        switch (query.code)
        {
            case ESCAPE:
            case 'q':
            case 'r':
            case 't':
            case '5':
            case '0':
            case '.':
            {
                done = TRUE;
                break;
            }
        }
    }

    /* Reset cursor stuff */
    Term->cursor_icky = FALSE;
    Term_set_cursor(FALSE);

    /* Clear top line */
    prt("", 0, 0);

    target_icky_screen = FALSE;
    if (full_icky_screen) Term_redraw();

    return TRUE;
}


void cmd_chat_close(int n)
{
    char buf[NORMAL_WID];

    if (n)
    {
        /* Request channel leave */
        if (channels[n].name[0] == '#')
        {
            strnfmt(buf, sizeof(buf), "-%s", channels[n].name);
            Send_chan(buf);
        }

        /* Close locally */
        else
        {
            if (view_channel == n)
                cmd_chat_cycle(-1);

            channels[n].name[0] = '\0';
            channels[n].id = 0;

            if (p_ptr->main_channel == n)
                p_ptr->main_channel = 0;
            if (STRZERO(channels[view_channel].name))
                cmd_chat_cycle(+1);

            /* Redraw */
            p_ptr->redraw |= PR_MESSAGE_CHAT;
        }
    }
    else
        Send_chan("");
}


void cmd_chat_cycle(int dir)
{
    s16b new_channel = view_channel;

    while (TRUE)
    {
        new_channel += dir;

        if (new_channel > MAX_CHANNELS || new_channel < 0) return;
        if (STRZERO(channels[new_channel].name)) continue;

        break;
    }

    if (new_channel != view_channel)
    {
        /* Set new */
        view_channel = new_channel;
        p_ptr->on_channel[view_channel] = 0;

        /* Redraw */
        p_ptr->redraw |= PR_MESSAGE_CHAT;
    }
}
