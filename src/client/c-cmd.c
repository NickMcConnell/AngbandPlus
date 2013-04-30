/*
 * File: c-cmd.c
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
#include "../common/tvalsval.h"
#include "../common/md5.h"
#include "c-cmds.h"
#include "netclient.h"
#include "prefs.h"


/*** Command functions ***/


static int race_index_fuzzy(char *name)
{
    char monster[NORMAL_WID];
    char* str;
    char* dst;
    int i;

    /* Lowercase our search string */
    for (str = name; *str; str++) *str = tolower((unsigned char)*str);

    /* For each monster race */
    for (i = 1; i < z_info->r_max; i++)
    {
        /* Skip non-entries */
        if (!r_info[i].name) continue;

        /* Clean up monster name */
        dst = monster;
        for (str = r_info[i].name; *str; str++)
        {
            if (isalpha(*str) || (*str == 32)) *dst++ = tolower((unsigned char)*str);
        }
        *dst++ = '\0';

        /* If cleaned name matches our search string, return it */
        if (strstr(monster, name)) return i;
    }

    return 0;
}


void textui_cmd_poly(void)
{
    char buf[NORMAL_WID];
    int number;

    /* Non mimics */
    if (!player_has(p_ptr, PF_MONSTER_SPELLS))
    {
        c_msg_print("You are too solid.");
        return;
    }

    /* Build the default */
    my_strcpy(buf, "*", sizeof(buf));

    /* Get a number/name or abort */
    if (!get_string("Enter race index/name (0 for player, * for list): ", buf, sizeof(buf)))
        return;

    /* Extract a number */
    number = atoi(buf);

    /* Handle list */
    if ((number == 0) && (buf[0] == '*'))
    {
        /* Set the hook */
        special_line_type = SPECIAL_FILE_POLY;

        /* Set the header */
        my_strcpy(special_line_header[NTERM_WIN_OVERHEAD], "Killed List",
            sizeof(special_line_header[0]));

        /* Call the file perusal */
        peruse_file();

        return;
    }

    /* Handle name */
    if ((number == 0) && (buf[0] != '0')) number = race_index_fuzzy(buf);

    /* Enforce the maximum */
    if (number > z_info->r_max - 1) number = z_info->r_max - 1;

    /* Enforce the minimum */
    if (number < 0) number = 0;

    Send_poly(number);
}


void textui_cmd_rest(void)
{
    const char *p = "Rest (1-9999, '!' for HP or SP, '*' for HP and SP, '&' as needed): ";
    char out_val[5] = "&";
    s16b resting;

    /* Ask for duration */
    if (!get_string(p, out_val, sizeof(out_val))) return;

    /* Rest until done */
    if (out_val[0] == '&') resting = REST_COMPLETE;

    /* Rest a lot */
    else if (out_val[0] == '*') resting = REST_ALL_POINTS;

    /* Rest until HP or SP filled */
    else if (out_val[0] == '!') resting = REST_SOME_POINTS;

    /* Rest some */
    else
    {
        resting = atoi(out_val);
        if (resting <= 0) return;
    }

    /* Paranoia */
    if (resting > 9999) resting = 9999;

    Send_rest(resting);
}


/*
 * Look command
 */
void do_cmd_look(void)
{
    cmd_target_interactive(TARGET_LOOK);
}


/*
 * Target command
 */
void do_cmd_target(void)
{
    cmd_target_interactive(TARGET_KILL);
} 


void do_cmd_target_friendly(void)
{
    cmd_target_interactive(TARGET_HELP);
} 


void do_cmd_target_closest(void)
{
    Send_target_closest(TARGET_KILL);
}


void textui_cmd_fire_at_nearest(void)
{
    /* Send it */
    Send_fire_at_nearest();
}


void textui_cmd_throw(void)
{
    int item, dir;
    const char *q, *s;

    /* Get an item */
    q = "Throw which item? ";
    s = "You have nothing to throw.";
    if (!get_item(&item, q, s, CMD_THROW, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

    /* Make sure the player isn't throwing wielded items */
    if ((item >= inven_wield) && (item < quiver_start))
    {
        c_msg_print("You cannot throw wielded items.");
        return;
    }

    /* Get a direction (or cancel) */
    if (!get_aim_dir(&dir)) return;

    /* Send it */
    Send_throw(item, dir);
}


/* Item management commands */
void do_cmd_equip(void)
{
    int item;
    int ret = 3;

    /* No context menu while shopping */
    if (shopping)
    {
        /* Save the screen */
        screen_save();

        /* Hack -- Show empty slots */
        item_tester_full = TRUE;

        show_equip(OLIST_WEIGHT);

        /* Hack -- Hide empty slots */
        item_tester_full = FALSE;

        inkey_ex();

        /* Restore the screen */
        screen_load(TRUE);
    }
    else
    {
        /* Hack -- Show empty slots */
        item_tester_full = TRUE;

        /* Loop this menu until an object context menu says differently */
        while (ret == 3)
        {
            /* Get an item to use a context command on (Display the equipment) */
            if (get_item(&item, "Select Item:", NULL, CMD_NULL,
                USE_EQUIP | USE_INVEN | USE_FLOOR | START_EQUIP))
            {
                char o_name[NORMAL_WID];
                object_type *o_ptr = object_from_item_idx(item, o_name, sizeof(o_name));

                if (o_ptr && o_ptr->kind)
                    while ((ret = context_menu_object(o_ptr, item, o_name)) == 2);
            }
            else
                ret = -1;
        }

        /* Hack -- Hide empty slots */
        item_tester_full = FALSE;
    }
}


void do_cmd_inven(void)
{
    int item;
    int ret = 3;
    int diff = p_ptr->inven_cnt;
    char buf[NORMAL_WID];

    /* No context menu while shopping */
    if (shopping)
    {
        /* Save the screen */
        screen_save();

        /* Display the inventory */
        show_inven(OLIST_WEIGHT | OLIST_QUIVER);

        strnfmt(buf, sizeof(buf), "Burden %d.%d lb (%d.%d lb %s)",
            p_ptr->total_weight / 10, p_ptr->total_weight % 10,
            abs(diff) / 10, abs(diff) % 10, ((diff < 0)? "overweight": "remaining"));
        prt(buf, 0, 0);

        inkey_ex();

        /* Restore the screen */
        screen_load(TRUE);
    }
    else
    {
        /* Hack -- Show empty slots */
        item_tester_full = TRUE;

        /* Loop this menu until an object context menu says differently */
        while (ret == 3)
        {
            /* Prompt for a command */
            strnfmt(buf, sizeof(buf), "Burden %d.%d lb (%d.%d lb %s). Select Item: ",
                p_ptr->total_weight / 10, p_ptr->total_weight % 10,
                abs(diff) / 10, abs(diff) % 10, ((diff < 0)? "overweight": "remaining"));
            prt(buf, 0, 0);

            /* Get an item to use a context command on (Display the inventory) */
            if (get_item(&item, NULL, NULL, CMD_NULL, USE_EQUIP | USE_INVEN | USE_FLOOR))
            {
                char o_name[NORMAL_WID];
                object_type *o_ptr = object_from_item_idx(item, o_name, sizeof(o_name));

                if (o_ptr && o_ptr->kind)
                    while ((ret = context_menu_object(o_ptr, item, o_name)) == 2);
            }
            else
                ret = -1;
        }

        /* Hack -- Hide empty slots */
        item_tester_full = FALSE;
    }
}


void textui_cmd_drop_gold(void)
{
    int amt = 1;
    char buf[NORMAL_WID];

    if (!p_ptr->au) return;

    /* Build the default */
    strnfmt(buf, sizeof(buf), "%d", amt);

    /* Ask for a quantity */
    if (!get_string("How much gold? ", buf, 10)) return;

    /* Extract a number */
    amt = atoi(buf);

    /* A star or letter means "all" */
    if ((buf[0] == '*') || isalpha((unsigned char)buf[0])) amt = p_ptr->au;

    /* A 'k' means "thousands" */
    else if (strchr(buf, 'k') || strchr(buf, 'K')) amt *= 1000;

    /* A 'm' means "millions" */
    else if (strchr(buf, 'm') || strchr(buf, 'M')) amt *= 1000000;

    /* Enforce the maximum */
    if (amt > p_ptr->au) amt = p_ptr->au;

    /* Enforce the minimum */
    if (amt < 0) amt = 0;

    /* Send it */
    if (amt) Send_drop_gold((s32b)amt);
}


void textui_cmd_destroy_menu(int item)
{
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    char out_val[160];

    o_ptr = object_from_item_idx(item, o_name, sizeof(o_name));
    if (!o_ptr->kind) return;

    /* Verify */
    strnfmt(out_val, sizeof(out_val), "Really ignore %s? ", o_name);
    if (!get_check(out_val)) return;

    /* Ask for destruction */
    strnfmt(out_val, sizeof(out_val), "Destroy %s instead? ", o_name);

    /* Send it */
    Send_destroy(item, get_check(out_val));
}


void textui_cmd_destroy(void)
{
    int item;
    const char *q = "Ignore which item? ";
    const char *s = "You have nothing to ignore.";

    /* Get an item */
    if (!get_item(&item, q, s, CMD_DESTROY, (USE_EQUIP | USE_INVEN | USE_FLOOR)))
        return;

    textui_cmd_destroy_menu(item);
}


void textui_cmd_toggle_ignore(void)
{
    Send_ignore();
}


/* Loop callback */
static void map_callback_begin(ui_event *cp)
{
    /* Wait until we get the whole thing */
    if (last_line_info == -1) cp->type = EVT_DONE;
}


/* Information access commands */
static void view_map_aux(byte mode)
{
    ui_event ke = EVENT_EMPTY;

    /* Hack -- If the screen is already icky, ignore this command */
    if (p_ptr->screen_icky) return;

    /* Save the screen */
    screen_save();

    /* Reset the line counter */
    last_line_info = -2;

    /* Send the request */
    Send_map(mode);

    /* Wait until we get the whole thing */
    while (last_line_info != -1)
    {
        /* Loop, looking for net input and responding to keypresses */
        ke = Net_loop(Term_inkey, map_callback_begin, NULL, SCAN_OFF);

        /* Check for user abort */
        if (is_exit(ke)) break;
    }

    /* Wait for a keypress, flush key buffer */
    if (!is_exit(ke))
    {
        inkey_ex();
        Term_flush();
    }

    /* Restore the screen */
    screen_load(TRUE);
}


void do_cmd_view_map(void)
{
    view_map_aux(0);
}


void do_cmd_wild_map(void)
{
    view_map_aux(1);
}


void do_cmd_itemlist(void)
{
    Send_objlist();
}


void do_cmd_monlist(void)
{
    Send_monlist();
}


void do_cmd_locate(void)
{
    int dir;
    ui_event ke;

    target_icky_screen = TRUE;

    /* Initialize */
    Send_locate(5);

    /* Show panels until done */
    while (1)
    {
        /* Assume no direction */
        dir = 0;

        /* Get a direction */
        while (!dir)
        {
            /* Get a command (or Cancel) */
            ke = inkey_ex();

            /* Check for cancel */
            if (is_exit(ke)) break;

            /* Extract direction */
            dir = target_dir(ke.key);

            /* Error */
            if (!dir) bell("Illegal direction for locate!");
        }

        /* No direction */
        if (!dir) break;

        /* Send the command */
        Send_locate(dir);
    }

    /* Done */
    Send_locate(0);

    /* Clear */
    c_msg_print(NULL);

    target_icky_screen = FALSE;
    if (full_icky_screen) Term_redraw();
}


void do_cmd_query_symbol(void)
{
    char buf[NORMAL_WID];

    /* Get a name or abort */
    buf[0] = '\0';
    if (!get_string("Enter (partial) monster name: ", buf, sizeof(buf))) return;

    Send_symbol(buf);
}


/*
 * Get a password from the user
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
static int cmd_changepass()
{
    char pass1[MAX_PASS_LEN];
    char pass2[MAX_PASS_LEN];
    ui_event ke;
    int res;

    pass1[0] = '\0';
    pass2[0] = '\0';

    res = get_string_ex("New password: ", pass1, sizeof(pass1), TRUE);
    if (res) return res;
    res = get_string_ex("Confirm it: ", pass2, sizeof(pass2), TRUE);
    if (res) return res;

    if (!strcmp(pass1, pass2))
    {
        MD5Password(pass1);
        Send_pass(pass1);
        prt(" Password changed [press any key]", 0, 0);
    }
    else
        prt(" Not matching [paused]", 0, 0);

    while (1)
    {
        ke = inkey_ex();
        return_on_abort(ke);
        if ((ke.type == EVT_KBRD) && ke.key.code) break;
    }

    return 0;
}


/* Display player mode */
static bool char_screen_mode = FALSE;


void do_cmd_change_name(void)
{
    ui_event ke;
    bool more = TRUE;

    /* Save the screen */
    screen_save();

    while (more)
    {
        /* Display player info */
        display_player_screen(char_screen_mode);

        /* Display message */
        prt("[ESC to quit, 'h' to change mode, 'p' to change password]", NORMAL_HGT - 1, 2);

        /* Wait for key */
        ke = inkey_ex();

        /* Check for quit */
        if (is_exit(ke)) break;

        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                /* Exit */
                case 'q':
                case 'Q': more = FALSE; break;

                /* Toggle */
                case 'h':
                case 'H': char_screen_mode = !char_screen_mode; break;

                /* Change password */
                case 'p':
                case 'P':
                {
                    if (cmd_changepass() == 1) more = FALSE;
                    break;
                }
            }
        }
    }

    /* Restore the screen */
    screen_load(TRUE);
}


static void do_cmd_interactive(int hook, const char *header)
{
    ui_event ke;

    /* Hack -- If the screen is already icky, ignore this command */
    if (p_ptr->screen_icky) return;

    /* Set the hook */
    special_line_type = hook;

    /* Set the header */
    my_strcpy(special_line_header[NTERM_WIN_OVERHEAD], header, sizeof(special_line_header[0]));

    /* Save the screen */
    screen_save();

    /* Send the request */
    Send_interactive(special_line_type, 0);

    /* Wait until we get the whole thing */
    while (1)
    {
        /* Wait for net input, or a key */
        ke = inkey_ex();
        if (is_exit(ke))
        {
            ke.type = EVT_KBRD;
            ke.key.code = ESCAPE;
            ke.key.mods = 0;
        }
        if ((ke.type != EVT_KBRD) || !ke.key.code) continue;

        /* Send the request */
        Send_interactive(special_line_type, ke.key.code);

        /* Check for user abort */
        if (is_exit(ke)) break;
    }

    /* Restore the screen */
    screen_load(TRUE);

    special_line_type = SPECIAL_FILE_NONE;
}


void do_cmd_help(void)
{
    do_cmd_interactive(SPECIAL_FILE_HELP, "Help");
}


/*
 * Show previous messages to the user
 *
 * The screen format uses line 0 and 23 for headers and prompts,
 * skips line 1 and 22, and uses line 2 thru 21 for old messages.
 *
 * This command shows you which commands you are viewing, and allows
 * you to "search" for strings in the recall.
 *
 * Note that messages may be longer than NORMAL_WID characters, but they are
 * displayed using "infinite" length, with a special sub-command to
 * "slide" the virtual display to the left or right.
 *
 * Attempt to only highlight the matching portions of the string.
 */
void do_cmd_messages(void)
{
    ui_event ke;
    bool more = TRUE;
    int i, j, n, q;
    int wid, hgt;
    char shower[NORMAL_WID] = "";

    /* Total messages */
    n = messages_num();

    /* Start on first message */
    i = 0;

    /* Start at leftmost edge */
    q = 0;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Enter "icky" mode */
    topline_icky = TRUE;

    /* Save screen */
    screen_save();

    /* Process requests until done */
    while (more)
    {
        /* Clear screen */
        Term_clear();

        /* Dump messages */
        for (j = 0; (j < hgt - 4) && (i + j < n); j++)
        {
            const char *msg;
            const char *str = message_str(i + j);
            byte attr = message_color(i + j);
            u16b count = message_count(i + j);

            if (count <= 1) msg = str;
            else msg = format("%s <%dx>", str, count);

            /* Hack: re-color message from string template */
            message_color_hack(msg, &attr);

            /* Apply horizontal scroll */
            msg = ((int)strlen(msg) >= q)? (msg + q): "";

            /* Dump the messages, bottom to top */
            Term_putstr(0, hgt - 3 - j, -1, attr, msg);

            /* Highlight "shower" */
            if (shower[0])
            {
                str = msg;

                /* Display matches */
                while ((str = my_stristr(str, shower)) != NULL)
                {
                    int len = strlen(shower);

                    /* Display the match */
                    Term_putstr(str - msg, hgt - 3 - j, len, TERM_YELLOW, str);

                    /* Advance */
                    str += len;
                }
            }
        }

        /* Display header */
        prt(format("Message recall (%d-%d of %d), offset %d", i, i + j - 1, n, q), 0, 0);

        /* Display prompt (not very informative) */
        if (shower[0])
            prt("[Movement keys to navigate, '-' for next, '=' to find]", hgt - 1, 0);
        else
            prt("[Movement keys to navigate, '=' to find, or ESCAPE to exit]", hgt - 1, 0);

        /* Get a command */
        ke = inkey_ex();

        /* Exit on Escape */
        if (is_exit(ke)) break;

        else if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                /* Find text */
                case '=':
                {
                    int res;

                    /* Get the string to find */
                    prt("Find: ", hgt - 1, 0);
                    res = askfor_ex(shower, sizeof(shower), NULL, FALSE);
                    if (res == 1) more = FALSE;
                    else if (!res)
                    {
                        /* Set to find */
                        ke.key.code = '-';
                    }

                    break;
                }

                /* Scroll left */
                case ARROW_LEFT:
                case '4':
                {
                    q = ((q >= wid / 2)? (q - wid / 2): 0);
                    break;
                }

                /* Scroll right */
                case ARROW_RIGHT:
                case '6':
                {
                    q = q + wid / 2;
                    break;
                }

                /* Recall 1 older message */
                case ARROW_UP:
                case '8':
                {
                    if (i + 1 < n) i += 1;
                    break;
                }

                /* Recall 1 newer message */
                case ARROW_DOWN:
                case '2':
                case KC_ENTER:
                {
                    i = ((i >= 1)? (i - 1): 0);
                    break;
                }

                /* Recall 20 older messages */
                case KC_PGUP:
                case 'p':
                case ' ':
                {
                    if (i + 20 < n) i += 20;
                    break;
                }

                /* Recall 20 newer messages */
                case KC_PGDOWN:
                case 'n':
                {
                    i = ((i >= 20)? (i - 20): 0);
                    break;
                }
            }

            /* Find the next item */
            if ((ke.key.code == '-') && shower[0])
            {
                s16b z;

                /* Scan messages */
                for (z = i + 1; z < n; z++)
                {
                    /* Search for it */
                    if (my_stristr(message_str(z), shower))
                    {
                        /* New location */
                        i = z;

                        /* Done */
                        break;
                    }
                }
            }
        }
    }

    /* Load screen */
    screen_load(TRUE);

    /* Leave "icky" mode */
    topline_icky = FALSE;
}


void do_cmd_message(void)
{
    char buf[60];
    bool ok;
    bool refocus_chat = FALSE;

#if !defined(USE_GCU) && !defined(USE_SDL)
    if (term_chat->user) refocus_chat = TRUE;
#endif

    /* Hack to just change the window focus in Windows client */
    if (refocus_chat)
        set_chat_focus();
    else
    {
        buf[0] = '\0';
        ok = get_string("Message: ", buf, sizeof(buf));
        if (ok && buf[0]) Send_msg(buf);
    }
}


/*
 * Chat channels
 *
 * "o" opens a channel:
 *    - (player name): send messages to a specific player only
 *    - #(channel name): open new channel, select existing channel, join channel
 *
 * "c" closes the current channel
 *    - (player name): send messages to all players again
 *    - #(channel name): leave channel
 */
void do_cmd_chat()
{
    struct keypress com;
    char buf[NORMAL_WID];

    if (!get_com("Chat command [n - next, p - previous, c - close, o - open]:", &com))
        return;

    switch (com.code)
    {
        case 'b':
        case 'p':
        case '4':
            cmd_chat_cycle(-1);
            break;
        case 'f':
        case 'n':
        case '6':
            cmd_chat_cycle(+1);
            break;
        case 'c':
        case 'l':
            cmd_chat_close(view_channel);
            break;
        case 'o':
        case 'j':
            buf[0] = '\0';
            if (get_string("Channel: ", buf, sizeof(buf)))
                Send_chan(buf);
            break;
    }
}


void do_cmd_party(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* We are now in party mode */
    party_mode = TRUE;

    /* Enter "icky" mode */
    topline_icky = TRUE;

    /* Save the screen */
    screen_save();

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Initialize buffer */
        buf[0] = '\0';

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Party commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Create a party");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) Add a player to party");
        Term_putstr(5, 6, -1, TERM_WHITE, "(3) Delete a player from party");
        Term_putstr(5, 7, -1, TERM_WHITE, "(4) Leave your current party");
        Term_putstr(5, 8, -1, TERM_WHITE, "(5) Specify player to attack");
        Term_putstr(5, 9, -1, TERM_WHITE, "(6) Make peace");

        /* Show current party status */
        Term_putstr(0, 13, -1, TERM_WHITE, party_info);

        /* Prompt */
        Term_putstr(0, 11, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        if (is_exit(ke)) break;

        else if (ke.type == EVT_KBRD)
        {
            /* Create party */
            if (ke.key.code == '1')
            {
                int res;

                /* Get party name */
                res = get_string_ex("Party name: ", buf, sizeof(buf), FALSE);
                if (!res) Send_party(PARTY_CREATE, buf);
                else if (res == 1) break;
            }

            /* Add player */
            else if (ke.key.code == '2')
            {
                int res;

                /* Get player name */
                res = get_string_ex("Add player: ", buf, sizeof(buf), FALSE);
                if (!res) Send_party(PARTY_ADD, buf);
                else if (res == 1) break;
            }

            /* Delete player */
            else if (ke.key.code == '3')
            {
                int res;

                /* Get player name */
                res = get_string_ex("Delete player: ", buf, sizeof(buf), FALSE);
                if (!res) Send_party(PARTY_DELETE, buf);
                else if (res == 1) break;
            }

            /* Leave party */
            else if (ke.key.code == '4')
            {
                /* Send the command */
                Send_party(PARTY_REMOVE_ME, "");
            }

            /* Attack player */
            else if (ke.key.code == '5')
            {
                int res;

                /* Get player name */
                res = get_string_ex("Player to attack: ", buf, sizeof(buf), FALSE);
                if (!res) Send_party(PARTY_HOSTILE, buf);
                else if (res == 1) break;
            }

            /* Make peace with player */
            else if (ke.key.code == '6')
            {
                int res;

                /* Get player name */
                res = get_string_ex("Make peace with: ", buf, sizeof(buf), FALSE);
                if (!res) Send_party(PARTY_PEACE, buf);
                else if (res == 1) break;
            }
        }

        /* Flush messages */
        c_msg_print(NULL);
    }

    /* Restore the screen */
    screen_load(TRUE);

    /* No longer in party mode */
    party_mode = FALSE;

    /* Leave "icky" mode */
    topline_icky = FALSE;
}


void do_cmd_describe(void)
{
    int item;
    const char *q, *s;
    char o_name[NORMAL_WID];

    /* Get an item */
    q = "Describe which item? ";
    s = "You have nothing to describe.";
    if (!get_item(&item, q, s, CMD_NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

    o_name[0] = '\0';
    object_from_item_idx(item, o_name, sizeof(o_name));

    if (o_name[0] != '\0') Send_msg(o_name);
}


void textui_cmd_suicide(void)
{
    struct keypress ch;

    /* Verify */
    if (!get_check("Do you really want to commit suicide? ")) return;

    /* Check again */
    prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
    flush();
    ch = inkey();
    prt("", 0, 0);
    if (ch.code != '@') return;

    /* Send it */
    Send_suicide();
}


void do_cmd_redraw(void)
{
    Send_redraw();
}


void do_cmd_purchase_house(void)
{
    int dir;

    if (!get_aim_dir(&dir)) return;

    /* Send it */
    Send_purchase_house(dir);
}


void do_cmd_quest(void)
{
    Send_quest();
}


/* Commands that shouldn't be shown to the user */
void do_cmd_pref(void)
{
    char buf[NORMAL_WID];

    buf[0] = '\0';
    if (get_string("Action: ", buf, sizeof(buf)))
        process_pref_file_command(buf);
}


static int cmd_master_aux_level(void)
{
    ui_event ke;

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Level commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Static your current level");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) Unstatic your current level");
        Term_putstr(5, 6, -1, TERM_WHITE, "(3) Enter manual design (new level)");
        Term_putstr(5, 7, -1, TERM_WHITE, "(4) Enter manual design");
        Term_putstr(5, 8, -1, TERM_WHITE, "(5) Exit manual design");

        /* Prompt */
        Term_putstr(0, 11, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Static the current level */
            if (ke.key.code == '1')
                Send_master(MASTER_LEVEL, "s");

            /* Unstatic the current level */
            else if (ke.key.code == '2')
                Send_master(MASTER_LEVEL, "u");

            /* Enter manual design (new level) */
            else if (ke.key.code == '3')
                Send_master(MASTER_LEVEL, "w");

            /* Enter manual design */
            else if (ke.key.code == '4')
                Send_master(MASTER_LEVEL, "m");

            /* Exit manual design */
            else if (ke.key.code == '5')
                Send_master(MASTER_LEVEL, "x");
        }

        /* Flush messages */
        c_msg_print(NULL);
    }
}


static int cmd_master_aux_generate_item(void)
{
    ui_event ke;
    char buf[NORMAL_WID];
    s32b tmp_quan;

    /* Clear screen */
    Term_clear();

    /* Inform server about cleared screen */
    Send_master(MASTER_GENERATE, "ir");

    /* Process requests until done */
    while (1)
    {
        /* Initialize buffer */
        buf[0] = 'i';
        buf[1] = '\0';

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Generate Item");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(k/e) Item/ego by number");
        Term_putstr(5, 5, -1, TERM_WHITE, "(K/E) Item/ego by name");
        Term_putstr(5, 6, -1, TERM_WHITE, "(+/*) Next item/ego");
        Term_putstr(5, 7, -1, TERM_WHITE, "(-//) Previous item/ego");

        Term_putstr(30, 4, -1, TERM_WHITE, "(h/H) Increment/decrement to-hit");
        Term_putstr(30, 5, -1, TERM_WHITE, "(d/D) Increment/decrement to-dam");
        Term_putstr(30, 6, -1, TERM_WHITE, "(a/A) Increment/decrement to-ac");
        Term_putstr(30, 7, -1, TERM_WHITE, "(n/N) Increment/decrement pval number");
        Term_putstr(30, 8, -1, TERM_WHITE, "(p/P) Increment/decrement current pval");
        Term_putstr(30, 9, -1, TERM_WHITE, "(x/X) Increment/decrement extra power");
        Term_putstr(30, 10, -1, TERM_WHITE, "(y/Y) Increment/decrement extra power");
        Term_putstr(30, 11, -1, TERM_WHITE, "(m/M) Increment/decrement extra dice");
        Term_putstr(30, 12, -1, TERM_WHITE, "(i/I) Increment/decrement identify status");

        Term_putstr(5, 14, -1, TERM_WHITE, "(g) Generate");

        /* Prompt */
        Term_putstr(0, 16, -1, TERM_WHITE, "Command: ");

        Term_putstr(0, 18, -1, TERM_WHITE, "Selection: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            if (ke.key.code == 'k')
            {
                /* Item by number */
                buf[1] = 'k';
                buf[2] = '#';
                tmp_quan = get_quantity_ex("Item number? ", z_info->k_max);
                if (tmp_quan == -1) return 1;
                if (!tmp_quan) continue;
                buf[4] = 0;
                buf[5] = 0;
                if (tmp_quan > 255)
                {
                    tmp_quan -= 255;
                    buf[4] = 255;
                }
                if (tmp_quan > 255)
                {
                    tmp_quan -= 255;
                    buf[5] = 255;
                }
                buf[3] = tmp_quan;
            }
            else if (ke.key.code == 'e')
            {
                /* Ego by number */
                buf[1] = 'e';
                buf[2] = '#';
                buf[3] = get_quantity_ex("Ego number? ", z_info->e_max);
                if (buf[3] == -1) return 1;
                if (!buf[3]) continue;
            }
            else if (ke.key.code == 'K')
            {
                int res;

                /* Item by name */
                buf[1] = 'k';
                buf[2] = 'n';
                res = get_string_ex("Item name: ", &buf[3], sizeof(buf), FALSE);
                if (res == 1) return 1;
                if ((res == 2) || !buf[3]) continue;
            }
            else if (ke.key.code == 'E')
            {
                int res;

                /* Ego by name */
                buf[1] = 'e';
                buf[2] = 'n';
                res = get_string_ex("Ego name: ", &buf[3], sizeof(buf), FALSE);
                if (res == 1) return 1;
                if ((res == 2) || !buf[3]) continue;
            }
            else if (ke.key.code == '+')
            {
                /* Next Item */
                buf[1] = 'k';
                buf[2] = '+';
            }
            else if (ke.key.code == '*')
            {
                /* Next Ego */
                buf[1] = 'e';
                buf[2] = '+';
            }
            else if (ke.key.code == '-')
            {
                /* Prev. Item */
                buf[1] = 'k';
                buf[2] = '-';
            }
            else if (ke.key.code == '/')
            {
                /* Prev. Ego */
                buf[1] = 'e';
                buf[2] = '-';
            }
            else if (strchr("HDANPXYMI", ke.key.code))
            {
                /* Decrement value */
                buf[1] = 'M';
                buf[2] = tolower((unsigned char)ke.key.code);
            }
            else if (strchr("hdanpxymi", ke.key.code))
            {
                /* Increment value */
                buf[1] = 'I';
                buf[2] = ke.key.code;
            }
            else if (ke.key.code == 'g')
            {
                /* Generate */
                buf[1] = 'd';
                buf[2] = get_quantity_ex("How many? ", 127);
                if (buf[2] == -1) return 1;
                if (!buf[2]) continue;
            }
            else
            {
                bell("Illegal command!");
                continue;
            }
        }
        else
        {
            bell("Illegal command!");
            continue;
        }

        /* Clear screen again */
        Term_clear();

        /* Send choice to server */
        Send_master(MASTER_GENERATE, buf);

        /* Flush messages */
        c_msg_print(NULL);
    }
}


static int cmd_master_aux_generate_vault(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Initialize buffer */
        buf[0] = 'v';

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Generate Vault");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) By number");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) By name");

        /* Prompt */
        Term_putstr(0, 8, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Generate by number (can be zero) */
            if (ke.key.code == '1')
            {
                int amt = 0, res;
                char tmp[NORMAL_WID];

                buf[1] = '#';
                strnfmt(tmp, sizeof(tmp), "Vault number (0-%d): ", v_max - 1);
                strnfmt(&buf[2], sizeof(buf), "%d", amt);
                res = get_string_ex(tmp, &buf[2], sizeof(buf), FALSE);
                if (res == 1) return 1;
                if (res == 2) continue;
                amt = atoi(&buf[2]);
                if (amt > v_max - 1) amt = v_max - 1;
                if (amt < 0) amt = 0;
                buf[2] = amt;
                buf[3] = '\0';
            }

            /* Generate by name */
            else if (ke.key.code == '2')
            {
                int res;

                buf[1] = 'n';
                res = get_string_ex("Vault name: ", &buf[2], sizeof(buf), FALSE);
                if (res == 1) return 1;
                if ((res == 2) || !buf[2]) continue;
            }

            else
            {
                bell("Illegal command!");
                continue;
            }
        }
        else
        {
            bell("Illegal command!");
            continue;
        }

        Send_master(MASTER_GENERATE, buf);

        /* Flush messages */
        c_msg_print(NULL);
    }
}


static int cmd_master_aux_generate(void)
{
    ui_event ke;

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Generation commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Vault");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) Item");

        /* Prompt */
        Term_putstr(0, 7, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Generate a vault */
            if (ke.key.code == '1')
            {
                if (cmd_master_aux_generate_vault() == 1) return 1;
            }

            /* Generate an item */
            else if (ke.key.code == '2')
            {
                if (cmd_master_aux_generate_item() == 1) return 1;
            }
        }

        /* Flush messages */
        c_msg_print(NULL);
    }
}


static int cmd_master_aux_build(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Initialize buffer */
        memset(buf, 0, NORMAL_WID);

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Building commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Set Feature");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) Place Feature");
        Term_putstr(5, 6, -1, TERM_WHITE, "(3) Draw Line");
        Term_putstr(5, 7, -1, TERM_WHITE, "(4) Fill Rectangle");
        Term_putstr(5, 8, -1, TERM_WHITE, "(5) Build Mode On");
        Term_putstr(5, 9, -1, TERM_WHITE, "(6) Build Mode Off");

        /* Prompt */
        Term_putstr(0, 12, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Set Feature */
            if (ke.key.code == '1')
            {
                buf[0] = 'i';
                buf[1] = get_quantity_ex("Feature number? ", z_info->f_max);
                if (buf[1] == -1) return 1;
            }

            /* Place Feature */
            else if (ke.key.code == '2')
                buf[0] = 'f';

            /* Draw Line */
            else if (ke.key.code == '3')
            {
                int dir, res;

                res = get_aim_dir_ex(&dir);
                if (res == 1) return 1;
                if (res == 2) continue;

                buf[0] = 'l';
                buf[1] = (char)dir;
            }

            /* Fill Rectangle */
            else if (ke.key.code == '4')
                buf[0] = 'r';

            /* Build Mode On */
            else if (ke.key.code == '5')
                buf[0] = 'm';

            /* Build Mode Off */
            else if (ke.key.code == '6')
                buf[0] = 'x';

            else
            {
                bell("Illegal command!");
                continue;
            }
        }
        else
        {
            bell("Illegal command!");
            continue;
        }

        /* If we got a valid command, send it */
        if (buf[0]) Send_master(MASTER_BUILD, buf);

        /* Flush messages */
        c_msg_print(NULL);
    }
}


static int cmd_master_aux_summon_type(char *buf)
{
    ui_event ke;

    /* Get how it should be summoned */
    while (1)
    {
        /* Make sure we get a valid summoning type before summoning */
        buf[0] = 0;

        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Summon...");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) X here");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) X at random locations");
        Term_putstr(5, 6, -1, TERM_WHITE, "(3) Group here");
        Term_putstr(5, 7, -1, TERM_WHITE, "(4) Group at random location");
        Term_putstr(5, 8, -1, TERM_WHITE, "(5) Summoning mode");

        /* Prompt */
        Term_putstr(0, 10, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* X here */
            if (ke.key.code == '1')
            {
                buf[0] = 'x';
                buf[1] = get_quantity_ex("Summon how many? ", 127);
                if (buf[1] == -1) return 1;
                if (!buf[1]) continue;
            }

            /* X in different places */
            else if (ke.key.code == '2')
            {
                buf[0] = 'X';
                buf[1] = get_quantity_ex("Summon how many? ", 127);
                if (buf[1] == -1) return 1;
                if (!buf[1]) continue;
            }

            /* Group here */
            else if (ke.key.code == '3')
                buf[0] = 'g';

            /* Group at random location */
            else if (ke.key.code == '4')
                buf[0] = 'G';

            /* Summoning mode on */
            else if (ke.key.code == '5')
            {
                buf[0] = 'T';
                buf[1] = 1;
            }

            else
            {
                bell("Illegal command!");
                continue;
            }
        }
        else
        {
            bell("Illegal command!");
            continue;
        }

        /* If we have a valid summoning type, then summon the monster */
        if (buf[0]) Send_master(MASTER_SUMMON, buf);
    }
}


static int cmd_master_aux_summon(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Summon...");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Depth");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) Specific");
        Term_putstr(5, 6, -1, TERM_WHITE, "(3) Mass Banishment");
        Term_putstr(5, 7, -1, TERM_WHITE, "(4) Summoning mode off");

        /* Prompt */
        Term_putstr(0, 10, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Summon from a specific depth */
            if (ke.key.code == '1')
            {
                buf[2] = 'd';
                buf[3] = get_quantity_ex("Summon from which depth? ", 127);
                if (buf[3] == -1) return 1;
                if (!buf[3]) continue;
                buf[4] = 0;
            }

            /* Summon a specific monster or character */
            else if (ke.key.code == '2')
            {
                int res;

                buf[2] = 's';
                buf[3] = 0;
                res = get_string_ex("Enter (partial) monster name: ", &buf[3], sizeof(buf),
                    FALSE);
                if (res == 1) return 1;
                if ((res == 2) || !buf[3]) continue;
            }

            /* Delete all the monsters near us (turn summoning mode on) */
            else if (ke.key.code == '3')
            {
                buf[0] = 'T';
                buf[1] = 1;
                buf[2] = '0';
                buf[3] = '\0'; /* Null terminate the monster name */
                Send_master(MASTER_SUMMON, buf);

                continue;
            }

            /* Disable summoning mode */
            else if (ke.key.code == '4')
            {
                buf[0] = 'F';
                buf[3] = '\0'; /* Null terminate the monster name */
                Send_master(MASTER_SUMMON, buf);

                continue;
            }

            else
            {
                bell("Illegal command!");
                continue;
            }
        }
        else
        {
            bell("Illegal command!");
            continue;
        }

        /* Get summoning type */
        if (cmd_master_aux_summon_type(buf) == 1) return 1;

        /* Flush messages */
        c_msg_print(NULL);
    }
}


static int cmd_master_aux_player_inside(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Inform about screen update */
        Send_master(MASTER_PLAYER, ">r");

        /* Clear screen */
        Term_clear();

        /* Initialize buffer */
        buf[0] = '\0';

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Player commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(<) Previous");
        Term_putstr(5, 5, -1, TERM_WHITE, "(>) Next");
        Term_putstr(5, 6, -1, TERM_WHITE, "RET Change");

        Term_putstr(5, 8, -1, TERM_WHITE, "(g) Ghost");
        Term_putstr(5, 9, -1, TERM_WHITE, "(w) Wizard");

        /* Prompt */
        Term_putstr(0, 15, -1, TERM_WHITE, "Selection: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Prev Sel */
            if (ke.key.code == '<')
            {
                buf[0] = '>';
                buf[1] = 'p';
            }

            /* Next Sel */
            else if (ke.key.code == '>')
            {
                buf[0] = '>';
                buf[1] = 'n';
            }

            /* Change */
            else if (ke.key.code == KC_ENTER)
            {
                buf[0] = '>';
                buf[1] = 'x';
            }

            /* Toggle Ghost */
            else if (ke.key.code == 'g')
            {
                buf[0] = '>';
                buf[1] = 'g';
            }

            /* Toggle Wizard */
            else if (ke.key.code == 'w')
            {
                buf[0] = '>';
                buf[1] = 'w';
            }

            else
            {
                bell("Illegal command!");
                continue;
            }
        }
        else
        {
            bell("Illegal command!");
            continue;
        }

        if (!STRZERO(buf))
        {
            buf[2] = '\0';
            Send_master(MASTER_PLAYER, buf);
        }
    }
}


static int cmd_master_aux_player(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Initialize buffer */
        buf[0] = '\0';

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Player commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Self");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) By Name");

        /* Prompt */
        Term_putstr(0, 8, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Self */
            if (ke.key.code == '1')
            {
                Send_master(MASTER_PLAYER, " ");
                if (cmd_master_aux_player_inside() == 1) return 1;
            }

            /* Get player name */
            else if (ke.key.code == '2')
            {
                int res = get_string_ex("Player: ", buf, sizeof(buf), FALSE);

                if (res == 1) return 1;
                if ((res == 2) || !buf[0]) continue;

                Send_master(MASTER_PLAYER, buf);
                if (cmd_master_aux_player_inside() == 1) return 1;
            }
        }
    }
}


static int cmd_master_aux_visuals(void)
{
    ui_event ke;

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Visual commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Display GF_XXX types");

        /* Prompt */
        Term_putstr(0, 8, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        /* Display GF_XXX types */
        if ((ke.type == EVT_KBRD) && (ke.key.code == '1'))
            Send_master(MASTER_VISUALS, " ");
    }
}


static int cmd_master_aux_orders(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Inform about screen update */
        Send_master(MASTER_ORDER, ">r");

        /* Clear screen */
        Term_clear();

        /* Initialize buffer */
        buf[0] = '\0';

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Manage XBM orders");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(<) Previous");
        Term_putstr(5, 5, -1, TERM_WHITE, "(>) Next");

        Term_putstr(5, 7, -1, TERM_WHITE, "(c) Cancel order");

        /* Prompt */
        Term_putstr(0, 15, -1, TERM_WHITE, "Selection: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Prev Sel */
            if (ke.key.code == '<')
            {
                buf[0] = '>';
                buf[1] = 'p';
            }

            /* Next Sel */
            else if (ke.key.code == '>')
            {
                buf[0] = '>';
                buf[1] = 'n';
            }

            /* Cancel order */
            else if (ke.key.code == 'c')
            {
                buf[0] = '>';
                buf[1] = 'c';
            }

            else
            {
                bell("Illegal command!");
                continue;
            }
        }
        else
        {
            bell("Illegal command!");
            continue;
        }

        if (!STRZERO(buf))
        {
            buf[2] = '\0';
            Send_master(MASTER_ORDER, buf);
        }
    }
}


void do_cmd_master(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* We are now in party mode */
    party_mode = TRUE;

    /* Enter "icky" mode */
    topline_icky = TRUE;

    /* Save the screen */
    screen_save();

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Initialize buffer */
        buf[0] = '\0';

        /* Describe */
        Term_putstr(0, 2, -1, TERM_WHITE, "Dungeon Master commands");

        /* Selections */
        Term_putstr(5, 4, -1, TERM_WHITE, "(1) Level Commands");
        Term_putstr(5, 5, -1, TERM_WHITE, "(2) Building Commands");
        Term_putstr(5, 6, -1, TERM_WHITE, "(3) Summoning Commands");
        Term_putstr(5, 7, -1, TERM_WHITE, "(4) Generation Commands");
        Term_putstr(5, 8, -1, TERM_WHITE, "(5) Player Commands");
        Term_putstr(5, 9, -1, TERM_WHITE, "(6) Visual Commands");
        Term_putstr(5, 10, -1, TERM_WHITE, "(7) Manage XBM orders");

        /* Prompt */
        Term_putstr(0, 13, -1, TERM_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        if (is_exit(ke)) break;

        if (ke.type == EVT_KBRD)
        {
            /* Level commands */
            if (ke.key.code == '1')
            {
                if (cmd_master_aux_level() == 1) break;
            }

            /* Build commands */
            else if (ke.key.code == '2')
            {
                if (cmd_master_aux_build() == 1) break;
            }

            /* Summon commands */
            else if (ke.key.code == '3')
            {
                if (cmd_master_aux_summon() == 1) break;
            }

            /* Generate commands */
            else if (ke.key.code == '4')
            {
                if (cmd_master_aux_generate() == 1) break;
            }

            /* Player commands */
            else if (ke.key.code == '5')
            {
                if (cmd_master_aux_player() == 1) break;
            }

            /* Visual commands */
            else if (ke.key.code == '6')
            {
                if (cmd_master_aux_visuals() == 1) break;
            }

            /* Manage XBM orders */
            else if (ke.key.code == '7')
            {
                if (cmd_master_aux_orders() == 1) break;
            }
        }

        /* Flush messages */
        c_msg_print(NULL);
    }

    /* Restore the screen */
    screen_load(TRUE);

    /* No longer in party mode */
    party_mode = FALSE;

    /* Leave "icky" mode */
    topline_icky = FALSE;
}


void do_cmd_social(void)
{
    int dir, k;
    bool found = FALSE;
    social_type *s_ptr;
    char buf[NORMAL_WID];
    memset(buf, 0, NORMAL_WID);

    /* Build the default */
    my_strcpy(buf, "*", sizeof(buf));

    /* Get a number or abort */
    if (!get_string("Enter action (* for list): ", buf, sizeof(buf))) return;

    /* Handle list */
    if (buf[0] == '*')
    {
        /* Set the hook */
        special_line_type = SPECIAL_FILE_SOCIALS;

        /* Set the header */
        my_strcpy(special_line_header[NTERM_WIN_OVERHEAD], "Socials", sizeof(special_line_header[0]));

        /* Call the file perusal */
        peruse_file();

        return;
    }

    /* Scan the socials */
    for (k = 0; k < z_info->soc_max; k++)
    {
        s_ptr = &soc_info[k];

        if (streq(s_ptr->name, buf))
        {
            found = TRUE;
            break;
        }
    }

    /* Not a valid action */
    if (!found)
    {
        c_msg_print("Not a valid action.");
        return;
    }

    /* Target is allowed, not required */
    if (s_ptr->target) get_aim_dir(&dir);

    Send_social(buf, dir);
}


void do_cmd_feeling(void)
{
    Send_feeling();
}


void do_cmd_fountain(void)
{
    int item;

    /* Check for empty bottles */
    for (item = 0; item < inven_pack; item++)
    {
        if (p_ptr->inventory[item].tval == TV_BOTTLE) break;
    }
    if (item == inven_pack) item = -1;
    else
    {
        /* Drink or refill? */
        if (!get_check("Do you want to fill a bottle? ")) item = -1;
    }

    /* Send it */
    Send_fountain(item);
}


void do_cmd_center_map(void)
{
    Send_center_map();
}
