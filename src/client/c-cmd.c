/*
 * File: c-cmd.c
 * Purpose: Deal with command processing
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


/*** Command functions ***/


void textui_cmd_poly(void)
{
    char buf[NORMAL_WID];
    int number;

    /* Non mimics */
    if (!player_has(player, PF_SHAPECHANGE))
    {
        c_msg_print("You are too solid.");
        return;
    }

    /* Build the default */
    my_strcpy(buf, "*", sizeof(buf));

    /* Get a number/name or abort */
    if (!get_string("Enter race index/symbol/partial name (0 for player, * for all): ", buf,
        sizeof(buf)))
    {
        return;
    }

    /* Extract a number */
    number = atoi(buf);

    /* Check bounds, default to all */
    if ((number < 0) || (number > z_info->r_max - 1))
    {
        number = 0;
        my_strcpy(buf, "*", sizeof(buf));
    }

    /* Symbol/partial name/all: display a list */
    if ((number == 0) && (buf[0] != '0'))
    {
        Send_poly_race(buf);

        /* Set the hook */
        special_line_type = SPECIAL_FILE_POLY;

        /* Set the header */
        my_strcpy(special_line_header[NTERM_WIN_OVERHEAD],
            (player_has(player, PF_MONSTER_SPELLS)? "Killed List": "Forms"),
            sizeof(special_line_header[0]));

        /* Call the file perusal */
        peruse_file();

        return;
    }

    /* Race index: polymorph into that race */
    Send_poly(number);
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


void do_cmd_fire_at_nearest(void)
{
    /* Send it */
    Send_fire_at_nearest();
}


void textui_cmd_drop_gold(void)
{
    int amt = 1;
    char buf[NORMAL_WID];

    if (!player->au) return;

    /* Build the default */
    strnfmt(buf, sizeof(buf), "%d", amt);

    /* Ask for a quantity */
    if (!get_string("How much gold? ", buf, 10)) return;

    /* Extract a number */
    amt = atoi(buf);

    /* A star or letter means "all" */
    if ((buf[0] == '*') || isalpha((unsigned char)buf[0])) amt = player->au;

    /* A 'k' means "thousands" */
    else if (strchr(buf, 'k') || strchr(buf, 'K')) amt *= 1000;

    /* A 'm' means "millions" */
    else if (strchr(buf, 'm') || strchr(buf, 'M')) amt *= 1000000;

    /* Enforce the maximum */
    if (amt > player->au) amt = player->au;

    /* Enforce the minimum */
    if (amt < 0) amt = 0;

    /* Send it */
    if (amt) Send_drop_gold((s32b)amt);
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

    /* Hack -- if the screen is already icky, ignore this command */
    if (player->screen_save_depth) return;

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
        ke = Net_loop(Term_inkey, map_callback_begin, NULL, SCAN_OFF, true);

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
    screen_load(true);
}


void do_cmd_view_map(void)
{
    view_map_aux(0);
}


void do_cmd_wild_map(void)
{
    view_map_aux(1);
}


static void get_printable_history(char printable[N_HIST_LINES][N_HIST_WRAP], char *history)
{
    int i, n;
    char *s, *t;
    char buf[N_HIST_LINES * (N_HIST_WRAP - 1) + 1];

    /* Make a local copy */
    my_strcpy(buf, history, sizeof(buf));

    /* Clear */
    for (i = 0; i < N_HIST_LINES; i++) printable[i][0] = '\0';

    /* Set pointer */
    t = &buf[0];

    /* Skip leading spaces */
    for (s = buf; *s == ' '; s++) /* loop */;

    /* Get apparent length */
    n = strlen(s);

    /* Kill trailing spaces */
    while ((n > 0) && (s[n - 1] == ' ')) s[--n] = '\0';

    /* Start at first line */
    i = 0;

    /* Collect the history */
    while (true)
    {
        /* Extract remaining length */
        n = strlen(s);

        /* All done */
        if (n < N_HIST_WRAP)
        {
            /* Save one line of history */
            my_strcpy(printable[i++], s, sizeof(printable[0]));

            /* All done */
            break;
        }

        /* Find a reasonable break-point */
        for (n = N_HIST_WRAP - 1; ((n > 0) && (s[n - 1] != ' ')); n--) /* loop */;

        /* Save next location */
        t = s + n;

        /* Save one line of history */
        my_strcpy(printable[i++], s, n + 1);

        /* Start next line */
        s = t;
    }
}


static void get_screen_loc(int cursor, int *x, int *y, int n_lines, int *line_starts,
    int *line_lengths)
{
    int i, lengths_so_far = 0;

    if (!line_starts || !line_lengths) return;

    for (i = 0; i < n_lines; i++)
    {
        /* Allow one extra character on the last line to expand the buffer */
        if (((cursor >= line_starts[i]) && (cursor < (line_starts[i] + line_lengths[i]))) ||
            (i == n_lines - 1))
        {
            *y = i;
            *x = cursor - lengths_so_far;
            break;
        }

        lengths_so_far += line_lengths[i];
    }
}


/*
 * Modify character history
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
static int cmd_changehistory(void)
{
    int len = 0;
    bool done = false;
    int i, cursor = 0;
    char history[N_HIST_LINES * (N_HIST_WRAP - 1) + 1];
    char printable[N_HIST_LINES][N_HIST_WRAP];

    /* Put the original history in a linear buffer */
    memset(history, 0, sizeof(history));
    for (i = 0; i < N_HIST_LINES; i++)
    {
        if (STRZERO(player->history[i])) break;
        if (i == 0) my_strcpy(history, player->history[i], sizeof(history));
        else my_strcat(history, player->history[i], sizeof(history));
        len += strlen(player->history[i]);
    }

    /* Prompt */
    prt("Press ENTER to accept character history (or hit ESCAPE to discard any change):", 0, 0);

    while (!done)
    {
        int x = 0, y = 0;
        ui_event ke;
        int line_starts[N_HIST_LINES], line_lengths[N_HIST_LINES];
        int n_lines = 0;

        /* Display on screen */
        clear_from(19);
        get_printable_history(printable, history);
        for (i = 0; i < N_HIST_LINES; i++)
            Term_putstr(1, i + 19, -1, COLOUR_WHITE, printable[i]);
        prt("[left/right/down/up/END/HOME to move, BACKSPACE/DELETE/any character to edit]",
            NORMAL_HGT - 1, 2);

        for (i = 0; i < N_HIST_LINES; i++)
        {
            if (STRZERO(printable[i])) break;
            line_starts[i] = ((i > 0)? (line_starts[i - 1] + line_lengths[i - 1]): 0);
            line_lengths[i] = strlen(printable[i]);
            n_lines++;
        }

        /* Set cursor to current editing position */
        get_screen_loc(cursor, &x, &y, n_lines, line_starts, line_lengths);
        Term_gotoxy(1 + x, 19 + y);

        ke = inkey_ex();
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                case KC_ENTER:
                {
                    /* Overwrite the original history */
                    for (i = 0; i < N_HIST_LINES; i++)
                        my_strcpy(player->history[i], printable[i], sizeof(player->history[0]));

                    /* Update */
                    for (i = 0; i < N_HIST_LINES; i++)
                        Send_history(i, player->history[i]);

                    done = true;
                    break;
                }

                case ARROW_LEFT:
                {
                    if (cursor > 0) cursor--;
                    break;
                }

                case ARROW_RIGHT:
                {
                    if (cursor < len) cursor++;
                    break;
                }

                case ARROW_DOWN:
                {
                    if (y != n_lines - 1) cursor += line_lengths[y];
                    break;
                }

                case ARROW_UP:
                {
                    if (y != 0) cursor -= line_lengths[y - 1];
                    break;
                }

                case KC_END:
                {
                    cursor = len;
                    break;
                }

                case KC_HOME:
                {
                    cursor = 0;
                    break;
                }

                case KC_BACKSPACE:
                {
                    /* Refuse to backspace into oblivion */
                    if (cursor == 0) break;

                    /* Move the string to the left by 1 */
                    memmove(&history[cursor - 1], &history[cursor], len - cursor);

                    /* Decrement */
                    cursor--;
                    len--;

                    /* Terminate */
                    history[len] = '\0';

                    break;
                }

                case KC_DELETE:
                {
                    /* Refuse to delete nothing */
                    if (cursor == len) break;

                    /* Move the string to the left by 1 */
                    memmove(&history[cursor], &history[cursor + 1], len - cursor - 1);

                    /* Decrement */
                    len--;

                    /* Terminate */
                    history[len] = '\0';

                    break;
                }

                default:
                {
                    if (!isprint(ke.key.code)) break;

                    /* Make sure we have enough room for a new character */
                    if (len == N_HIST_LINES * (N_HIST_WRAP - 1)) break;

                    /* Don't overflow the printable area */
                    if ((y == N_HIST_LINES - 1) && (x == N_HIST_WRAP - 1)) break;

                    /* Move the rest of the buffer along to make room */
                    if (history[cursor] != 0)
                        memmove(&history[cursor + 1], &history[cursor], len - cursor);

                    /* Insert the character */
                    history[cursor++] = (char)ke.key.code;
                    len++;

                    /* Terminate */
                    history[len] = '\0';
                }
            }
        }
    }

    return 0;
}


/* Display player mode */
static byte char_screen_mode = 0;


/*
 * Hack -- change name
 *
 * PWMAngband: character name cannot be changed; instead, you can generate a local character dump,
 * or even modify character history
 */
void do_cmd_change_name(void)
{
    ui_event ke;
    const char *p;
    bool more = true;

    /* Prompt */
    p = "['h'/'m' to change history/mode, 'f' to file, or ESC]";

    /* Save screen */
    screen_save();

    /* Forever */
    while (more)
    {
        /* Display the player */
        display_player_screen(char_screen_mode);

        /* Prompt */
        prt(p, NORMAL_HGT - 1, 2);

        /* Query */
        ke = inkey_ex();

        /* Check for quit */
        if (is_exit(ke)) break;

        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                /* Character dump */
                case 'f':
                case 'F': Send_char_dump(); break;

                /* Modify history */
                case 'h':
                case 'H':
                {
                    if (cmd_changehistory() == 1) more = false;
                    break;
                }

                /* Toggle */
                case 'm':
                case 'M':
                {
                    char_screen_mode++;
                    if (char_screen_mode == 3) char_screen_mode = 0;
                    break;
                }

                /* Exit */
                case 'q':
                case 'Q': more = false; break;
            }
        }
    }

    /* Load screen */
    screen_load(true);
}


static void do_cmd_interactive(int hook, const char *header)
{
    ui_event ke;

    /* Hack -- if the screen is already icky, ignore this command */
    if (player->screen_save_depth) return;

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
    screen_load(true);

    special_line_type = SPECIAL_FILE_NONE;
}


void do_cmd_help(void)
{
    do_cmd_interactive(SPECIAL_FILE_HELP, "Help");
}


void send_msg_chunks(char *pmsgbuf, int msglen)
{
    char pmsg[60];
    char nickbuf[30];
    int offset = 0, breakpoint, nicklen;
    char *startmsg;

    /* Send the text in chunks of 58 characters, or nearest break before 58 chars */
    if (msglen < 58)
    {
        Send_msg(pmsgbuf);
        return;
    }

    memset(nickbuf, 0, sizeof(nickbuf));

    /* See if this was a privmsg, if so, pull off the nick */
    for (startmsg = pmsgbuf; *startmsg; startmsg++)
    {
        if (*startmsg == ':') break;
    }
    if (*startmsg && (startmsg - pmsgbuf < 29))
    {
        my_strcpy(nickbuf, pmsgbuf, (startmsg - pmsgbuf) + 2);
        nicklen = strlen(nickbuf);
        startmsg += 2;
    }
    else
    {
        startmsg = pmsgbuf;
        nicklen = 0;
    }

    /* Now deal with what's left */
    while (msglen > 0)
    {
        memset(pmsg, 0, sizeof(pmsg));

        if (msglen < (58 - nicklen))
            breakpoint = msglen;
        else
        {
            /* Try to find a breaking char */
            for (breakpoint = 58 - nicklen; breakpoint > 0; breakpoint--)
            {
                if (startmsg[offset + breakpoint] == ' ') break;
                if (startmsg[offset + breakpoint] == ',') break;
                if (startmsg[offset + breakpoint] == '.') break;
                if (startmsg[offset + breakpoint] == ';') break;
            }
            if (!breakpoint) breakpoint = 58 - nicklen; /* nope */
        }

        /* If we pulled off a nick above, prepend it. */
        if (nicklen) my_strcpy(pmsg, nickbuf, nicklen + 1);

        /* Stash in this part of the msg */
        strncat(pmsg, startmsg + offset, breakpoint);
        msglen -= breakpoint;
        offset += breakpoint;
        Send_msg(pmsg);
        Net_flush();
    }
}


static void c_prt_last(byte attr, char *str, int y, int x, int n)
{
    int len = strlen(str);

    if (n < len)
        Term_putstr(x, y, n, attr, str + (len - n));
    else
        Term_putstr(x, y, len, attr, str);
}


static void c_prt_n(byte attr, char *str, int y, int x, int n)
{
    Term_putstr(x, y, n, attr, str);
}


static bool askfor_aux_msg(char *buf, int len)
{
    int y, x;
    struct keypress ch;
    int k = 0;  /* Is the end of line */
    int l = 0;  /* Is the cursor location on line */
    int j = 0;  /* Loop iterator */

	/* Terminal width */
	int wid = NORMAL_WID;

    /* Visible length on the screen */
	int vis_len = len;

    bool done = false;

    memset(&ch, 0, sizeof(ch));

    /* Locate the cursor */
    Term_locate(&x, &y);

    /* The top line is "icky" */
    topline_icky = true;

    /* Paranoia -- check len */
    if (len < 1) len = 1;

    /* Paranoia -- check column */
    if ((x < 0) || (x >= wid - 1)) x = 0;

    /* Restrict the visible length */
    if (x + vis_len > wid - 1) vis_len = wid - 1 - x;

    /* Truncate the default entry */
    buf[len] = '\0';

    /* Display the default answer */
    Term_erase(x, y, len);
    Term_putstr(x, y, -1, COLOUR_YELLOW, buf);

    /* Process input */
    while (!done)
    {
        /* Place cursor */
        if ((int)strlen(buf) > vis_len)
        {
            if (l > (int)strlen(buf) - vis_len)
                Term_gotoxy(x + l + vis_len - strlen(buf), y);
            else
                Term_gotoxy(x, y);
        }
        else
            Term_gotoxy(x + l, y);

        /* Get a key */
        ch = inkey();

        /* Analyze the key */
        switch (ch.code)
        {
            case ESCAPE:
            {
                k = 0;
                done = true;
                break;
            }

            case KC_ENTER:
            {
                buf[len] = '\0';
                k = l = strlen(buf);
                done = true;
                break;
            }

            case KC_DELETE:
            {
                /* Move the rest of the line one back */
                if (k > l)
                {
                    for (j = l + 1; j < k; j++) buf[j - 1] = buf[j];
                    k--;
                }
                break;
            }

            case KC_BACKSPACE:
            {
                if ((k == l) && (k > 0))
                {
                    k--;
                    l--;
                }

                /* Move the rest of the line one back, including char under cursor and cursor */
                if ((k > l) && (l > 0))
                {
                    for (j = l; j < k; j++) buf[j - 1] = buf[j];
                    l--;
                    k--;
                }

                break;
            }

            case ARROW_LEFT:
            {
                if (l > 0) l--;
                break;
            }

            case ARROW_RIGHT:
            {
                if (l < k) l++;
                break;
            }

            default:
            {
                if (!isprint(ch.code))
                {
                    bell("Illegal edit key!");
                    break;
                }

                /* Place character at end of line and increment k and l */
                if (k == l)
                {
                    if (k < len)
                    {
                        buf[k++] = (char)ch.code;
                        l++;
                    }
                }

                /*
                 * Place character at currect cursor position after moving
                 * the rest of the line one step forward
                 */
                else if (k > l)
                {
                    if (k < len)
                    {
                        for (j = k; j >= l; j--) buf[j + 1] = buf[j];
                        buf[l++] = (char)ch.code;
                        k++;
                    }
                }

                break;
            }
        }

        /* Terminate */
        buf[k] = '\0';

        /* Update the entry */
        Term_erase(x, y, vis_len);
        if (k >= vis_len)
        {
            if (l > k - vis_len)
                c_prt_last(COLOUR_WHITE, buf + k - vis_len, y, x, vis_len);
            else
                c_prt_n(COLOUR_WHITE, buf + l, y, x, vis_len);
        }
        else
            c_prt(COLOUR_WHITE, buf, y, x);
    }

    /* The top line is OK now */
    topline_icky = false;
    Flush_queue();

    /* Done */
    return (ch.code != ESCAPE);
}


static bool get_string_msg(const char *prompt, char *buf, int len)
{
    bool res;

    /* Display prompt */
    prt(prompt, 0, 0);

    /* Ask the user for a string */
    res = askfor_aux_msg(buf, len);

    /* Clear prompt */
    prt("", 0, 0);

    /* Result */
    return (res);
}


void do_cmd_message(void)
{
    char buf[240];
    bool ok;

#if !defined(USE_GCU) && !defined(USE_SDL)
    /* Hack to just change the window focus in Windows client */
    if (term_chat->user)
    {
#ifdef WINDOWS
        set_chat_focus();
#endif
        return;
    }
#endif

    buf[0] = '\0';
    ok = get_string_msg("Message: ", buf, sizeof(buf) - 1);
    if (ok && buf[0]) send_msg_chunks(buf, strlen(buf));
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
    party_mode = true;

    /* Enter "icky" mode */
    topline_icky = true;

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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Party commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Create a party");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) Add a player to party");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Delete a player from party");
        Term_putstr(5, 7, -1, COLOUR_WHITE, "(4) Leave your current party");
        Term_putstr(5, 8, -1, COLOUR_WHITE, "(5) Specify player to attack");
        Term_putstr(5, 9, -1, COLOUR_WHITE, "(6) Make peace");

        /* Show current party status */
        Term_putstr(0, 13, -1, COLOUR_WHITE, party_info);

        /* Prompt */
        Term_putstr(0, 11, -1, COLOUR_WHITE, "Command: ");

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
                res = get_string_ex("Party name: ", buf, sizeof(buf), false);
                if (!res) Send_party(PARTY_CREATE, buf);
                else if (res == 1) break;
            }

            /* Add player */
            else if (ke.key.code == '2')
            {
                int res;

                /* Get player name */
                res = get_string_ex("Add player: ", buf, sizeof(buf), false);
                if (!res) Send_party(PARTY_ADD, buf);
                else if (res == 1) break;
            }

            /* Delete player */
            else if (ke.key.code == '3')
            {
                int res;

                /* Get player name */
                res = get_string_ex("Delete player: ", buf, sizeof(buf), false);
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
                res = get_string_ex("Player to attack: ", buf, sizeof(buf), false);
                if (!res) Send_party(PARTY_HOSTILE, buf);
                else if (res == 1) break;
            }

            /* Make peace with player */
            else if (ke.key.code == '6')
            {
                int res;

                /* Get player name */
                res = get_string_ex("Make peace with: ", buf, sizeof(buf), false);
                if (!res) Send_party(PARTY_PEACE, buf);
                else if (res == 1) break;
            }
        }

        /* Flush messages */
        c_msg_print(NULL);
    }

    /* Restore the screen */
    screen_load(true);

    /* No longer in party mode */
    party_mode = false;

    /* Leave "icky" mode */
    topline_icky = false;
}


void do_cmd_describe(void)
{
    struct object *obj;
    const char *q, *s;

    /* Get an item */
    q = "Describe which item? ";
    s = "You have nothing to describe.";
    if (!get_item(&obj, q, s, CMD_NULL, NULL, (USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR)))
        return;

    if (obj->info_xtra.name[0] != '\0') Send_msg(obj->info_xtra.name);
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


static int cmd_master_aux_level(void)
{
    ui_event ke;

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Level commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Static your current level");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) Unstatic your current level");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Enter manual design (new level)");
        Term_putstr(5, 7, -1, COLOUR_WHITE, "(4) Enter manual design (same level)");
        Term_putstr(5, 8, -1, COLOUR_WHITE, "(5) Exit manual design (create town)");
        Term_putstr(5, 9, -1, COLOUR_WHITE, "(6) Exit manual design (create level)");

        /* Prompt */
        Term_putstr(0, 12, -1, COLOUR_WHITE, "Command: ");

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

            /* Enter manual design (same level) */
            else if (ke.key.code == '4')
                Send_master(MASTER_LEVEL, "m");

            /* Exit manual design (create town) */
            else if (ke.key.code == '5')
                Send_master(MASTER_LEVEL, "t");

            /* Exit manual design (create level) */
            else if (ke.key.code == '6')
                Send_master(MASTER_LEVEL, "x");
        }

        /* Flush messages */
        c_msg_print(NULL);
    }
}


static int cmd_master_aux_generate_item_aux(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Initialize buffer */
        buf[0] = 'i';
        buf[1] = '\0';

        /* Describe */
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Generate Item");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(k/e) Item/ego by name");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(+/*) Next item/ego");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(-//) Previous item/ego");

        Term_putstr(30, 4, -1, COLOUR_WHITE, "(h/H) Increment/decrement to-hit");
        Term_putstr(30, 5, -1, COLOUR_WHITE, "(d/D) Increment/decrement to-dam");
        Term_putstr(30, 6, -1, COLOUR_WHITE, "(a/A) Increment/decrement to-ac");
        Term_putstr(30, 7, -1, COLOUR_WHITE, "(p/P) Increment/decrement flagless pval");
        Term_putstr(30, 8, -1, COLOUR_WHITE, "(n/N) Increment/decrement modifier number");
        Term_putstr(30, 9, -1, COLOUR_WHITE, "(v/V) Increment/decrement modifier value");
        Term_putstr(30, 10, -1, COLOUR_WHITE, "(x/X) Increment/decrement extra power");
        Term_putstr(30, 11, -1, COLOUR_WHITE, "(y/Y) Increment/decrement extra resist");
        Term_putstr(30, 12, -1, COLOUR_WHITE, "(m/M) Increment/decrement extra dice");
        Term_putstr(30, 13, -1, COLOUR_WHITE, "(i/I) Increment/decrement identify status");

        Term_putstr(5, 15, -1, COLOUR_WHITE, "(g) Generate");

        /* Prompt */
        Term_putstr(0, 17, -1, COLOUR_WHITE, "Command: ");

        Term_putstr(0, 19, -1, COLOUR_WHITE, "Selection: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            if (ke.key.code == 'k')
            {
                int res;

                /* Item by name */
                buf[1] = 'k';
                buf[2] = 'n';
                res = get_string_ex("Item name: ", &buf[3], sizeof(buf), false);
                if (res == 1) return 1;
                if ((res == 2) || !buf[3]) continue;
            }
            else if (ke.key.code == 'e')
            {
                int res;

                /* Ego by name */
                buf[1] = 'e';
                buf[2] = 'n';
                res = get_string_ex("Ego name: ", &buf[3], sizeof(buf), false);
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
            else if (strchr("HDAPNVXYMI", ke.key.code))
            {
                /* Decrement value */
                buf[1] = 'M';
                buf[2] = tolower((unsigned char)ke.key.code);
            }
            else if (strchr("hdapnvxymi", ke.key.code))
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


static int cmd_master_aux_generate_item(void)
{
    int result;

    /* Clear screen */
    Term_clear();

    /* Inform server about cleared screen */
    Send_master(MASTER_GENERATE, "ir");

    result = cmd_master_aux_generate_item_aux();

    /* Inform server about cleared screen again */
    Send_master(MASTER_GENERATE, "ir");

    return result;
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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Generate Vault");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) By name");

        /* Prompt */
        Term_putstr(0, 7, -1, COLOUR_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Generate by name */
            if (ke.key.code == '1')
            {
                int res;

                buf[1] = 'n';
                res = get_string_ex("Vault name: ", &buf[2], sizeof(buf), false);
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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Generation commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Vault");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) Item");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Random artifact");
        Term_putstr(5, 7, -1, COLOUR_WHITE, "(4) Random artifact (reroll)");
        Term_putstr(5, 8, -1, COLOUR_WHITE, "(5) True artifact");

        /* Prompt */
        Term_putstr(0, 10, -1, COLOUR_WHITE, "Command: ");

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

            /* Generate a random artifact */
            else if (ke.key.code == '3')
            {
                Send_master(MASTER_GENERATE, "rn");
                return 1;
            }

            /* Generate a (rerolled) random artifact */
            else if (ke.key.code == '4')
            {
                Send_master(MASTER_GENERATE, "rr");
                return 1;
            }

            /* Generate a true artifact */
            else if (ke.key.code == '5')
            {
                char buf[NORMAL_WID];
                char kind[NORMAL_WID], name[NORMAL_WID];
                int res;

                memset(kind, 0, sizeof(kind));
                memset(name, 0, sizeof(name));

                /* Artifact by kind */
                res = get_string_ex("Artifact kind: ", kind, sizeof(kind), false);
                if (res == 1) return 1;
                if ((res == 2) || !kind[0]) continue;

                /* Artifact by name */
                res = get_string_ex("Artifact name: ", name, sizeof(name), false);
                if (res == 1) return 1;
                if ((res == 2) || !name[0]) continue;

                strnfmt(buf, sizeof(buf), "a%s|%s", kind, name);

                /* Send choice to server */
                Send_master(MASTER_GENERATE, buf);
                return 1;
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
        memset(buf, 0, sizeof(buf));

        /* Describe */
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Building commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Set Feature");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) Place Feature");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Draw Line");
        Term_putstr(5, 7, -1, COLOUR_WHITE, "(4) Fill Rectangle");
        Term_putstr(5, 8, -1, COLOUR_WHITE, "(5) Build Mode On");
        Term_putstr(5, 9, -1, COLOUR_WHITE, "(6) Build Mode Off");

        /* Prompt */
        Term_putstr(0, 12, -1, COLOUR_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Set Feature */
            if (ke.key.code == '1')
            {
                int res;

                buf[0] = 'i';
                res = get_string_ex("Feature name: ", &buf[1], sizeof(buf), false);
                if (res == 1) return 1;
                if ((res == 2) || !buf[1]) continue;
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
        /* Clear buffer */
        buf[0] = 0;
        buf[1] = 1;

        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Summon...");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) X here");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) X at random locations");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Group here");
        Term_putstr(5, 7, -1, COLOUR_WHITE, "(4) Group at random location");
        Term_putstr(5, 8, -1, COLOUR_WHITE, "(5) Pet");
        Term_putstr(5, 9, -1, COLOUR_WHITE, "(6) Summoning mode");

        /* Prompt */
        Term_putstr(0, 11, -1, COLOUR_WHITE, "Command: ");

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

            /* Pet */
            else if (ke.key.code == '5')
                buf[0] = 'P';

            /* Summoning mode on */
            else if (ke.key.code == '6')
                buf[0] = 'T';

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
        /* Clear buffer */
        memset(buf, 0, sizeof(buf));

        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Summon...");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Depth");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) Specific");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Mass Banishment");
        Term_putstr(5, 7, -1, COLOUR_WHITE, "(4) Summoning mode off");

        /* Prompt */
        Term_putstr(0, 10, -1, COLOUR_WHITE, "Command: ");

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
            }

            /* Summon a specific monster or character */
            else if (ke.key.code == '2')
            {
                int res;

                buf[2] = 's';
                res = get_string_ex("Enter (partial) monster name: ", &buf[3], sizeof(buf),
                    false);
                if (res == 1) return 1;
                if ((res == 2) || !buf[3]) continue;
            }

            /* Delete all the monsters near us (turn summoning mode on) */
            else if (ke.key.code == '3')
            {
                buf[0] = 'T';
                buf[1] = 1;
                buf[2] = 'b';
                Send_master(MASTER_SUMMON, buf);

                continue;
            }

            /* Disable summoning mode */
            else if (ke.key.code == '4')
            {
                buf[0] = 'F';
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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Player commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(<) Previous");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(>) Next");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "RET Change");

        Term_putstr(5, 8, -1, COLOUR_WHITE, "(g) Ghost");
        Term_putstr(5, 9, -1, COLOUR_WHITE, "(w) Wizard");

        /* Prompt */
        Term_putstr(0, 15, -1, COLOUR_WHITE, "Selection: ");

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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Player commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Self");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) By Name");

        /* Prompt */
        Term_putstr(0, 8, -1, COLOUR_WHITE, "Command: ");

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
                int res = get_string_ex("Player: ", buf, sizeof(buf), false);

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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Visual commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Display PROJ_XXX types");

        /* Prompt */
        Term_putstr(0, 8, -1, COLOUR_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        /* Display PROJ_XXX types */
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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Manage XBM orders");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(<) Previous");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(>) Next");

        Term_putstr(5, 7, -1, COLOUR_WHITE, "(c) Cancel order");

        /* Prompt */
        Term_putstr(0, 15, -1, COLOUR_WHITE, "Selection: ");

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


static int cmd_master_aux_debug(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Debug commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Perform an effect (EFFECT_XXX)");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) Create a trap");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Advance time");

        /* Prompt */
        Term_putstr(0, 8, -1, COLOUR_WHITE, "Command: ");

        /* Get a key */
        ke = inkey_ex();

        /* Leave */
        return_on_abort(ke);

        if (ke.type == EVT_KBRD)
        {
            /* Perform an effect */
            if (ke.key.code == '1')
            {
                int res;
                char tmp[NORMAL_WID];

                buf[0] = 'E';
                buf[1] = '\0';

                memset(tmp, 0, sizeof(tmp));

                /* Get the name */
                res = get_string_ex("Do which effect? ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));
                my_strcat(buf, "|", sizeof(buf));

                /* Get the dice */
                res = get_string_ex("Enter damage dice (eg 1+2d6M2): ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));
                my_strcat(buf, "|", sizeof(buf));

                /* Get the parameters */
                res = get_string_ex("Enter name or number for first parameter: ", tmp, sizeof(tmp),
                    false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));
                my_strcat(buf, "|", sizeof(buf));
                res = get_string_ex("Enter second parameter: ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));
                my_strcat(buf, "|", sizeof(buf));
                res = get_string_ex("Enter third parameter: ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));
                my_strcat(buf, "|", sizeof(buf));
                res = get_string_ex("Enter y parameter: ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));
                my_strcat(buf, "|", sizeof(buf));
                res = get_string_ex("Enter x parameter: ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));

                Send_master(MASTER_DEBUG, buf);
                return 1;
            }

            /* Create a trap */
            if (ke.key.code == '2')
            {
                int res;
                char tmp[NORMAL_WID];

                buf[0] = 'T';
                buf[1] = '\0';

                memset(tmp, 0, sizeof(tmp));

                /* Get the name */
                res = get_string_ex("Create which trap? ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));

                Send_master(MASTER_DEBUG, buf);
                return 1;
            }

            /* Advance time */
            if (ke.key.code == '3')
            {
                int res;
                char tmp[NORMAL_WID];

                buf[0] = 'H';
                buf[1] = '\0';

                memset(tmp, 0, sizeof(tmp));

                /* Get the amount */
                res = get_string_ex("Amount (1-12 hours): ", tmp, sizeof(tmp), false);
                if (res == 1) return 1;
                if ((res == 2) || !tmp[0]) continue;
                my_strcat(buf, tmp, sizeof(buf));

                Send_master(MASTER_DEBUG, buf);
                return 1;
            }
        }

        /* Flush messages */
        c_msg_print(NULL);
    }
}


void do_cmd_master(void)
{
    ui_event ke;
    char buf[NORMAL_WID];

    /* We are now in party mode */
    party_mode = true;

    /* Enter "icky" mode */
    topline_icky = true;

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
        Term_putstr(0, 2, -1, COLOUR_WHITE, "Dungeon Master commands");

        /* Selections */
        Term_putstr(5, 4, -1, COLOUR_WHITE, "(1) Level Commands");
        Term_putstr(5, 5, -1, COLOUR_WHITE, "(2) Building Commands");
        Term_putstr(5, 6, -1, COLOUR_WHITE, "(3) Summoning Commands");
        Term_putstr(5, 7, -1, COLOUR_WHITE, "(4) Generation Commands");
        Term_putstr(5, 8, -1, COLOUR_WHITE, "(5) Player Commands");
        Term_putstr(5, 9, -1, COLOUR_WHITE, "(6) Visual Commands");
        Term_putstr(5, 10, -1, COLOUR_WHITE, "(7) Manage XBM orders");
        Term_putstr(5, 11, -1, COLOUR_WHITE, "(8) Debug Commands");

        /* Prompt */
        Term_putstr(0, 14, -1, COLOUR_WHITE, "Command: ");

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

            /* Debug commands */
            else if (ke.key.code == '8')
            {
                if (cmd_master_aux_debug() == 1) break;
            }
        }

        /* Flush messages */
        c_msg_print(NULL);
    }

    /* Restore the screen */
    screen_load(true);

    /* No longer in party mode */
    party_mode = false;

    /* Leave "icky" mode */
    topline_icky = false;
}


void do_cmd_social(void)
{
    int dir, k;
    bool found = false;
    struct social *s;
    char buf[NORMAL_WID];

    memset(buf, 0, sizeof(buf));

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
        s = &soc_info[k];

        if (streq(s->name, buf))
        {
            found = true;
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
    if (s->target) get_aim_dir(&dir);

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
    for (item = 0; item < z_info->pack_size; item++)
    {
        struct object *obj = player->upkeep->inven[item];

        if (!obj) continue;
        if (tval_is_bottle(obj)) break;
    }
    if (item == z_info->pack_size) item = -1;
    else
    {
        /* Drink or refill? */
        if (!get_check("Do you want to fill a bottle? ")) item = -1;
    }

    /* Send it */
    Send_fountain(item);
}


void do_cmd_time(void)
{
    Send_time();
}
