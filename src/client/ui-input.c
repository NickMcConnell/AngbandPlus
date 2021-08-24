/*
 * File: ui-input.c
 * Purpose: Some high-level UI functions, inkey()
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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


/* See the "inkey()" function */
static bool inkey_xtra = false;
char inkey_scan = SCAN_OFF;
bool inkey_flag = false;


/* Trap detection indicator */
byte trap_indicator;


/* Escape command */
bool first_escape = false;


/* Top line is icky */
bool topline_icky;


/* Melee dice */
int dis_dd;
int dis_ds;


/* Melee to-hit/to-dam */
int dis_to_mhit;
int dis_to_mdam;


/* Missile to-hit/to-dam */
int dis_to_shit;
int dis_to_sdam;


/*
 * Flush all pending input.
 *
 * Actually, remember the flush, using the "inkey_xtra" flag, and in the
 * next call to "inkey()", perform the actual flushing, for efficiency,
 * and correctness of the "inkey()" function.
 */
void flush(game_event_type type, game_event_data *data, void *user)
{
    /* Do it later */
    inkey_xtra = true;
}


/*
 * Helper function called only from "inkey()"
 */
static ui_event inkey_aux(char scan_cutoff)
{
    ui_event ke;

    /* Loop, looking for net input and responding to keypresses */
    ke = Net_loop(Term_inkey, NULL, NULL, scan_cutoff);

    /* Excessive delay */
    if (ke.type == EVT_DELAY)
    {
        ui_event empty = EVENT_EMPTY;

        return empty;
    }

    /* Efficiency hack -- ignore escape key for keymaps */
    if (is_escape(ke)) first_escape = true;
    
    return (ke);
}


/*
 * Hack -- special "inkey_next" pointer.  XXX XXX XXX
 *
 * This special pointer allows a sequence of keys to be "inserted" into
 * the stream of keys returned by "inkey()".  We use it to implement keymaps.
 */
struct keypress *inkey_next = NULL;


/*
 * Get a keypress from the user.
 *
 * This function recognizes a few "global parameters".  These are variables
 * which, if set to true before calling this function, will have an effect
 * on this function, and which are always reset to false by this function
 * before this function returns.  Thus they function just like normal
 * parameters, except that most calls to this function can ignore them.
 *
 * If "inkey_xtra" is true, then all pending keypresses will be flushed.
 * This is set by flush(), which doesn't actually flush anything itself
 * but uses that flag to trigger delayed flushing.
 *
 * If "inkey_scan" is true, then we will immediately return "zero" if no
 * keypress is available, instead of waiting for a keypress.
 *
 * If "inkey_flag" is true, then we are waiting for a command in the main
 * map interface, and we shouldn't show a cursor.
 *
 * If we are waiting for a keypress, and no keypress is ready, then we will
 * refresh (once) the window which was active when this function was called.
 *
 * Note that "back-quote" is automatically converted into "escape" for
 * convenience on machines with no "escape" key.
 *
 * If "angband_term[0]" is not active, we will make it active during this
 * function, so that the various "main-xxx.c" files can assume that input
 * is only requested (via "Term_inkey()") when "angband_term[0]" is active.
 */
ui_event inkey_ex(void)
{
    bool cursor_state;
    ui_event kk;
    ui_event ke = EVENT_EMPTY;
    bool done = false;
    term *old = Term;

    /* Delayed flush */
    if (inkey_xtra)
    {
        Term_flush();
        inkey_next = NULL;
        inkey_xtra = false;
    }

    /* Hack -- use the "inkey_next" pointer */
    if (inkey_next && inkey_next->code)
    {
        /* Get next character, and advance */
        ke.key = *inkey_next++;

        /* Cancel the various "global parameters" */
        inkey_flag = false;
        inkey_scan = SCAN_OFF;

        /* Accept result */
        return (ke);
    }

    /* Forget pointer */
    inkey_next = NULL;

    /* Get the cursor state */
    Term_get_cursor(&cursor_state);

    /* Show the cursor if waiting, except sometimes in "command" mode */
    if (!inkey_scan && (!inkey_flag || player->screen_save_depth))
        Term_set_cursor(true);

    /* Hack -- activate main screen */
    Term_activate(term_screen);

    /* Get a key */
    while (ke.type == EVT_NONE)
    {
        /* Hack -- handle (inkey_scan == SCAN_INSTANT) */
        if ((inkey_scan == SCAN_INSTANT) && (0 != Term_inkey(&kk, false, false)))
            break;

        /* Hack -- flush output once when no key ready */
        if (!done && (0 != Term_inkey(&kk, false, false)))
        {
            /* Hack -- activate proper term */
            Term_activate(old);

            /* Flush output */
            Term_fresh();

            /* Hack -- activate main screen */
            Term_activate(term_screen);

            /* Only once */
            done = true;
        }

        /* Get a key (see above) */
        ke = inkey_aux(inkey_scan);

        /* The keypress timed out. We need to stop here. */
        if (inkey_scan && (ke.type == EVT_NONE)) break;

        /* Error */
        if (ke.type == EVT_ERROR) quit(NULL);

        /* Treat back-quote as escape */
        if (ke.key.code == '`') ke.key.code = ESCAPE;
    }

    /* Hack -- restore the term */
    Term_activate(old);
    
    /* Restore the cursor */
    Term_set_cursor(cursor_state);

    /* Cancel the various "global parameters" */
    inkey_flag = false;
    inkey_scan = SCAN_OFF;

    /* Return the keypress */
    return (ke);
}


/*
 * Get a keypress from the user.
 */
struct keypress inkey(void)
{
    ui_event ke = EVENT_EMPTY;

    /* Only accept a keypress (ignore abort) */
    while (ke.type != EVT_KBRD)
    {
        /* Get a keypress */
        ke = inkey_ex();

        /* Handle escape/error */
        if ((ke.type == EVT_ESCAPE) || (ke.type == EVT_ERROR))
        {
            ke.type = EVT_KBRD;
            ke.key.code = ESCAPE;
            ke.key.mods = 0;
        }
    }

    return ke.key;
}


void prt_icky(const char *str, int row, int col)
{
    /* Clear line, position cursor */
    Term_erase_icky(col, row, 255);

    /* Dump the attr/text */
    Term_addstr(-1, COLOUR_WHITE, str);
}


/*
 * Clear the bottom part of the screen
 */
void clear_from(int row)
{
    int y;

    /* Erase requested rows */
    for (y = row; y < Term->hgt; y++)
        Term_erase(0, y, 255);
}


/*
 * The default "keypress handling function" for askfor_aux, this takes the
 * given keypress, input buffer, length, etc, and does the appropriate action
 * for each keypress, such as moving the cursor left or inserting a character.
 *
 * It should return true when editing of the buffer is "complete" (e.g. on
 * the press of RETURN).
 */
bool askfor_aux_keypress(char *buf, size_t buflen, size_t *curs, size_t *len,
    struct keypress keypress, bool firsttime)
{
    switch (keypress.code)
    {
        case ESCAPE:
        {
            *curs = 0;
            return true;
        }

        case KC_ENTER:
        {
            *curs = *len;
            return true;
        }

        case ARROW_LEFT:
        {
            if (firsttime) *curs = 0;
            if (*curs > 0) (*curs)--;
            break;
        }

        case ARROW_RIGHT:
        {
            if (firsttime) *curs = *len - 1;
            if (*curs < *len) (*curs)++;
            break;
        }

        case KC_BACKSPACE:
        case KC_DELETE:
        {
            /* If this is the first time round, backspace means "delete all" */
            if (firsttime)
            {
                buf[0] = '\0';
                *curs = 0;
                *len = 0;

                break;
            }

            /* Refuse to backspace into oblivion */
            if (((keypress.code == KC_BACKSPACE) && (*curs == 0)) ||
                ((keypress.code == KC_DELETE) && (*curs >= *len)))
            {
                break;
            }

            /* Move the string from k to nul along to the left by 1 */
            if (keypress.code == KC_BACKSPACE)
                memmove(&buf[*curs - 1], &buf[*curs], *len - *curs);
            else
                memmove(&buf[*curs], &buf[*curs + 1], *len - *curs - 1);

            /* Decrement */
            if (keypress.code == KC_BACKSPACE) (*curs)--;
            (*len)--;

            /* Terminate */
            buf[*len] = '\0';

            break;
        }

        default:
        {
            bool atnull = (buf[*curs] == 0);

            if (!isprint(keypress.code))
            {
                bell("Illegal edit key!");
                break;
            }

            /* Clear the buffer if this is the first time round */
            if (firsttime)
            {
                buf[0] = '\0';
                *curs = 0;
                *len = 0;
                atnull = 1;
            }

            if (atnull)
            {
                /* Make sure we have enough room for a new character */
                if ((*curs + 1) >= buflen) break;
            }
            else
            {
                /* Make sure we have enough room to add a new character */
                if ((*len + 1) >= buflen) break;

                /* Move the rest of the buffer along to make room */
                memmove(&buf[*curs+1], &buf[*curs], *len - *curs);
            }

            /* Insert the character */
            buf[(*curs)++] = (char)keypress.code;
            (*len)++;

            /* Terminate */
            buf[*len] = '\0';

            break;
        }
    }

    /* By default, we aren't done. */
    return false;
}


/*
 * Get some input at the cursor location.
 *
 * The buffer is assumed to have been initialized to a default string.
 * Note that this string is often "empty" (see below).
 *
 * The default buffer is displayed in yellow until cleared, which happens
 * on the first keypress, unless that keypress is Return.
 *
 * Normal chars clear the default and append the char.
 * Backspace clears the default or deletes the final char.
 * Return accepts the current buffer contents and returns true.
 * Escape clears the buffer and the window and returns false.
 *
 * Note that 'len' refers to the size of the buffer.  The maximum length
 * of the input is 'len-1'.
 *
 * 'keypress_h' is a pointer to a function to handle keypresses, altering
 * the input buffer, cursor position and suchlike as required.  See
 * 'askfor_aux_keypress' (the default handler if you supply NULL for
 * 'keypress_h') for an example.
 */
bool askfor_aux(char *buf, int len, keypress_handler keypress_h)
{
    int y, x;
    size_t k = 0;   /* Cursor position */
    size_t nul = 0; /* Position of the null byte in the string */
    struct keypress ch;
    bool done = false;
    bool firsttime = true;

    memset(&ch, 0, sizeof(ch));

    if (keypress_h == NULL) keypress_h = askfor_aux_keypress;

    /* Locate the cursor */
    Term_locate(&x, &y);

    /* The top line is "icky" */
    topline_icky = true;

    /* Paranoia -- check len */
    if (len < 1) len = 1;

    /* Paranoia -- check column */
    if ((x < 0) || (x >= NORMAL_WID)) x = 0;

    /* Restrict the length */
    if (x + len > NORMAL_WID) len = NORMAL_WID - x;

    /* Truncate the default entry */
    buf[len - 1] = '\0';

    /* Get the position of the null byte */
    nul = strlen(buf);

    /* Display the default answer */
    Term_erase(x, y, len);
    Term_putstr(x, y, -1, COLOUR_YELLOW, buf);

    /* Process input */
    while (!done)
    {
        /* Place cursor */
        Term_gotoxy(x + k, y);

        /* Get a key */
        ch = inkey();

        /* Let the keypress handler deal with the keypress */
        done = keypress_h(buf, len, &k, &nul, ch, firsttime);

        /* Update the entry */
        Term_erase(x, y, len);
        Term_putstr(x, y, -1, COLOUR_WHITE, buf);

        /* Not the first time round anymore */
        firsttime = false;
    }

    /* The top line is OK now */
    topline_icky = false;
    Flush_queue();

    /* Done */
    return (ch.code != ESCAPE);
}


/*
 * Ask the user for a masked string
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
int askfor_ex(char *buf, int len, keypress_handler keypress_h, bool priv)
{
    int y, x;
    size_t i, k = 0;   /* Cursor position */
    size_t nul = 0; /* Position of the null byte in the string */
    ui_event ke = EVENT_EMPTY;
    bool done = false;
    bool firsttime = true;

    if (keypress_h == NULL) keypress_h = askfor_aux_keypress;

    /* Locate the cursor */
    Term_locate(&x, &y);

    /* The top line is "icky" */
    topline_icky = true;

    /* Paranoia -- check len */
    if (len < 1) len = 1;

    /* Paranoia -- check column */
    if ((x < 0) || (x >= NORMAL_WID)) x = 0;

    /* Restrict the length */
    if (x + len > NORMAL_WID) len = NORMAL_WID - x;

    /* Truncate the default entry */
    buf[len - 1] = '\0';

    /* Get the position of the null byte */
    nul = strlen(buf);

    /* Display the default answer */
    Term_erase(x, y, len);
    Term_putstr(x, y, -1, COLOUR_YELLOW, buf);

    /* Process input */
    while (!done)
    {
        /* Place cursor */
        Term_gotoxy(x + k, y);

        /* Get a key */
        ke = inkey_ex();

        /* Handle abort */
        if (is_exit(ke))
        {
            k = 0;
            done = true;
        }

        /* Let the keypress handler deal with the keypress */
        else done = keypress_h(buf, len, &k, &nul, ke.key, firsttime);

        Term_erase(x, y, len);

        /* Update the entry */
        if (!priv)
            Term_putstr(x, y, -1, COLOUR_WHITE, buf);
        else
        {
            for (i = 0; i < nul; i++)
                Term_putch(x + i, y, COLOUR_WHITE, 'x');
        }

        /* Not the first time round anymore */
        firsttime = false;
    }

    /* The top line is OK now */
    topline_icky = false;
    Flush_queue();

    /* Done */
    return_on_abort(ke);
    return 0;
}


/*
 * Get a string from the user
 *
 * The "prompt" should take the form "Prompt: "
 *
 * Note that the initial contents of the string is used as
 * the default response, so be sure to "clear" it if needed.
 *
 * We clear the input, and return false, on "ESCAPE".
 */
static bool textui_get_string(const char *prompt, char *buf, int len)
{
    bool res;

    /* Display prompt */
    prt(prompt, 0, 0);

    /* Ask the user for a string */
    res = askfor_aux(buf, len, NULL);

    /* Clear prompt */
    prt("", 0, 0);

    /* Result */
    return (res);
}


/*
 * Get a string from the user
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
static int textui_get_string_ex(const char *prompt, char *buf, int len, bool priv)
{
    int res;

    /* Display prompt */
    prt(prompt, 0, 0);

    /* Ask the user for a string */
    res = askfor_ex(buf, len, NULL, priv);

    /* Clear prompt */
    prt("", 0, 0);

    /* Result */
    return (res);
}


/*
 * Request a "quantity" from the user
 */
static s32b textui_get_quantity(const char *prompt, s32b max)
{
    int amt = 1;
    char tmp[NORMAL_WID];
    char buf[NORMAL_WID];

    /* Build a prompt if needed */
    if (!prompt)
    {
        /* Build a prompt */
        strnfmt(tmp, sizeof(tmp), "Quantity (0-%" PRId32 ", *=all): ", max);

        /* Use that prompt */
        prompt = tmp;
    }

    /* Build the default */
    strnfmt(buf, sizeof(buf), "%d", amt);

    /* Ask for a quantity */
    if (!get_string(prompt, buf, 10)) return (0);

    /* Extract a number */
    amt = atoi(buf);

    /* A star or letter means "all" */
    if ((buf[0] == '*') || isalpha((unsigned char)buf[0])) amt = max;

    /* Enforce the maximum */
    if (amt > max) amt = max;

    /* Enforce the minimum */
    if (amt < 0) amt = 0;

    /* Return the result */
    return (amt);
}


/*
 * Request a "quantity" from the user
 *
 * Return -1 on abort, 0 on escape
 */
static s32b textui_get_quantity_ex(const char *prompt, s32b max)
{
    int amt = 1, res;
    char tmp[NORMAL_WID];
    char buf[NORMAL_WID];

    /* Build a prompt if needed */
    if (!prompt)
    {
        /* Build a prompt */
        strnfmt(tmp, sizeof(tmp), "Quantity (0-%" PRId32 ", *=all): ", max);

        /* Use that prompt */
        prompt = tmp;
    }

    /* Build the default */
    strnfmt(buf, sizeof(buf), "%d", amt);

    /* Ask for a quantity */
    res = get_string_ex(prompt, buf, 10, false);
    if (res == 1) return -1;
    if (res == 2) return 0;

    /* Extract a number */
    amt = atoi(buf);

    /* A star or letter means "all" */
    if ((buf[0] == '*') || isalpha((unsigned char)buf[0])) amt = max;

    /* Enforce the maximum */
    if (amt > max) amt = max;

    /* Enforce the minimum */
    if (amt < 0) amt = 0;

    /* Return the result */
    return (amt);
}


/*
 * Verify something with the user
 *
 * The "prompt" should take the form "Query? "
 *
 * Note that "[y/n]" is appended to the prompt.
 */
static bool textui_get_check(const char *prompt)
{
    struct keypress ke;
    char buf[NORMAL_WID];

    /* Option -- "auto_accept" */
    if (OPT(player, auto_accept)) return true;

    /* Hack -- build a "useful" prompt */
    strnfmt(buf, NORMAL_WID - 2, "%.70s[y/n] ", prompt);

    /* The top line is "icky" */
    topline_icky = true;

    /* Prompt for it */
    prt(buf, 0, 0);

    /* Get an acceptable answer */
    ke = inkey();

    /* Erase the prompt */
    prt("", 0, 0);

    /* The top line is OK again */
    topline_icky = false;

    /* Flush any events that came in while we were icky */
    Flush_queue();

    /* Normal negation */
    if ((ke.code != 'Y') && (ke.code != 'y')) return false;

    /* Success */
    return true;
}


/*
 * Verify something with the user
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
static int textui_get_check_ex(const char *prompt)
{
    ui_event ke = EVENT_EMPTY;
    char buf[NORMAL_WID];

    /* Option -- "auto_accept" */
    if (OPT(player, auto_accept)) return 0;

    /* Hack -- build a "useful" prompt */
    strnfmt(buf, NORMAL_WID - 2, "%.70s[y/n] ", prompt);

    /* The top line is "icky" */
    topline_icky = true;

    /* Prompt for it */
    prt(buf, 0, 0);

    /* Get an acceptable answer */
    ke = inkey_ex();

    /* Erase the prompt */
    prt("", 0, 0);

    /* The top line is OK again */
    topline_icky = false;

    /* Flush any events that came in while we were icky */
    Flush_queue();

    /* Handle abort */
    return_on_abort(ke);

    /* Success */
    if ((ke.type == EVT_KBRD) && ((ke.key.code == 'Y') || (ke.key.code == 'y'))) return 0;

    return 2;
}


/*
 * Text-native way of getting a filename.
 */
static bool get_file_text(const char *suggested_name, char *path, size_t len)
{
    char buf[160];

    /* Get filename */
    my_strcpy(buf, suggested_name, sizeof(buf));
    if (!get_string("File name: ", buf, sizeof(buf))) return false;

    /* Make sure it's actually a filename */
    if ((buf[0] == '\0') || (buf[0] == ' ')) return false;

    /* Build the path */
    path_build(path, len, ANGBAND_DIR_USER, buf);

    /* Check if it already exists */
    if (file_exists(path))
    {
        char buf2[160];

        strnfmt(buf2, sizeof(buf2), "Replace existing file %s? ", buf);

        if (!get_check(buf2)) return false;
    }

    /* Tell the user where it's saved to. */
    prt(format("Saving as %s.", path), 0, 0);
    inkey();
    prt("", 0, 0);

    return true;
}


/*
 * Get a pathname to save a file to, given the suggested name. Returns the result in "path".
 */
bool (*get_file)(const char *suggested_name, char *path, size_t len) = get_file_text;


/*
 * Prompts for a keypress
 *
 * The "prompt" should take the form "Command: "
 *
 * Returns true unless the character is "Escape"
 */
static bool textui_get_com(const char *prompt, struct keypress *command)
{
    struct keypress ke;

    /* The top line is "icky" */
    topline_icky = true;

    /* Display a prompt */
    prt(prompt, 0, 0);

    /* Get a key (important: bypass abort) */
    ke = inkey();

    /* Clear the prompt */
    prt("", 0, 0);

    /* Fix the top line */
    topline_icky = false;

    /* Save the command */
    *command = ke;

    /* Flush any events */
    Flush_queue();

    /* Done */
    if (ke.code == ESCAPE) return false;
    return true;
}


static bool textui_get_com_ex(const char *prompt, ui_event *command)
{
    ui_event ke;

    /* The top line is "icky" */
    topline_icky = true;

    /* Display a prompt */
    prt(prompt, 0, 0);

    /* Get a key */
    ke = inkey_ex();

    /* Clear the prompt */
    prt("", 0, 0);

    /* Fix the top line */
    topline_icky = false;

    /* Save the command */
    *command = ke;

    /* Flush any events */
    Flush_queue();

    /* Done */
    if ((ke.type == EVT_KBRD) && (ke.key.code == ESCAPE)) return false;
    return true;
}


void flush_now(void)
{
    /* Clear various flags */
    inkey_xtra = false;

    /* Forgot old keypresses */
    Term_flush();
}


void flush_hack(void)
{
    /* Clear various flags */
    inkey_scan = SCAN_OFF;

    /* Forgot old keypresses */
    Term_flush();
}


/*
 * Extract a direction (or zero) from a character
 */
int target_dir(struct keypress ch)
{
    int d = 0;

    /* Already a direction? */
    if (isdigit((unsigned char)ch.code))
        d = D2I(ch.code);
    else if (isarrow(ch.code))
    {
        switch (ch.code)
        {
            case ARROW_DOWN:  d = 2; break;
            case ARROW_LEFT:  d = 4; break;
            case ARROW_RIGHT: d = 6; break;
            case ARROW_UP:    d = 8; break;
        }
    }
    else
    {
        int mode;
        const struct keypress *act;

        /* Roguelike */
        if (OPT(player, rogue_like_commands)) mode = KEYMAP_MODE_ROGUE;

        /* Original */
        else mode = KEYMAP_MODE_ORIG;

        /* XXX See if this key has a digit in the keymap we can use */
        act = keymap_find(mode, ch);
        if (act)
        {
            const struct keypress *cur;

            /* Convert to a direction */
            for (cur = act; cur->type == EVT_KBRD; cur++)
            {
                if (isdigit((unsigned char)cur->code)) d = D2I(cur->code);
            }
        }
    }

    /* Paranoia */
    if (d == 5) d = 0;

    /* Return direction */
    return (d);
}


static int dir_transitions[10][10] =
{
    /* 0-> */ { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
    /* 1-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 2-> */ { 0, 0, 2, 0, 1, 0, 3, 0, 5, 0 },
    /* 3-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 4-> */ { 0, 0, 1, 0, 4, 0, 5, 0, 7, 0 },
    /* 5-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 6-> */ { 0, 0, 3, 0, 5, 0, 6, 0, 9, 0 },
    /* 7-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    /* 8-> */ { 0, 0, 5, 0, 7, 0, 9, 0, 8, 0 },
    /* 9-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
};


/*
 * Get an "aiming direction" (1,2,3,4,6,7,8,9 or 5) from the user.
 *
 * Return true if a direction was chosen, otherwise return false.
 *
 * The direction "5" is special, and means "use current target".
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 */
static bool textui_get_aim_dir(int *dp)
{
    /* Global direction */
    int dir = 0;

    ui_event ke;
    const char *p;

    /* Initialize */
    (*dp) = 0;
    ke.type = EVT_KBRD;

    /* Ask until satisfied */
    while (!dir)
    {
        bool res;

        /* Choose a prompt */
        p = "Direction ('*' to target, \"'\" for closest, '(' for friendly, Escape to cancel)? ";

        /* Get a command (or Cancel) */
        Term->no_cursor = true;
        res = get_com(p, &ke.key);
        Term->no_cursor = false;
        if (!res) break;

        /* Analyze */
        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                /* Set new target, use target if legal */
                case '*':
                {
                    if (cmd_target_interactive(TARGET_KILL | TARGET_AIM)) dir = 5;
                    break;
                }

                /* Set new friendly target, use target if legal */
                case '(':
                {
                    if (cmd_target_interactive(TARGET_HELP)) dir = 5;
                    break;
                }

                /* Set to closest target */
                case '\'':
                {
                    Send_target_closest(TARGET_KILL);
                    dir = 5;
                    break;
                }

                /* Possible direction */
                default:
                {
                    int keypresses_handled = 0;

                    while (ke.key.code != 0)
                    {
                        int this_dir;

                        /*
                         * XXX Ideally show and move the cursor here to indicate
                         * the currently "Pending" direction. XXX
                         */
                        this_dir = target_dir(ke.key);

                        if (this_dir)
                            dir = dir_transitions[dir][this_dir];
                        else
                            break;

                        if ((player->opts.lazymove_delay == 0) || (++keypresses_handled > 1))
                            break;

                        /* See if there's a second keypress within the defined period of time */
                        inkey_scan = player->opts.lazymove_delay;
                        ke = inkey_ex();
                    }
                }
            }
        }

        /* Error */
        if (!dir) bell("Illegal aim direction!");
    }

    /* No direction */
    if (!dir) return false;

    /* Save direction */
    (*dp) = dir;

    /* A "valid" direction was entered */
    return true;
}


/*
 * Get an "aiming direction" (1,2,3,4,6,7,8,9 or 5) from the user.
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
static int textui_get_aim_dir_ex(int *dp)
{
    /* Global direction */
    int dir = 0;

    ui_event ke;

    /* Initialize */
    (*dp) = 0;

    /* The top line is "icky" */
    topline_icky = true;

    /* Display a prompt */
    prt("Direction (Escape to cancel)? ", 0, 0);

    /* Ask until satisfied */
    while (!dir)
    {
        /* Get a key */
        ke = inkey_ex();

        /* Handle "cancel" */
        if (is_exit(ke)) break;

        /* Analyze */
        dir = target_dir(ke.key);

        /* Error */
        if (!dir) bell("Illegal aim direction!");
    }

    /* Clear the prompt */
    prt("", 0, 0);

    /* Fix the top line */
    topline_icky = false;

    /* Flush any events */
    Flush_queue();

    /* Handle "cancel" */
    return_on_abort(ke);

    /* No direction */
    if (!dir) return 2;

    /* Save direction */
    (*dp) = dir;

    /* A "valid" direction was entered */
    return 0;
}


/*
 * Initialize the UI hooks to give input asked for by the game
 */
void textui_input_init(void)
{
    get_string_hook = textui_get_string;
    get_string_ex_hook = textui_get_string_ex;
    get_quantity_hook = textui_get_quantity;
    get_quantity_ex_hook = textui_get_quantity_ex;
    get_check_hook = textui_get_check;
    get_check_ex_hook = textui_get_check_ex;
    get_com_hook = textui_get_com;
    get_com_ex_hook = textui_get_com_ex;
    get_aim_dir_hook = textui_get_aim_dir;
    get_aim_dir_ex_hook = textui_get_aim_dir_ex;
    get_spell_hook = textui_get_spell;
    get_item_hook = textui_get_item;
    get_curse_hook = textui_get_curse;
}


void caveprt(cave_view_type* src, int len, s16b x, s16b y)
{
    int i;

    /* Draw a character n times */
    for (i = 0; i < len; i++)
    {
        /* Draw the character */
        Term_draw(i + x, y, src[i].a, src[i].c);
    }
}


void cavestr(cave_view_type* dest, const char *str, byte attr, int max_col)
{
    int i, e;

    e = strlen(str);
    for (i = 0; i < e; i++)
    {
        dest[i].a = attr;
        dest[i].c = str[i];
    }
    for (i = e; i < max_col; i++)
    {
        dest[i].a = COLOUR_WHITE;
        dest[i].c = ' ';
    }
}


/*
 * Hack -- given a pathname, point at the filename
 */
const char *extract_file_name(const char *s)
{
    const char *p;

    /* Start at the end */
    p = s + strlen(s) - 1;

    /* Back up to divider */
    while ((p >= s) && (*p != ':') && (*p != '\\')) p--;

    /* Return file name */
    return (p + 1);
}


/*
 * Loop, looking for net input and responding to keypresses.
 */
ui_event Net_loop(errr (*inkey_handler)(ui_event*, bool, bool),
    void (*callback_begin)(ui_event*), void (*callback_end)(void), char scan_cutoff)
{
    ui_event ke = EVENT_EMPTY;
    int net_fd;
    int w = 0;

    /* Acquire and save maximum file descriptor */
    net_fd = Net_fd();

    /* If no network yet, just wait for a keypress */
    if (net_fd == -1)
    {
        ke.type = EVT_ERROR;

        /* Look for a keypress */
        if (inkey_handler) inkey_handler(&ke, true, true);

        /* Report error */
        else
        {
            plog("Bad socket file descriptor");
            send_quit = false;
        }

        return ke;
    }

    /* Wait for keypress, while also checking for net input */
    while (1)
    {
        /* Look for a keypress */
        if (inkey_handler)
        {
            errr res = inkey_handler(&ke, false, true);

            /* Wait only as long as keymap activation would wait */
            if ((scan_cutoff != SCAN_OFF) && res)
            {
                /* Increase "wait" */
                w++;

                /* Excessive delay */
                if (w >= scan_cutoff)
                {
                    ke.type = EVT_DELAY;
                    break;
                }

                /* Delay */
                Term_xtra(TERM_XTRA_DELAY, 100);
            }
        }

        /* Call our callback */
        if (callback_begin) callback_begin(&ke);

        /* If we got a key, break */
        if (ke.type != EVT_NONE) break;

        /* Update our timer and if necessary send a keepalive packet */
        do_keepalive();

        /* Flush the network output buffer */
        if (Net_flush() == -1)
        {
            plog("Bad net flush");
            ke.type = EVT_ERROR;
            send_quit = false;
            break;
        }

        /* Wait for .001 sec, or until there is net input */
        SetTimeout(0, 1000);

        /* Update the screen */
        Term_fresh();

        /* Parse net input if we got any */
        if (SocketReadable(net_fd) && (Net_input() == -1))
        {
            ke.type = EVT_ERROR;
            send_quit = false;
            break;
        }

        /* Call our callback */
        if (callback_end) callback_end();

        /* Redraw */
        if (player && player->upkeep) redraw_stuff();
    }

    return ke;
}


/* Turn off the num-lock key by toggling it if it's currently on. */
void turn_off_numlock(void)
{
    KEYBDINPUT k;
    INPUT inp;
    int r;

    /* Extract the "disable_numlock" flag */
    bool disable_numlock = conf_get_int("MAngband", "DisableNumlock", true);

    /* Nothing to do if user doesn't want to auto-kill numlock */
    if (!disable_numlock) return;

    /* Already off? Then nothing to do */
    if (!(GetKeyState(VK_NUMLOCK) & 0xFFFF)) return;

    /* Numlock key */
    k.wVk = VK_NUMLOCK;
    k.wScan = 0;
    k.time = 0;
    k.dwFlags = 0;
    k.dwExtraInfo = 0;

    /* Press it */
    inp.type = INPUT_KEYBOARD;
    inp.ki = k;
    r = SendInput(1, &inp, sizeof(INPUT));
    if (!r) plog_fmt("SendInput error (down): %lu", GetLastError());

    /* Release it */
    k.dwFlags = KEYEVENTF_KEYUP;
    inp.type = INPUT_KEYBOARD;
    inp.ki = k;
    r = SendInput(1, &inp, sizeof(INPUT));
    if (!r) plog_fmt("SendInput error (up): %lu", GetLastError());
}


/*** Player access functions ***/


const char *get_title(struct player *p)
{
    return title;
}


s16b get_speed(struct player *p)
{
    return p->state.speed;
}


void get_plusses(struct player *p, struct player_state *state, int* dd, int* ds, int* mhit,
    int* mdam, int* shit, int* sdam)
{
    *dd = dis_dd;
    *ds = dis_ds;
    *mhit = dis_to_mhit;
    *shit = dis_to_shit;
    *mdam = dis_to_mdam;
    *sdam = dis_to_sdam;
}


s16b get_melee_skill(struct player *p)
{
    return p->state.skills[SKILL_TO_HIT_MELEE];
}


s16b get_ranged_skill(struct player *p)
{
    return p->state.skills[SKILL_TO_HIT_BOW];
}


byte get_dtrap(struct player *p)
{
    return trap_indicator;
}


int get_diff(struct player *p)
{
    return p->upkeep->inven_cnt;
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
ui_event textui_get_command(void)
{
    int mode = (OPT(player, rogue_like_commands)? KEYMAP_MODE_ROGUE: KEYMAP_MODE_ORIG);
    ui_event ke = EVENT_EMPTY;
    const struct keypress *act = NULL;

    /* Reset command */
    ui_event ke0 = EVENT_EMPTY;

    /* Activate "command mode" */
    inkey_flag = true;

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
        bool keymap_ok = true;

        switch (ke.key.code)
        {
            /* Allow "keymaps" to be bypassed */
            case '\\':
            {
                get_com_ex("Command: ", &ke);
                keymap_ok = false;
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

        first_escape = true;

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
bool textui_process_key(struct keypress kp, unsigned char *c)
{
    keycode_t key = kp.code;

    /* Null command */
    if ((key == '\0') || (key == ESCAPE) || (key == ' ') || (key == '\a'))
        return true;

    /* Invalid keypress */
    if (key > UCHAR_MAX)
        return false;

    *c = key;
    return true;
}


/*
 * Flush the output before displaying for emphasis
 */
void bell_message(game_event_type unused, game_event_data *data, void *user)
{
    /* Flush the output */
    Term_fresh();

    /* Window stuff */
    player->upkeep->redraw |= (PR_MESSAGE);
    redraw_stuff();

    /* Make a bell noise (if allowed) */
    sound(MSG_BELL);

    /* Flush the input */
    flush(unused, data, user);
}
