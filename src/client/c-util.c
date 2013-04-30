/*
 * File: c-util.c
 * Purpose: Gamma correction, some high-level UI functions, inkey()
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "../common/spells.h"
#include "grafmode.h"
#include "keymap.h"
#include "netclient.h"
#include "prefs.h"


/* See the "inkey()" function */
bool inkey_xtra = FALSE;
char inkey_scan = SCAN_OFF;
bool inkey_flag = FALSE;


/*
 * Flush all pending input.
 *
 * Actually, remember the flush, using the "inkey_xtra" flag, and in the
 * next call to "inkey()", perform the actual flushing, for efficiency,
 * and correctness of the "inkey()" function.
 */
void flush(void)
{
    /* Do it later */
    inkey_xtra = TRUE;
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

    /* Efficiency/Hack -- Ignore escape key for keymaps */
    if (is_escape(ke))
    {
        first_escape = TRUE;
        return (ke);
    }
    
    return (ke);
}


/*
 * Mega-Hack -- special "inkey_next" pointer.  XXX XXX XXX
 *
 * This special pointer allows a sequence of keys to be "inserted" into
 * the stream of keys returned by "inkey()".  This key sequence cannot be
 * bypassed by the Borg.  We use it to implement keymaps.
 */
struct keypress *inkey_next = NULL;


/*
 * Get a keypress from the user.
 *
 * This function recognizes a few "global parameters".  These are variables
 * which, if set to TRUE before calling this function, will have an effect
 * on this function, and which are always reset to FALSE by this function
 * before this function returns.  Thus they function just like normal
 * parameters, except that most calls to this function can ignore them.
 *
 * If "inkey_xtra" is TRUE, then all pending keypresses will be flushed.
 * This is set by flush(), which doesn't actually flush anything itself
 * but uses that flag to trigger delayed flushing.
 *
 * If "inkey_scan" is TRUE, then we will immediately return "zero" if no
 * keypress is available, instead of waiting for a keypress.
 *
 * If "inkey_flag" is TRUE, then we are waiting for a command in the main
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
    bool done = FALSE;
    term *old = Term;

    /* Delayed flush */
    if (inkey_xtra)
    {
        Term_flush();
        inkey_next = NULL;
        inkey_xtra = FALSE;
    }

    /* Hack -- Use the "inkey_next" pointer */
    if (inkey_next && inkey_next->code)
    {
        /* Get next character, and advance */
        ke.key = *inkey_next++;

        /* Cancel the various "global parameters" */
        inkey_flag = FALSE;
        inkey_scan = SCAN_OFF;

        /* Accept result */
        return (ke);
    }

    /* Forget pointer */
    inkey_next = NULL;

    /* Get the cursor state */
    Term_get_cursor(&cursor_state);

    /* Show the cursor if waiting, except sometimes in "command" mode */
    if (!inkey_scan && (!inkey_flag || p_ptr->screen_icky))
        Term_set_cursor(TRUE);

    /* Hack -- Activate main screen */
    Term_activate(term_screen);

    /* Get a key */
    while (ke.type == EVT_NONE)
    {
        /* Hack -- Handle (inkey_scan == SCAN_INSTANT) */
        if ((inkey_scan == SCAN_INSTANT) && (0 != Term_inkey(&kk, FALSE, FALSE)))
            break;

        /* Hack -- Flush output once when no key ready */
        if (!done && (0 != Term_inkey(&kk, FALSE, FALSE)))
        {
            /* Hack -- activate proper term */
            Term_activate(old);

            /* Flush output */
            Term_fresh();

            /* Hack -- activate main screen */
            Term_activate(term_screen);

            /* Only once */
            done = TRUE;
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
    inkey_flag = FALSE;
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


/*
 * Flush the screen, make a noise
 */
void bell(const char *reason)
{
    /* Mega-Hack -- Flush the output */
    Term_fresh();

    /* Hack -- Memorize the reason if possible */
    if (reason)
    {
        message_add(reason, MSG_BELL);

        /* Window stuff */
        p_ptr->redraw |= (PR_MESSAGE);
        redraw_stuff();
    }

    /* Make a bell noise (if allowed) */
    sound(MSG_BELL);

    /* Flush the input (later!) */
    flush();
}


/*
 * Hack -- Make a (relevant?) sound
 */
void sound(int val)
{
    /* No sound */
    if (!OPT(use_sound) || !sound_hook) return;

    sound_hook(val);
}


/*
 * Output a message to the top line of the screen.
 *
 * Long messages are truncated.
 *
 * These messages are memorized for later reference (see above).
 *
 * We must be very careful about using the "msg()" functions without
 * explicitly calling the special "msg(NULL)" function, since this may
 * result in the loss of information if the screen is cleared, or if anything
 * is displayed on the top line.
 *
 * Hack -- Note that "msg(NULL)" will clear the top line even if no
 * messages are pending.
 */
void c_msg_print_aux(const char *msg, u16b type)
{
    char buf[MSG_LEN];

    /* Hack -- Reset */
    prt("", 0, 0);

    /* Redraw */
    p_ptr->redraw |= (PR_MESSAGE | PR_MESSAGE_CHAT);

    /* No message */
    if (!msg) return;

    /* Memorize the message */
    message_add(msg, type);

    /* Copy it */
    my_strcpy(buf, msg, sizeof(buf));

    /* Display the message */
    Term_putstr(0, 0, strlen(buf), TERM_WHITE, buf);
}


/*
 * Print a message in the default color (white)
 */
void c_msg_print(const char *msg)
{
    c_msg_print_aux(msg, MSG_LOCAL);
}


/*
 * Save the screen, and increase the "icky" depth.
 *
 * This function must match exactly one call to "screen_load()".
 */
void screen_save(void)
{
    /* Increase "icky" depth */
    p_ptr->screen_icky++;
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
    p_ptr->screen_icky--;
    Send_icky();

    /* Flush any queued events */
    if (flush) Flush_queue();

    /* Hack -- Redraw distorted graphics */
    if (!p_ptr->screen_icky && (tile_distorted || full_icky_screen)) Term_redraw();
}


/*
 * Display a string on the screen using an attribute.
 *
 * At the given location, using the given attribute, if allowed,
 * add the given string.  Do not clear the line.
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
    Term_putstr(col, row, -1, TERM_WHITE, str);
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
    c_prt(TERM_WHITE, str, row, col);
}


void prt_icky(const char *str, int row, int col)
{
    /* Clear line, position cursor */
    Term_erase_icky(col, row, 255);

    /* Dump the attr/text */
    Term_addstr(-1, TERM_WHITE, str);
}


void clear_from(int row)
{
    int y;

    /* Erase requested rows */
    for (y = row; y < Term->hgt; y++)
    {
        /* Erase part of the screen */
        Term_erase(0, y, 255);
    }
}


/*
 * The default "keypress handling function" for askfor_aux, this takes the
 * given keypress, input buffer, length, etc, and does the appropriate action
 * for each keypress, such as moving the cursor left or inserting a character.
 *
 * It should return TRUE when editing of the buffer is "complete" (e.g. on
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
            return TRUE;
        }

        case KC_ENTER:
        {
            *curs = *len;
            return TRUE;
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
    return FALSE;
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
 * Return accepts the current buffer contents and returns TRUE.
 * Escape clears the buffer and the window and returns FALSE.
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
    struct keypress ch = {0};
    bool done = FALSE;
    bool firsttime = TRUE;

    if (keypress_h == NULL) keypress_h = askfor_aux_keypress;

    /* Locate the cursor */
    Term_locate(&x, &y);

    /* The top line is "icky" */
    topline_icky = TRUE;

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
    Term_putstr(x, y, -1, TERM_YELLOW, buf);

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
        Term_putstr(x, y, -1, TERM_WHITE, buf);

        /* Not the first time round anymore */
        firsttime = FALSE;
    }

    /* The top line is OK now */
    topline_icky = FALSE;
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
    int y, x, i;
    size_t k = 0;   /* Cursor position */
    size_t nul = 0; /* Position of the null byte in the string */
    ui_event ke = EVENT_EMPTY;
    bool done = FALSE;
    bool firsttime = TRUE;

    if (keypress_h == NULL) keypress_h = askfor_aux_keypress;

    /* Locate the cursor */
    Term_locate(&x, &y);

    /* The top line is "icky" */
    topline_icky = TRUE;

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
    Term_putstr(x, y, -1, TERM_YELLOW, buf);

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
            done = TRUE;
        }

        /* Let the keypress handler deal with the keypress */
        else done = keypress_h(buf, len, &k, &nul, ke.key, firsttime);

        Term_erase(x, y, len);

        /* Update the entry */
        if (!priv)
            Term_putstr(x, y, -1, TERM_WHITE, buf);
        else
        {
            for (i = 0; i < nul; i++)
                Term_putch(x + i, y, TERM_WHITE, 'x');
        }

        /* Not the first time round anymore */
        firsttime = FALSE;
    }

    /* The top line is OK now */
    topline_icky = FALSE;
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
 * We clear the input, and return FALSE, on "ESCAPE".
 */
bool get_string(const char *prompt, char *buf, int len)
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
int get_string_ex(const char *prompt, char *buf, int len, bool priv)
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
s32b get_quantity(const char *prompt, s32b max)
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
s32b get_quantity_ex(const char *prompt, s32b max)
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
    res = get_string_ex(prompt, buf, 10, FALSE);
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
bool get_check(const char *prompt)
{
    struct keypress ke;
    char buf[NORMAL_WID];

    /* Option -- "auto_accept" */
    if (OPT(auto_accept)) return (TRUE);

    /* Hack -- Build a "useful" prompt */
    strnfmt(buf, NORMAL_WID - 2, "%.70s[y/n] ", prompt);

    /* The top line is "icky" */
    topline_icky = TRUE;

    /* Prompt for it */
    prt(buf, 0, 0);

    /* Get an acceptable answer */
    ke = inkey();

    /* Erase the prompt */
    prt("", 0, 0);

    /* The top line is OK again */
    topline_icky = FALSE;

    /* Flush any events that came in while we were icky */
    Flush_queue();

    /* Normal negation */
    if ((ke.code != 'Y') && (ke.code != 'y')) return (FALSE);

    /* Success */
    return (TRUE);
}


/*
 * Verify something with the user
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
int get_check_ex(const char *prompt)
{
    ui_event ke = EVENT_EMPTY;
    char buf[NORMAL_WID];

    /* Option -- "auto_accept" */
    if (OPT(auto_accept)) return 0;

    /* Hack -- Build a "useful" prompt */
    strnfmt(buf, NORMAL_WID - 2, "%.70s[y/n] ", prompt);

    /* The top line is "icky" */
    topline_icky = TRUE;

    /* Prompt for it */
    prt(buf, 0, 0);

    /* Get an acceptable answer */
    ke = inkey_ex();

    /* Erase the prompt */
    prt("", 0, 0);

    /* The top line is OK again */
    topline_icky = FALSE;

    /* Flush any events that came in while we were icky */
    Flush_queue();

    /* Handle abort */
    return_on_abort(ke);

    /* Success */
    if ((ke.type == EVT_KBRD) && ((ke.key.code == 'Y') || (ke.key.code == 'y'))) return 0;

    return 2;
}


/*
 * Prompts for a keypress
 *
 * The "prompt" should take the form "Command: "
 *
 * Returns TRUE unless the character is "Escape"
 */
bool get_com(const char *prompt, struct keypress *command)
{
    struct keypress ke;

    /* The top line is "icky" */
    topline_icky = TRUE;

    /* Display a prompt */
    prt(prompt, 0, 0);

    /* Get a key (important: bypass abort) */
    ke = inkey();

    /* Clear the prompt */
    prt("", 0, 0);

    /* Fix the top line */
    topline_icky = FALSE;

    /* Save the command */
    *command = ke;

    /* Flush any events */
    Flush_queue();

    /* Done */
    if (ke.code == ESCAPE) return FALSE;
    return TRUE;
}


bool get_com_ex(const char *prompt, ui_event *command)
{
    ui_event ke;

    /* The top line is "icky" */
    topline_icky = TRUE;

    /* Display a prompt */
    prt(prompt, 0, 0);

    /* Get a key */
    ke = inkey_ex();

    /* Clear the prompt */
    prt("", 0, 0);

    /* Fix the top line */
    topline_icky = FALSE;

    /* Save the command */
    *command = ke;

    /* Flush any events */
    Flush_queue();

    /* Done */
    if ((ke.type == EVT_KBRD) && (ke.key.code == ESCAPE)) return FALSE;
    return TRUE;
}


#ifdef SUPPORT_GAMMA

/*
 * XXX XXX XXX Important note about "colors" XXX XXX XXX
 *
 * The "TERM_*" color definitions list the "composition" of each
 * "Angband color" in terms of "quarters" of each of the three color
 * components (Red, Green, Blue), for example, TERM_UMBER is defined
 * as 2/4 Red, 1/4 Green, 0/4 Blue.
 *
 * These values are NOT gamma-corrected.  On most machines (with the
 * Macintosh being an important exception), you must "gamma-correct"
 * the given values, that is, "correct for the intrinsic non-linearity
 * of the phosphor", by converting the given intensity levels based
 * on the "gamma" of the target screen, which is usually 1.7 (or 1.5).
 *
 * The actual formula for conversion is unknown to me at this time,
 * but you can use the table below for the most common gamma values.
 *
 * So, on most machines, simply convert the values based on the "gamma"
 * of the target screen, which is usually in the range 1.5 to 1.7, and
 * usually is closest to 1.7.  The converted value for each of the five
 * different "quarter" values is given below:
 *
 *  Given     Gamma 1.0       Gamma 1.5       Gamma 1.7     Hex 1.7
 *  -----       ----            ----            ----          ---
 *   0/4        0.00            0.00            0.00          #00
 *   1/4        0.25            0.27            0.28          #47
 *   2/4        0.50            0.55            0.56          #8f
 *   3/4        0.75            0.82            0.84          #d7
 *   4/4        1.00            1.00            1.00          #ff
 *
 * Note that some machines (i.e. most IBM machines) are limited to a
 * hard-coded set of colors, and so the information above is useless.
 *
 * Also, some machines are limited to a pre-determined set of colors,
 * for example, the IBM can only display 16 colors, and only 14 of
 * those colors resemble colors used by Angband, and then only when
 * you ignore the fact that "Slate" and "cyan" are not really matches,
 * so on the IBM, we use "orange" for both "Umber", and "Light Umber"
 * in addition to the obvious "Orange", since by combining all of the
 * "indeterminate" colors into a single color, the rest of the colors
 * are left with "meaningful" values.
 */

/* Table of gamma values */
byte gamma_table[256];

/* Table of ln(x / 256) * 256 for x going from 0 -> 255 */
static const s16b gamma_helper[256] =
{
    0, -1420, -1242, -1138, -1065, -1007, -961, -921, -887, -857, -830,
    -806, -783, -762, -744, -726, -710, -694, -679, -666, -652, -640,
    -628, -617, -606, -596, -586, -576, -567, -577, -549, -541, -532,
    -525, -517, -509, -502, -495, -488, -482, -475, -469, -463, -457,
    -451, -455, -439, -434, -429, -423, -418, -413, -408, -403, -398,
    -394, -389, -385, -380, -376, -371, -367, -363, -359, -355, -351,
    -347, -343, -339, -336, -332, -328, -325, -321, -318, -314, -311,
    -308, -304, -301, -298, -295, -291, -288, -285, -282, -279, -276,
    -273, -271, -268, -265, -262, -259, -257, -254, -251, -248, -246,
    -243, -241, -238, -236, -233, -231, -228, -226, -223, -221, -219,
    -216, -214, -212, -209, -207, -205, -203, -200, -198, -196, -194,
    -192, -190, -188, -186, -184, -182, -180, -178, -176, -174, -172,
    -170, -168, -166, -164, -162, -160, -158, -156, -155, -153, -151,
    -149, -147, -146, -144, -142, -140, -139, -137, -135, -134, -132,
    -130, -128, -127, -125, -124, -122, -120, -119, -117, -116, -114,
    -112, -111, -109, -108, -106, -105, -103, -102, -100, -99, -97, -96,
    -95, -93, -92, -90, -89, -87, -86, -85, -83, -82, -80, -79, -78,
    -76, -75, -74, -72, -71, -70, -68, -67, -66, -65, -63, -62, -61,
    -59, -58, -57, -56, -54, -53, -52, -51, -50, -48, -47, -46, -45,
    -44, -42, -41, -40, -39, -38, -37, -35, -34, -33, -32, -31, -30,
    -29, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16,
    -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1
};


/*
 * Build the gamma table so that floating point isn't needed.
 *
 * Note gamma goes from 0->256.  The old value of 100 is now 128.
 */
void build_gamma_table(int gamma)
{
    int i, n;

    /*
     * value is the current sum.
     * diff is the new term to add to the series.
     */
    long value, diff;

    /* Hack - convergence is bad in these cases. */
    gamma_table[0] = 0;
    gamma_table[255] = 255;

    for (i = 1; i < 255; i++)
    {
        /*
         * Initialise the Taylor series
         *
         * value and diff have been scaled by 256
         */
        n = 1;
        value = 256L * 256L;
        diff = ((long)gamma_helper[i]) * (gamma - 256);

        while (diff)
        {
            value += diff;
            n++;

            /*
             * Use the following identiy to calculate the gamma table.
             * exp(x) = 1 + x + x^2/2 + x^3/(2*3) + x^4/(2*3*4) +...
             *
             * n is the current term number.
             *
             * The gamma_helper array contains a table of
             * ln(x/256) * 256
             * This is used because a^b = exp(b*ln(a))
             *
             * In this case:
             * a is i / 256
             * b is gamma.
             *
             * Note that everything is scaled by 256 for accuracy,
             * plus another factor of 256 for the final result to
             * be from 0-255.  Thus gamma_helper[] * gamma must be
             * divided by 256*256 each itteration, to get back to
             * the original power series.
             */
            diff = (((diff / 256) * gamma_helper[i]) * (gamma - 256)) / (256 * n);
        }

        /*
         * Store the value in the table so that the
         * floating point pow function isn't needed.
         */
        gamma_table[i] = (byte)(((long)(value / 256) * i) / 256);
    }
}

#endif /* SUPPORT_GAMMA */


void flush_now(void)
{
    /* Clear various flags */
    inkey_xtra = FALSE;

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
        if (OPT(rogue_like_commands)) mode = KEYMAP_MODE_ROGUE;

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
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * The direction "5" is special, and means "use current target".
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 */
bool get_aim_dir(int *dp)
{
    int dir;
    ui_event ke;
    const char *p;

    /* Initialize */
    (*dp) = 0;

    /* Global direction */
    dir = 0;

    /* Ask until satisfied */
    while (!dir)
    {
        bool res;

        /* Choose a prompt */
        p = "Direction ('*' to target, \"'\" for closest, '(' for friendly, Escape to cancel)? ";

        /* Get a command (or Cancel) */
        Term->no_cursor = TRUE;
        res = get_com(p, &ke.key);
        Term->no_cursor = FALSE;
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

                        if ((lazymove_delay == 0) || (++keypresses_handled > 1))
                            break;

                        /* See if there's a second keypress within the defined period of time */
                        inkey_scan = lazymove_delay;
                        ke = inkey_ex();
                    }
                }
            }
        }

        /* Error */
        if (!dir) bell("Illegal aim direction!");
    }

    /* No direction */
    if (!dir) return (FALSE);

    /* Save direction */
    (*dp) = dir;

    /* A "valid" direction was entered */
    return (TRUE);
}


/*
 * Get an "aiming direction" (1,2,3,4,6,7,8,9 or 5) from the user.
 *
 * Return 1 on abort, 2 on escape, 0 otherwise.
 */
int get_aim_dir_ex(int *dp)
{
    int dir;
    ui_event ke;

    /* Initialize */
    (*dp) = 0;

    /* Global direction */
    dir = 0;

    /* The top line is "icky" */
    topline_icky = TRUE;

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
    topline_icky = FALSE;

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


void prt_num(const char *header, int num, int row, int col, byte color)
{
    int len = strlen(header);
    char out_val[32];
    put_str(header, row, col);
    put_str("   ", row, col + len);
    strnfmt(out_val, sizeof(out_val), "%6ld", (long)num);
    c_put_str(color, out_val, row, col + len + 3);
}


void prt_lnum(const char *header, s32b num, int row, int col, byte color)
{
    int len = strlen(header);
    char out_val[32];
    put_str(header, row, col);
    strnfmt(out_val, sizeof(out_val), "%9ld", (long)num);
    c_put_str(color, out_val, row, col + len);
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
        dest[i].a = TERM_WHITE;
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
    return (p+1);
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
        if (inkey_handler) inkey_handler(&ke, TRUE, TRUE);

        /* Report error */
        else
        {
            plog("Bad socket file descriptor");
            send_quit = FALSE;
        }

        return ke;
    }

    /* Wait for keypress, while also checking for net input */
    while (1)
    {
        /* Look for a keypress */
        if (inkey_handler)
        {
            errr res = inkey_handler(&ke, FALSE, TRUE);

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
            send_quit = FALSE;
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
            send_quit = FALSE;
            break;
        }

        /* Call our callback */
        if (callback_end) callback_end();

        /* Redraw */
        redraw_stuff();
    }

    return ke;
}


/*
 * Given a "formatted" chunk of text (i.e. one including tags like {red}{/})
 * in 'source', with starting point 'init', this finds the next section of
 * text and any tag that goes with it, return TRUE if it finds something to
 * print.
 *
 * If it returns TRUE, then it also fills 'text' with a pointer to the start
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
    if (*text[0] == '\0') return FALSE;

    next = strchr(*text, '{');
    while (next)
    {
        const char *s = next + 1;

        while (*s && isalpha((unsigned char) *s)) s++;

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
                    return TRUE;
                }

                /* Otherwise return the chunk up to this */
                else
                {
                    *len = next - *text;
                    *end = *text + *len;
                    return TRUE;
                }
            }

            /* No closing thing, therefore all one lump of text. */
            else
            {
                *len = strlen(*text);
                *end = *text + *len;
                return TRUE;
            }
        }

        /* End of the string, that's fine. */
        else if (*s == '\0')
        {
            *len = strlen(*text);
            *end = *text + *len;
            return TRUE;
        }

        /* An invalid tag, skip it. */
        else
            next = next + 1;

        next = strchr(next, '{');
    }

    /* Default to the rest of the string */
    *len = strlen(*text);
    *end = *text + *len;

    return TRUE;
}


/*
 * Output text to the screen or to a file depending on the
 * selected hook.  Takes strings with "embedded formatting",
 * such that something within {red}{/} will be printed in red.
 *
 * Note that such formatting will be treated as a "breakpoint"
 * for the printing, so if used within words may lead to part of the
 * word being moved to the next line.
 */
void text_out_e(const char *buf, int y)
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
            char tagbuffer[11];

            /* Colour names are less than 11 characters long. */
            assert(taglen < 11);

            memcpy(tagbuffer, tag, taglen);
            tagbuffer[taglen] = '\0';

            a = color_text_to_attr(tagbuffer);
        }

        if (a == -1) a = TERM_WHITE;

        /* Output now */
        c_put_str(a, smallbuf, y, x);

        /* Advance */
        x += strlen(smallbuf);

        start = next;
    }
}


/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 */
void reset_visuals(bool load_prefs)
{
    /* Nothing to do */
    if (!z_info) return;

    /* Reset the Client_setup info */
    memset(Client_setup.flvr_x_attr, 0, flavor_max);
    memset(Client_setup.flvr_x_char, 0, flavor_max);
    memset(Client_setup.f_attr, 0, z_info->f_max * FEAT_LIGHTING_MAX);
    memset(Client_setup.f_char, 0, z_info->f_max * FEAT_LIGHTING_MAX);
    memset(Client_setup.k_attr, 0, z_info->k_max);
    memset(Client_setup.k_char, 0, z_info->k_max);
    memset(Client_setup.r_attr, 0, z_info->r_max);
    memset(Client_setup.r_char, 0, z_info->r_max);
    memset(Client_setup.tval_attr, 0, 128);
    memset(Client_setup.gf_attr, 0, GF_MAX * BOLT_MAX);
    memset(Client_setup.gf_char, 0, GF_MAX * BOLT_MAX);

    /* Don't load the user pref file */
    if (!load_prefs) return;

    /* Graphic symbols */
    if (p_ptr->use_graphics)
    {
        /* If we have a graphics mode, see if the mode has a pref file name */
        graphics_mode *mode = get_graphics_mode(p_ptr->use_graphics, TRUE);

        if (mode && strstr(mode->pref, ".prf"))
            process_pref_file(mode->pref, FALSE, FALSE);

        /* Process "graf.prf" */
        else
            process_pref_file("graf.prf", FALSE, FALSE);
    }

    /* Normal symbols */
    else
    {
        /* Process "font.prf" */
        process_pref_file("font.prf", FALSE, FALSE);
    }
}


/* Turn off the num-lock key by toggling it if it's currently on. */
void turn_off_numlock(void)
{
    KEYBDINPUT k;
    INPUT inp;
    int r;

    /* Extract the "disable_numlock" flag */
    bool disable_numlock = conf_get_int("MAngband", "DisableNumlock", TRUE);

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


player_type *get_player(int Ind)
{
    return p_ptr;
}


const char *get_title(struct player *p)
{
    return title;
}


s16b get_speed(struct player *p)
{
    return p->state.speed;
}


void get_plusses(struct player *p, int* pfhit, int* pfdam, int* pmhit, int* pmdam,
    int* pshit, int* psdam)
{
    *pfhit = p->state.dis_to_h;
    *pmhit = dis_to_mhit;
    *pshit = dis_to_shit;
    *pfdam = p->state.dis_to_d;
    *pmdam = dis_to_mdam;
    *psdam = dis_to_sdam;
}


byte get_dtrap(struct player *p)
{
    return trap_indicator;
}
