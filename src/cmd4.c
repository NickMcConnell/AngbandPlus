/* File: cmd4.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Interface commands */

#include "angband.h"
#include "equip.h"
#include "int_map.h"
#include "z-doc.h"

#include <assert.h>

static void browser_cursor(char ch, int *column, int *grp_cur, int grp_cnt, int *list_cur, int list_cnt);

/*
 * A set of functions to maintain automatic dumps of various kinds.
 * -Mogami-
 *
 * remove_auto_dump(orig_file, mark)
 *     Remove the old automatic dump of type "mark".
 * auto_dump_printf(fmt, ...)
 *     Dump a formatted string using fprintf().
 * open_auto_dump(buf, mark)
 *     Open a file, remove old dump, and add new header.
 * close_auto_dump(void)
 *     Add a footer, and close the file.
 *
 *    The dump commands of original Angband simply add new lines to
 * existing files; these files will become bigger and bigger unless
 * an user deletes some or all of these files by hand at some
 * point.
 *
 *     These three functions automatically delete old dumped lines
 * before adding new ones. Since there are various kinds of automatic
 * dumps in a single file, we add a header and a footer with a type
 * name for every automatic dump, and kill old lines only when the
 * lines have the correct type of header and footer.
 *
 *     We need to be quite paranoid about correctness; the user might
 * (mistakenly) edit the file by hand, and see all their work come
 * to nothing on the next auto dump otherwise. The current code only
 * detects changes by noting inconsistencies between the actual number
 * of lines and the number written in the footer. Note that this will
 * not catch single-line edits.
 */

/*
 *  Mark strings for auto dump
 */
static char auto_dump_header[] = "# vvvvvvv== %s ==vvvvvvv";
static char auto_dump_footer[] = "# ^^^^^^^== %s ==^^^^^^^";

/*
 * Variables for auto dump
 */
static FILE *auto_dump_stream;
static cptr auto_dump_mark;
static int auto_dump_line_num;

/*
 * Remove old lines automatically generated before.
 */
static void remove_auto_dump(cptr orig_file)
{
    FILE *tmp_fff, *orig_fff;

    char tmp_file[1024];
    char buf[1024];
    bool between_mark = FALSE;
    bool changed = FALSE;
    int line_num = 0;
    long header_location = 0;
    char header_mark_str[80];
    char footer_mark_str[80];
    size_t mark_len;

    /* Prepare a header/footer mark string */
    sprintf(header_mark_str, auto_dump_header, auto_dump_mark);
    sprintf(footer_mark_str, auto_dump_footer, auto_dump_mark);

    mark_len = strlen(footer_mark_str);

    /* Open an old dump file in read-only mode */
    orig_fff = my_fopen(orig_file, "r");

    /* If original file does not exist, nothing to do */
    if (!orig_fff) return;

    /* Open a new (temporary) file */
    tmp_fff = my_fopen_temp(tmp_file, 1024);

    if (!tmp_fff)
    {
        msg_format("Failed to create temporary file %s.", tmp_file);
        msg_print(NULL);
        return;
    }

    /* Loop for every line */
    while (TRUE)
    {
        /* Read a line */
        if (my_fgets(orig_fff, buf, sizeof(buf)))
        {
            /* Read error: Assume End of File */

            /*
             * Was looking for the footer, but not found.
             *
             * Since automatic dump might be edited by hand,
             * it's dangerous to kill these lines.
             * Seek back to the next line of the (pseudo) header,
             * and read again.
             */
            if (between_mark)
            {
                fseek(orig_fff, header_location, SEEK_SET);
                between_mark = FALSE;
                continue;
            }

            /* Success -- End the loop */
            else
            {
                break;
            }
        }

        /* We are looking for the header mark of automatic dump */
        if (!between_mark)
        {
            /* Is this line a header? */
            if (!strcmp(buf, header_mark_str))
            {
                /* Memorise seek point of this line */
                header_location = ftell(orig_fff);

                /* Initialize counter for number of lines */
                line_num = 0;

                /* Look for the footer from now */
                between_mark = TRUE;

                /* There are some changes */
                changed = TRUE;
            }

            /* Not a header */
            else
            {
                /* Copy orginally lines */
                fprintf(tmp_fff, "%s\n", buf);
            }
        }

        /* We are looking for the footer mark of automatic dump */
        else
        {
            /* Is this line a footer? */
            if (!strncmp(buf, footer_mark_str, mark_len))
            {
                int tmp;

                /*
                 * Compare the number of lines
                 *
                 * If there is an inconsistency between
                 * actual number of lines and the
                 * number here, the automatic dump
                 * might be edited by hand. So it's
                 * dangerous to kill these lines.
                 * Seek back to the next line of the
                 * (pseudo) header, and read again.
                 */
                if (!sscanf(buf + mark_len, " (%d)", &tmp)
                    || tmp != line_num)
                {
                    fseek(orig_fff, header_location, SEEK_SET);
                }

                /* Look for another header */
                between_mark = FALSE;
            }

            /* Not a footer */
            else
            {
                /* Ignore old line, and count number of lines */
                line_num++;
            }
        }
    }

    /* Close files */
    my_fclose(orig_fff);
    my_fclose(tmp_fff);

    /* If there are some changes, overwrite the original file with new one */
    if (changed)
    {
        /* Copy contents of temporary file */

        tmp_fff = my_fopen(tmp_file, "r");
        orig_fff = my_fopen(orig_file, "w");

        while (!my_fgets(tmp_fff, buf, sizeof(buf)))
            fprintf(orig_fff, "%s\n", buf);

        my_fclose(orig_fff);
        my_fclose(tmp_fff);
    }

    /* Kill the temporary file */
    fd_kill(tmp_file);

    return;
}


/*
 * Dump a formatted line, using "vstrnfmt()".
 */
static void auto_dump_printf(cptr fmt, ...)
{
    cptr p;
    va_list vp;

    char buf[1024];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    (void)vstrnfmt(buf, sizeof(buf), fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Count number of lines */
    for (p = buf; *p; p++)
    {
        if (*p == '\n') auto_dump_line_num++;
    }

    /* Dump it */
    fprintf(auto_dump_stream, "%s", buf);
}


/*
 *  Open file to append auto dump.
 */
static bool open_auto_dump(cptr buf, cptr mark)
{

    char header_mark_str[80];

    /* Save the mark string */
    auto_dump_mark = mark;

    /* Prepare a header mark string */
    sprintf(header_mark_str, auto_dump_header, auto_dump_mark);

    /* Remove old macro dumps */
    remove_auto_dump(buf);

    /* Append to the file */
    auto_dump_stream = my_fopen(buf, "a");

    /* Failure */
    if (!auto_dump_stream) {
        msg_format("Failed to open %s.", buf);
        msg_print(NULL);

        /* Failed */
        return FALSE;
    }

    /* Start dumping */
    fprintf(auto_dump_stream, "%s\n", header_mark_str);

    /* Initialize counter */
    auto_dump_line_num = 0;

    auto_dump_printf("# *Warning!*  The lines below are an automatic dump.\n");
    auto_dump_printf("# Don't edit them; changes will be deleted and replaced automatically.\n");

    /* Success */
    return TRUE;
}

/*
 *  Append foot part and close auto dump.
 */
static void close_auto_dump(void)
{
    char footer_mark_str[80];

    /* Prepare a footer mark string */
    sprintf(footer_mark_str, auto_dump_footer, auto_dump_mark);

    auto_dump_printf("# *Warning!*  The lines above are an automatic dump.\n");
    auto_dump_printf("# Don't edit them; changes will be deleted and replaced automatically.\n");

    /* End of dump */
    fprintf(auto_dump_stream, "%s (%d)\n", footer_mark_str, auto_dump_line_num);

    /* Close */
    my_fclose(auto_dump_stream);

    return;
}


/*
 * Return suffix of ordinal number
 */
cptr get_ordinal_number_suffix(int num)
{
    num = ABS(num) % 100;
    switch (num % 10)
    {
    case 1:
        return (num == 11) ? "th" : "st";
    case 2:
        return (num == 12) ? "th" : "nd";
    case 3:
        return (num == 13) ? "th" : "rd";
    default:
        return "th";
    }
}

/*
 * Hack -- redraw the screen
 *
 * This command performs various low level updates, clears all the "extra"
 * windows, does a total redraw of the main window, and requests all of the
 * interesting updates and redraws that I can think of.
 *
 * This command is also used to "instantiate" the results of the user
 * selecting various things, such as graphics mode, so it must call
 * the "TERM_XTRA_REACT" hook before redrawing the windows.
 */
bool redraw_hack;
void do_cmd_redraw(void)
{
    int j;

    term *old = Term;


    /* Hack -- react to changes */
    Term_xtra(TERM_XTRA_REACT, 0);


    /* Combine and Reorder the pack (later) */
    plr->notice |= (PN_OPTIMIZE_PACK | PN_OPTIMIZE_QUIVER);


    /* Update torch */
    plr->update |= (PU_TORCH);

    /* Update stuff */
    plr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Forget lite/view */
    plr->update |= (PU_UN_VIEW | PU_UN_LIGHT);

    /* Update lite/view */
    plr->update |= (PU_VIEW | PU_LIGHT | PU_MON_LIGHT);

    /* Update monsters */
    plr->update |= (PU_MONSTERS);

    /* Redraw everything */
    plr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY | PR_MSG_LINE);

    /* Window stuff */
    plr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL);

    /* Window stuff */
    plr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON |
        PW_MONSTER | PW_MONSTER_LIST | PW_OBJECT_LIST | PW_OBJECT | PW_WORLD_MAP);

    /* Prevent spamming ^R to circumvent fuzzy detection */
    redraw_hack = TRUE;
    handle_stuff();
    redraw_hack = FALSE;

    if (plr->prace == RACE_ANDROID) android_calc_exp();


    /* Redraw every window */
    for (j = 0; j < 8; j++)
    {
        /* Dead window */
        if (!angband_term[j]) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Redraw */
        Term_redraw();

        /* Refresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}

/*
 * Show previous messages to the user    -BEN-
 *
 */
void do_cmd_messages(int old_now_turn)
{
    int     i;
    doc_ptr doc;
    u32b    current_turn = 0;
    int     current_row = 0;

    doc = doc_alloc(80);
    for (i = msg_count() - 1; i >= 0; i--)
    {
        msg_ptr m = msg_get(i);

        if (m->turn != current_turn)
        {
            if (doc_cursor(doc).y > current_row + 1)
                doc_newline(doc);
            current_turn = m->turn;
            current_row = doc_cursor(doc).y;
        }

        doc_insert_text(doc, m->color, str_buffer(m->msg));
        if (m->count > 1)
        {
            char buf[10];
            sprintf(buf, " (x%d)", m->count);
            doc_insert_text(doc, m->color, buf);
        }
        doc_newline(doc);
    }
    screen_save();
    doc_display(doc, "Previous Messages", doc_cursor(doc).y);
    screen_load();
    doc_free(doc);
}

static option_type autosave_info[2] =
{
    { &autosave_l,      TRUE, 255, 0x01, 0x00,
        "autosave_l",    "Autosave when entering new levels" },


    { &autosave_t,      FALSE, 255, 0x02, 0x00,
        "autosave_t",   "Timed autosave" },

};


static s16b toggle_frequency(s16b current)
{
    switch (current)
    {
    case 0: return 50;
    case 50: return 100;
    case 100: return 250;
    case 250: return 500;
    case 500: return 1000;
    case 1000: return 2500;
    case 2500: return 5000;
    case 5000: return 10000;
    case 10000: return 25000;
    default: return 0;
    }
}


/*
 * Interact with some options for cheating
 */
static void do_cmd_options_autosave(cptr info)
{
    char    ch;

    int     i, k = 0, n = 2;

    char    buf[80];


    /* Clear screen */
    Term_clear();

    /* Interact with the player */
    while (TRUE)
    {
        /* Prompt XXX XXX XXX */
        sprintf(buf, "%s (RET to advance, y/n to set, 'F' for frequency, ESC to accept) ", info);

        prt(buf, 0, 0);

        /* Display the options */
        for (i = 0; i < n; i++)
        {
            byte a = TERM_WHITE;

            /* Color current option */
            if (i == k) a = TERM_L_BLUE;

            /* Display the option text */
            sprintf(buf, "%-48s: %s (%s)",
                autosave_info[i].o_desc,
                (*autosave_info[i].o_var ? "yes" : "no "),

                autosave_info[i].o_text);
            c_prt(a, buf, i + 2, 0);
        }

        prt(format("Timed autosave frequency: every %d turns",  autosave_freq), 5, 0);



        /* Hilite current option */
        move_cursor(k + 2, 50);

        /* Get a key */
        ch = inkey();

        /* Analyze */
        switch (ch)
        {
            case ESCAPE:
            {
                return;
            }

            case '-':
            case '8':
            {
                k = (n + k - 1) % n;
                break;
            }

            case ' ':
            case '\n':
            case '\r':
            case '2':
            {
                k = (k + 1) % n;
                break;
            }

            case 'y':
            case 'Y':
            case '6':
            {

                (*autosave_info[k].o_var) = TRUE;
                k = (k + 1) % n;
                break;
            }

            case 'n':
            case 'N':
            case '4':
            {
                (*autosave_info[k].o_var) = FALSE;
                k = (k + 1) % n;
                break;
            }

            case 'f':
            case 'F':
            {
                autosave_freq = toggle_frequency(autosave_freq);
                prt(format("Timed autosave frequency: every %d turns",
                       autosave_freq), 5, 0);
                break;
            }

            case '?':
            {
                doc_display_help("option.txt", "Autosave");

                Term_clear();
                break;
            }

            default:
            {
                bell();
                break;
            }
        }
    }
}


/*
 * Interact with some options
 */
void do_cmd_options_aux(int page, cptr info)
{
    char    ch;
    int     i, k = 0, n = 0, l;
    int     opt[32];
    char    buf[80];
    bool    browse_only = (page == OPT_PAGE_BIRTH) && character_generated &&
                          (!plr->wizard || !allow_debug_opts);

/*    browse_only = FALSE; */

    /* Lookup the options */
    for (i = 0; i < 32; i++) opt[i] = 0;

    /* Scan the options */
    for (i = 0; option_info[i].o_desc; i++)
    {
        /* Notice options on this "page" */
        if (option_info[i].o_page == page) opt[n++] = i;
    }


    /* Clear screen */
    Term_clear();

    /* Interact with the player */
    while (TRUE)
    {
        int dir;

        /* Prompt XXX XXX XXX */
        sprintf(buf, "%s (RET:next, %s, ?:help) ", info, browse_only ? "ESC:exit" : "y/n:change, ESC:accept");

        prt(buf, 0, 0);


        /* HACK -- description for easy-auto-destroy options */
        if (page == OPT_PAGE_AUTODESTROY) c_prt(TERM_YELLOW, "Following options will protect items from easy auto-destroyer.", 8, 3);

        /* Display the options */
        for (i = 0; i < n; i++)
        {
            byte a = TERM_WHITE;

            /* Color current option */
            if (i == k) a = TERM_L_BLUE;

            /* Display the option text */
            if (option_info[opt[i]].o_var == &random_artifacts)
            {
                sprintf(buf, "%-48s: ", option_info[opt[i]].o_desc);
                if (random_artifacts)
                    sprintf(buf + strlen(buf), "%d%% ", random_artifact_pct);
                else
                    strcat(buf, "no  ");
                sprintf(buf + strlen(buf), "(%.19s)", option_info[opt[i]].o_text);
            }
            else
            {
                sprintf(buf, "%-48s: %s (%.19s)",
                    option_info[opt[i]].o_desc,
                    (*option_info[opt[i]].o_var ? "yes" : "no "),
                    option_info[opt[i]].o_text);
            }
            if ((page == OPT_PAGE_AUTODESTROY) && i > 4) c_prt(a, buf, i + 6, 0);
            else c_prt(a, buf, i + 2, 0);
        }

        if ((page == OPT_PAGE_AUTODESTROY) && (k > 4)) l = 4;
        else l = 0;

        /* Hilite current option */
        move_cursor(k + 2 + l, 50);

        /* Get a key */
        ch = inkey();

        /*
         * HACK - Try to translate the key into a direction
         * to allow using the roguelike keys for navigation.
         */
        dir = get_keymap_dir(ch);
        if ((dir == 2) || (dir == 4) || (dir == 6) || (dir == 8))
            ch = I2D(dir);

        /* Analyze */
        switch (ch)
        {
            case ESCAPE:
            {
                return;
            }

            case '-':
            case '8':
            {
                k = (n + k - 1) % n;
                break;
            }

            case ' ':
            case '\n':
            case '\r':
            case '2':
            {
                k = (k + 1) % n;
                break;
            }

            case 'y':
            case 'Y':
            case '6':
            {
                if (browse_only) break;
                if (option_info[opt[k]].o_var == &random_artifacts)
                {
                    if (!random_artifacts)
                    {
                        random_artifacts = TRUE;
                        random_artifact_pct = 10;
                    }
                    else
                    {
                        random_artifact_pct += 10;
                        if (random_artifact_pct > 100) random_artifacts = FALSE;
                    }
                }
                else
                {
                    (*option_info[opt[k]].o_var) = TRUE;
                    k = (k + 1) % n;
                }
                break;
            }

            case 'n':
            case 'N':
            case '4':
            {
                if (browse_only) break;
                if (option_info[opt[k]].o_var == &random_artifacts)
                {
                    if (!random_artifacts)
                    {
                        random_artifacts = TRUE;
                        random_artifact_pct = 100;
                    }
                    else
                    {
                        random_artifact_pct -= 10;
                        if (random_artifact_pct <= 0) random_artifacts = FALSE;
                    }
                }
                else
                {
                    (*option_info[opt[k]].o_var) = FALSE;
                    k = (k + 1) % n;
                }
                break;
            }

            case 't':
            case 'T':
            {
                if (!browse_only) (*option_info[opt[k]].o_var) = !(*option_info[opt[k]].o_var);
                break;
            }

            case '?':
            {
                doc_display_help("option.txt", option_info[opt[k]].o_text);
                Term_clear();
                break;
            }

            default:
            {
                bell();
                break;
            }
        }
    }
}


/*
 * Modify the "window" options
 */
static void do_cmd_options_win(void)
{
    int i, j, d;

    int y = 0;
    int x = 0;

    char ch;

    bool go = TRUE;

    u32b old_flag[8];


    /* Memorize old flags */
    for (j = 0; j < 8; j++)
    {
        /* Acquire current flags */
        old_flag[j] = window_flag[j];
    }


    /* Clear screen */
    Term_clear();

    /* Interact */
    while (go)
    {
        /* Prompt XXX XXX XXX */
        prt("Window Flags (<dir>, t, y, n, ESC) ", 0, 0);


        /* Display the windows */
        for (j = 0; j < 8; j++)
        {
            byte a = TERM_WHITE;

            cptr s = angband_term_name[j];

            /* Use color */
            if (j == x) a = TERM_L_BLUE;

            /* Window name, staggered, centered */
            Term_putstr(35 + j * 5 - strlen(s) / 2, 2 + j % 2, -1, a, s);
        }

        /* Display the options */
        for (i = 0; i < 16; i++)
        {
            byte a = TERM_WHITE;

            cptr str = window_flag_desc[i];

            /* Use color */
            if (i == y) a = TERM_L_BLUE;

            /* Unused option */
            if (!str) str = "(Unused option)";


            /* Flag name */
            Term_putstr(0, i + 5, -1, a, str);

            /* Display the windows */
            for (j = 0; j < 8; j++)
            {
                byte a = TERM_WHITE;

                char c = '.';

                /* Use color */
                if ((i == y) && (j == x)) a = TERM_L_BLUE;

                /* Active flag */
                if (window_flag[j] & (1L << i)) c = 'X';

                /* Flag value */
                Term_putch(35 + j * 5, i + 5, a, c);
            }
        }

        /* Place Cursor */
        Term_gotoxy(35 + x * 5, y + 5);

        /* Get key */
        ch = inkey();

        /* Analyze */
        switch (ch)
        {
            case ESCAPE:
            {
                go = FALSE;
                break;
            }

            case 'T':
            case 't':
            {
                /* Clear windows */
                for (j = 0; j < 8; j++)
                {
                    window_flag[j] &= ~(1L << y);
                }

                /* Clear flags */
                for (i = 0; i < 16; i++)
                {
                    window_flag[x] &= ~(1L << i);
                }
            }
            /* FALL THROUGH */

            case 'y':
            case 'Y':
            {
                /* Ignore screen */
                if (x == 0) break;

                /* Set flag */
                window_flag[x] |= (1L << y);
                break;
            }

            case 'n':
            case 'N':
            {
                /* Clear flag */
                window_flag[x] &= ~(1L << y);
                break;
            }

            case '?':
            {
                doc_display_help("option.txt", "Window");
                Term_clear();
                break;
            }

            default:
            {
                d = get_keymap_dir(ch);

                x = (x + ddx[d] + 8) % 8;
                y = (y + ddy[d] + 16) % 16;

                if (!d) bell();
            }
        }
    }

    /* Notice changes */
    for (j = 0; j < 8; j++)
    {
        term *old = Term;

        /* Dead window */
        if (!angband_term[j]) continue;

        /* Ignore non-changes */
        if (window_flag[j] == old_flag[j]) continue;

        /* Activate */
        Term_activate(angband_term[j]);

        /* Erase */
        Term_clear();

        /* Refresh */
        Term_fresh();

        /* Restore */
        Term_activate(old);
    }
}



#define OPT_NUM 13

static struct opts
{
    char key;
    cptr name;
    int row;
}
option_fields[OPT_NUM] =
{
    { '1', "Input Options", 3 },
    { '2', "Map Screen Options", 4 },
    { '3', "Text Display Options", 5 },
    { '4', "Game-Play Options", 6 },
    { '5', "Disturbance Options", 7 },
    { '6', "Auto-Destroyer Options", 8 },

    { 'p', "Auto-picker/destroyer editor", 11 },
    { 'd', "Base Delay Factor", 12 },
    { 'h', "Hitpoint Warning", 13 },
    { 'm', "Mana Color Threshold", 14 },
    { 'a', "Autosave Options", 15 },
    { 'w', "Window Flags", 16 },

    { 'b', "Birth Options (Browse Only)", 18 },
};


/*
 * Set or unset various options.
 *
 * The user must use the "Ctrl-R" command to "adapt" to changes
 * in any options which control "visual" aspects of the game.
 */
void do_cmd_options(void)
{
    char k;
    int i, d, skey;
    int y = 0;

    /* Save the screen */
    screen_save();

    /* Interact */
    while (1)
    {
        int n = OPT_NUM;

        /* Does not list cheat option when cheat option is off */
        if (!plr->noscore && !allow_debug_opts) n--;

        /* Clear screen */
        Term_clear();

        /* Why are we here */
        prt("Game Options", 1, 0);

        while(1)
        {
            /* Give some choices */
            for (i = 0; i < n; i++)
            {
                byte a = TERM_WHITE;
                if (i == y) a = TERM_L_BLUE;
#ifndef ALLOW_WIZARD
                if (option_fields[i].key == 'c') continue;
#endif
                Term_putstr(5, option_fields[i].row, -1, a,
                    format("(%c) %s", toupper(option_fields[i].key), option_fields[i].name));
            }

            prt("Move to <dir>, Select to Enter, Cancel to ESC, ? to help: ", 21, 0);

            /* Get command */
            skey = inkey_special(TRUE);
            if (!(skey & SKEY_MASK)) k = (char)skey;
            else k = 0;

            /* Exit */
            if (IS_ESCAPE(k)) break;

            if (my_strchr("\n\r ", k))
            {
                k = option_fields[y].key;
                break;
            }

            for (i = 0; i < n; i++)
            {
                if (tolower(k) == option_fields[i].key) break;
            }

            /* Command is found */
            if (i < n) break;

            /* Hack -- browse help */
            if (k == '?') break;

            /* Move cursor */
            d = 0;
            if (skey == SKEY_UP) d = 8;
            if (skey == SKEY_DOWN) d = 2;
            y = (y + ddy[d] + n) % n;
            if (!d) bell();
        }

        /* Exit */
        if (IS_ESCAPE(k)) break;

        /* Analyze */
        switch (k)
        {
            case '1':
            {
                /* Process the general options */
                do_cmd_options_aux(OPT_PAGE_INPUT, "Input Options");
                break;
            }

            case '2':
            {
                /* Process the general options */
                do_cmd_options_aux(OPT_PAGE_MAPSCREEN, "Map Screen Options");
                break;
            }

            case '3':
            {
                /* Spawn */
                do_cmd_options_aux(OPT_PAGE_TEXT, "Text Display Options");
                break;
            }

            case '4':
            {
                /* Spawn */
                do_cmd_options_aux(OPT_PAGE_GAMEPLAY, "Game-Play Options");
                break;
            }

            case '5':
            {
                /* Spawn */
                do_cmd_options_aux(OPT_PAGE_DISTURBANCE, "Disturbance Options");
                break;
            }

            case '6':
            {
                /* Spawn */
                do_cmd_options_aux(OPT_PAGE_AUTODESTROY, "Auto-Destroyer Options");
                break;
            }

            /* Birth Options */
            case 'B':
            case 'b':
            {
                do_cmd_options_aux(OPT_PAGE_BIRTH, (!plr->wizard || !allow_debug_opts) ? "Birth Options(browse only)" : "Birth Options((*)s effect score)");
                break;
            }

            case 'a':
            case 'A':
            {
                do_cmd_options_autosave("Autosave");
                break;
            }

            /* Window flags */
            case 'W':
            case 'w':
            {
                /* Spawn */
                do_cmd_options_win();
                plr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL |
                          PW_MONSTER_LIST | PW_OBJECT_LIST | PW_MESSAGE | PW_OVERHEAD |
                          PW_MONSTER | PW_OBJECT | PW_SNAPSHOT |
                          PW_BORG_1 | PW_BORG_2 | PW_DUNGEON);
                break;
            }

            /* Auto-picker/destroyer editor */
            case 'P':
            case 'p':
            {
                do_cmd_edit_autopick();
                break;
            }

            /* Hack -- Delay Speed */
            case 'D':
            case 'd':
            {
                msg_input_num("Animation Delay (ms)", &delay_animation, 0, 1000);
                msg_input_num("Running Delay (ms)", &delay_run, 0, 1000);
                msg_input_num("Resting Delay (ms)", &delay_rest, 0, 1000);
                break;
            }

            /* Hack -- hitpoint warning factor */
            case 'H':
            case 'h':
            {
                /* Prompt */
                clear_from(18);
                prt("Command: Hitpoint Warning", 19, 0);

                /* Get a new value */
                while (1)
                {
                    prt(format("Current hitpoint warning: %d0%%",
                           hitpoint_warn), 22, 0);

                    prt("Hitpoint Warning (0-9 or ESC to accept): ", 20, 0);

                    k = inkey();
                    if (k == ESCAPE) break;
                    else if (k == '?')
                    {
                        doc_display_help("option.txt", "HitPoint");
                        Term_clear();
                    }
                    else if (isdigit(k)) hitpoint_warn = D2I(k);
                    else bell();
                }

                break;
            }

            /* Hack -- mana color factor */
            case 'M':
            case 'm':
            {
                /* Prompt */
                clear_from(18);
                prt("Command: Mana Color Threshold", 19, 0);

                /* Get a new value */
                while (1)
                {
                    prt(format("Current mana color threshold: %d0%%",
                           mana_warn), 22, 0);

                    prt("Mana color Threshold (0-9 or ESC to accept): ", 20, 0);

                    k = inkey();
                    if (k == ESCAPE) break;
                    else if (k == '?')
                    {
                        doc_display_help("option.txt", "Manapoint");
                        Term_clear();
                    }
                    else if (isdigit(k)) mana_warn = D2I(k);
                    else bell();
                }

                break;
            }

            case '?':
                doc_display_help("option.txt", NULL);
                Term_clear();
                break;

            /* Unknown option */
            default:
            {
                /* Oops */
                bell();
                break;
            }
        }

        /* Flush messages */
        msg_print(NULL);
    }


    /* Restore the screen */
    screen_load();

    /* Hack - Redraw equippy chars */
    plr->redraw |= (PR_EQUIPPY);
}



/*
 * Ask for a "user pref line" and process it
 *
 * XXX XXX XXX Allow absolute file names?
 */
void do_cmd_pref(void)
{
    char buf[80];

    /* Default */
    strcpy(buf, "");

    /* Ask for a "user pref command" */
    if (!get_string("Pref: ", buf, 80)) return;


    /* Process that pref command */
    (void)process_pref_file_command(buf);
}

void do_cmd_reload_autopick(void)
{
    if (!get_check("Reload auto-pick preference file? ")) return;

    /* Load the file with messages */
    autopick_load_pref(TRUE);
}

#ifdef ALLOW_MACROS

/*
 * Hack -- append all current macros to the given file
 */
static errr macro_dump(cptr fname)
{
    static cptr mark = "Macro Dump";

    int i;

    char buf[1024];

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);

    /* Append to the file */
    if (!open_auto_dump(buf, mark)) return (-1);

    /* Start dumping */
    auto_dump_printf("\n# Automatic macro dump\n\n");

    /* Dump them */
    for (i = 0; i < macro__num; i++)
    {
        /* Extract the action */
        ascii_to_text(buf, macro__act[i]);

        /* Dump the macro */
        auto_dump_printf("A:%s\n", buf);

        /* Extract the action */
        ascii_to_text(buf, macro__pat[i]);

        /* Dump normal macros */
        auto_dump_printf("P:%s\n", buf);

        /* End the macro */
        auto_dump_printf("\n");
    }

    /* Close */
    close_auto_dump();

    /* Success */
    return (0);
}


/*
 * Hack -- ask for a "trigger" (see below)
 *
 * Note the complex use of the "inkey()" function from "util.c".
 *
 * Note that both "flush()" calls are extremely important.
 */
static void do_cmd_macro_aux(char *buf)
{
    int i, n = 0;

    char tmp[1024];


    /* Flush */
    flush();

    /* Do not process macros */
    inkey_base = TRUE;

    /* First key */
    i = inkey();

    /* Read the pattern */
    while (i)
    {
        /* Save the key */
        buf[n++] = i;

        /* Do not process macros */
        inkey_base = TRUE;

        /* Do not wait for keys */
        inkey_scan = TRUE;

        /* Attempt to read a key */
        i = inkey();
    }

    /* Terminate */
    buf[n] = '\0';

    /* Flush */
    flush();


    /* Convert the trigger */
    ascii_to_text(tmp, buf);

    /* Hack -- display the trigger */
    Term_addstr(-1, TERM_WHITE, tmp);
}

#endif


/*
 * Hack -- ask for a keymap "trigger" (see below)
 *
 * Note that both "flush()" calls are extremely important. This may
 * no longer be true, since "util.c" is much simpler now. XXX XXX XXX
 */
static void do_cmd_macro_aux_keymap(char *buf)
{
    char tmp[1024];


    /* Flush */
    flush();


    /* Get a key */
    buf[0] = inkey();
    buf[1] = '\0';


    /* Convert to ascii */
    ascii_to_text(tmp, buf);

    /* Hack -- display the trigger */
    Term_addstr(-1, TERM_WHITE, tmp);


    /* Flush */
    flush();
}


/*
 * Hack -- append all keymaps to the given file
 */
static errr keymap_dump(cptr fname)
{
    static cptr mark = "Keymap Dump";
    int i;

    char key[1024];
    char buf[1024];

    int mode;

    /* Roguelike */
    if (rogue_like_commands)
    {
        mode = KEYMAP_MODE_ROGUE;
    }

    /* Original */
    else
    {
        mode = KEYMAP_MODE_ORIG;
    }


    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);

    /* Append to the file */
    if (!open_auto_dump(buf, mark)) return -1;

    /* Start dumping */
    auto_dump_printf("\n# Automatic keymap dump\n\n");

    /* Dump them */
    for (i = 0; i < 256; i++)
    {
        cptr act;

        /* Loop up the keymap */
        act = keymap_act[mode][i];

        /* Skip empty keymaps */
        if (!act) continue;

        /* Encode the key */
        buf[0] = i;
        buf[1] = '\0';
        ascii_to_text(key, buf);

        /* Encode the action */
        ascii_to_text(buf, act);

        /* Dump the macro */
        auto_dump_printf("A:%s\n", buf);
        auto_dump_printf("C:%d:%s\n", mode, key);
    }

    /* Close */
    close_auto_dump();

    /* Success */
    return (0);
}



/*
 * Interact with "macros"
 *
 * Note that the macro "action" must be defined before the trigger.
 *
 * Could use some helpful instructions on this page. XXX XXX XXX
 */
void do_cmd_macros(void)
{
    int i;

    char tmp[1024];

    char buf[1024];

    int mode;


    /* Roguelike */
    if (rogue_like_commands)
    {
        mode = KEYMAP_MODE_ROGUE;
    }

    /* Original */
    else
    {
        mode = KEYMAP_MODE_ORIG;
    }

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);


    /* Save screen */
    screen_save();


    /* Process requests until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Describe */
        prt("Interact with Macros", 2, 0);



        /* Describe that action */
        prt("Current action (if any) shown below:", 20, 0);


        /* Analyze the current action */
        ascii_to_text(buf, macro__buf);

        /* Display the current action */
        prt(buf, 22, 0);


        /* Selections */
        prt("(1) Load a user pref file", 4, 5);

#ifdef ALLOW_MACROS
        prt("(2) Append macros to a file", 5, 5);
        prt("(3) Query a macro", 6, 5);
        prt("(4) Create a macro", 7, 5);
        prt("(5) Remove a macro", 8, 5);
        prt("(6) Append keymaps to a file", 9, 5);
        prt("(7) Query a keymap", 10, 5);
        prt("(8) Create a keymap", 11, 5);
        prt("(9) Remove a keymap", 12, 5);
        prt("(0) Enter a new action", 13, 5);

#endif /* ALLOW_MACROS */

        /* Prompt */
        prt("Command: ", 16, 0);


        /* Get a command */
        i = inkey();

        /* Leave */
        if (i == ESCAPE) break;

        /* Load a 'macro' file */
        else if (i == '1')
        {
            errr err;

            /* Prompt */
            prt("Command: Load a user pref file", 16, 0);


            /* Prompt */
            prt("File: ", 18, 0);


            /* Default filename */
            sprintf(tmp, "%s.prf", player_base);

            /* Ask for a file */
            if (!askfor(tmp, 80)) continue;

            /* Process the given filename */
            err = process_pref_file(tmp);
            if (-2 == err)
            {
                msg_format("Loaded default '%s'.", tmp);
            }
            else if (err)
            {
                /* Prompt */
                msg_format("Failed to load '%s'!", tmp);
            }
            else
            {
                msg_format("Loaded '%s'.", tmp);
            }
        }

#ifdef ALLOW_MACROS

        /* Save macros */
        else if (i == '2')
        {
            /* Prompt */
            prt("Command: Append macros to a file", 16, 0);


            /* Prompt */
            prt("File: ", 18, 0);


            /* Default filename */
            sprintf(tmp, "%s.prf", player_base);

            /* Ask for a file */
            if (!askfor(tmp, 80)) continue;

            /* Dump the macros */
            (void)macro_dump(tmp);

            /* Prompt */
            msg_print("Appended macros.");

        }

        /* Query a macro */
        else if (i == '3')
        {
            int k;

            /* Prompt */
            prt("Command: Query a macro", 16, 0);


            /* Prompt */
            prt("Trigger: ", 18, 0);


            /* Get a macro trigger */
            do_cmd_macro_aux(buf);

            /* Acquire action */
            k = macro_find_exact(buf);

            /* Nothing found */
            if (k < 0)
            {
                /* Prompt */
                msg_print("Found no macro.");

            }

            /* Found one */
            else
            {
                /* Obtain the action */
                strcpy(macro__buf, macro__act[k]);

                /* Analyze the current action */
                ascii_to_text(buf, macro__buf);

                /* Display the current action */
                prt(buf, 22, 0);

                /* Prompt */
                msg_print("Found a macro.");

            }
        }

        /* Create a macro */
        else if (i == '4')
        {
            /* Prompt */
            prt("Command: Create a macro", 16, 0);


            /* Prompt */
            prt("Trigger: ", 18, 0);


            /* Get a macro trigger */
            do_cmd_macro_aux(buf);

            /* Clear */
            clear_from(20);

            /* Help message */
            c_prt(TERM_L_RED, "Press Left/Right arrow keys to move cursor. Backspace/Delete to delete a char.", 22, 0);

            /* Prompt */
            prt("Action: ", 20, 0);


            /* Convert to text */
            ascii_to_text(tmp, macro__buf);

            /* Get an encoded action */
            if (askfor(tmp, 80))
            {
                /* Convert to ascii */
                text_to_ascii(macro__buf, tmp);

                /* Link the macro */
                macro_add(buf, macro__buf);

                /* Prompt */
                msg_print("Added a macro.");

            }
        }

        /* Remove a macro */
        else if (i == '5')
        {
            /* Prompt */
            prt("Command: Remove a macro", 16, 0);


            /* Prompt */
            prt("Trigger: ", 18, 0);


            /* Get a macro trigger */
            do_cmd_macro_aux(buf);

            /* Link the macro */
            macro_add(buf, buf);

            /* Prompt */
            msg_print("Removed a macro.");

        }

        /* Save keymaps */
        else if (i == '6')
        {
            /* Prompt */
            prt("Command: Append keymaps to a file", 16, 0);


            /* Prompt */
            prt("File: ", 18, 0);


            /* Default filename */
            sprintf(tmp, "%s.prf", player_base);

            /* Ask for a file */
            if (!askfor(tmp, 80)) continue;

            /* Dump the macros */
            (void)keymap_dump(tmp);

            /* Prompt */
            msg_print("Appended keymaps.");

        }

        /* Query a keymap */
        else if (i == '7')
        {
            cptr act;

            /* Prompt */
            prt("Command: Query a keymap", 16, 0);


            /* Prompt */
            prt("Keypress: ", 18, 0);


            /* Get a keymap trigger */
            do_cmd_macro_aux_keymap(buf);

            /* Look up the keymap */
            act = keymap_act[mode][(byte)(buf[0])];

            /* Nothing found */
            if (!act)
            {
                /* Prompt */
                msg_print("Found no keymap.");

            }

            /* Found one */
            else
            {
                /* Obtain the action */
                strcpy(macro__buf, act);

                /* Analyze the current action */
                ascii_to_text(buf, macro__buf);

                /* Display the current action */
                prt(buf, 22, 0);

                /* Prompt */
                msg_print("Found a keymap.");

            }
        }

        /* Create a keymap */
        else if (i == '8')
        {
            /* Prompt */
            prt("Command: Create a keymap", 16, 0);


            /* Prompt */
            prt("Keypress: ", 18, 0);


            /* Get a keymap trigger */
            do_cmd_macro_aux_keymap(buf);

            /* Clear */
            clear_from(20);

            /* Help message */
            c_prt(TERM_L_RED, "Press Left/Right arrow keys to move cursor. Backspace/Delete to delete a char.", 22, 0);

            /* Prompt */
            prt("Action: ", 20, 0);


            /* Convert to text */
            ascii_to_text(tmp, macro__buf);

            /* Get an encoded action */
            if (askfor(tmp, 80))
            {
                /* Convert to ascii */
                text_to_ascii(macro__buf, tmp);

                /* Free old keymap */
                z_string_free(keymap_act[mode][(byte)(buf[0])]);

                /* Make new keymap */
                keymap_act[mode][(byte)(buf[0])] = z_string_make(macro__buf);

                /* Prompt */
                msg_print("Added a keymap.");

            }
        }

        /* Remove a keymap */
        else if (i == '9')
        {
            /* Prompt */
            prt("Command: Remove a keymap", 16, 0);


            /* Prompt */
            prt("Keypress: ", 18, 0);


            /* Get a keymap trigger */
            do_cmd_macro_aux_keymap(buf);

            /* Free old keymap */
            z_string_free(keymap_act[mode][(byte)(buf[0])]);

            /* Make new keymap */
            keymap_act[mode][(byte)(buf[0])] = NULL;

            /* Prompt */
            msg_print("Removed a keymap.");

        }

        /* Enter a new action */
        else if (i == '0')
        {
            /* Prompt */
            prt("Command: Enter a new action", 16, 0);

            /* Clear */
            clear_from(20);

            /* Help message */
            c_prt(TERM_L_RED, "Press Left/Right arrow keys to move cursor. Backspace/Delete to delete a char.", 22, 0);

            /* Prompt */
            prt("Action: ", 20, 0);

            /* Hack -- limit the value */
            tmp[80] = '\0';

            /* Get an encoded action */
            if (!askfor(buf, 80)) continue;

            /* Extract an action */
            text_to_ascii(macro__buf, buf);
        }

#endif /* ALLOW_MACROS */

        /* Oops */
        else
        {
            /* Oops */
            bell();
        }

        /* Flush messages */
        msg_print(NULL);
    }

    /* Load screen */
    screen_load();
}


static bool cmd_visuals_aux(int i, int *num, int max)
{
    if (iscntrl(i))
    {
        char str[10] = "";
        int tmp;

        sprintf(str, "%d", *num);

        if (!get_string(format("Input new number(0-%d): ", max-1), str, 4))
            return FALSE;

        tmp = strtol(str, NULL, 0);
        if (tmp >= 0 && tmp < max)
            *num = tmp;
    }
    else if (isupper(i))
        *num = (*num + max - 1) % max;
    else
        *num = (*num + 1) % max;

    return TRUE;
}

static void print_visuals_menu(cptr choice_msg)
{
    prt("Interact with Visuals", 1, 0);

    /* Give some choices */
    prt("(0) Load a user pref file", 3, 5);

#ifdef ALLOW_VISUALS
    prt("(1) Dump monster attr/chars", 4, 5);
    prt("(2) Dump object attr/chars", 5, 5);
    prt("(4) Change monster attr/chars (numeric operation)", 7, 5);
    prt("(5) Change object attr/chars (numeric operation)", 8, 5);
    prt("(7) Change monster attr/chars (visual mode)", 10, 5);
    prt("(8) Change object attr/chars (visual mode)", 11, 5);

#endif /* ALLOW_VISUALS */

    prt("(R) Reset visuals", 13, 5);

    /* Prompt */
    prt(format("Command: %s", choice_msg ? choice_msg : ""), 15, 0);
}

static void do_cmd_knowledge_monsters(bool *need_redraw, bool visual_only);
static void do_cmd_knowledge_objects(bool *need_redraw, bool visual_only, int direct_k_idx);

/*
 * Interact with "visuals"
 */
static void _auto_dump_race(mon_race_ptr r)
{
    term_char_t tc = mon_race_visual(r);
    /*auto_dump_printf("# %s\n", r->name);*/
    auto_dump_printf("R:%s:0x%02X:0x%02X\n", sym_str(r->id), tc.a, tc.c);
}
void do_cmd_visuals(void)
{
    int i;
    char tmp[160];
    char buf[1024];
    bool need_redraw = FALSE;
    const char *empty_symbol = "<< ? >>";

    if (use_bigtile) empty_symbol = "<< ?? >>";

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);

    /* Save the screen */
    screen_save();

    /* Interact until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Ask for a choice */
        print_visuals_menu(NULL);

        /* Prompt */
        i = inkey();

        /* Done */
        if (i == ESCAPE) break;

        switch (i)
        {
        /* Load a 'pref' file */
        case '0':
            /* Prompt */
            prt("Command: Load a user pref file", 15, 0);

            /* Prompt */
            prt("File: ", 17, 0);

            /* Default filename */
            sprintf(tmp, "%s.prf", player_base);

            /* Query */
            if (!askfor(tmp, 70)) continue;

            /* Process the given filename */
            (void)process_pref_file(tmp);

            need_redraw = TRUE;
            break;

#ifdef ALLOW_VISUALS

        /* Dump monster attr/chars */
        case '1':
        {
            static cptr mark = "Monster attr/chars";

            /* Prompt */
            prt("Command: Dump monster attr/chars", 15, 0);

            /* Prompt */
            prt("File: ", 17, 0);

            /* Default filename */
            sprintf(tmp, "%s.prf", player_base);

            /* Get a filename */
            if (!askfor(tmp, 70)) continue;

            /* Build the filename */
            path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

            /* Append to the file */
            if (!open_auto_dump(buf, mark)) continue;

            /* Start dumping */
            auto_dump_printf("\n# Monster attr/char definitions\n\n");

            /* Dump monsters */
            mon_race_iter(_auto_dump_race);

            /* Close */
            close_auto_dump();

            /* Message */
            msg_print("Dumped monster attr/chars.");

            break;
        }

        /* Dump object attr/chars */
        case '2':
        {
            static cptr mark = "Object attr/chars";

            /* Prompt */
            prt("Command: Dump object attr/chars", 15, 0);

            /* Prompt */
            prt("File: ", 17, 0);

            /* Default filename */
            sprintf(tmp, "%s.prf", player_base);

            /* Get a filename */
            if (!askfor(tmp, 70)) continue;

            /* Build the filename */
            path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

            /* Append to the file */
            if (!open_auto_dump(buf, mark)) continue;

            /* Start dumping */
            auto_dump_printf("\n# Object attr/char definitions\n\n");

            /* Dump objects */
            for (i = 0; i < max_k_idx; i++)
            {
                char o_name[80];
                object_kind *k_ptr = &k_info[i];

                /* Skip non-entries */
                if (!k_ptr->name) continue;

                if (!k_ptr->flavor)
                {
                    /* Tidy name */
                    strip_name(o_name, i);
                }
                else
                {
                    object_type forge;

                    /* Prepare dummy object */
                    object_prep(&forge, i);

                    /* Get un-shuffled flavor name */
                    object_desc(o_name, &forge, OD_FORCE_FLAVOR);
                }

                /* Dump a comment */
                auto_dump_printf("# %s\n", o_name);

                /* Dump the object attr/char info */
                auto_dump_printf("K:%d:%d:0x%02X/0x%02X\n\n",
                    k_ptr->tval, k_ptr->sval,
                    (byte)(k_ptr->x_attr), (byte)(k_ptr->x_char));
            }

            /* Close */
            close_auto_dump();

            /* Message */
            msg_print("Dumped object attr/chars.");

            break;
        }

        /* Modify monster attr/chars (numeric operation) */
        case '4':
        {
        #ifdef FIX_R_INFO
            static cptr choice_msg = "Change monster attr/chars";
            static int r = 0;

            prt(format("Command: %s", choice_msg), 15, 0);

            /* Hack -- query until done */
            while (1)
            {
                monster_race *r_ptr = &r_info[r];
                char c;
                int t;

                byte da = r_ptr->d_attr;
                byte dc = r_ptr->d_char;
                byte ca = r_ptr->x_attr;
                byte cc = r_ptr->x_char;

                /* Label the object */
                Term_putstr(5, 17, -1, TERM_WHITE,
                        format("Monster = %d, Name = %-40.40s",
                           r, r_ptr->name));

                /* Label the Default values */
                Term_putstr(10, 19, -1, TERM_WHITE,
                        format("Default attr/char = %3u / %3u", da, dc));

                Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
                Term_queue_bigchar(43, 19, da, dc, 0, 0);

                /* Label the Current values */
                Term_putstr(10, 20, -1, TERM_WHITE,
                        format("Current attr/char = %3u / %3u", ca, cc));

                Term_putstr(40, 20, -1, TERM_WHITE, empty_symbol);
                Term_queue_bigchar(43, 20, ca, cc, 0, 0);

                /* Prompt */
                Term_putstr(0, 22, -1, TERM_WHITE,
                        "Command (n/N/^N/a/A/^A/c/C/^C/v/V/^V): ");

                /* Get a command */
                i = inkey();

                /* All done */
                if (i == ESCAPE) break;

                if (iscntrl(i)) c = 'a' + i - KTRL('A');
                else if (isupper(i)) c = 'a' + i - 'A';
                else c = i;

                switch (c)
                {
                case 'n':
                    {
                        int prev_r = r;
                        do
                        {
                            if (!cmd_visuals_aux(i, &r, max_r_idx))
                            {
                                r = prev_r;
                                break;
                            }
                        }
                        while (!r_info[r].name);
                    }
                    break;
                case 'a':
                    t = (int)r_ptr->x_attr;
                    (void)cmd_visuals_aux(i, &t, 256);
                    r_ptr->x_attr = (byte)t;
                    need_redraw = TRUE;
                    break;
                case 'c':
                    t = (int)r_ptr->x_char;
                    (void)cmd_visuals_aux(i, &t, 256);
                    r_ptr->x_char = (byte)t;
                    need_redraw = TRUE;
                    break;
                case 'v':
                    do_cmd_knowledge_monsters(&need_redraw, TRUE, r);

                    /* Clear screen */
                    Term_clear();
                    print_visuals_menu(choice_msg);
                    break;
                }
            }
        #endif
            break;
        }

        /* Modify object attr/chars (numeric operation) */
        case '5':
        {
            static cptr choice_msg = "Change object attr/chars";
            static int k = 0;

            prt(format("Command: %s", choice_msg), 15, 0);

            /* Hack -- query until done */
            while (1)
            {
                object_kind *k_ptr = &k_info[k];
                char c;
                int t;

                byte da = k_ptr->d_attr;
                byte dc = k_ptr->d_char;
                byte ca = k_ptr->x_attr;
                byte cc = k_ptr->x_char;

                /* Label the object */
                Term_putstr(5, 17, -1, TERM_WHITE,
                        format("Object = %d, Name = %-40.40s",
                           k, !k_ptr->flavor ? k_ptr->name : k_ptr->flavor_name));

                /* Label the Default values */
                Term_putstr(10, 19, -1, TERM_WHITE,
                        format("Default attr/char = %3d / %3d", da, dc));

                Term_putstr(40, 19, -1, TERM_WHITE, empty_symbol);
                Term_queue_bigchar(43, 19, da, dc, 0, 0);

                /* Label the Current values */
                Term_putstr(10, 20, -1, TERM_WHITE,
                        format("Current attr/char = %3d / %3d", ca, cc));

                Term_putstr(40, 20, -1, TERM_WHITE, empty_symbol);
                Term_queue_bigchar(43, 20, ca, cc, 0, 0);

                /* Prompt */
                Term_putstr(0, 22, -1, TERM_WHITE,
                        "Command (n/N/^N/a/A/^A/c/C/^C/v/V/^V): ");

                /* Get a command */
                i = inkey();

                /* All done */
                if (i == ESCAPE) break;

                if (iscntrl(i)) c = 'a' + i - KTRL('A');
                else if (isupper(i)) c = 'a' + i - 'A';
                else c = i;

                switch (c)
                {
                case 'n':
                    {
                        int prev_k = k;
                        do
                        {
                            if (!cmd_visuals_aux(i, &k, max_k_idx))
                            {
                                k = prev_k;
                                break;
                            }
                        }
                        while (!k_info[k].name);
                    }
                    break;
                case 'a':
                    t = (int)k_ptr->x_attr;
                    (void)cmd_visuals_aux(i, &t, 256);
                    k_ptr->x_attr = (byte)t;
                    need_redraw = TRUE;
                    break;
                case 'c':
                    t = (int)k_ptr->x_char;
                    (void)cmd_visuals_aux(i, &t, 256);
                    k_ptr->x_char = (byte)t;
                    need_redraw = TRUE;
                    break;
                case 'v':
                    do_cmd_knowledge_objects(&need_redraw, TRUE, k);

                    /* Clear screen */
                    Term_clear();
                    print_visuals_menu(choice_msg);
                    break;
                }
            }

            break;
        }
        /* Modify monster attr/chars (visual mode) */
        case '7':
            do_cmd_knowledge_monsters(&need_redraw, TRUE);
            break;

        /* Modify object attr/chars (visual mode) */
        case '8':
            do_cmd_knowledge_objects(&need_redraw, TRUE, -1);
            break;

#endif /* ALLOW_VISUALS */

        /* Reset visuals */
        case 'R':
        case 'r':
            /* Reset */
            reset_visuals();

            /* Message */
            msg_print("Visual attr/char tables reset.");

            need_redraw = TRUE;
            break;

        /* Unknown option */
        default:
            bell();
            break;
        }

        /* Flush messages */
        msg_print(NULL);
    }

    /* Restore the screen */
    screen_load();

    if (need_redraw) do_cmd_redraw();
}


/*
 * Interact with "colors"
 */
void do_cmd_colors(void)
{
    int i;

    char tmp[160];

    char buf[1024];


    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);


    /* Save the screen */
    screen_save();


    /* Interact until done */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Ask for a choice */
        prt("Interact with Colors", 2, 0);


        /* Give some choices */
        prt("(1) Load a user pref file", 4, 5);

#ifdef ALLOW_COLORS
        prt("(2) Dump colors", 5, 5);
        prt("(3) Modify colors", 6, 5);

#endif

        /* Prompt */
        prt("Command: ", 8, 0);


        /* Prompt */
        i = inkey();

        /* Done */
        if (i == ESCAPE) break;

        /* Load a 'pref' file */
        if (i == '1')
        {
            /* Prompt */
            prt("Command: Load a user pref file", 8, 0);


            /* Prompt */
            prt("File: ", 10, 0);


            /* Default file */
            sprintf(tmp, "%s.prf", player_base);

            /* Query */
            if (!askfor(tmp, 70)) continue;

            /* Process the given filename */
            (void)process_pref_file(tmp);

            /* Mega-Hack -- react to changes */
            Term_xtra(TERM_XTRA_REACT, 0);

            /* Mega-Hack -- redraw */
            Term_redraw();
        }

#ifdef ALLOW_COLORS

        /* Dump colors */
        else if (i == '2')
        {
            static cptr mark = "Colors";

            /* Prompt */
            prt("Command: Dump colors", 8, 0);


            /* Prompt */
            prt("File: ", 10, 0);


            /* Default filename */
            sprintf(tmp, "%s.prf", player_base);

            /* Get a filename */
            if (!askfor(tmp, 70)) continue;

            /* Build the filename */
            path_build(buf, sizeof(buf), ANGBAND_DIR_USER, tmp);

            /* Append to the file */
            if (!open_auto_dump(buf, mark)) continue;

            /* Start dumping */
            auto_dump_printf("\n# Color redefinitions\n\n");

            /* Dump colors */
            for (i = 0; i < 256; i++)
            {
                int kv = angband_color_table[i][0];
                int rv = angband_color_table[i][1];
                int gv = angband_color_table[i][2];
                int bv = angband_color_table[i][3];

                cptr name = "unknown";


                /* Skip non-entries */
                if (!kv && !rv && !gv && !bv) continue;

                /* Extract the color name */
                if (i < 16) name = color_names[i];

                /* Dump a comment */
                auto_dump_printf("# Color '%s'\n", name);

                /* Dump the monster attr/char info */
                auto_dump_printf("V:%d:0x%02X:0x%02X:0x%02X:0x%02X\n\n",
                    i, kv, rv, gv, bv);
            }

            /* Close */
            close_auto_dump();

            /* Message */
            msg_print("Dumped color redefinitions.");

        }

        /* Edit colors */
        else if (i == '3')
        {
            static byte a = 0;

            /* Prompt */
            prt("Command: Modify colors", 8, 0);


            /* Hack -- query until done */
            while (1)
            {
                cptr name;
                byte j;

                /* Clear */
                clear_from(10);

                /* Exhibit the normal colors */
                for (j = 0; j < 16; j++)
                {
                    /* Exhibit this color */
                    Term_putstr(j*4, 20, -1, a, "###");

                    /* Exhibit all colors */
                    Term_putstr(j*4, 22, -1, j, format("%3d", j));
                }

                /* Describe the color */
                name = ((a < 16) ? color_names[a] : "undefined");


                /* Describe the color */
                Term_putstr(5, 10, -1, TERM_WHITE,
                        format("Color = %d, Name = %s", a, name));


                /* Label the Current values */
                Term_putstr(5, 12, -1, TERM_WHITE,
                        format("K = 0x%02x / R,G,B = 0x%02x,0x%02x,0x%02x",
                           angband_color_table[a][0],
                           angband_color_table[a][1],
                           angband_color_table[a][2],
                           angband_color_table[a][3]));

                /* Prompt */
                Term_putstr(0, 14, -1, TERM_WHITE,
                        "Command (n/N/k/K/r/R/g/G/b/B): ");


                /* Get a command */
                i = inkey();

                /* All done */
                if (i == ESCAPE) break;

                /* Analyze */
                if (i == 'n') a = (byte)(a + 1);
                if (i == 'N') a = (byte)(a - 1);
                if (i == 'k') angband_color_table[a][0] = (byte)(angband_color_table[a][0] + 1);
                if (i == 'K') angband_color_table[a][0] = (byte)(angband_color_table[a][0] - 1);
                if (i == 'r') angband_color_table[a][1] = (byte)(angband_color_table[a][1] + 1);
                if (i == 'R') angband_color_table[a][1] = (byte)(angband_color_table[a][1] - 1);
                if (i == 'g') angband_color_table[a][2] = (byte)(angband_color_table[a][2] + 1);
                if (i == 'G') angband_color_table[a][2] = (byte)(angband_color_table[a][2] - 1);
                if (i == 'b') angband_color_table[a][3] = (byte)(angband_color_table[a][3] + 1);
                if (i == 'B') angband_color_table[a][3] = (byte)(angband_color_table[a][3] - 1);

                /* Hack -- react to changes */
                Term_xtra(TERM_XTRA_REACT, 0);

                /* Hack -- redraw */
                Term_redraw();
            }
        }

#endif

        /* Unknown option */
        else
        {
            bell();
        }

        /* Flush messages */
        msg_print(NULL);
    }


    /* Restore the screen */
    screen_load();
}

void msg_add_tiny_screenshot(int cx, int cy)
{
    if (!statistics_hack)
    {
        str_ptr s = get_tiny_screenshot(cx, cy);
        msg_add(str_buffer(s));
        str_free(s);
    }
}

str_ptr get_tiny_screenshot(int cx, int cy)
{
    str_ptr s = str_alloc_size(cx * cy);
    bool    old_use_graphics = use_graphics;
    rect_t  r = rect_create(plr->pos.x - cx/2, plr->pos.y - cy/2, cx, cy);
    point_t p;

    r = rect_intersect(cave->rect, r);

    if (old_use_graphics)
    {
        use_graphics = FALSE;
        reset_visuals();
    }

    for (p.y = r.y; p.y < r.y + r.cy; p.y++)
    {
        int  current_a = -1;
        for (p.x = r.x; p.x < r.x + r.cx; p.x++)
        {
            map_char_t mc = {0};
            term_char_t tc;

            assert(dun_pos_valid(cave, p));
            map_info(p, &mc);
            tc = map_char_top(&mc);

            if (tc.c == 127) /* Hack for special wall characters on Windows. See font-win.prf and main-win.c */
                tc.c = '#';

            if (tc.a != current_a)
            {
                if (current_a >= 0 && current_a != TERM_WHITE)
                {
                    str_append_s(s, "</color>");
                }
                if (tc.a != TERM_WHITE)
                {
                    str_printf(s, "<color:%c>", attr_to_attr_char(tc.a));
                }
                current_a = tc.a;
            }
            str_append_c(s, tc.c);
        }
        if (current_a >= 0 && current_a != TERM_WHITE)
            str_append_s(s, "</color>");
        str_append_c(s, '\n');
    }
    if (old_use_graphics)
    {
        use_graphics = TRUE;
        reset_visuals();
    }
    return s;
}

/* Note: This will not work if the screen is "icky" */
str_ptr get_screenshot(void)
{
    str_ptr s = str_alloc_size(80 * 27);
    bool       old_use_graphics = use_graphics;
    int        wid, hgt, x, y;

    Term_get_size(&wid, &hgt);

    if (old_use_graphics)
    {
        use_graphics = FALSE;
        reset_visuals();

        plr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY | PR_MSG_LINE);
        redraw_stuff();
    }

    for (y = 0; y < hgt; y++)
    {
        int  current_a = -1;
        for (x = 0; x < wid; x++)
        {
            byte a;
            char c;

            Term_what(x, y, &a, &c);

            if (c == 127) /* Hack for special wall characters on Windows. See font-win.prf and main-win.c */
                c = '#';

            if (a != current_a)
            {
                if (current_a >= 0 && current_a != TERM_WHITE)
                {
                    str_append_s(s, "</color>");
                }
                if (a != TERM_WHITE)
                {
                    str_printf(s, "<color:%c>", attr_to_attr_char(a));
                }
                current_a = a;
            }
            str_append_c(s, c);
        }
        if (current_a >= 0 && current_a != TERM_WHITE)
            str_append_s(s, "</color>");
        str_append_c(s, '\n');
    }
    if (old_use_graphics)
    {
        use_graphics = TRUE;
        reset_visuals();

        plr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY | PR_MSG_LINE);
        redraw_stuff();
    }
    return s;
}

/*
 * Note something in the message recall
 */
void do_cmd_note(void)
{
    char buf[80];
    str_ptr s = 0;

    /* Default */
    strcpy(buf, "");

    /* Input */
    if (!get_string("Note: ", buf, 60)) return;

    /* Ignore empty notes */
    if (!buf[0] || (buf[0] == ' ')) return;

    /* Add the note to the message recall */
    msg_format("<color:R>Note:</color> %s\n", buf);

    s = get_tiny_screenshot(50, 24);
    msg_add(str_buffer(s));
    str_free(s);
}


/*
 * Mention the current version
 */
void do_cmd_version(void)
{
    cptr xtra = "";
    if (VER_MINOR == 0)
    {
        if (VER_PATCH == 0) xtra = " (Alpha)";
        else xtra = " (Beta)";
    }
    msg_format("You are playing <color:B>%s</color> <color:r>%d.%d.%d%s</color>.",
        VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH, xtra);
    if (1)
    {
        rect_t r = ui_map_rect();
        msg_format("Map display is %dx%d.", r.cx, r.cy);
    }
    if (plr->wizard)
        msg_format("OF_COUNT=%d", OF_COUNT);
}



/*
 * Array of feeling strings
 */
struct _feeling_info_s
{
    byte color;
    cptr msg;
};
typedef struct _feeling_info_s _feeling_info_t;
static _feeling_info_t _level_feelings[11] =
{
    {TERM_SLATE, "Looks like any other level."},
    {TERM_L_BLUE, "You feel there is something special about this level."},
    {TERM_VIOLET, "You nearly faint as horrible visions of death fill your mind!"},
    {TERM_RED, "This level looks very dangerous."},
    {TERM_L_RED, "You have a very bad feeling..."},
    {TERM_ORANGE, "You have a bad feeling..."},
    {TERM_YELLOW, "You feel nervous."},
    {TERM_L_UMBER, "You feel your luck is turning..."},
    {TERM_L_WHITE, "You don't like the look of this place."},
    {TERM_WHITE, "This level looks reasonably safe."},
    {TERM_WHITE, "What a boring place..."},
};

static _feeling_info_t _level_feelings_lucky[11] =
{
    {TERM_SLATE, "Looks like any other level."},
    {TERM_L_BLUE, "You feel there is something special about this level."},
    {TERM_VIOLET, "You have a superb feeling about this level."},
    {TERM_RED, "You have an excellent feeling..."},
    {TERM_L_RED, "You have a very good feeling..."},
    {TERM_ORANGE, "You have a good feeling..."},
    {TERM_YELLOW, "You feel strangely lucky..."},
    {TERM_L_UMBER, "You feel your luck is turning..."},
    {TERM_L_WHITE, "You like the look of this place..."},
    {TERM_WHITE, "This level can't be all bad..."},
    {TERM_WHITE, "What a boring place..."},
};


/*
 * Note that "feeling" is set to zero unless some time has passed.
 * Note that this is done when the level is GENERATED, not entered.
 */
void do_cmd_feeling(void)
{
    /* No useful feeling in quests */
    if (!quests_allow_feeling())
        msg_print("Looks like a typical quest level.");

    /* No useful feeling in town */
    else if (dun_world_town_id())
        msg_print("Looks like a typical town.");

    /* No useful feeling in the wilderness */
    else if (cave->type->id == D_SURFACE)
        msg_print("Looks like a typical wilderness.");

    /* Display the feeling */
    else
    {
        _feeling_info_t feeling;
        assert(/*0 <= cave->feeling &&*/ cave->feeling < 11);
        if (plr->good_luck || plr->pclass == CLASS_ARCHAEOLOGIST)
            feeling = _level_feelings_lucky[cave->feeling];
        else
            feeling = _level_feelings[cave->feeling];
        cmsg_print(feeling.color, feeling.msg);
    }
}



/*
 * Description of each monster group.
 */
static cptr monster_group_text[] =
{
    "Corpses",
    "Uniques",
    "Ridable monsters",
    "Wanted monsters",
    "Amberite",
    "Olympian",
    "Guardian",
    "Ant",
    "Bat",
    "Centipede",
    "Dragon",
    "Floating Eye",
    "Feline",
    "Golem",
    "Hobbit/Elf/Dwarf",
    "Icky Thing",
    "Jelly",
    "Kobold",
    "Aquatic monster",
    "Mold",
    "Naga",
    "Orc",
    "Person/Human",
    "Quadruped",
    "Rodent",
    "Skeleton",
    "Demon",
    "Vortex",
    "Worm/Worm-Mass",
    /* "unused", */
    "Yeek",
    "Zombie/Mummy",
    "Angel",
    "Bird",
    "Canine",
    /* "Ancient Dragon/Wyrm", */
    "Elemental",
    "Dragon Fly",
    "Ghost",
    "Hybrid",
    "Insect",
    "Snake",
    "Killer Beetle",
    "Lich",
    "Multi-Headed Reptile",
    "Mystery Living",
    "Ogre",
    "Giant Humanoid",
    "Quylthulg",
    "Reptile/Amphibian",
    "Spider/Scorpion/Tick",
    "Troll",
    /* "Major Demon", */
    "Vampire",
    "Wight/Wraith/etc",
    "Xorn/Xaren/etc",
    "Yeti",
    "Zephyr Hound",
    "Mimic",
    "Wall/Plant/Gas",
    "Mushroom patch",
    "Ball",
    "Player",
    NULL
};


/*
 * Symbols of monsters in each group. Note the "Uniques" group
 * is handled differently.
 */
static cptr monster_group_char[] =
{
    (char *) -1L,
    (char *) -2L,
    (char *) -3L,
    (char *) -4L,
    (char *) -5L,
    (char *) -6L,
    (char *) -7L,
    "a",
    "b",
    "c",
    "dD",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "pt",
    "q",
    "r",
    "s",
    "uU",
    "v",
    "w",
    /* "x", */
    "y",
    "z",
    "A",
    "B",
    "C",
    /* "D", */
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    /* "U", */
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "!$&()+./=>?[\\]`{|~",
    "#%",
    ",",
    "*",
    "@",
    NULL
};


/*
 * Build a sorted list of monster races in the given group.
 *
 * mode & 0x01 : check for non-empty group
 * mode & 0x02 : visual operation only
 */
static int _mon_race_cmp_level(mon_race_ptr l, mon_race_ptr r)
{
    if (l->alloc.lvl < r->alloc.lvl) return 1;
    if (l->alloc.lvl > r->alloc.lvl) return -1;
    if (mon_race_is_unique(l) && !mon_race_is_unique(r)) return -1;
    if (!mon_race_is_unique(l) && mon_race_is_unique(r)) return 1;
    if (l->mexp < r->mexp) return 1;
    if (l->mexp > r->mexp) return -1;
    return strcmp(l->name, r->name);
}
static vec_ptr collect_monsters(int grp_cur, byte mode)
{
    int i;

    /* Get a list of x_char in this group */
    cptr group_char = monster_group_char[grp_cur];

    /* XXX Hack -- Check for special groups */
    bool        grp_corpses = (monster_group_char[grp_cur] == (char *) -1L);
    bool        grp_unique = (monster_group_char[grp_cur] == (char *) -2L);
    bool        grp_riding = (monster_group_char[grp_cur] == (char *) -3L);
    bool        grp_wanted = (monster_group_char[grp_cur] == (char *) -4L);
    bool        grp_amberite = (monster_group_char[grp_cur] == (char *) -5L);
    bool        grp_olympian = (monster_group_char[grp_cur] == (char *) -6L);
    bool        grp_guardian = (monster_group_char[grp_cur] == (char *) -7L);
    int_map_ptr available_corpses = NULL;

    vec_ptr v = vec_alloc(NULL);

    if (grp_corpses)
    {
        obj_ptr obj;
        available_corpses = int_map_alloc(NULL);

        /* In Pack */
        for (i = 1; i <= pack_max(); i++)
        {
            obj = pack_obj(i);
            if (!obj) continue;
            if (!object_is_(obj, TV_CORPSE, SV_CORPSE)) continue;
            int_map_add(available_corpses, obj->race_id, NULL);
        }

        /* At Home */
        for (i = 1; i <= home_max(); i++)
        {
            obj = home_obj(i);
            if (!obj) continue;
            if (!object_is_(obj, TV_CORPSE, SV_CORPSE)) continue;
            int_map_add(available_corpses, obj->race_id, NULL);
        }

        /* Underfoot */
        for (obj = dun_obj_at(cave, plr->pos); obj; obj = obj->next)
        {
            if (!object_is_(obj, TV_CORPSE, SV_CORPSE)) continue;
            int_map_add(available_corpses, obj->race_id, NULL);
        }

        /* Current Form for Easier Comparisons */
        if (plr->prace == RACE_MON_POSSESSOR && !plr_mon_race_is_("@.soul"))
            int_map_add(available_corpses, plr->current_r_idx, NULL);

    }


    /* Check every race */
    for (i = 0; i < vec_length(mon_alloc_tbl); i++)
    {
        mon_race_ptr r_ptr = vec_get(mon_alloc_tbl, i);

        /* Skip empty race */
        if (!plr->wizard && (r_ptr->flagsx & RFX_SUPPRESS)) continue;

        /* Require known monsters */
        #ifndef DEVELOPER
        if (!(mode & 0x02) && !plr->wizard && !r_ptr->lore.sightings) continue;
        #endif

        if (grp_corpses)
        {
            if (!int_map_contains(available_corpses, r_ptr->id))
                continue;
        }

        else if (grp_unique)
        {
            if (!mon_race_is_fixed_unique(r_ptr)) continue;
            if (mon_race_is_olympian(r_ptr)) continue; /* XXX Pending 'The Clash of the Titans' XXX */
        }

        else if (grp_guardian)
        {
            if (!(r_ptr->alloc.flags & RFA_GUARDIAN) && !(r_ptr->flagsx & RFX_GUARDIAN)) continue;
        }

        else if (grp_riding)
        {
            if (!mon_race_is_ridable(r_ptr)) continue;
        }

        else if (grp_wanted)
        {
            bool wanted = FALSE;
            int j;
            for (j = 0; j < MAX_KUBI; j++)
            {
                if (kubi_r_idx[j] == r_ptr->id || (plr->today_mon && plr->today_mon == r_ptr->id))
                {
                    wanted = TRUE;
                    break;
                }
            }
            if (!wanted) continue;
        }

        else if (grp_amberite)
        {
            if (!mon_race_is_amberite(r_ptr)) continue;
        }

        else if (grp_olympian)
        {
            if (!mon_race_is_olympian(r_ptr)) continue;
        }

        else
        {
            /* Check for race in the group */
            if (!mon_race_is_char_ex(r_ptr, group_char)) continue;
        }

        /* Add the race */
        vec_add(v, r_ptr);

        /* XXX Hack -- Just checking for non-empty group */
        if (mode & 0x01) break;
    }

    vec_sort(v, (vec_cmp_f)_mon_race_cmp_level);

    if (grp_corpses)
        int_map_free(available_corpses);

    return v;
}


/*
 * Description of each object group.
 */
static cptr object_group_text[] =
{
    "Food",
    "Potions",
/*  "Flasks", */
    "Scrolls",
/*  "Rings",
    "Amulets", */
/*  "Whistle", */
    "Lights",
/*  "Wands",
    "Staves",
    "Rods", */
/*  "Cards",
    "Capture Balls",
    "Parchments",
    "Spikes",
    "Boxs",
    "Figurines",
    "Statues",
    "Junks",
    "Bottles",
    "Skeletons",
    "Corpses", */
    "Swords",
    "Blunt Weapons",
    "Polearms",
    "Diggers",
    "Bows",
    "Shots",
    "Arrows",
    "Bolts",
    "Soft Armor",
    "Hard Armor",
    "Dragon Armor",
    "Shields",
    "Cloaks",
    "Gloves",
    "Helms",
    "Crowns",
    "Boots",
    "Spellbooks",
/*  "Treasure", */
    "Something",
    NULL
};


/*
 * TVALs of items in each group
 */
static byte object_group_tval[] =
{
    TV_FOOD,
    TV_POTION,
/*  TV_FLASK, */
    TV_SCROLL,
/*  TV_RING,
    TV_AMULET, */
/*  TV_WHISTLE, */
    TV_LIGHT,
/*  TV_WAND,
    TV_STAFF,
    TV_ROD,  */
/*  TV_CARD,
    TV_CAPTURE,
    TV_PARCHMENT,
    TV_SPIKE,
    TV_CHEST,
    TV_FIGURINE,
    TV_STATUE,
    TV_JUNK,
    TV_BOTTLE,
    TV_SKELETON,
    TV_CORPSE, */
    TV_SWORD,
    TV_HAFTED,
    TV_POLEARM,
    TV_DIGGING,
    TV_BOW,
    TV_SHOT,
    TV_ARROW,
    TV_BOLT,
    TV_SOFT_ARMOR,
    TV_HARD_ARMOR,
    TV_DRAG_ARMOR,
    TV_SHIELD,
    TV_CLOAK,
    TV_GLOVES,
    TV_HELM,
    TV_CROWN,
    TV_BOOTS,
    TV_LIFE_BOOK, /* Hack -- all spellbooks */
/*  TV_GOLD, */
    0,
    0,
};

static int _compare_k_level(obj_kind_ptr k1, obj_kind_ptr k2)
{
    if (k1->level < k2->level) return -1;
    if (k1->level > k2->level) return 1;
    return 0;
}

/*
 * Build a list of object indexes in the given group. Return the number
 * of objects in the group.
 *
 * mode & 0x01 : check for non-empty group
 * mode & 0x02 : visual operation only
 */
static int collect_objects(int grp_cur, vec_ptr kinds, byte mode)
{
    int i, j, k;

    /* Get a list of x_char in this group */
    byte group_tval = object_group_tval[grp_cur];

    vec_clear(kinds);

    for (i = 0; i < max_k_idx; i++)
    {
        /* Access the object */
        object_kind *k_ptr = &k_info[i];

        /* Skip empty objects */
        if (!k_ptr->name) continue;

        if (mode & 0x02)
        {
            /* Any objects will be displayed */
        }
        else
        {
            if (!k_ptr->flavor)
            {
                if (!k_ptr->counts.found && !k_ptr->counts.bought) continue;
            }

            /* Require objects ever seen */
            if (!k_ptr->aware) continue;

            /* Skip items with no distribution (special artifacts) */
            for (j = 0, k = 0; j < 4; j++) k += k_ptr->chance[j];
            if (!k) continue;
        }

        /* Check for objects in the group */
        if (TV_LIFE_BOOK == group_tval)
        {
            /* Hack -- All spell books */
            if (TV_LIFE_BOOK <= k_ptr->tval && k_ptr->tval <= TV_BLESS_BOOK)
                vec_add(kinds, k_ptr);
            else continue;
        }
        else if (k_ptr->tval == group_tval)
            vec_add(kinds, k_ptr);
        else continue;

        /* XXX Hack -- Just checking for non-empty group */
        if (mode & 0x01) break;
    }

    vec_sort(kinds, (vec_cmp_f)_compare_k_level);

    return vec_length(kinds);
}

void do_cmd_save_screen_term(void)
{
    Term_dump(Term_rect());
}

void do_cmd_save_screen_doc(void)
{
    str_ptr s = get_screenshot();
    char    buf[1024];
    FILE   *fff;

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "screen.doc");
    FILE_TYPE(FILE_TYPE_TEXT);
    fff = my_fopen(buf, "w");
    if (fff)
    {
        str_write_file(s, fff);
        my_fclose(fff);
    }
    str_free(s);
}

void save_screen_aux(cptr file, int format)
{
    str_ptr s = get_screenshot();
    doc_ptr doc = doc_alloc(Term->wid);
    FILE   *fff;

    doc_insert(doc, "<style:screenshot>");
    doc_insert(doc, str_buffer(s));
    doc_insert(doc, "</style>");

    FILE_TYPE(FILE_TYPE_TEXT);
    fff = my_fopen(file, "w");
    if (fff)
    {
        doc_write_file(doc, fff, format);
        my_fclose(fff);
    }
    str_free(s);
    doc_free(doc);
}

static void _save_screen_aux(int format)
{
    char buf[1024];

    if (format == DOC_FORMAT_HTML)
        path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "screen.html");
    else
        path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "screen.txt");

    save_screen_aux(buf, format);
}

void do_cmd_save_screen_txt(void)
{
    _save_screen_aux(DOC_FORMAT_TEXT);
}

void do_cmd_save_screen_html(void)
{
    _save_screen_aux(DOC_FORMAT_HTML);
}

void do_cmd_save_screen(void)
{
    str_ptr s = get_screenshot();
    doc_ptr doc = doc_alloc(Term->wid);

    doc_insert(doc, "<style:screenshot>");
    doc_insert(doc, str_buffer(s));
    doc_insert(doc, "</style>");
    screen_save();
    doc_display(doc, "Current Screenshot", 0);
    screen_load();

    str_free(s);
    doc_free(doc);
}

/************************************************************************
 * Artifact Lore (Standard Arts Only)
 * Note: Check out the Wizard Spoiler Commands for an alternative approach.
 *       ^a"a and ^a"O
 ************************************************************************/
typedef struct {
    object_p filter;
    cptr     name;
} _art_type_t, *_art_type_ptr;

static _art_type_t _art_types[] = {
    { obj_is_weapon, "Weapons" },
    { obj_is_shield, "Shield" },
    { obj_is_bow, "Bows" },
    { obj_is_ring, "Rings" },
    { obj_is_amulet, "Amulets" },
    { obj_is_light, "Lights" },
    { obj_is_body_armor, "Body Armor" },
    { obj_is_cloak, "Cloaks" },
    { obj_is_helmet, "Helmets" },
    { obj_is_gloves, "Gloves" },
    { obj_is_boots, "Boots" },
    { obj_is_ammo, "Ammo" },
    { NULL, NULL },
};

static int _art_cmp(art_ptr l, art_ptr r)
{
    if (l->level < r->level) return -1;
    if (l->level > r->level) return 1;
    if (l->id < r->id) return -1;
    if (l->id > r->id) return 1;
    return 0;
}
static _art_type_ptr _art_type;
static bool _art_show_all;
static bool _art_filter(int id, art_ptr art)
{
    obj_t forge;
    if (!art->found)
    {
        if (!_art_show_all) return FALSE;
        if (!art_has_lore(art)) return FALSE;
    }
    if (!art_create_std(&forge, art, AM_DEBUG | AM_NO_DROP)) return FALSE;
    if (!_art_type->filter(&forge)) return FALSE;
    return TRUE;
}
static vec_ptr _collect_arts(int grp_cur, bool show_all)
{
    vec_ptr v;
    _art_type = &_art_types[grp_cur];
    _art_show_all = show_all;
    v = arts_filter_ex(_art_filter);
    vec_sort(v, (vec_cmp_f)_art_cmp);
    return v;
}

static void do_cmd_knowledge_artifacts(void)
{
    static bool show_all = TRUE;

    int i, len, max;
    int grp_cur, grp_top, old_grp_cur;
    int art_cur, art_top;
    int grp_cnt, grp_idx[100];
    vec_ptr v = NULL;

    int column = 0;
    bool flag;
    bool redraw;
    bool rebuild;

    int browser_rows;
    int wid, hgt;

    if (random_artifacts)
    {
        /* FIXED_ART ... 
        if (random_artifact_pct >= 100)
        {
            cmsg_print(TERM_L_RED, "You won't find any fixed artifacts this game.");
            return;
        }
        */
    }
    else if (no_artifacts)
    {
        cmsg_print(TERM_L_RED, "You won't find any artifacts this game.");
        return;
    }

    /* Get size */
    Term_get_size(&wid, &hgt);

    browser_rows = hgt - 8;

    max = 0;
    grp_cnt = 0;
    for (i = 0; _art_types[i].filter; i++)
    {
        len = strlen(_art_types[i].name);
        if (len > max)
            max = len;

        v = _collect_arts(i, TRUE);
        if (vec_length(v))
            grp_idx[grp_cnt++] = i;
        vec_free(v);
        v = NULL;
    }
    grp_idx[grp_cnt] = -1;

    if (!grp_cnt)
    {
        prt("You haven't found any artifacts just yet. Press any key to continue.", 0, 0);
        inkey();
        prt("", 0, 0);
        return;
    }


    old_grp_cur = -1;
    grp_cur = grp_top = 0;
    art_cur = art_top = 0;

    flag = FALSE;
    redraw = TRUE;
    rebuild = TRUE;

    while (!flag)
    {
        char ch;
        if (redraw)
        {
            clear_from(0);

            prt(format("%s - Artifacts", "Knowledge"), 2, 0);
            prt("Group", 4, 0);
            prt("Name", 4, max + 3);

            for (i = 0; i < 72; i++)
            {
                Term_putch(i, 5, TERM_WHITE, '=');
            }

            for (i = 0; i < browser_rows; i++)
            {
                Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
            }

            redraw = FALSE;
        }

        /* Scroll group list */
        if (grp_cur < grp_top) grp_top = grp_cur;
        if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

        /* Display a list of object groups */
        for (i = 0; i < browser_rows && grp_idx[i] >= 0; i++)
        {
            int  grp = grp_idx[grp_top + i];
            byte attr = (grp_top + i == grp_cur) ? TERM_L_BLUE : TERM_WHITE;

            Term_erase(0, 6 + i, max);
            c_put_str(attr, _art_types[grp].name, 6 + i, 0);
        }

        if (rebuild || old_grp_cur != grp_cur)
        {
            old_grp_cur = grp_cur;
            if (v) vec_free(v);
            v = _collect_arts(grp_idx[grp_cur], show_all);
            art_cur = 0;
            rebuild = FALSE;
        }

        /* Scroll object list */
        while (art_cur < art_top)
            art_top = MAX(0, art_top - browser_rows/2);
        while (art_cur >= art_top + browser_rows)
            art_top = MIN(vec_length(v) - browser_rows, art_top + browser_rows/2);

        /* Display a list of artifacts in the current group */
        /* Display lines until done */
        for (i = 0; i < browser_rows && art_top + i < vec_length(v); i++)
        {
            char        name[MAX_NLEN];
            art_ptr     art = vec_get(v, art_top + i);
            object_type forge;
            obj_kind_ptr kind;
            byte        attr = TERM_WHITE;

            art_create_std(&forge, art, AM_DEBUG | AM_NO_DROP);
            kind = &k_info[forge.k_idx];
            forge.ident = IDENT_KNOWN;
            object_desc(name, &forge, OD_OMIT_INSCRIPTION);

            if (i + art_top == art_cur)
                attr = TERM_L_BLUE;
            else if (!art->found)
                attr = TERM_L_DARK;
            else
                attr = tv_color(forge.tval);

            Term_queue_bigchar(max + 3, 6 + i, kind->x_attr, kind->x_char, 0, 0);
            c_prt(attr, name, 6 + i, max + 4);
        }

        /* Clear remaining lines */
        for (; i < browser_rows; i++)
        {
            Term_erase(max + 3, 6 + i, 255);
        }

        if (show_all)
            prt("<dir>, 'r' to recall, 't' to Hide Unfound, ESC", hgt - 1, 0);
        else
            prt("<dir>, 'r' to recall, 't' to Show All, ESC", hgt - 1, 0);

        if (!column)
        {
            Term_gotoxy(0, 6 + (grp_cur - grp_top));
        }
        else
        {
            Term_gotoxy(max + 3, 6 + (art_cur - art_top));
        }

        ch = inkey();

        switch (ch)
        {
        case ESCAPE:
            flag = TRUE;
            break;

        case 'T': case 't':
            show_all = !show_all;
            art_cur = 0;
            rebuild = TRUE;
            break;

        case 'R': case 'r':
        case 'I': case 'i':
            if (grp_cnt > 0 && art_cur < vec_length(v))
            {
                art_ptr art = vec_get(v, art_cur);
                object_type forge;
                art_create_std(&forge, art, AM_DEBUG | AM_NO_DROP);
                forge.ident = IDENT_KNOWN;
                obj_display(&forge);
                redraw = TRUE;
            }
            break;

        default:
            browser_cursor(ch, &column, &grp_cur, grp_cnt, &art_cur, vec_length(v));
        }
    }
    if (v) vec_free(v);
}


/*
 * Display known uniques
 * With "XTRA HACK UNIQHIST" (Originally from XAngband)
 */
static bool _unique(mon_race_ptr r)
{
    if (!mon_race_is_living_unique(r)) return FALSE;
    if (r->flagsx & RFX_SUPPRESS) return FALSE;
    if (!r->lore.sightings) return FALSE;
    if (!r->alloc.rarity) return FALSE;
    if (r->alloc.rarity > 100 && !(r->flagsx & RFX_QUESTOR)) return FALSE;
    return TRUE;
}
static int _cmp_unique(mon_race_ptr l, mon_race_ptr r)
{
    if (l->alloc.lvl < r->alloc.lvl) return 1;
    if (l->alloc.lvl > r->alloc.lvl) return -1;
    if (l->mexp < r->mexp) return 1;
    if (l->mexp > r->mexp) return -1;
    return strcmp(l->name, r->name);
}
static void do_cmd_knowledge_uniques(void)
{
    vec_ptr v = mon_race_filter(_unique);
    doc_ptr doc = doc_alloc(80);

    if (!vec_length(v))
        doc_insert(doc, "There are no known living uniques.");
    else
    {
        int i, max_bucket = 0;
        int cts[12] = {0};

        vec_sort(v, (vec_cmp_f)_cmp_unique);

        for (i = 0; i < vec_length(v); i++)
        {
            mon_race_ptr r = vec_get(v, i);
            int bucket;

            if (r->alloc.lvl == 0) bucket = 0;
            if (r->alloc.lvl >= 100) bucket = 11;
            else bucket = (r->alloc.lvl + 9)/10; /* e.g. 21-30 -> 3 */

            cts[bucket]++;
            max_bucket = MAX(bucket, max_bucket);
        }

        for (i = max_bucket; i >= 0; i--)
        {
            doc_insert(doc, " <color:U>");
            if (i == 0)
                doc_insert(doc, "Surface      ");
            else if (i == 11)
                doc_insert(doc, "Level 100+   ");
            else
            {
                int l = 1 + (i-1)*10;
                int h = i*10;
                doc_printf(doc, "Level %3d-%3d", l, h);
            }
            doc_printf(doc, "</color>: %3d\n", cts[i]);
        }
        doc_printf(doc, " <color:R>Total        </color>: %3d\n\n", vec_length(v));

        for (i = 0; i < vec_length(v); i++)
        {
            mon_race_ptr r = vec_get(v, i);
            doc_printf(doc, " L%3d ", r->alloc.lvl);
            doc_insert_term_char(doc, mon_race_visual(r));
            doc_printf(doc, " %s\n", r->name);
        }
    }

    screen_save();
    doc_display(doc, "Remaining Uniques", 0);
    screen_load();

    vec_free(v);
    doc_free(doc);
}

void do_cmd_knowledge_shooter(void)
{
    plr_shoot_display();
}

void do_cmd_knowledge_weapon(void)
{
    plr_attack_display();
}

static void do_cmd_knowledge_extra(void)
{
    doc_ptr  doc = doc_alloc(80);

    doc_insert(doc, "<style:wide>");
    plr_hook_character_dump(doc);
    doc_insert(doc, "</style>");

    doc_display(doc, "Race/Class Extra Information", 0);
    doc_free(doc);
}

/*
 * Display weapon-exp.
 */
static int _compare_k_lvl(object_kind *left, object_kind *right)
{
    if (left->level < right->level) return -1;
    if (left->level > right->level) return 1;
    return 0;
}

static vec_ptr _prof_weapon_alloc(int tval)
{
    int i;
    vec_ptr v = vec_alloc(NULL);
    for (i = 0; i < max_k_idx; i++)
    {
        object_kind *k_ptr = &k_info[i];
        if (k_ptr->tval != tval) continue;
        if (tval == TV_POLEARM && k_ptr->sval == SV_DEATH_SCYTHE_HACK) continue;
        if (tval == TV_BOW && k_ptr->sval == SV_HARP) continue;
        if (tval == TV_BOW && k_ptr->sval == SV_CRIMSON) continue;
        if (tval == TV_BOW && k_ptr->sval == SV_RAILGUN) continue;
        vec_add(v, k_ptr);
    }
    vec_sort(v, (vec_cmp_f)_compare_k_lvl);
    return v;
}
 
static cptr _prof_exp_str[5]   = {"[Un]", "[Be]", "[Sk]", "[Ex]", "[Ma]"};
static char _prof_exp_color[5] = {'w',    'G',    'y',    'r',    'v'};
static cptr _prof_weapon_heading(int tval)
{
    switch (tval)
    {
    case TV_SWORD: return "Swords";
    case TV_POLEARM: return "Polearms";
    case TV_HAFTED: return "Hafted";
    case TV_DIGGING: return "Diggers";
    case TV_BOW: return "Bows";
    }
    return "";
}

static void _prof_weapon_doc(doc_ptr doc, int tval)
{
    vec_ptr v = _prof_weapon_alloc(tval);
    int     i;

    doc_insert_text(doc, TERM_RED, _prof_weapon_heading(tval));
    doc_newline(doc);

    for (i = 0; i < vec_length(v); i++)
    {
        object_kind *k_ptr = vec_get(v, i);
        int          exp = skills_weapon_current(k_ptr->tval, k_ptr->sval);
        int          max = skills_weapon_max(k_ptr->tval, k_ptr->sval);
        int          exp_lvl = weapon_exp_level(exp);
        char         name[MAX_NLEN];
        char         color = 'w';

        /* XXX player knows which weapons are ok for riding */
        if ( (plr->pclass == CLASS_BEASTMASTER || plr->pclass == CLASS_CAVALRY)
          && tval != TV_BOW
          && !have_flag(k_ptr->flags, OF_RIDING) )
        {
            color = 'D';
        }
        if (equip_find_obj(k_ptr->tval, k_ptr->sval))
            color = 'B';
        strip_name(name, k_ptr->idx);
        doc_printf(doc, "<color:%c>%-19s</color> ", color, name);
        doc_printf(doc, "%c<color:%c>%-4s</color>", exp >= max ? '!' : ' ', _prof_exp_color[exp_lvl], _prof_exp_str[exp_lvl]);
        doc_newline(doc);
    }
    doc_newline(doc);
    vec_free(v);
}

static void _prof_skill_aux(doc_ptr doc, int skill)
{
    int  exp, max, exp_lvl;
    cptr name;
    char color = 'w';

    switch (skill)
    {
    case SKILL_MARTIAL_ARTS:
        name = "Martial Arts";
        exp = skills_martial_arts_current();
        max = skills_martial_arts_max();
        exp_lvl = weapon_exp_level(exp);
        break;
    case SKILL_DUAL_WIELDING:
        name = "Dual Wielding";
        exp = skills_dual_wielding_current();
        max = skills_dual_wielding_max();
        exp_lvl = weapon_exp_level(exp);
        break;
    case SKILL_RIDING:
    default: /* gcc warnings ... */
        name = "Riding";
        exp = skills_riding_current();
        max = skills_riding_max();
        exp_lvl = riding_exp_level(exp);
        break;
    }
    doc_printf(doc, "<color:%c>%-19s</color> ", color, name);
    doc_printf(doc, "%c<color:%c>%-4s</color>", exp >= max ? '!' : ' ', _prof_exp_color[exp_lvl], _prof_exp_str[exp_lvl]);
    doc_newline(doc);
}

static void _prof_skill_doc(doc_ptr doc)
{
    doc_insert_text(doc, TERM_RED, "Miscellaneous");
    doc_newline(doc);
    _prof_skill_aux(doc, SKILL_MARTIAL_ARTS);
    _prof_skill_aux(doc, SKILL_DUAL_WIELDING);
    _prof_skill_aux(doc, SKILL_RIDING);
    doc_newline(doc);
}

static void do_cmd_knowledge_weapon_exp(void)
{
    doc_ptr doc = doc_alloc(80);
    doc_ptr cols[3] = {0};
    int     i;

    for (i = 0; i < 3; i++)
        cols[i] = doc_alloc(26);

    _prof_weapon_doc(cols[0], TV_SWORD);
    _prof_weapon_doc(cols[1], TV_POLEARM);
    _prof_weapon_doc(cols[1], TV_BOW);
    _prof_weapon_doc(cols[2], TV_HAFTED);
    _prof_weapon_doc(cols[2], TV_DIGGING);
    _prof_skill_doc(cols[2]);

    doc_insert_cols(doc, cols, 3, 1);
    doc_display(doc, "Proficiency", 0);

    doc_free(doc);
    for (i = 0; i < 3; i++)
        doc_free(cols[i]);
}

/*
 * Display spell-exp
 */
static void do_cmd_knowledge_spell_exp(void)
{
    doc_ptr doc = doc_alloc(80);

    doc_insert(doc, "<style:wide>");
    spellbook_character_dump(doc);
    doc_insert(doc, "</style>");
    doc_display(doc, "Spell Proficiency", 0);
    doc_free(doc);
}

/*
 * Pluralize a monster name
 */
static bool _plural_imp(char *name, const char *suffix, const char *replacement)
{
    bool result = FALSE;
    int l1 = strlen(name);
    int l2 = strlen(suffix);

    if (l1 >= l2)
    {
        char *tmp = name + (l1 - l2);
        if (streq(tmp, suffix))
        {
            strcpy(tmp, replacement);
            result = TRUE;
        }
    }
    return result;
}

void plural_aux(char *Name)
{
    int NameLen = strlen(Name);

    if (my_strstr(Name, "Disembodied hand"))
    {
        strcpy(Name, "Disembodied hands that strangled people");
    }
    else if (my_strstr(Name, "Colour out of space"))
    {
        strcpy(Name, "Colours out of space");
    }
    else if (my_strstr(Name, "stairway to hell"))
    {
        strcpy(Name, "stairways to hell");
    }
    else if (my_strstr(Name, "Dweller on the threshold"))
    {
        strcpy(Name, "Dwellers on the threshold");
    }
    else if (my_strstr(Name, " of "))
    {
        cptr aider = my_strstr(Name, " of ");
        char dummy[80];
        int i = 0;
        cptr ctr = Name;

        while (ctr < aider)
        {
            dummy[i] = *ctr;
            ctr++; i++;
        }

        if (dummy[i-1] == 's')
        {
            strcpy(&(dummy[i]), "es");
            i++;
        }
        else
        {
            strcpy(&(dummy[i]), "s");
        }

        strcpy(&(dummy[i+1]), aider);
        strcpy(Name, dummy);
    }
    else if (my_strstr(Name, "coins"))
    {
        char dummy[80];
        strcpy(dummy, "piles of ");
        strcat(dummy, Name);
        strcpy(Name, dummy);
        return;
    }
    else if (my_strstr(Name, "Manes"))
    {
        return;
    }
    else if (_plural_imp(Name, "ey", "eys"))
    {
    }
    else if (_plural_imp(Name, "y", "ies"))
    {
    }
    else if (_plural_imp(Name, "ouse", "ice"))
    {
    }
    else if (_plural_imp(Name, "us", "i"))
    {
    }
    else if (_plural_imp(Name, "kelman", "kelmen"))
    {
    }
    else if (_plural_imp(Name, "wordsman", "wordsmen"))
    {
    }
    else if (_plural_imp(Name, "oodsman", "oodsmen"))
    {
    }
    else if (_plural_imp(Name, "eastman", "eastmen"))
    {
    }
    else if (_plural_imp(Name, "izardman", "izardmen"))
    {
    }
    else if (_plural_imp(Name, "geist", "geister"))
    {
    }
    else if (_plural_imp(Name, "ex", "ices"))
    {
    }
    else if (_plural_imp(Name, "lf", "lves"))
    {
    }
    else if (suffix(Name, "ch") ||
         suffix(Name, "sh") ||
             suffix(Name, "nx") ||
             suffix(Name, "s") ||
             suffix(Name, "o"))
    {
        strcpy(&(Name[NameLen]), "es");
    }
    else
    {
        strcpy(&(Name[NameLen]), "s");
    }
}

/*
 * Display current pets 
 * XXX Share this with _build_pets
 * XXX Give an interactive pets display, allowing naming, dismissal, etc
 */
static void _mon_pos_doc(mon_ptr mon, doc_ptr doc)
{
    if (mon->dun->id == plr->dun_id)
    {
        point_t v = point_subtract(mon->pos, plr->pos); /* vector from plr to mon */
        if (v.x || v.y)
        {
            doc_insert_char(doc, TERM_WHITE, '[');
            if (v.y)
            {
                doc_printf(doc, "%c%d", (v.y > 0) ? 'S' : 'N', abs(v.y));
                if (v.x) doc_insert_char(doc, TERM_WHITE, ',');
            }
            if (v.x)
                doc_printf(doc, "%c%d", (v.x > 0) ? 'E' : 'W', abs(v.x));
            doc_insert(doc, "] ");
        }
    }
    else doc_insert(doc, "[<color:R>Off Level</color>] ");
}
static void _mon_health_doc(mon_ptr mon, doc_ptr doc) /* XXX share this with prt_mon_health_bar */
{
    int pct = 100 * mon->hp / mon->max_maxhp;
    byte attr = TERM_RED;/* Default to almost dead */

    if (pct >= 100 || mon->hp == mon->maxhp) attr = TERM_L_GREEN;
    else if (pct >= 60) attr = TERM_YELLOW;
    else if (pct >= 25) attr = TERM_ORANGE;
    else if (pct >= 10) attr = TERM_L_RED;

    doc_printf(doc, "<color:%c>%3d%%</color> ", attr_to_attr_char(attr), pct); 
}
static void _pet_doc(mon_ptr pet, doc_ptr doc)
{
    char pet_name[MAX_NLEN_MON];
    mon_race_ptr race = pet->apparent_race;

    doc_insert(doc, "  <indent><style:indent>");
    doc_insert_term_char(doc, mon_race_visual(race));
    monster_desc(pet_name, pet, MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE | MD_NO_PET_ABBREV);
    doc_printf(doc, " %s (L%d) ", pet_name, race->alloc.lvl);
    _mon_pos_doc(pet, doc);
    _mon_health_doc(pet, doc);
    mon_tim_display(pet, doc);
    doc_insert(doc, "</style></indent>\n");
}
static void do_cmd_knowledge_pets(void)
{
    doc_ptr doc = doc_alloc(80);
    mon_pack_ptr pets = plr_pets();
    int     ct = mon_pack_count(pets);
    int     i;

    if (ct)
    {
        doc_printf(doc, "  <color:G>Current Pets</color>\n");
        for (i = 0; i < ct; i++)
        {
            mon_ptr pet = vec_get(pets->members, i);
            _pet_doc(pet, doc);
        }
        doc_printf(doc, "  <color:R>Total :</color> %d pet%s.\n", ct, (ct == 1) ? "" : "s");
        doc_printf(doc, "  <color:R>Upkeep:</color> %d%% mana.\n", calculate_upkeep());
    }
    else
        doc_insert(doc, "  <color:U>You have no pets.</color>\n");

    doc_printf(doc, "\n  <color:G>Options</color>\n");
    doc_printf(doc, "  Pets open doors:                    %s\n", (plr->pet_extra_flags & PF_OPEN_DOORS) ? "ON" : "OFF");
    doc_printf(doc, "  Pets pick up items:                 %s\n", (plr->pet_extra_flags & PF_PICKUP_ITEMS) ? "ON" : "OFF");
    doc_printf(doc, "  Allow teleport:                     %s\n", (plr->pet_extra_flags & PF_TELEPORT) ? "ON" : "OFF");
    doc_printf(doc, "  Allow cast attack spell:            %s\n", (plr->pet_extra_flags & PF_ATTACK_SPELL) ? "ON" : "OFF");
    doc_printf(doc, "  Allow cast summon spell:            %s\n", (plr->pet_extra_flags & PF_SUMMON_SPELL) ? "ON" : "OFF");
    doc_printf(doc, "  Allow involve player in area spell: %s\n", (plr->pet_extra_flags & PF_BALL_SPELL) ? "ON" : "OFF");
    if (plr->wizard)
        doc_printf(doc, "  Riding Skill:                       %d\n", skills_riding_current());

    doc_newline(doc);
    doc_display(doc, "Pets", 0);

    doc_free(doc);
}

/*
 * Display the object groups.
 */
static void display_group_list(int col, int row, int wid, int per_page,
    int grp_idx[], cptr group_text[], int grp_cur, int grp_top)
{
    int i;

    /* Display lines until done */
    for (i = 0; i < per_page && (grp_idx[i] >= 0); i++)
    {
        /* Get the group index */
        int grp = grp_idx[grp_top + i];

        /* Choose a color */
        byte attr = (grp_top + i == grp_cur) ? TERM_L_BLUE : TERM_WHITE;

        /* Erase the entire line */
        Term_erase(col, row + i, wid);

        /* Display the group label */
        c_put_str(attr, group_text[grp], row + i, col);
    }
}


/*
 * Move the cursor in a browser window
 */
static void browser_cursor(char ch, int *column, int *grp_cur, int grp_cnt,
                           int *list_cur, int list_cnt)
{
    int d;
    int col = *column;
    int grp = *grp_cur;
    int list = *list_cur;

    /* Extract direction */
    if (ch == ' ')
    {
        /* Hack -- scroll up full screen */
        d = 3;
    }
    else if (ch == '-')
    {
        /* Hack -- scroll down full screen */
        d = 9;
    }
    else
    {
        d = get_keymap_dir(ch);
    }

    if (!d) return;

    /* Diagonals - hack */
    if ((ddx[d] > 0) && ddy[d])
    {
        int browser_rows;
        int wid, hgt;

        /* Get size */
        Term_get_size(&wid, &hgt);

        browser_rows = hgt - 8;

        /* Browse group list */
        if (!col)
        {
            int old_grp = grp;

            /* Move up or down */
            grp += ddy[d] * (browser_rows - 1);

            /* Verify */
            if (grp >= grp_cnt)    grp = grp_cnt - 1;
            if (grp < 0) grp = 0;
            if (grp != old_grp)    list = 0;
        }

        /* Browse sub-list list */
        else
        {
            /* Move up or down */
            list += ddy[d] * browser_rows;

            /* Verify */
            if (list >= list_cnt) list = list_cnt - 1;
            if (list < 0) list = 0;
        }

        (*grp_cur) = grp;
        (*list_cur) = list;

        return;
    }

    if (ddx[d])
    {
        col += ddx[d];
        if (col < 0) col = 0;
        if (col > 1) col = 1;

        (*column) = col;

        return;
    }

    /* Browse group list */
    if (!col)
    {
        int old_grp = grp;

        /* Move up or down */
        grp += ddy[d];

        /* Verify */
        if (grp >= grp_cnt)    grp = grp_cnt - 1;
        if (grp < 0) grp = 0;
        if (grp != old_grp)    list = 0;
    }

    /* Browse sub-list list */
    else
    {
        /* Move up or down */
        list += ddy[d];

        /* Verify */
        if (list >= list_cnt) list = list_cnt - 1;
        if (list < 0) list = 0;
    }

    (*grp_cur) = grp;
    (*list_cur) = list;
}


/*
 * Display visuals.
 */
static void display_visual_list(int col, int row, int height, int width, byte attr_top, byte char_left)
{
    int i, j;

    /* Clear the display lines */
    for (i = 0; i < height; i++)
    {
        Term_erase(col, row + i, width);
    }

    /* Bigtile mode uses double width */
    if (use_bigtile) width /= 2;

    /* Display lines until done */
    for (i = 0; i < height; i++)
    {
        /* Display columns until done */
        for (j = 0; j < width; j++)
        {
            byte a;
            char c;
            int x = col + j;
            int y = row + i;
            int ia, ic;

            /* Bigtile mode uses double width */
            if (use_bigtile) x += j;

            ia = attr_top + i;
            ic = char_left + j;

            /* Ignore illegal characters */
            if (ia > 0x7f || ic > 0xff || ic < ' ' ||
                (!use_graphics && ic > 0x7f))
                continue;

            a = (byte)ia;
            c = (char)ic;

            /* Force correct code for both ASCII character and tile */
            if (c & 0x80) a |= 0x80;

            /* Display symbol */
            Term_queue_bigchar(x, y, a, c, 0, 0);
        }
    }
}


/*
 * Place the cursor at the collect position for visual mode
 */
static void place_visual_list_cursor(int col, int row, byte a, byte c, byte attr_top, byte char_left)
{
    int i = (a & 0x7f) - attr_top;
    int j = c - char_left;

    int x = col + j;
    int y = row + i;

    /* Bigtile mode uses double width */
    if (use_bigtile) x += j;

    /* Place the cursor */
    Term_gotoxy(x, y);
}


/*
 *  Clipboard variables for copy&paste in visual mode
 */
static byte attr_idx = 0;
static byte char_idx = 0;

/*
 *  Do visual mode command -- Change symbols
 */
static bool visual_mode_command(char ch, bool *visual_list_ptr,
                int height, int width,
                byte *attr_top_ptr, byte *char_left_ptr,
                byte *cur_attr_ptr, byte *cur_char_ptr, bool *need_redraw)
{
    static byte attr_old = 0, char_old = 0;

    switch (ch)
    {
    case ESCAPE:
        if (*visual_list_ptr)
        {
            /* Cancel change */
            *cur_attr_ptr = attr_old;
            *cur_char_ptr = char_old;
            *visual_list_ptr = FALSE;

            return TRUE;
        }
        break;

    case '\n':
    case '\r':
        if (*visual_list_ptr)
        {
            /* Accept change */
            *visual_list_ptr = FALSE;
            *need_redraw = TRUE;

            return TRUE;
        }
        break;

    case 'V':
    case 'v':
        if (!*visual_list_ptr)
        {
            *visual_list_ptr = TRUE;

            *attr_top_ptr = MAX(0, (*cur_attr_ptr & 0x7f) - 5);
            *char_left_ptr = MAX(0, *cur_char_ptr - 10);

            attr_old = *cur_attr_ptr;
            char_old = *cur_char_ptr;

            return TRUE;
        }
        break;

    case 'C':
    case 'c':
        /* Set the visual */
        attr_idx = *cur_attr_ptr;
        char_idx = *cur_char_ptr;
        return TRUE;

    case 'P':
    case 'p':
        if (attr_idx || (!(char_idx & 0x80) && char_idx)) /* Allow TERM_DARK text */
        {
            /* Set the char */
            *cur_attr_ptr = attr_idx;
            *attr_top_ptr = MAX(0, (*cur_attr_ptr & 0x7f) - 5);
            if (!*visual_list_ptr) *need_redraw = TRUE;
        }

        if (char_idx)
        {
            /* Set the char */
            *cur_char_ptr = char_idx;
            *char_left_ptr = MAX(0, *cur_char_ptr - 10);
            if (!*visual_list_ptr) *need_redraw = TRUE;
        }

        return TRUE;

    default:
        if (*visual_list_ptr)
        {
            int eff_width;
            int d = get_keymap_dir(ch);
            byte a = (*cur_attr_ptr & 0x7f);
            byte c = *cur_char_ptr;

            if (use_bigtile) eff_width = width / 2;
            else eff_width = width;

            /* Restrict direction */
            if ((a == 0) && (ddy[d] < 0)) d = 0;
            if ((c == 0) && (ddx[d] < 0)) d = 0;
            if ((a == 0x7f) && (ddy[d] > 0)) d = 0;
            if ((c == 0xff) && (ddx[d] > 0)) d = 0;

            a += ddy[d];
            c += ddx[d];

            /* Force correct code for both ASCII character and tile */
            if (c & 0x80) a |= 0x80;

            /* Set the visual */
            *cur_attr_ptr = a;
            *cur_char_ptr = c;


            /* Move the frame */
            if ((ddx[d] < 0) && *char_left_ptr > MAX(0, (int)c - 10)) (*char_left_ptr)--;
            if ((ddx[d] > 0) && *char_left_ptr + eff_width < MIN(0xff, (int)c + 10)) (*char_left_ptr)++;
            if ((ddy[d] < 0) && *attr_top_ptr > MAX(0, (int)(a & 0x7f) - 4)) (*attr_top_ptr)--;
            if ((ddy[d] > 0) && *attr_top_ptr + height < MIN(0x7f, (a & 0x7f) + 4)) (*attr_top_ptr)++;
            return TRUE;
        }
        break;
    }

    /* Visual mode command is not used */
    return FALSE;
}

enum monster_mode_e
{
    MONSTER_MODE_STATS,
    MONSTER_MODE_SKILLS,
    MONSTER_MODE_EXTRA,
    MONSTER_MODE_MAX
};
static int monster_mode = MONSTER_MODE_STATS;

static term_char_t _equippy_char(int tval, int sval)
{
    int k_idx = lookup_kind(tval, sval);
    object_kind *k_ptr = &k_info[k_idx];
    term_char_t tc; /* XXX */
    tc.a = k_ptr->x_attr; /* XXX */
    tc.c = k_ptr->x_char; /* XXX */
    return tc;
}

static void _prt_equippy(int col, int row, int tval, int sval)
{
    Term_queue_term_char(point_create(col, row), _equippy_char(tval, sval));
}

/*
 * Display the monsters in a group.
 */
static void display_monster_list(int col, int row, int per_page, vec_ptr races,
    int mon_cur, int mon_top, bool visual_only)
{
    int i;

    /* Display lines until done */
    for (i = 0; i < per_page; i++)
    {
        byte attr;
        int j = mon_top + i;
        mon_race_ptr r_ptr;
        term_char_t tc;

        if (j >= vec_length(races)) break;

        r_ptr = vec_get(races, j);
        tc = mon_race_visual(r_ptr);

        /* Choose a color */
        attr = ((j == mon_cur) ? TERM_L_BLUE : TERM_WHITE);
        if (attr == TERM_WHITE && (r_ptr->flagsx & RFX_SUPPRESS))
            attr = TERM_L_DARK;

        /* Display the name */
        c_prt(attr, r_ptr->name, row + i, col);

        /* Hack -- visual_list mode */
        if (per_page == 1)
            c_prt(attr, format("%02x/%02x", tc.a, tc.c), row + i, (plr->wizard || visual_only) ? 46 : 51);

        /* Erase chars before overwritten by the race letter */
        Term_erase(53, row + i, 255);

        /* Display symbol */
        Term_queue_bigchar(use_bigtile ? 53 : 54, row + i, tc.a, tc.c, 0, 0);

        if (!visual_only)
        {
            /* Display kills */
            if (!mon_race_is_fixed_unique(r_ptr)) put_str(format("%5d", r_ptr->lore.kills.current), row + i, 57);
            else c_put_str((r_ptr->alloc.max_num == 0 ? TERM_L_DARK : TERM_WHITE), (r_ptr->alloc.max_num == 0 ? " dead" : "alive"), row + i, 57);

            /* Only Possessors get the extra body info display */
            if (plr->wizard || plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
            {
                /* And then, they must learn about the body first. (Or be a cheating wizard :) */
                if ((plr->wizard || (r_ptr->lore.flags & RFL_POSSESSOR)) && !(r_ptr->body.flags & RF_POS_DISABLED))
                {
                    char buf[255];
                    equip_template_ptr body = equip_template_lookup(r_ptr->body.body_id);
                    if (monster_mode == MONSTER_MODE_STATS)
                    {
                        int j;
                        for (j = 0; j < 6; j++)
                        {
                            int k = r_ptr->body.stats[j];
                            k += r_ptr->body.extra_stats[j]*plr->lev/50;
                            sprintf(buf, "%+3d", k);
                            c_put_str(j == r_ptr->body.spell_stat ? TERM_L_GREEN : TERM_WHITE,
                                      buf, row + i, 64 + j * 5);
                        }
                        if (r_ptr->body.life)
                            sprintf(buf, "%+3d%%", r_ptr->body.life);
                        else
                            sprintf(buf, "%+3d%%", 100);
                        c_put_str(TERM_WHITE, buf, row + i, 94);

                        for (j = 1; j <= body->max; j++)
                        {
                            int c = 99 + j;
                            int r = row + i;
                            switch (body->slots[j].type)
                            {
                            case EQUIP_SLOT_GLOVES:
                                _prt_equippy(c, r, TV_GLOVES, SV_SET_OF_GAUNTLETS);
                                break;
                            case EQUIP_SLOT_WEAPON_SHIELD:
                                if (body->slots[j].hand % 2)
                                    _prt_equippy(c, r, TV_SHIELD, SV_LARGE_METAL_SHIELD);
                                else
                                    _prt_equippy(c, r, TV_SWORD, SV_LONG_SWORD);
                                break;
                            case EQUIP_SLOT_WEAPON:
                                _prt_equippy(c, r, TV_SWORD, SV_LONG_SWORD);
                                break;
                            case EQUIP_SLOT_RING:
                                _prt_equippy(c, r, TV_RING, 0);
                                break;
                            case EQUIP_SLOT_BOW:
                                _prt_equippy(c, r, TV_BOW, SV_LONG_BOW);
                                break;
                            case EQUIP_SLOT_AMULET:
                                _prt_equippy(c, r, TV_AMULET, 0);
                                break;
                            case EQUIP_SLOT_LIGHT:
                                _prt_equippy(c, r, TV_LIGHT, SV_LIGHT_FEANOR);
                                break;
                            case EQUIP_SLOT_BODY_ARMOR:
                                _prt_equippy(c, r, TV_HARD_ARMOR, SV_CHAIN_MAIL);
                                break;
                            case EQUIP_SLOT_CLOAK:
                                _prt_equippy(c, r, TV_CLOAK, SV_CLOAK);
                                break;
                            case EQUIP_SLOT_BOOTS:
                                _prt_equippy(c, r, TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS);
                                break;
                            case EQUIP_SLOT_HELMET:
                                _prt_equippy(c, r, TV_HELM, SV_IRON_HELM);
                                break;
                            case EQUIP_SLOT_ANY:
                                Term_putch(c, r, TERM_WHITE, '*');
                                break;
                            case EQUIP_SLOT_CAPTURE_BALL:
                                _prt_equippy(c, r, TV_CAPTURE, 0);
                                break;
                            }
                        }
                    }
                    else if (monster_mode == MONSTER_MODE_SKILLS)
                    {
                        sprintf(buf, "%2d+%-2d  %2d+%-2d  %2d+%-2d  %4d  %4d  %4d  %2d+%-2d  %2d+%-2d\n",
                            r_ptr->body.skills.dis, r_ptr->body.extra_skills.dis,
                            r_ptr->body.skills.dev, r_ptr->body.extra_skills.dev,
                            r_ptr->body.skills.sav, r_ptr->body.extra_skills.sav,
                            r_ptr->body.skills.stl,
                            r_ptr->body.skills.srh,
                            r_ptr->body.skills.fos,
                            r_ptr->body.skills.thn, r_ptr->body.extra_skills.thn,
                            r_ptr->body.skills.thb, r_ptr->body.extra_skills.thb
                        );
                        c_put_str(TERM_WHITE, buf, row + i, 64);
                    }
                    else if (monster_mode == MONSTER_MODE_EXTRA)
                    {
                        int speed = possessor_r_speed(r_ptr->id);
                        int ac = possessor_r_ac(r_ptr->id);

                        sprintf(buf, "%3d  %3d  %+5d  %+4d  %s",
                            r_ptr->alloc.lvl, possessor_max_plr_lvl(r_ptr->id), speed, ac,
                            get_class_aux(r_ptr->body.class_id, 0)->name
                        );
                        c_put_str(TERM_WHITE, buf, row + i, 64);
                    }
                }
            }
        }
    }

    /* Clear remaining lines */
    for (; i < per_page; i++)
    {
        Term_erase(col, row + i, 255);
    }
}

static void _display_races(vec_ptr v)
{
    doc_ptr doc = doc_alloc(80);
    int i;
    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr race = vec_get(v, i);
        doc_printf(doc, "<topic:%s><color:r>=====================================================================</color>\n", race->name);
        mon_display_doc(race, doc);
        doc_newline(doc);
    }
    doc_display(doc, "Monster Knowledge", 0);
    doc_free(doc);
}
static void _doc_skill(doc_ptr doc, int b, int x, int lvl)
{
    #if 0
    if (x)
    {
        char buf[10];
        sprintf(buf, "%2d%+2d", b, x);
        doc_printf(doc, " %5.5s", buf);
    }
    else
        doc_printf(doc, " %5d", b);
    #endif
    doc_printf(doc, " %3d", b + lvl*x/50);
}
static void _doc_equippy(doc_ptr doc, int tval, int sval)
{
    doc_insert_term_char(doc, _equippy_char(tval, sval));
}
void possessor_wizard(vec_ptr races)
{
    doc_ptr doc = doc_alloc(120); /* 23 + (6+8)*6 + 12 = 119 */
    int i,j;

    doc_insert(doc, "<style:table>");
    for (i = 0; i < vec_length(races); i++)
    {
        mon_race_ptr race = vec_get(races, i);
        equip_template_ptr body = NULL;

        if (i % 25 == 0)
        {
            if (i > 0) doc_newline(doc);
            doc_printf(doc, "<color:G>%-22.22s", "Name");
            for (j = 0; j < MAX_STATS; j++)
                doc_printf(doc, " %5.5s", stat_abbrev_true[j]);
            doc_insert(doc, " Dis Dev Sav Stl Srh Fos Thn Thb Body");
            doc_insert(doc, "</color>\n");
        }

        doc_insert_term_char(doc, mon_race_visual(race));
        doc_insert_char(doc, TERM_WHITE, ' ');

        if (race->body.flags & RF_POS_DISABLED)
        {
            doc_printf(doc, "<color:D>%-20.20s Disabled</color>\n", race->name);
            continue;
        }
        if (!(spoiler_hack || plr->wizard || (race->lore.flags & RFL_POSSESSOR)))
        {
            doc_printf(doc, "<color:D>%-20.20s Unknown</color>\n", race->name);
            continue;
        }

        doc_printf(doc, "<color:U>%-20.20s</color>", race->name);
        for (j = 0; j < MAX_STATS; j++)
        {
            if (j == race->body.spell_stat)
                doc_insert(doc, "<color:v>");
            if (race->body.extra_stats[j])
            {
                char buf[10];
                sprintf(buf, "%2d%+2d", race->body.stats[j], race->body.extra_stats[j]);
                doc_printf(doc, " %5.5s", buf);
            }
            else
                doc_printf(doc, " %5d", race->body.stats[j]);
            if (j == race->body.spell_stat)
                doc_insert(doc, "</color>");
        }
        _doc_skill(doc, race->body.skills.dis, race->body.extra_skills.dis, race->alloc.lvl);
        _doc_skill(doc, race->body.skills.dev, race->body.extra_skills.dev, race->alloc.lvl);
        _doc_skill(doc, race->body.skills.sav, race->body.extra_skills.sav, race->alloc.lvl);
        _doc_skill(doc, race->body.skills.stl, race->body.extra_skills.stl, race->alloc.lvl);
        _doc_skill(doc, race->body.skills.srh, race->body.extra_skills.srh, race->alloc.lvl);
        _doc_skill(doc, race->body.skills.fos, race->body.extra_skills.fos, race->alloc.lvl);
        _doc_skill(doc, race->body.skills.thn, race->body.extra_skills.thn, race->alloc.lvl);
        _doc_skill(doc, race->body.skills.thb, race->body.extra_skills.thb, race->alloc.lvl);

        body = equip_template_lookup(race->body.body_id);
        if (body)
        {
            doc_insert_char(doc, TERM_WHITE, ' ');
            for (j = 1; j <= body->max; j++)
            {
                switch (body->slots[j].type)
                {
                case EQUIP_SLOT_GLOVES:
                    _doc_equippy(doc, TV_GLOVES, SV_SET_OF_GAUNTLETS);
                    break;
                case EQUIP_SLOT_WEAPON_SHIELD:
                    if (body->slots[j].hand % 2)
                        _doc_equippy(doc, TV_SHIELD, SV_LARGE_METAL_SHIELD);
                    else
                        _doc_equippy(doc, TV_SWORD, SV_LONG_SWORD);
                    break;
                case EQUIP_SLOT_WEAPON:
                    _doc_equippy(doc, TV_SWORD, SV_LONG_SWORD);
                    break;
                case EQUIP_SLOT_RING:
                    _doc_equippy(doc, TV_RING, 0);
                    break;
                case EQUIP_SLOT_BOW:
                    _doc_equippy(doc, TV_BOW, SV_LONG_BOW);
                    break;
                case EQUIP_SLOT_AMULET:
                    _doc_equippy(doc, TV_AMULET, 0);
                    break;
                case EQUIP_SLOT_LIGHT:
                    _doc_equippy(doc, TV_LIGHT, SV_LIGHT_FEANOR);
                    break;
                case EQUIP_SLOT_BODY_ARMOR:
                    _doc_equippy(doc, TV_HARD_ARMOR, SV_CHAIN_MAIL);
                    break;
                case EQUIP_SLOT_CLOAK:
                    _doc_equippy(doc, TV_CLOAK, SV_CLOAK);
                    break;
                case EQUIP_SLOT_BOOTS:
                    _doc_equippy(doc, TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS);
                    break;
                case EQUIP_SLOT_HELMET:
                    _doc_equippy(doc, TV_HELM, SV_IRON_HELM);
                    break;
                case EQUIP_SLOT_ANY:
                    doc_insert_char(doc, TERM_WHITE, '*');
                    break;
                case EQUIP_SLOT_CAPTURE_BALL:
                    _doc_equippy(doc, TV_CAPTURE, 0);
                    break;
                }
            }
        }
        else doc_insert(doc, " <color:v>None</color>"); /* serious bug! */
        doc_newline(doc);
    }
    doc_insert(doc, "</style>");
    doc_display(doc, "Possessor Spoilers", 0);
    doc_free(doc);
}


/*
 * Display known monsters.
 */
static void do_cmd_knowledge_monsters(bool *need_redraw, bool visual_only)
{
    int i, len, max;
    int grp_cur, grp_top, old_grp_cur;
    int mon_cur, mon_top;
    int grp_cnt, grp_idx[100];
    vec_ptr races = NULL;
    int mon_cnt = 0;

    int column = 0;
    bool flag;
    bool redraw;

    bool visual_list = FALSE;
    byte attr_top = 0, char_left = 0;

    int browser_rows;
    int wid, hgt;

    byte mode;

    /* Get size */
    Term_get_size(&wid, &hgt);

    browser_rows = hgt - 8;

    max = 0;
    grp_cnt = 0;

    mode = visual_only ? 0x03 : 0x01;

    /* Check every group */
    for (i = 0; monster_group_text[i] != NULL; i++)
    {
        if (monster_group_char[i] == ((char *) -1L) && plr->prace != RACE_MON_POSSESSOR)
            continue;

        /* Measure the label */
        len = strlen(monster_group_text[i]);

        /* Save the maximum length */
        if (len > max) max = len;

        /* See if any monsters are known */
        /*if (monster_group_char[i] == ((char *) -2L))
        {
            grp_idx[grp_cnt++] = i;
        }
        else */
        {
            vec_ptr v = collect_monsters(i, mode);
            if (vec_length(v))
                grp_idx[grp_cnt++] = i;
            vec_free(v);
        }
    }
    if (grp_cnt == 0) return;

    /* Terminate the list */
    grp_idx[grp_cnt] = -1;

    old_grp_cur = -1;
    grp_cur = grp_top = 0;
    mon_cur = mon_top = 0;

    flag = FALSE;
    redraw = TRUE;

    mode = visual_only ? 0x02 : 0x00;

    while (!flag)
    {
        char ch;
        monster_race *r_ptr;
        term_char_t r_tc;

        if (redraw)
        {
            clear_from(0);

            prt(format("%s - Monsters", !visual_only ? "Knowledge" : "Visuals"), 2, 0);
            /*if (direct_r_idx < 0)*/ prt("Group", 4, 0);
            prt("Name", 4, max + 3);
            prt("Sym", 4, 52);
            if (!visual_only) prt("Kills", 4, 57);

            if (plr->wizard || plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
            {
                char buf[255];
                if (monster_mode == MONSTER_MODE_STATS)
                {
                    sprintf(buf, "STR  INT  WIS  DEX  CON  CHR  Life  Body");
                    c_put_str(TERM_WHITE, buf, 4, 64);
                    for (i = 62; i < 114; i++)
                        Term_putch(i, 5, TERM_WHITE, '=');
                }
                else if (monster_mode == MONSTER_MODE_SKILLS)
                {
                    sprintf(buf, "Dsrm   Dvce   Save   Stlh  Srch  Prcp  Melee  Bows");
                    c_put_str(TERM_WHITE, buf, 4, 64);
                    for (i = 62; i < 114; i++)
                        Term_putch(i, 5, TERM_WHITE, '=');
                }
                else if (monster_mode == MONSTER_MODE_EXTRA)
                {
                    sprintf(buf, "Lvl  Max  Speed    AC  Pseudo-Class");
                    c_put_str(TERM_WHITE, buf, 4, 64);
                    for (i = 62; i < 114; i++)
                        Term_putch(i, 5, TERM_WHITE, '=');
                }
            }

            for (i = 0; i < 62; i++)
            {
                Term_putch(i, 5, TERM_WHITE, '=');
            }

            /*if (direct_r_idx < 0)*/
            {
                for (i = 0; i < browser_rows; i++)
                {
                    Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
                }
            }

            redraw = FALSE;
        }

        /*if (direct_r_idx < 0)*/
        {
            /* Scroll group list */
            if (grp_cur < grp_top) grp_top = grp_cur;
            if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

            /* Display a list of monster groups */
            display_group_list(0, 6, max, browser_rows, grp_idx, monster_group_text, grp_cur, grp_top);

            if (old_grp_cur != grp_cur)
            {
                old_grp_cur = grp_cur;

                /* Get a list of monsters in the current group */
                if (races) vec_free(races);
                races = collect_monsters(grp_idx[grp_cur], mode);
                mon_cnt = vec_length(races);
                mon_cur = mon_top = 0;
            }
            else
            {
                /* Scroll monster list */
                while (mon_cur < mon_top)
                    mon_top = MAX(0, mon_top - browser_rows/2);
                while (mon_cur >= mon_top + browser_rows)
                    mon_top = MIN(mon_cnt - browser_rows, mon_top + browser_rows/2);
            }
        }

        if (!visual_list)
        {
            /* Display a list of monsters in the current group */
            display_monster_list(max + 3, 6, browser_rows, races, mon_cur, mon_top, visual_only);
        }
        else
        {
            mon_top = mon_cur;

            /* Display a monster name */
            display_monster_list(max + 3, 6, 1, races, mon_cur, mon_top, visual_only);

            /* Display visual list below first monster */
            display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
        }

        /* Prompt */
        if (plr->wizard || plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
        {
            prt(format("<dir>%s%s%s%s, ESC",
                (!visual_list && !visual_only) ? ", '?' to recall, '|' to dump" : "",
                visual_list ? ", ENTER to accept" : ", 'v' for visuals",
                (attr_idx || char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy",
                ", '=' for more info"),
                hgt - 1, 0);
        }
        else
        {
            prt(format("<dir>%s%s%s, ESC",
                (!visual_list && !visual_only) ? ", '?' to recall, '|' to dump" : "",
                visual_list ? ", ENTER to accept" : ", 'v' for visuals",
                (attr_idx || char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy"),
                hgt - 1, 0);
        }

        /* Get the current monster */
        r_ptr = vec_get(races, mon_cur);
        r_tc = mon_race_visual(r_ptr);

        if (!visual_only)
        {
            /* Mega Hack -- track this monster race */
            if (mon_cnt) monster_race_track(r_ptr);

            /* Hack -- handle stuff */
            handle_stuff();
        }

        if (visual_list)
        {
            place_visual_list_cursor(max + 3, 7, r_tc.a, r_tc.c, attr_top, char_left);
        }
        else if (!column)
        {
            Term_gotoxy(0, 6 + (grp_cur - grp_top));
        }
        else
        {
            Term_gotoxy(max + 3, 6 + (mon_cur - mon_top));
        }

        ch = inkey();

        /* Do visual mode command if needed */
        if (visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, &r_tc.a, &r_tc.c, need_redraw))
        {
            visual_set(sym_str(r_ptr->id), r_tc, 0);
            /*if (direct_r_idx >= 0)
            {
                switch (ch)
                {
                case '\n':
                case '\r':
                case ESCAPE:
                    flag = TRUE;
                    break;
                }
            }*/
            continue;
        }

        switch (ch)
        {
            case ESCAPE:
            {
                flag = TRUE;
                break;
            }

            case '|':
                _display_races(races);
                redraw = TRUE;
                break;

            case '>':
                possessor_wizard(races);
                redraw = TRUE;
                break;

            case 'R':
            case 'r':
            case '?':
            {
                /* Recall on screen */
                if (!visual_list && !visual_only)
                {
                    mon_display(r_ptr);
                    redraw = TRUE;
                }
                break;
            }

            case 'm':
            case 'n':
            case 'h':
            case '=':
                monster_mode++;
                if (monster_mode == MONSTER_MODE_MAX)
                    monster_mode = MONSTER_MODE_STATS;
                redraw = TRUE;
                break;

            default:
            {
                /* Move the cursor */
                browser_cursor(ch, &column, &grp_cur, grp_cnt, &mon_cur, mon_cnt);

                break;
            }
        }
    }

    if (races) vec_free(races);
}


/*
 * Display the objects in a group.
 */
static void display_object_list(int col, int row, int per_page, vec_ptr kinds,
    int object_cur, int object_top, bool visual_only)
{
    int i;

    /* Display lines until done */
    for (i = 0; i < per_page && object_top + i < vec_length(kinds); i++)
    {
        char o_name[80];
        char buf[255];
        byte a, c;
        object_kind *flavor_k_ptr;

        /* Access the object */
        object_kind *k_ptr = vec_get(kinds, object_top + i);

        /* Choose a color */
        byte attr = ((k_ptr->aware || visual_only) ? TERM_WHITE : TERM_SLATE);
        byte cursor = ((k_ptr->aware || visual_only) ? TERM_L_BLUE : TERM_BLUE);


        if (!visual_only && k_ptr->flavor)
        {
            /* Appearance of this object is shuffled */
            flavor_k_ptr = &k_info[k_ptr->flavor];
        }
        else
        {
            /* Appearance of this object is very normal */
            flavor_k_ptr = k_ptr;
        }



        attr = ((i + object_top == object_cur) ? cursor : attr);

        if (!k_ptr->flavor || (!visual_only && k_ptr->aware))
        {
            /* Tidy name */
            strip_name_aux(o_name, k_ptr->name);
        }
        else
        {
            /* Flavor name */
            strcpy(o_name, flavor_k_ptr->flavor_name);
        }

        /* Display the name */
        #if DEVELOPER
        sprintf(buf, "%-32.32s %2d %5d %6d %4d %4d", o_name, k_ptr->level, k_ptr->counts.found, k_ptr->counts.bought, k_ptr->counts.used, k_ptr->counts.destroyed);
        #else
        sprintf(buf, "%-35.35s %5d %6d %4d %4d", o_name, k_ptr->counts.found, k_ptr->counts.bought, k_ptr->counts.used, k_ptr->counts.destroyed);
        #endif
        c_prt(attr, buf, row + i, col);

        /* Hack -- visual_list mode */
        if (per_page == 1)
        {
            c_prt(attr, format("%02x/%02x", flavor_k_ptr->x_attr, flavor_k_ptr->x_char), row + i, (plr->wizard || visual_only) ? 64 : 68);
        }
        if (visual_only)
        {
            c_prt(attr, format("%d/%d", k_ptr->tval, k_ptr->sval), row + i, 70);
        }

        a = flavor_k_ptr->x_attr;
        c = flavor_k_ptr->x_char;

        /* Display symbol */
        Term_queue_bigchar(use_bigtile ? 76 : 77, row + i, a, c, 0, 0);
    }

    /* Total Line? */
    if (!visual_only && i < per_page && object_top + i == vec_length(kinds))
    {
        char     buf[255];
        counts_t totals = {0};
        int      j;

        for (j = 0; j < vec_length(kinds); j++)
        {
            object_kind   *k_ptr = vec_get(kinds, j);

            totals.found += k_ptr->counts.found;
            totals.bought += k_ptr->counts.bought;
            totals.used += k_ptr->counts.used;
            totals.destroyed += k_ptr->counts.destroyed;
        }

        sprintf(buf, "%-35.35s %5d %6d %4d %4d",
            "Totals",
            totals.found, totals.bought, totals.used, totals.destroyed
        );
        c_prt(TERM_YELLOW, buf, row + i, col);
        i++;
    }

    /* Clear remaining lines */
    for (; i < per_page; i++)
    {
        Term_erase(col, row + i, 255);
    }
}

/*
 * Describe fake object
 */
static void desc_obj_fake(int k_idx)
{
    object_type *o_ptr;
    object_type object_type_body;

    /* Get local object */
    o_ptr = &object_type_body;

    /* Wipe the object */
    object_wipe(o_ptr);

    /* Create the artifact */
    object_prep(o_ptr, k_idx);

    /* It's fully know */
    o_ptr->ident |= IDENT_KNOWN;

    /* Track the object */
    /* object_actual_track(o_ptr); */

    /* Hack - mark as fake */
    /* term_obj_real = FALSE; */

    /* Hack -- Handle stuff */
    handle_stuff();

    obj_display(o_ptr);
}

static void desc_ego_fake(int e_idx)
{
    ego_type *e_ptr = &e_info[e_idx];
    ego_display(e_ptr);
}


typedef struct {
    u32b id;
    cptr name;
} _ego_type_t;

static _ego_type_t _ego_types[] = {
    { EGO_TYPE_WEAPON, "Weapons" },
    { EGO_TYPE_DIGGER, "Diggers" },

    { EGO_TYPE_SHIELD, "Shields" },
    { EGO_TYPE_BODY_ARMOR, "Body Armor" },
    { EGO_TYPE_ROBE, "Robes" },
    { EGO_TYPE_DRAGON_ARMOR, "Dragon Armor" },
    { EGO_TYPE_CLOAK, "Cloaks" },
    { EGO_TYPE_HELMET, "Helmets" },
    { EGO_TYPE_CROWN, "Crowns" },
    { EGO_TYPE_GLOVES, "Gloves" },
    { EGO_TYPE_BOOTS, "Boots" },

    { EGO_TYPE_BOW, "Bows" },
    { EGO_TYPE_AMMO, "Ammo" },
    { EGO_TYPE_HARP, "Harps" },

    { EGO_TYPE_RING, "Rings" },
    { EGO_TYPE_AMULET, "Amulets" },
    { EGO_TYPE_LIGHT, "Lights" },
    { EGO_TYPE_DEVICE, "Devices" },

    { EGO_TYPE_NONE, NULL },
};

static int _compare_e_level(ego_ptr left, ego_ptr right)
{
    if (left->level < right->level) return -1;
    if (left->level > right->level) return 1;
    return 0;
}

static int _collect_egos(int grp_cur, vec_ptr egos)
{
    int i;
    int type = _ego_types[grp_cur].id;

    vec_clear(egos);
    for (i = 0; i < max_e_idx; i++)
    {
        ego_type *e_ptr = &e_info[i];

        if (!e_ptr->name) continue;
        /*if (!e_ptr->aware) continue;*/
        if (!ego_has_lore(e_ptr) && !e_ptr->counts.found && !e_ptr->counts.bought) continue;
        if (!(e_ptr->type & type)) continue;

        vec_add(egos, e_ptr);
    }
    vec_sort(egos, (vec_cmp_f)_compare_e_level);
    return vec_length(egos);
}

static void do_cmd_knowledge_egos(void)
{
    int i, len, max;
    int grp_cur, grp_top, old_grp_cur;
    int ego_cur, ego_top;
    int grp_cnt, grp_idx[100];
    int ego_cnt;
    vec_ptr egos = vec_alloc(NULL);

    int column = 0;
    bool flag;
    bool redraw;

    int browser_rows;
    int wid, hgt;

    /* Get size */
    Term_get_size(&wid, &hgt);

    browser_rows = hgt - 8;

    max = 0;
    grp_cnt = 0;
    for (i = 0; _ego_types[i].id != EGO_TYPE_NONE; i++)
    {
        len = strlen(_ego_types[i].name);
        if (len > max)
            max = len;

        if (_collect_egos(i, egos))
            grp_idx[grp_cnt++] = i;
    }
    grp_idx[grp_cnt] = -1;

    if (!grp_cnt)
    {
        prt("You haven't found any egos just yet. Press any key to continue.", 0, 0);
        inkey();
        prt("", 0, 0);
        vec_free(egos);
        return;
    }

    ego_cnt = 0;

    old_grp_cur = -1;
    grp_cur = grp_top = 0;
    ego_cur = ego_top = 0;

    flag = FALSE;
    redraw = TRUE;

    while (!flag)
    {
        char ch;
        if (redraw)
        {
            clear_from(0);

            prt(format("%s - Egos", "Knowledge"), 2, 0);
            prt("Group", 4, 0);
            prt("Name", 4, max + 3);
            prt("Found Bought Dest", 4, max + 3 + 36);

            for (i = 0; i < 72; i++)
            {
                Term_putch(i, 5, TERM_WHITE, '=');
            }

            for (i = 0; i < browser_rows; i++)
            {
                Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
            }

            redraw = FALSE;
        }

        /* Scroll group list */
        if (grp_cur < grp_top) grp_top = grp_cur;
        if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

        /* Display a list of object groups */
        for (i = 0; i < browser_rows && grp_idx[i] >= 0; i++)
        {
            int  grp = grp_idx[grp_top + i];
            byte attr = (grp_top + i == grp_cur) ? TERM_L_BLUE : TERM_WHITE;

            Term_erase(0, 6 + i, max);
            c_put_str(attr, _ego_types[grp].name, 6 + i, 0);
        }

        if (old_grp_cur != grp_cur)
        {
            old_grp_cur = grp_cur;

            /* Get a list of objects in the current group */
            ego_cnt = _collect_egos(grp_idx[grp_cur], egos) + 1;
        }

        /* Scroll object list */
        while (ego_cur < ego_top)
            ego_top = MAX(0, ego_top - browser_rows/2);
        while (ego_cur >= ego_top + browser_rows)
            ego_top = MIN(ego_cnt - browser_rows, ego_top + browser_rows/2);

        /* Display a list of objects in the current group */
        /* Display lines until done */
        for (i = 0; i < browser_rows && ego_top + i < vec_length(egos); i++)
        {
            char           buf[255];
            char           name[255];
            ego_type      *e_ptr = vec_get(egos, ego_top + i);
            byte           attr = TERM_WHITE;

            if (i + ego_top == ego_cur)
                attr = TERM_L_BLUE;

            strip_name_aux(name, e_ptr->name);
            if (e_ptr->type & (~_ego_types[grp_idx[grp_cur]].id))
                strcat(name, " [Shared]");

            sprintf(buf, "%-35.35s %5d %6d %4d",
                name,
                e_ptr->counts.found, e_ptr->counts.bought, e_ptr->counts.destroyed
            );
            c_prt(attr, buf, 6 + i, max + 3);
        }
        /* Total Line? */
        if (i < browser_rows && ego_top + i == vec_length(egos))
        {
            char     buf[255];
            counts_t totals = {0};
            int j;
            for (j = 0; j < vec_length(egos); j++)
            {
                ego_type *e_ptr = vec_get(egos, j);
                totals.found += e_ptr->counts.found;
                totals.bought += e_ptr->counts.bought;
                totals.destroyed += e_ptr->counts.destroyed;
            }

            sprintf(buf, "%35.35s %5d %6d %4d",
                "Totals",
                totals.found, totals.bought, totals.destroyed
            );
            c_prt(TERM_YELLOW, buf, 6 + i, max + 3);
            i++;
        }


        /* Clear remaining lines */
        for (; i < browser_rows; i++)
        {
            Term_erase(max + 3, 6 + i, 255);
        }

        prt("<dir>, 'r' to recall, ESC", hgt - 1, 0);

        if (!column)
        {
            Term_gotoxy(0, 6 + (grp_cur - grp_top));
        }
        else
        {
            Term_gotoxy(max + 3, 6 + (ego_cur - ego_top));
        }

        ch = inkey();

        switch (ch)
        {
        case ESCAPE:
            flag = TRUE;
            break;

        case 'R':
        case 'r':
        case 'I':
        case 'i':
            if (grp_cnt > 0 && 0 <= ego_cur && ego_cur < vec_length(egos))
            {
                ego_ptr ego = vec_get(egos, ego_cur);
                desc_ego_fake(ego->id);
                redraw = TRUE;
            }
            break;

        default:
            browser_cursor(ch, &column, &grp_cur, grp_cnt, &ego_cur, ego_cnt);
        }
    }

    vec_free(egos);
}


/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(bool *need_redraw, bool visual_only, int direct_k_idx)
{
    int i, len, max;
    int grp_cur, grp_top, old_grp_cur;
    u32b object_old;
    int object_cur, object_top;
    int grp_cnt, grp_idx[100];
    int object_cnt;
    vec_ptr kinds = vec_alloc(NULL);

    int column = 0;
    bool flag;
    bool redraw;

    bool visual_list = FALSE;
    byte attr_top = 0, char_left = 0;

    int browser_rows;
    int wid, hgt;

    byte mode;

    /* Get size */
    Term_get_size(&wid, &hgt);

    browser_rows = hgt - 8;

    max = 0;
    grp_cnt = 0;

    if (direct_k_idx < 0)
    {
        mode = visual_only ? 0x03 : 0x01;

        /* Check every group */
        for (i = 0; object_group_text[i] != NULL; i++)
        {
            /* Measure the label */
            len = strlen(object_group_text[i]);

            /* Save the maximum length */
            if (len > max) max = len;

            /* See if any monsters are known */
            if (collect_objects(i, kinds, mode))
            {
                /* Build a list of groups with known object kinds */
                grp_idx[grp_cnt++] = i;
            }
        }

        object_old = -1;
        object_cnt = 0;
    }
    else
    {
        object_kind *k_ptr = &k_info[direct_k_idx];
        object_kind *flavor_k_ptr;

        if (!visual_only && k_ptr->flavor)
        {
            /* Appearance of this object is shuffled */
            flavor_k_ptr = &k_info[k_ptr->flavor];
        }
        else
        {
            /* Appearance of this object is very normal */
            flavor_k_ptr = k_ptr;
        }

        vec_clear(kinds);
        vec_add(kinds, k_ptr);
        object_old = direct_k_idx;
        object_cnt = 1;

        (void)visual_mode_command('v', &visual_list, browser_rows - 1, wid - (max + 3),
            &attr_top, &char_left, &flavor_k_ptr->x_attr, &flavor_k_ptr->x_char, need_redraw);
    }

    /* Terminate the list */
    grp_idx[grp_cnt] = -1;

    old_grp_cur = -1;
    grp_cur = grp_top = 0;
    object_cur = object_top = 0;

    flag = FALSE;
    redraw = TRUE;

    mode = visual_only ? 0x02 : 0x00;

    while (!flag)
    {
        char ch;
        object_kind *k_ptr = NULL, *flavor_k_ptr = NULL;

        if (redraw)
        {
            clear_from(0);

            prt(format("%s - Objects", !visual_only ? "Knowledge" : "Visuals"), 2, 0);
            if (direct_k_idx < 0) prt("Group", 4, 0);
            prt("Name", 4, max + 3);
            if (visual_only) prt("Idx", 4, 70);
            prt("Found Bought Used Dest Sym", 4, 52);

            for (i = 0; i < 78; i++)
            {
                Term_putch(i, 5, TERM_WHITE, '=');
            }

            if (direct_k_idx < 0)
            {
                for (i = 0; i < browser_rows; i++)
                {
                    Term_putch(max + 1, 6 + i, TERM_WHITE, '|');
                }
            }

            redraw = FALSE;
        }

        if (direct_k_idx < 0)
        {
            /* Scroll group list */
            if (grp_cur < grp_top) grp_top = grp_cur;
            if (grp_cur >= grp_top + browser_rows) grp_top = grp_cur - browser_rows + 1;

            /* Display a list of object groups */
            display_group_list(0, 6, max, browser_rows, grp_idx, object_group_text, grp_cur, grp_top);

            if (old_grp_cur != grp_cur)
            {
                old_grp_cur = grp_cur;

                /* Get a list of objects in the current group */
                object_cnt = collect_objects(grp_idx[grp_cur], kinds, mode) + 1;
            }

            /* Scroll object list */
            while (object_cur < object_top)
                object_top = MAX(0, object_top - browser_rows/2);
            while (object_cur >= object_top + browser_rows)
                object_top = MIN(object_cnt - browser_rows, object_top + browser_rows/2);
        }

        if (!visual_list)
        {
            /* Display a list of objects in the current group */
            display_object_list(max + 3, 6, browser_rows, kinds, object_cur, object_top, visual_only);
        }
        else
        {
            object_top = object_cur;

            /* Display a list of objects in the current group */
            display_object_list(max + 3, 6, 1, kinds, object_cur, object_top, visual_only);

            /* Display visual list below first object */
            display_visual_list(max + 3, 7, browser_rows-1, wid - (max + 3), attr_top, char_left);
        }

        /* Get the current object */
        if (0 <= object_cur  && object_cur < vec_length(kinds))
        {
            k_ptr = vec_get(kinds, object_cur);

            if (!visual_only && k_ptr->flavor)
            {
                /* Appearance of this object is shuffled */
                flavor_k_ptr = &k_info[k_ptr->flavor];
            }
            else
            {
                /* Appearance of this object is very normal */
                flavor_k_ptr = k_ptr;
            }
        }
        else
        {
            k_ptr = NULL;
            flavor_k_ptr = NULL;
        }

        /* Prompt */
        prt(format("<dir>%s%s%s, ESC",
            (!visual_list && !visual_only) ? ", 'r' to recall" : "",
            visual_list ? ", ENTER to accept" : ", 'v' for visuals",
            (attr_idx || char_idx) ? ", 'c', 'p' to paste" : ", 'c' to copy"),
            hgt - 1, 0);

        if (!visual_only && k_ptr)
        {
            /* Mega Hack -- track this object */
            if (object_cnt)
                object_kind_track(k_ptr->idx);

            /* The "current" object changed */
            if (object_old != k_ptr->idx)
            {
                /* Hack -- handle stuff */
                handle_stuff();

                /* Remember the "current" object */
                object_old = k_ptr->idx;
            }
        }

        if (visual_list && flavor_k_ptr)
        {
            place_visual_list_cursor(max + 3, 7, flavor_k_ptr->x_attr, flavor_k_ptr->x_char, attr_top, char_left);
        }
        else if (!column)
        {
            Term_gotoxy(0, 6 + (grp_cur - grp_top));
        }
        else
        {
            Term_gotoxy(max + 3, 6 + (object_cur - object_top));
        }

        ch = inkey();

        /* Do visual mode command if needed */
        if (flavor_k_ptr && visual_mode_command(ch, &visual_list, browser_rows-1, wid - (max + 3), &attr_top, &char_left, &flavor_k_ptr->x_attr, &flavor_k_ptr->x_char, need_redraw))
        {
            if (direct_k_idx >= 0)
            {
                switch (ch)
                {
                case '\n':
                case '\r':
                case ESCAPE:
                    flag = TRUE;
                    break;
                }
            }
            continue;
        }

        switch (ch)
        {
            case ESCAPE:
            {
                flag = TRUE;
                break;
            }

            case 'R':
            case 'r':
            {
                /* Recall on screen */
                if (!visual_list && !visual_only && (grp_cnt > 0) && k_ptr)
                {
                    desc_obj_fake(k_ptr->idx);
                    redraw = TRUE;
                }
                break;
            }

            default:
            {
                /* Move the cursor */
                browser_cursor(ch, &column, &grp_cur, grp_cnt, &object_cur, object_cnt);
                break;
            }
        }
    }
    vec_free(kinds);
}


/*
 * List virtues & status
 */

static void do_cmd_knowledge_virtues(void)
{
    doc_ptr doc = doc_alloc(80);

    virtue_display(doc, FALSE);
    doc_display(doc, "Virtues", 0);
    doc_free(doc);
}

/*
* Dungeon
*
*/
static void do_cmd_knowledge_dungeon(void)
{
    doc_ptr doc = doc_alloc(80);

    plr_display_dungeons(doc);
    doc_display(doc, "Dungeons", 0);
    doc_free(doc);
}

static void do_cmd_knowledge_stat(void)
{
    doc_ptr          doc = doc_alloc(80);
    race_t          *race_ptr = get_race();
    class_t         *class_ptr = get_class();
    personality_ptr  pers_ptr = get_personality();
    dun_world_ptr    world = dun_worlds_current();
    int              i;

    if (plr->knowledge & KNOW_HPRATE)
        doc_printf(doc, "Your current Life Rating is <color:G>%d%%</color>.\n\n", life_rating());
    else
        doc_insert(doc, "Your current Life Rating is <color:y>\?\?\?%</color>.\n\n");

    doc_insert(doc, "<color:r>Limits of maximum stats</color>\n");

    for (i = 0; i < MAX_STATS; i++)
    {
        if ((plr->knowledge & KNOW_STAT) || plr->stat_max[i] == plr->stat_max_max[i])
            doc_printf(doc, "%s <color:G>18/%d</color>\n", stat_names[i], plr->stat_max_max[i]-18);
        else
            doc_printf(doc, "%s <color:y>\?\?\?</color>\n", stat_names[i]);
    }
    doc_insert(doc, "\n\n");

    doc_printf(doc, "<color:r>World:</color> <color:B>%s</color>\n", world->name);
    if (world->plr_flags & WFP_COMPLETED)
    {
        dun_type_ptr type = dun_types_lookup(world->final_dungeon);
        if (world->next_world_id)
        {
            doc_printf(doc, "You have completed this world. A magical portal "
                            "awaits you on level %d of <color:U>%s</color> when you are "
                            "ready to continue your quest.\n\n", type->max_dun_lvl, type->name);
        }
        else
        {
            doc_insert(doc, "You have won the game! You may retire (commit suicide) when ready.\n\n");
        }
    }
    else
        doc_printf(doc, "%s\n\n", world->desc);

    doc_printf(doc, "<color:r>Race:</color> <color:B>%s</color>\n", race_ptr->name);
    doc_insert(doc, race_ptr->desc);
    if (plr->pclass == CLASS_MONSTER)
    {
        doc_printf(doc, " For more information, see <link:MonsterRaces.txt#%s>.", race_ptr->name);
        if (plr->prace == RACE_MON_RING)
            doc_insert(doc, " For information about rings, see <link:rings.txt>.");
        doc_insert(doc, "\n\n");
    }
    else
        doc_printf(doc, " For more information, see <link:Races.txt#%s>.\n\n", race_ptr->name);

    if (race_ptr->subdesc && strlen(race_ptr->subdesc))
    {
        doc_printf(doc, "<color:r>Subace:</color> <color:B>%s</color>\n", race_ptr->subname);
        doc_insert(doc, race_ptr->subdesc);
        doc_insert(doc, "\n\n");
    }

    if (plr->pclass != CLASS_MONSTER)
    {
        doc_printf(doc, "<color:r>Class:</color> <color:B>%s</color>\n", class_ptr->name);
        doc_insert(doc, class_ptr->desc);
        doc_printf(doc, " For more information, see <link:Classes.txt#%s>.\n\n", class_ptr->name);
    }

    doc_printf(doc, "<color:r>Personality:</color> <color:B>%s</color>\n", pers_ptr->name);
    doc_insert(doc, pers_ptr->desc);
    doc_printf(doc, " For more information, see <link:Personalities.txt#%s>.\n\n", pers_ptr->name);

    if (plr->realm1)
    {
        doc_printf(doc, "<color:r>Realm:</color> <color:B>%s</color>\n", realm_names[plr->realm1]);
        doc_insert(doc, realm_jouhou[technic2magic(plr->realm1)-1]);
        doc_insert(doc, "\n\n");
    }

    if (plr->realm2)
    {
        doc_printf(doc, "<color:r>Realm:</color> <color:B>%s</color>\n", realm_names[plr->realm2]);
        doc_insert(doc, realm_jouhou[technic2magic(plr->realm2)-1]);
        doc_insert(doc, "\n\n");
    }

    doc_display(doc, "Self Knowledge", 0);
    doc_free(doc);
}

/*
 * Check the status of "autopick"
 */
static void do_cmd_knowledge_autopick(void)
{
    int k;
    doc_ptr doc = doc_alloc(80);

    if (!max_autopick)
    {
        doc_insert(doc, "There are no preferences for automatic pickup/destruction.");
    }
    else
    {
        doc_printf(doc, "There are %d registered lines for automatic pickup/destruction.\n", max_autopick);
    }
    doc_insert(doc, "For help on the auto-picker, see <link:editor.txt>\n\n");

    for (k = 0; k < max_autopick; k++)
    {
        cptr tmp;
        str_ptr line = 0;
        char color = 'w';
        byte act = autopick_list[k].action;
        if (act & DONT_AUTOPICK)
        {
            tmp = "Leave";
            color = 'U';
        }
        else if (act & DO_AUTODESTROY)
        {
            tmp = "Destroy";
            color = 'r';
        }
        else if (act & DO_AUTOPICK)
        {
            tmp = "Pickup";
            color = 'B';
        }
        else /* if (act & DO_QUERY_AUTOPICK) */ /* Obvious */
        {
            tmp = "Query";
            color = 'y';
        }

        if (act & DO_DISPLAY)
            doc_printf(doc, "<color:%c>%-9.9s</color>", color, format("[%s]", tmp));
        else
            doc_printf(doc, "<color:%c>%-9.9s</color>", color, format("(%s)", tmp));

        line = autopick_line_from_entry(&autopick_list[k], AUTOPICK_COLOR_CODED);
        doc_printf(doc, " <indent><style:indent>%s</style></indent>\n", str_buffer(line));
        str_free(line);
    }

    doc_display(doc, "Automatic Pickup and Destroy Preferences", 0);
    doc_free(doc);
}


/*
 * Interact with "knowledge"
 */
void do_cmd_knowledge(void)
{
    int      i, row, col;
    bool     need_redraw = FALSE;
    class_t *class_ptr = get_class();
    race_t  *race_ptr = get_race();

    screen_save();

    while (1)
    {
        Term_clear();

        prt("Display current knowledge", 2, 0);

        /* Give some choices */
        row = 4;
        col = 2;
        c_prt(TERM_RED, "Object Knowledge", row++, col - 2);
        prt("(a) Artifacts", row++, col);
        prt("(o) Objects", row++, col);
        prt("(e) Egos", row++, col);
        prt("(_) Auto Pick/Destroy", row++, col);
        row++;

        c_prt(TERM_RED, "Monster Knowledge", row++, col - 2);
        prt("(m) Known Monsters", row++, col);
        prt("(w) Wanted Monsters", row++, col);
        prt("(u) Remaining Uniques", row++, col);
        prt("(p) Pets", row++, col);
        row++;

        c_prt(TERM_RED, "Dungeon Knowledge", row++, col - 2);
        prt("(d) Dungeons", row++, col);
        prt("(q) Quests", row++, col);
        row++;

        row = 4;
        col = 30;

        c_prt(TERM_RED, "Self Knowledge", row++, col - 2);
        prt("(@) About Yourself", row++, col);
        if (plr->prace != RACE_MON_RING)
            prt("(W) Weapon Damage", row++, col);
        if (equip_find_obj(TV_BOW, SV_ANY) && !prace_is_(RACE_MON_JELLY) && plr->shooter_info.tval_ammo)
            prt("(S) Shooter Damage", row++, col);
        if (mut_count(NULL))
            prt("(M) Mutations", row++, col);
        prt("(v) Virtues", row++, col);
        if (class_ptr->hooks.character_dump || race_ptr->hooks.character_dump)
            prt("(x) Extra info", row++, col);
        prt("(H) High Score List", row++, col);
        prt("(n) Change Player Name", row++, col);
        row++;

        c_prt(TERM_RED, "Skills", row++, col - 2);
        prt("(P) Proficiency", row++, col);
        if (plr->pclass != CLASS_RAGE_MAGE) /* TODO */
            prt("(s) Spell Proficiency", row++, col);
        row++;

        /* Prompt */
        prt("ESC) Exit menu", 23, 1);
        prt("Command: ", 22, 0);

        /* Prompt */
        i = inkey();

        /* Done */
        if (i == ESCAPE) break;
        switch (i)
        {
        /* Object Knowledge */
        case 'a':
            do_cmd_knowledge_artifacts();
            break;
        case 'o':
            do_cmd_knowledge_objects(&need_redraw, FALSE, -1);
            break;
        case 'e':
            do_cmd_knowledge_egos();
            break;
        case '_':
            do_cmd_knowledge_autopick();
            break;

        /* Monster Knowledge */
        case 'm':
            do_cmd_knowledge_monsters(&need_redraw, FALSE);
            break;
        case 'w':
            display_wanted_uniques();
            break;
        case 'u':
            do_cmd_knowledge_uniques();
            break;
        case 'p':
            do_cmd_knowledge_pets();
            break;

        /* Dungeon Knowledge */
        case 'd':
            do_cmd_knowledge_dungeon();
            break;
        case 'q':
            quests_display();
            break;

        /* Self Knowledge */
        case '@':
            do_cmd_knowledge_stat();
            break;
        case 'W':
            if (plr->prace != RACE_MON_RING)
                do_cmd_knowledge_weapon();
            else
                bell();
            break;
        case 'S':
            if (equip_find_obj(TV_BOW, SV_ANY) && !prace_is_(RACE_MON_JELLY) && plr->shooter_info.tval_ammo)
                do_cmd_knowledge_shooter();
            else
                bell();
            break;
        case 'M':
            if (mut_count(NULL))
                mut_do_cmd_knowledge();
            else
                bell();
            break;
        case 'v':
            do_cmd_knowledge_virtues();
            break;
        case 'x':
            if (class_ptr->hooks.character_dump || race_ptr->hooks.character_dump)
                do_cmd_knowledge_extra();
            else
                bell();
            break;
        case 'H': {
            vec_ptr scores;
            if (check_score())
                scores_update();
            scores = scores_load(NULL);
            scores_display(scores);
            vec_free(scores);
            break; }

        case 'n':
            if (plr_get_name())
                process_player_name(FALSE);
            break;

        /* Skills */
        case 'P':
            do_cmd_knowledge_weapon_exp();
            break;
        case 's':
            if (plr->pclass != CLASS_RAGE_MAGE)  /* TODO */
                do_cmd_knowledge_spell_exp();
            break;

        default:
            bell();
        }

        /* Flush messages */
        msg_print(NULL);
    }

    /* Restore the screen */
    screen_load();

    if (need_redraw) do_cmd_redraw();
}

/*
 * Display the time and date
 */
void do_cmd_time(void)
{
    int day, hour, min, full, start, end, num;
    char desc[1024];

    char buf[1024];
    char day_buf[10];

    FILE *fff;

    extract_day_hour_min(&day, &hour, &min);

    full = hour * 100 + min;

    start = 9999;
    end = -9999;

    num = 0;

    strcpy(desc, "It is a strange time.");


    if (day < MAX_DAYS) sprintf(day_buf, "%d", day);
    else strcpy(day_buf, "*****");

    /* Message */
    msg_format("This is day %s. The time is %d:%02d %s.",
           day_buf, (hour % 12 == 0) ? 12 : (hour % 12),
           min, (hour < 12) ? "AM" : "PM");


    /* Find the path */
    if (!randint0(10) || plr_tim_find(T_HALLUCINATE))
    {
        path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "timefun.txt");

    }
    else
    {
        path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "timenorm.txt");

    }

    /* Open this file */
    fff = my_fopen(buf, "rt");

    /* Oops */
    if (!fff) return;

    /* Find this time */
    while (!my_fgets(fff, buf, sizeof(buf)))
    {
        /* Ignore comments */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Ignore invalid lines */
        if (buf[1] != ':') continue;

        /* Process 'Start' */
        if (buf[0] == 'S')
        {
            /* Extract the starting time */
            start = atoi(buf + 2);

            /* Assume valid for an hour */
            end = start + 59;

            /* Next... */
            continue;
        }

        /* Process 'End' */
        if (buf[0] == 'E')
        {
            /* Extract the ending time */
            end = atoi(buf + 2);

            /* Next... */
            continue;
        }

        /* Ignore incorrect range */
        if ((start > full) || (full > end)) continue;

        /* Process 'Description' */
        if (buf[0] == 'D')
        {
            num++;

            /* Apply the randomizer */
            if (!randint0(num)) strcpy(desc, buf + 2);

            /* Next... */
            continue;
        }
    }

    /* Message */
    msg_print(desc);

    /* Close the file */
    my_fclose(fff);
}
