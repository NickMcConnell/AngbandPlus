/*
 * File: c-files.c
 * Purpose: Various file-related activities, poorly organised
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
#include "../common/buildid.h"
#include "c-cmds.h"
#include "netclient.h"


/*
 * Find the default paths to all of our important sub-directories.
 *
 * All of the sub-directories should, by default, be located inside
 * the main directory, whose location is very system dependant. (On multi-
 * user systems such as Linux this is not the default - see config.h for
 * more info.)
 *
 * This function takes a writable buffer, initially containing the
 * "path" to the "config", "lib" and "data" directories, for example,
 * "/etc/angband/", "/usr/share/angband" and "/var/games/angband" -
 * or a system dependant string, for example, ":lib:".  The buffer
 * must be large enough to contain at least 32 more characters.
 *
 * Various command line options may allow some of the important
 * directories to be changed to user-specified directories, most
 * importantly, the "apex" and "user" and "save" directories,
 * but this is done after this function, see "main.c".
 *
 * In general, the initial path should end in the appropriate "PATH_SEP"
 * string.  All of the "sub-directory" paths (created below or supplied
 * by the user) will NOT end in the "PATH_SEP" string, see the special
 * "path_build()" function in "util.c" for more information.
 *
 * Hack -- first we free all the strings, since this is known
 * to succeed even if the strings have not been allocated yet,
 * as long as the variables start out as "NULL".  This allows
 * this function to be called multiple times, for example, to
 * try several base "path" values until a good one is found.
 */
void init_file_paths(const char *configpath, const char *libpath, const char *datapath)
{
    /*** Free everything ***/

    free_file_paths();

    /*** Prepare the "path" ***/

    /* Build path names */
    ANGBAND_DIR_FILE = string_make(format("%sfile", libpath));
    ANGBAND_DIR_USER = string_make(format("%suser", datapath));
    ANGBAND_DIR_PREF = string_make(format("%spref", configpath));
    ANGBAND_DIR_XTRA = string_make(format("%sxtra", libpath));
}


void init_extra_paths()
{
    char path[MSG_LEN];

    /* Font */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "font");
    ANGBAND_DIR_XTRA_FONT = string_make(path);

    /* Graf */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "graf");
    ANGBAND_DIR_XTRA_GRAF = string_make(path);

#ifdef USE_SOUND
    /* Sound */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "sound");
    ANGBAND_DIR_XTRA_SOUND = string_make(path);
#endif
}


void free_file_paths()
{
    /* Free the sub-paths */
    string_free(ANGBAND_DIR_FILE);
    string_free(ANGBAND_DIR_USER);
    string_free(ANGBAND_DIR_PREF);
    string_free(ANGBAND_DIR_XTRA);

    string_free(ANGBAND_DIR_XTRA_FONT);
    string_free(ANGBAND_DIR_XTRA_GRAF);
    string_free(ANGBAND_DIR_XTRA_ICON);
    string_free(ANGBAND_DIR_XTRA_SOUND);
}


/*
 * Show the splash screen.
 */
void show_splashscreen(void)
{
    int i;
    ui_event ch;

    /* Clear the screen */
    Term_clear();

    for (i = 0; i < TEXTFILE__HGT; i++)
    {
        /* Show each line */
        text_out_e(&Setup.text_screen[TEXTFILE_MOTD][i * TEXTFILE__WID], i);
    }

    /* Show it */
    Term_fresh();

    /* Wait for a keypress */
    Term_inkey(&ch, TRUE, TRUE);

    /* Clear the screen again */
    Term_clear();
}


/*
 * Peruse a file sent by the server.
 */
bool peruse_file(void)
{
    ui_event ke;
    bool more = TRUE;
    int max_hgt = Client_setup.settings[SETTING_MAX_HGT];

    /* Initialize */
    cur_line = 0;
    max_line = 0;

    /* Save the screen */
    screen_save();

    /* The top line is "icky" */
    topline_icky = TRUE;

    /* Show the stuff */
    while (more)
    {
        /* Send the command */
        Send_special_line(special_line_type, cur_line);

        /* Get a keypress */
        ke = inkey_ex();

        /* Exit on abort */
        if (is_abort(ke)) break;

        /* Hack -- make any key escape if we're in popup mode */
        if ((max_line < max_hgt - 4) && (special_line_type == SPECIAL_FILE_OTHER))
        {
            ke.type = EVT_KBRD;
            ke.key.code = ESCAPE;
            ke.key.mods = 0;
        }

        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                /* Go to a specific line */
                case '#':
                {
                    char tmp[NORMAL_WID];
                    int res;

                    prt("Goto Line: ", max_hgt - 1, 0);
                    my_strcpy(tmp, "0", sizeof(tmp));
                    res = askfor_ex(tmp, sizeof(tmp), NULL, FALSE);
                    if (res == 1) more = FALSE;
                    else if (!res) cur_line = atoi(tmp);

                    break;
                }

                /* Up a line */
                case ARROW_UP:
                case '8':
                {
                    cur_line--;
                    if (cur_line < 0) cur_line = 0;
                    break;
                }

                /* Up a page */
                case KC_PGUP:
                case '9':
                case '-':
                {
                    cur_line -= 20;
                    if (cur_line < 0) cur_line = 0;
                    break;
                }

                /* Home */
                case KC_HOME:
                case '7':
                {
                    cur_line = 0;
                    break;
                }

                /* Down a line */
                case ARROW_DOWN:
                case '2':
                case KC_ENTER:
                {
                    cur_line++;
                    break;
                }

                /* Down a page */
                case KC_PGDOWN:
                case '3':
                case ' ':
                {
                    cur_line += 20;
                    break;
                }

                /* End */
                case KC_END:
                case '1':
                {
                    if (max_line)
                    {
                        cur_line = max_line - 20;
                        if (cur_line < 0) cur_line = 0;
                    }
                    break;
                }
            }
        }

        /* Exit on escape */
        if (is_escape(ke)) break;

        /* Check maximum line */
        if ((cur_line > max_line) || (cur_line < 0))
            cur_line = 0;
    }

    /* Tell the server we're done looking */
    Send_special_line(SPECIAL_FILE_NONE, 0);

    /* No longer using file perusal */
    special_line_type = SPECIAL_FILE_NONE;

    /* The top line is OK again */
    topline_icky = FALSE;

    /* Restore the screen */
    screen_load(TRUE);

    /* Flush messages */
    c_msg_print(NULL);

    /* Did we get kicked out of a store? */
    check_store_leave(TRUE);

    return !is_abort(ke);
}


/*
 * Client config file handler
 */
static char config_name[MSG_LEN];  /* Config filename */

void conf_init(void* param)
{
    char path[MSG_LEN];
    HINSTANCE hInstance = param;

    /* Search for file in user directory */
    if (GetEnvironmentVariable("USERPROFILE", path, sizeof(path)))
    {
        my_strcat(path, "\\mangclient.ini", sizeof(path));

        /* Ok */
        if (file_exists(path))
        {
            my_strcpy(config_name, path, sizeof(config_name));
            return;
        }
    }

    /* Get full path to executable */
    GetModuleFileName(hInstance, path, sizeof(path));
    my_strcpy(path + strlen(path) - 4, ".ini", 5);
    my_strcpy(config_name, path, sizeof(config_name));
}

void conf_save(void)
{
}

void conf_timer(int ticks)
{
}

bool conf_section_exists(const char *section)
{
    char sections[MSG_LEN];
    int n;
    size_t i;

    n = GetPrivateProfileSectionNames(sections, MSG_LEN, config_name);
    if (n != MSG_LEN - 2)
    {
        for (i = 0; sections[i]; i += (strlen(&sections[i]) + 1))
        {
            if (!strcasecmp(&sections[i], section)) return TRUE;
        }
    }

    return FALSE;
}

const char *conf_get_string(const char *section, const char *name,
    const char *default_value)
{
    char value[100];

    GetPrivateProfileString(section, name, default_value, value, 100, config_name);
    return &value[0];
}

s32b conf_get_int(const char *section, const char *name, s32b default_value)
{
    return GetPrivateProfileInt(section, name, default_value, config_name);
}

void conf_set_string(const char *section, const char *name, const char *value)
{
    WritePrivateProfileString(section, name, value, config_name);
}

void conf_set_int(const char *section, const char *name, s32b value)
{
    char s_value[100];

    strnfmt(s_value, sizeof(s_value), "%" PRId32, value);
    WritePrivateProfileString(section, name, s_value, config_name);
}

/* Hack: Append section */
void conf_append_section(const char *sectionFrom, const char *sectionTo, const char *filename)
{
    char keys[2*MSG_LEN];
    char value[MSG_LEN];
    int n;
    size_t i;

    /* Get all keys */
    n = GetPrivateProfileString(sectionTo, NULL, NULL, keys, 2 * MSG_LEN, filename);
    if (n != 2 * MSG_LEN - 2)
    {
        for (i = 0; keys[i]; i += (strlen(&keys[i]) + 1))
        {
            /* Extract key */
            GetPrivateProfileString(sectionFrom, &keys[i], "", value, sizeof(value),
                filename);

            /* Hack: Append key to original config */
            value[100] = '\0'; /* FIXME: change "strings" len */
            conf_set_string(sectionTo, &keys[i], value);
        }
    }
}

bool conf_exists(void)
{
    return file_exists(config_name);
}


/*** Screenshot saving code ***/


/*
 * Encode the screen colors
 */
static const char hack[BASIC_COLORS + 1] = "dwsorgbuDWPyRGBUpvtmYiTVIMzZ";


/*
 * At a given location, determine the "current" attr and char.
 * Display walls and floors properly.
 */
static void Term_what_hack(int x, int y, byte *a, char *c)
{
    /* Get the attr/char */
    Term_what(x, y, a, c);

    /* Hack -- Display walls and floors properly */
    if (*c == 7) *c = '.';
    if (*c == 127) *c = '#';
}


/*
 * Save a simple text screendump.
 */
static void do_cmd_save_screen_text(void)
{
    int y, x;
    byte a = 0;
    char c = ' ';
    ang_file *fff;
    char buf[MSG_LEN];
    int wid, hgt;

    /* Clear */
    c_msg_print(NULL);

    /* Build the filename */
    path_build(buf, MSG_LEN, ANGBAND_DIR_USER, "dump.txt");
    fff = file_open(buf, MODE_WRITE, FTYPE_TEXT);
    if (!fff) return;

    /* Retrieve current screen size */
    Term_get_size(&wid, &hgt);

    /* Dump the screen */
    for (y = 0; y < hgt; y++)
    {
        /* Dump each row */
        for (x = 0; x < wid; x++)
        {
            /* Get the attr/char */
            Term_what_hack(x, y, &a, &c);

            /* Dump it */
            buf[x] = c;
        }

        /* Terminate */
        buf[x] = '\0';

        /* End the row */
        file_putf(fff, "%s\n", buf);
    }

    /* Skip a line */
    file_put(fff, "\n");

    /* Dump the screen */
    for (y = 0; y < hgt; y++)
    {
        /* Dump each row */
        for (x = 0; x < wid; x++)
        {
            /* Get the attr/char */
            Term_what(x, y, &a, &c);

            /* Dump it */
            buf[x] = hack[a & 0x0F];
        }

        /* Terminate */
        buf[x] = '\0';

        /* End the row */
        file_putf(fff, "%s\n", buf);
    }

    /* Skip a line */
    file_put(fff, "\n");

    /* Close it */
    file_close(fff);

    /* Message */
    c_msg_print("Screen dump saved.");
}


static void write_html_escape_char(ang_file *fp, char c)
{
    switch (c)
    {
        case '<':
            file_put(fp, "&lt;");
            break;
        case '>':
            file_put(fp, "&gt;");
            break;
        case '&':
            file_put(fp, "&amp;");
            break;
        default:
            file_putf(fp, "%c", c);
            break;
    }
}


/* Take an html screenshot */
static void html_screenshot(const char *name, int mode)
{
    int y, x;
    int wid, hgt;
    byte a = TERM_WHITE;
    byte oa = TERM_WHITE;
    char c = ' ';
    const char *new_color_fmt = ((mode == 0)? "<font color=\"#%02X%02X%02X\">":
        "[COLOR=\"#%02X%02X%02X\"]");
    const char *change_color_fmt = ((mode == 0)?
        "</font><font color=\"#%02X%02X%02X\">":
        "[/COLOR][COLOR=\"#%02X%02X%02X\"]");
    const char *close_color_fmt = ((mode == 0)? "</font>": "[/COLOR]");
    ang_file *fp;
    char buf[MSG_LEN];

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);
    fp = file_open(buf, MODE_WRITE, FTYPE_TEXT);

    /* Oops */
    if (!fp)
    {
        plog_fmt("Cannot write the '%s' file!", buf);
        return;
    }

    /* Retrieve current screen size */
    Term_get_size(&wid, &hgt);

    if (mode == 0)
    {
        file_put(fp, "<!DOCTYPE html><html><head>\n");
        file_putf(fp, "  <meta='generator' content='%s'>\n", get_buildid(FALSE));
        file_putf(fp, "  <title>%s</title>\n", name);
        file_put(fp, "</head>\n\n");
        file_put(fp, "<body style='color: #fff; background: #000;'>\n");
        file_put(fp, "<pre>\n");
    }
    else
        file_put(fp, "[CODE][TT][BC=black][COLOR=white]\n");

    /* Dump the screen */
    for (y = 0; y < hgt; y++)
    {
        for (x = 0; x < wid; x++)
        {
            /* Get the attr/char */
            Term_what_hack(x, y, &a, &c);

            /* Color change */
            if ((oa != a) && (c != ' '))
            {
                /* From the default white to another color */
                if (oa == TERM_WHITE)
                    file_putf(fp, new_color_fmt, angband_color_table[a][1],
                        angband_color_table[a][2], angband_color_table[a][3]);

                /* From another color to the default white */
                else if (a == TERM_WHITE)
                    file_put(fp, close_color_fmt);

                /* Change colors */
                else
                    file_putf(fp, change_color_fmt, angband_color_table[a][1],
                        angband_color_table[a][2], angband_color_table[a][3]);

                /* Remember the last color */
                oa = a;
            }

            /* Write the character and escape special HTML characters */
            if (mode == 0) write_html_escape_char(fp, c);
            else file_putf(fp, "%c", c);
        }

        /* End the row */
        file_put(fp, "\n");
    }

    /* Close the last font-color tag if necessary */
    if (oa != TERM_WHITE) file_put(fp, close_color_fmt);

    if (mode == 0)
    {
        file_put(fp, "</pre>\n");
        file_put(fp, "</body>\n");
        file_put(fp, "</html>\n");
    }
    else
        file_put(fp, "[/COLOR][/BC][/TT][/CODE]\n");

    /* Close it */
    file_close(fp);
}


/*
 * Hack -- save a screen dump to a file in html format
 */
static void do_cmd_save_screen_html(int mode)
{
    char tmp_val[256];

    /* Ask for a file */
    if (mode == 0) my_strcpy(tmp_val, "dump.html", sizeof(tmp_val));
    else my_strcpy(tmp_val, "dump.txt", sizeof(tmp_val));
    if (!get_string("File: ", tmp_val, sizeof(tmp_val))) return;

    c_msg_print(NULL);

    /* Dump the screen with raw character attributes */
    html_screenshot(tmp_val, mode);

    c_msg_print("HTML screen dump saved.");
}


/*
 * Hack -- save a screen dump to a file
 */
void do_cmd_save_screen(void)
{
    ui_event ke;

    /* Doesn't work when graphics are on */
    if (p_ptr->use_graphics)
    {
        c_msg_print("This feature is only implemented in ASCII mode.");
        return;
    }

    c_msg_print("Dump type [(t)ext; (h)tml; (f)orum embedded html]:");

    while (1)
    {
        ke = inkey_ex();
        if (is_exit(ke)) break;

        if (ke.type == EVT_KBRD)
        {
            switch (ke.key.code)
            {
                case 't':
                    do_cmd_save_screen_text();
                    return;

                case 'h':
                    do_cmd_save_screen_html(0);
                    return;

                case 'f':
                    do_cmd_save_screen_html(1);
                    return;
            }
        }
    }
}


/*
 * Write formatted string `fmt` on line `y`, centred between points x1 and x2.
 */
static void put_str_centred(int y, int x1, int x2, const char *fmt, ...)
{
    va_list vp;
    char *tmp;
    size_t len;
    int x;

    /* Format into the (growable) tmp */
    va_start(vp, fmt);
    tmp = vformat(fmt, vp);
    va_end(vp);

    /* Centre now */
    len = strlen(tmp);
    x = x1 + ((x2 - x1) / 2 - len / 2);

    put_str(tmp, y, x);
}


/*
 * Display the tombstone
 */
void print_tomb(void)
{
    int i, line = 7;

    /* Clear the screen */
    Term_clear();

    for (i = 0; i < TEXTFILE__HGT; i++)
    {
        /* Show each line */
        text_out_e(&Setup.text_screen[TEXTFILE_TOMB][i * TEXTFILE__WID], i);
    }

    put_str_centred(line++, 8, 8+31, "%s", p_ptr->name);
    put_str_centred(line++, 8, 8+31, "the");
    put_str_centred(line++, 8, 8+31, "%s", p_ptr->death_info.title);

    line++;

    put_str_centred(line++, 8, 8+31, "%s", p_ptr->clazz->name);
    put_str_centred(line++, 8, 8+31, "Level: %d", (int)p_ptr->death_info.lev);
    put_str_centred(line++, 8, 8+31, "Exp: %d", (int)p_ptr->death_info.exp);
    put_str_centred(line++, 8, 8+31, "AU: %d", (int)p_ptr->death_info.au);
    put_str_centred(line++, 8, 8+31, "Killed on Level %d", p_ptr->death_info.depth);
    put_str_centred(line++, 8, 8+31, "by %s.", p_ptr->death_info.died_from);

    line++;

    put_str_centred(line++, 8, 8+31, "by %-.24s", p_ptr->death_info.ctime);

    /* Show it */
    Term_fresh();

    /* Disable updates (this is the final screen) */
    p_ptr->screen_icky++;
}


/*
 * Display the winner crown
 */
void display_winner(void)
{
    int i, wid, hgt;

    Term_clear();
	Term_get_size(&wid, &hgt);

    for (i = 0; i < TEXTFILE__HGT; i++)
    {
        /* Show each line */
        text_out_e(&Setup.text_screen[TEXTFILE_CRWN][i * TEXTFILE__WID], i);
    }

    put_str_centred(i, 0, wid, "All Hail the Mighty %s!", p_ptr->sex->winner);

    /* Show it */
    Term_fresh();

    /* Disable updates (this is the final screen) */
    p_ptr->screen_icky++;
}
