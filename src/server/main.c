/*
 * File: main.c
 * Purpose: Core game initialisation
 *
 * Copyright (c) 1997 Ben Harrison, and others
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


#include "s-angband.h"


/*
 * A hook for "quit()".
 *
 * Close down, then fall back into "quit()".
 */
static void quit_hook(const char *s)
{
    /* Quit with error */
    if (s) exit_game_panic();

    /* Free resources */
    else cleanup_angband();
}


/*
 * Initialize and verify the file paths.
 *
 * Use the DEFAULT_XXX_PATH constants by default.
 *
 * We must ensure that the path ends with "PATH_SEP" if needed,
 * since the "init_file_paths()" function will simply append the
 * relevant "sub-directory names" to the given path.
 */
static void init_stuff(void)
{
    char configpath[MSG_LEN];
    char libpath[MSG_LEN];
    char datapath[MSG_LEN];

    /* Use default */
    my_strcpy(configpath, DEFAULT_CONFIG_PATH, sizeof(configpath));
    my_strcpy(libpath, DEFAULT_LIB_PATH, sizeof(libpath));
    my_strcpy(datapath, DEFAULT_DATA_PATH, sizeof(datapath));

    /* Hack -- add a path separator (only if needed) */
    if (!suffix(configpath, PATH_SEP))
        my_strcat(configpath, PATH_SEP, sizeof(configpath));
    if (!suffix(libpath, PATH_SEP))
        my_strcat(libpath, PATH_SEP, sizeof(libpath));
    if (!suffix(datapath, PATH_SEP))
        my_strcat(datapath, PATH_SEP, sizeof(datapath));

    /* Initialize */
    init_file_paths(configpath, libpath, datapath);

    /* Create any missing directories */
    create_needed_dirs();
}


/* Daily log file */
static int tm_mday = 0;
static ang_file *fp = NULL;


/*
 * Translate from ISO Latin-1 characters 128+ to 7-bit ASCII.
 *
 * We use this table to maintain compatibility with systems that cannot
 * display 8-bit characters.  We also use it whenever we wish to suppress
 * accents or ensure that a character is 7-bit.
 */
static const char seven_bit_translation[128] =
{
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    'A', 'A', 'A', 'A', 'A', 'A', ' ', 'C',
    'E', 'E', 'E', 'E', 'I', 'I', 'I', 'I',
    'D', 'N', 'O', 'O', 'O', 'O', 'O', ' ',
    'O', 'U', 'U', 'U', 'U', 'Y', ' ', ' ',
    'a', 'a', 'a', 'a', 'a', 'a', ' ', 'c',
    'e', 'e', 'e', 'e', 'i', 'i', 'i', 'i',
    'o', 'n', 'o', 'o', 'o', 'o', 'o', ' ',
    'o', 'u', 'u', 'u', 'u', 'y', ' ', 'y'
};


/*
 * Server logging hook.
 * We should be cautious, as we may be called from a signal handler in a panic.
 */
static void server_log(const char *str)
{
    char buf[16];
    time_t t;
    struct tm *local;
    char path[MSG_LEN];
    char file[30];
    char ascii[MSG_LEN];
    char *s;

    /* Grab the time */
    time(&t);
    local = localtime(&t);
    strftime(buf, 16, "%d%m%y %H%M%S", local);

    /* Translate into ASCII */
    my_strcpy(ascii, str, sizeof(ascii));
    for (s = ascii; *s; s++)
    {
        if (*s < 0) *s = seven_bit_translation[128 + *s];
    }

    /* Output the message timestamped */
    fprintf(stderr, "%s %s\n", buf, ascii);

    /* Open the daily log file */
    if (tm_mday != local->tm_mday)
    {
        tm_mday = local->tm_mday;

        /* Close the daily log file */
        if (fp) file_close(fp);

        /* Open a new daily log file */
        strftime(file, 30, "pwmangband%d%m%y.log", local);
        path_build(path, sizeof(path), ANGBAND_DIR_SCORES, file);
        fp = file_open(path, MODE_APPEND, FTYPE_TEXT);
    }

    /* Output the message to the daily log file */
    file_putf(fp, "%s %s\n", buf, str);
    file_flush(fp);
}


static void show_version(void)
{
    printf("PWMAngband Server %s\n", version_build(NULL, true));
    puts("Copyright (c) 2007-2019 MAngband and PWMAngband Project Team");

    /* Actually abort the process */
    quit(NULL);
}


/*
 * Some machines can actually parse command line args
 */
int main(int argc, char *argv[])
{
    WSADATA wsadata;
    char buf[MSG_LEN];

    /* Setup assert hook */
    assert_aux = exit_game_panic;

    /* Get the file paths */
    init_stuff();

    /* Setup our logging hook */
    plog_aux = server_log;

    /* Save the "program name" */
    argv0 = argv[0];

    /* Load our debugging library on Windows, to give us nice stack dumps */
    /* We use exchndl.dll from the mingw-utils package */
    LoadLibrary("exchndl.dll");

    /* Initialize WinSock */
    WSAStartup(MAKEWORD(1, 1), &wsadata);

    /* Process the command line arguments */
    for (--argc, ++argv; argc > 0; --argc, ++argv)
    {
        /* Require proper options */
        if (argv[0][0] != '-') goto usage;

        /* Analyze option */
        switch (argv[0][1])
        {
            case 'v':
                show_version();

            default:
                usage:

                /* Note -- the Term is NOT initialized */
                puts("Usage: mangband [options]");
                puts("  -v   Show version");

                /* Actually abort the process */
                quit(NULL);
        }
    }

    /* Note we are starting up */
    plog("Game Restarted");

    /* Tell "quit()" to call "Term_nuke()" */
    quit_aux = quit_hook;

    /* Catch nasty "signals" on Windows */
    setup_exit_handler();

    /* Verify the "news" file */
    path_build(buf, sizeof(buf), ANGBAND_DIR_SCREENS, "news.txt");
    if (!file_exists(buf))
    {
        /* Why */
        plog_fmt("Cannot access the '%s' file!", buf);

        /* Quit with error */
        quit("Fatal Error.");
    }

    /* Load the mangband.cfg options */
    load_server_cfg();

    /* Initialize the basics */
    init_angband();

    /* Play the game */
    play_game();

    /* Quit */
    quit(NULL);

    /* Exit */
    return (0);
}
