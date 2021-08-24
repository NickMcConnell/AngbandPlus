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

/*
 * Some machines have a "main()" function in their "main-xxx.c" file,
 * all the others use this file for their "main()" function.
 */


#include "c-angband.h"


static errr init_error(void)
{
    return 1;
}


/*
 * List of the available modules in the order they are tried.
 */
static const struct module modules[] =
{
#ifdef USE_SDL
    {"sdl", init_sdl},
#endif

#ifdef USE_GCU
    {"gcu", init_gcu},
#endif

    {"none", init_error}
};


/*
 * A hook for "quit()".
 *
 * Close down, then fall back into "quit()".
 */
static void quit_hook(const char *s)
{
    int j;

    /* Scan windows */
    for (j = ANGBAND_TERM_MAX - 1; j >= 0; j--)
    {
        /* Unused */
        if (!angband_term[j]) continue;

        /* Nuke it */
        term_nuke(angband_term[j]);
    }

    textui_cleanup();
    cleanup_angband();

    /* Cleanup WinSock */
    WSACleanup();
}


static void read_credentials(void)
{
    char buffer[20] = {'\0'};
    DWORD bufferLen = sizeof(buffer);

    /* Initial defaults */
    my_strcpy(nick, "PLAYER", sizeof(nick));
    my_strcpy(pass, "passwd", sizeof(pass));
    my_strcpy(real_name, "PLAYER", sizeof(real_name));

    /* Get user name from Windows machine! */
    if (GetUserName(buffer, &bufferLen))
    {
        /* Cut */
        buffer[MAX_NAME_LEN] = '\0';

        /* Copy to real name */
        my_strcpy(real_name, buffer, sizeof(real_name));
    }
}


/*
 * Simple "main" function for multiple platforms.
 */
int main(int argc, char **argv)
{
    bool done = false;
    WSADATA wsadata;
    int i;

    /* Save the program name */
    argv0 = argv[0];

    /* Save command-line arguments */
    clia_init(argc, (const char**)argv);

    /* Initialize WinSock */
    WSAStartup(MAKEWORD(1, 1), &wsadata);

    memset(&Setup, 0, sizeof(Setup));

    /* Client Config-file */
    conf_init(NULL);

    /* Setup the file paths */
    init_stuff();

    /* Install "quit" hook */
    quit_aux = quit_hook;

    /* Try the modules in the order specified by modules[] */
    for (i = 0; i < (int)N_ELEMENTS(modules); i++)
    {
        if (0 == modules[i].init())
        {
            ANGBAND_SYS = modules[i].name;
            done = true;
            break;
        }
    }

    /* Make sure we have a display! */
    if (!done) quit("Unable to prepare any 'display module'!");

    /* Attempt to read default name/real name from OS */
    read_credentials();

    /* Get the meta address */
    my_strcpy(meta_address, conf_get_string("MAngband", "meta_address", "mangband.org"),
        sizeof(meta_address));
    meta_port = conf_get_int("MAngband", "meta_port", 8802);

    turn_off_numlock();

    /* Initialize everything, contact the server, and start the loop */
    client_init();

    /* Quit */
    quit(NULL);

    /* Exit */
    return 0;
}
