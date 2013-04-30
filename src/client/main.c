/*
 * File: main.c
 * Purpose: Core game initialisation
 *
 * Copyright (c) 1997 Ben Harrison, and others
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

/*
 * Some machines have a "main()" function in their "main-xxx.c" file,
 * all the others use this file for their "main()" function.
 */


#include "c-angband.h"
#include "netclient.h"


#if defined(USE_GCU) || defined(USE_SDL)
#include "main.h"
#endif


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


static errr init_sound_dummy(void)
{
    return 0;
}


/*
 * List of sound modules in the order they should be tried.
 */
static const struct module sound_modules[] =
{
#ifdef USE_SDL
#ifdef USE_SOUND
    {"sdl", init_sound_sdl},
#endif
#endif

    {"none", init_sound_dummy}
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
        buffer[16] = '\0';

        /* Copy to real name */
        my_strcpy(real_name, buffer, sizeof(real_name));
    }
}


/*
 * Simple "main" function for multiple platforms.
 */
int main(int argc, char **argv)
{
    bool done = FALSE;
    WSADATA wsadata;
    int i;

    /* Save the program name */
    argv0 = argv[0];

    /* Initialize WinSock */
    WSAStartup(MAKEWORD(1, 1), &wsadata);

    WIPE(&Setup, server_setup_t);

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
            done = TRUE;
            break;
        }
    }

    /* Make sure we have a display! */
    if (!done) quit("Unable to prepare any 'display module'!");

    /* Try the sound modules in the order specified by sound_modules[] */
    for (i = 0; i < (int)N_ELEMENTS(sound_modules); i++)
    {
        if (0 == sound_modules[i].init()) break;
    }

    /* Attempt to read default name/real name from OS */
    read_credentials();

    /* Get the meta address */
    my_strcpy(meta_address, conf_get_string("MAngband", "meta_address", "mangband.org"),
        sizeof(meta_address));

    turn_off_numlock();

    /* Initialize everything, contact the server, and start the loop */
    if (argc == 2)
    {
        /* Initialize with given server name */
        client_init(argv[1]);
    }
    else
    {
        /* Initialize and query metaserver */
        client_init(NULL);
    }

    /* Quit */
    quit(NULL);

    /* Exit */
    return 0;
}
