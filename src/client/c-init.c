/*
 * File: c-init.c
 * Purpose: Various game initialisation routines
 *
 * Copyright (c) 1997 Ben Harrison
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
 * This file should contain non-system-specific code.  If a
 * specific system needs its own "main" function (such as
 * Windows), then it should be placed in the "main-???.c" file.
 */

#include "c-angband.h"
#include "../common/buildid.h"
#include "../common/randname.h"
#include "c-cmds.h"
#include "game-event.h"
#include "netclient.h"
#include "prefs.h"
#include "textui.h"

static int Socket;

static void init_arrays(void)
{
    /* Message variables */
    messages_init();

    /* Initialize the current store */
    current_store.stock = C_ZNEW(STORE_INVEN_MAX, object_type);
    current_store.owner = ZNEW(owner_type);

    /* Clear client_setup */
    Client_setup.k_attr = NULL;
}


/* Init minor arrays */
static void init_minor(void)
{
    int i;

    /* Chat channels */
    p_ptr->on_channel = C_ZNEW(MAX_CHANNELS, byte);
    for (i = 0; i < MAX_CHANNELS; i++)
    {
        channels[i].name[0] = '\0';
        channels[i].id = channels[i].num = 0;
        p_ptr->on_channel[i] = 0;
    }
    p_ptr->main_channel = 0;

    /* Term channels */
    p_ptr->remote_term = NTERM_WIN_OVERHEAD;
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
void init_stuff(void)
{
    char configpath[MSG_LEN];
    char libpath[MSG_LEN];
    char datapath[MSG_LEN];

    /* Use default */
    my_strcpy(configpath, DEFAULT_CONFIG_PATH, sizeof(configpath));
    my_strcpy(libpath, DEFAULT_LIB_PATH, sizeof(libpath));
    my_strcpy(datapath, DEFAULT_DATA_PATH, sizeof(datapath));

    /* Hack -- Add a path separator (only if needed) */
    if (!suffix(configpath, PATH_SEP))
        my_strcat(configpath, PATH_SEP, sizeof(configpath));
    if (!suffix(libpath, PATH_SEP))
        my_strcat(libpath, PATH_SEP, sizeof(libpath));
    if (!suffix(datapath, PATH_SEP))
        my_strcat(datapath, PATH_SEP, sizeof(datapath));

    /* Initialize */
    init_file_paths(configpath, libpath, datapath);
}


/*
 * Open all relevant pref files.
 */
void initialize_all_pref_files(void)
{
    char buf[MSG_LEN];

    /* Access the "basic" pref file */
    my_strcpy(buf, "pref.prf", sizeof(buf));

    /* Process that file */
    process_pref_file(buf, FALSE, FALSE);

    /* Reset visuals */
    reset_visuals(TRUE);

    /* Access the "user" pref file */
    strnfmt(buf, sizeof(buf), "user.prf");

    /* Process that file */
    process_pref_file(buf, TRUE, TRUE);

    /* Access the "character" pref file */
    strnfmt(buf, sizeof(buf), "%s.prf", nick);
    buf[0] = tolower((unsigned char)buf[0]);

    /* Process that file */
    process_pref_file(buf, TRUE, TRUE);

    /* Set up the subwindows */
    subwindows_reinit_flags();
    subwindows_init_flags();
}


/* Loop callback */
static void input_callback_end()
{
    /* Process any commands we got */
    textui_process_command();

    /* Hack -- don't redraw the screen until we have all of it */
    if (last_line_info != -1) return;

    /* Flush input (now!) */
    flush_now();
}


/*
 * Loop, looking for net input and responding to keypresses.
 */
static void Input_loop(void)
{
    if (Net_flush() == -1)
    {
        send_quit = FALSE;
        return;
    }

    /* Loop, looking for net input and responding to keypresses */
    Net_loop(NULL, NULL, input_callback_end, SCAN_OFF);
}


bool gather_settings()
{
    int i;
    s16b new_settings[SETTING_MAX];
    bool changed = FALSE;

    /* Initialize */
    for (i = 0; i < SETTING_MAX; i++) new_settings[i] = 0;

    /* Gather */
    new_settings[SETTING_USE_GRAPHICS] = p_ptr->use_graphics;
    new_settings[SETTING_SCREEN_COLS] = Term->wid - COL_MAP - 1;
    new_settings[SETTING_SCREEN_ROWS] = Term->hgt - ROW_MAP - 1;
    new_settings[SETTING_TILE_WID] = tile_width;
    new_settings[SETTING_TILE_HGT] = tile_height;
    new_settings[SETTING_TILE_DISTORTED] = tile_distorted;
    new_settings[SETTING_MAX_HGT] = Term->max_hgt;
    new_settings[SETTING_WINDOW_FLAG] = 0;
    for (i = 0; i < 8; i++) new_settings[SETTING_WINDOW_FLAG] |= window_flag[i];
    new_settings[SETTING_HITPOINT_WARN] = p_ptr->other.hitpoint_warn;
    new_settings[SETTING_DELAY_FACTOR] = p_ptr->other.delay_factor;
    for (i = 0; i < TYPE_MAX; i++)
        new_settings[i + SETTING_SQUELCH_JEWELRY] = p_ptr->other.squelch_lvl[i];

    /* Change */
    for (i = 0; i < SETTING_MAX; i++)
    {
        if (Client_setup.settings[i] != new_settings[i])
        {
            Client_setup.settings[i] = new_settings[i];
            changed = TRUE;
        }
    }

    /* Return */
    return changed;
}


/*
 * Client is ready to play call-back
 */
bool client_ready()
{
    int i;

    /* Send request for MOTD to read (optional) */
    Send_text_screen(TEXTFILE_MOTD, 0); /* pass -1 to receive motd off-screen */

    /* Set a default warning level that will be overridden by the savefile */
    p_ptr->other.hitpoint_warn = 3;

    /* Initialize extra parameters */
    p_ptr->other.delay_factor = 1;
    for (i = 0; i < TYPE_MAX; i++) p_ptr->other.squelch_lvl[i] = SQUELCH_WORTHLESS;

    /* Initialize default option values */
    option_set_defaults(Client_setup.options);

    /* Initialize window options that will be overridden by the savefile */
    memset(window_flag, 0, sizeof(u32b) * ANGBAND_TERM_MAX);
    window_flag[0] |= (PW_PLAYER_2 | PW_STATUS);
    window_flag[1] |= (PW_MESSAGE);
    window_flag[2] |= (PW_EQUIP);
    window_flag[3] |= (PW_INVEN);
    window_flag[4] |= (PW_MESSAGE_CHAT);
    window_flag[5] |= (PW_MONLIST);
    window_flag[6] |= (PW_ITEMLIST);
    window_flag[7] |= (PW_MAP);

    /* Initialize the pref files */
    initialize_all_pref_files();

    /* Send event */
    Term_xtra(TERM_XTRA_REACT, p_ptr->use_graphics);

    gather_settings();

    /* Sneakily init command list */
    cmd_init();

    Send_options(TRUE);

    /* Send visual preferences */
    Net_verify();

    Setup.initialized = TRUE;

    /* Hack -- Don't enter the game if waiting for motd */
    if (Setup.wait && !Setup.ready) return FALSE;

    /* Request gameplay */
    Send_play(1);

    /* Hack -- Full icky screen */
    full_icky_screen = conf_get_int("MAngband", "FullIckyScreen", 0);

    return TRUE;
}


/*
 * Initialize everything, contact the server, and start the loop.
 */
void client_init(char *argv1)
{
    sockbuf_t ibuf;
    char status, num_types, expiry;
    int trycount;
    char host_name[NORMAL_WID], trymsg[NORMAL_WID], *s;
    u16b version = CUR_VERSION;
    u16b conntype = CONNTYPE_PLAYER;
    char buffer[NORMAL_WID];
    DWORD nSize = NORMAL_WID;
    bool done = FALSE;
    u16b num;
    u32b num_name;
    size_t i, j;
    struct keypress c;

    /* Set up the display handlers and things. */
    init_display();

    /* Initialize various arrays */
    init_arrays();

    /* Initialize minor arrays */
    init_minor();

    GetLocalHostName(host_name, NORMAL_WID);

    /* Default server host and port */
    server_port = conf_get_int("MAngband", "port", 18346);
    my_strcpy(server_name, conf_get_string("MAngband", "host", ""), sizeof(server_name));

    /* Set server host and port */
    if (argv1 != NULL)
    {
        /* Set the server's name */
        my_strcpy(server_name, argv1, sizeof(server_name));

        /* Set server port */
        s = strchr(server_name, ':');
        if (s)
        {
            sscanf(s, ":%d", &server_port);
            *s = '\0';
        }
    }

    /* Check whether we should query the metaserver */
    if (STRZERO(server_name))
    {
        /* Query metaserver */
        if (!get_server_name())
            quit("No server specified.");
    }

    /* Fix "localhost" */
    if (!strcmp(server_name, "localhost"))
        my_strcpy(server_name, host_name, sizeof(server_name));

    /* Default nickname and password */
    my_strcpy(nick, conf_get_string("MAngband", "nick", nick), sizeof(nick));
    my_strcpy(pass, conf_get_string("MAngband", "pass", pass), sizeof(pass));

    /* Capitalize the name */
    my_strcap(nick);

    /* Get account name and pass */
    get_account_name();

    /* Create the net socket and make the TCP connection */
    if ((Socket = CreateClientSocket(server_name, server_port)) == -1)
    {
        /* Display the socket error message */
        put_str(GetSocketErrorMessage(), 19, 1);

        while (!done)
        {
            /* Prompt for auto-retry */
            put_str("Couldn't connect to server, keep trying? [Y/N]", 21, 1);

            /* Make sure the message is shown */
            Term_fresh();
            WIPE(&c, struct keypress);
            while ((c.code != 'Y') && (c.code != 'y') && (c.code != 'N') && (c.code != 'n'))
            {
                /* Get a key */
                c = inkey();
            }

            /* If we dont want to retry, exit with error */
            if ((c.code == 'N') || (c.code == 'n'))
                quit("That server either isn't up, or you mistyped the hostname.");

            /* ...else, keep trying until socket connected */
            trycount = 1;
            while ((Socket = CreateClientSocket(server_name, server_port)) == -1)
            {
                if (trycount > 200) break;

                /* Progress Message */
                strnfmt(trymsg, sizeof(trymsg),
                    "Connecting to server [%i]                      ", trycount++);
                put_str(trymsg, 21, 1);
                
                /* Make sure the message is shown */
                Term_redraw(); /* Hmm maybe not the proper way to force an os poll */
                Term_flush();
            }
            if (Socket != -1) done = TRUE;
        }
    }

    /* Create a socket buffer */
    if (Sockbuf_init(&ibuf, Socket, CLIENT_SEND_SIZE, SOCKBUF_READ | SOCKBUF_WRITE) == -1)
        quit("No memory for socket buffer");

    /* Clear it */
    Sockbuf_clear(&ibuf);

    /* Get user name */
    if (GetUserName(buffer, &nSize))
    {
        buffer[16] = '\0';
        my_strcpy(real_name, buffer, sizeof(real_name));
    }

    /* Put the contact info in it */
    Packet_printf(&ibuf, "%hu", (unsigned)conntype);
    Packet_printf(&ibuf, "%hu", (unsigned)version);
    Packet_printf(&ibuf, "%s%s%s%s", real_name, host_name, nick, stored_pass);

    /* Send it */
    if (!Net_Send(Socket, &ibuf))
        quit("Couldn't send contact information");

    /* Wait for reply */
    if (!Net_WaitReply(Socket, &ibuf, 10))
        quit("Server didn't respond!");

    /* Read what he sent */
    Packet_scanf(&ibuf, "%c", &status);
    Packet_scanf(&ibuf, "%hu", &num);
    char_num = num;
    char_name = NULL;
    char_expiry = NULL;
    if (char_num)
    {
        char_name = C_ZNEW(char_num, char*);
        char_expiry = C_ZNEW(char_num, char);
    }
    for (i = 0; i < char_num; i++)
    {
        Packet_scanf(&ibuf, "%c", &expiry);
        char_expiry[i] = expiry;
        Packet_scanf(&ibuf, "%s", buffer);
        char_name[i] = string_make(buffer);
    }
    Packet_scanf(&ibuf, "%c", &num_types);

    /* Something went wrong... */
    if ((num_types != RANDNAME_NUM_TYPES) && !status)
        quit("Failed to retrieve random name fragments.");

    /* Initialise the random name fragments */
    num_names = C_ZNEW(RANDNAME_NUM_TYPES, u32b);
    name_sections = mem_zalloc(sizeof(char**) * RANDNAME_NUM_TYPES);
    for (i = 0; i < RANDNAME_NUM_TYPES; i++)
    {
        Packet_scanf(&ibuf, "%lu", &num_name);
        num_names[i] = num_name;
        name_sections[i] = mem_alloc(sizeof(char*) * (num_names[i] + 1));
        for (j = 0; j < num_names[i]; j++)
        {
            Packet_scanf(&ibuf, "%s", buffer);
            name_sections[i][j] = string_make(buffer);
        }
    }

    /* Some error */
    if (status)
    {
        /* The server didn't like us.... */
        switch (status)
        {
            case E_VERSION_OLD:
                quit("Client is outdated. Check MAngband forums to get the new PWMAngband client.");
            case E_INVAL:
                quit("The server didn't like your nickname, realname, or hostname.");
            case E_ACCOUNT:
                quit("The password you supplied for the account is incorrect.");
            case E_GAME_FULL:
                quit("Sorry, the game is full. Try again later.");
            case E_SOCKET:
                quit("Socket error.");
            case E_VERSION_NEW:
                quit("Your client will not work on that server (server version is too old).");
            default:
                quit_fmt("Connection failed with status %d.", status);
        }
    }

    /* Server agreed to talk, initialize the buffers */
    if (Net_init(Socket) == -1)
        quit("Network initialization failed!");

    /* Get character name and pass */
    get_char_name();

    /* Send request for anything needed for play */
    Send_play(0);

    /* Main loop */
    Input_loop();
}


static void free_races(void)
{
    struct player_race *r, *n;

    r = races;
    while (r)
    {
        n = r->next;
        string_free(r->name);
        mem_free(r);
        r = n;
    }
}


static void free_classes(void)
{
    struct player_class *c, *n;

    c = classes;
    while (c)
    {
        n = c->next;
        string_free(c->name);
        mem_free(c);
        c = n;
    }
}


void cleanup_angband(void)
{
    size_t i;

    /* Free the menus */
    free_command_menu();
    free_option_menus();

    /* Free the keymaps */
    keymap_free();

    /* Free the player inventory */
    mem_free(p_ptr->inventory);
    mem_nfree((void**)inventory_name, all_inven_total);
    mem_nfree((void**)eq_name, all_inven_total - inven_wield);

    mem_free(p_ptr->on_channel);

    event_remove_all_handlers();

    /* Free the current store */
    mem_free(current_store.stock);
    if (current_store.owner) string_free(current_store.owner->name);
    mem_free(current_store.owner);

    /* Free the character list */
    for (i = 0; i < char_num; i++) string_free(char_name[i]);
    mem_free(char_name);
    mem_free(char_expiry);

    /* Free the random name fragments */
    strings_free(name_sections, num_names, RANDNAME_NUM_TYPES);

    /* Free attr/chars */
    mem_free(Client_setup.k_attr);
    mem_free(Client_setup.k_char);
    mem_free(Client_setup.r_attr);
    mem_free(Client_setup.r_char);
    mem_free(Client_setup.f_attr);
    mem_free(Client_setup.f_char);
    mem_free(Client_setup.flvr_x_attr);
    mem_free(Client_setup.flvr_x_char);

    /* Free the messages */
    messages_free();

    /* Free the info arrays */
    for (i = 0; k_info && (i < z_info->k_max); i++) string_free(k_info[i].name);
    mem_free(k_info);
    free_races();
    free_classes();
    for (i = 0; soc_info && (i < z_info->soc_max); i++) string_free(soc_info[i].name);
    mem_free(soc_info);
    while (hints)
    {
        struct hint *h = hints->next;

        string_free(hints->hint);
        mem_free(hints);
        hints = h;
    }
    for (i = 0; r_info && (i < z_info->r_max); i++) string_free(r_info[i].name);
    mem_free(r_info);

    /* Free the format() buffer */
    vformat_kill();

    /* Free the directories */
    free_file_paths();
}
