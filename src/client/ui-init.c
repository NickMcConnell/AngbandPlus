/*
 * File: ui-init.c
 * Purpose: Various game initialisation routines
 *
 * Copyright (c) 1997 Ben Harrison
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
 * This file should contain non-system-specific code.  If a
 * specific system needs its own "main" function (such as
 * Windows), then it should be placed in the "main-???.c" file.
 */


#include "c-angband.h"


struct feature *f_info;


/* Connection parameters */
char meta_address[NORMAL_WID];
int meta_port;
char nick[NORMAL_WID];
char pass[NORMAL_WID];
char stored_pass[NORMAL_WID];
char real_name[NORMAL_WID];
char server_name[NORMAL_WID];
int server_port;


/* Character list */
u16b max_account_chars;
u16b char_num;
char **char_name;
char *char_expiry;


static int Socket;


/* Free the sub-paths */
static void free_file_paths(void)
{
    string_free(ANGBAND_DIR_CUSTOMIZE);
    string_free(ANGBAND_DIR_SCREENS);
    string_free(ANGBAND_DIR_FONTS);
    string_free(ANGBAND_DIR_TILES);
    string_free(ANGBAND_DIR_SOUNDS);
    string_free(ANGBAND_DIR_ICONS);
    string_free(ANGBAND_DIR_USER);
}


/*
 * Find the default paths to all of our important sub-directories.
 *
 * All of the sub-directories should, by default, be located inside
 * the main directory, whose location is very system dependent. (On multi-
 * user systems such as Linux this is not the default - see config.h for
 * more info.)
 *
 * This function takes a writable buffer, initially containing the
 * "path" to the "config", "lib" and "data" directories, for example,
 * "/etc/angband/", "/usr/share/angband" and "/var/games/angband" -
 * or a system dependent string, for example, ":lib:".  The buffer
 * must be large enough to contain at least 32 more characters.
 *
 * Various command line options may allow some of the important
 * directories to be changed to user-specified directories, most
 * importantly, the "scores" and "user" and "save" directories,
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
    char buf[MSG_LEN];

    /*** Free everything ***/

    /* Free the sub-paths */
    free_file_paths();

    /*** Prepare the paths ***/

#define BUILD_DIRECTORY_PATH(dest, basepath, dirname) \
    path_build(buf, sizeof(buf), (basepath), (dirname)); \
    dest = string_make(buf);

    /* Paths generally containing configuration data for Angband. */
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_CUSTOMIZE, configpath, "customize");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_SCREENS, libpath, "screens");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_FONTS, libpath, "fonts");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_TILES, libpath, "tiles");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_SOUNDS, libpath, "sounds");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_ICONS, libpath, "icons");

    /* Build the path to the user specific directory */
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_USER, datapath, "user");

#undef BUILD_DIRECTORY_PATH
}


static void init_arrays(void)
{
    /* Message variables */
    messages_init();

    /* Initialize the current store */
    memset(&current_store, 0, sizeof(current_store));
    current_store.owner = mem_zalloc(sizeof(struct owner));

    /* Clear Client_setup.k_attr */
    Client_setup.k_attr = NULL;
}


/* Init minor arrays */
static void init_minor(void)
{
    int i;

    /* Chat channels */
    player->on_channel = mem_zalloc(MAX_CHANNELS * sizeof(byte));
    for (i = 0; i < MAX_CHANNELS; i++)
    {
        channels[i].name[0] = '\0';
        channels[i].id = channels[i].num = 0;
        player->on_channel[i] = 0;
    }
    player->main_channel = 0;

    /* Term channels */
    player->remote_term = NTERM_WIN_OVERHEAD;
}


/*
 * Initialize player struct
 */
static void init_player(void)
{
    /* Create the player array, initialised with 0 */
    player = mem_zalloc(sizeof(*player));

    /* Allocate player sub-structs */
    player->upkeep = mem_zalloc(sizeof(struct player_upkeep));
    player->timed = mem_zalloc(TMD_MAX * sizeof(s16b));

    options_init_defaults(&player->opts);
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

    /* Hack -- add a path separator (only if needed) */
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
static void initialize_all_pref_files(void)
{
    char buf[MSG_LEN];

    /* Process the "pref.prf" file */
    process_pref_file("pref.prf", false, false);

    /* Reset visuals */
    reset_visuals(true);

    /* Process the "window.prf" file */
    process_pref_file("window.prf", true, true);

    /* Process the "user.prf" file */
    process_pref_file("user.prf", true, true);

    /* Get the "PLAYER.prf" filename */
    strnfmt(buf, sizeof(buf), "%s.prf", strip_suffix(nick));

    /* Process the "PLAYER.prf" file, using the character name */
    process_pref_file(buf, true, true);

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
        send_quit = false;
        return;
    }

    /* Loop, looking for net input and responding to keypresses */
    Net_loop(NULL, NULL, input_callback_end, SCAN_OFF);
}


bool gather_settings(void)
{
    int i;
    s16b new_settings[SETTING_MAX];
    bool changed = false;

    /* Initialize */
    for (i = 0; i < SETTING_MAX; i++) new_settings[i] = 0;

    /* Gather */
    new_settings[SETTING_USE_GRAPHICS] = use_graphics;
    new_settings[SETTING_SCREEN_COLS] = Term->wid - COL_MAP - 1;
    new_settings[SETTING_SCREEN_ROWS] = Term->hgt - ROW_MAP - 1;
    new_settings[SETTING_TILE_WID] = tile_width;
    new_settings[SETTING_TILE_HGT] = tile_height;
    new_settings[SETTING_TILE_DISTORTED] = tile_distorted;
    new_settings[SETTING_MAX_HGT] = Term->max_hgt;
    new_settings[SETTING_WINDOW_FLAG] = 0;
    for (i = 0; i < 8; i++) new_settings[SETTING_WINDOW_FLAG] |= window_flag[i];
    new_settings[SETTING_HITPOINT_WARN] = player->opts.hitpoint_warn;

    /* Change */
    for (i = 0; i < SETTING_MAX; i++)
    {
        if (Client_setup.settings[i] != new_settings[i])
        {
            Client_setup.settings[i] = new_settings[i];
            changed = true;
        }
    }

    /* Return */
    return changed;
}


/*
 * Client is ready to play call-back
 */
void client_ready(bool newchar)
{
    bool options[OPT_MAX];
    size_t opt;

    /* Save birth options for new characters */
    for (opt = 0; newchar && (opt < OPT_MAX); opt++)
        options[opt] = player->opts.opt[opt];

    /* Initialize default option values */
    init_options(player->opts.opt);

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

    /* Keep birth options for new characters */
    for (opt = 0; newchar && (opt < OPT_MAX); opt++)
    {
        const char *name = option_name(opt);

        if (name && strstr(name, "birth_"))
            player->opts.opt[opt] = options[opt];
    }

    /* Send event */
    Term_xtra(TERM_XTRA_REACT, use_graphics);

    gather_settings();

    /* Sneakily init command list */
    cmd_init();

    Send_options(true);
    Send_autoinscriptions();

    /* Send visual preferences */
    Net_verify();

    Setup.initialized = true;

    /* Send request for splash screen (MOTD) to read */
    Send_text_screen(TEXTFILE_MOTD, 0);
}


/*
 * Initialize everything, contact the server, and start the loop.
 */
void client_init(void)
{
    sockbuf_t ibuf;
    char status, num_types, expiry;
    int trycount;
    char host_name[NORMAL_WID], trymsg[NORMAL_WID];
    u16b conntype = CONNTYPE_PLAYER;
    char buffer[NORMAL_WID];
    DWORD nSize = NORMAL_WID;
    bool done = false;
    u16b num, max;
    u32b num_name;
    size_t i, j;
    struct keypress c;

    /* Initialize input hooks */
    textui_input_init();

    /* Initialise sound */
    init_sound();

    /* Set up the display handlers and things. */
    init_display();

    /* Load in basic data */
    init_arrays();

    /* Initialize player */
    init_player();

    /* Initialize minor arrays */
    init_minor();

    GetLocalHostName(host_name, NORMAL_WID);

    /* Default server host and port */
    server_port = conf_get_int("MAngband", "port", 18346);
    my_strcpy(server_name, conf_get_string("MAngband", "host", ""), sizeof(server_name));

    /* Read host/port from the command line */
    clia_read_string(server_name, sizeof(server_name), "host");
    clia_read_int(&server_port, "port");

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
            memset(&c, 0, sizeof(c));
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
            if (Socket != -1) done = true;
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
    Packet_printf(&ibuf, "%hu%c", (unsigned)current_version(), (int)beta_version());
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
    Packet_scanf(&ibuf, "%hu", &max);

    /* Check for error */
    switch (status)
    {
        case SUCCESS: break;

        /* The server didn't like us.... */
        case E_VERSION_OLD:
            quit("Your client will not work on that server (client version is too old).");
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
            quit("Your client will not work on that server (not a PWMAngband server).");
    }

    max_account_chars = max;
    char_num = num;
    char_name = NULL;
    char_expiry = NULL;
    if (char_num)
    {
        char_name = mem_zalloc(char_num * sizeof(char*));
        char_expiry = mem_zalloc(char_num * sizeof(char));
    }
    for (i = 0; i < (size_t)char_num; i++)
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

    /* Initialize the random name fragments */
    num_names = mem_zalloc(RANDNAME_NUM_TYPES * sizeof(u32b));
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


/*
 * Free player struct
 */
static void cleanup_player(void)
{
    int i;

    /* Free the things that are always initialised */
    mem_free(player->timed);
    if (player->upkeep)
    {
        mem_free(player->upkeep->inven);
        mem_free(player->upkeep->quiver);
    }
    mem_free(player->upkeep);
    player->upkeep = NULL;

    /* Free the things that are only there if there is a loaded player */
    mem_free(player->gear);
    mem_free(player->body.slots);

    /* PWMAngband */
    for (i = 0; player->scr_info && (i < Setup.max_row + ROW_MAP + 1); i++)
    {
        mem_free(player->scr_info[i]);
        mem_free(player->trn_info[i]);
    }
    mem_free(player->scr_info);
    mem_free(player->trn_info);
    for (i = 0; i < N_HISTORY_FLAGS; i++)
        mem_free(player->hist_flags[i]);
    mem_free(player->obj_aware);
    mem_free(player->kind_ignore);
    mem_free(player->kind_everseen);
    for (i = 0; player->ego_ignore_types && (i < z_info->e_max); i++)
        mem_free(player->ego_ignore_types[i]);
    mem_free(player->ego_ignore_types);
    mem_free(player->ego_everseen);

    mem_free(player->on_channel);

    /* Free the basic player struct */
    mem_free(player);
    player = NULL;
}


void cleanup_floor(void)
{
    int i;

    /* Paranoia */
    if (!floor_items) return;

    for (i = 0; i < z_info->floor_size; i++)
    {
        if (floor_items[i]) mem_free(floor_items[i]);
        floor_items[i] = NULL;
    }
}


/*
 * Free all the stuff initialised in init_angband()
 */
void cleanup_angband(void)
{
    int i;

    /* Free the menus */
    free_command_menu();

    cleanup_player();

    /* Free the floor items */
    cleanup_floor();
    mem_free(floor_items);

    event_remove_all_handlers();

    /* Free the current store */
    mem_free(current_store.stock);
    if (current_store.owner) string_free(current_store.owner->name);
    mem_free(current_store.owner);
    string_free(current_store.name);
    mem_free(store_names);

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
    mem_free(Client_setup.t_attr);
    mem_free(Client_setup.t_char);
    mem_free(Client_setup.flvr_x_attr);
    mem_free(Client_setup.flvr_x_char);
    mem_free(Client_setup.note_aware);

    /* Free the messages */
    messages_free();

    /* Free the info arrays */
    for (i = 0; k_info && (i < z_info->k_max); i++) string_free(k_info[i].name);
    mem_free(k_info);
    for (i = 0; e_info && (i < z_info->e_max); i++)
    {
        struct poss_item *poss, *pn;

        string_free(e_info[i].name);
        poss = e_info[i].poss_items;
        while (poss)
        {
            pn = poss->next;
            mem_free(poss);
            poss = pn;
        }
    }
    mem_free(e_info);
    cleanup_p_race();
    cleanup_realm();
    cleanup_class();
    cleanup_body();
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
    while (rb_info)
    {
        struct monster_base *rb = rb_info->next;

        string_free(rb_info->name);
        mem_free(rb_info);
        rb_info = rb;
    }
    for (i = 0; curses && (i < z_info->curse_max); i++)
    {
        string_free(curses[i].name);
        string_free(curses[i].desc);
    }
    mem_free(curses);
    for (i = 0; f_info && (i < z_info->f_max); i++) string_free(f_info[i].name);
    mem_free(f_info);
    for (i = 0; trap_info && (i < z_info->trap_max); i++) string_free(trap_info[i].desc);
    mem_free(trap_info);

    /* Free the format() buffer */
    vformat_kill();

    /* Free the directories */
    free_file_paths();
}


/*
 * Clean up UI
 */
void textui_cleanup(void)
{
    /* Cleanup any options menus */
    cleanup_options();

    keymap_free();
}
