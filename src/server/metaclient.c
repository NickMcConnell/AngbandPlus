/*
 * File: metaclient.c
 * Purpose: Meta client implementation
 *
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


/* Has the code been initialized? */
static bool meta_initialized = false;


/* Metaserver socket */
static int meta_fd = -1;


/* Local hostname */
static char meta_local_name[MSG_LEN];


/* Metaserver socket buffer */
static sockbuf_t meta_buf;


/*
 * Initialize the meta client code
 */
static void meta_init(void)
{
    /* If this is the first time called, initialize our hostname */
    if (!meta_initialized)
    {
        /* Never do this again */
        meta_initialized = true;

        /* Get our hostname */
        if (cfg_report_address)
            my_strcpy(meta_local_name, cfg_report_address, sizeof(meta_local_name));
        else if (cfg_bind_name)
            my_strcpy(meta_local_name, cfg_bind_name, sizeof(meta_local_name));
        else
            GetLocalHostName(meta_local_name, MSG_LEN);
    }
}


/*
 * Connect to the metaserver
 */
static void meta_connect(void)
{
    if ((meta_fd = CreateDgramSocket(0)) == -1)
        quit("Couldn't create meta-server Dgram socket");

    if (SetSocketNonBlocking(meta_fd, 1) == -1)
        quit("Can't make socket non-blocking");

    if (Sockbuf_init(&meta_buf, meta_fd, SERVER_SEND_SIZE,
        SOCKBUF_READ | SOCKBUF_WRITE | SOCKBUF_DGRAM) == -1)
    {
        quit("No memory for sockbuf buffer");
    }
}


/*
 * Send data to the metaserver
 */
static void meta_write(int flag)
{
    char buf_meta[16384];
    char temp[160];
    int k, num = 0;
    int bytes;

    /* Start with our address */
    if (cfg_mang_meta)
        strnfmt(buf_meta, sizeof(buf_meta), "%s:%d", meta_local_name, (int)cfg_tcp_port);
    else
    {
        strnfmt(buf_meta, sizeof(buf_meta), "<server url=\"%s\" port=\"%d\" protocol=\"2\"",
            meta_local_name, (int)cfg_tcp_port);
    }

    /* Hack -- if we're shutting down, don't send player list and version */
    if (flag & META_DIE)
    {
        /* Send address + whitespace, which metaserver recognizes as death report */
        if (cfg_mang_meta)
            my_strcat(buf_meta, " ", sizeof(buf_meta));
        else
            my_strcat(buf_meta, " death=\"true\"></server>", sizeof(buf_meta));
    }
    else
    {
        if (!cfg_mang_meta)
        {
            my_strcat(buf_meta, ">", sizeof(buf_meta));
            my_strcat(buf_meta, "<notes>", sizeof(buf_meta));
            my_strcat(buf_meta, "PWMAngband server", sizeof(buf_meta));
            my_strcat(buf_meta, "</notes>", sizeof(buf_meta));
        }

        /* Hack -- count players */
        for (k = 1; k <= NumPlayers; k++)
        {
            if (!(player_get(k)->dm_flags & DM_SECRET_PRESENCE)) num++;
        }

        /* 'Number of players' */
        if (cfg_mang_meta)
        {
            strnfmt(temp, sizeof(temp), " Number of players: %d ", num);
            my_strcat(buf_meta, temp, sizeof(buf_meta));
        }

        /* Scan the player list */
        if (num)
        {
            /* List player names */
            if (cfg_mang_meta) my_strcat(buf_meta, "Names: ", sizeof(buf_meta));

            for (k = 1; k <= NumPlayers; k++)
            {
                struct player *p = player_get(k);
                char safe[20];

                /* Hide dungeon master */
                if (p->dm_flags & DM_SECRET_PRESENCE) continue;

                player_safe_name(safe, sizeof(safe), p->name);

                /* Add an entry */
                if (cfg_mang_meta)
                {
                    my_strcat(buf_meta, safe, sizeof(buf_meta));
                    my_strcat(buf_meta, " ", sizeof(buf_meta));
                }
                else
                {
                    my_strcat(buf_meta, "<player>", sizeof(buf_meta));
                    my_strcat(buf_meta, safe, sizeof(buf_meta));
                    my_strcat(buf_meta, "</player>", sizeof(buf_meta));
                }
            }
        }

        if (!cfg_mang_meta)
            my_strcat(buf_meta, "<game>PWMAngband</game>", sizeof(buf_meta));

        /* Append the version number */
        if (cfg_mang_meta)
            strnfmt(temp, sizeof(temp), "Version: %s ", version_build(VB_BASE | VB_BUILD));
        else
            strnfmt(temp, sizeof(temp), "<version>%s</version></server>", version_build(VB_BASE));
        my_strcat(buf_meta, temp, sizeof(buf_meta));
    }

    /* If we haven't setup the meta connection yet, abort */
    if (meta_fd == -1) return;

    Sockbuf_clear(&meta_buf);
    Packet_printf(&meta_buf, "%S", buf_meta);

    /* Send data to metaserver */
    bytes = DgramSend(meta_fd, cfg_meta_address, cfg_meta_port, meta_buf.buf, meta_buf.len);
    if (bytes == -1) plog("Couldn't send info to meta-server!");
}


/*
 * Close the metaserver connection
 */
static void meta_close(void)
{
    if (meta_fd != -1) DgramClose(meta_fd);
}


/*
 * Called from Report_to_meta() in netserver.c
 */
void meta_report(int flag)
{
    if (flag & META_START)
    {
        meta_init();
        meta_connect();
    }
    meta_write(flag);
    if (flag & META_DIE)
        meta_close();
}
