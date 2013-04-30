/*
 * File: netclient.c
 * Purpose: The client side of the networking stuff
 *
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
#include "../common/spells.h"
#include "../common/tvalsval.h"
#include "c-cmds.h"
#include "game-event.h"
#include "netclient.h"
#include "prefs.h"


/* Client shutdown attempts to send a quit packet to the server when it exits  */
bool send_quit = TRUE;


static int conn_state; /* Similar to server's connp->state */
static u32b last_sent = 0, last_received = 0;
static u32b mticks = 0; /* Keeps track of time in milliseconds */
static bool request_redraw;
static sockbuf_t rbuf, wbuf, qbuf;
static int (*receive_tbl[256])(void), (*setup_tbl[256])(void);
static char talk_pend[MSG_LEN], initialized = 0;
static ang_file *fp;


/*** Utilities ***/


int Flush_queue(void)
{
    int len;

    if (!initialized) return 0;

    len = qbuf.len - (qbuf.ptr - qbuf.buf);

    if (Sockbuf_write(&rbuf, qbuf.ptr, len) != len)
    {
        errno = 0;
        plog("Can't copy queued data to buffer");
        return -1;
    }
    Sockbuf_clear(&qbuf);

    Net_packet();

    /* If a redraw has been requested, send the request */
    if (request_redraw)
    {
        Send_redraw();
        request_redraw = FALSE;
    }

    return 1;
}


/* Keep track of time in milliseconds */
static void updateTicks()
{
    SYSTEMTIME st;

    /* Retrieve the current system date and time */
    GetSystemTime(&st);

    /* Keep track of time in milliseconds */
    mticks = ((st.wHour * 60L + st.wMinute) * 60L + st.wSecond) * 1000L + st.wMilliseconds;

    /* Wrap every day */
    if ((mticks < last_sent) || (mticks < last_received))
        last_sent = last_received = 0;
}


static int Send_keepalive(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%lu", (unsigned)PKT_KEEPALIVE, last_sent)) <= 0)
        return n;

    return 1;
}


/* Write a keepalive packet to the output queue every second */
void do_keepalive()
{
    /* Keep track of time in milliseconds */
    updateTicks();

    /* Check to see if it has been 1 second since we last sent anything */
    if ((mticks - last_sent) > 1000)
    {
        if ((last_received < last_sent) && (conn_state == CONN_PLAYING))
        {
            lag_mark = 1000;
            p_ptr->redraw |= (PR_LAG);
        }
        last_sent = mticks;
        Send_keepalive();
    }
}


void check_term_resize(bool main_win, int *cols, int *rows)
{
    /* Window size bounds checking */
    if (main_win)
    {
        int dun_col = DUNGEON_WID * tile_width + COL_MAP + 1;
        int dun_row = DUNGEON_HGT * tile_height + ROW_MAP + 1;

        /* Minimum window size is 80x24 */
        if (*cols < NORMAL_WID) *cols = NORMAL_WID;
        if (*rows < NORMAL_HGT) *rows = NORMAL_HGT;

        /* Limit to server minimum while playing */
        if (Setup.initialized)
        {
            int min_col = Setup.min_col + COL_MAP + 1;
            int min_row = Setup.min_row + ROW_MAP + 1;

            if (*cols < min_col) *cols = min_col;
            if (*rows < min_row) *rows = min_row;
        }

        /* Maximum window size is max dungeon size */
        if (*cols > dun_col) *cols = dun_col;
        if (*rows > dun_row) *rows = dun_row;

        /* Limit to server maximum while playing */
        if (Setup.initialized)
        {
            int max_col = Setup.max_col * tile_width + COL_MAP + 1;
            int max_row = Setup.max_row * tile_height + ROW_MAP + 1;

            if (*cols > max_col) *cols = max_col;
            if (*rows > max_row) *rows = max_row;
        }

        /* Don't overflow */
        if (*cols > 255) *cols = 255;
        if (*rows > 255) *rows = 255;
    }
    else
    {
        /* Window size is 80x24 */
        *cols = NORMAL_WID;
        *rows = NORMAL_HGT;
    }
}


void net_term_resize(int cols, int rows, int max_rows)
{
    int dummy_cols;

    if (!Setup.initialized) return;

    /* Defaults */
    if (!cols && !rows && !max_rows)
    {
        cols = Term->wid;
        rows = Term->hgt;
        max_rows = Term->max_hgt;
    }
    dummy_cols = cols;

    /* Paranoia */
    check_term_resize(TRUE, &cols, &rows);
    check_term_resize(TRUE, &dummy_cols, &max_rows);

    /* Compact display */
    cols = cols - COL_MAP - 1;

    /* Status line */
    rows = rows - ROW_MAP - 1;

    /* Save */
    Client_setup.settings[SETTING_SCREEN_COLS] = cols;
    Client_setup.settings[SETTING_SCREEN_ROWS] = rows;
    Client_setup.settings[SETTING_TILE_WID] = tile_width;
    Client_setup.settings[SETTING_TILE_HGT] = tile_height;
    Client_setup.settings[SETTING_TILE_DISTORTED] = tile_distorted;
    Client_setup.settings[SETTING_MAX_HGT] = max_rows;

    /* Send */
    Send_options(TRUE);
}


/*** General network functions ***/


/* Forward declaration */
static void Receive_init(void);


/*
 * Process a packet.
 */
int Net_packet(void)
{
    int type, prev_type = 0, result;
    int (**ack_tbl)(void) = receive_tbl;
    char *old_ptr;

    if (conn_state == CONN_SETUP) ack_tbl = setup_tbl;

    /* Process all of the received client updates */
    while (rbuf.buf + rbuf.len > rbuf.ptr)
    {
        type = (*rbuf.ptr & 0xFF);

        if (ack_tbl[type] == NULL)
        {
            errno = 0;
#ifdef _DEBUG
            /* The player really doesn't need to know about this */
            plog_fmt("Received unknown packet type (%d, %d, %d), dropping",
                type, prev_type, conn_state);
#endif
            Sockbuf_clear(&rbuf);
            break;
        }
        else
        {
            old_ptr = rbuf.ptr;
            if ((result = (*ack_tbl[type])()) <= 0)
            {
                if (result == -1)
                {
                    if (type != PKT_QUIT)
                    {
                        errno = 0;
                        plog_fmt("Processing packet type (%d, %d) failed", type, prev_type);
                    }
                    Sockbuf_clear(&rbuf);
                    return -1;
                }
                if (result == -2)
                {
                    ack_tbl = ((conn_state == CONN_PLAYING)? receive_tbl: setup_tbl);
                    continue;
                }

                /* Check whether the socket buffer has advanced */
                if (rbuf.ptr == old_ptr)
                {
                    /* Return code 0 means that there wasn't enough data in the socket buffer */
                    if (result == 0) break;
                }

                /* Something weird may have happened, clear the socket buffer */
                Sockbuf_clear(&rbuf);
                break;
            }
        }
        prev_type = type;
    }
    return 0;
}


static int Send_verify_visual(int type)
{
    int n, i, size;
    byte *attr_ref;
    char *char_ref;

    switch (type)
    {
        case 0:
            size = flavor_max;
            attr_ref = Client_setup.flvr_x_attr;
            char_ref = Client_setup.flvr_x_char;
            break;
        case 1:
            size = z_info->f_max * FEAT_LIGHTING_MAX;
            attr_ref = NULL;
            char_ref = NULL;
            break;
        case 2:
            size = z_info->k_max;
            attr_ref = Client_setup.k_attr;
            char_ref = Client_setup.k_char;
            break;
        case 3:
            size = z_info->r_max;
            attr_ref = Client_setup.r_attr;
            char_ref = Client_setup.r_char;
            break;
        case 4:
            size = 128;
            attr_ref = Client_setup.tval_attr;
            char_ref = NULL;
            break;
        case 5:
            size = GF_MAX * BOLT_MAX;
            attr_ref = NULL;
            char_ref = NULL;
            break;
        default:
            return 0;
    }

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_VERIFY, type, size)) <= 0)
        return n;

    for (i = 0; i < size; i++)
    {
        char c = (char_ref? char_ref[i]: ' ');

        /* Hack -- Features */
        if (type == 1)
        {
            if ((n = Packet_printf(&wbuf, "%b%c",
                (unsigned)Client_setup.f_attr[i / FEAT_LIGHTING_MAX][i % FEAT_LIGHTING_MAX],
                (int)Client_setup.f_char[i / FEAT_LIGHTING_MAX][i % FEAT_LIGHTING_MAX])) <= 0)
                    return n;
        }

        /* Hack -- Special effects */
        else if (type == 5)
        {
            if ((n = Packet_printf(&wbuf, "%b%c",
                (unsigned)Client_setup.gf_attr[i / BOLT_MAX][i % BOLT_MAX],
                (int)Client_setup.gf_char[i / BOLT_MAX][i % BOLT_MAX])) <= 0)
                    return n;
        }

        else if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)attr_ref[i], (int)c)) <= 0)
            return n;
    }

    return 1;
}


/*
 * Send the first packet to the server with our name,
 * nick and display contained in it.
 */
int Net_verify()
{
    int i;

    /* There are 6 char/attr streams, go through all of them */
    for (i = 0; i < 6; i++) Send_verify_visual(i);

    return 0;
}


/*
 * Open the datagram socket and allocate the network data
 * structures like buffers.
 * Currently there are three different buffers used:
 * 1) wbuf is used only for sending packets (write/printf).
 * 2) rbuf is used for receiving packets in (read/scanf).
 * 3) qbuf is the queue buffer.
 */
int Net_init(int fd)
{
    int sock;

    Receive_init();

    sock = fd;

    wbuf.sock = sock;

    if (SetSocketNoDelay(sock, 1) == -1)
    {
        plog("Can't set TCP_NODELAY on socket");
        return -1;
    }

    if (SetSocketSendBufferSize(sock, CLIENT_SEND_SIZE + 256) == -1)
        plog_fmt("Can't set send buffer size to %d: error %d", CLIENT_SEND_SIZE + 256, errno);
    if (SetSocketReceiveBufferSize(sock, CLIENT_RECV_SIZE + 256) == -1)
        plog_fmt("Can't set receive buffer size to %d", CLIENT_RECV_SIZE + 256);

    /* Queue buffer, not a valid socket filedescriptor needed */
    if (Sockbuf_init(&qbuf, -1, CLIENT_RECV_SIZE, SOCKBUF_WRITE | SOCKBUF_READ | SOCKBUF_LOCK) == -1)
    {
        plog_fmt("No memory for queue buffer (%u)", CLIENT_RECV_SIZE);
        return -1;
    }

    /* Read buffer */
    if (Sockbuf_init(&rbuf, sock, CLIENT_RECV_SIZE, SOCKBUF_READ | SOCKBUF_WRITE) == -1)
    {
        plog_fmt("No memory for read buffer (%u)", CLIENT_RECV_SIZE);
        return -1;
    }

    /* Write buffer */
    if (Sockbuf_init(&wbuf, sock, CLIENT_SEND_SIZE, SOCKBUF_WRITE) == -1)
    {
        plog_fmt("No memory for write buffer (%u)", CLIENT_SEND_SIZE);
        return -1;
    }

    Setup.ready = Setup.wait = FALSE;

    /* Initialized */
    initialized = 1;

    /* Advance State */
    conn_state = CONN_SETUP;

    return 0;
}


/*
 * Cleanup all the network buffers and close the datagram socket.
 * Also try to send the server a quit packet if possible.
 */
void Net_cleanup(void)
{
    int sock = wbuf.sock;

    if (sock > 2)
    {
        char ch = PKT_QUIT;

        if (send_quit && (DgramWrite(sock, &ch, 1) != 1))
        {
            GetSocketError(sock);
            DgramWrite(sock, &ch, 1);
        }
        Term_xtra(TERM_XTRA_DELAY, 50);

        DgramClose(sock);
    }

    Sockbuf_cleanup(&rbuf);
    Sockbuf_cleanup(&wbuf);
    Sockbuf_cleanup(&qbuf);

    /*
     * Make sure that we won't try to write to the socket again,
     * after our connection has closed
     */
    wbuf.sock = -1;
}


/*
 * Flush the network output buffer if it has some data in it.
 * Called by the main loop before blocking on a select(2) call.
 */
int Net_flush(void)
{
    if (wbuf.len == 0)
    {
        wbuf.ptr = wbuf.buf;
        return 0;
    }
    if (Sockbuf_flush(&wbuf) == -1)
        return -1;
    Sockbuf_clear(&wbuf);
    return 1;
}


/*
 * Return the socket filedescriptor for use in a select(2) call.
 */
int Net_fd(void)
{
    if (!initialized)
        return -1;
    return rbuf.sock;
}


/*
 * Read packets from the net until there are no more available.
 */
int Net_input(void)
{
    int n;
    int netfd;

    netfd = Net_fd();

    /* Keep reading as long as we have something on the socket */
    while (SocketReadable(netfd))
    {
        n = Sockbuf_read(&rbuf);
        if (n == 0) quit("Server closed the connection");
        else if (n < 0) return n;
        else
        {
            n = Net_packet();

            /* Make room for more packets */
            Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);

            if (n == -1) return -1;
        }
    }

    return 1;
}


bool Net_Send(int Socket, sockbuf_t* ibuf)
{
    int bytes;

    /* Send the info */
    bytes = DgramWrite(Socket, ibuf->buf, ibuf->len);
    if (bytes == -1)
        return FALSE;

    return TRUE;
}


bool Net_WaitReply(int Socket, sockbuf_t* ibuf, int max_retries)
{
    int retries;

    /* Clear the socket buffer */
    Sockbuf_clear(ibuf);

    /* Listen for reply */
    for (retries = 0; retries < max_retries; retries++)
    {
        int bytes;

        /* Set timeout */
        SetTimeout(1, 0);

        /* Wait for info */
        if (!SocketReadable(Socket)) continue;

        /* Read reply */
        bytes = DgramRead(Socket, ibuf->buf, ibuf->size);
        if (bytes <= 0) continue;
        ibuf->len = bytes;

        break;
    }

    if (retries >= max_retries) return FALSE;

    return TRUE;
}


/*** Sending ***/


int Send_search(cmd_arg args[])
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_SEARCH)) <= 0)
        return n;

    return 1;
}


int Send_walk(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_WALK, dir)) <= 0)
        return n;

    return 1;
}  


int Send_jump(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_JUMP, dir)) <= 0)
        return n;

    return 1;
}


int Send_run(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_RUN, dir)) <= 0)
        return n;

    return 1;
}


int Send_drop(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    int amt = args[1].number;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_DROP, item, amt)) <= 0)
        return n;

    return 1;
}


int Send_drop_gold(s32b amt)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%ld", (unsigned)PKT_DROP_GOLD, amt)) <= 0)
        return n;

    return 1;
}


int Send_tunnel(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_TUNNEL, dir)) <= 0)
        return n;

    return 1;
}


int Send_pickup(cmd_arg args[])
{
    int n;
    byte squelch = (byte)args[0].number;

    if ((n = Packet_printf(&wbuf, "%b%b", (unsigned)PKT_PICKUP, (unsigned)squelch)) <= 0)
        return n;

    return 1;
}


int Send_quest(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_QUEST)) <= 0)
        return n;

    return 1;
}


int Send_toggle_search(cmd_arg args[])
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_SEARCH_MODE)) <= 0)
        return n;

    return 1;
}


int Send_rest(s16b resting)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_REST, (int)resting)) <= 0)
        return n;

    return 1;
}


int Send_go_up(cmd_arg args[])
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_GO_UP)) <= 0)
        return n;

    return 1;
}


int Send_go_down(cmd_arg args[])
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_GO_DOWN)) <= 0)
        return n;

    return 1;
}


int Send_open(cmd_arg args[])
{
    int n;
    int easy = OPT(easy_open);
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c%c", (unsigned)PKT_OPEN, dir, easy)) <= 0)
        return n;

    return 1;
}


int Send_close(cmd_arg args[])
{
    int n;
    int easy = OPT(easy_open);
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c%c", (unsigned)PKT_CLOSE, dir, easy)) <= 0)
        return n;

    return 1;
}


int Send_bash(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_BASH, dir)) <= 0)
        return n;

    return 1;
}      


int Send_alter(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_ALTER, dir)) <= 0)
        return n;

    return 1;
}


int Send_spike(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_SPIKE, dir)) <= 0)
        return n;

    return 1;
}


int Send_disarm(cmd_arg args[])
{
    int n;
    int easy = OPT(easy_open);
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c%c", (unsigned)PKT_DISARM, dir, easy)) <= 0)
        return n;

    return 1;
}


int Send_wield(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    int slot = args[1].number;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_WIELD, item, slot)) <= 0)
        return n;

    return 1;
}


int Send_take_off(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_TAKE_OFF, item)) <= 0)
        return n;

    return 1;
}


int Send_destroy(int item, bool des)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_DESTROY, item, (int)des)) <= 0)
        return n;

    return 1;
}


int Send_ignore(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_IGNORE)) <= 0)
        return n;

    return 1;
}


int Send_inscribe(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    char *buf = args[1].string;

    n = Packet_printf(&wbuf, "%b%hd%s", (unsigned)PKT_INSCRIBE, item, buf);
    string_free(buf);

    if (n <= 0) return n;
    return 1;
}


int Send_uninscribe(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_UNINSCRIBE, item)) <= 0)
        return n;

    return 1;
}


int Send_steal(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_STEAL, dir)) <= 0)
        return n;

    return 1;
}


int Send_quaff(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_QUAFF, item)) <= 0)
        return n;

    return 1;
}


int Send_read(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_READ, item)) <= 0)
        return n;

    return 1;
}


int Send_aim(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    int dir = args[1].direction;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_AIM_WAND, item, dir)) <= 0)
        return n;

    return 1;
}


int Send_use(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_USE, item)) <= 0)
        return n;

    return 1;
}


int Send_zap(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    int dir = args[1].direction;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_ZAP, item, dir)) <= 0)
        return n;

    return 1;
}


int Send_fill(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_FILL, item)) <= 0)
        return n;

    return 1;
}


int Send_eat(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_EAT, item)) <= 0)
        return n;

    return 1;
}


int Send_activate(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    int dir = args[1].direction;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_ACTIVATE, item, dir)) <= 0)
        return n;

    return 1;
}


int Send_target_interactive(int mode, keycode_t query)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%b%lu", (unsigned)PKT_TARGET, (unsigned)mode, query)) <= 0)
        return n;

    return 1;
}


int Send_target_closest(int mode)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%b", (unsigned)PKT_TARGET_CLOSEST, (unsigned)mode)) <= 0)
        return n;

    return 1;
}


int Send_msg(const char *message)
{
    int n;

    if (message && strlen(message))
        my_strcpy(talk_pend, message, sizeof(talk_pend));

    if (view_channel != p_ptr->main_channel)
    {
        /* Change channel */
        p_ptr->main_channel = view_channel;
        if ((n = Send_chan(channels[view_channel].name)) <= 0)
            return n;
    }

    if ((n = Packet_printf(&wbuf, "%b%S", (unsigned)PKT_MESSAGE, talk_pend)) <= 0)
        return n;

    return 1;
}


int Send_fire(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    int dir = args[1].direction;

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_FIRE, dir, item)) <= 0)
        return n;

    return 1;
}


int Send_fire_at_nearest(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_FIRE_AT_NEAREST)) <= 0)
        return n;

    return 1;
}


int Send_throw(int item, int dir)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_THROW, dir, item)) <= 0)
        return n;

    return 1;
}


int Send_item(int item)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_ITEM, item)) <= 0)
        return n;

    return 1;
}


int Send_gain(cmd_arg args[])
{
    int n;
    int book = args[0].item;
    int spell = args[1].number;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_GAIN, book, spell)) <= 0)
        return n;

    return 1;
}


int Send_cast(int book, int spell, int dir)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd%c", (unsigned)PKT_SPELL, book, spell, dir)) <= 0)
        return n;

    return 1;
}


int Send_pray(int book, int spell, int dir)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd%c", (unsigned)PKT_PRAY, book, spell, dir)) <= 0)
        return n;

    return 1;
}


int Send_ghost(int ability, int dir)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_GHOST, ability, dir)) <= 0)
        return n;

    return 1;
}


int Send_map(byte mode)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%b", (unsigned)PKT_MAP, (unsigned)mode)) <= 0)
        return n;

    return 1;
}


int Send_fullmap(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_FULLMAP)) <= 0)
        return n;

    return 1;
}


int Send_locate(int dir)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_LOCATE, dir)) <= 0)
        return n;

    return 1;
}


int Send_store_purchase(int item, int amt)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_PURCHASE, item,
            amt)) <= 0)
        return n;

    return 1;
}


int Send_store_examine(int item)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_STORE_EXAMINE, item)) <= 0)
        return n;

    return 1;
}


int Send_store_order(const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%s", (unsigned)PKT_STORE_ORDER, buf)) <= 0)
        return n;

    return 1;
}


int Send_store_sell(int item, int amt)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_SELL, item, amt)) <= 0)
        return n;

    return 1;
}


int Send_store_leave(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_STORE_LEAVE)) <= 0)
        return n;

    return 1;
}


int Send_store_confirm(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_STORE_CONFIRM)) <= 0)
        return n;

    return 1;
}


int Send_redraw(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_REDRAW)) <= 0)
        return n;

    /* Hack -- Clear the screen */
    Term_clear();

    return 1;
}


int Send_special_line(int type, int line)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_SPECIAL_LINE, type, line)) <= 0)
        return n;

    return 1;
}


int Send_party(s16b command, const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%s", (unsigned)PKT_PARTY, command, buf)) <= 0)
        return n;

    return 1;
}


int Send_purchase_house(int dir)
{
    int n, dummy = 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_PURCHASE, dir,
            dummy)) <= 0)
        return n;

    return 1;
}


int Send_suicide(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_SUICIDE)) <= 0)
        return n;

    return 1;
}


int Send_options(bool settings)
{
    int i, n;

    if ((n = Packet_printf(&wbuf, "%b%b", (unsigned)PKT_OPTIONS, (unsigned)settings)) <= 0)
        return n;

    if (settings)
    {
        /* Send each setting */
        for (i = 0; i < SETTING_MAX; i++)
        {
            n = Packet_printf(&wbuf, "%hd", (int)Client_setup.settings[i]);
            if (n <= 0)
                return n;
        }
    }

    /* Send each option */
    for (i = 0; i < OPT_MAX; i++)
    {
        n = Packet_printf(&wbuf, "%c", (int)Client_setup.options[i]);
        if (n <= 0)
            return n;
    }

    return 1;
}


int Send_master(s16b command, const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%s", (unsigned)PKT_MASTER, (int)command, buf)) <= 0)
        return n;

    return 1;
}


int Send_observe(cmd_arg args[])
{
    int n;
    int item = args[0].item;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_OBSERVE, item)) <= 0)
        return n;

    return 1;
}


int Send_clear()
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_CLEAR)) <= 0)
        return n;

    return 1;
}


int Send_pass(const char *newpass)
{
    int n;

    if (newpass && strlen(newpass))
    {
        if ((n = Packet_printf(&wbuf, "%b%s", (unsigned)PKT_CHANGEPASS, newpass)) <= 0)
            return n;
    }

    return 1;
}


int Send_symbol(const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%s", (unsigned)PKT_SYMBOL_QUERY, buf)) <= 0)
        return n;

    return 1;
}


int Send_objlist()
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_OBJLIST)) <= 0)
        return n;

    return 1;
}


int Send_monlist()
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_MONLIST)) <= 0)
        return n;

    return 1;
}


int Send_text_screen(int type, s32b off)
{
    int n;
    s32b offset = off;

    if (offset < 0) offset = 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%ld", (unsigned)PKT_TEXT_SCREEN, type, offset)) <= 0)
        return n;

    if ((type == TEXTFILE_MOTD) && (off == 0))
    {
        Setup.ready = FALSE;
        Setup.wait = TRUE;
    }

    return 1;
}


int Send_play(int mode)
{
    int n;

    /* Send */
    n = Packet_printf(&wbuf, "%b%b%s%s", (unsigned)PKT_PLAY, (unsigned)mode, nick, stored_pass);

    /* Set real player name */
    if (mode == 0)
    {
        int pos = strlen(nick);
        if (nick[pos - 1] == '=') nick[pos - 1] = '\0';
        else if (nick[pos - 1] == '-') nick[pos - 1] = '\0';
        else if (nick[pos - 1] == '+')
        {
            nick[pos - 1] = '\0';
            get_incarnation(1, nick, sizeof(nick));
        }
    }

    if (n <= 0) return n;
    return 1;
}


int Send_poly(int number)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_POLY, number)) <= 0)
        return n;

    return 1;
}


int Send_social(const char *buf, int dir)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%c%s", (unsigned)PKT_SOCIAL, dir, buf)) <= 0)
        return n;

    return 1;
}   


int Send_feeling(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_FEELING)) <= 0)
        return n;

    return 1;
}


int Send_breath(cmd_arg args[])
{
    int n;
    int dir = args[0].direction;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_BREATH, dir)) <= 0)
        return n;

    return 1;
}


int Send_mimic(int page, int spell, int dir)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd%c", (unsigned)PKT_MIMIC, page, spell, dir)) <= 0)
        return n;

    return 1;
}


int Send_chan(const char *channel)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%s", (unsigned)PKT_CHANNEL, channel)) <= 0)
        return n;

    return 1;
}


int Send_interactive(int type, keycode_t ch)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%c%lu", (unsigned)PKT_INTERACTIVE, type, ch)) <= 0)
        return n;

    return 1;
}


int Send_fountain(int item)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_FOUNTAIN, item)) <= 0)
        return n;

    return 1;
}


int Send_icky()
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_ICKY,
        p_ptr->screen_icky)) <= 0)
            return n;

    return 1;
}


int Send_center_map(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_CENTER)) <= 0)
        return n;

    return 1;
}


int Send_use_any(cmd_arg args[])
{
    int n;
    int item = args[0].item;
    int dir = args[1].direction;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_USE_ANY, item, dir)) <= 0)
        return n;

    return 1;
}


/*** Receiving ***/


/*
 * Decodes a (possibly) RLE-encoded stream of attr/char pairs
 *
 * See "rle_encode" for possible "mode" descriptions.
 *
 * Note -- if "lineref" is NULL, the packets will be read from   
 * the queue for no effect (usefull for discarding)
 */
static int rle_decode(sockbuf_t* buf, cave_view_type* lineref, int max_col, int mode,
    int* bytes_read)
{
    int x, i, nread;
    char c;
    byte a, n = 0;

    for (x = 0; x < max_col; x++)
    {
        /* Read the char/attr pair */
        nread = Packet_scanf(buf, "%c%b", &c, &a);
        if (nread <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(buf, *bytes_read);

            /* Packet isn't complete, graceful failure */
            return nread;
        }
        *bytes_read += 2;

        /* RLE-II */
        if ((mode == RLE_LARGE) && (c == -1))
        {
            /* Get the number of repetitions */
            n = a;

            /* Read the attr/char pair */
            nread = Packet_scanf(buf, "%c%b", &c, &a);
            if (nread <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(buf, *bytes_read);

                /* Packet isn't complete, graceful failure */
                return nread;
            }
            *bytes_read += 2;
        }

        /* RLE-I */
        else if ((mode == RLE_CLASSIC) && (a & 0x40))
        {
            /* First, clear the bit */
            a &= ~(0x40);

            /* Read the number of repetitions */
            nread = Packet_scanf(buf, "%b", &n);
            if (nread <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(buf, *bytes_read);

                /* Packet isn't complete, graceful failure */
                return nread;
            }
            *bytes_read += 1;
        }
        else
        {
            /* No RLE, just one instance */
            n = 1;
        }

        /* Draw a character n times */
        if (lineref)
        {
            for (i = 0; i < n; i++)
            {
                /* Memorize */
                lineref[x + i].a = a;
                lineref[x + i].c = c;
            }
        }

        /* Reset 'x' to the correct value */
        x += n - 1;
    }

    return 1;
}


/* Forward declarations */
static int Receive_text_screen(void);
static int Receive_basic_info(void);
static int Receive_death_cause(void);
static int Receive_winner(void);
static int Receive_char_info(void);
static int Receive_play(void);
static int Receive_struct_info(void);
static int Receive_char_info_conn(void);
static int Receive_end(void);
static int Receive_quit(void);
static int Receive_stat(void);
static int Receive_hp(void);
static int Receive_ac(void);
static int Receive_inven(void);
static int Receive_equip(void);
static int Receive_char_info(void);
static int Receive_various(void);
static int Receive_plusses(void);
static int Receive_lvl(void);
static int Receive_exp(void);
static int Receive_gold(void);
static int Receive_sp(void);
static int Receive_objflags(void);
static int Receive_history(void);
static int Receive_char(void);
static int Receive_message(void);
static int Receive_recall(void);
static int Receive_state(void);
static int Receive_title(void);
static int Receive_turn(void);
static int Receive_depth(void);
static int Receive_study(void);
static int Receive_food(void);
static int Receive_speed(void);
static int Receive_dtrap(void);
static int Receive_keepalive(void);
static int Receive_status(void);
static int Receive_item(void);
static int Receive_spell_info(void);
static int Receive_flush(void);
static int Receive_line_info(void);
static int Receive_fullmap(void);
static int Receive_special_other(void);
static int Receive_store(void);
static int Receive_store_info(void);
static int Receive_store_leave(void);
static int Receive_sell(void);
static int Receive_target_info(void);
static int Receive_sound(void);
static int Receive_special_line(void);
static int Receive_floor(void);
static int Receive_show_floor(void);
static int Receive_party(void);
static int Receive_skills(void);
static int Receive_pause(void);
static int Receive_cursor(void);
static int Receive_monster_health(void);
static int Receive_spell_desc(void);
static int Receive_poly(void);
static int Receive_weight(void);
static int Receive_channel(void);
static int Receive_term_info(void);
static int Receive_purchase(void);
static int Receive_confirm(void);
static int Receive_char_dump(void);
static int Receive_quiver_size(void);
static int Receive_player_pos(void);


/*
 * Initialize the function dispatch tables.
 * There are two tables.  One for the semi-important unreliable
 * data like frame updates.
 * The other one is for the reliable data stream, which is
 * received as part of the unreliable data packets.
 */
static void Receive_init(void)
{
    int i;

    for (i = 0; i < 256; i++)
    {
        receive_tbl[i]              = NULL;
        setup_tbl[i]                = NULL;
    }

    setup_tbl[PKT_PLAY]             = Receive_play;
    setup_tbl[PKT_QUIT]             = Receive_quit;
    setup_tbl[PKT_TEXT_SCREEN]      = Receive_text_screen;
    setup_tbl[PKT_BASIC_INFO]       = Receive_basic_info;
    setup_tbl[PKT_END]              = Receive_end;
    setup_tbl[PKT_KEEPALIVE]        = Receive_keepalive;
    setup_tbl[PKT_STRUCT_INFO]      = Receive_struct_info;
    setup_tbl[PKT_CHAR_INFO]        = Receive_char_info_conn;
    setup_tbl[PKT_CHAR_DUMP]        = Receive_char_dump;

    receive_tbl[PKT_QUIT]           = Receive_quit;
    receive_tbl[PKT_TEXT_SCREEN]    = Receive_text_screen;
    receive_tbl[PKT_DEATH_CAUSE]    = Receive_death_cause;
    receive_tbl[PKT_WINNER]         = Receive_winner;
    receive_tbl[PKT_END]            = Receive_end;
    receive_tbl[PKT_KEEPALIVE]      = Receive_keepalive;
    receive_tbl[PKT_LEV]            = Receive_lvl;
    receive_tbl[PKT_WEIGHT]         = Receive_weight;
    receive_tbl[PKT_PLUSSES]        = Receive_plusses;
    receive_tbl[PKT_AC]             = Receive_ac;
    receive_tbl[PKT_EXP]            = Receive_exp;
    receive_tbl[PKT_GOLD]           = Receive_gold;
    receive_tbl[PKT_HP]             = Receive_hp;
    receive_tbl[PKT_SP]             = Receive_sp;
    receive_tbl[PKT_CHAR_INFO]      = Receive_char_info;
    receive_tbl[PKT_VARIOUS]        = Receive_various;
    receive_tbl[PKT_STAT]           = Receive_stat;
    receive_tbl[PKT_HISTORY]        = Receive_history;
    receive_tbl[PKT_INVEN]          = Receive_inven;
    receive_tbl[PKT_EQUIP]          = Receive_equip;
    receive_tbl[PKT_TITLE]          = Receive_title;
    receive_tbl[PKT_TURN]           = Receive_turn;
    receive_tbl[PKT_DEPTH]          = Receive_depth;
    receive_tbl[PKT_FOOD]           = Receive_food;
    receive_tbl[PKT_STATUS]         = Receive_status;
    receive_tbl[PKT_RECALL]         = Receive_recall;
    receive_tbl[PKT_STATE]          = Receive_state;
    receive_tbl[PKT_LINE_INFO]      = Receive_line_info;
    receive_tbl[PKT_SPEED]          = Receive_speed;
    receive_tbl[PKT_STUDY]          = Receive_study;
    receive_tbl[PKT_QUIVER_SIZE]    = Receive_quiver_size;
    receive_tbl[PKT_SHOW_FLOOR]     = Receive_show_floor;
    receive_tbl[PKT_MESSAGE]        = Receive_message;
    receive_tbl[PKT_CHAR]           = Receive_char;
    receive_tbl[PKT_SPELL_INFO]     = Receive_spell_info;
    receive_tbl[PKT_FLOOR]          = Receive_floor;
    receive_tbl[PKT_SPECIAL_OTHER]  = Receive_special_other;
    receive_tbl[PKT_STORE]          = Receive_store;
    receive_tbl[PKT_STORE_INFO]     = Receive_store_info;
    receive_tbl[PKT_TARGET_INFO]    = Receive_target_info;
    receive_tbl[PKT_SOUND]          = Receive_sound;
    receive_tbl[PKT_MINI_MAP]       = Receive_line_info;
    receive_tbl[PKT_SKILLS]         = Receive_skills;
    receive_tbl[PKT_PAUSE]          = Receive_pause;
    receive_tbl[PKT_MONSTER_HEALTH] = Receive_monster_health;
    receive_tbl[PKT_ITEM]           = Receive_item;
    receive_tbl[PKT_SELL]           = Receive_sell;
    receive_tbl[PKT_PARTY]          = Receive_party;
    receive_tbl[PKT_SPECIAL_LINE]   = Receive_special_line;
    receive_tbl[PKT_FULLMAP]        = Receive_fullmap;
    receive_tbl[PKT_POLY]           = Receive_poly;
    receive_tbl[PKT_PURCHASE]       = Receive_purchase;
    receive_tbl[PKT_STORE_LEAVE]    = Receive_store_leave;
    receive_tbl[PKT_STORE_CONFIRM]  = Receive_confirm;
    receive_tbl[PKT_FLUSH]          = Receive_flush;
    receive_tbl[PKT_CURSOR]         = Receive_cursor;
    receive_tbl[PKT_OBJFLAGS]       = Receive_objflags;
    receive_tbl[PKT_SPELL_DESC]     = Receive_spell_desc;
    receive_tbl[PKT_DTRAP]          = Receive_dtrap;
    receive_tbl[PKT_CHANNEL]        = Receive_channel;
    receive_tbl[PKT_TERM]           = Receive_term_info;
    receive_tbl[PKT_PLAYER]         = Receive_player_pos;
}


/*
 * Receive the end of a new frame update packet,
 * which should contain the same loops number
 * as the frame head.  If this terminating packet
 * is missing then the packet is corrupt or incomplete.
 */
static int Receive_end(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    return 1;
}


/* Ready to play */
static int Receive_play(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    conn_state = CONN_PLAYING;

    return -2;
}


static int Receive_text_screen(void)
{
    int n, i;
    byte ch;
    s16b type;
    s32b off, len;
    int bytes_read;

    len = off = 0;

    if ((n = Packet_scanf(&rbuf, "%b%hd%ld%ld", &ch, &type, &len, &off)) <= 0)
        return n;
    bytes_read = 11;

    for (i = 0; i < len; i++)
    {
        if ((n = Packet_scanf(&rbuf, "%c", &Setup.text_screen[type][off + i])) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read += 1;
    }

    if (len == 0)
    {
        /* Splash screen (MOTD) */
        if (type == TEXTFILE_MOTD)
        {
            Setup.ready = TRUE;

            if (conn_state == CONN_SETUP)
            {
                if (Setup.wait)
                {
                    Send_play(1);
                    show_splashscreen();
                    full_icky_screen = conf_get_int("MAngband", "FullIckyScreen", 0);
                    Setup.wait = FALSE;

                    /* Send request for tombstone to read */
                    Send_text_screen(TEXTFILE_TOMB, 0);
                }
            }
        }

        /* Tombstone */
        else if (type == TEXTFILE_TOMB)
        {
            /* Send request for winner crown to read */
            Send_text_screen(TEXTFILE_CRWN, 0);
        }
    }
    else
    {
        /* Request continuation */
        Send_text_screen(type, off + i);
    }

    return 1;
}


static int Receive_basic_info(void)
{
    int n;
    byte ch;

    /* Clear any old info */
    Setup.frames_per_second = Setup.min_col = Setup.min_row = Setup.max_col = Setup.max_row = 0;

    if ((n = Packet_scanf(&rbuf, "%b%hd%b%b%b%b", &ch, &Setup.frames_per_second, &Setup.min_col,
        &Setup.min_row, &Setup.max_col, &Setup.max_row)) <= 0)
    {
        return n;
    }

    return 1;
}


static int Receive_death_cause(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%s%hd%ld%ld%hd%s%s", &ch, p_ptr->death_info.title,
        &p_ptr->death_info.lev, &p_ptr->death_info.exp, &p_ptr->death_info.au,
        &p_ptr->death_info.depth, p_ptr->death_info.died_from, p_ptr->death_info.ctime)) <= 0)
    {
        return n;
    }

    print_tomb();

    return 1;
}


static int Receive_winner(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0) return n;

    display_winner();

    return 1;
}


static int Receive_struct_info(void)
{
    byte ch;
    int i, n, j;
    char typ;
    u16b max;
    char name[NORMAL_WID];
    u32b wield, total;
    byte pflag;
    int bytes_read;

    typ = max = wield = total = 0;

    if ((n = Packet_scanf(&rbuf, "%b%c%hu%lu%lu", &ch, &typ, &max, &wield, &total)) <= 0)
        return n;
    bytes_read = 12;

    /* Which struct? */
    switch (typ)
    {
        /* Various Limits */
        case STRUCT_INFO_LIMITS:
        {
            u16b e_max, k_max, r_max, f_max;

            if ((n = Packet_scanf(&rbuf, "%hu%hu%hu%hu%hu%hu", &e_max, &v_max,
                &k_max, &r_max, &f_max, &flavor_max)) <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(&rbuf, bytes_read);

                /* Packet isn't complete, graceful failure */
                return n;
            }
            bytes_read += 12;

            /* z_info */
            z_info = &z_info_struct;

            z_info->e_max = e_max;

            /* k_info */
            z_info->k_max = k_max;
            Client_setup.k_attr = C_ZNEW(z_info->k_max, byte);
            Client_setup.k_char = C_ZNEW(z_info->k_max, char);

            /* r_info */
            z_info->r_max = r_max;
            Client_setup.r_attr = C_ZNEW(z_info->r_max, byte);
            Client_setup.r_char = C_ZNEW(z_info->r_max, char);

            /* f_info */
            z_info->f_max = f_max;
            Client_setup.f_attr = C_ZNEW(z_info->f_max, byte_lit);
            Client_setup.f_char = C_ZNEW(z_info->f_max, char_lit);

            /* Flavors */
            Client_setup.flvr_x_attr = C_ZNEW(flavor_max, byte);
            Client_setup.flvr_x_char = C_ZNEW(flavor_max, char);

            break;
        }

        /* Player Races */
        case STRUCT_INFO_RACE:
        {
            s16b r_adj, r_skills, r_exp;
            byte ridx, r_mhp, infra, flag;
            u16b choice;

            races = NULL;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct player_race *r;

                if ((n = Packet_scanf(&rbuf, "%b%s", &ridx, name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += strlen(name) + 2;

                r = mem_zalloc(sizeof(*r));
                r->ridx = ridx;
                r->name = string_make(name);

                /* Transfer other fields here */
                for (j = 0; j < A_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd", &r_adj)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(r->name);
                        mem_free(r);
                        return n;
                    }
                    bytes_read += 2;

                    r->r_adj[j] = r_adj;
                }
                for (j = 0; j < SKILL_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd", &r_skills)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(r->name);
                        mem_free(r);
                        return n;
                    }
                    bytes_read += 2;

                    r->r_skills[j] = r_skills;
                }
                if ((n = Packet_scanf(&rbuf, "%b%hd%b%hu", &r_mhp, &r_exp, &infra,
                    &choice)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    string_free(r->name);
                    mem_free(r);
                    return n;
                }
                bytes_read += 6;
                for (j = 0; j < PF_SIZE; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%b", &pflag)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(r->name);
                        mem_free(r);
                        return n;
                    }
                    bytes_read += 1;

                    r->pflags[j] = pflag;
                }
                for (j = 0; j < OF_SIZE; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%b", &flag)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(r->name);
                        mem_free(r);
                        return n;
                    }
                    bytes_read += 1;

                    r->flags[j] = flag;
                }

                r->r_mhp = r_mhp;
                r->r_exp = r_exp;
                r->infra = infra;
                r->choice = choice;
                r->next = races;
                races = r;
            }

            break;
        }

        /* Player Classes */
        case STRUCT_INFO_CLASS:
        {
            s16b c_adj, c_skills, c_exp;
            byte cidx, c_mhp, spell_book;

            classes = NULL;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct player_class *c;

                if ((n = Packet_scanf(&rbuf, "%b%s", &cidx, name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += strlen(name) + 2;

                c = mem_zalloc(sizeof(*c));
                c->cidx = cidx;
                c->name = string_make(name);

                /* Transfer other fields here */
                for (j = 0; j < A_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd", &c_adj)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += 2;

                    c->c_adj[j] = c_adj;
                }
                for (j = 0; j < SKILL_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd", &c_skills)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += 2;

                    c->c_skills[j] = c_skills;
                }
                if ((n = Packet_scanf(&rbuf, "%b%hd%b", &c_mhp, &c_exp, &spell_book)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    string_free(c->name);
                    mem_free(c);
                    return n;
                }
                bytes_read += 4;
                for (j = 0; j < PF_SIZE; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%b", &pflag)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += 1;

                    c->pflags[j] = pflag;
                }

                c->c_mhp = c_mhp;
                c->c_exp = c_exp;
                c->spell_book = spell_book;
                c->next = classes;
                classes = c;
            }

            break;
        }

        /* Inventory Slots */
        case STRUCT_INFO_INVEN:
        {
            byte i_pack, i_bow, i_left, i_light, q_start, m_stack;

            /* Size */
            inven_total = (byte)max;
            inven_wield = (byte)wield;
            all_inven_total = (byte)total;

            /* Alloc */
            eq_name = C_ZNEW(all_inven_total - inven_wield, char*);
            for (i = 0; i < all_inven_total - inven_wield; i++)
                eq_name[i] = C_ZNEW(NORMAL_WID, char);
            inventory_name = C_ZNEW(all_inven_total, char*);
            for (i = 0; i < all_inven_total; i++)
                inventory_name[i] = C_ZNEW(NORMAL_WID, char);
            p_ptr->inventory = C_ZNEW(all_inven_total, object_type);

            /* Read extra */
            if ((n = Packet_scanf(&rbuf, "%b%b%b%b%b%b", &i_pack, &i_bow, &i_left, &i_light,
                &q_start, &m_stack)) <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(&rbuf, bytes_read);

                /* Packet isn't complete, graceful failure */
                return n;
            }
            bytes_read += 6;

            inven_pack = i_pack;
            inven_bow = i_bow;
            inven_left = i_left;
            inven_light = i_light;
            quiver_start = q_start;
            max_stack_size = m_stack;

            /* Fill */
            for (i = 0; i < all_inven_total - inven_wield; i++)
            {
                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += strlen(name) + 1;

                my_strcpy(eq_name[i], name, NORMAL_WID);
            }

            break;
        }  

        /* Socials */
        case STRUCT_INFO_SOCIALS:
        {
            byte target;

            /* Alloc */
            soc_info = C_ZNEW(max, social_type);
            z_info->soc_max = max;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                social_type *s_ptr = &soc_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += strlen(name) + 1;

                s_ptr->name = string_make(name);

                /* Transfer other fields here */
                if ((n = Packet_scanf(&rbuf, "%b", &target)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += 1;

                s_ptr->target = target;
            }

            break;
        }

        /* Object kinds */
        case STRUCT_INFO_KINDS:
        {
            byte tval, sval;
            u32b kidx;

            /* Alloc */
            k_info = C_ZNEW(max, object_kind);

            /* Fill */
            for (i = 0; i < max; i++)
            {
                object_kind *k_ptr = &k_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += strlen(name) + 1;

                if (strlen(name)) k_ptr->name = string_make(name);

                /* Transfer other fields here */
                if ((n = Packet_scanf(&rbuf, "%b%b%lu", &tval, &sval, &kidx)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += 6;

                k_ptr->tval = tval;
                k_ptr->sval = sval;
                k_ptr->kidx = kidx;
            }

            break;
        }  

        /* Hints */
        case STRUCT_INFO_HINTS:
        {
            hints = NULL;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct hint *h;

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += strlen(name) + 1;

                h = mem_zalloc(sizeof(*h));
                h->hint = string_make(name);
                h->next = hints;
                hints = h;
            }

            break;
        }

        /* Monster races */
        case STRUCT_INFO_RINFO:
        {
            /* Alloc */
            r_info = C_ZNEW(max, monster_race);

            /* Fill */
            for (i = 0; i < max; i++)
            {
                monster_race *r_ptr = &r_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += strlen(name) + 1;

                if (strlen(name)) r_ptr->name = string_make(name);
            }

            break;
        }
    }

    return 1;
}


static int Send_char_info()
{
    int n, i;
    unsigned int ridx = p_ptr->race? p_ptr->race->ridx: 0;
    unsigned int cidx = p_ptr->clazz? p_ptr->clazz->cidx: 0;

    if ((n = Packet_printf(&wbuf, "%b%b%b%b", (unsigned)PKT_CHAR_INFO,
        (unsigned)ridx, (unsigned)cidx, (unsigned)p_ptr->psex)) <= 0)
            return n;

    /* Roller */
    for (i = 0; i <= A_MAX; i++)
    {
        n = Packet_printf(&wbuf, "%hd", (int)stat_roll[i]);
        if (n <= 0) return n;
    }

    return 1;
}


static int Receive_char_info_conn(void)
{
    int n;
    byte ch;
    byte mode, ridx, cidx, psex;

    /* Clear any old info */
    mode = ridx = cidx = psex = 0;

    if ((n = Packet_scanf(&rbuf, "%b%b%b%b%b", &ch, &mode, &ridx, &cidx, &psex)) <= 0)
        return n;

    /* No character */
    if (mode == 0)
    {
        char buf[MSG_LEN];

        /* We need to load the basic pref file for key mappings during character creation */
        my_strcpy(buf, "pref.prf", sizeof(buf));
        process_pref_file(buf, FALSE, FALSE);

        /* Create a character */
        player_birth();
        Send_char_info();
    }

    /* Set info */
    p_ptr->race = player_id2race(ridx);
    p_ptr->clazz = player_id2class(cidx);
    p_ptr->psex = psex;

    /* Set pointers */
    p_ptr->sex = &sex_info[p_ptr->psex];

    /* Copy his name */
    my_strcpy(p_ptr->name, nick, sizeof(p_ptr->name));

    /* Hack -- Assume ready */
    if (Setup.frames_per_second && mode)
        client_ready();

    return 1;
}


static int Receive_quit(void)
{
    char pkt;
    char reason[NORMAL_WID];

    /* Redraw stuff before quitting to show the cause of death */
    redraw_stuff();
    Term_fresh();

    if (Packet_scanf(&rbuf, "%c", &pkt) != 1)
    {
        errno = 0;
        plog("Can't read quit packet");
    }
    else
    {
        if (Packet_scanf(&rbuf, "%s", reason) <= 0)
            my_strcpy(reason, "unknown reason", sizeof(reason));
        errno = 0;
        quit_fmt("Quitting: %s", reason);
    }
    return -1;
}


static int Receive_stat(void)
{
    int n;
    byte ch;
    char stat;
    s16b stat_top, stat_use, stat_max, stat_add, stat_cur;

    if ((n = Packet_scanf(&rbuf, "%b%c%hd%hd%hd%hd%hd", &ch, &stat, &stat_top, &stat_use, &stat_max,
        &stat_add, &stat_cur)) <= 0)
            return n;

    p_ptr->state.stat_top[(int)stat] = stat_top;
    p_ptr->state.stat_use[(int)stat] = stat_use;
    p_ptr->stat_max[(int)stat] = stat_max;
    p_ptr->state.stat_add[(int)stat] = stat_add;
    p_ptr->stat_cur[(int)stat] = stat_cur;

    /* Redraw */
    p_ptr->redraw |= (PR_STATS);

    return 1;
}


static int Receive_hp(void)
{
    int n;
    byte ch;
    s16b max, cur;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &max, &cur)) <= 0)
        return n;

    p_ptr->mhp = max;
    p_ptr->chp = cur;

    /* Redraw */
    p_ptr->redraw |= (PR_HP);

    return 1;
}


static int Receive_ac(void)
{
    int n;
    byte ch;
    s16b base, plus;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &base, &plus)) <= 0)
        return n;

    p_ptr->state.dis_ac = base;
    p_ptr->state.dis_to_a = plus;

    p_ptr->redraw |= (PR_ARMOR);

    return 1;
}


static int Receive_items(int offset)
{
    int n;
    byte ch;
    char pos;
    byte attr, tval, act, sval, fuel, fail;
    s16b wgt, amt, slot;
    s32b price;
    char name[NORMAL_WID];
    object_type *o_ptr;

    if ((n = Packet_scanf(&rbuf, "%b%c%b%hd%ld%hd%b%b%b%b%b%hd%s", &ch, &pos, &attr, &wgt, &price,
        &amt, &tval, &sval, &act, &fuel, &fail, &slot, name)) <= 0)
    {
        return n;
    }

    /* Get the item (in the pack) */
    o_ptr = &p_ptr->inventory[pos - 'a' + offset];

    /* Item info used by the client */
    o_ptr->tval = tval;
    o_ptr->sval = sval;
    o_ptr->weight = wgt;
    o_ptr->number = amt;
    o_ptr->askprice = price;
    o_ptr->kind = (tval? lookup_kind(tval, sval): NULL);

    /* Hack -- Extra information used by the client */
    o_ptr->info_xtra.attr = attr;
    o_ptr->info_xtra.act = act;
    o_ptr->info_xtra.fuel = fuel;
    o_ptr->info_xtra.fail = fail;
    o_ptr->info_xtra.slot = slot;
    o_ptr->info_xtra.index = pos - 'a' + offset;

    my_strcpy(inventory_name[pos - 'a' + offset], name, NORMAL_WID);

    /* Redraw */
    p_ptr->redraw |= (offset? PR_EQUIP: PR_INVEN);

    return 1;
}


static int Receive_inven(void)
{
    return Receive_items(0);
}


static int Receive_equip(void)
{
    return Receive_items(inven_wield);
}


static int Receive_char_info(void)
{
    int n;
    byte ch;
    byte ridx, cidx, psex;

    /* Clear any old info */
    ridx = cidx = psex = 0;

    if ((n = Packet_scanf(&rbuf, "%b%b%b%b", &ch, &ridx, &cidx, &psex)) <= 0)
        return n;

    /* Set info */
    p_ptr->race = player_id2race(ridx);
    p_ptr->clazz = player_id2class(cidx);
    p_ptr->psex = psex;

    /* Set pointers */
    p_ptr->sex = &sex_info[p_ptr->psex];

    /* Copy his name */
    my_strcpy(p_ptr->name, nick, sizeof(p_ptr->name));

    /* Redraw */
    p_ptr->redraw |= (PR_MISC);

    return 1;
}


static int Receive_various(void)
{
    int n;
    byte ch;
    s16b hgt, wgt, age, sc;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%hd", &ch, &hgt, &wgt, &age, &sc)) <= 0)
        return n;

    p_ptr->ht = hgt;
    p_ptr->wt = wgt;
    p_ptr->age = age;
    p_ptr->sc = sc;

    /* Redraw */
    p_ptr->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_plusses(void)
{
    int n;
    byte ch;
    s16b fhit, fdam, mhit, mdam, shit, sdam;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%hd%hd%hd", &ch,
            &fhit, &fdam, &mhit, &mdam, &shit, &sdam)) <= 0)
        return n;

    p_ptr->state.dis_to_h = fhit;
    p_ptr->state.dis_to_d = fdam;
    dis_to_mhit = mhit;
    dis_to_mdam = mdam;
    dis_to_shit = shit;
    dis_to_sdam = sdam;

    /* Redraw */
    p_ptr->redraw |= (PR_PLUSSES);

    return 1;
}


static int Receive_lvl(void)
{
    int n;
    byte ch;
    s16b lev, mlev;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &lev, &mlev)) <= 0)
        return n;

    p_ptr->lev = lev;
    p_ptr->max_lev = mlev;

    /* Redraw */
    p_ptr->redraw |= (PR_LEV);

    return 1;
}   


static int Receive_exp(void)
{
    int n;
    byte ch;
    s32b max, cur;
    s16b expfact;

    if ((n = Packet_scanf(&rbuf, "%b%ld%ld%hd", &ch, &max, &cur, &expfact)) <= 0)
        return n;

    p_ptr->max_exp = max;
    p_ptr->exp = cur;
    p_ptr->expfact = expfact;

    p_ptr->redraw |= (PR_EXP);

    return 1;
}


static int Receive_gold(void)
{
    int n;
    byte ch;
    s32b gold;

    if ((n = Packet_scanf(&rbuf, "%b%ld", &ch, &gold)) <= 0)
        return n;

    p_ptr->au = gold;

    if (shopping) store_prt_gold();

    p_ptr->redraw |= (PR_GOLD);

    return 1;
}


static int Receive_sp(void)
{
    int n;
    byte ch;
    s16b max, cur;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &max, &cur)) <= 0)
        return n;

    p_ptr->msp = max;
    p_ptr->csp = cur;

    /* Redraw */
    p_ptr->redraw |= (PR_MANA);

    return 1;
}


static int Receive_objflags()
{
    byte ch;
    int n;
    s16b y;
    int bytes_read;

    if ((n = Packet_scanf(&rbuf, "%b%hd", &ch, &y)) <= 0) return n;
    bytes_read = 3;

    n = rle_decode(&rbuf, p_ptr->hist_flags[y], RES_COLS, DUNGEON_RLE_MODE(p_ptr),
        &bytes_read);
    if (n <= 0) return n;

    /* Redraw after last line is received */
    if (y == N_HISTORY_FLAGS - 1) p_ptr->redraw |= (PR_EQUIP);

    return 1;
}  


static int Receive_history(void)
{
    int n;
    byte ch;
    s16b line;
    char buf[NORMAL_WID];

    if ((n = Packet_scanf(&rbuf, "%b%hd%s", &ch, &line, buf)) <= 0)
        return n;

    my_strcpy(p_ptr->history[line], buf, sizeof(p_ptr->history[0]));

    /* Redraw */
    p_ptr->redraw |= (PR_OTHER);

    return 1;
}


/*
 * Mentally draw an attr/char at a given location
 *
 * Given location and values may or not be valid.
 */
static void Term_queue_char_safe(int x, int y, byte a, char c, byte ta, char tc)
{
    int w = Term->wid;
    int h = Term->hgt;

    /* Verify location */
    if ((x < 0) || (x >= w)) return;
    if ((y < 0) || (y >= h)) return;

    /* Queue it for later */
    Term_queue_char(Term, x, y, a, c, ta, tc);
}


/*
 * Queue a large-sized tile
 *
 * Given location and values may or not be valid.
 */
static void Term_big_queue_char_safe(int x, int y, byte a, char c, byte a1, char c1)
{
    int hor, vert;

    /* No tall skinny tiles */
    if (tile_width > 1)
    {
        /* Horizontal first */
        for (hor = 0; hor < tile_width; hor++)
        {
            /* Queue dummy character */
            if (hor != 0)
            {
                if (a & 0x80)
                    Term_queue_char_safe(x + hor, y, 255, -1, 0, 0);
                else
                    Term_queue_char_safe(x + hor, y, TERM_WHITE, ' ', a1, c1);
            }

            /* Now vertical */
            for (vert = 1; vert < tile_height; vert++)
            {
                /* Queue dummy character */
                if (a & 0x80)
                    Term_queue_char_safe(x + hor, y + vert, 255, -1, 0, 0);
                else
                    Term_queue_char_safe(x + hor, y + vert, TERM_WHITE, ' ', a1, c1);
            }
        }
    }
    else
    {
        /* Only vertical */
        for (vert = 1; vert < tile_height; vert++)
        {
            /* Queue dummy character */
            if (a & 0x80)
                Term_queue_char_safe(x, y + vert, 255, -1, 0, 0);
            else
                Term_queue_char_safe(x, y + vert, TERM_WHITE, ' ', a1, c1);
        }
    }
}


static int Receive_char(void)
{
    int n;
    byte ch;
    byte x, y, x_off;
    char c, tcp;
    byte a, tap;
    bool draw = TRUE;
    int bytes_read;

    tap = tcp = c = a = x = y = 0;

    if ((n = Packet_scanf(&rbuf, "%b%b%b%b%c", &ch, &x, &y, &a, &c)) <= 0)
        return n;
    bytes_read = 5;

    /* Hack -- Use ANOTHER terminal */
    if ((n = p_ptr->remote_term) != NTERM_WIN_OVERHEAD)
    {
        /* Only update the minimap window */
        if (n == NTERM_WIN_MAP)
        {
            if (y > last_remote_line[n]) last_remote_line[n] = y;
            remote_info[n][y][x].a = a;
            remote_info[n][y][x].c = c;

            event_signal_point(EVENT_MAP, x, y);
        }

        return 1;
    }

    p_ptr->scr_info[y][x].a = a;
    p_ptr->scr_info[y][x].c = c;

    if (p_ptr->use_graphics)
    {
        if ((n = Packet_scanf(&rbuf, "%b%c", &tap, &tcp)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read += 2;

        p_ptr->trn_info[y][x].a = tap;
        p_ptr->trn_info[y][x].c = tcp;
    }

    /* Hack: Manipulate offset */
    x_off = x + COL_MAP;

    if (p_ptr->screen_icky || section_icky_row || shopping) draw = FALSE;
    if (section_icky_row)
    {
        if (y >= section_icky_row) draw = TRUE;
        else if ((section_icky_col > 0) && (x_off >= section_icky_col)) draw = TRUE;
        else if ((section_icky_col < 0) && (x_off >= 0 - section_icky_col)) draw = TRUE;
    }

    if (draw)
    {
        x_off += x * (tile_width - 1);
        y = (y - 1) * tile_height + 1;

        Term_queue_char_safe(x_off, y, a, c, tap, tcp);
        if (tile_width * tile_height > 1)
        {
            byte a_dummy = (p_ptr->use_graphics? TERM_WHITE: 0);
            char c_dummy = (p_ptr->use_graphics? ' ': 0);

            Term_big_queue_char_safe(x_off, y, a, c, a_dummy, c_dummy);
        }
    }

    /* Queue for later */
    else
    {
        n = Packet_printf(&qbuf, "%b%b%b%b%c", (unsigned)ch, (unsigned)x,
            (unsigned)y, (unsigned)a, (int)c);
        if ((n > 0) && p_ptr->use_graphics)
            Packet_printf(&qbuf, "%b%c", (unsigned)tap, (int)tcp);
    }

    return 1;
}


static int Receive_message(void)
{
    int n, c;
    byte ch;
    u16b type = 0;
    char buf[MSG_LEN], search[MSG_LEN], *ptr;
    bool extended_char = FALSE;

    if ((n = Packet_scanf(&rbuf, "%b%S%hu", &ch, buf, &type)) <= 0)
        return n;

    /* Perform a sanity check on our string */
    for (c = 0; c < strlen(buf); c++)
    {
        /* Hack -- ' ' is probably the lowest character we will be trying to display */
        if (buf[c] < ' ')
        {
            /* Two extended chars in a row: probably a bad string */
            if (extended_char) return 1;

            /* Allow lone extended chars */
            extended_char = TRUE;
        }
        else
            extended_char = FALSE;
    }

    /* Hack -- Repeated message */
    if ((buf[0] == ' ') && (buf[1] == '\0'))
        my_strcpy(buf, message_last(), sizeof(buf));

    strnfmt(search, sizeof(search), "%s] ", nick);

    if (strstr(buf, search) != 0)
    {
        ptr = strstr(talk_pend, strchr(buf, ']') + 2);
        if (ptr) my_strcpy(talk_pend, ptr, sizeof(talk_pend));
        else my_strcpy(talk_pend, "", sizeof(talk_pend));
    }

    if (!topline_icky && (party_mode || shopping || !p_ptr->screen_icky))
        c_msg_print_aux(buf, type);
    else
        message_add(buf, type);

    /* Always display chat messages */
    p_ptr->redraw |= PR_MESSAGE_CHAT;

    /* Hack -- Highlight chat tabs messages */
    if (type >= MSG_CHAT)
    {
        for (n = 0; n < MAX_CHANNELS; n++)
        {
            if (!STRZERO(channels[n].name) && channels[n].id == type - MSG_CHAT)
            {
                if (n != view_channel) p_ptr->on_channel[n] = 1;
            }
        }
    }
    if (type == MSG_WHISPER)
    {
        n = find_whisper_tab(buf, search, sizeof(search));
        if (n && n != view_channel) p_ptr->on_channel[n] = 1;
    }

    return 1;
}


static int Receive_recall(void)
{
    int n;
    byte ch;
    s16b word_recall;

    if ((n = Packet_scanf(&rbuf, "%b%hd", &ch, &word_recall)) <= 0)
        return n;

    p_ptr->word_recall = word_recall;

    p_ptr->redraw |= (PR_STATE);

    return 1;
}


static int Receive_state(void)
{
    int n;
    byte ch;
    s16b searching, resting, unignoring;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd", &ch, &searching, &resting,
        &unignoring)) <= 0)
            return n;

    p_ptr->resting = resting;
    p_ptr->searching = (byte)searching;
    p_ptr->unignoring = (byte)unignoring;

    p_ptr->redraw |= (PR_STATE);

    return 1;
}


static int Receive_title(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%s", &ch, title)) <= 0)
        return n;

    /* XXX -- Extract "ghost-ness" */
    p_ptr->ghost = streq(title, "Ghost");

    /* Redraw */
    p_ptr->redraw |= (PR_TITLE | PR_LAG);

    return 1;
}


static int Receive_turn(void)
{
    int n;
    byte ch;
    u32b game_turn, player_turn, active_turn;

    if ((n = Packet_scanf(&rbuf, "%b%lu%lu%lu", &ch,
        &game_turn, &player_turn, &active_turn)) <= 0)
            return n;

    p_ptr->game_turn.era = 0;
    p_ptr->game_turn.turn = game_turn;
    p_ptr->player_turn.era = 0;
    p_ptr->player_turn.turn = player_turn;
    p_ptr->active_turn.era = 0;
    p_ptr->active_turn.turn = active_turn;

    /* Redraw */
    p_ptr->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_depth(void)
{
    int n;
    byte ch;
    s16b depth, maxdepth;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &depth, &maxdepth)) <= 0)
        return n;

    p_ptr->depth = depth;
    p_ptr->max_depth = maxdepth;

    p_ptr->redraw |= (PR_DEPTH);

    return 1;
}


static int Receive_study(void)
{
    int n;
    byte ch;
    s16b study;
    char can_study_book;

    if ((n = Packet_scanf(&rbuf, "%b%hd%c", &ch, &study, &can_study_book)) <= 0)
        return n;

    p_ptr->new_spells = study;
    p_ptr->can_study_book = (bool)can_study_book;

    p_ptr->redraw |= (PR_STUDY);

    return 1;
}


static int Receive_food(void)
{
    int n;
    byte ch;
    s16b food;

    if ((n = Packet_scanf(&rbuf, "%b%hd", &ch, &food)) <= 0)
        return n;

    p_ptr->food = food;

    p_ptr->redraw |= (PR_STATUS);

    return 1;
}


static int Receive_speed(void)
{
    int n;
    byte ch;
    s16b speed;

    if ((n = Packet_scanf(&rbuf, "%b%hd", &ch, &speed)) <= 0)
        return n;

    p_ptr->state.speed = speed;

    p_ptr->redraw |= (PR_SPEED);

    return 1;
}


static int Receive_dtrap(void)
{
    int n;
    byte ch;
    byte dtrap;

    if ((n = Packet_scanf(&rbuf, "%b%b", &ch, &dtrap)) <= 0)
        return n;

    trap_indicator = dtrap;

    p_ptr->redraw |= (PR_DTRAP);

    return 1;
}


static int Receive_keepalive(void)
{
    int n;
    byte ch;
    u32b ctime;

    if ((n = Packet_scanf(&rbuf, "%b%lu", &ch, &ctime)) <= 0)
        return n;

    /* Make sure it's the same one we sent... */
    if ((ctime == last_sent) && (conn_state == CONN_PLAYING))
    {
        /* Keep track of time in milliseconds */
        updateTicks();
        last_received = mticks;

        /* Update lag bar */
        lag_mark = (last_received - last_sent);
        if (lag_mark > 1000) lag_mark = 1000;
        p_ptr->redraw |= (PR_LAG);
    }

    return 1;
}


static int Receive_status(void)
{
    int n, i;
    byte ch;
    s16b effect;
    int bytes_read;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;
    bytes_read = 1;

    for (i = 0; i < TMD_MAX; i++)
    {
        if ((n = Packet_scanf(&rbuf, "%hd", &effect)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read += 2;

        p_ptr->timed[i] = effect;
    }

    p_ptr->redraw |= (PR_STATUS);

    return 1;
}


/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(struct player *p, const object_type *o_ptr)
{
    /* Ammo */
    if (!obj_is_ammo(p, o_ptr)) return FALSE;

    /* Magic ammo are always +0 +0 */
    if (o_ptr->sval == SV_AMMO_MAGIC) return FALSE;

    return TRUE;
}


/*
 * Hook to specify rechargeable items
 */
static bool item_tester_hook_recharge(struct player *p, const object_type *o_ptr)
{
    /* Recharge staves */
    if (o_ptr->tval == TV_STAFF) return (TRUE);

    /* Recharge wands */
    if (o_ptr->tval == TV_WAND) return (TRUE);

    /* Nope */
    return (FALSE);
}


/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(struct player *p, const object_type *o_ptr)
{
    return (wieldable_p(o_ptr) || (o_ptr->tval == TV_MSTAFF));
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(struct player *p, const object_type *o_ptr)
{
    return armor_p(o_ptr);
}


/* Hooks for Receive_item */
static bool (*item_tester_hook_item[N_HOOKS])(struct player *, const object_type *) =
{
    NULL,
    item_tester_hook_ammo,
    item_tester_hook_recharge,
    item_tester_hook_weapon,
    item_tester_hook_armour
};


static int Receive_item(void)
{
    byte ch;
    int n, item;
    byte tester_tval, tester_hook;
    int mode = 0;
    cmd_code code = CMD_NULL;

    if ((n = Packet_scanf(&rbuf, "%b%b%b", &ch, &tester_tval, &tester_hook)) <= 0)
        return n;

    if (!p_ptr->screen_icky && !topline_icky)
    {
        c_msg_print(NULL);
        item_tester_tval = tester_tval;
        item_tester_hook = ((tester_hook == HOOK_CARRY)? NULL:
            item_tester_hook_item[tester_hook]);

        if (tester_hook != HOOK_CARRY) mode |= USE_EQUIP;
        if (tester_hook != HOOK_CARRY) mode |= USE_INVEN;
        mode |= USE_FLOOR;

        if (tester_hook == HOOK_CARRY) code = CMD_PICKUP;

        if (!get_item(&item, "Which item? ", NULL, code, mode)) return 1;

        Send_item(item);
    }
    else if ((n = Packet_printf(&qbuf, "%b%b%b", (unsigned)ch, (unsigned)tester_tval,
        (unsigned)tester_hook)) <= 0)
    {
        return n;
    }

    return 1;
}


static int Receive_spell_info(void)
{
    byte ch;
    int n;
    s16b book, line;
    char buf[NORMAL_WID];
    byte line_attr, dir_attr, flag, proj_attr;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%s%b%b%b%b", &ch, &book, &line, buf, &line_attr, &flag,
        &dir_attr, &proj_attr)) <= 0)
    {
        return n;
    }

    /* Hack -- Wipe the arrays if blank */
    if (!strlen(buf))
    {
        memset(spell_info, 0, sizeof(spell_info));
        memset(spell_flag, 0, sizeof(spell_flag));
        memset(spell_desc, 0, sizeof(spell_desc));
    }

    /* Save the info */
    else
    {
        spell_flag[book][line].line_attr = line_attr;
        spell_flag[book][line].flag = flag;
        spell_flag[book][line].dir_attr = dir_attr;
        spell_flag[book][line].proj_attr = proj_attr;
        my_strcpy(spell_info[book][line], buf, sizeof(spell_info[0][0]));
    }

    /* Redraw */
    p_ptr->redraw |= PR_SPELL;

    return 1;
}


static int Receive_spell_desc(void)
{
    byte ch;
    int n;
    s16b book, line;
    char buf[MSG_LEN];

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%S", &ch, &book, &line, buf)) <= 0)
        return n;

    /* Save the info */
    my_strcpy(spell_desc[book][line], buf, sizeof(spell_desc[0][0]));

    return 1;
}


static int Receive_flush(void)
{
    char fresh;
    int n;
    byte ch, delay;

    if ((n = Packet_scanf(&rbuf, "%b%c%b", &ch, &fresh, &delay)) <= 0)
        return n;

    /* Flush the terminal */
    if (fresh) Term_fresh();

    /* Wait */
    if (delay) Term_xtra(TERM_XTRA_DELAY, delay);

    return 1;
}


static int Receive_line_info(void)
{
    byte ch, r;
    s16b y = 0;
    char n;
    int mode;
    s16b cols, xoff = 0, coff = 0;
    cave_view_type *dest, *trn;
    bool draw;
    int bytes_read;

    /* Read line number */
    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &y, &cols)) <= 0) return n;
    bytes_read = 5;

    /* Defaults */
    r = p_ptr->remote_term;
    mode = DUNGEON_RLE_MODE(p_ptr);
    dest = p_ptr->scr_info[y];
    trn = p_ptr->trn_info[y];
    draw = TRUE;

    /* Use ANOTHER terminal */
    if (r != NTERM_WIN_OVERHEAD)
    {
        /* Reset the line counter */
        if (y == -1) return 1;

        /* Mini Map Terminal */
        if (ch == PKT_MINI_MAP) p_ptr->redraw |= (PR_MAP);

        /* Values */
        dest = remote_info[r][y];
        draw = FALSE;

        /* Check the max line count */
        if (y > last_remote_line[r]) last_remote_line[r] = y;
    }

    /* Use MAIN terminal */
    else
    {
        int x;

        /* Check the max line count */
        last_line_info = y;

        /* Reset the line counter */
        if (y == -1) return 1;

        /* Mini Map */
        if (ch == PKT_MINI_MAP) y++;

        /* Reinitialize the secondary attr/char stream */
        for (x = 0; x < cols; x++)
        {
            p_ptr->trn_info[y][x].c = 0;
            p_ptr->trn_info[y][x].a = 0;
        }

        /* Mini Map */
        if (ch == PKT_MINI_MAP) draw = p_ptr->screen_icky;

        /* Dungeon */
        else
        {
            /* Decode the secondary attr/char stream */
            if (p_ptr->use_graphics)
            {
                n = rle_decode(&rbuf, p_ptr->trn_info[y], cols, RLE_LARGE, &bytes_read);
                if (n <= 0) return n;
            }

            draw = !p_ptr->screen_icky;

            /* Ugly Hack - Shopping */
            if (shopping) draw = FALSE;

            /* Hang on! Icky section! */
            if (section_icky_row && (y < section_icky_row))
            {
                if (section_icky_col > 0) xoff = section_icky_col;
                if (section_icky_col < 0) coff = section_icky_col;
                if ((xoff >= cols) || (cols - coff <= 0)) draw = FALSE;
            }

            /* Request a redraw if the line was icky */
            if (!draw) request_redraw = TRUE;
        }
    }

    /* Decode the attr/char stream */
    n = rle_decode(&rbuf, dest, cols, mode, &bytes_read);
    if (n <= 0) return n;

    /* Put data to screen */
    if (draw)
    {
        /* Use ANOTHER terminal */
        if (r != NTERM_WIN_OVERHEAD) caveprt(dest, cols, 0, y);

        /* Use MAIN terminal */
        else
        {
            int i, x;
            cave_view_type *scr_info, *trn_info;

            if (ch == PKT_MINI_MAP) Term->minimap_active = TRUE;

            /* For mini-map, be sure the display gets cleared */
            if (ch == PKT_MINI_MAP) Term_erase(COL_MAP, y, 255);

            /* For main map, apply vertical offset */
            else y = (y - 1) * tile_height + 1;

            /* Draw a character n times */
            for (i = 0; i < cols + coff; i++)
            {
                /* Index */
                x = i + xoff;
                scr_info = dest + x;
                trn_info = trn + x;

                /* Location */
                x += COL_MAP;
                if (ch != PKT_MINI_MAP) x += i * (tile_width - 1);

                /* Draw the character */
                Term_queue_char_safe(x, y, scr_info->a, scr_info->c, trn_info->a, trn_info->c);

                if ((ch != PKT_MINI_MAP) && (tile_width * tile_height > 1))
                {
                    byte a_dummy = (p_ptr->use_graphics? TERM_WHITE: 0);
                    char c_dummy = (p_ptr->use_graphics? ' ': 0);

                    Term_big_queue_char_safe(x, y, scr_info->a, scr_info->c, a_dummy, c_dummy);
                }
            }
        }
    }

    return 1;
}


static int Receive_fullmap(void)
{
    byte ch;
    s16b y = 0;
    char n;
    int x;
    int bytes_read;

    /* Read line number */
    if ((n = Packet_scanf(&rbuf, "%b%hd", &ch, &y)) <= 0) return n;
    bytes_read = 3;

    /* Check the max line count */
    last_line_info = y;

    /* Reset the line counter */
    if (y == -1) return 1;

    /* Reinitialize the secondary attr/char stream */
    for (x = 0; x < DUNGEON_WID; x++)
    {
        p_ptr->trn_info[y][x].c = 0;
        p_ptr->trn_info[y][x].a = 0;
    }

    /* Decode the secondary attr/char stream */
    if (p_ptr->use_graphics)
    {
        n = rle_decode(&rbuf, p_ptr->trn_info[y], DUNGEON_WID, RLE_LARGE, &bytes_read);
        if (n <= 0) return n;
    }

    /* Decode the attr/char stream */
    n = rle_decode(&rbuf, p_ptr->scr_info[y], DUNGEON_WID, RLE_LARGE, &bytes_read);
    if (n <= 0) return n;

    return 1;
}


static int Receive_special_other(void)
{
    int n;
    byte ch, peruse;
    char buf[NORMAL_WID];

    if ((n = Packet_scanf(&rbuf, "%b%s%b", &ch, buf, &peruse)) <= 0)
        return n;

    /* Set file perusal header */
    my_strcpy(special_line_header[NTERM_WIN_OVERHEAD], buf, sizeof(special_line_header[0]));

    if (p_ptr->screen_icky && special_line_type) return 1;
    if (!peruse) return 1;

    /* Set file perusal method to "other" */
    special_line_type = SPECIAL_FILE_OTHER;

    /* Peruse the file we're about to get */
    peruse_file();

    return 1;
}


static int Receive_store(void)
{
    int n;
    char name[MSG_LEN];
    byte attr;
    s16b wgt;
    char pos;
    s32b price;
    byte num, owned, tval, max;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%c%b%hd%b%b%ld%b%b%s", &ch, &pos, &attr, &wgt,
            &num, &owned, &price, &tval, &max, name)) <= 0)
        return n;

    /* Item info used by the client */
    current_store.stock[pos].tval = tval;
    current_store.stock[pos].weight = wgt;
    current_store.stock[pos].number = num;
    current_store.stock[pos].askprice = price;

    /* Hack -- Extra information used by the client */
    current_store.stock[pos].info_xtra.attr = attr;
    current_store.stock[pos].info_xtra.max = max;
    current_store.stock[pos].info_xtra.owned = owned;

    my_strcpy(store_names[pos], name, sizeof(store_names[0]));

    return 1;
}


static int Receive_store_info(void)
{
    int n;
    byte ch;
    char store_owner_name[NORMAL_WID];
    s16b num_items;
    s32b max_cost;
    s16b sidx;

    if ((n = Packet_scanf(&rbuf, "%b%hd%s%s%hd%ld", &ch, &sidx, store_name, store_owner_name,
        &num_items, &max_cost)) <= 0)
    {
        return n;
    }

    current_store.stock_num = num_items;
    current_store.owner->max_cost = max_cost;
    string_free(current_store.owner->name);
    current_store.owner->name = string_make(store_owner_name);
    current_store.sidx = ((sidx == -1)? MAX_STORES: sidx);

    /* Only display store if we're not already shopping */
    if (!shopping) do_cmd_store();
    else store_prt_frame();

    return 1;
}


static int Receive_store_leave(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    /* Get out of the store! */
    if (shopping) store_leave();

    return 1;
}


static int Receive_sell(void)
{
    int n;
    byte ch;
    s32b price;
    s16b reset;

    if ((n = Packet_scanf(&rbuf, "%b%ld%hd", &ch, &price, &reset)) <= 0)
        return n;

    /* Accept the selling price */
    store_sell_accept(price, reset);

    return 1;
}


static int Receive_target_info(void)
{
    int n;
    char x, y, buf[NORMAL_WID];
    byte ch;
    s16b dble;

    if ((n = Packet_scanf(&rbuf, "%b%c%c%hd%s", &ch, &x, &y, &dble, buf)) <= 0)
        return n;

    /* Print the message */
    if (Term->cursor_icky)
        prt_icky(buf, 0, 0);
    else
        prt(buf, 0, 0);

    /* Move the cursor */
    if (Term->cursor_icky)
    {
        int vx;

        /* Location in window */
        vx = COL_MAP + x * tile_width;
        y = (y - 1) * tile_height + 1;

        /* Move the cursor */
        Term_gotoxy(vx, y);
        Term_set_cursor(TRUE);
        Term->double_cursor = (bool)dble;
    }

    return 1;
}


static int Receive_sound(void)
{
    int n;
    byte val;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%b", &ch, &val)) <= 0)
        return n;

    /* Make a sound (if allowed) */
    sound(val);

    return 1;
}


static int Receive_special_line(void)
{
    int i, n;
    s16b max, last, line;
    char buf[NORMAL_WID];
    byte r, ch, attr;
    int max_hgt = Client_setup.settings[SETTING_MAX_HGT];

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%b%s", &ch, &max, &last, &line, &attr, buf)) <= 0)
        return n;

    /* Remote term */
    r = p_ptr->remote_term;

    /* Hack -- TERM_DARK means that we indent the line, print a symbol and then a string */
    if (attr == TERM_DARK)
    {
        byte a = (byte)buf[0];

        /* Put some blank characters first */
        for (i = 0; i < 5; i++)
        {
            remote_info[r][line][i].a = TERM_WHITE;
            remote_info[r][line][i].c = ' ';
        }

        /* Add symbol */
        remote_info[r][line][5].a = ((a & 0x80)? a: color_char_to_attr(a));
        remote_info[r][line][5].c = buf[1];

        remote_info[r][line][6].a = TERM_WHITE;
        remote_info[r][line][6].c = ' ';

        /* Copy the rest */
        cavestr(remote_info[r][line] + 7, buf + 3, color_char_to_attr(buf[2]), NORMAL_WID);
    }

    /* Hack -- TERM_SPECIAL means that we extract attr/char directly */
    else if (attr == TERM_SPECIAL)
    {
        /* Erase */
        for (i = 0; i < NORMAL_WID; i++)
        {
            remote_info[r][line][i].a = TERM_WHITE;
            remote_info[r][line][i].c = ' ';
        }

        /* Copy */
        for (i = 0; i < strlen(buf); i++)
        {
            if ((i % 2) == 0)
                remote_info[r][line][i / 2].a = (byte)buf[i];
            else
                remote_info[r][line][i / 2].c = buf[i];
        }
    }

    /* Hack -- TERM_SYMBOL means that we have a symbol as first character */
    else if (attr == TERM_SYMBOL)
    {
        /* Add symbol */
        remote_info[r][line][0].a = (byte)buf[0];
        remote_info[r][line][0].c = buf[1];

        /* Copy the rest */
        cavestr(remote_info[r][line] + 1, buf + 3, (byte)buf[2], NORMAL_WID);
    }

    /* Copy to local buffer */
    else
        cavestr(remote_info[r][line], buf, attr, NORMAL_WID);

    last_remote_line[r] = last;

    /* Redraw */
    if (r == NTERM_WIN_SPECIAL) p_ptr->redraw |= PR_SPECIAL_INFO;

    /* Maximum */
    max_line = max;

    if (!p_ptr->screen_icky) return 1;

    /* Hack -- Decide to go popup/fullon mode */
    if (line == 0)
    {
        /* Copy header to local buffer */
        my_strcpy(special_line_header[r], special_line_header[NTERM_WIN_OVERHEAD],
            sizeof(special_line_header[0]));

        if ((max_line >= max_hgt - 4) || (special_line_type != SPECIAL_FILE_OTHER))
        {
            /* Clear the screen */
            Term_clear();

            /* Show a general "title" + header */
            special_line_header[NTERM_WIN_OVERHEAD][60] = '\0';
            prt(format("[%s] %60s", get_buildid(FALSE), special_line_header[NTERM_WIN_OVERHEAD]), 0,
                0);

            /* Prompt (check if we have extra pages) */
            if (max_line >= max_hgt - 4)
                prt("[Press Space to advance, or ESC to exit.]", max_hgt - 1, 0);
            else
                prt("[Press ESC to exit.]", max_hgt - 1, 0);
        }
        else
        {
            /* Clear the screen */
            for (n = 0; n <= (full_icky_screen? NORMAL_HGT - 1: max_line + 5); n++)
                Term_erase(0, n, NORMAL_WID);

            /* Show a specific "title" -- header */
            c_put_str(TERM_YELLOW, special_line_header[NTERM_WIN_OVERHEAD], 0, 0);

            /* Prompt */
            c_put_str(TERM_L_BLUE, "[Press any key to continue]", max_line + 4, 0);
        }
    }

    /* Print out the info */
    if ((attr == TERM_DARK) || (attr == TERM_SPECIAL) || (attr == TERM_SYMBOL))
        caveprt(remote_info[r][line], NORMAL_WID, 0, line + 2);
    else
        c_put_str(attr, buf, line + 2, 0);

    return 1;
}


static int Receive_floor(void)
{
    int n;
    byte ch;
    byte num, attr, tval, act, sval, fuel, fail;
    s16b o_idx, amt, slot;
    char name[NORMAL_WID];
    object_type *o_ptr;

    if ((n = Packet_scanf(&rbuf, "%b%b%hd%b%hd%b%b%b%b%b%hd%s", &ch, &num,
        &o_idx, &attr, &amt, &tval, &sval, &act, &fuel, &fail, &slot, name)) <= 0)
            return n;

    /* Paranoia */
    if (num >= MAX_FLOOR_STACK) return 1;

    /* Get the item (on the floor) */
    o_ptr = &floor_item[num];

    /* Item info used by the client */
    o_ptr->tval = tval;
    o_ptr->sval = sval;
    o_ptr->number = amt;
    o_ptr->kind = (tval? lookup_kind(tval, sval): NULL);

    /* Hack -- Extra information used by the client */
    o_ptr->info_xtra.attr = attr;
    o_ptr->info_xtra.act = act;
    o_ptr->info_xtra.fuel = fuel;
    o_ptr->info_xtra.fail = fail;
    o_ptr->info_xtra.slot = slot;
    o_ptr->info_xtra.o_idx = o_idx;

    /* Hack -- The name is stored separately */
    my_strcpy(floor_name[num], name, sizeof(floor_name[0]));

    /* Hack -- Number of floor items */
    if (!num) floor_num = 0;
    if (tval) floor_num++;

    /* Redraw */
    if (!num) p_ptr->redraw |= (PR_EQUIP);

    return 1;
}


static int Receive_show_floor(void)
{
    int n;
    byte ch;
    byte mode;

    if ((n = Packet_scanf(&rbuf, "%b%b", &ch, &mode)) <= 0)
        return n;

    show_floor((olist_detail_t)mode);

    return 1;
}


static int Receive_party(void)
{
    int n;
    char buf[160];
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%S", &ch, buf)) <= 0)
        return n;

    /* Copy info */
    my_strcpy(party_info, buf, sizeof(party_info));

    /* Re-show party info */
    if (party_mode)
    {
        Term_erase(0, 13, 255);
        Term_putstr(0, 13, -1, TERM_WHITE, party_info);
        Term_putstr(0, 11, -1, TERM_WHITE, "Command: ");
    }

    return 1;
}


static int Receive_skills(void)
{
    int n, i;
    s16b tmp[11];
    byte ch;
    int bytes_read;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;
    bytes_read = 1;

    /* Read into skills info */
    for (i = 0; i < 11; i++)
    {
        if ((n = Packet_scanf(&rbuf, "%hd", &tmp[i])) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read += 2;
    }

    /* Store */
    p_ptr->state.skills[SKILL_TO_HIT_MELEE] = tmp[0];
    p_ptr->state.skills[SKILL_TO_HIT_BOW] = tmp[1];
    p_ptr->state.skills[SKILL_SAVE] = tmp[2];
    p_ptr->state.skills[SKILL_STEALTH] = tmp[3];
    p_ptr->state.skills[SKILL_SEARCH_FREQUENCY] = tmp[4];
    p_ptr->state.skills[SKILL_SEARCH] = tmp[5];
    p_ptr->state.skills[SKILL_DISARM] = tmp[6];
    p_ptr->state.skills[SKILL_DEVICE] = tmp[7];
    p_ptr->state.num_blows = tmp[8];
    p_ptr->state.num_shots = tmp[9];
    p_ptr->state.see_infra = tmp[10];

    /* Redraw */
    p_ptr->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_cursor(void)  
{
    int n;
    byte ch;
    char vis, x, y;

    if ((n = Packet_scanf(&rbuf, "%b%c%c%c", &ch, &vis, &x, &y)) <= 0)
        return n;

    /* Move the cursor */
    if (Term->cursor_icky)
    {
        int vx;

        /* Location in window */
        vx = COL_MAP + x * tile_width;
        y = (y - 1) * tile_height + 1;

        /* Move the cursor */
        Term_gotoxy(vx, y);
        Term_set_cursor((vis > 0)? TRUE: FALSE);
        Term->double_cursor = ((vis == 1)? TRUE: FALSE);
    }

    return 1;
}  


static int Receive_pause(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    /* Show the most recent changes to the screen */
    Term_fresh();

    /* Flush any pending keystrokes */
    Term_flush();

    /* Redraw */
    redraw_stuff();

    /* The screen is icky */
    p_ptr->screen_icky++;
    Send_icky();

    /* Wait */
    inkey_ex();

    /* Screen isn't icky any more */
    p_ptr->screen_icky--;
    Send_icky();

    /* Flush queue */
    Flush_queue();

    /* Show the most recent changes to the screen */
    Term_fresh();

    return 1;
}


static int Receive_monster_health(void)
{
    int n;
    char num;
    byte ch, attr;

    if ((n = Packet_scanf(&rbuf, "%b%c%b", &ch, &num, &attr)) <= 0)
        return n;

    health_amt = num;
    health_attr = attr;

    p_ptr->redraw |= (PR_HEALTH);

    return 1;
}


static int Receive_poly(void)
{
    int n;
    byte ch;
    s16b r_idx;

    if ((n = Packet_scanf(&rbuf, "%b%hd", &ch, &r_idx)) <= 0)
        return n;

    p_ptr->r_idx = r_idx;

    /* Redraw */
    p_ptr->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_weight(void)
{
    int n;
    byte ch;
    s16b weight, max_weight;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &weight, &max_weight)) <= 0)
        return n;

    p_ptr->total_weight = weight;

    /* Hack -- The capacity is stored in the inven_cnt (unused on client) */
    p_ptr->inven_cnt = max_weight;

    /* Redraw */
    p_ptr->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_channel(void)
{
    int n, j, free = -1;
    byte i;
    char name[MAX_CHAN_LEN];
    char buf[NORMAL_WID];
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%b%s", &ch, &i, buf)) <= 0) return n;

    /* Truncate name */
    my_strcpy(name, buf, sizeof(name));

    /** Close channel **/
    if (name[0] == '-')
    {
        for (n = 0; n < MAX_CHANNELS; n++)
        {
            if (channels[n].id == i)
            {
                if (view_channel == n) cmd_chat_cycle(-1);

                for (j = 0; j < messages_num(); j++)
                {
                    u16b type = message_type(j);

                    if (type == MSG_CHAT + i) message_del(j);
                }

                channels[n].name[0] = '\0';
                channels[n].id = 0;

                if (p_ptr->main_channel == n)
                    p_ptr->main_channel = 0;
                if (STRZERO(channels[view_channel].name))
                    cmd_chat_cycle(+1);

                /* Redraw */
                p_ptr->redraw |= PR_MESSAGE_CHAT;

                break;
            }
        }

        return 1;
    }

    /** Enforce channel **/
    if (STRZERO(name))
    {
        for (n = 0; n < MAX_CHANNELS; n++)
        {
            if (channels[n].id == i)
            {
                p_ptr->main_channel = view_channel = n;

                /* Redraw */
                p_ptr->redraw |= PR_MESSAGE_CHAT;

                break;
            }
        }

        return 1;
    }

    /** Open channel **/

    /* Find free and duplicates */
    for (n = 0; n < MAX_CHANNELS; n++)
    {
        if (free == -1 && STRZERO(channels[n].name))
        {
            free = n;
            continue;
        }
        if (!strcmp(channels[n].name, name)) return 1;
    }

    /* Found free slot */
    if ((n = free) != -1)
    {
        /* Copy info */
        my_strcpy(channels[n].name, name, sizeof(channels[0].name));
        channels[n].id = i;

        /* Highlight
        p_ptr->on_channel[n] = 1; */

        /* Redraw */
        p_ptr->redraw |= PR_MESSAGE_CHAT;

        return 1;
    }

    plog("CLIENT ERROR! No space for new channel...");

    return 1;
}


static int Receive_term_info(void)
{
    int n, last;
    char mode;
    u16b arg;
    byte ch;

    mode = arg = 0;

    if ((n = Packet_scanf(&rbuf, "%b%c%hu", &ch, &mode, &arg)) <= 0)
        return n;

    switch (mode)
    {
        case NTERM_ACTIVATE: p_ptr->remote_term = arg; break;
        case NTERM_CLEAR:
        {
            if (arg == 1) Term_clear();
            last_remote_line[p_ptr->remote_term] = 0;
            break;
        }
        case NTERM_HOLD:
        {
            ui_event ea = EVENT_ABORT;

            Term_event_push(&ea);
            break;
        }
        case NTERM_FLUSH:
        {
            last = last_remote_line[p_ptr->remote_term];
            for (n = 0; n <= last; n++)
                caveprt(remote_info[p_ptr->remote_term][n], NORMAL_WID, 0, n);
            break;
        }
        case NTERM_FRESH:
        {
            /* Copy header to local buffer */
            my_strcpy(special_line_header[p_ptr->remote_term],
                special_line_header[NTERM_WIN_OVERHEAD], sizeof(special_line_header[0]));

            /* Redraw */
            switch (p_ptr->remote_term)
            {
                case NTERM_WIN_MAP: p_ptr->redraw |= (PR_MAP); break;
                case NTERM_WIN_OBJLIST: p_ptr->redraw |= (PR_ITEMLIST); break;
                case NTERM_WIN_OBJECT: p_ptr->redraw |= (PR_OBJECT); break;
                case NTERM_WIN_MONSTER: p_ptr->redraw |= (PR_MONSTER); break;
                case NTERM_WIN_MONLIST: p_ptr->redraw |= (PR_MONLIST); break;
            }
            if (arg != NTERM_POP) break;

            /* Fall through */
        }
        case NTERM_POP:
        {
            /* Popup Hack */
            screen_save();
            last = last_remote_line[p_ptr->remote_term];
            for (n = 0; n <= (full_icky_screen? NORMAL_HGT - 1: last + 5); n++)
                Term_erase(0, n, NORMAL_WID);
            c_put_str(TERM_YELLOW, special_line_header[p_ptr->remote_term], 0, 0);
            for (n = 0; n <= last; n++)
                caveprt(remote_info[p_ptr->remote_term][n], NORMAL_WID, 0, n + 2);
            c_put_str(TERM_L_BLUE, "[Press any key to continue]", last + 4, 0);
            inkey_ex();
            screen_load(TRUE);
            Term_fresh();
            check_store_leave(TRUE);
            break;
        }
    }

    return 1;
}


static int Receive_purchase(void)
{
    byte ch;
    int n;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    /* Finish the buying process */
    store_purchase_end();

    return 1;
}  


static int Receive_confirm(void)
{
    byte ch;
    int n;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    /* Finish the selling process */
    store_sell_end();

    return 1;
}


static int Receive_char_dump(void)
{
    int n;
    byte ch;
    char buf[MSG_LEN];

    if ((n = Packet_scanf(&rbuf, "%b%s", &ch, buf)) <= 0)
        return n;

    /* Begin receiving */
    if (streq(buf, "BEGIN"))
    {
        char dumpname[42];
        char pathname[MSG_LEN];

        strnfmt(dumpname, sizeof(dumpname), "%s.txt", nick);
        path_build(pathname, MSG_LEN, ANGBAND_DIR_USER, dumpname);
        fp = file_open(pathname, MODE_WRITE, FTYPE_TEXT);
    }

    /* End receiving */
    else if (streq(buf, "END")) file_close(fp);

    /* Process the line */
    else x_file_putf(fp, "%s\n", buf);

    return 1;
}


static int Receive_quiver_size(void)
{
    int n;
    byte ch;
    u16b quiver_size, quiver_slots, quiver_remainder;

    if ((n = Packet_scanf(&rbuf, "%b%hu%hu%hu", &ch, &quiver_size, &quiver_slots,
        &quiver_remainder)) <= 0)
            return n;

    p_ptr->quiver_size = quiver_size;
    p_ptr->quiver_slots = quiver_slots;
    p_ptr->quiver_remainder = quiver_remainder;

    /* Redraw */
    p_ptr->redraw |= (PR_EQUIP | PR_INVEN);

    return 1;
}


static int Receive_player_pos(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%hd", &ch, &p_ptr->px, &p_ptr->offset_x,
        &p_ptr->py, &p_ptr->offset_y)) <= 0)
    {
        return n;
    }

    return 1;
}


