/*
 * File: netserver.c
 * Purpose: The server side of the network stuff
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


/*
 * This is the server side of the network connnection stuff.
 *
 * We try very hard to not let the game be disturbed by
 * players logging in.  Therefore a new connection
 * passes through several states before it is actively
 * playing.
 * First we make a new connection structure available
 * with a new socket to listen on.  This socket port
 * number is told to the client via the pack mechanism.
 * In this state the client has to send a packet to this
 * newly created socket with its name and playing parameters.
 * If this succeeds the connection advances to its second state.
 * In this second state the essential server configuration
 * like the map and so on is transmitted to the client.
 * If the client has acknowledged all this data then it
 * advances to the third state, which is the
 * ready-but-not-playing-yet state.  In this state the client
 * has some time to do its final initializations, like mapping
 * its user interface windows and so on.
 * When the client is ready to accept frame updates and process
 * keyboard events then it sends the start-play packet.
 * This play packet advances the connection state into the
 * actively-playing state.  A player structure is allocated and
 * initialized and the other human players are told about this new player.
 * The newly started client is told about the already playing players and
 * play has begun.
 * Apart from these four states there are also two intermediate states.
 * These intermediate states are entered when the previous state
 * has filled the reliable data buffer and the client has not
 * acknowledged all the data yet that is in this reliable data buffer.
 * They are so called output drain states.  Not doing anything else
 * then waiting until the buffer is empty.
 * The difference between these two intermediate states is tricky.
 * The second intermediate state is entered after the
 * ready-but-not-playing-yet state and before the actively-playing state.
 * The difference being that in this second intermediate state the client
 * is already considered an active player by the rest of the server
 * but should not get frame updates yet until it has acknowledged its last
 * reliable data.
 *
 * Communication between the server and the clients is only done
 * using UDP datagrams.  The first client/serverized version of XPilot
 * was using TCP only, but this was too unplayable across the Internet,
 * because TCP is a data stream always sending the next byte.
 * If a packet gets lost then the server has to wait for a
 * timeout before a retransmission can occur.  This is too slow
 * for a real-time program like this game, which is more interested
 * in recent events than in sequenced/reliable events.
 * Therefore UDP is now used which gives more network control to the
 * program.
 * Because some data is considered crucial, like the names of
 * new players and so on, there also had to be a mechanism which
 * enabled reliable data transmission.  Here this is done by creating
 * a data stream which is piggybacked on top of the unreliable data
 * packets.  The client acknowledges this reliable data by sending
 * its byte position in the reliable data stream.  So if the client gets
 * a new reliable data packet and it has not had this data before and
 * there is also no data packet missing inbetween, then it advances
 * its byte position and acknowledges this new position to the server.
 * Otherwise it discards the packet and sends its old byte position
 * to the server meaning that it detected a packet loss.
 * The server maintains an acknowledgement timeout timer for each
 * connection so that it can retransmit a reliable data packet
 * if the acknowledgement timer expires.
 */


#include "s-angband.h"
#include "../common/buildid.h"
#include "../common/randname.h"
#include "../common/tvalsval.h"
#include "cmds.h"
#include "monster/mon-util.h"
#include "netserver.h"
#include "party.h"
#include "s-spells.h"
#include "target.h"

#define MAX_RELIABLE_DATA_PACKET_SIZE   512
#define MAX_TEXTFILE_CHUNK              512


static server_setup_t Setup;
static int (*playing_receive[256])(int ind), (*setup_receive[256])(int ind);
static int login_in_progress;
static int num_logins, num_logouts;
static int MetaSocket = -1;


/* The contact socket */
static int Socket;
static sockbuf_t ibuf;


/*** Player connection/index wrappers ***/


/*
 * An array for player connections
 *
 * Connection index is in [1..NumConnections]
 */
static long NumConnections;

static connection_t *Conn = NULL;

static void init_connections(void)
{
    size_t size = MAX_PLAYERS * sizeof(*Conn);

    Conn = mem_alloc(size);
    if (!Conn) quit("Cannot allocate memory for connections");
    memset(Conn, 0, size);
}

static void free_connections(void)
{
    mem_free(Conn);
}

connection_t *get_connection(long idx)
{
    if (!Conn) return NULL;
    return &Conn[idx];
}


/*
 * An array for mapping player and connection indices
 *
 * Player index is in [1..NumPlayers]
 * Connection index is in [1..NumConnections]
 */
static long GetInd[MAX_PLAYERS];

long get_player_index(connection_t *connp)
{
    if (connp->id != -1) return GetInd[connp->id];
    return 0;
}

void set_player_index(connection_t *connp, long idx)
{
    GetInd[connp->id] = idx;
}


/*** General utilities ***/


/* Forward declarations */
static void Contact(int fd, int arg);
static void Init_receive(void);


/*
 * Initialize the connection structures.
 */
int Setup_net_server(void)
{
    Init_receive();

    if (Init_setup() == -1)
        return -1;

    init_connections();

    init_players();

    /* Tell the metaserver that we're starting up */
    Report_to_meta(META_START);

    plog_fmt("Server is running version %04x", CUR_VERSION);

    return 0;
}


static void Conn_set_state(connection_t *connp, int state)
{
    static int num_conn_busy;
    static int num_conn_playing;

    if ((connp->state == CONN_PLAYING) || (connp->state == CONN_QUIT))
        num_conn_playing--;
    else if (connp->state == CONN_FREE)
        num_conn_busy++;

    connp->state = state;
    ht_copy(&connp->start, &turn);

    if (connp->state == CONN_PLAYING)
    {
        num_conn_playing++;
        connp->timeout = PLAY_TIMEOUT;
    }
    else if (connp->state == CONN_QUIT)
    {
        num_conn_playing++;
        connp->timeout = QUIT_TIMEOUT;
    }
    else if (connp->state == CONN_SETUP)
        connp->timeout = SETUP_TIMEOUT;
    else if (connp->state == CONN_FREE)
    {
        num_conn_busy--;
        connp->timeout = FREE_TIMEOUT;
    }
    login_in_progress = num_conn_busy - num_conn_playing;
}


/*
 * Actually quit. This was separated as a hack to allow us to
 * "quit" when a quit packet has not been received, such as when
 * our TCP connection is severed.
 */
static void do_quit(int ind)
{
    int player, depth = 0;
    connection_t * connp = get_connection(ind);
    bool dungeon_master = FALSE;

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        depth = player_get(player)->depth;
        dungeon_master = is_dm(player);
    }

    /* Close the socket */
    SocketClose(connp->w.sock);

    /* No more packets from a player who is quitting */
    remove_input(connp->w.sock);

    /* Disable all output and input to and from this player */
    connp->w.sock = -1;

    /* Check for immediate disconnection */
    if (town_area(depth) || dungeon_master)
    {
        /* If we are close to the center of town, exit quickly. */
        /* DM always disconnects immediately */
        Destroy_connection(ind, "Client quit");
    }
    else
    {
        /* Otherwise wait for the timeout */
        Conn_set_state(connp, CONN_QUIT);
    }
}


/*
 * Process a client packet.
 * The client may be in one of several states,
 * therefore we use function dispatch tables for easy processing.
 * Some functions may process requests from clients being
 * in different states.
 * The behavior of this function has been changed somewhat.  New commands are now
 * put into a command queue, where they will be executed later.
 */
static void Handle_input(int fd, int arg)
{
    int ind = arg, player, old_numplayers = NumPlayers;
    connection_t *connp = get_connection(ind);

    /* Ignore input from client if not in SETUP or PLAYING state */
    if ((connp->state != CONN_PLAYING) && (connp->state != CONN_SETUP)) return;

    /* Handle "leaving" */
    if ((connp->id != -1) && player_get(get_player_index(connp))->new_level_flag) return;

    /* Reset the buffer we are reading into */
    Sockbuf_clear(&connp->r);

    /* Read in the data */
    if (Sockbuf_read(&connp->r) <= 0)
    {
        /*
         * On windows, we frequently get EWOULDBLOCK return codes, i.e.
         * there is no data yet, but there may be in a moment. Without
         * this check clients frequently get disconnected
         */
        if ((errno != EAGAIN) && (errno != EWOULDBLOCK))
        {
            /* If this happens, the the client has probably closed his TCP connection. */
            do_quit(ind);
        }

        return;
    }

    /* Add this new data to the command queue */
    if (Sockbuf_write(&connp->q, connp->r.ptr, connp->r.len) != connp->r.len)
    {
        errno = 0;
        Destroy_connection(ind, "Can't copy queued data to buffer");
        return;
    }

    /* Execute any new commands immediately if possible */
    process_pending_commands(ind);

    /*
     * Hack -- don't update the player info if the number of players since
     * the beginning of this function call has changed, which might indicate
     * that our player has left the game.
     */
    if ((old_numplayers == NumPlayers) && (connp->state == CONN_PLAYING))
    {
        /* Update the players display if necessary and possible */
        if (connp->id != -1)
        {
            player = get_player_index(connp);

            /* Refresh stuff */
            refresh_stuff(player);
        }
    }

    /*
     * PKT_END makes client pause with the net input and move to keyboard
     * so it's important to apply it at the end
     */
    if (connp->c.len > 0)
    {
        if (Packet_printf(&connp->c, "%b", (unsigned)PKT_END) <= 0)
        {
            Destroy_connection(ind, "Net input write error");
            return;
        }
        Send_reliable(ind);
    }
}


static u16b get_flavor_max(void)
{
    struct flavor *f;
    u16b max = 0;

    if (!flavors) return 0;

    for (f = flavors; f; f = f->next)
    {
        if (f->fidx > max) max = f->fidx;
    }

    return max + 1;
}


/*
 * After a TCP "Contact" was made we shall see if we have
 * room for more connections and create one.
 */
static int Setup_connection(u32b account, char *real, char *nick, char *addr, char *host,
    char *pass, u16b conntype, unsigned version, int fd)
{
    int i, free_conn_index = MAX_PLAYERS, my_port, sock;
    connection_t *connp;
    bool memory_error = FALSE;

    for (i = 0; i < MAX_PLAYERS; i++)
    {
        connp = get_connection(i);
        if (connp->state == CONN_FREE)
        {
            if (free_conn_index == MAX_PLAYERS)
                free_conn_index = i;
            continue;
        }

        if ((connp->state == CONN_CONSOLE) || (conntype == CONNTYPE_CONSOLE))
            continue;
    }

    if (free_conn_index >= MAX_PLAYERS)
    {
        plog_fmt("Full house for %s(%s)@%s", real, nick, host);
        return -2;
    }
    connp = get_connection(free_conn_index);

    /* A TCP connection already exists with the client, use it. */
    sock = fd;

    if ((my_port = GetPortNum(sock)) == 0)
    {
        plog("Cannot get port from socket");
        DgramClose(sock);
        return -1;
    }
    if (SetSocketNonBlocking(sock, 1) == -1)
        plog("Cannot make client socket non-blocking");
    if (SetSocketNoDelay(sock, 1) == -1)
        plog("Can't set TCP_NODELAY on the socket");
    if (SocketLinger(sock) == -1)
        plog("Couldn't set SO_LINGER on the socket");
    if (SetSocketReceiveBufferSize(sock, SERVER_RECV_SIZE + 256) == -1)
        plog_fmt("Cannot set receive buffer size to %d", SERVER_RECV_SIZE + 256);
    if (SetSocketSendBufferSize(sock, SERVER_SEND_SIZE + 256) == -1)
        plog_fmt("Cannot set send buffer size to %d", SERVER_SEND_SIZE + 256);

    Sockbuf_init(&connp->w, sock, SERVER_SEND_SIZE, SOCKBUF_WRITE);
    Sockbuf_init(&connp->r, sock, SERVER_RECV_SIZE, SOCKBUF_WRITE | SOCKBUF_READ);
    Sockbuf_init(&connp->c, -1, SERVER_SEND_SIZE, SOCKBUF_WRITE | SOCKBUF_READ | SOCKBUF_LOCK);
    Sockbuf_init(&connp->q, -1, SERVER_RECV_SIZE, SOCKBUF_WRITE | SOCKBUF_READ | SOCKBUF_LOCK);

    connp->id = -1;
    connp->conntype = conntype;
    connp->addr = string_make(addr);

    if ((connp->w.buf == NULL) || (connp->r.buf == NULL) || (connp->c.buf == NULL) ||
        (connp->q.buf == NULL) || (connp->addr == NULL))
    {
        memory_error = TRUE;
    }

    if (conntype == CONNTYPE_PLAYER)
    {
        connp->account = account;
        connp->real = string_make(real);
        connp->nick = string_make(nick);
        connp->host = string_make(host);
        connp->pass = string_make(pass);
        connp->version = version;
        ht_copy(&connp->start, &turn);
        connp->timeout = SETUP_TIMEOUT;

        if (connp->client_setup == 0)
        {
            u16b flavor_max = get_flavor_max();

            connp->Client_setup.k_attr = C_ZNEW(z_info->k_max, byte);
            connp->Client_setup.k_char = C_ZNEW(z_info->k_max, char);
            connp->Client_setup.r_attr = C_ZNEW(z_info->r_max, byte);
            connp->Client_setup.r_char = C_ZNEW(z_info->r_max, char);
            connp->Client_setup.f_attr = C_ZNEW(z_info->f_max, byte_lit);
            connp->Client_setup.f_char = C_ZNEW(z_info->f_max, char_lit);
            connp->Client_setup.flvr_x_attr = C_ZNEW(flavor_max, byte);
            connp->Client_setup.flvr_x_char = C_ZNEW(flavor_max, char);
            connp->client_setup = 1;
        }

        if ((connp->real == NULL) || (connp->nick == NULL) || (connp->pass == NULL) ||
            (connp->host == NULL))
        {
            memory_error = TRUE;
        }
    }

    if (memory_error)
    {
        plog("Not enough memory for connection");
        Destroy_connection(free_conn_index, "No memory");
        return -1;
    }

    if (conntype == CONNTYPE_CONSOLE)
    {
        connp->console_authenticated = FALSE;
        connp->console_listen = FALSE;

        Conn_set_state(connp, CONN_CONSOLE);
    }

    /* Non-players leave now */
    if (conntype != CONNTYPE_PLAYER)
        return free_conn_index;

    Conn_set_state(connp, CONN_SETUP);

    /* Remove the contact input handler */
    remove_input(sock);
    
    /* Install the game input handler */
    install_input(Handle_input, sock, free_conn_index);

    return free_conn_index;
}


static int Check_names(char *nick_name, char *real_name, char *host_name)
{
    char *ptr;

    if ((real_name[0] == 0) || (host_name[0] == 0) || (nick_name[0] < 'A') || (nick_name[0] > 'Z'))
        return E_INVAL;

    /* Any weird characters here, bail out.  We allow letters, numbers and space */
    for (ptr = &nick_name[strlen(nick_name)]; ptr-- > nick_name; )
    {
        if ((*ptr == 32) || ((*ptr >= 97) && (*ptr <= 122)) ||
            ((*ptr >= 65) && (*ptr <= 90)) || ((*ptr >= 48) && (*ptr <= 57)))
        {
            /* ok */
        }
        else
            return E_INVAL;
    }

    for (ptr = &nick_name[strlen(nick_name)]; ptr-- > nick_name; )
    {
        if (isascii(*ptr) && isspace(*ptr)) *ptr = '\0';
        else break;
    }

    /* The "server" and "account" names are reserved */
    if (!strcasecmp(nick_name, "server") || !strcasecmp(nick_name, "account"))
        return E_INVAL;

    return SUCCESS;
}


static void Contact_cancel(int fd, char *reason)
{
    plog(reason);
    remove_input(fd);
    close(fd);
}


static bool Net_Send(int fd)
{
    int bytes;

    /* Send the info */
    bytes = DgramWrite(fd, ibuf.buf, ibuf.len);
    if (bytes == -1)
    {
        GetSocketError(ibuf.sock);
        return FALSE;
    }

    return TRUE;
}


static void Contact(int fd, int arg)
{
    int newsock, bytes, len, ret;
    struct sockaddr_in sin;
    char host_addr[24];
    u16b conntype = 0;
    u16b version = 0;
    char status = SUCCESS;
    char real_name[NORMAL_WID], nick_name[NORMAL_WID], host_name[NORMAL_WID], pass_word[NORMAL_WID];
    u32b account = 0L;
    int *id_list = NULL;
    u16b num = 0;
    size_t i, j;

    /*
     * Create a TCP socket for communication with whoever contacted us
     *
     * Hack -- check if this data has arrived on the contact socket or not.
     * If it has, then we have not created a connection with the client yet,
     * and so we must do so.
     */
    if (fd == Socket)
    {
        if ((newsock = SocketAccept(fd)) == -1)
        {
            /*
             * We couldn't accept the socket connection. This is bad because we can't
             * handle this situation correctly yet.  For the moment, we just log the
             * error and quit
             */
            plog_fmt("Could not accept TCP Connection, socket error = %d", errno);
            quit("Couldn't accept TCP connection.");
        }
        install_input(Contact, newsock, 2);

        return;
    }

    /* Someone connected to us, now try and decipher the message */
    Sockbuf_clear(&ibuf);
    if ((bytes = DgramReceiveAny(fd, ibuf.buf, ibuf.size)) <= 1)
    {
        /* If 0 bytes have been sent than the client has probably closed the connection */
        if (bytes == 0) remove_input(fd);

        /* On Windows we may get a socket error without errno being set */
        else if ((bytes < 0) && (errno == 0)) remove_input(fd);

        else if ((bytes < 0) && (errno != EWOULDBLOCK) && (errno != EAGAIN) && (errno != EINTR))
        {
            /* Clear the error condition for the contact socket */
            GetSocketError(fd);
        }
        return;
    }
    ibuf.len = bytes;

    /* Get the IP address of the client, without using the broken DgramLastAddr() */
    len = sizeof(sin);
    if (getpeername(fd, (struct sockaddr *) &sin, &len) >= 0)
    {
        u32b addr = ntohl(sin.sin_addr.s_addr);
        strnfmt(host_addr, sizeof(host_addr), "%d.%d.%d.%d", (byte)(addr >> 24), (byte)(addr >> 16),
            (byte)(addr >> 8), (byte)addr);
    }

    /* Read first data he sent us -- connection type */
    if (Packet_scanf(&ibuf, "%hu", &conntype) <= 0)
    {
        Contact_cancel(fd, format("Incomplete handshake from %s", host_addr));
        return;
    }

    /* Convert connection type */
    conntype = connection_type_ok(conntype);

    /* For console, switch routines */
    if (conntype == CONNTYPE_CONSOLE)
    {
        /* Hack -- check local access */
        if (cfg_console_local_only &&
            (sin.sin_addr.s_addr != htonl(INADDR_LOOPBACK)))
        {
            Contact_cancel(fd, format("Non-local console attempt from %s", host_addr));
            return;
        }

        /* Try moving to console handlers */
        if ((ret = Setup_connection(0L, NULL, NULL, host_addr, NULL, NULL, conntype, 0, fd)) > -1)
            NewConsole(fd, 0 - (ret + 1));
        else
            Contact_cancel(fd, format("Unable to setup console connection for %s", host_addr));

        return;
    }

    /* For players - continue, otherwise - abort */
    else if (conntype != CONNTYPE_PLAYER)
    {
        Contact_cancel(fd, format("Invalid connection type requested from %s", host_addr));
        return;
    }

    /* Read next data he sent us -- client version */
    if (Packet_scanf(&ibuf, "%hu", &version) <= 0)
    {
        Contact_cancel(fd, format("Incompatible version packet from %s", host_addr));
        return;
    }

    /* Check client version */
    if (version < MIN_VERSION) status = E_VERSION_OLD;
    if (version > CUR_VERSION) status = E_VERSION_NEW;

    /* His version was correct and he's a player */
    if (!status)
    {
        /* Let's try to read the string */
        if (Packet_scanf(&ibuf, "%s%s%s%s", real_name, host_name, nick_name, pass_word) <= 0)
        {
            Contact_cancel(fd, format("Incomplete handshake from %s", host_addr));
            return;
        }

        /* Paranoia */
        real_name[sizeof(real_name) - 1] = '\0';
        host_name[sizeof(host_name) - 1] = '\0';
        nick_name[sizeof(nick_name) - 1] = '\0';
        pass_word[sizeof(pass_word) - 1] = '\0';

        /* Check if his names are valid */
        if (Check_names(nick_name, real_name, host_name))
            status = E_INVAL;
    }

    /* Check if nick_name/pass_word is a valid account */
    if (!status)
    {
        account = get_account(nick_name, pass_word);
        if (!account) status = E_ACCOUNT;
    }

    /* Setup the connection */
    if (!status)
    {
        if (NumPlayers >= MAX_PLAYERS)
            status = E_GAME_FULL;

        else if ((ret = Setup_connection(account, real_name, nick_name, host_addr, host_name,
            pass_word, conntype, version, fd)) == -1)
        {
            status = E_SOCKET;
        }

        if (ret == -2)
            status = E_GAME_FULL;

        /* Log the players connection */
        if (ret != -1)
        {
            plog_fmt("Welcome %s=%s@%s (%s) (version %04x)", nick_name, real_name, host_name,
                host_addr, version);
        }
    }

    /* Get characters attached to this account */
    if (!status)
        num = (u16b)player_id_list(&id_list, account);

    /* Clear buffer */
    Sockbuf_clear(&ibuf);

    /* Send reply */
    Packet_printf(&ibuf, "%c", (int)status);
    Packet_printf(&ibuf, "%hu", (unsigned)num);
    for (i = 0; i < num; i++)
    {
        hash_entry *ptr;

        /* Search for the entry */
        ptr = lookup_player(id_list[i]);

        /* Send info about characters attached to this account */
        if (ptr)
        {
            Packet_printf(&ibuf, "%c", player_expiry(&ptr->death_turn));
            Packet_printf(&ibuf, "%s", ptr->name);
        }
        else
        {
            /* Paranoia: entry has not been found! */
            Packet_printf(&ibuf, "%c", -2);
            Packet_printf(&ibuf, "%s", "--error--");
            plog_fmt("ERROR: player not found for id #%d", id_list[i]);
        }
    }

    /* Send the random name fragments */
    Packet_printf(&ibuf, "%c", RANDNAME_NUM_TYPES);
    for (i = 0; i < RANDNAME_NUM_TYPES; i++)
    {
        Packet_printf(&ibuf, "%lu", num_names[i]);
        for (j = 0; j < num_names[i]; j++)
            Packet_printf(&ibuf, "%s", name_sections[i][j]);
    }

    Net_Send(fd);

    /* Free the memory in the list */
    mem_free(id_list);
}


/*
 * The contact socket now uses TCP.  This breaks backwards
 * compatibility, but is a good thing.
 */
void setup_contact_socket(void)
{
    plog("Create TCP socket...");
    while ((Socket = CreateServerSocket(cfg_tcp_port)) == -1) Sleep(1);
    if (Socket == -2)
        quit("Address is already in use");
    plog("Set Non-Blocking...");
    if (SetSocketNonBlocking(Socket, 1) == -1)
        plog("Can't make contact socket non-blocking");
    if (SocketLinger(Socket) == -1)
        plog("Couldn't set SO_LINGER on the socket");

    if (Sockbuf_init(&ibuf, Socket, SERVER_SEND_SIZE, SOCKBUF_READ | SOCKBUF_WRITE) == -1)
        quit("No memory for contact buffer");

    install_input(Contact, Socket, 0);
}


/*
 * Talk to the metaserver.
 *
 * This function is called on startup, on death, and when the number of players
 * in the game changes.
 */
#define OLD_MANG_FORMAT
bool Report_to_meta(int flag)
{
    static sockbuf_t meta_buf;
    static char local_name[MSG_LEN];
    static int init = 0;
    int bytes, i;
    char buf[MSG_LEN], temp[100];
    int hidden_dungeon_master = 0;
    int build = VERSION_EXTRA;

    /* Abort if the user doesn't want to report */
    if (!cfg_report_to_meta)
        return FALSE;

    /* If this is the first time called, initialize our hostname */
    if (!init)
    {
        /* Never do this again */
        init = 1;

        /* Get our hostname */
        if (cfg_report_address)
            my_strcpy(local_name, cfg_report_address, sizeof(local_name));
        else if (cfg_bind_name)
            my_strcpy(local_name, cfg_bind_name, sizeof(local_name));
        else
            GetLocalHostName(local_name, MSG_LEN);

        my_strcat(local_name, ":", sizeof(local_name));
        strnfmt(temp, sizeof(temp), "%d", (int)cfg_tcp_port);
        my_strcat(local_name, temp, sizeof(local_name));
    }

    my_strcpy(buf, local_name, sizeof(buf));

#ifndef OLD_MANG_FORMAT
    /* Append the version number */
    strnfmt(temp, sizeof(temp), "  %s%s  ", get_buildid(FALSE), (build? "": " (Beta)"));
#endif

    if (flag & META_START)
    {
        if ((MetaSocket = CreateDgramSocket(0)) == -1)
            quit("Couldn't create meta-server Dgram socket");

        if (SetSocketNonBlocking(MetaSocket, 1) == -1)
            quit("Can't make socket non-blocking");

        if (Sockbuf_init(&meta_buf, MetaSocket, SERVER_SEND_SIZE,
            SOCKBUF_READ | SOCKBUF_WRITE | SOCKBUF_DGRAM) == -1)
        {
            quit("No memory for sockbuf buffer");
        }

        Sockbuf_clear(&meta_buf);
#ifdef OLD_MANG_FORMAT
        my_strcat(buf, " Number of players: 0 ", sizeof(buf));
#else
        my_strcat(buf, "(no players)", sizeof(buf));
#endif
    }

    else if (flag & META_DIE)
    {
#ifndef OLD_MANG_FORMAT
        my_strcat(buf, "(down)", sizeof(buf));
#endif
    }

    else if (flag & META_UPDATE)
    {
        /*
         * Hack -- If cfg_secret_dungeon_master is enabled, determine
         * if the Dungeon Master is playing, and if so, reduce the
         * number of players reported.
         */
        for (i = 1; i <= NumPlayers; i++)
        {
            if (player_get(i)->dm_flags & DM_SECRET_PRESENCE) hidden_dungeon_master++;
        }

        /* If someone other than a dungeon master is playing */
        if (NumPlayers - hidden_dungeon_master)
        {
            /* Tell the metaserver about everyone except hidden dungeon masters */
#ifdef OLD_MANG_FORMAT
            strnfmt(temp, sizeof(temp),
                " Number of players: %d Names:", NumPlayers - hidden_dungeon_master);
#else
            strnfmt(temp, sizeof(temp), "Players (%d):", NumPlayers - hidden_dungeon_master);
#endif
            my_strcat(buf, temp, sizeof(buf));
            for (i = 1; i <= NumPlayers; i++)
            {
                player_type *p_ptr = player_get(i);

                /* Handle the cfg_secret_dungeon_master option */
                if (p_ptr->dm_flags & DM_SECRET_PRESENCE) continue;
                my_strcat(buf, " ", sizeof(buf));
                my_strcat(buf, p_ptr->other.base_name, sizeof(buf));
            }
        }
        else
        {
#ifdef OLD_MANG_FORMAT
            my_strcat(buf, " Number of players: 0", sizeof(buf));
#else
            my_strcat(buf, "(no players)", sizeof(buf));
#endif
        }
    }

#ifdef OLD_MANG_FORMAT
    /* Append the version number */
    strnfmt(temp, sizeof(temp), " Version: %s%s", get_buildver(), (build? "": " (Beta)"));

    if (!(flag & META_DIE)) my_strcat(buf, temp, sizeof(buf));
#endif

    /* If we haven't setup the meta connection yet, abort */
    if (MetaSocket == -1) return FALSE;

    Sockbuf_clear(&meta_buf);

    Packet_printf(&meta_buf, "%S", buf);

    bytes = DgramSend(MetaSocket, cfg_meta_address, 8800, meta_buf.buf, meta_buf.len);
    if (bytes == -1)
    {
        plog("Couldn't send info to meta-server!");
        return FALSE;
    }

    return TRUE;
}


/*
 * Delete a player's information and save his game
 */
static void Delete_player(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    char buf[255];
    int i;
    monster_type *m_ptr;
    int depth = p_ptr->depth;

    /* Be paranoid */
    if (cave_get(depth))
    {
        /* Remove the player */
        cave_get(depth)->m_idx[p_ptr->py][p_ptr->px] = 0;

        /* Redraw */
        cave_light_spot(cave_get(depth), p_ptr->py, p_ptr->px);

        /* Forget the view */
        forget_view(p_ptr);

        /* Free monsters from slavery */
        for (i = 1; i < cave_monster_max(cave_get(depth)); i++)
        {
            /* Paranoia -- Skip dead monsters */
            m_ptr = cave_monster(cave_get(depth), i);
            if (!m_ptr->r_idx) continue;

            /* Skip non slaves */
            if (p_ptr->id != m_ptr->master) continue;

            /* Free monster from slavery */
            monster_set_master(m_ptr, NULL, MSTATUS_HOSTILE);
        }
        p_ptr->slaves = 0;
    }

    /* Leave chat channels */
    channels_leave(Ind);

    /* Hack -- Unstatic if the DM left while manually designing a dungeon level */
    if (players_on_depth[depth] == INHIBIT_DEPTH) players_on_depth[depth] = 0;

    /* Try to save his character */
    save_player(Ind);

    /* Un-hostile the player */
    for (i = 1; i < NumPlayers + 1; i++)
        pvp_check(player_get(i), p_ptr, PVP_REMOVE, TRUE, FEAT_NONE);

    /* If he was actively playing, tell everyone that he's left */
    /* Handle the cfg_secret_dungeon_master option */
    if (p_ptr->alive && !p_ptr->is_dead && !(p_ptr->dm_flags & DM_SECRET_PRESENCE))
    {
        strnfmt(buf, sizeof(buf), "%s has left the game.", p_ptr->name);
        msg_broadcast(p_ptr, buf);
    }

    /* Swap entry number 'Ind' with the last one */
    /* Also, update the "player_index" on the cave grids */
    if (Ind != NumPlayers)
    {
        p_ptr = player_get(NumPlayers);
        if (cave_get(p_ptr->depth))
            cave_get(p_ptr->depth)->m_idx[p_ptr->py][p_ptr->px] = 0 - Ind;
        player_set(NumPlayers, player_get(Ind));
        player_set(Ind, p_ptr);
        set_player_index(get_connection(player_get(Ind)->conn), Ind);
    }

    set_player_index(get_connection(player_get(NumPlayers)->conn), NumPlayers);

    /* Free memory */
    player_free(player_get(NumPlayers));
    mem_free(player_get(NumPlayers));

    /* Clear the player slot previously used */
    player_set(NumPlayers, NULL);

    /* Update the number of players */
    NumPlayers--;

    /* Tell the metaserver about the loss of a player */
    Report_to_meta(META_UPDATE);

    /* Fix the monsters and remaining players */
    if (cave_get(depth)) update_monsters(depth, TRUE);
    update_players();
}


/*
 * Hack -- reset all connection values
 *  but keep visual verify tables
 */
static void wipe_connection(connection_t *connp)
{
    byte *k_attr;
    char *k_char;
    byte *r_attr;
    char *r_char;
    byte (*f_attr)[FEAT_LIGHTING_MAX];
    char (*f_char)[FEAT_LIGHTING_MAX];
    byte* flvr_x_attr;
    char* flvr_x_char;
    int save_setup_tables = connp->client_setup;

    /* SAVE */
    if (save_setup_tables)
    {
        k_attr = connp->Client_setup.k_attr;
        r_attr = connp->Client_setup.r_attr;
        f_attr = connp->Client_setup.f_attr;
        flvr_x_attr = connp->Client_setup.flvr_x_attr;
        k_char = connp->Client_setup.k_char;
        r_char = connp->Client_setup.r_char;
        f_char = connp->Client_setup.f_char;
        flvr_x_char = connp->Client_setup.flvr_x_char;
    }

    /* -- WIPE -- */
    memset(connp, 0, sizeof(*connp));

    /* RESTORE */
    if (save_setup_tables)
    {
        connp->client_setup = save_setup_tables;
        connp->Client_setup.k_attr = k_attr;
        connp->Client_setup.r_attr = r_attr;
        connp->Client_setup.f_attr = f_attr;
        connp->Client_setup.flvr_x_attr = flvr_x_attr;
        connp->Client_setup.k_char = k_char;
        connp->Client_setup.r_char = r_char;
        connp->Client_setup.f_char = f_char;
        connp->Client_setup.flvr_x_char = flvr_x_char;
    }
}


/*
 * Cleanup a connection.  The client may not know yet that it is thrown out of
 * the game so we send it a quit packet if our connection to it has not already
 * closed.  If our connection to it has been closed, then connp->w.sock will
 * be set to -1.
 */
bool Destroy_connection(int ind, char *reason)
{
    connection_t *connp = get_connection(ind);
    int len, sock;
    char pkt[NORMAL_WID];

    if (connp->state == CONN_FREE)
    {
        errno = 0;
        plog_fmt("Cannot destroy empty connection (\"%s\")", reason);
        return TRUE;
    }

    sock = connp->w.sock;
    if (sock != -1) remove_input(sock);

    if (connp->conntype == CONNTYPE_PLAYER)
    {
        my_strcpy(&pkt[1], reason, sizeof(pkt) - 2);
        pkt[0] = PKT_QUIT;
        len = strlen(pkt) + 2;
        pkt[len - 1] = PKT_END;
        pkt[len] = '\0';
        if (sock != -1)
        {
            if (DgramWrite(sock, pkt, len) != len)
            {
                GetSocketError(sock);
                DgramWrite(sock, pkt, len);
            }
        }
        plog_fmt("Goodbye %s=%s@%s (\"%s\")", (connp->nick? connp->nick: ""),
            (connp->real? connp->real: ""), (connp->host? connp->host: ""), reason);
    }

    Conn_set_state(connp, CONN_FREE);

    if (connp->id != -1) Delete_player(get_player_index(connp));
    string_free(connp->real);
    string_free(connp->nick);
    string_free(connp->addr);
    string_free(connp->host);
    string_free(connp->pass);
    Sockbuf_cleanup(&connp->w);
    Sockbuf_cleanup(&connp->r);
    Sockbuf_cleanup(&connp->c);
    Sockbuf_cleanup(&connp->q);

    wipe_connection(connp);

    num_logouts++;

    if (sock != -1) DgramClose(sock);

    return TRUE;
}


void Stop_net_server(void)
{
    int i;

    /* Hack -- Free client setup tables */
    for (i = 0; i < MAX_PLAYERS; i++)
    {
        connection_t* connp = get_connection(i);

        if (connp && connp->client_setup)
        {
            mem_free(connp->Client_setup.k_attr);
            mem_free(connp->Client_setup.k_char);
            mem_free(connp->Client_setup.r_attr);
            mem_free(connp->Client_setup.r_char);
            mem_free(connp->Client_setup.f_attr);
            mem_free(connp->Client_setup.f_char);
            mem_free(connp->Client_setup.flvr_x_attr);
            mem_free(connp->Client_setup.flvr_x_char);
        }
    }

    /* Dealloc player array */
    free_players();

    /* Remove listening socket */
    if (Socket != -2) remove_input(Socket);
    Sockbuf_cleanup(&ibuf);

    /* Destroy networking */
    free_input();
    free_connections();
}


/* Actually execute commands from the client command queue */
int process_pending_commands(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    int player, type, result, old_energy = 0;
    int (**receive_tbl)(int ind);

    /* Hack to see if we have quit in this function */
    int num_players_start = NumPlayers;

    /* Hack to buffer data */
    int last_pos, data_advance = 0;

    /* Paranoia: ignore input from client if not in SETUP or PLAYING state */
    if ((connp->state != CONN_PLAYING) && (connp->state != CONN_SETUP)) return TRUE;

    /* SETUP state */
    if (connp->state == CONN_SETUP) receive_tbl = &setup_receive[0];
    else receive_tbl = &playing_receive[0];

    /*
     * Hack -- take any pending commands from the command queue connp->q
     * and move them to connp->r, where the Receive functions get their
     * data from.
     */
    Sockbuf_clear(&connp->r);
    if (connp->q.len > 0)
    {
        if (Sockbuf_write(&connp->r, connp->q.ptr, connp->q.len) != connp->q.len)
        {
            errno = 0;
            Destroy_connection(ind, "Can't copy queued data to buffer");
            return TRUE;
        }
        Sockbuf_clear(&connp->q);
    }

    /* If we have no commands to execute return */
    if (connp->r.len <= 0) return FALSE;

    /* Hack -- if our player id has not been set then do WITHOUT player */
    if (connp->id == -1)
    {
        while ((connp->r.ptr < connp->r.buf + connp->r.len))
        {
            /* Store all data for future, incase a command reports it lacks bytes! */
            if (Sockbuf_write(&connp->q, connp->r.ptr, (connp->r.len - data_advance)) !=
                (connp->r.len - data_advance))
            {
                errno = 0;
                Destroy_connection(ind, "Can't copy read data to queue buffer");
                return TRUE;
            }
            last_pos = (int)connp->r.ptr;
            type = (connp->r.ptr[0] & 0xFF);
            result = (*receive_tbl[type])(ind);
            data_advance += ((int)connp->r.ptr - last_pos);
            ht_copy(&connp->start, &turn);
            if (result == 0) return TRUE;
            Sockbuf_clear(&connp->q);
            if (result == -1) return TRUE;
        }

        return FALSE;
    }

    /* Get the player pointer */
    player = get_player_index(connp);
    p_ptr = player_get(player);

    /*
     * Attempt to execute every pending command. Any command that fails due
     * to lack of energy will be put into the queue for next turn by the
     * respective receive function.
     */
    while ((connp->r.ptr < connp->r.buf + connp->r.len))
    {
        type = (connp->r.ptr[0] & 0xFF);
        result = (*receive_tbl[type])(ind);
        if (connp->state == CONN_PLAYING) ht_copy(&connp->start, &turn);
        if (result == -1) return TRUE;

        /* We didn't have enough energy to execute an important command. */
        if (result == 0)
        {
            /* Hack -- if we tried to do something while resting, wake us up. */
            if (p_ptr->resting) disturb(p_ptr, 0, 0);

            /*
             * If we didn't have enough energy to execute this
             * command, in order to ensure that our important
             * commands execute in the proper order, stop
             * processing any commands that require energy. We
             * assume that any commands that don't require energy
             * (such as quitting, or talking) should be executed
             * ASAP.
             *
             * Mega-Hack -- save our old energy and set our energy
             * to 0.  This will allow us to execute "out of game"
             * actions such as talking while we wait for enough
             * energy to execute our next queued in game action.
             */
            if (p_ptr->energy)
            {
                old_energy = p_ptr->energy;
                p_ptr->energy = 0;
            }
        }
    }

    /* Restore our energy if necessary. */

    /*
     * Make sure that the player structure hasn't been deallocated in this
     * time due to a quit request.  Mega-Hack : to do this we check if the number
     * of players has changed while this loop has been executing.  This would be
     * a BAD thing to do if we ever went multithreaded.
     */
    if (NumPlayers == num_players_start)
        if (!p_ptr->energy) p_ptr->energy = old_energy;

    return FALSE;
}


void* console_buffer(int ind, bool read)
{
    connection_t *connp = get_connection(ind);

    if (read) return (void*)&connp->r;
    return (void*)&connp->w;
}


bool Conn_is_alive(int ind)
{
    connection_t *connp = get_connection(ind);

    if (!connp) return FALSE;
    if (connp->state != CONN_CONSOLE) return FALSE;
    return TRUE;
}


void Conn_set_console_setting(int ind, int set, bool val)
{
    connection_t *connp = get_connection(ind);

    if (set) connp->console_authenticated = val;
    else connp->console_listen = val;
}


bool Conn_get_console_setting(int ind, int set)
{
    connection_t *connp = get_connection(ind);

    if (set) return (bool)connp->console_authenticated;
    return (bool)connp->console_listen;
}


/*
 * Hack -- Explain a broken "lib" folder and quit (see below).
 */
static void init_angband_aux(const char *why)
{
    plog(why);
    plog("The 'lib' directory is probably missing or broken.");
    plog("Perhaps the archive was not extracted correctly.");
    plog("See the 'README' file for more information.");
    quit("Fatal Error.");
}


/*
 * Showing and updating the splash screen.
 */
static void show_splashscreen(void)
{
    ang_file *fp;
    char buf[MSG_LEN];
    int n = 0;

    /*** Verify the "news" file ***/

    path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "news.txt");
    if (!file_exists(buf))
    {
        char why[MSG_LEN];

        /* Crash and burn */
        strnfmt(why, sizeof(why), "Cannot access the '%s' file!", buf);
        init_angband_aux(why);
    }

    /*** Display the "news" file ***/

    /* Open the News file */
    fp = file_open(buf, MODE_READ, FTYPE_TEXT);

    /* Dump */
    if (fp)
    {
        /* Dump the file into the buffer */
        while (file_getl(fp, buf, sizeof(buf)) && (n < TEXTFILE__HGT))
        {
            char *version_marker = strstr(buf, "$VERSION");

            if (version_marker)
            {
                ptrdiff_t pos = version_marker - buf;

                strnfmt(version_marker, sizeof(buf) - pos, "%s", get_buildver());
            }

            my_strcpy(&Setup.text_screen[TEXTFILE_MOTD][n * TEXTFILE__WID], buf, TEXTFILE__WID);
            n++;
        }

        file_close(fp);
    }
}


/*
 * Display the tombstone
 */
static void print_tomb(void)
{
    ang_file *fp;
    char buf[MSG_LEN];
    int line = 0;

    /* Open the death file */
    path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "dead.txt");
    fp = file_open(buf, MODE_READ, FTYPE_TEXT);

    /* Dump */
    if (fp)
    {
        /* Dump the file into the buffer */
        while (file_getl(fp, buf, sizeof(buf)) && (line < TEXTFILE__HGT))
        {
            my_strcpy(&Setup.text_screen[TEXTFILE_TOMB][line * TEXTFILE__WID], buf, TEXTFILE__WID);
            line++;
        }

        file_close(fp);
    }
}


/*
 * Display the winner crown
 */
static void display_winner(void)
{
    char buf[MSG_LEN];
    ang_file *fp;
    int i = 2;
    int width = 0;

    path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "crown.txt");
    fp = file_open(buf, MODE_READ, FTYPE_TEXT);

    /* Dump */
    if (fp)
    {
        /* Get us the first line of file, which tells us how long the longest line is */
        file_getl(fp, buf, sizeof(buf));
        sscanf(buf, "%d", &width);
        if (!width) width = 25;

        /* Dump the file into the buffer */
        while (file_getl(fp, buf, sizeof(buf)) && (i < TEXTFILE__HGT))
        {
            strnfmt(&Setup.text_screen[TEXTFILE_CRWN][i * TEXTFILE__WID], TEXTFILE__WID, "%*c%s",
                (NORMAL_WID - width) / 2, ' ', buf);
            i++;
        }

        file_close(fp);
    }
}


int Init_setup(void)
{
    /* Initialize some values */
    Setup.frames_per_second = cfg_fps;
    Setup.min_col = SCREEN_WID;
    Setup.min_row = SCREEN_HGT;
    Setup.max_col = DUNGEON_WID;
    Setup.max_row = DUNGEON_HGT;

    /* Verify and load the splash screen */
    show_splashscreen();

    /* Verify and load the tombstone */
    print_tomb();

    /* Verify and load the winner crown */
    display_winner();

    return 0;
}


byte* Conn_get_console_channels(int ind)
{
    connection_t *connp = get_connection(ind);
    return connp->console_channels;
}


/*** General network functions ***/


/*
 * This function is used for sending data to clients who do not yet have
 * Player structures allocated, and for timing out players who have been
 * idle for a while.
 */
int Net_input(void)
{
    int i;
    connection_t *connp;
    char msg[MSG_LEN];

    for (i = 0; i < MAX_PLAYERS; i++)
    {
        connp = get_connection(i);

        if (connp->state == CONN_FREE) continue;
        if (connp->state == CONN_CONSOLE) continue;

        /* Handle the timeout */
        if (ht_diff(&turn, &connp->start) > connp->timeout * cfg_fps)
        {
            if (connp->state == CONN_QUIT)
                Destroy_connection(i, "Client quit");
            else
            {
                strnfmt(msg, sizeof(msg), "Timeout %02x", connp->state);
                Destroy_connection(i, msg);
            }
            continue;
        }

        /*
         * Make sure that the player we are looking at is not already in the
         * game. If he is already in the game then we will send him data
         * in the function Net_output.
         */
        if (connp->id != -1) continue;
    }

    if (num_logins | num_logouts)
        num_logins = num_logouts = 0;

    return login_in_progress;
}


int Net_output(void)
{
    int i;
    player_type *p_ptr;

    for (i = 1; i <= NumPlayers; i++)
    {
        p_ptr = player_get(i);

        /* Handle "leaving" */
        if (p_ptr->new_level_flag) continue;

        /* Send any information over the network */
        Net_output_p(i);
    }

    /* Every fifteen seconds, update the info sent to the metaserver */
    if (!(turn.turn % (15 * cfg_fps))) Report_to_meta(META_UPDATE);

    return 1;
}


int Net_output_p(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    connection_t *connp = get_connection(p_ptr->conn);

    /*
     * If we have any data to send to the client, terminate it
     * and send it to the client.
     */
    if (connp->c.len > 0)
    {
        if (Packet_printf(&connp->c, "%b", (unsigned)PKT_END) <= 0)
        {
            Destroy_connection(p_ptr->conn, "Net output write error");
            return 1;
        }
        Send_reliable(p_ptr->conn);
    }

    return 1;
}


/*** Sending ***/


/*
 * Encodes and sends an attr/char pairs stream using one of the following "mode"s:
 *
 * RLE_NONE - no encoding is performed, the stream is sent as is.
 * RLE_CLASSIC - classic mangband encoding, the attr is ORed with 0x40,
 *               uses 3 bytes per repetition
 * RLE_LARGE - a more traffic-consuming routine, which is using a separate
 *             byte to mark the repetition spot (making it 4 bytes per rep.),
 *             but it can be used to transfer high-bit attr/chars
 *
 * "lineref" is a pointer to an attr/char array, and "max_col" is specifying its size
 *
 * Note! To sucessfully decode, client MUST use the same "mode"
 */
static int rle_encode(sockbuf_t* buf, cave_view_type* lineref, int max_col, int mode)
{
    int x1, i;
    char c;
    byte a, n;
    int dummy = -1;

    /* Count bytes */
    int b = 0;

    /* Each column */
    for (i = 0; i < max_col; i++)
    {
        /* Obtain the char/attr pair */
        c = (lineref[i]).c;
        a = (lineref[i]).a;

        /* Start looking here */
        x1 = i + 1;

        /* Start with count of 1 */
        n = 1;

        /* Count repetitions of this grid */
        while (mode && lineref[x1].c == c && lineref[x1].a == a && x1 < max_col)
        {
            /* Increment count and column */
            n++;
            x1++;
        }

        /* RLE-II if there at least 3 similar grids in a row */
        if (mode == RLE_LARGE && n >= 3)
        {
            /* Output the info */
            Packet_printf(buf, "%c%b%c%b", dummy, (unsigned)n, (int)c, (unsigned)a);

            /* Start again after the run */
            i = x1 - 1;

            /* Count bytes */
            b += 4;
        }

        /* RLE-I if there at least 2 similar grids in a row */
        else if (mode == RLE_CLASSIC && n >= 2)
        {
            /* Set bit 0x40 of a */
            a |= 0x40;

            /* Output the info */
            Packet_printf(buf, "%c%b%b", (int)c, (unsigned)a, (unsigned)n);

            /* Start again after the run */
            i = x1 - 1;

            /* Count bytes */
            b += 3;
        }
        else
        {
            /* Normal, single grid */
            Packet_printf(buf, "%c%b", (int)c, (unsigned)a);

            /* Count bytes */
            b += 2;
        }
    }

    /* Report total bytes */
    return b;
}


int Send_game_start_conn(int ind)
{
    connection_t *connp = get_connection(ind);

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for play packet (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    return Packet_printf(&connp->c, "%b", (unsigned)PKT_PLAY);
}


int Send_text_screen(int ind, int type, s32b offset)
{
    connection_t *connp = get_connection(ind);
    int i;
    s32b max;

    max = MAX_TEXTFILE_CHUNK;
    if (offset + max > TEXTFILE__WID * TEXTFILE__HGT) max = TEXTFILE__WID * TEXTFILE__HGT - offset;
    if (offset > TEXTFILE__WID * TEXTFILE__HGT) offset = TEXTFILE__WID * TEXTFILE__HGT;

    if (Packet_printf(&connp->c, "%b%hd%ld%ld", (unsigned)PKT_TEXT_SCREEN, type, max, offset) <= 0)
    {
        Destroy_connection(ind, "Send_text_screen write error");
        return -1;
    }

    for (i = offset; i < offset + max; i++)
    {
        if (Packet_printf(&connp->c, "%c", (int)Setup.text_screen[type][i]) <= 0)
        {
            Destroy_connection(ind, "Send_text_screen write error");
            return -1;
        }
    }

    return 1;
}


int Send_basic_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for basic info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    return Packet_printf(&connp->c, "%b%hd%b%b%b%b", (unsigned)PKT_BASIC_INFO,
        (int)Setup.frames_per_second, (unsigned)Setup.min_col, (unsigned)Setup.min_row,
        (unsigned)Setup.max_col, (unsigned)Setup.max_row);
}


static connection_t *get_connp(struct player *p, const char *errmsg)
{
    connection_t *connp;

    /* Paranoia */
    if (!p) return NULL;

    /* Get pointer */
    connp = get_connection(p->conn);

    /* Check state */
    if (connp->state != CONN_PLAYING)
    {
        errno = 0;
        if (connp->state == CONN_QUIT) return NULL;
        plog_fmt("Connection #%d not ready for %s (%d)", connp->id, errmsg, connp->state);
        return NULL;
    }

    return connp;
}


int Send_death_cause(struct player *p)
{
    connection_t *connp = get_connp(p, "death_cause");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%s%hd%ld%ld%hd%s%s", (unsigned)PKT_DEATH_CAUSE,
        p->death_info.title, (int)p->death_info.lev, p->death_info.exp, p->death_info.au,
        (int)p->death_info.depth, p->death_info.died_from, p->death_info.ctime);
}


int Send_winner(struct player *p)
{
    connection_t *connp = get_connp(p, "winner");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b", (unsigned)PKT_WINNER);
}


static u16b get_vault_max(void)
{
    struct vault *v;
    u16b max = 0;

    if (!vaults) return 0;

    for (v = vaults; v; v = v->next)
    {
        if (v->vidx > max) max = v->vidx;
    }

    return max + 1;
}


int Send_limits_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    u16b dummy = 0;
    u32b dummy1 = 0, dummy2 = 0;
    u16b flavor_max = get_flavor_max();
    u16b v_max = get_vault_max();

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for limits info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_LIMITS, (unsigned)dummy, dummy1, dummy2) <= 0)
    {
        Destroy_connection(ind, "Send_limits_info_conn write error");
        return -1;
    }

    if (Packet_printf(&connp->c, "%hu%hu%hu%hu%hu%hu", (unsigned)z_info->e_max,
        (unsigned)v_max, (unsigned)z_info->k_max, (unsigned)z_info->r_max,
        (unsigned)z_info->f_max, (unsigned)flavor_max) <= 0)
    {
        Destroy_connection(ind, "Send_limits_info_conn write error");
        return -1;
    }

    return 1;
}  


int Send_inven_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    int i;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for inventory info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_INVEN, (unsigned)INVEN_TOTAL, (unsigned)INVEN_WIELD,
        (unsigned)ALL_INVEN_TOTAL) <= 0)
    {
        Destroy_connection(ind, "Send_inven_info_conn write error");
        return -1;
    }

    if (Packet_printf(&connp->c, "%b%b%b%b%b%b", (unsigned)INVEN_PACK, (unsigned)INVEN_BOW,
        (unsigned)INVEN_LEFT, (unsigned)INVEN_LIGHT, (unsigned)QUIVER_START,
        (unsigned)MAX_STACK_SIZE) <= 0)
    {
        Destroy_connection(ind, "Send_inven_info_conn write error");
        return -1;
    }

    for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
    {
        if (Packet_printf(&connp->c, "%s", mention_use(i)) <= 0)
        {
            Destroy_connection(ind, "Send_inven_info_conn write error");
            return -1;
        }
    }

    return 1;
}


int Send_race_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b dummy1 = 0, dummy2 = 0, j;
    struct player_race *r;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for race info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_RACE, (unsigned)player_rmax(), dummy1, dummy2) <= 0)
    {
        Destroy_connection(ind, "Send_race_info_conn write error");
        return -1;
    }

    for (r = races; r; r = r->next)
    {
        if (Packet_printf(&connp->c, "%b%s", r->ridx, r->name) <= 0)
        {
            Destroy_connection(ind, "Send_race_info_conn write error");
            return -1;
        }

        /* Transfer other fields here */
        for (j = 0; j < A_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)r->r_adj[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_info_conn write error");
                return -1;
            }
        }
        for (j = 0; j < SKILL_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)r->r_skills[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_info_conn write error");
                return -1;
            }
        }
        if (Packet_printf(&connp->c, "%b%hd%b%hu", (unsigned)r->r_mhp, (int)r->r_exp,
            (unsigned)r->infra, (unsigned)r->choice) <= 0)
        {
            Destroy_connection(ind, "Send_race_info_conn write error");
            return -1;
        }
        for (j = 0; j < PF_SIZE; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)r->pflags[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_info_conn write error");
                return -1;
            }
        }
        for (j = 0; j < OF_SIZE; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)r->flags[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_info_conn write error");
                return -1;
            }
        }
    }

    return 1;
}


int Send_class_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b dummy1 = 0, dummy2 = 0, j;
    struct player_class *c;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for class info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_CLASS, (unsigned)player_cmax(), dummy1, dummy2) <= 0)
    {
        Destroy_connection(ind, "Send_class_info_conn write error");
        return -1;
    }

    for (c = classes; c; c = c->next)
    {
        if (Packet_printf(&connp->c, "%b%s", c->cidx, c->name) <= 0)
        {
            Destroy_connection(ind, "Send_class_info_conn write error");
            return -1;
        }

        /* Transfer other fields here */
        for (j = 0; j < A_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)c->c_adj[j]) <= 0)
            {
                Destroy_connection(ind, "Send_class_info_conn write error");
                return -1;
            }
        }
        for (j = 0; j < SKILL_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)c->c_skills[j]) <= 0)
            {
                Destroy_connection(ind, "Send_class_info_conn write error");
                return -1;
            }
        }
        if (Packet_printf(&connp->c, "%b%hd%b", (unsigned)c->c_mhp, (int)c->c_exp,
            (unsigned)c->spell_book) <= 0)
        {
            Destroy_connection(ind, "Send_class_info_conn write error");
            return -1;
        }
        for (j = 0; j < PF_SIZE; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)c->pflags[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_info_conn write error");
                return -1;
            }
        }
    }

    return 1;
}


int Send_socials_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i, dummy1 = 0, dummy2 = 0;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for socials info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_SOCIALS, (unsigned)z_info->soc_max, dummy1, dummy2) <= 0)
    {
        Destroy_connection(ind, "Send_socials_info_conn write error");
        return -1;
    }

    for (i = 0; i < z_info->soc_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", soc_info[i].name) <= 0)
        {
            Destroy_connection(ind, "Send_socials_info_conn write error");
            return -1;
        }

        /* Transfer other fields here */
        if (Packet_printf(&connp->c, "%b", (unsigned)soc_info[i].target) <= 0)
        {
            Destroy_connection(ind, "Send_socials_info_conn write error");
            return -1;
        }
    }

    return 1;
}


int Send_kind_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i, dummy1 = 0, dummy2 = 0;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for kind info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_KINDS, (unsigned)z_info->k_max, dummy1, dummy2) <= 0)
    {
        Destroy_connection(ind, "Send_kind_info_conn write error");
        return -1;
    }

    for (i = 0; i < z_info->k_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", (k_info[i].name? k_info[i].name: "")) <= 0)
        {
            Destroy_connection(ind, "Send_kind_info_conn write error");
            return -1;
        }

        /* Transfer other fields here */
        if (Packet_printf(&connp->c, "%b%b%lu", (unsigned)k_info[i].tval,
            (unsigned)k_info[i].sval, k_info[i].kidx) <= 0)
        {
            Destroy_connection(ind, "Send_kind_info_conn write error");
            return -1;
        }
    }

    return 1;
}


int Send_hints_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    u16b max = 0;
    u32b dummy1 = 0, dummy2 = 0;
    struct hint *h;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for hints info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    /* Count hints */
    h = hints;
    while (h)
    {
        max++;
        h = h->next;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_HINTS, (unsigned)max, dummy1, dummy2) <= 0)
    {
        Destroy_connection(ind, "Send_hints_info_conn write error");
        return -1;
    }

    h = hints;
    while (h)
    {
        if (Packet_printf(&connp->c, "%s", h->hint) <= 0)
        {
            Destroy_connection(ind, "Send_hints_info_conn write error");
            return -1;
        }

        h = h->next;
    }

    return 1;
}


int Send_rinfo_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i, dummy1 = 0, dummy2 = 0;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for rinfo info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu%lu%lu", (unsigned)PKT_STRUCT_INFO,
        (int)STRUCT_INFO_RINFO, (unsigned)z_info->r_max, dummy1, dummy2) <= 0)
    {
        Destroy_connection(ind, "Send_rinfo_info_conn write error");
        return -1;
    }

    for (i = 0; i < z_info->r_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", (r_info[i].name? r_info[i].name: "")) <= 0)
        {
            Destroy_connection(ind, "Send_rinfo_info_conn write error");
            return -1;
        }
    }

    return 1;
}


int Send_char_info_conn(int ind)
{
    connection_t *connp = get_connection(ind);

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for char info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    return Packet_printf(&connp->c, "%b%b%b%b%b", (unsigned)PKT_CHAR_INFO,
        (unsigned)connp->char_state, (unsigned)connp->ridx, (unsigned)connp->cidx,
        (unsigned)connp->psex);
}


int Send_plusses(struct player *p, int tofhit, int tofdam, int tomhit, int tomdam,
    int toshit, int tosdam)
{
    connection_t *connp = get_connp(p, "plusses");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd%hd%hd%hd", (unsigned)PKT_PLUSSES,
        tofhit, tofdam, tomhit, tomdam, toshit, tosdam);
}


int Send_ac(struct player *p, int base, int plus)
{
    connection_t *connp = get_connp(p, "ac");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_AC, base, plus);
}


int Send_lvl(struct player *p, int lev, int mlev)
{
    connection_t *connp = get_connp(p, "level");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_LEV, lev, mlev);
}


int Send_exp(struct player *p, s32b max, s32b cur, s16b expfact)
{
    connection_t *connp = get_connp(p, "exp");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%ld%ld%hd", (unsigned)PKT_EXP, max, cur, expfact);
}


int Send_gold(struct player *p, s32b au)
{
    connection_t *connp = get_connp(p, "gold");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%ld", (unsigned)PKT_GOLD, au);
}


int Send_hp(struct player *p, int mhp, int chp)
{
    connection_t *connp = get_connp(p, "hp");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_HP, mhp, chp);
}


int Send_sp(struct player *p, int msp, int csp)
{
    connection_t *connp = get_connp(p, "sp");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_SP, msp, csp);
}


int Send_char_info(struct player *p, byte ridx, byte cidx, byte psex)
{
    connection_t *connp = get_connp(p, "char info");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%b%b", (unsigned)PKT_CHAR_INFO,
        (unsigned)ridx, (unsigned)cidx, (unsigned)psex);
}


int Send_various(int ind, int hgt, int wgt, int age, int sc)
{
    connection_t *connp = get_connp(player_get(ind), "various");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd%hd", (unsigned)PKT_VARIOUS,
        hgt, wgt, age, sc);
}


int Send_stat(struct player *p, int stat, int stat_top, int stat_use,
    int stat_max, int stat_add, int stat_cur)
{
    connection_t *connp = get_connp(p, "stat");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%hd%hd%hd%hd%hd", (unsigned)PKT_STAT, stat, stat_top,
        stat_use, stat_max, stat_add, stat_cur);
}


int Send_objflags(struct player *p, int line)
{
    connection_t *connp = get_connp(p, "objflags");
    if (connp == NULL) return 0;

    /* Put a header on the packet */
    Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_OBJFLAGS, line);

    /* Encode and send the attr/char stream */
    rle_encode(&connp->c, p->hist_flags[line], RES_COLS, DUNGEON_RLE_MODE(p));

    return 1;
}


int Send_history(int ind, int line, const char *hist)
{
    connection_t *connp = get_connp(player_get(ind), "history");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%s", (unsigned)PKT_HISTORY, line, hist);
}


int Send_inven(struct player *p, char pos, byte attr, int wgt, s32b price, int amt,
    byte tval, byte sval, byte act, byte fuel, byte fail, int slot, const char *name)
{
    connection_t *connp = get_connp(p, "inven");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%b%hd%ld%hd%b%b%b%b%b%hd%s",
        (unsigned)PKT_INVEN, (int)pos, (unsigned)attr, wgt, price, amt,
        (unsigned)tval, (unsigned)sval, (unsigned)act, (unsigned)fuel,
        (unsigned)fail, slot, name);
}


int Send_equip(struct player *p, char pos, byte attr, int wgt, s32b price, int amt,
    byte tval, byte sval, byte act, byte fuel, byte fail, int slot, const char *name)
{
    connection_t *connp = get_connp(p, "equip");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%b%hd%ld%hd%b%b%b%b%b%hd%s",
        (unsigned)PKT_EQUIP, (int)pos, (unsigned)attr, wgt, price, amt,
        (unsigned)tval, (unsigned)sval, (unsigned)act, (unsigned)fuel,
        (unsigned)fail, slot, name);
}


int Send_title(struct player *p, const char *title)
{
    connection_t *connp = get_connp(p, "title");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%s", (unsigned)PKT_TITLE, title);
}


int Send_turn(int ind, u32b game_turn, u32b player_turn, u32b active_turn)
{
    connection_t *connp = get_connp(player_get(ind), "turn");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%lu%lu%lu", (unsigned)PKT_TURN,
        game_turn, player_turn, active_turn);
}


int Send_depth(struct player *p, int depth, int maxdepth)
{
    connection_t *connp = get_connp(p, "depth");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_DEPTH, depth, maxdepth);
}


int Send_food(struct player *p, int food)
{
    connection_t *connp = get_connp(p, "food");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_FOOD, food);
}


int Send_status(struct player *p, s16b *effects)
{
    int i;

    connection_t *connp = get_connp(p, "blind");
    if (connp == NULL) return 0;

    Packet_printf(&connp->c, "%b", (unsigned)PKT_STATUS);

    for (i = 0; i < TMD_MAX; i++)
        Packet_printf(&connp->c, "%hd", (int)effects[i]);

    return 1;
}


int Send_speed(struct player *p, int speed)
{
    connection_t *connp = get_connp(p, "speed");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_SPEED, speed);
}


int Send_dtrap(struct player *p, byte dtrap)
{
    connection_t *connp = get_connp(p, "dtrap");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b", (unsigned)PKT_DTRAP, (unsigned)dtrap);
}


int Send_study(struct player *p, int study, bool can_study_book)
{
    connection_t *connp = get_connp(p, "study");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%c", (unsigned)PKT_STUDY, study, (int)can_study_book);
}


int Send_message(struct player *p, const char *msg, u16b typ)
{
    char buf[MSG_LEN];

    connection_t *connp = get_connp(p, "message");
    if (connp == NULL) return 0;

    if (msg == NULL) return 1;

    /* Clip end of msg if too long */
    my_strcpy(buf, msg, sizeof(buf));

    return Packet_printf(&connp->c, "%b%S%hu", (unsigned)PKT_MESSAGE, buf, (unsigned)typ);
}


static void end_mind_link(struct player *p, int Ind2)
{
    p->esp_link = 0;
    p->esp_link_type = 0;
    p->redraw |= PR_MAP;

    if (Ind2)
    {
        player_type *p_ptr2 = player_get(Ind2);

        p_ptr2->esp_link = 0;
        p_ptr2->esp_link_type = 0;

        msg(p, "You break the mind link with %s.", p_ptr2->name);
        msg(p_ptr2, "%s breaks the mind link with you.", p->name);
    }
    else
        msg(p, "Ending mind link.");
}


static void break_mind_link(int player)
{
    player_type *p_ptr = player_get(player);

    if (p_ptr->esp_link && (p_ptr->esp_link_type == LINK_DOMINANT))
    {
        int Ind2 = find_player(p_ptr->esp_link);

        end_mind_link(p_ptr, Ind2);
    }
}


static connection_t* get_mind_link(struct player *p)
{
    if (p->esp_link && (p->esp_link_type == LINK_DOMINATED))
    {
        int Ind2 = find_player(p->esp_link);

        if (Ind2) return get_connection(player_get(Ind2)->conn);
        end_mind_link(p, 0);
    }
    return NULL;
}


int Send_char(struct player *p, int x, int y, byte a, char c, byte ta, char tc)
{
    connection_t *connp, *connp2;

    /* Paranoia */
    if (!p) return 0;

    connp = get_connection(p->conn);

    if (connp->state != CONN_PLAYING)
    {
        errno = 0;
        if (connp->state == CONN_QUIT) return 0;

        /* No message when CONN_FREE because Send_char is called after Destroy_Connection */
        if (connp->state != CONN_FREE)
            plog_fmt("Connection #%d not ready for char (%d)", connp->id, connp->state);

        return 0;
    }

    connp2 = get_mind_link(p);
    if (connp2 && (connp2->state == CONN_PLAYING))
    {
        int Ind2 = find_player(p->esp_link);
        player_type *p_ptr2 = player_get(Ind2);

        if (p_ptr2->use_graphics && (p_ptr2->remote_term == NTERM_WIN_OVERHEAD))
        {
            Packet_printf(&connp2->c, "%b%b%b%b%c%b%c", (unsigned)PKT_CHAR, (unsigned)x, (unsigned)y,
                (unsigned)a, (int)c, (unsigned)ta, (int)tc);
        }
        else
        {
            Packet_printf(&connp2->c, "%b%b%b%b%c", (unsigned)PKT_CHAR, (unsigned)x, (unsigned)y,
                (unsigned)a, (int)c);
        }
    }

    if (p->use_graphics && (p->remote_term == NTERM_WIN_OVERHEAD))
    {
        return Packet_printf(&connp->c, "%b%b%b%b%c%b%c", (unsigned)PKT_CHAR, (unsigned)x,
            (unsigned)y, (unsigned)a, (int)c, (unsigned)ta, (int)tc);
    }
    return Packet_printf(&connp->c, "%b%b%b%b%c", (unsigned)PKT_CHAR, (unsigned)x, (unsigned)y,
        (unsigned)a, (int)c);
}


int Send_spell_info(struct player *p, int book, int i, const char *out_val,
    spell_flags *flags)
{
    connection_t *connp = get_connp(p, "spell info");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%s%b%b%b%b", (unsigned)PKT_SPELL_INFO,
        book, i, out_val, (unsigned)flags->line_attr, (unsigned)flags->flag,
        (unsigned)flags->dir_attr, (unsigned)flags->proj_attr);
}


int Send_spell_desc(struct player *p, int book, int i, char *out_val)
{
    connection_t *connp = get_connp(p, "spell description");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%S", (unsigned)PKT_SPELL_DESC, book, i, out_val);
}


int Send_item_request(struct player *p, byte tester_tval, byte tester_hook)
{
    connection_t *connp = get_connp(p, "item request");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%b", (unsigned)PKT_ITEM, (unsigned)tester_tval,
        (unsigned)tester_hook);
}


int Send_recall(struct player *p, s16b word_recall)
{
    connection_t *connp = get_connp(p, "recall");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_RECALL, (int)word_recall);
}


int Send_state(struct player *p, bool searching, bool resting, bool unignoring)
{
    connection_t *connp = get_connp(p, "state");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd", (unsigned)PKT_STATE,
        (int)searching, (int)resting, (int)unignoring);
}


int Send_flush(struct player *p, bool fresh, bool delay)
{
    byte df = 0;

    connection_t *connp = get_connp(p, "flush");
    if (connp == NULL) return 0;

    /* Set delay factor */
    if (delay) df = p->other.delay_factor;

    return Packet_printf(&connp->c, "%b%c%b", (unsigned)PKT_FLUSH, (int)fresh, (unsigned)df);
}


/*
 * As an attempt to lower bandwidth requirements, each line is run length
 * encoded.  Non-encoded grids are sent as normal, but if a grid is
 * repeated at least twice, then bit 0x40 of the attribute is set, and
 * the next byte contains the number of repetitions of the previous grid.
 */
int Send_line_info(struct player *p, int y)
{
    player_type *p_ptr2 = NULL;
    connection_t *connp, *connp2;
    int screen_wid, screen_wid2 = 0;

    connp = get_connp(p, "line info");
    if (connp == NULL) return 0;

    screen_wid = p->screen_cols / p->tile_wid;

    connp2 = get_mind_link(p);
    if (connp2)
    {
        int Ind2 = find_player(p->esp_link);

        p_ptr2 = player_get(Ind2);
        screen_wid2 = p_ptr2->screen_cols / p_ptr2->tile_wid;
    }

    /* Put a header on the packet */
    Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_LINE_INFO, y, screen_wid);
    if (connp2)
        Packet_printf(&connp2->c, "%b%hd%hd", (unsigned)PKT_LINE_INFO, y, screen_wid2);

    /* Reset the line counter */
    if (y == -1) return 1;

    /* Encode and send the transparency attr/char stream */
    if (p->use_graphics)
        rle_encode(&connp->c, p->trn_info[y], screen_wid, RLE_LARGE);
    if (connp2 && p_ptr2->use_graphics)
        rle_encode(&connp2->c, p->trn_info[y], screen_wid2, RLE_LARGE);

    /* Encode and send the attr/char stream */
    rle_encode(&connp->c, p->scr_info[y], screen_wid, DUNGEON_RLE_MODE(p));
    if (connp2)
        rle_encode(&connp2->c, p->scr_info[y], screen_wid2, DUNGEON_RLE_MODE(p_ptr2));

    return 1;
}


int Send_fullmap(int ind, int y)
{
    player_type *p_ptr = player_get(ind);

    connection_t *connp = get_connp(p_ptr, "full map");
    if (connp == NULL) return 0;

    /* Packet header */
    Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_FULLMAP, y);

    /* Reset the line counter */
    if (y == -1) return 1;

    /* Encode and send the transparency attr/char stream */
    if (p_ptr->use_graphics)
        rle_encode(&connp->c, p_ptr->trn_info[y], DUNGEON_WID, RLE_LARGE);

    /* Encode and send the attr/char stream */
    rle_encode(&connp->c, p_ptr->scr_info[y], DUNGEON_WID, RLE_LARGE);

    return 1;
}


int Send_mini_map(struct player *p, int y, s16b w)
{
    connection_t *connp = get_connp(p, "mini map");
    if (connp == NULL) return 0;

    /* Packet header */
    Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_MINI_MAP, y, (int)w);

    /* Reset the line counter */
    if (y == -1) return 1;

    rle_encode(&connp->c, p->scr_info[y], w, DUNGEON_RLE_MODE(p));

    return 1;
}


int Send_store(int ind, char pos, byte attr, s16b wgt, byte number, byte owned,
    s32b price, byte tval, byte max, const char *name)
{
    connection_t *connp = get_connp(player_get(ind), "store");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%b%hd%b%b%ld%b%b%s", (unsigned)PKT_STORE,
        (int)pos, (unsigned)attr, (int)wgt, (unsigned)number, (unsigned)owned,
        price, (unsigned)tval, (unsigned)max, name);
}


int Send_store_info(int Ind, int num, char *name, char *owner, int items,
    s32b purse)
{
    connection_t *connp = get_connp(player_get(Ind), "store info");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%s%s%hd%ld", (unsigned)PKT_STORE_INFO,
        num, name, owner, items, purse);
}


int Send_store_leave(struct player *p)
{
    connection_t *connp = get_connp(p, "store leave");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b", (unsigned)PKT_STORE_LEAVE);
}


int Send_store_sell(int ind, s32b price, bool reset)
{
    connection_t *connp = get_connp(player_get(ind), "store sell");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%ld%hd", (unsigned)PKT_SELL, price, (int)reset);
}


int Send_target_info(int ind, int x, int y, bool dble, const char *str)
{
    char buf[NORMAL_WID];

    connection_t *connp = get_connp(player_get(ind), "target info");
    if (connp == NULL) return 0;

    /* Copy */
    my_strcpy(buf, str, sizeof(buf));

    return Packet_printf(&connp->c, "%b%c%c%hd%s", (unsigned)PKT_TARGET_INFO, x, y, (int)dble, buf);
}


int Send_sound(struct player *p, int sound)
{
    connection_t *connp = get_connp(p, "sound");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b", (unsigned)PKT_SOUND, (unsigned)sound);
}


int Send_special_line(int ind, int max, int last, int line, byte attr, const char *buf)
{
    char temp[NORMAL_WID];

    connection_t *connp = get_connp(player_get(ind), "special line");
    if (connp == NULL) return 0;

    my_strcpy(temp, buf, sizeof(temp));
    return Packet_printf(&connp->c, "%b%hd%hd%hd%b%s", (unsigned)PKT_SPECIAL_LINE,
        max, last, line, (unsigned)attr, temp);
}


int Send_floor(struct player *p, byte num, int o_idx, byte attr, int amt, byte tval,
    byte sval, byte act, byte fuel, byte fail, int slot, const char *name)
{
    connection_t *connp = get_connp(p, "floor");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%hd%b%hd%b%b%b%b%b%hd%s",
        (unsigned)PKT_FLOOR, (unsigned)num, o_idx, (unsigned)attr, amt,
        (unsigned)tval, (unsigned)sval, (unsigned)act, (unsigned)fuel,
        (unsigned)fail, slot, name);
}


int Send_show_floor(int ind, byte mode)
{
    connection_t *connp = get_connp(player_get(ind), "show_floor");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b", (unsigned)PKT_SHOW_FLOOR,
        (unsigned)mode);
}


int Send_quiver_size(struct player *p, u16b quiver_size, u16b quiver_slots,
    u16b quiver_remainder)
{
    connection_t *connp = get_connp(p, "quiver_size");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hu%hu%hu", (unsigned)PKT_QUIVER_SIZE,
        (unsigned)quiver_size, (unsigned)quiver_slots, (unsigned)quiver_remainder);
}


int Send_party(struct player *p)
{
    char buf[160];

    connection_t *connp = get_connp(p, "party");
    if (connp == NULL) return 0;

    strnfmt(buf, sizeof(buf), "Party: %s", parties[p->party].name);

    if (p->party > 0)
    {
        my_strcat(buf, "     Owner: ", sizeof(buf));
        my_strcat(buf, parties[p->party].owner, sizeof(buf));
    }

    return Packet_printf(&connp->c, "%b%S", (unsigned)PKT_PARTY, buf);
}


int Send_special_other(struct player *p, char *header, byte peruse, bool protect)
{
    connection_t *connp = get_connp(p, "special other");
    if (connp == NULL) return 0;

    /* Protect our info area */
    if (protect)
    {
        alloc_info_icky(p);
        alloc_header_icky(p, header);
    }
    
    return Packet_printf(&connp->c, "%b%s%b", (unsigned)PKT_SPECIAL_OTHER,
        get_header(p, header), (unsigned)peruse);
}


int Send_skills(struct player *p)
{
    s16b skills[11];
    int i, tmp;
    object_type *o_ptr;

    connection_t *connp = get_connp(p, "skills");
    if (connp == NULL) return 0;

    /* Fighting skill */
    o_ptr = &p->inventory[INVEN_WIELD];
    tmp = p->state.to_h + o_ptr->to_h;
    skills[0] = p->state.skills[SKILL_TO_HIT_MELEE] + (tmp * BTH_PLUS_ADJ);

    /* Shooting skill */
    o_ptr = &p->inventory[INVEN_BOW];
    tmp = p->state.to_h + o_ptr->to_h;
    skills[1] = p->state.skills[SKILL_TO_HIT_BOW] + (tmp * BTH_PLUS_ADJ);

    /* Basic abilities */
    skills[2] = p->state.skills[SKILL_SAVE];
    skills[3] = p->state.skills[SKILL_STEALTH];
    skills[4] = p->state.skills[SKILL_SEARCH_FREQUENCY];
    skills[5] = p->state.skills[SKILL_SEARCH];
    skills[6] = p->state.skills[SKILL_DISARM];
    skills[7] = p->state.skills[SKILL_DEVICE];

    /* Number of blows */
    skills[8] = p->state.num_blows;
    skills[9] = p->state.num_shots;

    /* Infravision */
    skills[10] = p->state.see_infra;

    Packet_printf(&connp->c, "%b", (unsigned)PKT_SKILLS);

    for (i = 0; i < 11; i++)
        Packet_printf(&connp->c, "%hd", (int)skills[i]);

    return 1;
}


int Send_pause(struct player *p)
{
    connection_t *connp = get_connp(p, "pause");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b", (unsigned)PKT_PAUSE);
}


int Send_monster_health(struct player *p, int num, byte attr)
{
    connection_t *connp = get_connp(p, "monster health");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%b", (unsigned)PKT_MONSTER_HEALTH,
        num, (unsigned)attr);
}


int Send_cursor(struct player *p, char vis, char x, char y)
{
    connection_t *connp = get_connp(p, "cursor");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%c%c", (unsigned)PKT_CURSOR,
        (int)vis, (int)x, (int)y);
}


int Send_poly(struct player *p, int r_idx)
{
    connection_t *connp = get_connp(p, "poly");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_POLY, r_idx);
}


int Send_weight(struct player *p, int weight, int max_weight)
{
    connection_t *connp = get_connp(p, "weight");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_WEIGHT, weight, max_weight);
}


int Send_channel(int ind, byte n, const char *virt)
{
    connection_t *connp = get_connp(player_get(ind), "channel");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%s", (unsigned)PKT_CHANNEL,
        (unsigned)n, (virt? virt: channels[n].name));
}


int Send_term_info(struct player *p, int mode, u16b arg)
{
    connection_t *connp = get_connp(p, "term info");
    if (connp == NULL) return 0;

    /* Hack -- Do not change terms too often */
    if (mode == NTERM_ACTIVATE)
    {
        if (p->remote_term == (byte)arg) return 1;
        p->remote_term = (byte)arg;
    }

    return Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_TERM, mode, (unsigned)arg);
}


int Send_remote_line(struct player *p, int y)
{
    connection_t *connp = get_connp(p, "remote line");
    if (connp == NULL) return 0;

    /* Packet header */
    Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_LINE_INFO, y, NORMAL_WID);

    /* Packet body */
    rle_encode(&connp->c, p->info[y], NORMAL_WID, DUNGEON_RLE_MODE(p));

    return 1;
}


int Send_reliable(int ind)
{
    connection_t *connp = get_connection(ind);
    int num_written;

    /*
     * Hack -- Make sure we have a valid socket to write to.
     * -1 is used to specify a player that has disconnected but is still "in game".
     */
    if (connp->w.sock == -1) return 0;

    if ((num_written = Sockbuf_write(&connp->w, connp->c.buf, connp->c.len)) != connp->c.len)
    {
        plog_fmt("Cannot write reliable data (%d,%d)", num_written, connp->c.len);
        Destroy_connection(ind, "Cannot write reliable data");
        return -1;
    }
    if ((num_written = Sockbuf_flush(&connp->w)) < 0)
    {
        plog_fmt("Cannot flush reliable data (%d)", num_written);
        Destroy_connection(ind, "Cannot flush reliable data");
        return -1;
    }
    Sockbuf_clear(&connp->c);
    return num_written;
}


int Send_player_pos(struct player *p)
{
    connection_t *connp = get_connp(p, "player pos");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd%hd", (unsigned)PKT_PLAYER, (int)p->px,
        (int)p->offset_x, (int)p->py, (int)p->offset_y);
}


/*** Receiving ***/


/* Forward declarations */
static int Receive_text_screen(int ind);
static int Receive_char_info(int ind);
static int Receive_verify_visual(int ind);
static int Receive_quit(int ind);
static int Receive_play(int ind);
static int Receive_undefined(int ind);
static int Receive_pass(int ind);
static int Receive_keepalive(int ind);
static int Receive_walk(int ind);
static int Receive_jump(int ind);
static int Receive_run(int ind);
static int Receive_tunnel(int ind);
static int Receive_aim_wand(int ind);
static int Receive_drop(int ind);
static int Receive_fire(int ind);
static int Receive_fire_at_nearest(int ind);
static int Receive_pickup(int ind);
static int Receive_quest(int ind);
static int Receive_destroy(int ind);
static int Receive_target_closest(int ind);
static int Receive_spell(int ind);
static int Receive_observe(int ind);
static int Receive_open(int ind);
static int Receive_pray(int ind);
static int Receive_ghost(int ind);
static int Receive_mimic(int ind);
static int Receive_quaff(int ind);
static int Receive_read(int ind);
static int Receive_search(int ind);
static int Receive_take_off(int ind);
static int Receive_use(int ind);
static int Receive_throw(int ind);
static int Receive_wield(int ind);
static int Receive_zap(int ind);
static int Receive_target_interactive(int ind);
static int Receive_inscribe(int ind);
static int Receive_uninscribe(int ind);
static int Receive_activate(int ind);
static int Receive_bash(int ind);
static int Receive_alter(int ind);
static int Receive_spike(int ind);
static int Receive_disarm(int ind);
static int Receive_eat(int ind);
static int Receive_fill(int ind);
static int Receive_locate(int ind);
static int Receive_map(int ind);
static int Receive_fullmap(int ind);
static int Receive_search_mode(int ind);
static int Receive_close(int ind);
static int Receive_gain(int ind);
static int Receive_go_up(int ind);
static int Receive_go_down(int ind);
static int Receive_message(int ind);
static int Receive_item(int ind);
static int Receive_purchase(int ind);
static int Receive_sell(int ind);
static int Receive_store_examine(int ind);
static int Receive_store_order(int ind);
static int Receive_store_leave(int ind);
static int Receive_store_confirm(int ind);
static int Receive_drop_gold(int ind);
static int Receive_redraw(int ind);
static int Receive_rest(int ind);
static int Receive_party(int ind);
static int Receive_special_line(int ind);
static int Receive_symbol_query(int ind);
static int Receive_monlist(int ind);
static int Receive_objlist(int ind);
static int Receive_steal(int ind);
static int Receive_options(int ind);
static int Receive_suicide(int ind);
static int Receive_master(int ind);
static int Receive_poly(int ind);
static int Receive_social(int ind);
static int Receive_feeling(int ind);
static int Receive_clear(int ind);
static int Receive_breath(int ind);
static int Receive_channel(int ind);
static int Receive_interactive(int ind);
static int Receive_fountain(int ind);
static int Receive_icky(int ind);
static int Receive_center(int ind);
static int Receive_ignore(int ind);
static int Receive_use_any(int ind);


/*
 * Initialize the function dispatch tables for the various client
 * connection states.  Some states use the same table.
 */
static void Init_receive(void)
{
    int i;

    for (i = 0; i < 256; i++)
    {
        setup_receive[i]                = Receive_undefined;
        playing_receive[i]              = Receive_undefined;
    }

    setup_receive[PKT_VERIFY]           = Receive_verify_visual;
    setup_receive[PKT_PLAY]             = Receive_play;
    setup_receive[PKT_QUIT]             = Receive_quit;
    setup_receive[PKT_TEXT_SCREEN]      = Receive_text_screen;
    setup_receive[PKT_KEEPALIVE]        = Receive_keepalive;
    setup_receive[PKT_CHAR_INFO]        = Receive_char_info;
    setup_receive[PKT_OPTIONS]          = Receive_options;
    setup_receive[PKT_ICKY]             = Receive_icky;

    playing_receive[PKT_PLAY]           = Receive_play;
    playing_receive[PKT_QUIT]           = Receive_quit;
    playing_receive[PKT_TEXT_SCREEN]    = Receive_text_screen;
    playing_receive[PKT_KEEPALIVE]      = Receive_keepalive;
    playing_receive[PKT_MESSAGE]        = Receive_message;
    playing_receive[PKT_ITEM]           = Receive_item;
    playing_receive[PKT_SELL]           = Receive_sell;
    playing_receive[PKT_PARTY]          = Receive_party;
    playing_receive[PKT_SPECIAL_LINE]   = Receive_special_line;
    playing_receive[PKT_FULLMAP]        = Receive_fullmap;
    playing_receive[PKT_SYMBOL_QUERY]   = Receive_symbol_query;
    playing_receive[PKT_POLY]           = Receive_poly;
    playing_receive[PKT_BREATH]         = Receive_breath;
    playing_receive[PKT_WALK]           = Receive_walk;
    playing_receive[PKT_RUN]            = Receive_run;
    playing_receive[PKT_TUNNEL]         = Receive_tunnel;
    playing_receive[PKT_AIM_WAND]       = Receive_aim_wand;
    playing_receive[PKT_DROP]           = Receive_drop;
    playing_receive[PKT_FIRE]           = Receive_fire;
    playing_receive[PKT_PICKUP]         = Receive_pickup;
    playing_receive[PKT_DESTROY]        = Receive_destroy;
    playing_receive[PKT_TARGET_CLOSEST] = Receive_target_closest;
    playing_receive[PKT_SPELL]          = Receive_spell;
    playing_receive[PKT_OPEN]           = Receive_open;
    playing_receive[PKT_PRAY]           = Receive_pray;
    playing_receive[PKT_QUAFF]          = Receive_quaff;
    playing_receive[PKT_READ]           = Receive_read;
    playing_receive[PKT_SEARCH]         = Receive_search;
    playing_receive[PKT_TAKE_OFF]       = Receive_take_off;
    playing_receive[PKT_USE]            = Receive_use;
    playing_receive[PKT_THROW]          = Receive_throw;
    playing_receive[PKT_WIELD]          = Receive_wield;
    playing_receive[PKT_ZAP]            = Receive_zap;
    playing_receive[PKT_TARGET]         = Receive_target_interactive;
    playing_receive[PKT_INSCRIBE]       = Receive_inscribe;
    playing_receive[PKT_UNINSCRIBE]     = Receive_uninscribe;
    playing_receive[PKT_ACTIVATE]       = Receive_activate;
    playing_receive[PKT_BASH]           = Receive_bash;
    playing_receive[PKT_DISARM]         = Receive_disarm;
    playing_receive[PKT_EAT]            = Receive_eat;
    playing_receive[PKT_FILL]           = Receive_fill;
    playing_receive[PKT_LOCATE]         = Receive_locate;
    playing_receive[PKT_MAP]            = Receive_map;
    playing_receive[PKT_SEARCH_MODE]    = Receive_search_mode;
    playing_receive[PKT_SPIKE]          = Receive_spike;
    playing_receive[PKT_QUEST]          = Receive_quest;
    playing_receive[PKT_CLOSE]          = Receive_close;
    playing_receive[PKT_GAIN]           = Receive_gain;
    playing_receive[PKT_GO_UP]          = Receive_go_up;
    playing_receive[PKT_GO_DOWN]        = Receive_go_down;
    playing_receive[PKT_PURCHASE]       = Receive_purchase;
    playing_receive[PKT_STORE_LEAVE]    = Receive_store_leave;
    playing_receive[PKT_STORE_CONFIRM]  = Receive_store_confirm;
    playing_receive[PKT_DROP_GOLD]      = Receive_drop_gold;
    playing_receive[PKT_REDRAW]         = Receive_redraw;
    playing_receive[PKT_REST]           = Receive_rest;
    playing_receive[PKT_GHOST]          = Receive_ghost;
    playing_receive[PKT_SUICIDE]        = Receive_suicide;
    playing_receive[PKT_STEAL]          = Receive_steal;
    playing_receive[PKT_OPTIONS]        = Receive_options;
    playing_receive[PKT_MASTER]         = Receive_master;
    playing_receive[PKT_MIMIC]          = Receive_mimic;
    playing_receive[PKT_CLEAR]          = Receive_clear;
    playing_receive[PKT_OBSERVE]        = Receive_observe;
    playing_receive[PKT_STORE_EXAMINE]  = Receive_store_examine;
    playing_receive[PKT_STORE_ORDER]    = Receive_store_order;
    playing_receive[PKT_CHANGEPASS]     = Receive_pass;
    playing_receive[PKT_ALTER]          = Receive_alter;
    playing_receive[PKT_FIRE_AT_NEAREST]    = Receive_fire_at_nearest;
    playing_receive[PKT_JUMP]           = Receive_jump;
    playing_receive[PKT_CHANNEL]        = Receive_channel;
    playing_receive[PKT_SOCIAL]         = Receive_social;
    playing_receive[PKT_MONLIST]        = Receive_monlist;
    playing_receive[PKT_FEELING]        = Receive_feeling;
    playing_receive[PKT_INTERACTIVE]    = Receive_interactive;
    playing_receive[PKT_FOUNTAIN]       = Receive_fountain;
    playing_receive[PKT_ICKY]           = Receive_icky;
    playing_receive[PKT_OBJLIST]        = Receive_objlist;
    playing_receive[PKT_CENTER]         = Receive_center;
    playing_receive[PKT_IGNORE]         = Receive_ignore;
    playing_receive[PKT_USE_ANY]        = Receive_use_any;
}


static int Receive_quit(int ind)
{
    int n;
    connection_t *connp = get_connection(ind);
    char ch;

    if ((n = Packet_scanf(&connp->r, "%c", &ch)) != 1)
    {
        errno = 0;
        Destroy_connection(ind, "Quit receive error");
        return -1;
    }

    do_quit(ind);

    return 1;
}


/*
 * Check if screen size is compatible
 */
static bool screen_compatible(int ind)
{
    connection_t *connp = get_connection(ind);
    int cols = connp->Client_setup.settings[SETTING_SCREEN_COLS];
    int rows = connp->Client_setup.settings[SETTING_SCREEN_ROWS];
    int tile_wid = connp->Client_setup.settings[SETTING_TILE_WID];
    int tile_hgt = connp->Client_setup.settings[SETTING_TILE_HGT];

    /* Hack - Ensure his settings are allowed, disconnect otherwise */
    if ((cols < Setup.min_col) || (cols > Setup.max_col * tile_wid) ||
        (rows < Setup.min_row) || (rows > Setup.max_row * tile_hgt))
    {
        char buf[255];

        errno = 0;
        strnfmt(buf, sizeof(buf), "Incompatible screen size %dx%d (min %dx%d, max %dx%d).",
            cols, rows, Setup.min_col, Setup.min_row, Setup.max_col * tile_wid,
            Setup.max_row * tile_hgt);
        Destroy_connection(ind, buf);

        return FALSE;
    }

    return TRUE;
}


static void update_graphics(player_type *p_ptr, connection_t *connp)
{
    int i, j;

    /* Desired features */
    for (i = 0; i < z_info->f_max; i++)
    {
        for (j = 0; j < FEAT_LIGHTING_MAX; j++)
        {
            /* Ignore mimics */
            if (f_info[i].mimic != i)
            {
                p_ptr->f_attr[i][j] = connp->Client_setup.f_attr[f_info[i].mimic][j];
                p_ptr->f_char[i][j] = connp->Client_setup.f_char[f_info[i].mimic][j];
            }
            else
            {
                p_ptr->f_attr[i][j] = connp->Client_setup.f_attr[i][j];
                p_ptr->f_char[i][j] = connp->Client_setup.f_char[i][j];
            }

            if (!(p_ptr->f_attr[i][j] && p_ptr->f_char[i][j]))
            {
                p_ptr->f_attr[i][j] = f_info[i].x_attr[j];
                p_ptr->f_char[i][j] = f_info[i].x_char[j];
            }
        }
    }

    /* Desired objects */
    for (i = 0; i < z_info->k_max; i++)
    {
        /* Desired aware objects */
        p_ptr->k_attr[i] = connp->Client_setup.k_attr[i];
        p_ptr->k_char[i] = connp->Client_setup.k_char[i];

        /* Flavored objects */
        if (k_info[i].flavor)
        {
            /* Use flavor attr/char for unaware flavored objects */
            p_ptr->d_attr[i] = connp->Client_setup.flvr_x_attr[k_info[i].flavor->fidx];
            p_ptr->d_char[i] = connp->Client_setup.flvr_x_char[k_info[i].flavor->fidx];

            /* Use flavor attr/char as default for aware flavored objects */
            if (!(p_ptr->k_attr[i] && p_ptr->k_char[i]))
            {
                p_ptr->k_attr[i] = p_ptr->d_attr[i];
                p_ptr->k_char[i] = p_ptr->d_char[i];
            }

            if (!(p_ptr->d_attr[i] && p_ptr->d_char[i]))
            {
                p_ptr->d_attr[i] = k_info[i].flavor->x_attr;
                p_ptr->d_char[i] = k_info[i].flavor->x_char;
            }
        }

        else
        {
            /* Unflavored objects don't get redefined when aware */
            p_ptr->d_attr[i] = connp->Client_setup.k_attr[i];
            p_ptr->d_char[i] = connp->Client_setup.k_char[i];

            if (!(p_ptr->d_attr[i] && p_ptr->d_char[i]))
            {
                p_ptr->d_attr[i] = k_info[i].x_attr;
                p_ptr->d_char[i] = k_info[i].x_char;
            }
        }

        /* Default aware objects */
        if (!(p_ptr->k_attr[i] && p_ptr->k_char[i]))
        {
            p_ptr->k_attr[i] = p_ptr->d_attr[i];
            p_ptr->k_char[i] = p_ptr->d_char[i];
        }
    }

    /* Desired monsters */
    for (i = 0; i < z_info->r_max; i++)
    {
        p_ptr->r_attr[i] = connp->Client_setup.r_attr[i];
        p_ptr->r_char[i] = connp->Client_setup.r_char[i];

        if (!(p_ptr->r_attr[i] && p_ptr->r_char[i]))
        {
            p_ptr->r_attr[i] = r_info[i].x_attr;
            p_ptr->r_char[i] = r_info[i].x_char;
        }
    }

    /* Desired inventory objects */
    for (i = 0; i < 128; i++)
    {
        p_ptr->tval_attr[i] = connp->Client_setup.tval_attr[i];

        if (!p_ptr->tval_attr[i])
            p_ptr->tval_attr[i] = tval_to_attr[i];
    }

    /* Desired special things */
    for (i = 0; i < GF_MAX; i++)
    {
        for (j = 0; j < BOLT_MAX; j++)
        {
            p_ptr->gf_attr[i][j] = connp->Client_setup.gf_attr[i][j];
            p_ptr->gf_char[i][j] = connp->Client_setup.gf_char[i][j];

            if (!(p_ptr->gf_attr[i][j] && p_ptr->gf_char[i][j]))
            {
                p_ptr->gf_attr[i][j] = gf_to_attr[i][j];
                p_ptr->gf_char[i][j] = gf_to_char[i][j];
            }
        }
    }
}


/*
 * A client has requested to start active play.
 * See if we can allocate a player structure for it
 * and if this succeeds update the player information
 * to all connected players.
 */
static int Enter_player(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    int i;
    char buf[255];
    bool old_ironman, old_no_stores, old_no_artifacts, old_no_feelings, old_no_selling,
        old_no_ghost, old_fruit_bat;
    s16b roller = connp->stat_roll[A_MAX];
    int build = VERSION_EXTRA;

    if (NumConnections >= MAX_PLAYERS)
    {
        errno = 0;
        plog_fmt("Too many connections (%d)", NumConnections);
        return -2;
    }

    for (i = 1; i < NumPlayers + 1; i++)
    {
        if (strcasecmp(player_get(i)->name, connp->nick) == 0)
        {
            errno = 0;
            plog_fmt("Name already in use %s", connp->nick);
            Destroy_connection(ind, "Name already in use");
            return -1;
        }
    }

    /* Hack -- Ensure his settings are allowed, disconnect otherwise */
    if (!screen_compatible(ind)) return -1;

    p_ptr = player_birth(NumPlayers + 1, connp->account, connp->nick, connp->pass, ind, connp->ridx,
        connp->cidx, connp->psex, connp->stat_roll);
    if (!p_ptr)
    {
        /* Failed, connection already destroyed */
        return -1;
    }

    my_strcpy(p_ptr->other.full_name, connp->real, sizeof(p_ptr->other.full_name));
    my_strcpy(p_ptr->hostname, connp->host, sizeof(p_ptr->hostname));
    my_strcpy(p_ptr->addr, connp->addr, sizeof(p_ptr->addr));
    p_ptr->version = connp->version;

    /* Initialise message ptr before we start sending messages */
    p_ptr->msg_hist_ptr = 0;

    /* Copy the client preferences to the player struct */
    old_ironman = OPT_P(p_ptr, birth_ironman);
    old_no_stores = OPT_P(p_ptr, birth_no_stores);
    old_no_artifacts = OPT_P(p_ptr, birth_no_artifacts);
    old_no_feelings = OPT_P(p_ptr, birth_no_feelings);
    old_no_selling = OPT_P(p_ptr, birth_no_selling);
    old_no_ghost = OPT_P(p_ptr, birth_no_ghost);
    old_fruit_bat = OPT_P(p_ptr, birth_fruit_bat);
    for (i = 0; i < OPT_MAX; i++)
        p_ptr->other.opt[i] = connp->Client_setup.options[i];

    /* Birth options: must have 0 xp to be applied */
    if (p_ptr->max_exp > 0)
    {
        OPT_P(p_ptr, birth_ironman) = old_ironman;
        OPT_P(p_ptr, birth_no_stores) = old_no_stores;
        OPT_P(p_ptr, birth_no_artifacts) = old_no_artifacts;
        OPT_P(p_ptr, birth_no_feelings) = old_no_feelings;
        OPT_P(p_ptr, birth_no_selling) = old_no_selling;
        OPT_P(p_ptr, birth_no_ghost) = old_no_ghost;
        OPT_P(p_ptr, birth_fruit_bat) = old_fruit_bat;
    }

    /* Fruit bat mode: not when a Dragon, a Shapechanger or a Necromancer */
    if (pf_has(p_ptr->race->pflags, PF_DRAGON) || pf_has(p_ptr->clazz->pflags, PF_MONSTER_SPELLS) ||
        pf_has(p_ptr->clazz->pflags, PF_UNDEAD_POWERS))
    {
        OPT_P(p_ptr, birth_fruit_bat) = FALSE;
    }
    if (OPT_P(p_ptr, birth_fruit_bat) != old_fruit_bat)
        do_cmd_poly(p_ptr, (OPT_P(p_ptr, birth_fruit_bat)? get_r_idx("Fruit bat"): 0), FALSE, FALSE);

    /* Update graphics */
    update_graphics(p_ptr, connp);

    /* Hack -- Process "settings" */
    p_ptr->use_graphics = connp->Client_setup.settings[SETTING_USE_GRAPHICS];
    p_ptr->screen_cols = connp->Client_setup.settings[SETTING_SCREEN_COLS];
    p_ptr->screen_rows = connp->Client_setup.settings[SETTING_SCREEN_ROWS];
    p_ptr->tile_wid = connp->Client_setup.settings[SETTING_TILE_WID];
    p_ptr->tile_hgt = connp->Client_setup.settings[SETTING_TILE_HGT];
    p_ptr->tile_distorted = connp->Client_setup.settings[SETTING_TILE_DISTORTED];
    p_ptr->max_hgt = connp->Client_setup.settings[SETTING_MAX_HGT];
    p_ptr->window_flag = connp->Client_setup.settings[SETTING_WINDOW_FLAG];
    p_ptr->other.hitpoint_warn = connp->Client_setup.settings[SETTING_HITPOINT_WARN];
    p_ptr->other.delay_factor = connp->Client_setup.settings[SETTING_DELAY_FACTOR];
    for (i = 0; i < TYPE_MAX; i++)
        p_ptr->other.squelch_lvl[i] = connp->Client_setup.settings[i + SETTING_SQUELCH_JEWELRY];

    /*
     * Hack -- When processing a quickstart character, attr/char pair for
     * player picture is incorrect
     */
    if ((roller < 0) && p_ptr->use_graphics)
    {
        byte cidx = p_ptr->clazz->cidx;
        byte ridx = p_ptr->race->ridx;

        p_ptr->r_attr[0] = player_presets[p_ptr->use_graphics - 1][cidx][ridx][p_ptr->psex].a;
        p_ptr->r_char[0] = player_presets[p_ptr->use_graphics - 1][cidx][ridx][p_ptr->psex].c;
    }

    verify_panel(p_ptr);

    NumPlayers++;

    connp->id = NumConnections;
    set_player_index(connp, NumPlayers);

    NumConnections++;

    Send_game_start_conn(ind);

    Conn_set_state(connp, CONN_PLAYING);

    /* Send party information */
    Send_party(p_ptr);

    /* Send channel */
    Send_channel(NumPlayers, 0, NULL);

    /* Send him his history */
    for (i = 0; i < N_HIST_LINES; i++)
        Send_history(NumPlayers, i, p_ptr->history[i]);

    /* Send him his Various info (age, etc.) */
    Send_various(NumPlayers, p_ptr->ht, p_ptr->wt, p_ptr->age, p_ptr->sc);

    num_logins++;

    /* Report */
    strnfmt(buf, sizeof(buf), "%s=%s@%s (%s) connected.", p_ptr->name, p_ptr->other.full_name,
        p_ptr->hostname, p_ptr->addr);
    debug(buf);

    /* Tell the new player about server configuration options */
    if (cfg_more_towns)
        msg(p_ptr, "Server has static dungeon towns.");
    if (cfg_limit_stairs == 1)
        msg(p_ptr, "Server has non-connected stairs.");
    if (cfg_limit_stairs == 2)
        msg(p_ptr, "Server is no-up.");
    if (cfg_no_recall)
        msg(p_ptr, "Server is no-recall.");
    if (cfg_no_ghost)
        msg(p_ptr, "Server is no-ghost.");

    /* Tell the new player about the version number */
    if (build > 0)
    {
        msgt(p_ptr, MSG_VERSION, "Server is running version %s (Build %d)", get_buildver(),
            VERSION_EXTRA);
    }
    else
        msgt(p_ptr, MSG_VERSION, "Server is running version %s (Beta)", get_buildver());

    msg(p_ptr, "  ");
    msg(p_ptr, "   ");
    msg(p_ptr, "====================");
    msg(p_ptr, "  ");
    msg(p_ptr, "   ");

    /* Report delayed info */
    Send_poly(p_ptr, p_ptr->r_idx);
    Send_quiver_size(p_ptr, p_ptr->quiver_size, p_ptr->quiver_slots, p_ptr->quiver_remainder);
    p_ptr->delayed_display = TRUE;
    p_ptr->update |= (PU_BONUS | PU_MANA | PU_SPELLS);
    update_stuff(p_ptr);
    p_ptr->delayed_display = FALSE;
    if (random_level(p_ptr->depth)) display_feeling(p_ptr, FALSE);

    /* Handle the cfg_secret_dungeon_master option */
    if (p_ptr->dm_flags & DM_SECRET_PRESENCE) return 0;

    /* Tell everyone about our new player */
    if (p_ptr->exp == 0)
        strnfmt(buf, sizeof(buf), "%s begins a new game.", p_ptr->name);
    else
        strnfmt(buf, sizeof(buf), "%s has entered the game.", p_ptr->name);

    msg_broadcast(p_ptr, buf);

    /* Tell the meta server about the new player */
    Report_to_meta(META_UPDATE);

    return 0;
}


static bool Limit_connections(int ind)
{
    connection_t *connp = get_connection(ind);
    int i;

    /* Check all connections */
    for (i = 0; i < MAX_PLAYERS; i++)
    {
        connection_t *current;

        /* Skip current connection */
        if (i == ind) continue;

        /* Get connection */
        current = get_connection(i);

        /* Skip invalid connections */
        if (current->state == CONN_FREE) continue;
        if (current->state == CONN_CONSOLE) continue;

        /* Check name */
        if (strcasecmp(current->nick, connp->nick) == 0)
        {
            Destroy_connection(i, "Resume connection");
            return FALSE;
        }
    }

    /* Check all players */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Skip current connection */
        if (p_ptr->conn == ind) continue;

        /* Check name */
        if (strcasecmp(p_ptr->name, connp->nick) == 0)
        {
            /*
             * The following code allows you to "override" an
             * existing connection by connecting again
             */
             Destroy_connection(p_ptr->conn, "Resume connection");
             return FALSE;
        }

        /* Only one connection allowed? */
        if (cfg_limit_player_connections &&
            !strcasecmp(p_ptr->other.full_name, connp->real) &&
            !strcasecmp(p_ptr->addr, connp->addr) &&
            !strcasecmp(p_ptr->hostname, connp->host) &&
             strcasecmp(connp->nick, cfg_dungeon_master) &&
             strcasecmp(p_ptr->name, cfg_dungeon_master))
        {
            return TRUE;
        }
    }

    return FALSE;
}


static int Receive_play(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch, mode;
    char nick[NORMAL_WID];
    char pass[NORMAL_WID];
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%b%s%s", &ch, &mode, nick, pass)) != 4)
    {
        errno = 0;
        plog("Cannot receive play packet");
        Destroy_connection(ind, "Cannot receive play packet");
        return -1;
    }

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not in setup state (%02x)", connp->state);
        Destroy_connection(ind, "Connection not in setup state");
        return -1;
    }

    /* Just asking */
    if (mode == 0)
    {
        hash_entry *ptr;
        bool need_info = FALSE;
        byte ridx = 0, cidx = 0, psex = 0;
        int ret;
        int pos = strlen(nick);

        /* Get a character dump */
        if (nick[pos - 1] == '=')
        {
            char dumpname[42];
            char pathname[MSG_LEN];
            char buf[MSG_LEN];
            ang_file *fp;

            nick[pos - 1] = '\0';
            strnfmt(dumpname, sizeof(dumpname), "%s.txt", nick);

            /* Build the filename */
            path_build(pathname, MSG_LEN, ANGBAND_DIR_APEX, dumpname);

            /* Open the file for reading */
            fp = file_open(pathname, MODE_READ, FTYPE_TEXT);
            if (!fp)
            {
                Destroy_connection(ind, "Character dump failed");
                return -1;
            }

            /* Begin sending */
            Packet_printf(&connp->c, "%b%s", (unsigned)PKT_CHAR_DUMP, "BEGIN");

            /* Process the file */
            while (file_getl(fp, buf, sizeof(buf)))
                Packet_printf(&connp->c, "%b%s", (unsigned)PKT_CHAR_DUMP, buf);

            /* End sending */
            Packet_printf(&connp->c, "%b%s", (unsigned)PKT_CHAR_DUMP, "END");

            /* Close the file */
            file_close(fp);
        }

        /* Delete character */
        if (nick[pos - 1] == '-')
        {
            nick[pos - 1] = '\0';
            delete_player_name(nick);
            plog("Character deleted");
            Destroy_connection(ind, "Character deleted");
            return -1;
        }

        /* Play a new incarnation */
        if (nick[pos - 1] == '+')
        {
            nick[pos - 1] = '\0';
            delete_player_name(nick);
            get_incarnation(1, nick, sizeof(nick));
        }

        /* Check if this name is valid */
        if (Check_names(nick, "dummy", "dummy"))
        {
            plog("Invalid name");
            Destroy_connection(ind, "Invalid name");
            return -1;
        }

        /* Check if a character with this name exists */
        ptr = lookup_player_by_name(nick);
        if (!ptr)
        {
            /* New player */
            need_info = TRUE;

            /* Check number of characters */
            if (player_id_count(connp->account) == MAX_ACCOUNT_CHARS)
            {
                plog("Account is full");
                Destroy_connection(ind, "Account is full");
                return -1;
            }
        }

        /* Check that player really belongs to this account */
        else if (ptr->account && (ptr->account != connp->account))
        {
            plog("Invalid account");
            Destroy_connection(ind, "Invalid account");
            return -1;
        }

        /* Check if character is alive */
        else if (!ht_zero(&ptr->death_turn))
        {
            /* Dead player */
            need_info = TRUE;
        }

        /* Test if his password is matching */
        if (!need_info)
        {
            ret = scoop_player(nick, pass, &ridx, &cidx, &psex);

            /* Incorrect Password */
            if (ret == -2)
            {
                plog("Incorrect password");
                Destroy_connection(ind, "Incorrect password");
                return -1;
            }

            /* Technical Error */
            if (ret == -1)
            {
                plog("Error accessing savefile");
                Destroy_connection(ind, "Error accessing savefile");
                return -1;
            }

            /* Nickname exists, but not real char or new player */
            if (ret > 0) need_info = TRUE;
        }

        /* Set character connection info */
        string_free(connp->nick);
        string_free(connp->pass);
        connp->nick = string_make(nick);
        connp->pass = string_make(pass);
        connp->char_state = (need_info? 0: 1);
        connp->ridx = ridx;
        connp->cidx = cidx;
        connp->psex = psex;

        if ((connp->nick == NULL) || (connp->pass == NULL))
        {
            plog("Not enough memory for connection");
            Destroy_connection(ind, "Not enough memory for connection");
            return -1;
        }

        /* Let's see if he's already connected */
        if (Limit_connections(ind))
        {
            plog("Only one connection allowed");
            Destroy_connection(ind, "Only one connection allowed");
            return -1;
        }

        Send_basic_info_conn(ind);
        Send_limits_info_conn(ind);
        Send_kind_info_conn(ind);
        Send_inven_info_conn(ind);
        Send_race_info_conn(ind);
        Send_class_info_conn(ind);
        Send_socials_info_conn(ind);
        Send_hints_info_conn(ind);
        Send_rinfo_info_conn(ind);
        Send_char_info_conn(ind);
    }
    else
    {
        /* Trying to start gameplay! */
        if ((n = Enter_player(ind)) == -2)
        {
            errno = 0;
            plog_fmt("Unable to play (%02x)", connp->state);
            Destroy_connection(ind, "Unable to play");
        }
        if (n < 0)
        {
            /* The connection has already been destroyed */
            return -1;
        }
    }

    return 2;
}


static int Receive_undefined(int ind)
{
    connection_t *connp = get_connection(ind);
    byte what = (byte)connp->r.ptr[0];

    errno = 0;
    plog_fmt("Unknown packet type %s (%03d,%02x)", connp->nick, what, connp->state);
    Destroy_connection(ind, "Unknown packet type");
    return -1;
}


/*
 * Return codes for the "Receive_XXX" functions are as follows:
 *
 * -1 --> Some error occured
 *  0 --> The action was queued (not enough energy)
 *  1 --> The action was ignored (not enough energy)
 *  2 --> The action completed successfully
 *
 *  Every code except for 1 will cause the input handler to stop
 *  processing actions.
 */
static int Receive_keepalive(int ind)
{
    int n;
    connection_t *connp = get_connection(ind);
    byte ch;
    u32b ctime;

    if ((n = Packet_scanf(&connp->r, "%b%lu", &ch, &ctime)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Keepalive read error");
        return n;
    }
    Packet_printf(&connp->c, "%b%lu", (unsigned)PKT_KEEPALIVE, ctime);

    return 2;
}


static int Receive_verify_visual(int ind)
{
    connection_t *connp = get_connection(ind);
    int n, i, local_size = 0;
    byte ch;
    char type;
    s16b size;
    byte a;
    char c;
    bool discard = FALSE;
    char *char_ref;
    byte *attr_ref;

    type = size = 0;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &type, &size)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "verify_visual read error");
        return n;
    }

    /* Gather type */
    switch (type)
    {
        case 0:
        {
            local_size = get_flavor_max();
            attr_ref = connp->Client_setup.flvr_x_attr;
            char_ref = connp->Client_setup.flvr_x_char;
            break;
        }
        case 1:
        {
            local_size = z_info->f_max * FEAT_LIGHTING_MAX;
            attr_ref = NULL;
            char_ref = NULL;
            break;
        }
        case 2:
        {
            local_size = z_info->k_max;
            attr_ref = connp->Client_setup.k_attr;
            char_ref = connp->Client_setup.k_char;
            break;
        }
        case 3:
        {
            local_size = z_info->r_max;
            attr_ref = connp->Client_setup.r_attr;
            char_ref = connp->Client_setup.r_char;
            break;
        }
        case 4:
        {
            local_size = 128;
            attr_ref = connp->Client_setup.tval_attr;
            char_ref = NULL;
            break;
        }
        case 5:
        {
            local_size = GF_MAX * BOLT_MAX;
            attr_ref = NULL;
            char_ref = NULL;
            break;
        }
        default:
            discard = TRUE;
    }
    if (local_size != size) discard = TRUE;

    /* Finally read the data */
    for (i = 0; i < size; i++)
    {
        if ((n = Packet_scanf(&connp->r, "%b%c", &a, &c)) <= 0)
        {
            if (n == -1) Destroy_connection(ind, "verify_visual read error");
            return n;
        }
        if (!discard)
        {
            /* Hack -- Features */
            if (type == 1)
            {
                connp->Client_setup.f_attr[i / FEAT_LIGHTING_MAX][i % FEAT_LIGHTING_MAX] = a;
                connp->Client_setup.f_char[i / FEAT_LIGHTING_MAX][i % FEAT_LIGHTING_MAX] = c;
            }

            /* Hack -- Special effects */
            else if (type == 5)
            {
                connp->Client_setup.gf_attr[i / BOLT_MAX][i % BOLT_MAX] = a;
                connp->Client_setup.gf_char[i / BOLT_MAX][i % BOLT_MAX] = c;
            }

            else
            {
                attr_ref[i] = a;
                if (char_ref) char_ref[i] = c;
            }
        }
    }

    return 2;
}


static int Receive_text_screen(int ind)
{
    connection_t *connp = get_connection(ind);
    int n;
    byte ch;
    s16b type;
    s32b off;

    if ((n = Packet_scanf(&connp->r, "%b%hd%ld", &ch, &type, &off)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "text_screen read error");
        return n;
    }

    Send_text_screen(ind, type, off);

    return 2;
}


static int Receive_char_info(int ind)
{
    connection_t *connp = get_connection(ind);
    int n, i;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%b%b%b", &ch, &connp->ridx, &connp->cidx,
        &connp->psex)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "char_info read error");
        return n;
    }

    /* Roller */
    for (i = 0; i <= A_MAX; i++)
    {
        if ((n = Packet_scanf(&connp->r, "%hd", &connp->stat_roll[i])) == -1)
        {
            Destroy_connection(ind, "misread stat order");
            return n;
        }
    }

    /* Have Template */
    connp->char_state = 1;

    Send_char_info_conn(ind);

    return 2;
}


static int Receive_walk(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    char dir;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_walk read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        /* Disturb if running or resting */
        if (p_ptr->running || p_ptr->resting)
        {
            disturb(p_ptr, 0, 0);
            return 1;
        }

        if (has_energy(player))
        {
            do_cmd_walk(player, dir);
            return 2;
        }

        /*
         * If we have no commands queued, then queue our walk request.
         */
        if (!connp->q.len)
        {
            Packet_printf(&connp->q, "%b%c", (unsigned)PKT_WALK, (int)dir);
            return 0;
        }

        /*
         * If we have a walk command queued at the end of the queue,
         * then replace it with this queue request.
         */
        if (connp->q.buf[connp->q.len - 2] == PKT_WALK)
        {
            connp->q.len -= 2;
            Packet_printf(&connp->q, "%b%c", (unsigned)PKT_WALK, (int)dir);
            return 0;
        }
    }

    return 1;
}  


static int Receive_jump(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    char dir;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_jump read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        /* Disturb if running or resting */
        if (p_ptr->running || p_ptr->resting)
        {
            disturb(p_ptr, 0, 0);
            return 1;
        }

        if (has_energy(player))
        {
            do_cmd_jump(player, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_run(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    char dir;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_run read error");
        return n;
    }

    /* Hack -- Fix the running in '5' bug */
    if (dir == 5) return 1;

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        /* Start running */
        if (has_energy(player))
        {
            do_cmd_run(player, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_tunnel(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    char dir;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_tunnel read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_tunnel(player, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_aim_wand(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    s16b item;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_aim_wand read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_aim_wand(player, item, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)item, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_drop(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;
    s16b item, amt;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &amt)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_drop read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_drop(player, item, amt);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%hd", (unsigned)ch, (int)item, (int)amt);
        return 0;
    }

    return 1;
}


static int Receive_fire(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    s16b item;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &dir, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_fire read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_fire(player, dir, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c%hd", (unsigned)ch, (int)dir, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_fire_at_nearest(int ind)
{
    connection_t *connp = get_connection(ind);
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_fire_at_nearest read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_fire_at_nearest(player);
            return 2;
        }

        Packet_printf(&connp->q, "%b", (unsigned)ch);
        return 0;
    }

    return 1;
}


static int Receive_pickup(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    int n, player;
    byte squelch;

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &squelch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_pickup read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        switch (squelch)
        {
            case 0:
            {
                /* Stand still */
                p_ptr->squelch = 0;
                do_cmd_autopickup(player);
                do_cmd_pickup(player);
                break;
            }

            case 1:
            {
                /* Pick up objects */
                p_ptr->squelch = 1;
                do_cmd_pickup(player);
                break;
            }

            case 2:
            {
                /* Do autopickup */
                p_ptr->squelch = 1;
                do_cmd_autopickup(player);
                break;
            }
        }

        return 2;
    }

    return 1;
}


static int Receive_quest(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_quest read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_quest(player);
        return 2;
    }

    return 1;
}


static int Receive_destroy(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item, des;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &des)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_destroy read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_destroy(player, item, (bool)des);
        return 2;
    }

    return 1;
}


static int Receive_target_closest(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch, mode;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &mode)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_target_closest read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        target_set_closest(player, mode);
    }

    return 1;
}


static int Receive_observe(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_observe read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_observe(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_cast(int ind, char* errmsg)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    s16b book, spell;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd%c", &ch, &book, &spell, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, errmsg);
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_cast(player, book, spell, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%hd%c", (unsigned)ch, (int)book, (int)spell, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_spell(int ind)
{
    return Receive_cast(ind, "Receive_spell read error");
}


static int Receive_pray(int ind)
{
    return Receive_cast(ind, "Receive_pray read error");
}


static int Receive_open(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir, easy;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%c", &ch, &dir, &easy)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_open read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_open(player, dir, (bool)easy);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c%c", (unsigned)ch, (int)dir, (int)easy);
        return 0;
    }

    return 1;
}


static int Receive_ghost(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    s16b ability;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &ability, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_ghost read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_ghost(player, ability, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)ability, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_quaff(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_quaff read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_quaff_potion(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_read(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_read read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_read_scroll(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_search(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_search read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_search(player);
            return 2;
        }
    }

    return 1;
}


static int Receive_take_off(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_take_off read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_takeoff(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_use(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_use read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_use_staff(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_throw(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    s16b item;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &dir, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_throw read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_throw(player, dir, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c%hd", (unsigned)ch, (int)dir, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_wield(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item, slot;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &slot)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_wield read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_wield(player, item, slot);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%hd", (unsigned)ch, (int)item, (int)slot);
        return 0;
    }

    return 1;
}


static int Receive_zap(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    s16b item;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_zap read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_zap_rod(player, item, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)item, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_target_interactive(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch, mode;
    u32b query;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%b%lu", &ch, &mode, &query)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_target_interactive read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        target_set_interactive(player, mode, query);
    }

    return 1;
}


static int Receive_inscribe(int ind)
{
    s16b item;
    char inscription[NORMAL_WID];
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd%s", &ch, &item, inscription)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_inscribe read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_inscribe(player, item, inscription);
    }

    return 1;
}


static int Receive_uninscribe(int ind)
{
    s16b item;
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_uninscribe read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_uninscribe(player, item);
    }

    return 1;
}


static int Receive_activate(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    s16b item;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_activate read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_activate(player, item, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)item, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_bash(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_bash read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_bash(player, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}    


static int Receive_alter(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_alter read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_alter(player, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_spike(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_spike read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_spike(player, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_disarm(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir, easy;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%c", &ch, &dir, &easy)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_disarm read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_disarm(player, dir, (bool)easy);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c%c", (unsigned)ch, (int)dir, (int)easy);
        return 0;
    }

    return 1;
}


static int Receive_eat(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_eat read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_eat_food(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_fill(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_fill read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_refill(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_locate(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_locate read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_locate(player_get(player), dir);
    }

    return 1;
}


static int Receive_map(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch, mode;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &mode)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_map read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (mode == 0) do_cmd_view_map(player);
        else do_cmd_wild_map(player);
    }

    return 1;
}


static int Receive_fullmap(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_fullmap read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        display_fullmap(player);
    }

    return 1;
}


static int Receive_search_mode(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_search_mode read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_toggle_search(player);
    }

    return 1;
}


static int Receive_close(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir, easy;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%c", &ch, &dir, &easy)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_close read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_close(player, dir, (bool)easy);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c%c", (unsigned)ch, (int)dir, (int)easy);
        return 0;
    }

    return 1;
}


static int Receive_gain(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;
    s16b book, spell;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &book, &spell)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_gain read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_study(player, book, spell);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%hd", (unsigned)ch, (int)book, (int)spell);
        return 0;
    }

    return 1;
}


static int Receive_go_up(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_go_up read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_go_up(player);
            return 2;
        }

        Packet_printf(&connp->q, "%b", (unsigned)ch);
        return 0;
    }

    return 1;
}


static int Receive_go_down(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_go_down read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_go_down(player);
            return 2;
        }

        Packet_printf(&connp->q, "%b", (unsigned)ch);
        return 0;
    }

    return 1;
}


static void Handle_item(int Ind, int item)
{
    player_type *p_ptr = player_get(Ind);
    int i;

    /* Set current value */
    p_ptr->current_value = item;

    /* Current spell */
    if (p_ptr->current_spell != -1)
    {
        /* Cast current normal spell */
        if (p_ptr->current_item >= 0)
        {
            int old_num = get_player_num(p_ptr);

            if (!cast_spell(p_ptr, p_ptr->current_spell, (quark_t)p_ptr->current_item, 0))
                return;

            cast_spell_end(Ind);

            /* Take a turn */
            use_energy(Ind);

            /* Use some mana */
            p_ptr->csp -= p_ptr->spell_cost;

            /* Hack -- Redraw picture */
            redraw_picture(p_ptr, old_num);

            /* Redraw mana */
            p_ptr->redraw |= (PR_MANA);
        }

        /* Cast current projected spell */
        else if (!cast_spell_proj(p_ptr, 0 - p_ptr->current_item, p_ptr->current_spell))
            return;
    }

    /* Current item */
    else if (p_ptr->current_item != ITEM_REQUEST)
    {
        int cur_item = p_ptr->current_item;
        bool ident = FALSE, used;
        object_type *o_ptr;

        /* Get the item */
        o_ptr = object_from_item_idx(p_ptr, cur_item, 0, FALSE);

        /* Paranoia: requires an item */
        if (!o_ptr || !o_ptr->kind) return;

        /* The player is aware of the object's flavour */
        p_ptr->was_aware = object_flavor_is_aware(p_ptr, o_ptr);

        /* Use current item */
        used = use_object(Ind, o_ptr, &ident, 0);

        /* Quit if the item wasn't used and no knowledge was gained */
        if (!used && (p_ptr->was_aware || !ident)) return;

        /* Analyze the object */
        switch (o_ptr->tval)
        {
            case TV_SCROLL: do_cmd_read_scroll_end(Ind, ident, used); break;
            case TV_STAFF: do_cmd_use_staff_discharge(Ind, ident, used); break;
            case TV_ROD: do_cmd_zap_rod_end(Ind, ident, used); break;
        }
    }

    /* Pickup */
    else py_pickup(Ind, 3);

    /* Optimize inventory */
    for (i = INVEN_PACK - 1; i >= 0; i--) inven_item_optimize(p_ptr, i);
}


static int Receive_item(int ind)
{
    s16b item;
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_item read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        Handle_item(player, item);
    }

    return 1;
}


static int Receive_message(int ind)
{
    connection_t *connp = get_connection(ind);
    char buf[MSG_LEN];
    int n, player;
    byte ch;

    buf[0] = '\0';

    if ((n = Packet_scanf(&connp->r, "%b%S", &ch, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_message read error");
        return n;
    }

    player = get_player_index(connp);

    do_cmd_message(player, buf);

    return 1;
}


static int Receive_purchase(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    int n, player;
    s16b item, amt;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &amt)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_purchase read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        if (in_store(p_ptr))
        {
            store_purchase(player, item, amt);
            Packet_printf(&connp->c, "%b", (unsigned)PKT_PURCHASE);
        }
        else
            do_cmd_purchase_house(player, item);
    }

    return 1;
}


static int Receive_store_examine(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    int n, player;
    s16b item;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_store_examine read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        if (in_store(p_ptr)) store_examine(player, item);
    }

    return 1;
}


static int Receive_store_order(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    int n, player;
    char buf[NORMAL_WID];

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_store_order read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        if (in_store(p_ptr)) store_order(player, buf);
    }

    return 1;
}


static int Receive_sell(int ind)
{
    s16b item, amt;
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &amt)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_sell read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        store_sell(player, item, amt);
    }

    return 1;
}


static int Receive_store_leave(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    int n, player, store_num;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_store_leave read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        /* Update the visuals */
        p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

        /* Redraw */
        p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

        /* Update store info */
        message_flush(p_ptr);
        store_num = p_ptr->store_num;
        if (store_num != -1)
        {
            p_ptr->store_num = -1;

            /* Hack -- Don't stand in the way */
            if (store_num != STORE_PLAYER)
            {
                bool look = TRUE;
                int d, i, dis = 1, x = p_ptr->py, y = p_ptr->px;

                while (look)
                {
                    if (dis > 200) dis = 200;
                    for (i = 0; i < 500; i++)
                    {
                        while (1)
                        {
                            y = rand_spread(p_ptr->py, dis);
                            x = rand_spread(p_ptr->px, dis);
                            d = distance(p_ptr->py, p_ptr->px, y, x);
                            if (d <= dis) break;
                        }
                        if (!in_bounds_fully(y, x)) continue;
                        if (!cave_naked_bold(p_ptr->depth, y, x)) continue;
                        if (is_icky(p_ptr->depth, y, x)) continue;
                        look = FALSE;
                        break;
                    }
                    dis = dis * 2;
                }
                monster_swap(p_ptr->depth, p_ptr->py, p_ptr->px, y, x);
                handle_stuff(p_ptr);
            }

            cave_illuminate(p_ptr, cave_get(p_ptr->depth), (is_daytime()? TRUE: FALSE));
        }

        /* Redraw (remove selling prices) */
        p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
    }

    return 1;
}


static int Receive_store_confirm(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_store_confirm read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        if (in_store(p_ptr))
        {
            store_confirm(player);
            Packet_printf(&connp->c, "%b", (unsigned)PKT_STORE_CONFIRM);
        }
        else if (p_ptr->current_house != -1)
            do_cmd_purchase_house(player, 0);
        else
            py_pickup(player, 4);
    }

    return 1;
}


static int Receive_drop_gold(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;
    s32b amt;

    if ((n = Packet_scanf(&connp->r, "%b%ld", &ch, &amt)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_drop_gold read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_drop_gold(player, amt);
            return 2;
        }

        Packet_printf(&connp->q, "%b%ld", (unsigned)ch, amt);
        return 0;
    }

    return 1;
}


static int Receive_steal(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_steal read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (!cfg_no_steal)
        {
            if (has_energy(player))
            {
                do_cmd_steal(player, dir);
                return 2;
            }

            Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
            return 0;
        }
        else
            /* Handle the option to disable player/player stealing */
            msg(player_get(player), "Your pathetic attempts at stealing fail.");
    }

    return 1;
}


static int Receive_redraw(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_redraw read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_redraw(player);
    }

    return 1;
}


static int Receive_rest(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    byte ch;
    s16b resting;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &resting)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_rest read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (do_cmd_rest(player, resting)) return 2;

        /*
         * If we don't have enough energy to rest, disturb us (to stop
         * us from running) and queue the command.
         */
         disturb(player_get(player), 0, 0);
         Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)resting);
         return 0;
    }

    return 1;
}


static int Receive_special_line(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    char type;
    s16b line;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &type, &line)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_special_line read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        switch (type)
        {
            case SPECIAL_FILE_NONE:
                player_get(player)->special_file_type = SPECIAL_FILE_NONE;
                Send_term_info(player_get(player), NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
                free_info_icky(player);
                free_header_icky(player);
                break;
            case SPECIAL_FILE_PLAYER:
                do_cmd_check_players(player, line);
                break;
            case SPECIAL_FILE_OTHER:
                do_cmd_check_other(player, line);
                break;
            case SPECIAL_FILE_POLY:
                do_cmd_check_poly(player, line);
                break; 
            case SPECIAL_FILE_SOCIALS:
                do_cmd_check_socials(player, line);
                break;
            default:
                do_cmd_knowledge(player, type, line);
                break;
        }
    }

    return 1;
}


static int Receive_symbol_query(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    char buf[NORMAL_WID];
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_symbol_query read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Perform query */
        do_cmd_query_symbol(player, buf);
    }

    return 1;
}


static int Receive_monlist(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_monlist read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Perform query */
        do_cmd_monlist(player);
    }

    return 1;
}


static int Receive_objlist(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_objlist read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Perform query */
        do_cmd_itemlist(player);
    }

    return 1;
}


static int sync_settings(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    connection_t *connp = get_connection(p_ptr->conn);
    int i;

    /* Resize */
    if ((connp->Client_setup.settings[SETTING_SCREEN_COLS] != p_ptr->screen_cols) ||
        (connp->Client_setup.settings[SETTING_SCREEN_ROWS] != p_ptr->screen_rows))
    {
        p_ptr->screen_cols = connp->Client_setup.settings[SETTING_SCREEN_COLS];
        p_ptr->screen_rows = connp->Client_setup.settings[SETTING_SCREEN_ROWS];

        if (!screen_compatible(p_ptr->conn))
        {
            errno = 0;
            return -1;
        }

        verify_panel(p_ptr);

        /* Redraw map */
        p_ptr->redraw |= (PR_MAP);
    }

    /* Hack -- Process "settings" */
    p_ptr->use_graphics = connp->Client_setup.settings[SETTING_USE_GRAPHICS];
    p_ptr->tile_wid = connp->Client_setup.settings[SETTING_TILE_WID];
    p_ptr->tile_hgt = connp->Client_setup.settings[SETTING_TILE_HGT];
    p_ptr->tile_distorted = connp->Client_setup.settings[SETTING_TILE_DISTORTED];
    p_ptr->max_hgt = connp->Client_setup.settings[SETTING_MAX_HGT];
    p_ptr->window_flag = connp->Client_setup.settings[SETTING_WINDOW_FLAG];
    p_ptr->other.hitpoint_warn = connp->Client_setup.settings[SETTING_HITPOINT_WARN];
    p_ptr->other.delay_factor = connp->Client_setup.settings[SETTING_DELAY_FACTOR];
    for (i = 0; i < TYPE_MAX; i++)
        p_ptr->other.squelch_lvl[i] = connp->Client_setup.settings[i + SETTING_SQUELCH_JEWELRY];

    return 1;
}


static int Receive_options(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    int player, i, n;
    byte ch;
    bool old_ironman, old_no_stores, old_no_artifacts, old_no_feelings, old_no_selling,
        old_no_ghost, old_fruit_bat;
    byte settings;
    byte old_squelch_level[TYPE_MAX];

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &settings)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_options read error");
        return n;
    }

    if (settings)
    {
        for (i = 0; i < SETTING_MAX; i++)
        {
            n = Packet_scanf(&connp->r, "%hd", &connp->Client_setup.settings[i]);

            if (n <= 0)
            {
                Destroy_connection(ind, "Receive_options read error");
                return n;
            }
        }
    }

    for (i = 0; i < OPT_MAX; i++)
    {
        char opt;

        n = Packet_scanf(&connp->r, "%c", &opt);

        if (n <= 0)
        {
            Destroy_connection(ind, "Receive_options read error");
            return n;
        }

        connp->Client_setup.options[i] = (bool)opt;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        /* Save some old values */
        old_ironman = OPT_P(p_ptr, birth_ironman);
        old_no_stores = OPT_P(p_ptr, birth_no_stores);
        old_no_artifacts = OPT_P(p_ptr, birth_no_artifacts);
        old_no_feelings = OPT_P(p_ptr, birth_no_feelings);
        old_no_selling = OPT_P(p_ptr, birth_no_selling);
        old_no_ghost = OPT_P(p_ptr, birth_no_ghost);
        old_fruit_bat = OPT_P(p_ptr, birth_fruit_bat);
        for (i = 0; i < TYPE_MAX; i++)
            old_squelch_level[i] = p_ptr->other.squelch_lvl[i];

        /* Set new values */
        for (i = 0; i < OPT_MAX; i++)
            p_ptr->other.opt[i] = connp->Client_setup.options[i];

        /* Birth options: must have 0 xp to be applied */
        if (p_ptr->max_exp > 0)
        {
            OPT_P(p_ptr, birth_ironman) = old_ironman;
            OPT_P(p_ptr, birth_no_stores) = old_no_stores;
            OPT_P(p_ptr, birth_no_artifacts) = old_no_artifacts;
            OPT_P(p_ptr, birth_no_feelings) = old_no_feelings;
            OPT_P(p_ptr, birth_no_selling) = old_no_selling;
            OPT_P(p_ptr, birth_no_ghost) = old_no_ghost;
            OPT_P(p_ptr, birth_fruit_bat) = old_fruit_bat;
        }

        /* Fruit bat mode: not when a Dragon, a Shapechanger or a Necromancer */
        if (pf_has(p_ptr->race->pflags, PF_DRAGON) ||
            pf_has(p_ptr->clazz->pflags, PF_MONSTER_SPELLS) ||
            pf_has(p_ptr->clazz->pflags, PF_UNDEAD_POWERS))
        {
            OPT_P(p_ptr, birth_fruit_bat) = FALSE;
        }
        if (OPT_P(p_ptr, birth_fruit_bat) != old_fruit_bat)
        {
            do_cmd_poly(p_ptr, (OPT_P(p_ptr, birth_fruit_bat)? get_r_idx("Fruit bat"): 0), FALSE,
                TRUE);
        }

        /* Set squelched status */
        for (i = 0; i < TYPE_MAX; i++)
        {
            if (connp->Client_setup.settings[i + SETTING_SQUELCH_JEWELRY] > old_squelch_level[i])
                p_ptr->notice |= PN_SQUELCH;
        }

        return sync_settings(player);
    }

    return 1;
}


static int Receive_suicide(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_suicide read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Commit suicide */
        do_cmd_suicide(player);
    }

    return 1;
}


static int Receive_party(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    char buf[NORMAL_WID];
    s16b command;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%s", &ch, &command, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_party read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        do_cmd_party(player, command, buf);
    }

    return 1;
}


static int Receive_pass(int ind)
{
    connection_t *connp = get_connection(ind);
    char buf[MAX_PASS_LEN];
    int n, player;
    player_type *p_ptr;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_pass read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        my_strcpy(p_ptr->pass, buf, MAX_PASS_LEN + 1);
    }

    return 1;
}


/* Receive a dungeon master command */
static int Receive_master(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    char buf[NORMAL_WID];
    s16b command;
    byte ch;

    /* Make sure this came from the dungeon master.  Note that it may be
     * possible to spoof this, so probably in the future more advanced
     * authentication schemes will be necessary.
     */
    if ((n = Packet_scanf(&connp->r, "%b%hd%s", &ch, &command, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_master read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        do_cmd_master(player, command, buf);
    }

    return 2;
}


static int Receive_clear(int ind)
{
    byte ch;
    int n;
    connection_t *connp = get_connection(ind);

    /* Remove the clear command from the queue */
    if ((n = Packet_scanf(&connp->r, "%b", &ch)) != 1)
    {
        errno = 0;
        plog("Cannot receive clear packet");
        Destroy_connection(ind, "Cannot receive clear packet");
        return -1;
    }

    /* Clear any queued commands prior to this clear request */
    Sockbuf_clear(&connp->q);

    return 2;
}


static int Receive_poly(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    int n, player;
    s16b number;
    player_type *p_ptr;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &number)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_poly read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        p_ptr = player_get(player);

        /* Restrict to shapechangers in non-fruitbat mode */
        if (player_has(p_ptr, PF_MONSTER_SPELLS) && !OPT_P(p_ptr, birth_fruit_bat))
            do_cmd_poly(p_ptr, number, TRUE, TRUE);
    }

    return 1;
}


static int Receive_social(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int player, n;
    char buf[NORMAL_WID];
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%s", &ch, &dir, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_social read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_social(player, buf, dir);
    }

    return 1;
}    


static int Receive_feeling(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_feeling read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        display_feeling(player_get(player), FALSE);
    }

    return 1;
}


static int Receive_breath(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_breath read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_breath(player, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_mimic(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    int n, player;
    s16b page, spell;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd%c", &ch, &page, &spell, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_mimic read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_mimic(player, page, spell, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%hd%c", (unsigned)ch, (int)page, (int)spell, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_channel(int ind)
{
    connection_t *connp = get_connection(ind);
    char buf[NORMAL_WID];
    int n, player;
    byte ch;

    buf[0] = '\0';

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_channel read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_chat(player, buf);
    }

    return 1;
}


static int Receive_interactive(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    char type;
    u32b key;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%lu", &ch, &type, &key)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_interactive read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        do_cmd_interactive(player, type, key);
    }

    return 1;
}


static int Receive_fountain(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b item;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_fountain read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_fountain(player, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_icky(int ind)
{
    connection_t *connp = get_connection(ind);
    byte ch;
    s16b icky;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &icky)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_icky read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        player_get(player)->screen_icky = icky;
    }

    return 1;
}


static int Receive_center(int ind)
{
    connection_t *connp = get_connection(ind);
    int player, n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_center read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        do_cmd_center_map(player);
    }

    return 1;
}


static int Receive_ignore(int ind)
{
    connection_t *connp = get_connection(ind);
    player_type *p_ptr;
    byte ch;
    int n, player;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_ignore read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);
        p_ptr = player_get(player);

        /* Break mind link */
        break_mind_link(player);

        p_ptr->unignoring = !p_ptr->unignoring;
        p_ptr->notice |= PN_SQUELCH;
        do_cmd_redraw(player);
    }

    return 1;
}


static int Receive_use_any(int ind)
{
    connection_t *connp = get_connection(ind);
    char dir;
    s16b item;
    int n, player;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1)
            Destroy_connection(ind, "Receive_use_any read error");
        return n;
    }

    if (connp->id != -1)
    {
        player = get_player_index(connp);

        /* Break mind link */
        break_mind_link(player);

        if (has_energy(player))
        {
            do_cmd_use_any(player, item, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)item, (int)dir);
        return 0;
    }

    return 1;
}
