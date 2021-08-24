/*
 * File: netserver.c
 * Purpose: The server side of the network stuff
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


/*
 * This is the server side of the network connection stuff.
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


#define MAX_RELIABLE_DATA_PACKET_SIZE   512
#define MAX_TEXTFILE_CHUNK              512


static server_setup_t Setup;
static int login_in_progress;
static int num_logins, num_logouts;


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


/*
 * Initialize the connection structures.
 */
int Setup_net_server(void)
{
    if (Init_setup() == -1) return -1;

    init_connections();

    init_players();

    /* Tell the metaserver that we're starting up */
    plog("Report to metaserver");
    Report_to_meta(META_START);

    plog_fmt("Server is running version %s", version_build(VB_BASE | VB_BUILD));

    return 0;
}


void Conn_set_state(connection_t *connp, int state, long timeout)
{
    static int num_conn_busy;
    static int num_conn_playing;

    if ((connp->state == CONN_PLAYING) || (connp->state == CONN_QUIT))
        num_conn_playing--;
    else if (connp->state == CONN_FREE)
        num_conn_busy++;

    connp->state = state;
    ht_copy(&connp->start, &turn);

    if ((connp->state == CONN_PLAYING) || (connp->state == CONN_QUIT))
        num_conn_playing++;
    else if (connp->state == CONN_FREE)
        num_conn_busy--;

    if (timeout) connp->timeout = timeout;
    login_in_progress = num_conn_busy - num_conn_playing;
}


/*
 * Actually quit. This was separated as a hack to allow us to
 * "quit" when a quit packet has not been received, such as when
 * our TCP connection is severed.
 */
static void do_quit(int ind)
{
    connection_t *connp = get_connection(ind);
    struct worldpos wpos;
    bool dungeon_master = false;

    memset(&wpos, 0, sizeof(wpos));

    if (connp->id != -1)
    {
        struct player *p = player_get(get_player_index(connp));

        memcpy(&wpos, &p->wpos, sizeof(wpos));
        dungeon_master = is_dm_p(p);
    }

    /* Close the socket */
    SocketClose(connp->w.sock);

    /* No more packets from a player who is quitting */
    remove_input(connp->w.sock);

    /* Disable all output and input to and from this player */
    connp->w.sock = -1;

    /* Check for immediate disconnection */
    if (town_area(&wpos) || dungeon_master)
    {
        /* If we are close to a town, exit quickly. */
        /* DM always disconnects immediately */
        Destroy_connection(ind, "Client quit");
    }
    else
    {
        /* Otherwise wait for the timeout */
        connp->quit_msg = string_make("Client quit");
        Conn_set_state(connp, CONN_QUIT, QUIT_TIMEOUT);
    }
}


static int Send_reliable(int ind)
{
    connection_t *connp = get_connection(ind);
    int num_written;

    /*
     * Hack -- make sure we have a valid socket to write to.
     * -1 is used to specify a player that has disconnected but is still "in game".
     */
    if (connp->w.sock == -1) return 0;

    if ((num_written = Sockbuf_write(&connp->w, connp->c.buf, connp->c.len)) != connp->c.len)
    {
        plog_fmt("Cannot write reliable data (%d, %d)", num_written, connp->c.len);
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
    int ind = arg, old_numplayers = NumPlayers;
    connection_t *connp = get_connection(ind);
    struct player *p;

    /* Ignore input from client if not in SETUP or PLAYING state */
    if ((connp->state != CONN_PLAYING) && (connp->state != CONN_SETUP)) return;

    /* Handle "leaving" */
    if ((connp->id != -1) && player_get(get_player_index(connp))->upkeep->new_level_method) return;

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
            p = player_get(get_player_index(connp));

            /* Refresh stuff */
            refresh_stuff(p);
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
    unsigned int max = 0;

    if (!flavors) return 0;

    for (f = flavors; f; f = f->next)
    {
        if (f->fidx > max) max = f->fidx;
    }

    return (u16b)max + 1;
}


/*
 * After a TCP "Contact" was made we shall see if we have
 * room for more connections and create one.
 */
static int Setup_connection(u32b account, char *real, char *nick, char *addr, char *host,
    char *pass, u16b conntype, unsigned version, int fd)
{
    int i, free_conn_index = MAX_PLAYERS, sock;
    connection_t *connp;
    bool memory_error = false;

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

    if (GetPortNum(sock) == 0)
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
        memory_error = true;
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

        if (!connp->has_setup)
        {
            u16b flavor_max = get_flavor_max();

            connp->Client_setup.k_attr = mem_zalloc(z_info->k_max * sizeof(byte));
            connp->Client_setup.k_char = mem_zalloc(z_info->k_max * sizeof(char));
            connp->Client_setup.r_attr = mem_zalloc(z_info->r_max * sizeof(byte));
            connp->Client_setup.r_char = mem_zalloc(z_info->r_max * sizeof(char));
            connp->Client_setup.f_attr = mem_zalloc(z_info->f_max * sizeof(byte_lit));
            connp->Client_setup.f_char = mem_zalloc(z_info->f_max * sizeof(char_lit));
            connp->Client_setup.t_attr = mem_zalloc(z_info->trap_max * sizeof(byte_lit));
            connp->Client_setup.t_char = mem_zalloc(z_info->trap_max * sizeof(char_lit));
            connp->Client_setup.flvr_x_attr = mem_zalloc(flavor_max * sizeof(byte));
            connp->Client_setup.flvr_x_char = mem_zalloc(flavor_max * sizeof(char));
            connp->Client_setup.note_aware = mem_zalloc(z_info->k_max * sizeof(char_note));
            connp->has_setup = true;
        }

        if ((connp->real == NULL) || (connp->nick == NULL) || (connp->pass == NULL) ||
            (connp->host == NULL))
        {
            memory_error = true;
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
        connp->console_authenticated = false;
        connp->console_listen = false;

        Conn_set_state(connp, CONN_CONSOLE, 0);
    }

    /* Non-players leave now */
    if (conntype != CONNTYPE_PLAYER)
        return free_conn_index;

    Conn_set_state(connp, CONN_SETUP, SETUP_TIMEOUT);

    /* Remove the contact input handler */
    remove_input(sock);
    
    /* Install the game input handler */
    install_input(Handle_input, sock, free_conn_index);

    return free_conn_index;
}


/*
 * Check if we like the names.
 */
static int Check_names(char *nick_name, char *real_name, char *host_name)
{
    char *ptr;

    /** Realname / Hostname **/

    if ((real_name[0] == 0) || (host_name[0] == 0)) return E_INVAL;

    /* Replace weird characters with '?' */
    for (ptr = &real_name[strlen(real_name)]; ptr-- > real_name; )
    {
        if (!isascii(*ptr) || !isprint(*ptr)) *ptr = '?';
    }
    for (ptr = &host_name[strlen(host_name)]; ptr-- > host_name; )
    {
        if (!isascii(*ptr) || !isprint(*ptr)) *ptr = '?';
    }

    /** Playername **/

    if ((nick_name[0] < 'A') || (nick_name[0] > 'Z')) return E_INVAL;

    /* Any weird characters here, bail out. We allow letters, numbers and space */
    for (ptr = &nick_name[strlen(nick_name)]; ptr-- > nick_name; )
    {
        if (!isascii(*ptr)) return E_INVAL;
        if (!(isalpha(*ptr) || isdigit(*ptr) || (*ptr == ' '))) return E_INVAL;
    }

    /* Right-trim nick */
    for (ptr = &nick_name[strlen(nick_name)]; ptr-- > nick_name; )
    {
        if (isascii(*ptr) && isspace(*ptr)) *ptr = '\0';
        else break;
    }

    /* The "server" and "account" names are reserved */
    if (!my_stricmp(nick_name, "server") || !my_stricmp(nick_name, "account")) return E_INVAL;

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
        return false;
    }

    return true;
}


static void Contact(int fd, int arg)
{
    int newsock, bytes, len, ret;
    struct sockaddr_in sin;
    char host_addr[24];
    u16b conntype = 0;
    u16b version = 0;
    char status = SUCCESS, beta;
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
             * handle this situation correctly yet. For the moment, we just log the
             * error and quit.
             *
             * PWMAngband: when running the server in debug mode, we get a lot of
             * TCP connection failures with errno = 0, which doesn't make any sense.
             * In this case, we just return without quitting.
             */
            plog_fmt("Could not accept TCP Connection, socket error = %d", errno);
            if (errno) quit("Couldn't accept TCP connection.");
            return;
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
        if (cfg_console_local_only && (sin.sin_addr.s_addr != htonl(INADDR_LOOPBACK)))
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
    if (Packet_scanf(&ibuf, "%hu%c", &version, &beta) <= 0)
    {
        Contact_cancel(fd, format("Incompatible version packet from %s", host_addr));
        return;
    }

    /* Check client version */
    if ((beta && !beta_version()) || (version < min_version())) status = E_VERSION_OLD;
    if ((beta_version() && !beta) || (version > current_version())) status = E_VERSION_NEW;

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
    Packet_printf(&ibuf, "%hu", (unsigned)cfg_max_account_chars);

    /* Some error */
    if (status)
    {
        Net_Send(fd);
        return;
    }

    for (i = 0; i < (size_t)num; i++)
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
bool Report_to_meta(int flag)
{
    /* Abort if the user doesn't want to report */
    if (!cfg_report_to_meta) return false;

    /* New implementation */
    meta_report(flag);
    return true;
}


/*
 * Delete a player's information and save his game
 */
static void Delete_player(int id)
{
    struct player *p = player_get(id);
    char buf[255];
    int i;
    struct monster *mon;
    struct source who_body;
    struct source *who = &who_body;
    struct chunk *c = chunk_get(&p->wpos);
    struct chunk *c_last = NULL;

    source_player(who, id, p);

    /* Be paranoid */
    if (c)
    {
        /* Remove the player */
        c->squares[p->py][p->px].mon = 0;

        /* Redraw */
        square_light_spot(c, p->py, p->px);

        /* Free monsters from slavery */
        for (i = 1; i < cave_monster_max(c); i++)
        {
            /* Paranoia -- skip dead monsters */
            mon = cave_monster(c, i);
            if (!mon->race) continue;

            /* Skip non slaves */
            if (p->id != mon->master) continue;

            /* Free monster from slavery */
            monster_set_master(mon, NULL, MSTATUS_HOSTILE);
        }
        p->slaves = 0;
    }

    /* Leave chat channels */
    channels_leave(p);

    /* Hack -- unstatic if the DM left while manually designing a dungeon level */
    if (chunk_inhibit_players(&p->wpos)) chunk_set_player_count(&p->wpos, 0);

    /* Try to save his character */
    save_player(p);

    /* Un-hostile the player */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        if (q == p) continue;
        pvp_check(q, p, PVP_REMOVE, true, 0x00);
    }

    /* If he was actively playing, tell everyone that he's left */
    /* Handle the cfg_secret_dungeon_master option */
    if (p->alive && !p->is_dead && !(p->dm_flags & DM_SECRET_PRESENCE))
    {
        strnfmt(buf, sizeof(buf), "%s has left the game.", p->name);
        msg_broadcast(p, buf, MSG_BROADCAST_ENTER_LEAVE);
    }

    /* Hack -- don't track this player anymore */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);
        struct actor_race *monster_race = &q->upkeep->monster_race;
        struct source *cursor_who = &q->cursor_who;
        struct source *health_who = &q->upkeep->health_who;

        if (q == p) continue;

        /* Cancel the race */
        if (ACTOR_PLAYER_EQUAL(monster_race, who)) monster_race_track(q->upkeep, NULL);

        /* Cancel the target */
        if (target_equals(q, who)) target_set_monster(q, NULL);

        /* Cancel tracking */
        if (source_equal(cursor_who, who)) cursor_track(q, NULL);

        /* Cancel the health bar */
        if (source_equal(health_who, who)) health_track(q->upkeep, NULL);
    }

    /* Swap entry number 'id' with the last one */
    /* Also, update the "player_index" on the cave grids */
    if (id != NumPlayers)
    {
        struct player *q = player_get(NumPlayers);

        c_last = chunk_get(&q->wpos);
        if (c_last) c_last->squares[q->py][q->px].mon = 0 - id;
        player_set(NumPlayers, player_get(id));
        player_set(id, q);
        set_player_index(get_connection(player_get(id)->conn), id);
    }

    set_player_index(get_connection(player_get(NumPlayers)->conn), NumPlayers);

    /* Free memory */
    cleanup_player(player_get(NumPlayers));
    mem_free(player_get(NumPlayers));

    /* Clear the player slot previously used */
    player_set(NumPlayers, NULL);

    /* Update the number of players */
    NumPlayers--;

    /* Tell the metaserver about the loss of a player */
    Report_to_meta(META_UPDATE);

    /* Fix the monsters and remaining players */
    if (c) update_monsters(c, true);
    if (c_last) update_monsters(c_last, true);
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
    byte (*f_attr)[LIGHTING_MAX];
    char (*f_char)[LIGHTING_MAX];
    byte (*t_attr)[LIGHTING_MAX];
    char (*t_char)[LIGHTING_MAX];
    byte *flvr_x_attr;
    char *flvr_x_char;
    char (*note_aware)[4];
    bool has_setup = connp->has_setup;

    /* Save */
    if (has_setup)
    {
        k_attr = connp->Client_setup.k_attr;
        r_attr = connp->Client_setup.r_attr;
        f_attr = connp->Client_setup.f_attr;
        t_attr = connp->Client_setup.t_attr;
        flvr_x_attr = connp->Client_setup.flvr_x_attr;
        k_char = connp->Client_setup.k_char;
        r_char = connp->Client_setup.r_char;
        f_char = connp->Client_setup.f_char;
        t_char = connp->Client_setup.t_char;
        flvr_x_char = connp->Client_setup.flvr_x_char;
        note_aware = connp->Client_setup.note_aware;
    }

    /* Wipe */
    memset(connp, 0, sizeof(*connp));

    /* Restore */
    if (has_setup)
    {
        connp->has_setup = has_setup;
        connp->Client_setup.k_attr = k_attr;
        connp->Client_setup.r_attr = r_attr;
        connp->Client_setup.f_attr = f_attr;
        connp->Client_setup.t_attr = t_attr;
        connp->Client_setup.flvr_x_attr = flvr_x_attr;
        connp->Client_setup.k_char = k_char;
        connp->Client_setup.r_char = r_char;
        connp->Client_setup.f_char = f_char;
        connp->Client_setup.t_char = t_char;
        connp->Client_setup.flvr_x_char = flvr_x_char;
        connp->Client_setup.note_aware = note_aware;
    }
}


/*
 * Cleanup a connection.  The client may not know yet that it is thrown out of
 * the game so we send it a quit packet if our connection to it has not already
 * closed.  If our connection to it has been closed, then connp->w.sock will
 * be set to -1.
 */
void Destroy_connection(int ind, char *reason)
{
    connection_t *connp = get_connection(ind);

    if (connp->state == CONN_FREE)
    {
        errno = 0;
        plog_fmt("Cannot destroy empty connection (\"%s\")", reason);
        return;
    }

    if (connp->conntype == CONNTYPE_PLAYER)
    {
        if (connp->w.sock != -1)
        {
            char pkt[NORMAL_WID];
            int len;

            pkt[0] = PKT_QUIT;
            my_strcpy(&pkt[1], reason, sizeof(pkt) - 2);
            len = strlen(pkt) + 2;
            pkt[len - 1] = PKT_END;
            pkt[len] = '\0';

            if (DgramWrite(connp->w.sock, pkt, len) != len)
            {
                GetSocketError(connp->w.sock);
                DgramWrite(connp->w.sock, pkt, len);
            }
        }
        plog_fmt("Goodbye %s=%s@%s (\"%s\")", (connp->nick? connp->nick: ""),
            (connp->real? connp->real: ""), (connp->host? connp->host: ""), reason);
    }

    Conn_set_state(connp, CONN_FREE, FREE_TIMEOUT);

    if (connp->id != -1) Delete_player(get_player_index(connp));
    string_free(connp->real);
    string_free(connp->nick);
    string_free(connp->addr);
    string_free(connp->host);
    string_free(connp->pass);
    string_free(connp->quit_msg);
    Sockbuf_cleanup(&connp->w);
    Sockbuf_cleanup(&connp->r);
    Sockbuf_cleanup(&connp->c);
    Sockbuf_cleanup(&connp->q);

    if (connp->w.sock != -1)
    {
        DgramClose(connp->w.sock);
        remove_input(connp->w.sock);
    }

    wipe_connection(connp);

    num_logouts++;
}


void Stop_net_server(void)
{
    int i;

    /* Hack -- free client setup tables */
    for (i = 0; i < MAX_PLAYERS; i++)
    {
        connection_t* connp = get_connection(i);

        if (connp && connp->has_setup)
        {
            mem_free(connp->Client_setup.k_attr);
            mem_free(connp->Client_setup.k_char);
            mem_free(connp->Client_setup.r_attr);
            mem_free(connp->Client_setup.r_char);
            mem_free(connp->Client_setup.f_attr);
            mem_free(connp->Client_setup.f_char);
            mem_free(connp->Client_setup.t_attr);
            mem_free(connp->Client_setup.t_char);
            mem_free(connp->Client_setup.flvr_x_attr);
            mem_free(connp->Client_setup.flvr_x_char);
            mem_free(connp->Client_setup.note_aware);
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


void* console_buffer(int ind, bool read)
{
    connection_t *connp = get_connection(ind);

    if (read) return (void*)&connp->r;
    return (void*)&connp->w;
}


bool Conn_is_alive(int ind)
{
    connection_t *connp = get_connection(ind);

    if (!connp) return false;
    if (connp->state != CONN_CONSOLE) return false;
    return true;
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
 * Explain a broken "lib" folder and quit (see below).
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

    /* Verify the "news" file */
    path_build(buf, sizeof(buf), ANGBAND_DIR_SCREENS, "news.txt");
    if (!file_exists(buf))
    {
        char why[MSG_LEN];

        /* Crash and burn */
        strnfmt(why, sizeof(why), "Cannot access the '%s' file!", buf);
        init_angband_aux(why);
    }

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

                strnfmt(version_marker, sizeof(buf) - pos, "%s", version_build(VB_BASE));
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
    path_build(buf, sizeof(buf), ANGBAND_DIR_SCREENS, "dead.txt");
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

    path_build(buf, sizeof(buf), ANGBAND_DIR_SCREENS, "crown.txt");
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
    Setup.max_col = z_info->dungeon_wid;
    Setup.max_row = z_info->dungeon_hgt;

    /* Verify and load the splash screen */
    show_splashscreen();

    /* Verify and load the tombstone */
    print_tomb();

    /* Verify and load the winner crown */
    display_winner();

    return 0;
}


byte *Conn_get_console_channels(int ind)
{
    connection_t *connp = get_connection(ind);
    return connp->console_channels;
}


/*** Sending ***/


int Send_basic_info(int ind)
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


int Send_limits_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u16b dummy = 0;
    u16b flavor_max = get_flavor_max();

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for limits info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_LIMITS,
        (unsigned)dummy) <= 0)
    {
        Destroy_connection(ind, "Send_limits_struct_info write error");
        return -1;
    }

    if (Packet_printf(&connp->c, "%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu", (unsigned)z_info->a_max,
        (unsigned)z_info->e_max, (unsigned)z_info->k_max, (unsigned)z_info->r_max,
        (unsigned)z_info->f_max, (unsigned)z_info->trap_max, (unsigned)flavor_max,
        (unsigned)z_info->pack_size, (unsigned)z_info->quiver_size, (unsigned)z_info->floor_size,
        (unsigned)z_info->quiver_slot_size, (unsigned)z_info->store_inven_max,
        (unsigned)z_info->curse_max) <= 0)
    {
        Destroy_connection(ind, "Send_limits_struct_info write error");
        return -1;
    }

    return 1;
}


int Send_race_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b j;
    struct player_race *r;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for race info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_RACE,
        (unsigned)player_rmax()) <= 0)
    {
        Destroy_connection(ind, "Send_race_struct_info write error");
        return -1;
    }

    for (r = races; r; r = r->next)
    {
        if (Packet_printf(&connp->c, "%b%s", r->ridx, r->name) <= 0)
        {
            Destroy_connection(ind, "Send_race_struct_info write error");
            return -1;
        }

        /* Transfer other fields here */
        for (j = 0; j < STAT_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)r->r_adj[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_struct_info write error");
                return -1;
            }
        }
        for (j = 0; j < SKILL_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)r->r_skills[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_struct_info write error");
                return -1;
            }
        }
        if (Packet_printf(&connp->c, "%b%hd%b", (unsigned)r->r_mhp, (int)r->r_exp,
            (unsigned)r->infra) <= 0)
        {
            Destroy_connection(ind, "Send_race_struct_info write error");
            return -1;
        }
        for (j = 0; j < PF_SIZE; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)r->pflags[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_struct_info write error");
                return -1;
            }
        }
        for (j = 0; j < OF_SIZE; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)r->flags[j]) <= 0)
            {
                Destroy_connection(ind, "Send_race_struct_info write error");
                return -1;
            }
        }
        for (j = 0; j < ELEM_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)r->el_info[j].res_level) <= 0)
            {
                Destroy_connection(ind, "Send_race_struct_info write error");
                return -1;
            }
        }
    }

    return 1;
}


int Send_class_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b j;
    struct player_class *c;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for class info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_CLASS,
        (unsigned)player_cmax()) <= 0)
    {
        Destroy_connection(ind, "Send_class_struct_info write error");
        return -1;
    }

    for (c = classes; c; c = c->next)
    {
        byte tval = 0;

        if (c->magic.num_books)
            tval = c->magic.books[0].tval;

        if (Packet_printf(&connp->c, "%b%s", c->cidx, c->name) <= 0)
        {
            Destroy_connection(ind, "Send_class_struct_info write error");
            return -1;
        }

        /* Transfer other fields here */
        for (j = 0; j < STAT_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)c->c_adj[j]) <= 0)
            {
                Destroy_connection(ind, "Send_class_struct_info write error");
                return -1;
            }
        }
        for (j = 0; j < SKILL_MAX; j++)
        {
            if (Packet_printf(&connp->c, "%hd", (int)c->c_skills[j]) <= 0)
            {
                Destroy_connection(ind, "Send_class_struct_info write error");
                return -1;
            }
        }
        if (Packet_printf(&connp->c, "%b%hd", (unsigned)c->c_mhp, (int)c->c_exp) <= 0)
        {
            Destroy_connection(ind, "Send_class_struct_info write error");
            return -1;
        }
        for (j = 0; j < PF_SIZE; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)c->pflags[j]) <= 0)
            {
                Destroy_connection(ind, "Send_class_struct_info write error");
                return -1;
            }
        }
        if (Packet_printf(&connp->c, "%b%b%c", (unsigned)c->magic.total_spells, (unsigned)tval,
            c->magic.num_books) <= 0)
        {
            Destroy_connection(ind, "Send_class_struct_info write error");
            return -1;
        }
        for (j = 0; j < (u32b)c->magic.num_books; j++)
        {
            struct class_book *book = &c->magic.books[j];

            if (Packet_printf(&connp->c, "%s", book->realm->name) <= 0)
            {
                Destroy_connection(ind, "Send_class_struct_info write error");
                return -1;
            }
        }
    }

    return 1;
}


int Send_body_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    int j;
    struct player_body *b;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for body info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_BODY,
        (unsigned)player_bmax()) <= 0)
    {
        Destroy_connection(ind, "Send_body_struct_info write error");
        return -1;
    }

    for (b = bodies; b; b = b->next)
    {
        if (Packet_printf(&connp->c, "%hd%s", b->count, b->name) <= 0)
        {
            Destroy_connection(ind, "Send_body_struct_info write error");
            return -1;
        }

        /* Transfer other fields here */
        for (j = 0; j < b->count; j++)
        {
            if (Packet_printf(&connp->c, "%hd%s", b->slots[j].type, b->slots[j].name) <= 0)
            {
                Destroy_connection(ind, "Send_body_struct_info write error");
                return -1;
            }
        }
    }

    return 1;
}


int Send_socials_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for socials info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_SOCIALS,
        (unsigned)z_info->soc_max) <= 0)
    {
        Destroy_connection(ind, "Send_socials_struct_info write error");
        return -1;
    }

    for (i = 0; i < (u32b)z_info->soc_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", soc_info[i].name) <= 0)
        {
            Destroy_connection(ind, "Send_socials_struct_info write error");
            return -1;
        }

        /* Transfer other fields here */
        if (Packet_printf(&connp->c, "%b", (unsigned)soc_info[i].target) <= 0)
        {
            Destroy_connection(ind, "Send_socials_struct_info write error");
            return -1;
        }
    }

    return 1;
}


int Send_kind_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i;
    int j;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for kind info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_KINDS,
        (unsigned)z_info->k_max) <= 0)
    {
        Destroy_connection(ind, "Send_kind_struct_info write error");
        return -1;
    }

    for (i = 0; i < (u32b)z_info->k_max; i++)
    {
        s16b ac = 0;

        /* Hack -- put flavor index into unused field "ac" */
        if (k_info[i].flavor) ac = (s16b)k_info[i].flavor->fidx;

        if (Packet_printf(&connp->c, "%s", (k_info[i].name? k_info[i].name: "")) <= 0)
        {
            Destroy_connection(ind, "Send_kind_struct_info write error");
            return -1;
        }

        /* Transfer other fields here */
        if (Packet_printf(&connp->c, "%b%b%lu%hd", (unsigned)k_info[i].tval,
            (unsigned)k_info[i].sval, k_info[i].kidx, (int)ac) <= 0)
        {
            Destroy_connection(ind, "Send_kind_struct_info write error");
            return -1;
        }
        for (j = 0; j < KF_SIZE; j++)
        {
            if (Packet_printf(&connp->c, "%b", (unsigned)k_info[i].kind_flags[j]) <= 0)
            {
                Destroy_connection(ind, "Send_kind_struct_info write error");
                return -1;
            }
        }
    }

    return 1;
}


int Send_ego_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for ego info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_EGOS,
        (unsigned)z_info->e_max) <= 0)
    {
        Destroy_connection(ind, "Send_ego_struct_info write error");
        return -1;
    }

    for (i = 0; i < (u32b)z_info->e_max; i++)
    {
        u16b max = 0;
        struct poss_item *poss;

        if (Packet_printf(&connp->c, "%s", (e_info[i].name? e_info[i].name: "")) <= 0)
        {
            Destroy_connection(ind, "Send_ego_struct_info write error");
            return -1;
        }

        /* Count possible egos */
        poss = e_info[i].poss_items;
        while (poss)
        {
            max++;
            poss = poss->next;
        }

        /* Transfer other fields here */
        if (Packet_printf(&connp->c, "%lu%hu", e_info[i].eidx, max) <= 0)
        {
            Destroy_connection(ind, "Send_ego_struct_info write error");
            return -1;
        }

        poss = e_info[i].poss_items;
        while (poss)
        {
            if (Packet_printf(&connp->c, "%lu", poss->kidx) <= 0)
            {
                Destroy_connection(ind, "Send_ego_struct_info write error");
                return -1;
            }

            poss = poss->next;
        }
    }

    return 1;
}


int Send_hints_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u16b max = 0;
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

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_HINTS,
        (unsigned)max) <= 0)
    {
        Destroy_connection(ind, "Send_hints_struct_info write error");
        return -1;
    }

    h = hints;
    while (h)
    {
        if (Packet_printf(&connp->c, "%s", h->hint) <= 0)
        {
            Destroy_connection(ind, "Send_hints_struct_info write error");
            return -1;
        }

        h = h->next;
    }

    return 1;
}


int Send_rinfo_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for rinfo info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_RINFO,
        (unsigned)z_info->r_max) <= 0)
    {
        Destroy_connection(ind, "Send_rinfo_struct_info write error");
        return -1;
    }

    for (i = 0; i < (u32b)z_info->r_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", (r_info[i].name? r_info[i].name: "")) <= 0)
        {
            Destroy_connection(ind, "Send_rinfo_struct_info write error");
            return -1;
        }
    }

    return 1;
}


int Send_rbinfo_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u16b max = 0;
    struct monster_base *mb;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for rbinfo info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    /* Count monster base races */
    mb = rb_info;
    while (mb)
    {
        max++;
        mb = mb->next;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_RBINFO,
        (unsigned)max) <= 0)
    {
        Destroy_connection(ind, "Send_rbinfo_struct_info write error");
        return -1;
    }

    mb = rb_info;
    while (mb)
    {
        if (Packet_printf(&connp->c, "%s", mb->name) <= 0)
        {
            Destroy_connection(ind, "Send_rbinfo_struct_info write error");
            return -1;
        }

        mb = mb->next;
    }

    return 1;
}


int Send_curse_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for curse info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_CURSES,
        (unsigned)z_info->curse_max) <= 0)
    {
        Destroy_connection(ind, "Send_curse_struct_info write error");
        return -1;
    }

    for (i = 0; i < (u32b)z_info->curse_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", (curses[i].name? curses[i].name: "")) <= 0)
        {
            Destroy_connection(ind, "Send_curse_struct_info write error");
            return -1;
        }

        /* Transfer other fields here */
        if (Packet_printf(&connp->c, "%s", (curses[i].desc? curses[i].desc: "")) <= 0)
        {
            Destroy_connection(ind, "Send_curse_struct_info write error");
            return -1;
        }
    }

    return 1;
}


int Send_realm_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u16b max = 0;
    struct magic_realm *realm;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for realm info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    /* Count player magic realms */
    for (realm = realms; realm; realm = realm->next) max++;

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_REALM,
        (unsigned)max) <= 0)
    {
        Destroy_connection(ind, "Send_realm_struct_info write error");
        return -1;
    }

    for (realm = realms; realm; realm = realm->next)
    {
        const char *spell_noun = (realm->spell_noun? realm->spell_noun: "");
        const char *verb = (realm->verb? realm->verb: "");

        if (Packet_printf(&connp->c, "%s", realm->name) <= 0)
        {
            Destroy_connection(ind, "Send_realm_struct_info write error");
            return -1;
        }

        /* Transfer other fields here */
        if (Packet_printf(&connp->c, "%s%s", spell_noun, verb) <= 0)
        {
            Destroy_connection(ind, "Send_realm_struct_info write error");
            return -1;
        }
    }

    return 1;
}


int Send_feat_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for feat info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_FEAT,
        (unsigned)z_info->f_max) <= 0)
    {
        Destroy_connection(ind, "Send_feat_struct_info write error");
        return -1;
    }

    for (i = 0; i < (u32b)z_info->f_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", (f_info[i].name? f_info[i].name: "")) <= 0)
        {
            Destroy_connection(ind, "Send_feat_struct_info write error");
            return -1;
        }
    }

    return 1;
}


int Send_trap_struct_info(int ind)
{
    connection_t *connp = get_connection(ind);
    u32b i;

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for trap info (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    if (Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_STRUCT_INFO, (int)STRUCT_INFO_TRAP,
        (unsigned)z_info->trap_max) <= 0)
    {
        Destroy_connection(ind, "Send_trap_struct_info write error");
        return -1;
    }

    for (i = 0; i < (u32b)z_info->trap_max; i++)
    {
        if (Packet_printf(&connp->c, "%s", (trap_info[i].desc? trap_info[i].desc: "")) <= 0)
        {
            Destroy_connection(ind, "Send_trap_struct_info write error");
            return -1;
        }
    }

    return 1;
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

    return Packet_printf(&connp->c, "%b%s%hd%ld%ld%hd%hd%hd%s%s", (unsigned)PKT_DEATH_CAUSE,
        p->death_info.title, (int)p->death_info.lev, p->death_info.exp,
        p->death_info.au, (int)p->death_info.wpos.wy, (int)p->death_info.wpos.wx,
        (int)p->death_info.wpos.depth, p->death_info.died_from, p->death_info.ctime);
}


int Send_winner(struct player *p)
{
    connection_t *connp = get_connp(p, "winner");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b", (unsigned)PKT_WINNER);
}


int Send_lvl(struct player *p, int lev, int mlev)
{
    connection_t *connp = get_connp(p, "level");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_LEV, lev, mlev);
}


int Send_weight(struct player *p, int weight, int max_weight)
{
    connection_t *connp = get_connp(p, "weight");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_WEIGHT, weight, max_weight);
}


int Send_plusses(struct player *p, int dd, int ds, int mhit, int mdam, int shit, int sdam)
{
    connection_t *connp = get_connp(p, "plusses");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd%hd%hd%hd", (unsigned)PKT_PLUSSES, dd, ds, mhit,
        mdam, shit, sdam);
}


int Send_ac(struct player *p, int base, int plus)
{
    connection_t *connp = get_connp(p, "ac");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_AC, base, plus);
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


int Send_various(struct player *p, int hgt, int wgt, int age)
{
    connection_t *connp = get_connp(p, "various");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd", (unsigned)PKT_VARIOUS, hgt, wgt, age);
}


int Send_stat(struct player *p, int stat, int stat_top, int stat_use,
    int stat_max, int stat_add, int stat_cur)
{
    connection_t *connp = get_connp(p, "stat");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%hd%hd%hd%hd%hd", (unsigned)PKT_STAT, stat, stat_top,
        stat_use, stat_max, stat_add, stat_cur);
}


int Send_history(struct player *p, int line, const char *hist)
{
    connection_t *connp = get_connp(p, "history");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%s", (unsigned)PKT_HISTORY, line, hist);
}


int Send_autoinscription(struct player *p, struct object_kind *kind)
{
    const char *note;

    connection_t *connp = get_connp(p, "autoinscriptions");
    if (connp == NULL) return 0;

    note = get_autoinscription(p, kind);
    if (!note) note = "";

    return Packet_printf(&connp->c, "%b%lu%s", (unsigned)PKT_AUTOINSCR, kind->kidx, note);
}


int Send_index(struct player *p, int i, int index, byte type)
{
    connection_t *connp = get_connp(p, "index");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%b", (unsigned)PKT_INDEX, i, index, (unsigned)type);
}


int Send_item_request(struct player *p, byte tester_hook, char *dice_string)
{
    connection_t *connp = get_connp(p, "item request");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%s", (unsigned)PKT_ITEM_REQUEST, (unsigned)tester_hook,
        dice_string);
}


int Send_title(struct player *p, const char *title)
{
    connection_t *connp = get_connp(p, "title");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%s", (unsigned)PKT_TITLE, title);
}


int Send_turn(struct player *p, u32b game_turn, u32b player_turn, u32b active_turn)
{
    connection_t *connp = get_connp(p, "turn");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%lu%lu%lu", (unsigned)PKT_TURN, game_turn, player_turn,
        active_turn);
}


int Send_depth(struct player *p, int depth, int maxdepth, const char *depths)
{
    connection_t *connp = get_connp(p, "depth");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%s", (unsigned)PKT_DEPTH, depth, maxdepth, depths);
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


int Send_recall(struct player *p, s16b word_recall, s16b deep_descent)
{
    connection_t *connp = get_connp(p, "recall");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_RECALL, (int)word_recall,
        (int)deep_descent);
}


int Send_state(struct player *p, bool stealthy, bool resting, bool unignoring)
{
    connection_t *connp = get_connp(p, "state");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd%hd%hd", (unsigned)PKT_STATE, (int)stealthy,
        (int)resting, (int)unignoring, (int)p->obj_feeling, (int)p->mon_feeling);
}


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
    u16b a, n;
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
        while (mode && (lineref[x1].c == c) && (lineref[x1].a == a) && (x1 < max_col))
        {
            /* Increment count and column */
            n++;
            x1++;
        }

        /* RLE-II if there are at least 3 similar grids in a row */
        if ((mode == RLE_LARGE) && (n >= 3))
        {
            /* Output the info */
            Packet_printf(buf, "%c%hu%c%hu", dummy, (unsigned)n, (int)c, (unsigned)a);

            /* Start again after the run */
            i = x1 - 1;

            /* Count bytes */
            b += 6;
        }

        /* RLE-I if there are at least 2 similar grids in a row */
        else if ((mode == RLE_CLASSIC) && (n >= 2))
        {
            /* Set bit 0x40 of a */
            a |= 0x40;

            /* Output the info */
            Packet_printf(buf, "%c%hu%hu", (int)c, (unsigned)a, (unsigned)n);

            /* Start again after the run */
            i = x1 - 1;

            /* Count bytes */
            b += 5;
        }
        else
        {
            /* Normal, single grid */
            Packet_printf(buf, "%c%hu", (int)c, (unsigned)a);

            /* Count bytes */
            b += 3;
        }
    }

    /* Report total bytes */
    return b;
}


static void end_mind_link(struct player *p, struct player *p_ptr2)
{
    p->esp_link = 0;
    p->esp_link_type = 0;
    p->upkeep->redraw |= PR_MAP;

    if (p_ptr2)
    {
        p_ptr2->esp_link = 0;
        p_ptr2->esp_link_type = 0;

        msg(p, "You break the mind link with %s.", p_ptr2->name);
        msg(p_ptr2, "%s breaks the mind link with you.", p->name);
    }
    else
        msg(p, "Ending mind link.");
}


static struct player *find_player(s32b id)
{
    int i;

    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        if (p->id == id) return p;
    }

    /* Assume none */
    return NULL;
}


static void break_mind_link(struct player *p)
{
    if (p->esp_link && (p->esp_link_type == LINK_DOMINANT))
        end_mind_link(p, find_player(p->esp_link));
}


static connection_t* get_mind_link(struct player *p)
{
    if (p->esp_link && (p->esp_link_type == LINK_DOMINATED))
    {
        struct player *p_ptr2 = find_player(p->esp_link);

        if (p_ptr2) return get_connection(p_ptr2->conn);
        end_mind_link(p, NULL);
    }
    return NULL;
}


/*
 * As an attempt to lower bandwidth requirements, each line is run length
 * encoded.  Non-encoded grids are sent as normal, but if a grid is
 * repeated at least twice, then bit 0x40 of the attribute is set, and
 * the next byte contains the number of repetitions of the previous grid.
 */
#define DUNGEON_RLE_MODE(P) ((P)->use_graphics? RLE_LARGE: RLE_CLASSIC)
int Send_line_info(struct player *p, int y)
{
    struct player *p_ptr2 = NULL;
    connection_t *connp, *connp2;
    int screen_wid, screen_wid2 = 0;

    connp = get_connp(p, "line info");
    if (connp == NULL) return 0;

    screen_wid = p->screen_cols / p->tile_wid;

    connp2 = get_mind_link(p);
    if (connp2)
    {
        p_ptr2 = find_player(p->esp_link);
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


int Send_speed(struct player *p, int speed, int mult)
{
    connection_t *connp = get_connp(p, "speed");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd", (unsigned)PKT_SPEED, speed, mult);
}


int Send_study(struct player *p, int study, bool can_study_book)
{
    connection_t *connp = get_connp(p, "study");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%c", (unsigned)PKT_STUDY, study, (int)can_study_book);
}


int Send_count(struct player *p, byte type, s16b count)
{
    connection_t *connp = get_connp(p, "count");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%hd", (unsigned)PKT_COUNT, (unsigned)type, (int)count);
}


int Send_show_floor(struct player *p, byte mode)
{
    connection_t *connp = get_connp(p, "show_floor");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b", (unsigned)PKT_SHOW_FLOOR,
        (unsigned)mode);
}


int Send_char(struct player *p, int x, int y, u16b a, char c, u16b ta, char tc)
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
        struct player *p_ptr2 = find_player(p->esp_link);

        if (p_ptr2->use_graphics && (p_ptr2->remote_term == NTERM_WIN_OVERHEAD))
        {
            Packet_printf(&connp2->c, "%b%b%b%hu%c%hu%c", (unsigned)PKT_CHAR, (unsigned)x,
                (unsigned)y, (unsigned)a, (int)c, (unsigned)ta, (int)tc);
        }
        else
        {
            Packet_printf(&connp2->c, "%b%b%b%hu%c", (unsigned)PKT_CHAR, (unsigned)x, (unsigned)y,
                (unsigned)a, (int)c);
        }
    }

    if (p->use_graphics && (p->remote_term == NTERM_WIN_OVERHEAD))
    {
        return Packet_printf(&connp->c, "%b%b%b%hu%c%hu%c", (unsigned)PKT_CHAR, (unsigned)x,
            (unsigned)y, (unsigned)a, (int)c, (unsigned)ta, (int)tc);
    }
    return Packet_printf(&connp->c, "%b%b%b%hu%c", (unsigned)PKT_CHAR, (unsigned)x, (unsigned)y,
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


int Send_book_info(struct player *p, int book, const char *name)
{
    connection_t *connp = get_connp(p, "book info");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%s", (unsigned)PKT_BOOK_INFO, book, name);
}


int Send_floor(struct player *p, byte num, const struct object *obj, struct object_xtra *info_xtra)
{
    byte ignore = ((obj->known->notice & OBJ_NOTICE_IGNORE)? 1: 0);
    connection_t *connp = get_connp(p, "floor");
    if (connp == NULL) return 0;

    Packet_printf(&connp->c, "%b%b", (unsigned)PKT_FLOOR, (unsigned)num);
    Packet_printf(&connp->c, "%b%b%hd%lu%ld%b%hd", (unsigned)obj->tval, (unsigned)obj->sval,
        obj->number, obj->note, obj->pval, (unsigned)ignore, obj->oidx);
    Packet_printf(&connp->c, "%b%b%b%b%b%hd%b%b%b%b%hd%b%hd", (unsigned)info_xtra->attr,
        (unsigned)info_xtra->act, (unsigned)info_xtra->aim, (unsigned)info_xtra->fuel,
        (unsigned)info_xtra->fail, info_xtra->slot, (unsigned)info_xtra->known,
        (unsigned)info_xtra->carry, (unsigned)info_xtra->quality_ignore,
        (unsigned)info_xtra->ignored, info_xtra->eidx, (unsigned)info_xtra->magic, info_xtra->bidx);
    Packet_printf(&connp->c, "%s%s%s%s%s", info_xtra->name, info_xtra->name_terse,
        info_xtra->name_base, info_xtra->name_curse, info_xtra->name_power);

    return 1;
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


int Send_store(struct player *p, char pos, byte attr, s16b wgt, byte number, byte owned,
    s32b price, byte tval, byte max, s16b bidx, const char *name)
{
    connection_t *connp = get_connp(p, "store");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%b%hd%b%b%ld%b%b%hd%s", (unsigned)PKT_STORE,
        (int)pos, (unsigned)attr, (int)wgt, (unsigned)number, (unsigned)owned,
        price, (unsigned)tval, (unsigned)max, (int)bidx, name);
}


int Send_store_info(struct player *p, int num, char *name, char *owner, int items, s32b purse)
{
    connection_t *connp = get_connp(p, "store info");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%s%s%hd%ld", (unsigned)PKT_STORE_INFO,
        num, name, owner, items, purse);
}


int Send_target_info(struct player *p, int x, int y, bool dble, const char *str)
{
    char buf[NORMAL_WID];

    connection_t *connp = get_connp(p, "target info");
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


int Send_skills(struct player *p)
{
    s16b skills[11];
    int i;

    connection_t *connp = get_connp(p, "skills");
    if (connp == NULL) return 0;

    /* Melee */
    skills[0] = get_melee_skill(p);

    /* Ranged */
    skills[1] = get_ranged_skill(p);

    /* Basic abilities */
    skills[2] = p->state.skills[SKILL_SAVE];
    skills[3] = p->state.skills[SKILL_STEALTH];
    skills[4] = p->state.skills[SKILL_SEARCH];
    skills[5] = p->state.skills[SKILL_DISARM_PHYS];
    skills[6] = p->state.skills[SKILL_DISARM_MAGIC];
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


int Send_aware(struct player *p, u16b num)
{
    int i;

    connection_t *connp = get_connp(p, "aware");
    if (connp == NULL) return 0;

    Packet_printf(&connp->c, "%b%hu", (unsigned)PKT_AWARE, (unsigned)num);

    if (num == z_info->k_max)
    {
        for (i = 0; i < z_info->k_max; i++)
            Packet_printf(&connp->c, "%b", (unsigned)p->obj_aware[i]);
    }
    else
        Packet_printf(&connp->c, "%b", (unsigned)p->obj_aware[num]);

    return 1;
}


int Send_everseen(struct player *p, u16b num)
{
    int i;

    connection_t *connp = get_connp(p, "everseen");
    if (connp == NULL) return 0;

    Packet_printf(&connp->c, "%b%hu", (unsigned)PKT_EVERSEEN, (unsigned)num);

    if (num == z_info->k_max)
    {
        for (i = 0; i < z_info->k_max; i++)
            Packet_printf(&connp->c, "%b", (unsigned)p->kind_everseen[i]);
    }
    else
        Packet_printf(&connp->c, "%b", (unsigned)p->kind_everseen[num]);

    return 1;
}


int Send_ego_everseen(struct player *p, u16b num)
{
    int i;

    connection_t *connp = get_connp(p, "ego_everseen");
    if (connp == NULL) return 0;

    Packet_printf(&connp->c, "%b%hu", (unsigned)PKT_EGO_EVERSEEN, (unsigned)num);

    if (num == z_info->e_max)
    {
        for (i = 0; i < z_info->e_max; i++)
            Packet_printf(&connp->c, "%b", (unsigned)p->ego_everseen[i]);
    }
    else
        Packet_printf(&connp->c, "%b", (unsigned)p->ego_everseen[num]);

    return 1;
}


int Send_cursor(struct player *p, char vis, char x, char y)
{
    connection_t *connp = get_connp(p, "cursor");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%c%c", (unsigned)PKT_CURSOR,
        (int)vis, (int)x, (int)y);
}


int Send_objflags(struct player *p, int line)
{
    connection_t *connp = get_connp(p, "objflags");
    if (connp == NULL) return 0;

    /* Put a header on the packet */
    Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_OBJFLAGS, line);

    /* Encode and send the attr/char stream */
    rle_encode(&connp->c, p->hist_flags[line], p->body.count + 1, DUNGEON_RLE_MODE(p));

    return 1;
}


int Send_spell_desc(struct player *p, int book, int i, char *out_val)
{
    connection_t *connp = get_connp(p, "spell description");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%S", (unsigned)PKT_SPELL_DESC, book, i, out_val);
}


int Send_dtrap(struct player *p, byte dtrap)
{
    connection_t *connp = get_connp(p, "dtrap");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b", (unsigned)PKT_DTRAP, (unsigned)dtrap);
}


int Send_term_info(struct player *p, int mode, u16b arg)
{
    connection_t *connp = get_connp(p, "term info");
    if (connp == NULL) return 0;

    /* Hack -- do not change terms too often */
    if (mode == NTERM_ACTIVATE)
    {
        if (p->remote_term == (byte)arg) return 1;
        p->remote_term = (byte)arg;
    }

    return Packet_printf(&connp->c, "%b%c%hu", (unsigned)PKT_TERM, mode, (unsigned)arg);
}


int Send_player_pos(struct player *p)
{
    connection_t *connp = get_connp(p, "player pos");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd%hd%hd%hd", (unsigned)PKT_PLAYER, (int)p->px,
        (int)p->offset_x, (int)p->py, (int)p->offset_y);
}


int Send_play(int ind)
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


int Send_char_info(struct player *p, byte ridx, byte cidx, byte psex)
{
    connection_t *connp = get_connp(p, "char info");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%b%b", (unsigned)PKT_CHAR_INFO,
        (unsigned)ridx, (unsigned)cidx, (unsigned)psex);
}


struct birth_options
{
    bool force_descend;
    bool no_recall;
    bool no_artifacts;
    bool feelings;
    bool no_selling;
    bool start_kit;
    bool no_stores;
    bool no_ghost;
    bool fruit_bat;
};


int Send_birth_options(int ind, struct birth_options *options)
{
    connection_t *connp = get_connection(ind);

    if (connp->state != CONN_SETUP)
    {
        errno = 0;
        plog_fmt("Connection not ready for birth options (%d.%d.%d)", ind, connp->state, connp->id);
        return 0;
    }

    return Packet_printf(&connp->c, "%b%c%c%c%c%c%c%c%c%c", (unsigned)PKT_OPTIONS,
        (int)options->force_descend, (int)options->no_recall, (int)options->no_artifacts,
        (int)options->feelings, (int)options->no_selling, (int)options->start_kit,
        (int)options->no_stores, (int)options->no_ghost, (int)options->fruit_bat);
}


/*
 * Send a character dump to the client
 *
 * mode: 1 = normal dump, 2 = manual death dump
 */
bool Send_dump_character(connection_t *connp, const char *dumpname, int mode)
{
    char pathname[MSG_LEN];
    char buf[MSG_LEN];
    ang_file *fp;
    const char *tok;

    /* Build the filename */
    path_build(pathname, sizeof(pathname), ANGBAND_DIR_SCORES, dumpname);

    /* Open the file for reading */
    fp = file_open(pathname, MODE_READ, FTYPE_TEXT);
    if (!fp) return false;

    /* Begin sending */
    switch (mode)
    {
        case 1: tok = "BEGIN_NORMAL_DUMP"; break;
        case 2: tok = "BEGIN_MANUAL_DUMP"; break;
    }
    Packet_printf(&connp->c, "%b%s", (unsigned)PKT_CHAR_DUMP, tok);

    /* Process the file */
    while (file_getl(fp, buf, sizeof(buf)))
        Packet_printf(&connp->c, "%b%s", (unsigned)PKT_CHAR_DUMP, buf);

    /* End sending */
    switch (mode)
    {
        case 1: tok = "END_NORMAL_DUMP"; break;
        case 2: tok = "END_MANUAL_DUMP"; break;
    }
    Packet_printf(&connp->c, "%b%s", (unsigned)PKT_CHAR_DUMP, tok);

    /* Close the file */
    file_close(fp);

    return true;
}


int Send_message(struct player *p, const char *msg, u16b typ)
{
    char buf[MSG_LEN];

    connection_t *connp = get_connp(p, "message");
    if (connp == NULL) return 0;

    /* Flush messages */
    if (msg == NULL)
        return Packet_printf(&connp->c, "%b", (unsigned)PKT_MESSAGE_FLUSH);

    /* Clip end of msg if too long */
    my_strcpy(buf, msg, sizeof(buf));

    return Packet_printf(&connp->c, "%b%S%hu", (unsigned)PKT_MESSAGE, buf, (unsigned)typ);
}


int Send_item(struct player *p, const struct object *obj, int wgt, s32b price,
    struct object_xtra *info_xtra)
{
    byte ignore = ((obj->known->notice & OBJ_NOTICE_IGNORE)? 1: 0);
    connection_t *connp = get_connp(p, "item");
    if (connp == NULL) return 0;

    /* Packet and base info */
    Packet_printf(&connp->c, "%b%b%b", (unsigned)PKT_ITEM, (unsigned)obj->tval,
        (unsigned)info_xtra->equipped);

    /* Object info */
    Packet_printf(&connp->c, "%b%hd%hd%ld%lu%ld%b%hd", (unsigned)obj->sval, wgt, obj->number,
        price, obj->note, obj->pval, (unsigned)ignore, obj->oidx);

    /* Extra info */
    Packet_printf(&connp->c, "%b%b%b%b%b%hd%b%b%b%b%b%hd%b%hd", (unsigned)info_xtra->attr,
        (unsigned)info_xtra->act, (unsigned)info_xtra->aim, (unsigned)info_xtra->fuel,
        (unsigned)info_xtra->fail, info_xtra->slot, (unsigned)info_xtra->stuck,
        (unsigned)info_xtra->known, (unsigned)info_xtra->sellable,
        (unsigned)info_xtra->quality_ignore, (unsigned)info_xtra->ignored, info_xtra->eidx,
        (unsigned)info_xtra->magic, info_xtra->bidx);

    /* Descriptions */
    Packet_printf(&connp->c, "%s%s%s%s%s", info_xtra->name, info_xtra->name_terse,
        info_xtra->name_base, info_xtra->name_curse, info_xtra->name_power);

    return 1;
}


int Send_store_sell(struct player *p, s32b price, bool reset)
{
    connection_t *connp = get_connp(p, "store sell");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%ld%hd", (unsigned)PKT_SELL, price, (int)reset);
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


int Send_special_line(struct player *p, int max, int last, int line, byte attr, const char *buf)
{
    char temp[NORMAL_WID];

    connection_t *connp = get_connp(p, "special line");
    if (connp == NULL) return 0;

    my_strcpy(temp, buf, sizeof(temp));
    return Packet_printf(&connp->c, "%b%hd%hd%hd%b%s", (unsigned)PKT_SPECIAL_LINE,
        max, last, line, (unsigned)attr, temp);
}


int Send_fullmap(struct player *p, int y)
{
    connection_t *connp = get_connp(p, "full map");
    if (connp == NULL) return 0;

    /* Packet header */
    Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_FULLMAP, y);

    /* Reset the line counter */
    if (y == -1) return 1;

    /* Encode and send the transparency attr/char stream */
    if (p->use_graphics)
        rle_encode(&connp->c, p->trn_info[y], z_info->dungeon_wid, RLE_LARGE);

    /* Encode and send the attr/char stream */
    rle_encode(&connp->c, p->scr_info[y], z_info->dungeon_wid, RLE_LARGE);

    return 1;
}


int Send_poly(struct player *p, int race)
{
    connection_t *connp = get_connp(p, "poly");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%hd", (unsigned)PKT_POLY, race);
}


int Send_store_leave(struct player *p)
{
    connection_t *connp = get_connp(p, "store leave");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b", (unsigned)PKT_STORE_LEAVE);
}


int Send_ignore(struct player *p)
{
    int i, j, repeat = 0;
    byte last = p->ego_ignore_types[0][0];

    connection_t *connp = get_connp(p, "ignore");
    if (connp == NULL) return 0;

    Packet_printf(&connp->c, "%b", (unsigned)PKT_IGNORE);

    /* Flavour-aware ignoring */
    for (i = 0; i < z_info->k_max; i++)
        Packet_printf(&connp->c, "%b", (unsigned)p->kind_ignore[i]);

    /* Ego ignoring */
    for (i = 0; i < z_info->e_max; i++)
    {
        for (j = ITYPE_NONE; j < ITYPE_MAX; j++)
        {
            if (p->ego_ignore_types[i][j] == last)
                repeat++;
            else
            {
                Packet_printf(&connp->c, "%hd%b", (int)repeat, (unsigned)last);
                repeat = 1;
                last = p->ego_ignore_types[i][j];
            }
        }
    }
    Packet_printf(&connp->c, "%hd%b", (int)repeat, (unsigned)last);

    /* Quality ignoring */
    for (i = ITYPE_NONE; i < ITYPE_MAX; i++)
        Packet_printf(&connp->c, "%b", (unsigned)p->opts.ignore_lvl[i]);

    return 1;
}


int Send_flush(struct player *p, bool fresh, bool delay)
{
    connection_t *connp = get_connp(p, "flush");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%c%c", (unsigned)PKT_FLUSH, (int)fresh, (int)delay);
}


int Send_channel(struct player *p, byte n, const char *virt)
{
    connection_t *connp = get_connp(p, "channel");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->c, "%b%b%s", (unsigned)PKT_CHANNEL,
        (unsigned)n, (virt? virt: channels[n].name));
}


/*** Commands ***/


int cmd_ignore_drop(struct player *p)
{
    connection_t *connp = get_connp(p, "ignore_drop");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->q, "%b", (unsigned)PKT_IGNORE_DROP);
}


int cmd_run(struct player *p, int dir)
{
    connection_t *connp = get_connp(p, "run");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->q, "%b%c", (unsigned)PKT_RUN, (int)dir);
}


int cmd_rest(struct player *p, s16b resting)
{
    connection_t *connp = get_connp(p, "rest");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->q, "%b%hd", (unsigned)PKT_REST, (int)resting);
}


int cmd_tunnel(struct player *p)
{
    byte starting = 0;

    connection_t *connp = get_connp(p, "tunnel");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->q, "%b%c%b", (unsigned)PKT_TUNNEL, (int)p->digging_dir,
        (unsigned)starting);
}


int cmd_fire_at_nearest(struct player *p)
{
    byte starting = 0;

    connection_t *connp = get_connp(p, "fire_at_nearest");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->q, "%b%b", (unsigned)PKT_FIRE_AT_NEAREST, (unsigned)starting);
}


int cmd_cast(struct player *p, s16b book, s16b spell, int dir)
{
    byte starting = 0;

    connection_t *connp = get_connp(p, "cast");
    if (connp == NULL) return 0;

    return Packet_printf(&connp->q, "%b%hd%hd%c%b", (unsigned)PKT_SPELL, (int)book, (int)spell,
        (int)dir, (unsigned)starting);
}


/*** Receiving ***/


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
static int Receive_undefined(int ind)
{
    connection_t *connp = get_connection(ind);
    byte what = (byte)connp->r.ptr[0];

    errno = 0;
    plog_fmt("Unknown packet type %s (%03d,%02x)", connp->nick, what, connp->state);
    Destroy_connection(ind, "Unknown packet type");
    return -1;
}


static int Receive_verify(int ind)
{
    connection_t *connp = get_connection(ind);
    int n, i, local_size = 0;
    byte ch;
    char type;
    s16b size;
    bool discard = false;
    byte a;
    char c;

    type = size = 0;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &type, &size)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "verify read error");
        return n;
    }

    /* Check size */
    switch (type)
    {
        case 0: local_size = get_flavor_max(); break;
        case 1: local_size = z_info->f_max * LIGHTING_MAX; break;
        case 2: local_size = z_info->k_max; break;
        case 3: local_size = z_info->r_max; break;
        case 4: local_size = PROJ_MAX * BOLT_MAX; break;
        case 5: local_size = z_info->trap_max * LIGHTING_MAX; break;
        default: discard = true;
    }
    if (local_size != size) discard = true;

    /* Finally read the data */
    for (i = 0; i < size; i++)
    {
        if ((n = Packet_scanf(&connp->r, "%b%c", &a, &c)) <= 0)
        {
            if (n == -1) Destroy_connection(ind, "verify_visual read error");
            return n;
        }

        if (discard) continue;

        switch (type)
        {
            case 0:
                connp->Client_setup.flvr_x_attr[i] = a;
                connp->Client_setup.flvr_x_char[i] = c;
                break;
            case 1:
                connp->Client_setup.f_attr[i / LIGHTING_MAX][i % LIGHTING_MAX] = a;
                connp->Client_setup.f_char[i / LIGHTING_MAX][i % LIGHTING_MAX] = c;
                break;
            case 2:
                connp->Client_setup.k_attr[i] = a;
                connp->Client_setup.k_char[i] = c;
                break;
            case 3:
                connp->Client_setup.r_attr[i] = a;
                connp->Client_setup.r_char[i] = c;
                break;
            case 4:
                connp->Client_setup.proj_attr[i / BOLT_MAX][i % BOLT_MAX] = a;
                connp->Client_setup.proj_char[i / BOLT_MAX][i % BOLT_MAX] = c;
                break;
            case 5:
                connp->Client_setup.t_attr[i / LIGHTING_MAX][i % LIGHTING_MAX] = a;
                connp->Client_setup.t_char[i / LIGHTING_MAX][i % LIGHTING_MAX] = c;
                break;
        }
    }

    return 2;
}


static int Receive_icky(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b icky;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &icky)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_icky read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        p->screen_save_depth = icky;
    }

    return 1;
}


static int Receive_symbol_query(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    char buf[NORMAL_WID];
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_symbol_query read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Perform query */
        do_cmd_query_symbol(p, buf);
    }

    return 1;
}


static int Receive_poly_race(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    char buf[NORMAL_WID];
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_poly_race read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        my_strcpy(p->tempbuf, buf, sizeof(p->tempbuf));
    }

    return 1;
}


static int Receive_breath(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_breath read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_breath(p, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_walk(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    char dir;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_walk read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Disturb if running or resting */
        if (p->upkeep->running || player_is_resting(p))
        {
            disturb(p, 0);
            return 1;
        }

        if (has_energy(p))
        {
            do_cmd_walk(p, dir);
            return 2;
        }

        /*
         * If we have no commands queued, then queue our walk request.
         */
        if (!connp->q.len)
        {
            Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
            return 0;
        }

        /*
         * If we have a walk command queued at the end of the queue,
         * then replace it with this queue request.
         */
        if (connp->q.buf[connp->q.len - 2] == ch)
        {
            connp->q.len -= 2;
            Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
            return 0;
        }
    }

    return 1;
}


static int Receive_run(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    char dir;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_run read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Start running */
        if (has_energy(p))
        {
            do_cmd_run(p, dir);
            return 2;
        }

        /*
         * If we have no commands queued, then queue our run request.
         */
        if (!connp->q.len)
        {
            Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
            return 0;
        }

        /*
         * If we have a run command queued at the end of the queue,
         * then replace it with this queue request.
         */
        if (connp->q.buf[connp->q.len - 2] == ch)
        {
            connp->q.len -= 2;
            Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
            return 0;
        }
    }

    return 1;
}


static int Receive_tunnel(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch, starting;
    char dir;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%c%b", &ch, &dir, &starting)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_tunnel read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Repeat digging 99 times */
        if (starting)
        {
            p->digging_request = 99;
            p->digging_dir = (byte)dir;
            starting = 0;
        }

        if (do_cmd_tunnel(p)) return 2;

        /* If we don't have enough energy to dig, queue the command */
        Packet_printf(&connp->q, "%b%c%b", (unsigned)ch, (int)dir, (unsigned)starting);
        return 0;
    }

    return 1;
}


static int Receive_aim_wand(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    s16b item;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_aim_wand read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_aim_wand(p, item, dir);
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
    struct player *p;
    byte ch;
    int n;
    s16b item, amt;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &amt)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_drop read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_drop(p, item, amt);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%hd", (unsigned)ch, (int)item, (int)amt);
        return 0;
    }

    return 1;
}


static int Receive_ignore_drop(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_ignore_drop read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            ignore_drop(p);
            return 2;
        }

        Packet_printf(&connp->q, "%b", (unsigned)ch);
        return 0;
    }

    return 1;
}


static int Receive_fire(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    s16b item;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &dir, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_fire read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_fire(p, dir, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c%hd", (unsigned)ch, (int)dir, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_pickup(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    byte ignore;
    s16b item;

    if ((n = Packet_scanf(&connp->r, "%b%b%hd", &ch, &ignore, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_pickup read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        switch (ignore)
        {
            case 0:
            {
                /* Stand still */
                if (has_energy(p))
                {
                    p->ignore = 0;
                    do_cmd_hold(p, item);
                    return 2;
                }

                Packet_printf(&connp->q, "%b%b%hd", (unsigned)ch, (unsigned)ignore, (int)item);
                return 0;
            }

            case 1:
            {
                /* Pick up objects */
                if (!p->timed[TMD_PARALYZED])
                {
                    p->ignore = 1;
                    do_cmd_pickup(p, item);
                    return 2;
                }

                Packet_printf(&connp->q, "%b%b%hd", (unsigned)ch, (unsigned)ignore, (int)item);
                return 0;
            }

            case 2:
            {
                /* Do autopickup */
                if (!p->timed[TMD_PARALYZED])
                {
                    p->ignore = 1;
                    do_cmd_autopickup(p);
                    return 2;
                }

                Packet_printf(&connp->q, "%b%b%hd", (unsigned)ch, (unsigned)ignore, (int)item);
                return 0;
            }
        }

        return 2;
    }

    return 1;
}


static int Receive_destroy(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item, des;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &des)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_destroy read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_destroy(p, item, (bool)des);
        return 2;
    }

    return 1;
}


static int Receive_target_closest(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch, mode;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &mode)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_target_closest read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        target_set_closest(p, mode);
    }

    return 1;
}


static int Receive_cast(int ind, char *errmsg)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    s16b book, spell;
    byte ch, starting;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd%c%b", &ch, &book, &spell, &dir, &starting)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, errmsg);
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Repeat casting 99 times if fire-till-kill mode is active */
        if (starting)
        {
            if (OPT(p, fire_till_kill) && (dir == 5)) p->firing_request = 99;
            else p->firing_request = 1;
            starting = 0;
        }

        if (do_cmd_cast(p, book, spell, dir)) return 2;

        /* If we don't have enough energy to cast, queue the command */
        Packet_printf(&connp->q, "%b%hd%hd%c%b", (unsigned)ch, (int)book, (int)spell, (int)dir,
            (unsigned)starting);
        return 0;
    }

    return 1;
}


static int Receive_spell(int ind)
{
    return Receive_cast(ind, "Receive_spell read error");
}


static int Receive_open(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_open read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_open(p, dir, true);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_quaff(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item;
    char dir;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_quaff read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_quaff_potion(p, item, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)item, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_read(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_read read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_read_scroll(p, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_take_off(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_take_off read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_takeoff(p, item);
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
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_use read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_use_staff(p, item);
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
    struct player *p;
    char dir;
    int n;
    s16b item;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &dir, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_throw read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_throw(p, dir, item);
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
    struct player *p;
    byte ch;
    s16b item, slot;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &slot)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_wield read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_wield(p, item, slot);
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
    struct player *p;
    byte ch;
    s16b item;
    char dir;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_zap read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_zap_rod(p, item, dir);
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
    struct player *p;
    byte ch, mode;
    u32b query;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%b%lu", &ch, &mode, &query)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_target_interactive read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        target_set_interactive(p, mode, query);
    }

    return 1;
}


static int Receive_inscribe(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s16b item;
    char inscription[NORMAL_WID];

    if ((n = Packet_scanf(&connp->r, "%b%hd%s", &ch, &item, inscription)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_inscribe read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_inscribe(p, item, inscription);
    }

    return 1;
}


static int Receive_uninscribe(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s16b item;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_uninscribe read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_uninscribe(p, item);
    }

    return 1;
}


static int Receive_activate(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    s16b item;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_activate read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_activate(p, item, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)item, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_disarm(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_disarm read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_disarm(p, dir, true);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_eat(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_eat read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_eat_food(p, item);
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
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_fill read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_refill(p, item);
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
    struct player *p;
    char dir;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_locate read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_locate(p, dir);
    }

    return 1;
}


static int Receive_map(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch, mode;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &mode)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_map read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (mode == 0) do_cmd_view_map(p);
        else do_cmd_wild_map(p);
    }

    return 1;
}


static int Receive_stealth_mode(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_stealth_mode read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_toggle_stealth(p);
    }

    return 1;
}


static int Receive_quest(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_quest read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        start_quest(p);
        return 2;
    }

    return 1;
}


static int Receive_close(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_close read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_close(p, dir, true);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_gain(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s16b book, spell;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &book, &spell)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_gain read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_study(p, book, spell);
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
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_go_up read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_go_up(p);
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
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_go_down read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            current_clear(p);
            do_cmd_go_down(p);
            return 2;
        }

        Packet_printf(&connp->q, "%b", (unsigned)ch);
        return 0;
    }

    return 1;
}


static int Receive_drop_gold(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s32b amt;

    if ((n = Packet_scanf(&connp->r, "%b%ld", &ch, &amt)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_drop_gold read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_drop_gold(p, amt);
            return 2;
        }

        Packet_printf(&connp->q, "%b%ld", (unsigned)ch, amt);
        return 0;
    }

    return 1;
}


static int Receive_redraw(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_redraw read error");
        return n;
    }

    /* Silently discard the packet in setup mode */
    if ((connp->id != -1) && (connp->state != CONN_SETUP))
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_redraw(p);
    }

    return 1;
}


static int Receive_rest(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;
    s16b resting;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &resting)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_rest read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (do_cmd_rest(p, resting)) return 2;

        /* If we don't have enough energy to rest, cancel running and queue the command */
        if (p->upkeep->running) cancel_running(p);
        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)resting);
        return 0;
    }

    return 1;
}


static int Receive_ghost(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    s16b ability;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &ability, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_ghost read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_ghost(p, ability, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)ability, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_suicide(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_suicide read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* End character (or retire if winner) */
        do_cmd_suicide(p);

        /* Send any remaining information over the network (the tombstone) */
        Net_output_p(p);

        /* Get rid of him */
        if (!p->total_winner)
            Destroy_connection(p->conn, "Terminated");
        else
            Destroy_connection(p->conn, "Retired");
    }

    return 1;
}


static int Receive_steal(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_steal read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (!cfg_no_steal)
        {
            if (has_energy(p))
            {
                do_cmd_steal(p, dir);
                return 2;
            }

            Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
            return 0;
        }
        else
            /* Handle the option to disable player/player stealing */
            msg(p, "Your pathetic attempts at stealing fail.");
    }

    return 1;
}


/* Receive a dungeon master command */
static int Receive_master(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    char buf[NORMAL_WID];
    s16b command;
    byte ch;

    /*
     * Make sure this came from the dungeon master. Note that it may be
     * possible to spoof this, so probably in the future more advanced
     * authentication schemes will be necessary.
     */
    if ((n = Packet_scanf(&connp->r, "%b%hd%s", &ch, &command, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_master read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        do_cmd_master(p, command, buf);
    }

    return 2;
}


static int Receive_mimic(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    s16b page, spell;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd%c", &ch, &page, &spell, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_mimic read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_mimic(p, page, spell, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%hd%c", (unsigned)ch, (int)page, (int)spell, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_clear(int ind)
{
    byte ch;
    connection_t *connp = get_connection(ind);

    /* Remove the clear command from the queue */
    if (Packet_scanf(&connp->r, "%b", &ch) != 1)
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


static int Receive_observe(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_observe read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_observe(p, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_store_examine(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch, describe;
    int n;
    s16b item;

    if ((n = Packet_scanf(&connp->r, "%b%hd%b", &ch, &item, &describe)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_store_examine read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (in_store(p)) store_examine(p, item, (bool)describe);
    }

    return 1;
}


static int Receive_alter(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_alter read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_alter(p, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_fire_at_nearest(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch, starting;

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &starting)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_fire_at_nearest read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Repeat firing 99 times if fire-till-kill mode is active */
        if (starting)
        {
            if (OPT(p, fire_till_kill)) p->firing_request = 99;
            else p->firing_request = 1;
            starting = 0;
        }

        if (do_cmd_fire_at_nearest(p)) return 2;

        /* If we don't have enough energy to fire, queue the command */
        Packet_printf(&connp->q, "%b%b", (unsigned)ch, (unsigned)starting);
        return 0;
    }

    return 1;
}


static int Receive_jump(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    char dir;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%c", &ch, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_jump read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Disturb if running or resting */
        if (p->upkeep->running || player_is_resting(p))
        {
            disturb(p, 0);
            return 1;
        }

        if (has_energy(p))
        {
            do_cmd_jump(p, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%c", (unsigned)ch, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_social(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    int n;
    char buf[NORMAL_WID];
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%s", &ch, &dir, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_social read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_social(p, buf, dir);
    }

    return 1;
}


static int Receive_monlist(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_monlist read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Perform query */
        do_cmd_monlist(p);
    }

    return 1;
}


static int Receive_feeling(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_feeling read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        p->obj_feeling = -1;
        p->mon_feeling = -1;
        display_feeling(p, false);
        p->upkeep->redraw |= (PR_STATE);
    }

    return 1;
}


static int Receive_interactive(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    char type;
    u32b key;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%lu", &ch, &type, &key)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_interactive read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        do_cmd_interactive(p, type, key);
    }

    return 1;
}


static int Receive_fountain(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_fountain read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_fountain(p, item);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd", (unsigned)ch, (int)item);
        return 0;
    }

    return 1;
}


static int Receive_time(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_time read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        display_time(p);
    }

    return 1;
}


static int Receive_objlist(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_objlist read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Perform query */
        do_cmd_itemlist(p);
    }

    return 1;
}


static int Receive_center(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_center read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_center_map(p);
    }

    return 1;
}


static int Receive_toggle_ignore(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_toggle_ignore read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        p->unignoring = !p->unignoring;
        p->upkeep->notice |= PN_IGNORE;
        do_cmd_redraw(p);
    }

    return 1;
}


static int Receive_use_any(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char dir;
    s16b item;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%c", &ch, &item, &dir)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_use_any read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (has_energy(p))
        {
            do_cmd_use_any(p, item, dir);
            return 2;
        }

        Packet_printf(&connp->q, "%b%hd%c", (unsigned)ch, (int)item, (int)dir);
        return 0;
    }

    return 1;
}


static int Receive_store_order(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    char buf[NORMAL_WID];

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_store_order read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (in_store(p)) store_order(p, buf);
    }

    return 1;
}


static int Receive_track_object(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    s16b item;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &item)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_track_object read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Track the object */
        track_object(p->upkeep, object_from_index(p, item, false, false));
    }

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

    /* Hack -- ensure his settings are allowed, disconnect otherwise */
    if ((cols < Setup.min_col) || (cols > Setup.max_col * tile_wid) ||
        (rows < Setup.min_row) || (rows > Setup.max_row * tile_hgt))
    {
        char buf[255];

        errno = 0;
        strnfmt(buf, sizeof(buf), "Incompatible screen size %dx%d (min %dx%d, max %dx%d).",
            cols, rows, Setup.min_col, Setup.min_row, Setup.max_col * tile_wid,
            Setup.max_row * tile_hgt);
        Destroy_connection(ind, buf);

        return false;
    }

    return true;
}


static void get_birth_options(struct player *p, struct birth_options *options)
{
    options->force_descend = OPT(p, birth_force_descend);
    options->no_recall = OPT(p, birth_no_recall);
    options->no_artifacts = OPT(p, birth_no_artifacts);
    options->feelings = OPT(p, birth_feelings);
    options->no_selling = OPT(p, birth_no_selling);
    options->start_kit = OPT(p, birth_start_kit);
    options->no_stores = OPT(p, birth_no_stores);
    options->no_ghost = OPT(p, birth_no_ghost);
    options->fruit_bat = OPT(p, birth_fruit_bat);
}


static void update_birth_options(struct player *p, struct birth_options *options, bool domsg)
{
    /* Birth options: can only be set at birth */
    if (!ht_zero(&p->game_turn))
    {
        OPT(p, birth_force_descend) = options->force_descend;
        OPT(p, birth_no_recall) = options->no_recall;
        OPT(p, birth_no_artifacts) = options->no_artifacts;
        OPT(p, birth_feelings) = options->feelings;
        OPT(p, birth_no_selling) = options->no_selling;
        OPT(p, birth_start_kit) = options->start_kit;
        OPT(p, birth_no_stores) = options->no_stores;
        OPT(p, birth_no_ghost) = options->no_ghost;
        OPT(p, birth_fruit_bat) = options->fruit_bat;
    }

    /* Server options supercede birth options */
    if (cfg_limit_stairs == 3) OPT(p, birth_force_descend) = true;
    if (cfg_diving_mode == 3) OPT(p, birth_no_recall) = true;
    if (cfg_no_artifacts) OPT(p, birth_no_artifacts) = true;
    if (cfg_no_selling) OPT(p, birth_no_selling) = true;
    if (cfg_no_stores) OPT(p, birth_no_stores) = true;
    if (cfg_no_ghost) OPT(p, birth_no_ghost) = true;

    /* Fruit bat mode: not when a Dragon, a Shapechanger or a Necromancer */
    if (pf_has(p->race->pflags, PF_DRAGON) || pf_has(p->clazz->pflags, PF_MONSTER_SPELLS) ||
        pf_has(p->clazz->pflags, PF_UNDEAD_POWERS))
    {
        OPT(p, birth_fruit_bat) = false;
    }

    /* Fruit bat mode supercedes no-ghost mode */
    if (OPT(p, birth_fruit_bat)) OPT(p, birth_no_ghost) = true;

    /* Update form */
    if (OPT(p, birth_fruit_bat) != options->fruit_bat)
    {
        do_cmd_poly(p, (OPT(p, birth_fruit_bat)? get_race("fruit bat"): NULL), false,
            domsg);
    }
}


static void update_graphics(struct player *p, connection_t *connp)
{
    int i, j;

    /* Desired features */
    for (i = 0; i < z_info->f_max; i++)
    {
        for (j = 0; j < LIGHTING_MAX; j++)
        {
            /* Ignore mimics */
            if (f_info[i].mimic)
            {
                int mimic = lookup_feat(f_info[i].mimic);

                p->f_attr[i][j] = connp->Client_setup.f_attr[mimic][j];
                p->f_char[i][j] = connp->Client_setup.f_char[mimic][j];
            }
            else
            {
                p->f_attr[i][j] = connp->Client_setup.f_attr[i][j];
                p->f_char[i][j] = connp->Client_setup.f_char[i][j];
            }

            /* Default attribute value */
            if (p->f_attr[i][j] == 0xFF) p->f_attr[i][j] = feat_x_attr[i][j];

            if (!(p->f_attr[i][j] && p->f_char[i][j]))
            {
                p->f_attr[i][j] = feat_x_attr[i][j];
                p->f_char[i][j] = feat_x_char[i][j];
            }
        }
    }

    /* Desired traps */
    for (i = 0; i < z_info->trap_max; i++)
    {
        for (j = 0; j < LIGHTING_MAX; j++)
        {
            p->t_attr[i][j] = connp->Client_setup.t_attr[i][j];
            p->t_char[i][j] = connp->Client_setup.t_char[i][j];

            if (!(p->t_attr[i][j] && p->t_char[i][j]))
            {
                p->t_attr[i][j] = trap_x_attr[i][j];
                p->t_char[i][j] = trap_x_char[i][j];
            }
        }
    }

    /* Desired objects */
    for (i = 0; i < z_info->k_max; i++)
    {
        /* Desired aware objects */
        p->k_attr[i] = connp->Client_setup.k_attr[i];
        p->k_char[i] = connp->Client_setup.k_char[i];

        /* Flavored objects */
        if (k_info[i].flavor)
        {
            /* Use flavor attr/char for unaware flavored objects */
            p->d_attr[i] = connp->Client_setup.flvr_x_attr[k_info[i].flavor->fidx];
            p->d_char[i] = connp->Client_setup.flvr_x_char[k_info[i].flavor->fidx];

            /* Use flavor attr/char as default for aware flavored objects */
            if (!(p->k_attr[i] && p->k_char[i]))
            {
                p->k_attr[i] = p->d_attr[i];
                p->k_char[i] = p->d_char[i];
            }

            if (!(p->d_attr[i] && p->d_char[i]))
            {
                p->d_attr[i] = flavor_x_attr[k_info[i].flavor->fidx];
                p->d_char[i] = flavor_x_char[k_info[i].flavor->fidx];
            }
        }

        else
        {
            /* Unflavored objects don't get redefined when aware */
            p->d_attr[i] = connp->Client_setup.k_attr[i];
            p->d_char[i] = connp->Client_setup.k_char[i];

            if (!(p->d_attr[i] && p->d_char[i]))
            {
                p->d_attr[i] = kind_x_attr[i];
                p->d_char[i] = kind_x_char[i];
            }
        }

        /* Default aware objects */
        if (!(p->k_attr[i] && p->k_char[i]))
        {
            p->k_attr[i] = p->d_attr[i];
            p->k_char[i] = p->d_char[i];
        }
    }

    /* Desired monsters */
    for (i = 0; i < z_info->r_max; i++)
    {
        p->r_attr[i] = connp->Client_setup.r_attr[i];
        p->r_char[i] = connp->Client_setup.r_char[i];

        if (!(p->r_attr[i] && p->r_char[i]))
        {
            p->r_attr[i] = monster_x_attr[i];
            p->r_char[i] = monster_x_char[i];
        }
    }

    /* Desired special things */
    for (i = 0; i < PROJ_MAX; i++)
    {
        for (j = 0; j < BOLT_MAX; j++)
        {
            p->proj_attr[i][j] = connp->Client_setup.proj_attr[i][j];
            p->proj_char[i][j] = connp->Client_setup.proj_char[i][j];

            if (!(p->proj_attr[i][j] && p->proj_char[i][j]))
            {
                p->proj_attr[i][j] = proj_to_attr[i][j];
                p->proj_char[i][j] = proj_to_char[i][j];
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
    struct player *p;
    int i;
    char buf[255];
    struct birth_options options;
    s16b roller = connp->stat_roll[STAT_MAX];

    if (NumConnections >= MAX_PLAYERS)
    {
        errno = 0;
        plog_fmt("Too many connections (%d)", NumConnections);
        return -2;
    }

    for (i = 1; i <= NumPlayers; i++)
    {
        if (my_stricmp(player_get(i)->name, connp->nick) == 0)
        {
            errno = 0;
            plog_fmt("Name already in use %s", connp->nick);
            Destroy_connection(ind, "Name already in use");
            return -1;
        }
    }

    /* Hack -- ensure his settings are allowed, disconnect otherwise */
    if (!screen_compatible(ind)) return -1;

    /* Create the character */
    p = player_birth(NumPlayers + 1, connp->account, connp->nick, connp->pass, ind, connp->ridx,
        connp->cidx, connp->psex, connp->stat_roll, connp->options[OPT_birth_start_kit],
        connp->options[OPT_birth_no_recall]);

    /* Failed, connection already destroyed */
    if (!p) return -1;

    my_strcpy(p->full_name, connp->real, sizeof(p->full_name));
    my_strcpy(p->hostname, connp->host, sizeof(p->hostname));
    my_strcpy(p->addr, connp->addr, sizeof(p->addr));
    p->version = connp->version;

    /* Initialize message ptr before we start sending messages */
    p->msg_hist_ptr = 0;

    /* Copy the client preferences to the player struct */
    get_birth_options(p, &options);
    for (i = 0; i < OPT_MAX; i++)
        p->opts.opt[i] = connp->options[i];

    /* Update birth options */
    update_birth_options(p, &options, false);

    /* Reacquire the (modified) birth options and update the client */
    get_birth_options(p, &options);
    Send_birth_options(ind, &options);

    /* Update graphics */
    update_graphics(p, connp);

    /* Hack -- process "settings" */
    p->use_graphics = connp->Client_setup.settings[SETTING_USE_GRAPHICS];
    p->screen_cols = connp->Client_setup.settings[SETTING_SCREEN_COLS];
    p->screen_rows = connp->Client_setup.settings[SETTING_SCREEN_ROWS];
    p->tile_wid = connp->Client_setup.settings[SETTING_TILE_WID];
    p->tile_hgt = connp->Client_setup.settings[SETTING_TILE_HGT];
    p->tile_distorted = connp->Client_setup.settings[SETTING_TILE_DISTORTED];
    p->max_hgt = connp->Client_setup.settings[SETTING_MAX_HGT];
    p->window_flag = connp->Client_setup.settings[SETTING_WINDOW_FLAG];
    p->opts.hitpoint_warn = connp->Client_setup.settings[SETTING_HITPOINT_WARN];

    /*
     * Hack -- when processing a quickstart character, attr/char pair for
     * player picture is incorrect
     */
    if ((roller < 0) && p->use_graphics)
    {
        byte cidx = p->clazz->cidx;
        byte ridx = p->race->ridx;

        p->r_attr[0] = player_presets[p->use_graphics - 1][cidx][ridx][p->psex].a;
        p->r_char[0] = player_presets[p->use_graphics - 1][cidx][ridx][p->psex].c;
    }

    verify_panel(p);

    NumPlayers++;

    connp->id = NumConnections;
    set_player_index(connp, NumPlayers);

    NumConnections++;

    Send_play(ind);

    Conn_set_state(connp, CONN_PLAYING, PLAY_TIMEOUT);

    /* Send party information */
    Send_party(p);

    /* Send channel */
    Send_channel(p, 0, NULL);

    /* Send him his history */
    for (i = 0; i < N_HIST_LINES; i++)
        Send_history(p, i, p->history[i]);

    /* Send him his Various info (age, etc.) */
    Send_various(p, p->ht, p->wt, p->age);

    /* Send initial turn counts */
    Send_turn(p, ht_div(&p->game_turn, cfg_fps), ht_div(&p->player_turn, 1),
        ht_div(&p->active_turn, 1));

    /* Send ignore settings */
    Send_ignore(p);
    Send_aware(p, z_info->k_max);
    Send_everseen(p, z_info->k_max);
    Send_ego_everseen(p, z_info->e_max);

    num_logins++;

    /* Report */
    strnfmt(buf, sizeof(buf), "%s=%s@%s (%s) connected.", p->name, p->full_name, p->hostname,
        p->addr);
    debug(buf);

    /* Tell the new player about server configuration options */
    if (cfg_more_towns)
        msg(p, "Server has static dungeon towns.");
    if (cfg_limit_stairs == 1)
        msg(p, "Server has non-connected stairs.");
    if (cfg_limit_stairs == 2)
        msg(p, "Server is no-up.");
    if (cfg_limit_stairs == 3)
        msg(p, "Server is force-down.");
    if (cfg_diving_mode == 1)
        msg(p, "Server has fast wilderness.");
    if (cfg_diving_mode == 2)
        msg(p, "Server has no wilderness.");
    if (cfg_diving_mode == 3)
        msg(p, "Server is no-recall.");
    if (cfg_no_artifacts)
        msg(p, "Server has no artifacts.");
    if (cfg_no_selling)
        msg(p, "Server is no-selling.");
    if (cfg_no_stores)
        msg(p, "Server has no stores.");
    if (cfg_no_ghost)
        msg(p, "Server is no-ghost.");

    /* Tell the new player about the version number */
    msgt(p, MSG_VERSION, "Server is running version %s", version_build(VB_BASE | VB_BUILD));

    msg(p, "  ");
    msg(p, "   ");
    msg(p, "====================");
    msg(p, "  ");
    msg(p, "   ");

    /* Report delayed info */
    Send_poly(p, (p->poly_race? p->poly_race->ridx: 0));
    p->delayed_display = true;
    p->upkeep->update |= (PU_BONUS | PU_SPELLS | PU_INVEN);
    update_stuff(p, chunk_get(&p->wpos));
    p->delayed_display = false;

    /* Give a level feeling to this player */
    p->obj_feeling = -1;
    p->mon_feeling = -1;
    if (random_level(&p->wpos)) display_feeling(p, false);
    p->upkeep->redraw |= (PR_STATE);

    /*
     * Hack -- when processing a quickstart character, body has changed so we need to
     * resend the equipment indices
     */
    if (roller < 0) p->upkeep->redraw |= (PR_EQUIP);
    redraw_stuff(p);

    /* Handle the cfg_secret_dungeon_master option */
    if (p->dm_flags & DM_SECRET_PRESENCE) return 0;

    /* Tell everyone about our new player */
    if (p->exp == 0)
        strnfmt(buf, sizeof(buf), "%s begins a new game.", p->name);
    else
        strnfmt(buf, sizeof(buf), "%s has entered the game.", p->name);

    msg_broadcast(p, buf, MSG_BROADCAST_ENTER_LEAVE);

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
        if (my_stricmp(current->nick, connp->nick) == 0)
        {
            Destroy_connection(i, "Resume connection");
            return false;
        }
    }

    /* Check all players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Skip current connection */
        if (p->conn == ind) continue;

        /* Check name */
        if (my_stricmp(p->name, connp->nick) == 0)
        {
            /*
             * The following code allows you to "override" an
             * existing connection by connecting again
             */
             Destroy_connection(p->conn, "Resume connection");
             return false;
        }

        /* Only one connection allowed? */
        if (cfg_limit_player_connections && !my_stricmp(p->full_name, connp->real) &&
            !my_stricmp(p->addr, connp->addr) && !my_stricmp(p->hostname, connp->host) &&
            my_stricmp(connp->nick, cfg_dungeon_master) && my_stricmp(p->name, cfg_dungeon_master))
        {
            return true;
        }
    }

    return false;
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
        bool need_info = false;
        byte ridx = 0, cidx = 0, psex = 0;
        int ret;
        int pos = strlen(nick);

        /* Get a character dump */
        if (nick[pos - 1] == '=')
        {
            char dumpname[42];

            nick[pos - 1] = '\0';
            strnfmt(dumpname, sizeof(dumpname), "%s.txt", nick);

            /* Dump the character */
            if (!Send_dump_character(connp, dumpname, 2))
            {
                Destroy_connection(ind, "Character dump failed");
                return -1;
            }
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
            get_next_incarnation(nick, sizeof(nick));
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
            need_info = true;

            /* Check number of characters */
            if (player_id_count(connp->account) >= cfg_max_account_chars)
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
            need_info = true;
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
            if (ret > 0) need_info = true;
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

        Send_basic_info(ind);
        Send_limits_struct_info(ind);
        Send_kind_struct_info(ind);
        Send_ego_struct_info(ind);
        Send_race_struct_info(ind);
        Send_realm_struct_info(ind);
        Send_class_struct_info(ind);
        Send_body_struct_info(ind);
        Send_socials_struct_info(ind);
        Send_hints_struct_info(ind);
        Send_rinfo_struct_info(ind);
        Send_rbinfo_struct_info(ind);
        Send_curse_struct_info(ind);
        Send_feat_struct_info(ind);
        Send_trap_struct_info(ind);
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


static int Receive_quit(int ind)
{
    connection_t *connp = get_connection(ind);
    char ch;

    if (Packet_scanf(&connp->r, "%c", &ch) != 1)
    {
        errno = 0;
        Destroy_connection(ind, "Quit receive error");
        return -1;
    }

    do_quit(ind);

    return 1;
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
    for (i = 0; i <= STAT_MAX; i++)
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


static int sync_settings(struct player *p)
{
    connection_t *connp = get_connection(p->conn);

    /* Resize */
    if ((connp->Client_setup.settings[SETTING_SCREEN_COLS] != p->screen_cols) ||
        (connp->Client_setup.settings[SETTING_SCREEN_ROWS] != p->screen_rows))
    {
        p->screen_cols = connp->Client_setup.settings[SETTING_SCREEN_COLS];
        p->screen_rows = connp->Client_setup.settings[SETTING_SCREEN_ROWS];

        if (!screen_compatible(p->conn))
        {
            errno = 0;
            return -1;
        }

        verify_panel(p);

        /* Redraw map */
        p->upkeep->redraw |= (PR_MAP);
    }

    /* Hack -- process "settings" */
    p->use_graphics = connp->Client_setup.settings[SETTING_USE_GRAPHICS];
    p->tile_wid = connp->Client_setup.settings[SETTING_TILE_WID];
    p->tile_hgt = connp->Client_setup.settings[SETTING_TILE_HGT];
    p->tile_distorted = connp->Client_setup.settings[SETTING_TILE_DISTORTED];
    p->max_hgt = connp->Client_setup.settings[SETTING_MAX_HGT];
    p->window_flag = connp->Client_setup.settings[SETTING_WINDOW_FLAG];
    p->opts.hitpoint_warn = connp->Client_setup.settings[SETTING_HITPOINT_WARN];

    return 1;
}


static int Receive_options(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int i, n;
    byte ch;
    struct birth_options options;
    byte settings;

    if ((n = Packet_scanf(&connp->r, "%b%b", &ch, &settings)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_options read error");
        return n;
    }

    if (settings)
    {
        for (i = 0; i < SETTING_MAX; i++)
        {
            n = Packet_scanf(&connp->r, "%hd", &connp->Client_setup.settings[i]);

            if (n <= 0)
            {
                if (n == -1) Destroy_connection(ind, "Receive_options read error");
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
            if (n == -1) Destroy_connection(ind, "Receive_options read error");
            return n;
        }

        connp->options[i] = (bool)opt;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Save some old values */
        get_birth_options(p, &options);

        /* Set new values */
        for (i = 0; i < OPT_MAX; i++)
            p->opts.opt[i] = connp->options[i];

        /* Update birth options */
        update_birth_options(p, &options, true);

        return sync_settings(p);
    }

    return 1;
}


static int Receive_char_dump(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_char_dump read error");
        return n;
    }

    if (connp->id != -1)
    {
        char dumpname[42];

        p = player_get(get_player_index(connp));

        /* In-game dump */
        player_dump(p, false);
        strnfmt(dumpname, sizeof(dumpname), "%s.txt", p->name);
        Send_dump_character(connp, dumpname, 1);
    }

    return 1;
}


static int Receive_message(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char buf[MSG_LEN];
    int n;
    byte ch;

    buf[0] = '\0';

    if ((n = Packet_scanf(&connp->r, "%b%S", &ch, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_message read error");
        return n;
    }

    p = player_get(get_player_index(connp));

    do_cmd_message(p, buf);

    return 1;
}


static void Handle_item(struct player *p, int item, int curse)
{
    /* Set current value */
    p->current_value = item;

    /* Current spell */
    if (p->current_spell != -1)
    {
        /* Cast current normal spell */
        if (p->current_item >= 0)
        {
            int old_num = get_player_num(p);
            const struct player_class *c = p->clazz;
            quark_t note = (quark_t)p->current_item;
            const struct class_spell *spell;
            bool ident = false;
            struct beam_info beam;
            struct source who_body;
            struct source *who = &who_body;

            /* Hack -- select a single curse for uncursing */
            p->current_action = curse;

            if (p->ghost && !player_can_undead(p)) c = player_id2class(CLASS_GHOST);

            spell = spell_by_index(&c->magic, p->current_spell);
            fill_beam_info(p, p->current_spell, &beam);

            source_player(who, get_player_index(get_connection(p->conn)), p);
            if (!effect_do(spell->effect, who, &ident, true, 0, &beam, 0, note, NULL))
                return;

            cast_spell_end(p);

            /* Take a turn */
            use_energy(p);

            /* Use some mana */
            p->csp -= p->spell_cost;

            /* Hack -- redraw picture */
            redraw_picture(p, old_num);

            /* Redraw mana */
            p->upkeep->redraw |= (PR_MANA);
        }

        /* Cast current projected spell */
        else if (!cast_spell_proj(p, 0 - p->current_item, p->current_spell, true))
            return;
    }

    /* Current item */
    else if (p->current_item != ITEM_REQUEST)
    {
        struct object *obj = object_from_index(p, p->current_item, true, true);
        struct effect *effect;
        bool ident = false, used = false;
        bool notice = false;

        /* Paranoia: requires an item */
        if (!obj) return;

        /* Hack -- select a single curse for uncursing */
        p->current_action = curse;

        /* The player is aware of the object's flavour */
        p->was_aware = object_flavor_is_aware(p, obj);

        /* Figure out effect to use */
        effect = object_effect(obj);

        /* Do effect */
        if (effect)
        {
            if (execute_effect(p, &obj, effect, 0, &ident, &used, &notice)) return;
        }

        /* If the item is a null pointer or has been wiped, be done now */
        if (!obj) return;

        if (notice) object_notice_effect(p, obj);

        /* Analyze the object */
        switch (obj->tval)
        {
            case TV_SCROLL: do_cmd_read_scroll_end(p, obj, ident, used); break;
            case TV_STAFF: do_cmd_use_staff_discharge(p, obj, ident, used); break;
            case TV_ROD: do_cmd_zap_rod_end(p, obj, ident, used); break;
            default: if (obj->activation) do_cmd_activate_end(p, obj, ident, used); break;
        }
    }

    /* Current action */
    else
    {
        switch (p->current_action)
        {
            /* Pickup */
            case ACTION_PICKUP: player_pickup_item(p, chunk_get(&p->wpos), 3, NULL); break;
            case ACTION_GO_DOWN: do_cmd_go_down(p); break;
            default: return;
        }
    }
}


static int Receive_item(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s16b item, curse;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &curse)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_item read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        Handle_item(p, item, curse);
    }

    return 1;
}


static int Receive_sell(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s16b item, amt;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &amt)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_sell read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (in_store(p))
        {
            struct store *store = store_at(p);

            /* Real store */
            if (store->sidx != STORE_HOME)
                do_cmd_sell(p, item, amt);

            /* Player is at home */
            else
            {
                do_cmd_stash(p, item, amt);
                Send_store_sell(p, -1, false);
            }
        }
    }

    return 1;
}


static int Receive_party(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    char buf[NORMAL_WID];
    s16b command;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%hd%s", &ch, &command, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_party read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        do_cmd_party(p, command, buf);
    }

    return 1;
}


static int Receive_special_line(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    char type;
    s16b line;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b%c%hd", &ch, &type, &line)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_special_line read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        switch (type)
        {
            case SPECIAL_FILE_NONE:
                p->special_file_type = SPECIAL_FILE_NONE;
                Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
                free_info_icky(p);
                free_header_icky(p);
                break;
            case SPECIAL_FILE_PLAYER:
                do_cmd_check_players(p, line);
                break;
            case SPECIAL_FILE_OTHER:
                do_cmd_check_other(p, line);
                break;
            case SPECIAL_FILE_POLY:
                do_cmd_check_poly(p, line);
                break; 
            case SPECIAL_FILE_SOCIALS:
                do_cmd_check_socials(p, line);
                break;
            default:
                do_cmd_knowledge(p, type, line);
                break;
        }
    }

    return 1;
}


static int Receive_fullmap(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_fullmap read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        display_fullmap(p);
    }

    return 1;
}


static int Receive_poly(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s16b number;

    if ((n = Packet_scanf(&connp->r, "%b%hd", &ch, &number)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_poly read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Restrict to shapechangers in non-fruitbat mode */
        if (player_has(p, PF_MONSTER_SPELLS) && !OPT(p, birth_fruit_bat))
        {
            /* Check boundaries */
            if ((number < 0) || (number > z_info->r_max - 1))
            {
                msg(p, "This monster race doesn't exist.");
                return 1;
            }

            do_cmd_poly(p, (number? &r_info[number]: NULL), true, true);
        }
    }

    return 1;
}


static int Receive_purchase(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;
    s16b item, amt;

    if ((n = Packet_scanf(&connp->r, "%b%hd%hd", &ch, &item, &amt)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_purchase read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (in_store(p))
        {
            struct store *store = store_at(p);

            /* Attempt to buy it */
            if (store->sidx != STORE_HOME)
                do_cmd_buy(p, item, amt);

            /* Home is much easier */
            else
                do_cmd_retrieve(p, item, amt);

            Packet_printf(&connp->c, "%b", (unsigned)PKT_PURCHASE);
        }
        else
            do_cmd_purchase_house(p, item);
    }

    return 1;
}


static int Receive_store_leave(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n, store_num;
    struct chunk *c;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_store_leave read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));
        c = chunk_get(&p->wpos);

        /* Break mind link */
        break_mind_link(p);

        /* Update the visuals */
        p->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

        /* Redraw */
        p->upkeep->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_SPELL);

        sound(p, MSG_STORE_LEAVE);

        /* Update store info */
        message_flush(p);
        store_num = p->store_num;
        if (store_num != -1)
        {
            p->store_num = -1;

            /* Hack -- don't stand in the way */
            if (store_num != STORE_PLAYER)
            {
                bool look = true;
                int d, i, dis = 1, x = p->py, y = p->px;

                while (look)
                {
                    if (dis > 200) dis = 200;
                    for (i = 0; i < 500; i++)
                    {
                        while (1)
                        {
                            y = rand_spread(p->py, dis);
                            x = rand_spread(p->px, dis);
                            d = distance(p->py, p->px, y, x);
                            if (d <= dis) break;
                        }
                        if (!square_in_bounds_fully(c, y, x)) continue;
                        if (!square_isempty(c, y, x)) continue;
                        if (square_isvault(c, y, x)) continue;
                        look = false;
                        break;
                    }
                    dis = dis * 2;
                }
                monster_swap(c, p->py, p->px, y, x);
                handle_stuff(p);
            }

            /* Reapply illumination */
            cave_illuminate(p, c, is_daytime());
        }

        /* Redraw (remove selling prices) */
        p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
    }

    return 1;
}


static int Receive_store_confirm(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_store_confirm read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        if (in_store(p))
        {
            store_confirm(p);
            Packet_printf(&connp->c, "%b", (unsigned)PKT_STORE_CONFIRM);
        }
        else if (p->current_house != -1)
            do_cmd_purchase_house(p, 0);
        else
            player_pickup_item(p, chunk_get(&p->wpos), 4, NULL);
    }

    return 1;
}


static int Receive_ignore(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int i, j, k, n;
    byte ch;
    byte *new_kind_ignore;
    byte **new_ego_ignore_types;
    byte new_ignore_level[ITYPE_MAX];
    bool ignore = false;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_ignore read error");
        return n;
    }

    /* Flavour-aware ignoring */
    new_kind_ignore = mem_zalloc(z_info->k_max * sizeof(byte));
    for (i = 0; i < z_info->k_max; i++)
    {
        n = Packet_scanf(&connp->r, "%b", &new_kind_ignore[i]);

        if (n <= 0)
        {
            if (n == -1) Destroy_connection(ind, "Receive_ignore read error");
            mem_free(new_kind_ignore);
            return n;
        }
    }

    /* Ego ignoring */
    new_ego_ignore_types = mem_zalloc(z_info->e_max * sizeof(byte*));
    for (i = 0; i < z_info->e_max; i++)
        new_ego_ignore_types[i] = mem_zalloc(ITYPE_MAX * sizeof(byte));
    i = 0;
    j = 0;
    while (i < z_info->e_max)
    {
        s16b repeat;
        byte last;

        n = Packet_scanf(&connp->r, "%hd%b", &repeat, &last);
        if (n <= 0)
        {
            if (n == -1) Destroy_connection(ind, "Receive_ignore read error");
            for (k = 0; k < z_info->e_max; k++) mem_free(new_ego_ignore_types[k]);
            mem_free(new_ego_ignore_types);
            mem_free(new_kind_ignore);
            return n;
        }

        for (k = 0; k < repeat; k++)
        {
            new_ego_ignore_types[i][j] = last;
            j++;
            if (j == ITYPE_MAX)
            {
                j = 0;
                i++;
            }
        }
    }

    /* Quality ignoring */
    for (i = ITYPE_NONE; i < ITYPE_MAX; i++)
    {
        n = Packet_scanf(&connp->r, "%b", &new_ignore_level[i]);

        if (n <= 0)
        {
            if (n == -1) Destroy_connection(ind, "Receive_ignore read error");
            for (k = 0; k < z_info->e_max; k++) mem_free(new_ego_ignore_types[k]);
            mem_free(new_ego_ignore_types);
            mem_free(new_kind_ignore);
            return n;
        }
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Flavour-aware ignoring */
        for (i = 0; i < z_info->k_max; i++)
        {
            if (new_kind_ignore[i]) ignore = true;
            p->kind_ignore[i] = new_kind_ignore[i];
        }

        /* Ego ignoring */
        for (i = 0; i < z_info->e_max; i++)
        {
            for (j = ITYPE_NONE; j < ITYPE_MAX; j++)
            {
                if (new_ego_ignore_types[i][j]) ignore = true;
                p->ego_ignore_types[i][j] = new_ego_ignore_types[i][j];
            }
        }

        /* Quality ignoring */
        for (i = ITYPE_NONE; i < ITYPE_MAX; i++)
        {
            if (new_ignore_level[i] > p->opts.ignore_lvl[i]) ignore = true;
            p->opts.ignore_lvl[i] = new_ignore_level[i];
        }
    }

    /* Notice and redraw as needed */
    if (ignore)
    {
        p->upkeep->notice |= PN_IGNORE;
        do_cmd_redraw(p);
    }

    for (i = 0; i < z_info->e_max; i++) mem_free(new_ego_ignore_types[i]);
    mem_free(new_ego_ignore_types);
    mem_free(new_kind_ignore);
    return 1;
}


static int Receive_flush(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    byte ch;
    int n;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_flush read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        /* Clear */
        p->current_value = 0;
    }

    return 1;
}


static int Receive_channel(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    char buf[NORMAL_WID];
    int n;
    byte ch;

    buf[0] = '\0';

    if ((n = Packet_scanf(&connp->r, "%b%s", &ch, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_channel read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        do_cmd_chat(p, buf);
    }

    return 1;
}


static int Receive_history(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int n;
    byte ch;
    s16b line;
    char buf[NORMAL_WID];

    if ((n = Packet_scanf(&connp->r, "%b%hd%s", &ch, &line, buf)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_history read error");
        return n;
    }

    if (connp->id != -1)
    {
        p = player_get(get_player_index(connp));

        /* Break mind link */
        break_mind_link(p);

        my_strcpy(p->history[line], buf, sizeof(p->history[0]));
    }

    return 1;
}


static int Receive_autoinscriptions(int ind)
{
    connection_t *connp = get_connection(ind);
    int i, n;
    byte ch;

    if ((n = Packet_scanf(&connp->r, "%b", &ch)) <= 0)
    {
        if (n == -1) Destroy_connection(ind, "Receive_autoinscriptions read error");
        return n;
    }

    for (i = 0; i < z_info->k_max; i++)
    {
        n = Packet_scanf(&connp->r, "%s", connp->Client_setup.note_aware[i]);

        if (n <= 0)
        {
            if (n == -1) Destroy_connection(ind, "Receive_autoinscriptions read error");
            return n;
        }
    }

    return 1;
}


/*** General network functions ***/


typedef int (*receive_handler_f)(int);


/* Setup receive methods */
static const receive_handler_f setup_receive[] =
{
    #define PKT(a, b, c, d, e) Receive_##b,
    #include "../common/list-packets.h"
    #undef PKT
    NULL
};


/* Playing receive methods */
static const receive_handler_f playing_receive[] =
{
    #define PKT(a, b, c, d, e) Receive_##c,
    #include "../common/list-packets.h"
    #undef PKT
    NULL
};


/* Actually execute commands from the client command queue */
bool process_pending_commands(int ind)
{
    connection_t *connp = get_connection(ind);
    struct player *p;
    int type, result, old_energy = 0;
    const receive_handler_f *receive_tbl;

    /* Hack to see if we have quit in this function */
    int num_players_start = NumPlayers;

    /* Hack to buffer data */
    int last_pos, data_advance = 0;

    /* Paranoia: ignore input from client if not in SETUP or PLAYING state */
    if ((connp->state != CONN_PLAYING) && (connp->state != CONN_SETUP)) return true;

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
            return true;
        }
        Sockbuf_clear(&connp->q);
    }

    /* If we have no commands to execute return */
    if (connp->r.len <= 0) return false;

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
                return true;
            }
            last_pos = (int)connp->r.ptr;
            type = (connp->r.ptr[0] & 0xFF);

            /* Paranoia */
            if ((type < PKT_UNDEFINED) || (type >= PKT_MAX)) type = PKT_UNDEFINED;

            result = (*receive_tbl[type])(ind);
            data_advance += ((int)connp->r.ptr - last_pos);
            ht_copy(&connp->start, &turn);
            if (result == 0) return true;
            Sockbuf_clear(&connp->q);
            if (result == -1) return true;
        }

        return false;
    }

    /* Get the player pointer */
    p = player_get(get_player_index(connp));

    /*
     * Attempt to execute every pending command. Any command that fails due
     * to lack of energy will be put into the queue for next turn by the
     * respective receive function.
     */
    while ((connp->r.ptr < connp->r.buf + connp->r.len))
    {
        type = (connp->r.ptr[0] & 0xFF);

        /* Paranoia */
        if ((type < PKT_UNDEFINED) || (type >= PKT_MAX)) type = PKT_UNDEFINED;

        /* Cancel repeated commands */
        if ((type != PKT_TUNNEL) && (type != PKT_KEEPALIVE) && p->digging_request)
            p->digging_request = 0;
        if ((type != PKT_FIRE_AT_NEAREST) && (type != PKT_SPELL) && (type != PKT_KEEPALIVE) &&
            p->firing_request)
        {
            p->firing_request = 0;
        }

        result = (*receive_tbl[type])(ind);
        if (connp->state == CONN_PLAYING) ht_copy(&connp->start, &turn);
        if (result == -1) return true;

        /* We didn't have enough energy to execute an important command. */
        if (result == 0)
        {
            /* Hack -- if we tried to do something while resting, wake us up. */
            if ((type != PKT_REST) && player_is_resting(p)) disturb(p, 0);

            /*
             * If we didn't have enough energy to execute this
             * command, in order to ensure that our important
             * commands execute in the proper order, stop
             * processing any commands that require energy. We
             * assume that any commands that don't require energy
             * (such as quitting, or talking) should be executed
             * ASAP.
             *
             * Hack -- save our old energy and set our energy
             * to 0.  This will allow us to execute "out of game"
             * actions such as talking while we wait for enough
             * energy to execute our next queued in game action.
             */
            if (p->energy)
            {
                old_energy = p->energy;
                p->energy = 0;
            }
        }
    }

    /* Restore our energy if necessary. */

    /*
     * Make sure that the player structure hasn't been deallocated in this time due to a quit request.
     * Hack -- to do this we check if the number of players has changed while this loop has been
     * executing. This would be a BAD thing to do if we ever went multithreaded.
     */
    if ((NumPlayers == num_players_start) && !p->energy)
        p->energy = old_energy;

    return false;
}


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
        if (ht_diff(&turn, &connp->start) > (u32b)(connp->timeout * cfg_fps))
        {
            if (connp->state == CONN_QUIT)
                Destroy_connection(i, connp->quit_msg);
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
    struct player *p;

    for (i = 1; i <= NumPlayers; i++)
    {
        p = player_get(i);

        /* Handle "leaving" */
        if (p->upkeep->new_level_method) continue;

        /* Send any information over the network */
        Net_output_p(p);
    }

    /* Every fifteen seconds, update the info sent to the metaserver */
    if (!(turn.turn % (15 * cfg_fps))) Report_to_meta(META_UPDATE);

    return 1;
}


int Net_output_p(struct player *p)
{
    connection_t *connp = get_connection(p->conn);

    /*
     * If we have any data to send to the client, terminate it
     * and send it to the client.
     */
    if (connp->c.len > 0)
    {
        if (Packet_printf(&connp->c, "%b", (unsigned)PKT_END) <= 0)
        {
            Destroy_connection(p->conn, "Net output write error");
            return 1;
        }
        Send_reliable(p->conn);
    }

    return 1;
}


/*
 * HIGHLY EXPERIMENTAL: turn-based mode (for single player games)
 *
 * Return true if turn-based mode can be activated, false otherwise.
 */
bool process_turn_based(void)
{
    struct player *p = player_get(1);
    connection_t *connp = get_connection(p->conn);

    /* Only during PLAYING state */
    if (connp->state != CONN_PLAYING) return false;

    /* Only at the end of each turn */
    if (!has_energy(p)) return false;

    /* Not while resting */
    if (p->upkeep->resting) return false;

    return true;
}
