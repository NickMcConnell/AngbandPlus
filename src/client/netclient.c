/* The client side of the networking stuff */

/* The file is very messy, and probably has a lot of stuff
 * that isn't even used.  Some massive work needs to be done
 * on it, but it's a low priority for me.
 */


#define CLIENT
#include "angband.h"
#include "netclient.h"

static long		last_turns;
static bool		request_redraw;

static sockbuf_t	rbuf, cbuf, wbuf, qbuf;
static int		(*receive_tbl[256])(void),
			(*reliable_tbl[256])(void);
static unsigned		magic;
static long		last_send_anything,
			last_keyboard_change,
			last_keyboard_ack,
			talk_resend,
			reliable_offset,
			reliable_full_len,
			latest_reliable;
static char		talk_pend[1024], initialized = 0;

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
		receive_tbl[i] = NULL;
		reliable_tbl[i] = NULL;
	}

	receive_tbl[PKT_RELIABLE]	= Receive_reliable;
	receive_tbl[PKT_QUIT]		= Receive_quit;
	receive_tbl[PKT_START]		= Receive_start;
	receive_tbl[PKT_END]		= Receive_end;
/*	receive_tbl[PKT_CHAR]		= Receive_char;*/
	

	/*reliable_tbl[PKT_LEAVE]		= Receive_leave;*/
	reliable_tbl[PKT_QUIT]		= Receive_quit;
	reliable_tbl[PKT_STAT]		= Receive_stat;
	reliable_tbl[PKT_HP]		= Receive_hp;
	reliable_tbl[PKT_AC]		= Receive_ac;
	reliable_tbl[PKT_INVEN]		= Receive_inven;
	reliable_tbl[PKT_EQUIP]		= Receive_equip;
	reliable_tbl[PKT_CHAR_INFO]	= Receive_char_info;
	reliable_tbl[PKT_VARIOUS]	= Receive_various;
	reliable_tbl[PKT_PLUSSES]	= Receive_plusses;
	reliable_tbl[PKT_EXPERIENCE]	= Receive_experience;
	reliable_tbl[PKT_GOLD]		= Receive_gold;
	reliable_tbl[PKT_SP]		= Receive_sp;
	reliable_tbl[PKT_HISTORY]	= Receive_history;
	reliable_tbl[PKT_CHAR]		= Receive_char;
	reliable_tbl[PKT_MESSAGE]	= Receive_message;
	reliable_tbl[PKT_STATE]		= Receive_state;
	reliable_tbl[PKT_TITLE]		= Receive_title;
	reliable_tbl[PKT_DEPTH]		= Receive_depth;
	reliable_tbl[PKT_CONFUSED]	= Receive_confused;
	reliable_tbl[PKT_POISON]	= Receive_poison;
	reliable_tbl[PKT_STUDY]		= Receive_study;
	reliable_tbl[PKT_FOOD]		= Receive_food;
	reliable_tbl[PKT_FEAR]		= Receive_fear;
	reliable_tbl[PKT_SPEED]		= Receive_speed;
	reliable_tbl[PKT_CUT]		= Receive_cut;
	reliable_tbl[PKT_BLIND]		= Receive_blind;
	reliable_tbl[PKT_STUN]		= Receive_stun;
	reliable_tbl[PKT_ITEM]		= Receive_item;
	reliable_tbl[PKT_SPELL_INFO]	= Receive_spell_info;
	reliable_tbl[PKT_DIRECTION]	= Receive_direction;
	reliable_tbl[PKT_FLUSH]		= Receive_flush;
	reliable_tbl[PKT_LINE_INFO]	= Receive_line_info;
	reliable_tbl[PKT_SPECIAL_OTHER]	= Receive_special_other;
	reliable_tbl[PKT_STORE]		= Receive_store;
	reliable_tbl[PKT_STORE_INFO]	= Receive_store_info;
	reliable_tbl[PKT_SELL]		= Receive_sell;
	reliable_tbl[PKT_TARGET_INFO]	= Receive_target_info;
	reliable_tbl[PKT_SOUND]		= Receive_sound;
	reliable_tbl[PKT_MINI_MAP]	= Receive_line_info;
	/* reliable_tbl[PKT_MINI_MAP]	= Receive_mini_map; */
	reliable_tbl[PKT_SPECIAL_LINE]	= Receive_special_line;
	reliable_tbl[PKT_FLOOR]		= Receive_floor;
	reliable_tbl[PKT_PICKUP_CHECK]	= Receive_pickup_check;
	reliable_tbl[PKT_PARTY]		= Receive_party;
	reliable_tbl[PKT_SKILLS]	= Receive_skills;
	reliable_tbl[PKT_PAUSE]		= Receive_pause;
	reliable_tbl[PKT_MONSTER_HEALTH]= Receive_monster_health;
}

int Net_setup(void)
{
	int n, len, done = 0, retries;
	long todo = sizeof(setup_t);
	char *ptr;

	ptr = (char *) &Setup;

	while (todo > 0)
	{
		if (cbuf.ptr != cbuf.buf)
			Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);

		len = cbuf.len;
		if (len > todo)
			len = todo;

		if (len > 0)
		{
			if (done == 0)
			{
				n = Packet_scanf(&cbuf, "%ld%hd",
					&Setup.motd_len, &Setup.frames_per_second);
				if (n <= 0)
				{
					errno = 0;
					quit("Can't read setup info from reliable data buffer");
				}
				ptr = (char *) &Setup;
				done = (char *) &Setup.motd[0] - ptr;
				todo = Setup.motd_len;
			}
			else
			{
				memcpy(&ptr[done], cbuf.ptr, len);
				Sockbuf_advance(&cbuf, len + cbuf.ptr - cbuf.buf);
				done += len;
				todo -= len;
			}
		}
		if (todo > 0)
		{
			if (rbuf.ptr != rbuf.buf)
				Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);

			if (rbuf.len > 0)
			{
				if (rbuf.ptr[0] != PKT_RELIABLE)
				{
					if (rbuf.ptr[0] == PKT_QUIT)
					{
						quit("Server closed connection");
					}
					else
					{
						errno = 0;
						quit_fmt("Not a reliable packet (%d) in setup", rbuf.ptr[0]);
					}
				}
				if (Receive_reliable() == -1)
					return -1;
				if (Sockbuf_flush(&wbuf) == -1)
					return -1;
			}

			if (cbuf.ptr != cbuf.buf)
				Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);

			if (cbuf.len > 0)
				continue;

			for (retries = 0;;retries++)
			{
				if (retries >= 10)
				{
					errno = 0;
					quit_fmt("Can't read setup after %d retries "
						 "(todo=%d, left=%d)",
						 retries, todo, cbuf.len - (cbuf.ptr - cbuf.buf));
				}

				SetTimeout(2, 0);
				while (SocketReadable(rbuf.sock) > 0)
				{
					Sockbuf_clear(&rbuf);
					if (Sockbuf_read(&rbuf) == -1)
						quit("Can't read all of setup data");
					if (rbuf.len > 0)
						break;
					SetTimeout(0, 0);
				}
				if (rbuf.len > 0)
					break;
			}
		}
	}

	return 0;
}

/*
 * Send the first packet to the server with our name,
 * nick and display contained in it.
 * The server uses this data to verify that the packet
 * is from the right UDP connection, it already has
 * this info from the ENTER_GAME_pack.
 */
int Net_verify(char *real, char *nick, char *pass, int sex, int race, int class)
{
	int	i, n,
		type,
		result,
		retries;
	time_t	last;

	for (retries = 0;;retries++)
	{
		if (retries == 0 || time(NULL) - last >= 3)
		{
			if (retries++ >= 100) // wait for a VERY LONG time
			{
				errno = 0;
				plog(format("Can't connect to server after %d retries",
					retries));
				return -1;
			}
			Sockbuf_clear(&wbuf);
			n = Packet_printf(&wbuf, "%c%s%s%s%hd%hd%hd", PKT_VERIFY, real, nick, pass, sex, race, class);


			/* Send the desired stat order */
			for (i = 0; i < 6; i++)
			{
				Packet_printf(&wbuf, "%hd", stat_order[i]);
			}

			/* Send the options */
			for (i = 0; i < 64; i++)
			{
				Packet_printf(&wbuf, "%c", Client_setup.options[i]);
			}

#ifndef BREAK_GRAPHICS
			/* Send the "unknown" redefinitions */
			for (i = 0; i < TV_MAX; i++)
			{
				Packet_printf(&wbuf, "%c%c", Client_setup.u_attr[i], Client_setup.u_char[i]);
			}

			/* Send the "feature" redefinitions */
			for (i = 0; i < MAX_F_IDX; i++)
			{
				Packet_printf(&wbuf, "%c%c", Client_setup.f_attr[i], Client_setup.f_char[i]);
			}

			/* Send the "object" redefinitions */
			for (i = 0; i < MAX_K_IDX; i++)
			{
				Packet_printf(&wbuf, "%c%c", Client_setup.k_attr[i], Client_setup.k_char[i]);
			}

			/* Send the "monster" redefinitions */
			for (i = 0; i < MAX_R_IDX; i++)
			{
				Packet_printf(&wbuf, "%c%c", Client_setup.r_attr[i], Client_setup.r_char[i]);
			}
#endif

			if (n <= 0 || Sockbuf_flush(&wbuf) <= 0)
			{
				plog("Can't send verify packet");
				//return -1;
			}
			time(&last);

			if (retries > 1)
				printf("Waiting for verify response\n");
		}
		SetTimeout(1, 0);
		if (SocketReadable(rbuf.sock) == 0)
			continue;
		Sockbuf_clear(&rbuf);
		if (Sockbuf_read(&rbuf) == -1)
		{
			plog("Can't read verify reply packet");
			return -1;
		}
		if (rbuf.len <= 0)
			continue;
		if (rbuf.ptr[0] != PKT_RELIABLE)
		{
			if (rbuf.ptr[0] == PKT_QUIT)
			{
				errno = 0;
				plog("Server closed connection");	
				plog(&rbuf.ptr[1]);
				return -1;
			}
			else
			{
				errno = 0;
				plog(format("Bad packet type when verifying (%d)",
					rbuf.ptr[0]));
				return -1;
			}
		}
		if (Receive_reliable() == -1)
			return -1;
		if (Sockbuf_flush(&wbuf) == -1)
			return -1;
		if (cbuf.len == 0)
			continue;
		if (Receive_reply(&type, &result) <= 0)
		{
			errno = 0;	
			plog("Can't receive verify reply packet");
			return -1;
		}
		if (type != PKT_VERIFY)
		{
			errno = 0;
			plog(format("Verify wrong reply type (%d)", type));
			return -1;
		}
		if (result != PKT_SUCCESS)
		{
			errno = 0;
			plog(format("Verification failed (%d)", result));
			return -1;
		}
		if (Receive_magic() <= 0)
		{
			plog("Can't receive magic packet after verify");
			return -1;
		}
		break;
	}

	if (retries > 1)
		printf("Verifies correctly\n");

	return 0;
}


/*
 * Open the datagram socket and allocate the network data
 * structures like buffers.
 * Currently there are three different buffers used:
 * 1) wbuf is used only for sending packets (write/printf).
 * 2) rbuf is used for receiving packets in (read/scanf).
 * 3) cbuf is used to copy the reliable data stream
 *    into from the raw and unreliable rbuf packets.
 */
int Net_init(char *server, int port)
{
	int		 sock;

	/*signal(SIGPIPE, SIG_IGN);*/

	Receive_init();

	if ((sock = CreateDgramSocket(0)) == -1)
	{
		plog("Can't create datagram socket");
		return -1;
	}
	if (DgramConnect(sock, server, port) == -1)
	{
		plog(format("Can't connect to server %s on port %d", server, port));
		DgramClose(sock);
		return -1;
	}
	wbuf.sock = sock;
	if (SetSocketNonBlocking(sock, 1) == -1)
	{
		plog("Can't make socket non-blocking");
		return -1;
	}
	if (SetSocketSendBufferSize(sock, CLIENT_SEND_SIZE + 256) == -1)
		plog(format("Can't set send buffer size to %d: error %d", CLIENT_SEND_SIZE + 256, errno));
	if (SetSocketReceiveBufferSize(sock, CLIENT_RECV_SIZE + 256) == -1)
		plog(format("Can't set receive buffer size to %d", CLIENT_RECV_SIZE + 256));


	/* reliable data buffer, not a valid socket filedescriptor needed */
	if (Sockbuf_init(&cbuf, -1, CLIENT_RECV_SIZE,
		SOCKBUF_WRITE | SOCKBUF_READ | SOCKBUF_LOCK) == -1)
	{
		plog(format("No memory for control buffer (%u)", CLIENT_RECV_SIZE));
		return -1;
	}

	/* queue buffer, not a valid socket filedescriptor needed */
	if (Sockbuf_init(&qbuf, -1, CLIENT_RECV_SIZE,
		SOCKBUF_WRITE | SOCKBUF_READ | SOCKBUF_LOCK) == -1)
	{
		plog(format("No memory for queue buffer (%u)", CLIENT_RECV_SIZE));
		return -1;
	}

	/* read buffer */
	if (Sockbuf_init(&rbuf, sock, CLIENT_RECV_SIZE,
		SOCKBUF_READ | SOCKBUF_DGRAM) == -1)
	{
		plog(format("No memory for read buffer (%u)", CLIENT_RECV_SIZE));
		return -1;
	}

	/* write buffer */
	if (Sockbuf_init(&wbuf, sock, CLIENT_SEND_SIZE,
		SOCKBUF_WRITE | SOCKBUF_DGRAM) == -1)
	{
		plog(format("No memory for write buffer (%u)", CLIENT_SEND_SIZE));
		return -1;
	}

	/* reliable data byte stream offset */
	reliable_offset = 0;

	/* Initialized */
	initialized = 1;

	return 0;
}


/*
 * Cleanup all the network buffers and close the datagram socket.
 * Also try to send the server a quit packet if possible.
 * Because this quit packet may get lost we send one at the
 * beginning and one at the end.
 */
void Net_cleanup(void)
{
	int	sock = wbuf.sock;
	char	ch;

	if (sock > 2)
	{
		ch = PKT_QUIT;
		if (DgramWrite(sock, &ch, 1) != 1)
		{
			GetSocketError(sock);
			DgramWrite(sock, &ch, 1);
		}
		Term_xtra(TERM_XTRA_DELAY, 50);
	}

	Sockbuf_cleanup(&rbuf);
	Sockbuf_cleanup(&cbuf);
	Sockbuf_cleanup(&wbuf);
	Sockbuf_cleanup(&qbuf);

	if (sock > 2)
	{
		ch = PKT_QUIT;
		if (DgramWrite(sock, &ch, 1) != 1)
		{
			GetSocketError(sock);
			DgramWrite(sock, &ch, 1);
		}
		Term_xtra(TERM_XTRA_DELAY, 50);
		if (DgramWrite(sock, &ch, 1) != 1)
		{
			GetSocketError(sock);
			DgramWrite(sock, &ch, 1);
		}
		DgramClose(sock);
	}
}


/*
 * Flush the network output buffer if it has some data in it.
 * Called by the main loop before blocking on a select(2) call.
 */
int Net_flush(void)
{
	if (talk_resend < last_turns)
		Send_msg(NULL);
	if (wbuf.len == 0)
	{
		wbuf.ptr = wbuf.buf;
		return 0;
	}
	if (Sockbuf_flush(&wbuf) == -1)
		return -1;
	Sockbuf_clear(&wbuf);
	last_send_anything = last_turns;
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
 * Try to send a `start play' packet to the server and get an
 * acknowledgement from the server.  This is called after
 * we have initialized all our other stuff like the user interface
 * and we also have the map already.
 */
int Net_start(void)
{
	int		retries,
			type,
			result;
	time_t		last;

	for (retries = 0;;)
	{
		if (retries == 0 || (time(NULL) - last) > 1)
		{
			if (retries++ >= 10)
			{
				errno = 0;
				quit(format("Can't start play after %d retries",
					retries));
				return -1;
			}
			Sockbuf_clear(&wbuf);
			if (Packet_printf(&wbuf, "%c", PKT_PLAY) <= 0
				|| Sockbuf_flush(&wbuf) == -1)
			{
				quit("Can't send start play packet");
				return -1;
			}
			time(&last);
		}
		if (cbuf.ptr > cbuf.buf)
			Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);
		SetTimeout(2, 0);
		while (cbuf.len <= 0 && SocketReadable(rbuf.sock) != 0)
		{
			Sockbuf_clear(&rbuf);
			if (Sockbuf_read(&rbuf) == -1)
			{
				quit("Error reading play reply");
				return -1;
			}

			if (rbuf.len <= 0)
				continue;
			if (rbuf.ptr[0] != PKT_RELIABLE)
			{
				if (rbuf.ptr[0] == PKT_QUIT)
				{
					errno = 0;
					quit(&rbuf.ptr[1]);
					return -1;
				}
				else if (rbuf.ptr[0] == PKT_END)
				{
					Sockbuf_clear(&rbuf);
					continue;
				}
				else
				{
					printf("strange packet type while starting (%d)\n", rbuf.ptr[0]);
					Sockbuf_clear(&rbuf);
					continue;
				}
			}
			if (Receive_reliable() == -1)
				return -1;
			if (Sockbuf_flush(&wbuf) == -1)
				return -1;
		}
		if (cbuf.ptr - cbuf.buf >= cbuf.len)
		{
			continue;
		}

		if (cbuf.ptr[0] != PKT_REPLY)
		{
			errno = 0;
			quit(format("Not a reply packet after play (%d,%d,%d)",
				cbuf.ptr[0], cbuf.ptr - cbuf.buf, cbuf.len));
			return -1;
		}
		if (Receive_reply(&type, &result) <= 0)
		{
			errno = 0;
			quit("Can't receive reply packet after play");
			return -1;
		}
		if (type != PKT_PLAY)
		{
			errno = 0;
			quit("Can't receive reply packet after play");
			return -1;
		}
		if (result != PKT_SUCCESS)
		{
			errno = 0;
			quit(format("Start play not allowed (%d)", result));
			return -1;
		}
		break;
	}
	errno = 0;
	return 0;
}


/*
 * Process a packet which most likely is a frame update,
 * perhaps with some reliable data in it.
 */
static int Net_packet(void)
{
	int		type,
			prev_type = 0,
			result,
			replyto,
			status;

	while (rbuf.buf + rbuf.len > rbuf.ptr)
	{
		type = (*rbuf.ptr & 0xFF);
		if (receive_tbl[type] == NULL)
		{
			errno = 0;
			plog(format("Received unknown packet type (%d, %d), dropping",
				type, prev_type));
			Sockbuf_clear(&rbuf);
			break;
		}
		else if ((result = (*receive_tbl[type])()) <= 0)
		{
			if (result == -1)
			{
				if (type != PKT_QUIT)
				{
					errno = 0;
					plog(format("Processing packet type (%d, %d) failed", type, prev_type));
				}
				return -1;
			}
			Sockbuf_clear(&rbuf);
			break;
		}
		prev_type = type;
	}
	while (cbuf.buf + cbuf.len > cbuf.ptr && cbuf.len >= reliable_full_len)
	{
		/* Reset full length */
		reliable_full_len = 0;

		type = (*cbuf.ptr & 0xFF);
		if (type == PKT_REPLY)
		{
			if ((result = Receive_reply(&replyto, &status)) <= 0)
			{
				if (result == 0)
					break;
				return -1;
			}
			errno = 0;
			plog(format("Got reply packet (%d, %d)", replyto, status));
		}
		else if (reliable_tbl[type] == NULL)
		{
			int i;

			errno = 0;
			 //plog(format("Receive unknown reliable data packet type (%d, %d, %d)",
			//	type, cbuf.ptr - cbuf.buf, cbuf.len)); 

			 /* we have received bad data, ignore this packet */
			Sockbuf_clear(&cbuf);

			break;
		}
		else if ((result = (*reliable_tbl[type])()) <= 0)
		{
			if (result == 0)
				break;
			return -1;
		}
	}
	return 0;
}


/*
 * Read a packet into one of the input buffers.
 * If it is a frame update then we check to see
 * if it is an old or duplicate one.  If it isn't
 * a new frame then the packet is discarded and
 * we retry to read a packet once more.
 * It's a non-blocking read.
 */
static int Net_read(void)
{
	int	n;

	for (;;)
	{
		/* Wait for data to appear -- Dave Thaler */
		while (!SocketReadable(Net_fd()));

		if ((n = Sockbuf_read(&rbuf)) == -1)
		{
			plog("Net input error");
			return -1;
		}
		if (rbuf.len <= 0)
		{
			Sockbuf_clear(&rbuf);
			return 0;
		}

		/* If this is the end of a chunk of info, quit reading stuff */
		if (rbuf.ptr[rbuf.len - 1] == PKT_END)
			return 1;
	}
}


/*
 * Read frames from the net until there are no more available.
 * If the server has floaded us with frame updates then we should
 * discard everything except the most recent ones.  The X server
 * may be too slow to keep up with the rate of the XPilot server
 * or there may have been a network hickup if the net is overloaded.
 */
int Net_input(void)
{
	int	n;

	/* First, clear the buffer */
	Sockbuf_clear(&rbuf);

	if ((n = Net_read()) <= 0)
	{
		return n;
	}

	n = Net_packet();

	Sockbuf_clear(&rbuf);

	if (n == -1)
		return -1;

	return 1;
}

int Flush_queue(void)
{
	short	len;

	if (!initialized) return 0;

	len = qbuf.len;

	Net_packet();

	if (cbuf.ptr > cbuf.buf)
		Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);
	if (Sockbuf_write(&cbuf, qbuf.ptr, len) != len)
	{
		errno = 0;
		plog("Can't copy queued data to buffer");
		qbuf.ptr += len;
		Sockbuf_advance(&qbuf, qbuf.ptr - qbuf.buf);
		return -1;
	}
/*	reliable_offset += len; */
	qbuf.ptr += len;
	Sockbuf_advance(&qbuf, qbuf.ptr - qbuf.buf);

	Sockbuf_clear(&qbuf);

	/* If a redraw has been requested, send the request */
	if (request_redraw)
	{
		Send_redraw();
		request_redraw = FALSE;
	}

	return 1;
}

/*
 * Receive the beginning of a new frame update packet,
 * which contains the loops number.
 */
int Receive_start(void)
{
	int	n;
	long	loops;
	byte	ch;
	long	key_ack;

	if ((n = Packet_scanf(&rbuf, "%c%ld%ld", &ch, &loops, &key_ack)) <= 0)
			return n;

	if (last_turns >= loops)
	{
		printf("ignoring frame (%ld)\n", last_turns - loops);
		return 0;
	}
	last_turns = loops;
	if (key_ack > last_keyboard_ack)
	{
		if (key_ack > last_keyboard_change)
		{
			printf("Premature keyboard ack by server (%ld,%ld,%ld)\n",
				last_keyboard_change, last_keyboard_ack, key_ack);
			return 0;
		}
		else last_keyboard_ack = key_ack;
	}
#if 0
	if ((n = Handle_start(loops)) == -1)
		return -1;
#endif

	return 1;
}

/*
 * Receive the end of a new frame update packet,
 * which should contain the same loops number
 * as the frame head.  If this terminating packet
 * is missing then the packet is corrupt or incomplete.
 */
int Receive_end(void)
{
	int	n;
	byte	ch;

	if ((n = Packet_scanf(&rbuf, "%c", &ch)) <= 0)
		return n;

#if 0
	if ((n = Handle_end(loops)) == -1)
		return -1;
#endif

	return 1;
}


int Receive_magic(void)
{
	int	n;
	byte	ch;

	if ((n = Packet_scanf(&cbuf, "%c%u", &ch, &magic)) <= 0)
		return n;
	return 1;
}

int Send_ack(long rel_loops)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%ld%ld", PKT_ACK,
		reliable_offset, rel_loops)) <= 0)
	{
		if (n == 0)
			return 0;
		plog("Can't ack reliable data");
		return -1;
	}

	return 1;
}

int Receive_reliable(void)
{
	int	n;
	short	len;
	byte	ch;
	long	rel, rel_loops, full_len;

	if ((n = Packet_scanf(&rbuf, "%c%hd%ld%ld%ld", &ch, &len, &rel, &rel_loops, &full_len)) == -1)
		return -1;

	if (n == 0)
	{
		errno = 0;
		plog("Incomplete reliable data packet");
		return 0;
	}

	if (len <= 0)
	{
		errno = 0;
		plog(format("Bad reliable data length (%d)", len));
		return -1;
	}

	if (full_len <= 0)
	{
		errno = 0;
		plog(format("Bad reliable data full length (%d)", reliable_full_len));
		return -1;
	}

	/* Track maximum full length */
	if (full_len > reliable_full_len || rel_loops > latest_reliable)
	{
		reliable_full_len = full_len;
		latest_reliable = rel_loops;
		last_turns = rel_loops;
	}

	if (rbuf.ptr + len > rbuf.buf + rbuf.len)
	{
		errno = 0;
		plog(format("Not all reliable data in packet (%d, %d, %d)",
			rbuf.ptr - rbuf.buf, len, rbuf.len));
		rbuf.ptr += len;
		Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
		return -1;
	}
	if (rel > reliable_offset)
	{
		rbuf.ptr += len;
		Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
		if (Send_ack(rel_loops) == -1)
			return -1;
		return 1;
	}
	if (rel + len <= reliable_offset)
	{
		rbuf.ptr += len;
		Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
		if (Send_ack(rel_loops) == -1)
			return -1;
		return 1;
	}
	if (rel < reliable_offset)
	{
		len -= reliable_offset - rel;
		rbuf.ptr += reliable_offset - rel;
		rel = reliable_offset;
	}
	if (cbuf.ptr > cbuf.buf)
		Sockbuf_advance(&cbuf, cbuf.ptr - cbuf.buf);
	if (Sockbuf_write(&cbuf, rbuf.ptr, len) != len)
	{
		errno = 0;
		plog("Can't copy reliable data to buffer");
		rbuf.ptr += len;
		Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
		return -1;
	}
	reliable_offset += len;
	rbuf.ptr += len;
	Sockbuf_advance(&rbuf, rbuf.ptr - rbuf.buf);
	if (Send_ack(rel_loops) == -1)
		return -1;
	return 1;
}

int Receive_reply(int *replyto, int *result)
{
	int	n;
	byte	type, ch1, ch2;

	n = Packet_scanf(&cbuf, "%c%c%c", &type, &ch1, &ch2);
	if (n <= 0)
		return n;
	if (n != 3 || type != PKT_REPLY)
	{
		plog("Can't receive reply packet");
		return 1;
	}
	*replyto = ch1;
	*result = ch2;
	return 1;
}

int Receive_quit(void)
{
	unsigned char		pkt;
	sockbuf_t		*sbuf;
	char			reason[MAX_CHARS];

	if (rbuf.ptr < rbuf.buf + rbuf.len)
		sbuf = &rbuf;
	else sbuf = &cbuf;
	if (Packet_scanf(sbuf, "%c", &pkt) != 1)
	{
		errno = 0;
		plog("Can't read quit packet");
	}
	else
	{
		if (Packet_scanf(sbuf, "%s", reason) <= 0)
			strcpy(reason, "unknown reason");
		errno = 0;
		quit(format("Quitting: %s", reason));
	}
	return -1;
}

int Receive_stat(void)
{
	int	n;
	char	ch;
	char	stat;
	s16b	max, cur;

	if ((n = Packet_scanf(&cbuf, "%c%c%hd%hd", &ch, &stat, &max, &cur)) <= 0)
	{
		return n;
	}

	p_ptr->stat_top[(int) stat] = max;
	p_ptr->stat_use[(int) stat] = cur;

	if (!screen_icky && !shopping)
		prt_stat(stat, max, cur);
	else
		if ((n = Packet_printf(&qbuf, "%c%c%hd%hd", ch, stat, max, cur)) <= 0)
		{
			return n;
		}


	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_hp(void)
{
	int	n;
	char 	ch;
	s16b	max, cur;

	if ((n = Packet_scanf(&cbuf, "%c%hd%hd", &ch, &max, &cur)) <= 0)
	{
		return n;
	}

	p_ptr->mhp = max;
	p_ptr->chp = cur;

	if (!screen_icky && !shopping)
		prt_hp(max, cur);
	else
		if ((n = Packet_printf(&qbuf, "%c%hd%hd", ch, max, cur)) <= 0)
		{
			return n;
		}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_ac(void)
{
	int	n;
	char	ch;
	s16b	base, plus;

	if ((n = Packet_scanf(&cbuf, "%c%hd%hd", &ch, &base, &plus)) <= 0)
	{
		return n;
	}

	p_ptr->dis_ac = base;
	p_ptr->dis_to_a = plus;

	if (!screen_icky && !shopping)
		prt_ac(base + plus);
	else
		if ((n = Packet_printf(&qbuf, "%c%hd%hd", ch, base, plus)) <= 0)
		{
			return n;
		}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_inven(void)
{
	int	n;
	char	ch;
	char pos, attr, tval;
	s16b wgt, amt;
	char name[80];

	if ((n = Packet_scanf(&cbuf, "%c%c%c%hu%hd%c%s", &ch, &pos, &attr, &wgt, &amt, &tval, name)) <= 0)
	{
		return n;
	}

	/* Hack -- The color is stored in the sval, since we don't use it for anything else */
	inventory[pos - 'a'].sval = attr;
	inventory[pos - 'a'].tval = tval;
	inventory[pos - 'a'].weight = wgt;
	inventory[pos - 'a'].number = amt;

	strncpy(inventory_name[pos - 'a'], name, 79);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	return 1;
}

int Receive_equip(void)
{
	int	n;
	char 	ch;
	char pos, attr, tval;
	s16b wgt;
	char name[80];

	if ((n = Packet_scanf(&cbuf, "%c%c%c%hu%c%s", &ch, &pos, &attr, &wgt, &tval, name)) <= 0)
	{
		return n;
	}

	inventory[pos - 'a' + INVEN_WIELD].sval = attr;
	inventory[pos - 'a' + INVEN_WIELD].tval = tval;
	inventory[pos - 'a' + INVEN_WIELD].weight = wgt;
	inventory[pos - 'a' + INVEN_WIELD].number = 1;


	strncpy(inventory_name[pos - 'a' + INVEN_WIELD], name, 79);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);

	return 1;
}

int Receive_char_info(void)
{
	int	n;
	char	ch;
	static bool pref_files_loaded = FALSE;

	/* Clear any old info */
	race = class = sex = 0;

	if ((n = Packet_scanf(&cbuf, "%c%hd%hd%hd", &ch, &race, &class, &sex)) <= 0)
	{
		return n;
	}

	p_ptr->prace = race;
	p_ptr->pclass = class;
	p_ptr->male = sex;

	/* Mega-hack -- Read pref files if we haven't already */
	if (!pref_files_loaded)
	{
		/* Load */
		initialize_all_pref_files();

		/* Pref files are now loaded */
		pref_files_loaded = TRUE;
	}

	if (!screen_icky && !shopping)
		prt_basic();
	else
		if ((n = Packet_printf(&qbuf, "%c%hd%hd%hd", ch, race, class, sex)) <= 0)
		{
			return n;
		}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_various(void)
{
	int	n;
	char	ch;
	s16b	hgt, wgt, age, sc;

	if ((n = Packet_scanf(&cbuf, "%c%hu%hu%hu%hu", &ch, &hgt, &wgt, &age, &sc)) <= 0)
	{
		return n;
	}

	p_ptr->ht = hgt;
	p_ptr->wt = wgt;
	p_ptr->age = age;
	p_ptr->sc = sc;

	/*printf("Received various info: height %d, weight %d, age %d, sc %d\n", hgt, wgt, age, sc);*/

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_plusses(void)
{
	int	n;
	char	ch;
	s16b	dam, hit;

	if ((n = Packet_scanf(&cbuf, "%c%hd%hd", &ch, &hit, &dam)) <= 0)
	{
		return n;
	}

	p_ptr->dis_to_h = hit;
	p_ptr->dis_to_d = dam;

	/*printf("Received plusses: +%d tohit +%d todam\n", hit, dam);*/

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_experience(void)
{
	int	n;
	char	ch;
	int	max, cur, adv;
	s16b	lev;

	if ((n = Packet_scanf(&cbuf, "%c%hu%d%d%d", &ch, &lev, &max, &cur, &adv)) <= 0)
	{
		return n;
	}

	p_ptr->lev = lev;
	p_ptr->max_exp = max;
	p_ptr->exp = cur;
	exp_adv = adv;

	if (!screen_icky && !shopping)
		prt_level(lev, max, cur, adv);
	else
		if ((n = Packet_printf(&qbuf, "%c%hu%d%d%d", ch, lev, max, cur, adv)) <= 0)
		{
			return n;
		}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_gold(void)
{
	int	n;
	char	ch;
	int	gold;

	if ((n = Packet_scanf(&cbuf, "%c%d", &ch, &gold)) <= 0)
	{
		return n;
	}

	p_ptr->au = gold;

	if (shopping)
	{
	        char out_val[64];

	        prt("Gold Remaining: ", 19, 53);

	        sprintf(out_val, "%9ld", (long) gold);
	        prt(out_val, 19, 68);
	}
	else if (!screen_icky)
		prt_gold(gold);
	else
		if ((n = Packet_printf(&qbuf, "%c%d", ch, gold)) <= 0)
		{
			return n;
		}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_sp(void)
{
	int	n;
	char	ch;
	s16b	max, cur;

	if ((n = Packet_scanf(&cbuf, "%c%hd%hd", &ch, &max, &cur)) <= 0)
	{
		return n;
	}

	p_ptr->msp = max;
	p_ptr->csp = cur;

	if (!screen_icky && !shopping)
		prt_sp(max, cur);
	else
		if ((n = Packet_printf(&qbuf, "%c%hd%hd", ch, max, cur)) <= 0)
		{
			return n;
		}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_history(void)
{
	int	n;
	char	ch;
	s16b	line;
	char	buf[80];

	if ((n = Packet_scanf(&cbuf, "%c%hu%s", &ch, &line, buf)) <= 0)
	{
		return n;
	}

	strcpy(p_ptr->history[line], buf);

	/*printf("Received history line %d: %s\n", line, buf);*/

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_char(void)
{
	int	n;
	char	ch;
	char	x, y;
	char	a, c;

	if ((n = Packet_scanf(&cbuf, "%c%c%c%c%c", &ch, &x, &y, &a, &c)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
	{
		Term_draw(x, y, a, c);

		/* Put the cursor there */
		Term_gotoxy(x, y);
	}
	else
		if ((n = Packet_printf(&qbuf, "%c%c%c%c%c", ch, x, y, a, c)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_message(void)
{
	int	n, c;
	char	ch;
	char	buf[1024], search[1024], *ptr;

	if ((n = Packet_scanf(&cbuf, "%c%s", &ch, buf)) <= 0)
	{
		return n;
	}

	/* XXX Mega-hack -- because we are not using checksums, sometimes under
	 * heavy traffic Receive_line_input receives corrupted data, which will cause
	 * the run-length encoded algorithm to exit prematurely.  Since there is no 
	 * end of line_info tag, the packet interpretor assumes the line_input data
	 * is finished, and attempts to parse the next byte as a new packet type.
	 * Since the ascii value of '.' (a frequently updated character) is 46, 
	 * which equals PKT_MESSAGE, if corrupted line_info data is received this function
	 * may get called wihtout a valid string.  To try to prevent the client from
	 * displaying a garbled string and messing up our screen, we will do a quick
	 * sanity check on the string.  Note that this might screw up people using
	 * international character sets.  (Such as the Japanese players)
	 * 
	 * A better solution for this would be to impliment some kind of packet checksum.
	 * -APD
	 */

	/* perform a sanity check on our string */
	/* Hack -- ' ' is numericall the lowest charcter we will probably be trying to
	 * display.  This might screw up international character sets.
	 */
	for (c = 0; c < n; c++) if (buf[c] < ' ') return 1;

/*	printf("Message: %s\n", buf);*/

	sprintf(search, "%s] ", nick);

	if (strstr(buf, search) != 0)
	{
		ptr = strstr(talk_pend, strchr(buf, ']') + 2);
		ptr = strtok(ptr, "\t");
		ptr = strtok(NULL, "\t");
		if (ptr) strcpy(talk_pend, ptr);
		else strcpy(talk_pend, "");
	}

	if (!topline_icky && (party_mode || shopping || !screen_icky))
	{
		c_msg_print(buf);
	}
	else
		if ((n = Packet_printf(&qbuf, "%c%s", ch, buf)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_state(void)
{
	int	n;
	char	ch;
	s16b	paralyzed, searching, resting;

	if ((n = Packet_scanf(&cbuf, "%c%hu%hu%hu", &ch, &paralyzed, &searching, &resting)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_state(paralyzed, searching, resting);
	else
		if ((n = Packet_printf(&qbuf, "%c%hu%hu%hu", ch, paralyzed, searching, resting)) <= 0)
		{
			return n;
		}
	
	return 1;
}

int Receive_title(void)
{
	int	n;
	char	ch;
	char	buf[80];

	if ((n = Packet_scanf(&cbuf, "%c%s", &ch, buf)) <= 0)
	{
		return n;
	}

	/* XXX -- Extract "ghost-ness" */
	p_ptr->ghost = streq(buf, "Ghost");

	if (!screen_icky && !shopping)
		prt_title(buf);
	else
		if ((n = Packet_printf(&qbuf, "%c%s", ch, buf)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_depth(void)
{
	int	n;
	char	ch;
	s16b	depth;

	if ((n = Packet_scanf(&cbuf, "%c%hu", &ch, &depth)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_depth(depth);
	else
		if ((n = Packet_printf(&qbuf, "%c%hu", ch, depth)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_confused(void)
{
	int	n;
	char	ch;
	bool	confused;

	if ((n = Packet_scanf(&cbuf, "%c%c", &ch, &confused)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_confused(confused);
	else
		if ((n = Packet_printf(&qbuf, "%c%c", ch, confused)) <= 0)
		{
			return n;
		}

	return 1;
}
	
int Receive_poison(void)
{
	int	n;
	char	ch;
	bool	poison;

	if ((n = Packet_scanf(&cbuf, "%c%c", &ch, &poison)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_poisoned(poison);
	else
		if ((n = Packet_printf(&qbuf, "%c%c", ch, poison)) <= 0)
		{
			return n;
		}

	return 1;
}
	
int Receive_study(void)
{
	int	n;
	char	ch;
	bool	study;

	if ((n = Packet_scanf(&cbuf, "%c%c", &ch, &study)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_study(study);
	else
		if ((n = Packet_printf(&qbuf, "%c%c", ch, study)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_food(void)
{
	int	n;
	char	ch;
	u16b	food;

	if ((n = Packet_scanf(&cbuf, "%c%hu", &ch, &food)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_hunger(food);
	else
		if ((n = Packet_printf(&qbuf, "%c%hu", ch, food)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_fear(void)
{
	int	n;
	char	ch;
	bool	afraid;

	if ((n = Packet_scanf(&cbuf, "%c%c", &ch, &afraid)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_afraid(afraid);
	else
		if ((n = Packet_printf(&qbuf, "%c%c", ch, afraid)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_speed(void)
{
	int	n;
	char	ch;
	s16b	speed;

	if ((n = Packet_scanf(&cbuf, "%c%hd", &ch, &speed)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_speed(speed);
	else
		if ((n = Packet_printf(&qbuf, "%c%hd", ch, speed)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_cut(void)
{
	int	n;
	char	ch;
	s16b	cut;

	if ((n = Packet_scanf(&cbuf, "%c%hd", &ch, &cut)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_cut(cut);
	else
		if ((n = Packet_printf(&qbuf, "%c%hd", ch, cut)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_blind(void)
{
	int	n;
	char	ch;
	bool	blind;

	if ((n = Packet_scanf(&cbuf, "%c%c", &ch, &blind)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_blind(blind);
	else
		if ((n = Packet_printf(&qbuf, "%c%c", ch, blind)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_stun(void)
{
	int	n;
	char	ch;
	s16b	stun;

	if ((n = Packet_scanf(&cbuf, "%c%hd", &ch, &stun)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !shopping)
		prt_stun(stun);
	else
		if ((n = Packet_printf(&qbuf, "%c%hd", ch, stun)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_item(void)
{
	char	ch;
	int	n, item;

	if ((n = Packet_scanf(&cbuf, "%c", &ch)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !topline_icky)
	{
		c_msg_print(NULL);

		if (!c_get_item(&item, "Which item? ", TRUE, TRUE, FALSE))
		{
			return 1;
		}

		Send_item(item);
	}
	else
		if ((n = Packet_printf(&qbuf, "%c", ch)) <= 0)
		{
			return n;
		}
	
	return 1;
}

int Receive_spell_info(void)
{
	char	ch;
	int	n;
	u16b	book, line;
	char	buf[80];

	if ((n = Packet_scanf(&cbuf, "%c%hu%hu%s", &ch, &book, &line, buf)) <= 0)
	{
		return n;
	}

	/* Save the info */
	strcpy(spell_info[book][line], buf);

	return 1;
}

int Receive_direction(void)
{
	char	ch;
	int	n, dir = 0;

	if ((n = Packet_scanf(&cbuf, "%c", &ch)) <= 0)
	{
		return n;
	}

	if (!screen_icky && !topline_icky && !shopping)
	{
		/* Ask for a direction */
		get_dir(&dir);

		/* Send it back */
		if ((n = Packet_printf(&wbuf, "%c%c", PKT_DIRECTION, dir)) <= 0)
		{
			return n;
		}
	}
	else
		if ((n = Packet_printf(&qbuf, "%c", ch)) <= 0)
		{
			return n;
		}

	return 1;
}

int Receive_flush(void)
{
	char	ch;
	int	n;

	if ((n = Packet_scanf(&cbuf, "%c", &ch)) <= 0)
	{
		return n;
	}

	/* Flush the terminal */
	Term_fresh();

	/* Wait */
	Term_xtra(TERM_XTRA_DELAY, 1);

	return 1;
}


int Receive_line_info(void)
{
	char	ch, c, n;
	int	x, i;
	s16b	y;
	byte	a;
	bool	draw = FALSE;

	if ((n = Packet_scanf(&cbuf, "%c%hd", &ch, &y)) <= 0)
	{
		return n;
	}

	/* If this is the mini-map then we can draw if the screen is icky */
	if (ch == PKT_MINI_MAP || (!screen_icky && !shopping))
		draw = TRUE;
	
	/* Check the max line count */
	if (y > last_line_info)
		last_line_info = y;

	for (x = 0; x < 80; x++)
	{
		/* Read the char/attr pair */
		Packet_scanf(&cbuf, "%c%c", &c, &a);

		/* Check for bit 0x40 on the attribute */
		if (a & 0x40)
		{
			/* First, clear the bit */
			a &= ~(0x40);

			/* Read the number of repetitions */
			Packet_scanf(&cbuf, "%c", &n);
		}
		else
		{
			/* No RLE, just one instance */
			n = 1;
		}

		/* Draw a character n times */
		for (i = 0; i < n; i++)
		{
			/* Don't draw anything if "char" is zero */
			if (c && draw)
				Term_draw(x + i, y, a, c);
		}

		/* Reset 'x' to the correct value */
		x += n - 1;

		/* hack -- if x > 80, assume we have received corrupted data,
		 * flush our buffers 
		 */
		if (x > 80) 
		{
			Sockbuf_clear(&rbuf);
			Sockbuf_clear(&cbuf);
		}

	}

	/* Request a redraw if the screen was icky */
	if (screen_icky)
		request_redraw = TRUE;

	return 1;
}

/*
 * Note that this function does not honor the "screen_icky"
 * flag, as it is only used for displaying the mini-map,
 * and the screen should be icky during that time.
 */
int Receive_mini_map(void)
{
	char	ch, c;
	int	n, x;
	s16b	y;
	byte	a;

	if ((n = Packet_scanf(&cbuf, "%c%hd", &ch, &y)) <= 0)
	{
		return n;
	}

	/* Check the max line count */
	if (y > last_line_info)
		last_line_info = y;

	for (x = 0; x < 80; x++)
	{
		Packet_scanf(&cbuf, "%c%c", &c, &a);

		/* Don't draw anything if "char" is zero */
		/* Only draw if the screen is "icky" */
		if (c && screen_icky && x < 80 - 12)
			Term_draw(x + 12, y, a, c);
	}

	return 1;
}

int Receive_special_other(void)
{
	int	n;
	char	ch;

	if ((n = Packet_scanf(&cbuf, "%c", &ch)) <= 0)
	{
		return n;
	}

	/* Set file perusal method to "other" */
	special_line_type = SPECIAL_FILE_OTHER;

	/* Peruse the file we're about to get */
	peruse_file();

	return 1;
}

int Receive_store(void)
{
	int	n, price;
	char	ch, pos, name[1024];
	byte	attr;
	s16b	wgt, num;

	if ((n = Packet_scanf(&cbuf, "%c%c%c%hd%hd%d%s", &ch, &pos, &attr, &wgt, &num, &price, name)) <= 0)
	{
		return n;
	}

	store.stock[pos].sval = attr;
	store.stock[pos].weight = wgt;
	store.stock[pos].number = num;
	store_prices[(int) pos] = price;
	strncpy(store_names[(int) pos], name, 80);

	/* Make sure that we're in a store */
	if (shopping)
	{
		display_inventory();
	}

	return 1;
}

int Receive_store_info(void)
{
	int	n;
	char	ch;
	s16b	owner_num, num_items;

	if ((n = Packet_scanf(&cbuf, "%c%hd%hd%hd", &ch, &store_num, &owner_num, &num_items)) <= 0)
	{
		return n;
	}

	store.stock_num = num_items;
	store_owner = owners[store_num][owner_num];

	/* Only enter "display_store" if we're not already shopping */
	if (!shopping)
		display_store();

	return 1;
}

int Receive_sell(void)
{
	int	n, price;
	char	ch, buf[1024];

	if ((n = Packet_scanf(&cbuf, "%c%d", &ch, &price)) <= 0)
	{
		return n;
	}

	/* Tell the user about the price */
	sprintf(buf, "Accept %d gold? ", price);

	if (get_check(buf))
		Send_store_confirm();

	return 1;
}

int Receive_target_info(void)
{
	int	n;
	char	ch, x, y, buf[80];

	if ((n = Packet_scanf(&cbuf, "%c%c%c%s", &ch, &x, &y, buf)) <= 0)
	{
		return n;
	}

	/* Print the message */
	prt(buf, 0, 0);

	/* Move the cursor */
	Term_gotoxy(x, y);

	return 1;
}

int Receive_sound(void)
{
	int	n;
	char	ch, sound;

	if ((n = Packet_scanf(&cbuf, "%c%c", &ch, &sound)) <= 0)
	{
		return n;
	}

	/* Make a sound (if allowed) */
	if (use_sound) Term_xtra(TERM_XTRA_SOUND, sound);

	return 1;
}

int Receive_special_line(void)
{
	int	n;
	char	ch, attr;
	s16b	max, line;
	char	buf[80];

	if ((n = Packet_scanf(&cbuf, "%c%hd%hd%c%s", &ch, &max, &line, &attr, buf)) <= 0)
	{
		return n;
	}

	/* Maximum */
	max_line = max;

	/* Print out the info */
	c_put_str(attr, buf, line + 2, 0);

	return 1;
}

int Receive_floor(void)
{
	int	n;
	char	ch, tval;

	if ((n = Packet_scanf(&cbuf, "%c%c", &ch, &tval)) <= 0)
	{
		return n;
	}

	/* Ignore for now */
	tval = tval;

	return 1;
}

int Receive_pickup_check(void)
{
	int	n;
	char	ch, buf[180];

	if ((n = Packet_scanf(&cbuf, "%c%s", &ch, buf)) <= 0)
	{
		return n;
	}

	/* Get a check */
	if (get_check(buf))
	{
		/* Pick it up */
		Send_stay();
	}

	return 1;
}


int Receive_party(void)
{
	int n;
	char ch, buf[160];

	if ((n = Packet_scanf(&cbuf, "%c%s", &ch, buf)) <= 0)
	{
		return n;
	}

	/* Copy info */
	strcpy(party_info, buf);

	/* Re-show party info */
	if (party_mode)
	{
		Term_erase(0, 13, 255);
		Term_putstr(0, 13, -1, TERM_WHITE, party_info);
		Term_putstr(0, 11, -1, TERM_WHITE, "Command: ");
	}

	return 1;
}

int Receive_skills(void)
{
	int	n, i;
	s16b tmp[11];
	char	ch;

	if ((n = Packet_scanf(&cbuf, "%c", &ch)) <= 0)
	{
		return n;
	}

	/* Read into skills info */
	for (i = 0; i < 11; i++)
	{
		if ((n = Packet_scanf(&cbuf, "%hd", &tmp[i])) <= 0)
		{
			return n;
		}
	}

	/* Store */
	p_ptr->skill_thn = tmp[0];
	p_ptr->skill_thb = tmp[1];
	p_ptr->skill_sav = tmp[2];
	p_ptr->skill_stl = tmp[3];
	p_ptr->skill_fos = tmp[4];
	p_ptr->skill_srh = tmp[5];
	p_ptr->skill_dis = tmp[6];
	p_ptr->skill_dev = tmp[7];
	p_ptr->num_blow = tmp[8];
	p_ptr->num_fire = tmp[9];
	p_ptr->see_infra = tmp[10];

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	return 1;
}

int Receive_pause(void)
{
	int n;
	char ch;

	if ((n = Packet_scanf(&cbuf, "%c", &ch)) <= 0)
	{
		return n;
	}

	/* Show the most recent changes to the screen */
	Term_fresh();

	/* Flush any pending keystrokes */
	Term_flush();

	/* The screen is icky */
	screen_icky = TRUE;

	/* Wait */
	inkey();

	/* Screen isn't icky any more */
	screen_icky = FALSE;

	/* Flush queue */
	Flush_queue();

	return 1;
}


int Receive_monster_health(void)
{
	int n;
	char ch, num;
	byte attr;

	if ((n = Packet_scanf(&cbuf, "%c%c%c", &ch, &num, &attr)) <= 0)
	{
		return n;
	}

	if (!screen_icky)
	{
		/* Draw the health bar */
		health_redraw(num, attr);
	}
	else
	{
		if ((n = Packet_printf(&qbuf, "%c%c%c", ch, num, attr)) <= 0)
		{
			return n;
		}
	}

	return 1;
}

int Send_search(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_SEARCH)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_walk(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_WALK, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_run(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_RUN, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_drop(int item, int amt)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_DROP, item, amt)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_drop_gold(s32b amt)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%ld", PKT_DROP_GOLD, amt)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_tunnel(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_TUNNEL, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_stay(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_STAND)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_toggle_search(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_SEARCH_MODE)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_rest(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_REST)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_go_up(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_GO_UP)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_go_down(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_GO_DOWN)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_open(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_OPEN, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_close(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_CLOSE, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_bash(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_BASH, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_disarm(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_DISARM, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_wield(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_WIELD, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_take_off(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_TAKE_OFF, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_destroy(int item, int amt)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_DESTROY, item, amt)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_inscribe(int item, cptr buf)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%s", PKT_INSCRIBE, item, buf)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_uninscribe(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_UNINSCRIBE, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_steal(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_STEAL, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_quaff(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_QUAFF, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_read(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_READ, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_aim(int item, int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%c", PKT_AIM_WAND, item, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_use(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_USE, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_zap(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_ZAP, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_fill(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_FILL, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_eat(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_EAT, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_activate(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_ACTIVATE, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_target(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_TARGET, dir)) <= 0)
	{
		return n;
	}

	return 1;
}


int Send_target_friendly(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_TARGET_FRIENDLY, dir)) <= 0)
	{
		return n;
	}

	return 1;
}


int Send_look(int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_LOOK, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_msg(cptr message)
{
	int	n;

	if (message && strlen(message))
	{
		if (strlen(talk_pend))
			strcat(talk_pend, "\t");
		strcat(talk_pend, message);
	}

	talk_resend = last_turns + 36;

	if (!strlen(talk_pend)) return 1;

	if ((n = Packet_printf(&wbuf, "%c%S", PKT_MESSAGE, talk_pend)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_fire(int item, int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c%hd", PKT_FIRE, dir, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_throw(int item, int dir)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c%hd", PKT_THROW, dir, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_item(int item)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_ITEM, item)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_gain(int book, int spell)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_GAIN, book, spell)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_cast(int book, int spell)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_SPELL, book, spell)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_pray(int book, int spell)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_PRAY, book, spell)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_ghost(int ability)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd", PKT_GHOST, ability)) <= 0)
	{
		return n;
	}
	
	return 1;
}

int Send_map(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_MAP)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_locate(int dir)
{
	int n;

	if ((n = Packet_printf(&wbuf, "%c%c", PKT_LOCATE, dir)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_store_purchase(int item, int amt)
{
	int 	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_PURCHASE, item, amt)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_store_sell(int item, int amt)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_SELL, item, amt)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_store_leave(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_STORE_LEAVE)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_store_confirm(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_STORE_CONFIRM)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_redraw(void)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_REDRAW)) <= 0)
	{
		return n;
	}
	
	/* Hack -- Clear the screen */
	Term_clear();

	return 1;
}

int Send_special_line(int type, int line)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%c%hd", PKT_SPECIAL_LINE, type, line)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_party(s16b command, cptr buf)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%s", PKT_PARTY, command, buf)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_purchase_house(int dir)
{
	int n;

	if ((n = Packet_printf(&wbuf, "%c%hd%hd", PKT_PURCHASE, dir, 0)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_suicide(void)
{
	int n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_SUICIDE)) <= 0)
	{
		return n;
	}

	return 1;
}

int Send_options(void)
{
	int i, n;

	if ((n = Packet_printf(&wbuf, "%c", PKT_OPTIONS)) <= 0)
	{
		return n;
	}

	/* Send each option */
	for (i = 0; i < 64; i++)
	{
		Packet_printf(&wbuf, "%c", Client_setup.options[i]);
	}

	return 1;
}

int Send_master(s16b command, cptr buf)
{
	int	n;

	if ((n = Packet_printf(&wbuf, "%c%hd%s", PKT_MASTER, command, buf)) <= 0)
	{
		return n;
	}

	return 1;
}
