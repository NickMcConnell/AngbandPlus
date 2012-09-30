/* File: irc.c */

/* Purpose: irc chat */

/*
 * Copyright (c) 2001 DarkGod, Andrew Sidwell
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#ifdef USE_SOCK

void *pern_irc;
bool pern_irc_connect = FALSE;
char irc_nick[30];
char irc_world[50];

void irc_connect()
{
        void *c;
        char buf[500], *s;

        if (pern_irc_connect) return;

        sprintf(irc_world, "Pern-Arda");
        sprintf(irc_nick, "Dummy");
        get_string("Enter Nickname: ", irc_nick, 10);

        c = zsock_connect(IRC_SERVER, atoi(IRC_PORT));

        zsock_send(c, format("NICK _%03d_%s\r\n", randint(999), irc_nick));
        zsock_wait(c);
        zsock_recv(c, buf, 500);
        s = strchr(buf, ':');
        zsock_send(c, format("PONG %s\r\n", s));
        zsock_send(c, format("USER guest 0 *BIRC :PernAngband %d.%d.%d User\r\n", FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH));
        zsock_send(c, format("JOIN %s\r\n", IRC_CHANNEL));

        pern_irc = c;
        pern_irc_connect = TRUE;
        cmsg_print(TERM_L_GREEN, "Connected to IRC");
}

void irc_change_nick()
{
	return;
}

void irc_disconnect()
{
        if (!pern_irc_connect) return;
        pern_irc_connect = FALSE;

        irc_quit(format("PernAngband %d.%d.%d", FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH));

        cmsg_print(TERM_L_RED, "Disconnected from IRC");
}

void irc_disconnect_aux(char *str, bool message)
{
        if (!pern_irc_connect) return;
        pern_irc_connect = FALSE;

        irc_quit(str);

        if (message) cmsg_print(TERM_L_RED, "Disconnected from IRC");
}

void irc_chat()
{
        char buf[80] = "";

        if (!pern_irc_connect) return;
        if (get_string("Say: ", buf, 80))
        {
                if (prefix(buf, "/me "))
                {
                        zsock_send(pern_irc, format("PRIVMSG %s :%cACTION %s%c%c\r\n", IRC_CHANNEL, 1, buf + 4, 1, 1 /*,irc_world*/));
                        message_add(MESSAGE_IRC, format("* %s %s", irc_nick, buf + 4), TERM_YELLOW);
                        fix_irc_message();
                }
                else
                {
                        zsock_send(pern_irc, format("PRIVMSG %s :%s\r\n", IRC_CHANNEL, buf /*, 3, irc_world */));
                        message_add(MESSAGE_IRC, format("<%s> #w%s", irc_nick, buf), TERM_YELLOW);
                        fix_irc_message();
                }
        }
}

#define TERM_CTCP	TERM_L_GREEN
#define TERM_SERVER	TERM_L_BLUE
#define TERM_CHAT1	TERM_YELLOW
#define TERM_CHAT2	TERM_WHITE

void irc_poll(void *sock)
{
        char buf[5000], *next, *nick;

        if (pern_irc_connect && zsock_can_read(sock))
        {
                zsock_recv(sock, buf, 2500);

                if (prefix(buf, "PING "))
                {
                        message_add(MESSAGE_IRC, format("*** Recieved a PING request from server %s.", buf + 6), TERM_SERVER);
                        zsock_send(pern_irc, format("PONG %s\r\n", buf + 5));
                        return;
                }
                if (*buf != ':') return;
                nick = buf + 1;

		if (prefix(nick, "_"))
		{
			nick = buf + 6;
		}

                next = strchr(nick, '!');
                if (next == NULL) return;
                *next = '\0';
                next++;
                next = strchr(next, ' ');
                if (next == NULL) return;
                next++;
                if (prefix(next, "PRIVMSG"))
                {
                        next = strchr(next, ':');
                        if (next == NULL) return;
                        *next = '\0';
                        next++;
                        if (*next == 1)
                        {
                                next++;
                                if (prefix(next, "ACTION"))
                                {
                                        next += 7;
                                        if (strlen(next)) next[strlen(next) - 1] = '\0';
                                        message_add(MESSAGE_IRC, format("* %s %s", nick, next), TERM_CHAT1);
                                        fix_irc_message();
                                }
                                else if (prefix(next, "PING"))
                                {
                                        message_add(MESSAGE_IRC, format("*** PING request from %s", nick), TERM_CTCP);
                                        fix_irc_message();

                                        zsock_send(pern_irc, format("NOTICE %s :%cPING %d%c\r\n", nick, 1, next, 1));
                                }
                                else if (prefix(next, "NICK"))
                                {
                                        message_add(MESSAGE_IRC, format("*** NICK request from %s", nick), TERM_CTCP);
                                        fix_irc_message();

                                        zsock_send(pern_irc, format("NOTICE %s :%cNICK %s%c\r\n", nick, 1, irc_nick, 1));
                                }
                                else if (prefix(next, "VERSION"))
                                {
                                        message_add(MESSAGE_IRC, format("*** VERSION request from %s", nick), TERM_CTCP);
                                        fix_irc_message();

                                        zsock_send(pern_irc, format("NOTICE %s :%cVERSION PernAngband %d.%d.%d%c\r\n", nick, 1, FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH, 1));
                                }
                        }
                        else
                        {
                                message_add(MESSAGE_IRC, format("#y<%s> #w%s", nick, next), TERM_CHAT1);
                                fix_irc_message();
                        }
                }
                if (prefix(next, "JOIN"))
                {
                        message_add(MESSAGE_IRC, format("%s has entered the Chatroom", nick), TERM_YELLOW);
                        fix_irc_message();
                }
                if (prefix(next, "QUIT"))
                {
                        next = strchr(next, ':');
                        if (next == NULL) return;
                        *next = '\0';
                        next++;
                        message_add(MESSAGE_IRC, format("%s has quit the Chatroom (%s)", nick, next), TERM_YELLOW);
                        fix_irc_message();
                }
        }
}


void irc_quit(char *str)
{
        char buf[300];

        sprintf(buf, "QUIT :%s\r\n", str);
        zsock_send(pern_irc, buf);
        zsock_disconnect(pern_irc);
}

#endif
