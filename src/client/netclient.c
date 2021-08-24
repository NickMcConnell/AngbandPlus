/*
 * File: netclient.c
 * Purpose: The client side of the networking stuff
 *
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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


/* Client shutdown attempts to send a quit packet to the server when it exits  */
bool send_quit = true;


/*
 * Structure (not array) of game constants
 */
struct angband_constants z_info_struct;


/*
 * Maximum number of flavors
 */
u16b flavor_max;


/* Section is icky */
s16b section_icky_col;
byte section_icky_row;


/* Get out of icky screen if requested */
bool allow_disturb_icky = true;


/* Hack -- player position for the minimap */
int cursor_x = 0;
int cursor_y = 0;


/* Similar to server's connp->state */
static int conn_state;
static u32b last_sent = 0, last_received = 0;


/* Keeps track of time in milliseconds */
static u32b mticks = 0;
#ifndef WINDOWS
/* Helper variable to keep track of time on UNIX */
static u32b ticks = 0;
#endif


static bool request_redraw;
static sockbuf_t rbuf, wbuf, qbuf;
static char talk_pend[MSG_LEN], initialized = 0;
static ang_file *fp = NULL;
static bool dump_only = false;
static byte chardump = 0;


/* Packet types */
static int cur_type = 0;
static int prev_type = 0;


/*** Utilities ***/


static int string_bytes(const char *str)
{
    return strlen(str) + 1;
}


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
        request_redraw = false;
    }

    return 1;
}


/* Keep track of time in milliseconds */
static void updateTicks(void)
{
#ifdef WINDOWS
    SYSTEMTIME st;

    /* Retrieve the current system date and time */
    GetSystemTime(&st);

    /* Keep track of time in milliseconds */
    mticks = ((st.wHour * 60L + st.wMinute) * 60L + st.wSecond) * 1000L + st.wMilliseconds;

#else
/* BACK-Ported update_ticks from MAngband 1.1.0: */
// Update the current time, which is stored in 100 ms "ticks".
// I hope that Windows systems have gettimeofday on them by default.
// If not there should hopefully be some simmilar efficient call with the same
// functionality. 
// I hope this doesn't prove to be a bottleneck on some systems.  On my linux system
// calling gettimeofday seems to be very very fast.
	struct timeval cur_time;
	int newticks;
	float scale = 100000;
	int mins,hours;

	hours = time(NULL) % 86400;
	mins = time(NULL) % 3600;

	gettimeofday(&cur_time, NULL);

	// Set the new ticks to the old ticks rounded down to the number of seconds.
	newticks = ticks-(ticks%10);
	// Find the new least significant digit of the ticks
	newticks += cur_time.tv_usec / scale;

	// Assume that it has not been more than one second since this function was last called
	if (newticks < ticks) newticks += 10;
	ticks = newticks;
	/*RLS*/
	mticks = (long)(hours*3600*100) +
		(long)(mins*60*100) +
		(long)(cur_time.tv_sec*100) +
		cur_time.tv_usec/((scale/100)/10);
#endif
    /* Wrap every day */
    if ((mticks < last_sent) || (mticks < last_received))
        last_sent = last_received = 0;
}


/* Write a keepalive packet to the output queue every second */
void do_keepalive(void)
{
    /* Keep track of time in milliseconds */
    updateTicks();

    /* Check to see if it has been 1 second since we last sent anything */
    if ((mticks - last_sent) > 1000)
    {
        if ((last_received < last_sent) && (conn_state == CONN_PLAYING))
        {
            lag_mark = 1000;
            player->upkeep->redraw |= (PR_LAG);
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
        /* Minimum window size is 16x4 */
        if (*cols < NORMAL_WID / 5) *cols = NORMAL_WID / 5;
        if (*rows < NORMAL_HGT / 5) *rows = NORMAL_HGT / 5;

        /* Maximum window size is 80x24 */
        if (*cols > NORMAL_WID) *cols = NORMAL_WID;
        if (*rows > NORMAL_HGT) *rows = NORMAL_HGT;
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
    check_term_resize(true, &cols, &rows);
    check_term_resize(true, &dummy_cols, &max_rows);

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
    Send_options(true);
}


/*** Receiving ***/


static int Receive_undefined(void)
{
    errno = 0;
#ifdef DEBUG_MODE
    /* The player really doesn't need to know about this */
    plog_fmt("Received unknown packet type (%d, %d, %d), dropping", cur_type, prev_type, conn_state);
#endif
    return -3;
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

    player->scr_info = mem_zalloc((Setup.max_row + ROW_MAP + 1) * sizeof(cave_view_type*));
    player->trn_info = mem_zalloc((Setup.max_row + ROW_MAP + 1) * sizeof(cave_view_type*));
    for (n = 0; n < Setup.max_row + ROW_MAP + 1; n++)
    {
        player->scr_info[n] = mem_zalloc((Setup.max_col + COL_MAP) * sizeof(cave_view_type));
        player->trn_info[n] = mem_zalloc((Setup.max_col + COL_MAP) * sizeof(cave_view_type));
    }

    return 1;
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


static int Receive_struct_info(void)
{
    byte ch;
    int i, n, j;
    char typ;
    u16b max;
    char name[NORMAL_WID];
    byte pflag;
    int bytes_read;

    typ = max = 0;

    if ((n = Packet_scanf(&rbuf, "%b%c%hu", &ch, &typ, &max)) <= 0)
        return n;
    bytes_read = 4;

    /* Which struct? */
    switch (typ)
    {
        /* Various Limits */
        case STRUCT_INFO_LIMITS:
        {
            u16b a_max, e_max, k_max, r_max, f_max, trap_max, pack_size, quiver_size, floor_size,
                quiver_slot_size, store_inven_max, curse_max;

            if ((n = Packet_scanf(&rbuf, "%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu%hu", &a_max, &e_max,
                &k_max, &r_max, &f_max, &trap_max, &flavor_max, &pack_size, &quiver_size,
                &floor_size, &quiver_slot_size, &store_inven_max, &curse_max)) <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(&rbuf, bytes_read);

                /* Packet isn't complete, graceful failure */
                return n;
            }
            bytes_read += 26;

            /* z_info */
            z_info = &z_info_struct;

            /* a_info */
            z_info->a_max = a_max;

            /* e_info */
            z_info->e_max = e_max;

            /* k_info */
            z_info->k_max = k_max;
            Client_setup.k_attr = mem_zalloc(z_info->k_max * sizeof(byte));
            Client_setup.k_char = mem_zalloc(z_info->k_max * sizeof(char));

            /* r_info */
            z_info->r_max = r_max;
            Client_setup.r_attr = mem_zalloc(z_info->r_max * sizeof(byte));
            Client_setup.r_char = mem_zalloc(z_info->r_max * sizeof(char));

            /* f_info */
            z_info->f_max = f_max;
            Client_setup.f_attr = mem_zalloc(z_info->f_max * sizeof(byte_lit));
            Client_setup.f_char = mem_zalloc(z_info->f_max * sizeof(char_lit));

            /* trap_info */
            z_info->trap_max = trap_max;
            Client_setup.t_attr = mem_zalloc(z_info->trap_max * sizeof(byte_lit));
            Client_setup.t_char = mem_zalloc(z_info->trap_max * sizeof(char_lit));

            /* Flavors */
            Client_setup.flvr_x_attr = mem_zalloc(flavor_max * sizeof(byte));
            Client_setup.flvr_x_char = mem_zalloc(flavor_max * sizeof(char));

            /* Autoinscriptions */
            Client_setup.note_aware = mem_zalloc(z_info->k_max * sizeof(char_note));

            /* Alloc */
            player->obj_aware = mem_zalloc(z_info->k_max * sizeof(bool));
            player->kind_ignore = mem_zalloc(z_info->k_max * sizeof(byte));
            player->kind_everseen = mem_zalloc(z_info->k_max * sizeof(byte));
            player->ego_ignore_types = mem_zalloc(z_info->e_max * sizeof(byte*));
            for (i = 0; i < z_info->e_max; i++)
                player->ego_ignore_types[i] = mem_zalloc(ITYPE_MAX * sizeof(byte));
            player->ego_everseen = mem_zalloc(z_info->e_max * sizeof(byte));

            /* Carrying capacity constants */
            z_info->pack_size = pack_size;
            z_info->quiver_size = quiver_size;
            z_info->floor_size = floor_size;
            z_info->quiver_slot_size = quiver_slot_size;

            z_info->store_inven_max = store_inven_max;

            /* Alloc */
            player->upkeep->inven = mem_zalloc((z_info->pack_size + 1) * sizeof(struct object *));
            player->upkeep->quiver = mem_zalloc(z_info->quiver_size * sizeof(struct object *));
            floor_items = mem_zalloc(z_info->floor_size * sizeof(struct object *));

            current_store.stock = mem_zalloc(z_info->store_inven_max * sizeof(struct object));
            store_names = mem_zalloc(z_info->store_inven_max * sizeof(store_name));

            /* curses */
            z_info->curse_max = curse_max;

            break;
        }

        /* Player Races */
        case STRUCT_INFO_RACE:
        {
            s16b base, dice, sides, m_bonus, r_skills, r_exp, res_level;
            byte ridx, r_mhp, flag, lvl;

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
                bytes_read += string_bytes(name) + 1;

                r = mem_zalloc(sizeof(*r));
                r->ridx = ridx;
                r->name = string_make(name);

                /* Transfer other fields here */
                for (j = 0; j < OBJ_MOD_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd%hd%hd%hd%b", &base, &dice, &sides, &m_bonus,
                        &lvl)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(r->name);
                        mem_free(r);
                        return n;
                    }
                    bytes_read += 9;

                    r->modifiers[j].value.base = base;
                    r->modifiers[j].value.dice = dice;
                    r->modifiers[j].value.sides = sides;
                    r->modifiers[j].value.m_bonus = m_bonus;
                    r->modifiers[j].lvl = lvl;
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
                if ((n = Packet_scanf(&rbuf, "%b%hd", &r_mhp, &r_exp)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    string_free(r->name);
                    mem_free(r);
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
                for (j = 1; j < OF_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%b", &lvl)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(r->name);
                        mem_free(r);
                        return n;
                    }
                    bytes_read += 1;

                    r->flvl[j] = lvl;
                }
                for (j = 0; j < ELEM_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd%b", &res_level, &lvl)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(r->name);
                        mem_free(r);
                        return n;
                    }
                    bytes_read += 3;

                    r->el_info[j].res_level = res_level;
                    r->el_info[j].lvl = lvl;
                }

                r->r_mhp = r_mhp;
                r->r_exp = r_exp;
                r->next = races;
                races = r;
            }

            break;
        }

        /* Player Classes */
        case STRUCT_INFO_CLASS:
        {
            s16b base, dice, sides, m_bonus, c_skills, res_level;
            byte cidx, c_mhp, total_spells, flag, lvl;
            u16b tval, sval;
            char num_books;
            char realm[NORMAL_WID];

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
                bytes_read += string_bytes(name) + 1;

                c = mem_zalloc(sizeof(*c));
                c->cidx = cidx;
                c->name = string_make(name);

                /* Transfer other fields here */
                for (j = 0; j < OBJ_MOD_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd%hd%hd%hd%b", &base, &dice, &sides, &m_bonus,
                        &lvl)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += 9;

                    c->modifiers[j].value.base = base;
                    c->modifiers[j].value.dice = dice;
                    c->modifiers[j].value.sides = sides;
                    c->modifiers[j].value.m_bonus = m_bonus;
                    c->modifiers[j].lvl = lvl;
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
                if ((n = Packet_scanf(&rbuf, "%b", &c_mhp)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    string_free(c->name);
                    mem_free(c);
                    return n;
                }
                bytes_read += 1;
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
                for (j = 0; j < OF_SIZE; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%b", &flag)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += 1;

                    c->flags[j] = flag;
                }
                for (j = 1; j < OF_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%b", &lvl)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += 1;

                    c->flvl[j] = lvl;
                }
                for (j = 0; j < ELEM_MAX; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd%b", &res_level, &lvl)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += 3;

                    c->el_info[j].res_level = res_level;
                    c->el_info[j].lvl = lvl;
                }
                if ((n = Packet_scanf(&rbuf, "%b%hu%c", &total_spells, &tval, &num_books)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    string_free(c->name);
                    mem_free(c);
                    return n;
                }
                bytes_read += 4;

                c->c_mhp = c_mhp;
                c->magic.total_spells = total_spells;
                c->magic.num_books = num_books;
                c->magic.books = mem_zalloc(num_books * sizeof(struct class_book));

                /* Hack -- put the tval in the unused "spell_first" field */
                c->magic.spell_first = tval;

                for (j = 0; j < num_books; j++)
                {
                    struct class_book *book = &c->magic.books[j];

                    if ((n = Packet_scanf(&rbuf, "%hu%hu%s", &tval, &sval, realm)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(c->name);
                        mem_free(c);
                        return n;
                    }
                    bytes_read += string_bytes(realm) + 4;

                    book->tval = tval;
                    book->sval = sval;
                    if (strlen(realm)) book->realm = lookup_realm(realm);
                }

                c->next = classes;
                classes = c;
            }

            break;
        }

        /* Player Bodies */
        case STRUCT_INFO_BODY:
        {
            s16b count, type;

            bodies = NULL;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct player_body *b;

                if ((n = Packet_scanf(&rbuf, "%hd%s", &count, name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name) + 2;

                b = mem_zalloc(sizeof(*b));
                b->name = string_make(name);
                b->count = count;
                b->slots = mem_zalloc(b->count * sizeof(struct equip_slot));

                /* Transfer other fields here */
                for (j = 0; j < b->count; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%hd%s", &type, name)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        string_free(b->name);
                        mem_free(b);
                        return n;
                    }
                    bytes_read += string_bytes(name) + 2;

                    b->slots[j].type = type;
                    b->slots[j].name = string_make(name);
                }

                b->next = bodies;
                bodies = b;
            }

            break;
        }  

        /* Socials */
        case STRUCT_INFO_SOCIALS:
        {
            byte target;

            /* Alloc */
            soc_info = mem_zalloc(max * sizeof(struct social));
            z_info->soc_max = max;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct social *s = &soc_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                s->name = string_make(name);

                /* Transfer other fields here */
                if ((n = Packet_scanf(&rbuf, "%b", &target)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += 1;

                s->target = target;
            }

            break;
        }

        /* Object kinds */
        case STRUCT_INFO_KINDS:
        {
            u16b tval, sval;
            u32b kidx;
            s16b ac;

            /* Alloc */
            k_info = mem_zalloc(max * sizeof(struct object_kind));

            /* Paranoia */
            if (max != z_info->k_max) z_info->k_max = max;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct object_kind *kind = &k_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                if (strlen(name)) kind->name = string_make(name);

                /* Transfer other fields here */
                if ((n = Packet_scanf(&rbuf, "%hu%hu%lu%hd", &tval, &sval, &kidx, &ac)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += 10;

                kind->tval = tval;
                kind->sval = sval;
                kind->kidx = kidx;

                /* Hack -- put flavor index into unused field "ac" */
                kind->ac = ac;

                for (j = 0; j < KF_SIZE; j++)
                {
                    if ((n = Packet_scanf(&rbuf, "%b", &pflag)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        return n;
                    }
                    bytes_read += 1;

                    kind->kind_flags[j] = pflag;
                }
            }

            break;
        }

        /* Object egos */
        case STRUCT_INFO_EGOS:
        {
            u32b eidx, kidx;
            u16b p, pmax;

            /* Alloc */
            e_info = mem_zalloc(max * sizeof(struct ego_item));

            /* Paranoia */
            if (max != z_info->e_max) z_info->e_max = max;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct ego_item *ego = &e_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                if (strlen(name)) ego->name = string_make(name);

                /* Transfer other fields here */
                if ((n = Packet_scanf(&rbuf, "%lu%hu", &eidx, &pmax)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += 6;

                ego->eidx = eidx;

                for (p = 0; p < pmax; p++)
                {
                    struct poss_item *poss;

                    if ((n = Packet_scanf(&rbuf, "%lu", &kidx)) <= 0)
                    {
                        /* Rollback the socket buffer */
                        Sockbuf_rollback(&rbuf, bytes_read);

                        /* Packet isn't complete, graceful failure */
                        return n;
                    }
                    bytes_read += 4;

                    poss = mem_zalloc(sizeof(struct poss_item));
                    poss->kidx = kidx;
                    poss->next = ego->poss_items;
                    ego->poss_items = poss;
                }
            }

            break;
        }

        /* Monster races */
        case STRUCT_INFO_RINFO:
        {
            /* Alloc */
            r_info = mem_zalloc(max * sizeof(struct monster_race));

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct monster_race *race = &r_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                if (strlen(name)) race->name = string_make(name);
                race->ridx = i;
            }

            break;
        }

        /* Monster base races */
        case STRUCT_INFO_RBINFO:
        {
            rb_info = NULL;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct monster_base *mb;

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                mb = mem_zalloc(sizeof(*mb));
                mb->name = string_make(name);
                mb->next = rb_info;
                rb_info = mb;
            }

            break;
        }

        /* Object curses */
        case STRUCT_INFO_CURSES:
        {
            char desc[NORMAL_WID];

            /* Alloc */
            curses = mem_zalloc(max * sizeof(struct curse));

            /* Paranoia */
            if (max != z_info->curse_max) z_info->curse_max = max;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct curse *curse = &curses[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                if (strlen(name)) curse->name = string_make(name);

                /* Transfer other fields here */
                if ((n = Packet_scanf(&rbuf, "%s", desc)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(desc);

                if (strlen(desc))
                {
                    my_strcap(desc);
                    curse->desc = string_make(desc);
                }
            }

            Send_play(chardump? 3: 2);

            break;
        }

        /* Player magic realms */
        case STRUCT_INFO_REALM:
        {
            char spell_noun[NORMAL_WID];
            char verb[NORMAL_WID];

            realms = NULL;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct magic_realm *realm;

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                /* Transfer other fields here */
                if ((n = Packet_scanf(&rbuf, "%s%s", spell_noun, verb)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(spell_noun) + string_bytes(verb);

                realm = mem_zalloc(sizeof(*realm));
                realm->name = string_make(name);
                if (strlen(spell_noun)) realm->spell_noun = string_make(spell_noun);
                if (strlen(verb)) realm->verb = string_make(verb);
                realm->next = realms;
                realms = realm;
            }

            break;
        }

        /* Terrain features */
        case STRUCT_INFO_FEAT:
        {
            /* Alloc */
            f_info = mem_zalloc(max * sizeof(struct feature));

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct feature *f = &f_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                if (strlen(name)) f->name = string_make(name);
                f->fidx = i;
            }

            Send_play(3);

            break;
        }

        /* Traps */
        case STRUCT_INFO_TRAP:
        {
            /* Alloc */
            trap_info = mem_zalloc(max * sizeof(struct trap_kind));

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct trap_kind *t = &trap_info[i];

                if ((n = Packet_scanf(&rbuf, "%s", name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name);

                if (strlen(name)) t->desc = string_make(name);
                t->tidx = i;
            }

            break;
        }

        /* Player timed effects */
        case STRUCT_INFO_TIMED:
        {
            byte grade_color, dummy;
            s16b grade_max;
            struct timed_grade *current, *l;

            i = -1;
            while (true)
            {
                if ((n = Packet_scanf(&rbuf, "%b%b%hd%s", &dummy, &grade_color, &grade_max,
                    name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(name) + 4;

                if (strlen(name))
                {
                    l = mem_zalloc(sizeof(*l));
                    current->next = l;
                    l->color = grade_color;
                    l->max = grade_max;
                    l->name = string_make(name);
                    current = current->next;
                }
                else
                {
                    i++;
                    if (i == max) break;
                    timed_grades[i] = mem_zalloc(sizeof(struct timed_grade));
                    current = timed_grades[i];
                }
            }

            break;
        }

        /* Player abilities */
        case STRUCT_INFO_PROPS:
        {
            u16b index, value;
            char type[NORMAL_WID], desc[NORMAL_WID];

            player_abilities = NULL;

            /* Fill */
            for (i = 0; i < max; i++)
            {
                struct player_ability *a;

                if ((n = Packet_scanf(&rbuf, "%hu%hu%s%s%s", &index, &value, type, desc, name)) <= 0)
                {
                    /* Rollback the socket buffer */
                    Sockbuf_rollback(&rbuf, bytes_read);

                    /* Packet isn't complete, graceful failure */
                    return n;
                }
                bytes_read += string_bytes(type) + string_bytes(desc) + string_bytes(name) + 4;

                a = mem_zalloc(sizeof(*a));
                a->index = index;
                a->type = string_make(type);
                a->desc = string_make(desc);
                a->name = string_make(name);
                a->value = value;

                a->next = player_abilities;
                player_abilities = a;
            }

            break;
        }
    }

    return 1;
}


static int Receive_death_cause(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%s%hd%ld%ld%hd%hd%hd%s%s", &ch, player->death_info.title,
        &player->death_info.lev, &player->death_info.exp, &player->death_info.au,
        &player->death_info.wpos.grid.y, &player->death_info.wpos.grid.x,
        &player->death_info.wpos.depth, player->death_info.died_from,
        player->death_info.ctime)) <= 0)
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


static int Receive_lvl(void)
{
    int n;
    byte ch;
    s16b lev, mlev;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &lev, &mlev)) <= 0)
        return n;

    player->lev = lev;
    player->max_lev = mlev;

    /* Redraw */
    player->upkeep->redraw |= (PR_LEV);

    return 1;
}


static int Receive_weight(void)
{
    int n;
    byte ch;
    s16b weight, max_weight;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &weight, &max_weight)) <= 0)
        return n;

    player->upkeep->total_weight = weight;

    /* Hack -- the capacity is stored in the inven_cnt (unused on client) */
    player->upkeep->inven_cnt = max_weight;

    /* Redraw */
    player->upkeep->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_plusses(void)
{
    int n;
    byte ch;
    s16b dd, ds, mhit, mdam, shit, sdam;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%hd%hd%hd", &ch, &dd, &ds, &mhit, &mdam, &shit,
        &sdam)) <= 0)
    {
        return n;
    }

    dis_dd = dd;
    dis_ds = ds;
    dis_to_mhit = mhit;
    dis_to_mdam = mdam;
    dis_to_shit = shit;
    dis_to_sdam = sdam;

    /* Redraw */
    player->upkeep->redraw |= (PR_PLUSSES);

    return 1;
}


static int Receive_ac(void)
{
    int n;
    byte ch;
    s16b base, plus;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &base, &plus)) <= 0)
        return n;

    player->known_state.ac = base;
    player->known_state.to_a = plus;

    player->upkeep->redraw |= (PR_ARMOR);

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

    player->max_exp = max;
    player->exp = cur;
    player->expfact = expfact;

    player->upkeep->redraw |= (PR_EXP);

    return 1;
}


static int Receive_gold(void)
{
    int n;
    byte ch;
    s32b gold;

    if ((n = Packet_scanf(&rbuf, "%b%ld", &ch, &gold)) <= 0)
        return n;

    player->au = gold;

    if (store_ctx) store_prt_gold();

    player->upkeep->redraw |= (PR_GOLD);

    return 1;
}


static int Receive_hp(void)
{
    int n;
    byte ch;
    s16b max, cur;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &max, &cur)) <= 0)
        return n;

    player->mhp = max;
    player->chp = cur;

    /* Redraw */
    player->upkeep->redraw |= (PR_HP);

    return 1;
}


static int Receive_sp(void)
{
    int n;
    byte ch;
    s16b max, cur;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &max, &cur)) <= 0)
        return n;

    player->msp = max;
    player->csp = cur;

    /* Redraw */
    player->upkeep->redraw |= (PR_MANA);

    return 1;
}


static int Receive_various(void)
{
    int n;
    byte ch;
    s16b hgt, wgt, age;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd", &ch, &hgt, &wgt, &age)) <= 0)
        return n;

    player->ht = hgt;
    player->wt = wgt;
    player->age = age;

    /* Redraw */
    player->upkeep->redraw |= (PR_OTHER);

    return 1;
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

    player->state.stat_top[(int)stat] = stat_top;
    player->state.stat_use[(int)stat] = stat_use;
    player->stat_max[(int)stat] = stat_max;
    player->state.stat_add[(int)stat] = stat_add;
    player->stat_cur[(int)stat] = stat_cur;

    /* Redraw */
    player->upkeep->redraw |= (PR_STATS);

    return 1;
}


static struct object *object_from_index(int item)
{
    int i, size = z_info->pack_size + player->body.count + z_info->quiver_size;

    for (i = 0; i < size; i++)
    {
        struct object *obj = &player->gear[i];

        if (obj->oidx == item) return obj;
    }

    return NULL;
}


static int Receive_index(void)
{
    int n;
    byte ch, type;
    s16b slot, index;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%b", &ch, &slot, &index, &type)) <= 0)
        return n;

    /* Quiver index */
    if (type == 0) player->upkeep->quiver[slot] = object_from_index(index);

    /* Inventory index */
    else if (type == 1) player->upkeep->inven[slot] = object_from_index(index);

    /* Equipment index */
    else if (type == 2) player->body.slots[slot].obj = object_from_index(index);

    return 1;
}


/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(struct player *p, const struct object *obj)
{
    /* Ammo */
    if (!tval_is_ammo(obj)) return false;

    /* Normal magic ammo cannot be branded */
    if (obj->info_xtra.magic) return false;

    return true;
}


/*
 * Hook to specify rechargeable items
 */
static bool item_tester_hook_recharge(struct player *p, const struct object *obj)
{
    /* Recharge staves and wands */
    if (tval_can_have_charges(obj)) return true;

    return false;
}


/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(struct player *p, const struct object *obj)
{
    return tval_is_weapon(obj);
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(struct player *p, const struct object *obj)
{
    return tval_is_armor(obj);
}


/*
 * Hook to specify identifiable items
 */
static bool item_tester_hook_identify(struct player *p, const struct object *obj)
{
    return !obj->info_xtra.known;
}


/*
 * Hook to specify potions
 */
static bool item_tester_hook_potion(struct player *p, const struct object *obj)
{
    return tval_is_potion(obj);
}


/*
 * Hook to specify items ok to carry
 */
static bool item_tester_hook_carry_okay(struct player *p, const struct object *obj)
{
    return inven_carry_okay(obj);
}


/*
 * Hook to specify uncursable items
 */
static bool item_tester_hook_uncurse(struct player *p, const struct object *obj)
{
    if (STRZERO(obj->info_xtra.name_curse)) return false;
    return true;
}


/*
 * Hook to specify drainable items
 */
static bool item_tester_hook_drain(struct player *p, const struct object *obj)
{
    /* Drain staves and wands */
    if (tval_can_have_charges(obj)) return true;

    return false;
}


/*
 * Hook to specify a staff
 */
static bool item_tester_hook_staff(struct player *p, const struct object *obj)
{
    return tval_is_staff(obj);
}


struct tester_hook
{
    const char *prompt;
    const char *reject;
    cmd_code code;
    int mode;
    item_tester item_tester_hook_item;
};


/* Hooks for Receive_item_request */
static struct tester_hook tester_hook_item[N_HOOKS] =
{
    {"Get which item? ", "You see nothing there.", CMD_PICKUP,
        USE_FLOOR, item_tester_hook_carry_okay},
    {"Enchant which item? ", "You have nothing to enchant.", CMD_NULL,
        USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR, item_tester_hook_weapon},
    {"Enchant which item? ", "You have nothing to enchant.", CMD_NULL,
        USE_EQUIP | USE_INVEN | USE_FLOOR, item_tester_hook_armour},
    {"Identify which item? ", "You have nothing to identify.", CMD_NULL,
        USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR, item_tester_hook_identify},
    {"Recharge which item? ", "You have nothing to recharge.", CMD_NULL,
        USE_INVEN | USE_FLOOR | SHOW_RECHARGE, item_tester_hook_recharge},
    {"Brand which kind of ammunition? ", "You have nothing to brand.", CMD_NULL,
        USE_INVEN | USE_QUIVER | USE_FLOOR, item_tester_hook_ammo},
    {"Send which item? ", "You have nothing to send.", CMD_NULL,
        USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR, NULL},
    {"Transform which potion? ", "You have nothing to transform.", CMD_NULL,
        USE_INVEN | USE_FLOOR, item_tester_hook_potion},
    {"Uncurse which item? ", "You have nothing to uncurse.", CMD_NULL,
        USE_EQUIP | USE_INVEN | USE_FLOOR, item_tester_hook_uncurse},
    {"Drain charges from which item? ", "You have nothing to drain charges from.", CMD_NULL,
        USE_INVEN | USE_FLOOR, item_tester_hook_drain},
    {"Make arrows from which staff? ", "You have no staff to use.", CMD_NULL,
        USE_INVEN | USE_FLOOR, item_tester_hook_staff}
};


static int Receive_item_request(void)
{
    byte ch;
    int n, item = -1, curse = -1;
    byte tester_hook;
    bool result = false;
    char dice_string[NORMAL_WID];

    if ((n = Packet_scanf(&rbuf, "%b%b%s", &ch, &tester_hook, dice_string)) <= 0)
        return n;

    if (!player->screen_save_depth && !topline_icky)
    {
        char inscription[20];

        inscription[0] = '\0';
        c_msg_print(NULL);

        switch (tester_hook)
        {
            /* Special hooks */
            case HOOK_RECALL:
                result = get_string("Recall depth: ", inscription, sizeof(inscription));
                break;
            case HOOK_DOWN:
                result = get_check("Are you sure you want to descend? ");
                break;
            case HOOK_CONFIRM:
                result = get_check("Word of Recall is already active. Do you want to cancel it? ");
                break;

            /* Item hooks */
            default:
            {
                struct object *obj;

                /* Used to show recharge failure rates */
                player->upkeep->recharge_pow = atoi(dice_string);

                result = get_item(&obj, tester_hook_item[tester_hook].prompt,
                    tester_hook_item[tester_hook].reject, tester_hook_item[tester_hook].code,
                    tester_hook_item[tester_hook].item_tester_hook_item,
                    tester_hook_item[tester_hook].mode);

                if (obj) item = obj->oidx;

                /* Hack -- select a single curse for uncursing */
                if (result && (tester_hook == HOOK_UNCURSE)) get_curse(&curse, obj, dice_string);
            }
        }

        if (!result)
            Send_flush();
        else
            Send_item(item, curse, inscription);
    }
    else if ((n = Packet_printf(&qbuf, "%b%b", (unsigned)ch, (unsigned)tester_hook)) <= 0)
        return n;

    return 1;
}


static int Receive_title(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%s", &ch, title)) <= 0)
        return n;

    /* Hack -- extract "ghost-ness" */
    player->ghost = streq(title, "Ghost");

    /* Redraw */
    player->upkeep->redraw |= (PR_TITLE | PR_LAG);

    return 1;
}


static int Receive_turn(void)
{
    int n;
    byte ch;
    u32b game_turn, player_turn, active_turn;

    if ((n = Packet_scanf(&rbuf, "%b%lu%lu%lu", &ch, &game_turn, &player_turn, &active_turn)) <= 0)
        return n;

    player->game_turn.era = 0;
    player->game_turn.turn = game_turn;
    player->player_turn.era = 0;
    player->player_turn.turn = player_turn;
    player->active_turn.era = 0;
    player->active_turn.turn = active_turn;

    /* Redraw */
    player->upkeep->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_depth(void)
{
    int n;
    byte ch;
    s16b depth, maxdepth;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%s", &ch, &depth, &maxdepth, player->depths)) <= 0)
        return n;

    player->wpos.depth = depth;
    player->max_depth = maxdepth;

    player->upkeep->redraw |= (PR_DEPTH);

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

        player->timed[i] = effect;
    }

    player->upkeep->redraw |= (PR_STATUS);

    return 1;
}


static int Receive_recall(void)
{
    int n;
    byte ch;
    s16b word_recall, deep_descent;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &word_recall, &deep_descent)) <= 0)
        return n;

    player->word_recall = word_recall;
    player->deep_descent = deep_descent;

    player->upkeep->redraw |= (PR_STATE);

    return 1;
}


static int Receive_state(void)
{
    int n;
    byte ch;
    s16b stealthy, resting, unignoring, obj_feeling, mon_feeling, square_light, num_moves;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%hd%hd%hd%hd%s", &ch, &stealthy, &resting, &unignoring,
        &obj_feeling, &mon_feeling, &square_light, &num_moves, player->terrain)) <= 0)
    {
        return n;
    }

    player->upkeep->resting = resting;
    player->stealthy = (byte)stealthy;
    player->unignoring = (byte)unignoring;
    player->obj_feeling = obj_feeling;
    player->mon_feeling = mon_feeling;
    player->square_light = square_light;
    player->state.num_moves = num_moves;

    player->upkeep->redraw |= (PR_STATE);

    return 1;
}


/*
 * Decodes a (possibly) RLE-encoded stream of attr/char pairs
 *
 * See "rle_encode" for possible "mode" descriptions.
 *
 * Note -- if "lineref" is NULL, the packets will be read from   
 * the queue for no effect (useful for discarding)
 */
static int rle_decode(sockbuf_t* buf, cave_view_type* lineref, int max_col, int mode,
    int* bytes_read)
{
    int x, i, nread;
    char c;
    u16b a, n = 0;

    for (x = 0; x < max_col; x++)
    {
        /* Read the char/attr pair */
        nread = Packet_scanf(buf, "%c%hu", &c, &a);
        if (nread <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(buf, *bytes_read);

            /* Packet isn't complete, graceful failure */
            return nread;
        }
        *bytes_read += 3;

        /* RLE_LARGE */
        if ((mode == RLE_LARGE) && (a & 0x8000))
        {
            /* First, clear the bit */
            a &= ~(0x8000);

            /* Read the number of repetitions */
            nread = Packet_scanf(buf, "%hu", &n);
            if (nread <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(buf, *bytes_read);

                /* Packet isn't complete, graceful failure */
                return nread;
            }
            *bytes_read += 2;
        }

        /* RLE_CLASSIC */
        else if ((mode == RLE_CLASSIC) && (a & 0x40))
        {
            /* First, clear the bit */
            a &= ~(0x40);

            /* Read the number of repetitions */
            nread = Packet_scanf(buf, "%hu", &n);
            if (nread <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(buf, *bytes_read);

                /* Packet isn't complete, graceful failure */
                return nread;
            }
            *bytes_read += 2;
        }

        /* No RLE, just one instance */
        else
            n = 1;

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


/*
 * Mentally draw an attr/char at a given location
 *
 * Given location and values may or not be valid.
 */
static void Term_queue_char_safe(int x, int y, u16b a, char c, u16b ta, char tc)
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
static void Term_big_queue_char_safe(int x, int y, u16b a, char c, u16b a1, char c1)
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
                    Term_queue_char_safe(x + hor, y, COLOUR_WHITE, ' ', a1, c1);
            }

            /* Now vertical */
            for (vert = 1; vert < tile_height; vert++)
            {
                /* Queue dummy character */
                if (a & 0x80)
                    Term_queue_char_safe(x + hor, y + vert, 255, -1, 0, 0);
                else
                    Term_queue_char_safe(x + hor, y + vert, COLOUR_WHITE, ' ', a1, c1);
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
                Term_queue_char_safe(x, y + vert, COLOUR_WHITE, ' ', a1, c1);
        }
    }
}


#define DUNGEON_RLE_MODE() (use_graphics? RLE_LARGE: RLE_CLASSIC)
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
    r = player->remote_term;
    mode = DUNGEON_RLE_MODE();
    dest = player->scr_info[y];
    trn = player->trn_info[y];
    draw = true;

    /* Use ANOTHER terminal */
    if (r != NTERM_WIN_OVERHEAD)
    {
        /* Reset the line counter */
        if (y == -1) return 1;

        /* Mini Map Terminal */
        if (ch == PKT_MINI_MAP) player->upkeep->redraw |= (PR_MAP);

        /* Values */
        dest = remote_info[r][y];
        draw = false;

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
            player->trn_info[y][x].c = 0;
            player->trn_info[y][x].a = 0;
        }

        /* Mini Map */
        if (ch == PKT_MINI_MAP) draw = player->screen_save_depth;

        /* Dungeon */
        else
        {
            /* Decode the secondary attr/char stream */
            if (use_graphics)
            {
                n = rle_decode(&rbuf, player->trn_info[y], cols, RLE_LARGE, &bytes_read);
                if (n <= 0) return n;
            }

            draw = !player->screen_save_depth;

            /* Hack -- shopping */
            if (store_ctx) draw = false;

            /* Hang on! Icky section! */
            if (section_icky_row && (y < section_icky_row))
            {
                if (section_icky_col > 0) xoff = section_icky_col;
                if (section_icky_col < 0) coff = section_icky_col;
                if ((xoff >= cols) || (cols - coff <= 0)) draw = false;
            }

            /* Request a redraw if the line was icky */
            if (!draw) request_redraw = true;
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

            if (ch == PKT_MINI_MAP) Term->minimap_active = true;

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
                    u16b a_dummy = (use_graphics? COLOUR_WHITE: 0);
                    char c_dummy = (use_graphics? ' ': 0);

                    Term_big_queue_char_safe(x, y, scr_info->a, scr_info->c, a_dummy, c_dummy);
                }
            }
        }
    }

    return 1;
}


static int Receive_speed(void)
{
    int n;
    byte ch;
    s16b speed, mult;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &speed, &mult)) <= 0)
        return n;

    player->state.speed = speed;
    player->state.ammo_mult = mult;

    player->upkeep->redraw |= (PR_SPEED);

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

    player->upkeep->new_spells = study;
    player->can_study_book = (bool)can_study_book;

    player->upkeep->redraw |= (PR_STUDY);

    return 1;
}


static int Receive_count(void)
{
    int n;
    byte ch, type;
    s16b count;

    if ((n = Packet_scanf(&rbuf, "%b%b%hd", &ch, &type, &count)) <= 0)
        return n;

    if (type == 1) player->upkeep->quiver_cnt = count;
    else player->upkeep->equip_cnt = count;

    /* Redraw */
    player->upkeep->redraw |= (PR_EQUIP | PR_INVEN);

    return 1;
}


static int Receive_show_floor(void)
{
    int n;
    byte ch;
    byte mode;

    if ((n = Packet_scanf(&rbuf, "%b%b", &ch, &mode)) <= 0)
        return n;

    topline_icky = true;
    screen_save();
    show_floor((int)mode, NULL);
    inkey();
    screen_load(false);
    topline_icky = false;
    prt("", 0, 0);

    return 1;
}


static int Receive_char(void)
{
    int n;
    byte ch;
    byte x, y, x_off;
    char c, tcp;
    u16b a, tap;
    bool draw = true;
    int bytes_read;

    tap = tcp = c = a = x = y = 0;

    if ((n = Packet_scanf(&rbuf, "%b%b%b%hu%c", &ch, &x, &y, &a, &c)) <= 0)
        return n;
    bytes_read = 6;

    /* Hack -- use ANOTHER terminal */
    if ((n = player->remote_term) != NTERM_WIN_OVERHEAD)
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

    player->scr_info[y][x].a = a;
    player->scr_info[y][x].c = c;

    if (use_graphics)
    {
        if ((n = Packet_scanf(&rbuf, "%hu%c", &tap, &tcp)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read += 3;

        player->trn_info[y][x].a = tap;
        player->trn_info[y][x].c = tcp;
    }

    /* Hack -- manipulate offset */
    x_off = x + COL_MAP;

    if (player->screen_save_depth || section_icky_row || store_ctx) draw = false;
    if (section_icky_row)
    {
        if (y >= section_icky_row) draw = true;
        else if ((section_icky_col > 0) && (x_off >= section_icky_col)) draw = true;
        else if ((section_icky_col < 0) && (x_off >= 0 - section_icky_col)) draw = true;
    }

    if (draw)
    {
        x_off += x * (tile_width - 1);
        y = (y - 1) * tile_height + 1;

        Term_queue_char_safe(x_off, y, a, c, tap, tcp);
        if (tile_width * tile_height > 1)
        {
            u16b a_dummy = (use_graphics? COLOUR_WHITE: 0);
            char c_dummy = (use_graphics? ' ': 0);

            Term_big_queue_char_safe(x_off, y, a, c, a_dummy, c_dummy);
        }
    }

    /* Queue for later */
    else
    {
        n = Packet_printf(&qbuf, "%b%b%b%hu%c", (unsigned)ch, (unsigned)x,
            (unsigned)y, (unsigned)a, (int)c);
        if ((n > 0) && use_graphics)
            Packet_printf(&qbuf, "%hu%c", (unsigned)tap, (int)tcp);
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

    /* Hack -- wipe the arrays if blank */
    if (!strlen(buf))
        memset(book_info, 0, MAX_PAGES * sizeof(struct book_info));

    /* Save the info */
    else
    {
        book_info[book].spell_info[line].flag.line_attr = line_attr;
        book_info[book].spell_info[line].flag.flag = flag;
        book_info[book].spell_info[line].flag.dir_attr = dir_attr;
        book_info[book].spell_info[line].flag.proj_attr = proj_attr;
        my_strcpy(book_info[book].spell_info[line].info, buf, NORMAL_WID);
    }

    /* Redraw */
    player->upkeep->redraw |= PR_SPELL;

    return 1;
}


static int Receive_book_info(void)
{
    byte ch;
    int n;
    s16b book;
    char realm[NORMAL_WID];

    if ((n = Packet_scanf(&rbuf, "%b%hd%s", &ch, &book, realm)) <= 0)
    {
        return n;
    }

    if (strlen(realm)) book_info[book].realm = lookup_realm(realm);

    return 1;
}


static int Receive_floor(void)
{
    int n, bytes_read;
    byte ch, num, notice, attr, act, aim, fuel, fail, known, known_effect, carry, quality_ignore,
        ignored, magic, throwable, force;
    u16b tval, sval;
    s16b amt, slot, oidx, eidx, bidx;
    s32b pval;
    quark_t note;
    char name[NORMAL_WID];
    char name_terse[NORMAL_WID];
    char name_base[NORMAL_WID];
    char name_curse[NORMAL_WID];
    char name_power[NORMAL_WID];
    struct object *obj;

    if ((n = Packet_scanf(&rbuf, "%b%b%b", &ch, &num, &force)) <= 0)
        return n;
    bytes_read = 3;

    if ((n = Packet_scanf(&rbuf, "%hu%hu%hd%lu%ld%b%hd", &tval, &sval, &amt, &note, &pval, &notice,
        &oidx)) <= 0)
    {
        /* Rollback the socket buffer */
        Sockbuf_rollback(&rbuf, bytes_read);

        /* Packet isn't complete, graceful failure */
        return n;
    }
    bytes_read += 17;

    if ((n = Packet_scanf(&rbuf, "%b%b%b%b%b%hd%b%b%b%b%b%hd%b%hd%b", &attr, &act, &aim, &fuel,
        &fail, &slot, &known, &known_effect, &carry, &quality_ignore, &ignored, &eidx, &magic,
        &bidx, &throwable)) <= 0)
    {
        /* Rollback the socket buffer */
        Sockbuf_rollback(&rbuf, bytes_read);

        /* Packet isn't complete, graceful failure */
        return n;
    }
    bytes_read += 18;

    if ((n = Packet_scanf(&rbuf, "%s%s%s%s%s", name, name_terse, name_base, name_curse,
        name_power)) <= 0)
    {
        /* Rollback the socket buffer */
        Sockbuf_rollback(&rbuf, bytes_read);

        /* Packet isn't complete, graceful failure */
        return n;
    }

    /* Paranoia */
    if (num >= z_info->floor_size) return 1;

    /* No item */
    if (!tval)
    {
        /* Force response */
        if (force) Send_floor_ack();

        /* Clear */
        else
        {
            cleanup_floor();
            floor_num = 0;
            player->upkeep->redraw |= (PR_EQUIP);
        }
    }

    /* Add the item */
    else
    {
        floor_items[num] = mem_zalloc(sizeof(struct object));

        /* Get the item (on the floor) */
        obj = floor_items[num];

        /* Item info used by the client */
        obj->tval = tval;
        obj->sval = sval;
        obj->number = amt;
        obj->kind = (tval? lookup_kind(tval, sval): NULL);
        obj->note = note;
        obj->pval = pval;
        obj->notice = notice;
        obj->oidx = oidx;

        /* Hack -- extra information used by the client */
        obj->info_xtra.attr = attr;
        obj->info_xtra.act = act;
        obj->info_xtra.aim = aim;
        obj->info_xtra.fuel = fuel;
        obj->info_xtra.fail = fail;
        obj->info_xtra.slot = slot;
        obj->info_xtra.known = known;
        obj->info_xtra.known_effect = known_effect;
        obj->info_xtra.carry = carry;
        obj->info_xtra.quality_ignore = quality_ignore;
        obj->info_xtra.ignored = ignored;
        obj->info_xtra.eidx = eidx;
        obj->info_xtra.magic = magic;
        obj->info_xtra.bidx = bidx;
        obj->info_xtra.throwable = throwable;

        /* Hack -- the name is stored separately */
        my_strcpy(obj->info_xtra.name, name, sizeof(obj->info_xtra.name));
        my_strcpy(obj->info_xtra.name_terse, name_terse, sizeof(obj->info_xtra.name_terse));
        my_strcpy(obj->info_xtra.name_base, name_base, sizeof(obj->info_xtra.name_base));
        my_strcpy(obj->info_xtra.name_curse, name_curse, sizeof(obj->info_xtra.name_curse));
        my_strcpy(obj->info_xtra.name_power, name_power, sizeof(obj->info_xtra.name_power));

        /* Hack -- number of floor items */
        floor_num++;
    }

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

    if (player->screen_save_depth && special_line_type) return 1;
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
    s16b wgt, bidx;
    char pos;
    s32b price;
    byte num, owned, max;
    u16b tval;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%c%b%hd%b%b%ld%hu%b%hd%s", &ch, &pos, &attr, &wgt, &num, &owned,
        &price, &tval, &max, &bidx, name)) <= 0)
    {
        return n;
    }

    /* Item info used by the client */
    current_store.stock[pos].tval = tval;
    current_store.stock[pos].weight = wgt;
    current_store.stock[pos].number = num;
    current_store.stock[pos].askprice = price;

    /* Hack -- extra information used by the client */
    current_store.stock[pos].info_xtra.attr = attr;
    current_store.stock[pos].info_xtra.max = max;
    current_store.stock[pos].info_xtra.owned = owned;
    current_store.stock[pos].info_xtra.bidx = bidx;

    my_strcpy(store_names[pos], name, sizeof(store_names[0]));

    return 1;
}


static int Receive_store_info(void)
{
    int n;
    byte ch;
    char store_name[NORMAL_WID];
    char store_owner_name[NORMAL_WID];
    s16b num_items;
    s32b max_cost;
    s16b type;

    if ((n = Packet_scanf(&rbuf, "%b%hd%s%s%s%hd%ld", &ch, &type, store_name, store_owner_name,
        welcome, &num_items, &max_cost)) <= 0)
    {
        return n;
    }

    current_store.stock_num = num_items;
    current_store.owner->max_cost = max_cost;
    string_free(current_store.name);
    current_store.name = string_make(store_name);
    string_free(current_store.owner->name);
    current_store.owner->name = string_make(store_owner_name);
    current_store.type = type;

    /* Only display store if we're not already shopping */
    if (!store_ctx) store_enter();
    else store_prt_frame();

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
        Term_set_cursor(true);
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
    player->state.skills[SKILL_TO_HIT_MELEE] = tmp[0];
    player->state.skills[SKILL_TO_HIT_BOW] = tmp[1];
    player->state.skills[SKILL_SAVE] = tmp[2];
    player->state.skills[SKILL_STEALTH] = tmp[3];
    player->state.skills[SKILL_SEARCH] = tmp[4];
    player->state.skills[SKILL_DISARM_PHYS] = tmp[5];
    player->state.skills[SKILL_DISARM_MAGIC] = tmp[6];
    player->state.skills[SKILL_DEVICE] = tmp[7];
    player->state.num_blows = tmp[8];
    player->state.num_shots = tmp[9];
    player->state.see_infra = tmp[10];

    /* Redraw */
    player->upkeep->redraw |= (PR_OTHER);

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
    player->screen_save_depth++;
    Send_icky();

    /* Wait */
    inkey_ex();

    /* Screen isn't icky any more */
    player->screen_save_depth--;
    Send_icky();

    /* Flush queue */
    Flush_queue();

    /* Show the most recent changes to the screen */
    Term_fresh();

    /* Flush messages */
    c_msg_print(NULL);

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

    player->upkeep->redraw |= (PR_HEALTH);

    return 1;
}


static int Receive_aware(void)
{
    int n, i;
    byte ch;
    u16b num;
    byte setting;
    int bytes_read;

    if ((n = Packet_scanf(&rbuf, "%b%hu", &ch, &num)) <= 0)
        return n;
    bytes_read = 3;

    if (num == z_info->k_max)
    {
        for (i = 0; i < z_info->k_max; i++)
        {
            if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(&rbuf, bytes_read);

                /* Packet isn't complete, graceful failure */
                return n;
            }
            bytes_read++;

            player->obj_aware[i] = (setting? true: false);
        }
    }
    else
    {
        if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read++;

        player->obj_aware[num] = (setting? true: false);
    }

    return 1;
}


static int Receive_everseen(void)
{
    int n, i;
    byte ch;
    u16b num;
    byte setting;
    int bytes_read;

    if ((n = Packet_scanf(&rbuf, "%b%hu", &ch, &num)) <= 0)
        return n;
    bytes_read = 3;

    if (num == z_info->k_max)
    {
        for (i = 0; i < z_info->k_max; i++)
        {
            if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(&rbuf, bytes_read);

                /* Packet isn't complete, graceful failure */
                return n;
            }
            bytes_read++;

            player->kind_everseen[i] = setting;
        }
    }
    else
    {
        if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read++;

        player->kind_everseen[num] = setting;
    }

    return 1;
}


static int Receive_ego_everseen(void)
{
    int n, i;
    byte ch;
    u16b num;
    byte setting;
    int bytes_read;

    if ((n = Packet_scanf(&rbuf, "%b%hu", &ch, &num)) <= 0)
        return n;
    bytes_read = 3;

    if (num == z_info->e_max)
    {
        for (i = 0; i < z_info->e_max; i++)
        {
            if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
            {
                /* Rollback the socket buffer */
                Sockbuf_rollback(&rbuf, bytes_read);

                /* Packet isn't complete, graceful failure */
                return n;
            }
            bytes_read++;

            player->ego_everseen[i] = setting;
        }
    }
    else
    {
        if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read++;

        player->ego_everseen[num] = setting;
    }

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
        Term_set_cursor((vis > 0)? true: false);
        Term->double_cursor = ((vis == 1)? true: false);
    }

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

    n = rle_decode(&rbuf, player->hist_flags[y], player->body.count + 1, DUNGEON_RLE_MODE(),
        &bytes_read);
    if (n <= 0) return n;

    /* Redraw after last line is received */
    if (y == N_HISTORY_FLAGS - 1) player->upkeep->redraw |= (PR_EQUIP);

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
    my_strcpy(book_info[book].spell_info[line].desc, buf, MSG_LEN);

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

    player->upkeep->redraw |= (PR_DTRAP);

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
        case NTERM_ACTIVATE: player->remote_term = arg; break;
        case NTERM_CLEAR:
        {
            if (arg == 1) Term_clear();
            last_remote_line[player->remote_term] = 0;
            break;
        }
        case NTERM_HOLD:
        {
            /* Get out of icky screen if requested */
            if ((arg == 1) && allow_disturb_icky)
            {
                ui_event ea = EVENT_ABORT;

                Term_event_push(&ea);
            }
            break;
        }
        case NTERM_FLUSH:
        {
            last = last_remote_line[player->remote_term];
            for (n = 0; n <= last; n++)
                caveprt(remote_info[player->remote_term][n], NORMAL_WID, 0, n);
            break;
        }
        case NTERM_FRESH:
        {
            /* Copy header to local buffer */
            my_strcpy(special_line_header[player->remote_term],
                special_line_header[NTERM_WIN_OVERHEAD], sizeof(special_line_header[0]));

            /* Redraw */
            switch (player->remote_term)
            {
                case NTERM_WIN_MAP: player->upkeep->redraw |= (PR_MAP); break;
                case NTERM_WIN_OBJLIST: player->upkeep->redraw |= (PR_ITEMLIST); break;
                case NTERM_WIN_OBJECT: player->upkeep->redraw |= (PR_OBJECT); break;
                case NTERM_WIN_MONSTER: player->upkeep->redraw |= (PR_MONSTER); break;
                case NTERM_WIN_MONLIST: player->upkeep->redraw |= (PR_MONLIST); break;
            }
            if (arg != NTERM_POP) break;

            /* Fall through */
        }
        case NTERM_POP:
        {
            /* Popup Hack */
            screen_save();
            last = last_remote_line[player->remote_term];
            for (n = 0; n <= (full_icky_screen? NORMAL_HGT - 1: last + 5); n++)
                Term_erase(0, n, NORMAL_WID);
            c_put_str(COLOUR_YELLOW, special_line_header[player->remote_term], 0, 0);
            for (n = 0; n <= last; n++)
                caveprt(remote_info[player->remote_term][n], NORMAL_WID, 0, n + 2);
            c_put_str(COLOUR_L_BLUE, "[Press any key to continue]", last + 4, 0);
            inkey_ex();
            screen_load(true);
            Term_fresh();
            check_store_leave(true);
            break;
        }
    }

    return 1;
}


static int Receive_player_pos(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%hd", &ch, &player->grid.x, &player->offset_grid.x,
        &player->grid.y, &player->offset_grid.y)) <= 0)
    {
        return n;
    }

    return 1;
}


static int Receive_minipos(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &cursor_y, &cursor_x)) <= 0)
    {
        return n;
    }

    return 1;
}


static int Receive_message_flush(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    /* Flush messages */
    c_msg_print(NULL);

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


static int Receive_quit(void)
{
    char pkt;
    char reason[NORMAL_WID];

    /* Redraw stuff before quitting to show the cause of death (and last messages) */
    player->upkeep->redraw |= (PR_MESSAGE);
    redraw_stuff();
    Term_fresh();

    if (Packet_scanf(&rbuf, "%c", &pkt) != 1)
    {
        errno = 0;
        plog("Can't read quit packet");
    }
    else
    {
        ui_event ch;
        char buf[NORMAL_WID];

        if (Packet_scanf(&rbuf, "%s", reason) <= 0)
            my_strcpy(reason, "unknown reason", sizeof(reason));
        errno = 0;
        plog_fmt("Quitting: %s", reason);

        /* Hack -- restart game without quitting */
        if (!strstr(reason, "Server shutdown"))
        {
            strnfmt(buf, NORMAL_WID - 2, "%.70s[y/n] ", "Start a new game? ");
            prt(buf, 0, 0);
            Term_fresh();
            Term_inkey(&ch, true, true);
            if ((ch.key.code != 'Y') && (ch.key.code != 'y')) quit(NULL);
            play_again = true;
        }
    }
    return -1;
}


static int Receive_features(void)
{
    int n;
    byte ch;
    s16b lighting, off;

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd", &ch, &lighting, &off)) <= 0)
        return n;

    if (off == z_info->f_max)
    {
        off = 0;
        lighting++;
    }

    /* Send request for splash screen (MOTD) to read */
    if (lighting == LIGHTING_MAX)
    {
        Setup.initialized = true;
        Send_text_screen(TEXTFILE_MOTD, 0);
    }

    /* Request continuation */
    else
        Send_features(lighting, off);

    return 1;
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
            /* Send request for tombstone to read */
            Send_text_screen(TEXTFILE_TOMB, 0);
        }

        /* Tombstone */
        else if (type == TEXTFILE_TOMB)
        {
            /* Send request for winner crown to read */
            Send_text_screen(TEXTFILE_CRWN, 0);
        }

        /* Winner crown */
        else if (type == TEXTFILE_CRWN)
        {
            /* Show splash screen (MOTD) */
            show_splashscreen();

            /* Request gameplay */
            Send_play(4);
        }
    }
    else
    {
        /* Request continuation */
        Send_text_screen(type, off + i);
    }

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
        player->upkeep->redraw |= (PR_LAG);
    }

    return 1;
}


static int Receive_char_info_conn(void)
{
    int n;
    byte ch;
    byte mode, ridx, cidx, psex;
    static bool newchar = false;

    /* Clear any old info */
    mode = ridx = cidx = psex = 0;

    if ((n = Packet_scanf(&rbuf, "%b%b%b%b%b", &ch, &mode, &ridx, &cidx, &psex)) <= 0)
        return n;

    if (dump_only) return 1;

    /* No character */
    if (mode == 0)
    {
        /* We need to load the basic pref file for key mappings during character creation */
        process_pref_file("pref.prf", false, false);

        /* We need to load the "PLAYER.prf" pref file for birth options during character creation */
        process_pref_options();

        /* Roll up a new character */
        newchar = true;
        textui_do_birth();
        Send_char_info();
    }

    /* Set info */
    player->race = player_id2race(ridx);
    player->clazz = player_id2class(cidx);
    player->psex = psex;

    /* Set pointers */
    player->sex = &sex_info[player->psex];
    player_embody(player);

    /* Copy his name */
    my_strcpy(player->name, nick, sizeof(player->name));

    /* Hack -- assume ready */
    if (Setup.frames_per_second && mode)
        client_ready(newchar);

    return 1;
}


static int Receive_char_info(void)
{
    int n;
    byte ch;
    byte ridx, cidx, psex;
    bool update_body = true;

    /* Clear any old info */
    ridx = cidx = psex = 0;

    if ((n = Packet_scanf(&rbuf, "%b%b%b%b", &ch, &ridx, &cidx, &psex)) <= 0)
        return n;

    /* Only update body if race changed */
    if (player->race && ((unsigned int)ridx == player->race->ridx)) update_body = false;

    /* Set info */
    player->race = player_id2race(ridx);
    player->clazz = player_id2class(cidx);
    player->psex = psex;

    /* Set pointers */
    player->sex = &sex_info[player->psex];
    if (update_body)
    {
        /* Free old pointers, since body slots may have changed */
        mem_free(player->body.slots);
        for (n = 0; n < N_HISTORY_FLAGS; n++)
            mem_free(player->hist_flags[n]);

        /* Reallocate properly */
        player_embody(player);
    }

    /* Copy his name */
    my_strcpy(player->name, nick, sizeof(player->name));

    /* Redraw */
    player->upkeep->redraw |= (PR_MISC);

    return 1;
}


static int Receive_options(void)
{
    int n;
    byte ch;
    char force_descend, no_recall, no_artifacts, feelings, no_selling, start_kit, no_stores,
        no_ghost, fruit_bat;

    if ((n = Packet_scanf(&rbuf, "%b%c%c%c%c%c%c%c%c%c", &ch, &force_descend, &no_recall,
        &no_artifacts, &feelings, &no_selling, &start_kit, &no_stores, &no_ghost,
        &fruit_bat)) <= 0)
    {
        return n;
    }

    OPT(player, birth_force_descend) = force_descend;
    OPT(player, birth_no_recall) = no_recall;
    OPT(player, birth_no_artifacts) = no_artifacts;
    OPT(player, birth_feelings) = feelings;
    OPT(player, birth_no_selling) = no_selling;
    OPT(player, birth_start_kit) = start_kit;
    OPT(player, birth_no_stores) = no_stores;
    OPT(player, birth_no_ghost) = no_ghost;
    OPT(player, birth_fruit_bat) = fruit_bat;

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
    if (streq(buf, "BEGIN_NORMAL_DUMP") || streq(buf, "BEGIN_MANUAL_DUMP"))
    {
        if (streq(buf, "BEGIN_MANUAL_DUMP")) dump_only = true;

        /* Open a temporary file */
        path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "@@tmp@@.txt");
        fp = file_open(buf, MODE_WRITE, FTYPE_TEXT);
    }

    /* End receiving */
    else if (streq(buf, "END_NORMAL_DUMP") || streq(buf, "END_MANUAL_DUMP"))
    {
        char tmp[MSG_LEN];
        char fname[NORMAL_WID];

        /* Access the temporary file */
        path_build(tmp, sizeof(tmp), ANGBAND_DIR_USER, "@@tmp@@.txt");

        file_close(fp);
        strnfmt(fname, sizeof(fname), "%s.txt", nick);

        if (get_file(fname, buf, sizeof(buf)))
        {
            c_msg_print(NULL);

            /* Just rename the temporary file */
            file_delete(buf);
            file_move(tmp, buf);

            c_msg_print("Character dump successful.");
        }
        else
            file_delete(tmp);

        if (dump_only)
        {
            ui_event ev;

            strnfmt(buf, NORMAL_WID - 2, "%.70s[y/n] ", "Start a new game? ");
            prt(buf, 0, 0);
            Term_fresh();
            Term_inkey(&ev, true, true);
            if ((ev.key.code != 'Y') && (ev.key.code != 'y')) quit(NULL);
            play_again = true;

            return -1;
        }
    }

    /* Process the line */
    else file_putf(fp, "%s\n", buf);

    return 1;
}


static int Receive_message(void)
{
    int n;
    size_t c;
    byte ch;
    u16b type = 0;
    char buf[MSG_LEN], search[MSG_LEN], *ptr;
    bool extended_char = false;

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
            extended_char = true;
        }
        else
            extended_char = false;
    }

    /* Hack -- repeated message */
    if ((buf[0] == ' ') && (buf[1] == '\0'))
        my_strcpy(buf, message_last(), sizeof(buf));

    strnfmt(search, sizeof(search), "%s] ", nick);

    if (strstr(buf, search) != 0)
    {
        ptr = strstr(talk_pend, strchr(buf, ']') + 2);
        if (ptr) my_strcpy(talk_pend, ptr, sizeof(talk_pend));
        else my_strcpy(talk_pend, "", sizeof(talk_pend));
    }

    if (!topline_icky && (party_mode || store_ctx || !player->screen_save_depth))
        c_msg_print_aux(buf, type);
    else
        message_add(buf, type);

    /* Always display chat messages */
    player->upkeep->redraw |= PR_MESSAGE_CHAT;

    /* Hack -- highlight chat tabs messages */
    if (type >= MSG_CHAT)
    {
        for (n = 0; n < MAX_CHANNELS; n++)
        {
            if (!STRZERO(channels[n].name) && channels[n].id == type - MSG_CHAT)
            {
                if (n != view_channel) player->on_channel[n] = 1;
            }
        }
    }
    if (type == MSG_WHISPER)
    {
        n = find_whisper_tab(buf, search, sizeof(search));
        if (n && n != view_channel) player->on_channel[n] = 1;
    }

    return 1;
}


static int Receive_item(void)
{
    int n, bytes_read;
    byte ch, equipped, notice, attr, act, aim, fuel, fail, stuck, known, known_effect, sellable,
        quality_ignore, ignored, magic, throwable;
    u16b tval, sval;
    s16b wgt, amt, oidx, slot, eidx, bidx;
    s32b price, pval;
    quark_t note;
    char name[NORMAL_WID];
    char name_terse[NORMAL_WID];
    char name_base[NORMAL_WID];
    char name_curse[NORMAL_WID];
    char name_power[NORMAL_WID];

    /* Packet and base info */
    if ((n = Packet_scanf(&rbuf, "%b%hu%b", &ch, &tval, &equipped)) <= 0)
        return n;
    bytes_read = 4;

    /* Object info */
    if ((n = Packet_scanf(&rbuf, "%hu%hd%hd%ld%lu%ld%b%hd", &sval, &wgt, &amt, &price, &note, &pval,
        &notice, &oidx)) <= 0)
    {
        /* Rollback the socket buffer */
        Sockbuf_rollback(&rbuf, bytes_read);

        /* Packet isn't complete, graceful failure */
        return n;
    }
    bytes_read += 21;

    /* Extra info */
    if ((n = Packet_scanf(&rbuf, "%b%b%b%b%b%hd%b%b%b%b%b%b%hd%b%hd%b", &attr, &act, &aim, &fuel,
        &fail, &slot, &stuck, &known, &known_effect, &sellable, &quality_ignore, &ignored, &eidx,
        &magic, &bidx, &throwable)) <= 0)
    {
        /* Rollback the socket buffer */
        Sockbuf_rollback(&rbuf, bytes_read);

        /* Packet isn't complete, graceful failure */
        return n;
    }
    bytes_read += 19;

    /* Descriptions */
    if ((n = Packet_scanf(&rbuf, "%s%s%s%s%s", name, name_terse, name_base, name_curse,
        name_power)) <= 0)
    {
        /* Rollback the socket buffer */
        Sockbuf_rollback(&rbuf, bytes_read);

        /* Packet isn't complete, graceful failure */
        return n;
    }

    /* Clear */
    if (!tval)
    {
        int i, size = z_info->pack_size + player->body.count + z_info->quiver_size;

        if (!player->gear)
            player->gear = mem_zalloc(size * sizeof(struct object));
        else
        {
            for (i = 0; i < size; i++)
            {
                struct object *obj = &player->gear[i];

                if (equipped == obj->info_xtra.equipped)
                    memset(obj, 0, sizeof(struct object));
            }
        }
    }

    /* Add the item */
    else
    {
        struct object *obj = &player->gear[oidx];

        obj->tval = tval;
        obj->sval = sval;
        obj->weight = wgt;
        obj->number = amt;
        obj->askprice = price;
        obj->kind = (tval? lookup_kind(tval, sval): NULL);
        obj->note = note;
        obj->pval = pval;
        obj->notice = notice;
        obj->oidx = oidx;

        obj->info_xtra.attr = attr;
        obj->info_xtra.act = act;
        obj->info_xtra.aim = aim;
        obj->info_xtra.fuel = fuel;
        obj->info_xtra.fail = fail;
        obj->info_xtra.slot = slot;
        obj->info_xtra.stuck = stuck;
        obj->info_xtra.known = known;
        obj->info_xtra.known_effect = known_effect;
        obj->info_xtra.sellable = sellable;
        obj->info_xtra.quality_ignore = quality_ignore;
        obj->info_xtra.ignored = ignored;
        obj->info_xtra.eidx = eidx;
        obj->info_xtra.equipped = equipped;
        obj->info_xtra.bidx = bidx;
        obj->info_xtra.throwable = throwable;

        my_strcpy(obj->info_xtra.name, name, sizeof(obj->info_xtra.name));
        my_strcpy(obj->info_xtra.name_terse, name_terse, sizeof(obj->info_xtra.name_terse));
        my_strcpy(obj->info_xtra.name_base, name_base, sizeof(obj->info_xtra.name_base));
        my_strcpy(obj->info_xtra.name_curse, name_curse, sizeof(obj->info_xtra.name_curse));
        my_strcpy(obj->info_xtra.name_power, name_power, sizeof(obj->info_xtra.name_power));
    }

    /* Redraw */
    player->upkeep->redraw |= (equipped? PR_EQUIP: PR_INVEN);

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
        Term_putstr(0, 13, -1, COLOUR_WHITE, party_info);
        Term_putstr(0, 11, -1, COLOUR_WHITE, "Command: ");
    }

    return 1;
}


static int Receive_special_line(void)
{
    int n;
    size_t i;
    s16b max, last, line;
    char buf[NORMAL_WID];
    byte r, ch, attr;
    int max_hgt = Client_setup.settings[SETTING_MAX_HGT];

    if ((n = Packet_scanf(&rbuf, "%b%hd%hd%hd%b%s", &ch, &max, &last, &line, &attr, buf)) <= 0)
        return n;

    /* Remote term */
    r = player->remote_term;

    /* Hack -- COLOUR_DARK means that we indent the line, print a symbol and then a string */
    if (attr == COLOUR_DARK)
    {
        byte a = (byte)buf[0];

        /* Put some blank characters first */
        for (i = 0; i < 5; i++)
        {
            remote_info[r][line][i].a = COLOUR_WHITE;
            remote_info[r][line][i].c = ' ';
        }

        /* Add symbol */
        remote_info[r][line][5].a = ((a & 0x80)? a: color_char_to_attr(a));
        remote_info[r][line][5].c = buf[1];

        remote_info[r][line][6].a = COLOUR_WHITE;
        remote_info[r][line][6].c = ' ';

        /* Copy the rest */
        cavestr(remote_info[r][line] + 7, buf + 3, color_char_to_attr(buf[2]), NORMAL_WID);
    }

    /* Hack -- COLOUR_SPECIAL means that we extract attr/char directly */
    else if (attr == COLOUR_SPECIAL)
    {
        /* Erase */
        for (i = 0; i < NORMAL_WID; i++)
        {
            remote_info[r][line][i].a = COLOUR_WHITE;
            remote_info[r][line][i].c = ' ';
        }

        /* Copy */
        for (i = 0; i < strlen(buf); i++)
        {
            if ((i % 2) == 0)
                remote_info[r][line][i / 2].a = (u16b)buf[i];
            else
                remote_info[r][line][i / 2].c = buf[i];
        }
    }

    /* Hack -- COLOUR_SYMBOL means that we have a symbol as first character */
    else if (attr == COLOUR_SYMBOL)
    {
        /* Add symbol */
        remote_info[r][line][0].a = (u16b)buf[0];
        remote_info[r][line][0].c = buf[1];

        /* Copy the rest */
        cavestr(remote_info[r][line] + 1, buf + 3, (byte)buf[2], NORMAL_WID);
    }

    /* Copy to local buffer */
    else
        cavestr(remote_info[r][line], buf, attr, NORMAL_WID);

    last_remote_line[r] = last;

    /* Redraw */
    if (r == NTERM_WIN_SPECIAL) player->upkeep->redraw |= PR_SPECIAL_INFO;

    /* Maximum */
    max_line = max;

    if (!player->screen_save_depth) return 1;

    /* Hack -- decide to go popup/fullon mode */
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
            prt(format("[%s] %60s", version_build(VERSION_NAME, false),
                special_line_header[NTERM_WIN_OVERHEAD]), 0, 0);

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
            c_put_str(COLOUR_YELLOW, special_line_header[NTERM_WIN_OVERHEAD], 0, 0);

            /* Prompt */
            c_put_str(COLOUR_L_BLUE, "[Press any key to continue]", max_line + 4, 0);
        }
    }

    /* Print out the info */
    if ((attr == COLOUR_DARK) || (attr == COLOUR_SPECIAL) || (attr == COLOUR_SYMBOL))
        caveprt(remote_info[r][line], NORMAL_WID, 0, line + 2);
    else
        c_put_str(attr, buf, line + 2, 0);

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
    for (x = 0; x < Setup.max_col; x++)
    {
        player->trn_info[y][x].c = 0;
        player->trn_info[y][x].a = 0;
    }

    /* Decode the secondary attr/char stream */
    if (use_graphics)
    {
        n = rle_decode(&rbuf, player->trn_info[y], Setup.max_col, RLE_LARGE, &bytes_read);
        if (n <= 0) return n;
    }

    /* Decode the attr/char stream */
    n = rle_decode(&rbuf, player->scr_info[y], Setup.max_col, RLE_LARGE, &bytes_read);
    if (n <= 0) return n;

    return 1;
}


static int Receive_poly(void)
{
    int n;
    byte ch;
    s16b race;

    if ((n = Packet_scanf(&rbuf, "%b%hd", &ch, &race)) <= 0)
        return n;

    player->poly_race = (race? &r_info[race]: NULL);

    /* Redraw */
    player->upkeep->redraw |= (PR_OTHER);

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


static int Receive_store_leave(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    /* Get out of the store! */
    if (store_ctx) store_leave();

    return 1;
}


static int Receive_store_confirm(void)
{
    byte ch;
    int n;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;

    /* Finish the selling process */
    store_sell_end();

    return 1;
}


static int Receive_ignore(void)
{
    int n, i, j, k;
    byte ch;
    byte setting;
    int bytes_read;

    if ((n = Packet_scanf(&rbuf, "%b", &ch)) <= 0)
        return n;
    bytes_read = 1;

    /* Flavour-aware ignoring */
    for (i = 0; i < z_info->k_max; i++)
    {
        if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read++;

        player->kind_ignore[i] = setting;
    }

    /* Ego ignoring */
    i = 0;
    j = 0;
    while (i < z_info->e_max)
    {
        s16b repeat;
        byte last;

        if ((n = Packet_scanf(&rbuf, "%hd%b", &repeat, &last)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read += 3;

        for (k = 0; k < repeat; k++)
        {
            player->ego_ignore_types[i][j] = last;
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
        if ((n = Packet_scanf(&rbuf, "%b", &setting)) <= 0)
        {
            /* Rollback the socket buffer */
            Sockbuf_rollback(&rbuf, bytes_read);

            /* Packet isn't complete, graceful failure */
            return n;
        }
        bytes_read++;

        player->opts.ignore_lvl[i] = setting;
    }

    return 1;
}


static int Receive_flush(void)
{
    char fresh, delay;
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%c%c", &ch, &fresh, &delay)) <= 0)
        return n;

    /* Flush the terminal */
    if (fresh) Term_fresh();

    /* Wait */
    if (delay) Term_xtra(TERM_XTRA_DELAY, player->opts.delay_factor);

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

                if (player->main_channel == n)
                    player->main_channel = 0;
                if (STRZERO(channels[view_channel].name))
                    cmd_chat_cycle(+1);

                /* Redraw */
                player->upkeep->redraw |= PR_MESSAGE_CHAT;

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
                player->main_channel = view_channel = n;

                /* Redraw */
                player->upkeep->redraw |= PR_MESSAGE_CHAT;

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
        player->on_channel[n] = 1; */

        /* Redraw */
        player->upkeep->redraw |= PR_MESSAGE_CHAT;

        return 1;
    }

    plog("CLIENT ERROR! No space for new channel...");

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

    my_strcpy(player->history[line], buf, sizeof(player->history[0]));

    /* Redraw */
    player->upkeep->redraw |= (PR_OTHER);

    return 1;
}


static int Receive_autoinscriptions(void)
{
    int n;
    byte ch;
    u32b kidx;
    char note_aware[4];

    if ((n = Packet_scanf(&rbuf, "%b%lu%s", &ch, &kidx, note_aware)) <= 0)
        return n;

    my_strcpy(Client_setup.note_aware[kidx], note_aware, sizeof(Client_setup.note_aware[0]));

    return 1;
}


static int Receive_play_setup(void)
{
    int n;
    byte ch;

    if ((n = Packet_scanf(&rbuf, "%b%b", &ch, &chardump)) <= 0)
        return n;

    Send_play(1);

    return 1;
}


/*** Sending ***/


#define MAX_FEATURE_CHUNK   512

int Send_features(int lighting, int off)
{
    int n, i, size, max, offset = off;
    byte a;
    char c;

    /* Paranoia */
    if ((lighting < 0) || (lighting >= LIGHTING_MAX)) return 0;

    /* Size */
    size = z_info->f_max;

    max = MAX_FEATURE_CHUNK;
    if (offset + max > size) max = size - offset;
    if (offset > size) offset = size;

    if ((n = Packet_printf(&wbuf, "%b%c%hd%hd", (unsigned)PKT_FEATURES, lighting, max, offset)) <= 0)
        return n;

    /* Send attr/char stream */
    for (i = offset; i < offset + max; i++)
    {
        a = Client_setup.f_attr[i][lighting];
        c = Client_setup.f_char[i][lighting];

        if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)a, (int)c)) <= 0)
            return n;
    }

    return 1;
}


int Send_verify(int type)
{
    int n, i, size;
    byte a;
    char c;

    /* Size */
    switch (type)
    {
        case 0: size = flavor_max; break;
        case 1: size = z_info->k_max; break;
        case 2: size = z_info->r_max; break;
        case 3: size = PROJ_MAX * BOLT_MAX; break;
        case 4: size = z_info->trap_max * LIGHTING_MAX; break;
        default: return 0;
    }

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_VERIFY, type, size)) <= 0)
        return n;

    /* Send attr/char streams */
    for (i = 0; i < size; i++)
    {
        switch (type)
        {
            case 0:
                a = Client_setup.flvr_x_attr[i];
                c = Client_setup.flvr_x_char[i];
                break;
            case 1:
                a = Client_setup.k_attr[i];
                c = Client_setup.k_char[i];
                break;
            case 2:
                a = Client_setup.r_attr[i];
                c = Client_setup.r_char[i];
                break;
            case 3:
                a = Client_setup.proj_attr[i / BOLT_MAX][i % BOLT_MAX];
                c = Client_setup.proj_char[i / BOLT_MAX][i % BOLT_MAX];
                break;
            case 4:
                a = Client_setup.t_attr[i / LIGHTING_MAX][i % LIGHTING_MAX];
                c = Client_setup.t_char[i / LIGHTING_MAX][i % LIGHTING_MAX];
                break;
        }

        if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)a, (int)c)) <= 0)
            return n;
    }

    /* Hack -- flush the network output buffer */
    Net_flush();

    return 1;
}


int Send_icky(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_ICKY, player->screen_save_depth)) <= 0)
        return n;

    return 1;
}


int Send_symbol(const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%s", (unsigned)PKT_SYMBOL_QUERY, buf)) <= 0)
        return n;

    return 1;
}


int Send_poly_race(const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%s", (unsigned)PKT_POLY_RACE, buf)) <= 0)
        return n;

    return 1;
}


int Send_breath(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_BREATH, dir)) <= 0)
        return n;

    return 1;
}


int Send_walk(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_WALK, dir)) <= 0)
        return n;

    return 1;
}


int Send_run(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_RUN, dir)) <= 0)
        return n;

    return 1;
}


int Send_tunnel(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;
    byte starting = 1;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c%b", (unsigned)PKT_TUNNEL, dir, (unsigned)starting)) <= 0)
        return n;

    return 1;
}


int Send_aim(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Aim which wand? ",
        /* Error */ "You have no wands to aim.",
        /* Filter */ obj_is_wand,
        /* Choice */ USE_INVEN | USE_FLOOR | SHOW_FAIL);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_AIM_WAND, obj->oidx, dir)) <= 0)
        return n;

    return 1;
}


int Send_drop(struct command *cmd)
{
    int n;
    int amt;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Drop which item? ",
        /* Error */ "You have nothing to drop.",
        /* Filter */ NULL,
        /* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER) != CMD_OK)
    {
        return 0;
    }

    if (cmd_get_quantity(cmd, "quantity", &amt, obj->number, true) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_DROP, obj->oidx, amt)) <= 0)
        return n;

    return 1;
}


int Send_fire(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Fire which ammunition? ",
        /* Error */ "You have no ammunition to fire.",
        /* Filter */ item_tester_hook_fire,
        /* Choice */ USE_INVEN | USE_QUIVER | USE_FLOOR | QUIVER_TAGS) != CMD_OK)
    {
        return 0;
    }

    if (cmd_get_target(cmd, "target", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_FIRE, dir, obj->oidx)) <= 0)
        return n;

    return 1;
}


int Send_pickup(struct command *cmd)
{
    int n;
    int number = 1, item = 0;
    struct object *obj = NULL;

    cmd_get_arg_number(cmd, "number", &number);
    cmd_get_arg_item(cmd, "item", &obj);

    if (obj) item = obj->oidx;

    if ((n = Packet_printf(&wbuf, "%b%b%hd", (unsigned)PKT_PICKUP, (unsigned)number, item)) <= 0)
        return n;

    return 1;
}


int Send_autopickup(struct command *cmd)
{
    int n;
    int number = 2, item = 0;

    if ((n = Packet_printf(&wbuf, "%b%b%hd", (unsigned)PKT_PICKUP, (unsigned)number, item)) <= 0)
        return n;

    return 1;
}


int Send_hold(struct command *cmd)
{
    int n;
    int number = 0, item = 0;

    if ((n = Packet_printf(&wbuf, "%b%b%hd", (unsigned)PKT_PICKUP, (unsigned)number, item)) <= 0)
        return n;

    return 1;
}


int Send_destroy(struct object *obj, bool des)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_DESTROY, obj->oidx, (int)des)) <= 0)
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


int Send_cast(int book, int spell, int dir)
{
    int n;
    byte starting = 1;

    n = Packet_printf(&wbuf, "%b%hd%hd%c%b", (unsigned)PKT_SPELL, book, spell, dir,
        (unsigned)starting);
    if (n <= 0) return n;

    return 1;
}


int Send_open(struct command *cmd)
{
    int n;
    int dir = DIR_SKIP;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_OPEN, dir)) <= 0)
        return n;

    return 1;
}


int Send_quaff(struct command *cmd)
{
    int n;
    int dir;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Quaff which potion? ",
        /* Error */ "You have no potions from which to quaff.",
        /* Filter */ obj_is_potion,
        /* Choice */ USE_INVEN | USE_FLOOR);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    /* Hack -- potions of Dragon Breath can be aimed when aware */
    dir = need_dir(obj);

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_QUAFF, obj->oidx, dir)) <= 0)
        return n;

    return 1;
}


int Send_read(struct command *cmd)
{
    int n;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Read which scroll? ",
        /* Error */ "You have no scrolls to read.",
        /* Filter */ obj_is_scroll,
        /* Choice */ USE_INVEN | USE_FLOOR);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_READ, obj->oidx)) <= 0)
        return n;

    return 1;
}


int Send_take_off(struct command *cmd)
{
    int n;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Take off or unwield which item? ",
        /* Error */ "You have nothing to take off or unwield.",
        /* Filter */ obj_can_takeoff,
        /* Choice */ USE_EQUIP) != CMD_OK)
    {
        return 0;
    }

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_TAKE_OFF, obj->oidx)) <= 0)
        return n;

    return 1;
}


int Send_use(struct command *cmd)
{
    int n;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Use which staff? ",
        /* Error */ "You have no staves to use.",
        /* Filter */ obj_is_staff,
        /* Choice */ USE_INVEN | USE_FLOOR | SHOW_FAIL);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_USE, obj->oidx)) <= 0)
        return n;

    return 1;
}


int Send_throw(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Throw which item? ",
        /* Error */ "You have nothing to throw.",
        /* Filter */ NULL,
        /* Choice */ USE_QUIVER | USE_INVEN | USE_FLOOR | QUIVER_TAGS | START_INVEN | SHOW_THROWING) != CMD_OK)
    {
        return 0;
    }

    /* Make sure the player isn't throwing wielded items */
    if (object_is_equipped(player->body, obj))
    {
        c_msg_print("You cannot throw wielded items.");
        return 0;
    }

    if (cmd_get_target(cmd, "target", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_THROW, dir, obj->oidx)) <= 0)
        return n;

    return 1;
}


static bool obj_is_ring(struct player *p, const struct object *obj)
{
    return tval_is_ring(obj);
}


/*
 * Check if the object is inscribed @0 or @w0.
 */
static bool has_swap_tag(int slot)
{
    const char *s;
    char *buf = player->body.slots[slot].obj->info_xtra.name;
    char *buf2;

    /* Skip empty objects */
    if (!buf[0]) return false;

    /* Skip empty inscriptions */
    buf2 = strchr(buf, '{');
    if (!buf2) return false;

    /* Find a '@' */
    s = strchr(buf2, '@');

    /* Process all tags */
    while (s)
    {
        /* Check the normal tags */
        if (s[1] == '0')
        {
            /* Success */
            return true;
        }

        /* Check the special tags */
        if ((s[1] == 'w') && (s[2] == '0'))
        {
            /* Success */
            return true;
        }

        /* Find another '@' */
        s = strchr(s + 1, '@');
    }

    /* No such tag */
    return false;
}


int Send_wield(struct command *cmd)
{
    int n;
    struct object *equip_obj;
    int slot;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Wear or wield which item? ",
        /* Error */ "You have nothing to wear or wield.",
        /* Filter */ obj_can_wear,
        /* Choice */ USE_INVEN | USE_FLOOR);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    /* Get the slot the object wants to go in, and the item currently there */
    slot = obj->info_xtra.slot;
    equip_obj = slot_object(player, slot);

    /*
     * Usually if the slot is taken we'll just replace the item in the slot,
     * but in some cases we need to ask the user which slot they actually
     * want to replace
     */
    if (equip_obj && tval_is_ring(obj))
    {
        /* Look up the tag */
        if (has_swap_tag(slot + 1)) slot++;
        else if (!has_swap_tag(slot))
        {
            const char *q = "Replace which ring? ";
            const char *s = "Error in Send_wield(), please report";

            if (!get_item(&equip_obj, q, s, CMD_WIELD, obj_is_ring, USE_EQUIP)) return 0;

            /* Change slot if necessary */
            slot = equip_obj->info_xtra.slot;
        }
    }

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_WIELD, obj->oidx, slot)) <= 0)
        return n;

    return 1;
}


int Send_zap(struct command *cmd)
{
    int n;
    int dir;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Zap which rod? ",
        /* Error */ "You have no rods to zap.",
        /* Filter */ obj_is_rod,
        /* Choice */ USE_INVEN | USE_FLOOR | SHOW_FAIL);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    dir = need_dir(obj);

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_ZAP, obj->oidx, dir)) <= 0)
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


int Send_inscribe(struct command *cmd)
{
    int n;
    struct object *obj;
    const char *str;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Inscribe which item? ",
        /* Error */ "You have nothing to inscribe.",
        /* Filter */ NULL,
        /* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR) != CMD_OK)
    {
        return 0;
    }

    /* Form prompt */
    if (cmd_get_string(cmd, "inscription", &str, NULL, NULL, "Inscribe with what? ") != CMD_OK)
        return 0;

    n = Packet_printf(&wbuf, "%b%hd%s", (unsigned)PKT_INSCRIBE, obj->oidx, str);
    string_free((char*)str);

    if (n <= 0) return n;
    return 1;
}


int Send_uninscribe(struct command *cmd)
{
    int n;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Uninscribe which item? ",
        /* Error */ "You have nothing you can uninscribe.",
        /* Filter */ obj_has_inscrip,
        /* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR) != CMD_OK)
    {
        return 0;
    }

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_UNINSCRIBE, obj->oidx)) <= 0)
        return n;

    return 1;
}


int Send_activate(struct command *cmd)
{
    int n;
    int dir;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Activate which item? ",
        /* Error */ "You have no items to activate.",
        /* Filter */ obj_is_activatable,
        /* Choice */ USE_EQUIP | SHOW_FAIL);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    dir = need_dir(obj);

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_ACTIVATE, obj->oidx, dir)) <= 0)
        return n;

    return 1;
}


int Send_disarm(struct command *cmd)
{
    int n;
    int dir = DIR_SKIP;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_DISARM, dir)) <= 0)
        return n;

    return 1;
}


int Send_eat(struct command *cmd)
{
    int n;
    struct object *obj;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Eat which food? ",
        /* Error */ "You have no food to eat.",
        /* Filter */ obj_is_food,
        /* Choice */ USE_INVEN | USE_FLOOR);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_EAT, obj->oidx)) <= 0)
        return n;

    return 1;
}


int Send_fill(struct command *cmd)
{
    int n;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Refuel with with fuel source? ",
        /* Error */ "You have nothing you can refuel with.",
        /* Filter */ obj_can_refill,
        /* Choice */ USE_INVEN | USE_FLOOR) != CMD_OK)
    {
        return 0;
    }

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_FILL, obj->oidx)) <= 0)
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


int Send_map(byte mode)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%b", (unsigned)PKT_MAP, (unsigned)mode)) <= 0)
        return n;

    return 1;
}


int Send_toggle_stealth(struct command *cmd)
{
    int n;

    /* Non rogues */
    if (!player_has(player, PF_STEALTH_MODE))
    {
        c_msg_print("You are not skilled enough.");
        return 0;
    }

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_STEALTH_MODE)) <= 0)
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


int Send_close(struct command *cmd)
{
    int n;
    int dir = DIR_SKIP;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_CLOSE, dir)) <= 0)
        return n;

    return 1;
}


int Send_gain(struct command *cmd)
{
    int n;
    struct object *book_obj;
    int book;

    /* Learn random spell */
    int spell_index = -2;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &book_obj,
        /* Prompt */ "Study which book? ",
        /* Error */ "You cannot learn any new spells from the books you have.",
        /* Filter */ obj_can_study,
        /* Choice */ USE_INVEN | USE_FLOOR | BOOK_TAGS) != CMD_OK)
    {
        return 0;
    }

    book = book_obj->info_xtra.bidx;
    Send_track_object(book_obj->oidx);

    /* Learn a selected spell */
    if (player_has(player, PF_CHOOSE_SPELLS))
        spell_index = get_spell(book, "study", spell_okay_to_study);

    if (spell_index == -1) return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_GAIN, book_obj->oidx, spell_index)) <= 0)
        return n;

    return 1;
}


int Send_go_up(struct command *cmd)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_GO_UP)) <= 0)
        return n;

    return 1;
}


int Send_go_down(struct command *cmd)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_GO_DOWN)) <= 0)
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


int Send_redraw(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_REDRAW)) <= 0)
        return n;

    /* Hack -- clear the screen */
    Term_clear();

    return 1;
}


int Send_rest(s16b resting)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_REST, (int)resting)) <= 0)
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


int Send_suicide(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_SUICIDE)) <= 0)
        return n;

    return 1;
}


int Send_steal(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_STEAL, dir)) <= 0)
        return n;

    return 1;
}


int Send_master(s16b command, const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%s", (unsigned)PKT_MASTER, (int)command, buf)) <= 0)
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


int Send_clear(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_CLEAR)) <= 0)
        return n;

    return 1;
}


int Send_observe(struct command *cmd)
{
    int n;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Examine which item? ",
        /* Error */ "You have no items you can examine.",
        /* Filter */ NULL,
        /* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR) != CMD_OK)
    {
        return 0;
    }

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_OBSERVE, obj->oidx)) <= 0)
        return n;

    return 1;
}


int Send_store_examine(int item, bool describe)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%b", (unsigned)PKT_STORE_EXAMINE, item,
        (unsigned)describe)) <= 0)
    {
        return n;
    }

    return 1;
}


int Send_alter(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_ALTER, dir)) <= 0)
        return n;

    return 1;
}


int Send_fire_at_nearest(void)
{
    int n;
    byte starting = 1;

    if ((n = Packet_printf(&wbuf, "%b%b", (unsigned)PKT_FIRE_AT_NEAREST, (unsigned)starting)) <= 0)
        return n;

    return 1;
}


int Send_jump(struct command *cmd)
{
    int n;
    int dir = DIR_UNKNOWN;

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%c", (unsigned)PKT_JUMP, dir)) <= 0)
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


int Send_monlist(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_MONLIST)) <= 0)
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


int Send_time(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_TIME)) <= 0)
        return n;

    return 1;
}


int Send_objlist(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_OBJLIST)) <= 0)
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


int Send_toggle_ignore(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_TOGGLE_IGNORE)) <= 0)
        return n;

    return 1;
}


int Send_use_any(struct command *cmd)
{
    int n;
    int dir;
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Use which item? ",
        /* Error */ "You have no items to use.",
        /* Filter */ obj_is_useable,
        /* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR | START_INVEN | SHOW_FAIL | QUIVER_TAGS) != CMD_OK)
    {
        return 0;
    }

    dir = need_dir(obj);

    if (cmd_get_target(cmd, "direction", &dir) != CMD_OK)
        return 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%c", (unsigned)PKT_USE_ANY, obj->oidx, dir)) <= 0)
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


int Send_track_object(int item)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_TRACK_OBJECT, item)) <= 0)
        return n;

    return 1;
}


int Send_floor_ack(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_FLOOR_ACK)) <= 0)
        return n;

    return 1;
}


int Send_play(int phase)
{
    int n;

    /* Send marker */
    n = Packet_printf(&wbuf, "%b%b", (unsigned)PKT_PLAY, (unsigned)phase);
    if (n <= 0) return n;

    /* Send nick/pass */
    if (phase == 0)
    {
        int pos = strlen(nick);

        dump_only = false;
        chardump = 0;

        n = Packet_printf(&wbuf, "%s%s", nick, stored_pass);
        if (n <= 0) return n;

        if (nick[pos - 1] == '=') nick[pos - 1] = '\0';
        else if (nick[pos - 1] == '-') nick[pos - 1] = '\0';
        else if (nick[pos - 1] == '+')
        {
            nick[pos - 1] = '\0';
            get_next_incarnation(nick, sizeof(nick));
        }
    }

    return 1;
}


int Send_text_screen(int type, s32b off)
{
    int n;
    s32b offset = off;

    if (offset < 0) offset = 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%ld", (unsigned)PKT_TEXT_SCREEN, type, offset)) <= 0)
        return n;

    return 1;
}


int Send_keepalive(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%lu", (unsigned)PKT_KEEPALIVE, last_sent)) <= 0)
        return n;

    return 1;
}


int Send_char_info(void)
{
    int n, i;
    unsigned int ridx = player->race? player->race->ridx: 0;
    unsigned int cidx = player->clazz? player->clazz->cidx: 0;

    if ((n = Packet_printf(&wbuf, "%b%b%b%b", (unsigned)PKT_CHAR_INFO, (unsigned)ridx,
        (unsigned)cidx, (unsigned)player->psex)) <= 0)
    {
        return n;
    }

    /* Roller */
    for (i = 0; i <= STAT_MAX; i++)
    {
        n = Packet_printf(&wbuf, "%hd", (int)stat_roll[i]);
        if (n <= 0) return n;
    }

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
        n = Packet_printf(&wbuf, "%c", (int)player->opts.opt[i]);
        if (n <= 0)
            return n;
    }

    return 1;
}


int Send_char_dump(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_CHAR_DUMP)) <= 0)
        return n;

    return 1;
}


int Send_msg(const char *message)
{
    int n;

    if (message && strlen(message))
        my_strcpy(talk_pend, message, sizeof(talk_pend));

    if (view_channel != player->main_channel)
    {
        /* Change channel */
        player->main_channel = view_channel;
        if ((n = Send_chan(channels[view_channel].name)) <= 0)
            return n;
    }

    if ((n = Packet_printf(&wbuf, "%b%S", (unsigned)PKT_MESSAGE, talk_pend)) <= 0)
        return n;

    return 1;
}


int Send_item(int item, int curse, const char *inscription)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd%s", (unsigned)PKT_ITEM, item, curse, inscription)) <= 0)
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


int Send_party(s16b command, const char *buf)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%s", (unsigned)PKT_PARTY, command, buf)) <= 0)
        return n;

    return 1;
}


int Send_special_line(int type, int line)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%c%hd", (unsigned)PKT_SPECIAL_LINE, type, line)) <= 0)
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


int Send_poly(int number)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd", (unsigned)PKT_POLY, number)) <= 0)
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


int Send_purchase_house(int dir)
{
    int n, dummy = 0;

    if ((n = Packet_printf(&wbuf, "%b%hd%hd", (unsigned)PKT_PURCHASE, dir,
            dummy)) <= 0)
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


int Send_ignore(void)
{
    int i, j, n, repeat = 0;
    byte last = player->ego_ignore_types[0][0];

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_IGNORE)) <= 0)
        return n;

    /* Flavour-aware ignoring */
    for (i = 0; i < z_info->k_max; i++)
    {
        n = Packet_printf(&wbuf, "%b", (unsigned)player->kind_ignore[i]);
        if (n <= 0)
            return n;
    }

    /* Ego ignoring */
    for (i = 0; i < z_info->e_max; i++)
    {
        for (j = ITYPE_NONE; j < ITYPE_MAX; j++)
        {
            if (player->ego_ignore_types[i][j] == last)
                repeat++;
            else
            {
                n = Packet_printf(&wbuf, "%hd%b", (int)repeat, (unsigned)last);
                if (n <= 0)
                    return n;
                repeat = 1;
                last = player->ego_ignore_types[i][j];
            }
        }
    }
    n = Packet_printf(&wbuf, "%hd%b", (int)repeat, (unsigned)last);
    if (n <= 0)
        return n;

    /* Quality ignoring */
    for (i = ITYPE_NONE; i < ITYPE_MAX; i++)
    {
        n = Packet_printf(&wbuf, "%b", (unsigned)player->opts.ignore_lvl[i]);
        if (n <= 0)
            return n;
    }

    return 1;
}


int Send_flush(void)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_FLUSH)) <= 0)
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


int Send_history(int line, const char *hist)
{
    int n;

    if ((n = Packet_printf(&wbuf, "%b%hd%s", (unsigned)PKT_HISTORY, line, hist)) <= 0)
        return n;

    return 1;
}


int Send_autoinscriptions(void)
{
    int i, n;

    if ((n = Packet_printf(&wbuf, "%b", (unsigned)PKT_AUTOINSCR)) <= 0)
        return n;

    for (i = 0; i < z_info->k_max; i++)
    {
        n = Packet_printf(&wbuf, "%s", Client_setup.note_aware[i]);
        if (n <= 0)
            return n;
    }

    return 1;
}


/*** Commands ***/


/*
 * Browse the given book
 */
int textui_spell_browse(struct command *cmd)
{
    struct object *obj;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &obj,
        /* Prompt */ "Browse which book? ",
        /* Error */ "You have no book you can browse.",
        /* Filter */ obj_can_browse,
        /* Choice */ USE_INVEN | USE_FLOOR | BOOK_TAGS) != CMD_OK)
    {
        return 0;
    }

    /* Track the object kind */
    Send_track_object(obj->oidx);

    textui_book_browse(obj->info_xtra.bidx);
    return 1;
}


int cmd_cast(struct command *cmd)
{
    int n;
    struct object *book;
    int dir = 0;
    int spell;

    /* Hack -- don't get out of icky screen if disturbed */
    allow_disturb_icky = false;

    /* Get arguments */
    n = cmd_get_item(cmd, "item", &book,
        /* Prompt */ "Use which book? ",
        /* Error */ "You have no books you can use.",
        /* Filter */ obj_can_cast_from,
        /* Choice */ USE_INVEN | USE_FLOOR | BOOK_TAGS);

    allow_disturb_icky = true;
    if (n != CMD_OK) return 0;

    /* Track the object kind */
    Send_track_object(book->oidx);

    spell = textui_obj_cast(book->info_xtra.bidx, &dir);
    if (spell != -1) Send_cast(book->oidx, spell, dir);
    return 1;
}


int cmd_project(struct command *cmd)
{
    struct object *book;
    int dir = 0;
    int spell;

    /* Get arguments */
    if (cmd_get_item(cmd, "item", &book,
        /* Prompt */ "Use which book? ",
        /* Error */ "You have no books you can use.",
        /* Filter */ obj_can_cast_from,
        /* Choice */ USE_INVEN | USE_FLOOR | BOOK_TAGS) != CMD_OK)
    {
        return 0;
    }

    /* Track the object kind */
    Send_track_object(book->oidx);

    spell = textui_obj_project(book->info_xtra.bidx, &dir);
    if (spell != -1) Send_cast(book->oidx, spell, dir);
    return 1;
}


/*** General network functions ***/


typedef int (*receive_handler_f)(void);


/* Setup receive methods */
static const receive_handler_f setup_receive[] =
{
    #define PKT(a, b, c, d, e) Receive_##d,
    #include "../common/list-packets.h"
    #undef PKT
    NULL
};


/* Playing receive methods */
static const receive_handler_f playing_receive[] =
{
    #define PKT(a, b, c, d, e) Receive_##e,
    #include "../common/list-packets.h"
    #undef PKT
    NULL
};


/*
 * Process a packet.
 */
int Net_packet(void)
{
    int result;
    const receive_handler_f *receive_tbl = &playing_receive[0];
    char *old_ptr;

    if (conn_state == CONN_SETUP) receive_tbl = &setup_receive[0];

    /* Process all of the received client updates */
    while (rbuf.buf + rbuf.len > rbuf.ptr)
    {
        cur_type = (*rbuf.ptr & 0xFF);

        /* Paranoia */
        if ((cur_type <= PKT_UNDEFINED) || (cur_type >= PKT_MAX)) cur_type = PKT_UNDEFINED;

        old_ptr = rbuf.ptr;
        if ((result = (*receive_tbl[cur_type])()) <= 0)
        {
            if (result == -1)
            {
                if ((cur_type != PKT_QUIT) && !play_again)
                {
                    errno = 0;
                    plog_fmt("Processing packet type (%d, %d) failed", cur_type, prev_type);
                }
                Sockbuf_clear(&rbuf);
                return -1;
            }
            if (result == -2)
            {
                receive_tbl = ((conn_state == CONN_PLAYING)? &playing_receive[0]: &setup_receive[0]);
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
        prev_type = cur_type;
    }
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
        return false;

    return true;
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

    if (retries >= max_retries) return false;

    return true;
}


