/*
 * File: sockbuf.c
 * Purpose: Socket buffer code
 *
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


#include "angband.h"


int Sockbuf_init(sockbuf_t *sbuf, int sock, int size, int state)
{
    sbuf->ptr = NULL;
    sbuf->buf = (char*)mem_alloc(size);
    if (sbuf->buf == NULL) return -1;

    sbuf->sock = sock;
    sbuf->state = state;
    sbuf->len = 0;
    sbuf->size = size;
    sbuf->ptr = sbuf->buf;
    sbuf->state = state;

    return 0;
}


int Sockbuf_cleanup(sockbuf_t *sbuf)
{
    mem_free(sbuf->buf);
    sbuf->buf = NULL;
    sbuf->ptr = NULL;
    sbuf->size = sbuf->len = 0;
    sbuf->state = 0;

    return 0;
}


int Sockbuf_clear(sockbuf_t *sbuf)
{
    sbuf->len = 0;
    sbuf->ptr = sbuf->buf;

    return 0;
}


int Sockbuf_advance(sockbuf_t *sbuf, int len)
{
    /* First do a few buffer consistency checks */
    if (sbuf->ptr > sbuf->buf + sbuf->len)
    {
        errno = 0;
        plog("Sockbuf pointer too far");
        sbuf->ptr = sbuf->buf + sbuf->len;
    }
    if (sbuf->ptr < sbuf->buf)
    {
        errno = 0;
        plog("Sockbuf pointer bad");
        sbuf->ptr = sbuf->buf;
    }
    if (sbuf->len > sbuf->size)
    {
        errno = 0;
        plog("Sockbuf len too far");
        sbuf->len = sbuf->size;
    }
    if (sbuf->len < 0)
    {
        errno = 0;
        plog("Sockbuf len bad");
        sbuf->len = 0;
    }
    if (len <= 0)
    {
        if (len < 0)
        {
            errno = 0;
            plog_fmt("Sockbuf advance negative (%d)", len);
        }
    }
    else if (len >= sbuf->len)
    {
        if (len > sbuf->len)
        {
            errno = 0;
            plog("Sockbuf advancing too far");
        }
        sbuf->len = 0;
        sbuf->ptr = sbuf->buf;
    }
    else
    {
#if !defined(bcopy)
        memmove(sbuf->buf, sbuf->buf + len, sbuf->len - len);
#else
        bcopy(sbuf->buf + len, sbuf->buf, sbuf->len - len);
#endif
        sbuf->len -= len;
        if (sbuf->ptr - sbuf->buf <= len)
            sbuf->ptr = sbuf->buf;
        else
            sbuf->ptr -= len;
    }

    return 0;
}


int Sockbuf_rollback(sockbuf_t *sbuf, int len)
{
    /* First do a few buffer consistency checks */
    if (sbuf->ptr < sbuf->buf)
    {
        errno = 0;
        plog("Sockbuf pointer bad");
        sbuf->ptr = sbuf->buf;
    }
    if (len > sbuf->ptr - sbuf->buf)
    {
        plog("Sockbuf rollback too big");
        len = sbuf->ptr - sbuf->buf;
    }
    if (len < 0)
    {
        errno = 0;
        plog_fmt("Sockbuf rollback negative (%d)", len);
    }
    else
        sbuf->ptr -= len;

    return 0;
}


int Sockbuf_flush(sockbuf_t *sbuf)
{
    int len, i;

    if (BIT(sbuf->state, SOCKBUF_WRITE) == 0)
    {
        errno = 0;
        plog("No flush on non-writable socket buffer");
        plog_fmt("(state=%02x, buf=%08x, ptr=%08x, size=%d, len=%d, sock=%d)",
            sbuf->state, sbuf->buf, sbuf->ptr, sbuf->size, sbuf->len, sbuf->sock);
        return -1;
    }
    if (BIT(sbuf->state, SOCKBUF_LOCK) != 0)
    {
        errno = 0;
        plog_fmt("No flush on locked socket buffer (0x%02x)", sbuf->state);
        return -1;
    }
    if (sbuf->len <= 0)
    {
        if (sbuf->len < 0)
        {
            errno = 0;
            plog("Write socket buffer length negative");
            sbuf->len = 0;
            sbuf->ptr = sbuf->buf;
        }
        return 0;
    }

    if (BIT(sbuf->state, SOCKBUF_DGRAM) != 0)
    {
        errno = 0;
        i = 0;
        while ((len = DgramWrite(sbuf->sock, sbuf->buf, sbuf->len)) <= 0)
        {
            if (len == 0 || errno == EWOULDBLOCK || errno == EAGAIN)
            {
                Sockbuf_clear(sbuf);
                return 0;
            }
            if (errno == EINTR)
            {
                errno = 0;
                continue;
            }
            if (++i > MAX_SOCKBUF_RETRIES)
            {
                plog_fmt("Can't send on socket (%d, %d)", sbuf->sock, sbuf->len);
                Sockbuf_clear(sbuf);
                return -1;
            }
            if (GetSocketError(sbuf->sock) == -1)
            {
                plog("GetSocketError send");
                return -1;
            }
            errno = 0;
        }
        if (len != sbuf->len)
        {
            errno = 0;
            plog_fmt("Can't write complete datagram (%d, %d)", len, sbuf->len);
        }
        Sockbuf_clear(sbuf);
    }
    else
    {
        errno = 0;
        while ((len = DgramWrite(sbuf->sock, sbuf->buf, sbuf->len)) <= 0)
        {
            if (errno == EINTR)
            {
                errno = 0;
                continue;
            }
            if (errno != EWOULDBLOCK && errno != EAGAIN)
            {
                plog("Can't write on socket");
                return -1;
            }
            return 0;
        }
        Sockbuf_advance(sbuf, len);
    }

    return len;
}


int Sockbuf_write(sockbuf_t *sbuf, char *buf, int len)
{
    if (BIT(sbuf->state, SOCKBUF_WRITE) == 0)
    {
        errno = 0;
        plog("No write to non-writable socket buffer");
        return -1;
    }
    if (sbuf->size - sbuf->len < len)
    {
        if (BIT(sbuf->state, SOCKBUF_LOCK | SOCKBUF_DGRAM) != 0)
        {
            errno = 0;
            plog_fmt("No write to locked socket buffer (%d, %d, %d, %d)",
                sbuf->state, sbuf->size, sbuf->len, len);
            return -1;
        }
        if (Sockbuf_flush(sbuf) == -1) return -1;
        if (sbuf->size - sbuf->len < len) return 0;
    }
    memcpy(sbuf->buf + sbuf->len, buf, len);
    sbuf->len += len;

    return len;
}


int Sockbuf_read(sockbuf_t *sbuf)
{
    int max, i, len;

    if (BIT(sbuf->state, SOCKBUF_READ) == 0)
    {
        errno = 0;
        plog_fmt("No read from non-readable socket buffer (%d)", sbuf->state);
        return -1;
    }
    if (BIT(sbuf->state, SOCKBUF_LOCK) != 0) return 0;
    if (sbuf->ptr > sbuf->buf) Sockbuf_advance(sbuf, sbuf->ptr - sbuf->buf);
    if ((max = sbuf->size - sbuf->len) <= 0)
    {
        static int before;
        if (before++ == 0)
        {
            errno = 0;
            plog_fmt("Read socket buffer not big enough (%d, %d)",
                sbuf->size, sbuf->len);
        }
        return -1;
    }
    if (BIT(sbuf->state, SOCKBUF_DGRAM) != 0)
    {
        errno = 0;
        i = 0;
        while ((len = DgramRead(sbuf->sock, sbuf->buf + sbuf->len, max)) <= 0)
        {
            if (len == 0) return 0;
            if (errno == EINTR)
            {
                errno = 0;
                continue;
            }
            if (errno == EWOULDBLOCK || errno == EAGAIN) return 0;
            if (++i > MAX_SOCKBUF_RETRIES)
            {
                plog("Can't recv on socket");
                return -1;
            }
            if (GetSocketError(sbuf->sock) == -1)
            {
                plog("GetSocketError recv");
                return -1;
            }
            errno = 0;
        }
        sbuf->len += len;
    }
    else
    {
        errno = 0;
        while ((len = DgramRead(sbuf->sock, sbuf->buf + sbuf->len, max)) <= 0)
        {
            if (len == 0) return 0;
            if (errno == EINTR)
            {
                errno = 0;
                continue;
            }
            if (errno != EWOULDBLOCK && errno != EAGAIN)
            {
                /* Give a different message for disconnected clients */
                if (errno != ECONNRESET)
                    plog_fmt("Can't read on socket: error %d", errno);
                else
                    plog("Disconnected from server...");
                return -1;
            }
            return 0;
        }
        sbuf->len += len;
    }

    return sbuf->len;
}


int Sockbuf_copy(sockbuf_t *dest, sockbuf_t *src, int len)
{
    if (len < dest->size - dest->len)
    {
        errno = 0;
        plog("Not enough room in destination copy socket buffer");
        return -1;
    }
    if (len < src->len)
    {
        errno = 0;
        plog("Not enough data in source copy socket buffer");
        return -1;
    }
    memcpy(dest->buf + dest->len, src->buf, len);
    dest->len += len;

    return len;
}


/*
 * Writes a packet to the socket
 *
 * %c  = char type passed as int
 * %b  = byte type passed as unsigned
 * %hd = s16b type passed as int
 * %hu = u16b type passed as unsigned
 * %ld = s32b type
 * %lu = u32b type
 * %s  = string type (<= 80 chars)
 * %S  = string type (> 80 chars)
 */
int Packet_printf(sockbuf_t *sbuf, char *fmt, ...)
{
#define PRINTF_FMT  1
#define PRINTF_IO   2
#define PRINTF_SIZE 3
    va_list ap;
    int i, failure = 0, count;
    char *end, *buf;

    va_start(ap, fmt);

    /* Mark the end of the available buffer space */
    end = sbuf->buf + sbuf->size;
    buf = sbuf->buf + sbuf->len;

    /* Parse the format string */
    for (i = 0; ((failure == 0) && (fmt[i] != '\0')); i++)
    {
        if (fmt[i] == '%')
        {
            switch (fmt[++i])
            {
                case 'c':
                {
                    char cval;

                    if (buf + 1 >= end)
                    {
                        failure = PRINTF_SIZE;
                        break;
                    }
                    cval = (char)(va_arg(ap, int));
                    *buf++ = cval;
                    break;
                }
                case 'b':
                {
                    byte bval;

                    if (buf + 1 >= end)
                    {
                        failure = PRINTF_SIZE;
                        break;
                    }
                    bval = (byte)(va_arg(ap, unsigned));
                    *buf++ = bval;
                    break;
                }
                case 'h':
                {
                    if (buf + 2 >= end)
                    {
                        failure = PRINTF_SIZE;
                        break;
                    }
                    switch (fmt[++i])
                    {
                        case 'd':
                        {
                            s16b sval;

                            sval = (s16b)(va_arg(ap, int));
                            *buf++ = sval >> 8;
                            *buf++ = sval;
                            break;
                        }
                        case 'u':
                        {
                            u16b usval;

                            usval = (u16b)(va_arg(ap, unsigned));
                            *buf++ = usval >> 8;
                            *buf++ = usval;
                            break;
                        }
                        default:
                            failure = PRINTF_FMT;
                            break;
                    }
                    break;
                }
                case 'l':
                {
                    if (buf + 4 >= end)
                    {
                        failure = PRINTF_SIZE;
                        break;
                    }
                    switch (fmt[++i])
                    {
                        case 'd':
                        {
                            s32b lval;

                            lval = va_arg(ap, s32b);
                            *buf++ = lval >> 24;
                            *buf++ = lval >> 16;
                            *buf++ = lval >> 8;
                            *buf++ = lval;
                            break;
                        }
                        case 'u':
                        {
                            u32b ulval;

                            ulval = va_arg(ap, u32b);
                            *buf++ = ulval >> 24;
                            *buf++ = ulval >> 16;
                            *buf++ = ulval >> 8;
                            *buf++ = ulval;
                            break;
                        }
                        default:
                            failure = PRINTF_FMT;
                            break;
                    }
                    break;
                }
                case 'S':   /* Big strings */
                case 's':   /* Small strings */
                {
                    int max_str_size;
                    char *str, *stop;

                    max_str_size = ((fmt[i] == 'S')? MSG_LEN: NORMAL_WID);
                    str = va_arg(ap, char*);
                    if (buf + max_str_size >= end)
                        stop = end;
                    else
                        stop = buf + max_str_size;

                    /* Send the nul byte too */
                    do
                    {
                        if (buf >= stop) break;
                    }
                    while ((*buf++ = *str++) != '\0');
                    if (buf > stop) failure = PRINTF_SIZE;
                    break;
                }
                default:
                {
                    failure = PRINTF_FMT;
                    break;
                }
            }
        }
        else
            failure = PRINTF_FMT;
    }
    if (failure != 0)
    {
        count = -1;
        if (failure == PRINTF_SIZE)
        {
            if (BIT(sbuf->state, SOCKBUF_DGRAM) != 0)
            {
                count = 0;
                failure = 0;
            }
        }
        else if (failure == PRINTF_FMT)
        {
            errno = 0;
            plog_fmt("Error in format string (\"%s\")", fmt);
        }
    }
    else
    {
        count = buf - (sbuf->buf + sbuf->len);
        sbuf->len += count;
    }

    va_end(ap);

    return count;
}


/*
 * Reads a packet from a socket
 *
 * %c  = char type
 * %b  = byte type
 * %hd = s16b type
 * %hu = u16b type
 * %ld = s32b type
 * %lu = u32b type
 * %s  = string type (<= 80 chars)
 * %S  = string type (> 80 chars)
 */
int Packet_scanf(sockbuf_t *sbuf, char *fmt, ...)
{
    va_list ap;
    int i, j, failure = 0, count = 0;

    va_start(ap, fmt);

    /* Parse the format string */
    for (i = j = 0; ((failure == 0) && (fmt[i] != '\0')); i++)
    {
        if (fmt[i] == '%')
        {
            count++;
            switch (fmt[++i])
            {
                case 'c':
                {
                    char *cptr;

                    if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 1])
                    {
                        if (BIT(sbuf->state, SOCKBUF_DGRAM | SOCKBUF_LOCK) != 0)
                        {
                            failure = 3;
                            break;
                        }
                        if (Sockbuf_read(sbuf) == -1)
                        {
                            failure = 2;
                            break;
                        }
                        if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 1])
                        {
                            failure = 3;
                            break;
                        }
                    }
                    cptr = va_arg(ap, char*);
                    *cptr = sbuf->ptr[j++];
                    break;
                }
                case 'b':
                {
                    byte *bptr;

                    if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 1])
                    {
                        if (BIT(sbuf->state, SOCKBUF_DGRAM | SOCKBUF_LOCK) != 0)
                        {
                            failure = 3;
                            break;
                        }
                        if (Sockbuf_read(sbuf) == -1)
                        {
                            failure = 2;
                            break;
                        }
                        if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 1])
                        {
                            failure = 3;
                            break;
                        }
                    }
                    bptr = va_arg(ap, byte*);
                    *bptr = (sbuf->ptr[j++] & 0xFF);
                    break;
                }
                case 'h':
                {
                    if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 2])
                    {
                        if (BIT(sbuf->state, SOCKBUF_DGRAM | SOCKBUF_LOCK) != 0)
                        {
                            failure = 3;
                            break;
                        }
                        if (Sockbuf_read(sbuf) == -1)
                        {
                            failure = 2;
                            break;
                        }
                        if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 2])
                        {
                            failure = 3;
                            break;
                        }
                    }
                    switch (fmt[++i])
                    {
                        case 'd':
                        {
                            s16b *sptr;

                            sptr = va_arg(ap, s16b*);
                            *sptr = sbuf->ptr[j++] << 8;
                            *sptr |= (sbuf->ptr[j++] & 0xFF);
                            break;
                        }
                        case 'u':
                        {
                            u16b *usptr;

                            usptr = va_arg(ap, u16b*);
                            *usptr = (sbuf->ptr[j++] & 0xFF) << 8;
                            *usptr |= (sbuf->ptr[j++] & 0xFF);
                            break;
                        }
                        default:
                            failure = 1;
                            break;
                    }
                    break;
                }
                case 'l':
                {
                    if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 4])
                    {
                        if (BIT(sbuf->state, SOCKBUF_DGRAM | SOCKBUF_LOCK) != 0)
                        {
                            failure = 3;
                            break;
                        }
                        if (Sockbuf_read(sbuf) == -1)
                        {
                            failure = 2;
                            break;
                        }
                        if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 4])
                        {
                            failure = 3;
                            break;
                        }
                    }
                    switch (fmt[++i])
                    {
                        case 'd':
                        {
                            s32b *lptr;

                            lptr = va_arg(ap, s32b*);
                            *lptr = sbuf->ptr[j++] << 24;
                            *lptr |= (sbuf->ptr[j++] & 0xFF) << 16;
                            *lptr |= (sbuf->ptr[j++] & 0xFF) << 8;
                            *lptr |= (sbuf->ptr[j++] & 0xFF);
                            break;
                        }
                        case 'u':
                        {
                            u32b *ulptr;

                            ulptr = va_arg(ap, u32b*);
                            *ulptr = (sbuf->ptr[j++] & 0xFF) << 24;
                            *ulptr |= (sbuf->ptr[j++] & 0xFF) << 16;
                            *ulptr |= (sbuf->ptr[j++] & 0xFF) << 8;
                            *ulptr |= (sbuf->ptr[j++] & 0xFF);
                            break;
                        }
                        default:
                            failure = 1;
                            break;
                    }
                    break;
                }
                case 'N':   /* Newline terminated string */
                case 'S':   /* Big strings */
                case 's':   /* Small strings */
                {
                    char terminator;
                    int k, max_str_size;
                    char *str;

                    terminator = '\0';
                    if (fmt[i] == 'N') terminator = '\n';
                    k = 0;
                    max_str_size = ((fmt[i] == 'S' || fmt[i] == 'N')? MSG_LEN: NORMAL_WID);
                    str = va_arg(ap, char*);

                    for (;;)
                    {
                        if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 1])
                        {
                            if (BIT(sbuf->state, SOCKBUF_DGRAM | SOCKBUF_LOCK) != 0)
                            {
                                failure = 3;
                                break;
                            }
                            if (Sockbuf_read(sbuf) == -1)
                            {
                                failure = 2;
                                break;
                            }
                            if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j + 1])
                            {
                                failure = 3;
                                break;
                            }
                        }
                        if ((str[k++] = sbuf->ptr[j++]) == terminator)
                        {
                            if (terminator != '\0') str[k - 1] = '\0';
                            break;
                        }
                        else if (k >= max_str_size)
                        {
                            /* Hack -- terminate our string and clear the sbuf */
                            str[k - 1] = '\0';
                            break;
                        }
                    }
                    if (failure != 0) my_strcpy(str, "ErRoR", 6);
                    break;
                }
                default:
                {
                    failure = 1;
                    break;
                }
            }
        }
        else
            failure = 1;
    }
    if (failure == 1)
    {
        errno = 0;
        plog_fmt("Error in format string (%s)", fmt);
    }
    else if (failure == 3)
    {
        /* Not enough input for one complete packet */
        count = 0;
        failure = 0;
    }
    else if (failure == 0)
    {
        if (&sbuf->buf[sbuf->len] < &sbuf->ptr[j])
        {
            errno = 0;
            plog_fmt("Input buffer exceeded (%s)", fmt);
            failure = 1;
        }
        else
            sbuf->ptr += j;
    }

    va_end(ap);

    return (failure? -1: count);
}
