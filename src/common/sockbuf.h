/*
 * File: sockbuf.h
 * Purpose: Socket buffer code
 */

#ifndef NET_H
#define NET_H

#define SERVER_RECV_SIZE    (32*1024)
#define SERVER_SEND_SIZE    (128*1024)

#define CLIENT_SEND_SIZE    (8*1024)
#define CLIENT_RECV_SIZE    SERVER_SEND_SIZE

/*
 * Definitions for the states a socket buffer can be in.
 */
#define SOCKBUF_READ        0x01    /* if readable */
#define SOCKBUF_WRITE       0x02    /* if writeable */
#define SOCKBUF_LOCK        0x04    /* if locked against kernel i/o */
#define SOCKBUF_ERROR       0x08    /* if i/o error occurred */
#define SOCKBUF_DGRAM       0x10    /* if datagram socket */

/*
 * Maximum number of socket i/o retries if datagram socket.
 */
#define MAX_SOCKBUF_RETRIES 2

/*
 * A buffer to reduce the number of system calls made and to reduce
 * the number of network packets.
 */
typedef struct
{
    int  sock;       /* socket filedescriptor */
    char *buf;       /* i/o data buffer */
    int  size;       /* size of buffer */
    int  len;        /* amount of data in buffer (writing/reading) */
    char *ptr;       /* current position in buffer (reading) */
    int  state;      /* read/write/locked/error status flags */
} sockbuf_t;

extern int Sockbuf_init(sockbuf_t *sbuf, int sock, int size, int state);
extern int Sockbuf_cleanup(sockbuf_t *sbuf);
extern int Sockbuf_clear(sockbuf_t *sbuf);
extern int Sockbuf_advance(sockbuf_t *sbuf, int len);
extern int Sockbuf_rollback(sockbuf_t *sbuf, int len);
extern int Sockbuf_flush(sockbuf_t *sbuf);
extern int Sockbuf_write(sockbuf_t *sbuf, char *buf, int len);
extern int Sockbuf_read(sockbuf_t *sbuf);
extern int Sockbuf_copy(sockbuf_t *dest, sockbuf_t *src, int len);

extern int Packet_printf(sockbuf_t *, char *fmt, ...);
extern int Packet_scanf(sockbuf_t *, char *fmt, ...);

#endif
