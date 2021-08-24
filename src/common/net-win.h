#ifdef WINDOWS
/*
 * File: net-win.h
 * Purpose: Network module
 */

#ifndef _SOCKLIB_INCLUDED
#define _SOCKLIB_INCLUDED

/* Error values and their meanings */
#define SL_ESOCKET      0   /* socket system call error */
#define SL_EBIND        1   /* bind system call error */
#define SL_ELISTEN      2   /* listen system call error */
#define SL_EHOSTNAME    3   /* Invalid host name format */
#define SL_ECONNECT     5   /* connect system call error */
#define SL_ESHUTD       6   /* shutdown system call error */
#define SL_ECLOSE       7   /* close system call error */
#define SL_EWRONGHOST   8   /* message arrived from unspec. host */
#define SL_ENORESP      9   /* No response */
#define SL_ERECEIVE     10  /* Receive error */

#include <winsock2.h>    /* includes netinet/in.h's sockaddr_in */

extern void SetTimeout(int, int);
extern int  GetPortNum(int);
extern int  SetSocketReceiveBufferSize(int, int);
extern int  SetSocketSendBufferSize(int, int);
extern int  SetSocketNonBlocking(int, int);
extern int  GetSocketError(int);
extern int  SocketReadable(int);
extern int  SocketClose(int fd);
extern int  CreateDgramSocket(int);
extern int  DgramSend(int, char *, int, char *, int);
extern int  DgramReceiveAny(int, char *, int);
extern int  DgramReply(int, char *, int);
extern int  DgramRead(int fd, char *rbuf, int size);
extern int  DgramWrite(int fd, char *wbuf, int size);
extern char *DgramLastname(void);
extern void DgramClose(int);
extern void GetLocalHostName(char *, unsigned);
extern int  CreateServerSocket(int);
extern int  CreateClientSocket(char *host, int port);
extern int  SocketAccept(int fd);
extern int  SocketLinger(int);
extern int  SetSocketNoDelay(int fd, int flag);
extern int  SocketRead(int fd, char *buf, int size);
extern const char *GetSocketErrorMessageAux(int error);
extern const char *GetSocketErrorMessage(void);

#endif /* _SOCKLIB_INCLUDED */
#endif /* WINDOWS */
