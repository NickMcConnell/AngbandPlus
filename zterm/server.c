#include "lbsound.h"
#include "lbtools.h"
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <ctype.h>

#define MAXLINE 1024

//#define DEBUGSERVER

#ifdef DEBUGSERVER
#define SRVPUT DBGPUT
#else
#define SRVPUT if(1){}else printf
#endif


static ssize_t
my_read(int fd, char *ptr) {

    static int	read_cnt = 0;
    static char	*read_ptr;
    static char	read_buf[MAXLINE];

    if (read_cnt <= 0) {
    again:
	if ( (read_cnt = read(fd, read_buf, sizeof(read_buf))) < 0) {
	    if (errno == EINTR)
		goto again;
	    return(-1);
	} else if (read_cnt == 0)
	    return(0);
	read_ptr = read_buf;
    }

    read_cnt--;
    *ptr = *read_ptr++;
    return(1);
}

static ssize_t
readline(int fd, void *vptr, size_t maxlen)
{
    int		n, rc;
    char	c, *ptr;

    ptr = vptr;
    for (n = 1; n < maxlen; n++) {
	if ( (rc = my_read(fd, &c)) == 1) {
	    *ptr++ = c;
	    if (c == '\n')
		break;	/* newline is stored, like fgets() */
	} else if (rc == 0) {
	    if (n == 1)
		return(0);	/* EOF, no data read */
	    else
		break;		/* EOF, some data was read */
	} else
	    return(-1);		/* error, errno set by read() */
    }

    *ptr = 0;	/* null terminate like fgets() */
    return(n);
}
/* end readline */

static ssize_t
Readline(int fd, void *ptr, int maxlen) {

    ssize_t n;

    if ( (n = readline(fd, ptr, maxlen)) < 0) {
	puts("readline error");
	exit(-1);
    }
	
    return (n);
}


static ssize_t	/* Write "n" bytes to a descriptor. */
writen(int fd, const void *vptr, int n) {

    size_t		nleft;
    ssize_t		nwritten;
    const char	*ptr;

    ptr = vptr;
    nleft = n;
    while (nleft > 0) {
	if ( (nwritten = write(fd, ptr, nleft)) <= 0) {
	    if (errno == EINTR)
		nwritten = 0;		/* and call write() again */
	    else
		return(-1);			/* error */
	}

	nleft -= nwritten;
	ptr   += nwritten;
    }
    return(n);
}
/* end writen */

static void
Writen(int fd, void *ptr, int nbytes) {

    if (writen(fd, ptr, nbytes) != nbytes) {
	puts("Server: writen error");
	exit(-1);
    }
}


#define CMDLEN 5

/* legal commands:
   SINIT size
   SACTV
   SCLOS 
   LDMUS fname idx
   LDSFX fname idx
   PLMUS key where loops
   PLSFX key where channel loops
   STMUS
   STSFX channel
   GTPID
*/

#define MAX_ARGS 10
static int
parse_call(char **arr, const char *str, int max_args) {
    int cnt = 0;
    int idx = 0;
    int len = strlen(str);
    int i;

    while (str[len-1] == '\n') {
	len--;
    }
    
    for (i=0; i < len; i++) {
	if (str[i] == ' ' && i==idx) {
	    idx = i+1;
	}
	else if (str[i] == ' ') {
	    
	    strncpy(arr[cnt], str+idx, i-idx);
	    arr[cnt][i-idx] = 0;
	    cnt++;
	    idx = i+1;
	}

	if (cnt == (MAX_ARGS - 1))
	    return cnt;
    }

    if (idx < len) {
	strncpy(arr[cnt], str+idx, i-idx);
	arr[cnt][i-idx] = 0;
	cnt++;
    }
    
    return cnt;
}

static char **data = NULL;

static int
parse_incoming(char *str, int len) {
    
    const char *cmd = NULL;
    int num_args = -1;
    
    if (!data) {
	int i;
	data = malloc (MAX_ARGS * sizeof(char*));
	for (i=0; i < MAX_ARGS; i++) {
	    data[i] = malloc(80*sizeof(char));
	}
    }
    
    if (len < CMDLEN) {
	return -43;
    }

    num_args = parse_call(data, str, MAX_ARGS); 
    cmd = data[0];

#if 0
    {
	int i;
	for (i=0; i < num_args; i++) {
	    printf("Server-arg %d: '%s'\n", i, data[i]);
	}
    }
#endif
    
    // can be optimised, but there's no real need
    if (!strcmp(cmd, "SINIT")) {
	int size = atoi(data[1]);
	SRVPUT("Ext Server: Init %d '%s'\n", size, data[1]);
	return lbui_init_sound_system(size);
    }
    else if (!strcmp(cmd, "SACTV")) {
	SRVPUT("Ext Server: Activate\n");
	lbui_set_sound_status(1);
	return lbui_activate_sound_system();
    }
    else if (!strcmp(cmd, "SCLOS")) {
	SRVPUT("Ext Server: Close\n");
	return lbui_close_sound_system();
    }
    else if (!strcmp(cmd, "LDMUS")) {
	const char *fname = data[1];
	int idx = atoi(data[2]);
	SRVPUT("Ext Server: load music %s %d.\n", fname, idx);
	return lbui_load_music_file(fname, idx);
    }
    else if (!strcmp(cmd, "LDSFX")) {
	const char *fname = data[1];
	int idx = atoi(data[2]);
	SRVPUT("Server: load sfx %s %d.\n", fname, idx);
	return lbui_load_sound_effect(fname, idx);
    }
    else if (!strcmp(cmd, "PLMUS")) {
	int idx = atoi(data[1]);
	float where = atof(data[2]); // unused so far
	int loops = atoi(data[3]);
	SRVPUT("Server: play music %d.\n", idx);
	return lbui_play_music_file(idx, loops);
    }
    else if (!strcmp(cmd, "PLSFX")) {
	int idx = atoi(data[1]);
	float where = atof(data[2]); // unused so far
	int channel = atoi(data[3]);
	int loops = atoi(data[4]);
	SRVPUT("Server: play sfx %d.\n", idx);
	return lbui_play_sound_effect(idx, channel, loops);
    }
    else if (!strcmp(cmd, "STMUS")) {
	SRVPUT("Server: stop music\n");
	return lbui_halt_music();
    }
    else if (!strcmp(cmd, "STSFX")) {
	int channel = atoi(data[1]);
	SRVPUT("Server: stop sfx\n");
	return lbui_halt_sound_effects(channel);
    }
    else if (!strcmp(cmd, "GTPID")) {
	SRVPUT("Server: getpid()\n");
	return getpid();
    }
    else {
	return -44;
    }

    return 0;
}

static void
str_echo(int sockfd) {

    ssize_t n;
    char line[MAXLINE];
    char backbuf[20];
    int retval;

    for ( ; ; ) {
	//printf("waiting..\n");
	if ( (n = Readline(sockfd, line, MAXLINE)) == 0)
	    return; /* connection closed by other end */
	SRVPUT("Got: %s", line);
	retval = parse_incoming(line, n);
	sprintf(backbuf,"%d\n", retval);
	//printf("Returning '%s' %d, vs '%s' %d\n", backbuf, strlen(backbuf), line, n);
	Writen(sockfd, backbuf, strlen(backbuf));
	//Writen(sockfd, line, n);
    }
}

int
main(int argc, char *argv[]) {
    
    int listenfd, connfd;

    socklen_t clilen;
    struct sockaddr_un cliaddr, servaddr;
    const char *sockname = NULL;

    if (argc > 1) { // first arg should be wanted name for socket-file
	sockname = argv[1];
    }

    if (!sockname) {
	ERRORMSG("No argument for socket name given to lbsd.\n");
	return -1;
    }

    {
	int illegal_sockname = 0;
	int len = strlen(sockname);

	if (len <= 10) {
	    illegal_sockname = 1;
	}
	else if (strncmp(sockname,"/tmp/lbsd.", 10)) {
	    illegal_sockname = 1;
	}
	else {
	    int i;
	    for (i=10; i < len; i++) {
		if (!isdigit(sockname[i]))
		    illegal_sockname = 1;
	    }
	}
    
	if (illegal_sockname) {
	    ERRORMSG("Illegal socket name for lbsd: %s\n", sockname);
	    return -2;
	}
    }

    DBGPUT("Using socketfile %s\n", sockname);
    
    listenfd = socket(AF_LOCAL, SOCK_STREAM, 0);
    
    unlink(sockname);
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sun_family = AF_LOCAL;
    strcpy(servaddr.sun_path, sockname);
    
    bind(listenfd, (struct sockaddr *)&servaddr, sizeof(servaddr));
    
    listen(listenfd, 5);
    
#ifdef USE_SDL_MIXER
    INFOMSG("Going sdl-mixer in server.\n");
    lbui_set_soundsystem(SOUNDSYSTEM_SDL_MIXER);
#endif
    
#ifdef USE_OPENAL
    INFOMSG("Going openAL in server.\n");
    lbui_set_soundsystem(SOUNDSYSTEM_OPENAL);
#endif
    
    for ( ; ; ) {
	clilen = sizeof(cliaddr);
	if ( (connfd = accept(listenfd, (struct sockaddr *)&cliaddr, &clilen)) < 0) {
	    if (errno == EINTR)
		continue;               /* back to for() */
	    else {
		puts("accept error");
		return -1;
	    }
	}
	str_echo(connfd);
	close(connfd);
    }
       
    return 0;
}
