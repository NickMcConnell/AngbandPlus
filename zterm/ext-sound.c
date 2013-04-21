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

static int server_pid = -1;
static int thesocket_fd = -1;

#define MAXLINE 1024

#ifndef EXTDAEMON_PATH
#define EXTDAEMON_PATH "zterm"
#endif /* extdaemon */

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
    return 1;
}

static ssize_t
readline(int fd, void *vptr, int maxlen) {

    int n, rc;
    char c, *ptr;

    ptr = vptr;
    for (n = 1; n < maxlen; n++) {
	if ( (rc = my_read(fd, &c)) == 1) {
	    *ptr++ = c;
	    if (c == '\n')
		break;	/* newline is stored, like fgets() */
	} else if (rc == 0) {
	    if (n == 1)
		return 0;	/* EOF, no data read */
	    else
		break;		/* EOF, some data was read */
	} else
	    return -1;		/* error, errno set by read() */
    }

    *ptr = 0;	/* null terminate like fgets() */
    return n;
}
/* end readline */

static ssize_t
Readline(int fd, void *ptr, size_t maxlen) {

    ssize_t n;

    if ( (n = readline(fd, ptr, maxlen)) < 0) {
	puts("readline error");
	exit(-1);
    }
	
    return n;
}

static ssize_t	/* Write "n" bytes to a descriptor. */
writen(int fd, const void *vptr, size_t n) {

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
    return n;
}
/* end writen */

static int
Writen(int fd, void *ptr, int nbytes)
{
    if (writen(fd, ptr, nbytes) != nbytes) {
	puts("ext-sound: writen error");
	return -1;
    }
    else {
	return 0;
    }
}


static int
send_command(int fd, const char *cmd) {
    int n, retval;
    char buffer[MAXLINE];
    
    if (!cmd) return -100;
    
    if (fd < 1) {
	ERRORMSG("Illegal socket-descriptor\n");
	return -101;
    }
    
    retval = Writen(fd, (void*)cmd, strlen(cmd));
    if (retval < 0) return -102;
    n = Readline(fd, buffer, MAXLINE);
    //printf("extclient retval: '%s'\n", buffer);
    if (!n)
	return -101;
    else {
	int val = atoi(buffer);
	return val;
    }
}


static int
try_external_program(const char *prg, const char *sockname) {

    int	sockfd;
    struct sockaddr_un	servaddr;
    char buffer[MAXLINE];
    int retval;
    
    INFOMSG("Ext Client: Trying external program: %s\n", prg);

    sprintf(buffer, "%s %s &", prg, sockname);
    system(buffer);

    sleep(1);  // give it some time
    
    sockfd = socket(AF_LOCAL, SOCK_STREAM, 0);

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sun_family = AF_LOCAL;
    strcpy(servaddr.sun_path, sockname);

    connect(sockfd, (struct sockaddr *)&servaddr, sizeof(servaddr));

    retval = send_command(sockfd, "GTPID\n");

    if (retval < 0) {
	return retval;
    }
    else {
	server_pid = retval;
	return sockfd;
    }
    
}
    
int
lbext_init_mixer() {

    // we try a few combinations to find the server
    
    char buffer[MAXLINE];
    char sockname[128];
    int thesock = -1;
    int retval = -1;

    INFOMSG("Ext Client: Init external\n");

    sprintf(sockname, "/tmp/lbsd.%d", getpid());
    
    // the compiled in path first
    sprintf(buffer, "%s/lbsd", EXTDAEMON_PATH);
    
    thesock = try_external_program(buffer, sockname);
        
    if (thesock < 0) {
	// ok, let's check if it's in a zterm dir
	thesock = try_external_program("./zterm/lbsd", sockname);
    }
    
    if (thesock < 0) {
	ERRORMSG("Unable to contact external server, turning off sound.\n");
	thesocket_fd = -1;
	server_pid = -1;
	return thesock;
    }

    if (server_pid < 0) {
	ERRORMSG("Somehow the pid was not set right, turning off sound to be safe.\n");
	thesocket_fd = -1;
	server_pid = -1;
	return -34;
    }

    
    sprintf(buffer, "SINIT %d\n", lbui_get_max_effects());
    //DBGPUT("Ext: Call is %s", buffer);
    retval = send_command(thesock, buffer);
    
    retval = send_command(thesock, "SACTV\n");

    //DBGPUT("Retval is %d\n", retval);

    // shit, we weren't able to activate it.. shut down the shit
    if (retval < 0) {
	if (thesock > 0) {
	    shutdown(thesock, SHUT_WR);
	}
	thesocket_fd = -1;

	if (server_pid > 0) {
	    char sockname[128];
	    kill(server_pid, SIGTERM);
	    sprintf(sockname, "/tmp/lbsd.%d", getpid());
	    unlink(sockname);
	    server_pid = -1;
	}
    }
    else {
	thesocket_fd = thesock;
    }
    
    return retval;
}

int
lbext_close_mixer() {

    DBGPUT("Ext Client: Close external\n");
    
    send_command(thesocket_fd, "SCLOS\n");
    
    if (thesocket_fd > 0) {
	shutdown(thesocket_fd, SHUT_WR);
	thesocket_fd = -1;
    }

    if (server_pid > 0) {
	char sockname[128];
	kill(server_pid, SIGTERM);
	sprintf(sockname, "/tmp/lbsd.%d", getpid());
	unlink(sockname);
	server_pid = -1;
    }

    //DBGPUT("Langband/Ext: Mixer closed.\n");
    return 0;
}


int
lbext_load_sound_effect(const char *fname, int idx) {
    char buffer[128];
    //INFOMSG("Ext Client: Load sfx %d\n", idx);

    sprintf(buffer, "LDSFX %s %d\n", fname, idx);
    return send_command(thesocket_fd, buffer);
}


int
lbext_load_music_file(const char *fname, int idx) {
    char buffer[128];
    //INFOMSG("Langband/Ext: Trying to load music %s.\n", fname);
    //INFOMSG("Ext Client: Load music\n");

    sprintf(buffer, "LDMUS %s %d\n", fname, idx);
    return send_command(thesocket_fd, buffer);
    //INFOMSG("Langband/Ext: Loaded music file %s of size %ld in index %ld.\n", fname, size, handle->buffer_idx);
}

int
lbext_play_sound_effect(int num, float where, short channel, short loops) {

    char buffer[128];
    //INFOMSG("Ext Client: play sfx %d\n", num);
    sprintf(buffer, "PLSFX %d %f %d %d\n", num, where, channel, loops);
    return send_command(thesocket_fd, buffer);
}

int
lbext_play_music_file(int num, float where, short loops) {
    
    char buffer[128];
    //INFOMSG("Ext Client: play music\n");
    sprintf(buffer, "PLMUS %d %f %d\n", num, where, loops);
    return send_command(thesocket_fd, buffer);
}


int
lbext_halt_music(void) {

    //INFOMSG("Ext Client: halt music\n");
    return send_command(thesocket_fd, "STMUS\n");
}


int
lbext_halt_sound_effects(short channel) {
    char buffer[128];
    //INFOMSG("Ext Client: halt sfx\n");
    sprintf(buffer, "STSFX %d\n", channel);
    return send_command(thesocket_fd, buffer);
}
