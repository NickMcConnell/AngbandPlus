#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/soundcard.h>
#include <signal.h>
#include "snd.h"


typedef int errr;
typedef unsigned char byte;
#define rnd(x)		(int)((double)(x)*rand()/(RAND_MAX+1.0))


#define SNDEVENTS		64
#define MAXSOUNDS		64

typedef struct {
	byte *data;
	unsigned long length;           /* divided by SNDLEN */
} snd_sample;


static int snd_dev;
static char snd_dev_name[32] = "/dev/dsp";

#define DEFAULT_SND_FORMAT	AFMT_U8 //AFMT_S16_LE
#define DEFAULT_SND_CHANNELS	1
#define DEFAULT_SND_FREQ	11025 //22050

static int snd_format = DEFAULT_SND_FORMAT;
static int snd_channels = DEFAULT_SND_CHANNELS;
static int snd_freq = DEFAULT_SND_FREQ;
	/* 0xMMMMSSSS   MMMM - sound buffers, 2^SSSS samples per buffer */
static int snd_fragment = 0x00020009;
static audio_buf_info snd_info;


static int con_id = 0xFADE;
static int con_dev;

static snd_msg msg;


static struct {
	int sounds;
        snd_sample sound[MAXSOUNDS];
} event[SNDEVENTS];


static FILE *repf;

static const char *appName="AngSound";


static errr snd_init (void) {

	/* Open the sound device */
	snd_dev = open(snd_dev_name, O_WRONLY);

	/* Failure */
	if (snd_dev == -1) return 1;


	/* Set sampling type */

        /* Failure (try to fix?) */
        if (ioctl(snd_dev, SNDCTL_DSP_SETFMT, &snd_format) == -1) return 1;

        if (snd_format != DEFAULT_SND_FORMAT) {

		/* Failure (try to fix?) */
		return 1;
        }


	/* Set number of channels */

	/* Failure (try to fix?) */
        if (ioctl(snd_dev, SNDCTL_DSP_CHANNELS, &snd_channels) == -1) return 1;

        if (snd_channels != DEFAULT_SND_CHANNELS) {

		/* Failure (try to fix?) */
		return 1;
        }


	/* Set sampling rate */

	/* Failure */
	if (ioctl(snd_dev, SNDCTL_DSP_SPEED, &snd_freq) == -1) return 1;


	/* Set sound buffer fragments */
        if (ioctl(snd_dev, SNDCTL_DSP_SETFRAGMENT, &snd_fragment) == -1) return 1;

        /* Get sound buffers info */
        if (ioctl(snd_dev, SNDCTL_DSP_GETOSPACE, &snd_info) == -1) return 1;

	/* Success */
	return 0;
}


static errr snd_deinit (void) {
	close (snd_dev);

	return 0;
}


/*
 * Simple and dirty load of a non-compressed mono .wav file
 */
static int load_wav (const char *fn, snd_sample *snd) {
	FILE *wav;
	byte head[0x2C];
	byte cmprchnl[4] = {0x01, 0x00, 0x01, 0x00};
	long freq, length;


	if ((wav = fopen(fn, "rb")) == NULL) return 0;
	
	/* read header */
	fread (head, 1, 0x2C, wav);

	/* not a .wav */
	if (memcmp(&head[0x08], "WAVEfmt", 7)) return 0;
	
	/* compressed or not mono */
	if (memcmp(&head[0x14], cmprchnl, 4)) return 0;
	
	/* extract sample frequency */
	freq = ((long)head[0x18]) + ((long)head[0x19]) * 256 +
		((long)head[0x1A]) * 256*256 +
                ((long)head[0x1B]) * 256*256*256;

	/* extract sample length */
	length = ((long)head[0x28]) + ((long)head[0x29]) * 256 +
		((long)head[0x2A]) * 256*256 +
		((long)head[0x2B]) * 256*256*256;

	fprintf(repf,  "%s %s: freq %ld len %ld\n",appName,fn,freq,length);

	snd->length = length;
	snd->data = (byte*)malloc(snd->length);

	fread (snd->data, 1, length, wav);
	
	fclose (wav);
	return 1;
}


static int load_raw (const char *fn, snd_sample *snd) {
	int t = 0;
        FILE *raw;
	byte *tmp;


	tmp = (byte*)malloc(64*1024);
        if ((raw = fopen(fn, "rb")) == NULL) return 0;
	while (!feof(raw) && t < 64*1024) tmp[t++] = getc(raw);
	fclose (raw);
        snd->length = t;
        snd->data = (byte*)malloc(snd->length);
	memcpy (snd->data, tmp, snd->length);
	free (tmp);

	fprintf(repf,  "%s %s: len %ld\n",appName,fn,snd->length);

	return 1;
}


static errr load_sound (int v, const char *fn) {
	int s = event[v].sounds;
	char fn2[1024];


	/* try to load raw file first */
	if (load_raw(fn, &event[v].sound[s])) {

                event[v].sounds++;

		return 0;
	}

	/* try to load .wav file then */
	sprintf(fn2, "%s.wav", fn);
	if (load_wav(fn2, &event[v].sound[s])) {

                event[v].sounds++;

		return 0;
	}

	/* failure */
	return 1;
}


static errr con_init (void) {

	/* Create an IPC message queue */
	con_dev = msgget (con_id, IPC_CREAT | IPC_EXCL | 0777);

	/* Failure */
	if (con_dev == -1) return 1;

	/* Success */
	return 0;
}


static errr con_deinit (void) {
	return msgctl (con_dev, IPC_RMID, 0);
}

static void daemon_quit(void) {
    printf("%s exiting safely.\n", appName);
    snd_deinit ();
    con_deinit ();
    exit (0);
}

static void
handle_bad_signal(int sig) {
    
    (void)signal(sig, SIG_IGN);

    daemon_quit();
    
}

void signals_init(void)
{

#ifdef SIGHUP
	(void)signal(SIGHUP, SIG_IGN);
#endif


#ifdef SIGINT
	(void)signal(SIGINT, handle_bad_signal);
#endif

#ifdef SIGQUIT
	(void)signal(SIGQUIT, handle_bad_signal);
#endif


#ifdef SIGFPE
	(void)signal(SIGFPE, handle_bad_signal);
#endif

#ifdef SIGILL
	(void)signal(SIGILL, handle_bad_signal);
#endif

#ifdef SIGTRAP
	(void)signal(SIGTRAP, handle_bad_signal);
#endif

#ifdef SIGIOT
	(void)signal(SIGIOT, handle_bad_signal);
#endif

#ifdef SIGKILL
	(void)signal(SIGKILL, handle_bad_signal);
#endif

#ifdef SIGBUS
	(void)signal(SIGBUS, handle_bad_signal);
#endif

#ifdef SIGSEGV
	(void)signal(SIGSEGV, handle_bad_signal);
#endif

#ifdef SIGTERM
	(void)signal(SIGTERM, handle_bad_signal);
#endif

#ifdef SIGPIPE
	(void)signal(SIGPIPE, handle_bad_signal);
#endif

#ifdef SIGEMT
	(void)signal(SIGEMT, handle_bad_signal);
#endif

#ifdef SIGDANGER
	(void)signal(SIGDANGER, handle_bad_signal);
#endif

#ifdef SIGSYS
	(void)signal(SIGSYS, handle_bad_signal);
#endif

#ifdef SIGXCPU
	(void)signal(SIGXCPU, handle_bad_signal);
#endif

#ifdef SIGPWR
	(void)signal(SIGPWR, handle_bad_signal);
#endif

}




static errr con_get (void) {

	/* Check for MSG_CLOSE request in queue */
	if (msgrcv (con_dev, &msg, sizeof(snd_msg) - sizeof(long), MSG_CLOSE, IPC_NOWAIT) > 0) {
	    daemon_quit();
	}

	/* Check for MSG_LOADSOUND request in queue */
	if (msgrcv (con_dev, &msg, sizeof(snd_msg) - sizeof(long), MSG_LOADSOUND, IPC_NOWAIT) > 0) {
		load_sound (msg.extra, msg.str);
	}

        return msgrcv (con_dev, &msg, sizeof(snd_msg) - sizeof(long), MSG_PLAY, IPC_NOWAIT);
}


static snd_sample* select_sound (int v) {
	int s;


	if (v < 0 || v >= SNDEVENTS) return NULL;
        if (event[v].sounds == 0) return NULL;

	s = rnd(event[v].sounds);

	fprintf(repf,"%s [%d of %d]\n",appName,s,event[v].sounds);

	return &event[v].sound[s];
}


static void unload_sounds (void) {
	int s, v;


	for (v = 0; v < SNDEVENTS; v++) {
		for (s = 0; s < event[v].sounds; s++) free (event[v].sound[s].data);
	}
}



int
main (void) {
	snd_sample *snd;
	int playing = 0, sndlen, len;
	byte *sndbuf;

	repf = stdout;

	con_init ();
	snd_init ();

	signals_init();

	fprintf(repf,  "\n%s frag %d of %d (size %d)  %d\n",
		appName,
		snd_info.fragments,
		snd_info.fragstotal,
		snd_info.fragsize,
		snd_info.bytes);

	while (1) {
		if (con_get() > 0) {
		    fprintf(repf,  "%s sound %d requested\n",appName, msg.extra);
		    snd = select_sound(msg.extra);
		    //fprintf(repf,  "\n");
		    if (snd != NULL)
			sndbuf = snd->data,
			    sndlen = snd->length,
			    playing = 1;
		}
		if (playing) {
			len = snd_info.bytes;
			if (len > sndlen) len = sndlen;
			write (snd_dev, sndbuf, len);
			sndbuf += len;
			sndlen -= len;
			if (sndlen == 0) playing = 0;
		} else usleep (100);
	}

        snd_deinit ();
	con_deinit ();
	unload_sounds ();

	return 1;
}

