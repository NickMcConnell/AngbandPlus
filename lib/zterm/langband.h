#ifndef LANGBAND_H
#define LANGBAND_H

#ifdef USE_SOUND

typedef struct {
        long mtype;
       	short extra;
	char str[1024];
} snd_msg;

typedef enum {
	SNDMSG_PLAY = 1,
	SNDMSG_LOADSOUND,
	SNDMSG_CLOSE
} snd_msg_types;


errr
send_sound_msg (int type, int extra, const cptr str);
errr
sound_init (void);

#endif /* use_sound */

#endif /* langband_h */
