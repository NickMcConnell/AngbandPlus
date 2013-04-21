#ifndef LANGBAND_H
#define LANGBAND_H



#ifdef WIN32
/* angband.h included earlier with these defs */
#ifndef INCLUDED_H_TYPE_H
typedef unsigned char byte;
typedef const char *cptr;
typedef int errr;
typedef char bool;
#endif
#endif /* win32 */


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
