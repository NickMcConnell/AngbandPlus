#ifndef __OSS_SOUND__
#define __OSS_SOUND__

typedef struct {
        long mtype;
	short extra;
	char str[1024];
} snd_msg;

typedef enum {
	MSG_PLAY = 1,
        MSG_LOADSOUND,
	MSG_CLOSE
} snd_msg_types;




/* anything below was taken from defines.h */



#define MSG_MAX             30


/*
 * Mega-Hack -- some primitive sound support (see "main-win.c")
 *
 * Some "sound" constants for "Term_xtra(TERM_XTRA_SOUND, val)"
 */
#define SOUND_HIT	    1		-1
#define SOUND_MISS	    2		-1
#define SOUND_FLEE	    3		-1
#define SOUND_DROP	    4		-1
#define SOUND_KILL	    5		-1
#define SOUND_LEVEL	    6		-1
#define SOUND_DEATH	    7		-1
#define SOUND_STUDY     8		-1
#define SOUND_TELEPORT  9		-1
#define SOUND_SHOOT     10		-1
#define SOUND_QUAFF     11		-1
#define SOUND_ZAP       12		-1
#define SOUND_WALK      13		-1
#define SOUND_TPOTHER   14		-1
#define SOUND_HITWALL   15		-1
#define SOUND_EAT       16		-1
#define SOUND_STORE1    17		-1
#define SOUND_STORE2    18		-1
#define SOUND_STORE3    19		-1
#define SOUND_STORE4    20		-1
#define SOUND_DIG       21		-1
#define SOUND_OPENDOOR  22		-1
#define SOUND_SHUTDOOR  23		-1
#define SOUND_TPLEVEL   24		-1

/*
 * Mega-Hack -- maximum known sounds
 *
 * Should be the same as MSG_MAX for compatibility reasons.
 */
#define SOUND_MAX MSG_MAX



#endif /*__OSS_SOUND__*/

