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

typedef enum {
    LISPSYS_CMUCL = 0,
    LISPSYS_ACL   = 1,
    LISPSYS_CLISP = 2,
    LISPSYS_BAD   = 10
} LISP_SYSTEMS;

/* lisp-related stuff */
void play_game_lisp();
void set_lisp_system(LISP_SYSTEMS val);
void set_lisp_callback(void *ptr);

/** will we access lisp through callbacks? */
extern int lisp_will_use_callback;
extern LISP_SYSTEMS current_lisp_system;

#endif /* langband_h */
