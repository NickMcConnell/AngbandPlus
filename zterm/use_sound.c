/* hackish include of SysV IPC */
#ifdef USE_SOUND
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/msg.h>
#endif


static int snd_con_id;
static int snd_con_dev;

//static
errr
send_sound_msg (int type, int extra, const cptr str) {
	snd_msg sm;

	if (!use_sound) return 0;
	
//	printf("Sending %d,%d, '%s'\n", type, extra, str);
	
	sm.mtype = type;
        sm.extra = extra;
	strcpy (sm.str, str);
	if (msgsnd (snd_con_dev, &sm, sizeof(snd_msg) - 1023 - sizeof(long) + strlen(sm.str), 0) == -1) {
		/* error */
	    puts("X11 error");
		return 1;
	}

	return 0;
}

/*
static errr hack_load_sound_x11 (int n, const cptr fn) {
	char filename[1024], xtra_sound_dir[1024];


	// Build the "sound" path 
	path_build(xtra_sound_dir, 1024, ANGBAND_DIR_XTRA, "sound");
        path_build(filename, 1024, xtra_sound_dir, fn);

        return sound_con_send (SNDMSG_LOADSOUND, n, filename);
}
*/

errr
load_sound (int num, const cptr fname) {

//    printf("Loading %d,%s\n", num, fname);


    return send_sound_msg (SNDMSG_LOADSOUND, num, fname);
}

errr
sound_init (void) {

       	if (!use_sound) return 1;

	puts("Langband/C sound init");

	/* tmp hack */
        snd_con_id = 0xFADE;

	/* Join an IPC message queue */
	snd_con_dev = msgget(snd_con_id, 0);

	/* Failure */
	if (snd_con_dev == -1) {
		use_sound = FALSE;
		return 1;
	}

	/* Hack - Set hook */
//        hack_load_sound = hack_load_sound_x11;

//        process_sound_pref_file("sound.prf");

	/* Success */
	return 0;
}


errr
sound_terminate (void) {

	/* Paranoia */
        if (!use_sound) return 0;

	/* Shutdown angband sound daemon */
	return send_sound_msg (SNDMSG_CLOSE, 0, "");
}
