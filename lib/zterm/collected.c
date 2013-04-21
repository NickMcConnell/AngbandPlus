

#include "angband.h"
#include "langband.h"

/* hackish include of SysV IPC */
#ifdef USE_SOUND
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/msg.h>
#endif


#if defined(USE_GTK)
errr init_gtk(int argc, char **argv);
#elif defined(USE_X11)
errr init_x11(int argc, char **argv);
#elif defined(USE_GCU)
errr init_gcu(int argc, char **argv);
#endif

#if defined(USE_CMUCL)
#define USING_CALLBACK 1
#elif defined (USE_ACL)
#define USING_CALLBACK 1
#elif defined (USE_CLISP)
/* nothing */
#else
#error "Unknown lisp-system"
#endif

#if defined(USE_CMUCL)
typedef unsigned long lispobj ;
extern lispobj funcall0(lispobj function);
#endif

/*
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
        Term_erase(0, 23, 255);
        Term_putstr(20, 23, -1, TERM_WHITE, str);
        Term_fresh();
}


/*
 * Handle abrupt death of the visual system
 *
 * This routine is called only in very rare situations, and only
 * by certain visual systems, when they experience fatal errors.
 *
 * XXX XXX Hack -- clear the death flag when creating a HANGUP
 * save file so that player can see tombstone when restart.
 */
void exit_game_panic(void)
{
	/* If nothing important has happened, just quit */
	if (!character_generated || character_saved) quit("panic");

	/* Mega-Hack -- see "msg_print()" */
	msg_flag = FALSE;

	/* Clear the top line */
	prt("", 0, 0);

	/* Hack -- turn off some things */
	disturb(1, 0);

	/* Hack -- Delay death XXX XXX XXX */
	if (p_ptr->chp < 0) p_ptr->is_dead = FALSE;

	/* Hardcode panic save */
	p_ptr->panic_save = 1;

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Indicate panic save */
	strcpy(p_ptr->died_from, "(panic save)");

	/* Panic save, or get worried */
	if (!save_player()) quit("panic save failed!");

	/* Successful panic save */
	quit("panic save succeeded!");
}



/*
 * Display a string on the screen using an attribute, and clear
 * to the end of the line.
 */
void c_prt(byte attr, cptr str, int row, int col)
{
	/* Clear line, position cursor */
	Term_erase(col, row, 255);

	/* Dump the attr/text */
	Term_addstr(-1, attr, str);
}


/*
 * As above, but in "white"
 */
void prt(cptr str, int row, int col)
{
	/* Spawn */
	c_prt(TERM_WHITE, str, row, col);
}


void
disturb(int stop_search, int unused_flag) {

    (void)(stop_search + unused_flag); // to avoid warning
    note("disturb");
}

bool save_player(void) {

    note("saving");
    return 0;
}


void
init_gui(int argc, char *argv[] ) {

    // leak
    argc = 1;
    argv = malloc(100);
    argv[0] = "langband";
    argv[1] = NULL;

#ifdef USE_SOUND
	// hackish
	use_sound = 1;
	sound_init();
#endif

    
#if defined(USE_GTK)
    init_gtk(argc,argv);
#elif defined(USE_X11)
    init_x11(0,NULL);
#elif defined (USE_GCU)
    init_gcu(argc,argv);
#endif
    

#if defined(USE_X11) || defined(USE_GCU)
    /* Initialize */
    init_angband();
    pause_line(23);
    play_game(TRUE);
#endif

    
}
    
#if defined(USE_ACL)

int (*callback_fun)() = 0;

void
set_lisp_callback(int (*fun)()) {
//    printf("Setting cb to %p\n", fun);
    callback_fun = fun;
}

void
play_game_lisp() {
    puts("Note: playing lisp-game through callback from C");
    (*callback_fun)();
//    void (*lisp_call)(),*lisp_call_address();
//    lisp_call = lisp_call_address(callback_index);
//    (*lisp_call)();

}

#elif defined(USE_CMUCL)

lispobj callback_fun = 0;

void
set_lisp_callback(lispobj fun) {
//    printf("Setting cb to %uld\n", fun);
    callback_fun = fun;
}

void
play_game_lisp() {
    if (callback_fun) {
	puts("Note: playing lisp-game through callback from C");
	funcall0(callback_fun);
    }
}

#elif defined(USE_CLISP)



#else

#error "Unknown lisp-system"

#endif /* lisp-system */


#ifdef SMALL_BOYS_FOR_BREAKFAST
/*
 * Hack -- Explain a broken "lib" folder and quit (see below).
 *
 * XXX XXX XXX This function is "messy" because various things
 * may or may not be initialized, but the "plog()" and "quit()"
 * functions are "supposed" to work under any conditions.
 */
static void init_angband_aux(cptr why)
{
        /* Why */
        plog(why);

        /* Explain */
        plog("The 'lib' directory is probably missing or broken.");

        /* More details */
        plog("Perhaps the archive was not extracted correctly.");

        /* Explain */
        plog("See the 'README' file for more information.");

        /* Quit with error */
        quit("Fatal Error.");
}
#endif

void
init_angband(void) {

//       int fd = -1;

//        int mode = 0644;

        FILE *fp;

        char buf[1024];

 
        /* Open the News file */
        fp = my_fopen("./lib/file/news.txt", "r");

        /* Dump */
        if (fp)
        {
                int i = 0;

                /* Dump the file to the screen */
                while (0 == my_fgets(fp, buf, 1024))
                {
                        /* Display and advance */
                        Term_putstr(0, i++, -1, TERM_WHITE, buf);
                }

                /* Close */
                my_fclose(fp);
        }

	


        /* Flush it */
        Term_fresh();

	macro_init();

#ifdef USE_X11
//	process_pref_file("./lib/file/user.prf");
//	puts("prof..");
#endif
	
	note("[Initialization complete]");

}


/*
 * Global array for converting numbers to uppercase hecidecimal digit
 * This array can also be used to convert a number to an octal digit
 */
char hexsym[16] =
{
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};


void window_stuff(void) {

    printf("window stuff\n");

}




#ifdef HANDLE_SIGNALS


#include <signal.h>


/*
 * Handle signals -- suspend
 *
 * Actually suspend the game, and then resume cleanly
 */
static void handle_signal_suspend(int sig)
{
	/* Disable handler */
	(void)signal(sig, SIG_IGN);

#ifdef SIGSTOP

	/* Flush output */
	Term_fresh();

	/* Suspend the "Term" */
	Term_xtra(TERM_XTRA_ALIVE, 0);

	/* Suspend ourself */
	(void)kill(0, SIGSTOP);

	/* Resume the "Term" */
	Term_xtra(TERM_XTRA_ALIVE, 1);

	/* Redraw the term */
	Term_redraw();

	/* Flush the term */
	Term_fresh();

#endif

	/* Restore handler */
	(void)signal(sig, handle_signal_suspend);
}


/*
 * Handle signals -- simple (interrupt and quit)
 *
 * This function was causing a *huge* number of problems, so it has
 * been simplified greatly.  We keep a global variable which counts
 * the number of times the user attempts to kill the process, and
 * we commit suicide if the user does this a certain number of times.
 *
 * We attempt to give "feedback" to the user as he approaches the
 * suicide thresh-hold, but without penalizing accidental keypresses.
 *
 * To prevent messy accidents, we should reset this global variable
 * whenever the user enters a keypress, or something like that.
 */
static void handle_signal_simple(int sig)
{
	/* Disable handler */
	(void)signal(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	/* Count the signals */
	signal_count++;


	/* Terminate dead characters */
	if (p_ptr->is_dead)
	{
		/* Mark the savefile */
		strcpy(p_ptr->died_from, "Abortion");

		/* Close stuff */
		close_game();

		/* Quit */
		quit("interrupt");
	}

	/* Allow suicide (after 5) */
	else if (signal_count >= 5)
	{
		/* Cause of "death" */
		strcpy(p_ptr->died_from, "Interrupting");

		/* Commit suicide */
		p_ptr->is_dead = TRUE;

		/* Stop playing */
		p_ptr->playing = FALSE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Close stuff */
		close_game();

		/* Quit */
		quit("interrupt");
	}

	/* Give warning (after 4) */
	else if (signal_count >= 4)
	{
		/* Make a noise */
		Term_xtra(TERM_XTRA_NOISE, 0);

		/* Clear the top line */
		Term_erase(0, 0, 255);

		/* Display the cause */
		Term_putstr(0, 0, -1, TERM_WHITE, "Contemplating suicide!");

		/* Flush */
		Term_fresh();
	}

	/* Give warning (after 2) */
	else if (signal_count >= 2)
	{
		/* Make a noise */
		Term_xtra(TERM_XTRA_NOISE, 0);
	}

	/* Restore handler */
	(void)signal(sig, handle_signal_simple);
}


/*
 * Handle signal -- abort, kill, etc
 */
static void handle_signal_abort(int sig)
{
	/* Disable handler */
	(void)signal(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	/* Clear the bottom line */
	Term_erase(0, 23, 255);

	/* Give a warning */
	Term_putstr(0, 23, -1, TERM_RED,
	            "A gruesome software bug LEAPS out at you!");

	/* Message */
	Term_putstr(45, 23, -1, TERM_RED, "Panic save...");

	/* Flush output */
	Term_fresh();

	/* Panic Save */
	p_ptr->panic_save = 1;

	/* Panic save */
	strcpy(p_ptr->died_from, "(panic save)");

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Attempt to save */
	if (save_player())
	{
		Term_putstr(45, 23, -1, TERM_RED, "Panic save succeeded!");
	}

	/* Save failed */
	else
	{
		Term_putstr(45, 23, -1, TERM_RED, "Panic save failed!");
	}

	/* Flush output */
	Term_fresh();

	/* Quit */
	quit("software bug");
}




/*
 * Ignore SIGTSTP signals (keyboard suspend)
 */
void signals_ignore_tstp(void)
{

#ifdef SIGTSTP
	(void)signal(SIGTSTP, SIG_IGN);
#endif

}

/*
 * Handle SIGTSTP signals (keyboard suspend)
 */
void signals_handle_tstp(void)
{

#ifdef SIGTSTP
	(void)signal(SIGTSTP, handle_signal_suspend);
#endif

}


/*
 * Prepare to handle the relevant signals
 */
void signals_init(void)
{

#ifdef SIGHUP
	(void)signal(SIGHUP, SIG_IGN);
#endif


#ifdef SIGTSTP
	(void)signal(SIGTSTP, handle_signal_suspend);
#endif


#ifdef SIGINT
	(void)signal(SIGINT, handle_signal_simple);
#endif

#ifdef SIGQUIT
	(void)signal(SIGQUIT, handle_signal_simple);
#endif


#ifdef SIGFPE
	(void)signal(SIGFPE, handle_signal_abort);
#endif

#ifdef SIGILL
	(void)signal(SIGILL, handle_signal_abort);
#endif

#ifdef SIGTRAP
	(void)signal(SIGTRAP, handle_signal_abort);
#endif

#ifdef SIGIOT
	(void)signal(SIGIOT, handle_signal_abort);
#endif

#ifdef SIGKILL
	(void)signal(SIGKILL, handle_signal_abort);
#endif

#ifdef SIGBUS
	(void)signal(SIGBUS, handle_signal_abort);
#endif

#ifdef SIGSEGV
	(void)signal(SIGSEGV, handle_signal_abort);
#endif

#ifdef SIGTERM
	(void)signal(SIGTERM, handle_signal_abort);
#endif

#ifdef SIGPIPE
	(void)signal(SIGPIPE, handle_signal_abort);
#endif

#ifdef SIGEMT
	(void)signal(SIGEMT, handle_signal_abort);
#endif

#ifdef SIGDANGER
	(void)signal(SIGDANGER, handle_signal_abort);
#endif

#ifdef SIGSYS
	(void)signal(SIGSYS, handle_signal_abort);
#endif

#ifdef SIGXCPU
	(void)signal(SIGXCPU, handle_signal_abort);
#endif

#ifdef SIGPWR
	(void)signal(SIGPWR, handle_signal_abort);
#endif

}


#else	/* HANDLE_SIGNALS */


/*
 * Do nothing
 */
void signals_ignore_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_handle_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_init(void)
{
}


#endif	/* HANDLE_SIGNALS */



/*
 * Close up the current game (player may or may not be dead)
 *
 * This function is called only from "main.c" and "signals.c".
 *
 * Note that the savefile is not saved until the tombstone is
 * actually displayed and the player has a chance to examine
 * the inventory and such.  This allows cheating if the game
 * is equipped with a "quit without save" method.  XXX XXX XXX
 */
void close_game(void)
{
//	char buf[1024];


	/* Handle stuff */
	handle_stuff();

	/* Flush the messages */
	msg_print(NULL);

	/* Flush the input */
	flush();


	/* No suspending now */
	signals_ignore_tstp();


	/* ... */


	/* Allow suspending now */
	signals_handle_tstp();
}

void handle_stuff(void) {

}

void play_game(bool new_game) {

    new_game = 0;  // to avoid warning
    
        /* Hack -- Increase "icky" depth */
        character_icky++;

        /* Verify main term */
        if (!angband_term[0])
        {
                quit("main window does not exist");
        }

        /* Make sure main term is active */
        Term_activate(angband_term[0]);

        /* Verify minimum size */
        if ((Term->hgt < 24) || (Term->wid < 80))
        {
                quit("main window is too small");
        }

        /* Forbid resizing */
        Term->fixed_shape = TRUE;

        /* Hack -- Turn off the cursor */
        (void)Term_set_cursor(0);

#ifdef USING_CALLBACK
	// this is a callback
	play_game_lisp();
#endif
	
}

/* Here we try to do a few things on the C-side to avoid allocating
   things on the Lisp-side. */

#define MAX_BUF_SZ 1024
static char *hidden_buffer = NULL;

static void
clean_hidden_buffer() {
    if (!hidden_buffer) {
	C_MAKE(hidden_buffer, MAX_BUF_SZ, char);
    }
    memset(hidden_buffer,MAX_BUF_SZ,0);
}

/*
 * Converts stat num into a six-char (right justified) string
 */
static void
cnv_stat(int val, char *out_val) {

    /* Above 18 */
    if (val > 18) {
	int bonus = (val - 18);

	if (bonus >= 100) {
	    sprintf(out_val, "18/%03d", bonus);
	}
	else {
	    sprintf(out_val, " 18/%02d", bonus);
	}
    }

    /* From 3 to 18 */
    else {
	sprintf(out_val, "    %2d", val);
    }
}


void
print_coloured_stat (byte attr,
		     int stat,
		     int row,
		     int col) {
    
    clean_hidden_buffer();
    cnv_stat(stat,hidden_buffer);
    c_put_str(attr,hidden_buffer, row, col);

}

static const char *token_list[] = {
    "", // 0
    "Name",
    "Cur MP",
    "Max MP",
    "Level",
    "LEVEL", // 5
    "Exp",
    "EXP",
    "Cur HP",
    "Max HP",
    "Cur AC", // 10
    "AU",
    "Str",
    "Int",
    "Wis",
    "Dex", // 15
    "Con",
    "Chr",
    "STR",
    "INT",
    "WIS", //20
    "DEX",
    "CON",
    "CHR",
    "            "
};

void
print_coloured_token (byte attr,
		      int token,
		      int row,
		      int col) {

//    printf("Going for token %d\n", token);
//    printf("This token is %s\n", token_list[token]);
    c_put_str(attr,token_list[token], row, col);
    
}

void
print_coloured_number (byte attr,
			long number,
			int padding,
			int row,
			int col) {

    char *format_str = "%ld";
    clean_hidden_buffer();
    if (padding == 9) {
	format_str = "%9ld";
    }
    else if (padding == 5) {
	format_str = "%5ld";
    }
    else if (padding == 8) {
	format_str = "%8ld";
    }
    else if (padding == 6) {
	format_str = "%6ld";
    }
    else {
	printf("no print_col_number for %d\n", padding);
    }
    sprintf(hidden_buffer, format_str, number);
    c_put_str(attr,hidden_buffer, row, col);

}

#ifdef USE_SOUND


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

#endif /* USE_SOUND */


/*
int more_chars = 0;
char my_inkey_buffer[1024];

char
read_some_key(int a, int b) {

    char retval = '\0';

    // fix events
    if (Term->key_head == Term->key_tail)
    {
	// Process events (do not wait) 
	Term_xtra(TERM_XTRA_EVENT, FALSE);
    }

    retval = Term->key_queue[Term->key_tail];

    return retval;
}
*/
#ifdef SMALL_BOYS_EAT_VEGETABLES

/*
 * Parse a sub-file of the "extra info" (format shown below)
 *
 * Each "action" line has an "action symbol" in the first column,
 * followed by a colon, followed by some command specific info,
 * usually in the form of "tokens" separated by colons or slashes.
 *
 * Blank lines, lines starting with white space, and lines starting
 * with pound signs ("#") are ignored (as comments).
 *
 * Note the use of "tokenize()" to allow the use of both colons and
 * slashes as delimeters, while still allowing final tokens which
 * may contain any characters including "delimiters".
 *
 * Note the use of "strtol()" to allow all "integers" to be encoded
 * in decimal, hexidecimal, or octal form.
 *
 * Note that "monster zero" is used for the "player" attr/char, "object
 * zero" will be used for the "stack" attr/char, and "feature zero" is
 * used for the "nothing" attr/char.
 *
 * Specify the attr/char values for "monsters" by race index.
 *   R:<num>:<a>/<c>
 *
 * Specify the attr/char values for "objects" by kind index.
 *   K:<num>:<a>/<c>
 *
 * Specify the attr/char values for "features" by feature index.
 *   F:<num>:<a>/<c>
 *
 * Specify the attr/char values for "special" things.
 *   S:<num>:<a>/<c>
 *
 * Specify the attribute values for inventory "objects" by kind tval.
 *   E:<tv>:<a>
 *
 * Define a macro action, given an encoded macro action.
 *   A:<str>
 *
 * Create a macro, given an encoded macro trigger.
 *   P:<str>
 *
 * Create a keymap, given an encoded keymap trigger.
 *   C:<num>:<str>
 *
 * Turn an option off, given its name.
 *   X:<str>
 *
 * Turn an option on, given its name.
 *   Y:<str>
 *
 * Turn a window flag on or off, given a window, flag, and value.
 *   W:<win>:<flag>:<value>
 *
 * Specify visual information, given an index, and some data
 *   V:<num>:<kv>:<rv>:<gv>:<bv>
 */
errr process_pref_file_aux(char *buf)
{
	int i, j, n1, n2;

	char *zz[16];

//	printf("going %s\n", buf);
	
	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace(buf[0])) return (0);

	/* Skip comments */
	if (buf[0] == '#') return (0);


	/* Paranoia */
	/* if (strlen(buf) >= 1024) return (1); */


	/* Require "?:*" format */
	if (buf[1] != ':') return (1);


	/* Process "R:<num>:<a>/<c>" -- attr/char for monster races */
	if (buf[0] == 'R')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			monster_race *r_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= MAX_R_IDX) return (1);
			r_ptr = &r_info[i];
			if (n1) r_ptr->x_attr = n1;
			if (n2) r_ptr->x_char = n2;
			return (0);
		}
	}


	/* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
	else if (buf[0] == 'K')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			object_kind *k_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= MAX_K_IDX) return (1);
			k_ptr = &k_info[i];
			if (n1) k_ptr->x_attr = n1;
			if (n2) k_ptr->x_char = n2;
			return (0);
		}
	}


	/* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
	else if (buf[0] == 'F')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			feature_type *f_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= MAX_F_IDX) return (1);
			f_ptr = &f_info[i];
			if (n1) f_ptr->x_attr = n1;
			if (n2) f_ptr->x_char = n2;
			return (0);
		}
	}


	/* Process "S:<num>:<a>/<c>" -- attr/char for special things */
	else if (buf[0] == 'S')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			j = (byte)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			misc_to_attr[j] = n1;
			misc_to_char[j] = n2;
			return (0);
		}
	}


	/* Process "E:<tv>:<a>" -- attribute for inventory objects */
	else if (buf[0] == 'E')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{
			j = (byte)strtol(zz[0], NULL, 0) % 128;
			n1 = strtol(zz[1], NULL, 0);
			if (n1) tval_to_attr[j] = n1;
			return (0);
		}
	}


	/* Process "A:<str>" -- save an "action" for later */
	else if (buf[0] == 'A')
	{
		text_to_ascii(macro_buffer, buf+2);
		return (0);
	}

	/* Process "P:<str>" -- create macro */
	else if (buf[0] == 'P')
	{
		char tmp[1024];
		text_to_ascii(tmp, buf+2);
		macro_add(tmp, macro_buffer);
		return (0);
	}

	/* Process "C:<num>:<str>" -- create keymap */
	else if (buf[0] == 'C')
	{
		int mode;

		char tmp[1024];

		if (tokenize(buf+2, 2, zz) != 2) return (1);

		mode = strtol(zz[0], NULL, 0);
		if ((mode < 0) || (mode >= KEYMAP_MODES)) return (1);

		text_to_ascii(tmp, zz[1]);
		if (!tmp[0] || tmp[1]) return (1);
		i = (byte)(tmp[0]);

		string_free(keymap_act[mode][i]);

		keymap_act[mode][i] = string_make(macro_buffer);

		return (0);
	}


	/* Process "V:<num>:<kv>:<rv>:<gv>:<bv>" -- visual info */
	else if (buf[0] == 'V')
	{
		if (tokenize(buf+2, 5, zz) == 5)
		{
			i = (byte)strtol(zz[0], NULL, 0);
			angband_color_table[i][0] = (byte)strtol(zz[1], NULL, 0);
			angband_color_table[i][1] = (byte)strtol(zz[2], NULL, 0);
			angband_color_table[i][2] = (byte)strtol(zz[3], NULL, 0);
			angband_color_table[i][3] = (byte)strtol(zz[4], NULL, 0);
			return (0);
		}
	}




	/* Failure */
	return (1);
}


/*
 * Helper function for "process_pref_file()"
 *
 * Input:
 *   v: output buffer array
 *   f: final character
 *
 * Output:
 *   result
 */
static cptr process_pref_file_expr(char **sp, char *fp)
{
	cptr v;

	char *b;
	char *s;

	char b1 = '[';
	char b2 = ']';

	char f = ' ';

	/* Initial */
	s = (*sp);

	/* Skip spaces */
	while (isspace(*s)) s++;

	/* Save start */
	b = s;

	/* Default */
	v = "?o?o?";

	/* Analyze */
	if (*s == b1)
	{
		const char *p;
		const char *t;

		/* Skip b1 */
		s++;

		/* First */
		t = process_pref_file_expr(&s, &f);

		/* Oops */
		if (!*t)
		{
			/* Nothing */
		}

		/* Function: IOR */
		else if (streq(t, "IOR"))
		{
			v = "0";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "1";
			}
		}

		/* Function: AND */
		else if (streq(t, "AND"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && streq(t, "0")) v = "0";
			}
		}

		/* Function: NOT */
		else if (streq(t, "NOT"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "0";
			}
		}

		/* Function: EQU */
		else if (streq(t, "EQU"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_pref_file_expr(&s, &f);
				if (*t && !streq(p, t)) v = "0";
			}
		}

		/* Function: LEQ */
		else if (streq(t, "LEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_pref_file_expr(&s, &f);
				if (*t && (strcmp(p, t) >= 0)) v = "0";
			}
		}

		/* Function: GEQ */
		else if (streq(t, "GEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_pref_file_expr(&s, &f);
				if (*t && (strcmp(p, t) <= 0)) v = "0";
			}
		}

		/* Oops */
		else
		{
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
		}

		/* Verify ending */
		if (f != b2) v = "?x?x?";

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';
	}

	/* Other */
	else
	{
		/* Accept all printables except spaces and brackets */
		while (isprint(*s) && !strchr(" []", *s)) ++s;

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';

		/* Variable */
		if (*b == '$')
		{
			/* System */
			if (streq(b+1, "SYS"))
			{
				v = ANGBAND_SYS;
			}

			/* Graphics */
			else if (streq(b+1, "GRAF"))
			{
				v = ANGBAND_GRAF;
			}

			/* Race */
			else if (streq(b+1, "RACE"))
			{
				v = rp_ptr->title;
			}

			/* Class */
			else if (streq(b+1, "CLASS"))
			{
				v = cp_ptr->title;
			}

			/* Player */
			else if (streq(b+1, "PLAYER"))
			{
				v = op_ptr->base_name;
			}
		}

		/* Constant */
		else
		{
			v = b;
		}
	}

	/* Save */
	(*fp) = f;

	/* Save */
	(*sp) = s;

	/* Result */
	return (v);
}


/*
 * Process the "user pref file" with the given name
 *
 * See the function above for a list of legal "commands".
 *
 * We also accept the special "?" and "%" directives, which
 * allow conditional evaluation and filename inclusion.
 */
errr process_pref_file(cptr name)
{
	FILE *fp;

	char buf[1024];

	char old[1024];

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;

//	puts("going open..");
	/* Build the filename */
//	path_build(buf, 1024, ANGBAND_DIR_USER, name);

	// hack
	strcpy(buf, name);
	
	/* Open the file */
	fp = my_fopen(buf, "r");

	printf("opening %s and got %d\n", buf, fp);
	
	/* No such file */
	if (!fp) return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Count lines */
		num++;


		/* Skip "empty" lines */
		if (!buf[0]) continue;

		/* Skip "blank" lines */
		if (isspace(buf[0])) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;


		/* Save a copy */
		strcpy(old, buf);


		/* Process "?:<expr>" */
		if ((buf[0] == '?') && (buf[1] == ':'))
		{
			char f;
			cptr v;
			char *s;

			/* Start */
			s = buf + 2;

			/* Parse the expr */
			v = process_pref_file_expr(&s, &f);

			/* Set flag */
			bypass = (streq(v, "0") ? TRUE : FALSE);

			/* Continue */
			continue;
		}

		/* Apply conditionals */
		if (bypass) continue;


		/* Process "%:<file>" */
		if (buf[0] == '%')
		{
			/* Process that file if allowed */
			(void)process_pref_file(buf + 2);

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_pref_file_aux(buf);

		/* Oops */
		if (err) break;
	}


	/* Error */
	if (err)
	{
		/* Useful error message */
		msg_format("Error %d in line %d of file '%s'.", err, num, name);
		msg_format("Parsing '%s'", old);
		msg_print(NULL);
	}

	/* Close the file */
	my_fclose(fp);

	/* Result */
	return (err);
}




/*
 * Extract the first few "tokens" from a buffer
 *
 * This function uses "colon" and "slash" as the delimeter characters.
 *
 * We never extract more than "num" tokens.  The "last" token may include
 * "delimeter" characters, allowing the buffer to include a "string" token.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 *
 * Hack -- Attempt to handle the 'c' character formalism
 *
 * Hack -- An empty buffer, or a final delimeter, yields an "empty" token.
 *
 * Hack -- We will always extract at least one token
 */
s16b tokenize(char *buf, s16b num, char **tokens)
{
	int i = 0;

	char *s = buf;


	/* Process */
	while (i < num - 1)
	{
		char *t;

		/* Scan the string */
		for (t = s; *t; t++)
		{
			/* Found a delimiter */
			if ((*t == ':') || (*t == '/')) break;

			/* Handle single quotes */
			if (*t == '\'')
			{
				/* Advance */
				t++;

				/* Handle backslash */
				if (*t == '\\') t++;

				/* Require a character */
				if (!*t) break;

				/* Advance */
				t++;

				/* Hack -- Require a close quote */
				if (*t != '\'') *t = '\'';
			}

			/* Handle back-slash */
			if (*t == '\\') t++;
		}

		/* Nothing left */
		if (!*t) break;

		/* Nuke and advance */
		*t++ = '\0';

		/* Save the token */
		tokens[i++] = s;

		/* Advance */
		s = t;
	}

	/* Save the token */
	tokens[i++] = s;

	/* Number found */
	return (i);
}

#endif /* SMALL_BOYS_EAT_VEGETABLES */

#ifdef ESOTERIC_PQ_TEST

static const char *
gv(const char *val) {
    if (val)
	return val;
    else
	return "";
}

void
PQsetdbLogin(const char *pghost, const char *pgport,
	     const char *pgoptions, const char *pgtty,
	     const char *dbName,
	     const char *login, const char *pwd) {

    printf ("%p %p %p %p %p %p %p\n",
	    pghost, pgport, pgoptions, pgtty,
	    dbName, login, pwd);
    
    printf ("%s %s %s %s %s %s %s\n",
	    gv(pghost), gv(pgport), gv(pgoptions), gv(pgtty),
	    gv(dbName), gv(login), gv(pwd));
	    

}
#endif /* ESOTERIC_PQ_TEST */
