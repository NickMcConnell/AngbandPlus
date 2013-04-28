/* File: main-tnb.c */

/* Purpose: program entry point for X-Windows */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tclInt.h>

#include "angband.h"

#include "tnb.h"

#if defined(WINDOWS) || defined(WINNT)
#error "WINDOWS is defined"
#endif

static term data;
bool game_in_progress = FALSE;
bool initialized = FALSE;
cptr ANGBAND_DIR_XTRA_SOUND;
cptr ANGBAND_DIR_XTRA_HELP;
cptr ANGBAND_DIR_ROOT;
cptr ANGBAND_DIR_TK;

/*
 * Check for existance of a directory
 */
static bool check_file(cptr s)
{
	struct stat statBuf;

	if (TclStat(s, &statBuf)) return (FALSE);
	if (S_ISDIR(statBuf.st_mode)) return (FALSE);
	return (TRUE);
}

/*
 * Check for existance of a directory
 * EXPORTED!!!
 */
bool check_dir(cptr s)
{
	struct stat statBuf;

	if (TclStat(s, &statBuf)) return (FALSE);
	if (!S_ISDIR(statBuf.st_mode)) return (FALSE);
	return (TRUE);
}

/*
 * Validate a file
 */
static void validate_file(cptr s, cptr fmt)
{
	/* Verify or fail */
	if (!check_file(s))
	{
		if (!fmt) fmt = "Could not find a required file:\n%s";
		quit_fmt(fmt, s);
	}
}

/*
 * Validate a directory
 */
static void validate_dir(cptr s)
{
	/* Verify or fail */
	if (!check_dir(s))
	{
		quit_fmt("Cannot find required directory:\n%s", s);
	}
}

/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(cptr str)
{
	/* Give a warning */
	if (str)
	{
		fprintf(stderr, "%s", str);
	}

#ifdef ALLOW_SOUND
	sound_exit();
#endif

	free_icons();
	
	/* Cleanup Tcl and Tk (this exits via Tcl_Exit()) */
	ExitTclTk();

	exit(0);
}

static errr Term_user_hook(int n)
{
	return (0);
}

static errr Term_xtra_hook_react(void)
{
	return 0;
}

/*
 * Process at least one event
 */
static errr Term_xtra_hook_event(int v)
{
	int flags;

	/* Wait for an event */
	if (v) {

		/* Block */
		flags = TCL_ALL_EVENTS;

	/* Check for an event */
	} else {

		/* Check */
		flags = TCL_ALL_EVENTS | TCL_DONT_WAIT;
	}

	(void) Tcl_DoOneEvent(flags);

	/* Success */
	return 0;
}


/*
 * Process all pending events
 */
static errr Term_xtra_hook_flush(void)
{
	int flags = TCL_ALL_EVENTS | TCL_DONT_WAIT;

	while (Tcl_DoOneEvent(flags)) ;

	/* Success */
	return (0);
}

static errr Term_xtra_hook_clear(void)
{
	return 0;
}

/*
 * Hack -- make a noise
 */
static errr Term_xtra_hook_noise(void)
{
/*	XBell(Metadpy->dpy, 100); */
	return (0);
}

/*
 * Hack -- make a sound
 */
static errr Term_xtra_hook_sound(int v)
{
#ifdef ALLOW_SOUND

	return (0);

#else

	/* Oops */
	return (1);

#endif /* TNB */
}

/*
 * Delay for "x" milliseconds
 */
static int Term_xtra_hook_delay(int v)
{
	usleep(1000 * v);

	/* Success */
	return (0);
}

/*
 * Do a "special thing"
 */
static errr Term_xtra_hook(int n, int v)
{
	/* Handle a subset of the legal requests */
	switch (n)
	{
		/* Make a bell sound */
		case TERM_XTRA_NOISE:
		{
			return (Term_xtra_hook_noise());
		}

		/* Make a special sound */
		case TERM_XTRA_SOUND:
		{
			return (Term_xtra_hook_sound(v));
		}

		/* Process random events */
		case TERM_XTRA_BORED:
		{
			return (Term_xtra_hook_event(0));
		}

		/* Process an event */
		case TERM_XTRA_EVENT:
		{
			return (Term_xtra_hook_event(v));
		}

		/* Flush all events */
		case TERM_XTRA_FLUSH:
		{
			return (Term_xtra_hook_flush());
		}

		/* Clear the screen */
		case TERM_XTRA_CLEAR:
		{
			return (Term_xtra_hook_clear());
		}

		/* React to global changes */
		case TERM_XTRA_REACT:
		{
			return (Term_xtra_hook_react());
		}

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY:
		{
			return (Term_xtra_hook_delay(v));
		}
	}

	/* Oops */
	return 1;
}

static errr Term_curs_hook(int x, int y)
{
	return 0;
}

static errr Term_wipe_hook(int x, int y, int n)
{
	return 0;
}

static errr Term_text_hook(int x, int y, int n, byte a, const char *s)
{
	return 0;
}

static errr Term_pict_hook(int x, int y, int n, const byte *ap, const char *cp)
{
	return 0;
}

static void term_data_link(term *t)
{
	/* Initialize the term */
	term_init(t, 80, 24, 1024);

	/* Use a "software" cursor */
	t->soft_cursor = TRUE;

	/* Use "Term_pict" for "graphic" data */
	t->higher_pict = TRUE;

	/* Erase with "white space" */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

	/* Prepare the template hooks */
	t->user_hook = Term_user_hook;
	t->xtra_hook = Term_xtra_hook;
	t->curs_hook = Term_curs_hook;
	t->wipe_hook = Term_wipe_hook;
	t->text_hook = Term_text_hook;
	t->pict_hook = Term_pict_hook;

	/* Remember where we came from */
	t->data = NULL;
}

static void init_windows(void)
{
	int i;

	term *t = &data;

	/* Main window */
	term_data_link(t);
	angband_term[0] = t;

	/* No extra Term's required */
	for (i = 1; i < 8; i++) {
		angband_term[i] = NULL;
	}

	Term_activate(t);
}

/*
 * Init some stuff
 */
static void init_stuff(char *argv0)
{
	char path[1024];

	char *p;

dbwin("PATH_SEP='%s'\n", PATH_SEP);

	/* Get the application path */
	(void) strcpy(path, argv0);

	/* Strip off the application name */
	p = strrchr(path, '/');
	*p = '\0';
	
	/* Save the application directory */
	ANGBAND_DIR_ROOT = string_make(path);

	/* Append "lib" directory to pathname */
	(void) strcpy(p, "/lib/");

	/* Validate the path */
	validate_dir(path);

#if 1
	/* No spaces in path are allowed... */
	if (strchr(path, ' ') != NULL) {
		quit("Please install AngbandTk in a directory with no spaces in the name.");
	}
#endif

	/* Init the file paths */
	init_file_paths(path);

	/* Hack -- Validate the paths */
	validate_dir(ANGBAND_DIR_APEX);
	validate_dir(ANGBAND_DIR_BONE);
	validate_dir(ANGBAND_DIR_DATA);
	validate_dir(ANGBAND_DIR_EDIT);
	validate_dir(ANGBAND_DIR_FILE);
	validate_dir(ANGBAND_DIR_HELP);
	validate_dir(ANGBAND_DIR_INFO);
	validate_dir(ANGBAND_DIR_SAVE);
#if defined(ZANGBANDTK)
	validate_dir(ANGBAND_DIR_SCRIPT);
#endif /* ZANGBANDTK */
	validate_dir(ANGBAND_DIR_USER);
	validate_dir(ANGBAND_DIR_XTRA);

	/* Build the filename */
	path_build(path, 1024, ANGBAND_DIR_FILE, "news.txt");

	/* Hack -- Validate the "news.txt" file */
	validate_file(path, NULL);


#ifdef USE_SOUND

	/* Build the "sound" path */
	path_build(path, 1024, ANGBAND_DIR_XTRA, "sound");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_SOUND = string_make(path);

	/* Validate the "sound" directory */
	validate_dir(ANGBAND_DIR_XTRA_SOUND);

#endif


	/* Build the "help" path */
	path_build(path, 1024, ANGBAND_DIR_XTRA, "help");

	/* Allocate the path */
	ANGBAND_DIR_XTRA_HELP = string_make(path);

	/* Validate the "help" directory */
	/* validate_dir(ANGBAND_DIR_XTRA_HELP); */


	/* Build the "tk" path */
	path_build(path, 1024, ANGBAND_DIR_ROOT, "tk");

	/* Allocate the path */
	ANGBAND_DIR_TK = string_make(path);

	/* Validate the "tk" directory */
	validate_dir(ANGBAND_DIR_TK);


	/* Initialize sound */
#ifndef ALLOW_SOUND
	init_sound();
#endif

	/* Use graphics */
	use_graphics = 1;
}

int main(int argc, char **argv)
{
	char path[1024];

	/* Prepare the filepaths */
	init_stuff(argv[0]);

	/* Prepare the windows */
	init_windows();

	/* Install "quit" hook */
	quit_aux = hook_quit;

	/* Set the system suffix */
	ANGBAND_SYS = "x11";

	/* Initialize Tcl and Tk. */
	InitTclTk(argc, argv);

#if 0
	{
		Display *dpy = Tk_Display(Tk_MainWindow(g_interp));
		(void) XSynchronize(dpy, TRUE);
	}
#endif

	/* Initialize */
	InitAngbandTk();

	/* Source the "startup script" */
	path_build(path, 1024, ANGBAND_DIR_TK, "init-startup.tcl");
	validate_file(path, "Could not find a required file:\n\"%s\"\n"
		"Make sure your unzip utility supports long filenames.");
	if (Tcl_EvalFile(g_interp, path) == TCL_ERROR) {
		quit_fmt("Fatal error:\n%s", Tcl_GetStringResult(g_interp));
	}

	/* Initialize */
	init_angband();

#ifdef ALLOW_SOUND
	sound_init();
#endif

	/* We are now initialized */
	initialized = TRUE;

	/* Program is intialized */
	angtk_angband_initialized();

	/* Loop forever (never returns) */
	while (1) Tcl_DoOneEvent(0);

	/* Paranoia */
	quit(NULL);

	/* Paranoia */
	return (0);
}
