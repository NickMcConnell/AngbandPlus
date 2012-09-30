/* File: main-tnb.c */

/* Purpose: program entry point */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#ifdef USE_TNB

#include "tk/tnb.h"
#include "maid-grf.h"

cptr help_tnb[] =
{
	"To use tk/tcl port",
	NULL
};


static term data;
bool game_in_progress = FALSE;
char ANGBAND_DIR_TK[1024];
Tcl_Interp *g_interp;

/* Graphics information */
int tnb_tile_x;
int tnb_tile_y;

char tnb_tile_file[1024];

/* Font information */
char tnb_font_file[1024];
int tnb_font_size;

void tnb_get_term(int x, int y, byte *a, char *c, byte *ta, char *tc)
{
	/* Get the term information */
	*a = data.scr->a[y][x];
	*c = data.scr->c[y][x];
	*ta = data.scr->ta[y][x];
	*tc = data.scr->tc[y][x];
}

static errr Term_user_tnb(int n)
{
	/* Hack - ignore parameters for now */
	(void) n;

	return (0);
}

/*
 * Process at least one event
 */
static errr Term_xtra_tnb_event(int v)
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
static errr Term_xtra_tnb_flush(void)
{
	int flags = TCL_ALL_EVENTS | TCL_DONT_WAIT;
	
	while (Tcl_DoOneEvent(flags)) ;

	/* Success */
	return (0);
}

/*
 * Hack -- make a noise
 */
static errr Term_xtra_tnb_noise(void)
{
#ifdef PLATFORM_WIN
	MessageBeep(MB_ICONASTERISK);
#endif /* PLATFORM_WIN */

	return (0);
}

/*
 * Hack -- make a sound
 */
static errr Term_xtra_tnb_sound(int v)
{
	/* Hack - ignore parameters for now */
	(void) v;

	return (0);
}

/*
 * Delay for "x" milliseconds
 */
static int Term_xtra_tnb_delay(int v)
{
	if (v <= 0)
		return (0);

#ifdef PLATFORM_WIN

	/* Sleep */
	Sleep(v);

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

	usleep(1000 * v);

#endif /* PLATFORM_X11 */

	/* Success */
	return (0);
}

/*
 * Do a "special thing"
 */
static errr Term_xtra_tnb(int n, int v)
{
	/* Handle a subset of the legal requests */
	switch (n)
	{
		/* Make a bell sound */
		case TERM_XTRA_NOISE:
		{
			return (Term_xtra_tnb_noise());
		}

		/* Make a special sound */
		case TERM_XTRA_SOUND:
		{
			return (Term_xtra_tnb_sound(v));
		}

		/* Process random events */
		case TERM_XTRA_BORED:
		{
			return (Term_xtra_tnb_event(0));
		}

		/* Process an event */
		case TERM_XTRA_EVENT:
		{
			return (Term_xtra_tnb_event(v));
		}

		/* Flush all events */
		case TERM_XTRA_FLUSH:
		{
			return (Term_xtra_tnb_flush());
		}

		/* React to global changes */
		case TERM_XTRA_REACT:
		{
			return (Term_xtra_tnb_react());
		}

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY:
		{
			return (Term_xtra_tnb_delay(v));
		}
		
		/* Flush the output XXX XXX */
		case TERM_XTRA_FRESH:
		{
			int flags = TCL_WINDOW_EVENTS | TCL_IDLE_EVENTS | TCL_DONT_WAIT;

			while (Tcl_DoOneEvent(flags) != 0)
				;

			return (0);
		}
	}

	/* Oops */
	return 1;
}


static void term_data_link(term *t)
{
	/* Initialize the term */
	term_init(t, 80, 24, 1024);

	/* Use a "software" cursor */
	t->soft_cursor = TRUE;

	/* Use "Term_pict" for "graphic" data */
	t->higher_pict = TRUE;

	/* Erase with "black space" */
	t->attr_blank = TERM_DARK;
	t->char_blank = ' ';

	/* Prepare the template hooks */
	t->user_hook = Term_user_tnb;
	t->xtra_hook = Term_xtra_tnb;
	t->curs_hook = Term_curs_tnb;
	t->wipe_hook = Term_wipe_tnb;
	t->text_hook = Term_text_tnb;
	t->pict_hook = Term_pict_tnb;

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
	for (i = 1; i < 8; i++)
	{
		angband_term[i] = NULL;
	}

	Term_activate(t);
}


/*
 * Display error message and quit (see "z-util.c")
 */
static void hook_quit(cptr str)
{
	(void) str;

	Icon_Exit();
	
	/* cleanup_angband(); */
	
	/* Cleanup Tcl and Tk */
	Tcl_DeleteInterp(g_interp);
	
	/* Hack - no longer hook tcl memory routines */
	rnfree_aux = NULL;
}

/*
 * Memory allocation wrappers
 */
static vptr my_tcl_free(vptr data)
{
	Tcl_Free((char *) data);
	
	return (NULL);
}

static vptr my_tcl_alloc(huge size)
{
	return (Tcl_Alloc(size));
}


#ifdef PLATFORM_X11

/*
 * Init the tk port
 */
int init_tnb(int argc, cptr *argv)
{
	/* Hack -ignore parameter */
	(void) argc;
	
	/* Save the "tk" directory */
	path_make(ANGBAND_DIR_TK, ANGBAND_DIR_SCRIPT, "tk");

	/* Use graphics */
	pick_graphics(GRAPHICS_ADAM_BOLT, &tnb_tile_x, &tnb_tile_y, tnb_tile_file);

	/* Try the "16x16.bmp" file */
	path_make(tnb_font_file, ANGBAND_DIR_XTRA, "font/16x16.txt");
	
	/* Use the "16x16.bmp" file if it exists */
	if (fd_close(fd_open(tnb_font_file, O_RDONLY)))
	{
		quit("Could not initialise font metrics!");
	}
	
	/* Always 16x16 for now */
	tnb_font_size = 16;

	/* Prepare the windows */
	init_windows();

	/* Activate hooks */
	quit_aux = hook_quit;
	core_aux = hook_quit;
	
	/* Hack - TclTk_Init doesn't fail gracefully, so check manually for X11 */
	if (!XOpenDisplay("")) return (1);

	/* Initialize Tcl and Tk. */
	g_interp = TclTk_Init(argv);
	
	/* Paranoia */
	if (!g_interp) return(1);
	
	/* Set the memory-allocation hooks */
	rnfree_aux = my_tcl_free;
	ralloc_aux = my_tcl_alloc;

	/* Initialize */
	angtk_init();
	
	/* Catch nasty signals */
	signals_init();
	
	/* Initialize */
	init_angband();

	/* Program is intialized */
	angtk_angband_initialized();
	
	/* Init colours */
	Term_xtra_tnb_react();

#if 0
	while (TRUE)
	{
	while (Tcl_DoOneEvent(TRUE) != 0)
				;
	}
#endif /* 0 */

	/* Press a key for the player */
	Term_keypress(' ');
	
	/* Paranoia */
	return (0);
}

#endif /* PLATFORM_X11 */

#endif /* USE_TNB */
