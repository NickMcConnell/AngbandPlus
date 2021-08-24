/* File: main-x11.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */


/*
 * This file helps Angband work with UNIX/X11 computers.
 *
 * To use this file, compile with "USE_X11" defined, and link against all
 * the various "X11" libraries which may be needed.
 *
 * See also "main-xaw.c".
 *
 * Part of this file provides a user interface package composed of several
 * pseudo-objects, including "metadpy" (a display), "infowin" (a window),
 * "infoclr" (a color), and "infofnt" (a font).  Actually, the package was
 * originally much more interesting, but it was bastardized to keep this
 * file simple.
 *
 * The rest of this file is an implementation of "main-xxx.c" for X11.
 *
 * Most of this file is by Ben Harrison (benh@phial.com).
 */

/*
 * The following shell script can be used to launch Angband, assuming that
 * it was extracted into "~/Angband", and compiled using "USE_X11", on a
 * Linux machine, with a 1280x1024 screen, using 6 windows (with the given
 * characteristics), with gamma correction of 1.8 -> (1 / 1.8) * 256 = 142,
 * and without graphics (add "-g" for graphics).  Just copy this comment
 * into a file, remove the leading " * " characters (and the head/tail of
 * this comment), and make the file executable.
 *
 *
 * #!/bin/csh
 *
 * # Describe attempt
 * echo "Launching angband..."
 * sleep 2
 *
 * # Main window
 * setenv ANGBAND_X11_FONT_0 10x20
 * setenv ANGBAND_X11_AT_X_0 5
 * setenv ANGBAND_X11_AT_Y_0 510
 *
 * # Message window
 * setenv ANGBAND_X11_FONT_1 8x13
 * setenv ANGBAND_X11_AT_X_1 5
 * setenv ANGBAND_X11_AT_Y_1 22
 * setenv ANGBAND_X11_ROWS_1 35
 *
 * # Inventory window
 * setenv ANGBAND_X11_FONT_2 8x13
 * setenv ANGBAND_X11_AT_X_2 635
 * setenv ANGBAND_X11_AT_Y_2 182
 * setenv ANGBAND_X11_ROWS_3 23
 *
 * # Equipment window
 * setenv ANGBAND_X11_FONT_3 8x13
 * setenv ANGBAND_X11_AT_X_3 635
 * setenv ANGBAND_X11_AT_Y_3 22
 * setenv ANGBAND_X11_ROWS_3 12
 *
 * # Monster recall window
 * setenv ANGBAND_X11_FONT_4 6x13
 * setenv ANGBAND_X11_AT_X_4 817
 * setenv ANGBAND_X11_AT_Y_4 847
 * setenv ANGBAND_X11_COLS_4 76
 * setenv ANGBAND_X11_ROWS_4 11
 *
 * # Object recall window
 * setenv ANGBAND_X11_FONT_5 6x13
 * setenv ANGBAND_X11_AT_X_5 817
 * setenv ANGBAND_X11_AT_Y_5 520
 * setenv ANGBAND_X11_COLS_5 76
 * setenv ANGBAND_X11_ROWS_5 24
 *
 * # The build directory
 * cd ~/Angband
 *
 * # Gamma correction
 * setenv ANGBAND_X11_GAMMA 142
 *
 * # Launch Angband
 * ./src/angband -mx11 -- -n6 &
 *
 */

#include "angband.h"

#ifdef USE_X11

#ifndef __MAKEDEPEND__
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include <X11/Xatom.h>
#endif /* __MAKEDEPEND__ */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <limits.h>

#include <sys/time.h>

/* /me pffts Solaris */
#ifndef NAME_MAX
#define	NAME_MAX	_POSIX_NAME_MAX
#endif

/*
 * Include some helpful X11 code.
 */
#include "maid-x11.c"

/*
 * Hack -- avoid some compiler warnings
 */
#define IGNORE_UNUSED_FUNCTIONS


/*
 * Notes on Colors:
 *
 *   1) On a monochrome (or "fake-monochrome") display, all colors
 *   will be "cast" to "fg," except for the bg color, which is,
 *   obviously, cast to "bg".  Thus, one can ignore this setting.
 *
 *   2) Because of the inner functioning of the color allocation
 *   routines, colors may be specified as (a) a typical color name,
 *   (b) a hexidecimal color specification (preceded by a pound sign),
 *   or (c) by strings such as "fg", "bg", "zg".
 *
 *   3) Due to the workings of the init routines, many colors
 *   may also be dealt with by their actual pixel values.  Note that
 *   the pixel with all bits set is "zg = (1<<metadpy->depth)-1", which
 *   is not necessarily either black or white.
 */



/**** Generic Types ****/


/*
 * An X11 pixell specifier
 */
typedef unsigned long Pixell;

/*
 * The structures defined below
 */
typedef struct metadpy metadpy;
typedef struct infowin infowin;
typedef struct infoclr infoclr;
typedef struct infofnt infofnt;


/*
 * A structure summarizing a given Display.
 *
 *	- The Display itself
 *	- The default Screen for the display
 *	- The virtual root (usually just the root)
 *	- The default colormap (from a macro)
 *
 *	- The "name" of the display
 *
 *	- The socket to listen to for events
 *
 *	- The width of the display screen (from a macro)
 *	- The height of the display screen (from a macro)
 *	- The bit depth of the display screen (from a macro)
 *
 *	- The black Pixell (from a macro)
 *	- The white Pixell (from a macro)
 *
 *	- The background Pixell (default: black)
 *	- The foreground Pixell (default: white)
 *	- The maximal Pixell (Equals: ((2 ^ depth)-1), is usually ugly)
 *
 *	- Bit Flag: Force all colors to black and white (default: !color)
 *	- Bit Flag: Allow the use of color (default: depth > 1)
 *	- Bit Flag: We created 'dpy', and so should nuke it when done.
 */
struct metadpy
{
	Display *dpy;
	Screen *screen;
	Window root;
	Colormap cmap;

	char *name;

	int fd;

	uint width;
	uint height;
	uint depth;

	Pixell black;
	Pixell white;

	Pixell bg;
	Pixell fg;
	Pixell zg;

uint mono:
	1;
uint color:
	1;
uint nuke:
	1;
};



/*
 * A Structure summarizing Window Information.
 *
 * I assume that a window is at most 30000 pixels on a side.
 * I assume that the root windw is also at most 30000 square.
 *
 *	- The Window
 *	- The current Input Event Mask
 *
 *	- The location of the window
 *	- The width, height of the window
 *	- The border width of this window
 *
 *	- Byte: 1st Extra byte
 *
 *	- Bit Flag: This window is currently Mapped
 *	- Bit Flag: This window needs to be redrawn
 *	- Bit Flag: This window has been resized
 *
 *	- Bit Flag: We should nuke 'win' when done with it
 *
 *	- Bit Flag: 1st extra flag
 *	- Bit Flag: 2nd extra flag
 *	- Bit Flag: 3rd extra flag
 *	- Bit Flag: 4th extra flag
 */
struct infowin
{
	Window win;
	long mask;

	s16b ox, oy;

	s16b x, y;
	s16b w, h;
	u16b b;

	byte byte1;

uint mapped:
	1;
uint redraw:
	1;
uint resize:
	1;

uint nuke:
	1;

uint flag1:
	1;
uint flag2:
	1;
uint flag3:
	1;
uint flag4:
	1;
};






/*
 * A Structure summarizing Operation+Color Information
 *
 *	- The actual GC corresponding to this info
 *
 *	- The Foreground Pixell Value
 *	- The Background Pixell Value
 *
 *	- Num (0-15): The operation code (As in Clear, Xor, etc)
 *	- Bit Flag: The GC is in stipple mode
 *	- Bit Flag: Destroy 'gc' at Nuke time.
 */
struct infoclr
{
	GC gc;

	Pixell fg;
	Pixell bg;

uint code:
	4;
uint stip:
	1;
uint nuke:
	1;
};



/*
 * A Structure to Hold Font Information
 *
 *	- The 'XFontStruct*' (yields the 'Font')
 *
 *	- The font name
 *
 *	- The default character width
 *	- The default character height
 *	- The default character ascent
 *
 *	- Byte: Pixel offset used during fake mono
 *
 *	- Flag: Force monospacing via 'wid'
 *	- Flag: Nuke info when done
 */
struct infofnt
{
	XFontStruct *info;

	cptr name;

	s16b wid;
	s16b twid;
	s16b hgt;
	s16b asc;

	byte off;

uint mono:
	1;
uint nuke:
	1;
};




/**** Generic Macros ****/



/* Set current metadpy (Metadpy) to 'M' */
#define Metadpy_set(M) \
Metadpy = M


/* Initialize 'M' using Display 'D' */
#define Metadpy_init_dpy(D) \
Metadpy_init_2(D,cNULL)

/* Initialize 'M' using a Display named 'N' */
#define Metadpy_init_name(N) \
Metadpy_init_2((Display*)(NULL),N)

/* Initialize 'M' using the standard Display */
#define Metadpy_init() \
Metadpy_init_name("")


/* Init an infowin by giving father as an (info_win*) (or NULL), and data */
#define Infowin_init_dad(D,X,Y,W,H,B,FG,BG) \
Infowin_init_data(((D) ? ((D)->win) : (Window)(None)), \
X,Y,W,H,B,FG,BG)


/* Init a top level infowin by pos,size,bord,Colors */
#define Infowin_init_top(X,Y,W,H,B,FG,BG) \
Infowin_init_data(None,X,Y,W,H,B,FG,BG)


/* Request a new standard window by giving Dad infowin and X,Y,W,H */
#define Infowin_init_std(D,X,Y,W,H,B) \
Infowin_init_dad(D,X,Y,W,H,B,Metadpy->fg,Metadpy->bg)


/* Set the current Infowin */
#define Infowin_set(I) \
(Infowin = (I))


/* Set the current Infoclr */
#define Infoclr_set(C) \
(Infoclr = (C))


#define Infoclr_init_ppo(F,B,O,M) \
Infoclr_init_data(F,B,O,M)

#define Infoclr_init_cco(F,B,O,M) \
Infoclr_init_ppo(Infoclr_Pixell(F),Infoclr_Pixell(B),O,M)

#define Infoclr_init_ppn(F,B,O,M) \
Infoclr_init_ppo(F,B,Infoclr_Opcode(O),M)

#define Infoclr_init_ccn(F,B,O,M) \
Infoclr_init_cco(F,B,Infoclr_Opcode(O),M)


/* Set the current infofnt */
#define Infofnt_set(I) \
(Infofnt = (I))


/* Errr: Expose Infowin */
#define Infowin_expose() \
(!(Infowin->redraw = 1))

/* Errr: Unxpose Infowin */
#define Infowin_unexpose() \
(Infowin->redraw = 0)



/**** Generic Globals ****/


/*
 * The "default" values
 */
static metadpy metadpy_default;


/*
 * The "current" variables
 */
static metadpy *Metadpy = &metadpy_default;
static infowin *Infowin = (infowin*)(NULL);
static infoclr *Infoclr = (infoclr*)(NULL);
static infofnt *Infofnt = (infofnt*)(NULL);


/**** Generic code ****/

/*
 * Simple routine to save the state of the game when the display connection
 * is broken. Remember, you cannot do anything in this function that will
 * generate X protocol requests.
 */
int x_io_error_handler(Display *d)
{
	/* We have nothing to save */
	if (!character_generated) return 0;

	save_dungeon();
	save_player();

	return 0;
}

/*
 * Calculate how much space there is in the key queue for the current term.
 */
int Term_queue_space(void)
{
	/* Find the gap if the tail is before the head. */
	int space = Term->key_tail - Term->key_head;

	/* Otherwise, add in the extra for looping. */
	if (space <= 0) space = Term->key_size - space;

	/* The last space is never used as that would be interpreted as leaving
	 * no pending keypresses. */
	return space -1;
}


/*
 * Add a series of keypresses to the "queue".
 *
 * Return any errors generated by Term_keypress() in doing so, or SUCCESS
 * if there are none.
 *
 * Catch the "out of space" error before anything is printed.
 *
 * NB: The keys added here will be interpreted by any macros or keymaps.
 */
errr type_string(char *str, uint len)
{
	char *s;

	term *old = Term;

	/* Paranoia - no string. */
	if (!str) return 5;

	/* Hack - calculate the string length here if none given. */
	if (!len) len = strlen(str);

	/* Activate the main window, as all pastes go there. */
	Term_activate(term_screen);

	/* Not enough space for the string. */
	if (Term_queue_space() <= (int)len) {
		msg_format("NOT ENOUGH MEMORY: %d available, %d required", Term_queue_space(), (int)len );
		msg_print(NULL);		
		return 7;
	}

	for (s = str; s < str + len; s++)
	{
		errr err = Term_keypress(*s);

		/* Catch errors other than "str[i] == 0", which is ignored. */
		if (err && err != -1) return err;
	}

	/* Activate the original window. */
	Term_activate(old);

	return 0;
}


/*
 * Init the current metadpy, with various initialization stuff.
 *
 * Inputs:
 *	dpy:  The Display* to use (if NULL, create it)
 *	name: The name of the Display (if NULL, the current)
 *
 * Notes:
 *	If 'name' is NULL, but 'dpy' is set, extract name from dpy
 *	If 'dpy' is NULL, then Create the named Display
 *	If 'name' is NULL, and so is 'dpy', use current Display
 *
 * Return -1 if no Display given, and none can be opened.
 */
static errr Metadpy_init_2(Display *dpy, cptr name)
{
	metadpy *m = Metadpy;

	/*** Open the display if needed ***/

	/* If no Display given, attempt to Create one */
	if (!dpy)
	{
		/* Attempt to open the display */
		dpy = XOpenDisplay(name);

		/* Failure */
		if (!dpy) return ( -1);

		/* We will have to nuke it when done */
		m->nuke = 1;
	}

	/* Since the Display was given, use it */
	else
	{
		/* We will not have to nuke it when done */
		m->nuke = 0;
	}

	XSetIOErrorHandler(x_io_error_handler);

	/*** Save some information ***/

	/* Save the Display itself */
	m->dpy = dpy;

	/* Get the Screen and Virtual Root Window */
	m->screen = DefaultScreenOfDisplay(dpy);
	m->root = RootWindowOfScreen(m->screen);

	/* Get the default colormap */
	m->cmap = DefaultColormapOfScreen(m->screen);

	/* Extract the true name of the display */
	m->name = DisplayString(dpy);

	/* Extract the fd */
	m->fd = ConnectionNumber(Metadpy->dpy);

	/* Save the Size and Depth of the screen */
	m->width = WidthOfScreen(m->screen);
	m->height = HeightOfScreen(m->screen);
	m->depth = DefaultDepthOfScreen(m->screen);

	/* Save the Standard Colors */
	m->black = BlackPixelOfScreen(m->screen);
	m->white = WhitePixelOfScreen(m->screen);

	/*** Make some clever Guesses ***/

	/* Guess at the desired 'fg' and 'bg' Pixell's */
	m->bg = m->black;
	m->fg = m->white;

	/* Calculate the Maximum allowed Pixel value.  */
	m->zg = (1 << m->depth) - 1;

	/* Save various default Flag Settings */
	m->color = ((m->depth > 1) ? 1 : 0);
	m->mono = ((m->color) ? 0 : 1);

	/* Return "success" */
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Nuke the current metadpy
 */
static errr Metadpy_nuke(void)
{
	metadpy *m = Metadpy;


	/* If required, Free the Display */
	if (m->nuke)
	{
		/* Close the Display */
		XCloseDisplay(m->dpy);

		/* Forget the Display */
		m->dpy = (Display*)(NULL);

		/* Do not nuke it again */
		m->nuke = 0;
	}

	/* Return Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * General Flush/ Sync/ Discard routine
 */
static errr Metadpy_update(int flush, int sync, int discard)
{
	/* Flush if desired */
	if (flush) XFlush(Metadpy->dpy);

	/* Sync if desired, using 'discard' */
	if (sync) XSync(Metadpy->dpy, discard);

	/* Success */
	return (0);
}


/*
 * Make a simple beep
 */
static errr Metadpy_do_beep(void)
{
	/* Make a simple beep */
	XBell(Metadpy->dpy, 100);

	return (0);
}



/*
 * Set the name (in the title bar) of Infowin
 */
static errr Infowin_set_name(cptr name)
{
	Status st;
	XTextProperty tp;
	char buf[128];
	char *bp = buf;
	strcpy(buf, name);
	st = XStringListToTextProperty(&bp, 1, &tp);
	if (st) XSetWMName(Metadpy->dpy, Infowin->win, &tp);
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Set the icon name of Infowin
 */
static errr Infowin_set_icon_name(cptr name)
{
	Status st;
	XTextProperty tp;
	char buf[128];
	char *bp = buf;
	strcpy(buf, name);
	st = XStringListToTextProperty(&bp, 1, &tp);
	if (st) XSetWMIconName(Metadpy->dpy, Infowin->win, &tp);
	return (0);
}


/*
 * Nuke Infowin
 */
static errr Infowin_nuke(void)
{
	infowin *iwin = Infowin;

	/* Nuke if requested */
	if (iwin->nuke)
	{
		/* Destory the old window */
		XDestroyWindow(Metadpy->dpy, iwin->win);
	}

	/* Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Prepare a new 'infowin'.
 */
static errr Infowin_prepare(Window xid)
{
	infowin *iwin = Infowin;

	Window tmp_win;
	XWindowAttributes xwa;
	int x, y;
	unsigned int w, h, b, d;

	/* Assign stuff */
	iwin->win = xid;

	/* Check For Error XXX Extract some ACTUAL data from 'xid' */
	XGetGeometry(Metadpy->dpy, xid, &tmp_win, &x, &y, &w, &h, &b, &d);

	/* Apply the above info */
	iwin->x = x;
	iwin->y = y;
	iwin->w = w;
	iwin->h = h;
	iwin->b = b;

	/* Check Error XXX Extract some more ACTUAL data */
	XGetWindowAttributes(Metadpy->dpy, xid, &xwa);

	/* Apply the above info */
	iwin->mask = xwa.your_event_mask;
	iwin->mapped = ((xwa.map_state == IsUnmapped) ? 0 : 1);

	/* And assume that we are exposed */
	iwin->redraw = 1;

	/* Success */
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Initialize a new 'infowin'.
 */
static errr Infowin_init_real(Window xid)
{
	/* Wipe it clean */
	(void)WIPE(Infowin, infowin);

	/* Start out non-nukable */
	Infowin->nuke = 0;

	/* Attempt to Prepare ourself */
	return (Infowin_prepare(xid));
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Init an infowin by giving some data.
 *
 * Inputs:
 *	dad: The Window that should own this Window (if any)
 *	x,y: The position of this Window
 *	w,h: The size of this Window
 *	b,d: The border width and pixel depth
 *
 * Notes:
 *	If 'dad == None' assume 'dad == root'
 */
static errr Infowin_init_data(Window dad, int x, int y, int w, int h,
                              int b, Pixell fg, Pixell bg)
{
	Window xid;

	/* Wipe it clean */
	(void)WIPE(Infowin, infowin);


	/*** Error Check XXX ***/


	/*** Create the Window 'xid' from data ***/

	/* What happened here?  XXX XXX XXX */

	/* If no parent given, depend on root */
	if (dad == None)

		/* #ifdef USE_GRAPHICS

				xid = XCreateWindow(Metadpy->dpy, Metadpy->root, x, y, w, h, b, 8, InputOutput, CopyFromParent, 0, 0);

			else
		*/

		/* #else */

		dad = Metadpy->root;

	/* #endif */

	/* Create the Window XXX Error Check */
	xid = XCreateSimpleWindow(Metadpy->dpy, dad, x, y, w, h, b, fg, bg);

	/* Start out selecting No events */
	XSelectInput(Metadpy->dpy, xid, 0L);


	/*** Prepare the new infowin ***/

	/* Mark it as nukable */
	Infowin->nuke = 1;

	/* Attempt to Initialize the infowin */
	return (Infowin_prepare(xid));
}



/*
 * Modify the event mask of an Infowin
 */
static errr Infowin_set_mask(long mask)
{
	/* Save the new setting */
	Infowin->mask = mask;

	/* Execute the Mapping */
	XSelectInput(Metadpy->dpy, Infowin->win, Infowin->mask);

	/* Success */
	return (0);
}


/*
 * Request that Infowin be mapped
 */
static errr Infowin_map(void)
{
	/* Execute the Mapping */
	XMapWindow(Metadpy->dpy, Infowin->win);

	/* Success */
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Request that Infowin be unmapped
 */
static errr Infowin_unmap(void)
{
	/* Execute the Un-Mapping */
	XUnmapWindow(Metadpy->dpy, Infowin->win);

	/* Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Request that Infowin be raised
 */
static errr Infowin_raise(void)
{
	/* Raise towards visibility */
	XRaiseWindow(Metadpy->dpy, Infowin->win);

	/* Success */
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Request that Infowin be lowered
 */
static errr Infowin_lower(void)
{
	/* Lower towards invisibility */
	XLowerWindow(Metadpy->dpy, Infowin->win);

	/* Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Request that Infowin be moved to a new location
 */
static errr Infowin_impell(int x, int y)
{
	/* Execute the request */
	XMoveWindow(Metadpy->dpy, Infowin->win, x, y);

	/* Success */
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Move and Resize an infowin
 */
static errr Infowin_locate(int x, int y, int w, int h)
{
	/* Execute the request */
	XMoveResizeWindow(Metadpy->dpy, Infowin->win, x, y, w, h);

	/* Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Visually clear Infowin
 */
static errr Infowin_wipe(void)
{
	/* Execute the request */
	XClearWindow(Metadpy->dpy, Infowin->win);

	/* Success */
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Visually Paint Infowin with the current color
 */
static errr Infowin_fill(void)
{
	/* Execute the request */
	XFillRectangle(Metadpy->dpy, Infowin->win, Infoclr->gc,
	               0, 0, Infowin->w, Infowin->h);

	/* Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * A NULL terminated pair list of legal "operation names"
 *
 * Pairs of values, first is texttual name, second is the string
 * holding the decimal value that the operation corresponds to.
 */
static cptr opcode_pairs[] =
{
	"cpy", "3",
	"xor", "6",
	"and", "1",
	"ior", "7",
	"nor", "8",
	"inv", "10",
	"clr", "0",
	"set", "15",

	"src", "3",
	"dst", "5",

	"+andReverse", "2",
	"+andInverted", "4",
	"+noop", "5",
	"+equiv", "9",
	"+orReverse", "11",
	"+copyInverted", "12",
	"+orInverted", "13",
	"+nand", "14",
	NULL
};


/*
 * Parse a word into an operation "code"
 *
 * Inputs:
 *	str: A string, hopefully representing an Operation
 *
 * Output:
 *	0-15: if 'str' is a valid Operation
 *	-1:   if 'str' could not be parsed
 */
static int Infoclr_Opcode(cptr str)
{
	register int i;

	/* Scan through all legal operation names */
	for (i = 0; opcode_pairs[i*2]; ++i)
	{
		/* Is this the right oprname? */
		if (streq(opcode_pairs[i*2], str))
		{
			/* Convert the second element in the pair into a Code */
			return (atoi(opcode_pairs[i*2 + 1]));
		}
	}

	/* The code was not found, return -1 */
	return ( -1);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Request a Pixell by name.  Note: uses 'Metadpy'.
 *
 * Inputs:
 *      name: The name of the color to try to load (see below)
 *
 * Output:
 *	The Pixell value that metched the given name
 *	'Metadpy->fg' if the name was unparseable
 *
 * Valid forms for 'name':
 *	'fg', 'bg', 'zg', '<name>' and '#<code>'
 */
static Pixell Infoclr_Pixell(cptr name)
{
	XColor scrn;

	/* Attempt to Parse the name */
	if (name && name[0])
	{
		/* The 'bg' color is available */
		if (streq(name, "bg")) return (Metadpy->bg);

		/* The 'fg' color is available */
		if (streq(name, "fg")) return (Metadpy->fg);

		/* The 'zg' color is available */
		if (streq(name, "zg")) return (Metadpy->zg);

		/* The 'white' color is available */
		if (streq(name, "white")) return (Metadpy->white);

		/* The 'black' color is available */
		if (streq(name, "black")) return (Metadpy->black);

		/* Attempt to parse 'name' into 'scrn' */
		if (!(XParseColor(Metadpy->dpy, Metadpy->cmap, name, &scrn)))
		{
			plog_fmt("Warning: Couldn't parse color '%s'\n", name);
		}

		/* Attempt to Allocate the Parsed color */
		if (!(XAllocColor(Metadpy->dpy, Metadpy->cmap, &scrn)))
		{
			plog_fmt("Warning: Couldn't allocate color '%s'\n", name);
		}

		/* The Pixel was Allocated correctly */
		else return (scrn.pixel);
	}

	/* Warn about the Default being Used */
	plog_fmt("Warning: Using 'fg' for unknown color '%s'\n", name);

	/* Default to the 'Foreground' color */
	return (Metadpy->fg);
}


/*
 * Initialize a new 'infoclr' with a real GC.
 */
static errr Infoclr_init_1(GC gc)
{
	infoclr *iclr = Infoclr;

	/* Wipe the iclr clean */
	(void)WIPE(iclr, infoclr);

	/* Assign the GC */
	iclr->gc = gc;

	/* Success */
	return (0);
}


/*
 * Nuke an old 'infoclr'.
 */
static errr Infoclr_nuke(void)
{
	infoclr *iclr = Infoclr;

	/* Deal with 'GC' */
	if (iclr->nuke)
	{
		/* Free the GC */
		XFreeGC(Metadpy->dpy, iclr->gc);
	}

	/* Forget the current */
	Infoclr = (infoclr*)(NULL);

	/* Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Initialize an infoclr with some data
 *
 * Inputs:
 *	fg:   The Pixell for the requested Foreground (see above)
 *	bg:   The Pixell for the requested Background (see above)
 *	op:   The Opcode for the requested Operation (see above)
 *	stip: The stipple mode
 */
static errr Infoclr_init_data(Pixell fg, Pixell bg, int op, int stip)
{
	infoclr *iclr = Infoclr;

	GC gc;
	XGCValues gcv;
	unsigned long gc_mask;



	/*** Simple error checking of opr and clr ***/

	/* Check the 'Pixells' for realism */
	if (bg > Metadpy->zg) return ( -1);
	if (fg > Metadpy->zg) return ( -1);

	/* Check the data for trueness */
	if ((op < 0) || (op > 15)) return ( -1);


	/*** Create the requested 'GC' ***/

	/* Assign the proper GC function */
	gcv.function = op;

	/* Assign the proper GC background */
	gcv.background = bg;

	/* Assign the proper GC foreground */
	gcv.foreground = fg;

	/* Hack -- Handle XOR (xor is code 6) by hacking bg and fg */
	if (op == 6) gcv.background = 0;
	if (op == 6) gcv.foreground = (bg ^ fg);

	/* Assign the proper GC Fill Style */
	gcv.fill_style = (stip ? FillStippled : FillSolid);

	/* Turn off 'Give exposure events for pixmap copying' */
	gcv.graphics_exposures = False;

	/* Set up the GC mask */
	gc_mask = (GCFunction | GCBackground | GCForeground |
	           GCFillStyle | GCGraphicsExposures);

	/* Create the GC detailed above */
	gc = XCreateGC(Metadpy->dpy, Metadpy->root, gc_mask, &gcv);


	/*** Initialize ***/

	/* Wipe the iclr clean */
	(void)WIPE(iclr, infoclr);

	/* Assign the GC */
	iclr->gc = gc;

	/* Nuke it when done */
	iclr->nuke = 1;

	/* Assign the parms */
	iclr->fg = fg;
	iclr->bg = bg;
	iclr->code = op;
	iclr->stip = stip ? 1 : 0;

	/* Success */
	return (0);
}



/*
 * Change the 'fg' for an infoclr
 *
 * Inputs:
 *	fg:   The Pixell for the requested Foreground (see above)
 */
static errr Infoclr_change_fg(Pixell fg)
{
	infoclr *iclr = Infoclr;


	/*** Simple error checking of opr and clr ***/

	/* Check the 'Pixells' for realism */
	if (fg > Metadpy->zg) return ( -1);


	/*** Change ***/

	/* Change */
	XSetForeground(Metadpy->dpy, iclr->gc, fg);

	/* Success */
	return (0);
}



#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Nuke an old 'infofnt'.
 */
static errr Infofnt_nuke(void)
{
	infofnt *ifnt = Infofnt;

	/* Deal with 'name' */
	if (ifnt->name)
	{
		/* Free the name */
		string_free(ifnt->name);
	}

	/* Nuke info if needed */
	if (ifnt->nuke)
	{
		/* Free the font */
		XFreeFont(Metadpy->dpy, ifnt->info);
	}

	/* Success */
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Prepare a new 'infofnt'
 */
static errr Infofnt_prepare(XFontStruct *info)
{
	infofnt *ifnt = Infofnt;

	XCharStruct *cs;

	/* Assign the struct */
	ifnt->info = info;

	/* Jump into the max bouonds thing */
	cs = &(info->max_bounds);

	/* Extract default sizing info */
	ifnt->asc = info->ascent;
	ifnt->hgt = info->ascent + info->descent;
	ifnt->wid = cs->width;
	if (use_bigtile)
		ifnt->twid = 2 * ifnt->wid;
	else
		ifnt->twid = ifnt->wid;


#ifdef OBSOLETE_SIZING_METHOD
	/* Extract default sizing info */
	ifnt->asc = cs->ascent;
	ifnt->hgt = (cs->ascent + cs->descent);
	ifnt->wid = cs->width;
#endif

	/* Success */
	return (0);
}


#ifndef IGNORE_UNUSED_FUNCTIONS

/*
 * Initialize a new 'infofnt'.
 */
static errr Infofnt_init_real(XFontStruct *info)
{
	/* Wipe the thing */
	(void)WIPE(Infofnt, infofnt);

	/* No nuking */
	Infofnt->nuke = 0;

	/* Attempt to prepare it */
	return (Infofnt_prepare(info));
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Init an infofnt by its Name
 *
 * Inputs:
 *	name: The name of the requested Font
 */
static errr Infofnt_init_data(cptr name)
{
	XFontStruct *info;


	/*** Load the info Fresh, using the name ***/

	/* If the name is not given, report an error */
	if (!name) return ( -1);

	/* Attempt to load the font */
	info = XLoadQueryFont(Metadpy->dpy, name);

	/* The load failed, try to recover */
	if (!info) return ( -1);


	/*** Init the font ***/

	/* Wipe the thing */
	(void)WIPE(Infofnt, infofnt);

	/* Attempt to prepare it */
	if (Infofnt_prepare(info))
	{
		/* Free the font */
		XFreeFont(Metadpy->dpy, info);

		/* Fail */
		return ( -1);
	}

	/* Save a copy of the font name */
	Infofnt->name = string_make(name);

	/* Mark it as nukable */
	Infofnt->nuke = 1;

	/* Success */
	return (0);
}


/*
 * Standard Text
 */
static errr Infofnt_text_std(int x, int y, cptr str, int len)
{
	int i;


	/*** Do a brief info analysis ***/

	/* Do nothing if the string is null */
	if (!str || !*str) return ( -1);

	/* Get the length of the string */
	if (len < 0) len = strlen(str);


	/*** Decide where to place the string, vertically ***/

	/* Ignore Vertical Justifications */
	y = (y * Infofnt->hgt) + Infofnt->asc + Infowin->oy;


	/*** Decide where to place the string, horizontally ***/

	/* Line up with x at left edge of column 'x' */
	x = (x * Infofnt->wid) + Infowin->ox;


	/*** Actually draw 'str' onto the infowin ***/

	/* Be sure the correct font is ready */
	XSetFont(Metadpy->dpy, Infoclr->gc, Infofnt->info->fid);


	/*** Handle the fake mono we can enforce on fonts ***/

	/* Monotize the font */
	if (Infofnt->mono)
	{
		/* Do each character */
		for (i = 0; i < len; ++i)
		{
			/* Note that the Infoclr is set up to contain the Infofnt */
			XDrawImageString(Metadpy->dpy, Infowin->win, Infoclr->gc,
			                 x + i * Infofnt->wid + Infofnt->off, y, str + i, 1);
		}
	}

	/* Assume monoospaced font */
	else
	{
		/* Note that the Infoclr is set up to contain the Infofnt */
		XDrawImageString(Metadpy->dpy, Infowin->win, Infoclr->gc,
		                 x, y, str, len);
	}


	/* Success */
	return (0);
}


/*
 * Painting where text would be
 */
static errr Infofnt_text_non(int x, int y, cptr str, int len)
{
	int w, h;


	/*** Find the width ***/

	/* Negative length is a flag to count the characters in str */
	if (len < 0) len = strlen(str);

	/* The total width will be 'len' chars * standard width */
	w = len * Infofnt->wid;


	/*** Find the X dimensions ***/

	/* Line up with x at left edge of column 'x' */
	x = x * Infofnt->wid + Infowin->ox;


	/*** Find other dimensions ***/

	/* Simply do 'Infofnt->hgt' (a single row) high */
	h = Infofnt->hgt;

	/* Simply do "at top" in row 'y' */
	y = y * h + Infowin->oy;


	/*** Actually 'paint' the area ***/

	/* Just do a Fill Rectangle */
	XFillRectangle(Metadpy->dpy, Infowin->win, Infoclr->gc, x, y, w, h);

	/* Success */
	return (0);
}



/*************************************************************************/


/*
 * Angband specific code follows... (ANGBAND)
 */


/*
 * Hack -- cursor color
 */
static infoclr *xor;

/*
 * Actual color table
 */
static infoclr *clr[256];

/*
 * Color info (unused, red, green, blue).
 */
static byte color_table[256][4];

/*
 * Forward declare
 */
typedef struct term_data term_data;

/*
 * A structure for each "term"
 */
struct term_data
{
	term t;

	infofnt *fnt;

	infowin *win;

#ifdef USE_GRAPHICS

	XImage *tiles;

#ifdef USE_TRANSPARENCY

	/* Tempory storage for overlaying tiles. */
	XImage *TmpImage;

#endif

#endif

};


/*
 * The number of term data structures
 */
#define MAX_TERM_DATA 8

/*
 * The array of term data structures
 */
static term_data data[MAX_TERM_DATA];

/* Use short names for the most commonly used elements of various structures. */
#define DPY (Metadpy->dpy)
#define WIN (Infowin->win)

/*
 * Simply push a set of co-ordinates around.
 */
typedef struct co_ord co_ord;
struct co_ord
{
	int x;
	int y;
};

/*
 * A special structure to store information about the text currently
 * selected.
 */
typedef struct x11_selection_type x11_selection_type;
struct x11_selection_type
{
	bool select;  /* The selection is currently in use. */
	bool drawn;  /* The selection is currently displayed. */
	term *t;  /* The window where the selection is found. */
	co_ord init;  /* The starting co-ordinates. */
	co_ord cur;  /* The end co-ordinates (the current ones if still copying). */
	co_ord old;  /* The previous end co-ordinates. */
	Time time;  /* The time at which the selection was finalised. */
};

static x11_selection_type s_ptr[1];



/*
 * Process a keypress event
 *
 * Also appears in "main-xaw.c".
 */
static void react_keypress(XKeyEvent *xev)
{
	int i, n, mc, ms, mo, mx;

	uint ks1;

	XKeyEvent *ev = (XKeyEvent*)(xev);

	KeySym ks;

	char buf[128];
	char msg[128];


	/* Check for "normal" keypresses */
	n = XLookupString(ev, buf, 125, &ks, NULL);

	/* Terminate */
	buf[n] = '\0';


	/* Hack -- Ignore "modifier keys" */
	if (IsModifierKey(ks)) return;


	/* Hack -- convert into an unsigned int */
	ks1 = (uint)(ks);

	/* Extract four "modifier flags" */
	mc = (ev->state & ControlMask) ? TRUE : FALSE;
	ms = (ev->state & ShiftMask) ? TRUE : FALSE;
	mo = (ev->state & Mod1Mask) ? TRUE : FALSE;
	mx = (ev->state & Mod2Mask) ? TRUE : FALSE;


	/* Normal keys with no modifiers */
	if (n && !mo && !mx && !IsSpecialKey(ks))
	{
		/* Enqueue the normal key(s) */
		for (i = 0; buf[i]; i++) Term_keypress(buf[i]);

		/* All done */
		return;
	}


	/* Handle a few standard keys (bypass modifiers) XXX XXX XXX */
	switch (ks1)
	{
	case XK_Escape:
		{
			Term_keypress(ESCAPE);
			return;
		}

	case XK_Return:
		{
			Term_keypress('\r');
			return;
		}

	case XK_Tab:
		{
			Term_keypress('\t');
			return;
		}

	case XK_Delete:
	case XK_BackSpace:
		{
			Term_keypress('\010');
			return;
		}
	}


	/* Hack -- Use the KeySym */
	if (ks)
	{
		sprintf(msg, "%c%s%s%s%s_%lX%c", 31,
		        mc ? "N" : "", ms ? "S" : "",
		        mo ? "O" : "", mx ? "M" : "",
		        (unsigned long)(ks), 13);
	}

	/* Hack -- Use the Keycode */
	else
	{
		sprintf(msg, "%c%s%s%s%sK_%X%c", 31,
		        mc ? "N" : "", ms ? "S" : "",
		        mo ? "O" : "", mx ? "M" : "",
		        ev->keycode, 13);
	}

	/* Enqueue the "macro trigger" string */
	for (i = 0; msg[i]; i++) Term_keypress(msg[i]);


	/* Hack -- auto-define macros as needed */
	if (n && (macro_find_exact(msg) < 0))
	{
		/* Create a macro */
		macro_add(msg, buf);
	}
}


/*
 * Find the square a particular pixel is part of.
 */
static void pixel_to_square(int * const x, int * const y,
                            const int ox, const int oy)
{
	(*x) = (ox - Infowin->ox) / Infofnt->wid;
	(*y) = (oy - Infowin->oy) / Infofnt->hgt;
}

/*
 * Find the pixel at the top-left corner of a square.
 */
static void square_to_pixel(int * const x, int * const y,
                            const int ox, const int oy)
{
	(*x) = ox * Infofnt->wid + Infowin->ox;
	(*y) = oy * Infofnt->hgt + Infowin->oy;
}

/*
 * Convert co-ordinates from starting corner/opposite corner to minimum/maximum.
 */
static void sort_co_ord(co_ord *min, co_ord *max,
                        const co_ord *b, const co_ord *a)
{
	min->x = MIN(a->x, b->x);
	min->y = MIN(a->y, b->y);
	max->x = MAX(a->x, b->x);
	max->y = MAX(a->y, b->y);
}

/*
 * Remove the selection by redrawing it.
 */
static void mark_selection_clear(int x1, int y1, int x2, int y2)
{
	Term_redraw_section(x1, y1, x2, y2);
}

/*
 * Select an area by drawing a grey box around it.
 * NB. These two functions can cause flicker as the selection is modified,
 * as the game redraws the entire marked section.
 */
static void mark_selection_mark(int x1, int y1, int x2, int y2)
{
	square_to_pixel(&x1, &y1, x1, y1);
	square_to_pixel(&x2, &y2, x2, y2);
	XDrawRectangle(Metadpy->dpy, Infowin->win, clr[2]->gc, x1, y1,
	               x2 - x1 + Infofnt->wid - 1, y2 - y1 + Infofnt->hgt - 1);
}

/*
 * Mark a selection by drawing boxes around it (for now).
 */
static void mark_selection(void)
{
	co_ord min, max;
	term *old = Term;
	bool draw = s_ptr->select;
	bool clear = s_ptr->drawn;

	/* Open the correct term if necessary. */
	if (s_ptr->t != old) Term_activate(s_ptr->t);

	if (clear)
	{
		sort_co_ord(&min, &max, &s_ptr->init, &s_ptr->old);
		mark_selection_clear(min.x, min.y, max.x, max.y);
	}
	if (draw)
	{
		sort_co_ord(&min, &max, &s_ptr->init, &s_ptr->cur);
		mark_selection_mark(min.x, min.y, max.x, max.y);
	}

	/* Finish on the current term. */
	if (s_ptr->t != old) Term_activate(old);

	s_ptr->old.x = s_ptr->cur.x;
	s_ptr->old.y = s_ptr->cur.y;
	s_ptr->drawn = s_ptr->select;
}

/*
 * Forget a selection for one reason or another.
 */
static void copy_x11_release(void)
{
	/* Deselect the current selection. */
	s_ptr->select = FALSE;

	/* Remove its graphical represesntation. */
	mark_selection();
}

/*
 * Start to select some text on the screen.
 */
static void copy_x11_start(int x, int y)
{
	if (s_ptr->select) copy_x11_release();

	/* Remember where the selection started. */
	s_ptr->t = Term;
	s_ptr->init.x = s_ptr->cur.x = s_ptr->old.x = x;
	s_ptr->init.y = s_ptr->cur.y = s_ptr->old.y = y;
}

/*
 * Respond to movement of the mouse when selecting text.
 */
static void copy_x11_cont(int x, int y, unsigned int buttons)
{
	/* Use the nearest square within bounds if the mouse is outside. */
	x = MIN(MAX(x, 0), Term->wid - 1);
	y = MIN(MAX(y, 0), Term->hgt - 1);

	/* The left mouse button isn't pressed. */
	if (~buttons & Button1Mask) return;

	/* Not a selection in this window. */
	if (s_ptr->t != Term) return;

	/* Not enough movement. */
	if (x == s_ptr->old.x && y == s_ptr->old.y && s_ptr->select) return;

	/* Something is being selected. */
	s_ptr->select = TRUE;

	/* Track the selection. */
	s_ptr->cur.x = x;
	s_ptr->cur.y = y;

	/* Hack - display it inefficiently. */
	mark_selection();
}

/*
 * Respond to release of the left mouse button by putting the selected text in
 * the primary buffer.
 */
static void copy_x11_end(const Time time)
{
	/* No selection. */
	if (!s_ptr->select) return;

	/* Not a selection in this window. */
	if (s_ptr->t != Term) return;

	/* Remember when the selection was finalised. */
	s_ptr->time = time;

	/* Acquire the primary selection. */
	XSetSelectionOwner(Metadpy->dpy, XA_PRIMARY, Infowin->win, time);
	if (XGetSelectionOwner(Metadpy->dpy, XA_PRIMARY) != Infowin->win)
	{
		/* Failed to acquire the selection, so forget it. */
		bell();
		s_ptr->select = FALSE;
		mark_selection();
	}
}

/*
 * Send a message to request that the PRIMARY buffer be sent here.
 */
static void paste_x11_request(const Time time)
{
	XEvent event[1];
	XSelectionRequestEvent *ptr = &(event->xselectionrequest);

	/* Set various things. */
	ptr->type = SelectionRequest;
	ptr->display = Metadpy->dpy;
	ptr->owner = XGetSelectionOwner(Metadpy->dpy, XA_PRIMARY);
	ptr->requestor = Infowin->win;
	ptr->selection = XA_PRIMARY;
	ptr->target = XA_STRING;
	ptr->property = XA_STRING;  /* Unused */
	ptr->time = time;

	/* Check the owner. */
	if (ptr->owner == None)
	{
		/* No selection. */
		bell();
		return;
	}

	/* Send the SelectionRequest event. */
	XSendEvent(Metadpy->dpy, ptr->owner, False, NoEventMask, event);
}

/*
 * Add a character to a string in preparation for sending it to another
 * client as a STRING.
 * This doesn't change anything, as clients tend not to have difficulty in
 * receiving this format (although the standard specifies a restricted set).
 * Strings do not have a colour.
 */
static int add_char_string(char *buf, byte a, char c)
{
	*buf = c;
	return 1;
}

/*
 * Send some text requested by another X client.
 */
static void paste_x11_send(XSelectionRequestEvent *rq)
{
	XEvent event;
	XSelectionEvent *ptr = &(event.xselection);
	int (*add)(char *, byte, char) = 0;

	/* Set the event parameters. */
	ptr->type = SelectionNotify;
	ptr->property = rq->property;
	ptr->display = rq->display;
	ptr->requestor = rq->requestor;
	ptr->selection = rq->selection;
	ptr->target = rq->target;
	ptr->time = rq->time;

	/* Determine the correct "add a character" function.
	 * As Term->wid is at most 255, these can add up to 4 characters of
	 * output per character of input without problem.
	 * The mechanism will need to change if much more than this is needed.
	 */
	switch (rq->target)
	{
	case XA_STRING:
		add = add_char_string;
		break;
	default:
		goto error;
	}

	/* Reply to a known target received recently with data. */
	if (rq->time >= s_ptr->time && add)
	{
		char buf[1024];
		co_ord max, min;
		int x, y, l;
		byte a;
		char c;

		/* Work out which way around to paste. */
		sort_co_ord(&min, &max, &s_ptr->init, &s_ptr->cur);

		/* Paranoia. */
		if (XGetSelectionOwner(DPY, XA_PRIMARY) != WIN)
		{
			bell();
			goto error;
		}

		/* Delete the old value of the property. */
		XDeleteProperty(DPY, rq->requestor, rq->property);

		for (y = 0; y < Term->hgt; y++)
		{
			if (y < min.y) continue;
			if (y > max.y) break;

			for (x = l = 0; x < Term->wid; x++)
			{
				if (x < min.x) continue;
				if (x > max.x) break;

				/* Find the character. */
				Term_what(x, y, &a, &c);

				/* Add it. */
				l += (*add)(buf + l, a, c);
			}

			/* Terminate all but the last line in an appropriate way. */
			if (y != max.y) l += (*add)(buf + l, TERM_WHITE, '\n');

			/* Send the (non-empty) string. */
			XChangeProperty(DPY, rq->requestor, rq->property, rq->target, 8,
			                PropModeAppend, (unsigned char*)buf, l);
		}
	}
	else
	{
		/* Respond to all bad requests with property None. */
error:
		ptr->property = None;
	}

	/* Send whatever event we're left with. */
	XSendEvent(DPY, rq->requestor, FALSE, NoEventMask, &event);
}

extern errr type_string(char *str, uint len);

/*
 * Add the contents of the PRIMARY buffer to the input queue.
 *
 * Hack - This doesn't use the "time" of the event, and so accepts anything a
 * client tries to send it.
 */
static void paste_x11_accept(const XSelectionEvent *ptr)
{
	long offset;
	unsigned long left;

	/* Failure. */
	if (ptr->property == None)
	{
		bell();
		return;
	}

	if (ptr->selection != XA_PRIMARY)
	{
		bell();
		return;
	}
	if (ptr->target != XA_STRING)
	{
		bell();
		return;
	}

	for (offset = 0; ; offset += left)
	{
		errr err;

		/* A pointer for the pasted information. */
		unsigned char *data;

		Atom type;
		int fmt;
		unsigned long nitems;

		/* Set data to the string, and catch errors. */
		if (XGetWindowProperty(Metadpy->dpy, Infowin->win, XA_STRING, offset,
		                       32767, TRUE, XA_STRING, &type, &fmt, &nitems, &left, &data)
		                != Success) break;

		/* Paste the text. */
		err = type_string((char*)data, (uint)nitems);

		/* Free the data pasted. */
		XFree(data);

		/* No room. */
		if (err == 7)
		{
			bell();
			break;
		}
		/* Paranoia? - strange errors. */
		else if (err)
		{
			break;
		}

		/* Pasted everything. */
		if (!left) return;
	}

	/* An error has occurred, so free the last bit of data before returning. */
	XFree(data);
}

/*
 * Handle various events conditional on presses of a mouse button.
 */
static void handle_button(Time time, int x, int y, int button,
                          bool press)
{
	/* The co-ordinates are only used in Angband format. */
	pixel_to_square(&x, &y, x, y);

	if (press && button == 1) copy_x11_start(x, y);
	if (!press && button == 1) copy_x11_end(time);
	if (!press && button == 2) paste_x11_request(time);
}


/*
 * Process events
 */
static errr CheckEvent(bool wait)
{
	term_data *old_td = (term_data*)(Term->data);

	XEvent xev_body, *xev = &xev_body;

	term_data *td = NULL;
	infowin *iwin = NULL;

	int i;


	/* Do not wait unless requested */
	if (!wait && !XPending(Metadpy->dpy)) return (1);

	/* Hack - redraw the selection, if needed.
	* This doesn't actually check that one of its squares was drawn to,
	* only that this may have happened.
	*/
	if (s_ptr->select && !s_ptr->drawn) mark_selection();

	/* Load the Event */
	XNextEvent(Metadpy->dpy, xev);


	/* Notice new keymaps */
	if (xev->type == MappingNotify)
	{
		XRefreshKeyboardMapping(&xev->xmapping);
		return 0;
	}


	/* Scan the windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		if (xev->xany.window == data[i].win->win)
		{
			td = &data[i];
			iwin = td->win;
			break;
		}
	}

	/* Unknown window */
	if (!td || !iwin) return (0);


	/* Hack -- activate the Term */
	Term_activate(&td->t);

	/* Hack -- activate the window */
	Infowin_set(iwin);


	/* Switch on the Type */
	switch (xev->type)
	{

	case ButtonPress:
	case ButtonRelease:
		{
			bool press = (xev->type == ButtonPress);

			/* Where is the mouse */
			int x = xev->xbutton.x;
			int y = xev->xbutton.y;

			int z;

			/* Which button is involved */
			if (xev->xbutton.button == Button1) z = 1;
			else if (xev->xbutton.button == Button2) z = 2;
			else if (xev->xbutton.button == Button3) z = 3;
			else if (xev->xbutton.button == Button4) z = 4;
			else if (xev->xbutton.button == Button5) z = 5;
			else z = 0;

			/* Where is the mouse */
			x = xev->xbutton.x;
			y = xev->xbutton.y;

			/* XXX Handle */
			handle_button(xev->xbutton.time, x, y, z, press);

			break;
		}

	case EnterNotify:
	case LeaveNotify:
		{
			/* Where is the mouse */
			/* XXX Handle */

			break;
		}

	case MotionNotify:
		{
			int x = xev->xmotion.x;
			int y = xev->xmotion.y;
			unsigned int z = xev->xmotion.state;

			/* Convert to co-ordinates Angband understands. */
			pixel_to_square(&x, &y, x, y);

			/* Alter the selection if appropriate. */
			copy_x11_cont(x, y, z);

			/* XXX Handle */

			break;
		}

	case SelectionNotify:
		{
			paste_x11_accept(&(xev->xselection));
			break;
		}

	case SelectionRequest:
		{
			paste_x11_send(&(xev->xselectionrequest));
			break;
		}

	case SelectionClear:
		{
			s_ptr->select = FALSE;
			mark_selection();
			break;
		}

	case KeyRelease:
		{
			/* Nothing */
			break;
		}

	case KeyPress:
		{
			/* Hack -- use "old" term */
			Term_activate(&old_td->t);

			/* Process the key */
			react_keypress(&(xev->xkey));

			break;
		}

	case Expose:
		{
			/* Ignore "extra" exposes */
			if (xev->xexpose.count) break;

			/* Clear the window */
			Infowin_wipe();

			/* Redraw */
			Term_redraw();

			break;
		}

	case MapNotify:
		{
			Infowin->mapped = 1;
			Term->mapped_flag = TRUE;
			break;
		}

	case UnmapNotify:
		{
			Infowin->mapped = 0;
			Term->mapped_flag = FALSE;
			break;
		}

		/* Move and/or Resize */
	case ConfigureNotify:
		{
			int cols, rows, wid, hgt;

			int ox = Infowin->ox;
			int oy = Infowin->oy;

			/* Save the new Window Parms */
			Infowin->x = xev->xconfigure.x;
			Infowin->y = xev->xconfigure.y;
			Infowin->w = xev->xconfigure.width;
			Infowin->h = xev->xconfigure.height;

			/* Determine "proper" number of rows/cols */
			cols = ((Infowin->w - (ox + ox)) / td->fnt->wid);
			rows = ((Infowin->h - (oy + oy)) / td->fnt->hgt);

			/* Hack -- minimal size */
			if (td == &data[0])
			{
				if (cols < 80) cols = 80;
				if (rows < 24) rows = 24;
			}

			else
			{
				if (cols < 1) cols = 1;
				if (rows < 1) rows = 1;
			}

			/* Paranoia */
			if (cols > 255) cols = 255;
			if (rows > 255) rows = 255;

			/* Desired size of window */
			wid = cols * td->fnt->wid + (ox + ox);
			hgt = rows * td->fnt->hgt + (oy + oy);

			/* Resize the Term (if needed) */
			Term_resize(cols, rows);
			break;
		}
	}


	/* Hack -- Activate the old term */
	Term_activate(&old_td->t);

	/* Hack -- Activate the proper window */
	Infowin_set(old_td->win);


	/* Success */
	return (0);
}


/*
 * Handle "activation" of a term
 */
static errr Term_xtra_x11_level(int v)
{
	term_data *td = (term_data*)(Term->data);

	/* Handle "activate" */
	if (v)
	{
		/* Activate the window */
		Infowin_set(td->win);

		/* Activate the font */
		Infofnt_set(td->fnt);
	}

	/* Success */
	return (0);
}


/*
 * React to changes
 */
static errr Term_xtra_x11_react(void)
{
	int i;

	if (Metadpy->color)
	{
		/* Check the colors */
		for (i = 0; i < 256; i++)
		{
			if ((color_table[i][0] != angband_color_table[i][0]) ||
			                (color_table[i][1] != angband_color_table[i][1]) ||
			                (color_table[i][2] != angband_color_table[i][2]) ||
			                (color_table[i][3] != angband_color_table[i][3]))
			{
				Pixell pixel;

				/* Save new values */
				color_table[i][0] = angband_color_table[i][0];
				color_table[i][1] = angband_color_table[i][1];
				color_table[i][2] = angband_color_table[i][2];
				color_table[i][3] = angband_color_table[i][3];

				/* Create pixel */
				pixel = create_pixel(Metadpy->dpy,
				                     color_table[i][1],
				                     color_table[i][2],
				                     color_table[i][3]);

				/* Change the foreground */
				Infoclr_set(clr[i]);
				Infoclr_change_fg(pixel);
			}
		}
	}

	/* Success */
	return (0);
}


/*
 * Handle a "special request"
 */
static errr Term_xtra_x11(int n, int v)
{
	/* Handle a subset of the legal requests */
	switch (n)
	{
		/* Make a noise */
	case TERM_XTRA_NOISE:
		Metadpy_do_beep(); return (0);

		/* Flush the output XXX XXX */
	case TERM_XTRA_FRESH: Metadpy_update(1, 0, 0); return (0);

		/* Process random events XXX */
	case TERM_XTRA_BORED:
		{
			irc_poll();

			return (CheckEvent(0));
		}

		/* Process Events XXX */
	case TERM_XTRA_EVENT:
		{
			irc_poll();

			return (CheckEvent(v));
		}

		/* Flush the events XXX */
	case TERM_XTRA_FLUSH: while (!CheckEvent(FALSE)); return (0);

		/* Handle change in the "level" */
	case TERM_XTRA_LEVEL: return (Term_xtra_x11_level(v));

		/* Clear the screen, and redraw any selection later. */
	case TERM_XTRA_CLEAR: Infowin_wipe(); s_ptr->drawn = FALSE; return (0);

		/* Delay for some milliseconds */
	case TERM_XTRA_DELAY:
		irc_poll();

		usleep(1000 * v);
		return (0);

		/* Get Delay of some milliseconds */
	case TERM_XTRA_GET_DELAY:
		{
			int ret;
			struct timeval tv;

			ret = gettimeofday(&tv, NULL);
			Term_xtra_long = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);

			return ret;
		}

		/* Subdirectory scan */
	case TERM_XTRA_SCANSUBDIR:
		{
			DIR *directory;
			struct dirent *entry;

			scansubdir_max = 0;

			directory = opendir(scansubdir_dir);
			if (!directory)
				return 1;

			while ((entry = readdir(directory)))
			{
				char file[PATH_MAX + NAME_MAX + 2];
				struct stat filedata;

				file[PATH_MAX + NAME_MAX] = 0;
				strncpy(file, scansubdir_dir, PATH_MAX);
				strncat(file, "/", 2);
				strncat(file, entry->d_name, NAME_MAX);
				if (!stat(file, &filedata) && S_ISDIR((filedata.st_mode)))
				{
					string_free(scansubdir_result[scansubdir_max]);
					scansubdir_result[scansubdir_max] = string_make(entry->d_name);
					++scansubdir_max;
				}
			}

			closedir(directory);
			return 0;
		}

		/* React to changes */
	case TERM_XTRA_REACT: return (Term_xtra_x11_react());

		/* Rename main window */
	case TERM_XTRA_RENAME_MAIN_WIN: Infowin_set_name(angband_term_name[0]); return (0);
	}

	/* Unknown */
	return (1);
}


/*
 * Draw the cursor as an inverted rectangle.
 *
 * Consider a rectangular outline like "main-mac.c".  XXX XXX
 */
static errr Term_curs_x11(int x, int y)
{
	/* Draw the cursor */
	Infoclr_set(xor);

	if (use_bigtile && x + 1 < Term->wid && Term->old->a[y][x + 1] == 255)
		Infofnt_text_non(x, y, "  ", 2);
	else
		/* Hilite the cursor character */
		Infofnt_text_non(x, y, " ", 1);

	/* Redraw the selection if any, as it may have been obscured. (later) */
	s_ptr->drawn = FALSE;

	/* Success */
	return (0);
}


/*
 * Erase some characters.
 */
static errr Term_wipe_x11(int x, int y, int n)
{
	/* Erase (use black) */
	Infoclr_set(clr[TERM_DARK]);

	/* Mega-Hack -- Erase some space */
	Infofnt_text_non(x, y, "", n);

	/* Redraw the selection if any, as it may have been obscured. (later) */
	s_ptr->drawn = FALSE;

	/* Success */
	return (0);
}


/*
 * Draw some textual characters.
 */
static errr Term_text_x11(int x, int y, int n, byte a, cptr s)
{
	/* Draw the text */
	Infoclr_set(clr[a]);

	/* Draw the text */
	Infofnt_text_std(x, y, s, n);

	/* Success */
	return (0);
}


#ifdef USE_GRAPHICS

/*
 * Draw some graphical characters.
 */
# ifdef USE_TRANSPARENCY
# ifdef USE_EGO_GRAPHICS
static errr Term_pict_x11(int x, int y, int n, const byte *ap, const char *cp,
                          const byte *tap, const char *tcp, const byte *eap, const char *ecp)
# else /* USE_EGO_GRAPHICS */
static errr Term_pict_x11(int x, int y, int n, const byte *ap, const char *cp,
                          const byte *tap, const char *tcp)
# endif  /* USE_EGO_GRAPHICS */
# else /* USE_TRANSPARENCY */
static errr Term_pict_x11(int x, int y, int n, const byte *ap, const char *cp)
# endif  /* USE_TRANSPARENCY */
{
	int i, x1, y1;

	byte a;
	char c;


#ifdef USE_TRANSPARENCY
	byte ta;
	char tc;
	int x2, y2;

# ifdef USE_EGO_GRAPHICS
	byte ea;
	char ec;
	int x3, y3;
	bool has_overlay;
# endif  /* USE_EGO_GRAPHICS */

	int k, l;

	unsigned long pixel, blank;
#endif /* USE_TRANSPARENCY */

	term_data *td = (term_data*)(Term->data);

	y *= Infofnt->hgt;
	x *= Infofnt->wid;

	/* Add in affect of window boundaries */
	y += Infowin->oy;
	x += Infowin->ox;

	for (i = 0; i < n; ++i, x += td->fnt->wid)
	{
		a = *ap++;
		c = *cp++;

		/* For extra speed - cache these values */
		x1 = (c & 0x7F) * td->fnt->twid;
		y1 = (a & 0x7F) * td->fnt->hgt;

#ifdef USE_TRANSPARENCY

		ta = *tap++;
		tc = *tcp++;

		/* For extra speed - cache these values */
		x2 = (tc & 0x7F) * td->fnt->twid;
		y2 = (ta & 0x7F) * td->fnt->hgt;

# ifdef USE_EGO_GRAPHICS

		ea = *eap++;
		ec = *ecp++;
		has_overlay = (ea && ec);

		/* For extra speed - cache these values too */
		x3 = (ec & 0x7F) * td->fnt->twid;
		y3 = (ea & 0x7F) * td->fnt->hgt;

# endif  /* USE_EGO_GRAPHICS */

		/* Optimise the common case */
		if ((x1 == x2) && (y1 == y2))
		{
# ifndef USE_EGO_GRAPHICS

			/* Draw object / terrain */
			XPutImage(Metadpy->dpy, td->win->win,
			          clr[0]->gc,
			          td->tiles,
			          x1, y1,
			          x, y,
			          td->fnt->twid, td->fnt->hgt);
# else /* !USE_EGO_GRAPHICS */

			/* Draw object / terrain */
			if (!has_overlay)
			{
				XPutImage(Metadpy->dpy, td->win->win,
				          clr[0]->gc,
				          td->tiles,
				          x1, y1,
				          x, y,
				          td->fnt->twid, td->fnt->hgt);
			}

			/* There's a terrain overlay */
			else
			{
				/* Mega Hack^2 - assume the top left corner is "black" */
				blank = XGetPixel(td->tiles, 0, td->fnt->hgt * 6);
				for (k = 0; k < td->fnt->twid; k++)
				{
					for (l = 0; l < td->fnt->hgt; l++)
					{
						/* If mask set in overlay... */
						if ((pixel = XGetPixel(td->tiles, x3 + k, y3 + l)) == blank)
						{
							/* Output from the terrain */
							pixel = XGetPixel(td->tiles, x1 + k, y1 + l);
						}

						/* Store into the temp storage. */
						XPutPixel(td->TmpImage, k, l, pixel);
					}
				}

				/* Draw to screen */
				XPutImage(Metadpy->dpy, td->win->win,
				          clr[0]->gc,
				          td->TmpImage,
				          0, 0, x, y,
				          td->fnt->twid, td->fnt->hgt);
			}

# endif  /* !USE_EGO_GRAPHICS */

		}
		else
		{

			/* Mega Hack^2 - assume the top left corner is "black" */
			blank = XGetPixel(td->tiles, 0, td->fnt->hgt * 6);

# ifndef USE_EGO_GRAPHICS

			for (k = 0; k < td->fnt->twid; k++)
			{
				for (l = 0; l < td->fnt->hgt; l++)
				{
					/* If mask set... */
					if ((pixel = XGetPixel(td->tiles, x1 + k, y1 + l)) == blank)
					{
						/* Output from the terrain */
						pixel = XGetPixel(td->tiles, x2 + k, y2 + l);
					}

					/* Store into the temp storage. */
					XPutPixel(td->TmpImage, k, l, pixel);
				}
			}

# else /* !USE_EGO_GRAPHICS */

			for (k = 0; k < td->fnt->twid; k++)
			{
				for (l = 0; l < td->fnt->hgt; l++)
				{
					/* Overlay */
					if (has_overlay)
					{
						pixel = XGetPixel(td->tiles, x3 + k, y3 + l);
					}

					/* Hack -- No overlay */
					else
					{
						pixel = blank;
					}

					/* If it's blank... */
					if (pixel == blank)
					{
						/* Look at mon/obj */
						pixel = XGetPixel(td->tiles, x1 + k, y1 + l);
					}

					/* If it's blank too, use terrain */
					if (pixel == blank)
					{
						pixel = XGetPixel(td->tiles, x2 + k, y2 + l);
					}

					/* Store into the temp storage. */
					XPutPixel(td->TmpImage, k, l, pixel);
				}
			}

# endif  /* !USE_EGO_GRAPHICS */


			/* Draw to screen */
			XPutImage(Metadpy->dpy, td->win->win,
			          clr[0]->gc,
			          td->TmpImage,
			          0, 0, x, y,
			          td->fnt->twid, td->fnt->hgt);
		}

#else /* USE_TRANSPARENCY */

/* Draw object / terrain */
		XPutImage(Metadpy->dpy, td->win->win,
		          clr[0]->gc,
		          td->tiles,
		          x1, y1,
		          x, y,
		          td->fnt->twid, td->fnt->hgt);

#endif /* USE_TRANSPARENCY */
		x += td->fnt->wid;
	}

	/* Redraw the selection if any, as it may have been obscured. (later) */
	s_ptr->drawn = FALSE;

	/* Success */
	return (0);
}

#endif /* USE_GRAPHICS */



/*
 * Initialize a term_data
 */
static errr term_data_init(term_data *td, int i)
{
	term *t = &td->t;

	cptr name = angband_term_name[i];

	cptr font;

	int x = 0;
	int y = 0;

	int cols = 80;
	int rows = 24;

	int ox = 1;
	int oy = 1;

	int wid, hgt, num;

	char buf[80];

	cptr str;

	int val;

	XClassHint *ch;

	char res_name[20];
	char res_class[20];

	XSizeHints *sh;


	/* Window specific font name */
	sprintf(buf, "ANGBAND_X11_FONT_%d", i);

	/* Check environment for that font */
	font = getenv(buf);

	/* Check environment for "base" font */
	if (!font) font = getenv("ANGBAND_X11_FONT");

	/* No environment variables, use default font */
	if (!font)
	{
		switch (i)
		{
		case 0:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		case 1:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		case 2:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		case 3:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		case 4:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		case 5:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		case 6:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		case 7:
			{
				font = DEFAULT_X11_FONT;
			}
			break;
		default:
			{
				font = DEFAULT_X11_FONT;
			}
		}
	}

	/* Window specific location (x) */
	sprintf(buf, "ANGBAND_X11_AT_X_%d", i);
	str = getenv(buf);
	x = (str != NULL) ? atoi(str) : -1;

	/* Window specific location (y) */
	sprintf(buf, "ANGBAND_X11_AT_Y_%d", i);
	str = getenv(buf);
	y = (str != NULL) ? atoi(str) : -1;


	/* Window specific cols */
	sprintf(buf, "ANGBAND_X11_COLS_%d", i);
	str = getenv(buf);
	val = (str != NULL) ? atoi(str) : -1;
	if (val > 0) cols = val;

	/* Window specific rows */
	sprintf(buf, "ANGBAND_X11_ROWS_%d", i);
	str = getenv(buf);
	val = (str != NULL) ? atoi(str) : -1;
	if (val > 0) rows = val;


	/* Window specific inner border offset (ox) */
	sprintf(buf, "ANGBAND_X11_IBOX_%d", i);
	str = getenv(buf);
	val = (str != NULL) ? atoi(str) : -1;
	if (val > 0) ox = val;

	/* Window specific inner border offset (oy) */
	sprintf(buf, "ANGBAND_X11_IBOY_%d", i);
	str = getenv(buf);
	val = (str != NULL) ? atoi(str) : -1;
	if (val > 0) oy = val;


	/* Prepare the standard font */
	MAKE(td->fnt, infofnt);
	Infofnt_set(td->fnt);
	Infofnt_init_data(font);

	/* Hack -- key buffer size */
	num = (i == 0 ? 1024 : 16);

	/* Assume full size windows */
	wid = cols * td->fnt->wid + (ox + ox);
	hgt = rows * td->fnt->hgt + (oy + oy);

	/* Create a top-window */
	MAKE(td->win, infowin);
	Infowin_set(td->win);
	Infowin_init_top(x, y, wid, hgt, 0,
	                 Metadpy->fg, Metadpy->bg);

	/* Ask for certain events */
	Infowin_set_mask(ExposureMask | StructureNotifyMask | KeyPressMask |
	                 PointerMotionMask | ButtonPressMask | ButtonReleaseMask);

	/* Set the window name */
	Infowin_set_name(name);

	/* Save the inner border */
	Infowin->ox = ox;
	Infowin->oy = oy;

	/* Make Class Hints */
	ch = XAllocClassHint();

	if (ch == NULL) quit("XAllocClassHint failed");

	strcpy(res_name, name);
	res_name[0] = FORCELOWER(res_name[0]);
	ch->res_name = res_name;

	strcpy(res_class, "Angband");
	ch->res_class = res_class;

	XSetClassHint(Metadpy->dpy, Infowin->win, ch);

	/* Make Size Hints */
	sh = XAllocSizeHints();

	/* Oops */
	if (sh == NULL) quit("XAllocSizeHints failed");

	/* Fixed window size */
	if (i == 0)
	{
		/* Main window: 80x24 -- 255x255 */
		sh->flags = PMinSize | PMaxSize;
		sh->min_width = 80 * td->fnt->wid + (ox + ox);
		sh->min_height = 24 * td->fnt->hgt + (oy + oy);
		sh->max_width = 255 * td->fnt->wid + (ox + ox);
		sh->max_height = 255 * td->fnt->hgt + (oy + oy);
	}

	/* Variable window size */
	else
	{
		/* Subwindows: 1x1 -- 255x255 */
		sh->flags = PMinSize | PMaxSize;
		sh->min_width = td->fnt->wid + (ox + ox);
		sh->min_height = td->fnt->hgt + (oy + oy);
		sh->max_width = 255 * td->fnt->wid + (ox + ox);
		sh->max_height = 255 * td->fnt->hgt + (oy + oy);
	}

	/* Resize increment */
	sh->flags |= PResizeInc;
	sh->width_inc = td->fnt->wid;
	sh->height_inc = td->fnt->hgt;

	/* Base window size */
	sh->flags |= PBaseSize;
	sh->base_width = (ox + ox);
	sh->base_height = (oy + oy);

	/* Use the size hints */
	XSetWMNormalHints(Metadpy->dpy, Infowin->win, sh);

	/* Map the window */
	Infowin_map();


	/* Move the window to requested location */
	if ((x >= 0) && (y >= 0)) Infowin_impell(x, y);


	/* Initialize the term */
	term_init(t, cols, rows, num);

	/* Use a "soft" cursor */
	t->soft_cursor = TRUE;

	/* Erase with "white space" */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

	/* Hooks */
	t->xtra_hook = Term_xtra_x11;
	t->curs_hook = Term_curs_x11;
	t->wipe_hook = Term_wipe_x11;
	t->text_hook = Term_text_x11;

	/* Save the data */
	t->data = td;

	/* Activate (important) */
	Term_activate(t);

	/* Success */
	return (0);
}


/*
 * Initialization function for an "X11" module to Angband
 */
errr init_x11(int argc, char *argv[])
{
	int i;

	cptr dpy_name = "";

	int num_term = 1;

#ifdef USE_GRAPHICS

	char filename[1024];

	int pict_wid = 0;
	int pict_hgt = 0;
	bool force_old_graphics = FALSE;

#ifdef USE_TRANSPARENCY

	char *TmpData;
#endif /* USE_TRANSPARENCY */

#endif /* USE_GRAPHICS */


	/* Parse args */
	for (i = 1; i < argc; i++)
	{
		if (prefix(argv[i], "-d"))
		{
			dpy_name = &argv[i][2];
			continue;
		}

#ifdef USE_GRAPHICS

		if (prefix(argv[i], "-s"))
		{
			smoothRescaling = FALSE;
			continue;
		}

		if (prefix(argv[i], "-o"))
		{
			force_old_graphics = TRUE;
			continue;
		}

		if (prefix(argv[i], "-b"))
		{
			arg_bigtile = use_bigtile = TRUE;
			continue;
		}

#endif /* USE_GRAPHICS */

		if (prefix(argv[i], "-n"))
		{
			num_term = atoi(&argv[i][2]);
			if (num_term > MAX_TERM_DATA) num_term = MAX_TERM_DATA;
			else if (num_term < 1) num_term = 1;
			continue;
		}

		plog_fmt("Ignoring option: %s", argv[i]);
	}


	/* Init the Metadpy if possible */
	if (Metadpy_init_name(dpy_name)) return ( -1);


	/* Prepare cursor color */
	MAKE(xor, infoclr);
	Infoclr_set(xor);
	Infoclr_init_ppn(Metadpy->fg, Metadpy->bg, "xor", 0);


	/* Prepare normal colors */
	for (i = 0; i < 256; ++i)
	{
		Pixell pixel;

		MAKE(clr[i], infoclr);

		Infoclr_set(clr[i]);

		/* Acquire Angband colors */
		color_table[i][0] = angband_color_table[i][0];
		color_table[i][1] = angband_color_table[i][1];
		color_table[i][2] = angband_color_table[i][2];
		color_table[i][3] = angband_color_table[i][3];

		/* Default to monochrome */
		pixel = ((i == 0) ? Metadpy->bg : Metadpy->fg);

		/* Handle color */
		if (Metadpy->color)
		{
			/* Create pixel */
			pixel = create_pixel(Metadpy->dpy,
			                     color_table[i][1],
			                     color_table[i][2],
			                     color_table[i][3]);
		}

		/* Initialize the color */
		Infoclr_init_ppn(pixel, Metadpy->bg, "cpy", 0);
	}


	/* Initialize the windows */
	for (i = 0; i < num_term; i++)
	{
		term_data *td = &data[i];

		/* Initialize the term_data */
		term_data_init(td, i);

		/* Save global entry */
		angband_term[i] = Term;
	}

	/* Raise the "Angband" window */
	Infowin_set(data[0].win);
	Infowin_raise();

	/* Activate the "Angband" window screen */
	Term_activate(&data[0].t);


#ifdef USE_GRAPHICS

	/* Try graphics */
	if (arg_graphics)
	{
		/* Try the "16x16.bmp" file */
		path_build(filename, 1024, ANGBAND_DIR_XTRA, "graf/16x16.bmp");

		/* Use the "16x16.bmp" file if it exists */
		if (!force_old_graphics &&
		                (0 == fd_close(fd_open(filename, O_RDONLY))))
		{
			/* Use graphics */
			use_graphics = TRUE;

			pict_wid = pict_hgt = 16;

			ANGBAND_GRAF = "new";
		}
		else
		{
			/* Try the "8x8.bmp" file */
			path_build(filename, 1024, ANGBAND_DIR_XTRA, "graf/8x8.bmp");

			/* Use the "8x8.bmp" file if it exists */
			if (0 == fd_close(fd_open(filename, O_RDONLY)))
			{
				/* Use graphics */
				use_graphics = TRUE;

				pict_wid = pict_hgt = 8;

				ANGBAND_GRAF = "old";
			}
		}
	}

	/* Load graphics */
	if (use_graphics)
	{
		Display *dpy = Metadpy->dpy;

		XImage *tiles_raw;

		/* Load the graphical tiles */
		tiles_raw = ReadBMP(dpy, filename);

		/* Initialize the windows */
		for (i = 0; i < num_term; i++)
		{
			term_data *td = &data[i];

			term *t = &td->t;

			/* Graphics hook */
			t->pict_hook = Term_pict_x11;

			/* Use graphics sometimes */
			t->higher_pict = TRUE;

			/* Resize tiles */
			td->tiles =
			        ResizeImage(dpy, tiles_raw,
			                    pict_wid, pict_hgt,
			                    td->fnt->twid, td->fnt->hgt);
		}

#ifdef USE_TRANSPARENCY
		/* Initialize the transparency masks */
		for (i = 0; i < num_term; i++)
		{
			term_data *td = &data[i];
			int ii, jj;
			int depth = DefaultDepth(dpy, DefaultScreen(dpy));
			Visual *visual = DefaultVisual(dpy, DefaultScreen(dpy));
			int total;


			/* Determine total bytes needed for image */
			ii = 1;
			jj = (depth - 1) >> 2;
			while (jj >>= 1) ii <<= 1;
			total = td->fnt->twid * td->fnt->hgt * ii;


			TmpData = (char *)malloc(total);

			td->TmpImage = XCreateImage(dpy, visual, depth,
			                            ZPixmap, 0, TmpData,
			                            td->fnt->twid, td->fnt->hgt, 8, 0);

		}
#endif /* USE_TRANSPARENCY */


		/* Free tiles_raw? XXX XXX */
	}

#endif /* USE_GRAPHICS */


	/* Success */
	return (0);
}

#endif /* USE_X11 */

