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



#include "angband.h"


#ifdef USE_X11

#include "main.h"

#ifndef __MAKEDEPEND__
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include <X11/Xatom.h>
#endif /* __MAKEDEPEND__ */


/*
 * Include some helpful X11 code.
 */
#include "maid-x11.h"


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

	unsigned int width;
	unsigned int height;
	unsigned int depth;

	Pixell black;
	Pixell white;

	Pixell bg;
	Pixell fg;
	Pixell zg;

	unsigned int mono:1;
	unsigned int color:1;
	unsigned int nuke:1;
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
 *      - The saved (startup) location of the window
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
	s16b x_save, y_save;
	s16b w, h;
	u16b b;

	byte byte1;

	unsigned int mapped:1;
	unsigned int redraw:1;
	unsigned int resize:1;

	unsigned int nuke:1;

	unsigned int flag1:1;
	unsigned int flag2:1;
	unsigned int flag3:1;
	unsigned int flag4:1;
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

	unsigned int code:4;
	unsigned int stip:1;
	unsigned int nuke:1;
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
	s16b hgt;
	s16b asc;

	byte off;

	unsigned int mono:1;
	unsigned int nuke:1;
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


/*
 * Actual color table
 */
static infoclr *clr[MAX_COLORS];



/**** Generic code ****/


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
		if (!dpy) return (-1);

		/* We will have to nuke it when done */
		m->nuke = 1;
	}

	/* Since the Display was given, use it */
	else
	{
		/* We will not have to nuke it when done */
		m->nuke = 0;
	}


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
	m->zg = ((Pixell)1 << m->depth) - 1;

	/* Save various default Flag Settings */
	m->color = ((m->depth > 1) ? 1 : 0);
	m->mono = ((m->color) ? 0 : 1);

	/* Return "success" */
	return (0);
}


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
	(void)my_strcpy(buf, name, sizeof(buf));
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
	(void)my_strcpy(buf, name, sizeof(buf));
	st = XStringListToTextProperty(&bp, 1, &tp);
	if (st) XSetWMIconName(Metadpy->dpy, Infowin->win, &tp);
	return (0);
}

#endif /* IGNORE_UNUSED_FUNCTIONS */


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
	iwin->x_save = x;
	iwin->y_save = y;
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

#endif /* IGNORE_UNUSED_FUNCTIONS */


/*
 * Resize an infowin
 */
static errr Infowin_resize(int w, int h)
{
	/* Execute the request */
	XResizeWindow(Metadpy->dpy, Infowin->win, w, h);

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
			return (atoi(opcode_pairs[i*2+1]));
		}
	}

	/* The code was not found, return -1 */
	return (-1);
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

#endif /* IGNORE_UNUSED_FUNCTIONS */


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
	if (bg > Metadpy->zg) return (-1);
	if (fg > Metadpy->zg) return (-1);

	/* Check the data for trueness */
	if ((op < 0) || (op > 15)) return (-1);


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
	if (fg > Metadpy->zg) return (-1);


	/*** Change ***/

	/* Change */
	XSetForeground(Metadpy->dpy, iclr->gc, fg);

	/* Success */
	return (0);
}



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
		(void)string_free(ifnt->name);
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
	if (!name) return (-1);

	/* Attempt to load the font */
	info = XLoadQueryFont(Metadpy->dpy, name);

	/* The load failed, try to recover */
	if (!info) return (-1);


	/*** Init the font ***/

	/* Wipe the thing */
	(void)WIPE(Infofnt, infofnt);

	/* Attempt to prepare it */
	if (Infofnt_prepare(info))
	{
		/* Free the font */
		XFreeFont(Metadpy->dpy, info);

		/* Fail */
		return (-1);
	}

	/* Save a copy of the font name */
	Infofnt->name = string_make(name);

	/* Mark it as nukable */
	Infofnt->nuke = 1;

	/* HACK - force all fonts to be printed character by character */
	Infofnt->mono = 1;

	/* Success */
	return (0);
}


/*
 * Standard Text
 */
static errr Infofnt_text_std(int x, int y, cptr str, int len)
{
	int i;
	int w, h;


	/*** Do a brief info analysis ***/

	/* Do nothing if the string is null */
	if (!str || !*str) return (-1);

	/* Get the length of the string */
	if (len < 0) len = strlen(str);


	/*** Decide where to place the string, vertically ***/

	/* Ignore Vertical Justifications */
	y = (y * Infofnt->hgt) + Infowin->oy;


	/*** Decide where to place the string, horizontally ***/

	/* Line up with x at left edge of column 'x' */
	x = (x * Infofnt->wid) + Infowin->ox;


	/*** Erase the background ***/

	/* The total width will be 'len' chars * standard width */
	w = len * Infofnt->wid;

	/* Simply do 'td->Infofnt->hgt' (a singlw row) high */
	h = Infofnt->hgt;

	/* Fill the background */
	XFillRectangle(Metadpy->dpy, Infowin->win, clr[TERM_DARK]->gc, x, y, w, h);


	/*** Actually draw 'str' onto the infowin ***/

	/* Be sure the correct font is ready */
	XSetFont(Metadpy->dpy, Infoclr->gc, Infofnt->info->fid);


	y += Infofnt->asc;


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

	/* Assume monospaced font */
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
 * Color info (unused, red, green, blue).
 */
static byte x11_color_table[MAX_COLORS][4];

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

	infofnt *fnt;  /* Current font */

	infofnt *nfnt; /* Normal font */

	infofnt *sfnt; /* Small font for hot bigscreen */

	infowin *win;

#ifdef USE_GRAPHICS

	XImage *tiles;

	/* Tempory storage for overlaying tiles. */
	XImage *TmpImage;

#endif /* USE_GRAPHICS */

	/* Pointers to allocated data, needed to clear up memory */
	XClassHint *classh;
	XSizeHints *sizeh;
};


/*
 * The number of term data structures
 */
#define MAX_TERM_DATA 8

/*
 * The array of term data structures
 */
static term_data data[MAX_TERM_DATA];


/*
 * Path to the X11 settings file
 */
static char settings[1024];


/* Use short names for the most commonly used elements of various structures. */
#define DPY (Metadpy->dpy)
#define WIN (Infowin->win)


/*
 * Remember the number of terminal windows open
 */
static int term_windows_open;


/* Describe a set of co-ordinates. */
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
	bool select; /* The selection is currently in use. */
	bool drawn; /* The selection is currently displayed. */
	term *t; /* The window where the selection is found. */
	co_ord init; /* The starting co-ordinates. */
	co_ord cur; /* The end co-ordinates (the current ones if still copying). */
	co_ord old; /* The previous end co-ordinates. */
	Time time; /* The time at which the selection was finalised. */
};

static x11_selection_type x11_selection[1];



/*
 * Change the number of text lines shown on the screen.
 *
 * We only switch between 25-line display, using a font taller than it is
 * wide, and 50-line display, using a font of equal height and width.
 */
static errr Term_rows_x11(bool fifty_rows)
{
	term_data *td = &data[0]; /* Main window */

	/* Set to 50-row display */
	if (fifty_rows)
	{
		screen_rows = 50;

		/* Change to small font */
		td->fnt = td->sfnt;
	}

	/* Set to 25-line display */
	else
	{
		screen_rows = 25;

		/* Change to normal font */
		td->fnt = td->nfnt;
	}

	/* Activate the window */
	Infowin_set(td->win);

	/* Activate the font */
	Infofnt_set(td->fnt);

	/* Clear screen */
	(void)Infowin_wipe();

	/* Assume success */
	return (0);
}


/*
 * Process a keypress event
 *
 * Also appears in "main-xaw.c".
 */
static void react_keypress(XKeyEvent *ev)
{
	int i, n, mc, ms, mo, mx;

	unsigned int ks1;

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
	ks1 = (unsigned int)(ks);

	/* Extract four "modifier flags" */
	mc = (ev->state & ControlMask) ? TRUE : FALSE;
	ms = (ev->state & ShiftMask) ? TRUE : FALSE;
	mo = (ev->state & Mod1Mask) ? TRUE : FALSE;
	mx = (ev->state & Mod2Mask) ? TRUE : FALSE;


	/* Normal keys with no modifiers */
	if (n && !mo && !mx && !IsSpecialKey(ks))
	{
		/* Enqueue the normal key(s) */
		for (i = 0; buf[i]; i++) (void)Term_keypress(buf[i]);

		/* All done */
		return;
	}


	/* Handle a few standard keys (bypass modifiers) XXX XXX XXX */
	switch (ks1)
	{
		case XK_Escape:
		{
			(void)Term_keypress(ESCAPE);
			return;
		}

		case XK_Return:
		{
			(void)Term_keypress('\r');
			return;
		}

		case XK_Tab:
		{
			(void)Term_keypress('\t');
			return;
		}

		case XK_Delete:
		case XK_BackSpace:
		{
			(void)Term_keypress('\010');
			return;
		}
	}


	/* Hack -- Use the KeySym */
	if (ks)
	{
		(void)strnfmt(msg, sizeof(msg), "%c%s%s%s%s_%lX%c", 31,
		        mc ? "N" : "", ms ? "S" : "",
		        mo ? "O" : "", mx ? "M" : "",
		        (unsigned long)(ks), 13);
	}

	/* Hack -- Use the Keycode */
	else
	{
		(void)strnfmt(msg, sizeof(msg), "%c%s%s%s%sK_%X%c", 31,
		        mc ? "N" : "", ms ? "S" : "",
		        mo ? "O" : "", mx ? "M" : "",
		        ev->keycode, 13);
	}

	/* Enqueue the "macro trigger" string */
	for (i = 0; msg[i]; i++) (void)Term_keypress(msg[i]);


	/* Hack -- auto-define macros as needed */
	if (n && (macro_find_exact(msg) < 0))
	{
		/* Create a macro */
		(void)macro_add(msg, buf);
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
	(void)Term_redraw_section(x1, y1, x2, y2);
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
	               x2-x1+Infofnt->wid - 1, y2-y1+Infofnt->hgt - 1);
}


/*
 * Mark a selection by drawing boxes around it (for now).
 */
static void mark_selection(void)
{
	co_ord min, max;
	term *old = Term;
	bool draw = x11_selection->select;
	bool clear = x11_selection->drawn;

	/* Open the correct term if necessary. */
	if (x11_selection->t != old) (void)Term_activate(x11_selection->t);

	if (clear)
	{
		sort_co_ord(&min, &max, &x11_selection->init, &x11_selection->old);
		mark_selection_clear(min.x, min.y, max.x, max.y);
	}

	if (draw)
	{
		sort_co_ord(&min, &max, &x11_selection->init, &x11_selection->cur);
		mark_selection_mark(min.x, min.y, max.x, max.y);
	}

	/* Finish on the current term. */
	if (x11_selection->t != old) (void)Term_activate(old);

	x11_selection->old.x = x11_selection->cur.x;
	x11_selection->old.y = x11_selection->cur.y;
	x11_selection->drawn = x11_selection->select;
}


/*
 * Forget a selection for one reason or another.
 */
static void copy_x11_release(void)
{
	/* Deselect the current selection. */
	x11_selection->select = FALSE;

	/* Remove its graphical represesntation. */
	mark_selection();
}


/*
 * Start to select some text on the screen.
 */
static void copy_x11_start(int x, int y)
{
	if (x11_selection->select) copy_x11_release();

	/* Remember where the selection started. */
	x11_selection->t = Term;
	x11_selection->init.x = x11_selection->cur.x = x11_selection->old.x = x;
	x11_selection->init.y = x11_selection->cur.y = x11_selection->old.y = y;
}


/*
 * Respond to movement of the mouse when selecting text.
 */
static void copy_x11_cont(int x, int y, unsigned int buttons)
{
	/* Use the nearest square within bounds if the mouse is outside. */
	x = MIN(MAX(x, 0), Term->wid-1);
	y = MIN(MAX(y, 0), Term->hgt-1);

	/* The left mouse button isn't pressed. */
	if (~buttons & Button1Mask) return;

	/* Not a selection in this window. */
	if (x11_selection->t != Term) return;

	/* Not enough movement. */
	if ((x == x11_selection->old.x) && (y == x11_selection->old.y) && x11_selection->select) return;

	/* Something is being selected. */
	x11_selection->select = TRUE;

	/* Track the selection. */
	x11_selection->cur.x = x;
	x11_selection->cur.y = y;

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
	if (!x11_selection->select) return;

	/* Not a selection in this window. */
	if (x11_selection->t != Term) return;

	/* Remember when the selection was finalised. */
	x11_selection->time = time;

	/* Acquire the primary selection. */
	XSetSelectionOwner(Metadpy->dpy, XA_PRIMARY, Infowin->win, time);

	if (XGetSelectionOwner(Metadpy->dpy, XA_PRIMARY) != Infowin->win)
	{
		/* Failed to acquire the selection, so forget it. */
		bell("Failed to acquire primary buffer.");
		x11_selection->select = FALSE;
		mark_selection();
	}
}


/*
 * Send some text requested by another X client
 */
static void paste_x11_send(XSelectionRequestEvent *rq)
{
	XEvent event;
	XSelectionEvent *ptr = &(event.xselection);

	static Atom xa_targets = None;

	if (xa_targets == None)
		xa_targets = XInternAtom(DPY, "TARGETS", False);

	/* Set the event parameters */
	ptr->type = SelectionNotify;
	ptr->property = rq->property;
	ptr->display = rq->display;
	ptr->requestor = rq->requestor;
	ptr->selection = rq->selection;
	ptr->target = rq->target;
	ptr->time = rq->time;

	if (rq->target == xa_targets)
	{
		Atom target_list[2];
		target_list[0] = xa_targets;
		target_list[1] = XA_STRING;

		XChangeProperty(DPY, rq->requestor, rq->property, rq->target,
		                (8 * sizeof(target_list[0])), PropModeReplace,
		                (unsigned char *)target_list,
		                (sizeof(target_list) / sizeof(target_list[0])));

		event.xselection.property = rq->property;
	}
	else if (rq->target == XA_STRING)
	{
		/* Reply to a known target received recently with data */
		char buf[1024];
		co_ord max, min;
		int x, y, i;
		byte a;
		char c;

		/* Work out which way around to paste */
		sort_co_ord(&min, &max, &x11_selection->init, &x11_selection->cur);

		/* Delete the old value of the property */
		XDeleteProperty(DPY, rq->requestor, rq->property);

		for (y = 0; y < Term->hgt; y++)
		{
			if (y < min.y) continue;
			if (y > max.y) break;

			for (x = i = 0; x < Term->wid; x++)
			{
				if (x < min.x) continue;
				if (x > max.x) break;

				/* Protect the buffer boundary */
				if (i >= (sizeof(buf) - 2)) break;

				/* Find the character */
				(void)Term_what(x, y, &a, &c);

				/* Add it */
				buf[i++] = c;
			}

			/* Terminate all but the last line in an appropriate way */
			if (y != max.y) buf[i++] = '\n';

			/* Send the (non-empty) string */
			XChangeProperty(DPY, rq->requestor, rq->property, rq->target, 8,
			                PropModeAppend, (unsigned char *)buf, i);
		}
	}
	else
	{
		/* Respond to all bad requests with property None */
		ptr->property = None;
	}

	/* Send whatever event we're left with */
	XSendEvent(DPY, rq->requestor, FALSE, NoEventMask, &event);
}


/*
 * Handle various events conditional on presses of a mouse button.
 */
static void handle_button(Time time, int x, int y, int button, bool press)
{
	/* The co-ordinates are only used in Angband format. */
	pixel_to_square(&x, &y, x, y);

	if (press && button == 1) copy_x11_start(x, y);
	if (!press && button == 1) copy_x11_end(time);
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
	int window = 0;

	/* Do not wait unless requested */
	if (!wait && !XPending(Metadpy->dpy)) return (1);

	/*
	 * Hack - redraw the selection, if needed.
	 * This doesn't actually check that one of its squares was drawn to,
	 * only that this may have happened.
	 */
	if (x11_selection->select && !x11_selection->drawn) mark_selection();

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
			window = i;
			break;
		}
	}

	/* Unknown window */
	if (!td || !iwin) return (0);


	/* Hack -- activate the Term */
	(void)Term_activate(&td->t);

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

			/* XXX Handle */
			handle_button(xev->xbutton.time, x, y, z, press);

			break;
		}

		case MotionNotify:
		{
			/* Where is the mouse */
			int x = xev->xmotion.x;
			int y = xev->xmotion.y;
			unsigned int z = xev->xmotion.state;

			/* Convert to co-ordinates Angband understands. */
			pixel_to_square(&x, &y, x, y);

			/* Alter the selection if appropriate. */
			copy_x11_cont(x, y, z);

			break;
		}

		case SelectionRequest:
		{
			paste_x11_send(&(xev->xselectionrequest));
			break;
		}

		case SelectionClear:
		{
			x11_selection->select = FALSE;
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
			(void)Term_activate(&old_td->t);

			/* Process the key */
			react_keypress(&(xev->xkey));

			break;
		}

		case Expose:
		{
			int x1, x2, y1, y2;

			x1 = (xev->xexpose.x - Infowin->ox) / Infofnt->wid;
			x2 = (xev->xexpose.x + xev->xexpose.width -
				 Infowin->ox) / Infofnt->wid;

			y1 = (xev->xexpose.y - Infowin->oy) / Infofnt->hgt;
			y2 = (xev->xexpose.y + xev->xexpose.height -
				 Infowin->oy) / Infofnt->hgt;

			/* Redraw */
			(void)Term_redraw_section(x1, y1, x2, y2);

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

			if (window == 0)
			{
				/* Hack the main window doesn't resize  XXX XXX */
				break;
			}

			/* Save the new Window Parms */
			Infowin->x = xev->xconfigure.x;
			Infowin->y = xev->xconfigure.y;
			Infowin->w = xev->xconfigure.width;
			Infowin->h = xev->xconfigure.height;

			/* Determine "proper" number of rows/cols */
			cols = ((Infowin->w - (ox + ox)) / td->fnt->wid);
			rows = ((Infowin->h - (oy + oy)) / td->fnt->hgt);

			/* Hack -- minimal size */
			if (cols < 1) cols = 1;
			if (rows < 1) rows = 1;

			if (window == 0)
			{
				if (screen_rows != 50)
				{
					/* Hack -- the main window must be at least 80x25 */
					if (cols < 80) cols = 80;
					if (rows < 25) rows = 25;
				}
				else
				{
					/* Hack -- the main window must be at least 80x50 */
					if (cols < 80) cols = 80;
					if (rows < 50) rows = 50;
				}
			}

			/* Desired size of window */
			wid = cols * td->fnt->wid + (ox + ox);
			hgt = rows * td->fnt->hgt + (oy + oy);

			/* Resize the Term (if needed) */
			(void)Term_resize(cols, rows);

			/* Resize the windows if any "change" is needed */
			if ((Infowin->w != wid) || (Infowin->h != hgt))
			{
				/* Resize window */
				Infowin_set(td->win);
				(void)Infowin_resize(wid, hgt);
			}

			break;
		}
	}


	/* Hack -- Activate the old term */
	(void)Term_activate(&old_td->t);

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
		for (i = 0; i < MAX_COLORS; i++)
		{
			if ((x11_color_table[i][0] != color_table[i].kv) ||
			    (x11_color_table[i][1] != color_table[i].rv) ||
			    (x11_color_table[i][2] != color_table[i].gv) ||
			    (x11_color_table[i][3] != color_table[i].bv))
			{
				Pixell pixel;

				/* Save new values */
				x11_color_table[i][0] = color_table[i].kv;
				x11_color_table[i][1] = color_table[i].rv;
				x11_color_table[i][2] = color_table[i].gv;
				x11_color_table[i][3] = color_table[i].bv;

				/* Create pixel */
				pixel = create_pixel(Metadpy->dpy,
				                     x11_color_table[i][1],
				                     x11_color_table[i][2],
				                     x11_color_table[i][3]);

				/* Change the foreground */
				Infoclr_set(clr[i]);
				(void)Infoclr_change_fg(pixel);
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
	case TERM_XTRA_NOISE: (void)Metadpy_do_beep(); return (0);

		/* Flush the output XXX XXX */
	case TERM_XTRA_FRESH: (void)Metadpy_update(1, 0, 0); return (0);

		/* Process random events XXX */
		case TERM_XTRA_BORED: return (CheckEvent(0));

		/* Process Events XXX */
		case TERM_XTRA_EVENT: return (CheckEvent(v));

		/* Flush the events XXX */
		case TERM_XTRA_FLUSH: while (!CheckEvent(FALSE)); return (0);

		/* Handle change in the "level" */
		case TERM_XTRA_LEVEL: return (Term_xtra_x11_level(v));

		/* Clear the screen and redraw any selection later */
	case TERM_XTRA_CLEAR: (void)Infowin_wipe(); x11_selection->drawn = FALSE; return (0);

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY:
		if (v <= 0) return (0); usleep(1000 * v); return (0);

		/* React to changes */
		case TERM_XTRA_REACT: return (Term_xtra_x11_react());
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
	XDrawRectangle(Metadpy->dpy, Infowin->win, xor->gc,
			 x * Infofnt->wid + Infowin->ox,
			 y * Infofnt->hgt + Infowin->oy,
			 Infofnt->wid - 1, Infofnt->hgt - 1);

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
	(void)Infofnt_text_non(x, y, "", n);

	/* Redraw the selection if any, as it may have been obscured. (later) */
	x11_selection->drawn = FALSE;

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
	(void)Infofnt_text_std(x, y, s, n);

	/* Redraw the selection if any, as it may have been obscured. (later) */
	x11_selection->drawn = FALSE;

	/* Success */
	return (0);
}


#ifdef USE_GRAPHICS

/*
 * Draw some graphical characters.
 */
static errr Term_pict_x11(int x, int y, int n, const byte *ap, const char *cp, const byte *tap, const char *tcp)
{
	int i;
	int x1 = 0, y1 = 0;

	byte a;
	char c;

	byte ta;
	char tc;

	int x2, y2;
	int k,l;

	unsigned long pixel, blank;


	term_data *td = (term_data*)(Term->data);

	y *= Infofnt->hgt;
	x *= Infofnt->wid;

	/* Add in affect of window boundaries */
	y += Infowin->oy;
	x += Infowin->ox;

	for (i = 0; i < n; ++i)
	{
		a = *ap++;
		c = *cp++;

		/* For extra speed - cache these values */
		x1 = (c & 0x7F) * td->fnt->wid;
		y1 = (a & 0x7F) * td->fnt->hgt;

		ta = *tap++;
		tc = *tcp++;

		/* For extra speed - cache these values */
		x2 = (tc & 0x7F) * td->fnt->wid;
		y2 = (ta & 0x7F) * td->fnt->hgt;

		/* Optimise the common case */
		if (((x1 == x2) && (y1 == y2)) ||
		    !(((byte)ta & 0x80) && ((byte)tc & 0x80)))
		{
			/* Draw object / terrain */
			XPutImage(Metadpy->dpy, td->win->win,
			          clr[0]->gc,
			          td->tiles,
			          x1, y1,
			          x, y,
			          td->fnt->wid, td->fnt->hgt);
		}
		else
		{

			/* Mega Hack^2 - assume the top left corner is "blank" */
			if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
				blank = XGetPixel(td->tiles, 0, 0);
			else
				blank = XGetPixel(td->tiles, 0, td->fnt->hgt * 6);

			for (k = 0; k < td->fnt->wid; k++)
			{
				for (l = 0; l < td->fnt->hgt; l++)
				{
					/* If mask set... */
					if ((pixel = XGetPixel(td->tiles, x1 + k, y1 + l)) == blank)
					{
						/* Output from the terrain */
						pixel = XGetPixel(td->tiles, x2 + k, y2 + l);

						if (pixel == blank)
							pixel = 0L;
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
			          td->fnt->wid, td->fnt->hgt);
		}

		x += td->fnt->wid;
	}

	/* Redraw the selection if any, as it may have been obscured. (later) */
	x11_selection->drawn = FALSE;

	/* Success */
	return (0);
}

#endif /* USE_GRAPHICS */


static void save_prefs(void)
{
	FILE *fff;
	int i;

	/* Open the settings file */
	fff = my_fopen(settings, "w");

	/* Oops */
	if (!fff) return;

	/* Header */
	fprintf(fff, "# %s X11 settings\n\n", VERSION_NAME);

	/* Number of term windows to open */
	fprintf(fff, "TERM_WINS=%d\n\n", term_windows_open);

	/* Save window prefs */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		term_data *td = &data[i];

		if (!td->t.mapped_flag) continue;

		/* Header */
		fprintf(fff, "# Term %d\n", i);

		/*
		 * This doesn't seem to work under various WMs
		 * since the decoration messes the position up
		 *
		 * Hack -- Use saved window positions.
		 * This means that we wont remember ingame repositioned
		 * windows, but also means that WMs wont screw predefined
		 * positions up. -CJN-
		 */

		/* Window specific location (x) */
		fprintf(fff, "AT_X_%d=%d\n", i, td->win->x_save);

		/* Window specific location (y) */
		fprintf(fff, "AT_Y_%d=%d\n", i, td->win->y_save);

		/* Window specific cols */
		fprintf(fff, "COLS_%d=%d\n", i, td->t.wid);

		/* Window specific rows */
		fprintf(fff, "ROWS_%d=%d\n", i, td->t.hgt);

		/* Window specific inner border offset (ox) */
		fprintf(fff, "IBOX_%d=%d\n", i, td->win->ox);

		/* Window specific inner border offset (oy) */
		fprintf(fff, "IBOY_%d=%d\n", i, td->win->oy);

		/* Window specific font name */
		fprintf(fff, "FONT_%d=%s\n", i, td->nfnt->name);

		if (i == 0)
		{
			/* Window specific small font name */
			fprintf(fff, "SFONT_%d=%s\n", i, td->sfnt->name);
		}

		/* Footer */
		fprintf(fff, "\n");
	}

	/* Close */
	(void)my_fclose(fff);
}


/*
 * Given a position in the ISO Latin-1 character set, return
 * the correct character on this system.
 */
static char Term_xchar_x11(char c)
{
	/* The X11 port uses the Latin-1 standard */
	return (c);
}


/*
 * Initialize a term_data
 */
static errr term_data_init(term_data *td, int i)
{
	term *t = &td->t;

	cptr name = angband_term_name[i];

	cptr font;
	cptr sfont;

	int x = 0;
	int y = 0;

	int cols = 80;
	int rows = 25;

	int ox = 1;
	int oy = 1;

	int wid, hgt, num;

	cptr str;

	int val;

	XClassHint *ch;

	char res_name[20];
	char res_class[20];

	XSizeHints *sh;

	FILE *fff;

	char buf[1024];
	char cmd[40];
	char font_name[256];
	char sfont_name[256];

	int line = 0;

	/* Get default font for this term */
	font = get_default_font(i);
	sfont = get_default_small_font(i);

	/* Build the filename */
	(void)path_build(settings, sizeof(settings), ANGBAND_DIR_USER, "x11-settings.prf");

	/* Open the file */
	fff = my_fopen(settings, "r");

	/* File exists */
	if (fff)
	{
		/* Process the file */
		while (0 == my_fgets(fff, buf, sizeof(buf)))
		{
			/* Count lines */
			line++;

			/* Skip "empty" lines */
			if (!buf[0]) continue;

			/* Skip "blank" lines */
			if (isspace((unsigned char)buf[0])) continue;

			/* Skip comments */
			if (buf[0] == '#') continue;

			/* Window specific location (x) */
			sprintf(cmd, "AT_X_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				x = (str != NULL) ? atoi(str + 1) : -1;
				continue;
			}

			/* Window specific location (y) */
			sprintf(cmd, "AT_Y_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				y = (str != NULL) ? atoi(str + 1) : -1;
				continue;
			}

			/* Window specific cols */
			sprintf(cmd, "COLS_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				val = (str != NULL) ? atoi(str + 1) : -1;
				if (val > 0) cols = val;
				continue;
			}

			/* Window specific rows */
			sprintf(cmd, "ROWS_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				val = (str != NULL) ? atoi(str + 1) : -1;
				if (val > 0) rows = val;
				continue;
			}

			/* Window specific inner border offset (ox) */
			sprintf(cmd, "IBOX_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				val = (str != NULL) ? atoi(str + 1) : -1;
				if (val > 0) ox = val;
				continue;
			}

			/* Window specific inner border offset (oy) */
			sprintf(cmd, "IBOY_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				val = (str != NULL) ? atoi(str + 1) : -1;
				if (val > 0) oy = val;
				continue;
			}

			/* Window specific font name */
			sprintf(cmd, "FONT_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				if (str != NULL)
				{
					(void)my_strcpy(font_name, str + 1, sizeof(font_name));
					font = font_name;
				}
				continue;
			}

			/* Window specific small font name */
			sprintf(cmd, "SFONT_%d", i);

			if (prefix(buf, cmd))
			{
				str = strstr(buf, "=");
				if (str != NULL)
				{
					(void)my_strcpy(sfont_name, str + 1, sizeof(sfont_name));
					sfont = sfont_name;
				}
				continue;
			}
		}

		/* Close */
		(void)my_fclose(fff);
	}

	/*
	 * Env-vars overwrite the settings in the settings file
	 */

	/* Window specific location (x) */
	sprintf(buf, "ANGBAND_X11_AT_X_%d", i);
	str = getenv(buf);
	val = (str != NULL) ? atoi(str) : -1;
	if (val > 0) x = val;

	/* Window specific location (y) */
	sprintf(buf, "ANGBAND_X11_AT_Y_%d", i);
	str = getenv(buf);
	val = (str != NULL) ? atoi(str) : -1;
	if (val > 0) y = val;

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


#if 0  /* Disabled, we can't use the same fonts as Vanilla! */

	/* Window specific font name */
	sprintf(buf, "ANGBAND_X11_FONT_%d", i);
	str = getenv(buf);
	if (str) font = str;

	if (i == 0)
	{
		/* Window specific small font name */
		sprintf(buf, "ANGBAND_X11_SFONT_%d", i);
		str = getenv(buf);
		if (str) sfont = str;
	}
#endif /* Disabled */

	/* Hack the main window must be at least 80x25 (for now) */
	if (!i)
	{
		if (cols < 80) cols = 80;
		if (rows < 25) rows = 25;
	}


	/* Prepare the standard font */
	MAKE(td->fnt, infofnt);
	Infofnt_set(td->fnt);
	if (Infofnt_init_data(font)) quit_fmt("Couldn't load the requested font. (%s)", font);

	/* Save pointer to normal font */
	td->nfnt = td->fnt;

	/* Prepare the standard small font */
	MAKE(td->sfnt, infofnt);
	Infofnt_set(td->sfnt);
	if (Infofnt_init_data(sfont)) quit_fmt("Couldn't load the requested font. (%s)", sfont);

	/* Hack -- key buffer size */
	num = ((i == 0) ? 1024 : 16);

	/* Assume full size windows */
	wid = cols * td->fnt->wid + (ox + ox);
	hgt = rows * td->fnt->hgt + (oy + oy);

	/* Create a top-window */
	MAKE(td->win, infowin);
	Infowin_set(td->win);
	(void)Infowin_init_top(x, y, wid, hgt, 0,
	                 Metadpy->fg, Metadpy->bg);

	/* Ask for certain events */
	(void)Infowin_set_mask(ExposureMask | StructureNotifyMask | KeyPressMask |
	                 PointerMotionMask | ButtonPressMask | ButtonReleaseMask);

	/* Set the window name */
	(void)Infowin_set_name(name);

	/* Save the inner border */
	Infowin->ox = ox;
	Infowin->oy = oy;

	/* Make Class Hints */
	ch = XAllocClassHint();

	if (ch == NULL) quit("XAllocClassHint failed");

	(void)my_strcpy(res_name, name, sizeof(res_name));
	res_name[0] = tolower((unsigned char)res_name[0]);
	ch->res_name = res_name;

	strcpy(res_class, "Angband");
	ch->res_class = res_class;

	XSetClassHint(Metadpy->dpy, Infowin->win, ch);

	/* Make Size Hints */
	sh = XAllocSizeHints();

	/* Oops */
	if (sh == NULL) quit("XAllocSizeHints failed");

	/* Main window has a differing minimum size */
	if (i == 0)
	{
		/* Main window min size is 80x25 */
		sh->flags = PMinSize | PMaxSize | PPosition;
		sh->min_width = (ox + ox) + 80 *
			(td->fnt->wid > td->sfnt->wid ? td->fnt->wid : td->sfnt->wid);
		sh->min_height = (oy + oy) +
			(25 * td->fnt->hgt > 50 * td->sfnt->hgt ?
			 25 * td->fnt->hgt : 50 * td->sfnt->hgt);
		sh->max_width = sh->min_width;
		sh->max_height = sh->min_height;
		/* Use hints to move window */
		sh->x = x;
		sh->y = y;
	}

	/* Other windows can be shrunk to 1x1 */
	else
	{
		/* Other windows */
		sh->flags = PMinSize | PMaxSize | PPosition;
		sh->min_width = td->fnt->wid + (ox + ox);
		sh->min_height = td->fnt->hgt + (oy + oy);
		sh->max_width = 255 * td->fnt->wid + (ox + ox);
		sh->max_height = 255 * td->fnt->hgt + (oy + oy);
		/* Use hints to move window */
		sh->x = x;
		sh->y = y;

		/* Resize increment */
		sh->flags |= PResizeInc;
		sh->width_inc = td->fnt->wid;
		sh->height_inc = td->fnt->hgt;
	}

	/* Base window size */
	sh->flags |= PBaseSize;
	sh->base_width = (ox + ox);
	sh->base_height = (oy + oy);

	/* Use the size hints */
	XSetWMNormalHints(Metadpy->dpy, Infowin->win, sh);

	/* Map the window */
	(void)Infowin_map();

	/* Set pointers to allocated data */
	td->sizeh = sh;
	td->classh = ch;

	/* Move the window to requested location */
	/* Use hints instead */
	/* if ((x >= 0) && (y >= 0)) Infowin_impell(x, y); */


	/* Hack the main window must be initialized as 80x50 for hot bigscreen */
	if (!i)
	{
		if (cols < 80) cols = 80;
		if (rows < 50) rows = 50;
	}

	/* Initialize the term */
	(void)term_init(t, cols, rows, num);

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
	t->xchar_hook = Term_xchar_x11;

	/* Initialize the rows hook only on the main term */
	if (i == 0) t->rows_hook = Term_rows_x11;

	/* Save the data */
	t->data = td;

	/* Activate (important) */
	(void)Term_activate(t);

	/* Success */
	return (0);
}


const char help_x11[] = "Basic X11, subopts -d<display> -n<windows>"
#ifdef USE_GRAPHICS
                        " -s(moothRescale)"
                        "\n           -b(Bigtile) -o(original) -a(AdamBolt) -g(David Gervais)"
#endif
                        ;


static void hook_quit(cptr str)
{
	int i;

	/* Unused */
	(void)str;

	(void)unregister_angband_fonts();

	save_prefs();

	/* Free allocated data */
	for (i = 0; i < term_windows_open; i++)
	{
		term_data *td = &data[i];
		term *t = &td->t;

		/* Free size hints */
		XFree(td->sizeh);

		/* Free class hint */
		XFree(td->classh);

		/* Free fonts */
		Infofnt_set(td->fnt);
		(void)Infofnt_nuke();
		KILL(td->fnt);

		Infofnt_set(td->sfnt);
		(void)Infofnt_nuke();
		KILL(td->sfnt);

		/* Free window */
		Infowin_set(td->win);
		(void)Infowin_nuke();
		KILL(td->win);

		/* Free term */
		(void)term_nuke(t);
	}

	/* Free colors */
	Infoclr_set(xor);
	(void)Infoclr_nuke();
	KILL(xor);

	for (i = 0; i < MAX_COLORS; ++i)
	{
		Infoclr_set(clr[i]);
		(void)Infoclr_nuke();
		KILL(clr[i]);
	}

	/* Close link to display */
	(void)Metadpy_nuke();
}


/*
 * Initialization function for an "X11" module to Angband
 */
errr init_x11(int argc, char *argv[])
{
	int i;

	cptr dpy_name = "";

	int num_term = 1;
	int cdepth;

	FILE *fff;

	char buf[1024];
	cptr str;
	int val;
	int line = 0;

#ifdef USE_GRAPHICS

	cptr bitmap_file = "";
	char filename[1024];

	int pict_wid = 0;
	int pict_hgt = 0;

	char *TmpData;

#endif /* USE_GRAPHICS */


	/*
	 * Check x11-settings for the number of windows before handling
	 * command line options to allow for easy override
	 */

	/* Build the filename */
	(void)path_build(settings, sizeof(settings), ANGBAND_DIR_USER, "x11-settings.prf");

	/* Open the file */
	fff = my_fopen(settings, "r");

	/* File exists */
	if (fff)
	{
		/* Process the file */
		while (0 == my_fgets(fff, buf, sizeof(buf)))
		{
			/* Count lines */
			line++;

			/* Skip "empty" lines */
			if (!buf[0]) continue;

			/* Skip "blank" lines */
			if (isspace((unsigned char)buf[0])) continue;

			/* Skip comments */
			if (buf[0] == '#') continue;

			/* Number of terminal windows */
			if (prefix(buf, "TERM_WINS"))
			{
				str = strstr(buf, "=");
				val = (str != NULL) ? atoi(str + 1) : -1;
				if (val > 0) num_term = val;
				continue;
			}
		}

		/* Close */
		(void)my_fclose(fff);
	}

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
			arg_graphics = GRAPHICS_ORIGINAL;
			continue;
		}

		if (prefix(argv[i], "-a"))
		{
			arg_graphics = GRAPHICS_ADAM_BOLT;
			continue;
		}

		if (prefix(argv[i], "-g"))
		{
			smoothRescaling = FALSE;
			arg_graphics = GRAPHICS_DAVID_GERVAIS;
			continue;
		}

#if 0 /* Sangband doesn't use bigtile */
		if (prefix(argv[i], "-b"))
		{
			use_bigtile = TRUE;
			continue;
		}
#endif /* Bigtile */

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

	/* Remember the number of terminal windows */
	term_windows_open = num_term;

	/* Register the font directory */
	register_angband_fonts();

	/* Init the Metadpy if possible */
	if (Metadpy_init_name(dpy_name)) return (-1);


	/* Test color depth */
	/* Number of colors = 2^cdepth: a depth of 8 means 256 colors */
	cdepth = DefaultDepth(Metadpy->dpy, DefaultScreen(Metadpy->dpy));
	if (cdepth >= 8)
	{
		/*
		 * Go no higher than 128, as graphics use the higher values.
		 */
		max_system_colors = 128;
	}
	/* Unusual color depths, check for completeness */
	else if (cdepth == 7)
	{
		max_system_colors = 128;
	}
	else if (cdepth == 6)
	{
		max_system_colors = 64;
	}
	else if (cdepth == 5)
	{
		max_system_colors = 32;
	}
	/* 16 colors */
	else if (cdepth == 4)
	{
		max_system_colors = 16;
	}
	/* Fewer that 16 colors not supported */
	else
	{
		quit("Sytems with fewer than 16 colors not supported!");
	}


	/* Prepare cursor color */
	MAKE(xor, infoclr);
	Infoclr_set(xor);
	(void)Infoclr_init_ppn(Metadpy->fg, Metadpy->bg, "xor", 0);


	/* Prepare normal colors */
	for (i = 0; i < MAX_COLORS; ++i)
	{
		Pixell pixel;

		MAKE(clr[i], infoclr);

		Infoclr_set(clr[i]);

		/* Acquire Angband colors */
		x11_color_table[i][0] = color_table[i].kv;
		x11_color_table[i][1] = color_table[i].rv;
		x11_color_table[i][2] = color_table[i].gv;
		x11_color_table[i][3] = color_table[i].bv;

		/* Default to monochrome */
		pixel = ((i == 0) ? Metadpy->bg : Metadpy->fg);

		/* Handle color */
		if (Metadpy->color)
		{
			/* Create pixel */
			pixel = create_pixel(Metadpy->dpy,
			                     x11_color_table[i][1],
			                     x11_color_table[i][2],
			                     x11_color_table[i][3]);
		}

		/* Initialize the color */
		(void)Infoclr_init_ppn(pixel, Metadpy->bg, "cpy", 0);
	}


	/* Initialize the windows */
	for (i = 0; i < num_term; i++)
	{
		term_data *td = &data[i];

		/* Initialize the term_data */
		(void)term_data_init(td, i);

		/* Save global entry */
		angband_term[i] = Term;
	}

	/* Raise the "Angband" window */
	Infowin_set(data[0].win);
	(void)Infowin_raise();

	/* Activate the "Angband" window screen */
	(void)Term_activate(&data[0].t);


#ifdef USE_GRAPHICS

	/* Try graphics */
	switch (arg_graphics)
	{
	case GRAPHICS_ADAM_BOLT:
		/* Use tile graphics of Adam Bolt */
		bitmap_file = "16x16.bmp";

		/* Try the "16x16.bmp" file */
		(void)path_build(filename, sizeof(filename), ANGBAND_DIR_XTRA, format("graf/%s", bitmap_file));

		/* Use the "16x16.bmp" file if it exists */
		if (0 == fd_close(fd_open(filename, O_RDONLY)))
		{
			/* Use graphics */
			use_graphics = GRAPHICS_ADAM_BOLT;
			use_transparency = TRUE;

			pict_wid = pict_hgt = 16;

			ANGBAND_GRAF = "new";

			break;
		}
		/* Fall through */

	case GRAPHICS_ORIGINAL:
		/* Use original tile graphics */
		bitmap_file = "8x8.bmp";

		/* Try the "8x8.bmp" file */
		(void)path_build(filename, sizeof(filename), ANGBAND_DIR_XTRA, format("graf/%s", bitmap_file));

		/* Use the "8x8.bmp" file if it exists */
		if (0 == fd_close(fd_open(filename, O_RDONLY)))
		{
			/* Use graphics */
			use_graphics = GRAPHICS_ORIGINAL;

			pict_wid = pict_hgt = 8;

			ANGBAND_GRAF = "old";
			break;
		}
		break;

	case GRAPHICS_DAVID_GERVAIS:
		/* Use tile graphics of David Gervais */
		bitmap_file = "32x32.bmp";

		/* Use graphics */
		use_graphics = GRAPHICS_DAVID_GERVAIS;
		use_transparency = TRUE;

		pict_wid = pict_hgt = 32;

		ANGBAND_GRAF = "david";
		break;
	}

	/* Load graphics */
	if (use_graphics)
	{
		Display *dpy = Metadpy->dpy;

		XImage *tiles_raw;

		/* Initialize */
		for (i = 0; i < num_term; i++)
		{
			term_data *td = &data[i];
			td->tiles = NULL;
		}

		(void)path_build(filename, sizeof(filename), ANGBAND_DIR_XTRA, format("graf/%s", bitmap_file));

		/* Load the graphical tiles */
		tiles_raw = ReadBMP(dpy, filename);

		if (tiles_raw)
		{
			/* Initialize the windows */
			for (i = 0; i < num_term; i++)
			{
				int j;
				bool same = FALSE;

				term_data *td = &data[i];
				term_data *o_td = NULL;

				term *t = &td->t;

				/* Graphics hook */
				t->pict_hook = Term_pict_x11;

				/* Use graphics sometimes */
				t->higher_pict = TRUE;

				/* Look for another term with same tile size */
				for (j = 0; j < i; j++)
				{
					o_td = &data[j];

					if ((td->fnt->wid == o_td->fnt->wid) &&
					    (td->fnt->hgt == o_td->fnt->hgt))
					{
						same = TRUE;
						break;
					}
				}

				if (!same)
				{
					/* Resize tiles */
					td->tiles = ResizeImage(dpy, tiles_raw,
								pict_wid, pict_hgt,
								td->fnt->wid, td->fnt->hgt);
				}
				else
				{
					/* Use same graphics */
					td->tiles = o_td->tiles;
				}
			}

			/* Free tiles_raw */
			FREE(tiles_raw);
		}

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
			total = td->fnt->wid * td->fnt->hgt * ii;


			TmpData = (char *)malloc(total);

			td->TmpImage = XCreateImage(dpy, visual, depth,
				ZPixmap, 0, TmpData,
				td->fnt->wid, td->fnt->hgt, 32, 0);

		}
	}

#endif /* USE_GRAPHICS */

	/* Activate hook */
	quit_aux = hook_quit;

	/* Success */
	return (0);
}

#endif /* USE_X11 */

