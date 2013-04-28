/* File: main-ibm.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/* Purpose: Visual Display Support for "z-term.c", for the IBM */


/*
 * Original code by "Billy Tanksley (wtanksle@ucsd.edu)"
 * Use "Makefile.ibm" to compile Angband using this file.
 *
 * Support for DJGPP v2 by "Scott Egashira (egashira@u.washington.edu)"
 *
 * Extensive modifications by "Ben Harrison (benh@phial.com)",
 * including "collation" of the Watcom C/C++ and DOS-286 patches.
 *
 * Watcom C/C++ changes by "David Boeren (akemi@netcom.com)"
 * Use "Makefile.wat" to compile this file with Watcom C/C++, and
 * be sure to define "USE_IBM" and "USE_WAT".
 *
 * True color palette support by "Mike Marcelais (michmarc@microsoft.com)",
 * with interface to the "color_table" array by Ben Harrison.
 *
 * As of Sangband 1.0.0, this file no longer supports 286 machines, and it
 * requires VGA or better.  This means that the conio code and the simple_
 * color function are gone.  The game always runs in 400 line mode, and
 * switches from 8x8 to 8x16 text (or graphics) as needed.  -LM-
 *
 * Both "shift" keys are treated the same, and all the modifier keys
 * (control, shift, alt) are ignored when used with "normal" keys, unless
 * they modify the underlying "ascii" value of the key.  You must use the
 * new "user pref files" to be able to interact with the keypad and such.
 *
 * The "lib/user/pref-ibm.prf" file contains macro definitions and possible
 * alternative color set definitions.  The "lib/user/font-ibm.prf" contains
 * attr/char mappings for walls and floors and such.
 *
 * Note the "Term_user_ibm()" function hook, which could allow the user
 * to interact with the "main-ibm.c" visual system.  Currently this hook
 * is unused, but, for example, it could allow the user to toggle "sound"
 * or "graphics" modes.
 */


#include "angband.h"

#include "main.h"


#ifdef USE_IBM

/*
 * Undefine things this port can't handle, like graphics
 */
#undef USE_GRAPHICS


#include <bios.h>
#include <dos.h>
#include <dpmi.h>


#ifdef USE_WAT  /* Watcom C/C++ compiler */

# include <conio.h>
# include <graph.h>

#  define bioskey(C)	_bios_keybrd(C)
#  define int86(a,b,c)	int386(a,b,c)
# define inportb(x)	inp(x)
# define outportb(x,y)	outp(x,y)


#else /* DJGPP compiler */

# if __DJGPP__ > 1

# include <pc.h>
# include <osfcn.h>

# else /* __DJGPP__ > 1 */
#  ifdef __DJGPP__
#   error "Upgrade to version 2.0 of DJGPP"
#  endif /* __DJGPP__ */
# endif /* __DJGPP__ > 1 */

#endif /* Compiler-specific */



/*
 * Keypress input modifier flags (hard-coded by DOS)
 */
#define K_RSHIFT        0	/* Right shift key down */
#define K_LSHIFT        1	/* Left shift key down */
#define K_CTRL          2	/* Ctrl key down */
#define K_ALT           3	/* Alt key down */
#define K_SCROLL        4	/* Scroll lock on */
#define K_NUM           5	/* Num lock on */
#define K_CAPS          6	/* Caps lock on */
#define K_INSERT        7	/* Insert on */


/*
 * Foreground color bits (hard-coded by DOS)
 */
#define VID_BLACK     0x00
#define VID_BLUE      0x01
#define VID_GREEN     0x02
#define VID_CYAN      0x03
#define VID_RED       0x04
#define VID_MAGENTA   0x05
#define VID_YELLOW    0x06
#define VID_WHITE     0x07

/*
 * Bright text (hard-coded by DOS)
 */
#define VID_BRIGHT    0x08

/*
 * Background color bits (hard-coded by DOS)
 */
#define VUD_BLACK     0x00
#define VUD_BLUE      0x10
#define VUD_GREEN     0x20
#define VUD_CYAN      0x30
#define VUD_RED       0x40
#define VUD_MAGENTA   0x50
#define VUD_YELLOW    0x60
#define VUD_WHITE     0x70

/*
 * Blinking text (hard-coded by DOS)
 */
#define VUD_BRIGHT    0x80


/*
 * Screen Size
 */
static int rows = 51;
static int cols = 80;


/*
 * Physical Screen
 */
#define PhysicalScreen ((byte *)(0xB800 << 4))


/*
 * Virtual Screen Contents
 */
static byte *VirtualScreen;


/*
 * Hack -- the cursor "visibility"
 */
static int saved_cur_v;
static int saved_cur_high;
static int saved_cur_low;


/*
 * This array is used for "wiping" the screen
 */
static byte wiper[160];


/*
 * The main screen (currently the only screen)
 */
static term term_screen_body;

/*
 * The "complex" color set
 */
static long ibm_color_complex[16];



/*
 * Activate the "ibm_color_complex" palette information.
 *
 * Code by Mike Marcelais, with help from "The programmer's guide
 * to the EGA and VGA video cards" [Farraro].
 *
 * On VGA cards, colors go through a double-indirection when looking
 * up the `real' color when in 16 color mode.  The color value in the
 * attribute is looked up in the EGA color registers.  Then that value
 * is looked up in the VGA color registers.  Then the color is displayed.
 * This is done for compatibility.  However, the EGA registers are
 * initialized by default to 0..5, 14, 7, 38..3F and not 0..F which means
 * that unless these are reset, the VGA setpalette function will not
 * update the correct palette register!
 *
 * DJGPP's GrSetColor() does _not_ set the EGA palette list, only the
 * VGA color list.
 *
 * Note that the "traditional" method, using "int86(0x10)", is very slow
 * when called in protected mode, so we use a faster method using video
 * ports instead.
 *
 * On Watcom machines, we could simply use the special "_remapallpalette()"
 * function, which not only sets both palette lists (see below) but also
 * checks for legality of the monitor mode, but, if we are doing bitmapped
 * graphics, that function forgets to set the EGA registers for some reason.
 */
static void activate_color_complex(void)
{
	int i;
	printf("%c%c%c%c",8,8,8,8);

	/* Edit the EGA palette */
	inportb(0x3da);

	/* Edit the colors */
	for (i = 0; i < 16; i++)
	{
		/* Set color "i" */
		outportb(0x3c0, i);

		/* To value "i" */
		outportb(0x3c0, i);
	};

	/* Use that EGA palette */
	outportb(0x3c0, 0x20);

	/* Edit VGA palette, starting at color zero */
	outportb(0x3c8, 0);

	/* Send the colors */
	for (i = 0; i < 16; i++)
	{
		/* Send the red, green, blue components */
		outportb(0x3c9, ((ibm_color_complex[i]) & 0xFF));
		outportb(0x3c9, ((ibm_color_complex[i] >> 8) & 0xFF));
		outportb(0x3c9, ((ibm_color_complex[i] >> 16) & 0xFF));
	}
}


/*
 * Note the use of "(x >> 2)" to convert an 8 bit value to a 6 bit value
 * without losing much precision.
 */
static int Term_xtra_ibm_react(void)
{
	int i;

	long rv, gv, bv, code;

	bool change = FALSE;

	/* Save the default colors */
	for (i = 0; i < 16; i++)
	{
		/* Extract desired values */
		rv = color_table[i].rv >> 2;
		gv = color_table[i].gv >> 2;
		bv = color_table[i].bv >> 2;

		/* Extract a full color code */
		code = ((rv) | (gv << 8) | (bv << 16));

		/* Activate changes */
		if (ibm_color_complex[i] != code)
		{
			/* Note the change */
			change = TRUE;

			/* Apply the desired color */
			ibm_color_complex[i] = code;
		}
	}

	/* Activate the palette if needed */
	if (change) activate_color_complex();

	/* Success */
	return (0);
}



#define TICK_PER_DAY (24*60*60*10000/182)

/* From the CVS build of DJGPP */
static void djgpp_delay(unsigned msec)
{
  __dpmi_regs r;

  while (msec)
  {
    unsigned usec;
    unsigned msec_this = msec;
    if (msec_this > 4000)
      msec_this = 4000;
    usec = msec_this * 1000;
    r.h.ah = 0x86;
    r.x.cx = usec>>16;
    r.x.dx = usec & 0xffff;
    __dpmi_int(0x15, &r);
    if ((r.x.flags & 1) || (r.h.ah == 0x83))
    {
      /* INT 15 FAILED, so fall back to the Time Of Day Tick */
      unsigned long start_tick;
      unsigned long end_tick;

      r.h.ah = 0x00;
      __dpmi_int(0x1A, &r);

      start_tick = (r.x.cx << 16) + (r.x.dx & 0xffff);
      end_tick = (msec*182)/10000 + start_tick;

      if ((msec%10000/182) > (5000/182)) /* Manual round ticks */
      {
	end_tick++;
      }
      if (end_tick > TICK_PER_DAY)  /* Tick time past midnight */
      {
	/* check for midnight */
	while (r.h.al == 0)
	{
	  r.h.ah = 0x00;
	  __dpmi_int(0x1A, &r);
	  __dpmi_yield();
	}
	end_tick = end_tick - TICK_PER_DAY;
      }

      while (((r.x.cx << 16) + (r.x.dx & 0xffffUL)) <= end_tick)
      {
	r.h.ah = 0x00;
	__dpmi_int(0x1A, &r);
	__dpmi_yield();
      }
      msec = 0;  /* waited the required time */
    }
    else
    {
      msec -= msec_this;
    }
  }
}




/*
 * Hack -- set the cursor "visibility"
 */
static void curs_set(int v)
{
	/* If needed */
	if (saved_cur_v != v)
	{
		union REGS r;

		/* Set cursor */
		r.h.ah = 1;

		/* Visible */
		if (v)
		{
			/* Use the saved values */
			r.h.ch = saved_cur_high;
			r.h.cl = saved_cur_low;
		}

		/* Invisible */
		else
		{
			/* Make it invisible */
			r.h.ch = 0x20;
			r.h.cl = 0x00;
		}

		/* Make the call */
		int86(0x10, &r, &r);

		/* Save the cursor state */
		saved_cur_v = v;
	}
}



/*
 * Process an event (check for a keypress)
 *
 * The keypress processing code is often the most system dependant part
 * of Angband, since sometimes even the choice of compiler is important.
 *
 * For the IBM, we divide all keypresses into two categories, first, the
 * "normal" keys, including all keys required to play Angband, and second,
 * the "special" keys, such as keypad keys, function keys, and various keys
 * used in combination with various modifier keys.
 *
 * To simplify this file, we use Angband's "macro processing" ability, in
 * combination with a specialized "pref-ibm.prf" file, to handle most of the
 * "special" keys, instead of attempting to fully analyze them here.  This
 * file only has to determine when a "special" key has been pressed, and
 * translate it into a simple string which signals the use of a "special"
 * key, the set of modifiers used, if any, and the hardware scan code of
 * the actual key which was pressed.  To simplify life for the user, we
 * treat both "shift" keys as identical modifiers.
 *
 * The final encoding is "^_MMMxSS\r", where "MMM" encodes the modifiers
 * ("C" for control, "S" for shift, "A" for alt, or any ordered combination),
 * and "SS" encodes the keypress (as the two "digit" hexadecimal encoding of
 * the scan code of the key that was pressed), and the "^_" and "x" and "\r"
 * delimit the encoding for recognition by the macro processing code.
 *
 * Some important facts about scan codes follow.  All "normal" keys use
 * scan codes from 1-58.  The "function" keys use 59-68 (and 133-134).
 * The "keypad" keys use 69-83.  Escape uses 1.  Enter uses 28.  Control
 * uses 29.  Left Shift uses 42.  Right Shift uses 54.  PrtScrn uses 55.
 * Alt uses 56.  Space uses 57.  CapsLock uses 58.  NumLock uses 69.
 * ScrollLock uses 70.  The "keypad" keys which use scan codes 71-83
 * are ordered KP7,KP8,KP9,KP-,KP4,KP5,KP6,KP+,KP1,KP2,KP3,INS,DEL.
 *
 * Using "bioskey(0x10)" instead of "bioskey(0)" apparently provides more
 * information, including better access to the keypad keys in combination
 * with various modifiers, but only works on "PC's after 6/1/86", and there
 * is no way to determine if the function is provided on a machine.  I have
 * been told that without it you cannot detect, for example, control-left.
 * The basic scan code + ascii value pairs returned by the keypad follow,
 * with values in parentheses only available to "bioskey(0x10)".
 *
 *         /      *      -      +      1      2      3      4
 * Norm:  352f   372a   4a2d   4e2b   4f00   5000   5100   4b00
 * Shft:  352f   372a   4a2d   4e2b   4f31   5032   5133   4b34
 * Ctrl: (9500) (9600) (8e00) (9000)  7500  (9100)  7600   7300
 *
 *         5      6      7      8      9      0      .     Enter
 * Norm: (4c00)  4d00   4700   4800   4900   5200   5300  (e00d)
 * Shft:  4c35   4d36   4737   4838   4939   5230   532e  (e00d)
 * Ctrl: (8f00)  7400   7700  (8d00)  8400  (9200) (9300) (e00a)
 *
 * See "pref-ibm.prf" for the "standard" macros for various keys.
 *
 * Certain "bizarre" keypad keys (such as "enter") return a "scan code"
 * of "0xE0", and a "usable" ascii value.  These keys should be treated
 * like the normal keys, see below.  XXX XXX XXX Note that these "special"
 * keys could be prefixed with an optional "ctrl-^" which would allow them
 * to be used in macros without hurting their use in normal situations.
 */
static errr Term_xtra_ibm_event(int v)
{
	int i, k, s;

	bool mc = FALSE;
	bool ms = FALSE;
	bool ma = FALSE;


	/* Hack -- Check for a keypress */
	if (!v && !bioskey(1)) return (1);

	/* Wait for a keypress */
	k = bioskey(0x10);

	/* Access the "modifiers" */
	i = bioskey(2);

	/* Extract the "scan code" */
	s = ((k >> 8) & 0xFF);

	/* Extract the "ascii value" */
	k = (k & 0xFF);

	/* Process "normal" keys */
	if ((s <= 58) || (s == 0xE0))
	{
		/* Enqueue it */
		if (k) (void)Term_keypress(k);

		/* Success */
		return (0);
	}

	/* Extract the modifier flags */
	if (i & (1 << K_CTRL)) mc = TRUE;
	if (i & (1 << K_LSHIFT)) ms = TRUE;
	if (i & (1 << K_RSHIFT)) ms = TRUE;
	if (i & (1 << K_ALT)) ma = TRUE;


	/* Begin a "macro trigger" */
	Term_keypress(31);

	/* Hack -- Send the modifiers */
	if (mc) (void)Term_keypress('C');
	if (ms) (void)Term_keypress('S');
	if (ma) (void)Term_keypress('A');

	/* Introduce the hexadecimal scan code */
	Term_keypress('x');

	/* Encode the hexadecimal scan code */
	(void)Term_keypress(hexsym[s/16]);
	(void)Term_keypress(hexsym[s%16]);

	/* Success */
	return (0);
}


/*
 * Handle a "special request"
 *
 * The given parameters are "valid".
 */
static errr Term_xtra_ibm(int n, int v)
{
	int i;

	/* Analyze the request */
	switch (n)
	{
		/* Make a "bell" noise */
		case TERM_XTRA_NOISE:
		{
			/* Make a bell noise */
			(void)write(1, "\007", 1);

			/* Success */
			return (0);
		}

		/* Set the cursor shape */
		case TERM_XTRA_SHAPE:
		{
			/* Set cursor shape */
			curs_set(v);

			/* Success */
			return (0);
		}

		/* Flush one line of output */
		case TERM_XTRA_FROSH:
		{
#ifdef USE_WAT

			/* Copy the virtual screen to the physical screen */
			memcpy(PhysicalScreen + (v*160), VirtualScreen + (v*160), 160);

#else /* USE_WAT */

			/* Apply the virtual screen to the physical screen */
			ScreenUpdateLine(VirtualScreen + ((v*cols) << 1), v);

#endif /* USE_WAT */

			/* Success */
			return (0);
		}

		/* Clear the screen */
		case TERM_XTRA_CLEAR:
		{
			/* Clear each line (virtual or physical) */
			for (i = 0; i < rows; i++)
			{
				/* Clear the line */
				memcpy((VirtualScreen + ((i*cols) << 1)), wiper, (cols << 1));
			}

#ifdef USE_WAT

			/* Copy the virtual screen to the physical screen */
			memcpy(PhysicalScreen, VirtualScreen, rows*cols*2);

#else /* USE_WAT */

			/* Erase the physical screen */
			ScreenClear();

#endif /* USE_WAT */

			/* Success */
			return (0);
		}

		/* Process events */
		case TERM_XTRA_EVENT:
		{
			/* Process one event */
			return (Term_xtra_ibm_event(v));
		}

		/* Flush events */
		case TERM_XTRA_FLUSH:
		{
			/* Strip events */
			while (!Term_xtra_ibm_event(FALSE)) /* loop */;

			/* Success */
			return (0);
		}

		/* React to global changes */
		case TERM_XTRA_REACT:
		{
			/* React to "color_table" changes */
			return (Term_xtra_ibm_react());
		}

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY:
		{
			if (v <= 0) return (0);

			/* Delay if needed */
			if (v > 0) djgpp_delay(v);

			/* Success */
			return (0);
		}
	}

	/* Unknown request */
	return (1);
}



/*
 * Move the cursor
 *
 * The given parameters are "valid".
 */
static errr Term_curs_ibm(int x, int y)
{

#ifdef USE_WAT

	union REGS r;

	r.h.ah = 2;
	r.h.bh = 0;
	r.h.dl = x;
	r.h.dh = y;

	/* Place the cursor */
	int86(0x10, &r, &r);

#else /* USE_WAT */

	/* Move the cursor */
	ScreenSetCursor(y, x);

#endif /* USE_WAT */

	/* Success */
	return (0);
}


/*
 * Erase a block of the screen
 *
 * The given parameters are "valid".
 */
static errr Term_wipe_ibm(int x, int y, int n)
{
	/* Wipe part of the virtual (or physical) screen */
	memcpy(VirtualScreen + ((cols*y + x)<<1), wiper, n<<1);

	/* Success */
	return (0);
}


/*
 * Translate from ISO Latin-1 characters 128+ to 8-bit IBM extended ASCII.
 *
 * Many IBM extended characters are semi-graphical; we carefully do not
 * translate them.
 */
const byte ibm_char_conv[128] =
{
	  0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,
	  0, 173, 135, 136,   0, 137,   0,  21,
	  0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0,   0,
	  0,   0,   0,   0,   0,   0,   0, 168,
	'A', 'A', 'A', 'A', 142, 143, 146, 128,
	144, 'E', 'E', 'E', 152, 152, 152, 152,
	'D', 165, 'O', 'O', 'O', 'O', 153,   0,
	'O', 'U', 'U', 'U', 154, 'Y',   0, 225,
	133, 160, 131, 'a', 132, 134, 145, 135,
	138, 130, 136, 137, 'i', 161, 140, 139,
	'o', 164, 149, 162, 147, 'o', 148,   0,
	237, 151, 163, 150, 129, 'y',   0, 'y'
};


/*
 * Given a position in the ISO Latin-1 character set, return
 * the IBM ASCII equivalent.
 */
static byte Term_xchar_ibm(byte c)
{
	byte s;

	/* 7-bit characters are not changed */
	if (c < 128) return (c);

	/* Translate extended characters */
	s = ibm_char_conv[c - 128];

	/* Ignore translations to zero */
	if (s) return (s);
	return (c);
}


/*
 * Place some text on the screen using an attribute
 *
 * The given parameters are "valid".  Be careful with "a".
 *
 * The string "cp" has length "n" and is NOT null-terminated.
 */
static errr Term_text_ibm(int x, int y, int n, byte a, const char *cp)
{
	register int i;
	register byte attr;
	register byte *dest;

	/* Extract a color index */
	if (a > 15) attr = color_table[a].color_translate;
	else attr = a;

	/* Access the virtual (or physical) screen */
	dest = VirtualScreen + (((cols * y) + x) << 1);

	/* Save the data */
	for (i = 0; i < n; i++)
	{
		/* Apply */
		*dest++ = cp[i];
		*dest++ = attr;
	}

	/* Success */
	return (0);
}


/*
 * Place some attr/char pairs on the screen
 *
 * The given parameters are "valid".
 */
static errr Term_pict_ibm(int x, int y, int n, const byte *ap, const char *cp, const byte *tap, const char *tcp)
{
	register int i;
	register byte attr;
	register byte *dest;

	/* Ignore transparency */
	(void)*tap;
	(void)*tcp;


	/* Access the virtual (or physical) screen */
	dest = VirtualScreen + (((cols * y) + x) << 1);

	/* Save the data */
	for (i = 0; i < n; i++)
	{
		/* Extract a color index */
		if (ap[i] > 15) attr = color_table[ap[i]].color_translate;
		else attr = ap[i];

		/* Apply */
		*dest++ = cp[i];
		*dest++ = attr;
	}

	/* Success */
	return (0);
}


/*
 * Init a Term
 */
static void Term_init_ibm(term *t)
{
	/* Unused parameter */
	(void)t;

	/* XXX Nothing */
}


/*
 * Nuke a Term
 */
static void Term_nuke_ibm(term *t)
{
#ifdef USE_WAT

	/* Nothing */

#else /* USE_WAT */

	union REGS r;

#endif /* USE_WAT */

	/* Unused parameter */
	(void)t;

	/* Move the cursor to the bottom of the screen */
	Term_curs_ibm(0, rows-1);

#ifdef USE_WAT

	/* Restore the original video mode */
	_setvideomode(_DEFAULTMODE);

#else /* USE_WAT */

	/* Restore the original video mode */
	r.h.ah = 0x00;
	r.h.al = 0x03;
	int86(0x10, &r, &r);

#endif /* USE_WAT */

	/* Make the cursor visible */
	curs_set(1);
}



#ifdef USE_GRAPHICS

#ifdef USE_WAT  /* Extra functions for the Watcom compiler */

/*
 * This structure is used by the DMPI function to hold registers when
 * doing a real mode interrupt call.  (Stolen from the DJGPP <dpmi.h>
 * header file).
 */

typedef union
{
	struct
	{
		unsigned long edi;
		unsigned long esi;
		unsigned long ebp;
		unsigned long res;
		unsigned long ebx;
		unsigned long edx;
		unsigned long ecx;
		unsigned long eax;
	} d;
	struct
	{
		unsigned short di, di_hi;
		unsigned short si, si_hi;
		unsigned short bp, bp_hi;
		unsigned short res, res_hi;
		unsigned short bx, bx_hi;
		unsigned short dx, dx_hi;
		unsigned short cx, cx_hi;
		unsigned short ax, ax_hi;
		unsigned short flags;
		unsigned short es;
		unsigned short ds;
		unsigned short fs;
		unsigned short gs;
		unsigned short ip;
		unsigned short cs;
		unsigned short sp;
		unsigned short ss;
	} x;
	struct
	{
		unsigned char edi[4];
		unsigned char esi[4];
		unsigned char ebp[4];
		unsigned char res[4];
		unsigned char bl, bh, ebx_b2, ebx_b3;
		unsigned char dl, dh, edx_b2, edx_b3;
		unsigned char cl, ch, ecx_b2, ecx_b3;
		unsigned char al, ah, eax_b2, eax_b3;
	} h;
} __dpmi_regs;

unsigned  __dpmi_allocate_dos_memory(int size, unsigned *selector)
{
	union REGPACK regs =
	{0};

	regs.w.ax  = 0x100;   /* DPMI function -- allocate low memory */
	regs.w.bx  = size;    /* Number of Paragraphs to allocate */
	intr(0x31, &regs);    /* DPMI interface */

	*selector = regs.w.dx;
	return (regs.w.ax);
};

void __dpmi_free_dos_memory(unsigned sel)
{
	union REGPACK regs =
	{0};

	regs.w.ax  = 0x101;      /* DPMI function -- free low memory */
	regs.x.edx = sel;        /* PM selector for memory block */
	intr(0x31, &regs);       /* DPMI interface */
};

void __dpmi_int(int intno, __dpmi_regs *dblock)
{
	union REGPACK regs =
	{0};

	regs.w.ax  = 0x300;           /* DPMI function -- real mode interrupt */
	regs.h.bl  = intno;           /* interrupt 0x10 */
	regs.x.edi = FP_OFF(dblock);  /* Pointer to dblock (offset and segment) */
	regs.x.es  = FP_SEG(dblock);
	intr(0x31, &regs);            /* DPMI interface */
};

unsigned short __dpmi_sel = 0x0000;
#define _farsetsel(x) __dpmi_sel=(x)
extern void _farnspokeb(unsigned long offset, unsigned char value);
#pragma aux _farnspokeb =        \
          "push   fs"            \
          "mov    fs,__dpmi_sel" \
          "mov    fs:[eax],bl"   \
          "pop    fs"            \
          parm [eax] [bl];

#else /* USE_WAT */

#include <dpmi.h>
#include <go32.h>
#include <sys/farptr.h>

#endif /* USE_WAT */


/*
 * Since you cannot send 32bit pointers to a 16bit interrupt handler
 * and the video BIOS wants a (16bit) pointer to the font, we have
 * to allocate a block of dos memory, copy the font into it, then
 * translate a 32bit pointer into a 16bit pointer to that block.
 *
 * DPMI - Dos Protected Mode Interface provides functions that let
 *        us do that.
 */
static void enable_graphic_font(const char *font)
{
	__dpmi_regs dblock;

	unsigned int seg, i;
	int sel;

	(void)WIPE(&dblock, __dpmi_regs);

	/*
	 * Allocate a block of memory 4096 bytes big in `low memory' so a real
	 * mode interrupt can access it.  Real mode pointer is returned as seg:0
	 * Protected mode pointer is sel:0.
	 */
	seg = __dpmi_allocate_dos_memory(256, &sel);

	/* Copy the information into low memory buffer, by copying one byte at
	 * a time.  According to the info in <sys/farptr.h>, the functions
	 * _farsetsel() and _farnspokeb() will optimise away completely
	 */
	_farsetsel(sel);               /* Set the selector to write to */
	for (i = 0; i<4096; i++)
	{
		_farnspokeb(i, *font++);      /* Copy 1 byte into low (far) memory */
	}

	/*
	 * Now we use DPMI as a jumper to call the real mode interrupt.  This
	 * is needed because loading `es' while in protected mode with a real
	 * mode pointer will cause a Protection Fault and calling the interrupt
	 * directly using the protected mode pointer will result in garbage
	 * being received by the interrupt routine
	 */
	dblock.d.eax = 0x1102;         /* BIOS function -- set font */
	dblock.d.ebx = 0x800;         /* bh = size of a letter; bl = 0 (reserved) */
	dblock.d.ecx = 0x00FF;         /* Last character in font */
	dblock.x.es  = seg;            /* Pointer to font segment */
	dblock.d.ebp = 0x0000;         /* Pointer to font offset */

	__dpmi_int(0x10, &dblock);

	/* We're done with the low memory, free it */
	__dpmi_free_dos_memory(sel);
}

#endif /* ALLOW_GRAPH */

const char help_ibm[] = "IBM Visual Display Support";


/*
 * Change the display.
 *
 * We only switch between 25-line display, using a font taller than it is
 * wide, and 50-line display, using a font of equal height and width.
 */
static errr switch_display_ibm(int display)
{
	union REGS r;
	term *old = Term;

	/* Make sure that the main term is active */
	(void)Term_activate(term_main);

	/* Use the tall display (50 rows in our case) */
	if (display)
	{
#ifdef USE_WAT

		/* I need someone with a Watcom compiler to make this work... */
		_setvideomode(_TEXTC80);
		_settextrows(50);

#else /* USE_WAT */

		/* Use the ROM 8x8 font */
		r.h.ah = 0x11;
		r.h.bl = 0x00;
		r.h.al = 0x12;
		int86(0x10, &r, &r);

#endif /* USE_WAT */

		/* Resize the main term to 80x50 */
		(void)Term_resize(80, 50);
	}

	/* Use the regular display (25 rows) */
	else
	{
#ifdef USE_WAT

		/* I need someone with a Watcom compiler to make this work... */
		_setvideomode(_TEXTC80);
		_settextrows(25);

#else /* USE_WAT */

		/* Use the ROM 8x16 font */
		r.h.ah = 0x11;
		r.h.bl = 0x00;
		r.h.al = 0x14;
		int86(0x10, &r, &r);

#endif /* USE_WAT */

		/* Resize the main term to 80x25 */
		(void)Term_resize(80, 25);
	}

	/* Redraw the main term */
	(void)Term_redraw();

	/* Restore previous term (if different) */
	(void)Term_activate(old);

	/* Assume success */
	return (0);
}


/*
 * Free any structures specific to the IBM port
 */
static void hook_quit(cptr str)
{
	/* Display a message */
	if (str)
	{
		prt(str, 0, 0);
		(void)Term_fresh();
		(void)inkey(FALSE);
	}

	/* Free the virtual screen */
	FREE(VirtualScreen);
}


/*
 * Initialize the IBM "visual module"
 *
 * Hack -- we assume that "blank space" should be "white space"
 * (and not "black space" which might make more sense).
 *
 * Note the use of "((x << 2) | (x >> 4))" to "expand" a 6 bit value
 * into an 8 bit value, without losing much precision, by using the 2
 * most significant bits as the least significant bits in the new value.
 */
errr init_ibm(int argc, char **argv)
{
	int i;

	term *t = &term_screen_body;

	union REGS r;

	/* Unused parameters */
	(void)argc;
	(void)argv;

	/* Check for "Windows" */
	if (getenv("windir"))
	{
		r.h.ah = 0x16;           /* Windows API Call -- Set device focus */
		r.h.al = 0x8B;           /* Causes Dos boxes to become fullscreen */
		r.h.bh = r.h.bl = 0x00;  /* 0x0000 = current Dos box */
		int86(0x2F, &r, &r);       /* Call the Windows API */
	};

	/* Initialize "color_table" */
	for (i = 0; i < 16; i++)
	{
		long rv, gv, bv;

		/* Extract desired values */
		rv = color_table[i].rv >> 2;
		gv = color_table[i].gv >> 2;
		bv = color_table[i].bv >> 2;

		/* Extract the "complex" codes */
		ibm_color_complex[i] = ((rv) | (gv << 8) | (bv << 16));
	}


#ifdef USE_WAT

	/* Set the video mode */
	if (!(_setvideomode(_VRES16COLOR)))
	{
		quit(format("You must have at least VGA resolution to run %s.",
			VERSION_NAME));
	}

#else /* USE_WAT */

	/* Request 400 scan lines */
	r.h.ah = 0x12;
	r.h.bl = 0x30;
	r.h.al = 0x02;
	int86(0x10, &r, &r);

	/* Require VGA */
	if (r.h.al != 0x12)
	{
		quit(format("You must have at least VGA resolution to run %s.",
			VERSION_NAME));
	}

	/* Reset to color text mode */
	r.h.al = 0x03;
	int86(0x10, &r, &r);

#endif /* USE_WAT */


	/* Use 50 text lines */
	switch_display_ibm(TRUE);

	/* Instantiate the color set */
	activate_color_complex();

/* This port can use bitmapped character graphics, but they have not been updated. */
#ifdef USE_GRAPHICS


	/* Try to activate bitmap graphics */
	if (arg_graphics == GRAPHICS_CHAR)
	{
		FILE *f;

		char buf[4096];

		/* Build the filename */
		path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA, "angband.fnt");

		/* Open the file */
		f = fopen(buf, "rb");

		/* Okay */
		if (f)
		{
			/* Load the bitmap data */
			if (fread(buf, 1, sizeof(buf), f) != 4096)
			{
				quit("Corrupt 'angband.fnt' file");
			}

			/* Close the file */
			fclose(f);

			/* Enable graphics */
			enable_graphic_font(buf);

			/* Enable colors (again) */
			activate_color_complex();

			/* Use graphics */
			use_graphics = GRAPHICS_CHAR;
		}
	}

#endif

	/* Build a "wiper line" */
	for (i = 0; i < cols; i++)
	{
		/* Space */
		wiper[2*i] = ' ';

		/* Black */
		wiper[2*i+1] = TERM_WHITE;
	}


	/* Make the virtual screen */
	C_MAKE(VirtualScreen, rows * cols * 2, byte);

	/* Set the exit hook */
	quit_aux = hook_quit;

	/* Erase the screen */
	Term_xtra_ibm(TERM_XTRA_CLEAR, 0);


	/* Place the cursor */
	Term_curs_ibm(0, 0);


	/* Access the "default" cursor info */
	r.h.ah = 3;
	r.h.bh = 0;

	/* Make the call */
	int86(0x10, &r, &r);

	/* Extract the standard cursor info */
	saved_cur_v = 1;
	saved_cur_high = r.h.ch;
	saved_cur_low = r.h.cl;


	/* Initialize the term */
	(void)term_init(t, cols, rows-1, 256);

	/* Always use "Term_pict()" */
	t->always_pict = TRUE;

	/* Use "white space" to erase */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

	/* Prepare the init/nuke hooks */
	t->init_hook = Term_init_ibm;
	t->nuke_hook = Term_nuke_ibm;

	/* Connect the hooks */
	t->xtra_hook = Term_xtra_ibm;
	t->curs_hook = Term_curs_ibm;
	t->wipe_hook = Term_wipe_ibm;
	t->text_hook = Term_text_ibm;
	t->pict_hook = Term_pict_ibm;
	t->xchar_hook = Term_xchar_ibm;
	switch_display_hook = switch_display_ibm;

	/* Save it */
	angband_term[TERM_MAIN] = t;

	/* Activate it */
	Term_activate(term_main);

	/* Success */
	return (0);
}


#endif /* USE_IBM */
