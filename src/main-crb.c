/* File: main-crb.c */

/*
 * Copyright (c) 1997 Ben Harrison, Keith Randall, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */


/*
 * This file helps Angband work with Macintosh computers running OS X,
 * or OS 8/9 with CarbonLib system extention.
 *
 * To use this file, use an appropriate "Makefile" or "Project File", which
 * should define "MACINTOSH".
 *
 * The official compilation uses the CodeWarrior Pro compiler.
 *
 * If you are never going to use "graphics" (especially if you are not
 * compiling support for graphics anyway) then you can delete the "pict"
 * resources with id "1001", "1002" and "1003" with no dangerous side effects.
 *
 *
 * This file assumes that you will be using a PPC Mac running OS X
 * or OS 8/9 (8.6 or greater) with CarbonLib system extention enabled.
 * In fact, the game will refuse to run unless these features are available.
 *
 * Note that the "preference" file is now a simple XML text file
 * called "<program name>.plist", which contains a version stamp, so that
 * obsolete preference files can be ignored.
 *
 *
 * Note that "init1.c", "init2.c", "load1.c", "load2.c", and "birth.c"
 * should probably be "unloaded" as soon as they are no longer needed,
 * to save space, but I do not know how to do this.  XXX XXX XXX
 *
 * Stange bug -- The first "ClipRect()" call crashes if the user closes
 * all the windows, switches to another application, switches back, and
 * re-opens the main window, for example, using "command-a".  XXX XXX XXX
 *
 *
 * Initial framework (and most code) by Ben Harrison (benh@phial.com).
 *
 * Some code adapted from "MacAngband 2.6.1" by Keith Randall
 *
 * Initial PowerMac port by Maarten Hazewinkel (mmhazewi@cs.ruu.nl).
 *
 * Most "USE_SFL_CODE" code provided by Steve Linberg (slinberg@crocker.com).
 *
 * Carbon code adopted from works by Peter Ammon and Ron Anderson.
 *
 * Some API calls are updated to OS 8.x-- ones.
 *
 * Pixmap locking code in Term_pict_map() follows Carbon Porting Guide
 * by Apple.
 *
 * The idle loop in TERM_XTRA_DELAY is rewritten to sleep on WaitNextEvent
 * for a couple of reasons.
 *
 * CheckEvent now really blocks whenever asked to wait.
 *
 * The unused buffer GWorld is completely removed. It has long been pure waste
 * of memory.
 *
 * Most of the graphics code is adapted from an extremely minimal subset of
 * the "Sprite World II" package, an amazing (and free) animation package.
 *
 *
 * Important Resources in the resource file:
 *
 *   FREF 130 = ANGBAND_CREATOR / 'APPL' (application)
 *   FREF 129 = ANGBAND_CREATOR / 'SAVE' (save file)
 *   FREF 130 = ANGBAND_CREATOR / 'TEXT' (bone file, generic text file)
 *   FREF 131 = ANGBAND_CREATOR / 'DATA' (binary image file, score file)
 *
 *   DLOG 128 = "About Angband..."
 *
 *   ALRT 128 = unused (?)
 *   ALRT 129 = "Warning..."
 *   ALRT 130 = "Are you sure you want to quit without saving?"
 *
 *   DITL 128 = body for DLOG 128
 *   DITL 129 = body for ALRT 129
 *   DITL 130 = body for ALRT 130
 *
 *   ICON 128 = "warning" icon
 *
 *   MENU 128 = apple (about, -, ...)
 *   MENU 129 = File (new, open, close, save, -, exit, quit)
 *     (If SAVEFILE_SCREEN is defined)
 *   MENU 129 = File (close, save, -, exit, quit)
 *   MENU 130 = Edit (undo, -, cut, copy, paste, clear)
 *
 *   PICT 1001 = Graphics tile set (8x8)
 *   PICT 1002 = Graphics tile set (16x16 images)
 *   PICT 1003 = Graphics tile set (16x16 masks)
 *
 *   STR# 128 = "Please select the "lib" folder"
 *
 *   plst 0   can be empty, but required for single binary Carbon apps on OS X
 *
 * Note: You can no longer use the exit menu unless you build the programme
 *       with an appropriate compile-time option.
 *
 *
 * File name patterns:
 *   all 'APEX' files have a filename of the form "*:apex:*" (?)
 *   all 'BONE' files have a filename of the form "*:bone:*" (?)
 *   all 'DATA' files have a filename of the form "*:data:*"
 *   all 'SAVE' files have a filename of the form "*:save:*"
 *   all 'USER' files have a filename of the form "*:user:*" (?)
 *
 * Perhaps we should attempt to set the "_ftype" flag inside this file,
 * to avoid nasty file type information being spread all through the
 * rest of the code.  (?)  This might require adding hooks into the
 * "fd_open()" and "my_fopen()" functions in "util.c".  XXX XXX XXX
 *
 *
 * Reasons for each header file:
 *
 *   angband.h = Angband header file
 *
 *   Types.h = (included anyway)
 *   Gestalt.h = gestalt code
 *   QuickDraw.h = (included anyway)
 *   OSUtils.h = (included anyway)
 *   Files.h = file code
 *   Fonts.h = font code
 *   Menus.h = menu code
 *   Dialogs.h = dialog code
 *   Windows.h = (included anyway)
 *   Palettes.h = palette code
 *   StandardFile.h = file dialog box
 *   DiskInit.h = disk initialization
 *   ToolUtils.h = HiWord() / LoWord()
 *   Desk.h = OpenDeskAcc()
 *   Devices.h = OpenDeskAcc()
 *   Events.h = event code
 *   Resources.h = resource code
 *   Controls.h = button code
 *   SegLoad.h = ExitToShell(), AppFile, etc
 *   Memory.h = SetApplLimit(), NewPtr(), etc
 *   QDOffscreen.h = GWorld code
 *   Sound.h = Sound code
 *   Navigation.h = save file / lib locating dialogues
 *   CFPreferences.h = Preferences
 *   CFNumber.h = read/write short values from/to preferences
 */

/*
 * Carbonizer Carbonized (pelpel) - revision 4
 *
 * Since I'm using CodeWarrior, the traditional header files are
 * #include'd below.
 *
 * If you try this on OS X + Project Builder, a single <Carbon/Carbon.h>
 * is enough. #define MACINTOSH does not work well with the rest of the
 * code, though. Carbon (not Carbon + nib) targets seem to accept
 * traditional Resource Manager resources (in the DeRez format),
 * but I haven't have any luck with it, yet. Conversion to nib
 * resources mean non-trivial rewrite of resource handling code,
 * which I'm very reluctant to do.
 *
 * Code adopted from Peter Ammon's work on 2.8.3 and some modifications
 * are made when Apple's Carbon Porting Guide says they are absolutely
 * necessary. Other arbirary changes are mostly because of my hatred
 * of deep nests and indentations.
 *
 * I also took Ron Anderson's (minimising the use of local-global coordinate
 * conversions). Some might say his QuickTime multimedia is the most
 * significant achievement... Play your favourite CD instead, if you really
 * miss that (^ ^;)
 *
 * I replaced some old API calls with new (OS 8.x--) ones, especially
 * when I felt Apple is strongly against their continued usage.
 *
 * On the contrary, I deliverately left traditional resource interfaces.
 * Whatever Apple might say, I abhor file name extentions. And keeping two
 * different sets of resources for Classic and Carbon is just too much for
 * a personal project XXX
 *
 * Because Carbon forbids the use of 68K code, ANGBAND_LITE_MAC sections
 * are removed.
 *
 *
 * You can use the resource file that comes with the ext-mac archive
 * on the Angband FTP server, with these two additions:
 * - plst 0 : can be empty, although Apple recommends us to fill it in.
 * - STR# 128 : something like "Please select your lib folder"
 *
 * To build Carbonised Angband with CodeWarrior, copy your PPC project
 * and
 * - replace main-mac.c in the project with this file (in the link order tab)
 * - remove InterfaceLib and MathLib
 * - add CarbonLib (found in Carbon SDK or CW's UniversalInterfaces) --
 *   if you have compiler/linker errors, you'll need Carbon SDK 1.1 or greater
 * - replace MSL C.PPC.Lib with MSL C.Carbon.Lib (both found in
 *   MSL:MSL_C:MSL_MacOS:Lib:PPC)
 * - leave MSL RuntimePPC.Lib as it is
 * - don't forget to update resource file, as described above
 * - as in Classic targets, you may have to include <unistd.h> and
 *   <fcntl.h>. The most convinient place for them is the first
 *   #ifdef MACINTOSH in h-system.h
 * - check variant dependent ifdef's explained below, and add
 *   appropriate one(s) in you A-mac-h.pch. As far as I know,
 *   [V] (of course) and [Ey] doesn't require any.
 */

/*
 * Force Carbon-compatible APIs
 */
#define TARGET_API_MAC_CARBON 1

#include "angband.h"


#ifdef MACINTOSH

/*
 * Variant-dependent features:
 *
 * #define ALLOW_BIG_SCREEN (O and Z. Dr's big screen needs more work)
 * #define ANG281_RESET_VISUALS (Cth, Gum, Pern, Z)
 * #define SAVEFILE_SCREEN (Pern)
 * #define AUTO_SAVE_ARG_REQUIRED (O and Z)
 * #define ANGBAND_CREATOR four letter code for your variant, if any.
 *  or use the default one.
 *
 * If a variant supports transparency effect for 16x16 tiles but doesn't
 * have variable use_transparency in the code (i.e. Pern), then
 * #define NO_USE_TRANSPARENCY_VAR
 */


/* Default creator signature */
#ifndef ANGBAND_CREATOR
# define ANGBAND_CREATOR 'A271'
#endif


/*
 * In OS X + Project Buiilder, use <Carbon/Carbon.h> for ALL of these, 
 * including the Apple Event ones (i.e. ifdef USE_SFL_CODE)
 */
#ifndef __MWERKS__

#include <Carbon/Carbon.h>
#include <CoreServices/CoreServices.h>
#include <CoreFoundation/CoreFoundation.h>

#else

#include <Types.h>
#include <Gestalt.h>
#include <QuickDraw.h>
#include <Files.h>
#include <Fonts.h>
#include <Menus.h>
#include <Dialogs.h>
#include <Windows.h>
#include <Palettes.h>
#include <StandardFile.h>
#include <DiskInit.h>
#include <ToolUtils.h>
#include <Devices.h>
#include <Events.h>
#include <Resources.h>
#include <Controls.h>
#include <SegLoad.h>
#include <Memory.h>
#include <QDOffscreen.h>
#include <Sound.h>
#include <Navigation.h>
#include <CFPreferences.h>
#include <CFNumber.h>

#endif /* !__MWERKS__ */

/*
 * Use "malloc()" instead of "NewPtr()"
 */
/* #define USE_MALLOC */

/*
 * Activate some special code - never comment this out.
 */
#define USE_SFL_CODE

/*
 * The sole exception to the above statement - use <Carbon/Carbon.h> instead
 * if you compile this on OS X + Project Manager
 */
#if defined(USE_SFL_CODE) && defined(__MWERKS__)

/*
 * Include the necessary header files
 */
#include <AppleEvents.h>
#include <EPPC.h>
#include <Folders.h>

#endif /* USE_SFL_CODE && __MWERKS__ */


/*
 * Information about each of the 256 available colors
 */
static RGBColor color_info[256];


/*
 * Forward declare
 */
typedef struct term_data term_data;

/*
 * Extra "term" data
 */
struct term_data
{
	term *t;

	Rect r;

	WindowPtr w;


	short padding;

	short pixelDepth;

	GWorldPtr theGWorld;	/* not used ... */

	GDHandle theGDH;

	GDHandle mainSWGDH;	/* not used ... */

	Str15 title;

	s16b oops;

	s16b keys;

	s16b last;

	s16b mapped;

	s16b rows;
	s16b cols;

	s16b font_id;
	s16b font_size;
	s16b font_face;
	s16b font_mono;

	s16b font_o_x;
	s16b font_o_y;
	s16b font_wid;
	s16b font_hgt;

	s16b tile_o_x;
	s16b tile_o_y;
	s16b tile_wid;
	s16b tile_hgt;

	s16b size_wid;
	s16b size_hgt;

	s16b size_ow1;
	s16b size_oh1;
	s16b size_ow2;
	s16b size_oh2;
};




/*
 * Forward declare -- see below
 */
static bool CheckEvents(bool wait);


/*
 * Hack -- location of the main directory
 */
static short app_vol;
static long  app_dir;


/*
 * Delay handling of double-clicked savefiles
 */
Boolean open_when_ready = FALSE;

/*
 * Delay handling of pre-emptive "quit" event
 */
Boolean quit_when_ready = FALSE;


/*
 * Aqua automatically supplies the Quit menu.
 */
static Boolean is_aqua = FALSE;

/*
 * Version of Mac OS - for version specific bug workarounds (; ;)
 */
static long mac_os_version;


/*
 * Hack -- game in progress
 */
static int game_in_progress = 0;


/*
 * Only do "SetPort()" when needed
 */
static WindowPtr active = NULL;


/*
 * Maximum number of terms
 */
#define MAX_TERM_DATA 8


/*
 * An array of term_data's
 */
static term_data data[MAX_TERM_DATA];


/*
 * Note when "open"/"new" become valid
 */
static bool initialized = FALSE;


#ifdef ALLOW_QUITING

/*
 * CodeWarrior uses Universal Procedure Pointers
 */
static ModalFilterUPP ynfilterUPP;

#endif /* ALLOW_QUITING */


/*
 * Convert refnum+vrefnum+fname into a full file name
 * Store this filename in 'buf' (make sure it is long enough)
 * Note that 'fname' looks to be a "pascal" string
 */
static void refnum_to_name(char *buf, long refnum, short vrefnum, char *fname)
{
	DirInfo pb;
	Str255 name;
	int err;
	int i, j;

	char res[1000];

	i=999;

	res[i]=0; i--;
	for (j=1; j<=fname[0]; j++)
	{
		res[i-fname[0]+j] = fname[j];
	}
	i-=fname[0];

	pb.ioCompletion=NULL;
	pb.ioNamePtr=name;
	pb.ioVRefNum=vrefnum;
	pb.ioDrParID=refnum;
	pb.ioFDirIndex=-1;

	while (1)
	{
		pb.ioDrDirID=pb.ioDrParID;
		err = PBGetCatInfoSync((CInfoPBPtr)&pb);
		res[i] = ':'; i--;
		for (j=1; j<=name[0]; j++)
		{
			res[i-name[0]+j] = name[j];
		}
		i -= name[0];

		if (pb.ioDrDirID == fsRtDirID) break;
	}

	/* Extract the result */
	for (j = 0, i++; res[i]; j++, i++) buf[j] = res[i];
	buf[j] = 0;
}


#if 0 /* Very old */

/*
 * XXX XXX XXX Allow the system to ask us for a filename
 */
static bool askfor_file(char *buf, int len)
{
	SFReply reply;
	Str255 dflt;
	Point topleft;
	short vrefnum;
	long drefnum, junk;

	/* Default file name */
	sprintf((char*)dflt + 1, "%s's description", buf);
	dflt[0] = strlen((char*)dflt + 1);

	/* Ask for a file name */
	topleft.h=(qd.screenBits.bounds.left+qd.screenBits.bounds.right)/2-344/2;
	topleft.v=(2*qd.screenBits.bounds.top+qd.screenBits.bounds.bottom)/3-188/2;
	SFPutFile(topleft, "\pSelect a filename:", dflt, NULL, &reply);
	/* StandardPutFile("\pSelect a filename:", dflt, &reply); */

	/* Process */
	if (reply.good)
	{
		int fc;

		/* Get info */
		GetWDInfo(reply.vRefNum, &vrefnum, &drefnum, &junk);

		/* Extract the name */
		refnum_to_name(buf, drefnum, vrefnum, (char*)reply.fName);

		/* Success */
		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}

#endif /* Very old */



/*
 * Center a rectangle inside another rectangle
 *
 * Consider using RepositionWindow() whenever possible
 */
static void center_rect(Rect *r, Rect *s)
{
	int centerx = (s->left + s->right)/2;
	int centery = (2*s->top + s->bottom)/3;
	int dx = centerx - (r->right - r->left)/2 - r->left;
	int dy = centery - (r->bottom - r->top)/2 - r->top;
	r->left += dx;
	r->right += dx;
	r->top += dy;
	r->bottom += dy;
}


/*
 * Convert a pascal string in place
 *
 * This function may be defined elsewhere, but since it is so
 * small, it is not worth finding the proper function name for
 * all the different platforms.
 */
static void ptocstr(StringPtr src)
{
	int i;

	/* Hack -- pointer */
	char *s = (char*)(src);

	/* Hack -- convert the string */
	for (i = s[0]; i; i--, s++) s[0] = s[1];

	/* Hack -- terminate the string */
	s[0] = '\0';
}


#ifdef USE_SFL_CODE


/*
 * The following three routines (pstrcat, pstrinsert, and PathNameFromDirID)
 * were taken from the Think Reference section called "Getting a Full Pathname"
 * (under the File Manager section).  We need PathNameFromDirID to get the
 * full pathname of the opened savefile, making no assumptions about where it
 * is.
 *
 * I had to hack PathNameFromDirID a little for MetroWerks, but it's awfully
 * nice.
 */
static void pstrcat(StringPtr dst, StringPtr src)
{
	/* copy string in */
	BlockMove(src + 1, dst + *dst + 1, *src);

	/* adjust length byte */
	*dst += *src;
}

/*
 * pstrinsert - insert string 'src' at beginning of string 'dst'
 */
static void pstrinsert(StringPtr dst, StringPtr src)
{
	/* make room for new string */
	BlockMove(dst + 1, dst + *src + 1, *dst);

	/* copy new string in */
	BlockMove(src + 1, dst + 1, *src);

	/* adjust length byte */
	*dst += *src;
}

static void PathNameFromDirID(long dirID, short vRefNum, StringPtr fullPathName)
{
	CInfoPBRec block;
	Str255 directoryName;
	OSErr err;

	fullPathName[0] = '\0';

	block.dirInfo.ioDrParID = dirID;
	block.dirInfo.ioNamePtr = directoryName;

	while (1)
	{
		block.dirInfo.ioVRefNum = vRefNum;
		block.dirInfo.ioFDirIndex = -1;
		block.dirInfo.ioDrDirID = block.dirInfo.ioDrParID;
		err = PBGetCatInfoSync(&block);
		pstrcat(directoryName, (StringPtr)"\p:");
		pstrinsert(fullPathName, directoryName);
		if (block.dirInfo.ioDrDirID == 2) break;
	}
}

#endif /* USE_SFL_CODE */



/*
 * Activate a given window, if necessary
 */
static void activate(WindowPtr w)
{
	/* Activate */
	if (active != w)
	{
		/* Activate */
		if (w) SetPort(GetWindowPort(w));

		/* Remember */
		active = w;
	}
}


/*
 * Display a warning message
 */
static void mac_warning(cptr warning)
{
	Str255 text;
	int len, i;

	/* Limit of 250 chars */
	len = strlen(warning);
	if (len > 250) len = 250;

	/* Make a "Pascal" string */
	text[0] = len;
	for (i=0; i<len; i++) text[i+1] = warning[i];

	/* Prepare the dialog box values */
	ParamText(text, "\p", "\p", "\p");

	/* Display the Alert, wait for Okay */
	Alert(129, 0L);
}



/*** Some generic functions ***/

/*
 * Hack -- activate a color (0 to 255)
 */
static void term_data_color(term_data *td, int a)
{
	/* Activate the color */
	if (td->last != a)
	{
		/* Activate the color */
		RGBForeColor(&color_info[a]);

		/* Memorize color */
		td->last = a;
	}
}


/*
 * Hack -- Apply and Verify the "font" info
 *
 * This should usually be followed by "term_data_check_size()"
 */
static void term_data_check_font(term_data *td)
{
	int i;

	FontInfo info;

	WindowPtr old = active;


	/* Activate */
	activate(td->w);

	/* Instantiate font */
	TextFont(td->font_id);
	TextSize(td->font_size);
	TextFace(td->font_face);

	/* Extract the font info */
	GetFontInfo(&info);

	/* Assume monospaced */
	td->font_mono = TRUE;

	/* Extract the font sizing values XXX XXX XXX */
	td->font_wid = CharWidth('@'); /* info.widMax; */
	td->font_hgt = info.ascent + info.descent;
	td->font_o_x = 0;
	td->font_o_y = info.ascent;

	/* Check important characters */
	for (i = 33; i < 127; i++)
	{
		/* Hack -- notice non-mono-space */
		if (td->font_wid != CharWidth(i)) td->font_mono = FALSE;

		/* Hack -- collect largest width */
		if (td->font_wid < CharWidth(i)) td->font_wid = CharWidth(i);
	}

	/* Set default offsets */
	td->tile_o_x = td->font_o_x;
	td->tile_o_y = td->font_o_y;

	/* Set default tile size */
	td->tile_wid = td->font_wid;
	td->tile_hgt = td->font_hgt;

	/* Re-activate the old window */
	activate(old);
}


/*
 * Hack -- Apply and Verify the "size" info
 */
static void term_data_check_size(term_data *td)
{
	if (td == &data[0])
	{
#ifndef ALLOW_BIG_SCREEN

		/* Forbid resizing of the Angband window */
		td->cols = 80;
		td->rows = 24;

#else

		/* Enforce minimal size */
		if (td->cols < 80) td->cols = 80;
		if (td->rows < 24) td->rows = 24;

#endif /* !ALLOW_BIG_SCREEN */
	}

	/* Information windows can be much smaller */
	else
	{
		if (td->cols < 1) td->cols = 1;
		if (td->rows < 1) td->rows = 1;
	}

	/* Minimal tile size */
	if (td->tile_wid < 4) td->tile_wid = 4;
	if (td->tile_hgt < 4) td->tile_hgt = 4;

	/* Default tile offsets */
	td->tile_o_x = (td->tile_wid - td->font_wid) / 2;
	td->tile_o_y = (td->tile_hgt - td->font_hgt) / 2;

	/* Minimal tile offsets */
	if (td->tile_o_x < 0) td->tile_o_x = 0;
	if (td->tile_o_y < 0) td->tile_o_y = 0;

	/* Apply font offsets */
	td->tile_o_x += td->font_o_x;
	td->tile_o_y += td->font_o_y;

	/* Calculate full window size */
	td->size_wid = td->cols * td->tile_wid + td->size_ow1 + td->size_ow2;
	td->size_hgt = td->rows * td->tile_hgt + td->size_oh1 + td->size_oh2;

	{
		BitMap tScreen;

		/* Get current screen */
		(void)GetQDGlobalsScreenBits(&tScreen);

		/* Verify the top */
		if (td->r.top > tScreen.bounds.bottom - td->size_hgt)
		{
			td->r.top = tScreen.bounds.bottom - td->size_hgt;
		}
			
		/* Verify the top */
		if (td->r.top < tScreen.bounds.top + GetMBarHeight())
		{
			td->r.top = tScreen.bounds.top + GetMBarHeight();
		}

		/* Verify the left */
		if (td->r.left > tScreen.bounds.right - td->size_wid)
		{
			td->r.left = tScreen.bounds.right - td->size_wid;
		}

		/* Verify the left */
		if (td->r.left < tScreen.bounds.left)
		{
			td->r.left = tScreen.bounds.left;
		}
	}

	/* Calculate bottom right corner */
	td->r.right = td->r.left + td->size_wid;
	td->r.bottom = td->r.top + td->size_hgt;

	/* Assume no graphics */
	td->t->always_pict = FALSE;


	/* Handle graphics */
	if (use_graphics && ((td == &data[0]) || (td == &data[6])))
	{
		td->t->always_pict = TRUE;
	}

	/* Fake mono-space */
	if (!td->font_mono ||
	    (td->font_wid != td->tile_wid) ||
	    (td->font_hgt != td->tile_hgt))
	{
		/* Handle fake monospace */
		td->t->always_pict = TRUE;
	}
}


/*
 * Hack -- resize a term_data
 *
 * This should normally be followed by "term_data_redraw()"
 */
static void term_data_resize(term_data *td)
{
	/*
	 * Actually resize the window
	 * 
	 * ResizeWindow is the preferred API call, but it cannot
	 * be used here.
	 */
	SizeWindow(td->w, td->size_wid, td->size_hgt, 0);
}



/*
 * Hack -- redraw a term_data
 *
 * Note that "Term_redraw()" calls "TERM_XTRA_CLEAR"
 */
static void term_data_redraw(term_data *td)
{
	term *old = Term;
	Rect tRect;

	/* Activate the term */
	Term_activate(td->t);

	/* Redraw the contents */
	Term_redraw();

	/* Flush the output */
	Term_fresh();

	/* Restore the old term */
	Term_activate(old);

	/* No need to redraw */
	ValidWindowRect(td->w, GetPortBounds(GetWindowPort(td->w), &tRect));
}


/*
 * Constants
 */

static int pictID = 1001;	/* 8x8 tiles; 16x16 tiles are 1002 */
static int maskID = 1001;	/* 8x8 tiles; 16x16 tiles are 1003 */

static int grafWidth = 8;	/* Always equal to grafHeight */
static int grafHeight = 8;	/* Either 8 or 16 */

static int pictCols;		/* Number of columns in tiles */
static int pictRows;		/* Number of rows in tiles */

#define kMaxChannels 10

static bool arg_transparency;	/* "Fake" arg for tile support */
#ifdef NO_USE_TRANSPARENCY_VAR
static bool use_transparency;	
#endif /* NO_USE_TRANSPARENCY_VAR */

/*
 * Forward Declare
 */
typedef struct FrameRec FrameRec;

/*
 * Frame
 *
 *	- GWorld for the frame image
 *	- Handle to pix map (saved for unlocking/locking)
 *	- Pointer to color pix map (valid only while locked)
 */
struct FrameRec
{
	GWorldPtr 		framePort;
	PixMapHandle 	framePixHndl;
	PixMapPtr 		framePix;

	GWorldPtr		maskPort;
	PixMapHandle	maskPixHndl;
	PixMapPtr		maskPix;
};


/*
 * The global picture data
 */
static FrameRec *frameP = NULL;


/*
 * Lock a frame
 */
static void BenSWLockFrame(FrameRec *srcFrameP)
{
	PixMapHandle pixMapH;

	pixMapH = GetGWorldPixMap(srcFrameP->framePort);
	(void)LockPixels(pixMapH);
	HLockHi((Handle)pixMapH);
	srcFrameP->framePixHndl = pixMapH;
	srcFrameP->framePix = (PixMapPtr)(*(Handle)pixMapH);

	pixMapH = GetGWorldPixMap(srcFrameP->maskPort);
	(void)LockPixels(pixMapH);
	HLockHi((Handle)pixMapH);
	srcFrameP->maskPixHndl = pixMapH;
	srcFrameP->maskPix = (PixMapPtr)(*(Handle)pixMapH);
}


/*
 * Unlock a frame
 */
static void BenSWUnlockFrame(FrameRec *srcFrameP)
{
	if (srcFrameP->framePort != NULL)
	{
		HUnlock((Handle)srcFrameP->framePixHndl);
		UnlockPixels(srcFrameP->framePixHndl);
	}

	srcFrameP->framePix = NULL;

	if (srcFrameP->maskPort != NULL)
	{
		HUnlock((Handle)srcFrameP->maskPixHndl);
		UnlockPixels(srcFrameP->maskPixHndl);
	}

	srcFrameP->maskPix = NULL;
}



static OSErr BenSWCreateGWorldFromPict(
	GWorldPtr *pictGWorld,
	GWorldPtr *maskGWorld,
	PicHandle pictH,
	PicHandle maskH)
{
	OSErr err;
	GWorldPtr saveGWorld;
	GDHandle saveGDevice;
	GWorldPtr tempGWorld;
	Rect pictRect;
	short depth;
	GDHandle theGDH;

	{
		tempGWorld = NULL;

		/* Reset */
		*pictGWorld = NULL;

		/* Get depth */
		depth = data[0].pixelDepth;

		/* Get GDH */
		theGDH = data[0].theGDH;

		/* Obtain size rectangle */
		pictRect = (**pictH).picFrame;
		OffsetRect(&pictRect, -pictRect.left, -pictRect.top);

		/* Calculate and set numbers of rows and columns */
		pictRows = pictRect.bottom / grafHeight;
		pictCols = pictRect.right / grafWidth;

		/* Create a GWorld */
		err = NewGWorld(&tempGWorld, depth, &pictRect, nil,
						theGDH, noNewDevice);

		/* Success */
		if (err != noErr)
		{
			return (err);
		}

		/* Save pointer */
		*pictGWorld = tempGWorld;

		/* Save GWorld */
		GetGWorld(&saveGWorld, &saveGDevice);

		/* Activate */
		SetGWorld(tempGWorld, nil);

		/* Dump the pict into the GWorld */
		(void)LockPixels(GetGWorldPixMap(tempGWorld));
		EraseRect(&pictRect);
		DrawPicture(pictH, &pictRect);
		UnlockPixels(GetGWorldPixMap(tempGWorld));

		/* Restore GWorld */
		SetGWorld(saveGWorld, saveGDevice);
	}

	{
		tempGWorld = NULL;

		/* Reset */
		*maskGWorld = NULL;

		/* Get depth */
		depth = data[0].pixelDepth;

		/* Get GDH */
		theGDH = data[0].theGDH;

		/* Obtain size rectangle */
		pictRect = (**maskH).picFrame;
		OffsetRect(&pictRect, -pictRect.left, -pictRect.top);

		/* Create a GWorld */
		err = NewGWorld(&tempGWorld, depth, &pictRect, nil,
						theGDH, noNewDevice);

		/* Success */
		if (err != noErr)
		{
			return (err);
		}

		/* Save pointer */
		*maskGWorld = tempGWorld;

		/* Save GWorld */
		GetGWorld(&saveGWorld, &saveGDevice);

		/* Activate */
		SetGWorld(tempGWorld, nil);

		/* Dump the mask into the GWorld */
		(void)LockPixels(GetGWorldPixMap(tempGWorld));
		EraseRect(&pictRect);
		DrawPicture(maskH, &pictRect);
		UnlockPixels(GetGWorldPixMap(tempGWorld));

		/* Restore GWorld */
		SetGWorld(saveGWorld, saveGDevice);
	}

	/* Success */
	return (0);
}


/*
 * Init the global "frameP"
 */
static errr globe_init(void)
{
	OSErr err;

	GWorldPtr tempPictGWorldP;
	GWorldPtr tempPictMaskGWorldP;

	PicHandle newPictH;
	PicHandle newMaskH;


	/* Use window XXX XXX XXX */
	SetPort(GetWindowPort(data[0].w));


	/* Get the pict resource */
	newPictH = GetPicture(pictID);
	newMaskH = GetPicture(maskID);

	/* Analyze result */
	err = (newPictH ? 0 : -1) || (newMaskH ? 0 : -1);

	/* Oops */
	if (err == noErr)
	{
		/* Create GWorld */
		err = BenSWCreateGWorldFromPict(&tempPictGWorldP,
		                                &tempPictMaskGWorldP,
		                                newPictH,
		                                newMaskH);

		/* Release resource */
		ReleaseResource((Handle)newPictH);
		ReleaseResource((Handle)newMaskH);

		/* Error */
		if (err == noErr)
		{
			/* Create the frame */
			frameP = (FrameRec*)NewPtrClear((Size)sizeof(FrameRec));

			/* Analyze result */
			err = (frameP ? 0 : -1);

			/* Oops */
			if (err == noErr)
			{
				/* Save GWorld */
				frameP->framePort = tempPictGWorldP;
				frameP->maskPort = tempPictMaskGWorldP;

				/* Lock it */
				BenSWLockFrame(frameP);
			}
		}
	}

	/* Result */
	return (err);
}


/*
 * Nuke the global "frameP"
 */
static errr globe_nuke(void)
{
	/* Dispose */
	if (frameP)
	{
		/* Unlock */
		BenSWUnlockFrame(frameP);

		/* Dispose of the GWorld */
		DisposeGWorld(frameP->framePort);
		DisposeGWorld(frameP->maskPort);

		/* Dispose of the memory */
		DisposePtr((Ptr)frameP);

		/* Forget */
		frameP = NULL;
	}

	/* Flush events */
	FlushEvents(everyEvent, 0);

	/* Success */
	return (0);
}


/*** Support for the "z-term.c" package ***/


/*
 * Initialize a new Term
 *
 * Note also the "window type" called "noGrowDocProc", which might be more
 * appropriate for the main "screen" window.
 *
 * Note the use of "srcCopy" mode for optimized screen writes.
 */
static void Term_init_mac(term *t)
{
	term_data *td = (term_data*)(t->data);
	WindowAttributes wattrs;
	OSStatus err;

	static RGBColor black = {0x0000,0x0000,0x0000};
	static RGBColor white = {0xFFFF,0xFFFF,0xFFFF};

#ifndef ALLOW_BIG_SCREEN

	/* Every window has close and collapse boxes */
	wattrs = kWindowCloseBoxAttribute | kWindowCollapseBoxAttribute;

	/* Information windows are resizable */
	if (td != &data[0]) wattrs |= kWindowResizableAttribute;

#else

	/* Big screen - every window has close, collapse and resize boxes */
	wattrs = kWindowCloseBoxAttribute |
		kWindowCollapseBoxAttribute |
		kWindowResizableAttribute;

#endif /* !ALLOW_BIG_SCREEN */

	/* Make the window  */
	err = CreateNewWindow(
			kDocumentWindowClass,
			wattrs,
			&td->r,
			&td->w);

	/* 
	 * XXX XXX Although the original main-mac.c doesn't perform error
	 * checking, it should be done here.
	 */
	
	/* Set window title */
	SetWTitle(td->w, td->title);

	/* Activate the window */
	activate(td->w);

	/* Erase behind words */
	TextMode(srcCopy);

	/* Apply and Verify */
	term_data_check_font(td);
	term_data_check_size(td);

	/* Resize the window */
	term_data_resize(td);


	/* Prepare the colors (real colors) */
	RGBBackColor(&black);
	RGBForeColor(&white);

	/* Block */
	{
		Rect globalRect;
		GDHandle mainGDH;
		GDHandle currentGDH;
		GWorldPtr windowGWorld;
		PixMapHandle basePixMap;

		/* Obtain the global rect */
		GetWindowBounds((WindowRef)td->w, kWindowContentRgn, &globalRect);

		/* Obtain the proper GDH */
		mainGDH = GetMaxDevice(&globalRect);

		/* Extract GWorld and GDH */
		GetGWorld(&windowGWorld, &currentGDH);

		/* Obtain base pixmap */
		basePixMap = (**mainGDH).gdPMap;

		/* Save pixel depth */
		td->pixelDepth = (**basePixMap).pixelSize;

		/* Save Window GWorld - unused */
		td->theGWorld = windowGWorld;

		/* Save Window GDH */
		td->theGDH = currentGDH;

		/* Save main GDH - unused */
		td->mainSWGDH = mainGDH;
	}

	{
		Rect portRect;

		/* Get current Rect */
		GetPortBounds(GetWindowPort(td->w), &portRect);

		/* Clip to the window */
		ClipRect(&portRect);

		/* Erase the window */
		EraseRect(&portRect);

		/* Invalidate the window */
		InvalWindowRect(td->w, &portRect);
	}

	/*
	 * A certain release of OS X fails to display windows at proper
	 * locations (_ _#)
	 */
	if (mac_os_version >= 0x1000)
	{
		/* Hack - Make sure the window is displayed at (r.left,r.top) */
		MoveWindow(td->w, td->r.left, td->r.top, 1);
	}

	/* Display the window if needed */
	if (td->mapped) TransitionWindow(td->w,
						kWindowZoomTransitionEffect,
						kWindowShowTransitionAction,
						NULL);

	/* Hack -- set "mapped" flag */
	t->mapped_flag = td->mapped;

	/* Forget color */
	td->last = -1;
}



/*
 * Nuke an old Term
 */
static void Term_nuke_mac(term *t)
{

#pragma unused (t)

	/* XXX */
}



/*
 * Unused
 */
static errr Term_user_mac(int n)
{

#pragma unused (n)

	/* Success */
	return (0);
}



/*
 * React to changes
 */
static errr Term_xtra_mac_react(void)
{
	term_data *td = (term_data*)(Term->data);

	int i;


	/* Reset color */
	td->last = -1;

	/* Update colors */
	for (i = 0; i < 256; i++)
	{
		u16b rv, gv, bv;

		/* Extract the R,G,B data */
		rv = angband_color_table[i][1];
		gv = angband_color_table[i][2];
		bv = angband_color_table[i][3];

		/* Save the actual color */
		color_info[i].red = (rv | (rv << 8));
		color_info[i].green = (gv | (gv << 8));
		color_info[i].blue = (bv | (bv << 8));
	}

	/* Handle sound */
	if (use_sound != arg_sound)
	{
		/* Apply request */
		use_sound = arg_sound;
	}

	/* Handle transparency */
	if (((td == &data[0]) || (td == &data[6])) && (use_transparency != arg_transparency))
	{
		globe_nuke();

		if (globe_init() != 0)
		{
			plog("Cannot initialize graphics!");
			arg_graphics = FALSE;
			arg_transparency = FALSE;
		}

		/* Apply request */
		use_transparency = arg_transparency;

		/* Apply and Verify */
		term_data_check_size(td);

		/* Resize the window */
		term_data_resize(td);

		/* Reset visuals */
#ifndef ANG281_RESET_VISUALS
		reset_visuals(TRUE);
#else
		reset_visuals();
#endif /* !ANG281_RESET_VISUALS */
	}

	/* Handle graphics */
	if (((td == &data[0]) || (td == &data[6])) && (use_graphics != arg_graphics))
	{
		/* Initialize graphics */
		if (!use_graphics && !frameP && (globe_init() != 0))
		{
			plog("Cannot initialize graphics!");
			arg_graphics = FALSE;
		}

		/* Apply request */
		use_graphics = arg_graphics;

		/* Apply and Verify */
		term_data_check_size(td);

		/* Resize the window */
		term_data_resize(td);

		/* Reset visuals */
#ifndef ANG281_RESET_VISUALS
		reset_visuals(TRUE);
#else
		reset_visuals();
#endif /* !ANG281_RESET_VISUALS */
	}

	/* Success */
	return (0);
}


/*
 * Do a "special thing"
 */
static errr Term_xtra_mac(int n, int v)
{
	term_data *td = (term_data*)(Term->data);

	Rect r;

	/* Analyze */
	switch (n)
	{
		/* Make a noise */
		case TERM_XTRA_NOISE:
		{
			/* Make a noise */
			SysBeep(1);

			/* Success */
			return (0);
		}

		/* Make a sound */
		case TERM_XTRA_SOUND:
		{
			Handle handle;
			Str255 sound;
			
			static SndChannelPtr mySndChannel[kMaxChannels];
			static bool channelInit = FALSE;
			SCStatus status;
			int chan;


			/* Get the proper sound name */
			sprintf((char*)sound + 1, "%.16s.wav", angband_sound_name[v]);
			sound[0] = strlen((char*)sound + 1);

			/* Obtain resource XXX XXX XXX */
			handle = GetNamedResource('snd ', sound);

			/* Oops -- it is a failure, but we return 0 anyway */
			if (handle == nil) return (0);

			/* Load and Lock */
			LoadResource(handle);
			HLock(handle);

#if 0 /* The old code - synchronous player */

			/* Play sound (wait for completion) */
			SndPlay(nil, (SndListHandle)handle, false);

#else /* Asynchronous sound player */

			/* Initialise sound channels if they aren't */
			if (!channelInit)
			{
				for (chan = 0; chan < kMaxChannels; chan++)
				{
					/* Create sound channel for all sounds to play from */
					SndNewChannel(
						&mySndChannel[chan],
						sampledSynth,
						initMono,
						0L);
				}
				channelInit = TRUE;
			}

			for (chan = 0; chan < kMaxChannels; chan++)
			{
				OSErr myErr;

				/* See if we can use this channel */
				myErr = SndChannelStatus(
					mySndChannel[chan],
					sizeof(SCStatus),
					&status);

				/* Not available, try next one */
				if (myErr != noErr || status.scChannelBusy) continue;

				/* Play new sound asynchronously */
				SndPlay(mySndChannel[chan], (SndListHandle)handle, true);
					
				/* Done */
				break;
			}

#endif /* Synch and Async */

			/* Unlock and release */
			HUnlock(handle);
			ReleaseResource(handle);

			/* Success */
			return (0);
		}

		/* Process random events */
		case TERM_XTRA_BORED:
		{
			/* Process an event */
			(void)CheckEvents(FALSE);

			/* Success */
			return (0);
		}

		/* Process pending events */
		case TERM_XTRA_EVENT:
		{
			/* Process an event */
			(void)CheckEvents(v);

			/* Success */
			return (0);
		}

		/* Flush all pending events (if any) */
		case TERM_XTRA_FLUSH:
		{
			/* Hack -- flush all events */
			while (CheckEvents(FALSE)) /* loop */;

			/* Success */
			return (0);
		}

		/* Hack -- Change the "soft level" */
		case TERM_XTRA_LEVEL:
		{
			/* Activate if requested */
			if (v) activate(td->w);

			/* Success */
			return (0);
		}

		/* Clear the screen */
		case TERM_XTRA_CLEAR:
		{
			Rect portRect;

			/* Get current Rect */
			GetPortBounds(GetWindowPort(td->w), &portRect);

			/* No clipping XXX XXX XXX */
			ClipRect(&portRect);

			/* Erase the window */
			EraseRect(&portRect);

			/* Set the color */
			term_data_color(td, TERM_WHITE);

			/* Frame the window in white */
			MoveTo(0, 0);
			LineTo(0, td->size_hgt-1);
			LineTo(td->size_wid-1, td->size_hgt-1);
			LineTo(td->size_wid-1, 0);

			/* Clip to the new size */
			r.left = portRect.left + td->size_ow1;
			r.top = portRect.top + td->size_oh1;
			r.right = portRect.right - td->size_ow2;
			r.bottom = portRect.bottom - td->size_oh2;
			ClipRect(&r);

			/* Success */
			return (0);
		}

		/* React to changes */
		case TERM_XTRA_REACT:
		{
			/* React to changes */
			return (Term_xtra_mac_react());
		}

		/* Delay (milliseconds) */
		case TERM_XTRA_DELAY:
		{
			/*
			 * WaitNextEvent relinquishes CPU as well as
			 * induces a screen refresh on OS X
			 */

			/* If needed */
			if (v > 0)
			{
				EventRecord tmp;
				UInt32 ticks;

				/* Convert millisecs to ticks */
				ticks = (v * 60L) / 1000;

				/*
				 * Put the programme into sleep.
				 * No events match ~everyEvent, so nothing
				 * should be lost in Angband's event queue.
				 * Even if ticks are 0, it's worth calling for
				 * the above mentioned reasons.
				 */
				WaitNextEvent(~everyEvent, &tmp, ticks, nil);
			}

			/* Success */
			return (0);
		}
	}

	/* Oops */
	return (1);
}



/*
 * Low level graphics (Assumes valid input).
 * Draw a "cursor" at (x,y), using a "yellow box".
 * We are allowed to use "Term_what()" to determine
 * the current screen contents (for inverting, etc).
 */
static errr Term_curs_mac(int x, int y)
{
	Rect r;

	term_data *td = (term_data*)(Term->data);

	/* Set the color */
	term_data_color(td, TERM_YELLOW);

	/* Frame the grid */
	r.left = x * td->tile_wid + td->size_ow1;
	r.right = r.left + td->tile_wid;
	r.top = y * td->tile_hgt + td->size_oh1;
	r.bottom = r.top + td->tile_hgt;
	FrameRect(&r);

	/* Success */
	return (0);
}


/*
 * Low level graphics (Assumes valid input)
 *
 * Erase "n" characters starting at (x,y)
 */
static errr Term_wipe_mac(int x, int y, int n)
{
	Rect r;

	term_data *td = (term_data*)(Term->data);

	/* Erase the block of characters */
	r.left = x * td->tile_wid + td->size_ow1;
	r.right = r.left + n * td->tile_wid;
	r.top = y * td->tile_hgt + td->size_oh1;
	r.bottom = r.top + td->tile_hgt;
	EraseRect(&r);

	/* Success */
	return (0);
}


/*
 * Low level graphics.  Assumes valid input.
 *
 * Draw several ("n") chars, with an attr, at a given location.
 */
static errr Term_text_mac(int x, int y, int n, byte a, const char *cp)
{
	int xp, yp;

	term_data *td = (term_data*)(Term->data);

	/* Set the color */
	term_data_color(td, a);

	/* Starting pixel */
	xp = x * td->tile_wid + td->tile_o_x + td->size_ow1;
	yp = y * td->tile_hgt + td->tile_o_y + td->size_oh1;

	/* Move to the correct location */
	MoveTo(xp, yp);

	/* Draw the character */
	if (n == 1) DrawChar(*cp);

	/* Draw the string */
	else DrawText(cp, 0, n);

	/* Success */
	return (0);
}


/*
 * Low level graphics (Assumes valid input)
 *
 * Erase "n" characters starting at (x,y)
 */
#ifdef USE_TRANSPARENCY
static errr Term_pict_mac(int x, int y, int n, const byte *ap, const char *cp,
                          const byte *tap, const char *tcp)
#else /* USE_TRANSPARENCY */
static errr Term_pict_mac(int x, int y, int n, const byte *ap, const char *cp)
#endif /* USE_TRANSPARENCY */
{
	int i;
	Rect r2;
	term_data *td = (term_data*)(Term->data);

	/* Destination rectangle */
	r2.left = x * td->tile_wid + td->size_ow1;
	r2.right = r2.left + td->tile_wid;
	r2.top = y * td->tile_hgt + td->size_oh1;
	r2.bottom = r2.top + td->tile_hgt;

	/* Scan the input */
	for (i = 0; i < n; i++)
	{
		bool done = FALSE;

		byte a = ap[i];
		char c = cp[i];

#ifdef USE_TRANSPARENCY
		byte ta = tap[i];
		char tc = tcp[i];
#endif

		/* Graphics -- if Available and Needed */
		if (use_graphics && ((td == &data[0]) || (td == &data[6])) &&
		    ((byte)a & 0x80) && ((byte)c & 0x80))
		{
#ifdef USE_TRANSPARENCY
			int t_col, t_row;

			Rect r3;
#endif /* USE_TRANSPARENCY */

			int col, row;
			Rect r1;

			/* Row and Col */
			row = ((byte)a & 0x7F) % pictRows;
			col = ((byte)c & 0x7F) % pictCols;

			/* Source rectangle */
			r1.left = col * grafWidth;
			r1.top = row * grafHeight;
			r1.right = r1.left + grafWidth;
			r1.bottom = r1.top + grafHeight;

#ifdef USE_TRANSPARENCY
			/* Row and Col */
			t_row = ((byte)ta & 0x7F) % pictRows;
			t_col = ((byte)tc & 0x7F) % pictCols;

			/* Source rectangle */
			r3.left = t_col * grafWidth;
			r3.top = t_row * grafHeight;
			r3.right = r3.left + grafWidth;
			r3.bottom = r3.top + grafHeight;
#endif /* USE_TRANSPARENCY */

			/* Hardwire CopyBits */
			BackColor(whiteColor);
			ForeColor(blackColor);

			/*
			 * OS X requires locking and unlocking of window port
			 * when we draw directly to its pixmap.
			 * The Lock/Unlock protocol is described in the Carbon
			 * Porting Guide.
			 */
			{
				GrafPtr tPort;
				PixMapHandle tPixMapH;
				
				/* Obtain current window's graphic port */
				tPort = GetWindowPort(td->w);

				/* Lock pixels, so we can use handle safely */
				LockPortBits(tPort);

				/* Get Pixmap handle */
				tPixMapH = GetPortPixMap(tPort);
				
#ifdef USE_TRANSPARENCY

				/* Draw the picture */
				CopyBits((BitMap*)frameP->framePix,
					 (BitMap*)*tPixMapH,
					 &r3, &r2, srcCopy, NULL);
				CopyMask((BitMap*)frameP->framePix,
					(BitMap*)frameP->maskPix,
					(BitMap*)*tPixMapH,
					&r1, &r1, &r2);

#else /* USE_TRANSPARENCY */

				/* Draw the picture */
				CopyBits((BitMap*)frameP->framePix,
					(BitMap*)*tPixMapH,
					&r1, &r2, srcCopy, NULL);

#endif /* USE_TRANSPARENCY */

				/* Release the lock and dispose the PixMap handle */
				UnlockPortBits(tPort);
			}

			/* Restore colors */
			BackColor(blackColor);
			ForeColor(whiteColor);

			/* Forget color */
			td->last = -1;

			/* Done */
			done = TRUE;
		}

		/* Normal */
		if (!done)
		{
			int xp, yp;

			/* Erase */
			EraseRect(&r2);

			/* Set the color */
			term_data_color(td, a);

			/* Starting pixel */
			xp = r2.left + td->tile_o_x;
			yp = r2.top + td->tile_o_y;

			/* Move to the correct location */
			MoveTo(xp, yp);

			/* Draw the character */
			DrawChar(c);
		}

		/* Advance */
		r2.left += td->tile_wid;
		r2.right += td->tile_wid;
	}

	/* Success */
	return (0);
}





/*
 * Create and initialize window number "i"
 */
static void term_data_link(int i)
{
	term *old = Term;

	term_data *td = &data[i];

	/* Only once */
	if (td->t) return;

	/* Require mapped */
	if (!td->mapped) return;

	/* Allocate */
	MAKE(td->t, term);

	/* Initialize the term */
	term_init(td->t, td->cols, td->rows, td->keys);

	/* Use a "software" cursor */
	td->t->soft_cursor = TRUE;

	/* Erase with "white space" */
	td->t->attr_blank = TERM_WHITE;
	td->t->char_blank = ' ';

	/* Prepare the init/nuke hooks */
	td->t->init_hook = Term_init_mac;
	td->t->nuke_hook = Term_nuke_mac;

	/* Prepare the function hooks */
	td->t->user_hook = Term_user_mac;
	td->t->xtra_hook = Term_xtra_mac;
	td->t->wipe_hook = Term_wipe_mac;
	td->t->curs_hook = Term_curs_mac;
	td->t->text_hook = Term_text_mac;
	td->t->pict_hook = Term_pict_mac;

#if 1

	/* Doesn't make big difference? */
	td->t->never_bored = TRUE;

#endif

	/* Link the local structure */
	td->t->data = (vptr)(td);

	/* Activate it */
	Term_activate(td->t);

	/* Global pointer */
	angband_term[i] = td->t;

	/* Activate old */
	Term_activate(old);
}




/*
 * Set the "current working directory" (also known as the "default"
 * volume/directory) to the location of the current application.
 *
 * Original code by: Maarten Hazewinkel (mmhazewi@cs.ruu.nl)
 *
 * Completely rewritten to use Carbon Process Manager.
 */
static void SetupAppDir(void)
{
	OSErr err;
	ProcessSerialNumber curPSN;
	ProcessInfoRec procInfo;
	FSSpec cwdSpec;

	/* Initialise PSN info for the current process */
	curPSN.highLongOfPSN = 0;
	curPSN.lowLongOfPSN = kCurrentProcess;

	/* Fill in mandatory fields */
	procInfo.processInfoLength = sizeof(ProcessInfoRec);
	procInfo.processName = nil;
	procInfo.processAppSpec = &cwdSpec;

	/* Obtain current process information */
	err = GetProcessInformation(&curPSN, &procInfo);

	/* Oops */
	if (err != noErr)
	{
		mac_warning("Unable to get process information");

		/* Quit without writing anything */
		ExitToShell();
	}

	/* Extract and save the Vol and Dir */
	app_vol = cwdSpec.vRefNum;
	app_dir = cwdSpec.parID;
}




/*
 * Using Core Foundation's Preferences services 
 *
 * Requires OS 8.6 or greater with CarbonLib 1.1 or greater. Or OS X,
 * of course.
 *
 * Without this, we can support older versions of OS 8 as well
 * (with CarbonLib 1.0.4).
 *
 * pelpel: Any shortcomings of this section are mine, because I wrote this
 * from scratch.
 *
 * Frequent allocation/deallocation of small chunks of data is
 * far from my liking, but since this is only called at the
 * beginning and the end of a session, I hope this hardly matters.
 *
 * Specification of Core Foundation does not seem stable,
 * so is its implementation. Therefore we use wrapper
 * functions and restrict ourselves to use small number of
 * functionalities here. This also saves us from Carbon
 * API's ultra-verbosity. Carbon-finger?
 */



/*
 * Store "value" as the value for preferences item name
 * pointed by key
 */
static void save_pref_short(const char *key, short value)
{
	CFStringRef cf_key;
	CFNumberRef cf_value;

	/* allocate and initialise the key. 0 stands for good ol' ASCII */
	cf_key = CFStringCreateWithCString(NULL, key, 0);

	/* allocate and initialise the value */
	cf_value = CFNumberCreate(NULL, kCFNumberShortType, &value);

	if ((cf_key != NULL) && (cf_value != NULL))
	{
		/* Store the key-value pair in the applications preferences */
		CFPreferencesSetAppValue(
			cf_key,
			cf_value,
			kCFPreferencesCurrentApplication);
	}

	/*
	 * Free CF data - the reverse order is a vain attempt to
	 * minimise memory fragmentation.
	 */
	if (cf_value) CFRelease(cf_value);
	if (cf_key) CFRelease(cf_key);
}


/*
 * Load preference value for key, returns TRUE if it succeeds with
 * vptr updated appropriately, FALSE otherwise.
 */
static bool query_load_pref_short(const char *key, short *vptr)
{
	CFStringRef cf_key;
	CFNumberRef cf_value;

	/* allocate and initialise the key */
	cf_key = CFStringCreateWithCString(NULL, key, 0);

	/* Oops */
	if (cf_key == NULL) return (FALSE);

	/* Retrieve value for the key */
	cf_value = CFPreferencesCopyAppValue(
		cf_key,
		kCFPreferencesCurrentApplication);

	/* Value not found */
	if (cf_value == NULL)
	{
		CFRelease(cf_key);
		return (FALSE);
	}

	/* Convert the value to short */
	CFNumberGetValue(
		cf_value,
		kCFNumberShortType,
		vptr);

	/* Free CF data */
	CFRelease(cf_value);
	CFRelease(cf_key);

	/* Success */
	return (TRUE);
}


/*
 * Update short data pointed by vptr only if preferences
 * value for key is located.
 */
static void load_pref_short(const char *key, short *vptr)
{
	short tmp;

	if (query_load_pref_short(key, &tmp)) *vptr = tmp;
	return;
}


/*
 * Save preferences to preferences file for current host+current user+
 * current application.
 */
static void cf_save_prefs()
{
	int i;

	save_pref_short("version.major", VERSION_MAJOR);
	save_pref_short("version.minor", VERSION_MINOR);
	save_pref_short("version.patch", VERSION_PATCH);
	save_pref_short("version.extra", VERSION_EXTRA);

	/* Windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		term_data *td = &data[i];

		save_pref_short(format("term%d.mapped", i), td->mapped);

		save_pref_short(format("term%d.font_id", i), td->font_id);
		save_pref_short(format("term%d.font_size", i), td->font_size);
		save_pref_short(format("term%d.font_face", i), td->font_face);

		save_pref_short(format("term%d.cols", i), td->cols);
		save_pref_short(format("term%d.rows", i), td->rows);
		save_pref_short(format("term%d.left", i), td->r.left);
		save_pref_short(format("term%d.top", i), td->r.top);
	}

	/*
	 * Make sure preferences are persistent
	 */
	CFPreferencesAppSynchronize(
		kCFPreferencesCurrentApplication);
}


/*
 * Load preferences from preferences file for current host+current user+
 * current application.
 */
static void cf_load_prefs()
{
	bool ok;
	short pref_major, pref_minor, pref_patch, pref_extra;
	int i;

	/* Assume nothing is wrong, yet */
	ok = TRUE;

	/* Load version information */
	ok &= query_load_pref_short("version.major", &pref_major);
	ok &= query_load_pref_short("version.minor", &pref_minor);
	ok &= query_load_pref_short("version.patch", &pref_patch);
	ok &= query_load_pref_short("version.extra", &pref_extra);

	/* Any of the above failed */
	if (!ok)
	{
		/* This may be the first run */
		mac_warning("Preferences are not found.");

		/* Ignore the rest */
		return;
	}

	/* Check version */
	if ((pref_major != VERSION_MAJOR) ||
		(pref_minor != VERSION_MINOR) ||
		(pref_patch != VERSION_PATCH) ||
		(pref_extra != VERSION_EXTRA))
	{
		/* Message */
		mac_warning(
			format("Ignoring %d.%d.%d.%d preferences.",
				pref_major, pref_minor, pref_patch, pref_extra));

		/* Ignore */
		return;
	}

	/* Windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		term_data *td = &data[i];

		load_pref_short(format("term%d.mapped", i), &td->mapped);

		load_pref_short(format("term%d.font_id", i), &td->font_id);
		load_pref_short(format("term%d.font_size", i), &td->font_size);
		load_pref_short(format("term%d.font_face", i), &td->font_face);

		load_pref_short(format("term%d.cols", i), &td->cols);
		load_pref_short(format("term%d.rows", i), &td->rows);
		load_pref_short(format("term%d.left", i), &td->r.left);
		load_pref_short(format("term%d.top", i), &td->r.top);
	}
}




/*
 * Hack -- default data for a window
 */
static void term_data_hack(term_data *td)
{
	short fid;

	/* Default to Monaco font */
	GetFNum("\pmonaco", &fid);

	/* Wipe it */
	WIPE(td, term_data);

	/* No color */
	td->last = -1;

	/* Default borders */
	td->size_ow1 = 2;
	td->size_ow2 = 2;
	td->size_oh2 = 2;

	/* Start hidden */
	td->mapped = FALSE;

	/* Default font */
	td->font_id = fid;

	/* Default font size */
	td->font_size = 12;

	/* Default font face */
	td->font_face = 0;

	/* Default size */
	td->rows = 24;
	td->cols = 80;

	/* Default position */
	td->r.left = 10;
	td->r.top = 40;

	/* Minimal keys */
	td->keys = 16;
}


/*
 * Read the preference file, Create the windows.
 *
 * We attempt to use "FindFolder()" to track down the preference file.
 */
static void init_windows(void)
{
	int i, b = 0;

	term_data *td;


	/*** Default values ***/

	/* Initialize (backwards) */
	for (i = MAX_TERM_DATA; i-- > 0; )
	{
		int n;

		cptr s;

		/* Obtain */
		td = &data[i];

		/* Defaults */
		term_data_hack(td);

		/* Obtain title */
		s = angband_term_name[i];

		/* Get length */
		n = strlen(s);

		/* Maximal length */
		if (n > 15) n = 15;

		/* Copy the title */
		strncpy((char*)(td->title) + 1, s, n);

		/* Save the length */
		td->title[0] = n;

		/* Tile the windows */
		td->r.left += (b * 30);
		td->r.top += (b * 30);

		/* Tile */
		b++;
	}


	/*** Load preferences ***/

	cf_load_prefs();


	/*** Instantiate ***/

	/* Main window */
	td = &data[0];

	/* Many keys */
	td->keys = 1024;

	/* Start visible */
	td->mapped = TRUE;

	/* Link (backwards, for stacking order) */
	for (i = MAX_TERM_DATA; i-- > 0; )
	{
		term_data_link(i);
	}

	/* Main window */
	td = &data[0];

	/* Main window */
	Term_activate(td->t);
}


/*
 * Save preferences
 */
static void save_pref_file(void)
{
	cf_save_prefs();
}



#ifdef ALLOW_QUITING

/*
 * A simple "Yes/No" filter to parse "key press" events in dialog windows
 */
static pascal Boolean ynfilter(DialogPtr dialog, EventRecord *event, short *ip)
{
	/* Parse key press events */
	if (event->what == keyDown)
	{
		int i = 0;
		char c;

		/* Extract the pressed key */
		c = (event->message & charCodeMask);

		/* Accept "no" and <return> and <enter> */
		if ((c=='n') || (c=='N') || (c==13) || (c==3)) i = 1;

		/* Accept "yes" */
		else if ((c=='y') || (c=='Y')) i = 2;

		/* Handle "yes" or "no" */
		if (i)
		{
			short type;
			ControlHandle control;
			Rect r;

			/* Get the button */
			GetDialogItem(dialog, i, &type, (Handle*)&control, &r);

			/* Blink button for 1/10 second */
			HiliteControl(control, 1);
			Term_xtra_mac(TERM_XTRA_DELAY, 100);
			HiliteControl(control, 0);

			/* Result */
			*ip = i;
			return (1);
		}
	}

	/* Ignore */
	return (0);
}

#endif /* ALLOW_QUITING */



#ifndef SAVEFILE_SCREEN

/*
 * Prepare savefile dialogue and set the variable
 * savefile accordingly. Returns true if it succeeds, false (or
 * aborts) otherwise. If all is false, only allow files whose type
 * is 'SAVE'.
 * Originally written by Peter Ammon
 */
static bool select_savefile(bool all)
{
	OSErr err;
	FSSpec theFolderSpec;
	FSSpec savedGameSpec;
	NavDialogOptions dialogOptions;
	NavReplyRecord reply;
	/* Used only when 'all' is true */
	NavTypeList types = {ANGBAND_CREATOR, 1, 1, 'SAVE'};
	NavTypeListHandle myTypeList;
	AEDesc defaultLocation;

	/* Find :lib:save: folder */
	err = FSMakeFSSpec(
		app_vol,
		app_dir,
		"\p:lib:save:",
		&theFolderSpec);

	/* Oops */
	if (err != noErr) quit("Unable to find the folder :lib:save:");

	/* Get default Navigator dialog options */
	err = NavGetDefaultDialogOptions(&dialogOptions);

	/* Clear preview option */
	dialogOptions.dialogOptionFlags &= ~kNavAllowPreviews;

	/* Disable multiple file selection */
	dialogOptions.dialogOptionFlags &= ~kNavAllowMultipleFiles;

	/* Make descriptor for default location */
	err = AECreateDesc(
		typeFSS,
		&theFolderSpec,
		sizeof(FSSpec),
		&defaultLocation);

	/* Oops */
	if (err != noErr) quit("Unable to allocate descriptor");

	/* We are indifferent to signature and file types */
	if (all)
	{
		myTypeList = (NavTypeListHandle)nil;
	}

	/* Set up type handle */
	else
	{
		err = PtrToHand(&types, &myTypeList, sizeof(NavTypeList));

		/* Oops */
		if (err != noErr) quit("Error in PtrToHand. Try enlarging heap");

	}

	/* Call NavGetFile() with the types list */
	err = NavChooseFile(
		&defaultLocation,
		&reply,
		&dialogOptions,
		nil,
		nil,
		nil,
		myTypeList,
		nil);

	/* Free type list */
	DisposeHandle(myTypeList);

	/* Invalid response -- allow the user to cancel */
	if (!reply.validRecord) return (FALSE);

	/* Retrieve FSSpec from the reply */
	if (err == noErr)
	{
		AEKeyword theKeyword;
		DescType actualType;
		Size actualSize;

		/* Get a pointer to selected file */
		(void)AEGetNthPtr(
			&reply.selection,
			1,
			typeFSS,
			&theKeyword,
			&actualType,
			&savedGameSpec,
			sizeof(FSSpec),
			&actualSize);

		/* Dispose NavReplyRecord, resources and descriptors */
		(void)NavDisposeReply(&reply);
	}

	/* Dispose location info */
	AEDisposeDesc(&defaultLocation);

	/* Convert FSSpec to pathname */
	refnum_to_name(
		savefile,
		savedGameSpec.parID,
		savedGameSpec.vRefNum,
		(char *)savedGameSpec.name);

	/* Success */
	return (TRUE);
}


/*
 * Handle menu: "File" + "New"
 */
static void do_menu_file_new(void)
{
	/* Hack */
	HiliteMenu(0);

	/* Game is in progress */
	game_in_progress = 1;

	/* Flush input */
	Term_flush();

	/* Play a game */
	play_game(TRUE);

	/* Hack -- quit */
	quit(NULL);
}

/*
 * Handle menu: "File" + "Open" /  "Import"
 */
static void do_menu_file_open(bool all)
{
	/* Let the player to choose savefile */
	if (!select_savefile(all)) return;

	/* Hack */
	HiliteMenu(0);

	/* Game is in progress */
	game_in_progress = 1;

	/* Flush input */
	flush();

	/* Play a game */
	play_game(FALSE);

	/* Hack -- quit */
	quit(NULL);
}

#endif /* !SAVEFILE_SCREEN */


/*
 * Handle the "open_when_ready" flag
 */
static void handle_open_when_ready(void)
{
	/* Check the flag XXX XXX XXX make a function for this */
	if (open_when_ready && initialized && !game_in_progress)
	{
		/* Forget */
		open_when_ready = FALSE;

		/* Game is in progress */
		game_in_progress = 1;

		/* Wait for it */
		pause_line(23);

		/* Flush input */
		flush();

#ifdef SAVEFILE_SCREEN

		/* User double-clicked savefile; no savefile screen */
		no_begin_screen = TRUE;

#endif /* SAVEFILE_SCREEN */

		/* Play a game */
		play_game(FALSE);

		/* Quit */
		quit(NULL);
	}
}



/*
 * Initialize the menus
 *
 * Verify menus 128, 129, 130
 * Create menus 131, 132, 133, 134
 *
 * The standard menus are:
 *
 *   Apple (128) =   { About, -, ... }
 *   File (129) =    { New,Open,Import,Close,Save,-,Exit,Quit }
 *     (If SAVEFILE_SCREEN is defined, this becomes)
 *   File (129) =    { Close,Save,-,Exit,Quit }
 *   Edit (130) =    { Cut, Copy, Paste, Clear }   (?)
 *   Font (131) =    { Bold, Extend, -, Monaco, ..., -, ... }
 *   Size (132) =    { ... }
 *   Window (133) =  { Angband, Term-1/Mirror, Term-2/Recall, Term-3/Choice,
 *                     Term-4, Term-5, Term-6, Term-7 }
 *   Special (134) = { arg_sound, arg_graphics, -,
 *                     arg_fiddle, arg_wizard }
 */
static void init_menubar(void)
{
	int i, n;

	Rect r;

	WindowPtr tmpw;

	MenuHandle m;


	/* Get the "apple" menu */
	m = GetMenu(128);

	/* Insert the menu */
	InsertMenu(m, 0);

	/* Get the "File" menu */
	m = GetMenu(129);

	/* Insert the menu */
	InsertMenu(m, 0);

	/* Nuke Quit in the File menu if we are running in Aqua */
	if (is_aqua)
	{
		MenuRef tempMenu;

		/* Get a handle to the file menu */
		tempMenu = GetMenuHandle(129);

		/* Nuke the quit menu since Aqua does that for us */
#ifndef SAVEFILE_SCREEN
		DeleteMenuItem(tempMenu, 8);
#else
		DeleteMenuItem(tempMenu, 5);
#endif /* !SAVEFILE_SCREEN */
	}

	/* Get the "Edit" menu */
	m = GetMenu(130);

	/* Insert the menu */
	InsertMenu(m, 0);


	/* Make the "Font" menu */
	m = NewMenu(131, "\pFont");

	/* Insert the menu */
	InsertMenu(m, 0);

	/* Add "bold" */
	AppendMenu(m, "\pBold");

	/* Add "wide" */
	AppendMenu(m, "\pWide");

	/* Add a separator */
	AppendMenu(m, "\p-");

	/* Fake window */
	r.left = r.right = r.top = r.bottom = 0;

	/* Make the fake window */
	(void)CreateNewWindow(
		kDocumentWindowClass,
		kWindowNoAttributes,
		&r,
		&tmpw);

	/* Activate the "fake" window */
	SetPort(GetWindowPort(tmpw));

	/* Default mode */
	TextMode(0);

	/* Default size */
	TextSize(12);

	/* Add the fonts to the menu */
	AppendResMenu(m, 'FONT');

	/* Size of menu */
	n = CountMenuItems(m);

	/* Scan the menu */
	for (i = n; i >= 4; i--)
	{
		Str255 tmpName;
		short fontNum;

		/* Acquire the font name */
		GetMenuItemText(m, i, tmpName);

		/* Acquire the font index */
		GetFNum(tmpName, &fontNum);

		/* Apply the font index */
		TextFont(fontNum);

		/* Remove non-mono-spaced fonts */
		if ((CharWidth('i') != CharWidth('W')) || (CharWidth('W') == 0))
		{
			/* Delete the menu item */
			DeleteMenuItem(m, i);
		}
	}

	/* Destroy the fake window */
	DisposeWindow(tmpw);

	/* Add a separator */
	AppendMenu(m, "\p-");

	/* Add the fonts to the menu */
	AppendResMenu(m, 'FONT');


	/* Make the "Size" menu */
	m = NewMenu(132, "\pSize");

	/* Insert the menu */
	InsertMenu(m, 0);

	/* Add some sizes (stagger choices) */
	for (i = 8; i <= 32; i += ((i / 16) + 1))
	{
		Str15 buf;

		/* Textual size */
		sprintf((char*)buf + 1, "%d", i);
		buf[0] = strlen((char*)buf + 1);

		/* Add the item */
		AppendMenu(m, buf);
	}


	/* Make the "Windows" menu */
	m = NewMenu(133, "\pWindows");

	/* Insert the menu */
	InsertMenu(m, 0);

	/* Default choices */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		Str15 buf;

		/* Describe the item */
		sprintf((char*)buf + 1, "%.15s", angband_term_name[i]);
		buf[0] = strlen((char*)buf + 1);

		/* Add the item */
		AppendMenu(m, buf);

		/* Command-Key shortcuts */
		if (i < 8) SetItemCmd(m, i + 1, I2D(i));
	}


	/* Make the "Special" menu */
	m = NewMenu(134, "\pSpecial");

	/* Insert the menu */
	InsertMenu(m, 0);

	/* Append the choices */
	AppendMenu(m, "\parg_sound");
	AppendMenu(m, "\parg_graphics");
	AppendMenu(m, "\p-");
	AppendMenu(m, "\parg_fiddle");
	AppendMenu(m, "\parg_wizard");
	/* Next 2 lines for AB+AK tile graphics */
	AppendMenu(m, "\p-");
	AppendMenu(m, "\parg_transparency");


	/* Make the "TileWidth" menu */
	m = NewMenu(135, "\pTileWidth");

	/* Insert the menu */
	InsertMenu(m, 0);

	/* Add some sizes */
	for (i = 4; i <= 32; i++)
	{
		Str15 buf;

		/* Textual size */
		sprintf((char*)buf + 1, "%d", i);
		buf[0] = strlen((char*)buf + 1);

		/* Append item */
		AppendMenu(m, buf);
	}


	/* Make the "TileHeight" menu */
	m = NewMenu(136, "\pTileHeight");

	/* Insert the menu */
	InsertMenu(m, 255);

	/* Add some sizes */
	for (i = 4; i <= 32; i++)
	{
		Str15 buf;

		/* Textual size */
		sprintf((char*)buf + 1, "%d", i);
		buf[0] = strlen((char*)buf + 1);

		/* Append item */
		AppendMenu(m, buf);
	}


	/* Update the menu bar */
	DrawMenuBar();
}


/*
 * Prepare the menus
 *
 * It is very important that the player not be allowed to "save" the game
 * unless the "inkey_flag" variable is set, indicating that the game is
 * waiting for a new command.  XXX XXX XXX
 */

static void setup_menus(void)
{
	int i, n;

	short value;

	Str255 s;

	MenuHandle m;

	term_data *td = NULL;


	/* Relevant "term_data" */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* Unused */
		if (!data[i].t) continue;

		/* Notice the matching window */
		if (data[i].w == FrontWindow()) td = &data[i];
	}


	/* File menu */
	m = GetMenuHandle(129);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Reset menu */
	for (i = 1; i <= n; i++)
	{
		/* Reset */
		DisableMenuItem(m, i);
		CheckMenuItem(m, i, FALSE);
	}

#ifndef SAVEFILE_SCREEN

	/* Enable "new"/"open..."/"import..." */
	if (initialized && !game_in_progress)
	{
		EnableMenuItem(m, 1);
		EnableMenuItem(m, 2);
		EnableMenuItem(m, 3);
	}

	/* Enable "close" */
	if (initialized)
	{
		EnableMenuItem(m, 4);
	}

	/* Enable "save" */
	if (initialized && character_generated && inkey_flag)
	{
		EnableMenuItem(m, 5);
	}

#ifdef ALLOW_QUITING

	/* Enable "exit" */
	if (TRUE)
	{
		EnableMenuItem(m, 7);
	}

#endif /* ALLOW_QUITING */

	/* Enable "quit" */
	if (!is_aqua)
	{
		if (!initialized || !character_generated || inkey_flag)
		{
			EnableMenuItem(m, 8);
		}
	}

#else /* In-game savefile screen */

	/* Enable "close" */
	if (initialized)
	{
		EnableMenuItem(m, 1);
	}

	/* Enable "save" */
	if (initialized && character_generated && inkey_flag)
	{
		EnableMenuItem(m, 2);
	}

#ifdef ALLOW_QUITING

	/* Enable "exit" */
	if (TRUE)
	{
		EnableMenuItem(m, 4);
	}

#endif /* ALLOW_QUITING */

	/* Enable "quit" */
	if (!is_aqua)
	{
		if (!initialized || !character_generated || inkey_flag)
		{
			EnableMenuItem(m, 5);
		}
	}	

#endif /* !SAVEFILE_SCREEN */


	/* Edit menu */
	m = GetMenuHandle(130);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Reset menu */
	for (i = 1; i <= n; i++)
	{
		/* Reset */
		DisableMenuItem(m, i);
		CheckMenuItem(m, i, FALSE);
	}

	/* Enable "edit" options if "needed" */
	if (!td)
	{
		EnableMenuItem(m, 1);
		EnableMenuItem(m, 3);
		EnableMenuItem(m, 4);
		EnableMenuItem(m, 5);
		EnableMenuItem(m, 6);
	}


	/* Font menu */
	m = GetMenuHandle(131);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Reset menu */
	for (i = 1; i <= n; i++)
	{
		/* Reset */
		DisableMenuItem(m, i);
		CheckMenuItem(m, i, FALSE);
	}

	/* Hack -- look cute XXX XXX */
	/* SetItemStyle(m, 1, bold); */

	/* Hack -- look cute XXX XXX */
	/* SetItemStyle(m, 2, extend); */

	/* Active window */
	if (initialized && td)
	{
		/* Enable "bold" */
		EnableMenuItem(m, 1);

		/* Enable "extend" */
		EnableMenuItem(m, 2);

		/* Check the appropriate "bold-ness" */
		if (td->font_face & bold) CheckMenuItem(m, 1, TRUE);

		/* Check the appropriate "wide-ness" */
		if (td->font_face & extend) CheckMenuItem(m, 2, TRUE);

		/* Analyze fonts */
		for (i = 4; i <= n; i++)
		{
			/* Enable it */
			EnableMenuItem(m, i);

			/* Analyze font */
			GetMenuItemText(m,i,s);
			GetFNum(s, &value);

			/* Check active font */
			if (td->font_id == value) CheckMenuItem(m, i, TRUE);
		}
	}


	/* Size menu */
	m = GetMenuHandle(132);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Reset menu */
	for (i = 1; i <= n; i++)
	{
		/* Reset */
		DisableMenuItem(m, i);
		CheckMenuItem(m, i, FALSE);
	}

	/* Active window */
	if (initialized && td)
	{
		/* Analyze sizes */
		for (i = 1; i <= n; i++)
		{
			/* Analyze size */
			GetMenuItemText(m,i,s);
			s[s[0]+1] = '\0';
			value = atoi((char*)(s+1));

			/* Enable the "real" sizes */
			if (RealFont(td->font_id, value)) EnableMenuItem(m, i);

			/* Check the current size */
			if (td->font_size == value) CheckMenuItem(m, i, TRUE);
		}
	}


	/* Windows menu */
	m = GetMenuHandle(133);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Check active windows */
	for (i = 1; i <= n; i++)
	{
		/* Check if needed */
		CheckMenuItem(m, i, data[i-1].mapped);
	}


	/* Special menu */
	m = GetMenuHandle(134);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Reset menu */
	for (i = 1; i <= n; i++)
	{
		/* Reset */
		DisableMenuItem(m, i);
		CheckMenuItem(m, i, FALSE);
	}

	/* Item "arg_sound" */
	EnableMenuItem(m, 1);
	CheckMenuItem(m, 1, arg_sound);

	/* Item "arg_graphics" */
	EnableMenuItem(m, 2);
	CheckMenuItem(m, 2, arg_graphics);

	/* Item "arg_fiddle" */
	EnableMenuItem(m, 4);
	CheckMenuItem(m, 4, arg_fiddle);

	/* Item "arg_wizard" */
	EnableMenuItem(m, 5);
	CheckMenuItem(m, 5, arg_wizard);

	/* Item "arg_transparency" - it's set later */
	EnableMenuItem(m, 7);
	CheckMenuItem(m, 7, use_transparency);

	/* Item "Hack" */
	/* EnableMenuItem(m, 9); */


	/* TileWidth menu */
	m = GetMenuHandle(135);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Reset menu */
	for (i = 1; i <= n; i++)
	{
		/* Reset */
		DisableMenuItem(m, i);
		CheckMenuItem(m, i, FALSE);
	}

	/* Active window */
	if (initialized && td)
	{
		/* Analyze sizes */
		for (i = 1; i <= n; i++)
		{
			/* Analyze size */
			GetMenuItemText(m,i,s);
			s[s[0]+1] = '\0';
			value = atoi((char*)(s+1));

			/* Enable */
			EnableMenuItem(m, i);

			/* Check the current size */
			if (td->tile_wid == value) CheckMenuItem(m, i, TRUE);
		}
	}


	/* TileHeight menu */
	m = GetMenuHandle(136);

	/* Get menu size */
	n = CountMenuItems(m);

	/* Reset menu */
	for (i = 1; i <= n; i++)
	{
		/* Reset */
		DisableMenuItem(m, i);
		CheckMenuItem(m, i, FALSE);
	}

	/* Active window */
	if (initialized && td)
	{
		/* Analyze sizes */
		for (i = 1; i <= n; i++)
		{
			/* Analyze size */
			GetMenuItemText(m,i,s);
			s[s[0]+1] = '\0';
			value = atoi((char*)(s+1));

			/* Enable */
			EnableMenuItem(m, i);

			/* Check the current size */
			if (td->tile_hgt == value) CheckMenuItem(m, i, TRUE);
		}
	}
}


/*
 * Process a menu selection (see above)
 *
 * Hack -- assume that invalid menu selections are disabled above,
 * which I have been informed may not be reliable.  XXX XXX XXX
 */
static void menu(long mc)
{
	int i;

	int menuid, selection;

	static unsigned char s[1000];

	short fid;

	term_data *td = NULL;

	WindowPtr old_win;


	/* Analyze the menu command */
	menuid = HiWord(mc);
	selection = LoWord(mc);


	/* Find the window */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* Skip dead windows */
		if (!data[i].t) continue;

		/* Notice matches */
		if (data[i].w == FrontWindow()) td = &data[i];
	}


	/* Branch on the menu */
	switch (menuid)
	{
		/* Apple Menu */
		case 128:
		{
			/* About Angband... */
			if (selection == 1)
			{
				DialogPtr dialog;
				short item_hit;

				dialog=GetNewDialog(128, 0, (WindowPtr)-1);

				RepositionWindow(
					GetDialogWindow(dialog),
					NULL,
					kWindowCenterOnMainScreen);

				TransitionWindow(GetDialogWindow(dialog),
					kWindowZoomTransitionEffect,
					kWindowShowTransitionAction,
					NULL);
				ModalDialog(0, &item_hit);
				DisposeDialog(dialog);
				break;
			}

			break;
		}

		/* File Menu */
		case 129:
		{
			switch (selection)
			{
#ifndef SAVEFILE_SCREEN

				/* New */
				case 1:
				{
					do_menu_file_new();
					break;
				}

				/* Open... */
				case 2:
				{
					do_menu_file_open(FALSE);
					break;
				}

				/* Import... */
				case 3:
				{
					do_menu_file_open(TRUE);
					break;
				}

				/* Close */
				case 4:
				{
					/* No window */
					if (!td) break;

					/* Not Mapped */
					td->mapped = FALSE;

					/* Not Mapped */
					td->t->mapped_flag = FALSE;

					/* Hide the window */
					TransitionWindow(td->w,
						kWindowZoomTransitionEffect,
						kWindowHideTransitionAction,
						NULL);

					break;
				}

				/* Save */
				case 5:
				{
					/* Hack -- Forget messages */
					msg_flag = FALSE;

					/* Hack -- Save the game */
#ifndef AUTO_SAVE_ARG_REQUIRED
					do_cmd_save_game();
#else
					do_cmd_save_game(FALSE);
#endif /* !AUTO_SAVE_ARG_REQUIRED */

					break;
				}

#ifdef ALLOW_QUITING

				/* Exit (without save) */
				case 7:
				{
					/* Allow user to cancel "dangerous" exit */
					if (game_in_progress && character_generated)
					{
						AlertTHndl alert;
						short item_hit;
						BitMap tempBitMap;

						/* Get the "alert" info */
						alert = (AlertTHndl)GetResource('ALRT', 130);

						/* Center the "alert" rectangle */
						center_rect(&(*alert)->boundsRect,
						            &(GetQDGlobalsScreenBits(&tempBitMap)->bounds));

						/* Display the Alert, get "No" or "Yes" */
						item_hit = Alert(130, ynfilterUPP);

						/* Require "yes" button */
						if (item_hit != 2) break;
					}

					/* Quit */
					quit(NULL);
					break;
				}
#endif /* ALLOW_QUITING */

				/* Quit (with save) */
				case 8:
				{
					/* Save the game (if necessary) */
					if (game_in_progress && character_generated)
					{
						/* Hack -- Forget messages */
						msg_flag = FALSE;

						/* Save the game */
#ifndef AUTO_SAVE_ARG_REQUIRED
						do_cmd_save_game();
#else
						do_cmd_save_game(FALSE);
#endif /* !AUTO_SAVE_ARG_REQUIRED */
					}

					/* Quit */
					quit(NULL);
					break;
				}

#else /* In-game savefile screen */

				/* Close */
				case 1:
				{
					/* No window */
					if (!td) break;

					/* Not Mapped */
					td->mapped = FALSE;

					/* Not Mapped */
					td->t->mapped_flag = FALSE;

					/* Hide the window */
					TransitionWindow(td->w,
						kWindowZoomTransitionEffect,
						kWindowHideTransitionAction,
						NULL);

					break;
				}

				/* Save */
				case 2:
				{
					/* Hack -- Forget messages */
					msg_flag = FALSE;

					/* Hack -- Save the game */
#ifndef AUTO_SAVE_ARG_REQUIRED
					do_cmd_save_game();
#else
					do_cmd_save_game(FALSE);
#endif /* !AUTO_SAVE_ARG_REQUIRED */

					break;
				}

#ifdef ALLOW_QUITING

				/* Exit (without save) */
				case 4:
				{
					/* Allow user to cancel "dangerous" exit */
					if (game_in_progress && character_generated)
					{
						AlertTHndl alert;
						short item_hit;
						BitMap tempBitMap;

						/* Get the "alert" info */
						alert = (AlertTHndl)GetResource('ALRT', 130);

						/* Center the "alert" rectangle */
						center_rect(
							&(*alert)->boundsRect,
							&(GetQDGlobalsScreenBits(&tempBitMap)->bounds));

						/* Display the Alert, get "No" or "Yes" */
						item_hit = Alert(130, ynfilterUPP);

						/* Require "yes" button */
						if (item_hit != 2) break;
					}

					/* Quit */
					quit(NULL);
					break;
				}

#endif /* ALLOW_QUITING */

				/* Quit (with save) */
				case 5:
				{
					/* Save the game (if necessary) */
					if (game_in_progress && character_generated)
					{
						/* Hack -- Forget messages */
						msg_flag = FALSE;

						/* Save the game */
#ifndef AUTO_SAVE_ARG_REQUIRED
						do_cmd_save_game();
#else
						do_cmd_save_game(FALSE);
#endif /* !AUTO_SAVE_ARG_REQUIRED */
					}

					/* Quit */
					quit(NULL);
					break;
				}

#endif /* !SAVEFILE_SCREEN */

			}
			break;
		}

		/* Edit menu */
		case 130:
		{
			/* Unused */
			break;
		}

		/* Font menu */
		case 131:
		{
			/* Require a window */
			if (!td) break;

			/* Memorize old */
			old_win = active;

			/* Activate */
			activate(td->w);

			/* Toggle the "bold" setting */
			if (selection == 1)
			{
				/* Toggle the setting */
				if (td->font_face & bold)
				{
					td->font_face &= ~bold;
				}
				else
				{
					td->font_face |= bold;
				}

				/* Apply and Verify */
				term_data_check_font(td);
				term_data_check_size(td);

				/* Resize and Redraw */
				term_data_resize(td);
				term_data_redraw(td);

				break;
			}

			/* Toggle the "wide" setting */
			if (selection == 2)
			{
				/* Toggle the setting */
				if (td->font_face & extend)
				{
					td->font_face &= ~extend;
				}
				else
				{
					td->font_face |= extend;
				}

				/* Apply and Verify */
				term_data_check_font(td);
				term_data_check_size(td);

				/* Resize and Redraw */
				term_data_resize(td);
				term_data_redraw(td);

				break;
			}

			/* Get a new font name */
			GetMenuItemText(GetMenuHandle(131), selection, s);
			GetFNum(s, &fid);

			/* Save the new font id */
			td->font_id = fid;

			/* Current size is bad for new font */
			if (!RealFont(td->font_id, td->font_size))
			{
				/* Find similar size */
				for (i = 1; i <= 32; i++)
				{
					/* Adjust smaller */
					if (td->font_size - i >= 8)
					{
						if (RealFont(td->font_id, td->font_size - i))
						{
							td->font_size -= i;
							break;
						}
					}

					/* Adjust larger */
					if (td->font_size + i <= 128)
					{
						if (RealFont(td->font_id, td->font_size + i))
						{
							td->font_size += i;
							break;
						}
					}
				}
			}

			/* Apply and Verify */
			term_data_check_font(td);
			term_data_check_size(td);

			/* Resize and Redraw */
			term_data_resize(td);
			term_data_redraw(td);

			/* Restore the window */
			activate(old_win);

			break;
		}

		/* Size menu */
		case 132:
		{
			if (!td) break;

			/* Save old */
			old_win = active;

			/* Activate */
			activate(td->w);

			GetMenuItemText(GetMenuHandle(132), selection, s);
			s[s[0]+1]=0;
			td->font_size = atoi((char*)(s+1));

			/* Apply and Verify */
			term_data_check_font(td);
			term_data_check_size(td);

			/* Resize and Redraw */
			term_data_resize(td);
			term_data_redraw(td);

			/* Restore */
			activate(old_win);

			break;
		}

		/* Window menu */
		case 133:
		{
			/* Parse */
			i = selection - 1;

			/* Check legality of choice */
			if ((i < 0) || (i >= MAX_TERM_DATA)) break;

			/* Obtain the window */
			td = &data[i];

			/* Mapped */
			td->mapped = TRUE;

			/* Link */
			term_data_link(i);

			/* Mapped (?) */
			td->t->mapped_flag = TRUE;

			/* Show the window */
			TransitionWindow(td->w,
				kWindowZoomTransitionEffect,
				kWindowShowTransitionAction,
				NULL);

			/* Bring to the front */
			SelectWindow(td->w);

			break;
		}

		/* Special menu */
		case 134:
		{
			switch (selection)
			{
				case 1:
				{
					/* Toggle arg_sound */
					arg_sound = !arg_sound;

					/* React to changes */
					Term_xtra(TERM_XTRA_REACT, 0);

					break;
				}

				case 2:
				{
					/* Toggle arg_graphics */
					arg_graphics = !arg_graphics;

					/* Hack -- Force redraw */
					Term_key_push(KTRL('R'));

					break;
				}

				case 4:
				{
					arg_fiddle = !arg_fiddle;
					break;
				}

				case 5:
				{
					arg_wizard = !arg_wizard;
					break;
				}

				case 7:
				{
					if (streq(ANGBAND_GRAF, "old"))
					{
						ANGBAND_GRAF = "new";
						arg_transparency = true;
						pictID = 1002;
						maskID = 1003;
						grafWidth = grafHeight = 16;
					}
					else
					{
						ANGBAND_GRAF = "old";
						arg_transparency = false;
						pictID = 1001;
						maskID = 1001;
						grafWidth = grafHeight = 8;
					}

					/* Hack -- Force redraw */
					Term_key_push(KTRL('R'));

					break;
				}
			}

			break;
		}

		/* TileWidth menu */
		case 135:
		{
			if (!td) break;

			/* Save old */
			old_win = active;

			/* Activate */
			activate(td->w);

			GetMenuItemText(GetMenuHandle(135), selection, s);
			s[s[0]+1]=0;
			td->tile_wid = atoi((char*)(s+1));

			/* Apply and Verify */
			term_data_check_size(td);

			/* Resize and Redraw */
			term_data_resize(td);
			term_data_redraw(td);

			/* Restore */
			activate(old_win);

			break;
		}

		/* TileHeight menu */
		case 136:
		{
			if (!td) break;

			/* Save old */
			old_win = active;

			/* Activate */
			activate(td->w);

			GetMenuItemText(GetMenuHandle(136), selection, s);
			s[s[0]+1]=0;
			td->tile_hgt = atoi((char*)(s+1));

			/* Apply and Verify */
			term_data_check_size(td);

			/* Resize and Redraw */
			term_data_resize(td);
			term_data_redraw(td);

			/* Restore */
			activate(old_win);

			break;
		}
	}


	/* Clean the menu */
	HiliteMenu(0);
}


#ifdef USE_SFL_CODE


/*
 * Check for extra required parameters -- From "Maarten Hazewinkel"
 */
static OSErr CheckRequiredAEParams(const AppleEvent *theAppleEvent)
{
	OSErr aeError;
	DescType returnedType;
	Size actualSize;

	aeError = AEGetAttributePtr(
		theAppleEvent,
		keyMissedKeywordAttr,
		typeWildCard,
		&returnedType,
		NULL,
		0,
		&actualSize);

	if (aeError == errAEDescNotFound) return (noErr);

	if (aeError == noErr) return (errAEParamMissed);

	return (aeError);
}


/*
 * Apple Event Handler -- Open Application
 */
static pascal OSErr AEH_Start(const AppleEvent *theAppleEvent,
                              const AppleEvent *reply, long handlerRefCon)
{
#pragma unused(reply, handlerRefCon)

	return (CheckRequiredAEParams(theAppleEvent));
}


/*
 * Apple Event Handler -- Quit Application
 */
static pascal OSErr AEH_Quit(const AppleEvent *theAppleEvent,
                             const AppleEvent *reply, long handlerRefCon)
{
#pragma unused(reply, handlerRefCon)

	/* Quit later */
	quit_when_ready = TRUE;

	/* Check arguments */
	return (CheckRequiredAEParams(theAppleEvent));
}


/*
 * Apple Event Handler -- Print Documents
 */
static pascal OSErr AEH_Print(const AppleEvent *theAppleEvent,
                              const AppleEvent *reply, long handlerRefCon)
{
#pragma unused(theAppleEvent, reply, handlerRefCon)

	return (errAEEventNotHandled);
}


/*
 * Apple Event Handler by Steve Linberg (slinberg@crocker.com).
 *
 * The old method of opening savefiles from the finder does not work
 * on the Power Macintosh, because CountAppFiles and GetAppFiles,
 * used to return information about the selected document files when
 * an application is launched, are part of the Segment Loader, which
 * is not present in the RISC OS due to the new memory architecture.
 *
 * The "correct" way to do this is with AppleEvents.  The following
 * code is modeled on the "Getting Files Selected from the Finder"
 * snippet from Think Reference 2.0.  (The prior sentence could read
 * "shamelessly swiped & hacked")
 */
static pascal OSErr AEH_Open(AppleEvent *theAppleEvent,
                             AppleEvent* reply, long handlerRefCon)
{
#pragma unused(reply, handlerRefCon)

	FSSpec myFSS;
	AEDescList docList;
	OSErr err;
	Size actualSize;
	AEKeyword keywd;
	DescType returnedType;
	char foo[128];
	FInfo myFileInfo;

	/* Put the direct parameter (a descriptor list) into a docList */
	err = AEGetParamDesc(
		theAppleEvent,
		keyDirectObject,
		typeAEList,
		&docList);
	if (err) return err;

	/*
	 * We ignore the validity check, because we trust the FInder, and we only
	 * allow one savefile to be opened, so we ignore the depth of the list.
	 */

	err = AEGetNthPtr(
		&docList,
		1L,
		typeFSS,
		&keywd,
		&returnedType,
		(Ptr) &myFSS,
		sizeof(myFSS),
		&actualSize);
	if (err) return err;

	/* Only needed to check savefile type below */
	err = FSpGetFInfo(&myFSS, &myFileInfo);
	if (err)
	{
		sprintf(foo, "Arg!  FSpGetFInfo failed with code %d", err);
		mac_warning (foo);
		return err;
	}

	/* Ignore non 'SAVE' files */
	if (myFileInfo.fdType != 'SAVE') return noErr;

	/* XXX XXX XXX Extract a file name */
	PathNameFromDirID(myFSS.parID, myFSS.vRefNum, (StringPtr)savefile);
	pstrcat((StringPtr)savefile, (StringPtr)&myFSS.name);

	/* Convert the string */
	ptocstr((StringPtr)savefile);

	/* Delay actual open */
	open_when_ready = TRUE;

	/* Dispose */
	err = AEDisposeDesc(&docList);

	/* Success */
	return noErr;
}


#endif /* USE_SFL_CODE */


/*
 * Handle quit_when_ready, by Peter Ammon,
 * slightly modified to check inkey_flag.
 */
static void quit_calmly(void)
{
	/* Quit immediately if game's not started */
	if (!game_in_progress || !character_generated) quit(NULL);
	
	/* Save the game and Quit (if it's safe) */
	if (inkey_flag)
	{
		/* Hack -- Forget messages */
		msg_flag = FALSE;

		/* Save the game */
#ifndef AUTO_SAVE_ARG_REQUIRED
		do_cmd_save_game();
#else
		do_cmd_save_game(FALSE);
#endif /* !AUTO_SAVE_ARG_REQUIRED */

		/* Quit */
		quit(NULL);
	}

	/* Wait until inkey_flag is set */
}


/*
 * Macintosh modifiers (event.modifier & ccc):
 *   cmdKey, optionKey, shiftKey, alphaLock, controlKey
 *
 *
 * Macintosh Keycodes (0-63 normal, 64-95 keypad, 96-127 extra):
 *
 * Return:36
 * Delete:51
 *
 * Period:65
 * Star:67
 * Plus:69
 * Clear:71
 * Slash:75
 * Enter:76
 * Minus:78
 * Equal:81
 * 0-7:82-89
 * 8-9:91-92
 *
 * backslash/vertical bar (Japanese keyboard):93
 *
 * F5: 96
 * F6: 97
 * F7: 98
 * F3:99
 * F8:100
 * F10:101
 * F11:103
 * F13:105
 * F14:107
 * F9:109
 * F12:111
 * F15:113
 * Help:114
 * Home:115
 * PgUp:116
 * Del:117
 * F4: 118
 * End:119
 * F2:120
 * PgDn:121
 * F1:122
 * Lt:123
 * Rt:124
 * Dn:125
 * Up:126
 */


/*
 * Check for Events, return TRUE if we process any
 *
 * Now it really waits for events if wait set to true, to prevent
 * undesirable monopoly of CPU. The side-effect is that you cannot do
 * while (CheckEvents(TRUE)); without discretion.
 */
static bool CheckEvents(bool wait)
{
	EventRecord event;

	WindowPtr w;

	Rect r;
	
	UInt32 sleep_ticks;

	int ch, ck;

	int mc, ms, mo, mx;

	int i;

	term_data *td = NULL;


	/*
	 * With the wait mode blocking for available event / timeout,
	 * the non-wait mode should actually call WaitNextEvent,
	 * because of those event draining loops. Or we had to
	 * implement yet another mode.
	 */

	/* Handles the quit_when_ready flag */
	if (quit_when_ready) quit_calmly();
	
	/* Blocking call to WaitNextEvent */
	if (wait) sleep_ticks = 0x7FFFFFFL;
	
	/* Non-blocking */
	else sleep_ticks = 0L;

	/* Get an event (or null)  */
	WaitNextEvent(everyEvent, &event, sleep_ticks, nil);

	/* Hack -- Nothing is ready yet */
	if (event.what == nullEvent) return (FALSE);


	/* Analyze the event */
	switch (event.what)
	{

#if 0

		case activateEvt:
		{
			w = (WindowPtr)event.message;

			activate(w);

			break;
		}

#endif

		case updateEvt:
		{
			/* Extract the window */
			w = (WindowPtr)event.message;

			/* Find the window */
			for (i = 0; i < MAX_TERM_DATA; i++)
			{
				/* Skip dead windows */
				if (!data[i].t) continue;

				/* Notice matches */
				if (data[i].w == w) td = &data[i];
			}

			/* Hack XXX XXX XXX */
			BeginUpdate(w);
			EndUpdate(w);

			/* Redraw the window */
			if (td) term_data_redraw(td);

			break;
		}

		case keyDown:
		case autoKey:
		{
			/* Extract some modifiers */
			mc = (event.modifiers & controlKey) ? TRUE : FALSE;
			ms = (event.modifiers & shiftKey) ? TRUE : FALSE;
			mo = (event.modifiers & optionKey) ? TRUE : FALSE;
			mx = (event.modifiers & cmdKey) ? TRUE : FALSE;

			/* Keypress: (only "valid" if ck < 96) */
			ch = (event.message & charCodeMask) & 255;

			/* Keycode: see table above */
			ck = ((event.message & keyCodeMask) >> 8) & 255;

			/* Command + "normal key" -> menu action */
			if (mx && (ck < 64))
			{
				/* Hack -- Prepare the menus */
				setup_menus();

				/* Mega-Hack -- allow easy exit if nothing to save */
				/* if (!character_generated && (ch=='Q' || ch=='q')) ch = 'e'; */

				/* Run the Menu-Handler */
				menu(MenuKey(ch));

				/* Turn off the menus */
				HiliteMenu(0);

				/* Done */
				break;
			}


			/* Hide the mouse pointer */
			ObscureCursor();

			/* Normal key -> simple keypress */
			if ((ck < 64) || (ck == 93))
			{
				/* Enqueue the keypress */
				Term_keypress(ch);
			}

			/* Keypad keys -> trigger plus simple keypress */
			else if (!mc && !ms && !mo && !mx && (ck < 96))
			{
				/* Hack -- "enter" is confused */
				if (ck == 76) ch = '\n';

				/* Begin special trigger */
				Term_keypress(31);

				/* Send the "keypad" modifier */
				Term_keypress('K');

				/* Send the "ascii" keypress */
				Term_keypress(ch);

				/* Terminate the trigger */
				Term_keypress(13);
			}

			/* Bizarre key -> encoded keypress */
			else if (ck <= 127)
			{
				/* Begin special trigger */
				Term_keypress(31);

				/* Send some modifier keys */
				if (mc) Term_keypress('C');
				if (ms) Term_keypress('S');
				if (mo) Term_keypress('O');
				if (mx) Term_keypress('X');

				/* Downshift and encode the keycode */
				Term_keypress(I2D((ck - 64) / 10));
				Term_keypress(I2D((ck - 64) % 10));

				/* Terminate the trigger */
				Term_keypress(13);
			}

			break;
		}

		case mouseDown:
		{
			int code;

			/* Analyze click location */
			code = FindWindow(event.where, &w);

			/* Find the window */
			for (i = 0; i < MAX_TERM_DATA; i++)
			{
				/* Skip dead windows */
				if (!data[i].t) continue;

				/* Notice matches */
				if (data[i].w == w) td = &data[i];
			}

			/* Analyze */
			switch (code)
			{
				case inMenuBar:
				{
					setup_menus();
					menu(MenuSelect(event.where));
					HiliteMenu(0);
					break;
				}

				case inDrag:
				{
					WindowPtr old_win;
					BitMap tBitMap;
					Rect pRect;

					r = GetQDGlobalsScreenBits(&tBitMap)->bounds;
					r.top += 20; /* GetMBarHeight() XXX XXX XXX */
					InsetRect(&r, 4, 4);
					DragWindow(w, event.where, &r);

					/* Oops */
					if (!td) break;

					/* Save */
					old_win = active;

					/* Activate */
					activate(td->w);

					/* Analyze */
					GetWindowBounds(
						(WindowRef)td->w,
						kWindowContentRgn,
						&pRect);
					td->r.left = pRect.left;
					td->r.top = pRect.top;

					/* Apply and Verify */
					term_data_check_size(td);

					/* Restore */
					activate(old_win);

					break;
				}

				case inGoAway:
				{
					/* Oops */
					if (!td) break;

					/* Track the go-away box */
					if (TrackGoAway(w, event.where))
					{
						/* Not Mapped */
						td->mapped = FALSE;

						/* Not Mapped */
						td->t->mapped_flag = FALSE;

						/* Hide the window */
						TransitionWindow(td->w,
							kWindowZoomTransitionEffect,
							kWindowHideTransitionAction,
							NULL);
					}

					break;
				}

				case inGrow:
				{
					int x, y;
					
					Rect nr;

					term *old = Term;

					/* Oops */
					if (!td) break;

#ifndef ALLOW_BIG_SCREEN

					/* Minimum and maximum sizes */
					r.left = 20 * td->tile_wid + td->size_ow1;
					r.right = 80 * td->tile_wid + td->size_ow1 + td->size_ow2 + 1;
					r.top = 1 * td->tile_hgt + td->size_oh1;
					r.bottom = 24 * td->tile_hgt + td->size_oh1 + td->size_oh2 + 1;

					/* Grow the rectangle */
					if (!ResizeWindow(w, event.where, &r, NULL)) break;
#else

					/* Grow the rectangle */
					if (!ResizeWindow(w, event.where, NULL, NULL)) break;

#endif /* !ALLOW_BIG_SCREEN */


					/* Obtain geometry of resized window */
					GetWindowBounds(w, kWindowContentRgn, &nr);

					/* Extract the new size in pixels */
					y = nr.bottom - nr.top - td->size_oh1 - td->size_oh2;
					x = nr.right - nr.left - td->size_ow1 - td->size_ow2;

					/* Extract a "close" approximation */
					td->rows = y / td->tile_hgt;
					td->cols = x / td->tile_wid;

					/* Apply and Verify */
					term_data_check_size(td);

					/* Activate */
					Term_activate(td->t);

					/* Hack -- Resize the term */
					Term_resize(td->cols, td->rows);

					/* Resize and Redraw */
					term_data_resize(td);
					term_data_redraw(td);

					/* Restore */
					Term_activate(old);

					break;
				}

				case inContent:
				{
					SelectWindow(w);

					break;
				}
			}

			break;
		}

#if 0 /* made obsolete by carbon */
		/* Disk Event -- From "Maarten Hazewinkel" */
		case diskEvt:
		{
			/* check for error when mounting the disk */
			if (HiWord(event.message) != noErr)
			{
				Point p =
				{120, 120};

				DILoad();
				DIBadMount(p, event.message);
				DIUnload();
			}

			break;
		}
#endif /* non-carbon */

		/* OS Event -- From "Maarten Hazewinkel" */
		case osEvt:
		{
			switch ((event.message >> 24) & 0x000000FF)
			{
				case suspendResumeMessage:

				/* Resuming: activate the front window */
				if (event.message & resumeFlag)
				{
					Cursor tempCursor;
					SetPort(GetWindowPort(FrontWindow()));
					SetCursor(GetQDGlobalsArrow(&tempCursor));
				}

				/* Suspend: deactivate the front window */
				else
				{
					/* Nothing */
				}

				break;
			}

			break;
		}

#ifdef USE_SFL_CODE

		/* From "Steve Linberg" and "Maarten Hazewinkel" */
		case kHighLevelEvent:
		{
			/* Process apple events */
			if (AEProcessAppleEvent(&event) != noErr)
			{
				plog("Error in Apple Event Handler!");
			}

			/* Handle "quit_when_ready" */
			if (quit_when_ready)
			{
#if 0 /* Doesn't work with Aqua well */
				/* Forget */
				quit_when_ready = FALSE;

				/* Do the menu key */
				menu(MenuKey('q'));
#endif
				/* Turn off the menus */
				HiliteMenu(0);
			}

			/* Handle "open_when_ready" */
			else
			{
				handle_open_when_ready();
			}

			break;
		}

#endif

	}


	/* Something happened */
	return (TRUE);
}


/*** Some Hooks for various routines ***/


/*
 * Mega-Hack -- emergency lifeboat
 */
static vptr lifeboat = NULL;


/*
 * Hook to "release" memory
 */
static vptr hook_rnfree(vptr v, huge size)
{

#pragma unused (size)

#ifdef USE_MALLOC

	/* Alternative method */
	free(v);

#else

	/* Dispose */
	DisposePtr(v);

#endif

	/* Success */
	return (NULL);
}

/*
 * Hook to "allocate" memory
 */
static vptr hook_ralloc(huge size)
{

#ifdef USE_MALLOC

	/* Make a new pointer */
	return (malloc(size));

#else

	/* Make a new pointer */
	return (NewPtr(size));

#endif

}

/*
 * Hook to handle "out of memory" errors
 */
static vptr hook_rpanic(huge size)
{

#pragma unused (size)

	vptr mem = NULL;

	/* Free the lifeboat */
	if (lifeboat)
	{
		/* Free the lifeboat */
		DisposePtr(lifeboat);

		/* Forget the lifeboat */
		lifeboat = NULL;

		/* Mega-Hack -- Warning */
		mac_warning("Running out of Memory!\rAbort this process now!");

		/* Mega-Hack -- Never leave this function */
		while (TRUE) CheckEvents(TRUE);
	}

	/* Mega-Hack -- Crash */
	return (NULL);
}


/*
 * Hook to tell the user something important
 */
static void hook_plog(cptr str)
{
	/* Warning message */
	mac_warning(str);
}

/*
 * Hook to tell the user something, and then quit
 */
static void hook_quit(cptr str)
{
	/* Warning if needed */
	if (str) mac_warning(str);

	/* Write a preference file */
	save_pref_file();

	/* All done */
	ExitToShell();
}

/*
 * Hook to tell the user something, and then crash
 */
static void hook_core(cptr str)
{
	/* XXX Use the debugger */
	/* DebugStr(str); */

	/* Warning */
	if (str) mac_warning(str);

	/* Warn, then save player */
	mac_warning("Fatal error.\rI will now attempt to save and quit.");

	/* Attempt to save */
	if (!save_player()) mac_warning("Warning -- save failed!");

	/* Quit */
	quit(NULL);
}



/*** Main program ***/


/*
 * Init some stuff
 *
 * XXX XXX XXX Hack -- This function attempts to "fix" the nasty
 * "Macintosh Save Bug" by using "absolute" path names, since on
 * System 7 machines anyway, the "current working directory" often
 * "changes" due to background processes, invalidating any "relative"
 * path names.  Note that the Macintosh is limited to 255 character
 * path names, so be careful about deeply embedded directories...
 *
 * XXX XXX XXX Hack -- This function attempts to "fix" the nasty
 * "missing lib folder bug" by allowing the user to help find the
 * "lib" folder by hand if the "application folder" code fails...
 */
static void init_stuff(void)
{
	Rect r;
	BitMap tBitMap;
	Rect screenRect;
	Point topleft;

	char path[1024];

	OSErr err = noErr;
	NavDialogOptions dialogOptions;
	FSSpec theFolderSpec;
	NavReplyRecord theReply;


	/* Fake rectangle */
	r.left = 0;
	r.top = 0;
	r.right = 344;
	r.bottom = 188;

	/* Center it */
	screenRect = GetQDGlobalsScreenBits(&tBitMap)->bounds;
	center_rect(&r, &screenRect);

	/* Extract corner */
	topleft.v = r.top;
	topleft.h = r.left;


	/* Default to the "lib" folder with the application */
	refnum_to_name(path, app_dir, app_vol, (char*)("\plib:"));


	/* Check until done */
	while (1)
	{
		/* Prepare the paths */
		init_file_paths(path);

		/* Build the filename */
		path_build(path, 1024, ANGBAND_DIR_FILE, "news.txt");

		/* Attempt to open and close that file */
		if (0 == fd_close(fd_open(path, O_RDONLY))) break;

		/* Warning */
		plog_fmt("Unable to open the '%s' file.", path);

		/* Warning */
		plog("The Angband 'lib' folder is probably missing or misplaced.");

		/* Ask the user to choose the lib folder */
		err = NavGetDefaultDialogOptions(&dialogOptions);

		/* Paranoia */
		if (err != noErr) quit(NULL);

		/* Set default location option */
		dialogOptions.dialogOptionFlags |= kNavSelectDefaultLocation;

		/* Clear preview option */
		dialogOptions.dialogOptionFlags &= ~(kNavAllowPreviews);

		/* Forbit selection of multiple files */
		dialogOptions.dialogOptionFlags &= ~(kNavAllowMultipleFiles);

		/* Display location */
		dialogOptions.location = topleft;

		/* Load the message for the missing folder from the resource fork */
		GetIndString(dialogOptions.message, 128, 1);

		/* Wait for the user to choose a folder */
		err = NavChooseFolder(
			nil,
			&theReply,
			&dialogOptions,
			nil,
			nil,
			nil);

		/* Assume the player doesn't want to go on */
		if ((err != noErr) || !theReply.validRecord) quit(NULL);

		/* Retrieve FSSpec from the reply */
		{
			AEKeyword theKeyword;
			DescType actualType;
			Size actualSize;

			/* Get a pointer to selected folder */
			err = AEGetNthPtr(
				&(theReply.selection),
				1,
				typeFSS,
				&theKeyword,
				&actualType,
				&theFolderSpec,
				sizeof(FSSpec),
				&actualSize);

			/* Paranoia */
			if (err != noErr) quit(NULL);
		}

		/* Free navitagor reply */
		err = NavDisposeReply(&theReply);

		/* Paranoia */
		if (err != noErr) quit(NULL);

		/* Extract textual file name for given file */
		refnum_to_name(
			path,
			theFolderSpec.parID,
			theFolderSpec.vRefNum,
			(char *)theFolderSpec.name);
	}
}


/*
 * Macintosh Main loop
 */
int main(void)
{
	int i;
	long response;
	OSStatus err;
	EventRecord tempEvent;
	UInt32 numberOfMasters = 10;

	/* Get more Masters */
	MoreMasterPointers(numberOfMasters);


	/* Check for existence of Carbon */
	err = Gestalt(gestaltCarbonVersion, &response);

	if (err != noErr) quit("This program requires Carbon API");

	/* See if we are running on Aqua */
	err = Gestalt(gestaltMenuMgrAttr, &response);

	/* Cache the result */
	if ((err == noErr) &&
	    (response & gestaltMenuMgrAquaLayoutMask)) is_aqua = TRUE;

	/* 
	 * Remember Mac OS version, in case we have to cope with version-specific
	 * problems
	 */
	(void)Gestalt(gestaltSystemVersion, &mac_os_version);


	/* Set up the Macintosh */
	InitCursor();

	/* Flush events */
	FlushEvents(everyEvent, 0);

	/* Flush events some more (?) */
	(void)EventAvail(everyEvent, &tempEvent);
	(void)EventAvail(everyEvent, &tempEvent);
	(void)EventAvail(everyEvent, &tempEvent);


#ifdef USE_SFL_CODE

	/* Install the start event hook (ignore error codes) */
	AEInstallEventHandler(
		kCoreEventClass,
		kAEOpenApplication,
		NewAEEventHandlerUPP(AEH_Start),
		0L,
		FALSE);

	/* Install the quit event hook (ignore error codes) */
	AEInstallEventHandler(
		kCoreEventClass,
		kAEQuitApplication,
		NewAEEventHandlerUPP(AEH_Quit),
		0L,
		FALSE);

	/* Install the print event hook (ignore error codes) */
	AEInstallEventHandler(
		kCoreEventClass,
		kAEPrintDocuments,
		NewAEEventHandlerUPP(AEH_Print),
		0L,
		FALSE);

	/* Install the open event hook (ignore error codes) */
	AEInstallEventHandler(
		kCoreEventClass,
		kAEOpenDocuments,
		NewAEEventHandlerUPP(AEH_Open),
		0L,
		FALSE);

#endif /* USE_SFL_CODE */


	/* Find the current application */
	SetupAppDir();


#if defined(MACINTOSH) && !defined(applec)

	/* Mark ourself as the file creator */
	_fcreator = ANGBAND_CREATOR;

	/* Default to saving a "text" file */
	_ftype = 'TEXT';

#endif


#if defined(ALLOW_QUITING) && defined(__MWERKS__)

	/* Obtian a "Universal Procedure Pointer" */
	ynfilterUPP = NewModalFilterUPP(ynfilter);

#endif /* ALLOW_QUITING && __MWERKS__ */


	/* Hook in some "z-virt.c" hooks */
	rnfree_aux = hook_rnfree;
	ralloc_aux = hook_ralloc;
	rpanic_aux = hook_rpanic;

	/* Hooks in some "z-util.c" hooks */
	plog_aux = hook_plog;
	quit_aux = hook_quit;
	core_aux = hook_core;


	/* Initialize colors */
	for (i = 0; i < 256; i++)
	{
		u16b rv, gv, bv;

		/* Extract the R,G,B data */
		rv = angband_color_table[i][1];
		gv = angband_color_table[i][2];
		bv = angband_color_table[i][3];

		/* Save the actual color */
		color_info[i].red = (rv | (rv << 8));
		color_info[i].green = (gv | (gv << 8));
		color_info[i].blue = (bv | (bv << 8));
	}


	/* Show the "watch" cursor */
	SetCursor(*(GetCursor(watchCursor)));

	/* Prepare the menubar */
	init_menubar();

	/* Prepare the windows */
	init_windows();

	/* Hack -- process all events */
	while (CheckEvents(FALSE)) /* loop */;

	/* Reset the cursor */
	{
		Cursor tempCursor;

		SetCursor(GetQDGlobalsArrow(&tempCursor));
	}

	/* Mega-Hack -- Allocate a "lifeboat" */
	lifeboat = NewPtr(16384);

	/* Note the "system" */
	ANGBAND_SYS = "mac";


	/* Initialize */
	init_stuff();

	/* Initialize */
	init_angband();


	/* Hack -- process all events */
	while (CheckEvents(FALSE)) /* loop */;


	/* We are now initialized */
	initialized = TRUE;


	/* Handle "open_when_ready" */
	handle_open_when_ready();

#ifndef SAVEFILE_SCREEN

	/* Prompt the user */
	prt("[Choose 'New' or 'Open' from the 'File' menu]", 23, 15);

	/* Flush the prompt */
	Term_fresh();

	/* Hack -- Process Events Forever */
	while (TRUE) CheckEvents(TRUE);

#else

	/* Game is in progress */
	game_in_progress = 1;

	/* Wait for keypress */
	pause_line(23);

	/* flush input - Warning: without this, _system_ would hang */
	flush();

	/* Play the game - note the value of the argument */
	play_game(FALSE);

	/* Quit */
	quit(NULL);

#endif /* !SAVEFILE_SCREEN */
}

#endif /* MACINTOSH */
