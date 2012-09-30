/* File: main-mac.c */

/* Purpose: Simple support for MACINTOSH Angband */

/*
 * This file should only be compiled with the "Macintosh" version
 *
 * This file written by "Ben Harrison (benh@phial.com)".
 *
 * Some code adapted from "MacAngband 2.6.1" by Keith Randall
 *
 * Maarten Hazewinkel (mmhazewi@cs.ruu.nl) provided some initial
 * suggestions for the PowerMac port.
 *
 * Steve Linberg (slinberg@crocker.com) provided the code surrounded
 * by "USE_SFL_CODE".
 *
 * The graphics code is adapted from an extremely minimal subset of
 * the code from "Sprite World II", an amazing animation package.
 *
 * See "z-term.c" for info on the concept of the "generic terminal"
 *
 * The preference file is now a text file named "Angband preferences".
 *
 * Note that the "preference" file is now a simple text file called
 * "Angband preferences", which contains the versions information, so
 * that obsolete preference files can be ignored (this may be bad).
 *
 * Note that "init1.c", "init2.c", "load1.c", "load2.c", and "birth.c"
 * should probably be "unloaded" as soon as they are no longer needed,
 * to save space, but I do not know how to do this.
 *
 * Stange bug -- The first "ClipRect()" call crashes if the user closes
 * all the windows, switches to another application, switches back, and
 * then re-opens the main window, for example, using "command-a".
 *
 * By default, this file assumes that you will be using a 68020 or better
 * machine, running System 7 and Color Quickdraw.  In fact, the game will
 * refuse to run unless these features are available.  This allows the use
 * of a variety of interesting features such as graphics and sound.
 *
 * To create a version which can be used on 68000 machines, or on machines
 * which are not running System 7 or Color Quickdraw, simply activate the
 * "ANGBAND_LITE_MAC" compilation flag in the proper header file.  This
 * will disable all "modern" features used in this file, including support
 * for multiple sub-windows, color, graphics, and sound.
 *
 * When compiling with the "ANGBAND_LITE_MAC" flag, the "ANGBAND_LITE"
 * flag will be automatically defined, which will disable many of the
 * advanced features of the game itself, reducing the total memory usage.
 *
 * If you are never going to use "graphics" (especially if you are not
 * compiling support for graphics anyway) then you can delete the "pict"
 * resource with id "1001" with no dangerous side effects.
 */


/*
 * Important Resources in the resource file:
 *
 *   FREF 130 = 'A271' / 'APPL' (application)
 *   FREF 129 = 'A271' / 'SAVE' (save file)
 *   FREF 130 = 'A271' / 'TEXT' (bone file, generic text file)
 *   FREF 131 = 'A271' / 'DATA' (binary image file, score file)
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
 *   MENU 130 = Edit (undo, -, cut, copy, paste, clear)
 *
 *   PICT 1001 = Graphics tile set
 */


/*
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
 */


/*
 * Reasons for each header file:
 *
 *   angband.h = Angband header file
 *
 *   Types.h = (included anyway)
 *   Gestalt.h = gestalt code
 *   QuickDraw.h = (included anyway)
 *   Files.h = file code
 *   Fonts.h = font code
 *   Menus.h = menu code
 *   Dialogs.h = dialog code
 *   MacWindows.h = window code
 *   Palettes.h = palette code
 *   ToolUtils.h = HiWord() / LoWord()
 *   Events.h = event code
 *   Resources.h = resource code
 *   Controls.h = control code
 *   SegLoad.h = ExitToShell()
 *   Memory.h = memory code
 *   QDOffscreen.h = GWorld code
 *   Sound.h = sound code
 *   Navigation.h = file dialog/navigation code
 *   Processes.h = process code

 *
 * For backwards compatibility:
 *   Use GestaltEqu.h instead of Gestalt.h
 *   Add Desk.h to include simply includes Menus.h, Devices.h, Events.h
 */
 
#ifndef TARGET_CARBON
#define TARGET_CARBON 1
#endif

#include "angband.h"

#include <Carbon/Carbon.h>
#include <QuickTime/QuickTime.h>
#include <CoreFoundation/CoreFoundation.h>

/*
#include <Types.h>
#include <Gestalt.h>
#include <QuickDraw.h>
#include <QuickdrawText.h>
#include <Files.h>
#include <Fonts.h>
#include <Menus.h>
#include <Dialogs.h>
#include <MacWindows.h>
#include <Palettes.h>
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
#include <Processes.h>
#include <QuickTimeComponents.h>
#include <ImageCompression.h>
#include <Movies.h>
*/

#ifndef TARGET_CARBON
#include <DiskInit.h>
#endif


/* Non-Carbon backward compatibility */
#ifndef TARGET_CARBON
#define EnableMenuItem(a,b)		EnableItem(a,b)
#define DisableMenuItem(a,b)	DisableItem(a,b)
#endif


/*
 * Use "malloc()" instead of "NewPtr()"
 */
#define USE_MALLOC


#if defined(powerc) || defined(__powerc)

/*
 * Disable "LITE" version
 */
# undef ANGBAND_LITE_MAC

#endif

 
#ifdef ANGBAND_LITE_MAC

/*
 * Maximum number of windows
 */
# define MAX_TERM_DATA 1

#else /* ANGBAND_LITE_MAC */

/*
 * Maximum number of windows
 */
# define MAX_TERM_DATA 8

/*
 * Activate some special code
 */
# define USE_SFL_CODE

#endif /* ANGBAND_LITE_MAC */



#ifdef USE_SFL_CODE

/*
 * Include the necessary header files
 */
/*
#include <AppleEvents.h>
#include <EPPC.h>
#include <Folders.h>
*/
#endif


#if 0

/*
 * The Angband Color Set (0 to 15):
 *   Black, White, Slate, Orange,    Red, Blue, Green, Umber
 *   D-Gray, L-Gray, Violet, Yellow, L-Red, L-Blue, L-Green, L-Umber
 *
 * Colors 8 to 15 are basically "enhanced" versions of Colors 0 to 7.
 *
 * On the Macintosh, we use color quickdraw, and we use actual "RGB"
 * values below to choose the 16 colors.
 *
 * If we are compiled for ancient machines, we bypass color and simply
 * draw everything in white (letting "z-term.c" automatically convert
 * "black" into "wipe" calls).
 */
static RGBColor foo[16] =
{
	{0x0000, 0x0000, 0x0000},	/* TERM_DARK */
	{0xFFFF, 0xFFFF, 0xFFFF},	/* TERM_WHITE */
	{0x8080, 0x8080, 0x8080},	/* TERM_SLATE */
	{0xFFFF, 0x8080, 0x0000},	/* TERM_ORANGE */
	{0xC0C0, 0x0000, 0x0000},	/* TERM_RED */
	{0x0000, 0x8080, 0x4040},	/* TERM_GREEN */
	{0x0000, 0x0000, 0xFFFF},	/* TERM_BLUE */
	{0x8080, 0x4040, 0x0000},	/* TERM_UMBER */
	{0x4040, 0x4040, 0x4040},	/* TERM_L_DARK */
	{0xC0C0, 0xC0C0, 0xC0C0},	/* TERM_L_WHITE */
	{0xFFFF, 0x0000, 0xFFFF},	/* TERM_VIOLET */
	{0xFFFF, 0xFFFF, 0x0000},	/* TERM_YELLOW */
	{0xFFFF, 0x0000, 0x0000},	/* TERM_L_RED */
	{0x0000, 0xFFFF, 0x0000},	/* TERM_L_GREEN */
	{0x0000, 0xFFFF, 0xFFFF},	/* TERM_L_BLUE */
	{0xC0C0, 0x8080, 0x4040}	/* TERM_L_UMBER */
};

#endif


/*
 * Forward declare
 */
typedef struct term_data term_data;

/*
 * Extra "term" data
 */
struct term_data
{
	term		*t;

	Rect		r;

	WindowPtr	w;

#ifdef ANGBAND_LITE_MAC

	/* Nothing */

#else /* ANGBAND_LITE_MAC */

	short padding;

	short pixelDepth;

	GWorldPtr theGWorld;

	GDHandle theGDH;

	GDHandle mainSWGDH;

#endif /* ANGBAND_LITE_MAC */

	Str15		title;

	s16b		oops;

	s16b		keys;

	s16b		last;

	s16b		mapped;

	s16b		rows;
	s16b		cols;

	s16b		font_id;
	s16b		font_size;
	s16b		font_face;
	s16b		font_mono;

	s16b		font_o_x;
	s16b		font_o_y;
	s16b		font_wid;
	s16b		font_hgt;

	s16b		tile_o_x;
	s16b		tile_o_y;
	s16b		tile_wid;
	s16b		tile_hgt;

	s16b		size_wid;
	s16b		size_hgt;

	s16b		size_ow1;
	s16b		size_oh1;
	s16b		size_ow2;
	s16b		size_oh2;
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
 * Hack -- game in progress
 */
static int game_in_progress = 0;


/*
 * Only do "SetPort()" when needed
 */
static WindowPtr active = NULL;



/*
 * An array of term_data's
 */
static term_data data[MAX_TERM_DATA];



/*
 * Note when "open"/"new" become valid
 */
static bool initialized = FALSE;

/*
 * Constants
 */

/*
 * Strings that contain the basic "lib/xtra" directory paths
 */
#define ANGBAND_XTRA_FONT		"/lib/xtra/font"
#define ANGBAND_XTRA_GRAF		"/lib/xtra/graf"
#define ANGBAND_XTRA_MUSIC		"/lib/xtra/music"
#define ANGBAND_XTRA_SOUND		"/lib/xtra/sound"

#define ANGBAND_XTRA_GRAF_16x16	":lib:xtra:graf:16x16.gif"
#define ANGBAND_XTRA_GRAF_8x8	":lib:xtra:graf:8x8.gif"

/*
 * Constants that control the sound effects and music sound tracks
 */

#define SAMPLE_MAX				10		/* How many sample choices per sound effect are allowed */
#define kMaxChannels			20		/* How many sound channels will be pooled */
#define SOUND_VOLUME_MIN		0		/* Default minimum sound volume for sound effects */
#define SOUND_VOLUME_MAX		255		/* Default maximum sound volume for sound effects */
#define VOLUME_MIN				0		/* Minimum sound volume in % */
#define VOLUME_MAX				100		/* Maximum sound volume in % */
#define VOLUME_INC				5		/* Increment sound volume in % */
#define SONG_MAX				20		/* How many music sound tracks */
#define SONG_VOLUME_MIN			0		/* Default minimum sound volume for sound tracks */
#define SONG_VOLUME_MAX			255		/* Default maximum sound volume for sound tracks */
#define SONG_REPEAT_MIN			1		/* Minimum times a sound track can be repeated */
#define SONG_REPEAT_MAX			100		/* Maximum times a sound track can be repeated */

static char sample_name[SOUND_MAX][SAMPLE_MAX][32];			/* File names of each sound effect sample */
static Handle sample[SOUND_MAX][SAMPLE_MAX];				/* Data handle containing the sound data for each sample */
static int sample_count[SOUND_MAX];							/* Sample count for each sound effect (not necessarily SAMPLE_MAX for each sound effect) */
static SndChannelPtr mySndChannel[kMaxChannels];			/* Sound channel pool */
static long	channelInit = 0;								/* Has the sound channel pool been initialized */

static short gSoundVolume = 10;								/* sound volume percentage */
static char song_name[SONG_MAX][32];						/* File names for each music sound track */
static char song_description[SONG_MAX][32];					/* File description for each music sound track */
static short song_volume[SONG_MAX];							/* Volume for each music sound track */
static short song_repeat[SONG_MAX];							/* Repeat count for each music sound track */
static Movie song[SONG_MAX];								/* QT Movie for each music sound track */
static int current_song = 0;								/* Which music sound track is currently playing */
static bool songReady = false;								/* Has the music sound tracks been loaded */
static bool songStarted = false;							/* Has the music sound tracks been started */
static bool songOn = false;									/* Do we want the music to play or not */

short	gTileWidth;								/* Graf Size (X) */
short	gTileHeight;							/* Graf Size (Y) */
short	gTileCols;								/* Number of Cols in Pict */
short	gTileRows;								/* Number of Rows in Pict */
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
	GWorldPtr 		framePort;					/* Graphic World for storing graphic tiles */
	PixMapHandle 	framePixHndl;				/* Handle to PixMap for storing graphic tiles */
	PixMapPtr 		framePix;					/* Ptr to PixMap for storing graphic tiles */
	
	GWorldPtr		bufferPort;					/* Graphic World for buffering one row of drawing */
	PixMapHandle	bufferPixHndl;				/* Handle to PixMap for buffering one row of drawing */
	PixMapPtr		bufferPix;					/* Ptr to PixMap for buffering one row of drawing */
};


/*
 * The global picture data
 */
static FrameRec *frameP = NULL;
			
/*
 * CodeWarrior uses Universal Procedure Pointers
 */


#ifdef USE_SFL_CODE

/*
 * Apple Event Hooks
 */
AEEventHandlerUPP AEH_Start_UPP;
AEEventHandlerUPP AEH_Quit_UPP;
AEEventHandlerUPP AEH_Print_UPP;
AEEventHandlerUPP AEH_Open_UPP;

#endif

static void local_to_global( Rect *r )
{
	Point		temp;
	
	temp.h = r->left;
	temp.v = r->top;
	
	LocalToGlobal( &temp );
	
	r->left = temp.h;
	r->top = temp.v;
	
	temp.h = r->right;
	temp.v = r->bottom;
	
	LocalToGlobal( &temp );
	
	r->right = temp.h;
	r->bottom = temp.v;
}

static void global_to_local( Rect *r )
{
	Point		temp;
	
	temp.h = r->left;
	temp.v = r->top;
	
	GlobalToLocal( &temp );
	
	r->left = temp.h;
	r->top = temp.v;
	
	temp.h = r->right;
	temp.v = r->bottom;
	
	GlobalToLocal( &temp );
	
	r->right = temp.h;
	r->bottom = temp.v;
}

/*
 * Center a rectangle inside another rectangle
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


#if defined(USE_SFL_CODE)


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

static void p2c_stringcopy( char *dst, StringPtr src )
{
	BlockMove( src + 1, (void *)dst, *src );
	dst[*src] = 0x00;
}

static void c2p_stringcopy( StringPtr dst, const char *src )
{
	short	len = strlen(src);
	if( len > 255 ) len = 255;
	
	BlockMove( (void *)src, dst + 1, len );
	*dst = (unsigned char)len;
}

static void PathNameFromDirID(long dirID, short vRefNum, StringPtr fullPathName)
{
	CInfoPBRec	block;
	Str255	directoryName;
	OSErr	err;

	fullPathName[0] = '\0';

	block.dirInfo.ioDrParID = dirID;
	block.dirInfo.ioNamePtr = directoryName;

	while (1)
	{
		block.dirInfo.ioVRefNum = vRefNum;
		block.dirInfo.ioFDirIndex = -1;
		block.dirInfo.ioDrDirID = block.dirInfo.ioDrParID;
		err = PBGetCatInfo(&block, FALSE);
		pstrcat(directoryName, (StringPtr)"\p/");
		pstrinsert(fullPathName, directoryName);
		if (block.dirInfo.ioDrDirID == 2) break;
	}
	
	/* Add the root slash */
	pstrinsert(fullPathName, (StringPtr)"\p/" );
}

#endif





/*
 * Activate a given window, if necessary
 */
static void activate(WindowPtr w)
{
	/* Activate */
	if (active != w)
	{
		/* Activate */
		if (w)
		{
			SetPort(GetWindowPort(w));
		}
		
		/* Remember */
		active = w;
	}
}
/*
 * Display a confirm dialog
 */
static bool mac_confirm(cptr message, cptr description)
{
	bool return_value = false;
	OSErr err;
	Str255 errorMessage;
	Str255 descriptionMessage;
	SInt16 itemHit;
	/*
		Four types of alert we can choose from:
			kAlertStopAlert
			kAlertNoteAlert
			kAlertCautionAlert
			kAlertPlainAlert
	*/
	if( message != NULL )
		c2p_stringcopy( errorMessage, message );
	else
		c2p_stringcopy( errorMessage, "Warning!" );
	
	if( description != NULL )
		c2p_stringcopy( descriptionMessage, description );
	else
		c2p_stringcopy( descriptionMessage, "" );
		
	err = StandardAlert( kAlertCautionAlert, errorMessage, descriptionMessage, NULL, &itemHit );
	if( err == noErr )
	{
		/*
			Four Alert Button Choices
				kAlertStdAlertOKButton
				kAlertStdAlertCancelButton
				kAlertStdAlertOtherButton
				kAlertStdAlertHelpButton
		*/
		
		switch( itemHit )
		{
			case kAlertStdAlertOKButton:
				{
					return_value = true;
				}
				break;
			case kAlertStdAlertCancelButton:
				{
					return_value = false;
				}
				break;
			case kAlertStdAlertOtherButton:
				{
					return_value = false;
				}
				break;
			case kAlertStdAlertHelpButton:
				{
					return_value = false;
				}
				break;
			default:
				{
					return_value = false;
				}
		}
	}
	
	return( return_value );
}

/*
 * Display a warning message
 */
static void mac_warning(cptr warning)
{
	/* ignore the button pressed */
	mac_confirm( warning, "" );
}



static OSType GetProcessSignature( void )
{
	ProcessSerialNumber		thePSN;
	ProcessInfoRec			info;
	OSErr					err;
	
	thePSN.highLongOfPSN = 0;
	thePSN.lowLongOfPSN  = kCurrentProcess;
	
	info.processInfoLength	= sizeof(ProcessInfoRec);
	info.processName		= nil;
	info.processAppSpec		= nil;
		
	err = GetProcessInformation(&thePSN, &info);
	
	if( err != noErr )
		quit( "Internal System Error.  Process Information could not be found." );
	
	return info.processSignature;
}

static OSErr ChooseFile( StringPtr filename, OSType *typelist, long typeCount )
{
	NavReplyRecord		reply;
	NavDialogOptions	dialogOptions;
	NavTypeListHandle	navTypeList = NULL;
	OSErr				err;
	
	err = NavGetDefaultDialogOptions( &dialogOptions );
	
	if( err == noErr )
	{
		if( typeCount > 0 )
		{
			navTypeList = (NavTypeListHandle)NewHandle( sizeof(NavTypeList) + (sizeof(OSType)*(typeCount-1)) );
			if( navTypeList == NULL )
				quit( "Could not allocate memory for navigation file filter list." );
			
			/* populate the navtypelist object */
			{
				NavTypeListPtr	typesP = (NavTypeListPtr) *((Handle) navTypeList);
				OSType	signature = GetProcessSignature();
			
				typesP->componentSignature	= signature;
				typesP->reserved			= 0;
				typesP->osTypeCount			= typeCount;
				
				BlockMoveData(typelist, typesP->osType,
										(Size) (sizeof(OSType) * typeCount));
			}
		}
							
		err = NavChooseFile( NULL, &reply, &dialogOptions, NULL, NULL, NULL, navTypeList, NULL );
		if( reply.validRecord && err == noErr )
		{
			FSSpec		finalFSSpec;
			
			long		index;
			long		count;
			/*
			 * We are ready to open the document(s),
			 * grab information about each file for opening:
			 */
			err = AECountItems( &(reply.selection), &count );
			for ( index=1; index<=count; index++ )
			{
				AEKeyword 	keyWord;
				DescType 	typeCode;
				Size 		actualSize = 0;

				if (( err = AEGetNthPtr( &(reply.selection), index, typeFSS, &keyWord, &typeCode, &finalFSSpec, sizeof( FSSpec ), &actualSize )) == noErr )
				{
					FSRef		fileRef;
					Handle		pathH;
					short		length;
					
					err = FSpMakeFSRef( &finalFSSpec, &fileRef );
					if( err == noErr )
					{
						err = FSRefMakePath( &fileRef, filename, &length );
						/* plog_fmt( "filename = %s", filename ); */
					}
				}
			}
		}
		
		if( navTypeList != NULL )
		{
			DisposeHandle( (Handle)navTypeList );
			navTypeList = NULL;
		}
	}
	
	return err;
}

static errr process_sound_config_file( const char *name, const char *section )
{
	FILE *fp;

	char buf[1024];

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;

	bool foundSection = FALSE;
	
	char soundpath[1024];

	/* Build the filename */
	path_make(soundpath, ANGBAND_DIR_XTRA, "sound");
	/* plog_fmt("XTRA/SOUND = %s", soundpath ); */
	path_make(buf, soundpath, name);

	/* Open the file */
	fp = my_fopen(buf, "r");

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

		/* Look for section, presumably [SOUND] */
		if( buf[0] == '[' )
		{
			char		section_string[64];
			
			strnfmt( section_string, 64, "[%s]", section );
			
			if( streq( section_string, buf ) )
			{
				foundSection = TRUE;
				continue;
			}
		}
		
		if( foundSection )
		{
			int i, index = 0, count = 0;
			char *t;
			char *cptr = &(buf[0]);
			char *the_file;
			char *the_name = strtok(buf, "=");
			
			if( !the_name ) continue;
			
			/* trim spaces */
			while( *the_name && *the_name == ' ' ) the_name++;
			t = the_name + strlen(the_name) - 1;
			while( *t && *t == ' ' )
			{
				*t = '\0';
				t--;
			}
			
			/* Find sound effect in pre-defined sound effect list */
			for( i = 1; i < SOUND_MAX; i++ )
			{
				if(streq( angband_sound_name[i], the_name ))
				{
					index = i;
					break;
				}
			}
			
			/* If sound effect number not found, then skip it */
			if( index == 0 ) continue;
			
			/* Load in the specified sample filenames */
			do
			{
				the_file = strtok(NULL, " ");
				
				if( the_file )
				{
					/* trim spaces */
					while( *the_file && *the_file == ' ' ) the_file++;
					t = the_file + strlen(the_file) - 1;
					while( *t && *t == ' ' )
					{
						*t = '\0';
						t--;
					}
			
					/* Add the sample filename */
					strcpy( sample_name[index][count], the_file );
					count++;
				}
			}
			while( the_file );
			
			/* Set the sound effect sample count */
			sample_count[index] = count;
		}

		/* Oops */
		if (err) break;
	}


	/* Error */
	if (err)
	{
		/* Useful error message */
		msgf("Error %d in line %d of file '%s'.", err, num, name);
		msgf("Parsing '%s'", buf);
	}

	/* Close the file */
	my_fclose(fp);

	/* Result */
	return (err);
}

static errr process_music_config_file( const char *name, const char *section )
{
	FILE *fp;
	
	char musicpath[1024];
	char buf[1024];

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;

	bool foundSection = FALSE;
	

	/* Build the filename */
	path_make(musicpath, ANGBAND_DIR_XTRA, "music");
	path_make(buf, musicpath, name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No such file */
	if (!fp) return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, 1024))
	{
		char *pbuf = &(buf[0]);
		
		/* Count lines */
		num++;


		/* Skip "empty" lines */
		if (!pbuf[0]) continue;

		/* Skip "blank" lines */
		if (isspace(pbuf[0])) continue;

		/* Skip comments */
		if (pbuf[0] == '#') continue;

		/* Look for section, presumably [MUSIC] */
		if( pbuf[0] == '[' )
		{
			char		section_string[64];
			
			strnfmt( section_string, 64, "[%s]", section );
			
			if( streq( section_string, pbuf ) )
			{
				foundSection = TRUE;
				continue;
			}
		}
		
		if( foundSection )
		{
			int i, index = 0, count = 0, volume = 0, repeat = 0;
			char *t;
			char *the_file;
			char *the_description;
			char *the_volume;
			char *the_repeat;
			char *the_name = strtok(pbuf, "=");
			
			if( !the_name ) continue;
			
			/* trim spaces */
			while( *the_name && *the_name == ' ' ) the_name++;
			t = the_name + strlen(the_name) - 1;
			while( *t && *t == ' ' )
			{
				*t = '\0';
				t--;
			}
			
			/* Get the music sound track number */
			index = atoi( the_name );
			
			/* If the sound track number is invalid, then skip it */
			if( index < 0 || index > SONG_MAX ) continue;
			
			/* Get the filename for the sound track, assumed to be in the lib/xtra/music directory */	
			the_file = strtok(NULL, "|");
			
			if( the_file )
			{
				/* trim spaces */
				while( *the_file && *the_file == ' ' ) the_file++;
				t = the_file + strlen(the_file) - 1;
				while( *t && *t == ' ' )
				{
					*t = '\0';
					t--;
				}
		
				strcpy( song_name[index], the_file );
				strcpy( song_description[index], the_file );
			}
			
			/* Get the description for the sound track, assumed to be in the lib/xtra/music directory */	
			the_description = strtok(NULL, "|");
			
			if( the_description )
			{
				/* trim spaces */
				while( *the_description && *the_description == ' ' ) the_description++;
				t = the_description + strlen(the_description) - 1;
				while( *t && *t == ' ' )
				{
					*t = '\0';
					t--;
				}
		
				if( *the_description != '-' || *the_description != (char)0x00 )
				{
					strcpy( song_description[index], the_description );
				}
				else if( the_file != NULL )
				{
					strcpy( song_description[index], the_file );
				}
				else
				{
					song_description[index][0] = 0);
				}
			}
			
			/* Get the volume (0-255) of this sound track, if it exists */
			the_volume = strtok(NULL, "|");
				
			if( the_volume )
			{
				/* trim spaces */
				while( *the_volume && *the_volume == ' ' ) the_volume++;
				t = the_volume + strlen(the_volume) - 1;
				while( *t && *t == ' ' )
				{
					*t = '\0';
					t--;
				}
		
				if( *the_volume != '-' )
				{
					volume = atoi( the_volume );
					
					/* Ignore specified volume if it is out of range, default is SONG_VOLUME_MAX */
					if( volume < SONG_VOLUME_MIN || volume > SONG_VOLUME_MAX )
					{
						song_volume[index] = SONG_VOLUME_MAX;
						/*continue;*/
					}
					else
					{
						song_volume[index] = volume;
					}
				}
			}
			
			/* Get the repeat value for the sound track, if it exists */
			the_repeat = strtok(NULL, "|");
				
			if( the_repeat )
			{
				/* trim spaces */
				while( *the_repeat && *the_repeat == ' ' ) the_repeat++;
				t = the_repeat + strlen(the_repeat) - 1;
				while( *t && *t == ' ' )
				{
					*t = '\0';
					t--;
				}
		
				if( *the_repeat != '-' )
				{
					repeat = atoi( the_repeat );
					
					/* Ignore the specified repeat value if it is out of range, default is SONG_REPEAT_MIN */
					if( repeat < SONG_REPEAT_MIN || repeat > SONG_REPEAT_MAX )
					{
						song_repeat[index] = SONG_REPEAT_MIN;
						/*continue;*/
					}
					else
					{
						song_repeat[index] = repeat;
					}
				}
			}
		}

		/* Oops */
		if (err) break;
	}


	/* Error */
	if (err)
	{
		/* Useful error message */
		msgf("Error %d in line %d of file '%s'.", err, num, name);
		msgf("Parsing '%s'", buf);
	}

	/* Close the file */
	my_fclose(fp);

	/* Result */
	return (err);
}

/*
 * get_picture_from_file
 *
 * Instead of loading the graphic tiles from macintosh resources, load the tiles from
 * a single graphic file, of any format supported by QuickTime.
 * Pass in the filename (path relative to the application), and it returns a PicHandle.
 */
static PicHandle get_picture_from_file( const char *filename )
{
	GraphicsImportComponent		gi;
	PicHandle					thePicture = NULL;
	OSErr						theErr;
	Str255						pFileName;
	FSSpec						theFile;
	
	c2p_stringcopy( pFileName, filename );
	
	theErr = FSMakeFSSpec( app_vol, app_dir, pFileName, &theFile );
	
	if( theErr == noErr )
	{
		theErr = GetGraphicsImporterForFile( &theFile, &gi );
		if( theErr == noErr )
		{
			GraphicsImportGetAsPicture( gi, &thePicture );
			
			if( thePicture == NULL )
			{
				char		message[512];
			
				strcpy( message, "The graphics file storing the appropriate tiles could not be loaded.  This file must be authentic and the format must be recognized by Quicktime. Please verify and try again.  The target file is -> " );
				strcat( message, filename );
				
				mac_warning( message );
			}
		}
		else
		{
			char		message[512];
			
			strcpy( message, "The graphics file storing the appropriate tiles could not be loaded.  Verify its validity and try again.  The target file is -> " );
			strcat( message, filename );
			
			mac_warning( message );
		}
	}
	else
	{
		char		message[512];
			
		strcpy( message, "The graphics file storing the appropriate tiles could not be found.  Verify its existence and try again.  The target file is -> " );
		strcat( message, filename );
		
		mac_warning( message );
	}
		
	return thePicture;
}

/*
 * get_next_song
 *
 * Determine the next sound track to play
 */
static int get_next_song( int current )
{
	int last_song = current++;
	int next = current;
	
	while( next != last_song )
	{
		if( next == SONG_MAX ) next = 0;
		if( streq( song_name[next], "" ) )
		{
			next++;
		}
		else
		{
			break;
		}
	}
	
	return next;
}

/*
 * check_music
 *
 * Check status of music sound track.
 * If the music sound track is playing, then devote some QT time for various tasks (MoviesTask)
 * If the sound track is done, then  start the next sound track
 * If we haven't started any sound track yet, then go ahead and start everything going.
 * Call this function OFTEN.  Best called from the idle thread or something.
 */
static void check_music( void )
{
	if( songReady )
	{
		if( songOn )
		{
			if( !songStarted )
			{
				GoToBeginningOfMovie( song[current_song] );
				SetMovieVolume( song[current_song], (short)song_volume[current_song] );
				StartMovie( song[current_song] );
				
				songStarted = true;
			}
			else if( songStarted && IsMovieDone( song[current_song] ) )
			{
				/* restart from first available song */
				current_song = get_next_song( current_song );
				
				GoToBeginningOfMovie( song[current_song] );
				SetMovieVolume( song[current_song], (short)song_volume[current_song] );
				StartMovie( song[current_song] );
				
				songStarted = true;
			}
			else
			{
				if( !streq( song_name[current_song], "" ) )
				{
					MoviesTask( song[current_song], 0 );
				}
			}
		}
		else
		{
			if( songStarted && !IsMovieDone( song[current_song] ) )
			{
				/* stop the current song */
				StopMovie( song[current_song] );
				
				songStarted = false;
			}
		}
	}
}

/*
 * restart_music
 *
 * When someone selects a specific sound track to play,
 * interrupt the currently playing sound track, and start the specified one
 */
static void restart_music( int new_song )
{
	if( songReady )
	{
		if( !songOn && (new_song != current_song))
		{
			songOn = true;
			
			if( new_song >= 0 && new_song < SONG_MAX )
			{
				if( songStarted && !IsMovieDone( song[current_song] ) )
				{
					StopMovie( song[current_song] );
					
					current_song = new_song;
					
					songStarted = false;
				}
				else
				{
					current_song = new_song;
					
					songStarted = false;
				}
			}
		}
		else
		{
			/* stop song playing */
			songOn = false;
		}
	}
}

/*
 * init_music
 *
 * Load the music sound tracks specified in the "lib/xtra/music/music.cfg" file
 */
static void init_music( void )
{
	OSErr		err;
	int			i;
	short		movieResFile;
	char		musicpath[1024];
	char		filepath[1024];
	Str255		pFilePath;
	FSSpec		theFile;
		
	songReady = false;
	songStarted = false;
	
	for( i = 0; i < SONG_MAX; i++ )
	{
		song_name[i][0] = 0;
		song_description[i][0] = 0;
		song_volume[i] = ((gSoundVolume*255)/100);
		song_repeat[i] = 1;
		song[i] = (Movie)NULL;
		
		songReady = false;
	}
		
	err = process_music_config_file( "music.cfg", "Music" );
	
	if( err == noErr )
	{
		for( i = 0; i < SONG_MAX; i++ )
		{
			if( !streq( song_name[i], "" ) )
			{
				/*path_make(musicpath, ANGBAND_DIR_XTRA, "music");*/
				/*path_make( filepath, musicpath, song_name[i]);*/
				
				strnfmt( filepath, 1024, ":lib:xtra:music:%s", song_name[i] );
				/*path_make( filepath, ANGBAND_XTRA_MUSIC, song_name[i] );*/
				
				c2p_stringcopy( pFilePath, filepath );

				err = FSMakeFSSpec( app_vol, app_dir, pFilePath, &theFile );

				if( err == noErr )
				{
					err = OpenMovieFile( &theFile, &movieResFile, fsRdPerm );
					if( err == noErr )
					{
						short		movieResID = 0;
						Str255		movieName;
						Boolean		wasChanged;
						
						err = NewMovieFromFile( &song[i],
												movieResFile,
												&movieResID,
												movieName,
												newMovieActive,
												&wasChanged );
						
						if( err == noErr )
						{
							songReady = true;
						}
					}
				}
			}
		}
	}
}

/*
 * init_sounds
 *
 * Load the sound effect samples specified in the "lib/xtra/sound/sound.cfg" file
 */
static void init_sounds( void )
{
	OSErr			err;
	int				i, j;
	
	err = EnterMovies();
	
	
	/* Start Loading Sounds */
	
	
	err = process_sound_config_file( "sound.cfg", "Sound" );
	
	/*
	 * This set of loops may take a while depending on the count
	 * and size of samples to load.
	 *
	 * We should use a progress dialog for this.
	 */
	for( i = 1; i < SOUND_MAX; i++ )
	{
		if( sample_count[i] > SAMPLE_MAX ) sample_count[i] = SAMPLE_MAX;
		
		for( j = 0; j < sample_count[i]; j++ )
		{
			short		movieResFile;
			char		soundpath[1024];
			char		filepath[1024];
			Str255		pFilePath;
			FSSpec		theFile;
		
			/*path_make( soundpath, ANGBAND_DIR_XTRA, "sound");*/
			/*path_make( filepath, soundpath, sample_name[i][j]);*/
		
			/*path_make( filepath, ANGBAND_XTRA_SOUND, sample_name[i][j] );*/
			strnfmt( filepath, 1024, ":lib:xtra:sound:%s", sample_name[i][j] );
			
			c2p_stringcopy( pFilePath, filepath );
	
			err = FSMakeFSSpec( app_vol, app_dir, pFilePath, &theFile );
	
			if( err == noErr )
			{
				err = OpenMovieFile( &theFile, &movieResFile, fsRdPerm );
				if( err == noErr )
				{
					short		movieResID = 0;
					Str255		movieName;
					Boolean		wasChanged;
					Movie		theMovie;
					
					err = NewMovieFromFile( &theMovie,
											movieResFile,
											&movieResID,
											movieName,
											newMovieActive,
											&wasChanged );
				
					if( err == noErr )
					{
						Track		myTrack = NULL;
					
						myTrack = GetMovieIndTrackType( theMovie, 
													1, 
													AudioMediaCharacteristic,
													movieTrackCharacteristic | movieTrackEnabledOnly );
					
						if( myTrack != NULL )
						{
							sample[i][j] = NewHandle(0);
						
							if( sample[i][j] != NULL )
							{
								/*SetMovieProgressProc(theMovie, (MovieProgressUPP) -1L, 0 );*/
								
								err = PutMovieIntoTypedHandle(
													theMovie,
													myTrack,
													soundListRsrc,
													sample[i][j],
													0,
													GetTrackDuration(myTrack),
													0L,
													NULL );
							}
						}
					}
					
					CloseMovieFile( movieResFile );
					DisposeMovie( theMovie );
				}
			}
			
			if( err != noErr )
			{
				sample[i][j] = (Handle)NULL;
			}
		}
	}
	
	/* Stop Loading Sounds */
}

/*
 * close_sounds
 *
 * Call this when quitting the application.
 */
static void close_sounds( void )
{
	ExitMovies();
}

/*
 * play_sound
 *
 * Play an asynchronous sound
 */
static void play_sound( int sound_number, int sound_volume, bool async )
{
	if( sound_number > 0 && sound_number < SOUND_MAX )
	{
		if( sample_count[sound_number] > 0 )
		{
			int		random_sample = randint(sample_count[sound_number]) - 1;
			
			if (sample[sound_number][random_sample])
			{
				/* Lock */
				HLock(sample[sound_number][random_sample]);

				{
					
					long					kk;
					OSErr					myErr = noErr;

					if( !channelInit )
					{
						for( kk=0; kk < kMaxChannels; kk++ )
						{
						  /* Create sound channel for all sounds to play from */
						  SndNewChannel( &mySndChannel[kk], sampledSynth, initMono, 0L );
						}
						channelInit = 1;
					}

					if( sample[sound_number][random_sample] != nil )
					{
						SCStatus		status;
						long			found;

						for( kk=0,found=0; kk < kMaxChannels && !found; kk++ )
						{
							myErr = SndChannelStatus( mySndChannel[kk], sizeof(SCStatus), &status );
							if( myErr == noErr && !status.scChannelBusy )
							{
								SndCommand		theVolumeCommand;
								
								theVolumeCommand.cmd = volumeCmd;
								theVolumeCommand.param1 = 0;
								theVolumeCommand.param2 = ((short)(sound_volume)<<4) | ((short)(sound_volume));
								
								/* Set up volume for channel */
								SndDoImmediate( mySndChannel[kk], &theVolumeCommand );
								
								/* Play new sound ansynchronously */
								SndPlay( mySndChannel[kk], (SndListHandle)sample[sound_number][random_sample], async );
								
								found = 1;
							}
						}
						
						if( !found )
						{
							SndCommand		theQuietCommand;
							SndCommand		theVolumeCommand;
							
							theQuietCommand.cmd = quietCmd;
							theQuietCommand.param1 = 0;
							theQuietCommand.param2 = 0;
								
							theVolumeCommand.cmd = volumeCmd;
							theVolumeCommand.param1 = 0;
							theVolumeCommand.param2 = ((short)(sound_volume)<<4) | ((short)(sound_volume));
							
							/* Lets grab the first sound channel and play now */
							/* I'm sure a better algorithm could be used here. */
							SndDoImmediate( mySndChannel[0], &theQuietCommand );
							SndDoImmediate( mySndChannel[0], &theVolumeCommand );
							SndPlay( mySndChannel[0], (SndListHandle)sample[sound_number][random_sample], async );
						}
					}
				}
	            
				/* Unlock */
				HUnlock(sample[sound_number][random_sample]);
			}
		}
	}
}






/*** Some generic functions ***/


#ifdef ANGBAND_LITE_MAC

/*
 * Hack -- activate a color (0 to 255)
 */
#define term_data_color(TD,A) /* Nothing */

#else /* ANGBAND_LITE_MAC */

/*
 * Hack -- activate a color (0 to 255)
 */
static void term_data_color(term_data *td, int a)
{
	/* Activate the color */
	/*if (td->last != a) */
	{
		u16b rv, gv, bv;

		RGBColor color;

		/* Extract the R,G,B data */
		rv = angband_color_table[a][1];
		gv = angband_color_table[a][2];
		bv = angband_color_table[a][3];

		/* Set the color */
		color.red = (rv | (rv << 8));
		color.green = (gv | (gv << 8));
		color.blue = (bv | (bv << 8));
	
		/* Activate the color */
		RGBForeColor(&color);

		/* Memorize color */
		td->last = a;
	}
}

#endif /* ANGBAND_LITE_MAC */


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
	BitMap		screen;

#ifdef TARGET_CARBON						
	GetQDGlobalsScreenBits( &screen );
#else
	screen = qd.screenBits;
#endif	
		
	/* Minimal window size */
	if (td->cols < 1) td->cols = 1;
	if (td->rows < 1) td->rows = 1;

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

	/* verify the window is within the currently available screens */
	{
		GDHandle	gdNthDevice;
		Boolean		inside = false;
		
		gdNthDevice = GetDeviceList();
		
		while( gdNthDevice != nil )
		{
			if( TestDeviceAttribute( gdNthDevice, screenDevice ) )
			{
				if( TestDeviceAttribute( gdNthDevice, screenActive ) )
				{
					Point		topleft;
					Boolean		cornerIn = false;
					
					topleft.h = td->r.left;
					topleft.v = td->r.top;
					
					cornerIn = PtInRect( topleft, &(*gdNthDevice)->gdRect );
					if( cornerIn )
					{
						inside = true;
						break;
					}
				}
			}
			gdNthDevice = GetNextDevice( gdNthDevice );
		}
		
		if( !inside )
		{
			/* Verify the top */
			if (td->r.top > screen.bounds.bottom - td->size_hgt)
			{
				td->r.top = screen.bounds.bottom - td->size_hgt;
			}

			/* Verify the top */
			if (td->r.top < screen.bounds.top + GetMBarHeight())
			{
				td->r.top = screen.bounds.top + GetMBarHeight();
			}

			/* Verify the left */
			if (td->r.left > screen.bounds.right - td->size_wid)
			{
				td->r.left = screen.bounds.right - td->size_wid;
			}

			/* Verify the left */
			if (td->r.left < screen.bounds.left)
			{
				td->r.left = screen.bounds.left;
			}
		}
		
		/* Calculate bottom right corner */
		td->r.right = td->r.left + td->size_wid;
		td->r.bottom = td->r.top + td->size_hgt;
	}
	

	/* Assume no graphics */
	td->t->always_pict = FALSE;

#ifdef ANGBAND_LITE_MAC

	/* No graphics */

#else /* ANGBAND_LITE_MAC */

	/* Handle graphics */
	if (use_graphics && ((td == &data[0]) || (td == &data[6])))
	{
		td->t->always_pict = TRUE;
	}

#endif /* ANGBAND_LITE_MAC */

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
 * This should normally be followed by "term_data_resize()"
 */
static void term_data_resize(term_data *td)
{
	/* Actually resize the window */
	SizeWindow(td->w, td->size_wid, td->size_hgt, 0);
}



/*
 * Hack -- redraw a term_data
 */
static void term_data_redraw(term_data *td)
{
	/*
		We want to make a multi-threaded redraw implementation for MacOSX!
		So, lets use our locally defined thread routines to take use of multi-cpu machines.
		
		
	*/
	term *old = Term;

	/* Activate the term */
	Term_activate(td->t);

	/* Redraw the contents */
	Term_redraw();

	/* Flush the output */
	Term_fresh();

	/* Restore the old term */
	Term_activate(old);

	/* No need to redraw */
#ifdef TARGET_CARBON
	{
		RgnHandle		theRgn = NewRgn();
		GetWindowRegion( td->w, kWindowContentRgn, theRgn );
		ValidWindowRgn( (WindowRef)(td->w), theRgn );
		DisposeRgn( theRgn );
	}
#else
	ValidRect( &(td->w->portRect) );
#endif
}




#ifdef ANGBAND_LITE_MAC

/* No graphics */

#else /* ANGBAND_LITE_MAC */



/*
 * Lock a frame
 */
static void BenSWLockFrame(FrameRec *srcFrameP)
{
	PixMapHandle 		pixMapH;

	pixMapH = GetGWorldPixMap(srcFrameP->framePort);
	(void)LockPixels(pixMapH);
	HLockHi((Handle)pixMapH);
	srcFrameP->framePixHndl = pixMapH;
	srcFrameP->framePix = (PixMapPtr)*(Handle)pixMapH;
		
	pixMapH = GetGWorldPixMap(srcFrameP->bufferPort);
	(void)LockPixels(pixMapH);
	HLockHi((Handle)pixMapH);
	srcFrameP->bufferPixHndl = pixMapH;
	srcFrameP->bufferPix = (PixMapPtr)*(Handle)pixMapH;
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
		
	if (srcFrameP->bufferPort != NULL)
	{
		HUnlock((Handle)srcFrameP->bufferPixHndl);
		UnlockPixels(srcFrameP->bufferPixHndl);
	}

	srcFrameP->bufferPix = NULL;
}



static OSErr BenSWCreateGWorldFromPict(
	GWorldPtr *pictGWorld,
	GWorldPtr *bufferGWorld,
	PicHandle pictH,
	term_data *td )
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
		/*depth = 8; */
			
		/* Get GDH */
		theGDH = data[0].theGDH;

		/* Obtain size rectangle */
		pictRect = (**pictH).picFrame;
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
		*bufferGWorld = NULL;

		/* Get depth */
		depth = data[0].pixelDepth;
		/*depth = 8;*/
		
		/* Get GDH */
		theGDH = data[0].theGDH;

		/* Obtain size rectangle */
		pictRect.left = 0;
		pictRect.right = ((COL_MAP+1)+(66+1)) * 32;
		pictRect.top = 0;
		pictRect.bottom = td->tile_hgt;
		
		/*OffsetRect(&pictRect, -pictRect.left, -pictRect.top); */

		/* Create a GWorld */
		err = NewGWorld(&tempGWorld, depth, &pictRect, nil, 
						theGDH, noNewDevice);

		/* Success */
		if (err != noErr)
		{
			return (err);
		}

		/* Save pointer */
		*bufferGWorld = tempGWorld;
	}
	
	
	/* Success */
	return (0);
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
		DisposeGWorld(frameP->bufferPort);

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


/*
 * Init the global "frameP"
 */
static errr globe_init(term_data *td)
{
	OSErr err;
	
	GWorldPtr tempPictGWorldP;
	GWorldPtr tempPictBufferGWorldP;

	PicHandle newPictH;

	/* If we already initialized, then dispose of the previous objects */
	{
		globe_nuke();
	}
	
	/* Use window XXX XXX XXX */
	SetPort(GetWindowPort(data[0].w));


	/* Get the pict resource */
	switch( arg_graphics )
	{
		case GRAPHICS_ORIGINAL:
			{
				Rect pictRect;
				char grafpath[1024];
				char filepath[1024];
				
				newPictH = get_picture_from_file( ANGBAND_XTRA_GRAF_8x8 );
				
				gTileWidth = 8;
				gTileHeight = 8;
				
				pictRect = (**newPictH).picFrame;
				gTileCols = (pictRect.right - pictRect.left) / gTileWidth;
				gTileCols = (pictRect.bottom - pictRect.top) / gTileHeight;
				
				use_transparency = false;
				use_graphics = GRAPHICS_ORIGINAL;
				
				break;
			}
		case GRAPHICS_ADAM_BOLT:
			{
				Rect pictRect;
				char grafpath[1024];
				char filepath[1024];
				
				newPictH = get_picture_from_file( ANGBAND_XTRA_GRAF_16x16 );
				
				gTileWidth = 16;
				gTileHeight = 16;
				
				pictRect = (**newPictH).picFrame;
				gTileCols = (pictRect.right - pictRect.left) / gTileWidth;
				gTileCols = (pictRect.bottom - pictRect.top) / gTileHeight;
				
				use_transparency = true;
				use_graphics = GRAPHICS_ADAM_BOLT;
				
				break;
			}
		
		case GRAPHICS_NONE:
		default:
			{
				use_graphics = GRAPHICS_NONE;
				use_transparency = false;
			}
			return 0;
	}

	/* Analyze result */
	err = (newPictH ? 0 : -1);

	/* Oops */
	if (err == noErr)
	{
		/* Create GWorld */
		err = BenSWCreateGWorldFromPict( &tempPictGWorldP, 
										&tempPictBufferGWorldP,
										newPictH,
										td );

		/* Release resource */
		ReleaseResource((Handle)newPictH);
		
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
				frameP->bufferPort = tempPictBufferGWorldP;

				/* Lock it */
				BenSWLockFrame(frameP);
			}
		}
	}
	

	/* Result */
	return (err);
}




#endif /* ANGBAND_LITE_MAC */



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

	static RGBColor black = {0x0000,0x0000,0x0000};
	static RGBColor white = {0xFFFF,0xFFFF,0xFFFF};

#ifdef ANGBAND_LITE_MAC

	/* Make the window */
	td->w = NewWindow(0, &td->r, td->title, 0, noGrowDocProc, (WindowPtr)-1, 1, 0L);

#else /* ANGBAND_LITE_MAC */

	/* Make the window */
	td->w = NewCWindow(0, &td->r, td->title, 0, documentProc, (WindowPtr)-1, 1, 0L);

#endif /* ANGBAND_LITE_MAC */

	/* Activate the window */
	activate(td->w);

	/* Erase behind words */
	TextMode(srcCopy);

	/* Apply and Verify */
	term_data_check_font(td);
	term_data_check_size(td);

	/* Resize the window */
	term_data_resize(td);

#ifdef ANGBAND_LITE_MAC

	/* Prepare the colors (base colors) */
	BackColor(blackColor);
	ForeColor(whiteColor);

#else /* ANGBAND_LITE_MAC */

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

		/* Obtain the rect */
#ifdef TARGET_CARBON
		GetWindowBounds( (WindowRef)td->w, kWindowContentRgn, &globalRect );
#else
		globalRect = td->w->portRect;
		local_to_global( &globalRect );
#endif

		/* Obtain the proper GDH */
		mainGDH = GetMaxDevice(&globalRect);

		/* Extract GWorld and GDH */
		GetGWorld(&windowGWorld, &currentGDH);

		/* Obtain base pixmap */
		basePixMap = (**mainGDH).gdPMap;

		/* Save pixel depth */
		td->pixelDepth = (**basePixMap).pixelSize;

		/* Save Window GWorld */
		td->theGWorld = windowGWorld;

		/* Save Window GDH */
		td->theGDH = currentGDH;

		/* Save main GDH */
		td->mainSWGDH = mainGDH;
	}

#endif /* ANGBAND_LITE_MAC */
	{
		Rect		portRect;
		
#ifdef TARGET_CARBON
		GetWindowBounds( (WindowRef)td->w, kWindowContentRgn, &portRect );
		global_to_local( &portRect );
#else
		portRect = td->w->portRect;
#endif
		
		/* Clip to the window */
		ClipRect(&portRect);

		/* Erase the window */
		EraseRect(&portRect);

		/* Invalidate the window */
#ifdef TARGET_CARBON
		InvalWindowRect((WindowRef)(td->w), (const Rect *)(&portRect));
#else
		InvalRect( &portRect );
#endif
		/* Display the window if needed */
		if (td->mapped) ShowWindow(td->w);

		/* Hack -- set "mapped" flag */
		t->mapped_flag = td->mapped;

		/* Forget color */
		td->last = -1;
	}
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
	static byte current_graphics = -1;

	/* Reset color */
	td->last = -1;

#ifdef ANGBAND_LITE_MAC

	/* Nothing */
	
#else /* ANGBAND_LITE_MAC */

	/* Handle sound */
	if (use_sound != arg_sound)
	{
		/* Apply request */
		use_sound = arg_sound;
	}

	/* We are comparing a byte with a boolean, not good! */
	
	/* Handle graphics */
	if (((td == &data[0]) || (td == &data[6])) && (current_graphics != arg_graphics))
	{
		/* Initialize graphics */
		if (globe_init(td) != 0)
		{
			plog("Cannot initialize graphics!");
			arg_graphics = GRAPHICS_NONE;
			use_graphics = GRAPHICS_NONE;
		}

		/* Apply request */
		current_graphics = arg_graphics;
		use_graphics = current_graphics;

		/* Apply and Verify */
		term_data_check_size(td);

		/* Resize the window */
		term_data_resize(td);

		/* Reset visuals */
		reset_visuals();
	}

#endif /* ANGBAND_LITE_MAC */

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

#ifdef ANGBAND_LITE_MAC

		/* Nothing */

#else /* ANGBAND_LITE_MAC */

		/* Make a sound */
		case TERM_XTRA_SOUND:
		{
			/* play sound effect */
			short	volume = ( gSoundVolume > 0 ?
				 (SOUND_VOLUME_MAX*gSoundVolume)/100 : 0 );
			play_sound( v, volume, true );
			
			/* Success */
			return (0);
		}

#endif /* ANGBAND_LITE_MAC */

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

		/* React to changes */
		case TERM_XTRA_REACT:
		{
			/* React to changes */
			return (Term_xtra_mac_react());
		}

		/* Delay (milliseconds) */
		case TERM_XTRA_DELAY:
		{
			/* If needed */
			if (v > 0)
			{
				EventRecord tmp;
				UInt32 ticks;
				
				/* Convert milliseconds to ticks */
				ticks = (v * 60L) / 1000;
				
				/* Hack - block for those ticks */
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
 * We are allowed to use "Term_grab()" to determine
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
	term_data_color(td, (a & 0x0F));

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
static errr Term_pict_mac(int x, int y, int n, const byte *ap, const char *cp, const byte *tap, const char *tcp)
{
	int i;
	Rect r2;
	bool use_buffer = false;
	term_data *td = (term_data*)(Term->data);
	GDHandle saveGDevice;
	GWorldPtr saveGWorld;
	
	/* Save GWorld */
	GetGWorld(&saveGWorld, &saveGDevice);
	
	if( n > 200 )
	{
		/* Use the row buffer if we are drawing more than one cell in the row */
		use_buffer = true;
		
		/* Destination rectangle */
		r2.left = x * td->tile_wid + td->size_ow1;
		r2.right = r2.left + td->tile_wid;
		r2.top = 0;
		r2.bottom = r2.top + td->tile_hgt;
	
		/* We must change to the buffer port for output */
		/* Activate */
		SetGWorld(frameP->bufferPort, nil);
		
		/* Instantiate font */
		TextFont(td->font_id);
		TextSize(td->font_size);
		TextFace(td->font_face);
		
		/* Restore colors */
		BackColor(blackColor);
		ForeColor(whiteColor);
		
#ifdef TARGET_CARBON
		{
			Rect		portRect;
			
			GetPortBounds( frameP->bufferPort, &portRect );
			EraseRect( &portRect );
		}
#else
		EraseRect( &td->w->portRect );
#endif
	}
	else
	{
		/* no buffering, so we use the normal current port */
		use_buffer = false;
		
		/* Destination rectangle */
		r2.left = x * td->tile_wid + td->size_ow1;
		r2.right = r2.left + td->tile_wid;
		r2.top = y * td->tile_hgt + td->size_oh1;
		r2.bottom = r2.top + td->tile_hgt;
	}
		
	/* Scan the input */
	for (i = 0; i < n; i++)
	{
		bool done = FALSE;

		byte a = ap[i];
		char c = cp[i];
		
		byte ta = tap[i];
		char tc = tcp[i];

#ifdef ANGBAND_LITE_MAC

		/* Nothing */

#else /* ANGBAND_LITE_MAC */

		/* Graphics -- if Available and Needed */
		if (use_graphics && ((td == &data[0]) || (td == &data[6])) &&
		    ((byte)a & 0x80) && ((byte)c & 0x80))
		{
			int col, row;
			Rect r1;
			
			int terrain_col, terrain_row;
			Rect terrain_rect;

			/* Row and Col */
			row = ((byte)a & 0x7F) % gTileRows;
			col = ((byte)c & 0x7F) % gTileCols;

			terrain_row = ((byte)ta & 0x7F) % gTileRows;
			terrain_col = ((byte)tc & 0x7F) % gTileCols;
			
			/* Source rectangle */
			r1.left = col * gTileWidth;
			r1.top = row * gTileHeight;
			r1.right = r1.left + gTileWidth;
			r1.bottom = r1.top + gTileHeight;

			/* Hardwire CopyBits */
			BackColor(whiteColor);
			ForeColor(blackColor);

			/* Draw the picture */
			{
				int lock_pixels = 0;
				BitMapPtr	srcBitMap = (BitMapPtr)(frameP->framePix);
				BitMapPtr	destBitMap = 0L;
				
#ifdef TARGET_CARBON
				if( use_buffer )
				{
					destBitMap = (BitMapPtr)(frameP->bufferPix);
				}
				else
				{
					PixMapHandle	bMap = 0L;
					LockPixels( GetPortPixMap(GetWindowPort(td->w)) );
					bMap = GetPortPixMap(GetWindowPort(td->w));
					destBitMap = *bMap;
					lock_pixels = 1;
					/*destBitMap = GetPortBitMapForCopyBits( (CGrafPtr)(td->w) ); */
				}
#else
				if( use_buffer )
				{
					destBitMap = (BitMapPtr)(frameP->bufferPix);
				}
				else
				{
					destBitMap = (BitMapPtr)&(td->w->portBits);
				}
#endif

				/* Terrain rectangle */
				terrain_rect.left = terrain_col * gTileWidth;
				terrain_rect.top = terrain_row * gTileHeight;
				terrain_rect.right = terrain_rect.left + gTileWidth;
				terrain_rect.bottom = terrain_rect.top + gTileHeight;
				
				/* draw terrain */
				CopyBits( srcBitMap, destBitMap, &terrain_rect, &r2, srcCopy, NULL );
				
				/* draw transparent tile */
				BackColor(blackColor);
				CopyBits( srcBitMap, destBitMap, &r1, &r2, transparent, NULL );
				
				if( lock_pixels == 1 )
					UnlockPixels( GetPortPixMap(GetWindowPort(td->w)) );
			}
			
			/* Restore colors */
			BackColor(blackColor);
			ForeColor(whiteColor);

			/* Forget color */
			td->last = -1;

			/* Done */
			done = TRUE;
		}

#endif /* ANGBAND_LITE_MAC */

		/* Normal */
		if (!done)
		{
			int xp, yp;

			/* Erase */
			EraseRect(&r2);

			/* Set the color */
			term_data_color(td, (a & 0x0F));

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
	
	if( use_buffer )
	{
		int lock_pixels = 0;
		Rect		srcRect;
		Rect		destRect;
		/* Now we blast the buffer pixmap onto the screen in the right place */
		BitMapPtr	srcBitMap = (BitMapPtr)(frameP->bufferPix);
#ifdef TARGET_CARBON
		BitMapPtr	destBitMap = 0L;
		PixMapHandle	bMap = 0L;
		LockPixels( GetPortPixMap(GetWindowPort(td->w)) );
		bMap = GetPortPixMap(GetWindowPort(td->w));
		destBitMap = *bMap;
		lock_pixels = 1;
		/*BitMapPtr	destBitMap = GetPortBitMapForCopyBits((CGrafPtr)(td->w));*/
#else
		BitMapPtr	destBitMap = (BitMapPtr)&(td->w->portBits);
#endif

		srcRect.left = x * td->tile_wid + td->size_ow1;
		srcRect.top = 0;
		srcRect.right = srcRect.left + (td->tile_wid * n);
		srcRect.bottom = td->tile_hgt;
		
		destRect.left = x * td->tile_wid + td->size_ow1;
		destRect.right = destRect.left + (td->tile_wid * n);
		destRect.top = y * td->tile_hgt + td->size_oh1;
		destRect.bottom = destRect.top + td->tile_hgt;
		
		/* Restore GWorld */
		SetGWorld(saveGWorld, saveGDevice);
		
		/* Hardwire CopyBits */
		BackColor(whiteColor);
		ForeColor(blackColor);
		
		CopyBits( srcBitMap, destBitMap, &srcRect, &destRect, srcCopy, NULL );

		/* Restore colors */
		BackColor(blackColor);
		ForeColor(whiteColor);
		
		if( lock_pixels == 1 )
			UnlockPixels( GetPortPixMap(GetWindowPort(td->w)) );
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

	/* Erase with "black space" */
	td->t->attr_blank = TERM_DARK;
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
 * Code by: Maarten Hazewinkel (mmhazewi@cs.ruu.nl)
 *
 * This function does not appear to work correctly with System 6.
 */
static void SetupAppDir(void)
{
	OSErr err = noErr;
	ProcessSerialNumber psn;
	ProcessInfoRec info;
	Str255 myName;
	FSSpec mySpec;
	char errString[255];
	
	err = GetCurrentProcess( &psn );
	if( err == noErr )
	{
		info.processInfoLength = sizeof(ProcessInfoRec);
		info.processName = myName;
		info.processAppSpec = &mySpec;
		
		err = GetProcessInformation( &psn, &info );
		if( err == noErr )
		{
			app_vol = info.processAppSpec->vRefNum;
			app_dir = info.processAppSpec->parID;
		}
	}
	
	/* Set the current working directory to that location */
	err = HSetVol(NULL, app_vol, app_dir);
	if (err != noErr)
	{
		strnfmt(errString, 255, "Fatal HSetVol Error #%d.\r Exiting.", err);
		mac_warning(errString);
		ExitToShell();
	}
}




/*
 * Global "preference" file pointer
 */
static FILE *fff;

/*
 * Read a "short" from the file
 */
static int getshort(void)
{
	int x = 0;
	char buf[256];
	if (0 == my_fgets(fff, buf, 256)) x = atoi(buf);
	return (x);
}

/*
 * Read a "byte" from the file
 */
static int getbyte(void)
{
	byte x = 0;
	unsigned char buf[256];
	if (0 == my_fgets(fff, (char*)buf, 256)) x = buf[0];
	return (x);
}

/*
 * Dump a "short" to the file
 */
static void putshort(int x)
{
	fprintf(fff, "%d\n", x);
}

/*
 * Dump a "byte" to the file
 */
static void putbyte(byte x)
{
	fprintf(fff, "%c\n", x);
}



/*
 * Write the "preference" data to the current "file"
 */
static void save_prefs(void)
{
	int i;

	term_data *td;
	
	/*** The current version ***/
	putshort(VER_MAJOR);
	putshort(VER_MINOR);
	putshort(VER_PATCH);
	putshort(VER_EXTRA);

	/* Dump */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* Access */
		td = &data[i];

		putshort(td->mapped);

		putshort(td->font_id);
		putshort(td->font_size);
		putshort(td->font_face);

		putshort(td->cols);
		putshort(td->rows);

		putshort(td->r.left);
		putshort(td->r.top);
	}
	
	putshort(arg_graphics);
	putshort(arg_sound);
}


/*
 * Load the preferences from the current "file"
 *
 * XXX XXX XXX Being able to undefine various windows is
 * slightly bizarre, and may cause problems.
 */
static void load_prefs(void)
{
	int i;

	int old_major, old_minor, old_patch, old_extra;

	term_data *td;


	/*** Version information ***/

	/* Preferences version */
	old_major = getshort();
	old_minor = getshort();
	old_patch = getshort();
	old_extra = getshort();

	/* Hack -- Verify or ignore */
	if ((old_major != VER_MAJOR) ||
	    (old_minor != VER_MINOR) ||
	    (old_patch != VER_PATCH) ||
	    (old_extra != VER_EXTRA))
	{
		/* Message */
		mac_warning("Ignoring old preferences.");

		/* Ignore */
		return;
	}
	
	/* Windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* Access */
		td = &data[i];

		td->mapped = getshort();

		td->font_id = getshort();
		td->font_size = getshort();
		td->font_face = getshort();

		td->cols = getshort();
		td->rows = getshort();

		td->r.left = getshort();
		td->r.top = getshort();

		/* Done */
		if (feof(fff)) break;
	}
	
	arg_graphics = getshort();
	arg_sound = getshort();
}




/*
 * Hack -- default data for a window
 */
static void term_data_hack(term_data *td)
{
	short fid;

#ifdef TARGET_CARBON
	/* Default to Monaco font */
	fid = FMGetFontFamilyFromName( "\pmonaco" );
#else
	GetFNum("\pmonaco", &fid);
#endif

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
 * We attempt to use "FindFolder()" to track down the preference file,
 * but if this fails, for any reason, we will try the "SysEnvirons()"
 * method, which may work better with System 6.
 */
static void init_windows(void)
{
	int i, b = 0;

	term_data *td;

	bool oops;


	/*** Default values ***/

	/* Initialize (backwards) */
	for (i = MAX_TERM_DATA - 1; i >= 0; i--)
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
	load_pref_file();


	/*** Instantiate ***/

	/* Main window */
	td = &data[0];

	/* Many keys */
	td->keys = 1024;

	/* Start visible */
	td->mapped = TRUE;

	/* Link (backwards, for stacking order) */
	for (i = MAX_TERM_DATA - 1; i >= 0; i--)
	{
		term_data_link(i);
	}

	/* Main window */
	td = &data[0];

	/* Main window */
	Term_activate(td->t);
}


/*
 * Exit the program
 */
static void save_prefs_carbon( void )
{
	/* Creating a plist prefs file using the new Core Preferences Services */
	short			theShortValue = 0;
	
	/* Version info */
	{
		CFStringRef		prefVersionMajorKey = CFSTR( "version.major" );
		CFStringRef		prefVersionMinorKey = CFSTR( "version.minor" );
		CFStringRef		prefVersionPatchKey = CFSTR( "version.patch" );
		CFStringRef		prefVersionExtraKey = CFSTR( "version.extra" );
		
		CFNumberRef		prefVersionMajorValue;
		CFNumberRef		prefVersionMinorValue;
		CFNumberRef		prefVersionPatchValue;
		CFNumberRef		prefVersionExtraValue;
		
		theShortValue = VER_MAJOR; 
		prefVersionMajorValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
		theShortValue = VER_MINOR; 
		prefVersionMinorValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
		theShortValue = VER_PATCH; 
		prefVersionPatchValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
		theShortValue = VER_EXTRA; 
		prefVersionExtraValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
	
		CFPreferencesSetAppValue( prefVersionMajorKey, prefVersionMajorValue, kCFPreferencesCurrentApplication );
		CFPreferencesSetAppValue( prefVersionMinorKey, prefVersionMinorValue, kCFPreferencesCurrentApplication );
		CFPreferencesSetAppValue( prefVersionPatchKey, prefVersionPatchValue, kCFPreferencesCurrentApplication );
		CFPreferencesSetAppValue( prefVersionExtraKey, prefVersionExtraValue, kCFPreferencesCurrentApplication );
		
		/* Cleanup CoreFoundation property list value refs */
#if 0
		CFRelease( prefVersionMajorKey );
		CFRelease( prefVersionMinorKey );
		CFRelease( prefVersionPatchKey );
		CFRelease( prefVersionExtraKey );
		
		CFRelease( prefVersionMajorValue );
		CFRelease( prefVersionMinorValue );
		CFRelease( prefVersionPatchValue );
		CFRelease( prefVersionExtraValue );
#endif /* 0 */
	}
	
	/* Misc info */
	{
		CFStringRef		prefGraphicsKey = CFSTR( "arg.graphics" );
		CFStringRef		prefSoundKey = CFSTR( "arg.sound" );
	
		CFNumberRef		prefGraphicsValue;
		CFNumberRef		prefSoundValue;
	
		theShortValue = arg_graphics; 
		prefGraphicsValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
		theShortValue = arg_sound; 
		prefSoundValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
	
		CFPreferencesSetAppValue( prefGraphicsKey, prefGraphicsValue, kCFPreferencesCurrentApplication );
		CFPreferencesSetAppValue( prefSoundKey, prefSoundValue, kCFPreferencesCurrentApplication );
		
		/* Cleanup CoreFoundation property list value refs */
#if 0
		CFRelease( prefGraphicsKey );
		CFRelease( prefSoundKey );
		
		CFRelease( prefGraphicsValue );
		CFRelease( prefSoundValue );
#endif /* 0 */
	}
	
	/* store term/window prefs */
	{
		/* Create an array collection */
		int i;
		CFMutableArrayRef		theTermArray = CFArrayCreateMutable( 
														NULL, 
														(CFIndex)MAX_TERM_DATA, 
														&kCFTypeArrayCallBacks );
		
		for( i=0; i < MAX_TERM_DATA; i++ )
		{
			CFMutableDictionaryRef myDictionary = CFDictionaryCreateMutable(
														NULL, 
														0, 
														&kCFCopyStringDictionaryKeyCallBacks,
														&kCFTypeDictionaryValueCallBacks );
			
			CFStringRef		prefTermMappedKey = CFSTR( "term.mapped" );
			CFStringRef		prefTermFontIDKey = CFSTR( "term.font_id" );
			CFStringRef		prefTermFontSizeKey = CFSTR( "term.font_size" );
			CFStringRef		prefTermFontFaceKey = CFSTR( "term.font_face" );
			CFStringRef		prefTermColsKey = CFSTR( "term.cols" );
			CFStringRef		prefTermRowsKey = CFSTR( "term.rows" );
			CFStringRef		prefTermLeftKey = CFSTR( "term.left" );
			CFStringRef		prefTermTopKey = CFSTR( "term.top" );
			
			CFNumberRef		prefTermMappedValue;
			CFNumberRef		prefTermFontIDValue;
			CFNumberRef		prefTermFontSizeValue;
			CFNumberRef		prefTermFontFaceValue;
			CFNumberRef		prefTermColsValue;
			CFNumberRef		prefTermRowsValue;
			CFNumberRef		prefTermLeftValue;
			CFNumberRef		prefTermTopValue;
			
			/* Access */
			term_data		*td = &data[i];
		
			theShortValue = td->mapped;
			prefTermMappedValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			theShortValue = td->font_id;
			prefTermFontIDValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			theShortValue = td->font_size;
			prefTermFontSizeValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			theShortValue = td->font_face;
			prefTermFontFaceValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			theShortValue = td->cols;
			prefTermColsValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			theShortValue = td->rows;
			prefTermRowsValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			theShortValue = td->r.left;
			prefTermLeftValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			theShortValue = td->r.top;
			prefTermTopValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberShortType, &theShortValue );
			
			CFDictionaryAddValue( myDictionary, prefTermMappedKey, prefTermMappedValue );
			CFDictionaryAddValue( myDictionary, prefTermFontIDKey, prefTermFontIDValue );
			CFDictionaryAddValue( myDictionary, prefTermFontSizeKey, prefTermFontSizeValue );
			CFDictionaryAddValue( myDictionary, prefTermFontFaceKey, prefTermFontFaceValue );
			CFDictionaryAddValue( myDictionary, prefTermColsKey, prefTermColsValue );
			CFDictionaryAddValue( myDictionary, prefTermRowsKey, prefTermRowsValue );
			CFDictionaryAddValue( myDictionary, prefTermLeftKey, prefTermLeftValue );
			CFDictionaryAddValue( myDictionary, prefTermTopKey, prefTermTopValue );
			
			CFArrayAppendValue( theTermArray, (void *)myDictionary );
		}
		
		CFPreferencesSetAppValue( CFSTR("terms"), theTermArray, kCFPreferencesCurrentApplication );
		
		/* Cleanup CoreFoundation property list value refs */
		/* CFRelease( theTermArray ); */
	}
	
	CFPreferencesAppSynchronize( kCFPreferencesCurrentApplication );
	
	 
}

static void load_prefs_carbon( void )
{
	/* Load Version Info */
	CFNumberRef		prefVersionMajorValue = CFPreferencesCopyAppValue( CFSTR("version.major"), kCFPreferencesCurrentApplication );
	CFNumberRef		prefVersionMinorValue = CFPreferencesCopyAppValue( CFSTR("version.minor"), kCFPreferencesCurrentApplication );
	CFNumberRef		prefVersionPatchValue = CFPreferencesCopyAppValue( CFSTR("version.patch"), kCFPreferencesCurrentApplication );
	CFNumberRef		prefVersionExtraValue = CFPreferencesCopyAppValue( CFSTR("version.extra"), kCFPreferencesCurrentApplication );
	/* Load Misc Info */
	CFNumberRef		prefGraphicsValue = CFPreferencesCopyAppValue( CFSTR("arg.graphics"), kCFPreferencesCurrentApplication );
	CFNumberRef		prefSoundValue = CFPreferencesCopyAppValue( CFSTR("arg.sound"), kCFPreferencesCurrentApplication );
	/* Load Term/Window Info */
	CFArrayRef		prefTermArray = CFPreferencesCopyAppValue( CFSTR("terms"), kCFPreferencesCurrentApplication );
	
	short			old_major, old_minor, old_patch, old_extra;
	short			old_graphics, old_sound;
	int	i;
	
	if( prefVersionMajorValue == NULL ||
		prefVersionMinorValue == NULL ||
		prefVersionPatchValue == NULL ||
		prefVersionExtraValue == NULL )
	{
		/* Preferences have not been saved yet */
		mac_warning("Preferences have not been found.");
		
		/* Ignore */
		return;
	}
	
	CFNumberGetValue( prefVersionMajorValue, kCFNumberShortType, &old_major );
	CFNumberGetValue( prefVersionMinorValue, kCFNumberShortType, &old_minor );
	CFNumberGetValue( prefVersionPatchValue, kCFNumberShortType, &old_patch );
	CFNumberGetValue( prefVersionExtraValue, kCFNumberShortType, &old_extra );
	
	/* Hack -- Verify or ignore */
	if ((old_major != VER_MAJOR) ||
	    (old_minor != VER_MINOR) ||
	    (old_patch != VER_PATCH) ||
	    (old_extra != VER_EXTRA))
	{
		/* Message */
		mac_warning("Ignoring old preferences.");

		/* Ignore */
		return;
	}
	
	CFNumberGetValue( prefGraphicsValue, kCFNumberShortType, &old_graphics );
	CFNumberGetValue( prefSoundValue, kCFNumberShortType, &old_sound );
	
	/* Windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		CFDictionaryRef		termDictionary = CFArrayGetValueAtIndex( prefTermArray, i );
		CFNumberRef			prefTermMappedValue = CFDictionaryGetValue( termDictionary, CFSTR("term.mapped" ) );
		CFNumberRef			prefTermFontIDValue = CFDictionaryGetValue( termDictionary, CFSTR("term.font_id" ) );
		CFNumberRef			prefTermFontSizeValue = CFDictionaryGetValue( termDictionary, CFSTR("term.font_size" ) );
		CFNumberRef			prefTermFontFaceValue = CFDictionaryGetValue( termDictionary, CFSTR("term.font_face" ) );
		CFNumberRef			prefTermColsValue = CFDictionaryGetValue( termDictionary, CFSTR("term.cols" ) );
		CFNumberRef			prefTermRowsValue = CFDictionaryGetValue( termDictionary, CFSTR("term.rows" ) );
		CFNumberRef			prefTermLeftValue = CFDictionaryGetValue( termDictionary, CFSTR("term.left" ) );
		CFNumberRef			prefTermTopValue = CFDictionaryGetValue( termDictionary, CFSTR("term.top" ) );
		
		short		old_mapped, old_font_id, old_font_size, old_font_face, old_cols, old_rows, old_left, old_top;
		
		/* Access */
		term_data	*td = &data[i];
		
		CFNumberGetValue( prefTermMappedValue, kCFNumberShortType, &old_mapped );
		CFNumberGetValue( prefTermFontIDValue, kCFNumberShortType, &old_font_id );
		CFNumberGetValue( prefTermFontSizeValue, kCFNumberShortType, &old_font_size );
		CFNumberGetValue( prefTermFontFaceValue, kCFNumberShortType, &old_font_face );
		CFNumberGetValue( prefTermColsValue, kCFNumberShortType, &old_cols );
		CFNumberGetValue( prefTermRowsValue, kCFNumberShortType, &old_rows );
		CFNumberGetValue( prefTermLeftValue, kCFNumberShortType, &old_left );
		CFNumberGetValue( prefTermTopValue, kCFNumberShortType, &old_top );

		td->mapped = old_mapped;

		td->font_id = old_font_id;
		td->font_size = old_font_size;
		td->font_face = old_font_face;

		td->cols = old_cols;
		td->rows = old_rows;

		td->r.left = old_left;
		td->r.top = old_top;
		
		/* Cleanup CoreFoundation property list value refs */
		/* CFRelease( termDictionary ); */
	}
	
	arg_graphics = old_graphics;
	arg_sound = old_sound;
	
	/* Cleanup CoreFoundation property list value refs */
#if 0
	CFRelease( prefVersionMajorValue );
	CFRelease( prefVersionMinorValue );
	CFRelease( prefVersionPatchValue );
	CFRelease( prefVersionExtraValue );
	
	CFRelease( prefGraphicsValue );
	CFRelease( prefSoundValue );
	
	CFRelease( prefTermArray );
#endif /* 0 */
}

static void save_pref_file(void)
{
#ifdef TARGET_CARBON
	save_prefs_carbon();
#else
	bool oops;


	/* Assume failure */
	oops = TRUE;

	/* Assume failure */
	fff = NULL;

#if defined(MACINTOSH) && !defined(applec)
	/* Text file */
	_ftype = 'TEXT';
#endif


#ifdef USE_SFL_CODE

	/* Block */
	if (TRUE)
	{
		OSErr	err;
		short	vref;
		long	dirID;
		char	foo[128];

		/* Find the folder */
		err = FindFolder(kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
		                 &vref, &dirID);

		/* Success */
		if (!err)
		{
			/* Extract a path name */
			PathNameFromDirID(dirID, vref, (StringPtr)foo);

			/* Convert the string */
			ptocstr((StringPtr)foo);

			/* Append the preference file name */
			strcat(foo, "Angband Preferences");

			/* Open the preference file */
			fff = fopen(foo, "w");

			/* Success */
			oops = FALSE;
		}
	}

#endif /* USE_SFL_CODE */

	

	/* Save preferences */
	if (fff)
	{
		/* Write the preferences */
		save_prefs();

		/* Close it */
		my_fclose(fff);
	}
#endif /* TARGET_CARBON */
}

static void load_pref_file(void)
{
#ifdef TARGET_CARBON
	load_prefs_carbon();
#else
	/* Assume failure */
	oops = TRUE;

	/* Assume failure */
	fff = NULL;

#ifdef USE_SFL_CODE

	/* Block */
	if (TRUE)
	{
		OSErr	err;
		short	vref;
		long	dirID;
		char	foo[128];

		/* Find the folder */
		err = FindFolder(kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
		                 &vref, &dirID);

		/* Success */
		if (err == noErr)
		{
			/* Extract a path name */
			PathNameFromDirID(dirID, vref, (StringPtr)foo);

			/* Convert the string */
			ptocstr((StringPtr)foo);

			/* Append the preference file name */
			strcat(foo, "Angband Preferences");

			/* Open the preference file */
			fff = fopen(foo, "r");

			if( fff )
			{
				/* Success */
				oops = FALSE;
			}
			else
			{
				oops = TRUE;
			}
		}
	}

#endif /* USE_SFL_CODE */

	

	/* Load preferences */
	if (fff)
	{
		/* Load a real preference file */
		load_prefs();

		/* Close the file */
		my_fclose(fff);
	}
#endif	/* TARGET_CARBON */
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
	flush();

	/* Play a game */
	play_game(TRUE);

	/* Hack -- quit */
	quit(NULL);
}


/*
 * Handle menu: "File" + "Open"
 */
static void do_menu_file_open(bool all)
{
	int err;
	BitMap screen;
	
	/* Window location */
#ifdef TARGET_CARBON						
	GetQDGlobalsScreenBits( &screen );
#else
	screen = qd.screenBits;
#endif	

	/* Allow "all" files */
	if (all)
	{
		/* Get any file */
		err = ChooseFile( (unsigned char*)savefile, NULL, 0 );
	}

	/* Allow "save" files */
	else
	{
		OSType	types[1];
		types[0] = 'SAVE';
		
		err = ChooseFile( (unsigned char*)savefile, types, 1 );
	}

	/* Allow cancel */
	if (err != noErr) return;

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
 * Create menus 131, 132, 133, 134, 135, 136, 137
 *
 * The standard menus are:
 *
 *   Apple (128) =   { About, -, ... }
 *   File (129) =    { New,Open,Import,Close,Save,-,Exit,Quit }
 *   Edit (130) =    { Undo, Cut, Copy, Paste, Delete }   (?)
 *   Font (131) =    { Bold, Extend, -, Monaco, ..., -, ... }
 *   Size (132) =    { ... }
 *   Window (133) =  { Angband, Mirror, Recall, Choice,
 *                     Term-4, Term-5, Term-6, Term-7 }
 *   Special (134) = { arg_sound, arg_graphics->137, arg_music->138, -,
 *                     arg_fiddle, arg_wizard }
 *   Tile Width (135) = {...}
 *   Tile Height (136) = {...}
 *   Graphics (137) = { Text Only, Original, Adam Bolt }
 */
 
static void init_menubar(void)
{
	int i, n;

	Rect r;

	WindowPtr tmpw;

	MenuHandle 	menu_apple, 
				menu_file, 
				menu_edit, 
				menu_font, 
				menu_size, 
				menu_window, 
				menu_special,
				menu_tilewidth,
				menu_tileheight,
				menu_graphics,
				menu_music;

	
	{
		/* Get the "apple" menu */
		menu_apple = GetMenu(128);

		/* Insert the menu */
		InsertMenu(menu_apple, 0);

		/* Add the DA's to the "apple" menu */
		AppendResMenu(menu_apple, 'DRVR');
	}
	
	
	{
		/* Get the "File" menu */
		/* menu_file = GetMenu(129); */
		
		menu_file = NewMenu( 129, "\pFile" );
		
		/* Add New */
		AppendMenu( menu_file, "\pNew/N" );
		
		/* Add Open */
		AppendMenu( menu_file, "\pOpen/O" );
		
		/* Add Import */
		AppendMenu( menu_file, "\pImport/I" );

		/* Add Close */
		AppendMenu( menu_file, "\pClose/W" );

		/* Add Save */
		AppendMenu( menu_file, "\pSave/S" );

		/* Add --- */
		AppendMenu( menu_file, "\p-" );

		/* Add Exit */
		AppendMenu( menu_file, "\pExit/X" );

		/* Add Quit */
		AppendMenu( menu_file, "\pQuit/Q" );


		/* Insert the menu */
		InsertMenu(menu_file, 0);
	}
	
	{
		/* Get the "Edit" menu */
		/* menu_edit = GetMenu(130); */
		menu_edit = NewMenu( 130, "\pEdit" );
		
		/* Add Undo */
		AppendMenu( menu_edit, "\p(Undo/Z" );
		/* Add --- */
		AppendMenu( menu_edit, "\p-" );
		/* Add Cut */
		AppendMenu( menu_edit, "\p(Cut/X" );
		/* Add Copy */
		AppendMenu( menu_edit, "\p(Copy/C" );
		/* Add Paste */
		AppendMenu( menu_edit, "\p(Paste/V" );
		/* Add Delete */
		AppendMenu( menu_edit, "\p(Delete" );
		
		/* Insert the menu */
		InsertMenu(menu_edit, 0);
	}
	
	{
		/* Make the "Font" menu */
		menu_font = NewMenu(131, "\pFont");

		/* Insert the menu */
		InsertMenu(menu_font, 0);

		/* Add "bold" */
		AppendMenu(menu_font, "\pBold");

		/* Add "wide" */
		AppendMenu(menu_font, "\pWide");

		/* Add a separator */
		AppendMenu(menu_font, "\p-");

		/* Fake window */
		r.left = r.right = r.top = r.bottom = 0;
	
		/* Make the fake window */
		tmpw = NewWindow(0, &r, "\p", false, documentProc, 0, 0, 0);
	
		/* Activate the "fake" window */
		SetPort(GetWindowPort(tmpw));
	
		/* Default mode */
		TextMode( srcCopy );
	
		/* Default size */
		TextSize(12);
	
		/* Add the fonts to the menu */
		AppendResMenu(menu_font, 'FONT');
	
		/* Size of menu */
		n = CountMenuItems(menu_font);
	
		/* Scan the menu */
		for (i = n; i >= 4; i--)
		{
			Str255 tmpName;
			short fontNum;
	
			/* Acquire the font name */
			/* GetMenuItemText(m, i, tmpName); */
			GetMenuItemText(menu_font, i, tmpName);
	
			/* Acquire the font index */
			GetFNum(tmpName, &fontNum);
	
			/* Apply the font index */
			TextFont(fontNum);
	
			/* Remove non-mono-spaced fonts */
			if ((CharWidth('i') != CharWidth('W')) || (CharWidth('W') == 0))
			{
				/* Delete the menu item XXX XXX XXX */
				/* DeleteMenuItem(m, i); */
				DeleteMenuItem(menu_font, i);
			}
		}
	
		/* Destroy the old window */
		DisposeWindow(tmpw);

		/* Add a separator */
		AppendMenu(menu_font, "\p-");

		/* Add the fonts to the menu */
		AppendResMenu(menu_font, 'FONT');
	}
	
	{
		/* Make the "Size" menu */
		menu_size = NewMenu(132, "\pSize");

		/* Insert the menu */
		InsertMenu(menu_size, 0);

		/* Add some sizes (stagger choices) */
		for (i = 8; i <= 32; i += ((i / 16) + 1))
		{
			Str15 buf;
			
			/* Textual size */
			strnfmt((char*)buf + 1, 15, "%d", i);
			buf[0] = strlen((char*)buf + 1);

			/* Add the item */
			AppendMenu(menu_size, buf);
		}
	}

	{
		/* Make the "Windows" menu */
		menu_window = NewMenu(133, "\pWindows");

		/* Insert the menu */
		InsertMenu(menu_window, 0);

		/* Default choices */
		for (i = 0; i < MAX_TERM_DATA; i++)
		{
			Str15 buf;
			
			/* Describe the item */
			strnfmt((char*)buf + 1, 15, "%.15s", angband_term_name[i]);
			buf[0] = strlen((char*)buf + 1);

			/* Add the item */
			AppendMenu(menu_window, buf);

			/* Command-Key shortcuts */
			if (i < 8) SetItemCmd(menu_window, i + 1, '0' + i);
		}
	}
	
	{
		/* Make the "Special" menu */
		menu_special = NewMenu(134, "\pSpecial");

		/* Insert the menu */
		InsertMenu(menu_special, 0);

		/* Append the choices */
		AppendMenu(menu_special, "\parg_sound");
		AppendMenu(menu_special, "\parg_graphics");
		AppendMenu(menu_special, "\parg_muisic");
		AppendMenu(menu_special, "\p-");
		AppendMenu(menu_special, "\parg_fiddle");
		AppendMenu(menu_special, "\parg_wizard");
	}

	{
		/* Make the "TileWidth" menu */
		menu_tilewidth = NewMenu(135, "\pTileWidth");

		/* Insert the menu */
		InsertMenu(menu_tilewidth, 0);

		/* Add some sizes */
		for (i = 4; i <= 32; i++)
		{
			Str15 buf;
			
			/* Textual size */
			strnfmt((char*)buf + 1, 15, "%d", i);
			buf[0] = strlen((char*)buf + 1);

			/* Append item */
			AppendMenu(menu_tilewidth, buf);
		}
	}
	
	{
		/* Make the "TileHeight" menu */
		menu_tileheight = NewMenu(136, "\pTileHeight");

		/* Insert the menu */
		InsertMenu(menu_tileheight, 255);

		/* Add some sizes */
		for (i = 4; i <= 32; i++)
		{
			Str15 buf;

			/* Textual size */
			strnfmt((char*)buf + 1, 15, "%d", i);
			buf[0] = strlen((char*)buf + 1);

			/* Append item */
			AppendMenu(menu_tileheight, buf);
		}
	}
	
	{
		/* Make the "Graphics" menu */
		menu_graphics = NewMenu(137, "\parg_graphics");
		
		/* Append the choices */
		AppendMenu(menu_graphics, "\pText Only" );
		AppendMenu(menu_graphics, "\pOrignial" );
		AppendMenu(menu_graphics, "\pAdam Bolt" );
		
		/* Insert SubMenu */
		InsertMenu( menu_graphics, -1 );
		/* This call is only support with AppearanceLib 1.0 or greater */
		SetMenuItemHierarchicalID( menu_special, 2, 137 );
	}
	
	{
		short	i;
		
		/* Make the "Music" menu */
		menu_music = NewMenu(138, "\parg_music");
		
		/* Append the choices */
		for( i=0; i < SONG_MAX; i++ )
		{
			if( !streq(song_description[i], "" ) )
			{
				Str255		menu_title;
			
				c2p_stringcopy( menu_title, song_description[i] );
			
				AppendMenu( menu_music, menu_title );
			}
			else
			{
				AppendMenu( menu_music, "\pEmpty" );
			}
		}
		
		/* Insert SubMenu */
		InsertMenu( menu_music, -1 );
		/* This call is only support with AppearanceLib 1.0 or greater */
		SetMenuItemHierarchicalID( menu_special, 3, 138 );
	}
	

	/* Update the menu bar */
	DrawMenuBar();
}


/*
 * Prepare the menus
 */
static void setup_menus(void)
{
	int i, n;

	short value;

	Str255 s;

	MenuHandle m, subm;

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
	{
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
		if (initialized && character_generated)
		{
			EnableMenuItem(m, 5);
		}

		/* Enable "exit"/"quit" */
		if (TRUE)
		{
			EnableMenuItem(m, 7);
			EnableMenuItem(m, 8);
		}
	}

	/* Edit menu */
	{
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
	}

	/* Font menu */
	{
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
		if (td)
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
				/*GetItem(m, i, s); */
				GetFNum(s, &value);

				/* Check active font */
				if (td->font_id == value) CheckMenuItem(m, i, TRUE);
			}
		}
	}

	/* Size menu */
	{
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
		if (td)
		{
			/* Analyze sizes */
			for (i = 1; i <= n; i++)
			{
				/* Analyze size */
				GetMenuItemText(m,i,s);
				/*GetItem(m, i, s); */
				s[s[0]+1] = '\0';
				value = atoi((char*)(s+1));

				/* Enable the "real" sizes */
				if (RealFont(td->font_id, value)) EnableMenuItem(m, i);

				/* Check the current size */
				if (td->font_size == value) CheckMenuItem(m, i, TRUE);
			}
		}
	}

	/* Windows menu */
	{
		m = GetMenuHandle(133);

		/* Get menu size */
		n = CountMenuItems(m);

		/* Check active windows */
		for (i = 1; i <= n; i++)
		{
			/* Check if needed */
			CheckMenuItem(m, i, data[i-1].mapped);
		}
	}

	/* Special menu */
	{
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

		/* graphics state */
		{
			/* Item "arg_graphics" */
			EnableMenuItem(m, 2);
		
			subm = GetMenuHandle(137);
			
			/* Get menu size */
			n = CountMenuItems(subm);

			/* Reset menu */
			for (i = 1; i <= n; i++)
			{
				/* Reset */
				EnableMenuItem(subm, i);
				CheckMenuItem(subm, i, FALSE);
			}
						
			switch( arg_graphics )
			{
				case GRAPHICS_ORIGINAL:
					{
						EnableMenuItem(subm, 2);
						CheckMenuItem(subm, 2, true);
					}
					break;
				case GRAPHICS_ADAM_BOLT:
					{
						EnableMenuItem(subm, 3);
						CheckMenuItem(subm, 3, true);
					}
					break;
				
				case GRAPHICS_NONE:
				default:
					{
						EnableMenuItem(subm, 1);
						CheckMenuItem(subm, 1, true);
					}
					break;
			}
		}
		
		/* music state */
		{
			/* Item arg_music */
			EnableMenuItem(m, 3);
			
			subm = GetMenuHandle(138);
			
			 
			/* Get menu size */
			n = CountMenuItems(subm);
			
			/* add songs to menu */
			for( i=1; i <= n; i++ )
			{
				short		song_index = i-1;
				
				if( !streq( song_name[song_index], "" ) )
				{
					if( song_index == current_song )
					{
						/* Reset */
						EnableMenuItem(subm, i);
						CheckMenuItem(subm, i, TRUE);
					}
					else
					{
						/* Reset */
						EnableMenuItem(subm, i);
						CheckMenuItem(subm, i, FALSE);
					}
				}
				else
				{
					/* Reset */
					CheckMenuItem(subm, i, FALSE);
					DisableMenuItem(subm, i);
				}
			}
		}

		/* Item "arg_fiddle" */
		EnableMenuItem(m, 5);
		CheckMenuItem(m, 5, arg_fiddle);

		/* Item "arg_wizard" */
		EnableMenuItem(m, 6);
		CheckMenuItem(m, 6, arg_wizard);

		/* Item "Hack" */
		/* EnableMenuItem(m, 9); */
	}

	/* TileWidth menu */
	{
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
		if (td)
		{
			/* Analyze sizes */
			for (i = 1; i <= n; i++)
			{
				/* Analyze size */
				GetMenuItemText(m,i,s);
				/*GetItem(m, i, s); */
				s[s[0]+1] = '\0';
				value = atoi((char*)(s+1));

				/* Enable */
				EnableMenuItem(m, i);

				/* Check the current size */
				if (td->tile_wid == value) CheckMenuItem(m, i, TRUE);
			}
		}
	}

	/* TileHeight menu */
	{
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
		if (td)
		{
			/* Analyze sizes */
			for (i = 1; i <= n; i++)
			{
				/* Analyze size */
				GetMenuItemText(m,i,s);
				/*GetItem(m, i, s); */
				s[s[0]+1] = '\0';
				value = atoi((char*)(s+1));

				/* Enable */
				EnableMenuItem(m, i);

				/* Check the current size */
				if (td->tile_hgt == value) CheckMenuItem(m, i, TRUE);
			}
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
				Rect r;
				short item_hit;
				BitMap screen;

#ifdef TARGET_CARBON						
				GetQDGlobalsScreenBits( &screen );
#else
				screen = qd.screenBits;
#endif	
				
				dialog=GetNewDialog(128, 0, (WindowPtr)-1);

#ifdef TARGET_CARBON
				GetWindowBounds( (WindowRef)dialog, kWindowContentRgn, &r );
#else
				r = dialog->portRect;
				local_to_global( &r );
#endif
				
				center_rect(&r, &screen.bounds);
				MacMoveWindow((WindowRef)dialog, r.left, r.top, 1);
				MacShowWindow((WindowRef)dialog);
				ModalDialog(0, &item_hit);
				DisposeDialog(dialog);
				break;
			}

			
#ifdef TARGET_CARBON
#else
			/* Desk accessory */
			GetMenuItemText(GetMenuHandle(128),selection,s);
			/* GetMenuItem(GetMenuHandle(128), selection, s); */
			OpenDeskAcc(s);
#endif
			break;
		}

		/* File Menu */
		case 129:
		{
			switch (selection)
			{
				/* New */
				case 1:
				{
					do_menu_file_new();
					break;
				}

				/* Open... */
				case 2:
				{
					/* do_menu_file_open(FALSE); */
					do_menu_file_open(TRUE);
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
					HideWindow(td->w);

					break;
				}

				/* Save */
				case 5:
				{
					/* Hack -- Forget messages */
					msg_flag = FALSE;

					/* Hack -- Save the game */
					do_cmd_save_game(FALSE);

					break;
				}

				/* Exit (without save) */
				case 7:
				{
					/* Allow user to cancel "dangerous" exit */
					if (game_in_progress && character_generated)
					{
						if( mac_confirm( "Quitting without save?", "The current player state will not be saved." ) == true )
						{
							break;
						}
					}

					/* Quit */
					quit(NULL);
					break;
				}

				/* Quit (with save) */
				case 8:
				{
					/* Save the game (if necessary) */
					if (game_in_progress && character_generated)
					{
						/* Hack -- Forget messages */
						msg_flag = FALSE;

						/* Save the game */
						do_cmd_save_game(FALSE);
					}

					/* Quit */
					quit(NULL);
					break;
				}
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
			/* GetItem(GetMenuHandle(131), selection, s); */
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
			/* GetItem(GetMenuHandle(132), selection, s); */
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
			ShowWindow(td->w);

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
					/* arg_graphics = !arg_graphics; */

					/* Hack -- Force redraw */
					/* Term_key_push(KTRL('R')); */

					break;
				}

				case 5:
				{
					arg_fiddle = !arg_fiddle;
					break;
				}

				case 6:
				{
					arg_wizard = !arg_wizard;
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
			/* GetItem(GetMenuHandle(135), selection, s); */
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
			/* GetItem(GetMenuHandle(136), selection, s); */
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
		
		
		/* Graphics menu */
		case 137:
		{
			switch (selection)
			{
				/* Text Only */
				case 1:
				{
					/* Toggle arg_sound */
					arg_graphics = GRAPHICS_NONE;
					use_graphics = GRAPHICS_NONE;
					use_transparency = false;
					
					/* React to changes */
					Term_xtra(TERM_XTRA_REACT, 0);

					/* Hack -- Force redraw */
					Term_key_push(KTRL('R'));
					
					break;
				}

				/* Original Graphics 8x8 tiles */
				case 2:
				{
					/* Toggle arg_sound */
					arg_graphics = GRAPHICS_ORIGINAL;
					use_graphics = GRAPHICS_ORIGINAL;
					use_transparency = false;
					
					/* React to changes */
					Term_xtra(TERM_XTRA_REACT, 0);

					/* Hack -- Force redraw */
					Term_key_push(KTRL('R'));
					
					break;
				}
				
				/* Adam Bolt Graphics 16x16 tiles */
				case 3:
				{
					/* Toggle arg_sound */
					arg_graphics = GRAPHICS_ADAM_BOLT;
					use_graphics = GRAPHICS_ADAM_BOLT;
					use_transparency = true;
					
					/* React to changes */
					Term_xtra(TERM_XTRA_REACT, 0);

					/* Hack -- Force redraw */
					Term_key_push(KTRL('R'));
					
					break;
				}
			}
			
			break;
		}
		
		/* Music menu */
		case 138:
		{
			restart_music( selection - 1 );
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
	OSErr	aeError;
	DescType	returnedType;
	Size	actualSize;

	aeError = AEGetAttributePtr(theAppleEvent, keyMissedKeywordAttr, typeWildCard,
	                            &returnedType, NULL, 0, &actualSize);

	if (aeError == errAEDescNotFound) return (noErr);

	if (aeError == noErr) return (errAEParamMissed);

	return (aeError);
}


/*
 * Apple Event Handler -- Open Application
 */
static pascal OSErr AEH_Start(const AppleEvent *theAppleEvent,
                              AppleEvent *reply, UInt32 handlerRefCon)
{
#pragma unused(reply, handlerRefCon)

	return (CheckRequiredAEParams(theAppleEvent));
}


/*
 * Apple Event Handler -- Quit Application
 */
static pascal OSErr AEH_Quit(const AppleEvent *theAppleEvent,
                             AppleEvent *reply, UInt32 handlerRefCon)
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
                              AppleEvent *reply, UInt32 handlerRefCon)
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
static pascal OSErr AEH_Open(const AppleEvent *theAppleEvent,
                             AppleEvent* reply, UInt32 handlerRefCon)
{
#pragma unused(reply, handlerRefCon)

	FSSpec		myFSS;
	AEDescList	docList;
	OSErr		err;
	Size		actualSize;
	AEKeyword	keywd;
	DescType	returnedType;
	char		foo[128];
	FInfo		myFileInfo;

	/* Put the direct parameter (a descriptor list) into a docList */
	err = AEGetParamDesc(theAppleEvent, keyDirectObject, typeAEList, &docList);
	if (err) return err;

	/*
	 * We ignore the validity check, because we trust the FInder, and we only
	 * allow one savefile to be opened, so we ignore the depth of the list.
	 */

	err = AEGetNthPtr(&docList, 1L, typeFSS, &keywd,
	                  &returnedType, (Ptr) &myFSS, sizeof(myFSS), &actualSize);
	if (err) return err;

	/* Only needed to check savefile type below */
	err = FSpGetFInfo(&myFSS, &myFileInfo);
	if (err)
	{
		strnfmt(foo, 128, "Arg!  FSpGetFInfo failed with code %d", err);
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


#endif



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
 * Optimize non-blocking calls to "CheckEvents()"
 * Idea from "Maarten Hazewinkel <mmhazewi@cs.ruu.nl>"
 */
#define EVENT_TICKS 6


/*
 * Check for Events, return TRUE if we process any
 *
 * Hack -- Handle AppleEvents if appropriate (ignore result code).
 */
static bool CheckEvents(bool wait)
{
	EventRecord event;

	WindowPtr w;

	Rect r;

	long newsize;

	int ch, ck;

	int mc, ms, mo, mx;

	int i;

	term_data *td = NULL;

	UInt32 sleep_ticks;

#ifndef TARGET_CARBON

	huge curTicks;

	static huge lastTicks = 0L;


	/* Access the clock */
	curTicks = TickCount();

	/* Hack -- Allow efficient checking for non-pending events */
	if (!wait && (curTicks < lastTicks + EVENT_TICKS)) return (FALSE);

	/* Timestamp last check */
	lastTicks = curTicks;

	/* Let the "system" run */
	SystemTask();

	if( use_sound )
	{
		check_music();
	}
	
	/* Blocking call to WaitNextEvent - Should use MAX_INT XXX XXX XXX */
	if (wait)
	{
		sleep_ticks = 0x7FFFFFFFL;
	}
	else
	{
		/* Non-blocking call */
		sleep_ticks = 0L;
	}
		
	/* Get an event (or null) */
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
			if( mx )
			{
				/* Handle a few special keyboard shortcuts */
				if( ch == 44 )
				{
					int inc = ( ms ? VOLUME_INC : 1 );
					
					/* Adjust sound volume DOWN */
					if( gSoundVolume > (VOLUME_MIN + inc) )
						gSoundVolume -= inc;
					else
						gSoundVolume = VOLUME_MIN;
					
					/* test sound volume */
					sound( SOUND_HIT );
					
					/* Done */
					break;
				}
				else if( ch == 46 )
				{
					int inc = ( ms ? VOLUME_INC : 1 );
					
					/* Adjust sound volume UP */
					if( gSoundVolume < (VOLUME_MAX - inc) )
						gSoundVolume += inc;
					else
						gSoundVolume = VOLUME_MAX;
					
					/* test sound volume */
					sound( SOUND_HIT );
					
					/* Done */
					break;
				}
				else if(ck < 64)
				{
					/* Hack -- Prepare the menus */
					setup_menus();
	
					/* Mega-Hack -- allow easy exit if nothing to save */
					if (!character_generated && (ch=='Q' || ch=='q')) ch = 'e';
	
					/* Run the Menu-Handler */
					menu(MenuKey(ch));
	
					/* Turn off the menus */
					HiliteMenu(0);
	
					/* Done */
					break;
				}
			}


			/* Hide the mouse pointer */
			ObscureCursor();
			
			/* plog_fmt( "Keypress code=%ld, char=%ld", (long)ck, (long)ch );*/
			
			/* Normal key -> simple keypress */
			if (ck < 64)
			{
				/* Enqueue the keypress */
				Term_keypress(ch);
			}

			/* Hack -- normal "keypad keys" -> special keypress */
			else if (!mc && !ms && !mo && !mx && (ck < 96))
			{
				/*plog( "KeyPad keypress being handled by Zangband!" ); */
				
				/* Hack -- "enter" is confused */
				if (ck == 76) ch = '\n';

				/* Send control-caret as a trigger */
				Term_keypress(30);

				/* Send the "ascii" keypress */
				Term_keypress(ch);
			}

			/* Bizarre key -> encoded keypress */
			else if (ck <= 127)
			{
				/* Hack -- introduce with control-underscore */
				Term_keypress(31);

				/* Send some modifier keys */
				if (mc) Term_keypress('C');
				if (ms) Term_keypress('S');
				if (mo) Term_keypress('O');
				if (mx) Term_keypress('X');

				/* Hack -- Downshift and encode the keycode */
				Term_keypress('0' + (ck - 64) / 10);
				Term_keypress('0' + (ck - 64) % 10);

				/* Hack -- Terminate the sequence */
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

				case inSysWindow:
				{
#ifdef TARGET_CARBON
#else
					SystemClick(&event, w);
#endif
					break;
				}

				case inDrag:
				{
					Point p;

					WindowPtr old_win;

					BitMap screen;
					
					Rect portRect;
					
#ifdef TARGET_CARBON						
					GetQDGlobalsScreenBits( &screen );
#else
					screen = qd.screenBits;
#endif	
					
					r = screen.bounds;
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
#ifdef TARGET_CARBON
					GetWindowBounds( (WindowRef)td->w, kWindowContentRgn, &portRect );
#else
					portRect = td->w->portRect;
					local_to_global( &portRect );
#endif
					p.h = portRect.left;
					p.v = portRect.top;
					td->r.left = p.h;
					td->r.top = p.v;

					/* Restore */
					activate(old_win);

					/* Apply and Verify */
					term_data_check_size(td);

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
						HideWindow(td->w);
					}

					break;
				}

				case inGrow:
				{
					int x, y;

					term *old = Term;

					/* Oops */
					if (!td) break;

					/* Fake rectangle */
					r.left = 20 * td->tile_wid + td->size_ow1;
					r.right = 80 * td->tile_wid + td->size_ow1 + td->size_ow2 + 1;
					r.top = 1 * td->tile_hgt + td->size_oh1;
					r.bottom = 24 * td->tile_hgt + td->size_oh1 + td->size_oh2 + 1;

					/* Grow the rectangle */
					newsize = GrowWindow(w, event.where, &r);

					/* Handle abort */
					if (!newsize) break;

					/* Extract the new size in pixels */
					y = HiWord(newsize) - td->size_oh1 - td->size_oh2;
					x = LoWord(newsize) - td->size_ow1 - td->size_ow2;

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

		/* Disk Event -- From "Maarten Hazewinkel" */
		case diskEvt:
		{
			/* check for error when mounting the disk */
			
#ifdef TARGET_CARBON
#else
			if (HiWord(event.message) != noErr)
			{
				Point p =
				{120, 120};

				DILoad();
				DIBadMount(p, event.message);
				DIUnload();
			}
#endif
			break;
		}

		/* OS Event -- From "Maarten Hazewinkel" */
		case osEvt:
		{
			switch ((event.message >> 24) & 0x000000FF)
			{
				case suspendResumeMessage:

				/* Resuming: activate the front window */
				if (event.message & resumeFlag)
				{
					SetPort( GetWindowPort(FrontWindow()) );
					
#ifdef TARGET_CARBON
					{
						Cursor	arrow;
					
						GetQDGlobalsArrow( &arrow );
						SetCursor(&arrow);
					}
#else
					SetCursor( &qd.arrow );
#endif
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
				/* Forget */
				quit_when_ready = FALSE;

				/* Do the menu key */
				menu(MenuKey('q'));

				/* Turn off the menus */
				HiliteMenu(0);
			}

			/* Handle "open_when_ready" */
			handle_open_when_ready();

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
static vptr hook_rnfree(vptr v)
{

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

	/* Exit the sound support */
	close_sounds();
	
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
static void init_paths( void )
{
	char basepath[1024];
	
	/* Default to the "lib" folder with the application */
	{
		char app_path[1024];
		
		PathNameFromDirID(app_dir, app_vol, (StringPtr)app_path);
		ptocstr((StringPtr)app_path);
		strcat(app_path, "lib/" );
		strcpy(basepath,app_path);
	}
	
	/* Prepare the paths */
	init_file_paths(basepath);
}

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
	int i;
	Rect r;
	Point topleft;
	char path[1024];
	BitMap screen;

	/* Fake rectangle */
	r.left = 0;
	r.top = 0;
	r.right = 344;
	r.bottom = 188;

	/* Center it */
#ifdef TARGET_CARBON						
	GetQDGlobalsScreenBits( &screen );
#else
	screen = qd.screenBits;
#endif	

	center_rect(&r, &screen.bounds);

	/* Extract corner */
	topleft.v = r.top;
	topleft.h = r.left;

	/* Check until done */
	while (1)
	{
		OSType		types[3];
		
		/* Build the filename */
		path_make(path, ANGBAND_DIR_FILE, "news.txt");
		
		/* Attempt to open and close that file */
		if (0 == fd_close(fd_open(path, O_RDONLY))) break;

		/* Warning */
		plog_fmt("Unable to open the file '%s'", path);

		/* Warning */
		plog("The Angband 'lib' folder is probably missing or misplaced.");

		/* Warning */
		plog("Please 'open' any file in any sub-folder of the 'lib' folder.");

		/* Allow "text" files */
		types[0] = 'TEXT';

		/* Allow "save" files */
		types[1] = 'SAVE';

		/* Allow "data" files */
		types[2] = 'DATA';

		/* Get any file */
		{
			OSErr		err;
			
			err = ChooseFile( (unsigned char *)path, types, 3 );
			
			if( err != noErr )
			{
				quit(NULL);
			}
		}

		/* Hack -- Remove the "filename" */
		i = strlen(path) - 1;
		while ((i > 0) && (path[i] != '/')) i--;
		if (path[i] == '/') path[i+1] = '\0';

		/* Hack -- allow "lib" folders */
		if (suffix(path, "/lib")) continue;

		/* Hack -- Remove the "sub-folder" */
		i = i - 1;
		while ((i > 1) && (path[i] != '/')) i--;
		if (path[i] == '/') path[i+1] = '\0';
	}
}


/*
 * Macintosh Main loop
 */
int main(void)
{
	EventRecord tempEvent;
	int numberOfMasters = 10;

	/* Get more Masters */
	/* Only effective on MacOS 8/9 platform */
	while (numberOfMasters--) MoreMasters();

#ifndef TARGET_CARBON
	/* Set up the Macintosh */
	InitGraf(&qd.thePort);
	InitFonts();
	InitWindows();
	InitMenus();
	/* TEInit(); */
	InitDialogs(NULL);
#endif
	InitCursor();

	/* Flush events */
	FlushEvents(everyEvent, 0);

	/* Flush events some more (?) */
	(void)EventAvail(everyEvent, &tempEvent);
	(void)EventAvail(everyEvent, &tempEvent);
	(void)EventAvail(everyEvent, &tempEvent);


#ifdef ANGBAND_LITE_MAC

	/* Nothing */

#else /* ANGBAND_LITE_MAC */

# if defined(powerc) || defined(__powerc)

	/* Assume System 7 */
	
	/* Assume Color Quickdraw */

# else

	/* Block */
	if (TRUE)
	{
		OSErr err;
		long versionNumber;

		/* Check the Gestalt */
		err = Gestalt(gestaltSystemVersion, &versionNumber);

		/* Check the version */
		if ((err != noErr) || (versionNumber < 0x0700))
		{
			quit("You must have System 7 to use this program.");
		}
	}

	
# endif

#endif /* ANGBAND_LITE_MAC */


#ifdef USE_SFL_CODE

	/* Obtain a "Universal Procedure Pointer" */
	AEH_Start_UPP = NewAEEventHandlerUPP(AEH_Start);

	/* Install the hook (ignore error codes) */
	AEInstallEventHandler(kCoreEventClass, kAEOpenApplication, AEH_Start_UPP,
	                      0L, FALSE);

	/* Obtain a "Universal Procedure Pointer" */
	AEH_Quit_UPP = NewAEEventHandlerUPP(AEH_Quit);

	/* Install the hook (ignore error codes) */
	AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, AEH_Quit_UPP,
	                      0L, FALSE);

	/* Obtain a "Universal Procedure Pointer" */
	AEH_Print_UPP = NewAEEventHandlerUPP(AEH_Print);

	/* Install the hook (ignore error codes) */
	AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments, AEH_Print_UPP,
	                      0L, FALSE);

	/* Obtain a "Universal Procedure Pointer" */
	AEH_Open_UPP = NewAEEventHandlerUPP(AEH_Open);

	/* Install the hook (ignore error codes) */
	AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments, AEH_Open_UPP,
	                      0L, FALSE);

#endif


	/* Find the current application */
	SetupAppDir();


#if defined(MACINTOSH) && !defined(applec)

	/* Mark ourself as the file creator */
	_fcreator = 'A271';

	/* Default to saving a "text" file */
	_ftype = 'TEXT';

#endif

	/* Hook in some "z-virt.c" hooks */
	rnfree_aux = hook_rnfree;
	ralloc_aux = hook_ralloc;
	rpanic_aux = hook_rpanic;

	/* Hooks in some "z-util.c" hooks */
	plog_aux = hook_plog;
	quit_aux = hook_quit;
	core_aux = hook_core;



	/* Show the "watch" cursor */
	SetCursor(*(GetCursor(watchCursor)));

	/* Prepare paths */
	init_paths();
	
	/* Prepare sound effects */
	init_sounds();
	
	/* Prepare music soundtrack */
	init_music();
	
	/* Prepare the menubar */
	init_menubar();

	/* Prepare the windows */
	init_windows();
	
	/* Hack -- process all events */
	while (CheckEvents(FALSE)) /* loop */;

	/* Reset the cursor */
#ifdef TARGET_CARBON
	{
		Cursor	arrow;
		GetQDGlobalsArrow( &arrow );
		SetCursor(&arrow);
	}
#else
	SetCursor( &qd.arrow );
#endif

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


	/* Prompt the user */
	prtf(15, 23, "[Choose 'New' or 'Open' from the 'File' menu]");

	/* Flush the prompt */
	Term_fresh();


	/* Hack -- Process Events Forever */
	while (TRUE) CheckEvents(TRUE);
}

