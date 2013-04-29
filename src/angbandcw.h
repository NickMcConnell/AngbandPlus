#ifndef INCLUDED_ANGBANDCW_H
#define INCLUDED_ANGBANDCW_H

#include <windows.h>
#include "angband.h"
#include "readdib.h"

#define MAIN_KEY_DOWN_TIMER			300 /* FWG 5-28-2001 */

/* FWG 5-24-2001 */
#define MAX_MENU_FONTS		11

/* FWG 5-25-2001 */
/* Defines hardware buttons */
#define DIR_A		16 /* A */
#define DIR_B		32 /* B */
#define DIR_C		64 /* C */
#define DIR_START	128 /* Start */
#define DIR_AUX1	256 /* Aux1 */
#define DIR_AUX2	512 /* Aux2 */
#define DIR_1		1024 /* 1 */
#define DIR_2		2048 /* 2 */
#define DIR_3		4096 /* 3 */

/* FWG 5-23-2001 */
/* A struct for handling of fake fonts. */
/* Fonts have to start with the string "font"
	followed by a '_' then the width followed
	by a 'x' followed by a height. So normal
	fonts take on the form "font_NxN.bmp". 
	Landscape fonts take on the form 
	"font_NxN_ls.bmp". 16 colors fonts that don't
	need any transparency follow the format 
	"font_NxN_16.bmp". Landscape 16 color fonts
	follow the form "font_NxN_ls_16.bmp".
*/
typedef struct FAKEFONT_DATA
{
	int width;
	int height;

	int font;
	int colors;

	int font16;
};



struct CONTROLBITMAPS
{
	DIBINIT mImage; // can I get src image size from this?
	int mDstX;
	int mDstY;
	int mDstW;
	int mDstH;
};

typedef struct GXKeyList {
    short vkUp;             // key for up
    POINT ptUp;             // x,y position of key/button.  Not on screen but in screen coordinates.
    short vkDown;
    POINT ptDown;
    short vkLeft;
    POINT ptLeft;
    short vkRight;
    POINT ptRight;
    short vkA;
    POINT ptA;
    short vkB;
    POINT ptB;
    short vkC;
    POINT ptC;
    short vkStart;
    POINT ptStart;
}; 

typedef struct winceGlobals_data 
{
	
	// These variables are required to display text. 
	
	int g_xClient;     // width of client area 
	int g_yClient;     // height of client area 
	int g_xClientMax;  // maximum width of client area 
	int g_yClientMax;  // maximum height of client area 
	
	int m_MenuHeight;
	int m_MacroBarHeight;

	HWND g_mainhWnd;
	HWND g_TBhWnd;			// The macro bar handle
	HWND g_hwndCB;			// The command bar handle
	int g_cx;
	int g_cy;
	
	int g_useWinCEFakeFont;
	int g_mw_fakeFontWidth;
	int g_mw_fakeFontHeight;
	struct CONTROLBITMAPS	g_maskedLetters;
	struct CONTROLBITMAPS	g_maskedLetterColors;
	
	/* FWG 5-7-2001 */
	BOOL m_bTitleBarShown;
	// SJG
	BOOL m_bMacroBarShown;
	BOOL m_bMacroButton;

	/* FWG 5-8-2001 */
	int m_winX;
	int m_winY;
	int m_winW;
	int m_winH;

	/* FWG 5-23-2001 */
	int m_numFakeFonts;
	struct FAKEFONT_DATA* m_pFakeFontList;

	/* FWG 5-24-2001 */
	BOOL m_fakeFont16ColorHack;
	int m_fontOffset;

	/* FWG 5-25-2001 */
	BOOL m_bButtonPressed;
	unsigned long m_buttonsDown;

	/* FWG 5-30-2001 */
	unsigned long m_gxDll;
	BOOL m_bHardwareButtonsOpen;

	struct GXKeyList g_gxkl;

} WINCEGLOBALS;



// From angbandcw.c FWG 4-8-2001
void InitWinCEGlobalData(WINCEGLOBALS* pg);
void SetDefaultAngbandCEMenuItems(HWND hwndMenuBar);
int ScanForFakeFontFiles(HWND hWnd, char* pDir);
int CalculateWindowExtents();

int ButtonPress(int vkKey);
int ButtonUp(int vkKey);
void HandleKeyDownTimer();

#endif  /* INCLUDED_ANGBANDCW_H */
