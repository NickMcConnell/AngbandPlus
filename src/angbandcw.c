#include "angbandcw.h"

#include <aygshell.h>

#include "resource.h"

extern WINCEGLOBALS g_g;

extern char *ANGBAND_DIR_XTRA_GRAF;

#define SHGetSubMenu(hWndMB,ID_MENU) (HMENU)SendMessage((hWndMB), SHCMBM_GETSUBMENU, (WPARAM)0, (LPARAM)ID_MENU);


/*
 * Available graphic modes
 */
#define GRAPHICS_NONE       0
#define GRAPHICS_ORIGINAL   1
#define GRAPHICS_ADAM_BOLT  2
#define GRAPHICS_DAVID_GERVAIS  3

/****************************************************\
	FWG 4-8-2001
\****************************************************/
void InitWinCEGlobalData(WINCEGLOBALS* pg)
{
	memset(pg, 0, sizeof(WINCEGLOBALS));

	// Default to an 8x13 font.
	pg->g_mw_fakeFontWidth = 8;
	pg->g_mw_fakeFontHeight = 13;
}

/* FWG 5-24-2001 */
/****************************************************\
	This function handles the font menu choices
\****************************************************/
int HandleFontMenuChoices(HWND hwndMenuBar, int offset, int rows, int cols)
{
	if (offset == g_g.m_fontOffset)
	{
		return -1;
	}

	g_g.m_fontOffset = offset;

	{
		int i;
		DWORD dwRet;
		HMENU hSetupMenu;
		
		hSetupMenu = SHGetSubMenu(hwndMenuBar, ID_OPTIONS);

		/* Check the other font sizes */
		for (i = 0; i < g_g.m_numFakeFonts; i++)
		{
			dwRet = CheckMenuItem(hSetupMenu
				, (ID_OPTIONS_MAINWINDOW_FONT_FONT0) + i
				, (i == offset) 
					? (MF_CHECKED | MF_BYCOMMAND) 
					: (MF_UNCHECKED | MF_BYCOMMAND) ); 
		}

		g_g.g_mw_fakeFontWidth = g_g.m_pFakeFontList[g_g.m_fontOffset].width;
		g_g.g_mw_fakeFontHeight = g_g.m_pFakeFontList[g_g.m_fontOffset].height;
		
		g_g.g_xClientMax = g_g.g_mw_fakeFontWidth * cols; 
		g_g.g_yClientMax = g_g.g_mw_fakeFontHeight * rows;
		
	}

	return 0;
}

/****************************************************\
	This function updates the font menu choices
\****************************************************/
void UpdateFontMenuChoices(HWND hwndMenuBar)
{
	DWORD dwRet;
	HMENU hSetupMenu;

	hSetupMenu = SHGetSubMenu(hwndMenuBar, ID_OPTIONS);
  
	/* I'm in normal mode */
	if (!g_g.m_fakeFont16ColorHack)
	{
		int i;

		/* check the font */
		dwRet = CheckMenuItem(hSetupMenu
			, (ID_OPTIONS_MAINWINDOW_FONT_FONT0) + g_g.m_fontOffset
			, MF_CHECKED | MF_BYCOMMAND);

		/* check both font16 and landscape */
		dwRet = CheckMenuItem(hSetupMenu
			, ID_OPTIONS_MAINWINDOW_FONT_16COLORHACKFAST 
			, MF_UNCHECKED | MF_BYCOMMAND);

		/* From here you can only do two options +
		number of fonts -1 options. */

		/* check font16 */
		if (g_g.m_pFakeFontList[g_g.m_fontOffset].font16)
		{
			dwRet = EnableMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_FONT_16COLORHACKFAST 
				, MF_ENABLED | MF_BYCOMMAND);
		}
		else
		{
			dwRet = EnableMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_FONT_16COLORHACKFAST 
				, MF_GRAYED | MF_BYCOMMAND);
		}
		
		/* Check the other font sizes */
		for (i = 0; i < g_g.m_numFakeFonts; i++)
		{
			if (i != g_g.m_fontOffset)
			{
				if ( (g_g.m_pFakeFontList[i].font) && 
					(g_g.m_pFakeFontList[i].colors) )
				{
					dwRet = EnableMenuItem(hSetupMenu
						, (ID_OPTIONS_MAINWINDOW_FONT_FONT0) + i
						, MF_ENABLED | MF_BYCOMMAND);
					
				}
				else
				{
					dwRet = EnableMenuItem(hSetupMenu
						, (ID_OPTIONS_MAINWINDOW_FONT_FONT0) + i
						, MF_GRAYED | MF_BYCOMMAND);
				}
			}
		}
	}
	/* I'm in mormal mode */

	/* I'm in font16 mode */
	else if (g_g.m_fakeFont16ColorHack)
	{
		int i;

		/* check the font */
		dwRet = CheckMenuItem(hSetupMenu
			, (ID_OPTIONS_MAINWINDOW_FONT_FONT0) + g_g.m_fontOffset
			, MF_CHECKED | MF_BYCOMMAND);

		/* check both font16 and landscape */
		dwRet = CheckMenuItem(hSetupMenu
			, ID_OPTIONS_MAINWINDOW_FONT_16COLORHACKFAST 
			, MF_CHECKED | MF_BYCOMMAND);

		/* From here you can only do two options +
		number of fonts -1 options. */

		/* uncheck font16 */
		if ( (g_g.m_pFakeFontList[g_g.m_fontOffset].font)
			&& (g_g.m_pFakeFontList[g_g.m_fontOffset].colors) )
		{
			/* I do have one or both of the normal 
			font and colors in landscape mode. */
			dwRet = EnableMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_FONT_16COLORHACKFAST 
				, MF_ENABLED | MF_BYCOMMAND);
		}
		else
		{
			dwRet = EnableMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_FONT_16COLORHACKFAST 
				, MF_GRAYED | MF_BYCOMMAND);
		}
		
		/* Check the other font sizes */
		for (i = 0; i < g_g.m_numFakeFonts; i++)
		{
			if (i != g_g.m_fontOffset)
			{
				if (g_g.m_pFakeFontList[i].font16)
				{
					dwRet = EnableMenuItem(hSetupMenu
						, (ID_OPTIONS_MAINWINDOW_FONT_FONT0) + i
						, MF_ENABLED | MF_BYCOMMAND);
					
				}
				else
				{
					dwRet = EnableMenuItem(hSetupMenu
						, (ID_OPTIONS_MAINWINDOW_FONT_FONT0) + i
						, MF_GRAYED | MF_BYCOMMAND);
				}
			}
		}
	}
	/* I'm in font16 mode */

}


/****************************************************\
	This function sets the default values for our
	menu. FWG 4-8-2001
\****************************************************/
void SetDefaultAngbandCEMenuItems(HWND hwndMenuBar)
{
	int i;
	DWORD dwRet;
	HMENU hSetupMenu;

	hSetupMenu = SHGetSubMenu(hwndMenuBar, ID_OPTIONS);
  
	for (i = 0; i < g_g.m_numFakeFonts; i++)
	{
		char str[128];
		TCHAR wcStr[128];

		sprintf(str, "%dX%d", g_g.m_pFakeFontList[i].width, g_g.m_pFakeFontList[i].height);

		mbstowcs(wcStr, str, 128);

		InsertMenu(
			hSetupMenu, 
			ID_OPTIONS_MAINWINDOW_FONT_FONT10, 
			MF_STRING,
			(ID_OPTIONS_MAINWINDOW_FONT_FONT0) + i, 
			wcStr);
	}

	// Delete existing font placeholders
	for (i = 0; i < MAX_MENU_FONTS; i++)
	{
		DeleteMenu(hSetupMenu, ID_OPTIONS_MAINWINDOW_FONT_FONT0+i, MF_BYCOMMAND);
	}
	
	dwRet = CheckMenuItem(hSetupMenu
		, IDM_OPTIONS_TRPTILE 
		, (use_trptile)
		? (MF_CHECKED | MF_BYCOMMAND) 
		: (MF_UNCHECKED | MF_BYCOMMAND) );

	dwRet = CheckMenuItem(hSetupMenu
		, IDM_OPTIONS_DBLTILE 
		, (use_dbltile)
		? (MF_CHECKED | MF_BYCOMMAND) 
		: (MF_UNCHECKED | MF_BYCOMMAND) );

	dwRet = CheckMenuItem(hSetupMenu
		, IDM_OPTIONS_BIGTILE 
		, (use_bigtile)
		? (MF_CHECKED | MF_BYCOMMAND) 
		: (MF_UNCHECKED | MF_BYCOMMAND) );

	dwRet = CheckMenuItem(hSetupMenu
		, IDM_OPTIONS_SMALL_SCREEN 
		, (small_screen)
		? (MF_CHECKED | MF_BYCOMMAND) 
		: (MF_UNCHECKED | MF_BYCOMMAND) );

	dwRet = CheckMenuItem(hSetupMenu
		, ID_OPTIONS_MAINWINDOW_TITLEBAR 
		, (g_g.m_bTitleBarShown) 
		? (MF_CHECKED | MF_BYCOMMAND) 
		: (MF_UNCHECKED | MF_BYCOMMAND) );

	dwRet = CheckMenuItem(hSetupMenu
		, IDM_OPTIONS_MACRO_BAR 
		, (g_g.m_bMacroBarShown) 
		? (MF_CHECKED | MF_BYCOMMAND) 
		: (MF_UNCHECKED | MF_BYCOMMAND) );

	if (g_g.g_useWinCEFakeFont)
	{
		if ( (g_g.g_mw_fakeFontWidth == 4) && (g_g.g_mw_fakeFontHeight == 6) )
		{
			dwRet = CheckMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_FONT_4X6 
				, (MF_CHECKED | MF_BYCOMMAND) );
		}
		else if ( (g_g.g_mw_fakeFontWidth == 5) && (g_g.g_mw_fakeFontHeight == 8) )
		{
			dwRet = CheckMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_FONT_5X8 
				, (MF_CHECKED | MF_BYCOMMAND) );
		}
		else
		{
			dwRet = CheckMenuItem(hSetupMenu
				, ID_OPTIONS_MAINWINDOW_FONT_8X13 
				, (MF_CHECKED | MF_BYCOMMAND) );
		}
	}


	switch (arg_graphics)
	{
	case GRAPHICS_DAVID_GERVAIS:
		dwRet = CheckMenuItem(hSetupMenu
			, IDM_OPTIONS_GRAPHICS_DAVID 
			, (MF_CHECKED | MF_BYCOMMAND) );
		break;

	case GRAPHICS_ADAM_BOLT:
		dwRet = CheckMenuItem(hSetupMenu
			, IDM_OPTIONS_NEW_GRAPHICS 
			, (MF_CHECKED | MF_BYCOMMAND) );
		break;

	case GRAPHICS_ORIGINAL:
		dwRet = CheckMenuItem(hSetupMenu
			, IDM_OPTIONS_OLD_GRAPHICS 
			, (MF_CHECKED | MF_BYCOMMAND) );
		break;

	default:
		dwRet = CheckMenuItem(hSetupMenu
			, IDM_OPTIONS_NO_GRAPHICS 
			, (MF_CHECKED | MF_BYCOMMAND) );
		break;
	}

	UpdateFontMenuChoices(hwndMenuBar);
}

int CalculateWindowExtents()
{
	HDC hdc;
	//get screen size
	hdc = GetDC(NULL);
	g_g.g_cx = GetDeviceCaps(hdc, HORZRES);
	g_g.g_cy = GetDeviceCaps(hdc, VERTRES);

	if (g_g.g_cx > 320)
	{
		g_g.m_MenuHeight = 52;
	}
	else
	{
		g_g.m_MenuHeight = 26;
	}
	
	g_g.m_winX = 0;
	g_g.m_winY = 0;
	g_g.m_winW = g_g.g_cx;
	g_g.m_winH = g_g.g_cy;
	
	if (g_g.m_bTitleBarShown)
	{
		g_g.m_winY += g_g.m_MenuHeight;
		g_g.m_winH -= g_g.m_MenuHeight;
	}
	
	g_g.m_winH -= g_g.m_MenuHeight;
	
	return 0;
}

void ParseFontBmp(const char* str, struct FAKEFONT_DATA* pData)
{
	char tempStr[128];
	char* pStr1;

	strcpy(tempStr, str);

	pStr1 = strtok(tempStr, "_");
	if (!pStr1)
	{
		return;
	}

	/* See if it is a fake font file */
	if (!strcmp("colors", pStr1))
	{
		pData->colors = 1;
	}
	else if (!strcmp("font", pStr1))
	{
		pData->font = 1;
	}
	else 
	{
		return;
	}

	pStr1 = strtok(NULL, "x");
	if (!pStr1)
	{
		return;
	}

	pData->width = atoi(pStr1);

	pStr1 = strtok(NULL, "_.");
	if (!pStr1)
	{
		return;
	}

	pData->height = atoi(pStr1);


	pStr1 = strtok(NULL, "_.");
	if (!pStr1)
	{
		return;
	}

	/* we have more data */
	if (!strcmp("16", pStr1))
	{
		pData->font = 0;
		pData->font16 = 1;
		return;
	}
	else 
	{
		return;
	}

	/* Should only be a period left if this bmp
	file is a 16 colors landscape file. */
	pStr1 = strtok(NULL, ".");
	if (!pStr1)
	{
		return;
	}

	if (!strcmp("16", pStr1))
	{
		pData->font = 0;
	}
}


/****************************************************\
	This function scans the directory where the bitmap 
	files are kept for all known BMP's. FWG 5-23-2001
\****************************************************/
int ScanForFakeFontFiles(HWND hWnd, char* pDir)
{
	HANDLE hFirstFile;
	WIN32_FIND_DATA findFileData;
	int numFiles;
	int tempCount;
	char tempStr[1024];
	TCHAR strLook[1024];

	
	mbstowcs(strLook, pDir, 1024);
	_tcscat(strLook, _T("*.bmp"));
		
	tempCount = numFiles = 0;

	hFirstFile = FindFirstFile(strLook, &findFileData); 
	if (hFirstFile == INVALID_HANDLE_VALUE)
	{
		return -1;
	}

	do
	{
		struct FAKEFONT_DATA data;
		
		memset(&data, 0, sizeof(struct FAKEFONT_DATA));

		wcstombs(tempStr, findFileData.cFileName, 1024);

		ParseFontBmp(tempStr, &data);

		if ( (data.width > 0) && (data.height > 0) )
		{
			if (g_g.m_numFakeFonts == 0)
			{
				g_g.m_numFakeFonts++;
				
				g_g.m_pFakeFontList = (struct FAKEFONT_DATA*)
					malloc(sizeof(struct FAKEFONT_DATA) * g_g.m_numFakeFonts);
				if (!g_g.m_pFakeFontList)
				{
					return -2;
				}
				
				*g_g.m_pFakeFontList = data;
			}
			else
			{
				int i;
				struct FAKEFONT_DATA* pTempData = NULL;
				
				for (i=0; i<g_g.m_numFakeFonts; i++)
				{
					if ( (g_g.m_pFakeFontList[i].width == data.width) &&
						(g_g.m_pFakeFontList[i].height == data.height) )
					{
						pTempData = &g_g.m_pFakeFontList[i];
						break;
					}
				}
				
				if (!pTempData)
				{
					struct FAKEFONT_DATA* pTempData = NULL;
					
					g_g.m_numFakeFonts++;
					
					pTempData = (struct FAKEFONT_DATA*)
						malloc(sizeof(struct FAKEFONT_DATA) * g_g.m_numFakeFonts);
					if (!pTempData)
					{
						return -3;
					}
					
					memcpy(pTempData, 
						g_g.m_pFakeFontList, 
						sizeof(struct FAKEFONT_DATA) * (g_g.m_numFakeFonts-1) );
					
					free(g_g.m_pFakeFontList);
					
					g_g.m_pFakeFontList = pTempData; 
					
					g_g.m_pFakeFontList[g_g.m_numFakeFonts-1] = data;
				}
				else
				{
					/* I could probably do else if's here */
					if (data.font == 1)
					{
						pTempData->font = 1;
					}
					if (data.colors == 1)
					{
						pTempData->colors = 1;
					}
					if (data.font16 == 1)
					{
						pTempData->font16 = 1;
					}
				}
			}
		}
	} while (FindNextFile(hFirstFile, &findFileData));

	return 0;
}


/****************************************\
	FWG 5-24-2001
	This code will check if we have the 
	bmp files for the global font we want.
	If not it wil change the global fake
	font to something we can handle.
\****************************************/
void TestWinCEFakeFont()
{
	int i;
	
	g_g.m_fontOffset = 0;
	
	for (i=0; i<g_g.m_numFakeFonts; i++)
	{
		if ( (g_g.m_pFakeFontList[i].width == g_g.g_mw_fakeFontWidth) &&
			(g_g.m_pFakeFontList[i].height == g_g.g_mw_fakeFontHeight) )
		{
			g_g.m_fontOffset = i;
			break;
		}
	}

	/* Test to see if we have the bmp's to handle 
	what the user wants. */
	if (g_g.m_fakeFont16ColorHack)
	{
		if (g_g.m_pFakeFontList[g_g.m_fontOffset].font16)
		{
			/* OK */
			return;
		}
	}
	else
	{
		if ( (g_g.m_pFakeFontList[g_g.m_fontOffset].font) &&
			(g_g.m_pFakeFontList[g_g.m_fontOffset].colors) )
		{
			/* OK */
			return;
		}
	}

	/* We didn't have what the user wanted. */
	if ( (g_g.m_pFakeFontList[g_g.m_fontOffset].font) &&
		(g_g.m_pFakeFontList[g_g.m_fontOffset].colors) )
	{
		/* OK */
		g_g.m_fakeFont16ColorHack = 0;
		return;
	}
	else if (g_g.m_pFakeFontList[g_g.m_fontOffset].font16)
	{
		/* OK */
		g_g.m_fakeFont16ColorHack = 1;
		return;
	}
}

void HandleKeyDownTimer()
{
	if (!g_g.m_buttonsDown)
	{
		KillTimer(g_g.g_mainhWnd, MAIN_KEY_DOWN_TIMER);
	}

	if (g_g.m_buttonsDown & DIR_A)
	{
		Term_keypress(150);
	}
	if (g_g.m_buttonsDown & DIR_B)
	{
		Term_keypress(151);
	}
	if (g_g.m_buttonsDown & DIR_C)
	{
		Term_keypress(152);
	}
	if (g_g.m_buttonsDown & DIR_START)
	{
		Term_keypress(153);
	}
	if (g_g.m_buttonsDown & DIR_AUX1)
	{
		Term_keypress(154);
	}
	if (g_g.m_buttonsDown & DIR_AUX2)
	{
		Term_keypress(155);
	}
	if (g_g.m_buttonsDown & DIR_1)
	{
		Term_keypress(156);
	}
	if (g_g.m_buttonsDown & DIR_2)
	{
		Term_keypress(157);
	}
	if (g_g.m_buttonsDown & DIR_3)
	{
		Term_keypress(158);
	}
}

int ButtonUp(int vkKey)
{
	switch(vkKey)
	{
		case 204: /* 1 */
			g_g.m_buttonsDown &= !DIR_1;
			break;
		case 205: /* 2 */
			g_g.m_buttonsDown &= !DIR_2;
			break;
		case 198: /* 3 */
			g_g.m_buttonsDown &= !DIR_3;
			break;
		case 196: /* A */
			g_g.m_buttonsDown &= !DIR_A;
			break;
		case 197: /* B */
			g_g.m_buttonsDown &= !DIR_B;
			break;
		case 195: /* C */
			g_g.m_buttonsDown &= !DIR_C;
			break;
		case 194: /* Start */
			g_g.m_buttonsDown &= !DIR_START;
			break;
		case 192: /* Aux1 */
			g_g.m_buttonsDown &= !DIR_AUX1;
			break;
		case 193: /* Aux2 */
			g_g.m_buttonsDown &= !DIR_AUX2;
			break;
		default:
			return -2;
	}
		
	if (!g_g.m_buttonsDown)
	{
		KillTimer(g_g.g_mainhWnd, MAIN_KEY_DOWN_TIMER);
	}
	
	return 1;
}


int ButtonPress(int vkKey)
{
	switch(vkKey)
	{
		case 204: /* 1 */
			g_g.m_buttonsDown |= DIR_1;
			break;
		case 205: /* 2 */
			g_g.m_buttonsDown |= DIR_2;
			break;
		case 198: /* 3 */
			g_g.m_buttonsDown |= DIR_3;
			break;
		case 196: /* A */
			g_g.m_buttonsDown |= DIR_A;
			break;
		case 197: /* B */
			g_g.m_buttonsDown |= DIR_B;
			break;
		case 195: /* C */
			g_g.m_buttonsDown |= DIR_C;
			break;
		case 194: /* Start */
			g_g.m_buttonsDown |= DIR_START;
			break;
		case 192: /* Aux1 */
			g_g.m_buttonsDown |= DIR_AUX1;
			break;
		case 193: /* Aux2 */
			g_g.m_buttonsDown |= DIR_AUX2;
			break;
		default:
			return -2;
	}

	SetTimer(g_g.g_mainhWnd, MAIN_KEY_DOWN_TIMER, 100, NULL);

	return 1;
}
