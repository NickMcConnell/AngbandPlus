/* File: plat.c */

/* Purpose: platform-specific stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifdef PLATFORM_WIN
#include <windows.h>
#include <tkWinInt.h>
#include "cmdinfo-dll.h"
#endif

#ifdef PLATFORM_X11
#include <tcl.h>
#endif

#ifdef PLATFORM_WIN

/*
 * This command puts up a font-selection dialog, but is NOT USED.
 */
int AngbandTk_CmdChooseFont(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
    Tk_Window parent = Tk_MainWindow(interp);
	CHOOSEFONT cf;
	LOGFONT lf;
	BOOL result;
	int i, index, oldMode;
	char *t;
	
    /* Initialize members of the CHOOSEFONT structure. */ 
 
    cf.lStructSize = sizeof(CHOOSEFONT); 
    cf.hwndOwner = (HWND)NULL; 
    cf.hDC = (HDC)NULL; 
    cf.lpLogFont = &lf; 
    cf.iPointSize = 0; 
    cf.Flags = CF_SCREENFONTS; 
    cf.rgbColors = RGB(0,0,0); 
    cf.lCustData = 0L; 
    cf.lpfnHook = (LPCFHOOKPROC)NULL; 
    cf.lpTemplateName = (LPSTR)NULL; 

    cf.hInstance = (HINSTANCE) NULL; 
    cf.lpszStyle = (LPSTR)NULL; 
    cf.nFontType = SCREEN_FONTTYPE; 
    cf.nSizeMin = 0; 
    cf.nSizeMax = 0; 

	for (i = 1; i < objc; i++)
	{
		static CONST char *option[] = {"-fixedpitch", "-parent", "-style", NULL};
		
	    if (Tcl_GetIndexFromObj(interp, objv[i], option, "option", 0, 
			&index) != TCL_OK)
		{
			return TCL_ERROR;
	    }

		switch (index)
		{
			case 0: /* -fixedpitch */
				cf.Flags |= CF_FIXEDPITCHONLY;
				break;

			case 1: /* -parent */
				t = Tcl_GetStringFromObj(objv[i+1], NULL);
				parent = Tk_NameToWindow(interp, t, Tk_MainWindow(interp));
				if (parent == NULL)
				{
					return TCL_ERROR;
				}
				i++;
				break;

			case 2: /* -style */
				lf.lfUnderline = 0;
				lf.lfStrikeOut = 0;
				cf.Flags |= CF_EFFECTS;
				break;
		}
	}

	if (Tk_WindowId(parent) == None)
	{
		Tk_MakeWindowExist(parent);
	}
	cf.hwndOwner = Tk_GetHWND(Tk_WindowId(parent));
	
    oldMode = Tcl_SetServiceMode(TCL_SERVICE_ALL);
	result = ChooseFont(&cf);
    (void) Tcl_SetServiceMode(oldMode);

    Tcl_ResetResult(interp);

 	if (result)
	{
		char buf[1024];
		(void) sprintf(buf, "-family {%s} -size %d -weight %s -slant %s "
			"-underline %d -overstrike %d",
			lf.lfFaceName, cf.iPointSize / 10,
			(lf.lfWeight > FW_MEDIUM) ? "bold" : "normal",
			(lf.lfItalic != 0) ? "italic" : "roman",
			(lf.lfUnderline != 0) ? 1 : 0,
			(lf.lfStrikeOut != 0) ? 1 : 0);
		Tcl_SetResult(interp, buf, TCL_VOLATILE);
	}

	return TCL_OK;
}

void *Plat_XWindowToHWND(Window xwin)
{
	return Tk_GetHWND(xwin);
}

/*
 * From tkWinColor.c. I want to know the system colors so I can set
 * a "disabled" toolbar button image to the actual RGB values. Canvas
 * items and widgets can use SytemButtonFace etc as valid color names,
 * but "$imageName put" does not accept them.
 */
typedef struct {
    char *name;
    int index;
} SystemColorEntry;

static SystemColorEntry sysColors[] = {
    {"ButtonFace",		COLOR_BTNFACE},
    {"ButtonHighlight",	COLOR_BTNHIGHLIGHT},
    {"ButtonShadow",	COLOR_BTNSHADOW},
    {"ButtonText",		COLOR_BTNTEXT},
    {NULL,				0}
};

/* SetArrayXXX copied from describe.c */

/*
 * Set a field of a Tcl array variable
 */
static int SetArrayValueLong(Tcl_Interp *interp, char *varName, char *field, long value)
{
	char string[20];

	(void) sprintf(string, "%ld", value);
	if (Tcl_SetVar2(interp, varName, field, string, TCL_LEAVE_ERR_MSG)
		== NULL)
	{
		return TCL_ERROR;
	}
	return TCL_OK;
}

/*
 * Set a field of a Tcl array variable
 */
static int SetArrayValueString(Tcl_Interp *interp, char *varName, char *field, char *value)
{
	if (Tcl_SetVar2(interp, varName, field, value, TCL_LEAVE_ERR_MSG)
		== NULL)
	{
		return TCL_ERROR;
	}
	return TCL_OK;
}

#define BUFSIZE 80

/* This routine adapted from MSDN */
int GetOSVersion(Tcl_Interp *interp, char *arrayName)
{
	OSVERSIONINFOEX osvi;
	BOOL bOsVersionInfoEx;
	char buf[BUFSIZE];

	SetArrayValueString(interp, arrayName, "product", "");
	SetArrayValueString(interp, arrayName, "type", "");
	SetArrayValueString(interp, arrayName, "servicepack", "");
	SetArrayValueString(interp, arrayName, "build", "");

	/* Try calling GetVersionEx using the OSVERSIONINFOEX structure. */
	/* If that fails, try using the OSVERSIONINFO structure. */

	ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

	if( !(bOsVersionInfoEx = GetVersionEx ((OSVERSIONINFO *) &osvi)) )
	{
		osvi.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
		if (! GetVersionEx ( (OSVERSIONINFO *) &osvi) ) 
			return TCL_ERROR;
	}

	switch (osvi.dwPlatformId)
	{
		/* Test for the Windows NT product family. */
		case VER_PLATFORM_WIN32_NT:

			/* Test for the specific product. */
			if ( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 1 )
			{
				if ( osvi.wProductType == VER_NT_WORKSTATION )
					SetArrayValueString(interp, arrayName, "product", "7");
				else
					SetArrayValueString(interp, arrayName, "product", "Server 2008 R2");
			}
			if ( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0 )
			{
				if ( osvi.wProductType == VER_NT_WORKSTATION )
					SetArrayValueString(interp, arrayName, "product", "Vista");
				else
					SetArrayValueString(interp, arrayName, "product", "Server 2008");
			}
			if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 )
				SetArrayValueString(interp, arrayName, "product", "Server 2003");

			if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
				SetArrayValueString(interp, arrayName, "product", "XP");

			if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 )
				SetArrayValueString(interp, arrayName, "product", "2000");

			if ( osvi.dwMajorVersion <= 4 )
				SetArrayValueString(interp, arrayName, "product", "NT");

			/* Test for specific product on Windows NT 4.0 SP6 and later. */
			if( bOsVersionInfoEx )
			{
				/* Test for the workstation type. */
				if ( osvi.wProductType == VER_NT_WORKSTATION )
				{
					if( osvi.dwMajorVersion == 4 )
						SetArrayValueString(interp, arrayName, "type", "Workstation 4.0" );
					else if( osvi.wSuiteMask & VER_SUITE_PERSONAL )
						SetArrayValueString(interp, arrayName, "type", "Home Edition" );
					else
						SetArrayValueString(interp, arrayName, "type", "Professional" );
				}

				/* Test for the server type. */
				else if ( osvi.wProductType == VER_NT_SERVER || 
							osvi.wProductType == VER_NT_DOMAIN_CONTROLLER )
				{
					if( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 )
					{
						if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
							SetArrayValueString(interp, arrayName, "type", "Datacenter Edition" );
						else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
							SetArrayValueString(interp, arrayName, "type", "Enterprise Edition" );
						else if ( osvi.wSuiteMask == VER_SUITE_BLADE )
							SetArrayValueString(interp, arrayName, "type", "Web Edition" );
						else
							SetArrayValueString(interp, arrayName, "type", "Standard Edition" );
					}

					else if( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 )
					{
						if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
							SetArrayValueString(interp, arrayName, "type", "Datacenter Server" );
						else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
							SetArrayValueString(interp, arrayName, "type", "Advanced Server" );
						else
							SetArrayValueString(interp, arrayName, "type", "Server" );
					}

					else  /* Windows NT 4.0  */
					{
						if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
							SetArrayValueString(interp, arrayName, "type", "Server 4.0, Enterprise Edition" );
						else
							SetArrayValueString(interp, arrayName, "type", "Server 4.0");
					}
				}
			}
			else  /* Test for specific product on Windows NT 4.0 SP5 and earlier */
			{
				HKEY hKey;
				DWORD dwBufLen=BUFSIZE;
				LONG lRet;
				char *typeStr = "???";

				lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
					"SYSTEM\\CurrentControlSet\\Control\\ProductOptions",
					0, KEY_QUERY_VALUE, &hKey );
				if( lRet != ERROR_SUCCESS )
					return TCL_ERROR;

				lRet = RegQueryValueEx( hKey, "ProductType", NULL, NULL,
					(LPBYTE) buf, &dwBufLen);
				if( (lRet != ERROR_SUCCESS) || (dwBufLen > BUFSIZE) )
					return TCL_ERROR;

				RegCloseKey( hKey );

				if ( lstrcmpi( "WINNT", buf) == 0 )
					typeStr = "Workstation";
				if ( lstrcmpi( "LANMANNT", buf) == 0 )
					typeStr = "Server";
				if ( lstrcmpi( "SERVERNT", buf) == 0 )
					typeStr = "Advanced Server";

				sprintf(buf, "%s %ld.%ld", typeStr,
					osvi.dwMajorVersion, osvi.dwMinorVersion );
				SetArrayValueString(interp, arrayName, "type", buf);
			}

			/* Display service pack (if any) and build number. */

			if( osvi.dwMajorVersion == 4 && 
				lstrcmpi( osvi.szCSDVersion, "Service Pack 6" ) == 0 )
			{
				HKEY hKey;
				LONG lRet;

				/* Test for SP6 versus SP6a. */
				lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
					"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Hotfix\\Q246009",
					0, KEY_QUERY_VALUE, &hKey );
				if( lRet == ERROR_SUCCESS )
				{
					SetArrayValueString(interp, arrayName, "servicepack", "Service Pack 6a");
					SetArrayValueLong(interp, arrayName, "build", osvi.dwBuildNumber & 0xFFFF);
				}
				else /* Windows NT 4.0 prior to SP6a */
				{
					SetArrayValueString(interp, arrayName, "servicepack", osvi.szCSDVersion);
					SetArrayValueLong(interp, arrayName, "build", osvi.dwBuildNumber & 0xFFFF);
				}

				RegCloseKey( hKey );
			}
			else /* not Windows NT 4.0 */
			{
				SetArrayValueString(interp, arrayName, "servicepack", osvi.szCSDVersion);
				SetArrayValueLong(interp, arrayName, "build", osvi.dwBuildNumber & 0xFFFF);
			}

			break;

		/* Test for the Windows Me/98/95. */
		case VER_PLATFORM_WIN32_WINDOWS:

			if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0)
			{
				SetArrayValueString(interp, arrayName, "product", "95");
				if ( osvi.szCSDVersion[1] == 'C' || osvi.szCSDVersion[1] == 'B' )
					SetArrayValueString(interp, arrayName, "servicepack", "OSR2");
			} 

			if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10)
			{
				SetArrayValueString(interp, arrayName, "product", "98");
				if ( osvi.szCSDVersion[1] == 'A' )
					SetArrayValueString(interp, arrayName, "servicepack", "SE");
			} 

			if (osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90)
			{
				SetArrayValueString(interp, arrayName, "product", "Millennium Edition");
			} 
			break;

		case VER_PLATFORM_WIN32s:

			SetArrayValueString(interp, arrayName, "product", "Win32s");
			break;
	}
	return TCL_OK; 
}

#endif /* PLATFORM_WIN */

/*
 *--------------------------------------------------------------
 *
 * objcmd_system --
 *
 *--------------------------------------------------------------
 */
int
objcmd_system(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
#ifdef PLATFORM_X11

	return TCL_OK;
	
#endif /* PLATFORM_X11 */

#ifdef PLATFORM_WIN

	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"color", "mouse", "workarea",
		"osversion", "load_hack",
#if (TK_MINOR_VERSION == 3) && (TK_RELEASE_SERIAL < 3)
 		"windowicon",
#endif /* version < 8.3.3 */
 		NULL};
	enum {IDX_COLOR, IDX_MOUSE, IDX_WORKAREA, IDX_OSVERSION, IDX_LOAD_HACK,
#if (TK_MINOR_VERSION == 3) && (TK_RELEASE_SERIAL < 3)
		IDX_WINDOWICON
#endif /* version < 8.3.3 */
	} option;

	char *t, buf[128];
	int i;
	RECT rect;

	/* Required number of arguments */
	if (objC < 2)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	/* Get requested option */
	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_COLOR: /* color */
			t = Tcl_GetStringFromObj(objV[2], NULL);
			for (i = 0; sysColors[i].name; i++)
			{
				if (!strcmp(t + 6, sysColors[i].name))
				{
					DWORD color = GetSysColor(sysColors[i].index);
					int red = GetRValue(color);
					int green = GetGValue(color);
					int blue = GetBValue(color);
					(void) sprintf(buf, "%d %d %d", red, green, blue);
					Tcl_SetResult(interp, buf, TCL_VOLATILE);
					return TCL_OK;
				}
			}
			sprintf(buf, "unknown color name \"%s\"", t);
			Tcl_SetResult(interp, buf, TCL_VOLATILE);
			return TCL_ERROR;

		case IDX_MOUSE: /* mouse */
			buf[0] = '\0';
			if (GetAsyncKeyState(VK_LBUTTON) & 0x8000)
			{
				strcat(buf, "button1");
			}
			if (GetAsyncKeyState(VK_MBUTTON) & 0x8000)
			{
				strcat(buf, " button2");
			}
			if (GetAsyncKeyState(VK_RBUTTON) & 0x8000)
			{
				strcat(buf, " button3");
			}
			Tcl_SetResult(interp, buf, TCL_VOLATILE);
			break;

		case IDX_WORKAREA:
			if (!SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0))
			{
				return TCL_ERROR;
			}
			sprintf(buf, "%ld %ld %ld %ld",
				rect.left, rect.top, rect.right, rect.bottom);
			Tcl_SetResult(interp, buf, TCL_VOLATILE);
			break;

		case IDX_LOAD_HACK:
		{
			char *extDllPath, *utfString;
			Tcl_DString extDString;
			HMODULE hModule;

DLL_EXTERN char *UtfToExt_TranslateFileName(Tcl_Interp *interp, char *utfPath,
	Tcl_DString *extDStringPtr);

			/* Get the DLL path */
			utfString = Tcl_GetString(objV[infoCmd->depth + 1]);

			/* Convert to system format */
			extDllPath = UtfToExt_TranslateFileName(interp, utfString, &extDString);

			/* Bad path */
			if (extDllPath == NULL)
			{
				/* Note: Tcl_DStringFree() is called for us */
				return TCL_ERROR;
			}

			/* I had fmod.dll in a Windows system directory, so even though
				I changed PATH to look first in the OmnibandTk/lib directory
				for the fmod.dll I wanted, it got the other one because PATH
				is searched after the system directories. */
			/* The whole point of this is to make Windows look for libraries
				which this library depends on from the library's directory */
			hModule = LoadLibraryEx(extDllPath, NULL, LOAD_WITH_ALTERED_SEARCH_PATH);
			if (hModule == NULL)
			{
				Tcl_AppendResult(interp, "LoadLibraryEx failed for \"",
					extDllPath, "\"", NULL);
			}

			/* Clean up */
			Tcl_DStringFree(&extDString);

			if (hModule == NULL)
				return TCL_ERROR;
			break;
		}

#if (TK_MINOR_VERSION == 3) && (TK_RELEASE_SERIAL < 3)

		/*
		 * Set the window icon to the ANGBAND icon. This was taken from
		 * the "winico" Tcl extension Copyright(c) 1999 Brueckner&Jarosch.
		 */
		case IDX_WINDOWICON:
		{
			char *t = Tcl_GetString(objV[2]);
			HICON hIcon;
			char cmdBuf[1024];
			HWND hWnd;

			hIcon = LoadIcon(GetModuleHandle(NULL), "ANGBAND");
			if (hIcon == NULL)
			{
				Tcl_SetResult(interp, "can't load icon", TCL_STATIC);
				return TCL_ERROR;
			}
			(void) sprintf(cmdBuf, "wm frame %s", t);
			if (Tcl_Eval(interp, cmdBuf) != TCL_OK)
				return TCL_ERROR;
			(void) sscanf(Tcl_GetStringResult(interp), "0x%x",
				(unsigned int *) &hWnd);
			Tcl_ResetResult(interp);
			(void) SendMessage(hWnd, WM_SETICON, ICON_SMALL, (LPARAM) hIcon);
			(void) SendMessage(hWnd, WM_SETICON, ICON_BIG, (LPARAM) hIcon);
			break;
		}

#endif /* version < 8.3.3 */

		/* Same as tcl_platform(os), but detects 95/98/ME/NT/2000 */
		case IDX_OSVERSION:
		{
			char *arrayName;

			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "arrayName");
				return TCL_ERROR;
			}
			arrayName = Tcl_GetStringFromObj(objv[infoCmd->depth + 2], NULL);
			return GetOSVersion(interp, arrayName);
			break;
		}
	}

	return TCL_OK;

#endif /* PLATFORM_WIN */
}

/* Return the number of milliseconds */
unsigned long Milliseconds(void)
{
#ifdef PLATFORM_WIN
	return GetTickCount();
#endif
#ifdef PLATFORM_X11
	return TclpGetClicks() / 1000;
#endif
}

