/* File: main-boot.c */

/* Purpose: stub application for loading AngbandTk and DLLs */

/*
 * Copyright (c) 1997-2005 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <tcl.h>
#include "shlib.h"
#include "stubs.h"
#include "dbwin.h"

#if defined(PLATFORM_SDL) && defined(__WIN32__)
#undef PLATFORM_X11
#define PLATFORM_WIN
#endif /*  */

#ifdef PLATFORM_WIN
#define PATH_SEP_S "\\"
#define PATH_SEP_C '\\'
#endif
#ifdef PLATFORM_X11
#include <sys/stat.h>
#define PATH_SEP_S "/"
#define PATH_SEP_C '/'
#endif

#ifdef PLATFORM_WIN

#include <windows.h>

#define INVALID_FILE_NAME (DWORD)0xFFFFFFFF

static void setargv(
    int *argcPtr,		/* Filled with number of argument strings. */
    char ***argvPtr		/* Filled with argument strings (malloc'd). */
)
{
	char *cmdLine, *p, *arg, *argSpace;
	char **argv;
	int argc, size, inquote, copy, slashes;
	
	cmdLine = GetCommandLine();

	/*
	 * Precompute an overly pessimistic guess at the number of arguments
	 * in the command line by counting non-space spans.
	 */

	size = 2;
	for (p = cmdLine; *p != '\0'; p++)
	{
		if (isspace(*p))
		{
			size++;
			while (isspace(*p))
			{
				p++;
			}
			if (*p == '\0')
			{
				break;
			}
		}
	}
	argSpace = (char *) malloc((unsigned) (size * sizeof(char *) 
		+ strlen(cmdLine) + 1));
	argv = (char **) argSpace;
	argSpace += size * sizeof(char *);
	size--;

	p = cmdLine;
	for (argc = 0; argc < size; argc++)
	{
		argv[argc] = arg = argSpace;
		while (isspace(*p))
		{
			p++;
		}
		if (*p == '\0')
		{
			break;
		}

		inquote = 0;
		slashes = 0;
		while (1)
		{
			copy = 1;
			while (*p == '\\')
			{
				slashes++;
				p++;
			}
			if (*p == '"')
			{
				if ((slashes & 1) == 0)
				{
					copy = 0;
					if ((inquote) && (p[1] == '"'))
					{
						p++;
						copy = 1;
					}
					else
					{
						inquote = !inquote;
					}
				}
				slashes >>= 1;
			}

			while (slashes)
			{
				*arg = '\\';
				arg++;
				slashes--;
			}

			if ((*p == '\0') || (!inquote && isspace(*p)))
			{
				break;
			}
			if (copy != 0)
			{
				*arg = *p;
				arg++;
			}
			p++;
		}
		*arg = '\0';
		argSpace = arg + 1;
	}
	argv[argc] = NULL;

	*argcPtr = argc;
	*argvPtr = argv;
}

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

/* /home/tnb/AngbandTk/./angband --> /home/tnb/AngbandTk/angband */
/* /home/./tnb/foo/../bar --> /home/tnb/bar */
static char *clean_path(char *inp, char *outp)
{
	char buf[1024];
	char *elem[64], *elem2[64];
	int elemc, elem2c;
	int i;

	(void) strcpy(buf, inp);

	/* Split path into elements */
	elemc = 0;
	for (i = 0; buf[i]; i++)
	{
		if (buf[i] == '/')
		{
			elem[elemc++] = buf + i + 1;
			buf[i] = '\0';
		}
	}

	/* Handle . and .. */
	elem2c = 0;
	for (i = 0; i < elemc; i++)
	{
		if (!strcmp(elem[i], ".")) continue;
		if (!strcmp(elem[i], ".."))
		{
			elem2c--;
			continue;
		}
		elem2[elem2c++] = elem[i];
	}

	/* Join path */
	outp[0] = '\0';
	for (i = 0; i < elem2c; i++)
	{
		strcat(outp, "/");
		strcat(outp, elem2[i]);
	}

	return outp;
}

#endif /* PLATFORM_X11 */

#if 0

static void Error(void)
{
	LPVOID lpMsgBuf;
	 
	FormatMessage( 
	    FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
	    NULL,
	    GetLastError(),
	    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
	    (LPTSTR) &lpMsgBuf,
	    0,
	    NULL 
	);
	
	/* Display the string. */
	MessageBox( NULL, lpMsgBuf, "GetLastError", MB_OK | MB_ICONINFORMATION );
	
	/* Free the buffer. */
	LocalFree( lpMsgBuf );
}

#endif

static void Msg(char *fmt, ...)
{
	char msg[1024];
	va_list args;

	va_start(args, fmt);
	vsprintf(msg, fmt, args);
	va_end(args);

#ifdef PLATFORM_WIN

	MessageBox(NULL, msg, "Error Starting Angband",
		MB_ICONEXCLAMATION | MB_OK);

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11
	fprintf(stderr, "%s\n", msg);
#endif
}

static int Exists(char *path, int isDir)
{
#ifdef PLATFORM_WIN

	DWORD attrib;

	/* Examine */
	attrib = GetFileAttributes(path);

	/* Require valid filename */
	if (attrib == INVALID_FILE_NAME) return 0;

	/* Require directory */
	if (((attrib & FILE_ATTRIBUTE_DIRECTORY) != 0) != isDir) return 0;

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

	struct stat statBuf;

	if (stat(path, &statBuf)) return 0;
	if ((S_ISDIR(statBuf.st_mode) != 0) != isDir) return 0;

#endif /* PLATFORM_X11 */

	return 1;
}

static ShLibRef TryLib(char *path, char *msg)
{
	ShLibRef shLib;

	if (!Exists(path, 0))
	{
		Msg("Couldn't find %s.%s%s", path, msg ? "\n" : "", msg ? msg : "");
		return NULL;
	}

	shLib = ShLib_Load(path);
	if (shLib == NULL)
	{
		Msg("Couldn't load %s.\n%s%s%s", path, ShLib_Error(),
			msg ? "\n" : "", msg ? msg : "");
		return NULL;
	}
dbwin("loaded %s\n", path);
	return shLib;
}

int EvalFile(Tcl_Interp *interp, char *extFileName)
{
	char *utfFileName;
	Tcl_DString dString;
	int result;

	utfFileName = Tcl_ExternalToUtfDString(NULL, extFileName, -1, &dString);
	result = Tcl_EvalFile(interp, utfFileName);
	Tcl_DStringFree(&dString);
	return result;
}

/* The full path of the OmnibandTk directory */
char gCWD[1024] = "";

/* The full path of the variant DLL to load */
char gTheDll[1024] = "";

/* boot dll */
int ObjCmd_Boot(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	char *extName, *utfName;
	Tcl_DString dString;
	int i;

	if (objc != 2)
	{
		Tcl_WrongNumArgs(interp, 1, objv, "dll");
		return TCL_ERROR;
	}

	utfName = Tcl_GetString(objv[1]);
	extName = Tcl_UtfToExternalDString(NULL, utfName, -1, &dString);

	/* Save the path, converting to Win32 separators */
	for (i = 0; extName[i]; i++)
	{
		char ch = extName[i];
#ifdef PLATFORM_WIN
		if (ch == '/')
			ch = '\\';
#endif /* PLATFORM_WIN */
		gTheDll[i] = ch;
	}
	gTheDll[i] = '\0';

	Tcl_DStringFree(&dString);

	return TCL_OK;
}

int Run(int argc, char **argv)
{
	ShLibRef shLib;
	char path[1024], *p;

	struct stub {
		int (*Main)(int, char **, char *, char *);
	} stub;
	t_stub init[2] = {
		{STUB_DESC(Main, struct stub)},
		{NULL, 0, 0}
	};

	/* Load the variant DLL */
	shLib = TryLib(gTheDll, NULL);
	if (shLib == NULL)
	{
		return 1;
	}

	/* Get the address of Main() */
	if (Stub_Load(shLib, init, &stub) == -1)
	{
		Msg("%s: %s", gTheDll, Stub_Error());
		return 1;
	}

	/* Get variant directory by stripping off DLL name */
	(void) strcpy(path, gTheDll);
	p = strrchr(path, PATH_SEP_C);
	*p = '\0';

	/* Run the game */
	return (*stub.Main)(argc, argv, gCWD, path);
}

#ifdef PLATFORM_WIN
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
	LPSTR lpCmdLine, int nCmdShow)
#endif
#ifdef PLATFORM_X11
int main(int argc, char **argv)
#endif
{
#ifdef PLATFORM_WIN
	int argc;
	char **argv;
#endif /* PLATFORM_WIN */
	char cwd[1024], libPath[1024], tclPath[1024];
	char buf[2048], path[2048], *p;
	int i;
	int tcl_major, tcl_minor, tcl_patch;
	FILE *fp;
	ShLibRef shLib;
	Tcl_Interp *interp;

	/* These structs list the funcs to load from common.dll */
	struct stub {
		Tcl_Interp *(*TclTk_Init)(int, char **);
	} stub;
	t_stub init[2] = {
		{STUB_DESC(TclTk_Init, struct stub)},
		{NULL, 0, 0}
	};

#ifdef USE_EXCHNDL
	LoadLibrary("exchndl.dll");
#endif /* USE_EXCHNDL */

#ifdef PLATFORM_WIN

	/* Split command-line into argc,argv */
	setargv(&argc, &argv);

	/* Get the complete path of this application */
	GetModuleFileName(hInstance, cwd, 1024);

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

	/* On Linux, full pathname isn't given when starting from Bash */
	if (argv[0][0] != '/')
	{
		(void) getcwd(cwd, 1024);
		(void) strcat(cwd, "/");

		/* Note: This may give us "/home/tnb/bin/../AngbandTk/angband" */
		(void) strcat(cwd, argv[0]);
	}
	else
	{
		(void) strcpy(cwd, argv[0]);
	}

	/* Eliminate . and .. from path */
	clean_path(cwd, cwd);

#endif /* PLATFORM_X11 */

	/* Strip off the application name */
	p = strrchr(cwd, PATH_SEP_C);
	*p = '\0';

	/* Save cwd, it is passed to variant DLL */
	(void) strcpy(gCWD, cwd);

	/* Build path to \lib */
	(void) sprintf(libPath, "%s%s%s", cwd, PATH_SEP_S, "lib");

	/* Verify \lib exists */
	if (!Exists(libPath, 1))
	{
		Msg("Can't find %s", libPath);
		return 1;
	}

	/* Build path to tcl-version.txt */
	(void) sprintf(path, "%s%s%s", libPath, PATH_SEP_S, "tcl-version.txt");
	if (!Exists(path, 0))
	{
		Msg("Can't find %s", path);
		return 1;
	}

	/* Open the file */
	fp = fopen(path, "rt");

	/* Couldn't open the file */
	if (fp == NULL)
	{
		Msg("Couldn't open %s", path);
		return 1;
	}

	/* Read 1 line */
	if (fgets(buf, 128, fp) == NULL)
	{
		fclose(fp);
		Msg("Couldn't read %s", path);
		return 1;
	}
	
	/* Scan version numbers */
	if (sscanf(buf, "%d.%d.%d", &tcl_major, &tcl_minor, &tcl_patch) != 3)
	{
		fclose(fp);
		Msg("Bad version string \"%s\" in %s", buf, path);
		return 1;
	}

	/* close */
	fclose(fp);

#ifdef PLATFORM_WIN

	/* Look for Tcl in local lib\TclTk-x.y.z directory, then ..\TclTk-x.y.z */
	(void) sprintf(tclPath, "%s%slib%sTclTk-%d.%d.%d",
		cwd, PATH_SEP_S, PATH_SEP_S, tcl_major, tcl_minor, tcl_patch);
	if (!Exists(tclPath, 1))
	{
		(void) strcpy(tclPath, cwd);
		p = strrchr(tclPath, PATH_SEP_C);
		(void) sprintf(p, "%sTclTk-%d.%d.%d", PATH_SEP_S,
			tcl_major, tcl_minor, tcl_patch);
		if (!Exists(tclPath, 1))
		{
			Msg("Can't find TclTk-%d.%d.%d directory.\n",
				tcl_major, tcl_minor, tcl_patch);
			return 1;
		}
	}
#if 1 /* July 14 2004 */
	if ((p = getenv("PATH")) == NULL)
	{
		Msg("Couldn't read value of PATH environment variable");
		return 1;
	}

	/* set PATH=OmnibandTk\lib;TclTk\bin;$PATH */
	(void) sprintf(path, "PATH=%s;%s%sbin;%s", libPath, tclPath, PATH_SEP_S, p);
	if (putenv(path) == -1)
	{
		Msg("Couldn't set value of PATH environment variable");
		return 1;
	}

#else /* WTF? This doesn't work on XP? */
	/* If sizeof(buf) < given length, it fails */
	if (GetEnvironmentVariable("PATH", buf, 2048) == 0)
	{
		Msg("Couldn't read value of PATH environment variable");
		return 1;
	}

	/* set PATH=OmnibandTk\lib;TclTk\bin;$PATH */
	(void) sprintf(path, "%s;%s%sbin;%s", libPath, tclPath, PATH_SEP_S, buf);
	if (SetEnvironmentVariableW(L"PATH", path) == 0)
	{
		if (SetEnvironmentVariableA("PATH", path) == 0)
		{
			Msg("Couldn't set value of PATH environment variable");
			return 1;
		}
	}
#endif

#if 0 /* Aug 10 2004 */
	/* set TCL_LIBRARY=TclTk\lib\tclNN */
	(void) sprintf(path, "TCL_LIBRARY=%s%slib%stcl%d.%d",
		tclPath, PATH_SEP_S, PATH_SEP_S, tcl_major, tcl_minor);
	if (putenv(path) == -1)
	{
		Msg("Couldn't set value of TCL_LIBRARY environment variable");
		return 1;
	}
#endif

	/* Try tclNNg.dll (debug version) */
	(void) sprintf(path, "%s%sbin%stcl%d%dg.dll", tclPath, PATH_SEP_S,
		PATH_SEP_S, tcl_major, tcl_minor);
	if (Exists(path, 0))
	{
		if (TryLib(path, NULL) == NULL)
			return 1;
	}
	else
	{	
		/* Load tclNN.dll */
		(void) sprintf(path, "%s%sbin%stcl%d%d.dll", tclPath, PATH_SEP_S,
			PATH_SEP_S, tcl_major, tcl_minor);
		if (TryLib(path, NULL) == NULL)
		{
			return 1;
		}
	}

	/* Try tkNNg.dll (debug version) */
	(void) sprintf(path, "%s%sbin%stk%d%dg.dll", tclPath, PATH_SEP_S,
		PATH_SEP_S, tcl_major, tcl_minor);
	if (Exists(path, 0))
	{
		if (TryLib(path, NULL) == NULL)
			return 1;
	}
	else
	{	
		/* Load tkNN.dll */
		(void) sprintf(path, "%s%sbin%stk%d%d.dll", tclPath, PATH_SEP_S,
			PATH_SEP_S, tcl_major, tcl_minor);
		if (TryLib(path, NULL) == NULL)
		{
			return 1;
		}
	}

	/* Load zlib.dll */
	(void) sprintf(path, "%s%szlib1.dll", libPath, PATH_SEP_S);
	if (TryLib(path, NULL) == NULL)
	{
		return 1;
	}

#endif /* PLATFORM_WIN */

	/* Load common.dll */
#ifdef PLATFORM_WIN
	(void) sprintf(path, "%s%scommon%s", libPath, PATH_SEP_S, SHLIB_SUF);
#endif
#ifdef PLATFORM_X11
	(void) sprintf(path, "%s%scommon%s", libPath, PATH_SEP_S, SHLIB_SUF);
#endif
	shLib = TryLib(path, NULL);
	if (shLib == NULL)
	{
		return 1;
	}

	/* Get address of TclTk_Init() in common.dll */
	if (Stub_Load(shLib, init, &stub) == -1)
	{
		Msg("%s: %s", path, Stub_Error());
		return 1;
	}

	/* Initialize Tcl */
/* Hack -- Ignore all arguments except app name */
	interp = (*stub.TclTk_Init)(argc, argv);
	if (interp == NULL)
	{
		Msg("Failed to initialize Tcl");
		return 1;
	}

#ifdef USE_TCL_STUBS

	(void) sprintf(buf, "%d.%d", tcl_major, tcl_minor);
	if (Tcl_InitStubs(interp, buf, 1) == NULL)
	{
		Msg("Failed to intialize Tcl %s stubs", buf);
		return 1;
	}

#endif /* USE_TCL_STUBS */

	/* If -variant option was given, load that variant */
	for (i = 1; i < argc; i++)
	{
		if (!strcmp(argv[i], "-variant"))
		{
			/* OmnibandTk\variant\AngbandTk\angband.dll */
			(void) sprintf(gTheDll, "%s%svariant%s%s%sangband%s",
				cwd, PATH_SEP_S, PATH_SEP_S, argv[i + 1], PATH_SEP_S,
				SHLIB_SUF);
			return Run(argc, argv);
		}
	}

	Tcl_CreateObjCommand(interp, "boot", ObjCmd_Boot, NULL, NULL);

	/* Source OmnibandTk\tk\boot.tcl */
	(void) sprintf(path, "%s%stk%sboot.tcl", cwd, PATH_SEP_S, PATH_SEP_S);
	if (EvalFile(interp, path) != TCL_OK)
	{
#if 1 /* June 26 2004 */
		CONST char *errorInfo = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
		Msg("Error sourcing %s\n\n%s", path, errorInfo);
#else /* June 26 2004 */
		Msg("Error sourcing %s\n%s", path, Tcl_GetStringResult(interp));
#endif /* June 26 2004 */
		return 1;
	}

	if (!gTheDll[0])
	{
		Msg("No variant DLL was specified");
		return 1;
	}

	return Run(argc, argv);
}
