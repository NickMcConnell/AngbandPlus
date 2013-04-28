/* File: TclTk.c */

/* Purpose: Tcl initialization for X-Windows */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tkInt.h>

#include "angband.h"

#include "tnb.h"

Tcl_Interp *g_interp;

void InitTclTk(int argc, char **argv)
{
	char *args, *fileName;
	char buf[20];
	size_t length;

	/*** From tk80/generic/TkMain.c ***/
	Tcl_FindExecutable(argv[0]);

	g_interp = Tcl_CreateInterp();

	fileName = NULL;
	if (argc > 1)
	{
		length = strlen(argv[1]);
		if ((length >= 2) && (strncmp(argv[1], "-file", length) == 0))
		{
			argc--;
			argv++;
		}
	}
	if ((argc > 1) && (argv[1][0] != '-'))
	{
		fileName = argv[1];
		argc--;
		argv++;
	}

	args = Tcl_Merge(argc-1, argv+1);
	Tcl_SetVar(g_interp, "argv", args, TCL_GLOBAL_ONLY);
	ckfree(args);
	sprintf(buf, "%d", argc-1);
	Tcl_SetVar(g_interp, "argc", buf, TCL_GLOBAL_ONLY);
	Tcl_SetVar(g_interp, "argv0", (fileName != NULL) ? fileName : argv[0],
		TCL_GLOBAL_ONLY);

	Tcl_SetVar(g_interp, "tcl_interactive",
		((fileName == NULL) && isatty(0)) ? "1" : "0", TCL_GLOBAL_ONLY);

	/*** from tk80/unix/tkAppInit.c (Tcl_AppInit) ***/
	if (Tcl_Init(g_interp) == TCL_ERROR)
	{
		goto error;
	}
	if (Tk_Init(g_interp) == TCL_ERROR)
	{
		goto error;
	}
	Tcl_StaticPackage(g_interp, "Tk", Tk_Init, Tk_SafeInit);

	/* Require the same Tcl/Tk version */
	if (1)
	{
		char *t = Tcl_GetVar(g_interp, "tk_patchLevel", TCL_GLOBAL_ONLY);
		if (!t || strcmp(t, TK_PATCH_LEVEL))
		{
			char msg[160];
			(void) sprintf(msg, "This version of Angband was compiled for "
				"use\nwith Tcl/Tk version %s, not %s.", TK_PATCH_LEVEL, t);
			TkpDisplayWarning(msg, "Fatal Error In Angband");
		    Tcl_DeleteInterp(g_interp);
		    Tcl_Exit(1);
		}
	}
	
	Tcl_ResetResult(g_interp);

	return;

error:
	TkpDisplayWarning(Tcl_GetStringResult(g_interp), "Fatal Error In Angband");
    Tcl_DeleteInterp(g_interp);
    Tcl_Exit(1);
}

void ExitTclTk(void)
{
	Tcl_DeleteInterp(g_interp);
	Tcl_Exit(0);
}

