/* File: TclTk.c */

/* Purpose: embedded Tcl */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifdef PLATFORM_WIN

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <windows.h>
#include <tk.h>
#include "tcltk-dll.h"

#define ALLOW_TK_CONSOLE

#ifdef ALLOW_TK_CONSOLE
extern void TkConsoleCreate_ _ANSI_ARGS_((void));
EXTERN int		TkConsoleInit(Tcl_Interp *interp);
#endif /* */
/*
static void		setargv _ANSI_ARGS_((int *argcPtr, char ***argvPtr));
*/
static void		WishPanic _ANSI_ARGS_(TCL_VARARGS(CONST char *,format));

/* The interpreter. The app can call TclTk_Interp() to get it */
static Tcl_Interp *g_interp = NULL;

static void CloseStdHandle(DWORD nStdHandle)
{
	HANDLE handle;

	handle = GetStdHandle(nStdHandle);
	if ((handle == INVALID_HANDLE_VALUE) ||
		(handle == 0) ||
		(GetFileType(handle) == FILE_TYPE_UNKNOWN))
	{
		return;
	}

	CloseHandle(handle);
}

#if 0
/* Did this because atexit() doesn't seem to work */
static void ExitProc_db_dump_allocs(ClientData clientData)
{
	extern void db_heap_check(int mark);
	extern void db_dump_allocs();
	dbwin(">>> db_dump_allocs\n");
	db_heap_check(0);
	db_dump_allocs();
}
#endif

Tcl_Interp *TclTk_Init(int argc, CONST char **argv)
{
	char *p, *args, *fileName;
	char buffer[MAX_PATH], buf[20], *t;
	size_t length;
	Tcl_DString argString;
	Tcl_Interp *interp;

	/*** From tk80/win/winMain.c ***/

	Tcl_SetPanicProc(WishPanic);

	/* GDB chokes on this call. Is it safe to remove? */
/*	setlocale(LC_ALL, "C"); */

	/* This call does nothing on Win32 systems */
	SetMessageQueue(64);

	GetModuleFileName(NULL, buffer, sizeof(buffer));
	argv[0] = buffer;
	for (p = buffer; *p != '\0'; p++)
	{
		if (*p == '\\')
		{
			*p = '/';
		}
	}

	/*** From tk80/generic/TkMain.c ***/

	Tcl_FindExecutable(argv[0]);

	/* According to Hobbs, this should come after Tcl_FindExecutable() */
	interp = Tcl_CreateInterp();

	Tcl_InitMemory(interp); /* Mar 7 2009 */

	/* XXX Hack -- When run from a BAT file, the input/output doesn't
	 * go to the Tk Console. */
	CloseStdHandle(STD_INPUT_HANDLE);
	CloseStdHandle(STD_OUTPUT_HANDLE);
	CloseStdHandle(STD_ERROR_HANDLE);
	
#ifdef ALLOW_TK_CONSOLE
	Tk_InitConsoleChannels(interp);
#endif /* ALLOW_TK_CONSOLE */

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
		fileName = (char *) argv[1];
		argc--;
		argv++;
	}

	args = Tcl_Merge(argc-1, argv+1);
	Tcl_ExternalToUtfDString(NULL, args, -1, &argString);
	Tcl_SetVar(interp, "argv", Tcl_DStringValue(&argString),
		TCL_GLOBAL_ONLY);
	Tcl_DStringFree(&argString);
	ckfree(args);
	sprintf(buf, "%d", argc-1);

	if (fileName == NULL)
	{
		Tcl_ExternalToUtfDString(NULL, argv[0], -1, &argString);
	}
	else
	{
		fileName = Tcl_ExternalToUtfDString(NULL, fileName, -1, &argString);
	}
	Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
	Tcl_SetVar(interp, "argv0", Tcl_DStringValue(&argString),
		TCL_GLOBAL_ONLY);
    Tcl_DStringFree(&argString);

	Tcl_SetVar(interp, "tcl_interactive",
		(fileName == NULL) ? "1" : "0", TCL_GLOBAL_ONLY);

	/*** from tk80/win/winMain.c (Tcl_AppInit) ***/
	
	if (Tcl_Init(interp) != TCL_OK)
	{
		goto error;
	}
#if 0
/* Do this before Tk_Init(), because it creates an exit handler to delete
 all windows, and I want this called *after* deleting windows */
Tcl_CreateExitHandler(ExitProc_db_dump_allocs, NULL);
#endif
	if (Tk_Init(interp) != TCL_OK)
	{
		goto error;
	}
	Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

#ifdef ALLOW_TK_CONSOLE
	Tk_CreateConsoleWindow(interp);
#endif /* ALLOW_TK_CONSOLE */

	/* Require the same Tcl version */
	t = (char *) Tcl_GetVar(interp, "tcl_patchLevel", TCL_GLOBAL_ONLY);
	if (!t || strcmp(t, TCL_PATCH_LEVEL))
	{
		WishPanic("This version of Angband was compiled for use\nwith "
			"Tcl version %s, not %s.", TCL_PATCH_LEVEL, t);
	}

	/* Require the same Tk version */
	t = (char *) Tcl_GetVar(interp, "tk_patchLevel", TCL_GLOBAL_ONLY);
	if (!t || strcmp(t, TK_PATCH_LEVEL))
	{
		WishPanic("This version of Angband was compiled for use\nwith "
			"Tk version %s, not %s.", TK_PATCH_LEVEL, t);
	}
	
	Tcl_ResetResult(interp);

	g_interp = interp;

	return interp;

error:
	WishPanic(interp->result);
	return NULL;
}

static void WishPanic TCL_VARARGS_DEF(CONST char *,arg1)
{
    va_list argList;
    char buf[1024];
    char *format;
    
    format = (char *) TCL_VARARGS_START(char *,arg1,argList);
    vsprintf(buf, format, argList);

    MessageBeep(MB_ICONEXCLAMATION);
    MessageBox(NULL, buf, "Fatal Error in Angband",
	    MB_ICONSTOP | MB_OK | MB_TASKMODAL | MB_SETFOREGROUND);
#ifdef _MSC_VER
    _asm {
        int 3
    }
#endif
    ExitProcess(1);
}

#if 0

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
	argSpace = (char *) ckalloc((unsigned) (size * sizeof(char *) 
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

#endif /* 0 */

void TclTk_Exit(Tcl_Interp *interp)
{
	Tcl_DeleteInterp(interp);
	Tcl_Exit(0);
}

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

#include <tkInt.h>
#include <unistd.h>
#include "tcltk-dll.h"

typedef struct ThreadSpecificData {
    Tcl_Interp *interp;         /* Interpreter for this thread. */
    Tcl_DString command;        /* Used to assemble lines of terminal input
				 * into Tcl commands. */
    Tcl_DString line;           /* Used to read the next line from the
				 * terminal input. */
    int tty;                    /* Non-zero means standard input is a 
				 * terminal-like device.  Zero means it's
				 * a file. */
} ThreadSpecificData;
Tcl_ThreadDataKey dataKey;

static void		Prompt _ANSI_ARGS_((Tcl_Interp *interp, int partial));
static void		StdinProc _ANSI_ARGS_((ClientData clientData,
			    int mask));

/* The interpreter. The app can call TclTk_Interp() to get it */
static Tcl_Interp *g_interp = NULL;

Tcl_Interp *TclTk_Init(int argc, CONST char **argv)
{
	char *args;
	char buf[20];
	Tcl_DString argString;
	Tcl_Interp *interp;
    Tcl_Channel inChannel, outChannel;
    ThreadSpecificData *tsdPtr;

	interp = Tcl_CreateInterp();

	Tcl_InitMemory(interp); /* Mar 7 2009 */

	/*** From tk83/generic/TkMain.c ***/

	tsdPtr = (ThreadSpecificData *) 
		Tcl_GetThreadData(&dataKey, sizeof(ThreadSpecificData));

	Tcl_FindExecutable(argv[0]);
	tsdPtr->interp = interp;

	args = Tcl_Merge(argc-1, argv+1);
	Tcl_ExternalToUtfDString(NULL, args, -1, &argString);
	Tcl_SetVar(interp, "argv", Tcl_DStringValue(&argString), TCL_GLOBAL_ONLY);
	Tcl_DStringFree(&argString);
	Tcl_Free(args);
	sprintf(buf, "%d", argc-1);

	Tcl_ExternalToUtfDString(NULL, argv[0], -1, &argString);
	Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
	Tcl_SetVar(interp, "argv0", Tcl_DStringValue(&argString), TCL_GLOBAL_ONLY);
	Tcl_DStringFree(&argString);

	tsdPtr->tty = isatty(0);
	Tcl_SetVar(interp, "tcl_interactive",
		tsdPtr->tty ? "1" : "0", TCL_GLOBAL_ONLY);

	/*** from tk83/unix/tkAppInit.c (Tcl_AppInit) ***/

	if (Tcl_Init(interp) != TCL_OK)
	{
		goto error;
	}
	if (Tk_Init(interp) != TCL_OK)
	{
		goto error;
	}
	Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

	/*** From tk83/generic/TkMain.c (again) ***/

	/*
	 * Establish a channel handler for stdin.
	 */

	inChannel = Tcl_GetStdChannel(TCL_STDIN);
	if (inChannel)
	{
		Tcl_CreateChannelHandler(inChannel, TCL_READABLE, StdinProc,
			(ClientData) inChannel);
	}
	if (tsdPtr->tty)
	{
		Prompt(interp, 0);
	}

	outChannel = Tcl_GetStdChannel(TCL_STDOUT);
	if (outChannel)
	{
		Tcl_Flush(outChannel);
	}

    Tcl_DStringInit(&tsdPtr->command);
    Tcl_DStringInit(&tsdPtr->line);
	Tcl_ResetResult(interp);

	g_interp = interp;

	return interp;

error:
	TkpDisplayWarning(Tcl_GetStringResult(interp), "TclTk_Init Failed!");
	Tcl_DeleteInterp(interp);
	Tcl_Exit(1);
	return NULL;
}

void TclTk_Exit(Tcl_Interp *interp)
{
	Tcl_DeleteInterp(interp);
	Tcl_Exit(0);
}

/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	standard input becomes readable.  It grabs the next line of
 *	input characters, adds them to a command being assembled, and
 *	executes the command if it's complete.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's
 *	typed.
 *
 *----------------------------------------------------------------------
 */

    /* ARGSUSED */
static void
StdinProc(clientData, mask)
    ClientData clientData;		/* Not used. */
    int mask;				/* Not used. */
{
    static int gotPartial = 0;
    char *cmd;
    int code, count;
    Tcl_Channel chan = (Tcl_Channel) clientData;
    ThreadSpecificData *tsdPtr = (ThreadSpecificData *) 
            Tcl_GetThreadData(&dataKey, sizeof(ThreadSpecificData));
    Tcl_Interp *interp = tsdPtr->interp;

    count = Tcl_Gets(chan, &tsdPtr->line);

    if (count < 0) {
	if (!gotPartial) {
	    if (tsdPtr->tty) {
		Tcl_Exit(0);
	    } else {
		Tcl_DeleteChannelHandler(chan, StdinProc, (ClientData) chan);
	    }
	    return;
	} 
    }

    (void) Tcl_DStringAppend(&tsdPtr->command, Tcl_DStringValue(
            &tsdPtr->line), -1);
    cmd = Tcl_DStringAppend(&tsdPtr->command, "\n", -1);
    Tcl_DStringFree(&tsdPtr->line);
    if (!Tcl_CommandComplete(cmd)) {
        gotPartial = 1;
        goto prompt;
    }
    gotPartial = 0;

    /*
     * Disable the stdin channel handler while evaluating the command;
     * otherwise if the command re-enters the event loop we might
     * process commands from stdin before the current command is
     * finished.  Among other things, this will trash the text of the
     * command being evaluated.
     */

    Tcl_CreateChannelHandler(chan, 0, StdinProc, (ClientData) chan);
    code = Tcl_RecordAndEval(interp, cmd, TCL_EVAL_GLOBAL);
    
    chan = Tcl_GetStdChannel(TCL_STDIN);
    if (chan) {
	Tcl_CreateChannelHandler(chan, TCL_READABLE, StdinProc,
		(ClientData) chan);
    }
    Tcl_DStringFree(&tsdPtr->command);
    if (Tcl_GetStringResult(interp)[0] != '\0') {
	if ((code != TCL_OK) || (tsdPtr->tty)) {
	    chan = Tcl_GetStdChannel(TCL_STDOUT);
	    if (chan) {
		Tcl_WriteObj(chan, Tcl_GetObjResult(interp));
		Tcl_WriteChars(chan, "\n", 1);
	    }
	}
    }

    /*
     * Output a prompt.
     */

    prompt:
    if (tsdPtr->tty) {
	Prompt(interp, gotPartial);
    }
    Tcl_ResetResult(interp);
}

/*
 *----------------------------------------------------------------------
 *
 * Prompt --
 *
 *	Issue a prompt on standard output, or invoke a script
 *	to issue the prompt.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A prompt gets output, and a Tcl script may be evaluated
 *	in interp.
 *
 *----------------------------------------------------------------------
 */

static void
Prompt(interp, partial)
    Tcl_Interp *interp;			/* Interpreter to use for prompting. */
    int partial;			/* Non-zero means there already
					 * exists a partial command, so use
					 * the secondary prompt. */
{
    char *promptCmd;
    int code;
    Tcl_Channel outChannel, errChannel;

    promptCmd = Tcl_GetVar(interp,
	partial ? "tcl_prompt2" : "tcl_prompt1", TCL_GLOBAL_ONLY);
    if (promptCmd == NULL) {
defaultPrompt:
	if (!partial) {

            /*
             * We must check that outChannel is a real channel - it
             * is possible that someone has transferred stdout out of
             * this interpreter with "interp transfer".
             */

	    outChannel = Tcl_GetChannel(interp, "stdout", NULL);
            if (outChannel != (Tcl_Channel) NULL) {
                Tcl_WriteChars(outChannel, "% ", 2);
            }
	}
    } else {
	code = Tcl_Eval(interp, promptCmd);
	if (code != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		    "\n    (script that generates prompt)");
            /*
             * We must check that errChannel is a real channel - it
             * is possible that someone has transferred stderr out of
             * this interpreter with "interp transfer".
             */
            
	    errChannel = Tcl_GetChannel(interp, "stderr", NULL);
            if (errChannel != (Tcl_Channel) NULL) {
                Tcl_WriteObj(errChannel, Tcl_GetObjResult(interp));
                Tcl_WriteChars(errChannel, "\n", 1);
            }
	    goto defaultPrompt;
	}
    }
    outChannel = Tcl_GetChannel(interp, "stdout", NULL);
    if (outChannel != (Tcl_Channel) NULL) {
        Tcl_Flush(outChannel);
    }
}

#endif /* PLATFORM_X11 */

Tcl_Interp *TclTk_Interp(void)
{
	return g_interp;
}
