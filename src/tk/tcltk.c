/* File: TclTk.c */

/* Purpose: embedded Tcl */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "tnb.h"

#ifdef PLATFORM_WIN
#include <windows.h>

static void WishPanic TCL_VARARGS_DEF(char *,arg1)
{
    va_list argList;
    char buf[1024];
    char *format;
    
    format = TCL_VARARGS_START(char *, arg1, argList);
    vstrnfmt(buf, 1024, format, &argList);

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

Tcl_Interp *TclTk_Init(char **argv)
{
	char *p;
	char buffer[MAX_PATH], *t;
	size_t length;
	Tcl_Interp *interp;

	/*** From tk80/win/winMain.c ***/

	Tcl_SetPanicProc(WishPanic);

	/* This call does nothing on Win32 systems */
	SetMessageQueue(64);

	GetModuleFileName(NULL, buffer, sizeof(buffer));
	for (p = buffer; *p != '\0'; p++)
	{
		if (*p == '\\')
		{
			*p = '/';
		}
	}

	/*** From tk80/generic/TkMain.c ***/

	Tcl_FindExecutable(buffer);

	/* According to Hobbs, this should come after Tcl_FindExecutable() */
	interp = Tcl_CreateInterp();

	/* XXX Hack -- When run from a BAT file, the input/output doesn't
	 * go to the Tk Console. */
	CloseStdHandle(STD_INPUT_HANDLE);
	CloseStdHandle(STD_OUTPUT_HANDLE);
	CloseStdHandle(STD_ERROR_HANDLE);
	
	Tcl_SetVar(interp, "tcl_interactive", "1", TCL_GLOBAL_ONLY);

	/*** from tk80/win/winMain.c (Tcl_AppInit) ***/
	
	if (Tcl_Init(interp) != TCL_OK)
	{
		goto error;
	}
	if (Tk_Init(interp) != TCL_OK)
	{
		goto error;
	}
	Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

	/* Require the same Tcl version */
	t = Tcl_GetVar(interp, "tcl_patchLevel", TCL_GLOBAL_ONLY);
	if (!t || strcmp(t, TCL_PATCH_LEVEL))
	{
		WishPanic("This version of Angband was compiled for use\nwith "
			"Tcl version %s, not %s.", TCL_PATCH_LEVEL, t);
	}

	/* Require the same Tk version */
	t = Tcl_GetVar(interp, "tk_patchLevel", TCL_GLOBAL_ONLY);
	if (!t || strcmp(t, TK_PATCH_LEVEL))
	{
		WishPanic("This version of Angband was compiled for use\nwith "
			"Tk version %s, not %s.", TK_PATCH_LEVEL, t);
	}
	
	Tcl_ResetResult(interp);

	return interp;

error:
	WishPanic(interp->result);
	return NULL;
}

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11
#include <unistd.h>

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

/*
 * Interpreter to use for prompting.
 * Non-zero 'partial' means there already
 * exists a partial command, so use
 * the secondary prompt.
 */
static void Prompt(Tcl_Interp *interp, int partial)
{
    cptr promptCmd;
    int code;
    Tcl_Channel outChannel, errChannel;

    promptCmd = Tcl_GetVar(interp,
	(partial ? "tcl_prompt2" : "tcl_prompt1"), TCL_GLOBAL_ONLY);
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

static void StdinProc(ClientData clientData, int mask)
{
    static int gotPartial = 0;
    char *cmd;
    int code, count;
    Tcl_Channel chan = (Tcl_Channel) clientData;
    ThreadSpecificData *tsdPtr = (ThreadSpecificData *) 
            Tcl_GetThreadData(&dataKey, sizeof(ThreadSpecificData));
    Tcl_Interp *interp = tsdPtr->interp;

	/* Hack - ignore parameter */
	(void) mask;

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

extern void TkpDisplayWarning(const char * msg, const char * title);

Tcl_Interp *TclTk_Init(cptr *argv)
{
	Tcl_Interp *interp;
    Tcl_Channel inChannel, outChannel;
    ThreadSpecificData *tsdPtr;

	interp = Tcl_CreateInterp();

	/*** From tk83/generic/TkMain.c ***/

	tsdPtr = (ThreadSpecificData *) 
		Tcl_GetThreadData(&dataKey, sizeof(ThreadSpecificData));

	Tcl_FindExecutable(argv[0]);
	tsdPtr->interp = interp;

	tsdPtr->tty = isatty(0);
	Tcl_SetVar(interp, "tcl_interactive",
		(tsdPtr->tty ? "1" : "0"), TCL_GLOBAL_ONLY);

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

	return interp;

error:
	TkpDisplayWarning(Tcl_GetStringResult(interp), "TclTk_Init Failed!");
	Tcl_DeleteInterp(interp);
	Tcl_Exit(1);
	return NULL;
}


#endif /* PLATFORM_X11 */
