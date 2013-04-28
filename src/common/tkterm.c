/* File: tkterm.c */

/* Purpose: Tk terminal widget */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "tnb.h"

extern errr Term_user_win(int n);
extern errr Term_xtra_win(int n, int v);

cptr keyword_attr[] = {
	"TERM_DARK", "TERM_WHITE", "TERM_SLATE", "TERM_ORANGE",
	"TERM_RED", "TERM_GREEN", "TERM_BLUE", "TERM_UMBER",
	"TERM_L_DARK", "TERM_L_WHITE", "TERM_VIOLET", "TERM_YELLOW",
	"TERM_L_RED", "TERM_L_GREEN", "TERM_L_BLUE", "TERM_L_UMBER",
	NULL};

typedef struct {
    Tk_Window tkwin;
    Display *display;
    Tcl_Interp *interp;
    Tcl_Command widgetCmd;
    int updatePending;
    int height, width;
    Tk_Font tkfont;
    int fontWid;
    int fontHgt;
    int ascent;
    GC gcFg, gcBg, gcCursor, gcFont;
	GC gcTerm[16];
	int angband_term;
    term t;
} TkTerm;

static Tk_OptionTable optionTable = NULL;

static Tk_OptionSpec configSpecs[] = {
    {TK_OPTION_INT, "-height", (char *) NULL, (char *) NULL,
	 "24", -1, Tk_Offset(TkTerm, height), 0, 0,
	 0},
    {TK_OPTION_INT, "-width", (char *) NULL, (char *) NULL,
	 "80", -1, Tk_Offset(TkTerm, width), 0, 0,
	 0},
    {TK_OPTION_FONT, "-font", (char *) NULL, (char *) NULL,
	 "Courier 9", -1, Tk_Offset(TkTerm, tkfont), 0, 0,
	 0},
    {TK_OPTION_INT, "-angband_term", (char *) NULL, (char *) NULL,
	 "0", -1, Tk_Offset(TkTerm, angband_term), 0, 0,
	 0},
    {TK_OPTION_END}
};

static errr TkTerm_clear(void)
{
	TkTerm *termPtr = (TkTerm *) Term->data;
	Tk_Window tkwin = termPtr->tkwin;
	Drawable drawable = Tk_WindowId(tkwin);

	XFillRectangle(termPtr->display, drawable, termPtr->gcBg,
		0, 0, Tk_Width(tkwin), Tk_Height(tkwin));
		
	/* Success */
	return 0;
}

static errr TkTerm_curs(int x, int y)
{
	TkTerm *termPtr = (TkTerm *) Term->data;
	Tk_Window tkwin = termPtr->tkwin;
	Drawable drawable = Tk_WindowId(tkwin);

	XDrawRectangle(termPtr->display, drawable, termPtr->gcCursor,
		x * termPtr->fontWid, y * termPtr->fontHgt,
		termPtr->fontWid - 1, termPtr->fontHgt - 1);

	/* Success */
	return 0;
}

static errr TkTerm_wipe(int x, int y, int n)
{
	TkTerm *termPtr = (TkTerm *) Term->data;
	Tk_Window tkwin = termPtr->tkwin;
	Drawable drawable = Tk_WindowId(tkwin);

	XFillRectangle(termPtr->display, drawable, termPtr->gcBg,
		x * termPtr->fontWid, y * termPtr->fontHgt,
		n * termPtr->fontWid, termPtr->fontHgt);

	/* Success */
	return 0;
}

static errr TkTerm_text(int x, int y, int n, byte a, const char *s)
{
	TkTerm *termPtr = (TkTerm *) Term->data;
	Tk_Window tkwin = termPtr->tkwin;
	Drawable drawable = Tk_WindowId(tkwin);

	XFillRectangle(termPtr->display, drawable, termPtr->gcBg,
		x * termPtr->fontWid, y * termPtr->fontHgt,
		n * termPtr->fontWid, termPtr->fontHgt);

if (a > 15) a = 0;
	Tk_DrawChars(termPtr->display, drawable, termPtr->gcTerm[a],
		termPtr->tkfont, s, n, x * termPtr->fontWid,
		y * termPtr->fontHgt + termPtr->ascent);

	/* Success */
	return 0;
}

#ifdef USE_TRANSPARENCY
static errr TkTerm_pict(int x, int y, int n, const byte *ap, const char *cp, const byte *tap, const char *tcp)
#else
static errr TkTerm_pict(int x, int y, int n, const byte *ap, const char *cp)
#endif
{
	/* Just erase this grid */
	return (TkTerm_wipe(x, y, n));

	/* Success */
	return 0;
}

static errr TkTerm_xtra(int n, int v)
{
	/* Handle a subset of the legal requests */
	switch (n)
	{
		/* Clear the screen */
		case TERM_XTRA_CLEAR:
		{
			return (TkTerm_clear());
		}
	}

	/* Oops */
	return Term_xtra_win(n, v);
}


static void TkTermDeletedProc(clientData)
{
	TkTerm *termPtr = (TkTerm *) clientData;
	Tk_Window tkwin = termPtr->tkwin;

    if (tkwin != NULL)
    {
		Tk_DestroyWindow(tkwin);
    }
}

static void TkTermDisplay(clientData)
{
	TkTerm *termPtr = (TkTerm *) clientData;
	Tk_Window tkwin = termPtr->tkwin;
	term *t = Term;

    termPtr->updatePending = 0;
	if (!Tk_IsMapped(tkwin))
	{
		return;
	}

	/* Activate the term */
	Term_activate(&termPtr->t);

	/* Redraw the contents */
	Term_redraw();

	/* Restore the term */
	Term_activate(t);

}

static int TkTermConfigure(Tcl_Interp *interp, TkTerm *termPtr)
{
	Tk_Window tkwin = termPtr->tkwin;
	XGCValues gcValues;
	unsigned long mask;

	if(termPtr->t.data == NULL)
	{
		term *t;

		/* Initialize the term */
		t = &termPtr->t;
		
		term_init(t, termPtr->width, termPtr->height, 1024);
	
		/* Use a "software" cursor */
		t->soft_cursor = TRUE;
	
		/* Use "Term_pict" for "graphic" data */
		t->higher_pict = TRUE;
	
		/* Erase with "white space" */
		t->attr_blank = TERM_WHITE;
		t->char_blank = ' ';
	
		/* Prepare the template hooks */
		t->user_hook = Term_user_win;
		t->xtra_hook = TkTerm_xtra;
		t->curs_hook = TkTerm_curs;
		t->wipe_hook = TkTerm_wipe;
		t->text_hook = TkTerm_text;
		t->pict_hook = TkTerm_pict;
	
		/* Remember where we came from */
		t->data = (vptr) termPtr;
	}

	if (termPtr->gcFg == None)
	{
	    gcValues.foreground = WhitePixelOfScreen(Tk_Screen(tkwin));
	    gcValues.background = BlackPixelOfScreen(Tk_Screen(tkwin));
	    gcValues.graphics_exposures = False;
	    termPtr->gcFg = Tk_GetGC(tkwin,
		    GCForeground | GCBackground,
		    &gcValues);
	}

	if (termPtr->gcBg == None)
	{
	    gcValues.background = BlackPixelOfScreen(Tk_Screen(tkwin));
	    gcValues.graphics_exposures = False;
	    termPtr->gcBg = Tk_GetGC(tkwin,
			GCBackground,
		    &gcValues);
	}

	if (termPtr->gcFont == None)
	{
	    gcValues.foreground = WhitePixelOfScreen(Tk_Screen(tkwin));
	    gcValues.font = Tk_FontId(termPtr->tkfont);
	    gcValues.graphics_exposures = False;
	    mask = GCForeground | GCFont | GCGraphicsExposures;
	    termPtr->gcFont = Tk_GetGC(tkwin, mask, &gcValues);
	}

	if (!termPtr->fontWid)
	{
		Tk_FontMetrics fm;
		Tk_GetFontMetrics(termPtr->tkfont, &fm);
		termPtr->fontWid = Tk_TextWidth(termPtr->tkfont, "W", 1);
		termPtr->fontHgt = fm.linespace;
		termPtr->ascent = fm.ascent;
	}

	if (termPtr->gcTerm[0] == None)
	{
		int i;
	
		for (i = 0; i < 16; i++)
		{
			byte rv, gv, bv;
	
			rv = angband_color_table[i][1];
			gv = angband_color_table[i][2];
			bv = angband_color_table[i][3];

		    gcValues.foreground = PALETTERGB(rv, gv, bv);
		    gcValues.font = Tk_FontId(termPtr->tkfont);
		    gcValues.graphics_exposures = False;
		    mask = GCForeground | GCFont | GCGraphicsExposures;
		    termPtr->gcTerm[i] = Tk_GetGC(tkwin, mask, &gcValues);
		}

termPtr->gcCursor = termPtr->gcTerm[TERM_YELLOW];
	}

	if ((termPtr->angband_term >= 1) && (termPtr->angband_term <= 8))
	{
		if (Term == angband_term[termPtr->angband_term - 1])
		{
			Term_activate(&termPtr->t);
		}
		angband_term[termPtr->angband_term - 1] = &termPtr->t;
	}

	Tk_GeometryRequest(tkwin, termPtr->width * termPtr->fontWid,
		termPtr->height * termPtr->fontHgt);

	if (!termPtr->updatePending)
	{
		Tcl_DoWhenIdle(TkTermDisplay, (ClientData) termPtr);
		termPtr->updatePending = 1;
	}

	return TCL_OK;
}

static void TkTermDestroy(char *memPtr)
{
	TkTerm *termPtr = (TkTerm *) memPtr;
    
    Tcl_FreeDebug((char *) termPtr);
}

static void TkTermEventProc(ClientData clientData, XEvent *eventPtr)
{
	TkTerm *termPtr = (TkTerm *) clientData;

	if (eventPtr->type == Expose)
	{
		if (!termPtr->updatePending)
		{
			Tcl_DoWhenIdle(TkTermDisplay, (ClientData) termPtr);
			termPtr->updatePending = 1;
		}
	}
	else if (eventPtr->type == ConfigureNotify)
	{
		if (!termPtr->updatePending)
		{
			Tcl_DoWhenIdle(TkTermDisplay, (ClientData) termPtr);
			termPtr->updatePending = 1;
		}
	}
	else if (eventPtr->type == DestroyNotify)
	{
		if (termPtr->tkwin != NULL)
		{
			Tk_FreeConfigOptions((char *) termPtr,
				optionTable, termPtr->tkwin);
			if (termPtr->gcFg != None)
				Tk_FreeGC(termPtr->display, termPtr->gcFg);
			if (termPtr->gcBg != None)
				Tk_FreeGC(termPtr->display, termPtr->gcBg);
			if (termPtr->gcFont != None)
				Tk_FreeGC(termPtr->display, termPtr->gcFont);
			termPtr->tkwin = NULL;
			Tcl_DeleteCommandFromToken(termPtr->interp, termPtr->widgetCmd);
		}
		if (termPtr->updatePending)
		{
			Tcl_CancelIdleCall(TkTermDisplay, (ClientData) termPtr);
		}
		Tcl_EventuallyFree((ClientData) termPtr, TkTermDestroy);
	}
}

/* $path $option args */
static int TkTermWidgetCmd(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	TkTerm *termPtr = (TkTerm *) clientData;
	static char *cmdOption[] = { "cget", "configure", "fresh", "putstr",
		"clear", (char *) NULL };
	enum {IDX_CGET, IDX_CONFIGURE, IDX_FRESH, IDX_PUTSTR, IDX_CLEAR} option;
	Tcl_Obj *resultObjPtr;
	int result = TCL_OK;

	if (objc < 2)
	{
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg arg...?");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objv[1], cmdOption, "command", 0,
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	Tcl_Preserve((ClientData) termPtr);

	switch (option)
	{
		case IDX_CGET:
		{
			if (objc != 3)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "option");
				goto error;
			}
			resultObjPtr =
				Tk_GetOptionValue(interp, (char *) termPtr,
				optionTable, objv[2], termPtr->tkwin);
			if (resultObjPtr == NULL)
			{
				result = TCL_ERROR;
			}
			else
			{
				Tcl_SetObjResult(interp, resultObjPtr);
			}
			break;
		}

		case IDX_CONFIGURE:
		{
			resultObjPtr = NULL;
			if (objc == 2)
			{
				resultObjPtr =
					Tk_GetOptionInfo(interp, (char *) termPtr,
					optionTable, (Tcl_Obj *) NULL,
					termPtr->tkwin);
				if (resultObjPtr == NULL)
				{
					result = TCL_ERROR;
				}
			}
			else if (objc == 3)
			{
				resultObjPtr =
					Tk_GetOptionInfo(interp, (char *) termPtr,
					optionTable, objv[2], termPtr->tkwin);
				if (resultObjPtr == NULL)
				{
					result = TCL_ERROR;
				}
			}
			else
			{
				result =
					Tk_SetOptions(interp, (char *) termPtr,
					optionTable, objc - 2, objv + 2,
					termPtr->tkwin, NULL, (int *) NULL);
				if (result == TCL_OK)
				{
					result = TkTermConfigure(interp, termPtr);
				}
			}
			if (resultObjPtr != NULL)
			{
				Tcl_SetObjResult(interp, resultObjPtr);
			}
			break;
		}

		case IDX_FRESH:
		{
			term *t = Term;
			Term_activate(&termPtr->t);
			Term_fresh();
			Term_activate(t);
			break;
		}

		/* $term putstr $col $row $string ?$attr? */
		case IDX_PUTSTR:
		{
			char *s;
			int col, row, length;
			int a = TERM_WHITE;
			term *t = Term;

			if (Tcl_GetIntFromObj(interp, objv[2], &col) != TCL_OK)
				return TCL_ERROR;
			if (Tcl_GetIntFromObj(interp, objv[3], &row) != TCL_OK)
				return TCL_ERROR;
			s = Tcl_GetStringFromObj(objv[4], &length);
			if (objc == 6)
			{
				if (Tcl_GetIndexFromObj(interp, objv[5],
					(char **) keyword_attr, "attr", 0, &a) != TCL_OK)
				{
					return TCL_ERROR;
				}
			}
			if (col < 0 || col >= termPtr->width) break;
			if (col + length > termPtr->width) break;
			if (row < 0 || row >= termPtr->height) break;
			Term_activate(&termPtr->t);
			Term_putstr(col, row, length, a, s);
			Term_activate(t);
			break;
		}

		case IDX_CLEAR:
		{
			term *t = Term;
			Term_activate(&termPtr->t);
			Term_clear();
			Term_activate(t);
			break;
		}
	}

	Tcl_Release((ClientData) termPtr);
	return result;

  error:
	Tcl_Release((ClientData) termPtr);
	return TCL_ERROR;
}

/* tkterm $path ?$option $value...? */
static int TkTermCmd(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	Tk_Window tkwin;
	TkTerm *termPtr;

	if (optionTable == NULL)
	{
		optionTable = Tk_CreateOptionTable(interp, configSpecs);
	}

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objV, "pathName ?options?");
		return TCL_ERROR;
    }

	tkwin = Tk_CreateWindowFromPath(interp, Tk_MainWindow(interp),
		Tcl_GetStringFromObj(objV[1], NULL), (char *) NULL);
	if (tkwin == NULL)
	{
		return TCL_ERROR;
	}
	Tk_SetClass(tkwin, "TkTerm");

    termPtr = (TkTerm *) Tcl_AllocDebug(sizeof(TkTerm));
    termPtr->tkwin = tkwin;
    termPtr->display = Tk_Display(tkwin);
    termPtr->interp = interp;
    termPtr->widgetCmd = Tcl_CreateObjCommand(interp,
	    Tk_PathName(termPtr->tkwin), TkTermWidgetCmd,
	    (ClientData) termPtr, TkTermDeletedProc);
	termPtr->updatePending = 0;
	termPtr->gcFg = None;
	termPtr->gcBg = None;
	termPtr->gcCursor = None;
	termPtr->gcFont = None;
	termPtr->fontWid = 0;
	termPtr->fontHgt = 0;
	termPtr->ascent = 0;
	termPtr->gcTerm[0] = None;
	termPtr->angband_term = 0;
	termPtr->t.data = NULL;

	if (Tk_InitOptions(interp, (char *) termPtr, optionTable, tkwin)
		!= TCL_OK)
	{
		Tk_DestroyWindow(termPtr->tkwin);
		Tcl_FreeDebug((char *) termPtr);
		return TCL_ERROR;
	}
 
	Tk_CreateEventHandler(termPtr->tkwin,
		ExposureMask | StructureNotifyMask,
		TkTermEventProc, (ClientData) termPtr);

	if (Tk_SetOptions(interp, (char *) termPtr, optionTable, objC - 2,
		objV + 2, tkwin, NULL, (int *) NULL) != TCL_OK)
	{
		goto error;
	}
	if (TkTermConfigure(interp, termPtr) != TCL_OK)
	{
		goto error;
	}

	Tcl_SetObjResult(interp, Tcl_NewStringObj(Tk_PathName(termPtr->tkwin),
		-1));
	return TCL_OK;
	
error:
	Tk_DestroyWindow(termPtr->tkwin);
	return TCL_ERROR;
}

static CommandInit commandInit[] = {
	{0, "tkterm", 2, 0, "path ?option value ...?", TkTermCmd, (ClientData) 0},
	{0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

int TkTerm_Init(Tcl_Interp *interp)
{
	(void) CommandInfo_Init(g_interp, commandInit, NULL);
	return TCL_OK;
}
