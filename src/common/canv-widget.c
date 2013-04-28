/* canv-widget.c */

/* Purpose: canvas item for displaying one icon */

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
#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11
#include <sys/param.h> /* Warning about NBBY being redefined */
#include <tkInt.h>
#endif /* PLATFORM_X11 */

#include <tkCanvas.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "util-dll.h"
#include "icon.h"

static BitmapType CanvWidgetBitmap = { NULL };
static int g_256color;

typedef struct WidgetItem WidgetItem;
struct WidgetItem  {
	Tk_Item header;
	Tcl_Interp *interp;
	Tk_Canvas canvas;
	double x, y;
	Tk_Anchor anchor;
	t_assign assign;
	t_assign assignbg;
	int gwidth, gheight;
	XColor *borderColor;
	int borderWidth;
	int borderDist;
	DoubleLink link; /* Linked list of animated WidgetItems */
};

/*
 * Linked list of all Widget-type Canvas items in all Canvases which
 * display animated icons.
 */
static DoubleLinker WidgetItemList;

/*
 * Keep a list of all Tk_Canvas displaying WidgetItem so I can clear the
 * t_assign when "angband init_icons" is called.
 */
typedef struct WidgetCanvas {
	TkCanvas *canvas;
	int count; /* Number of WidgetItem */
} WidgetCanvas;
static WidgetCanvas *WidgetCanvasList = NULL;
static int WidgetCanvasCount = 0;

static int Assign_ParseProc _ANSI_ARGS_((
				ClientData clientData, Tcl_Interp *interp,
				Tk_Window tkwin, CONST char *value, char *widgRec,
				int offset));
static char *Assign_PrintProc _ANSI_ARGS_((
				ClientData clientData, Tk_Window tkwin,
				char *widgRec, int offset,
				Tcl_FreeProc **freeProcPtr));

#if TK_MINOR_VERSION >= 5

/* Not exported from tk.dll in 8.5+ */
int
TkStateParseProc(
    ClientData clientData,	/* some flags.*/
    Tcl_Interp *interp,		/* Used for reporting errors. */
    Tk_Window tkwin,		/* Window containing canvas widget. */
    const char *value,		/* Value of option. */
    char *widgRec,		/* Pointer to record for item. */
    int offset)			/* Offset into item. */
{
    int c;
    int flags = PTR2INT(clientData);
    size_t length;

    register Tk_State *statePtr = (Tk_State *) (widgRec + offset);

    if(value == NULL || *value == 0) {
	*statePtr = TK_STATE_NULL;
	return TCL_OK;
    }

    c = value[0];
    length = strlen(value);

    if ((c == 'n') && (strncmp(value, "normal", length) == 0)) {
	*statePtr = TK_STATE_NORMAL;
	return TCL_OK;
    }
    if ((c == 'd') && (strncmp(value, "disabled", length) == 0)) {
	*statePtr = TK_STATE_DISABLED;
	return TCL_OK;
    }
    if ((c == 'a') && (flags&1) && (strncmp(value, "active", length) == 0)) {
	*statePtr = TK_STATE_ACTIVE;
	return TCL_OK;
    }
    if ((c == 'h') && (flags&2) && (strncmp(value, "hidden", length) == 0)) {
	*statePtr = TK_STATE_HIDDEN;
	return TCL_OK;
    }

    Tcl_AppendResult(interp, "bad ", (flags&4)?"-default" : "state",
	    " value \"", value, "\": must be normal", NULL);
    if (flags&1) {
	Tcl_AppendResult(interp, ", active", NULL);
    }
    if (flags&2) {
	Tcl_AppendResult(interp, ", hidden", NULL);
    }
    if (flags&3) {
	Tcl_AppendResult(interp, ",", NULL);
    }
    Tcl_AppendResult(interp, " or disabled", NULL);
    *statePtr = TK_STATE_NORMAL;
    return TCL_ERROR;
}

/* Not exported from tk.dll in 8.5+ */
char *
TkStatePrintProc(
    ClientData clientData,	/* Ignored. */
    Tk_Window tkwin,		/* Window containing canvas widget. */
    char *widgRec,		/* Pointer to record for item. */
    int offset,			/* Offset into item. */
    Tcl_FreeProc **freeProcPtr)	/* Pointer to variable to fill in with
				 * information about how to reclaim storage
				 * for return string. */
{
    register Tk_State *statePtr = (Tk_State *) (widgRec + offset);

    switch (*statePtr) {
    case TK_STATE_NORMAL:
	return "normal";
    case TK_STATE_DISABLED:
	return "disabled";
    case TK_STATE_HIDDEN:
	return "hidden";
    case TK_STATE_ACTIVE:
	return "active";
    default:
	return "";
    }
}

#endif /* Tk 8.5 */

#if TK_MINOR_VERSION >= 3

/*
 * XXX Hack -- Work around "initializer element is not constant" error
 * It happens because these functions are DLLIMPORT
 */
#if defined(PLATFORM_WIN) && defined(__GNUC__) && defined(__declspec)
extern int TkStateParseProc _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, CONST char *value,
	char *widgRec, int offset));
extern char *TkStatePrintProc _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr));
#endif

#ifdef __LCC__

static int StateParseProc(ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, CONST char *value,
	char *widgRec, int offset)
{
	return TkStateParseProc(clientData, interp, tkwin, value,
		widgRec, offset);
}
static char *StatePrintProc(ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset,
	Tcl_FreeProc **freeProcPtr)
{
	return TkStatePrintProc(clientData, tkwin, widgRec, offset,
		freeProcPtr);
}

static Tk_CustomOption stateOption = {
    (Tk_OptionParseProc *) StateParseProc,
    StatePrintProc,
    (ClientData) 2
};

#else

static Tk_CustomOption stateOption = {
    (Tk_OptionParseProc *) TkStateParseProc,
    TkStatePrintProc,
    (ClientData) 2
};

#endif

#endif /* 8.3-8.4 */

/*
 * XXX Hack -- Work around "initializer element is not constant" error
 * It happens because these functions are DLLIMPORT
 */
#if defined(__WIN32__/*PLATFORM_WIN*/) && defined(__GNUC__) && defined(__declspec)
extern int Tk_CanvasTagsParseProc _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, CONST char *value, 
	char *widgRec, int offset));
extern char *Tk_CanvasTagsPrintProc _ANSI_ARGS_((ClientData clientData,
	Tk_Window tkwin, char *widgRec, int offset, 
	Tcl_FreeProc ** freeProcPtr));
#endif

#ifdef __LCC__

static int CanvasTagsParseProc(ClientData clientData,
	Tcl_Interp *interp, Tk_Window tkwin, char *value, 
	char *widgRec, int offset)
{
	return Tk_CanvasTagsParseProc(clientData, interp, tkwin, value,
		widgRec, offset);
}

static char *CanvasTagsPrintProc(ClientData clientData,
	Tk_Window tkwin, char * widgRec, int offset, 
	Tcl_FreeProc ** freeProcPtr)
{
	return Tk_CanvasTagsPrintProc(clientData, tkwin, widgRec, offset,
		freeProcPtr);
}

static Tk_CustomOption tagsOption = {
	CanvasTagsParseProc,
    CanvasTagsPrintProc,
	(ClientData) NULL
};

#else /* not __LCC__ */

static Tk_CustomOption tagsOption = {
	Tk_CanvasTagsParseProc,
    Tk_CanvasTagsPrintProc,
	(ClientData) NULL
};

#endif /* not __LCC__ */

static Tk_CustomOption assignOption = {
	Assign_ParseProc,
    Assign_PrintProc,
	(ClientData) NULL
};

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
	 "nw", Tk_Offset(WidgetItem, anchor),
	 TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-bordercolor", (char *) NULL, (char *) NULL,
	 "Yellow", Tk_Offset(WidgetItem, borderColor),
	 TK_CONFIG_DONT_SET_DEFAULT | TK_CONFIG_NULL_OK},
    {TK_CONFIG_INT, "-borderdistance", (char *) NULL, (char *) NULL,
	 "1", Tk_Offset(WidgetItem, borderDist), 0},
    {TK_CONFIG_INT, "-borderwidth", (char *) NULL, (char *) NULL,
	 "2", Tk_Offset(WidgetItem, borderWidth), 0},
    {TK_CONFIG_CUSTOM, "-assign", (char *) NULL, (char *) NULL,
	 (char *) NULL, Tk_Offset(WidgetItem, assign), TK_CONFIG_USER_BIT,
	 &assignOption},
    {TK_CONFIG_CUSTOM, "-assignbg", (char *) NULL, (char *) NULL,
	 (char *) NULL, Tk_Offset(WidgetItem, assignbg), TK_CONFIG_USER_BIT,
	 &assignOption},
#if TK_MINOR_VERSION >= 3
    {TK_CONFIG_CUSTOM, "-state", (char *) NULL, (char *) NULL,
	 (char *) NULL, Tk_Offset(Tk_Item, state), TK_CONFIG_NULL_OK,
	 &stateOption},
#endif /* 8.3 */
    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
	 (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	 (char *) NULL, 0, 0}
};

static int		WidgetToArea _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *rectPtr));
static double		WidgetToPoint _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *coordPtr));
static void		ComputeWidgetBbox _ANSI_ARGS_((Tk_Canvas canvas,
			    WidgetItem *widgetPtr));
#if TK_MINOR_VERSION >= 3
static int WidgetCoords _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
	Tcl_Obj *CONST argv[]));
static int CreateWidget _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Canvas canvas, struct Tk_Item *itemPtr,
	int argc, Tcl_Obj *CONST argv[]));
static int ConfigureWidget _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
	Tcl_Obj *CONST argv[], int flags));
#else /* not 8.3 */
static int		WidgetCoords _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
			    char **argv));
static int		ConfigureWidget _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
			    char **argv, int flags));
static int		CreateWidget _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, struct Tk_Item *itemPtr,
			    int argc, char **argv));
#endif /* not 8.3 */
static void		DeleteWidget _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display));
static void		DisplayWidget _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, Display *display, Drawable dst,
			    int x, int y, int width, int height));
static void		ScaleWidget _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double originX, double originY,
			    double scaleX, double scaleY));
static void		TranslateWidget _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double deltaX, double deltaY));

Tk_ItemType WidgetType = {
    "widget",						/* name */
    sizeof(WidgetItem),				/* itemSize */
    CreateWidget,					/* createProc */
    configSpecs,					/* configSpecs */
    ConfigureWidget,				/* configureProc */
    WidgetCoords,					/* coordProc */
    DeleteWidget,					/* deleteProc */
    DisplayWidget,					/* displayProc */
#if TK_MINOR_VERSION >= 3
    TK_CONFIG_OBJS,					/* flags */
#else /* not 8.3 */
    0,								/* alwaysRedraw */
#endif /* not 8.3 */
    WidgetToPoint,					/* pointProc */
    WidgetToArea,					/* areaProc */
    (Tk_ItemPostscriptProc *) NULL,	/* postscriptProc */
    ScaleWidget,					/* scaleProc */
    TranslateWidget,				/* translateProc */
    (Tk_ItemIndexProc *) NULL,		/* indexProc */
    (Tk_ItemCursorProc *) NULL,		/* icursorProc */
    (Tk_ItemSelectionProc *) NULL,	/* selectionProc */
    (Tk_ItemInsertProc *) NULL,		/* insertProc */
    (Tk_ItemDCharsProc *) NULL,		/* dTextProc */
    (Tk_ItemType *) NULL			/* nextPtr */
};

static int
CreateWidget(
	Tcl_Interp *interp,			/* Interpreter for error reporting. */
	Tk_Canvas canvas,			/* Canvas to hold new item. */
	Tk_Item *itemPtr,			/* Record to hold new item;  header
								 * has been initialized by caller. */
	int argc,					/* Number of arguments in argv. */
#if TK_MINOR_VERSION >= 3
	Tcl_Obj *CONST argv[]		/* Arguments describing item. */
#else /* not 8.3 */
	char **argv 				/* Arguments describing item. */
#endif /* not 8.3 */
)
{
	WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
	t_assign none;
	int i;

	if (argc < 2)
	{
		FormatResult(interp,
			"wrong # args: should be \"%s create %s x y ?options?\"",
			Tk_PathName(Tk_CanvasTkwin(canvas)),
			itemPtr->typePtr->name, "");
		return TCL_ERROR;
	}

	/*
	 * Initialize item's record.
	 */

	widgetPtr->interp = interp;
	widgetPtr->canvas = canvas;
	none.assignType = ASSIGN_TYPE_ICON;
	none.icon.type = ICON_TYPE_NONE;
	none.icon.index = 0;
	none.icon.ascii = -1;
	widgetPtr->assign = none;
	widgetPtr->assignbg = none;
	widgetPtr->gwidth = g_icon_width;
	widgetPtr->gheight = g_icon_height;
	widgetPtr->anchor = TK_ANCHOR_NW;
	widgetPtr->borderColor = NULL;
	widgetPtr->borderWidth = 0;
	widgetPtr->borderDist = 0;
	DoubleLink_Init(&WidgetItemList, &widgetPtr->link, widgetPtr);

	/*
	 * Process the arguments to fill in the item record.
	 */

#if TK_MINOR_VERSION >= 3
	if ((Tk_CanvasGetCoordFromObj(interp, canvas, argv[0], &widgetPtr->x) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, argv[1], &widgetPtr->y)
		!= TCL_OK))
	{
		return TCL_ERROR;
	}
#else /* not 8.3 */
	if ((Tk_CanvasGetCoord(interp, canvas, argv[0], &widgetPtr->x) != TCL_OK)
		|| (Tk_CanvasGetCoord(interp, canvas, argv[1], &widgetPtr->y)
		!= TCL_OK))
	{
		return TCL_ERROR;
	}
#endif /* not 8.3 */

	if (ConfigureWidget(interp, canvas, itemPtr, argc-2, argv+2, 0) != TCL_OK)
	{
		DeleteWidget(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
		return TCL_ERROR;
	}

	for (i = 0; i < WidgetCanvasCount; i++)
	{
		if (WidgetCanvasList[i].canvas == (TkCanvas *) canvas)
		{
			WidgetCanvasList[i].count++;
			break;
		}
	}
	if (i == WidgetCanvasCount)
	{
		WidgetCanvas widgetCanvas;

		widgetCanvas.canvas = (TkCanvas *) canvas;
		widgetCanvas.count = 1;
		WidgetCanvasList = Array_Append(WidgetCanvasList, &WidgetCanvasCount,
			sizeof(WidgetCanvas), &widgetCanvas);
	}

    return TCL_OK;
}

static int
WidgetCoords(
    Tcl_Interp *interp,			/* Used for error reporting. */
    Tk_Canvas canvas,			/* Canvas containing item. */
    Tk_Item *itemPtr,			/* Item whose coordinates are to be
								 * read or modified. */
    int argc,					/* Number of coordinates supplied in
								 * argv. */
#if TK_MINOR_VERSION >= 3
	Tcl_Obj *CONST argv[] 		/* Array of coordinates: x1, y1,
								 * x2, y2, ... */
#else /* not 8.3 */
    char **argv 				/* Array of coordinates: x1, y1,
								 * x2, y2, ... */
#endif /* not 8.3 */
)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
    char x[TCL_DOUBLE_SPACE], y[TCL_DOUBLE_SPACE];

    if (argc == 0)
    {
		Tcl_PrintDouble(interp, widgetPtr->x, x);
		Tcl_PrintDouble(interp, widgetPtr->y, y);
		Tcl_AppendResult(interp, x, " ", y, (char *) NULL);
    }
    else if (argc == 2)
    {
#if TK_MINOR_VERSION >= 3
		if ((Tk_CanvasGetCoordFromObj(interp, canvas, argv[0], &widgetPtr->x)
			!= TCL_OK)
			|| (Tk_CanvasGetCoordFromObj(interp, canvas, argv[1],
			&widgetPtr->y) != TCL_OK))
		{
		    return TCL_ERROR;
		}
#else /* not 8.3 */
		if ((Tk_CanvasGetCoord(interp, canvas, argv[0], &widgetPtr->x) != TCL_OK)
			|| (Tk_CanvasGetCoord(interp, canvas, argv[1],
			    &widgetPtr->y) != TCL_OK))
		{
		    return TCL_ERROR;
		}
#endif /* not 8.3 */
		ComputeWidgetBbox(canvas, widgetPtr);
    }
    else
    {
		sprintf(interp->result,
			"wrong # coordinates: expected 0 or 2, got %d", argc);
		return TCL_ERROR;
    }
    return TCL_OK;
}

static int
ConfigureWidget(
    Tcl_Interp *interp,		/* Used for error reporting. */
    Tk_Canvas canvas,		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr,		/* Widget item to reconfigure. */
    int argc,				/* Number of elements in argv.  */
#if TK_MINOR_VERSION >= 3
    Tcl_Obj *CONST argv[],	/* Arguments describing things to configure. */
#else /* not 8.3 */
    char **argv,			/* Arguments describing things to configure. */
#endif /* not 8.3 */
    int flags 				/* Flags to pass to Tk_ConfigureWidget. */
)
{
	WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
	Tk_Window tkwin;
	t_icon_type *iconTypePtr;
	IconSpec iconSpec;

	tkwin = Tk_CanvasTkwin(canvas);
#if TK_MINOR_VERSION >= 3
	if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc,
		(CONST char **) argv, (char *) widgetPtr, flags | TK_CONFIG_OBJS) != TCL_OK)
#else /* not 8.3 */
	if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc,
		argv, (char *) widgetPtr, flags) != TCL_OK)
#endif /* not 8.3 */
	{
		return TCL_ERROR;
	}

	FinalIcon(&iconSpec, &widgetPtr->assign, 0, NULL, NULL);
	iconTypePtr = &g_icon_type[iconSpec.type];
	widgetPtr->gwidth = iconTypePtr->width;
	widgetPtr->gheight = iconTypePtr->height;

	ComputeWidgetBbox(canvas, widgetPtr);

	/* Keep a linked list of WidgetItem's displaying sprites */
	if (widgetPtr->link.isLinked)
	{
		if (!is_sprite(&widgetPtr->assign) && !is_sprite(&widgetPtr->assignbg))
		{
			DoubleLink_Unlink(&widgetPtr->link);
		}
	}
	else
	{
		if (is_sprite(&widgetPtr->assign) || is_sprite(&widgetPtr->assignbg))
		{
			DoubleLink_Link(&widgetPtr->link);
		}
	}

    return TCL_OK;
}

static void
DeleteWidget(
    Tk_Canvas canvas,			/* Info about overall canvas widget. */
    Tk_Item *itemPtr,			/* Item that is being deleted. */
    Display *display 			/* Display containing window for
					 * canvas. */
)
{
	WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
	int i;

	DoubleLink_Unlink(&widgetPtr->link);

	for (i = 0; i < WidgetCanvasCount; i++)
	{
		if (WidgetCanvasList[i].canvas == (TkCanvas *) canvas)
		{
			WidgetCanvasList[i].count--;
			if (WidgetCanvasList[i].count == 0)
				WidgetCanvasList = Array_Delete(WidgetCanvasList,
					&WidgetCanvasCount, sizeof(WidgetCanvas), i);
			return;
		}
	}
	panic("can't find canvas %s %d", __FILE__, __LINE__);
}

static void
ComputeWidgetBbox(
    Tk_Canvas canvas,				/* Canvas that contains item. */
    WidgetItem *widgetPtr 			/* Item whose bbox is to be
									 * recomputed. */
)
{
	int x, y;
	int borderSize = 0;
#if TK_MINOR_VERSION >= 3
    Tk_State state = widgetPtr->header.state;

	if(state == TK_STATE_NULL)
	{
		state = ((TkCanvas *) canvas)->canvas_state;
	}
#endif /* 8.3 */

    x = (int) (widgetPtr->x + ((widgetPtr->x >= 0) ? 0.5 : - 0.5));
    y = (int) (widgetPtr->y + ((widgetPtr->y >= 0) ? 0.5 : - 0.5));

#if TK_MINOR_VERSION >= 3
	if (state == TK_STATE_HIDDEN)
	{
		widgetPtr->header.x1 = widgetPtr->header.x2 = x;
		widgetPtr->header.y1 = widgetPtr->header.y2 = y;
		return;
	}
#endif /* 8.3 */

	switch (widgetPtr->anchor)
	{
		case TK_ANCHOR_NW:
		case TK_ANCHOR_N:
		case TK_ANCHOR_NE:
			break;

		case TK_ANCHOR_W:
		case TK_ANCHOR_CENTER:
		case TK_ANCHOR_E:
			y -=  widgetPtr->gheight / 2;
			break;

		case TK_ANCHOR_SW:
		case TK_ANCHOR_S:
		case TK_ANCHOR_SE:
			y -=  widgetPtr->gheight;
			break;
	}
	switch (widgetPtr->anchor)
	{
		case TK_ANCHOR_NW:
		case TK_ANCHOR_W:
		case TK_ANCHOR_SW:
			break;

		case TK_ANCHOR_N:
		case TK_ANCHOR_CENTER:
		case TK_ANCHOR_S:
			x -=  widgetPtr->gwidth / 2;
			break;

		case TK_ANCHOR_NE:
		case TK_ANCHOR_E:
		case TK_ANCHOR_SE:
			x -= widgetPtr->gwidth;
			break;
	}

	if (widgetPtr->borderColor && widgetPtr->borderWidth)
	{
		borderSize = widgetPtr->borderWidth + widgetPtr->borderDist;
	}

    widgetPtr->header.x1 = x - borderSize;
    widgetPtr->header.y1 = y - borderSize;
    widgetPtr->header.x2 = x + widgetPtr->gwidth + borderSize;
    widgetPtr->header.y2 = y + widgetPtr->gheight + borderSize;
}

/*
 * Draw one icon into our bitmap.
 */
static void DrawIconSpec(IconSpec *iconSpecPtr)
{
	IconPtr srcPtr, dstPtr;
	int y;
	t_icon_type *iconTypePtr;

	iconTypePtr = &g_icon_type[iconSpecPtr->type];

	dstPtr = CanvWidgetBitmap.pixelPtr;

	/*
	 * If this is an ascii-type icon, then we must call a routine to
	 * give us the colorized icon data.
	 */
	if (iconSpecPtr->ascii != -1)
	{
		IconData iconData;
		srcPtr = Icon_GetAsciiData(iconSpecPtr, iconData);
		for (y = 0; y < iconTypePtr->height; y++)
		{
			memcpy(dstPtr, srcPtr, iconTypePtr->pitch);
			srcPtr += iconTypePtr->pitch;
			dstPtr += CanvWidgetBitmap.pitch;
		}
	}

	/* Not an ascii-type icon */
	else
	{
		srcPtr = iconTypePtr->icon_data + iconSpecPtr->index * iconTypePtr->length;

		/* Transparent */
		if (iconTypePtr->rle_data)
		{
			int col = 0;
			IconPtr rlebuf;
			unsigned char *bounds = iconTypePtr->rle_bounds +
				iconSpecPtr->index * 4;
			int bypp = iconTypePtr->bypp;
			IconPtr dst = dstPtr + bounds[0] * bypp +
				bounds[1] * CanvWidgetBitmap.pitch;
			int w = bounds[2], h = bounds[3];

			if (iconTypePtr->dynamic)
			{
				IconValue empty[2] = { 0, 0 };
				rlebuf = ((IconPtr *) iconTypePtr->rle_data)[iconSpecPtr->index];
				if (!rlebuf)
					rlebuf = empty;
			}
			else
			{
				rlebuf = iconTypePtr->rle_data +
					iconTypePtr->rle_offset[iconSpecPtr->index];
			}

			while (1)
			{
				unsigned int trans, opaq;

				trans = rlebuf[0];
				opaq = rlebuf[1];
				rlebuf += 2;

				col += trans;

				if (opaq)
				{
					memcpy(dst + col * bypp, rlebuf, opaq * bypp);
					rlebuf += opaq * bypp;
					col += opaq;
				}
				else if (!col)
					break;

				if (col == w)
				{
					if (!--h)
						break;
					col = 0;
					dst += CanvWidgetBitmap.pitch;
				}
			}
		}

		/* Not transparent */
		else
		{
			for (y = 0; y < iconTypePtr->height; y++)
			{
				memcpy(dstPtr, srcPtr, iconTypePtr->pitch);
				srcPtr += iconTypePtr->pitch;
				dstPtr += CanvWidgetBitmap.pitch;
			}
		}
	}
}

static void
DisplayWidget(
    Tk_Canvas canvas,			/* Canvas that contains item. */
    Tk_Item *itemPtr,			/* Item to be displayed. */
    Display *display,			/* Display on which to draw item. */
    Drawable drawable,			/* Pixmap or window in which to draw
								 * item. */
    int x, int y,				/* Describes region of canvas that */
    int width, int height 		/* must be redisplayed (not used). */
)
{
#ifdef PLATFORM_WIN

    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
    short drawableX, drawableY;
    HDC dc, dc2;
    TkWinDCState state, state2;
	IconSpec iconSpecFG, iconSpecBG;
	int borderSize = 0;

    /*
     * Translate the coordinates to those of the item, then redisplay it.
     */

    Tk_CanvasDrawableCoords(canvas,
		(double) widgetPtr->header.x1,
		(double) widgetPtr->header.y1,
		&drawableX, &drawableY);

	FinalIcon(&iconSpecFG, &widgetPtr->assign, 0, NULL, NULL);
	FinalIcon(&iconSpecBG, &widgetPtr->assignbg, 0, NULL, NULL);

	if (widgetPtr->borderColor && widgetPtr->borderWidth)
	{
		borderSize = widgetPtr->borderWidth + widgetPtr->borderDist;
	}

	if ((iconSpecFG.type != ICON_TYPE_NONE) || (iconSpecBG.type != ICON_TYPE_NONE))
	{
		/* This code is like TkPutImage() */
		dc = TkWinGetDrawableDC(display, drawable, &state);
		dc2 = TkWinGetDrawableDC(display, CanvWidgetBitmap.pixmap, &state2);

		/* Transparent */
		if (g_icon_type[iconSpecFG.type].rle_data ||
			g_icon_type[iconSpecBG.type].rle_data)
		{
			/*
			 * Ignorance alert! I want to copy the background from where
			 * the masked icon should be drawn to the CanvWidgetBitmap
			 * and draw the masked icon on top of that. So I figure I
			 * will BitBlt() from the canvas HDC to CanvWidgetBitmap HDC.
			 * But on 256-color monitors it is way too slow. So I find
			 * what color is under the canvas item and use that.
			 */
			if (!g_256color)
			{
			    BitBlt(dc2, 0, 0, widgetPtr->gwidth,
					widgetPtr->gheight, dc, drawableX + borderSize,
					drawableY + borderSize, SRCCOPY);
			}
			else
			{
				TkWinFillRect(dc2, 0, 0,
					widgetPtr->gwidth, widgetPtr->gheight,
					GetPixel(dc, drawableX + borderSize,
					drawableY + borderSize));
			}
		}

		/* Draw background icon */
		if (iconSpecBG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecBG);
		}

		/* Draw foreground icon */
		if (iconSpecFG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecFG);
		}

		BitBlt(dc, drawableX + borderSize, drawableY + borderSize,
			widgetPtr->gwidth, widgetPtr->gheight, dc2, 0, 0, SRCCOPY);

		TkWinReleaseDrawableDC(CanvWidgetBitmap.pixmap, dc2, &state2);
		TkWinReleaseDrawableDC(drawable, dc, &state);
	}

	if (borderSize)
	{
		XGCValues gcValues;
		GC gc;
		int lineWidth = widgetPtr->borderWidth;

		gcValues.foreground = widgetPtr->borderColor->pixel;
		gcValues.line_width = lineWidth;
		gc = Tk_GetGC(Tk_CanvasTkwin(canvas), GCForeground | GCLineWidth,
			&gcValues);

	    XDrawRectangle(display, drawable, gc,
			drawableX + lineWidth / 2,
			drawableY + lineWidth / 2,
			(widgetPtr->header.x2 - widgetPtr->header.x1) - lineWidth / 2 - 1,
			(widgetPtr->header.y2 - widgetPtr->header.y1) - lineWidth / 2 - 1);

		Tk_FreeGC(display, gc);
	}

#endif /* PLATFORM_WIN */

	/*
	 * This code works on Win32 too, last time I checked.
	 * EXCEPT for the special case of 256-colors handled
	 * by the above code.
	 */

#ifdef PLATFORM_X11

    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
    short drawableX, drawableY;
	IconSpec iconSpecFG, iconSpecBG;
	int borderSize = 0;
	Tk_Window tkwin = ((TkCanvas *) canvas)->tkwin;
	XGCValues gcValues;
	GC gc;

    /*
     * Translate the coordinates to those of the image, then redisplay it.
     */

    Tk_CanvasDrawableCoords(canvas,
		(double) widgetPtr->header.x1,
		(double) widgetPtr->header.y1,
		&drawableX, &drawableY);

	FinalIcon(&iconSpecFG, &widgetPtr->assign, 0, NULL, NULL);
	FinalIcon(&iconSpecBG, &widgetPtr->assignbg, 0, NULL, NULL);

	if (widgetPtr->borderColor && widgetPtr->borderWidth)
	{
		borderSize = widgetPtr->borderWidth + widgetPtr->borderDist;
	}

	if ((iconSpecFG.type != ICON_TYPE_NONE) || (iconSpecBG.type != ICON_TYPE_NONE))
	{
		gcValues.function = GXcopy;
		gcValues.graphics_exposures = False;
		gc = Tk_GetGC(tkwin, GCFunction | GCGraphicsExposures, &gcValues);

		/* Transparent */
		if (g_icon_type[iconSpecFG.type].rle_data ||
			g_icon_type[iconSpecBG.type].rle_data)
		{
			XCopyArea(display,
				drawable, /* source drawable */
				CanvWidgetBitmap.pixmap, /* dest drawable */
				gc, /* graphics context */
				drawableX + borderSize, drawableY + borderSize, /* source top-left */
				(unsigned int) widgetPtr->gwidth, /* width */
				(unsigned int) widgetPtr->gheight, /* height */
				0, 0 /* dest top-left */
			);

			Plat_SyncDisplay(display);
		}

		/* Draw background icon */
		if (iconSpecBG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecBG);
		}

		/* Draw foreground icon */
		if (iconSpecFG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecFG);
		}

		XCopyArea(display,
			CanvWidgetBitmap.pixmap, /* source drawable */
			drawable, /* dest drawable */
			gc, /* graphics context */
			0, 0, /* source top-left */
			(unsigned int) widgetPtr->gwidth, /* width */
			(unsigned int) widgetPtr->gheight, /* height */
			drawableX + borderSize, /* dest top-left */
			drawableY + borderSize
		);

		Tk_FreeGC(display, gc);
	}

	if (borderSize)
	{
		XGCValues gcValues;
		GC gc;
		int lineWidth = widgetPtr->borderWidth;

		gcValues.foreground = widgetPtr->borderColor->pixel;
		gcValues.line_width = lineWidth;
		gc = Tk_GetGC(Tk_CanvasTkwin(canvas), GCForeground | GCLineWidth,
			&gcValues);

	    XDrawRectangle(display, drawable, gc,
			drawableX + lineWidth / 2,
			drawableY + lineWidth / 2,
			(widgetPtr->header.x2 - widgetPtr->header.x1) - lineWidth / 2 - 1,
			(widgetPtr->header.y2 - widgetPtr->header.y1) - lineWidth / 2 - 1);

		Tk_FreeGC(display, gc);
	}

	/* Since multiple items may be drawn into CanvWidgetBitmap */
	Plat_SyncDisplay(display);

#endif /* PLATFORM_X11 */
}

static double
WidgetToPoint(
    Tk_Canvas canvas,		/* Canvas containing item. */
    Tk_Item *itemPtr,		/* Item to check against point. */
    double *coordPtr 		/* Pointer to x and y coordinates. */
)
{
	WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
	double x1, x2, y1, y2, xDiff, yDiff;

	x1 = widgetPtr->header.x1;
	y1 = widgetPtr->header.y1;
	x2 = widgetPtr->header.x2;
	y2 = widgetPtr->header.y2;

	/*
	 * Point is outside rectangle.
	 */

	if (coordPtr[0] < x1)
	{
		xDiff = x1 - coordPtr[0];
	}
	else if (coordPtr[0] > x2)
	{
		xDiff = coordPtr[0] - x2;
	}
	else
	{
		xDiff = 0;
	}

	if (coordPtr[1] < y1)
	{
		yDiff = y1 - coordPtr[1];
	}
	else if (coordPtr[1] > y2) 
	{
		yDiff = coordPtr[1] - y2;
	}
	else
	{
		yDiff = 0;
	}

	return hypot(xDiff, yDiff);
}

static int
WidgetToArea(
    Tk_Canvas canvas,		/* Canvas containing item. */
    Tk_Item *itemPtr,		/* Item to check against rectangle. */
    double *rectPtr 		/* Pointer to array of four coordinates
				 * (x1, y1, x2, y2) describing rectangular
				 * area.  */
)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;

    if ((rectPtr[2] <= widgetPtr->header.x1)
	    || (rectPtr[0] >= widgetPtr->header.x2)
	    || (rectPtr[3] <= widgetPtr->header.y1)
	    || (rectPtr[1] >= widgetPtr->header.y2))
	{
		return -1;
    }
    if ((rectPtr[0] <= widgetPtr->header.x1)
	    && (rectPtr[1] <= widgetPtr->header.y1)
	    && (rectPtr[2] >= widgetPtr->header.x2)
	    && (rectPtr[3] >= widgetPtr->header.y2))
	{
		return 1;
    }
    return 0;
}

static void
ScaleWidget(
    Tk_Canvas canvas,			/* Canvas containing rectangle. */
    Tk_Item *itemPtr,			/* Rectangle to be scaled. */
    double originX,
    double originY,			/* Origin about which to scale rect. */
    double scaleX,			/* Amount to scale in X direction. */
    double scaleY 			/* Amount to scale in Y direction. */
)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;

    widgetPtr->x = originX + scaleX*(widgetPtr->x - originX);
    widgetPtr->y = originY + scaleY*(widgetPtr->y - originY);
    ComputeWidgetBbox(canvas, widgetPtr);
}

static void
TranslateWidget(
    Tk_Canvas canvas,			/* Canvas containing item. */
    Tk_Item *itemPtr,			/* Item that is being moved. */
    double deltaX,				/* Amount by which item is to be */
    double deltaY				/* moved. */
)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;

    widgetPtr->x += deltaX;
    widgetPtr->y += deltaY;
    ComputeWidgetBbox(canvas, widgetPtr);
}

static int
Assign_ParseProc(ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, CONST char *value, char *widgRec, int offset)
{
/*	WidgetItem *itemPtr = (WidgetItem *) widgRec; */

	if (assign_parse(interp, (t_assign *) (widgRec + offset), (char *) value) != TCL_OK)
	{
		return TCL_ERROR;
	}

    return TCL_OK;
}

static char *
Assign_PrintProc(ClientData clientData, Tk_Window tkwin, char *widgRec,
	int offset, Tcl_FreeProc **freeProcPtr)
{
/*	WidgetItem *itemPtr = (WidgetItem *) widgRec; */
	char *buf = Tcl_Alloc(128);

	*freeProcPtr = (Tcl_FreeProc *) TCL_DYNAMIC;
	return assign_print(buf, (t_assign *) (widgRec + offset));
}

void CanvasWidget_Idle(void)
{
	DoubleLink *link;
	WidgetItem *widgetPtr;

	for (link = WidgetItemList.head; link; link = link->next)
	{
		widgetPtr = DoubleLink_Data(link, WidgetItem);
		Tk_CanvasEventuallyRedraw(widgetPtr->canvas, widgetPtr->header.x1,
			widgetPtr->header.y1, widgetPtr->header.x2, widgetPtr->header.y2);
	}
}

void CanvasWidget_Setup(void)
{
	int i;
	t_assign none;

	CanvWidgetBitmap.height = 100;
	CanvWidgetBitmap.width = 100;
	CanvWidgetBitmap.depth = g_icon_depth;
	Bitmap_New(g_interp, &CanvWidgetBitmap);

	/*
	 * If the game's icon configuration changed, we must go through all the
	 * Canvas widgets displaying WidgetItem items and set the assignments to
	 * something safe since all the old icons have now been deleted.
	 */
	none.assignType = ASSIGN_TYPE_ICON;
	none.icon.type = ICON_TYPE_NONE;
	none.icon.index = 0;
	none.icon.ascii = -1;

	for (i = 0; i < WidgetCanvasCount; i++)
	{
		TkCanvas *canvas = WidgetCanvasList[i].canvas;
		Tk_Item *item;

		for (item = canvas->firstItemPtr; item != NULL; item = item->nextPtr)
		{
			if (item->typePtr == &WidgetType)
			{
				WidgetItem *widgetPtr = (WidgetItem *) item;

				widgetPtr->assign = none;
				widgetPtr->assignbg = none;
				widgetPtr->gwidth = g_icon_width;
				widgetPtr->gheight = g_icon_height;
				Tk_CanvasEventuallyRedraw(widgetPtr->canvas,
					widgetPtr->header.x1, widgetPtr->header.y1,
					widgetPtr->header.x2, widgetPtr->header.y2);
				ComputeWidgetBbox(widgetPtr->canvas, (WidgetItem *) item);
				Tk_CanvasEventuallyRedraw(widgetPtr->canvas,
					widgetPtr->header.x1, widgetPtr->header.y1,
					widgetPtr->header.x2, widgetPtr->header.y2);
			}
		}
	}
}

void CanvasWidget_Unload(void)
{
	if (CanvWidgetBitmap.pixelPtr)
	{
		Bitmap_Delete(&CanvWidgetBitmap);
		CanvWidgetBitmap.pixelPtr = NULL;
	}
}

int CanvasWidget_Init(void)
{
	DoubleLink_Init(&WidgetItemList, NULL, NULL);

#ifdef PLATFORM_WIN
	{
		HDC monDC = GetDC(NULL);
		g_256color = (GetDeviceCaps(monDC, BITSPIXEL) == 8);
		ReleaseDC(NULL, monDC);
	}
#endif /* PLATFORM_WIN */

	Tk_CreateItemType(&WidgetType);

	return TCL_OK;
}

void CanvasWidget_Exit(void)
{
	CanvasWidget_Unload();
#if 0 /* Should be freed when all canvas widgets deleted */
	if (WidgetCanvasList)
		Tcl_FreeDebug(WidgetCanvasList);
#endif
}

