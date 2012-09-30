/* canv.c */

/* Purpose: canvas item for displaying one icon */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "tnb.h"
#include "icon.h"
#include <tkCanvas.h>

static BitmapType CanvWidgetBitmap;
#ifdef PLATFORM_WIN
static int g_256color;
#endif /* PLATFORM_WIN */

typedef struct WidgetItem WidgetItem;
struct WidgetItem  {
    Tk_Item header;
    Tcl_Interp *interp;
    Tk_Canvas canvas;
	double x, y;
	Tk_Anchor anchor;
	t_assign_icon assign;
	t_assign_icon assignbg;
	int gwidth, gheight;
	XColor *borderColor;
	int borderWidth;
	int borderDist;
};

static int Assign_ParseProc _ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp,
			    Tk_Window tkwin, cptr value, char *widgRec,
			    int offset));
static char *Assign_PrintProc _ANSI_ARGS_((
			    ClientData clientData, Tk_Window tkwin,
			    char *widgRec, int offset,
			    Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption stateOption = {
    (Tk_OptionParseProc *) TkStateParseProc,
    TkStatePrintProc,
    (ClientData) 2
};

static Tk_CustomOption tagsOption = {
	Tk_CanvasTagsParseProc,
    Tk_CanvasTagsPrintProc,
	NULL
};

static Tk_CustomOption assignOption = {
	Assign_ParseProc,
    Assign_PrintProc,
	NULL
};

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_ANCHOR, (char *) "-anchor", NULL, NULL,
	 "nw", Tk_Offset(WidgetItem, anchor),
	 TK_CONFIG_DONT_SET_DEFAULT, NULL},
    {TK_CONFIG_COLOR, (char *) "-bordercolor", NULL, NULL,
	 "Yellow", Tk_Offset(WidgetItem, borderColor),
	 TK_CONFIG_DONT_SET_DEFAULT | TK_CONFIG_NULL_OK, NULL},
    {TK_CONFIG_INT, (char *) "-borderdistance", NULL, NULL,
	 "1", Tk_Offset(WidgetItem, borderDist), 0, NULL},
    {TK_CONFIG_INT, (char *) "-borderwidth", NULL, NULL,
	 "2", Tk_Offset(WidgetItem, borderWidth), 0, NULL},
    {TK_CONFIG_CUSTOM, (char *) "-assign", NULL, NULL,
	 NULL, Tk_Offset(WidgetItem, assign), TK_CONFIG_USER_BIT,
	 &assignOption},
    {TK_CONFIG_CUSTOM, (char *) "-assignbg", NULL, NULL,
	 NULL, Tk_Offset(WidgetItem, assignbg), TK_CONFIG_USER_BIT,
	 &assignOption},
    {TK_CONFIG_CUSTOM, (char *) "-state", NULL, NULL,
	 NULL, Tk_Offset(Tk_Item, state), TK_CONFIG_NULL_OK,
	 &stateOption},
    {TK_CONFIG_CUSTOM, (char *) "-tags", NULL, NULL,
	 NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_END, NULL, NULL, NULL,
	 NULL, 0, 0, NULL}
};

static int		WidgetToArea _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *rectPtr));
static double		WidgetToPoint _ANSI_ARGS_((Tk_Canvas canvas,
			    Tk_Item *itemPtr, double *coordPtr));
static void		ComputeWidgetBbox _ANSI_ARGS_((Tk_Canvas canvas,
			    WidgetItem *widgetPtr));
static int WidgetCoords _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
	Tcl_Obj *CONST argv[]));
static int CreateWidget _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Canvas canvas, struct Tk_Item *itemPtr,
	int argc, Tcl_Obj *CONST argv[]));
static int ConfigureWidget _ANSI_ARGS_((Tcl_Interp *interp,
	Tk_Canvas canvas, Tk_Item *itemPtr, int argc,
	Tcl_Obj *CONST argv[], int flags));
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
    (char *) "widget",				/* name */
    sizeof(WidgetItem),				/* itemSize */
    CreateWidget,					/* createProc */
    configSpecs,					/* configSpecs */
    ConfigureWidget,				/* configureProc */
    WidgetCoords,					/* coordProc */
    DeleteWidget,					/* deleteProc */
    DisplayWidget,					/* displayProc */
    TK_CONFIG_OBJS,					/* flags */
    WidgetToPoint,					/* pointProc */
    WidgetToArea,					/* areaProc */
    NULL,							/* postscriptProc */
    ScaleWidget,					/* scaleProc */
    TranslateWidget,				/* translateProc */
    NULL,	   						/* indexProc */
    NULL,							/* icursorProc */
    NULL,							/* selectionProc */
    NULL,	  						/* insertProc */
    NULL,	 						/* dTextProc */
    NULL,							/* nextPtr */
	NULL,							/* Reserved */
	0,								/* Reserved */
	NULL,							/* Reserved */
	NULL							/* Reserved */
};

static int
CreateWidget(
    Tcl_Interp *interp,			/* Interpreter for error reporting. */
    Tk_Canvas canvas,			/* Canvas to hold new item. */
    Tk_Item *itemPtr,			/* Record to hold new item;  header
								 * has been initialized by caller. */
    int argc,					/* Number of arguments in argv. */
	Tcl_Obj *CONST argv[]		/* Arguments describing item. */
)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
    t_assign_icon none;

    if (argc < 2)
    {
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			Tk_PathName(Tk_CanvasTkwin(canvas)), " create ",
			itemPtr->typePtr->name, " x y ?options?\"",
			NULL);
		return TCL_ERROR;
    }

    /*
     * Initialize item's record.
     */

	widgetPtr->interp = interp;
    widgetPtr->canvas = canvas;
    none.type = ICON_TYPE_NONE;
    none.index = 0;
	widgetPtr->assign = none;
	widgetPtr->assignbg = none;
	widgetPtr->gwidth = widgetPtr->gheight = g_icon_size;
	widgetPtr->anchor = TK_ANCHOR_NW;
	widgetPtr->borderColor = NULL;
	widgetPtr->borderWidth = 0;
	widgetPtr->borderDist = 0;

    /*
     * Process the arguments to fill in the item record.
     */

    if ((Tk_CanvasGetCoordFromObj(interp, canvas, argv[0], &widgetPtr->x) != TCL_OK)
	    || (Tk_CanvasGetCoordFromObj(interp, canvas, argv[1], &widgetPtr->y)
		!= TCL_OK))
	{
		return TCL_ERROR;
    }

    if (ConfigureWidget(interp, canvas, itemPtr, argc-2, argv+2, 0) != TCL_OK)
    {
		DeleteWidget(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
		return TCL_ERROR;
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
	Tcl_Obj *CONST argv[] 		/* Array of coordinates: x1, y1,
								 * x2, y2, ... */
)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
    char x[TCL_DOUBLE_SPACE], y[TCL_DOUBLE_SPACE];

    if (argc == 0)
    {
		Tcl_PrintDouble(interp, widgetPtr->x, x);
		Tcl_PrintDouble(interp, widgetPtr->y, y);
		Tcl_AppendResult(interp, x, " ", y, NULL);
    }
    else if (argc == 2)
    {
		if ((Tk_CanvasGetCoordFromObj(interp, canvas, argv[0], &widgetPtr->x)
			!= TCL_OK)
			|| (Tk_CanvasGetCoordFromObj(interp, canvas, argv[1],
			&widgetPtr->y) != TCL_OK))
		{
		    return TCL_ERROR;
		}
		ComputeWidgetBbox(canvas, widgetPtr);
    }
    else
    {
		interp->result = format("wrong # coordinates: expected 0 or 2, got %d", argc);
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
    Tcl_Obj *CONST argv[],	/* Arguments describing things to configure. */
    int flags 				/* Flags to pass to Tk_ConfigureWidget. */
)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
    Tk_Window tkwin;
    t_icon_data *iconDataPtr;
    IconSpec iconSpec;

    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc,
	    (cptr *) argv, (char *) widgetPtr, flags | TK_CONFIG_OBJS) != TCL_OK)
	{
		return TCL_ERROR;
    }

	FinalIcon(&iconSpec, &widgetPtr->assign);
	iconDataPtr = &g_icon_data[iconSpec.type];
	widgetPtr->gwidth = iconDataPtr->width;
	widgetPtr->gheight = iconDataPtr->height;
	
    ComputeWidgetBbox(canvas, widgetPtr);

    return TCL_OK;
}

/*
 * canvas: Info about overall canvas widget.
 * itemPtr: Item that is being deleted.
 * display: Display containing window for canvas.
 */

static void DeleteWidget(Tk_Canvas canvas, Tk_Item *itemPtr, Display *display)
{
	/* Hack - ignore unused parameters */
	(void) canvas;
	(void) display;
	(void) itemPtr;
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
    Tk_State state = widgetPtr->header.state;

	if(state == TK_STATE_NULL)
	{
		state = ((TkCanvas *) canvas)->canvas_state;
	}

    x = (int) (widgetPtr->x + ((widgetPtr->x >= 0) ? 0.5 : - 0.5));
    y = (int) (widgetPtr->y + ((widgetPtr->y >= 0) ? 0.5 : - 0.5));

	if (state == TK_STATE_HIDDEN)
	{
		widgetPtr->header.x1 = widgetPtr->header.x2 = x;
		widgetPtr->header.y1 = widgetPtr->header.y2 = y;
		return;
	}

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
	t_icon_data *iconDataPtr;

	iconDataPtr = &g_icon_data[iconSpecPtr->type];

	dstPtr = CanvWidgetBitmap.pixelPtr;

	srcPtr = iconDataPtr->icon_data + iconSpecPtr->index * g_icon_length;
	
	for (y = 0; y < iconDataPtr->height; y++)
	{
		memcpy(dstPtr, srcPtr, iconDataPtr->pitch);
		srcPtr += iconDataPtr->pitch;
		dstPtr += CanvWidgetBitmap.pitch;
	}
}

/*
 * canvas: Canvas that contains item
 * itemPtr: Item to be displayed
 * display: Display on which to draw item.
 * drawable: Pixmap or window in which to draw item.
 * x, y, width, height : Describes region of canvas
 *						 that must be redisplayed (not used).
 */
static void DisplayWidget(Tk_Canvas canvas, Tk_Item *itemPtr,
					 		Display *display, Drawable drawable,
							int x, int y, int width, int height)
{
	WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
	short drawableX, drawableY;	
	int borderSize = 0;
	IconSpec iconSpecFG, iconSpecBG;
	
#ifdef PLATFORM_WIN
    HDC dc, dc2;
    TkWinDCState state, state2;
#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11
	Tk_Window tkwin = ((TkCanvas *) canvas)->tkwin;
	XGCValues gcValues;
	GC gc;
#endif /* PLATFORM_X11 */

	/* Hack -ignore unused parameters */
	(void) x;
	(void) y;
	(void) width;
	(void) height;

    /*
     * Translate the coordinates to those of the image, then redisplay it.
     */

    Tk_CanvasDrawableCoords(canvas,
		(double) widgetPtr->header.x1,
		(double) widgetPtr->header.y1,
		&drawableX, &drawableY);

	FinalIcon(&iconSpecFG, &widgetPtr->assign);
	FinalIcon(&iconSpecBG, &widgetPtr->assignbg);

	if (widgetPtr->borderColor && widgetPtr->borderWidth)
	{
		borderSize = widgetPtr->borderWidth + widgetPtr->borderDist;
	}


	if ((iconSpecFG.type != ICON_TYPE_NONE) || (iconSpecBG.type != ICON_TYPE_NONE))
	{

#ifdef PLATFORM_WIN
		/* This code is like TkPutImage() */
		dc = TkWinGetDrawableDC(display, drawable, &state);
		dc2 = TkWinGetDrawableDC(display, CanvWidgetBitmap.pixmap, &state2);

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11
		gcValues.function = GXcopy;
		gcValues.graphics_exposures = False;
		gc = Tk_GetGC(tkwin, GCFunction | GCGraphicsExposures, &gcValues);
#endif /* PLATFORM_X11 */
		
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
		
		
#ifdef PLATFORM_WIN
		BitBlt(dc, drawableX + borderSize, drawableY + borderSize,
			widgetPtr->gwidth, widgetPtr->gheight, dc2, 0, 0, SRCCOPY);
	
		TkWinReleaseDrawableDC(CanvWidgetBitmap.pixmap, dc2, &state2);
		TkWinReleaseDrawableDC(drawable, dc, &state);
#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

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

#endif /* PLATFORM_X11 */

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
}

/*
 * canvas: Canvas containing item.
 * itemPtr: Item to check against point.
 * coorPtr: Pointer to x and y coordinates.
 */
static double WidgetToPoint(Tk_Canvas canvas, Tk_Item *itemPtr,
				double *coordPtr)
{
	WidgetItem *widgetPtr = (WidgetItem *) itemPtr;
	double x1, x2, y1, y2, xDiff, yDiff;
	
	/* Hack - ignore unused parameter */
	(void) canvas; 

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

/*
 * canvas: Canvas containing item.
 * itemPtr: Item to check against rectangle.
 * rectPtr: Pointer to array of four coordinates
 * (x1, y1, x2, y2) describing rectangular area.
 */
static int
WidgetToArea(Tk_Canvas canvas, Tk_Item *itemPtr, double *rectPtr)
{
    WidgetItem *widgetPtr = (WidgetItem *) itemPtr;

	/* Hack - ignore unused parameter */
	(void) canvas;

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

static int Assign_ParseProc(ClientData clientData, Tcl_Interp *interp,
	Tk_Window tkwin, cptr value, char *widgRec, int offset)
{
/*	WidgetItem *itemPtr = (WidgetItem *) widgRec; */

	/* Hack - ignore unused parameters */
	(void) tkwin;
	(void) clientData;

	if (assign_parse(interp, (t_assign_icon *) (widgRec + offset), value) != TCL_OK)
	{
		return TCL_ERROR;
	}

    return TCL_OK;
}

static char *Assign_PrintProc(ClientData clientData, Tk_Window tkwin,
				 char *widgRec, int offset, Tcl_FreeProc **freeProcPtr)
{
/*	WidgetItem *itemPtr = (WidgetItem *) widgRec; */
	char *buf;
	
	C_MAKE(buf, 128, char);
		
	/* Hack - ignore unused parameters */
	(void) tkwin;
	(void) clientData;

	*freeProcPtr = (Tcl_FreeProc *) TCL_DYNAMIC;
	return AssignToString_Icon(buf, (t_assign_icon *) (widgRec + offset));
}

int CanvasWidget_Init(Tcl_Interp *interp)
{
	CanvWidgetBitmap.height = 100;
	CanvWidgetBitmap.width = 100;
	CanvWidgetBitmap.depth = g_icon_depth;
	Bitmap_New(interp, &CanvWidgetBitmap);

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

