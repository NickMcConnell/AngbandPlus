/* File: widget-dll.h */

/* Purpose: Widget definitions */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_WIDGETDLL_H_
#define _INCLUDE_WIDGETDLL_H_

#if !defined(PLATFORM_MAC) && !defined(PLATFORM_WIN) && !defined(PLATFORM_X11)
#error "you must define one of PLATFORM_MAC, PLATFORM_WIN or PLATFORM_X11"
#endif /* */

#include "storage.h"

typedef struct Widget Widget;
typedef void Widget_CenterProc(Widget *widgetPtr, int y, int x);
typedef void Widget_ChangedProc(Widget *widgetPtr);
typedef int Widget_ConfigureProc(Widget *widgetPtr, int error, int mask);
typedef int Widget_CreateProc(Tcl_Interp *interp, Widget **widgetPtr);
typedef void Widget_DestroyProc(Widget *widgetPtr);
typedef void Widget_DrawAllProc(Widget *widgetPtr);
typedef void Widget_DrawInvalidProc(Widget *widgetPtr);
typedef int Widget_HitTestProc(Widget *widgetPtr, int x, int y, int col, int row, int *xc, int *yc);
typedef void Widget_InvalidateProc(Widget *widgetPtr, int row, int col);
typedef void Widget_InvalidateAreaProc(Widget *widgetPtr, int top, int left, int bottom, int right);
typedef void Widget_WipeProc(Widget *widgetPtr);

struct Widget
{
    Tk_Window tkwin;
    Display *display;
    Tcl_Interp *interp;
    Tcl_Command widgetCmd;
	GC copyGC;					/* XCopyArea() context */
	BitmapType bitmap;			/* Offscreen bitmap */
    int width;					/* Window width in pixels */
    int height;					/* Window height in pixels */

#define WIDGET_STYLE_ICON 0
#define WIDGET_STYLE_MAP 1
#define WIDGET_STYLE_ISO 2
/*#define WIDGET_STYLE_TEXT 3*/
	int style;

#define WIDGET_REDRAW 0x0001
#define WIDGET_DRAW_INVALID 0x0002
#define WIDGET_DELETED 0x0008
#define WIDGET_EXPOSE 0x0010
#define WIDGET_NO_UPDATE 0x0020
#define WIDGET_WIPE 0x0040
	int flags;                  /* Misc flags */

    int oldWidth, oldHeight;	/* To notice changes */
    int oldStyle;				/* To notice changes */
    int oldTileCnt;				/* To notice changes */
    Tk_Cursor cursor;           /* Cursor? */
	int setGrid;                /* Use gridded geometry */
    char *takeFocus;			/* Used? */

	int y, x;					/* Cave location (center of widget) */
	Widget_CenterProc *centerProc; /* Per-widget centering routine */
	Widget_ChangedProc *changedProc; /* Per-widget config routine */
	Widget_ConfigureProc *configureProc; /* Per-widget config routine */
	Widget_DestroyProc *destroyProc; /* Per-widget destroy routine */
	Widget_DrawAllProc *drawAllProc;  /* Per-widget drawing routine */
	Widget_DrawInvalidProc *drawInvalidProc;  /* Per-widget drawing routine */
	Widget_InvalidateProc *invalidateProc;
	Widget_InvalidateAreaProc *invalidateAreaProc;
	Widget_WipeProc *wipeProc;  /* Per-widget fresh routine */
	Widget_HitTestProc *hitTestProc; /* Per-widget hit-test routine */
	DoubleLink link;			/* Linked list of Widget's */
	DoubleLink linkMapped;			/* Linked list of mapped Widget's */
	DoubleLinker linkerItem;	/* List of items */
	DoubleLinker linkerItemVis;	/* List of visible items */
	int noUpdate;				/* Drawing is disabled */

	int tc;						/* rc * cc */
	int rc, cc;					/* Rows, columns */
	int bh, bw;					/* Bitmap width & height */
	int by, bx;					/* Offset of window from bitmap */
	int dy, dx, dw, dh;			/* Dirty rect for copying */

	XColor *paintColor;			/* Fill bitmap with this color */

	/* WIDGET_STYLE_ICON & WIDGET_STYLE_ISO */

#define WIDGET_INFO_IGNORE 0x0001	/* This tile isn't visible */
#define WIDGET_INFO_DIRTY 0x0002	/* This tile needs redraw */
#define WIDGET_INFO_ANIM 0x0004		/* This tile is a sprite */
	short *info;					/* Flags for each tile */

	int *invalid, invalidCnt;	/* List of invalid grids */
	int *anim, animCnt;			/* List of animated grids */

	/* WIDGET_STYLE_ICON & WIDGET_STYLE_MAP */
	int y_min, y_max;           /* Limits of displayed info */
	int x_min, x_max;           /* Limits of displayed info */
    int gwidth;					/* Source column width */
    int gheight;				/* Source row height */
    int oldGWidth, oldGHeight;	/* To notice changes */

	/* WIDGET_STYLE_MAP */
	int dirty[4]; /* Dirty rect for map */

	/* WIDGET_STYLE_ISO */
	int *yp, *xp;				/* Pixel offsets of each tile */
	int *yo, *xo;				/* Cave coord offsets of each tile */
	int centerTile;				/* Center tile */
	int y0, x0;					/* Cave location of top-left (tile 0) */
	int cLeft, cRight, rTop, rBottom;
	int ignoreLeft, ignoreRight;
	int ignoreTop, ignoreBottom;
	int cx, cy;					/* Offset of bitmap from "canvas" */
	int ox, oy;					/* testing */
};

#if 0
#define ISO_WID 54 /* Width of iso icon */
#define ISO_HGT 49 /* Height of iso icon */
#define ISO_FH 27 /* Height of "floor" */
#define ISO_LAPY 1 /* Overlap in y */
#define ISO_FH2 (ISO_FH - ISO_LAPY) /* Visible floor height */
#define ISO_LAPX 2 /* Amount of overlap in x direction (on one side) */
#define ISO_WID2 (ISO_WID - ISO_LAPX)
#define ISO_BOTTOM 11
#endif

typedef struct t_iso_params {
	int width; /* Width of iso icon */
	int height; /* Height of iso icon */
	int lapx; /* Amount of overlap in x direction (on one side) */
	int lapy; /* Overlap in y */
	int floor; /* Height of "floor" */
	int bottom; /* Offset from bottom for "non-iso" icons */
} t_iso_params;

#if defined(__LCC__) && !defined(BUILD_icon_dll)
extern RGBInfo *_imp__g_iso;
#define g_iso (*_imp__g_iso)
#else
DLL_EXTVAR t_iso_params g_iso;
#endif


#define ISO_WID g_iso.width
#define ISO_HGT g_iso.height
#define ISO_FH g_iso.floor
#define ISO_LAPY g_iso.lapy
#define ISO_FH2 (ISO_FH - ISO_LAPY) /* Visible floor height */
#define ISO_LAPX g_iso.lapx
#define ISO_WID2 (ISO_WID - ISO_LAPX)
#define ISO_BOTTOM g_iso.bottom

/*
 * For each extension in a Widget there exists one record with
 * the following structure.  Each actual item is represented by
 * a record with the following stuff at its beginning, plus additional
 * type-specific stuff after that.
 */

typedef struct WidgetItem
{
	int visible; /* TRUE if the item should be drawn */
    struct WidgetItemType *typePtr;	/* Table of procedures that
					 * implement this type of item. */
    int x1, y1, x2, y2;			/* Bounding box for item, in pixels.
					 * Set by item-specific code and guaranteed to
					 * contain every pixel drawn in item. Item area
					 * includes x1 and y1 but not x2 and y2. */
	int minX, minY, maxX, maxY; /* Rows/Columns clobbered in widget */
	DoubleLink link; /* Linked list of items */
	DoubleLink linkVis; /* Linked list of visible items */
} WidgetItem;

typedef int	WidgetItem_CreateProc _ANSI_ARGS_((Tcl_Interp *interp,
	Widget *widgetPtr, WidgetItem *itemPtr,
	int objc, Tcl_Obj *CONST objv[]));
typedef int	WidgetItem_ConfigProc _ANSI_ARGS_((Tcl_Interp *interp,
	Widget *widgetPtr, WidgetItem *itemPtr,
	int objc, Tcl_Obj *CONST objv[]));
typedef int	WidgetItem_DisplayProc _ANSI_ARGS_((Tcl_Interp *interp,
	Widget *widgetPtr, WidgetItem *itemPtr));
typedef int	WidgetItem_ChangedProc _ANSI_ARGS_((Tcl_Interp *interp,
	Widget *widgetPtr, WidgetItem *itemPtr));
typedef void WidgetItem_DeleteProc _ANSI_ARGS_((Widget *widgetPtr,
	WidgetItem *itemPtr));

/*
 * Records of the following type are used to describe a type of
 * extension (e.g. monster bar, status, etc.) that can modify the
 * appearance of a Widget.
 */

typedef struct WidgetItemType
{
	char *name;
	int itemSize;
	Tk_OptionSpec *optionSpecs;
	Tk_OptionTable optionTable;
	WidgetItem_CreateProc *createProc;
	WidgetItem_ConfigProc *configProc;
	WidgetItem_DisplayProc *displayProc;
	WidgetItem_ChangedProc *changedProc;
	WidgetItem_DeleteProc *deleteProc;
	struct WidgetItemType *nextPtr;
} WidgetItemType;

extern int WidgetItem_Create(Tcl_Interp *interp, Widget *widgetPtr,
	int objc, Tcl_Obj *CONST objv[]);
extern int WidgetItem_Cget(Tcl_Interp *interp, Widget *widgetPtr,
	int objc, Tcl_Obj *CONST objv[]);
extern int WidgetItem_Configure(Tcl_Interp *interp, Widget *widgetPtr,
	int objc, Tcl_Obj *CONST objv[]);
extern void	WidgetItem_Delete(Widget *widgetPtr,
	WidgetItem *itemPtr);

/*
 * Widget items use tint tables for transparency. Since each tint table
 * takes 256 bytes, and there may be many Widget items, we allow items
 * to share tint tables.
 */
typedef struct t_widget_color
{
	int ref_cnt; /* Number of allocations; zero means free'd */
	int color; /* Palette index for this table */
	int opacity; /* Opacity for this table */
	TintTable tint; /* The tint table */
} t_widget_color;

extern t_widget_color *WidgetColor_Alloc(int color, int opacity);
extern void WidgetColor_Deref(t_widget_color *color_ptr);

#define DEBUG_WIDGET_CONFIG 0x0001
#define DEBUG_WIDGET_DRAW 0x0002
#define DEBUG_WIDGET_ITEM 0x0004
#define DEBUG_WIDGET_ISO 0x0008
#define DEBUG_WIDGET_ALL 0xFFFF
DLL_EXTVAR int debug_widgets;

#if defined(__LCC__) && !defined(BUILD_icon_dll)
extern DoubleLinker *_imp__WidgetList;
#define WidgetList (*_imp__WidgetList)
extern DoubleLinker *_imp__WidgetListMapped;
#define WidgetListMapped (*_imp__WidgetListMapped)
#else
DLL_EXTVAR DoubleLinker WidgetList;
DLL_EXTVAR DoubleLinker WidgetListMapped;
#endif
DLL_EXTERN int Widget_Init(Tcl_Interp *interp, Widget_CreateProc *proc);
DLL_EXTERN void Widget_Exit(Tcl_Interp *interp);
DLL_EXTERN int Widget_AddOptions(Tcl_Interp *interp, Tk_OptionSpec *option);
DLL_EXTERN void Widget_Center(Widget *widgetPtr, int cy, int cx);
DLL_EXTERN void Widget_Display(ClientData clientData);
DLL_EXTERN void Widget_Invalidate(Widget *widgetPtr, int row, int col);
DLL_EXTERN void Widget_InvalidateArea(Widget *widgetPtr, int top, int left, int right, int bottom);
DLL_EXTERN void Widget_Wipe(Widget *widgetPtr);
DLL_EXTERN void Widget_DrawAll(Widget *widgetPtr);
DLL_EXTERN void Widget_DrawInvalid(Widget *widgetPtr);
DLL_EXTERN void Widget_EventuallyRedraw(Widget *widgetPtr);
DLL_EXTERN int Widget_CaveToView(Widget *widgetPtr, int y, int x, int *rowPtr, int *colPtr);

#endif /* _INCLUDE_WIDGETDLL_H_*/
