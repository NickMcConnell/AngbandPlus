/* File: widget2-dll.c */

/* Purpose: widget extensions */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <string.h>
#include <tk.h>
#include "util-dll.h"
#include "widget-dll.h"
#include "plat-dll.h"
#include "dbwin.h"

/* typedef unsigned char IconValue, *IconPtr; */

/*
 * Support for and transparent items (only on  8-bit bitmaps)
 */
/* #define WIDGET_TRANSPARENCY */

#define XSetRect(r,x1,y1,w,h) (*r).x=x1,(*r).y=y1,(*r).width=w,(*r).height=h

#define BAD_COLOR(c) (((c) < 0) || ((c) > 255))

static void debug_option(int mask, char **flag)
{
	int i;

	dbwin("(mask %0#6X) ", mask);
	for (i = 0; flag[i]; i++)
	{
		if (mask & (1L << i)) dbwin("%s ", flag[i]);
	}
	dbwin("\n");
}

#ifdef WIDGET_TRANSPARENCY

/*
 * Draws a 2-color bevel into a bitmap
 */
static void Bevel(XRectangle *rect, BitmapPtr bitmapPtr, IconValue topLeft,
	IconValue botRight)
{
	int y, x;
	IconPtr topPtr, botPtr, leftPtr, rightPtr;

	/* Calculate the address of the top-left corner */
	topPtr = bitmapPtr->pixelPtr + rect->x +
		rect->y * bitmapPtr->pitch;

	/* Calculate the address of the bottom-left corner */
	botPtr = bitmapPtr->pixelPtr + rect->x +
		(rect->y + rect->height - 1) * bitmapPtr->pitch;

	/* Draw top and bottom lines */
	for (x = rect->x; x < rect->x + rect->width; x++)
	{
		*topPtr++ = topLeft;
		*botPtr++ = botRight;
	}

	/* Calculate the address of the top-(left+1) corner */
	leftPtr = bitmapPtr->pixelPtr + rect->x +
		(rect->y + 1) * bitmapPtr->pitch;

	/* Calculate the address of the (top+1)-right corner */
	rightPtr = bitmapPtr->pixelPtr + (rect->x + rect->width - 1) +
		(rect->y + 1) * bitmapPtr->pitch;

	/* Draw left and right lines */
	for (y = rect->y + 1; y < rect->y + rect->height - 1; y++)
	{
		*leftPtr = topLeft;
		*rightPtr = botRight;
		leftPtr += bitmapPtr->pitch;
		rightPtr += bitmapPtr->pitch;
	}
}

#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

static int XColor2PaletteIndex(XColor *xColorPtr)
{
	if (xColorPtr == NULL)
		return 0;
	return Palette_RGB2Index(
		((double) xColorPtr->red / USHRT_MAX) * 255,
		((double) xColorPtr->green / USHRT_MAX) * 255,
		((double) xColorPtr->blue / USHRT_MAX) * 255);
}

#endif /* WIDGET_TRANSPARENCY */

/*
 * Calculate rows/columns covered in parent Widget.
 */
static void CalcLimits(Widget *widgetPtr, WidgetItem *itemPtr)
{
	int height, width, dy, dx, my;

	/* WindowToBitmap */
	dy = widgetPtr->by;
	dx = widgetPtr->bx;

	if (widgetPtr->style != WIDGET_STYLE_ISO)
	{
		height = widgetPtr->gheight;
		width = widgetPtr->gwidth;
		my = 1;
	}
	else
	{
		height = ISO_FH2;
		width = ISO_WID2;
		my = 2;

		/* BitmapToCanvas */
		dy += widgetPtr->cy - (ISO_HGT - ISO_FH);
		dx += widgetPtr->cx;
	}

	itemPtr->minY = (dy + itemPtr->y1) / height * my;
	itemPtr->maxY = (dy + itemPtr->y2) / height * my;
	itemPtr->minX = (dx + itemPtr->x1) / width;
	itemPtr->maxX = (dx + itemPtr->x2) / width;
	if (((dy + itemPtr->y2) % height) == 0)
		itemPtr->maxY -= my;
	if (((dx + itemPtr->x2) % width) == 0)
		--itemPtr->maxX;
}

#define DEF_BEVEL_LIGHT "Black"
#define DEF_BEVEL_DARK "White"
#define DEF_BEVEL_OPACITY "127"

/*
 * The structure below defines the record for each progress bar item.
 */
typedef struct ProgressItem {
	WidgetItem header; /* Required header info */
    Tk_Anchor anchor;	/* Where to anchor rect relative to (x,y). */
	int x, y; /* Pixel offsets in Widget bitmap */
	int width, height; /* Size of bar in pixels */
	XColor *done, *todo; /* Color info */
	int bevel; /* Boolean: Draw a bevel or not */
	XColor *bevelLight, *bevelDark; /* Color info for bevel */
	int cur, max; /* Progress current, maximum values */
	long length; /* Length in pixels of "done" part */
/* WIDGET_TRANSPARENCY */
	int done1, todo1, done2, todo2; /* */
	int bevelLight1, bevelDark1, bevelLight2, bevelDark2; /* */
#ifdef WIDGET_TRANSPARENCY
	t_widget_color *tints[4];
	BitmapType bitmap; /* Bitmap to draw into */
	IconPtr srcBits, dstBits; /* Pixel addresses for reading/writing */
#endif
	int dirty[4], hasDirty;
} ProgressItem;

/*
 * Constants for Tk_OptionSpec.typeMask.
 */
#define PROGRESS_SIZE 0x0001
#define PROGRESS_MOVE 0x0002
#define PROGRESS_DONE 0x0004
#define PROGRESS_TODO 0x0008
#define PROGRESS_BEVEL_L 0x0010
#define PROGRESS_BEVEL_D 0x0020
#define PROGRESS_VAL 0x0040
#define PROGRESS_DRAW 0x4000
#define PROGRESS_DISPLAY 0x8000

static Tk_OptionSpec optionSpecProgress[] = {
    {TK_OPTION_INT, "-x", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(ProgressItem, x), 0, 0,
     PROGRESS_MOVE | PROGRESS_DISPLAY},
    {TK_OPTION_INT, "-y", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(ProgressItem, y), 0, 0,
     PROGRESS_MOVE | PROGRESS_DISPLAY},
    {TK_OPTION_INT, "-width", (char *) NULL, (char *) NULL,
     "160", -1, Tk_Offset(ProgressItem, width), 0, 0,
     PROGRESS_SIZE | PROGRESS_DRAW | PROGRESS_DISPLAY},
    {TK_OPTION_INT, "-height", (char *) NULL, (char *) NULL,
     "7", -1, Tk_Offset(ProgressItem, height), 0, 0,
     PROGRESS_SIZE | PROGRESS_DRAW | PROGRESS_DISPLAY},
    {TK_OPTION_COLOR, "-done", (char *) NULL, (char *) NULL,
     "Black", -1, Tk_Offset(ProgressItem, done), 0, 0,
     PROGRESS_DONE | PROGRESS_DISPLAY},
    {TK_OPTION_COLOR, "-todo", (char *) NULL, (char *) NULL,
     "White", -1, Tk_Offset(ProgressItem, todo), 0, 0,
     PROGRESS_TODO | PROGRESS_DISPLAY},
    {TK_OPTION_BOOLEAN, "-bevel", (char *) NULL, (char *) NULL,
     "1", -1, Tk_Offset(ProgressItem, bevel), 0, 0, 0},
    {TK_OPTION_COLOR, "-bevellight", (char *) NULL, (char *) NULL,
     DEF_BEVEL_LIGHT, -1, Tk_Offset(ProgressItem, bevelLight), 0, 0,
     PROGRESS_BEVEL_L | PROGRESS_DISPLAY},
    {TK_OPTION_COLOR, "-beveldark", (char *) NULL, (char *) NULL,
     DEF_BEVEL_DARK, -1, Tk_Offset(ProgressItem, bevelDark), 0, 0,
     PROGRESS_BEVEL_D | PROGRESS_DISPLAY},
/* WIDGET_TRANSPARENCY */
    {TK_OPTION_INT, "-done2", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(ProgressItem, done2), 0, 0,
     PROGRESS_DONE | PROGRESS_DISPLAY},
    {TK_OPTION_INT, "-todo2", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(ProgressItem, todo2), 0, 0,
     PROGRESS_TODO | PROGRESS_DISPLAY},
    {TK_OPTION_INT, "-bevellight2", (char *) NULL, (char *) NULL,
     DEF_BEVEL_OPACITY, -1, Tk_Offset(ProgressItem, bevelLight2), 0, 0,
     PROGRESS_BEVEL_L | PROGRESS_DISPLAY},
    {TK_OPTION_INT, "-beveldark2", (char *) NULL, (char *) NULL,
     DEF_BEVEL_OPACITY, -1, Tk_Offset(ProgressItem, bevelDark2), 0, 0,
     PROGRESS_BEVEL_D | PROGRESS_DISPLAY},
/* */
    {TK_OPTION_INT, "-current", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(ProgressItem, cur), 0, 0,
     PROGRESS_VAL | PROGRESS_DRAW | PROGRESS_DISPLAY},
    {TK_OPTION_INT, "-maximum", (char *) NULL, (char *) NULL,
     "1", -1, Tk_Offset(ProgressItem, max), 0, 0, 
     PROGRESS_VAL | PROGRESS_DRAW | PROGRESS_DISPLAY},
    {TK_OPTION_BOOLEAN, "-visible", (char *) NULL, (char *) NULL,
     "1", -1, Tk_Offset(WidgetItem, visible), 0, 0,
     PROGRESS_DISPLAY},
    {TK_OPTION_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
	 "center", -1, Tk_Offset(ProgressItem, anchor), 0, 0,
	 PROGRESS_MOVE | PROGRESS_DISPLAY},
    {TK_OPTION_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, -1, 0, 0, 0}
};

static int	CreateProgress _ANSI_ARGS_((Tcl_Interp *interp,
		    Widget *widgetPtr, WidgetItem *itemPtr,
		    int objc, Tcl_Obj *CONST objv[]));
static int	ConfigureProgress _ANSI_ARGS_((Tcl_Interp *interp,
		    Widget *widgetPtr, WidgetItem *itemPtr,
		    int objc, Tcl_Obj *CONST objv[]));
static int	DisplayProgress _ANSI_ARGS_((Tcl_Interp *interp,
		    Widget *widgetPtr, WidgetItem *itemPtr));
static int	ChangedProgress _ANSI_ARGS_((Tcl_Interp *interp,
		    Widget *widgetPtr, WidgetItem *itemPtr));
static void	DeleteProgress _ANSI_ARGS_((Widget *widgetPtr,
			WidgetItem *itemPtr));
#ifdef WIDGET_TRANSPARENCY
static void SetBitsOfProgress(WidgetItem *itemPtr);
#endif /* WIDGET_TRANSPARENCY */
static void ComputeProgressBbox(ProgressItem *barPtr);

WidgetItemType ProgressType = {
	"progressbar",
	sizeof(ProgressItem),
	optionSpecProgress,
	NULL, /* optionTable */
	CreateProgress,
	ConfigureProgress,
	DisplayProgress,
	ChangedProgress,
	DeleteProgress,
	(WidgetItemType *) NULL
};

/* 
 * Item creation callback
 */
static int CreateProgress(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
	ProgressItem *barPtr = (ProgressItem *) itemPtr;
#ifdef WIDGET_TRANSPARENCY
	int i;
#endif

	barPtr->header.visible = FALSE;
	barPtr->anchor = TK_ANCHOR_CENTER;
	barPtr->x = 0;
	barPtr->y = 0;
	barPtr->width = 0;
	barPtr->height = 0;
	barPtr->done = NULL;
	barPtr->todo = NULL;
	barPtr->bevel = TRUE;
	barPtr->bevelLight = NULL;
	barPtr->bevelDark = NULL;
/* WIDGET_TRANSPARENCY */
	barPtr->done1 = 0;
	barPtr->todo1 = 0;
	barPtr->done2 = 0;
	barPtr->todo2 = 0;
	barPtr->bevelLight2 = 0;
	barPtr->bevelDark2 = 0;
#ifdef WIDGET_TRANSPARENCY
	for (i = 0; i < 4; i++) barPtr->tints[i] = NULL;
	barPtr->bitmap.pixelPtr = NULL;
#endif
	barPtr->cur = 0;
	barPtr->max = 0;
	barPtr->length = 0;
	barPtr->hasDirty = FALSE;

	/* Set the default options for the new widget */
	if (Tk_InitOptions(interp, (char *) itemPtr,
		itemPtr->typePtr->optionTable, widgetPtr->tkwin) != TCL_OK)
	{
		DeleteProgress(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	if (ConfigureProgress(interp, widgetPtr, itemPtr, objc, objv) != TCL_OK)
	{
		DeleteProgress(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * Item configuration callback
 */
static int ConfigureProgress(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
    Tk_SavedOptions savedOptions;
	Tcl_Obj *errorResult = NULL;
	int error, mask = 0;

	ProgressItem *barPtr = (ProgressItem *) itemPtr;
	int redraw = 0, redisplay = 0, resize = 0, move = 0;
	int wasVisible = itemPtr->visible;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("ConfigureProgress\n");

	/*
	 * The following loop is potentially executed twice.  During the
	 * first pass configuration options get set to their new values.
	 * If there is an error in this pass, we execute a second pass
	 * to restore all the options to their previous values.
	 */
	for (error = 0; error <= 1; error++)
	{
		if (!error)
		{
			/*
			 * First pass: set options to new values.
			 */
			if (Tk_SetOptions(interp, (char *) itemPtr,
				itemPtr->typePtr->optionTable, objc, objv,
				widgetPtr->tkwin, &savedOptions, &mask) != TCL_OK)
			{
				continue;
			}
		}
		else
		{
			/*
			 * Second pass: restore options to old values.
			 */
			errorResult = Tcl_GetObjResult(interp);
			Tcl_IncrRefCount(errorResult);
			Tk_RestoreSavedOptions(&savedOptions);
		}

		redisplay = (mask & PROGRESS_DISPLAY) != 0;
		redraw = (mask & PROGRESS_DRAW) != 0;

		/* The item is being created, don't bother optimizing */
		if (barPtr->done == NULL)
		{
			ComputeProgressBbox(barPtr);

			if ((barPtr->header.x1 < 0) || (barPtr->width < 0) ||
				(barPtr->header.x2 >= widgetPtr->width))
			{
				Tcl_AppendResult(interp, "illegal x size or position", (char *) NULL);
				continue;
			}
			if ((barPtr->header.y1 < 0) || (barPtr->height < 0) ||
				(barPtr->header.y2 >= widgetPtr->height))
			{
				Tcl_AppendResult(interp, "illegal y size or position", (char *) NULL);
				continue;
			}

			/* Calculate which grids are covered by us */
			CalcLimits(widgetPtr, itemPtr);

#ifdef WIDGET_TRANSPARENCY

			if (widgetPtr->bitmap.depth == 8)
			{
				barPtr->bitmap.width = barPtr->width;
				barPtr->bitmap.height = barPtr->height;
				barPtr->bitmap.depth = 8;
				Bitmap_New(interp, &barPtr->bitmap);

				barPtr->srcBits = barPtr->bitmap.pixelPtr;
				barPtr->dstBits = widgetPtr->bitmap.pixelPtr +
					(widgetPtr->bx + barPtr->header.x1) * widgetPtr->bitmap.pixelSize +
					(widgetPtr->by + barPtr->header.y1) * widgetPtr->bitmap.pitch;

				barPtr->done1 = XColor2PaletteIndex(barPtr->done);
				barPtr->todo1 = XColor2PaletteIndex(barPtr->todo);
				barPtr->bevelLight1 = XColor2PaletteIndex(barPtr->bevelLight);
				barPtr->bevelDark1 = XColor2PaletteIndex(barPtr->bevelDark);

				if (BAD_COLOR(barPtr->done2) || BAD_COLOR(barPtr->todo2))
				{
					Tcl_AppendResult(interp, "bad opacity", (char *) NULL);
					continue;
				}

				barPtr->tints[0] = WidgetColor_Alloc(barPtr->done1,
					barPtr->done2);
				barPtr->tints[1] = WidgetColor_Alloc(barPtr->todo1,
					barPtr->todo2);
				barPtr->tints[2] = WidgetColor_Alloc(barPtr->bevelLight1,
					barPtr->bevelLight2);
				barPtr->tints[3] = WidgetColor_Alloc(barPtr->bevelDark1,
					barPtr->bevelDark2);
			}
#endif /* */

			if ((barPtr->cur < 0) || (barPtr->cur > barPtr->max) ||
				(barPtr->max < 1))
			{
				Tcl_AppendResult(interp, "bad min/max value", (char *) NULL);
				continue;
			}
			barPtr->length = (barPtr->width - 2) * ((long) barPtr->cur) / barPtr->max;
		}

		/* Post-creation configure, so optimize */
		else
		{
			/* Resize if height or width changed */
			resize = (mask & PROGRESS_SIZE);

			/* Move if x, y, or anchor changed */
			move = (mask & PROGRESS_MOVE);

			/* Recalculate the bound box if it moved or resized */
			if (resize || move)
			{
				ComputeProgressBbox(barPtr);

				if ((barPtr->header.x1 < 0) || (barPtr->width < 0) ||
					(barPtr->header.x2 >= widgetPtr->width))
				{
					Tcl_AppendResult(interp, "illegal x size or position",
						(char *) NULL);
					continue;
				}
				if ((barPtr->header.y1 < 0) || (barPtr->height < 0) ||
					(barPtr->header.y2 >= widgetPtr->height))
				{
					Tcl_AppendResult(interp, "illegal y size or position",
						(char *) NULL);
					continue;
				}

				/* Calculate which grids are covered by us */
				CalcLimits(widgetPtr, itemPtr);
			}

#ifdef WIDGET_TRANSPARENCY

			if (widgetPtr->bitmap.depth == 8)
			{
				/* Create the bitmap if we resized */
				if (resize)
				{
					if (barPtr->bitmap.pixelPtr != NULL)
					{
						Bitmap_Delete(&barPtr->bitmap);
						barPtr->bitmap.pixelPtr = NULL;
					}
					barPtr->bitmap.width = barPtr->width;
					barPtr->bitmap.height = barPtr->height;
					barPtr->bitmap.depth = 8;
					Bitmap_New(interp, &barPtr->bitmap);
				}

				if (resize || move)
				{
					barPtr->srcBits = barPtr->bitmap.pixelPtr;
					barPtr->dstBits = widgetPtr->bitmap.pixelPtr +
						(widgetPtr->bx + barPtr->header.x1) * widgetPtr->bitmap.pixelSize +
						(widgetPtr->by + barPtr->header.y1) * widgetPtr->bitmap.pitch;
				}

				if (BAD_COLOR(barPtr->done1) || BAD_COLOR(barPtr->done2) ||
					BAD_COLOR(barPtr->todo1) || BAD_COLOR(barPtr->todo2))
				{
					Tcl_AppendResult(interp, "bad color or opacity", (char *) NULL);
					continue;
				}

				if (mask & PROGRESS_DONE)
				{
					barPtr->done1 = XColor2PaletteIndex(barPtr->done);
					WidgetColor_Deref(barPtr->tints[0]);
					barPtr->tints[0] = WidgetColor_Alloc(barPtr->done1,
						barPtr->done2);
				}

				if (mask & PROGRESS_TODO)
				{
					barPtr->todo1 = XColor2PaletteIndex(barPtr->todo);
					WidgetColor_Deref(barPtr->tints[1]);
					barPtr->tints[1] = WidgetColor_Alloc(barPtr->todo1,
						barPtr->todo2);
				}

				if (mask & PROGRESS_BEVEL_L)
				{
					barPtr->bevelLight1 = XColor2PaletteIndex(barPtr->bevelLight);
					WidgetColor_Deref(barPtr->tints[2]);
					barPtr->tints[2] = WidgetColor_Alloc(barPtr->bevelLight1,
						barPtr->bevelLight2);
				}

				if (mask & PROGRESS_BEVEL_D)
				{
					barPtr->bevelDark1 = XColor2PaletteIndex(barPtr->bevelDark);
					WidgetColor_Deref(barPtr->tints[3]);
					barPtr->tints[3] = WidgetColor_Alloc(barPtr->bevelDark1,
						barPtr->bevelDark2);
				}
			}
#endif /* */

			/* Current/Maximum */
			if (resize || (mask & PROGRESS_VAL))
			{
				if ((barPtr->cur < 0) || (barPtr->cur > barPtr->max) ||
					(barPtr->max < 1))
				{
					Tcl_AppendResult(interp, "bad min/max value", (char *) NULL);
					continue;
				}
				barPtr->length = (barPtr->width - 2) * ((long) barPtr->cur) / barPtr->max;
			}
		}
		break;
	}

	if (error)
	{
		Tcl_SetObjResult(interp, errorResult);
		Tcl_DecrRefCount(errorResult);

		/* Failure */
		return TCL_ERROR;
	}

	Tk_FreeSavedOptions(&savedOptions);

#ifdef WIDGET_TRANSPARENCY
	if (widgetPtr->bitmap.depth == 8)
	{
		if (redraw)
			SetBitsOfProgress(itemPtr);
	}
#endif /* */

	if (redisplay && (itemPtr->visible || wasVisible))
	{
		if (barPtr->hasDirty && (resize || move))
		{
			Widget_InvalidateArea(widgetPtr, barPtr->dirty[0],
				barPtr->dirty[1], barPtr->dirty[2], barPtr->dirty[3]);
		}
		barPtr->hasDirty = FALSE;
		Widget_InvalidateArea(widgetPtr, itemPtr->minY, itemPtr->minX,
			itemPtr->maxY, itemPtr->maxX);
		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);
	}

	/* Success */
	return TCL_OK;
}

/*
 * Item delete callback
 */
static void	DeleteProgress(Widget *widgetPtr, WidgetItem *itemPtr)
{
#ifdef WIDGET_TRANSPARENCY
	ProgressItem *barPtr = (ProgressItem *) itemPtr;
	int i;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("DeleteProgress\n");

	if (barPtr->bitmap.pixelPtr != NULL)
	{
		Bitmap_Delete(&barPtr->bitmap);
	}

	for (i = 0; i < 4; i++)
	{
		WidgetColor_Deref(barPtr->tints[i]);
	}
#endif /* */
}

/*
 * Calculates the coordinates of the Widget item, using the given
 * position and anchor.
 */
static void ComputeProgressBbox(ProgressItem *barPtr)
{
	int leftX, topY, width, height;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("ComputeProgressBbox\n");

	width = barPtr->width;
	height = barPtr->height;

	leftX = barPtr->x;
	topY = barPtr->y;

	switch (barPtr->anchor)
	{
		case TK_ANCHOR_NW:
		case TK_ANCHOR_N:
		case TK_ANCHOR_NE:
			break;

		case TK_ANCHOR_W:
		case TK_ANCHOR_CENTER:
		case TK_ANCHOR_E:
			topY -= height / 2;
			break;

		case TK_ANCHOR_SW:
		case TK_ANCHOR_S:
		case TK_ANCHOR_SE:
			topY -= height;
			break;
	}
	switch (barPtr->anchor)
	{
		case TK_ANCHOR_NW:
		case TK_ANCHOR_W:
		case TK_ANCHOR_SW:
			break;

		case TK_ANCHOR_N:
		case TK_ANCHOR_CENTER:
		case TK_ANCHOR_S:
			leftX -= width / 2;
			break;

		case TK_ANCHOR_NE:
		case TK_ANCHOR_E:
		case TK_ANCHOR_SE:
			leftX -= width;
			break;
	}

	barPtr->header.x1 = leftX;
	barPtr->header.y1 = topY;
	barPtr->header.x2 = leftX + width;
	barPtr->header.y2 = topY + height;
}

#ifdef WIDGET_TRANSPARENCY

/*
 * Item display callback
 */
static int DisplayProgress_Transparent(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
	ProgressItem *barPtr = (ProgressItem *) itemPtr;
	int y, x, i;
	int width = barPtr->width;
	int height = barPtr->height;
	IconPtr srcBits = barPtr->srcBits;
	IconPtr dstBits = barPtr->dstBits;
	TintPtr tints[4];

	for (i = 0; i < 4; i++)
	{
		tints[i] = barPtr->tints[i]->tint;
	}

	for (y = 0; y < height; y++)
	{
		for (x = 0; x < width; x++)
		{
			i = *(srcBits + x); if (i > 3) continue;
			*(dstBits + x) = tints[i][*(dstBits + x)];
		}
		srcBits += barPtr->bitmap.pitch;
		dstBits += widgetPtr->bitmap.pitch;
	}

	barPtr->dirty[0] = itemPtr->minY;
	barPtr->dirty[1] = itemPtr->minX;
	barPtr->dirty[2] = itemPtr->maxY;
	barPtr->dirty[3] = itemPtr->maxX;
	barPtr->hasDirty = TRUE;

	return TCL_OK;
}

/*
 * Draws the progress bar into the item bitmap
 */
static void SetBitsOfProgress(WidgetItem *itemPtr)
{
	ProgressItem *barPtr = (ProgressItem *) itemPtr;
	XRectangle rect;
	int y, x;
	IconPtr bits1;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("SetBitsOfProgress\n");

	XSetRect(&rect, 0, 0, barPtr->width, barPtr->height);
	Bevel(&rect, &barPtr->bitmap, 2, 3);

	/* Done Part/ Todo part */
	bits1 = barPtr->srcBits + 1 + barPtr->bitmap.pitch;
	for (y = 0; y < barPtr->height - 2; y++)
	{
		for (x = 0; x < barPtr->length; x++)
		{
			*(bits1 + x) = 0;
		}
		for (; x < barPtr->width - 2; x++)
		{
			*(bits1 + x) = 1;
		}
		bits1 += barPtr->bitmap.pitch;
	}
}

#endif /* not WIDGET_TRANSPARENCY */

/*
 * Item display callback
 */
static int DisplayProgress_Solid(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
	ProgressItem *barPtr = (ProgressItem *) itemPtr;
	int width = barPtr->width;
	int height = barPtr->height;
	GC gc;
	Display *display = widgetPtr->display;
	Pixmap pixmap = widgetPtr->bitmap.pixmap;

	gc = Tk_GCForColor(barPtr->done, pixmap);
	XFillRectangle(display, pixmap, gc,
		widgetPtr->bx + itemPtr->x1 + 1, widgetPtr->by + itemPtr->y1 + 1,
		barPtr->length, height - 2);

	gc = Tk_GCForColor(barPtr->todo, pixmap);
	XFillRectangle(display, pixmap, gc,
		widgetPtr->bx + itemPtr->x1 + 1 + barPtr->length,
		widgetPtr->by + itemPtr->y1 + 1,
		width - barPtr->length - 2, height - 2);

	gc = Tk_GCForColor(barPtr->bevelLight, pixmap);
	XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x1,
		widgetPtr->by + itemPtr->y2 - 1,
		widgetPtr->bx + itemPtr->x1, widgetPtr->by + itemPtr->y1);
	XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x1,
		widgetPtr->by + itemPtr->y1,
		widgetPtr->bx + itemPtr->x2 - 1, widgetPtr->by + itemPtr->y1);

	gc = Tk_GCForColor(barPtr->bevelDark, pixmap);
	XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x1,
		widgetPtr->by + itemPtr->y2 - 1,
		widgetPtr->bx + itemPtr->x2 - 1, widgetPtr->by + itemPtr->y2 - 1);
	XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x2 - 1,
		widgetPtr->by + itemPtr->y2 - 1,
		widgetPtr->bx + itemPtr->x2 - 1, widgetPtr->by + itemPtr->y1);

	barPtr->dirty[0] = itemPtr->minY;
	barPtr->dirty[1] = itemPtr->minX;
	barPtr->dirty[2] = itemPtr->maxY;
	barPtr->dirty[3] = itemPtr->maxX;
	barPtr->hasDirty = TRUE;

	return TCL_OK;
}

/*
 * Item display callback
 */
static int DisplayProgress(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
#ifdef WIDGET_TRANSPARENCY

	/* Transparent */
	if (widgetPtr->bitmap.depth == 8)
		return DisplayProgress_Transparent(interp, widgetPtr, itemPtr);

#endif /* WIDGET_TRANSPARENCY */

	/* Not transparent */
	return DisplayProgress_Solid(interp, widgetPtr, itemPtr);
}

/*
 * Item changed callback
 */
static int ChangedProgress(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
#ifdef WIDGET_TRANSPARENCY
	ProgressItem *barPtr = (ProgressItem *) itemPtr;
#endif
	int delta;

	if (itemPtr->x2 >= widgetPtr->width)
	{
		delta = itemPtr->x2 - widgetPtr->width;
		itemPtr->x1 -= delta, itemPtr->x2 -= delta;
	}
	if (itemPtr->y2 >= widgetPtr->height)
	{
		delta = itemPtr->y2 - widgetPtr->height;
		itemPtr->y1 -= delta, itemPtr->y2 -= delta;
	}

	/* Calculate which grids are covered by us */
	CalcLimits(widgetPtr, itemPtr);

#ifdef WIDGET_TRANSPARENCY
	if (widgetPtr->bitmap.depth == 8)
	{
		barPtr->dstBits = widgetPtr->bitmap.pixelPtr +
			(widgetPtr->bx + itemPtr->x1) * widgetPtr->bitmap.pixelSize +
			(widgetPtr->by + itemPtr->y1) * widgetPtr->bitmap.pitch;
	}
#endif /* */

	return TCL_OK;
}

#define DEF_TEXT_FILL "Black"
#define DEF_TEXT_BACK "White"
#define DEF_TEXT_OPACITY "127"

/*
 * The structure below defines the record for each text item.
 */
typedef struct TextItem {
	WidgetItem header; /* Required header info */
	int x, y; /* Pixel offsets in Widget bitmap */
    Tk_Anchor anchor;	/* Where to anchor rect relative to (x,y). */
    Tk_Font tkfont;		/* Font for drawing text. */
    Tk_Justify justify;		/* Justification mode for text. */
    char *text; /* Text for item (malloc'd). */
	int numChars; /* Number of non-NULL characters in text. */
	int width, height; /* Size of bounding rect in pixels */
	XColor *fill, *background; /* Color info */
	int bevel; /* Boolean: Draw a bevel or not */
	XColor *bevelLight, *bevelDark; /* Color info for bevel */
	int clipX, clipY; /* Boolean: Clip displayed area to size of text + padding */
	int expandX, expandY; /* Boolean: Expand to text size if needed */
	int padLeft, padRight, padTop, padBottom; /* Text padding */
	int fill1, background1, fill2, background2; /* */
	int bevelLight1, bevelDark1, bevelLight2, bevelDark2; /* */
#ifdef WIDGET_TRANSPARENCY
	t_widget_color *tints[4]; /* Tint tables */
	BitmapType bitmap; /* Bitmap to draw into */
	IconPtr srcBits, dstBits; /* Pixel addresses for reading/writing */
#endif
	int textHeight, textWidth, textLeft; /* Height & width of text, left of text */
	int textAscent;
	GC textGC;
	int dirty[4], hasDirty;
} TextItem;

#define TEXT_SIZE 0x0001
#define TEXT_MOVE 0x0002
#define TEXT_DRAW 0x0004
#define TEXT_DISPLAY 0x0008
#define TEXT_TEXT 0x0010
#define TEXT_FONT 0x0020
#define TEXT_FILL 0x0040
#define TEXT_BACKGROUND 0x0080
#define TEXT_BEVEL_L 0x0100
#define TEXT_BEVEL_D 0x0200

#define TEXT_EXPAND 0x0400

static char *keyword_config_text[] = {
	"size", "move", "draw", "display",
	"text", "font", "fill", "background",
	"bevel_light", "bevel_dark", "expand",
	NULL};

static Tk_OptionSpec optionSpecText[] = {
    {TK_OPTION_PIXELS, "-x", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, x), 0, 0, 0},
    {TK_OPTION_PIXELS, "-y", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, y), 0, 0, 0},
    {TK_OPTION_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
	 "center", -1, Tk_Offset(TextItem, anchor), 0, 0, 0},
    {TK_OPTION_FONT, "-font", (char *) NULL, (char *) NULL,
	 "{MS Sans Serif} 8", -1, Tk_Offset(TextItem, tkfont), 0, 0,
	 TEXT_FONT | TEXT_TEXT | TEXT_DRAW | TEXT_DISPLAY},
    {TK_OPTION_JUSTIFY, "-justify", (char *) NULL, (char *) NULL,
	 "left", -1, Tk_Offset(TextItem, justify), 0, 0,
	 TEXT_TEXT | TEXT_DRAW | TEXT_DISPLAY},
    {TK_OPTION_STRING, "-text", (char *) NULL, (char *) NULL,
	 "", -1, Tk_Offset(TextItem, text), 0, 0,
	 TEXT_TEXT | TEXT_DRAW | TEXT_DISPLAY},
    {TK_OPTION_PIXELS, "-width", (char *) NULL, (char *) NULL,
     "160", -1, Tk_Offset(TextItem, width), 0, 0,
     TEXT_SIZE | TEXT_DRAW | TEXT_DISPLAY},
    {TK_OPTION_PIXELS, "-height", (char *) NULL, (char *) NULL,
     "7", -1, Tk_Offset(TextItem, height), 0, 0,
     TEXT_SIZE | TEXT_DRAW | TEXT_DISPLAY},
    {TK_OPTION_COLOR, "-fill", (char *) NULL, (char *) NULL,
     DEF_TEXT_FILL, -1, Tk_Offset(TextItem, fill), 0, 0,
     TEXT_FILL | TEXT_DISPLAY},
    {TK_OPTION_COLOR, "-background", (char *) NULL, (char *) NULL,
     DEF_TEXT_BACK, -1, Tk_Offset(TextItem, background),
     TK_OPTION_NULL_OK, 0,
     TEXT_BACKGROUND | TEXT_DISPLAY},
    {TK_OPTION_BOOLEAN, "-bevel", (char *) NULL, (char *) NULL,
     "1", -1, Tk_Offset(TextItem, bevel), 0, 0, 0},
    {TK_OPTION_COLOR, "-bevellight", (char *) NULL, (char *) NULL,
     DEF_BEVEL_LIGHT, -1, Tk_Offset(TextItem, bevelLight), 0, 0,
     TEXT_BEVEL_L | TEXT_DISPLAY},
    {TK_OPTION_COLOR, "-beveldark", (char *) NULL, (char *) NULL,
     DEF_BEVEL_DARK, -1, Tk_Offset(TextItem, bevelDark), 0, 0,
     TEXT_BEVEL_D | TEXT_DISPLAY},
/* WIDGET_TRANSPARENCY */
    {TK_OPTION_INT, "-fill2", (char *) NULL, (char *) NULL,
     DEF_TEXT_OPACITY, -1, Tk_Offset(TextItem, fill2), 0, 0,
     TEXT_FILL | TEXT_DISPLAY},
    {TK_OPTION_INT, "-background2", (char *) NULL, (char *) NULL,
     DEF_TEXT_OPACITY, -1, Tk_Offset(TextItem, background2), 0, 0,
     TEXT_BACKGROUND | TEXT_DISPLAY},
    {TK_OPTION_INT, "-bevellight2", (char *) NULL, (char *) NULL,
     DEF_BEVEL_OPACITY, -1, Tk_Offset(TextItem, bevelLight2), 0, 0,
     TEXT_BEVEL_L | TEXT_DISPLAY},
    {TK_OPTION_INT, "-beveldark2", (char *) NULL, (char *) NULL,
     DEF_BEVEL_OPACITY, -1, Tk_Offset(TextItem, bevelDark2), 0, 0,
     TEXT_BEVEL_D | TEXT_DISPLAY},
/* */
    {TK_OPTION_BOOLEAN, "-clipx", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, clipX), 0, 0, 0},
    {TK_OPTION_BOOLEAN, "-clipy", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, clipY), 0, 0, 0},
    {TK_OPTION_BOOLEAN, "-expandx", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, expandX), 0, 0, 0},
    {TK_OPTION_BOOLEAN, "-expandy", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, expandY), 0, 0, 0},
    {TK_OPTION_PIXELS, "-padleft", (char *) NULL, (char *) NULL,
     "2", -1, Tk_Offset(TextItem, padLeft), 0, 0, 0},
    {TK_OPTION_PIXELS, "-padright", (char *) NULL, (char *) NULL,
     "2", -1, Tk_Offset(TextItem, padRight), 0, 0, 0},
    {TK_OPTION_PIXELS, "-padtop", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, padTop), 0, 0, 0},
    {TK_OPTION_PIXELS, "-padbottom", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(TextItem, padBottom), 0, 0, 0},
    {TK_OPTION_BOOLEAN, "-visible", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(WidgetItem, visible), 0, 0,
     TEXT_DISPLAY},
    {TK_OPTION_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, -1, 0, 0, 0}
};

static int	CreateText _ANSI_ARGS_((Tcl_Interp *interp,
			Widget *widgetPtr, WidgetItem *itemPtr,
			int objc, Tcl_Obj *CONST objv[]));
static int	ConfigureText _ANSI_ARGS_((Tcl_Interp *interp,
			Widget *widgetPtr, WidgetItem *itemPtr,
			int objc, Tcl_Obj *CONST objv[]));
static int	DisplayText _ANSI_ARGS_((Tcl_Interp *interp,
			Widget *widgetPtr, WidgetItem *itemPtr));
static int	ChangedText _ANSI_ARGS_((Tcl_Interp *interp,
			Widget *widgetPtr, WidgetItem *itemPtr));
static void	DeleteText _ANSI_ARGS_((Widget *widgetPtr,
			WidgetItem *itemPtr));
#ifdef WIDGET_TRANSPARENCY
static void SetBitsOfText(Widget *widgetPtr, WidgetItem *itemPtr);
#endif /* WIDGET_TRANSPARENCY */
static int SanityCheckText(Widget *widgetPtr, TextItem *textPtr);
static void ComputeTextLayout(TextItem *textPtr, int *maskPtr);

WidgetItemType TextType = {
	"text",
	sizeof(TextItem),
	optionSpecText,
	NULL, /* optionTable */
	CreateText,
	ConfigureText,
	DisplayText,
	ChangedText,
	DeleteText,
	(WidgetItemType *) NULL
};

/*
 * Item creation callback
 */
static int CreateText(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
	TextItem *textPtr = (TextItem *) itemPtr;
#ifdef WIDGET_TRANSPARENCY
	int i;
#endif

	textPtr->header.visible = FALSE;
	textPtr->x = 0;
	textPtr->y = 0;
	textPtr->anchor	= TK_ANCHOR_CENTER;	
	textPtr->tkfont	= NULL;
	textPtr->justify = TK_JUSTIFY_LEFT;
	textPtr->text = NULL;
	textPtr->numChars = 0;
	textPtr->width = 0;
	textPtr->height = 0;
	textPtr->fill = NULL;
	textPtr->background = NULL;
	textPtr->bevel = FALSE;
	textPtr->bevelLight = NULL;
	textPtr->bevelDark = NULL;
/* WIDGET_TRANSPARENCY */
	textPtr->fill1 = 0;
	textPtr->background1 = 0;
	textPtr->fill2 = 0;
	textPtr->background2 = 0;
	textPtr->bevelLight1 = 0;
	textPtr->bevelDark1 = 0;
	textPtr->bevelLight2 = 0;
	textPtr->bevelDark2 = 0;
#ifdef WIDGET_TRANSPARENCY
	for (i = 0; i < 4; i++) textPtr->tints[i] = NULL;
	textPtr->bitmap.pixelPtr = NULL;
	textPtr->srcBits = textPtr->dstBits = NULL;
#endif
	textPtr->clipX = FALSE;
	textPtr->clipY = FALSE;
	textPtr->expandX = FALSE;
	textPtr->expandY = FALSE;
	textPtr->padLeft = 2;
	textPtr->padRight = 2;
	textPtr->padTop = 0;
	textPtr->padBottom = 0;
	textPtr->textHeight = 0;
	textPtr->textWidth = 0;
	textPtr->textLeft = 0;
	textPtr->textAscent = 0;
	textPtr->textGC = None;
	textPtr->hasDirty = FALSE;

	/* Set the default options for the new widget */
	if (Tk_InitOptions(interp, (char *) itemPtr,
		itemPtr->typePtr->optionTable, widgetPtr->tkwin) != TCL_OK)
	{
		DeleteText(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	if (ConfigureText(interp, widgetPtr, itemPtr, objc, objv)
		!= TCL_OK)
	{
		DeleteText(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * Item configuration callback
 */
static int ConfigureText(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
	Tk_SavedOptions savedOptions;
	Tcl_Obj *errorResult = NULL;
	int error, mask = 0;

	TextItem *textPtr = (TextItem *) itemPtr;
	int wasVisible = itemPtr->visible;
	int redraw = 0, redisplay = 0, resize = 0, text, layout = 0;
	Tk_FontMetrics fm;
	XGCValues gcValues;
	unsigned long gcMask;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("ConfigureText\n");

	/*
	 * The following loop is potentially executed twice.  During the
	 * first pass configuration options get set to their new values.
	 * If there is an error in this pass, we execute a second pass
	 * to restore all the options to their previous values.
	 */
	for (error = 0; error <= 1; error++)
	{
		if (!error)
		{
			/*
			 * First pass: set options to new values.
			 */
			if (Tk_SetOptions(interp, (char *) itemPtr,
				itemPtr->typePtr->optionTable, objc, objv,
				widgetPtr->tkwin, &savedOptions, &mask) != TCL_OK)
			{
				continue;
			}

			if (debug_widgets & DEBUG_WIDGET_ITEM) debug_option(mask, keyword_config_text);
		}
		else
		{
			/*
			 * Second pass: restore options to old values.
			 */
			errorResult = Tcl_GetObjResult(interp);
			Tcl_IncrRefCount(errorResult);
			Tk_RestoreSavedOptions(&savedOptions);
		}

		redraw = (mask & TEXT_DRAW) != 0;
		redisplay = (mask & TEXT_DISPLAY) != 0;

		/* We are creating the item, so don't bother optimizing */
		if (textPtr->textGC == None)
		{
			textPtr->numChars = strlen(textPtr->text);
			textPtr->textWidth = Tk_TextWidth(textPtr->tkfont, textPtr->text,
				textPtr->numChars);

			Tk_GetFontMetrics(textPtr->tkfont, &fm);
			textPtr->textHeight = fm.linespace;
			textPtr->textAscent = fm.ascent;

			gcValues.foreground = textPtr->fill->pixel;
#ifdef WIDGET_TRANSPARENCY
			if (widgetPtr->bitmap.depth == 8)
			{
#ifdef PLATFORM_X11
				gcValues.foreground = 0;
#endif
#ifdef PLATFORM_WIN
				unsigned char *rgb = Colormap_GetRGB();
				gcValues.foreground = Plat_RGB2XPixel(rgb[0], rgb[1], rgb[2]);
#endif
			}
#endif /* */
			gcValues.graphics_exposures = False;
			gcValues.font = Tk_FontId(textPtr->tkfont);
			gcMask = GCForeground | GCFont | GCGraphicsExposures;
			textPtr->textGC = Tk_GetGC(widgetPtr->tkwin, gcMask, &gcValues);

			ComputeTextLayout(textPtr, &layout);

			if (SanityCheckText(widgetPtr, textPtr) != TCL_OK)
			{
				continue;
			}

			/* Calculate which grids are covered by us */
			CalcLimits(widgetPtr, itemPtr);

#ifdef WIDGET_TRANSPARENCY

			if (widgetPtr->bitmap.depth == 8)
			{
				/* Create the bitmap */
				textPtr->bitmap.width = textPtr->width;
				textPtr->bitmap.height = textPtr->height;
				textPtr->bitmap.depth = 8;
				Bitmap_New(interp, &textPtr->bitmap);
				textPtr->srcBits = textPtr->bitmap.pixelPtr;
				textPtr->dstBits = widgetPtr->bitmap.pixelPtr +
					(widgetPtr->bx + itemPtr->x1) * widgetPtr->bitmap.pixelSize +
					(widgetPtr->by + itemPtr->y1) * widgetPtr->bitmap.pitch;

				textPtr->fill1 = XColor2PaletteIndex(textPtr->fill);
				textPtr->background1 = XColor2PaletteIndex(textPtr->background);
				textPtr->bevelLight1 = XColor2PaletteIndex(textPtr->bevelLight);
				textPtr->bevelDark1 = XColor2PaletteIndex(textPtr->bevelDark);

				textPtr->tints[0] = WidgetColor_Alloc(textPtr->fill1,
					textPtr->fill2);
				textPtr->tints[1] = WidgetColor_Alloc(textPtr->background1,
					textPtr->background2);
				textPtr->tints[2] = WidgetColor_Alloc(textPtr->bevelLight1,
					textPtr->bevelLight2);
				textPtr->tints[3] = WidgetColor_Alloc(textPtr->bevelDark1,
					textPtr->bevelDark2);
			}

#endif /* WIDGET_TRANSPARENCY */
		}
		else
		{
			resize = (mask & TEXT_SIZE) != 0;
			text = (mask & TEXT_TEXT) != 0;

			/* Text, font or justify changed */
			if (text)
			{
				/* Calculate the length of the text */
				textPtr->numChars = strlen(textPtr->text);

				/* Calculate the width of the text in pixels */
				textPtr->textWidth = Tk_TextWidth(textPtr->tkfont,
					textPtr->text, textPtr->numChars);
			}

			if (mask & TEXT_FONT)
			{
				Tk_GetFontMetrics(textPtr->tkfont, &fm);
				textPtr->textHeight = fm.linespace;
				textPtr->textAscent = fm.ascent;
			}

			if (mask & (TEXT_FONT | TEXT_FILL))
			{
				if (textPtr->textGC)
				{
					Tk_FreeGC(Tk_Display(widgetPtr->tkwin), textPtr->textGC);
				}
				gcValues.foreground = textPtr->fill->pixel;
#ifdef WIDGET_TRANSPARENCY
				if (widgetPtr->bitmap.depth == 8)
				{
#ifdef PLATFORM_X11
					gcValues.foreground = 0;
#endif
#ifdef PLATFORM_WIN
					unsigned char *rgb = Colormap_GetRGB();
					gcValues.foreground = Plat_RGB2XPixel(rgb[0], rgb[1], rgb[2]);
#endif
				}
#endif /* */
				gcValues.graphics_exposures = False;
				gcValues.font = Tk_FontId(textPtr->tkfont);
				gcMask = GCForeground | GCFont | GCGraphicsExposures;
				textPtr->textGC = Tk_GetGC(widgetPtr->tkwin, gcMask, &gcValues);
			}

			/* Calculate the size and position on the display */
			ComputeTextLayout(textPtr, &layout);

			if (SanityCheckText(widgetPtr, textPtr) != TCL_OK)
			{
				continue;
			}

			/* The display size or position changed */
			if (layout)
			{
				/* Calculate which grids are covered by us */
				CalcLimits(widgetPtr, itemPtr);

#ifdef WIDGET_TRANSPARENCY

				if (widgetPtr->bitmap.depth == 8)
				{
					/* The display position changed */
					if (layout & TEXT_MOVE)
					{
						textPtr->dstBits = widgetPtr->bitmap.pixelPtr +
							(widgetPtr->bx + itemPtr->x1) * widgetPtr->bitmap.pixelSize +
							(widgetPtr->by + itemPtr->y1) * widgetPtr->bitmap.pitch;
					}
				}

#endif /* WIDGET_TRANSPARENCY */

				/* The display size changed */
				if (layout & TEXT_SIZE)
				{
					/* Handle automatic expansion of bitmap */
					if (layout & TEXT_EXPAND)
					{
						if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("TEXT_EXPAND\n");
						resize = TRUE;
					}

					/* Redraw the bitmap */
					redraw = TRUE;
				}

				/* Update display if visible */
				redisplay = TRUE;
			}

#ifdef WIDGET_TRANSPARENCY

			if (widgetPtr->bitmap.depth == 8)
			{
				/* The bitmap changed size */
				if (resize)
				{
					if (textPtr->bitmap.pixelPtr != NULL)
					{
						/* Delete the old bitmap */
						Bitmap_Delete(&textPtr->bitmap);

						/* Forget the old bitmap */
						textPtr->bitmap.pixelPtr = NULL;
					}

					/* Create a new bitmap */
					textPtr->bitmap.width = textPtr->width;
					textPtr->bitmap.height = textPtr->height;
					textPtr->bitmap.depth = 8;
					Bitmap_New(interp, &textPtr->bitmap);

					/* Get the address to read from */
					textPtr->srcBits = textPtr->bitmap.pixelPtr;
				}

				/* The text foreground color/opacity changed */
				if (mask & TEXT_FILL)
				{
					textPtr->fill1 = XColor2PaletteIndex(textPtr->fill);
					WidgetColor_Deref(textPtr->tints[0]);
					textPtr->tints[0] = WidgetColor_Alloc(textPtr->fill1,
						textPtr->fill2);
				}

				/* The text background color/opacity changed */
				if (mask & TEXT_BACKGROUND)
				{
					textPtr->background1 = XColor2PaletteIndex(textPtr->background);
					WidgetColor_Deref(textPtr->tints[1]);
					textPtr->tints[1] = WidgetColor_Alloc(textPtr->background1,
						textPtr->background2);
				}

				/* The bevel color/opacity changed */
				if (mask & TEXT_BEVEL_L)
				{
					textPtr->bevelLight1 = XColor2PaletteIndex(textPtr->bevelLight);
					WidgetColor_Deref(textPtr->tints[2]);
					textPtr->tints[2] = WidgetColor_Alloc(textPtr->bevelLight1,
						textPtr->bevelLight2);
				}

				/* The bevel color/opacity changed */
				if (mask & TEXT_BEVEL_D)
				{
					textPtr->bevelDark1 = XColor2PaletteIndex(textPtr->bevelDark);
					WidgetColor_Deref(textPtr->tints[3]);
					textPtr->tints[3] = WidgetColor_Alloc(textPtr->bevelDark1,
						textPtr->bevelDark2);
				}
			}

#endif /* WIDGET_TRANSPARENCY */
		}
		break;
	}

	if (error)
	{
		Tcl_SetObjResult(interp, errorResult);
		Tcl_DecrRefCount(errorResult);

		/* Failure */
		return TCL_ERROR;
	}

	Tk_FreeSavedOptions(&savedOptions);

#ifdef WIDGET_TRANSPARENCY

	if (widgetPtr->bitmap.depth == 8)
	{
		/* Redraw the bitmap */
		if (redraw)
		{
			SetBitsOfText(widgetPtr, itemPtr);
		}
	}

#endif /* WIDGET_TRANSPARENCY */

	/* Update display (later) */
	if (redisplay && (itemPtr->visible || wasVisible))
	{
		/* Was drawn, and size or position changed */
		if (textPtr->hasDirty && layout)
		{
			Widget_InvalidateArea(widgetPtr,
				textPtr->dirty[0], textPtr->dirty[1],
				textPtr->dirty[2], textPtr->dirty[3]);
		}
		textPtr->hasDirty = FALSE;
		Widget_InvalidateArea(widgetPtr,
			itemPtr->minY, itemPtr->minX,
			itemPtr->maxY, itemPtr->maxX);
		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);
	}

	/* Success */
	return TCL_OK;
}

/*
 * Item deletion callback
 */
static void	DeleteText(Widget *widgetPtr, WidgetItem *itemPtr)
{
	TextItem *textPtr = (TextItem *) itemPtr;
#ifdef WIDGET_TRANSPARENCY
	int i;
#endif

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("DeleteText\n");

#ifdef WIDGET_TRANSPARENCY
	/* Our bitmap exists */
	if (textPtr->bitmap.pixelPtr != NULL)
	{
		/* Free the bitmap */
		Bitmap_Delete(&textPtr->bitmap);
	}

	/* Free colors */
	for (i = 0; i < 4; i++)
	{
		/* Free this color (if exists) */
		WidgetColor_Deref(textPtr->tints[i]);
	}
#endif

	if (textPtr->textGC)
	{
		Tk_FreeGC(Tk_Display(widgetPtr->tkwin), textPtr->textGC);
	}
}

/*
 * Calculates the coordinates of the text item
 */
static void ComputeTextLayout(TextItem *textPtr, int *maskPtr)
{
	WidgetItem *itemPtr = (WidgetItem *) textPtr;
	int leftX, topY, width, height;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("ComputeTextLayout: ");

	(*maskPtr) = 0;

	/* Get the requested width */
	width = textPtr->width;

	/* Get the requested height */
	height = textPtr->height;

	/* Clip vertically */
	if (textPtr->clipY)
	{
		/* Calculate height of text plus vertical padding */
		height = (textPtr->bevel != 0) * 2 + textPtr->padTop +
			textPtr->textHeight + textPtr->padBottom;
	}

	/* Clip horizontally */
	if (textPtr->clipX)
	{
		/* Calculate width of text plus horizontal padding */
		width = (textPtr->bevel != 0) * 2 + textPtr->padLeft +
			textPtr->textWidth + textPtr->padRight;
	}

	/* Expand vertically */
	if (height > textPtr->height)
	{
		if (textPtr->expandY)
		{
			textPtr->height = height;
			(*maskPtr) |= TEXT_EXPAND;
		}
		else
			height = textPtr->height;
	}

	/* Expand horizontally */
	if (width > textPtr->width)
	{
		if (textPtr->expandX)
		{
			textPtr->width = width;
			(*maskPtr) |= TEXT_EXPAND;
		}
		else
			width = textPtr->width;
	}

	/*
	 * Use overall geometry information to compute the top-left corner
	 * of the bounding box for the text item.
	 */

	leftX = textPtr->x;
	topY = textPtr->y;
	switch (textPtr->anchor)
	{
		case TK_ANCHOR_NW:
		case TK_ANCHOR_N:
		case TK_ANCHOR_NE:
			break;

		case TK_ANCHOR_W:
		case TK_ANCHOR_CENTER:
		case TK_ANCHOR_E:
			topY -= height / 2;
			break;

		case TK_ANCHOR_SW:
		case TK_ANCHOR_S:
		case TK_ANCHOR_SE:
			topY -= height;
			break;
	}
	switch (textPtr->anchor)
	{
		case TK_ANCHOR_NW:
		case TK_ANCHOR_W:
		case TK_ANCHOR_SW:
			break;

		case TK_ANCHOR_N:
		case TK_ANCHOR_CENTER:
		case TK_ANCHOR_S:
			leftX -= width / 2;
			break;

		case TK_ANCHOR_NE:
		case TK_ANCHOR_E:
		case TK_ANCHOR_SE:
			leftX -= width;
			break;
	}

	/* The position changed */
	if ((itemPtr->x1 != leftX) || (itemPtr->y1 != topY))
	{
		(*maskPtr) |= TEXT_MOVE;
	}

	/* The size changed */
	if (width != (itemPtr->x2 - itemPtr->x1) ||
		height != (itemPtr->y2 - itemPtr->y1))
	{
		(*maskPtr) |= TEXT_SIZE;
	}

	itemPtr->x1 = leftX;
	itemPtr->y1 = topY;
	itemPtr->x2 = leftX + width;
	itemPtr->y2 = topY + height;

	switch (textPtr->justify)
	{
		case TK_JUSTIFY_LEFT:
			textPtr->textLeft = textPtr->padLeft;
			break;
		case TK_JUSTIFY_CENTER:
			textPtr->textLeft = width / 2 - textPtr->textWidth / 2;
			break;
		case TK_JUSTIFY_RIGHT:
			textPtr->textLeft = width - textPtr->textWidth -
				textPtr->padRight;
			break;
	}

	if (debug_widgets & DEBUG_WIDGET_ITEM) debug_option((*maskPtr), keyword_config_text);
}

/*
 * Return an error if there are problems with a text item's options
 */
static int SanityCheckText(Widget *widgetPtr, TextItem *textPtr)
{
	WidgetItem *itemPtr = (WidgetItem *) textPtr;

#ifdef WIDGET_TRANSPARENCY
	/* Prevent illegal colors */
	if (BAD_COLOR(textPtr->fill1) || BAD_COLOR(textPtr->background1) ||
		BAD_COLOR(textPtr->fill2) || BAD_COLOR(textPtr->background2))
	{
		Tcl_AppendResult(widgetPtr->interp, "bad color or opacity",
			(char *) NULL);
		return TCL_ERROR;
	}
#endif
	/* Prevent illegal x size or position */
	if ((itemPtr->x1 < 0) || (textPtr->width < 0) ||
		(itemPtr->x2 > widgetPtr->width) ||
		((itemPtr->x2 - itemPtr->x1) > textPtr->width))
	{
		Tcl_AppendResult(widgetPtr->interp, "illegal x size or position",
			(char *) NULL);
		return TCL_ERROR;
	}

	/* Prevent illegal y size or position */
	if ((itemPtr->y1 < 0) || (textPtr->height < 0) ||
		(itemPtr->y2 > widgetPtr->height) ||
		((itemPtr->y2 - itemPtr->y1) > textPtr->height))
	{
		Tcl_AppendResult(widgetPtr->interp, "illegal y size or position",
			(char *) NULL);
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

#ifdef WIDGET_TRANSPARENCY

/*
 * Item display callback
 */
static int DisplayText_Transparent(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
	TextItem *textPtr = (TextItem *) itemPtr;
	int y, x;
	int width = textPtr->header.x2 - textPtr->header.x1;
	int height = textPtr->header.y2 - textPtr->header.y1;
	IconPtr srcBits = textPtr->srcBits;
	IconPtr dstBits = textPtr->dstBits;
	IconValue i;
	TintPtr tints[4];

	for (i = 0; i < 4; i++) tints[i] = textPtr->tints[i]->tint;

	for (y = 0; y < height; y++)
	{
		for (x = 0; x < width; x++)
		{
			/* Get the pixel */
			i = srcBits[x];

			/* Sanity check */
			if (i > 3) continue;

			/* Put the pixel */
			dstBits[x] = tints[i][dstBits[x]];
		}
		srcBits += textPtr->bitmap.pitch;
		dstBits += widgetPtr->bitmap.pitch;
	}

	return TCL_OK;
}

#endif /* WIDGET_TRANSPARENCY */

/*
 * Item display callback
 */
static int DisplayText_Solid(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
	TextItem *textPtr = (TextItem *) itemPtr;
	int width = textPtr->header.x2 - textPtr->header.x1;
	int height = textPtr->header.y2 - textPtr->header.y1;
	Display *display = widgetPtr->display;
	GC gc;
	Pixmap pixmap = widgetPtr->bitmap.pixmap;

	if (textPtr->background != NULL)
	{
		gc = Tk_GCForColor(textPtr->background, pixmap);
		XFillRectangle(display, pixmap, gc,
			widgetPtr->bx + itemPtr->x1, widgetPtr->by + itemPtr->y1,
			width, height);
	}

	/* Draw the text */
	Tk_DrawChars(widgetPtr->display, pixmap,
		textPtr->textGC, textPtr->tkfont, textPtr->text, textPtr->numChars,
		widgetPtr->bx + itemPtr->x1 + textPtr->textLeft,
		widgetPtr->by + itemPtr->y1 + (textPtr->bevel != 0) +
		textPtr->padTop + textPtr->textAscent);

	if (textPtr->bevel)
	{
		gc = Tk_GCForColor(textPtr->bevelLight, pixmap);
		XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x1,
			widgetPtr->by + itemPtr->y2 - 1,
			widgetPtr->bx + itemPtr->x1, widgetPtr->by + itemPtr->y1);
		XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x1,
			widgetPtr->by + itemPtr->y1,
			widgetPtr->bx + itemPtr->x2 - 1, widgetPtr->by + itemPtr->y1);

		gc = Tk_GCForColor(textPtr->bevelDark, pixmap);
		XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x1,
			widgetPtr->by + itemPtr->y2 - 1,
			widgetPtr->bx + itemPtr->x2 - 1, widgetPtr->by + itemPtr->y2 - 1);
		XDrawLine(display, pixmap, gc, widgetPtr->bx + itemPtr->x2 - 1,
			widgetPtr->by + itemPtr->y2 - 1,
			widgetPtr->bx + itemPtr->x2 - 1, widgetPtr->by + itemPtr->y1);
	}

	textPtr->dirty[0] = itemPtr->minY;
	textPtr->dirty[1] = itemPtr->minX;
	textPtr->dirty[2] = itemPtr->maxY;
	textPtr->dirty[3] = itemPtr->maxX;
	textPtr->hasDirty = TRUE;

	return TCL_OK;
}

/*
 * Item display callback
 */
static int DisplayText(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
#ifdef WIDGET_TRANSPARENCY

	/* Transparent */
	if (widgetPtr->bitmap.depth == 8)
		return DisplayText_Transparent(interp, widgetPtr, itemPtr);

#endif /* WIDGET_TRANSPARENCY */

	/* Not transparent */
	return DisplayText_Solid(interp, widgetPtr, itemPtr);
}

#ifdef WIDGET_TRANSPARENCY

/*
 * Draws the text into the item bitmap
 */
static void SetBitsOfText(Widget *widgetPtr, WidgetItem *itemPtr)
{
	TextItem *textPtr = (TextItem *) itemPtr;
	Tk_Window tkwin = widgetPtr->tkwin;
	Display *display = Tk_Display(tkwin);
	int height, width;
	int y, x;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("SetBitsOfText\n");

	height = textPtr->header.y2 - textPtr->header.y1;
	width = textPtr->header.x2 - textPtr->header.x1;

	/* Erase the background */
	for (y = 0; y < height; y++)
	{
		for (x = 0; x < width; x++)
		{
			textPtr->bitmap.pixelPtr[y * textPtr->bitmap.pitch + x] = 1;
		}
	}

	/* Draw text */	
	Tk_DrawChars(display, textPtr->bitmap.pixmap, textPtr->textGC,
		textPtr->tkfont, textPtr->text, textPtr->numChars,
		textPtr->textLeft,
		(textPtr->bevel != 0) + textPtr->padTop + textPtr->textAscent);

	if (textPtr->bevel)
	{
		XRectangle rect;
		XSetRect(&rect, 0, 0, width, height);
		Bevel(&rect, &textPtr->bitmap, 2, 3);
	}
}

#endif /* WIDGET_TRANSPARENCY */

/*
 * Item changed callback
 */
static int ChangedText(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
#ifdef WIDGET_TRANSPARENCY
	TextItem *textPtr = (TextItem *) itemPtr;
#endif
	int delta;

	if (itemPtr->x2 >= widgetPtr->width)
	{
		delta = itemPtr->x2 - widgetPtr->width;
		itemPtr->x1 -= delta, itemPtr->x2 -= delta;
	}
	if (itemPtr->y2 >= widgetPtr->height)
	{
		delta = itemPtr->y2 - widgetPtr->height;
		itemPtr->y1 -= delta, itemPtr->y2 -= delta;
	}

	/* Calculate which grids are covered by us */
	CalcLimits(widgetPtr, itemPtr);

#ifdef WIDGET_TRANSPARENCY
	if (widgetPtr->bitmap.depth == 8)
	{
		textPtr->dstBits = widgetPtr->bitmap.pixelPtr +
			(widgetPtr->bx + itemPtr->x1) * widgetPtr->bitmap.pixelSize +
			(widgetPtr->by + itemPtr->y1) * widgetPtr->bitmap.pitch;
	}
#endif

	return TCL_OK;
}

/*
 * The structure below defines the record for each cursor item.
 */
typedef struct CursorItem {
	WidgetItem header; /* Required header info */
	int x, y; /* Cave location to highlight */
	XColor *color; /* Color info */
	int lineWidth; /* Width of rectangle */
	int col, row; /* Displayed row/col */
	int dirty[2], hasDirty; /* Previous row/col */
} CursorItem;

static Tk_OptionSpec optionSpecCursor[] = {
    {TK_OPTION_BOOLEAN, "-visible", (char *) NULL, (char *) NULL,
     "1", -1, Tk_Offset(WidgetItem, visible), 0, 0, 0},
    {TK_OPTION_PIXELS, "-x", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(CursorItem, x), 0, 0, 0},
    {TK_OPTION_PIXELS, "-y", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(CursorItem, y), 0, 0, 0},
    {TK_OPTION_COLOR, "-color", (char *) NULL, (char *) NULL,
     "yellow", -1, Tk_Offset(CursorItem, color), 0, 0, 0},
    {TK_OPTION_INT, "-linewidth", (char *) NULL, (char *) NULL,
     "2", -1, Tk_Offset(CursorItem, lineWidth), 0, 0, 0},
    {TK_OPTION_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, -1, 0, 0, 0}
};

static int	KreateCursor _ANSI_ARGS_((Tcl_Interp *interp,
			Widget *widgetPtr, WidgetItem *itemPtr,
			int objc, Tcl_Obj *CONST objv[]));
static int	ConfigureCursor _ANSI_ARGS_((Tcl_Interp *interp,
			Widget *widgetPtr, WidgetItem *itemPtr,
			int objc, Tcl_Obj *CONST objv[]));
static int	DisplayCursor _ANSI_ARGS_((Tcl_Interp *interp,
			Widget *widgetPtr, WidgetItem *itemPtr));
static void	DeleteCursor _ANSI_ARGS_((Widget *widgetPtr,
			WidgetItem *itemPtr));
static int CalcCursorPosition(Widget *widgetPtr, CursorItem *cursorPtr);

WidgetItemType CursorType = {
	"cursor",
	sizeof(CursorItem),
	optionSpecCursor,
	NULL, /* optionTable */
	KreateCursor,
	ConfigureCursor,
	DisplayCursor,
	NULL,
	DeleteCursor,
	(WidgetItemType *) NULL
};

/*
 * Item creation callback
 */
static int KreateCursor(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
	CursorItem *cursorPtr = (CursorItem *) itemPtr;

	cursorPtr->header.visible = TRUE;
	cursorPtr->x = 0;
	cursorPtr->y = 0;
	cursorPtr->color = NULL;
	cursorPtr->lineWidth = 2;
	cursorPtr->hasDirty = FALSE;

	/* Set the default options for the new widget */
	if (Tk_InitOptions(interp, (char *) itemPtr,
		itemPtr->typePtr->optionTable, widgetPtr->tkwin) != TCL_OK)
	{
		DeleteCursor(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	if (ConfigureCursor(interp, widgetPtr, itemPtr, objc, objv)
		!= TCL_OK)
	{
		DeleteCursor(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * Item configuration callback
 */
static int ConfigureCursor(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
	Tk_SavedOptions savedOptions;
	Tcl_Obj *errorResult = NULL;
	int error, mask = 0;
	int wasVisible = itemPtr->visible;

	CursorItem *cursorPtr = (CursorItem *) itemPtr;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("ConfigureCursor\n");

	/*
	 * The following loop is potentially executed twice.  During the
	 * first pass configuration options get set to their new values.
	 * If there is an error in this pass, we execute a second pass
	 * to restore all the options to their previous values.
	 */
	for (error = 0; error <= 1; error++)
	{
		if (!error)
		{
			/*
			 * First pass: set options to new values.
			 */
			if (Tk_SetOptions(interp, (char *) itemPtr,
				itemPtr->typePtr->optionTable, objc, objv,
				widgetPtr->tkwin, &savedOptions, &mask) != TCL_OK)
			{
				continue;
			}
		}
		else
		{
			/*
			 * Second pass: restore options to old values.
			 */
			errorResult = Tcl_GetObjResult(interp);
			Tcl_IncrRefCount(errorResult);
			Tk_RestoreSavedOptions(&savedOptions);
		}

		if (!CalcCursorPosition(widgetPtr, cursorPtr))
		{
			itemPtr->minY = 0, itemPtr->maxY = -1;
			itemPtr->minX = 0, itemPtr->maxX = -1;
		}

		break;
	}

	if (error)
	{
		Tcl_SetObjResult(interp, errorResult);
		Tcl_DecrRefCount(errorResult);

		/* Failure */
		return TCL_ERROR;
	}

	Tk_FreeSavedOptions(&savedOptions);

	if (itemPtr->visible || wasVisible)
	{
		if (cursorPtr->hasDirty &&
			((cursorPtr->dirty[0] != cursorPtr->row) ||
			(cursorPtr->dirty[1] != cursorPtr->col)))
		{
			Widget_Invalidate(widgetPtr,
				cursorPtr->dirty[0], cursorPtr->dirty[1]);
		}
		cursorPtr->hasDirty = FALSE;
		Widget_Invalidate(widgetPtr, cursorPtr->row, cursorPtr->col);
	}

	widgetPtr->flags |= WIDGET_DRAW_INVALID;
	Widget_EventuallyRedraw(widgetPtr);

	return TCL_OK;
}

/*
 * Item deletion callback
 */
static void	DeleteCursor(Widget *widgetPtr, WidgetItem *itemPtr)
{
	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("DeleteCursor\n");
}

/*
 * Item display callback
 */
static int DisplayCursor(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
	CursorItem *cursorPtr = (CursorItem *) itemPtr;
	int col, row;
	XGCValues gcValues;
	GC gc;

	if (CalcCursorPosition(widgetPtr, cursorPtr))
	{
		int lineWidth = cursorPtr->lineWidth;

		col = cursorPtr->col, row = cursorPtr->row;

		gcValues.foreground = cursorPtr->color->pixel;

		if (widgetPtr->style != WIDGET_STYLE_ISO)
		{
			int gHeight = widgetPtr->gheight;
			int gWidth = widgetPtr->gwidth;

			gcValues.line_width = lineWidth;
			gc = Tk_GetGC(widgetPtr->tkwin, GCForeground | GCLineWidth, &gcValues);

			XDrawRectangle(widgetPtr->display,
				widgetPtr->bitmap.pixmap, gc,
				col * gWidth + lineWidth / 2,
				row * gHeight + lineWidth / 2,
				gWidth - lineWidth / 2 - 1,
				gHeight - lineWidth / 2 - 1);
		}
		else
		{
			int tile = row * widgetPtr->cc + col;
			int yp = widgetPtr->yp[tile];
			int xp = widgetPtr->xp[tile];

			gc = Tk_GetGC(widgetPtr->tkwin, GCForeground, &gcValues);
#if 0
			points[np].x = xp,					 points[np++].y = yp + ISO_HGT - ISO_FH2 / 2 - 1;
			points[np].x = xp + ISO_WID / 2,	 points[np++].y = yp + ISO_HGT - ISO_FH2 - 1;
			points[np].x = xp + ISO_WID / 2 - 1, points[np++].y = yp + ISO_HGT - ISO_FH2 - 1;
			points[np].x = xp + ISO_WID - 1,	 points[np++].y = yp + ISO_HGT - ISO_FH2 / 2 - 1;
			points[np].x = xp + ISO_WID / 2 - 1, points[np++].y = yp + ISO_HGT - 1;
			points[np].x = xp + ISO_WID / 2,	 points[np++].y = yp + ISO_HGT - 1;
			points[np++] = points[0];
#endif
			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp, yp + ISO_HGT - ISO_FH2 / 2 - 1,
				xp + ISO_WID / 2, yp + ISO_HGT - ISO_FH2 - 1);

			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp + ISO_WID / 2 - 1, yp + ISO_HGT - ISO_FH2 - 1,
				xp + ISO_WID - 1, yp + ISO_HGT - ISO_FH2 / 2 - 1);

			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp + ISO_WID - 1, yp + ISO_HGT - ISO_FH2 / 2 - 1,
				xp + ISO_WID / 2, yp + ISO_HGT - 1);

			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp + ISO_WID / 2, yp + ISO_HGT - 1,
				xp, yp + ISO_HGT - ISO_FH2 / 2 - 1);

			if (lineWidth == 2)
			{
				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp+2, yp + ISO_HGT - ISO_FH2 / 2 - 1,
					xp + ISO_WID / 2, yp+1 + ISO_HGT - ISO_FH2 - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp + ISO_WID / 2 - 1, yp+1 + ISO_HGT - ISO_FH2 - 1,
					xp-2 + ISO_WID - 1, yp + ISO_HGT - ISO_FH2 / 2 - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp-2 + ISO_WID - 1, yp + ISO_HGT - ISO_FH2 / 2 - 1,
					xp + ISO_WID / 2, yp-1 + ISO_HGT - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp + ISO_WID / 2, yp-1 + ISO_HGT - 1,
					xp+2, yp + ISO_HGT - ISO_FH2 / 2 - 1);
			}
		}

		Tk_FreeGC(widgetPtr->display, gc);

		cursorPtr->dirty[0] = cursorPtr->row;
		cursorPtr->dirty[1] = cursorPtr->col;
		cursorPtr->hasDirty = TRUE;
	}
	else
	{
		itemPtr->minY = 0, itemPtr->maxY = -1;
		itemPtr->minX = 0, itemPtr->maxX = -1;
	}

	return TCL_OK;
}

/*
 * Calculates the row and column in the parent Widget of the cursor.
 * Return 1 if the row/column is in bounds, 0 otherwise.
 */
static int CalcCursorPosition(Widget *widgetPtr, CursorItem *cursorPtr)
{
	WidgetItem *itemPtr = (WidgetItem *) cursorPtr;
	int row, col;

	if (!Widget_CaveToView(widgetPtr, cursorPtr->y, cursorPtr->x, &row, &col))
		return 0;

	cursorPtr->row = row;
	cursorPtr->col = col;
	itemPtr->minX = itemPtr->maxX = col;
	itemPtr->minY = itemPtr->maxY = row;

	if (widgetPtr->style == WIDGET_STYLE_ISO)
	{
		int tile = row * widgetPtr->cc + col;
		int yp = widgetPtr->yp[tile];
		int xp = widgetPtr->xp[tile];

		itemPtr->x1 = xp + ISO_LAPX - widgetPtr->bx;
		itemPtr->y1 = yp + ISO_HGT - ISO_FH - widgetPtr->by;
		itemPtr->x2 = itemPtr->x1 + ISO_WID;
		itemPtr->y2 = itemPtr->y1 + ISO_HGT;
	}
	else
	{
		int gHeight = widgetPtr->gheight;
		int gWidth = widgetPtr->gwidth;

		itemPtr->x1 = col * gWidth - widgetPtr->bx;
		itemPtr->x2 = itemPtr->x1 + gWidth;
		itemPtr->y1 = row * gHeight - widgetPtr->by;
		itemPtr->y2 = itemPtr->y1 + gHeight;
	}

	return 1;
}

#define RECT_BOUNDS 0x0001

/*
 * The structure below defines the record for each cursor item.
 */
typedef struct RectItem {
	WidgetItem header; /* Required header info */
	int x1, y1, x2, y2; /* Cave coordinates */
	int width, height; /* Columns and rows */
	int x_min, x_max, y_min, y_max; /* Widget row/col */
	XColor *color; /* Color */
	int lineWidth; /* Width of rectangle */
	int dirty[4], hasDirty; /* Previous displayed top,left,bottom,right */
	int outside; /* Draw rect around grids not inside */
} RectItem;

static Tk_OptionSpec optionSpecRect[] = {
    {TK_OPTION_BOOLEAN, "-visible", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(WidgetItem, visible), 0, 0, 0},
    {TK_OPTION_PIXELS, "-x1", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(RectItem, x1), 0, 0, RECT_BOUNDS},
    {TK_OPTION_PIXELS, "-y1", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(RectItem, y1), 0, 0, RECT_BOUNDS},
    {TK_OPTION_PIXELS, "-x2", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(RectItem, x2), 0, 0, RECT_BOUNDS},
    {TK_OPTION_PIXELS, "-y2", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(RectItem, y2), 0, 0, RECT_BOUNDS},
    {TK_OPTION_COLOR, "-color", (char *) NULL, (char *) NULL,
     "yellow", -1, Tk_Offset(RectItem, color), 0, 0, 0},
    {TK_OPTION_INT, "-linewidth", (char *) NULL, (char *) NULL,
     "2", -1, Tk_Offset(RectItem, lineWidth), 0, 0, 0},
    {TK_OPTION_BOOLEAN, "-outside", (char *) NULL, (char *) NULL,
     "0", -1, Tk_Offset(RectItem, outside), 0, 0, 0},
    {TK_OPTION_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, -1, 0, 0, 0}
};

static int CreateRect _ANSI_ARGS_((Tcl_Interp *interp,
	Widget *widgetPtr, WidgetItem *itemPtr,
	int objc, Tcl_Obj *CONST objv[]));
static int ConfigureRect _ANSI_ARGS_((Tcl_Interp *interp,
	Widget *widgetPtr, WidgetItem *itemPtr,
	int objc, Tcl_Obj *CONST objv[]));
static int DisplayRect _ANSI_ARGS_((Tcl_Interp *interp,
	Widget *widgetPtr, WidgetItem *itemPtr));
static void DeleteRect _ANSI_ARGS_((Widget *widgetPtr,
	WidgetItem *itemPtr));
static int CalcRectPosition(Widget *widgetPtr, RectItem *rectPtr);

WidgetItemType RectType = {
	"rectangle",
	sizeof(RectItem),
	optionSpecRect,
	NULL, /* optionTable */
	CreateRect,
	ConfigureRect,
	DisplayRect,
	NULL,
	DeleteRect,
	(WidgetItemType *) NULL
};

/*
 * Item creation callback
 */
static int CreateRect(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
	RectItem *rectPtr = (RectItem *) itemPtr;

	rectPtr->header.visible = FALSE;
	rectPtr->x1 = 0;
	rectPtr->y1 = 0;
	rectPtr->x2 = 0;
	rectPtr->y2 = 0;
	rectPtr->width = 0;
	rectPtr->height = 0;
	rectPtr->color = NULL;
	rectPtr->lineWidth = 2;
	rectPtr->hasDirty = FALSE;

	/* Set the default options for the new widget */
	if (Tk_InitOptions(interp, (char *) itemPtr,
		itemPtr->typePtr->optionTable, widgetPtr->tkwin) != TCL_OK)
	{
		DeleteRect(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	if (ConfigureRect(interp, widgetPtr, itemPtr, objc, objv) != TCL_OK)
	{
		DeleteRect(widgetPtr, itemPtr);
		return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * Item configuration callback
 */
static int ConfigureRect(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr, int objc, Tcl_Obj *CONST objv[])
{
	Tk_SavedOptions savedOptions;
	Tcl_Obj *errorResult = NULL;
	int error, mask = 0;
	int wasVisible = itemPtr->visible;
	RectItem *rectPtr = (RectItem *) itemPtr;
	int redisplay = 1;

	if (debug_widgets & DEBUG_WIDGET_ITEM) dbwin("ConfigureRect\n");

	/*
	 * The following loop is potentially executed twice.  During the
	 * first pass configuration options get set to their new values.
	 * If there is an error in this pass, we execute a second pass
	 * to restore all the options to their previous values.
	 */
	for (error = 0; error <= 1; error++)
	{
		if (!error)
		{
			/*
			 * First pass: set options to new values.
			 */
			if (Tk_SetOptions(interp, (char *) itemPtr,
				itemPtr->typePtr->optionTable, objc, objv,
				widgetPtr->tkwin, &savedOptions, &mask) != TCL_OK)
			{
				continue;
			}
		}
		else
		{
			/*
			 * Second pass: restore options to old values.
			 */
			errorResult = Tcl_GetObjResult(interp);
			Tcl_IncrRefCount(errorResult);
			Tk_RestoreSavedOptions(&savedOptions);
		}

		if (!CalcRectPosition(widgetPtr, rectPtr))
		{
			itemPtr->minY = 0, itemPtr->maxY = -1;
			itemPtr->minX = 0, itemPtr->maxX = -1;
		}

		rectPtr->height = rectPtr->y2 - rectPtr->y1 + 1;
		rectPtr->width = rectPtr->x2 - rectPtr->x1 + 1;

		break;
	}

	if (error)
	{
		Tcl_SetObjResult(interp, errorResult);
		Tcl_DecrRefCount(errorResult);

		/* Failure */
		return TCL_ERROR;
	}

	Tk_FreeSavedOptions(&savedOptions);

	if (redisplay && (itemPtr->visible || wasVisible))
	{
		if (widgetPtr->style == WIDGET_STYLE_ISO)
		{
			Widget_Wipe(widgetPtr);
		}
		else
		{
			if (rectPtr->hasDirty &&
				((rectPtr->dirty[0] != itemPtr->minY) ||
				(rectPtr->dirty[1] != itemPtr->minX) ||
				(rectPtr->dirty[2] != itemPtr->maxY) ||
				(rectPtr->dirty[3] != itemPtr->maxX)))
			{
				Widget_InvalidateArea(widgetPtr,
					rectPtr->dirty[0], rectPtr->dirty[1],
					rectPtr->dirty[2], rectPtr->dirty[3]);
			}
			rectPtr->hasDirty = FALSE;
			Widget_InvalidateArea(widgetPtr, itemPtr->minY, itemPtr->minX,
				itemPtr->maxY, itemPtr->maxX);
			widgetPtr->flags |= WIDGET_DRAW_INVALID;
			Widget_EventuallyRedraw(widgetPtr);
		}
	}

	/* Success */
	return TCL_OK;
}

/*
 * Item deletion callback
 */
static void DeleteRect(Widget *widgetPtr, WidgetItem *itemPtr)
{
}

/*
 * Item display callback
 */
static int DisplayRect(Tcl_Interp *interp, Widget *widgetPtr,
	WidgetItem *itemPtr)
{
	RectItem *rectPtr = (RectItem *) itemPtr;
	XGCValues gcValues;
	GC gc;

	if (CalcRectPosition(widgetPtr, rectPtr))
	{
		int lineWidth = rectPtr->lineWidth;

		gcValues.foreground = rectPtr->color->pixel;

		if (widgetPtr->style != WIDGET_STYLE_ISO)
		{
			int gHeight = widgetPtr->gheight;
			int gWidth = widgetPtr->gwidth;

			gcValues.line_width = lineWidth;
			gc = Tk_GetGC(widgetPtr->tkwin, GCForeground | GCLineWidth, &gcValues);

			if (rectPtr->outside)
				XDrawRectangle(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					rectPtr->x_min * gWidth - (lineWidth + 1) / 2,
					rectPtr->y_min * gHeight - (lineWidth + 1) / 2,
					rectPtr->width * gWidth + lineWidth,
					rectPtr->height * gHeight + lineWidth);
			else
				XDrawRectangle(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					rectPtr->x_min * gWidth + lineWidth / 2,
					rectPtr->y_min * gHeight + lineWidth / 2,
					rectPtr->width * gWidth - lineWidth,
					rectPtr->height * gHeight - lineWidth);
		}
		else
		{
			int cy, cx, cyp, cxp;
			int yp[4], xp[4], y, x;
			XPoint points[10];
			int np = 0;

			gc = Tk_GetGC(widgetPtr->tkwin, GCForeground, &gcValues);
#if 1
			cy = widgetPtr->y; cx = widgetPtr->x;
			cyp = widgetPtr->yp[widgetPtr->centerTile];
			cxp = widgetPtr->xp[widgetPtr->centerTile];

			y = rectPtr->y1; x = rectPtr->x1;
			yp[0] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[0] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;

			y = rectPtr->y1; x = rectPtr->x2;
			yp[1] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[1] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;

			y = rectPtr->y2; x = rectPtr->x2;
			yp[2] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[2] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;

			y = rectPtr->y2; x = rectPtr->x1;
			yp[3] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[3] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;

#if 1
			while (lineWidth-- > 0)
			{
				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[3]+lineWidth*2, yp[3] + ISO_HGT - ISO_FH2 / 2 - 1,
					xp[0] + ISO_WID / 2, yp[0]+lineWidth*1 + ISO_HGT - ISO_FH2 - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[0] + ISO_WID / 2 - 1, yp[0]+lineWidth*1 + ISO_HGT - ISO_FH2 - 1,
					xp[1]-lineWidth*2 + ISO_WID - 1, yp[1] + ISO_HGT - ISO_FH2 / 2 - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[1]-lineWidth*2 + ISO_WID - 1, yp[1] + ISO_HGT - ISO_FH2 / 2 - 1,
					xp[2] + ISO_WID / 2, yp[2]-lineWidth*1 + ISO_HGT - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[2] + ISO_WID / 2, yp[2]-lineWidth*1 + ISO_HGT - 1,
					xp[3]+lineWidth*2, yp[3] + ISO_HGT - ISO_FH2 / 2 - 1);
			}
#else
			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp[3], yp[3] + ISO_HGT - ISO_FH2 / 2 - 1,
				xp[0] + ISO_WID / 2, yp[0] + ISO_HGT - ISO_FH2 - 1);

			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp[0] + ISO_WID / 2 - 1, yp[0] + ISO_HGT - ISO_FH2 - 1,
				xp[1] + ISO_WID - 1, yp[1] + ISO_HGT - ISO_FH2 / 2 - 1);

			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp[1] + ISO_WID - 1, yp[1] + ISO_HGT - ISO_FH2 / 2 - 1,
				xp[2] + ISO_WID / 2, yp[2] + ISO_HGT - 1);

			XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
				xp[2] + ISO_WID / 2, yp[2] + ISO_HGT - 1,
				xp[3], yp[3] + ISO_HGT - ISO_FH2 / 2 - 1);

			if (lineWidth == 2)
			{
				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[3]+2, yp[3] + ISO_HGT - ISO_FH2 / 2 - 1,
					xp[0] + ISO_WID / 2, yp[0]+1 + ISO_HGT - ISO_FH2 - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[0] + ISO_WID / 2 - 1, yp[0]+1 + ISO_HGT - ISO_FH2 - 1,
					xp[1]-2 + ISO_WID - 1, yp[1] + ISO_HGT - ISO_FH2 / 2 - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[1]-2 + ISO_WID - 1, yp[1] + ISO_HGT - ISO_FH2 / 2 - 1,
					xp[2] + ISO_WID / 2, yp[2]-1 + ISO_HGT - 1);

				XDrawLine(widgetPtr->display, widgetPtr->bitmap.pixmap, gc,
					xp[2] + ISO_WID / 2, yp[2]-1 + ISO_HGT - 1,
					xp[3]+2, yp[3] + ISO_HGT - ISO_FH2 / 2 - 1);
			}
#endif
#else

			cy = widgetPtr->y; cx = widgetPtr->x;
			cyp = widgetPtr->yp[widgetPtr->centerTile] + (ISO_HGT - ISO_FH);
			cxp = widgetPtr->xp[widgetPtr->centerTile]/* + ISO_LAPX */;

			y = rectPtr->y1; x = rectPtr->x1;
			yp[0] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[0] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;
			xp[0] += ISO_WID2 / 2;

			y = rectPtr->y1; x = rectPtr->x2;
			yp[1] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[1] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;
			yp[1] += ISO_FH2 / 2;
			xp[1] += ISO_WID2;

			y = rectPtr->y2; x = rectPtr->x2;
			yp[2] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[2] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;
			yp[2] += ISO_FH2;
			xp[2] += ISO_WID2 / 2;

			y = rectPtr->y2; x = rectPtr->x1;
			yp[3] = cyp + (y - cy) * ISO_FH2 / 2 + (x - cx) * ISO_FH2 / 2;
			xp[3] = cxp + (x - cx) * ISO_WID2 / 2 - (y - cy) * ISO_WID2 / 2;
			yp[3] += ISO_FH2 / 2;

			points[np].x = xp[0], points[np++].y = yp[0];
			points[np].x = xp[1], points[np++].y = yp[1];
			points[np].x = xp[2], points[np++].y = yp[2];
			points[np].x = xp[3], points[np++].y = yp[3];
			points[np++] = points[0];

			XDrawLines(widgetPtr->display,
				widgetPtr->bitmap.pixmap, gc,
				points, np, CoordModeOrigin);
#endif
		}

		Tk_FreeGC(widgetPtr->display, gc);

		rectPtr->dirty[0] = itemPtr->minY;
		rectPtr->dirty[1] = itemPtr->minX;
		rectPtr->dirty[2] = itemPtr->maxY;
		rectPtr->dirty[3] = itemPtr->maxX;
		rectPtr->hasDirty = TRUE;
	}

	/* The rectangle is completely offscreen, so no grids are affected */
	else
	{
		itemPtr->minY = 0, itemPtr->maxY = -1;
		itemPtr->minX = 0, itemPtr->maxX = -1;
	}

	return TCL_OK;
}

static int CalcRectPosition(Widget *widgetPtr, RectItem *rectPtr)
{
	WidgetItem *itemPtr = (WidgetItem *) rectPtr;

	if (widgetPtr->style != WIDGET_STYLE_ISO)
	{
		int x_min, x_max, y_min, y_max;

		y_min = widgetPtr->y_min, y_max = widgetPtr->y_max;
		x_min = widgetPtr->x_min, x_max = widgetPtr->x_max;

		rectPtr->x_min = rectPtr->x1 - x_min;
		rectPtr->y_min = rectPtr->y1 - y_min;
		rectPtr->x_max = rectPtr->x_min + rectPtr->width - 1;
		rectPtr->y_max = rectPtr->y_min + rectPtr->height - 1;

		if ((rectPtr->x2 < x_min) || (rectPtr->x1 >= x_max)) return 0;
		if ((rectPtr->y2 < y_min) || (rectPtr->y1 >= y_max)) return 0;

		itemPtr->minX = MAX(rectPtr->x_min, 0);
		itemPtr->maxX = MIN(rectPtr->x_max, widgetPtr->cc - 1);
		itemPtr->minY = MAX(rectPtr->y_min, 0);
		itemPtr->maxY = MIN(rectPtr->y_max, widgetPtr->rc - 1);
/*dbwin("y,x %d %d y,x %d %d\n", itemPtr->minY, itemPtr->minX, itemPtr->maxY, itemPtr->maxX);*/

		itemPtr->x1 = itemPtr->minX * widgetPtr->gwidth - widgetPtr->bx;
		itemPtr->x2 = (itemPtr->maxX + 1) * widgetPtr->gwidth - widgetPtr->bx;
		itemPtr->y1 = itemPtr->minY * widgetPtr->gheight - widgetPtr->by;
		itemPtr->y2 = (itemPtr->maxY + 1) * widgetPtr->gheight - widgetPtr->by;

		if (rectPtr->outside)
		{
			itemPtr->minX--;
			itemPtr->minY--;
			itemPtr->maxX++;
			itemPtr->maxY++;

			itemPtr->x1 -= rectPtr->lineWidth;
			itemPtr->y1 -= rectPtr->lineWidth;
			itemPtr->x2 += rectPtr->lineWidth;
			itemPtr->y2 += rectPtr->lineWidth;
		}
	}
	else
	{
#if 1
		itemPtr->minX = 0;
		itemPtr->maxX = widgetPtr->cc - 1;
		itemPtr->minY = 0;
		itemPtr->maxY = widgetPtr->rc - 1;

		/* Nov 1 2004 */
		itemPtr->x1 = widgetPtr->bx;
		itemPtr->x2 = widgetPtr->bx + widgetPtr->width;
		itemPtr->y1 = widgetPtr->by;
		itemPtr->y2 = widgetPtr->by + widgetPtr->height;
#else
		int cy = widgetPtr->y;
		int cx = widgetPtr->x;
		int ct = widgetPtr->centerTile;
		int cc = widgetPtr->cc;
		int row = ct / cc, col = ct % cc;

		itemPtr->minY = row + (rectPtr->y1 - cy) + (rectPtr->x1 - cx);
		itemPtr->minX = col - (rectPtr->y2 - cy) + (rectPtr->x1 - cx);

		itemPtr->maxY = row + (rectPtr->y2 - cy) + (rectPtr->x2 - cx);
		itemPtr->maxX = col - (rectPtr->y1 - cy) + (rectPtr->x2 - cx);
dbwin("y,x %d %d y,x %d %d\n", itemPtr->minY, itemPtr->minX, itemPtr->maxY, itemPtr->maxX);
#endif
	}

	return 1;
}



