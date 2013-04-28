/* File: map.c */

/* Purpose: micro-map backend for Widget */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tcl.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "cmdinfo-dll.h"
#include "util-dll.h"
#include "icon.h"
#include "widget.h"
#include "map-dll.h"

#if defined(KANGBANDTK) || defined(ZANGBANDTK)

#define USE_MAP_PET /* Friends and pets use different color */

#endif /* KANBANDTK, ZANGBANDTK */

/* Symbol for each cave grid */
byte *g_map_symbol[DUNGEON_HGT];

#define SYMBOL_ASSIGN_CHARACTER 0
#define SYMBOL_ASSIGN_FEATURE 1
#define SYMBOL_ASSIGN_MONSTER 2
#define SYMBOL_ASSIGN_OBJECT 3
#define SYMBOL_ASSIGN_MAX 4

typedef struct t_symbol_assign t_symbol_assign;

struct t_symbol_assign
{
	int count;
	int *assign;
};

t_symbol_assign g_symbol_assign[SYMBOL_ASSIGN_MAX];

#define SYMBOL_SPECIAL_BLANK 0
#define SYMBOL_SPECIAL_PET 1
#define SYMBOL_SPECIAL_MAX 2
int g_symbol_special[SYMBOL_SPECIAL_MAX];

int map_symbol_find(Tcl_Interp *interp, Tcl_Obj *objName, int *symbolIndex)
{
	return symbol_find(interp, objName, NULL, NULL, symbolIndex);
}

char *map_symbol_name(int symbolIndex)
{
	return g_symbol[symbolIndex]->name;
}

int map_symbol_feature(int f_idx)
{
	return g_symbol_assign[SYMBOL_ASSIGN_FEATURE].assign[f_idx];
}

/* special pet|blank ?symbol? */
int objcmd_symbol_special(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	static CONST char *option[] = {"blank", "pet", NULL};
	int special, symbol;

	if (Tcl_GetIndexFromObj(interp, objV[1], option,
		"special", 0, &special) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (objC == 2)
	{
		symbol = g_symbol_special[special];
		StaticResult(interp, g_symbol[symbol]->name);
		return TCL_OK;
	}

	if (symbol_find(interp, objV[2], NULL, NULL, &symbol) != TCL_OK)
	{
		return TCL_ERROR;
	}

	g_symbol_special[special] = symbol;

	return TCL_OK;
}

/*
 * assign $group $member ?$symbol?
 */
int objcmd_symbol_assign(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	static CONST char *optionAssign[] = {"character", "feature", "monster",
		"object", NULL};
	int group, member;
	int symbol;

	if (Tcl_GetIndexFromObj(interp, objV[1], optionAssign,
		"group", 0, &group) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIntFromObj(interp, objV[2], &member) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if ((member < 0) || (member >= g_symbol_assign[group].count))
	{
		Tcl_SetObjResult(interp,
			Tcl_NewStringObj(format("bad member \"%d\" : "
			"must be from 0 to %d", member,
			g_symbol_assign[group].count - 1), -1));
		return TCL_ERROR;
	}

	/* Return the current assignment */
	if (objC == 3)
	{
		symbol = g_symbol_assign[group].assign[member];
		StaticResult(interp, g_symbol[symbol]->name);
		return TCL_OK;
	}

	if (symbol_find(interp, objV[3], NULL, NULL, &symbol) != TCL_OK)
	{
		return TCL_ERROR;
	}
	g_symbol_assign[group].assign[member] = symbol;

	return TCL_OK;
}

/*
 * Determine the symbol for given cave location
 */
void map_symbol_set(int y, int x)
{
	int m_idx, o_idx, f_idx;
	t_grid *gridPtr = &g_grid[y][x];
	int symbol;

	m_idx = gridPtr->m_idx;
	o_idx = gridPtr->o_idx;
	f_idx = gridPtr->f_idx;

	/* Character */
	if (m_idx == -1)
	{
		symbol = g_symbol_assign[SYMBOL_ASSIGN_CHARACTER].assign[0]; 
	}

	/* Monster */
	else if (m_idx > 0)
	{
		monster_type *m_ptr = &m_list[m_idx];
#ifdef USE_MAP_PET
		if (monster_is_friend(m_ptr))
			symbol = g_symbol_special[SYMBOL_SPECIAL_PET];
		else
#endif /* USE_MAP_PET */
			symbol = g_symbol_assign[SYMBOL_ASSIGN_MONSTER].assign[m_ptr->r_idx];
	}

	/* Object */
	else if (o_idx)
	{
		object_type *o_ptr = &o_list[o_idx];
		symbol = g_symbol_assign[SYMBOL_ASSIGN_OBJECT].assign[o_ptr->k_idx];
	}

	/* Feature */
	else if (f_idx)
	{
		symbol = g_symbol_assign[SYMBOL_ASSIGN_FEATURE].assign[f_idx];
	}

	/* Blank */
	else
	{
		symbol = g_symbol_special[SYMBOL_SPECIAL_BLANK];
	}

	g_map_symbol[y][x] = symbol;
}

#if 0

/* Is this ever called? */
void Widget_DrawMap(Widget *widgetPtr, int y, int x)
{
	DrawSymbolProc symbolProc;
	IconPtr *tilePtr;
	long *srcPtr, *dstPtr, pitch;
	int night, symbol, size = widgetPtr->gwidth;

	symbolProc = symbolProcTable[g_pixel_size - 1][size - 4];
	tilePtr = g_bits[size - 4];

	night = (p_ptr_depth || !g_daytime);

	if (in_bounds_test(widgetPtr->y_min + y, widgetPtr->x_min + x))
	{
		symbol = g_map_symbol[widgetPtr->y_min + y][widgetPtr->x_min + x];
#if USE_MAP_MIMIC
		symbol = g_symbol[symbol]->mimic;
#endif
		if (night && g_symbol[symbol]->light)
			symbol += g_grid[y][x].dark;
	}
	else
	{
		symbol = g_symbol_assign[SYMBOL_ASSIGN_FEATURE].assign[0];
	}

	pitch = widgetPtr->bitmap.pitch;
	srcPtr = (long *) (long *) tilePtr[symbol];
	dstPtr = (long *) (widgetPtr->bitmap.pixelPtr + x * size * g_pixel_size +
		y * size * pitch);

	(*symbolProc)(srcPtr, dstPtr, pitch);
}

#endif /* 0 */

#define INCR(p,d) \
	p = (void *) ((byte *) p + (d));

/* ExWidget.symbolProc() */
int map_symbol_proc(Widget *widgetPtr, int y, int x)
{
	int night = !is_daytime();
	int symbol = -1;

	if (in_bounds_test(y, x))
	{
		symbol = g_map_symbol[y][x];
#if USE_MAP_MIMIC
		symbol = g_symbol[symbol]->mimic;
#endif
		if (night && g_symbol[symbol]->light)
			symbol += g_grid[y][x].dark;
	}

	return symbol;
}

MapBlinker *MapBlinker_Alloc(void)
{
	return (MapBlinker *)Tcl_AllocDebug(sizeof(MapBlinker));
}

void MapBlinker_Free(MapBlinker *blinkPtr)
{
	Tcl_FreeDebug(blinkPtr);
}

void map_add_blinker(ExWidget *widgetPtr, int type, int x, int y)
{
	MapBlinker *blinkPtr = widgetPtr->blinkPtr;

	while (blinkPtr != NULL)
	{
		if (blinkPtr->type == BLINK_NONE)
			break;
		blinkPtr = blinkPtr->next;
	}
	if (blinkPtr == NULL)
	{
		blinkPtr = MapBlinker_Alloc();
		blinkPtr->next = widgetPtr->blinkPtr;
		widgetPtr->blinkPtr = blinkPtr;
	}
	blinkPtr->type = type;
	blinkPtr->x = x;
	blinkPtr->y = y;
}

static int g_blink_radius = 1;

void map_draw_blinker(Widget *widgetPtr, MapBlinker *blinkPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int ymin = widgetPtr->y_min;
	int xmin = widgetPtr->x_min;
	int r = abs(g_blink_radius);
	int x = blinkPtr->x;
	int y = blinkPtr->y;
	int x2, y2;
	GC gc = None;

	x2 = /*widgetPtr->bx +*/ (x - r - xmin) * widgetPtr->gwidth + widgetPtr->gwidth / 2;
	y2 = /*widgetPtr->by +*/ (y - r - ymin) * widgetPtr->gheight + widgetPtr->gheight / 2;

	/* Get a graphics context for drawing */
	if (blinkPtr->type == BLINK_UP)
		gc = Tk_GCForColor(exPtr->blink1ColorPtr, widgetPtr->bitmap.pixmap);
	if (blinkPtr->type == BLINK_DOWN)
		gc = Tk_GCForColor(exPtr->blink2ColorPtr, widgetPtr->bitmap.pixmap);
	if (blinkPtr->type == BLINK_PLAYER)
		gc = Tk_GCForColor(exPtr->blink3ColorPtr, widgetPtr->bitmap.pixmap);
	if (gc == None)
		return;

	XDrawArc(widgetPtr->display, widgetPtr->bitmap.pixmap, gc, x2, y2,
		r * 2 * widgetPtr->gwidth - 1, r * 2 * widgetPtr->gheight - 1, 0, 360 * 64);
}

void map_update_blinkers(void)
{
	static unsigned long last_update = 0;
	static unsigned long last_call = 0;
	unsigned long tick_count, ticks;
	DoubleLink *link;

	/* Get the system tick count */
	tick_count = Milliseconds();

	/* Don't update twice the same tick */
	if (tick_count == last_call) return;

	/* Remember the tick count */
	last_call = tick_count;

	/* Calculate the elapsed ticks */
	ticks = MIN(tick_count - last_update, 1000);

	/* Not enough time has passed */
	if (ticks < 200) return;

	/* Remember the tick count */
	last_update = tick_count;

	if (g_blink_radius > 0)
	{
		if (++g_blink_radius > 3)
			g_blink_radius = -2;
	}
	else
	{
		if (++g_blink_radius > -1)
			g_blink_radius = 2;
	}

	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);
		MapBlinker *blinkPtr = ((ExWidget *) widgetPtr)->blinkPtr;

		while (blinkPtr != NULL)
		{
			if (blinkPtr->type != BLINK_NONE)
			{
				int row = blinkPtr->y - widgetPtr->y_min;
				int col = blinkPtr->x - widgetPtr->x_min;
				Widget_InvalidateArea(widgetPtr,
					MAX(row - 3,0), MAX(col - 3,0),
					MIN(row + 3,widgetPtr->rc-1), MIN(col + 3,widgetPtr->cc-1));
				widgetPtr->flags |= WIDGET_DRAW_INVALID;
				Widget_EventuallyRedraw(widgetPtr);
			}
			blinkPtr = blinkPtr->next;
		}
	}
}

static int is_stair_up(int feat)
{
	if (feat == FEAT_LESS) return 1;
#if defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (feat == FEAT_QUEST_EXIT) return 1;
	if (feat == FEAT_QUEST_UP) return 1;
#endif
	return 0;
}

static int is_stair_down(int feat)
{
	if (feat == FEAT_MORE) return 1;
#if defined(KANGBANDTK) || defined(ZANGBANDTK)
	if (feat == FEAT_QUEST_ENTER) return 1;
	if (feat == FEAT_QUEST_DOWN) return 1;
#endif
	return 0;
}

/* Widget.drawProc() */
void map_draw_all(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int y, x, y_min, y_max, x_min, x_max;
	DrawSymbolProc symbolProc;
	IconPtr *tilePtr;
	long *srcPtr, *dstPtr, *rowPtr, pitch;
	int size = widgetPtr->gwidth;
	int symbol, symbol_blank;
	DoubleLink *link;
MapBlinker *blinkPtr = exPtr->blinkPtr;

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("map_draw_all\n");

	/* Drawing is disabled */
	if (widgetPtr->flags & WIDGET_NO_UPDATE) return;

while (blinkPtr != NULL)
{
	blinkPtr->type = BLINK_NONE;
	blinkPtr = blinkPtr->next;
}

	y_min = widgetPtr->y_min;
	y_max = widgetPtr->y_max;

	x_min = widgetPtr->x_min;
	x_max = widgetPtr->x_max;

	symbolProc = symbolProcTable[g_pixel_size - 1][size - 4];
	tilePtr = g_bits[size - 4];

	symbol_blank = g_symbol_special[SYMBOL_SPECIAL_BLANK];

	rowPtr = (long *) widgetPtr->bitmap.pixelPtr;
	pitch = widgetPtr->bitmap.pitch;

	for (y = y_min; y < y_max; y++)
	{
		dstPtr = rowPtr;
		for (x = x_min; x < x_max; x++)
		{
			symbol = (*exPtr->symbolProc)(widgetPtr, y, x);
			if (symbol < 0)
				symbol = symbol_blank;
if (exPtr->blink && (symbol != symbol_blank))
{
	t_grid *gridPtr = &g_grid[y][x];
	if (gridPtr->m_idx == -1)
		map_add_blinker(exPtr, BLINK_PLAYER, x, y);
	else if (is_stair_up(gridPtr->f_idx))
		map_add_blinker(exPtr, BLINK_UP, x, y);
	else if (is_stair_down(gridPtr->f_idx))
		map_add_blinker(exPtr, BLINK_DOWN, x, y);
}
			srcPtr = (long *) tilePtr[symbol];
			(*symbolProc)(srcPtr, dstPtr, pitch);
			INCR(dstPtr, size * g_pixel_size)
		}
		INCR(rowPtr, size * pitch)
	}

	/* Nothing is invalid */
	widgetPtr->dirty[0] = widgetPtr->cc - 1;
	widgetPtr->dirty[1] = widgetPtr->rc - 1;
	widgetPtr->dirty[2] = 0;
	widgetPtr->dirty[3] = 0;

	/* Now draw all of the items for this Widget */
	for (link = widgetPtr->linkerItemVis.head; link; link = link->next)
	{
		WidgetItem *itemPtr = DoubleLink_Data(link, WidgetItem);

		(*itemPtr->typePtr->displayProc)(widgetPtr->interp, widgetPtr,
			itemPtr);

		/* Invalidate the grids covered by the item */
		Widget_InvalidateArea(widgetPtr, itemPtr->minY, itemPtr->minX,
			itemPtr->maxY, itemPtr->maxX);
	}

blinkPtr = exPtr->blinkPtr;
while (blinkPtr != NULL)
{
	if (blinkPtr->type != BLINK_NONE)
	{
//		int y = blinkPtr->y - widgetPtr->y_min;
//		int x = blinkPtr->x - widgetPtr->x_min;
		map_draw_blinker(widgetPtr, blinkPtr);
//		Widget_InvalidateArea(widgetPtr, y - 3, x - 3,
//			y + 3, x + 3);
//		widgetPtr->flags |= WIDGET_DRAW_INVALID;
	}
	blinkPtr = blinkPtr->next;
}

	/* Set dirty bounds to entire window */
	widgetPtr->dx = widgetPtr->bx;
	widgetPtr->dy = widgetPtr->by;
	widgetPtr->dw = widgetPtr->width;
	widgetPtr->dh = widgetPtr->height;
}

/* Widget.invalidProc() */
void map_draw_invalid(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int y, x, y_min, y_max, x_min, x_max;
	DrawSymbolProc symbolProc;
	IconPtr *tilePtr;
	long *srcPtr, *dstPtr, *rowPtr, pitch;
	int size = widgetPtr->gwidth;
	int symbol, symbol_blank;
	DoubleLink *link;
MapBlinker *blinkPtr = exPtr->blinkPtr;

	y_min = widgetPtr->dirty[1];
	y_max = widgetPtr->dirty[3];

	x_min = widgetPtr->dirty[0];
	x_max = widgetPtr->dirty[2];

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("map_draw_invalid %d %d %d %d\n", y_min, x_min, y_max, x_max);

	/* Drawing is disabled */
	if (widgetPtr->flags & WIDGET_NO_UPDATE) return;

while (blinkPtr != NULL)
{
	if (blinkPtr->y >= y_min && blinkPtr->y <= y_max &&
		blinkPtr->x >= x_min && blinkPtr->x <= x_max)
		blinkPtr->type = BLINK_NONE;
	blinkPtr = blinkPtr->next;
}

	symbolProc = symbolProcTable[g_pixel_size - 1][size - 4];
	tilePtr = g_bits[size - 4];

	symbol_blank = g_symbol_special[SYMBOL_SPECIAL_BLANK];

	pitch = widgetPtr->bitmap.pitch;
	rowPtr = (long *) (widgetPtr->bitmap.pixelPtr
		+ x_min * size * g_pixel_size
		+ y_min * size * pitch);

	for (y = y_min; y <= y_max; y++)
	{
		dstPtr = rowPtr;
		for (x = x_min; x <= x_max; x++)
		{
			symbol = (*exPtr->symbolProc)(widgetPtr,
				widgetPtr->y_min + y, widgetPtr->x_min + x);
			if (symbol < 0)
				symbol = symbol_blank;
if (exPtr->blink && (symbol != symbol_blank))
{
	int cy = widgetPtr->y_min + y;
	int cx = widgetPtr->x_min + x;
	t_grid *gridPtr = &g_grid[cy][cx];
	if (gridPtr->m_idx == -1)
		map_add_blinker(exPtr, BLINK_PLAYER, cx, cy);
	else if (is_stair_up(gridPtr->f_idx))
		map_add_blinker(exPtr, BLINK_UP, cx, cy);
	else if (is_stair_down(gridPtr->f_idx))
		map_add_blinker(exPtr, BLINK_DOWN, cx, cy);
}
			srcPtr = (long *) tilePtr[symbol];
			(*symbolProc)(srcPtr, dstPtr, pitch);
			INCR(dstPtr, size * g_pixel_size)
		}
		INCR(rowPtr, size * pitch)
	}
	
	/* Nothing is invalid */
	widgetPtr->dirty[0] = widgetPtr->cc - 1;
	widgetPtr->dirty[1] = widgetPtr->rc - 1;
	widgetPtr->dirty[2] = 0;
	widgetPtr->dirty[3] = 0;

	/* Now draw all of the items for this Widget */
	for (link = widgetPtr->linkerItemVis.head; link; link = link->next)
	{
		WidgetItem *itemPtr = DoubleLink_Data(link, WidgetItem);

		(*itemPtr->typePtr->displayProc)(widgetPtr->interp, widgetPtr,
			itemPtr);

		/* Invalidate the grids covered by the item */
		Widget_InvalidateArea(widgetPtr, itemPtr->minY, itemPtr->minX,
			itemPtr->maxY, itemPtr->maxX);
	}

blinkPtr = exPtr->blinkPtr;
while (blinkPtr != NULL)
{
	if (blinkPtr->type != BLINK_NONE)
	{
//		int y = blinkPtr->y - widgetPtr->y_min;
//		int x = blinkPtr->x - widgetPtr->x_min;
		map_draw_blinker(widgetPtr, blinkPtr);
//		Widget_InvalidateArea(widgetPtr, y - 3, x - 3,
//			y + 3, x + 3);
//		widgetPtr->flags |= WIDGET_DRAW_INVALID;
	}
	blinkPtr = blinkPtr->next;
}

	/* Dirty bounds for display */
	widgetPtr->dx = x_min * size;
	widgetPtr->dy = y_min * size;
	widgetPtr->dw = (x_max - x_min + 1) * size;
	widgetPtr->dh = (y_max - y_min + 1) * size;
}

static CommandInit commandInit[] = {
	{0, "symbol", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "assign", 3, 4, "group member ?symbol?", objcmd_symbol_assign, (ClientData) 0},
		{1, "special", 2, 3, "pet|dark ?symbol?", objcmd_symbol_special, (ClientData) 0},
	{0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

static void assign_wipe(t_symbol_assign *groupPtr)
{
	int i;

	for (i = 0; i < groupPtr->count; i++)
	{
		groupPtr->assign[i] = 0;
	}
}

void map_init(void)
{
	int i;

	(void) Map_Init(g_interp);

	g_symbol_assign[SYMBOL_ASSIGN_CHARACTER].count = 1;
	g_symbol_assign[SYMBOL_ASSIGN_CHARACTER].assign =
		(int *) Tcl_Alloc(1 * sizeof(int));
	assign_wipe(&g_symbol_assign[SYMBOL_ASSIGN_CHARACTER]);

	g_symbol_assign[SYMBOL_ASSIGN_FEATURE].count = MAX_F_IDX;
	g_symbol_assign[SYMBOL_ASSIGN_FEATURE].assign =
		(int *) Tcl_Alloc(MAX_F_IDX * sizeof(int));
	assign_wipe(&g_symbol_assign[SYMBOL_ASSIGN_FEATURE]);

	g_symbol_assign[SYMBOL_ASSIGN_MONSTER].count = MAX_R_IDX;
	g_symbol_assign[SYMBOL_ASSIGN_MONSTER].assign =
		(int *) Tcl_Alloc(MAX_R_IDX * sizeof(int));
	assign_wipe(&g_symbol_assign[SYMBOL_ASSIGN_MONSTER]);

	g_symbol_assign[SYMBOL_ASSIGN_OBJECT].count = MAX_K_IDX;
	g_symbol_assign[SYMBOL_ASSIGN_OBJECT].assign =
		(int *) Tcl_Alloc(MAX_K_IDX * sizeof(int));
	assign_wipe(&g_symbol_assign[SYMBOL_ASSIGN_OBJECT]);

	for (i = 0; i < DUNGEON_HGT; i++)
	{
		/* Info about what feature/monster/object is known. */
		C_MAKE(g_map_symbol[i], DUNGEON_WID, byte);
	}

	for (i = 0; i < SYMBOL_SPECIAL_MAX; i++)
	{
		g_symbol_special[i] = 0;
	}

	(void) CommandInfo_Init(g_interp, commandInit, NULL);
}

void map_exit(void)
{
	Map_Exit(g_interp);
}
