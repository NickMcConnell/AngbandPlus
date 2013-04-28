/* File: widget.c */

/* Purpose: Widget back-end stuff */

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
#include "util-dll.h"
#include "plat-dll.h"
#include "icon.h"
#include "widget.h"

extern int g_track_grid_x, g_track_grid_y;

extern int iso_hittest(Widget *widgetPtr, int x, int y, int col, int row, int *xc, int *yc);
extern void iso_wtd(Widget *widgetPtr, int y, int x, t_display *wtd);
extern void iso_draw_all(Widget *widgetPtr);
extern void iso_draw_invalid(Widget *widgetPtr);
extern void map_draw_all(Widget *widgetPtr);
extern void map_draw_invalid(Widget *widgetPtr);
extern int map_symbol_proc(Widget *widgetPtr, int y, int x);
extern void vault_wtd(Widget *widgetPtr, int y, int x, t_display *wtd);
extern int vault_symbol_proc(Widget *widgetPtr, int y, int x);

#ifdef WIDGET_STYLE_TEXT
#define EXWIDGET_CONF_FONT 0x00010000
#endif
#define EXWIDGET_CONF_BLINK 0x00020000

static Tk_OptionSpec extraOptions[] = {
    {TK_OPTION_INT, "-hit", "", "",
     "-1", -1, Tk_Offset(ExWidget, hit), 0, 0, 0},
    {TK_OPTION_INT, "-hitx", "", "",
     "-1", -1, Tk_Offset(ExWidget, hitx), 0, 0, 0},
    {TK_OPTION_INT, "-hity", "", "",
     "-1", -1, Tk_Offset(ExWidget, hity), 0, 0, 0},
    {TK_OPTION_INT, "-vaultnum", "vaultNum", "VaultNum",
     "0", -1, Tk_Offset(ExWidget, vaultNum), 0, 0, 0},
#ifdef PLAYER_HEALTH_BAR
    {TK_OPTION_COLOR, "-plrhlthcolor1", (char *) NULL, (char *) NULL,
     "#CCCCCC", -1, Tk_Offset(ExWidget, healthBar.firstColorPtr), 0, 0, 0},
    {TK_OPTION_COLOR, "-plrhlthcolor2", (char *) NULL, (char *) NULL,
     "#FF0000", -1, Tk_Offset(ExWidget, healthBar.secondColorPtr), 0, 0, 0},
#endif
    {TK_OPTION_BOOLEAN, "-blink", "", "",
     "0", -1, Tk_Offset(ExWidget, blink), 0, 0, 0},
    {TK_OPTION_COLOR, "-blinkdowncolor", (char *) NULL, (char *) NULL,
     "#0080ff", -1, Tk_Offset(ExWidget, blink2ColorPtr), 0, 0, EXWIDGET_CONF_BLINK},
    {TK_OPTION_COLOR, "-blinkplayercolor", (char *) NULL, (char *) NULL,
     "yellow", -1, Tk_Offset(ExWidget, blink3ColorPtr), 0, 0, EXWIDGET_CONF_BLINK},
    {TK_OPTION_COLOR, "-blinkupcolor", (char *) NULL, (char *) NULL,
     "green", -1, Tk_Offset(ExWidget, blink1ColorPtr), 0, 0, EXWIDGET_CONF_BLINK},
#ifdef WIDGET_STYLE_TEXT
    {TK_OPTION_FONT, "-font", (char *) NULL, (char *) NULL,
     "Courier 9", -1, Tk_Offset(ExWidget, tkfont), 0, 0,
     EXWIDGET_CONF_FONT},
#endif
    {TK_OPTION_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, -1, 0, 0, 0}
};

/*
 * Invert a grid in the bitmap of a Widget
 */
void Widget_InvertSpot(Widget *widgetPtr, int row, int col, t_display *wtd)
{
	BitmapPtr bitmapPtr = &widgetPtr->bitmap;
	int bypp = bitmapPtr->pixelSize;
	int pitch = bitmapPtr->pitch;
	IconPtr dstPtr;
	t_icon_type *iconTypePtr;
	int xp, yp, x, y;

#ifdef WIDGET_STYLE_TEXT
	if (widgetPtr->style != WIDGET_STYLE_ISO)
#else
	if (widgetPtr->style == WIDGET_STYLE_ICON)
#endif
	{
		yp = row * widgetPtr->gheight;
		xp = col * widgetPtr->gwidth;
	}
	else
	{
		yp = widgetPtr->yp[row * widgetPtr->cc + col];
		xp = widgetPtr->xp[row * widgetPtr->cc + col] + 10;
	}

	/* The icon is transparent */
	if (!wtd->blank && (wtd->fg.type != ICON_TYPE_NONE) &&
		g_icon_type[wtd->fg.type].rle_data)
	{
		IconPtr rlePtr;
		int col = 0;
		int h, w;
		unsigned char *bounds;

		/* Access the icon info */
		iconTypePtr = &g_icon_type[wtd->fg.type];

		rlePtr = iconTypePtr->rle_data + iconTypePtr->rle_offset[wtd->fg.index];
		bounds = iconTypePtr->rle_bounds + wtd->fg.index * 4;
		w = bounds[2];
		h = bounds[3];
#ifdef WIDGET_STYLE_TEXT
		if (widgetPtr->style != WIDGET_STYLE_ISO)
#else
		if (widgetPtr->style == WIDGET_STYLE_ICON)
#endif
		{
			xp += bounds[0];
			yp += bounds[1];
		}
		else
		{
			xp += bounds[0];
			yp += ISO_HGT - ISO_BOTTOM - h;
		}
		dstPtr = bitmapPtr->pixelPtr + xp * bypp + yp * pitch;

		while (1)
		{
			unsigned int trans, opaq;

			trans = rlePtr[0];
			opaq = rlePtr[1];
			rlePtr += 2;
			col += trans;

			if (opaq)
			{
				for (x = 0; x < opaq * bypp; x++)
					*(dstPtr + col * bypp + x) = ~*(dstPtr + col * bypp + x);
				rlePtr += opaq * bypp;
				col += opaq;
			}
			else if (!col)
				break;

			if (col == w)
			{
				if (!--h)
					break;
				col = 0;
				dstPtr += pitch;
			}
		}
	}
	else
	{
		if (widgetPtr->style == WIDGET_STYLE_ISO)
		{
			yp += (ISO_HGT - ISO_BOTTOM - g_icon_height);
		}
 
		/* Get the address of the top-left corner */
		dstPtr = bitmapPtr->pixelPtr + xp * bypp + yp * pitch;

		for (y = 0; y < g_icon_height; y++)
		{
			for (x = 0; x < g_icon_width * bypp; x++)
			{
				*(dstPtr + x) = ~*(dstPtr + x);
			}
			dstPtr += pitch;
		}
	}

#ifndef GRID_EFFECT
#ifndef WIDGET_EFFECT
	XCopyArea(widgetPtr->display,
		widgetPtr->bitmap.pixmap, /* source drawable */
		Tk_WindowId(widgetPtr->tkwin), /* dest drawable */
		widgetPtr->copyGC, /* graphics context */
		xp, yp, /* source top-left */
		(unsigned int) widgetPtr->gwidth, /* width */
		(unsigned int) widgetPtr->gheight, /* height */
		xp - widgetPtr->bx, yp - widgetPtr->by /* dest top-left */
	);
#endif /* WIDGET_EFFECT */
#endif /* GRID_EFFECT */
}

#ifdef GRID_EFFECT

/*
 * Visually "flash" the given location.
 */
void angtk_invert_spot(int y, int x)
{
	int delay;
	int row, col;
	DoubleLink *link;
	t_grid_effect *effectPtr = &g_grid_effect[y][x];
	Widget *widgets[128];
	int i, widgetCnt = 0;
	Display *display = None;

	/* Don't bother if the user won't see it. */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	delay = op_ptr->delay_factor * op_ptr->delay_factor;
#endif
#if defined(ZANGBANDTK)
	delay = delay_factor * delay_factor * delay_factor;
#endif
	if (delay <= 0)
		return;

	/* Set "invert" effect */
	effectPtr->flags |= GRID_EFFECT_INVERT;

	/* Check each mapped Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Skip micro-map Widgets */
		if (widgetPtr->style == WIDGET_STYLE_MAP)
			continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* Invalidate the grid, so it will be redrawn */
		Widget_Invalidate(widgetPtr, row, col);
		widgetPtr->flags |= WIDGET_DRAW_INVALID;

		/* Display */
		Widget_Display((ClientData)widgetPtr);

		if (widgetCnt < sizeof(widgets)/sizeof(widgets[0]))
			widgets[widgetCnt++] = widgetPtr;

		display = widgetPtr->display;
	}

	if (display != None)
		Plat_SyncDisplay(display);

	/* Delay */
	Term_xtra(TERM_XTRA_DELAY, delay);

	/* Remove "invert" effect*/
	effectPtr->flags &= ~GRID_EFFECT_INVERT;

	/* Check each mapped Widget */
	for (i = 0; i < widgetCnt; i++)
	{
		Widget *widgetPtr = widgets[i];

		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* Invalidate the grid, so it will be redrawn */
		Widget_Invalidate(widgetPtr, row, col);
		widgetPtr->flags |= WIDGET_DRAW_INVALID;

		Widget_Display((ClientData)widgetPtr);
	}

	if (display != None)
		Plat_SyncDisplay(display);

	/* Delay */
	Term_xtra(TERM_XTRA_DELAY, delay);
}

#elif defined(WIDGET_EFFECT)

/*
 * Visually "flash" the given location.
 */
void angtk_invert_spot(int y, int x)
{
	int delay;
	int row, col;
	DoubleLink *link;
	ExWidgetEffect *effectPtr;
	Widget *widgets[128];
	int i, widgetCnt = 0;

	/* Don't bother if the user won't see it. */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	delay = op_ptr->delay_factor * op_ptr->delay_factor;
#endif
#if defined(ZANGBANDTK)
	delay = delay_factor * delay_factor * delay_factor;
#endif
	if (delay <= 0)
		return;

	/* Check each mapped Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Skip micro-map Widgets */
		if (widgetPtr->style == WIDGET_STYLE_MAP)
			continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* Add "invert" effect. */
		effectPtr = &((ExWidget *) widgetPtr)->effect[col + row * widgetPtr->cc];
		effectPtr->flags |= WIDGET_EFFECT_INVERT;

		/* Invalidate the grid, so it will be redrawn */
		Widget_Invalidate(widgetPtr, row, col);
		widgetPtr->flags |= WIDGET_DRAW_INVALID;

		/* Display */
		Widget_Display((ClientData)widgetPtr);

		if (widgetCnt < sizeof(widgets)/sizeof(widgets[0]))
			widgets[widgetCnt++] = widgetPtr;
	}

	/* Delay */
	Term_xtra(TERM_XTRA_DELAY, delay);

	/* Check each mapped Widget */
	for (i = 0; i < widgetCnt; i++)
	{
		Widget *widgetPtr = widgets[i];

		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* Clear "invert" effect. */
		effectPtr = &((ExWidget *) widgetPtr)->effect[col + row * widgetPtr->cc];
		effectPtr->flags &= ~WIDGET_EFFECT_INVERT;

		/* Invalidate the grid, so it will be redrawn */
		Widget_Invalidate(widgetPtr, row, col);
		widgetPtr->flags |= WIDGET_DRAW_INVALID;

		Widget_Display((ClientData)widgetPtr);
	}

	/* Delay */
	Term_xtra(TERM_XTRA_DELAY, delay);
}

#else /* WIDGET_EFFECT */

/*
 * Visually "flash" the given location.
 */
void angtk_invert_spot(int y, int x)
{
	int row, col;
	DoubleLink *link;
	t_display wtd;
	Display *display = None;

#if 1 /* Mar 9 2009 */
	/* Don't bother if we won't see it. */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	if (op_ptr->delay_factor <= 0)
		return;
#endif
#if defined(ZANGBANDTK)
	if (delay_factor <= 0)
		return;
#endif
#endif /* Mar 9 2009 */

	/* Check each mapped Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Skip micro-map Widgets */
		if (widgetPtr->style == WIDGET_STYLE_MAP)
			continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

#ifdef WIDGET_STYLE_TEXT
		if (widgetPtr->style == WIDGET_STYLE_TEXT)
			wtd.blank = TRUE;
		else
#endif
		(*((ExWidget *) widgetPtr)->whatToDrawProc)(widgetPtr, y, x, &wtd);

		/* Invert pixels, copy to screen */
		Widget_InvertSpot(widgetPtr, row, col, &wtd);

		/* Invalidate the grid, so it will be redrawn later */
		Widget_Invalidate(widgetPtr, row, col);

		/* Redraw later */
		widgetPtr->flags |= WIDGET_DRAW_INVALID;
#if 0 /* Mar 9 2009 */
		Widget_EventuallyRedraw(widgetPtr);
#endif /* Mar 9 2009 */

		display = widgetPtr->display;
	}

	if (display != None)
		Plat_SyncDisplay(display);

	/* Delay */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	Term_xtra(TERM_XTRA_DELAY, op_ptr->delay_factor * op_ptr->delay_factor);
#endif
#if defined(ZANGBANDTK)
	Term_xtra(TERM_XTRA_DELAY, delay_factor * delay_factor * delay_factor);
#endif

#if 1 /* Mar 9 2009 */
	/* Check each mapped Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Skip micro-map Widgets */
		if (widgetPtr->style == WIDGET_STYLE_MAP)
			continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		if (!(widgetPtr->flags & WIDGET_DRAW_INVALID))
			continue;

		Widget_Display((ClientData)widgetPtr);
	}

	if (display != None)
		Plat_SyncDisplay(display);
#endif /* Mar 9 2009 */
}

#endif /* WIDGET_EFFECT */

#ifdef PROJECT_HINT

/*
 * Here is some experiemental code to display the grids affected by
 * a spell during targetting. Before calling get_aim_dir(), we call
 * angtk_project_hint(1,rad,flg). Call angtk_project_hint(2,y,x) to
 * specify a target location.
 *
 * I decided against using this because there are so many calls
 * for spells, rods, wands, artifacts, racial powers etc, which would
 * mean adding many calls to angtk_project_hint() and also be a lot of
 * work keeping the calls up-to-date.
 *
 * On the Tcl side, when the cursor moves over the Main Window Widget
 * we call "angband keypress &$y\n$x\n" which calls angtk_project_hint(4,y,x).
 *
 * Look through the ZAngbandTk sources for PROJECT_HINT.
 */

struct project_type
{
	int who;
	int rad;
	int dam;
	int typ;
	int flg;

	int y, x;
	bool hint;
}
g_prj = {0, 0, 0, 0, 0, -1, -1, FALSE};

void angtk_project_hint(int action, int rad, int y, int x, int flg)
{
	switch (action)
	{	
		case 1:
		{
			g_prj.rad = rad;
			g_prj.flg = flg;

			g_prj.y = -1;
			g_prj.x = -1;

			g_prj.hint = TRUE;
			break;
		}

		case 2:
		{
			if (!g_prj.hint) break;
			g_prj.y = y;
			g_prj.x = x;
			break;
		}

		case 3:
		{
			g_prj.y = -1;
			g_prj.x = -1;
			g_prj.hint = FALSE;
			break;
		}

		case 4:
		{
			if (!g_prj.hint) break;
			g_prj.y = y;
			g_prj.x = x;
			break;
		}
	}
}

void draw_rectangle(Widget *widgetPtr, int row, int col)
{
	int gHeight = widgetPtr->gheight;
	int gWidth = widgetPtr->gwidth;
	int lineWidth = 1;
    HPEN pen, oldPen;
    HBRUSH oldBrush;

    pen = CreatePen(PS_SOLID, lineWidth, RGB(128,128,128));
    oldPen = SelectObject(widgetPtr->bitmap.hdc, pen);
    oldBrush = SelectObject(widgetPtr->bitmap.hdc,
    	GetStockObject(NULL_BRUSH));
    SetROP2(widgetPtr->bitmap.hdc, R2_COPYPEN);
    Rectangle(widgetPtr->bitmap.hdc,
		col * gWidth + lineWidth / 2,
		row * gHeight + lineWidth / 2,
		(col + 1) * gWidth - lineWidth + lineWidth / 2 + 1,
		(row + 1) * gHeight - lineWidth + lineWidth / 2 + 1);
    DeleteObject(SelectObject(widgetPtr->bitmap.hdc, oldPen));
    SelectObject(widgetPtr->bitmap.hdc, oldBrush);
}

void highlight_target_grids_aux(int y, int x)
{
	int y_min, y_max, x_min, x_max;
	DoubleLink *link;

	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);
		ExWidget *exPtr = (ExWidget *) widgetPtr;

		/* Don't draw in micro-map */
		if (widgetPtr->style != WIDGET_STYLE_ICON) continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE) continue;

		/* Get the visible bounds */
		y_min = widgetPtr->y_min, y_max = widgetPtr->y_max;
		x_min = widgetPtr->x_min, x_max = widgetPtr->x_max;

		/* Don't draw out of bounds */
		if ((y < y_min) || (y >= y_max)) continue;
		if ((x < x_min) || (x >= x_max)) continue;

		draw_rectangle(widgetPtr, y - y_min, x - x_min);

		/* Mark the grid as invalid */
		GridGet(&exPtr->grid, y - y_min, x - x_min)->valid = FALSE;
	}
}

bool angtk_project(int who, int rad, int y, int x, int dam, int typ, int flg)
{
	int i, t, dist;

	int y1, x1;
	int y2, x2;

	int dist_hack = 0;

	/* Assume to be a normal ball spell */
	bool breath = FALSE;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
	u16b path_g[512];

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	byte gx[256], gy[256];

	/* Encoded "radius" info (see above) */
	byte gm[32];

	/* Actual radius encoded in gm[] */
	int gm_rad = rad;

	bool jump = FALSE;

	/* Hack -- Jump to target */
	if (flg & (PROJECT_JUMP))
	{
		x1 = x;
		y1 = y;

		/* Clear the flag */
		flg &= ~(PROJECT_JUMP);

		jump = TRUE;
	}

	/* Start at player */
	else if (who <= 0)
	{
		x1 = px;
		y1 = py;
	}

	/* Start at monster */
	else if (who > 0)
	{
		x1 = m_list[who].fx;
		y1 = m_list[who].fy;
	}

	/* Oops */
	else
	{
		x1 = x;
		y1 = y;
	}

	/* Default "destination" */
	y2 = y;
	x2 = x;

	/* Hack -- verify stuff */
	if (flg & (PROJECT_THRU))
	{
		if ((x1 == x2) && (y1 == y2))
		{
			flg &= ~(PROJECT_THRU);
		}
	}

	/* Handle a breath attack */
	if (rad < 0)
	{
		rad = 0 - rad;
		breath = TRUE;
		flg |= PROJECT_HIDE;
	}

	/* Hack -- Assume there will be no blast (max radius 32) */
	for (dist = 0; dist < 32; dist++)
		gm[dist] = 0;

	/* Initial grid */
	y = y1;
	x = x1;
	dist = 0;

	/* Collect beam grids */
	if (flg & (PROJECT_BEAM))
	{
		gy[grids] = y;
		gx[grids] = x;
		grids++;
	}

	/* Calculate the projection path */
	path_n = project_path(path_g, MAX_RANGE, y1, x1, y2, x2, flg);

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		/* Hack -- Balls explode before reaching walls */
		if (!cave_floor_bold(ny, nx) && (rad > 0))
			break;

		/* Advance */
		y = ny;
		x = nx;

		/* Collect beam grids */
		if (flg & (PROJECT_BEAM))
		{
			gy[grids] = y;
			gx[grids] = x;
			grids++;
		}
	}

	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Start the "explosion" */
	gm[0] = 0;

	/* Hack -- make sure beams get to "explode" */
	gm[1] = grids;

	dist_hack = dist;
	dist = path_n;

	/* If we found a "target", explode there */
	if (dist <= MAX_RANGE)
	{
		/* Mega-Hack -- remove the final "beam" grid */
		if ((flg & (PROJECT_BEAM)) && (grids > 0))
			grids--;

		/*
		 * Create a conical breath attack
		 *
		 *         ***
		 *     ********
		 * D********@**
		 *     ********
		 *         ***
		 */
		if (breath)
		{
			int by, bx;
			int brad = 0;
			int bdis = 0;
			int cdis;

			/* Not done yet */
			bool done = FALSE;

			flg &= ~(PROJECT_HIDE);

			by = y1;
			bx = x1;

			while (bdis <= dist + rad)
			{
				/* Travel from center outward */
				for (cdis = 0; cdis <= brad; cdis++)
				{
					/* Scan the maximal blast area of radius "cdis" */
					for (y = by - cdis; y <= by + cdis; y++)
					{
						for (x = bx - cdis; x <= bx + cdis; x++)
						{
							/* Ignore "illegal" locations */
							if (!in_bounds(y, x))
								continue;

							/* Enforce a circular "ripple" */
							if (distance(y1, x1, y, x) != bdis)
								continue;

							/* Enforce an arc */
							if (distance(by, bx, y, x) != cdis)
								continue;

							/* The blast is stopped by walls */
							if (!los(by, bx, y, x))
								continue;

							/* Save this grid */
							gy[grids] = y;
							gx[grids] = x;
							grids++;
						}
					}
				}

				/* Encode some more "radius" info */
				gm[bdis + 1] = grids;

				/* Stop moving */
				if ((by == y2) && (bx == x2))
					done = TRUE;

				/* Finish */
				if (done)
				{
					bdis++;
					continue;
				}

				/* Ripple outwards */
				mmove2(&by, &bx, y1, x1, y2, x2);

				/* Find the next ripple */
				bdis++;

				/* Increase the size */
				brad = (rad * bdis) / dist;
			}

			/* Store the effect size */
			gm_rad = bdis;
		}

		else
		{
			/* Determine the blast area, work from the inside out */
			for (dist = 0; dist <= rad; dist++)
			{
				/* Scan the maximal blast area of radius "dist" */
				for (y = y2 - dist; y <= y2 + dist; y++)
				{
					for (x = x2 - dist; x <= x2 + dist; x++)
					{
						/* Ignore "illegal" locations */
						if (!in_bounds2(y, x))
							continue;

						/* Enforce a "circular" explosion */
						if (distance(y2, x2, y, x) != dist)
							continue;

						if (typ == GF_DISINTEGRATE)
						{
						}
						else
						{
							/* Ball explosions are stopped by walls */
							if (!los(y2, x2, y, x))
								continue;
						}

						/* Save this grid */
						gy[grids] = y;
						gx[grids] = x;
						grids++;
					}
				}

				/* Encode some more "radius" info */
				gm[dist + 1] = grids;
			}
		}
	}

	/* Speed -- ignore "non-explosions" */
	if (!grids)
		return (FALSE);

	/* Display the "blast area" if requested */
	if (!blind && !(flg & (PROJECT_HIDE)))
	{
		/* Then do the "blast", from inside out */
		for (t = 0; t <= gm_rad; t++)
		{
			/* Dump everything with this radius */
			for (i = gm[t]; i < gm[t + 1]; i++)
			{
				/* Extract the location */
				y = gy[i];
				x = gx[i];

				/* The player can see it */
				if (player_has_los_bold(y, x))
				{
					highlight_target_grids_aux(y, x);
				}
			}
		}
	}

	/* Return "something was noticed" */
	return (TRUE);
}

#endif /* PROJECT_HINT */

static void DrawIconSpec(int y, int x, int w, int h, IconSpec iconSpec, BitmapPtr bitmapPtr)
{
	int pitch = bitmapPtr->pitch;
	int bypp = bitmapPtr->pixelSize;
	IconPtr srcPtr, dstPtr;
	int y2, dx, dy;
	int length;
	t_icon_type *iconTypePtr;

	/* Ignore NONE icon */
	if (iconSpec.type == ICON_TYPE_NONE)
		return;

	/* Special handling of BLANK */
	if (iconSpec.type == ICON_TYPE_BLANK)
	{
		/* Access the "blank" icon data */
		iconTypePtr = &g_icon_type[ICON_TYPE_BLANK];
/*		length = iconTypePtr->pitch;*/
		srcPtr = iconTypePtr->icon_data;
#if 1
		goto blitIt;
#else
		/* Get the address of where to write the data in the bitmap */
		dx = (w - iconTypePtr->width) / 2;
		dy = (h - iconTypePtr->height) / 2;
		dstPtr = bitmapPtr->pixelPtr +
			(x + dx) * bypp +
			(y + dy) * pitch;

		/* Write the icon data */
		for (y2 = 0; y2 < iconTypePtr->height; y2++)
		{
			memcpy(dstPtr, srcPtr, length);
			srcPtr += length;
			dstPtr += pitch;
		}

		/* Done */
		return;
#endif
	}

	/* Sanity check icon type */
	if ((iconSpec.type) < 0 || (iconSpec.type >= g_icon_type_count))
	{
		/* Use "default" icon */
		iconSpec.type = ICON_TYPE_DEFAULT;
		iconSpec.index = 0;
		iconSpec.ascii = -1;
		iconSpec.dark = 0;
	}

	/* Access the icon type */
	iconTypePtr = &g_icon_type[iconSpec.type];

	/* Sanity check icon index */
	if ((iconSpec.index < 0) || (iconSpec.index >= iconTypePtr->icon_count))
	{
		/* Use "default" icon */
		iconSpec.type = ICON_TYPE_DEFAULT;
		iconSpec.index = 0;
		iconSpec.ascii = -1;
		iconSpec.dark = 0;

		/* Access the DEFAULT icon type */
		iconTypePtr = &g_icon_type[iconSpec.type];
	}

	if (iconSpec.ascii != -1)
	{
		if (iconSpec.ascii >= g_ascii_count)
		{
			/* Use "default" icon */
			iconSpec.type = ICON_TYPE_DEFAULT;
			iconSpec.index = 0;
			iconSpec.ascii = -1;
			iconSpec.dark = 0;

			/* Access the DEFAULT icon type */
			iconTypePtr = &g_icon_type[iconSpec.type];
		}
		else
		{
			IconData iconData;
/*			iconTypePtr = &g_icon_type[ICON_TYPE_BLANK];*/
/*			length = iconTypePtr->pitch;*/
			srcPtr = Icon_GetAsciiData(&iconSpec, iconData);
#if 1
			goto blitIt;
#else
			dx = (w - iconTypePtr->width) / 2;
			dy = (h - iconTypePtr->height) / 2;
			dstPtr = bitmapPtr->pixelPtr +
				(x + dx) * bypp +
				(y + dy) * pitch;
			for (y2 = 0; y2 < iconTypePtr->height; y2++)
			{
				memcpy(dstPtr, srcPtr, length);
				srcPtr += length;
				dstPtr += pitch;
			}
			return;
#endif
		}
	}

	/* Verify darkness */
	if (iconSpec.dark)
	{
		if ((iconSpec.dark < 0) || (iconSpec.dark > 2))
		{
			/* Use "default" icon */
			iconSpec.type = ICON_TYPE_DEFAULT;
			iconSpec.index = 0;
			iconSpec.ascii = -1;
			iconSpec.dark = 0;

			/* Access the DEFAULT icon type */
			iconTypePtr = &g_icon_type[iconSpec.type];
		}
	}

	/* Create dark_data if needed */
	if (iconSpec.dark && (iconTypePtr->flags[iconSpec.index] & ICON_FLAG_DARK))
	{
		Icon_MakeDark(iconTypePtr, iconSpec.index);
	}

	/* Transparent */
	if (iconTypePtr->rle_data)
	{
		unsigned char *bounds = iconTypePtr->rle_bounds + iconSpec.index * 4;
		int dw = bounds[2];
		int dh = bounds[3];
		int col = 0;
		IconPtr rlePtr;

		if (iconSpec.dark)
			rlePtr = iconTypePtr->dark_data[iconSpec.index] + (iconSpec.dark - 1) * iconTypePtr->rle_len[iconSpec.index];
		else if (iconTypePtr->dynamic)
			rlePtr = ((IconPtr *) iconTypePtr->rle_data)[iconSpec.index];
		else
			rlePtr = iconTypePtr->rle_data + iconTypePtr->rle_offset[iconSpec.index];

		dx = (w - iconTypePtr->width) / 2;
		dy = (h - iconTypePtr->height) / 2;

		/* Sanity check */
		if ((x + dx < 0) || (x + dx + iconTypePtr->width > bitmapPtr->width) ||
			(y + dy < 0) || (y + dy + iconTypePtr->height > bitmapPtr->height))
		{
			dbwin("can't blit at x,y=%d,%d\n", x, y);
			return;
		}

		dstPtr = bitmapPtr->pixelPtr +
			(x + dx) * bypp +
			(y + dy) * pitch
			+ bounds[0] * bypp
			+ bounds[1] * pitch;

		while (1)
		{
			unsigned int trans, opaq;

			trans = rlePtr[0];
			opaq = rlePtr[1];
			rlePtr += 2;

			col += trans;

			if (opaq)
			{
				memcpy(dstPtr + col * bypp, rlePtr, opaq * bypp);
				rlePtr += opaq * bypp;
				col += opaq;
			}
			else if (!col)
				break;

			if (col == dw)
			{
				if (!--dh)
					break;
				col = 0;
				dstPtr += pitch;
			}
		}

		/* Done */
		return;
	}

	if (iconSpec.dark)
		srcPtr = iconTypePtr->dark_data[iconSpec.index] + (iconSpec.dark - 1) * iconTypePtr->length;
	else
		/* FIXME: tint */
		srcPtr = iconTypePtr->icon_data + iconSpec.index * iconTypePtr->length;

blitIt:

	/* Get the address of where to write the data in the bitmap */
	dx = (w - iconTypePtr->width) / 2;
	dy = (h - iconTypePtr->height) / 2;

	/* Sanity check */
	if ((x + dx < 0) || (x + dx + iconTypePtr->width > bitmapPtr->width) ||
		(y + dy < 0) || (y + dy + iconTypePtr->height > bitmapPtr->height))
	{
		return;
	}

	dstPtr = bitmapPtr->pixelPtr +
		(x + dx) * bypp +
		(y + dy) * pitch;

	length = iconTypePtr->pitch;

	/* Write the icon data */
	for (y2 = 0; y2 < iconTypePtr->height; y2++)
	{
		memcpy(dstPtr, srcPtr, length);
		srcPtr += length;
		dstPtr += pitch;
	}
}

/* Widget.whatToDrawProc() */
void widget_wtd(Widget *widgetPtr, int y, int x, t_display *wtd)
{
	/* If this is a valid cave location, get the display info. */
	if (in_bounds_test(y, x))
		get_display_info(y, x, wtd);

	/* This isn't a valid cave location, so draw a "blank" icon */
	else
		wtd->blank = TRUE;
}

#if 0 /* Nov 12 2004 */

/*
 * Draw an aura around an icon
 */
void Widget_DrawAura(Widget *widgetPtr, int row, int col, t_display *wtd)
{
	BitmapPtr bitmapPtr = &widgetPtr->bitmap;
	int bypp = bitmapPtr->pixelSize;
	int pitch = bitmapPtr->pitch;
	IconPtr dstPtr;
	t_icon_type *iconTypePtr;
	int xp, yp, x, y;
	IconValue highPixel[4];

	PixelSet_RGB((IconPtr) highPixel, 255, 0, 0, bypp);

#ifdef WIDGET_STYLE_TEXT
	if (widgetPtr->style != WIDGET_STYLE_ISO)
#else
	if (widgetPtr->style == WIDGET_STYLE_ICON)
#endif
	{
		yp = row * widgetPtr->gheight;
		xp = col * widgetPtr->gwidth;
	}
	else
	{
		yp = widgetPtr->yp[row * widgetPtr->cc + col];
		xp = widgetPtr->xp[row * widgetPtr->cc + col] + 10;
	}

	/* The icon is transparent */
	if (!wtd->blank && (wtd->fg.type != ICON_TYPE_NONE) &&
		g_icon_type[wtd->fg.type].rle_data)
	{
		IconPtr rlePtr;
		int col = 0;
		int h, w;
		unsigned char *bounds;
		unsigned char *bufPtr;
		int off, val;
		int warning = (p_ptr->mhp * op_ptr_hitpoint_warn) / 10;

		/* Access the icon info */
		iconTypePtr = &g_icon_type[wtd->fg.type];

		rlePtr = iconTypePtr->rle_data + iconTypePtr->rle_offset[wtd->fg.index];
		bounds = iconTypePtr->rle_bounds + wtd->fg.index * 4;
		w = bounds[2];
		h = bounds[3];
#ifdef WIDGET_STYLE_TEXT
		if (widgetPtr->style != WIDGET_STYLE_ISO)
#else
		if (widgetPtr->style == WIDGET_STYLE_ICON)
#endif
		{
			xp += bounds[0];
			yp += bounds[1];
		}
		else
		{
			xp += bounds[0];
			yp += ISO_HGT - ISO_BOTTOM - h;
		}

		bufPtr = Tcl_AllocDebug((w + 4) * (h + 4));
		for (x = 0; x < (w + 4) * (h + 4); x++)
			bufPtr[x] = 0;
		y = 2;

		while (1)
		{
			unsigned int trans, opaq;

			trans = rlePtr[0];
			opaq = rlePtr[1];
			rlePtr += 2;
			col += trans;

			if (opaq)
			{
				for (x = col; x < col + opaq; x++)
					bufPtr[y * (w + 4) + x + 2] = 1;

				rlePtr += opaq * bypp;
				col += opaq;
			}
			else if (!col)
				break;

			if (col == w)
			{
				if (++y == h + 2)
					break;
				col = 0;
			}
		}

		for (x = 1; x < w + 3; x++)
		{
			off = 1;
			for (y = 1; y < h + 3; y++)
			{
				val = bufPtr[y * (w + 4) + x];
				if (off && (val == 1))
					bufPtr[(y - 1) * (w + 4) + x] = 2;
				else if (!off && (val == 0))
					bufPtr[y * (w + 4) + x] = 2;
				off = val != 1;
			}
		}

		for (y = 1; y < h + 3; y++)
		{
			off = 1;
			for (x = 1; x < w + 3; x++)
			{
				val = bufPtr[y * (w + 4) + x];
				if (off && (val == 1))
					bufPtr[y * (w + 4) + x - 1] = 2;
				else if (!off && (val == 0))
					bufPtr[y * (w + 4) + x] = 2;
				off = val != 1;
			}
		}

		dstPtr = bitmapPtr->pixelPtr + (xp - 2) * bypp + (yp - 2) * pitch;

		y = 0;
		if (p_ptr->chp <= warning)
			y = (h + 4.0) - (h + 4.0) * (((double) p_ptr->chp) / warning);
		dstPtr += y * pitch;

		for (; y < h + 4; y++)
		{
			for (x = 0; x < w + 4; x++)
			{
				if (bufPtr[y * (w + 4) + x] == 2)
					memcpy(dstPtr + x * bypp, highPixel, bypp);
			}
			dstPtr += pitch;
		}

		Tcl_FreeDebug(bufPtr);
	}
	else
	{
		if (widgetPtr->style == WIDGET_STYLE_ISO)
		{
			yp += (ISO_HGT - ISO_BOTTOM - g_icon_height);
		}
 
		/* Get the address of the top-left corner */
		dstPtr = bitmapPtr->pixelPtr + xp * bypp + yp * pitch;

		for (y = 0; y < g_icon_height; y++)
		{
			for (x = 0; x < g_icon_width * bypp; x++)
			{
				*(dstPtr + x) = ~*(dstPtr + x);
			}
			dstPtr += pitch;
		}
	}
}

#endif /* Nov 12 2004 */

#ifdef PLAYER_HEALTH_BAR
#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

static void DrawPlayerHealthBar(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int r, g, b;
	int row, col;
	int gHeight = widgetPtr->gheight;
	int gWidth = widgetPtr->gwidth;
#if 1
	BitmapType *bitmapPtr = &widgetPtr->bitmap;
	IconPtr dstPtr;
	int x, y;
	char pixel[4];
	int width;

	if (p_ptr->chp > (p_ptr->mhp * 8/*op_ptr_hitpoint_warn*/) / 10)
		return;

	if (!Widget_CaveToView(widgetPtr, p_ptr_py, p_ptr_px, &row, &col))
		return;

	r = ((double) exPtr->healthBar.firstColorPtr->red / USHRT_MAX) * 255;
	g = ((double) exPtr->healthBar.firstColorPtr->green / USHRT_MAX) * 255;
	b = ((double) exPtr->healthBar.firstColorPtr->blue / USHRT_MAX) * 255;
	PixelSet_RGB((IconPtr) pixel, r, g, b, bitmapPtr->pixelSize);

	x = col * gWidth, y = row * gHeight;
	dstPtr = bitmapPtr->pixelPtr + x * bitmapPtr->pixelSize + y * bitmapPtr->pitch;
	for (y = 0; y < 2; y++)
	{
		for (x = 0; x < gWidth; x++)
		{
			memcpy(dstPtr + x * bitmapPtr->pixelSize, pixel, bitmapPtr->pixelSize);
		}
		dstPtr += bitmapPtr->pitch;
	}

	width = gWidth;
	if (p_ptr->mhp > 0)
		width *= (double)p_ptr->chp / p_ptr->mhp;

	r = ((double) exPtr->healthBar.secondColorPtr->red / USHRT_MAX) * 255;
	g = ((double) exPtr->healthBar.secondColorPtr->green / USHRT_MAX) * 255;
	b = ((double) exPtr->healthBar.secondColorPtr->blue / USHRT_MAX) * 255;
	PixelSet_RGB((IconPtr) pixel, r, g, b, bitmapPtr->pixelSize);

	x = col * gWidth, y = row * gHeight;
	dstPtr = bitmapPtr->pixelPtr + x * bitmapPtr->pixelSize + y * bitmapPtr->pitch;
	for (y = 0; y < 2; y++)
	{
		for (x = 0; x < width; x++)
		{
			memcpy(dstPtr + x * bitmapPtr->pixelSize, pixel, bitmapPtr->pixelSize);
		}
		dstPtr += bitmapPtr->pitch;
	}
#else /* 0 */
	int x1, y1, x2, y2;
	XGCValues gcValues;
	GC gc;
	int width;

	if (p_ptr->chp > (p_ptr->mhp * 8/*op_ptr_hitpoint_warn*/) / 10)
		return;

	if (!Widget_CaveToView(widgetPtr, p_ptr_py, p_ptr_px, &row, &col))
		return;

	x1 = col * gWidth;
	x2 = x1 + gWidth;
	y1 = row * gHeight;
	y2 = y1 + gHeight;

	gcValues.foreground = exPtr->healthBar.firstColorPtr->pixel;
	gc = Tk_GetGC(widgetPtr->tkwin, GCForeground, &gcValues);

	XFillRectangle(widgetPtr->display,
		widgetPtr->bitmap.pixmap, gc,
		x1,
		y1,
		(x2 - x1),
		2);

	Tk_FreeGC(widgetPtr->display, gc);

	gcValues.foreground = exPtr->healthBar.secondColorPtr->pixel;
	gc = Tk_GetGC(widgetPtr->tkwin, GCForeground, &gcValues);

	width = (x2 - x1);
	if (p_ptr->mhp)
		width *= (double)p_ptr->chp / p_ptr->mhp;
	XFillRectangle(widgetPtr->display,
		widgetPtr->bitmap.pixmap, gc,
		x1,
		y1,
		width,
		2);

	Tk_FreeGC(widgetPtr->display, gc);
#endif /* 0 */
	exPtr->healthBar.drawn = TRUE;
	exPtr->healthBar.width = width;
}

void angtk_update_health_bar(viod)
{
	DoubleLink *link;
	int row, col;
	int y = p_ptr_py, x = p_ptr_px;

	/* Check each Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);
		ExWidget *exPtr = (ExWidget *) widgetPtr;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		/* A full redraw is already pending */
		if (widgetPtr->flags & WIDGET_WIPE)
			continue;

		/* Skip micromap */
		if (widgetPtr->style == WIDGET_STYLE_MAP)
			continue;

		/* Cave location isn't visible */
		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* Should the bar be visible? */
		if (p_ptr->chp <= (p_ptr->mhp * 8/*op_ptr_hitpoint_warn*/) / 10)
		{
			/* Is it already visible? */
			if (exPtr->healthBar.drawn == TRUE)
			{
				int width;
				if (widgetPtr->style == WIDGET_STYLE_ISO)
					width = ISO_WID;
				else
					width = widgetPtr->gwidth;
				if (p_ptr->mhp)
					width *= (double)p_ptr->chp / p_ptr->mhp;

				/* If the appearance wouldn't change skip this widget */
				if (width == exPtr->healthBar.width)
					continue;
			}
		}
		/* Bar should be hidden. */
		else if (exPtr->healthBar.drawn == FALSE)
		{
			continue;
		}

		/* Mark the location as invalid */
		Widget_Invalidate(widgetPtr, row, col);

		/* Redraw invalid grids later */
		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);
	}
}

#endif /* PLAYER_HEALTH_BAR */

/*
 * Redraw everything.
 */
void widget_draw_all(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int tile, layer;
	int rc = widgetPtr->rc;
	int cc = widgetPtr->cc;
	int gw = widgetPtr->gwidth;
	int gh = widgetPtr->gheight;
	DoubleLink *link;
	int y, x, yp, xp;
	t_display wtd;
	IconSpec iconSpec;
	BitmapPtr bitmapPtr = &widgetPtr->bitmap;
#ifdef GRID_EFFECT
	t_grid_effect *effectPtr;
#elif defined(WIDGET_EFFECT)
	ExWidgetEffect *effectPtr;
#endif

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("widget_draw_all\n");

	/* Paranoia: make sure the bitmap exists */
	if (widgetPtr->bitmap.pixelPtr == NULL) return;

	/* Drawing is disabled */
	if (widgetPtr->flags & WIDGET_NO_UPDATE) return;

	/* Keep track of animated tiles */
	widgetPtr->animCnt = 0;

#ifdef PLAYER_HEALTH_BAR
	exPtr->healthBar.drawn = FALSE;
#endif

	for (tile = 0; tile < cc * rc; tile++)
	{
		/* This tile does not need to be redrawn */
		widgetPtr->info[tile] &= ~(WIDGET_INFO_DIRTY | WIDGET_INFO_ANIM);

		y = widgetPtr->y_min + tile / cc;
		x = widgetPtr->x_min + tile % cc;

		(*exPtr->whatToDrawProc)(widgetPtr, y, x, &wtd);

		yp = tile / cc * gh;
		xp = tile % cc * gw;

		/* Just "erase" this spot */
		if (wtd.blank)
		{
			iconSpec.type = ICON_TYPE_BLANK;
			DrawIconSpec(yp, xp, gw, gh, iconSpec, bitmapPtr);
			continue;
		}

		/* Keep track of animated tiles */
		if (wtd.anim)
		{
			widgetPtr->info[tile] |= WIDGET_INFO_ANIM;
			widgetPtr->anim[widgetPtr->animCnt++] = tile;
		}

		/*
		 * Draw 1-4 background layers.
		 */
		for (layer = 0; layer < ICON_LAYER_MAX; layer++)
		{
			iconSpec = wtd.bg[layer];

			/* Stop at NONE icon */
			if (iconSpec.type == ICON_TYPE_NONE)
				break;

			/* Draw background icon */
			DrawIconSpec(yp, xp, gw, gh, iconSpec, bitmapPtr);

			/* Stop at BLANK icon */
			if (iconSpec.type == ICON_TYPE_BLANK)
				break;
		}

		/* Draw foreground icon */
		if (wtd.fg.type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, wtd.fg, bitmapPtr);

#if 0
if (y == p_ptr_py && x == p_ptr_px) Widget_DrawAura(widgetPtr, tile/cc, tile%cc, &wtd);
#endif

#ifdef GRID_EFFECT
		effectPtr = &g_grid_effect[y][x];

		/* Invert */
		if (effectPtr->flags & GRID_EFFECT_INVERT)
			Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

		/* Draw effect icon */
		if (effectPtr->icon.type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, effectPtr->icon, bitmapPtr);
#elif defined(WIDGET_EFFECT)
		effectPtr = &exPtr->effect[tile];

		/* Invert */
		if (effectPtr->flags & WIDGET_EFFECT_INVERT)
			Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

		/* Draw effect icon */
		if (effectPtr->icon.type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, effectPtr->icon, bitmapPtr);
#else
		/* Draw effect icon */
		if (exPtr->effect[tile].type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, exPtr->effect[tile], bitmapPtr);
#endif
	}

#ifdef PLAYER_HEALTH_BAR
	DrawPlayerHealthBar(widgetPtr);
#endif

	/* There are no invalid tiles */
	widgetPtr->invalidCnt = 0;

	/* Now draw all of the items for this Widget */
	for (link = widgetPtr->linkerItemVis.head; link; link = link->next)
	{
		WidgetItem *itemPtr = DoubleLink_Data(link, WidgetItem);

		(*itemPtr->typePtr->displayProc)(widgetPtr->interp, widgetPtr,
			itemPtr);

		/* Invalidate the grids covered by the item */
/*		Widget_InvalidateArea(widgetPtr, itemPtr->minY, itemPtr->minX,
			itemPtr->maxY, itemPtr->maxX);
*/	}

	/* Set dirty bounds to entire window */
	widgetPtr->dx = widgetPtr->bx;
	widgetPtr->dy = widgetPtr->by;
	widgetPtr->dw = widgetPtr->width;
	widgetPtr->dh = widgetPtr->height;
}

/*
 * Redraws only those grids that were specifically marked as invalid
 * Any affected widget items are also redrawn.
 */
void widget_draw_invalid(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int i, layer;
	int cc = widgetPtr->cc;
	int gw = widgetPtr->gwidth;
	int gh = widgetPtr->gheight;
	int y, x, yp, xp;
	DoubleLink *link;
	t_display wtd;
	IconSpec iconSpec;
	BitmapPtr bitmapPtr = &widgetPtr->bitmap;
	short *pinfo = widgetPtr->info;
	int dl, dt, dr, db;
	int by, bx;
#ifdef GRID_EFFECT
	t_grid_effect *effectPtr;
#elif defined(WIDGET_EFFECT)
	ExWidgetEffect *effectPtr;
#endif

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("widget_draw_invalid: %d\n", widgetPtr->invalidCnt);

	/* Paranoia: make sure the bitmap exists */
	if (bitmapPtr->pixelPtr == NULL) return;

	/* Drawing is disabled */
	if (widgetPtr->flags & WIDGET_NO_UPDATE) return;

	/* Keep track of dirty area */
	dl = bitmapPtr->width;
	dt = bitmapPtr->height;
	dr = 0;
	db = 0;

/*	widgetPtr->animCnt = 0; */

	for (i = 0; i < widgetPtr->invalidCnt; i++)
	{
		int tile = widgetPtr->invalid[i];

		/* This tile does not need to be redrawn */
		pinfo[tile] &= ~WIDGET_INFO_DIRTY;

		/* Cave coords */
		y = widgetPtr->y_min + tile / cc;
		x = widgetPtr->x_min + tile % cc;

		(*exPtr->whatToDrawProc)(widgetPtr, y, x, &wtd);

		/* Bitmap coords */
		yp = (tile / cc) * gh;
		xp = (tile % cc) * gw;

		/* Dirty bounds */
		if (xp < dl)
			dl = xp;
		if (yp < dt)
			dt = yp;
		if (xp + gw - 1 > dr)
			dr = xp + gw - 1;
		if (yp + gh - 1 > db)
			db = yp + gh - 1;

		/* Just "erase" this spot */
		if (wtd.blank)
		{
			iconSpec.type = ICON_TYPE_BLANK;
			DrawIconSpec(yp, xp, gw, gh, iconSpec, bitmapPtr);
			continue;
		}

		if (wtd.anim && !(pinfo[tile] & WIDGET_INFO_ANIM))
		{
			pinfo[tile] |= WIDGET_INFO_ANIM;
			widgetPtr->anim[widgetPtr->animCnt++] = tile;
		}

		/*
		 * Draw 1-4 background layers.
		 */
		for (layer = 0; layer < ICON_LAYER_MAX; layer++)
		{
			iconSpec = wtd.bg[layer];

			/* Stop at NONE icon */
			if (iconSpec.type == ICON_TYPE_NONE)
				break;

			/* Draw background icon */
			DrawIconSpec(yp, xp, gw, gh, iconSpec, bitmapPtr);

			/* Stop at BLANK icon */
			if (iconSpec.type == ICON_TYPE_BLANK)
				break;
		}

		/* Draw foreground icon */
		if (wtd.fg.type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, wtd.fg, bitmapPtr);

#ifdef GRID_EFFECT
		effectPtr = &g_grid_effect[y][x];

		/* Invert */
		if (effectPtr->flags & GRID_EFFECT_INVERT)
			Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

		/* Draw effect icon */
		if (effectPtr->icon.type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, effectPtr->icon, bitmapPtr);
#elif defined(WIDGET_EFFECT)
		effectPtr = &exPtr->effect[tile];

		/* Invert */
		if (effectPtr->flags & WIDGET_EFFECT_INVERT)
			Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

		/* Draw effect icon */
		if (effectPtr->icon.type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, effectPtr->icon, bitmapPtr);
#else
		/* Draw effect icon */
		if (exPtr->effect[tile].type != ICON_TYPE_NONE)
			DrawIconSpec(yp, xp, gw, gh, exPtr->effect[tile], bitmapPtr);
#endif

#ifdef PLAYER_HEALTH_BAR
		if (y == p_ptr_py && x == p_ptr_px)
		{
			exPtr->healthBar.drawn = FALSE;
			DrawPlayerHealthBar(widgetPtr);
		}
#endif
	}

	widgetPtr->invalidCnt = 0;

#ifdef PROJECT_HINT
	if (g_prj.y != -1)
		angtk_project(g_prj.who, g_prj.rad, g_prj.y, g_prj.x, g_prj.dam, g_prj.typ, g_prj.flg);
#endif /* PROJECT_HINT */

	/* Redraw any items inside the dirty area */
	by = widgetPtr->by;
	bx = widgetPtr->bx;
	for (link = widgetPtr->linkerItemVis.head; link; link = link->next)
	{
		WidgetItem *itemPtr = DoubleLink_Data(link, WidgetItem);

		if ((bx + itemPtr->x2 > dl) && (bx + itemPtr->x1 < dr) &&
			(by + itemPtr->y2 > dt) && (by + itemPtr->y1 < db))
		{
			(*itemPtr->typePtr->displayProc)(widgetPtr->interp, widgetPtr,
				itemPtr);
		}

		/* Invalidate the grids covered by the item */
/*		Widget_InvalidateArea(widgetPtr, itemPtr->minY, itemPtr->minX,
			itemPtr->maxY, itemPtr->maxX);
*/	}

	widgetPtr->dx = dl;
	widgetPtr->dy = dt;
	widgetPtr->dw = dr - dl + 1;
	widgetPtr->dh = db - dt + 1;
}

#if 0

/*
 * This routine is called to actually draw an icon into a Widget
 */
void Widget_DrawIcon(Widget *widgetPtr, int row, int col)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	BitmapPtr bitmapPtr = &widgetPtr->bitmap;
	tGridInfo *displayPtr = exPtr->optimPtr;
	IconSpec iconSpec;
	int layer, y, x;

	y = row * widgetPtr->gheight;
	x = col * widgetPtr->gwidth;

	/* Just "erase" this spot */
	if (displayPtr->blank)
	{
		iconSpec.type = ICON_TYPE_BLANK;
		DrawIconSpec(y, x, iconSpec, bitmapPtr);
		return;
	}

	/*
	 * Draw 1-4 background layers.
	 */
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		iconSpec = displayPtr->bg[layer];

		/* Stop at NONE icon */
		if (iconSpec.type == ICON_TYPE_NONE)
			break;

		/* Draw background icon */
		DrawIconSpec(y, x, iconSpec, bitmapPtr);

		/* Stop at BLANK icon */
		if (iconSpec.type == ICON_TYPE_BLANK)
			break;
	}

	/* Draw foreground icon */
	DrawIconSpec(y, x, displayPtr->fg, bitmapPtr);

	/* Draw effect icon */
	DrawIconSpec(y, x, exPtr->effect[col + row * widgetPtr->cc], bitmapPtr);
}

static bool IsTheSame(tGridInfo *gridPtr, t_display *displayPtr)
{
	int layer;

	/* Both blank */
	if (gridPtr->blank && displayPtr->blank)
		return TRUE;

	/* One is blank */
	if (gridPtr->blank || displayPtr->blank)
		return FALSE;

	/* FIXME: check tint */

	/* Different foreground */
	if (gridPtr->fg.type != displayPtr->fg.type)
		return FALSE;

	/* Different foreground */
	if ((gridPtr->fg.type != ICON_TYPE_NONE) &&
		((gridPtr->fg.index != displayPtr->fg.index) ||
		(gridPtr->fg.ascii != displayPtr->fg.ascii)))
		return FALSE;

	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		/* Different background */
		if (gridPtr->bg[layer].type != displayPtr->bg[layer].type)
			return FALSE;

		/* Stop at NONE and BLANK */
		if ((gridPtr->bg[layer].type == ICON_TYPE_NONE) ||
			(gridPtr->bg[layer].type == ICON_TYPE_BLANK))
			break;

		/* Different background */
		if ((gridPtr->bg[layer].index != displayPtr->bg[layer].index) ||
			(gridPtr->bg[layer].ascii != displayPtr->bg[layer].ascii) ||
			(gridPtr->bg[layer].dark != displayPtr->bg[layer].dark))
			return FALSE;
	}

	/* The same */
	return TRUE;
}

#endif /* 0 */

#ifdef WIDGET_STYLE_TEXT
void text_draw_all(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int tile;
	int rc = widgetPtr->rc;
	int cc = widgetPtr->cc;
	int gw = widgetPtr->gwidth;
	int gh = widgetPtr->gheight;
	DoubleLink *link;
	int y, x, yp, xp;
	BitmapPtr bitmapPtr = &widgetPtr->bitmap;

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("text_draw_all\n");

	/* Paranoia: make sure the bitmap exists */
	if (widgetPtr->bitmap.pixelPtr == NULL) return;

	/* Drawing is disabled */
	if (widgetPtr->flags & WIDGET_NO_UPDATE) return;

	widgetPtr->animCnt = 0;

	for (tile = 0; tile < cc * rc; tile++)
	{
		byte a;
		char c;

		/* This tile does not need to be redrawn */
		widgetPtr->info[tile] &= ~(WIDGET_INFO_DIRTY | WIDGET_INFO_ANIM);

		y = widgetPtr->y_min + tile / cc;
		x = widgetPtr->x_min + tile % cc;

		map_info(y, x, &a, &c);

		yp = tile / cc * gh;
		xp = tile % cc * gw;

		XFillRectangle(widgetPtr->display, bitmapPtr->pixmap, exPtr->gcBg,
			xp, yp, gw, gh);

		Tk_DrawChars(widgetPtr->display, bitmapPtr->pixmap, exPtr->gcTerm[a],
			exPtr->tkfont, &c, 1, xp, yp + exPtr->fm.ascent);
	}

	/* There are no invalid tiles */
	widgetPtr->invalidCnt = 0;

	/* Now draw all of the items for this Widget */
	for (link = widgetPtr->linkerItemVis.head; link; link = link->next)
	{
		WidgetItem *itemPtr = DoubleLink_Data(link, WidgetItem);

		(*itemPtr->typePtr->displayProc)(widgetPtr->interp, widgetPtr,
			itemPtr);
	}

	/* Set dirty bounds to entire window */
	widgetPtr->dx = widgetPtr->bx;
	widgetPtr->dy = widgetPtr->by;
	widgetPtr->dw = widgetPtr->width;
	widgetPtr->dh = widgetPtr->height;
}

void text_draw_invalid(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int i;
	int cc = widgetPtr->cc;
	int gw = widgetPtr->gwidth;
	int gh = widgetPtr->gheight;
	int y, x, yp, xp;
	DoubleLink *link;
	BitmapPtr bitmapPtr = &widgetPtr->bitmap;
	short *pinfo = widgetPtr->info;
	int dl, dt, dr, db;
	int by, bx;

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("text_draw_invalid: %d\n", widgetPtr->invalidCnt);

	/* Paranoia: make sure the bitmap exists */
	if (bitmapPtr->pixelPtr == NULL) return;

	/* Drawing is disabled */
	if (widgetPtr->flags & WIDGET_NO_UPDATE) return;

	/* Keep track of dirty area */
	dl = bitmapPtr->width;
	dt = bitmapPtr->height;
	dr = 0;
	db = 0;

	for (i = 0; i < widgetPtr->invalidCnt; i++)
	{
		int tile = widgetPtr->invalid[i];
		byte a;
		char c;

		/* This tile does not need to be redrawn */
		pinfo[tile] &= ~WIDGET_INFO_DIRTY;

		/* Cave coords */
		y = widgetPtr->y_min + tile / cc;
		x = widgetPtr->x_min + tile % cc;

		map_info(y, x, &a, &c);

		/* Bitmap coords */
		yp = (tile / cc) * gh;
		xp = (tile % cc) * gw;

		/* Dirty bounds */
		if (xp < dl)
			dl = xp;
		if (yp < dt)
			dt = yp;
		if (xp + gw - 1 > dr)
			dr = xp + gw - 1;
		if (yp + gh - 1 > db)
			db = yp + gh - 1;

		XFillRectangle(widgetPtr->display, bitmapPtr->pixmap, exPtr->gcBg,
			xp, yp, gw, gh);

		Tk_DrawChars(widgetPtr->display, bitmapPtr->pixmap, exPtr->gcTerm[a],
			exPtr->tkfont, &c, 1, xp, yp + exPtr->fm.ascent);
	}

	widgetPtr->invalidCnt = 0;

#ifdef PROJECT_HINT
	if (g_prj.y != -1)
		angtk_project(g_prj.who, g_prj.rad, g_prj.y, g_prj.x, g_prj.dam, g_prj.typ, g_prj.flg);
#endif /* PROJECT_HINT */

	/* Redraw any items inside the dirty area */
	by = widgetPtr->by;
	bx = widgetPtr->bx;
	for (link = widgetPtr->linkerItemVis.head; link; link = link->next)
	{
		WidgetItem *itemPtr = DoubleLink_Data(link, WidgetItem);

		if ((bx + itemPtr->x2 > dl) && (bx + itemPtr->x1 < dr) &&
			(by + itemPtr->y2 > dt) && (by + itemPtr->y1 < db))
		{
			(*itemPtr->typePtr->displayProc)(widgetPtr->interp, widgetPtr,
				itemPtr);
		}
	}

	widgetPtr->dx = dl;
	widgetPtr->dy = dt;
	widgetPtr->dw = dr - dl + 1;
	widgetPtr->dh = db - dt + 1;
}
#endif /* WIDGET_STYLE_TEXT */

int widget_configure(Widget *widgetPtr, int error, int mask)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
#ifdef WIDGET_STYLE_TEXT
	int i;
	XGCValues gcValues;
	unsigned long gcMask;
	Tk_Window tkwin = widgetPtr->tkwin;
#endif

	if (error)
		return TCL_OK;

	/* Valid micro-map sizes: 4, 6, 8 */
	if (widgetPtr->style == WIDGET_STYLE_MAP)
	{
		widgetPtr->drawAllProc = map_draw_all;
		widgetPtr->drawInvalidProc = map_draw_invalid;
		widgetPtr->hitTestProc = NULL;
		exPtr->whatToDrawProc = NULL;
		exPtr->symbolProc = map_symbol_proc;
	}

	/* Make this Widget draw icons */
	if (widgetPtr->style == WIDGET_STYLE_ICON)
	{
		widgetPtr->drawAllProc = widget_draw_all;
		widgetPtr->drawInvalidProc = widget_draw_invalid;
		widgetPtr->hitTestProc = NULL;
		exPtr->whatToDrawProc = widget_wtd;
		exPtr->symbolProc = NULL;
	}

	/* Make this Widget draw isometric icons */
	if (widgetPtr->style == WIDGET_STYLE_ISO)
	{
		widgetPtr->drawAllProc = iso_draw_all;
		widgetPtr->drawInvalidProc = iso_draw_invalid;
		widgetPtr->hitTestProc = iso_hittest;
		exPtr->whatToDrawProc = iso_wtd;
		exPtr->symbolProc = NULL;
	}

	/*
	 * This allows a widget to display the icons in a vault created
	 * by the "vault" command. I was thinking about a vault/wilderness
	 * editor.
	 */
	exPtr->vaultPtr = NULL;
	if ((exPtr->vaultNum > 0) && (widgetPtr->style != WIDGET_STYLE_MAP))
	{
		exPtr->whatToDrawProc = vault_wtd;
		Widget_SetVault(widgetPtr);
	}

	else if ((exPtr->vaultNum > 0) && (widgetPtr->style == WIDGET_STYLE_MAP))
	{
		widgetPtr->drawAllProc = map_draw_all;
		widgetPtr->drawInvalidProc = map_draw_invalid;
		widgetPtr->hitTestProc = NULL;
		exPtr->symbolProc = vault_symbol_proc;
		Widget_SetVault(widgetPtr);
	}

#ifdef WIDGET_STYLE_TEXT
	/* Make this Widget draw text */
	if (widgetPtr->style == WIDGET_STYLE_TEXT)
	{
		widgetPtr->drawAllProc = text_draw_all;
		widgetPtr->drawInvalidProc = text_draw_invalid;
		exPtr->whatToDrawProc = NULL;
		exPtr->symbolProc = NULL;
	}

	if (mask & EXWIDGET_CONF_FONT)
	{
		exPtr->fm.ascent = 0;
		if (exPtr->gcFg != None)
		{
			Tk_FreeGC(widgetPtr->display, exPtr->gcFg);
			exPtr->gcFg = None;
		}
		if (exPtr->gcBg != None)
		{
			Tk_FreeGC(widgetPtr->display, exPtr->gcBg);
			exPtr->gcBg = None;
		}
		for (i = 0; i < 16; i++)
		{
			if (exPtr->gcTerm[i] != None)
			{
				Tk_FreeGC(widgetPtr->display, exPtr->gcTerm[i]);
				exPtr->gcTerm[i] = None;
			}
		}
	}

	if (exPtr->gcFg == None)
	{
		gcValues.foreground = WhitePixelOfScreen(Tk_Screen(tkwin));
		gcValues.background = BlackPixelOfScreen(Tk_Screen(tkwin));
		gcValues.graphics_exposures = False;
		exPtr->gcFg = Tk_GetGC(tkwin,
			GCForeground | GCBackground,
			&gcValues);
	}

	if (exPtr->gcBg == None)
	{
		gcValues.background = BlackPixelOfScreen(Tk_Screen(tkwin));
		gcValues.graphics_exposures = False;
		exPtr->gcBg = Tk_GetGC(tkwin,
			GCBackground,
			&gcValues);
	}

	if (exPtr->gcTerm[0] == None)
	{
		int i;

		for (i = 0; i < 16; i++)
		{
			byte rv, gv, bv;

			rv = angband_color_table[i][1];
			gv = angband_color_table[i][2];
			bv = angband_color_table[i][3];

			gcValues.foreground = Plat_RGB2XPixel(rv, gv, bv);
			gcValues.font = Tk_FontId(exPtr->tkfont);
			gcValues.graphics_exposures = False;
			gcMask = GCForeground | GCFont | GCGraphicsExposures;
			exPtr->gcTerm[i] = Tk_GetGC(tkwin, gcMask, &gcValues);
		}
	}

	if (exPtr->fm.ascent == 0)
	{
		Tk_GetFontMetrics(exPtr->tkfont, &exPtr->fm);
	}
#endif /* WIDGET_STYLE_TEXT */

	return TCL_OK;
}

void widget_changed(Widget *widgetPtr)
{
#ifndef GRID_EFFECT
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int i;

	if ((exPtr->effect != NULL) &&
		((widgetPtr->tc != widgetPtr->oldTileCnt)))
	{
		Tcl_FreeDebug((char *) exPtr->effect);
		exPtr->effect = NULL;
if (debug_widgets & DEBUG_WIDGET_CONFIG) dbwin("widget_changed: free mem\n");
	}

	/*  */
	if ((widgetPtr->style != WIDGET_STYLE_MAP) && (exPtr->effect == NULL))
	{
#if defined(WIDGET_EFFECT)
		exPtr->effect = Tcl_AllocDebug(sizeof(ExWidgetEffect) * widgetPtr->tc);
		for (i = 0; i < widgetPtr->tc; i++)
		{
			exPtr->effect[i].icon.type = ICON_TYPE_NONE;
			exPtr->effect[i].flags = 0;
		}
#else
		exPtr->effect = Tcl_AllocDebug(sizeof(IconSpec) * widgetPtr->tc);
		for (i = 0; i < widgetPtr->tc; i++)
		{
			exPtr->effect[i].type = ICON_TYPE_NONE;
		}
#endif
if (debug_widgets & DEBUG_WIDGET_CONFIG) dbwin("widget_changed: alloc mem\n");
	}
#endif /* GRID_EFFECT */
}

void widget_destroy(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	MapBlinker *blinkPtr = exPtr->blinkPtr;
#ifdef WIDGET_STYLE_TEXT
	int i;
#endif

#ifndef GRID_EFFECT
	if (exPtr->effect)
		Tcl_FreeDebug((char *) exPtr->effect);
#endif

	while (blinkPtr != NULL)
	{
		MapBlinker *nextPtr = blinkPtr->next;
		MapBlinker_Free(blinkPtr);
		blinkPtr = nextPtr;
	}

#ifdef WIDGET_STYLE_TEXT
	if (exPtr->gcFg != None)
		Tk_FreeGC(widgetPtr->display, exPtr->gcFg);
	if (exPtr->gcBg != None)
		Tk_FreeGC(widgetPtr->display, exPtr->gcBg);
	for (i = 0; i < 16; i++)
	{
		if (exPtr->gcTerm[i] != None)
			Tk_FreeGC(widgetPtr->display, exPtr->gcTerm[i]);
	}
#endif
}

/*
 * Allocate storage for a new Widget.
 */
int widget_create(Tcl_Interp *interp, Widget **ptr)
{
	ExWidget *exPtr = (ExWidget *) Tcl_AllocDebug(sizeof(ExWidget));
	Widget *widgetPtr = (Widget *) exPtr;

if (debug_widgets & DEBUG_WIDGET_CONFIG) dbwin("widget_create\n");

	widgetPtr->centerProc = NULL;
	widgetPtr->configureProc = widget_configure;
	widgetPtr->changedProc = widget_changed;
	widgetPtr->destroyProc = widget_destroy;
	widgetPtr->drawInvalidProc = NULL;
	widgetPtr->hitTestProc = NULL;
	widgetPtr->wipeProc = NULL;
	widgetPtr->invalidateProc = NULL;
	widgetPtr->invalidateAreaProc = NULL;
	exPtr->vaultNum = 0;
	exPtr->whatToDrawProc = NULL;
	exPtr->symbolProc = NULL;
#ifndef GRID_EFFECT
	exPtr->effect = NULL;
#endif
#ifdef PLAYER_HEALTH_BAR
	exPtr->healthBar.drawn = FALSE;
	exPtr->healthBar.firstColorPtr = NULL;
	exPtr->healthBar.secondColorPtr = NULL;
#endif
	exPtr->blink = FALSE;
	exPtr->blinkPtr = NULL;
	exPtr->blink1ColorPtr = NULL;
	exPtr->blink2ColorPtr = NULL;
	exPtr->blink3ColorPtr = NULL;
#ifdef WIDGET_STYLE_TEXT
	exPtr->gcTerm[0] = None;
	exPtr->gcFg = exPtr->gcBg = None;
	exPtr->tkfont = None;
	exPtr->fm.ascent = 0;
#endif

	(*ptr) = widgetPtr;

	return TCL_OK;
}

#if 0
int WidgetProc(Tcl_Interp *interp, Widget *widgetPtr, int message, long param)
{
	switch (message)
	{
		WIDGET_MSG_INIT:
			break;
		WIDGET_MSG_DESTROY:
			break;
		default:
			return TCL_OK;
	}
}
#endif

void widget_setup(void)
{
	DoubleLink *link;

	/* When the game's icon configuration changes, go through all the
	 * Widgets and update -gwidth/-gheight/-style */
	for (link = WidgetList.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);
		if (widgetPtr->style == WIDGET_STYLE_MAP) continue;
		/* FIXME: Use Tcl_GetCommandFullName() */
		Tcl_EvalEx(g_interp, format("%s configure -gwidth %d -gheight %d -style %s",
			Tk_PathName(widgetPtr->tkwin), g_icon_width, g_icon_height,
			(g_icon_style == ICON_STYLE_ISO) ? "iso" : "icon"),
			-1, TCL_EVAL_GLOBAL);
	}
}

int widget_init(void)
{
	if (Widget_Init(g_interp, widget_create) != TCL_OK)
		return TCL_ERROR;
	Widget_AddOptions(g_interp, extraOptions);
	return TCL_OK;
}

void widget_exit(void)
{
	/* Danger! Widgets aren't freed yet */
	/* They are freed later when the Tk exit handler executes */
	Widget_Exit(g_interp);
}

void angtk_widget_lock(bool lock)
{
	DoubleLink *link;
	Widget *widgetPtr;

	if (lock)
	{
		for (link = WidgetList.head; link; link = link->next)
		{
			widgetPtr = DoubleLink_Data(link, Widget);
			widgetPtr->flags |= WIDGET_NO_UPDATE;
		}
	}
	else
	{
		for (link = WidgetList.head; link; link = link->next)
		{
			widgetPtr = DoubleLink_Data(link, Widget);
			if (!widgetPtr->noUpdate)
			{
				widgetPtr->flags &= ~WIDGET_NO_UPDATE;
Widget_Wipe(widgetPtr);
			}
		}
	}
}

/*
 * Now for spell and missile effects in a Widget. The basic idea is to
 * prepare for drawing, perform the drawing (offscreen), display
 * the changed grids onscreen, then repair the affected grids offscreen.
 *
 * Specifically, we call angtk_effect_prep(), then another routine to
 * draw any number of spell/missile grids offscreen, and finally
 * angtk_effect_fresh() to display the changes onscreen. This allows us
 * to draw each radius of a spell explosion in turn.
 *
 * Because a Widget may display an arbitrary area of the cave, and because
 * a particular Widget may not be mapped (ie, visible onscreen), we keep
 * track of which Widgets were really drawn into and require redisplaying.
 */

/*
 * Return EFFECT_SPELL_XXX constant for given GF_XXX constant. For each
 * EFFECT_SPELL_XXX we have a bolt icon assignment and a ball icon assignment.
 * The bolt assignment is assumed to be the first of four icons.
 */
static byte effect_index(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_MISSILE:	return (EFFECT_SPELL_MISSILE);
		case GF_ACID:		return (EFFECT_SPELL_ACID);
		case GF_ELEC:		return (EFFECT_SPELL_ELEC);
		case GF_FIRE:		return (EFFECT_SPELL_FIRE);
		case GF_COLD:		return (EFFECT_SPELL_COLD);
		case GF_POIS:		return (EFFECT_SPELL_POIS);
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		case GF_HOLY_ORB:	return (EFFECT_SPELL_HOLY_ORB);
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
		case GF_HOLY_FIRE:	return (EFFECT_SPELL_HOLY_FIRE);
		case GF_HELL_FIRE:	return (EFFECT_SPELL_HELL_FIRE);
#endif /* ZANGBANDTK */
		case GF_MANA:		return (EFFECT_SPELL_MANA);
		case GF_ARROW:		return (EFFECT_SPELL_ARROW);
		case GF_WATER:		return (EFFECT_SPELL_WATER);
		case GF_NETHER:		return (EFFECT_SPELL_NETHER);
		case GF_CHAOS:		return (EFFECT_SPELL_CHAOS);
		case GF_DISENCHANT:	return (EFFECT_SPELL_DISENCHANT);
		case GF_NEXUS:		return (EFFECT_SPELL_NEXUS);
		case GF_CONFUSION:	return (EFFECT_SPELL_CONFUSION);
		case GF_SOUND:		return (EFFECT_SPELL_SOUND);
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		case GF_SHARD:		return (EFFECT_SPELL_SHARD);
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
		case GF_SHARDS:		return (EFFECT_SPELL_SHARD);
#endif /* ZANGBANDTK */
		case GF_FORCE:		return (EFFECT_SPELL_FORCE);
		case GF_INERTIA:	return (EFFECT_SPELL_INERTIA);
		case GF_GRAVITY:	return (EFFECT_SPELL_GRAVITY);
		case GF_TIME:		return (EFFECT_SPELL_TIME);
		case GF_LITE_WEAK:	return (EFFECT_SPELL_LITE_WEAK);
		case GF_LITE:		return (EFFECT_SPELL_LITE);
		case GF_DARK_WEAK:	return (EFFECT_SPELL_DARK_WEAK);
		case GF_DARK:		return (EFFECT_SPELL_DARK);
		case GF_PLASMA:		return (EFFECT_SPELL_PLASMA);
		case GF_METEOR:		return (EFFECT_SPELL_METEOR);
		case GF_ICE:		return (EFFECT_SPELL_ICE);
#if defined(ZANGBANDTK)
		case GF_ROCKET:		return (EFFECT_SPELL_ROCKET);
		case GF_DEATH_RAY:	return (EFFECT_SPELL_DEATH_RAY);
		case GF_NUKE:		return (EFFECT_SPELL_NUKE);
		case GF_DISINTEGRATE:	return (EFFECT_SPELL_DISINTEGRATE);
		case GF_PSI:
		case GF_PSI_DRAIN:
		case GF_TELEKINESIS:
		case GF_DOMINATION:		return (EFFECT_SPELL_PSI);
#endif /* ZANGBANDTK */
	}

	/* Standard "color" */
	return (EFFECT_SPELL_FORCE);
}

/* #define SPRITE_EFFECT */
#ifdef SPRITE_EFFECT
static icon s_effect_icon; /* The icon for the current effect */
#endif

bool angtk_effect_aux(int y, int x, IconSpec *iconSpecPtr)
{
	Widget *widgetPtr;
	ExWidget *exPtr;
	DoubleLink *link;
	int row, col;
	bool drawn = FALSE;

	/* Paranoia */
	iconSpecPtr->dark = 0;

#ifdef GRID_EFFECT
	/* Remember the effect icon */
	g_grid_effect[y][x].icon = *iconSpecPtr;
#endif

	/* Check each Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		widgetPtr = DoubleLink_Data(link, Widget);
		exPtr = (ExWidget *) widgetPtr;

		/* Don't draw in micro-map */
		if (widgetPtr->style == WIDGET_STYLE_MAP) continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE) continue;

		/* Don't draw out of bounds */
		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

#ifdef GRID_EFFECT
#elif defined(WIDGET_EFFECT)
		/* Set effect icon */
		exPtr->effect[col + row * widgetPtr->cc].icon = *iconSpecPtr;
#else
		/* Set effect icon */
		exPtr->effect[col + row * widgetPtr->cc] = *iconSpecPtr;
#endif

		/* Mark the grid as invalid */
		Widget_Invalidate(widgetPtr, row, col);

		/* Redraw later */
		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);

		/* Something was drawn */
		drawn = TRUE;
	}

	return drawn;
}

void angtk_effect_clear(int y, int x)
{
	Widget *widgetPtr;
	ExWidget *exPtr;
	DoubleLink *link;
	int row, col;
#ifdef WIDGET_EFFECT
	ExWidgetEffect *effectPtr;
#endif

#ifdef GRID_EFFECT
	/* Clear the effect icon */
	g_grid_effect[y][x].icon.type = ICON_TYPE_NONE;
#endif

	/* Check each Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		widgetPtr = DoubleLink_Data(link, Widget);
		exPtr = (ExWidget *) widgetPtr;

		/* Don't draw in micro-map */
		if (widgetPtr->style == WIDGET_STYLE_MAP) continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE) continue;

		/* Don't draw out of bounds */
		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

#ifdef GRID_EFFECT
#elif defined(WIDGET_EFFECT)
		effectPtr = &exPtr->effect[col + row * widgetPtr->cc];

		/* No effect was drawn here */
		if (effectPtr->icon.type == ICON_TYPE_NONE)
			continue;

		/* Clear effect icon */
		effectPtr->icon.type = ICON_TYPE_NONE;
#else
		/* No effect was drawn here */
		if (exPtr->effect[col + row * widgetPtr->cc].type == ICON_TYPE_NONE)
			continue;

		/* Clear effect icon */
		exPtr->effect[col + row * widgetPtr->cc].type = ICON_TYPE_NONE;
#endif

		/* Mark the grid as invalid */
		Widget_Invalidate(widgetPtr, row, col);

		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);
	}
}

#ifdef SPRITE_EFFECT
void angtk_effect_delay(void)
{
	if (s_effect_icon.type == TYPE_SPRITE)
	{
		t_sprite *sprite_ptr = &g_sprite[s_effect_icon.index];
		int frame, count = sprite_ptr->count;
		for (frame = 0; frame < count; frame++)
		{
			t_icon icon = &sprite_ptr->icon[frame];
			for (widgetPtr = WidgetListMapped.head; widgetPtr; widgetPtr = widgetPtr->linkMapped.next)
			{
				int y, x;
				if (!(widgetPtr->flags & WIDGET_FLAG_DRAW)) continue;
				for (y = y_min; y < y_max; y++)
				{
					for (x = x_min; x < x_max; x++)
					{
						if (GridGet(&widgetPtr->grid, y - y_min, x - x_min).flag & GRID_FLAG_EFFECT)
						{
							/* draw sprite frame here */
						}
					}
				}
				Widget_Display((ClientData) widgetPtr);
				Widget_DrawInvalid(widgetPtr);
			}
			Term_xtra(TERM_XTRA_DELAY, sprite_ptr->speed);
		}
	}
	else
	{
		int msec = op_ptr->delay_factor * op_ptr->delay_factor;
		Term_xtra(TERM_XTRA_DELAY, msec);
	}
}
#endif /* SPRITE_EFFECT */

/*
 * Draws the icon assigned to a particular spell type (GF_XXX constant).
 */
bool angtk_effect_spell(int y, int x, int typ, int bolt)
{
	int effect = effect_index(typ);
	IconSpec iconSpec;

	if (bolt)
	{
		iconSpec = g_effect[EFFECT_SPELL_BOLT].icon[effect];

		/*
		 * 1: ns, 2: we, 3: sw-ne, 4: nw-se
		 */
		if (g_icon_style == ICON_STYLE_ISO)
		{
			int xform[4] = { 3, 4, 2, 1 };
			bolt = xform[bolt - 1];
		}

		iconSpec.index += bolt - 1;
	}
	else
	{
		iconSpec = g_effect[EFFECT_SPELL_BALL].icon[effect];
	}

	return angtk_effect_aux(y, x, &iconSpec);
}

/*
 * Draws the icon assigned to a fired ammunition
 */
bool angtk_effect_ammo(int y, int x, object_type *o_ptr, int dir)
{
	IconSpec iconSpec;

	/* Eliminate '5' */
	if (dir >= 5) dir -= 1;

	/*
	 * 1: sw, 2: s, 3: se, 4: w, 5: e, 6: nw, 7: n, 8: ne
	 */
	if (g_icon_style == ICON_STYLE_ISO)
	{
		int xform[8] = { 4, 1, 2, 6, 3, 7, 8, 5 };
		dir = xform[dir - 1];
	}

	switch (k_info[o_ptr->k_idx].tval)
	{
		case TV_ARROW:
			iconSpec = g_effect[EFFECT_AMMO].icon[EFFECT_AMMO_ARROW];
			iconSpec.index += dir - 1;
			break;

		case TV_BOLT:
			iconSpec = g_effect[EFFECT_AMMO].icon[EFFECT_AMMO_BOLT];
			iconSpec.index += dir - 1;
			break;

		/* Sling ammo */
		default:
			return angtk_effect_object(y, x, o_ptr);
	}

	/* Nothing is assigned */
	if (iconSpec.type == ICON_TYPE_DEFAULT)
	{
		return angtk_effect_object(y, x, o_ptr);
	}

	return angtk_effect_aux(y, x, &iconSpec);
}

/*
 * Draws the icon assigned to thrown object
 */
bool angtk_effect_object(int y, int x, object_type *o_ptr)
{
	t_assign assign = g_assign[ASSIGN_OBJECT].assign[o_ptr->k_idx];
	IconSpec iconSpec;

	FinalIcon(&iconSpec, &assign, 0, o_ptr, NULL);
	return angtk_effect_aux(y, x, &iconSpec);
}

/*
 * Performs actions at idle time
 */
void angtk_idle(void)
{
	DoubleLink *link;
	int i;

	map_update_blinkers();

	/* Calculate the current frame for sprites */
	if (update_sprites() == 0) return;

	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* This Widget doesn't have any animated grids */
		if (!widgetPtr->animCnt) continue;

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("animCnt is %d\n", widgetPtr->animCnt);

		/*
		 * It is possible that some grids which don't display sprites
		 * are in the array, but that's okay for now.
		 */
		for (i = 0; i < widgetPtr->animCnt; i++)
		{
			int tile = widgetPtr->anim[i];
			if (!(widgetPtr->info[tile] & WIDGET_INFO_DIRTY))
			{
				widgetPtr->invalid[widgetPtr->invalidCnt++] = tile;
				widgetPtr->info[tile] |= WIDGET_INFO_DIRTY;
			}
		}

		/* A redraw was scheduled */
		if (widgetPtr->flags & WIDGET_REDRAW)
		{
			/* Cancel the scheduled redraw */
			Tcl_CancelIdleCall(Widget_Display, (ClientData) widgetPtr);
		}

		/* Redraw invalidated grids (the sprites) */
		widgetPtr->flags |= WIDGET_DRAW_INVALID;

		/* Update grids, and copy to screen */
		Widget_Display((ClientData) widgetPtr);
	}

	/* Redisplay sprites in all Widget-type Canvas items */
	CanvasWidget_Idle();

	TreeCtrl_Idle();
}

/*
 * This is a dummy lite_spot() routine that may get called before
 * the icons have been initialized.
 */
void angtk_lite_spot_dummy(int y, int x)
{
	/* Nothing */
}

void (*angtk_lite_spot)(int y, int x) = angtk_lite_spot_dummy;

/*
 * This is called whenever the game thinks a grid needs to be redrawn,
 * via lite_spot(). A redraw is scheduled in every Widget if needed.
 */
void angtk_lite_spot_real(int y, int x)
{
	DoubleLink *link;
	int row, col;

	/* Get knowledge about location */
	get_grid_info(y, x, &g_grid[y][x]);

	/* Update the global array of map symbols */
	map_symbol_set(y, x);

	/* Check each Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		/* A full redraw is already pending */
		if (widgetPtr->flags & WIDGET_WIPE)
			continue;

		/* Cave location isn't visible */
		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* Mark the location as invalid */
		Widget_Invalidate(widgetPtr, row, col);

		/* Redraw invalid grids later */
		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);
	}

	if (y == g_track_grid_y && x == g_track_grid_x)
	{
		Bind_Track(KEYWORD_TRACK_GRID + 1, 0, y, x);
	}
}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

/*
 * Marks the given cave location as invalid in every mapped Widget
 */
void angtk_wipe_spot(int y, int x)
{
	DoubleLink *link;
	int row, col;

	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		/* A full redraw is already pending */
		if (widgetPtr->flags & WIDGET_WIPE)
			continue;

		/* Don't draw out of bounds */
		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* */
		Widget_Invalidate(widgetPtr, row, col);

		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);
	}
}

#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

/*
 * Mark a cave location for redraw in all icon-drawing mapped widgets.
 */
void widget_invalidate_shape(int y, int x)
{
	int row, col;
	DoubleLink *link;

/*	dbwin("widget_invalidate_shape %d %d\n", y, x);*/

	/* Check each mapped Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Skip micro-map Widgets */
		if (widgetPtr->style == WIDGET_STYLE_MAP)
			continue;

		/* Drawing is disabled */
		if (widgetPtr->flags & WIDGET_NO_UPDATE)
			continue;

		/* Displaying a vault */
		if (((ExWidget *) widgetPtr)->vaultNum)
			continue;

		if (!Widget_CaveToView(widgetPtr, y, x, &row, &col))
			continue;

		/* Invalidate the grid, so it will be redrawn later */
		Widget_Invalidate(widgetPtr, row, col);

		/* Redraw later */
		widgetPtr->flags |= WIDGET_DRAW_INVALID;
		Widget_EventuallyRedraw(widgetPtr);
	}
}

/*
 * Visual effect: Draw a circle centered at y,x of radius r.
 * Used for detection spells, earthquake, etc.
 */
static void Widget_DetectRadius(Widget *widgetPtr, GC gc, int y, int x, int r)
{
	int ymin = widgetPtr->y_min;
	int xmin = widgetPtr->x_min;
	int x2, y2;

	x2 = /*widgetPtr->bx +*/ (x - r - xmin) * widgetPtr->gwidth + widgetPtr->gwidth / 2;
	y2 = /*widgetPtr->by +*/ (y - r - ymin) * widgetPtr->gheight + widgetPtr->gheight / 2;

	XDrawArc(widgetPtr->display, widgetPtr->bitmap.pixmap, gc, x2, y2,
		r * 2 * widgetPtr->gwidth, r * 2 * widgetPtr->gheight, 0, 360 * 64);
}

/*
 * Visual effect: Draw a circle centered at y,x of radius r.
 * Used for detection spells, earthquake, etc.
 */
void angtk_detect_radius(int y, int x, int r)
{
	static s32b s_detect_turn = 0L;
	Tk_Window tkwin = Tk_MainWindow(g_interp);
	XGCValues gcValues;
	XColor *color;
	GC gc;
	DoubleLink *link;
	int dist;

#ifdef ALLOW_BORG
	int noUpdate = 0, count = 0;
#endif /* ALLOW_BORG */

	/* Hack -- Avoid multiple flashes per spell */
	if (turn == s_detect_turn) return;
	s_detect_turn = turn;

#ifdef ALLOW_BORG

	/* Count widgets and disabled widgets */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);
		if (widgetPtr->flags & WIDGET_NO_UPDATE) noUpdate++;
		count++;
	}

	/* Don't delay if no Widgets are enabled */
	/* This is a hack for the Borg if the interface is detached */
	if (count == noUpdate) return;

#endif /* ALLOW_BORG */

	/* Get gray pixel value */
	color = Tk_GetColor(NULL, tkwin, Tk_GetUid("gray"));
	if (color == NULL)
	{
		dbwin("Tk_GetColor failed!\n");
		return;
	}

	/* Get a graphics context for drawing */
	gcValues.foreground = color->pixel;
	gcValues.line_width = 2;
	gc = Tk_GetGC(tkwin, GCForeground | GCLineWidth, &gcValues);

	/*
	 * Draw "radius = dist * dist", and always draw "radius = r".
	 */
	dist = 2;
	while (1)
	{
		int r2 = dist * dist;
		unsigned long msStart;

		if (r2 > r)
		{
			r2 = r;
		}

		msStart = Milliseconds();

		/* Draw the effect */
		for (link = WidgetListMapped.head; link; link = link->next)
		{
			Widget *widgetPtr = DoubleLink_Data(link, Widget);

			if (widgetPtr->flags & WIDGET_NO_UPDATE) continue;
			if (widgetPtr->style != WIDGET_STYLE_MAP) continue;

			Widget_DetectRadius(widgetPtr, gc, y, x, r2);
			Widget_Display((ClientData) widgetPtr);
		}

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, 30 - (Milliseconds() - msStart));

		/* Repair offscreen pixmap */
		for (link = WidgetListMapped.head; link; link = link->next)
		{
			Widget *widgetPtr = DoubleLink_Data(link, Widget);

			if (widgetPtr->flags & WIDGET_NO_UPDATE) continue;
			if (widgetPtr->style != WIDGET_STYLE_MAP) continue;

			Widget_DrawAll(widgetPtr);

			if (r2 == r)
				Widget_Display((ClientData) widgetPtr);
		}

		/* Stop */
		if (r2 == r) break;

		++dist;
	}

	/* Cleanup */
	Tk_FreeGC(Tk_Display(tkwin), gc);
	Tk_FreeColor(color);
}

/*
 * A big silly routine for a big silly visual effect. When arg=1, every
 * icon-drawing mapped Widget is randomly recentered and redisplayed
 * a number of times. When arg=2, every icon-drawing mapped widget is
 * totally erased to white and a display is scheduled. When arg=3 every
 * icon-drawing mapped widget is wiped and a redraw is scheduled (to
 * erase the white when arg=2).
 */
void angtk_destroy_area(int arg)
{
	int i, cy, cx;
	int offset_y[16] = {0, -1, 1, -1, 1, 1, 0, -1, 0, -1, 1, -1, 1, 1, 0, -1};
	int offset_x[16] = {-1, 1, 0, 0, 1, -1, 1, -1, -1, 1, 0, 0, 1, -1, 1, -1};
	unsigned long msStart, msEnd;
	DoubleLink *link;

	/*
	 * Mega-Hack -- Update everything. I noticed that the smarty-button
	 * popups were not withdrawn before the effect took place.
	 */
	Term_fresh();

	/* Check each Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);

		/* Skip non-icon-drawing Widgets */
		if (widgetPtr->style == WIDGET_STYLE_MAP) continue;

		/* Step 1: Shake n' bake */
		if (arg == 1)
		{
			/* Drawing is disabled */
			if (widgetPtr->flags & WIDGET_NO_UPDATE) continue;

			cy = widgetPtr->y;
			cx = widgetPtr->x;
			for (i = 0; i < 16; i++)
			{
				msStart = Milliseconds();
				Widget_Center(widgetPtr, cy + offset_y[i], cx + offset_x[i]);
				Widget_Display((ClientData) widgetPtr);
				msEnd = Milliseconds();
				Term_xtra(TERM_XTRA_DELAY, 70 - (msEnd - msStart));
			}
			Widget_Center(widgetPtr, cy, cx);
		}

		/* Step 2: Blast of light */
		else if (arg == 2)
		{
			XRectangle rect;
			XGCValues gcValues;
			GC gc;

			rect.x = widgetPtr->bx;
			rect.y = widgetPtr->by;
			rect.width = widgetPtr->width;
			rect.height = widgetPtr->height;

			gcValues.foreground = WhitePixelOfScreen(Tk_Screen(widgetPtr->tkwin));
			gc = Tk_GetGC(widgetPtr->tkwin, GCForeground, &gcValues);

			XFillRectangles(widgetPtr->display,
				widgetPtr->bitmap.pixmap, gc,
				&rect, 1);

			Tk_FreeGC(widgetPtr->display, gc);

			/*
			 * Eventually redisplay the widget, but do NOT update the grids.
			 * That is, the redisplay should show us the white field we
			 * painted above. NOTE: angtk_widget_lock() is called for us.
			 */
			widgetPtr->flags &= ~(WIDGET_DRAW_INVALID | WIDGET_WIPE);
			Widget_EventuallyRedraw(widgetPtr);
		}

		/* Step 3: Redraw later */
		else if (arg == 3)
		{
			/* Drawing is disabled */
			if (widgetPtr->flags & WIDGET_NO_UPDATE) continue;

			Widget_Wipe(widgetPtr);
		}

		/*
		 * XXX Mega-Hack -- Only actually draw into the first suitable
		 * Widget in the list. If we had two or more suitable Widgets
		 * we would need to rewrite the loops to make it work.
		 */
		break;
	}
}

/*
 * do_cmd_locate() helper
 */
void angtk_locate(int dir)
{
	DoubleLink *link;
	int y, x;

	/* Check each Widget */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);
		int rc = widgetPtr->rc;
		int cc = widgetPtr->cc;

		/* Skip non-icon-drawing Widgets */
		if (widgetPtr->style == WIDGET_STYLE_MAP) continue;

		if (dir == -1)
		{
			Widget_Center(widgetPtr, p_ptr_py, p_ptr_px);
			break;
		}

		y = widgetPtr->y;
		x = widgetPtr->x;

#ifdef WIDGET_STYLE_TEXT
		if (widgetPtr->style == WIDGET_STYLE_ICON ||
			widgetPtr->style == WIDGET_STYLE_TEXT)
#else
		if (widgetPtr->style == WIDGET_STYLE_ICON)
#endif
		{
			if (ddy[dir] < 0)
			{
				if (widgetPtr->y_min > -1)
				{
					y = widgetPtr->y_min;
					if (y - rc / 2 < -1)
						y = rc / 2 - 1;
				}
			}
			if (ddy[dir] > 0)
			{
				if (widgetPtr->y_max - 1 < g_cave_hgt)
				{
					y = widgetPtr->y_max - 1;
					if (y - rc / 2 + rc > g_cave_hgt)
						y = g_cave_hgt - rc + rc / 2 + 1;
				}
			}
			if (ddx[dir] < 0)
			{
				if (widgetPtr->x_min > -1)
				{
					x = widgetPtr->x_min;
					if (x - cc / 2 < -1)
						x = cc / 2 - 1;
				}
			}
			if (ddx[dir] > 0)
			{
				if (widgetPtr->x_max - 1 < g_cave_wid)
				{
					x = widgetPtr->x_max - 1;
					if (x - cc / 2 + cc > g_cave_wid)
						x = g_cave_wid - cc + cc / 2 + 1;
				}
			}
		}

		if (widgetPtr->style == WIDGET_STYLE_ISO)
		{
#if 1
			y += ddy[dir] * 10;
			x += ddx[dir] * 10;
			if (ddy[dir] < 0)
			{
				if (y < 0)
					y = 0;
			}
			if (ddy[dir] > 0)
			{
				if (y >= g_cave_hgt)
					y = g_cave_hgt - 1;
			}
			if (ddx[dir] < 0)
			{
				if (x < 0)
					x = 0;
			}
			if (ddx[dir] > 0)
			{
				if (x >= g_cave_wid)
					x = g_cave_wid - 1;
			}
#else
			int isoy, isox, isohgt, isowid;
			int y1, x1, y2, x2, isoy1, isox1, isoy2, isox2;

			/* Get cave coords of top-left tile */
			y1 = widgetPtr->y0;
			x1 = widgetPtr->x0;

			/* Get cave coords of bottom-right tile */
			y2 = y1 + widgetPtr->yo[rc * cc - 1];
			x2 = x1 + widgetPtr->xo[rc * cc - 1];

			/* Cave x,y -> Iso x-y,x+y */
			isoy1 = x1 + y1;
			isox1 = x1 - y1;

			/* Cave x,y -> Iso x-y,x+y */
			isoy2 = x2 + y2;
			isox2 = x2 - y2;

			/* Cave x,y -> Iso x-y,x+y */
			isoy = x + y;
			isox = x - y;

			/* Make zero-based */
			isox1 += g_cave_hgt - 1;
			isox2 += g_cave_hgt - 1;
			isox += g_cave_hgt - 1;

			isohgt = isowid = g_cave_hgt + g_cave_wid;

			if (ddy[dir] < 0)
			{
				if (isoy1 > -1)
				{
					isoy = isoy1;
//					if (isoy - rc / 2 < -1)
//						isoy = rc / 2 - 1;
				}
			}
			if (ddy[dir] > 0)
			{
				if (isoy2 < isohgt)
				{
					isoy = isoy2;
//					if (isoy - rc / 2 + rc > isohgt)
//						isoy = isohgt - rc + rc / 2 + 1;
				}
			}
			if (ddx[dir] < 0)
			{
				if (isox1 > -1)
				{
					isox = isox1;
//					if (isox - cc / 2 < -1)
//						isox = cc / 2 - 1;
				}
			}
			if (ddx[dir] > 0)
			{
				if (isox2 < isowid)
				{
					isox = isox2;
//					if (isox - cc / 2 + cc > isowid)
//						isox = isowid - cc + cc / 2 + 1;
				}
			}

			/* Undo zero-based */
			isox -= g_cave_hgt - 1;

			y = (isoy - isox) / 2;
			x = (isoy + isox) / 2;
#endif /* 0 */
		}

		Widget_Center(widgetPtr, y, x);

		/* Stop after first Widget */
		break;
	}
}
