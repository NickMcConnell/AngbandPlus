/* File: widget-iso.c */

/* Purpose: Isometric stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include <tkInt.h> /* This is after to avoid Unix warning about NBBY */
#include "tnb.h"
#include "util-dll.h"
#include "icon.h"
#include "widget.h"

void iso_wtd(Widget *widgetPtr, int y, int x, t_display *wtd);

int PointToTile(Widget *widgetPtr, int x, int y, int *colPtr, int *rowPtr,
	int *xcPtr, int *ycPtr);
int PointInIcon(Widget *widgetPtr, int x, int y, IconSpec *iconSpecPtr);
int HitTestTile(Widget *widgetPtr, int x, int y, int col, int row,
	int xc, int yc);

extern void Widget_InvertSpot(Widget *widgetPtr, int row, int col, t_display *wtd);

#ifdef PIXAR

struct Pixar
{
	int xfb, xlr, y;
	int dfb, dlr, dy;
}
thePixar = {0, -16, +16, -1, 1, -1};
void Pixar_Click(Widget *widgetPtr)
{
	thePixar.xfb += thePixar.dfb;
	if (thePixar.xfb < -16)
	{
		thePixar.xfb = -15;
		thePixar.dfb = 1;
	}
	else if (thePixar.xfb > 16)
	{
		thePixar.xfb = 15;
		thePixar.dfb = -1;
	}

	thePixar.xlr += thePixar.dlr;
	if (thePixar.xlr < -16)
	{
		thePixar.xlr = -15;
		thePixar.dlr = 1;
	}
	else if (thePixar.xlr > 16)
	{
		thePixar.xlr = 15;
		thePixar.dlr = -1;
	}

	thePixar.y += thePixar.dy;
	if (thePixar.y < -16)
	{
		thePixar.y = -15;
		thePixar.dy = 1;
	}
	else if (thePixar.y > 16)
	{
		thePixar.y = 15;
		thePixar.dy = -1;
	}
}

void Pixar_Draw(Widget *widgetPtr, int x, int y)
{
	BitmapType *bmp = &widgetPtr->bitmap;
	int bypp = bmp->pixelSize;
	IconPtr dst;
	int i, j;
	char pixel[4];

	/* Yellow */
	PixelSet_RGB((IconPtr) pixel, 255, 255, 0, bypp);

	x += 16 + thePixar.xlr;
	y += 16 + thePixar.y;
	dst = bmp->pixelPtr + y * bmp->pitch + x * bypp;
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			memcpy(dst + j * bmp->pixelSize, pixel, bypp);
		}
		dst += bmp->pitch;
	}
}

#endif /* PIXAR */

static void WindowToBitmap(Widget *widgetPtr, int *y, int *x)
{
	*y += widgetPtr->by;
	*x += widgetPtr->bx;
}

int HitTestTile(Widget *widgetPtr, int x, int y, int col, int row, int xc, int yc)
{
	t_display wtd;
	int yp, xp, y1, x1;
	int layer;

	/* Sanity check */
	if (col < 0 || col >= widgetPtr->cc)
		return -1;
	if (row < 0 || row >= widgetPtr->rc)
		return -1;

	/* Hit nothing when out-of-bounds */
	if (!in_bounds_test(yc, xc))
		return -1;

	(*((ExWidget *) widgetPtr)->whatToDrawProc)(widgetPtr, yc, xc, &wtd);

	/* Nothing is drawn */
	if (wtd.blank)
		return -1;

	xp = widgetPtr->xp[row * widgetPtr->cc + col];
	yp = widgetPtr->yp[row * widgetPtr->cc + col];

	WindowToBitmap(widgetPtr, &y, &x);

	/* Zero-based coords within icon */
	x1 = x - xp;
	y1 = y - yp;

	/* Test mon/obj/pla */
	if (wtd.fg.type != ICON_TYPE_NONE)
	{
		/* Handle left/right icons */
		for (layer = ICON_LAYER_4; layer >= ICON_LAYER_1; layer--)
		{
			t_icon_type *iconTypePtr;
			int index;

			if (wtd.bg[layer].type == ICON_TYPE_NONE)
				continue;

			if (wtd.bg[layer].type == ICON_TYPE_BLANK)
				break;

			iconTypePtr = &g_icon_type[wtd.bg[layer].type];
			index = wtd.bg[layer].index;
			if ((iconTypePtr->flags[index] & ICON_FLAG_LEFT) && (x1 < ISO_WID / 2))
				if (PointInIcon(widgetPtr, x1, y1, &wtd.bg[layer]))
					return layer + 1;
			if ((iconTypePtr->flags[index] & ICON_FLAG_RIGHT) && (x1 >= ISO_WID / 2))
				if (PointInIcon(widgetPtr, x1, y1, &wtd.bg[layer]))
					return layer + 1;

			break;
		}

		if (PointInIcon(widgetPtr, x1, y1, &wtd.fg))
			return 0;
	}

	/* Test feat */
	for (layer = ICON_LAYER_4; layer >= ICON_LAYER_1; layer--)
	{
		if (wtd.bg[layer].type == ICON_TYPE_NONE)
			continue;

		if (PointInIcon(widgetPtr, x1, y1, &wtd.bg[layer]))
			return layer + 1;
	}

	return -1;
}

int iso_hittest(Widget *widgetPtr, int x, int y, int col, int row, int *xc, int *yc)
{
	int layer, yc1, xc1;

	/* Test tile in front */
	xc1 = *xc + 1;
	yc1 = *yc + 1;
	layer = HitTestTile(widgetPtr, x, y, col, row + 2, xc1, yc1);
	if (layer != -1)
	{
		*xc = xc1;
		*yc = yc1;
		return layer;
	}

	/* Test SW tile */
	xc1 = *xc;
	yc1 = *yc + 1;
	if (row & 1)
	{
		layer = HitTestTile(widgetPtr, x, y, col, row + 1, xc1, yc1);
	}
	else
	{
		layer = HitTestTile(widgetPtr, x, y, col - 1, row + 1, xc1, yc1);
	}
	if (layer != -1)
	{
		*xc = xc1;
		*yc = yc1;
		return layer;
	}

	/* Test SE tile */
	xc1 = *xc + 1;
	yc1 = *yc;
	if (row & 1)
	{
		layer = HitTestTile(widgetPtr, x, y, col + 1, row + 1, xc1, yc1);
	}
	else
	{
		layer = HitTestTile(widgetPtr, x, y, col, row + 1, xc1, yc1);
	}
	if (layer != -1)
	{
		*xc = xc1;
		*yc = yc1;
		return layer;
	}

	/* Test hit tile */
	layer = HitTestTile(widgetPtr, x, y, col, row, *xc, *yc);

	return layer;
}

#ifdef PLAYER_HEALTH_BAR
static void DrawPlayerHealthBar(Widget *widgetPtr,
	int iconBbox[4], int x1, int y1, int w1, int h1, int clip)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	BitmapType *bitmapPtr = &widgetPtr->bitmap;
	int r, g, b;
	int width;
	IconPtr dstPtr;
	int x, y;
	char pixel[4];

	if (p_ptr->chp > (p_ptr->mhp * 8/*op_ptr_hitpoint_warn*/) / 10)
		return;

	if (clip && (iconBbox[1] >= y1 + h1 || iconBbox[1] + 2 <= y1))
		return;

	r = ((double) exPtr->healthBar.firstColorPtr->red / USHRT_MAX) * 255;
	g = ((double) exPtr->healthBar.firstColorPtr->green / USHRT_MAX) * 255;
	b = ((double) exPtr->healthBar.firstColorPtr->blue / USHRT_MAX) * 255;
	PixelSet_RGB((IconPtr) pixel, r, g, b, bitmapPtr->pixelSize);

	x = iconBbox[0], y = iconBbox[1];
	dstPtr = bitmapPtr->pixelPtr + x * bitmapPtr->pixelSize + y * bitmapPtr->pitch;
	for (y = 0; y < 2; y++)
	{
		for (x = 0; x < iconBbox[2]; x++)
		{
			if (clip && (iconBbox[1] + y < y1 || iconBbox[1] + y >= y1 + h1 ||
				iconBbox[0] + x < x1 || iconBbox[0] + x >= x1 + w1))
				continue;
			memcpy(dstPtr + x * bitmapPtr->pixelSize, pixel, bitmapPtr->pixelSize);
		}
		dstPtr += bitmapPtr->pitch;
	}

	width = iconBbox[3];
	if (p_ptr->mhp > 0)
		width *= (double)p_ptr->chp / p_ptr->mhp;

	r = ((double) exPtr->healthBar.secondColorPtr->red / USHRT_MAX) * 255;
	g = ((double) exPtr->healthBar.secondColorPtr->green / USHRT_MAX) * 255;
	b = ((double) exPtr->healthBar.secondColorPtr->blue / USHRT_MAX) * 255;
	PixelSet_RGB((IconPtr) pixel, r, g, b, bitmapPtr->pixelSize);

	x = iconBbox[0], y = iconBbox[1];
	dstPtr = bitmapPtr->pixelPtr + x * bitmapPtr->pixelSize + y * bitmapPtr->pitch;
	for (y = 0; y < 2; y++)
	{
		for (x = 0; x < width; x++)
		{
			if (clip && (iconBbox[1] + y < y1 || iconBbox[1] + y >= y1 + h1 ||
				iconBbox[0] + x < x1 || iconBbox[0] + x >= x1 + w1))
				continue;
			memcpy(dstPtr + x * bitmapPtr->pixelSize, pixel, bitmapPtr->pixelSize);
		}
		dstPtr += bitmapPtr->pitch;
	}

	exPtr->healthBar.drawn = TRUE;
	exPtr->healthBar.width = width;
}
#endif /* PLAYER_HEALTH_BAR */

#define BLIT_CLIP 0x0001
#define BLIT_HIGH 0x0002
#define BLIT_PLYR 0x0004

int blit(int x, int y, IconSpec iconSpec, Widget *widgetPtr,
	int x1, int y1, int w1, int h1, int flags)
{
	int clip = (flags & BLIT_CLIP) != 0;
	int high = (flags & BLIT_HIGH) != 0;
	BitmapType *bitmapPtr = &widgetPtr->bitmap;
	int col = 0;
	t_icon_type *iconTypePtr;
	IconPtr srcPtr, dstPtr;
	IconPtr rlePtr;
	int bypp = bitmapPtr->pixelSize;
	int pitch = bitmapPtr->pitch;
	int i, j, w, h;
	unsigned char *bounds;
	IconValue highPixel[4];
#ifdef PLAYER_HEALTH_BAR
	int iconBbox[4];
#endif

	/* Yellow */
	if (high)
		PixelSet_RGB((IconPtr) highPixel, 255, 255, 0, bypp);

	/* Ignore NONE icon */
	if (iconSpec.type == ICON_TYPE_NONE)
		return 0;

	/* Special handling of BLANK */
	if (iconSpec.type == ICON_TYPE_BLANK)
	{
		t_assign assign = g_assign[ASSIGN_FEATURE].assign[0];
		if (assign.assignType == ASSIGN_TYPE_ICON)
		{
			iconSpec.type = assign.icon.type;
			iconSpec.index = assign.icon.index;
			iconSpec.ascii = assign.icon.ascii;
			iconSpec.dark = 0;
		}
		else
		{
			iconSpec.type = ICON_TYPE_DEFAULT;
			iconSpec.index = 0;
			iconSpec.ascii = -1;
			iconSpec.dark = 0;
		}
	}

	/* Sanity check icon type */
	if ((iconSpec.type < 0) || (iconSpec.type >= g_icon_type_count))
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

		/* Access the icon type */
		iconTypePtr = &g_icon_type[iconSpec.type];
	}

	/* Sanity check draw location */
	if (x < 0 || x + iconTypePtr->width > bitmapPtr->width) return 2;
	if (y < 0 || y + iconTypePtr->height > bitmapPtr->height) return 3;

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

			h = iconTypePtr->height;
			w = iconTypePtr->width;
			x += (ISO_WID - w) / 2;
			y += ISO_HGT - ISO_BOTTOM - h;
#ifdef PLAYER_HEALTH_BAR
			if (flags & BLIT_PLYR)
				iconBbox[0] = x, iconBbox[1] = y, iconBbox[2] = w, iconBbox[3] = h;
#endif
			srcPtr = Icon_GetAsciiData(&iconSpec, iconData);
			if (clip)
			{
				if (x + w <= x1 || x >= x1 + w1)
					goto done;
				if (y + h <= y1 || y >= y1 + h1)
					goto done;
				if (x < x1)
				{
					srcPtr += (x1 - x) * bypp;
					w -= x1 - x;
					x = x1;
				}
				if (x + w >= x1 + w1)
				{
					w -= (x + w) - (x1 + w1);
				}
				if (y < y1)
				{
					srcPtr += (y1 - y) * iconTypePtr->pitch;
					h -= y1 - y;
					y = y1;
				}
				if (y + h >= y1 + h1)
				{
					h -= (y + h) - (y1 + h1);
				}
			}
			dstPtr = bitmapPtr->pixelPtr + x * bypp + y * pitch;
			while (h--)
			{
				memcpy(dstPtr, srcPtr, w * bypp);
				if (high)
				{
					if (y & 1)
						j = (x & 1) != 0;
					else
						j = (x & 1) == 0;
					for (i = j; i < w; i += 2)
						memcpy(dstPtr + i * bypp, highPixel, bypp);
				}
				srcPtr += iconTypePtr->pitch;
				dstPtr += pitch;
				++y;
			}
			goto done;
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

	/* Not transparent */
	if (!iconTypePtr->rle_data)
	{
		h = iconTypePtr->height;
		w = iconTypePtr->width;
		if (w < ISO_WID) /* Not an isometric icon */
		{
			x += (ISO_WID - w) / 2;
			y += ISO_HGT - ISO_BOTTOM - h;
		}
#ifdef PLAYER_HEALTH_BAR
		if (flags & BLIT_PLYR)
			iconBbox[0] = x, iconBbox[1] = y, iconBbox[2] = w, iconBbox[3] = h;
#endif
		if (iconSpec.dark)
			srcPtr = iconTypePtr->dark_data[iconSpec.index] + (iconSpec.dark - 1) * iconTypePtr->length;
		else
			srcPtr = iconTypePtr->icon_data + iconTypePtr->length * iconSpec.index;
		if (clip)
		{
			if (x + w <= x1 || x >= x1 + w1)
				goto done;
			if (y + h <= y1 || y >= y1 + h1)
				goto done;
			if (x < x1)
			{
				srcPtr += (x1 - x) * bypp;
				w -= x1 - x;
				x = x1;
			}
			if (x + w >= x1 + w1)
			{
				w -= (x + w) - (x1 + w1);
			}
			if (y < y1)
			{
				srcPtr += (y1 - y) * iconTypePtr->pitch;
				h -= y1 - y;
				y = y1;
			}
			if (y + h >= y1 + h1)
			{
				h -= (y + h) - (y1 + h1);
			}
		}
		dstPtr = bitmapPtr->pixelPtr + x * bypp + y * pitch;
		while (h--)
		{
			memcpy(dstPtr, srcPtr, w * bypp);
			if (high)
			{
				if (y & 1)
					j = (x & 1) != 0;
				else
					j = (x & 1) == 0;
				for (i = j; i < w; i += 2)
					memcpy(dstPtr + i * bypp, highPixel, bypp);
			}
			srcPtr += iconTypePtr->pitch;
			dstPtr += pitch;
			++y;
		}
		goto done;
	}

#ifdef PLAYER_HEALTH_BAR
	if (flags & BLIT_PLYR)
	{
		iconBbox[0] = x, iconBbox[1] = y;
		iconBbox[2] = iconTypePtr->width, iconBbox[3] = iconTypePtr->height;
		if (iconTypePtr->width < ISO_WID) /* Not an isometric icon */
		{
			iconBbox[0] += (ISO_WID - iconTypePtr->width) / 2;
			iconBbox[1] += ISO_HGT - ISO_BOTTOM - iconTypePtr->height;
		}
	}
#endif

	if (iconSpec.dark)
	{
		rlePtr = iconTypePtr->dark_data[iconSpec.index] + (iconSpec.dark - 1) * iconTypePtr->rle_len[iconSpec.index];
	}
	else if (iconTypePtr->dynamic)
		rlePtr = ((IconPtr *) iconTypePtr->rle_data)[iconSpec.index];
	else
		rlePtr = iconTypePtr->rle_data + iconTypePtr->rle_offset[iconSpec.index];
	bounds = iconTypePtr->rle_bounds + iconSpec.index * 4;

	x += bounds[0];
	y += bounds[1];
	w = bounds[2];
	h = bounds[3];

	if (iconTypePtr->width < ISO_WID) /* Not an isometric icon */
	{
		x += (ISO_WID - iconTypePtr->width) / 2;
		y -= bounds[1]; /* undo above */
		y += ISO_HGT - ISO_BOTTOM - h;
	}

	if (clip)
	{
		if (x + w <= x1 || x >= x1 + w1) goto done;
		if (y + h <= y1 || y >= y1 + h1) goto done;
	}

	dstPtr = bitmapPtr->pixelPtr + x * bypp + y * pitch;

#if 1
	if (clip)
	{
		/* Skip rows */
		while (y < y1)
		{
			unsigned int trans, opaq;

			trans = rlePtr[0];
			opaq = rlePtr[1];
			rlePtr += 2;
			col += trans;
			if (opaq)
			{
				rlePtr += opaq * bypp;
				col += opaq;
			}
			if (col == w)
			{
				++y;
				--h;
				col = 0;
				dstPtr += pitch;
			}
		}
		col = 0;
		while (y < y1 + h1)
		{
			unsigned int trans, opaq;

			trans = rlePtr[0];
			opaq = rlePtr[1];
			rlePtr += 2;

			col += trans;

			if (opaq)
			{
				for (i = 0; i < opaq; i++)
					if ((x + col + i >= x1) && (x + col + i < x1 + w1))
						memcpy(dstPtr + (col + i) * bypp, rlePtr + i * bypp, bypp);
				if (high)
				{
					if (y & 1)
						j = (col & 1) != 0;
					else
						j = (col & 1) == 0;
					for (i = j; i < opaq; i += 2)
						if ((x + col + i >= x1) && (x + col + i < x1 + w1))
							memcpy(dstPtr + (col + i) * bypp, highPixel, bypp);
				}
				rlePtr += opaq * bypp;
				col += opaq;
			}
			else if (!col)
				break;

			if (col == w)
			{
				if (!--h)
					break;
				++y;
				col = 0;
				dstPtr += pitch;
			}
		}
		goto done;
	}

	/* Not clipped */
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
			if (high)
			{
				if (y & 1)
					j = (col & 1) != 0;
				else
					j = (col & 1) == 0;
				for (i = j; i < opaq; i += 2)
					memcpy(dstPtr + (col + i) * bypp, highPixel, bypp);
			}
			rlePtr += opaq * bypp;
			col += opaq;
		}
		else if (!col)
			break;

		if (col == w)
		{
			if (!--h)
				break;
			++y;
			col = 0;
			dstPtr += pitch;
		}
	}

#else

	while (1)
	{
		unsigned int trans, opaq;

		trans = rlePtr[0];
		opaq = rlePtr[1];
		rlePtr += 2;

		col += trans;

		if (opaq)
		{
			if (clip)
			{
				if (y >= y1 && y < y1 + h1)
				{
					for (i = 0; i < opaq; i++)
						if ((x + col + i >= x1) && (x + col + i < x1 + w1))
							memcpy(dstPtr + (col + i) * bypp, rlePtr + i * bypp, bypp);
					if (high)
					{
						if (y & 1)
							j = (col & 1) != 0;
						else
							j = (col & 1) == 0;
						for (i = j; i < opaq; i += 2)
							if ((x + col + i >= x1) && (x + col + i < x1 + w1))
								memcpy(dstPtr + (col + i) * bypp, highPixel, bypp);
					}
				}
			}
			else
			{
				memcpy(dstPtr + col * bypp, rlePtr, opaq * bypp);
				if (high)
				{
					if (y & 1)
						j = (col & 1) != 0;
					else
						j = (col & 1) == 0;
					for (i = j; i < opaq; i += 2)
						memcpy(dstPtr + (col + i) * bypp, highPixel, bypp);
				}
			}
			rlePtr += opaq * bypp;
			col += opaq;
		}
		else if (!col)
			break;

		if (col == w)
		{
			if (!--h)
				break;
			++y;
			if (clip && (y >= y1 + h1))
				break;
			col = 0;
			dstPtr += pitch;
		}
	}
#endif /* 0 */

#ifdef PLAYER_HEALTH_BAR
done:
	if (flags & BLIT_PLYR)
		DrawPlayerHealthBar(widgetPtr, iconBbox, x1, y1, w1, h1, clip);
#endif

	return 0;
}

extern int *g_image_monster, *g_image_object;

#if 1
static bool is_unknown_floor(int y, int x, int hack)
{
	int feat;

	/* Require valid location */
	if (!in_bounds_test(y, x))
		return FALSE;

	/* Outer wall always has unknown floor */
	if (hack /* (g_grid[y][x].xtra & 0x0001) */ &&
		((y == g_cave_hgt - 1) || (x == g_cave_wid - 1)))
		return TRUE;

	feat = g_grid[y][x].f_idx;

	/* Unknown feat */
	if (!feat)
		return TRUE;

	/* This is a (known or seen) floor */
	if (feat == FEAT_FLOOR)
		return FALSE;

	/* No floor under this known/seen feat */
	if (!hack && (g_background[feat] != FEAT_FLOOR))
		return FALSE;

	/* Torch-lit */
	if (!g_grid[y][x].dark)
		return FALSE;

	/* Perma-lit */
	if (cave_info(y, x) & CAVE_GLOW)
		return FALSE;

	/* Unlit floor */
	return TRUE;
}

#endif

void iso_wtd(Widget *widgetPtr, int y, int x, t_display *wtd)
{
	int m_idx, o_idx, f_idx;
	int layer;
	t_grid *gridPtr;
	int dark, daytime;
	t_assign assign;
	IconSpec iconSpec;

	if (!in_bounds_test(y, x))
	{
		wtd->blank = TRUE;
		return;
	}

	/* Access the global cave memory */
	gridPtr = &g_grid[y][x];

	/* Get the darkness factor */
	dark = gridPtr->dark;

	/* Determine if there is daylight in the town */
	daytime = is_daytime();

	m_idx = gridPtr->m_idx;
	o_idx = gridPtr->o_idx;
	f_idx = gridPtr->f_idx;

	/* The grid is completely uninteresting */
	if (!f_idx && !m_idx && !o_idx)
	{
		wtd->blank = TRUE;

		/* Done */
		return;
	}

	wtd->anim = FALSE;
	wtd->blank = FALSE;
	wtd->fg.type = ICON_TYPE_NONE;
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		wtd->bg[layer].type = ICON_TYPE_NONE;
	}

	if (m_idx || o_idx)
	{
		/* Character */
		if (m_idx == -1)
		{
			int k = 0;

			/*
			 * Currently only one icon is assigned to the character. We
			 * could allow different icons to be used depending on the
			 * state of the character (badly wounded, invincible, etc).
			 */

#if defined(OANGBANDTK)
			/* OAngbandTk allows an icon for each shape */
			if (SCHANGE)
				k = p_ptr->schange;
#endif /* */

			assign = g_assign[ASSIGN_CHARACTER].assign[k];
		}

		/* Monster */
		else if (m_idx > 0)
		{
			/* Get the monster race */
			int r_idx = m_list[m_idx].r_idx;

			/* XXX Hack -- Hallucination */
			if (p_ptr->image)
			{
				/* Get a random monster race */
				r_idx = g_image_monster[r_idx];
			}

			/* Get the icon assigned to the monster race */
			assign = g_assign[ASSIGN_MONSTER].assign[r_idx];
		}

		/* Object */
		else if (o_idx)
		{
#ifdef ALLOW_PILE_IMAGE

			/* This a pile of objects */
			if (easy_floor && (gridPtr->xtra & GRID_XTRA_PILE))
			{
				/* Get the icon assigned to object-kind zero */
				assign = g_assign[ASSIGN_OBJECT].assign[0];
			}

			/* Not a pile */
			else
			{
				/* Get the object kind */
				int k_idx = o_list[o_idx].k_idx;

				/* XXX Hack -- Hallucination */
				if (p_ptr->image)
				{
					/* Get a random object kind */
					k_idx = g_image_object[k_idx];
				}

				/* Get the icon assigned to the object kind */
				assign = g_assign[ASSIGN_OBJECT].assign[k_idx];
			}

#else /* not ALLOW_PILE_IMAGE */

			/* Get the object kind */
			int k_idx = o_list[o_idx].k_idx;

			/* Get the icon assigned to the object kind */
			assign = g_assign[ASSIGN_OBJECT].assign[k_idx];

#endif /* not ALLOW_PILE_IMAGE */
		}

		/*
		 * Now we have the assignment for the character, monster, object or
		 * feature. For a feature, the icon index may have been adjusted
		 * for lighting. The assignment may be TYPE_ALTERNATE, TYPE_FLAVOR, or
		 * TYPE_SPRITE, which we must resolve into a "real" icon type and
		 * index (for example, the current frame of a sprite).
		 *
		 * XXX TYPE_ALTERNATE is currently used only for objects and
		 * features, but feature assignments must already be resolved
		 * in set_grid_assign(). That's why TYPE_ALTERNATE is only
		 * checked for objects (see below).
		 */

		switch (assign.assignType)
		{
			/*
			 * TYPE_ALTERNATE assignments use one of two icons,
			 * depending on some property of the object.
			 */
			case ASSIGN_TYPE_ALTERNATE:
			{
				/* Access the alternate */
				t_alternate *alternatePtr = &g_alternate[assign.alternate.index];

				/* Default to the first frame */
				int index = 0;

				switch (alternatePtr->reason)
				{
					case REASON_NONE:
						break;

					case REASON_NUMBER:
						if (o_idx == 0)
							dbwin("requesting REASON_NUMBER but o_idx==0");
						else if (o_list[o_idx].number == 1) ++index;
						break;

					case REASON_IDENT:
						if (o_idx == 0)
							dbwin("requesting REASON_IDENT but o_idx==0");
						else if (object_known_p(&o_list[o_idx])) ++index;
						break;

					case REASON_FRIEND:
						if (m_idx <= 0)
							dbwin("requesting REASON_FRIEND but m_idx<=0");
						else if (monster_is_friend(&m_list[m_idx])) ++index;
						break;
				}

				/* Get the type and index of the frame */
				iconSpec = alternatePtr->icon[index];
				break;
			}

			/* Resolve flavor */
			case ASSIGN_TYPE_FLAVOR:
			{
				/* Access the flavor */
				t_flavor *flavor = &g_flavor[assign.flavor.group];

				/* Get the flavor index */
				int index = flavor->sorted[assign.flavor.index];

				/* Get the icon */
				iconSpec = flavor->icon[index];
				break;
			}

			/* Resolve icon */
			case ASSIGN_TYPE_ICON:
			{
				iconSpec.type = assign.icon.type;
				iconSpec.index = assign.icon.index;
				iconSpec.ascii = assign.icon.ascii;

				/* XXX Hack -- Multi-hued ascii icons are animated */
				if (iconSpec.ascii != -1)
				{
					if (g_ascii[iconSpec.ascii].mode != ASCII_NORMAL)
					{
						/* This grid is animated */
						wtd->anim = TRUE;
					}
					if (m_idx > 0 &&
						g_ascii[iconSpec.ascii].altFriend != -1 &&
						monster_is_friend(&m_list[m_idx]))
					{
						iconSpec.ascii = g_ascii[iconSpec.ascii].altFriend;
					}
				}
				break;
			}

			/* Resolve sprite */
			case ASSIGN_TYPE_SPRITE:
			{
				/* Access the sprite */
				t_sprite *spritePtr = &g_sprite[assign.sprite.index];

				/* Get the current frame */
				if ((spritePtr->frame >= 0) && (spritePtr->count > spritePtr->frame))
					iconSpec = spritePtr->icon[spritePtr->frame];
				else
				{
					iconSpec.type = ICON_TYPE_DEFAULT;
					iconSpec.index = 0;
					iconSpec.ascii = -1;
				}

				/* This grid is animated */
				wtd->anim = TRUE;
			}
		}

		/* */
		iconSpec.dark = 0;
		wtd->fg = iconSpec;
	}

	if (!f_idx)
	{
		wtd->bg[ICON_LAYER_1].type = ICON_TYPE_BLANK;
		return;
	}

	/* Delayed update of g_icon_map[] */
	if (g_icon_map_changed)
	{
		int y, x;

		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				set_grid_assign(y, x);
			}
		}
		g_icon_map_changed = FALSE;
	}

	/* Option: Use 4 levels of lighting. */
	if (g_torchlite)
	{
		/* Grid is lit by the character's light source */
		if (dark == GRID_LITE_TORCH)
		{
			/* Calculate distance from py,px */
			dark = MAX(ABS(x - p_ptr_px), ABS(y - p_ptr_py)) - 1;

			/* We may have dark == -1 at py,px */
			if (dark < 0) dark = 0;

			/* Light radius > 3 not allowed */
			else if (dark > 3) dark = 3;
		}

		/* Grid isn't lit by character's light source */
		else
		{
			/* Maximum darkness */
			dark = 3;
		}
	}

	/* In the Town */
	if (daytime)
	{
		/* Use bright light */
		dark = 0;
	}

#if 0
if (!daytime && !dark && (g_grid[y][x].xtra & GRID_XTRA_WALL) &&
	(g_grid[y][x].shape != GRID_SHAPE_SINGLE))
{
	if (p_ptr_py < y && p_ptr_px < x)
		dark = 2;
}
#endif

	/*
	 * Get the assignment from the global assign map. The g_icon_map[]
	 * array allows us to use different assignments for the same
	 * feature index. For example, doors may be vertical or
	 * horizontal, and some walls are actually pillars.
	 */
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		assign = g_icon_map[layer][y][x];

#if 1
		if (!layer)
		{
			if ((g_grid[y][x].xtra & 0x0001) &&
				(is_unknown_floor(y, x, TRUE) ||
				is_unknown_floor(y+1, x, FALSE) ||
				is_unknown_floor(y, x+1, FALSE)))
			{
				assign = g_assignshape[g_grid[y][x].shape][MAX_F_IDX + f_idx];
			}
			else if (is_unknown_floor(y, x, FALSE))
			{
				assign = g_assign[ASSIGN_FEATURE].assign[FEAT_NONE];
			}
		}
#else
		/*
		 * XXX Hack -- There is a second assignment for this feature
		 * with an "unknown" floor.
		 */
		if (!layer && (g_grid[y][x].xtra & 0x0001) &&
			(((y == g_cave_hgt - 1) || !g_grid[y+1][x].f_idx) ||
			((x == g_cave_wid - 1) || !g_grid[y][x+1].f_idx)))
		{
			assign = g_assignshape[g_grid[y][x].shape][MAX_F_IDX + f_idx];
		}

		/*
		 * XXX Hack -- If floor is not lit, use "unknown" floor.
		 */
		else if (!layer && (f_idx != FEAT_FLOOR) &&
			(g_background[f_idx] == FEAT_FLOOR) &&
			(g_grid[y][x].dark && !(cave_info(y, x) & CAVE_GLOW)))
		{
			assign = g_assign[ASSIGN_FEATURE].assign[FEAT_NONE];
		}
#endif

		/* Resolve sprite */
		if (assign.assignType == ASSIGN_TYPE_SPRITE)
		{
			/* Access the sprite */
			t_sprite *spritePtr = &g_sprite[assign.sprite.index];

			/* Get the type and index of the current frame */
			iconSpec = spritePtr->icon[spritePtr->frame];

			/* This grid is animated */
			wtd->anim = TRUE;
		}

		/* Must be an icon */
		else
		{
			iconSpec.type = assign.icon.type;
			iconSpec.index = assign.icon.index;
			iconSpec.ascii = assign.icon.ascii;

			/* Only layer 1 is required */
			if (iconSpec.type == ICON_TYPE_NONE)
			{
				wtd->bg[layer] = iconSpec;
				break;
			}

			/* XXX Hack -- Multi-hued ascii icons are animated */
			if (iconSpec.ascii != -1)
			{
				if (g_ascii[iconSpec.ascii].mode != ASCII_NORMAL)
				{
					/* This grid is animated */
					wtd->anim = TRUE;
				}
			}
		}
#if 0
		/*
		 * Note that TYPE_ALTERNATE assignments must already have
		 * been resolved in set_grid_assign() or this will bomb.
		 * And TYPE_SPRITE assignments will bomb if lighting is
		 * FT_LIGHT_ICON and not FT_LIGHT_TINT.
		 */

		if (dark)
		{
			/* Examine the lighting mode for this feature */
			switch (g_feat_lite[f_idx])
			{
				/* Use icon series for lighting */
				case FT_LIGHT_ICON:

					/* Paranoia: only icons use light */
					if (assign.assignType == ASSIGN_TYPE_ICON)
					{
						if (assign.icon.type > ICON_TYPE_DEFAULT)
							iconSpec.index += dark;
					}
					break;

				/* Use tint table for lighting (slow) */
				case FT_LIGHT_TINT:
					if (g_icon_depth == 8)
						wtd->tint[1] = g_darken[dark-1].table;
					break;
			}
		}
#endif
		/* A darkened copy of the icon exists, or will exist */
		if ((g_icon_type[iconSpec.type].dark_data &&
			g_icon_type[iconSpec.type].dark_data[iconSpec.index]) ||
			(g_icon_type[iconSpec.type].flags[iconSpec.index] & ICON_FLAG_DARK))
		{
			iconSpec.dark = dark;
		}
		else
			iconSpec.dark = 0;
		wtd->bg[layer] = iconSpec;
	}
}

int PointInIcon(Widget *widgetPtr, int x, int y, IconSpec *iconSpecPtr)
{
	t_icon_type *iconTypePtr;
	IconPtr rlePtr;
	int w, h, row = 0;
	int col = 0;
	int bypp;
	unsigned char *bounds;

	if ((iconSpecPtr->type < 0) || (iconSpecPtr->type >= g_icon_type_count))
		return 0;

	iconTypePtr = &g_icon_type[iconSpecPtr->type];

	if ((iconSpecPtr->index < 0) || (iconSpecPtr->index >= iconTypePtr->icon_count))
		return 0;

	w = iconTypePtr->width;
	h = iconTypePtr->height;

	if (iconTypePtr->rle_data)
	{
		bounds = iconTypePtr->rle_bounds + iconSpecPtr->index * 4;

		x -= bounds[0];
		y -= bounds[1];
		w = bounds[2];
		h = bounds[3];

		if (iconTypePtr->width < ISO_WID) /* Not an isometric icon */
		{
			y += bounds[1];
		}
	}

	if (iconTypePtr->width < ISO_WID) /* Not an isometric icon */
	{
		x -= (ISO_WID - iconTypePtr->width) / 2;
		y -= ISO_HGT - ISO_BOTTOM - h;
	}

	if ((x < 0) || (x >= w))
		return 0;
	if ((y < 0) || (y >= h))
		return 0;

	if (!iconTypePtr->rle_data)
		return 1;

	bypp = iconTypePtr->bypp;

	if (iconTypePtr->dynamic)
		rlePtr = ((IconPtr *) iconTypePtr->rle_data)[iconSpecPtr->index];
	else
		rlePtr = iconTypePtr->rle_data +
			iconTypePtr->rle_offset[iconSpecPtr->index];

	while (1)
	{
		unsigned int trans, opaq;

		trans = rlePtr[0];
		opaq = rlePtr[1];
		rlePtr += 2;

		if (y == row)
		{
			if (x < col + trans)
				return 0;
			if (x < col + trans + opaq)
				return 1;
		}

		col += trans;

		if (opaq)
		{
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
			++row;
		}
	}

	return 0;
}

void iso_draw_all(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	t_display wtd;
	int tile, y, x, yp, xp, yt, xt;
	int rc = widgetPtr->rc;
	int cc = widgetPtr->cc;
	short *pinfo = widgetPtr->info;
	int *pyo = widgetPtr->yo;
	int *pxo = widgetPtr->xo;
	int *pyp = widgetPtr->yp;
	int *pxp = widgetPtr->xp;
#ifdef GRID_EFFECT
	t_grid_effect *effectPtr;
#elif defined(WIDGET_EFFECT)
	ExWidgetEffect *effectPtr = ((ExWidget *) widgetPtr)->effect;
#else
	IconSpec *effectPtr = ((ExWidget *) widgetPtr)->effect;
#endif
	IconSpec iconSpec, darkIconSpec;
	IconSpec bfSpec;
	int bfHigh;
	BitmapType *bitmapPtr = &widgetPtr->bitmap;
	void (*wtdProc)(Widget *, int, int, t_display *) =
		exPtr->whatToDrawProc;
	int layer, high;
	DoubleLink *link;

/* dbwin("fresh\n"); */

	if (widgetPtr->flags & WIDGET_NO_UPDATE)
		return;

#if 0
	/* Paint to catch errors */
	IconPtr dst = bitmapPtr->pixelPtr;
	for (y = 0; y < bitmapPtr->height; y++)
	{
		for (x = 0; x < bitmapPtr->width; x++)
			dst[x] = 10;
		dst += bitmapPtr->pitch;
	}
#endif

#ifdef PIXAR
	Pixar_Click(widgetPtr);
#endif

	widgetPtr->animCnt = 0;

#ifdef PLAYER_HEALTH_BAR
	exPtr->healthBar.drawn = FALSE;
#endif

	/* Get cave coords of top-left tile */
	yt = widgetPtr->y0;
	xt = widgetPtr->x0;

	/* Darkness */
	darkIconSpec.type = ICON_TYPE_BLANK;

	/* Check every tile */
	for (tile = 0; tile < cc * rc; tile++)
	{
		if (pinfo[tile] & WIDGET_INFO_IGNORE)
			continue;

		pinfo[tile] &= ~(WIDGET_INFO_DIRTY | WIDGET_INFO_ANIM);

		/* Calculate cave coords */
		y = yt + pyo[tile];
		x = xt + pxo[tile];

		/* Get pixel offsets to draw tile */
		yp = pyp[tile];
		xp = pxp[tile];

		/* Callback tells us what to draw */
		(*wtdProc)(widgetPtr, y, x, &wtd);

		/* Unknown grid */
		if (wtd.blank)
		{
			/* Darkness */
			high = 0;
			if (exPtr->hit == 1 && exPtr->hitx == x && exPtr->hity == y)
				high = BLIT_HIGH;
			blit(xp, yp, darkIconSpec, widgetPtr, 0, 0, 0, 0, high);

			/* Next cave location */
			continue;
		}

		if (wtd.anim)
		{
			pinfo[tile] |= WIDGET_INFO_ANIM;
			widgetPtr->anim[widgetPtr->animCnt++] = tile;
		}

		bfSpec.type = ICON_TYPE_NONE;
		bfHigh = 0;

		/*
		 * Draw 1-4 background layers.
		 */
		for (layer = 0; layer < ICON_LAYER_MAX; layer++)
		{
			iconSpec = wtd.bg[layer];

			/* Stop at NONE icon */
			if (iconSpec.type == ICON_TYPE_NONE)
				break;

			/* Oct 29 2004 */
			/* Catch drawing transparent icon on layer 0 */
			if (iconSpec.type != ICON_TYPE_BLANK)
			{
				extern int Icon_Transparent(t_icon_type *iconTypePtr, int index, int iso);
				if (!layer && Icon_Transparent(&g_icon_type[iconSpec.type], iconSpec.index, TRUE))
				{
					iconSpec.type = ICON_TYPE_DEFAULT;
					iconSpec.index = 0;
					iconSpec.ascii = -1;
					iconSpec.dark = 0;
				}
			}

			/* Draw background icon */
			high = 0;
			if ((exPtr->hit == (layer + 1)) && (exPtr->hitx == x) && (exPtr->hity == y))
				high = BLIT_HIGH;
			if (blit(xp, yp, iconSpec, widgetPtr, 0, 0, 0, 0, high) > 1)
			{
				dbwin("can't blit col,row %d,%d @ x,y %d,%d w,h %d,%d icon \"%s %d\"\n",
					tile % cc, tile / cc, xp, yp, bitmapPtr->width, bitmapPtr->height,
					g_icon_type[iconSpec.type].desc,
					iconSpec.index);
			}

			/* Stop at BLANK icon */
			if (iconSpec.type == ICON_TYPE_BLANK)
				break;

			/* This icon has 2 halves */
			if (g_icon_type[iconSpec.type].flags[iconSpec.index] & (ICON_FLAG_LEFT | ICON_FLAG_RIGHT))
			{
				bfSpec = iconSpec;
				bfHigh = high;
			}
		}

		/* Draw foreground icon */
		if (wtd.fg.type != ICON_TYPE_NONE)
		{
#ifdef PIXAR
			if (i == widgetPtr->centerTile && thePixar.xfb <= 0)
				Pixar_Draw(widgetPtr, xp + 10, yp + 7);
#endif
			high = 0;
			if (exPtr->hit == 0 && exPtr->hitx == x && exPtr->hity == y)
				high = BLIT_HIGH;
#ifdef PLAYER_HEALTH_BAR
			if (y == p_ptr_py && x == p_ptr_px)
				high |= BLIT_PLYR;
#endif
			blit(xp, yp, wtd.fg, widgetPtr, 0, 0, 0, 0, high);
#ifdef PIXAR
			if (i == widgetPtr->centerTile && thePixar.xfb > 0)
				Pixar_Draw(widgetPtr, xp + 10, yp + 7);
#endif
		}

#ifdef GRID_EFFECT
		effectPtr = &g_grid_effect[y][x];

		/* Invert */
		if (effectPtr->flags & GRID_EFFECT_INVERT)
			Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

		/* Draw effect */
		iconSpec = effectPtr->icon;
#elif defined(WIDGET_EFFECT)
		/* Invert */
		if (effectPtr[tile].flags & WIDGET_EFFECT_INVERT)
			Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

		/* Draw effect */
		iconSpec = effectPtr[tile].icon;
#else
		/* Draw effect */
		iconSpec = effectPtr[tile];
#endif
		if (iconSpec.type != ICON_TYPE_NONE)
		{
			blit(xp, yp, iconSpec, widgetPtr, 0, 0, 0, 0, /*flags*/0);
		}

		/* This icon has 2 halves */
		if (bfSpec.type != ICON_TYPE_NONE)
		{
			if ((wtd.fg.type != ICON_TYPE_NONE) || (iconSpec.type != ICON_TYPE_NONE))
			{
				if (g_icon_type[bfSpec.type].flags[bfSpec.index] & ICON_FLAG_LEFT)
					blit(xp, yp, bfSpec, widgetPtr,
						xp, yp, ISO_WID / 2, ISO_HGT, BLIT_CLIP | bfHigh);
				else if (g_icon_type[bfSpec.type].flags[bfSpec.index] & ICON_FLAG_RIGHT)
					blit(xp, yp, bfSpec, widgetPtr,
						xp + ISO_WID / 2, yp, ISO_WID / 2, ISO_HGT, BLIT_CLIP | bfHigh);
			}
		}
	}

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

#if 0
	/* horizontal line 1/2 height */
	y = widgetPtr->by + widgetPtr->height / 2 - 1;
	dst = widgetPtr->bitmap.pixelPtr + y * widgetPtr->bitmap.pitch;
	for (x = widgetPtr->bx; x < widgetPtr->bx + widgetPtr->width; x++)
		dst[x] = 10;

	/* vertical line 1/2 width */
	x = widgetPtr->bx + widgetPtr->width / 2 - 1;
	dst = widgetPtr->bitmap.pixelPtr + x * widgetPtr->bitmap.pixelSize;
	for (y = widgetPtr->by; y < widgetPtr->by + widgetPtr->height; y++)
		*(dst + y * widgetPtr->bitmap.pitch) = 10;

	/* vertical line -1 */
	x = widgetPtr->bx - 1;
	dst = widgetPtr->bitmap.pixelPtr + x * widgetPtr->bitmap.pixelSize;
	for (y = widgetPtr->by; y < widgetPtr->by + widgetPtr->height; y++)
		*(dst + y * widgetPtr->bitmap.pitch) = 14;
#endif
}

int sectrect(int x1, int y1, int w1, int h1, int x2, int y2, int w2, int h2, int b[4])
{
	int y, x, x3, y3, x4, y4;

	x3 = x1 + w1;
	y3 = y1 + h1;
	x4 = x2 + w2;
	y4 = y2 + h2;

	b[0] = (x1 < x2) ? x2 : x1;
	b[1] = (y1 < y2) ? y2 : y1;
	x = (x3 < x4) ? x3 : x4;
	y = (y3 < y4) ? y3 : y4;
	b[2] = x - b[0];
	b[3] = y - b[1];

	return (b[2] > 0 && b[3] > 0);
}

void DrawClipped(Widget *widgetPtr, int x, int y, int w, int h,
	int tile, int yc, int xc)
{
	t_display wtd;
	int xp, yp;
	int rc = widgetPtr->rc;
	int cc = widgetPtr->cc;
	IconSpec iconSpec, darkIconSpec;
	IconSpec bfSpec;
	void (*wtdProc)(Widget *, int, int, t_display *) =
		((ExWidget *) widgetPtr)->whatToDrawProc;
	int layer;
#ifdef GRID_EFFECT
	t_grid_effect *effectPtr = &g_grid_effect[yc][xc];
#elif defined(WIDGET_EFFECT)
	ExWidgetEffect *effectPtr = &((ExWidget *) widgetPtr)->effect[tile];
#endif

	if (tile < 0 || tile >= rc * cc)
		return;
	if (widgetPtr->info[tile] & WIDGET_INFO_IGNORE)
		return;

	/* Darkness */
	darkIconSpec.type = ICON_TYPE_BLANK;

	/* Tile is no longer dirty */
	widgetPtr->info[tile] &= ~WIDGET_INFO_DIRTY;

	/* Get pixel offsets to draw tile */
	yp = widgetPtr->yp[tile];
	xp = widgetPtr->xp[tile];

if ((yp + ISO_HGT <= y) || (yp >= y + h)) return;
if ((xp + ISO_WID <= x) || (xp >= x + w)) return;

	(*wtdProc)(widgetPtr, yc, xc, &wtd);

	/* Unknown grid */
	if (wtd.blank)
	{
		/* Darkness */
		blit(xp, yp, darkIconSpec, widgetPtr, x, y, w, h, BLIT_CLIP);

		/* Next cave location */
		return;
	}

	if (wtd.anim && !(widgetPtr->info[tile] & WIDGET_INFO_ANIM))
	{
		widgetPtr->info[tile] |= WIDGET_INFO_ANIM;
		widgetPtr->anim[widgetPtr->animCnt++] = tile;
	}

	bfSpec.type = ICON_TYPE_NONE;

	/*
	 * Draw 1-4 background layers.
	 */
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		iconSpec = wtd.bg[layer];

		/* Stop at NONE icon */
		if (iconSpec.type == ICON_TYPE_NONE)
			break;

		/* Oct 29 2004 */
		/* Catch drawing transparent icon on layer 0 */
		if (iconSpec.type != ICON_TYPE_BLANK)
		{
			extern int Icon_Transparent(t_icon_type *iconTypePtr, int index, int iso);
			if (!layer && Icon_Transparent(&g_icon_type[iconSpec.type], iconSpec.index, TRUE))
			{
				iconSpec.type = ICON_TYPE_DEFAULT;
				iconSpec.index = 0;
				iconSpec.ascii = -1;
				iconSpec.dark = 0;
			}
		}

		/* FIXME: tint */

		/* Draw background icon */
		if (blit(xp, yp, iconSpec, widgetPtr, x, y, w, h, BLIT_CLIP) > 1)
		{
			dbwin("can't blit col,row %d,%d @ x,y %d,%d\n",
				tile % cc, tile / cc, xp, yp);
		}

		/* Stop at BLANK icon */
		if (iconSpec.type == ICON_TYPE_BLANK)
			break;

		if (g_icon_type[iconSpec.type].flags[iconSpec.index] & (ICON_FLAG_LEFT | ICON_FLAG_RIGHT))
			bfSpec = iconSpec;
	}

	/* Draw foreground icon */
	if (wtd.fg.type != ICON_TYPE_NONE)
	{
		int flags = BLIT_CLIP;
#ifdef PLAYER_HEALTH_BAR
		if (yc == p_ptr_py && xc == p_ptr_px)
		{
			((ExWidget *)widgetPtr)->healthBar.drawn = FALSE;
			flags |= BLIT_PLYR;
		}
#endif
		blit(xp, yp, wtd.fg, widgetPtr, x, y, w, h, flags);
	}

#ifdef GRID_EFFECT
	/* Invert */
	if (effectPtr->flags & GRID_EFFECT_INVERT)
		Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

	/* Draw effect */
	iconSpec = effectPtr->icon;
#elif defined(WIDGET_EFFECT)
	/* Invert */
	if (effectPtr->flags & WIDGET_EFFECT_INVERT)
		Widget_InvertSpot(widgetPtr, tile / cc, tile % cc, &wtd);

	/* Draw effect */
	iconSpec = effectPtr->icon;
#else
	/* Draw effect */
	iconSpec = ((ExWidget *) widgetPtr)->effect[tile];
#endif
	if (iconSpec.type != ICON_TYPE_NONE)
	{
		blit(xp, yp, iconSpec, widgetPtr, x, y, w, h, BLIT_CLIP);
	}

	if (bfSpec.type != ICON_TYPE_NONE)
	{
		if ((wtd.fg.type != ICON_TYPE_NONE) || (iconSpec.type != ICON_TYPE_NONE))
		{
			if (g_icon_type[bfSpec.type].flags[bfSpec.index] & ICON_FLAG_LEFT)
			{
				int b[4];
				if (sectrect(x, y, w, h, xp, yp, ISO_WID / 2, ISO_HGT, b))
					blit(xp, yp, bfSpec, widgetPtr, b[0], b[1], b[2], b[3], BLIT_CLIP);
			}
			else if (g_icon_type[bfSpec.type].flags[bfSpec.index] & ICON_FLAG_RIGHT)
			{
				int b[4];
				if (sectrect(x, y, w, h, xp + ISO_WID / 2, yp, ISO_WID / 2, ISO_HGT, b))
					blit(xp, yp, bfSpec, widgetPtr, b[0], b[1], b[2], b[3], BLIT_CLIP);
			}
		}
	}
}

void iso_draw_invalid(Widget *widgetPtr)
{
	int tile, i, yp, xp, yt, xt;
	int rc = widgetPtr->rc;
	int cc = widgetPtr->cc;
	int *pyo = widgetPtr->yo;
	int *pxo = widgetPtr->xo;
	int *pyp = widgetPtr->yp;
	int *pxp = widgetPtr->xp;
	int *pinvalid = widgetPtr->invalid;
	int dl, dt, dr, db;
	int by, bx;
	int row, col;
	DoubleLink *link;

if (debug_widgets & DEBUG_WIDGET_DRAW) dbwin("iso_draw_invalid %d\n", widgetPtr->invalidCnt);

	if (widgetPtr->flags & WIDGET_NO_UPDATE)
		return;

	/* Keep track of dirty area */
	dl = widgetPtr->bitmap.width;
	dt = widgetPtr->bitmap.height;
	dr = 0;
	db = 0;

	/* Clear the "animation" flag */
	/* Don't clear, because only some grids drawn */
/*	widgetPtr->animCnt = 0 */

	/* Get cave coords of top-left tile */
	yt = widgetPtr->y0;
	xt = widgetPtr->x0;

	for (i = 0; i < widgetPtr->invalidCnt; i++)
	{
		tile = pinvalid[i];

		/* Pixel offsets in bitmap */
		xp = pxp[tile];
		yp = pyp[tile];

#if 1
		if (debug_widgets & DEBUG_WIDGET_DRAW)
		{
			/* Paint to catch errors */
			IconPtr dst = widgetPtr->bitmap.pixelPtr + yp * widgetPtr->bitmap.pitch +
				xp * widgetPtr->bitmap.pixelSize;
			int y2, x2;
			for (y2 = 0; y2 < ISO_HGT; y2++)
			{
				for (x2 = 0; x2 < ISO_WID * widgetPtr->bitmap.pixelSize; x2++)
					dst[x2] = 10;
				dst += widgetPtr->bitmap.pitch;
			}
		}
#endif

		/* Dirty bounds */
		if (xp < dl)
			dl = xp;
		if (yp < dt)
			dt = yp;
		if (xp + ISO_WID - 1 > dr)
			dr = xp + ISO_WID - 1;
		if (yp + ISO_HGT - 1 > db)
			db = yp + ISO_HGT - 1;

		for (row = (tile / cc) - 3; row <= (tile / cc) + 3; row++)
		{
			if (row < 0 || row >= rc) continue;
			for (col = (tile % cc) - 1; col <= (tile % cc) + 1; col++)
			{
				int tile2 = row * cc + col;
				if (col < 0 || col >= cc) continue;
				DrawClipped(widgetPtr, xp, yp, ISO_WID, ISO_HGT,
					tile2, yt + pyo[tile2], xt + pxo[tile2]);
			}
		}
	}

	widgetPtr->invalidCnt = 0;

	/* Now draw all of the items for this Widget */
	by = widgetPtr->by;
	bx = widgetPtr->bx;
	for (link = widgetPtr->linkerItemVis.head; 1 && link; link = link->next)
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

