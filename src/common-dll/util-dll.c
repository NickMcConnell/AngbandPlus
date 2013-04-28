/* File: util-dll.c */

/* Purpose: misc stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

#include <math.h>
#include <string.h>
#include <tcl.h>
#include "cmdinfo-dll.h"
#include "util-dll.h"
#include "plat-dll.h"
#include "dbwin.h"

extern int debug_widgets;

#define MAX_DELTA 195076 /* 255^2 + 255^2 + 255^2 + 1 */
#define MAX_COLOR_ENTRY 1021

/* Structure for a hash table used by rgb2index() */
typedef struct
{
	int valid; /* The hash-table entry is valid */
	unsigned long pixel; /* an RGB pixel value */
	int index; /* closest matching palette index for 'pixel' */
} t_color_entry;

typedef struct IndexedColor IndexedColor;
struct IndexedColor
{
	unsigned char rgb[256 * 3];
	t_color_entry hash[MAX_COLOR_ENTRY];
	void *platData;
};

static IndexedColor g_palette;
static int Palette_Initialized = 0;

DLLEXPORT int g_palette_black = 255;
DLLEXPORT int g_palette_white = 0;
DLLEXPORT int g_colormap_black;
DLLEXPORT int g_colormap_white;

DLLEXPORT unsigned char g_colormap2palette[256];
DLLEXPORT unsigned char g_palette2colormap[256];

/*
 * Allocate a new array
 */
#ifdef MEMORY_DEBUG
void *_Array_New(int count, int elem_size, char *file, int line)
#else
void *Array_New(int count, int elem_size)
#endif
{
#ifdef MEMORY_DEBUG
	char *p = _db_malloc(count * elem_size, file, line);
	memset(p, 0xFF, count * elem_size);
#else
	char *p = Tcl_AllocDebug(count * elem_size);
#endif /* MEMORY_DEBUG */
	return (void *) p;
}

/*
 * Append an element to an array
 */
void *Array_Append(void *array_ptr, int *count, int elem_size,
	void *elem_ptr)
{
	char *ap = array_ptr;
	int n = (*count) + 1;

	ap = Tcl_ReallocDebug(ap, n * elem_size);
	(void) memcpy(ap + (n - 1) * elem_size, elem_ptr, elem_size);
	(*count) = n;
	return ap;
}

/*
 * Insert an element in an array
 */
void *Array_Insert(void *array_ptr, int *count, int elem_size,
	int index)
{
	char *ap = array_ptr;
	int n = (*count) + 1;

	if (index < 0) index = 0;
	if (index >= n) index = n - 1;

	ap = Tcl_ReallocDebug(ap, n * elem_size);
	if (index != n - 1)
	{
		(void) memcpy(ap + (index + 1) * elem_size,
			ap + index * elem_size,
			(n - index - 1) * elem_size);
	}
	else
	{
		memset(ap + index * elem_size, 0, elem_size);
	}
	(*count) = n;
	return ap;
}

/*
 * Delete an element from an array
 */
void *Array_Delete(void *array_ptr, int *count, int elem_size,
	int index)
{
	char *ap = array_ptr;
	int i, n = (*count) - 1;

	if (index < 0) index = 0;
	if (index > n) index = n;

	if (index != n)
	{
		(void) memcpy(ap + index * elem_size,
			ap + (index + 1) * elem_size,
			(n - index) * elem_size);
	}

	/* Zero the trailing slot to catch errors */
	for (i = 0; i < elem_size; i++)
	{
		ap[n * elem_size + i] = 0;
	}
	
	(*count) = n;
	return (void *) Tcl_ReallocDebug(ap, n * elem_size);
}

void IndexedColor_ResetHash(IndexedColor *idc)
{
	int i;

	for (i = 0; i < 256; i++)
	{
		idc->hash[i].valid = 0;
	}
}

/*
 * Returns the nearest matching palette index for the given
 * RGB values.
 */
int IndexedColor_RGB2Index(IndexedColor *idc, unsigned char r, unsigned char g, unsigned char b)
{
	int i, delta, index, sum, max;
	unsigned char *col;
	unsigned long pixel;
	t_color_entry *entry;

	/* Calculate the pixel value */
	pixel = (r << 16) | (g << 8) | b;

	/* Hash the pixel value */
	entry = &idc->hash[pixel % MAX_COLOR_ENTRY];

	/* We already calculated the palette index */
	if (entry->valid && (entry->pixel == pixel))
	{
		/* Return the palette index */
		return entry->index;
    }

	index = 0;
	max = MAX_DELTA;

	col = idc->rgb;

	/* Check each palette entry */
	for (i = 0; i < 256; i++)
	{
		/*
		 * Calculate the minimum difference between the given RGB value
		 * and each of the palette entries
		 */
		delta = r - *col++; sum = delta * delta;
		delta = g - *col++; sum += delta * delta;
		delta = b - *col++; sum += delta * delta;

		/* This palette entry is a better match than any other so far */
		if (sum < max)
		{
			/* Remember the palette index */
			index = i;

			/* Remember the minimum difference */
			max = sum;
		}
	}

	/* Remember the hash table entry info */
	entry->pixel = pixel;
	entry->index = index;
	entry->valid = 1;

	/* Return the palette index */
	return index;
}

/*
 * Calculate the 256 palette indices that the palette index 'tint'
 * looks like at the given opacity against each of the standard
 * palette colors.
 */
void IndexedColor_TintTable(IndexedColor *idc, int tint, int opacity, TintTable table)
{
	int i;
	long v1, v2, m;
	int r1, g1, b1, r2, g2, b2;
	unsigned char *rgb = idc->rgb;

#define RGB_MAX_S 127
#define RGB_MAX_U 255

	m = opacity;

	/* Get the RGB values for the given palette index */
	r2 = rgb[tint * 3];
	g2 = rgb[tint * 3 + 1];
	b2 = rgb[tint * 3 + 2];

	/* Check each palette entry */
	for (i = 0; i < 256; ++i)
	{
		/* Get the i'th RGB values */
		r1 = rgb[i * 3];
		g1 = rgb[i * 3 + 1];
		b1 = rgb[i * 3 + 2];

		/*
		 * Simple linear equation using opacity as "slope"
		 */

		v1 = r1;
		v2 = r2;
		r1 = v1 + ( m * ( v2 - v1 ) + (RGB_MAX_S) ) / RGB_MAX_U;
		
		v1 = g1;
		v2 = g2;
		g1 = v1 + ( m * ( v2 - v1 ) + (RGB_MAX_S) ) / RGB_MAX_U;
		
		v1 = b1;
		v2 = b2;
		b1 = v1 + ( m * ( v2 - v1 ) + (RGB_MAX_S) ) / RGB_MAX_U;

		/* Now we have the RGB value, convert it into a palette index */
		table[i] = (TintValue) IndexedColor_RGB2Index(idc, r1, g1, b1);
	}
}

/*
 * Return gamma-corrected intensity. This is usually called on
 * each component of an RGB value.
 */
int gamma_correct(int value, double gamma)
{
	double ind;
	double inverse;

	if (gamma)
	{
		inverse = 1.0 / gamma;
	}
	else
	{
		inverse = 1.0;
	}
	ind = (double) value / 256.0;
	return (int) (256 * pow(ind, inverse));
}

/* 
 * Writes 256 palette indices into the given tint table. Each
 * index is calculated by applying a gamma correction to each
 * RGB value of the global palette, then finding the nearest
 * palette index for the gamma-corrected RGB.
 */
void IndexedColor_GammaTable(IndexedColor *idc, double gamma, TintTable tint)
{
	int i, r, g, b;
	unsigned char *rgb = idc->rgb;

	/* Check each palette index */
	for (i = 0; i < 256; i++)
	{
		/* Gamma-correct each RGB intensity */
		r = gamma_correct(rgb[i * 3], gamma);
		g = gamma_correct(rgb[i * 3 + 1], gamma);
		b = gamma_correct(rgb[i * 3 + 2], gamma);

		/* Find nearest palette index for RGB values */
		tint[i] = IndexedColor_RGB2Index(idc, r, g, b);
	}
}

/*
 * Returns the given value adjusted for brightness.
 * This is usually called on each component of an RGB value.
 */
int brightness_value(int intensity, int value)
{
	double brightValue = intensity;

	/* Darken */
	if (brightValue < 0)
	{
		return (unsigned char) ((value * (255 + brightValue)) / 255);
	}
	
	/* Lighten */
	else
	{
		return (unsigned char) (value + ((255 - value) * brightValue) / 255);
	}
}

/*
 * Adjusts the given 256 palette indices for the given intensity.
 * 'Intensity' can be from -127 to +127 inclusive.
 */
void IndexedColor_BrightnessTable(IndexedColor *idc, int intensity, TintTable table)
{
	int i, r, g, b;
	unsigned char *rgb = idc->rgb;

	for (i = 0; i < 256; i++)
	{
		/* Get the i'th palette index */
		int n = table[i];

		/* Get the n'th RGB values */
		r = rgb[n * 3 + 0];
		g = rgb[n * 3 + 1];
		b = rgb[n * 3 + 2];

		/* Adjust brightness */
		r = brightness_value(intensity, r);
		g = brightness_value(intensity, g);
		b = brightness_value(intensity, b);
		
		/* Now we have the RGB value, convert it into a palette index */
		table[i] = IndexedColor_RGB2Index(idc, r, g, b);
	}
}

/*
 * Returns the given value adjusted for contrast.
 * This is usually called on each component of an RGB value.
 */
int contrast_value(int intensity, int value)
{
	int v;
	double contrastValue = intensity;
	double x, y;

#define CLIPVALUE(a,x,y) ((a < x) ? x : ((a > y) ? y : a))

	/* Less contrast */
	if (contrastValue < 0)
	{
		v = (value > 127) ? (255 - value) : value;
		x = (v ? v : 1) / 127.0;
		y = (127 + contrastValue) / 127.0;
		v = (int) (127.0 * pow(x, y));
		v = CLIPVALUE(v, 0, 255);
		return (value > 127) ? (255 - v) : v;
	}
	
	/* More contrast */
	else
	{
		v = (value > 127) ? (255 - value) : value;
		x = v / 127.0;
		y = (contrastValue == 127) ? 127 : 127.0 / (127 - contrastValue);
		v = (int) (127.0 * pow(x, y));
		v = CLIPVALUE(v, 0, 255);
		return (value > 127) ? (255 - v) : v;
	}
}

/*
 * Adjusts the given 256 palette indices for the given contrast.
 * 'Intensity' can be from -127 to +127 inclusive.
 */
void IndexedColor_ContrastTable(IndexedColor *idc, int intensity, TintTable table)
{
	int i, r, g, b;
	unsigned char *rgb = idc->rgb;
	
	for (i = 0; i < 256; i++)
	{
		/* Get the i'th palette index */
		int n = table[i];

		/* Get the n'th RGB values */
		r = rgb[n * 3 + 0];
		g = rgb[n * 3 + 1];
		b = rgb[n * 3 + 2];

		/* Adjust contrast */
		r = contrast_value(intensity, r);
		g = contrast_value(intensity, g);
		b = contrast_value(intensity, b);
				
		/* Now we have the RGB value, convert it into a palette index */
		table[i] = IndexedColor_RGB2Index(idc, r, g, b);
	}
}

/*
 * Create a tint table that maps to the same palette index
 */
void IndexedColor_One2OneTable(IndexedColor *idc, TintTable table)
{
	int i;

	for (i = 0; i < 256; i++)
	{
		table[i] = i;
	}
}

/* nearest $color */
int objcmd_palette_nearest(ClientData clientData, Tcl_Interp *interp,
	int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	XColor *xColorPtr;
	int nearest;

	xColorPtr = Tk_AllocColorFromObj(interp, Tk_MainWindow(interp), objV[1]);
	if (xColorPtr == NULL)
	{
		return TCL_ERROR;
	}

	nearest = Palette_RGB2Index(
		((double) xColorPtr->red / USHRT_MAX) * 255,
		((double) xColorPtr->green / USHRT_MAX) * 255,
		((double) xColorPtr->blue / USHRT_MAX) * 255);

	Tk_FreeColor(xColorPtr);

	Tcl_SetObjResult(interp, Tcl_NewIntObj(nearest));

	return TCL_OK;
}

/* set $index ?$color? */
int objcmd_palette_set(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	int i, r, g, b;

	if (Tcl_GetIntFromObj(interp, objV[1], &i) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((i < 0) || (i >= 256))
	{
		return TCL_ERROR;
	}

	if (objC == 3)
	{
		XColor *xColorPtr = Tk_AllocColorFromObj(interp, Tk_MainWindow(interp), objV[2]);
		if (xColorPtr == NULL)
		{
			return TCL_ERROR;
		}
		g_palette.rgb[i * 3] = ((double) xColorPtr->red / USHRT_MAX) * 255;
		g_palette.rgb[i * 3 + 1] = ((double) xColorPtr->green / USHRT_MAX) * 255;
		g_palette.rgb[i * 3 + 2] = ((double) xColorPtr->blue / USHRT_MAX) * 255;
		Tk_FreeColor(xColorPtr);
		return TCL_OK;
	}

	r = g_palette.rgb[i * 3];
	g = g_palette.rgb[i * 3 + 1];
	b = g_palette.rgb[i * 3 + 2];
	FormatResult(interp, "#%02X%02X%02X", r, g, b);

	return TCL_OK;
}

static CommandInit commandInit[] = {
	{0, "palette", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "nearest", 2, 2, "color", objcmd_palette_nearest, (ClientData) 0},
		{1, "set", 2, 3, "?color?", objcmd_palette_set, (ClientData) 0},
	{0, NULL, 0, 0, NULL, NULL, 0}
};

int Palette_Init(Tcl_Interp *interp, char *fileName)
{
	FILE *fp;
	int count = 0, r, g, b;
	char buf[80];
	unsigned char *rgb;

	if (Palette_Initialized) return TCL_OK;

	rgb = g_palette.rgb;
	g_palette.platData = NULL;

	/* Try to open the palette file */
	if ((fp = fopen(fileName, "r")) == NULL)
	{
		FormatResult(interp, "can't open palette file \"%s\"", fileName);
		return TCL_ERROR;
	}

	/* Read each line until done */
	while (!feof(fp))
	{
		/* Get a line */
		if (!fgets(buf, 80, fp)) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;

		/* Require RGB triplet */
		if (sscanf(buf, "%d %d %d", &r, &g, &b) != 3) continue;

		/* Write this color to the array */
		*rgb++ = r, *rgb++ = g, *rgb++ = b;

		/* Count the colors */
		count++;
	}

	/* Close the file */
	fclose(fp);

	/* Require 256 colors exactly */
	if (count != 256)
	{
		/* Fatal error */
		FormatResult(interp, "expected 256 colors, got \"%d\"", count);
		return TCL_ERROR;
	}

	g_palette.platData = Plat_PaletteInit(g_palette.rgb);

	(void) CommandInfo_Init(interp, commandInit, NULL);

	Palette_ResetHash();

	Palette_Initialized = 1;

	Colormap_Init(interp);

	return TCL_OK;
}

#if 0

#ifdef PLATFORM_X11

int Palette_Init(Tcl_Interp *interp, char *fileName)
{
	FILE *fp;
	int count = 0, r, g, b;
	char buf[80], errorMsg[512];
	unsigned char *rgb;

	if (Palette_Initialized) return TCL_OK;

	rgb = g_palette.rgb;

	/* Try to open the palette file */
	if ((fp = fopen(fileName, "r")) == NULL)
	{
		FormatResult(interp, "can't open palette file \"%s\"", fileName);
		return TCL_ERROR;
	}

	/* Read each line until done */
	while (!feof(fp))
	{
		/* Get a line */
		if (!fgets(buf, 80, fp)) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;

		/* Require RGB triplet */
		if (sscanf(buf, "%d %d %d", &r, &g, &b) != 3) continue;

		/* Write this color to the array */
		*rgb++ = r, *rgb++ = g, *rgb++ = b;

		/* Count the colors */
		count++;
	}

	/* Close the file */
	fclose(fp);

	/* Require 256 colors exactly */
	if (count != 256)
	{
		/* Fatal error */
		(void) sprintf(errorMsg,
			"expected 256 colors, got \"%d\"", count);
		return TCL_ERROR;
	}

	(void) CommandInfo_Init(interp, commandInit, NULL);

	Palette_ResetHash();

	Palette_Initialized = 1;

Colormap_Init(interp);

	return TCL_OK;
}

#endif /* PLATFORM_X11 */

#endif /* 0 */

void Palette_ResetHash(void)
{
	IndexedColor_ResetHash(&g_palette);
}

/*
 * Returns the nearest matching palette index for the given
 * RGB values.
 */
int Palette_RGB2Index(unsigned char r, unsigned char g, unsigned char b)
{
	return IndexedColor_RGB2Index(&g_palette, r, g, b);
}

/*
 * Calculate the 256 palette indices that the palette index 'tint'
 * looks like at the given opacity against each of the standard
 * palette colors.
 */
void Palette_TintTable(int tint, int opacity, TintTable table)
{
	IndexedColor_TintTable(&g_palette, tint, opacity, table);
}

/* 
 * Writes 256 palette indices into the given tint table. Each
 * index is calculated by applying a gamma correction to each
 * RGB value of the global palette, then finding the nearest
 * palette index for the gamma-corrected RGB.
 */
void Palette_GammaTable(double gamma, unsigned char *tint)
{
	IndexedColor_GammaTable(&g_palette, gamma, tint);
}

#ifdef PLATFORM_WIN

void *Palette_GetHPal(void)
{
	return g_palette.platData; /* HPALETTE */
}

#endif /* PLATFORM_WIN */

unsigned char *Palette_GetRGB(void)
{
	return g_palette.rgb;
}

void DoubleLink_Init(DoubleLinker *linker, DoubleLink *link, void *data)
{
	if (!link)
	{
		linker->count = 0;
		linker->what = "unknown";
		linker->head = NULL;
		linker->tail = NULL;
	}

	if (link)
	{
		link->isLinked = FALSE;
		link->linker = linker;
		link->prev = NULL;
		link->next = NULL;
		link->data = data;
	}
}

void DoubleLink_Link(DoubleLink *link)
{
	DoubleLinker *linker = link->linker;

	if (link->isLinked) return;

	if (linker->head == NULL)
	{
		linker->head = link;
	}
	else
	{
		link->prev = linker->tail;
		linker->tail->next = link;
	}
	link->isLinked = TRUE;
	linker->tail = link;
	++linker->count;
	
	if (debug_widgets)
		dbwin("DoubleLink_Link %s: count=%d\n", linker->what, linker->count);
}

void DoubleLink_Unlink(DoubleLink *link)
{
	DoubleLinker *linker = link->linker;
	
	if (!link->isLinked) return;
	
	if (link->prev)
		link->prev->next = link->next;
	if (link->next)
		link->next->prev = link->prev;

	if (linker->head == link)
	{
		linker->head = link->next;
		if (!linker->head)
			linker->tail = NULL;
	}
	if (linker->tail == link)
		linker->tail = link->prev;

	link->prev = NULL;
	link->next = NULL;
	link->isLinked = FALSE;
	--linker->count;

if (!linker->count)
	if (linker->head != NULL || linker->tail != NULL)
		Tcl_Panic("linker \"%s\" count is zero, but head=%ld tail=%ld",
			linker->what, linker->head, linker->tail);
			
	if (debug_widgets)
		dbwin("DoubleLink_Unlink %s: count=%d\n", linker->what, linker->count);
}

#if 1

void Bitmap_New(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	Plat_BitmapNew(interp, bitmapPtr);
}

void Bitmap_Delete(BitmapPtr bitmapPtr)
{
	Plat_BitmapDelete(bitmapPtr);
}

#else /* not 1 */

#ifdef PLATFORM_WIN

void Bitmap_New16or24(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	int depth = bitmapPtr->depth;
	BITMAPINFO *infoPtr;

	/* Hack -- See Tcl_AllocDebug() */
	((int *) bitmapPtr)[0] = TCL_FREE_MAGIC;
	((int *) bitmapPtr)[1] = sizeof(BitmapType);

	/* Allocate temp storage */
	infoPtr = (BITMAPINFO *) Tcl_Alloc(sizeof(BITMAPINFO));

	/* Set header fields */
	infoPtr->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	infoPtr->bmiHeader.biWidth = bitmapPtr->width;
	infoPtr->bmiHeader.biHeight = -bitmapPtr->height; /* Top-down */
	infoPtr->bmiHeader.biPlanes = 1;
	infoPtr->bmiHeader.biBitCount = depth; /* 16 or 24 */
	infoPtr->bmiHeader.biCompression = BI_RGB;
	infoPtr->bmiHeader.biSizeImage = 0;
	infoPtr->bmiHeader.biXPelsPerMeter = 0;
	infoPtr->bmiHeader.biYPelsPerMeter = 0;
	infoPtr->bmiHeader.biClrUsed = 0;
	infoPtr->bmiHeader.biClrImportant = 0;

	/* Create the bitmap, and get the address of the bits */
	bitmapPtr->hbm = CreateDIBSection(NULL /* bitmapPtr->hdc */, infoPtr,
		DIB_RGB_COLORS, (LPVOID) &bitmapPtr->pixelPtr, NULL, 0);

	/* Free temp storage */
	Tcl_Free((char *) infoPtr);

	bitmapPtr->pixelSize = depth / 8;
	bitmapPtr->pitch = bitmapPtr->width * bitmapPtr->pixelSize;

	if (bitmapPtr->pitch % sizeof(LONG))
		bitmapPtr->pitch += sizeof(LONG) - bitmapPtr->pitch % sizeof(LONG);

	/* Now create a Pixmap */
	bitmapPtr->twd.type = TWD_BITMAP;
	bitmapPtr->twd.bitmap.depth = depth;
	bitmapPtr->twd.bitmap.handle = bitmapPtr->hbm;
	bitmapPtr->twd.bitmap.colormap = Tk_Colormap(Tk_MainWindow(interp));
	bitmapPtr->pixmap = (Pixmap) &bitmapPtr->twd;
}

/*
 * Create an 8-bits-per-pixel bitmap.
 * bitmapPtr->width and bitmapPtr->height must be set on input with
 * the desired size (in pixels) of the new bitmap.
 */
void Bitmap_New(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	int depth = bitmapPtr->depth;
	BITMAPINFO *infoPtr;
	PALETTEENTRY entries[256];
	int i;

	if (depth != 8)
	{
		Bitmap_New16or24(interp, bitmapPtr);
		return;
	}

/*	if (debug_widgets) dbwin("AngTk_CreateBitmap: width=%d height=%d\n",
		bitmapPtr->width, bitmapPtr->height); */

	/* Hack -- See Tcl_AllocDebug() */
	((int *) bitmapPtr)[0] = TCL_FREE_MAGIC;
	((int *) bitmapPtr)[1] = sizeof(BitmapType);

	/* Allocate temp storage */
	infoPtr = (BITMAPINFO *) Tcl_Alloc(sizeof(BITMAPINFOHEADER) + 
		256 * sizeof(RGBQUAD));

	/* Set header fields */
	infoPtr->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	infoPtr->bmiHeader.biWidth = bitmapPtr->width;
	infoPtr->bmiHeader.biHeight = -bitmapPtr->height; /* Top-down */
	infoPtr->bmiHeader.biPlanes = 1;
	infoPtr->bmiHeader.biBitCount = 8;
	infoPtr->bmiHeader.biCompression = BI_RGB;
	infoPtr->bmiHeader.biSizeImage = 0;
	infoPtr->bmiHeader.biXPelsPerMeter = 0;
	infoPtr->bmiHeader.biYPelsPerMeter = 0;
	infoPtr->bmiHeader.biClrUsed = 256;
	infoPtr->bmiHeader.biClrImportant = 0;

	/* Get the 256 standard colors as PALETTEENTRIES */
    GetPaletteEntries(g_palette.hPal, 0, 256, entries);

	/* Copy the colors to the bitmap color table */
	for (i = 0; i < 256; i++)
	{
		infoPtr->bmiColors[i].rgbRed = entries[i].peRed;
		infoPtr->bmiColors[i].rgbGreen = entries[i].peGreen;
		infoPtr->bmiColors[i].rgbBlue = entries[i].peBlue;
		infoPtr->bmiColors[i].rgbReserved = 0;
	}		

	/* Create the bitmap, and get the address of the bits */
	bitmapPtr->hbm = CreateDIBSection(NULL /* bitmapPtr->hdc */, infoPtr,
		DIB_RGB_COLORS, (LPVOID) &bitmapPtr->pixelPtr, NULL, 0);

	/* Free temp storage */
	Tcl_Free((char *) infoPtr);

	bitmapPtr->pixelSize = depth / 8;
	bitmapPtr->pitch = bitmapPtr->width * bitmapPtr->pixelSize;

	if (bitmapPtr->pitch % sizeof(LONG))
		bitmapPtr->pitch += sizeof(LONG) - bitmapPtr->pitch % sizeof(LONG);

	/* Create a Colormap */
	bitmapPtr->twc.palette = Palette_GetHPal();

	/* Now create a Pixmap */
	bitmapPtr->twd.type = TWD_BITMAP;
	bitmapPtr->twd.bitmap.depth = depth;
	bitmapPtr->twd.bitmap.handle = bitmapPtr->hbm;
	bitmapPtr->twd.bitmap.colormap = (Colormap) &bitmapPtr->twc;
	bitmapPtr->pixmap = (Pixmap) &bitmapPtr->twd;
}

#ifdef USE_STARTUP_LOG
extern void startup_log(char *fmt, ...);
#endif

/*
 * Delete a bitmap.
 */
void Bitmap_Delete(BitmapPtr bitmapPtr)
{
	int i, size;

/* if (debug_widgets) dbwin("AngTk_DeleteBitmap\n"); */
#ifdef USE_STARTUP_LOG
startup_log("AngTk_DeleteBitmap: entry");
#endif

	/* Hack -- See Tcl_FreeDebug() */
	if (((int *) bitmapPtr)[0] != TCL_FREE_MAGIC)
		Tcl_Panic("Tcl_FreeDebug: magic number not found");
	size = ((int *) bitmapPtr)[1];

#ifdef USE_STARTUP_LOG
startup_log("AngTk_DeleteBitmap: size is %d", size);
#endif

	DeleteObject(bitmapPtr->hbm);

#ifdef USE_STARTUP_LOG
startup_log("AngTk_DeleteBitmap: DeleteObject() okay");
#endif

	/* Hack -- See Tcl_FreeDebug() */
	for (i = 0; i < size; i++)
		((char *) bitmapPtr)[i] = 0;	

#ifdef USE_STARTUP_LOG
startup_log("AngTk_DeleteBitmap: leave");
#endif
}

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

#include <errno.h>

static int ErrorHandler(ClientData clientData, XErrorEvent *errEventPtr)
{
    int *anyError = (int *) clientData;
    *anyError = 1;
    return 0;
}

void Bitmap_New(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	int depth = bitmapPtr->depth;
	Display *display = tk_display;
	int screenNum = XDefaultScreen(display);
	Tk_ErrorHandler handler;
	Window root = RootWindow(display, screenNum);
	Visual *visual = DefaultVisual(display, screenNum);
	int ret, anyError;

	/* Verify shared-memory pixmaps are available */
	{
		int major, minor;
		Bool pixmaps;
		XShmQueryVersion(display, &major, &minor, &pixmaps);
		if (pixmaps != True)
		{
			Tcl_Panic("no shared pixmaps");
		}
	}

	if (bitmapPtr->width < 1) bitmapPtr->width = 1;
	if (bitmapPtr->height < 1) bitmapPtr->height = 1;

dbwin("Bitmap_New: width=%d height=%d\n", bitmapPtr->width, bitmapPtr->height);
dbwin("red=%#lx green=%#lx blue=%#lx\n", visual->red_mask, visual->green_mask, visual->blue_mask);

	/* Create shared-memory image. */
    bitmapPtr->ximage = XShmCreateImage(display, visual, depth, ZPixmap, NULL,
		&bitmapPtr->shminfo, bitmapPtr->width, bitmapPtr->height);

	/* Allocate shared memory */
    ret = bitmapPtr->shminfo.shmid = shmget(IPC_PRIVATE,
		bitmapPtr->ximage->bytes_per_line * bitmapPtr->ximage->height,
		IPC_CREAT | 0777);
	if (ret < 0)
	{
		printf("shmget: errno is %s (%d): %s\n", Tcl_ErrnoId(), errno, Tcl_ErrnoMsg(errno));
		Tcl_Panic("shmget() failed");
	}

    ret = (int) bitmapPtr->shminfo.shmaddr =
    	shmat(bitmapPtr->shminfo.shmid, 0, 0);
    if(ret == -1)
    {
		printf("shmat: errno is %s (%d): %s\n", Tcl_ErrnoId(), errno, Tcl_ErrnoMsg(errno));
		shmctl(bitmapPtr->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("shmat() failed");
	}

	/* Allow the server to write into our pixmap */
	bitmapPtr->shminfo.readOnly = False;

    anyError = 0;
    handler = Tk_CreateErrorHandler(display, -1, -1, -1, ErrorHandler,
    	(ClientData) &anyError);
    ret = XShmAttach(display, &bitmapPtr->shminfo);
    if(ret != True)
    {
		shmdt(bitmapPtr->shminfo.shmaddr);
		shmctl(bitmapPtr->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("XShmAttach() failed");
    }
	XSync(display, False);
    Tk_DeleteErrorHandler(handler);
    if (anyError)
    {
		shmdt(bitmapPtr->shminfo.shmaddr);
		shmctl(bitmapPtr->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("XShmAttach() etc gave errors");
    }

    ret = shmctl(bitmapPtr->shminfo.shmid, IPC_RMID, 0);
    if(ret < 0)
    {
		XShmDetach(display, &bitmapPtr->shminfo);
		shmdt(bitmapPtr->shminfo.shmaddr);
		shmctl(bitmapPtr->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("shmctl() failed");
    }

	/* Image uses shared memory we allocated */
    bitmapPtr->ximage->data = bitmapPtr->shminfo.shmaddr;

	/* Create shared-memory Pixmap */
    bitmapPtr->pixmap = XShmCreatePixmap(display, root,
    	bitmapPtr->shminfo.shmaddr, &bitmapPtr->shminfo,
    	bitmapPtr->width, bitmapPtr->height, depth);

	if (bitmapPtr->pixmap == None)
	{
		Tcl_Panic("XShmCreatePixmap() failed");
	}

	/* Set pitch, pixelSize, and pixelPtr */
	bitmapPtr->pitch = bitmapPtr->ximage->bytes_per_line;
	bitmapPtr->pixelSize = bitmapPtr->ximage->bits_per_pixel / 8;
	bitmapPtr->pixelPtr = bitmapPtr->shminfo.shmaddr;
}

#include <X11/Xutil.h>

void Bitmap_Delete(BitmapPtr bitmapPtr)
{
	Display *display = tk_display;

	XShmDetach(display, &bitmapPtr->shminfo);
	XDestroyImage(bitmapPtr->ximage);
	XFreePixmap(display, bitmapPtr->pixmap);
	shmdt(bitmapPtr->shminfo.shmaddr);
	shmctl(bitmapPtr->shminfo.shmid, IPC_RMID, 0);
}

#endif /* PLATFORM_X11 */

#endif /* 0 */

void FormatResult(Tcl_Interp *interp, char *fmt, ...)
{
	va_list ap;
	char buf[1024];

	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);
	va_end(ap);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);
}

Tcl_Obj *ExtToUtf_NewStringObj(CONST char *bytes, int length)
{
	char *utfString;
	Tcl_DString utfDString;
	Tcl_Obj *objResult;

	utfString = Tcl_ExternalToUtfDString(NULL, bytes, length, &utfDString);
	objResult = Tcl_NewStringObj(utfString, Tcl_DStringLength(&utfDString));
	Tcl_DStringFree(&utfDString);
	return objResult;
}

void ExtToUtf_SetResult(Tcl_Interp *interp, char *string)
{
	char *utfString;
	Tcl_DString utfDString;

	utfString = Tcl_ExternalToUtfDString(NULL, string, -1, &utfDString);
	Tcl_SetResult(interp, utfString, TCL_VOLATILE);
	Tcl_DStringFree(&utfDString);
}

char *UtfToExt_TranslateFileName(Tcl_Interp *interp, char *utfPath, Tcl_DString *extDStringPtr)
{
	char *extString, *utfString;
	Tcl_DString utfDString;
	int utfLen;

	utfString = Tcl_TranslateFileName(interp, utfPath, &utfDString);
	if (utfString == NULL) return NULL;
	utfLen = Tcl_DStringLength(&utfDString);
	extString = Tcl_UtfToExternalDString(NULL, utfString, utfLen, extDStringPtr);
	Tcl_DStringFree(&utfDString);
	return extString;
}



#ifdef PLATFORM_WIN

#if 0 /* NOT USED */

#include <tkFont.h>

#define FONTMAP_SHIFT	    10

#define FONTMAP_PAGES	    	(1 << (sizeof(Tcl_UniChar)*8 - FONTMAP_SHIFT))
#define FONTMAP_BITSPERPAGE	(1 << FONTMAP_SHIFT)

typedef struct FontFamily {
    struct FontFamily *nextPtr;	/* Next in list of all known font families. */
    int refCount;		/* How many SubFonts are referring to this
				 * FontFamily.  When the refCount drops to
				 * zero, this FontFamily may be freed. */
    /*
     * Key.
     */
     
    Tk_Uid faceName;		/* Face name key for this FontFamily. */

    /*
     * Derived properties.
     */
     
    Tcl_Encoding encoding;	/* Encoding for this font family. */
    int isSymbolFont;		/* Non-zero if this is a symbol font. */
    int isWideFont;		/* 1 if this is a double-byte font, 0 
				 * otherwise. */
    BOOL (WINAPI *textOutProc)(HDC, int, int, TCHAR *, int);
				/* The procedure to use to draw text after
				 * it has been converted from UTF-8 to the 
				 * encoding of this font. */
    BOOL (WINAPI *getTextExtentPoint32Proc)(HDC, TCHAR *, int, LPSIZE);
				/* The procedure to use to measure text after
				 * it has been converted from UTF-8 to the 
				 * encoding of this font. */

    char *fontMap[FONTMAP_PAGES];
				/* Two-level sparse table used to determine
				 * quickly if the specified character exists.
				 * As characters are encountered, more pages
				 * in this table are dynamically added.  The
				 * contents of each page is a bitmask
				 * consisting of FONTMAP_BITSPERPAGE bits,
				 * representing whether this font can be used
				 * to display the given character at the
				 * corresponding bit position.  The high bits
				 * of the character are used to pick which
				 * page of the table is used. */

    /*
     * Cached Truetype font info.
     */
     
    int segCount;		/* The length of the following arrays. */
    USHORT *startCount;		/* Truetype information about the font, */
    USHORT *endCount;		/* indicating which characters this font
				 * can display (malloced).  The format of
				 * this information is (relatively) compact,
				 * but would take longer to search than 
				 * indexing into the fontMap[][] table. */
} FontFamily;

typedef struct SubFont {
    char **fontMap;		/* Pointer to font map from the FontFamily, 
				 * cached here to save a dereference. */
    HFONT hFont;		/* The specific screen font that will be
				 * used when displaying/measuring chars
				 * belonging to the FontFamily. */
    FontFamily *familyPtr;	/* The FontFamily for this SubFont. */
} SubFont;

#define SUBFONT_SPACE		3

typedef struct WinFont {
    TkFont font;		/* Stuff used by generic font package.  Must
				 * be first in structure. */
    SubFont staticSubFonts[SUBFONT_SPACE];
				/* Builtin space for a limited number of
				 * SubFonts. */

	/*** Missing fields here ***/
} WinFont ;

HFONT TkToWinFont(Tk_Font tkFont)
{
	return ((WinFont *) tkFont)->staticSubFonts[0].hFont;
}

#endif /* 0 */

#endif /* PLATFORM_WIN */

static IndexedColor g_colormap;
unsigned char *g_colormap_rgb;

#ifdef PLATFORM_SDL
#define XQueryColor(display,colormap,xColor) {(xColor)->red=(xColor)->green=(xColor)->blue=0;}
#endif /* PLATFORM_SDL */

int Colormap_Init(Tcl_Interp *interp)
{
#ifdef PLATFORM_X11
	Tk_Window tkwin = Tk_MainWindow(interp);
	Display *display = Tk_Display(tkwin);
	Colormap colormap = Tk_Colormap(tkwin); /* DefaultColormap() */
	XColor xColor;
#endif
	int i, k, r, g, b;

	IndexedColor_ResetHash(&g_colormap);

#ifdef PLATFORM_X11
	if (Tk_Depth(tkwin) == 8)
	{
		/*
		 * Allocate each color in the colormap, to prevent the colormap
		 * entries from changing.
		 */
		for (i = 0; i < 256; i++)
		{
			xColor.pixel = i;
			XQueryColor(display, colormap, &xColor);
			(void) Tk_GetColorByValue(tkwin, &xColor);
		}
	}
#endif /* PLATFORM_X11 */

	for (i = 0; i < 256; i++)
	{
		r = g_palette.rgb[i * 3 + 0];
		g = g_palette.rgb[i * 3 + 1];
		b = g_palette.rgb[i * 3 + 2];

#ifdef PLATFORM_X11
		if (Tk_Depth(tkwin) == 8)
		{
			/* Get the XColor at this location in the colormap */
			xColor.pixel = i;
			XQueryColor(display, colormap, &xColor);
	
			/* Convert RGB values to 0-255 */
			r = ((double) xColor.red) / USHRT_MAX * 255;
			g = ((double) xColor.green) / USHRT_MAX * 255;
			b = ((double) xColor.blue) / USHRT_MAX * 255;
		}
#endif /* PLATFORM_X11 */

		/* Remember RGB values at this colormap index */
		g_colormap.rgb[i * 3 + 0] = r;
		g_colormap.rgb[i * 3 + 1] = g;
		g_colormap.rgb[i * 3 + 2] = b;

		/* Find the closest color in the palette */
		k = Palette_RGB2Index(r, g, b);

		/* Map colormap index -> palette index */
		g_colormap2palette[i] = k;
	}

	for (i = 0; i < 256; i++)
	{
		/* Get the RGB values at this palette index */
		r = g_palette.rgb[i * 3 + 0];
		g = g_palette.rgb[i * 3 + 1];
		b = g_palette.rgb[i * 3 + 2];		

		/* Find the closest color in the colormap */
		k = Colormap_RGB2Index(r, g, b);

		/* Map palette index -> colormap index */
		g_palette2colormap[i] = k;
	}

	/* Get the black and white pixels */
	g_colormap_black = PALETTE_BLACK;
	g_colormap_white = PALETTE_WHITE;

#ifdef PLATFORM_X11
	if (Tk_Depth(tkwin) == 8)
	{
		g_colormap_black = BlackPixelOfScreen(Tk_Screen(tkwin));
		g_colormap_white = WhitePixelOfScreen(Tk_Screen(tkwin));
	}
#endif /* PLATFORM_X11 */

	g_colormap_rgb = g_colormap.rgb;

	return TCL_OK;
}

unsigned char *Colormap_GetRGB(void)
{
	return g_colormap.rgb;
}

int Colormap_RGB2Index(unsigned char r, unsigned char g, unsigned char b)
{
	return IndexedColor_RGB2Index(&g_colormap, r, g, b);
}

void Colormap_GammaTable(double gamma, TintTable table)
{
	IndexedColor_GammaTable(&g_colormap, gamma, table);
}

void Colormap_TintTable(int tint, int opacity, TintTable table)
{
	IndexedColor_TintTable(&g_colormap, tint, opacity, table);
}

void Colormap_BrightnessTable(int intensity, TintTable table)
{
	IndexedColor_BrightnessTable(&g_colormap, intensity, table);
}

void Colormap_ContrastTable(int intensity, TintTable table)
{
	IndexedColor_ContrastTable(&g_colormap, intensity, table);
}

void Colormap_One2OneTable(TintTable table)
{
	IndexedColor_One2OneTable(&g_colormap, table);
}

#if 0 /* NOT USED */

/*
 * XXX Hack -- Check to see if the colormap changed
 * That would be bad, because all our icons would look technicolor.
 */
void Colormap_Check(Tcl_Interp *interp)
{
#ifdef PLATFORM_X11
	Tk_Window tkwin = Tk_MainWindow(interp);
	Display *display = Tk_Display(tkwin);
	Colormap colormap = Tk_Colormap(tkwin); /* DefaultColormap() */
	XColor xColor;
#endif
	int i, r, g, b;

	dbwin("Colormap_Check()\n");

	for (i = 0; i < 256; i++)
	{
#ifdef PLATFORM_X11
		/* Get the XColor at this location in the colormap */
		xColor.pixel = i;
		XQueryColor(display, colormap, &xColor);

		/* Convert RGB values to 0-255 */
		r = ((double) xColor.red) / USHRT_MAX * 255;
		g = ((double) xColor.green) / USHRT_MAX * 255;
		b = ((double) xColor.blue) / USHRT_MAX * 255;
#endif /* */

#ifdef PLATFORM_WIN
		PALETTEENTRY entry;
		HDC hdc = GetDC(NULL);
		GetSystemPaletteEntries(hdc, i, 1, &entry);
		ReleaseDC(NULL, hdc);
		r = entry.peRed;
		g = entry.peGreen;
		b = entry.peBlue;
#endif /* PLATFORM_WIN */

		if (g_colormap.rgb[i * 3 + 0] == r &&
			g_colormap.rgb[i * 3 + 1] == g &&
			g_colormap.rgb[i * 3 + 2] == b) continue;

		dbwin("XXX colormap %d changed: was %d/%d/%d is %d/%d/%d\n", i,
			g_colormap.rgb[i * 3 + 0],
			g_colormap.rgb[i * 3 + 1],
			g_colormap.rgb[i * 3 + 2],
			r, g, b);
	}
}

#endif /* 0 */
