/* File: icon.c */

/* Purpose: icon stuff */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "tnb.h"
#include "icon.h"

unsigned char *g_palette_rgb;

t_icon_data *g_icon_data; /* Array of icon types */
int g_icon_data_count = 0; /* Number of icon types */
Tcl_HashTable g_icon_table; /* Hash table for icon types */
long g_icon_length = 0; /* Length in bytes of one icon */
int g_icon_size = 0; /* Icon dimensions (16, 24 or 32) */
int g_icon_depth = 0; /* Icon depth (8, 16 or 24 bpp) */
int g_pixel_size; /* Num bytes per pixel (1, 2, 3 or 4) */
int g_icon_pixels; /* Num pixels per icon (16x16, 24x24, 32x32) */


int *g_background = NULL;

bool g_icon_map_changed = FALSE;


/*
 * Determine the icon type/index of a real icon type from the given
 * icon type/index. This routine is used to get the actual frame of
 * a sprite for example. Usually the given icon type/index is returned.
 */
void FinalIcon(IconSpec *iconOut, t_assign_icon *assignPtr)
{
	iconOut->type = assignPtr->type;
	iconOut->index = assignPtr->index;
}

void init_palette(void)
{
	if (Palette_Init(g_interp) != TCL_OK)
		quit(Tcl_GetStringResult(g_interp));

	g_palette_rgb = Palette_GetRGB();
}


char *AssignToString_Icon(char *buf, t_assign_icon *assign)
{
	strnfmt(buf, 128, "icon %s %d",
			g_icon_data[assign->type].desc,
			assign->index);

	return buf;
}

static int Icon_Validate(Tcl_Interp *interp, char *typeName, int index, IconSpec *specPtr)
{
	t_icon_data *iconDataPtr;
	int type;

	/* Look up the icon type by name */
	if (Icon_FindTypeByName(interp, &type, typeName) != TCL_OK)
	{
		return TCL_ERROR;
	}

	iconDataPtr = &g_icon_data[type];

	/* Verify index */
	if ((index < 0) || (index >= iconDataPtr->icon_count))
	{
		/* Set the error */
		Tcl_SetStringObj(Tcl_GetObjResult(interp),
			format("bad icon index \"%d\": must be from 0 to %d",
			index, iconDataPtr->icon_count - 1), -1);

		/* Failure */
		return TCL_ERROR;
	}

	specPtr->type = type;
	specPtr->index = index;

	/* Success */
	return TCL_OK;
}

static int StringToAssign_Icon(Tcl_Interp *interp, t_assign_icon *assignPtr, cptr desc)
{
	char option[64], typeName[64];
	IconSpec iconSpec;

	if (sscanf(desc, "%s %s %d", option, typeName, &iconSpec.index) < 3)
	{
		Tcl_SetResult(interp, format("malformed assignment \"%s\"",
			desc), TCL_VOLATILE);
		return TCL_ERROR;
	}

	if (Icon_Validate(interp, typeName, iconSpec.index, &iconSpec) != TCL_OK)
	{
		return TCL_ERROR;
	}

	assignPtr->type = iconSpec.type;
	assignPtr->index = iconSpec.index;

	return TCL_OK;
}


cptr keyword_assign_type[] = {"icon", NULL};

int assign_parse(Tcl_Interp *interp, t_assign_icon *assignPtr, cptr desc)
{
	char option[64];
	Tcl_Obj *objPtr;
	int assignType;

	/* Ex. "icon dungeon 10" */
	if (sscanf(desc, "%s", option) != 1)
	{
		Tcl_SetResult(interp, format("malformed assignment \"%s\"",
			desc), TCL_VOLATILE);
		return TCL_ERROR;
	}

	objPtr = Tcl_NewStringObj(option, -1);
	if (Tcl_GetIndexFromObj(interp, objPtr, keyword_assign_type,
		"option", 0, &assignType) != TCL_OK)
	{
		Tcl_DecrRefCount(objPtr);
		return TCL_ERROR;
	}
	Tcl_DecrRefCount(objPtr);

	return StringToAssign_Icon(interp, assignPtr, desc);
}


/*
 *
 */
static void Icon_AddType(t_icon_data *data)
{
	int new;
	t_icon_data iconData, *icon_data_ptr = &iconData;
	Tcl_HashEntry *hPtr;
	memset(icon_data_ptr, 0, sizeof(t_icon_data));
	icon_data_ptr->desc = string_make(data->desc);
	icon_data_ptr->icon_count = data->icon_count;
	icon_data_ptr->icon_data = data->icon_data;
	icon_data_ptr->depth = data->depth;
	icon_data_ptr->bypp = data->bypp;
	icon_data_ptr->width = data->width;
	icon_data_ptr->height = data->height;
	icon_data_ptr->pitch = data->pitch;
	icon_data_ptr->length = data->length;
	icon_data_ptr->pixels = data->pixels;

	g_icon_data = Array_Append(g_icon_data, &g_icon_data_count,
		sizeof(t_icon_data), icon_data_ptr);
	
	hPtr = Tcl_CreateHashEntry(&g_icon_table, data->desc, &new);
	Tcl_SetHashValue(hPtr, (ClientData) (g_icon_data_count - 1));
}


/*
 * Initialize the icon environment. This should be called once with
 * the desired dimensions of the icons to use (16x16, 24x24 or 32x32).
 */
void init_icons(int size, int depth)
{
	int i, n, y, x, y2, x2;
	t_assign_icon assign;
	t_icon_data icon_data, *icon_data_ptr = &icon_data;
	unsigned char *rgb = Colormap_GetRGB();

	/* Initialize the Icon library */
	if (Icon_Init(g_interp, size, depth) != TCL_OK)
	{
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));
	}

	/*
	 * The TYPE_NONE/"none" type icon is a single masked icon with an empty
	 * mask. It is suitable for equipment displays when no item is present
	 * in a slot.
	 */
	icon_data_ptr->desc = "none";
	icon_data_ptr->icon_count = 1;
	C_MAKE(icon_data_ptr->icon_data, g_icon_length, byte);
	for (i = 0; i < g_icon_length; i++)
	{
		icon_data_ptr->icon_data[i] = 0x00;
	}

	icon_data_ptr->depth = g_icon_depth;
	icon_data_ptr->bypp = g_pixel_size;
	icon_data_ptr->width = g_icon_size;
	icon_data_ptr->height = g_icon_size;
	icon_data_ptr->pitch = g_icon_size * g_pixel_size;
	icon_data_ptr->length = g_icon_size * g_icon_size * g_pixel_size;
	icon_data_ptr->pixels = g_icon_size * g_icon_size;

	Icon_AddType(icon_data_ptr);

	for (i = 0; i < g_icon_length; i++)
	{
		if (g_icon_depth != 8)
			icon_data_ptr->icon_data[i] = 0; /* Black (RGB 0,0,0) */
		else
			icon_data_ptr->icon_data[i] = COLORMAP_BLACK;
	}

	/*
	 * The TYPE_BLANK/"blank" icon type is a single black unmasked icon
	 */
	icon_data_ptr->desc = "blank";
	icon_data_ptr->icon_count = 1;
	C_MAKE(icon_data_ptr->icon_data, g_icon_length, byte);
	for (i = 0; i < g_icon_length; i++)
	{
		if (g_icon_depth != 8)
			icon_data_ptr->icon_data[i] = 0; /* Black (RGB 0,0,0) */
		else
			icon_data_ptr->icon_data[i] = COLORMAP_BLACK;
	}

	icon_data_ptr->depth = g_icon_depth;
	icon_data_ptr->bypp = g_pixel_size;
	icon_data_ptr->width = g_icon_size;
	icon_data_ptr->height = g_icon_size;
	icon_data_ptr->pitch = g_icon_size * g_pixel_size;
	icon_data_ptr->length = g_icon_size * g_icon_size * g_pixel_size;
	icon_data_ptr->pixels = g_icon_size * g_icon_size;

	Icon_AddType(icon_data_ptr);

	/*
	 * The TYPE_DEFAULT/"default" icon type is a single multicolored
	 * unmasked icon. If we see it, it probably means we forgot to
	 * assign an icon to something.
	 */
	icon_data_ptr->desc = "default";
	icon_data_ptr->icon_count = 1;
	C_MAKE(icon_data_ptr->icon_data, g_icon_length, byte);
	n = 0, y2 = 0;
	for (y = 0; y < 16; y++)
	{
		int dy = 0;
		if (g_icon_size == 24)
		{
			if (!(y & 1)) dy++;
		}
		if (g_icon_size == 32) dy++;
		x2 = 0;
		for (x = 0; x < 16; x++)
		{
			int dx = 0;
			if (g_icon_size == 24)
			{
				if (!(x & 1)) dx++;
			}
			if (g_icon_size == 32) dx++;
			PixelSet_RGB(icon_data_ptr->icon_data + (x2 * g_pixel_size) + (y2 * g_icon_size * g_pixel_size),
				rgb[0], rgb[1], rgb[2], g_pixel_size);
			PixelSet_RGB(icon_data_ptr->icon_data + ((x2 + dx) * g_pixel_size) + (y2 * g_icon_size * g_pixel_size),
				rgb[0], rgb[1], rgb[2], g_pixel_size);
			PixelSet_RGB(icon_data_ptr->icon_data + (x2 * g_pixel_size) + ((y2 + dy) * g_icon_size * g_pixel_size),
				rgb[0], rgb[1], rgb[2], g_pixel_size);
			PixelSet_RGB(icon_data_ptr->icon_data + ((x2 + dx) * g_pixel_size) + ((y2 + dy) * g_icon_size * g_pixel_size),
				rgb[0], rgb[1], rgb[2], g_pixel_size);
			rgb += 3;

			n++;
			x2 += dx ? 2 : 1;
		}
		y2 += dy ? 2 : 1;
	}

	icon_data_ptr->depth = g_icon_depth;
	icon_data_ptr->bypp = g_pixel_size;
	icon_data_ptr->width = g_icon_size;
	icon_data_ptr->height = g_icon_size;
	icon_data_ptr->pitch = g_icon_size * g_pixel_size;
	icon_data_ptr->length = g_icon_size * g_icon_size * g_pixel_size;
	icon_data_ptr->pixels = g_icon_size * g_icon_size;

	Icon_AddType(icon_data_ptr);


	assign.type = ICON_TYPE_DEFAULT;
	assign.index = 0;


	/*
	 * When a feature is masked, or a masked icon is drawn on
	 * a feature, we may use the icon assigned to a different feature
	 * as the background.
	 */
	C_MAKE(g_background, z_info->f_max, int);

	/* Clear the color hash table */
	Palette_ResetHash();
	
	if (init_widget(g_interp, g_icon_depth) != TCL_OK)
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));
	
	if (CanvasWidget_Init(g_interp) != TCL_OK)
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));
}


#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

int PixelPtrToLong(IconPtr p, int bypp);
void PixelLongToPtr(IconPtr dst, int pixel, int bypp);

/* Hack -- Standard 16 "term" colors. User should be able to change */
int g_term_palette[16] = {255, 0, 250, 17, 217, 196, 199, 101, 129,
	247, 30, 5, 35, 185, 180, 52};

/* Actual 8/16/24 pixel values for above */
unsigned long g_term_colormap[16];


static int InitPixelSize(Tcl_Interp *interp)
{
	BitmapType bitmap;

	bitmap.width = bitmap.height = 10;
	bitmap.depth = g_icon_depth;
	Bitmap_New(interp, &bitmap);
	g_pixel_size = bitmap.pixelSize;
	Bitmap_Delete(&bitmap);

	return TCL_OK;
}

RGBInfo g_rgbi;

static int CountBits(unsigned long mask)
{
	int n;

	for (n = 0; mask != 0; mask &= mask - 1)
		n++;
	return n;
}

static void InitRGBInfo(Tcl_Interp *interp)
{
	Tk_Window tkwin = Tk_MainWindow(interp);
	Visual *visual = Tk_Visual(tkwin);

	g_rgbi.red_mask = visual->red_mask;
	g_rgbi.green_mask = visual->green_mask;
	g_rgbi.blue_mask = visual->blue_mask;

#ifdef PLATFORM_WIN
	/* XXX Always 5-5-5 */
	g_rgbi.red_mask = 0x7c00;
	g_rgbi.green_mask = 0x03e0;
	g_rgbi.blue_mask = 0x001f;
#endif

	g_rgbi.red_count = CountBits(g_rgbi.red_mask);
	g_rgbi.green_count = CountBits(g_rgbi.green_mask);
	g_rgbi.blue_count = CountBits(g_rgbi.blue_mask);

	g_rgbi.red_shift = g_rgbi.red_count + g_rgbi.green_count + g_rgbi.blue_count - 8;
	g_rgbi.green_shift = g_rgbi.green_count + g_rgbi.blue_count - 8;
	g_rgbi.blue_shift = -(g_rgbi.blue_count - 8);

	g_rgbi.extra = ~(g_rgbi.red_mask | g_rgbi.green_mask | g_rgbi.blue_mask);
}


static void SetPix8(unsigned char *p, int r, int g, int b)
{
	/* NOTE: Not Colormap */
	*p = Palette_RGB2Index(r, g, b);
}

void SetPix16(unsigned char *p, int r, int g, int b)
{
	int r2 = (r << g_rgbi.red_shift) & g_rgbi.red_mask;
	int g2 = (g << g_rgbi.green_shift) & g_rgbi.green_mask;
	int b2 = (b >> g_rgbi.blue_shift) & g_rgbi.blue_mask;

	*((unsigned short *) p) = g_rgbi.extra | r2 | g2 | b2;
}

static void SetPix24(unsigned char *p, int r, int g, int b)
{
	*p++ = b;
	*p++ = g;
	*p++ = r;
	if (g_pixel_size == 4)
		*p++ = 0xFF;
}

void PixelSet_RGB(IconPtr dst, int r, int g, int b, int bypp)
{
	switch (bypp)
	{
		case 1:
			SetPix8(dst, r, g, b);
			break;
		case 2:
			SetPix16(dst, r, g, b);
			break;
		case 3:
		case 4:
			SetPix24(dst, r, g, b);
			break;
	}
}

void PixelLongToPtr(IconPtr p, int pixel, int bypp)
{
	switch (bypp)
	{
		case 1:
			*p = (unsigned char) pixel;
			break;
		case 2:
			*((unsigned short *) p) = pixel & 0xFFFF;
			break;
		case 3:
		{
			unsigned char *p2 = (unsigned char *) &pixel;
			*p++ = *p2++;
			*p++ = *p2++;
			*p++ = *p2++;
			break;
		}
		case 4:
			*((unsigned long *) p) = pixel;
			break;
	}
}

int PixelPtrToLong(IconPtr p, int bypp)
{
	switch (bypp)
	{
		case 1:
			return *p;
		case 2:
			return *(unsigned short *) p;
		case 3:
			return p[0] + (p[1] << 8) + (p[2] << 16); /* ??? */
		case 4:
			return *(int *) p;
	}
	return 0;
}

int Image2Bits(Tcl_Interp *interp, t_icon_data *iconDataPtr,
	Tk_PhotoHandle photoH, int imageW, int imageH, XColor *xColorPtr)
{
	Tk_PhotoImageBlock photoBlock;
	int row, col, y, x;
	int i, r, g, b;
	int r2 = 0, g2 = 0, b2 = 0;
	unsigned char *srcPtr, *dstPtr;
	int dataSize;
	int numCol, numRow;
	int iconW = iconDataPtr->width;
	int pixelSize = iconDataPtr->bypp;

	/* Hack - ignore parameter */
	(void) interp;

	/* Get info about the image */
	(void) Tk_PhotoGetImage(photoH, &photoBlock);

	/* Calculate the number of icons based on image dimensions */
	numCol = (photoBlock.width / imageW);
	numRow = (photoBlock.height / imageH);
	iconDataPtr->icon_count = numCol * numRow;

	/* Allocate icon buffer */
	dataSize = iconDataPtr->icon_count * iconDataPtr->length;
	C_MAKE(iconDataPtr->icon_data, dataSize, byte);

	if (pixelSize == 1)
	{
		/* Clear the color hash table used by Palette_RGB2Index() */
		Palette_ResetHash();
	}

	if (xColorPtr)
	{
		r2 = ((double) xColorPtr->red / USHRT_MAX) * 255;
		g2 = ((double) xColorPtr->green / USHRT_MAX) * 255;
		b2 = ((double) xColorPtr->blue / USHRT_MAX) * 255;
	}		

	dstPtr = iconDataPtr->icon_data;
	for (row = 0; row < numRow; row++)
	{
		for (col = 0; col < numCol; col++)
		{
			srcPtr = photoBlock.pixelPtr +
				col * imageW * photoBlock.pixelSize +
				row * imageH * photoBlock.pitch;
			for (y = 0; y < imageH; y++)
			{
				for (x = 0; x < imageW; x++)
				{
					r = *(srcPtr + x * photoBlock.pixelSize + photoBlock.offset[0]);
					g = *(srcPtr + x * photoBlock.pixelSize + photoBlock.offset[1]);
					b = *(srcPtr + x * photoBlock.pixelSize + photoBlock.offset[2]);
					if (xColorPtr)
					{
						/* Transparent */
						if ((r == r2) && (g == g2) && (b == b2))
						{
							/* Exclude from mask */
							r = g = b = 0;
						}
		
						/* Opaque */
						else
						{
							/* Include in mask */
							r = g = b = 0xFF;
						}
					}
					if (imageW != iconW)
					{
						if (iconW == 24)
						{
							if ((y & 1) && (x & 1))
							{
								PixelSet_RGB(dstPtr, r, g, b, pixelSize);
								dstPtr += pixelSize;
							}
							else if (y & 1)
							{
								PixelSet_RGB(dstPtr, r, g, b, pixelSize);
								dstPtr += pixelSize;
								PixelSet_RGB(dstPtr, r, g, b, pixelSize);
								dstPtr += pixelSize;
							}
							else if (x & 1)
							{
								PixelSet_RGB(dstPtr, r, g, b, pixelSize);
								PixelSet_RGB(dstPtr + iconW * pixelSize, r, g, b, pixelSize);
								dstPtr += pixelSize;
							}
							else
							{
								PixelSet_RGB(dstPtr, r, g, b, pixelSize);
								PixelSet_RGB(dstPtr + pixelSize, r, g, b, pixelSize);
								PixelSet_RGB(dstPtr + iconW * pixelSize, r, g, b, pixelSize);
								PixelSet_RGB(dstPtr + (iconW + 1) * pixelSize, r, g, b, pixelSize);
								dstPtr += 2 * pixelSize;
							}
						}
						if (iconW == 32)
						{
							PixelSet_RGB(dstPtr, r, g, b, pixelSize);
							PixelSet_RGB(dstPtr + pixelSize, r, g, b, pixelSize);
							PixelSet_RGB(dstPtr + iconW * pixelSize, r, g, b, pixelSize);
							PixelSet_RGB(dstPtr + (iconW + 1) * pixelSize, r, g, b, pixelSize);
							dstPtr += 2 * pixelSize;
						}
					}
					else
					{
						PixelSet_RGB(dstPtr, r, g, b, pixelSize);
						dstPtr += pixelSize;
					}
				}
				srcPtr += photoBlock.pitch;
				if (imageW != iconW)
				{
					if (iconW == 24)
					{
						if (!(y & 1))
							dstPtr += iconW * pixelSize;
					}
					if (iconW == 32)
					{
						dstPtr += iconW * pixelSize;
					}
				}
			}
		}
	}

	/* This step strips off blank icons at the end */
	{
		int whiteValue = 0xFF;
		int emptyIcon = TRUE;
		int n = iconDataPtr->icon_count - 1;

		if (pixelSize == 1)
			whiteValue = PALETTE_WHITE;
		while (emptyIcon)
		{
			srcPtr = iconDataPtr->icon_data + n * iconDataPtr->length;
			for (i = 0; i < iconDataPtr->length; i++)
			{
				if (*srcPtr++ != whiteValue)
				{
					emptyIcon = FALSE;
					break;
				}
			}
			if (emptyIcon)
			{
				iconDataPtr->icon_count -= 1;
				n--;
			}
		}
	}

	return TCL_OK;
}


/*
 * objcmd_icon --
 */
static int objcmd_icon(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static cptr cmdOption[] = {"count", "size",
		"depth", "height", "width", NULL};
	enum {IDX_COUNT, IDX_SIZE, IDX_DEPTH, IDX_HEIGHT, IDX_WIDTH} option;
	Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);

	t_icon_data *iconDataPtr;
	
	/* Hack - ignore parameter */
	(void) dummy;
	
	/* Required number of arguments */
    if (objc < 2)
    {
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objv[1], cmdOption, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
		case IDX_COUNT: /* count */
			if (objc != 3)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "typeName");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconDataPtr, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Return the number of icons */
			Tcl_SetIntObj(resultPtr, iconDataPtr->icon_count);
			break;
			
		case IDX_SIZE: /* size */
			Tcl_SetIntObj(resultPtr, g_icon_size);
			break;

		case IDX_DEPTH: /* depth */
			Tcl_SetIntObj(resultPtr, g_icon_depth);
			break;

		case IDX_HEIGHT: /* height */
		{
			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconDataPtr, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			Tcl_SetIntObj(resultPtr, iconDataPtr->height);
			break;
		}

		case IDX_WIDTH: /* width */
		{
			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconDataPtr, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			Tcl_SetIntObj(resultPtr, iconDataPtr->width);
			break;
		}
	}

	/* Success */
	return TCL_OK;
}


int Icon_FindTypeByName(Tcl_Interp *interp, int *typeIndexPtr, char *typeName)
{
	Tcl_HashEntry *hPtr;

	/* Look up the icon type by name */
	hPtr = Tcl_FindHashEntry(&g_icon_table, typeName);

	/* The icon type was not found */
	if (hPtr == NULL)
	{
		Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);

		/* Set the error */
		Tcl_AppendStringsToObj(resultPtr, "unknown icon type \"",
			typeName, "\"", NULL);

		/* Failure */
		return TCL_ERROR;
	}

	(*typeIndexPtr) = (int) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

int Icon_GetTypeFromObj(Tcl_Interp *interp, t_icon_data **typePtrPtr,
	Tcl_Obj *objPtr)
{
	int typeIndex;

	if (Icon_FindTypeByName(interp, &typeIndex, Tcl_GetString(objPtr))
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	(*typePtrPtr) = &g_icon_data[typeIndex];

	return TCL_OK;
}

int Icon_GetIndexFromObj(Tcl_Interp *interp, int *indexPtr,
	Tcl_Obj *objPtr, t_icon_data *iconDataPtr)
{
	int index;

	if (Tcl_GetIntFromObj(interp, objPtr, &index) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((index < 0) || (index >= iconDataPtr->icon_count))
	{
		/* Set the error */
		Tcl_SetStringObj(Tcl_GetObjResult(interp),
			format("bad icon index \"%d\": must be from 0 to %d",
			index, iconDataPtr->icon_count - 1), -1);

		/* Failure */
		return TCL_ERROR;
	}

	(*indexPtr) = index;

	return TCL_OK;
}

/*
 * Initialization.
 */
int Icon_Init(Tcl_Interp *interp, int size, int depth)
{
	/* Only initialize once */
	if (g_icon_size != 0) return TCL_OK;

	/* Require a known icon size */
	if ((size != 16) && (size != 24) && (size != 32))
	{
		Tcl_SetStringObj(Tcl_GetObjResult(interp),
			format("invalid icon size \"%d\": must be 16, 24 or 32", size), -1);
		return TCL_ERROR;
	}

	/* Require a known icon depth */
	if ((depth != 8) && (depth != 16) && (depth != 24))
	{
		Tcl_SetStringObj(Tcl_GetObjResult(interp),
			format("invalid icon depth \"%d\": must be 8, 16 or 24", depth), -1);
		return TCL_ERROR;
	}

	/* Remember the requested icon size */
	g_icon_size = size;

	/* Remember the requested icon depth */
	g_icon_depth = depth;

	if (InitPixelSize(interp) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Remember number of pixels in an icon */
	g_icon_pixels = size * size;

	/* Remember the number of bytes in an icon */
	g_icon_length = g_icon_pixels * g_pixel_size;

	/***** RETURN ERROR IF PALETTE NOT INITIALIZED *****/

	g_palette_rgb = Palette_GetRGB();

	if (g_icon_depth == 16) InitRGBInfo(interp);




	/* New block here */
	{
		int i, paletteIndex;
		for (i = 0; i < 16; i++)
		{
			paletteIndex = g_term_palette[i];
			switch (g_icon_depth)
			{
				case 8:
					g_term_colormap[i] = g_palette2colormap[paletteIndex];
					break;
				case 16:
				{
					unsigned short pix16;
					unsigned char *rgb = &g_palette_rgb[paletteIndex * 3];
					SetPix16((unsigned char *) &pix16, rgb[0], rgb[1], rgb[2]);
					g_term_colormap[i] = pix16;
					break;
				}
				case 24:
				{
					unsigned char *rgb = &g_palette_rgb[paletteIndex * 3];
					unsigned char *pix24 = (unsigned char *) &g_term_colormap[i];
					SetPix24(pix24 + 1, rgb[0], rgb[1], rgb[2]);
					break;
				}
			}
		}
	}
	
	/*
	 * This is an array of t_icon_data types, which specify
	 * the icon data and optional mask data for each type of
	 * icon. Icon types are defined through the "icon createtype"
	 * command.
	 */
	MAKE(g_icon_data, t_icon_data);
	g_icon_data_count = 0;

	/*
	 * This hash table maps symbolic names of icon types (as defined
	 * through the "icon createtype" command) to indexes into
	 * the g_icon_data[] array above.
	 */
	Tcl_InitHashTable(&g_icon_table, TCL_STRING_KEYS);

	Tcl_CreateObjCommand(interp, "icon", objcmd_icon, NULL, NULL);
	
	return TCL_OK;
}

void Icon_Exit(void)
{
	int i;
	
	if (g_icon_size == 0) return;

	/* Check each icon type */
	for (i = 0; i < g_icon_data_count; i++)
	{
		t_icon_data *iconDataPtr = &g_icon_data[i];

		/* Help the memory debugger */
		if (iconDataPtr->icon_data)
			FREE(iconDataPtr->icon_data);
	}
}
