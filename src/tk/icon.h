/* File: icon.h */

/* Purpose: icon environment definitions */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_ICON_H_
#define _INCLUDE_ICON_H_

typedef struct IconSpec {
	int type;
	int index;
} IconSpec;

extern int g_icon_depth; /* 8, 16 or 24 */
extern long g_icon_length;

/* 32x32 x 32bits = 32x32x4 */
#define ICON_LENGTH_MAX (4096L * 4)

typedef byte IconData[ICON_LENGTH_MAX];
typedef byte *IconPtr;

typedef union PixelPtr
{
	byte *pix8;
	u16b *pix16;
	u32b *pix24;
} PixelPtr;

typedef struct t_icon_data {
	cptr desc; /* type name */
	IconPtr icon_data; /* Address of icon data */
	int icon_count; /* Number of icons */

	int depth; /* Bits per pixel (8, 16, 24) */
	int bypp; /* Bytes per pixel (1, 2, 3 or 4) */
	int width; /* Pixels per column */
	int height; /* Pixels per row */
	int pitch; /* Convenience: width * bypp */
	int length; /* Convenience: width * height * bypp */
	int pixels; /* Convenience: width * height */
} t_icon_data;

extern t_icon_data *g_icon_data; /* Array of icon types */
extern int g_icon_data_count; /* Number of icon types */

extern void PixelSet_RGB(IconPtr dst, int r, int g, int b, int bypp);

extern int Icon_Init(Tcl_Interp *interp, int size, int depth);


/*
 * Constants for g_icon_data[] index.
 * icon_type.type constants.
 */

/* Special type "none". It's a transparent icon. */
#define ICON_TYPE_NONE 0

#define ICON_TYPE_BLANK 1
#define ICON_TYPE_DEFAULT 2

/* One assigned icon */
typedef struct t_assign_icon {
	int type;
	int index;
} t_assign_icon;


/*
 * Icon assignment for each member of each group (monster,
 * object, features, etc)
 */
typedef struct t_assign_group {
	int count; /* Number of elements in array */
	t_assign_icon *assign; /* Array of iassignments */
} t_assign_group;


extern byte *g_palette_rgb;

#endif /* _INCLUDE_ICON_H_ */
