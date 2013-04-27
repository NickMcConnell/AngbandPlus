
/* $Id: readdib.h,v 1.3 2003/03/17 22:45:28 cipher Exp $ */

/*
 * This file has been modified for use with "Angband 2.8.2"
 *
 * Copyright 1991 Microsoft Corporation. All rights reserved.
 */

/*
 * Information about a bitmap
 */
typedef struct
{
     HANDLE          hDIB;
     HBITMAP         hBitmap;
     HPALETTE        hPalette;
     BYTE            CellWidth;
     BYTE            CellHeight;
}
DIBINIT;

/* Read a DIB from a file */
extern BOOL     ReadDIB(HWND,
                        LPSTR,
                        DIBINIT *);

/* Free a DIB */
extern void     FreeDIB(DIBINIT * dib);
