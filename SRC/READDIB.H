/* File: readdib.h */

/*
 * This file has been modified for use with "Angband 2.8.2"
 *
 * Copyright 1991 Microsoft Corporation. All rights reserved.
 */

/*
 * Information about a bitmap
 */
typedef struct {
	HANDLE hDIB;
	HANDLE hBitmap;
	HANDLE hPalette;
	BYTE   CellWidth;
	BYTE   CellHeight;
} DIBINIT;

/* Read a DIB from a file */
BOOL ReadDIB(HWND, LPSTR, DIBINIT *);
