/* File: readdib.h */

/*
 * This file has been modified for use with "Angband 2.8.2"
 *
 * COPYRIGHT:
 *
 *   (C) Copyright Microsoft Corp. 1991.  All rights reserved.
 *
 *   You have a royalty-free right to use, modify, reproduce and
 *   distribute the Sample Files (and/or any modified version) in
 *   any way you find useful, provided that you agree that
 *   Microsoft has no warranty obligations or liability for any
 *   Sample Application Files which are modified.
 *
 *   I Konijn found it useful to distribute this code under the terms of the GNU General Public License (version 2),
 *   as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 *   or under the terms of the traditional Angband license. 
 */

/*
 * Information about a bitmap
 */
typedef struct {
	HANDLE   hDIB;
	HBITMAP  hBitmap;
	HPALETTE hPalette;
	BYTE     CellWidth;
	BYTE     CellHeight;
} DIBINIT;

/* Read a DIB from a file */
extern BOOL ReadDIB(HWND, LPSTR, DIBINIT *);

/* Free a DIB */
extern void FreeDIB(DIBINIT *dib);
