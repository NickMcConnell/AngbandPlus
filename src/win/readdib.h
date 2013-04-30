/*
 * File: readdib.h
 * Purpose: DIB support
 */

#ifndef INCLUDED_READDIB_H
#define INCLUDED_READDIB_H

/*
 * Information about a bitmap
 */
typedef struct
{
    HANDLE hDIB;
    HBITMAP  hBitmap;
    HPALETTE hPalette;
    BYTE CellWidth;
    BYTE CellHeight;
    int ImageWidth;
    int ImageHeight;
} DIBINIT;

/* Read a DIB from a file */
extern BOOL ReadDIB(HWND, LPSTR, DIBINIT *);

/* Free a DIB */
extern void FreeDIB(DIBINIT *dib);

/* New png stuff */
extern BOOL ReadDIB2_PNG(HWND, LPSTR, DIBINIT *, DIBINIT *, BOOL);
extern BOOL SaveWindow_PNG(HWND hWnd, LPSTR lpFileName);

#endif /* INCLUDED_READDIB_H */
