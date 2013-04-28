/* File: plat-dll.c */

/* Purpose: platform-specific stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "util-dll.h"
#include "plat-dll.h"

#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

extern int g_icon_depth;

#ifdef PLATFORM_WIN

#include <tkWinInt.h>

struct PlatBitmap
{
	HBITMAP hbm; /* Bitmap */
	TkWinDrawable twd; /* Pixmap */
	TkWinColormap twc; /* Colormap */
};

void Plat_BitmapNew(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	int depth = bitmapPtr->depth;
	struct PlatBitmap *platData;
	BITMAPINFO *infoPtr;
	unsigned char *rgb;
	int i;

	platData = (struct PlatBitmap *) Tcl_AllocDebug(sizeof(struct PlatBitmap));

	if (depth == 8)
	{
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

		rgb = Palette_GetRGB(); /* Colormap_ ? */
	
		/* Copy the colors to the bitmap color table */
		for (i = 0; i < 256; i++)
		{
			infoPtr->bmiColors[i].rgbRed = *rgb++;
			infoPtr->bmiColors[i].rgbGreen = *rgb++;
			infoPtr->bmiColors[i].rgbBlue = *rgb++;
			infoPtr->bmiColors[i].rgbReserved = 0;
		}		
	
		/* Create the bitmap, and get the address of the bits */
		platData->hbm = CreateDIBSection(NULL, infoPtr,
			DIB_RGB_COLORS, (LPVOID) &bitmapPtr->pixelPtr, NULL, 0);

		/* Create a Colormap */
		platData->twc.palette = Palette_GetHPal();

		/* Colormap */
		platData->twd.bitmap.colormap = (Colormap) &platData->twc;
	}
	else
	{
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
		platData->hbm = CreateDIBSection(NULL, infoPtr,
			DIB_RGB_COLORS, (LPVOID) &bitmapPtr->pixelPtr, NULL, 0);

		platData->twd.bitmap.colormap = Tk_Colormap(Tk_MainWindow(interp));
	}
	
	/* Free temp storage */
	Tcl_Free((char *) infoPtr);

	bitmapPtr->pixelSize = bitmapPtr->depth / 8;
	bitmapPtr->pitch = bitmapPtr->width * bitmapPtr->pixelSize;

	if (bitmapPtr->pitch % 4)
		bitmapPtr->pitch += 4 - bitmapPtr->pitch % 4;

	/* Now create a Pixmap */
	platData->twd.type = TWD_BITMAP;
	platData->twd.bitmap.depth = depth;
	platData->twd.bitmap.handle = platData->hbm;
	bitmapPtr->pixmap = (Pixmap) &platData->twd;

	bitmapPtr->platData = platData;
}

void Plat_BitmapDelete(BitmapPtr bitmapPtr)
{
	struct PlatBitmap *platData = bitmapPtr->platData;

	DeleteObject(platData->hbm);

	Tcl_FreeDebug((char *) platData);
	bitmapPtr->platData = NULL;
}

typedef struct _PALETTE
{
	WORD Version;
	WORD NumberOfEntries;
	PALETTEENTRY aEntries[256];
} _PALETTE;

void *Plat_PaletteInit(unsigned char *rgb)
{
	int i;

	/* The 'logical' palette we will use */
	/* "0x300" = Windows 3.0 or later */
	/* "256" = Number of colors */
	_PALETTE LogicalPalette = {0x300, 256};

	for (i = 0; i < 256; i++)
	{
		LogicalPalette.aEntries[i].peRed = *rgb++;
		LogicalPalette.aEntries[i].peGreen = *rgb++;
		LogicalPalette.aEntries[i].peBlue = *rgb++;
		LogicalPalette.aEntries[i].peFlags = PC_NOCOLLAPSE;
	}
	
    return CreatePalette((LPLOGPALETTE) &LogicalPalette);
}

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_SDL

#include <SDL/SDL.h>
#include <tkPort.h>

/* X11 Pixmap internal rep */
/* OH DEAR GOD MAKE SURE THIS MATCHES SDLTk */
typedef struct _Pixmap
{
    int type; /* must be first */
#ifdef TK_NO_DOUBLE_BUFFERING
	Region clip;
#endif
    SDL_Surface *sdl; /* SDLTK_RGB24 etc */
    int format;
} _Pixmap;

struct PlatBitmap
{
	Display *display;
};

void Plat_BitmapNew(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	struct PlatBitmap *platData;
	int depth = bitmapPtr->depth;
	Display *display = Tk_Display(Tk_MainWindow(interp));
	int screenNum = Tk_ScreenNumber(Tk_MainWindow(interp));
	Window root = RootWindow(display, screenNum);
	_Pixmap pixmap;

	platData = (struct PlatBitmap *) Tcl_AllocDebug(sizeof(struct PlatBitmap));

	if (bitmapPtr->width < 1) bitmapPtr->width = 1;
	if (bitmapPtr->height < 1) bitmapPtr->height = 1;

	bitmapPtr->pixmap = XCreatePixmap(display, root,
		bitmapPtr->width, bitmapPtr->height, depth);

	/* Set pitch, pixelSize, and pixelPtr */
	bitmapPtr->pitch = ((_Pixmap *) bitmapPtr->pixmap)->sdl->pitch;
	bitmapPtr->pixelSize = ((_Pixmap *) bitmapPtr->pixmap)->sdl->format->BytesPerPixel;
	/* This is bogus.  Gotta SDL_LockSurface() this shiz. */
	bitmapPtr->pixelPtr = ((_Pixmap *) bitmapPtr->pixmap)->sdl->pixels;

	platData->display = display;
	bitmapPtr->platData = platData;
}

void Plat_BitmapDelete(BitmapPtr bitmapPtr)
{
	struct PlatBitmap *platData = bitmapPtr->platData;
	Display *display = platData->display;

	XFreePixmap(display, bitmapPtr->pixmap);
	Tcl_FreeDebug((char *) platData);
	bitmapPtr->platData = NULL;
}

unsigned char *Plat_BitmapLock(BitmapPtr bitmapPtr)
{
}

void Plat_BitmapUnlock(BitmapPtr bitmapPtr)
{
}

void *Plat_PaletteInit(unsigned char *rgb)
{
	return NULL;
}

#undef PLATFORM_X11

#endif /* PLATFORM_SDL */

#ifdef PLATFORM_X11

#include <errno.h>
#include <X11/extensions/XShm.h>
#include <sys/ipc.h>
#include <sys/shm.h>

struct PlatBitmap
{
	Display *display;
	XShmSegmentInfo shminfo;
	XImage *ximage;
};

static int ErrorHandler(ClientData clientData, XErrorEvent *errEventPtr)
{
	int *anyError = (int *) clientData;
	*anyError = 1;
	return 0;
}

void Plat_BitmapNew(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	int depth = bitmapPtr->depth;
	struct PlatBitmap *platData;
	Display *display = Tk_Display(Tk_MainWindow(interp));
	int screenNum = XDefaultScreen(display);
	Tk_ErrorHandler handler;
	Window root = RootWindow(display, screenNum);
	Visual *visual = DefaultVisual(display, screenNum);
	int ret, anyError;

	platData = (struct PlatBitmap *) Tcl_AllocDebug(sizeof(struct PlatBitmap));

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
    platData->ximage = XShmCreateImage(display, visual, depth, ZPixmap, NULL,
		&platData->shminfo, bitmapPtr->width, bitmapPtr->height);

	/* Allocate shared memory */
    ret = platData->shminfo.shmid = shmget(IPC_PRIVATE,
		platData->ximage->bytes_per_line * platData->ximage->height,
		IPC_CREAT | 0777);
	if (ret < 0)
	{
		printf("shmget: errno is %s (%d): %s\n", Tcl_ErrnoId(), errno, Tcl_ErrnoMsg(errno));
		Tcl_Panic("shmget() failed");
	}

	ret = (int) (platData->shminfo.shmaddr =
		shmat(platData->shminfo.shmid, 0, 0));
	if(ret == -1)
	{
		printf("shmat: errno is %s (%d): %s\n", Tcl_ErrnoId(), errno, Tcl_ErrnoMsg(errno));
		shmctl(platData->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("shmat() failed");
	}

	/* Allow the server to write into our pixmap */
	platData->shminfo.readOnly = False;

	anyError = 0;
	handler = Tk_CreateErrorHandler(display, -1, -1, -1, ErrorHandler,
		(ClientData) &anyError);
	ret = XShmAttach(display, &platData->shminfo);
	if(ret != True)
	{
		shmdt(platData->shminfo.shmaddr);
		shmctl(platData->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("XShmAttach() failed");
	}
	XSync(display, False);
	Tk_DeleteErrorHandler(handler);
	if (anyError)
	{
		shmdt(platData->shminfo.shmaddr);
		shmctl(platData->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("XShmAttach() etc gave errors");
	}

	ret = shmctl(platData->shminfo.shmid, IPC_RMID, 0);
	if(ret < 0)
	{
		XShmDetach(display, &platData->shminfo);
		shmdt(platData->shminfo.shmaddr);
		shmctl(platData->shminfo.shmid, IPC_RMID, 0);
		Tcl_Panic("shmctl() failed");
	}

	/* Image uses shared memory we allocated */
	platData->ximage->data = platData->shminfo.shmaddr;

	/* Create shared-memory Pixmap */
	bitmapPtr->pixmap = XShmCreatePixmap(display, root,
		platData->shminfo.shmaddr, &platData->shminfo,
		bitmapPtr->width, bitmapPtr->height, depth);

	if (bitmapPtr->pixmap == None)
	{
		Tcl_Panic("XShmCreatePixmap() failed");
	}

	/* Set pitch, pixelSize, and pixelPtr */
	bitmapPtr->pitch = platData->ximage->bytes_per_line;
	bitmapPtr->pixelSize = platData->ximage->bits_per_pixel / 8;
	bitmapPtr->pixelPtr = platData->shminfo.shmaddr;

	platData->display = display;

	bitmapPtr->platData = platData;
}

#include "tcltk-dll.h"

void Plat_BitmapDelete(BitmapPtr bitmapPtr)
{
	struct PlatBitmap *platData = bitmapPtr->platData;
	Display *display = platData->display;

	XShmDetach(display, &platData->shminfo);
	XDestroyImage(platData->ximage);
	XFreePixmap(display, bitmapPtr->pixmap);
	shmdt(platData->shminfo.shmaddr);
	shmctl(platData->shminfo.shmid, IPC_RMID, 0);

	Tcl_FreeDebug((char *) platData);
	bitmapPtr->platData = NULL;
}

void *Plat_PaletteInit(unsigned char *rgb)
{
	return NULL;
}

#endif /* PLATFORM_X11 */

#ifdef PLATFORM_SDL
#define PLATFORM_X11 /* undo #undef above */
#endif /* PLATFORM_SDL */

int Plat_XColor2Pixel(XColor *xColorPtr)
{
	if (xColorPtr == NULL)
		return 0;
#ifdef PLATFORM_X11
	return xColorPtr->pixel;
#endif
#ifdef PLATFORM_WIN
	switch (g_icon_depth)
	{
		case 8:
			return Colormap_RGB2Index(
				((double) xColorPtr->red / USHRT_MAX) * 255,
				((double) xColorPtr->green / USHRT_MAX) * 255,
				((double) xColorPtr->blue / USHRT_MAX) * 255);
		case 16:
		{
			unsigned short pix16;
			SetPix16((unsigned char *) &pix16,
				GetRValue(xColorPtr->pixel),
				GetGValue(xColorPtr->pixel),
				GetBValue(xColorPtr->pixel));
			return pix16;
		}
		case 24:
		{
			return (GetBValue(xColorPtr->pixel) << 16) |
				(GetGValue(xColorPtr->pixel) << 8) |
				GetRValue(xColorPtr->pixel);
		}
	}
#endif /* PLATFORM_WIN */

	return 0;
}

int Plat_RGB2XPixel(int r, int g, int b)
{
#ifdef PLATFORM_X11
	return 0;
#endif
#ifdef PLATFORM_WIN
	return RGB(r, g, b);
#endif
}

void Plat_SyncDisplay(Display *display)
{
#ifdef PLATFORM_X11
	TkpSync(display);
#endif
}

