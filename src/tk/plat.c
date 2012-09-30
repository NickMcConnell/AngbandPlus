/* File: plat.c */

/* Purpose: platform-specific stuff */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "tnb.h"

#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

#ifdef PLATFORM_WIN

/* #include <tkWinInt.h> */

/* HPALETTE in windows headers */
extern void *Palette_GetHPal(void);

struct PlatBitmap
{
	HBITMAP hbm; /* Bitmap */
	TkWinDrawable twd; /* Pixmap */
	TkWinColormap twc; /* Colormap */
};

void Bitmap_New(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	int depth = bitmapPtr->depth;
	struct PlatBitmap *platData;
	BITMAPINFO *infoPtr;
	unsigned char *rgb;
	int i;

	MAKE(platData, PlatBitmap);

	if (depth == 8)
	{
		/* Allocate temp storage */
		C_MAKE(infoPtr, sizeof(BITMAPINFOHEADER) + 256 * sizeof(RGBQUAD), byte);
	
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
		MAKE(infoPtr, BITMAPINFO);
	
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
	FREE(infoPtr);

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

void Bitmap_Delete(BitmapPtr bitmapPtr)
{
	struct PlatBitmap *platData = bitmapPtr->platData;

	DeleteObject(platData->hbm);

	FREE(platData);
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

#ifdef PLATFORM_X11

#include <X11/extensions/XShm.h>
#include <X11/Xutil.h>

#ifdef HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif
#ifdef HAVE_SYS_SHM_H
#include <sys/shm.h>
#endif

/* #include <tkIntPlatDecls.h> */

struct PlatBitmap
{
	Display *display;
	XShmSegmentInfo shminfo;
	XImage *ximage;
};

static int ErrorHandler(ClientData clientData, XErrorEvent *errEventPtr)
{
    int *anyError = (int *) clientData;

	/* Hack - ignore parameter */
	(void) errEventPtr;

    *anyError = 1;
    return 0;
}

void Bitmap_New(Tcl_Interp *interp, BitmapPtr bitmapPtr)
{
	int depth = bitmapPtr->depth;
	struct PlatBitmap *platData;
	Display *display = Tk_Display(Tk_MainWindow(interp));
	int screenNum = XDefaultScreen(display);
	Tk_ErrorHandler handler;
	Window root = RootWindow(display, screenNum);
	Visual *visual = DefaultVisual(display, screenNum);
	int ret, anyError;

	MAKE(platData, struct PlatBitmap);

#ifdef HAVE_SHMGET
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
#endif /* HAVE_SHMGET */

	if (bitmapPtr->width < 1) bitmapPtr->width = 1;
	if (bitmapPtr->height < 1) bitmapPtr->height = 1;

#ifdef HAVE_SHMGET
	/* Create shared-memory image. */
    platData->ximage = XShmCreateImage(display, visual, depth, ZPixmap, NULL,
		&platData->shminfo, bitmapPtr->width, bitmapPtr->height);

	/* Allocate shared memory */
    ret = platData->shminfo.shmid = shmget(IPC_PRIVATE,
		platData->ximage->bytes_per_line * platData->ximage->height,
		IPC_CREAT | 0777);
	if (ret < 0)
	{
		Tcl_Panic("shmget() failed");
	}

	platData->shminfo.shmaddr = shmat(platData->shminfo.shmid, 0, 0);
	ret = (int) platData->shminfo.shmaddr;
	
    if(ret == -1)
    {
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

#else /* HAVE_SHMGET */

	platData->ximage = XCreateImage(display, visual, depth, ZPixmap, 0, NULL,
		bitmapPtr->width, bitmapPtr->height, 32, 0);
	platData->ximage->data = malloc(platData->ximage->bytes_per_line * platData->ximage->height);

	bitmapPtr->pixmap = XCreatePixmap(display, root, bitmapPtr->width,
		bitmapPtr->height, depth);

#endif /* !HAVE_SHMGET */

	/* Set pitch, pixelSize, and pixelPtr */
	bitmapPtr->pitch = platData->ximage->bytes_per_line;
	bitmapPtr->pixelSize = platData->ximage->bits_per_pixel / 8;
	bitmapPtr->pixelPtr = (unsigned char *) platData->shminfo.shmaddr;

	platData->display = display;

	bitmapPtr->platData = platData;
}

void Bitmap_Delete(BitmapPtr bitmapPtr)
{
	struct PlatBitmap *platData = bitmapPtr->platData;
	Display *display = platData->display;

#ifdef HAVE_SHMGET
	XShmDetach(display, &platData->shminfo);
	XDestroyImage(platData->ximage);
	XFreePixmap(display, bitmapPtr->pixmap);
	shmdt(platData->shminfo.shmaddr);
	shmctl(platData->shminfo.shmid, IPC_RMID, 0);
#else
	XDestroyImage(platData->ximage);
	XFreePixmap(display, bitmapPtr->pixmap);
#endif

	FREE(platData);
	bitmapPtr->platData = NULL;
}

void *Plat_PaletteInit(unsigned char *rgb)
{
	/* Hack - just return NULL */
	(void) rgb;
	return NULL;
}

#endif /* PLATFORM_X11 */

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

#ifdef HAVE_TKPSYNC
extern void TkpSync (Display * display);
#endif

void Plat_SyncDisplay(Display *display)
{
#ifdef HAVE_TKPSYNC
	TkpSync(display);
#endif
}


/*
 * The Win32 "BITMAPFILEHEADER" type.
 */
typedef struct BITMAPFILEHEADER
{
	u16b bfType;
	u32b bfSize;
	u16b bfReserved1;
	u16b bfReserved2;
	u32b bfOffBits;
} BITMAPFILEHEADER;


/*
 * The Win32 "BITMAPINFOHEADER" type.
 */
typedef struct BITMAPINFOHEADER
{
	u32b biSize;
	u32b biWidth;
	u32b biHeight;
	u16b biPlanes;
	u16b biBitCount;
	u32b biCompresion;
	u32b biSizeImage;
	u32b biXPelsPerMeter;
	u32b biYPelsPerMeter;
	u32b biClrUsed;
	u32b biClrImportand;
} BITMAPINFOHEADER;

/*
 * The Win32 "RGBQUAD" type.
 */
typedef struct RGBQUAD
{
	unsigned char b, g, r;
	unsigned char filler;
} RGBQUAD;


/*** Helper functions for system independent file loading. ***/

static byte get_byte(FILE *fff)
{
	/* Get a character, and return it */
	return (getc(fff) & 0xFF);
}

static void rd_byte(FILE *fff, byte *ip)
{
	*ip = get_byte(fff);
}

static void rd_u16b(FILE *fff, u16b *ip)
{
	(*ip) = get_byte(fff);
	(*ip) |= ((u16b)(get_byte(fff)) << 8);
}

static void rd_u32b(FILE *fff, u32b *ip)
{
	(*ip) = get_byte(fff);
	(*ip) |= ((u32b)(get_byte(fff)) << 8);
	(*ip) |= ((u32b)(get_byte(fff)) << 16);
	(*ip) |= ((u32b)(get_byte(fff)) << 24);
}


/*
 * Read a Win32 BMP file.
 *
 * Assumes that the bitmap has a size such that no padding is needed in
 * various places.
 */
static u32b *ReadBMP(cptr Name, int *bw, int *bh)
{
	FILE *f;

	BITMAPFILEHEADER fileheader;
	BITMAPINFOHEADER infoheader;

	u32b *Data32;
	u32b *p;
	u32b pixel;
	
	/* Palette of bitmap */
	byte *pal = NULL;

	int ncol;

	int total;

	int i;

	int x, y;

	/* Open the BMP file */
	f = fopen(Name, "r");

	/* No such file */
	if (!f)
	{
		quit_fmt("No bitmap to load! (%s)", Name);
	}

	/* Read the "BITMAPFILEHEADER" */
	rd_u16b(f, &(fileheader.bfType));
	rd_u32b(f, &(fileheader.bfSize));
	rd_u16b(f, &(fileheader.bfReserved1));
	rd_u16b(f, &(fileheader.bfReserved2));
	rd_u32b(f, &(fileheader.bfOffBits));

	/* Read the "BITMAPINFOHEADER" */
	rd_u32b(f, &(infoheader.biSize));
	rd_u32b(f, &(infoheader.biWidth));
	rd_u32b(f, &(infoheader.biHeight));
	rd_u16b(f, &(infoheader.biPlanes));
	rd_u16b(f, &(infoheader.biBitCount));
	rd_u32b(f, &(infoheader.biCompresion));
	rd_u32b(f, &(infoheader.biSizeImage));
	rd_u32b(f, &(infoheader.biXPelsPerMeter));
	rd_u32b(f, &(infoheader.biYPelsPerMeter));
	rd_u32b(f, &(infoheader.biClrUsed));
	rd_u32b(f, &(infoheader.biClrImportand));

	/* Verify the header */
	if (feof(f) ||
	    (fileheader.bfType != 19778) ||
	    (infoheader.biSize != 40))
	{
		quit_fmt("Incorrect BMP file format %s", Name);
	}

	/* The two headers above occupy 54 bytes total */
	/* The "bfOffBits" field says where the data starts */
	/* The "biClrUsed" field does not seem to be reliable */
	/* Compute number of colors recorded */
	ncol = (fileheader.bfOffBits - 54) / 4;
	
	
	if (ncol)
	{
		/* Create palette */
		C_MAKE(pal, ncol * 3, byte);
	}
	
	for (i = 0; i < ncol; i++)
	{
		RGBQUAD clrg;

		/* Read an "RGBQUAD" */
		rd_byte(f, &(clrg.b));
		rd_byte(f, &(clrg.g));
		rd_byte(f, &(clrg.r));
		rd_byte(f, &(clrg.filler));

		/* Analyze the color */
		pal[i * 3] = clrg.b;
		pal[i * 3 + 1] = clrg.g;
		pal[i * 3 + 2] = clrg.r;
	}
	
	/* Look for illegal bitdepths. */
	if ((infoheader.biBitCount == 1) || (infoheader.biBitCount == 4))
	{
		quit_fmt("Illegal biBitCount %d in %s",
			 infoheader.biBitCount, Name);
	}

	/* Determine total bytes needed for image */
	total = infoheader.biWidth * (infoheader.biHeight + 2);

	/*
	 * Allocate image memory
	 * (plus a tiny bit extra so we can use aligned 32bit accesses)
	 */
	C_MAKE(Data32, total + 4, u32b);
	p = Data32;
		
	for (y = 0; y < (int) infoheader.biHeight; y++)
	{
		for (x = 0; x < (int) infoheader.biWidth; x++)
		{
			int ch = getc(f);
			
			p = Data32 + x + (infoheader.biHeight - 1 - y) * infoheader.biWidth;

			/* Verify not at end of file XXX XXX */
			if (feof(f)) quit_fmt("Unexpected end of file in %s", Name);

			if (infoheader.biBitCount == 8)
			{
				pixel = pal[ch * 3];
				pixel = pixel * 256 + pal[ch * 3 + 1];
				pixel = pixel * 256 + pal[ch * 3 + 2];
				*p = pixel;
			}
			else if (infoheader.biBitCount == 24)
			{
				pixel = ch;
				ch = getc(f);

				/* Verify not at end of file XXX XXX */
				if (feof(f)) quit_fmt("Unexpected end of file in %s", Name);
				pixel = pixel * 256 + ch;
				ch = getc(f);

				/* Verify not at end of file XXX XXX */
				if (feof(f)) quit_fmt("Unexpected end of file in %s", Name);
				pixel = pixel * 256 + ch;

				*p = pixel;
			}
		}
	}

	fclose(f);
	
	/* Save the size for later */
	*bw = infoheader.biWidth;
	*bh = infoheader.biHeight;

	/* Free palette */
	FREE(pal);

	/*
	 * Return data
	 */
	return (Data32);
}

/*
 * Actually load a bitmap
 */
BitmapPtr Bitmap_Load(Tcl_Interp *interp, cptr name)
{
	BitmapPtr bitmapPtr;
	
	u32b *data;
	
	int i, j;
	
	MAKE(bitmapPtr, BitmapType);
	
	/* Want a 24bit bitmap */
	bitmapPtr->depth = 24;
	
	data = ReadBMP(name, &bitmapPtr->width, &bitmapPtr->height);

	Bitmap_New(interp, bitmapPtr);
	
	/* Copy in the data */
	if (bitmapPtr->pixelSize == 3)
	{
		byte *d8 = (byte *) data;
	
		byte *p = (byte *) bitmapPtr->pixelPtr;
	
		/* We need to copy each part seperately */
		for (i = 0; i < bitmapPtr->height; i++)
		{
			for (j = 0; j < bitmapPtr->width; j++)
			{
				/* I bet this is wrong... */
			
				*p++ = *d8++;
				*p++ = *d8++;
				*p++ = *d8++;
				d8++;
			}
		}
	}
	else if (bitmapPtr->pixelSize == 4)
	{
		C_COPY(bitmapPtr->pixelPtr, data, bitmapPtr->pitch * bitmapPtr->height, byte);
	}
	
	FREE(data);
	
	return (bitmapPtr);
}


/*
 * Load a font
 */
BitmapPtr Font_Load(Tcl_Interp *interp, cptr name, int size)
{
	BitmapPtr bitmapPtr;
	
	FILE *fp;
	
	int i, j, k, m;
	
	int num = 0, error_idx = -1;
	
	char buf[1024];
	char line[16];
	
	byte *pixel;
	
	/* Open the BMP file */
	fp = fopen(name, "r");

	/* No such file */
	if (fp == NULL)
	{
		return (NULL);
	}
	
	/* Create bitmap for font */
	MAKE(bitmapPtr, BitmapType);
	
	/* Want a 8bit bitmap */
	bitmapPtr->depth = 8;
	
	/* 128 characters */
	bitmapPtr->width = 128 * size;
	bitmapPtr->height = size;
	
	/* Actually allocate the bitmap */
	Bitmap_New(interp, bitmapPtr);
	
	
	/* Paraonia */
	if (!bitmapPtr)
	{
		fclose(fp);
		return (NULL);
	}
		/* Reset the counters for use in parsing the font data */
	i = 0;
	j = 16;
	
	/* Process the file */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Count lines */
		num++;

		/* Skip "empty" lines */
		if (!buf[0]) continue;

		/* Skip "blank" lines */
		if (isspace(buf[0])) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;

		/* Look at the line */
		
		/* Verify correct "colon" format */
		if (buf[1] != ':')
		{
			quit_fmt("Incorrect font file format on line %d", num);
		}

		/* Get number */
		if (buf[0] == 'N')
		{
			/* Get the index */
			i = atoi(buf+2);
		
			/* Verify information */
			if (i <= error_idx)
			{
				quit_fmt("Incorrect font file numbering on line %d", num);
			}

			error_idx = i;
			
			/* Verify information */
			if (i >= 128)
			{
				quit_fmt("Incorrect font file numbering on line %d", num);
			}
			
			/* Verify information */
			if (j != size)
			{
				quit_fmt("Incorrect font size on line %d", num);
			}
			
			/* Start from the top */
			j = 0;
		}
		
		/* Get font data */
		if (buf[0] == 'F')
		{
			/* Verify information */
			if (j >= size)
			{
				quit_fmt("Incorrect font size length on line %d", num);
			}
			
			if (((int) strlen(buf)) != size + 2)
			{
				quit_fmt("Incorrect font size width on line %d", num);
			}
			
			/* Create the line */
			for (k = 0; k < size; k++)
			{
				line[k] = buf[k + 2];
			}
			
			/* Copy it to the bitmap */
			for (m = 0; m < size; m++)
			{
				pixel = get_icon_ptr(bitmapPtr, i * size + m, j);
			
				if (line[m] == '*')
				{
					/* Coloured pixel */
					*pixel = 255;
				}
				else
				{
					/* Blank pixel */
					*pixel = 0;
				}
			}
			
			/* Next line of the character */
			j++;
		}
	}

	/* Close the file */
	my_fclose(fp);

	/* Paranoia - does the file end early? */
	if ((i != 127) || (j != size)) return (NULL);
	
	return (bitmapPtr);
}
