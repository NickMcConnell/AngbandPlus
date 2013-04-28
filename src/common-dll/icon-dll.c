/* File: icon-dll.c */

/* Purpose: icon stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include "icon-dll.h"
#include "util-dll.h"
#include "plat-dll.h"
#include "dbwin.h"

#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

int PixelPtrToLong(IconPtr p, int bypp);
void PixelLongToPtr(IconPtr dst, int pixel, int bypp);

char *format TCL_VARARGS_DEF(char *, arg1)
{
	static char buf[512];
	va_list argList;
	char *format;

	format = TCL_VARARGS_START(char *, arg1, argList);
	vsprintf(buf, format, argList);
	va_end(argList);

	return buf;
}

char *common_dll_string_make(char *str)
{
	int len = 0;
	char *t = str;
	char *s, *res;

	/* Simple sillyness */
	if (!str) return (str);

	/* Get the number of chars in the string, including terminator */
	while (str[len++]) /* loop */;

	/* Allocate space for the string */
	s = res = (char*)(Tcl_Alloc(len));

	/* Copy the string (with terminator) */
	while ((*s++ = *t++) != 0) /* loop */;

	/* Return the allocated, initialized, string */
	return (res);
}

extern int objcmd_makeicon _ANSI_ARGS_((ClientData clientData,
			Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
static int init_ascii_data(Tcl_Interp *interp, t_icon_type *iconTypePtr);

DLLEXPORT t_icon_type *g_icon_type = NULL; /* Array of icon types */
DLLEXPORT int g_icon_type_count = 0; /* Number of icon types */
DLLEXPORT Tcl_HashTable g_icon_table; /* Hash table for icon types */
DLLEXPORT long g_icon_length = 0; /* Length in bytes of one icon */
DLLEXPORT int g_icon_width = 0; /* Icon dimensions (16, 24 or 32) */
DLLEXPORT int g_icon_height = 0; /* Icon dimensions (16, 24 or 32) */
DLLEXPORT int g_icon_depth = 0; /* Icon depth (8, 16 or 24 bpp) */
DLLEXPORT t_ascii *g_ascii = NULL; /* Array of ascii info */
DLLEXPORT int g_ascii_count;  /* Number of elems in g_ascii[] array */
DLLEXPORT int g_pixel_size; /* Num bytes per pixel (1, 2, 3 or 4) */
DLLEXPORT int g_icon_pixels; /* Num pixels per icon (16x16, 24x24, 32x32) */
DLLEXPORT int g_icon_style; /* ICON_STYLE_xxx */

unsigned char *g_palette_rgb;

/* Hack -- Standard 16 "term" colors. User should be able to change */
IconValue g_term_palette[16] = {255, 0, 250, 17, 217, 196, 199, 101, 129,
	247, 30, 5, 35, 185, 180, 52};

/* Actual 8/16/24 pixel values for above */
unsigned long g_term_colormap[16];

#define USE_COMPRESS
#define USE_ZLIB
/* #define USE_COMPRESS_LOG */
/* #define USE_STARTUP_LOG */

#ifdef USE_COMPRESS

#ifdef USE_ZLIB

#include "zlib.h"

#define BUFLEN 16384

#include "stubs.h"
static struct stub
{
	gzFile (*gzopen)(const char *path, const char *mode);
	int (*gzclose)(gzFile file);
	const char *(*gzerror)(gzFile file, int *errnum);
	int (*gzwrite)(gzFile file, const voidp buf, unsigned len);
	int (*gzread)(gzFile file, voidp buf, unsigned len);
	const char *(*zlibVersion)(void);
} stub;
static t_stub stub_init[] =
{
	{STUB_DESC(gzopen, struct stub)},
	{STUB_DESC(gzclose, struct stub)},
	{STUB_DESC(gzerror, struct stub)},
	{STUB_DESC(gzwrite, struct stub)},
	{STUB_DESC(gzread, struct stub)},
	{STUB_DESC(zlibVersion, struct stub)},
	{NULL, 0, 0}
};

static ShLibRef shLib = NULL;

#ifdef USE_COMPRESS_LOG

static FILE *compress_fp = NULL;

static void compress_log_open(void)
{
	char path[1024];

	path_build(path, 1024, ANGBAND_DIR_ROOT, "compress.log");
	compress_fp = fopen(path, "w");
}

static void compress_log_close(void)
{
	if (compress_fp)
		fclose(compress_fp);
}

static void compress_log(char *s, gzFile gzFileRef)
{
	if (!compress_fp) return;
	if (gzFileRef)
	{
		int err;
		(void) dll_gzerror(gzFileRef, &err);
		fprintf(compress_fp, "(error %d): ", err); 
	}
	fprintf(compress_fp, "%s\n", s);
}

#endif /* USE_COMPRESS_LOG */

int init_compress(Tcl_Interp *interp)
{
	char errorMsg[512];
	char *shLibName = "lib/" ZLIB_LIB_SH_NAME;

	shLib = ShLib_Load(shLibName);
	if (shLib == NULL)
	{
		(void) sprintf(errorMsg, "couldn't load \"%s\"", shLibName);
		goto error;
	}
	if (Stub_Load(shLib, stub_init, &stub) == -1)
	{
		(void) sprintf(errorMsg, "%s: %s", shLibName, Stub_Error());
		goto error;
	}
	if (strcmp((*stub.zlibVersion)(), ZLIB_VERSION))
	{
		(void) sprintf(errorMsg, "expected zlib %s, got %s\n", ZLIB_VERSION,
			(*stub.zlibVersion)());
		goto error;
	}

#ifdef USE_COMPRESS_LOG
	compress_log_open();
#endif

	/* Success */
	return TCL_OK;

error:

	/* Set the error */
	StringResult(interp, errorMsg);

	/* Failure */
	return TCL_ERROR;
}

void free_compress(void)
{
	if (shLib != NULL)
		ShLib_Free(shLib);

#ifdef USE_COMPRESS_LOG
	compress_log_close();
#endif
}

int CompressIconFile(Tcl_Interp *interp, char *fileName, t_icon_type *idp)
{
	gzFile out;
	long sum = 0, dataSize;
	int err, result = TCL_OK;

dbwin("CompressIconFile\n");

	out = (*stub.gzopen)(fileName, "wb6");
	if (out == NULL)
	{
#ifdef USE_COMPRESS_LOG
		compress_log(format("error opening \"%s\"", fileName), NULL);
#endif
		Tcl_AppendResult(interp, "couldn't open \"", fileName, "\"", NULL);
		return TCL_ERROR;
	}
#ifdef USE_COMPRESS_LOG
		compress_log(format("opened \"%s\" for WRITING", fileName), NULL);
#endif

	if ((*stub.gzwrite)(out, "tnbz", 4) != 4)
	{
#ifdef USE_COMPRESS_LOG
		compress_log("error writing header", out);
#endif
		Tcl_AppendResult(interp, "error writing header", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
#ifdef USE_COMPRESS_LOG
		compress_log("wrote header", NULL);
#endif

	if ((*stub.gzwrite)(out, &idp->depth, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing depth", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
	if ((*stub.gzwrite)(out, &idp->bypp, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing pixel-size", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
	if ((*stub.gzwrite)(out, &g_rgbi.red_shift, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing red_shift", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
	if ((*stub.gzwrite)(out, &g_rgbi.green_shift, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing green_shift", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
	if ((*stub.gzwrite)(out, &g_rgbi.blue_shift, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing blue_shift", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
	if ((*stub.gzwrite)(out, &idp->width, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing width", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
	if ((*stub.gzwrite)(out, &idp->height, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing height", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
	if ((*stub.gzwrite)(out, &idp->icon_count, sizeof(int)) != sizeof(int))
	{
		Tcl_AppendResult(interp, "error writing count", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}

	dataSize = idp->icon_count * idp->length;
	if ((*stub.gzwrite)(out, &dataSize, sizeof(long)) != sizeof(long))
	{
#ifdef USE_COMPRESS_LOG
		compress_log("error writing file size", out);
#endif
		Tcl_AppendResult(interp, "error writing file size", NULL);
		result = TCL_ERROR;
		goto cleanup;
	}
#ifdef USE_COMPRESS_LOG
	compress_log("wrote file size", NULL);
#endif

	while (1)
	{
		int len = idp->length * 16L;
		if (len > (dataSize - sum))
			len = dataSize - sum;
		if ((*stub.gzwrite)(out, idp->icon_data + sum, len) != len)
		{
#ifdef USE_COMPRESS_LOG
			compress_log(format("gzwrite() failed after %ld/%ld bytes", sum, dataSize), out);
#endif
			(void) (*stub.gzerror)(out, &err);
			Tcl_AppendResult(interp,
				format("gzwrite() failed: err %d", err), NULL);
			result = TCL_ERROR;
			break;
		}
#ifdef USE_COMPRESS_LOG
		compress_log(format("wrote %d bytes (%ld/%ld total)", len, sum, dataSize), NULL);
#endif
		sum += len;
		if (sum >= dataSize) break;
	}
#ifdef USE_COMPRESS_LOG
	compress_log(format("finished writing %ld bytes total", sum), NULL);
#endif

cleanup:
	if ((*stub.gzclose)(out) != Z_OK)
	{
#ifdef USE_COMPRESS_LOG
		compress_log(format("gzclose() failed"), out);
#endif
		(void) (*stub.gzerror)(out, &err);
		Tcl_AppendResult(interp, format("gzclose() failed: err %d", err),
			NULL);
		result = TCL_ERROR;
	}
#ifdef USE_COMPRESS_LOG
	else
		compress_log(format("closed \"%s\"\n", fileName), NULL);
#endif

	return result;
}

int DecompressIconFile(Tcl_Interp *interp, char *fileName, t_icon_type *iconTypePtr)
{
	gzFile in;
	char *buf = NULL;
	char header[4];
	int len;
	int err;
	long sum = 0, size;
	int depth, bypp, shift, width, height, count;
	int length;

	in = (*stub.gzopen)(fileName, "rb");
	if (in == NULL)
	{
#ifdef USE_COMPRESS_LOG
		compress_log(format("error opening \"%s\"", fileName), NULL);
#endif
		Tcl_AppendResult(interp, "couldn't open \"", fileName, "\"",
			NULL);
		return TCL_ERROR;
	}
#ifdef USE_COMPRESS_LOG
	compress_log(format("opened \"%s\" for READING", fileName), NULL);
#endif

	len = (*stub.gzread)(in, header, 4);
	if ((len != 4) || strncmp(header, "tnbz", 4))
	{
#ifdef USE_COMPRESS_LOG
		compress_log("error reading header", in);
#endif
		Tcl_AppendResult(interp, "error reading header", NULL);
		goto error;
	}
#ifdef USE_COMPRESS_LOG
	compress_log(format("read header \"%.4s\"", header), NULL);
#endif

	len = (*stub.gzread)(in, &depth, sizeof(int));
	if ((len != sizeof(int)) || (depth != g_icon_depth))
	{
		Tcl_AppendResult(interp, "invalid depth", NULL);
		goto error;
	}

	len = (*stub.gzread)(in, &bypp, sizeof(int));
	if ((len != sizeof(int)) || (bypp != g_pixel_size))
	{
		Tcl_AppendResult(interp, "invalid pixel-size", NULL);
		goto error;
	}

	len = (*stub.gzread)(in, &shift, sizeof(int));
	if ((len != sizeof(int)) || (shift != g_rgbi.red_shift))
	{
		Tcl_AppendResult(interp, "red_shift mismatch", NULL);
		goto error;
	}

	len = (*stub.gzread)(in, &shift, sizeof(int));
	if ((len != sizeof(int)) || (shift != g_rgbi.green_shift))
	{
		Tcl_AppendResult(interp, "green_shift mismatch", NULL);
		goto error;
	}

	len = (*stub.gzread)(in, &shift, sizeof(int));
	if ((len != sizeof(int)) || (shift != g_rgbi.blue_shift))
	{
		Tcl_AppendResult(interp, "blue_shift mismatch", NULL);
		goto error;
	}

	len = (*stub.gzread)(in, &width, sizeof(int));
	if ((len != sizeof(int)) || (width <= 0))
	{
		Tcl_AppendResult(interp, "invalid width", NULL);
		goto error;
	}

	len = (*stub.gzread)(in, &height, sizeof(int));
	if ((len != sizeof(int)) || (height <= 0))
	{
		Tcl_AppendResult(interp, "invalid height", NULL);
		goto error;
	}

	length = width * height * bypp;

	len = (*stub.gzread)(in, &count, sizeof(int));
	if ((len != sizeof(int)) || (count < 0))
	{
		Tcl_AppendResult(interp, "invalid count", NULL);
		goto error;
	}

	len = (*stub.gzread)(in, &size, sizeof(long));
	if ((len != sizeof(long)) || (size < 0) || (size > 10L * 1024L * 1024L)
		|| ((size / length) != count))
	{
#ifdef USE_COMPRESS_LOG
		compress_log(format("invalid data size %ld", size), in);
#endif
		Tcl_AppendResult(interp, "invalid data size", NULL);
		goto error;
	}
#ifdef USE_COMPRESS_LOG
	compress_log(format("read file size %ld bytes", size), NULL);
#endif

	buf = Tcl_AllocDebug(size);

	while (sum < size)
	{
		len = (*stub.gzread)(in, buf + sum, length * 16L);
		(void) (*stub.gzerror)(in, &err);
		if (sum + len == size) err = 0;
		if ((len < 0) || (err < 0))
		{
#ifdef USE_COMPRESS_LOG
			compress_log(format("gzread() failed after %ld/%ld bytes", sum, size), in);
#endif
			(void) (*stub.gzerror)(in, &err);
			Tcl_AppendResult(interp,
				format("gzread() failed: err %d", err), NULL);
			goto error;
		}
#ifdef USE_COMPRESS_LOG
		compress_log(format("read %d bytes (%ld/%ld total)", len, sum, size), NULL);
#endif
		if (len == 0)
			break;
		sum += len;
	}
#ifdef USE_COMPRESS_LOG
	compress_log(format("finished reading %ld bytes total", sum), NULL);
#endif

	if (sum != size)
	{
		Tcl_AppendResult(interp, "file is truncated", NULL);
		goto error;
	}

	/* gzclose() fails sometimes, even though the whole file was read */
	if (((*stub.gzclose)(in) != Z_OK) && 0)
	{
#ifdef USE_COMPRESS_LOG
		compress_log(format("gzclose() failed"), in);
#endif
		Tcl_FreeDebug(buf);
		(void) (*stub.gzerror)(in, &err);
		Tcl_AppendResult(interp, format("gzclose() failed: err %d", err),
			NULL);

		/* Failure */
		return TCL_ERROR;
	}
#ifdef USE_COMPRESS_LOG
	compress_log(format("closed \"%s\"\n", fileName), NULL);
#endif

	iconTypePtr->icon_data = (IconPtr) buf;
	iconTypePtr->icon_count = count;
	iconTypePtr->depth = depth;
	iconTypePtr->bypp = bypp;
	iconTypePtr->width = width;
	iconTypePtr->height = height;
	iconTypePtr->pitch = width * bypp;
	iconTypePtr->length = length;
	iconTypePtr->pixels = width * height;

	/* Success */
	return TCL_OK;

error:
	if (buf)
		Tcl_FreeDebug(buf);
	if ((*stub.gzclose)(in) != Z_OK)
	{
	}

	/* Failure */
	return TCL_ERROR;
}

#endif /* USE_ZLIB */

#endif /* USE_COMPRESS */

/* RL_Encode(), but discard results (return len only) */
int RL_Len(int w, int h, int bypp, IconPtr srcbuf, int pitch, int key)
{
	int y;
	int total = 0, lastline = 0;

	for (y = 0; y < h; y++)
	{
		int x = 0;
		int blankline = 0;

		do
		{
			int opaq, trans;
			int opaqstart;
			int transstart = x;

			while ((x < w) && (PixelPtrToLong(srcbuf + x * bypp, bypp) == key))
				x++;
			opaqstart = x;
			while ((x < w) && (PixelPtrToLong(srcbuf + x * bypp, bypp) != key))
				x++;
			trans = opaqstart - transstart;
			if (trans == w)
				blankline = 1;
			opaq = x - opaqstart;

			total += 2;

			if (opaq)
			{
				total += opaq * bypp;
			}

			if (!blankline)
				lastline = total;

		} while (x < w);

		srcbuf += pitch;
	}

	total = lastline + 2;

	return total;
}

int RL_Encode(int w, int h, int bypp, IconPtr srcbuf, int pitch, int key, IconPtr rlePtr)
{
	int y;
	IconPtr dst, lastline;

	dst = rlePtr;
	lastline = dst;

	for (y = 0; y < h; y++)
	{
		int x = 0;
		int blankline = 0;

		do
		{
			int opaq, trans;
			int opaqstart;
			int transstart = x;

			while ((x < w) && (PixelPtrToLong(srcbuf + x * bypp, bypp) == key))
				x++;
			opaqstart = x;
			while ((x < w) && (PixelPtrToLong(srcbuf + x * bypp, bypp) != key))
				x++;
			trans = opaqstart - transstart;
			if (trans == w)
				blankline = 1;
			opaq = x - opaqstart;

			dst[0] = trans;
			dst[1] = opaq;
			dst += 2;

			if (opaq)
			{
				memcpy(dst, srcbuf + opaqstart * bypp, opaq * bypp);
				dst += opaq * bypp;
			}

			if (!blankline)
				lastline = dst;

		} while (x < w);

		srcbuf += pitch;
	}

	dst = lastline;
	dst[0] = 0;
	dst[1] = 0;
	dst += 2;

	return dst - rlePtr;
}

int RL_Decode(int w, int h, int bypp, IconPtr rlePtr, IconPtr dst, int pitch)
{
	int offset = 0;
	int total = 0;

	while (1)
	{
		unsigned int trans, opaq;

		trans = rlePtr[0];
		opaq = rlePtr[1];
		rlePtr += 2;

		offset += trans;

		if (opaq)
		{
			memcpy(dst + offset * bypp, rlePtr, opaq * bypp);
			rlePtr += opaq * bypp;
			offset += opaq;
		}
		else if (!offset)
			break;

		total += trans + opaq;

		if (offset == w)
		{
			offset = 0;
			dst += pitch;
			if (!--h)
				break;
		}
	}
	return total;
}

void RL_Bounds(int w, int h, int bypp, IconPtr srcbuf, int key, unsigned char *bounds)
{
	int left, top, right, bottom;
	int x, y;

	left = 255;
	right = 0;
	top = 255;
	bottom = 0;

	for (y = 0; y < h; y++)
	{
		for (x = 0; x < w; x++)
		{
			if (PixelPtrToLong(srcbuf + x * bypp, bypp) != key)
			{
				if (x < left)
					left = x;
				if (x > right)
					right = x;
				if (y < top)
					top = y;
				if (y > bottom)
					bottom = y;
			}
		}
		srcbuf += w * bypp;
	}

	if (!right)
		left = 0;
	if (!bottom)
		top = 0;

	bounds[0] = left;
	bounds[1] = top;
	bounds[2] = right - left + 1;
	bounds[3] = bottom - top + 1;
}

void Icon_MakeRLEBounds(t_icon_type *iconTypePtr, int i)
{
	int left, top, right, bottom;
	int bypp = iconTypePtr->bypp;
	int x, y;
	IconPtr srcbuf = iconTypePtr->icon_data + i * iconTypePtr->length;
	unsigned char *bounds;

	left = 255;
	right = 0;
	top = 255;
	bottom = 0;

	for (y = 0; y < iconTypePtr->height; y++)
	{
		for (x = 0; x < iconTypePtr->width; x++)
		{
			if (PixelPtrToLong(srcbuf + x * bypp, bypp) != iconTypePtr->rle_pixel)
			{
				if (x < left)
					left = x;
				if (x > right)
					right = x;
				if (y < top)
					top = y;
				if (y > bottom)
					bottom = y;
			}
		}
		srcbuf += iconTypePtr->pitch;
	}

	if (!right)
		left = 0;
	if (!bottom)
		top = 0;

	bounds = iconTypePtr->rle_bounds + i * 4;
	bounds[0] = left;
	bounds[1] = top;
	bounds[2] = right - left + 1;
	bounds[3] = bottom - top + 1;
}

void Icon_MakeRLE(t_icon_type *iconTypePtr)
{
	int i, len, slop;
	long total;

	total = 0;

	/* x, y, width, height for each icon */
	iconTypePtr->rle_bounds =
		(unsigned char *) Tcl_AllocDebug(sizeof(unsigned char) *
		iconTypePtr->icon_count * 4);

	for (i = 0; i < iconTypePtr->icon_count; i++)
	{
		IconPtr iconPtr = iconTypePtr->icon_data + i * iconTypePtr->length;
		unsigned char *bounds = iconTypePtr->rle_bounds + i * 4;

		Icon_MakeRLEBounds(iconTypePtr, i);

		iconPtr += bounds[0] * iconTypePtr->bypp +
			bounds[1] * iconTypePtr->pitch;

		/* Get length of encoded data */
		len = RL_Len(bounds[2], bounds[3], iconTypePtr->bypp,
			iconPtr, iconTypePtr->pitch, iconTypePtr->rle_pixel);

		total += len;
	}

	/* RL_Encode() can write past 'len' by a number of transparent lines */
	if (iconTypePtr->bypp == 4)
		slop = iconTypePtr->height * 4;
	else
		slop = iconTypePtr->height * 2;

	iconTypePtr->rle_data = (IconPtr) Tcl_AllocDebug(total + slop);
	iconTypePtr->rle_offset =
		(long *) Tcl_AllocDebug(sizeof(long) * iconTypePtr->icon_count);
	iconTypePtr->rle_len =
		(int *) Tcl_AllocDebug(sizeof(int) * iconTypePtr->icon_count);

	total = 0;

	for (i = 0; i < iconTypePtr->icon_count; i++)
	{
		IconPtr iconPtr = iconTypePtr->icon_data + i * iconTypePtr->length;
		unsigned char *bounds = iconTypePtr->rle_bounds + i * 4;
if (!i) dbwin("first pixel is %x %x %x %x\n", iconPtr[0], iconPtr[1], iconPtr[2], iconPtr[3]);
if (!i) dbwin("  rle_pixel is %08lx\n", iconTypePtr->rle_pixel);
		iconPtr += bounds[0] * iconTypePtr->bypp +
			bounds[1] * iconTypePtr->pitch;

		/* Encode */
		len = RL_Encode(bounds[2], bounds[3], iconTypePtr->bypp,
			iconPtr, iconTypePtr->pitch, iconTypePtr->rle_pixel,
			iconTypePtr->rle_data + total);

		iconTypePtr->rle_offset[i] = total;
		iconTypePtr->rle_len[i] = len;

		total += len;
	}

	Tcl_FreeDebug(iconTypePtr->icon_data);
	iconTypePtr->icon_data = NULL;
}

static int InitPixelSize(Tcl_Interp *interp)
{
	BitmapType bitmap;

	bitmap.width = bitmap.height = 10;
	bitmap.depth = g_icon_depth;
	Bitmap_New(interp, &bitmap);
	g_pixel_size = bitmap.pixelSize;
	Bitmap_Delete(&bitmap);

dbwin("Monitor depth is %d, pixel-size is %d\n", g_icon_depth, g_pixel_size);

	return TCL_OK;
}

DLLEXPORT RGBInfo g_rgbi;

static int CountBits(unsigned long mask)
{
	int n;

	for (n = 0; mask != 0; mask &= mask - 1)
		n++;
	return n;
}

void InitRGBInfo(Tcl_Interp *interp)
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

dbwin("InitRGBInfo: Rshift %d Gshift %d BShift %d", g_rgbi.red_shift, g_rgbi.green_shift, g_rgbi.blue_shift);
}

void GetPix8(unsigned char *p, int *r, int *g, int *b)
{
	unsigned char *rgbPtr = &g_palette_rgb[*p * 3]; /* FIXME: colormap */
	*r = *rgbPtr++;
	*g = *rgbPtr++;
	*b = *rgbPtr++;
}

void SetPix8(unsigned char *p, int r, int g, int b)
{
	/* NOTE: Not Colormap */
	*p = Palette_RGB2Index(r, g, b);
}

void GetPix16(unsigned char *p, int *r, int *g, int *b)
{
	unsigned short pix16 = *(unsigned short *) p;
	*r = (pix16 & g_rgbi.red_mask) >> g_rgbi.red_shift;
	*g = (pix16 & g_rgbi.green_mask) >> g_rgbi.green_shift;
	*b = (pix16 & g_rgbi.blue_mask) << g_rgbi.blue_shift;
}

void SetPix16(unsigned char *p, int r, int g, int b)
{
	int r2 = (r << g_rgbi.red_shift) & g_rgbi.red_mask;
	int g2 = (g << g_rgbi.green_shift) & g_rgbi.green_mask;
	int b2 = (b >> g_rgbi.blue_shift) & g_rgbi.blue_mask;

	*((unsigned short *) p) = g_rgbi.extra | r2 | g2 | b2;
}

void GetPix24(unsigned char *p, int *r, int *g, int *b)
{
	*b = *p++;
	*g = *p++;
	*r = *p++;
}

#if 1
void SetPix24(unsigned char *p, int r, int g, int b)
{
	*p++ = b;
	*p++ = g;
	*p++ = r;
	if (g_pixel_size == 4)
		*p++ = 0xFF;
}
#else
#define SetPix24(p,r,g,b) \
	(p)[0] = b; (p)[1] = g; (p)[2] = r;
#endif

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

void PixelPtrToRGB(IconPtr src, int *r, int *g, int *b, int bypp)
{
	switch (bypp)
	{
		case 1:
			GetPix8(src, r, g, b);
			break;
		case 2:
			GetPix16(src, r, g, b);
			break;
		case 3:
		case 4:
			GetPix24(src, r, g, b);
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

int RGBToPixelLong(int r, int g, int b, int bypp)
{
	unsigned char p[4];
	PixelSet_RGB(p, r, g, b, bypp);
	return PixelPtrToLong(p, bypp);
}

int Image2Bits(Tcl_Interp *interp, t_icon_type *iconTypePtr,
	Tk_PhotoHandle photoH, int imageW, int imageH, XColor *xColorPtr)
{
	Tk_PhotoImageBlock photoBlock;
	int row, col, y, x;
	int i, r, g, b;
	int r2 = 0, g2 = 0, b2 = 0;
	unsigned char *srcPtr, *dstPtr;
	int dataSize;
	int numCol, numRow;
	int iconW = iconTypePtr->width;
	int pixelSize = iconTypePtr->bypp;

	/* Get info about the image */
	(void) Tk_PhotoGetImage(photoH, &photoBlock);

	/* Calculate the number of icons based on image dimensions */
	numCol = (photoBlock.width / imageW);
	numRow = (photoBlock.height / imageH);
	iconTypePtr->icon_count = numCol * numRow;

	/* Allocate icon buffer */
	dataSize = iconTypePtr->icon_count * iconTypePtr->length;
	iconTypePtr->icon_data = (unsigned char *) Tcl_AllocDebug(dataSize);

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

	dstPtr = iconTypePtr->icon_data;
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
		int n = iconTypePtr->icon_count - 1;

		if (pixelSize == 1)
			whiteValue = PALETTE_WHITE;
		while (emptyIcon)
		{
			srcPtr = iconTypePtr->icon_data + n * iconTypePtr->length;
			for (i = 0; i < iconTypePtr->length; i++)
			{
				if (*srcPtr++ != whiteValue)
				{
					emptyIcon = FALSE;
					break;
				}
			}
			if (emptyIcon)
			{
				iconTypePtr->icon_count -= 1;
				n--;
			}
		}
	}

	return TCL_OK;
}

int WriteIconFile(Tcl_Interp *interp, char *utfFileName, t_icon_type *idp)
{
#ifdef USE_COMPRESS

	Tcl_DString extDString;
	char *extFileName;
	int result;

	extFileName = UtfToExt_TranslateFileName(interp, utfFileName, &extDString);
	if (extFileName == NULL)
	{
		return TCL_ERROR;
	}

	result = CompressIconFile(interp, extFileName, idp);

	Tcl_DStringFree(&extDString);

	return result;

#else /* not USE_COMPRESS */

	Tcl_DString temp;
	Tcl_Channel chan;
	char *fileBuf = NULL;
	int dataSize;

	/* Translate the file name */
	fileName = Tcl_TranslateFileName(interp, fileName, &temp);
	if (fileName == NULL)
	{
		/* Note: Tcl_DStringFree() is called for us */
		goto error;
	}

	/* Open the file for writing */
	chan = Tcl_OpenFileChannel(interp, fileName, "w", 0644);
	if (chan == (Tcl_Channel) NULL)
	{
		Tcl_ResetResult(interp);
		goto nowrite;
	}

	(void) Tcl_SetChannelOption(interp, chan, "-translation", "binary");

	dataSize = iconCount * iconLength;
	if (Tcl_Write(chan, iconData, dataSize) < 0)
	{
		Tcl_Close(interp, chan);
		goto nowrite;
	}

	if (Tcl_Close(interp, chan) != TCL_OK)
	{
		goto error;
	}

	Tcl_DStringFree(&temp);

	return TCL_OK;

nowrite:
	Tcl_AppendResult(interp, "couldn't write file \"", fileName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);

error:
	if (fileBuf != NULL)
	{
		Tcl_Free(fileBuf);
	}
	Tcl_DStringFree(&temp);
	return TCL_ERROR;

#endif /* not USE_COMPRESS */
}

/*
 * Read an icon data file. Returns the icon data and number of icons
 * read in. The icon data files are created with an external program
 * "makeicon".
 */
int ReadIconFile(Tcl_Interp *interp, char *fileName, t_icon_type *iconTypePtr)
{
#ifdef USE_COMPRESS

	Tcl_DString extDString;
	int result;

	fileName = UtfToExt_TranslateFileName(interp, fileName, &extDString);
	if (fileName == NULL)
	{
		return TCL_ERROR;
	}

	result = DecompressIconFile(interp, fileName, iconTypePtr);

	Tcl_DStringFree(&extDString);

	return result;

#else /* not USE_COMPRESS */

	Tcl_DString temp;
	Tcl_Channel chan;
	struct stat statBuf;
	char *fileBuf = NULL;

	/* Translate the file name */
	fileName = Tcl_TranslateFileName(interp, fileName, &temp);
	if (fileName == NULL)
	{
		/* Note: Tcl_DStringFree() is called for us */
		goto error;
	}

	/* stat() the file to get the length */
	if (stat(fileName, &statBuf) == -1)
	{
		Tcl_SetErrno(errno);
		goto noread;
	}

	if ((statBuf.st_size % ICON_LENGTH) != 0)
	{
		Tcl_AppendResult(interp, "bad size on icon file \"", fileName,
			"\"", (char *) NULL);
		goto error;
	}

	/* Open the file for reading */
	chan = Tcl_OpenFileChannel(interp, fileName, "r", 0644);
	if (chan == (Tcl_Channel) NULL)
	{
		Tcl_ResetResult(interp);
		goto noread;
	}

	(void) Tcl_SetChannelOption(interp, chan, "-translation", "binary");

	fileBuf = Tcl_Alloc(statBuf.st_size);
	if (Tcl_Read(chan, fileBuf, statBuf.st_size) < 0)
	{
		Tcl_Close(interp, chan);
		goto noread;
	}
	if (Tcl_Close(interp, chan) != TCL_OK)
	{
		goto error;
	}
	Tcl_DStringFree(&temp);

	(*iconData) = (IconPtr) fileBuf;
	(*iconCount) = statBuf.st_size / ICON_LENGTH;

	return TCL_OK;

noread:
	Tcl_AppendResult(interp, "couldn't read file \"", fileName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);

error:
	if (fileBuf != NULL)
	{
		Tcl_Free(fileBuf);
	}
	Tcl_DStringFree(&temp);
	return TCL_ERROR;

#endif /* not USE_COMPRESS */
}

/*
 * Get colorized data for TYPE_ASCII icon.
 */
IconPtr Icon_GetAsciiData(IconSpec *specPtr, IconPtr iconPtr)
{
	int i;
	int j = specPtr->index;
	int k = specPtr->ascii;
	t_icon_type *iconTypePtr = &g_icon_type[specPtr->type];
	PixelPtr srcPtr, dstPtr;
	int *color = g_ascii[k].color;
	int colors[2];

	srcPtr.pix8 = iconTypePtr->icon_data + j * iconTypePtr->length;
	dstPtr.pix8 = iconPtr;

	/* Multi-hued */
	if (g_ascii[k].mode == ASCII_ATTR_MULTI)
	{
		colors[0] = g_term_colormap[g_ascii_multi];
		colors[1] = color[1];
		color = colors;
	}

	/* Shapechanger */
	else if (g_ascii[k].mode == ASCII_SHAPECHANGER)
	{
		i = g_ascii_char;

		/* Paranoia */
		if (i >= iconTypePtr->icon_count)
		{
			i = iconTypePtr->icon_count - 1;
		}

		srcPtr.pix8 = iconTypePtr->icon_data + i * iconTypePtr->length;
	}

	if (iconTypePtr->depth == 8)
	{
		for (i = 0; i < iconTypePtr->pixels; i++)
		{
			*dstPtr.pix8++ = color[*srcPtr.pix8++];
		}
	}
	else if (iconTypePtr->depth == 16)
	{
		for (i = 0; i < iconTypePtr->pixels; i++)
		{
			*dstPtr.pix16++ = color[*srcPtr.pix16++];
		}
	}
	else if (iconTypePtr->depth == 24)
	{
		/* Skip to low-order byte */
		srcPtr.pix8 += 2;

		for (i = 0; i < iconTypePtr->pixels; i++)
		{
			unsigned char *zbgr = (unsigned char *) &color[*srcPtr.pix8];

#ifdef PLATFORM_WIN
			dstPtr.pix8[0] = zbgr[2]; /* b */
			dstPtr.pix8[1] = zbgr[1]; /* g */
			dstPtr.pix8[2] = zbgr[0]; /* r */
#endif /* */
#ifdef PLATFORM_X11
			dstPtr.pix8[0] = zbgr[0]; /* r */
			dstPtr.pix8[1] = zbgr[1]; /* g */
			dstPtr.pix8[2] = zbgr[2]; /* b */
#endif /* */

			/* Skip to low-order byte */
			srcPtr.pix8 += g_pixel_size;

			dstPtr.pix8 += g_pixel_size;
		}
	}

	return iconPtr;
}

static int g_ascii_y = 0, g_ascii_x = 0;

/*
 * Writes into the icon_data field of the given icon data struct,
 * which is assumed to be an ascii icon type. This is called when
 * creating a new ascii icon type and whenever the font for this
 * icon type is changed (via the "ascii font" command).
 */
static int init_ascii_data(Tcl_Interp *interp, t_icon_type *iconTypePtr)
{
	BitmapType bitmap;
	int i;
	int y, x;
	char buf[2];
	PixelPtr srcPtr, dstPtr;
	Tk_FontMetrics fm;
	Tk_Window tkwin = Tk_MainWindow(interp);
	Display *display = Tk_Display(tkwin);
	int width, y2, x2;
	GC gc;
	XGCValues gcValues;
	Pixmap pixmap;

	/* Create a temporary bitmap as large as one icon */
	bitmap.width = iconTypePtr->width;
	bitmap.height = iconTypePtr->height;
	bitmap.depth = iconTypePtr->depth;
	Bitmap_New(interp, &bitmap);

	pixmap = bitmap.pixmap;

	/* Graphics context for text */
	gcValues.foreground = 0x333333;
	gcValues.graphics_exposures = False;
	gcValues.font = Tk_FontId(iconTypePtr->font);
	gc = Tk_GetGC(tkwin,
		GCForeground | GCFont | GCGraphicsExposures,
		&gcValues);

	/* Get info about the font */
	Tk_GetFontMetrics(iconTypePtr->font, &fm);

	dstPtr.pix8 = iconTypePtr->icon_data;

	/* Check each icon */
	for (i = 0; i < iconTypePtr->icon_count; i++)
	{
		int ch = 32 + i; /* printable only */

		/* If a character set was specified, get the character */
		if (iconTypePtr->char_table)
		{
			ch = iconTypePtr->char_table[i];
		}

		/* Erase background to ones */
		for (y = 0; y < bitmap.height; y++)
		{
			PixelPtr pixelPtr;
			pixelPtr.pix8 = bitmap.pixelPtr + y * bitmap.pitch;
			for (x = 0; x < bitmap.width; x++)
			{
				switch (bitmap.depth)
				{
					case 8:
						*pixelPtr.pix8 = 1;
						break;
					case 16:
						*pixelPtr.pix16 = 1;
						break;
					case 24:
						pixelPtr.pix8[0] = 1;
						pixelPtr.pix8[1] = 1;
						pixelPtr.pix8[2] = 1;
						break;
				}
				pixelPtr.pix8 += bitmap.pixelSize;
			}
		}

		/* Only allow printable characters */
		if (isprint(ch))
		{
			/* Get a 2-byte string */
			(void) sprintf(buf, "%c", ch);

			/* Calculate the width of the character */
			width = Tk_TextWidth(iconTypePtr->font, buf, 1);

			/* Calculate the position of the character in the bitmap */
			x2 = g_ascii_x + (g_icon_width - width) / 2;
			y2 = g_ascii_y + (g_icon_height - fm.linespace) / 2 + fm.ascent;

			/* Draw the character into the bitmap */
			Tk_DrawChars(display, pixmap, gc,
				iconTypePtr->font, buf, 1, x2, y2);

			/* Flush before we copy */
			Plat_SyncDisplay(display);

			/*
			 * Convert text pixel values to zero, and copy from the bitmap
			 * to the icon data
			 */
			for (y = 0; y < bitmap.height; y++)
			{
				/* Point to the row */
				srcPtr.pix8 = bitmap.pixelPtr + y * bitmap.pitch;

				for (x = 0; x < bitmap.width; x++)
				{
					switch (bitmap.depth)
					{
						case 8:
						{
							if (srcPtr.pix8[0] != 1)
								srcPtr.pix8[0] = 0;
							dstPtr.pix8[0] = srcPtr.pix8[0];
							break;
						}
						case 16:
						{
							if (srcPtr.pix16[0] != 1)
								srcPtr.pix16[0] = 0;
							dstPtr.pix16[0] = srcPtr.pix16[0];
							break;
						}
						case 24:
						{
							if (srcPtr.pix8[0] != 1)
								srcPtr.pix8[0] = 0;
							if (srcPtr.pix8[1] != 1)
								srcPtr.pix8[1] = 0;
							if (srcPtr.pix8[2] != 1)
								srcPtr.pix8[2] = 0;
							dstPtr.pix8[0] = srcPtr.pix8[0];
							dstPtr.pix8[1] = srcPtr.pix8[1];
							dstPtr.pix8[2] = srcPtr.pix8[2];
							break;
						}
					}
					srcPtr.pix8 += bitmap.pixelSize;
					dstPtr.pix8 += iconTypePtr->bypp;
				}
			}
		}

		/* Not printable */
		else
		{
			/* Just skip the icon (set to TYPE_DEFAULT?) */
			dstPtr.pix8 += iconTypePtr->length;
		}
	}

	/* Done with GC */
	Tk_FreeGC(display, gc);

	/* Delete bitmap */
	Bitmap_Delete(&bitmap);

	/* Success */
	return TCL_OK;
}

/*
 * objcmd_ascii --
 *
 * Configure the "ascii" icon type.
 * Usage:
 *		ascii count
 *		ascii create forground background
 *		ascii font typeName ?fontSpec?
 *		ascii index typeName char attr -- Return index for character & attr
 *		ascii delay ?ticks?
 *		ascii inset ?y x?
 */

/* The current index into the g_term_colormap[] array */
DLLEXPORT int g_ascii_multi = 0;

/* The "frame delay" for animated ascii configurations */
DLLEXPORT int g_ascii_ticks = 0;
DLLEXPORT int g_ascii_delay = 1000;

/* The current character for shapechanging ascii configurations */
DLLEXPORT int g_ascii_char = 0;

int
objcmd_ascii(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *cmdOption[] = {
		"count", "create", "font", "index", "delay", "configure", "isascii",
		"convert", NULL
	};
	int index;

	static CONST char *configSwitch[] = {"-background", "-foreground", "-mode",
		"-alternate_friend", NULL};
	static CONST char *asciiMode[] = {"normal", "attr_multi", "shapechanger",
		NULL};

	char d_char, *t;
	Tcl_Obj *CONST *objPtr;
	Tk_Font tkFont;
	t_icon_type *iconTypePtr;
	char *typeName;
	int fg, bg, mode;
	int altFriend;
	t_ascii *ascii_ptr;
	int i;

	/* Required number of arguments */
	if (objc < 2)
	{
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	/* Get requested option */
	if (Tcl_GetIndexFromObj(interp, objv[1], cmdOption, "option", 0, 
		&index) != TCL_OK)
	{
		return TCL_ERROR;
	}

	switch (index)
	{
		case 0: /* count */
			IntResult(interp, g_ascii_count);
			break;

		case 1: /* create */
		{
			XColor *fgPtr, *bgPtr;
			Tk_Window tkwin = Tk_MainWindow(interp);

			/* Default */
			fg = COLORMAP_WHITE; /* Only valid on 8-bit! */
			bg = COLORMAP_BLACK; /* Only valid on 8-bit! */
			mode = ASCII_NORMAL;
			altFriend = -1;

			/* Point to the first option/value pair */
			objPtr = objv + 2;
			objc -= 2;

			/* Scan all option/value pairs */
			while (objc > 1)
			{
				if (Tcl_GetIndexFromObj(interp, objPtr[0], configSwitch,
					"switch", 0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}
				switch (index)
				{
					case 0: /* -background */
						if ((bgPtr = Tk_AllocColorFromObj(interp,
							tkwin, objPtr[1])) == NULL)
						{
							return TCL_ERROR;
						}
						bg = Plat_XColor2Pixel(bgPtr);
						Tk_FreeColor(bgPtr); /* XXX */
						break;

					case 1: /* -foreground */
						if ((fgPtr = Tk_AllocColorFromObj(interp,
							tkwin, objPtr[1])) == NULL)
						{
							return TCL_ERROR;
						}
						fg = Plat_XColor2Pixel(fgPtr);
						Tk_FreeColor(fgPtr); /* XXX */
						break;

					case 2: /* -mode */
						if (Tcl_GetIndexFromObj(interp, objPtr[1], asciiMode,
							"mode", 0, &mode) != TCL_OK)
						{
							return TCL_ERROR;
						}
						break;

					case 3: /* -alternate_friend */
						if (Tcl_GetIntFromObj(interp, objPtr[1], &altFriend) != TCL_OK)
						{
							return TCL_ERROR;
						}
						if (altFriend < 0 || altFriend >= g_ascii_count)
						{
							/* Set the error */
							FormatResult(interp, "bad ascii \"%d\": must be from 0 to %d",
								altFriend, g_ascii_count - 1);
							return TCL_ERROR;
						}
						break;
				}

				/* Next option/value pair */
				objPtr += 2;
				objc -= 2;
			}

			/* Append a new ascii configuration to the global array */
			g_ascii = Array_Insert(g_ascii, &g_ascii_count,
				sizeof(t_ascii), g_ascii_count);

			/* Access the ascii configuration */
			ascii_ptr = &g_ascii[g_ascii_count - 1];

			/* Set the fields */
			ascii_ptr->color[0] = fg;
			ascii_ptr->color[1] = bg;
			ascii_ptr->mode = mode;
			ascii_ptr->altFriend = altFriend;

			/* Return the index of the new ascii configuration */
			IntResult(interp, g_ascii_count - 1);

			break;
		}

		case 2: /* font */

			if (objc < 3 || objc > 4)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type ?font?");
				return TCL_ERROR;
			}

			/* Get the icon type name */
			typeName = Tcl_GetStringFromObj(objv[2], NULL);

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* This icon type doesn't have a font */
			if (iconTypePtr->font == NULL)
			{
				/* Set the error */
				FormatResult(interp, "icon type \"%s\" not ascii", typeName);

				/* Failure */
				return TCL_ERROR;
			}

			if (objc == 4)
			{
				/* Get the name of the font */
				t = Tcl_GetStringFromObj(objv[3], NULL);

				/* Get the font */
				tkFont = Tk_GetFont(interp, Tk_MainWindow(interp), t);

				/* The font could not be created */
				if (tkFont == NULL)
				{
					return TCL_ERROR;
				}

				/* Free the old (Tk) font, if any */
				if (iconTypePtr->font) Tk_FreeFont(iconTypePtr->font);

				/* Remember current font */
				iconTypePtr->font = tkFont;

				/* Set the icon_data */
				if (init_ascii_data(interp, iconTypePtr) != TCL_OK)
				{
					return TCL_ERROR;
				}
			}

			StringResult(interp, Tk_NameOfFont(iconTypePtr->font));
			break;

		case 3: /* index */

			if (objc != 4)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type char");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the desired character */
			t = Tcl_GetStringFromObj(objv[3], NULL);
			d_char = t[0];

			/*
			 * The char_table for ascii icon types is used for
			 * specific ranges of characters.
			 */
			if (iconTypePtr->char_table)
			{
				/* Check each icon */
				for (i = 0; i < iconTypePtr->icon_count; i++)
				{
					/* This icon represents the desired character */
					if (iconTypePtr->char_table[i] == d_char)
					{
						/* Return the icon index */
						IntResult(interp, i);

						/* Stop */
						break;
					}
				}

				/* The character was not found */
				if (i == iconTypePtr->icon_count)
				{
					/* Set the error */
					FormatResult(interp, "bad character \"%c\": ", d_char);

					/* Failure */
					return TCL_ERROR;
				}
			}

			/* This icon type uses all printable ascii characters */
			else if (isprint(d_char))
			{
				IntResult(interp, d_char - 32); /* printable only */
			}

			/* Failure */
			else
			{
				IntResult(interp, 0);
			}
			break;

		case 4: /* delay */

			if (objc < 2)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "?delay?");
				return TCL_ERROR;
			}

			/* A new value was given */
			if (objc == 3)
			{
				/* Get the new value */
				if (Tcl_GetIntFromObj(interp, objv[2], &g_ascii_delay) != TCL_OK)
				{
					return TCL_ERROR;
				}
				/* VERIFY g_ascii_delay */
			}

			/* Return the current value */
			IntResult(interp, g_ascii_delay);
			break;

		case 5: /* configure */
		{
			XColor *fgPtr, *bgPtr;
			Tk_Window tkwin = Tk_MainWindow(interp);

			if (objc < 4)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "index option ?value?");
				return TCL_ERROR;
			}

			/* Get the ascii configuration index */
			if (Tcl_GetIntFromObj(interp, objv[2], &index) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify the ascii configuration index */
			if ((index < 0) || (index >= g_ascii_count))
			{
				/* Set the error */
				FormatResult(interp, "bad ascii \"%d\": must be from 0 to %d",
					index, g_ascii_count -1);

				/* Failure */
				return TCL_ERROR;
			}

			/* Access the ascii configuration */
			ascii_ptr = &g_ascii[index];

			/* "ascii configure $index $option" */
			/* Return the value of a single option */
			if (objc == 4)
			{
				if (Tcl_GetIndexFromObj(interp, objv[3], configSwitch,
					"switch", 0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}
				switch (index)
				{
					case 0: /* -background */
						IntResult(interp, ascii_ptr->color[1]);
						break;

					case 1: /* -foreground */
						IntResult(interp, ascii_ptr->color[0]);
						break;

					case 2: /* -mode */
						StaticResult(interp, asciiMode[ascii_ptr->mode]);
						break;

					case 3: /* -alternate_friend */
						IntResult(interp, ascii_ptr->altFriend);
						break;
				}

				/* Done */
				break;
			}

			/* Point to the first option/value pair */
			objPtr = objv + 3;
			objc -= 3;

			/* Scan all option/value pairs */
			while (objc > 1)
			{
				if (Tcl_GetIndexFromObj(interp, objPtr[0], configSwitch,
					"switch", 0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}
				switch (index)
				{
					case 0: /* -background */
						if ((bgPtr = Tk_AllocColorFromObj(interp,
							tkwin, objPtr[1])) == NULL)
						{
							return TCL_ERROR;
						}
						ascii_ptr->color[1] = Plat_XColor2Pixel(bgPtr);
						Tk_FreeColor(bgPtr); /* XXX */
						break;

					case 1: /* -foreground */
						if ((fgPtr = Tk_AllocColorFromObj(interp,
							tkwin, objPtr[1])) == NULL)
						{
							return TCL_ERROR;
						}
						ascii_ptr->color[0] = Plat_XColor2Pixel(fgPtr);
						Tk_FreeColor(fgPtr); /* XXX */
						break;
#if 0 /* Aug 12 2004 */
					case 0: /* -background */
						if (Tcl_GetIntFromObj(interp, objPtr[1], &bg) != TCL_OK)
						{
							return TCL_ERROR;
						}
						/* VERIFY bg */
						ascii_ptr->color[1] = bg;
						break;

					case 1: /* -foreground */
						if (Tcl_GetIntFromObj(interp, objPtr[1], &fg) != TCL_OK)
						{
							return TCL_ERROR;
						}
						/* VERIFY fg */
						ascii_ptr->color[0] = fg;
						break;
#endif /* 0 */
					case 2: /* -mode */
						if (Tcl_GetIndexFromObj(interp, objPtr[1], asciiMode,
							"mode", 0, &index) != TCL_OK)
						{
							return TCL_ERROR;
						}
						ascii_ptr->mode = index;
						break;

					case 3: /* -alternate_friend */
						if (Tcl_GetIntFromObj(interp, objPtr[1], &altFriend) != TCL_OK)
						{
							return TCL_ERROR;
						}
						if (altFriend < 0 || altFriend >= g_ascii_count)
						{
							/* Set the error */
							FormatResult(interp, "bad ascii \"%d\": must be from 0 to %d",
								altFriend, g_ascii_count - 1);
						}
						ascii_ptr->altFriend = altFriend;
						break;
				}

				/* Next option/value pair */
				objPtr += 2;
				objc -= 2;
			}
			/* VERIFY objc */
			break;
		}

		case 6: /* isascii */

			if (objc < 3)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* This icon type doesn't have a font */
			if (iconTypePtr->font == NULL)
			{
				BooleanResult(interp, 0);
				break;
			}
			BooleanResult(interp, 1);
			break;

		case 7: /* convert */
		{
			IconSpec iconSpec;

			if (objc != 4)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type attr");
				return TCL_ERROR;
			}

			/* Get the icon type name */
			typeName = Tcl_GetStringFromObj(objv[2], NULL);

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, &iconSpec.type,
				objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* This icon type doesn't have a font */
			if (iconTypePtr->font == NULL)
			{
				/* Set the error */
				FormatResult(interp, "icon type \"%s\" not ascii", typeName);

				/* Failure */
				return TCL_ERROR;
			}

			/* Get the ascii configuration index */
			if (Tcl_GetIntFromObj(interp, objv[3], &index) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify the ascii configuration index */
			if ((index < 0) || (index >= g_ascii_count))
			{
				/* Set the error */
				FormatResult(interp, "bad ascii \"%d\": must be from 0 to %d",
					index, g_ascii_count -1);

				/* Failure */
				return TCL_ERROR;
			}

			for (i = 0; i < iconTypePtr->icon_count; i++)
			{
				iconSpec.index = i;
				iconSpec.ascii = index;
				iconSpec.dark = 0;
				Icon_GetAsciiData(&iconSpec, iconTypePtr->icon_data +
					i * iconTypePtr->length);
			}

			Tk_FreeFont(iconTypePtr->font);
			iconTypePtr->font = NULL;
			break;
		}
	}

	/* Success */
	return TCL_OK;
}

/*
 * icon photo $imageName -type $type -index $index
 * Set an existing photo image with icon data. This is similar to the
 * Widget_Photo() routine and lets us set a label widget with a picture
 * of any icon.
 */
int
objcmd_icon_photo(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	Tk_PhotoHandle photoH;
	Tk_PhotoImageBlock photoBlock;
	char *imageName;
	int i, transparent = 0;
	int r, g, b;
	IconSpec iconSpec;
	t_icon_type *iconTypePtr;
	unsigned char *photoData;
	unsigned char *srcPtr = NULL, *dstPtr;
	unsigned char *dataPtr, *maskPtr = NULL;

	/* Required number of arguments */
	if (objc < 3)
	{
		Tcl_WrongNumArgs(interp, 2, objv,
			"imageName -type iconType -index iconIndex");
		return TCL_ERROR;
	}

	/* Get the name of the Tk photo image. */
	imageName = Tcl_GetStringFromObj(objv[2], NULL);

	/* Lookup the photo by name. It must already exist */
	photoH = Tk_FindPhoto(interp, imageName);

	/* The photo was not found */
	if (photoH == NULL)
	{
		/* Set the error */
		Tcl_AppendResult(interp, "image \"", imageName,
			"\" doesn't exist or is not a photo image",
			(char *) NULL);

		/* Failure */
		return TCL_ERROR;
	}

	/* Scan the arguments for icon type and index */
	if (Icon_ParseArgs(interp, objc, objv, 3, &iconSpec) != TCL_OK)
	{
		/* Failure */
		return TCL_ERROR;
	}

	iconTypePtr = &g_icon_type[iconSpec.type];
	dataPtr = (unsigned char *) Tcl_AllocDebug(iconTypePtr->length);

	/* Ascii-type icon */
	if (iconSpec.ascii != -1)
	{
		srcPtr = Icon_GetAsciiData(&iconSpec, dataPtr);
	}

	/* Dynamic transparent */
	else if (iconTypePtr->dynamic)
	{
		/* Should never be assigned */
		transparent = 1;
	}

	/* Transparent */
	else if (iconTypePtr->rle_data != NULL)
	{
		unsigned char *bounds = iconTypePtr->rle_bounds + iconSpec.index * 4;
		IconPtr rlePtr = iconTypePtr->rle_data +
			iconTypePtr->rle_offset[iconSpec.index];

		/* Init to transparent pixel */
		for (i = 0; i < iconTypePtr->pixels; i++)
		{
			PixelLongToPtr(dataPtr + i * iconTypePtr->bypp,
				iconTypePtr->rle_pixel, iconTypePtr->bypp);
		}

		/* Decode image */
		RL_Decode(bounds[2], bounds[3], iconTypePtr->bypp,
			rlePtr, dataPtr + bounds[0] * iconTypePtr->bypp + 
			bounds[1] * iconTypePtr->pitch, iconTypePtr->pitch);

		/* Create a mask, 1-byte-per-pixel */
		maskPtr = (unsigned char *) Tcl_AllocDebug(iconTypePtr->pixels);
		for (i = 0; i < iconTypePtr->pixels; i++)
		{
			int pixel = PixelPtrToLong(dataPtr + i * iconTypePtr->bypp,
				iconTypePtr->bypp);

			/* Not transparent */
			if (pixel != iconTypePtr->rle_pixel)
				maskPtr[i] = 1;

			/* Transparent */
			else
				maskPtr[i] = 0;
		}

		srcPtr = dataPtr;
		transparent = 1;
	}

	/* Simple */
	else
	{
		srcPtr = iconTypePtr->icon_data + iconSpec.index * iconTypePtr->length;
	}

	/* Convert the icon data to RGB */
	photoData = (unsigned char *) Tcl_AllocDebug(iconTypePtr->pixels * 3);
	dstPtr = photoData;
	for (i = 0; i < iconTypePtr->pixels; i++)
	{
		PixelPtrToRGB(srcPtr, &r, &g, &b, iconTypePtr->bypp);
		*dstPtr++ = r;
		*dstPtr++ = g;
		*dstPtr++ = b;
		srcPtr += iconTypePtr->bypp;
	}

	photoBlock.width = iconTypePtr->width;
	photoBlock.height = iconTypePtr->height;
	photoBlock.pixelSize = 3;
	photoBlock.pitch = photoBlock.width * photoBlock.pixelSize;
	photoBlock.offset[0] = 0;
	photoBlock.offset[1] = 1;
	photoBlock.offset[2] = 2;

	/*
	 * The next bit is adapted from FileReadGIF() in tkImgGIF.c
	 * from Tk 8.0.4
	 */
	if (transparent)
	{
		int start, end, y, x;

		/*
		 * The trick to creating a photo image with transparency
		 * is to only put image data into the masked regions.
		 */
		i = 0;
		for (y = 0; y < photoBlock.height; y++)
		{
			x = 0;
			while (x < photoBlock.width)
			{
				/* Search for first non-transparent pixel */
				while ((x < photoBlock.width) && !maskPtr[i])
				{
					x++; i++;
				}
				end = x;
				start = i;

				/* Search for first transparent pixel */
				while ((end < photoBlock.width) && maskPtr[i])
				{
					end++; i++;
				}
				if (end > x)
				{
					photoBlock.pixelPtr = &photoData[start * 3];
					Tk_PhotoPutBlock(interp, photoH, &photoBlock, x,
						y, end - x, 1, TK_PHOTO_COMPOSITE_SET);
				}
				x = end;
			}
		}
	}

	/* Not transparent */
	else
	{
		photoBlock.pixelPtr = photoData;

		/* Add the data to the photo image */
		Tk_PhotoPutBlock(interp, photoH, &photoBlock, 0, 0, photoBlock.width,
			photoBlock.height, TK_PHOTO_COMPOSITE_SET);
	}

	Tcl_FreeDebug((char *) photoData);
	Tcl_FreeDebug((char *) dataPtr);
	if (maskPtr)
		Tcl_FreeDebug((char *) maskPtr);

	return TCL_OK;
}

#ifdef USE_STARTUP_LOG
extern void startup_log(char *fmt, ...);
#endif

void Icon_MakeDark(t_icon_type *iconTypePtr, int index)
{
	int i, j, dark, length;
	IconPtr srcPtr, dstPtr;
	double gamma[2];

dbwin("Icon_MakeDark %s %d\n", iconTypePtr->desc, index);

	if (iconTypePtr->dark_data == NULL)
	{
		iconTypePtr->dark_data = (IconPtr *)
			Tcl_AllocDebug(iconTypePtr->icon_count * sizeof(IconPtr));
		for (i = 0; i < iconTypePtr->icon_count; i++)
		{
			iconTypePtr->dark_data[i] = NULL;
		}
	}

	if (iconTypePtr->dynamic)
	{
		length = iconTypePtr->rle_len[index];
		srcPtr = NULL;
	}
	else if (iconTypePtr->rle_data)
	{
		length = iconTypePtr->rle_len[index];
		srcPtr = iconTypePtr->rle_data + iconTypePtr->rle_offset[index];
	}
	else
	{
		length = iconTypePtr->length;
		srcPtr = iconTypePtr->icon_data + index * iconTypePtr->length;
	}

	if (iconTypePtr->dark_data[index] == NULL)
	{
		iconTypePtr->dark_data[index] =
			(IconPtr) Tcl_AllocDebug(2 * length);
	}

	/*
	 * For dynamic icons, instead of dark'ing the whole icon,
	 * we copy each dark'd component icon (if any exist).
	 */
	if (iconTypePtr->dynamic)
	{
		IconPtr tmp;

		/* New temp buffer for 1 icon */
		tmp = (IconPtr) Tcl_AllocDebug(iconTypePtr->length);

		for (dark = 0; dark < 2; dark++)
		{
			unsigned char *bounds;

			/* Init to transparent pixel */
			for (i = 0; i < iconTypePtr->pixels; i++)
			{
				PixelLongToPtr(tmp + i * iconTypePtr->bypp,
					iconTypePtr->rle_pixel, iconTypePtr->bypp);
			}

			for (i = 0; i < 4; i++)
			{
				IconSpec iconSpec;
				t_icon_type *srcTypePtr;

				/* Get the component icon */
				iconSpec = iconTypePtr->dynamic_spec[index * 4 + i];

				/* Stop */
				if (iconSpec.type == -1)
					break;

				/* Get the source icon type */
				srcTypePtr = &g_icon_type[iconSpec.type];

				/* Create dark data for component if needed */
				if (srcTypePtr->flags[iconSpec.index] & ICON_FLAG_DARK)
					Icon_MakeDark(srcTypePtr, iconSpec.index);

				/* Source data is not transparent */
				if (srcTypePtr->rle_data == NULL)
				{
					/* A darkened copy exists */
					if (srcTypePtr->dark_data && srcTypePtr->dark_data[iconSpec.index])
					{
						srcPtr = srcTypePtr->dark_data[iconSpec.index] +
							dark * srcTypePtr->length;
					}
					else
					{
						srcPtr = srcTypePtr->icon_data + srcTypePtr->length
							* iconSpec.index;
					}
					memcpy(tmp, srcPtr, srcTypePtr->length);
					continue;
				}

				/* A darkened copy exists */
				if (srcTypePtr->dark_data && srcTypePtr->dark_data[iconSpec.index])
				{
					srcPtr = srcTypePtr->dark_data[iconSpec.index] +
						dark * srcTypePtr->rle_len[iconSpec.index];
				}
				else
				{
					srcPtr = srcTypePtr->rle_data + srcTypePtr->rle_offset[iconSpec.index];
				}

				bounds = srcTypePtr->rle_bounds + iconSpec.index * 4;
				RL_Decode(bounds[2], bounds[3], srcTypePtr->bypp, srcPtr,
					tmp + bounds[0] * srcTypePtr->bypp +
					bounds[1] * srcTypePtr->pitch,
					srcTypePtr->pitch);
			}

			dstPtr = iconTypePtr->dark_data[index] +
				dark * iconTypePtr->rle_len[index];
			bounds = iconTypePtr->rle_bounds + index * 4;

			/* Encode */
			(void) RL_Encode(bounds[2], bounds[3], iconTypePtr->bypp,
				tmp + bounds[0] * iconTypePtr->bypp +
				bounds[1] * iconTypePtr->pitch,
				iconTypePtr->pitch, iconTypePtr->rle_pixel, dstPtr);
		}

		Tcl_FreeDebug((char *) tmp);

		/* Dark data created */
		iconTypePtr->flags[index] &= ~ICON_FLAG_DARK;

		return;
	}

	gamma[0] = iconTypePtr->gamma[0][index] / 100.0;
	gamma[1] = iconTypePtr->gamma[1][index] / 100.0;

	for (i = 0; i < 2; i++)
	{
		IconPtr darkData = iconTypePtr->dark_data[index] + i * length;
		int bypp = iconTypePtr->bypp;

		/* Copy the original icon */
		memcpy(darkData, srcPtr, length);

		/* Transparent */
		if (iconTypePtr->rle_data)
		{
			IconPtr rlePtr = darkData;

			/* Apply gamma-correction */
			while (1)
			{
				unsigned int trans, opaq;
				trans = rlePtr[0];
				opaq = rlePtr[1];
				if (!trans && !opaq)
					break;
				rlePtr += 2;
				for (j = 0; j < opaq; j++)
				{
					extern int gamma_correct(int value, double gamma);

					int r, g, b;

					PixelPtrToRGB(rlePtr + j * bypp, &r, &g, &b, bypp);

					/* Gamma-correct each RGB intensity */
					r = gamma_correct(r, gamma[i]);
					g = gamma_correct(g, gamma[i]);
					b = gamma_correct(b, gamma[i]);

					PixelSet_RGB(rlePtr + j * bypp, r, g, b, bypp);
				}
				rlePtr += opaq * bypp;
			}

			continue;
		}

		/* Darken */
		for (j = 0; j < iconTypePtr->pixels; j++)
		{
			extern int gamma_correct(int value, double gamma);

			int r, g, b;

			PixelPtrToRGB(darkData + j * iconTypePtr->bypp,
				&r, &g, &b, iconTypePtr->bypp);

			/* Gamma-correct each RGB intensity */
			r = gamma_correct(r, gamma[i]);
			g = gamma_correct(g, gamma[i]);
			b = gamma_correct(b, gamma[i]);

			PixelSet_RGB(darkData + j * iconTypePtr->bypp,
				r, g, b, iconTypePtr->bypp);
		}
	}

	/* Dark data created */
	iconTypePtr->flags[index] &= ~ICON_FLAG_DARK;
}

/*
 * objcmd_icon_dark --
 * (icon) dark $type $index ?$gamma $gamma?
 */
int
objcmd_icon_dark(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	int index, g1, g2, i;
	t_icon_type *iconTypePtr;
	double gamma[2];

	/* Required number of arguments */
	if (objc != 4 && objc != 6)
	{
		Tcl_WrongNumArgs(interp, 2, objv, "$type $index ?$gamma $gamma?");
		return TCL_ERROR;
	}

	if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Icon_GetIndexFromObj(interp, &index, objv[3], iconTypePtr) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (objc == 4)
	{
		if (iconTypePtr->gamma[0] == NULL)
		{
			/* Empty string as result */
			return TCL_OK;
		}
		g1 = iconTypePtr->gamma[0][index];
		g2 = iconTypePtr->gamma[1][index];
		FormatResult(interp, "%d.%d %d.%d", g1 / 100, g1 % 100,
			g2 / 100, g2 % 100);
		return TCL_OK;
	}

	/* Get the desired gamma value */
	if (Tcl_GetDoubleFromObj(interp, objv[4], &gamma[0]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the desired gamma value */
	if (Tcl_GetDoubleFromObj(interp, objv[5], &gamma[1]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (gamma[0] < 0.0 || gamma[0] > 2.0)
	{
		return TCL_ERROR;
	}
	if (gamma[1] < 0.0 || gamma[1] > 2.0)
	{
		return TCL_ERROR;
	}

	/* Pass 1.0 to free it */
	if (gamma[0] == 1.0)
	{
		if ((iconTypePtr->dark_data != NULL) &&
			(iconTypePtr->dark_data[index] != NULL))
		{
			Tcl_FreeDebug(iconTypePtr->dark_data[index]);
			iconTypePtr->dark_data[index] = NULL;
		}
		if (iconTypePtr->gamma[0])
		{
			iconTypePtr->gamma[0][index] = 100;
			iconTypePtr->gamma[1][index] = 100;
		}
		iconTypePtr->flags[index] &= ~ICON_FLAG_DARK;
		return TCL_OK;
	}

	if (iconTypePtr->gamma[0] == NULL)
	{
		iconTypePtr->gamma[0] = (unsigned char *)
			Tcl_AllocDebug(iconTypePtr->icon_count * sizeof(unsigned char));
		iconTypePtr->gamma[1] = (unsigned char *)
			Tcl_AllocDebug(iconTypePtr->icon_count * sizeof(unsigned char));
		for (i = 0; i < iconTypePtr->icon_count; i++)
		{
			iconTypePtr->gamma[0][i] = 100;
			iconTypePtr->gamma[1][i] = 100;
		}
	}

	iconTypePtr->gamma[0][index] = gamma[0] * 100;
	iconTypePtr->gamma[1][index] = gamma[1] * 100;
	iconTypePtr->flags[index] |= ICON_FLAG_DARK;

	return TCL_OK;
}

/*
 * objcmd_icon_dynamic --
 *	(icon) dynamic blank $type $index
 *	(icon) dynamic copy $type $index -type -index -ascii
 *	(icon) dynamic count $type ?$size?
 */
int
objcmd_icon_dynamic(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *cmdOption[] = {"blank", "copy", "count", NULL};
	enum {IDX_BLANK, IDX_COPY, IDX_COUNT} option;
	int index;

	t_icon_type *iconTypePtr;

	/* Required number of arguments */
	if (objc < 3)
	{
		Tcl_WrongNumArgs(interp, 1, objv, "option type ?arg ...?");
		return TCL_ERROR;
	}

	/* Get requested option */
	if (Tcl_GetIndexFromObj(interp, objv[1], cmdOption, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (!iconTypePtr->dynamic)
	{
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_BLANK:
		{
			IconPtr *mem;
			int i;

			if (Icon_GetIndexFromObj(interp, &index, objv[3], iconTypePtr)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}

			mem = (IconPtr *) iconTypePtr->rle_data;
			if (mem[index])
			{
				Tcl_FreeDebug((char *) mem[index]);
				mem[index] = NULL;
			}
			if (iconTypePtr->dark_data && iconTypePtr->dark_data[index])
			{
				Tcl_FreeDebug((char *) iconTypePtr->dark_data[index]);
				iconTypePtr->dark_data[index] = NULL;
			}
			for (i = 0; i < 4; i++)
				iconTypePtr->dynamic_spec[index * 4 + i].type = -1;
			break;
		}

		case IDX_COPY:
		{
			IconSpec iconSpec;
			IconPtr srcPtr, bits, *mem, tmp;
			t_icon_type *srcTypePtr;
			int i, len;
			unsigned char *srcBounds, *bounds;
			int bypp = iconTypePtr->bypp;

			if (Icon_GetIndexFromObj(interp, &index, objv[3], iconTypePtr)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Return list of components */
			if (objc == 4)
			{
				Tcl_Obj *listObjPtr = Tcl_NewListObj(0, NULL);
				for (i = 0; i < 4; i++)
				{
					char buf[64];
					iconSpec = iconTypePtr->dynamic_spec[index * 4 + i];
					if (iconSpec.type == -1)
						break;
					(void) sprintf(buf, "%s %d",
						g_icon_type[iconSpec.type].desc, iconSpec.index);
					Tcl_ListObjAppendElement(interp, listObjPtr,
						Tcl_NewStringObj(buf, -1));
				}
				Tcl_SetObjResult(interp, listObjPtr);
				break;
			}

			/* Scan the arguments for icon type and index */
			if (Icon_ParseArgs(interp, objc, objv, 4, &iconSpec) != TCL_OK)
			{
				/* Failure */
				return TCL_ERROR;
			}

			/* Get the source icon type */
			srcTypePtr = &g_icon_type[iconSpec.type];

			/* Source data is transparent */
			if (srcTypePtr->rle_data != NULL)
			{
				srcPtr = srcTypePtr->rle_data +
					srcTypePtr->rle_offset[iconSpec.index];
				srcBounds = srcTypePtr->rle_bounds + (iconSpec.index * 4);
			}

			/* Source data is not transparent*/
			else
			{
				/* FIXME: Since I am copying a non-transparent icon, this
				 * should not be a RLE icon */
				srcPtr = srcTypePtr->icon_data + iconSpec.index *
					srcTypePtr->length;
			}

			/* Dest icon data */
			mem = (IconPtr *) iconTypePtr->rle_data;

			/* New temp buffer for 1 icon */
			tmp = (IconPtr) Tcl_AllocDebug(iconTypePtr->length);

			/* Init to transparent pixel */
			for (i = 0; i < iconTypePtr->pixels; i++)
			{
				PixelLongToPtr(tmp + i * bypp,
					iconTypePtr->rle_pixel, bypp);
			}

			bounds = iconTypePtr->rle_bounds + index * 4;

			/* If old data, decode it first */
			if (mem[index])
			{
				RL_Decode(bounds[2], bounds[3], bypp,
					mem[index], tmp + bounds[0] * bypp + 
					bounds[1] * iconTypePtr->pitch, iconTypePtr->pitch);
				Tcl_FreeDebug((char *) mem[index]);
				mem[index] = NULL;
			}

			/* Source data is transparent */
			if (srcTypePtr->rle_data != NULL)
			{
				/* Decode source data */
				RL_Decode(srcBounds[2], srcBounds[3], bypp,
					srcPtr, tmp + srcBounds[0] * bypp + srcBounds[1] *
					srcTypePtr->pitch, srcTypePtr->pitch);
			}
			else
			{
				/* Copy source data */
				memcpy(tmp, srcPtr, srcTypePtr->length);
			}

			/* Get bounds of non-transparent pixels */
			RL_Bounds(iconTypePtr->width, iconTypePtr->height, bypp,
				tmp, iconTypePtr->rle_pixel, bounds);

			/* Get length of encoded data */
			len = RL_Len(bounds[2], bounds[3],
				bypp, tmp + bounds[0] * bypp + bounds[1] * iconTypePtr->pitch,
				iconTypePtr->pitch, iconTypePtr->rle_pixel);

			/* Allocate RLE buf */
			/* FIXME: Does RL_Encode() still go past end of buffer? */
			bits = (IconPtr) Tcl_AllocDebug(len);

			/* Encode */
			(void) RL_Encode(bounds[2], bounds[3],
				bypp, tmp + bounds[0] * bypp + bounds[1] * iconTypePtr->pitch,
				iconTypePtr->pitch, iconTypePtr->rle_pixel, bits);

			mem[index] = bits;
			iconTypePtr->rle_len[index] = len;

			/* Keep a list of component icons */
			for (i = 0; i < 4; i++)
			{
				if (iconTypePtr->dynamic_spec[index * 4 + i].type == -1)
				{
					iconTypePtr->dynamic_spec[index * 4 + i] = iconSpec;
					break;
				}
			}

			Tcl_FreeDebug((char *) tmp);

			break;
		}

		case IDX_COUNT:
		{
			IconPtr *newMem, *oldMem;
			int i, count, *newLen, *oldLen;
			unsigned char *newBounds, *oldBounds;
			IconSpec *oldSpec, *newSpec;
			short *oldFlags, *newFlags;

			if (objc == 3)
			{
				IntResult(interp, iconTypePtr->icon_count);
				break;
			}
			if (Tcl_GetIntFromObj(interp, objv[3], &count) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (count < 0)
			{
				return TCL_ERROR;
			}
dbwin("dynamic count %s %d -> %d\n", iconTypePtr->desc, iconTypePtr->icon_count, count);
			newMem = (IconPtr *) Tcl_AllocDebug(sizeof(IconPtr) * count);
			newBounds = (unsigned char *) Tcl_AllocDebug(sizeof(unsigned char) * count * 4);
			memset(newBounds, '\0', count * 4);
			newLen = (int *) Tcl_AllocDebug(sizeof(int) * count);
			newSpec = (IconSpec *) Tcl_AllocDebug(sizeof(IconSpec) * count * 4);
			newFlags = (short *) Tcl_AllocDebug(sizeof(short) * count);
			for (i = 0; i < count * 4; i++)
				newSpec[i].type = -1;
			for (i = 0; i < count; i++)
			{
				newMem[i] = NULL;
				newLen[i] = 0;
				newFlags[i] = 0;
			}
			oldMem = (IconPtr *) iconTypePtr->rle_data;
			oldBounds = iconTypePtr->rle_bounds;
			oldLen = iconTypePtr->rle_len;
			oldSpec = iconTypePtr->dynamic_spec;
			oldFlags = iconTypePtr->flags;
			if (oldMem)
			{
				for (i = 0; i < count; i++)
				{
					newMem[i] = oldMem[i];
					memcpy(newBounds + i * 4, oldBounds + i * 4, 4);
					newLen[i] = oldLen[i];
				}
				for ( ; i < iconTypePtr->icon_count; i++)
				{
					Tcl_FreeDebug((char *) oldMem[i]);
				}
				Tcl_FreeDebug((char *) oldMem);
				Tcl_FreeDebug((char *) oldBounds);
				Tcl_FreeDebug((char *) oldLen);

				for (i = 0; i < count * 4; i++)
					newSpec[i] = oldSpec[i];
				Tcl_FreeDebug((char *) oldSpec);

				Tcl_FreeDebug((char *) oldFlags);
			}
			iconTypePtr->icon_count = count;
			iconTypePtr->rle_data = (IconPtr) newMem;
			iconTypePtr->rle_bounds = newBounds;
			iconTypePtr->rle_len = newLen;
			iconTypePtr->dynamic_spec = newSpec;
			iconTypePtr->flags = newFlags;
			break;
		}
	}

	return TCL_OK;
}

/*
 * objcmd_icon --
 */
int
objcmd_icon(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *cmdOption[] = {"createtype", "count",
		"gettypes", "validate", "ascii",
		"gamma", "photo", "makeicon", "depth", "dump",
		"rle", "height", "width", "dynamic", "duplicate",
		"transparent", "dark", "flags", "style", "darkhack", NULL};
	enum {IDX_CREATETYPE, IDX_COUNT,
		IDX_GETTYPES, IDX_VALIDATE, IDX_ASCII,
		IDX_GAMMA, IDX_PHOTO, IDX_MAKEICON, IDX_DEPTH, IDX_DUMP,
		IDX_RLE, IDX_HEIGHT, IDX_WIDTH, IDX_DYNAMIC, IDX_DUPLICATE,
		IDX_TRANSPARENT, IDX_DARK, IDX_FLAGS, IDX_STYLE, IDX_DARKHACK} option;
	int index;

	Tcl_Obj *CONST *objPtr;
	Tcl_Obj *listObjPtr;
	char *typeName = NULL, *iconFile = NULL;
	char *fontName = NULL;
	char *charSet = NULL;
	Tcl_HashEntry *hPtr;
	int i;
	t_icon_type iconData, *iconTypePtr;
	IconSpec iconSpec = {-1, -1, -1};

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
		case IDX_CREATETYPE: /* createtype */
		{
			if (objc < 5)
			{
wrongCreateArgs:
				Tcl_WrongNumArgs(interp, 2, objv,
					"typeName ?-file fileName? ?-font font? ?...?");
argsFailed:
				return TCL_ERROR;
			}

			/* Get the name of the new icon type */
			typeName = Tcl_GetStringFromObj(objv[2], NULL);
#ifdef USE_STARTUP_LOG
startup_log("createtype: typeName is \"%s\"", typeName);
#endif
			/* Lookup the icon type by name */
			hPtr = Tcl_FindHashEntry(&g_icon_table, typeName);

			/* The icon type already exists */
			if (hPtr != NULL)
			{
				/* Set the error */
				FormatResult(interp, "icon type \"%s\" already exists",
					typeName);

				/* Failure */
				return TCL_ERROR;
			}

			/* Point to the first configuration option */
			objPtr = objv + 3;
			objc -= 3;

			iconData.dynamic = FALSE;
			iconData.icon_count = 0;
			iconData.icon_data = NULL;
			iconData.char_table = NULL;

			iconData.height = g_icon_height;
			iconData.width = g_icon_width;
			iconData.depth = g_icon_depth;
			iconData.bypp = g_pixel_size;

			/* Scan all option/value pairs */
			while (objc > 1)
			{
				static CONST char *createSwitch[] = {
					"-charset", "-file", "-font", "-height", "-width",
					"-dynamic", NULL
				};

				if (Tcl_GetIndexFromObj(interp, objPtr[0], createSwitch,
					"switch", 0, &index) != TCL_OK)
					goto argsFailed;
#ifdef USE_STARTUP_LOG
startup_log("createtype: option is \"%s\"", createSwitch[index]);
#endif
				switch (index)
				{
					case 0: /* -charset */
						charSet = Tcl_GetStringFromObj(objPtr[1], NULL);
						break;

					case 1: /* -file */
/*						if (iconFile != NULL) goto wrongCreateArgs; */
						iconFile = Tcl_GetStringFromObj(objPtr[1], NULL);
						break;

					case 2: /* -font */
						fontName = Tcl_GetStringFromObj(objPtr[1], NULL);
						break;

					case 3: /* -height */
						if (Tcl_GetIntFromObj(interp, objPtr[1], &iconData.height) != TCL_OK)
							goto argsFailed;
						break;

					case 4: /* -width */
						if (Tcl_GetIntFromObj(interp, objPtr[1], &iconData.width) != TCL_OK)
							goto argsFailed;
						break;

					case 5: /* -dynamic */
						if (Tcl_GetBooleanFromObj(interp, objPtr[1], &iconData.dynamic) != TCL_OK)
							goto argsFailed;
						break;
				}

				/* Next option/value pair */
				objPtr += 2;
				objc -= 2;
			}

			/* Required number of arguments */
			if ((objc != 0) ||
				(!iconData.dynamic && (iconFile == NULL) && (fontName == NULL)) ||
				(iconData.height <= 0) || (iconData.width <= 0))
			{
				goto wrongCreateArgs;
			}
#ifdef USE_STARTUP_LOG
startup_log("createtype: iconFile is \"%s\" fontName is \"%s\" charSet is \"%s\"",
	iconFile, fontName, charSet);
#endif

			/* Calculate some values */
			iconData.pitch = iconData.width * iconData.bypp;
			iconData.length = iconData.height * iconData.pitch;
			iconData.pixels = iconData.width * iconData.height;

			/* An icon file was specified */
			if (iconFile != NULL)
			{
				/* Read the data file */
				if (ReadIconFile(interp, iconFile, &iconData) != TCL_OK)
				{
					/* Failure */
					return TCL_ERROR;
				}

				/* 256-color icons */
				if (iconData.depth == 8)
				{
					int j;

					/* Check each icon */
					for (i = 0; i < iconData.icon_count; i++)
					{
						/* Access the icon */
						IconPtr iconPtr = iconData.icon_data + i * iconData.length;

						/* Convert palette index to colormap index */
						for (j = 0; j < iconData.length; j++)
						{
							/* Convert palette index to colormap index */
							iconPtr[j] = g_palette2colormap[iconPtr[j]];
						}
					}
				}
			}

			/*
			 * A font description was given, meaning we are going to
			 * create a brand-new set of ascii type icons.
			 */
			if (fontName != NULL)
			{
#ifdef USE_STARTUP_LOG
startup_log("createtype: Tk_GetFont(%s)...", fontName);
#endif
				/* Get the requested font */
				iconData.font = Tk_GetFont(interp, Tk_MainWindow(interp),
					fontName);
#ifdef USE_STARTUP_LOG
startup_log("createtype: Tk_GetFont(%s) okay", fontName);
#endif
				/* The font could not be created */
				if (iconData.font == NULL)
				{
					return TCL_ERROR;
				}

				/* A set of characters was specified */
				if (charSet != NULL)
				{
					/* The number of icons is the number of characters */
					iconData.icon_count = strlen(charSet);
#ifdef USE_STARTUP_LOG
startup_log("createtype: strlen(charSet) is %d", iconData.icon_count);
#endif
					/*
					 * The char_table is used to remember
					 * which characters each icon represents.
					 */
					iconData.char_table = (int *)
						Tcl_AllocDebug(sizeof(int) * iconData.icon_count);

					/* Check each icon */
					for (i = 0; i < iconData.icon_count; i++)
					{
						/* Remember the character the i'th icon represents */
						iconData.char_table[i] = charSet[i];
					}
				}

				/* No character set specified, so use full printable set */
				else
				{
					iconData.icon_count = 126 - 32 + 1; /* printable only */
				}
#ifdef USE_STARTUP_LOG
startup_log("createtype: Tcl_Alloc(%ld)", iconData.icon_count * ICON_LENGTH);
#endif
				/* Allocate the icon data buffer */
				iconData.icon_data = (IconPtr) Tcl_AllocDebug(iconData.icon_count *
					iconData.length);

				/* Set the icon data using the desired font */
				if (init_ascii_data(interp, &iconData) != TCL_OK)
				{
					return TCL_ERROR;
				}
			}

			/* No font was specified */
			else
			{
				iconData.font = NULL;
			}

			iconData.desc = typeName;
			Icon_AddType(&iconData);
			break;
		}

		case IDX_COUNT: /* count */
			if (objc != 3)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "typeName");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Return the number of icons */
			IntResult(interp, iconTypePtr->icon_count);
			break;

		case IDX_GETTYPES: /* gettypes */
			if (objc != 2)
			{
				Tcl_WrongNumArgs(interp, 2, objv, NULL);
				return TCL_ERROR;
			}

			/* Create a new Tcl list object */
			listObjPtr = Tcl_NewListObj(0, NULL);

			/* Check each icon type */
			for (i = 0; i < g_icon_type_count; i++)
			{
				/* Append icon type name as a string to the list object */
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj(g_icon_type[i].desc, -1));
			}

			/* Return the list object */
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_VALIDATE: /* validate */
			if (objc < 6)
			{
				Tcl_WrongNumArgs(interp, 2, objv,
					"-type iconType -index iconIndex ?-ascii asciiIndex?");
				return TCL_ERROR;
			}
			if (Icon_ParseArgs(interp, objc, objv, 2, &iconSpec) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		case IDX_ASCII: /* ascii */
			return objcmd_ascii(dummy, interp, objc - 1, objv + 1);

		case IDX_GAMMA: /* gamma */
		{
			double gamma;
			int i, j, first, last;
			TintTable table;

			if (objc < 4 || objc > 5)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type gamma ?index?");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the desired gamma value */
			if (Tcl_GetDoubleFromObj(interp, objv[3], &gamma) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the icon index */
			if (objc == 5)
			{
				if (Tcl_GetIntFromObj(interp, objv[4], &first) != TCL_OK)
				{
					return TCL_ERROR;
				}

				/* And the first shall be last */
				last = first;
			}
			else
			{
				first = 0;
				last = iconTypePtr->icon_count - 1;
			}

			/* Aaargh! */
			if (iconTypePtr->depth != 8)
				break;

			/* Initialize the 256-color gamma-corrected tint table */
			Colormap_GammaTable(gamma, table);

			/* Transparent */
			if (iconTypePtr->rle_data)
			{
				for (i = first; i <= last; i++)
				{
					IconPtr rlePtr = iconTypePtr->rle_data +
						iconTypePtr->rle_offset[i];

					/* Apply gamma-correction */
					while (1)
					{
						unsigned int trans, opaq;
						trans = rlePtr[0];
						opaq = rlePtr[1];
						if (!trans && !opaq)
							break;
						rlePtr += 2;
						for (j = 0; j < opaq; j++)
						{
							rlePtr[j] = table[rlePtr[j]];
						}
						rlePtr += opaq;
					}
				}
			}

			/* Unmasked */
			else
			{
				/* Check each icon */
				for (i = first; i <= last; i++)
				{
					/* Access the icon */
					IconPtr iconPtr = iconTypePtr->icon_data + i * iconTypePtr->length;

					/* Gamma-correct each byte */
					for (j = 0; j < iconTypePtr->length; j++)
					{
						/* Apply gamma-correction */
						iconPtr[j] = table[iconPtr[j]];
					}
				}
			}
			break;
		}

		case IDX_PHOTO: /* photo */
			return objcmd_icon_photo(dummy, interp, objc, objv);

		case IDX_MAKEICON: /* makeicon */
			return objcmd_makeicon(dummy, interp, objc - 1, objv + 1);

		case IDX_DEPTH: /* depth */
			IntResult(interp, g_icon_depth);
			break;

		case IDX_DUMP: /* dump */
		{
#if 0
			int y, x;
			PixelPtr p;

			if (Icon_ParseArgs(interp, objc, objv, 2, &iconSpec) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Access the icon data */
			iconTypePtr = &g_icon_type[iconSpec.type];

			if (iconTypePtr->mask_count)
			{
				p.pix8 = iconTypePtr->mask_bits +
					iconTypePtr->mask_table[iconSpec.index] *
					iconTypePtr->length;
			}
			else
			{
				p.pix8 = iconTypePtr->icon_data + iconSpec.index
					* iconTypePtr->length;
			}
			for (y = 0; y < iconTypePtr->height; y++)
			{
				for (x = 0; x < iconTypePtr->width; x++)
				{
					switch (iconTypePtr->depth)
					{
						case 8:
							Tcl_AppendResult(interp, format("%02x ", *p.pix8++), NULL);
							break;
						case 16:
							Tcl_AppendResult(interp, format("%04x ", (int) *p.pix16++), NULL);
							break;
						case 24:
							Tcl_AppendResult(interp, format("%02x%02x%02x ",
								p.pix8[0], p.pix8[1], p.pix8[2]), NULL);
							p.pix8 += 3;
							break;
					}
				}
				Tcl_AppendResult(interp, "\n", NULL);
			}
#endif
			break;
		}

		case IDX_RLE: /* rle */
		{
			XColor *xColorPtr;
			Tk_Window tkwin = Tk_MainWindow(interp);

			if (objc != 4)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "typeName color");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the transparent color */
			xColorPtr = Tk_AllocColorFromObj(interp, tkwin, objv[3]);
			if (xColorPtr == NULL)
			{
				return TCL_ERROR;
			}
			iconTypePtr->rle_pixel = Plat_XColor2Pixel(xColorPtr); 

/* July 20 2005 for cygwin X11 */
if (g_pixel_size == 4) iconTypePtr->rle_pixel |= 0xFF000000;

			/* Check size because 'dynamic' needs rle_pixel too */
			if (iconTypePtr->icon_count)
				Icon_MakeRLE(iconTypePtr);

			Tk_FreeColor(xColorPtr);
			break;
		}

		case IDX_HEIGHT: /* height */
		{
			if (objc == 2)
			{
				IntResult(interp, g_icon_height);
				break;
			}

			if (objc != 3)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "?typeName?");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			IntResult(interp, iconTypePtr->height);
			break;
		}

		case IDX_WIDTH: /* width */
		{
			if (objc == 2)
			{
				IntResult(interp, g_icon_width);
				break;
			}

			if (objc != 3)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "?typeName?");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			IntResult(interp, iconTypePtr->width);
			break;
		}

		case IDX_DYNAMIC: /* dynamic */
			return objcmd_icon_dynamic(dummy, interp, objc - 1, objv + 1); 

		case IDX_DUPLICATE: /* duplicate */
		{
			int index, count;

			if (objc != 5)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type index count");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (iconTypePtr->dynamic || iconTypePtr->rle_data || iconTypePtr->font)
				return TCL_ERROR;

			/* Get the icon index */
			if (Icon_GetIndexFromObj(interp, &index, objv[3], iconTypePtr)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the number of duplicates */
			if (Tcl_GetIntFromObj(interp, objv[4], &count) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (count < 0)
				return TCL_ERROR;
			if (!count)
				break;

			/* Allocate more space */
			iconTypePtr->icon_data = Tcl_ReallocDebug(iconTypePtr->icon_data,
				(iconTypePtr->icon_count + count) * iconTypePtr->length);

			/* Move following icons down */
			memcpy(
				iconTypePtr->icon_data + (index + 1 + count) * iconTypePtr->length,
				iconTypePtr->icon_data + (index + 1) * iconTypePtr->length,
				(iconTypePtr->icon_count - index - 1) * iconTypePtr->length);

			/* Copy icon */
			for (i = 0; i < count; i++)
			{
				memcpy(
					iconTypePtr->icon_data + (index + 1 + i) * iconTypePtr->length,
					iconTypePtr->icon_data + index * iconTypePtr->length,
					iconTypePtr->length);
			}

			/* New number of icons */
			iconTypePtr->icon_count += count;
			break;
		}

		case IDX_TRANSPARENT: /* transparent */
		{
			int trans = TRUE;

			if (objc != 4)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type index");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the icon index */
			if (Icon_GetIndexFromObj(interp, &index, objv[3], iconTypePtr)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Not transparent */
			if (iconTypePtr->rle_data == NULL)

				/* It *is* transparent when using isometric code */
				trans = (g_icon_style == ICON_STYLE_ISO);

			/* Special non-transparent "floor" icon */
			if (iconTypePtr->flags[index] & ICON_FLAG_ISO)
				trans = FALSE;

			BooleanResult(interp, trans);
			break;
		}

		case IDX_DARK: /* dark */
			return objcmd_icon_dark(dummy, interp, objc, objv);

		case IDX_FLAGS: /* flags */
		{
			static CONST char *flags[] = {"left", "right", "isofloor", NULL};
			int flag;

			if (objc < 4 || objc > 6)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type index ?flag? ?boolean?");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the icon index */
			if (Icon_GetIndexFromObj(interp, &index, objv[3], iconTypePtr)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Return value of all flags */
			if (objc == 4)
			{
				Tcl_Obj *listObjPtr = Tcl_NewListObj(0, NULL);

				for (flag = 0; flags[flag] != NULL; flag++)
				{
					if (iconTypePtr->flags[index] & (1L << (flag + 1)))
					{
						Tcl_ListObjAppendElement(interp, listObjPtr,
							Tcl_NewStringObj(flags[flag], -1));
					}
				}
				Tcl_SetObjResult(interp, listObjPtr);
				break;
			}

			if (Tcl_GetIndexFromObj(interp, objv[4], flags, "flag", 0, 
				&flag) != TCL_OK)
			{
				return TCL_ERROR;
			}
			++flag;

			/* Return value of one flag */
			if (objc == 5)
			{
				BooleanResult(interp,
					(iconTypePtr->flags[index] & (1L << flag)) != 0);
				break;
			}

			/* Set value of one flag */
			iconTypePtr->flags[index] |= (1L << flag);

			break;
		}

		case IDX_STYLE: /* style */
		{
			static CONST char *keyword_icon_style[] = {"icon", "iso", NULL};
			if (objc == 3)
			{
				if (Tcl_GetIndexFromObj(interp, objv[2], keyword_icon_style,
					"style", 0, &g_icon_style) != TCL_OK)
				{
					return TCL_ERROR;
				}
				break;
			}
			StaticResult(interp, (char *) keyword_icon_style[g_icon_style]);
			break;
		}

		/* Lets us copy data from an existing icon to dark_data */
		case IDX_DARKHACK: /* darkhack */
		{
			t_icon_type *it1, *it2;
			int ii1, ii2;

			if (objc != 8)
			{
				Tcl_WrongNumArgs(interp, 2, objv, "type index t1 i1 t2 i2");
				return TCL_ERROR;
			}

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &iconTypePtr, NULL, objv[2]) != TCL_OK)
				return TCL_ERROR;

			/* Get the icon index */
			if (Icon_GetIndexFromObj(interp, &index, objv[3], iconTypePtr)
				!= TCL_OK)
				return TCL_ERROR;

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &it1, NULL, objv[4]) != TCL_OK)
				return TCL_ERROR;

			/* Get the icon index */
			if (Icon_GetIndexFromObj(interp, &ii1, objv[5], it1) != TCL_OK)
				return TCL_ERROR;

			/* Lookup the icon type by name */
			if (Icon_GetTypeFromObj(interp, &it2, NULL, objv[6]) != TCL_OK)
				return TCL_ERROR;

			/* Get the icon index */
			if (Icon_GetIndexFromObj(interp, &ii2, objv[7], it2) != TCL_OK)
				return TCL_ERROR;

			/* Next stuff is like Icon_MakeDark */
			if (iconTypePtr->dark_data == NULL)
			{
				iconTypePtr->dark_data = (IconPtr *)
					Tcl_AllocDebug(iconTypePtr->icon_count * sizeof(IconPtr));
				for (i = 0; i < iconTypePtr->icon_count; i++)
					iconTypePtr->dark_data[i] = NULL;
			}
			if (iconTypePtr->dark_data[index] == NULL)
				iconTypePtr->dark_data[index] =
					(IconPtr) Tcl_AllocDebug(2 * iconTypePtr->length);

			/* FIXME: check for different icons sizes, rle, ascii */
			memcpy(iconTypePtr->dark_data[index],
				it1->icon_data + ii1 * it1->length,
				iconTypePtr->length);
			memcpy(iconTypePtr->dark_data[index] + iconTypePtr->length,
				it2->icon_data + ii2 * it1->length,
				iconTypePtr->length);
			iconTypePtr->flags[index] &= ~ICON_FLAG_DARK;
			break;
		}
	}

	/* Success */
	return TCL_OK;
}

/*
 * objcmd_makeicon --
 *
 * makeicon ?-makemask? ?-scaleup? ?--? iconSize imageFile dataFile
 */
int
objcmd_makeicon(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	Tcl_Obj *CONST *objPtr;
	char buf[1024];
	int index;

	Tk_PhotoHandle photoH = NULL;
	char *imageFile = NULL, *dataFile = NULL, *imageName = "MakeIconImage";
	int imageW, imageH;
	int length;
	t_icon_type iconData;
	XColor *xColorPtr = NULL;
	int result = TCL_ERROR;

	iconData.icon_data = NULL;
	iconData.depth = g_icon_depth;
	iconData.bypp = g_pixel_size;
	iconData.height = iconData.width = 0;
	imageW = imageH = 0;

	objPtr = objv + 1;
	objc -= 1;

	/* Scan arguments for options */
	while (objc > 0)
	{
		static CONST char *switches[] = {"-datafile", "-iconheight", "-iconwidth",
			"-imagefile", "-imageheight", "-imagewidth", "-transparent", NULL};
		int n = 2;

		/* Get the sub-option */
		if (Tcl_GetIndexFromObj(interp, objPtr[0], switches, "switch",
			0, &index) != TCL_OK)
		{
			goto error;
		}

		switch (index)
		{
			case 0: /* -datafile */
				dataFile = Tcl_GetStringFromObj(objPtr[1], NULL);
				break;

			case 1: /* -iconheight */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &iconData.height) != TCL_OK)
				{
					goto error;
				}
				break;

			case 2: /* -iconwidth */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &iconData.width) != TCL_OK)
				{
					goto error;
				}
				break;

			case 3: /* -imagefile */
				imageFile = Tcl_GetStringFromObj(objPtr[1], NULL);
				break;

			case 4: /* -imageheight */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &imageH) != TCL_OK)
				{
					goto error;
				}
				break;

			case 5: /* -imagewidth */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &imageW) != TCL_OK)
				{
					goto error;
				}
				break;

			case 6: /* -transparent */
			{
				Tk_Window tkwin = Tk_MainWindow(interp);
				if (xColorPtr)
					Tk_FreeColor(xColorPtr);
				xColorPtr = Tk_AllocColorFromObj(interp, tkwin, objPtr[1]);
				if (xColorPtr == NULL)
				{
					goto error;
				}
				break;
			}
		}

		objc -= n;
		objPtr += n;
	}

	if (objc || (dataFile == NULL) || (imageFile == NULL))
	{
		Tcl_WrongNumArgs(interp, 1, objv, NULL);
		goto error;
	}

	if ((iconData.width <= 0) || (iconData.height <= 0))
	{
		FormatResult(interp, "invalid icon size \"%d x %d\"",
			iconData.width, iconData.height);
		goto error;
	}

	iconData.length = iconData.width * iconData.height * g_pixel_size;

	if (imageW <= 0)
		imageW = iconData.width;
	if (imageH <= 0)
		imageH = iconData.height;

	/* FIXME */
	if ((imageW != iconData.width) && (imageW != 16))
	{
		StaticResult(interp, "can only scale a 16x16 image");
		goto error;
	}

	/* Read the image file */
	length = sprintf(buf, "image create photo %s -file \"%s\"",
		imageName, imageFile);
	if (Tcl_EvalEx(interp, buf, length, TCL_EVAL_GLOBAL) != TCL_OK)
	{
		goto error;
	}

	/* Lookup the photo by name */
	photoH = Tk_FindPhoto(interp, imageName);

	/* The photo was not found */
	if (photoH == NULL)
	{
		goto error;
	}

	/* Convert 24-bit image to g_icon_depth-bit data */
	if (Image2Bits(interp, &iconData, photoH, imageW, imageH, xColorPtr) != TCL_OK)
	{
		goto error;
	}

	if (WriteIconFile(interp, dataFile, &iconData) != TCL_OK)
	{
		goto error;
	}

	result = TCL_OK;

error:
	if (xColorPtr)
		Tk_FreeColor(xColorPtr);
	if (iconData.icon_data)
		Tcl_FreeDebug((char *) iconData.icon_data);
	if (photoH)
	{
		length = sprintf(buf, "image delete %s", imageName);
		(void) Tcl_EvalEx(interp, buf, length, TCL_EVAL_GLOBAL);
	}
	return result;
}

int Icon_FindTypeByName(Tcl_Interp *interp, int *typeIndexPtr, char *typeName)
{
	Tcl_HashEntry *hPtr;

	/* Look up the icon type by name */
	hPtr = Tcl_FindHashEntry(&g_icon_table, typeName);

	/* The icon type was not found */
	if (hPtr == NULL)
	{
		/* Set the error */
		FormatResult(interp, "unknown icon type \"%s\"", typeName);

		/* Failure */
		return TCL_ERROR;
	}

	(*typeIndexPtr) = (int) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

int Icon_GetTypeFromObj(Tcl_Interp *interp, t_icon_type **typePtrPtr,
	int *typeIndexPtr, Tcl_Obj *objPtr)
{
	int typeIndex;

	if (Icon_FindTypeByName(interp, &typeIndex, Tcl_GetString(objPtr))
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	if (typeIndexPtr != NULL)
		(*typeIndexPtr) = typeIndex;
	if (typePtrPtr != NULL)
		(*typePtrPtr) = &g_icon_type[typeIndex];

	return TCL_OK;
}

int Icon_GetIndexFromObj(Tcl_Interp *interp, int *indexPtr,
	Tcl_Obj *objPtr, t_icon_type *iconTypePtr)
{
	int index;

	if (Tcl_GetIntFromObj(interp, objPtr, &index) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((index < 0) || (index >= iconTypePtr->icon_count))
	{
		/* Set the error */
		FormatResult(interp, "bad icon index \"%d\": must be from 0 to %d",
			index, iconTypePtr->icon_count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	(*indexPtr) = index;

	return TCL_OK;
}

int Icon_Validate(Tcl_Interp *interp, char *typeName, int index, int ascii,
	IconSpec *specPtr)
{
	t_icon_type *iconTypePtr;
	int type;

	/* Look up the icon type by name */
	if (Icon_FindTypeByName(interp, &type, typeName) != TCL_OK)
	{
		return TCL_ERROR;
	}

	iconTypePtr = &g_icon_type[type];

	/* Font-icon must specify ascii */
	if ((iconTypePtr->font != NULL) && (ascii == -1))
	{
		FormatResult(interp, "icon type \"%s\" is ascii", typeName);
		return TCL_ERROR;
	}

	/* Non-font-icon must not specify ascii */
	if ((iconTypePtr->font == NULL) && (ascii != -1))
	{
		FormatResult(interp, "icon type \"%s\" is not ascii", typeName);
		return TCL_ERROR;
	}

	/* Verify index */
	if ((index < 0) || (index >= iconTypePtr->icon_count))
	{
		/* Set the error */
		FormatResult(interp, "bad icon index \"%d\": must be from 0 to %d",
			index, iconTypePtr->icon_count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	if ((ascii != -1) && (ascii >= g_ascii_count))
	{
		/* Set the error */
		FormatResult(interp, "bad ascii index \"%d\": "
			"must be from 0 to %d", ascii,
			g_ascii_count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	specPtr->type = type;
	specPtr->index = index;
	specPtr->ascii = ascii;

	/* Success */
	return TCL_OK;
}

/*
 * Scans the given object argument array and fills 'iconPtr' with the
 * corresponding type and index. Sets the interpreter result with an
 * error if the specified icon type or index is not valid.
 */
int Icon_ParseArgs(Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[],
	int offset, IconSpec *specPtr)
{
	int index;
	Tcl_Obj *CONST *objPtr;
	char *typeName = NULL;
	IconSpec iconSpec;

	objPtr = objv + offset;
	objc -= offset;

	iconSpec.type = -1;
	iconSpec.index = -1;
	iconSpec.ascii = -1;

	while (objc > 1)
	{
		static CONST char *assignSwitch[] = {"-ascii", "-index", "-type", NULL};

		if (Tcl_GetIndexFromObj(interp, objPtr[0], assignSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
		}
		switch (index)
		{
			case 0: /* ascii */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &iconSpec.ascii)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				break;

			case 1: /* -index */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &iconSpec.index)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				break;

			case 2: /* -type */
				typeName = Tcl_GetStringFromObj(objPtr[1], NULL);
				break;

		}
		objPtr += 2;
		objc -= 2;
	}

	/* Required number of arguments */
	if ((objc != 0) || (typeName == NULL) || (iconSpec.index == -1))
	{
		/* Set the error */
		Tcl_WrongNumArgs(interp, offset, objv,
			"-type iconType -index iconIndex ?-ascii asciiIndex?");

		/* Failure */
		return TCL_ERROR;
	}

	if (Icon_Validate(interp, typeName, iconSpec.index, iconSpec.ascii,
		specPtr) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

/*
 *
 */
void Icon_AddType(t_icon_type *data)
{
	int new;
	t_icon_type iconData, *iconTypePtr = &iconData;
	Tcl_HashEntry *hPtr;
#if 1
	memset(iconTypePtr, 0, sizeof(t_icon_type));
#else
	g_icon_type = Array_Insert(g_icon_type, &g_icon_type_count,
		sizeof(t_icon_type), g_icon_type_count);
	iconTypePtr = &g_icon_type[g_icon_type_count - 1];
#endif
	iconTypePtr->desc = common_dll_string_make(data->desc);
	iconTypePtr->icon_count = data->icon_count;
	iconTypePtr->icon_data = data->icon_data;
	iconTypePtr->char_table = data->char_table;
	iconTypePtr->font = data->font;
	iconTypePtr->dynamic = data->dynamic;
	iconTypePtr->depth = data->depth;
	iconTypePtr->bypp = data->bypp;
	iconTypePtr->width = data->width;
	iconTypePtr->height = data->height;
	iconTypePtr->pitch = data->pitch;
	iconTypePtr->length = data->length;
	iconTypePtr->pixels = data->pixels;

	/* Could be dynamic */
	if (data->icon_count)
	{
		iconTypePtr->flags = Tcl_AllocDebug(
			sizeof(short) * iconTypePtr->icon_count);
		memset(iconTypePtr->flags, 0,
			sizeof(short) * iconTypePtr->icon_count);
	}

	g_icon_type = Array_Append(g_icon_type, &g_icon_type_count,
		sizeof(t_icon_type), iconTypePtr);

	hPtr = Tcl_CreateHashEntry(&g_icon_table, data->desc, &new);
	Tcl_SetHashValue(hPtr, (ClientData) (g_icon_type_count - 1));
}

void Icon_FreeType(t_icon_type *typePtr)
{
	int i;
	Tcl_HashEntry *hPtr;

	/* Important: free the font, otherwise Tk panics at shutdown */
	if (typePtr->font)
		Tk_FreeFont(typePtr->font);

	if (typePtr->char_table)
		Tcl_FreeDebug(typePtr->char_table);

	if (typePtr->icon_data)
		Tcl_FreeDebug(typePtr->icon_data);

	if (typePtr->rle_data)
	{
		if (typePtr->dynamic)
		{
			IconPtr *mem = (IconPtr *) typePtr->rle_data;
			for (i = 0; i < typePtr->icon_count; i++)
				if (mem[i])
					Tcl_FreeDebug(mem[i]);
			if (typePtr->dynamic_spec)
				Tcl_FreeDebug(typePtr->dynamic_spec);
		}
		else
			Tcl_FreeDebug(typePtr->rle_offset);
		Tcl_FreeDebug(typePtr->rle_bounds);
		Tcl_FreeDebug(typePtr->rle_data);
		Tcl_FreeDebug(typePtr->rle_len);
	}

	Tcl_FreeDebug((char *) typePtr->flags);

	if (typePtr->gamma[0])
		Tcl_FreeDebug((char *) typePtr->gamma[0]);
	if (typePtr->gamma[1])
		Tcl_FreeDebug((char *) typePtr->gamma[1]);

	if (typePtr->dark_data)
	{
		for (i = 0; i < typePtr->icon_count; i++)
			if (typePtr->dark_data[i])
				Tcl_FreeDebug((char *) typePtr->dark_data[i]);
		Tcl_FreeDebug(typePtr->dark_data);
	}

	hPtr = Tcl_FindHashEntry(&g_icon_table, typePtr->desc);
	Tcl_DeleteHashEntry(hPtr);

	/* Should remove from g_icon_type[] array but would have to fix all
	 * assigments using this type and those that moved in the array */
}

int Icon_Setup(Tcl_Interp *interp, int width, int height, int depth, int style)
{
	int i, paletteIndex;

	/* Only initialize once */
	if (g_icon_width != 0)
	{
		StaticResult(interp, "icon environment already initialized");
		return TCL_ERROR;
	}

	/* Require a sane icon size */
	if ((width < 10) || (width > 48) || (height < 10) || (height > 48))
	{
		FormatResult(interp, "invalid icon size \"%d x %d\"", width, height);
		return TCL_ERROR;
	}

	/* Require a known icon depth */
	if ((depth != 8) && (depth != 16) && (depth != 24))
	{
		FormatResult(interp, "invalid icon depth \"%d\": must be 8, 16 or 24",
			depth);
		return TCL_ERROR;
	}

	/* Remember the requested icon size */
	g_icon_width = width;
	g_icon_height = height;

	/* Remember the requested icon depth */
	g_icon_depth = depth;

	/* Hacks-a-plenty */
	g_icon_style = style;

	if (InitPixelSize(interp) != TCL_OK)
		return TCL_ERROR;

	/* Remember number of pixels in an icon */
	g_icon_pixels = width * height;

	/* Remember the number of bytes in an icon */
	g_icon_length = g_icon_pixels * g_pixel_size;

	if (g_icon_depth == 16)
		InitRGBInfo(interp);

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

	/*
	 * This is an array of t_icon_type types, which specify
	 * the icon data and optional mask data for each type of
	 * icon. Icon types are defined through the "icon createtype"
	 * command.
	 */
	g_icon_type = Array_New(0, sizeof(t_icon_type));
	g_icon_type_count = 0;

	/* Array of ascii configurations */
	g_ascii = Array_New(0, sizeof(t_ascii));
	g_ascii_count = 0;

	return TCL_OK;
}

void Icon_Unload(Tcl_Interp *interp)
{
	int i;

	if (g_icon_width == 0) return;

	/* Check each icon type */
	for (i = 0; i < g_icon_type_count; i++)
		Icon_FreeType(&g_icon_type[i]);

	Tcl_FreeDebug(g_icon_type);
	g_icon_type = NULL;

	Tcl_FreeDebug(g_ascii);
	g_ascii = NULL;

	g_icon_width = 0;
	g_icon_height = 0;
	g_icon_depth = 0;
}

/*
 * Initialization.
 */
int Icon_Init(Tcl_Interp *interp)
{
	/* Why is this here? */
#ifdef USE_TCL_STUBS
	if (Tcl_InitStubs(interp, "8.4", 0) == NULL)
		return TCL_ERROR;

	if (Tk_InitStubs(interp, "8.4", 0) == NULL)
		return TCL_ERROR;
#endif /* USE_TCL_STUBS */

	/***** RETURN ERROR IF PALETTE NOT INITIALIZED *****/
	g_palette_rgb = Palette_GetRGB();

#ifdef USE_COMPRESS
	if (init_compress(interp) != TCL_OK)
		return TCL_ERROR;
#endif /* USE_COMPRESS */

	/*
	 * This hash table maps symbolic names of icon types (as defined
	 * through the "icon createtype" command) to indexes into
	 * the g_icon_type[] array above.
	 */
	Tcl_InitHashTable(&g_icon_table, TCL_STRING_KEYS);

	Tcl_CreateObjCommand(interp, "icon", objcmd_icon, NULL, NULL);

	return TCL_OK;
}

void Icon_Exit(Tcl_Interp *interp)
{
	Icon_Unload(interp);

#ifdef USE_COMPRESS
	free_compress();
#endif /* USE_COMPRESS */
}
