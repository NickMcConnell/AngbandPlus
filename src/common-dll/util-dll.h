/* File: util-dll.h */

/* Purpose: misc stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_UTILDLL_H_
#define _INCLUDE_UTILDLL_H_

#include <tk.h>

#if !defined(PLATFORM_MAC) && !defined(PLATFORM_WIN) && !defined(PLATFORM_X11)
#error "you must define one of PLATFORM_MAC, PLATFORM_WIN or PLATFORM_X11"
#endif /* */

#include "storage.h"

#if !defined(MIN)
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

DLL_EXTVAR int g_palette_white, g_palette_black;
#define PALETTE_WHITE g_palette_white
#define PALETTE_BLACK g_palette_black

DLL_EXTVAR int g_colormap_white, g_colormap_black;
#define COLORMAP_WHITE g_colormap_white
#define COLORMAP_BLACK g_colormap_black

DLL_EXTVAR unsigned char g_palette2colormap[256];
DLL_EXTVAR unsigned char g_colormap2palette[256];

typedef unsigned char TintValue, TintTable[256], *TintPtr;
DLL_EXTERN int Palette_Init(Tcl_Interp *interp, char *fileName);
#ifdef PLATFORM_WIN
DLL_EXTERN /* HPALETTE */ void *Palette_GetHPal(void);
#endif /* PLATFORM_WIN */
DLL_EXTERN unsigned char *Palette_GetRGB(void);
DLL_EXTERN void Palette_ResetHash(void);
DLL_EXTERN int Palette_RGB2Index(unsigned char r, unsigned char g, unsigned char b);
DLL_EXTERN void Palette_GammaTable(double gamma, TintTable table);
DLL_EXTERN void Palette_TintTable(int tint, int opacity, TintTable table);

DLL_EXTERN int Colormap_Init(Tcl_Interp *interp);
DLL_EXTERN unsigned char *Colormap_GetRGB(void);
DLL_EXTERN int Colormap_RGB2Index(unsigned char r, unsigned char g, unsigned char b);
DLL_EXTERN void Colormap_GammaTable(double gamma, TintTable table);
DLL_EXTERN void Colormap_TintTable(int tint, int opacity, TintTable table);
DLL_EXTERN void Colormap_BrightnessTable(int intensity, TintTable table);
DLL_EXTERN void Colormap_ContrastTable(int intensity, TintTable table);
DLL_EXTERN void Colormap_One2OneTable(TintTable table);

typedef struct RGBInfo RGBInfo;
struct RGBInfo
{
	int red_count, green_count, blue_count;
	int red_mask, green_mask, blue_mask;
	int red_shift, green_shift, blue_shift;
	int extra;
};
#if defined(__LCC__) && !defined(BUILD_icon_dll)
extern RGBInfo *_imp__g_rgbi;
#define g_rgbi (*_imp__g_rgbi)
#else
DLL_EXTVAR RGBInfo g_rgbi;
#endif

DLL_EXTERN void GetPix16(unsigned char *p, int *r, int *g, int *b);
DLL_EXTERN void SetPix16(unsigned char *p, int r, int g, int b);

#undef MEMORY_DEBUG
#ifdef MEMORY_DEBUG
DLL_EXTERN void db_dump_allocs(void);
DLL_EXTERN int db_heap_error(void* vp,int head);

DLL_EXTERN void *_db_malloc(size_t siz, char *file, int line);
DLL_EXTERN void *_db_realloc(void *ptr, size_t siz, char *file, int line);
DLL_EXTERN void _db_free(void *ptr, char *file, int line);
#define Tcl_AllocDebug(siz) _db_malloc(siz,__FILE__,__LINE__)
#define Tcl_ReallocDebug(str,siz) _db_realloc(str,siz,__FILE__,__LINE__)
#define Tcl_FreeDebug(str) _db_free(str,__FILE__,__LINE__)
#else /* not MEMORY_DEBUG */
#define Tcl_AllocDebug Tcl_Alloc
#define Tcl_ReallocDebug(p,siz) Tcl_Realloc((char*)(p),siz)
#define Tcl_FreeDebug(p) Tcl_Free((char*)(p))
#endif /* not MEMORY_DEBUG */

#ifdef MEMORY_DEBUG
#define Array_New(c,e) _Array_New(c,e,__FILE__,__LINE__)
DLL_EXTERN void *_Array_New(int count, int elem_size, char *file, int line);
#else
DLL_EXTERN void *Array_New(int count, int elem_size);
#endif
DLL_EXTERN void *Array_Append(void *array_ptr, int *count, int elem_size,
	void *elem_ptr);
DLL_EXTERN void *Array_Insert(void *array_ptr, int *count, int elem_size,
	int index);
DLL_EXTERN void *Array_Delete(void *array_ptr, int *count, int elem_size,
	int index);

typedef struct BitmapType {
	unsigned char *pixelPtr; /* Address of top-left pixel */
	int width; /* Width in pixels */
	int height; /* Height in pixels */
	int depth; /* 8, 16 or 24 */
	int pitch; /* Address difference between vertically adjacent pixels */
	int pixelSize; /* Address difference between horizontally adjacent pixels */
    Pixmap pixmap;
    void *platData; /* Platform-specific info */
} BitmapType, *BitmapPtr;

DLL_EXTERN void Bitmap_New(Tcl_Interp *interp, BitmapPtr bitmapPtr);
DLL_EXTERN void Bitmap_Delete(BitmapPtr bitmapPtr);

#ifdef PLATFORM_WINxxx
DLL_EXTERN HFONT TkToWinFont(Tk_Font tkFont);
#endif /* PLATFORM_WIN */

typedef struct DoubleLink DoubleLink;
typedef struct DoubleLinker DoubleLinker;

struct DoubleLink
{
	int isLinked;
	DoubleLinker *linker;
	DoubleLink *prev;
	DoubleLink *next;
	void *data;
};

struct DoubleLinker
{
	int count;
	char *what;
	DoubleLink *head;
	DoubleLink *tail;
};

#define DoubleLink_Data(link,type) ((type *) link->data)
DLL_EXTERN void DoubleLink_Init(DoubleLinker *linker, DoubleLink *link, void *data);
DLL_EXTERN void DoubleLink_Link(DoubleLink *link);
DLL_EXTERN void DoubleLink_Unlink(DoubleLink *link);
/* DLL_EXTERN void DoubleLink_Move(DoubleLink *old, DoubleLink *new); */

#define BooleanResult(interp,value) Tcl_SetObjResult(interp, Tcl_NewBooleanObj(value))
#define IntResult(interp,value) Tcl_SetObjResult(interp, Tcl_NewIntObj(value))
#define LongResult(interp,value) Tcl_SetObjResult(interp, Tcl_NewLongObj(value))
#define StaticResult(interp,str) Tcl_SetResult(interp, str, TCL_STATIC)
#define StringResult(interp,str) Tcl_SetResult(interp, str, TCL_VOLATILE)
DLL_EXTERN void FormatResult(Tcl_Interp *interp, char *fmt, ...);

DLL_EXTERN Tcl_Obj *ExtToUtf_NewStringObj(CONST char *bytes, int length);
DLL_EXTERN void ExtToUtf_SetResult(Tcl_Interp *interp, char *string);
DLL_EXTERN char *UtfToExt_TranslateFileName(Tcl_Interp *interp, char *utfPath,
	Tcl_DString *extDStringPtr);

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#endif /* _INCLUDE_UTILDLL_H_ */

