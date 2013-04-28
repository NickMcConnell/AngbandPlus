/* File: map-dll.h */

/* Purpose: micro-map backend for Widget */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_MAPDLL_H_
#define _INCLUDE_MAPDLL_H_

#if !defined(PLATFORM_MAC) && !defined(PLATFORM_WIN) && !defined(PLATFORM_X11)
#error "you must define one of PLATFORM_MAC, PLATFORM_WIN or PLATFORM_X11"
#endif /* */

#include "storage.h"

#define USE_MAP_DATA 1
#define USE_MAP_MIMIC 1

typedef struct t_symbol t_symbol;

struct t_symbol
{
	char *name;
	int inner;
	int outer;
	int light;
	IconPtr bits[5];
	IconValue bits4[16 * 4]; /* 4 is max pixel size */
	IconValue bits5[25 * 4];
	IconValue bits6[36 * 4];
	IconValue bits7[49 * 4];
	IconValue bits8[64 * 4];
#if USE_MAP_MIMIC
	int mimic; /* Get info from another symbol */
#endif
};

DLL_EXTVAR t_symbol **g_symbol;
DLL_EXTVAR int g_symbol_count;

#if defined(__LCC__) && !defined(BUILD_icon_dll)
extern IconPtr *_imp__g_bits[5];
#define g_bits (*_imp__g_bits)
extern int *_imp__g_bits_count[5];
#define g_bits_count (*_imp__g_bits_count)
#else
DLL_EXTVAR IconPtr *g_bits[5];
DLL_EXTVAR int g_bits_count[5];
#endif

#if defined(__LCC__) && !defined(BUILD_icon_dll)
extern Tcl_HashTable *_imp__g_symbol_hash;
#define g_symbol_hash (*_imp__g_symbol_hash)
#else
DLL_EXTVAR Tcl_HashTable g_symbol_hash;
#endif

typedef void (*DrawSymbolProc)(long *srcPtr, long *dstPtr, long pitch);
#if defined(__LCC__) && !defined(BUILD_icon_dll)
extern DrawSymbolProc (*_imp__symbolProcTable)[4][5];
#define symbolProcTable (*_imp__symbolProcTable)
#else
DLL_EXTVAR DrawSymbolProc symbolProcTable[4][5];
#endif

DLL_EXTERN int Map_Init(Tcl_Interp *interp);
DLL_EXTERN void Map_Exit(Tcl_Interp *interp);
DLL_EXTERN void DrawMapSymbol(Widget *widgetPtr, int y, int x, int symbol);
DLL_EXTERN int symbol_find(Tcl_Interp *interp, Tcl_Obj *objName, char *strName,
	t_symbol **symbolPtrPtr, int *symbolIndex);

#endif /* _INCLUDE_MAPDLL_H_ */
