/* File: storage.h */

/* Purpose: DLL import/export declarations */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef INCLUDED_STORAGE_H
#define INCLUDED_STORAGE_H

#ifndef DLLEXPORT
#ifdef PLATFORM_WIN
#define DLLEXPORT __declspec(dllexport)
#define DLLIMPORT __declspec(dllimport)
#else /* not PLATFORM_WIN */
#define DLLEXPORT
#define DLLIMPORT
#endif /* not PLATFORM_WIN */
#endif /* not DLLEXPORT */

#ifdef BUILD_icon_dll
#define DLL_STORAGE DLLEXPORT
#else
#define DLL_STORAGE DLLIMPORT
#endif

/*
 * LCC doesn't handle "extern declspec" for functions, they must be
 * declspec only. Variables must be also declared dllexport
 * where they are declared (i.e. "DLLEXPORT int foo;") with LCC.
 */

#ifdef __LCC__
#define DLL_EXTERN DLL_STORAGE
#define DLL_EXTVAR extern DLL_STORAGE
#else
#define DLL_EXTERN extern DLL_STORAGE
#define DLL_EXTVAR extern DLL_STORAGE
#endif

#endif /* INCLUDED_STORAGE_H */
