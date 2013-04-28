/* File: shlib.h */

/* Purpose: shared library support */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_SHLIB_H_
#define _INCLUDE_SHLIB_H_

typedef void *ShLibRef;
extern ShLibRef ShLib_Load(const char *f);
extern void ShLib_Free(ShLibRef shLib);
extern void *ShLib_Address(ShLibRef shLib, const char *t);
extern char *ShLib_Error(void);

#endif /* _INCLUDE_SHLIB_H_ */
