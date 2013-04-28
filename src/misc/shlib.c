/* File: shlib.c */

/* Purpose: shared library support */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "shlib.h"

static char *s_error = 0;

static void SetError(char *fmt, ...)
{
	va_list args;
	char buf[256];

	if (s_error)
		free(s_error);

	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);

	s_error = malloc(strlen(buf) + 1);
	(void) strcpy(s_error, buf);
}

char *ShLib_Error(void)
{
	if (!s_error)
		return "no error";
	return s_error;
}

#ifdef __WIN32__ /*PLATFORM_WIN*/

#include <windows.h>

ShLibRef ShLib_Load(const char *f)
{
	return (ShLibRef) LoadLibrary(f);
}

void ShLib_Free(ShLibRef shLib)
{
	FreeLibrary(shLib);
}

void *ShLib_Address(ShLibRef shLib, const char *t)
{
	return GetProcAddress(shLib, t);
}

#undef PLATFORM_X11 /* PLATFORM_SDL */

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

#include <dlfcn.h>

ShLibRef ShLib_Load(const char *f)
{
	void *shlib = dlopen(f, RTLD_NOW | RTLD_GLOBAL);
	if (!shlib)
		SetError(dlerror());
	return (ShLibRef) shlib;
}

void ShLib_Free(ShLibRef shLib)
{
	if (dlclose(shLib))
		SetError(dlerror());
}

void *ShLib_Address(ShLibRef shLib, const char *t)
{
	void *sym = dlsym(shLib, t);
	if (!sym)
		SetError(dlerror());
	return sym;
}

#endif /* PLATFORM_X11 */

