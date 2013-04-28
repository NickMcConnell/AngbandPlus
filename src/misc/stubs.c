/* File: stubs.c */

/* Purpose: stubs stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <memory.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stubs.h"

static char *s_error = NULL;

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

int Stub_Load(ShLibRef shLib, t_stub *stub, void *data)
{
	void *p;
	void *proc;

	while (stub->name != NULL)
	{
		proc = ShLib_Address(shLib, stub->name);
		if (proc == NULL)
		{
			char name[128];
			(void) sprintf(name, "_%s", stub->name);
			proc = ShLib_Address(shLib, name);
		}
		if (proc == NULL)
		{
			SetError("can't find symbol \"%s\" in shared library",
				stub->name);
			return -1;
		}
		p = (unsigned char *) data + stub->offset;
		*(unsigned char **) p = proc;

		stub++;
	}
	return 0;
}

char *Stub_Error(void)
{
	if (!s_error)
		return "no error";
	return s_error;
}
