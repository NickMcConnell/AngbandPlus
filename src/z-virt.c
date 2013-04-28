/* File: z-virt.c */

/*
 * Copyright (c) 2007 Ben Harrison
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

/* Purpose: Memory management routines -BEN- */

#include "z-virt.h"

#include "z-util.h"


/*
 * Optional auxiliary "rpanic" function
 */
void* (*rpanic_aux)(size_t) = NULL;

/*
 * The system is out of memory, so panic.  If "rpanic_aux" is set,
 * it can be used to free up some memory and do a new "ralloc()",
 * or if not, it can be used to save things, clean up, and exit.
 * By default, this function simply quits the program.
 */
void* rpanic(size_t len)
{
	/* Hopefully, we have a real "panic" function */
	if (rpanic_aux) return ((*rpanic_aux)(len));

	/* Attempt to quit before icky things happen */
	quit("Out of Memory!");

	/* Paranoia */
	return (NULL);
}


/*
 * Optional auxiliary "ralloc" function
 */
void* (*ralloc_aux)(size_t) = NULL;


/*
 * Allocate some memory
 */
void* ralloc(size_t len)
{
	void *mem;

	/* Allow allocation of "zero bytes" */
	if (len == 0) return (NULL);

	/* Use the aux function if set */
	if (ralloc_aux) mem = (*ralloc_aux)(len);

	/* Use malloc() to allocate some memory */
	else mem = malloc(len);

	/* We were able to acquire memory */
	if (!mem) mem = rpanic(len);

	/* Return the memory, if any */
	return (mem);
}


/*
 * Optional auxiliary "rnfree" function
 */
void* (*rnfree_aux)(void*) = NULL;


/*
 * Free some memory (allocated by ralloc), return NULL
 */
void* rnfree(void *p)
{
	/* Easy to free nothing */
	if (!p) return (NULL);

	/* Use the "aux" function */
	if (rnfree_aux) return ((*rnfree_aux)(p));

	/* Use "free" */
	free(p);

	/* Done */
	return (NULL);
}


/*
 * Allocate a constant string, containing the same thing as 'str'
 */
cptr string_make(cptr str)
{
	char *res;

	/* Simple sillyness */
	if (!str) return (str);

	/* Allocate space for the string including terminator */
	res = (char*) ralloc(strlen(str) + 1);

	/* Copy the string (with terminator) */
	strcpy(res, str);

	/* Return the allocated and initialized string */
	return (res);
}


/*
 * Un-allocate a string allocated above.
 */
errr string_free(cptr str)
{
	/* Succeed on non-strings */
	if (!str) return (0);

	/* Kill the buffer of chars we must have allocated above */
	(void)rnfree((void*)str);

	/* Success */
	return (0);
}
