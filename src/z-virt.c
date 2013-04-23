#define Z_VIRT_C
/* File: z-virt.c */

/* Purpose: Memory management routines -BEN- */

#include "z-virt.h"
#include "externs.h"

/*
 * Allow debugging messages to track memory usage.
 */

/*
 * Optional auxiliary "rnfree" function
 */
vptr (*rnfree_aux)(vptr) = NULL;

/*
 * Free some memory (allocated by ralloc), return NULL
 */
vptr rnfree(vptr p)
{
	/* Easy to free nothing */
	if (!p) return (NULL);

	/* Use the "aux" function */
	if (rnfree_aux) return ((*rnfree_aux)(p));

	/* Use "free" */
	free((char*)(p));

	/* Done */
	return (NULL);
}

/*
 * Hack - a fake rpanic_aux hook for when "out of memory" errors are handled
 * by the caller.
 */
vptr rpanic_none(huge UNUSED len)
{
	return NULL;
}


/*
 * Optional auxiliary "rpanic" function
 */
vptr (*rpanic_aux)(huge) = NULL;

/*
 * The system is out of memory, so panic.  If "rpanic_aux" is set,
 * it can be used to free up some memory and do a new "ralloc()",
 * or if not, it can be used to save things, clean up, and exit.
 * By default, this function simply crashes the computer.
 */
static vptr rpanic(huge len)
{
	/* Hopefully, we have a real "panic" function */
	if (rpanic_aux) return ((*rpanic_aux)(len));

	/* Attempt to crash before icky things happen */
	core("Out of Memory!");

	/* Paranoia */
	return ((vptr)(NULL));
}


/*
 * Optional auxiliary "ralloc" function
 */
vptr (*ralloc_aux)(huge) = NULL;


/*
 * Allocate some memory
 */
vptr ralloc(huge len)
{
	vptr mem;

	/* Allow allocation of "zero bytes" */
	if (len == 0) return ((vptr)(NULL));

	/* Use the aux function if set */
	if (ralloc_aux) mem = (*ralloc_aux)(len);

	/* Use malloc() to allocate some memory */
	else mem = ((vptr)(malloc((size_t)(len))));

	/* We were able to acquire memory */
	if (!mem) mem = rpanic(len);

	/* Return the memory, if any */
	return (mem);
}




/*
 * Allocate a string containing the same thing as 'str'
 */
char *string_make(cptr str)
{
	huge len = 0;
	cptr t = str;
	char *s, *res;

	/* Simple sillyness */
	if (!str) return NULL;

	/* Get the number of chars in the string, including terminator */
	while (str[len++]) /* loop */;

	/* Allocate space for the string */
	s = res = (char*)(ralloc(len));

	/* Copy the string (with terminator) */
	while ((*s++ = *t++) != 0) /* loop */;

	/* Return the allocated, initialized, string */
	return (res);
}

/*
 * Return a pointer in "ap" which is the same as p.
 * If p is NULL, ap can be enlarged as necessary to create such a pointer.
 * If a pointer is found, return a pointer to it. Otherwise, return 0.
 */
static vptr *get_alloc_pointer(vptr p)
{
	static vptr *ap = NULL;
	static size_t num_ap = 0;
	vptr *pp;

	/* Search the existing array. */
	if (num_ap)
	{
		for (pp = ap; pp < ap+num_ap; pp++)
		{
			if (*pp == p) return pp;
		}
	}

	/* Grow the array (start at 128 and double as needed) to find a NULL. */
	if (!p)
	{
		vptr *new_ap;
		size_t UNREAD(new_num), max_num = 65535U / sizeof(vptr);

		/* Paranoia - 65535 should be more than enough. */
		if (num_ap == max_num) quit("Out of safe malloc space.");
		else if (max_num < 2 * num_ap) new_num = max_num;
		else if (num_ap) new_num = num_ap * 2;
		else new_num = 128;

		/* Grow the array. */
		C_MAKE(new_ap, new_num, vptr);
		if (num_ap) C_COPY(new_ap, ap, num_ap, vptr);

		/* Set ap and num_ap to the new values, and new_ap to the first newly
		 * allocated pointer.
		 */
		ap = new_ap;
		new_ap += num_ap;
		num_ap = new_num;

		/* Return the pointer. */
		return new_ap;
	}
	/* Reject any other request for an absent pointer. */
	else
	{
		return NULL;
	}
}

/*
 * Allocate some memory and remember the pointer used.
 */
static vptr safe_malloc(huge len)
{
	vptr *np = get_alloc_pointer(NULL);

	*np = ralloc(len);

	return *np;
}

/*
 * Free a pointer if it was set via safe_malloc() above.
 * Otherwise, do nothing.
 *
 * This takes a pointer to the pointer as input, as it sets it to 0 itself.
 */
void safe_free(vptr p)
{
	/* Look for the pointer. */
	vptr *np = get_alloc_pointer(p);

	/* Found it, so free it and set to 0. */
	if (np) KILL(*np);
}

/*
 * Allocate a constant string with safe_malloc, containing the same thing as
 * 'str'
 */
cptr safe_string_make(cptr str)
{
	huge len = 0;
	cptr t = str;
	char *s, *res;

	/* Simple sillyness */
	if (!str) return (str);

	/* Get the number of chars in the string, including terminator */
	while (str[len++]) /* loop */;

	/* Allocate space for the string */
	s = res = (char*)(safe_malloc(len));

	/* Copy the string (with terminator) */
	while ((*s++ = *t++) != 0) /* loop */;

	/* Return the allocated, initialized, string */
	return (res);
}

