/* File z-util.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#ifndef INCLUDED_Z_UTIL_H
#define INCLUDED_Z_UTIL_H

#include "h-basic.h"


/*
 * Extremely basic stuff, like global temp and constant variables.
 * Also, some very useful low level functions, such as "streq()".
 * All variables and functions in this file are "addressable".
 */


/**** Available variables ****/

/* A cptr to the name of the program */
extern cptr argv0;


/* Aux functions */
extern void (*plog_aux)(cptr);
extern void (*quit_aux)(cptr);
extern void (*core_aux)(cptr);


/**** Available Functions ****/

/* Case insensitive comparison between two strings */
extern int my_stricmp(const char *s1, const char *s2);

/* Copy a string */
extern size_t my_strcpy(char *buf, const char *src, size_t bufsize);

/* Concatenate two strings */
extern size_t my_strcat(char *buf, const char *src, size_t bufsize);

/* Test equality, prefix, suffix */
extern bool streq(cptr s, cptr t);
extern bool prefix(cptr s, cptr t);
extern bool suffix(cptr s, cptr t);


/* Print an error message */
extern void plog(cptr str);

/* Exit, with optional message */
extern void quit(cptr str);

/* Dump core, with optional message */
extern void core(cptr str);


/* Alex: the following macros was in object1.c */

/*
 * Efficient version of '(T) += sprintf((T), "%c", (C))'
 */
#define object_desc_chr_macro(T,C) do { \
 \
	/* Copy the char */ \
	*(T)++ = (C); \
 \
} while (0)



/*
 * Efficient version of '(T) += sprintf((T), "%s", (S))'
 */
#define object_desc_str_macro(T,S) do { \
 \
	cptr s = (S); \
 \
	/* Copy the string */ \
	while (*s) *(T)++ = *s++; \
 \
} while (0)



/*
 * Efficient version of '(T) += sprintf((T), "%u", (N))'
 */
#define object_desc_num_macro(T,N) do { \
 \
	int n = (N); \
 \
	int p; \
 \
	/* Find "size" of "n" */ \
	for (p = 1; n >= p * 10; p = p * 10) /* loop */; \
 \
	/* Dump each digit */ \
	while (p >= 1) \
	{ \
		/* Dump the digit */ \
		*(T)++ = I2D(n / p); \
 \
		/* Remove the digit */ \
		n = n % p; \
 \
		/* Process next digit */ \
		p = p / 10; \
	} \
 \
} while (0)



/*
 * Efficient version of '(T) += sprintf((T), "%+d", (I))'
 */
#define object_desc_int_macro(T,I) do { \
 \
	int i = (I); \
 \
	/* Negative */ \
	if (i < 0) \
	{ \
		/* Take the absolute value */ \
		i = 0 - i; \
 \
		/* Use a "minus" sign */ \
		*(T)++ = '-'; \
	} \
 \
	/* Positive (or zero) */ \
	else \
	{ \
		/* Use a "plus" sign */ \
		*(T)++ = '+'; \
	} \
 \
	/* Dump the number itself */ \
	object_desc_num_macro(T, i); \
 \
} while (0)


/*Alex: positive int without sign*/
#define object_desc_s_int_macro(T,I) do { \
 \
	int i = (I); \
 \
	/* Negative */ \
	if (i < 0) \
	{ \
		/* Take the absolute value */ \
		i = 0 - i; \
 \
		/* Use a "minus" sign */ \
		*(T)++ = '-'; \
	} \
 \
	/* Dump the number itself */ \
	object_desc_num_macro(T, i); \
 \
} while (0)
#endif


