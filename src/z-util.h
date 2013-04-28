/* File z-util.h */

/*
 * Copyright (c) 2007 Ben Harrison
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
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


/**** Available Functions ****/

/* Case insensitive comparison between two strings */
extern int my_stricmp(const char *s1, const char *s2);
extern int my_strnicmp(cptr a, cptr b, int n);

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


/*
 * Hack -- conditional (or "bizarre") externs
 */

#ifdef SET_UID
# ifndef HAVE_USLEEP
/* util.c */
extern int usleep(unsigned long usecs);
# endif /* HAVE_USLEEP */
extern void user_name(char *buf, size_t len, int id);
#endif /* SET_UID */


#endif /* INCLUDED_Z_UTIL_H */


