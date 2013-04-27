/* File z-util.h */

/*
 * Copyright (c) 1997-2005 Ben Harrison, Robert Ruehlmann.
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
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
extern const char* argv0;


/* Aux functions */
extern void (*plog_aux)(const char*);
extern void (*quit_aux)(const char*);
extern void (*core_aux)(const char*);


/**** Available Functions ****/

/* Case insensitive comparison between two strings */
extern int my_stricmp(const char *s1, const char *s2);
extern int my_strnicmp(const char* a, const char* b, int n);

/* Copy a string */
extern size_t my_strcpy(char *buf, const char *src, size_t bufsize);

/* Concatenate two strings */
extern size_t my_strcat(char *buf, const char *src, size_t bufsize);

/* Test equality, prefix, suffix */
inline bool streq(const char* s, const char* t) { return !strcmp(s, t); }
extern bool prefix(const char* s, const char* t);
extern bool suffix(const char* s, const char* t);


/* Print an error message */
extern void plog(const char* str);

/* Exit, with optional message */
extern void quit(const char* str);

/* Dump core, with optional message */
extern void core(const char* str);



#endif


