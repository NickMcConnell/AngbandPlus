/* File z-form.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * Ben Harrison released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
 */

#ifndef INCLUDED_Z_FORM_H
#define INCLUDED_Z_FORM_H

#include "h-basic.h"

/*
 * This file provides functions very similar to "sprintf()", but which
 * not only parse some additional "format sequences", but also enforce
 * bounds checking, and allow repeated "appends" to the same buffer.
 *
 * See "z-form.c" for more detailed information about the routines,
 * including a list of the legal "format sequences".
 *
 * This file makes use of both "z-util.c" and "z-virt.c"
 */


/**** Available Functions ****/

/* Format arguments into given bounded-length buffer */
extern size_t vstrnfmt(char *buf, size_t max, cptr fmt, va_list vp);

/* Simple interface to "vstrnfmt()" */
extern size_t strnfmt(char *buf, size_t max, cptr fmt, ...);

/* Format arguments into a static resizing buffer */
extern char *vformat(cptr fmt, va_list vp);

/* Free the memory allocated for the format buffer */
extern void vformat_kill(void);

/* Append a formatted string to another string */
extern void strnfcat(char *str, size_t max, size_t *end, cptr fmt, ...);

/* Simple interface to "vformat()" */
extern char *format(cptr fmt, ...);

/* Vararg interface to "plog()", using "format()" */
extern void plog_fmt(cptr fmt, ...);

/* Vararg interface to "quit()", using "format()" */
extern void quit_fmt(cptr fmt, ...);

/* Vararg interface to "core()", using "format()" */
extern void core_fmt(cptr fmt, ...);


#endif


