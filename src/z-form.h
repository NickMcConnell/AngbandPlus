/* File z-form.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
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


/*
 * The "type" of the "user defined print routine" function pointers
 */
typedef void (*vstrnfmt_aux_func) (char *buf, uint max, cptr fmt, va_list *vp);


/**** Available Functions ****/

/* Register table of user format functions */
extern void register_format_funcs(vstrnfmt_aux_func *table);

/* Format arguments into given bounded-length buffer */
extern uint vstrnfmt(char *buf, uint max, cptr fmt, va_list *vp);

/* Simple interface to "vstrnfmt()" */
extern uint strnfmt(char *buf, uint max, cptr fmt, ...);

/* Append a formatted string to another string */
extern void strnfcat(char *str, int max, int *end, cptr fmt, ...);

/* Free the memory allocated for the format buffer */
extern void vformat_kill(void);

/* Simple interface to "vformat()" */
extern char *format(cptr fmt, ...);

/* Vararg interface to "plog()", using "format()" */
extern void plog_fmt(cptr fmt, ...);

/* Vararg interface to "quit()", using "format()" */
extern void quit_fmt(cptr fmt, ...);

/* Vararg interface to "core()", using "format()" */
extern void core_fmt(cptr fmt, ...);


#endif
