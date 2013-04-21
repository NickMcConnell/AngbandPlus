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

/* Temporary Vars */
extern char char_tmp;
extern byte byte_tmp;
extern sint sint_tmp;
extern uint uint_tmp;
extern long long_tmp;
extern huge huge_tmp;
extern errr errr_tmp;

/* Temporary Pointers */
extern cptr cptr_tmp;
extern vptr vptr_tmp;


/* Constant pointers (NULL) */
extern cptr cptr_null;
extern vptr vptr_null;


/* A bizarre vptr that always points at itself */
extern vptr vptr_self;


/* A cptr to the name of the program */
extern cptr argv0;


/* Aux functions */
extern void (*plog_aux)(cptr);
extern void (*quit_aux)(cptr);
extern void (*core_aux)(cptr);


/**** Available Functions ****/

/* Function that does nothing */
extern void func_nothing(void);

/* Functions that return basic "errr" codes */
extern errr func_success(void);
extern errr func_problem(void);
extern errr func_failure(void);

/* Functions that return bools */
extern bool func_true(void);
extern bool func_false(void);


/* Test equality, prefix, suffix */
extern bool streq(cptr s, cptr t);
extern bool prefix(cptr s, cptr t);
extern bool suffix(cptr s, cptr t);


/* Print an error message */
extern void plog(cptr str);

/* Exit, with optional message */
/* it is quite messy for CMUCL which has a function of the same name */
extern void z_quit(cptr str);

#define quit(a) z_quit(a)

/* Dump core, with optional message */
extern void core(cptr str);



#endif

