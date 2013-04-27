/* Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

stripped by Kenneth Boyd for Zaiband; file is GPL 2.1-licensed.

Copy this file to the main src directory if your system doesn't have a clue what stdbool.h is.
*/

/*
 * ISO C Standard:  7.16  Boolean type and values  <stdbool.h>
 */

#ifndef STDBOOL_H
#define STDBOOL_H

#ifndef __cplusplus

/* This is non-compliant (correct expansion is _Bool), but if you're using this file you're on a pre-C99 system */
#define bool char
#define true	1
#define false	0

/* Signal that all the definitions are present.  */
#define __bool_true_false_are_defined	1

#endif /* __cplusplus */

#endif	/* stdbool.h */
