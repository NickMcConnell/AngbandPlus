/* File: zborg9.h */
/* Purpose: Header file for "borg9.c" -BEN- */

#ifndef INCLUDED_BORG9_H
#define INCLUDED_BORG9_H

#include "angband.h"

#ifdef ALLOW_BORG

#define BORG_SHOW_FEAT	1
#define BORG_SHOW_INFO	2
#define BORG_SHOW_FLAG	3
#define BORG_SHOW_FLOW	4
#define BORG_SHOW_AVOID	5
#define BORG_SHOW_STEP	6
#define BORG_SHOW_FEAR	7


/*
 * This file provides support for "borg9.c".
 */
extern void borg_status_window(void);

/*
 * Initialize this file
 */
extern void borg_init_9(void);


#endif

#endif
