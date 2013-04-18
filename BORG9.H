/* File: borg9.h */
/* Purpose: Header file for "borg9.c" -BEN- */

#ifndef INCLUDED_BORG9_H
#define INCLUDED_BORG9_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg9.c".
 */

/* Print the map upon death */
extern void borg_write_map(bool ask);

extern void borg_resurrect(void);

/*
 * Initialize this file
 */
extern void borg_init_9(void);

#endif

#endif

