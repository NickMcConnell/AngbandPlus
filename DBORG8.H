/* File: borg8.h */

/* Purpose: Header file for "borg8.c" -BEN- */

#ifndef INCLUDED_BORG8_H
#define INCLUDED_BORG8_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg8.c".
 */

#include "dborg1.h"
#include "dborg2.h"
#include "dborg3.h"
#include "dborg6.h"


/*
 * Think about the stores
 */
extern bool borg_think_store(void);

extern bool borg_caution_phase(int, int);
extern bool borg_lite_beam(bool simulation);
/*
 * Think about the dungeon
 */
extern bool borg_think_dungeon(void);


/*
 * Initialize this file
 */
extern void borg_init_8(void);


#endif

#endif

