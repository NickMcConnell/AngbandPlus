/* File: zborg7.h */

/* Purpose: Header file for "zborg7.c" -BEN- */

#ifndef INCLUDED_BORG7_H
#define INCLUDED_BORG7_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "zborg7.c".
 */

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"


/*
 * Various functions
 */
extern bool borg_use_things(void);
extern bool borg_check_lite(void);

extern bool borg_enchanting(void);
extern bool borg_recharging(void);
extern bool borg_destroy(void);
extern bool borg_id_meta(void);
extern bool borg_wear_stuff(void);
extern bool borg_unwear_stuff(void);
extern bool borg_play_magic(bool bored);
extern bool borg_wait_recharge(void);

/*
 * Attempt to leave the level
 */
extern bool borg_leave_level(bool bored);


/*
 * Initialize this file
 */
extern void borg_init_7(void);


#endif

#endif
