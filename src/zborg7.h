/* File: zborg7.h */

/* Purpose: Header file for "borg7.c" -BEN- */

#ifndef INCLUDED_BORG7_H
#define INCLUDED_BORG7_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg7.c".
 */

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"


/*
 * Determine if an item is "icky"
 */
extern bool borg_item_icky(list_item *l_ptr);

/*
 * Various functions
 */
extern bool borg_use_things(void);
extern bool borg_check_lite(void);

extern bool borg_enchanting(void);
extern bool borg_recharging(void);
extern bool borg_crush_junk(void);
extern bool borg_crush_hole(void);
extern bool borg_crush_slow(void);
extern bool borg_obj_star_id_able(list_item *l_ptr);
extern bool borg_test_stuff(void);
extern bool borg_test_stuff_pseudo(void);
extern bool borg_test_stuff_star(void);
extern bool borg_takeoff_stuff(void);
extern bool borg_wear_stuff(void);
extern bool borg_unwear_stuff(void);
extern bool borg_wears_cursed(bool heavy);
extern bool borg_play_magic(bool bored);
extern bool borg_remove_stuff(void);
extern bool borg_wear_recharge(void);

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
