
/* File: zborg4.h */

/* Purpose: Header file for "borg4.c" -BEN- */

#ifndef INCLUDED_BORG4_H
#define INCLUDED_BORG4_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg4.c".
 */

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"

/* Object list interface */
extern void borg_list_info(byte list_type, vptr dummy);

/* Treat items differently depending on flags */
#define TREAT_AS_NORM	0		/* Normal item */
#define TREAT_AS_GONE	1		/* Pretend item doesn't exist */
#define TREAT_AS_LESS	2		/* Pretend one less item */
#define TREAT_AS_MORE	3		/* Pretend one more item */
#define TREAT_AS_SWAP	4		/* Pretend other item is here */
#define TREAT_AS_SHOP	5		/* Some trickery needed for shops */

/* The current home */
extern int home_shop;

extern list_item *borg_home;	/* Current home items - (only remember one.) */
extern int home_num;	/* Number of items in the home */


/* Use current shop in power calculation */
extern int use_shop;

/* Borg functions */
extern list_item *look_up_equip_slot(int slot);
extern bool borg_test_bad_curse(list_item *l_ptr);
extern void borg_update_frame(void);
extern void borg_notice(void);
extern void borg_notice_home(void);
extern s32b borg_power_home(void);
extern int  borg_notice_enchant_ac(void);
extern int  borg_notice_enchant_hit(bool *inven);
extern int  borg_notice_enchant_dam(bool *inven);
extern int  borg_notice_create_artifact(bool *b_inven);


/*
 * Macro for the borg_power functions:  This way the borg can count how far (a)
 * is in a range from (b) to (c)
 * if c <= b return 0		is b to c a range?
 * if a <= b return 0
 * if a <= c return a - b
 * if a > c return c - b
 */
#define MIN_FLOOR(a,b,c)	(((b) < (c)) ? (MIN(MAX((a), (b)), (c)) - (b)) : 0)

/*
 * Initialize this file
 */
extern void borg_init_4(void);


#endif

#endif
