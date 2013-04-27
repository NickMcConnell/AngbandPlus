/* melee.h */

/*
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#ifndef MELEE_H
#define MELEE_H 1

/*
 * New monster blow methods
 */
#define RBM_HIT		1
#define RBM_TOUCH	2
#define RBM_PUNCH	3
#define RBM_KICK	4
#define RBM_CLAW	5
#define RBM_BITE	6
#define RBM_STING	7
#define RBM_BUTT	9
#define RBM_CRUSH	10
#define RBM_ENGULF	11
#define RBM_CRAWL	13
#define RBM_DROOL	14
#define RBM_SPIT	15
#define RBM_GAZE	17
#define RBM_WAIL	18
#define RBM_SPORE	19
#define RBM_BEG		21
#define RBM_INSULT	22
#define RBM_MOAN	23

/*
 * ZaiBand specific monster blow types
 * We assume that V will add types in increasing order.  To minimize chance of 
 * collision with V, we add types in decreasing order.
 */

/* The augmented gazes */
#define RBM_PRESBYOPIC_GAZE	255		/* range: 2-4, LOS */
#define RBM_HYPEROPIC_GAZE	254 	/* range: 4-MAX_RANGE, LOS */
#define RBM_RANGED_GAZE 253			/* range: 1-MAX_RANGE, LOS */
#define RBM_CLAIRVOYANT_GAZE 252	/* range: 1-MAX_RANGE, ignore LOS */

/*
 * New monster blow effects
 */
#define RBE_HURT		1
#define RBE_POISON		2
#define RBE_UN_BONUS	3
#define RBE_UN_POWER	4
#define RBE_EAT_GOLD	5
#define RBE_EAT_ITEM	6
#define RBE_EAT_FOOD	7
#define RBE_EAT_LITE	8
#define RBE_ACID		9
#define RBE_ELEC		10
#define RBE_FIRE		11
#define RBE_COLD		12
#define RBE_BLIND		13
#define RBE_CONFUSE		14
#define RBE_TERRIFY		15
#define RBE_PARALYZE	16
#define RBE_LOSE_STR	17
#define RBE_LOSE_INT	18
#define RBE_LOSE_WIS	19
#define RBE_LOSE_DEX	20
#define RBE_LOSE_CON	21
#define RBE_LOSE_CHR	22
#define RBE_LOSE_ALL	23
#define RBE_SHATTER		24
#define RBE_EXP_10		25
#define RBE_EXP_20		26
#define RBE_EXP_40		27
#define RBE_EXP_80		28
#define RBE_HALLU		29

#endif
