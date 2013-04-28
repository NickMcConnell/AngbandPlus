/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* posband.h: main PosBand include file  */

#ifndef POSBAND_H_INCLUDED
#define POSBAND_H_INCLUDED

/***** Some older copyright messages follow below *****/

/*
 * Note that these copyright messages apply to an ancient version
 * of Angband, as in, from pre-2.4.frog-knows days, and thus the
 * references to version numbers may be rather misleading...
 */

/*
 * UNIX ANGBAND Version 5.0
 */

/* Original copyright message follows. */

/*
 * ANGBAND Version 4.8	COPYRIGHT (c) Robert Alan Koeneke
 *
 *	 I lovingly dedicate this game to hackers and adventurers
 *	 everywhere...
 *
 *	 Designer and Programmer:
 *		Robert Alan Koeneke
 *		University of Oklahoma
 *
 *	 Assistant Programmer:
 *		Jimmey Wayne Todd
 *		University of Oklahoma
 *
 *	 Assistant Programmer:
 *		Gary D. McAdoo
 *		University of Oklahoma
 *
 *	 UNIX Port:
 *		James E. Wilson
 *		UC Berkeley
 *		wilson@ernie.Berkeley.EDU
 *		ucbvax!ucbernie!wilson
 */

/*
 *	 ANGBAND may be copied and modified freely as long as the above
 *	 credits are retained.	No one who-so-ever may sell or market
 *	 this software in any form without the expressed written consent
 *	 of the author Robert Alan Koeneke.
 */

/**** Low-level functions ****/

#include "system.h"

/**** Config ****/

#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif /* HAVE_CONFIG_H */

/**** Include service Angband functions ****/

#include "lib.h"

/**** Version ****/

/* Variant and version */
#define VERSION_NAME	PACKAGE_NAME
#define VERSION_STRING	PACKAGE_VERSION

/* Savefile version */
#define VERSION_MAJOR	4
#define VERSION_MINOR	1
#define VERSION_PATCH	0
#define VERSION_EXTRA	2

/* Oldest savefile version number that can still be imported */
#define OLD_VERSION_MAJOR	4
#define OLD_VERSION_MINOR	1
#define OLD_VERSION_PATCH	0

/* Version of random artifact code -- meaningless now */
#define RANDART_VERSION	70

/**** Includes ****/

#include "misc.h"
#include "options.h"
#include "cave.h"
#include "object.h"
#include "monster.h"
#include "quest.h"
#include "store.h"
#include "spells.h"
#include "player.h"

#endif /* POSBAND_H_INCLUDED */
