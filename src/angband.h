/* File: angband.h */
/*
 * Copyright (c) 1989 James E. Wilson
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


/* Main "Angband" header file */
#ifndef INCLUDED_ANGBAND_H
#define INCLUDED_ANGBAND_H

/*
 * First, include the low-level includes.  Be sure to edit "h-config.h"
 * to reflect any hardware, operating system, or compiler nuances.
 */
#include "h-basic.h"

/*
 * Then, include the header files for the low-level code
 */
#include "rect.h"
#include "z-util.h"
#include "z-virt.h"
#include "z-form.h"
#include "z-rand.h"
#include "z-term.h"
#include "z-doc.h"

/*
 * Include the "Angband" configuration header
 */
#include "z-config.h"

#include "savefile.h"

/*
 * Now, include the define's, the type's, and the extern's
 */
#include "defines.h"
#include "variant.h"
#include "resist.h"

#include "obj.h"
#include "inv.h"
#include "equip.h"
#include "pack.h"
#include "quiver.h"
#include "home.h"
#include "rooms.h"
#include "shop.h"
#include "mon.h"
#include "obj_prompt.h"
#include "py_throw.h"
#include "quest.h"
#include "gf.h"
#include "monspell.h"

#include "types.h"
#include "externs.h"

#include "message.h"
#include "mut.h"
#include "spells.h"
#include "menu.h"
#include "fear.h"
#include "scores.h"

/***** Some copyright messages follow below *****/

/*
 * Note that these copyright messages apply to an ancient version
 * of Angband, as in, from pre-2.4.frog-knows days, and thus the
 * reference to "5.0" is rather misleading...
 */

/*
 * UNIX ANGBAND Version 5.0
 */


/* Original copyright message follows. */

/*
 * ANGBAND Version 4.8    COPYRIGHT (c) Robert Alan Koeneke
 *
 *     I lovingly dedicate this game to hackers and adventurers
 *     everywhere...
 *
 *     Designer and Programmer:
 *        Robert Alan Koeneke
 *        University of Oklahoma
 *
 *     Assistant Programmer:
 *        Jimmey Wayne Todd
 *        University of Oklahoma
 *
 *     Assistant Programmer:
 *        Gary D. McAdoo
 *        University of Oklahoma
 *
 *     UNIX Port:
 *        James E. Wilson
 *        UC Berkeley
 *        wilson@ernie.Berkeley.EDU
 *        ucbvax!ucbernie!wilson
 */


/*
 *     ANGBAND may be copied and modified freely as long as the above
 *     credits are retained.    No one who-so-ever may sell or market
 *     this software in any form without the expressed written consent
 *     of the author Robert Alan Koeneke.
 */


#endif



