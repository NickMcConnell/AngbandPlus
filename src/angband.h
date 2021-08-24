/* File: angband.h */
/*
 * Copyright (c) 1989 James E. Wilson
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */
/*
  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
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
#include "z-sym.h"

/*
 * Include the "Angband" configuration header
 */
#include "z-config.h"

#include "savefile.h"

/*
 * Now, include the define's, the type's, and the extern's
 */
#include "defines.h"
#include "var.h"

#include "poschengband.h"
#include "obj.h"
#include "art.h"
#include "inv.h"
#include "mon_race.h"
#include "mon.h"
#include "plr.h"
#include "equip.h"
#include "pack.h"
#include "quiver.h"
#include "home.h"
#include "rooms.h"
#include "quest.h"
#include "shop.h"
#include "dun.h"
#include "dun_cell.h"
#include "dun_util.h"
#include "dun_project.h"
#include "obj_prompt.h"
#include "plr_throw.h"
#include "monk_attack.h"
#include "plr_attack.h"
#include "plr_shoot.h"
#include "plr_tim.h"
#include "gf.h"
#include "mon_spell.h"
#include "mon_tim.h"
#include "mon_attack.h"
#include "mon_ai.h"
#include "resist.h"

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



