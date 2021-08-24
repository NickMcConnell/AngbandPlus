/*
 * File: angband.h
 * Purpose: Main Angband header file
 */

#ifndef INCLUDED_ANGBAND_H
#define INCLUDED_ANGBAND_H

/*
 * Include the low-level includes
 */
#include "h-basic.h"
#include "h-quark.h"

#include "option.h"

/*
 * Include the mid-level includes
 */
#include "z-bitflag.h"
#include "z-color.h"
#include "z-file.h"
#include "z-form.h"
#include "z-msg.h"
#include "z-rand.h"
#include "z-set.h"
#include "z-spells.h"
#include "z-type.h"
#include "z-util.h"
#include "z-virt.h"

#include "z-expression.h"
#include "z-dice.h"

/*
 * Include the high-level includes
 */
#include "z-defines.h"

#include "buildid.h"
#include "config.h"
#include "datafile.h"
#include "guid.h"
#include "md5.h"
#include "source.h"
#include "parser.h"
#include "obj-common.h"
#include "trap-common.h"
#include "player-state.h"
#include "mon-common.h"
#include "player-common.h"
#include "display.h"
#include "obj-gear-common.h"
#include "obj-tval.h"
#include "player-common-calcs.h"
#include "randname.h"
#include "store-types.h"
#include "util.h"


/* Horrible hack -- should we link lib math instead? */

#ifndef min
# define min(A, B) (A < B ? A : B)
#endif
#ifndef max
# define max(A, B) (A > B ? A : B)
#endif


#endif
