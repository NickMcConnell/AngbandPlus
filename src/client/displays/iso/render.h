
/* $Id: render.h,v 1.2 2003/04/02 22:17:40 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_RENDER_H
#define IH_DISPLAYS_ISO_RENDER_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"

/* Internal headers */
#include "angband/angband.h"

/* Function prototypes.
 */

/* Data definitions.
 */
enum
{
     IH_HILITE_NORMAL,
     IH_HILITE_HOVER,
     IH_HILITE_SELECT,

     IH_HILITE_END
};

#endif /* IH_DISPLAYS_ISO_RENDER_H */
