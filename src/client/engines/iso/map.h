
/* $Id: map.h,v 1.1 2003/04/13 05:17:41 cipher Exp $ */

#ifndef IH_ENGINES_ISO_MAP_H
#define IH_ENGINES_ISO_MAP_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */

/* Internal headers */
#include "angband/angband.h"

/* Function prototypes.
 */
errr            IH_ISO_LoadMapImages(void);
void            IH_ISO_FreeMapImages(void);
void            IH_ISO_DrawMap(void);

/* Data definitions.
 */
#define IH_TILE_BASE_WIDTH 108
#define IH_TILE_ACTUAL_WIDTH 108
#define IH_TILE_BASE_HEIGHT 54
#define IH_TILE_ACTUAL_HEIGHT 198

#endif /* IH_ENGINES_ISO_MAP_H */
