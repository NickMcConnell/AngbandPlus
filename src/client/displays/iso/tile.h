
/* $Id: tile.h,v 1.2 2003/04/07 00:27:13 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_TILE_H
#define IH_DISPLAYS_ISO_TILE_H

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
#include "displays/iso/scene.h"

/* Function prototypes.
 */
errr            IH_InitTiles(void);
void            IH_RenderTiles(void);

/* Data definitions.
 */
#define IH_TILE_BASE_WIDTH 108
#define IH_TILE_ACTUAL_WIDTH 108
#define IH_TILE_BASE_HEIGHT 54
#define IH_TILE_ACTUAL_HEIGHT 198

#define IH_MAX_TILES 1024
#define IH_MAX_OBJECTS 1024
#define IH_MAX_CREATURES 1024

#endif /* IH_DISPLAYS_ISO_TILE_H */
