
/* $Id: engines.h,v 1.3 2003/04/15 20:21:56 cipher Exp $ */

#ifndef IH_ENGINES_H
#define IH_ENGINES_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Internal headers */
#include "angband/angband.h"
#include "render/icon.h"
#include "render/misc.h"

/* Function prototypes.
 */
errr            IH_Engines_Init(void);
void            IH_Engines_Cleanup(void);

/* Data definitions.
 */
typedef struct _displayData displayData;

struct _displayData
{
     SceneObject    *icons[IH_MAX_ICONS];
     SceneObject    *tiles[IH_MAX_TILES];
     SceneObject    *objects[IH_MAX_OBJECTS];
     SceneObject    *creatures[IH_MAX_CREATURES];
     SceneObject    *misc_objects[IH_MAX_MISC_OBJECTS];

     SceneObject    *player_image;

     int             tile_count;
     int             object_count;
     int             creature_count;
     int             misc_object_count;
     int             icon_count;

     void           *engine_data;
};

#endif /* IH_ENGINES_H */
