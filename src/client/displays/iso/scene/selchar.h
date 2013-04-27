
/* $Id: selchar.h,v 1.1 2003/04/01 22:26:20 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_SCENE_SELCHAR_H
#define IH_DISPLAYS_ISO_SCENE_SELCHAR_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/angband.h"
#include "displays/iso/render.h"

/* Function prototypes.
*/
void            IH_InitScene_SelChar(void);
void            IH_ProcessScene_SelChar(SDL_Event * event);
void            IH_RenderScene_SelChar(void);
void            IH_CleanupScene_SelChar(void);

/* Data definitions.
*/

#endif /* IH_DISPLAYS_ISO_SCENE_SELCHAR_H */
