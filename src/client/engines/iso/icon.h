
/* $Id: icon.h,v 1.4 2003/04/18 03:32:56 cipher Exp $ */

#ifndef IH_ENGINES_ISO_ICON_H
#define IH_ENGINES_ISO_ICON_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers.
 */
#include "SDL.h"

/* Internal headers.
 */
#include "angband/angband.h"
#include "scene.h"

/* Function prototypes (callbacks).
 */
void            IH_ISO_RenderIcon(SceneObject * object,
                                  SDL_Rect * srect,
                                  SDL_Rect * drect);

/* Function prototypes (setup).
 */
errr            IH_ISO_LoadIcons(void);
void            IH_ISO_FreeIcons(void);
errr            IH_ISO_LoadCharacterIcon(void);

/* Function prototypes (miscellaneous).
 */
SDL_Surface    *IH_ISO_LoadIcon(int icon_size,
                                cptr name);

/* Data definitions.
 */

#endif /* IH_ENGINES_ISO_ICON_H */
