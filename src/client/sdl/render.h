#ifndef IH_SDL_RENDER_H
#define IH_SDL_RENDER_H

/* File: sdl/render.h */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"

#include "angband.h"

/* Function prototypes.
 */
void IH_RenderScene(void);

/* Data definitions.
 */
enum
{
     IH_HILITE_NORMAL,
     IH_HILITE_HOVER,
     IH_HILITE_SELECT,

     IH_HILITE_END
};

#endif /* IH_SDL_RENDER_H */