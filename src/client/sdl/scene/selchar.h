#ifndef IH_SDL_SCENE_SELCHAR_H
#define IH_SDL_SCENE_SELCHAR_H

/* File: sdl/scene/selchar.h */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "sdl/render.h"

/* Function prototypes.
*/
void IH_InitScene_SelChar(void);
void IH_ProcessScene_SelChar(SDL_Event *event);
void IH_RenderScene_SelChar(void);

/* Data definitions.
*/

#endif /* IH_SDL_SCENE_SELCHAR_H */
