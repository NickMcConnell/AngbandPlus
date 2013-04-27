#ifndef IH_SDL_SCENE_INTRO_H
#define IH_SDL_SCENE_INTRO_H

/* File: sdl/scene/intro.h */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* Function prototypes.
*/
void IH_InitScene_Intro(void);
void IH_ProcessScene_Intro(SDL_Event *event);
void IH_RenderScene_Intro(void);

/* Data definitions.
*/

#endif /* IH_SDL_SCENE_INTRO_H */