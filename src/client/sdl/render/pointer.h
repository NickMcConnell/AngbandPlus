#ifndef IH_SDL_RENDER_POINTER_H
#define IH_SDL_RENDER_POINTER_H

/* File: sdl/render/pointer.h */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "h-type.h"

/* Function prototypes.
 */
errr IH_LoadPointers(void);
void IH_RenderPointer(void);

/* Data definitions.
 */
enum
{
     IH_POINTER_NONE,
	IH_POINTER_STANDARD,
	IH_POINTER_FORBID,
	IH_POINTER_ATTACK,
	IH_POINTER_DIG,
	IH_POINTER_TARGET,
	IH_POINTER_OPEN,
	IH_POINTER_CLOSE,
	IH_POINTER_DISARM,
	IH_POINTER_SPELL,

	IH_POINTER_MAX
};

#endif /* IH_SDL_RENDER_POINTER_H */
