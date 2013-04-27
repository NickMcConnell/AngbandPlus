
/* $Id: misc.h,v 1.8 2003/03/17 22:45:39 cipher Exp $ */

#ifndef IH_SDL_RENDER_MISC_H
#define IH_SDL_RENDER_MISC_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "sdl/render.h"
#include "sdl/render/text.h"

/* Function prototypes.
*/
errr            IH_LoadLMX(void);
errr            IH_LoadImages(void);
void            IH_PositionMisc(void);
void            IH_RenderMisc(void);
void            IH_RenderError(void);
void            IH_GetButtonColor(int hilite,
                                  SDL_Color * color);
bool            IH_IsPointerInRect(int x,
                                   int y,
                                   SDL_Rect * rect);
void            IH_ShadeArea(int x,
                             int y,
                             int w,
                             int h);

/* Data definitions.
*/

#endif /* IH_SDL_RENDER_MISC_H */
