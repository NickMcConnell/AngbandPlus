
/* $Id: icon.h,v 1.5 2003/04/18 03:32:56 cipher Exp $ */

#ifndef IH_RENDER_ICON_H
#define IH_RENDER_ICON_H

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
#include "scene.h"

/* Function prototypes.
 */
SDL_Surface    *IH_LoadIcon(int icon_size,
                            cptr name);
void            IH_PositionIcons(void);
int             IH_GetIconSize(int size);
SceneObject    *IH_GetIconAtPosition(int x,
                                     int y);
void            IH_CalcIconPosition(int icon,
                                    int *x,
                                    int *y);
void            IH_RenderIcon(SceneObject * object,
                              SDL_Rect * srect,
                              SDL_Rect * drect);
void            IH_RenderIcons(void);

/* Data definitions.
 */
enum
{
     IH_ICON_SIZE_NONE,
     IH_ICON_SIZE_SMALL,
     IH_ICON_SIZE_MEDIUM,
     IH_ICON_SIZE_LARGE,

     IH_ICON_SIZE_CURRENT,

     IH_ICON_SIZE_END
};

#define IH_MAX_ICONS 1024

#endif /* IH_RENDER_ICON_H */
