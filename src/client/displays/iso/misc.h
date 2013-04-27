
/* $Id: misc.h,v 1.2 2003/04/06 15:22:11 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_MISC_H
#define IH_DISPLAYS_ISO_MISC_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/angband.h"
#include "displays/iso/render.h"
#include "displays/iso/text.h"
#include "displays/iso/scene.h"

/* Function prototypes.
 */
errr            IH_InitMisc(void);
errr            IH_LoadLMX(void);
errr            IH_LoadImages(void);
void            IH_PositionMisc(void);
void            IH_RenderMisc(void);
void            IH_RenderSceneObjectText(SceneObject * object);
void            IH_RenderError(void);
void            IH_ShadeArea(int x,
                             int y,
                             int w,
                             int h);
void            IH_RenderSceneObjectText(SceneObject * object);
void            IH_AttrToColor(byte attr,
                               SDL_Color * color);
void            IH_GetButtonColor(int hilite,
                                  SDL_Color * color);
bool            IH_IsPointerInRect(int x,
                                   int y,
                                   SDL_Rect * rect);

/* Data definitions.
 */
#define IH_MAX_MISC_OBJECTS 1024

#endif /* IH_DISPLAYS_ISO_MISC_H */
