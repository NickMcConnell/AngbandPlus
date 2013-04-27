
/* $Id: object.h,v 1.2 2003/04/18 03:32:27 cipher Exp $ */

#ifndef IH_OBJECT_H
#define IH_OBJECT_H

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
#include "ironhells.h"
#include "scene.h"

/* Function prototypes.
 */
void            IH_SceneButton_SetText(SceneButton * button,
                                       cptr text);
void            IH_SceneObject_RenderText(SceneObject * object);
void            IH_SceneObject_RenderImage(SceneObject * object,
                                           SDL_Rect * srect,
                                           SDL_Rect * drect);
void            IH_SceneObject_Update(SceneObject * object);
SceneObject    *IH_SceneObject_Alloc(int type,
                                     int object);
void            IH_SceneObject_Free(SceneObject * object);

#endif /* IH_OBJECT_H */
