
/* $Id: icon.h,v 1.2 2003/04/06 15:22:11 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_ICON_H
#define IH_DISPLAYS_ISO_ICON_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband/angband.h"

/* Function prototypes.
 */
SDL_Surface    *IH_LoadIcon(int icon_size,
                            cptr name);
errr            IH_LoadIcons(void);
void            IH_PositionIcons(void);
void            IH_UpdateIcons(void);
void            IH_RenderIcons(void);
int             IH_GetIconSize(int size);
errr            IH_LoadCharacterIcon(void);
void            IH_UpdateSceneObject(SceneObject * object);
SceneObject    *IH_GetIconAtPosition(int x,
                                     int y);
void            IH_CalcIconPosition(int icon,
                                    int *x,
                                    int *y);

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

#endif /* IH_DISPLAYS_ISO_ICON_H */
