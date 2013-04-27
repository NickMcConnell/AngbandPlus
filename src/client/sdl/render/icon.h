
/* $Id: icon.h,v 1.6 2003/03/18 19:17:40 cipher Exp $ */

#ifndef IH_SDL_RENDER_ICON_H
#define IH_SDL_RENDER_ICON_H

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
errr            IH_LoadIcons(void);
void            IH_PositionIcons(void);
void            IH_RenderIcons(void);
int             IH_GetIconSize(int size);
errr            IH_LoadCharacterIcon(void);

/* Data definitions.
 */
enum
{
     IH_ICON_SIZE_SMALL,
     IH_ICON_SIZE_MEDIUM,
     IH_ICON_SIZE_LARGE,

     IH_ICON_SIZE_CURRENT,

     IH_ICON_SIZE_END
};

#endif /* IH_SDL_RENDER_ICON_H */
