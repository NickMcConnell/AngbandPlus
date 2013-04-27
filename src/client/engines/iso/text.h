
/* $Id: text.h,v 1.5 2003/04/18 03:32:56 cipher Exp $ */

#ifndef IH_ENGINES_ISO_TEXT_H
#define IH_ENGINES_ISO_TEXT_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

/* SDL headers */
#include "SDL.h"

/* Internal headers */
#include "angband/angband.h"
#include "render/text.h"
#include "render/misc.h"

/* Function prototypes (callbacks).
 */
void            IH_ISO_RenderText(int size,
                                  cptr text,
                                  ihFontPos * pos,
                                  ihColor * color,
                                  u32b flags,
                                  SDL_Rect * rect);
int             IH_ISO_GetTextWidth(int size,
                                    cptr text);
int             IH_ISO_GetFontHeight(int size);

/* Function prototypes (setup).
 */
errr            IH_ISO_LoadFonts(void);
void            IH_ISO_FreeFonts(void);

/* Data definitions.
 */
#define IH_RTF_SHADOW 0x00000001L

#endif /* IH_ENGINES_ISO_TEXT_H */
