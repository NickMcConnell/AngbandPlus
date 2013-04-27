
/* $Id: misc.h,v 1.7 2003/04/18 21:45:12 cipher Exp $ */

#ifndef IH_ENGINES_ISO_MISC_H
#define IH_ENGINES_ISO_MISC_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Internal headers */
#include "angband/angband.h"

/* Function prototypes.
 */
errr            IH_ISO_LoadMisc(void);
void            IH_ISO_FreeMisc(void);
void            IH_ISO_RenderError(void);
void            IH_ISO_ShadeArea(int x,
                                 int y,
                                 int w,
                                 int h,
                                 ihColor * color);
void            IH_ISO_FrameArea(int x,
                                 int y,
                                 int w,
                                 int h,
                                 ihColor * color);
void            IH_ISO_RenderImage(SDL_Surface * surface,
                                   SDL_Rect * srect,
                                   SDL_Rect * drect);
void            IH_ISO_DrawLine(int x1,
                                int y1,
                                int x2,
                                int y2,
                                ihColor * color);
void            IH_ISO_ClearDrawingBuffer(void);
void            IH_ISO_SwapBuffers(void);
void            IH_ISO_FillArea(SDL_Rect * rect,
                                ihColor * color);
void            IH_ISO_RenderBackground(void);
void            IH_ISO_RenderSplash(void);
void            IH_ISO_RenderTitle(void);
void            IH_ISO_ConvertColor(ihColor * src_color,
                                    SDL_Color * dst_color);

/* Data definitions.
 */
#define IH_ISO_SDL_COLOR_VALUE 255

#endif /* IH_ENGINES_ISO_MISC_H */
