
/* $Id: text.h,v 1.5 2003/04/18 21:45:46 cipher Exp $ */

#ifndef IH_RENDER_TEXT_H
#define IH_RENDER_TEXT_H

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
#include "render/misc.h"

typedef struct _ihFontPos ihFontPos;

/* Function prototypes.
 */
void            IH_RenderText(int size,
                              cptr text,
                              ihFontPos * pos,
                              ihColor * color,
                              u32b flags,
                              SDL_Rect * rect);
int             IH_GetTextWidth(int size,
                                cptr text);
void            IH_ProcessFontPos(SDL_Rect * srect,
                                  ihFontPos * pos,
                                  SDL_Rect * drect);
int             IH_GetFontHeight(int size);

/* Data definitions.
 */
enum
{
     IH_FONT_SMALL,
     IH_FONT_NORMAL,
     IH_FONT_LARGE,

     IH_FONT_END
};

struct _ihFontPos
{
     struct
     {
          int             type;
          int             pixel;
          float           perc;
     }
     x;
     struct
     {
          int             type;
          int             pixel;
          float           perc;
     }
     y;
};

enum
{
     IH_POSITION_TYPE_PIXEL,
     IH_POSITION_TYPE_PERCENT,
     IH_POSITION_TYPE_PIXEL_CENTER,
     IH_POSITION_TYPE_ABS_CENTER,
     IH_POSITION_TYPE_PIXEL_LEFT,
     IH_POSITION_TYPE_ABS_LEFT,
     IH_POSITION_TYPE_PIXEL_TOP,
     IH_POSITION_TYPE_ABS_TOP,
     IH_POSITION_TYPE_PIXEL_RIGHT,
     IH_POSITION_TYPE_ABS_RIGHT,
     IH_POSITION_TYPE_PIXEL_BOTTOM,
     IH_POSITION_TYPE_ABS_BOTTOM,

     IH_POSITION_TYPE_END
};

#define IH_FONT_NORMAL_SIZE 14
#define IH_FONT_LARGE_SIZE  20

#endif /* IH_RENDER_TEXT_H */
