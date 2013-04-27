
/* $Id: text.h,v 1.7 2003/03/24 06:04:53 cipher Exp $ */

#ifndef IH_SDL_RENDER_TEXT_H
#define IH_SDL_RENDER_TEXT_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#ifdef HAVE_CONFIG_H
#include "autoconf.h"
#endif

#include "SDL.h"
#ifdef USE_SDLTTF
#include "SDL_ttf.h"
#else
#include "sdl/bfont.h"
#endif

#include "angband.h"

typedef struct _ihFontPos ihFontPos;

/* Function prototypes.
 */
void            IH_RenderText(int size,
                              cptr text,
                              ihFontPos * font_pos,
                              SDL_Color color,
                              SDL_Rect * rect);
errr            IH_InitFonts(void);
void            IH_FreeFonts(void);
void            IH_ProcessFontPos(SDL_Surface * image,
                                  ihFontPos * font_pos,
                                  SDL_Rect * rect);
int             IH_GetTextWidth(int size,
                                cptr text);
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

#endif /* IH_SDL_RENDER_TEXT_H */
