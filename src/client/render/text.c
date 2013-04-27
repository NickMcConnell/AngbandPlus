
/* $Id: text.c,v 1.5 2003/04/18 21:45:45 cipher Exp $ */

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
#include "ironhells.h"
#include "path.h"
#include "platform/platform.h"

void
IH_RenderText(int size,
              cptr text,
              ihFontPos * pos,
              ihColor * color,
              u32b flags,
              SDL_Rect * rect)
{
     if(ih.display.render_text_func)
          (*ih.display.render_text_func) (size, text, pos, color, flags,
                                          rect);
}

int
IH_GetTextWidth(int size,
                cptr text)
{
     int             width = 0;

     if(ih.display.get_text_width_func)
          width = (*ih.display.get_text_width_func) (size, text);

     return width;
}

void
IH_ProcessFontPos(SDL_Rect * srect,
                  ihFontPos * pos,
                  SDL_Rect * drect)
{
     int             width, height;

     if(!srect || !pos || !drect)
          return;

     width = srect->w;
     height = srect->h;

     switch (pos->x.type)
     {
          case IH_POSITION_TYPE_PIXEL:
               srect->x = pos->x.pixel;
               pos->x.perc = pos->x.pixel / ih.display.width;
               break;

          case IH_POSITION_TYPE_PERCENT:
               srect->x = ih.display.width * pos->x.perc;
               pos->x.pixel = srect->x;
               break;

          case IH_POSITION_TYPE_ABS_CENTER:
               srect->x = (ih.display.width - width) / 2;
               pos->x.pixel = srect->x;
               pos->x.perc = pos->x.pixel / ih.display.width;
               break;

          case IH_POSITION_TYPE_PIXEL_CENTER:
               srect->x = pos->x.pixel - (width / 2);
               pos->x.pixel = srect->x;
               pos->x.perc = pos->x.pixel / ih.display.width;
               break;

          case IH_POSITION_TYPE_ABS_LEFT:
          case IH_POSITION_TYPE_ABS_TOP:
               srect->x = 0;
               pos->x.pixel = 0;
               pos->x.perc = 0.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_LEFT:
          case IH_POSITION_TYPE_PIXEL_TOP:
               srect->x = pos->x.pixel;
               pos->x.perc = pos->x.pixel / ih.display.width;
               break;

          case IH_POSITION_TYPE_ABS_RIGHT:
          case IH_POSITION_TYPE_ABS_BOTTOM:
               srect->x = ih.display.width - width;
               pos->x.pixel = srect->x;
               pos->x.perc = 1.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_RIGHT:
          case IH_POSITION_TYPE_PIXEL_BOTTOM:
               srect->x = pos->x.pixel - width;
               pos->x.pixel = srect->x;
               pos->x.perc = pos->x.pixel / ih.display.width;
               break;
     }

     switch (pos->y.type)
     {
          case IH_POSITION_TYPE_PIXEL:
               srect->y = pos->y.pixel;
               pos->y.perc = pos->y.pixel / ih.display.height;
               break;

          case IH_POSITION_TYPE_PERCENT:
               srect->y = ih.display.height * pos->y.perc;
               pos->y.pixel = srect->y;
               break;

          case IH_POSITION_TYPE_ABS_CENTER:
               srect->y = (ih.display.height - height) / 2;
               pos->y.pixel = srect->y;
               pos->y.perc = pos->y.pixel / ih.display.height;
               break;

          case IH_POSITION_TYPE_PIXEL_CENTER:
               srect->y = pos->y.pixel - (height / 2);
               pos->y.pixel = srect->y;
               pos->y.perc = pos->y.pixel / ih.display.height;
               break;

          case IH_POSITION_TYPE_ABS_LEFT:
          case IH_POSITION_TYPE_ABS_TOP:
               srect->y = 0;
               pos->y.pixel = 0;
               pos->y.perc = 0.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_LEFT:
          case IH_POSITION_TYPE_PIXEL_TOP:
               srect->y = pos->y.pixel;
               pos->y.perc = pos->y.pixel / ih.display.height;
               break;

          case IH_POSITION_TYPE_ABS_RIGHT:
          case IH_POSITION_TYPE_ABS_BOTTOM:
               srect->y = ih.display.height - height;
               pos->y.pixel = srect->y;
               pos->y.perc = 1.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_RIGHT:
          case IH_POSITION_TYPE_PIXEL_BOTTOM:
               srect->y = pos->y.pixel - height;
               pos->y.pixel = srect->y;
               pos->y.perc = pos->y.pixel / ih.display.height;
               break;
     }
}

int
IH_GetFontHeight(int size)
{
     int             height = 0;

     if(ih.display.get_font_height_func)
          height = (*ih.display.get_font_height_func) (size);

     return height;
}
