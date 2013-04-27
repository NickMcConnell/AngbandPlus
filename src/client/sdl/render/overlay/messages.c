
/* $Id: messages.c,v 1.2 2003/03/24 06:04:53 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "ironhells.h"
#include "sdl/render/overlay.h"
#include "sdl/render/text.h"

void
IH_RenderOverlay_Messages(Overlay * overlay)
{
     int             i, height;

     /* Calculate top Y position.
      */
     height = IH_GetFontHeight(IH_FONT_NORMAL);

     /* Shade the area.
      */
     // FIXME

     /* Display the messages.
      */
     for(i = 0; i < ih.messages_shown; i++)
     {
          SDL_Color       color;
          ihFontPos       pos;
          cptr            msg;
          int             display_y;

          msg = message_str(i);
          if(!msg)
               continue;

          display_y = ih.display_height - ((i + 1) * height);

          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = 0;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel = display_y;

          color.r = color.g = color.b = 255;

          IH_RenderText(IH_FONT_NORMAL, msg, &pos, color, NULL);
     }
}
