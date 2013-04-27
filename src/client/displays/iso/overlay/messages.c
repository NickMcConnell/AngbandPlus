
/* $Id: messages.c,v 1.1 2003/04/01 22:26:18 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Internal headers */
#include "ironhells.h"
#include "displays/iso/overlay.h"
#include "displays/iso/text.h"
#include "displays/iso/misc.h"

void
IH_RenderOverlay_Messages(Overlay * overlay)
{
     int             i, height;

     /* Get font height.
      */
     height = IH_GetFontHeight(IH_FONT_NORMAL);

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

          IH_AttrToColor(message_color(i), &color);

          IH_RenderText(IH_FONT_NORMAL, msg, &pos, color, NULL);
     }
}

void
IH_ReleaseOverlay_Messages(Overlay * overlay)
{
     if(!overlay)
          return;
}
