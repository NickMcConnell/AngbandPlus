
/* $Id: opponent.c,v 1.4 2003/04/20 05:20:58 cipher Exp $ */

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
#include "ironhells.h"
#include "overlay.h"
#include "render/text.h"

#define IH_OVERLAY_OPPONENT_NAME_FONT IH_FONT_NORMAL
#define IH_OVERLAY_OPPONENT_STATUS_FONT IH_FONT_SMALL

void
IH_RenderOverlay_Opponent(Overlay * overlay)
{
     ihColor         color;
     SDL_Rect        rect;
     ihFontPos       pos;
     int             cur_hp, max_hp;
     int             pct, back_width, bar_width;
     byte            attr;
     u32b            flags;
     char           *m_name, *m_stat;

     fprintf(stderr, "IH_RenderOverlay_Opponent()\n");

     if(!overlay)
          return;

     m_name = overlay->buffer[0];
     m_stat = overlay->buffer[1];
     cur_hp = overlay->var[0];
     max_hp = MIN(1, overlay->var[1]); /* prevent divide by 0 */
     attr = (byte) overlay->var[2];
     flags = overlay->flags;

     fprintf(stderr, "IH_RenderOverlay_Opponent(): set overlay y and h\n");
     overlay->position.y = 30;
     overlay->position.h = IH_GetFontHeight(IH_OVERLAY_OPPONENT_NAME_FONT);

     /* Extract the "percent" of health */
     fprintf(stderr,
             "IH_RenderOverlay_Opponent(): calculate health percentage\n");
     pct = 100L * cur_hp / max_hp;
     fprintf(stderr, "IH_RenderOverlay_Opponent(): pct = %d\n", pct);

     fprintf(stderr,
             "IH_RenderOverlay_Opponent(): calculate bar and back width\n");
     if(m_name)
          back_width =
              MAX(max_hp,
                  IH_GetTextWidth(IH_OVERLAY_OPPONENT_NAME_FONT,
                                  m_name) + 6);
     else
          back_width = MIN(max_hp, 400);
     bar_width = (back_width * pct) / 100;

     fprintf(stderr,
             "IH_RenderOverlay_Opponent(): calculate overlay x and w\n");
     overlay->position.x = (ih.display.width - back_width) / 2;
     overlay->position.w = back_width;

     /* Draw the health bar.
      */
     if(flags & OPP_UNKNOWN)
     {
          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): monster is not unknown\n");

          fprintf(stderr, "IH_RenderOverlay_Opponent(): set color\n");
          IH_AttrToColor(attr, &color);
          color.alpha = IH_ALPHA_VALUE;
          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): map rgba from color\n");

          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): calculate rectangle\n");
          rect.x = (ih.display.width - back_width) / 2;
          rect.y = overlay->position.y;
          rect.w = bar_width;
          rect.h = overlay->position.h;

          fprintf(stderr, "IH_RenderOverlay_Opponent(): fill rectangle\n");
          IH_FillArea(&rect, &color);
     }

     /* Draw the health bar "backing."  This is done after the health bar
      * is drawn, so that the bar coloring is dimmed.
      */
     fprintf(stderr, "IH_RenderOverlay_Opponent(): shade overlay area\n");
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y, back_width, overlay->position.h,
                  NULL);

     /* Draw the monster name and status.
      */
     fprintf(stderr,
             "IH_RenderOverlay_Opponent(): check to display the name\n");
     if(m_name)
     {
          /* Name */
          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): set text position\n");
          pos.x.type = IH_POSITION_TYPE_ABS_CENTER;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel = overlay->position.y + 1;

          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): set color to white\n");
          IH_AttrToColor(COLOR_WHITE, &color);

          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): render monster name\n");
          IH_RenderText(IH_OVERLAY_OPPONENT_NAME_FONT, m_name, &pos,
                        &color, 0, NULL);

          /* Status */
          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): set status text position\n");
          pos.y.pixel = overlay->position.y + overlay->position.h + 3;

          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): set color to grey\n");
          IH_AttrToColor(COLOR_L_WHITE, &color);

          fprintf(stderr,
                  "IH_RenderOverlay_Opponent(): render monster status\n");
          IH_RenderText(IH_OVERLAY_OPPONENT_STATUS_FONT, m_stat, &pos,
                        &color, 0, NULL);
     }
     fprintf(stderr, "IH_RenderOverlay_Opponent(): return\n");
}

void
IH_ReleaseOverlay_Opponent(Overlay * overlay)
{
     if(!overlay)
          return;
}
