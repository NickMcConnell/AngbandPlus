
/* $Id: error.c,v 1.1 2003/04/01 22:26:18 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL_draw.h"

/* Internal headers */
#include "ironhells.h"
#include "displays/iso/overlay.h"
#include "displays/iso/text.h"
#include "displays/iso/misc.h"

static void
divide_prompt(Overlay * overlay)
{
     char           *p;
     int             n = 0;

     fprintf(stderr, "divide_prompt()\n");

     /* Paranoia */
     if(!overlay)
          return;
     if(!overlay->gfx.error.text)
          return;

     p = overlay->gfx.error.text;

     /* The first line */
     overlay->gfx.error.lines[0] = p;

     /* The rest of the lines */
     while(n++ < IH_OVERLAY_ERROR_DISPLAY_MAX_LINES)
     {
          char           *c;

          c = strchr(p, '\n');
          if(!c)
               break;

          *c = 0;
          p = c + 1;
          overlay->gfx.error.lines[n - 1] = p;
     }

     /* Save the number of lines */
     overlay->gfx.error.n_lines = n;

     fprintf(stderr, "divide_prompt(): return\n");
}

void
IH_RenderOverlay_Error(Overlay * overlay)
{
     Uint32          color_val;
     int             i;

     fprintf(stderr, "IH_RenderOverlay_Error()\n");

     /* Paranoia */
     if(!overlay)
          return;
     if(!overlay->shown)
          return;
     if(!ih.err_message)
          return;

     /* Intialize the overlay?
      */
     if(overlay->recalc)
     {
          int             len, widest = 0;

          if(!SDL_SemWait(ih.sem.talk))
          {
               len = strlen(ih.err_message);

               overlay->gfx.error.text = ralloc(len + 1);

               my_strcpy(overlay->gfx.error.text, ih.err_message, len);

               SDL_SemPost(ih.sem.talk);
          }

          divide_prompt(overlay);

          for(i = 0; i < overlay->gfx.error.n_lines; i++)
          {
               int             width;

               width = IH_GetTextWidth(IH_FONT_NORMAL,
                                       overlay->gfx.error.lines[i]);
               if(width > widest)
                    widest = width;
          }

          overlay->position.w = widest + 6;
          overlay->position.h =
              (IH_FONT_NORMAL_SIZE * (overlay->gfx.error.n_lines + 1)) + 6;

          overlay->position.x =
              (ih.display_width - overlay->position.w) / 2;
          overlay->position.y =
              (ih.display_height - overlay->position.h) / 2;
     }

     /* More paranoia */
     if(overlay->gfx.error.n_lines < 1)
          return;

     /* Draw the overlay border and backing */
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + 6, overlay->position.h + 6);

     color_val = SDL_MapRGB(ih.screen->format, 50, 50, 50);
     Draw_Round(ih.screen,
                overlay->position.x, overlay->position.y,
                overlay->position.w, overlay->position.h, 5, color_val);

     /* Draw the prompt */
     for(i = 0; i < overlay->gfx.error.n_lines; i++)
     {
          SDL_Color       color;
          ihFontPos       pos;

          /* Set the line's position */
          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = overlay->position.x + 3;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel =
              overlay->position.y + 3 + (i * IH_FONT_NORMAL_SIZE);

          /* Yellow */
          color.r = color.g = 255;
          color.b = 0;

          /* Draw the line */
          IH_RenderText(IH_FONT_NORMAL, overlay->gfx.error.lines[i], &pos,
                        color, NULL);
     }

     /* Draw the buffer text */
     if(overlay->buffer[0])
     {
          char           *buf_s;

          buf_s = overlay->buffer[0];
          while(1)
          {
               int             width;

               width = IH_GetTextWidth(IH_FONT_NORMAL, buf_s);
               if(!width)
                    break;

               /* Check if text will fit.
                */
               if(width <= overlay->position.w - 6)
               {
                    SDL_Color       color;
                    ihFontPos       pos;

                    /* Set the line's position */
                    pos.x.type = IH_POSITION_TYPE_PIXEL;
                    pos.x.pixel = overlay->position.x + 3;
                    pos.y.type = IH_POSITION_TYPE_PIXEL;
                    pos.y.pixel =
                        overlay->position.y + 3 +
                        (overlay->gfx.error.n_lines * IH_FONT_NORMAL_SIZE);

                    /* White */
                    color.r = color.g = color.b = 255;

                    IH_RenderText(IH_FONT_NORMAL, buf_s, &pos, color,
                                  NULL);

                    /* Draw a "cursor" */
                    Draw_Line(ih.screen,
                              overlay->position.x + 3 + width + 1,
                              overlay->position.y + 3 +
                              (overlay->gfx.error.n_lines *
                               IH_FONT_NORMAL_SIZE),
                              overlay->position.x + 3 + width + 1,
                              overlay->position.y + 3 +
                              ((overlay->gfx.error.n_lines +
                                1) * IH_FONT_NORMAL_SIZE),
                              SDL_MapRGB(ih.screen->format, 255, 255,
                                         255));

                    break;
               }

               buf_s++;
          }
     }

     fprintf(stderr, "IH_RenderOverlay_Error(): return\n");
}

void
IH_ReleaseOverlay_Error(Overlay * overlay)
{
     if(!overlay)
          return;
}
