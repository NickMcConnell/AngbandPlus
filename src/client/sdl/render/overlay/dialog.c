
/* $Id: dialog.c,v 1.3 2003/03/18 19:17:42 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"
#include "SDL_image.h"
#include "SDL_draw.h"

#include "ironhells.h"
#include "sdl/render/overlay.h"
#include "sdl/render/text.h"
#include "sdl/render/misc.h"

static void
divide_prompt(Overlay * overlay)
{
     char           *p;
     int             n = 0;

     fprintf(stderr, "divide_prompt()\n");

     /* Paranoia */
     if(!overlay)
          return;
     if(!overlay->gfx.dialog.prompt)
          return;

     p = overlay->gfx.dialog.prompt;

     /* The first line */
     overlay->gfx.dialog.lines[0] = p;

     /* The rest of the lines */
     while(n++ < IH_OVERLAY_DIALOG_PROMPT_MAX_LINES)
     {
          char           *c;

          c = strchr(p, '\n');
          if(!c)
               break;

          *c = 0;
          p = c + 1;
          overlay->gfx.dialog.lines[n - 1] = p;
     }

     /* Save the number of lines */
     overlay->gfx.dialog.n_lines = n;

     fprintf(stderr, "divide_prompt(): return\n");
}

void
IH_RenderOverlay_Dialog(Overlay * overlay)
{
     Uint32          color_val;
     int             i;

     fprintf(stderr, "IH_RenderOverlay_Dialog()\n");

     /* Paranoia */
     if(!overlay)
          return;
     if(!overlay->shown)
          return;
     if(!ih.dialog_prompt)
          return;

     /* Intialize the overlay?
      */
     if(!overlay->position.x)
     {
          int             len, widest = 0;

          if(!SDL_SemWait(ih.sem.talk))
          {
               len = strlen(ih.dialog_prompt);

               overlay->gfx.dialog.prompt = ralloc(len + 1);

               my_strcpy(overlay->gfx.dialog.prompt, ih.dialog_prompt,
                         len);

               SDL_SemPost(ih.sem.talk);
          }

          divide_prompt(overlay);

          for(i = 0; i < overlay->gfx.dialog.n_lines; i++)
          {
               int             width;

               width = IH_GetTextWidth(IH_FONT_NORMAL,
                                       overlay->gfx.dialog.lines[i]);
               if(width > widest)
                    widest = width;
          }

          overlay->gfx.dialog.w = widest + 6;
          overlay->gfx.dialog.h =
              (IH_FONT_NORMAL_SIZE * (overlay->gfx.dialog.n_lines + 1)) +
              6;

          overlay->position.x =
              (ih.display_width - overlay->gfx.dialog.w) / 2;
          overlay->position.y =
              (ih.display_height - overlay->gfx.dialog.h) / 2;
     }

     /* More paranoia */
     if(overlay->gfx.dialog.n_lines < 1)
          return;

     /* Draw the overlay border and backing */
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->gfx.dialog.w + 6, overlay->gfx.dialog.h + 6);

     color_val = SDL_MapRGB(ih.screen->format, 50, 50, 50);
     Draw_Round(ih.screen,
                overlay->position.x, overlay->position.y,
                overlay->gfx.dialog.w, overlay->gfx.dialog.h,
                5, color_val);

     /* Draw the prompt */
     for(i = 0; i < overlay->gfx.dialog.n_lines; i++)
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
          IH_RenderText(IH_FONT_NORMAL, overlay->gfx.dialog.lines[i], &pos,
                        color, NULL);
     }

     /* Draw the buffer text */
     if(ih.input_buffer)
     {
		  SDL_Rect rect;
          char           *buf_s;
		 
		  /* Backfill the buffer area with black, otherwise the
		   * text will hardly be readable.
		   */
		  rect.x = overlay->position.x + 3;
		  rect.y = overlay->position.y + 3 + (overlay->gfx.dialog.n_lines * IH_FONT_NORMAL_SIZE);
		  rect.w = overlay->gfx.dialog.w;
		  rect.h = IH_FONT_NORMAL_SIZE;
		  SDL_FillRect(ih.screen,
		 			   &rect,
		 			   SDL_MapRGB(ih.screen->format, 0, 0, 0));

		  /* Draw the buffer text, so that it fits.
		   */
          buf_s = ih.input_buffer;
          while(1)
          {
               int             width;

               width = IH_GetTextWidth(IH_FONT_NORMAL, buf_s);
               if(!width)
                    break;

               /* Check if text will fit.
                */
               if(width <= overlay->gfx.dialog.w - 6)
               {
                    SDL_Color       color;
                    ihFontPos       pos;

                    /* Set the line's position */
                    pos.x.type = IH_POSITION_TYPE_PIXEL;
                    pos.x.pixel = overlay->position.x + 3;
                    pos.y.type = IH_POSITION_TYPE_PIXEL;
                    pos.y.pixel =
                        overlay->position.y + 3 +
                        (overlay->gfx.dialog.n_lines *
                         IH_FONT_NORMAL_SIZE);

                    /* White */
                    color.r = color.g = color.b = 255;

                    IH_RenderText(IH_FONT_NORMAL, buf_s, &pos, color,
                                  NULL);

                    /* Draw a "cursor" */
                    Draw_Line(ih.screen,
                              overlay->position.x + 3 + width + 1,
                              overlay->position.y + 3 +
                              (overlay->gfx.dialog.n_lines *
                               IH_FONT_NORMAL_SIZE),
                              overlay->position.x + 3 + width + 1,
                              overlay->position.y + 3 +
                              ((overlay->gfx.dialog.n_lines +
                                1) * IH_FONT_NORMAL_SIZE),
                              SDL_MapRGB(ih.screen->format, 255, 255,
                                         255));

                    break;
               }

               buf_s++;
          }
     }

     fprintf(stderr, "IH_RenderOverlay_Dialog(): return\n");
}
