
/* $Id: dialog.c,v 1.3 2003/04/18 21:45:13 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"
#include "SDL_image.h"
#include "SDL_draw.h"

/* Internal headers */
#include "ironhells.h"
#include "overlay.h"
#include "render/text.h"
#include "render/misc.h"

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

     fprintf(stderr, "divide_prompt(): set p\n");
     p = overlay->gfx.dialog.prompt;

     /* The first line */
     fprintf(stderr, "divide_prompt(): mark the first line\n");
     overlay->gfx.dialog.lines[0] = p;

     /* The rest of the lines */
     fprintf(stderr, "divide_prompt(): check for max lines\n");
     while(n++ < IH_OVERLAY_DIALOG_PROMPT_MAX_LINES)
     {
          char           *c;

          fprintf(stderr, "divide_prompt(): check for newline\n");
          c = strchr(p, '\n');
          if(!c)
               break;

          fprintf(stderr, "divide_prompt(): mark the next string\n");
          *c = 0;
          p = c + 1;
          overlay->gfx.dialog.lines[n - 1] = p;
          fprintf(stderr, "divide_prompt(): set line to p (%s)\n", p);
     }

     /* Save the number of lines */
     fprintf(stderr, "divide_prompt(): set n_lines = %d\n", n);
     overlay->gfx.dialog.n_lines = n;
     fprintf(stderr, "divide_prompt(): return\n");
}

void
IH_RenderOverlay_Dialog(Overlay * overlay)
{
     ihColor         color;
     int             i;

     fprintf(stderr, "IH_RenderOverlay_Dialog()\n");
     /* Paranoia */
     if(!overlay)
          return;
     if(!overlay->shown)
          return;

     /* Check that buffer[0] contains something.  That is our
      * prompt string.
      */
     fprintf(stderr, "IH_RenderOverlay_Dialog(): check for buffer[0]\n");
     if(!overlay->buffer[0])
          return;

     /* Intialize the overlay?
      */
     fprintf(stderr, "IH_RenderOverlay_Dialog(): check for recalc\n");
     if(overlay->recalc)
     {
          int             len, widest = 0;

          fprintf(stderr, "IH_RenderOverlay_Dialog(): recalc\n");
          if(!SDL_SemWait(ih.ipc.sem.talk))
          {
               len = strlen(overlay->buffer[0]);

               fprintf(stderr,
                       "IH_RenderOverlay_Dialog(): free the old prompt\n");
               if(overlay->gfx.dialog.prompt)
                    rnfree(overlay->gfx.dialog.prompt);

               fprintf(stderr,
                       "IH_RenderOverlay_Dialog(): allocate space for a new prompt\n");
               overlay->gfx.dialog.prompt = ralloc(len + 1);

               fprintf(stderr,
                       "IH_RenderOverlay_Dialog(): copy the prompt\n");
               my_strcpy(overlay->gfx.dialog.prompt, overlay->buffer[0],
                         len);

               fprintf(stderr,
                       "IH_RenderOverlay_Dialog(): release the semaphore\n");
               SDL_SemPost(ih.ipc.sem.talk);
          }

          fprintf(stderr,
                  "IH_RenderOverlay_Dialog(): divide the prompt\n");
          divide_prompt(overlay);

          for(i = 0; i < overlay->gfx.dialog.n_lines; i++)
          {
               int             width;

               fprintf(stderr,
                       "IH_RenderOverlay_Dialog(): get the width of the line\n");
               width =
                   IH_GetTextWidth(IH_FONT_LARGE,
                                   overlay->gfx.dialog.lines[i]);
               if(width > widest)
                    widest = width;
               fprintf(stderr, "IH_RenderOverlay_Dialog(): widest = %d\n",
                       widest);
          }

          fprintf(stderr, "IH_RenderOverlay_Dialog(): set size\n");
          overlay->position.w = widest;
          overlay->position.h =
              (IH_GetFontHeight(IH_FONT_LARGE) *
               overlay->gfx.dialog.n_lines) +
              (overlay->buffer[1] ? IH_GetFontHeight(IH_FONT_NORMAL) : 0);

          fprintf(stderr, "IH_RenderOverlay_Dialog(): set origin\n");
          overlay->position.x =
              (ih.display.width - overlay->position.w +
               (IH_OVERLAY_DIALOG_OFFSET_X * 2)) / 2;
          overlay->position.y =
              (ih.display.height - overlay->position.h +
               (IH_OVERLAY_DIALOG_OFFSET_Y * 2)) / 2;

          fprintf(stderr,
                  "IH_RenderOverlay_Dialog(): clear the recalc flag\n");
          overlay->recalc = FALSE;
     }

     /* More paranoia */
     fprintf(stderr,
             "IH_RenderOverlay_Dialog(): check for n_lines >= 1\n");
     if(overlay->gfx.dialog.n_lines < 1)
          return;

     /* Draw the overlay border and backing */
     fprintf(stderr, "IH_RenderOverlay_Dialog(): shade the area\n");
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_DIALOG_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_DIALOG_OFFSET_Y * 2),
                  NULL);
     IH_FrameArea(overlay->position.x, overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_DIALOG_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_DIALOG_OFFSET_Y * 2),
                  NULL);

     /* Draw the prompt */
     fprintf(stderr, "IH_RenderOverlay_Dialog(): draw the prompt\n");
     for(i = 0; i < overlay->gfx.dialog.n_lines; i++)
     {
          ihFontPos       pos;

          /* Set the line's position */
          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = overlay->position.x + IH_OVERLAY_DIALOG_OFFSET_X;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel =
              overlay->position.y + IH_OVERLAY_DIALOG_OFFSET_Y +
              (i * IH_GetFontHeight(IH_FONT_LARGE));

          /* Yellow */
          IH_AttrToColor(COLOR_YELLOW, &color);

          /* Draw the line */
          IH_RenderText(IH_FONT_LARGE, overlay->gfx.dialog.lines[i], &pos,
                        &color, 0, NULL);
     }

     /* Draw the buffer text */
     fprintf(stderr,
             "IH_RenderOverlay_Dialog(): check if there's an input buffer\n");
     if(overlay->buffer[1])
     {
          SDL_Rect        rect;
          char           *buf_s;

          fprintf(stderr,
                  "IH_RenderOverlay_Dialog(): draw the input buffer\n");
          /* Backfill the buffer area with black, otherwise the
           * text will hardly be readable.
           */
          rect.x = overlay->position.x + IH_OVERLAY_DIALOG_OFFSET_X;
          rect.y =
              overlay->position.y + IH_OVERLAY_DIALOG_OFFSET_Y +
              (overlay->gfx.dialog.n_lines *
               IH_GetFontHeight(IH_FONT_LARGE));
          rect.w = overlay->position.w;
          rect.h = IH_GetFontHeight(IH_FONT_NORMAL);

          IH_AttrToColor(COLOR_DARK, &color);
          IH_FillArea(&rect, &color);

          /* Draw the buffer text, so that it fits.
           */
          buf_s = overlay->buffer[1];
          while(1)
          {
               int             width;

               width = IH_GetTextWidth(IH_FONT_NORMAL, buf_s);
               if(!width)
                    break;

               /* Check if text will fit.
                */
               if(width <=
                  overlay->position.w - (IH_OVERLAY_DIALOG_OFFSET_X * 2))
               {
                    ihColor         color;
                    ihFontPos       pos;

                    /* Set the line's position */
                    pos.x.type = IH_POSITION_TYPE_PIXEL;
                    pos.x.pixel =
                        overlay->position.x + IH_OVERLAY_DIALOG_OFFSET_X;
                    pos.y.type = IH_POSITION_TYPE_PIXEL;
                    pos.y.pixel =
                        overlay->position.y + IH_OVERLAY_DIALOG_OFFSET_Y +
                        (overlay->gfx.dialog.n_lines *
                         IH_GetFontHeight(IH_FONT_LARGE));

                    /* White */
                    IH_AttrToColor(COLOR_WHITE, &color);

                    IH_RenderText(IH_FONT_NORMAL, buf_s, &pos, &color,
                                  0, NULL);

                    /* Draw a "cursor" */
                    IH_DrawLine(overlay->position.x +
                                IH_OVERLAY_DIALOG_OFFSET_X + width + 1,
                                overlay->position.y +
                                IH_OVERLAY_DIALOG_OFFSET_Y +
                                (overlay->gfx.dialog.n_lines *
                                 IH_GetFontHeight(IH_FONT_LARGE)),
                                overlay->position.x +
                                IH_OVERLAY_DIALOG_OFFSET_X + width + 1,
                                overlay->position.y +
                                IH_OVERLAY_DIALOG_OFFSET_Y +
                                ((overlay->gfx.dialog.n_lines *
                                  IH_GetFontHeight(IH_FONT_LARGE)) +
                                 IH_GetFontHeight(IH_FONT_NORMAL)),
                                &color);

                    break;
               }

               buf_s++;
          }
     }
     fprintf(stderr, "IH_RenderOverlay_Dialog(): return\n");
}

void
IH_ReleaseOverlay_Dialog(Overlay * overlay)
{
     if(!overlay)
          return;

     if(overlay->gfx.dialog.prompt)
          rnfree(overlay->gfx.dialog.prompt);
}
