
/* $Id: options.c,v 1.4 2003/04/20 05:20:58 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"
#include "SDL_draw.h"

/* Internal headers */
#include "ironhells.h"
#include "overlay.h"
#include "render/text.h"

struct option_page_name
{
     char            ch;
     cptr            name;
};

static struct option_page_name option_page_names[] = {
     {'1', "User Interface"},
     {'2', "Disturbance"},
     {'3', "Game-play"},
     {'4', "Efficiency"},
     {'5', "Display"},
     {'6', "Birth"},
     {'7', "Cheat"},
     {'L', "Load a user pref file"},
     {'A', "Append options to a file"},
     {'D', "Base delay factor"},
     {'H', "Hitpoint warning"},
     {-1, NULL}
};

void
IH_RenderOverlay_Options(Overlay * overlay)
{
     ihColor         color;
     ihFontPos       pos;
     char            buf[100];
     int             i, n = 0;
     int             opt[OPT_PAGE_PER];
     int             fhl, fhn;
     int             page = 0, sel = 0;

     if(!overlay)
          return;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          page = overlay->var[0];
          sel = overlay->var[1];

          SDL_SemPost(ih.ipc.sem.overlay);
     }

     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_OPTIONS_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_OPTIONS_OFFSET_Y * 2),
                  NULL);
     IH_FrameArea(overlay->position.x, overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_OPTIONS_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_OPTIONS_OFFSET_Y * 2),
                  NULL);

     fhl = IH_GetFontHeight(IH_FONT_LARGE);
     fhn = IH_GetFontHeight(IH_FONT_NORMAL);

     if(page == -1)
     {
          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.y.type = IH_POSITION_TYPE_PIXEL;

          for(i = 0; option_page_names[i].ch != -1; i++)
          {
               sprintf(buf, "(%c) %s", option_page_names[i].ch,
                       option_page_names[i].name);
               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_OPTIONS_OFFSET_X +
                   (overlay->position.w / 2) -
                   (IH_GetTextWidth(IH_FONT_NORMAL, buf) / 2);
               pos.y.pixel =
                   overlay->position.y + IH_OVERLAY_OPTIONS_OFFSET_Y +
                   fhl + 2 + (i * fhn);

               IH_AttrToColor(COLOR_WHITE, &color);

               IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, NULL);
          }

          return;
     }

     /* Scan the options */
     for(i = 0; i < OPT_PAGE_PER; i++)
     {
          /* Collect options on this "page" */
          if(option_page[page][i] != OPT_NONE)
          {
               opt[n++] = option_page[page][i];
          }
     }

     /* Page title */
     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.x.pixel =
         overlay->position.x + IH_OVERLAY_OPTIONS_OFFSET_X +
         (overlay->position.w / 2) -
         (IH_GetTextWidth(IH_FONT_NORMAL, option_page_names[page].name) /
          2);
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel = overlay->position.y + IH_OVERLAY_OPTIONS_OFFSET_Y;

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE, option_page_names[page].name, &pos,
                   &color, 0, NULL);

     /* Display the options */
     for(i = 0; i < n; i++)
     {
          ihColor         color;
          ihFontPos       pos;
          byte            a = COLOR_WHITE;

          /* Color current option */
          if(i == sel)
               a = COLOR_L_BLUE;

          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = overlay->position.x + IH_OVERLAY_OPTIONS_OFFSET_X;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel =
              overlay->position.y + IH_OVERLAY_OPTIONS_OFFSET_Y +
              (i * (fhn + 2)) + (fhl * 2);

          IH_AttrToColor(a, &color);

          /* Display the option description */
          IH_RenderText(IH_FONT_NORMAL, option_desc[opt[i]], &pos, &color,
                        0, NULL);

          /* Display the option value */
          pos.x.pixel =
              overlay->position.x + IH_OVERLAY_OPTIONS_OFFSET_X + 300;
          IH_RenderText(IH_FONT_NORMAL, op_ptr->opt[opt[i]] ? "yes" : "no",
                        &pos, &color, 0, NULL);

          /* Display the option text */
          pos.x.pixel =
              overlay->position.x + IH_OVERLAY_OPTIONS_OFFSET_X + 350;
          sprintf(buf, "(%s)", option_text[opt[i]]);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, NULL);
     }
}

void
IH_ReleaseOverlay_Options(Overlay * overlay)
{
     if(!overlay)
          return;
}
