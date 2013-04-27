
/* $Id: title.c,v 1.4 2003/04/07 20:53:57 cipher Exp $ */

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

/* Internal headers */
#include "ironhells.h"
#include "path.h"
#include "platform/platform.h"
#include "displays/iso/scene.h"
#include "displays/iso/render.h"
#include "displays/iso/misc.h"
#include "displays/iso/scene/title.h"
#include "displays/iso/text.h"
#include "displays/iso/icon.h"

void
IH_InitScene_Title(void)
{
     ih.pointer = IH_POINTER_NONE;

}

void
IH_ProcessScene_Title(SDL_Event * event)
{
     int             stage;

     if(!event)
          return;

     if(!SDL_SemWait(ih.sem.scene))
     {
          stage = ih.stage;

          SDL_SemPost(ih.sem.scene);
     }

     switch (stage)
     {
          case SCENE_TITLE_STAGE_COMPLETE:
               switch (event->type)
               {
                    case SDL_MOUSEBUTTONDOWN:
                    case SDL_KEYDOWN:
                         IH_SetScene(SCENE_SELECT_CHARACTER);
                         // IH_SetScene(SCENE_PLAY);
                         break;
               }
               break;
     }
}

void
IH_RenderScene_Title(void)
{
     ihFontPos       pos;
     SDL_Rect        srect, drect;
     SDL_Color       color;
     int             center_x, center_y;

#ifdef DEBUG
     fprintf(stderr, "IH_RenderScene_Title()\n");
#endif

     if(!ih.splash)
          return;
     if(!ih.screen)
          return;

#ifdef DEBUG
     fprintf(stderr, "Rendering title screen.\n");
#endif

     center_x = ih.display_width / 2;
     center_y = ih.display_height / 2;

     /* Set the source and destination rendering rectangles.
      */
     srect.x = 0;
     srect.y = 0;
     srect.w = ih.splash->w;
     srect.h = ih.splash->h;

     drect.x = 0;
     drect.y = 0;
     drect.w = ih.display_width;
     drect.h = ih.display_height;

#ifdef DEBUG
     fprintf(stderr, "Blitting splash image to screen.\n");
#endif
//     SDL_ScaleBlit(ih.splash, &srect, ih.screen, &drect);
     SDL_BlitSurface(ih.splash, &srect, ih.screen, &drect);

#if 0
     /* Render text.
      */
     font_pos.x.type = IH_POSITION_TYPE_CENTER;
//     font_pos.x.width = IH_GetTextWidth(IH_TEXT_TITLE_PROGRAM);
     font_pos.y.type = IH_POSITION_TYPE_PERCENT;
     font_pos.y.perc = .75;
     IH_RenderText(IH_TEXT_TITLE_PROGRAM, &font_pos, 255, 255, 255);

     font_pos.x.type = IH_POSITION_TYPE_CENTER;
//     font_pos.x.width = IH_GetTextWidth(IH_TEXT_TITLE_COPYRIGHT);
     font_pos.y.type = IH_POSITION_TYPE_PIXEL;
     font_pos.y.pixel += ih.font_size;
     IH_RenderText(IH_TEXT_TITLE_COPYRIGHT, &font_pos, 255, 255, 255);

     font_pos.x.type = IH_POSITION_TYPE_LEFT;
     font_pos.y.type = IH_POSITION_TYPE_BOTTOM;
     IH_RenderText(IH_TEXT_TITLE_VERSION, &font_pos, 255, 255, 255);
#endif

     if(ih.load_message)
     {
          pos.x.type = IH_POSITION_TYPE_ABS_CENTER;
          pos.y.type = IH_POSITION_TYPE_PERCENT;
          pos.y.perc = .5;
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        ih.load_message, &pos, color, NULL);
     }

#if 0
     if(!SDL_SemWait(ih.sem.scene))
     {
          stage = ih.stage;

          SDL_SemPost(ih.sem.scene);
     }

     switch (stage)
     {
          case SCENE_TITLE_STAGE_ICONS:
               IH_SetStage(SCENE_TITLE_STAGE_TILES);
               break;

          case SCENE_TITLE_STAGE_TILES:
               ih.stage = SCENE_TITLE_STAGE_MISC;
               break;

          case SCENE_TITLE_STAGE_MISC:
               ih.stage = SCENE_TITLE_STAGE_COMPLETE;
               break;
     }
#endif
}

void
IH_CleanupScene_Title(void)
{
}
