
/* $Id: title.c,v 1.4 2003/04/18 21:45:46 cipher Exp $ */

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
#include "scene.h"
#include "scene/title.h"
#include "render/pointer.h"
#include "render/text.h"
#include "render/misc.h"

void
IH_InitScene_Title(void)
{
     ih.pointer = IH_POINTER_NONE;

}

void
IH_ProcessScene_Title(SDL_Event * event)
{
     int             stage = 0;

     fprintf(stderr, "IH_ProcessScene_Title()\n");

     if(!event)
          return;

     fprintf(stderr, "IH_ProcessScene_Title(): get semaphore\n");
     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          fprintf(stderr, "IH_ProcessScene_Title(): get stage\n");
          stage = ih.scene.stage;
          fprintf(stderr, "IH_ProcessScene_Title(): stage = %d\n", stage);

          fprintf(stderr, "IH_ProcessScene_Title(): release semaphore\n");
          SDL_SemPost(ih.ipc.sem.scene);
     }

     fprintf(stderr, "IH_ProcessScene_Title(): switch\n");
     switch (stage)
     {
          case SCENE_TITLE_STAGE_COMPLETE:
               fprintf(stderr,
                       "IH_ProcessScene_Title(): title stage complete\n");
               switch (event->type)
               {
                    case SDL_MOUSEBUTTONDOWN:
                    case SDL_KEYDOWN:
                         fprintf(stderr,
                                 "IH_ProcessScene_Title(): mouse or key event; set scene\n");
                         IH_SetScene(SCENE_SELECT_CHARACTER);
                         break;
               }
               break;
     }
     fprintf(stderr, "IH_ProcessScene_Title(): return\n");
}

void
IH_RenderScene_Title(void)
{
     ihFontPos       pos;
     ihColor         color;

     fprintf(stderr, "IH_RenderScene_Title()\n");

     if(!ih.display.screen)
          return;

     fprintf(stderr, "IH_RenderScene_Title(): render the title\n");
     IH_RenderTitle();

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

     if(ih.load_message[0])
     {
          fprintf(stderr, "IH_RenderScene_Title(): draw a load message\n");
          pos.x.type = IH_POSITION_TYPE_ABS_CENTER;
          pos.y.type = IH_POSITION_TYPE_PERCENT;
          pos.y.perc = .5;

          IH_AttrToColor(COLOR_L_WHITE, &color);

          IH_RenderText(IH_FONT_NORMAL,
                        ih.load_message, &pos, &color, 0, NULL);
     }

     fprintf(stderr, "IH_RenderScene_Title(): return\n");
}

void
IH_CleanupScene_Title(void)
{
}
