
/* $Id: render.c,v 1.5 2003/03/17 06:09:04 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "ironhells.h"
#include "sdl/scene.h"
#include "sdl/render/tile.h"
#include "sdl/render/icon.h"
#include "sdl/render/overlay.h"
#include "sdl/render/pointer.h"
#include "sdl/render/misc.h"
#include "sdl/scene/title.h"
#include "sdl/render.h"
#include "sdl/scene/selchar.h"
#include "sdl/scene/newchar.h"
#include "sdl/scene/splash.h"
#include "sdl/scene/intro.h"
#include "sdl/scene/mhost.h"
#include "sdl/scene/mjoin.h"
#include "sdl/scene/grave.h"
#include "sdl/scene/play.h"

void
IH_RenderScene(void)
{
     SDL_Rect        rect;

     fprintf(stderr, "IH_RenderScene()\n");

     SDL_ShowCursor(FALSE);

     /* Clear the drawing surface.
      */
     rect.x = 0;
     rect.y = 0;
     rect.w = ih.display_width;
     rect.h = ih.display_height;
     SDL_FillRect(ih.screen,
                  &rect, SDL_MapRGB(ih.screen->format, 0, 0, 0));

     fprintf(stderr,
             "IH_RenderScene(): Render proper scene (ih.scene = %d, ih.stage = %d)\n",
             ih.scene, ih.stage);
     switch (ih.scene)
     {
          case IH_SCENE_SPLASH:
               fprintf(stderr, "IH_RenderScene(): splash (%d)\n",
                       ih.scene);
               IH_RenderScene_Splash();
               break;

          case IH_SCENE_INTRO:
               fprintf(stderr, "IH_RenderScene(): intro (%d)\n", ih.scene);
               IH_RenderScene_Intro();
               break;

          case IH_SCENE_TITLE:
               fprintf(stderr, "IH_RenderScene(): title (%d)\n", ih.scene);
               IH_RenderScene_Title();
               break;

          case IH_SCENE_SELECT_CHARACTER:
               fprintf(stderr, "IH_RenderScene(): selchar (%d)\n",
                       ih.scene);
               IH_RenderScene_SelChar();
               break;

          case IH_SCENE_NEW_CHARACTER:
               fprintf(stderr, "IH_RenderScene(): newchar (%d)\n",
                       ih.scene);
               IH_RenderScene_NewChar();
               break;

          case IH_SCENE_MULTIPLAYER_HOST:
               fprintf(stderr, "IH_RenderScene(): mhost (%d)\n", ih.scene);
               IH_RenderScene_MultiHost();
               break;

          case IH_SCENE_MULTIPLAYER_JOIN:
               fprintf(stderr, "IH_RenderScene(): mjoin (%d)\n", ih.scene);
               IH_RenderScene_MultiJoin();
               break;

          case IH_SCENE_PLAY:
               fprintf(stderr, "IH_RenderScene(): play (%d)\n", ih.scene);
               IH_RenderScene_Play();
               break;

          case IH_SCENE_GRAVE:
               fprintf(stderr, "IH_RenderScene(): grave (%d)\n", ih.scene);
               IH_RenderScene_Grave();
               break;
     }

     IH_RenderPointer();

     SDL_Flip(ih.screen);
}
