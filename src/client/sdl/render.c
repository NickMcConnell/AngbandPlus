/* File: render.c */

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

void IH_RenderScene(void)
{
     SDL_Rect rect;

     SDL_ShowCursor(FALSE);
     
     /* Clear the drawing surface.
      */
     rect.x = 0;
     rect.y = 0;
     rect.w = ih.display_width;
     rect.h = ih.display_height;
     SDL_FillRect(ih.screen,
                  &rect,
                  SDL_MapRGB(ih.screen->format, 0, 0, 0));
     
     switch(ih.scene)
     {
          case IH_SCENE_SPLASH:
               IH_RenderScene_Splash();
               break;

          case IH_SCENE_INTRO:
               IH_RenderScene_Intro();
               break;
               
          case IH_SCENE_TITLE:
               IH_RenderScene_Title();
               break;
               
          case IH_SCENE_SELECT_CHARACTER:
               IH_RenderScene_SelChar();
               break;

          case IH_SCENE_NEW_CHARACTER:
               IH_RenderScene_NewChar();
               break;

          case IH_SCENE_MULTIPLAYER_HOST:
               IH_RenderScene_MultiHost();
               break;

          case IH_SCENE_MULTIPLAYER_JOIN:
               IH_RenderScene_MultiJoin();
               break;

          case IH_SCENE_PLAY:
               IH_RenderScene_Play();
               break;

          case IH_SCENE_GRAVE:
               IH_RenderScene_Grave();
               break;
          
     }

     IH_RenderPointer();

     SDL_Flip(ih.screen);
}


