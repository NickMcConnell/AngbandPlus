
/* $Id: play.c,v 1.4 2003/04/18 21:45:46 cipher Exp $ */

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
#include "ipc.h"
#include "overlay.h"
#include "scene/play.h"
#include "render/pointer.h"
#include "render/map.h"
#include "render/misc.h"
#include "render/icon.h"

void
IH_InitScene_Play(void)
{
     ih.pointer = IH_POINTER_STANDARD;

     /* Set default video modes.
      */
     ih.display.flags = SDL_DOUBLEBUF | SDL_ASYNCBLIT;
     if(ih.display.is_fullscreen)
          ih.display.flags |= SDL_FULLSCREEN;
     if(ih.display.is_hw_accel)
          ih.display.flags |= SDL_HWSURFACE;
     else
          ih.display.flags |= SDL_SWSURFACE;
     switch (ih.display.engine)
     {
#ifdef BUILD_ISO_ENGINE
          case IH_DISPLAY_ENGINE_ISO:
               break;
#endif
#ifdef BUILD_3D_ENGINE
          case IH_DISPLAY_ENGINE_3D:
               ih.display.flags |= SDL_OPENGL;
               break;
#endif
     }

     ih.display.screen = SDL_SetVideoMode(ih.display.desired_width,
                                          ih.display.desired_height,
                                          ih.display.depth,
                                          ih.display.flags);
     if(ih.display.screen == NULL)
     {
          fprintf(stderr,
                  "Couldn't set %dx%dx%d video mode: %s\n",
                  ih.display.desired_width, ih.display.desired_height,
                  ih.display.depth, SDL_GetError());
          SDL_Quit();
          exit(2);
     }

     ih.playing = TRUE;
     ih.display.width = ih.display.desired_width;
     ih.display.height = ih.display.desired_height;

     /* Reposition the icons.
      */
     IH_PositionIcons();
     IH_PositionMisc();
}

void
IH_ProcessScene_Play(SDL_Event * event)
{
     if(!event)
          return;

     /* Just pass on key events.
      */
     if(event->type == SDL_KEYDOWN)
     {
          IH_SetIPCEvent(event);
          return;
     }

     /* Process mouse events.
      */
}

void
IH_RenderScene_Play(void)
{
     if(!Disp_lock())
     {
          IH_DrawMap();

          IH_RenderIcons();
          IH_RenderMisc();
          IH_RenderOverlays();

          Disp_unlock();
     }

     IH_RenderError();
}

void
IH_CleanupScene_Play(void)
{
}
