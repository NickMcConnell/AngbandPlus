
/* $Id: play.c,v 1.3 2003/04/07 00:27:13 cipher Exp $ */

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
#include "ipc.h"
#include "displays/iso/scene/play.h"
#include "displays/iso/tile.h"
#include "displays/iso/icon.h"
#include "displays/iso/misc.h"
#include "displays/iso/overlay.h"
#include "displays/iso/text.h"

void
IH_InitScene_Play(void)
{
     ih.pointer = IH_POINTER_STANDARD;

     /* Set default video modes.
      */
     ih.display_flags = SDL_HWSURFACE | SDL_DOUBLEBUF | SDL_ASYNCBLIT;
     if(ih.is_fullscreen)
          ih.display_flags |= SDL_FULLSCREEN;
     /* if(ih.hw_accel)
      * ih.display_flags |= SDL_OPENGL; */

     ih.screen = SDL_SetVideoMode(ih.desired_display_width,
                                  ih.desired_display_height,
                                  ih.display_depth, ih.display_flags);
     if(ih.screen == NULL)
     {
          fprintf(stderr,
                  "Couldn't set %dx%dx%d video mode: %s\n",
                  ih.display_width, ih.display_height, ih.display_depth,
                  SDL_GetError());
          SDL_Quit();
          exit(2);
     }

     ih.playing = TRUE;
     ih.display_width = ih.desired_display_width;
     ih.display_height = ih.desired_display_height;

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
          IH_RenderTiles();
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
