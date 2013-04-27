
/* $Id: main.c,v 1.28 2003/04/20 05:20:57 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Standard headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* SDL headers */
#include "SDL.h"
#include "SDL_thread.h"

#define IH_MAIN

/* Internal headers */
#include "ironhells.h"
#include "angband/z-disp.h"
#include "ipc.h"
#include "prefs.h"
#include "thread.h"
#include "engines.h"
#include "overlay.h"
#include "platform/platform.h"
#include "render/icon.h"
#include "render/pointer.h"

struct IronHells ih;

/* Main game code.
 */
int
main(int argc,
     char *argv[])
{
     int             rc;
     Uint32          initflags =
         SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_NOPARACHUTE;

     memset(&ih, 0, sizeof(ih));

     /* Set some defaults.
      */
//     ih.is_fullscreen = TRUE;
     ih.display.desired_width = 1024;
     ih.display.desired_height = 768;
     ih.display.depth = 24;
     ih.icon_size = IH_ICON_SIZE_SMALL;
     ih.pointer = IH_POINTER_STANDARD;
     ih.messages_shown = 10;

     /* Do some basic initialization.
      */

     /* Save the "program name" XXX XXX XXX */
     argv0 = argv[0];

#ifdef USE_286
     /* Attempt to use XMS (or EMS) memory for swap space */
     if(_OvrInitExt(0L, 0L))
     {
          _OvrInitEms(0, 0, 64);
     }
#endif /* USE_286 */

     IH_InitSetuid();

     /* Get user preferences, such as display mode, etc.
      */
     IH_GetPrefs();

     /* Set default video modes.
      */
     ih.display.flags = SDL_HWSURFACE | SDL_DOUBLEBUF | SDL_ASYNCBLIT;
     if(ih.display.is_fullscreen)
          ih.display.flags |= SDL_FULLSCREEN;
     /* if(ih.hw_accel)
      * ih.display.flags |= SDL_OPENGL; */
#ifdef DEBUG
     fprintf(stderr, "requested display flags = 0x%x\n", ih.display.flags);
#endif

     /* Catch nasty signals.
      */
     signals_init();

     /* Initialize the SDL library */
     if(SDL_Init(initflags) < 0)
     {
          fprintf(stderr, "Couldn't initialize SDL: %s\n", SDL_GetError());
          exit(1);
     }
     atexit(SDL_Quit);

     /* Turn on Unicode translation.
      */
     SDL_EnableUNICODE(1);

     /* Initialize the angband "displays."
      */
     IH_InitDisplays();

     /* Initialize the angband "game."
      */
     IH_InitAngband();

     /* Initialize the display engine.
      */
     rc = IH_Engines_Init();
     if(rc)
     {
          fprintf(stderr,
                  "Couldn't initialize display engine: error code %d\n",
                  rc);
          SDL_Quit();
          exit(2);
     }

     ih.scene.scene = SCENE_SPLASH;

     /* Tell the display engine to get itself ready.
      */
     rc = (*ih.display.setup_display_func) ();
     if(rc)
     {
          fprintf(stderr, "Couldn't setup display (error code %d)\n", rc);
          SDL_Quit();
          exit(2);
     }

     /* Set video mode */
     ih.display.screen = SDL_SetVideoMode(800, 600,
                                          ih.display.depth,
                                          ih.display.flags);
     if(ih.display.screen == NULL)
     {
          fprintf(stderr,
                  "Couldn't set %dx%dx%d video mode: %s\n",
                  ih.display.width, ih.display.height, ih.display.depth,
                  SDL_GetError());
          SDL_Quit();
          exit(2);
     }
#ifdef DEBUG
     fprintf(stderr, "actual display flags = 0x%x\n",
             ih.display.screen->flags);
#endif
     ih.display.width = 800;
     ih.display.height = 600;

     ih.load_message[0] = 0;

     ih.err_shown = FALSE;
     ih.err_message[0] = 0;

     /* Initialize semaphores.
      */
     fprintf(stderr, "main(): init semaphores\n");
     if(!IH_InitSemaphores())
     {
          fprintf(stderr,
                  "Unable to create semaphore: %s!\n", SDL_GetError());
          SDL_Quit();
          exit(2);
     }

     /* Setup the display engine.
      */
     fprintf(stderr, "main(): call display setup\n");
     (*ih.display.setup_func) ();
     fprintf(stderr, "main(): done display setup\n");

     /* Initialize overlays.
      */
     fprintf(stderr, "main(): init overlays\n");
     IH_InitOverlays();

     /* Start the game thread.
      */
     fprintf(stderr, "main(): initialize game thread\n");
     ih.ipc.game_thread = SDL_CreateThread(IH_GameThread, NULL);
     if(!ih.ipc.game_thread)
     {
          fprintf(stderr,
                  "Unable to create thread: %s!\n", SDL_GetError());

          goto shutdown;
     }

     /* Signal the start the intro video, if there is one.
      */
     fprintf(stderr, "main(): set scene (intro)\n");
     ih.scene.scene = SCENE_INTRO;

     /* FIXME -- do an intro movie or something...
      */

     /* Set the title scene.
      */
     fprintf(stderr, "main(): set scene (title)\n");
     ih.scene.scene = SCENE_TITLE;

     /* Enter the main game loop.
      */
     fprintf(stderr, "main(): game loop\n");
     ih.done = 0;
     while(!ih.done)
     {
          SDL_Event       event;

          /* Initialize the current scene.
           */
          IH_Scene_Init();
          
          /* Delay, to be nice to the system.
           */
          SDL_Delay(10);

          /* Draw the scene.
           */
          IH_Scene_Render();

          /* Check for events */
          while(SDL_PollEvent(&event))
          {
               /* Do some global event processing.
                */
               switch (event.type)
               {
                    case SDL_MOUSEMOTION:
#ifdef DEBUG
                         fprintf(stderr, "x = %d, y = %d\n",
                                 event.motion.x, event.motion.y);
#endif
                         ih.display.mouse_x = event.motion.x;
                         ih.display.mouse_y = event.motion.y;
                         break;

                    case SDL_KEYDOWN:
                         if(event.key.keysym.sym != SDLK_F10)
                              break;
                    case SDL_QUIT:
                         ih.done = 1;

                         SDL_KillThread(ih.ipc.game_thread);
                         break;
               }

               if(ih.done)
                    break;

               /* Do scene-specific event processing.
                */
               IH_Scene_Process(&event);
          }
     }

   shutdown:
     IH_Engines_Cleanup();

     IH_DestroySemaphores();

     SDL_ShowCursor(TRUE);

     /* Clean up the SDL library */
     SDL_Quit();

     return (0);
}
