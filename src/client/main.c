
/* $Id: main.c,v 1.22 2003/04/06 15:21:34 cipher Exp $ */

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
#include "SDL_draw.h"

#define IH_MAIN

/* Internal headers */
#include "ironhells.h"
#include "angband/z-disp.h"
#include "ipc.h"
#include "list.h"
#include "prefs.h"
#include "thread.h"
#include "platform/platform.h"
#include "displays/iso/display.h"
#include "displays/iso/icon.h"
#include "displays/iso/render.h"
#include "displays/iso/misc.h"
#include "displays/iso/overlay.h"

struct IronHells ih;

static void
IH_Cleanup(void)
{
     if(ih.game_thread)
          SDL_KillThread(ih.game_thread);

     IH_DestroySemaphores();

     IH_FreeFonts();

#if 0
     IH_FreeImages();
#endif

     SDL_ShowCursor(TRUE);
}

static void
IH_InitSetuid(void)
{
#ifdef SET_UID
     /* Default permissions on files */
     (void) umask(022);

# ifdef SECURE
     /* Authenticate */
     Authenticate();
# endif /* SECURE */

     /* Get the user id (?) */
     player_uid = getuid();

#ifdef VMS
     /* Mega-Hack -- Factor group id */
     player_uid += (getgid() * 1000);
#endif /* VMS */

# ifdef SAFE_SETUID

#  if defined(HAVE_SETEGID) || defined(SAFE_SETUID_POSIX)
     /* Save some info for later */
     player_euid = geteuid();
     player_egid = getegid();

#  endif /* defined(HAVE_SETEGID) || defined(SAFE_SETUID_POSIX) */

#  if 0                         /* XXX XXX XXX */
     /* Redundant setting necessary in case root is running the game */
     /* If not root or game not setuid the following two calls do nothing */
     if(setgid(getegid()) != 0)
     {
          quit("setgid(): cannot set permissions correctly!");
     }

     if(setuid(geteuid()) != 0)
     {
          quit("setuid(): cannot set permissions correctly!");
     }
#  endif /* 0 */

# endif /* SAFE_SETUID */

#endif /* SET_UID */

     /* Drop permissions */
     safe_setuid_drop();

#ifdef SET_UID
     /* Initialize the "time" checker */
     if(check_time_init() || check_time())
     {
          quit("The gates to Angband are closed (bad time).");
     }

     /* Initialize the "load" checker */
     if(check_load_init() || check_load())
     {
          quit("The gates to Angband are closed (bad load).");
     }

     /* Get the "user name" as a default player name */
     user_name(op_ptr->full_name, sizeof(op_ptr->full_name), player_uid);

#ifdef PRIVATE_USER_PATH
     /* Create a directory for the users files. */
     IH_CreateConfigDir();
#endif /* PRIVATE_USER_PATH */

#endif /* SET_UID */
}

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
     ih.desired_display_width = 1024;
     ih.desired_display_height = 768;
     ih.display_depth = 24;
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
     ih.display_flags = SDL_HWSURFACE | SDL_DOUBLEBUF | SDL_ASYNCBLIT;
     if(ih.is_fullscreen)
          ih.display_flags |= SDL_FULLSCREEN;
     /* if(ih.hw_accel)
      * ih.display_flags |= SDL_OPENGL; */
#ifdef DEBUG
     fprintf(stderr, "requested display flags = 0x%x\n", ih.display_flags);
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
     atexit(TTF_Quit);

     /* Turn on Unicode translation.
      */
     SDL_EnableUNICODE(1);

     IH_InitDisplays();

     ih.scene = SCENE_SPLASH;

     /* Set video mode */
     ih.screen = SDL_SetVideoMode(800, 600,
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
#ifdef DEBUG
     fprintf(stderr, "actual display flags = 0x%x\n", ih.screen->flags);
#endif
     ih.display_width = 800;
     ih.display_height = 600;

     ih.load_message = NULL;

     ih.err_shown = FALSE;
     ih.err_message = NULL;

     /* Initialize SDL_draw.
      */
     Draw_Init();

#ifdef DEBUG
     fprintf(stderr, "Initializing font.\n");
#endif
     if(rc = IH_InitFonts())
     {
          fprintf(stderr, "Unable to initialize fonts!\n");
          SDL_Quit();
          exit(rc);
     }

     ih.scene = SCENE_INTRO;

#ifdef DEBUG
     fprintf(stderr, "Initializing images.\n");
#endif
     IH_LoadImages();

     if(!IH_InitSemaphores())
     {
          fprintf(stderr,
                  "Unable to create semaphore: %s!\n", SDL_GetError());
          SDL_Quit();
          exit(2);
     }

     IH_InitOverlays();

     ih.thread_done = FALSE;
     ih.game_thread = SDL_CreateThread(IH_GameThread, NULL);
     if(!ih.game_thread)
     {
          IH_Cleanup();

          fprintf(stderr,
                  "Unable to create thread: %s!\n", SDL_GetError());
          SDL_Quit();
          exit(2);
     }

#ifdef DEBUG
     fprintf(stderr, "main: Entering main loop.\n");
#endif
     ih.done = 0;
     while(!ih.done)
     {
          SDL_Event       event;

#ifdef DEBUG
          fprintf(stderr, "main: IH_InitScene\n");
#endif
          IH_Scene_Init();

#ifdef DEBUG
          fprintf(stderr, "main: IH_RenderScene\n");
#endif
          IH_Scene_Render();

          /* Check for events */
#ifdef DEBUG
          fprintf(stderr, "main: Check for events.\n");
#endif
          while(SDL_PollEvent(&event))
          {
               /* Do some global event processing.
                */
#ifdef DEBUG
               fprintf(stderr, "main: Process an event.\n");
#endif
               switch (event.type)
               {
                    case SDL_MOUSEMOTION:
#ifdef DEBUG
                         fprintf(stderr, "x = %d, y = %d\n",
                                 event.motion.x, event.motion.y);
#endif
                         ih.mouse_x = event.motion.x;
                         ih.mouse_y = event.motion.y;
                         break;

                    case SDL_KEYDOWN:
                         if(event.key.keysym.sym != SDLK_F10)
                              break;
                    case SDL_QUIT:
                         ih.done = 1;
                         break;
               }

#ifdef DEBUG
               fprintf(stderr, "main: Check for quit.\n");
#endif
               if(ih.done)
                    break;

               /* Do scene-specific event processing.
                */
#ifdef DEBUG
               fprintf(stderr, "main: IH_ProcessScene\n");
#endif
               IH_Scene_Process(&event);
          }
     }

     /* Wait for the thread to finish.
      */
#ifdef DEBUG
     fprintf(stderr, "main: wait for thread to finish\n");
#endif
     for(;;)
     {
          bool            thread_done = FALSE;

          if(!SDL_SemWait(ih.sem.talk))
          {
               thread_done = ih.thread_done;

               SDL_SemPost(ih.sem.talk);
          }

          if(thread_done)
               break;
     }

#ifdef DEBUG
     fprintf(stderr, "main: IH_Cleanup\n");
#endif
     IH_Cleanup();

     /* Clean up the SDL library */
#ifdef DEBUG
     fprintf(stderr, "main: SDL_Quit\n");
#endif
     TTF_Quit();
     SDL_Quit();

#ifdef DEBUG
     fprintf(stderr, "main: exit\n");
#endif
     return (0);
}
