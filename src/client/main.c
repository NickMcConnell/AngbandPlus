/* File: main.c */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "SDL.h"
#include "SDL_thread.h"
#include "SDL_draw.h"

#define IH_MAIN

#include "ironhells.h"
#include "ipc.h"
#include "init.h"
#include "list.h"
#include "file.h"
#include "prefs.h"
#include "thread.h"
#include "term.h"
#include "sdl/scene.h"
#include "sdl/render.h"
#include "sdl/setup.h"
#include "sdl/render/icon.h"
#include "sdl/render/misc.h"

struct IronHells ih;

static void IH_Cleanup(void)
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

static void IH_InitSetuid(void)
{
#ifdef SET_UID
     /* Default permissions on files */
     (void)umask(022);

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

#  if 0 /* XXX XXX XXX */
     /* Redundant setting necessary in case root is running the game */
     /* If not root or game not setuid the following two calls do nothing */
     if (setgid(getegid()) != 0)
     {
          quit("setgid(): cannot set permissions correctly!");
     }

     if (setuid(geteuid()) != 0)
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
     if (check_time_init() || check_time())
     {
          quit("The gates to Angband are closed (bad time).");
     }
     
     /* Initialize the "load" checker */
     if (check_load_init() || check_load())
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
int main(int argc, char *argv[])
{
     cptr path_lib;
     int rc;
     Uint32 initflags = SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_NOPARACHUTE;

#if 1 // FIXME
      //     ih.is_fullscreen = TRUE;
     ih.desired_display_width = 1024;
     ih.desired_display_height = 768;
     ih.display_depth = 24;
     ih.icon_size = IH_ICON_SIZE_SMALL;
     ih.pointer = IH_POINTER_STANDARD;
#endif

	/* Do some basic initialization.
	 */

     /* Save the "program name" XXX XXX XXX */
     argv0 = argv[0];

#ifdef USE_286
     /* Attempt to use XMS (or EMS) memory for swap space */
     if (_OvrInitExt(0L, 0L))
     {
          _OvrInitEms(0, 0, 64);
     }
#endif /* USE_286 */

     IH_InitSetuid();

     /* Set default video modes.
      */
     ih.display_flags = SDL_HWSURFACE | SDL_DOUBLEBUF | SDL_ASYNCBLIT;
     if(ih.is_fullscreen)
          ih.display_flags |= SDL_FULLSCREEN;
     fprintf(stderr, "requested display flags = 0x%x\n", ih.display_flags);

     /* Get user preferences, such as display mode, etc.
      */
     IH_GetPrefs();

     /* Catch nasty signals.
      */
     signals_init();

     IH_InitTerm();
     
     /* Initialize the SDL library */
     if(SDL_Init(initflags) < 0)
     {
         fprintf(stderr,
                 "Couldn't initialize SDL: %s\n",
                 SDL_GetError());
         exit(1);
     }

     ih.scene = IH_SCENE_SPLASH;

     /* Set video mode */
     ih.screen = SDL_SetVideoMode(800, 600,
                                  ih.display_depth,
                                  ih.display_flags);
     if(ih.screen == NULL)
     {
         fprintf(stderr,
                 "Couldn't set %dx%dx%d video mode: %s\n",
                 ih.display_width, ih.display_height, ih.display_depth,
                 SDL_GetError());
         SDL_Quit();
         exit(2);
     }
     fprintf(stderr, "actual display flags = 0x%x\n", ih.screen->flags);
#if 0
     /* HACK - Set the source alpha on the main screen for hardware
      * accelerated displays.
      */
     if(ih.screen->flags & SDL_HWSURFACE)
          SDL_SetAlpha(ih.screen, SDL_SRCALPHA, 127);
#endif
     ih.display_width = 800;
     ih.display_height = 600;

     ih.load_message = NULL;

     ih.err_shown = FALSE;
     ih.err_message = NULL;

     /* Initialize SDL_draw.
      */
     Draw_Init();
     
     fprintf(stderr, "Initializing lists.\n");
     IH_ListInit(&ih.icons);
     IH_ListInit(&ih.misc);
     IH_ListInit(&ih.tiles);

     fprintf(stderr, "Initializing font.\n");
     if(rc = IH_InitFonts())
     {
          SDL_Quit();
          exit(rc);
     }

     ih.scene = IH_SCENE_INTRO;

     fprintf(stderr, "Initializing images.\n");
     IH_LoadImages();

     if(!IH_InitSemaphores())
     {
          fprintf(stderr,
                  "Unable to create semaphore: %s!\n",
                  SDL_GetError());
          SDL_Quit();
          exit(2);
     }
     
	ih.game_thread = SDL_CreateThread(IH_GameThread, NULL);
	if(!ih.game_thread)
	{
          IH_Cleanup();
          
		fprintf(stderr,
                  "Unable to create thread: %s!\n",
                  SDL_GetError());
		SDL_Quit();
		exit(2);
	}

     fprintf(stderr, "Entering main loop.\n");
     ih.done = 0;
     while(!ih.done)
     {
          SDL_Event event;

          IH_InitScene();

          IH_RenderScene();
          
          /* Check for events */
          while(SDL_PollEvent(&event))
          {
               /* Do some global event processing.
                */
               switch(event.type)
               {
                    case SDL_MOUSEMOTION:
#ifdef DEBUG
                         fprintf(stderr, "x = %d, y = %d\n", event.motion.x, event.motion.y);
#endif
                         ih.mouse_x = event.motion.x;
                         ih.mouse_y = event.motion.y;
                         break;

                    case SDL_KEYDOWN:
                         if(event.key.keysym.sym != SDLK_ESCAPE)
                              break;
                    case SDL_QUIT:
                         ih.done = 1;
                         break;
               }

               if(ih.done)
                    break;

               /* Do scene-specific event processing.
                */
               IH_ProcessScene(&event);
          }
     }

     IH_Cleanup();
     
     /* Clean up the SDL library */
     SDL_Quit();

     return(0);
}

