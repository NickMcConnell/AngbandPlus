
/* $Id: thread.c,v 1.12 2003/04/06 15:21:34 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL_thread.h"

/* Internal headers */
#include "ironhells.h"
#include "thread.h"
#include "path.h"
#include "displays/iso/display.h"
#include "displays/iso/scene.h"
#include "displays/iso/icon.h"
#include "displays/iso/tile.h"
#include "displays/iso/misc.h"
#include "platform/platform.h"

int
IH_GameThread(void *data)
{
     bool            new_game = TRUE;

     IH_ISO_Init();
     // IH_GL_Init();

     /* Hack - force some features to be on.
      */
     use_graphics = TRUE;
     arg_graphics = GRAPHICS_DAVID_GERVAIS;
     arg_sound = TRUE;

     IH_SetLoadMessage(NULL);

     /* This loop waits for the playing flag to be set by the drawing
      * thread, to let us know that the user has either elected to create
      * a new character, or wants to load one.
      */
     fprintf(stderr, "IH_GameThread(): Entering game loop.\n");
     for(;;)
     {
          bool            playing = FALSE;

          if(!SDL_SemWait(ih.sem.talk))
          {
               playing = ih.playing;
               new_game = ih.new_game;

               SDL_SemPost(ih.sem.talk);
          }

          if(playing)
               break;
     }

     /* Start the game.
      */
     fprintf(stderr, "IH_GameThread(): play_game\n");
     play_game(new_game);
     fprintf(stderr, "IH_GameThread(): play_game exited\n");

     ih.done = TRUE;

     if(!SDL_SemWait(ih.sem.talk))
     {
#if 0
          IH_SDLISO_Cleanup();
#endif

          fprintf(stderr, "IH_GameThread(): cleanup_angband\n");
          cleanup_angband();

          ih.thread_done = TRUE;

          SDL_SemPost(ih.sem.talk);
     }

     fprintf(stderr, "IH_GameThread(): return 0\n");
     return 0;
}

bool
IH_InitSemaphores(void)
{
     ih.sem.msg = SDL_CreateSemaphore(1);
     if(!ih.sem.msg)
          return FALSE;

     ih.sem.map = SDL_CreateSemaphore(1);
     if(!ih.sem.map)
          return FALSE;

     ih.sem.player = SDL_CreateSemaphore(1);
     if(!ih.sem.msg)
          return FALSE;

     ih.sem.talk = SDL_CreateSemaphore(1);
     if(!ih.sem.talk)
          return FALSE;

     ih.sem.scene = SDL_CreateSemaphore(1);
     if(!ih.sem.scene)
          return FALSE;

     ih.sem.overlay = SDL_CreateSemaphore(1);
     if(!ih.sem.overlay)
          return FALSE;

     return TRUE;
}

void
IH_DestroySemaphores(void)
{
     if(ih.sem.msg)
          SDL_DestroySemaphore(ih.sem.msg);
     ih.sem.msg = NULL;

     if(ih.sem.map)
          SDL_DestroySemaphore(ih.sem.map);
     ih.sem.map = NULL;

     if(ih.sem.player)
          SDL_DestroySemaphore(ih.sem.player);
     ih.sem.player = NULL;

     if(ih.sem.talk)
          SDL_DestroySemaphore(ih.sem.talk);
     ih.sem.talk = NULL;

     if(ih.sem.scene)
          SDL_DestroySemaphore(ih.sem.scene);
     ih.sem.scene = NULL;

     if(ih.sem.overlay)
          SDL_DestroySemaphore(ih.sem.overlay);
     ih.sem.overlay = NULL;
}
