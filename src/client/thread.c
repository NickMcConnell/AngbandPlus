
/* $Id: thread.c,v 1.17 2003/04/18 21:45:09 cipher Exp $ */

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
#include "angband/angband.h"
#include "ironhells.h"
#include "thread.h"
#include "path.h"
#include "platform/platform.h"

int
IH_GameThread(void *data)
{
     bool            new_game = TRUE;

     fprintf(stderr, "IH_GameThread()\n");

     /* Hack - force some features to be on.
      */
     fprintf(stderr, "IH_GameThread(): force some features\n");
     use_graphics = TRUE;
     arg_graphics = GRAPHICS_DAVID_GERVAIS;
     use_sound = TRUE;
     arg_sound = TRUE;

     fprintf(stderr, "IH_GameThread(): set load message\n");
     IH_SetLoadMessage(NULL);

     /* This loop waits for the playing flag to be set by the drawing
      * thread, to let us know that the user has either elected to create
      * a new character, or wants to load one.
      */
     fprintf(stderr, "IH_GameThread(): wait for flags\n");
     for(;;)
     {
          bool            playing = FALSE;

// fprintf(stderr, "IH_GameThread(): get semaphore\n");
          if(!SDL_SemWait(ih.ipc.sem.talk))
          {
// fprintf(stderr, "IH_GameThread(): get playing and new_game\n");
               playing = ih.playing;
               new_game = ih.new_game;

// fprintf(stderr, "IH_GameThread(): free semaphore\n");
               SDL_SemPost(ih.ipc.sem.talk);
          }

// fprintf(stderr, "IH_GameThread(): check playing\n");
          if(playing)
               break;
     }

     /* Start the game.
      */
     fprintf(stderr, "IH_GameThread(): play_game\n");
     play_game(new_game);

     fprintf(stderr, "IH_GameThread(): get semaphore\n");
     if(!SDL_SemWait(ih.ipc.sem.talk))
     {
          fprintf(stderr, "IH_GameThread(): cleanup_angband\n");
          cleanup_angband();

          fprintf(stderr, "IH_GameThread(): set ih.done to TRUE\n");
          ih.done = TRUE;

          fprintf(stderr, "IH_GameThread(): free the semaphore\n");
          SDL_SemPost(ih.ipc.sem.talk);
     }

     fprintf(stderr, "IH_GameThread(): return 0\n");
     return 0;
}

bool
IH_InitSemaphores(void)
{
     ih.ipc.sem.msg = SDL_CreateSemaphore(1);
     if(!ih.ipc.sem.msg)
          return FALSE;

     ih.ipc.sem.map = SDL_CreateSemaphore(1);
     if(!ih.ipc.sem.map)
          return FALSE;

     ih.ipc.sem.player = SDL_CreateSemaphore(1);
     if(!ih.ipc.sem.msg)
          return FALSE;

     ih.ipc.sem.talk = SDL_CreateSemaphore(1);
     if(!ih.ipc.sem.talk)
          return FALSE;

     ih.ipc.sem.scene = SDL_CreateSemaphore(1);
     if(!ih.ipc.sem.scene)
          return FALSE;

     ih.ipc.sem.overlay = SDL_CreateSemaphore(1);
     if(!ih.ipc.sem.overlay)
          return FALSE;

     return TRUE;
}

void
IH_DestroySemaphores(void)
{
     if(ih.ipc.sem.msg)
          SDL_DestroySemaphore(ih.ipc.sem.msg);
     ih.ipc.sem.msg = NULL;

     if(ih.ipc.sem.map)
          SDL_DestroySemaphore(ih.ipc.sem.map);
     ih.ipc.sem.map = NULL;

     if(ih.ipc.sem.player)
          SDL_DestroySemaphore(ih.ipc.sem.player);
     ih.ipc.sem.player = NULL;

     if(ih.ipc.sem.talk)
          SDL_DestroySemaphore(ih.ipc.sem.talk);
     ih.ipc.sem.talk = NULL;

     if(ih.ipc.sem.scene)
          SDL_DestroySemaphore(ih.ipc.sem.scene);
     ih.ipc.sem.scene = NULL;

     if(ih.ipc.sem.overlay)
          SDL_DestroySemaphore(ih.ipc.sem.overlay);
     ih.ipc.sem.overlay = NULL;
}
