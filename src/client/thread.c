
/* $Id: thread.c,v 1.7 2003/03/18 19:17:40 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL_thread.h"

#include "ironhells.h"
#include "thread.h"
#include "sdl/scene.h"
#include "sdl/render/icon.h"
#include "sdl/render/tile.h"
#include "sdl/render/misc.h"
#include "path.h"
#include "file.h"
#include "init.h"

int
IH_GameThread(void *data)
{
     char           *path_lib;
     bool            new_game = TRUE;

     IH_SetScene(IH_SCENE_TITLE);
     IH_SetStage(IH_SCENE_TITLE_STAGE_ICONS);
     IH_SetLoadMessage("Loading image data...");
     IH_LoadIcons();

     IH_SetScene(IH_SCENE_TITLE);
     IH_SetStage(IH_SCENE_TITLE_STAGE_TILES);
     IH_SetLoadMessage("Loading tile data...");
     IH_InitTiles();

     IH_SetScene(IH_SCENE_TITLE);
     IH_SetStage(IH_SCENE_TITLE_STAGE_MISC);

     IH_SetLoadMessage("Loading miscellaneous data...");
     IH_LoadLMX();
     IH_PositionMisc();

     IH_LoadPointers();

     /* Initialize */
     fprintf(stderr, "IH_GameThread(): Getting lib directory.\n");
     path_lib = IH_GetDataDir("lib/");

     fprintf(stderr, "IH_GameThread(): Initializing file paths...\n");
     IH_SetLoadMessage("Initializing file paths...");
     init_file_paths(path_lib);

     fprintf(stderr, "IH_GameThread(): Initializing angband...\n");
     IH_SetLoadMessage("Initializing angband...");
     init_angband();

     fprintf(stderr, "IH_GameThread(): Freeing path_lib.\n");
     rnfree(path_lib);

     IH_InitMisc();

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

#if 0
     IH_FreeMisc();
     IH_FreePointers();
     IH_FreeLMX();
     IH_FreeTiles();
     IH_FreeIcons();
#endif

     fprintf(stderr, "IH_GameThread(): cleanup_angband\n");
     cleanup_angband();

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
