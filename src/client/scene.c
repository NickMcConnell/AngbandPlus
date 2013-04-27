
/* $Id: scene.c,v 1.8 2003/04/20 05:20:57 cipher Exp $ */

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
#include "angband/angband.h"
#include "ironhells.h"
#include "path.h"
#include "platform/platform.h"
#include "render/misc.h"
#include "render/pointer.h"

/* Scene headers */
#include "scene/splash.h"
#include "scene/title.h"
#include "scene/intro.h"
#include "scene/selchar.h"
#include "scene/newchar.h"
#include "scene/mjoin.h"
#include "scene/mhost.h"
#include "scene/play.h"
#include "scene/grave.h"
#include "scene/video.h"

struct SceneFuncs
{
     int             id;
     void            (*init_func) (void);
     void            (*process_func) (SDL_Event * event);
     void            (*render_func) (void);
     void            (*cleanup_func) (void);
};

static struct SceneFuncs scenes[] = {
     {SCENE_SPLASH, IH_InitScene_Splash, IH_ProcessScene_Splash,
      IH_RenderScene_Splash, IH_CleanupScene_Splash},
     {SCENE_INTRO, IH_InitScene_Intro, IH_ProcessScene_Intro,
      IH_RenderScene_Intro, IH_CleanupScene_Intro},
     {SCENE_TITLE, IH_InitScene_Title, IH_ProcessScene_Title,
      IH_RenderScene_Title, IH_CleanupScene_Title},
     {SCENE_SELECT_CHARACTER, IH_InitScene_SelChar,
      IH_ProcessScene_SelChar, IH_RenderScene_SelChar,
      IH_CleanupScene_SelChar},
     {SCENE_SELECT_VIDEO, IH_InitScene_Video, IH_ProcessScene_Video,
      IH_RenderScene_Video, IH_CleanupScene_Video},
     {SCENE_NEW_CHARACTER, IH_InitScene_NewChar,
      IH_ProcessScene_NewChar, IH_RenderScene_NewChar,
      IH_CleanupScene_NewChar},
     {SCENE_MULTIPLAYER_HOST, IH_InitScene_MultiHost,
      IH_ProcessScene_MultiHost, IH_RenderScene_MultiHost,
      IH_CleanupScene_MultiHost},
     {SCENE_MULTIPLAYER_JOIN, IH_InitScene_MultiJoin,
      IH_ProcessScene_MultiJoin, IH_RenderScene_MultiJoin,
      IH_CleanupScene_MultiJoin},
     {SCENE_PLAY, IH_InitScene_Play, IH_ProcessScene_Play,
      IH_RenderScene_Play, IH_CleanupScene_Play},
     {SCENE_GRAVE, IH_InitScene_Grave, IH_ProcessScene_Grave,
      IH_RenderScene_Grave, IH_CleanupScene_Grave},
     {-1}
};

#ifdef DEBUG

/* Print modifier info */
static void
PrintModifiers(SDLMod mod)
{
     fprintf(stderr, "KEY: Modifers: ");

     /* If there are none then say so and return */
     if(mod == KMOD_NONE)
     {
          fprintf(stderr, "None\n");
          return;
     }

     /* Check for the presence of each SDLMod value */
     /* This looks messy, but there really isn't    */
     /* a clearer way.                              */
     if(mod & KMOD_NUM)
          fprintf(stderr, "NUMLOCK ");
     if(mod & KMOD_CAPS)
          fprintf(stderr, "CAPSLOCK ");
     if(mod & KMOD_LCTRL)
          fprintf(stderr, "LCTRL ");
     if(mod & KMOD_RCTRL)
          fprintf(stderr, "RCTRL ");
     if(mod & KMOD_RSHIFT)
          fprintf(stderr, "RSHIFT ");
     if(mod & KMOD_LSHIFT)
          fprintf(stderr, "LSHIFT ");
     if(mod & KMOD_RALT)
          fprintf(stderr, "RALT ");
     if(mod & KMOD_LALT)
          fprintf(stderr, "LALT ");
     if(mod & KMOD_CTRL)
          fprintf(stderr, "CTRL ");
     if(mod & KMOD_SHIFT)
          fprintf(stderr, "SHIFT ");
     if(mod & KMOD_ALT)
          fprintf(stderr, "ALT ");
     fprintf(stderr, "\n");
}

/* Print all information about a key event */
static void
PrintKeyInfo(SDL_KeyboardEvent * key)
{
     /* Is it a release or a press? */
     if(key->type == SDL_KEYUP)
          fprintf(stderr, "KEY: Release:- ");
     else
          fprintf(stderr, "KEY: Press:- ");

     /* Print the hardware scancode first */
     fprintf(stderr, "Scancode: 0x%02X", key->keysym.scancode);
     /* Print the name of the key */
     fprintf(stderr, ", Name: %s", SDL_GetKeyName(key->keysym.sym));
     /* We want to print the unicode info, but we need to make */
     /* sure its a press event first (remember, release events */
     /* don't have unicode info                                */
     if(key->type == SDL_KEYDOWN)
     {
          /* If the Unicode value is less than 0x80 then the    */
          /* unicode value can be used to get a printable       */
          /* representation of the key, using (char)unicode.    */
          fprintf(stderr, ", Unicode: ");
          if(key->keysym.unicode < 0x80 && key->keysym.unicode > 0)
          {
               fprintf(stderr, "%c (0x%04X)", (char) key->keysym.unicode,
                       key->keysym.unicode);
          }
          else
          {
               fprintf(stderr, "? (0x%04X)", key->keysym.unicode);
          }
     }
     fprintf(stderr, "\n");
     /* Print modifier info */
     PrintModifiers(key->keysym.mod);
}
#endif /* DEBUG */

static struct SceneFuncs *
scene_funcs(int scene_id)
{
     int             i;

     for(i = 0; scenes[i].id != -1; i++)
     {
          if(scenes[i].id == scene_id)
               return &scenes[i];
     }

     return NULL;
}

void
IH_Scene_Init(void)
{
     struct SceneFuncs *funcs;
     static void     (*cleanup_func) (void) = NULL;
     int             scene = 0;
     bool            dirty = FALSE;

#ifdef DEBUG
     fprintf(stderr, "IH_InitScene()\n");
#endif

     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          scene = ih.scene.scene;
          dirty = ih.scene.is_dirty;

          SDL_SemPost(ih.ipc.sem.scene);
     }

#ifdef DEBUG
     fprintf(stderr, "IH_InitScene(): Check if scene is dirty.\n");
#endif

     if(!dirty)
          return;

     /* If there's a cleanup function for the old scene, call it.
      */
     if(cleanup_func)
     {
          (*cleanup_func) ();

          /* Clear it, so we don't call it again.
           */
          cleanup_func = NULL;
     }

     /* Initialize the new scene.
      */
     funcs = scene_funcs(scene);
     if(!funcs)
          return;

     if(funcs->init_func)
     {
          (*funcs->init_func) ();

          cleanup_func = funcs->cleanup_func;
     }

     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          ih.scene.is_dirty = FALSE;

          SDL_SemPost(ih.ipc.sem.scene);
     }

#ifdef DEBUG
     fprintf(stderr, "IH_InitScene: return\n");
#endif
}

void
IH_Scene_Process(SDL_Event * event)
{
     struct SceneFuncs *funcs;
     int             scene = 0;

     fprintf(stderr, "IH_Scene_Process()\n");

     fprintf(stderr, "IH_Scene_Process(): check if scene is changing\n");
     if(ih.scene.is_changing)
     {
          fprintf(stderr, "IH_Scene_Process(): scene is changing\n");

          ih.scene.is_changing = FALSE;

          return;
     }

     fprintf(stderr, "IH_Scene_Process(): check event\n");
     if(!event)
          return;

#ifdef DEBUG
     switch (event->type)
     {
          case SDL_KEYDOWN:
          case SDL_KEYUP:
               PrintKeyInfo(&event->key);
               break;

          default:
               break;
     }
     fprintf(stderr, "scene = %d\n", ih.scene.scene);
#endif

     fprintf(stderr, "IH_Scene_Process(): get semaphore\n");
     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          fprintf(stderr, "IH_Scene_Process(): get scene\n");
          scene = ih.scene.scene;
          fprintf(stderr, "IH_Scene_Process(): scene = %d\n", scene);

          fprintf(stderr, "IH_Scene_Process(): release semaphore\n");
          SDL_SemPost(ih.ipc.sem.scene);
     }

     fprintf(stderr, "IH_Scene_Process(): get funcs for scene %d\n",
             scene);
     funcs = scene_funcs(scene);
     fprintf(stderr, "IH_Scene_Process(): check funcs\n");
     if(!funcs)
          return;

     fprintf(stderr, "IH_Scene_Process(): call process func\n");
     if(funcs->process_func)
          (*funcs->process_func) (event);
     fprintf(stderr, "IH_Scene_Process(): return\n");
}

void
IH_Scene_Render(void)
{
     struct SceneFuncs *funcs;
     int             scene = 0;

     fprintf(stderr, "IH_RenderScene()\n");

     SDL_ShowCursor(FALSE);

     /* Clear the drawing surface.
      */
     IH_ClearDrawingBuffer();

     fprintf(stderr,
             "IH_RenderScene(): Render proper scene (scene = %d, stage = %d)\n",
             ih.scene.scene, ih.scene.stage);

     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          scene = ih.scene.scene;

          SDL_SemPost(ih.ipc.sem.scene);
     }

     funcs = scene_funcs(scene);
     if(!funcs)
          return;

     if(funcs->render_func)
          (*funcs->render_func) ();

     IH_RenderPointer();

     IH_SwapBuffers();
}

void
IH_SetScene(int scene)
{
     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          ih.scene.scene = scene;
          ih.scene.is_changing = TRUE;
          ih.scene.is_dirty = TRUE;

          SDL_SemPost(ih.ipc.sem.scene);
     }
}

void
IH_SetStage(int stage)
{
	fprintf(stderr, "IH_SetStage()\n");
	
	fprintf(stderr, "IH_SetStage(): get semaphore\n");
     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
	fprintf(stderr, "IH_SetStage(): set stage to %d\n", stage);
          ih.scene.stage = stage;
	fprintf(stderr, "IH_SetStage(): ih.scene.stage = %d\n", ih.scene.stage);
#if 0
          ih.scene.is_changing = TRUE;
          ih.scene.is_dirty = TRUE;
#endif

	fprintf(stderr, "IH_SetStage(): release semaphore\n");
          SDL_SemPost(ih.ipc.sem.scene);
     }
	fprintf(stderr, "IH_SetStage(): return\n");
}
