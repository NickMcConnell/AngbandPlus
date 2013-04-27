
/* $Id: scene.c,v 1.7 2003/04/07 20:53:55 cipher Exp $ */

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
#include "displays/iso/scene.h"
#include "displays/iso/icon.h"
#include "displays/iso/misc.h"
#include "displays/iso/overlay.h"
#include "displays/iso/scene/selchar.h"
#include "displays/iso/scene/video.h"
#include "displays/iso/scene/newchar.h"
#include "displays/iso/scene/intro.h"
#include "displays/iso/scene/mhost.h"
#include "displays/iso/scene/mjoin.h"
#include "displays/iso/scene/splash.h"
#include "displays/iso/scene/title.h"
#include "displays/iso/scene/play.h"
#include "displays/iso/scene/grave.h"

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

     if(!SDL_SemWait(ih.sem.scene))
     {
          scene = ih.scene;
          dirty = ih.scene_dirty;

          SDL_SemPost(ih.sem.scene);
     }

#ifdef DEBUG
     fprintf(stderr, "IH_InitScene(): Check if scene is dirty.\n");
#endif

     if(!dirty)
          return;

     /* If there's a cleanup function, call it.
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
#if 0
     fprintf(stderr, "map_game_id(%d)\n", scene);
     scene = map_game_id(scene);
#endif

     funcs = scene_funcs(scene);
     if(!funcs)
          return;

     if(funcs->init_func)
     {
          (*funcs->init_func) ();

          cleanup_func = funcs->cleanup_func;
     }

#if 0
     switch (scene)
     {
          case SCENE_SPLASH:
               IH_InitScene_Splash();
               cleanup_func = IH_CleanupScene_Splash;
               break;

          case SCENE_INTRO:
               IH_InitScene_Intro();
               cleanup_func = IH_CleanupScene_Intro;
               break;

          case SCENE_TITLE:
               IH_InitScene_Title();
               cleanup_func = IH_CleanupScene_Title;
               break;

          case SCENE_SELECT_CHARACTER:
               IH_InitScene_SelChar();
               cleanup_func = IH_CleanupScene_SelChar;
               break;

          case SCENE_SELECT_VIDEO:
               IH_InitScene_Video();
               cleanup_func = IH_CleanupScene_Video;
               break;

          case SCENE_NEW_CHARACTER:
               IH_InitScene_NewChar();
               cleanup_func = IH_CleanupScene_NewChar;
               break;

          case SCENE_MULTIPLAYER_HOST:
               IH_InitScene_MultiHost();
               cleanup_func = IH_CleanupScene_MultiHost;
               break;

          case SCENE_MULTIPLAYER_JOIN:
               IH_InitScene_MultiJoin();
               cleanup_func = IH_CleanupScene_MultiJoin;
               break;

          case SCENE_PLAY:
               IH_InitScene_Play();
               cleanup_func = IH_CleanupScene_Play;
               break;

          case SCENE_GRAVE:
               IH_InitScene_Grave();
               cleanup_func = IH_CleanupScene_Grave;
               break;
     }
#endif

     if(!SDL_SemWait(ih.sem.scene))
     {
          ih.scene_dirty = FALSE;

          SDL_SemPost(ih.sem.scene);
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

     if(ih.changing_scene)
     {
#ifdef DEBUG
          fprintf(stderr, "Changing scene.\n");
#endif

          ih.changing_scene = FALSE;
          return;
     }

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
#ifdef DEBUG
     fprintf(stderr, "scene = %d\n", ih.scene);
#endif
#endif

     if(!SDL_SemWait(ih.sem.scene))
     {
          scene = ih.scene;

          SDL_SemPost(ih.sem.scene);
     }

#if 0
     fprintf(stderr, "map_game_id(%d)\n", ih.scene);
     scene = map_game_id(ih.scene);
#endif

     funcs = scene_funcs(scene);
     if(!funcs)
          return;

     if(funcs->process_func)
          (*funcs->process_func) (event);

#if 0
     switch (ih.scene)
     {
          case SCENE_SPLASH:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_SPLASH.\n");
#endif
               IH_ProcessScene_Splash(event);
               break;

          case SCENE_INTRO:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_INTRO.\n");
#endif
               IH_ProcessScene_Intro(event);
               break;

          case SCENE_TITLE:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_TITLE.\n");
#endif
               IH_ProcessScene_Title(event);
               break;

          case SCENE_SELECT_CHARACTER:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_SELCHAR.\n");
#endif
               IH_ProcessScene_SelChar(event);
               break;

          case SCENE_NEW_CHARACTER:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_NEWCHAR.\n");
#endif
               IH_ProcessScene_NewChar(event);
               break;

          case SCENE_MULTIPLAYER_HOST:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_MHOST.\n");
#endif
               IH_ProcessScene_MultiHost(event);
               break;

          case SCENE_MULTIPLAYER_JOIN:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_MJOIN.\n");
#endif
               IH_ProcessScene_MultiJoin(event);
               break;

          case SCENE_PLAY:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_PLAY.\n");
#endif
               IH_ProcessScene_Play(event);
               break;

          case SCENE_GRAVE:
#ifdef DEBUG
               fprintf(stderr, "process scene SCENE_GRAVE.\n");
#endif
               IH_ProcessScene_Grave(event);
               break;
     }
#endif
}

void
IH_Scene_Render(void)
{
     struct SceneFuncs *funcs;
     SDL_Rect        rect;
     int             scene = 0;

#ifdef DEBUG
     fprintf(stderr, "IH_RenderScene()\n");
#endif

     SDL_ShowCursor(FALSE);

     /* Clear the drawing surface.
      */
     rect.x = 0;
     rect.y = 0;
     rect.w = ih.display_width;
     rect.h = ih.display_height;
     SDL_FillRect(ih.screen,
                  &rect, SDL_MapRGB(ih.screen->format, 0, 0, 0));

#ifdef DEBUG
     fprintf(stderr,
             "IH_RenderScene(): Render proper scene (ih.scene = %d, ih.stage = %d)\n",
             ih.scene, ih.stage);
#endif

     if(!SDL_SemWait(ih.sem.scene))
     {
          scene = ih.scene;

          SDL_SemPost(ih.sem.scene);
     }

#if 0
     fprintf(stderr, "map_game_id(%d)\n", ih.scene);
     scene = map_game_id(ih.scene);
#endif

     funcs = scene_funcs(scene);
     if(!funcs)
          return;

     if(funcs->render_func)
          (*funcs->render_func) ();

#if 0
     switch (ih.scene)
     {
          case SCENE_SPLASH:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): splash (%d)\n",
                       ih.scene);
#endif
               IH_RenderScene_Splash();
               break;

          case SCENE_INTRO:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): intro (%d)\n", ih.scene);
#endif
               IH_RenderScene_Intro();
               break;

          case SCENE_TITLE:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): title (%d)\n", ih.scene);
#endif
               IH_RenderScene_Title();
               break;

          case SCENE_SELECT_CHARACTER:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): selchar (%d)\n",
                       ih.scene);
#endif
               IH_RenderScene_SelChar();
               break;

          case SCENE_NEW_CHARACTER:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): newchar (%d)\n",
                       ih.scene);
#endif
               IH_RenderScene_NewChar();
               break;

          case SCENE_MULTIPLAYER_HOST:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): mhost (%d)\n", ih.scene);
#endif
               IH_RenderScene_MultiHost();
               break;

          case SCENE_MULTIPLAYER_JOIN:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): mjoin (%d)\n", ih.scene);
#endif
               IH_RenderScene_MultiJoin();
               break;

          case SCENE_PLAY:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): play (%d)\n", ih.scene);
#endif
               IH_RenderScene_Play();
               break;

          case SCENE_GRAVE:
#ifdef DEBUG
               fprintf(stderr, "IH_RenderScene(): grave (%d)\n", ih.scene);
#endif
               IH_RenderScene_Grave();
               break;
     }
#endif

     IH_RenderPointer();

     SDL_Flip(ih.screen);
}

void
IH_SceneButton_SetText(SceneButton * button,
                       cptr text)
{
     if(!button || !text)
          return;

}

SceneObject    *
IH_SceneObjectAlloc(int type,
                    int object)
{
     SceneObject    *obj = NULL;

     obj = ralloc(sizeof(SceneObject));
     if(obj)
     {
          /* Clear out the structure.
           */
          memset(obj, 0, sizeof(SceneObject));

          /* Initialize some things.
           */
          obj->type = type;
          obj->object = object;

          obj->hilite = IH_SCENE_OBJECT_HILITE_NONE;
          obj->image.shown = TRUE;
     }

     return obj;
}

void
IH_SceneObjectFree(SceneObject * object)
{
     if(!object)
          return;

#ifdef DEBUG
     fprintf(stderr, "Freeing SceneObject.\n");
#endif

     rnfree(object);
}

void
IH_SetLoadMessage(cptr msg)
{
     int             len;

     if(!SDL_SemWait(ih.sem.msg))
     {
          if(ih.load_message)
               rnfree(ih.load_message);
          ih.load_message = NULL;

          if(msg)
          {
               len = strlen(msg) + 1;
               ih.load_message = ralloc(len);
               my_strcpy(ih.load_message, msg, len - 1);
          }

          SDL_SemPost(ih.sem.msg);
     }
}

void
IH_SetErrorMessage(cptr msg)
{
     int             len;

     if(!SDL_SemWait(ih.sem.msg))
     {
          if(ih.err_message)
               rnfree(ih.err_message);
          ih.err_message = NULL;

          if(msg)
          {
               len = strlen(msg) + 1;
               ih.err_message = ralloc(len);
               my_strcpy(ih.err_message, msg, len - 1);
          }

          SDL_SemPost(ih.sem.msg);
     }

     if(ih.err_message)
     {
          IH_Overlay_Show(DISPLAY_ERROR);
     }
     else
     {
          IH_Overlay_Hide(DISPLAY_ERROR);
     }
}

void
IH_SetScene(int scene)
{
     if(!SDL_SemWait(ih.sem.scene))
     {
          ih.scene = scene;
          ih.changing_scene = TRUE;
          ih.scene_dirty = TRUE;

          SDL_SemPost(ih.sem.scene);
     }
}

void
IH_SetStage(int stage)
{
     if(!SDL_SemWait(ih.sem.scene))
     {
          ih.stage = stage;
#if 0
          ih.changing_scene = TRUE;
          ih.scene_dirty = TRUE;
#endif

          SDL_SemPost(ih.sem.scene);
     }
}
