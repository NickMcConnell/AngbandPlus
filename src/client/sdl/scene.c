
/* $Id: scene.c,v 1.11 2003/03/23 06:10:27 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"
#include "SDL_image.h"

#include "angband.h"
#include "sdl/scene.h"
#include "ironhells.h"
#include "path.h"
#include "file.h"
#include "sdl/render/icon.h"
#include "sdl/render/misc.h"
#include "sdl/render/overlay.h"
#include "sdl/scene/selchar.h"
#include "sdl/scene/newchar.h"
#include "sdl/scene/intro.h"
#include "sdl/scene/mhost.h"
#include "sdl/scene/mjoin.h"
#include "sdl/scene/splash.h"
#include "sdl/scene/title.h"
#include "sdl/scene/play.h"
#include "sdl/scene/grave.h"

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

void
IH_InitScene(void)
{
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

     switch (scene)
     {
          case IH_SCENE_SPLASH:
               IH_InitScene_Splash();
               break;

          case IH_SCENE_INTRO:
               IH_InitScene_Intro();
               break;

          case IH_SCENE_TITLE:
               IH_InitScene_Title();
               break;

          case IH_SCENE_SELECT_CHARACTER:
               IH_InitScene_SelChar();
               break;

          case IH_SCENE_NEW_CHARACTER:
               IH_InitScene_NewChar();
               break;

          case IH_SCENE_MULTIPLAYER_HOST:
               IH_InitScene_MultiHost();
               break;

          case IH_SCENE_MULTIPLAYER_JOIN:
               IH_InitScene_MultiJoin();
               break;

          case IH_SCENE_PLAY:
               IH_InitScene_Play();
               break;

          case IH_SCENE_GRAVE:
               IH_InitScene_Grave();
               break;
     }

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
IH_ProcessScene(SDL_Event * event)
{
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

     switch (ih.scene)
     {
          case IH_SCENE_SPLASH:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_SPLASH.\n");
#endif
               IH_ProcessScene_Splash(event);
               break;

          case IH_SCENE_INTRO:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_INTRO.\n");
#endif
               IH_ProcessScene_Intro(event);
               break;

          case IH_SCENE_TITLE:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_TITLE.\n");
#endif
               IH_ProcessScene_Title(event);
               break;

          case IH_SCENE_SELECT_CHARACTER:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_SELCHAR.\n");
#endif
               IH_ProcessScene_SelChar(event);
               break;

          case IH_SCENE_NEW_CHARACTER:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_NEWCHAR.\n");
#endif
               IH_ProcessScene_NewChar(event);
               break;

          case IH_SCENE_MULTIPLAYER_HOST:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_MHOST.\n");
#endif
               IH_ProcessScene_MultiHost(event);
               break;

          case IH_SCENE_MULTIPLAYER_JOIN:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_MJOIN.\n");
#endif
               IH_ProcessScene_MultiJoin(event);
               break;

          case IH_SCENE_PLAY:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_PLAY.\n");
#endif
               IH_ProcessScene_Play(event);
               break;

          case IH_SCENE_GRAVE:
#ifdef DEBUG
               fprintf(stderr, "process scene IH_SCENE_GRAVE.\n");
#endif
               IH_ProcessScene_Grave(event);
               break;
     }
}

SceneObject    *
IH_GetIconAtPosition(int x,
                     int y)
{
     ihNode         *node;

     /* Iterate over the list of graphic elements and find out which icon
      * the mouse is over.  Must account for layering.
      */
     for(node = IH_ListFirst(&ih.icons);
         node; node = IH_ListNext(&ih.icons, node))
     {
          SceneObject    *object;

          object = (SceneObject *) node->data;
          if(!object)
               continue;

          if((x >= object->x) &&
             (x < (object->x + ih.icon_size)) &&
             (y >= object->y) && (y < (object->y + ih.icon_size)))
               return object;
     }

     return NULL;
}

void
IH_CalcIconPosition(int icon,
                    int *x,
                    int *y)
{

}

SceneObject    *
IH_SceneObjectAlloc(int type,
                    int object)
{
     SceneObject    *obj = NULL;

     obj = ralloc(sizeof(SceneObject));
     if(obj)
     {
          obj->type = type;
          obj->object = object;

          obj->x = 0;
          obj->y = 0;
          obj->slot = 0;
          obj->hilite = IH_SCENE_OBJECT_HILITE_NONE;
          obj->shown = TRUE;
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
          IH_ActivateOverlay(IH_OVERLAY_ERROR);
     }
     else
     {
          IH_DeactivateOverlay(IH_OVERLAY_ERROR);
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
