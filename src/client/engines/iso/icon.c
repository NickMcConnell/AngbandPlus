
/* $Id: icon.c,v 1.10 2003/04/20 05:20:58 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

#ifdef BUILD_ISO_ENGINE

/* SDL headers */
#include "SDL.h"
#include "SDL_image.h"

/* Internal headers */
#include "ironhells.h"
#include "path.h"
#include "engines.h"
#include "object.h"
#include "platform/platform.h"
#include "render/icon.h"

/* Display engine headers */
#include "engines/iso/init.h"
#include "engines/iso/icon.h"
#include "engines/iso/misc.h"

SDL_Surface    *tinting = NULL;

struct IconObjectInit
{
     const char     *name;
     int             object;
     int             is_dynamic:1;
     int             mode;
     int             x_align;
     int             y_align;
};

static struct IconObjectInit general_icons[] = {
     {NULL}
};

static struct IconObjectInit size_icons[] = {
     {"armor-class", IH_SCENE_OBJECT_ARMOR_CLASS, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"character-level", IH_SCENE_OBJECT_CHARACTER_LEVEL, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"strength", IH_SCENE_OBJECT_STRENGTH, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"intelligence", IH_SCENE_OBJECT_INTELLIGENCE, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"wisdom", IH_SCENE_OBJECT_WISDOM, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"dexterity", IH_SCENE_OBJECT_DEXTERITY, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"constitution", IH_SCENE_OBJECT_CONSTITUTION, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"charisma", IH_SCENE_OBJECT_CHARISMA, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"gold", IH_SCENE_OBJECT_GOLD, TRUE, IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"burden", IH_SCENE_OBJECT_BURDEN, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"inventory", IH_SCENE_OBJECT_INVENTORY, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"book", IH_SCENE_OBJECT_SPELL_BOOK, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"hungry", IH_SCENE_OBJECT_HUNGRY, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_HOVER, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP},
     {"graze", IH_SCENE_OBJECT_GRAZE, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_HOVER, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP},
     {"blind", IH_SCENE_OBJECT_BLIND, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"confused", IH_SCENE_OBJECT_CONFUSED, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"afraid", IH_SCENE_OBJECT_AFRAID, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"poisoned", IH_SCENE_OBJECT_POISONED, FALSE,
      IH_SCENE_OBJECT_TEXT_MODE_NONE, 0, 0},
     {"stun", IH_SCENE_OBJECT_STUN, FALSE, IH_SCENE_OBJECT_TEXT_MODE_HOVER,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP},
     {"study", IH_SCENE_OBJECT_STUDY, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {"depth", IH_SCENE_OBJECT_DEPTH, TRUE,
      IH_SCENE_OBJECT_TEXT_MODE_NORMAL, IH_SCENE_OBJECT_TEXT_ALIGN_CENTER,
      IH_SCENE_OBJECT_TEXT_ALIGN_CENTER},
     {NULL, 0}
};

static void
icon_create_tint(int w,
                 int h,
                 SDL_Color * color)
{
     SDL_Rect        irect;

     if(!color)
          return;

     /* Make sure the size of the tinting surface is correct.
      */
     if(tinting)
     {
          if(w != tinting->w || h != tinting->h)
          {
               /* Size is incorrect, so free the old surface.
                */
               SDL_FreeSurface(tinting);
               tinting = NULL;
          }
     }

     /* Create a new surface?
      */
     if(!tinting)
     {
          Uint32          rmask, gmask, bmask, amask;

#if SDL_BYTEORDER == SDL_BIG_ENDIAN
          rmask = 0xff000000;
          gmask = 0x00ff0000;
          bmask = 0x0000ff00;
          amask = 0x000000ff;
#else
          rmask = 0x000000ff;
          gmask = 0x0000ff00;
          bmask = 0x00ff0000;
          amask = 0xff000000;
#endif

          tinting = SDL_CreateRGBSurface(SDL_SWSURFACE | SDL_SRCALPHA,
                                         w, h, 32, rmask, gmask, bmask,
                                         amask);
     }

     /* Paranoia.
      */
     if(!tinting)
          return;

     irect.x = 0;
     irect.y = 0;
     irect.w = w;
     irect.h = h;

     /* Fill the square with color.
      */
     SDL_FillRect(tinting,
                  &irect,
                  SDL_MapRGBA(tinting->format,
                              color->r, color->g, color->b,
                              IH_ALPHA_VALUE));

     /* Set the alpha on the shader surface.
      */
     SDL_SetAlpha(tinting, SDL_SRCALPHA | SDL_RLEACCEL, IH_ALPHA_VALUE);
}

errr
IH_ISO_LoadIcons(void)
{
     displayData    *display_data;
     isoEngineData  *engine_data;
     errr            rc = 0;
     int             i;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     memset(display_data->icons, 0, sizeof(display_data->icons));
     display_data->icon_count = 0;

     /* Load character image according to class.
      */
     IH_ISO_LoadCharacterIcon();

     /* Load the general icon images.
      */

     /* Load the size icon images.
      */
     for(i = 0; size_icons[i].name; i++)
     {
          SceneObject    *object;

          object = IH_SceneObject_Alloc(IH_SCENE_OBJECT_TYPE_ICON,
                                        size_icons[i].object);
          if(object)
          {
               display_data->icons[display_data->icon_count++] = object;

               object->text.is_dynamic = size_icons[i].is_dynamic;
               object->text.mode = size_icons[i].mode;
               object->text.x_align = size_icons[i].x_align;
               object->text.y_align = size_icons[i].y_align;

               object->image.bits =
                   (void *) IH_LoadIcon(IH_ICON_SIZE_CURRENT,
                                        size_icons[i].name);
               if(!object->image.bits)
               {
#ifdef DEBUG
                    fprintf(stderr,
                            "Unable to load icon image: %s: %s\n",
                            size_icons[i].name, IMG_GetError());
#endif
                    rc = IH_ERROR_CANT_LOAD_ICON;
               }
               else
               {
                    object->image.width = ((SDL_Surface *)object->image.bits)->w;
                    object->image.height = ((SDL_Surface *)object->image.bits)->h;
               }
          }
     }

     /* Do icon positioning.
      */
     IH_PositionIcons();

     return rc;
}

void
IH_ISO_FreeIcons(void)
{
// FIXME
}

errr
IH_ISO_LoadCharacterIcon(void)
{
     errr            rc = 0;

     return rc;
}

void
IH_ISO_RenderIcon(SceneObject * object,
                  SDL_Rect * srect,
                  SDL_Rect * drect)
{
     SDL_Surface    *surface;

     if(!object)
          return;

     surface = (SDL_Surface *) object->image.bits;
     if(!surface)
          return;

     /* Handle alpha.
      */
     SDL_SetAlpha(surface, SDL_SRCALPHA,
                  (Uint8) (object->image.alpha * 255));

     /* Draw the icon.
      */
     SDL_BlitSurface(surface, srect, ih.display.screen, drect);

     /* Handle tinting.
      */
     if(object->image.is_tinted)
     {
          SDL_Color       sdl_color;

          IH_ISO_ConvertColor(&object->image.tint, &sdl_color);

          icon_create_tint(surface->w, surface->h, &sdl_color);

          if(tinting)
               SDL_BlitSurface(tinting, NULL, ih.display.screen, drect);
     }
}

#endif /* BUILD_ISO_ENGINE */
