
/* $Id: icon.c,v 1.7 2003/04/18 03:32:56 cipher Exp $ */

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
#include "ironhells.h"
#include "engines.h"
#include "path.h"
#include "object.h"
#include "render/icon.h"
#include "platform/platform.h"

struct IconObjectPos
{
     int             object;
     int             dir, anchor;
     int             x_offset, y_offset;
};

enum
{
     IH_ICON_DIR_DOWN = 0,
     IH_ICON_DIR_UP,
     IH_ICON_DIR_LEFT,
     IH_ICON_DIR_RIGHT
};

enum
{
     IH_ICON_ANCHOR_TOPLEFT = 0,
     IH_ICON_ANCHOR_TOPRIGHT,
     IH_ICON_ANCHOR_BOTTOMLEFT,
     IH_ICON_ANCHOR_BOTTOMRIGHT
};

static struct IconObjectPos icon_pos[] = {
     {IH_SCENE_OBJECT_CHARACTER, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 0},
     {IH_SCENE_OBJECT_ARMOR_CLASS, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 1},
     {IH_SCENE_OBJECT_CHARACTER_LEVEL, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 2},
     {IH_SCENE_OBJECT_STRENGTH, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 3},
     {IH_SCENE_OBJECT_INTELLIGENCE, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 4},
     {IH_SCENE_OBJECT_WISDOM, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT, 0,
      5},
     {IH_SCENE_OBJECT_DEXTERITY, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 6},
     {IH_SCENE_OBJECT_CONSTITUTION, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPLEFT, 0, 7},
     {IH_SCENE_OBJECT_CHARISMA, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT,
      0, 8},
     {IH_SCENE_OBJECT_GOLD, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT, 0,
      9},
     {IH_SCENE_OBJECT_BURDEN, IH_ICON_DIR_DOWN, IH_ICON_ANCHOR_TOPLEFT, 0,
      10},
     {IH_SCENE_OBJECT_INVENTORY, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_TOPRIGHT,
      0, 0},
     {IH_SCENE_OBJECT_INVENTORY_SLOT, IH_ICON_DIR_DOWN,
      IH_ICON_ANCHOR_TOPRIGHT, 0, 1},
     {IH_SCENE_OBJECT_SPELL_BOOK, IH_ICON_DIR_LEFT,
      IH_ICON_ANCHOR_TOPRIGHT, 1, 0},
     {IH_SCENE_OBJECT_DEPTH, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      0, 0},
     {IH_SCENE_OBJECT_HUNGRY, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      1, 0},
     {IH_SCENE_OBJECT_GRAZE, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      2, 0},
     {IH_SCENE_OBJECT_BLIND, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      3, 0},
     {IH_SCENE_OBJECT_CONFUSED, IH_ICON_DIR_LEFT,
      IH_ICON_ANCHOR_BOTTOMRIGHT, 4, 0},
     {IH_SCENE_OBJECT_AFRAID, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      5, 0},
     {IH_SCENE_OBJECT_POISONED, IH_ICON_DIR_LEFT,
      IH_ICON_ANCHOR_BOTTOMRIGHT, 6, 0},
     {IH_SCENE_OBJECT_STUN, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      7, 0},
     {IH_SCENE_OBJECT_STUDY, IH_ICON_DIR_LEFT, IH_ICON_ANCHOR_BOTTOMRIGHT,
      8, 0},
     {IH_SCENE_OBJECT_END}
};

static void
format_stat(int val,
            char *out_val)
{
     /* Above 18 */
     if(val > 18)
     {
          int             bonus = (val - 18);

          if(bonus >= 100)
          {
               sprintf(out_val, "18/%03d", bonus);
          }
          else
          {
               sprintf(out_val, "18/%02d", bonus);
          }
     }

     /* From 3 to 18 */
     else
     {
          sprintf(out_val, "%2d", val);
     }
}

SDL_Surface    *
IH_LoadIcon(int icon_size,
            cptr name)
{
     SDL_Surface    *image = NULL;
     char           *path_data, *path_size, *filename, *file;
     cptr            size = NULL;
     int             filename_len;

     if(!name)
          return NULL;

     path_data = IH_GetDataDir("gfx");

     if(icon_size == IH_ICON_SIZE_CURRENT)
          icon_size = ih.icon_size;

     switch (icon_size)
     {
          case IH_ICON_SIZE_LARGE:
               size = "large";
               break;

          case IH_ICON_SIZE_MEDIUM:
               size = "medium";
               break;

          case IH_ICON_SIZE_SMALL:
               size = "small";
               break;

          default:
               size = NULL;
               break;
     }
     if(size)
          path_size = IH_PathBuild(path_data, "icon", size, NULL);
     else
          path_size = IH_PathBuild(path_data, "icon", NULL);

     filename_len = strlen(name) + strlen(IH_IMAGE_FORMAT_EXT) + 2;
     filename = ralloc(filename_len);
     my_strcpy(filename, name, filename_len);
     my_strcat(filename, "." IH_IMAGE_FORMAT_EXT, filename_len);

     file = IH_PathBuild(path_size, filename, NULL);
     rnfree(filename);

     if(file)
     {
          SDL_Surface    *t_image;

          /* Load the image.
           */
          t_image = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);

          /* Convert the surface to match the display.
           */
          if(t_image)
          {
               image = SDL_DisplayFormatAlpha(t_image);

               SDL_FreeSurface(t_image);
          }

          rnfree(file);
     }

     rnfree(path_size);
     rnfree(path_data);

     return image;
}

void
IH_PositionIcons(void)
{
     displayData    *display_data;
     int             i;

     display_data = (displayData *) ih.display.data;

#ifdef DEBUG
     fprintf(stderr, "IH_PositionIcons()\n");
#endif

     for(i = 0; i < display_data->icon_count; i++)
     {
          SceneObject    *object;
          struct IconObjectPos *pos;
          int             j;
          int             base_x = 0, base_y = 0;
          int             dir_x = 0, dir_y = 0;
          int             slot_x = 0, slot_y = 0;
          int             icon_size;

          object = (SceneObject *) display_data->icons[i];
          if(!object)
               continue;

#ifdef DEBUG
          fprintf(stderr, "object->type = %d\n", object->type);
          fprintf(stderr, "object->object = %d\n", object->object);
#endif

          pos = NULL;

          for(j = 0; icon_pos[j].object != IH_SCENE_OBJECT_END; j++)
          {
               if(icon_pos[j].object == object->object)
               {
#ifdef DEBUG
                    fprintf(stderr, "Object matches positioning data.\n");
#endif

                    pos = &icon_pos[j];
                    break;
               }
          }

          if(!pos)
               continue;

#ifdef DEBUG
          fprintf(stderr, "Getting icon size value.\n");
#endif
          switch (ih.icon_size)
          {
               default:
               case IH_ICON_SIZE_SMALL:
                    icon_size = IH_ICON_SIZE_SMALL_VALUE;
                    break;

               case IH_ICON_SIZE_MEDIUM:
                    icon_size = IH_ICON_SIZE_MEDIUM_VALUE;
                    break;

               case IH_ICON_SIZE_LARGE:
                    icon_size = IH_ICON_SIZE_LARGE_VALUE;
                    break;
          }
#ifdef DEBUG
          fprintf(stderr, "icon_size = %d\n", icon_size);
#endif

#ifdef DEBUG
          fprintf(stderr, "Getting anchor positions.\n");
#endif
          switch (pos->anchor)
          {
               case IH_ICON_ANCHOR_TOPLEFT:
                    base_x = 0;
                    base_y = 0;
                    break;

               case IH_ICON_ANCHOR_BOTTOMLEFT:
                    base_x = 0;
                    base_y = ih.display.height - icon_size;
                    break;

               case IH_ICON_ANCHOR_TOPRIGHT:
                    base_x = ih.display.width - icon_size;
                    base_y = 0;
                    break;

               case IH_ICON_ANCHOR_BOTTOMRIGHT:
                    base_x = ih.display.width - icon_size;
                    base_y = ih.display.height - icon_size;
                    break;
          }
#ifdef DEBUG
          fprintf(stderr, "base_x = %d\nbase_y = %d\n", base_x, base_y);
#endif

#ifdef DEBUG
          fprintf(stderr, "Getting directional info.\n");
#endif
          switch (pos->dir)
          {
               case IH_ICON_DIR_DOWN:
                    base_y = 0;
                    dir_x = 0;
                    dir_y = 1;
                    if(object->slot > 0)
                         slot_y = object->slot - 1;
                    break;

               case IH_ICON_DIR_UP:
                    base_y = ih.display.height - icon_size;
                    dir_x = 0;
                    dir_y = -1;
                    if(object->slot > 0)
                         slot_y = object->slot - 1;
                    break;

               case IH_ICON_DIR_RIGHT:
                    base_x = 0;
                    dir_x = 1;
                    dir_y = 0;
                    if(object->slot > 0)
                         slot_x = object->slot - 1;
                    break;

               case IH_ICON_DIR_LEFT:
                    base_x = ih.display.width - icon_size;
                    dir_x = -1;
                    dir_y = 0;
                    if(object->slot > 0)
                         slot_x = object->slot - 1;
                    break;
          }
#ifdef DEBUG
          fprintf(stderr,
                  "base_x = %d\nbase_y = %d\ndir_x = %d\ndir_y = %d\nslot_x = %d\nslot_y = %d\n",
                  base_x, base_y, dir_x, dir_y, slot_x, slot_y);
#endif

          object->x =
              base_x + (pos->x_offset * icon_size * dir_x) +
              (slot_x * icon_size * dir_x);
#ifdef DEBUG
          fprintf(stderr, "object->x = %d\n", object->x);
#endif

          object->y =
              base_y + (pos->y_offset * icon_size * dir_y) +
              (slot_y * icon_size * dir_y);
#ifdef DEBUG
          fprintf(stderr, "object->y = %d\n", object->y);
#endif
     }
}

int
IH_GetIconSize(int size)
{
     int             size_val = 0;

     if(size == IH_ICON_SIZE_CURRENT)
          size = ih.icon_size;

     switch (size)
     {
          case IH_ICON_SIZE_SMALL:
               size_val = IH_ICON_SIZE_SMALL_VALUE;
               break;

          case IH_ICON_SIZE_MEDIUM:
               size_val = IH_ICON_SIZE_MEDIUM_VALUE;
               break;

          case IH_ICON_SIZE_LARGE:
               size_val = IH_ICON_SIZE_LARGE_VALUE;
               break;
     }

     return size_val;
}

SceneObject    *
IH_GetIconAtPosition(int x,
                     int y)
{
     displayData    *display_data;
     int             i;

     display_data = (displayData *) ih.display.data;

     /* Iterate over the list of graphic elements and find out 
      * which icon the mouse is over.  
      * Must account for layering.
      */
     for(i = 0; i < IH_MAX_ICONS; i++)
     {
          SceneObject    *object;

          object = display_data->icons[i];
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

void
IH_RenderIcon(SceneObject * object,
              SDL_Rect * srect,
              SDL_Rect * drect)
{
     if(ih.display.render_icon_func)
          (*ih.display.render_icon_func) (object, srect, drect);
}

void
IH_RenderIcons(void)
{
     displayData    *display_data;
     int             i;

     display_data = (displayData *) ih.display.data;

     for(i = 0; i < IH_MAX_ICONS; i++)
     {
          SceneObject    *object;

          object = display_data->icons[i];
          if(object)
          {
               SDL_Rect        rect;

               if(object->type != IH_SCENE_OBJECT_TYPE_ICON)
                    continue;

               /* Make sure the object's text is up-to-date.
                */
               IH_SceneObject_Update(object);

               /* Render the icon image.
                */
               if(object->image.bits && object->image.is_shown)
               {
                    rect.x = object->x;
                    rect.y = object->y;

                    IH_RenderIcon(object, NULL, &rect);
               }

               /* Render the text string.
                */
               if(object->text.is_shown)
                    IH_SceneObject_RenderText(object);
          }
     }
}
