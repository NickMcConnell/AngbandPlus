
/* $Id: icon.c,v 1.8 2003/03/23 06:10:27 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"
#include "SDL_image.h"

#include "ironhells.h"
#include "sdl/render/icon.h"
#include "sdl/scene.h"
#include "sdl/render.h"
#include "path.h"
#include "file.h"

struct IconObjectInit
{
     const char     *name;
     int             object;
};

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

static struct IconObjectInit general_icons[] = {
     {NULL}
};

static struct IconObjectInit size_icons[] = {
     {"armor-class", IH_SCENE_OBJECT_ARMOR_CLASS},
     {"character-level", IH_SCENE_OBJECT_CHARACTER_LEVEL},
     {"strength", IH_SCENE_OBJECT_STRENGTH},
     {"intelligence", IH_SCENE_OBJECT_INTELLIGENCE},
     {"wisdom", IH_SCENE_OBJECT_WISDOM},
     {"dexterity", IH_SCENE_OBJECT_DEXTERITY},
     {"constitution", IH_SCENE_OBJECT_CONSTITUTION},
     {"charisma", IH_SCENE_OBJECT_CHARISMA},
     {"gold", IH_SCENE_OBJECT_GOLD},
     {"burden", IH_SCENE_OBJECT_BURDEN},
     {"inventory", IH_SCENE_OBJECT_INVENTORY},
     {"book", IH_SCENE_OBJECT_SPELL_BOOK},
     {"hungry", IH_SCENE_OBJECT_HUNGRY},
     {"graze", IH_SCENE_OBJECT_GRAZE},
     {"blind", IH_SCENE_OBJECT_BLIND},
     {"confused", IH_SCENE_OBJECT_CONFUSED},
     {"afraid", IH_SCENE_OBJECT_AFRAID},
     {"poisoned", IH_SCENE_OBJECT_POISONED},
     {"stun", IH_SCENE_OBJECT_STUN},
     {"study", IH_SCENE_OBJECT_STUDY},
     {"depth", IH_SCENE_OBJECT_DEPTH},
     {NULL, 0}
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

errr
IH_LoadIcons(void)
{
     errr            rc = 0;
     char           *path_data;
     char           *path_general; // general icons; regardless of selected size
     cptr            size;
     char           *path_size; // icons of the user-selected size
     int             i;

     path_data = IH_GetDataDir("gfx");
     path_general = IH_PathBuild(path_data, "icon", NULL);

     switch (ih.icon_size)
     {
          case IH_ICON_SIZE_LARGE:
               size = "large";
               break;

          case IH_ICON_SIZE_MEDIUM:
               size = "medium";
               break;

          case IH_ICON_SIZE_SMALL:
          default:
               size = "small";
               break;
     }
     path_size = IH_PathBuild(path_data, "icon", size, NULL);

     /* Load character image according to class.
      */
     // IH_LoadCharacterIcon();

     /* Load the general icon images.
      */

     /* Load the size icon images.
      */
     for(i = 0; size_icons[i].name; i++)
     {
          char           *file;
          char           *filename;
          int             filename_len;

          filename_len =
              strlen(size_icons[i].name) + strlen(IH_IMAGE_FORMAT_EXT) + 2;
          filename = ralloc(filename_len);
          my_strcpy(filename, size_icons[i].name, filename_len);
          my_strcat(filename, ".", filename_len);
          my_strcat(filename, IH_IMAGE_FORMAT_EXT, filename_len);

          file = IH_PathBuild(path_size, filename, NULL);
          rnfree(filename);

          if(file)
          {
               SceneObject    *object;

               object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_ICON,
                                            size_icons[i].object);
               if(object)
               {
                    IH_ListAppend(&ih.icons, (void *) object);

                    object->data.image =
                        IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
                    if(!object->data.image)
                    {
#ifdef DEBUG
                         fprintf(stderr,
                                 "Unable to load icon image: %s: %s\n",
                                 file, IMG_GetError());
#endif
                         rc = IH_ERROR_CANT_LOAD_ICON;
                    }
               }
          }

          rnfree(file);
     }

     /* Do icon positioning.
      */
     IH_PositionIcons();

     /* Free the path variables.
      */
     rnfree(path_data);
     rnfree(path_size);
     rnfree(path_general);

     return rc;
}

errr
IH_LoadCharacterIcon(void)
{
     errr            rc = 0;

     return rc;
}

void
IH_PositionIcons(void)
{
     ihNode         *node;

#ifdef DEBUG
     fprintf(stderr, "IH_PositionIcons()\n");
#endif

     for(node = IH_ListFirst(&ih.icons);
         node; node = IH_ListNext(&ih.icons, node))
     {
          SceneObject    *object;
          struct IconObjectPos *pos;
          int             i;
          int             base_x = 0, base_y = 0;
          int             dir_x = 0, dir_y = 0;
          int             slot_x = 0, slot_y = 0;
          int             icon_size;

          object = (SceneObject *) node->data;
          if(!object)
               continue;

#ifdef DEBUG
          fprintf(stderr, "object->type = %d\n", object->type);
          fprintf(stderr, "object->object = %d\n", object->object);
#endif

          pos = NULL;

          for(i = 0; icon_pos[i].object != IH_SCENE_OBJECT_END; i++)
          {
               if(icon_pos[i].object == object->object)
               {
#ifdef DEBUG
                    fprintf(stderr, "Object matches positioning data.\n");
#endif

                    pos = &icon_pos[i];
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
                    base_y = ih.display_height - icon_size;
                    break;

               case IH_ICON_ANCHOR_TOPRIGHT:
                    base_x = ih.display_width - icon_size;
                    base_y = 0;
                    break;

               case IH_ICON_ANCHOR_BOTTOMRIGHT:
                    base_x = ih.display_width - icon_size;
                    base_y = ih.display_height - icon_size;
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
                    base_y = ih.display_height - icon_size;
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
                    base_x = ih.display_width - icon_size;
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

void
IH_RenderIcons(void)
{
     ihNode         *node;

#ifdef DEBUG
     fprintf(stderr, "IH_RenderIcons()\n");
#endif

     for(node = IH_ListFirst(&ih.icons);
         node; node = IH_ListNext(&ih.icons, node))
     {
          SceneObject    *object;

#ifdef DEBUG
          fprintf(stderr, "Got a node.\n");
#endif
          object = (SceneObject *) node->data;
          if(object)
          {
               SDL_Rect        rect;

               if(object->type != IH_SCENE_OBJECT_TYPE_ICON)
                    continue;
               if(!object->data.image)
                    continue;

               rect.x = object->x;
               rect.y = object->y;

               SDL_BlitSurface(object->data.image, NULL, ih.screen, &rect);
          }
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
