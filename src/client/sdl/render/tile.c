
/* $Id: tile.c,v 1.13 2003/03/18 19:17:42 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include <math.h>

#include "SDL_image.h"

#include "ironhells.h"
#include "list.h"
#include "file.h"
#include "path.h"
#include "sdl/scene.h"
#include "sdl/render/tile.h"

errr
IH_InitTiles(void)
{
     /* Tiles are loaded on-demand, so this function doesn't really
      * need to do much; it's here for consistency.
      */

     return 0;
}

static SDL_Surface *
IH_LoadTile(int tile_num)
{
     SDL_Surface    *image = NULL;
     char           *path_data = NULL, *path_tile = NULL;
     char           *file, *filename;

     if((tile_num >= IH_MAX_TILES) || (tile_num < 0))
          return NULL;

     if(ih.tile_array[tile_num])
          return ih.tile_array[tile_num];

     /* Determine path to tiles.
      */
     path_data = IH_GetDataDir("gfx");
     path_tile = IH_PathBuild(path_data, "tile", NULL);

     filename = IH_GetManifestFilename(path_tile, tile_num);
     if(filename)
     {
          fprintf(stderr, "IH_LoadTile(): filename = %s\n", filename);

          file = IH_PathBuild(path_tile, filename, NULL);
          if(file)
          {
               SceneObject    *object;

               fprintf(stderr,
                       "IH_LoadTile(): Allocate object for %s\n", file);
               object = IH_SceneObjectAlloc
                   (IH_SCENE_OBJECT_TYPE_TILE, IH_SCENE_OBJECT_TILE);
               if(object)
               {
                    fprintf(stderr,
                            "IH_LoadTile(): Load image from %s\n", file);
                    object->data.image =
                        IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
                    if(object->data.image)
                    {
                         image = ih.tile_array[tile_num] =
                             object->data.image;

                         fprintf(stderr,
                                 "IH_LoadTile(): Append object to tile list\n");
                         IH_ListAppend(&ih.tiles, (void *) object);
                    }
                    else
                    {
                         fprintf(stderr,
                                 "IH_LoadTile(): Free scene object\n");
                         IH_SceneObjectFree(object);

                         fprintf(stderr,
                                 "IH_LoadTile(): Unable to load tile image: %s: %s\n",
                                 file, IMG_GetError());
                    }
               }

               fprintf(stderr, "IH_LoadTile(): Free file variable\n");
               rnfree(file);
          }

          rnfree(filename);
     }

     fprintf(stderr, "IH_LoadTile(): Free path_tile\n");
     rnfree(path_tile);
     fprintf(stderr, "IH_LoadTile(): Free path_data\n");
     rnfree(path_data);

     return image;
}

static SDL_Surface *
IH_LoadObject(int obj_num)
{
     SDL_Surface    *image = NULL;
     char           *path_data = NULL, *path_obj = NULL;
     char           *file, *filename;

     if((obj_num >= IH_MAX_OBJS) || (obj_num < 0))
          return NULL;

     if(ih.obj_array[obj_num])
          return ih.obj_array[obj_num];

     /* Determine path to objects.
      */
     path_data = IH_GetDataDir("gfx");
     path_obj = IH_PathBuild(path_data, "object", NULL);

     filename = IH_GetManifestFilename(path_obj, obj_num);
     if(filename)
     {
          fprintf(stderr, "IH_LoadObject(): filename = %s\n", filename);

          file = IH_PathBuild(path_obj, filename, NULL);
          if(file)
          {
               SceneObject    *object;

               fprintf(stderr,
                       "IH_LoadObject(): Allocate object for %s\n", file);
               object = IH_SceneObjectAlloc
                   (IH_SCENE_OBJECT_TYPE_OBJECT, IH_SCENE_OBJECT_OBJECT);
               if(object)
               {
                    fprintf(stderr,
                            "IH_LoadObject(): Load image from %s\n", file);
                    object->data.image =
                        IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
                    if(object->data.image)
                    {
                         image = ih.obj_array[obj_num] =
                             object->data.image;

                         fprintf(stderr,
                                 "IH_LoadObject(): Append object to obj list\n");
                         IH_ListAppend(&ih.objects, (void *) object);
                    }
                    else
                    {
                         fprintf(stderr,
                                 "IH_LoadObject(): Free scene object\n");
                         IH_SceneObjectFree(object);

                         fprintf(stderr,
                                 "IH_LoadObject(): Unable to load object image: %s: %s\n",
                                 file, IMG_GetError());
                    }
               }

               fprintf(stderr, "IH_LoadObject(): Free file variable\n");
               rnfree(file);
          }

          rnfree(filename);
     }

     fprintf(stderr, "IH_LoadObject(): Free path_obj\n");
     rnfree(path_obj);
     fprintf(stderr, "IH_LoadObject(): Free path_data\n");
     rnfree(path_data);

     return image;
}

#if 0
static SDL_Surface *
IH_LoadCreature(int creature_num)
{
     SDL_Surface    *image = NULL;
     char           *path_data = NULL, *path_creature = NULL;
     char           *file, *filename;

     if((creature_num >= IH_MAX_CREATURES) || (creature_num < 0))
          return NULL;

     if(ih.creature_array[creature_num])
          return ih.creature_array[creature_num];

     /* Determine path to creatures.
      */
     path_data = IH_GetDataDir("gfx");
     path_creature = IH_PathBuild(path_data, "creature", NULL);

     filename = IH_GetManifestFilename(path_creature, creature_num);
     if(filename)
     {
          fprintf(stderr, "IH_LoadCreature(): filename = %s\n", filename);

          file = IH_PathBuild(path_creature, filename, NULL);
          if(file)
          {
               SceneObject    *object;

               fprintf(stderr,
                       "IH_LoadObject(): Allocate object for %s\n", file);
               object = IH_SceneObjectAlloc
                   (IH_SCENE_OBJECT_TYPE_CREATURE,
                    IH_SCENE_OBJECT_CREATURE);
               if(object)
               {
                    fprintf(stderr,
                            "IH_LoadCreature(): Load image from %s\n",
                            file);
                    object->data.image =
                        IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
                    if(object->data.image)
                    {
                         image = ih.creature_array[creature_num] =
                             object->data.image;

                         fprintf(stderr,
                                 "IH_LoadCreature(): Append object to creature list\n");
                         IH_ListAppend(&ih.creatures, (void *) object);
                    }
                    else
                    {
                         fprintf(stderr,
                                 "IH_LoadCreature(): Free scene object\n");
                         IH_SceneObjectFree(object);

                         fprintf(stderr,
                                 "IH_LoadCreature(): Unable to load creature image: %s: %s\n",
                                 file, IMG_GetError());
                    }
               }

               fprintf(stderr, "IH_LoadCreature(): Free file variable\n");
               rnfree(file);
          }

          rnfree(filename);
     }

     fprintf(stderr, "IH_LoadCreature(): Free path_creature\n");
     rnfree(path_creature);
     fprintf(stderr, "IH_LoadCreature(): Free path_data\n");
     rnfree(path_data);

     return image;
}
#endif

static void
IH_DrawTile(int pixel_x,
            int pixel_y,
            int dungeon_x,
            int dungeon_y)
{
     SDL_Surface    *tile;
     SDL_Rect        rect;
     byte            ap, tap;
     char            cp, tcp;
     int             tile_num = 0;

     /* Figure out if it will even be displayed on the screen.
      */
     // FIXME?

     /* Get the map grid.
      */
     map_info(dungeon_x, dungeon_y, &ap, &cp, &tap, &tcp);
     fprintf(stderr,
             "IH_DrawTile(): map_info says (%d,%d) contains: ap=%d, cp=%d, tap=%d, tcp=%d\n",
             dungeon_x, dungeon_y, ap, cp, tap, tcp);
     if(cp & 0x80)
          tile_num = PICT(ap, ap);
     fprintf(stderr, "IH_DrawTile(): tile_num = %d\n", tile_num);

     /* Display the map grid.
      */
     // FIXME
     rect.x = pixel_x;
     rect.y = pixel_y;
     tile = IH_LoadTile(0 /* tile_num */ );
     if(!tile)
          return;

     SDL_BlitSurface(tile, NULL, ih.screen, &rect);
}

static void
IH_DrawObject(int pixel_x,
              int pixel_y,
              int dungeon_x,
              int dungeon_y)
{
     SDL_Surface    *obj;
     SDL_Rect        rect;
     byte            ap, tap;
     char            cp, tcp;
     int             obj_num = 0;

     /* Figure out if it will even be displayed on the screen.
      */
     // FIXME?

     /* Get the map grid.
      */
     map_info(dungeon_x, dungeon_y, &ap, &cp, &tap, &tcp);
     fprintf(stderr,
             "IH_DrawTile(): map_info says (%d,%d) contains: ap=%d, cp=%d, tap=%d, tcp=%d\n",
             dungeon_x, dungeon_y, ap, cp, tap, tcp);
     if(tcp & 0x80)
          obj_num = PICT(tap, tcp);
     fprintf(stderr, "IH_DrawObject(): obj_num = %d\n", obj_num);

     /* Display the map grid.
      */
     // FIXME
     rect.x = pixel_x;
     rect.y = pixel_y;
     obj = IH_LoadObject(0 /* obj_num */ );
     if(!obj)
          return;

     SDL_BlitSurface(obj, NULL, ih.screen, &rect);
}

#if 0
static void
IH_DrawCreature(int pixel_x,
                int pixel_y,
                int dungeon_x,
                int dungeon_y)
{
     SDL_Surface    *creature;
     SDL_Rect        rect;
     byte            oap, tap, cap;
     char            ocp, tcp, ccp;
     int             creature_num = 0;

     /* Figure out if it will even be displayed on the screen.
      */
     // FIXME?

     /* Get the map grid.
      */
     map_info(dungeon_x, dungeon_y, &cap, &ccp, &oap, &ocp, &tap, &tcp);
     fprintf(stderr,
             "IH_DrawTile(): map_info says (%d,%d) contains: cap=%d, ccp=%d, oap=%d, ocp=%d, tap=%d, tcp=%d\n",
             dungeon_x, dungeon_y, cap, ccp, oap, ocp, tap, tcp);
     if(ccp & 0x80)
          creature_num = PICT(cap, ccp);
     fprintf(stderr, "IH_DrawCreature(): creature_num = %d\n",
             creature_num);

     /* Display the map grid.
      */
     // FIXME
     rect.x = pixel_x;
     rect.y = pixel_y;
     creature = IH_LoadCreature(0 /* creature_num */ );
     if(!creature)
          return;

     SDL_BlitSurface(creature, NULL, ih.screen, &rect);
}
#endif

void
IH_RenderTiles(void)
{
     int             center_tile_x, center_tile_y, start_tile_x,
         start_tile_y;
     int             num_tiles_left, num_tiles_right, num_tiles_up,
         num_tiles_down;
     int             num_tiles_x, num_tiles_y;
     int             x, y;

     fprintf(stderr, "IH_RenderTiles()\n");

     fprintf(stderr, "p_ptr->px,py = %d,%d\n", p_ptr->px, p_ptr->py);

     /* Calculate pixel positions for the center tile (the player).
      */
     center_tile_x =
         ((ih.display_width - IH_TILE_BASE_WIDTH) / 2) -
         (IH_TILE_ACTUAL_WIDTH - IH_TILE_BASE_WIDTH);
     center_tile_y =
         ((ih.display_height - IH_TILE_BASE_HEIGHT) / 2) -
         (IH_TILE_ACTUAL_HEIGHT - IH_TILE_BASE_HEIGHT);
     fprintf(stderr, "center_tile_x,y = %d,%d\n", center_tile_x,
             center_tile_y);

     /* Do some dimension calculations.
      */
     num_tiles_up = (center_tile_y / IH_TILE_BASE_HEIGHT) + 1;
     num_tiles_down =
         ((ih.display_height -
           (center_tile_y +
            IH_TILE_BASE_HEIGHT)) / IH_TILE_BASE_HEIGHT) + 1;
     num_tiles_left = (center_tile_x / IH_TILE_BASE_WIDTH) + 1;
     num_tiles_right =
         ((ih.display_width -
           (center_tile_x + IH_TILE_BASE_WIDTH)) / IH_TILE_BASE_WIDTH) + 1;
     fprintf(stderr, "num_tiles_up,down,left,right = %d,%d,%d,%d\n",
             num_tiles_up, num_tiles_down, num_tiles_left,
             num_tiles_right);

     num_tiles_x = (ih.display_width / IH_TILE_BASE_WIDTH) + 1;
     num_tiles_y = (ih.display_height / IH_TILE_BASE_HEIGHT) + 1;
     fprintf(stderr, "num_tiles_x,y = %d,%d\n", num_tiles_x, num_tiles_y);

     for(y = 0; y < num_tiles_y; y++)
     {
          int             pixel_x, pixel_y;
          int             dungeon_x, dungeon_y;

          pixel_y =
              center_tile_y -
              ((num_tiles_up - y) * IH_TILE_BASE_HEIGHT) -
              (IH_TILE_BASE_HEIGHT / 2) - (IH_TILE_ACTUAL_HEIGHT -
                                           IH_TILE_BASE_HEIGHT);

          /* Draw staggered row.
           */
          for(x = 0; x < num_tiles_x + 1; x++)
          {
               pixel_x =
                   center_tile_x -
                   ((num_tiles_left - x) * IH_TILE_BASE_WIDTH) -
                   (IH_TILE_BASE_WIDTH / 2) -
                   (IH_TILE_ACTUAL_WIDTH - IH_TILE_BASE_WIDTH);

               fprintf(stderr, "(staggered) pixel_x,y = %d,%d\n",
                       pixel_x, pixel_y);

               dungeon_x =
                   p_ptr->px - (num_tiles_left - x) - num_tiles_up - 1;
               dungeon_y = p_ptr->py + num_tiles_left - (num_tiles_up - y);

               fprintf(stderr, "(staggered) dungeon_x,y = %d,%d\n",
                       dungeon_x, dungeon_y);

               IH_DrawTile(pixel_x, pixel_y, dungeon_x, dungeon_y);
               IH_DrawObject(pixel_x, pixel_y, dungeon_x, dungeon_y);
#if 0
               IH_DrawCreature(pixel_x, pixel_y, dungeon_x, dungeon_y);
#endif
          }

          /* Draw "normal" row.
           */
          for(x = 0; x < num_tiles_x + 1; x++)
          {
               pixel_x =
                   center_tile_x -
                   ((num_tiles_left - x) * IH_TILE_BASE_WIDTH) -
                   (IH_TILE_ACTUAL_WIDTH - IH_TILE_BASE_WIDTH);

               fprintf(stderr, "(normal) pixel_x,y = %d,%d\n",
                       pixel_x, pixel_y);

               dungeon_x = p_ptr->px - (num_tiles_left - x) - num_tiles_up;
               dungeon_y = p_ptr->py + num_tiles_left - (num_tiles_up - y);

               fprintf(stderr, "(normal) dungeon_x,y = %d,%d\n",
                       dungeon_x, dungeon_y);

               IH_DrawTile(pixel_x,
                           pixel_y + (IH_TILE_BASE_HEIGHT / 2),
                           dungeon_x, dungeon_y);
               IH_DrawObject(pixel_x,
                             pixel_y + (IH_TILE_BASE_HEIGHT / 2),
                             dungeon_x, dungeon_y);
#if 0
               IH_DrawCreature(pixel_x,
                               pixel_y + (IH_TILE_BASE_HEIGHT / 2),
                               dungeon_x, dungeon_y);
#endif
          }
     }
}
