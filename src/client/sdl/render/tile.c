
/* $Id: tile.c,v 1.17 2003/03/24 06:04:53 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include <stdio.h>
#include <math.h>

#include "SDL_image.h"

#include "ironhells.h"
#include "list.h"
#include "file.h"
#include "path.h"
#include "sdl/scene.h"
#include "sdl/render/tile.h"

static SDL_Surface *
IH_LoadTile(cptr filename)
{
     SDL_Surface    *image = NULL;
     char           *path_data = NULL, *path_tile = NULL;
     char           *file;

     fprintf(stderr, "IH_LoadTile()\n");

     /* Paranoia.
      */
     if(!filename)
          return NULL;

     /* Determine path to tiles.
      */
     fprintf(stderr, "IH_LoadTile(): build path\n");
     path_data = IH_GetDataDir("gfx");
     path_tile = IH_PathBuild(path_data, "tile", NULL);
     fprintf(stderr, "IH_LoadTile(): path_tile = '%s'\n", path_tile);

     /* Create full path to file.
      */
     file = IH_PathBuild(path_tile, filename, NULL);
     if(file)
     {
          SceneObject    *object;

          fprintf(stderr, "IH_LoadTile(): create scene object\n");
          /* Create a scene object.
           */
          object = IH_SceneObjectAlloc
              (IH_SCENE_OBJECT_TYPE_TILE, IH_SCENE_OBJECT_TILE);
          if(object)
          {
               /* Load the image data.
                */
               fprintf(stderr,
                       "IH_LoadTile(): load image from file '%s'\n", file);
               object->data.image =
                   IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
               if(object->data.image)
               {
                    /* Remember the image.
                     */
                    fprintf(stderr,
                            "IH_LoadTile(): remember image data\n");
                    image = object->data.image;

                    /* Put the object in a list.
                     */
                    fprintf(stderr, "IH_LoadTile(): add object to list\n");
                    IH_ListAppend(&ih.tiles, (void *) object);
               }
               else
               {
                    /* Oops!  Get rid of the object.
                     */
                    fprintf(stderr,
                            "IH_LoadTile(): get rid of the object\n");
                    IH_SceneObjectFree(object);

                    fprintf(stderr,
                            "IH_LoadTile(): Unable to load tile image: %s: %s\n",
                            file, IMG_GetError());
               }
          }

          rnfree(file);
     }

     rnfree(path_tile);
     rnfree(path_data);

     fprintf(stderr, "IH_LoadTile(): return %p\n", image);
     return image;
}

static SDL_Surface *
IH_LoadObject(cptr filename)
{
     SDL_Surface    *image = NULL;
     char           *path_data = NULL, *path_obj = NULL;
     char           *file;

     /* Paranoia.
      */
     if(!filename)
          return NULL;

     /* Determine path to objects.
      */
     path_data = IH_GetDataDir("gfx");
     path_obj = IH_PathBuild(path_data, "object", NULL);

     file = IH_PathBuild(path_obj, filename, NULL);
     if(file)
     {
          SceneObject    *object;

          object = IH_SceneObjectAlloc
              (IH_SCENE_OBJECT_TYPE_OBJECT, IH_SCENE_OBJECT_OBJECT);
          if(object)
          {
               object->data.image =
                   IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
               if(object->data.image)
               {
                    image = object->data.image;

                    IH_ListAppend(&ih.objects, (void *) object);
               }
               else
               {
                    IH_SceneObjectFree(object);

                    fprintf(stderr,
                            "IH_LoadObject(): Unable to load object image: %s: %s\n",
                            file, IMG_GetError());
               }
          }

          rnfree(file);
     }

     rnfree(path_obj);
     rnfree(path_data);

     return image;
}

#ifdef EXT_MAP_INFO
static SDL_Surface *
IH_LoadCreature(cptr filename)
{
     SDL_Surface    *image = NULL;
     char           *path_data = NULL, *path_creature = NULL;
     char           *file;

     /* Paranoia.
      */
     if(!filename)
          return NULL;

     /* Determine path to creatures.
      */
     path_data = IH_GetDataDir("gfx");
     path_creature = IH_PathBuild(path_data, "creature", NULL);

     file = IH_PathBuild(path_creature, filename, NULL);
     if(file)
     {
          SceneObject    *object;

          object = IH_SceneObjectAlloc
              (IH_SCENE_OBJECT_TYPE_CREATURE, IH_SCENE_OBJECT_CREATURE);
          if(object)
          {
               object->data.image =
                   IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
               if(object->data.image)
               {
                    image = object->data.image;

                    IH_ListAppend(&ih.creatures, (void *) object);
               }
               else
               {
                    IH_SceneObjectFree(object);

                    fprintf(stderr,
                            "IH_LoadCreature(): Unable to load creature image: %s: %s\n",
                            file, IMG_GetError());
               }
          }

          rnfree(file);
     }

     rnfree(path_creature);
     rnfree(path_data);

     return image;
}
#endif

static void
IH_ProcessManifestFile(cptr manifest_file,
                       SDL_Surface ** obj_array,
                       SDL_Surface * (*loader_func) (cptr),
                       int max_objs)
{
     FILE           *fp;

     fprintf(stderr, "IH_ProcessManifestFile()\n");

     if(!manifest_file || !obj_array || !loader_func || !max_objs)
          return;

     fprintf(stderr, "IH_ProcessManifestFile(): open manifest file '%s'\n",
             manifest_file);
     fp = fopen(manifest_file, "r");
     if(fp)
     {
          char            buf[200];

          fprintf(stderr, "IH_ProcessManifestFile(): get contents\n");
          while(fgets(buf, sizeof(buf), fp))
          {
               byte           *num = NULL, *f = NULL, *c;

               fprintf(stderr, "IH_ProcessManifestFile(): buf = '%s'\n",
                       buf);
               /* Skip blank lines and "comments."
                */
               if(!buf[0] ||
                  buf[0] == '\n' || buf[0] == '\r' || buf[0] == '#')
                    continue;

               /* Get rid of newline characters.
                */
               fprintf(stderr,
                       "IH_ProcessManifestFile(): get rid of newlines\n");
               if(c = strchr(buf, '\r'))
                    *c = 0;
               if(c = strchr(buf, '\n'))
                    *c = 0;

               /* Look for whitespace (space or tab).
                */
               fprintf(stderr,
                       "IH_ProcessManifestFile(): look for whitespace\n");
               c = strchr(buf, ' ');
               if(!c)
                    c = strchr(buf, '\t');

               /* Find first non-whitespace character.
                */
               fprintf(stderr,
                       "IH_ProcessManifestFile(): find non-whitespace\n");
               while(isspace(*c))
                    c++;
               c--;

               if(c)
               {
                    fprintf(stderr,
                            "IH_ProcessManifestFile(): separate number and name\n");
                    /* Separate the item number and filename.
                     */
                    *c = 0;
                    num = buf;
                    f = c + 1;
               }

               fprintf(stderr,
                       "IH_ProcessManifestFile(): check for number and filename\n");
               if(num && f)
               {
                    int             inum;

                    fprintf(stderr,
                            "IH_ProcessManifestFile(): convert '%s' to num\n",
                            num);
                    /* Get item number.
                     */
                    inum = atoi(num);

                    /* Check for out of bounds.
                     */
                    fprintf(stderr,
                            "IH_ProcessManifestFile(): check for bounds\n");
                    if(inum < 0)
                         continue;
                    if(inum > max_objs)
                         continue;

                    /* Load the object?
                     */
                    fprintf(stderr,
                            "IH_ProcessManifestFile(): load the object?\n");
                    if(!obj_array[inum])
                    {
                         fprintf(stderr,
                                 "IH_ProcessManifestFile(): call the loader\n");
                         obj_array[inum] = (*loader_func) (f);
                    }
               }
          }

          fprintf(stderr, "IH_ProcessManifestFile(): close the file\n");
          fclose(fp);
     }
     fprintf(stderr, "IH_ProcessManifestFile(): return\n");
}

static void
IH_DrawTile(int pixel_x,
            int pixel_y,
            int dungeon_x,
            int dungeon_y)
{
     SDL_Surface    *tile;
     SDL_Rect        rect;
     byte            tap;
     char            tcp;
     int             tile_num = 0;

     /* Figure out if it will even be displayed on the screen.
      */
     if(dungeon_x < 0 ||
        dungeon_x >= DUNGEON_WID ||
        dungeon_y < 0 || dungeon_y >= DUNGEON_HGT)
          return;

     /* Get the map grid.
      */
#ifdef EXT_MAP_INFO
     map_info(dungeon_y, dungeon_x, NULL, NULL, NULL, NULL, &tap, &tcp);
#else
     map_info(dungeon_y, dungeon_x, NULL, NULL, &tap, &tcp);
#endif
#ifdef DEBUG
     fprintf(stderr,
             "IH_DrawTile(): map_info says (%d,%d) contains: tap=%d, tcp=%d (%c)\n",
             dungeon_x, dungeon_y, tap, tcp, tcp);
#endif
     tile_num = PICT(tap, tcp);

     /* Paranoia.
      */
     if(tile_num >= IH_MAX_TILES)
          return;

     /* Get the tile image.
      */
     tile = ih.tile_array[tile_num];
     if(!tile)
     {
#ifdef TILE_HACK
          SDL_Color       color;
          ihFontPos       pos;
          char            buf[2];

          buf[0] = tcp;
          buf[1] = 0;

          color.r = color.g = color.b = 255;

          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = pixel_x + (IH_TILE_ACTUAL_WIDTH / 2);
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel = pixel_y + (IH_TILE_ACTUAL_HEIGHT / 3);

          IH_RenderText(IH_FONT_LARGE, buf, &pos, color, NULL);
#endif
          return;
     }

     /* If the tile is not a "flat" tile, e.g., a floor or down-staircase,
      * then make it transparent.
      */
#ifdef USE_OBJ_TRANSPARENCY
     if(((dungeon_x == p_ptr->px - 1) ||
         (dungeon_x == p_ptr->px)) &&
        ((dungeon_y == p_ptr->py + 1) || (dungeon_y == p_ptr->py)))
     {
#ifdef DEBUG
          fprintf(stderr, "IH_DrawTile(): Make tile transparent.\n");
#endif
          SDL_SetAlpha(tile, SDL_SRCALPHA, 128);
     }
     else
     {
          SDL_SetAlpha(tile, 0, 0);
     }
#endif /* USE_OBJ_TRANSPARENCY */

     /* Draw the tile.
      */
     rect.x = pixel_x;
     rect.y = pixel_y;

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
     byte            oap, tap;
     char            ocp, tcp;
     int             obj_num = 0;

     /* Figure out if it will even be displayed on the screen.
      */
     if(dungeon_x < 0 ||
        dungeon_x >= DUNGEON_WID ||
        dungeon_y < 0 || dungeon_y >= DUNGEON_HGT)
          return;

     /* Get the map grid.
      */
#ifdef EXT_MAP_INFO
     map_info(dungeon_y, dungeon_x, NULL, NULL, &oap, &ocp, &tap, &tcp);
#else
     map_info(dungeon_y, dungeon_x, &oap, &ocp, &tap, &tcp);
#endif
#ifdef DEBUG
     fprintf(stderr,
             "IH_DrawObject(): map_info says (%d,%d) contains: oap=%d, ocp=%d (%c)\n",
             dungeon_x, dungeon_y, oap, ocp, ocp);

     /* If it's the same thing as the tile, don't draw it.
      */
     if(ocp == tcp)
          return;

#endif
     obj_num = PICT(oap, ocp);
     if(obj_num >= IH_MAX_OBJECTS)
          return;

#ifdef DEBUG
     fprintf(stderr, "IH_DrawObject(): obj_num = %d\n", obj_num);
#endif

     /* Get the object image.
      */
     obj = ih.object_array[obj_num];
     if(!obj)
     {
#ifdef TILE_HACK
          SDL_Color       color;
          ihFontPos       pos;
          char            buf[2];

          buf[0] = ocp;
          buf[1] = 0;

          color.r = color.g = color.b = 255;

          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = pixel_x + (IH_TILE_ACTUAL_WIDTH / 2);
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel = pixel_y + (IH_TILE_ACTUAL_HEIGHT / 3);

          IH_RenderText(IH_FONT_LARGE, buf, &pos, color, NULL);
#endif
          return;
     }

     /* If the object is "in front of" the player, make it transparent, so
      * we can still see the player.
      */
#ifdef USE_OBJ_TRANSPARENCY
     if(((dungeon_x == p_ptr->px - 1) ||
         (dungeon_x == p_ptr->px)) &&
        ((dungeon_y == p_ptr->py + 1) || (dungeon_y == p_ptr->py)))
     {
#ifdef DEBUG
          fprintf(stderr, "IH_DrawTile(): Make tile transparent.\n");
#endif
          SDL_SetAlpha(obj, SDL_SRCALPHA, 128);
     }
     else
     {
          SDL_SetAlpha(obj, 0, 0);
     }
#endif /* USE_OBJ_TRANSPARENCY */

     /* Draw the object.
      */
     rect.x = pixel_x;
     rect.y = pixel_y;

     SDL_BlitSurface(obj, NULL, ih.screen, &rect);
}

#ifdef EXT_MAP_INFO
static void
IH_DrawCreature(int pixel_x,
                int pixel_y,
                int dungeon_x,
                int dungeon_y)
{
     SDL_Surface    *creature;
     SDL_Rect        rect;
     byte            cap, tap;
     char            ccp, tcp;
     int             creature_num = 0;

     /* Figure out if it will even be displayed on the screen.
      */
     if(dungeon_x < 0 ||
        dungeon_x >= DUNGEON_WID ||
        dungeon_y < 0 || dungeon_y >= DUNGEON_HGT)
          return;

     /* Get the map grid.
      */
     map_info(dungeon_y, dungeon_x, &cap, &ccp, NULL, NULL, &tap, &tcp);
#ifdef DEBUG
     fprintf(stderr,
             "IH_DrawCreature(): map_info says (%d,%d) contains: cap=%d, ccp=%d (%c)\n",
             dungeon_x, dungeon_y, cap, ccp, ccp);
#endif

     /* If it's the same thing as the tile, don't draw it.
      */
     if(ccp == tcp)
          return;

     creature_num = PICT(cap, ccp);
     if(creature_num >= IH_MAX_CREATURES)
          return;

#ifdef DEBUG
     fprintf(stderr, "IH_DrawCreature(): creature_num = %d\n",
             creature_num);
#endif

     /* Get the creature image.
      */
     creature = ih.creature_array[creature_num];
     if(!creature)
     {
#ifdef TILE_HACK
          SDL_Color       color;
          ihFontPos       pos;
          char            buf[2];

          buf[0] = ccp;
          buf[1] = 0;

          color.r = color.g = color.b = 255;

          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = pixel_x + (IH_TILE_ACTUAL_WIDTH / 2);
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel = pixel_y + (IH_TILE_ACTUAL_HEIGHT / 3);

          IH_RenderText(IH_FONT_LARGE, buf, &pos, color, NULL);
#endif
          return;
     }

     /* Draw the creature.
      */
     rect.x = pixel_x;
     rect.y = pixel_y;

     SDL_BlitSurface(creature, NULL, ih.screen, &rect);
}
#endif

errr
IH_InitTiles(void)
{
     char           *path_gfx, *path_manifest, *manifest_file;

     fprintf(stderr, "IH_InitTiles()\n");

     path_gfx = IH_GetDataDir("gfx");

     /* Process the tile manifest.
      */
     path_manifest = IH_PathBuild(path_gfx, "tile", NULL);
     manifest_file = IH_PathBuild(path_manifest, "MANIFEST", NULL);
     fprintf(stderr, "IH_InitTiles(): process tiles (file = '%s')\n",
             manifest_file);
     IH_ProcessManifestFile(manifest_file, ih.tile_array, IH_LoadTile,
                            IH_MAX_TILES);
     rnfree(manifest_file);
     rnfree(path_manifest);

     /* Process the object manifest.
      */
     path_manifest = IH_PathBuild(path_gfx, "object", NULL);
     manifest_file = IH_PathBuild(path_manifest, "MANIFEST", NULL);
     fprintf(stderr, "IH_InitTiles(): process objects (file = '%s')\n",
             manifest_file);
     IH_ProcessManifestFile(manifest_file, ih.object_array, IH_LoadObject,
                            IH_MAX_OBJECTS);
     rnfree(manifest_file);
     rnfree(path_manifest);

     /* Process the creature manifest.
      */
     path_manifest = IH_PathBuild(path_gfx, "creature", NULL);
     manifest_file = IH_PathBuild(path_manifest, "MANIFEST", NULL);
     fprintf(stderr, "IH_InitTiles(): process creatures (file = '%s')\n",
             manifest_file);
     IH_ProcessManifestFile(manifest_file, ih.creature_array,
                            IH_LoadCreature, IH_MAX_CREATURES);
     rnfree(manifest_file);
     rnfree(path_manifest);

     rnfree(path_gfx);

     fprintf(stderr, "IH_InitTiles(): return\n");
     return 0;
}

void
IH_RenderTiles(void)
{
     int             center_tile_x, center_tile_y, start_tile_x,
         start_tile_y;
     int             num_tiles_left, num_tiles_right, num_tiles_up,
         num_tiles_down;
     int             num_tiles_x, num_tiles_y;
     int             x, y;

     /* Calculate pixel positions for the center tile (the player).
      */
     center_tile_x =
         ((ih.display_width - IH_TILE_BASE_WIDTH) / 2) -
         ((IH_TILE_ACTUAL_WIDTH - IH_TILE_BASE_WIDTH) / 2);
     center_tile_y =
         ((ih.display_height - IH_TILE_BASE_HEIGHT) / 2) -
         ((IH_TILE_ACTUAL_HEIGHT - IH_TILE_BASE_HEIGHT) / 2);

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

     num_tiles_x = (ih.display_width / IH_TILE_BASE_WIDTH) + 1;
     num_tiles_y = (ih.display_height / IH_TILE_BASE_HEIGHT) + 1;

#if 0
     fprintf(stderr, "Draw map grid.\n");
     for(y = 0; y < DUNGEON_HGT; y++)
     {
          for(x = 0; x < DUNGEON_WID; x++)
          {
               byte            ap;
               char            cp;

               map_info(y, x, NULL, NULL, &ap, &cp);
               if(cp < 33)
                    continue;
               fprintf(stderr, "%c", cp);
          }

          fprintf(stderr, "\n");
     }

     fprintf(stderr, "Draw displayed map grid.\n");
     for(y = 0; y < num_tiles_y; y += 2)
     {
          int             dungeon_x, dungeon_y;
          byte            ap;
          char            cp;

          /* Draw staggered row.
           */
          for(x = 0; x < num_tiles_x + 1; x++)
          {
               dungeon_x =
                   p_ptr->px - num_tiles_left + num_tiles_up + (x - 1) -
                   (y - 1);
               dungeon_y =
                   p_ptr->py - num_tiles_left - num_tiles_up + (x - 1) +
                   (y - 1) - 1;

               map_info(dungeon_y, dungeon_x, NULL, NULL, &ap, &cp);

               if(cp < 33)
                    continue;

               if(dungeon_x < 0 || dungeon_y < 0)
                    fprintf(stderr, "  ");
               else
                    fprintf(stderr, "%c ", cp);
          }
          fprintf(stderr, "\n");

          /* Draw normal row.
           */
          fprintf(stderr, " ");
          for(x = 0; x < num_tiles_x + 1; x++)
          {
               dungeon_x =
                   p_ptr->px - num_tiles_left + num_tiles_up + (x - 1) -
                   (y - 1);
               dungeon_y =
                   p_ptr->py - num_tiles_left - num_tiles_up + (x - 1) +
                   (y - 1);

               map_info(dungeon_y, dungeon_x, NULL, NULL, &ap, &cp);

               if(cp < 33)
                    continue;

               if(dungeon_x < 0 || dungeon_y < 0)
                    fprintf(stderr, "  ");
               else
                    fprintf(stderr, "%c ", cp);
          }
          fprintf(stderr, "\n");
     }
#endif

     for(y = 0; y < num_tiles_y + 1; y++)
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

#ifdef DEBUG
               fprintf(stderr, "(staggered) pixel_x,y = %d,%d\n",
                       pixel_x, pixel_y);
#endif

               dungeon_x =
                   p_ptr->px - num_tiles_left + num_tiles_up + (x - 1) -
                   (y - 1);
               dungeon_y =
                   p_ptr->py - num_tiles_left - num_tiles_up + (x - 1) +
                   (y - 1) - 1;

#ifdef DEBUG
               fprintf(stderr, "(staggered) dungeon_x,y = %d,%d\n",
                       dungeon_x, dungeon_y);
#endif

               IH_DrawTile(pixel_x, pixel_y, dungeon_x, dungeon_y);
               IH_DrawObject(pixel_x, pixel_y, dungeon_x, dungeon_y);
#ifdef EXT_MAP_INFO
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

#ifdef DEBUG
               fprintf(stderr, "(normal) pixel_x,y = %d,%d\n",
                       pixel_x, pixel_y);
#endif

               dungeon_x =
                   p_ptr->px - num_tiles_left + num_tiles_up + (x - 1) -
                   (y - 1);
               dungeon_y =
                   p_ptr->py - num_tiles_left - num_tiles_up + (x - 1) +
                   (y - 1);

#ifdef DEBUG
               fprintf(stderr, "(normal) dungeon_x,y = %d,%d\n",
                       dungeon_x, dungeon_y);
#endif

               IH_DrawTile(pixel_x,
                           pixel_y + (IH_TILE_BASE_HEIGHT / 2),
                           dungeon_x, dungeon_y);
               IH_DrawObject(pixel_x,
                             pixel_y + (IH_TILE_BASE_HEIGHT / 2),
                             dungeon_x, dungeon_y);
#ifdef EXT_MAP_INFO
               IH_DrawCreature(pixel_x,
                               pixel_y + (IH_TILE_BASE_HEIGHT / 2),
                               dungeon_x, dungeon_y);
#endif
          }
     }
}
