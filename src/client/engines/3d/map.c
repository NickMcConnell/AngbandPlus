
/* $Id: map.c,v 1.3 2003/04/16 17:30:20 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

#ifdef BUILD_3D_ENGINE

/* Standard headers */
#include <stdio.h>
#include <math.h>

/* SDL headers */
#include "SDL_image.h"

/* Internal headers */
#include "ironhells.h"
#include "list.h"
#include "platform/platform.h"
#include "path.h"
#include "displays/iso/scene.h"
#include "displays/iso/tile.h"

static SceneObject *tiles[IH_MAX_TILES];
static int      tile_count = 0;
static SceneObject *objects[IH_MAX_OBJECTS];
static int      object_count = 0;
static SceneObject *creatures[IH_MAX_CREATURES];
static int      creature_count = 0;

static SceneObject *player_image = NULL;

static SceneObject *
IH_LoadTile(cptr filename)
{
     SceneObject    *object = NULL;
     char           *path_data = NULL;
     char           *file;

     /* Paranoia.
      */
     if(!filename)
          return NULL;

     /* Determine path to tiles.
      */
     path_data = IH_GetDataDir("gfx");

     /* Create full path to file.
      */
     file = IH_PathBuild(path_data, "tile", filename, NULL);
     if(file)
     {
          /* Create a scene object.
           */
          object = IH_SceneObjectAlloc
              (IH_SCENE_OBJECT_TYPE_TILE, IH_SCENE_OBJECT_TILE);
          if(object)
          {
               /* Load the image data.
                */
               object->image.surface =
                   IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
               if(!object->image.surface)
               {
                    /* Oops!  Get rid of the object.
                     */
                    IH_SceneObjectFree(object);
                    object = NULL;
               }
          }

          rnfree(file);
     }

     rnfree(path_data);

     return object;
}

static SceneObject *
IH_LoadObject(cptr filename)
{
     SceneObject    *object = NULL;
     char           *path_data = NULL;
     char           *file;

     /* Paranoia.
      */
     if(!filename)
          return NULL;

     /* Determine path to objects.
      */
     path_data = IH_GetDataDir("gfx");

     file = IH_PathBuild(path_data, "object", filename, NULL);
     if(file)
     {
          object = IH_SceneObjectAlloc
              (IH_SCENE_OBJECT_TYPE_OBJECT, IH_SCENE_OBJECT_OBJECT);
          if(object)
          {
               object->image.surface =
                   IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
               if(!object->image.surface)
               {
                    IH_SceneObjectFree(object);
                    object = NULL;

                    fprintf(stderr,
                            "IH_LoadObject(): Unable to load object image: %s: %s\n",
                            file, IMG_GetError());
               }
          }

          rnfree(file);
     }

     rnfree(path_data);

     return object;
}

#ifdef EXT_MAP_INFO
static SceneObject *
IH_LoadCreature(cptr filename)
{
     SceneObject    *object = NULL;
     char           *path_data = NULL;
     char           *file;

     /* Paranoia.
      */
     if(!filename)
          return NULL;

     /* Determine path to creatures.
      */
     path_data = IH_GetDataDir("gfx");

     file = IH_PathBuild(path_data, "creature", filename, NULL);
     if(file)
     {
          object = IH_SceneObjectAlloc
              (IH_SCENE_OBJECT_TYPE_CREATURE, IH_SCENE_OBJECT_CREATURE);
          if(object)
          {
               object->image.surface =
                   IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
               if(!object->image.surface)
               {
                    IH_SceneObjectFree(object);
                    object = NULL;

                    fprintf(stderr,
                            "IH_LoadCreature(): Unable to load creature image: %s: %s\n",
                            file, IMG_GetError());
               }
          }

          rnfree(file);
     }

     rnfree(path_data);

     return object;
}
#endif

static SceneObject *
IH_LoadPlayer(cptr race_name,
              cptr class_name,
              cptr gender_name)
{
     SceneObject    *object = NULL;
     char           *path_data = NULL;
     char           *file, *file_wg, race_buf[100], player_file[100],
         player_file_wg[100];

     /* Paranoia.
      */
     if(!race_name || !class_name)
          return NULL;

     /* Determine path to creatures.
      */
     path_data = IH_GetDataDir("gfx");

     my_strcpy(player_file, class_name, sizeof(player_file));
     if(gender_name)
     {
          my_strcat(player_file_wg, "-", sizeof(player_file_wg));
          my_strcat(player_file_wg, gender_name, sizeof(player_file_wg));
     }
     my_strcat(player_file, "." IH_IMAGE_FORMAT_EXT, sizeof(player_file));
     my_strcat(player_file_wg, "." IH_IMAGE_FORMAT_EXT,
               sizeof(player_file_wg));

     /* Make sure everything is lowercase.
      */
     my_strcpy(race_buf, race_name, sizeof(race_buf));
     string_lower(race_buf);
     string_lower(player_file);

     file =
         IH_PathBuild(path_data, "creature", "player", race_buf,
                      player_file, NULL);
     file_wg =
         IH_PathBuild(path_data, "creature", "player", race_buf,
                      player_file_wg, NULL);
     if(file || file_wg)
     {
          object = IH_SceneObjectAlloc
              (IH_SCENE_OBJECT_TYPE_CREATURE, IH_SCENE_OBJECT_CREATURE);
          if(object)
          {
               SDL_Surface    *image;

               image = IMG_Load_RW(SDL_RWFromFile(file_wg, "rb"), 1);
               if(!image)
                    image = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);

               object->image.surface = image;

               if(!image)
               {
                    IH_SceneObjectFree(object);
                    object = NULL;

                    fprintf(stderr,
                            "IH_LoadCreature(): Unable to load player image: %s: %s\n",
                            file, IMG_GetError());
               }
          }

          if(file)
               rnfree(file);
          if(file_wg)
               rnfree(file_wg);
     }

     rnfree(path_data);

     return object;
}

static void
IH_ProcessManifestFile(cptr manifest_file,
                       SceneObject ** obj_array,
                       SceneObject * (*loader_func) (cptr),
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
     tile_num = PICT(tap, tcp);
#ifdef DEBUG
     fprintf(stderr,
             "IH_DrawTile(): (%d,%d): tap=%d, tcp=%d (%c), tile_num = %d\n",
             dungeon_x, dungeon_y, tap, tcp, tcp, tile_num);
#endif

     /* Paranoia.
      */
     if(tile_num >= IH_MAX_TILES)
          return;
     if(!tiles[tile_num])
          return;

     /* Get the tile image.
      */
     tile = tiles[tile_num]->image.surface;
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

     /* If it's the same thing as the tile, don't draw it.
      */
     if(ocp == tcp)
          return;

     obj_num = PICT(oap, ocp);
#ifdef DEBUG
     fprintf(stderr,
             "IH_DrawObject(): (%d,%d): oap=%d, ocp=%d (%c), obj_num = %d\n",
             dungeon_x, dungeon_y, oap, ocp, ocp, obj_num);
#endif
     if(obj_num >= IH_MAX_OBJECTS)
          return;
     if(!objects[obj_num])
          return;

#ifdef DEBUG
     fprintf(stderr, "IH_DrawObject(): obj_num = %d\n", obj_num);
#endif

     /* Get the object image.
      */
     obj = objects[obj_num]->image.surface;
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
     SDL_Surface    *creature = NULL;
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

     /* If it's the same thing as the tile, don't draw it.
      */
     if(ccp == tcp)
          return;

     creature_num = PICT(cap, ccp);
#ifdef DEBUG
     fprintf(stderr,
             "IH_DrawCreature(): (%d,%d): cap=%d, ccp=%d (%c), creature_num = %d\n",
             dungeon_x, dungeon_y, cap, ccp, ccp, creature_num);
#endif
     if(creature_num >= IH_MAX_CREATURES)
          return;
     if(!creatures[creature_num])
          return;

#ifdef DEBUG
     fprintf(stderr, "IH_DrawCreature(): creature_num = %d\n",
             creature_num);
#endif

     /* Hack -- Are we drawing the player?
      */
     if(ccp == '@')
     {
          /* Hack -- Is the player image loaded?
           */
          if(!player_image)
          {
               player_sex     *gender_ptr;
               player_race    *race_ptr;
               player_class   *class_ptr;
               char           *race_str, *class_str, *gender_str;

               race_ptr = &p_info[p_ptr->prace];
               race_str = p_name + race_ptr->name;

               class_ptr = &c_info[p_ptr->pclass];
               class_str = c_name + class_ptr->name;

               gender_ptr = &sex_info[p_ptr->psex];
               gender_str = gender_ptr->title;

               player_image =
                   IH_LoadPlayer(race_str, class_str, gender_str);
          }

          if(player_image)
               creature = player_image->image.surface;
     }
     else
     {
          /* Get the creature image.
           */
          creature = creatures[creature_num]->image.surface;
     }

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
          pos.y.pixel =
              pixel_y + IH_TILE_ACTUAL_HEIGHT - (IH_TILE_BASE_HEIGHT / 2);

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
     IH_ProcessManifestFile(manifest_file, tiles, IH_LoadTile,
                            IH_MAX_TILES);
     rnfree(manifest_file);
     rnfree(path_manifest);

     /* Process the object manifest.
      */
     path_manifest = IH_PathBuild(path_gfx, "object", NULL);
     manifest_file = IH_PathBuild(path_manifest, "MANIFEST", NULL);
     fprintf(stderr, "IH_InitTiles(): process objects (file = '%s')\n",
             manifest_file);
     IH_ProcessManifestFile(manifest_file, objects, IH_LoadObject,
                            IH_MAX_OBJECTS);
     rnfree(manifest_file);
     rnfree(path_manifest);

     /* Process the creature manifest.
      */
     path_manifest = IH_PathBuild(path_gfx, "creature", NULL);
     manifest_file = IH_PathBuild(path_manifest, "MANIFEST", NULL);
     fprintf(stderr, "IH_InitTiles(): process creatures (file = '%s')\n",
             manifest_file);
     IH_ProcessManifestFile(manifest_file, creatures,
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
     int             center_tile_x, center_tile_y;
     int             start_tile_x, start_tile_y;
     int             num_tiles_left, num_tiles_right;
     int             num_tiles_up, num_tiles_down;
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

#endif /* BUILD_3D_ENGINE */
