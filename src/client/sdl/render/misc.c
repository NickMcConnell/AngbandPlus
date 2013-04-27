
/* $Id: misc.c,v 1.11 2003/03/23 06:10:27 cipher Exp $ */

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
#include "file.h"
#include "sdl/scene.h"
#include "sdl/render.h"
#include "path.h"
#include "sdl/render/misc.h"
#include "sdl/setup.h"

errr
IH_LoadLMX(void)
{
     SceneObject    *object;
     errr            rc = 0;
     char           *path_data;
     char           *path_lmx, *path_xp_bar, *path_life_bar,
         *path_mana_bar;

     path_data = IH_GetDataDir("gfx");

     /* Load the decoration.
      */
     path_lmx =
         IH_PathBuild(path_data, "lmx-deco." IH_IMAGE_FORMAT_EXT, NULL);

     object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_MISC,
                                  IH_SCENE_OBJECT_LMXDECO);
     if(object)
     {
          IH_ListAppend(&ih.misc, (void *) object);

          object->data.image =
              IMG_Load_RW(SDL_RWFromFile(path_lmx, "rb"), 1);
          if(!object->data.image)
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_lmx, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
     }

     /* Load the decoration.
      */
     path_life_bar =
         IH_PathBuild(path_data, "life-bar." IH_IMAGE_FORMAT_EXT, NULL);

     object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_MISC,
                                  IH_SCENE_OBJECT_LIFE);
     if(object)
     {
          IH_ListAppend(&ih.misc, (void *) object);

          object->data.image =
              IMG_Load_RW(SDL_RWFromFile(path_life_bar, "rb"), 1);
          if(!object->data.image)
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_life_bar, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
     }

     /* Load the decoration.
      */
     path_mana_bar =
         IH_PathBuild(path_data, "mana-bar." IH_IMAGE_FORMAT_EXT, NULL);

     object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_MISC,
                                  IH_SCENE_OBJECT_MANA);
     if(object)
     {
          IH_ListAppend(&ih.misc, (void *) object);

          object->data.image =
              IMG_Load_RW(SDL_RWFromFile(path_mana_bar, "rb"), 1);
          if(!object->data.image)
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_mana_bar, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
     }

     /* Load the decoration.
      */
     path_xp_bar =
         IH_PathBuild(path_data, "xp-bar." IH_IMAGE_FORMAT_EXT, NULL);

     object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_MISC,
                                  IH_SCENE_OBJECT_XP);
     if(object)
     {
          IH_ListAppend(&ih.misc, (void *) object);

          object->data.image =
              IMG_Load_RW(SDL_RWFromFile(path_xp_bar, "rb"), 1);
          if(!object->data.image)
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_xp_bar, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
     }

     /* Free the path variables.
      */
     rnfree(path_data);
     rnfree(path_lmx);
     rnfree(path_life_bar);
     rnfree(path_mana_bar);
     rnfree(path_xp_bar);

     return rc;
}

errr
IH_LoadImages(void)
{
     errr            rc = 0;
     char           *path_images;
     int             i;

     path_images = IH_GetDataDir("gfx");

     /* Load the splash and background images.
      */
     {
          char           *file;

          file =
              IH_PathBuild(path_images, "splash." IH_IMAGE_FORMAT_EXT,
                           NULL);

          ih.splash = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
          if(!ih.splash)
          {
               fprintf(stderr, "Unable to load splash image: %s: %s\n",
                       file, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }

          rnfree(file);

          file =
              IH_PathBuild(path_images, "background." IH_IMAGE_FORMAT_EXT,
                           NULL);

          ih.background = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
          if(!ih.background)
          {
               fprintf(stderr, "Unable to load background image: %s: %s\n",
                       file, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }

          rnfree(file);
     }

#if 0
     /* Load any graphic images.
      */
     for(i = 0; graphics[i].name; i++)
     {
          char           *file;
          char           *filename;
          int             filename_len;

          filename_len =
              strlen(graphics[i].name) + strlen(IH_IMAGE_FORMAT_EXT) + 2;
          filename = ralloc(filename_len);
          my_strcpy(filename, graphics[i].name, filename_len);
          my_strcat(filename, ".", filename_len);
          my_strcat(filename, IH_IMAGE_FORMAT_EXT, filename_len);

          file = IH_PathBuild(path_images, filename, NULL);
          rnfree(filename);

          if(file)
          {
               SceneObject    *object;

               object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_IMAGE,
                                            graphics[i].object);
               if(object)
               {
                    IH_ListAppend(&ih.icons, (void *) object);

                    object->data.image =
                        IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
                    if(!object->data.image)
                    {
                         fprintf(stderr,
                                 "Unable to load icon image: %s: %s\n",
                                 file, IMG_GetError());
                         rc = IH_ERROR_CANT_LOAD_ICON;
                    }
               }
          }
     }
#endif

     rnfree(path_images);

     return rc;
}

void
IH_PositionMisc(void)
{
     ihNode         *node;
     int             lmx_x = 0;

     for(node = IH_ListFirst(&ih.misc);
         node; node = IH_ListNext(&ih.misc, node))
     {
          SceneObject    *object;

          object = (SceneObject *) node->data;
          if(!object)
               continue;

          switch (object->object)
          {
               case IH_SCENE_OBJECT_LMXDECO:
                    lmx_x = object->x =
                        (ih.display_width - object->data.image->w) / 2;
                    object->y = 0;
                    break;

               case IH_SCENE_OBJECT_LIFE:
                    object->x = lmx_x + 19;
                    object->y = 0;
                    break;

               case IH_SCENE_OBJECT_MANA:
                    object->x = lmx_x + 141;
                    object->y = 0;
                    break;

               case IH_SCENE_OBJECT_XP:
                    object->x = lmx_x + 263;
                    object->y = 0;
                    break;
          }
     }
}

void
IH_RenderMisc(void)
{
     ihNode         *node;
     bool            shade = FALSE;

     for(node = IH_ListFirst(&ih.misc);
         node; node = IH_ListNext(&ih.misc, node))
     {
          SceneObject    *object;
          SDL_Rect        srect, drect;
          float           perc = 1.0f;

          object = (SceneObject *) node->data;
          if(!object)
               continue;

          if(object->type != IH_SCENE_OBJECT_TYPE_MISC)
               continue;
          if(!object->data.image)
               continue;

          /* Set up the source and destination rectangles.
           */
          srect.x = 0;
          srect.y = 0;
          srect.w = object->data.image->w;
          srect.h = object->data.image->h;

          drect.x = object->x;
          drect.y = object->y;

          /* Handle life, mana, and xp bar levels.
           */
          switch (object->object)
          {
               case IH_SCENE_OBJECT_LIFE:
#ifdef DEBUG
                    fprintf(stderr, "chp = %d, mhp = %d\n", p_ptr->chp,
                            p_ptr->mhp);
#endif
                    if(p_ptr->mhp)
                         perc = (p_ptr->chp / p_ptr->mhp);
                    else
                         perc = 0.0f;
#ifdef DEBUG
                    fprintf(stderr, "perc = %d%%\n", perc * 100);
#endif

                    shade = TRUE;

                    break;

               case IH_SCENE_OBJECT_MANA:
#ifdef DEBUG
                    fprintf(stderr, "csp = %d, msp = %d\n", p_ptr->csp,
                            p_ptr->msp);
#endif
                    if(p_ptr->msp)
                         perc = (p_ptr->csp / p_ptr->msp);
                    else
                         perc = 0.0f;
#ifdef DEBUG
                    fprintf(stderr, "perc = %d%%\n", perc * 100);
#endif

                    shade = TRUE;

                    break;

               case IH_SCENE_OBJECT_XP:
#ifdef DEBUG
                    fprintf(stderr, "exp = %ld, max_exp = %ld\n",
                            p_ptr->exp, p_ptr->max_exp);
#endif
                    if(p_ptr->max_exp)
                         perc = (p_ptr->exp / p_ptr->max_exp);
                    else
                         perc = 0.0f;
#ifdef DEBUG
                    fprintf(stderr, "perc = %d%%\n", perc * 100);
#endif

                    shade = TRUE;

                    break;
          }
          srect.w *= perc;

          /* Blit the alpha surface first.
           */
          if(shade)
               IH_ShadeArea(object->x, object->y,
                            object->data.image->w, object->data.image->h);

          /* Blit the image.
           */
          SDL_BlitSurface(object->data.image, &srect, ih.screen, &drect);

     }
}

void
IH_RenderError(void)
{
     char           *p, s;
     char            buf[10][200];
     int             widest, len, width, line;
     bool            cont;

#if 0
     if(!ih.err_message)
     {
          ih.err_shown = FALSE;
          return;
     }

     s = p = ih.err_message;
     line = 0;

     do
     {
          len = 0;

          cont = FALSE;
          while(*p)
          {
               if(*p == '\n')
               {
                    cont = TRUE;
                    break;
               }

               len++;
          }

          if(len)
          {
               my_strcpy(buf[line], s, min(sizeof(buf[line]) - 1, len));
          }

          width = IH_GetTextWidth(IH_FONT_NORMAL, buf[line]);
          if(width > widest)
               widest = width;

          if(*p == '\n')
          {
               s = p + 1;
               line++;
          }

          p++;

     } while(cont && (line < 5));

#endif
}

void
IH_ShadeArea(int x,
             int y,
             int w,
             int h)
{
     SDL_Rect        srect, drect;

#ifdef DEBUG
     fprintf(stderr, "IH_ShadeArea()\n");
#endif

     if(!ih.shader)
          return;

#ifdef DEBUG
     fprintf(stderr,
             "IH_ShadeArea(): check if the shader is the right size\n");
#endif
     if(w > ih.shader->w || h > ih.shader->h)
     {
#ifdef DEBUG
          fprintf(stderr, "IH_ShadeArea(): IH_CreateShader\n");
#endif
          IH_CreateShader(w, h);
     }

#ifdef DEBUG
     fprintf(stderr, "IH_ShadeArea(): double-check the shader\n");
#endif
     if(!ih.shader)
          return;

     /* Setup the rectangles.
      */
#ifdef DEBUG
     fprintf(stderr, "IH_ShadeArea(): setup the rectangles\n");
#endif
     srect.x = 0;
     srect.y = 0;
     srect.w = w;
     srect.h = h;

     drect.x = x;
     drect.y = y;

     /* Draw it.
      */
#ifdef DEBUG
     fprintf(stderr, "IH_ShadeArea(): draw the surface\n");
#endif
     SDL_BlitSurface(ih.shader, &srect, ih.screen, &drect);

#ifdef DEBUG
     fprintf(stderr, "IH_ShadeArea(): return\n");
#endif
}
