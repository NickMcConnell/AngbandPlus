
/* $Id: misc.c,v 1.11 2003/04/21 02:31:44 cipher Exp $ */

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
#include "SDL_draw.h"

/* Internal headers */
#include "ironhells.h"
#include "path.h"
#include "engines.h"
#include "object.h"
#include "platform/platform.h"
#include "engines/iso/init.h"
#include "engines/iso/misc.h"

static SDL_Surface *shader = NULL;

static          errr
IH_ISO_LoadLMX(void)
{
     SceneObject    *object;
     errr            rc = 0;
     char           *path_data;
     char           *path_lmx, *path_xp_bar, *path_life_bar,
         *path_mana_bar;
     displayData    *display_data;

fprintf(stderr, "IH_ISO_LoadLMX()\n");

     display_data = (displayData *) ih.display.data;

fprintf(stderr, "IH_ISO_LoadLMX(): get gfx path\n");
     path_data = IH_GetDataDir("gfx");

     /* Load the decoration.
      */
fprintf(stderr, "IH_ISO_LoadLMX(): get lmx path\n");
     path_lmx =
         IH_PathBuild(path_data, "lmx-deco." IH_IMAGE_FORMAT_EXT, NULL);
fprintf(stderr, "IH_ISO_LoadLMX(): path_lmx = %s\n", path_lmx);

fprintf(stderr, "IH_ISO_LoadLMX(): alloc object\n");
     object = IH_SceneObject_Alloc(IH_SCENE_OBJECT_TYPE_MISC,
                                   IH_SCENE_OBJECT_LMXDECO);
     if(object)
     {
fprintf(stderr, "IH_ISO_LoadLMX(): remember lmx object\n");
          display_data->misc_objects[display_data->misc_object_count++] =
              object;

fprintf(stderr, "IH_ISO_LoadLMX(): load lmx image\n");
          object->image.bits =
              (void *)IMG_Load_RW(SDL_RWFromFile(path_lmx, "rb"), 1);
          if(!object->image.bits)
          {
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_lmx, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
          else
          {
               object->image.width = ((SDL_Surface *)object->image.bits)->w;
               object->image.height = ((SDL_Surface *)object->image.bits)->h;
          }
     }

     /* Load the life bar.
      */
fprintf(stderr, "IH_ISO_LoadLMX(): get life bar path\n");
     path_life_bar =
         IH_PathBuild(path_data, "life-bar." IH_IMAGE_FORMAT_EXT, NULL);
fprintf(stderr, "IH_ISO_LoadLMX(): path_life_bar = %s\n", path_life_bar);

fprintf(stderr, "IH_ISO_LoadLMX(): alloc life bar object\n");
     object = IH_SceneObject_Alloc(IH_SCENE_OBJECT_TYPE_MISC,
                                   IH_SCENE_OBJECT_LIFE);
     if(object)
     {
fprintf(stderr, "IH_ISO_LoadLMX(): remember life bar object\n");
          display_data->misc_objects[display_data->misc_object_count++] =
              object;

          object->text.is_dynamic = TRUE;
          object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;
          object->text.x_align = IH_SCENE_OBJECT_TEXT_ALIGN_CENTER;
          object->text.y_align = IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM;
          object->text.font = IH_FONT_NORMAL;

fprintf(stderr, "IH_ISO_LoadLMX(): load life bar image\n");
          object->image.bits =
              (void *)IMG_Load_RW(SDL_RWFromFile(path_life_bar, "rb"), 1);
          if(!object->image.bits)
          {
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_life_bar, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
          else
          {
               object->image.width = ((SDL_Surface *)object->image.bits)->w;
               object->image.height = ((SDL_Surface *)object->image.bits)->h;
          }
     }

     /* Load the mana bar.
      */
fprintf(stderr, "IH_ISO_LoadLMX(): get mana bar path\n");
     path_mana_bar =
         IH_PathBuild(path_data, "mana-bar." IH_IMAGE_FORMAT_EXT, NULL);
fprintf(stderr, "IH_ISO_LoadLMX(): path_mana_bar = %s\n", path_mana_bar);

fprintf(stderr, "IH_ISO_LoadLMX(): alloc mana bar object\n");
     object = IH_SceneObject_Alloc(IH_SCENE_OBJECT_TYPE_MISC,
                                   IH_SCENE_OBJECT_MANA);
     if(object)
     {
fprintf(stderr, "IH_ISO_LoadLMX(): remember mana bar object\n");
          display_data->misc_objects[display_data->misc_object_count++] =
              object;

          object->text.is_dynamic = TRUE;
          object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;
          object->text.x_align = IH_SCENE_OBJECT_TEXT_ALIGN_CENTER;
          object->text.y_align = IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM;
          object->text.font = IH_FONT_NORMAL;

fprintf(stderr, "IH_ISO_LoadLMX(): load mana bar image\n");
          object->image.bits =
              (void *)IMG_Load_RW(SDL_RWFromFile(path_mana_bar, "rb"), 1);
          if(!object->image.bits)
          {
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_mana_bar, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
          else
          {
               object->image.width = ((SDL_Surface *)object->image.bits)->w;
               object->image.height = ((SDL_Surface *)object->image.bits)->h;
          }
     }

     /* Load the experience bar.
      */
fprintf(stderr, "IH_ISO_LoadLMX(): get xp bar path\n");
     path_xp_bar =
         IH_PathBuild(path_data, "xp-bar." IH_IMAGE_FORMAT_EXT, NULL);
fprintf(stderr, "IH_ISO_LoadLMX(): path_xp_bar = %s\n", path_xp_bar);

fprintf(stderr, "IH_ISO_LoadLMX(): alloc xp bar object\n");
     object = IH_SceneObject_Alloc(IH_SCENE_OBJECT_TYPE_MISC,
                                   IH_SCENE_OBJECT_XP);
     if(object)
     {
fprintf(stderr, "IH_ISO_LoadLMX(): remember xp bar object\n");
          display_data->misc_objects[display_data->misc_object_count++] =
              object;

          object->text.is_dynamic = TRUE;
          object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;
          object->text.x_align = IH_SCENE_OBJECT_TEXT_ALIGN_CENTER;
          object->text.y_align = IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM;
          object->text.font = IH_FONT_NORMAL;

fprintf(stderr, "IH_ISO_LoadLMX(): load image for xp bar\n");
          object->image.bits =
              (void *)IMG_Load_RW(SDL_RWFromFile(path_xp_bar, "rb"), 1);
          if(!object->image.bits)
          {
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_xp_bar, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
          else
          {
               object->image.width = ((SDL_Surface *)object->image.bits)->w;
               object->image.height = ((SDL_Surface *)object->image.bits)->h;
          }
     }

     /* Free the path variables.
      */
fprintf(stderr, "IH_ISO_LoadLMX(): free strings\n");
     rnfree(path_data);
     rnfree(path_lmx);
     rnfree(path_life_bar);
     rnfree(path_mana_bar);
     rnfree(path_xp_bar);

fprintf(stderr, "IH_ISO_LoadLMX(): return %d\n", rc);
     return rc;
}

static          errr
IH_ISO_LoadImages(void)
{
     isoEngineData  *engine_data;
     errr            rc = 0;
     char           *path_images;
     int             i;

     fprintf(stderr, "IH_ISO_LoadImages()\n");

     engine_data =
         (isoEngineData *) ((displayData *) ih.display.data)->engine_data;

     fprintf(stderr, "IH_ISO_LoadImages(): get the gfx path\n");
     path_images = IH_GetDataDir("gfx");
     fprintf(stderr, "IH_ISO_LoadImages(): path_images = %s\n",
             path_images);

     /* Load the splash, title, and background images.
      */
     {
          SDL_Surface    *surface;
          char           *file;

          /* Splash image.
           */
          file =
              IH_PathBuild(path_images, "splash." IH_IMAGE_FORMAT_EXT,
                           NULL);
          fprintf(stderr, "IH_ISO_LoadImages(): file = %s\n", file);

          fprintf(stderr, "IH_ISO_LoadImages(): get splash image\n");

          surface = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
          if(surface)
          {
               engine_data->splash = SDL_DisplayFormat(surface);
               SDL_FreeSurface(surface);
          }

          if(!engine_data->splash)
          {
               fprintf(stderr, "Unable to load splash image: %s: %s\n",
                       file, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }

          rnfree(file);

          /* Title image.
           */
          file =
              IH_PathBuild(path_images, "title." IH_IMAGE_FORMAT_EXT,
                           NULL);
          fprintf(stderr, "IH_ISO_LoadImages(): file = %s\n", file);

          fprintf(stderr, "IH_ISO_LoadImages(): get title image\n");

          surface = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
          if(surface)
          {
               engine_data->title = SDL_DisplayFormat(surface);
               SDL_FreeSurface(surface);
          }

          if(!engine_data->title)
          {
               fprintf(stderr, "Unable to load title image: %s: %s\n",
                       file, IMG_GetError());
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }

          rnfree(file);

          /* Background image.
           */
          file =
              IH_PathBuild(path_images, "background." IH_IMAGE_FORMAT_EXT,
                           NULL);
          fprintf(stderr, "IH_ISO_LoadImages(): file = %s\n", file);

          fprintf(stderr, "IH_ISO_LoadImages(): get background image\n");

          surface = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);
          if(surface)
          {
               engine_data->background = SDL_DisplayFormat(surface);
               SDL_FreeSurface(surface);
          }

          if(!engine_data->background)
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
                    else
                    {
                         object->image.width = ((SDL_Surface *)object->image.bits)->w;
                         object->image.height = ((SDL_Surface *)object->image.bits)->h;
                    }
               }
          }
     }
#endif

     fprintf(stderr, "IH_ISO_LoadImages(): free the path var\n");
     rnfree(path_images);

     fprintf(stderr, "IH_ISO_LoadImages(): return %d\n", rc);
     return rc;
}

errr
IH_ISO_LoadMisc(void)
{
     fprintf(stderr, "IH_ISO_LoadMisc()\n");

     fprintf(stderr, "IH_ISO_LoadMisc(): load images\n");
     (void)IH_ISO_LoadImages();

     fprintf(stderr, "IH_ISO_LoadMisc(): load lmx\n");
     (void)IH_ISO_LoadLMX();

     fprintf(stderr, "IH_ISO_LoadMisc(): return 0\n");
     return 0;
}

void
IH_ISO_FreeMisc(void)
{
// FIXME
}

static int
IH_ISO_CreateShader(int w,
                    int h,
                    ihColor * color)
{
     SDL_Rect        irect;
     SDL_Color       sdl_color;
     Uint32          rmask, gmask, bmask, amask;
     int             alpha = IH_ALPHA_VALUE;

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

     sdl_color.r = sdl_color.g = sdl_color.b = 0;

     if(color)
     {
          alpha = color->alpha;

          IH_ISO_ConvertColor(color, &sdl_color);
     }

     if(shader)
     {
          SDL_FreeSurface(shader);
          shader = NULL;
     }

     shader = SDL_CreateRGBSurface(SDL_SWSURFACE | SDL_SRCALPHA,
                                   w, h, 32, rmask, gmask, bmask, amask);

     if(!shader)
          return -1;

     irect.x = 0;
     irect.y = 0;
     irect.w = w;
     irect.h = h;

     /* Fill the square with black.
      */
     SDL_FillRect(shader,
                  &irect,
                  SDL_MapRGBA(shader->format, sdl_color.r, sdl_color.g,
                              sdl_color.b, alpha));

     /* Set the alpha on the shader surface.
      */
     SDL_SetAlpha(shader, SDL_SRCALPHA | SDL_RLEACCEL, alpha);

     return 0;
}

void
IH_ISO_ShadeArea(int x,
                 int y,
                 int w,
                 int h,
                 ihColor * color)
{
     SDL_Rect        srect, drect;

     /* Create a shader surface, if necessary.
      */
     if(!shader)
     {
          IH_ISO_CreateShader(w, h, color);
     }

     /* Paranoia */
     if(!shader)
          return;

     /* Make sure the shader is big enough.
      */
     if(w > shader->w || h > shader->h)
     {
          IH_ISO_CreateShader(w, h, color);
     }

     /* More paranoia */
     if(!shader)
          return;

     /* Setup the rectangles.
      */
     srect.x = 0;
     srect.y = 0;
     srect.w = w;
     srect.h = h;

     drect.x = x;
     drect.y = y;

     /* Draw it.
      */
     SDL_BlitSurface(shader, &srect, ih.display.screen, &drect);
}

void
IH_ISO_FrameArea(int x,
                 int y,
                 int w,
                 int h,
                 ihColor * color)
{
     Uint32          color_val;

     if(color)
     {
          SDL_Color       sdl_color;

          IH_ISO_ConvertColor(color, &sdl_color);

          color_val =
              SDL_MapRGB(ih.display.screen->format, sdl_color.r,
                         sdl_color.g, sdl_color.b);
     }
     else
     {
          color_val = SDL_MapRGB(ih.display.screen->format, 50, 50, 50);
     }

     Draw_Round(ih.display.screen, x, y, w, h, 5, color_val);
}

void
IH_ISO_RenderImage(SDL_Surface * surface,
                   SDL_Rect * srect,
                   SDL_Rect * drect)
{
     fprintf(stderr, "IH_ISO_RenderBackground()\n");
     if(!surface)
          return;

     fprintf(stderr, "IH_ISO_RenderImage(): blit surface\n");
     SDL_BlitSurface(surface, srect, ih.display.screen, drect);
     fprintf(stderr, "IH_ISO_RenderImage: return()\n");
}

void
IH_ISO_FillArea(SDL_Rect * rect,
                ihColor * color)
{
     Uint32          color_val;

     if(!rect)
          return;

     if(color)
     {
          SDL_Color       sdl_color;

          IH_ISO_ConvertColor(color, &sdl_color);

          color_val =
              SDL_MapRGB(ih.display.screen->format, sdl_color.r,
                         sdl_color.g, sdl_color.b);
     }
     else
     {
          color_val = SDL_MapRGB(ih.display.screen->format, 50, 50, 50);
     }

     SDL_FillRect(ih.display.screen, rect, color_val);
}

void
IH_ISO_DrawLine(int x1,
                int y1,
                int x2,
                int y2,
                ihColor * color)
{
     Uint32          color_val;

     if(color)
     {
          SDL_Color       sdl_color;

          IH_ISO_ConvertColor(color, &sdl_color);

          color_val =
              SDL_MapRGB(ih.display.screen->format, sdl_color.r,
                         sdl_color.g, sdl_color.b);
     }
     else
     {
          color_val = SDL_MapRGB(ih.display.screen->format, 50, 50, 50);
     }

     Draw_Line(ih.display.screen, x1, y1, x2, y2, color_val);
}

void
IH_ISO_ClearDrawingBuffer(void)
{
     SDL_Rect        rect;

     rect.x = 0;
     rect.y = 0;
     rect.w = ih.display.width;
     rect.h = ih.display.height;
     SDL_FillRect(ih.display.screen,
                  &rect, SDL_MapRGB(ih.display.screen->format, 0, 0, 0));
}

void
IH_ISO_SwapBuffers(void)
{
     SDL_Flip(ih.display.screen);
}

void
IH_ISO_ConvertColor(ihColor * src_color,
                    SDL_Color * dst_color)
{
     if(!src_color || !dst_color)
          return;

     dst_color->r = src_color->red * IH_ISO_SDL_COLOR_VALUE;
     dst_color->g = src_color->green * IH_ISO_SDL_COLOR_VALUE;
     dst_color->b = src_color->blue * IH_ISO_SDL_COLOR_VALUE;
     /* dst_color.(SDL_Color doesn't contain an alpha component) = src_color.alpha * IH_ISO_SDL_COLOR_VALUE; */
}

void
IH_ISO_RenderBackground(void)
{
     isoEngineData  *engine_data;

     fprintf(stderr, "IH_ISO_RenderBackground()\n");

     engine_data =
         (isoEngineData *) ((displayData *) ih.display.data)->engine_data;

     fprintf(stderr, "IH_ISO_RenderBackground(): render image\n");
     IH_ISO_RenderImage(engine_data->background, NULL, NULL);
     fprintf(stderr, "IH_ISO_RenderBackground(): return\n");
}

void
IH_ISO_RenderTitle(void)
{
     isoEngineData  *engine_data;

     fprintf(stderr, "IH_ISO_RenderTitle()\n");

     engine_data =
         (isoEngineData *) ((displayData *) ih.display.data)->engine_data;

     fprintf(stderr, "IH_ISO_RenderTitle(): render image\n");
     IH_ISO_RenderImage(engine_data->title, NULL, NULL);
     fprintf(stderr, "IH_ISO_RenderTitle(): return\n");
}

void
IH_ISO_RenderSplash(void)
{
     isoEngineData  *engine_data;

     fprintf(stderr, "IH_ISO_RenderSplash()\n");

     engine_data =
         (isoEngineData *) ((displayData *) ih.display.data)->engine_data;

     fprintf(stderr, "IH_ISO_RenderSplash(): render image\n");
     IH_ISO_RenderImage(engine_data->splash, NULL, NULL);
     fprintf(stderr, "IH_ISO_RenderSplash(): return\n");
}

#endif /* BUILD_ISO_ENGINE */
