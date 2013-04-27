
/* $Id: misc.c,v 1.6 2003/04/07 20:53:54 cipher Exp $ */

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
#include "path.h"
#include "platform/platform.h"
#include "displays/iso/scene.h"
#include "displays/iso/render.h"
#include "displays/iso/misc.h"
#include "displays/iso/icon.h"

static SceneObject *misc_objects[IH_MAX_MISC_OBJECTS];
static int      misc_object_count = 0;

struct color_table
{
     byte            attr;
     float           r;
     float           g;
     float           b;
};

static struct color_table color_table[] = {
     /* color,         r,      g,      b) */
     {COLOR_DARK, 0.0f, 0.0f, 0.0f}, /* 0, 0, 0 */
     {COLOR_WHITE, 1.0f, 1.0f, 1.0f}, /* 4, 4, 4 */
     {COLOR_SLATE, 0.5f, 0.5f, 0.5f}, /* 2, 2, 2 */
     {COLOR_ORANGE, 1.0f, 0.5f, 0.0f}, /* 4, 2, 0 */
     {COLOR_RED, 0.75f, 0.0f, 0.0f}, /* 3, 0, 0 */
     {COLOR_GREEN, 0.0f, 0.5f, 0.25f}, /* 0, 2, 1 */
     {COLOR_BLUE, 0.0f, 0.0f, 1.0f}, /* 0, 0, 4 */
     {COLOR_UMBER, 0.5f, 0.25f, 0.0f}, /* 2, 1, 0 */
     {COLOR_L_DARK, 0.25f, 0.25f, 0.25f}, /* 1, 1, 1 */
     {COLOR_L_WHITE, 0.75f, 0.75f, 0.75f}, /* 3, 3, 3 */
     {COLOR_VIOLET, 1.0f, 0.0f, 1.0f}, /* 4, 0, 4 */
     {COLOR_YELLOW, 1.0f, 1.0f, 0.0f}, /* 4, 4, 0 */
     {COLOR_L_RED, 1.0f, 0.0f, 0.0f}, /* 4, 0, 0 */
     {COLOR_L_GREEN, 0.0f, 1.0f, 0.0f}, /* 0, 4, 0 */
     {COLOR_L_BLUE, 0.0f, 1.0f, 1.0f}, /* 0, 4, 4 */
     {COLOR_L_UMBER, 0.75f, 0.5f, 0.25f}, /* 3, 2, 1 */
     {COLOR_END, 0.0f, 0.0f, 0.0f}
};

static SDL_Surface *shader = NULL;

errr
IH_InitMisc(void)
{

     return (0);
}

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
          misc_objects[misc_object_count++] = object;

          object->image.surface =
              IMG_Load_RW(SDL_RWFromFile(path_lmx, "rb"), 1);
          if(!object->image.surface)
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_lmx, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
     }

     /* Load the life bar.
      */
     path_life_bar =
         IH_PathBuild(path_data, "life-bar." IH_IMAGE_FORMAT_EXT, NULL);

     object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_MISC,
                                  IH_SCENE_OBJECT_LIFE);
     if(object)
     {
          misc_objects[misc_object_count++] = object;

          object->text.is_dynamic = TRUE;
          object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;
          object->text.x_align = IH_SCENE_OBJECT_TEXT_ALIGN_CENTER;
          object->text.y_align = IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM;
          object->text.font = IH_FONT_NORMAL;

          object->image.surface =
              IMG_Load_RW(SDL_RWFromFile(path_life_bar, "rb"), 1);
          if(!object->image.surface)
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_life_bar, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
     }

     /* Load the mana bar.
      */
     path_mana_bar =
         IH_PathBuild(path_data, "mana-bar." IH_IMAGE_FORMAT_EXT, NULL);

     object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_MISC,
                                  IH_SCENE_OBJECT_MANA);
     if(object)
     {
          misc_objects[misc_object_count++] = object;

          object->text.is_dynamic = TRUE;
          object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;
          object->text.x_align = IH_SCENE_OBJECT_TEXT_ALIGN_CENTER;
          object->text.y_align = IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM;
          object->text.font = IH_FONT_NORMAL;

          object->image.surface =
              IMG_Load_RW(SDL_RWFromFile(path_mana_bar, "rb"), 1);
          if(!object->image.surface)
          {
#ifdef DEBUG
               fprintf(stderr, "Unable to load icon image: %s: %s\n",
                       path_mana_bar, IMG_GetError());
#endif
               rc = IH_ERROR_CANT_LOAD_IMAGE;
          }
     }

     /* Load the experience bar.
      */
     path_xp_bar =
         IH_PathBuild(path_data, "xp-bar." IH_IMAGE_FORMAT_EXT, NULL);

     object = IH_SceneObjectAlloc(IH_SCENE_OBJECT_TYPE_MISC,
                                  IH_SCENE_OBJECT_XP);
     if(object)
     {
          misc_objects[misc_object_count++] = object;

          object->text.is_dynamic = TRUE;
          object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;
          object->text.x_align = IH_SCENE_OBJECT_TEXT_ALIGN_CENTER;
          object->text.y_align = IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM;
          object->text.font = IH_FONT_NORMAL;

          object->image.surface =
              IMG_Load_RW(SDL_RWFromFile(path_xp_bar, "rb"), 1);
          if(!object->image.surface)
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
     int             i, lmx_x = 0;

     for(i = 0; i < misc_object_count; i++)
     {
          SceneObject    *object;

          object = misc_objects[i];
          if(!object)
               continue;

          switch (object->object)
          {
               case IH_SCENE_OBJECT_LMXDECO:
                    lmx_x = object->x =
                        (ih.display_width - object->image.surface->w) / 2;
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
     int             i;
     bool            shade = FALSE;

     for(i = 0; i < misc_object_count; i++)
     {
          SceneObject    *object;
          SDL_Rect        srect, drect;
          float           perc = 1.0f;

          object = misc_objects[i];
          if(!object)
               continue;

          IH_UpdateSceneObject(object);

          if(object->type != IH_SCENE_OBJECT_TYPE_MISC)
               continue;

          if(object->image.surface)
          {
               /* Set up the source and destination rectangles.
                */
               srect.x = 0;
               srect.y = 0;
               srect.w = object->image.surface->w;
               srect.h = object->image.surface->h;

               drect.x = object->x;
               drect.y = object->y;

               /* Handle life, mana, and xp bar levels.
                */
               switch (object->object)
               {
                    case IH_SCENE_OBJECT_LIFE:
                         if(p_ptr->mhp)
                              perc =
                                  ((float) p_ptr->chp /
                                   (float) p_ptr->mhp);
                         else
                              perc = 0.0f;

                         shade = TRUE;

                         break;

                    case IH_SCENE_OBJECT_MANA:
                         if(p_ptr->msp)
                              perc =
                                  ((float) p_ptr->csp /
                                   (float) p_ptr->msp);
                         else
                              perc = 0.0f;

                         shade = TRUE;

                         break;

                    case IH_SCENE_OBJECT_XP:
#if 0
                         if(p_ptr->max_exp)
                              perc =
                                  ((float) p_ptr->exp /
                                   (float) p_ptr->max_exp);
                         else
                              perc = 0.0f;
#endif

                         shade = TRUE;

                         break;
               }
               srect.w *= perc;

               /* Blit the alpha surface first.
                */
               if(shade)
                    IH_ShadeArea(object->x, object->y,
                                 object->image.surface->w,
                                 object->image.surface->h);

               /* Blit the image.
                */
               SDL_BlitSurface(object->image.surface, &srect, ih.screen,
                               &drect);
          }

          IH_RenderSceneObjectText(object);
     }
}

void
IH_RenderSceneObjectText(SceneObject * object)
{
     ihFontPos       pos;
     int             width, height, img_width, img_height;

     /* All kinds of paranoia.
      */
     if(!object)
          return;
     if(!object->text.string[0])
          return;
     if(object->text.mode == IH_SCENE_OBJECT_TEXT_MODE_NONE)
          return;

     /* Do we show hover text?
      */
     if(object->text.mode == IH_SCENE_OBJECT_TEXT_MODE_HOVER &&
        !object->text.shown)
          return;

     /* Get text width and height.
      */
     width = IH_GetTextWidth(object->text.font, object->text.string);
     height = IH_GetFontHeight(object->text.font);

     img_width = object->image.surface ? object->image.surface->w : 0;
     img_height = object->image.surface ? object->image.surface->h : 0;

     /* Calculate position.
      */
     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     switch (object->text.x_align)
     {
          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_LEFT:
               pos.x.pixel = object->x - width - 2;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_LEFT:
               pos.x.pixel = object->x + 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_CENTER:
               pos.x.pixel = (object->x + (img_width / 2)) - (width / 2);
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_RIGHT:
               pos.x.pixel = (object->x + img_width) - width - 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT:
               pos.x.pixel = object->x + img_width + 2;
               break;
     }

     switch (object->text.y_align)
     {
          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP:
               pos.y.pixel = object->y - height - 2;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_TOP:
               pos.y.pixel = object->y + 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_CENTER:
               pos.y.pixel = (object->y + (img_height / 2)) - (height / 2);
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_BOTTOM:
               pos.y.pixel = (object->y + img_height) - height - 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM:
               pos.y.pixel = object->y + img_height + 2;
               break;
     }

     IH_RenderText(object->text.font,
                   object->text.string, &pos, object->text.color, NULL);
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

static int
IH_CreateShader(int w,
                int h,
                int alpha)
{
     SDL_Rect        irect;
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
                  SDL_MapRGBA(shader->format, 0, 0, 0, IH_ALPHA_VALUE));

     /* Set the alpha on the shader surface.
      */
     SDL_SetAlpha(shader, SDL_SRCALPHA | SDL_RLEACCEL, IH_ALPHA_VALUE);

     return 0;
}

void
IH_ShadeArea(int x,
             int y,
             int w,
             int h)
{
     SDL_Rect        srect, drect;

     /* Create a shader surface, if necessary.
      */
     if(!shader)
     {
          IH_CreateShader(w, h, IH_ALPHA_VALUE);
     }

     /* Paranoia */
     if(!shader)
          return;

     /* Make sure the shader is big enough.
      */
     if(w > shader->w || h > shader->h)
     {
          IH_CreateShader(w, h, IH_ALPHA_VALUE);
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
     SDL_BlitSurface(shader, &srect, ih.screen, &drect);
}

void
IH_AttrToColor(byte attr,
               SDL_Color * color)
{
     int             i;

     if(!color)
          return;

     for(i = 0; color_table[i].attr != COLOR_END; i++)
     {
          if(color_table[i].attr == attr)
          {
               color->r = (Uint8) (color_table[i].r * 255);
               color->g = (Uint8) (color_table[i].g * 255);
               color->b = (Uint8) (color_table[i].b * 255);

               return;
          }
     }
}

void
IH_GetButtonColor(int hilite,
                  SDL_Color * color)
{
     if(!color)
          return;

     switch (hilite)
     {
          case IH_HILITE_HOVER:
               color->r = 255;
               color->g = 255;
               color->b = 255;
               break;

          case IH_HILITE_SELECT:
               color->r = 255;
               color->g = 255;
               color->b = 0;
               break;

          case IH_HILITE_NORMAL:
          default:
               color->r = 150;
               color->g = 150;
               color->b = 150;
               break;
     }
}

bool
IH_IsPointerInRect(int x,
                   int y,
                   SDL_Rect * rect)
{
     if(!rect)
          return FALSE;

#ifdef DEBUG
     fprintf(stderr,
             "Checking rectangle position of rect (%d, %d, %d, %d) against (%d,%d)\n",
             rect->x, rect->y, rect->w, rect->h, x, y);
#endif

     if((x >= rect->x) &&
        (x < (rect->x + rect->w)) &&
        (y >= rect->y) && (y < (rect->y + rect->h)))
          return TRUE;

#ifdef DEBUG
     fprintf(stderr, "Position is not in rectangle.\n");
#endif
     return FALSE;
}
