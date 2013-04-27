
/* $Id: text.c,v 1.11 2003/03/24 06:04:53 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#ifdef HAVE_CONFIG_H
#include "autoconf.h"
#endif

#include "SDL.h"
#include "SDL_image.h"
#ifdef USE_SDLTTF
#include "SDL_ttf.h"
#else
#include "bfont.h"
#endif

#include "ironhells.h"
#include "path.h"
#include "file.h"
#include "sdl/render/text.h"
#include "sdl/render/misc.h"

void
IH_RenderText(int size,
              cptr text,
              ihFontPos * pos,
              SDL_Color color,
              SDL_Rect * rect)
{
     SDL_Surface    *text_surface;
     SDL_Rect        drect;
     Uint32          rmask, gmask, bmask, amask;

#ifdef USE_SDLTTF
     TTF_Font       *font;
#else
     BFont_Info     *font;
     int             w, h;
#endif

     if(!text)
          return;
     if(!pos)
          return;

     if(!*text)
          return;

     switch (size)
     {
          case IH_FONT_LARGE:
               font = ih.large_font;
               break;

          case IH_FONT_NORMAL:
          case IH_FONT_SMALL:
          default:
               font = ih.normal_font;
               break;
     }

     if(!font)
          return;

#ifdef USE_SDLTTF
     if(text_surface = TTF_RenderText_Blended(font, text, color))
     {
          IH_ProcessFontPos(text_surface, pos, &drect);

          drect.x = pos->x.pixel;
          drect.y = pos->y.pixel;

          if(rect)
          {
               rect->x = pos->x.pixel;
               rect->y = pos->y.pixel;
               rect->w = text_surface->w;
               rect->h = text_surface->h;
          }
#ifdef DEBUG
          fprintf(stderr, "Blitting text at position %d,%d\n", drect.x,
                  drect.y);
#endif
          SDL_BlitSurface(text_surface, NULL, ih.screen, &drect);

          SDL_FreeSurface(text_surface);
     }
#else
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
     w = TextWidthFont(font, text);
     h = FontHeight(font);
     if(text_surface = SDL_CreateRGBSurface(SDL_SWSURFACE | SDL_SRCALPHA,
                                            w, h,
                                            ih.screen->format->
                                            BitsPerPixel, rmask, gmask,
                                            bmask, amask))
     {
          BFont_Info     *new_font;

#if 0
          if(new_font = SetFontColor(font, color.r, color.g, color.b))
          {
#endif
               PutStringFont(text_surface, /* new_ */ font, 0, 0, text);

               IH_ProcessFontPos(text_surface, pos, &drect);

               drect.x = pos->x.pixel;
               drect.y = pos->y.pixel;

               if(rect)
               {
                    rect->x = pos->x.pixel;
                    rect->y = pos->y.pixel;
                    rect->w = text_surface->w;
                    rect->h = text_surface->h;
               }
#ifdef DEBUG
               fprintf(stderr, "Blitting text at position %d,%d\n",
                       drect.x, drect.y);
#endif
               SDL_BlitSurface(text_surface, NULL, ih.screen, &drect);

#if 0
               FreeFont(new_font);
          }
#endif

          SDL_FreeSurface(text_surface);
     }
#endif
}

errr
IH_InitFonts(void)
{
     char           *path_data;
     char           *path_font;
     errr            rc = 0;

     fprintf(stderr, "IH_InitFonts()\n");

#ifdef USE_SDLTTF
#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): TTF_Init\n");
#endif
     if(TTF_Init())
     {
          fprintf(stderr,
                  "Can't initialize font library: %s\n", TTF_GetError());
          return IH_ERROR_CANT_LOAD_FONT;
     }
#endif

#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): Get font data dir\n");
#endif
     path_data = IH_GetDataDir("font");
#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): path_data = %s\n", path_data);
#endif

#ifdef USE_SDLTTF
#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): get font file path\n");
#endif
     path_font = IH_PathBuild(path_data, "Angband.ttf", NULL);
#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): path_font = %s\n", path_font);
#endif

#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): TTF_OpenFont(%s, %d)\n", path_font,
             IH_FONT_NORMAL_SIZE);
#endif
     ih.normal_font = TTF_OpenFont(path_font, IH_FONT_NORMAL_SIZE);
     if(!ih.normal_font)
     {
          fprintf(stderr, "Can't load font: Angband.ttf: %s\n",
                  TTF_GetError());
          return IH_ERROR_CANT_LOAD_FONT;
     }
     TTF_SetFontStyle(ih.normal_font, TTF_STYLE_NORMAL);
#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): normal font height = %d\n",
             TTF_FontHeight(ih.normal_font));
#endif

#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): TTF_OpenFont(%s, %d)\n", path_font,
             IH_FONT_LARGE_SIZE);
#endif
     ih.large_font = TTF_OpenFont(path_font, IH_FONT_LARGE_SIZE);
     if(!ih.large_font)
     {
          fprintf(stderr, "Can't load font: Angband.ttf: %s\n",
                  TTF_GetError());
          return IH_ERROR_CANT_LOAD_FONT;
     }
     TTF_SetFontStyle(ih.large_font, TTF_STYLE_NORMAL);
#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): large font height = %d\n",
             TTF_FontHeight(ih.large_font));
#endif
#else
     path_font = IH_PathBuild(path_data, "Angband-normal.png", NULL);
     ih.normal_font = LoadFont(path_font);
     if(!ih.normal_font)
     {
          fprintf(stderr, "Can't load font: Angband-normal.png\n");
          return IH_ERROR_CANT_LOAD_FONT;
     }
     rnfree(path_font);

     path_font = IH_PathBuild(path_data, "Angband-large.png", NULL);
     ih.large_font = LoadFont(path_font);
     if(!ih.normal_font)
     {
          fprintf(stderr, "Can't load font: Angband-large.png\n");
          return IH_ERROR_CANT_LOAD_FONT;
     }
#endif

#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): free path_font\n");
#endif
     rnfree(path_font);
#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): free path_data\n");
#endif
     rnfree(path_data);

#ifdef DEBUG
     fprintf(stderr, "IH_InitFonts(): return (rc = %d)\n", rc);
#endif
     return rc;
}

void
IH_FreeFonts(void)
{
     if(ih.normal_font)
     {
#ifdef USE_SDLTTF
          TTF_CloseFont(ih.normal_font);
#else
          FreeFont(ih.normal_font);
#endif
          ih.normal_font = NULL;
     }

     if(ih.large_font)
     {
#ifdef USE_SDLTTF
          TTF_CloseFont(ih.large_font);
#else
          FreeFont(ih.large_font);
#endif
          ih.large_font = NULL;
     }
}

int
IH_GetTextWidth(int size,
                cptr text)
{
#ifdef USE_SDLTTF
     TTF_Font       *font;
#else
     BFont_Info     *font;
#endif
     int             width = 0, height;

     if(!text)
          return 0;

     switch (size)
     {
          case IH_FONT_LARGE:
               font = ih.large_font;
               break;

          case IH_FONT_NORMAL:
          case IH_FONT_SMALL:
          default:
               font = ih.normal_font;
               break;
     }

     if(!font)
          return 0;

#ifdef USE_SDLTTF
     TTF_SizeText(font, text, &width, &height);
#else
     width = TextWidthFont(font, text);
     height = FontHeight(font);
#endif

     return width;
}

void
IH_ProcessFontPos(SDL_Surface * image,
                  ihFontPos * font_pos,
                  SDL_Rect * rect)
{
     int             width, height;

     if(!image)
          return;
     if(!font_pos)
          return;
     if(!rect)
          return;

     width = image->w;
     height = image->h;

     switch (font_pos->x.type)
     {
          case IH_POSITION_TYPE_PIXEL:
               rect->x = font_pos->x.pixel;
               font_pos->x.perc = font_pos->x.pixel / ih.display_width;
               break;

          case IH_POSITION_TYPE_PERCENT:
               rect->x = ih.display_width * font_pos->x.perc;
               font_pos->x.pixel = rect->x;
               break;

          case IH_POSITION_TYPE_ABS_CENTER:
               rect->x = (ih.display_width - width) / 2;
               font_pos->x.pixel = rect->x;
               font_pos->x.perc = font_pos->x.pixel / ih.display_width;
               break;

          case IH_POSITION_TYPE_PIXEL_CENTER:
               rect->x = font_pos->x.pixel - (width / 2);
               font_pos->x.pixel = rect->x;
               font_pos->x.perc = font_pos->x.pixel / ih.display_width;
               break;

          case IH_POSITION_TYPE_ABS_LEFT:
          case IH_POSITION_TYPE_ABS_TOP:
               rect->x = 0;
               font_pos->x.pixel = 0;
               font_pos->x.perc = 0.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_LEFT:
          case IH_POSITION_TYPE_PIXEL_TOP:
               rect->x = font_pos->x.pixel;
               font_pos->x.perc = font_pos->x.pixel / ih.display_width;
               break;

          case IH_POSITION_TYPE_ABS_RIGHT:
          case IH_POSITION_TYPE_ABS_BOTTOM:
               rect->x = ih.display_width - width;
               font_pos->x.pixel = rect->x;
               font_pos->x.perc = 1.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_RIGHT:
          case IH_POSITION_TYPE_PIXEL_BOTTOM:
               rect->x = font_pos->x.pixel - width;
               font_pos->x.pixel = rect->x;
               font_pos->x.perc = font_pos->x.pixel / ih.display_width;
               break;
     }

     switch (font_pos->y.type)
     {
          case IH_POSITION_TYPE_PIXEL:
               rect->y = font_pos->y.pixel;
               font_pos->y.perc = font_pos->y.pixel / ih.display_height;
               break;

          case IH_POSITION_TYPE_PERCENT:
               rect->y = ih.display_height * font_pos->y.perc;
               font_pos->y.pixel = rect->y;
               break;

          case IH_POSITION_TYPE_ABS_CENTER:
               rect->y = (ih.display_height - height) / 2;
               font_pos->y.pixel = rect->y;
               font_pos->y.perc = font_pos->y.pixel / ih.display_height;
               break;

          case IH_POSITION_TYPE_PIXEL_CENTER:
               rect->y = font_pos->y.pixel - (height / 2);
               font_pos->y.pixel = rect->y;
               font_pos->y.perc = font_pos->y.pixel / ih.display_height;
               break;

          case IH_POSITION_TYPE_ABS_LEFT:
          case IH_POSITION_TYPE_ABS_TOP:
               rect->y = 0;
               font_pos->y.pixel = 0;
               font_pos->y.perc = 0.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_LEFT:
          case IH_POSITION_TYPE_PIXEL_TOP:
               rect->y = font_pos->y.pixel;
               font_pos->y.perc = font_pos->y.pixel / ih.display_height;
               break;

          case IH_POSITION_TYPE_ABS_RIGHT:
          case IH_POSITION_TYPE_ABS_BOTTOM:
               rect->y = ih.display_height - height;
               font_pos->y.pixel = rect->y;
               font_pos->y.perc = 1.0f;
               break;

          case IH_POSITION_TYPE_PIXEL_RIGHT:
          case IH_POSITION_TYPE_PIXEL_BOTTOM:
               rect->y = font_pos->y.pixel - height;
               font_pos->y.pixel = rect->y;
               font_pos->y.perc = font_pos->y.pixel / ih.display_height;
               break;
     }
}

int
IH_GetFontHeight(int size)
{
#ifdef USE_SDLTTF
     TTF_Font       *font;
#else
     BFont_Info     *font;
#endif
     int             height = 0;

     switch (size)
     {
          case IH_FONT_LARGE:
               font = ih.large_font;
               break;

          case IH_FONT_NORMAL:
          case IH_FONT_SMALL:
          default:
               font = ih.normal_font;
               break;
     }

     if(!font)
          return 0;

#ifdef USE_SDLTTF
     height = TTF_FontHeight(font);
#else
#endif

     return height;
}
