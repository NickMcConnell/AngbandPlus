
/* $Id: text.c,v 1.6 2003/04/18 03:32:56 cipher Exp $ */

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
#include "SDL_ttf.h"

/* Internal headers */
#include "ironhells.h"
#include "path.h"
#include "engines.h"
#include "platform/platform.h"
#include "render/text.h"
#include "engines/iso/text.h"
#include "engines/iso/misc.h"
#include "engines/iso/init.h"

errr
IH_ISO_LoadFonts(void)
{
     displayData    *display_data;
     isoEngineData  *engine_data = NULL;
     char           *path_data;
     char           *path_font;
     errr            rc = 0;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     if(TTF_Init())
     {
          fprintf(stderr,
                  "Can't initialize font library: %s\n", TTF_GetError());
          return IH_ERROR_CANT_LOAD_FONT;
     }

     path_data = IH_GetDataDir("font");
     path_font = IH_PathBuild(path_data, "Angband.ttf", NULL);

     engine_data->normal_font =
         TTF_OpenFont(path_font, IH_FONT_NORMAL_SIZE);
     if(!engine_data->normal_font)
     {
          fprintf(stderr, "Can't load font: Angband.ttf: %s\n",
                  TTF_GetError());
          return IH_ERROR_CANT_LOAD_FONT;
     }
     TTF_SetFontStyle(engine_data->normal_font, TTF_STYLE_NORMAL);

     engine_data->large_font = TTF_OpenFont(path_font, IH_FONT_LARGE_SIZE);
     if(!engine_data->large_font)
     {
          fprintf(stderr, "Can't load font: Angband.ttf: %s\n",
                  TTF_GetError());
          return IH_ERROR_CANT_LOAD_FONT;
     }
     TTF_SetFontStyle(engine_data->large_font, TTF_STYLE_NORMAL);

     rnfree(path_font);
     rnfree(path_data);

     return rc;
}

void
IH_ISO_FreeFonts(void)
{
     displayData    *display_data;
     isoEngineData  *engine_data = NULL;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     if(engine_data->normal_font)
     {
          TTF_CloseFont(engine_data->normal_font);

          engine_data->normal_font = NULL;
     }

     if(engine_data->large_font)
     {
          TTF_CloseFont(engine_data->large_font);

          engine_data->large_font = NULL;
     }
}

void
IH_ISO_RenderText(int size,
                  cptr text,
                  ihFontPos * pos,
                  ihColor * color,
                  u32b flags,
                  SDL_Rect * rect)
{
     displayData    *display_data;
     isoEngineData  *engine_data = NULL;
     SDL_Surface    *text_surface;
     TTF_Font       *font;
     SDL_Rect        drect;
     SDL_Color       sdl_color;
     Uint32          rmask, gmask, bmask, amask;

     if(!text || !pos)
          return;

     if(!*text)
          return;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     switch (size)
     {
          case IH_FONT_LARGE:
               font = engine_data->large_font;
               break;

          case IH_FONT_NORMAL:
          case IH_FONT_SMALL:
          default:
               font = engine_data->normal_font;
               break;
     }

     if(!font)
          return;

     /* Render a drop shadow on the text.
      */
     if(flags & IH_RTF_SHADOW)
     {
          sdl_color.r = sdl_color.g = sdl_color.b = 0;

          if(text_surface = TTF_RenderText_Blended(font, text, sdl_color))
          {
               SDL_Rect        srect;

               srect.w = text_surface->w;
               srect.h = text_surface->h;

               IH_ProcessFontPos(&srect, pos, &drect);

               drect.x = pos->x.pixel + 1;
               drect.y = pos->y.pixel + 1;

               if(rect)
               {
                    rect->x = pos->x.pixel;
                    rect->y = pos->y.pixel;
                    rect->w = text_surface->w;
                    rect->h = text_surface->h;
               }

               SDL_BlitSurface(text_surface, NULL, ih.display.screen,
                               &drect);

               SDL_FreeSurface(text_surface);
          }
     }

     IH_ISO_ConvertColor(color, &sdl_color);

     if(text_surface = TTF_RenderText_Blended(font, text, sdl_color))
     {
          SDL_Rect        srect;

          srect.w = text_surface->w;
          srect.h = text_surface->h;

          IH_ProcessFontPos(&srect, pos, &drect);

          drect.x = pos->x.pixel;
          drect.y = pos->y.pixel;

          if(rect)
          {
               rect->x = pos->x.pixel;
               rect->y = pos->y.pixel;
               rect->w = text_surface->w;
               rect->h = text_surface->h;
          }

          SDL_BlitSurface(text_surface, NULL, ih.display.screen, &drect);

          SDL_FreeSurface(text_surface);
     }
}

int
IH_ISO_GetTextWidth(int size,
                    cptr text)
{
     displayData    *display_data;
     isoEngineData  *engine_data = NULL;
     TTF_Font       *font;
     int             width = 0, height;

     if(!text)
          return 0;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     switch (size)
     {
          case IH_FONT_LARGE:
               font = engine_data->large_font;
               break;

          case IH_FONT_NORMAL:
          case IH_FONT_SMALL:
          default:
               font = engine_data->normal_font;
               break;
     }

     if(!font)
          return 0;

     TTF_SizeText(font, text, &width, &height);

     return width;
}

int
IH_ISO_GetFontHeight(int size)
{
     displayData    *display_data;
     isoEngineData  *engine_data = NULL;
     TTF_Font       *font;
     int             height = 0;

     display_data = (displayData *) ih.display.data;
     engine_data = (isoEngineData *) display_data->engine_data;

     switch (size)
     {
          case IH_FONT_LARGE:
               font = engine_data->large_font;
               break;

          case IH_FONT_NORMAL:
          case IH_FONT_SMALL:
          default:
               font = engine_data->normal_font;
               break;
     }

     if(!font)
          return 0;

     height = TTF_FontHeight(font);

     return height;
}

#endif /* BUILD_ISO_ENGINE */
