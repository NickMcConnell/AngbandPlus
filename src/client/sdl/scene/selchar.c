
/* $Id: selchar.c,v 1.7 2003/03/18 22:35:15 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"
#include "SDL_draw.h"

#include "angband.h"
#include "ironhells.h"
#include "file.h"
#include "sdl/scene/selchar.h"
#include "sdl/render.h"
#include "sdl/render/text.h"
#include "sdl/scene.h"
#include "sdl/render/misc.h"
#include "sdl/strings.h"

static struct
{
     ihList         *characters;

     bool            loading;

     int             selected_character;
     int             hilite_load;
     int             hilite_new;
     int             hilite_options;
     int             hilite_quit;
     SDL_Rect        load_button;
     SDL_Rect        new_button;
     SDL_Rect        options_button;
     SDL_Rect        quit_button;
     SDL_Rect        char_list;
}
scene_info;

void
IH_InitScene_SelChar(void)
{
     memset(&scene_info, 0, sizeof(scene_info));

     /* Get the list of characters.
      */
     scene_info.characters = IH_GetSaveFiles();

     ih.pointer = IH_POINTER_STANDARD;
}

void
IH_CleanupScene_SelChar(void)
{
}

void
IH_ProcessScene_SelChar(SDL_Event * event)
{
     scene_info.hilite_load = IH_HILITE_NORMAL;
     scene_info.hilite_new = IH_HILITE_NORMAL;
     scene_info.hilite_quit = IH_HILITE_NORMAL;

     if(IH_IsPointerInRect
        (ih.mouse_x, ih.mouse_y, &scene_info.load_button))
     {
          scene_info.hilite_load =
              scene_info.
              selected_character ? IH_HILITE_HOVER : IH_HILITE_NORMAL;

          if((event->type == SDL_MOUSEBUTTONDOWN) &&
             scene_info.selected_character)
          {
               scene_info.loading = TRUE;
               scene_info.hilite_load = IH_HILITE_SELECT;

               IH_SetLoadMessage("Loading character...");
          }

          return;
     }

     if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.new_button))
     {
          scene_info.hilite_new = IH_HILITE_HOVER;

          if(event->type == SDL_MOUSEBUTTONDOWN)
          {
               scene_info.hilite_new = IH_HILITE_SELECT;
#if 0
               scene_info.commence = TRUE;
               scene_info.next_scene = IH_SCENE_NEW_CHARACTER;
#endif

               if(!SDL_SemWait(ih.sem.talk))
               {
                    ih.playing = TRUE;
                    ih.new_game = TRUE;

                    SDL_SemPost(ih.sem.talk);
               }
          }

          return;
     }

     if(IH_IsPointerInRect
        (ih.mouse_x, ih.mouse_y, &scene_info.quit_button))
     {
          scene_info.hilite_quit = IH_HILITE_HOVER;

          if(event->type == SDL_MOUSEBUTTONDOWN)
          {
               scene_info.hilite_quit = IH_HILITE_SELECT;
               ih.done = TRUE;
          }

          return;
     }
}

void
IH_RenderScene_SelChar(void)
{
     SDL_Rect        rect, srect, drect;
     SDL_Color       color;
     Uint32          color_val;
     ihFontPos       pos;
     char           *path_save;
     ihNode         *node;
     int             i;

//     path_save = IH_GetDataDir("save");

     /* Set the source and destination rendering rectangles.
      */
     srect.x = 0;
     srect.y = 0;
     srect.w = ih.background->w;
     srect.h = ih.background->h;

     drect.x = 0;
     drect.y = 0;
     drect.w = ih.display_width;
     drect.h = ih.display_height;

     SDL_BlitSurface(ih.background, &srect, ih.screen, &drect);

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .1;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_CHARACTERS, &pos, color, &rect);

     /* Show the list of characters.
      */
     IH_ShadeArea(rect.x, rect.y + IH_FONT_LARGE_SIZE + 5, 300, 350);
     color_val = SDL_MapRGB(ih.screen->format, IH_COLOR_BORDER_RED,
                            IH_COLOR_BORDER_GREEN, IH_COLOR_BORDER_BLUE);
     Draw_Round(ih.screen,
                rect.x, rect.y + rect.h + 5, 300, 350, 5, color_val);
     for(node = IH_ListFirst(scene_info.characters), i = 0;
         node; node = IH_ListNext(scene_info.characters, node), i++)
     {
          SDL_Color       color;
          ihFontPos       pos;
          cptr            name;

          name = (cptr) node->data;
          if(!name)
               continue;
          if(i * IH_FONT_NORMAL_SIZE > 345)
               break;

          pos.x.type = IH_POSITION_TYPE_PIXEL;
          pos.x.pixel = rect.x + 6;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel = rect.y + rect.h + 11 + (IH_FONT_NORMAL_SIZE * i);
          color.r = color.g = color.b = 150;
          IH_RenderText(IH_FONT_NORMAL, name, &pos, color, NULL);
     }

     /* Draw text buttons.
      */
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .3;
     IH_GetButtonColor(scene_info.hilite_load, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_LOAD, &pos, color, &rect);
     memcpy(&scene_info.load_button, &rect, sizeof(SDL_Rect));

     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += IH_FONT_LARGE_SIZE + 10;
     IH_GetButtonColor(scene_info.hilite_new, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_NEW, &pos, color, &rect);
     memcpy(&scene_info.new_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .85;
     IH_GetButtonColor(scene_info.hilite_options, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_OPTIONS, &pos, color, &rect);
     memcpy(&scene_info.options_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_quit, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_QUIT, &pos, color, &rect);
     memcpy(&scene_info.quit_button, &rect, sizeof(SDL_Rect));
}
