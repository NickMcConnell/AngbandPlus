/* File: game.c */

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
#include "sdl/scene/selchar.h"
#include "sdl/render.h"
#include "sdl/render/text.h"
#include "file.h"
#include "sdl/scene.h"
#include "sdl/render/misc.h"
#include "sdl/strings.h"

static struct
{
#if 0
     bool commence;
     int next_scene;
#endif
     
     bool loading;

     int selected_character;
     int hilite_load;
     int hilite_new;
     int hilite_options;
     int hilite_quit;
     SDL_Rect load_button;
     SDL_Rect new_button;
     SDL_Rect options_button;
     SDL_Rect quit_button;
     SDL_Rect char_list;
} scene_info;

void IH_InitScene_SelChar(void)
{
     memset(&scene_info, 0, sizeof(scene_info));

     ih.pointer = IH_POINTER_STANDARD;
}

void IH_ProcessScene_SelChar(SDL_Event *event)
{
#if 0
     if(scene_info.commence)
     {
#ifdef DEBUG
          fprintf(stderr, "Setting scene to %d\n", scene_info.next_scene);
#endif
          IH_SetScene(scene_info.next_scene);
          return;
     }
     else
          if(scene_info.loading)
     {
#ifdef DEBUG
          fprintf(stderr, "Commencing player load.\n");
#endif
          // FIXME

          scene_info.loading = FALSE;
          scene_info.commence = TRUE;
          scene_info.next_scene = IH_SCENE_PLAY; // FIXME
          return;
     }
#endif
     
     scene_info.hilite_load = IH_HILITE_NORMAL;
     scene_info.hilite_new = IH_HILITE_NORMAL;
     scene_info.hilite_quit = IH_HILITE_NORMAL;
     
     if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.load_button))
     {
          scene_info.hilite_load = scene_info.selected_character ? IH_HILITE_HOVER : IH_HILITE_NORMAL;

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

     if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.quit_button))
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

void IH_RenderScene_SelChar(void)
{
     SDL_Rect rect, srect, drect;
     SDL_Color color;
     Uint32 color_val;
     ihFontPos pos;
     cptr path_save;

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
     //     font_pos.x.width = IH_GetTextWidth(IH_TEXT_TITLE_PROGRAM);
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_CHARACTERS,
                   &pos,
                   color,
                   &rect);

     /* Get the list of characters.
      */


     /* Show the list of characters.
      */
     color_val = SDL_MapRGBA(ih.screen->format, 200, 200, 200, 100);
     Draw_Round(ih.screen,
                rect.x, rect.y + IH_FONT_LARGE_SIZE + 5,
                300, 350,
                5, color_val);


     
     /* Draw text buttons.
      */
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .3;
     IH_GetButtonColor(scene_info.hilite_load, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_LOAD,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.load_button, &rect, sizeof(SDL_Rect));

     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += IH_FONT_LARGE_SIZE + 10;
     IH_GetButtonColor(scene_info.hilite_new, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_NEW,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.new_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .85;
     IH_GetButtonColor(scene_info.hilite_options, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_OPTIONS,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.options_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_quit, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_CHARLOAD_QUIT,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.quit_button, &rect, sizeof(SDL_Rect));
}
