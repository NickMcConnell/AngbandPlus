
/* $Id: selchar.c,v 1.5 2003/04/06 15:22:12 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"
#include "SDL_draw.h"

/* Internal headers */
#include "angband/angband.h"
#include "angband/z-disp.h"
#include "ironhells.h"
#include "strings.h"
#include "ipc.h"
#include "platform/platform.h"
#include "displays/iso/render.h"
#include "displays/iso/text.h"
#include "displays/iso/misc.h"
#include "displays/iso/overlay.h"
#include "displays/iso/scene.h"
#include "displays/iso/scene/selchar.h"

enum
{
     IH_BUTTON_SELCHAR_LOAD,
     IH_BUTTON_SELCHAR_NEW,
     IH_BUTTON_SELCHAR_VIDEO,
     IH_BUTTON_SELCHAR_QUIT,
     IH_BUTTON_SELCHAR_CHARACTERS,
     IH_BUTTON_SELCHAR_CHARACTERS_1,
     IH_BUTTON_SELCHAR_CHARACTERS_2,
     IH_BUTTON_SELCHAR_CHARACTERS_3,
     IH_BUTTON_SELCHAR_CHARACTERS_4,
     IH_BUTTON_SELCHAR_CHARACTERS_5,
     IH_BUTTON_SELCHAR_CHARACTERS_6,
     IH_BUTTON_SELCHAR_CHARACTERS_7,
     IH_BUTTON_SELCHAR_CHARACTERS_8,
     IH_BUTTON_SELCHAR_CHARACTERS_9,
     IH_BUTTON_SELCHAR_CHARACTERS_10,
     IH_BUTTON_SELCHAR_CHARACTERS_11,
     IH_BUTTON_SELCHAR_CHARACTERS_12,
     IH_BUTTON_SELCHAR_CHARACTERS_13,
     IH_BUTTON_SELCHAR_CHARACTERS_14,
     IH_BUTTON_SELCHAR_CHARACTERS_15,
     IH_BUTTON_SELCHAR_CHARACTERS_16,
     IH_BUTTON_SELCHAR_CHARACTERS_17,
     IH_BUTTON_SELCHAR_CHARACTERS_18,
     IH_BUTTON_SELCHAR_CHARACTERS_MAX,

     IH_BUTTON_SELCHAR_END
};

#define IH_SELCHAR_CHARACTER_LIST_LINES 15
#define IH_SELCHAR_CHARACTER_LIST_ITEMS 30

static struct
{
     bool            loading;

     int             selected_character;
     int             display_offset;
#if 0
     int             hilite_load;
     int             hilite_new;
     int             hilite_options;
     int             hilite_quit;
     int             hilite_video;
     SDL_Rect        load_button;
     SDL_Rect        new_button;
     SDL_Rect        options_button;
     SDL_Rect        quit_button;
     SDL_Rect        video_button;
#endif
     char           *char_list[IH_SELCHAR_CHARACTER_LIST_ITEMS];
     int             list_count;

     SceneButton     buttons[IH_BUTTON_SELCHAR_END];
}
scene_info;

static int
selchar_select_character(SceneButton * button)
{
     int             rc = 0;

     return rc;
}

static int
selchar_select_video(SceneButton * button)
{
     int             rc = 0;

     IH_SetScene(SCENE_SELECT_VIDEO);

     return rc;
}

static int
selchar_quit(SceneButton * button)
{
     int             rc = 0;

     ih.done = TRUE;

     return rc;
}

static int
selchar_load_character(SceneButton * button)
{
     int             rc = 0;

     if(!SDL_SemWait(ih.sem.talk))
     {
          ih.new_game = FALSE;
          ih.playing = TRUE;

          // FIXME - setup character name to load

          SDL_SemPost(ih.sem.talk);
     }

     return rc;
}

static int
selchar_new_character(SceneButton * button)
{
     int             rc = 0;

     if(!SDL_SemWait(ih.sem.talk))
     {
          ih.new_game = TRUE;
          ih.playing = TRUE;

          SDL_SemPost(ih.sem.talk);
     }

     return rc;
}

void
IH_InitScene_SelChar(void)
{
     memset(&scene_info, 0, sizeof(scene_info));

     /* Get the list of characters.
      */
     scene_info.list_count =
         IH_GetSaveFiles(scene_info.char_list,
                         IH_SELCHAR_CHARACTER_LIST_ITEMS);

     ih.pointer = IH_POINTER_STANDARD;
}

void
IH_ProcessScene_SelChar(SDL_Event * event)
{
     int             i;

     for(i = 0; i < IH_BUTTON_SELCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          button->hilite =
              button->selected ? IH_HILITE_SELECT : IH_HILITE_NORMAL;

          if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &button->rect))
          {
               /* Mark it as "hovered."
                */
               button->hilite = IH_HILITE_HOVER;

               /* Check for activation.
                */
               if(event->type == SDL_MOUSEBUTTONDOWN)
               {
                    button->hilite = IH_HILITE_SELECT;

                    /* Process its activation callback.
                     */
                    if(button->activate_func)
                         (*button->activate_func) (&scene_info.buttons[i]);
               }

#if 0
               if((event->type == SDL_MOUSEBUTTONDOWN) &&
                  scene_info.selected_character)
               {
                    scene_info.loading = TRUE;
                    scene_info.hilite_load = IH_HILITE_SELECT;

                    IH_SetLoadMessage("Loading character...");
               }
#endif
          }
     }

#if 0
     if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.new_button))
     {
          scene_info.hilite_new = IH_HILITE_HOVER;

          if(event->type == SDL_MOUSEBUTTONDOWN)
          {
               scene_info.hilite_new = IH_HILITE_SELECT;

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
        (ih.mouse_x, ih.mouse_y, &scene_info.video_button))
     {
          scene_info.hilite_video = IH_HILITE_HOVER;

          if(event->type == SDL_MOUSEBUTTONDOWN)
          {
               scene_info.hilite_video = IH_HILITE_SELECT;

               IH_SetScene(SCENE_SELECT_VIDEO);
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
#endif
}

void
IH_RenderScene_SelChar(void)
{
     SDL_Rect        rect, srect, drect;
     SDL_Color       color;
     Uint32          color_val;
     ihFontPos       pos;
     int             i;
     int             list_height;

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

     /* Draw the background.
      */
     SDL_BlitSurface(ih.background, &srect, ih.screen, &drect);

     /* Draw the character box.
      */
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .1;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     IH_AttrToColor(COLOR_WHITE, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_SELCHAR_CHARACTERS, &pos, color, &rect);

     list_height = IH_GetFontHeight(IH_FONT_NORMAL) *
         IH_SELCHAR_CHARACTER_LIST_LINES + 6;

     IH_ShadeArea(rect.x, rect.y + rect.h + 5, 300, list_height);

     color_val = SDL_MapRGB(ih.screen->format, IH_COLOR_BORDER_RED,
                            IH_COLOR_BORDER_GREEN, IH_COLOR_BORDER_BLUE);
     Draw_Round(ih.screen,
                rect.x, rect.y + rect.h + 5, 300, list_height, 5,
                color_val);

     /* Display the buttons.
      */
     for(i = 0; i < IH_BUTTON_SELCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          /* Check if the button needs to be initialized.
           */
          if(!button->init)
          {
               int             init = FALSE;

               button->id = i;

               switch (i)
               {
                    case IH_BUTTON_SELCHAR_LOAD:
                         button->activate_func = selchar_load_character;

                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .3;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_SELCHAR_LOAD;

                         init = TRUE;
                         break;

                    case IH_BUTTON_SELCHAR_NEW:
                         button->activate_func = selchar_new_character;

                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .35;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_SELCHAR_NEW;

                         init = TRUE;
                         break;

                    case IH_BUTTON_SELCHAR_VIDEO:
                         button->activate_func = selchar_select_video;

                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_SELCHAR_VIDEO;

                         init = TRUE;
                         break;

                    case IH_BUTTON_SELCHAR_QUIT:
                         button->activate_func = selchar_quit;

                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .95;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_SELCHAR_QUIT;

                         init = TRUE;
                         break;

                         /* Character list buttons.
                          */
                    default:
                         {
                              cptr            name = NULL;

                              name =
                                  scene_info.char_list[scene_info.
                                                       display_offset +
                                                       (i -
                                                        IH_BUTTON_SELCHAR_CHARACTERS)];

                              /* Set position.
                               */
                              button->pos.x.type = IH_POSITION_TYPE_PIXEL;
                              button->pos.x.pixel = rect.x + 6;
                              button->pos.y.type = IH_POSITION_TYPE_PIXEL;
                              button->pos.y.pixel =
                                  rect.y + rect.h + 11 +
                                  (IH_FONT_NORMAL_SIZE *
                                   (i - IH_BUTTON_SELCHAR_CHARACTERS));

                              /* Set color.
                               */
                              IH_AttrToColor(COLOR_L_WHITE,
                                             &button->color);

                              /* Set the text.
                               */
                              button->font_size = IH_FONT_NORMAL;
                              button->text = name;

                              /* Create an event.
                               */
                              IH_CreateFakeEvent(&button->event,
                                                 SDL_KEYDOWN,
                                                 'A' + (i -
                                                        IH_BUTTON_SELCHAR_CHARACTERS),
                                                 KMOD_SHIFT);

                              /* Set the activation callback.
                               */
                              button->activate_func =
                                  selchar_select_character;

                              init = TRUE;
                         }
                         break;
               }

               /* Mark it initialized.
                */
               button->init = init;
          }

          if(!button->init)
               continue;

          /* Process any modifications to the button's visuals.
           */
          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, button->color, &button->rect);
     }

     IH_RenderOverlays();
}

void
IH_CleanupScene_SelChar(void)
{
     int             i;

     for(i = 0; i < IH_SELCHAR_CHARACTER_LIST_ITEMS; i++)
     {
          if(scene_info.char_list[i])
               rnfree(scene_info.char_list[i]);
          scene_info.char_list[i] = NULL;
     }
}
