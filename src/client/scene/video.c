
/* $Id: video.c,v 1.5 2003/04/20 05:20:58 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"

/* Internal headers */
#include "ironhells.h"
#include "ipc.h"
#include "strings.h"
#include "scene/video.h"

enum
{
     IH_BUTTON_VIDEO_MODE,
     IH_BUTTON_VIDEO_HWACCEL,
     IH_BUTTON_VIDEO_FULLSCREEN,
     IH_BUTTON_VIDEO_DEPTH_LIST,
     IH_BUTTON_VIDEO_DEPTH_LIST_1,
     IH_BUTTON_VIDEO_DEPTH_LIST_2,
     IH_BUTTON_VIDEO_DEPTH_LIST_3,
     IH_BUTTON_VIDEO_DEPTH_LIST_4,
     IH_BUTTON_VIDEO_MODES_LIST,
     IH_BUTTON_VIDEO_MODES_LIST_1,
     IH_BUTTON_VIDEO_MODES_LIST_2,
     IH_BUTTON_VIDEO_MODES_LIST_3,
     IH_BUTTON_VIDEO_MODES_LIST_4,
     IH_BUTTON_VIDEO_MODES_LIST_5,
     IH_BUTTON_VIDEO_MODES_LIST_6,
     IH_BUTTON_VIDEO_MODES_LIST_7,
     IH_BUTTON_VIDEO_MODES_LIST_8,
     IH_BUTTON_VIDEO_MODES_LIST_9,
     IH_BUTTON_VIDEO_ACCEPT,
     IH_BUTTON_VIDEO_CANCEL,

     IH_BUTTON_VIDEO_END
};

#define IH_VIDEO_RESOLUTION_LIST_LINES 10
#define IH_VIDEO_DEPTH_LIST_LINES 5

static struct
{
     struct
     {
          SDL_Rect        dimensions;
          int             depth;
          int             engine;

          unsigned        is_hw_accel:1;
          unsigned        is_fullscreen:1;
     }
     selected;

     int             selected_depth, selected_res;

     SceneButton     buttons[IH_BUTTON_VIDEO_END];
}
scene_info;

static char     hw_accel_text[50];
static char     fullscreen_text[50];
static char     res_text[IH_VIDEO_RESOLUTION_LIST_LINES][20];
static SDL_Rect mode_rect[IH_VIDEO_RESOLUTION_LIST_LINES];
static char     depth_text[IH_VIDEO_DEPTH_LIST_LINES][20];
static char     mode_text[35];

static int      depths[IH_VIDEO_DEPTH_LIST_LINES] = {
     8,
     15,
     16,
     24,
     32
};

static int
video_toggle_hwaccel(SceneButton * button)
{
     int             rc = 0;

     scene_info.selected.is_hw_accel = !scene_info.selected.is_hw_accel;

     return rc;
}

static int
video_toggle_fullscreen(SceneButton * button)
{
     int             rc = 0;

     scene_info.selected.is_fullscreen =
         !scene_info.selected.is_fullscreen;

     return rc;
}

static int
video_select_depth(SceneButton * button)
{
     int             rc = 0;
     int             i;

     i = button->id - IH_BUTTON_VIDEO_DEPTH_LIST;

     scene_info.selected.depth = depths[i];

     scene_info.selected_depth = i;

     return rc;
}

static int
video_select_resolution(SceneButton * button)
{
     int             rc = 0;
     int             i;

     i = button->id - IH_BUTTON_VIDEO_MODES_LIST;
     memcpy(&scene_info.selected.dimensions, &mode_rect[i],
            sizeof(SDL_Rect));

     scene_info.selected_res = i;

     return rc;
}

static int
video_accept(SceneButton * button)
{
     int             rc = 0;

     /* FIXME: Save the settings to disk.
      */

     /* Handle video mode changes, etc.
      */
     ih.display.desired_width = scene_info.selected.dimensions.w;
     ih.display.desired_height = scene_info.selected.dimensions.h;
     ih.display.depth = scene_info.selected.depth;

     ih.display.is_fullscreen = scene_info.selected.is_fullscreen;
     ih.display.is_hw_accel = scene_info.selected.is_hw_accel;

     // FIXME: modify screen

     /* Change the scene.
      */
     IH_SetScene(SCENE_SELECT_CHARACTER);

     return rc;
}

static int
video_cancel(SceneButton * button)
{
     int             rc = 0;

     /* Change the scene.
      */
     IH_SetScene(SCENE_SELECT_CHARACTER);

     return rc;
}

void
IH_InitScene_Video(void)
{
     memset(&scene_info, 0, sizeof(scene_info));

     scene_info.selected.dimensions.w = ih.display.desired_width;
     scene_info.selected.dimensions.h = ih.display.desired_height;
     scene_info.selected.depth = ih.display.depth;
     scene_info.selected.is_fullscreen = ih.display.is_fullscreen;
     scene_info.selected.is_hw_accel = ih.display.is_hw_accel;
}

void
IH_ProcessScene_Video(SDL_Event * event)
{
     int             i;

     for(i = 0; i < IH_BUTTON_VIDEO_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          button->hilite =
              button->selected ? IH_HILITE_SELECT : IH_HILITE_NORMAL;

          if(IH_IsPointerInRect
             (ih.display.mouse_x, ih.display.mouse_y, &button->rect))
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
          }
     }
}

void
IH_RenderScene_Video(void)
{
     SDL_Rect        res_rect, depth_rect;
     ihColor         color;
     ihFontPos       pos;
     SDL_Rect      **modes;
     Uint32          flags = 0;
     int             i, j, res_list_height, depth_list_height;

     /* Set up flags based on requested modes.
      */
     if(scene_info.selected.is_hw_accel)
          flags |= SDL_HWSURFACE;
     if(scene_info.selected.is_fullscreen)
          flags |= SDL_FULLSCREEN;
     switch (scene_info.selected.engine)
     {
#ifdef BUILD_ISO_ENGINE
          case IH_DISPLAY_ENGINE_ISO:
               break;
#endif
#ifdef BUILD_3D_ENGINE
          case IH_DISPLAY_ENGINE_3D:
               flags |= SDL_OPENGL;
               break;
#endif
     }

     /* Get available modes.
      */
     modes = SDL_ListModes(NULL, flags);

     for(i = 0; i < IH_VIDEO_RESOLUTION_LIST_LINES; i++)
     {
          res_text[i][0] = 0;
          memset(&mode_rect[i], 0, sizeof(SDL_Rect));
     }

     if(modes == (SDL_Rect **) 0)
     {
          /* No modes available.
           */
     }
     else if(modes == (SDL_Rect **) - 1)
     {
          i = 0;

          /* All modes available, so populate list with standard modes.
           */
          sprintf(res_text[i], "%dx%d", 800, 600);
          mode_rect[i].w = 800;
          mode_rect[i].h = 600;
          i++;
          sprintf(res_text[i], "%dx%d", 1024, 768);
          mode_rect[i].w = 1024;
          mode_rect[i].h = 768;
          i++;
          sprintf(res_text[i], "%dx%d", 1280, 1024);
          mode_rect[i].w = 1280;
          mode_rect[i].h = 1024;
          i++;
          sprintf(res_text[i], "%dx%d", 1600, 1200);
          mode_rect[i].w = 1600;
          mode_rect[i].h = 1200;
          i++;
     }
     else
     {
          for(i = 0, j = 0; modes[i]; i++)
          {
               if(i >= IH_VIDEO_RESOLUTION_LIST_LINES)
                    break;

               if(modes[i]->w < 800 || modes[i]->h < 600)
                    continue;

               sprintf(res_text[j], "%dx%d", modes[i]->w, modes[i]->h);
               memcpy(&mode_rect[j], modes[i], sizeof(SDL_Rect));
          }
     }

     for(i = 0; i < IH_VIDEO_DEPTH_LIST_LINES; i++)
     {
          sprintf(depth_text[i], "%d", depths[i]);
     }

     /* Draw the background.
      */
     IH_RenderBackground();

     /* Print the scene title.
      */
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     IH_AttrToColor(COLOR_WHITE, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_VIDEO_SETDISPLAY, &pos, &color, 0, NULL);

     /* Create a box for the screen dimensions.
      */
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .1;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .1;
     IH_AttrToColor(COLOR_WHITE, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_VIDEO_RESOLUTION, &pos, &color, 0, &res_rect);

     res_list_height = IH_GetFontHeight(IH_FONT_NORMAL) *
         IH_VIDEO_RESOLUTION_LIST_LINES + 6;

     IH_ShadeArea(res_rect.x, res_rect.y + res_rect.h + 5, 300,
                  res_list_height, NULL);
     IH_FrameArea(res_rect.x, res_rect.y + res_rect.h + 5, 300,
                  res_list_height, NULL);

     /* Create a box for the screen depths.
      */
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .1;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .5;
     IH_AttrToColor(COLOR_WHITE, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_VIDEO_DEPTH, &pos, &color, 0, &depth_rect);

     depth_list_height = IH_GetFontHeight(IH_FONT_NORMAL) *
         IH_VIDEO_DEPTH_LIST_LINES + 6;

     IH_ShadeArea(depth_rect.x, depth_rect.y + IH_FONT_LARGE_SIZE + 5, 300,
                  depth_list_height, NULL);
     IH_FrameArea(depth_rect.x, depth_rect.y + IH_FONT_LARGE_SIZE + 5, 300,
                  depth_list_height, NULL);

     /* Display the buttons and list items.
      */
     for(i = 0; i < IH_BUTTON_VIDEO_END; i++)
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
                    case IH_BUTTON_VIDEO_HWACCEL:
                         /* Set position.
                          */
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .1;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .87;

                         /* Set color.
                          */
                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         /* Set the text.
                          */
                         sprintf(hw_accel_text, IH_TEXT_VIDEO_HWACCEL_FMT,
                                 scene_info.selected.
                                 is_hw_accel ? IH_TEXT_VIDEO_HWACCEL_ON :
                                 IH_TEXT_VIDEO_HWACCEL_OFF);

                         button->font_size = IH_FONT_LARGE;
                         button->text = hw_accel_text;

                         /* Set the activation callback.
                          */
                         button->activate_func = video_toggle_hwaccel;

                         init = TRUE;

                         break;

                    case IH_BUTTON_VIDEO_FULLSCREEN:
                         /* Set position.
                          */
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .1;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .92;

                         /* Set color.
                          */
                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         /* Set the text.
                          */
                         sprintf(fullscreen_text,
                                 IH_TEXT_VIDEO_FULLSCREEN_FMT,
                                 scene_info.selected.
                                 is_fullscreen ?
                                 IH_TEXT_VIDEO_FULLSCREEN_ON :
                                 IH_TEXT_VIDEO_FULLSCREEN_OFF);

                         button->font_size = IH_FONT_LARGE;
                         button->text = fullscreen_text;

                         /* Set the activation callback.
                          */
                         button->activate_func = video_toggle_fullscreen;

                         init = TRUE;

                         break;

                    case IH_BUTTON_VIDEO_MODE:
                         /* Set position.
                          */
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .1;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .82;

                         /* Set the text.
                          */

                         button->font_size = IH_FONT_LARGE;
                         button->text = mode_text;

                         /* Set the activation callback.
                          */
                         button->activate_func = NULL;

                         init = TRUE;

                         break;

                    case IH_BUTTON_VIDEO_ACCEPT:
                         /* Set position.
                          */
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         /* Set color.
                          */
                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         /* Set the text.
                          */
                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_VIDEO_ACCEPT;

                         /* Set the activation callback.
                          */
                         button->activate_func = video_accept;

                         init = TRUE;

                         break;

                    case IH_BUTTON_VIDEO_CANCEL:
                         /* Set position.
                          */
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .95;

                         /* Set color.
                          */
                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         /* Set the text.
                          */
                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_VIDEO_CANCEL;

                         /* Set the activation callback.
                          */
                         button->activate_func = video_cancel;

                         init = TRUE;

                         break;

                    case IH_BUTTON_VIDEO_DEPTH_LIST:
                    case IH_BUTTON_VIDEO_DEPTH_LIST_1:
                    case IH_BUTTON_VIDEO_DEPTH_LIST_2:
                    case IH_BUTTON_VIDEO_DEPTH_LIST_3:
                    case IH_BUTTON_VIDEO_DEPTH_LIST_4:
                         /* Set position.
                          */
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .11;
                         button->pos.y.type = IH_POSITION_TYPE_PIXEL;
                         button->pos.y.pixel =
                             depth_rect.y + depth_rect.h + 5 +
                             ((i -
                               IH_BUTTON_VIDEO_DEPTH_LIST) *
                              IH_GetFontHeight(IH_FONT_NORMAL));

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         /* Set the text.
                          */
                         button->font_size = IH_FONT_NORMAL;
                         button->text =
                             depth_text[i - IH_BUTTON_VIDEO_DEPTH_LIST];

                         /* Set the activation callback.
                          */
                         button->activate_func = video_select_depth;

                         init = TRUE;

                         break;

                    case IH_BUTTON_VIDEO_MODES_LIST:
                    case IH_BUTTON_VIDEO_MODES_LIST_1:
                    case IH_BUTTON_VIDEO_MODES_LIST_2:
                    case IH_BUTTON_VIDEO_MODES_LIST_3:
                    case IH_BUTTON_VIDEO_MODES_LIST_4:
                    case IH_BUTTON_VIDEO_MODES_LIST_5:
                    case IH_BUTTON_VIDEO_MODES_LIST_6:
                    case IH_BUTTON_VIDEO_MODES_LIST_7:
                    case IH_BUTTON_VIDEO_MODES_LIST_8:
                    case IH_BUTTON_VIDEO_MODES_LIST_9:
                         /* Set position.
                          */
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .11;
                         button->pos.y.type = IH_POSITION_TYPE_PIXEL;
                         button->pos.y.pixel =
                             res_rect.y + res_rect.h + 5 +
                             ((i -
                               IH_BUTTON_VIDEO_MODES_LIST) *
                              IH_GetFontHeight(IH_FONT_NORMAL));

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         /* Set the text.
                          */
                         button->font_size = IH_FONT_NORMAL;
                         button->text =
                             res_text[i - IH_BUTTON_VIDEO_MODES_LIST];

                         /* Set the activation callback.
                          */
                         button->activate_func = video_select_resolution;

                         init = TRUE;

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

          IH_GetButtonColor(IH_HILITE_NORMAL,
                            &scene_info.buttons[IH_BUTTON_VIDEO_MODE].
                            color);
          sprintf(mode_text, IH_TEXT_VIDEO_MODE_FMT,
                  scene_info.selected.dimensions.w,
                  scene_info.selected.dimensions.h,
                  scene_info.selected.depth);

          sprintf(hw_accel_text, IH_TEXT_VIDEO_HWACCEL_FMT,
                  scene_info.selected.
                  is_hw_accel ? IH_TEXT_VIDEO_HWACCEL_ON :
                  IH_TEXT_VIDEO_HWACCEL_OFF);
          sprintf(fullscreen_text, IH_TEXT_VIDEO_FULLSCREEN_FMT,
                  scene_info.selected.
                  is_fullscreen ? IH_TEXT_VIDEO_FULLSCREEN_ON :
                  IH_TEXT_VIDEO_FULLSCREEN_OFF);

          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, &button->color, 0, &button->rect);
     }
}

void
IH_CleanupScene_Video(void)
{
}
