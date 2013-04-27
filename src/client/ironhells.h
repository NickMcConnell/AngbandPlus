
/* $Id: ironhells.h,v 1.28 2003/04/21 02:31:44 cipher Exp $ */

#ifndef IH_IRONHELLS_H
#define IH_IRONHELLS_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Config headers */
#include "angband/h-config.h"

/* SDL headers */
#include "SDL.h"
#include "SDL_thread.h"

/* Internal headers */
#include "angband/z-disp.h"
#include "list.h"
#include "scene.h"
#include "render/misc.h"
#include "render/text.h"

/* Forward declarations.
 */
typedef struct disp_data disp_data;

/* Function prototypes.
 */
void            IH_InitSetuid(void);
bool            IH_InitDisplays(void);
errr            IH_InitAngband(void);
void            IH_SetLoadMessage(cptr msg);
void            IH_SetErrorMessage(cptr msg);

/* Extra "term" data.
 */
struct disp_data
{
     disp            t;
};

/* Program data structure.
 */
struct IronHells
{
     /* Display/engine information.
      */
     struct
     {
          SDL_Surface    *screen;
          unsigned        is_fullscreen:1;
          unsigned        is_hw_accel:1;
          int             desired_width, desired_height; // screen resolution
          int             width, height; // screen resolution
          int             depth;
          int             flags;

          /* Mouse co-ordinates.
           */
          int             mouse_x, mouse_y;
          
          /* Map center co-ordinates.
           */
          int map_center_x, map_center_y;

          /* Display engine to use */
          int             engine;

          /* Engine's data storage */
          void           *data;

          /* Setup and teardown */
                          errr(*setup_display_func) (void);
                          errr(*setup_func) (void);
          void            (*cleanup_func) (void);

          /* Miscellaneous rendering functions */
          void            (*clear_buffer_func) (void);
          void            (*swap_buffer_func) (void);
          void            (*render_bg_func) (void);
          void            (*render_title_func) (void);
          void            (*render_splash_func) (void);
          void            (*shade_area_func) (int x,
                                              int y,
                                              int w,
                                              int h,
                                              ihColor * color);
          void            (*frame_area_func) (int x,
                                              int y,
                                              int w,
                                              int h,
                                              ihColor * color);
          void            (*render_image_func) (SDL_Surface * surface,
                                                SDL_Rect * srect,
                                                SDL_Rect * drect);
          void            (*fill_area_func) (SDL_Rect * rect,
                                             ihColor * color);
          void            (*draw_line_func) (int x1,
                                             int y1,
                                             int x2,
                                             int y2,
                                             ihColor * color);

          /* Text rendering functions */
          void            (*render_text_func) (int size,
                                               cptr text,
                                               ihFontPos * pos,
                                               ihColor * color,
                                               u32b flags,
                                               SDL_Rect * rect);
          int             (*get_text_width_func) (int size,
                                                  cptr text);
          int             (*get_font_height_func) (int size);

          /* Icon rendering functions */
          void            (*render_icon_func) (SceneObject * object,
                                               SDL_Rect * srect,
                                               SDL_Rect * drect);

          /* Pointer rendering functions */
          void            (*render_pointer_func) (void);

          /* Game screen functions */
          void            (*draw_map_func) (void);
     }
     display;

     /* Scene variables.
      */
     struct
     {
          int             scene;
          int             stage;
          unsigned        is_changing:1;
          unsigned        is_dirty:1;
          unsigned        is_init:1;
     }
     scene;

     /* Thread communication variables.
      */
     struct
     {
          SDL_Thread     *game_thread;
          struct
          {
               SDL_sem        *msg; // for display messages
               SDL_sem        *map; // for the game map
               SDL_sem        *player; // for the player data structures
               SDL_sem        *scene; // for scene transition
               SDL_sem        *talk; // for inter-thread communication
               SDL_sem        *overlay; // for the overlay list
          }
          sem;

          unsigned        is_valid:1;
          int             type;
          union
          {
               long            value;
               void           *ptr;
               SDL_Event       event;
          }
          data;
     }
     ipc;

     /* Miscellaneous variables.
      */
     int             done;
     int             playing;
     int             new_game;

     int             icon_size;

     int             pointer;
#if 0                           // these should be internal to the display engine
     SDL_Surface    *splash;
     SDL_Surface    *background;
#endif
     char            load_message[1024];
     int             err_shown;
     char            err_message[1024];

     int             messages_shown;
};

enum
{
     IH_ERROR_NONE,
     IH_ERROR_INVALID_DATA,
     IH_ERROR_SDL_VIDEO_ERROR,
     IH_ERROR_SDL_AUDIO_ERROR,
     IH_ERROR_CANT_LOAD_POINTER,
     IH_ERROR_CANT_LOAD_ICON,
     IH_ERROR_CANT_LOAD_TILE,
     IH_ERROR_CANT_LOAD_IMAGE,
     IH_ERROR_CANT_LOAD_FONT,
     IH_ERROR_NO_MANIFEST,

     IH_ERROR_MAX
};

#define IH_IMAGE_FORMAT_EXT "png"

#define IH_ICON_SIZE_SMALL_VALUE  32
#define IH_ICON_SIZE_MEDIUM_VALUE 40
#define IH_ICON_SIZE_LARGE_VALUE  64

#define IH_ALPHA_VALUE 96

#define IH_COLOR_BORDER_RED   80
#define IH_COLOR_BORDER_GREEN 80
#define IH_COLOR_BORDER_BLUE  80

/* Supported display engines.
 */
enum
{
     IH_DISPLAY_ENGINE_ISO,
     IH_DISPLAY_ENGINE_OPENGL,

     IH_DISPLAY_ENGINE_END
};

/*
 * Maximum number of displays
 */
#define MAX_DISP_DATA 8

#ifndef IH_MAIN
extern struct IronHells ih;
#endif

#endif /* IH_IRONHELLS_H */
