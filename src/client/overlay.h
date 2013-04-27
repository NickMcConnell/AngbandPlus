
/* $Id: overlay.h,v 1.2 2003/04/20 05:20:57 cipher Exp $ */

#ifndef IH_DISPLAYS_ISO_OVERLAY_H
#define IH_DISPLAYS_ISO_OVERLAY_H

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
#include "angband/angband.h"

typedef struct _Overlay Overlay;

/* Function prototypes.
 */
int             IH_InitOverlays(void);
void            IH_RenderOverlays(void);

bool            IH_Overlay_Prepare(int overlay_num);
bool            IH_Overlay_Show(int overlay_num);
void            IH_Overlay_Hide(int overlay_num);
int             IH_Overlay_GetState(int overlay_num);
int             IH_Overlay_GetDimensions(int overlay_num,
                                         SDL_Rect * rect);
bool            IH_Overlay_Toggle(int overlay_num);
void            IH_Overlay_Update(int overlay_num);
void            IH_Overlay_Recalc(int overlay_num);
void            IH_Overlay_SetVar(int overlay_num,
                                  int num,
                                  int var);
void            IH_Overlay_SetBuffer(int overlay_num,
                                     int num,
                                     char *buffer);
void            IH_Overlay_SetFlags(int overlay_num,
                                    u32b flags);

void            IH_RenderOverlay_Character(Overlay * overlay);
void            IH_ReleaseOverlay_Character(Overlay * overlay);
void            IH_RenderOverlay_Equipment(Overlay * overlay);
void            IH_ReleaseOverlay_Equipment(Overlay * overlay);
void            IH_RenderOverlay_Inventory(Overlay * overlay);
void            IH_ReleaseOverlay_Inventory(Overlay * overlay);
void            IH_RenderOverlay_Book(Overlay * overlay);
void            IH_ReleaseOverlay_Book(Overlay * overlay);
void            IH_RenderOverlay_Messages(Overlay * overlay);
void            IH_ReleaseOverlay_Messages(Overlay * overlay);
void            IH_RenderOverlay_Memory(Overlay * overlay);
void            IH_ReleaseOverlay_Memory(Overlay * overlay);
void            IH_RenderOverlay_Browser(Overlay * overlay);
void            IH_ReleaseOverlay_Browser(Overlay * overlay);
void            IH_RenderOverlay_Map(Overlay * overlay);
void            IH_ReleaseOverlay_Map(Overlay * overlay);
void            IH_RenderOverlay_Store(Overlay * overlay);
void            IH_ReleaseOverlay_Store(Overlay * overlay);
void            IH_RenderOverlay_Error(Overlay * overlay);
void            IH_ReleaseOverlay_Error(Overlay * overlay);
void            IH_RenderOverlay_Dialog(Overlay * overlay);
void            IH_ReleaseOverlay_Dialog(Overlay * overlay);
void            IH_RenderOverlay_Options(Overlay * overlay);
void            IH_ReleaseOverlay_Options(Overlay * overlay);
void            IH_RenderOverlay_Opponent(Overlay * overlay);
void            IH_ReleaseOverlay_Opponent(Overlay * overlay);
void            IH_RenderOverlay_Help(Overlay * overlay);
void            IH_ReleaseOverlay_Help(Overlay * overlay);

/* Data definitions.
 */
#define IH_OVERLAY_DEFAULT_OFFSET_X 3
#define IH_OVERLAY_DEFAULT_OFFSET_Y 3

#define IH_OVERLAY_CHARACTER_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_CHARACTER_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y

#define IH_OVERLAY_INVENTORY_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_INVENTORY_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y
#define IH_OVERLAY_INVENTORY_INDEX_WIDTH 15
#define IH_OVERLAY_INVENTORY_DESC_WIDTH 350
#define IH_OVERLAY_INVENTORY_WEIGHT_WIDTH 50
#define IH_OVERLAY_INVENTORY_COLUMN_SPACING 4

#define IH_OVERLAY_EQUIPMENT_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_EQUIPMENT_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y
#define IH_OVERLAY_EQUIPMENT_INDEX_WIDTH 15
#define IH_OVERLAY_EQUIPMENT_LABEL_WIDTH 75
#define IH_OVERLAY_EQUIPMENT_DESC_WIDTH 350
#define IH_OVERLAY_EQUIPMENT_WEIGHT_WIDTH 50
#define IH_OVERLAY_EQUIPMENT_COLUMN_SPACING 4

#define IH_OVERLAY_BOOK_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_BOOK_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y
#define IH_OVERLAY_BOOK_INDEX_WIDTH 15
#define IH_OVERLAY_BOOK_NAME_WIDTH 250
#define IH_OVERLAY_BOOK_LEVEL_WIDTH 20
#define IH_OVERLAY_BOOK_MANA_WIDTH 30
#define IH_OVERLAY_BOOK_FAIL_WIDTH 30
#define IH_OVERLAY_BOOK_INFO_WIDTH 65
#define IH_OVERLAY_BOOK_COLUMN_SPACING 4

#define IH_OVERLAY_STORE_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_STORE_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y
#define IH_OVERLAY_STORE_INVENTORY_LINES 12

#define IH_OVERLAY_OPTIONS_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_OPTIONS_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y
#define IH_OVERLAY_OPTIONS_LINES 15

#define IH_OVERLAY_DIALOG_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_DIALOG_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y
#define IH_OVERLAY_DIALOG_PROMPT_MAX_LINES 4

#define IH_OVERLAY_ERROR_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_ERROR_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y
#define IH_OVERLAY_ERROR_DISPLAY_MAX_LINES 6

#define IH_OVERLAY_MAP_OFFSET_X IH_OVERLAY_DEFAULT_OFFSET_X
#define IH_OVERLAY_MAP_OFFSET_Y IH_OVERLAY_DEFAULT_OFFSET_Y

#define IH_OVERLAY_MAX_VARS 4
#define IH_OVERLAY_MAX_BUFFERS 2

struct _Overlay
{
     int             type;
     int             shown;
     int             prep;
     int             mode;
     int             update, recalc;

     void            (*render_func) (Overlay * overlay);
     void            (*release_func) (Overlay * overlay);

     SDL_Surface    *background;
     SDL_Rect        position;

     u32b            flags;
     int             var[IH_OVERLAY_MAX_VARS];
     char           *buffer[IH_OVERLAY_MAX_BUFFERS];
     int             buflen[IH_OVERLAY_MAX_BUFFERS];

     union
     {
          struct
          {
               SDL_Surface    *scale;
          }
          character;
          struct
          {
               bool            show_index[24];
               int             out_index[24];
               byte            out_color[24];
               char            out_desc[24][80];
               int             n_items;
          }
          equipment;
          struct
          {
               bool            show_index[24];
               int             n_items;
          }
          inventory;
          struct
          {
               int             n_items;
          }
          book;
          struct
          {
               byte            filler;
          }
          messages;
          struct
          {
               int             n_lines;
               char           *lines[IH_OVERLAY_ERROR_DISPLAY_MAX_LINES];
               char           *text;
          }
          error;
          struct
          {
               int             n_lines;
               char           *lines[IH_OVERLAY_DIALOG_PROMPT_MAX_LINES];
               char           *prompt;
          }
          dialog;
          struct
          {
               byte            filler;
          }
          options;
          struct
          {
               SDL_Surface    *shop_image;
               SDL_Surface    *gold_icon;
               int             inv_x;
               int             shop_info_y;
               int             gold_y;
          }
          store;
          struct
          {
               byte            filler;
          }
          message_browser;
     }
     gfx;
};

enum
{
     IH_OVERLAY_MODE_END
};

#endif /* IH_DISPLAY_ISO_OVERLAY_H */
