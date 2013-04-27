
/* $Id: overlay.h,v 1.9 2003/03/23 06:10:27 cipher Exp $ */

#ifndef IH_SDL_RENDER_OVERLAY_H
#define IH_SDL_RENDER_OVERLAY_H

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"

#include "angband.h"

typedef struct _Overlay Overlay;

/* Function prototypes.
 */
void            IH_RenderOverlays(void);
bool            IH_ActivateOverlay(int overlay_num);
void            IH_DeactivateOverlay(int overlay_num);
bool            IH_ToggleOverlay(int overlay_num);

void            IH_RenderOverlay_Character(Overlay * overlay);
void            IH_RenderOverlay_Equipment(Overlay * overlay);
void            IH_RenderOverlay_Inventory(Overlay * overlay);
void            IH_RenderOverlay_Book(Overlay * overlay);
void            IH_RenderOverlay_Messages(Overlay * overlay);
void            IH_RenderOverlay_Store(Overlay * overlay);
void            IH_RenderOverlay_Error(Overlay * overlay);
void            IH_RenderOverlay_Dialog(Overlay * overlay);
void            IH_RenderOverlay_Options(Overlay * overlay);

/* Data definitions.
 */
#define IH_OVERLAY_DIALOG_PROMPT_MAX_LINES 4
#define IH_OVERLAY_ERROR_DISPLAY_MAX_LINES 6

struct _Overlay
{
     int             type;
     int             shown;
     int             mode;
     SDL_Surface    *surface;
     SDL_Rect        position;

     union
     {
          struct
          {
               SDL_Surface    *scale;
          }
          character;
          struct
          {
               byte            filler;
          }
          equipment;
          struct
          {
               byte            filler;
          }
          inventory;
          struct
          {
               byte            filler;
          }
          book;
          struct
          {
               byte            filler;
          }
          messages;
          struct
          {
               int             w, h;
               int             n_lines;
               char           *lines[IH_OVERLAY_ERROR_DISPLAY_MAX_LINES];
               char           *text;
          }
          error;
          struct
          {
               int             w, h;
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
               byte            filler;
          }
          store;
     }
     gfx;
};

enum
{
     IH_OVERLAY_NONE,
     IH_OVERLAY_CHARACTER,
     IH_OVERLAY_INVENTORY,
     IH_OVERLAY_EQUIPMENT,
     IH_OVERLAY_BOOK,
     IH_OVERLAY_MESSAGES,
     IH_OVERLAY_STORE,

     IH_OVERLAY_OPTIONS,
     IH_OVERLAY_DIALOG,
     IH_OVERLAY_ERROR,

     IH_OVERLAY_END
};

#endif /* IH_SDL_RENDER_OVERLAY_H */
