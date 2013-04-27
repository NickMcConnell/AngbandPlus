#ifndef IH_SDL_RENDER_OVERLAY_H
#define IH_SDL_RENDER_OVERLAY_H

/* File: sdl/render/overlay.h */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

typedef struct _Overlay Overlay;

/* Function prototypes.
 */
void IH_RenderOverlays(void);
bool IH_ActivateOverlay(int overlay_num);
void IH_DeactivateOverlay(int overlay_num);
bool IH_InitOverlay(int overlay_num);

/* Data definitions.
 */
struct _Overlay
{
     int          type;
     bool         shown;
     int          mode;
     SDL_Surface *surface;
     SDL_Rect     position;
};

enum
{
     IH_OVERLAY_NONE,
     IH_OVERLAY_CHARACTER,
     IH_OVERLAY_INVENTORY,
     IH_OVERLAY_EQUIPMENT,
     IH_OVERLAY_BOOK,
     IH_OVERLAY_MESSAGES,

     IH_OVERLAY_END
};

#endif /* IH_SDL_RENDER_OVERLAY_H */