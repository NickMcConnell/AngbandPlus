
/* $Id: map.c,v 1.1 2003/04/01 22:26:18 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"
#include "SDL_image.h"
#include "SDL_draw.h"

/* Internal headers */
#include "ironhells.h"
#include "displays/iso/overlay.h"
#include "displays/iso/text.h"
#include "displays/iso/misc.h"
#include "displays/iso/icon.h"

void
IH_RenderOverlay_Map(Overlay * overlay)
{
     Uint32          color_val;
     int             size;

     /* Paranoia.
      */
     if(!overlay)
          return;

     size = IH_GetIconSize(IH_ICON_SIZE_CURRENT);

     overlay->position.w = ih.display_width - (size * 2);
     overlay->position.h = ih.display_height - (size * 2);
     overlay->position.x = size;
     overlay->position.y = size;

     /* Draw the overlay border and backing */
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_MAP_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_MAP_OFFSET_Y * 2));

     color_val = SDL_MapRGB(ih.screen->format, 50, 50, 50);
     Draw_Round(ih.screen,
                overlay->position.x,
                overlay->position.y,
                overlay->position.w + (IH_OVERLAY_MAP_OFFSET_X * 2),
                overlay->position.h + (IH_OVERLAY_MAP_OFFSET_Y * 2),
                5, color_val);

}

void
IH_ReleaseOverlay_Map(Overlay * overlay)
{
}
