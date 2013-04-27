
/* $Id: map.c,v 1.2 2003/04/17 06:43:51 cipher Exp $ */

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
#include "overlay.h"
#include "render/icon.h"

void
IH_RenderOverlay_Map(Overlay * overlay)
{
     int             size;

     /* Paranoia.
      */
     if(!overlay)
          return;

     size = IH_GetIconSize(IH_ICON_SIZE_CURRENT);

     overlay->position.w = ih.display.width - (size * 2);
     overlay->position.h = ih.display.height - (size * 2);
     overlay->position.x = size;
     overlay->position.y = size;

     /* Draw the overlay border and backing */
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_MAP_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_MAP_OFFSET_Y * 2),
                  NULL);
     IH_FrameArea(overlay->position.x, overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_MAP_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_MAP_OFFSET_Y * 2),
                  NULL);

}

void
IH_ReleaseOverlay_Map(Overlay * overlay)
{
}
