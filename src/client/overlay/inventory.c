
/* $Id: inventory.c,v 1.3 2003/04/18 21:45:14 cipher Exp $ */

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
#include "ironhells.h"
#include "overlay.h"

void
IH_RenderOverlay_Inventory(Overlay * overlay)
{
     ihColor         color;
     ihFontPos       pos;
     register int    i, n;
     int             fhn;
     object_type    *o_ptr;
     byte            attr;
     char            o_name[80];

     /* Do the shading and border */
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w +
                  (IH_OVERLAY_INVENTORY_OFFSET_X * 2),
                  overlay->position.h +
                  (IH_OVERLAY_INVENTORY_OFFSET_Y * 2), NULL);
     IH_FrameArea(overlay->position.x, overlay->position.y,
                  overlay->position.w +
                  (IH_OVERLAY_INVENTORY_OFFSET_X * 2),
                  overlay->position.h +
                  (IH_OVERLAY_INVENTORY_OFFSET_Y * 2), NULL);

     fhn = IH_GetFontHeight(IH_FONT_NORMAL);

     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.y.type = IH_POSITION_TYPE_PIXEL;

     /* Display the pack */
     for(i = 0; i < overlay->gfx.inventory.n_items; i++)
     {
          pos.y.pixel =
              overlay->position.y + IH_OVERLAY_INVENTORY_OFFSET_Y +
              (i * fhn);
          pos.x.pixel =
              overlay->position.x + IH_OVERLAY_INVENTORY_OFFSET_X;

          /* Examine the item */
          o_ptr = &inventory[i];

          /* Is this item "acceptable"? */
          if(overlay->gfx.inventory.show_index[i])
          {
               char            buf[5];

               /* Prepare an "index" */
               sprintf(buf, "%c)", index_to_label(i));

               IH_AttrToColor(COLOR_WHITE, &color);
               IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, NULL);
          }

          /* Increment the X position */
          if(show_labels)
               pos.x.pixel +=
                   IH_OVERLAY_INVENTORY_INDEX_WIDTH +
                   IH_OVERLAY_INVENTORY_COLUMN_SPACING;

          /* Obtain an item description */
          object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

          /* Obtain the length of the description */
          n = strlen(o_name);

          /* Get inventory color */
          attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

          /* Display the entry itself */
          IH_AttrToColor(attr, &color);
          IH_RenderText(IH_FONT_NORMAL, o_name, &pos, &color, 0, NULL);

          /* Display the weight if needed */
          if(show_weights && o_ptr->weight)
          {
               char            buf[10];
               int             wgt = o_ptr->weight * o_ptr->number;

               sprintf(buf, "%3d.%1d lb", wgt / 10, wgt % 10);

               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_INVENTORY_OFFSET_X +
                   overlay->position.w - IH_GetTextWidth(IH_FONT_NORMAL,
                                                         buf);

               IH_AttrToColor(COLOR_WHITE, &color);
               IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, NULL);
          }
     }
}

void
IH_ReleaseOverlay_Inventory(Overlay * overlay)
{
     if(!overlay)
          return;
}
