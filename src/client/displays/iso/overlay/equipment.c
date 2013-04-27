
/* $Id: equipment.c,v 1.2 2003/04/03 22:58:35 cipher Exp $ */

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
#include "displays/iso/overlay.h"
#include "displays/iso/text.h"
#include "displays/iso/icon.h"
#include "displays/iso/misc.h"

void
IH_RenderOverlay_Equipment(Overlay * overlay)
{
     SDL_Color       color;
     ihFontPos       pos;
     Uint32          color_val;
     register int    j, n;
     int             fhn;
     object_type    *o_ptr;

     /* Check if the inventory overlay is shown, and set the position
      * of the equipment overlay accordingly.
      */
     overlay->position.x =
         ih.display_width - overlay->position.w -
         IH_GetIconSize(IH_ICON_SIZE_CURRENT) - 2;
     overlay->position.y = IH_GetIconSize(IH_ICON_SIZE_CURRENT);

     if(IH_Overlay_GetState(DISPLAY_INVENTORY))
     {
          SDL_Rect        inv;

          if(IH_Overlay_GetDimensions(DISPLAY_INVENTORY, &inv))
          {
               overlay->position.x =
                   ih.display_width - overlay->position.w -
                   IH_GetIconSize(IH_ICON_SIZE_CURRENT) - 2;
               overlay->position.y = inv.y + inv.h + 4;
          }
     }

     /* Do the shading and border */
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w +
                  (IH_OVERLAY_INVENTORY_OFFSET_X * 2),
                  overlay->position.h +
                  (IH_OVERLAY_INVENTORY_OFFSET_Y * 2));

     color_val = SDL_MapRGB(ih.screen->format, 50, 50, 50);
     Draw_Round(ih.screen,
                overlay->position.x,
                overlay->position.y,
                overlay->position.w + (IH_OVERLAY_INVENTORY_OFFSET_X * 2),
                overlay->position.h + (IH_OVERLAY_INVENTORY_OFFSET_Y * 2),
                5, color_val);

     fhn = IH_GetFontHeight(IH_FONT_NORMAL);

     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.y.type = IH_POSITION_TYPE_PIXEL;

     /* Output each entry */
     for(j = 0; j < overlay->gfx.equipment.n_items; j++)
     {
          char            buf[80];
          int             i;

          pos.y.pixel =
              overlay->position.y + IH_OVERLAY_EQUIPMENT_OFFSET_Y +
              (j * fhn);

          /* Get the index */
          i = overlay->gfx.equipment.out_index[j];

          /* Get the item */
          o_ptr = &inventory[i];

          /* Prepare and display an index --(-- */
          sprintf(buf, "%c)", index_to_label(i));

          pos.x.pixel =
              overlay->position.x + IH_OVERLAY_INVENTORY_OFFSET_X;

          IH_AttrToColor(COLOR_WHITE, &color);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, NULL);

          /* Display the entry itself */
          pos.x.pixel +=
              IH_OVERLAY_EQUIPMENT_INDEX_WIDTH +
              IH_OVERLAY_EQUIPMENT_COLUMN_SPACING;

          /* Use labels */
          if(show_labels)
          {
               /* Mention the use */
               IH_AttrToColor(COLOR_WHITE, &color);
               IH_RenderText(IH_FONT_NORMAL, mention_use(i), &pos, color,
                             NULL);

               pos.x.pixel +=
                   IH_OVERLAY_EQUIPMENT_LABEL_WIDTH +
                   IH_OVERLAY_EQUIPMENT_COLUMN_SPACING;
          }

          /* Display the item */
          IH_AttrToColor(overlay->gfx.equipment.out_color[j], &color);
          IH_RenderText(IH_FONT_NORMAL, overlay->gfx.equipment.out_desc[j],
                        &pos, color, NULL);

          /* Display the weight if needed */
          if(show_weights)
          {
               int             wgt = o_ptr->weight * o_ptr->number;

               sprintf(buf, "%3d.%1d lb", wgt / 10, wgt % 10);

               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_EQUIPMENT_OFFSET_X +
                   overlay->position.w - IH_GetTextWidth(IH_FONT_NORMAL,
                                                         buf);

               IH_AttrToColor(COLOR_WHITE, &color);
               IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, NULL);
          }
     }
}

void
IH_ReleaseOverlay_Equipment(Overlay * overlay)
{
     if(!overlay)
          return;
}
