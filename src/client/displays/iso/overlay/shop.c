
/* $Id: shop.c,v 1.1 2003/04/01 22:26:19 cipher Exp $ */

/* NOTE: This file is called shop.c to prevent a naming conflict with store.c
 * in the src/angband/.
 */

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
#include "ironhells.h"
#include "platform/platform.h"
#include "displays/iso/overlay.h"
#include "displays/iso/text.h"
#include "displays/iso/misc.h"

extern int      store_num;
extern int      store_top;
extern store_type *st_ptr;
extern owner_type *ot_ptr;

void
IH_RenderOverlay_Store(Overlay * overlay)
{
     SDL_Color       color;
     SDL_Rect        rect;
     ihFontPos       pos;
     char           *path_data, buf[20];
     Uint32          color_val;
     cptr            store_name =
         (f_name + f_info[FEAT_SHOP_HEAD + store_num].name);
     cptr            owner_name = &(b_name[ot_ptr->owner_name]);
     cptr            race_name = p_name + p_info[ot_ptr->owner_race].name;
     int             gold_x;

     if(!overlay)
          return;

     path_data = IH_GetDataDir("gfx");

     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_STORE_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_STORE_OFFSET_Y * 2));

     color_val = SDL_MapRGB(ih.screen->format, 50, 50, 50);
     Draw_Round(ih.screen,
                overlay->position.x,
                overlay->position.y,
                overlay->position.w + (IH_OVERLAY_STORE_OFFSET_X * 2),
                overlay->position.h + (IH_OVERLAY_STORE_OFFSET_Y * 2),
                5, color_val);

     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.x.pixel = overlay->position.x + IH_OVERLAY_STORE_OFFSET_X;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel = overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y;

     /* Draw the store information (type and cost max).
      */
     color.r = color.g = color.b = 255;

     IH_RenderText(IH_FONT_LARGE, store_name, &pos, color, &rect);

     if(store_num != STORE_HOME)
     {
          pos.y.pixel += rect.h;
          sprintf(buf, "(%ld)", (long) ot_ptr->max_cost);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);
     }

     /* Draw the shopkeep's picture.
      */
     rect.h += 10;
     if(overlay->gfx.store.shop_image)
     {
          SDL_Rect        irect;

          irect.x = overlay->position.x + IH_OVERLAY_STORE_OFFSET_X;
          irect.y = (pos.y.pixel += rect.h + 5);

          SDL_BlitSurface(overlay->gfx.store.shop_image, NULL,
                          ih.screen, &irect);

          rect.h = overlay->gfx.store.shop_image->h;
     }

     if(store_num != STORE_HOME)
     {
          /* Draw the shopkeep's information.
           */
          color.r = color.g = 255;
          color.b = 0;

          pos.y.pixel += rect.h;
          IH_RenderText(IH_FONT_NORMAL, owner_name, &pos, color, &rect);

          pos.y.pixel += rect.h;
          IH_RenderText(IH_FONT_NORMAL, race_name, &pos, color, &rect);
     }

     /* Draw the player's gold.
      */
     if(overlay->gfx.store.gold_icon)
     {
          rect.x = overlay->position.x + IH_OVERLAY_STORE_OFFSET_X;
          rect.y =
              overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
              overlay->gfx.store.gold_y;

          SDL_BlitSurface(overlay->gfx.store.gold_icon, NULL, ih.screen,
                          &rect);

          gold_x = overlay->gfx.store.gold_icon->w;
     }
     else
     {
          gold_x = 0;
     }
     pos.x.pixel =
         overlay->position.x + IH_OVERLAY_STORE_OFFSET_X + gold_x;
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
         overlay->gfx.store.gold_y;

     sprintf(buf, "%9ld", (long) p_ptr->au);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, NULL);

     /* Draw the inventory.
      */
     {
          store_item     *item;
          char            buf[30];
          int             k;

          /* Headers */
          pos.y.pixel = overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y;

          pos.x.pixel =
              overlay->position.x + IH_OVERLAY_STORE_OFFSET_X +
              overlay->gfx.store.inv_x + 20;
          IH_RenderText(IH_FONT_NORMAL, "Description", &pos, color, NULL);

          if(show_weights)
          {
               if(store_num == STORE_HOME)
                    pos.x.pixel =
                        (overlay->position.x + overlay->position.w) -
                        IH_OVERLAY_STORE_OFFSET_X -
                        IH_GetTextWidth(IH_FONT_NORMAL, "Weight");
               else
                    pos.x.pixel =
                        (overlay->position.x + overlay->position.w) -
                        IH_OVERLAY_STORE_OFFSET_X -
                        IH_GetTextWidth(IH_FONT_NORMAL, "Weight") - 50;
               IH_RenderText(IH_FONT_NORMAL, "Weight", &pos, color, NULL);
          }

          if(store_num != STORE_HOME)
          {
               pos.x.pixel =
                   (overlay->position.x + overlay->position.w) -
                   IH_OVERLAY_STORE_OFFSET_X -
                   IH_GetTextWidth(IH_FONT_NORMAL, "Price");
               IH_RenderText(IH_FONT_NORMAL, "Price", &pos, color, NULL);
          }

          /* Display the next 12 items */
          for(k = 0; k < IH_OVERLAY_STORE_INVENTORY_LINES; k++)
          {
               /* Stop when we run out of items */
               if(store_top + k >= st_ptr->stock_num)
                    break;

               /* Get the item */
               item = get_store_item(store_top + k);

               pos.y.pixel =
                   overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
                   ((k + 1) * (IH_GetFontHeight(IH_FONT_NORMAL) + 2));

               color.r = color.g = color.b = 255;

               /* Item selection letter */
               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_STORE_OFFSET_X +
                   overlay->gfx.store.inv_x;
               sprintf(buf, "%c)", item->label);
               IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, NULL);

               /* Item description */
               IH_AttrToColor(item->attr, &color);

               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_STORE_OFFSET_X +
                   overlay->gfx.store.inv_x + 20;
               IH_RenderText(IH_FONT_NORMAL, item->o_name, &pos, color,
                             NULL);

               /* Item weight */
               if(show_weights)
               {
                    sprintf(buf, "%3d.%d lb", item->wgt / 10,
                            item->wgt % 10);
                    if(store_num == STORE_HOME)
                         pos.x.pixel =
                             (overlay->position.x + overlay->position.w) -
                             IH_OVERLAY_STORE_OFFSET_X -
                             IH_GetTextWidth(IH_FONT_NORMAL, buf);
                    else
                         pos.x.pixel =
                             (overlay->position.x + overlay->position.w) -
                             IH_OVERLAY_STORE_OFFSET_X -
                             IH_GetTextWidth(IH_FONT_NORMAL, buf) - 50;

                    color.r = color.g = color.b = 255;

                    IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, NULL);
               }

               if(store_num != STORE_HOME)
               {
                    /* Item price */
                    sprintf(buf, "%9ld ", item->price);
                    pos.x.pixel =
                        (overlay->position.x + overlay->position.w) -
                        IH_OVERLAY_STORE_OFFSET_X -
                        IH_GetTextWidth(IH_FONT_NORMAL, buf);

                    color.r = color.g = 255;
                    color.b = 0;

                    IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, NULL);

                    if(item->price_fixed)
                    {
                         pos.x.pixel =
                             (overlay->position.x + overlay->position.w) -
                             IH_OVERLAY_STORE_OFFSET_X -
                             IH_GetTextWidth(IH_FONT_NORMAL, "F");

                         color.r = color.g = color.b = 255;

                         IH_RenderText(IH_FONT_NORMAL, "F", &pos, color,
                                       NULL);
                    }
               }

               /* Free the item */
               rnfree(item);
          }

          if(st_ptr->stock_num > IH_OVERLAY_STORE_INVENTORY_LINES)
          {
               /* Show "more" reminder (after the last object ) */
               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_STORE_OFFSET_X +
                   overlay->gfx.store.inv_x + 20;
               pos.y.pixel =
                   overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
                   ((k + 1) * (IH_GetFontHeight(IH_FONT_NORMAL) + 2));

               color.r = color.g = color.b = 255;

               IH_RenderText(IH_FONT_NORMAL, "-more-", &pos, color, NULL);

               /* Indicate the "current page" */
               sprintf(buf, "(Page %d)",
                       store_top / IH_OVERLAY_STORE_INVENTORY_LINES + 1);
               pos.x.pixel =
                   (overlay->position.x + overlay->position.w) -
                   IH_OVERLAY_STORE_OFFSET_X -
                   IH_GetTextWidth(IH_FONT_NORMAL, buf);
               pos.y.pixel =
                   overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
                   ((IH_OVERLAY_STORE_INVENTORY_LINES +
                     1) * (IH_GetFontHeight(IH_FONT_NORMAL) + 2));

               IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, NULL);
          }
     }

     /* Draw the commands.
      */
     pos.x.pixel =
         overlay->position.x + IH_OVERLAY_STORE_OFFSET_X +
         overlay->gfx.store.inv_x;
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
         ((IH_OVERLAY_STORE_INVENTORY_LINES +
           2) * (IH_GetFontHeight(IH_FONT_NORMAL) + 2));

     color.r = color.g = color.b = 255;

     IH_RenderText(IH_FONT_NORMAL, "ESC) Exit from Building.", &pos, color,
                   &rect);

     /* Commands */
     pos.y.pixel += rect.h;
     IH_RenderText(IH_FONT_NORMAL, "g) Get/Purchase an item.", &pos, color,
                   &rect);

     pos.y.pixel += rect.h;
     IH_RenderText(IH_FONT_NORMAL, "d) Drop/Sell an item.", &pos, color,
                   NULL);

     /* Move over half way */
     pos.x.pixel += (overlay->position.w - overlay->gfx.store.inv_x) / 2;
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
         ((IH_OVERLAY_STORE_INVENTORY_LINES +
           2) * (IH_GetFontHeight(IH_FONT_NORMAL) + 2));

     /* Browse if necessary */
     if(st_ptr->stock_num > 12)
     {
          IH_RenderText(IH_FONT_NORMAL, "SPACE) Next page of stock.", &pos,
                        color, &rect);

          pos.y.pixel += rect.h;
     }

     /* Add in the eXamine option */
     if(rogue_like_commands)
          IH_RenderText(IH_FONT_NORMAL, "x) eXamine an item.", &pos, color,
                        &rect);
     else
          IH_RenderText(IH_FONT_NORMAL, "l) Look at an item.", &pos, color,
                        &rect);

     rnfree(path_data);
}

void
IH_ReleaseOverlay_Store(Overlay * overlay)
{
     if(!overlay)
          return;
}
