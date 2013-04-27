
/* $Id: shop.c,v 1.4 2003/04/20 05:20:58 cipher Exp $ */

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

/* Internal headers */
#include "angband/angband.h"
#include "ironhells.h"
#include "overlay.h"
#include "strings.h"
#include "platform/platform.h"
#include "render/text.h"

extern int      store_num;
extern int      store_top;
extern store_type *st_ptr;
extern owner_type *ot_ptr;

void
IH_RenderOverlay_Store(Overlay * overlay)
{
     ihColor         color;
     SDL_Rect        rect;
     ihFontPos       pos;
     char           *path_data, buf[20];
     cptr            store_name =
         (f_name + f_info[FEAT_SHOP_HEAD + store_num].name);
     cptr            owner_name = &(b_name[ot_ptr->owner_name]);
     cptr            race_name = p_name + p_info[ot_ptr->owner_race].name;
     int             gold_x;

fprintf(stderr, "IH_RenderOverlay_Store()\n");

     if(!overlay)
          return;

fprintf(stderr, "IH_RenderOverlay_Store(): get gfx path\n");
     path_data = IH_GetDataDir("gfx");

     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_STORE_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_STORE_OFFSET_Y * 2),
                  NULL);
     IH_FrameArea(overlay->position.x, overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_STORE_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_STORE_OFFSET_Y * 2),
                  NULL);

     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.x.pixel = overlay->position.x + IH_OVERLAY_STORE_OFFSET_X;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel = overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y;

     /* Draw the store information (type and cost max).
      */
     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE, store_name, &pos, &color, 0, &rect);

     if(store_num != STORE_HOME)
     {
          pos.y.pixel += rect.h;
          sprintf(buf, "(%ld)", (long) ot_ptr->max_cost);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);
     }

     /* Draw the shopkeep's picture.
      */
     rect.h += 10;
     if(overlay->gfx.store.shop_image)
     {
          SDL_Rect        irect;

          irect.x = overlay->position.x + IH_OVERLAY_STORE_OFFSET_X;
          irect.y = (pos.y.pixel += rect.h + 5);

          IH_RenderImage(overlay->gfx.store.shop_image, 0, &irect);

          rect.h = overlay->gfx.store.shop_image->h;
     }

     if(store_num != STORE_HOME)
     {
          /* Draw the shopkeep's information.
           */
          IH_AttrToColor(COLOR_YELLOW, &color);

          pos.y.pixel += rect.h;
          IH_RenderText(IH_FONT_NORMAL, owner_name, &pos, &color, 0,
                        &rect);

          pos.y.pixel += rect.h;
          IH_RenderText(IH_FONT_NORMAL, race_name, &pos, &color, 0, &rect);
     }

     /* Draw the player's gold.
      */
     if(overlay->gfx.store.gold_icon)
     {
          rect.x = overlay->position.x + IH_OVERLAY_STORE_OFFSET_X;
          rect.y =
              overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
              overlay->gfx.store.gold_y;

          IH_RenderImage(overlay->gfx.store.gold_icon, 0, &rect);

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
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, NULL);

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
          IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_DESCRIPTION,
                        &pos, &color, 0, NULL);

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
               IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_WEIGHT,
                             &pos, &color, 0, NULL);
          }

          if(store_num != STORE_HOME)
          {
               pos.x.pixel =
                   (overlay->position.x + overlay->position.w) -
                   IH_OVERLAY_STORE_OFFSET_X -
                   IH_GetTextWidth(IH_FONT_NORMAL, "Price");
               IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_PRICE,
                             &pos, &color, 0, NULL);
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

               IH_AttrToColor(COLOR_WHITE, &color);

               /* Item selection letter */
               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_STORE_OFFSET_X +
                   overlay->gfx.store.inv_x;
               sprintf(buf, "%c)", item->label);
               IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, NULL);

               /* Item description */
               IH_AttrToColor(item->attr, &color);

               pos.x.pixel =
                   overlay->position.x + IH_OVERLAY_STORE_OFFSET_X +
                   overlay->gfx.store.inv_x + 20;
               IH_RenderText(IH_FONT_NORMAL, item->o_name, &pos, &color,
                             0, NULL);

               /* Item weight */
               if(show_weights)
               {
                    sprintf(buf, IH_TEXT_OVERLAY_SHOP_WEIGHT_FMT,
                            item->wgt / 10, item->wgt % 10);
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

                    IH_AttrToColor(COLOR_WHITE, &color);

                    IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0,
                                  NULL);
               }

               if(store_num != STORE_HOME)
               {
                    /* Item price */
                    sprintf(buf, "%9ld ", item->price);
                    pos.x.pixel =
                        (overlay->position.x + overlay->position.w) -
                        IH_OVERLAY_STORE_OFFSET_X -
                        IH_GetTextWidth(IH_FONT_NORMAL, buf);

                    IH_AttrToColor(COLOR_YELLOW, &color);

                    IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0,
                                  NULL);

                    if(item->price_fixed)
                    {
                         pos.x.pixel =
                             (overlay->position.x + overlay->position.w) -
                             IH_OVERLAY_STORE_OFFSET_X -
                             IH_GetTextWidth(IH_FONT_NORMAL, "F");

                         IH_AttrToColor(COLOR_WHITE, &color);

                         IH_RenderText(IH_FONT_NORMAL, "!", &pos, &color,
                                       0, NULL);
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

               IH_AttrToColor(COLOR_WHITE, &color);

               IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_MORE,
                             &pos, &color, 0, NULL);

               /* Indicate the "current page" */
               sprintf(buf, IH_TEXT_OVERLAY_SHOP_PAGE_FMT,
                       store_top / IH_OVERLAY_STORE_INVENTORY_LINES + 1);
               pos.x.pixel =
                   (overlay->position.x + overlay->position.w) -
                   IH_OVERLAY_STORE_OFFSET_X -
                   IH_GetTextWidth(IH_FONT_NORMAL, buf);
               pos.y.pixel =
                   overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
                   ((IH_OVERLAY_STORE_INVENTORY_LINES +
                     1) * (IH_GetFontHeight(IH_FONT_NORMAL) + 2));

               IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, NULL);
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

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_CMD_EXIT, &pos,
                   &color, 0, &rect);

     /* Commands */
     pos.y.pixel += rect.h;
     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_CMD_BUY, &pos,
                   &color, 0, &rect);

     pos.y.pixel += rect.h;
     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_CMD_SELL, &pos,
                   &color, 0, NULL);

     /* Move over half way */
     pos.x.pixel += (overlay->position.w - overlay->gfx.store.inv_x) / 2;
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_STORE_OFFSET_Y +
         ((IH_OVERLAY_STORE_INVENTORY_LINES +
           2) * (IH_GetFontHeight(IH_FONT_NORMAL) + 2));

     /* Browse if necessary */
     if(st_ptr->stock_num > 12)
     {
          IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_CMD_NEXT_PAGE,
                        &pos, &color, 0, &rect);

          pos.y.pixel += rect.h;
     }

     /* Add in the eXamine option */
     if(rogue_like_commands)
          IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_CMD_EXAMINE,
                        &pos, &color, 0, &rect);
     else
          IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_SHOP_CMD_LOOK,
                        &pos, &color, 0, &rect);

     rnfree(path_data);
}

void
IH_ReleaseOverlay_Store(Overlay * overlay)
{
     if(!overlay)
          return;
}
