
/* $Id: overlay.c,v 1.4 2003/04/21 02:31:44 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL_draw.h"
#include "SDL_image.h"

/* Internal headers */
#include "angband/angband.h"
#include "angband/z-disp.h"
#include "ironhells.h"
#include "overlay.h"
#include "path.h"
#include "platform/platform.h"
#include "render/icon.h"
#include "render/text.h"

static Overlay  overlays[DISPLAY_MAX];
static Overlay *overlay_order[DISPLAY_MAX];

static cptr     store_icons[] = {
     "general",
     "armory",
     "weapon-smith",
     "temple",
     "alchemist",
     "magic-shop",
     "black-market",
     "home",

     NULL
};

extern int      store_num;

static void
overlay_condense(void)
{
     int             i;

     /* Iterate over the list of overlays.
      */
     for(i = 0;
         i < DISPLAY_MAX; i++)
     {
          Overlay *overlay;
          int shown = FALSE;

          /* Get the overlay.
           */          
          overlay = overlay_order[i];
          
          if(!SDL_SemWait(ih.ipc.sem.overlay))
          {
               /* Get the overlay's shown flag?
                */
               if(overlay)
                    shown = overlay->shown;

               SDL_SemPost(ih.ipc.sem.overlay);
          }

          /* If there isn't an overlay, or it's not shown, then
           * condense the list.
           */
          if(!overlay ||
             !shown)
          {
               int             j;

               for(j = i + 1; j < DISPLAY_MAX; j++)
               {
                    /* Bump it down one place.
                     */
                    overlay_order[j - 1] = overlay_order[j];
                    
                    /* Clear the old spot.
                     */
                    overlay_order[j] = NULL;
               }
          }
     }
}

static void
overlay_unplace(Overlay *overlay)
{
     int             i;

     if(!overlay)
          return;
          
     /* Iterate over the list of overlays.
      */
     for(i = 0;
         i < DISPLAY_MAX; i++)
     {
          if(overlay_order[i] == overlay)
          {
               overlay_order[i] = NULL;
               break;
          }
     }
}

static void
overlay_place(Overlay *overlay)
{
     int             i;
     
     if(!overlay)
          return;

     /* Condense them first.
      */
     overlay_unplace(overlay);
     overlay_condense();
     
     for(i = 0;
         i < DISPLAY_MAX; i++)
     {
          if(!overlay_order[i])
          {
               overlay_order[i] = overlay;
               return;
          }
     }
}

static int
overlay_init(int overlay_num)
{
     Overlay        *overlay;
     int             prep = FALSE;
     char           *backdrop_file = NULL;
     char           *path_data;

     if(overlay_num >= DISPLAY_MAX)
          return FALSE;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          overlay = &overlays[overlay_num];

          memset(overlay, 0, sizeof(Overlay));
          overlay->type = overlay_num;

          path_data = IH_GetDataDir("gfx");

          switch (overlay->type)
          {
               case DISPLAY_CHARACTER:
fprintf(stderr, "Initialize the character overlay.\n");
                    overlay->render_func = IH_RenderOverlay_Character;
                    overlay->release_func = IH_ReleaseOverlay_Character;
                    {
                         char           *scale_file;
                         int             icon_size;
                         int             icon_text_group_height,
                             history_group_height,
                             personal_info_group_height,
                             scales_group_height, scale_width,
                             scale_height;

                         icon_size = IH_GetIconSize(IH_ICON_SIZE_CURRENT);
                         scale_width = 115; /* default */
                         scale_height = 7; /* default */

                         overlay->position.x = icon_size + 2;
                         overlay->position.y = icon_size + 2;

                         icon_text_group_height =
                             MAX(2 * IH_FONT_NORMAL_SIZE, icon_size);
                         history_group_height = 3 * IH_FONT_NORMAL_SIZE;
                         personal_info_group_height =
                             4 * IH_FONT_NORMAL_SIZE;
                         scales_group_height = 8 * IH_FONT_NORMAL_SIZE;

                         scale_file = IH_PathBuild(path_data, "screen",
                                                   "character-info-scale."
                                                   IH_IMAGE_FORMAT_EXT,
                                                   NULL);

                         if(scale_file)
                         {
                              SDL_Surface    *scale;

                              scale = overlay->gfx.character.scale =
                                  IMG_Load_RW(SDL_RWFromFile
                                              (scale_file, "rb"), 1);
                              if(scale)
                              {
                                   scale_width = scale->w;
                                   scale_height = scale->h;
                              }
                         }

                         overlay->position.w =
                             (2 * (icon_size + 120) + scale_width);
                         overlay->position.h =
                             MAX(personal_info_group_height +
                                 (6 * icon_text_group_height),
                                 MAX((6 * icon_text_group_height) +
                                     (2 * IH_FONT_NORMAL_SIZE),
                                     scales_group_height +
                                     (2 * icon_text_group_height)) +
                                 history_group_height);

                         if(path_data)
                              backdrop_file =
                                  IH_PathBuild(path_data, "screen",
                                               "character-info."
                                               IH_IMAGE_FORMAT_EXT, NULL);

                         rnfree(scale_file);
                    }
                    break;

               case DISPLAY_INVENTORY:
                    {
                         object_type    *o_ptr;
                         int             i, k, z = 0;

                         overlay->render_func = IH_RenderOverlay_Inventory;
                         overlay->release_func =
                             IH_ReleaseOverlay_Inventory;

                         /* Find the "final" slot */
                         for(i = 0; i < INVEN_PACK; i++)
                         {
                              o_ptr = &inventory[i];

                              /* Skip non-objects */
                              if(!o_ptr->k_idx)
                                   continue;

                              /* Track */
                              z = i + 1;
                         }

                         /* Process the inventory */
                         for(k = 0, i = 0; i < z; i++)
                         {
                              /* Examine the item */
                              o_ptr = &inventory[i];

                              overlay->gfx.inventory.show_index[k] = FALSE;

                              /* Is this item "acceptable"? */
                              if(item_tester_okay(o_ptr))
                              {
                                   /* Prepare an "index" */
                                   overlay->gfx.inventory.show_index[k] =
                                       TRUE;
                              }

                              /* Track */
                              k++;
                         }

                         overlay->gfx.inventory.n_items = k;

                         /* Calculate size and position.
                          */
                         overlay->position.w =
                             (show_labels ?
                              IH_OVERLAY_INVENTORY_INDEX_WIDTH : 0) +
                             IH_OVERLAY_INVENTORY_COLUMN_SPACING +
                             IH_OVERLAY_INVENTORY_DESC_WIDTH +
                             IH_OVERLAY_INVENTORY_COLUMN_SPACING +
                             (show_weights ?
                              IH_OVERLAY_INVENTORY_WEIGHT_WIDTH : 0);
                         overlay->position.h =
                             (overlay->gfx.inventory.n_items *
                              IH_GetFontHeight(IH_FONT_NORMAL)) +
                             (IH_OVERLAY_INVENTORY_OFFSET_Y * 2);

                         overlay->position.x =
                             ih.display.width - overlay->position.w -
                             IH_GetIconSize(IH_ICON_SIZE_CURRENT) - 2;
                         overlay->position.y =
                             IH_GetIconSize(IH_ICON_SIZE_CURRENT);
                    }
                    break;

               case DISPLAY_EQUIPMENT:
                    {
                         int             i, k;

                         overlay->render_func = IH_RenderOverlay_Equipment;
                         overlay->release_func =
                             IH_ReleaseOverlay_Equipment;

                         /* Scan the equipment list */
                         for(k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
                         {
                              object_type    *o_ptr;
                              char            o_name[80];

                              o_ptr = &inventory[i];

                              /* Is this item acceptable? */
                              overlay->gfx.equipment.show_index[k] = FALSE;
                              if(!item_tester_okay(o_ptr))
                                   continue;

                              overlay->gfx.equipment.show_index[k] = TRUE;

                              /* Description */
                              object_desc(o_name, sizeof(o_name), o_ptr,
                                          TRUE, 3);

                              /* Save the index */
                              overlay->gfx.equipment.out_index[k] = i;

                              /* Get inventory color */
                              overlay->gfx.equipment.out_color[k] =
                                  tval_to_attr[o_ptr->tval %
                                               N_ELEMENTS(tval_to_attr)];

                              /* Save the description */
                              my_strcpy(overlay->gfx.equipment.out_desc[k],
                                        o_name,
                                        sizeof(overlay->gfx.equipment.
                                               out_desc[0]));

                              /* Advance the entry */
                              k++;
                         }

                         overlay->gfx.equipment.n_items = k;

                         /* Calculate size and position.
                          */
                         overlay->position.w =
                             IH_OVERLAY_EQUIPMENT_INDEX_WIDTH +
                             IH_OVERLAY_EQUIPMENT_COLUMN_SPACING +
                             (show_labels ?
                              IH_OVERLAY_EQUIPMENT_LABEL_WIDTH : 0) +
                             IH_OVERLAY_EQUIPMENT_COLUMN_SPACING +
                             IH_OVERLAY_EQUIPMENT_DESC_WIDTH +
                             IH_OVERLAY_EQUIPMENT_COLUMN_SPACING +
                             (show_weights ?
                              IH_OVERLAY_EQUIPMENT_WEIGHT_WIDTH : 0);
                         overlay->position.h =
                             (overlay->gfx.equipment.n_items *
                              IH_GetFontHeight(IH_FONT_NORMAL)) +
                             (IH_OVERLAY_EQUIPMENT_OFFSET_Y * 2);
                    }
                    break;

               case DISPLAY_BOOK:
                    overlay->render_func = IH_RenderOverlay_Book;
                    overlay->release_func = IH_ReleaseOverlay_Book;

                    overlay->position.w = IH_OVERLAY_BOOK_INDEX_WIDTH +
                        IH_OVERLAY_BOOK_COLUMN_SPACING +
                        IH_OVERLAY_BOOK_NAME_WIDTH +
                        IH_OVERLAY_BOOK_COLUMN_SPACING +
                        IH_OVERLAY_BOOK_LEVEL_WIDTH +
                        IH_OVERLAY_BOOK_COLUMN_SPACING +
                        IH_OVERLAY_BOOK_MANA_WIDTH +
                        IH_OVERLAY_BOOK_COLUMN_SPACING +
                        IH_OVERLAY_BOOK_FAIL_WIDTH +
                        IH_OVERLAY_BOOK_COLUMN_SPACING +
                        IH_OVERLAY_BOOK_INFO_WIDTH;
                    overlay->position.y =
                        IH_GetIconSize(IH_ICON_SIZE_CURRENT);

                    break;

               case DISPLAY_MESSAGES:
                    overlay->render_func = IH_RenderOverlay_Messages;
                    overlay->release_func = IH_ReleaseOverlay_Messages;

                    /* Position is fixed in the bottom-left portion of the screen.
                     */
                    break;

               case DISPLAY_MESSAGE_BROWSER:
                    overlay->render_func = IH_RenderOverlay_Browser;
                    overlay->release_func = IH_ReleaseOverlay_Browser;
                    break;

               case DISPLAY_MAP:
                    overlay->render_func = IH_RenderOverlay_Map;
                    overlay->release_func = IH_ReleaseOverlay_Map;
                    break;

               case DISPLAY_STORE:
                    overlay->render_func = IH_RenderOverlay_Store;
                    overlay->release_func = IH_ReleaseOverlay_Store;

                    overlay->position.w = 550;
                    overlay->position.h =
                        ((IH_GetFontHeight(IH_FONT_NORMAL) +
                          2) * (IH_OVERLAY_STORE_INVENTORY_LINES + 1)) +
                        IH_GetFontHeight(IH_FONT_NORMAL) * 4;

                    /* Load the shopkeeper's image.
                     */
                    if(!overlay->gfx.store.shop_image)
                         overlay->gfx.store.shop_image =
                             IH_LoadIcon(IH_ICON_SIZE_NONE,
                                         store_icons[store_num]);

                    overlay->gfx.store.inv_x = 130;
                    overlay->gfx.store.shop_info_y = 75;

                    if(!overlay->gfx.store.gold_icon)
                         overlay->gfx.store.gold_icon =
                             IH_LoadIcon(IH_ICON_SIZE_CURRENT, "gold");

                    if(overlay->gfx.store.gold_icon)
                    {
                         overlay->gfx.store.gold_y =
                             overlay->position.h -
                             overlay->gfx.store.gold_icon->h - 10;
                    }
                    else
                    {
                         overlay->gfx.store.gold_y =
                             overlay->position.h -
                             IH_GetFontHeight(IH_FONT_NORMAL) - 10;
                    }

                    overlay->position.x =
                        (ih.display.width -
                         (overlay->position.w +
                          (IH_OVERLAY_STORE_OFFSET_X * 2))) / 2;
                    overlay->position.y =
                        (ih.display.height -
                         (overlay->position.h +
                          (IH_OVERLAY_STORE_OFFSET_Y * 2))) / 2;
                    break;

               case DISPLAY_ERROR:
                    overlay->render_func = IH_RenderOverlay_Error;
                    overlay->release_func = IH_ReleaseOverlay_Error;

                    /* Position and size are determined at the time of first
                     * rendering, because that is when we can be certain that the
                     * text string will be provided.
                     */
                    overlay->position.x = 0;

                    /* Force recalc on initialization, so we process the buffers.
                     */
                    overlay->recalc = TRUE;

                    break;

               case DISPLAY_DIALOG:
                    overlay->render_func = IH_RenderOverlay_Dialog;
                    overlay->release_func = IH_ReleaseOverlay_Dialog;

                    /* Position and size are determined at the time of first
                     * rendering, because that is when we can be certain that the
                     * prompt string and initial buffer text will be provided.
                     */
                    overlay->position.x = 0;

                    /* Force recalc on initialization, so we process the buffers.
                     */
                    overlay->recalc = TRUE;

                    break;

               case DISPLAY_OPTIONS:
                    overlay->render_func = IH_RenderOverlay_Options;
                    overlay->release_func = IH_ReleaseOverlay_Options;

                    overlay->position.w = 550;
                    overlay->position.h =
                        (IH_GetFontHeight(IH_FONT_LARGE) * 2) +
                        (IH_OVERLAY_OPTIONS_OFFSET_Y * 2) +
                        (IH_GetFontHeight(IH_FONT_NORMAL) *
                         IH_OVERLAY_OPTIONS_LINES);

                    overlay->position.x =
                        (ih.display.width -
                         (overlay->position.w +
                          (IH_OVERLAY_OPTIONS_OFFSET_X * 2))) / 2;
                    overlay->position.y =
                        (ih.display.height -
                         (overlay->position.h +
                          (IH_OVERLAY_OPTIONS_OFFSET_Y * 2))) / 2;

                    overlay->var[0] = -1;
                    break;

               case DISPLAY_OPPONENT:
                    overlay->render_func = IH_RenderOverlay_Opponent;
                    overlay->release_func = IH_ReleaseOverlay_Opponent;
                    break;

               case DISPLAY_HELP:
                    overlay->render_func = IH_RenderOverlay_Help;
                    overlay->release_func = IH_ReleaseOverlay_Help;
                    break;

               case DISPLAY_MEMORY:
                    overlay->render_func = IH_RenderOverlay_Memory;
                    overlay->release_func = IH_ReleaseOverlay_Memory;
                    break;
          }

          if(!overlay->background)
          {
               if(backdrop_file)
                    overlay->background =
                        IMG_Load_RW(SDL_RWFromFile(backdrop_file, "rb"),
                                    1);
          }

          prep = overlay->prep = TRUE;

          rnfree(backdrop_file);
          rnfree(path_data);

          SDL_SemPost(ih.ipc.sem.overlay);
     }

     return prep;
}

int
IH_InitOverlays(void)
{
     memset(overlays, 0, sizeof(overlays));
     memset(overlay_order, 0, sizeof(overlay_order));

     return 0;
}

void
IH_RenderOverlays(void)
{
     int             i;

     for(i = 0; i < DISPLAY_MAX; i++)
     {
          Overlay        *overlay;
          int             shown = FALSE;

          overlay = overlay_order[i];
          
          if(!overlay)
               continue;

          if(!SDL_SemWait(ih.ipc.sem.overlay))
          {
               shown = overlay->shown;

               SDL_SemPost(ih.ipc.sem.overlay);
          }

          if(!shown)
               continue;

          if(!overlay->render_func)
               continue;

          (*overlay->render_func) (overlay);
     }
}

bool
IH_Overlay_Prepare(int overlay_num)
{
     int             prep = FALSE;

     if(overlay_num >= DISPLAY_MAX)
          return FALSE;

#if 0
     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          prep = overlays[overlay_num].prep;

          SDL_SemPost(ih.ipc.sem.overlay);
     }

     if(prep)
          return TRUE;
#endif

     return overlay_init(overlay_num);
}

bool
IH_Overlay_Show(int overlay_num)
{
     int             shown = FALSE;

     if(overlay_num >= DISPLAY_MAX)
          return FALSE;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          if(overlays[overlay_num].prep)
          {
#ifdef DEBUG
               fprintf(stderr, "Showing overlay %d\n", overlay_num);
#endif
               shown = overlays[overlay_num].shown = TRUE;
          }

          SDL_SemPost(ih.ipc.sem.overlay);
     }

     overlay_place(&overlays[overlay_num]);

     return shown;
}

void
IH_Overlay_Hide(int overlay_num)
{
     Overlay        *overlay = NULL;

     if(overlay_num >= DISPLAY_MAX)
          return;

     overlay = &overlays[overlay_num];

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          overlay->shown = FALSE;

          if(overlay->release_func)
               (*overlay->release_func) (overlay);

          SDL_SemPost(ih.ipc.sem.overlay);
     }
     
     overlay_unplace(overlay);
}

bool
IH_Overlay_Toggle(int overlay_num)
{
     int             shown = FALSE;

     if(overlay_num >= DISPLAY_MAX)
          return FALSE;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          shown = overlays[overlay_num].shown;

          SDL_SemPost(ih.ipc.sem.overlay);
     }

     if(shown)
     {
          IH_Overlay_Hide(overlay_num);

          return TRUE;
     }

     return IH_Overlay_Show(overlay_num);
}

int
IH_Overlay_GetState(int overlay_num)
{
     int             shown = FALSE;

     if(overlay_num >= DISPLAY_MAX)
          return TRUE;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          shown = overlays[overlay_num].shown;

          SDL_SemPost(ih.ipc.sem.overlay);
     }

     return shown;
}

int
IH_Overlay_GetDimensions(int overlay_num,
                         SDL_Rect * rect)
{
     Overlay        *overlay;

     if(overlay_num >= DISPLAY_MAX || !rect)
          return FALSE;

     overlay = &overlays[overlay_num];

     if(!overlay)
          return FALSE;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          memcpy(rect, &overlay->position, sizeof(SDL_Rect));

          SDL_SemPost(ih.ipc.sem.overlay);
     }

     return TRUE;
}

void
IH_Overlay_Update(int overlay_num)
{
     Overlay        *overlay;

     if(overlay_num >= DISPLAY_MAX)
          return;

     overlay = &overlays[overlay_num];

     if(!overlay)
          return;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          overlay->update = TRUE;

          SDL_SemPost(ih.ipc.sem.overlay);
     }
}

void
IH_Overlay_Recalc(int overlay_num)
{
     Overlay        *overlay;

     if(overlay_num >= DISPLAY_MAX)
          return;

     overlay = &overlays[overlay_num];

     if(!overlay)
          return;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          overlay->recalc = TRUE;

          SDL_SemPost(ih.ipc.sem.overlay);
     }
}

void
IH_Overlay_SetBuffer(int overlay_num,
                     int num,
                     char *buffer)
{
     Overlay        *overlay;

     fprintf(stderr, "IH_Overlay_SetBuffer()\n");

     fprintf(stderr,
             "IH_Overlay_SetBuffer(): overlay_num = %d, num = %d\n",
             overlay_num, num);
     if(overlay_num >= DISPLAY_MAX || num >= IH_OVERLAY_MAX_BUFFERS)
          return;

     fprintf(stderr, "IH_Overlay_SetBuffer(): get pointer to overlay %d\n",
             overlay_num);
     overlay = &overlays[overlay_num];

     fprintf(stderr, "IH_Overlay_SetBuffer(): check for overlay\n");
     if(!overlay)
          return;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          fprintf(stderr, "IH_Overlay_SetBuffer(): set buffer %d to %s\n",
                  num, buffer);
          overlay->buffer[num] = buffer;

          SDL_SemPost(ih.ipc.sem.overlay);
     }
     fprintf(stderr, "IH_Overlay_SetBuffer(): return\n");
}

void
IH_Overlay_SetVar(int overlay_num,
                  int num,
                  int var)
{
     Overlay        *overlay;

     if(overlay_num >= DISPLAY_MAX || num >= IH_OVERLAY_MAX_VARS)
          return;

     overlay = &overlays[overlay_num];

     if(!overlay)
          return;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          overlay->var[num] = var;

          SDL_SemPost(ih.ipc.sem.overlay);
     }
}

void
IH_Overlay_SetFlags(int overlay_num,
                    u32b flags)
{
     Overlay        *overlay;

     if(overlay_num >= DISPLAY_MAX)
          return;

     overlay = &overlays[overlay_num];

     if(!overlay)
          return;

     if(!SDL_SemWait(ih.ipc.sem.overlay))
     {
          overlay->flags = flags;

          SDL_SemPost(ih.ipc.sem.overlay);
     }
}
