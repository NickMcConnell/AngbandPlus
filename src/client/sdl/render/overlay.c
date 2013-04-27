
/* $Id: overlay.c,v 1.12 2003/03/18 19:17:41 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL_draw.h"
#include "SDL_image.h"

#include "angband.h"
#include "ironhells.h"
#include "file.h"
#include "path.h"
#include "sdl/render/icon.h"
#include "sdl/render/overlay.h"

static Overlay  overlays[IH_OVERLAY_END];

static int
IH_InitOverlay(int overlay_num)
{
     Overlay        *overlay;
     int             shown = FALSE;
     Uint32          rmask, gmask, bmask, amask;
     char           *backdrop_file = NULL;
     char           *path_data;

     if(overlay_num >= IH_OVERLAY_END)
          return FALSE;

     if(!SDL_SemWait(ih.sem.overlay))
     {
          overlay = &overlays[overlay_num];

          memset(overlay, 0, sizeof(Overlay));

          overlay->type = overlay_num;

#if SDL_BYTEORDER == SDL_BIG_ENDIAN
          rmask = 0xff000000;
          gmask = 0x00ff0000;
          bmask = 0x0000ff00;
          amask = 0x000000ff;
#else
          rmask = 0x000000ff;
          gmask = 0x0000ff00;
          bmask = 0x00ff0000;
          amask = 0xff000000;
#endif

          path_data = IH_GetDataDir("gfx");

          switch (overlay->type)
          {
               case IH_OVERLAY_CHARACTER:
                    {
                         SDL_Surface    *scale;
                         char           *scale_file;
                         int             group, icon_size;
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

               case IH_OVERLAY_INVENTORY:
                    break;

               case IH_OVERLAY_EQUIPMENT:
                    break;

               case IH_OVERLAY_BOOK:
                    break;

               case IH_OVERLAY_MESSAGES:
                    break;

               case IH_OVERLAY_ERROR:
                    /* Position and size are determined at the time of first
                     * rendering, because that is when we can be certain that the
                     * text string will be provided.
                     */
                    break;

               case IH_OVERLAY_DIALOG:
                    /* Position and size are determined at the time of first
                     * rendering, because that is when we can be certain that the
                     * prompt string and initial buffer text will be provided.
                     */
                    break;

               case IH_OVERLAY_OPTIONS:
                    break;
          }

          if(!overlay->surface)
          {
               if(backdrop_file)
                    overlay->surface =
                        IMG_Load_RW(SDL_RWFromFile(backdrop_file, "rb"),
                                    1);
          }

          shown = overlay->shown = TRUE;

          rnfree(backdrop_file);
          rnfree(path_data);

          SDL_SemPost(ih.sem.overlay);
     }

     return shown;
}

void
IH_RenderOverlays(void)
{
     int             overlay_num;

     for(overlay_num = IH_OVERLAY_CHARACTER;
         overlay_num < IH_OVERLAY_END; overlay_num++)
     {
          Overlay        *overlay;
          int             shown = FALSE;

          overlay = &overlays[overlay_num];

          if(!SDL_SemWait(ih.sem.overlay))
          {
               shown = overlay->shown;

               SDL_SemPost(ih.sem.overlay);
          }

          if(!shown)
               continue;

          switch (overlay->type)
          {
               case IH_OVERLAY_CHARACTER:
                    IH_RenderOverlay_Character(overlay);
                    break;

               case IH_OVERLAY_INVENTORY:
                    IH_RenderOverlay_Inventory(overlay);
                    break;

               case IH_OVERLAY_EQUIPMENT:
                    IH_RenderOverlay_Equipment(overlay);
                    break;

               case IH_OVERLAY_BOOK:
                    IH_RenderOverlay_Book(overlay);
                    break;

               case IH_OVERLAY_MESSAGES:
                    IH_RenderOverlay_Messages(overlay);
                    break;

               case IH_OVERLAY_ERROR:
                    IH_RenderOverlay_Error(overlay);
                    break;

               case IH_OVERLAY_DIALOG:
                    IH_RenderOverlay_Dialog(overlay);
                    break;

               case IH_OVERLAY_OPTIONS:
                    IH_RenderOverlay_Options(overlay);
                    break;
          }
     }
}

bool
IH_ActivateOverlay(int overlay_num)
{
     Overlay        *overlay = NULL;
     int             shown = FALSE;

     if(overlay_num >= IH_OVERLAY_END)
          return FALSE;

     if(!SDL_SemWait(ih.sem.overlay))
     {
          shown = overlays[overlay_num].shown;

          SDL_SemPost(ih.sem.overlay);
     }

     if(shown)
          return TRUE;

     return IH_InitOverlay(overlay_num);
}

void
IH_DeactivateOverlay(int overlay_num)
{
     if(overlay_num >= IH_OVERLAY_END)
          return;

     if(!SDL_SemWait(ih.sem.overlay))
     {
          overlays[overlay_num].shown = FALSE;

          SDL_SemPost(ih.sem.overlay);
     }
}

bool
IH_ToggleOverlay(int overlay_num)
{
     Overlay        *overlay = NULL;
     int             shown = FALSE;

     if(overlay_num >= IH_OVERLAY_END)
          return FALSE;

     if(!SDL_SemWait(ih.sem.overlay))
     {
          shown = overlays[overlay_num].shown;

          SDL_SemPost(ih.sem.overlay);
     }

     if(shown)
     {
          IH_DeactivateOverlay(overlay_num);

          return TRUE;
     }

     return IH_ActivateOverlay(overlay_num);
}
