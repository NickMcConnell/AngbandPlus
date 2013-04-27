/* File: overlay.c */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL_draw.h"
#include "SDL_image.h"

#include "ironhells.h"
#include "file.h"
#include "sdl/render/overlay.h"
#include "sdl/render/icon.h"
#include "sdl/render/text.h"
#include "sdl/strings.h"

static Overlay overlays[IH_OVERLAY_END];

static enum
{
     IH_OV_CHARACTER_NAME,
     IH_OV_CHARACTER_GENDER,
     IH_OV_CHARACTER_RACE,
     IH_OV_CHARACTER_CLASS,
     IH_OV_CHARACTER_TITLE,
     IH_OV_CHARACTER_HP,
     IH_OV_CHARACTER_SP,
     
     IH_OV_CHARACTER_LEVEL,
     IH_OV_CHARACTER_CUREXP,
     IH_OV_CHARACTER_MAXEXP,
     IH_OV_CHARACTER_ADVEXP,
     
     IH_OV_CHARACTER_GOLD,
     
     IH_OV_CHARACTER_BURDEN,
     
     IH_OV_CHARACTER_AGE,
     IH_OV_CHARACTER_HEIGHT,
     IH_OV_CHARACTER_WEIGHT,
     IH_OV_CHARACTER_STATUS,
     IH_OV_CHARACTER_MAXIMIZE,
     IH_OV_CHARACTER_PRESERVE,
     
     IH_OV_CHARACTER_ARMOR,
     IH_OV_CHARACTER_FIGHT,
     IH_OV_CHARACTER_MELEE,
     IH_OV_CHARACTER_SHOOT,
     IH_OV_CHARACTER_BLOWS,
     IH_OV_CHARACTER_SHOTS,
     
     IH_OV_CHARACTER_INFRA,
     
     IH_OV_CHARACTER_STR,
     IH_OV_CHARACTER_INT,
     IH_OV_CHARACTER_WIS,
     IH_OV_CHARACTER_DEX,
     IH_OV_CHARACTER_CON,
     IH_OV_CHARACTER_CHR,
     
     IH_OV_CHARACTER_SAVINGTHROW,
     IH_OV_CHARACTER_STEALTH,
     IH_OV_CHARACTER_FIGHTING,
     IH_OV_CHARACTER_SHOOTING,
     IH_OV_CHARACTER_DISARMING,
     IH_OV_CHARACTER_MAGICDEVICE,
     IH_OV_CHARACTER_PERCEPTION,
     IH_OV_CHARACTER_SEARCHING,

     IH_OV_CHARACTER_END
};

static struct CharInfoPos
{
     int item;
     cptr text;
     int line;
     int column;
     int text_pos;
     int group;
     int text_width;
     SDL_Color label_color;
     SDL_Color text_color;
};

#define IH_TEXTWIDTH_GROUP1 100
#define IH_TEXTWIDTH_GROUP2 100
#define IH_TEXTWIDTH_GROUP3 100
#define IH_TEXTWIDTH_GROUP4 100
#define IH_TEXTWIDTH_GROUP5 40
#define IH_TEXTWIDTH_GROUP6 70
#define IH_TEXTWIDTH_GROUP7 70
#define IH_TEXTWIDTH_GROUP8 80

#define IH_CHARINFO_GROUPS 8

static struct CharInfoPos ci_pos[IH_OV_CHARACTER_END] =
{
     { IH_OV_CHARACTER_NAME, IH_TEXT_CHARINFO_NAME, 0, 0, IH_POSITION_TYPE_PIXEL_LEFT, 1, IH_TEXTWIDTH_GROUP1, { 255, 255, 255 } },
     { IH_OV_CHARACTER_GENDER, IH_TEXT_CHARINFO_GENDER, 1, 0, IH_POSITION_TYPE_PIXEL_LEFT, 1, IH_TEXTWIDTH_GROUP1, { 255, 255, 255 } },
     { IH_OV_CHARACTER_RACE, IH_TEXT_CHARINFO_RACE, 2, 0, IH_POSITION_TYPE_PIXEL_LEFT, 1, IH_TEXTWIDTH_GROUP1, { 255, 255, 255 } },
     { IH_OV_CHARACTER_CLASS, IH_TEXT_CHARINFO_CLASS, 3, 0, IH_POSITION_TYPE_PIXEL_LEFT, 1, IH_TEXTWIDTH_GROUP1, { 255, 255, 255 } },
     { IH_OV_CHARACTER_TITLE, IH_TEXT_CHARINFO_TITLE, 4, 0, IH_POSITION_TYPE_PIXEL_LEFT, 1, IH_TEXTWIDTH_GROUP1, { 255, 255, 255 } },
     { IH_OV_CHARACTER_HP, IH_TEXT_CHARINFO_HP, 5, 0, IH_POSITION_TYPE_PIXEL_LEFT, 1, IH_TEXTWIDTH_GROUP1, { 255, 255, 255 } },
     { IH_OV_CHARACTER_SP, IH_TEXT_CHARINFO_SP, 6, 0, IH_POSITION_TYPE_PIXEL_LEFT, 1, IH_TEXTWIDTH_GROUP1, { 255, 255, 255 } },
     
     { IH_OV_CHARACTER_LEVEL, IH_TEXT_CHARINFO_LEVEL, 8, 0, IH_POSITION_TYPE_PIXEL_RIGHT, 2, IH_TEXTWIDTH_GROUP2, { 255, 255, 255 } },
     { IH_OV_CHARACTER_CUREXP, IH_TEXT_CHARINFO_CUREXP, 9, 0, IH_POSITION_TYPE_PIXEL_RIGHT, 2, IH_TEXTWIDTH_GROUP2, { 255, 255, 255 } },
     { IH_OV_CHARACTER_MAXEXP, IH_TEXT_CHARINFO_MAXEXP, 10, 0, IH_POSITION_TYPE_PIXEL_RIGHT, 2, IH_TEXTWIDTH_GROUP2, { 255, 255, 255 } },
     { IH_OV_CHARACTER_ADVEXP, IH_TEXT_CHARINFO_ADVEXP, 11, 0, IH_POSITION_TYPE_PIXEL_RIGHT, 2, IH_TEXTWIDTH_GROUP2, { 255, 255, 255 } },
     
     { IH_OV_CHARACTER_GOLD, IH_TEXT_CHARINFO_GOLD, 13, 0, IH_POSITION_TYPE_PIXEL_RIGHT, 3, IH_TEXTWIDTH_GROUP4, { 255, 255, 255 } },
     
     { IH_OV_CHARACTER_BURDEN, IH_TEXT_CHARINFO_BURDEN, 15, 0, IH_POSITION_TYPE_PIXEL_RIGHT, 4, IH_TEXTWIDTH_GROUP4, { 255, 255, 255 } },
     
     { IH_OV_CHARACTER_AGE, IH_TEXT_CHARINFO_AGE, 1, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 5, IH_TEXTWIDTH_GROUP5, { 255, 255, 255 } },
     { IH_OV_CHARACTER_HEIGHT, IH_TEXT_CHARINFO_HEIGHT, 2, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 5, IH_TEXTWIDTH_GROUP5, { 255, 255, 255 } },
     { IH_OV_CHARACTER_WEIGHT, IH_TEXT_CHARINFO_WEIGHT, 3, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 5, IH_TEXTWIDTH_GROUP5, { 255, 255, 255 } },
     { IH_OV_CHARACTER_STATUS, IH_TEXT_CHARINFO_STATUS, 4, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 5, IH_TEXTWIDTH_GROUP5, { 255, 255, 255 } },
     { IH_OV_CHARACTER_MAXIMIZE, IH_TEXT_CHARINFO_MAXIMIZE, 5, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 5, IH_TEXTWIDTH_GROUP5, { 255, 255, 255 } },
     { IH_OV_CHARACTER_PRESERVE, IH_TEXT_CHARINFO_PRESERVE, 6, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 5, IH_TEXTWIDTH_GROUP5, { 255, 255, 255 } },
     
     { IH_OV_CHARACTER_ARMOR, IH_TEXT_CHARINFO_ARMOR, 8, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 6, IH_TEXTWIDTH_GROUP6, { 255, 255, 255 } },
     { IH_OV_CHARACTER_FIGHT, IH_TEXT_CHARINFO_FIGHT, 9, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 6, IH_TEXTWIDTH_GROUP6, { 255, 255, 255 } },
     { IH_OV_CHARACTER_MELEE, IH_TEXT_CHARINFO_MELEE, 10, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 6, IH_TEXTWIDTH_GROUP6, { 255, 255, 255 } },
     { IH_OV_CHARACTER_SHOOT, IH_TEXT_CHARINFO_SHOOT, 11, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 6, IH_TEXTWIDTH_GROUP6, { 255, 255, 255 } },
     { IH_OV_CHARACTER_BLOWS, IH_TEXT_CHARINFO_BLOWS, 12, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 6, IH_TEXTWIDTH_GROUP6, { 255, 255, 255 } },
     { IH_OV_CHARACTER_SHOTS, IH_TEXT_CHARINFO_SHOTS, 13, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 6, IH_TEXTWIDTH_GROUP6, { 255, 255, 255 } },
     
     { IH_OV_CHARACTER_INFRA, IH_TEXT_CHARINFO_INFRA, 15, 1, IH_POSITION_TYPE_PIXEL_RIGHT, 7, IH_TEXTWIDTH_GROUP7, { 255, 255, 255 } },
     
     { IH_OV_CHARACTER_SAVINGTHROW, IH_TEXT_CHARINFO_SAVINGTHROW, 8, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },
     { IH_OV_CHARACTER_STEALTH, IH_TEXT_CHARINFO_STEALTH, 9, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },
     { IH_OV_CHARACTER_FIGHTING, IH_TEXT_CHARINFO_FIGHTING, 10, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },
     { IH_OV_CHARACTER_SHOOTING, IH_TEXT_CHARINFO_SHOOTING, 11, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },
     { IH_OV_CHARACTER_DISARMING, IH_TEXT_CHARINFO_DISARMING, 12, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },
     { IH_OV_CHARACTER_MAGICDEVICE, IH_TEXT_CHARINFO_MAGICDEVICE, 13, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },
     { IH_OV_CHARACTER_PERCEPTION, IH_TEXT_CHARINFO_PERCEPTION, 14, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },
     { IH_OV_CHARACTER_SEARCHING, IH_TEXT_CHARINFO_SEARCHING, 15, 2, IH_POSITION_TYPE_PIXEL_RIGHT, 8, IH_TEXTWIDTH_GROUP8, { 255, 255, 255 } },

     { IH_OV_CHARACTER_END, }
};

static void IH_RenderOverlay_Character(Overlay *overlay)
{
     SDL_Color color;
     SDL_Rect rect;
     Uint32 color_val;
     ihFontPos pos;
     int widest = 0, i;

     if(!overlay)
          return;
     
     color_val = SDL_MapRGBA(ih.screen->format, 200, 200, 200, 100);
     Draw_Round(ih.screen,
                overlay->position.x, overlay->position.y,
                overlay->surface->w + 6, overlay->surface->h + 6,
                5, color_val);

     rect.x = overlay->position.x + 3;
     rect.y = overlay->position.y + 3;
     SDL_BlitSurface(overlay->surface, NULL,
                     ih.screen, &rect);



     /* Ability scores.
      */
     
#if 0
     for(i = 0;
         i < IH_CHARINFO_GROUPS;
         i++)
     {
          int j;

          for(j = 0;
              j < IH_OV_CHARACTER_END;
              j++)
          {
               int col_width, label_width;
               
               if(ci_pos[j].group != i)
                    continue;

               col_width = 0;
               if(ci_pos[j].column > 0)
                    col_width = column_x[ci_pos[j].column - 1];

               pos.x.type = IH_POSITION_TYPE_PIXEL;
               pos.x.pixel = overlay->position.x + 5 + col_width + (ci_pos[j].column * 5);
               pos.y.type = IH_POSITION_TYPE_PIXEL;
               pos.y.pixel = overlay->position.y + 5 + (ci_pos[j].line * IH_FONT_NORMAL_SIZE);
               color.r = color.g = color.b = 255;
               IH_RenderText(IH_FONT_NORMAL,
                             ci_pos[j].text,
                             &pos,
                             ci_pos[j].label_color,
                             &rect);

               label_width = rect.w;
               if(label_width + 5 + ci_pos[j].text_width > column_x[ci_pos[j].column])
                    column_x[ci_pos[j].column] = label_width + 5 + ci_pos[j].text_width;
          }

          for(j = 0;
              j < IH_OV_CHARACTER_END;
              j++)
          {
               if(ci_pos[j].group != i)
                    continue;
          }
     }
#endif
}

static void IH_RenderOverlay_Inventory(Overlay *overlay)
{

}

static void IH_RenderOverlay_Equipment(Overlay *overlay)
{

}

static void IH_RenderOverlay_Book(Overlay *overlay)
{

}

static void IH_RenderOverlay_Messages(Overlay *overlay)
{

}

void IH_RenderOverlays(void)
{
     int overlay_num;

     for(overlay_num = IH_OVERLAY_CHARACTER;
         overlay_num < IH_OVERLAY_END;
         overlay_num++)
     {
          Overlay *overlay;

          overlay = &overlays[overlay_num];
          
          if(!overlay->shown)
               continue;

          switch(overlay->type)
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
          }
     }
}

bool IH_ActivateOverlay(int overlay_num)
{
     Overlay *overlay = NULL;

     if(overlay_num >= IH_OVERLAY_END)
          return FALSE;

     if(overlays[overlay_num].shown)
          return TRUE;

     return IH_InitOverlay(overlay_num);
}

void IH_DeactivateOverlay(int overlay_num)
{
     if(overlay_num >= IH_OVERLAY_END)
          return;

     overlays[overlay_num].shown = FALSE;
}

bool IH_InitOverlay(int overlay_num)
{
     Overlay *overlay;
     Uint32 rmask, gmask, bmask, amask;

     if(overlay_num >= IH_OVERLAY_END)
          return FALSE;

     overlay = &overlays[overlay_num];
     
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
     
     switch(overlay->type)
     {
          case IH_OVERLAY_CHARACTER:
               overlay->position.x = IH_GetIconSize(IH_ICON_SIZE_CURRENT) + 2;
               overlay->position.y = IH_GetIconSize(IH_ICON_SIZE_CURRENT);
               overlay->position.w = 400;
               overlay->position.h = 300;

               break;
               
          case IH_OVERLAY_INVENTORY:
               break;
               
          case IH_OVERLAY_EQUIPMENT:
               break;
               
          case IH_OVERLAY_BOOK:
               break;
               
          case IH_OVERLAY_MESSAGES:
               break;
     }

     if(!overlay->surface)
     {
          cptr path_data;
          cptr file;

          path_data = IH_GetDataDir("gfx");
          if(path_data)
          {
               file = IH_PathBuild(path_data, "screen", "character-info." IH_IMAGE_FORMAT_EXT, NULL);
               if(file)
               {
                    overlay->surface = IMG_Load_RW(SDL_RWFromFile(file, "rb"), 1);

                    rnfree(file);
               }

               rnfree(path_data);
          }
     }
     
     if(!overlay->surface)
          return FALSE;

     overlay->shown = TRUE;
     
     return TRUE;
}
