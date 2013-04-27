
/* $Id: book.c,v 1.3 2003/04/07 00:27:13 cipher Exp $ */

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
#include "angband/script.h"
#include "ironhells.h"
#include "strings.h"
#include "displays/iso/overlay.h"
#include "displays/iso/misc.h"
#include "displays/iso/icon.h"
#include "displays/iso/text.h"

void
IH_RenderOverlay_Book(Overlay * overlay)
{
     SDL_Color       color;
     ihFontPos       pos;
     Uint32          color_val;
     int             fhn, i, spell, num, slot;
     const magic_type *s_ptr;
     const byte     *spells;
     cptr            comment;
     char            out_val[160];
     byte            line_attr;

     /* Get some variables.
      */
     slot = overlay->var[0];
     num = overlay->var[1];
     spells = overlay->buffer[0];

     /* Calculate position based on number of items.
      */
     overlay->position.x =
         ih.display_width -
         ((slot + 1) * IH_GetIconSize(IH_ICON_SIZE_CURRENT)) -
         overlay->position.w;
     overlay->position.h =
         ((num + 1) * IH_GetFontHeight(IH_FONT_NORMAL)) +
         (IH_OVERLAY_BOOK_OFFSET_Y * 2);

     /* Do the shading and border */
     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->position.w + (IH_OVERLAY_BOOK_OFFSET_X * 2),
                  overlay->position.h + (IH_OVERLAY_BOOK_OFFSET_Y * 2));

     color_val = SDL_MapRGB(ih.screen->format, 50, 50, 50);
     Draw_Round(ih.screen,
                overlay->position.x,
                overlay->position.y,
                overlay->position.w + (IH_OVERLAY_BOOK_OFFSET_X * 2),
                overlay->position.h + (IH_OVERLAY_BOOK_OFFSET_Y * 2),
                5, color_val);

     fhn = IH_GetFontHeight(IH_FONT_NORMAL);

     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.y.type = IH_POSITION_TYPE_PIXEL;

     /* Column titles */
     pos.y.pixel = overlay->position.y + IH_OVERLAY_BOOK_OFFSET_Y;

     IH_AttrToColor(COLOR_WHITE, &color);

     pos.x.pixel =
         overlay->position.x + IH_OVERLAY_BOOK_OFFSET_X +
         IH_OVERLAY_BOOK_INDEX_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_BOOK_COLUMN_NAME, &pos,
                   color, NULL);

     pos.x.pixel +=
         IH_OVERLAY_BOOK_NAME_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_BOOK_COLUMN_LEVEL, &pos,
                   color, NULL);

     pos.x.pixel +=
         IH_OVERLAY_BOOK_LEVEL_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_BOOK_COLUMN_MANA, &pos,
                   color, NULL);

     pos.x.pixel +=
         IH_OVERLAY_BOOK_MANA_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_BOOK_COLUMN_FAIL, &pos,
                   color, NULL);

     pos.x.pixel +=
         IH_OVERLAY_BOOK_FAIL_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
     IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_BOOK_COLUMN_INFO, &pos,
                   color, NULL);

     /* Display the book */
     for(i = 0; i < num; i++)
     {
          pos.y.pixel =
              overlay->position.y + IH_OVERLAY_BOOK_OFFSET_Y +
              ((i + 1) * fhn);
          pos.x.pixel = overlay->position.x + IH_OVERLAY_BOOK_OFFSET_X;

          /* Get the spell index */
          spell = spells[i];

          /* Get the spell info */
          s_ptr = &mp_ptr->info[spell];

          IH_AttrToColor(COLOR_WHITE, &color);

          sprintf(out_val, "%c)", I2A(i));
          IH_RenderText(IH_FONT_NORMAL, out_val, &pos, color, NULL);

          pos.x.pixel +=
              (IH_OVERLAY_BOOK_COLUMN_SPACING +
               IH_OVERLAY_BOOK_INDEX_WIDTH);

          /* Skip illegible spells */
          if(s_ptr->slevel >= 99)
          {
               IH_RenderText(IH_FONT_NORMAL,
                             IH_TEXT_OVERLAY_BOOK_ILLEGIBLE, &pos, color,
                             NULL);

               continue;
          }

          /* Get extra info */
          comment = get_spell_info(cp_ptr->spell_book, spell);

          /* Assume spell is known and tried */
          line_attr = COLOR_WHITE;

          /* Analyze the spell */
          if(p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
          {
               comment = IH_TEXT_OVERLAY_BOOK_COMMENT_FORGOTTEN;
               line_attr = COLOR_YELLOW;
          }
          else if(!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))
          {
               if(s_ptr->slevel <= p_ptr->lev)
               {
                    comment = IH_TEXT_OVERLAY_BOOK_COMMENT_UNKNOWN;
                    line_attr = COLOR_L_BLUE;
               }
               else
               {
                    comment = IH_TEXT_OVERLAY_BOOK_COMMENT_DIFFICULT;
                    line_attr = COLOR_RED;
               }
          }
          else if(!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
          {
               comment = IH_TEXT_OVERLAY_BOOK_COMMENT_UNTRIED;
               line_attr = COLOR_L_GREEN;
          }

          /* Dump the spell --(-- */
          IH_AttrToColor(line_attr, &color);

          pos.x.pixel =
              overlay->position.x + IH_OVERLAY_BOOK_OFFSET_X +
              IH_OVERLAY_BOOK_INDEX_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
          IH_RenderText(IH_FONT_NORMAL,
                        get_spell_name(cp_ptr->spell_book, spell), &pos,
                        color, NULL);

          pos.x.pixel +=
              IH_OVERLAY_BOOK_NAME_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
          sprintf(out_val, "%d", s_ptr->slevel);
          IH_RenderText(IH_FONT_NORMAL, out_val, &pos, color, NULL);

          pos.x.pixel +=
              IH_OVERLAY_BOOK_LEVEL_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
          sprintf(out_val, "%d", s_ptr->smana);
          IH_RenderText(IH_FONT_NORMAL, out_val, &pos, color, NULL);

          pos.x.pixel +=
              IH_OVERLAY_BOOK_MANA_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
          sprintf(out_val, "%d%%", spell_chance(spell));
          IH_RenderText(IH_FONT_NORMAL, out_val, &pos, color, NULL);

          pos.x.pixel +=
              IH_OVERLAY_BOOK_FAIL_WIDTH + IH_OVERLAY_BOOK_COLUMN_SPACING;
          IH_RenderText(IH_FONT_NORMAL, comment, &pos, color, NULL);
     }
}

void
IH_ReleaseOverlay_Book(Overlay * overlay)
{
     if(!overlay)
          return;
}
