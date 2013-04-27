
/* $Id: character.c,v 1.2 2003/03/18 19:17:42 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"
#include "SDL_image.h"
#include "SDL_draw.h"

#include "angband.h"
#include "ironhells.h"
#include "file.h"
#include "path.h"
#include "sdl/render/overlay.h"
#include "sdl/render/misc.h"
#include "sdl/render/text.h"
#include "sdl/strings.h"

#define IH_OVERLAY_CHARACTER_ICON_WIDTH 32

static void
format_stat(int val,
            char *out_val)
{
     /* Above 18 */
     if(val > 18)
     {
          int             bonus = (val - 18);

          if(bonus >= 100)
          {
               sprintf(out_val, "18/%03d", bonus);
          }
          else
          {
               sprintf(out_val, "18/%02d", bonus);
          }
     }

     /* From 3 to 18 */
     else
     {
          sprintf(out_val, "%2d", val);
     }
}

static void
format_stat_info(char *buf,
                 int len,
                 int max,
                 int top,
                 int use)
{
     char            abuf[10];

     /* Initialize buffer */
     buf[0] = 0;

     if(use < top)
     {
          format_stat(use, abuf);

          my_strcat(buf, "[", len);
          my_strcat(buf, abuf, len);
          my_strcat(buf, "] ", len);
     }

     format_stat(top, abuf);
     my_strcat(buf, abuf, len);

     format_stat(max, abuf);
     my_strcat(buf, " (", len);
     my_strcat(buf, abuf, len);
     my_strcat(buf, ")", len);
}

static float
likert(int x,
       int y)
{
     float           val;

     /* Paranoia */
     if(y <= 0)
          y = 1;

     /* Negative value */
     if(x < 0)
     {
          return 0;
     }

     /* Analyze the value */
     val = ((float) (x / y) / 18.0f);
     if(val > 1.0f)
          val = 1.0f;

     return val;
}

void
IH_RenderOverlay_Character(Overlay * overlay)
{
     SDL_Color       color;
     SDL_Rect        rect, shrect;
     Uint32          color_val;
     ihFontPos       pos;
     char            buf[200];
     cptr            path_data, scale_file, title;
     int             widest = 0, i;

     if(!overlay || !overlay->surface)
          return;

     path_data = IH_GetDataDir("gfx");

     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->surface->w + 6, overlay->surface->h + 6);

     color_val = SDL_MapRGB(ih.screen->format, 50, 50, 50);
     Draw_Round(ih.screen,
                overlay->position.x, overlay->position.y,
                overlay->surface->w + 6, overlay->surface->h + 6,
                5, color_val);

     rect.x = overlay->position.x + 3;
     rect.y = overlay->position.y + 3;
     SDL_BlitSurface(overlay->surface, NULL, ih.screen, &rect);

     /* Personal information - Name
      */
     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.x.pixel = overlay->position.x + 3;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel = overlay->position.y + 3;
     color.r = color.g = 255;
     color.b = 0;
     IH_RenderText(IH_FONT_NORMAL,
                   (op_ptr &&
                    op_ptr->full_name) ? op_ptr->full_name : "(No name)",
                   &pos, color, &rect);

     /* Personal information - Class/Title
      */
     /* Wizard */
     if(p_ptr->wizard)
     {
          title = "[=-WIZARD-=]";
     }

     /* Winner */
     else if(p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
     {
          title = "***WINNER***";
     }

     /* Normal */
     else
     {
          title = c_text + cp_ptr->title[(p_ptr->lev - 1) / 5];
     }

     fprintf(stderr, "rect.h = %d\n", rect.h);
     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1,
             "%s / %s", c_name + cp_ptr->name, title);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Personal information - Gender and Race
      */
     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1,
             "%s %s", sp_ptr->title, p_name + rp_ptr->name);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Ability scores
      */
     pos.x.pixel += IH_OVERLAY_CHARACTER_ICON_WIDTH + 2;
     pos.y.pixel += rect.h;
     for(i = 0; i < A_MAX; i++)
     {
#if 0
          /* Reduced */
          if(p_ptr->stat_use[i] < p_ptr->stat_top[i])
          {
               /* Use lowercase stat name */
               put_str(stat_names_reduced[i], row + i, col);
          }

          /* Normal */
          else
          {
               /* Assume uppercase stat name */
               put_str(stat_names[i], row + i, col);
          }
#endif

          /* White */
          color.r = color.g = color.b = 255;

          /* Indicate natural maximum - gold */
          if(p_ptr->stat_max[i] == 18 + 100)
          {
               color.r = 100;   // FIXME
               color.g = 50;    // FIXME
               color.b = 250;   // FIXME
#if 0
               put_str("!", row + i, col + 3);
#endif
          }
          else if(p_ptr->stat_use[i] < p_ptr->stat_top[i])
          {
               color.r = 255;
               color.g = 255;
               color.b = 0;
          }

          /* White */
          color.r = color.g = color.b = 255;

          pos.y.pixel += rect.h;
          format_stat_info(buf, sizeof(buf) - 1,
                           p_ptr->stat_max[i],
                           p_ptr->stat_top[i], p_ptr->stat_use[i]);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

          pos.y.pixel += rect.h;
          strnfmt(buf, sizeof(buf) - 1,
                  "%+d/%+d/%+d",
                  rp_ptr->r_adj[i], cp_ptr->c_adj[i], p_ptr->stat_add[i]);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);
     }

     /* Next column */
     pos.x.pixel += 102 + IH_OVERLAY_CHARACTER_ICON_WIDTH;

     /* Personal information - Age
      */
     pos.y.pixel = overlay->position.y + 3;
     color.r = color.g = 255;
     color.b = 0;
     strnfmt(buf, sizeof(buf) - 1, "%d years", p_ptr->age);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Personal information - Height and weight
      */
     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1, "%d\" / %d#", p_ptr->ht, p_ptr->wt);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Combat info - Armor
      */
     pos.y.pixel += (rect.h * 2);
     strnfmt(buf, sizeof(buf) - 1,
             "[%d,%+d]", p_ptr->dis_ac, p_ptr->dis_to_a);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Combat info - Fight/Melee
      */
     pos.y.pixel += (rect.h * 2);
     strnfmt(buf, sizeof(buf) - 1,
             "(%+d,%+d)", p_ptr->dis_to_h, p_ptr->dis_to_d);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     pos.y.pixel += rect.h;
     {
          object_type    *o_ptr;
          int             hit, dam;

          o_ptr = &inventory[INVEN_WIELD];

          /* Base skill */
          hit = p_ptr->dis_to_h;
          dam = p_ptr->dis_to_d;

          /* Apply weapon bonuses */
          if(object_known_p(o_ptr))
               hit += o_ptr->to_h;
          if(object_known_p(o_ptr))
               dam += o_ptr->to_d;

          strnfmt(buf, sizeof(buf) - 1, "(%+d,%+d)", hit, dam);
     }
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Combat info - Shoot
      */
     pos.y.pixel += rect.h;
     {
          object_type    *o_ptr;
          int             hit, dam;

          /* Range weapon */
          o_ptr = &inventory[INVEN_BOW];

          /* Base skill */
          hit = p_ptr->dis_to_h;
          dam = 0;

          /* Apply weapon bonuses */
          if(object_known_p(o_ptr))
               hit += o_ptr->to_h;
          if(object_known_p(o_ptr))
               dam += o_ptr->to_d;

          strnfmt(buf, sizeof(buf) - 1, "(%+d,%+d)", hit, dam);
     }
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Combat info - Blows/Shots
      */
     pos.y.pixel += (rect.h * 2);
     strnfmt(buf, sizeof(buf) - 1, "%d/turn", p_ptr->num_blow);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1, "%d/turn", p_ptr->num_fire);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Infravision
      */
     pos.y.pixel += (rect.h * 2);
     strnfmt(buf, sizeof(buf) - 1, "%d ft", p_ptr->see_infra * 10);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* History
      */

     /* Next column */
     pos.x.pixel += 102 + IH_OVERLAY_CHARACTER_ICON_WIDTH;

     /* Personal information - Burden
      */
     pos.y.pixel = overlay->position.y + 3;
     color.r = color.g = 255;
     color.b = 0;
     strnfmt(buf, sizeof(buf) - 1,
             "%ld.%ld lbs",
             p_ptr->total_weight / 10L, p_ptr->total_weight % 10L);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     /* Personal information - Gold
      */
     pos.y.pixel += (rect.h * 2);
     color.r = color.g = 255;
     color.b = 0;
     strnfmt(buf, sizeof(buf) - 1, "%ld", p_ptr->au);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);

     pos.x.pixel -= IH_OVERLAY_CHARACTER_ICON_WIDTH + 2;
     pos.y.pixel += (rect.h * 2) + 2;

     {
          SDL_Rect        srect, drect;
          int             tmp;
          int             xthn, xthb, xfos, xsrh;
          int             xdis, xdev, xsav, xstl;
          int             col, scale_height;
          float           perc;
          object_type    *o_ptr;

          srect.x = srect.y = 0;
          srect.h = scale_height = overlay->gfx.character.scale->h;
          drect.x = pos.x.pixel;

          /* Fighting Skill (with current weapon) */
          o_ptr = &inventory[INVEN_WIELD];
          tmp = p_ptr->to_h + o_ptr->to_h;
          xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

          /* Shooting Skill (with current bow) */
          o_ptr = &inventory[INVEN_BOW];
          tmp = p_ptr->to_h + o_ptr->to_h;
          xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

          /* Basic abilities */
          xdis = p_ptr->skill_dis;
          xdev = p_ptr->skill_dev;
          xsav = p_ptr->skill_sav;
          xstl = p_ptr->skill_stl;
          xsrh = p_ptr->skill_srh;
          xfos = p_ptr->skill_fos;

          /* Saving throw */
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_NORMAL,
                        "Saving Throw", &pos, color, &rect);

          perc = likert(xsav, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);

          /* Stealth */
          pos.y.pixel += rect.h + scale_height;
          IH_RenderText(IH_FONT_NORMAL, "Stealth", &pos, color, &rect);

          perc = likert(xstl, 1);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);

          /* Fighting */
          pos.y.pixel += rect.h + scale_height;
          IH_RenderText(IH_FONT_NORMAL, "Fighting", &pos, color, &rect);

          perc = likert(xthn, 12);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);

          /* Shooting */
          pos.y.pixel += rect.h + scale_height;
          IH_RenderText(IH_FONT_NORMAL, "Shooting", &pos, color, &rect);

          perc = likert(xthb, 12);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);

          /* Disarming */
          pos.y.pixel += rect.h + scale_height;
          IH_RenderText(IH_FONT_NORMAL, "Disarming", &pos, color, &rect);

          perc = likert(xdis, 8);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);

          /* Magic Device */
          pos.y.pixel += rect.h + scale_height;
          IH_RenderText(IH_FONT_NORMAL,
                        "Magic Device", &pos, color, &rect);

          perc = likert(xdev, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);

          /* Perception */
          pos.y.pixel += rect.h + scale_height;
          IH_RenderText(IH_FONT_NORMAL, "Perception", &pos, color, &rect);

          perc = likert(xfos, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);

          /* Searching */
          pos.y.pixel += rect.h + scale_height;
          IH_RenderText(IH_FONT_NORMAL, "Searching", &pos, color, &rect);

          perc = likert(xsrh, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          SDL_BlitSurface(overlay->gfx.character.scale, &srect,
                          ih.screen, &drect);
     }
}
