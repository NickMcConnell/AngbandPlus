
/* $Id: character.c,v 1.3 2003/04/18 21:45:13 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */
#include "SDL.h"
#include "SDL_image.h"
#include "SDL_draw.h"

/* Internal headers */
#include "angband/angband.h"
#include "ironhells.h"
#include "path.h"
#include "overlay.h"
#include "strings.h"
#include "platform/platform.h"

#define IH_OVERLAY_CHARACTER_ICON_WIDTH  32
#define IH_OVERLAY_CHARACTER_ICON_HEIGHT 38

#define IH_OVERLAY_CHARACTER_STATS_Y 78
#define IH_OVERLAY_CHARACTER_AGE_Y 4
#define IH_OVERLAY_CHARACTER_ARMOR_Y 56
#define IH_OVERLAY_CHARACTER_FIGHT_Y 94
#define IH_OVERLAY_CHARACTER_SHOOT_Y 134
#define IH_OVERLAY_CHARACTER_TURNS_Y 172
#define IH_OVERLAY_CHARACTER_INFRA_Y 230
#define IH_OVERLAY_CHARACTER_BURDEN_Y 4
#define IH_OVERLAY_CHARACTER_GOLD_Y 42
#define IH_OVERLAY_CHARACTER_BARS_Y 80
#define IH_OVERLAY_CHARACTER_HISTORY_Y 200

#define IH_OVERLAY_CHARACTER_BARS_SPACING_Y 2

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
     ihColor         color;
     SDL_Rect        rect, shrect;
     ihFontPos       pos;
     char            buf[200];
     cptr            path_data, scale_file, title;
     int             widest = 0, i;

     if(!overlay || !overlay->background)
          return;

     path_data = IH_GetDataDir("gfx");

     IH_ShadeArea(overlay->position.x,
                  overlay->position.y,
                  overlay->background->w +
                  (IH_OVERLAY_CHARACTER_OFFSET_X * 2),
                  overlay->background->h +
                  (IH_OVERLAY_CHARACTER_OFFSET_Y * 2), NULL);
     IH_FrameArea(overlay->position.x, overlay->position.y,
                  overlay->background->w +
                  (IH_OVERLAY_CHARACTER_OFFSET_X * 2),
                  overlay->background->h +
                  (IH_OVERLAY_CHARACTER_OFFSET_Y * 2), NULL);

     /* Draw background image.
      */
     rect.x = overlay->position.x + IH_OVERLAY_CHARACTER_OFFSET_X;
     rect.y = overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y;
     IH_RenderImage(overlay->background, NULL, &rect);

     /* Personal information - Name
      */
     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.x.pixel = overlay->position.x + IH_OVERLAY_CHARACTER_OFFSET_X;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel = overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y;
     IH_AttrToColor(COLOR_YELLOW, &color);
     IH_RenderText(IH_FONT_NORMAL,
                   (op_ptr &&
                    op_ptr->full_name) ? op_ptr->
                   full_name : IH_TEXT_OVERLAY_CHARACTER_NO_NAME, &pos,
                   &color, 0, &rect);

     /* Personal information - Class/Title
      */
     /* Wizard */
     if(p_ptr->wizard)
     {
          title = IH_TEXT_OVERLAY_CHARACTER_WIZARD;
     }

     /* Winner */
     else if(p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
     {
          title = IH_TEXT_OVERLAY_CHARACTER_WINNER;
     }

     /* Normal */
     else
     {
          title = c_text + cp_ptr->title[(p_ptr->lev - 1) / 5];
     }

     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1,
             "%s / %s", c_name + cp_ptr->name, title);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Personal information - Gender and Race
      */
     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1,
             "%s %s", sp_ptr->title, p_name + rp_ptr->name);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Ability scores
      */
     pos.x.pixel += IH_OVERLAY_CHARACTER_ICON_WIDTH + 2;
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
          IH_AttrToColor(COLOR_WHITE, &color);

          /* Indicate natural maximum - gold */
          if(p_ptr->stat_max[i] == 18 + 100)
          {
               IH_AttrToColor(COLOR_L_UMBER, &color);
#if 0
               put_str("!", row + i, col + 3);
#endif
          }
          else if(p_ptr->stat_use[i] < p_ptr->stat_top[i])
          {
               IH_AttrToColor(COLOR_YELLOW, &color);
          }

          pos.y.pixel =
              overlay->position.y + IH_OVERLAY_CHARACTER_STATS_Y +
              (i * IH_OVERLAY_CHARACTER_ICON_HEIGHT) +
              IH_OVERLAY_CHARACTER_OFFSET_Y;
          format_stat_info(buf, sizeof(buf) - 1, p_ptr->stat_max[i],
                           p_ptr->stat_top[i], p_ptr->stat_use[i]);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

          /* White */
          IH_AttrToColor(COLOR_L_WHITE, &color);

          pos.y.pixel += rect.h;
          strnfmt(buf, sizeof(buf) - 1,
                  "%+d/%+d/%+d",
                  rp_ptr->r_adj[i], cp_ptr->c_adj[i], p_ptr->stat_add[i]);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);
     }

     /* Next column */
     pos.x.pixel += 102 + IH_OVERLAY_CHARACTER_ICON_WIDTH;

     /* Personal information - Age
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_AGE_Y;
     IH_AttrToColor(COLOR_YELLOW, &color);
     strnfmt(buf, sizeof(buf) - 1, IH_TEXT_OVERLAY_CHARACTER_AGE_FMT,
             p_ptr->age);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Personal information - Height and weight
      */
     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1, "%d\" / %d#", p_ptr->ht, p_ptr->wt);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Combat info - Armor
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_ARMOR_Y;
     strnfmt(buf, sizeof(buf) - 1, "[%d,%+d]", p_ptr->dis_ac,
             p_ptr->dis_to_a);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Combat info - Fight/Melee
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_FIGHT_Y;
     strnfmt(buf, sizeof(buf) - 1, "(%+d,%+d)", p_ptr->dis_to_h,
             p_ptr->dis_to_d);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

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
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Combat info - Shoot
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_SHOOT_Y;
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
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Combat info - Blows/Shots
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_TURNS_Y;
     strnfmt(buf, sizeof(buf) - 1, IH_TEXT_OVERLAY_CHARACTER_TURNS_FMT,
             p_ptr->num_blow);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     pos.y.pixel += rect.h;
     strnfmt(buf, sizeof(buf) - 1, IH_TEXT_OVERLAY_CHARACTER_TURNS_FMT,
             p_ptr->num_fire);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Infravision
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_INFRA_Y;
     strnfmt(buf, sizeof(buf) - 1, IH_TEXT_OVERLAY_CHARACTER_INFRA_FMT,
             p_ptr->see_infra * 10);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* History
      */

     /* Next column */
     pos.x.pixel += 102 + IH_OVERLAY_CHARACTER_ICON_WIDTH;

     /* Personal information - Burden
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_BURDEN_Y;
     IH_AttrToColor(COLOR_YELLOW, &color);
     strnfmt(buf, sizeof(buf) - 1,
             IH_TEXT_OVERLAY_CHARACTER_BURDEN_FMT,
             p_ptr->total_weight / 10L, p_ptr->total_weight % 10L);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     /* Personal information - Gold
      */
     pos.y.pixel =
         overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
         IH_OVERLAY_CHARACTER_GOLD_Y;
     IH_AttrToColor(COLOR_YELLOW, &color);
     strnfmt(buf, sizeof(buf) - 1, "%ld", p_ptr->au);
     IH_RenderText(IH_FONT_NORMAL, buf, &pos, &color, 0, &rect);

     pos.x.pixel -= IH_OVERLAY_CHARACTER_ICON_WIDTH + 2;
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
          pos.y.pixel =
              overlay->position.y + IH_OVERLAY_CHARACTER_OFFSET_Y +
              IH_OVERLAY_CHARACTER_BARS_Y;
          IH_AttrToColor(COLOR_WHITE, &color);
          IH_RenderText(IH_FONT_NORMAL,
                        IH_TEXT_OVERLAY_CHARACTER_SAVING_THROW, &pos,
                        &color, 0, &rect);

          perc = likert(xsav, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);

          /* Stealth */
          pos.y.pixel +=
              rect.h + scale_height + IH_OVERLAY_CHARACTER_BARS_SPACING_Y;
          IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_CHARACTER_STEALTH,
                        &pos, &color, 0, &rect);

          perc = likert(xstl, 1);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);

          /* Fighting */
          pos.y.pixel +=
              rect.h + scale_height + IH_OVERLAY_CHARACTER_BARS_SPACING_Y;
          IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_CHARACTER_FIGHTING,
                        &pos, &color, 0, &rect);

          perc = likert(xthn, 12);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);

          /* Shooting */
          pos.y.pixel +=
              rect.h + scale_height + IH_OVERLAY_CHARACTER_BARS_SPACING_Y;
          IH_RenderText(IH_FONT_NORMAL, IH_TEXT_OVERLAY_CHARACTER_SHOOTING,
                        &pos, &color, 0, &rect);

          perc = likert(xthb, 12);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);

          /* Disarming */
          pos.y.pixel +=
              rect.h + scale_height + IH_OVERLAY_CHARACTER_BARS_SPACING_Y;
          IH_RenderText(IH_FONT_NORMAL,
                        IH_TEXT_OVERLAY_CHARACTER_DISARMING, &pos, &color,
                        0, &rect);

          perc = likert(xdis, 8);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);

          /* Magic Device */
          pos.y.pixel +=
              rect.h + scale_height + IH_OVERLAY_CHARACTER_BARS_SPACING_Y;
          IH_RenderText(IH_FONT_NORMAL,
                        IH_TEXT_OVERLAY_CHARACTER_MAGIC_DEVICE, &pos,
                        &color, 0, &rect);

          perc = likert(xdev, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);

          /* Perception */
          pos.y.pixel +=
              rect.h + scale_height + IH_OVERLAY_CHARACTER_BARS_SPACING_Y;
          IH_RenderText(IH_FONT_NORMAL,
                        IH_TEXT_OVERLAY_CHARACTER_PERCEPTION, &pos, &color,
                        0, &rect);

          perc = likert(xfos, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);

          /* Searching */
          pos.y.pixel +=
              rect.h + scale_height + IH_OVERLAY_CHARACTER_BARS_SPACING_Y;
          IH_RenderText(IH_FONT_NORMAL,
                        IH_TEXT_OVERLAY_CHARACTER_SEARCHING, &pos, &color,
                        0, &rect);

          perc = likert(xsrh, 6);
          srect.w = overlay->gfx.character.scale->w * perc;
          drect.y = rect.y + rect.h;
          IH_RenderImage(overlay->gfx.character.scale, &srect, &drect);
     }
}

void
IH_ReleaseOverlay_Character(Overlay * overlay)
{
     if(!overlay)
          return;
}
