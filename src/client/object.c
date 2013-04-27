
/* $Id: object.c,v 1.4 2003/04/21 02:31:44 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* SDL headers */

/* Internal headers */
#include "angband/angband.h"
#include "ironhells.h"
#include "object.h"
#include "scene.h"

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

void
IH_SceneButton_SetText(SceneButton * button,
                       cptr text)
{
     if(!button || !text)
          return;

}

void
IH_SceneObject_RenderText(SceneObject * object)
{
     ihFontPos       pos;
     int             text_width, text_height, img_width, img_height;

     /* All kinds of paranoia.
      */
     if(!object)
          return;
     if(!object->text.string[0])
          return;
     if(object->text.mode == IH_SCENE_OBJECT_TEXT_MODE_NONE)
          return;

     /* Do we show hover text?
      */
     if(object->text.mode == IH_SCENE_OBJECT_TEXT_MODE_HOVER &&
        !object->text.is_shown)
          return;

     /* Get text width and height.
      */
     text_width = IH_GetTextWidth(object->text.font, object->text.string);
     text_height = IH_GetFontHeight(object->text.font);

     img_width = object->image.width;
     img_height = object->image.height;

     /* Calculate position.
      */
     pos.x.type = IH_POSITION_TYPE_PIXEL;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     switch (object->text.x_align)
     {
          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_LEFT:
               pos.x.pixel = object->x - text_width - 2;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_LEFT:
               pos.x.pixel = object->x + 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_CENTER:
               pos.x.pixel = (object->x + (img_width / 2)) - (text_width / 2);
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_RIGHT:
               pos.x.pixel = (object->x + img_width) - text_width - 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_RIGHT:
               pos.x.pixel = object->x + img_width + 2;
               break;
     }

     switch (object->text.y_align)
     {
          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_TOP:
               pos.y.pixel = object->y - text_height - 2;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_TOP:
               pos.y.pixel = object->y + 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_CENTER:
               pos.y.pixel = (object->y + (img_height / 2)) - (text_height / 2);
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_INSIDE_BOTTOM:
               pos.y.pixel = (object->y + img_height) - text_height - 1;
               break;

          case IH_SCENE_OBJECT_TEXT_ALIGN_OUTSIDE_BOTTOM:
               pos.y.pixel = object->y + img_height + 2;
               break;
     }

     IH_RenderText(object->text.font,
                   object->text.string, &pos, &object->text.color, object->text.flags, NULL);
}

void
IH_SceneObject_RenderImage(SceneObject *object,
                           SDL_Rect *srect,
                           SDL_Rect *drect)
{
fprintf(stderr, "IH_SceneObject_RenderImage()\n");

fprintf(stderr, "IH_SceneObject_RenderImage(): func = %p\n", ih.display.render_icon_func);
     if(ih.display.render_icon_func)
          (*ih.display.render_icon_func)(object, srect, drect);
fprintf(stderr, "IH_SceneObject_RenderImage(): return\n");
}

void
IH_SceneObject_Update(SceneObject *object)
{
     if(!object)
          return;
          
     if(object->type != IH_SCENE_OBJECT_TYPE_ICON &&
        object->type != IH_SCENE_OBJECT_TYPE_MISC)
          return;

     /* Get rid of the old dynamically-generated text.
      */
#if 0
     if(object->text.is_dynamic)
     {
          memset(object->text.string, 0, sizeof(object->text.string));
     }
#endif
     
     /* Set some defaults.
      */
     IH_AttrToColor(COLOR_WHITE, &object->text.color);
     IH_AttrToColor(COLOR_WHITE, &object->image.tint);
     object->image.is_tinted = FALSE;

     /* Update the text.
      */
     switch(object->object)
     {
          case IH_SCENE_OBJECT_CHARACTER:
               my_strcpy(object->text.string, op_ptr->full_name, sizeof(object->text.string));
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_ARMOR_CLASS:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "[%d,%+d]", p_ptr->dis_ac, p_ptr->dis_to_a);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_CHARACTER_LEVEL:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "%d", p_ptr->lev);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_STRENGTH:
               format_stat(p_ptr->stat_use[0], object->text.string);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_INTELLIGENCE:
               format_stat(p_ptr->stat_use[1], object->text.string);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_WISDOM:
               format_stat(p_ptr->stat_use[2], object->text.string);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_DEXTERITY:
               format_stat(p_ptr->stat_use[3], object->text.string);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_CONSTITUTION:
               format_stat(p_ptr->stat_use[4], object->text.string);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_CHARISMA:
               format_stat(p_ptr->stat_use[5], object->text.string);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_GOLD:
               strnfmt(object->text.string, sizeof(object->text.string), "%ld", p_ptr->au);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_BURDEN:
               strnfmt(object->text.string, sizeof(object->text.string),
                       "%ld.%ld lbs",
                       p_ptr->total_weight / 10L, p_ptr->total_weight % 10L);
               object->text.is_shown = TRUE;
               break;
               
          case IH_SCENE_OBJECT_LIFE:
               strnfmt(object->text.string, sizeof(object->text.string), "%d/%d", p_ptr->chp, p_ptr->mhp);
               
               if(p_ptr->chp >= p_ptr->mhp)
               {
                    /* White */
                    IH_AttrToColor(COLOR_WHITE, &object->text.color);
               }
               else if(p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
               {
                    /* Yellow */
                    IH_AttrToColor(COLOR_YELLOW, &object->text.color);
               }
               else
               {
                    /* Red */
                    IH_AttrToColor(COLOR_RED, &object->text.color);
               }
               break;
               
          case IH_SCENE_OBJECT_MANA:
               if(cp_ptr->spell_book)
               {
                    strnfmt(object->text.string, sizeof(object->text.string), "%d/%d", p_ptr->csp, p_ptr->msp);
               
                    if(p_ptr->csp >= p_ptr->msp)
                    {
                         /* White */
                         IH_AttrToColor(COLOR_WHITE, &object->text.color);
                    }
                    else if(p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
                    {
                         /* Yellow */
                         IH_AttrToColor(COLOR_YELLOW, &object->text.color);
                    }
                    else
                    {
                         /* Red */
                         IH_AttrToColor(COLOR_RED, &object->text.color);
                    }
               }
               break;
               
          case IH_SCENE_OBJECT_XP:
               strnfmt(object->text.string, sizeof(object->text.string), "%ld [%ld]", (long)p_ptr->exp, (long)0 /* FIXME */);
               break;
               
          case IH_SCENE_OBJECT_INVENTORY:
               break;
               
          case IH_SCENE_OBJECT_SPELL_BOOK:
               break;
               
          case IH_SCENE_OBJECT_INVENTORY_SLOT:
               break;
               
          case IH_SCENE_OBJECT_DEPTH:
               object->text.is_shown = TRUE;
               if(!p_ptr->depth)
               {
                    strcpy(object->text.string, "Town");
               }
               else if(depth_in_feet)
               {
                    sprintf(object->text.string, "%d'", p_ptr->depth * 50);
               }
               else
               {
                    sprintf(object->text.string, "%d", p_ptr->depth);
               }
               break;
               
          case IH_SCENE_OBJECT_STUDY:
               if(p_ptr->new_spells)
               {
                    object->image.is_shown = TRUE;
                    object->text.is_shown = TRUE;
                    strnfmt(object->text.string, sizeof(object->text.string), "%d", (int)p_ptr->new_spells);
               }
               else
               {
                    object->image.is_shown = FALSE;
                    object->text.is_shown = FALSE;
               }
               break;

		case IH_SCENE_OBJECT_HUNGRY:
               {
                    int shown = TRUE;
                    
                    object->image.alpha = 1.0f;
						object->image.is_tinted = FALSE;
               
                    /* Fainting / Starving */
                    if(p_ptr->food < PY_FOOD_FAINT)
                    {
                         IH_AttrToColor(COLOR_RED, &object->image.tint);
						object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Weak", sizeof(object->text.string));
                    }
               
                    /* Weak */
                    else if(p_ptr->food < PY_FOOD_WEAK)
                    {
                         IH_AttrToColor(COLOR_ORANGE, &object->image.tint);
						object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Weak", sizeof(object->text.string));
                    }
               
                    /* Hungry */
                    else if(p_ptr->food < PY_FOOD_ALERT)
                    {
                         IH_AttrToColor(COLOR_YELLOW, &object->image.tint);
						object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Hungry", sizeof(object->text.string));
                    }
               
                    /* Normal */
                    else if(p_ptr->food < PY_FOOD_FULL)
                    {
                         shown = FALSE;
                    }
               
                    /* Full */
                    else if(p_ptr->food < PY_FOOD_MAX)
                    {
                         IH_AttrToColor(COLOR_L_GREEN, &object->image.tint);
						object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Full", sizeof(object->text.string));
                    }
               
                    /* Gorged */
                    else
                    {
                         IH_AttrToColor(COLOR_GREEN, &object->image.tint);
						object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Gorged", sizeof(object->text.string));
                    }
                    
                    object->image.is_shown = shown;
               }
			break;

		case IH_SCENE_OBJECT_GRAZE:
               {
                    int             c = p_ptr->cut;
                    
                    if(c)
                    {
                         object->image.is_shown = TRUE;
                         object->image.alpha = (float)(CLAMP(c, 0, 1001) / 1001.0f);
                    }
               
                    object->text.mode = IH_SCENE_OBJECT_TEXT_MODE_HOVER;

                    if(c > 1000)
                    {
                         my_strcpy(object->text.string, "Mortal wound", sizeof(object->text.string));
                    }
                    else if(c > 200)
                    {
                         my_strcpy(object->text.string, "Deep gash", sizeof(object->text.string));
                    }
                    else if(c > 100)
                    {
                         my_strcpy(object->text.string, "Severe cut", sizeof(object->text.string));
                    }
                    else if(c > 50)
                    {
                         my_strcpy(object->text.string, "Nasty cut", sizeof(object->text.string));
                    }
                    else if(c > 25)
                    {
                         my_strcpy(object->text.string, "Bad cut", sizeof(object->text.string));
                    }
                    else if(c > 10)
                    {
                         my_strcpy(object->text.string, "Light cut", sizeof(object->text.string));
                    }
                    else if(c)
                    {
                         my_strcpy(object->text.string, "Graze", sizeof(object->text.string));
                    }
               }
			break;

		case IH_SCENE_OBJECT_BLIND:
               object->image.is_shown = !!p_ptr->blind;
			break;

		case IH_SCENE_OBJECT_CONFUSED:
               object->image.is_shown = !!p_ptr->confused;
			break;

		case IH_SCENE_OBJECT_AFRAID:
               object->image.is_shown = !!p_ptr->afraid;
			break;

		case IH_SCENE_OBJECT_POISONED:
               object->image.is_shown = !!p_ptr->poisoned;
			break;

		case IH_SCENE_OBJECT_STUN:
               {
                    int             s = p_ptr->stun;
                    
				   object->image.is_tinted = FALSE;
                    if(s)
                    {
                         object->image.is_shown = TRUE;
                         object->image.alpha = (float)(CLAMP(s, 0, 101) / 101.0f);
                    }
               
                    if(s > 100)
                    {
                         IH_AttrToColor(COLOR_RED, &object->image.tint);
				   object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Knocked out", sizeof(object->text.string));
                    }
                    else if(s > 50)
                    {
                         IH_AttrToColor(COLOR_ORANGE, &object->image.tint);
				   object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Heavy stun", sizeof(object->text.string));
                    }
                    else if(s)
                    {
                         IH_AttrToColor(COLOR_ORANGE, &object->image.tint);
				   object->image.is_tinted = TRUE;
                         my_strcpy(object->text.string, "Stun", sizeof(object->text.string));
                    }
               }                         
			break;
     }
}

SceneObject    *
IH_SceneObject_Alloc(int type,
                    int object)
{
     SceneObject    *obj = NULL;

     obj = ralloc(sizeof(SceneObject));
     if(obj)
     {
          /* Clear out the structure.
           */
          memset(obj, 0, sizeof(SceneObject));

          /* Initialize some things.
           */
          obj->type = type;
          obj->object = object;

          obj->hilite = IH_SCENE_OBJECT_HILITE_NONE;
          obj->image.is_shown = TRUE;
     }

     return obj;
}

void
IH_SceneObject_Free(SceneObject * object)
{
     if(!object)
          return;

#ifdef DEBUG
     fprintf(stderr, "Freeing SceneObject.\n");
#endif

     rnfree(object);
}
