
/* $Id: misc.c,v 1.9 2003/04/21 02:31:44 cipher Exp $ */

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

/* Internal headers */
#include "ironhells.h"
#include "engines.h"
#include "path.h"
#include "scene.h"
#include "object.h"
#include "platform/platform.h"

struct color_table
{
     byte            attr;
     float           red;
     float           green;
     float           blue;
};

static struct color_table color_table[] = {
     /* color, r, g, b) */
     {COLOR_DARK, 0.0f, 0.0f, 0.0f}, /* 0, 0, 0 */
     {COLOR_WHITE, 1.0f, 1.0f, 1.0f}, /* 4, 4, 4 */
     {COLOR_SLATE, 0.5f, 0.5f, 0.5f}, /* 2, 2, 2 */
     {COLOR_ORANGE, 1.0f, 0.5f, 0.0f}, /* 4, 2, 0 */
     {COLOR_RED, 0.75f, 0.0f, 0.0f}, /* 3, 0, 0 */
     {COLOR_GREEN, 0.0f, 0.5f, 0.25f}, /* 0, 2, 1 */
     {COLOR_BLUE, 0.0f, 0.0f, 1.0f}, /* 0, 0, 4 */
     {COLOR_UMBER, 0.5f, 0.25f, 0.0f}, /* 2, 1, 0 */
     {COLOR_L_DARK, 0.25f, 0.25f, 0.25f}, /* 1, 1, 1 */
     {COLOR_L_WHITE, 0.75f, 0.75f, 0.75f}, /* 3, 3, 3 */
     {COLOR_VIOLET, 1.0f, 0.0f, 1.0f}, /* 4, 0, 4 */
     {COLOR_YELLOW, 1.0f, 1.0f, 0.0f}, /* 4, 4, 0 */
     {COLOR_L_RED, 1.0f, 0.0f, 0.0f}, /* 4, 0, 0 */
     {COLOR_L_GREEN, 0.0f, 1.0f, 0.0f}, /* 0, 4, 0 */
     {COLOR_L_BLUE, 0.0f, 1.0f, 1.0f}, /* 0, 4, 4 */
     {COLOR_L_UMBER, 0.75f, 0.5f, 0.25f}, /* 3, 2, 1 */
     {COLOR_END, 0.0f, 0.0f, 0.0f}
};

errr
IH_InitMisc(void)
{

     return (0);
}

void
IH_PositionMisc(void)
{
     displayData    *display_data;
     int             i, lmx_x = 0;

     display_data = (displayData *) ih.display.data;

     for(i = 0; i < display_data->misc_object_count; i++)
     {
          SceneObject    *object;

          object = display_data->misc_objects[i];
          if(!object)
               continue;

          switch (object->object)
          {
               case IH_SCENE_OBJECT_LMXDECO:
                    lmx_x = object->x =
                        (ih.display.width - object->image.width) / 2;
                    object->y = 0;
                    break;

               case IH_SCENE_OBJECT_LIFE:
                    object->x = lmx_x + 19;
                    object->y = 0;
                    break;

               case IH_SCENE_OBJECT_MANA:
                    object->x = lmx_x + 141;
                    object->y = 0;
                    break;

               case IH_SCENE_OBJECT_XP:
                    object->x = lmx_x + 263;
                    object->y = 0;
                    break;
          }
     }
}

void
IH_RenderMisc(void)
{
     displayData    *display_data;
     int             i;
     bool            shade = FALSE;

fprintf(stderr, "IH_RenderMisc()\n");

     display_data = (displayData *) ih.display.data;

fprintf(stderr, "IH_RenderMisc(): iterate over the misc object list\n");
     for(i = 0; i < display_data->misc_object_count; i++)
     {
          SceneObject    *object;
          SDL_Rect        srect, drect;
          float           perc = 1.0f;

fprintf(stderr, "IH_RenderMisc(): get the object pointer\n");
          object = display_data->misc_objects[i];
fprintf(stderr, "IH_RenderMisc(): object = %p\n", object);
          if(!object)
               continue;

fprintf(stderr, "IH_RenderMisc(): update the object\n");
          IH_SceneObject_Update(object);

fprintf(stderr, "IH_RenderMisc(): check if it's the right type\n");
          if(object->type != IH_SCENE_OBJECT_TYPE_MISC)
               continue;

fprintf(stderr, "IH_RenderMisc(): check for image data\n");
          if(object->image.bits)
          {
               /* Set up the source and destination rectangles.
                */
fprintf(stderr, "IH_RenderMisc(): setup the display of it\n");
               srect.x = 0;
               srect.y = 0;
               srect.w = object->image.width;
               srect.h = object->image.height;

               drect.x = object->x;
               drect.y = object->y;

               /* Handle life, mana, and xp bar levels.
                */
               switch (object->object)
               {
                    case IH_SCENE_OBJECT_LIFE:
fprintf(stderr, "IH_RenderMisc(): it's the life bar\n");
                         if(p_ptr->mhp)
                              perc =
                                  ((float) p_ptr->chp /
                                   (float) p_ptr->mhp);
                         else
                              perc = 0.0f;

                         shade = TRUE;

                         break;

                    case IH_SCENE_OBJECT_MANA:
fprintf(stderr, "IH_RenderMisc(): it's the mana bar\n");
                         if(p_ptr->msp)
                              perc =
                                  ((float) p_ptr->csp /
                                   (float) p_ptr->msp);
                         else
                              perc = 0.0f;

                         shade = TRUE;

                         break;

                    case IH_SCENE_OBJECT_XP:
fprintf(stderr, "IH_RenderMisc(): it's the XP bar\n");
#if 0
                         if(p_ptr->max_exp)
                              perc =
                                  ((float) p_ptr->exp /
                                   (float) p_ptr->max_exp);
                         else
                              perc = 0.0f;
#endif

                         shade = TRUE;

                         break;
               }
               srect.w *= perc;

               /* Blit the alpha surface first.
                */
               if(shade)
                    IH_ShadeArea(object->x, object->y,
                                 object->image.width,
                                 object->image.height, NULL);

               /* Display the image.
                */
fprintf(stderr, "IH_RenderMisc(): display it\n");
               IH_SceneObject_RenderImage(object, &srect, &drect);
//               SDL_BlitSurface(object->image.surface, &srect, ih.screen, &drect);
          }

fprintf(stderr, "IH_RenderMisc(): display its text\n");
          IH_SceneObject_RenderText(object);
     }
fprintf(stderr, "IH_RenderMisc(): return\n");
}

void
IH_RenderError(void)
{
     char           *p, s;
     char            buf[10][200];
     int             widest, len, width, line;
     bool            cont;

#if 0
     if(!ih.err_message)
     {
          ih.err_shown = FALSE;
          return;
     }

     s = p = ih.err_message;
     line = 0;

     do
     {
          len = 0;

          cont = FALSE;
          while(*p)
          {
               if(*p == '\n')
               {
                    cont = TRUE;
                    break;
               }

               len++;
          }

          if(len)
          {
               my_strcpy(buf[line], s, min(sizeof(buf[line]) - 1, len));
          }

          width = IH_GetTextWidth(IH_FONT_NORMAL, buf[line]);
          if(width > widest)
               widest = width;

          if(*p == '\n')
          {
               s = p + 1;
               line++;
          }

          p++;

     } while(cont && (line < 5));

#endif
}

void
IH_RenderBackground(void)
{
     if(ih.display.render_bg_func)
          (*ih.display.render_bg_func) ();
}

void
IH_RenderTitle(void)
{
     if(ih.display.render_title_func)
          (*ih.display.render_title_func) ();
}

void
IH_RenderSplash(void)
{
     if(ih.display.render_splash_func)
          (*ih.display.render_splash_func) ();
}

void
IH_FrameArea(int x,
             int y,
             int w,
             int h,
             ihColor * color)
{
     if(ih.display.frame_area_func)
          (*ih.display.frame_area_func) (x, y, w, h, color);
}

void
IH_ShadeArea(int x,
             int y,
             int w,
             int h,
             ihColor * color)
{
     if(ih.display.shade_area_func)
          (*ih.display.shade_area_func) (x, y, w, h, color);
}

void
IH_RenderImage(SDL_Surface * surface,
               SDL_Rect * srect,
               SDL_Rect * drect)
{
     if(ih.display.render_image_func)
          (*ih.display.render_image_func) (surface, srect, drect);
}

void
IH_SwapBuffers(void)
{
     if(ih.display.swap_buffer_func)
          (*ih.display.swap_buffer_func) ();
}

void
IH_AttrToColor(byte attr,
               ihColor * color)
{
     int             i;

     if(!color)
          return;

     for(i = 0; color_table[i].attr != COLOR_END; i++)
     {
          if(color_table[i].attr == attr)
          {
               color->red = color_table[i].red;
               color->green = color_table[i].green;
               color->blue = color_table[i].blue;

               return;
          }
     }
}

void
IH_GetButtonColor(int hilite,
                  ihColor * color)
{
     if(!color)
          return;

     switch (hilite)
     {
          case IH_HILITE_HOVER:
               IH_AttrToColor(COLOR_WHITE, color);
               break;

          case IH_HILITE_SELECT:
               IH_AttrToColor(COLOR_YELLOW, color);
               break;

          case IH_HILITE_NORMAL:
          default:
               IH_AttrToColor(COLOR_L_WHITE, color);
               break;
     }
}

int
IH_IsPointerInRect(int x,
                   int y,
                   SDL_Rect * rect)
{
     if(!rect)
          return FALSE;

#ifdef DEBUG
     fprintf(stderr,
             "Checking rectangle position of rect (%d, %d, %d, %d) against (%d,%d)\n",
             rect->x, rect->y, rect->w, rect->h, x, y);
#endif

     if((x >= rect->x) &&
        (x < (rect->x + rect->w)) &&
        (y >= rect->y) && (y < (rect->y + rect->h)))
          return TRUE;

#ifdef DEBUG
     fprintf(stderr, "Position is not in rectangle.\n");
#endif
     return FALSE;
}

void
IH_ClearDrawingBuffer(void)
{
     if(ih.display.clear_buffer_func)
          (*ih.display.clear_buffer_func) ();
}

void
IH_DrawLine(int x1,
            int y1,
            int x2,
            int y2,
            ihColor * color)
{
     if(ih.display.draw_line_func)
          (*ih.display.draw_line_func) (x1, y1, x2, y2, color);
}

void
IH_FillArea(SDL_Rect * rect,
            ihColor * color)
{
     if(ih.display.fill_area_func)
          (*ih.display.fill_area_func) (rect, color);
}
