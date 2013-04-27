/* File: create.c */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"

#include "angband.h"

#include "ironhells.h"
#include "sdl/render/misc.h"
#include "ipc.h"
#include "sdl/scene/newchar.h"
#include "sdl/render/misc.h"
#include "sdl/render/text.h"
#include "sdl/render/overlay.h"
#include "sdl/scene.h"
#include "sdl/strings.h"

static struct
{
#if 0
     bool commence;
     int next_scene;
#endif

#if 0
     int selected_gender;
     int selected_race;
     int selected_class;
     int strength, intelligence, wisdom, dexterity, constitution, charisma;
#endif
     bool has_stats;
     int stats_buttons;

     bool init_overlay;
     
     int hilite_gender[MAX_SEXES];
     int hilite_race[MAX_RACES];
     int hilite_class[MAX_CLASSES];
     int hilite_stats[6];
     int stats_button_type[6];
     int hilite_back;
     int hilite_next;
     int hilite_random;
     SDL_Rect gender_button[MAX_SEXES];
     SDL_Rect race_button[MAX_RACES];
     SDL_Rect class_button[MAX_CLASSES];
     SDL_Rect stats_button[6];
     SDL_Rect back_button;
     SDL_Rect next_button;
     SDL_Rect random_button;
} scene_info;

enum
{
     IH_STATS_BUTTON_REROLL,
     IH_STATS_BUTTON_LOADPREV,

     IH_STATS_BUTTON_END
};

static void stat_str(int val, char *out_val)
{
     /* Above 18 */
     if (val > 18)
     {
          int bonus = (val - 18);

          if (bonus >= 100)
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

void IH_InitScene_NewChar(void)
{
     int i;

     memset(&scene_info, 0, sizeof(scene_info));
     
#ifdef DEBUG
     fprintf(stderr, "Initialize new character scene.\n");
#endif
#if 0
     ih.stage = IH_SCENE_NEW_CHARACTER_STAGE_GENDER;
     scene_info.commence = FALSE;
     scene_info.selected_gender = -1;
     scene_info.selected_race = -1;
     scene_info.selected_class = -1;
     scene_info.strength =
          scene_info.intelligence =
          scene_info.wisdom =
          scene_info.dexterity =
          scene_info.constitution =
          scene_info.charisma = 0;
#endif

     IH_DeactivateOverlay(IH_OVERLAY_CHARACTER);
     IH_DeactivateOverlay(IH_OVERLAY_INVENTORY);
     IH_DeactivateOverlay(IH_OVERLAY_EQUIPMENT);
     IH_DeactivateOverlay(IH_OVERLAY_BOOK);
     IH_DeactivateOverlay(IH_OVERLAY_MESSAGES);
}

static void IH_RenderScene_NewChar_Gender(void)
{
     SDL_Rect rect;
     SDL_Color color;
     ihFontPos pos;
     int n;

#ifdef DEBUG
     fprintf(stderr, "Rendering new character (gender) scene.\n");
#endif
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSEGENDER,
                   &pos,
                   color,
                   NULL);

     for(n = 0; n < MAX_SEXES; n++)
     {
          /* Analyze */
          p_ptr->psex = n;
          sp_ptr = &sex_info[p_ptr->psex];

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .65;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (n ? IH_FONT_NORMAL_SIZE : IH_FONT_LARGE_SIZE) + 5;
          IH_GetButtonColor(scene_info.hilite_gender[n], &color);
          IH_RenderText(IH_FONT_NORMAL,
                        sp_ptr->title,
                        &pos,
                        color,
                        &rect);
          memcpy(&scene_info.gender_button[n], &rect, sizeof(SDL_Rect));
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_random, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_RANDOM,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.random_button, &rect, sizeof(SDL_Rect));
     
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_BACK,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));
}

static void IH_RenderScene_NewChar_Race(void)
{
     SDL_Rect rect;
     SDL_Color color;
     ihFontPos pos;
     int n;

#ifdef DEBUG
     fprintf(stderr, "Rendering new character (race) scene.\n");
#endif
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSERACE,
                   &pos,
                   color,
                   NULL);

     for(n = 0; n < z_info->p_max; n++)
     {
          cptr str;
          
          /* Analyze */
          p_ptr->prace = n;
          rp_ptr = &p_info[p_ptr->prace];
          str = p_name + rp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .65;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (n ? IH_FONT_NORMAL_SIZE : IH_FONT_LARGE_SIZE) + 5;
          IH_GetButtonColor(scene_info.hilite_race[n], &color);
          IH_RenderText(IH_FONT_NORMAL,
                        str,
                        &pos,
                        color,
                        &rect);
          memcpy(&scene_info.race_button[n], &rect, sizeof(SDL_Rect));
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_random, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_RANDOM,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.random_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_STARTOVER,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));
}

static void IH_RenderScene_NewChar_Class(void)
{
     SDL_Rect rect;
     SDL_Color color;
     ihFontPos pos;
     int n;

#ifdef DEBUG
     fprintf(stderr, "Rendering new character (class) scene.\n");
#endif
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSECLASS,
                   &pos,
                   color,
                   NULL);

     /* Dump classes */
     for (n = 0; n < z_info->c_max; n++)
     {
          char buf[200];
          cptr mod1 = "", mod2 = "";
          cptr str;

          /* Analyze */
          p_ptr->pclass = n;
          cp_ptr = &c_info[p_ptr->pclass];
          mp_ptr = &cp_ptr->spells;
          str = c_name + cp_ptr->name;

          /* Verify legality */
          if (!(rp_ptr->choice & (1L << n)))
          {
               mod1 = "[";
               mod2 = "]";
          }

          /* Display */
          strnfmt(buf, sizeof(buf), "%s%s%s", mod1, str, mod2);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .65;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (n ? IH_FONT_NORMAL_SIZE : IH_FONT_LARGE_SIZE) + 5;
          IH_GetButtonColor(scene_info.hilite_class[n], &color);
          IH_RenderText(IH_FONT_NORMAL,
                        buf,
                        &pos,
                        color,
                        &rect);
          memcpy(&scene_info.class_button[n], &rect, sizeof(SDL_Rect));
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_random, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_RANDOM,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.random_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_STARTOVER,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));
}

static void IH_RenderScene_NewChar_Stats(void)
{
     SDL_Rect rect;
     SDL_Color color;
     ihFontPos pos;

#if 0
     fprintf(stderr, "Rendering new character (stats) scene.\n");
#endif
     
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_ROLLSTATS,
                   &pos,
                   color,
                   NULL);

     /* FIXME: need to handle auto-roller and other variations!
      */
     if(!scene_info.init_overlay)
     {
          fprintf(stderr, "Activate character overlay.\n");
          scene_info.init_overlay = IH_ActivateOverlay(IH_OVERLAY_CHARACTER);
     }

     IH_RenderOverlays();
     
     switch(ih.stage)
     {
          case IH_SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED:
               break;

          case IH_SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER:
               break;

          default:
               scene_info.stats_buttons = 2;

               scene_info.stats_button_type[0] = IH_STATS_BUTTON_REROLL;
               pos.x.type = IH_POSITION_TYPE_PERCENT;
               pos.x.perc = .65;
               pos.y.type = IH_POSITION_TYPE_PIXEL;
               pos.y.pixel += IH_FONT_LARGE_SIZE + 5;
               IH_GetButtonColor(scene_info.hilite_stats[0], &color);
               IH_RenderText(IH_FONT_NORMAL,
                             IH_TEXT_NEWCHAR_REROLL,
                             &pos,
                             color,
                             &rect);
               memcpy(&scene_info.stats_button[0], &rect, sizeof(SDL_Rect));

               scene_info.stats_button_type[1] = IH_STATS_BUTTON_LOADPREV;
               pos.x.type = IH_POSITION_TYPE_PERCENT;
               pos.x.perc = .65;
               pos.y.type = IH_POSITION_TYPE_PIXEL;
               pos.y.pixel += rect.h + 5;
               IH_GetButtonColor(scene_info.hilite_stats[1], &color);
               IH_RenderText(IH_FONT_NORMAL,
                             IH_TEXT_NEWCHAR_LOADPREV,
                             &pos,
                             color,
                             &rect);
               memcpy(&scene_info.stats_button[1], &rect, sizeof(SDL_Rect));
               break;
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_next, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_NEXT,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.next_button, &rect, sizeof(SDL_Rect));
     
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_STARTOVER,
                   &pos,
                   color,
                   &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));
}

void IH_ProcessScene_NewChar(SDL_Event *event)
{
     int i;

     if(!event)
          return;
     
#ifdef DEBUG
     fprintf(stderr, "Processing new character scene.\n");
#endif

#if 0
     if(scene_info.commence)
     {
#ifdef DEBUG
          fprintf(stderr, "Commencing scene change.\n");
#endif
          IH_SetScene(scene_info.next_scene);
          return;
     }
#endif

     if(event->type == SDL_KEYDOWN)
     {
          IH_SetIPCEvent(event);
          return;
     }
               
     scene_info.hilite_back = IH_HILITE_NORMAL;
     scene_info.hilite_random = IH_HILITE_NORMAL;

     switch(ih.stage)
     {
          case IH_SCENE_NEW_CHARACTER_STAGE_GENDER:
               for(i = 0; i < MAX_SEXES; i++)
               {
                    scene_info.hilite_gender[i] = IH_HILITE_NORMAL;
                    
                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.gender_button[i]))
                    {
                         scene_info.hilite_gender[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;
                              
                              scene_info.hilite_gender[i] = IH_HILITE_SELECT;
                              
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 'a' + i,
                                                 NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
               }
                    
                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.random_button))
                    {
                         scene_info.hilite_random = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;
                              
                              scene_info.hilite_random = IH_HILITE_SELECT;
                              
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 '*',
                                                 NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
                    
                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.back_button))
                    {
                         scene_info.hilite_back = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              scene_info.hilite_back = IH_HILITE_SELECT;
                              ih.done = TRUE;

                              // FIXME
                         }
                    }
                    break;

          case IH_SCENE_NEW_CHARACTER_STAGE_RACE:
               for(i = 0; i < MAX_RACES; i++)
               {
                    scene_info.hilite_race[i] = IH_HILITE_NORMAL;

                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.race_button[i]))
                    {
                         scene_info.hilite_race[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;
                              
                              scene_info.hilite_race[i] = IH_HILITE_SELECT;
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 'a' + i,
                                                 NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
               }

                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.random_button))
                    {
                         scene_info.hilite_random = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;
                              
                              scene_info.hilite_random = IH_HILITE_SELECT;
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 '*',
                                                 NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }

                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.back_button))
                    {
                         scene_info.hilite_back = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              scene_info.hilite_back = IH_HILITE_SELECT;
                         }
                    }
                    break;

          case IH_SCENE_NEW_CHARACTER_STAGE_CLASS:
               for(i = 0; i < MAX_CLASSES; i++)
               {
                    scene_info.hilite_class[i] = IH_HILITE_NORMAL;

                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.class_button[i]))
                    {
                         scene_info.hilite_class[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;
                              
                              scene_info.hilite_class[i] = IH_HILITE_SELECT;
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 'a' + i,
                                                 NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
               }

                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.random_button))
                    {
                         scene_info.hilite_random = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;

                              scene_info.hilite_random = IH_HILITE_SELECT;
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 '*',
                                                 NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }

                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.back_button))
                    {
                         scene_info.hilite_back = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;
                              
                              scene_info.hilite_back = IH_HILITE_SELECT;
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 's',
                                                 KMOD_SHIFT);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
                    break;

          case IH_SCENE_NEW_CHARACTER_STAGE_STATS:
               for(i = 0; i < scene_info.stats_buttons; i++)
               {
                    scene_info.hilite_next = IH_HILITE_NORMAL;
                    scene_info.hilite_stats[i] = IH_HILITE_NORMAL;

                    if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.stats_button[i]))
                    {
                         scene_info.hilite_stats[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event fevent;
                              
                              scene_info.hilite_stats[i] = IH_HILITE_SELECT;

                              switch(scene_info.stats_button_type[i])
                              {
                                   case IH_STATS_BUTTON_REROLL:
                                        IH_CreateFakeEvent(&fevent,
                                                           SDL_KEYDOWN,
                                                           'r',
                                                           NULL);
                                        IH_SetIPCEvent(&fevent);
                                        break;

                                   case IH_STATS_BUTTON_LOADPREV:
                                        IH_CreateFakeEvent(&fevent,
                                                           SDL_KEYDOWN,
                                                           'p',
                                                           NULL);
                                        IH_SetIPCEvent(&fevent);
                                        break;
                              }
                         }
                    }
               }

               if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.next_button))
               {
                    scene_info.hilite_next = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                    }
               }

               if(IH_IsPointerInRect(ih.mouse_x, ih.mouse_y, &scene_info.back_button))
               {
                    scene_info.hilite_back = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event fevent;
                         
                         scene_info.hilite_back = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN,
                                            's',
                                            KMOD_SHIFT);
                         IH_SetIPCEvent(&fevent);
                    }
               }
               break;
     }
}

void IH_RenderScene_NewChar(void)
{
     SDL_Rect rect, srect, drect;
     SDL_Color color;
	ihFontPos pos;
     char buf[15];
     
     /* Set the source and destination rendering rectangles.
      */
     srect.x = 0;
     srect.y = 0;
     srect.w = ih.background->w;
     srect.h = ih.background->h;

     drect.x = 0;
     drect.y = 0;
     drect.w = ih.display_width;
     drect.h = ih.display_height;

#ifdef DEBUG
     fprintf(stderr, "Blitting background image to screen.\n");
#endif
     SDL_BlitSurface(ih.background, &srect, ih.screen, &drect);

     switch(ih.stage)
     {
          case IH_SCENE_NEW_CHARACTER_STAGE_GENDER:
               IH_RenderScene_NewChar_Gender();
               break;

          case IH_SCENE_NEW_CHARACTER_STAGE_RACE:
               IH_RenderScene_NewChar_Race();
               break;

          case IH_SCENE_NEW_CHARACTER_STAGE_CLASS:
               IH_RenderScene_NewChar_Class();
               break;

          case IH_SCENE_NEW_CHARACTER_STAGE_STATS:
          case IH_SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED:
          case IH_SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER:
               IH_RenderScene_NewChar_Stats();
               IH_RenderOverlays(); 
               return;
     }

     if(ih.stage > IH_SCENE_NEW_CHARACTER_STAGE_GENDER)
     {
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PERCENT;
          pos.y.perc = .05;
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_GENDER,
                        &pos,
                        color,
                        &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        sp_ptr->title,
                        &pos,
                        color,
                        NULL);
     }

     if(ih.stage > IH_SCENE_NEW_CHARACTER_STAGE_RACE)
     {
          cptr str;
          
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_RACE,
                        &pos,
                        color,
                        &rect);

          str = p_name + rp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += rect.h + 2;
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        str,
                        &pos,
                        color,
                        NULL);
     }

     if(ih.stage > IH_SCENE_NEW_CHARACTER_STAGE_CLASS)
     {
          cptr str;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_CLASS,
                        &pos,
                        color,
                        &rect);

          str = c_name + cp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += rect.h + 2;
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        str,
                        &pos,
                        color,
                        NULL);
     }

#if 0
     if(ih.stage >= IH_SCENE_NEW_CHARACTER_STAGE_STATS)
     {
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_STR,
                        &pos,
                        color,
                        &rect);

          stat_str(p_ptr->stat_cur[0], buf);
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        buf,
                        &pos,
                        color,
                        &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_INT,
                        &pos,
                        color,
                        &rect);

          stat_str(p_ptr->stat_cur[1], buf);
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        buf,
                        &pos,
                        color,
                        &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_WIS,
                        &pos,
                        color,
                        &rect);

          stat_str(p_ptr->stat_cur[2], buf);
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        buf,
                        &pos,
                        color,
                        &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_DEX,
                        &pos,
                        color,
                        &rect);

          stat_str(p_ptr->stat_cur[3], buf);
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        buf,
                        &pos,
                        color,
                        &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_CON,
                        &pos,
                        color,
                        &rect);

          stat_str(p_ptr->stat_cur[4], buf);
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        buf,
                        &pos,
                        color,
                        &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_CHA,
                        &pos,
                        color,
                        &rect);

          stat_str(p_ptr->stat_cur[5], buf);
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL,
                        buf,
                        &pos,
                        color,
                        &rect);
     }
#endif
}
