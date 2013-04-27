
/* $Id: newchar.c,v 1.6 2003/04/08 04:28:23 cipher Exp $ */

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
#include "ipc.h"
#include "strings.h"
#include "displays/iso/misc.h"
#include "displays/iso/misc.h"
#include "displays/iso/text.h"
#include "displays/iso/overlay.h"
#include "displays/iso/scene.h"
#include "displays/iso/scene/newchar.h"

static struct
{
     int             stats_buttons;

     bool            init_overlay;

     int             hilite_gender[MAX_SEXES];
     int             hilite_race[MAX_RACES];
     int             hilite_class[MAX_CLASSES];
     int             hilite_stats[6];
     int             stats_button_type[6];
     int             hilite_back;
     int             hilite_next;
     int             hilite_options;
     int             hilite_yes;
     int             hilite_no;
     int             hilite_random;
     SDL_Rect        gender_button[MAX_SEXES];
     SDL_Rect        race_button[MAX_RACES];
     SDL_Rect        class_button[MAX_CLASSES];
     SDL_Rect        stats_button[6];
     SDL_Rect        back_button;
     SDL_Rect        next_button;
     SDL_Rect        options_button;
     SDL_Rect        yes_button;
     SDL_Rect        no_button;
     SDL_Rect        random_button;

     int             num_buttons;
     SceneButton     buttons[20];
}
scene_info;

enum
{
     IH_BUTTON_GENDERS = MAX_SEXES,

     IH_BUTTON_GENDER_RANDOM,
     IH_BUTTON_GENDER_BACK,

     IH_BUTTON_GENDER_END
};

enum
{
     IH_BUTTON_RACES = MAX_RACES,

     IH_BUTTON_RACE_RANDOM,
     IH_BUTTON_RACE_BACK,

     IH_BUTTON_RACE_END
};

enum
{
     IH_BUTTON_CLASSES = MAX_CLASSES,

     IH_BUTTON_CLASS_RANDOM,
     IH_BUTTON_CLASS_BACK,

     IH_BUTTON_CLASS_END
};

enum
{
     IH_BUTTON_STATS_EDIT_STR,
     IH_BUTTON_STATS_EDIT_INT,
     IH_BUTTON_STATS_EDIT_WIS,
     IH_BUTTON_STATS_EDIT_DEX,
     IH_BUTTON_STATS_EDIT_CON,
     IH_BUTTON_STATS_EDIT_CHA,
     IH_BUTTON_STATS_INC,
     IH_BUTTON_STATS_DEC,

     IH_BUTTON_STATS_REROLL,
     IH_BUTTON_STATS_LOADPREV,

     IH_BUTTON_STATS_ACCEPT,
     IH_BUTTON_STATS_STARTOVER,

     IH_BUTTON_STATS_END
};

static void
stat_str(int val,
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
IH_InitScene_NewChar(void)
{
     memset(&scene_info, 0, sizeof(scene_info));
}

static void
IH_RenderScene_NewChar_Gender(void)
{
     SDL_Rect        rect;
     SDL_Color       color;
     ihFontPos       pos;
     int             n;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSEGENDER, &pos, color, NULL);

     for(n = 0; n < MAX_SEXES; n++)
     {
          /* Analyze */
          sp_ptr = &sex_info[n];

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .65;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel +=
              (n ? IH_FONT_NORMAL_SIZE : IH_FONT_LARGE_SIZE) + 5;
          IH_GetButtonColor(scene_info.hilite_gender[n], &color);
          IH_RenderText(IH_FONT_NORMAL, sp_ptr->title, &pos, color, &rect);
          memcpy(&scene_info.gender_button[n], &rect, sizeof(SDL_Rect));
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_random, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_RANDOM, &pos, color, &rect);
     memcpy(&scene_info.random_button, &rect, sizeof(SDL_Rect));

#if 0
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_BACK, &pos, color, &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));
#endif
}

static void
IH_RenderScene_NewChar_Race(void)
{
     SDL_Rect        rect;
     SDL_Color       color;
     ihFontPos       pos;
     int             n;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSERACE, &pos, color, NULL);

     for(n = 0; n < z_info->p_max; n++)
     {
          cptr            str;

          /* Analyze */
          rp_ptr = &p_info[n];
          str = p_name + rp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .65;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel +=
              (n ? IH_FONT_NORMAL_SIZE : IH_FONT_LARGE_SIZE) + 5;
          IH_GetButtonColor(scene_info.hilite_race[n], &color);
          IH_RenderText(IH_FONT_NORMAL, str, &pos, color, &rect);
          memcpy(&scene_info.race_button[n], &rect, sizeof(SDL_Rect));
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_random, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_RANDOM, &pos, color, &rect);
     memcpy(&scene_info.random_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_STARTOVER, &pos, color, &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));
}

static void
IH_RenderScene_NewChar_Class(void)
{
     SDL_Rect        rect;
     SDL_Color       color;
     ihFontPos       pos;
     int             n;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSECLASS, &pos, color, NULL);

     /* Dump classes */
     for(n = 0; n < z_info->c_max; n++)
     {
          char            buf[200];
          cptr            mod1 = "", mod2 = "";
          cptr            str;

          /* Analyze */
          cp_ptr = &c_info[n];
          mp_ptr = &cp_ptr->spells;
          str = c_name + cp_ptr->name;

          /* Verify legality */
          if(!(rp_ptr->choice & (1L << n)))
          {
               mod1 = "[";
               mod2 = "]";
          }

          /* Display */
          strnfmt(buf, sizeof(buf), "%s%s%s", mod1, str, mod2);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .65;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel +=
              (n ? IH_FONT_NORMAL_SIZE : IH_FONT_LARGE_SIZE) + 5;
          IH_GetButtonColor(scene_info.hilite_class[n], &color);
          IH_RenderText(IH_FONT_NORMAL, buf, &pos, color, &rect);
          memcpy(&scene_info.class_button[n], &rect, sizeof(SDL_Rect));
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_random, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_RANDOM, &pos, color, &rect);
     memcpy(&scene_info.random_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_STARTOVER, &pos, color, &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));
}

static void
IH_RenderScene_NewChar_Options(void)
{
     SDL_Rect        rect;
     SDL_Color       color;
     ihFontPos       pos;
     int             stage = 0;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_MODIFYOPTIONS, &pos,
                   color, &rect);

     if(!SDL_SemWait(ih.sem.scene))
     {
          stage = ih.stage;

          SDL_SemPost(ih.sem.scene);
     }

     switch (stage)
     {
          case SCENE_NEW_CHARACTER_STAGE_OPTIONS_QUERY:
               {
                    pos.x.type = IH_POSITION_TYPE_PERCENT;
                    pos.x.perc = .65;
                    pos.y.type = IH_POSITION_TYPE_PIXEL;
                    pos.y.pixel += rect.h;
                    color.r = color.g = color.b = 255;
                    IH_GetButtonColor(scene_info.hilite_yes, &color);
                    IH_RenderText(IH_FONT_NORMAL, IH_TEXT_NEWCHAR_YES,
                                  &pos, color, &rect);
                    memcpy(&scene_info.yes_button, &rect,
                           sizeof(SDL_Rect));

                    pos.x.type = IH_POSITION_TYPE_PERCENT;
                    pos.x.perc = .65;
                    pos.y.type = IH_POSITION_TYPE_PIXEL;
                    pos.y.pixel += rect.h;
                    color.r = color.g = color.b = 255;
                    IH_GetButtonColor(scene_info.hilite_no, &color);
                    IH_RenderText(IH_FONT_NORMAL, IH_TEXT_NEWCHAR_NO, &pos,
                                  color, &rect);
                    memcpy(&scene_info.no_button, &rect, sizeof(SDL_Rect));

                    pos.x.type = IH_POSITION_TYPE_PERCENT;
                    pos.x.perc = .65;
                    pos.y.type = IH_POSITION_TYPE_PERCENT;
                    pos.y.perc = .9;
                    IH_GetButtonColor(scene_info.hilite_back, &color);
                    IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_STARTOVER,
                                  &pos, color, &rect);
                    memcpy(&scene_info.back_button, &rect,
                           sizeof(SDL_Rect));
               }
               break;

          case SCENE_NEW_CHARACTER_STAGE_OPTIONS:
               IH_RenderOverlays();
               break;
     }
}

static void
IH_RenderScene_NewChar_Stats(void)
{
     SDL_Rect        rect;
     SDL_Color       color;
     ihFontPos       pos;
     int             stage = 0;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_ROLLSTATS, &pos, color,
                   NULL);

     /* FIXME: need to handle auto-roller and other variations!
      */
     if(!SDL_SemWait(ih.sem.scene))
     {
          stage = ih.stage;

          SDL_SemPost(ih.sem.scene);
     }

     switch (stage)
     {
          case SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED:
               scene_info.stats_buttons = 2;

               scene_info.stats_button_type[0] = IH_BUTTON_STATS_EDIT_STR;
               pos.x.type = IH_POSITION_TYPE_PERCENT;
               pos.x.perc = .1;
               pos.y.type = IH_POSITION_TYPE_PERCENT;
               pos.y.perc = .5;
               IH_GetButtonColor(scene_info.hilite_stats[0], &color);
               IH_RenderText(IH_FONT_NORMAL,
                             IH_TEXT_NEWCHAR_STR, &pos, color, &rect);
               memcpy(&scene_info.stats_button[0], &rect,
                      sizeof(SDL_Rect));
               break;

#ifdef ALLOW_AUTOROLLER
          case SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER:
               break;
#endif

          case SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL:
               /* Do nothing */
               break;

          case SCENE_NEW_CHARACTER_STAGE_STATS_APPROVE:
               scene_info.stats_buttons = 2;

               scene_info.stats_button_type[0] = IH_BUTTON_STATS_REROLL;
               pos.x.type = IH_POSITION_TYPE_PERCENT;
               pos.x.perc = .65;
               pos.y.type = IH_POSITION_TYPE_PIXEL;
               pos.y.pixel += IH_FONT_LARGE_SIZE + 5;
               IH_GetButtonColor(scene_info.hilite_stats[0], &color);
               IH_RenderText(IH_FONT_NORMAL, IH_TEXT_NEWCHAR_REROLL, &pos,
                             color, &rect);
               memcpy(&scene_info.stats_button[0], &rect,
                      sizeof(SDL_Rect));

               scene_info.stats_button_type[1] = IH_BUTTON_STATS_LOADPREV;
               pos.x.type = IH_POSITION_TYPE_PERCENT;
               pos.x.perc = .65;
               pos.y.type = IH_POSITION_TYPE_PIXEL;
               pos.y.pixel += rect.h + 5;
               IH_GetButtonColor(scene_info.hilite_stats[1], &color);
               IH_RenderText(IH_FONT_NORMAL, IH_TEXT_NEWCHAR_LOADPREV,
                             &pos, color, &rect);
               memcpy(&scene_info.stats_button[1], &rect,
                      sizeof(SDL_Rect));
               break;
     }

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_next, &color);
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_NEXT, &pos, color,
                   &rect);
     memcpy(&scene_info.next_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_STARTOVER, &pos, color,
                   &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));

     /* Render any overlays that are necessary.
      */
     IH_RenderOverlays();
}

static void
IH_RenderScene_NewChar_Name(void)
{
     SDL_Rect        rect;
     SDL_Color       color;
     ihFontPos       pos;
     int             stage;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_GETNAME, &pos, color,
                   NULL);

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_next, &color);
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_DONE, &pos, color,
                   &rect);
     memcpy(&scene_info.next_button, &rect, sizeof(SDL_Rect));

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_STARTOVER, &pos, color,
                   &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));

     /* Render any overlays that are necessary.
      */
     IH_RenderOverlays();
}

static void
IH_RenderScene_NewChar_Finalize(void)
{
     SDL_Rect        rect;
     SDL_Color       color;
     ihFontPos       pos;

     fprintf(stderr, "IH_RenderScene_NewChar_Finalize()\n");

     fprintf(stderr,
             "IH_RenderScene_NewChar_Finalize(): draw finalize text\n");
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;
     color.r = color.g = color.b = 255;
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_FINALIZE, &pos, color,
                   NULL);

     fprintf(stderr,
             "IH_RenderScene_NewChar_Finalize(): draw play button\n");
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .9;
     IH_GetButtonColor(scene_info.hilite_next, &color);
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_PLAY, &pos, color,
                   &rect);
     memcpy(&scene_info.next_button, &rect, sizeof(SDL_Rect));

     fprintf(stderr,
             "IH_RenderScene_NewChar_Finalize(): draw start over button\n");
     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PIXEL;
     pos.y.pixel += rect.h + 5;
     IH_GetButtonColor(scene_info.hilite_back, &color);
     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_STARTOVER, &pos, color,
                   &rect);
     memcpy(&scene_info.back_button, &rect, sizeof(SDL_Rect));

     /* Render any overlays that are necessary.
      */
     fprintf(stderr,
             "IH_RenderScene_NewChar_Finalize(): render overlays\n");
     IH_RenderOverlays();

     fprintf(stderr, "IH_RenderScene_NewChar_Finalize(): return\n");
}

void
IH_ProcessScene_NewChar(SDL_Event * event)
{
     int             i;
     int             stage = 0;

     fprintf(stderr, "IH_ProcessScene_NewChar()\n");

     if(!event)
          return;

     fprintf(stderr, "IH_ProcessScene_NewChar(): check for key event\n");
     if(event->type == SDL_KEYDOWN)
     {
          fprintf(stderr, "IH_ProcessScene_NewChar(): send it\n");
          IH_SetIPCEvent(event);
          fprintf(stderr, "IH_ProcessScene_NewChar(): (force) return\n");
          return;
     }

     fprintf(stderr, "IH_ProcessScene_NewChar(): normalize buttons\n");
     scene_info.hilite_yes = IH_HILITE_NORMAL;
     scene_info.hilite_no = IH_HILITE_NORMAL;
     scene_info.hilite_back = IH_HILITE_NORMAL;
     scene_info.hilite_random = IH_HILITE_NORMAL;
     scene_info.hilite_next = IH_HILITE_NORMAL;

     fprintf(stderr, "IH_ProcessScene_NewChar(): get semaphore\n");
     if(!SDL_SemWait(ih.sem.scene))
     {
          fprintf(stderr, "IH_ProcessScene_NewChar(): get stage\n");
          stage = ih.stage;

          SDL_SemPost(ih.sem.scene);
     }

     fprintf(stderr, "IH_ProcessScene_NewChar(): process stage\n");
     switch (stage)
     {
          case SCENE_NEW_CHARACTER_STAGE_GENDER:
               for(i = 0; i < MAX_SEXES; i++)
               {
                    scene_info.hilite_gender[i] = IH_HILITE_NORMAL;

                    if(IH_IsPointerInRect
                       (ih.mouse_x, ih.mouse_y,
                        &scene_info.gender_button[i]))
                    {
                         scene_info.hilite_gender[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event       fevent;

                              scene_info.hilite_gender[i] =
                                  IH_HILITE_SELECT;

                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 'a' + i, NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.random_button))
               {
                    scene_info.hilite_random = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_random = IH_HILITE_SELECT;

                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, '*', KMOD_SHIFT);
                         IH_SetIPCEvent(&fevent);
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.back_button))
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

          case SCENE_NEW_CHARACTER_STAGE_RACE:
               for(i = 0; i < MAX_RACES; i++)
               {
                    scene_info.hilite_race[i] = IH_HILITE_NORMAL;

                    if(IH_IsPointerInRect
                       (ih.mouse_x, ih.mouse_y,
                        &scene_info.race_button[i]))
                    {
                         scene_info.hilite_race[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event       fevent;

                              scene_info.hilite_race[i] = IH_HILITE_SELECT;
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 'a' + i, NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.random_button))
               {
                    scene_info.hilite_random = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_random = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, '*', NULL);
                         IH_SetIPCEvent(&fevent);
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.back_button))
               {
                    scene_info.hilite_back = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         scene_info.hilite_back = IH_HILITE_SELECT;
                    }
               }
               break;

          case SCENE_NEW_CHARACTER_STAGE_CLASS:
               for(i = 0; i < MAX_CLASSES; i++)
               {
                    scene_info.hilite_class[i] = IH_HILITE_NORMAL;

                    if(IH_IsPointerInRect
                       (ih.mouse_x, ih.mouse_y,
                        &scene_info.class_button[i]))
                    {
                         scene_info.hilite_class[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event       fevent;

                              scene_info.hilite_class[i] =
                                  IH_HILITE_SELECT;
                              IH_CreateFakeEvent(&fevent,
                                                 SDL_KEYDOWN,
                                                 'a' + i, NULL);
                              IH_SetIPCEvent(&fevent);
                         }
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.random_button))
               {
                    scene_info.hilite_random = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_random = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, '*', NULL);
                         IH_SetIPCEvent(&fevent);
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.back_button))
               {
                    scene_info.hilite_back = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_back = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, 's', KMOD_SHIFT);
                         IH_SetIPCEvent(&fevent);
                    }
               }
               break;

          case SCENE_NEW_CHARACTER_STAGE_OPTIONS_QUERY:
               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.yes_button))
               {
                    scene_info.hilite_yes = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_yes = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, 'y', NULL);
                         IH_SetIPCEvent(&fevent);
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.no_button))
               {
                    scene_info.hilite_no = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_no = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, 'n', NULL);
                         IH_SetIPCEvent(&fevent);
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.back_button))
               {
                    scene_info.hilite_back = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_back = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, 's', KMOD_SHIFT);
                         IH_SetIPCEvent(&fevent);
                    }
               }
               break;

          case SCENE_NEW_CHARACTER_STAGE_OPTIONS:
               break;

          case SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL:
               break;

          case SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED:
               break;

          case SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER:
               break;

          case SCENE_NEW_CHARACTER_STAGE_STATS_APPROVE:
               for(i = 0; i < scene_info.stats_buttons; i++)
               {
                    scene_info.hilite_next = IH_HILITE_NORMAL;
                    scene_info.hilite_stats[i] = IH_HILITE_NORMAL;

                    if(IH_IsPointerInRect
                       (ih.mouse_x, ih.mouse_y,
                        &scene_info.stats_button[i]))
                    {
                         scene_info.hilite_stats[i] = IH_HILITE_HOVER;

                         if(event->type == SDL_MOUSEBUTTONDOWN)
                         {
                              SDL_Event       fevent;

                              scene_info.hilite_stats[i] =
                                  IH_HILITE_SELECT;

                              switch (scene_info.stats_button_type[i])
                              {
                                   case IH_BUTTON_STATS_REROLL:
                                        IH_CreateFakeEvent(&fevent,
                                                           SDL_KEYDOWN,
                                                           'r', NULL);
                                        IH_SetIPCEvent(&fevent);
                                        break;

                                   case IH_BUTTON_STATS_LOADPREV:
                                        IH_CreateFakeEvent(&fevent,
                                                           SDL_KEYDOWN,
                                                           'p', NULL);
                                        IH_SetIPCEvent(&fevent);
                                        break;
                              }
                         }
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.next_button))
               {
                    scene_info.hilite_next = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_next = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, '\n', NULL);
                         IH_SetIPCEvent(&fevent);
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.back_button))
               {
                    scene_info.hilite_back = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_back = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, 's', KMOD_SHIFT);
                         IH_SetIPCEvent(&fevent);
                    }
               }
               break;

          case SCENE_NEW_CHARACTER_STAGE_NAME:
               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.next_button))
               {
                    scene_info.hilite_next = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_next = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, '\n', NULL);
                         IH_SetIPCEvent(&fevent);
                    }
               }

               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.back_button))
               {
                    scene_info.hilite_back = IH_HILITE_HOVER;

                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         scene_info.hilite_back = IH_HILITE_SELECT;
                         IH_CreateFakeEvent(&fevent,
                                            SDL_KEYDOWN, SDLK_ESCAPE,
                                            NULL);
                         IH_SetIPCEvent(&fevent);
                    }
               }
               break;

          case SCENE_NEW_CHARACTER_STAGE_FINALIZE:
               fprintf(stderr, "IH_ProcessScene_NewChar(): finalize\n");
               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.next_button))
               {
                    fprintf(stderr,
                            "IH_ProcessScene_NewChar(): mouse is over next button\n");
                    scene_info.hilite_next = IH_HILITE_HOVER;

                    fprintf(stderr,
                            "IH_ProcessScene_NewChar(): check for click\n");
                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): highlight select\n");
                         scene_info.hilite_next = IH_HILITE_SELECT;
                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): create fake key event\n");
                         IH_CreateFakeEvent(&fevent, SDL_KEYDOWN, '\n',
                                            KMOD_SHIFT);
                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): send it\n");
                         IH_SetIPCEvent(&fevent);
                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): done sending\n");
                    }
               }

               fprintf(stderr,
                       "IH_ProcessScene_NewChar(): check for mouse over back button\n");
               if(IH_IsPointerInRect
                  (ih.mouse_x, ih.mouse_y, &scene_info.back_button))
               {
                    fprintf(stderr,
                            "IH_ProcessScene_NewChar(): mouse is over back button\n");
                    scene_info.hilite_back = IH_HILITE_HOVER;

                    fprintf(stderr,
                            "IH_ProcessScene_NewChar(): check for click\n");
                    if(event->type == SDL_MOUSEBUTTONDOWN)
                    {
                         SDL_Event       fevent;

                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): select the back button\n");
                         scene_info.hilite_back = IH_HILITE_SELECT;
                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): create fake key event\n");
                         IH_CreateFakeEvent(&fevent, SDL_KEYDOWN, 's',
                                            KMOD_SHIFT);
                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): send it\n");
                         IH_SetIPCEvent(&fevent);
                         fprintf(stderr,
                                 "IH_ProcessScene_NewChar(): done sending it\n");
                    }
               }
               break;
     }
     fprintf(stderr, "IH_ProcessScene_NewChar(): return\n");
}

void
IH_RenderScene_NewChar(void)
{
     SDL_Rect        rect, srect, drect;
     SDL_Color       color;
     ihFontPos       pos;
     char            buf[15];
     int             stage = 0;

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

     SDL_BlitSurface(ih.background, &srect, ih.screen, &drect);

     if(!SDL_SemWait(ih.sem.scene))
     {
          stage = ih.stage;

          SDL_SemPost(ih.sem.scene);
     }

     switch (stage)
     {
          case SCENE_NEW_CHARACTER_STAGE_GENDER:
               IH_RenderScene_NewChar_Gender();
               break;

          case SCENE_NEW_CHARACTER_STAGE_RACE:
               IH_RenderScene_NewChar_Race();
               break;

          case SCENE_NEW_CHARACTER_STAGE_CLASS:
               IH_RenderScene_NewChar_Class();
               break;

          case SCENE_NEW_CHARACTER_STAGE_OPTIONS_QUERY:
          case SCENE_NEW_CHARACTER_STAGE_OPTIONS:
               IH_RenderScene_NewChar_Options();
               break;

          case SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL:
          case SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED:
          case SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER:
          case SCENE_NEW_CHARACTER_STAGE_STATS_APPROVE:
               IH_RenderScene_NewChar_Stats();
               break;

          case SCENE_NEW_CHARACTER_STAGE_NAME:
               IH_RenderScene_NewChar_Name();
               break;

          case SCENE_NEW_CHARACTER_STAGE_FINALIZE:
               IH_RenderScene_NewChar_Finalize();
               break;
     }

     /* Don't display the summary information if we're generating
      * stats, as it will probably be clobbered by the character
      * info overlay.
      */
     if(ih.stage >= SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL)
          return;

     /* Display the gender if it's already been selected.
      */
     if(ih.stage > SCENE_NEW_CHARACTER_STAGE_GENDER)
     {
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PERCENT;
          pos.y.perc = .05;
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_GENDER, &pos, color, &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL, sp_ptr->title, &pos, color, NULL);
     }

     /* Display the race if it's already been selected.
      */
     if(ih.stage > SCENE_NEW_CHARACTER_STAGE_RACE)
     {
          cptr            str;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_RACE, &pos, color, &rect);

          str = p_name + rp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += rect.h + 2;
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL, str, &pos, color, NULL);
     }

     /* Display the class if it's already been selected.
      * (Does this ever get shown?)
      */
     if(ih.stage > SCENE_NEW_CHARACTER_STAGE_CLASS)
     {
          cptr            str;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);
          color.r = color.g = color.b = 255;
          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_CLASS, &pos, color, &rect);

          str = c_name + cp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += rect.h + 2;
          color.r = color.g = color.b = 200;
          IH_RenderText(IH_FONT_NORMAL, str, &pos, color, NULL);
     }
}

void
IH_CleanupScene_NewChar(void)
{
}
