
/* $Id: newchar.c,v 1.6 2003/04/20 05:20:58 cipher Exp $ */

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
#include "overlay.h"
#include "scene/newchar.h"

enum
{
     IH_BUTTON_NEWCHAR_CHOICES,
     IH_BUTTON_NEWCHAR_CHOICES_1,
     IH_BUTTON_NEWCHAR_CHOICES_2,
     IH_BUTTON_NEWCHAR_CHOICES_3,
     IH_BUTTON_NEWCHAR_CHOICES_4,
     IH_BUTTON_NEWCHAR_CHOICES_5,
     IH_BUTTON_NEWCHAR_CHOICES_6,
     IH_BUTTON_NEWCHAR_CHOICES_7,
     IH_BUTTON_NEWCHAR_CHOICES_8,
     IH_BUTTON_NEWCHAR_CHOICES_9,
     IH_BUTTON_NEWCHAR_CHOICES_10,
     IH_BUTTON_NEWCHAR_CHOICES_11,
     IH_BUTTON_NEWCHAR_CHOICES_12,
     IH_BUTTON_NEWCHAR_CHOICES_13,
     IH_BUTTON_NEWCHAR_CHOICES_14,
     IH_BUTTON_NEWCHAR_CHOICES_15,
     IH_BUTTON_NEWCHAR_CHOICES_16,
     IH_BUTTON_NEWCHAR_CHOICES_17,
     IH_BUTTON_NEWCHAR_CHOICES_18,
     IH_BUTTON_NEWCHAR_CHOICES_19,
     IH_BUTTON_NEWCHAR_BACK,
     IH_BUTTON_NEWCHAR_RANDOM,
     IH_BUTTON_NEWCHAR_NEXT,
     IH_BUTTON_NEWCHAR_OPTIONS,
     IH_BUTTON_NEWCHAR_PLAY,
     IH_BUTTON_NEWCHAR_YES,
     IH_BUTTON_NEWCHAR_NO,

     IH_BUTTON_NEWCHAR_END
};

static struct
{
     unsigned            init_overlay:1;

     SceneButton     buttons[IH_BUTTON_NEWCHAR_END];
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

static int
newchar_select_gender(SceneButton * button)
{

     return 0;
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
     ihColor         color;
     ihFontPos       pos;
     int             n, i;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSEGENDER, &pos, &color, 0, NULL);

     /* Display the buttons.
      */
     for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          /* Check if the button needs to be initialized.
           */
          if(!button->init)
          {
               int             init = FALSE;
               int             n;

               button->id = i;

               switch (i)
               {
                    case IH_BUTTON_NEWCHAR_RANDOM:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_RANDOM;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            '*', KMOD_SHIFT);

                         init = TRUE;
                         break;

#if 0
                    case IH_BUTTON_NEWCHAR_BACK:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .95;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_BACK;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'S', KMOD_SHIFT);

                         init = TRUE;
                         break;
#endif

                    case IH_BUTTON_NEWCHAR_CHOICES:
                    case IH_BUTTON_NEWCHAR_CHOICES_1:
                    case IH_BUTTON_NEWCHAR_CHOICES_2:
                    case IH_BUTTON_NEWCHAR_CHOICES_3:
                    case IH_BUTTON_NEWCHAR_CHOICES_4:
                    case IH_BUTTON_NEWCHAR_CHOICES_5:
                    case IH_BUTTON_NEWCHAR_CHOICES_6:
                    case IH_BUTTON_NEWCHAR_CHOICES_7:
                    case IH_BUTTON_NEWCHAR_CHOICES_8:
                    case IH_BUTTON_NEWCHAR_CHOICES_9:
                    case IH_BUTTON_NEWCHAR_CHOICES_10:
                    case IH_BUTTON_NEWCHAR_CHOICES_11:
                    case IH_BUTTON_NEWCHAR_CHOICES_12:
                    case IH_BUTTON_NEWCHAR_CHOICES_13:
                    case IH_BUTTON_NEWCHAR_CHOICES_14:
                    case IH_BUTTON_NEWCHAR_CHOICES_15:
                    case IH_BUTTON_NEWCHAR_CHOICES_16:
                    case IH_BUTTON_NEWCHAR_CHOICES_17:
                    case IH_BUTTON_NEWCHAR_CHOICES_18:
                    case IH_BUTTON_NEWCHAR_CHOICES_19:
                         n = i - IH_BUTTON_NEWCHAR_CHOICES;

                         if(n >= MAX_SEXES)
                              break;

                         /* Analyze */
                         sp_ptr = &sex_info[n];

                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .25 + (n * .03);

                         /* Set the text.
                          */
                         button->font_size = IH_FONT_NORMAL;
                         button->text = sp_ptr->title;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'a' + (i -
                                                   IH_BUTTON_NEWCHAR_CHOICES),
                                            NULL);

                         /* Set the activation callback.
                          */
                         button->activate_func = newchar_select_gender;

                         init = TRUE;

                         break;
               }

               /* Mark it initialized.
                */
               button->init = init;
          }

          if(!button->init)
               continue;

          /* Process any modifications to the button's visuals.
           */
          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, &button->color, 0, &button->rect);
     }
}

static void
IH_RenderScene_NewChar_Race(void)
{
     SDL_Rect        rect;
     ihColor         color;
     ihFontPos       pos;
     int             n, i;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSERACE, &pos, &color, 0, NULL);

     /* Display the buttons.
      */
     for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          /* Check if the button needs to be initialized.
           */
          if(!button->init)
          {
               char           *str;
               int             init = FALSE;
               int             n;

               button->id = i;

               switch (i)
               {
                    case IH_BUTTON_NEWCHAR_RANDOM:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_RANDOM;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            '*', KMOD_SHIFT);

                         init = TRUE;
                         break;

                    case IH_BUTTON_NEWCHAR_BACK:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .95;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_STARTOVER;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'S', KMOD_SHIFT);

                         init = TRUE;
                         break;

                    case IH_BUTTON_NEWCHAR_CHOICES:
                    case IH_BUTTON_NEWCHAR_CHOICES_1:
                    case IH_BUTTON_NEWCHAR_CHOICES_2:
                    case IH_BUTTON_NEWCHAR_CHOICES_3:
                    case IH_BUTTON_NEWCHAR_CHOICES_4:
                    case IH_BUTTON_NEWCHAR_CHOICES_5:
                    case IH_BUTTON_NEWCHAR_CHOICES_6:
                    case IH_BUTTON_NEWCHAR_CHOICES_7:
                    case IH_BUTTON_NEWCHAR_CHOICES_8:
                    case IH_BUTTON_NEWCHAR_CHOICES_9:
                    case IH_BUTTON_NEWCHAR_CHOICES_10:
                    case IH_BUTTON_NEWCHAR_CHOICES_11:
                    case IH_BUTTON_NEWCHAR_CHOICES_12:
                    case IH_BUTTON_NEWCHAR_CHOICES_13:
                    case IH_BUTTON_NEWCHAR_CHOICES_14:
                    case IH_BUTTON_NEWCHAR_CHOICES_15:
                    case IH_BUTTON_NEWCHAR_CHOICES_16:
                    case IH_BUTTON_NEWCHAR_CHOICES_17:
                    case IH_BUTTON_NEWCHAR_CHOICES_18:
                    case IH_BUTTON_NEWCHAR_CHOICES_19:
                         n = i - IH_BUTTON_NEWCHAR_CHOICES;

                         if(n >= z_info->p_max)
                              break;

                         /* Analyze */
                         rp_ptr = &p_info[n];
                         str = p_name + rp_ptr->name;

                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .25 + (n * .03);

                         /* Set the text.
                          */
                         button->font_size = IH_FONT_NORMAL;
                         button->text = str;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'a' + (i -
                                                   IH_BUTTON_NEWCHAR_CHOICES),
                                            NULL);

                         /* Set the activation callback.
                          */
                         button->activate_func = newchar_select_gender;

                         init = TRUE;

                         break;
               }

               /* Mark it initialized.
                */
               button->init = init;
          }

          if(!button->init)
               continue;

          /* Process any modifications to the button's visuals.
           */
          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, &button->color, 0, &button->rect);
     }
}

static void
IH_RenderScene_NewChar_Class(void)
{
     SDL_Rect        rect;
     ihColor         color;
     ihFontPos       pos;
     int             n, i;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE,
                   IH_TEXT_NEWCHAR_CHOOSECLASS, &pos, &color, 0, NULL);

     /* Display the buttons.
      */
     for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          /* Check if the button needs to be initialized.
           */
          if(!button->init)
          {
               char           *str;
               int             init = FALSE;
               int             n;

               button->id = i;

               switch (i)
               {
                    case IH_BUTTON_NEWCHAR_RANDOM:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_RANDOM;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            '*', KMOD_SHIFT);

                         init = TRUE;
                         break;

                    case IH_BUTTON_NEWCHAR_BACK:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .95;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_STARTOVER;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'S', KMOD_SHIFT);

                         init = TRUE;
                         break;

                    case IH_BUTTON_NEWCHAR_CHOICES:
                    case IH_BUTTON_NEWCHAR_CHOICES_1:
                    case IH_BUTTON_NEWCHAR_CHOICES_2:
                    case IH_BUTTON_NEWCHAR_CHOICES_3:
                    case IH_BUTTON_NEWCHAR_CHOICES_4:
                    case IH_BUTTON_NEWCHAR_CHOICES_5:
                    case IH_BUTTON_NEWCHAR_CHOICES_6:
                    case IH_BUTTON_NEWCHAR_CHOICES_7:
                    case IH_BUTTON_NEWCHAR_CHOICES_8:
                    case IH_BUTTON_NEWCHAR_CHOICES_9:
                    case IH_BUTTON_NEWCHAR_CHOICES_10:
                    case IH_BUTTON_NEWCHAR_CHOICES_11:
                    case IH_BUTTON_NEWCHAR_CHOICES_12:
                    case IH_BUTTON_NEWCHAR_CHOICES_13:
                    case IH_BUTTON_NEWCHAR_CHOICES_14:
                    case IH_BUTTON_NEWCHAR_CHOICES_15:
                    case IH_BUTTON_NEWCHAR_CHOICES_16:
                    case IH_BUTTON_NEWCHAR_CHOICES_17:
                    case IH_BUTTON_NEWCHAR_CHOICES_18:
                    case IH_BUTTON_NEWCHAR_CHOICES_19:
                         n = i - IH_BUTTON_NEWCHAR_CHOICES;

                         if(n >= z_info->c_max)
                              break;

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
                              strnfmt(buf, sizeof(buf), "%s%s%s", mod1,
                                      str, mod2);

                              button->pos.x.type =
                                  IH_POSITION_TYPE_PERCENT;
                              button->pos.x.perc = .65;
                              button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                              button->pos.y.perc = .25 + (n * .03);

                              /* Set the text.
                               */
                              button->font_size = IH_FONT_NORMAL;
                              button->text = buf;
                         }

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'a' + (i -
                                                   IH_BUTTON_NEWCHAR_CHOICES),
                                            NULL);

                         /* Set the activation callback.
                          */
                         button->activate_func = newchar_select_gender;

                         init = TRUE;

                         break;
               }

               /* Mark it initialized.
                */
               button->init = init;
          }

          if(!button->init)
               continue;

          /* Process any modifications to the button's visuals.
           */
          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, &button->color, 0, &button->rect);
     }
}

static void
IH_RenderScene_NewChar_Options(void)
{
     SDL_Rect        rect;
     ihColor         color;
     ihFontPos       pos;
     int             stage = 0, i;

     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          stage = ih.scene.stage;

          SDL_SemPost(ih.ipc.sem.scene);
     }

     switch (stage)
     {
          case SCENE_NEW_CHARACTER_STAGE_OPTIONS_QUERY:
               pos.x.type = IH_POSITION_TYPE_PERCENT;
               pos.x.perc = .65;
               pos.y.type = IH_POSITION_TYPE_PERCENT;
               pos.y.perc = .2;

               IH_AttrToColor(COLOR_WHITE, &color);

               IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_MODIFYOPTIONS,
                             &pos, &color, 0, &rect);

               /* Display the buttons.
                */
               for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
               {
                    SceneButton    *button;

                    button = &scene_info.buttons[i];

                    /* Check if the button needs to be initialized.
                     */
                    if(!button->init)
                    {
                         char           *str;
                         int             init = FALSE;
                         int             n;

                         button->id = i;

                         switch (i)
                         {
                              case IH_BUTTON_NEWCHAR_YES:
                                   button->pos.x.type =
                                       IH_POSITION_TYPE_PERCENT;
                                   button->pos.x.perc = .65;
                                   button->pos.y.type =
                                       IH_POSITION_TYPE_PERCENT;
                                   button->pos.y.perc = .25;

                                   IH_GetButtonColor(IH_HILITE_NORMAL,
                                                     &button->color);

                                   button->font_size = IH_FONT_NORMAL;
                                   button->text = IH_TEXT_NEWCHAR_YES;

                                   /* Create an event.
                                    */
                                   IH_CreateFakeEvent(&button->event,
                                                      SDL_KEYDOWN, 'y',
                                                      NULL);

                                   init = TRUE;
                                   break;

                              case IH_BUTTON_NEWCHAR_NO:
                                   button->pos.x.type =
                                       IH_POSITION_TYPE_PERCENT;
                                   button->pos.x.perc = .65;
                                   button->pos.y.type =
                                       IH_POSITION_TYPE_PERCENT;
                                   button->pos.y.perc = .28;

                                   IH_GetButtonColor(IH_HILITE_NORMAL,
                                                     &button->color);

                                   button->font_size = IH_FONT_NORMAL;
                                   button->text = IH_TEXT_NEWCHAR_NO;

                                   /* Create an event.
                                    */
                                   IH_CreateFakeEvent(&button->event,
                                                      SDL_KEYDOWN, 'n',
                                                      NULL);

                                   init = TRUE;
                                   break;

                              case IH_BUTTON_NEWCHAR_BACK:
                                   button->pos.x.type =
                                       IH_POSITION_TYPE_PERCENT;
                                   button->pos.x.perc = .65;
                                   button->pos.y.type =
                                       IH_POSITION_TYPE_PIXEL;
                                   button->pos.y.pixel += rect.h + 5;

                                   IH_GetButtonColor(IH_HILITE_NORMAL,
                                                     &button->color);

                                   button->font_size = IH_FONT_LARGE;
                                   button->text =
                                       IH_TEXT_NEWCHAR_STARTOVER;

                                   /* Create an event.
                                    */
                                   IH_CreateFakeEvent(&button->event,
                                                      SDL_KEYDOWN, 'S',
                                                      KMOD_SHIFT);

                                   init = TRUE;
                                   break;
                         }

                         /* Mark it initialized.
                          */
                         button->init = init;
                    }

                    if(!button->init)
                         continue;

                    /* Process any modifications to the button's visuals.
                     */
                    IH_GetButtonColor(button->hilite, &button->color);
                    if(button->selected)
                         IH_AttrToColor(COLOR_YELLOW, &button->color);

                    IH_RenderText(button->font_size,
                                  button->text,
                                  &button->pos, &button->color, 0,
                                  &button->rect);
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
     ihColor         color;
     ihFontPos       pos;
     int             stage = 0, i;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_ROLLSTATS, &pos, &color,
                   0, NULL);

     /* FIXME: need to handle auto-roller and other variations!
      */
     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          stage = ih.scene.stage;

          SDL_SemPost(ih.ipc.sem.scene);
     }

     /* Display the buttons.
      */
     for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          /* Check if the button needs to be initialized.
           */
          if(!button->init)
          {
               char           *str;
               int             init = FALSE;
               int             n;

               button->id = i;

               switch (i)
               {
                    case IH_BUTTON_NEWCHAR_CHOICES:
                    case IH_BUTTON_NEWCHAR_CHOICES_1:
                    case IH_BUTTON_NEWCHAR_CHOICES_2:
                    case IH_BUTTON_NEWCHAR_CHOICES_3:
                    case IH_BUTTON_NEWCHAR_CHOICES_4:
                    case IH_BUTTON_NEWCHAR_CHOICES_5:
                    case IH_BUTTON_NEWCHAR_CHOICES_6:
                    case IH_BUTTON_NEWCHAR_CHOICES_7:
                    case IH_BUTTON_NEWCHAR_CHOICES_8:
                    case IH_BUTTON_NEWCHAR_CHOICES_9:
                    case IH_BUTTON_NEWCHAR_CHOICES_10:
                    case IH_BUTTON_NEWCHAR_CHOICES_11:
                    case IH_BUTTON_NEWCHAR_CHOICES_12:
                    case IH_BUTTON_NEWCHAR_CHOICES_13:
                    case IH_BUTTON_NEWCHAR_CHOICES_14:
                    case IH_BUTTON_NEWCHAR_CHOICES_15:
                    case IH_BUTTON_NEWCHAR_CHOICES_16:
                    case IH_BUTTON_NEWCHAR_CHOICES_17:
                    case IH_BUTTON_NEWCHAR_CHOICES_18:
                    case IH_BUTTON_NEWCHAR_CHOICES_19:
                         switch (stage)
                         {
                              case SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED:
#if 0
                                   scene_info.stats_buttons = 2;

                                   scene_info.stats_button_type[0] =
                                       IH_BUTTON_STATS_EDIT_STR;
                                   pos.x.type = IH_POSITION_TYPE_PERCENT;
                                   pos.x.perc = .1;
                                   pos.y.type = IH_POSITION_TYPE_PERCENT;
                                   pos.y.perc = .5;
                                   IH_GetButtonColor(scene_info.
                                                     hilite_stats[0],
                                                     &color);
                                   IH_RenderText(IH_FONT_NORMAL,
                                                 IH_TEXT_NEWCHAR_STR, &pos,
                                                 &color, 0, &rect);
                                   memcpy(&scene_info.stats_button[0],
                                          &rect, sizeof(SDL_Rect));
#endif
                                   break;

#ifdef ALLOW_AUTOROLLER
                              case SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER:
                                   break;
#endif

                              case SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL:
                                   /* Do nothing */
                                   break;

                              case SCENE_NEW_CHARACTER_STAGE_STATS_APPROVE:
                                   n = i - IH_BUTTON_NEWCHAR_CHOICES;
                                   
                                   if(n >= 2)
                                        break;

                                   button->pos.x.type =
                                       IH_POSITION_TYPE_PERCENT;
                                   button->pos.x.perc = .65;
                                   button->pos.y.type =
                                       IH_POSITION_TYPE_PERCENT;
                                   button->pos.y.perc = .25 + (n * .03);

                                   IH_GetButtonColor(IH_HILITE_NORMAL,
                                                     &button->color);

                                   button->font_size = IH_FONT_NORMAL;
                                   button->text =
                                       n ? IH_TEXT_NEWCHAR_LOADPREV :
                                       IH_TEXT_NEWCHAR_REROLL;

                                   /* Create an event.
                                    */
                                   IH_CreateFakeEvent(&button->event,
                                                      SDL_KEYDOWN, 'y',
                                                      NULL);

                                   init = TRUE;
                                   break;
                         }
                         break;

                    case IH_BUTTON_NEWCHAR_NEXT:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .85;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_NEXT;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            '\n', NULL);

                         init = TRUE;
                         break;

                    case IH_BUTTON_NEWCHAR_BACK:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_STARTOVER;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'S', KMOD_SHIFT);

                         init = TRUE;
                         break;
               }

               /* Mark it initialized.
                */
               button->init = init;
          }

          if(!button->init)
               continue;

          /* Process any modifications to the button's visuals.
           */
          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, &button->color, 0, &button->rect);
     }

     /* Render any overlays that are necessary.
      */
     IH_RenderOverlays();
}

static void
IH_RenderScene_NewChar_Name(void)
{
     SDL_Rect        rect;
     ihColor         color;
     ihFontPos       pos;
     int             stage, i;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_GETNAME, &pos, &color,
                   0, NULL);

     /* Display the buttons.
      */
     for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          /* Check if the button needs to be initialized.
           */
          if(!button->init)
          {
               char           *str;
               int             init = FALSE;
               int             n;

               button->id = i;

               switch (i)
               {
                    case IH_BUTTON_NEWCHAR_NEXT:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .85;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_DONE;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            '\n', NULL);

                         init = TRUE;
                         break;

                    case IH_BUTTON_NEWCHAR_BACK:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_STARTOVER;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'S', KMOD_SHIFT);

                         init = TRUE;
                         break;
               }

               /* Mark it initialized.
                */
               button->init = init;
          }

          if(!button->init)
               continue;

          /* Process any modifications to the button's visuals.
           */
          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, &button->color, 0, &button->rect);
     }

     /* Render any overlays that are necessary.
      */
     IH_RenderOverlays();
}

static void
IH_RenderScene_NewChar_Finalize(void)
{
     SDL_Rect        rect;
     ihColor         color;
     ihFontPos       pos;
     int             i;

     pos.x.type = IH_POSITION_TYPE_PERCENT;
     pos.x.perc = .65;
     pos.y.type = IH_POSITION_TYPE_PERCENT;
     pos.y.perc = .2;

     IH_AttrToColor(COLOR_WHITE, &color);

     IH_RenderText(IH_FONT_LARGE, IH_TEXT_NEWCHAR_FINALIZE, &pos, &color,
                   0, NULL);

     /* Display the buttons.
      */
     for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          /* Check if the button needs to be initialized.
           */
          if(!button->init)
          {
               char           *str;
               int             init = FALSE;
               int             n;

               button->id = i;

               switch (i)
               {
                    case IH_BUTTON_NEWCHAR_PLAY:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .85;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_PLAY;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            '\n', NULL);

                         init = TRUE;
                         break;

                    case IH_BUTTON_NEWCHAR_BACK:
                         button->pos.x.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.x.perc = .65;
                         button->pos.y.type = IH_POSITION_TYPE_PERCENT;
                         button->pos.y.perc = .9;

                         IH_GetButtonColor(IH_HILITE_NORMAL,
                                           &button->color);

                         button->font_size = IH_FONT_LARGE;
                         button->text = IH_TEXT_NEWCHAR_STARTOVER;

                         /* Create an event.
                          */
                         IH_CreateFakeEvent(&button->event, SDL_KEYDOWN,
                                            'S', KMOD_SHIFT);

                         init = TRUE;
                         break;
               }

               /* Mark it initialized.
                */
               button->init = init;
          }

          if(!button->init)
               continue;

          /* Process any modifications to the button's visuals.
           */
          IH_GetButtonColor(button->hilite, &button->color);
          if(button->selected)
               IH_AttrToColor(COLOR_YELLOW, &button->color);

          IH_RenderText(button->font_size,
                        button->text,
                        &button->pos, &button->color, 0, &button->rect);
     }

     /* Render any overlays that are necessary.
      */
     IH_RenderOverlays();
}

void
IH_ProcessScene_NewChar(SDL_Event * event)
{
     int             i;

     if(event)
     {
          if(event->type == SDL_KEYDOWN)
          {
               IH_SetIPCEvent(event);
               return;
          }
     }

     for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
     {
          SceneButton    *button;

          button = &scene_info.buttons[i];

          button->hilite =
              button->selected ? IH_HILITE_SELECT : IH_HILITE_NORMAL;

          if(IH_IsPointerInRect
             (ih.display.mouse_x, ih.display.mouse_y, &button->rect))
          {
               /* Mark it as "hovered."
                */
               button->hilite = IH_HILITE_HOVER;

               /* Check for activation.
                */
               if(event && event->type == SDL_MOUSEBUTTONDOWN)
               {
                    button->hilite = IH_HILITE_SELECT;

                    /* Process its activation callback.
                     */
                    if(button->activate_func)
                         (*button->activate_func) (&scene_info.buttons[i]);

                    IH_SetIPCEvent(&button->event);
               }
          }
     }
#if 0
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
     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          fprintf(stderr, "IH_ProcessScene_NewChar(): get stage\n");
          stage = ih.stage;

          SDL_SemPost(ih.ipc.sem.scene);
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
#endif

     fprintf(stderr, "IH_ProcessScene_NewChar(): return\n");
}

void
IH_RenderScene_NewChar(void)
{
     SDL_Rect        rect;
     ihColor         color;
     ihFontPos       pos;
     char            buf[15];
     int             stage = 0;
     static int      last_stage = 0;

     IH_RenderBackground();

     if(!SDL_SemWait(ih.ipc.sem.scene))
     {
          stage = ih.scene.stage;

          SDL_SemPost(ih.ipc.sem.scene);
     }
     
     if(stage != last_stage)
     {
          int i;
          
          /* Stage has changed, so reset all the buttons.
           */
          for(i = 0; i < IH_BUTTON_NEWCHAR_END; i++)
          {
               scene_info.buttons[i].init = FALSE;
          }
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
     if(stage >= SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL)
          return;

     /* Display the gender if it's already been selected.
      */
     if(stage > SCENE_NEW_CHARACTER_STAGE_GENDER)
     {
          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PERCENT;
          pos.y.perc = .05;

          IH_AttrToColor(COLOR_WHITE, &color);

          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_GENDER, &pos, &color, 0, &rect);

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);

          IH_AttrToColor(COLOR_L_WHITE, &color);

          IH_RenderText(IH_FONT_NORMAL, sp_ptr->title, &pos, &color, 0,
                        NULL);
     }

     /* Display the race if it's already been selected.
      */
     if(stage > SCENE_NEW_CHARACTER_STAGE_RACE)
     {
          cptr            str;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);

          IH_AttrToColor(COLOR_WHITE, &color);

          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_RACE, &pos, &color, 0, &rect);

          str = p_name + rp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += rect.h + 2;

          IH_AttrToColor(COLOR_L_WHITE, &color);

          IH_RenderText(IH_FONT_NORMAL, str, &pos, &color, 0, NULL);
     }

     /* Display the class if it's already been selected.
      * (Does this ever get shown?)
      */
     if(stage > SCENE_NEW_CHARACTER_STAGE_CLASS)
     {
          cptr            str;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .1;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += (rect.h + 2);

          IH_AttrToColor(COLOR_WHITE, &color);

          IH_RenderText(IH_FONT_LARGE,
                        IH_TEXT_NEWCHAR_CLASS, &pos, &color, 0, &rect);

          str = c_name + cp_ptr->name;

          pos.x.type = IH_POSITION_TYPE_PERCENT;
          pos.x.perc = .15;
          pos.y.type = IH_POSITION_TYPE_PIXEL;
          pos.y.pixel += rect.h + 2;

          IH_AttrToColor(COLOR_L_WHITE, &color);

          IH_RenderText(IH_FONT_NORMAL, str, &pos, &color, 0, NULL);
     }
}

void
IH_CleanupScene_NewChar(void)
{
}
