
/* $Id: ironhells.c,v 1.6 2003/04/20 05:20:57 cipher Exp $ */

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
#include "ironhells.h"
#include "ipc.h"
#include "overlay.h"
#include "angband/angband.h"
#include "platform/platform.h"

/*
 * An array of disp_data's
 */
static disp_data data[MAX_DISP_DATA];

/*
 * Key translation structure.
 */
struct key_trans
{
     SDLKey          sdlkey;
     int             key;
};

struct key_trans shift_key_trans[] = {
     {SDLK_1, '!'},
     {SDLK_2, '@'},
     {SDLK_3, '#'},
     {SDLK_4, '$'},
     {SDLK_5, '%'},
     {SDLK_6, '^'},
     {SDLK_7, '&'},
     {SDLK_8, '*'},
     {SDLK_9, '('},
     {SDLK_0, ')'},
     {SDLK_MINUS, '_'},
     {SDLK_EQUALS, '+'},
     {SDLK_LEFTBRACKET, '{'},
     {SDLK_RIGHTBRACKET, '}'},
     {SDLK_BACKSLASH, '|'},
     {SDLK_SEMICOLON, ':'},
     {SDLK_SLASH, '?'},
     {SDLK_COMMA, '<'},
     {SDLK_PERIOD, '>'},
     {SDLK_BACKQUOTE, '~'},
     {SDLK_QUOTE, '"'},
     {0, 0}
};

static          bool
HandleKeyEvent(SDL_Event * event)
{
     int             key;

     /* Paranoia.
      */
     if(!event)
          return FALSE;

     fprintf(stderr, "HandleKeyEvent(): sym=%c (%d), mod=%x\n",
             event->key.keysym.sym, event->key.keysym.sym,
             event->key.keysym.mod);

     key = event->key.keysym.sym;

     if(event->key.keysym.mod & KMOD_SHIFT)
     {
          int             i;

          fprintf(stderr, "HandleKeyEvent(): Handling shifted key.\n");

          /* Handle uppercase letters.
           */
          if(isalpha(event->key.keysym.sym))
          {
               key = toupper(event->key.keysym.sym);
          }

          /* Handle other cases.
           */
          for(i = 0; shift_key_trans[i].sdlkey; i++)
          {
               if(event->key.keysym.sym == shift_key_trans[i].sdlkey)
               {
                    key = shift_key_trans[i].key;
                    break;
               }
          }
     }

     if(event->key.keysym.mod & KMOD_CTRL)
     {
          fprintf(stderr, "HandleKeyEvent(): Handling CTRL'd key.\n");

          if(isalpha(event->key.keysym.sym))
          {
               key = toupper(event->key.keysym.sym) - 'A' + 1;
               fprintf(stderr, "HandleKeyEvent(): key = %d\n", key);
          }
     }

     /* Handle special cases.
      */
     switch (event->key.keysym.sym)
     {
               /* Skip keystrokes where the sym is just the modifier key.
                */
          case SDLK_LSHIFT:
          case SDLK_RSHIFT:
          case SDLK_LCTRL:
          case SDLK_RCTRL:
          case SDLK_LALT:
          case SDLK_RALT:
          case SDLK_LMETA:
          case SDLK_RMETA:
          case SDLK_LSUPER:
          case SDLK_RSUPER:
          case SDLK_MODE:
               key = 0;
               break;

               /* Handle keypad.
                */
          case SDLK_KP0:
          case SDLK_KP1:
          case SDLK_KP2:
          case SDLK_KP3:
          case SDLK_KP4:
          case SDLK_KP5:
          case SDLK_KP6:
          case SDLK_KP7:
          case SDLK_KP8:
          case SDLK_KP9:
               fprintf(stderr, "HandleKeyEvent(): Handling keypad.\n");
               /* Begin special trigger */
               Disp_keypress(31);

               /* Send the "keypad" modifier */
               Disp_keypress('K');

               /* Terminate the trigger */
               Disp_keypress(13);

               /* Send the "ascii" keypress */
               key = (event->key.keysym.sym - SDLK_KP0) + '0';
               break;

               /* Handle arrow keys.
                */
          case SDLK_UP:
               key = '8';
               break;

          case SDLK_DOWN:
               key = '2';
               break;

          case SDLK_LEFT:
               key = '4';
               break;

          case SDLK_RIGHT:
               key = '6';
               break;
     }

     if(key)
     {
          fprintf(stderr,
                  "HandleKeyEvent(): Inserting keypress '%c'\n", key);
          Disp_keypress(key);

          fprintf(stderr, "HandleKeyEvent(): Returning TRUE.\n");

          return TRUE;
     }

     fprintf(stderr, "HandleKeyEvent(): Returning FALSE.\n");
     return FALSE;
}

static          bool
CheckEvents(bool wait)
{
     SDL_Event       event;
     bool            valid = FALSE;

     for(;;)
     {
          valid = IH_GetIPCEvent(&event);

          if(!valid)
          {
               /* Should we keep waiting for a keypress?
                */
               if(wait)
                    continue;

               return FALSE;
          }

          switch (event.type)
          {
                    /* We're really only interested in keydown events, since
                     * everything else will have been processed by the drawing
                     * thread.
                     */
               case SDL_KEYDOWN:
                    /* Try to save a screenshot. */
                    if(event.key.keysym.sym == SDLK_PRINT)
                    {
                         fprintf(stderr,
                                 "CheckEvents(): Handling Print Screen request.\n");
#if 0
                         if(SDL_SaveBMP(data[0].face, "newshot.bmp"))
                         {
                              plog("You fail to get the screenshot off!");
                              break;
                         }

                         for(i = 0; i < 999; ++i)
                         {
                              sprintf(buf, "%03d.bmp", i);
                              if((tmp = fopen(buf, "rb")) != NULL)
                              {
                                   fclose(tmp);
                                   continue;
                              }
                              rename("newshot.bmp", buf);
                         }
                         plog("*click*");
#endif
                         break;
                    }

                    return HandleKeyEvent(&event);
          }
     }

     return TRUE;

#if 0
     EventRecord     event;
     WindowPtr       w;
     Rect            r;
     long            newsize;
     int             ch, ck;
     int             mc, ms, mo, mx;
     int             i;
     disp_data      *td = NULL;
     unsigned long   curTicks;
     static unsigned long lastTicks = 0L;

     /* Access the clock */
     curTicks = TickCount();

     /* Hack -- Allow efficient checking for non-pending events */
     if(!wait && (curTicks < lastTicks + EVENT_TICKS))
          return (FALSE);

     /* Timestamp last check */
     lastTicks = curTicks;

     /* Let the "system" run */
     SystemTask();

     /* Get an event (or null) */
     GetNextEvent(everyEvent, &event);

     /* Hack -- Nothing is ready yet */
     if(event.what == nullEvent)
          return (FALSE);

     /* Analyze the event */
     switch (event.what)
     {
#if 0
          case activateEvt:
               {
                    w = (WindowPtr) event.message;

                    activate(w);

                    break;
               }
#endif

          case updateEvt:
               {
                    /* Extract the window */
                    w = (WindowPtr) event.message;

                    /* Find the window */
                    for(i = 0; i < MAX_DISP_DATA; i++)
                    {
                         /* Skip dead windows */
                         if(!data[i].t)
                              continue;

                         /* Notice matches */
                         if(data[i].w == w)
                              td = &data[i];
                    }

                    /* Hack XXX XXX XXX */
                    BeginUpdate(w);
                    EndUpdate(w);

                    /* Redraw the window */
                    if(td)
                         disp_data_redraw(td);

                    break;
               }

          case keyDown:
          case autoKey:
               {
                    /* Extract some modifiers */
                    mc = (event.modifiers & controlKey) ? TRUE : FALSE;
                    ms = (event.modifiers & shiftKey) ? TRUE : FALSE;
                    mo = (event.modifiers & optionKey) ? TRUE : FALSE;
                    mx = (event.modifiers & cmdKey) ? TRUE : FALSE;

                    /* Keypress: (only "valid" if ck < 96) */
                    ch = (event.message & charCodeMask) & 255;

                    /* Keycode: see table above */
                    ck = ((event.message & keyCodeMask) >> 8) & 255;

                    /* Command + "normal key" -> menu action */
                    if(mx && (ck < 64))
                    {
                         /* Hack -- Prepare the menus */
                         setup_menus();

                         /* Mega-Hack -- allow easy exit if nothing to save */
                         if(!character_generated &&
                            (ch == 'Q' || ch == 'q'))
                              ch = 'e';

                         /* Run the Menu-Handler */
                         menu(MenuKey(ch));

                         /* Turn off the menus */
                         HiliteMenu(0);

                         /* Done */
                         break;
                    }

                    /* Hide the mouse pointer */
                    ObscureCursor();

                    /* Normal key -> simple keypress */
                    if(ck < 64)
                    {
                         /* Enqueue the keypress */
                         Disp_keypress(ch);
                    }

                    /* Keypad keys -> trigger plus simple keypress */
                    else if(!mc && !ms && !mo && !mx && (ck < 96))
                    {
                         /* Hack -- "enter" is confused */
                         if(ck == 76)
                              ch = '\n';

                         /* Begin special trigger */
                         Disp_keypress(31);

                         /* Send the "keypad" modifier */
                         Disp_keypress('K');

                         /* Terminate the trigger */
                         Disp_keypress(13);

                         /* Send the "ascii" keypress */
                         Disp_keypress(ch);
                    }

                    /* Bizarre key -> encoded keypress */
                    else if(ck <= 127)
                    {
                         /* Begin special trigger */
                         Disp_keypress(31);

                         /* Send some modifier keys */
                         if(mc)
                              Disp_keypress('C');
                         if(ms)
                              Disp_keypress('S');
                         if(mo)
                              Disp_keypress('O');
                         if(mx)
                              Disp_keypress('X');

                         /* Downshift and encode the keycode */
                         Disp_keypress(I2D((ck - 64) / 10));
                         Disp_keypress(I2D((ck - 64) % 10));

                         /* Terminate the trigger */
                         Disp_keypress(13);
                    }

                    break;
               }

          case mouseDown:
               {
                    int             code;

                    /* Analyze click location */
                    code = FindWindow(event.where, &w);

                    /* Find the window */
                    for(i = 0; i < MAX_DISP_DATA; i++)
                    {
                         /* Skip dead windows */
                         if(!data[i].t)
                              continue;

                         /* Notice matches */
                         if(data[i].w == w)
                              td = &data[i];
                    }

                    /* Analyze */
                    switch (code)
                    {
                         case inMenuBar:
                              {
                                   setup_menus();
                                   menu(MenuSelect(event.where));
                                   HiliteMenu(0);
                                   break;
                              }

                         case inSysWindow:
                              {
                                   SystemClick(&event, w);
                                   break;
                              }

                         case inDrag:
                              {
                                   Point           p;

                                   WindowPtr       old_win;

                                   r = qd.screenBits.bounds;
                                   r.top += 20; /* GetMBarHeight() XXX XXX XXX */
                                   InsetRect(&r, 4, 4);
                                   DragWindow(w, event.where, &r);

                                   /* Oops */
                                   if(!td)
                                        break;

                                   /* Save */
                                   old_win = active;

                                   /* Activate */
                                   activate(td->w);

                                   /* Analyze */
                                   p.h = td->w->portRect.left;
                                   p.v = td->w->portRect.top;
                                   LocalToGlobal(&p);
                                   td->r.left = p.h;
                                   td->r.top = p.v;

                                   /* Restore */
                                   activate(old_win);

                                   /* Apply and Verify */
                                   disp_data_check_size(td);

                                   break;
                              }

                         case inGoAway:
                              {
                                   /* Oops */
                                   if(!td)
                                        break;

                                   /* Track the go-away box */
                                   if(TrackGoAway(w, event.where))
                                   {
                                        /* Not Mapped */
                                        td->mapped = FALSE;

                                        /* Not Mapped */
                                        td->t->mapped_flag = FALSE;

                                        /* Hide the window */
                                        HideWindow(td->w);
                                   }

                                   break;
                              }

                         case inGrow:
                              {
                                   int             x, y;

                                   disp           *old = Term;

                                   /* Oops */
                                   if(!td)
                                        break;

                                   /* Fake rectangle */
                                   r.left =
                                       20 * td->tile_wid + td->size_ow1;
                                   r.right =
                                       80 * td->tile_wid + td->size_ow1 +
                                       td->size_ow2 + 1;
                                   r.top = 1 * td->tile_hgt + td->size_oh1;
                                   r.bottom =
                                       24 * td->tile_hgt + td->size_oh1 +
                                       td->size_oh2 + 1;

                                   /* Grow the rectangle */
                                   newsize =
                                       GrowWindow(w, event.where, &r);

                                   /* Handle abort */
                                   if(!newsize)
                                        break;

                                   /* Extract the new size in pixels */
                                   y = HiWord(newsize) - td->size_oh1 -
                                       td->size_oh2;
                                   x = LoWord(newsize) - td->size_ow1 -
                                       td->size_ow2;

                                   /* Extract a "close" approximation */
                                   td->rows = y / td->tile_hgt;
                                   td->cols = x / td->tile_wid;

                                   /* Apply and Verify */
                                   disp_data_check_size(td);

                                   /* Activate */
                                   Disp_activate(td->t);

                                   /* Hack -- Resize the disp */
                                   Disp_resize(td->cols, td->rows);

                                   /* Resize and Redraw */
                                   disp_data_resize(td);
                                   disp_data_redraw(td);

                                   /* Restore */
                                   Disp_activate(old);

                                   break;
                              }

                         case inContent:
                              {
                                   SelectWindow(w);

                                   break;
                              }
                    }

                    break;
               }

               /* Disk Event -- From "Maarten Hazewinkel" */
          case diskEvt:
               {
                    /* check for error when mounting the disk */
                    if(HiWord(event.message) != noErr)
                    {
                         Point           p = { 120, 120 };

                         DILoad();
                         DIBadMount(p, event.message);
                         DIUnload();
                    }

                    break;
               }

               /* OS Event -- From "Maarten Hazewinkel" */
          case osEvt:
               {
                    switch ((event.message >> 24) & 0x000000FF)
                    {
                         case suspendResumeMessage:

                              /* Resuming: activate the front window */
                              if(event.message & resumeFlag)
                              {
                                   SetPort(FrontWindow());
                                   SetCursor(&qd.arrow);
                              }

                              /* Suspend: deactivate the front window */
                              else
                              {
                                   /* Nothing */
                              }

                              break;
                    }

                    break;
               }

#ifdef USE_SFL_CODE

               /* From "Steve Linberg" and "Maarten Hazewinkel" */
          case kHighLevelEvent:
               {
                    /* Process apple events */
                    if(AEProcessAppleEvent(&event) != noErr)
                    {
                         plog("Error in Apple Event Handler!");
                    }

                    /* Handle "quit_when_ready" */
                    if(quit_when_ready)
                    {
                         /* Forget */
                         quit_when_ready = FALSE;

                         /* Do the menu key */
                         menu(MenuKey('q'));

                         /* Turn off the menus */
                         HiliteMenu(0);
                    }

                    /* Handle "open_when_ready" */
                    handle_open_when_ready();

                    break;
               }

#endif

     }

#endif

     /* Something happened */
     return (TRUE);
}

static void
Disp_init_iso(disp * t)
{
#pragma unused (t)
}

static void
Disp_nuke_iso(disp * t)
{
#pragma unused (t)
     /* XXX */
}

/* Unused
 */
static          errr
Disp_user_iso(int n)
{
#pragma unused (n)
     /* Success */
     return (0);
}

/* React to changes
 */
static          errr
Disp_xtra_iso_react(void)
{

     return (0);
}

static          errr
Disp_lock_iso(void)
{
     if(!SDL_SemWait(ih.ipc.sem.map))
          return (0);

     return (-1);
}

static          errr
Disp_unlock_iso(void)
{
     SDL_SemPost(ih.ipc.sem.map);

     return (0);
}

static          errr
Disp_xtra_iso(int n,
              int v)
{
     /* Analyze */
     switch (n)
     {
               /* Play a video.
                */
          case DISP_XTRA_VIDEO:
               break;

               /* Play some music.
                */
          case DISP_XTRA_MUSIC:
               break;

               /* Make a noise */
          case DISP_XTRA_NOISE:
#if 0
               /* Make a noise */
               SysBeep(1);
#endif
               /* Success */
               return (0);

               break;

               /* Make a sound */
          case DISP_XTRA_SOUND:
               {
#if 0
                    Handle          handle;
                    Str255          sound;

#if 0
                    short           oldResFile;
                    short           newResFile;

                    /* Open the resource file */

                    oldResFile = CurResFile();
                    newResFile = OpenResFile(sound);

                    /* Close the resource file */
                    CloseResFile(newResFile);
                    UseResFile(oldResFile);
#endif

                    /* Get the proper sound name */
                    sprintf((char *) sound + 1, "%.16s.wav",
                            angband_sound_name[v]);
                    sound[0] = strlen((char *) sound + 1);

                    /* Obtain resource XXX XXX XXX */
                    handle = GetNamedResource('snd ', sound);

                    /* Oops */
                    if(handle)
                    {
                         /* Load and Lock */
                         LoadResource(handle);
                         HLock(handle);

                         /* Play sound (wait for completion) */
                         /*SndPlay(nil, (SndListHandle)handle, false); */
                         {
                              static          SndChannelPtr
                                  mySndChannel[kMaxChannels];
                              static long     channelInit = 0;
                              long            kk;
                              OSErr           myErr = noErr;

                              if(!channelInit)
                              {
                                   for(kk = 0; kk < kMaxChannels; kk++)
                                   {
                                        /* Create sound channel for all sounds to play from */
                                        SndNewChannel(&mySndChannel[kk],
                                                      sampledSynth,
                                                      initMono, 0L);
                                   }
                                   channelInit = 1;
                              }
                              if(handle != nil)
                              {
                                   SCStatus        status;
                                   long            found;

                                   for(kk = 0, found = 0;
                                       kk < kMaxChannels && !found; kk++)
                                   {
                                        myErr =
                                            SndChannelStatus(mySndChannel
                                                             [kk],
                                                             sizeof
                                                             (SCStatus),
                                                             &status);
                                        if(myErr == noErr &&
                                           !status.scChannelBusy)
                                        {
                                             /* Play new sound ansynchronously */
                                             SndPlay(mySndChannel[kk],
                                                     (SndListHandle)
                                                     handle, true);
                                             found = 1;
                                        }
                                   }

                                   /*
                                    * if( !found )
                                    * {
                                    * SndPlay( 0L, (SndListHandle)handle, false );
                                    * }
                                    */
                              }
                         }

                         /* Unlock and release */
                         HUnlock(handle);
                         ReleaseResource(handle);
                    }
#endif
                    /* Success */
                    return (0);
               }

               /* Process random events */
          case DISP_XTRA_BORED:
               {
                    /* Process an event */
                    (void) CheckEvents(0);

                    /* Success */
                    return (0);
               }

               /* Process pending events */
          case DISP_XTRA_EVENT:
               {
                    /* Process an event */
                    (void) CheckEvents(v);

                    /* Success */
                    return (0);
               }

               /* Flush all pending events (if any) */
          case DISP_XTRA_FLUSH:
               {
                    /* Hack -- flush all events */
                    while(CheckEvents(FALSE))
                         /* loop */ ;

                    /* Success */
                    return (0);
               }

               /* Hack -- Change the "soft level" */
          case DISP_XTRA_LEVEL:
               {
#if 0
                    /* Activate if requested */
                    if(v)
                         activate(td->w);
#endif
                    /* Success */
                    return (0);
               }

               /* Clear the screen */
          case DISP_XTRA_CLEAR:
               {
#if 0
                    /* No clipping XXX XXX XXX */
                    ClipRect(&td->w->portRect);

                    /* Erase the window */
                    EraseRect(&td->w->portRect);

                    /* Set the color */
                    disp_data_color(td, DISP_WHITE);

                    /* Frame the window in white */
                    MoveTo(0, 0);
                    LineTo(0, td->size_hgt - 1);
                    LineTo(td->size_wid - 1, td->size_hgt - 1);
                    LineTo(td->size_wid - 1, 0);
                    /* Clip to the new size */
                    r.left = td->w->portRect.left + td->size_ow1;
                    r.top = td->w->portRect.top + td->size_oh1;
                    r.right = td->w->portRect.right - td->size_ow2;
                    r.bottom = td->w->portRect.bottom - td->size_oh2;
                    ClipRect(&r);
#endif
                    /* Success */
                    return (0);
               }

               /* React to changes */
          case DISP_XTRA_REACT:
               {
                    /* React to changes */
                    return (Disp_xtra_iso_react());
               }

               /* Delay (milliseconds) */
          case DISP_XTRA_DELAY:
               {
#if 0
                    /* If needed */
                    if(v > 0)
                    {
                         long            m =
                             TickCount() + (v * 60L) / 1000;

                         /* Wait for it */
                         while(TickCount() < m)
                              /* loop */ ;
                    }
#endif
                    /* Success */
                    return (0);
               }

               /* Change the scene */
          case DISP_XTRA_SCENE:
               {
                    IH_SetScene(v);

                    /* Success */
                    return (0);
               }

               /* Change the stage */
          case DISP_XTRA_STAGE:
               {
                    IH_SetStage(v);

                    /* Success */
                    return (0);
               }

               /* Toggle an overlay */
          case DISP_XTRA_TOGGL:
               {
                    return !IH_Overlay_Toggle(v);
               }

               /* Activate an overlay */
          case DISP_XTRA_SHOW:
               {
                    return !IH_Overlay_Show(v);
               }

               /* Deactivate an overlay */
          case DISP_XTRA_HIDE:
               {
                    IH_Overlay_Hide(v);

                    return (0);
               }

               /* Prepare an overlay */
          case DISP_XTRA_PREP:
               {
                    return !IH_Overlay_Prepare(v);
               }

          case DISP_XTRA_MSG:
               {
                    IH_SetLoadMessage((cptr) v);

                    return (0);
               }
     }

     /* Oops */
     return (1);
}

static          errr
Disp_curs_iso(int x,
              int y)
{
     /* Success */
     return (0);
}

/*
 * Low level graphics (Assumes valid input).
 * Draw a "cursor" at (x,y), using a "yellow box".
 * We are allowed to use "Disp_what()" to determine
 * the current screen contents (for inverting, etc).
 */
static          errr
Disp_bigcurs_iso(int x,
                 int y)
{
     return (0);
}

static          errr
Disp_wipe_iso(int x,
              int y,
              int n)
{
     /* Success */
     return (0);
}

/* Low level graphics.  Assumes valid input.
 *
 * Draw several ("n") chars, with an attr, at a given location.
 */
static          errr
Disp_text_iso(int x,
              int y,
              int n,
              byte a,
              const char *cp)
{

     return (0);
}

static          errr
Disp_pict_iso(int x,
              int y,
              int n,
              const byte * ap,
              const char *cp,
              const byte * tap,
              const char *tcp)
{
     fprintf(stderr,
             "Disp_pict_iso(): x=%d, y=%d, n=%d, ap=%s, cp=%s, tap=%s, tcp=%s\n",
             x, y, n, ap, cp, tap, tcp);

     return (0);
}

static          errr
Disp_param_iso(int display,
               int param,
               void *value)
{
     errr            rc = 0;

     switch (param)
     {
          case DISP_PARAM_RECALC:
               IH_Overlay_Recalc(display);
               break;

          case DISP_PARAM_UPDATE:
               IH_Overlay_Update(display);
               break;

          case DISP_PARAM_VAR1:
          case DISP_PARAM_VAR2:
          case DISP_PARAM_VAR3:
          case DISP_PARAM_VAR4:
               IH_Overlay_SetVar(display, param - DISP_PARAM_VAR1,
                                 (int) value);
               break;

          case DISP_PARAM_BUFFER1:
          case DISP_PARAM_BUFFER2:
               IH_Overlay_SetBuffer(display, param - DISP_PARAM_BUFFER1,
                                    (char *) value);
               break;

          case DISP_PARAM_FLAGS:
               if(value)
                    IH_Overlay_SetFlags(display, *(u32b *) value);
               else
                    rc = -1;
               break;
     }

     return (rc);
}

bool
IH_InitDisplays(void)
{
     disp           *t;

     angband_disp[0] = &data[0].t;
     t = &data[0].t;

     /* Initialize the disp */
     disp_init(t, 80 /* td->cols */ , 25 /* td->rows */ ,
               20 /* td->keys */ );

     /* Use a "software" cursor */
     t->soft_cursor = TRUE;

     /* Erase with "white space" */
     t->attr_blank = COLOR_WHITE;
     t->char_blank = ' ';

     /* Prepare the init/nuke hooks */
     t->init_hook = Disp_init_iso;
     t->nuke_hook = Disp_nuke_iso;

     /* Prepare the function hooks */
     t->user_hook = Disp_user_iso;
     t->xtra_hook = Disp_xtra_iso;
     t->wipe_hook = Disp_wipe_iso;
     t->curs_hook = Disp_curs_iso;
     t->bigcurs_hook = Disp_bigcurs_iso;
     t->text_hook = Disp_text_iso;
     t->pict_hook = Disp_pict_iso;
     t->param_hook = Disp_param_iso;
     t->lock_hook = Disp_lock_iso;
     t->unlock_hook = Disp_unlock_iso;

     /* Link the local structure */
     t->data = &data[0];

     /* Activate it */
     Disp_activate(t);

     return TRUE;
}

errr
IH_InitAngband(void)
{
     char           *path_lib;

     /* Initialize */
     fprintf(stderr, "IH_InitAngband(): Getting lib directory.\n");
     path_lib = IH_GetDataDir("lib/");

     fprintf(stderr, "IH_InitAngband(): Initializing file paths...\n");
     IH_SetLoadMessage("Initializing file paths...");
     init_file_paths(path_lib);

     fprintf(stderr, "IH_InitAngband(): Initializing angband...\n");
     IH_SetLoadMessage("Initializing angband...");
     init_angband();

     fprintf(stderr, "IH_InitAngband(): Freeing path_lib.\n");
     rnfree(path_lib);

     fprintf(stderr, "IH_InitAngband(): set title stage complete.\n");
     IH_SetStage(SCENE_TITLE_STAGE_COMPLETE);

     fprintf(stderr, "IH_InitAngband(): return 0\n");
     return 0;
}

void
IH_InitSetuid(void)
{
#ifdef SET_UID
     /* Default permissions on files */
     (void) umask(022);

# ifdef SECURE
     /* Authenticate */
     Authenticate();
# endif /* SECURE */

     /* Get the user id (?) */
     player_uid = getuid();

#ifdef VMS
     /* Mega-Hack -- Factor group id */
     player_uid += (getgid() * 1000);
#endif /* VMS */

# ifdef SAFE_SETUID

#  if defined(HAVE_SETEGID) || defined(SAFE_SETUID_POSIX)
     /* Save some info for later */
     player_euid = geteuid();
     player_egid = getegid();

#  endif /* defined(HAVE_SETEGID) || defined(SAFE_SETUID_POSIX) */

#  if 0                         /* XXX XXX XXX */
     /* Redundant setting necessary in case root is running the game */
     /* If not root or game not setuid the following two calls do nothing */
     if(setgid(getegid()) != 0)
     {
          quit("setgid(): cannot set permissions correctly!");
     }

     if(setuid(geteuid()) != 0)
     {
          quit("setuid(): cannot set permissions correctly!");
     }
#  endif /* 0 */

# endif /* SAFE_SETUID */

#endif /* SET_UID */

     /* Drop permissions */
     safe_setuid_drop();

#ifdef SET_UID
     /* Initialize the "time" checker */
     if(check_time_init() || check_time())
     {
          quit("The gates to Angband are closed (bad time).");
     }

     /* Initialize the "load" checker */
     if(check_load_init() || check_load())
     {
          quit("The gates to Angband are closed (bad load).");
     }

     /* Get the "user name" as a default player name */
     user_name(op_ptr->full_name, sizeof(op_ptr->full_name), player_uid);

#ifdef PRIVATE_USER_PATH
     /* Create a directory for the users files. */
     IH_CreateConfigDir();
#endif /* PRIVATE_USER_PATH */

#endif /* SET_UID */
}

void
IH_SetLoadMessage(cptr msg)
{
     fprintf(stderr, "IH_SetLoadMessage()\n");

     fprintf(stderr, "IH_SetLoadMessage(): get semaphore\n");
     if(!SDL_SemWait(ih.ipc.sem.msg))
     {
          ih.load_message[0] = 0;

          fprintf(stderr, "IH_SetLoadMessage(): check for msg\n");
          if(msg)
          {
               fprintf(stderr, "IH_SetLoadMessage(): copy the message\n");
               my_strcpy(ih.load_message, msg, sizeof(ih.load_message));
          }

          fprintf(stderr, "IH_SetLoadMessage(): release semaphore\n");
          SDL_SemPost(ih.ipc.sem.msg);
     }
     fprintf(stderr, "IH_SetLoadMessage(): return\n");
}

void
IH_SetErrorMessage(cptr msg)
{
     if(!SDL_SemWait(ih.ipc.sem.msg))
     {
          ih.err_message[0] = 0;

          if(msg)
          {
               my_strcpy(ih.err_message, msg, sizeof(ih.err_message));
          }

          SDL_SemPost(ih.ipc.sem.msg);
     }

     if(ih.err_message[0])
     {
          IH_Overlay_Show(DISPLAY_ERROR);
     }
     else
     {
          IH_Overlay_Hide(DISPLAY_ERROR);
     }
}
