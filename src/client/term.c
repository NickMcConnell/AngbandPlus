
/* $Id: term.c,v 1.9 2003/03/18 22:02:58 cipher Exp $ */

/*
 * Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "SDL.h"

#include "ironhells.h"
#include "term.h"
#include "ipc.h"
#include "sdl/scene.h"
#include "sdl/render/overlay.h"

/*
 * An array of term_data's
 */
static term_data data[MAX_TERM_DATA];

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
               if(wait)
                    continue;

               return FALSE;
          }

          switch (event.type)
          {
                    int             key;

                    /* We're really only interested in keydown events, since
                     * everything else will have been processed by the drawing
                     * thread.
                     */
               case SDL_KEYDOWN:
                    fprintf(stderr, "CheckEvents(): sym=%c (%d), mod=%x\n",
                            event.key.keysym.sym, event.key.keysym.sym,
                            event.key.keysym.mod);

                    key = event.key.keysym.sym;

                    /* Handle uppercase letters.
                     */
                    if(event.key.keysym.mod & KMOD_SHIFT)
                    {
                         fprintf(stderr,
                                 "CheckEvents(): Handling shifted key.\n");
                         if(isalpha(event.key.keysym.sym))
                         {
                              key = toupper(event.key.keysym.sym);
                         }
                    }

                    /* Handle special cases.
                     */
                    switch (event.key.keysym.sym)
                    {
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
                              fprintf(stderr,
                                      "CheckEvents(): Handling keypad.\n");
                              /* Begin special trigger */
                              Term_keypress(31);

                              /* Send the "keypad" modifier */
                              Term_keypress('K');

                              /* Terminate the trigger */
                              Term_keypress(13);

                              /* Send the "ascii" keypress */
                              key =
                                  (event.key.keysym.sym - SDLK_KP0) + '0';
                              break;
                    }

                    fprintf(stderr,
                            "CheckEvents(): Inserting keypress '%c'\n",
                            key);
                    Term_keypress(key);

                    fprintf(stderr, "CheckEvents(): Returning TRUE.\n");
                    return TRUE;
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
     term_data      *td = NULL;
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
                    for(i = 0; i < MAX_TERM_DATA; i++)
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
                         term_data_redraw(td);

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
                         Term_keypress(ch);
                    }

                    /* Keypad keys -> trigger plus simple keypress */
                    else if(!mc && !ms && !mo && !mx && (ck < 96))
                    {
                         /* Hack -- "enter" is confused */
                         if(ck == 76)
                              ch = '\n';

                         /* Begin special trigger */
                         Term_keypress(31);

                         /* Send the "keypad" modifier */
                         Term_keypress('K');

                         /* Terminate the trigger */
                         Term_keypress(13);

                         /* Send the "ascii" keypress */
                         Term_keypress(ch);
                    }

                    /* Bizarre key -> encoded keypress */
                    else if(ck <= 127)
                    {
                         /* Begin special trigger */
                         Term_keypress(31);

                         /* Send some modifier keys */
                         if(mc)
                              Term_keypress('C');
                         if(ms)
                              Term_keypress('S');
                         if(mo)
                              Term_keypress('O');
                         if(mx)
                              Term_keypress('X');

                         /* Downshift and encode the keycode */
                         Term_keypress(I2D((ck - 64) / 10));
                         Term_keypress(I2D((ck - 64) % 10));

                         /* Terminate the trigger */
                         Term_keypress(13);
                    }

                    break;
               }

          case mouseDown:
               {
                    int             code;

                    /* Analyze click location */
                    code = FindWindow(event.where, &w);

                    /* Find the window */
                    for(i = 0; i < MAX_TERM_DATA; i++)
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
                                   term_data_check_size(td);

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

                                   term           *old = Term;

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
                                   term_data_check_size(td);

                                   /* Activate */
                                   Term_activate(td->t);

                                   /* Hack -- Resize the term */
                                   Term_resize(td->cols, td->rows);

                                   /* Resize and Redraw */
                                   term_data_resize(td);
                                   term_data_redraw(td);

                                   /* Restore */
                                   Term_activate(old);

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
Term_init_sdl(term * t)
{
#pragma unused (t)
}

static void
Term_nuke_sdl(term * t)
{
#pragma unused (t)
     /* XXX */
}

/* Unused
 */
static          errr
Term_user_sdl(int n)
{
#pragma unused (n)
     /* Success */
     return (0);
}

/* React to changes
 */
static          errr
Term_xtra_sdl_react(void)
{

     return (0);
}

static          errr
Term_xtra_sdl(int n,
              int v)
{
     /* Analyze */
     switch (n)
     {
               /* Play a video.
                */
          case TERM_XTRA_VIDEO:
               break;

               /* Play some music.
                */
          case TERM_XTRA_MUSIC:
               break;

               /* Make a noise */
          case TERM_XTRA_NOISE:
#if 0
               /* Make a noise */
               SysBeep(1);
#endif
               /* Success */
               return (0);

               break;

               /* Make a sound */
          case TERM_XTRA_SOUND:
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
          case TERM_XTRA_BORED:
               {
                    /* Process an event */
                    (void) CheckEvents(0);

                    /* Success */
                    return (0);
               }

               /* Process pending events */
          case TERM_XTRA_EVENT:
               {
                    /* Process an event */
                    (void) CheckEvents(v);

                    /* Success */
                    return (0);
               }

               /* Flush all pending events (if any) */
          case TERM_XTRA_FLUSH:
               {
                    /* Hack -- flush all events */
                    while(CheckEvents(FALSE))
                         /* loop */ ;

                    /* Success */
                    return (0);
               }

               /* Hack -- Change the "soft level" */
          case TERM_XTRA_LEVEL:
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
          case TERM_XTRA_CLEAR:
               {
#if 0
                    /* No clipping XXX XXX XXX */
                    ClipRect(&td->w->portRect);

                    /* Erase the window */
                    EraseRect(&td->w->portRect);

                    /* Set the color */
                    term_data_color(td, TERM_WHITE);

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
          case TERM_XTRA_REACT:
               {
                    /* React to changes */
                    return (Term_xtra_sdl_react());
               }

               /* Delay (milliseconds) */
          case TERM_XTRA_DELAY:
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
          case TERM_XTRA_SCENE:
               {
                    IH_SetScene(v);

                    /* Success */
                    return (0);
               }

               /* Change the stage */
          case TERM_XTRA_STAGE:
               {
                    IH_SetStage(v);

                    /* Success */
                    return (0);
               }

               /* Toggle an overlay */
          case TERM_XTRA_OVERL:
               {
                    errr            rc;

                    rc = IH_ToggleOverlay(v);

                    return (rc);
               }

               /* Activate an overlay */
          case TERM_XTRA_OVER1:
               {
                    errr            rc;

                    rc = IH_ActivateOverlay(v);

                    return (rc);
               }

               /* Deactivate an overlay */
          case TERM_XTRA_OVER0:
               {
                    IH_DeactivateOverlay(v);

                    return (0);
               }
     }

     /* Oops */
     return (1);
}

static          errr
Term_curs_sdl(int x,
              int y)
{
     /* Success */
     return (0);
}

/*
 * Low level graphics (Assumes valid input).
 * Draw a "cursor" at (x,y), using a "yellow box".
 * We are allowed to use "Term_what()" to determine
 * the current screen contents (for inverting, etc).
 */
static          errr
Term_bigcurs_sdl(int x,
                 int y)
{
     return (0);
}

static          errr
Term_wipe_sdl(int x,
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
Term_text_sdl(int x,
              int y,
              int n,
              byte a,
              const char *cp)
{

     return (0);
}

static          errr
Term_pict_sdl(int x,
              int y,
              int n,
              const byte * ap,
              const char *cp,
              const byte * tap,
              const char *tcp)
{
     fprintf(stderr,
             "Term_pict_sdl(): x=%d, y=%d, n=%d, ap=%s, cp=%s, tap=%s, tcp=%s\n",
             x, y, n, ap, cp, tap, tcp);

     return (0);
}

static          errr
Term_setup_input_sdl(const byte * prompt,
                     byte * buf,
                     int len)
{
     if(!SDL_SemWait(ih.sem.talk))
     {
          ih.dialog_prompt = prompt;
          ih.input_buffer = buf;
          ih.input_len = len;

          SDL_SemPost(ih.sem.talk);
     }

     return (0);
}

bool
IH_InitTerm(void)
{
     term           *t;

     angband_term[0] = &data[0].t;
     t = &data[0].t;

     /* Initialize the term */
     term_init(t, 80 /* td->cols */ , 25 /* td->rows */ ,
               20 /* td->keys */ );

     /* Use a "software" cursor */
     t->soft_cursor = TRUE;

     /* Erase with "white space" */
     t->attr_blank = TERM_WHITE;
     t->char_blank = ' ';

     /* Prepare the init/nuke hooks */
     t->init_hook = Term_init_sdl;
     t->nuke_hook = Term_nuke_sdl;

     /* Prepare the function hooks */
     t->user_hook = Term_user_sdl;
     t->xtra_hook = Term_xtra_sdl;
     t->wipe_hook = Term_wipe_sdl;
     t->curs_hook = Term_curs_sdl;
     t->bigcurs_hook = Term_bigcurs_sdl;
     t->text_hook = Term_text_sdl;
     t->pict_hook = Term_pict_sdl;
     t->setup_input_hook = Term_setup_input_sdl;

     /* Link the local structure */
     t->data = &data[0];

     /* Activate it */
     Term_activate(t);

     return TRUE;
}
