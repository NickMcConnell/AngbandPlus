
/* $Id: z-disp.c,v 1.2 2003/04/01 22:26:09 cipher Exp $ */

/*
 * Copyright (c) 1997 Ben Harrison
 * Modifications: Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/* Purpose: a generic, (efficient?,) display window package -PAS- */

#include "z-disp.h"
#include "z-virt.h"

/*
 * This file provides a generic, efficient, terminal window package,
 * which can be used not only on standard terminal environments such
 * as dumb terminals connected to a Unix box, but also in more modern
 * "graphic" environments, such as the Macintosh or Unix/X11.
 *
 * Each "window" works like a standard "dumb terminal", that is, it
 * can display a two dimensional array of grids containing colored
 * textual symbols, plus an optional cursor, and it can be used to
 * get keypress events from the user.
 *
 * In fact, this package can simply be used, if desired, to support
 * programs which will look the same on a dumb terminal as they do
 * on a graphic platform such as the Macintosh.
 *
 * This package was designed to help port the game "Angband" to a wide
 * variety of different platforms.  Angband, like many other games in
 * the "rogue-like" heirarchy, requires, at the minimum, the ability
 * to display "colored textual symbols" in a standard 80x24 "window",
 * such as that provided by most dumb terminals, and many old personal
 * computers, and to check for "keypresses" from the user.  The major
 * concerns were thus portability and efficiency, so Angband could be
 * easily ported to many different systems, with minimal effort, and
 * yet would run quickly on each of these systems, no matter what kind
 * of underlying hardware/software support was being used.
 *
 * It is important to understand the differences between the older
 * "dumb terminals" and the newer "graphic interface" machines, since
 * this package was designed to work with both types of systems.
 *
 * New machines:
 *   waiting for a keypress is complex
 *   checking for a keypress is often cheap
 *   changing "colors" may be expensive
 *   the "color" of a "blank" is rarely important
 *   moving the "cursor" is relatively cheap
 *   use a "software" cursor (only moves when requested)
 *   drawing characters normally will not erase old ones
 *   drawing a character on the cursor often erases it
 *   may have fast routines for "clear a region"
 *   the bottom right corner is usually not special
 *
 * Old machines:
 *   waiting for a keypress is simple
 *   checking for a keypress is often expensive
 *   changing "colors" is usually cheap
 *   the "color" of a "blank" may be important
 *   moving the "cursor" may be expensive
 *   use a "hardware" cursor (moves during screen updates)
 *   drawing new symbols automatically erases old ones
 *   characters may only be drawn at the cursor location
 *   drawing a character on the cursor will move the cursor
 *   may have fast routines for "clear entire window"
 *   may have fast routines for "clear to end of line"
 *   the bottom right corner is often dangerous
 *
 *
 * This package provides support for multiple windows, each of an
 * arbitrary size (up to 255x255), each with its own set of flags,
 * and its own hooks to handle several low-level procedures which
 * differ from platform to platform.  Then the main program simply
 * creates one or more "disp" structures, setting the various flags
 * and hooks in a manner appropriate for the current platform, and
 * then it can use the various "disp" structures without worrying
 * about the underlying platform.
 *
 *
 * This package allows each "grid" in each window to hold an attr/char
 * pair, with each ranging from 0 to 255, and makes very few assumptions
 * about the meaning of any attr/char values.  Normally, we assume that
 * "attr 0" is "black", with the semantics that "black" text should be
 * sent to "Disp_wipe()" instead of "Disp_text()", but this sematics is
 * modified if either the "always_pict" or the "always_text" flags are
 * set.  We assume that "char 0" is "dangerous", since placing such a
 * "char" in the middle of a string "terminates" the string, and usually
 * we prevent its use.
 *
 * Finally, we use a special attr/char pair, defaulting to "attr 0" and
 * "char 32", also known as "black space", when we "erase" or "clear"
 * any window, but this pair can be redefined to any pair, including
 * the standard "white space", or the bizarre "emptiness" ("attr 0"
 * and "char 0"), as long as various obscure restrictions are met.
 *
 *
 * This package provides several functions which allow a program to
 * interact with the "disp" structures.  Most of the functions allow
 * the program to "request" certain changes to the current "disp",
 * such as moving the cursor, drawing an attr/char pair, erasing a
 * region of grids, hiding the cursor, etc.  Then there is a special
 * function which causes all of the "pending" requests to be performed
 * in an efficient manner.  There is another set of functions which
 * allow the program to query the "requested state" of the current
 * "disp", such as asking for the cursor location, or what attr/char
 * is at a given location, etc.  There is another set of functions
 * dealing with "keypress" events, which allows the program to ask if
 * the user has pressed any keys, or to forget any keys the user pressed.
 * There is a pair of functions to allow this package to memorize the
 * contents of the current "disp", and to restore these contents at
 * a later time.  There is a special function which allows the program
 * to specify which "disp" structure should be the "current" one.  At
 * the lowest level, there is a set of functions which allow a new
 * "disp" to be initialized or destroyed, and which allow this package,
 * or a program, to access the special "hooks" defined for the current
 * "disp", and a set of functions which those "hooks" can use to inform
 * this package of the results of certain occurances, for example, one
 * such function allows this package to learn about user keypresses,
 * detected by one of the special "hooks".
 *
 * We provide, among other things, the functions "Disp_keypress()"
 * to "react" to keypress events, and "Disp_redraw()" to redraw the
 * entire window, plus "Disp_resize()" to note a new size.
 *
 *
 * Note that the current "disp" contains two "window images".  One of
 * these images represents the "requested" contents of the "disp", and
 * the other represents the "actual" contents of the "disp", at the time
 * of the last performance of pending requests.  This package uses these
 * two images to determine the "minimal" amount of work needed to make
 * the "actual" contents of the "disp" match the "requested" contents of
 * the "disp".  This method is not perfect, but it often reduces the
 * amount of work needed to perform the pending requests, which thus
 * increases the speed of the program itself.  This package promises
 * that the requested changes will appear to occur either "all at once"
 * or in a "top to bottom" order.  In addition, a "cursor" is maintained,
 * and this cursor is updated along with the actual window contents.
 *
 * Currently, the "Disp_fresh()" routine attempts to perform the "minimum"
 * number of physical updates, in terms of total "work" done by the hooks
 * Disp_wipe(), Disp_text(), and Disp_pict(), making use of the fact that
 * adjacent characters of the same color can both be drawn together using
 * the "Disp_text()" hook, and that "black" text can often be sent to the
 * "Disp_wipe()" hook instead of the "Disp_text()" hook, and if something
 * is already displayed in a window, then it is not necessary to display
 * it again.  Unfortunately, this may induce slightly non-optimal results
 * in some cases, in particular, those in which, say, a string of ten
 * characters needs to be written, but the fifth character has already
 * been displayed.  Currently, this will cause the "Disp_text()" routine
 * to be called once for each half of the string, instead of once for the
 * whole string, which, on some machines, may be non-optimal behavior.
 *
 * The new formalism includes a "displayed" screen image (old) which
 * is actually seen by the user, a "requested" screen image (scr)
 * which is being prepared for display, a "memorized" screen image
 * (mem) which is used to save and restore screen images, and a
 * "temporary" screen image (tmp) which is currently unused.
 *
 *
 * Several "flags" are available in each "disp" to allow the underlying
 * visual system (which initializes the "disp" structure) to "optimize"
 * the performance of this package for the given system, or to request
 * certain behavior which is helpful/required for the given system.
 *
 * The "soft_cursor" flag indicates the use of a "soft" cursor, which
 * only moves when explicitly requested,and which is "erased" when
 * any characters are drawn on top of it.  This flag is used for all
 * "graphic" systems which handle the cursor by "drawing" it.
 *
 * The "icky_corner" flag indicates that the bottom right "corner"
 * of the windows are "icky", and "printing" anything there may
 * induce "messy" behavior, such as "scrolling".  This flag is used
 * for most old "dumb terminal" systems.
 *
 *
 * The "disp" structure contains the following function "hooks":
 *
 *   Disp->init_hook = Init the disp
 *   Disp->nuke_hook = Nuke the disp
 *   Disp->user_hook = Perform user actions
 *   Disp->xtra_hook = Perform extra actions
 *   Disp->curs_hook = Draw (or Move) the cursor
 *   Disp->bigcurs_hook = Draw (or Move) the big cursor (bigtile mode)
 *   Disp->wipe_hook = Draw some blank spaces
 *   Disp->text_hook = Draw some text in the window
 *   Disp->pict_hook = Draw some attr/chars in the window
 *
 * The "Disp->user_hook" hook provides a simple hook to an implementation
 * defined function, with application defined semantics.  It is available
 * to the program via the "Disp_user()" function.
 *
 * The "Disp->xtra_hook" hook provides a variety of different functions,
 * based on the first parameter (which should be taken from the various
 * DISP_XTRA_* defines) and the second parameter (which may make sense
 * only for some first parameters).  It is available to the program via
 * the "Disp_xtra()" function, though some first parameters are only
 * "legal" when called from inside this package.
 *
 * The "Disp->curs_hook" hook provides this package with a simple way
 * to "move" or "draw" the cursor to the grid "x,y", depending on the
 * setting of the "soft_cursor" flag.  Note that the cursor is never
 * redrawn if "nothing" has happened to the screen (even temporarily).
 * This hook is required.
 *
 * The "Disp->wipe_hook" hook provides this package with a simple way
 * to "erase", starting at "x,y", the next "n" grids.  This hook assumes
 * that the input is valid.  This hook is required, unless the setting
 * of the "always_pict" or "always_text" flags makes it optional.
 *
 * The "Disp->text_hook" hook provides this package with a simple way
 * to "draw", starting at "x,y", the "n" chars contained in "cp", using
 * the attr "a".  This hook assumes that the input is valid, and that
 * "n" is between 1 and 256 inclusive, but it should NOT assume that
 * the contents of "cp" are null-terminated.  This hook is required,
 * unless the setting of the "always_pict" flag makes it optional.
 *
 * The "Disp->pict_hook" hook provides this package with a simple way
 * to "draw", starting at "x,y", the "n" attr/char pairs contained in
 * the arrays "ap" and "cp".  This hook assumes that the input is valid,
 * and that "n" is between 1 and 256 inclusive, but it should NOT assume
 * that the contents of "cp" are null-terminated.  This hook is optional,
 * unless the setting of the "always_pict" or "higher_pict" flags make
 * it required.  Note that recently, this hook was changed from taking
 * a byte "a" and a char "c" to taking a length "n", an array of bytes
 * "ap" and an array of chars "cp".  Old implementations of this hook
 * should now iterate over all "n" attr/char pairs.
 * The two new arrays "tap" and "tcp" can contain the attr/char pairs
 * of the terrain below the values in "ap" and "cp".  These values can
 * be used to implement transparency when using graphics by drawing
 * the terrain values as a background and the "ap", "cp" values in
 * the foreground.
 *
 * The game "Angband" uses a set of files called "main-xxx.c", for
 * various "xxx" suffixes.  Most of these contain a function called
 * "init_xxx()", that will prepare the underlying visual system for
 * use with Angband, and then create one or more "disp" structures,
 * using flags and hooks appropriate to the given platform, so that
 * the "main()" function can call one (or more) of the "init_xxx()"
 * functions, as appropriate, to prepare the required "disp" structs
 * (one for each desired sub-window), and these "init_xxx()" functions
 * are called from a centralized "main()" function in "main.c".  Other
 * "main-xxx.c" systems contain their own "main()" function which, in
 * addition to doing everything needed to initialize the actual program,
 * also does everything that the normal "init_xxx()" functions would do.
 *
 * The game "Angband" defines, in addition to "attr 0", all of the
 * attr codes from 1 to 15, using definitions in "defines.h", and
 * thus the "main-xxx.c" files used by Angband must handle these
 * attr values correctly.  Also, they must handle all other attr
 * values, though they may do so in any way they wish, for example,
 * by always taking every attr code mod 16.  Many of the "main-xxx.c"
 * files use "white space" ("attr 1" / "char 32") to "erase" or "clear"
 * any window, for efficiency.
 *
 * The game "Angband" uses the "Disp_user" hook to allow any of the
 * "main-xxx.c" files to interact with the user, by calling this hook
 * whenever the user presses the "!" key when the game is waiting for
 * a new command.  This could be used, for example, to provide "unix
 * shell commands" to the Unix versions of the game.
 *
 * See "main-xxx.c" for a simple skeleton file which can be used to
 * create a "visual system" for a new platform when porting Angband.
 */

/*
 * The current "disp"
 */
disp           *Disp = NULL;

/*** Local routines ***/

/*
 * Nuke a disp_win (see below)
 */
static          errr
disp_win_nuke(disp_win * s)
{
     /* Free the window access arrays */
     KILL(s->a);
     KILL(s->c);

     /* Free the window content arrays */
     KILL(s->va);
     KILL(s->vc);

     /* Free the terrain access arrays */
     KILL(s->ta);
     KILL(s->tc);

     /* Free the terrain content arrays */
     KILL(s->vta);
     KILL(s->vtc);

     /* Success */
     return (0);
}

/*
 * Initialize a "disp_win" (using the given window size)
 */
static          errr
disp_win_init(disp_win * s,
              int w,
              int h)
{
     int             y;

     /* Make the window access arrays */
     C_MAKE(s->a, h, byte *);
     C_MAKE(s->c, h, char *);

     /* Make the window content arrays */
     C_MAKE(s->va, h * w, byte);
     C_MAKE(s->vc, h * w, char);

     /* Make the terrain access arrays */
     C_MAKE(s->ta, h, byte *);
     C_MAKE(s->tc, h, char *);

     /* Make the terrain content arrays */
     C_MAKE(s->vta, h * w, byte);
     C_MAKE(s->vtc, h * w, char);

     /* Prepare the window access arrays */
     for(y = 0; y < h; y++)
     {
          s->a[y] = s->va + w * y;
          s->c[y] = s->vc + w * y;

          s->ta[y] = s->vta + w * y;
          s->tc[y] = s->vtc + w * y;
     }

     /* Success */
     return (0);
}

/*
 * Copy a "disp_win" from another
 */
static          errr
disp_win_copy(disp_win * s,
              disp_win * f,
              int w,
              int h)
{
     int             x, y;

     /* Copy contents */
     for(y = 0; y < h; y++)
     {
          byte           *f_aa = f->a[y];
          char           *f_cc = f->c[y];

          byte           *s_aa = s->a[y];
          char           *s_cc = s->c[y];

          byte           *f_taa = f->ta[y];
          char           *f_tcc = f->tc[y];

          byte           *s_taa = s->ta[y];
          char           *s_tcc = s->tc[y];

          for(x = 0; x < w; x++)
          {
               *s_aa++ = *f_aa++;
               *s_cc++ = *f_cc++;

               *s_taa++ = *f_taa++;
               *s_tcc++ = *f_tcc++;
          }
     }

     /* Copy cursor */
     s->cx = f->cx;
     s->cy = f->cy;
     s->cu = f->cu;
     s->cv = f->cv;

     /* Success */
     return (0);
}

/*** External hooks ***/

/*
 * Execute the "Disp->user_hook" hook, if available (see above).
 */
errr
Disp_user(int n)
{
     /* Verify the hook */
     if(!Disp->user_hook)
          return (-1);

     /* Call the hook */
     return ((*Disp->user_hook) (n));
}

/*
 * Execute the "Disp->param_hook" hook, if available (see above).
 */
errr
Disp_param(int display,
           int param,
           void *value)
{
     /* Verify the hook */
     if(!Disp->param_hook)
          return (-1);

     /* Call the hook */
     return ((*Disp->param_hook) (display, param, value));
}

/*
 * Execute the "Disp->xtra_hook" hook, if available (see above).
 */
errr
Disp_xtra(int n,
          int v)
{
     /* Verify the hook */
     if(!Disp->xtra_hook)
          return (-1);

     /* Call the hook */
     return ((*Disp->xtra_hook) (n, v));
}

/*
 * Execute the "Disp->lock_hook" hook, if available (see above).
 */
errr
Disp_lock(void)
{
     /* Verify the hook */
     if(!Disp->lock_hook)
          return (-1);

     /* Call the hook */
     return ((*Disp->lock_hook) ());
}

/*
 * Execute the "Disp->unlock_hook" hook, if available (see above).
 */
errr
Disp_unlock(void)
{
     /* Verify the hook */
     if(!Disp->unlock_hook)
          return (-1);

     /* Call the hook */
     return ((*Disp->unlock_hook) ());
}

/*** Fake hooks ***/

/*
 * Hack -- fake hook for "Disp_curs()" (see above)
 */
static          errr
Disp_curs_hack(int x,
               int y)
{
     /* Compiler silliness */
     if(x || y)
          return (-2);

     /* Oops */
     return (-1);
}

/*
 * Hack -- fake hook for "Disp_wipe()" (see above)
 */
static          errr
Disp_wipe_hack(int x,
               int y,
               int n)
{
     /* Compiler silliness */
     if(x || y || n)
          return (-2);

     /* Oops */
     return (-1);
}

/*
 * Hack -- fake hook for "Disp_text()" (see above)
 */
static          errr
Disp_text_hack(int x,
               int y,
               int n,
               byte a,
               const char *cp)
{
     /* Compiler silliness */
     if(x || y || n || a || cp)
          return (-2);

     /* Oops */
     return (-1);
}

/*
 * Hack -- fake hook for "Disp_pict()" (see above)
 */
static          errr
Disp_pict_hack(int x,
               int y,
               int n,
               const byte * ap,
               const char *cp,
               const byte * tap,
               const char *tcp)
{
     /* Compiler silliness */
     if(x || y || n || ap || cp || tap || tcp)
          return (-2);

     /* Oops */
     return (-1);
}

/*** Efficient routines ***/

/*
 * Mentally draw an attr/char at a given location
 *
 * Assumes given location and values are valid.
 */
void
Disp_queue_char(int x,
                int y,
                byte a,
                char c,
                byte ta,
                char tc)
{
     byte           *scr_aa = Disp->scr->a[y];
     char           *scr_cc = Disp->scr->c[y];
     byte            oa = scr_aa[x];
     char            oc = scr_cc[x];
     byte           *scr_taa = Disp->scr->ta[y];
     char           *scr_tcc = Disp->scr->tc[y];
     byte            ota = scr_taa[x];
     char            otc = scr_tcc[x];

     /* Don't change is the terrain value is 0 */
     if(!ta)
          ta = ota;
     if(!tc)
          tc = otc;

     /* Hack -- Ignore non-changes */
     if((oa == a) && (oc == c) && (ota == ta) && (otc == tc))
          return;

     /* Save the "literal" information */
     scr_aa[x] = a;
     scr_cc[x] = c;

     scr_taa[x] = ta;
     scr_tcc[x] = tc;

     /* Check for new min/max row info */
     if(y < Disp->y1)
          Disp->y1 = y;
     if(y > Disp->y2)
          Disp->y2 = y;

     /* Check for new min/max col info for this row */
     if(x < Disp->x1[y])
          Disp->x1[y] = x;
     if(x > Disp->x2[y])
          Disp->x2[y] = x;
}

/*
 * Mentally draw some attr/chars at a given location
 *
 * Assumes that (x,y) is a valid location, that the first "n" characters
 * of the string "s" are all valid (non-zero), and that (x+n-1,y) is also
 * a valid location, so the first "n" characters of "s" can all be added
 * starting at (x,y) without causing any illegal operations.
 */
void
Disp_queue_chars(int x,
                 int y,
                 int n,
                 byte a,
                 cptr s)
{
     int             x1 = -1, x2 = -1;
     byte           *scr_aa = Disp->scr->a[y];
     char           *scr_cc = Disp->scr->c[y];
     byte           *scr_taa = Disp->scr->ta[y];
     char           *scr_tcc = Disp->scr->tc[y];

     /* Queue the attr/chars */
     for(; n; x++, s++, n--)
     {
          byte            oa = scr_aa[x];
          char            oc = scr_cc[x];

          byte            ota = scr_taa[x];
          char            otc = scr_tcc[x];

          /* Hack -- Ignore non-changes */
          if((oa == a) && (oc == *s) && (ota == 0) && (otc == 0))
               continue;

          /* Save the "literal" information */
          scr_aa[x] = a;
          scr_cc[x] = *s;

          scr_taa[x] = 0;
          scr_tcc[x] = 0;

          /* Note the "range" of window updates */
          if(x1 < 0)
               x1 = x;
          x2 = x;
     }

     /* Expand the "change area" as needed */
     if(x1 >= 0)
     {
          /* Check for new min/max row info */
          if(y < Disp->y1)
               Disp->y1 = y;
          if(y > Disp->y2)
               Disp->y2 = y;

          /* Check for new min/max col info in this row */
          if(x1 < Disp->x1[y])
               Disp->x1[y] = x1;
          if(x2 > Disp->x2[y])
               Disp->x2[y] = x2;
     }
}

/*** Refresh routines ***/

/*
 * Flush a row of the current window (see "Disp_fresh")
 *
 * Display text using "Disp_pict()"
 */
static void
Disp_fresh_row_pict(int y,
                    int x1,
                    int x2)
{
     int             x;
     byte           *old_aa = Disp->old->a[y];
     char           *old_cc = Disp->old->c[y];
     byte           *scr_aa = Disp->scr->a[y];
     char           *scr_cc = Disp->scr->c[y];
     byte           *old_taa = Disp->old->ta[y];
     char           *old_tcc = Disp->old->tc[y];
     byte           *scr_taa = Disp->scr->ta[y];
     char           *scr_tcc = Disp->scr->tc[y];
     byte            ota;
     char            otc;
     byte            nta;
     char            ntc;

     /* Pending length */
     int             fn = 0;

     /* Pending start */
     int             fx = 0;
     byte            oa;
     char            oc;
     byte            na;
     char            nc;

     /* Scan "modified" columns */
     for(x = x1; x <= x2; x++)
     {
          /* See what is currently here */
          oa = old_aa[x];
          oc = old_cc[x];

          /* See what is desired there */
          na = scr_aa[x];
          nc = scr_cc[x];

          ota = old_taa[x];
          otc = old_tcc[x];

          nta = scr_taa[x];
          ntc = scr_tcc[x];

          /* Handle unchanged grids */
          if((na == oa) && (nc == oc) && (nta == ota) && (ntc == otc))
          {
               /* Flush */
               if(fn)
               {
                    /* Draw pending attr/char pairs */
                    (void) ((*Disp->pict_hook) (fx, y, fn, &scr_aa[fx],
                                                &scr_cc[fx], &scr_taa[fx],
                                                &scr_tcc[fx]));

                    /* Forget */
                    fn = 0;
               }

               /* Skip */
               continue;
          }

          /* Save new contents */
          old_aa[x] = na;
          old_cc[x] = nc;

          old_taa[x] = nta;
          old_tcc[x] = ntc;

          /* Restart and Advance */
          if(fn++ == 0)
               fx = x;
     }

     /* Flush */
     if(fn)
     {
          /* Draw pending attr/char pairs */
          (void) ((*Disp->pict_hook) (fx, y, fn, &scr_aa[fx], &scr_cc[fx],
                                      &scr_taa[fx], &scr_tcc[fx]));
     }
}

/*
 * Flush a row of the current window (see "Disp_fresh")
 *
 * Display text using "Disp_text()" and "Disp_wipe()",
 * but use "Disp_pict()" for high-bit attr/char pairs
 */
static void
Disp_fresh_row_both(int y,
                    int x1,
                    int x2)
{
     int             x;
     byte           *old_aa = Disp->old->a[y];
     char           *old_cc = Disp->old->c[y];
     byte           *scr_aa = Disp->scr->a[y];
     char           *scr_cc = Disp->scr->c[y];
     byte           *old_taa = Disp->old->ta[y];
     char           *old_tcc = Disp->old->tc[y];
     byte           *scr_taa = Disp->scr->ta[y];
     char           *scr_tcc = Disp->scr->tc[y];
     byte            ota;
     char            otc;
     byte            nta;
     char            ntc;

     /* The "always_text" flag */
     int             always_text = Disp->always_text;

     /* Pending length */
     int             fn = 0;

     /* Pending start */
     int             fx = 0;

     /* Pending attr */
     byte            fa = Disp->attr_blank;
     byte            oa;
     char            oc;
     byte            na;
     char            nc;

     /* Scan "modified" columns */
     for(x = x1; x <= x2; x++)
     {
          /* See what is currently here */
          oa = old_aa[x];
          oc = old_cc[x];

          /* See what is desired there */
          na = scr_aa[x];
          nc = scr_cc[x];

          ota = old_taa[x];
          otc = old_tcc[x];

          nta = scr_taa[x];
          ntc = scr_tcc[x];

          /* Handle unchanged grids */
          if((na == oa) && (nc == oc) && (nta == ota) && (ntc == otc))
          {
               /* Flush */
               if(fn)
               {
                    /* Draw pending chars (normal) */
                    if(fa || always_text)
                    {
                         (void) ((*Disp->text_hook) (fx, y, fn, fa,
                                                     &scr_cc[fx]));
                    }

                    /* Draw pending chars (black) */
                    else
                    {
                         (void) ((*Disp->wipe_hook) (fx, y, fn));
                    }

                    /* Forget */
                    fn = 0;
               }

               /* Skip */
               continue;
          }

          /* Save new contents */
          old_aa[x] = na;
          old_cc[x] = nc;
          old_taa[x] = nta;
          old_tcc[x] = ntc;

          /* Handle high-bit attr/chars */
          if((na & 0x80) && (nc & 0x80))
          {
               /* 2nd byte of bigtile */
               if((na == 255) && (nc == -1))
                    continue;

               /* Flush */
               if(fn)
               {
                    /* Draw pending chars (normal) */
                    if(fa || always_text)
                    {
                         (void) ((*Disp->text_hook) (fx, y, fn, fa,
                                                     &scr_cc[fx]));
                    }

                    /* Draw pending chars (black) */
                    else
                    {
                         (void) ((*Disp->wipe_hook) (fx, y, fn));
                    }

                    /* Forget */
                    fn = 0;
               }

               /* Hack -- Draw the special attr/char pair */
               (void) ((*Disp->pict_hook) (x, y, 1, &na, &nc, &nta, &ntc));

               /* Skip */
               continue;
          }

          /* Notice new color */
          if(fa != na)
          {
               /* Flush */
               if(fn)
               {
                    /* Draw the pending chars */
                    if(fa || always_text)
                    {
                         (void) ((*Disp->text_hook) (fx, y, fn, fa,
                                                     &scr_cc[fx]));
                    }

                    /* Hack -- Erase "leading" spaces */
                    else
                    {
                         (void) ((*Disp->wipe_hook) (fx, y, fn));
                    }

                    /* Forget */
                    fn = 0;
               }

               /* Save the new color */
               fa = na;
          }

          /* Restart and Advance */
          if(fn++ == 0)
               fx = x;
     }

     /* Flush */
     if(fn)
     {
          /* Draw pending chars (normal) */
          if(fa || always_text)
          {
               (void) ((*Disp->text_hook) (fx, y, fn, fa, &scr_cc[fx]));
          }

          /* Draw pending chars (black) */
          else
          {
               (void) ((*Disp->wipe_hook) (fx, y, fn));
          }
     }
}

/*
 * Flush a row of the current window (see "Disp_fresh")
 *
 * Display text using "Disp_text()" and "Disp_wipe()"
 */
static void
Disp_fresh_row_text(int y,
                    int x1,
                    int x2)
{
     int             x;
     byte           *old_aa = Disp->old->a[y];
     char           *old_cc = Disp->old->c[y];
     byte           *scr_aa = Disp->scr->a[y];
     char           *scr_cc = Disp->scr->c[y];

     /* The "always_text" flag */
     int             always_text = Disp->always_text;

     /* Pending length */
     int             fn = 0;

     /* Pending start */
     int             fx = 0;

     /* Pending attr */
     byte            fa = Disp->attr_blank;
     byte            oa;
     char            oc;
     byte            na;
     char            nc;

     /* Scan "modified" columns */
     for(x = x1; x <= x2; x++)
     {
          /* See what is currently here */
          oa = old_aa[x];
          oc = old_cc[x];

          /* See what is desired there */
          na = scr_aa[x];
          nc = scr_cc[x];

          /* Handle unchanged grids */
          if((na == oa) && (nc == oc))
          {
               /* Flush */
               if(fn)
               {
                    /* Draw pending chars (normal) */
                    if(fa || always_text)
                    {
                         (void) ((*Disp->text_hook) (fx, y, fn, fa,
                                                     &scr_cc[fx]));
                    }

                    /* Draw pending chars (black) */
                    else
                    {
                         (void) ((*Disp->wipe_hook) (fx, y, fn));
                    }

                    /* Forget */
                    fn = 0;
               }

               /* Skip */
               continue;
          }

          /* Save new contents */
          old_aa[x] = na;
          old_cc[x] = nc;

          /* Notice new color */
          if(fa != na)
          {
               /* Flush */
               if(fn)
               {
                    /* Draw the pending chars */
                    if(fa || always_text)
                    {
                         (void) ((*Disp->text_hook) (fx, y, fn, fa,
                                                     &scr_cc[fx]));
                    }

                    /* Hack -- Erase "leading" spaces */
                    else
                    {
                         (void) ((*Disp->wipe_hook) (fx, y, fn));
                    }

                    /* Forget */
                    fn = 0;
               }

               /* Save the new color */
               fa = na;
          }

          /* Restart and Advance */
          if(fn++ == 0)
               fx = x;
     }

     /* Flush */
     if(fn)
     {
          /* Draw pending chars (normal) */
          if(fa || always_text)
          {
               (void) ((*Disp->text_hook) (fx, y, fn, fa, &scr_cc[fx]));
          }

          /* Draw pending chars (black) */
          else
          {
               (void) ((*Disp->wipe_hook) (fx, y, fn));
          }
     }
}

/*
 * Actually perform all requested changes to the window
 *
 * If absolutely nothing has changed, not even temporarily, or if the
 * current "Disp" is not mapped, then this function will return 1 and
 * do absolutely nothing.
 *
 * Note that when "soft_cursor" is true, we erase the cursor (if needed)
 * whenever anything has changed, and redraw it (if needed) after all of
 * the screen updates are complete.  This will induce a small amount of
 * "cursor flicker" but only when the screen has been updated.  If the
 * screen is updated and then restored, you may still get this flicker.
 *
 * When "soft_cursor" is not true, we make the cursor invisible before
 * doing anything else if it is supposed to be invisible by the time we
 * are done, and we make it visible after moving it to its final location
 * after all of the screen updates are complete.
 *
 * Note that "Disp_xtra(DISP_XTRA_CLEAR,0)" must erase the entire screen,
 * including the cursor, if needed, and may place the cursor anywhere.
 *
 * Note that "Disp_xtra(DISP_XTRA_FROSH,y)" will be always be called
 * after any row "y" has been "flushed", unless the "Disp->never_frosh"
 * flag is set, and "Disp_xtra(DISP_XTRA_FRESH,0)" will be called after
 * all of the rows have been "flushed".
 *
 * Note the use of three different functions to handle the actual flush,
 * based on the settings of the "Disp->always_pict" and "Disp->higher_pict"
 * flags (see below).
 *
 * The three helper functions (above) work by collecting similar adjacent
 * grids into stripes, and then sending each stripe to "Disp->pict_hook",
 * "Disp->text_hook", or "Disp->wipe_hook", based on the settings of the
 * "Disp->always_pict" and "Disp->higher_pict" flags, which select which
 * of the helper functions to call to flush each row.
 *
 * The helper functions currently "skip" any grids which already contain
 * the desired contents.  This may or may not be the best method, especially
 * when the desired content fits nicely into the current stripe.  For example,
 * it might be better to go ahead and queue them while allowed, but keep a
 * count of the "trailing skipables", then, when time to flush, or when a
 * "non skippable" is found, force a flush if there are too many skippables.
 *
 * Perhaps an "initialization" stage, where the "text" (and "attr")
 * buffers are "filled" with information, converting "blanks" into
 * a convenient representation, and marking "skips" with "zero chars",
 * and then some "processing" is done to determine which chars to skip.
 *
 * Currently, the helper functions are optimal for systems which prefer
 * to "print a char + move a char + print a char" to "print three chars",
 * and for applications that do a lot of "detailed" color printing.
 *
 * In the two "queue" functions, total "non-changes" are "pre-skipped".
 * The helper functions must also handle situations in which the contents
 * of a grid are changed, but then changed back to the original value,
 * and situations in which two grids in the same row are changed, but
 * the grids between them are unchanged.
 *
 * If the "Disp->always_pict" flag is set, then "Disp_fresh_row_pict()"
 * will be used instead of "Disp_fresh_row_text()".  This allows all the
 * modified grids to be collected into stripes of attr/char pairs, which
 * are then sent to the "Disp->pict_hook" hook, which can draw these pairs
 * in whatever way it would like.
 *
 * If the "Disp->higher_pict" flag is set, then "Disp_fresh_row_both()"
 * will be used instead of "Disp_fresh_row_text()".  This allows all the
 * "special" attr/char pairs (in which both the attr and char have the
 * high-bit set) to be sent (one pair at a time) to the "Disp->pict_hook"
 * hook, which can draw these pairs in whatever way it would like.
 *
 * Normally, the "Disp_wipe()" function is used only to display "blanks"
 * that were induced by "Disp_clear()" or "Disp_erase()", and then only
 * if the "attr_blank" and "char_blank" fields have not been redefined
 * to use "white space" instead of the default "black space".  Actually,
 * the "Disp_wipe()" function is used to display all "black" text, such
 * as the default "spaces" created by "Disp_clear()" and "Disp_erase()".
 *
 * Note that the "Disp->always_text" flag will disable the use of the
 * "Disp_wipe()" function hook entirely, and force all text, even text
 * drawn in the color "black", to be explicitly drawn.  This is useful
 * for machines which implement "Disp_wipe()" by just drawing spaces.
 *
 * Note that the "Disp->always_pict" flag will disable the use of the
 * "Disp_wipe()" function entirely, and force everything, even text
 * drawn in the attr "black", to be explicitly drawn.
 *
 * Note that if no "black" text is ever drawn, and if "attr_blank" is
 * not "zero", then the "Disp_wipe" hook will never be used, even if
 * the "Disp->always_text" flag is not set.
 *
 * This function does nothing unless the "Disp" is "mapped", which allows
 * certain systems to optimize the handling of "closed" windows.
 *
 * On systems with a "soft" cursor, we must explicitly erase the cursor
 * before flushing the output, if needed, to prevent a "jumpy" refresh.
 * The actual method for this is horrible, but there is very little that
 * we can do to simplify it efficiently.  XXX XXX XXX
 *
 * On systems with a "hard" cursor, we will "hide" the cursor before
 * flushing the output, if needed, to avoid a "flickery" refresh.  It
 * would be nice to *always* hide the cursor during the refresh, but
 * this might be expensive (and/or ugly) on some machines.
 *
 * The "Disp->icky_corner" flag is used to avoid calling "Disp_wipe()"
 * or "Disp_pict()" or "Disp_text()" on the bottom right corner of the
 * window, which might induce "scrolling" or other nasty stuff on old
 * dumb terminals.  This flag is handled very efficiently.  We assume
 * that the "Disp_curs()" call will prevent placing the cursor in the
 * corner, if needed, though I doubt such placement is ever a problem.
 * Currently, the use of "Disp->icky_corner" and "Disp->soft_cursor"
 * together may result in undefined behavior.
 */
errr
Disp_fresh(void)
{
     int             x, y;
     int             w = Disp->wid;
     int             h = Disp->hgt;
     int             y1 = Disp->y1;
     int             y2 = Disp->y2;
     disp_win       *old = Disp->old;
     disp_win       *scr = Disp->scr;

     /* Do nothing unless "mapped" */
     if(!Disp->mapped_flag)
          return (1);

     /* Trivial Refresh */
     if((y1 > y2) &&
        (scr->cu == old->cu) &&
        (scr->cv == old->cv) &&
        (scr->cx == old->cx) &&
        (scr->cy == old->cy) && !(Disp->total_erase))
     {
          /* Nothing */
          return (1);
     }

     /* Paranoia -- use "fake" hooks to prevent core dumps */
     if(!Disp->curs_hook)
          Disp->curs_hook = Disp_curs_hack;
     if(!Disp->bigcurs_hook)
          Disp->bigcurs_hook = Disp->curs_hook;
     if(!Disp->wipe_hook)
          Disp->wipe_hook = Disp_wipe_hack;
     if(!Disp->text_hook)
          Disp->text_hook = Disp_text_hack;
     if(!Disp->pict_hook)
          Disp->pict_hook = Disp_pict_hack;

     /* Handle "total erase" */
     if(Disp->total_erase)
     {
          byte            na = Disp->attr_blank;
          char            nc = Disp->char_blank;

          /* Physically erase the entire window */
          Disp_xtra(DISP_XTRA_CLEAR, 0);

          /* Hack -- clear all "cursor" data */
          old->cv = old->cu = FALSE;
          old->cx = old->cy = 0;

          /* Wipe each row */
          for(y = 0; y < h; y++)
          {
               byte           *aa = old->a[y];
               char           *cc = old->c[y];
               byte           *taa = old->ta[y];
               char           *tcc = old->tc[y];

               /* Wipe each column */
               for(x = 0; x < w; x++)
               {
                    /* Wipe each grid */
                    *aa++ = na;
                    *cc++ = nc;

                    *taa++ = na;
                    *tcc++ = nc;
               }
          }

          /* Redraw every row */
          Disp->y1 = y1 = 0;
          Disp->y2 = y2 = h - 1;

          /* Redraw every column */
          for(y = 0; y < h; y++)
          {
               Disp->x1[y] = 0;
               Disp->x2[y] = w - 1;
          }

          /* Forget "total erase" */
          Disp->total_erase = FALSE;
     }

     /* Cursor update -- Erase old Cursor */
     if(Disp->soft_cursor)
     {
          /* Cursor was visible */
          if(!old->cu && old->cv)
          {
               int             tx = old->cx;
               int             ty = old->cy;
               byte           *old_aa = old->a[ty];
               char           *old_cc = old->c[ty];
               byte            oa = old_aa[tx];
               char            oc = old_cc[tx];
               byte           *old_taa = old->ta[ty];
               char           *old_tcc = old->tc[ty];
               byte            ota = old_taa[tx];
               char            otc = old_tcc[tx];

               /* Hack -- use "Disp_pict()" always */
               if(Disp->always_pict)
               {
                    (void) ((*Disp->pict_hook) (tx, ty, 1, &oa, &oc, &ota,
                                                &otc));
               }

               /* Hack -- use "Disp_pict()" sometimes */
               else if(Disp->higher_pict && (oa & 0x80) && (oc & 0x80))
               {
                    (void) ((*Disp->pict_hook) (tx, ty, 1, &oa, &oc, &ota,
                                                &otc));
               }

               /* Hack -- restore the actual character */
               else if(oa || Disp->always_text)
               {
                    (void) ((*Disp->text_hook) (tx, ty, 1, oa, &oc));
               }

               /* Hack -- erase the grid */
               else
               {
                    (void) ((*Disp->wipe_hook) (tx, ty, 1));
               }
          }
     }

     /* Cursor Update -- Erase old Cursor */
     else
     {
          /* Cursor will be invisible */
          if(scr->cu || !scr->cv)
          {
               /* Make the cursor invisible */
               Disp_xtra(DISP_XTRA_SHAPE, 0);
          }
     }

     /* Something to update */
     if(y1 <= y2)
     {
          /* Handle "icky corner" */
          if(Disp->icky_corner)
          {
               /* Avoid the corner */
               if(y2 >= h - 1)
               {
                    /* Avoid the corner */
                    if(Disp->x2[h - 1] > w - 2)
                    {
                         /* Avoid the corner */
                         Disp->x2[h - 1] = w - 2;
                    }
               }
          }

          /* Scan the "modified" rows */
          for(y = y1; y <= y2; ++y)
          {
               int             x1 = Disp->x1[y];
               int             x2 = Disp->x2[y];

               /* Flush each "modified" row */
               if(x1 <= x2)
               {
                    /* Always use "Disp_pict()" */
                    if(Disp->always_pict)
                    {
                         /* Flush the row */
                         Disp_fresh_row_pict(y, x1, x2);
                    }

                    /* Sometimes use "Disp_pict()" */
                    else if(Disp->higher_pict)
                    {
                         /* Flush the row */
                         Disp_fresh_row_both(y, x1, x2);
                    }

                    /* Never use "Disp_pict()" */
                    else
                    {
                         /* Flush the row */
                         Disp_fresh_row_text(y, x1, x2);
                    }

                    /* This row is all done */
                    Disp->x1[y] = w;
                    Disp->x2[y] = 0;

                    /* Hack -- Flush that row (if allowed) */
                    if(!Disp->never_frosh)
                         Disp_xtra(DISP_XTRA_FROSH, y);
               }
          }

          /* No rows are invalid */
          Disp->y1 = h;
          Disp->y2 = 0;
     }

     /* Cursor update -- Show new Cursor */
     if(Disp->soft_cursor)
     {
          /* Draw the cursor */
          if(!scr->cu && scr->cv)
          {
               if((scr->cx + 1 < w) &&
                  (old->a[scr->cy][scr->cx + 1] == 255))
               {
                    /* Double width cursor for the Bigtile mode */
                    (void) ((*Disp->bigcurs_hook) (scr->cx, scr->cy));
               }
               else
               {
                    /* Call the cursor display routine */
                    (void) ((*Disp->curs_hook) (scr->cx, scr->cy));
               }
          }
     }

     /* Cursor Update -- Show new Cursor */
     else
     {
          /* The cursor is useless, hide it */
          if(scr->cu)
          {
               /* Paranoia -- Put the cursor NEAR where it belongs */
               (void) ((*Disp->curs_hook) (w - 1, scr->cy));

               /* Make the cursor invisible */
               /* Disp_xtra(DISP_XTRA_SHAPE, 0); */
          }

          /* The cursor is invisible, hide it */
          else if(!scr->cv)
          {
               /* Paranoia -- Put the cursor where it belongs */
               (void) ((*Disp->curs_hook) (scr->cx, scr->cy));

               /* Make the cursor invisible */
               /* Disp_xtra(DISP_XTRA_SHAPE, 0); */
          }

          /* The cursor is visible, display it correctly */
          else
          {
               /* Put the cursor where it belongs */
               (void) ((*Disp->curs_hook) (scr->cx, scr->cy));

               /* Make the cursor visible */
               Disp_xtra(DISP_XTRA_SHAPE, 1);
          }
     }

     /* Save the "cursor state" */
     old->cu = scr->cu;
     old->cv = scr->cv;
     old->cx = scr->cx;
     old->cy = scr->cy;

     /* Actually flush the output */
     Disp_xtra(DISP_XTRA_FRESH, 0);

     /* Success */
     return (0);
}

/*** Output routines ***/

/*
 * Set the cursor visibility
 */
errr
Disp_set_cursor(bool v)
{
     /* Already done */
     if(Disp->scr->cv == v)
          return (1);

     /* Change */
     Disp->scr->cv = v;

     /* Success */
     return (0);
}

/*
 * Place the cursor at a given location
 *
 * Note -- "illegal" requests do not move the cursor.
 */
errr
Disp_gotoxy(int x,
            int y)
{
     int             w = Disp->wid;
     int             h = Disp->hgt;

     /* Verify */
     if((x < 0) || (x >= w))
          return (-1);
     if((y < 0) || (y >= h))
          return (-1);

     /* Remember the cursor */
     Disp->scr->cx = x;
     Disp->scr->cy = y;

     /* The cursor is not useless */
     Disp->scr->cu = 0;

     /* Success */
     return (0);
}

/*
 * At a given location, place an attr/char
 * Do not change the cursor position
 * No visual changes until "Disp_fresh()".
 */
errr
Disp_draw(int x,
          int y,
          byte a,
          char c)
{
     int             w = Disp->wid;
     int             h = Disp->hgt;

     /* Verify location */
     if((x < 0) || (x >= w))
          return (-1);
     if((y < 0) || (y >= h))
          return (-1);

     /* Paranoia -- illegal char */
     if(!c)
          return (-2);

     /* Queue it for later */
     Disp_queue_char(x, y, a, c, 0, 0);

     /* Success */
     return (0);
}

/*
 * Using the given attr, add the given char at the cursor.
 *
 * We return "-2" if the character is "illegal". XXX XXX
 *
 * We return "-1" if the cursor is currently unusable.
 *
 * We queue the given attr/char for display at the current
 * cursor location, and advance the cursor to the right,
 * marking it as unuable and returning "1" if it leaves
 * the screen, and otherwise returning "0".
 *
 * So when this function, or the following one, return a
 * positive value, future calls to either function will
 * return negative ones.
 */
errr
Disp_addch(byte a,
           char c)
{
     int             w = Disp->wid;

     /* Handle "unusable" cursor */
     if(Disp->scr->cu)
          return (-1);

     /* Paranoia -- no illegal chars */
     if(!c)
          return (-2);

     /* Queue the given character for display */
     Disp_queue_char(Disp->scr->cx, Disp->scr->cy, a, c, 0, 0);

     /* Advance the cursor */
     Disp->scr->cx++;

     /* Success */
     if(Disp->scr->cx < w)
          return (0);

     /* Note "Useless" cursor */
     Disp->scr->cu = 1;

     /* Note "Useless" cursor */
     return (1);
}

/*
 * At the current location, using an attr, add a string
 *
 * We also take a length "n", using negative values to imply
 * the largest possible value, and then we use the minimum of
 * this length and the "actual" length of the string as the
 * actual number of characters to attempt to display, never
 * displaying more characters than will actually fit, since
 * we do NOT attempt to "wrap" the cursor at the screen edge.
 *
 * We return "-1" if the cursor is currently unusable.
 * We return "N" if we were "only" able to write "N" chars,
 * even if all of the given characters fit on the screen,
 * and mark the cursor as unusable for future attempts.
 *
 * So when this function, or the preceding one, return a
 * positive value, future calls to either function will
 * return negative ones.
 */
errr
Disp_addstr(int n,
            byte a,
            cptr s)
{
     int             k;
     int             w = Disp->wid;
     errr            res = 0;

     /* Handle "unusable" cursor */
     if(Disp->scr->cu)
          return (-1);

     /* Obtain maximal length */
     k = (n < 0) ? (w + 1) : n;

     /* Obtain the usable string length */
     for(n = 0; (n < k) && s[n]; n++) /* loop */ ;

     /* React to reaching the edge of the screen */
     if(Disp->scr->cx + n >= w)
          res = n = w - Disp->scr->cx;

     /* Queue the first "n" characters for display */
     Disp_queue_chars(Disp->scr->cx, Disp->scr->cy, n, a, s);

     /* Advance the cursor */
     Disp->scr->cx += n;

     /* Hack -- Notice "Useless" cursor */
     if(res)
          Disp->scr->cu = 1;

     /* Success (usually) */
     return (res);
}

/*
 * Move to a location and, using an attr, add a char
 */
errr
Disp_putch(int x,
           int y,
           byte a,
           char c)
{
     errr            res;

     /* Move first */
     if((res = Disp_gotoxy(x, y)) != 0)
          return (res);

     /* Then add the char */
     if((res = Disp_addch(a, c)) != 0)
          return (res);

     /* Success */
     return (0);
}

/*
 * Move to a location and, using an attr, add a string
 */
errr
Disp_putstr(int x,
            int y,
            int n,
            byte a,
            cptr s)
{
     errr            res;

     /* Move first */
     if((res = Disp_gotoxy(x, y)) != 0)
          return (res);

     /* Then add the string */
     if((res = Disp_addstr(n, a, s)) != 0)
          return (res);

     /* Success */
     return (0);
}

/*
 * Place cursor at (x,y), and clear the next "n" chars
 */
errr
Disp_erase(int x,
           int y,
           int n)
{
     int             i;
     int             w = Disp->wid;

     /* int h = Disp->hgt; */
     int             x1 = -1;
     int             x2 = -1;
     byte            na = Disp->attr_blank;
     char            nc = Disp->char_blank;
     byte           *scr_aa;
     char           *scr_cc;
     byte           *scr_taa;
     char           *scr_tcc;

     /* Place cursor */
     if(Disp_gotoxy(x, y))
          return (-1);

     /* Force legal size */
     if(x + n > w)
          n = w - x;

     /* Fast access */
     scr_aa = Disp->scr->a[y];
     scr_cc = Disp->scr->c[y];

     scr_taa = Disp->scr->ta[y];
     scr_tcc = Disp->scr->tc[y];

     if((n > 0) && (scr_cc[x] == -1) && (scr_aa[x] == 255))
     {
          x--;
          n++;
     }

     /* Scan every column */
     for(i = 0; i < n; i++, x++)
     {
          byte            oa = scr_aa[x];
          char            oc = scr_cc[x];

          /* Hack -- Ignore "non-changes" */
          if((oa == na) && (oc == nc))
               continue;

          /* Save the "literal" information */
          scr_aa[x] = na;
          scr_cc[x] = nc;

          scr_taa[x] = 0;
          scr_tcc[x] = 0;

          /* Track minumum changed column */
          if(x1 < 0)
               x1 = x;

          /* Track maximum changed column */
          x2 = x;
     }

     /* Expand the "change area" as needed */
     if(x1 >= 0)
     {
          /* Check for new min/max row info */
          if(y < Disp->y1)
               Disp->y1 = y;
          if(y > Disp->y2)
               Disp->y2 = y;

          /* Check for new min/max col info in this row */
          if(x1 < Disp->x1[y])
               Disp->x1[y] = x1;
          if(x2 > Disp->x2[y])
               Disp->x2[y] = x2;
     }

     /* Success */
     return (0);
}

/*
 * Clear the entire window, and move to the top left corner
 *
 * Note the use of the special "total_erase" code
 */
errr
Disp_clear(void)
{
     int             x, y;
     int             w = Disp->wid;
     int             h = Disp->hgt;
     byte            na = Disp->attr_blank;
     char            nc = Disp->char_blank;

     /* Cursor usable */
     Disp->scr->cu = 0;

     /* Cursor to the top left */
     Disp->scr->cx = Disp->scr->cy = 0;

     /* Wipe each row */
     for(y = 0; y < h; y++)
     {
          byte           *scr_aa = Disp->scr->a[y];
          char           *scr_cc = Disp->scr->c[y];
          byte           *scr_taa = Disp->scr->ta[y];
          char           *scr_tcc = Disp->scr->tc[y];

          /* Wipe each column */
          for(x = 0; x < w; x++)
          {
               scr_aa[x] = na;
               scr_cc[x] = nc;

               scr_taa[x] = 0;
               scr_tcc[x] = 0;
          }

          /* This row has changed */
          Disp->x1[y] = 0;
          Disp->x2[y] = w - 1;
     }

     /* Every row has changed */
     Disp->y1 = 0;
     Disp->y2 = h - 1;

     /* Force "total erase" */
     Disp->total_erase = TRUE;

     /* Success */
     return (0);
}

/*
 * Redraw (and refresh) the whole window.
 */
errr
Disp_redraw(void)
{
     /* Force "total erase" */
     Disp->total_erase = TRUE;

     /* Hack -- Refresh */
     Disp_fresh();

     /* Success */
     return (0);
}

/*
 * Redraw part of a widow.
 */
errr
Disp_redraw_section(int x1,
                    int y1,
                    int x2,
                    int y2)
{
     int             i, j;
     char           *c_ptr;

     /* Bounds checking */
     if(y2 >= Disp->hgt)
          y2 = Disp->hgt - 1;
     if(x2 >= Disp->wid)
          x2 = Disp->wid - 1;
     if(y1 < 0)
          y1 = 0;
     if(x1 < 0)
          x1 = 0;

     /* Set y limits */
     Disp->y1 = y1;
     Disp->y2 = y2;

     /* Set the x limits */
     for(i = Disp->y1; i <= Disp->y2; i++)
     {
          if((x1 > 0) && (Disp->old->a[i][x1] == 255))
               x1--;

          Disp->x1[i] = x1;
          Disp->x2[i] = x2;

          c_ptr = Disp->old->c[i];

          /* Clear the section so it is redrawn */
          for(j = x1; j <= x2; j++)
          {
               /* Hack - set the old character to "none" */
               c_ptr[j] = 0;
          }
     }

     /* Hack -- Refresh */
     Disp_fresh();

     /* Success */
     return (0);
}

/*** Access routines ***/

/*
 * Extract the cursor visibility
 */
errr
Disp_get_cursor(bool * v)
{
     /* Extract visibility */
     (*v) = Disp->scr->cv;

     /* Success */
     return (0);
}

/*
 * Extract the current window size
 */
errr
Disp_get_size(int *w,
              int *h)
{
     /* Access the cursor */
     (*w) = Disp->wid;
     (*h) = Disp->hgt;

     /* Success */
     return (0);
}

/*
 * Extract the current cursor location
 */
errr
Disp_locate(int *x,
            int *y)
{
     /* Access the cursor */
     (*x) = Disp->scr->cx;
     (*y) = Disp->scr->cy;

     /* Warn about "useless" cursor */
     if(Disp->scr->cu)
          return (1);

     /* Success */
     return (0);
}

/*
 * At a given location, determine the "current" attr and char
 * Note that this refers to what will be on the window after the
 * next call to "Disp_fresh()".  It may or may not already be there.
 */
errr
Disp_what(int x,
          int y,
          byte * a,
          char *c)
{
     int             w = Disp->wid;
     int             h = Disp->hgt;

     /* Verify location */
     if((x < 0) || (x >= w))
          return (-1);
     if((y < 0) || (y >= h))
          return (-1);

     /* Direct access */
     (*a) = Disp->scr->a[y][x];
     (*c) = Disp->scr->c[y][x];

     /* Success */
     return (0);
}

/*** Input routines ***/

/*
 * Flush and forget the input
 */
errr
Disp_flush(void)
{
     /* Hack -- Flush all events */
     Disp_xtra(DISP_XTRA_FLUSH, 0);

     /* Forget all keypresses */
     Disp->key_head = Disp->key_tail = 0;

     /* Success */
     return (0);
}

/*
 * Add a keypress to the "queue"
 */
errr
Disp_keypress(int k)
{
     /* Hack -- Refuse to enqueue non-keys */
     if(!k)
          return (-1);

     /* Store the char, advance the queue */
     Disp->key_queue[Disp->key_head++] = k;

     /* Circular queue, handle wrap */
     if(Disp->key_head == Disp->key_size)
          Disp->key_head = 0;

     /* Success (unless overflow) */
     if(Disp->key_head != Disp->key_tail)
          return (0);

#if 0
     /* Hack -- Forget the oldest key */
     if(++Disp->key_tail == Disp->key_size)
          Disp->key_tail = 0;
#endif

     /* Problem */
     return (1);
}

/*
 * Add a keypress to the FRONT of the "queue"
 */
errr
Disp_key_push(int k)
{
     /* Hack -- Refuse to enqueue non-keys */
     if(!k)
          return (-1);

     /* Hack -- Overflow may induce circular queue */
     if(Disp->key_tail == 0)
          Disp->key_tail = Disp->key_size;

     /* Back up, Store the char */
     Disp->key_queue[--Disp->key_tail] = k;

     /* Success (unless overflow) */
     if(Disp->key_head != Disp->key_tail)
          return (0);

#if 0
     /* Hack -- Forget the oldest key */
     if(++Disp->key_tail == Disp->key_size)
          Disp->key_tail = 0;
#endif

     /* Problem */
     return (1);
}

/*
 * Check for a pending keypress on the key queue.
 *
 * Store the keypress, if any, in "ch", and return "0".
 * Otherwise store "zero" in "ch", and return "1".
 *
 * Wait for a keypress if "wait" is true.
 *
 * Remove the keypress if "take" is true.
 */
errr
Disp_inkey(char *ch,
           bool wait,
           bool take)
{
     /* Assume no key */
     (*ch) = '\0';

     /* Hack -- get bored */
     if(!Disp->never_bored)
     {
          /* Process random events */
          Disp_xtra(DISP_XTRA_BORED, 0);
     }

     /* Wait */
     if(wait)
     {
          /* Process pending events while necessary */
          while(Disp->key_head == Disp->key_tail)
          {
               /* Process events (wait for one) */
               Disp_xtra(DISP_XTRA_EVENT, TRUE);
          }
     }

     /* Do not Wait */
     else
     {
          /* Process pending events if necessary */
          if(Disp->key_head == Disp->key_tail)
          {
               /* Process events (do not wait) */
               Disp_xtra(DISP_XTRA_EVENT, FALSE);
          }
     }

     /* No keys are ready */
     if(Disp->key_head == Disp->key_tail)
          return (1);

     /* Extract the next keypress */
     (*ch) = Disp->key_queue[Disp->key_tail];

     /* If requested, advance the queue, wrap around if necessary */
     if(take && (++Disp->key_tail == Disp->key_size))
          Disp->key_tail = 0;

     /* Success */
     return (0);
}

/*** Extra routines ***/

/*
 * Save the "requested" screen into the "memorized" screen
 *
 * Every "Disp_save()" should match exactly one "Disp_load()"
 */
errr
Disp_save(void)
{
     int             w = Disp->wid;
     int             h = Disp->hgt;

     /* Create */
     if(!Disp->mem)
     {
          /* Allocate window */
          MAKE(Disp->mem, disp_win);

          /* Initialize window */
          disp_win_init(Disp->mem, w, h);
     }

     /* Grab */
     disp_win_copy(Disp->mem, Disp->scr, w, h);

     /* Success */
     return (0);
}

/*
 * Restore the "requested" contents (see above).
 *
 * Every "Disp_save()" should match exactly one "Disp_load()"
 */
errr
Disp_load(void)
{
     int             y;

     int             w = Disp->wid;
     int             h = Disp->hgt;

     /* Create */
     if(!Disp->mem)
     {
          /* Allocate window */
          MAKE(Disp->mem, disp_win);

          /* Initialize window */
          disp_win_init(Disp->mem, w, h);
     }

     /* Load */
     disp_win_copy(Disp->scr, Disp->mem, w, h);

     /* Assume change */
     for(y = 0; y < h; y++)
     {
          /* Assume change */
          Disp->x1[y] = 0;
          Disp->x2[y] = w - 1;
     }

     /* Assume change */
     Disp->y1 = 0;
     Disp->y2 = h - 1;

     /* Success */
     return (0);
}

/*
 * Exchange the "requested" screen with the "tmp" screen
 */
errr
Disp_exchange(void)
{
     int             y;
     int             w = Disp->wid;
     int             h = Disp->hgt;
     disp_win       *exchanger;

     /* Create */
     if(!Disp->tmp)
     {
          /* Allocate window */
          MAKE(Disp->tmp, disp_win);

          /* Initialize window */
          disp_win_init(Disp->tmp, w, h);
     }

     /* Swap */
     exchanger = Disp->scr;
     Disp->scr = Disp->tmp;
     Disp->tmp = exchanger;

     /* Assume change */
     for(y = 0; y < h; y++)
     {
          /* Assume change */
          Disp->x1[y] = 0;
          Disp->x2[y] = w - 1;
     }

     /* Assume change */
     Disp->y1 = 0;
     Disp->y2 = h - 1;

     /* Success */
     return (0);
}

/*
 * React to a new physical window size.
 */
errr
Disp_resize(int w,
            int h)
{
     int             i;
     int             wid, hgt;
     byte           *hold_x1;
     byte           *hold_x2;
     disp_win       *hold_old;
     disp_win       *hold_scr;
     disp_win       *hold_mem;
     disp_win       *hold_tmp;

     /* Resizing is forbidden */
     if(Disp->fixed_shape)
          return (-1);

     /* Ignore illegal changes */
     if((w < 1) || (h < 1))
          return (-1);

     /* Ignore non-changes */
     if((Disp->wid == w) && (Disp->hgt == h))
          return (1);

     /* Minimum dimensions */
     wid = MIN(Disp->wid, w);
     hgt = MIN(Disp->hgt, h);

     /* Save scanners */
     hold_x1 = Disp->x1;
     hold_x2 = Disp->x2;

     /* Save old window */
     hold_old = Disp->old;

     /* Save old window */
     hold_scr = Disp->scr;

     /* Save old window */
     hold_mem = Disp->mem;

     /* Save old window */
     hold_tmp = Disp->tmp;

     /* Create new scanners */
     C_MAKE(Disp->x1, h, byte);
     C_MAKE(Disp->x2, h, byte);

     /* Create new window */
     MAKE(Disp->old, disp_win);

     /* Initialize new window */
     disp_win_init(Disp->old, w, h);

     /* Save the contents */
     disp_win_copy(Disp->old, hold_old, wid, hgt);

     /* Create new window */
     MAKE(Disp->scr, disp_win);

     /* Initialize new window */
     disp_win_init(Disp->scr, w, h);

     /* Save the contents */
     disp_win_copy(Disp->scr, hold_scr, wid, hgt);

     /* If needed */
     if(hold_mem)
     {
          /* Create new window */
          MAKE(Disp->mem, disp_win);

          /* Initialize new window */
          disp_win_init(Disp->mem, w, h);

          /* Save the contents */
          disp_win_copy(Disp->mem, hold_mem, wid, hgt);
     }

     /* If needed */
     if(hold_tmp)
     {
          /* Create new window */
          MAKE(Disp->tmp, disp_win);

          /* Initialize new window */
          disp_win_init(Disp->tmp, w, h);

          /* Save the contents */
          disp_win_copy(Disp->tmp, hold_tmp, wid, hgt);
     }

     /* Free some arrays */
     FREE(hold_x1);
     FREE(hold_x2);

     /* Nuke */
     disp_win_nuke(hold_old);

     /* Kill */
     FREE(hold_old);

     /* Illegal cursor */
     if(Disp->old->cx >= w)
          Disp->old->cu = 1;
     if(Disp->old->cy >= h)
          Disp->old->cu = 1;

     /* Nuke */
     disp_win_nuke(hold_scr);

     /* Kill */
     FREE(hold_scr);

     /* Illegal cursor */
     if(Disp->scr->cx >= w)
          Disp->scr->cu = 1;
     if(Disp->scr->cy >= h)
          Disp->scr->cu = 1;

     /* If needed */
     if(hold_mem)
     {
          /* Nuke */
          disp_win_nuke(hold_mem);

          /* Kill */
          FREE(hold_mem);

          /* Illegal cursor */
          if(Disp->mem->cx >= w)
               Disp->mem->cu = 1;
          if(Disp->mem->cy >= h)
               Disp->mem->cu = 1;
     }

     /* If needed */
     if(hold_tmp)
     {
          /* Nuke */
          disp_win_nuke(hold_tmp);

          /* Kill */
          FREE(hold_tmp);

          /* Illegal cursor */
          if(Disp->tmp->cx >= w)
               Disp->tmp->cu = 1;
          if(Disp->tmp->cy >= h)
               Disp->tmp->cu = 1;
     }

     /* Save new size */
     Disp->wid = w;
     Disp->hgt = h;

     /* Force "total erase" */
     Disp->total_erase = TRUE;

     /* Assume change */
     for(i = 0; i < h; i++)
     {
          /* Assume change */
          Disp->x1[i] = 0;
          Disp->x2[i] = w - 1;
     }

     /* Assume change */
     Disp->y1 = 0;
     Disp->y2 = h - 1;

     /* Success */
     return (0);
}

/*
 * Activate a new Disp (and deactivate the current Disp)
 *
 * This function is extremely important, and also somewhat bizarre.
 * It is the only function that should "modify" the value of "Disp".
 *
 * To "create" a valid "disp", one should do "disp_init(t)", then
 * set the various flags and hooks, and then do "Disp_activate(t)".
 */
errr
Disp_activate(disp * t)
{
     /* Hack -- already done */
     if(Disp == t)
          return (1);

     /* Deactivate the old Disp */
     if(Disp)
          Disp_xtra(DISP_XTRA_LEVEL, 0);

     /* Hack -- Call the special "init" hook */
     if(t && !t->active_flag)
     {
          /* Call the "init" hook */
          if(t->init_hook)
               (*t->init_hook) (t);

          /* Remember */
          t->active_flag = TRUE;

          /* Assume mapped */
          t->mapped_flag = TRUE;
     }

     /* Remember the Disp */
     Disp = t;

     /* Activate the new Disp */
     if(Disp)
          Disp_xtra(DISP_XTRA_LEVEL, 1);

     /* Success */
     return (0);
}

/*
 * Nuke a disp
 */
errr
disp_nuke(disp * t)
{
     /* Hack -- Call the special "nuke" hook */
     if(t->active_flag)
     {
          /* Call the "nuke" hook */
          if(t->nuke_hook)
               (*t->nuke_hook) (t);

          /* Remember */
          t->active_flag = FALSE;

          /* Assume not mapped */
          t->mapped_flag = FALSE;
     }

     /* Nuke "displayed" */
     disp_win_nuke(t->old);

     /* Kill "displayed" */
     KILL(t->old);

     /* Nuke "requested" */
     disp_win_nuke(t->scr);

     /* Kill "requested" */
     KILL(t->scr);

     /* If needed */
     if(t->mem)
     {
          /* Nuke "memorized" */
          disp_win_nuke(t->mem);

          /* Kill "memorized" */
          KILL(t->mem);
     }

     /* If needed */
     if(t->tmp)
     {
          /* Nuke "temporary" */
          disp_win_nuke(t->tmp);

          /* Kill "temporary" */
          KILL(t->tmp);
     }

     /* Free some arrays */
     KILL(t->x1);
     KILL(t->x2);

     /* Free the input queue */
     KILL(t->key_queue);

     /* Success */
     return (0);
}

/*
 * Initialize a disp, using a window of the given size.
 * Also prepare the "input queue" for "k" keypresses
 * By default, the cursor starts out "invisible"
 * By default, we "erase" using "black spaces"
 */
errr
disp_init(disp * t,
          int w,
          int h,
          int k)
{
     int             y;

     /* Wipe it */
     (void) WIPE(t, disp);

     /* Prepare the input queue */
     t->key_head = t->key_tail = 0;

     /* Determine the input queue size */
     t->key_size = k;

     /* Allocate the input queue */
     C_MAKE(t->key_queue, t->key_size, char);

     /* Save the size */
     t->wid = w;
     t->hgt = h;

     /* Allocate change arrays */
     C_MAKE(t->x1, h, byte);
     C_MAKE(t->x2, h, byte);

     /* Allocate "displayed" */
     MAKE(t->old, disp_win);

     /* Initialize "displayed" */
     disp_win_init(t->old, w, h);

     /* Allocate "requested" */
     MAKE(t->scr, disp_win);

     /* Initialize "requested" */
     disp_win_init(t->scr, w, h);

     /* Assume change */
     for(y = 0; y < h; y++)
     {
          /* Assume change */
          t->x1[y] = 0;
          t->x2[y] = w - 1;
     }

     /* Assume change */
     t->y1 = 0;
     t->y2 = h - 1;

     /* Force "total erase" */
     t->total_erase = TRUE;

     /* Default "blank" */
     t->attr_blank = 0;
     t->char_blank = ' ';

     /* Success */
     return (0);
}
