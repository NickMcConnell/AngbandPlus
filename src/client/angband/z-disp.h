
/* $Id: z-disp.h,v 1.6 2003/04/08 16:37:27 cipher Exp $ */

#ifndef INCLUDED_Z_DISP_H
#define INCLUDED_Z_DISP_H

/*
 * Copyright (c) 1997 Ben Harrison
 * Modifications: Copyright (c) 2003 Paul A. Schifferer
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "h-basic.h"

/*
 * A disp_win is a "window" for a Disp
 *
 *	- Cursor Useless/Visible codes
 *	- Cursor Location (see "Useless")
 *
 *	- Array[h] -- Access to the attribute array
 *	- Array[h] -- Access to the character array
 *
 *	- Array[h*w] -- Attribute array
 *	- Array[h*w] -- Character array
 *
 * Note that the attr/char pair at (x,y) is a[y][x]/c[y][x]
 * and that the row of attr/chars at (0,y) is a[y]/c[y]
 */

typedef struct disp_win disp_win;

struct disp_win
{
     bool            cu, cv;
     byte            cx, cy;

     byte          **a;
     char          **c;

     byte           *va;
     char           *vc;

     byte          **ta;
     char          **tc;

     byte           *vta;
     char           *vtc;
};

/*
 * An actual "disp" structure
 *
 *	- Extra "user" info (used by application)
 *
 *	- Extra "data" info (used by implementation)
 *
 *
 *	- Flag "user_flag"
 *	  An extra "user" flag (used by application)
 *
 *
 *	- Flag "data_flag"
 *	  An extra "data" flag (used by implementation)
 *
 *
 *	- Flag "active_flag"
 *	  This "disp" is "active"
 *
 *	- Flag "mapped_flag"
 *	  This "disp" is "mapped"
 *
 *	- Flag "total_erase"
 *	  This "disp" should be fully erased
 *
 *	- Flag "fixed_shape"
 *	  This "disp" is not allowed to resize
 *
 *	- Flag "soft_cursor"
 *	  This "disp" uses a "software" cursor
 *
 *	- Flag "always_pict"
 *	  Use the "Disp_pict()" routine for all text
 *
 *	- Flag "higher_pict"
 *	  Use the "Disp_pict()" routine for special text
 *
 *	- Flag "always_text"
 *	  Use the "Disp_text()" routine for invisible text
 *
 *	- Flag "unused_flag"
 *	  Reserved for future use
 *
 *	- Flag "never_bored"
 *	  Never call the "DISP_XTRA_BORED" action
 *
 *	- Flag "never_frosh"
 *	  Never call the "DISP_XTRA_FROSH" action
 *
 *
 *	- Value "attr_blank"
 *	  Use this "attr" value for "blank" grids
 *
 *	- Value "char_blank"
 *	  Use this "char" value for "blank" grids
 *
 *
 *	- Ignore this pointer
 *
 *	- Keypress Queue -- various data
 *
 *	- Keypress Queue -- pending keys
 *
 *
 *	- Window Width (max 255)
 *	- Window Height (max 255)
 *
 *	- Minimum modified row
 *	- Maximum modified row
 *
 *	- Minimum modified column (per row)
 *	- Maximum modified column (per row)
 *
 *
 *	- Displayed screen image
 *	- Requested screen image
 *
 *	- Temporary screen image
 *	- Memorized screen image
 *
 *
 *	- Hook for init-ing the disp
 *	- Hook for nuke-ing the disp
 *
 *	- Hook for user actions
 *
 *	- Hook for extra actions
 *
 *	- Hook for placing the cursor
 *
 *	- Hook for drawing some blank spaces
 *
 *	- Hook for drawing a string of chars using an attr
 *
 *	- Hook for drawing a sequence of special attr/char pairs
 */

typedef struct disp disp;

struct disp
{
     void           *user;

     void           *data;

     bool            user_flag;

     bool            data_flag;

     bool            active_flag;
     bool            mapped_flag;
     bool            total_erase;
     bool            fixed_shape;
     bool            icky_corner;
     bool            soft_cursor;
     bool            always_pict;
     bool            higher_pict;
     bool            always_text;
     bool            unused_flag;
     bool            never_bored;
     bool            never_frosh;

     byte            attr_blank;
     char            char_blank;

     char           *key_queue;

     u16b            key_head;
     u16b            key_tail;
     u16b            key_xtra;
     u16b            key_size;

     byte            wid;
     byte            hgt;

     byte            y1;
     byte            y2;

     byte           *x1;
     byte           *x2;

     disp_win       *old;
     disp_win       *scr;

     disp_win       *tmp;
     disp_win       *mem;

     void            (*init_hook) (disp * t);
     void            (*nuke_hook) (disp * t);

                     errr(*user_hook) (int n);

                     errr(*xtra_hook) (int n,
                                       int v);

                     errr(*curs_hook) (int x,
                                       int y);

                     errr(*bigcurs_hook) (int x,
                                          int y);

                     errr(*wipe_hook) (int x,
                                       int y,
                                       int n);

                     errr(*text_hook) (int x,
                                       int y,
                                       int n,
                                       byte a,
                                       cptr s);

                     errr(*pict_hook) (int x,
                                       int y,
                                       int n,
                                       const byte * ap,
                                       const char *cp,
                                       const byte * tap,
                                       const char *tcp);

                     errr(*param_hook) (int display,
                                        int param,
                                        void *value);

                     errr(*lock_hook) (void);

                     errr(*unlock_hook) (void);
};

/**** Available Constants ****/

/*
 * Definitions for the "actions" of "Disp_xtra()"
 *
 * These values may be used as the first parameter of "Disp_xtra()",
 * with the second parameter depending on the "action" itself.  Many
 * of the actions shown below are optional on at least one platform.
 *
 * The "DISP_XTRA_EVENT" action uses "v" to "wait" for an event
 * The "DISP_XTRA_SHAPE" action uses "v" to "show" the cursor
 * The "DISP_XTRA_FROSH" action uses "v" for the index of the row
 * The "DISP_XTRA_SOUND" action uses "v" for the index of a sound
 * The "DISP_XTRA_ALIVE" action uses "v" to "activate" (or "close")
 * The "DISP_XTRA_LEVEL" action uses "v" to "resume" (or "suspend")
 * The "DISP_XTRA_DELAY" action uses "v" as a "millisecond" value
 * The "DISP_XTRA_MUSIC" action uses "v" for the index of a video
 * The "DISP_XTRA_SCENE" action uses "v" as a "scene" value
 * The "DISP_XTRA_STAGE" action uses "v" as a scene's "stage" value
 * The "DISP_XTRA_TOGGL" action uses "v" for the overlay number
 * The "DISP_XTRA_HIDE" action uses "v" for the overlay number
 * The "DISP_XTRA_SHOW" action uses "v" for the overlay number
 *
 * The other actions do not need a "v" code, so "zero" is used.
 */
#define DISP_XTRA_EVENT 1       /* Process some pending events */
#define DISP_XTRA_FLUSH 2       /* Flush all pending events */
#define DISP_XTRA_CLEAR 3       /* Clear the entire window */
#define DISP_XTRA_SHAPE 4       /* Set cursor shape (optional) */
#define DISP_XTRA_FROSH 5       /* Flush one row (optional) */
#define DISP_XTRA_FRESH 6       /* Flush all rows (optional) */
#define DISP_XTRA_NOISE 7       /* Make a noise (optional) */
#define DISP_XTRA_SOUND 8       /* Make a sound (optional) */
#define DISP_XTRA_BORED 9       /* Handle stuff when bored (optional) */
#define DISP_XTRA_REACT 10      /* React to global changes (optional) */
#define DISP_XTRA_ALIVE 11      /* Change the "hard" level (optional) */
#define DISP_XTRA_LEVEL 12      /* Change the "soft" level (optional) */
#define DISP_XTRA_DELAY 13      /* Delay some milliseconds (optional) */
#define DISP_XTRA_MUSIC 14      /* Play a song (optional) */
#define DISP_XTRA_VIDEO 15      /* Play a video (optional) */
#define DISP_XTRA_SCENE 16      /* Change the display scene */
#define DISP_XTRA_STAGE 17      /* Change the display scene's stage */
#define DISP_XTRA_PREP  18      /* Prepare a special window for use */
#define DISP_XTRA_TOGGL 19      /* Toggle the display of a special window */
#define DISP_XTRA_SHOW  20      /* Explicitly display a special window */
#define DISP_XTRA_HIDE  21      /* Explicitly hide a special window */
#define DISP_XTRA_MSG   22      /* Display a message */

/**** Game "scenes" and "stages" ****/

#define SCENE_SPLASH                               1
#define SCENE_INTRO                                2
#define SCENE_TITLE                                3
#define SCENE_TITLE_STAGE_ICONS                    4
#define SCENE_TITLE_STAGE_TILES                    5
#define SCENE_TITLE_STAGE_MISC                     6
#define SCENE_TITLE_STAGE_COMPLETE                 7
#define SCENE_SELECT_CHARACTER                     8
#define SCENE_SELECT_VIDEO				      9
#define SCENE_NEW_CHARACTER                        10
#define SCENE_NEW_CHARACTER_STAGE_GENDER           11
#define SCENE_NEW_CHARACTER_STAGE_RACE             12
#define SCENE_NEW_CHARACTER_STAGE_CLASS            13
#define SCENE_NEW_CHARACTER_STAGE_OPTIONS_QUERY    14
#define SCENE_NEW_CHARACTER_STAGE_OPTIONS          15
#define SCENE_NEW_CHARACTER_STAGE_STATS_NORMAL     16
#define SCENE_NEW_CHARACTER_STAGE_STATS_POINTBASED 17
#define SCENE_NEW_CHARACTER_STAGE_STATS_AUTOROLLER 18
#define SCENE_NEW_CHARACTER_STAGE_STATS_APPROVE    19
#define SCENE_NEW_CHARACTER_STAGE_NAME             20
#define SCENE_NEW_CHARACTER_STAGE_FINALIZE         21
#define SCENE_MULTIPLAYER_HOST                     22
#define SCENE_MULTIPLAYER_JOIN                     23
#define SCENE_PLAY                                 24
#define SCENE_GRAVE                                25
#define SCENE_MAX                                  26

/**** Special display types ****/

#define DISPLAY_CHARACTER       1
#define DISPLAY_INVENTORY       2
#define DISPLAY_EQUIPMENT       3
#define DISPLAY_BOOK            4
#define DISPLAY_MEMORY          5
#define DISPLAY_MESSAGES        6
#define DISPLAY_OPPONENT        7
#define DISPLAY_STORE           8
#define DISPLAY_MESSAGE_BROWSER 9
#define DISPLAY_MAP             10
#define DISPLAY_OPTIONS         11
#define DISPLAY_DIALOG          12
#define DISPLAY_HELP            13
#define DISPLAY_ERROR           14
#define DISPLAY_MAX             15

/**** Parameter types for Disp_param() ****/

#define DISP_PARAM_RECALC     1
#define DISP_PARAM_UPDATE     2
#define DISP_PARAM_VAR1       3
#define DISP_PARAM_VAR2       4
#define DISP_PARAM_VAR3       5
#define DISP_PARAM_VAR4       6
#define DISP_PARAM_BUFFER1    7
#define DISP_PARAM_BUFFER2    8
#define DISP_PARAM_BUFLEN1    9
#define DISP_PARAM_BUFLEN2    10
#define DISP_PARAM_FLAGS      11

/**** Available Variables ****/

extern disp    *Disp;

/**** Available Functions ****/

extern errr     Disp_user(int n);
extern errr     Disp_xtra(int n,
                          int v);

extern void     Disp_queue_char(int x,
                                int y,
                                byte a,
                                char c,
                                byte ta,
                                char tc);
extern void     Disp_queue_chars(int x,
                                 int y,
                                 int n,
                                 byte a,
                                 cptr s);

extern errr     Disp_fresh(void);
extern errr     Disp_set_cursor(bool v);
extern errr     Disp_gotoxy(int x,
                            int y);
extern errr     Disp_draw(int x,
                          int y,
                          byte a,
                          char c);
extern errr     Disp_addch(byte a,
                           char c);
extern errr     Disp_addstr(int n,
                            byte a,
                            cptr s);
extern errr     Disp_putch(int x,
                           int y,
                           byte a,
                           char c);
extern errr     Disp_putstr(int x,
                            int y,
                            int n,
                            byte a,
                            cptr s);
extern errr     Disp_erase(int x,
                           int y,
                           int n);
extern errr     Disp_clear(void);
extern errr     Disp_redraw(void);
extern errr     Disp_redraw_section(int x1,
                                    int y1,
                                    int x2,
                                    int y2);

extern errr     Disp_get_cursor(bool * v);
extern errr     Disp_get_size(int *w,
                              int *h);
extern errr     Disp_locate(int *x,
                            int *y);
extern errr     Disp_what(int x,
                          int y,
                          byte * a,
                          char *c);

extern errr     Disp_flush(void);
extern errr     Disp_keypress(int k);
extern errr     Disp_key_push(int k);
extern errr     Disp_inkey(char *ch,
                           bool wait,
                           bool take);
extern errr     Disp_param(int display,
                           int param,
                           void *value);
extern errr     Disp_lock(void);
extern errr     Disp_unlock(void);

extern errr     Disp_save(void);
extern errr     Disp_load(void);

extern errr     Disp_exchange(void);

extern errr     Disp_resize(int w,
                            int h);

extern errr     Disp_activate(disp * t);

extern errr     disp_nuke(disp * t);
extern errr     disp_init(disp * t,
                          int w,
                          int h,
                          int k);

#endif
