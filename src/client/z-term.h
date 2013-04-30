/*
 * File: z-term.h
 * Purpose: A generic, efficient, terminal window package
 */

#ifndef INCLUDED_Z_TERM_H
#define INCLUDED_Z_TERM_H

#include "ui-event.h"

/*
 * A term_win is a "window" for a Term
 *
 *  - Cursor Useless/Visible codes
 *  - Cursor Location (see "Useless")
 *
 *  - Array[h] -- Access to the attribute array
 *  - Array[h] -- Access to the character array
 *
 *  - Array[h*w] -- Attribute array
 *  - Array[h*w] -- Character array
 *
 *  - next screen saved
 *  - hook to be called on screen size change
 *
 * Note that the attr/char pair at (x,y) is a[y][x]/c[y][x]
 * and that the row of attr/chars at (0,y) is a[y]/c[y]
 */
typedef struct _term_win
{
    bool cu, cv;
    byte cx, cy;
    byte **a;
    char **c;
    byte *va;
    char *vc;
    byte **ta;
    char **tc;
    byte *vta;
    char *vtc;
    struct _term_win *next;
} term_win;

/*
 * An actual "term" structure
 *
 *  - Extra "user" info (used by application)
 *
 *  - Extra "data" info (used by implementation)
 *
 *  - Flag "user_flag"
 *    An extra "user" flag (used by application)
 *
 *  - Flag "data_flag"
 *    An extra "data" flag (used by implementation)
 *
 *  - Flag "active_flag"
 *    This "term" is "active"
 *
 *  - Flag "mapped_flag"
 *    This "term" is "mapped"
 *
 *  - Flag "total_erase"
 *    This "term" should be fully erased
 *
 *  - Flag "fixed_shape"
 *    This "term" is not allowed to resize
 *
 *  - Flag "icky_corner"
 *    This "term" has an "icky" corner grid
 *
 *  - Flag "soft_cursor"
 *    This "term" uses a "software" cursor
 *
 *  - Flag "always_pict"
 *    Use the "Term_pict()" routine for all text
 *
 *  - Flag "higher_pict"
 *    Use the "Term_pict()" routine for special text
 *
 *  - Flag "always_text"
 *    Use the "Term_text()" routine for invisible text
 *
 *  - Flag "never_bored"
 *    Never call the "TERM_XTRA_BORED" action
 *
 *  - Flag "never_frosh"
 *    Never call the "TERM_XTRA_FROSH" action
 *
 *  - Value "attr_blank"
 *    Use this "attr" value for "blank" grids
 *
 *  - Value "char_blank"
 *    Use this "char" value for "blank" grids
 *
 *  - Flag "complex_input"
 *    Distinguish between Enter/^m/^j, Tab/^i, etc.
 *
 *  - Ignore this pointer
 *
 *  - Keypress Queue -- various data
 *
 *  - Keypress Queue -- pending keys
 *
 *  - Window Width (max 255)
 *  - Window Height (max 255)
 *
 *  - Minimum modified row
 *  - Maximum modified row
 *
 *  - Minimum modified column (per row)
 *  - Maximum modified column (per row)
 *
 *  - Displayed screen image
 *  - Requested screen image
 *
 *  - Temporary screen image
 *  - Memorized screen image
 *
 *  - Hook for init-ing the term
 *  - Hook for nuke-ing the term
 *
 *  - Hook for extra actions
 *
 *  - Hook for placing the cursor
 *
 *  - Hook for drawing some blank spaces
 *
 *  - Hook for drawing a string of chars using an attr
 *
 *  - Hook for drawing a sequence of special attr/char pairs
 */
typedef struct _term
{
    void *user;
    void *data;
    bool user_flag;
    bool data_flag;
    bool active_flag;
    bool mapped_flag;
    bool total_erase;
    bool fixed_shape;
    bool icky_corner;
    bool soft_cursor;
    bool always_pict;
    bool higher_pict;
    bool always_text;
    bool cursor_icky;
    bool double_cursor;
    bool no_cursor;
    bool never_bored;
    bool never_frosh;
    byte attr_blank;
    char char_blank;
    bool complex_input;
    ui_event *key_queue;
    u16b key_head;
    u16b key_tail;
    u16b key_length;
    u16b key_size, key_size_orig;
    byte wid;
    byte hgt, max_hgt;
    byte y1;
    byte y2;
    byte *x1;
    byte *x2;

    /* Offsets used by the map subwindows */
    byte offset_x;
    byte offset_y;

    term_win *old;
    term_win *scr;
    term_win *tmp;
    term_win *mem;

    /* Number of times saved */
    byte saved;

    /* Term is used to display a minimap */
    bool minimap_active;

    void (*init_hook)(struct _term *t);
    void (*nuke_hook)(struct _term *t);
    errr (*xtra_hook)(int n, int v);
    errr (*curs_hook)(int x, int y);
    errr (*bigcurs_hook)(int x, int y);
    errr (*wipe_hook)(int x, int y, int n);
    errr (*text_hook)(int x, int y, int n, byte a, const char *s);
    errr (*pict_hook)(int x, int y, int n, const byte *ap, const char *cp,
        const byte *tap, const char *tcp);
} term;

/**** Available Constants ****/

/*
 * Definitions for the "actions" of "Term_xtra()"
 *
 * These values may be used as the first parameter of "Term_xtra()",
 * with the second parameter depending on the "action" itself.  Many
 * of the actions shown below are optional on at least one platform.
 *
 * The "TERM_XTRA_EVENT" action uses "v" to "wait" for an event
 * The "TERM_XTRA_SHAPE" action uses "v" to "show" the cursor
 * The "TERM_XTRA_FROSH" action uses "v" for the index of the row
 * The "TERM_XTRA_LEVEL" action uses "v" to "resume" (or "suspend")
 * The "TERM_XTRA_DELAY" action uses "v" as a "millisecond" value
 *
 * The other actions do not need a "v" code, so "zero" is used.
 */
#define TERM_XTRA_EVENT 1   /* Process some pending events */
#define TERM_XTRA_FLUSH 2   /* Flush all pending events */
#define TERM_XTRA_CLEAR 3   /* Clear the entire window */
#define TERM_XTRA_SHAPE 4   /* Set cursor shape (optional) */
#define TERM_XTRA_FROSH 5   /* Flush one row (optional) */
#define TERM_XTRA_FRESH 6   /* Flush all rows (optional) */
#define TERM_XTRA_BORED 7   /* Handle stuff when bored (optional) */
#define TERM_XTRA_REACT 8   /* React to global changes (optional) */
#define TERM_XTRA_LEVEL 9   /* Change the "soft" level (optional) */
#define TERM_XTRA_DELAY 10  /* Delay some milliseconds (optional) */

/**** Available Variables ****/

extern term *Term;
extern byte tile_width;
extern byte tile_height;
extern bool tile_distorted;

/**** Available Functions ****/

extern errr Term_xtra(int n, int v);

extern void Term_queue_char(term *t, int x, int y, byte a, char c, byte ta, char tc);
extern void Term_big_queue_char(term *t, int x, int y, byte a, char c, byte a1, char c1);
extern void Term_queue_chars(int x, int y, int n, byte a, const char *s);

extern errr Term_fresh(void);
extern errr Term_set_cursor(bool v);
extern errr Term_gotoxy(int x, int y);
extern errr Term_draw(int x, int y, byte a, char c);
extern errr Term_addch(byte a, char c);
extern errr Term_addstr(int n, byte a, const char *buf);
extern errr Term_putch(int x, int y, byte a, char c);
extern void Term_big_putch(int x, int y, byte a, char c);
extern errr Term_putstr(int x, int y, int n, byte a, const char *s);
extern errr Term_erase(int x, int y, int n);
extern errr Term_erase_icky(int x, int y, int n);
extern errr Term_clear(void);
extern errr Term_redraw(void);
extern errr Term_redraw_section(int x1, int y1, int x2, int y2);
extern errr Term_mark(int x, int y);

extern errr Term_get_cursor(bool *v);
extern errr Term_get_size(int *w, int *h);
extern errr Term_locate(int *x, int *y);
extern errr Term_what(int x, int y, byte *a, char *c);
extern errr Term_info(int x, int y, byte *a, char *c, byte *ta, char *tc);

extern errr Term_flush(void);
extern errr Term_keypress(keycode_t k, byte mods);
extern errr Term_key_push(int k);
extern errr Term_event_push(const ui_event *ke);
extern errr Term_inkey(ui_event *ch, bool wait, bool take);

extern errr Term_save(void);
extern errr Term_load(void);

extern errr Term_resize(int w, int h, int hmax);

extern errr Term_activate(term *t);

extern errr term_nuke(term *t);
extern errr term_init(term *t, int w, int h, int hmax, int k);

extern void reset_tile_params(void);

#endif /* INCLUDED_Z_TERM_H */
