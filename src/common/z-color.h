/*
 * File: z-color.h
 * Purpose: Color constants
 */


#ifndef INCLUDED_Z_COLOR_H
#define INCLUDED_Z_COLOR_H


/*** Color constants ***/


/*
 * Angband "attributes" (with symbols, and base (R,G,B) codes)
 *
 * The "(R,G,B)" codes are given in "fourths" of the "maximal" value,
 * and should be "gamma corrected" on most (non-Macintosh) machines.
 */
#define TERM_DARK           0   /* d */     /* 0 0 0 */
#define TERM_WHITE          1   /* w */     /* 4 4 4 */
#define TERM_SLATE          2   /* s */     /* 2 2 2 */
#define TERM_ORANGE         3   /* o */     /* 4 2 0 */
#define TERM_RED            4   /* r */     /* 3 0 0 */
#define TERM_GREEN          5   /* g */     /* 0 2 1 */
#define TERM_BLUE           6   /* b */     /* 0 0 4 */
#define TERM_UMBER          7   /* u */     /* 2 1 0 */
#define TERM_L_DARK         8   /* D */     /* 1 1 1 */
#define TERM_L_WHITE        9   /* W */     /* 3 3 3 */
#define TERM_L_PURPLE       10  /* P */
#define TERM_YELLOW         11  /* y */     /* 4 4 0 */
#define TERM_L_RED          12  /* R */     /* 4 0 0 */
#define TERM_L_GREEN        13  /* G */     /* 0 4 0 */
#define TERM_L_BLUE         14  /* B */     /* 0 4 4 */
#define TERM_L_UMBER        15  /* U */     /* 3 2 1 */

#define TERM_PURPLE         16  /* p */
#define TERM_VIOLET         17  /* v */     /* 4 0 4 */
#define TERM_TEAL           18  /* t */
#define TERM_MUD            19  /* m */
#define TERM_L_YELLOW       20  /* Y */
#define TERM_MAGENTA        21  /* i */
#define TERM_L_TEAL         22  /* T */
#define TERM_L_VIOLET       23  /* V */
#define TERM_L_PINK         24  /* I */
#define TERM_MUSTARD        25  /* M */
#define TERM_BLUE_SLATE     26  /* z */
#define TERM_DEEP_L_BLUE    27  /* Z */


/*
 * The following allow color 'translations' to support environments with a limited color depth
 * as well as translate colours to alternates for e.g. menu highlighting.
 */
#define ATTR_FULL   0   /* full color translation */
#define ATTR_MONO   1   /* mono color translation */
#define ATTR_VGA    2   /* 16 color translation */
#define ATTR_BLIND  3   /* "Blind" color translation */
#define ATTR_LITE   4   /* "Torchlit" color translation */
#define ATTR_DARK   5   /* "Dark" color translation */
#define ATTR_HIGH   6   /* "Highlight" color translation */
#define ATTR_METAL  7   /* "Metallic" color translation */
#define ATTR_MISC   8   /* "Miscellaneous" color translation - see misc_to_attr */
#define ATTR_DOOR   9   /* "Door" color translation */

#define MAX_ATTR    10


/*
 * Maximum number of colours, and number of "basic" Angband colours
 */
#define MAX_COLORS      256
#define BASIC_COLORS    28


#endif
