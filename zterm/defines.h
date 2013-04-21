/* File: defines.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Stig: This file will eventually be removed, you have been warned */


/*
 * OPTION: Maximum number of macros (see "io.c")
 * Default: assume at most 256 macros are used
 */
#define MACRO_MAX	256


/*** Macro Definitions ***/

/*
 * Hack -- Prepare to use the "Secure" routines
 */
#if defined(SET_UID) && defined(SECURE)
extern int PlayerUID;
# define getuid() PlayerUID
# define geteuid() PlayerUID
#endif



/*** Color constants ***/


/*
 * Angband "attributes" (with symbols, and base (R,G,B) codes)
 *
 * The "(R,G,B)" codes are given in "fourths" of the "maximal" value,
 * and should "gamma corrected" on most (non-Macintosh) machines.
 */
#define TERM_DARK		0	/* 'd' */	/* 0,0,0 */
#define TERM_WHITE		1	/* 'w' */	/* 4,4,4 */
#define TERM_SLATE		2	/* 's' */	/* 2,2,2 */
#define TERM_ORANGE		3	/* 'o' */	/* 4,2,0 */
#define TERM_RED		4	/* 'r' */	/* 3,0,0 */
#define TERM_GREEN		5	/* 'g' */	/* 0,2,1 */
#define TERM_BLUE		6	/* 'b' */	/* 0,0,4 */
#define TERM_UMBER		7	/* 'u' */	/* 2,1,0 */
#define TERM_L_DARK		8	/* 'D' */	/* 1,1,1 */
#define TERM_L_WHITE	9	/* 'W' */	/* 3,3,3 */
#define TERM_VIOLET		10	/* 'v' */	/* 4,0,4 */
#define TERM_YELLOW		11	/* 'y' */	/* 4,4,0 */
#define TERM_L_RED		12	/* 'R' */	/* 4,0,0 */
#define TERM_L_GREEN	13	/* 'G' */	/* 0,4,0 */
#define TERM_L_BLUE		14	/* 'B' */	/* 0,4,4 */
#define TERM_L_UMBER	15	/* 'U' */	/* 3,2,1 */
