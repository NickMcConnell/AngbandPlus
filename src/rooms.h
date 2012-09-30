/* CVS: Last edit by $Author: rr9 $ on $Date: 2000/01/26 15:02:00 $
 *
 * File: rooms.h
 * Purpose: Header file for rooms.c, used only in generate.c
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


/* Externs */
#ifdef ALLOW_CAVERNS_AND_LAKES
extern void build_lake(int type);
extern void build_cavern(void);
#endif /* ALLOW_CAVERNS_AND_LAKES */

extern bool room_build(int y0, int x0, int typ);
