/* CVS: Last edit by $Author: sfuerst $ on $Date: 2000/09/25 11:27:04 $
 *
 * File: streams.h
 * Purpose: header file for streams.c. This is only used in generate.c.
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


/* Externs */
extern void add_river(int feat1, int feat2);
extern void build_streamer(int feat, int chance);
extern void place_trees(int x,int y);
extern void destroy_level(void);
extern void build_lake(int type);
extern void build_cavern(void);

