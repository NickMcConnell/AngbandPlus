/* CVS: Last edit by $Author: rr9 $ on $Date: 1999/11/24 21:52:01 $
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

#if 0
extern void add_moss(void);
#endif

extern void add_river(int feat1, int feat2, int depth);
extern void build_streamer(int feat, int chance);
extern void build_streamer2(int feat, bool killwall, bool pool);
extern void destroy_level(void);
