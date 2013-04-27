/* File: score.h */

/*
 * Purpose: Highscore handling for Angband
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

extern bool open_scoretable(int mode);
extern void close_scoretable(void);

extern void show_scores(void);
extern void display_scores(int from, int to);
extern void top_twenty(void);
extern int enter_score(void);
extern int predict_score(void);
extern void print_tomb(void);
