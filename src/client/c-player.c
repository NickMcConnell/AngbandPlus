/*
 * File: c-player.c
 * Purpose: Player information (client side)
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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

#include "c-angband.h"

/* Pointer to the player struct */
struct player *player;
char title[NORMAL_WID];

/* Party information */
char party_info[160];

/* Are we looking at the full map? */
bool map_active;

/* Last line of info we've received */
s16b last_line_info = -1;

/* Special info display */
int special_line_type;
char special_line_header[ANGBAND_TERM_MAX][NORMAL_WID];

/* Roller */
s16b stat_roll[STAT_MAX + 1];

/* Party mode */
bool party_mode;

struct timed_grade *timed_grades[TMD_MAX];
