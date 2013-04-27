/* File: wind_flg.h */

/*
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

#ifndef WIND_FLG_H
#define WIND_FLG_H 1

/*
 * Bit flags for the "p_ptr->window" variable (etc)
 */
#define PW_INVEN            0x00000001L /* Display inven/equip */	/* only for object1.c */
#define PW_EQUIP            0x00000002L /* Display equip/inven */	/* only for object1.c */
#define PW_MAP              0x00000020L /* Display dungeon map */	/* only for cave.c */

#endif
