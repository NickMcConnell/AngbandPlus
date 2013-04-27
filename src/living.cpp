// File: living.cpp
// Purpose: Functions for livings

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"

void CLiving::correct_hp_overflows(void)
{
    if (chp >= mhp) {
        chp = mhp;
        chp_frac = 0;
    }
}
