/*
 * File: cmd-misc.c
 * Purpose: Deal with miscellaneous commands.
 *
 * Copyright (c) 2010 Andi Sidwell
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


#include "s-angband.h"


/*
 * Kill character
 */
void do_cmd_suicide(struct player *p)
{
    /* Mark as killed */
    p->alive = false;

    /* Note cause of death */
    my_strcpy(p->died_from, "self-inflicted wounds", sizeof(p->died_from));

    /* Record the original (pre-ghost) cause of death */
    if (p->ghost != 1) player_death_info(p, "self-inflicted wounds");

    /* Mark as quitter */
    if ((p->ghost != 1) && !p->total_winner) p->noscore = 1;

    if (p->total_winner) kingly(p);

    /* Kill him */
    player_death(p);
}
