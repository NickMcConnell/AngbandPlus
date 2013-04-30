/*
 * File: cmd-misc.c
 * Purpose: Deal with miscellaneous commands.
 *
 * Copyright (c) 2010 Andi Sidwell
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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
#include "cmds.h"
#include "monster/mon-util.h"


/*
 * Display the main-screen monster list.
 */
void do_cmd_monlist(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Display visible monsters */
    display_monlist(p_ptr, TRUE);

    /* Notify player */
    notify_player(Ind, "Monster List", NTERM_WIN_MONLIST, TRUE);
}


/*
 * Display the main-screen item list.
 */
void do_cmd_itemlist(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Display visible monsters */
    display_itemlist(p_ptr, TRUE);

    /* Notify player */
    notify_player(Ind, "Object List", NTERM_WIN_OBJLIST, TRUE);
}