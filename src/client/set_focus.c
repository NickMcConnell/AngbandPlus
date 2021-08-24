/*
 * File: set_focus.c
 * Purpose: A gross hack to allow the client to scroll the dungeon display
 *
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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

#if defined(USE_GCU) || defined(USE_SDL)

/*
 * This is required for large graphical tiles where we cant have an
 * 66x22 tile display except in very high screen resolutions.
 * When the server supports player recentering this can go.
 *
 * When we receive a request to plot a tile at a location, we
 * shift the x-coordinate by this value. If the resultant
 * x-coordinate is negative we just ignore it and plot nothing.
 *
 * We only need scrolling along the x axis.
 */

/* Hack -- set focus to chat message control */
void set_chat_focus( void )
{
#ifdef __MSVC__
    old_focus = GetFocus();
    SetFocus(editmsg);
#endif 
}

void unset_chat_focus( void )
{
#ifdef __MSVC__
    /* Set focus back to original window */
    if (old_focus) SetFocus(old_focus);
#endif
}

void stretch_chat_ctrl( void )
{
#ifdef __MSVC__
    /* Resize the edit control */
    SetWindowPos(editmsg, 0, 2, data[PMSG_TERM].client_hgt - 21,
                 data[PMSG_TERM].client_wid - 6, 20,
                 SWP_NOZORDER);
#endif
}

#endif
