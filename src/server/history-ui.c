/*
 * File: history-ui.c
 * Purpose: Character auto-history display UI
 *
 * Copyright (c) 2007 J.D. White
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


#include "s-angband.h"


/*
 * Used to determine whether the history entry is visible in the listing or not.
 * Returns true if the item is masked -- that is, if it is invisible
 */
static bool history_masked(struct player *p, size_t i)
{
    /* Empty entries */
    if (hist_is_empty(p->hist.entries[i].type)) return true;

    /* Missed artifacts */
    if (hist_has(p->hist.entries[i].type, HIST_ARTIFACT_UNKNOWN)) return true;

    return false;
}


/* Convert turn counter to real time */
void get_real_time(hturn *pturn, int* pd, int* ph, int* pm)
{
    u32b i;
    u32b seconds, turns;

    /* Convert turn counter to real time */
    turns = pturn->turn;
    seconds = turns / cfg_fps; turns = turns % cfg_fps;
    *pm = seconds / 60; seconds = seconds % 60;
    *ph = *pm / 60; *pm = *pm % 60;
    *pd = *ph / 24; *ph = *ph % 24;
    for (i = 1; i <= pturn->era; i++)
    {
        turns += HTURN_ERA_FLIP;
        seconds += turns / cfg_fps; turns = turns % cfg_fps;
        *pm += seconds / 60; seconds = seconds % 60;
        *ph += *pm / 60; *pm = *pm % 60;
        *pd += *ph / 24; *ph = *ph % 24;
    }
}


/*
 * Dump one character history entry to a file
 */
static void dump_entry(struct history_info *entry, ang_file *file)
{
    int days, hours, mins;
    char depths[8];

    get_real_time(&entry->turn, &days, &hours, &mins);
    if (entry->dlev > 0)
        strnfmt(depths, sizeof(depths), "%ift", entry->dlev * 50);
    else if (entry->dlev == 0)
        my_strcpy(depths, "Town", sizeof(depths));
    else
        my_strcpy(depths, "Wild", sizeof(depths));
    file_putf(file, "%02i:%02i:%02i   %-7s   %-2i    %s%s\n", days, hours, mins, depths,
        entry->clev, entry->event, (hist_has(entry->type, HIST_ARTIFACT_LOST)? " (LOST)": ""));
}


/*
 * Dump character history to a file, which we assume is already open.
 */
void dump_history(struct player *p, ang_file *file)
{
    int i;

    file_put(file, "Time       Depth     Level Event\n");
    for (i = 0; i < p->hist.next; i++)
    {
        /* Skip missed artifacts/empty entries */
        if (history_masked(p, i)) continue;

        dump_entry(&p->hist.entries[i], file);
    }
    file_put(file, "\n\n");
}