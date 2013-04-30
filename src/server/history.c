/*
 * File: history.c
 * Purpose: Character auto-history creation, management, and display
 *
 * Copyright (c) 2007 J.D. White
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
#include "history.h"


#define HISTORY_MAX 5000


/*
 * Initialise an empty history list
 */
void history_init(player_type *p_ptr)
{
    p_ptr->history_list = C_ZNEW(HISTORY_MAX, history_info);
    p_ptr->history_ctr = p_ptr->history_size = 0;
}


/*
 * Clear any existing history
 */
void history_clear(player_type *p_ptr)
{
    mem_free(p_ptr->history_list);
}


/*
 * Wipe any existing history
 */
void history_wipe(player_type *p_ptr)
{
    C_WIPE(p_ptr->history_list, HISTORY_MAX, history_info);
    p_ptr->history_ctr = p_ptr->history_size = 0;
}


/*
 * Mark artifact number `id` as known.
 */
static void history_know_artifact(player_type *p_ptr, const object_type *o_ptr, size_t id)
{
    char o_name[NORMAL_WID];
    char buf[NORMAL_WID];

    /* Only the first time */
    if (p_ptr->history_list[id].type & HISTORY_ARTIFACT_KNOWN) return;

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);
    strnfmt(buf, sizeof(buf), "Found %s", o_name);

    p_ptr->history_list[id].type = HISTORY_ARTIFACT_KNOWN;
    p_ptr->history_list[id].dlev = p_ptr->depth;
    p_ptr->history_list[id].clev = p_ptr->lev;
    p_ptr->history_list[id].turn = p_ptr->game_turn;
    my_strcpy(p_ptr->history_list[id].event, buf, sizeof(p_ptr->history_list[0].event));
}


/*
 * Mark artifact number `id` as lost forever, either due to leaving it on a
 * level, or due to a store purging its inventory after the player sold it.
 */
bool history_lose_artifact(player_type *p_ptr, const object_type *o_ptr)
{
    size_t i;
    char o_name[NORMAL_WID];

    my_assert(o_ptr->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);

    for (i = 0; i < p_ptr->history_size; i++)
    {
        if ((p_ptr->history_list[i].a_idx == o_ptr->artifact->aidx) &&
            streq(p_ptr->history_list[i].name, o_name))
        {
            if (p_ptr->history_list[i].type & HISTORY_ARTIFACT_LOST) continue;
            p_ptr->history_list[i].type |= HISTORY_ARTIFACT_LOST;
            return TRUE;
        }
    }

    return FALSE;
}


/*
 * Add an entry with text `event` to the history list, with type `type`
 * ("HISTORY_xxx" in defines.h), and artifact number `id` (0 for everything else).
 */
void history_add(player_type *p_ptr, const char *event, u16b type, const object_type *o_ptr)
{
    byte a_idx = 0;
    char o_name[NORMAL_WID];

    o_name[0] = '\0';
    if (o_ptr)
    {
        a_idx = o_ptr->artifact->aidx;
        object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);
    }

    /* Add an entry at the current counter location. */
    p_ptr->history_list[p_ptr->history_ctr].type = type;
    p_ptr->history_list[p_ptr->history_ctr].dlev = p_ptr->depth;
    p_ptr->history_list[p_ptr->history_ctr].clev = p_ptr->lev;
    p_ptr->history_list[p_ptr->history_ctr].a_idx = a_idx;
    my_strcpy(p_ptr->history_list[p_ptr->history_ctr].name, o_name,
        sizeof(p_ptr->history_list[0].name));
    p_ptr->history_list[p_ptr->history_ctr].turn = p_ptr->game_turn;
    my_strcpy(p_ptr->history_list[p_ptr->history_ctr].event, event,
        sizeof(p_ptr->history_list[0].event));

    p_ptr->history_ctr++;
    p_ptr->history_size++;

    /* Maintain a circular buffer */
    if (p_ptr->history_ctr == HISTORY_MAX) p_ptr->history_ctr = 0;
    if (p_ptr->history_size > HISTORY_MAX) p_ptr->history_size = HISTORY_MAX;
}


void history_add_unique(player_type *p_ptr, const char *event, u16b type)
{
    size_t i;

    /* Check unicity */
    for (i = 0; i < p_ptr->history_size; i++)
    {
        if (streq(p_ptr->history_list[p_ptr->history_ctr].event, event))
            return;
    }

    history_add(p_ptr, event, type, NULL);
}


/*
 * Returns TRUE if the artifact denoted by a_idx is an active entry in
 * the history log (i.e. is not marked HISTORY_ARTIFACT_LOST).  This permits
 * proper handling of the case where the player loses an artifact but (in
 * preserve mode) finds it again later.
 */
static bool history_is_artifact_logged(player_type *p_ptr, const object_type *o_ptr, size_t *p_idx)
{
    size_t i;
    char o_name[NORMAL_WID];

    my_assert(o_ptr->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);

    for (i = 0; i < p_ptr->history_size; i++)
    {
        /*
         * Don't count ARTIFACT_LOST entries; then we can handle
         * re-finding previously lost artifacts in preserve mode
         */
        if (p_ptr->history_list[i].type & HISTORY_ARTIFACT_LOST) continue;

        if ((p_ptr->history_list[i].a_idx == o_ptr->artifact->aidx) &&
            streq(p_ptr->history_list[i].name, o_name))
        {
            if (p_idx) *p_idx = i;
            return TRUE;
        }
    }

    return FALSE;
}


/*
 * Adding artifacts to the history list is trickier than other operations.
 * This is a wrapper function that gets some of the logic out of places
 * where it really doesn't belong.  Call this to add an artifact to the history
 * list or make the history entry visible -- history_add_artifact will make that
 * determination depending on whether the object was found or not.
 */
bool history_add_artifact(player_type *p_ptr, const object_type *o_ptr, bool found)
{
    char o_name[NORMAL_WID];
    char buf[NORMAL_WID];
    size_t idx;

    my_assert(o_ptr->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_BASE);
    strnfmt(buf, sizeof(buf), (found? "Found %s": "Missed %s"), o_name);

    /* Known objects gets different treatment */
    if (found)
    {
        /* Try revealing any existing artifact, otherwise log it */
        if (history_is_artifact_logged(p_ptr, o_ptr, &idx))
            history_know_artifact(p_ptr, o_ptr, idx);
        else
            history_add(p_ptr, buf, HISTORY_ARTIFACT_KNOWN, o_ptr);
    }
    else
    {
        /* If we generated an artifact that previously had a history, then we lost it somehow */
        if (history_is_artifact_logged(p_ptr, o_ptr, &idx))
            p_ptr->history_list[idx].type |= HISTORY_ARTIFACT_LOST;

        /* Add a new entry */
        history_add(p_ptr, buf, HISTORY_ARTIFACT_UNKNOWN, o_ptr);
    }

    return TRUE;
}


/*
 * Convert all ARTIFACT_UNKNOWN history items to HISTORY_ARTIFACT_KNOWN.
 * Use only after player retirement/death for the final character dump.
 */
void history_unmask_unknown(player_type *p_ptr)
{
    size_t i;

    for (i = 0; i < p_ptr->history_size; i++)
    {
        if (p_ptr->history_list[i].type & HISTORY_ARTIFACT_UNKNOWN)
        {
            p_ptr->history_list[i].type &= ~(HISTORY_ARTIFACT_UNKNOWN);
            p_ptr->history_list[i].type |= HISTORY_ARTIFACT_KNOWN;
        }
    }
}


/*
 * Used to determine whether the history entry is visible in the listing or not.
 * Returns TRUE if the item is masked -- that is, if it is invisible
 */
static bool history_masked(player_type *p_ptr, size_t i)
{
    if (p_ptr->history_list[i].type & HISTORY_ARTIFACT_UNKNOWN) return TRUE;
    return FALSE;
}


/* Convert turn counter to real time */
void get_real_time(hturn *pturn, int* pd, int* ph, int* pm)
{
    int i;
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


/* Dump one character history entry to a file */
static void dump_entry(history_info *entry, ang_file *file)
{
    int days, hours, mins;
    char depths[8];

    get_real_time(&entry->turn, &days, &hours, &mins);
    if (!entry->dlev)
        my_strcpy(depths, "Town", sizeof(depths));
    else if (entry->dlev < 0)
        strnfmt(depths, sizeof(depths), "W%i", 0 - entry->dlev);
    else
        strnfmt(depths, sizeof(depths), "%ift", entry->dlev * 50);
    x_file_putf(file, "%02i:%02i:%02i   %-7s   %-2i    %s%s\n", days, hours, mins, depths,
        entry->clev, entry->event, ((entry->type & HISTORY_ARTIFACT_LOST)? " (LOST)": ""));
}


/*
 * Dump character history to a file, which we assume is already open.
 */
void dump_history(player_type *p_ptr, ang_file *file)
{
    size_t i;

    file_put(file, "Time       Depth     Level Event\n");
    for (i = p_ptr->history_ctr; i < p_ptr->history_size; i++)
    {
        /* Skip unknown artifacts */
        if (history_masked(p_ptr, i)) continue;

        dump_entry(&p_ptr->history_list[i], file);
    }
    for (i = 0; i < p_ptr->history_ctr; i++)
    {
        /* Skip unknown artifacts */
        if (history_masked(p_ptr, i)) continue;

        dump_entry(&p_ptr->history_list[i], file);
    }
    file_put(file, "\n\n");
}
