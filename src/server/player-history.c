/*
 * File: player-history.c
 * Purpose: Character auto-history creation and management
 *
 * Copyright (c) 2007 J.D. White
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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


#define HISTORY_MAX 5000


/*
 * Initialize an empty history list
 */
void history_init(struct player *p)
{
    p->history_list = mem_zalloc(HISTORY_MAX * sizeof(struct history_info));
    p->history_ctr = p->history_size = 0;
}


/*
 * Clear any existing history
 */
void history_clear(struct player *p)
{
    mem_free(p->history_list);
}


/*
 * Wipe any existing history
 */
void history_wipe(struct player *p)
{
    memset(p->history_list, 0, HISTORY_MAX * sizeof(struct history_info));
    p->history_ctr = p->history_size = 0;
}


/*
 * Mark artifact number `id` as lost forever, either due to leaving it on a
 * level, or due to a store purging its inventory after the player sold it.
 */
bool history_lose_artifact(struct player *p, const struct object *obj)
{
    int i;
    char o_name[NORMAL_WID];

    my_assert(obj->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);

    for (i = 0; i < p->history_size; i++)
    {
        if ((p->history_list[i].a_idx == (byte)obj->artifact->aidx) &&
            streq(p->history_list[i].name, o_name) &&
            hist_has(p->history_list[i].type, HIST_ARTIFACT_KNOWN))
        {
            hist_wipe(p->history_list[i].type);
            hist_on(p->history_list[i].type, HIST_ARTIFACT_LOST);
            return true;
        }
    }

    return false;
}


/*
 * Add an entry with text `event` to the history list, with type `type`
 * ("HIST_xxx" in player-history.h), and artifact number `id` (0 for everything else).
 */
void history_add(struct player *p, const char *event, int type, const struct object *obj)
{
    byte a_idx = 0;
    char o_name[NORMAL_WID];

    o_name[0] = '\0';
    if (obj)
    {
        a_idx = obj->artifact->aidx;
        object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
    }

    /* Add an entry at the current counter location. */
    hist_wipe(p->history_list[p->history_ctr].type);
    hist_on(p->history_list[p->history_ctr].type, type);
    p->history_list[p->history_ctr].dlev = p->depth;
    p->history_list[p->history_ctr].clev = p->lev;
    p->history_list[p->history_ctr].a_idx = a_idx;
    my_strcpy(p->history_list[p->history_ctr].name, o_name,
        sizeof(p->history_list[0].name));
    p->history_list[p->history_ctr].turn = p->game_turn;
    my_strcpy(p->history_list[p->history_ctr].event, event,
        sizeof(p->history_list[0].event));

    p->history_ctr++;
    p->history_size++;

    /* Maintain a circular buffer */
    if (p->history_ctr == HISTORY_MAX) p->history_ctr = 0;
    if (p->history_size > HISTORY_MAX) p->history_size = HISTORY_MAX;
}


/*
 * Add a unique entry with text `event` to the history list, with type `type`
 */
void history_add_unique(struct player *p, const char *event, int type)
{
    int i;

    /* Check unicity */
    for (i = 0; i < p->history_size; i++)
    {
        if (streq(p->history_list[p->history_ctr].event, event))
            return;
    }

    history_add(p, event, type, NULL);
}


/*
 * Returns true if the artifact denoted by a_idx is an active entry in
 * the history log (i.e. is marked HIST_ARTIFACT_KNOWN). This permits
 * proper handling of the case where the player loses an artifact but (in
 * preserve mode) finds it again later.
 */
static bool history_is_artifact_logged(struct player *p, const struct object *obj)
{
    int i;
    char o_name[NORMAL_WID];

    my_assert(obj->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);

    for (i = 0; i < p->history_size; i++)
    {
        /*
         * Only count HISTORY_ARTIFACT_KNOWN entries; then we can handle
         * re-finding previously lost artifacts in preserve mode
         */
        if ((p->history_list[i].a_idx == (byte)obj->artifact->aidx) &&
            streq(p->history_list[i].name, o_name) &&
            hist_has(p->history_list[i].type, HIST_ARTIFACT_KNOWN))
        {
            return true;
        }
    }

    return false;
}


/*
 * Returns the index of the latest inactive entry for the artifact denoted by a_idx in
 * the history log (i.e. is marked HIST_ARTIFACT_UNKNOWN).
 */
static int history_latest(struct player *p, const struct object *obj)
{
    int i;
    char o_name[NORMAL_WID];

    my_assert(obj->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);

    /* Start with latest */
    i = p->history_ctr - 1;
    while (i >= 0)
    {
        if ((p->history_list[i].a_idx == (byte)obj->artifact->aidx) &&
            streq(p->history_list[i].name, o_name) &&
            hist_has(p->history_list[i].type, HIST_ARTIFACT_UNKNOWN))
        {
            return i;
        }

        i--;
    }

    /* Take into account the circular buffer */
    i = p->history_size - 1;
    while (i >= p->history_ctr)
    {
        if ((p->history_list[i].a_idx == (byte)obj->artifact->aidx) &&
            streq(p->history_list[i].name, o_name) &&
            hist_has(p->history_list[i].type, HIST_ARTIFACT_UNKNOWN))
        {
            return i;
        }

        i--;
    }

    return -1;
}


/*
 * Adding artifacts to the history list is trickier than other operations.
 * This is a wrapper function that gets some of the logic out of places
 * where it really doesn't belong. Call this to add an artifact to the history
 * list or make the history entry visible -- history_add_artifact will make that
 * determination depending on whether the object was found or not.
 */
bool history_add_artifact(struct player *p, const struct object *obj, bool found)
{
    char o_name[NORMAL_WID];
    char buf[NORMAL_WID];
    int i;

    my_assert(obj->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
    strnfmt(buf, sizeof(buf), (found? "Found %s": "Missed %s"), o_name);

    /* Only once */
    if (history_is_artifact_logged(p, obj)) return false;

    /* Player has found an artifact */
    if (found)
    {
        /* Find and wipe the latest unknown entry */
        i = history_latest(p, obj);
        if (i >= 0) hist_wipe(p->history_list[i].type);

        /* Add a new entry */
        history_add(p, buf, HIST_ARTIFACT_KNOWN, obj);
        return true;
    }

    /* Player has generated an artifact: only record "missed" entries if preserve=no */
    if (!cfg_preserve_artifacts)
    {
        history_add(p, buf, HIST_ARTIFACT_UNKNOWN, obj);
        return true;
    }

    return false;
}


/*
 * Convert all HIST_ARTIFACT_UNKNOWN history items to HIST_ARTIFACT_KNOWN.
 * Use only after player retirement/death for the final character dump.
 */
void history_unmask(struct player *p)
{
    int i;

    for (i = 0; i < p->history_size; i++)
    {
        if (hist_has(p->history_list[i].type, HIST_ARTIFACT_UNKNOWN))
        {
            hist_wipe(p->history_list[i].type);
            hist_on(p->history_list[i].type, HIST_ARTIFACT_KNOWN);
        }
    }
}
