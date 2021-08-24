/*
 * File: player-history.c
 * Purpose: Character auto-history creation and management
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
 * Memory allocation constants.
 */
#define HISTORY_LEN_INIT 20
#define HISTORY_LEN_INCR 20


/*
 * Initialize an empty history list
 */
static void history_init(struct player_history *h)
{
    h->next = 0;
    h->length = HISTORY_LEN_INIT;
    h->entries = mem_zalloc(h->length * sizeof(*h->entries));
}


/*
 * Increase the history array size.
 */
static void history_realloc(struct player_history *h)
{
    h->length += HISTORY_LEN_INCR;
    h->entries = mem_realloc(h->entries, h->length * sizeof(*h->entries));
}


/*
 * Clear any existing history
 */
void history_clear(struct player *p)
{
    struct player_history *h = &p->hist;

    if (h->entries)
    {
        mem_free(h->entries);
        h->entries = NULL;
    }
    h->next = 0;
    h->length = 0;
}


/*
 * Add an entry to the history list.
 */
void history_add_full(struct player *p, struct history_info *entry)
{
    struct player_history *h = &p->hist;

    /* Allocate or expand the history list if needed */
    if (!h->entries)
        history_init(h);
    else if (h->next == h->length)
        history_realloc(h);

    /* Add entry */
    memcpy(&h->entries[h->next], entry, sizeof(struct history_info));

    h->next++;
}


/*
 * Add an entry to the history ledger with specified bitflags.
 */
static void history_add_with_flags(struct player *p, const char *text, bitflag flags[HIST_SIZE],
    const struct object *obj)
{
    struct artifact *art = NULL;
    char o_name[NORMAL_WID];
    struct history_info entry;

    o_name[0] = '\0';
    if (obj)
    {
        art = obj->artifact;
        object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
    }

    hist_copy(entry.type, flags);
    if (p->wpos.depth > 0)
        entry.dlev = p->wpos.depth;
    else if (in_town(&p->wpos))
        entry.dlev = 0;
    else
        entry.dlev = -1;
    entry.clev = p->lev;
    entry.art = art;
    my_strcpy(entry.name, o_name, sizeof(entry.name));
    ht_copy(&entry.turn, &p->game_turn);
    my_strcpy(entry.event, text, sizeof(entry.event));

    history_add_full(p, &entry);
}


/*
 * Adds an entry to the history ledger.
 */
void history_add(struct player *p, const char *text, int type)
{
    bitflag flags[HIST_SIZE];

    hist_wipe(flags);
    hist_on(flags, type);

    history_add_with_flags(p, text, flags, NULL);
}


/*
 * Returns true if the artifact is KNOWN in the history log.
 */
static bool history_is_artifact_known(struct player *p, const struct object *obj)
{
    struct player_history *h = &p->hist;
    size_t i = h->next;
    char o_name[NORMAL_WID];

    my_assert(obj->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);

    while (i--)
    {
        if (h->entries[i].art && (h->entries[i].art->aidx == obj->artifact->aidx) &&
            streq(h->entries[i].name, o_name) &&
            hist_has(h->entries[i].type, HIST_ARTIFACT_KNOWN))
        {
            return true;
        }
    }

    return false;
}


/*
 * Returns the index of the latest inactive entry for the artifact in
 * the history log (i.e. is marked HIST_ARTIFACT_UNKNOWN).
 */
static int history_latest_unknown(struct player *p, const struct object *obj)
{
    struct player_history *h = &p->hist;
    size_t i = h->next;
    char o_name[NORMAL_WID];

    my_assert(obj->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);

    while (i--)
    {
        if (h->entries[i].art && (h->entries[i].art->aidx == obj->artifact->aidx) &&
            streq(h->entries[i].name, o_name) &&
            hist_has(h->entries[i].type, HIST_ARTIFACT_UNKNOWN))
        {
            return i;
        }
    }

    return -1;
}


/*
 * Add an artifact to the history log.
 *
 * Call this to make the history entry visible.
 */
void history_find_artifact(struct player *p, const struct object *obj)
{
    my_assert(obj->artifact);

    /* Try revealing any existing artifact, otherwise log it */
    if (!history_is_artifact_known(p, obj))
    {
        char o_name[NORMAL_WID];
        char text[NORMAL_WID];
        int i;
        bitflag flags[HIST_SIZE];

        /* Description */
        object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
        strnfmt(text, sizeof(text), "Found %s", o_name);

        /* Find and wipe the latest unknown entry */
        i = history_latest_unknown(p, obj);
        if (i >= 0) hist_wipe(p->hist.entries[i].type);

        /* Add a new entry */
        hist_wipe(flags);
        hist_on(flags, HIST_ARTIFACT_KNOWN);
        history_add_with_flags(p, text, flags, obj);
    }
}


/*
 * Add an artifact to the history log.
 *
 * Call this to add an artifact to the history list.
 */
void history_generate_artifact(struct player *p, const struct object *obj)
{
    my_assert(obj->artifact);

    /* Player has generated an artifact: only record "missed" entries if preserve=no */
    if (!history_is_artifact_known(p, obj) && !cfg_preserve_artifacts)
    {
        char o_name[NORMAL_WID];
        char text[NORMAL_WID];
        bitflag flags[HIST_SIZE];

        /* Description */
        object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
        strnfmt(text, sizeof(text), "Missed %s", o_name);

        /* Add a new entry */
        hist_wipe(flags);
        hist_on(flags, HIST_ARTIFACT_UNKNOWN);
        history_add_with_flags(p, text, flags, obj);
    }
}


/*
 * Mark artifact number `id` as lost forever.
 */
void history_lose_artifact(struct player *p, const struct object *obj)
{
    struct player_history *h = &p->hist;
    size_t i = h->next;
    char o_name[NORMAL_WID];

    my_assert(obj->artifact);

    /* Description */
    object_desc(NULL, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);

    while (i--)
    {
        if (h->entries[i].art && (h->entries[i].art->aidx == obj->artifact->aidx) &&
            streq(h->entries[i].name, o_name) &&
            hist_has(h->entries[i].type, HIST_ARTIFACT_KNOWN))
        {
            hist_wipe(h->entries[i].type);
            hist_on(h->entries[i].type, HIST_ARTIFACT_LOST);
            return;
        }
    }
}


/*
 * Add a unique entry with text `event` to the history list, with type `type`
 */
void history_add_unique(struct player *p, const char *event, int type)
{
    struct player_history *h = &p->hist;
    size_t i = h->next;

    /* Check unicity */
    while (i--)
    {
        if (streq(h->entries[i].event, event))
            return;
    }

    history_add(p, event, type);
}


/*
 * Convert all HIST_ARTIFACT_UNKNOWN history items to HIST_ARTIFACT_KNOWN.
 * Use only after player retirement/death for the final character dump.
 */
void history_unmask_unknown(struct player *p)
{
    struct player_history *h = &p->hist;
    size_t i = h->next;

    while (i--)
    {
        if (hist_has(h->entries[i].type, HIST_ARTIFACT_UNKNOWN))
        {
            hist_wipe(h->entries[i].type);
            hist_on(h->entries[i].type, HIST_ARTIFACT_KNOWN);
        }
    }
}
