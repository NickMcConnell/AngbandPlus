/*
 * File: mon-list.c
 * Purpose: Monster list construction.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2013 Ben Semmler
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


/*
 * Allocate a new monster list based on the size of the current cave's monster
 * array.
 */
monster_list_t *monster_list_new(struct player *p)
{
	monster_list_t *list = mem_zalloc(sizeof(monster_list_t));
	size_t size = cave_monster_max(chunk_get(p->depth));

	if (list == NULL) return NULL;

	list->entries = mem_zalloc(size * sizeof(monster_list_entry_t));

	if (list->entries == NULL)
    {
		mem_free(list);
		return NULL;
	}

	list->entries_size = size;

	return list;
}


/*
 * Free a monster list.
 */
void monster_list_free(monster_list_t *list)
{
	if (list == NULL) return;

	if (list->entries != NULL)
    {
		mem_free(list->entries);
		list->entries = NULL;
	}

	mem_free(list);
	list = NULL;
}


/*
 * Initialize the monster list module.
 */
void monster_list_init(struct player *p)
{
    p->monster_list_subwindow = NULL;
}


/*
 * Tear down the monster list module.
 */
void monster_list_finalize(struct player *p)
{
    monster_list_free((monster_list_t *)p->monster_list_subwindow);
}


/*
 * Return a common monster list instance.
 */
monster_list_t *monster_list_shared_instance(struct player *p)
{
    if (p->monster_list_subwindow == NULL)
        p->monster_list_subwindow = monster_list_new(p);

    return (monster_list_t *)p->monster_list_subwindow;
}


/*
 * Return true if there is nothing preventing the list from being updated. This
 * should be for structural sanity checks and not gameplay checks.
 */
static bool monster_list_can_update(monster_list_t *list, struct chunk *c)
{
    if ((list == NULL) || (list->entries == NULL)) return false;

    return ((int)list->entries_size >= cave_monster_max(c));
}


/*
 * Zero out the contents of a monster list. If needed, this function will
 * reallocate the entry list if the number of monsters has changed.
 */
void monster_list_reset(struct player *p, monster_list_t *list)
{
    struct chunk *c = chunk_get(p->depth);

    if ((list == NULL) || (list->entries == NULL)) return;

    if ((int)list->entries_size < cave_monster_max(c))
    {
        list->entries = mem_realloc(list->entries, sizeof(list->entries[0]) * cave_monster_max(c));
        list->entries_size = cave_monster_max(c);
    }

    memset(list->entries, 0, list->entries_size * sizeof(monster_list_entry_t));
    memset(list->total_entries, 0, MONSTER_LIST_SECTION_MAX * sizeof(u16b));
    memset(list->total_monsters, 0, MONSTER_LIST_SECTION_MAX * sizeof(u16b));
    list->distinct_entries = 0;
    list->sorted = false;
}


/*
 * Collect monster information from the current cave's monster list.
 */
void monster_list_collect(struct player *p, monster_list_t *list)
{
	int i;
    struct chunk *c = chunk_get(p->depth);

	if (!monster_list_can_update(list, c)) return;

	/* Use cave_monster_max() here in case the monster list isn't compacted. */
	for (i = 1; i < cave_monster_max(c); i++)
    {
		struct monster *mon = cave_monster(c, i);
		monster_list_entry_t *entry = NULL;
		int j, field;
		bool los = false;

        /* Skip dead monsters */
        if (!mon->race) continue;

		/* Only consider visible, known monsters */
        if (!mflag_has(p->mflag[i], MFLAG_VISIBLE) || mon->unaware) continue;

		/* Find or add a list entry. */
		for (j = 0; j < (int)list->entries_size; j++)
        {
			if (list->entries[j].race == NULL)
            {
				/* We found an empty slot, so add this race here. */
                entry = &list->entries[j];
                memset(entry, 0, sizeof(monster_list_entry_t));
				entry->race = mon->race;
				break;
			}
			else if (list->entries[j].race == mon->race)
            {
				/* We found a matching race and we'll use that. */
				entry = &list->entries[j];
				break;
			}
		}

		if (entry == NULL) continue;

        /* Always collect the latest monster attribute so that flicker animation works. */
        if (p->tile_distorted)
            entry->attr = mon->race->d_attr;
        else if (mon->attr)
            entry->attr = mon->attr;
        else
            entry->attr = p->r_attr[mon->race->ridx];

		/* Check for LOS using projectable() */
		los = (projectable(c, p->py, p->px, mon->fy, mon->fx, PROJECT_NONE) &&
            mflag_has(p->mflag[i], MFLAG_VIEW));
		field = (los? MONSTER_LIST_SECTION_LOS: MONSTER_LIST_SECTION_ESP);
		entry->count[field]++;

		if (mon->m_timed[MON_TMD_SLEEP] > 0)
			entry->asleep[field]++;

		/* Store the location offset from the player; this is only used for monster counts of 1 */
		entry->dx = mon->fx - p->px;
		entry->dy = mon->fy - p->py;
	}

	/* Collect totals for easier calculations of the list. */
	for (i = 0; i < (int)list->entries_size; i++)
    {
		if (list->entries[i].race == NULL) continue;

		if (list->entries[i].count[MONSTER_LIST_SECTION_LOS] > 0)
			list->total_entries[MONSTER_LIST_SECTION_LOS]++;

		if (list->entries[i].count[MONSTER_LIST_SECTION_ESP] > 0)
			list->total_entries[MONSTER_LIST_SECTION_ESP]++;

		list->total_monsters[MONSTER_LIST_SECTION_LOS] +=
            list->entries[i].count[MONSTER_LIST_SECTION_LOS];
		list->total_monsters[MONSTER_LIST_SECTION_ESP] +=
            list->entries[i].count[MONSTER_LIST_SECTION_ESP];
		list->distinct_entries++;
	}

    list->sorted = false;
}


/*
 * Standard comparison function for the monster list: sort by depth and then
 * power.
 */
int monster_list_standard_compare(const void *a, const void *b)
{
	const struct monster_race *ar = ((monster_list_entry_t *)a)->race;
	const struct monster_race *br = ((monster_list_entry_t *)b)->race;

	/* If this happens, something might be wrong in the collect function. */
	if ((ar == NULL) || (br == NULL)) return 1;

	/* Check depth first.*/
	if (ar->level > br->level) return -1;

	if (ar->level < br->level) return 1;

	/* Depths are equal, check power. */
	if (ar->power > br->power) return -1;

	if (ar->power < br->power) return 1;

	return 0;
}


/*
 * Sort the monster list with the given sort function.
 */
void monster_list_sort(monster_list_t *list, int (*compare)(const void *, const void *))
{
	size_t elements;

	if ((list == NULL) || (list->entries == NULL)) return;

    if (list->sorted) return;

	elements = list->distinct_entries;

	if (elements <= 1) return;

	sort(list->entries, MIN(elements, list->entries_size), sizeof(list->entries[0]), compare);
    list->sorted = true;
}


/*
 * Return a color to display a particular list entry with.
 *
 * entry is the monster list entry to display.
 */
byte monster_list_entry_line_color(struct player *p, const monster_list_entry_t *entry)
{
	/* Display uniques in a special colour */
	if (rf_has(entry->race->flags, RF_UNIQUE))
		return COLOUR_VIOLET;
	else if (entry->race->level > monster_level(p->depth))
		return COLOUR_RED;
	else
		return COLOUR_WHITE;
}
