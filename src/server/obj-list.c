/*
 * File: obj-list.c
 * Purpose: Object list construction.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2013 Ben Semmler
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
 * Allocate a new object list.
 */
object_list_t *object_list_new(void)
{
	object_list_t *list = mem_zalloc(sizeof(object_list_t));
	size_t size = MAX_ITEMLIST;

	if (list == NULL) return NULL;

	list->entries = mem_zalloc(size * sizeof(object_list_entry_t));

	if (list->entries == NULL)
    {
		mem_free(list);
		return NULL;
	}

	list->entries_size = size;

	return list;
}


/*
 * Free an object list.
 */
void object_list_free(object_list_t *list)
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
 * Initialize the object list module.
 */
void object_list_init(struct player *p)
{
	p->object_list_subwindow = NULL;
}


/*
 * Tear down the object list module.
 */
void object_list_finalize(struct player *p)
{
	object_list_free((object_list_t *)p->object_list_subwindow);
}


/*
 * Return a common object list instance.
 */
object_list_t *object_list_shared_instance(struct player *p)
{
	if (p->object_list_subwindow == NULL)
		p->object_list_subwindow = object_list_new();

	return (object_list_t *)p->object_list_subwindow;
}


/*
 * Return true if there is nothing preventing the list from being updated. This
 * should be for structural sanity checks and not gameplay checks.
 */
static bool object_list_can_update(object_list_t *list)
{
	if ((list == NULL) || (list->entries == NULL)) return false;

    return true;
}


/*
 * Zero out the contents of an object list.
 */
void object_list_reset(object_list_t *list)
{
	if ((list == NULL) || (list->entries == NULL)) return;

	memset(list->entries, 0, list->entries_size * sizeof(object_list_entry_t));
	memset(list->total_entries, 0, OBJECT_LIST_SECTION_MAX * sizeof(u16b));
	memset(list->total_objects, 0, OBJECT_LIST_SECTION_MAX * sizeof(u16b));
    list->distinct_entries = 0;
	list->sorted = false;
}


/*
 * Return true if the object should be omitted from the object list.
 */
static bool object_list_should_ignore_object(struct player *p, struct chunk *c,
    const struct object *obj)
{
	/* Make sure it's on the same dungeon level */
    if (!COORDS_EQUAL(&p->wpos, &obj->wpos)) return true;

	if (!is_unknown(obj) && ignore_item_ok(p, obj)) return true;

	if (tval_is_money(obj) || is_unknown_money(obj)) return true;

	return false;
}


/*
 * Collect object information from the current cave.
 */
void object_list_collect(struct player *p, object_list_t *list)
{
	int i, y, x;
    int py = p->py;
    int px = p->px;
    struct chunk *c = chunk_get(&p->wpos);

	if (!object_list_can_update(list)) return;

	/* Scan each object in the dungeon. */
	for (y = 1; y < c->height; y++)
    {
        for (x = 1; x < c->width; x++)
        {
            object_list_entry_t *entry;
            int entry_index;
            int field;
            bool los = false;
            struct object *obj = square_known_pile(p, c, y, x);

            /* Skip unfilled entries, unknown objects and monster-held objects */
            if (!obj) continue;

            /* Determine which section of the list the object entry is in */
            los = (projectable(c, py, px, y, x, PROJECT_NONE) || ((y == py) && (x == px)));
            field = (los? OBJECT_LIST_SECTION_LOS: OBJECT_LIST_SECTION_NO_LOS);

            for ( ; obj; obj = obj->next)
            {
                if (object_list_should_ignore_object(p, c, obj)) continue;

                /* Find or add a list entry. */
                entry = NULL;
                for (entry_index = 0; entry_index < (int)list->entries_size; entry_index++)
                {
                    int j;

                    /* We found an empty slot, so add this object here. */
                    if (list->entries[entry_index].object == NULL)
                    {
                        list->entries[entry_index].object = obj;
                        for (j = 0; j < OBJECT_LIST_SECTION_MAX; j++)
                            list->entries[entry_index].count[j] = 0;
                        list->entries[entry_index].dy = y - py;
                        list->entries[entry_index].dx = x - px;
                        list->entries[entry_index].player = p;
                        entry = &list->entries[entry_index];
                        break;
                    }

                    /* Use a matching object if we find one. */
                    if (!is_unknown(obj) &&
                        object_similar(p, obj, list->entries[entry_index].object, OSTACK_LIST))
                    {
                        /* We found a matching object and we'll use that. */
                        entry = &list->entries[entry_index];
                        break;
                    }
                }

                if (entry == NULL) return;

                /* We only know the number of objects we've actually seen */
                if (!is_unknown(obj))
                    entry->count[field] += obj->number;
                else
                    entry->count[field] = 1;
            }
        }
	}

	/* Collect totals for easier calculations of the list. */
	for (i = 0; i < (int)list->entries_size; i++)
    {
		if (list->entries[i].object == NULL) continue;

		if (list->entries[i].count[OBJECT_LIST_SECTION_LOS] > 0)
			list->total_entries[OBJECT_LIST_SECTION_LOS]++;

		if (list->entries[i].count[OBJECT_LIST_SECTION_NO_LOS] > 0)
			list->total_entries[OBJECT_LIST_SECTION_NO_LOS]++;

		list->total_objects[OBJECT_LIST_SECTION_LOS] +=
            list->entries[i].count[OBJECT_LIST_SECTION_LOS];
		list->total_objects[OBJECT_LIST_SECTION_NO_LOS] +=
            list->entries[i].count[OBJECT_LIST_SECTION_NO_LOS];
		list->distinct_entries++;
	}

	list->sorted = false;
}


/*
 * Object distance comparator: nearest to farthest.
 */
static int object_list_distance_compare(const void *a, const void *b)
{
	const object_list_entry_t *ae = (object_list_entry_t *)a;
	const object_list_entry_t *be = (object_list_entry_t *)b;
	int a_distance = ae->dy * ae->dy + ae->dx * ae->dx;
	int b_distance = be->dy * be->dy + be->dx * be->dx;

	if (a_distance < b_distance)
		return -1;
	else if (a_distance > b_distance)
		return 1;

	return 0;
}


/*
 * Standard comparison function for the object list. Uses compare_items().
 */
int object_list_standard_compare(const void *a, const void *b)
{
	int result;
	const struct object *ao = ((object_list_entry_t *)a)->object;
	const struct object *bo = ((object_list_entry_t *)b)->object;

	/* If this happens, something might be wrong in the collect function. */
	if ((ao == NULL) || (bo == NULL)) return 1;

	result = compare_items(((object_list_entry_t *)a)->player, ao, bo);

	/* If the objects are equivalent, sort nearest to farthest. */
	if (result == 0)
		result = object_list_distance_compare(a, b);

	return result;
}


/*
 * Sort the object list with the given sort function.
 */
void object_list_sort(object_list_t *list, int (*compare)(const void *, const void *))
{
	size_t elements;

	if ((list == NULL) || (list->entries == NULL)) return;

	if (list->sorted) return;

	elements = list->distinct_entries;

	if (elements <= 1) return;

	sort(list->entries, elements, sizeof(list->entries[0]), compare);
	list->sorted = true;
}


/*
 * Return an attribute to display a particular list entry with.
 *
 * entry is the object list entry to display.
 */
byte object_list_entry_line_attribute(struct player *p, const object_list_entry_t *entry)
{
	byte attr;

	if ((entry == NULL) || (entry->object == NULL) || (entry->object->kind == NULL))
		return COLOUR_WHITE;

    /* Unknown object */
    if (is_unknown(entry->object))
        attr = COLOUR_RED;

    /* Known artifact */
    else if (object_is_known_artifact(entry->object))
        attr = COLOUR_VIOLET;

    /* Unaware of kind */
    else if (!object_flavor_is_aware(p, entry->object))
        attr = COLOUR_L_RED;

    /* Worthless */
    else if (entry->object->kind->cost == 0)
        attr = COLOUR_SLATE;

    /* Default */
    else
        attr = COLOUR_WHITE;

	return attr;
}


/*
 * Format the object name so that the prefix is right aligned to a common column.
 *
 * This uses the default logic of object_desc() in order to handle flavors, artifacts,
 * vowels and so on. It was easier to do this and then use strtok() to break it up than
 * to do anything else.
 *
 * entry is the object list entry that has a name to be formatted.
 * line_buffer is the buffer to format into.
 * size is the size of line_buffer.
 */
void object_list_format_name(struct player *p, const object_list_entry_t *entry,
    char *line_buffer, size_t size)
{
    char name[NORMAL_WID];
    const char *chunk;
    char *source;
    bool has_singular_prefix;
    bool los = false;
    int field;
    byte old_number;
    int py = p->py;
    int px = p->px;
    int iy;
    int ix;
    bool object_is_recognized_artifact;
    struct chunk *c = chunk_get(&p->wpos);

    if ((entry == NULL) || (entry->object == NULL) || (entry->object->kind == NULL))
        return;

    iy = entry->object->iy;
    ix = entry->object->ix;
    object_is_recognized_artifact = (entry->object->artifact &&
        (entry->object->known->artifact || object_is_known(p, entry->object)));

    /* Hack -- these don't have a prefix when there is only one, so just pad with a space. */
    switch (entry->object->kind->tval)
    {
        case TV_SOFT_ARMOR:
            has_singular_prefix = (entry->object->kind->sval == lookup_sval(TV_SOFT_ARMOR, "Robe"));
            break;
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
            has_singular_prefix = false;
            break;
        default:
            has_singular_prefix = true;
    }
    if (object_is_recognized_artifact || is_unknown(entry->object))
        has_singular_prefix = true;

    /* Work out if the object is in view */
    los = (projectable(c, py, px, iy, ix, PROJECT_NONE) || ((iy == py) && (ix == px)));
    field = (los? OBJECT_LIST_SECTION_LOS: OBJECT_LIST_SECTION_NO_LOS);

    /* Hack -- we need to set object number to total count */
    old_number = entry->object->number;
    entry->object->number = entry->count[field];

    object_desc(p, name, sizeof(name), entry->object, ODESC_PREFIX | ODESC_FULL);
    entry->object->number = old_number;

    /* The source string for strtok() needs to be set properly, depending on when we use it. */
    if (!has_singular_prefix && (entry->count[field] == 1))
    {
        chunk = " ";
        source = name;
    }
    else
    {
        chunk = strtok(name, " ");
        source = NULL;
    }

    /* leave 1 space between symbol and name ("the" case) */

    /* Right alight the prefix and clip. */
    strnfmt(line_buffer, size, "%3.3s ", chunk);

    /* Get the rest of the name and clip it to fit the max width. */
    chunk = strtok(source, "\0");
    my_strcat(line_buffer, chunk, size);
}
