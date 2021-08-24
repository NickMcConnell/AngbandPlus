/*
 * File: obj-list-ui.c
 * Purpose: Object list UI.
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
 * Format a section of the object list: a header followed by object list entry rows.
 *
 * This function will process each entry for the given section. It will display:
 * - object char;
 * - number of objects;
 * - object name (truncated, if needed to fit the line);
 * - object distance from the player (aligned to the right side of the list).
 * By passing in false to tb, the maximum line width of the section can be found.
 *
 * list is the object list to format.
 * tb is true to display info or false if only the dimensions need to be calculated.
 * section is the section of the object list to format.
 * lines_to_display are the number of entries to display (not including the header).
 * max_width is the maximum line width.
 * prefix is the beginning of the header; the remainder is appended with the number of objects.
 * show_others is used to append "other objects" to the header, after the number of objects.
 * max_width_result is returned with the width needed to format the list without truncation.
 */
static void object_list_format_section(struct player *p, const object_list_t *list, bool tb,
    object_list_section_t section, int lines_to_display, int max_width, const char *prefix,
    bool show_others, size_t *max_width_result)
{
	int remaining_object_total = 0;
	int line_count = 0;
	int index;
	int total;
	char line_buffer[200];
	const char *punctuation = ((lines_to_display == 0)? ".": ":");
    const char *others = (show_others? "other ": "");
	size_t max_line_length = 0;

	if ((list == NULL) || (list->entries == NULL)) return;

	total = list->distinct_entries;

	if (list->total_objects[section] == 0)
    {
		max_line_length = strnfmt(line_buffer, sizeof(line_buffer), "%s no objects.\n", prefix);

		if (tb)
			text_out(p, "%s", line_buffer);

		/* Force a minimum width so that the prompt doesn't get cut off. */
		if (max_width_result != NULL)
			*max_width_result = MAX(max_line_length, 40);

		return;
	}

	max_line_length = strnfmt(line_buffer, sizeof(line_buffer), "%s %d %sobject%s%s\n", prefix,
        list->total_objects[section], others, PLURAL(list->total_objects[section]), punctuation);

	if (tb)
		text_out(p, "%s", line_buffer);

	for (index = 0; ((index < total) && (line_count < lines_to_display)); index++)
    {
		char location[20] = {'\0'};
		byte line_attr;
		size_t full_width;
		const char *direction_y = ((list->entries[index].dy <= 0)? "N": "S");
		const char *direction_x = ((list->entries[index].dx <= 0)? "W": "E");

		line_buffer[0] = '\0';

		if (list->entries[index].count[section] == 0)
			continue;

		/* Build the location string. */
		strnfmt(location, sizeof(location), " %d %s %d %s", abs(list->entries[index].dy),
            direction_y, abs(list->entries[index].dx), direction_x);

		/*
         * Get width available for object name: 2 for char and space; location includes padding;
         * last -1 for some reason?
         */
        full_width = max_width - 2 - utf8_strlen(location) - 1;

		/* Add the object count and clip the object name to fit. */
        object_list_format_name(p, &list->entries[index], line_buffer, sizeof(line_buffer));
        utf8_clipto(line_buffer, full_width);

		/*
         * Calculate the width of the line for dynamic sizing; use a fixed max width for location
         * and object char.
         */
		max_line_length = MAX(max_line_length, utf8_strlen(line_buffer) + 12 + 2);

		/* Display the pict */
		if (tb)
        {
			byte a;
            char c;

            if (!is_unknown(list->entries[index].object))
            {
                if (p->tile_distorted)
                {
                    a = list->entries[index].object->kind->d_attr;
                    c = list->entries[index].object->kind->d_char;

                    if (list->entries[index].object->kind->flavor &&
                        !(p->obj_aware[list->entries[index].object->kind->kidx] && a && c))
                    {
                        a = list->entries[index].object->kind->flavor->d_attr;
                        c = list->entries[index].object->kind->flavor->d_char;
                    }
                }
                else
                {
                    /* Always collect the latest object attribute so that flicker animation works. */
                    if (list->entries[index].object->attr)
                        a = list->entries[index].object->attr;
                    else
                        a = object_attr(p, list->entries[index].object);
                    c = object_char(p, list->entries[index].object);
                }

                /* Multi-hued object */
                if (a == COLOUR_MULTI) a = COLOUR_VIOLET;
            }
            else if (p->tile_distorted)
            {
                a = unknown_item_kind->d_attr;
                c = unknown_item_kind->d_char;
            }
            else
            {
                a = object_kind_attr(p, unknown_item_kind);
                c = object_kind_char(p, unknown_item_kind);
            }

            text_out_c(p, a, "%c", c);
            text_out(p, " ");
		}

		/* Add the left-aligned and padded object name which will align the location to the right. */
		if (tb)
        {
			full_width += strlen(line_buffer) - utf8_strlen(line_buffer);
            line_attr = object_list_entry_line_attribute(p, &list->entries[index]);
			text_out_c(p, line_attr, "%-*s%s\n", full_width, line_buffer, location);
		}

		line_count++;
	}

	/* Don't worry about the "...others" line, since it's probably shorter than what's already printed. */
	if (max_width_result != NULL)
		*max_width_result = max_line_length;

	/*
     * Bail since we don't have enough room to display the remaining count or since we've displayed
     * them all.
     */
	if ((lines_to_display <= 0) || (lines_to_display >= list->total_entries[section]))
		return;

	/* Sum the remaining objects; start where we left off in the above loop. */
	while (index < total)
    {
		remaining_object_total += list->entries[index].count[section];
		index++;
	}

	/* Count the remaining objects, starting where we left off in the above loop. */
	/*remaining_object_total = total - index;*/

	if (tb)
		text_out(p, "%6s...and %d others.\n", " ", remaining_object_total);
}


/*
 * Allow the standard list formatted to be bypassed for special cases.
 *
 * Returning true will bypass any other formatteding in object_list_format_textblock().
 *
 * list is the object list to format.
 * tb is true to display info or false if only the dimensions need to be calculated.
 * max_lines is the maximum number of lines that can be displayed.
 * max_width is the maximum line width that can be displayed.
 * max_height_result is returned with the number of lines needed to format the list without truncation.
 * max_width_result is returned with the width needed to format the list without truncation.
 */
static bool object_list_format_special(struct player *p, const object_list_t *list, bool tb,
    int max_lines, int max_width, size_t *max_height_result, size_t *max_width_result)
{
	if (p->timed[TMD_IMAGE] > 0)
    {
		/* Hack -- message needs newline to calculate width properly. */
		const char *message = "Your hallucinations are too wild to see things clearly.\n";

		if (max_height_result != NULL)
			*max_height_result = 1;

		if (max_width_result != NULL)
			*max_width_result = strlen(message);

		if (tb)
			text_out_c(p, COLOUR_ORANGE, "%s", message);

		return true;
	}

	return false;
}


/*
 * Format the entire object list with the given parameters.
 *
 * This function can be used to calculate the preferred dimensions for the list by passing in
 * false to tb. This function calls object_list_format_special() first; if that function
 * returns true, it will bypass normal list formatting.
 *
 * list is the object list to format.
 * tb is true to display info or false if only the dimensions need to be calculated.
 * max_lines is the maximum number of lines that can be displayed.
 * max_width is the maximum line width that can be displayed.
 * max_height_result is returned with the number of lines needed to format the list without truncation.
 * max_width_result is returned with the width needed to format the list without truncation.
 */
static void object_list_format_textblock(struct player *p, const object_list_t *list, bool tb,
    int max_lines, int max_width, size_t *max_height_result, size_t *max_width_result)
{
	int header_lines = 1;
	int lines_remaining;
	int los_lines_to_display;
	int no_los_lines_to_display;
	size_t max_los_line = 0;
	size_t max_no_los_line = 0;

	if ((list == NULL) || (list->entries == NULL))
		return;

	if (object_list_format_special(p, list, tb, max_lines, max_width, max_height_result,
        max_width_result))
    {
		return;
    }

	los_lines_to_display = list->total_entries[OBJECT_LIST_SECTION_LOS];
    no_los_lines_to_display = list->total_entries[OBJECT_LIST_SECTION_NO_LOS];

	if (list->total_entries[OBJECT_LIST_SECTION_NO_LOS] > 0)
		header_lines += 2;

	if (max_height_result != NULL)
		*max_height_result = header_lines + los_lines_to_display + no_los_lines_to_display;

	lines_remaining = max_lines - header_lines - list->total_entries[OBJECT_LIST_SECTION_LOS];

	/* Remove non-los lines as needed. */
	if (lines_remaining < list->total_entries[OBJECT_LIST_SECTION_NO_LOS])
		no_los_lines_to_display = MAX(lines_remaining - 1, 0);

	/*
     * If we don't even have enough room for the NO_LOS header, start removing
     * LOS lines, leaving one for the "...others".
     */
	if (lines_remaining < 0)
    {
		los_lines_to_display = list->total_entries[OBJECT_LIST_SECTION_LOS] -
            abs(lines_remaining) - 1;
    }

	/* Display only headers if we don't have enough space. */
	if (header_lines >= max_lines)
    {
		los_lines_to_display = 0;
		no_los_lines_to_display = 0;
	}

	object_list_format_section(p, list, tb, OBJECT_LIST_SECTION_LOS, los_lines_to_display,
        max_width, "You can see", false, &max_los_line);

	if (list->total_entries[OBJECT_LIST_SECTION_NO_LOS] > 0)
    {
		bool show_others = (list->total_objects[OBJECT_LIST_SECTION_LOS] > 0);

		if (tb) text_out(p, "\n");

		object_list_format_section(p, list, tb, OBJECT_LIST_SECTION_NO_LOS,
            no_los_lines_to_display, max_width, "You are aware of", show_others, &max_no_los_line);
	}

	if (max_width_result != NULL)
		*max_width_result = MAX(max_los_line, max_no_los_line);
}


/*
 * Display the object list statically. This will force the list to be displayed to
 * the provided dimensions. Contents will be adjusted accordingly.
 *
 * In order to be more efficient, this function uses a shared list object so that
 * it's not constantly allocating and freeing the list.
 *
 * height is the height of the list.
 * width is the width of the list.
 */
void object_list_show_subwindow(struct player *p, int height, int width)
{
	object_list_t *list;

    if ((height < 1) || (width < 1)) return;

    list = object_list_shared_instance(p);

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

	object_list_reset(list);
    object_list_collect(p, list);
	object_list_sort(list, object_list_standard_compare);

	/* Draw the list to exactly fit the subwindow. */
	object_list_format_textblock(p, list, true, height, width, NULL, NULL);

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}


/*
 * Display the object list interactively. This will dynamically size the list for
 * the best appearance. This should only be used in the main term.
 *
 * height is the height limit for the list.
 * width is the width limit for the list.
 */
void object_list_show_interactive(struct player *p, int height, int width)
{
	object_list_t *list;
	size_t max_width = 0, max_height = 0;
	int safe_width;

    if ((height < 1) || (width < 1)) return;

    list = object_list_new();

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

	object_list_collect(p, list);
	object_list_sort(list, object_list_standard_compare);

	/*
	 * Figure out optimal display rect. Large numbers are passed as the height and
	 * width limit so that we can calculate the maximum number of rows and columns
	 * to display the list nicely. We then adjust those values as needed to fit in
	 * the main term. Height is adjusted to account for the texblock prompt. The
	 * list is positioned on the right side of the term underneath the status line.
	 */
	object_list_format_textblock(p, list, false, 1000, 1000, &max_height, &max_width);
    safe_width = MIN(width, (int)max_width);

	/*
	 * Actually draw the list. We pass in max_height to the format function so that
	 * all lines will be appended to the textblock. The textblock itself will handle
	 * fitting it into the region. However, we have to pass safe_width so that the
	 * format function will pad the lines properly so that the location string is
	 * aligned to the right edge of the list.
	 */
	object_list_format_textblock(p, list, true, (int)max_height, safe_width, NULL, NULL);

	object_list_free(list);

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}
