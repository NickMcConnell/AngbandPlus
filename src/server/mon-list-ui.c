/*
 * File: mon-list-ui.c
 * Purpose: Monster list UI.
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
 * Format a section of the monster list: a header followed by monster list
 * entry rows.
 *
 * This function will process each entry for the given section. It will display:
 * - monster char;
 * - number of monsters;
 * - monster name (truncated, if needed to fit the line);
 * - whether or not the monster is asleep (and how many if in a group);
 * - monster distance from the player (aligned to the right side of the list).
 * By passing in false to tb, the maximum line width of the section can be found.
 *
 * list is the monster list to format.
 * tb is true to display info or false if only the dimensions need to be calculated.
 * section is the section of the monster list to format.
 * lines_to_display are the number of entries to display (not including the header).
 * max_width is the maximum line width.
 * prefix is the beginning of the header; the remainder is appended with the number of monsters.
 * show_others is used to append "other monsters" to the header, after the number of monsters.
 * max_width_result is returned with the width needed to format the list without truncation.
 */
static void monster_list_format_section(struct player *p, const monster_list_t *list, bool tb,
    monster_list_section_t section, int lines_to_display, int max_width, const char *prefix,
    bool show_others, size_t *max_width_result)
{
	int remaining_monster_total = 0;
	int line_count = 0;
	int index;
	int total;
	char line_buffer[200];
	const char *punctuation = ((lines_to_display == 0)? ".": ":");
	const char *others = (show_others? "other ": "");
    size_t max_line_length = 0;

	if ((list == NULL) || (list->entries == NULL)) return;

    total = list->distinct_entries;

	if (list->total_monsters[section] == 0)
    {
		max_line_length = strnfmt(line_buffer, sizeof(line_buffer), "%s no monsters.\n", prefix);

		if (tb)
            text_out(p, "%s", line_buffer);

        /* Force a minimum width so that the prompt doesn't get cut off. */
        if (max_width_result != NULL)
            *max_width_result = MAX(max_line_length, 40);

		return;
	}

	max_line_length = strnfmt(line_buffer, sizeof(line_buffer), "%s %d %smonster%s%s\n", prefix,
        list->total_monsters[section], others, PLURAL(list->total_monsters[section]), punctuation);

	if (tb)
        text_out(p, "%s", line_buffer);

	for (index = 0; ((index < total) && (line_count < lines_to_display)); index++)
    {
		char asleep[20] = {'\0'};
		char location[20] = {'\0'};
		byte line_attr;
		size_t full_width;
		size_t name_width;
        u16b count_in_section = 0;
        u16b asleep_in_section = 0;

		line_buffer[0] = '\0';

		if (list->entries[index].count[section] == 0)
			continue;

		/* Only display directions for the case of a single monster. */
		if (list->entries[index].count[section] == 1)
        {
			const char *direction1 = ((list->entries[index].dy[section] <= 0)? "N": "S");
			const char *direction2 = ((list->entries[index].dx[section] <= 0)? "W": "E");

			strnfmt(location, sizeof(location), " %d %s %d %s",
                abs(list->entries[index].dy[section]), direction1,
                abs(list->entries[index].dx[section]), direction2);
		}

        /*
         * Get width available for monster name and sleep tag: 2 for char and
         * space; location includes padding; last -1 for some reason?
         */
        full_width = max_width - 2 - utf8_strlen(location) - 1;

        asleep_in_section = list->entries[index].asleep[section];
        count_in_section = list->entries[index].count[section];

		if ((asleep_in_section > 0) && (count_in_section > 1))
			strnfmt(asleep, sizeof(asleep), " (%d asleep)", asleep_in_section);
		else if ((asleep_in_section == 1) && (count_in_section == 1))
			strnfmt(asleep, sizeof(asleep), " (asleep)");

		/* Clip the monster name to fit, and append the sleep tag. */
		name_width = MIN(full_width - utf8_strlen(asleep), sizeof(line_buffer));
		get_mon_name(line_buffer, sizeof(line_buffer), list->entries[index].race,
            list->entries[index].count[section]);
        utf8_clipto(line_buffer, name_width);
		my_strcat(line_buffer, asleep, sizeof(line_buffer));

		/*
         * Calculate the width of the line for dynamic sizing; use a fixed max
         * width for location and monster char.
         */
		max_line_length = MAX(max_line_length, utf8_strlen(line_buffer) + 12 + 2);

        /* Display the pict */
        if (tb)
        {
            char c;

            if (p->tile_distorted) c = list->entries[index].race->d_char;
            else c = p->r_char[list->entries[index].race->ridx];

            text_out_c(p, list->entries[index].attr, "%c", c);
            text_out(p, " ");
        }

		/*
         * Add the left-aligned and padded monster name which will align the
         * location to the right.
         */
		if (tb)
        {
			full_width += strlen(line_buffer) - utf8_strlen(line_buffer);
            line_attr = monster_list_entry_line_color(p, &list->entries[index]);
			text_out_c(p, line_attr, "%-*s%s\n", full_width, line_buffer, location);
		}

		line_count++;
	}

	/*
     * Don't worry about the "...others" line, since it's probably shorter
     * than what's already printed.
     */
	if (max_width_result != NULL)
		*max_width_result = max_line_length;

	/*
     * Bail since we don't have enough room to display the remaining count or
     * since we've displayed them all.
     */
	if ((lines_to_display <= 0) || (lines_to_display >= list->total_entries[section]))
		return;

	/* Sum the remaining monsters; start where we left off in the above loop. */
	while (index < total)
    {
		remaining_monster_total += list->entries[index].count[section];
		index++;
	}

	if (tb)
		text_out(p, "%6s...and %d others.\n", " ", remaining_monster_total);
}


/*
 * Allow the standard list formatted to be bypassed for special cases.
 *
 * Returning true will bypass any other formatting in monster_list_format_textblock().
 *
 * list is the monster list to format.
 * tb is true to display info or false if only the dimensions need to be calculated.
 * max_lines is the maximum number of lines that can be displayed.
 * max_width is the maximum line width that can be displayed.
 * max_height_result is returned with the number of lines needed to
 *   format the list without truncation.
 * max_width_result is returned with the width needed to format the list without truncation.
 */
static bool monster_list_format_special(struct player *p, const monster_list_t *list, bool tb,
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
 * Format the entire monster list with the given parameters.
 *
 * This function can be used to calculate the preferred dimensions for the list
 * by passing in false to tb. The LOS section of the list will always be
 * shown, while the other section will be added conditionally. Also, this function
 * calls monster_list_format_special() first; if that function returns true, it
 * will bypass normal list formatting.
 *
 * list is the monster list to format.
 * tb is true to display info or false if only the dimensions need to be calculated.
 * max_lines is the maximum number of lines that can be displayed.
 * max_width is the maximum line width that can be displayed.
 * max_height_result is returned with the number of lines needed to
 *   format the list without truncation.
 * max_width_result is returned with the width needed to format the list without truncation.
 */
static void monster_list_format_textblock(struct player *p, const monster_list_t *list, bool tb,
    int max_lines, int max_width, size_t *max_height_result, size_t *max_width_result)
{
	int header_lines = 1;
	int lines_remaining;
	int los_lines_to_display;
	int esp_lines_to_display;
	size_t max_los_line = 0;
	size_t max_esp_line = 0;

	if ((list == NULL) || (list->entries == NULL))
		return;

	if (monster_list_format_special(p, list, tb, max_lines, max_width, max_height_result,
        max_width_result))
    {
		return;
    }

    los_lines_to_display = list->total_entries[MONSTER_LIST_SECTION_LOS];
    esp_lines_to_display = list->total_entries[MONSTER_LIST_SECTION_ESP];

	if (list->total_entries[MONSTER_LIST_SECTION_ESP] > 0)
		header_lines += 2;

	if (max_height_result != NULL)
		*max_height_result = header_lines + los_lines_to_display + esp_lines_to_display;

	lines_remaining = max_lines - header_lines - list->total_entries[MONSTER_LIST_SECTION_LOS];

	/* Remove ESP lines as needed. */
	if (lines_remaining < list->total_entries[MONSTER_LIST_SECTION_ESP])
		esp_lines_to_display = MAX(lines_remaining - 1, 0);

	/*
     * If we don't even have enough room for the ESP header, start removing
     * LOS lines, leaving one for the "...others".
     */
	if (lines_remaining < 0)
    {
		los_lines_to_display = list->total_entries[MONSTER_LIST_SECTION_LOS] -
            abs(lines_remaining) - 1;
    }

	/* Display only headers if we don't have enough space. */
	if (header_lines >= max_lines)
    {
		los_lines_to_display = 0;
		esp_lines_to_display = 0;
	}

	monster_list_format_section(p, list, tb, MONSTER_LIST_SECTION_LOS, los_lines_to_display,
        max_width, "You can see", false, &max_los_line);

	if (list->total_entries[MONSTER_LIST_SECTION_ESP] > 0)
    {
		bool show_others = (list->total_monsters[MONSTER_LIST_SECTION_LOS] > 0);

		if (tb) text_out(p, "\n");

		monster_list_format_section(p, list, tb, MONSTER_LIST_SECTION_ESP, esp_lines_to_display,
            max_width, "You are aware of", show_others, &max_esp_line);
	}

	if (max_width_result != NULL)
		*max_width_result = MAX(max_los_line, max_esp_line);
}


/*
 * Display the monster list statically. This will force the list to be displayed to
 * the provided dimensions. Contents will be adjusted accordingly.
 *
 * In order to support more efficient monster flicker animations, this function uses
 * a shared list object so that it's not constantly allocating and freeing the list.
 *
 * height is the height of the list.
 * width is the width of the list.
 */
void monster_list_show_subwindow(struct player *p, int height, int width)
{
	monster_list_t *list;

    if ((height < 1) || (width < 1)) return;

    list = monster_list_shared_instance(p);

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

	monster_list_reset(p, list);
    monster_list_collect(p, list);
	monster_list_sort(list, monster_list_standard_compare);

	/* Draw the list to exactly fit the subwindow. */
	monster_list_format_textblock(p, list, true, height, width, NULL, NULL);

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}


/*
 * Display the monster list interactively. This will dynamically size the list for
 * the best appearance. This should only be used in the main term.
 *
 * height is the height limit for the list.
 * width is the width limit for the list.
 */
void monster_list_show_interactive(struct player *p, int height, int width)
{
	monster_list_t *list;
	size_t max_width = 0, max_height = 0;
	int safe_width;

    if ((height < 1) || (width < 1)) return;

    list = monster_list_new(p);

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

	monster_list_collect(p, list);
	monster_list_sort(list, monster_list_standard_compare);

	/*
	 * Figure out optimal display rect. Large numbers are passed as the height and
	 * width limit so that we can calculate the maximum number of rows and columns
	 * to display the list nicely. We then adjust those values as needed to fit in
	 * the main term. Height is adjusted to account for the texblock prompt. The
	 * list is positioned on the right side of the term underneath the status line.
	 */
	monster_list_format_textblock(p, list, false, 1000, 1000, &max_height, &max_width);
    safe_width = MIN(width, (int)max_width);

	/*
	 * Actually draw the list. We pass in max_height to the format function so that
	 * all lines will be appended to the textblock. The textblock itself will handle
	 * fitting it into the region. However, we have to pass safe_width so that the
	 * format function will pad the lines properly so that the location string is
	 * aligned to the right edge of the list.
	 */
	monster_list_format_textblock(p, list, true, (int)max_height, safe_width, NULL, NULL);

	monster_list_free(list);

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}