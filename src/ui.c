#define UI_C
/* File: ui.c */

/* Purpose: User interface stuff. */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * At the moment, it contains functions to allow a prompt which contains a
 * large number of relatively short options, which are shrunk to fit.
 */

/*
 * Sorting hook for strip_names()
 *
 * Sort from highest frequency to lowest.
 */
static bool PURE ang_sort_comp_strip(vptr u, vptr v, int a, int b)
{
	int *has_sub = (int*)u;
	int *order = (int*)v;
	return (has_sub[order[a]] >= has_sub[order[b]]);
}

/*
 * Swapping hook for strip_names().
 */
static void ang_sort_swap_strip(vptr UNUSED u, vptr v, int a, int b)
{
	int *order = (int*)v;
	int temp;
	temp = order[a];
	order[a] = order[b];
	order[b] = temp;
}

/*
 * Return the first copy of t in s surrounded by space.
 * If there is none, return NULL.
 */
static char PURE *my_strword(cptr s, cptr t)
{
	cptr u;
	uint len = strlen(t);

	assert(s && t); /* Caller */

	for (u = s; u; u = strchr(u, ' '))
	{
		while (*u == ' ') u++;
		if (prefix(u, t) && (u[len] == ' ' || u[len] == '\0')) break;
	}

	return (char*)u;
}

/*
 * Remove common words from every elementof start in which they appear.
 * start should be an array of strings which this function is free to write to.
 * It will never do so in such a way as to make the string grow.
 */
static void strip_names(char **start, int num, uint len)
{
	char this[257];

	int i, j, k, l;
	char *sub[10];
	int has_sub[10], times[10], order[10];

	/* Look for prefixes to cut. */
	for (i = 0; i < num; i++)
	{
		/* Short enough already. */
		if (strlen(start[i]) < len) continue;

		/* Copy to temporary string. */
		sprintf(this, "%.256s", start[i]);

		/* Locate each word. */
		for (j = 0; j < 10; j++)
		{
			/* Find the end of the current word or the start of the string. */
			if (j)
				sub[j] = strchr(sub[j-1], ' ');
			else
				sub[j] = this;

			/* Set times and order to default values. */
			times[j] = 1;
			order[j] = j;
			has_sub[j] = 0;

			/* Don't continue past the end of the name. */
			if (!sub[j]) break;

			/* Don't change the string for the first word. */
			if (!j) continue;

			/* End last word and advance. */
			*(sub[j]++) = '\0';
		}

		/* Look for duplicated words. */
		for (k = 1; k < j; k++)
		{
			for (l = 0; l < k; l++)
			{
				/* Ignore different words. */
				if (strcmp(sub[l], sub[k])) continue;

				/* Increment the number of previous occurrences */
				times[k]++;
			}
		}
		/* Calculate how often each word appears in other entries. */
		for (k = 0; k < j; k++)
		{
			for (l = has_sub[k] = 0; l < num; l++)
			{
				cptr time;
				int m;
				for (m = 1, time = start[l];; m++)
				{
					/* Find the next copy. */
					time = my_strword(time, sub[k]);

					/* Not enough found. */
					if (!time) break;

					/* Don't consider this copy again. */
					time++;

					/* Go to the next copy. */
					if (m < times[k]) continue;

					/* This is a real substring. */
					has_sub[k]++;

					/* Finish. */
					break;
				}
			}
		}

		/* Sort the list. */
		ang_sort(has_sub, order, j, ang_sort_comp_strip, ang_sort_swap_strip);

		/* Remove words, most common first. */
		for (k = 0; k < j && strlen(start[i]) > len; k++)
		{
			cptr substr = sub[order[k]];

			/* Replace the word in every string in which it appears. */
			for (l = 0;l < num; l++)
			{
				char *b, *a = my_strword(start[l], substr);
				if (!a) continue;
				b = a + strlen(substr);
				while (*a) *++a = *b++;
			}
		}
	}
}

static int display_item_list(char **list, int num, bool truncate,
	int minx, int miny, int maxx, int maxy)
{
	int cx, cy, i, len;
	const int xr = maxx-minx+1, yr = maxy-miny+1;

	/* See how long the strings we'd like to use are. */
	for (i = len = 0; i < num; i++)
	{
		int l = strlen(list[i]);
		if (l > len) len = l;
	}

	/* Find out how many columns the display can be split into but still fit. */
	for (i = 0; xr / (i+1) > len+5; i++);

	/* Not enough, so allow truncation. */
	if (i * yr < num) i = (num + yr-1) / yr;

	/* Avoid menus which have more columns than rows. */
	else while (((i-1)*yr >= num) && (i > (num + i - 1) / i)) i--;

	/* Work out the column width and the number of rows. */
	cy = (num + i - 1) / i;
	cx = xr / i;

	/* Trim object lists in a more selective way than simple truncation. */
	if (!truncate) strip_names(list, num, cx - 5);

	/* Print everything. */
	for (i = 0; i < num; i++)
	{
		int ty = miny + (i % cy);
		int tx = minx + cx * (i / cy);

		mc_put_lfmt(ty, tx, cx-1, "[%c] %s", option_chars[i], list[i]);
	}

	return miny + cy;
}

/*
 * Display an item list stored in a name_centry array.
 * This copies everything to a separate array as strip_names() may change the
 * strings, but the strings within the name_centry are not changed.
 */
int display_entry_list_bounded(name_centry *list, int num, int truncate,
	int minx, int miny, int maxx, int maxy)
{
	int i;
	C_TNEW(tmp, num, char *);

	/* Copy the strings to a new string array. */
	for (i = 0; i < num; i++) tmp[i] = string_make(list[i].str);

	/* Display the list and store its length. */
	i = display_item_list(tmp, num, truncate, minx, miny, maxx, maxy);

	/* Clean up and return. */
	while (num--) FREE(tmp[num]);
	TFREE(tmp);
	return i;
}

/*
 * Display a name list at a standard location on the screen.
 */
int display_entry_list(name_centry *list, int num, int maxy, bool truncate)
{
	int i, j;

	Term_get_size(&i, &j);

	/* Display the list and store its length. */
	i = display_entry_list_bounded(list, num, truncate, 0, 2, i, j-maxy);

	return i;
}

/*
 * Given a list of categories (input as name_entries, although only the index
 * is used here), copy those for which at least one number in [0,items) matches
 * to list and return the number copied.
 */
int build_choice_list_1(name_entry *list, name_centry *start,
	int listm, int items, bool (*item_good)(int, int))
{
	int i;
	name_entry *this;
	for (this = list; this < list+listm && start->str; start++)
	{
		for (i = 0; i < items; i++)
		{
			if ((*item_good)(i, start->idx))
			{
				this->idx = start->idx;
				this->str = start->str;
				this++;
				break;
			}
		}
	}
	return this - list;
}

/*
 * Given an index, copy the index and print_f1 name of each number in [0,items)
 * matches to list and return the number copied.
 */
int build_choice_list_2(name_entry *list, int idx,
	int listm, int items, bool (*item_good)(int, int))
{
	int i;
	name_entry *this;
	for (this = list, i = 0; this < list+listm && i < items; i++)
	{
		if ((*item_good)(i, idx))
		{
			this->idx = i;
			this->str = NULL;
			this++;
		}
	}
	return this - list;
}
