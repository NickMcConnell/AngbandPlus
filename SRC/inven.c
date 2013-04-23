#include "angband.h"


/*
 * Dynamic inventory display
 */
static char show_inven_aux(cptr name, cptr caption, int *line)
{
	int i, k;

	/* Number of "real" lines passed by */
	int next = 0;

	/* Number of "real" lines in the file */
	int size;

	/* Backup value for "line" */
	int back = 0;

	/* Inventory colors */
	int attr;

	/* Current inventory file */
	FILE *fff;

	char *t;

	/* Hold a string to show */
	char shower[128];

	/* General buffer */
	char buf[512];

	/* Temporary buffer */
	char tmp[512];

	/* Show the inventory */
	bool show = TRUE;

	/* Wipe shower */
	strcpy(shower, "");

	/* Open */
	fff = my_fopen(name, "r");

	/* Oops */
	if (!fff)
	{
		/* Message */
		msg_format("Cannot open '%s'!", name);
		msg_print(NULL);

		/* Oops */
		return ESCAPE;
	}


	/* Pre-Parse the file */
	while (TRUE)
	{
		/* Read a line or stop */
		if (my_fgets(fff, buf, 512)) break;

		/* Count the "real" lines */
		next++;
	}

	/* Save the number of "real" lines */
	size = next;



	/* Display the file */
	while (show)
	{
		/* Clear screen */
		Term_clear();


		/* Restart when necessary */
		if (*line >= size) *line = 0;


		/* Re-open the file if needed */
		if (next > *line)
		{
			/* Close it */
			my_fclose(fff);

			/* Hack -- Re-Open the file */
			fff = my_fopen(name, "r");

			/* Oops */
			if (!fff) return ESCAPE;

			/* File has been restarted */
			next = 0;
		}

		/* Skip lines if needed */
		for (; next < *line; next++)
		{
			/* Skip a line */
			if (my_fgets(fff, buf, 512)) break;
		}


		/* Dump the next 20 lines of the file */
		for (i = 0; i < 20; )
		{
			/* Hack -- track the "first" line */
			if (!i) *line = next;

			/* Get a line of the file or stop */
			if (my_fgets(fff, buf, 512)) break;

			/* Default color */
			attr = TERM_WHITE;

			t = buf;

			/* Process the color information */
			if (*t == '&')
			{
				attr = color_char_to_attr(*(t + 1));

				if (attr == -1)
				{
					/* Message */
					msg_format("Unknown color '%s'!", *(t + 1));
					msg_print(NULL);

					/* Oops */
					return ESCAPE;
				}

				t += 2;
			}

			/* Add labels */
			strcpy(tmp, format("%c) ", 'a' + i));
			strcat(tmp, t);

			strcpy(buf, tmp);

			/* Count the lines */
			next++;

			/* Dump the line */
			Term_putstr(0, i+2, -1, attr, buf);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = buf;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-buf, i+2, len, TERM_VIOLET, shower);

					/* Advance */
					str += len;
				}
			}

			/* Count the printed lines */
			i++;
		}

		/* Show a general "title" */
		prt(caption, 0, 0);

		/* Prompt -- small files */
		if (size <= 20)
		{
			/* Wait for it */
			prt("[Press ESC to exit.]", 23, 0);
		}

		/* Prompt -- large files */
		else
		{
			/* Wait for it */
			prt("[Press ., <, >, Space, :, or ESC to exit.]", 23, 0);
		}

		/* Get a keypress */
		k = inkey();


		switch(k)
		{
			/* Hack -- try showing */
			case ':':
			{
				/* Get "shower" */
				prt("Show: ", 23, 0);
				(void)askfor_aux(shower, 80);

				break;
			}

			/* Hack -- go to a specific line */
			case '#':
			{
				char tmp[80];
				prt("Goto Line: ", 23, 0);
				strcpy(tmp, "0");
				if (askfor_aux(tmp, 80))
				{
					*line = atoi(tmp);
				}

				break;
			}

			/* Hack -- Allow backing up */
			case '.':
			{
				/* Allow wrapping */
				if (*line == 0) *line = size;

				*line = *line - 20;
				if (*line < 0) *line = 0;

				break;
			}

			/* Hack -- Back up a single line */
			case '<':
			{
				/* Allow wrapping */
				if (*line == 0) *line = size;

				*line = *line - 1;
				if (*line < 0) *line = 0;

				break;
			}

			/* Hack -- Advance a single line */
			case '>':
			{
				*line = *line + 1;

				break;
			}

			/* Advance one page */
			case ' ':
			{
				*line = *line + 20;

				break;
			}

			/* Output a command */
			default:
			{
				show = FALSE;
				break;
			}
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Normal return */
	return k;
}


char show_inven(cptr caption, int *line)
{
	int i, j, z = 0;
	int lim;

	object_type *o_ptr;

	char o_name[80];

	char tmp_val[160];

	char cmd, t;

	byte out_color;

	FILE *fff;

	char file_name[1024];

	/* Temporary file */
	if (path_temp(file_name, 1024)) return ESCAPE;

	/* Open */
	fff = my_fopen(file_name, "w");

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;


	/* Find the "final" slot */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i + 1;
	}

	/* Display the inventory */
	for (i = 0; i < z; i++)
	{
		o_ptr = &inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Pad descriptions */
		for (j = strlen(o_name); j < lim; j++)
		{
			o_name[j] = ' ';
		}

		/* Acquire inventory color */
		out_color = tval_to_attr[o_ptr->tval & 0x7F];

		/* Disable inventory colors */
		if (!inventory_colors) out_color = TERM_WHITE;

		/* Process the colors */
		if ((t = attr_to_color_char(out_color)) != '\0')
		{
			sprintf(tmp_val, "&%c%s", t, o_name);
		}
		else
		{
			sprintf(tmp_val, "&w%s", o_name);
		}

		/* Display the weight if needed */
		if (show_weights)
		{
			int wt = o_ptr->wt * o_ptr->number;
			strcat(tmp_val, format("%3d.%1d lb", wt / 10, wt % 10));
		}

		my_fputs(fff, tmp_val, 1024);
	}

	/* Close the file */
	my_fclose(fff);

	cmd = show_inven_aux(file_name, caption, line);

	/* Delete the file */
	fd_kill(file_name);

	return cmd;
}


char show_equip(cptr caption, int *line)
{
	int i, j, z = 0;
	int lim;

	object_type *o_ptr;

	char o_name[80];

	char tmp_val[160];

	char cmd, t;

	byte out_color;

	FILE *fff;

	char file_name[1024];

	/* Temporary file */
	if (path_temp(file_name, 1024)) return ESCAPE;

	/* Open */
	fff = my_fopen(file_name, "w");

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for labels (if needed) */
	if (show_labels) lim -= (14 + 2);

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;


	/* Find the "final" slot */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i + 1;
	}

	/* Display the inventory */
	for (i = INVEN_WIELD; i < z; i++)
	{
		o_ptr = &inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Pad descriptions */
		for (j = strlen(o_name); j < lim; j++)
		{
			o_name[j] = ' ';
		}

		/* Acquire inventory color */
		out_color = tval_to_attr[o_ptr->tval & 0x7F];

		/* Disable inventory colors */
		if (!inventory_colors) out_color = TERM_WHITE;

		/* Process the colors */
		if ((t = attr_to_color_char(out_color)) != '\0')
		{
			sprintf(tmp_val, "&%c%s", t, o_name);
		}
		else
		{
			sprintf(tmp_val, "&w%s", o_name);
		}

		/* Use labels */
		if (show_labels)
		{
			/* Mention the use */
			strcat(tmp_val, format("%-14s: ", mention_use(i)));
		}

		/* Display the weight if needed */
		if (show_weights)
		{
			int wt = o_ptr->wt * o_ptr->number;
			strcat(tmp_val, format("%3d.%1d lb", wt / 10, wt % 10));
		}

		my_fputs(fff, tmp_val, 1024);
	}

	/* Close the file */
	my_fclose(fff);

	cmd = show_inven_aux(file_name, caption, line);

	/* Delete the file */
	fd_kill(file_name);

	return cmd;
}


char show_floor(cptr caption, int *line)
{
	int j;
	int lim;

	char o_name[80];

	char tmp_val[160];

	char cmd, t;

	int py = p_ptr->py;
	int px = p_ptr->px;

	s16b this_o_idx, next_o_idx;

	byte out_color;

	FILE *fff;

	char file_name[1024];

	/* Temporary file */
	if (path_temp(file_name, 1024)) return ESCAPE;

	/* Open */
	fff = my_fopen(file_name, "w");

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Pad descriptions */
		for (j = strlen(o_name); j < lim; j++)
		{
			o_name[j] = ' ';
		}

		/* Acquire inventory color */
		out_color = tval_to_attr[o_ptr->tval & 0x7F];

		if (pack_heavy(o_ptr) || pack_bulky(o_ptr)) out_color = TERM_VIOLET;

		/* Disable inventory colors */
		if (!inventory_colors) out_color = TERM_WHITE;

		/* Process the colors */
		if ((t = attr_to_color_char(out_color)) != '\0')
		{
			sprintf(tmp_val, "&%c%s", t, o_name);
		}
		else
		{
			sprintf(tmp_val, "&w%s", o_name);
		}

		/* Display the weight if needed */
		if (show_weights)
		{
			int wt = o_ptr->wt * o_ptr->number;
			strcat(tmp_val, format("%3d.%1d lb", wt / 10, wt % 10));
		}

		my_fputs(fff, tmp_val, 1024);
	}

	/* Close the file */
	my_fclose(fff);

	cmd = show_inven_aux(file_name, caption, line);

	/* Delete the file */
	fd_kill(file_name);

	return cmd;
}


char show_store(store_type *st_ptr, owner_type *ot_ptr, bool home, cptr caption, int *line)
{
	int i, j, x, z = 0;
	int lim;

	object_type *o_ptr;

	char o_name[80];

	char tmp_val[160];

	char cmd, t;

	byte out_color;

	FILE *fff;

	char file_name[1024];

	/* Temporary file */
	if (path_temp(file_name, 1024)) return ESCAPE;

	/* Open */
	fff = my_fopen(file_name, "w");

	/* Maximum space allowed for descriptions */
	lim = 79 - 4;

	/* Require space for price in stores */
	if (home)
	{
		/* Require space for weight (if needed) */
		if (show_weights) lim -= 10;
	}
	else
	{
		lim -= 10;

		/* Require space for weight (if needed) */
		if (show_weights) lim -= 8;
	}


	/* Find the "final" slot */
	for (i = 0; i < STORE_INVEN_MAX; i++)
	{
		o_ptr = &st_ptr->stock[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i + 1;
	}

	/* Display the inventory */
	for (i = 0; i < z; i++)
	{
		o_ptr = &st_ptr->stock[i];

		/* Describe the object */
		if (home) object_desc(o_name, o_ptr, TRUE, 3);
		else object_desc_store(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Pad descriptions */
		for (j = strlen(o_name); j < lim; j++)
		{
			o_name[j] = ' ';
		}

		/* Acquire inventory color */
		out_color = tval_to_attr[o_ptr->tval & 0x7F];

		/* Disable inventory colors */
		if (!inventory_colors) out_color = TERM_WHITE;

		/* Process the colors */
		if ((t = attr_to_color_char(out_color)) != '\0')
		{
			sprintf(tmp_val, "&%c%s", t, o_name);
		}
		else
		{
			sprintf(tmp_val, "&w%s", o_name);
		}

		/* Display the weight if needed */
		if (show_weights)
		{
			int wt = o_ptr->wt;
			strcat(tmp_val, format("%3d.%1d lb", wt / 10, wt % 10));
		}

		/* Display a "fixed" cost */
		if (o_ptr->ident & (IDENT_FIXED))
		{
			/* Extract the "minimum" price */
			x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

			/* Actually insert the price (fixed) */
			strcat(tmp_val, format("%9ld F", (long)x));
		}

		/* Display a "taxed" cost */
		else if (auto_haggle)
		{
			/* Extract the "minimum" price */
			x = price_item(o_ptr, ot_ptr->min_inflate, FALSE);

			/* Hack -- Apply Sales Tax if needed */
			if (!noneedtobargain(x)) x += x / 10;

			/* Actually insert the price (with tax) */
			strcat(tmp_val, format("%9ld  ", (long)x));
		}

		/* Display a "haggle" cost */
		else
		{
			/* Extrect the "maximum" price */
			x = price_item(o_ptr, ot_ptr->max_inflate, FALSE);

			/* Actually insert the price (not fixed) */
			strcat(tmp_val, format("%9ld  ", (long)x));
		}

		my_fputs(fff, tmp_val, 1024);
	}

	/* Close the file */
	my_fclose(fff);

	cmd = show_inven_aux(file_name, caption, line);

	/* Delete the file */
	fd_kill(file_name);

	return cmd;
}

