/* File: files.c */

/*
 * Read preference files.  Get best kill, display the character screen.
 * Character dumps, read random line from a file.  Show a file (inc. the
 * online help), context-specific help. Display a file (colorized).  Process
 * a character name, commit suicide, save the game.  Calculate and display
 * score.  The character death interface.  Controlled exit, panic saves, and
 * signal-handling.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * Extract the first few "tokens" from a buffer
 *
 * This function uses "colon" and "forward slash" as the delimiter
 * characters.
 *
 * We never extract more than "num" tokens.  The "last" token may include
 * "delimiter" characters, allowing the buffer to include a "string" token.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 *
 * Hack -- Attempt to handle the 'c' character formalism
 *
 * Hack -- An empty buffer, or a final delimiter, yields an "empty" token.
 *
 * Hack -- We will always extract at least one token
 */
s16b tokenize(char *buf, s16b num, char **tokens)
{
	int i = 0;

	char *s = buf;

	/* Process */
	while (i < num - 1)
	{
		char *t;

		/* Scan the string */
		for (t = s; *t; t++)
		{
			/* Found a delimiter */
			if ((*t == ':') || (*t == '/')) break;

			/* Handle single quotes */
			if (*t == '\'')
			{
				/* Advance */
				t++;

				/* End of string */
				if (!*t) break;

				/* Notice possible delimiter */
				if ((*t == ':') || (*t == '/'))
				{
					/* Quote it only if followed by a single quote */
					if (!*(t+1) || (*(t+1) != '\'')) break;
				}

				/* Advance */
				t++;

				/* Require a character */
				if (!*t) break;

				/* Hack -- Require a close quote */
				if (*t != '\'') *t = '\'';
			}

			/* Handle back-slash */
			if (*t == '\\') t++;

			/* Ignore empty space */
			if (my_isspace(*t)) t++;
		}

		/* Nothing left */
		if (!*t) break;

		/* Nuke and advance */
		*t++ = '\0';

		/* Save the token */
		tokens[i++] = s;

		/* Advance */
		s = t;
	}

	/* Save the token */
	tokens[i++] = s;

	/* Number found */
	return (i);
}


/*
 * Allow users to supply attr and char information in decimal, hexa-
 * decimal, octal, or even character form (the last of these three
 * being most familiar to many).  -LM-
 */
static bool read_byte_or_char(char *zz, byte *a, char *c)
{
	*a = 0;  *c = '\0';

	/* First character is a single quote; third is another */
	if ((zz[0] == '\'') && (zz[1]) && (zz[2]) && (zz[2] == '\''))
	{
		/* Accept the character between them */
		*c = zz[1];

		/* We are returning a char */
		return (FALSE);
	}

	/* First character is a '+' followed by a digit */
	if ((zz[0] == '+') && zz[1] && isdigit(zz[1]))
	{
		/* Skip the '+' */
		zz++;

		/* Read as 8-bit number */
		*a = (byte)strtol(zz, NULL, 0);

		/* If number is less than 128, add 128 */
		if (*a < 128) *a += 128;

		/* We are returning a byte */
		return (TRUE);
	}

	/* First character is a digit, or a '-' followed by a digit */
	if (isdigit(zz[0]) || ((zz[0] == '-') && zz[1] && isdigit(zz[1])))
	{
		/* Read as 8-bit number */
		*a = (byte)strtol(zz, NULL, 0);

		/* We are returning a byte */
		return (TRUE);
	}

	/* Usual case -- read it as a character */
	*c = zz[0];

	/* We are returning a char */
	return (FALSE);
}

/*
 * Convert preference file colors to 16-color mode, if necessary.
 */
static byte color_conv(byte a)
{
	/* This attribute has the high bit set -- leave it alone */
	if (a >= 128) return (a);

	/* System cannot display this color */
	if (a >= max_system_colors)
	{
		/* Translate to 16-color mode */
		a = color_table[a].color_translate;
	}
	return (a);
}


/*
 * Parse a sub-file of the "extra info" (format shown below)
 *
 * Each "action" line has an "action symbol" in the first column,
 * followed by a colon, followed by some command specific info,
 * usually in the form of "tokens" separated by colons or slashes.
 *
 * Blank lines, lines starting with white space, and lines starting
 * with pound signs ("#") are ignored (as comments).
 *
 * Note the use of "tokenize()" to allow the use of both colons and
 * slashes as delimiters, while still allowing final tokens which
 * may contain any characters including "delimiters".
 *
 * Note that "monster zero" is used for the "player" attr/char, "object
 * zero" will be used for the "stack" attr/char, and "feature zero" is
 * used for the "nothing" attr/char.
 */
errr process_pref_file_command(char *buf)
{
	int i, j;

	char *zz[16];

	char c;
	byte a;


	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (my_isspace((unsigned char)buf[0])) return (0);

	/* Skip comments */
	if (buf[0] == '#') return (0);


	/* Require "?:*" format */
	if (buf[1] != ':') return (1);


	/* Process "R:<num>:<a>/<c>" -- attr/char for monster races */
	if (buf[0] == 'R')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			monster_race *r_ptr;
			i = strtol(zz[0], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->r_max)) return (1);
			r_ptr = &r_info[i];

			/* Get monster color */
			if (read_byte_or_char(zz[1], &a, &c))
			      r_ptr->x_attr = color_conv(a);
			else  r_ptr->x_attr = (byte)color_char_to_attr(c);

			/* Get monster symbol */
			if (read_byte_or_char(zz[2], &a, &c))
			      r_ptr->x_char = (char)a;
			else  r_ptr->x_char = c;

			return (0);
		}
	}


	/* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
	else if (buf[0] == 'K')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			object_kind *k_ptr;
			i = strtol(zz[0], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->k_max)) return (1);
			k_ptr = &k_info[i];

			/* Get object kind color */
			if (read_byte_or_char(zz[1], &a, &c))
			      k_ptr->x_attr = color_conv(a);
			else  k_ptr->x_attr = (byte)color_char_to_attr(c);

			/* Get object kind symbol */
			if (read_byte_or_char(zz[2], &a, &c))
			      k_ptr->x_char = (char)a;
			else  k_ptr->x_char = c;

			return (0);
		}
	}


	/* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
	else if (buf[0] == 'F')
	{
		/* Accept any number of entries, up to 8 */
		int result = tokenize(buf+2, 8, zz);

		/* Ignore if we don't get at least an index and one attr/char pair */
		if (result >= 3)
		{
			feature_type *f_ptr;
			i = strtol(zz[0], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->f_max)) return (1);
			f_ptr = &f_info[i];

			/* Get feature color -- basic */
			if (read_byte_or_char(zz[1], &a, &c))
			      f_ptr->x_attr = color_conv(a);
			else  f_ptr->x_attr = (byte)color_char_to_attr(c);

			/* Get feature symbol -- basic */
			if (read_byte_or_char(zz[2], &a, &c))
			      f_ptr->x_char = (char)a;
			else  f_ptr->x_char = c;


			/* We have entries for "brightly lit" */
			if (result >= 5)
			{
				/* Get feature color -- bright */
				if (read_byte_or_char(zz[3], &a, &c))
					  f_ptr->x_attr_lit = color_conv(a);
				else  f_ptr->x_attr_lit = (byte)color_char_to_attr(c);

				/* Get feature symbol -- bright */
				if (read_byte_or_char(zz[4], &a, &c))
					  f_ptr->x_char_lit = (char)a;
				else  f_ptr->x_char_lit = c;
			}
			else
			{
				/* Otherwise, use the basic pictures for all cases */
				f_ptr->x_char_dim = f_ptr->x_char_lit = f_ptr->x_char;
				f_ptr->x_attr_dim = f_ptr->x_attr_lit = f_ptr->x_attr;
			}

			/* We have entries for "in shadow" */
			if (result >= 7)
			{
				/* Get feature color -- dimmed */
				if (read_byte_or_char(zz[5], &a, &c))
					  f_ptr->x_attr_dim = color_conv(a);
				else  f_ptr->x_attr_dim = (byte)color_char_to_attr(c);

				/* Get feature symbol -- dimmed */
				if (read_byte_or_char(zz[6], &a, &c))
					  f_ptr->x_char_dim = (char)a;
				else  f_ptr->x_char_dim = c;
			}
			else
			{
				/* Otherwise, use the basic pictures for shadow */
				f_ptr->x_char_dim = f_ptr->x_char;
				f_ptr->x_attr_dim = f_ptr->x_attr;
			}

			/* We have an entry that controls torch/glow lighting */
			if (result >= 8)
			{
				/* Allow pref file to specify special lighting rules */
				if (strstr(zz[7], "torch_only"))
				{
					f_ptr->flags |= (TF_TORCH_ONLY);
				}
			}

			return (0);
		}
	}

	/* Process "L:<a>/<c>:<a>/<c>" -- attr/char for flavors */
	else if (buf[0] == 'L')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			flavor_type *flavor_ptr;

			/* Get index */
			i = strtol(zz[0], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->flavor_max)) return (1);
			flavor_ptr = &flavor_info[i];

			/* Get desired flavor color */
			if (read_byte_or_char(zz[1], &a, &c)) flavor_ptr->x_attr = color_conv(a);
			else                                  flavor_ptr->x_attr = (byte)color_char_to_attr(c);

			/* Get desired flavor symbol */
			if (read_byte_or_char(zz[2], &a, &c)) flavor_ptr->x_char = (char)a;
			else                                  flavor_ptr->x_char = c;

			return (0);
		}
	}

	/* Process "S:<num>:<a>/<c>" -- projection graphics */
	else if (buf[0] == 'S')
	{
		if (tokenize(buf+2, 11, zz) == 11)
		{
			i = strtol(zz[0], NULL, 0);

			/* Scan the project graphics array */
			for (j = i; j < 256; j++)
			{
				int k;
				byte atmp;
				char ctmp;

				/* Scan down the list */
				for (k = 1; k < 11; k++)
				{
					/* Translate attr/chars */
					if (read_byte_or_char(zz[k], &a, &c))
					{
						if ((k == 1) || (k == 3) || (k == 5) || (k == 7) || (k == 9))
							atmp = color_conv(a);
						else
							atmp = a;
						ctmp = (char)atmp;
					}
					else
					{
						if ((k == 1) || (k == 3) || (k == 5) || (k == 7) || (k == 9))
							ctmp = color_char_to_attr(c);
						else
							ctmp = c;
						atmp = (byte)ctmp;
					}

					/* Store attr/char pairs */
					if (k == 1) proj_graphics[j].attr_vert  = atmp;
					else if (k == 2) proj_graphics[j].char_vert  = ctmp;
					else if (k == 3) proj_graphics[j].attr_horiz = atmp;
					else if (k == 4) proj_graphics[j].char_horiz = ctmp;
					else if (k == 5) proj_graphics[j].attr_rdiag = atmp;
					else if (k == 6) proj_graphics[j].char_rdiag = ctmp;
					else if (k == 7) proj_graphics[j].attr_ldiag = atmp;
					else if (k == 8) proj_graphics[j].char_ldiag = ctmp;
					else if (k == 9) proj_graphics[j].attr_ball  = atmp;
					else if (k ==10) proj_graphics[j].char_ball  = ctmp;
				}

				/* Usually only store this set of graphics */
				if (i) break;
			}

			return (0);
		}
	}

	/* Process "E:<tv>:<a>" -- attribute for inventory objects */
	else if (buf[0] == 'E')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{
			/* Get tval */
			j = strtol(zz[0], NULL, 0) % 128;
			if ((j < 0) || (j >= (long)N_ELEMENTS(tval_to_attr))) return (1);

			/* Handle 'r', or '4', or '0x04' for "red" */
			if (read_byte_or_char(zz[1], &a, &c))
			     tval_to_attr[j] = color_conv(a);
			else tval_to_attr[j] = (byte)color_char_to_attr(c);

			return (0);
		}
	}



	/* Process "A:<str>" -- save an "action" for later */
	else if (buf[0] == 'A')
	{
		text_to_ascii(macro_buffer, sizeof(macro_buffer), buf+2);
		return (0);
	}

	/* Process "P:<str>" -- create macro */
	else if (buf[0] == 'P')
	{
		char tmp[1024];
		text_to_ascii(tmp, sizeof(tmp), buf+2);
		(void)macro_add(tmp, macro_buffer);
		return (0);
	}

	/* Process "C:<num>:<str>" -- create keymap */
	else if (buf[0] == 'C')
	{
		long mode;

		char tmp[1024];

		if (tokenize(buf+2, 2, zz) != 2) return (1);

		mode = strtol(zz[0], NULL, 0);
		if ((mode < 0) || (mode >= KEYMAP_MODES)) return (1);

		text_to_ascii(tmp, sizeof(tmp), zz[1]);
		if (!tmp[0] || tmp[1]) return (1);
		i = (byte)(tmp[0]);

		(void)string_free(keymap_act[mode][i]);

		keymap_act[mode][i] = string_make(macro_buffer);

		return (0);
	}


	/* Handle colors */
	/* Process "V:<num>:<index_char>:<name>:<kv>:<rv>:<gv>:<bv>" */
	else if (buf[0] == 'V')
	{
		if (tokenize(buf+2, 8, zz) == 8)
		{
			/* Extract index */
			i = strtol(zz[0], NULL, 0);

			/* Require legal index */
			if ((i < 0) || (i >= MAX_COLORS)) return (1);

			/* Get index character */
			strcpy(buf, format("%s", zz[1]));

			/* Index character must be unique */
			for (j = 0; j < i; j++)
			{
				if (color_table[j].index_char == buf[0])
				{
					msg_format("Index character of color %d is already used for color %d.", i, j);
					return (1);
				}
			}
			color_table[i].index_char = buf[0];

			/* Save name */
			strcpy(color_table[i].name, format("%s", zz[2]));

			/* Save color values */
			color_table[i].kv = (byte)strtol(zz[3], NULL, 0);
			color_table[i].rv  = (byte)strtol(zz[4], NULL, 0);
			color_table[i].gv  = (byte)strtol(zz[5], NULL, 0);
			color_table[i].bv  = (byte)strtol(zz[6], NULL, 0);

			/* Save 16-color mode translation value */
			color_table[i].color_translate =
				(s16b)strtol(zz[7], NULL, 0);

			/* Paranoia -- require legal 16-color translations */
			if (color_table[i].color_translate >= max_system_colors)
			{
				color_table[i].color_translate =
					translate_into_16_colors(color_table[i].color_translate);
			}

			return (0);
		}
	}

	/* Process "N:<index>:<a>:<c>" -- Trap graphics */
	else if (buf[0] == 'N')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			/* Get index */
			j = strtol(zz[0], NULL, 0);
			if ((j < 0) || (j >= TRAP_KIND_MAX)) return (1);

			/* Get desired trap color */
			if (read_byte_or_char(zz[1], &a, &c)) t_kind_info[j].x_attr = color_conv(a);
			else                                  t_kind_info[j].x_attr = (byte)color_char_to_attr(c);

			/* Get desired trap symbol */
			if (read_byte_or_char(zz[2], &a, &c)) t_kind_info[j].x_char = (char)a;
			else                                  t_kind_info[j].x_char = c;

			return (0);
		}
	}

	/* Process "B:<index>:<a>:<c>" -- Player graphics (race and skill-dependent) */
	else if (buf[0] == 'B')
	{
		bool all_races = FALSE;
		int specialty;

		char *zz_large[MAX_SPECIALTIES * 2];

		/* Find out how many specialty graphics are defined */
		int lim = tokenize(buf+2, MAX_SPECIALTIES * 2, zz_large);

		/* Scan the available information */
		for (i = 0, j = 0; i < lim; i++)
		{
			/* Get race index */
			if (i == 0)
			{
				j = strtol(zz_large[0], NULL, 0);
				if (j >= MAX_RACES) return (1);

				/* Allow specification of "all races" */
				if (j < 0)
				{
					all_races = TRUE;
					j = 0;
				}
				continue;
			}

			/* Calculate current specialty (2 data points per specialty) */
			specialty = ((i-1) / 2);

			/* Read either the attribute or the char */
			if (i % 2)
			{
				/* Get desired player attribute */
				if (read_byte_or_char(zz_large[i], &a, &c))
					player_graphics[j][specialty][0] = color_conv(a);
				else
					player_graphics[j][specialty][0] = (byte)color_char_to_attr(c);
			}
			else
			{
				/* Get desired player symbol */
				if (read_byte_or_char(zz_large[i], &a, &c))
					player_graphics[j][specialty][1] = (char)a;
				else
					player_graphics[j][specialty][1] = c;
			}


			/* Handle case of "all races" or "all specialties" */
			if ((all_races) || (lim == 3))
			{
				int x, y;

				/* Handle request to fill in the entire "player_graphics" array */
				if ((all_races) && (lim == 3))
				{
					/* All the races and all the specialties use the current attr/char */
					for (y = 0; y < MAX_RACES; y++)
					{
						for (x = 0; x < MAX_SPECIALTIES; x++)
						{
							if (i % 2) player_graphics[y][x][0] =
								player_graphics[j][specialty][0];
							else       player_graphics[y][x][1] =
								player_graphics[j][specialty][1];
						}
					}
				}

				/* Handle request to use this definition for all races */
				else if (all_races)
				{
					for (y = 0; y < MAX_RACES; y++)
					{
						if (i % 2) player_graphics[y][specialty][0] =
							player_graphics[j][specialty][0];
						else       player_graphics[y][specialty][1] =
							player_graphics[j][specialty][1];
					}
				}

				/* Handle request to use this definition for all specialties */
				else if (lim == 3)
				{
					for (x = 0; x < MAX_SPECIALTIES; x++)
					{
						if (i % 2) player_graphics[j][x][0] =
							player_graphics[j][specialty][0];
						else       player_graphics[j][x][1] =
							player_graphics[j][specialty][1];
					}
				}
			}
		}
		return (0);
	}

	/* Process "I:<index>:<attr>:<char>" -- Miscellaneous graphics */
	else if (buf[0] == 'I')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			/* Get index */
			j = strtol(zz[0], NULL, 0);
			if ((j < 0) || (j >= MISC_GRAPHICS_MAX)) return (1);

			/* Get desired color */
			if (read_byte_or_char(zz[1], &a, &c)) misc_graphics_info[j][0] = color_conv(a);
			else                                  misc_graphics_info[j][0] = (byte)color_char_to_attr(c);

			/* Get desired trap symbol */
			if (read_byte_or_char(zz[2], &a, &c)) misc_graphics_info[j][1] = a;
			else                                  misc_graphics_info[j][1] = (byte)c;

			return (0);
		}
	}

	/* set macro trigger names and a template */
	/* Process "T:<trigger>:<keycode>:<shift-keycode>" */
	/* Process "T:<template>:<modifier chr>:<modifier name>:..." */
	else if (buf[0] == 'T')
	{
		int tok;

		tok = tokenize(buf + 2, MAX_MACRO_MOD + 2, zz);

		/* Trigger template */
		if (tok >= 4)
		{
			int num;

			/* Free existing macro triggers and trigger template */
			(void)macro_trigger_free();

			/* Clear template done */
			if (*zz[0] == '\0') return (0);

			/* Count modifier-characters */
			num = strlen(zz[1]);

			/* One modifier-character per modifier */
			if (num + 2 != tok) return (1);

			/* Macro template */
			macro_template = string_make(zz[0]);

			/* Modifier chars */
			macro_modifier_chr = string_make(zz[1]);

			/* Modifier names */
			for (i = 0; i < num; i++)
			{
				macro_modifier_name[i] = string_make(zz[2+i]);
			}
		}
		/* Macro trigger */
		else if (tok >= 2)
		{
			char *tmp;
			cptr s;
			char *t;

			if (max_macrotrigger >= MAX_MACRO_TRIGGER)
			{
				msg_print("Too many macro triggers!");
				return (1);
			}

			/* Buffer for the trigger name */
			C_MAKE(tmp, strlen(zz[0]) + 1, char);

			/* Simulate strcpy() and skip the '\' escape character */
			s = zz[0];
			t = tmp;

			while (*s)
			{
				if ('\\' == *s) s++;
				*t++ = *s++;
			}

			/* Terminate the trigger name */
			*t = '\0';

			/* Store the trigger name */
			macro_trigger_name[max_macrotrigger] = string_make(tmp);

			/* Free the buffer */
			FREE(tmp);

			/* Normal keycode */
			macro_trigger_keycode[0][max_macrotrigger] = string_make(zz[1]);

			/* Special shifted keycode */
			if (tok == 3)
			{
				macro_trigger_keycode[1][max_macrotrigger] = string_make(zz[2]);
			}
			/* Shifted keycode is the same as the normal keycode */
			else
			{
				macro_trigger_keycode[1][max_macrotrigger] = string_make(zz[1]);
			}

			/* Count triggers */
			max_macrotrigger++;
		}

		return (0);
	}

	/* Process "X:<str>" -- turn option off */
	else if (buf[0] == 'X')
	{
		/* Check non-birth options */
		for (i = 0; i < OPT_BIRTH; i++)
		{
			if (option_text[i] && streq(option_text[i], buf + 2))
			{
				op_ptr->opt[i] = FALSE;
				return (0);
			}
		}

		/* Hack -- ignore incorrect options  XXX XXX */
		return (0);
	}

	/* Process "Y:<str>" -- turn option on */
	else if (buf[0] == 'Y')
	{
		/* Check non-birth options */
		for (i = 0; i < OPT_BIRTH; i++)
		{
			if (option_text[i] && streq(option_text[i], buf + 2))
			{
				op_ptr->opt[i] = TRUE;
				return (0);
			}
		}

		/* Hack -- ignore incorrect options  XXX XXX */
		return (0);
	}


	/* Process "W:<win>:<flag>:<value>" -- window flags */
	else if (buf[0] == 'W')
	{
		long win, flag, value;

		if (tokenize(buf + 2, 3, zz) == 3)
		{
			win = strtol(zz[0], NULL, 0);
			flag = strtol(zz[1], NULL, 0);
			value = strtol(zz[2], NULL, 0);

			/* Ignore windows other than the assignable sub-windows */
			if ((win < TERM_SUBWINDOW) || (win >= TERM_MAX)) return (1);

			/* Special case -- turn ALL displays off */
			if (flag == -1)
			{
				for (i = 0; i < 32; i++)
				{
					op_ptr->window_flag[win] &= ~(1L << i);
				}
				return (0);
			}

			/* Ignore illegal flags */
			if ((flag < 0) || (flag >= 32)) return (1);

			/* Require a real flag */
			if (window_flag_desc[flag])
			{
				if (value)
				{
					/* Turn flag on, cancel all previous */
					op_ptr->window_flag[win] = (1L << flag);
				}
				else
				{
					/* Turn flag off */
					op_ptr->window_flag[win] &= ~(1L << flag);
				}
			}

			/* Success */
			return (0);
		}
	}


	/* Process "M:<type>:<attr>" -- colors for message-types */
	else if (buf[0] == 'M')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{
			long type = strtol(zz[0], NULL, 0);
			int color = color_char_to_attr(zz[1][0]);

			/* Ignore illegal color */
			if (color < 0) return (1);

			/* Store the color */
			return (message_color_define((u16b)type, (byte)color));
		}
	}


	/* Process "D:<num>" -- delay factor */
	else if (buf[0] == 'D')
	{
		if (tokenize(buf + 2, 1, zz) == 1)
		{
			op_ptr->delay_factor = strtol(zz[0], NULL, 0);

			/* Success */
			return (0);
		}
	}

	/* Process "H:<num>" -- hit point warning */
	else if (buf[0] == 'H')
	{
		if (tokenize(buf + 2, 1, zz) == 1)
		{
			op_ptr->hitpoint_warn = strtol(zz[0], NULL, 0);

			/* Success */
			return (0);
		}
	}

	/* Process "a:<flag>:<num>" -- Autosave */
	else if (buf[0] == 'a')
	{
		if (tokenize(buf + 2, 2, zz) == 2)
		{
			autosave_freq = strtol(zz[1], NULL, 0);

			/* Success */
			return (0);
		}
	}

	/* Process "t:<flag>:<num>:<vert><hori>" -- screen options */
	else if (buf[0] == 't')
	{
		int vert, hori;

		i = tokenize(buf + 2, 5, zz);

		/* Handle only up-to-date options (changed for v1.0.0) */
		if (i == 5)
		{
			/* The first number is currently unused */

			more_tall_display = strtol(zz[1], NULL, 0);
			map_display_precise_fit = strtol(zz[2], NULL, 0);
			vert = strtol(zz[3], NULL, 0);
			hori = strtol(zz[4], NULL, 0);

			if ((vert > 0) && (vert < 22)) clear_y = vert;
			if ((hori > 0) && (hori < 22)) clear_x = hori;

			/* Success */
			return (0);
		}
	}

	/* Failure */
	return (1);
}


/*
 * Helper function for "process_pref_file()"
 *
 * Input:
 *   v: output buffer array
 *   f: final character
 *
 * Output:
 *   result
 */
static cptr process_pref_file_expr(char **sp, char *fp)
{
	cptr v;

	char *b;
	char *s;

	char b1 = '[';
	char b2 = ']';

	char f = ' ';

	/* Initial */
	s = (*sp);

	/* Skip spaces */
	while (my_isspace((unsigned char)*s)) s++;

	/* Save start */
	b = s;

	/* Default */
	v = "?o?o?";

	/* Analyze */
	if (*s == b1)
	{
		const char *p;
		const char *t;

		/* Skip b1 */
		s++;

		/* First */
		t = process_pref_file_expr(&s, &f);

		/* Oops */
		if (!*t)
		{
			/* Nothing */
		}

		/* Function: IOR */
		else if (streq(t, "IOR"))
		{
			v = "0";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "1";
			}
		}

		/* Function: AND */
		else if (streq(t, "AND"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && streq(t, "0")) v = "0";
			}
		}

		/* Function: NOT */
		else if (streq(t, "NOT"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "0";
			}
		}

		/* Function: EQU */
		else if (streq(t, "EQU"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_pref_file_expr(&s, &f);
				if (*t && !streq(p, t)) v = "0";
			}
		}

		/* Function: LEQ */
		else if (streq(t, "LEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_pref_file_expr(&s, &f);
				if (*t && (strcmp(p, t) >= 0)) v = "0";
			}
		}

		/* Function: GEQ */
		else if (streq(t, "GEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_pref_file_expr(&s, &f);
				if (*t && (strcmp(p, t) <= 0)) v = "0";
			}
		}

		/* Oops */
		else
		{
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
		}

		/* Verify ending */
		if (f != b2) v = "?x?x?";

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';
	}

	/* Other */
	else
	{
		/* Accept all printables except spaces and brackets */
		while (my_isprint((unsigned char)*s) && !strchr(" []", *s)) ++s;

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';

		/* Variable */
		if (*b == '$')
		{
			/* System */
			if (streq(b + 1, "SYS"))
			{
				v = ANGBAND_SYS;
			}

			/* Graphics */
			else if (streq(b + 1, "GRAF"))
			{
				v = graphics_data[use_graphics].file_name;
			}

			/* Race */
			else if ((streq(b + 1, "RACE")) && (rp_ptr))
			{
				v = rp_ptr->title;
			}

			/* Realm */
			else if ((streq(b + 1, "REALM")) && (mp_ptr))
			{
				v = mp_ptr->title;
			}

			/* Player */
			else if ((streq(b + 1, "PLAYER")) && (op_ptr))
			{
				v = op_ptr->base_name;
			}
		}

		/* Constant */
		else
		{
			v = b;
		}
	}

	/* Save */
	(*fp) = f;

	/* Save */
	(*sp) = s;

	/* Result */
	return (v);
}


/*
 * Open the "user pref file" and parse it.
 */
static errr process_pref_file_aux(cptr name)
{
	FILE *fp;

	char buf[1024];

	char old[1024];

	int line = -1;

	errr err = 0;

	bool bypass = FALSE;


	/* Open the file */
	fp = my_fopen(name, "r");

	/* No such file */
	if (!fp) return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, sizeof(buf)))
	{
		/* Count lines */
		line++;


		/* Skip "empty" lines */
		if (!buf[0]) continue;

		/* Skip "blank" lines */
		if (my_isspace((unsigned char)buf[0])) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;


		/* Save a copy */
		(void)my_strcpy(old, buf, sizeof(old));


		/* Process "?:<expr>" */
		if ((buf[0] == '?') && (buf[1] == ':'))
		{
			char f;
			cptr v;
			char *s;

			/* Start */
			s = buf + 2;

			/* Parse the expr */
			v = process_pref_file_expr(&s, &f);

			/* Set flag */
			bypass = (streq(v, "0") ? TRUE : FALSE);

			/* Continue */
			continue;
		}

		/* Apply conditionals */
		if (bypass) continue;


		/* Process "%:<file>" */
		if (buf[0] == '%')
		{
			static int depth_count = 0;

			/* Ignore if deeper than 20 level */
			if (depth_count > 20) continue;

			/* Count depth level */
			depth_count++;

			/* Process that file if allowed */
			(void)process_pref_file(buf + 2);

			/* Set back depth level */
			depth_count--;

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_pref_file_command(buf);

		/* Oops */
		if (err) break;
	}


	/* Error */
	if (err)
	{
		/* Print error message */
		/* ToDo: Add better error messages */
		msg_format("Error %d in line %d of file '%s'.", err, line, name);
		msg_format("Parsing '%s'", old);
		message_flush();
	}

	/* Close the file */
	(void)my_fclose(fp);

	/* Result */
	return (err);
}



/*
 * Process the "user pref file" with the given name
 *
 * We first process any such file in the "pref" directory, then any
 * in the "user" directory.  Rules in the latter take priority.
 *
 * See the functions above for a list of legal commands.
 *
 * We also accept the special "?" and "%" directives, which
 * allow conditional evaluation and filename inclusion.
 */
errr process_pref_file(cptr name)
{
	char buf[1024];

	errr err = 0;

	bool file_found = FALSE;


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_PREF, name);

	/* Process the pref file */
	err = process_pref_file_aux(buf);

	/* Notice existence of file */
	if (!err) file_found = TRUE;

	/* Stop at parser errors, but not at non-existing file */
	if (err < 1)
	{
		/* Build the filename */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

		/* Process the pref file */
		err = process_pref_file_aux(buf);

		/* If file doesn't exist here, but did in the "pref" dir, suppress error */
		if ((err == -1) && (file_found)) err = 0;
	}

	/* Result */
	return (err);
}



/*
 * Find the most powerful opponent defeated.
 */
static int slain_opponent(bool require_unique)
{
	int i;
	int depth1 = 0;
	int depth2 = 0;
	int mon1 = 0;
	int mon2 = 0;

	bool sauron_dead = FALSE;
	bool morgoth_dead = FALSE;

	/* Some races of non-unique monsters give little fame when killed */
	cptr unworthy_foes = "abcijlmrtwF";

	/* Scan the monster list */
	for (i = 0; i < z_info->r_max; i++)
	{
		/* Get the monster race and lore */
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Must have killed (at least one of) this foe */
		if (!l_ptr->pkills) continue;

		/* Skip player ghosts */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

		/* Remember the highest-level slain unique */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			if (depth1 < r_ptr->level)
			{
				depth1 = r_ptr->level;
				mon1 = i;
			}
		}

		/* Option to allow OOD non-uniques */
		else if (!require_unique)
		{
			/* Ignore unworthy foes */
			if (strchr(unworthy_foes, r_ptr->d_char)) continue;

			/* Remember the highest-level slain non-unique */
			if (depth2 < r_ptr->level)
			{
				depth2 = r_ptr->level;
				mon2 = i;
			}
		}

		/* Sauron is dead */
		if (i == MON_SAURON) sauron_dead = TRUE;

		/* Morgoth is dead */
		if (i == MON_MORGOTH) morgoth_dead = TRUE;
	}


	/* Make sure that we return the winner monsters */
	if (morgoth_dead) return (MON_MORGOTH);
	if (sauron_dead)  return (MON_SAURON);

	/* Even if we allow non-uniques, we still favour uniques heavily */
	if (depth2 >= depth1 + 6)
	{
		return (mon2);
	}

	/* Otherwise, return a unique */
	return (mon1);
}


/*
 * Print long number with header at given row, column
 * Use the color for the number, not the header
 */
static void prt_lnum(cptr header, s32b num, int row, int col, byte color)
{
	int len = strlen(header);
	char out_val[32];
	put_str(header, row, col);
	(void)strnfmt(out_val, sizeof(out_val), "%9ld", (long)num);
	c_put_str(color, out_val, row, col + len);
}

static void prt_lnum2(cptr header, s32b num, int row, int col, byte color)
{
	int len = strlen(header);
	char buf[32];
	char out_val[32];
	put_str(header, row, col);
	(void)strnfmt(buf, sizeof(buf), "(%ld)", ABS((long)num));
	(void)strnfmt(out_val, sizeof(out_val), "%9s", buf);
	c_put_str(color, out_val, row, col + len);
}

/*
 * Print number with header at given row, column
 */
static void prt_num(cptr header, int num, int row, int col, byte color)
{
	int len = strlen(header);
	char out_val[32];
	put_str(header, row, col);
	put_str("   ", row, col + len);
	(void)strnfmt(out_val, sizeof(out_val), "%6ld", (long)num);
	c_put_str(color, out_val, row, col + len);
}

/*
 * Print two number separated by slash with header at given row, column
 */
static void prt_2num(cptr header, int num, int num2, int row, int col, byte color)
{
	int len = strlen(header);
	char out_val[32];
	(void)strnfmt(out_val, sizeof(out_val), "%d/%d", num, num2);
    (void)strnfmt(out_val, sizeof(out_val), "%7s", out_val);
	c_put_str(color, out_val, row, col + len);

	put_str(header, row, col);
}


/*
 * Prints the following information on the screen.
 *
 * For this to look right, the following should be spaced the
 * same as in the prt_lnum code... -CFT
 */
static void display_player_middle(void)
{
	int show_m_tohit = 0;
	int show_a_tohit = 0;
	int show_m_todam = p_ptr->dis_to_d;
	int show_a_todam = p_ptr->dis_to_d;

	object_type *o_ptr;
	char tmp[DESC_LEN];

	/* Dump the fighting bonuses to hit/dam */

	o_ptr = &inventory[INVEN_WIELD];

	/* Combat information -- melee */
	if (p_ptr->twoweap) prt_2num("Blows per Round ", p_ptr->num_blow, p_ptr->num_blow2, 14, 1, TERM_L_BLUE);
	else prt_num("Blows per Round  ", p_ptr->num_blow, 14, 1, TERM_L_BLUE);

	/* Using a weapon */
	if (is_melee_weapon(o_ptr))
	{
		/* Hack -- add in weapon info if known */
		if (object_known_p(o_ptr)) show_m_tohit += o_ptr->to_h;
		if (object_known_p(o_ptr)) show_m_todam += o_ptr->to_d;
	}

    /* Display hit rate */

    if (p_ptr->twoweap) prt_2num("Hit Rate        ", p_ptr->avg_hit, p_ptr->avg_hit_offhand, 15, 1, TERM_L_BLUE);
    else prt_num("Hit Rate         ", p_ptr->avg_hit, 15, 1, TERM_L_BLUE);

    /* Display average damage */
    if (p_ptr->twoweap) prt_2num("Average Damage  ", (p_ptr->avg_dam + 5) / 10, (p_ptr->avg_dam_offhand + 5) / 10, 16, 1, TERM_L_BLUE);
    else prt_num("Average Damage   ", (p_ptr->avg_dam + 5) / 10, 16, 1, TERM_L_BLUE);



	/* Dump the shooting bonuses to hit/dam */

	o_ptr = &inventory[INVEN_BOW];

	/* Hack -- add in weapon info if known */
	if (object_known_p(o_ptr)) show_a_tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) show_a_todam += o_ptr->to_d;


	/* Combat information -- shooting */
	if (p_ptr->num_fire % 2)
	{
		prt_num("Shots per round", p_ptr->num_fire / 2, 14, 53, TERM_L_BLUE);
		c_put_str(TERM_L_BLUE, ".5", 14, 74);
	}
	else
	{
		prt_num("Shots per round  ", p_ptr->num_fire / 2, 14, 53, TERM_L_BLUE);
	}

	prt_num("+ to Skill       ", show_a_tohit, 15, 53, TERM_L_BLUE);
	if (show_a_todam > 0)
		prt_num("Deadliness (%)   ", deadliness_conversion[show_a_todam], 16, 53, TERM_L_BLUE);
	else
		prt_num("Deadliness (%)   ", -deadliness_conversion[-show_a_todam], 16, 53, TERM_L_BLUE);


	/* Print maximum depth */
	put_str("Max Depth", 12, 27);

	if (depth_in_feet)
	{
		if (use_metric) (void)strnfmt(tmp, sizeof(tmp), " %4d m", p_ptr->max_depth * 15);
		else (void)strnfmt(tmp, sizeof(tmp), "%4d ft", p_ptr->max_depth * 50);
	}
	else
	{
		if (p_ptr->max_depth < 10)
		     (void)strnfmt(tmp, sizeof(tmp), "  Lev %d", p_ptr->max_depth);
		else if (p_ptr->max_depth < 100)
		     (void)strnfmt(tmp, sizeof(tmp), " Lev %d", p_ptr->max_depth);
		else (void)strnfmt(tmp, sizeof(tmp), "Lev %d", p_ptr->max_depth);
	}
	c_put_str(TERM_L_BLUE, tmp, 12, 43);



	/* Power, Score, Character Type (if not normal), Unspent Exp, Gold */
	prt_num("Power            ", (int)p_ptr->power, 9, 27, TERM_L_GREEN);

	if (!p_ptr->deaths)
	   prt_lnum("Score         ",  total_points(), 10, 27, TERM_L_BLUE);
	else
	   prt_lnum2("Score         ", total_points(), 10, 27, TERM_L_BLUE);

	if (p_ptr->character_type != PCHAR_NORMAL)
	{
		move_cursor(8, 10);
		c_roff(TERM_L_RED, character_type_name[p_ptr->character_type], 0, 0);
	}

	prt_lnum("Unspent Exp   ", p_ptr->exp,           11, 27, TERM_L_GREEN);


	/* Middle top */
	prt_lnum("Turns         ", turn,                 14, 27, TERM_L_BLUE);
	prt_lnum("Active Turns  ", p_ptr->total_turns - p_ptr->resting_turns,  15, 27, TERM_L_BLUE);
	prt_lnum("Resting Turns ", p_ptr->resting_turns, 16, 27, TERM_L_BLUE);


	/* Right top */
	prt_lnum("Gold          ", p_ptr->au,             9, 53, TERM_L_GREEN);


	/* Hitpoints, Mana */
	if (TRUE)
	{
		int warn = MAX(2, op_ptr->hitpoint_warn);

		int attr = TERM_L_GREEN;
		if (p_ptr->chp < (p_ptr->mhp * warn / 10)) attr = TERM_RED;
		else if (p_ptr->chp < p_ptr->mhp) attr = TERM_YELLOW;

		prt_num("Max Hitpoints    ", p_ptr->mhp, 9, 1, TERM_L_GREEN);
		prt_num("Cur Hitpoints    ", p_ptr->chp, 10, 1, attr);
	}
	if (TRUE)
	{
		int warn = MAX(2, op_ptr->hitpoint_warn);

		int attr = TERM_L_GREEN;
		if (p_ptr->csp < (p_ptr->msp * warn / 10)) attr = TERM_RED;
		else if (p_ptr->csp < p_ptr->msp) attr = TERM_YELLOW;

		prt_num("Max Mana         ", p_ptr->msp, 11, 1, TERM_L_GREEN);
		prt_num("Cur Mana         ", p_ptr->csp, 12, 1, attr);
	}
}


/*
 * Hack -- pass color info around this file
 */
static byte likert_color = TERM_WHITE;


/*
 * Returns a "rating" of x depending on y
 *
 * String lengths should not be longer than 10.
 */
static cptr likert(int x, int y)
{
	/* Paranoia */
	if (y <= 0) y = 1;


	/* Debug code  -EFG- */
	if ((x < -10000) || (x > 10000))
	{
		likert_color = TERM_RED;
		return ("GARBLED");
	}


	/* Negative value */
	if (x < 0)
	{
		likert_color = TERM_L_DARK;
		if (p_ptr->image) return ("Belcherous");
		else              return ("Awful");
	}

	/* Analyze the value */
	switch ((x / y))
	{
		case 0:
		{
			likert_color = TERM_RED;
			if (p_ptr->image) return ("Crappy");
			else              return ("Very Bad");
		}
		case 1:
		{
			likert_color = TERM_L_RED;
			if (p_ptr->image) return ("Sucky");
			else              return ("Bad");
		}
		case 2:
		{
			likert_color = TERM_ORANGE;
			if (p_ptr->image) return ("Uncool");
			else              return ("Poor");
		}
		case 3:
		{
			likert_color = TERM_ORANGE;
			if (p_ptr->image) return ("Iffy");
			else              return ("Iffy");
		}
		case 4:
		{
			likert_color = TERM_YELLOW;
			if (p_ptr->image) return ("So-so");
			else              return ("Fair");
		}
		case 5:
		{
			likert_color = TERM_YELLOW;
			if (p_ptr->image) return ("Cool");
			else              return ("Good");
		}
		case 6:
		case 7:
		{
			likert_color = TERM_YELLOW;
			if (p_ptr->image) return ("Cuspy");
			else              return ("Very Good");
		}
		case 8:
		case 9:
		case 10:
		{
			likert_color = TERM_L_GREEN;
			if (p_ptr->image) return ("Froody");
			else              return ("Excellent");
		}
		case 11:
		case 12:
		case 13:
		{
			likert_color = TERM_L_GREEN;
			if (p_ptr->image) return ("Radical");
			else              return ("Superb");
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			likert_color = TERM_BLUE;
			if (p_ptr->image) return ("Bodacious");
			else              return ("Heroic");
		}
		default:
		{
			likert_color = TERM_PURPLE;
			if (p_ptr->image) return ("Stupendous");
			else              return ("Legendary");
		}
	}
}


/*
 * Prints ratings on certain abilities
 *
 * This code is "imitated" elsewhere to "dump" a character sheet.
 */
static void display_player_various(void)
{
	int attr;
	int xthn, xthb, xtht, xdig, xsrh;
	int xdis, xdev, xsav, xstl;
	cptr desc;
	char fame_desc[DESC_LEN];

	object_type *o_ptr;


	/* Get melee weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* We are using a weapon */
	if (is_melee_weapon(o_ptr))
	{
		/* Fighting Skill (with current weapon) */
		xthn = p_ptr->skill_thn + (BTH_PLUS_ADJ * o_ptr->to_h);
	}

	/* We're using bare-handed combat */
	else
	{
		/* We are using a martial art */
		if (get_skill(p_ptr->barehand, 0, 100))
		{
			xthn = p_ptr->skill_thn + get_skill(p_ptr->barehand, 0, 40);
		}

		/* We're just using our fists */
		else
		{
			xthn = p_ptr->skill_thn / (p_ptr->power ? 2 : 1);
		}
	}

	/* Get missile launcher */
	o_ptr = &inventory[INVEN_BOW];

	/* Shooting Skill (with current bow and normal missile) */
	xthb = p_ptr->skill_thb + (o_ptr->to_h * BTH_PLUS_ADJ);

	/* Throwing Skill (approximate) */
	xtht = 4 * p_ptr->skill_tht / 3;

	/* Digging ability */
	xdig = p_ptr->skill_dig;

	/* Basic abilities */
	xdis = p_ptr->skill_dis;
	xdev = p_ptr->skill_dev;
	xsav = p_ptr->skill_sav;
	xstl = p_ptr->skill_stl;
	xsrh = p_ptr->skill_srh;

	put_str("Melee       :", 19, 1);
	desc = likert(xthn,8 + p_ptr->max_depth / 20);
	c_put_str(likert_color, desc, 19, 15);

	put_str("Shooting    :", 20, 1);
	desc = likert(xthb, 5 + p_ptr->max_depth / 15);
	c_put_str(likert_color, desc, 20, 15);

	put_str("Throwing    :", 21, 1);
	desc = likert(xtht, 6 + p_ptr->max_depth / 15);
	c_put_str(likert_color, desc, 21, 15);

	put_str("Digging     :", 22, 1);
	desc = likert(xdig, 10);
	c_put_str(likert_color, desc, 22, 15);

	put_str("Saving Throw:", 19, 27);
	desc = likert(xsav, 6);
	c_put_str(likert_color, desc, 19, 41);

	put_str("Stealth     :", 20, 27);
	if (p_ptr->aggravate)
	{
		c_put_str(TERM_L_RED, "Aggravate", 20, 41);
	}
	else
	{
		desc = likert(xstl, 1);
		c_put_str(likert_color, desc, 20, 41);
	}

	put_str("Perception  :", 21, 27);
	desc = likert(xsrh, 3 + p_ptr->max_depth / 40);
	c_put_str(likert_color, desc, 21, 41);

	put_str("Disarming   :", 22, 27);
	desc = likert(xdis, 5 + p_ptr->max_depth / 23);
	c_put_str(likert_color, desc, 22, 41);

	put_str("Magic Device:", 19, 53);
	desc = likert(xdev, 5 + p_ptr->max_depth / 40);
	c_put_str(likert_color, desc, 19, 67);

	put_str("Dodging     :", 20, 53);
	desc = likert(dodging_ability(300), 11 + p_ptr->max_depth / 20);
	c_put_str(likert_color, desc, 20, 67);

	put_str("Fame        :", 21, 53);
	get_fame_desc(&attr, fame_desc);
	c_put_str(attr, fame_desc, 21, 67);

	put_str("Infra-Vision:", 22, 53);
	if (use_metric) put_str(format("%d meters", p_ptr->see_infra * 3), 22, 67);
	else put_str(format("%d feet", p_ptr->see_infra * 10), 22, 67);
}

/*
 * Equippy chars
 */
static void display_player_equippy(int y, int x, bool extend)
{
	int i;

	byte a;
	char c;

	object_type *o_ptr;


	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < (extend ? INVEN_TOTAL : INVEN_SUBTOTAL); i++)
	{
		/* Object */
		o_ptr = &inventory[i];

		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;

		/* Skip the pouch */
		if (i == INVEN_POUCH) continue;

		/* Get attr/char for display */
		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Dump */
		(void)Term_putch(x + i - INVEN_WIELD, y, a, c);
	}
}


/*
 * Given a position on the equipment screen listing, return a flag.
 */
static int get_flag_here(int group, int member)
{
	/* Stats */
	if (group == 1)
	{
		if (member == 0) return (PVAL_STR);
		if (member == 1) return (PVAL_INT);
		if (member == 2) return (PVAL_WIS);
		if (member == 3) return (PVAL_DEX);
		if (member == 4) return (PVAL_CON);
		if (member == 5) return (PVAL_CHR);
	}

	/* Other pval-dependant qualities - left side */
	else if (group == 2)
	{
		if (member == 0) return (PVAL_STEALTH);
		if (member == 1) return (PVAL_INVIS);
		if (member == 2) return (PVAL_AWARE);
		if (member == 3) return (PVAL_DISARM);
		if (member == 4) return (PVAL_DEVICE);
		if (member == 5) return (PVAL_SPEED);

	}

	/* Other pval-dependant qualities - right side */
	else if (group == 3)
	{
		if (member == 0) return (PVAL_INFRA);
		if (member == 1) return (PVAL_TUNNEL);
		if (member == 2) return (PVAL_SAVE);
		if (member == 3) return (PVAL_MANA);
		if (member == 4) return (PVAL_LIGHT);
	}

	/* Non pval-dependant flags, set 1 */
	else if (group == 4)
	{
		if (member == 0) return (RES_ACID);
		if (member == 1) return (RES_ELEC);
		if (member == 2) return (RES_FIRE);
		if (member == 3) return (RES_COLD);
		if (member == 4) return (RES_POIS);
		if (member == 5) return (RES_FEAR);
		if (member == 6) return (RES_BLIND);
		if (member == 7) return (RES_CONFU);
	}

	/* Non pval-dependant flags, set 2 */
	else if (group == 5)
	{
		if (member == 0) return (RES_LITE);
		if (member == 1) return (RES_DARK);
		if (member == 2) return (RES_SOUND);
		if (member == 3) return (RES_SHARD);
		if (member == 4) return (RES_NEXUS);
		if (member == 5) return (RES_NETHR);
		if (member == 6) return (RES_CHAOS);
		if (member == 7) return (RES_DISEN);
	}

	/* Non pval-dependant flags, set 3 */
	else if (group == 6)
	{
		if (member == 0) return (SLOW_DIGEST);
		if (member == 1) return (FEATHER);
		if (member == 2) return (LITE);
		if (member == 3) return (REGEN);
		if (member == 4) return (TELEPATHY);
		if (member == 5) return (SEE_INVIS);
		if (member == 6) return (FREE_ACT);
		if (member == 7) return (HOLD_LIFE);
	}

	/* Non pval-dependant flags, set 4 */
	else if (group == 7)
	{
		if (member == 0) return (NOFUEL);
		if (member == 1) return (SOULSTEAL);
		if (member == 2) return (NOMAGIC);
		if (member == 3) return (TELEPORT);
		if (member == 4) return (AGGRAVATE);
		if (member == 5) return (DRAIN_EXP);

		if (member == 7) return (LIGHT_CURSE);
	}

	/* Flags related to melee weapons */
	else if (group == 8)
	{
		if (member ==  0) return (PVAL_BLOWS);
		if (member ==  1) return (ADD_SKILL);
		if (member ==  2) return (ADD_DEADLINESS);

		if (member ==  3) return (SLAY_ANIMAL);
		if (member ==  4) return (SLAY_EVIL);
		if (member ==  5) return (SLAY_UNDEAD);
		if (member ==  6) return (SLAY_DEMON);
		if (member ==  7) return (SLAY_ORC);
		if (member ==  8) return (SLAY_TROLL);
		if (member ==  9) return (SLAY_GIANT);
		if (member == 10) return (SLAY_DRAGON);
		if (member == 11) return (BRAND_ACID);
		if (member == 12) return (BRAND_ELEC);
		if (member == 13) return (BRAND_FIRE);
		if (member == 14) return (BRAND_COLD);
		if (member == 15) return (BRAND_POIS);
		if (member == 16) return (BLESSED);
		if (member == 17) return (VORPAL);
		if (member == 18) return (IMPACT);
		if (member == 19) return (THROWING);
		if (member == 20) return (TWO_HANDED_DES);
	}

	/* Flags related to missile weapons */
	else if (group == 9)
	{
		if (member ==  0) return (PVAL_MIGHT);
		if (member ==  1) return (PVAL_SHOTS);
		if (member ==  2) return (ADD_SKILL);
		if (member ==  3) return (ADD_DEADLINESS);

		if (member ==  4) return (SLAY_ANIMAL);
		if (member ==  5) return (SLAY_EVIL);
		if (member ==  6) return (SLAY_UNDEAD);
		if (member ==  7) return (SLAY_DEMON);
		if (member ==  8) return (SLAY_ORC);
		if (member ==  9) return (SLAY_TROLL);
		if (member == 10) return (SLAY_GIANT);
		if (member == 11) return (SLAY_DRAGON);
		if (member == 12) return (BRAND_ACID);
		if (member == 13) return (BRAND_ELEC);
		if (member == 14) return (BRAND_FIRE);
		if (member == 15) return (BRAND_COLD);
		if (member == 16) return (BRAND_POIS);
		if (member == 17) return (VORPAL);
		if (member == 18) return (IMPACT);
		if (member == 19) return (THROWING);
		if (member == 20) return (RETURNING);
	}

	/* This position is empty */
	return (-1);
}


/*
 * Given a flag index, return a short name.
 *
 * Names (including ':') are 6 chars long for most flags, and 12 for
 * flags affecting melee and missiles.
 *
 * Flags for which this function is not supposed to be called are
 * marked with an 'X'.
 */
static cptr short_flag_names[128] =
{
	/* Flags_pval */
	"X",
	"X",
	"X",
	"X",
	"X",
	"X",
	"X",
	"X",
	"Stlth:",	/* TTR_PVAL_STEALTH */
	"Aware:",	/* TR_PVAL_AWARE */
	"Infra:",	/* TR_PVAL_INFRA */
	"Tunnl:",	/* TR_PVAL_TUNNEL */
	"Speed:",	/* TR_PVAL_SPEED */
	"Invis:",	/* TR_PVAL_INVIS */
	"Disar:",	/* TR_PVAL_DISARM */
	"Devic:",	/* TR_PVAL_DEVICE */
	"Save :",	/* TR_PVAL_SAVE */
	"Mana :",	/* TR_PVAL_MANA */
	"Light:",	/* TR_PVAL_LIGHT */
	"",
	"Blows      :",	/* TR_PVAL_BLOWS */
	"Shots      :",	/* TR_PVAL_SHOTS */
	"Might      :",	/* TR_PVAL_MIGHT */
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"",

	/* Flags1 */
	"X",
	"X",
	"X",
	"X",
	"X",
	"X",
	"X",
	"X",
	"Slay Animal:",
	"Slay Evil  :",
	"Slay Undead:",
	"Slay Demon :",
	"Slay Orc   :",
	"Slay Troll :",
	"Slay Giant :",
	"Slay Dragon:",
	"",
	"",
	"",
	"Acid Brand :",
	"Elec Brand :",
	"Fire Brand :",
	"Cold Brand :",
	"Pois Brand :",
	"",
	"",
	"Returning  :",
	"Vorpal     :",
	"Throwing   :",
	"X",
	"X",
	"Two-handed :",

	/* Flags2 */
	"X",
	"X",
	"X",
	"X",
	"Acid :",	/* TR2_RES_ACID */
	"Elec :",	/* TR2_RES_ELEC */
	"Fire :",	/* TR2_RES_FIRE */
	"Cold :",	/* TR2_RES_COLD */
	"Pois :",	/* TR2_RES_POIS */
	"Light:",	/* TR2_RES_LITE */
	"Dark :",	/* TR2_RES_DARK */
	"Fear :",	/* TR2_RES_FEAR */
	"Blind:",	/* TR2_RES_BLIND */
	"Confu:",	/* TR2_RES_CONFU */
	"Sound:",	/* TR2_RES_SOUND */
	"Shard:",	/* TR2_RES_SHARD */
	"Nexus:",	/* TR2_RES_NEXUS */
	"Nethr:",	/* TR2_RES_NETHR */
	"Chaos:",	/* TR2_RES_CHAOS */
	"Disen:",	/* TR2_RES_DISEN */
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"X",
	"X",
	"X",
	"X",

	/* Flags3 */
	"Food :",	/* TR3_SLOW_DIGEST */
	"Feath:",	/* TR3_FEATHER */
	"Shine:",	/* TR3_LITE */
	"Regen:",	/* TR3_REGEN */
	"ESP  :",	/* TR3_TELEPATHY */
	"SeeIn:",	/* TR3_SEE_INVIS */
	"FrAct:",	/* TR3_FREE_ACT */
	"HLife:",	/* TR3_HOLD_LIFE */
	"Blessed    :",
	"Impact     :",
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	"NFuel:",	/* TR3_NOFUEL */
	"Steal:",	/* TR3_SOULSTEAL */
	"NoMag:",	/* TR3_NOMAGIC */
	"Telep:",	/* TR3_TELEPORT */
	"Aggra:",	/* TR3_AGGRAVATE */
	"Drain:",	/* TR3_DRAIN_EXP */
	"",
	"",
	"",
	"",
	"Curse:",	/* TR3_LIGHT_CURSE, etc */
	"X",
	"X"
};


/*
 * Special display, part 1
 *
 * This function is now pretty cludgy, but it provides more information -JM
 */
static void display_player_flag_info(void)
{
	int x, y, i, n;

	int row, col;
	int attr, attr_title;
	bool immune;
	bool bad_flag;
	bool resist;
	bool temp_resist;
	bool vuln;

	int flag;
	cptr name;

	u32b f[4];
	u32b f_player[4];
	u32b f_cancel[4];
	u32b f_vuln[4];

	/* Get player flags */
	player_flags(&f_player[1], &f_player[2], &f_player[3], TRUE, FALSE);

	/* Get "cancelled" flags */
	player_flags_cancel(&f_cancel[1], &f_cancel[2], &f_cancel[3], TRUE);

	player_flags_vulnerable(&f_vuln[1], &f_vuln[2], &f_vuln[3], TRUE);


	/* Four columns of non pval-dependant flags */
	for (x = 0; x < 4; x++)
	{
		/* Get top-left corner of this column */
		row = 12;
		col = 20 * x;

		/* Header */
		display_player_equippy(row++, col + 6, FALSE);
		c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 6);

		/* Eight rows */
		for (y = 0; y < 8; y++)
		{
			attr_title = TERM_WHITE;
			bad_flag = FALSE;
			immune = FALSE;
			resist = FALSE;
			temp_resist = FALSE;
			vuln = FALSE;

			/* Get flag index */
			flag = get_flag_here(4 + x, y);

			if ((flag / 32 == 3) && ((1L << (flag % 32)) >= TR3_SOULSTEAL)) bad_flag = TRUE;


			/* If no flag belongs in this position, we leave it empty */
			if (flag < 0)
			{
				/* Advance */
				row++;

				/* Next */
				continue;
			}

			/* Check equipment */
			for (n = 6, i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++, n++)
			{
				/* Get object */
				object_type *o_ptr = &inventory[i];

				attr = TERM_SLATE;

				/* Known flags */
				object_flags_known(o_ptr, &f[1], &f[2], &f[3]);

				/* Color columns by parity */
				if (i % 2) attr = TERM_L_WHITE;

				/* Nonexistent objects */
				if (!o_ptr->k_idx) attr = TERM_L_DARK;

				/* This object has the current flag */
				if (f[flag / 32] & (1L << (flag % 32)))
				{
					resist = TRUE;
					c_put_str(TERM_WHITE, "+", row, col + n);
				}

				/* It does not */
				else
				{
					c_put_str(attr, ".", row, col + n);
				}

				/* Special case -- Check immunities */
				if (flag == RES_ACID)
				{
					if (f[2] & (TR2_IM_ACID))
					{
						immune = TRUE;
						c_put_str(TERM_L_PURPLE, "*", row, col + n);
					}
				}
				if (flag == RES_ELEC)
				{
					if (f[2] & (TR2_IM_ELEC))
					{
						immune = TRUE;
						c_put_str(TERM_L_PURPLE, "*", row, col + n);
					}
				}
				if (flag == RES_FIRE)
				{
					if (f[2] & (TR2_IM_FIRE))
					{
						immune = TRUE;
						c_put_str(TERM_L_PURPLE, "*", row, col + n);
					}
				}
				if (flag == RES_COLD)
				{
					if (f[2] & (TR2_IM_COLD))
					{
						immune = TRUE;
						c_put_str(TERM_L_PURPLE, "*", row, col + n);
					}
				}

				/* Special case -- Check heavy and permanent curses */
				if (flag == LIGHT_CURSE)
				{
					if (f[3] & (TR3_PERMA_CURSE))
						c_put_str(TERM_RED, "*", row, col + n);
					else if (f[3] & (TR3_HEAVY_CURSE))
						c_put_str(TERM_YELLOW, "*", row, col + n);
				}
			}

			/* Default */
			c_put_str(TERM_SLATE, ".", row, col + n);

			/* Check flags */
			if (f_player[flag / 32] & (1L << (flag % 32)))
			{
				resist = TRUE;
				c_put_str(TERM_WHITE, "+", row, col + n);
			}

			/* Special case -- Check immunities */
			if (flag == RES_ACID)
			{
				if (f_player[2] & (TR2_IM_ACID))
				{
					immune = TRUE;
					c_put_str(TERM_L_PURPLE, "*", row, col + n);
				}
				else if (p_ptr->oppose_acid)
				{
					temp_resist = TRUE;
					c_put_str(TERM_GREEN, "+", row, col + n);
				}
			}
			if (flag == RES_ELEC)
			{
				if (f_player[2] & (TR2_IM_ELEC))
				{
					immune = TRUE;
					c_put_str(TERM_L_PURPLE, "*", row, col + n);
				}
				else if (p_ptr->oppose_elec)
				{
					temp_resist = TRUE;
					c_put_str(TERM_GREEN, "+", row, col + n);
				}
			}
			if (flag == RES_FIRE)
			{
				if (f_player[2] & (TR2_IM_FIRE))
				{
					immune = TRUE;
					c_put_str(TERM_L_PURPLE, "*", row, col + n);
				}
				else if (p_ptr->oppose_fire)
				{
					temp_resist = TRUE;
					c_put_str(TERM_GREEN, "+", row, col + n);
				}
			}
			if (flag == RES_COLD)
			{
				if (f_player[2] & (TR2_IM_COLD))
				{
					immune = TRUE;
					c_put_str(TERM_L_PURPLE, "*", row, col + n);
				}
				else if (p_ptr->oppose_cold)
				{
					temp_resist = TRUE;
					c_put_str(TERM_GREEN, "+", row, col + n);
				}
			}

			if (flag == RES_POIS && p_ptr->oppose_pois)
			{
				temp_resist = TRUE;
				c_put_str(TERM_GREEN, "+", row, col + n);
			}

			if (flag == RES_LITE && p_ptr->oppose_ethereal)
			{
				resist = TRUE;

				c_put_str(TERM_GREEN, "+", row, col + n);
			}

			if (flag == RES_DARK && p_ptr->oppose_ethereal)
			{
				resist = TRUE;
				c_put_str(TERM_GREEN, "+", row, col + n);
			}

			/* Hack -- Cancel immunities specially -JM */
			if (immune && (f_cancel[flag/32] & (1L << (flag % 32 - 4))))
			{
				immune = FALSE;
			}

			/* Handle vulnerabilities */
			if (f_vuln[flag / 32] & (1L << (flag % 32)))
			{
				vuln = TRUE;
				c_put_str(TERM_L_RED, "-", row, col + n);
			}


			/* Check to see if the character cancels this flag */
			if (f_cancel[flag / 32] & (1L << (flag % 32)))
			{
				/* For "good" flags, cancellation is bad. */
				attr = TERM_L_RED;
				/* For "bad" flags, cancellation is good */
				if (bad_flag)
					attr = TERM_L_BLUE;

				resist = FALSE;

				/* Note cancellation */
				if (temp_resist)
				{
					c_put_str(attr, "~", row, col + n);
				}
				else
					c_put_str(attr, "X", row, col + n);

			}

			/* Choose the correct color to display based on the level of resistance */
			if (!bad_flag)
			{
				if (immune)   attr_title = TERM_L_PURPLE;
				else if (temp_resist && resist && vuln)
					attr_title = TERM_GREEN;
				else if ((resist || temp_resist) && vuln)
					attr_title = TERM_YELLOW;
				else if (vuln)
					attr_title = TERM_L_RED;
				else if (temp_resist && resist)
					attr_title = TERM_L_BLUE;
				else if (temp_resist || resist)
					attr_title = TERM_GREEN;
			}
			else
			{
				if (immune || temp_resist || resist)
					attr_title = TERM_RED;
			}


			/* Get name of flag */
			name = short_flag_names[flag];

			/* Header (colorized) */
			c_put_str(attr_title, name, row, col);

			/* Advance */
			row++;
		}
	}
}



/*
 * Display internal stats, equipment and innate modifiers to them, and
 * the resulting adjusted and current stats.
 */
static void display_player_stat_info(void)
{
	int i, row, col, pval;
	int stat_col, stat;

	object_type *o_ptr;
	s16b k_idx;

	u32b f[4];

	byte a;
	char c;

	char buf[DESC_LEN];

	/* Assume no drained stats */
	bool drained = FALSE;

	/* Column */
	stat_col = 20;

	/* Row */
	row = 2;

	/* Print out the labels for the columns */
	c_put_str(TERM_WHITE, "Stat", row - 1, stat_col);
	c_put_str(TERM_BLUE, "Intrnl", row - 1, stat_col + 5);

	display_player_equippy(row - 2, stat_col + 12, FALSE);
	c_put_str(TERM_WHITE, "abcdefghijkl@", row - 1, stat_col + 12);

	c_put_str(TERM_L_GREEN, "Adjust", row - 1, stat_col + 26);


	/* Display the stats */
	for (stat = 0; stat < A_MAX; stat++)
	{
		/* Special treatment of drained stats */
		if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
		{
			/* Use lowercase stat name */
			c_put_str(TERM_WHITE, stat_names_reduced[stat], row + stat, stat_col);
		}

		/* Normal treatment of undrained stats */
		else
		{
			/* Assume uppercase stat name */
			c_put_str(TERM_WHITE, stat_names[stat], row + stat, stat_col);

			/* Indicate natural maximum */
			if (p_ptr->stat_max[stat] == 18 + 100)
			{
				put_str("!", row + stat, stat_col + 3);
			}
		}

		/* Display maximal intrinsic stats */
		cnv_stat(buf, sizeof(buf), p_ptr->stat_max[stat]);
		c_put_str(TERM_BLUE, buf, row + stat, stat_col + 5);

		/* Display maximal modified stats */
		i = modify_stat(p_ptr->stat_max[stat], p_ptr->stat_add[stat]);
		cnv_stat(buf, sizeof(buf), i);
		c_put_str(TERM_L_GREEN, buf, row + stat, stat_col + 26);

		/* If different, also display current modified stats */
		if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
		{
			/* Note drained stat */
			drained = TRUE;

			cnv_stat(buf, sizeof(buf), p_ptr->stat_use[stat]);
			c_put_str(TERM_YELLOW, buf, row + stat, stat_col + 33);
		}
	}

	/* Provide a header for drained stats, if any */
	if (drained)
	{
		c_put_str(TERM_YELLOW, "Currnt", row - 1, stat_col + 33);
	}


	/* Column */
	col = stat_col + 12;

	/* Process equipment (ignore quiver slots) */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		/* Access object */
		o_ptr = &inventory[i];

		/* Object kind */
		k_idx = o_ptr->k_idx;

		/* Known flags */
		object_flags_known(o_ptr, &f[1], &f[2], &f[3]);

		/* Initialize color based of sign of pval. */
		for (stat = 0; stat < A_MAX; stat++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';

			/* Get this object's modifier to the current stat, if any */
			pval = get_object_pval(o_ptr, TR_PVAL_STR << stat);

			/* Unknown objects display no pval */
			if (!object_known_p(o_ptr)) pval = 0;

			/* Stat is affected by this piece of equipment */
			if (pval)
			{
				/* Default */
				c = '*';

				/* Good */
				if (pval > 0)
				{
					/* Good */
					a = TERM_L_GREEN;

					/* Label boost */
					if (pval < 10) c = '0' + pval;
				}

				/* Bad */
				if (pval < 0)
				{
					/* Bad */
					a = TERM_RED;

					/* Label boost */
					if (pval < 10) c = '0' - pval;
				}
			}

			/* Sustain */
			if (f[1] & (TR1_SUST_STR << stat))
			{
				/* Dark green or yellow, "s" if no stat bonus. */
				if (pval >= 0) a = TERM_GREEN;
				else           a = TERM_YELLOW;
				if (c == '.') c = 's';
			}

			/* Dump proper character */
			(void)Term_putch(col, row + stat, a, c);
		}

		/* Advance */
		col++;
	}


	/* Player flags (non-pval) */
	player_flags(&f[1], &f[2], &f[3], TRUE, FALSE);

	/* Display the character's own stat modifiers and sustains */
	for (stat = 0; stat < A_MAX; stat++)
	{
		/* Default */
		a = TERM_SLATE;
		c = '.';

		/* Get total of innate and shapechange modifiers */
		pval = player_flags_pval(TR_PVAL_STR << stat, TRUE);


		/* Character has modifiers to this stat */
		if (pval)
		{
			/* Default */
			c = '*';

			/* Good */
			if (pval > 0)
			{
				/* Good */
				a = TERM_L_GREEN;

				/* Label boost */
				if (pval < 10) c = '0' + pval;
			}

			/* Bad */
			if (pval < 0)
			{
				/* Bad */
				a = TERM_RED;

				/* Label boost */
				if (pval < 10) c = '0' - pval;
			}
		}

		/* Sustain */
		if (f[1] & (TR1_SUST_STR << stat))
		{
			/* Dark green if stat is not also harmed, orange otherwise */
			if (pval >= 0) a = TERM_GREEN;
			else           a = TERM_ORANGE;

			/* Print a 's' if the stat is not modified */
			if (c == '.') c = 's';
		}

		/* Dump */
		(void)Term_putch(col, row + stat, a, c);
	}
}


/*
 * Display the modifiers to various pval-dependant qualities,
 * other than stats.
 */
static void display_player_pval_info(void)
{
	object_type *o_ptr;

	int k_idx;

	byte a;
	char c;

	int pval;

	int x, y, i, n;

	int row, col;

	int flag;
	cptr name;


	/* Two columns */
	for (x = 0; x < 2; x++)
	{
		/* Reset */
		row = 0;
		col = (x == 0 ? 0 : 60);

		/* Header */
		display_player_equippy(row++, col + 6, FALSE);
		c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 6);

		/* Eight rows */
		for (y = 0; y < 8; y++)
		{
			/* Get flag index */
			flag = get_flag_here(2 + x, y);

			/* If no flag belongs in this position, we leave it empty */
			if (flag < 0)
			{
				/* Advance */
				row++;

				/* Next */
				continue;
			}

			/* Get name of flag */
			name = short_flag_names[flag];

			/* Header */
			c_put_str(TERM_WHITE, name, row, col);

			/* Process equipment (ignore quiver slots) */
			for (n = 6, i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++, n++)
			{
				/* Access object */
				o_ptr = &inventory[i];

				/* Object kind */
				k_idx = o_ptr->k_idx;

				/* Default */
				a = TERM_SLATE;
				c = '.';

				/* Get the modifier to the current attribute, if any */
				pval = get_object_pval(o_ptr, (1L << (flag % 32)));

				/* Unknown objects display no pval */
				if (!object_known_p(o_ptr)) pval = 0;

				/* Attribute is affected by this piece of equipment */
				if (pval)
				{
					/* Default */
					c = '*';

					/* Good */
					if (pval > 0)
					{
						/* Good */
						a = TERM_L_GREEN;

						/* Label boost */
						if (pval < 10) c = '0' + pval;
					}

					/* Bad */
					if (pval < 0)
					{
						/* Bad */
						a = TERM_RED;

						/* Label boost */
						if (pval < 10) c = '0' - pval;
					}
				}

				/* Dump proper character */
				(void)Term_putch(col + n, row, a, c);
			}

			/* Default */
			a = TERM_SLATE;
			c = '.';

			/* Get total of innate and shapechange modifiers */
			pval = player_flags_pval((1L << (flag % 32)), TRUE);

			/* Attribute is affected by this piece of equipment */
			if (pval)
			{
				/* Default */
				c = '*';

				/* Good */
				if (pval > 0)
				{
					/* Good */
					a = TERM_L_GREEN;

					/* Label boost */
					if (pval < 10) c = '0' + pval;
				}

				/* Bad */
				if (pval < 0)
				{
					/* Bad */
					a = TERM_RED;

					/* Label boost */
					if (pval < 10) c = '0' - pval;
				}
			}

			/* Dump proper character */
			(void)Term_putch(col + n, row, a, c);

			/* Advance */
			row++;
		}
	}
}

/*
 * Display the combat section of the character screen.
 *
 * Note:  We display combat-related flags for all equipped items, despite
 * the fact that we currently do not allow many equipment objects to affect
 * melee.  The reasons for this include both easier debugging and automatic
 * support for, say, rings that impart permanent slays and brands.
 *
 * Be sure to handle martial arts correctly.
 */
static void display_player_combat(void)
{
	int x, y, i, n;

	int row, col;

	int flag;
	cptr name;
	int attr, attr_title;

	u32b f[4];
	u32b f_player[4];
	u32b f_cancel[4];


	/* Get player flags */
	player_flags(&f_player[1], &f_player[2], &f_player[3], TRUE, FALSE);

	/* Get "cancelled" flags */
	player_flags_cancel(&f_cancel[1], &f_cancel[2], &f_cancel[3], TRUE);


	/* Two columns of combat flags */
	for (x = 0; x < 2; x++)
	{
		int equip_limit = (x == 0) ? INVEN_SUBTOTAL : INVEN_TOTAL;

		/* Get top-left corner of this column */
		row = 0;
		col = 4 + x * 36;

		/* Header (extended for archery) */
		display_player_equippy(row++, col + 12, x == 1);
		if (x == 0)
		{
			c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 12);
		}
		else
		{
			c_put_str(TERM_WHITE, "abcdefghijkl 1234567890@", row++, col + 12);
		}

		/* Print the rows */
		for (y = 0; y < 22; y++)
		{
			attr_title = TERM_WHITE;

			/* Get flag index */
			flag = get_flag_here(8 + x, y);

			/* If no flag belongs in this position, we leave it empty */
			if (flag < 0)
			{
				/* Advance */
				row++;

				/* Next */
				continue;
			}

			/* Check equipment */
			for (n = 12, i = INVEN_WIELD; i < equip_limit; i++, n++)
			{
				object_type *o_ptr;
				char c;

				attr = TERM_SLATE;

				/* Always skip the pouch */
				if (i == INVEN_POUCH) continue;

				/* Checking archery */
				if (x != 0)
				{
					if (i == INVEN_WIELD) continue;
					if (i == INVEN_ARM)   continue;
				}

				/* Checking melee */
				else
				{
					if (i == INVEN_BOW) continue;
				}

				/* Get object */
				o_ptr = &inventory[i];

				/* Known flags */
				object_flags_known(o_ptr, &f[1], &f[2], &f[3]);

				/* Color columns by parity */
				if (i <= INVEN_SUBTOTAL)
				{
					if (i % 2) attr = TERM_L_WHITE;
				}
				else
				{
					if (!(i % 2)) attr = TERM_L_WHITE;
				}

				/* Nonexistent objects */
				if (!o_ptr->k_idx) attr = TERM_L_DARK;

				/* Filler */
				c_put_str(attr, ".", row, col + n);

				/* Special case -- display pvals for some flags */
				if (flag < 32)
				{
					/* Get the modifier to the current attribute, if any */
					int pval = get_object_pval(o_ptr, (1L << (flag % 32)));

					/* Attribute is affected by this piece of equipment */
					if (pval)
					{
						/* Default */
						c = '*';

						/* Good */
						if (pval > 0)
						{
							/* Good */
							attr = TERM_L_GREEN;

							/* Label boost */
							if (pval < 10) c = '0' + pval;
						}

						/* Bad */
						if (pval < 0)
						{
							/* Bad */
							attr = TERM_RED;

							/* Label boost */
							if (pval < 10) c = '0' - pval;
						}

						/* Display the pval */
						(void)Term_putch(col + n, row, attr, c);
					}
				}

				/* Special case -- display plusses for some flags */
				else if (flag >= 160)
				{
					int plus = 0;

					if      (flag == ADD_SKILL)      plus = o_ptr->to_h;
					else if (flag == ADD_DEADLINESS) plus = o_ptr->to_d;

					/* This piece of equipment gives plusses */
					if ((plus) && (object_known_p(o_ptr)))
					{
						/* Default */
						c = '*';

						/* Good */
						if (plus > 0)
						{
							/* Good */
							attr = TERM_L_GREEN;

							/* Label boost */
							if (plus < 10) c = '0' + plus;
						}

						/* Bad */
						if (plus < 0)
						{
							/* Bad */
							attr = TERM_RED;

							/* Label boost */
							if (plus < 10) c = '0' - plus;
						}

						/* Display the pval */
						(void)Term_putch(col + n, row, attr, c);
					}
				}

				/* A normal flag */
				else
				{
					/* This object has the current flag */
					if (f[flag / 32] & (1L << (flag % 32)))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_WHITE, "+", row, col + n);
					}
				}

				/* Special case -- Check throwing */
				if ((flag == THROWING))
				{
					if (f[1] & (TR1_PERFECT_BALANCE))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_WHITE, "*", row, col + n);
					}
				}
				/* Special case -- Check two-handed wield (melee column) */
				if ((x == 0) && (flag == TWO_HANDED_DES))
				{
					if (f[1] & (TR1_TWO_HANDED_REQ))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_WHITE, "*", row, col + n);
					}
				}

				/* Special case -- display the heavy brands and slays */
				if (flag == BRAND_FIRE)
				{
					if (f[1] & (TR1_BRAND_FLAME))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_WHITE, "*", row, col + n);
					}
				}
				if (flag == BRAND_POIS)
				{
					if (f[1] & (TR1_BRAND_VENOM))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_WHITE, "*", row, col + n);
					}
				}
				if (flag == SLAY_DRAGON)
				{
					if (f[1] & (TR1_KILL_DRAGON))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_WHITE, "*", row, col + n);
					}
				}
			}

			/* Default */
			c_put_str(TERM_SLATE, ".", row, col + n);

			/* Check normal flags */
			if ((flag >= 32) && (flag < 160))
			{
				if (f_player[flag / 32] & (1L << (flag % 32)))
					c_put_str(TERM_WHITE, "+", row, col + n);
			}

			/* Check pval-dependant flags */
			else if (flag < 32)
			{
				/* Get the modifier to the current attribute, if any */
				int pval = player_flags_pval((1L << (flag % 32)), TRUE);

				/* Attribute is affected by this piece of equipment */
				if (pval)
				{
					char c;

					/* Default */
					c = '*';
					attr = TERM_SLATE;

					/* Good */
					if (pval > 0)
					{
						/* Good */
						attr = TERM_L_GREEN;

						/* Label boost */
						if (pval < 10) c = '0' + pval;
					}

					/* Bad */
					if (pval < 0)
					{
						/* Bad */
						attr = TERM_RED;

						/* Label boost */
						if (pval < 10) c = '0' - pval;
					}

					/* Display the pval */
					(void)Term_putch(col + n, row, attr, c);
				}
			}

			/* Special case -- Check heavy and icky wield */
			if (flag == ADD_SKILL)
			{
				if ((x == 0) && (p_ptr->icky_wield))
					c_put_str(TERM_RED, "X", row, col + n);
				else if ((x == 0) && (p_ptr->heavy_wield))
					c_put_str(TERM_ORANGE, "X", row, col + n);

				else if ((x == 1) && (p_ptr->heavy_shoot))
					c_put_str(TERM_ORANGE, "X", row, col + n);
			}

			/* Check to see if the character cancels this flag */
			if ((flag >= 32) && (flag < 160))
			{
				if (f_cancel[flag / 32] & (1L << (flag % 32)))
				{
					/* Note cancellation */
					c_put_str(TERM_RED, "X", row, col + n);
				}
			}

			/* Get name of flag */
			if (flag < 128)
			{
				/* Get name of flag */
				name = short_flag_names[flag];
			}
			else if (flag == ADD_SKILL)      name = "Add Skill  :";
			else if (flag == ADD_DEADLINESS) name = "Deadliness :";
			else name = "X";

			/* Header */
			c_put_str(attr_title, name, row, col);

			/* Advance */
			row++;
		}
	}
}


/*
 * Build a string describing this character's best kill.
 */
static bool best_kill_description(char *kill_desc, size_t len, bool split_desc)
{
	/* Check for slain opponent (allow OOD non-uniques) */
	int i = slain_opponent(FALSE);

	/* Character has slain an opponent */
	if (i)
	{
		monster_race *r_ptr = &r_info[i];

		/* Get monster race name */
		cptr name = (r_name + r_ptr->name);

		/* Note the kill */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			(void)strnfmt(kill_desc, len, "Slayer of%c%s",
				(split_desc ? '\n' : ' '), name);
		}
		else
		{
			(void)strnfmt(kill_desc, len, "Slayer of a level %d%c%s",
				r_ptr->level, (split_desc ? '\n' : ' '), name);
		}

		/* We have a worthy kill */
		return (TRUE);
	}

	/* No kills worthy of recording */
	return (FALSE);
}


/*
 * Display the character on the screen (various modes)
 *
 * The top two and bottom two lines are left blank.
 *
 * Mode 0 = standard display with skills
 * Mode 1 = standard display with history
 * Mode 2 = special display with flags
 * Mode 3 = combat information
 */
void display_player(int mode, bool change_display)
{
	int i;

	char kill_desc[DESC_LEN];


	/* Center the display and use the standard view */
	if (change_display) display_change(DSP_REMEMBER | DSP_CLEAR | DSP_NORM | DSP_CX, 80, 0);

	/* Clear the display (the side-window) */
	else Term_clear();

	/* Verify mode XXX XXX */
	mode = (mode % 4);

	/* Standard */
	if ((mode == 0) || (mode == 1))
	{
		char buf[DESC_LEN];

		cptr title = get_title(99, FALSE, TRUE);
		bool quotes = (title[0] == '\"');

		/* Build a name and title string */
		(void)strnfmt(buf, sizeof(buf), "%s\n %s%s",
		        (op_ptr->full_name ? op_ptr->full_name : "Anonymous"),
		        (quotes ? "" : "the "), title);

		/* No need to insert a return in a short string */
		if (strlen(buf) < 20)
		{
			for (i = 0; i < 80; i++)
			{
				if (!buf[i]) break;
				if (buf[i] == '\n') buf[i] = ',';
			}
		}
		else strcat(buf, " ");

		/* Print character's name and title (may take up to two lines) */
		move_cursor(2, 0);
		c_roff_centered(TERM_L_BLUE, buf, 0, 25);

		/* Print race and sex */
		put_str("Gender :", 5, 1);
		put_str("Race   :", 6, 1);
		c_put_str(TERM_L_BLUE, format("%.15s", sp_ptr->title), 5, 10);
		c_put_str(TERM_L_BLUE, format("%.15s", rp_ptr->title), 6, 10);

		/* Print realm (if any) */
		if (p_ptr->realm)
		{
			if (p_ptr->realm == MAGE)   strcpy(buf, "Sorcery");
			if (p_ptr->realm == PRIEST) strcpy(buf, "Piety");
			if (p_ptr->realm == DRUID)  strcpy(buf, "Nature Magic");
			if (p_ptr->realm == NECRO)  strcpy(buf, "Necromancy");

			put_str("Realm  :", 7, 1);
			c_put_str(realm_color(), format("%.15s", buf), 7, 10);
		}


		/* Age (or Deaths), Height, Weight */
		if (p_ptr->deaths)
		{
			prt_num("Lives  :         ", (int)p_ptr->deaths, 2, 27, TERM_L_RED);
		}
		else
		{
			prt_num("Age    :         ", (int)p_ptr->age, 2, 27, TERM_L_BLUE);
		}

		put_str("Height :", 3, 27);
		put_str("Weight :", 4, 27);

		if (use_metric)	/* A GSNband idea. */
		{
			c_put_str(TERM_L_BLUE, format("%5d cm", p_ptr->ht * 254 / 100), 3, 42);
			c_put_str(TERM_L_BLUE, format("%5d kg", p_ptr->wt * 10 / 22), 4, 42);
		}
		else
		{
			c_put_str(TERM_L_BLUE,
				format("%2d ft, %d in", p_ptr->ht / 12, p_ptr->ht % 12),
				3, 38 + (p_ptr->ht % 12 < 10 ? 1 : 0));
			c_put_str(TERM_L_BLUE, format("%5d lb", p_ptr->wt), 4, 42);
		}

		/* Get the slay string, if any (display on two lines) */
		if (best_kill_description(kill_desc, sizeof(kill_desc), TRUE))
		{
			move_cursor(6, 0);
			c_roff_centered(TERM_L_BLUE, kill_desc, 26, 51);
		}

		/* Mention social class if character has no great victories */
		else
		{
			prt_num("Social Class :   ", (int)p_ptr->sc, 5, 27, TERM_L_BLUE);
		}


		/* Display the stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Special treatment of "injured" stats */
			if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
			{
				int value;

				/* Use lowercase stat name */
				put_str(stat_names_reduced[i], 2 + i, 53);

				/* Get the current stat */
				value = p_ptr->stat_use[i];

				/* Obtain the current stat (modified) */
				cnv_stat(buf, sizeof(buf), value);

				/* Display the current stat (modified) */
				c_put_str(TERM_YELLOW, buf, 2 + i, 61);

				/* Acquire the max stat */
				value = modify_stat(p_ptr->stat_max[i], p_ptr->stat_add[i]);

				/* Obtain the maximum stat (modified) */
				cnv_stat(buf, sizeof(buf), value);

				/* Display the maximum stat (modified) */
				c_put_str(TERM_L_GREEN, buf, 2 + i, 70);
			}

			/* Normal treatment of "normal" stats */
			else
			{
				/* Assume uppercase stat name */
				put_str(stat_names[i], 2 + i, 53);

				/* Indicate natural maximum */
				if (p_ptr->stat_max[i] == 18+100)
				{
					put_str("!", 2 + i, 56);
				}

				/* Obtain the current stat (modified) */
				cnv_stat(buf, sizeof(buf), p_ptr->stat_use[i]);

				/* Display the current stat (modified) */
				c_put_str(TERM_L_GREEN, buf, 2 + i, 61);
			}
		}

		/* Extra info */
		display_player_middle();

		/* Display "history" info */
		if (mode == 1)
		{
			/* Header */
			put_str("(Character Background)", 18, 28);

			/* History */
			roff("\n", 0, 0);
			roff(p_ptr->history, 7, 73);
		}

		/* Display "various" info */
		else
		{
			put_str("(Character Abilities)", 18, 28);

			display_player_various();
		}
	}

	/* Special */
	else if (mode == 2)
	{
		/* Dump the info */
		display_player_pval_info();
		display_player_stat_info();
		display_player_flag_info();
	}

	/* Special */
	else if (mode == 3)
	{
		/* Dump the info */
		display_player_combat();
	}

	/* Restore previous display */
	if (change_display) display_change(DSP_RESTORE, 0, 0);
}



/*
 * Dump a textual description of the cheat and difficulty options.
 */
static bool file_character_options(FILE *fff)
{
	cptr desc, desc2;

	/* Assume that nothing is printed in this section */
	bool flag = FALSE;


	/* Know all about monsters */
	if (cheat_know)
	{
		fprintf(fff, "You know everything about monsters.");
		if ((cheat_wizard) || (cheat_debug) || (cheat_borg))
			fprintf(fff, "  ");
		else fprintf(fff, "\n\n");

		flag = TRUE;
	}

	/* Wizard mode */
	if (cheat_wizard)
	{
		fprintf(fff, "You have used wizard mode.");
		if ((cheat_debug) || (cheat_borg)) fprintf(fff, "  ");
		else fprintf(fff, "\n\n");

		flag = TRUE;
	}

	/* Debug mode */
	if (cheat_debug)
	{
		fprintf(fff, "You have used debug mode.");
		if (cheat_borg) fprintf(fff, "  ");
		else fprintf(fff, "\n\n");

		flag = TRUE;
	}

	/* Borg mode */
	if (cheat_borg)
	{
		fprintf(fff, "The %s Borg ran this character.\n\n", VERSION_NAME);

		flag = TRUE;
	}

	if (no_skill_cap && (calc_max_power() == 100))
	{
		fprintf(fff, "You have transcended the limits of normal players.\n\n");

		flag = TRUE;
	}


	/* Rolled for stats, etc. a fairly large number of times */
	if (p_ptr->birth_roll_requirement >= 2500L)
	{
		fprintf(fff,
			"You were rolled up %ld times before being accepted.\n",
			p_ptr->birth_roll_requirement);

		flag = TRUE;
	}

	/* Rolled very few times */
	else if ((p_ptr->birth_roll_requirement >= 1L) &&
	         (p_ptr->birth_roll_requirement < 10L))
	{
		if (p_ptr->birth_roll_requirement == 1L)
		{
			fprintf(fff, "You were rolled up exactly once.\n");
		}
		else
		{
			fprintf(fff, "You were rolled up only %ld times.\n",
				p_ptr->birth_roll_requirement);
		}
		flag = TRUE;
	}

	/* We do not print out starting gold, because high gold often means low starting stats */

	/* Ironman play */
	if (p_ptr->character_type == PCHAR_IRONMAN)
	{
		if ((p_ptr->total_winner) || (p_ptr->is_dead)) desc = "refused";
		else desc = "refuse";
		fprintf(fff,
			"You %s to return to the town or go up any stairs until victorious.\n",
			desc);

		flag = TRUE;
	}

	/* No stores */
	if (birth_no_stores)
	{
		if ((p_ptr->total_winner) || (p_ptr->is_dead))
		{
			desc = "used";
			desc2 = "had";
		}
		else
		{
			desc = "use";
			desc2 = "have";
		}
		fprintf(fff,
			"You %s no stores and %s no home to store things in.\n",
			desc, desc2);

		flag = TRUE;
	}

	/* No artifacts */
	if (birth_no_artifacts)
	{
		if ((p_ptr->total_winner) || (p_ptr->is_dead)) desc = "played";
		else desc = "are playing";
		fprintf(fff, "You %s in a world without artifacts.\n", desc);

		flag = TRUE;
	}

	/* No return stairs */
	if (birth_no_return_stair)
	{
		if ((p_ptr->total_winner) || (p_ptr->is_dead)) desc = "played";
		else desc = "are playing";
		fprintf(fff, "You %s without connected stairs.\n", desc);

		flag = TRUE;
	}

	/* Monster know all about you */
	if (birth_smart_cheat)
	{
		if ((p_ptr->total_winner) || (p_ptr->is_dead))
		{
			desc = "knew";
			desc2 = "used";
		}
		else
		{
			desc = "know";
			desc2 = "use";
		}
		fprintf(fff,
			"Your opponents always %s your weaknesses, and %s them to choose the\nbest attacks.\n", desc, desc2);

		flag = TRUE;
	}

	/* Note if we printed anything */
	return (flag);
}




/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(cptr name, bool full)
{
	int i, x, y, x1, y1;
	int cnt;
	int offset_x;

	byte a;
	char c;

	int fd;

	FILE *fff = NULL;

	store_type *st_ptr = &store[STORE_HOME];

	byte (*old_xchar_hook)(byte c) = Term->xchar_hook;

	char o_name[DESC_LEN];

	char buf[1024];

	/* We use either ascii or system-specific encoding */
	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;

	int speed;


	/* Unused parameter */
	(void)full;

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Check for existing file */
	fd = fd_open(buf, O_RDONLY);

	/* Existing file */
	if (fd >= 0)
	{
		char out_val[DESC_LEN];

		/* Close the file */
		(void)fd_close(fd);

		/* Build query */
		(void)strnfmt(out_val, sizeof(out_val), "Replace existing file %s?", buf);

		/* Ask */
		if (get_check(out_val)) fd = -1;
	}

	/* Open the non-existing file */
	if (fd < 0) fff = my_fopen(buf, "w");


	/* Invalid file */
	if (!fff) return (-1);


	/* Activate display centering */
	screen_center_x(80);

	/* Remember offset  XXX XXX */
	offset_x = Term->offset_x;

	/* De-activate display centering */
	screen_center_x(0);



 	/* Display the requested encoding -- ASCII or system-specific */
 	if (!xchars_to_file) Term->xchar_hook = NULL;

	/* Begin dump */
	fprintf(fff, "                 [%s %s Character Dump]\n\n",
	        VERSION_NAME, VERSION_STRING);

	/* Print a crown */
	if (p_ptr->total_winner)
	{
		fprintf(fff, "                                  #\n");
		fprintf(fff, "                                #####\n");
		fprintf(fff, "                                  #\n");
		fprintf(fff, "                            ,,,  $$$  ,,,\n");
		fprintf(fff, "                        ,,=$   \"$$$$$\"   $=,,\n");
		fprintf(fff, "                      ,$$        $$$        $$,\n");
		fprintf(fff, "                      *>         <*>         <*\n");
		fprintf(fff, "                      $$         $$$         $$\n");
		fprintf(fff, "                      \"$$        $$$        $$\"\n");
		fprintf(fff, "                       \"$$       $$$       $$\"\n");
		fprintf(fff, "                        *#########*#########*\n");
		fprintf(fff, "                        *#########*#########*\n\n");


		/* Describe the cheating and difficulty options */
		(void)file_character_options(fff);
	}

	/* Display player */
	display_player(0, TRUE);

	/* Dump part of the screen */
	for (y = 2; y < 22; y++)
	{
		/* Jump to the correct screen row */
		y1 = Term->offset_y + y;

		/* Dump each row */
		for (x = 1, cnt = 0; x < 78; x++)
		{
			/* Jump to the correct screen column */
			x1 = offset_x + x;

			/* Hack -- skip some columns */
			if ((x == 25) || (x == 51)) continue;

			/* Hack -- skip some more columns */
			if ((y >= 18) && ((x == 10) || (x == 11))) continue;
			if ((y <=  7) && ((x == 57) || (x == 58))) continue;
			if ((y >= 9) && (y <= 15) && ((x == 69) || (x == 70)))
				continue;


			/* Get the attr/char */
			(void)(Term_what(x1, y1, &a, &c));

			/* Dump it */
			buf[cnt++] = c;
		}

		/* Back up over spaces */
		while ((cnt > 0) && (buf[cnt-1] == ' ')) --cnt;

		/* Terminate */
		buf[cnt] = '\0';

		/* End the row */
		x_fprintf(fff, encoding, "%s\n", format_literal(buf));
	}
	fprintf(fff, "\n");

	/* Get speed */
	speed = p_ptr->pspeed;

	/* Hack -- undo the sneaking slowdown */
	if (p_ptr->sneaking) speed += 5;

	/* Fast or slow */
	if (speed != 110) (void)strnfmt(buf, sizeof(buf), "%+d", (speed - 110));
	else              (void)strcpy(buf, "normal");

	/* Display various things */
	fprintf(fff, "   Speed       : %s\n", buf);
	fprintf(fff, "   Armor      : %d\n", p_ptr->dis_ac + p_ptr->dis_to_a);
	fprintf(fff, "   Kills       : %ld\n", p_ptr->total_kills);

	/* Display time elapsed */
	if (TRUE)
	{
		s32b len = 10L * TOWN_DAWN;
		s32b tick = turn % len;

		s32b day = turn / len;
		s32b hour = (  24L * tick / len) % 24;
		s32b min =  (1440L * tick / len) % 60;

		fprintf(fff,
			"   Time Elapsed: %ld days, %ld hours, %ld minutes   (%ld turns)\n\n",
				day, hour, min, turn);
	}


	/* Character attributes  (This section needs some work) */
	fprintf(fff, "\n  [Character Attributes]\n\n");

	/* Display player */
	display_player(2, TRUE);

	/* Dump part of the screen -- group #1 */
	for (y = 1; y < 9; y++)
	{
		/* Jump to the correct screen row */
		y1 = Term->offset_y + y;

		/* Dump each row */
		for (x = 0, cnt = 0; x < 60; x++)
		{
			/* Jump to the correct screen column */
			x1 = offset_x + x;

			/* Get the attr/char */
			(void)(Term_what(x1, y1, &a, &c));

			/* Dump it */
			buf[cnt++] = c;

			/* Hack -- insert some blank space for neatness */
			if (x == 19)
			{
				buf[cnt] = '\0';
				strcat(buf, "       ");
				cnt += 7;
			}
		}

		/* Back up over spaces */
		while ((cnt > 0) && (buf[cnt-1] == ' ')) --cnt;

		/* Terminate */
		buf[cnt] = '\0';

		/* End the row */
		x_fprintf(fff, encoding, "%s\n", format_literal(buf));
	}

	/* Dump part of the screen -- group #2 */
	for (y = 0; y < 9; y++)
	{
		/* Start of string */
		cnt = 0;


		/* Jump to the correct screen row */
		y1 = Term->offset_y + y + 1;

		/* Grab the first set */
		for (x = 60; x < 80; x++)
		{
			/* Jump to the correct screen column */
			x1 = offset_x + x;

			/* Get the attr/char */
			(void)(Term_what(x1, y1, &a, &c));

			/* Dump it */
			buf[cnt++] = c;
		}


		/* Insert some blank space for neatness */
		buf[cnt] = '\0';
		strcat(buf, "                    ");
		cnt += 20;


		/* Jump to the correct screen row */
		y1 = Term->offset_y + y + 13;

		/* Grab the second set */
		for (x = 60; x < 80; x++)
		{
			/* Jump to the correct screen column */
			x1 = offset_x + x;

			/* Get the attr/char */
			(void)(Term_what(x1, y1, &a, &c));

			/* Dump it */
			buf[cnt++] = c;
		}


		/* Back up over spaces */
		while ((cnt > 0) && (buf[cnt-1] == ' ')) --cnt;

		/* Terminate */
		buf[cnt] = '\0';

		/* End the row */
		x_fprintf(fff, encoding, "%s\n", format_literal(buf));
	}

	/* Spacing */
	fprintf(fff, "\n");


	/* Dump part of the screen -- group #3 */
	for (y = 13; y < 22; y++)
	{
		/* Jump to the correct screen row */
		y1 = Term->offset_y + y;

		/* Dump each row */
		for (x = 0; x < 60; x++)
		{
			/* Jump to the correct screen column */
			x1 = offset_x + x;

			/* Get the attr/char */
			(void)(Term_what(x1, y1, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Back up over spaces */
		while ((x > 0) && (buf[x-1] == ' ')) --x;

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		x_fprintf(fff, encoding, "%s\n", format_literal(buf));
	}


	/* Skip two lines */
	fprintf(fff, "\n\n");


	/* Dump most recent messages */
	if (TRUE)
	{
		s16b msgs;

		/* Get total number of saved messages */
		i = message_num();

		/* Clean up the player display  XXX */
		display_player(0, TRUE);

		/* Move cursor */
		move_cursor(0, 0);

		/* Suggest recall of 15 messages */
		get_quantity_default = MIN(i, 15);
		msgs = (s16b)get_quantity("Recall how many recent messages?", 0, i);

		/* Recall if desired */
		if (msgs > 0)
		{
			/* Dump the messages */
			fprintf(fff, "  [Last Messages]\n\n");
			while (msgs-- > 0)
			{
				(void)my_strcpy(buf, message_str(msgs), sizeof(buf));
				x_fprintf(fff, encoding, "%s\n", format_literal(buf));
			}
			fprintf(fff, "\n\n");
		}
	}


	/* Skills header */
	fprintf(fff, "  [Skills]\n\n");

	/* Print the active skills */
	for (i = 0; i < NUM_SK_USED; i++)
	{
		/* Nothing's been invested in this skill */
		if (!p_ptr->pskills[i].max) continue;

		/* Build a skill listing */
		(void)strnfmt(buf, sizeof(buf), "%-18s: %3d%%", skill_info[i].name,
			(int)p_ptr->pskills[i].cur);

		/* Note drained skills */
		if (p_ptr->pskills[i].cur != p_ptr->pskills[i].max)
		{
			strcat(buf, format("    (max %d%%)", p_ptr->pskills[i].max));
		}

		x_fprintf(fff, encoding, "%s\n", format_literal(buf));
	}
	fprintf(fff, "\n");


	/* Oaths */
	if (p_ptr->oath)
	{
		if (p_ptr->oath & (OATH_OF_IRON))
			fprintf(fff, "%s\n", "You have taken the Oath of Iron.");
		if (p_ptr->oath & (OATH_OF_SORCERY))
			fprintf(fff, "%s\n", "You have taken the Oath of Sorcery.");
		if (p_ptr->oath & (COVENANT_OF_FAITH))
			fprintf(fff, "%s\n", "You have subscribed to the Covenant of Faith.");
		if (p_ptr->oath & (YAVANNAS_FELLOWSHIP))
			fprintf(fff, "%s\n", "You have joined Yavanna's Fellowship.");
		if (p_ptr->oath & (BLACK_MYSTERY))
			fprintf(fff, "%s\n", "You have bound yourself to the Black Mystery.");
		if (p_ptr->oath & (BURGLARS_GUILD))
			fprintf(fff, "%s\n", "You have joined the Burglar's Guild.");
	}

	fprintf(fff, "\n\n");


	/* Quests */
	fprintf(fff, "  [Quests]\n\n");
	do_cmd_knowledge_quests(fff);
	fprintf(fff, "\n\n");

	/* Dump the equipment */
	if (p_ptr->equip_cnt)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			if (!inventory[i].k_idx) continue;

			/* Describe the object (truncate to 72 columns */
			object_desc(o_name, sizeof(o_name), &inventory[i], TRUE, 3);
			o_name[72] = '\0';
			x_fprintf(fff, encoding, "%^s\n", format_literal(o_name));

			/* Display special object attributes */
			dump_obj_attrib(fff, &inventory[i], (p_ptr->is_dead ? 1 : 0));
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	fprintf(fff, "  [Character Inventory]\n\n");
	for (i = 0; i < INVEN_PACK; i++)
	{
		if (!inventory[i].k_idx) break;

		/* Describe the object (truncate to 72 columns */
		object_desc(o_name, sizeof(o_name), &inventory[i], TRUE, 3);
		o_name[72] = '\0';
		x_fprintf(fff, encoding, "%s\n", format_literal(o_name));

		/* Display special object attributes */
		dump_obj_attrib(fff, &inventory[i], (p_ptr->is_dead ? 1 : 0));
	}
	fprintf(fff, "\n\n");


	/* Dump the Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Header */
		fprintf(fff, "  [Home Inventory]\n\n");

		/* Dump all available items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			/* Describe the object (truncate to 72 columns */
			object_desc(o_name, sizeof(o_name), &st_ptr->stock[i], TRUE, 3);
			o_name[72] = '\0';
			x_fprintf(fff, encoding, "%s\n", format_literal(o_name));

			/* Display special object attributes */
			dump_obj_attrib(fff, &st_ptr->stock[i], (p_ptr->is_dead ? 1 : 0));
		}

		/* Add two empty lines */
		fprintf(fff, "\n\n");
	}

	/* Describe the cheating and difficulty options (non-winner) */
	if (!p_ptr->total_winner)
	{
		fprintf(fff, "  [Special Advantages and Disadvantages]\n\n");
		if (!file_character_options(fff))
		{
			fprintf(fff, "(none)\n");
		}
	}

	/* Skip a line */
	fprintf(fff, "\n");

    /* Display history information */
    history_dump(fff);

	/* Close the file */
	(void)my_fclose(fff);

	/* Return to standard display */
	Term->xchar_hook = old_xchar_hook;

	/* Success */
	return (0);
}


/*
 * Get a random line from a file.  Taken from Zangband.
 */
errr get_rnd_line(const char *file_name, char *output)
{
	FILE	    *fp;
	char	buf[1024];
	int lines, line, counter;


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, file_name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp) return (-1);

	/* Parse the file */
	lines = 0;
	if (0 == my_fgets(fp, buf, 80)) lines = atoi(buf);
	else return (1);

	/* choose a random line */
	line = randint(lines);

	for (counter = 0; counter <= line; counter++)
	{
		if (!(0 == my_fgets(fp, buf, 80))) return (1);
		else if (counter == line) break;
	}

	strcpy(output, buf);

	/* Close the file */
	(void)my_fclose(fp);

	return (0);
}


/*
 * Make a string lower case.
 */
static void string_lower(char *buf)
{
	char *s;

	/* Lowercase the string */
	for (s = buf; *s != 0; s++) *s = my_tolower((unsigned char)*s);
}


/*
 * Recursive file perusal.
 *
 * Return FALSE on "ESCAPE", otherwise TRUE.
 *
 * Process various special text in the input file, including the "menu"
 * structures used by the "help file" system.
 *
 * This function could be made much more efficient with the use of "seek"
 * functionality, especially when moving backwards through a file, or
 * forwards through a file by less than a page at a time.  XXX XXX XXX
 *
 * Consider using a temporary file, in which special lines do not appear, and
 * which could be pre-padded to Term->cols characters per line, to allow the
 * use of perfect seeking.  XXX XXX XXX
 */
bool show_file(cptr name, cptr what, int line, int mode)
{
	int i, k, n;

	char ch;

	/* Assume text is not colored */
	byte attr = TERM_WHITE;

	/* Number of "real" lines passed by */
	int next = 0;

	/* Number of "real" lines in the file */
	int size;

	/* Backup value for "line" */
	int back = 0;

	/* This screen has sub-screens */
	bool menu = FALSE;

	/* Case sensitive search */
	bool case_sensitive = FALSE;

	/* No shift-movement yet */
	bool shift_key = FALSE;

	/* Current help file */
	FILE *fff = NULL;

	/* Find this string (if any) */
	char *find = NULL;

	/* Jump to this tag */
	cptr tag = NULL;

	/* Hold a string to find */
	char finder[DESC_LEN];

	/* Hold a string to show */
	char shower[DESC_LEN];

	/* Filename */
	char filename[1024];

	/* Describe this thing */
	char caption[DESC_LEN];

	/* Path buffer */
	char path[1024];

	/* General buffer */
	char buf[1024];

	/* Lower case version of the buffer, for searching */
	char lc_buf[1024];

	/* Sub-menu information */
	char hook[10][32];

	int wid, hgt;

	int old_curs = inkey_cursor_hack[TERM_MAIN];


	/* Wipe finder */
	strcpy(finder, "");

	/* Wipe shower */
	strcpy(shower, "");

	/* Wipe caption */
	strcpy(caption, "");

	/* Wipe the hooks */
	for (i = 0; i < 10; i++) hook[i][0] = '\0';

	/* Get size */
	(void)Term_get_size(&wid, &hgt);


	/* Copy the filename */
	(void)my_strcpy(filename, name, sizeof(filename));

	n = strlen(filename);

	/* Extract the tag from the filename */
	for (i = 0; i < n; i++)
	{
		if (filename[i] == '#')
		{
			filename[i] = '\0';
			tag = filename + i + 1;

			/* Search for a string (case-insensitive) */
			if (mode == 1)
			{
				strcpy(finder, tag);
				strlower(finder);
				find = finder;
			}
			break;
		}
	}

	/* Redirect the name */
	name = filename;


	/* Hack XXX XXX XXX */
	if (what)
	{
		(void)my_strcpy(caption, what, sizeof(caption));

		/* Get the filename */
		(void)my_strcpy(path, name, sizeof(path));

		/* Open */
		fff = my_fopen(path, "r");
	}

	/* Look in "help" */
	if (!fff)
	{
		/* Caption */
		(void)strnfmt(caption, sizeof(caption), "Help file '%s'", name);

		/* Build the filename */
		(void)path_build(path, sizeof(buf), ANGBAND_DIR_HELP, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Look in "info" */
	if (!fff)
	{
		/* Caption */
		(void)strnfmt(caption, sizeof(caption), "Info file '%s'", name);

		/* Build the filename */
		(void)path_build(path, sizeof(buf), ANGBAND_DIR_INFO, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Oops */
	if (!fff)
	{
		/* Message */
		msg_format("Cannot open '%s'.", name);
		message_flush();

		/* Oops */
		return (TRUE);
	}

	/* Pre-Parse the file */
	while (TRUE)
	{
		/* Read a line or stop */
		if (my_fgets(fff, buf, sizeof(buf))) break;

		/* XXX Parse "menu" items */
		if (prefix(buf, "***** "))
		{
			char b1 = '[', b2 = ']';

			/* Notice "menu" requests */
			if ((buf[6] == b1) && isdigit((unsigned char)buf[7]) &&
			    (buf[8] == b2) && (buf[9] == ' '))
			{
				/* This is a menu file */
				menu = TRUE;

				/* Extract the menu item */
				k = D2I(buf[7]);

				/* Extract the menu item */
				(void)my_strcpy(hook[k], buf + 10, sizeof(hook[0]));
			}

			/* Notice "tag" requests */
			else if (buf[6] == '<')
			{
				if (tag)
				{
					/* Remove the closing '>' of the tag */
					buf[strlen(buf) - 1] = '\0';

					/* Compare with the requested tag */
					if (streq(buf + 7, tag))
					{
						/* Remember the tagged line */
						line = next;
					}
				}
			}

			/* Skip this */
			continue;
		}

		/* Count the "real" lines */
		next++;
	}

	/* Save the number of "real" lines */
	size = next;



	/* Display the file */
	while (TRUE)
	{
		/* Clear screen */
		(void)Term_clear();


		/* Restart when necessary */
		if (line >= size) line = 0;


		/* Re-open the file if needed */
		if (next > line)
		{
			/* Close it */
			(void)my_fclose(fff);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");

			/* Oops */
			if (!fff) return (TRUE);

			/* File has been restarted */
			next = 0;
		}


		/* Go to the selected line */
		while (next < line)
		{
			/* Get a line */
			if (my_fgets(fff, buf, sizeof(buf))) break;

			/* Skip tags/links */
			if (prefix(buf, "***** ")) continue;

			/* Count the lines */
			next++;
		}


		/* Dump the next lines of the file */
		for (i = 0; i < hgt - 4; )
		{
			/* Hack -- track the "first" line */
			if (!i) line = next;

			/* Get a line of the file or stop */
			if (my_fgets(fff, buf, sizeof(buf))) break;

			/* Hack -- skip "special" lines */
			if (prefix(buf, "***** ")) continue;

			/* Count the "real" lines */
			next++;

			/* Make a copy of the current line for searching */
			(void)my_strcpy(lc_buf, buf, sizeof(lc_buf));

			/* Make the line lower case */
			if (!case_sensitive) string_lower(lc_buf);

			/* Hack -- keep searching */
			if (find && !i && !strstr(lc_buf, find)) continue;

			/* Hack -- stop searching */
			find = NULL;

			/* Colorize */
			if ((buf[0] == '=') && (strstr(buf, "===")))
			{
				attr = TERM_L_BLUE;
			}
			else if ((buf[0] == '-') && (strstr(buf, "---")))
			{
				attr = TERM_L_BLUE;
			}
			else if ((buf[strlen(buf) - 1] == ':') && (buf[0] >= 'A') &&
				(buf[0] <= 'Z'))
			{
				attr = TERM_L_BLUE;
			}
			else
			{
				attr = TERM_WHITE;
			}

			/* Dump the line */
			(void)Term_putstr(0, i+2, -1, attr, buf);

			/* Highlight "shower" */
			if (shower[0])
			{
				cptr str = lc_buf;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					(void)Term_putstr(str-lc_buf, i+2, len, TERM_YELLOW, &buf[str-lc_buf]);

					/* Advance */
					str += len;
				}
			}

			/* Count the printed lines */
			i++;
		}

		/* Hack -- failed search */
		if (find)
		{
			bell("Search string not found!");
			line = back;
			find = NULL;
			continue;
		}

		/* Display a title */
		if (size <= hgt - 4)
		{
			prt(format("[%s]", caption), 0, 0);
		}
		else
		{
			prt(format("[%s, Line %d/%d]", caption, line, size), 0, 0);
		}


		/* Hack -- simple files (encourage saving) */
		if (mode == 2)
		{
			if (size <= hgt - 4)
			{
				prt("[Press 'S' to save to file, or ESC to exit.]", hgt - 1, 0);
			}
			else
			{
				prt("[Press Space to advance, direction to move, 'S' to save to file, ESC to exit.]", hgt - 1, 0);
			}
		}

		/* Prompt -- menu screen */
		else if (menu)
		{
			/* Wait for it */
			prt("[Press a number, or ESC to exit.]", hgt - 1, 0);
		}

		/* Prompt -- small files */
		else if (size <= hgt - 4)
		{
			/* Wait for it */
			prt("[Press ESC to return to the previous file, or ? to exit.]", hgt - 1, 0);
		}

		/* Prompt -- large files */
		else
		{
			/* Wait for it */
			prt("[Press Space to advance, direction keys to move, ESC for previous, '?' to exit.]", hgt - 1, 0);
		}

		/* Hide the cursor */
		inkey_cursor_hack[TERM_MAIN] = -1;

		/* Wait for legal input */
		while (TRUE)
		{
			/* Get a keypress */
			ch = inkey(ALLOW_CLICK);

			/* If not a menu, and file is large, allow movement */
			if ((!menu) && (size > hgt - 4))
			{
				/* Allow all direction keys and shift-movement */
				get_ui_direction(&ch, 0x00, &shift_key);
			}

			/* Accept anything except untranslated mouse actions */
			if (ch != MOUSEKEY) break;
		}

		/* Restore previous cursor settings */
		inkey_cursor_hack[TERM_MAIN] = old_curs;

		/* Hack -- return to last screen on escape */
		if (ch == ESCAPE) break;

		/* Use menus to navigate -- Recurse on numbers */
		else if ((menu) && (isdigit((unsigned char)ch)) && (hook[D2I(ch)][0]))
		{
			/* Recurse on that file */
			if (!show_file(hook[D2I(ch)], NULL, 0, mode)) ch = '?';
		}


		/* Back up to the top */
		else if ((ch == 'g') || (ch == '7'))
		{
			line = 0;
		}

		/* Back up one full page */
		else if (ch == '9')
		{
			line = line - (hgt - 4);
			if (line < 0) line = 0;
		}

		/* Back up one half page */
		else if ((ch == '-') || (ch == '_') || (ch == '4'))
		{
			line = line - ((hgt - 4) / 2);
			if (line < 0) line = 0;
		}

		/* Back up one line */
		else if (ch == '8')
		{
			line = line - (shift_key ? 5 : 1);
			if (line < 0) line = 0;
		}

		/* Advance one line */
		else if ((ch == '2') || (ch == '\n') || (ch == '\r'))
		{
			line = line + (shift_key ? 5 : 1);
		}

		/* Advance one half page */
		else if ((ch == '+') || (ch == '=') || (ch == '6'))
		{
			line = line + ((hgt - 4) / 2);
			if (line < 0) line = 0;
		}

		/* Advance one full page */
		else if ((ch == '3') || (ch == ' '))
		{
			line = line + (hgt - 4);
		}

		/* Advance to the bottom (minus just under a page) */
		else if ((ch == 'G') || (ch == '1'))
		{
			line = size - (hgt - 4);
		}

		/* Toggle case sensitivity on/off */
		else if (ch == '!')
		{
			case_sensitive = !case_sensitive;
		}

		/* Try showing */
		else if (ch == '&')
		{
			/* Get "shower" */
			prt("Show: ", hgt - 1, 0);
			(void)askfor_aux(shower, 80, FALSE);

			/* Make the "shower" lowercase */
			if (!case_sensitive) string_lower(shower);
		}

		/* Try finding */
		else if (ch == '/')
		{
			/* Get "finder" */
			prt("Find: ", hgt - 1, 0);
			if (askfor_aux(finder, 80, FALSE))
			{
				/* Find it */
				find = finder;
				back = line;
				line = line + 1;

				/* Make the "finder" lowercase */
				if (!case_sensitive) string_lower(finder);

				/* Show it */
				(void)my_strcpy(shower, finder, sizeof(shower));
			}
		}

		/* Go to a specific line */
		else if (ch == '#')
		{
			char tmp[DESC_LEN];
			prt("Goto Line: ", hgt - 1, 0);
			strcpy(tmp, "0");
			if (askfor_aux(tmp, 80, FALSE))
			{
				line = atoi(tmp);
			}
		}

		/* Go to a specific file */
		else if (ch == '%')
		{
			char ftmp[DESC_LEN];
			prt("Goto File: ", hgt - 1, 0);
			strcpy(ftmp, "help.hlp");
			if (askfor_aux(ftmp, 80, FALSE))
			{
				if (!show_file(ftmp, NULL, 0, mode)) ch = '?';
			}
		}

		/* Dump to file (from Hengband) */
		else if ((ch == 'S') || (ch == 's'))
		{
			FILE *ffp;
			char buff[1024];
			char xtmp[DESC_LEN];

			strcpy(xtmp, "");

			(void)Term_gotoxy(0, 0);
			if (!get_string("File name: ", xtmp, 80))
			{
				continue;
			}

			/* Close it */
			(void)my_fclose(fff);

			/* Drop priv's */
			safe_setuid_drop();

			/* Build the filename */
			(void)path_build(buff, sizeof(buff), ANGBAND_DIR_USER, xtmp);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");

			ffp = my_fopen(buff, "w");

			/* Oops */
			if (!(fff && ffp))
			{
				msg_print("Failed to open file.");
				k = ESCAPE;
				break;
			}

			/* Save the data to file */
			while (!my_fgets(fff, buff, sizeof(buff)))
				(void)my_fputs(ffp, buff, 80);

			/* Close it */
			(void)my_fclose(fff);
			(void)my_fclose(ffp);

			/* Grab priv's */
			safe_setuid_grab();

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");
		}


		/* Exit on '?' */
		if (ch == '?')
		{
			break;
		}
	}

	/* Close the file */
	(void)my_fclose(fff);

	/* Exit on '?' */
	if (ch == '?') return (FALSE);

	/* Normal return */
	return (TRUE);
}


/*
 * Peruse help files.  We allow both general browsing of help files and
 * instant access to specific sections.  -LM-
 */
void do_cmd_help(void)
{

	/* Save and center the display */
	if (more_tall_display)
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_TALL | DSP_CX, 80, 0);
	else
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_NORM | DSP_CX, 80, 0);


	/* Show appropriate help file and section */
	switch (p_ptr->get_help_index)
	{
		case HELP_CMD_REVIEW:
		{
			if (rogue_like_commands)
			{
				show_file("cmdlist.txt#--- Roguelike keyset ---", NULL, 0, 1);
			}
			else
			{
				show_file("cmdlist.txt#--- Original keyset ---", NULL, 0, 1);
			}
			break;
		}

		case HELP_CMD_INSCRIP:
		{
			show_file("macro.txt#====== inscriptions ======", NULL, 0, 1);
			break;
		}
		case HELP_TARGET:
		{
			show_file("intrface.txt#--- targeting", NULL, 0, 1);
			break;
		}
		case HELP_CHAR_SCREEN_MAIN:
		{
			show_file("charscrn.txt#====== the character screen", NULL, 0, 1);
			break;
		}
		case HELP_CHAR_SCREEN_FLAGS:
		{
			show_file("charscrn.txt#--- attributes screen", NULL, 0, 1);
			break;
		}
		case HELP_CHAR_SCREEN_COMBAT:
		{
			show_file("charscrn.txt#--- combat screen", NULL, 0, 1);
			break;
		}
		case HELP_CMD_MACRO:
		{
			show_file("macro.txt#====== macros", NULL, 0, 1);
			break;
		}
		case HELP_CMD_VISUALS:
		{
			show_file("pref.txt#interact with visuals:", NULL, 0, 1);
			break;
		}
		case HELP_CMD_COLORS:
		{
			show_file("pref.txt#interact with colors:", NULL, 0, 1);
			break;
		}
		case HELP_BIRTH_GENDER:
		{
			show_file("overview.txt#====== the birth interface", NULL, 0, 1);
			break;
		}
		case HELP_BIRTH_RACE:
		{
			show_file("race.txt#racial adjustments to stats", NULL, 0, 1);
			break;
		}

		case HELP_STAT_STR:
		{
			show_file("attribut.txt#strength:", NULL, 0, 1);
			break;
		}
		case HELP_STAT_INT:
		{
			show_file("attribut.txt#intelligence:", NULL, 0, 1);
			break;
		}
		case HELP_STAT_WIS:
		{
			show_file("attribut.txt#wisdom:", NULL, 0, 1);
			break;
		}
		case HELP_STAT_DEX:
		{
			show_file("attribut.txt#dexterity:", NULL, 0, 1);
			break;
		}
		case HELP_STAT_CON:
		{
			show_file("attribut.txt#constitution:", NULL, 0, 1);
			break;
		}
		case HELP_STAT_CHR:
		{
			show_file("attribut.txt#charisma:", NULL, 0, 1);
			break;
		}

		case HELP_TUTORIAL:
		{
			show_file("tutorial.txt#when you begin the", NULL, 0, 1);
			break;
		}

		case HELP_STORE:
		{
			show_file("town.txt#--- stores", NULL, 0, 1);
			break;
		}
		case HELP_QUEST:
		{
			show_file("town.txt#--- getting quests", NULL, 0, 1);
			break;
		}
		case HELP_SCREENSHOT:
		{
			show_file("cmddesc.txt#save screen shot", NULL, 0, 1);
			break;
		}

		case HELP_SKILLS + 0:
		case HELP_SKILLS + 1:
		case HELP_SKILLS + 2:
		case HELP_SKILLS + 3:
		case HELP_SKILLS + 4:
		case HELP_SKILLS + 5:
		case HELP_SKILLS + 6:
		case HELP_SKILLS + 7:
		case HELP_SKILLS + 8:
		case HELP_SKILLS + 9:
		case HELP_SKILLS + 10:
		case HELP_SKILLS + 11:
		case HELP_SKILLS + 12:
		case HELP_SKILLS + 13:
		case HELP_SKILLS + 14:
		case HELP_SKILLS + 15:
		case HELP_SKILLS + 16:
		case HELP_SKILLS + 17:
		case HELP_SKILLS + 18:
		case HELP_SKILLS + 19:
		case HELP_SKILLS + 20:
		case HELP_SKILLS + 21:
		case HELP_SKILLS + 22:
		case HELP_SKILLS + 23:
		case HELP_SKILLS + 24:
		case HELP_SKILLS + 25:
		case HELP_SKILLS + 26:
		case HELP_SKILLS + 27:
		case HELP_SKILLS + 28:
		case HELP_SKILLS + 29:
		case HELP_SKILLS + 30:
		case HELP_SKILLS + 31:
		case HELP_SKILLS + 32:
		case HELP_SKILLS + 33:
		case HELP_SKILLS + 34:
		case HELP_SKILLS + 35:
		{
			char buf[DESC_LEN];
			(void)strnfmt(buf, sizeof(buf), "skills.txt#%s  ",
				skill_info[p_ptr->get_help_index - HELP_SKILLS].name);
			strlower(buf);
			show_file(buf, NULL, 0, 1);
			break;
		}

		case HELP_TALENTS:
		{
			show_file("talents.txt#====== talents", NULL, 0, 1);
			break;
		}
		case HELP_FORGING:
		{
			show_file("talents.txt#--- object-forging", NULL, 0, 1);
			break;
		}

		case HELP_INFUSION:
		{
			show_file("talents.txt#--- infusion of wargear", NULL, 0, 1);
			break;
		}

		/* Show the main help file menu by default */
		default:
		{
			(void)show_file("help.hlp", NULL, 0, 0);
			break;
		}
	}

	/* Show general help next time */
	p_ptr->get_help_index = HELP_GENERAL;

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);
}


/*
 * Display a text file on screen.
 *
 * This function is used to display the "news", "dead", and "victory" files.
 * It is generally suitable for any file display that does not require user
 * input.  If the file has the correct textual markers, we can display color
 * and switch between regular and large displays.  It will be easy to extend
 * this code to handle other tasks.
 *
 * After calling this function, the screen may be in either normal or large
 * display mode.
 */
bool display_file(char *filename)
{
	cptr s;
	char buf[1024];
	FILE *fp;

	int y = 0, x = 0, left_margin = 0, n;
	int this_len, maxlen = 0;

	/* Assume we're reading character, not color information */
	bool char_mode = TRUE;
	bool attr_mode = FALSE;

	int x1 = -1, x2 = -1;
	byte attr;

	byte *scr_aa;
	char *scr_cc;


	/* Open the file */
	fp = my_fopen(filename, "r");

	/* No file -- do nothing */
	if (!fp) return (FALSE);


	/* Read file, get length of longest string */
	while (0 == my_fgets(fp, buf, sizeof(buf)))
	{
		/* Save the length of this string (before terminal NULL) */
		this_len = strlen(buf);
		if (maxlen < this_len) maxlen = this_len;
	}

	/* Close the file */
	(void)my_fclose(fp);

	/* Open the file again */
	fp = my_fopen(filename, "r");

	/* Read the file again */
	while (0 == my_fgets(fp, buf, sizeof(buf)))
	{
		/* First character may be a special marker */
		if (buf[0] == '#')
		{
			/* Skip the (possible) marker, point to the remaining text */
			s = buf + 1;

			/* This file is in text/attr format */
			if (strstr(s, "charattr"))
			{
				/* We start off in character mode */
				char_mode = TRUE;
				continue;
			}

			/* The file wants to be shown using the full display */
			if (strstr(s, "fulldisplay"))
			{
				/* Use the tall display (unless using the full screen view) */
				display_change(DSP_TALL, 0, 0);

				/* Calculate left margin (center the display, never go negative) */
				left_margin = MAX(0, (Term->cols - maxlen) / 2);

				continue;
			}

			/* The file wants to be shown using the standard display */
			if (strstr(s, "regulardisplay"))
			{
				/* Use the standard display (unless using the full screen view) */
				display_change(DSP_NORM, 0, 0);

				/* Calculate left margin (center the display, never go negative) */
				left_margin = MAX(0, (Term->cols - maxlen) / 2);

				continue;
			}

			/* The file is now displaying color data */
			if ((strstr(s, "endchar")) && (char_mode))
			{
				/* Switch to attr mode */
				char_mode = FALSE;
				attr_mode = TRUE;

				/* Start at beginning of table */
				y = 0;
				continue;
			}

			/* The file is now displaying character data */
			if ((strstr(s, "endattr")) && (attr_mode))
			{
				/* Switch to char mode */
				char_mode = TRUE;
				attr_mode = FALSE;

				/* Start at beginning of table */
				y = 0;
				continue;
			}
		}

		/* Check bounds */
		if (y >= Term->rows) continue;

		/* Insert any other data into the Term */
		/* This code comes largely from "Term_queue_chars" */
		if (TRUE)
		{
			/* Get length */
			n = strlen(buf);

			/* Check bounds */
			if (left_margin + n >= Term->cols) n = Term->cols - left_margin - 1;

			/* Point to start of scanning data for this row */
			scr_aa = Term->scr->a[y];
			scr_cc = Term->scr->c[y];

			/* Queue the attr/chars */
			for (s = buf, x = left_margin; n; x++, s++, n--)
			{
				/* Saving color */
				if (attr_mode)
				{
					/* Get color */
					if (*s == ' ') attr = TERM_WHITE;
					else
					{
						/* Convert character into a legal color */
						int attr_int = color_char_to_attr(*s);

						/* Paranoia -- insist upon a legal color */
						if (attr_int < 0) attr_int = TERM_WHITE;

						attr = (byte)attr_int;
					}

					/* Save it */
					scr_aa[x] = attr;
				}

				/* Saving (translated) characters */
				else if (char_mode)
				{
					scr_cc[x] = xchar_trans(*s);
				}
			}

			/* Note the "range" of window updates */
			x1 = left_margin;
			x2 = x - 1;

			/* Expand the "change area" as needed */
			if (x1 >= 0)
			{
				/* Check for new min/max row info */
				if (y < Term->y1) Term->y1 = y;
				if (y > Term->y2) Term->y2 = y;

				/* Check for new min/max col info in this row */
				if (x1 < Term->x1[y]) Term->x1[y] = x1;
				if (x2 > Term->x2[y]) Term->x2[y] = x2;
			}

			/* Increment rows */
			y++;
		}
	}

	/* Flush it */
	(void)Term_fresh();

	/* Close */
	(void)my_fclose(fp);

	/* Note success */
	return (TRUE);
}


/*
 * Process the player name and extract a clean base name.
 *
 * Convert any troublesome characters, shorten name if necessary, and use
 * "PLAYER" as a base name if the character has no full name.
 *
 * If "change_sf_name" is TRUE, we initialize the savefile name based on
 * base name. This should only be done when a new character is being
 * created, or if "change_save_names" is set.
 *
 * Some platforms (Windows, Macintosh, Amiga) leave the "savefile" empty
 * when a new character is created, and then when the character is done
 * being created, they call this function to choose a new savefile name.
 */
void process_player_name(bool change_sf_name)
{
	int i;
	char temp[DESC_LEN];


	/* Cannot be too long */
	if (strlen(op_ptr->full_name) > 30)
	{
		/* Truncate it */
		op_ptr->full_name[30] = '\0';
	}

	/* Paranoia -- handle empty base names  XXX */
	if (!op_ptr->base_name[0])
	{
		strcpy(op_ptr->base_name, "PLAYER");
	}


	/* We're done unless we have to change the savefile name */
	if (!change_sf_name) return;


	/* Use "PLAYER" as a base name if the character is unnamed  XXX */
	if (!op_ptr->full_name[0])
	{
		strcpy(op_ptr->base_name, "PLAYER");
	}

	/* Usual case -- character has a name */
	else
	{
		/* Process the base name */
		for (i = 0; op_ptr->full_name[i]; i++)
		{
			byte c = (byte)op_ptr->full_name[i];

			/* Translate to 7-bit ASCII */
			if (c > 127) c = seven_bit_translation[c - 128];

/* All operating systems other than Macintosh */
#ifndef MACINTOSH
			/* Convert all non-alphanumeric symbols */
			if (!isalpha(c) && !isdigit(c)) c = '_';

/* Macintosh */
#else
			/* Convert "dot" to "underscore" */
			if (c == '.') c = '_';
#endif


/* Some operating systems require short filenames */
#ifdef MSDOS
			/* If first word is at least four chars long, use it as the base name */
			if ((i >= 4) && (c == ' ')) break;

			/* Must never be longer than eight chars */
			if (i == 8) break;
#endif

			/* Build the base name */
			op_ptr->base_name[i] = (char)c;
		}

		/* End the base name */
		op_ptr->base_name[i] = '\0';
	}

#ifdef SAVEFILE_USE_UID
	/* Rename the savefile, using the player_uid and base_name */
	(void)strnfmt(temp, sizeof(temp), "%d.%s", player_uid, op_ptr->base_name);
#else
	/* Rename the savefile, using the base name */
	(void)strnfmt(temp, sizeof(temp), "%s", op_ptr->base_name);
#endif


	/* Build the filename */
	(void)path_build(savefile, sizeof(savefile), ANGBAND_DIR_SAVE, temp);
}


/*
 * Gets a name for the character, reacting to name changes.
 *
 * Build and use a new savefile name at character birth.  Optionally,
 * do the same thing whenever changing name.  Check for existing file
 * with the same name (to avoid overwrites).
 *
 * Perhaps we should NOT ask for a name (at "birth()") on
 * UNIX machines?  XXX XXX XXX
 *
 * What a horrible name for a global function.  XXX XXX XXX
 */
void get_name(void)
{
	char tmp[32];
	int fd;
	char old_name[32];


	/* Usual rule -- change savefile name only for new character */
	bool change_sf_name = !character_loaded;

	/* Alternate rule -- always change savefile name */
	if (change_save_names) change_sf_name = TRUE;


	/* Save the old name */
	(void)my_strcpy(old_name, op_ptr->full_name, sizeof(old_name));


	/* Interact */
	while (TRUE)
	{
		/* Save the current player name */
		(void)my_strcpy(tmp, op_ptr->full_name, sizeof(tmp));

		/* Prompt */
		prt("Enter a name for your character: ", 0, 0);

		/* Get an input, usually cancel on "Escape" */
		if (askfor_aux(tmp, 31, TRUE))
		{
			/* Use the name */
			(void)my_strcpy(op_ptr->full_name, tmp, sizeof(op_ptr->full_name));

			/* Process the player name, sometimes change savefile name */
			process_player_name(change_sf_name);

			/* Filename is static, and doesn't need to be checked */
			if (!change_sf_name) break;

			/* Check if it's already taken */
			fd = fd_make(savefile, 0644);

			/* File exists, and character in it is alive */
			if (fd < 0)
			{
				/* Warning */
				msg_print("Using that name would overwrite a previous savefile.");

				/* Allow cancel */
				if (!get_check("Really do this?"))
				{
					/* Restore the old name */
					(void)my_strcpy(op_ptr->full_name, old_name,
						sizeof(op_ptr->full_name));

					/* Process it, restore old savefile name */
					process_player_name(TRUE);

					/* Prompt again (with original name) */
					continue;
				}
			}

			/* Save character under new name, overwriting if necessary */
			(void)fd_close(fd);
			break;
		}
		else break;
	}
}



/*
 * Hack -- commit suicide or retire
 */
void do_cmd_quit(void)
{
	/* Flush input */
	flush();

	/* Verify Retirement */
	if (p_ptr->total_winner)
	{
		/* Verify */
		if (!get_check("Do you want to retire?")) return;

		/* Cause of death */
		strcpy(p_ptr->died_from, "Ripe Old Age");
	}

	/* Verify Suicide */
	else
	{
		char ch;

		/* Verify */
		if (!get_check("Do you really want to suicide?")) return;

		/* Special Verification for suicide */
		prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
		flush();
		ch = inkey(FALSE);
		prt("", 0, 0);
		if (ch != '@') return;

		/* Cause of death */
		strcpy(p_ptr->died_from, "(Quit the game)");
	}

	/* Commit suicide */
	p_ptr->is_dead = TRUE;

	/* Stop playing */
	p_ptr->playing = FALSE;

	/* Leaving */
	p_ptr->leaving = TRUE;
}



/*
 * Save the game
 */
void do_cmd_save_game(bool is_autosave)
{
	/* Disturb the player, unless autosaving. */
	if (!is_autosave) disturb(1, 0);

	/* Clear messages */
	message_flush();

	/* Handle stuff */
	handle_stuff();

	/* Message */
	if (!is_autosave) msg_print("Saving game...");
	else              msg_print("Autosaving the game...");

	/* Refresh */
	(void)Term_fresh();

	/* The player is not dead */
	strcpy(p_ptr->died_from, "(saved)");

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Hack -- temporarily advance the turn count  XXX */
	if (is_autosave) turn++;

	/* Save the player */
	if (save_player()) msg_print("done.");

	/* Save failed */
	else
	{
		msg_print("Failed to save the game!  Check to see if the \"lib/save\" directory exists.   ");
		msg_print("On multi-user machines, the problem may be that you're not allowed to over-write the savefile you're using.");
	}

	/* Hack -- restore the turn count  XXX */
	if (is_autosave) turn--;

	/* Allow suspend again */
	signals_handle_tstp();

	/* Refresh */
	(void)Term_fresh();

	/* Note that the player is not dead */
	strcpy(p_ptr->died_from, "(alive and well)");
}


/*
 * Calculates the total number of points earned.
 *
 * You gain points by killing deeper uniques, and lose them by buying more
 * skills.  The first is a more powerful factor then the second.
 *
 * Many of the birth options affect your score.
 *
 * Dunadain and High-elves get 80% of the points all other races do.
 *
 * Scores never go down; once you've earned the points, they cannot be
 * taken away.
 */
s32b total_points(void)
{
	int i;
	s32b k;
	s32b score = 0L;
	s32b divisor;

	int r_idx;


	/* Calculate the skill divisor */
	for (divisor = 50L, i = 0; i < NUM_SK_USED; i++)
	{
		/* Skip nonexistent skills */
		if (skill_info[i].cost_adj == 0) continue;

		/* Skip skills with no investment */
		if (p_ptr->pskills[i].max < 1) continue;

		/* Multiply skill value by inherent difficulty (from about 4 to 16) */
		k = (s32b)(p_ptr->pskills[i].max * skill_info[i].cost_adj);

		/*
		 * Investing in similar skills reduces the score penalty in (almost) the
		 * same way as it reduces skill cost and effect on character power.
		 */
		adv_cost_reduce_similar(i, &k, 3);

		/* Sum up all the skill values */
		divisor += k;
	}

	/* Get the highest-level slain dungeon unique */
	r_idx = slain_opponent(TRUE);

	/* The big points come from defeating uniques */
	if (r_idx)
	{
		/* Big scores for deep uniques (10x inflation) */
		score = 2500L * (s32b)r_info[r_idx].level * (s32b)r_info[r_idx].level;

		/* Bonus for the heads of Sauron and Morgoth */
		if      (r_idx == MON_MORGOTH) score += score;
		else if (r_idx == MON_SAURON)  score += score / 2;
	}

	/* Get some points for killing non-uniques */
	else
	{
		/* Check for slain opponent (allow OOD non-uniques) */
		r_idx = slain_opponent(FALSE);

		/* Character has slain a worthy opponent -- grant some points (10x inflation) */
		if (r_idx) score = 1000L * (s32b)r_info[r_idx].level *
		                   (s32b)r_info[r_idx].level;
	}

	/* Divide score by divisor */
	score /= divisor;

	/* Wealth earns (a few) points if great enough */
	score += rsqrt((p_ptr->au - p_ptr->au_birth) / 1000L);


	/* Reward/Penalize for difficulty options  -clefs- */
	k = score;

	/*
	 * TODO: There should be an unified way of handling score for
	 * character_types.  XXX XXX XXX
	 */

	/* +40% for ironman */
	if (p_ptr->character_type == PCHAR_IRONMAN) score += k * 4 / 10;

	/* Otherwise +15% for no stores, +5% for no return stairs */
	else
	{
		if (birth_no_stores)   score += k * 3 / 20;
		if (birth_no_return_stair) score += k / 20;
	}

	/* +10% for no artifacts, +7% for cheating monsters */
	if (birth_no_artifacts)     score += k / 10;
	if (birth_smart_cheat)      score += k * 7 / 100;

	/* Special case -- Dunadain and High-Elves get 80% of normal score */
	if ((p_ptr->prace == RACE_DUNADAN) || (p_ptr->prace == RACE_HIGH_ELF))
	{
		score = 4 * score / 5;
	}

	/* If we know all about monsters, reduce score by 33% */
	if (cheat_know) score = 2 * score / 3;


	/* If this score is greater than the best previous, save it */
	if (score > p_ptr->score) p_ptr->score = score;

	/* Return the score */
	return (p_ptr->score);
}


/*
 * Save a "bones" file for a dead character.  Now activated and (slightly)
 * altered.
 */
static void make_bones(void)
{
	FILE *fp;

	char str[1024];

	int i;

	/* Ignore all kinds of cheaters */
	if (!p_ptr->noscore)
	{
		/* Ignore people who die in town */
		if (p_ptr->depth)
		{
			int level;
			char tmp[DESC_LEN];

			/* Slightly more tenacious saving routine. */
			for (i = 0; i < 20; i++)
			{
				/* Ghost hovers near level of death. */
				if (i == 0) level = p_ptr->depth;
				else level = p_ptr->depth + 5 - damroll(2, 4);
				if (level < 1) level = randint(4);

				/* XXX XXX XXX "Bones" name */
				(void)strnfmt(tmp, sizeof(tmp), "bone.%03d", level);

				/* Build the filename */
				(void)path_build(str, sizeof(str), ANGBAND_DIR_BONE, tmp);

				/* Attempt to open the bones file */
				fp = my_fopen(str, "r");

				/* Close it right away */
				if (fp) (void)my_fclose(fp);

				/* Do not over-write a previous ghost */
				if (fp) continue;

				/* If no file by that name exists, we can make a new one. */
				if (!(fp)) break;
			}

			/* Failure */
			if (fp) return;

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

			/* Try to write a new "Bones File" */
			fp = my_fopen(str, "w");

			/* Not allowed to write it?  Weird. */
			if (!fp) return;

			/* Save the info */
			if (op_ptr->full_name[0] != '\0') fprintf(fp, "%s\n", op_ptr->full_name);
			else fprintf(fp, "Anonymous\n");

			/* Save gender, race, and realm */
			fprintf(fp, "%d\n", p_ptr->psex);
			fprintf(fp, "%d\n", p_ptr->prace);
			fprintf(fp, "%d\n", p_ptr->realm);

			/* Get character specialty */
			if (TRUE)
			{
				/* Get specialty */
				(void)get_title(99, FALSE, FALSE);

				/* Save specialty, if any */
				fprintf(fp, "%d\n", p_ptr->specialty);
			}

			/* Close and save the Bones file */
			(void)my_fclose(fp);
		}
	}
}


/*
 * Hack - save the time of death
 */
static time_t death_time = (time_t)0;


/*
 * Display a "tomb-stone"
 */
void print_tomb(void)
{
	cptr p;

	int points = total_points();

	char tmp[DESC_LEN];
	char buf[1024];

	/* Center on the screen (conform to size and position of tombstone) */
	int l_margin = 18 + (display_width() - 70) / 2;


	/*** Display the "dead" file ***/

	/* Clear screen */
	screen_clear();

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "dead.txt");

	/* Display the file */
	(void)display_file(buf);

	/* Name */
	move_cursor(6, 0);
	c_roff_centered(TERM_WHITE, format("%s\n", op_ptr->full_name), l_margin + 1, l_margin + 31);


	/* King or Queen */
	if (p_ptr->total_winner)
	{
		p = "the Magnificent";
	}

	/* Normal */
	else
	{
		char buf2[DESC_LEN];

		cptr title = get_title(32, FALSE, TRUE);
		bool quotes = (title[0] == '\"');

		/* Build a title string */
		(void)strnfmt(buf2, sizeof(buf2), "%s\n%s", (quotes ? "" : "the "), title);

		/* Point to the formatted title */
		p = buf2;
	}

	/* Title */
	move_cursor(7, 0);
	c_roff_centered(TERM_WHITE, format("%s\n\n", p), l_margin, l_margin + 32);

	/* Score */
	if (!p_ptr->deaths)
	{
		c_roff_centered(TERM_L_BLUE,
			format("Score: %ld\n\n", points), l_margin, l_margin + 32);
	}
	else
	{
		c_roff_centered(TERM_L_BLUE,
			format("Score: (%ld)\n\n", ABS(points)), l_margin, l_margin + 32);
	}

	/* Death Level */
	strcpy(tmp, "Killed ");

	if (!p_ptr->depth)
	{
		strcat(tmp, "in the town");
	}
	else if (depth_in_feet)
	{
		if (use_metric)
			strcat(tmp, format("at %d m", p_ptr->depth * 15));
		else
			strcat(tmp, format("at %d'", p_ptr->depth * 50));
	}
	else
	{
		strcat(tmp, format("on level %d", p_ptr->depth));
	}

	c_roff_centered(TERM_WHITE,
		format("%s\n", tmp), l_margin, l_margin + 33);

	/* Killer */
	(void)strnfmt(tmp, sizeof(tmp), "%s%s.\n\n\n",
		(p_ptr->died_from[0] == '(' ? "" : "by "), p_ptr->died_from);
	c_roff_centered(TERM_WHITE, tmp, l_margin, l_margin + 33);

	/* Get time of death */
	(void)time(&death_time);

	/* Time */
	c_roff_centered(TERM_WHITE, format("%-.24s", ctime(&death_time)), l_margin, l_margin + 33);
}


/*
 * Hack - Know inventory and home items upon death
 */
static void death_knowledge(void)
{
	int i;

	object_type *o_ptr;

	store_type *st_ptr = &store[STORE_HOME];


	/* Hack -- Know everything in the inven/equip */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);
	}

	/* Hack -- Know everything in the home */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		o_ptr = &st_ptr->stock[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);
	}

	/* Hack -- Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();
}


/*
 * Display some character info
 */
static void show_info(void)
{
	int i, j, k;
	char buf[1024];

	object_type *o_ptr;

	store_type *st_ptr = &store[STORE_HOME];


	/* Display player */
	display_player(0, TRUE);

	/* Center the prompt */
	center_string(buf, sizeof(buf),
		"Hit any key to see more information (ESC to abort): ", display_width());

	/* Prompt for it */
	prt(buf, 23, 0);

	/* Allow abort at this point */
	if (inkey(ALLOW_CLICK) == ESCAPE) return;


	/* Show equipment and inventory */

	/* Equipment -- if any */
	if (p_ptr->equip_cnt)
	{
		(void)Term_clear();
		item_tester_full = TRUE;
		show_equip();
		prt("You are using: -more-", 0, 0);
		if (inkey(ALLOW_CLICK) == ESCAPE) return;
	}

	/* Inventory -- if any */
	if (p_ptr->inven_cnt)
	{
		(void)Term_clear();
		item_tester_full = TRUE;
		show_inven();
		prt("You are carrying: -more-", 0, 0);
		if (inkey(ALLOW_CLICK) == ESCAPE) return;
	}



	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Display contents of the home */
		for (k = 0, i = 0; i < st_ptr->stock_num; k++)
		{
			/* Clear screen */
			(void)Term_clear();

			/* Show 12 items */
			for (j = 0; (j < 12) && (i < st_ptr->stock_num); j++, i++)
			{
				byte attr;

				char o_name[DESC_LEN];
				char tmp_val[DESC_LEN];

				/* Get the object */
				o_ptr = &st_ptr->stock[i];

				/* Print header, clear line */
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%c) ", I2A(j));
				prt(tmp_val, j+2, 4);

				/* Get the object description */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

				/* Get the inventory color */
				attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

				/* Display the object */
				c_put_str(attr, o_name, j + 2, 7);
			}

			/* Caption */
			prt(format("Your home contains (page %d): -more-", k+1), 0, 0);

			/* Wait for it */
			if (inkey(ALLOW_CLICK) == ESCAPE) return;
		}
	}
}


/*
 * Special version of 'do_cmd_examine'
 */
static void death_examine(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	/* Start out in "display" mode */
	p_ptr->command_see = TRUE;

	/* Interact until satisfied */
	while (TRUE)
	{
		/* Get an item */
		q = "Examine which item?";
		s = "You have nothing to examine.";
		if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;
		item_to_object(o_ptr, item);

		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);

		/* Describe it fully */
		do_cmd_observe(o_ptr, FALSE);
	}
}

/*
 * Seek score 'i' in the highscore file
 */
static int highscore_seek(int i)
{
	/* Seek for the requested record */
	return (fd_seek(highscore_fd, i * sizeof(high_score)));
}


/*
 * Read one score from the highscore file
 */
static errr highscore_read(high_score *score)
{
	/* Read the record, note failure */
	return (fd_read(highscore_fd, (char*)(score), sizeof(high_score)));
}


/*
 * Write one score to the highscore file
 */
static int highscore_write(const high_score *score)
{
	/* Write the record, note failure */
	return (fd_write(highscore_fd, (cptr)(score), sizeof(high_score)));
}




/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(const high_score *score)
{
	int i;

	high_score the_score;

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return (-1);

	/* Go to the start of the highscore file */
	if (highscore_seek(0)) return (-1);

	/* Read until we get to a higher score */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		int tmp;

		if (highscore_read(&the_score)) return (i);

		tmp = strcmp(the_score.pts, score->pts);

		/* This score is lower than mine */
		if (tmp < 0) return (i);

		/* This score is higher than mine, try next one */
		if (tmp > 0) continue;

		/* Compare maximum dungeon level only if the scores are equal */
		tmp = strcmp(the_score.max_dun, score->max_dun);

		/* This max_dun is lower than mine */
		if (tmp < 0) return (i);

		/* Higher than or equal to mine, try next. (older is greater) */
	}


	/* The "last" entry is always usable */
	return (MAX_HISCORES - 1);
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static int highscore_add(const high_score *score)
{
	int i, slot;
	bool done = FALSE;

	high_score the_score, tmpscore;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return (-1);

	/* Determine where the score should go */
	slot = highscore_where(score);

	/* Hack -- Not on the list */
	if (slot < 0) return (-1);

	/* Hack -- prepare to dump the new score */
	the_score = (*score);

	/* Slide all the scores down one */
	for (i = slot; !done && (i < MAX_HISCORES); i++)
	{
		/* Read the old guy, note errors */
		if (highscore_seek(i)) return (-1);
		if (highscore_read(&tmpscore)) done = TRUE;

		/* Back up and dump the score we were holding */
		if (highscore_seek(i)) return (-1);
		if (highscore_write(&the_score)) return (-1);

		/* Hack -- Save the old score, for the next pass */
		the_score = tmpscore;
	}

	/* Return location used */
	return (slot);
}

/*
 * File descriptor for screen dumps of various sorts.
 */
static FILE *dump_file_fff = NULL;

/*
 * Save text from the screen to a text file.  Accept margins.
 */
static void to_dump_file(int y0, int x0, int y1, int x1)
{
	int y, x, i;

	char buf[DESC_LEN];

	byte a;
	char c;


	/* Dump a block of the screen */
	for (y = y0; y <= y1; y++)
	{
		for (i = 0, x = x0; x <= x1; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[i++] = c;
		}

		/* Back up over spaces */
		while ((i > 0) && (buf[i-1] == ' ')) --i;

		/* Terminate */
		buf[i] = '\0';

		/* End the row */
		fprintf(dump_file_fff, "%s\n", buf);
	}
}


/*
 * Save high scores to a human-readable file.
 */
static int save_high_scores(int note, high_score *score)
{
	char buf[1024];


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.txt");

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Check for existing file */
	dump_file_fd = fd_open(buf, O_RDONLY);

	/* Existing file */
	if (dump_file_fd >= 0)
	{
		char out_val[DESC_LEN];

		/* Close the file */
		(void)fd_close(dump_file_fd);

		/* Build query */
		(void)strnfmt(out_val, sizeof(out_val), "Replace existing file %s?", buf);

		/* Ask */
		if (get_check(out_val)) dump_file_fd = -1;
	}

	/* Open the non-existing file */
	if (dump_file_fd < 0) dump_file_fff = my_fopen(buf, "w");

	/* Invalid file */
	if (!dump_file_fff) return (-1);


	/* Begin dump */
	fprintf(dump_file_fff, "                 [%s %s High Scores]\n\n",
	        VERSION_NAME, VERSION_STRING);

	/* Dump all the scores */
	display_scores_aux(0, MAX_HISCORES, note, score);

	/* Close the file */
	(void)my_fclose(dump_file_fff);

	/* Forget the file */
	dump_file_fd = -1;
	dump_file_fff = NULL;

	/* Success */
	return (0);
}

/*
 * Display the scores in a given range.
 * Assumes the high score list is already open and that the display is already
 * adjusted.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
void display_scores_aux(int from, int to, int note, high_score *score)
{
	char ch;

	int j, k, n, place;
	int count;
	int size, num_to_show;
	bool flag = FALSE;

	high_score the_score;

	char out_val[DESC_LEN];
	char tmp_val[DESC_LEN];

	byte attr;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return;

	/* Seek to the beginning */
	if (highscore_seek(0)) return;


	/*
	 * Scores take up 4 rows.  If we're using the tall display or saving to
	 * file, insert a spacer.
	 */
	if ((use_tall_display) || (dump_file_fff)) size = 5;
	else                                       size = 4;

	/* Calculate the number of high scores to show on a page */
	num_to_show = (Term->rows - 4) / size;


	/* Start at the beginning, unless specified */
	if (from < 0) from = 0;


	/* Unless specified, assume we will show all, or a full page */
	if (to < 0) to = MAX_HISCORES;
	else if (to < from + num_to_show) to = from + num_to_show;
	else if (to > MAX_HISCORES) to = MAX_HISCORES;


	/* Hack -- Count the high scores */
	for (count = 0; count < MAX_HISCORES; count++)
	{
		if (highscore_read(&the_score)) break;
	}

	/* Hack -- allow "fake" entry to be last */
	if ((note == count) && score) count++;

	/* Do not show more scores than requested */
	if (count > to) count = to;


	/* Show 5 or 10 per page, until "done" */
	for (k = from, j = from, place = k+1; k < count; k += num_to_show)
	{
		/* Clear screen */
		(void)Term_clear();

		/* Title */
		center_string(tmp_val, sizeof(tmp_val), format("%s Hall of Fame", VERSION_NAME), display_width());
		c_put_str(TERM_L_BLUE, tmp_val, 0, 0);

		/* Show a page of entries */
		for (n = 0; j < count && n < num_to_show;
			place++, j++, n++)
		{
			int pr, p_mag, cdun, mdun;

			cptr gold, when, aged;
#ifdef SAVEFILE_USE_UID
			cptr user;
#endif /* SAVEFILE_USE_UID */

			/* Colored text to differentiate entries */
			attr = (n % 2 ? TERM_L_WHITE : TERM_WHITE);


			/* Mega-Hack -- insert a "fake" record */
			if ((note == j) && score && !flag)
			{
				the_score = (*score);
				attr = TERM_L_GREEN;
				flag = TRUE;
				j--;
			}

			/* Read a normal record */
			else
			{
				/* Read the proper record */
				if (highscore_seek(j)) break;
				if (highscore_read(&the_score)) break;
			}

			/* Extract the race and magic realm */
			pr = atoi(the_score.p_r);
			p_mag = atoi(the_score.p_mag);

			/* Extract the level info */
			cdun = atoi(the_score.cur_dun);
			mdun = atoi(the_score.max_dun);

			/* Hack -- extract the gold and such */
#ifdef SAVEFILE_USE_UID
			for (user = the_score.uid; my_isspace(*user); user++) /* loop */;
#endif /* SAVEFILE_USE_UID */
			for (when = the_score.day; my_isspace(*when); when++) /* loop */;
			for (gold = the_score.gold; my_isspace(*gold); gold++) /* loop */;
			for (aged = the_score.turns; my_isspace(*aged); aged++) /* loop */;

			/* Clean up standard encoded form of "when" */
			if ((*when == '@') && strlen(when) == 9)
			{
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%.4s-%.2s-%.2s",
					when + 1, when + 5, when + 7);
				when = tmp_val;
			}

			/* Dump some info */
			(void)strnfmt(out_val, sizeof(out_val), "%3d.%7s  %s the %s %s",
				place, the_score.pts, the_score.who,
				race_info[pr].title, the_score.title);

			/* Dump the first line */
			c_put_str(attr, out_val, n*size + 2, 0);

			/* Dump the second line */
			c_put_str(attr, the_score.kill_desc, n*size + 3, 15);

			/* Another line of info */
			if (streq(the_score.how, "alive and well"))
			{
				(void)strnfmt(out_val, sizeof(out_val), "Still alive on %s %d",
					"dungeon level", cdun);
			}
			else
			{
				(void)strnfmt(out_val, sizeof(out_val), "Killed by %s ", the_score.how);

				if (depth_in_feet)
				{
					if (use_metric)
						strcat(out_val, format("at %d m",  cdun * 15));
					else
						strcat(out_val, format("at %d ft", cdun * 50));
				}
				else
				{
					strcat(out_val, format("on dungeon level %d", cdun));
				}
			}

			/* Hack -- some people die in the town */
			if (!cdun)
			{
				if (streq(the_score.how, "alive and well"))
				{
					(void)strnfmt(out_val, sizeof(out_val), "Still alive in the Town");
				}
				else
				{
					(void)strnfmt(out_val, sizeof(out_val), "Killed by %s in the Town",
						the_score.how);
				}
			}

			/* Append a "maximum level" */
			if (mdun > cdun)
			{
				if ((depth_in_feet) && (!streq(the_score.how, "alive and well")))
				{
					if (use_metric)
						strcat(out_val, format(" (Max %d m)", mdun * 15));
					else
						strcat(out_val, format(" (Max %d ft)", mdun * 50));
				}
				else
				{
					strcat(out_val, format(" (Max %d)", mdun));
				}
			}

			/* Dump the info */
			c_put_str(attr, out_val, n*size + 4, 15);

			/* And still another line of info */
#ifdef SAVEFILE_USE_UID
			(void)strnfmt(out_val, sizeof(out_val),
				"(User %s, Date %s, Gold %s, Turn %s).",
				user, when, gold, aged);
#else
			(void)strnfmt(out_val, sizeof(out_val),
				"(Date %s, Gold %s, Turn %s).",
				when, gold, aged);
#endif /* SAVEFILE_USE_UID */
			c_put_str(attr, out_val, n*size + 5, 15);
		}


		/* Wait for response */
		if (k < count - num_to_show)
		{
			center_string(tmp_val, sizeof(tmp_val),
				"[Press ESC to exit, 'S' to save to file, or any other key to continue.]",
				display_width());
		}
		else
		{
			center_string(tmp_val, sizeof(tmp_val),
				"[Press 'S' to save to file, or any other key to exit.]",
				display_width());
		}
		put_str(tmp_val, Term->rows - 1, 0);

		/* Wait for it, unless we are saving the screen to text */
		if (!dump_file_fff)
		{
			ch = inkey(ALLOW_CLICK);

			/* Hack -- notice Escape */
			if (ch == ESCAPE) break;

			/* Save scores to text file */
			if ((ch == 's') || (ch == 'S'))
			{
				/* Display scores, read to file */
				if (!save_high_scores(note, score))
				{
					/* Report success */
					center_string(tmp_val, sizeof(tmp_val),
						format("High scores saved in the \"%s\" directory.",
						ANGBAND_DIR_APEX), display_width());
				}

				/* Report failure */
				else
				{
					center_string(tmp_val, sizeof(tmp_val),
						"Could not save high scores to file!", display_width());
				}

				/* Do not wait */
				inkey_scan = TRUE;

				/* Re-display this page of high scores  XXX */
				display_scores_aux(k, k + num_to_show, note, score);

				/* Prompt */
				put_str(tmp_val, 0, 0);

				/* Wait for it */
				ch = inkey(ALLOW_CLICK);

				/* Allow Escape */
				if (ch == ESCAPE) break;
			}
		}

		/* Save this screen to file */
		else
		{
			/* Get last line of text */
			int x2 = 1 + size * num_to_show;

			/* Dump to file */
			to_dump_file(2, 0, x2, 80);
		}
	}
}


/*
 * Hack - save index of player's high score
 */
static int score_idx = -1;


/*
 * Enters a player's name on a high score table, if "legal".
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static errr enter_score(void)
{
	high_score the_score;
	cptr character_title;
	char kill_desc[DESC_LEN];


	/* No score file */
	if (highscore_fd < 0)
	{
		return (0);
	}

#ifndef SCORE_WIZARDS

	/* Wizard-mode preempts scoring */
	if (cheat_wizard)
	{
		msg_print("Score not registered for wizards.");
		message_flush();
		score_idx = -1;
		return (0);
	}

	/* Debug-mode preempts scoring */
	if (cheat_debug)
	{
		msg_print("Score not registered when debugging the game.");
		message_flush();
		score_idx = -1;
		return (0);
	}
#endif

#ifndef SCORE_BORGS

	/* Borg-mode preempts scoring */
	if (cheat_borg)
	{
		msg_print("Score not registered for borgs.");
		message_flush();
		score_idx = -1;
		return (0);
	}
#endif /* SCORE_BORGS */

	/* Those who cheat death are not scored */
	if ((p_ptr->deaths) || (cheat_death))
	{
		msg_print("Score only registered for your character's first life.");
		message_flush();
		score_idx = -1;
		return (0);
	}

	/* Hack -- Quitter */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "(Quit the game)"))
	{
		msg_print("Score not registered due to quitting.");
		message_flush();
		score_idx = -1;
		return (0);
	}


	/* Clear the record */
	(void)WIPE(&the_score, high_score);

	/* Save the version */
	(void)strnfmt(the_score.what, sizeof(the_score.what), "%s", VERSION_STRING);

	/* Calculate and save the points */
	(void)strnfmt(the_score.pts, sizeof(the_score.pts), "%9ld", (long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	(void)strnfmt(the_score.gold, sizeof(the_score.gold), "%9ld", (long)p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	(void)strnfmt(the_score.turns, sizeof(the_score.turns), "%9ld", (long)turn);
	the_score.turns[9] = '\0';

	/* Save the date in standard encoded form (9 chars) */
	(void)strftime(the_score.day, 10, "@%Y%m%d", localtime(&death_time));

	/* Save the player name (30 chars) */
	(void)strnfmt(the_score.who, sizeof(the_score.who), "%-.30s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
	(void)strnfmt(the_score.uid, sizeof(the_score.uid), "%7d", player_uid);
	(void)strnfmt(the_score.sex, sizeof(the_score.sex), "%c", (p_ptr->psex ? 'm' : 'f'));
	(void)strnfmt(the_score.p_r, sizeof(the_score.p_r), "%2d", p_ptr->prace);
	(void)strnfmt(the_score.p_mag, sizeof(the_score.p_mag), "%2d", p_ptr->realm);

	/* Save the title */
	character_title = get_title(25, FALSE, FALSE);
	(void)strnfmt(the_score.title, sizeof(the_score.title), "%s", character_title);

	/* Save the best kill */
	if (best_kill_description(kill_desc, sizeof(kill_desc), FALSE))
	{
		(void)strnfmt(the_score.kill_desc, sizeof(the_score.kill_desc), "%s", kill_desc);
	}
	else
	{
		(void)strnfmt(the_score.kill_desc, sizeof(the_score.kill_desc), "(No great victories recorded)");
	}

	/* Save the level and such */
	(void)strnfmt(the_score.cur_dun, sizeof(the_score.cur_dun), "%3d", p_ptr->depth);
	(void)strnfmt(the_score.max_dun, sizeof(the_score.max_dun), "%3d", p_ptr->max_depth);

	/* Save the cause of death (31 chars) */
	(void)strnfmt(the_score.how, sizeof(the_score.how), "%-.31s", p_ptr->died_from);

	/* Grab permissions */
	safe_setuid_grab();

	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK)) return (1);

	/* Drop permissions */
	safe_setuid_drop();

	/* Add a new entry to the score list, see where it went */
	score_idx = highscore_add(&the_score);

	/* Grab permissions */
	safe_setuid_grab();

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK)) return (1);

	/* Drop permissions */
	safe_setuid_drop();

	/* Success */
	return (0);
}



/*
 * Enters a player's name on a high score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static void top_twenty(void)
{
	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		message_flush();
		return;
	}

	/* Use the standard or tall display and center an 80 column view */
	if (more_tall_display)
	{
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_TALL | DSP_CX,
			80, 0);
	}
	else
	{
		display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX,
			80, 0);
	}

	/* Display the top 20 scores */
	if (score_idx <= 5)
	{
		display_scores_aux(0, 20, score_idx, NULL);
	}

	/* Display the 20 scores surrounding the player */
	else
	{
		display_scores_aux(score_idx - 2, score_idx + 17, score_idx, NULL);
	}

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

	/* Success */
	return;
}


/*
 * Predict the player's score, and display it.
 */
errr predict_score(void)
{
	int this_score_idx;
	cptr character_title;
	char kill_desc[DESC_LEN];

	high_score the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		message_flush();
		return (0);
	}


	/* Save the version */
	(void)strnfmt(the_score.what, sizeof(the_score.what), "%s", VERSION_STRING);

	/* Calculate and save the points */
	(void)strnfmt(the_score.pts, sizeof(the_score.pts), "%9ld", (long)total_points());

	/* Save the current gold */
	(void)strnfmt(the_score.gold, sizeof(the_score.gold), "%9ld", (long)p_ptr->au);

	/* Save the current turn */
	(void)strnfmt(the_score.turns, sizeof(the_score.turns), "%9ld", (long)turn);

	/* Hack -- no time needed */
	strcpy(the_score.day, "TODAY");

	/* Save the player name (30 chars) */
	(void)strnfmt(the_score.who, sizeof(the_score.who), "%-.30s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
#ifdef SAVEFILE_USE_UID
	(void)strnfmt(the_score.uid, sizeof(the_score.uid), "%7d", player_uid);
#endif /* SAVEFILE_USE_UID */

	(void)strnfmt(the_score.sex, sizeof(the_score.sex), "%c", (p_ptr->psex ? 'm' : 'f'));
	(void)strnfmt(the_score.p_r, sizeof(the_score.p_r), "%2d", p_ptr->prace);
	(void)strnfmt(the_score.p_mag, sizeof(the_score.p_mag), "%2d", p_ptr->realm);

	/* Save the title */
	character_title = get_title(25, FALSE, FALSE);
	(void)strnfmt(the_score.title, sizeof(the_score.title), "%s", character_title);

	/* Save the best kill */
	if (best_kill_description(kill_desc, sizeof(kill_desc), FALSE))
	{
		(void)strnfmt(the_score.kill_desc, sizeof(the_score.kill_desc), "%s", kill_desc);
	}
	else
	{
		(void)strnfmt(the_score.kill_desc, sizeof(the_score.kill_desc), "(No great victories recorded)");
	}

	/* Save the level and such */
	(void)strnfmt(the_score.cur_dun, sizeof(the_score.cur_dun), "%3d", p_ptr->depth);
	(void)strnfmt(the_score.max_dun, sizeof(the_score.max_dun), "%3d", p_ptr->max_depth);

	/* Hack -- no cause of death */
	strcpy(the_score.how, "alive and well");


	/* See where the entry would be placed */
	this_score_idx = highscore_where(&the_score);

	/* Display the top few scores */
	if (this_score_idx <= 5)
	{
		display_scores_aux(0, 20, this_score_idx, &the_score);
	}

	/* Display some "useful" scores */
	else
	{
		display_scores_aux(this_score_idx - 2, this_score_idx + 17, this_score_idx,
			&the_score);
	}


	/* Success */
	return (0);
}




/*
 * Change the player into a Winner
 */
static void kingly(void)
{
	int i;

	char buf[1024];


	/* Hack -- retire in town */
	p_ptr->depth = 0;

	/* Restore skills */
	for (i = 0; i < NUM_SKILLS; i++)
	{
		if (p_ptr->pskills[i].cur < p_ptr->pskills[i].max)
		{
			p_ptr->pskills[i].cur = p_ptr->pskills[i].max;
		}
	}

	/* Hack -- Instant Gold */
	p_ptr->au += 10000000L;


	/*** Display the "victory" file ***/

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "victory.txt");

	/* Request a full-screen display (must be the size of the tall display) */
	display_change(DSP_SAVE | DSP_FULL,
		term_size_min[WINDOW_DISPLAY][0], term_size_min[WINDOW_DISPLAY][1]);

	/* Display the file */
	(void)display_file(buf);

	/* Wait for response */
	pause_line(Term->rows - 1);

	/* Cancel full-screen view */
	display_change(DSP_FULL | DSP_LOAD, 0, 0);
}


/*
 * Handle character death
 */
static void close_game_aux(void)
{
	int ch;
	char buf[256];

	bool wants_to_quit = FALSE;

	/* Dump bones file */
	make_bones();

	/* Lock the screen */
	display_change(DSP_REMEMBER | DSP_LOCK, 0, 0);

	/* Handle retirement */
	if (p_ptr->total_winner) kingly();

	/* Save dead player */
	if (!save_player())
	{
		msg_print("death save failed!");
		message_flush();
	}

	/* Get time of death */
	(void)time(&death_time);

	/* Hack - Know everything upon death */
	death_knowledge();

	/* Enter player in high score list */
	(void)enter_score();

	/* Flush all input keys */
	flush();

	/* Flush messages */
	message_flush();


	/* Build a centered prompt */
	center_string(buf, sizeof(buf),
		"[(i)nformation, (m)essages, (f)ile dump, (v)iew scores, e(x)amine item, ESC]",
		display_width());

	/* Loop */
	while (!wants_to_quit)
	{
		/* Display the tombstone */
		print_tomb();

		/* Describe options */
		(void)Term_putstr(1, 23, -1, TERM_WHITE, buf);

		/* Query */
		ch = inkey(FALSE);

		switch (ch)
		{
			/* Exit */
			case ESCAPE:
			{
				if (get_check("Do you want to quit?"))
					wants_to_quit = TRUE;

				break;
			}

			/* File dump */
			case 'f':
			case 'F':
			{
				char ftmp[DESC_LEN];

				(void)strnfmt(ftmp, sizeof(ftmp), "%s.txt", op_ptr->base_name);

				if (get_string("File name:", ftmp, sizeof(ftmp)))
				{
					if (ftmp[0] && (ftmp[0] != ' '))
					{
						errr err;

						/* Save screen */
						screen_save(TRUE);

						/* Dump a character file */
						err = file_character(ftmp, FALSE);

						/* Load screen */
						screen_load();

						/* Check result */
						if (err)
						{
							msg_print("Character dump failed!");
						}
						else
						{
							/* Prompt */
							msg_format("Character dump saved in the \"%s\" directory.",
									ANGBAND_DIR_USER);
						}

						/* Flush messages */
						message_flush();
					}
				}

				break;
			}

			/* Show more info */
			case 'i':
			case 'I':
			{
				/* Show the character */
				show_info();

				break;
			}

			/* Show last messages */
			case 'm':
			case 'M':
			{
				/* Display messages */
				do_cmd_messages();

				break;
			}

			/* Show top scores */
			case 'v':
			case 'V':
			{
				/* Show the scores */
				top_twenty();

				break;
			}

			/* Examine an item */
			case 'x':
			case 'X':
			{
				/* Examine items */
				death_examine();

				break;
			}
		}
	}

	/* Unlock the screen */
	display_change(DSP_UNLOCK | DSP_RESTORE, 0, 0);
}


/*
 * Close up the current game (player may or may not be dead)
 */
void close_game(void)
{
	char buf[1024];
	char ch;


	/* Handle stuff */
	handle_stuff();

	/* Flush the messages */
	message_flush();

	/* Flush the input */
	flush();


	/* No suspending now */
	signals_ignore_tstp();


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Grab permissions */
	safe_setuid_grab();

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);

	/* Drop permissions */
	safe_setuid_drop();

	/* Handle death */
	if (p_ptr->is_dead)
	{
		/* Auxiliary routine */
		close_game_aux();
	}

	/* Still alive */
	else
	{
		/* Save the game */
		do_cmd_save_game(FALSE);

		/* Flush the messages */
		message_flush();

		/* Prompt for scores XXX XXX XXX */
		prt("Press 'S' to see high scores, or any other key to quit.", 0, 0);

		/* Hack -- hide the map cursor */
		if (use_special_map) inkey_cursor_hack[TERM_MAP] = -1;

		/* Wait for it (no clicks) */
		ch = inkey(FALSE);

		/* Predict score if asked */
		if ((ch == 'S') || (ch == 's'))
		{
			/* Lock the screen */
			display_change(DSP_LOCK, 0, 0);

			/* Display scores */
			(void)predict_score();

			/* Unlock the screen */
			display_change(DSP_UNLOCK, 0, 0);
		}
	}


	/* Shut the high score file */
	(void)fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;

	/* Allow suspending now */
	signals_handle_tstp();
}


/*
 * Handle abrupt death of the visual system
 *
 * This routine is called only in very rare situations, and only
 * by certain visual systems, when they experience fatal errors.
 *
 * XXX XXX Hack -- clear the death flag when creating a HANGUP
 * save file so that player can see tombstone when restart.
 */
void exit_game_panic(void)
{
	/* If nothing important has happened, just quit */
	if (!character_generated || character_saved) quit("panic");

	/* Mega-Hack -- see "msg_print()" */
	msg_flag = FALSE;

	/* Clear the top line */
	prt("", 0, 0);

	/* Hack -- turn off some things */
	disturb(1, 0);

	/* Hack -- Delay death XXX XXX XXX */
	if (p_ptr->chp < 0) p_ptr->is_dead = FALSE;

	/* Hardcode panic save */
	p_ptr->panic_save = 1;

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Indicate panic save */
	strcpy(p_ptr->died_from, "(panic save)");

	/* Panic save, or get worried */
	if (!save_player()) quit("panic save failed!");

	/* Successful panic save */
	quit("panic save succeeded!");
}





/*
 * The signal-handling code.  This is all platform-specific and should not be
 * in the game module.  However, I am not experienced enough to properly
 * separate out the non-game bits into an external module (z-util.c, for
 * example).
 */
#ifdef HANDLE_SIGNALS


#include <signal.h>


typedef void (*Signal_Handler_t)(int);

/*
 * Wrapper around signal() which it is safe to take the address
 * of, in case signal itself is hidden by some some macro magic.
 */
static Signal_Handler_t wrap_signal(int sig, Signal_Handler_t handler)
{
	return (signal(sig, handler));
}

/* Call this instead of calling signal() directly. */
Signal_Handler_t (*signal_aux)(int, Signal_Handler_t) = wrap_signal;



/*
 * Handle signals -- term resize (SIGWINCH)  -CJN-
 */
static void handle_signal_resize(int sig)
{
	/* Protect errno from library calls in signal handler */
	int save_errno = errno;

	/* Disable handler */
	(void)(*signal_aux)(sig, SIG_IGN);

	/* React to changes */
	(void)Term_xtra(TERM_XTRA_REACT, 0);

	/* Restore handler */
	(void)(*signal_aux)(sig, handle_signal_resize);

	/* Restore errno */
	errno = save_errno;
}


/*
 * Handle signals -- suspend
 *
 * Actually suspend the game, and then resume cleanly
 */
static void handle_signal_suspend(int sig)
{
	/* Protect errno from library calls in signal handler */
	int save_errno = errno;

	/* Disable handler */
	(void)(*signal_aux)(sig, SIG_IGN);

#ifdef SIGSTOP

	/* Flush output */
	(void)Term_fresh();

	/* Suspend the "Term" */
	(void)Term_xtra(TERM_XTRA_ALIVE, 0);

	/* Suspend ourself */
	(void)kill(0, SIGSTOP);

	/* Resume the "Term" */
	(void)Term_xtra(TERM_XTRA_ALIVE, 1);

	/* Redraw the term */
	(void)Term_redraw();

	/* Flush the term */
	(void)Term_fresh();

#endif

	/* Restore handler */
	(void)(*signal_aux)(sig, handle_signal_suspend);

	/* Restore errno */
	errno = save_errno;
}


/*
 * Handle signals -- simple (interrupt and quit)
 *
 * This function was causing a *huge* number of problems, so it has
 * been simplified greatly.  We keep a global variable which counts
 * the number of times the user attempts to kill the process, and
 * we commit suicide if the user does this a certain number of times.
 *
 * We attempt to give "feedback" to the user as he approaches the
 * suicide thresh-hold, but without penalizing accidental keypresses.
 *
 * To prevent messy accidents, we should reset this global variable
 * whenever the user enters a keypress, or something like that.
 */
static void handle_signal_simple(int sig)
{
	/* Protect errno from library calls in signal handler */
	int save_errno = errno;

	/* Disable handler */
	(void)(*signal_aux)(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	/* Count the signals */
	signal_count++;


	/* Terminate dead characters */
	if (p_ptr->is_dead)
	{
		/* Mark the savefile */
		strcpy(p_ptr->died_from, "(Game was aborted)");

		/* HACK - Skip the tombscreen if it is already displayed */
		if (score_idx == -1)
		{
			/* Close stuff */
			close_game();
		}

		/* Quit */
		quit("interrupt");
	}

	/* Allow suicide (after 5) */
	else if (signal_count >= 5)
	{
		/* Cause of "death" */
		strcpy(p_ptr->died_from, "(Suicide -- at least 5 interrupts)");

		/* Commit suicide */
		p_ptr->is_dead = TRUE;

		/* Stop playing */
		p_ptr->playing = FALSE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Close stuff */
		close_game();

		/* Quit */
		quit("interrupt");
	}

	/* Give warning (after 4) */
	else if (signal_count >= 4)
	{
		/* Make a noise */
		(void)Term_xtra(TERM_XTRA_NOISE, 0);

		/* Clear the top line */
		clear_row(0);

		/* Display the cause */
		(void)Term_putstr(0, 0, -1, TERM_WHITE, "Contemplating suicide!");

		/* Flush */
		(void)Term_fresh();
	}

	/* Give warning (after 2) */
	else if (signal_count >= 2)
	{
		/* Make a noise */
		(void)Term_xtra(TERM_XTRA_NOISE, 0);
	}

	/* Restore handler */
	(void)(*signal_aux)(sig, handle_signal_simple);

	/* Restore errno */
	errno = save_errno;
}


/*
 * Handle signal -- abort, kill, etc
 */
static void handle_signal_abort(int sig)
{
	int row = Term->rows - 1;

	/* Disable handler */
	(void)(*signal_aux)(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	/* Clear the bottom line */
	clear_row(row);

	/* Give a warning */
	(void)Term_putstr(0, row, -1, TERM_RED,
		"A gruesome software bug LEAPS out at you!");

	/* Message */
	(void)Term_putstr(45, row, -1, TERM_RED, "Panic save...");

	/* Flush output */
	(void)Term_fresh();

	/* Panic Save */
	p_ptr->panic_save = 1;

	/* Panic save */
	strcpy(p_ptr->died_from, "(panic save)");

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Attempt to save */
	if (save_player())
	{
		(void)Term_putstr(45, row, -1, TERM_RED, "Panic save succeeded!");
	}

	/* Save failed */
	else
	{
		(void)Term_putstr(45, row, -1, TERM_RED, "Panic save failed!");
	}

	/* Flush output */
	(void)Term_fresh();

	/* Quit */
	quit("software bug");
}




/*
 * Ignore SIGTSTP signals (keyboard suspend)
 */
void signals_ignore_tstp(void)
{

#ifdef SIGTSTP
	(void)(*signal_aux)(SIGTSTP, SIG_IGN);
#endif

}

/*
 * Handle SIGTSTP signals (keyboard suspend)
 */
void signals_handle_tstp(void)
{

#ifdef SIGTSTP
	(void)(*signal_aux)(SIGTSTP, handle_signal_suspend);
#endif

}


/*
 * Prepare to handle the relevant signals
 */
void signals_init(void)
{

#ifdef SIGHUP
	(void)(*signal_aux)(SIGHUP, SIG_IGN);
#endif


#ifdef SIGTSTP
	(void)(*signal_aux)(SIGTSTP, handle_signal_suspend);
#endif


#ifdef SIGINT
	(void)(*signal_aux)(SIGINT, handle_signal_simple);
#endif

#ifdef SIGQUIT
	(void)(*signal_aux)(SIGQUIT, handle_signal_simple);
#endif


#ifdef SIGFPE
	(void)(*signal_aux)(SIGFPE, handle_signal_abort);
#endif

#ifdef SIGILL
	(void)(*signal_aux)(SIGILL, handle_signal_abort);
#endif

#ifdef SIGTRAP
	(void)(*signal_aux)(SIGTRAP, handle_signal_abort);
#endif

#ifdef SIGIOT
	(void)(*signal_aux)(SIGIOT, handle_signal_abort);
#endif

#ifdef SIGBUS
	(void)(*signal_aux)(SIGBUS, handle_signal_abort);
#endif

#ifdef SIGSEGV
	(void)(*signal_aux)(SIGSEGV, handle_signal_abort);
#endif

#ifdef SIGTERM
	(void)(*signal_aux)(SIGTERM, handle_signal_abort);
#endif

#ifdef SIGPIPE
	(void)(*signal_aux)(SIGPIPE, handle_signal_abort);
#endif

#ifdef SIGEMT
	(void)(*signal_aux)(SIGEMT, handle_signal_abort);
#endif

/*
 * SIGDANGER:
 * This is not a common (POSIX, SYSV, BSD) signal, it is used by AIX(?) to
 * signal that the system will soon be out of memory.
 */
#ifdef SIGDANGER
	(void)(*signal_aux)(SIGDANGER, handle_signal_abort);
#endif

#ifdef SIGSYS
	(void)(*signal_aux)(SIGSYS, handle_signal_abort);
#endif

#ifdef SIGXCPU
	(void)(*signal_aux)(SIGXCPU, handle_signal_abort);
#endif

#ifdef SIGPWR
	(void)(*signal_aux)(SIGPWR, handle_signal_abort);
#endif

#ifdef SIGWINCH
	(void)(*signal_aux)(SIGWINCH, handle_signal_resize);
#endif
}


#else	/* HANDLE_SIGNALS */


/*
 * Do nothing
 */
void signals_ignore_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_handle_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_init(void)
{
}


#endif	/* HANDLE_SIGNALS */

