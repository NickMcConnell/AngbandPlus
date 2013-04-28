
/* File: files.c */

/*
 * Drop and set permissions.  Read preference files.  Check time and load.
 * Get best kill.  Display the character screen.  Character dumps, read
 * random line from a file.  Show a file (inc. the online help), context-
 * specific help.  Display a file (alternate method).  Process a character
 * name, commit suicide, save the game.  Get and display score, turn
 * character into a winner.  The character death interface.  Controlled
 * exit and panic saves.
 *
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Hack -- drop permissions
 */
void safe_setuid_drop(void)
{

#ifdef SET_UID

# ifdef SAFE_SETUID

#  ifdef HAVE_SETEGID

	if (setegid(getgid()) != 0)
	{
		quit("setegid(): cannot set permissions correctly!");
	}

#  else /* HAVE_SETEGID */

#   ifdef SAFE_SETUID_POSIX

	if (setgid(getgid()) != 0)
	{
		quit("setgid(): cannot set permissions correctly!");
	}

#   else /* SAFE_SETUID_POSIX */

	if (setregid(getegid(), getgid()) != 0)
	{
		quit("setregid(): cannot set permissions correctly!");
	}

#   endif /* SAFE_SETUID_POSIX */

#  endif /* HAVE_SETEGID */

# endif /* SAFE_SETUID */

#endif /* SET_UID */

}


/*
 * Hack -- grab permissions
 */
void safe_setuid_grab(void)
{

#ifdef SET_UID

# ifdef SAFE_SETUID

#  ifdef HAVE_SETEGID

	if (setegid(player_egid) != 0)
	{
		quit("setegid(): cannot set permissions correctly!");
	}

#  else /* HAVE_SETEGID */

#   ifdef SAFE_SETUID_POSIX

	if (setgid(player_egid) != 0)
	{
		quit("setgid(): cannot set permissions correctly!");
	}

#   else /* SAFE_SETUID_POSIX */

	if (setregid(getegid(), getgid()) != 0)
	{
		quit("setregid(): cannot set permissions correctly!");
	}

#   endif /* SAFE_SETUID_POSIX */

#  endif /* HAVE_SETEGID */

# endif /* SAFE_SETUID */

#endif /* SET_UID */

}

/*
 * Extract the first few "tokens" from a buffer
 *
 * This function uses "colon" and "slash" as the delimiter characters.
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

				/* Handle backslash */
				if (*t == '\\') t++;

				/* Require a character */
				if (!*t) break;

				/* Advance */
				t++;

				/* Hack -- Require a close quote */
				if (*t != '\'') *t = '\'';
			}

			/* Handle back-slash */
			if (*t == '\\') t++;
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
 * Note the use of "strtol()" to allow all "integers" to be encoded
 * in decimal, hexadecimal, or octal form.
 *
 * Note that "monster zero" is used for the "player" attr/char, "object
 * zero" will be used for the "stack" attr/char, and "feature zero" is
 * used for the "nothing" attr/char.
 *
 * Specify the attr/char values for "monsters" by race index.
 *   R:<num>:<a>/<c>
 *
 * Specify the attr/char values for "objects" by kind index.
 *   K:<num>:<a>/<c>
 *
 * Specify the attr/char values for "features" by feature index.
 *   F:<num>:<a>/<c>
 *
 * Specify the attr/char values for "special" things.
 *   S:<num>:<a>/<c>
 *
 * Specify the attribute values for inventory "objects" by kind tval.
 *   E:<tv>:<a>
 *
 * Define a macro action, given an encoded macro action.
 *   A:<str>
 *
 * Create a macro, given an encoded macro trigger.
 *   P:<str>
 *
 * Create a keymap, given an encoded keymap trigger.
 *   C:<num>:<str>
 *
 * Turn an option off, given its name.
 *   X:<str>
 *
 * Turn an option on, given its name.
 *   Y:<str>
 *
 * Turn a window flag on or off, given a window, flag, and value.
 *   W:<win>:<flag>:<value>
 *
 * Specify visual information, given an index, and some data.
 *   V:<num>:<kv>:<rv>:<gv>:<bv>
 *
 * Specify colors for message-types.
 *   M:<type>:<attr>
 */
errr process_pref_file_command(char *buf)
{
	int i, j, n1, n2;

	char *zz[16];


	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace((unsigned char)buf[0])) return (0);

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
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->r_max)) return (1);
			r_ptr = &r_info[i];
			if (n1) r_ptr->x_attr = (byte)n1;
			if (n2) r_ptr->x_char = (char)n2;
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
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->k_max)) return (1);
			k_ptr = &k_info[i];
			if (n1) k_ptr->x_attr = (byte)n1;
			if (n2) k_ptr->x_char = (char)n2;
			return (0);
		}
	}


	/* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
	else if (buf[0] == 'F')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			feature_type *f_ptr;
			i = strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->f_max)) return (1);
			f_ptr = &f_info[i];

			if (n1) f_ptr->x_attr = (byte)n1;
			if (n2)
			{
				/* Update all the feature chars */
				feat_x_char_50[i] = feat_x_char_25[i] =
					f_ptr->x_char = (char)n2;
			}

			return (0);
		}
	}

	/* Process "f:<feat>:<char>" -- feature chars when in 50-line mode */
	else if (buf[0] == 'f')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{
			feature_type *f_ptr;
			i = strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->f_max)) return (1);
			f_ptr = &f_info[i];

			if (n1)
			{
				/* Update the feature char used when displaying 50 rows */
				feat_x_char_50[i] = (char)n1;

				/* Update visuals if needed */
				if (!force_25_rows) f_ptr->x_char = feat_x_char_50[i];
			}

			return (0);
		}
	}

	/* Process "S:<num>:<a>/<c>" -- attr/char for special things */
	else if (buf[0] == 'S')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			j = strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((j < 0) || (j >= (long)N_ELEMENTS(misc_to_attr))) return (1);
			misc_to_attr[j] = (byte)n1;
			misc_to_char[j] = (char)n2;
			return (0);
		}
	}


	/* Process "E:<tv>:<a>" -- attribute for inventory objects */
	else if (buf[0] == 'E')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{
			j = strtol(zz[0], NULL, 0) % 128;
			n1 = strtol(zz[1], NULL, 0);
			if ((j < 0) || (j >= (long)N_ELEMENTS(tval_to_attr))) return (1);
			if (n1) tval_to_attr[j] = (byte)n1;
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
		macro_add(tmp, macro_buffer);
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

		string_free(keymap_act[mode][i]);

		keymap_act[mode][i] = string_make(macro_buffer);

		return (0);
	}


	/* Process "V:<num>:<kv>:<rv>:<gv>:<bv>" -- visual info */
	else if (buf[0] == 'V')
	{
		if (tokenize(buf+2, 5, zz) == 5)
		{
			i = strtol(zz[0], NULL, 0);
			if ((i < 0) || (i >= 256)) return (1);
			angband_color_table[i][0] = (byte)strtol(zz[1], NULL, 0);
			angband_color_table[i][1] = (byte)strtol(zz[2], NULL, 0);
			angband_color_table[i][2] = (byte)strtol(zz[3], NULL, 0);
			angband_color_table[i][3] = (byte)strtol(zz[4], NULL, 0);
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
			macro_trigger_free();

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
			macro_trigger_name[max_macrotrigger] = string_make(buf);

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

			/* Ignore illegal windows */
			/* Hack -- Ignore the main window */
			if ((win <= 0) || (win >= ANGBAND_TERM_MAX)) return (1);

			/* Ignore illegal flags */
			if ((flag < 0) || (flag >= 32)) return (1);

			/* Require a real flag */
			if (window_flag_desc[flag])
			{
				if (value)
				{
					/* Turn flag on */
					op_ptr->window_flag[win] |= (1L << flag);
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
		int tmp, vert, hori;

		i = tokenize(buf + 2, 5, zz);

		if (i == 4)
		{
			force_25_rows = strtol(zz[0], NULL, 0);
			tmp = strtol(zz[1], NULL, 0);
			vert = strtol(zz[2], NULL, 0);
			hori = strtol(zz[3], NULL, 0);

			/* Use legal values */
			if ((tmp == 2 * BLOCK_HGT) || (tmp == 3 * BLOCK_HGT) ||
			    (tmp == 4 * BLOCK_HGT))
			{
				map_rows = tmp;
			}
			if ((vert > 1) && (vert < 14)) clear_y = vert;
			if ((hori > 1) && (hori < 14)) clear_x = hori;

			/* Success */
			return (0);
		}

		/* New system -- with text_50_rows */
		else if (i == 5)
		{
			force_25_rows = strtol(zz[0], NULL, 0);
			text_50_rows = strtol(zz[1], NULL, 0);
			tmp = strtol(zz[2], NULL, 0);
			vert = strtol(zz[3], NULL, 0);
			hori = strtol(zz[4], NULL, 0);

			/* Use legal values */
			if ((tmp == 2 * BLOCK_HGT) || (tmp == 3 * BLOCK_HGT) ||
			    (tmp == 4 * BLOCK_HGT))
			{
				map_rows = tmp;
			}
			if ((vert > 1) && (vert < 14)) clear_y = vert;
			if ((hori > 1) && (hori < 14)) clear_x = hori;

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
	while (isspace((unsigned char)*s)) s++;

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
		while (isprint((unsigned char)*s) && !strchr(" []", *s)) ++s;

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
				v = ANGBAND_GRAF;
			}
#if 0
			/* Race */
			else if (streq(b + 1, "RACE"))
			{
				v = rp_ptr->title;  /* Confirm that this is initialized */
			}

			/* Realm */
			else if (streq(b + 1, "REALM"))
			{
				v = mp_ptr->title;  /* Confirm that this is initialized */
			}
#endif
			/* Player */
			else if (streq(b + 1, "PLAYER"))
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
		if (isspace((unsigned char)buf[0])) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;


		/* Save a copy */
		my_strcpy(old, buf, sizeof(old));


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
			/* Process that file if allowed */
			(void)process_pref_file(buf + 2);

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
	my_fclose(fp);

	/* Result */
	return (err);
}



/*
 * Process the "user pref file" with the given name
 *
 * We first process any such file in the "pref" directory, then any
 * in the "user" directory.  Rules in the latter take priority.
 *
 * See the functions above for a list of legal "commands".
 *
 * We also accept the special "?" and "%" directives, which
 * allow conditional evaluation and filename inclusion.
 */
errr process_pref_file(cptr name)
{
	char buf[1024];

	errr err = 0;
	errr err2 = 0;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_PREF, name);

	/* Process the pref file */
	err = process_pref_file_aux(buf);

	/* Stop at parser errors, but not at non-existing file */
	if (err < 1)
	{
		/* Build the filename */
		path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

		/* Process the pref file */
		err2 = process_pref_file_aux(buf);

		/* Take note of parser errors, clear "missing file" errors */
		if (err2 >= 0) err = err2;
	}

	/* Result */
	return (err);
}




#ifdef CHECK_TIME

/*
 * Operating hours for ANGBAND (defaults to non-work hours)
 */
static char days[7][29] =
{
	"SUN:XXXXXXXXXXXXXXXXXXXXXXXX",
	"MON:XXXXXXXX.........XXXXXXX",
	"TUE:XXXXXXXX.........XXXXXXX",
	"WED:XXXXXXXX.........XXXXXXX",
	"THU:XXXXXXXX.........XXXXXXX",
	"FRI:XXXXXXXX.........XXXXXXX",
	"SAT:XXXXXXXXXXXXXXXXXXXXXXXX"
};

/*
 * Restrict usage (defaults to no restrictions)
 */
static bool check_time_flag = FALSE;

#endif /* CHECK_TIME */


/*
 * Handle CHECK_TIME
 */
errr check_time(void)
{

#ifdef CHECK_TIME

	time_t c;
	struct tm *tp;

	/* No restrictions */
	if (!check_time_flag) return (0);

	/* Check for time violation */
	c = time((time_t *)0);
	tp = localtime(&c);

	/* Violation */
	if (days[tp->tm_wday][tp->tm_hour + 4] != 'X') return (1);

#endif /* CHECK_TIME */

	/* Success */
	return (0);
}



/*
 * Initialize CHECK_TIME
 */
errr check_time_init(void)
{

#ifdef CHECK_TIME

	FILE *fp;

	char buf[1024];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "time.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No file, no restrictions */
	if (!fp) return (0);

	/* Assume restrictions */
	check_time_flag = TRUE;

	/* Parse the file */
	while (0 == my_fgets(fp, buf, sizeof(buf)))
	{
		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Chop the buffer */
		buf[sizeof(days[0]) - 1] = '\0';

		/* Extract the info */
		if (prefix(buf, "SUN:")) my_strcpy(days[0], buf, sizeof(days[0]));
		if (prefix(buf, "MON:")) my_strcpy(days[1], buf, sizeof(days[1]));
		if (prefix(buf, "TUE:")) my_strcpy(days[2], buf, sizeof(days[2]));
		if (prefix(buf, "WED:")) my_strcpy(days[3], buf, sizeof(days[3]));
		if (prefix(buf, "THU:")) my_strcpy(days[4], buf, sizeof(days[4]));
		if (prefix(buf, "FRI:")) my_strcpy(days[5], buf, sizeof(days[5]));
		if (prefix(buf, "SAT:")) my_strcpy(days[6], buf, sizeof(days[6]));
	}

	/* Close it */
	my_fclose(fp);

#endif /* CHECK_TIME */

	/* Success */
	return (0);
}



#ifdef CHECK_LOAD

#ifndef MAXHOSTNAMELEN
# define MAXHOSTNAMELEN  64
#endif

typedef struct statstime statstime;

struct statstime
{
	int cp_time[4];
	int dk_xfer[4];
	unsigned int        v_pgpgin;
	unsigned int        v_pgpgout;
	unsigned int        v_pswpin;
	unsigned int        v_pswpout;
	unsigned int        v_intr;
	int if_ipackets;
	int if_ierrors;
	int if_opackets;
	int if_oerrors;
	int if_collisions;
	unsigned int        v_swtch;
	long avenrun[3];
	struct timeval      boottime;
	struct timeval      curtime;
};

/*
 * Maximal load (if any).
 */
static int check_load_value = 0;

#endif /* CHECK_LOAD */


/*
 * Handle CHECK_LOAD
 */
errr check_load(void)
{

#ifdef CHECK_LOAD

	struct statstime    st;

	/* Success if not checking */
	if (!check_load_value) return (0);

	/* Check the load */
	if (0 == rstat("localhost", &st))
	{
		long val1 = (long)(st.avenrun[2]);
		long val2 = (long)(check_load_value) * FSCALE;

		/* Check for violation */
		if (val1 >= val2) return (1);
	}

#endif /* CHECK_LOAD */

	/* Success */
	return (0);
}


/*
 * Initialize CHECK_LOAD
 */
errr check_load_init(void)
{


#ifdef CHECK_LOAD

	FILE *fp;

	char buf[1024];

	char temphost[MAXHOSTNAMELEN+1];
	char thishost[MAXHOSTNAMELEN+1];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "load.txt");

	/* Open the "load" file */
	fp = my_fopen(buf, "r");

	/* No file, no restrictions */
	if (!fp) return (0);

	/* Default load */
	check_load_value = 100;

	/* Get the host name */
	(void)gethostname(thishost, (sizeof thishost) - 1);

	/* Parse it */
	while (0 == my_fgets(fp, buf, sizeof(buf)))
	{
		int value;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Parse, or ignore */
		if (sscanf(buf, "%s%d", temphost, &value) != 2) continue;

		/* Skip other hosts */
		if (!streq(temphost, thishost) &&
			!streq(temphost, "localhost")) continue;

		/* Use that value */
		check_load_value = value;

		/* Done */
		break;
	}

	/* Close the file */
	my_fclose(fp);

#endif /* CHECK_LOAD */

	/* Success */
	return (0);
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
	sprintf(out_val, "%9ld", (long)num);
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
	sprintf(out_val, "%6ld", (long)num);
	c_put_str(color, out_val, row, col + len);
}



/*
 * Prints the following information on the screen.
 *
 * For this to look right, the following should be spaced the
 * same as in the prt_lnum code... -CFT
 */
static void display_player_middle(void)
{
	int show_m_tohit = p_ptr->dis_to_h;
	int show_a_tohit = p_ptr->dis_to_h;
	int show_m_todam = p_ptr->dis_to_d;
	int show_a_todam = p_ptr->dis_to_d;

	object_type *o_ptr;
	char tmp[80];

	/* Dump the fighting bonuses to hit/dam */

	o_ptr = &inventory[INVEN_WIELD];

	/* Combat information -- melee */
	put_str("       (Melee)       ", 12, 1);
	prt_num("Blows/Round      ", p_ptr->num_blow, 13, 1, TERM_L_BLUE);

	/* Using a weapon */
	if (is_melee_weapon(o_ptr))
	{
		/* Hack -- add in weapon info if known */
		if (object_known_p(o_ptr)) show_m_tohit += o_ptr->to_h;
		if (object_known_p(o_ptr)) show_m_todam += o_ptr->to_d;

		/* Show Skill and Deadliness */
		prt_num("+ to Skill       ", show_m_tohit, 14, 1, TERM_L_BLUE);

		if (show_m_todam > 0)
			prt_num("Deadliness (%)   ", deadliness_conversion[show_m_todam], 15, 1, TERM_L_BLUE);
		else
			prt_num("Deadliness (%)   ", -deadliness_conversion[-show_m_todam], 15, 1, TERM_L_BLUE);
	}

	/* Using martial arts */
	else
	{
		/* Point to the right variable */
		s16b *temp = &p_ptr->karate_dam;
		if (p_ptr->barehand != S_KARATE) temp = &p_ptr->wrestling_dam;

		/* Display average damage */
		prt_num("Average Damage   ", (*temp + 5) / 10, 14, 1, TERM_L_BLUE);

		/* Note which martial art we're using */
		if (p_ptr->barehand == S_KARATE)
			put_str("    (using Karate)   ", 15, 1);
		else
			put_str("     (Wrestling)     ", 15, 1);
	}



	/* Dump the shooting bonuses to hit/dam */

	o_ptr = &inventory[INVEN_BOW];

	/* Hack -- add in weapon info if known */
	if (object_known_p(o_ptr)) show_a_tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) show_a_todam += o_ptr->to_d;


	/* Combat information -- shooting */
	put_str("       (Missile)     ", 12, 53);
	if (p_ptr->num_fire % 2)
	{
		prt_num("Shots/Round    ", p_ptr->num_fire / 2, 13, 53, TERM_L_BLUE);
		c_put_str(TERM_L_BLUE, ".5", 13, 74);
	}
	else
	{
		prt_num("Shots/Round      ", p_ptr->num_fire / 2, 13, 53, TERM_L_BLUE);
	}

	prt_num("+ to Skill       ", show_a_tohit, 14, 53, TERM_L_BLUE);
	if (show_a_todam > 0)
		prt_num("Deadliness (%)   ", deadliness_conversion[show_a_todam], 15, 53, TERM_L_BLUE);
	else
		prt_num("Deadliness (%)   ", -deadliness_conversion[-show_a_todam], 15, 53, TERM_L_BLUE);


	/* Print maximum depth */
	put_str("Max Depth", 15, 27);

	if (depth_in_feet)
	{
		if (use_metric) sprintf(tmp, " %4d m", p_ptr->max_depth * 15);
		else sprintf(tmp, "%4d ft", p_ptr->max_depth * 50);
	}
	else
	{
		if (p_ptr->max_depth < 10)
		     sprintf(tmp, "  Lev %d", p_ptr->max_depth);
		else if (p_ptr->max_depth < 100)
		     sprintf(tmp, " Lev %d", p_ptr->max_depth);
		else sprintf(tmp, "Lev %d", p_ptr->max_depth);
	}
	c_put_str(TERM_L_BLUE, tmp, 15, 43);



	/* Power, score, Unspent Exp, Gold */
	prt_num("Power            ", (int)p_ptr->power, 9, 27, TERM_L_GREEN);
	prt_lnum("Score         ",   total_points(),   10, 27, TERM_L_BLUE);

	prt_lnum("Unspent Exp   ",   p_ptr->exp,       12, 27, TERM_L_GREEN);
	prt_lnum("Gold          ",   p_ptr->au,        13, 27, TERM_L_GREEN);


	/* Hitpoints, Mana */
	prt_num("Max Hit Points   ", p_ptr->mhp, 9, 1, TERM_L_GREEN);

	if (p_ptr->chp >= p_ptr->mhp)
	{
		prt_num("Cur Hit Points   ", p_ptr->chp, 10, 1, TERM_L_GREEN);
	}
	else if (p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
	{
		prt_num("Cur Hit Points   ", p_ptr->chp, 10, 1, TERM_YELLOW);
	}
	else
	{
		prt_num("Cur Hit Points   ", p_ptr->chp, 10, 1, TERM_RED);
	}


	prt_num("Max Mana         ", p_ptr->msp, 9, 53, TERM_L_GREEN);

	if (p_ptr->csp >= p_ptr->msp)
	{
		prt_num("Cur Mana         ", p_ptr->csp, 10, 53, TERM_L_GREEN);
	}
	else if (p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
	{
		prt_num("Cur Mana         ", p_ptr->csp, 10, 53, TERM_YELLOW);
	}
	else
	{
		prt_num("Cur Mana         ", p_ptr->csp, 10, 53, TERM_RED);
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
			likert_color = TERM_VIOLET;
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
	int tmp, attr;
	int xthn, xthb, xtht, xdig, xsrh;
	int xdis, xdev, xsav, xstl;
	cptr desc;
	char fame_desc[80];

	object_type *o_ptr;


	/* Get melee weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* We are actually using a weapon */
	if (is_melee_weapon(o_ptr))
	{
		/* Fighting Skill (with current weapon) */
		tmp = p_ptr->to_h + o_ptr->to_h;
		xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);
	}
	/* We're using bare-handed combat */
	else
	{
		xthn = p_ptr->skill_thn;
	}

	/* Get missile launcher */
	o_ptr = &inventory[INVEN_BOW];

	/* Shooting Skill (with current bow and normal missile) */
	tmp = p_ptr->to_h + o_ptr->to_h;
	xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

	/* Throwing Skill (ordinary object) */
	xtht = 3 * p_ptr->skill_tht / 2;

	/* Digging ability */
	xdig = p_ptr->skill_dig;

	/* Basic abilities */
	xdis = p_ptr->skill_dis;
	xdev = p_ptr->skill_dev;
	xsav = p_ptr->skill_sav;
	xstl = p_ptr->skill_stl;
	xsrh = p_ptr->skill_srh;


	put_str("Melee       :", 18, 1);
	desc = likert(xthn, 8 + p_ptr->power / 20);
	c_put_str(likert_color, desc, 18, 15);

	put_str("Shooting    :", 19, 1);
	desc = likert(xthb, 5 + p_ptr->power / 15);
	c_put_str(likert_color, desc, 19, 15);

	put_str("Throwing    :", 20, 1);
	desc = likert(xtht, 6 + p_ptr->power / 15);
	c_put_str(likert_color, desc, 20, 15);

	put_str("Digging     :", 21, 1);
	desc = likert(xdig, 10);
	c_put_str(likert_color, desc, 21, 15);


	put_str("Saving Throw:", 18, 27);
	desc = likert(xsav, 6);
	c_put_str(likert_color, desc, 18, 41);

	put_str("Stealth     :", 19, 27);
	if (p_ptr->aggravate)
	{
		c_put_str(TERM_L_RED, "Aggravate", 19, 41);
	}
	else
	{
		desc = likert(xstl, 1);
		c_put_str(likert_color, desc, 19, 41);
	}

	put_str("Perception  :", 20, 27);
	desc = likert(xsrh, 3 + p_ptr->power / 40);
	c_put_str(likert_color, desc, 20, 41);

	put_str("Disarming   :", 21, 27);
	desc = likert(xdis, 5 + p_ptr->power / 23);
	c_put_str(likert_color, desc, 21, 41);

	put_str("Magic Device:", 18, 53);
	desc = likert(xdev, 5 + p_ptr->power / 40);
	c_put_str(likert_color, desc, 18, 67);

	put_str("Dodging     :", 19, 53);
	desc = likert(dodging_ability(300), 11 + p_ptr->power / 20);
	c_put_str(likert_color, desc, 19, 67);

	put_str("Fame        :", 20, 53);
	get_fame_desc(&attr, fame_desc);
	c_put_str(attr, fame_desc, 20, 67);

	put_str("Infra-Vision:", 21, 53);
	if (use_metric) put_str(format("%d meters", p_ptr->see_infra * 3), 21, 67);
	else put_str(format("%d feet", p_ptr->see_infra * 10), 21, 67);
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
		Term_putch(x + i - INVEN_WIELD, y, a, c);
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
	"",
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
 */
static void display_player_flag_info(void)
{
	int x, y, i, n;

	int row, col;
	int attr, attr_title;

	int flag;
	cptr name;

	u32b f[4];
	u32b f_player[4];
	u32b f_cancel[4];


	/* Get player flags */
	player_flags(&f_player[1], &f_player[2], &f_player[3], TRUE, FALSE);

	/* Get "cancelled" flags */
	player_flags_cancel(&f_cancel[1], &f_cancel[2], &f_cancel[3], TRUE);


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

			/* Get flag index */
			flag = get_flag_here(4 + x, y);

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
					attr_title = TERM_GREEN;
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
						attr_title = TERM_GREEN;
						c_put_str(TERM_L_BLUE, "*", row, col + n);
					}
				}
				if (flag == RES_ELEC)
				{
					if (f[2] & (TR2_IM_ELEC))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_L_BLUE, "*", row, col + n);
					}
				}
				if (flag == RES_FIRE)
				{
					if (f[2] & (TR2_IM_FIRE))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_L_BLUE, "*", row, col + n);
					}
				}
				if (flag == RES_COLD)
				{
					if (f[2] & (TR2_IM_COLD))
					{
						attr_title = TERM_GREEN;
						c_put_str(TERM_L_BLUE, "*", row, col + n);
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
				attr_title = TERM_GREEN;
				c_put_str(TERM_WHITE, "+", row, col + n);
			}

			/* Special case -- Check immunities */
			if (flag == RES_ACID)
			{
				if (f_player[2] & (TR2_IM_ACID))
				{
					attr_title = TERM_GREEN;
					c_put_str(TERM_L_BLUE, "*", row, col + n);
				}
			}
			if (flag == RES_ELEC)
			{
				if (f_player[2] & (TR2_IM_ELEC))
				{
					attr_title = TERM_GREEN;
					c_put_str(TERM_L_BLUE, "*", row, col + n);
				}
			}
			if (flag == RES_FIRE)
			{
				if (f_player[2] & (TR2_IM_FIRE))
				{
					attr_title = TERM_GREEN;
					c_put_str(TERM_L_BLUE, "*", row, col + n);
				}
			}
			if (flag == RES_COLD)
			{
				if (f_player[2] & (TR2_IM_COLD))
				{
					attr_title = TERM_GREEN;
					c_put_str(TERM_L_BLUE, "*", row, col + n);
				}
			}

			/* Check to see if the character cancels this flag */
			if (f_cancel[flag / 32] & (1L << (flag % 32)))
			{
				/* For "good" flags, cancellation is bad. */
				attr = TERM_L_RED;

				/* For "bad" flags, cancellation is good */
				if ((flag / 32 == 3) && ((1L << (flag % 32)) >= TR3_SOULSTEAL))
					attr = TERM_L_BLUE;

				/* Note cancellation */
				c_put_str(attr, "X", row, col + n);
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

	char buf[80];

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

		/* Internal stat value */
		cnv_stat(p_ptr->stat_max[stat], buf);
		c_put_str(TERM_BLUE, buf, row + stat, stat_col + 5);

		/* Resulting "modified" maximum value */
		cnv_stat(p_ptr->stat_top[stat], buf);
		c_put_str(TERM_L_GREEN, buf, row + stat, stat_col + 26);

		/* Only display stat_use if not maximal */
		if (p_ptr->stat_use[stat] < p_ptr->stat_top[stat])
		{
			/* Note drained stat */
			drained = TRUE;

			cnv_stat(p_ptr->stat_use[stat], buf);
			c_put_str(TERM_YELLOW, buf, row + stat, stat_col + 33);
		}
	}

	/* Provide a header for any drained stats */
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
			Term_putch(col, row + stat, a, c);
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
		Term_putch(col, row + stat, a, c);
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
				Term_putch(col + n, row, a, c);
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
			Term_putch(col + n, row, a, c);

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
			c_put_str(TERM_WHITE, "abcdefghijkl 0123456789@", row++, col + 12);
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
						Term_putch(col + n, row, attr, c);
					}
				}

				/* Special case -- display plusses for some flags */
				else if (flag >= 160)
				{
					int plus = 0;

					if (flag == ADD_SKILL)
					{
						plus = ((p_ptr->barehanded && x == 0) ?
							0 : o_ptr->to_h);
					}
					else if (flag == ADD_DEADLINESS)
					{
						plus = ((p_ptr->barehanded && x == 0) ?
							0 : o_ptr->to_d);
					}

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
						Term_putch(col + n, row, attr, c);
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

				/* Special case -- Check throwing (melee column) */
				if ((x == 0) && (flag == THROWING))
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
					Term_putch(col + n, row, attr, c);
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
 * Display the character on the screen (various modes)
 *
 * The top two and bottom two lines are left blank.
 *
 * Mode 0 = standard display with skills
 * Mode 1 = standard display with history
 * Mode 2 = special display with flags
 * Mode 3 = combat information
 */
void display_player(int mode)
{
	int i;

	int old_rows = screen_rows;

	/* Set to 25 screen rows */
	Term_rows(FALSE);

	/* Verify mode XXX XXX */
	mode = (mode % 4);

	/* Erase screen */
	clear_from(0);

	/* Standard */
	if ((mode == 0) || (mode == 1))
	{
		char buf[80];

		cptr title = get_title(99, FALSE);
		bool quotes = FALSE;

		/* Note that title is meant to be in quotes, skip past marker */
		if (title[0] == '#')
		{
			quotes = TRUE;
			title++;
		}

		/* Build a name and title string */
		sprintf(buf, "%s\n %s%s%c",
		        (!op_ptr->full_name ? "Anonymous" : op_ptr->full_name),
		        (quotes ? "\"" : "the "), title,
		        (quotes ? '\"' : '\0'));

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
			char realm_desc[80];

			if (p_ptr->realm == MAGE)   strcpy(realm_desc, "Sorcery");
			if (p_ptr->realm == PRIEST) strcpy(realm_desc, "Piety");
			if (p_ptr->realm == DRUID)  strcpy(realm_desc, "Nature Magic");
			if (p_ptr->realm == NECRO)  strcpy(realm_desc, "Necromancy");

			put_str("Realm  :", 7, 1);
			c_put_str(realm_color(), format("%.15s", realm_desc), 7, 10);
		}


		/* Age, Height, Weight */
		prt_num("Age    :         ", (int)p_ptr->age, 2, 27, TERM_L_BLUE);

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

		/* Check for slain opponent (allow OOD non-uniques) */
		i = slain_opponent(FALSE);

		/* Character has slain an opponent */
		if (i)
		{
			monster_race *r_ptr = &r_info[i];

			/* Get monster race name */
			cptr name = (r_name + r_ptr->name);

			/* Note the kill */
			move_cursor(6, 0);
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				c_roff_centered(TERM_L_BLUE,
					format("Slayer of\n%s", name),
					26, 51);
			}
			else
			{
				c_roff_centered(TERM_L_BLUE,
					format("Slayer of a level %d\n%s", r_ptr->level, name),
					26, 51);
			}
		}

		/* Those without personal fame prestige on family ties */
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
				cnv_stat(value, buf);

				/* Display the current stat (modified) */
				c_put_str(TERM_YELLOW, buf, 2 + i, 61);

				/* Acquire the max stat */
				value = p_ptr->stat_top[i];

				/* Obtain the maximum stat (modified) */
				cnv_stat(value, buf);

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
				cnv_stat(p_ptr->stat_use[i], buf);

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
			put_str("(Character Background)", 17, 28);

			/* History */
			roff("\n", 0, 0);
			roff(p_ptr->history, 7, 73);
		}

		/* Display "various" info */
		else
		{
			put_str("(Character Abilities)", 17, 28);

			display_player_various();
		}
	}

	/* Special */
	else if (mode == 2)
	{
		/* See "http://www.cs.berkeley.edu/~davidb/angband.html" */

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

	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
	}
}




/*
 * Dump a txtual description of the cheat and difficulty options.
 */
static bool file_character_options(FILE *fff)
{
	cptr desc, desc2;

	/* Assume that nothing is printed in this section */
	bool flag = FALSE;


	/* Peeking into various things */
	if (cheat_peek || cheat_hear || cheat_room)
	{
		fprintf(fff, "You are peeking into");
		if (cheat_peek)
		{
			fprintf(fff, " object creation");
			if ((cheat_hear) && (cheat_room)) fprintf(fff, ",");
			else if ((cheat_hear) || (cheat_room)) fprintf(fff, " and");
		}
		if (cheat_hear)
		{
			fprintf(fff, " monster creation");
			if (cheat_room)
			{
				if (cheat_peek) fprintf(fff, ",");
				fprintf(fff, " and");
			}
		}
		if (cheat_room)
		{
			fprintf(fff, " dungeon creation");
		}
		fprintf(fff, ".");

		if ((cheat_know) || (p_ptr->noscore & (0x0001))) fprintf(fff, "\n");
		else fprintf(fff, "\n\n");

		flag = TRUE;
	}

	/* Know all about monsters */
	if (cheat_know)
	{
		fprintf(fff, "You know everything about monsters.");
		if (p_ptr->noscore & (0x0001)) fprintf(fff, "  ");
		else fprintf(fff, "\n\n");

		flag = TRUE;
	}

	/* Cheated death */
	if (p_ptr->noscore & (0x0001))
	{
		fprintf(fff, "You have cheated death %d times.\n\n", p_ptr->age);

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
	if (birth_ironman)
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
	int i, x, y, x2, y2;

	byte a;
	char c;

	int fd;

	FILE *fff = NULL;

	store_type *st_ptr = &store[STORE_HOME];

	char o_name[80];

	char buf[1024];

	int speed;


	/* Unused parameter */
	(void)full;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, name);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Check for existing file */
	fd = fd_open(buf, O_RDONLY);

	/* Existing file */
	if (fd >= 0)
	{
		char out_val[160];

		/* Close the file */
		fd_close(fd);

		/* Build query */
		sprintf(out_val, "Replace existing file %s?", buf);

		/* Ask */
		if (get_check(out_val)) fd = -1;
	}

	/* Open the non-existing file */
	if (fd < 0) fff = my_fopen(buf, "w");


	/* Invalid file */
	if (!fff) return (-1);


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
	display_player(0);

	/* Dump part of the screen */
	for (y = 2; y < 22; y++)
	{
		/* Dump each row */
		for (x = 1, x2 = 0; x < 78; x++)
		{
			/* Hack -- skip some columns */
			if ((x == 25) || (x == 51)) continue;

			/* Hack -- skip some more columns */
			if ((y >= 18) && ((x == 10) || (x == 11))) continue;
			if ((y <=  7) && ((x == 57) || (x == 58))) continue;
			if ((y >= 9) && (y <= 15) && ((x == 69) || (x == 70)))
				continue;


			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x2++] = c;
		}

		/* Back up over spaces */
		while ((x2 > 0) && (buf[x2-1] == ' ')) --x2;

		/* Terminate */
		buf[x2] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}
	fprintf(fff, "\n");

	/* Get speed */
	speed = p_ptr->pspeed;

	/* Hack -- undo the sneaking slowdown */
	if (p_ptr->sneaking) speed += 5;

	/* Fast or slow */
	if (speed != 110) sprintf(buf, "%+d", (speed - 110));
	else              strcpy(buf, "normal");

	/* Display various things */
	fprintf(fff, "   Speed       : %s\n", buf);
	fprintf(fff, "   Armour      : %d\n", p_ptr->dis_ac + p_ptr->dis_to_a);
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
	display_player(2);

	/* Dump art of the screen -- group #1 */
	for (y = 1; y < 9; y++)
	{
		/* Dump each row */
		for (x = 0, x2 = 0; x < 60; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x2++] = c;

			/* Hack -- insert some blank space for neatness */
			if (x == 19)
			{
				buf[x2] = '\0';
				strcat(buf, "       ");
				x2 += 7;
			}
		}

		/* Back up over spaces */
		while ((x2 > 0) && (buf[x2-1] == ' ')) --x2;

		/* Terminate */
		buf[x2] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Dump art of the screen -- group #2 */
	for (y2 = 0; y2 < 9; y2++)
	{
		/* Start of string */
		x2 = 0;

		/* Reset screen line */
		y = y2 + 1;

		/* Grab the first set */
		for (y = y2 + 1, x = 60; x < 80; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x2++] = c;
		}

		/* Insert some blank space for neatness */
		buf[x2] = '\0';
		strcat(buf, "                    ");
		x2 += 20;

		/* Grab the second set */
		for (y = y2 + 13, x = 60; x < 80; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x2++] = c;
		}

		/* Back up over spaces */
		while ((x2 > 0) && (buf[x2-1] == ' ')) --x2;

		/* Terminate */
		buf[x2] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}
	fprintf(fff, "\n");

	/* Dump art of the screen -- group #3 */
	for (y = 13; y < 22; y++)
	{
		/* Dump each row */
		for (x = 0; x < 60; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Back up over spaces */
		while ((x > 0) && (buf[x-1] == ' ')) --x;

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}


	/* Skip two lines */
	fprintf(fff, "\n\n");


	/* If dead, dump last messages -- Prfnoff */
	if (p_ptr->is_dead)
	{
		i = message_num();
		if (i > 15) i = 15;
		fprintf(fff, "  [Last Messages]\n\n");
		while (i-- > 0)
		{
			fprintf(fff, "> %s\n", message_str((s16b)i));
		}
		fprintf(fff, "\n\n");
	}


	/* Skills header */
	fprintf(fff, "  [Skills]\n\n");

	/* Print the active skills */
	for (i = 0; i < NUM_SK_USED; i++)
	{
		/* Nothing's been invested in this skill */
		if (!p_ptr->pskills[i].max) continue;

		/* Build a skill listing */
		sprintf(buf, "%-18s: %3d%%", skill_info[i].name,
			p_ptr->pskills[i].cur);

		/* Note drained skills */
		if (p_ptr->pskills[i].cur != p_ptr->pskills[i].max)
		{
			strcat(buf, format("    (max %d%%)", p_ptr->pskills[i].max));
		}

		fprintf(fff, "%s\n", buf);
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
	}

	fprintf(fff, "\n\n");


	/* Dump the equipment */
	if (p_ptr->equip_cnt)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			if (!inventory[i].k_idx) continue;
			object_desc(o_name, &inventory[i], TRUE, 3);
			o_name[72] = '\0';
			fprintf(fff, "%s\n", o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	fprintf(fff, "  [Character Inventory]\n\n");
	for (i = 0; i < INVEN_PACK; i++)
	{
		if (!inventory[i].k_idx) break;

		object_desc(o_name, &inventory[i], TRUE, 3);
		o_name[72] = '\0';

		fprintf(fff, "%s\n", o_name);
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
			object_desc(o_name, &st_ptr->stock[i], TRUE, 3);
			o_name[72] = '\0';

			fprintf(fff, "%s\n", o_name);
		}

		/* Add two empty lines */
		fprintf(fff, "\n\n");
	}

	/* Describe the cheating and difficulty options */
	if (!p_ptr->total_winner)
	{
		fprintf(fff, "  [Special advantages and disadvantages]\n");
		if (!file_character_options(fff))
		{
			fprintf(fff, "(none)\n");
		}
	}

	/* Skip a line */
	fprintf(fff, "\n");

	/* Close the file */
	my_fclose(fff);

	/* Success */
	return (0);
}


/*
 * Get a random line from a file.  Taken from Zangband.  What a good idea!
 */
errr get_rnd_line(const char *file_name, char *output)
{
	FILE	    *fp;
	char	buf[1024];
	int lines, line, counter;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, file_name);

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
	my_fclose(fp);

	return (0);
}


/*
 * Make a string lower case.
 */
static void string_lower(char *buf)
{
	char *s;

	/* Lowercase the string */
	for (s = buf; *s != 0; s++) *s = tolower((unsigned char)*s);
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
 * Consider using a temporary file, in which special lines do not appear,
 * and which could be pre-padded to 80 characters per line, to allow the
 * use of perfect seeking.  XXX XXX XXX
 *
 * Allow the user to "save" the current file.  XXX XXX XXX
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

	/* Current help file */
	FILE *fff = NULL;

	/* Find this string (if any) */
	char *find = NULL;

	/* Jump to this tag */
	cptr tag = NULL;

	/* Hold a string to find */
	char finder[81];

	/* Hold a string to show */
	char shower[81];

	/* Filename */
	char filename[1024];

	/* Describe this thing */
	char caption[128];

	/* Path buffer */
	char path[1024];

	/* General buffer */
	char buf[1024];

	/* Lower case version of the buffer, for searching */
	char lc_buf[1024];

	/* Sub-menu information */
	char hook[10][32];

	int wid, hgt;


	/* Wipe finder */
	strcpy(finder, "");

	/* Wipe shower */
	strcpy(shower, "");

	/* Wipe caption */
	strcpy(caption, "");

	/* Wipe the hooks */
	for (i = 0; i < 10; i++) hook[i][0] = '\0';

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* We may not be using all the possible rows */
	hgt = screen_rows;

	/* Copy the filename */
	my_strcpy(filename, name, sizeof(filename));

	n = strlen(filename);

	/* Extract the tag from the filename */
	for (i = 0; i < n; i++)
	{
		if (filename[i] == '#')
		{
			filename[i] = '\0';
			tag = filename + i + 1;

			/* Search for a string */
			if (mode == 1)
			{
				strcpy(finder, tag);
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
		my_strcpy(caption, what, sizeof(caption));

		/* Get the filename */
		my_strcpy(path, name, sizeof(path));

		/* Open */
		fff = my_fopen(path, "r");
	}

	/* Look in "help" */
	if (!fff)
	{
		/* Caption */
		strnfmt(caption, sizeof(caption), "Help file '%s'", name);

		/* Build the filename */
		path_build(path, sizeof(buf), ANGBAND_DIR_HELP, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Look in "info" */
	if (!fff)
	{
		/* Caption */
		strnfmt(caption, sizeof(caption), "Info file '%s'", name);

		/* Build the filename */
		path_build(path, sizeof(buf), ANGBAND_DIR_INFO, name);

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
				my_strcpy(hook[k], buf + 10, sizeof(hook[0]));
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
		Term_clear();


		/* Restart when necessary */
		if (line >= size) line = 0;


		/* Re-open the file if needed */
		if (next > line)
		{
			/* Close it */
			my_fclose(fff);

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
			my_strcpy(lc_buf, buf, sizeof(lc_buf));

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
			Term_putstr(0, i+2, -1, attr, buf);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = lc_buf;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str-lc_buf, i+2, len, TERM_YELLOW, &buf[str-lc_buf]);

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


		/* Hack -- simple files */
		if (mode == 2)
		{
			if (size <= hgt - 4)
			{
				prt("[Press ESC to exit.]", hgt - 1, 0);
			}
			else
			{
				prt("[Press Space to advance, or ESC to exit.]", hgt - 1, 0);
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
			prt("[Press Space to advance, ESC to return to the previous file, or ? to exit.]", hgt - 1, 0);
		}

		/* Get a keypress */
		ch = inkey();

		/* Hack -- return to last screen on escape */
		if (ch == ESCAPE) break;

		/* Toggle case sensitive on/off */
		if (ch == '!')
		{
			case_sensitive = !case_sensitive;
		}

		/* Try showing */
		if (ch == '&')
		{
			/* Get "shower" */
			prt("Show: ", hgt - 1, 0);
			(void)askfor_aux(shower, 80);

			/* Make the "shower" lowercase */
			if (!case_sensitive) string_lower(shower);
		}

		/* Try finding */
		if (ch == '/')
		{
			/* Get "finder" */
			prt("Find: ", hgt - 1, 0);
			if (askfor_aux(finder, 80))
			{
				/* Find it */
				find = finder;
				back = line;
				line = line + 1;

				/* Make the "finder" lowercase */
				if (!case_sensitive) string_lower(finder);

				/* Show it */
				my_strcpy(shower, finder, sizeof(shower));
			}
		}

		/* Go to a specific line */
		if (ch == '#')
		{
			char tmp[80];
			prt("Goto Line: ", hgt - 1, 0);
			strcpy(tmp, "0");
			if (askfor_aux(tmp, 80))
			{
				line = atoi(tmp);
			}
		}

		/* Go to a specific file */
		if (ch == '%')
		{
			char ftmp[80];
			prt("Goto File: ", hgt - 1, 0);
			strcpy(ftmp, "help.hlp");
			if (askfor_aux(ftmp, 80))
			{
				if (!show_file(ftmp, NULL, 0, mode)) ch = ESCAPE;
			}
		}

		/* Back up one line */
		if (ch == '=')
		{
			line = line - 1;
			if (line < 0) line = 0;
		}

		/* Back up one half page */
		if (ch == '_')
		{
			line = line - ((hgt - 4) / 2);
			if (line < 0) line = 0;
		}

		/* Back up one full page */
		if (ch == '-')
		{
			line = line - (hgt - 4);
			if (line < 0) line = 0;
		}

		/* Advance one line */
		if ((ch == '\n') || (ch == '\r'))
		{
			line = line + 1;
		}

		/* Advance one half page */
		if (ch == '+')
		{
			line = line + ((hgt - 4) / 2);
			if (line < 0) line = 0;
		}

		/* Advance one full page */
		if (ch == ' ')
		{
			line = line + (hgt - 4);
		}

		/* Recurse on numbers */
		if (menu && isdigit((unsigned char)ch) && hook[D2I(ch)][0])
		{
			/* Recurse on that file */
			if (!show_file(hook[D2I(ch)], NULL, 0, mode)) ch = ESCAPE;
		}

		/* Exit on '?' */
		if (ch == '?') break;
	}

	/* Close the file */
	my_fclose(fff);

	/* Exit on '?' */
	if (ch == '?') return (FALSE);

	/* Normal return */
	return (TRUE);
}


/*
 * Peruse help files.  We allow both general browsing of help files and
 * instant access to specific sections.  -LM-
 *
 * When specifying search strings, make sure that they are lowercase.
 */
void do_cmd_help(void)
{
	int old_rows = screen_rows;


	/* Save screen */
	screen_save();


	/* We want 50 rows */
	if (text_50_rows) Term_rows(TRUE);

	/* We don't */
	else Term_rows(FALSE);


	/* Show appropriate help file and section */
	switch (p_ptr->get_help_index)
	{
		case HELP_CMD_REVIEW:
		{
			if (rogue_like_commands)
			{
				show_file("cmdlist.txt#roguelike keyset", NULL, 0, 1);
			}
			else
			{
				show_file("cmdlist.txt#original keyset", NULL, 0, 1);
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
			show_file("cmddesc.txt#targeting:", NULL, 0, 1);
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
			char buf[80];
			sprintf(buf, "skills.txt#%s  ",
				skill_info[p_ptr->get_help_index - HELP_SKILLS].name);
			show_file(strlower(buf), NULL, 0, 1);
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
			show_file("talents.txt#infusion of forged items:", NULL, 0, 1);
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


	/* Set to 50 screen rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
	}

	/* Set to 25 rows, if we were showing 25 before */
	else
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(FALSE);
	}


	/* Load screen */
	screen_load();
}


/*
 * Display a text file on screen.
 *
 * This function is used to display the "news", "dead", and "victory"
 * files.  It is generally suitable for any file display that does not
 * require user input.  If the file has the correct textual markers, we
 * can display color and switch between 25 and 50-line modes.  It will
 * be easy to extend this code to handle other tasks.
 *
 * After calling this function, the screen may be in either 25 or 50
 * line mode.
 */
bool display_file(FILE *fp)
{
	cptr s;

	char buf[1024];

	int y, x;

	/* Array of char/attr pairs - assume a 50-line screen */
	byte char_attr_table[50][81][2];

	/* Assume we're reading character, not color information */
	bool char_mode = TRUE;
	bool attr_mode = FALSE;

	/* Clear table  (XXX XXX - Brute force) */
	for (y = 0; y < 50; y++)
	{
		for (x = 0; x < 81; x++)
		{
			char_attr_table[y][x][0] = ' ';
			char_attr_table[y][x][1] = TERM_WHITE;
		}
	}

	/* Read the file */
	if (fp)
	{
		/* Start at the beginning */
		x = 0;
		y = 0;

		/* Read file, store data */
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

				/* The file wants 50 screen rows */
				if (strstr(s, "rows50"))
				{
					/* Set to 50 screen rows */
					Term_rows(TRUE);
					continue;
				}

				/* The file wants 25 screen rows */
				if (strstr(s, "rows25"))
				{
					/* Set to 25 screen rows */
					Term_rows(FALSE);
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
					/* Switch to attr mode */
					char_mode = TRUE;
					attr_mode = FALSE;

					/* Start at beginning of table */
					y = 0;
					continue;
				}
			}

			/* We're reading characters as text */
			if (char_mode)
			{
				/* Read in this line */
				for (x = 0, s = buf; *s && x < 80; x++)
					char_attr_table[y][x][0] = *s++;

				/* End of line marker */
				char_attr_table[y][++x][0] = '\0';
			}

			/* We're reading color information */
			else if (attr_mode)
			{
				int attr;

				/* Read in this line */
				for (x = 0, s = buf; *s && x < 80; x++, s++)
				{
					/* Get color corresponding to this character */
					if (*s == ' ') attr = TERM_WHITE;
					else
					{
						attr = color_char_to_attr(*s);
						if (attr == -1) attr = TERM_WHITE;
					}

					/* Store the color */
					char_attr_table[y][x][1] = attr;
				}
			}

			/* Go to next row, but stay legal */
			if (++y > 49) break;
		}

		/* Dump the file to screen */
		for (y = 0; y < MIN(50, screen_rows); y++)
		{
			for (x = 0; x < 80; x++)
			{
				byte c = char_attr_table[y][x][0];

				if (c == '\0') break;

				(void)Term_putch(x, y, char_attr_table[y][x][1], c);
			}
		}

		/* Flush it */
		Term_fresh();

		/* Close */
		my_fclose(fp);

		/* Note success */
		return (TRUE);
	}

	/* Note failure */
	return (FALSE);
}


/*
 * Process the player name and extract a clean "base name".
 *
 * If "sf" is TRUE, we initialize "savefile" based on base player name.
 * This should only be done if SAVEFILE_MUTABLE is defined, or the player
 * deliberately chooses to do this.
 */
void process_player_name(bool sf)
{
	int i;

	/* Cannot be too long */
	if (strlen(op_ptr->full_name) > 30)
	{
		/* Truncate it */
		op_ptr->full_name[30] = '\0';
	}

	/* Process the player name */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		char c = op_ptr->full_name[i];

		/* No control characters */
		if (iscntrl((unsigned char)c))
		{
			/* Correct it */
			op_ptr->full_name[i] = ' ';
		}

/* Some operating systems require short filenames */
#if defined(WINDOWS) || defined(MSDOS)
		/* If first word is at least four chars long, use it as the base name */
		if ((i >= 3) && (c == ' ')) break;

		/* Must never be longer than eight chars */
		if (i == 8) break;
#endif

/* Convert various characters */
#ifdef MACINTOSH
		/* Convert "dot" to "underscore" */
		if (c == '.') c = '_';
#else
		/* Convert all non-alphanumeric symbols */
		if (!isalpha((unsigned char)c) && !isdigit((unsigned char)c)) c = '_';
#endif

		/* Build "base_name" */
		op_ptr->base_name[i] = c;
	}

	/* Terminate */
	op_ptr->base_name[i] = '\0';

	/* Require a "base" name */
	if (!op_ptr->base_name[0])
	{
		strcpy(op_ptr->base_name, "PLAYER");
	}


/* Allow savefile names to change when the character name does. */
#ifdef SAVEFILE_MUTABLE

	/* Accept */
	sf = TRUE;

#endif


	/* Pick savefile name if needed */
	if (sf)
	{
		char temp[128];

#ifdef SAVEFILE_USE_UID
		/* Rename the savefile, using the player_uid and base_name */
		strnfmt(temp, sizeof(temp), "%d.%s", player_uid, op_ptr->base_name);
#else
		/* Rename the savefile, using the base name */
		strnfmt(temp, sizeof(temp), "%s", op_ptr->base_name);
#endif

#ifdef VM
		/* Hack -- support "flat directory" usage on VM/ESA */
		strnfmt(temp, sizeof(temp), "%s.sv", op_ptr->base_name);
#endif /* VM */

		/* Build the filename */
		path_build(savefile, sizeof(savefile), ANGBAND_DIR_SAVE, temp);
	}
}


/*
 * Gets a name for the character, reacting to name changes.
 *
 * Perhaps we should NOT ask for a name (at "birth()") on
 * UNIX machines?  XXX XXX XXX
 *
 * What a horrible name for a global function.  XXX XXX XXX
 */
void get_name(void)
{
	char tmp[32];

	/* Save the player name */
	my_strcpy(tmp, op_ptr->full_name, sizeof(tmp));

	/* Prompt for a new name */
	if (get_string("Enter a name for your character:", tmp, 25))
	{
		/* Use the name */
		my_strcpy(op_ptr->full_name, tmp, sizeof(op_ptr->full_name));

		/* Process the player name */
		process_player_name(FALSE);
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
		ch = inkey();
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
	if (!is_autosave) prt("Saving game...", 0, 0);
	else              prt("Autosaving the game...", 0, 0);

	/* Refresh */
	Term_fresh();

	/* The player is not dead */
	strcpy(p_ptr->died_from, "(saved)");

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Hack -- temporarily advance the turn count  XXX */
	if (is_autosave) turn++;

	/* Save the player */
	if (save_player())
	{
		if (!is_autosave) prt("Saving game... done.", 0, 0);
		else              prt("Autosaving the game... done.", 0, 0);
	}

	/* Save failed (oops) */
	else
	{
		if (!is_autosave) prt("Saving game...  Failed!", 0, 0);
		else              prt("Autosaving the game...  Failed!", 0, 0);
	}

	/* Hack -- restore the turn count  XXX */
	if (is_autosave) turn--;

	/* Allow suspend again */
	signals_handle_tstp();

	/* Refresh */
	Term_fresh();

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
	int level;
	s32b k;
	s32b score = 0L;
	s32b divisor;

	int r_idx;


	/* Calculate the skill divisor */
	for (divisor = 50L, i = 0; i < NUM_SK_USED; i++)
	{
		/* Skip nonexistent skills */
		if (skill_info[i].cost_adj == 0) continue;

		/* Use maximal skills */
		level = p_ptr->pskills[i].max;

		/* Sum up all the skills, adjusted for inherent difficulty */
		divisor += (long)(level * skill_info[i].cost_adj);
	}

	/* Get the highest-level slain dungeon unique */
	r_idx = slain_opponent(TRUE);

	/* The big points come from defeating uniques */
	if (r_idx)
	{
		/* Big scores for deep uniques */
		score = 250L * (s32b)r_info[r_idx].level * (s32b)r_info[r_idx].level;

		/* Bonus for the heads of Sauron and Morgoth */
		if      (r_idx == MON_MORGOTH) score += score;
		else if (r_idx == MON_SAURON)  score += score / 2;
	}

	/* Get some points for killing non-uniques */
	else
	{
		/* Check for slain opponent (allow OOD non-uniques) */
		r_idx = slain_opponent(FALSE);

		/* Character has slain a worthy opponent -- grant some points */
		if (r_idx) score = 100L * (s32b)r_info[r_idx].level *
		                   (s32b)r_info[r_idx].level;
	}

	/* Check gold too */
	k = ((p_ptr->au - p_ptr->au_birth) - 500L) / 10L;
	if (k > 250L) k = 250L;

	/* Wealth earns (a few) points if great enough */
	if (k > score) score = k;

	/* Divide score by divisor */
	score /= divisor;

	/* Reward/Penalize for difficulty options  -clefs- */
	k = score;

	/* +40% for ironman */
	if (birth_ironman)          score += k * 4 / 10;

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

	/* Ignore wizards and borgs */
	if (!(p_ptr->noscore & 0x00FF))
	{
		/* Ignore people who die in town */
		if (p_ptr->depth)
		{
			int level;
			char tmp[128];

			/* Slightly more tenacious saving routine. */
			for (i = 0; i < 5; i++)
			{
				/* Ghost hovers near level of death. */
				if (i == 0) level = p_ptr->depth;
				else level = p_ptr->depth + 5 - damroll(2, 4);
				if (level < 1) level = randint(4);

				/* XXX XXX XXX "Bones" name */
				sprintf(tmp, "bone.%03d", level);

				/* Build the filename */
				path_build(str, sizeof(str), ANGBAND_DIR_BONE, tmp);

				/* Attempt to open the bones file */
				fp = my_fopen(str, "r");

				/* Close it right away */
				if (fp) my_fclose(fp);

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

			fprintf(fp, "%d\n", p_ptr->psex);
			fprintf(fp, "%d\n", p_ptr->prace);
			fprintf(fp, "%d\n", p_ptr->realm);

			/* Close and save the Bones file */
			my_fclose(fp);
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

	char tmp[160];

	char buf[1024];

	FILE *fp;


	/*** Display the "dead" file ***/

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "dead.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Display the file */
	(void)display_file(fp);

	/* Name */
	move_cursor(6, 0);
	c_roff_centered(TERM_WHITE, format("%s\n", op_ptr->full_name), 22, 51);


	/* King or Queen */
	if (p_ptr->total_winner)
	{
		p = "the Magnificent";
	}

	/* Normal */
	else
	{
		char buf2[80];

		cptr title = get_title(31, FALSE);
		bool quotes = FALSE;

		/* Note that title is meant to be in quotes, skip past marker */
		if (title[0] == '#')
		{
			quotes = TRUE;
			title++;
		}

		/* Build a title string */
		if (quotes) sprintf(buf2, "\n%c%s%c", '\"', title, '\"');
		else        sprintf(buf2, "the\n%s", title);

		/* Point to the formatted title */
		p = buf2;
	}

	/* Title */
	move_cursor(7, 0);
	c_roff_centered(TERM_WHITE, format("%s\n\n", p), 20, 53);

	/* Score */
	c_roff_centered(TERM_L_BLUE,
		format("Score: %ld\n\n", total_points()), 20, 53);

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
		format("%s\n", tmp), 20, 53);

	/* Killer */
	sprintf(tmp, "%s%s.\n\n\n",
		(p_ptr->died_from[0] == '(' ? "" : "by "), p_ptr->died_from);
	c_roff_centered(TERM_WHITE, tmp, 20, 53);

	/* Get time of death */
	(void)time(&death_time);

	/* Time */
	c_roff_centered(TERM_WHITE, format("%-.24s", ctime(&death_time)), 20, 53);
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

	object_type *o_ptr;

	store_type *st_ptr = &store[STORE_HOME];


	/* Display player */
	display_player(0);

	/* Prompt for inventory */
	prt("Hit any key to see more information (ESC to abort): ", 23, 0);

	/* Allow abort at this point */
	if (inkey() == ESCAPE) return;


	/* Show equipment and inventory */

	/* Equipment -- if any */
	if (p_ptr->equip_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_equip();
		prt("You are using: -more-", 0, 0);
		if (inkey() == ESCAPE) return;
	}

	/* Inventory -- if any */
	if (p_ptr->inven_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_inven();
		prt("You are carrying: -more-", 0, 0);
		if (inkey() == ESCAPE) return;
	}



	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Display contents of the home */
		for (k = 0, i = 0; i < st_ptr->stock_num; k++)
		{
			/* Clear screen */
			Term_clear();

			/* Show 12 items */
			for (j = 0; (j < 12) && (i < st_ptr->stock_num); j++, i++)
			{
				byte attr;

				char o_name[80];
				char tmp_val[80];

				/* Get the object */
				o_ptr = &st_ptr->stock[i];

				/* Print header, clear line */
				sprintf(tmp_val, "%c) ", I2A(j));
				prt(tmp_val, j+2, 4);

				/* Get the object description */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Get the inventory color */
				attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

				/* Display the object */
				c_put_str(attr, o_name, j + 2, 7);
			}

			/* Caption */
			prt(format("Your home contains (page %d): -more-", k+1), 0, 0);

			/* Wait for it */
			if (inkey() == ESCAPE) return;
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

	char o_name[80];

	cptr q, s;


	/* Start out in "display" mode */
	p_ptr->command_see = TRUE;

	while (TRUE)
	{
		/* Get an item */
		q = "Examine which item?";
		s = "You have nothing to examine.";
		if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;
		item_to_object(o_ptr, item);


		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);

		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Describe */
		msg_format("Examining %s...", o_name);

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
		if (highscore_read(&the_score)) return (i);
		if (strcmp(the_score.pts, score->pts) < 0) return (i);
		if (strcmp(the_score.max_dun, score->max_dun) < 0) return (i);
	}

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
 * Display the scores in a given range.
 * Assumes the high score list is already open.
 * Only five entries per line, too much info.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
static void display_scores_aux(int from, int to, int note, high_score *score)
{
	char ch;

	int j, k, n, place;
	int count;

	high_score the_score;

	char out_val[160];
	char tmp_val[160];

	byte attr;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return;


	/* Assume we will show the first 10 */
	if (from < 0) from = 0;
	if (to < 0) to = 10;
	if (to > MAX_HISCORES) to = MAX_HISCORES;


	/* Seek to the beginning */
	if (highscore_seek(0)) return;

	/* Hack -- Count the high scores */
	for (count = 0; count < MAX_HISCORES; count++)
	{
		if (highscore_read(&the_score)) break;
	}

	/* Hack -- allow "fake" entry to be last */
	if ((note == count) && score) count++;

	/* Forget about the last entries */
	if (count > to) count = to;


	/* Show 5 per page, until "done" */
	for (k = from, j = from, place = k+1; k < count; k += 5)
	{
		/* Clear screen */
		Term_clear();

		/* Title */
		put_str(format("                %s Hall of Fame", VERSION_NAME),
			0, 0);

		/* Indicate non-top scores */
		if (k > 0)
		{
			sprintf(tmp_val, "(from position %d)", place);
			put_str(tmp_val, 0, 40);
		}

		/* Dump 5 entries */
		for (n = 0; j < count && n < 5; place++, j++, n++)
		{
			int pr, p_mag, cdun, mdun;

			cptr user, gold, when, aged;


			/* Hack -- indicate death in yellow */
			attr = (j == note) ? TERM_YELLOW : TERM_WHITE;


			/* Mega-Hack -- insert a "fake" record */
			if ((note == j) && score)
			{
				the_score = (*score);
				attr = TERM_L_GREEN;
				score = NULL;
				note = -1;
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
			for (user = the_score.uid; isspace(*user); user++) /* loop */;
			for (when = the_score.day; isspace(*when); when++) /* loop */;
			for (gold = the_score.gold; isspace(*gold); gold++) /* loop */;
			for (aged = the_score.turns; isspace(*aged); aged++) /* loop */;

			/* Clean up standard encoded form of "when" */
			if ((*when == '@') && strlen(when) == 9)
			{
				sprintf(tmp_val, "%.4s-%.2s-%.2s",
					when + 1, when + 5, when + 7);
				when = tmp_val;
			}

			/* Dump some info */
			sprintf(out_val, "%3d.%9s  %s the %s %s",
				place, the_score.pts, the_score.who,
				race_info[pr].title, magic_info[p_mag].title);

			/* Dump the first line */
			c_put_str(attr, out_val, n*4 + 2, 0);

			/* Another line of info */
			if (streq(the_score.how, "alive and well"))
			{
				sprintf(out_val, "               Still alive on %s %d",
					"Dungeon Level", cdun);
			}
			else
			{
				sprintf(out_val, "               Killed by %s on %s %d",
					the_score.how, "Dungeon Level", cdun);
			}

			/* Hack -- some people die in the town */
			if (!cdun)
			{
				if (streq(the_score.how, "alive and well"))
				{
					sprintf(out_val, "               Still alive in the Town");
				}
				else
				{
					sprintf(out_val, "               Killed by %s in the Town",
						the_score.how);
				}
			}

			/* Append a "maximum level" */
			if (mdun > cdun) strcat(out_val, format(" (Max %d)", mdun));

			/* Dump the info */
			c_put_str(attr, out_val, n*4 + 3, 0);

			/* And still another line of info */
#ifdef SAVEFILE_USE_UID
			sprintf(out_val,
				"               (User %s, Date %s, Gold %s, Turn %s).",
				user, when, gold, aged);
#else
			sprintf(out_val,
				"               (Date %s, Gold %s, Turn %s).",
				when, gold, aged);
#endif /* SAVEFILE_USE_UID */
			c_put_str(attr, out_val, n*4 + 4, 0);
		}


		/* Wait for response */
		prt("[Press ESC to exit, any other key to continue.]", 23, 17);
		ch = inkey();
		prt("", 23, 0);

		/* Hack -- notice Escape */
		if (ch == ESCAPE) break;
	}
}


/*
 * Hack -- Display the scores in a given range and quit.
 *
 * This function is only called from "main.c" when the user asks
 * to see the "high scores".
 */
void display_scores(int from, int to)
{
	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Clear screen */
	Term_clear();

	/* Set to 25 screen rows  XXX */
	Term_rows(FALSE);

	/* Title */
	put_str(format("                %s Hall of Fame", VERSION_NAME), 0, 0);

	/* Display the scores */
	display_scores_aux(from, to, -1, NULL);

	/* Shut the high score file */
	fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;

	/* Wait for response */
	prt("[Press any key to exit.]", 23, 17);
	(void)inkey();
	prt("", 23, 0);

	/* Quit */
	quit(NULL);
}


/*
 * Hack - save index of player's high score
 */
static int score_idx = -1;


/*
 * Enters a player's name on a hi-score table, if "legal".
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static errr enter_score(void)
{
#ifndef SCORE_CHEATERS
	int j;
#endif /* SCORE_CHEATERS */

	high_score the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
		return (0);
	}

#ifndef SCORE_WIZARDS

	/* Wizard-mode preempts scoring */
	if (p_ptr->noscore & 0x000F)
	{
		msg_print("Score not registered for wizards.");
		message_flush();
		score_idx = -1;
		return (0);
	}

#endif

#ifndef SCORE_BORGS

	/* Borg-mode preempts scoring */
	if (p_ptr->noscore & 0x00F0)
	{
		msg_print("Score not registered for borgs.");
		message_flush();
		score_idx = -1;
		return (0);
	}
#endif /* SCORE_BORGS */

#ifndef SCORE_CHEATERS

	/* Cheaters are not scored */
	for (j = OPT_CHEAT_HEADER; j < OPT_BIRTH; ++j)
	{
		if (!op_ptr->opt[j]) continue;

		msg_print("Score not registered for cheaters.");
		message_flush();
		score_idx = -1;
		return (0);
	}

#endif /* SCORE_CHEATERS */

	/* Hack -- Interrupted */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "(Game interrupted)"))
	{
		msg_print("Score not registered due to interruption.");
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
	strnfmt(the_score.what, sizeof(the_score.what), "%s", VERSION_STRING);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);
	the_score.turns[9] = '\0';

	/* Save the date in standard encoded form (9 chars) */
	strftime(the_score.day, 10, "@%Y%m%d", localtime(&death_time));

	/* Save the player name (30 chars) */
	sprintf(the_score.who, "%-.30s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_mag, "%2d", p_ptr->realm);

	/* Save the level and such */
	sprintf(the_score.cur_dun, "%3d", p_ptr->depth);
	sprintf(the_score.max_dun, "%3d", p_ptr->max_depth);

	/* Save the cause of death (31 chars) */
	sprintf(the_score.how, "%-.31s", p_ptr->died_from);

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
 * Enters a player's name on a hi-score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static void top_twenty(void)
{
	/* Clear screen */
	Term_clear();

	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		message_flush();
		return;
	}

	/* Player's score unavailable */
	if (score_idx == -1)
	{
		display_scores_aux(0, 10, -1, NULL);
		return;
	}

	/* Hack -- Display the top fifteen scores */
	else if (score_idx < 10)
	{
		display_scores_aux(0, 15, score_idx, NULL);
	}

	/* Display the scores surrounding the player */
	else
	{
		display_scores_aux(0, 5, score_idx, NULL);
		display_scores_aux(score_idx - 2, score_idx + 7, score_idx, NULL);
	}


	/* Success */
	return;
}


/*
 * Predict the player's score, and display it.
 */
errr predict_score(void)
{
	int j;

	high_score the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		message_flush();
		return (0);
	}


	/* Save the version */
	strnfmt(the_score.what, sizeof(the_score.what), "%s", VERSION_STRING);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long)total_points());

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);

	/* Hack -- no time needed */
	strcpy(the_score.day, "TODAY");

	/* Save the player name (30 chars) */
	sprintf(the_score.who, "%-.30s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
#ifdef SAVEFILE_USE_UID
	sprintf(the_score.uid, "%7u", player_uid);
#endif /* SAVEFILE_USE_UID */

	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_mag, "%2d", p_ptr->realm);

	/* Save the level and such */
	sprintf(the_score.cur_dun, "%3d", p_ptr->depth);
	sprintf(the_score.max_dun, "%3d", p_ptr->max_depth);

	/* Hack -- no cause of death */
	strcpy(the_score.how, "alive and well");


	/* See where the entry would be placed */
	j = highscore_where(&the_score);


	/* Hack -- Display the top fifteen scores */
	if (j < 10)
	{
		display_scores_aux(0, 15, j, &the_score);
	}

	/* Display some "useful" scores */
	else
	{
		display_scores_aux(0, 5, -1, NULL);
		display_scores_aux(j - 2, j + 7, j, &the_score);
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

	FILE *fp;


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

	/* Clear screen */
	Term_clear();


	/*** Display the "victory" file ***/

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "victory.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Display the file */
	(void)display_file(fp);


	/* Wait for response */
	pause_line(Term->hgt - 1);
}


/*
 * Handle character death
 */
static void close_game_aux(void)
{
	int ch;

	bool wants_to_quit = FALSE;

	/* Prompt */
	cptr p = "[(i)nformation, (m)essages, (f)ile dump, (v)iew scores, e(x)amine item, ESC]";

	/* Dump bones file */
	make_bones();

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

	/* You are dead */
	print_tomb();

	/* Hack - Know everything upon death */
	death_knowledge();

	/* Enter player in high score list */
	enter_score();

	/* Flush all input keys */
	flush();

	/* Flush messages */
	message_flush();

	/* Loop */
	while (!wants_to_quit)
	{
		/* Describe options */
		Term_putstr(1, 23, -1, TERM_WHITE, p);

		/* Query */
		ch = inkey();

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
				char ftmp[80];

				strnfmt(ftmp, sizeof(ftmp), "%s.txt", op_ptr->base_name);

				if (get_string("File name:", ftmp, sizeof(ftmp)))
				{
					if (ftmp[0] && (ftmp[0] != ' '))
					{
						errr err;

						/* Save screen */
						screen_save();

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
				/* Save screen */
				screen_save();

				/* Show the character */
				show_info();

				/* Load screen */
				screen_load();

				break;
			}

			/* Show last messages */
			case 'm':
			case 'M':
			{
				/* Save screen */
				screen_save();

				/* Display messages */
				do_cmd_messages();

				/* Load screen */
				screen_load();

				break;
			}

			/* Show top scores */
			case 'v':
			case 'V':
			{
				/* Save screen */
				screen_save();

				/* Show the scores */
				top_twenty();

				/* Load screen */
				screen_load();

				break;
			}

			/* Examine an item */
			case 'x':
			case 'X':
			{
				/* Save screen */
				screen_save();

				/* Clear the screen */
				Term_clear();

				/* Examine items */
				death_examine();

				/* Load screen */
				screen_load();

				break;
			}
		}
	}
}


/*
 * Close up the current game (player may or may not be dead)
 *
 * Note that the savefile is not saved until the tombstone is
 * actually displayed and the player has a chance to examine
 * the inventory and such.  This allows cheating if the game
 * is equipped with a "quit without save" method.  XXX XXX XXX
 */
void close_game(void)
{
	char buf[1024];


	/* Handle stuff */
	handle_stuff();

	/* Flush the messages */
	message_flush();

	/* Flush the input */
	flush();


	/* No suspending now */
	signals_ignore_tstp();


	/* Hack -- Increase "icky" depth */
	character_icky++;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

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

		/* Prompt for scores XXX XXX XXX */
		prt("Press Return (or Escape).", 0, 40);

		/* Predict score (or ESCAPE) */
		if (inkey() != ESCAPE) predict_score();
	}


	/* Shut the high score file */
	fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;


	/* Hack -- Decrease "icky" depth */
	character_icky--;


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
	Term_fresh();

	/* Suspend the "Term" */
	Term_xtra(TERM_XTRA_ALIVE, 0);

	/* Suspend ourself */
	(void)kill(0, SIGSTOP);

	/* Resume the "Term" */
	Term_xtra(TERM_XTRA_ALIVE, 1);

	/* Redraw the term */
	Term_redraw();

	/* Flush the term */
	Term_fresh();

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
		Term_xtra(TERM_XTRA_NOISE, 0);

		/* Clear the top line */
		clear_row(0);

		/* Display the cause */
		Term_putstr(0, 0, -1, TERM_WHITE, "Contemplating suicide!");

		/* Flush */
		Term_fresh();
	}

	/* Give warning (after 2) */
	else if (signal_count >= 2)
	{
		/* Make a noise */
		Term_xtra(TERM_XTRA_NOISE, 0);
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
	int row = Term->hgt - 1;

	/* Disable handler */
	(void)(*signal_aux)(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	/* Clear the bottom line */
	clear_row(row);

	/* Give a warning */
	Term_putstr(0, row, -1, TERM_RED,
		"A gruesome software bug LEAPS out at you!");

	/* Message */
	Term_putstr(45, row, -1, TERM_RED, "Panic save...");

	/* Flush output */
	Term_fresh();

	/* Panic Save */
	p_ptr->panic_save = 1;

	/* Panic save */
	strcpy(p_ptr->died_from, "(panic save)");

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Attempt to save */
	if (save_player())
	{
		Term_putstr(45, row, -1, TERM_RED, "Panic save succeeded!");
	}

	/* Save failed */
	else
	{
		Term_putstr(45, row, -1, TERM_RED, "Panic save failed!");
	}

	/* Flush output */
	Term_fresh();

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

#ifdef SIGKILL
	(void)(*signal_aux)(SIGKILL, handle_signal_abort);
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
