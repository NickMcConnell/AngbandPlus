/* File: files.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "macro.h"
#include "option.h"
#include "score.h"
#include "store.h"
#include "tvalsval.h"

#define MAX_PANEL 12

#if 0

/*
 * Use this (perhaps) for Angband 2.8.4
 *
 * Extract "tokens" from a buffer
 *
 * This function uses "whitespace" as delimiters, and treats any amount of
 * whitespace as a single delimiter.  We will never return any empty tokens.
 * When given an empty buffer, or a buffer containing only "whitespace", we
 * will return no tokens.  We will never extract more than "num" tokens.
 *
 * By running a token through the "text_to_ascii()" function, you can allow
 * that token to include (encoded) whitespace, using "\s" to encode spaces.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 */
static s16b tokenize_whitespace(char *buf, s16b num, char **tokens)
{
	int k = 0;

	char *s = buf;


	/* Process */
	while (k < num)
	{
		char *t;

		/* Skip leading whitespace */
		for ( ; *s && isspace((unsigned char)*s); ++s) /* loop */;

		/* All done */
		if (!*s) break;

		/* Find next whitespace, if any */
		for (t = s; *t && !isspace((unsigned char)*t); ++t) /* loop */;

		/* Nuke and advance (if necessary) */
		if (*t) *t++ = '\0';

		/* Save the token */
		tokens[k++] = s;

		/* Advance */
		s = t;
	}

	/* Count */
	return (k);
}

#endif


/*
 * Extract the first few "tokens" from a buffer
 *
 * This function uses "colon" and "slash" as the delimeter characters.
 *
 * We never extract more than "num" tokens.  The "last" token may include
 * "delimeter" characters, allowing the buffer to include a "string" token.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 *
 * Hack -- Attempt to handle the 'c' character formalism
 *
 * Hack -- An empty buffer, or a final delimeter, yields an "empty" token.
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
 * slashes as delimeters, while still allowing final tokens which
 * may contain any characters including "delimiters".
 *
 * Note the use of "strtol()" to allow all "integers" to be encoded
 * in decimal, hexidecimal, or octal form.
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
 *
 * Specify the attr/char values for "flavors" by flavors index.
 *   L:<num>:<a>/<c>
 */
errr process_pref_file_command(char *buf)
{
	long i, n1, n2;

	char *zz[16];


	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace((unsigned char)buf[0])) return (0);

	/* Skip comments */
	if (buf[0] == '#') return (0);


	/* Paranoia */
	/* if (strlen(buf) >= 1024) return (1); */


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
			r_ptr = &monster_type::r_info[i];
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
			k_ptr = &object_type::k_info[i];
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
			f_ptr = &feature_type::f_info[i];
			if (n1) f_ptr->x_attr = (byte)n1;
			if (n2) f_ptr->x_char = (char)n2;
			return (0);
		}
	}


	/* Process "L:<num>:<a>/<c>" -- attr/char for flavors */
	else if (buf[0] == 'L')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			flavor_type *flavor_ptr;
			i = strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= (long)z_info->flavor_max)) return (1);
			flavor_ptr = &object_kind::flavor_info[i];
			if (n1) flavor_ptr->x_attr = (byte)n1;
			if (n2) flavor_ptr->x_char = (char)n2;
			return (0);
		}
	}


	/* Process "S:<num>:<a>/<c>" -- attr/char for special things */
	else if (buf[0] == 'S')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			i = strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= (long)N_ELEMENTS(misc_to_attr))) return (1);
			misc_to_attr[i] = (byte)n1;
			misc_to_char[i] = (char)n2;
			return (0);
		}
	}


	/* Process "E:<tv>:<a>" -- attribute for inventory objects */
	else if (buf[0] == 'E')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{
			i = strtol(zz[0], NULL, 0) % 128;
			n1 = strtol(zz[1], NULL, 0);
			if ((i < 0) || (i >= (long)N_ELEMENTS(tval_to_attr))) return (1);
			if (n1) tval_to_attr[i] = (byte)n1;
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
		if (macro_add(tmp, macro_buffer)) return (1);
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
		i = (long)tmp[0];

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
			if ((i < 0) || (i >= MAX_COLORS)) return (1);
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
			int i;
			int num;

			/* Free existing macro triggers and trigger template */
			macro_trigger_free();

			/* Clear template done */
			if (*zz[0] == '\0') return 0;

			/* Count modifier-characters */
			num = strlen(zz[1]);

			/* One modifier-character per modifier */
			if (num + 2 != tok) return 1;

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
			char *buf;
			const char* s;
			char *t;

			if (max_macrotrigger >= MAX_MACRO_TRIGGER)
			{
				msg_print("Too many macro triggers!");
				return 1;
			}

			/* Buffer for the trigger name */
			C_MAKE(buf, strlen(zz[0]) + 1, char);

			/* Simulate strcpy() and skip the '\' escape character */
			s = zz[0];
			t = buf;

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
			FREE(buf);

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

		return 0;
	}

	/* Process "X:<str>" -- turn option off */
	else if (buf[0] == 'X')
	{
		/* Check non-adult options */
		for (i = 0; i < OPT_ADULT; i++)
		{
			if (options[i].text && streq(options[i].text, buf + 2))
			{
				op_ptr->opt[i] = FALSE;
				return (0);
			}
		}

		/* Ignore unknown options */
		return (0);
	}

	/* Process "Y:<str>" -- turn option on */
	else if (buf[0] == 'Y')
	{
		/* Check non-adult options */
		for (i = 0; i < OPT_ADULT; i++)
		{
			if (options[i].text && streq(options[i].text, buf + 2))
			{
				op_ptr->opt[i] = TRUE;
				return (0);
			}
		}

		/* Ignore unknown options */
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
			if ((flag < 0) || (flag >= (int)N_ELEMENTS(window_flag_desc))) return (1);

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
static const char* process_pref_file_expr(char **sp, char *fp)
{
	const char* v;

	char *b;
	char *s = (*sp);	/* Initial */

	char b1 = '[';
	char b2 = ']';

	char f = ' ';

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
			if (streq(b+1, "SYS"))
			{
				v = ANGBAND_SYS;
			}

			/* Graphics */
			else if (streq(b+1, "GRAF"))
			{
				v = ANGBAND_GRAF;
			}

			/* Race */
			else if (streq(b+1, "RACE"))
			{
				v = p_ptr->racename();
			}

			/* Class */
			else if (streq(b+1, "CLASS"))
			{
				v = p_ptr->classname();
			}

			/* Player */
			else if (streq(b+1, "PLAYER"))
			{
				v = op_ptr->base_name;
			}

			/* Game version */
			else if (streq(b+1, "VERSION"))
			{
				v = VERSION_STRING;
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
static errr process_pref_file_aux(const char* name)
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
			const char* v;
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
 * See the functions above for a list of legal "commands".
 *
 * We also accept the special "?" and "%" directives, which
 * allow conditional evaluation and filename inclusion.
 */
errr process_pref_file(const char* name)
{
	char buf[1024];

	errr err = 0;


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
		err = process_pref_file_aux(buf);
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
 * Restict usage (defaults to no restrictions)
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


/*
 * Returns a "rating" of x depending on y, and sets "attr" to the
 * corresponding "attribute".
 */
static const char* likert(int x, int y, byte *attr)
{
	/* Paranoia */
	if (y <= 0) y = 1;

	/* Negative value */
	if (x < 0)
	{
		*attr = TERM_RED;
		return ("Very Bad");
	}

	/* Analyze the value */
	switch ((x / y))
	{
		case 0:
		case 1:
		{
			*attr = TERM_RED;
			return ("Bad");
		}
		case 2:
		{
			*attr = TERM_RED;
			return ("Poor");
		}
		case 3:
		case 4:
		{
			*attr = TERM_YELLOW;
			return ("Fair");
		}
		case 5:
		{
			*attr = TERM_YELLOW;
			return ("Good");
		}
		case 6:
		{
			*attr = TERM_YELLOW;
			return ("Very Good");
		}
		case 7:
		case 8:
		{
			*attr = TERM_L_GREEN;
			return ("Excellent");
		}
		case 9:
		case 10:
		case 11:
		case 12:
		case 13:
		{
			*attr = TERM_L_GREEN;
			return ("Superb");
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			*attr = TERM_L_GREEN;
			return ("Heroic");
		}
		default:
		{
			*attr = TERM_L_GREEN;
			return ("Legendary");
		}
	}
}


/*
 * Obtain the "flags" for the player as if he was an item
 */
void
player_type::flags(u32b* f) const
{
	/* Clear/set racial flags */
	C_COPY(f,rp_ptr->flags,OBJECT_FLAG_STRICT_UB);

	if ((cp_ptr->flags & CF_BRAVERY_30) && (30 <= lev)) f[1] |= (TR2_RES_FEAR);
}


/*
 * Equippy chars
 */
static void display_player_equippy(int y, int x)
{
	int i;

	byte a;
	char c;

	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
	{	/* Object */
		const object_type* const o_ptr = &p_ptr->inventory[i];

		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;

		/* Get attr/char for display */
		a = o_ptr->attr_user();
		c = o_ptr->char_user();

		/* Dump */
		Term_putch(x+i-INVEN_WIELD, y, a, c);
	}
}


/*
 * 'Database' of resistances and abilities to display
 */
#define RES_ROWS 8
struct player_flag_record {
	const char name[7];		/* Name of resistance/ability */
	byte set;				/* Which field this resistance is in { 1 2 3 } */
	u32b res_flag;			/* resistance flag bit */
	u32b im_flag;			/* corresponding immunity bit, if any. */
};
static const struct player_flag_record player_flag_table[RES_ROWS*4] =
{
	{ " Acid",	2, TR2_RES_ACID,	TR2_IM_ACID },
	{ " Elec",	2, TR2_RES_ELEC,	TR2_IM_ELEC },
	{ " Fire",	2, TR2_RES_FIRE,	TR2_IM_FIRE },
	{ " Cold",	2, TR2_RES_COLD,	TR2_IM_COLD },
	{ " Pois",	2, TR2_RES_POIS,	0 },	/* TR2_IM_POIS */
	{ " Fear",	2, TR2_RES_FEAR,	0 },
	{ " Lite",	2, TR2_RES_LITE,	0 },
	{ " Dark",	2, TR2_RES_DARK,	0 },

	{ "Blind",	2, TR2_RES_BLIND,	0 },
	{ "Confu",	2, TR2_RES_CONFU,	0 },
	{ "Sound",	2, TR2_RES_SOUND,	0 },
	{ "Shard",	2, TR2_RES_SHARD,	0 },
	{ "Nexus",	2, TR2_RES_NEXUS,	0 },
	{ "Nethr",	2, TR2_RES_NETHR,	0 },
	{ "Chaos",	2, TR2_RES_CHAOS,	0 },
	{ "Disen",	2, TR2_RES_DISEN,	0 },

	{ "S.Dig",	3, TR3_SLOW_DIGEST,	0 },
	{ "Feath",	3, TR3_FEATHER, 	0 },
	{ "PLite",	3, TR3_LITE, 		0 },
	{ "Regen",	3, TR3_REGEN, 		0 },
	{ "Telep",	3, TR3_TELEPATHY, 	0 },
	{ "Invis",	3, TR3_SEE_INVIS, 	0 },
	{ "FrAct",	3, TR3_FREE_ACT, 	0 },
	{ "HLife",	3, TR3_HOLD_LIFE, 	0 },

	{ "Stea.",	1, TR1_STEALTH,		0 },
	{ "Sear.",	1, TR1_SEARCH,		0 },
	{ "Infra",	1, TR1_INFRA,		0 },
	{ "Tunn.",	1, TR1_TUNNEL,		0 },
	{ "Speed",	1, TR1_SPEED,		0 },
	{ "Blows",	1, TR1_BLOWS,		0 },
	{ "Shots",	1, TR1_SHOTS,		0 },
	{ "Might",	1, TR1_MIGHT,		0 },
};

#define RES_COLS (5 + 2 + INVEN_TOTAL - INVEN_WIELD)
static const region resist_region[] =
{
	{  0*(RES_COLS+1), 11, RES_COLS, RES_ROWS+2 },
	{  1*(RES_COLS+1), 11, RES_COLS, RES_ROWS+2 },
	{  2*(RES_COLS+1), 11, RES_COLS, RES_ROWS+2 },
	{  3*(RES_COLS+1), 11, RES_COLS, RES_ROWS+2 },
};

static void display_resistance_panel(const struct player_flag_record *resists,
									size_t size, const region& bounds) 
{
	size_t i;
	int j;
	int col = bounds.col;
	int row = bounds.row;
	Term_putstr(col, row++, RES_COLS, TERM_WHITE, "      abcdefghijkl@");
	for (i = 0; i < size-3; i++, row++)
	{
		byte name_attr = TERM_WHITE;
		Term_gotoxy(col+6, row);
		/* repeated extraction of flags is inefficient but more natural */
		for (j = INVEN_WIELD; j <= INVEN_TOTAL; j++)
		{
			const object_type* const o_ptr = &p_ptr->inventory[j];
			byte attr = TERM_WHITE | (j % 2) * 8; /* alternating columns */
			u32b f[OBJECT_FLAG_STRICT_UB] = {0, 0, 0};
			bool res, imm;
			char sym;
			if(j < INVEN_TOTAL)
				object_flags_known(o_ptr, f);
			else
				p_ptr->flags(f);

			res = (0 != (f[resists[i].set] & resists[i].res_flag));
			imm = (0 != (f[resists[i].set] & resists[i].im_flag));
			if(imm) name_attr = TERM_GREEN;
			else if(res && name_attr == TERM_WHITE) name_attr = TERM_L_BLUE;
			sym = imm ? '*' : ( res ? '+' : '.' );
			Term_addch(attr, sym);
		}
		Term_putstr(col, row, 6, name_attr, format("%5s:", resists[i].name));
	}
	Term_putstr(col, row++, RES_COLS, TERM_WHITE, "      abcdefghijkl@");
	/* Equippy */
	display_player_equippy(row++, col+6);
}

static void display_player_flag_info(void)
{
	int i;
	for (i = 0; i < 4; i++)
		display_resistance_panel(player_flag_table+(i*RES_ROWS), RES_ROWS+3, resist_region[i]);
}


/*
 * Special display, part 2b
 */
static void display_player_stat_info(void)
{
	int i;

	char buf[80];

	const int row = 3;	/* Row */
	const int col = 42;	/* Column */

	/* Print out the labels for the columns */
	c_put_str(TERM_WHITE, "  Self", row-1, col+5);
	c_put_str(TERM_WHITE, " RB", row-1, col+12);
	c_put_str(TERM_WHITE, " CB", row-1, col+16);
	c_put_str(TERM_WHITE, " EB", row-1, col+20);
	c_put_str(TERM_WHITE, "  Best", row-1, col+24);

	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Reduced */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			/* Use lowercase stat name */
			put_str(stat_names_reduced[i], row+i, col);
		}

		/* Normal */
		else
		{
			/* Assume uppercase stat name */
			put_str(stat_names[i], row+i, col);
		}

		/* Indicate natural maximum */
		if (p_ptr->stat_max[i] == 18+100)
		{
			put_str("!", row+i, col+3);
		}

		/* Internal "natural" maximum value */
		cnv_stat(p_ptr->stat_max[i], buf, sizeof(buf));
		c_put_str(TERM_L_GREEN, buf, row+i, col+5);

		/* Race Bonus */
		strnfmt(buf, sizeof(buf), "%+3d", p_ptr->rp_ptr->r_adj[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, col+12);

		/* Class Bonus */
		strnfmt(buf, sizeof(buf), "%+3d", p_ptr->cp_ptr->c_adj[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, col+16);

		/* Equipment Bonus */
		strnfmt(buf, sizeof(buf), "%+3d", p_ptr->stat_add[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, col+20);

		/* Resulting "modified" maximum value */
		cnv_stat(p_ptr->stat_top[i], buf, sizeof(buf));
		c_put_str(TERM_L_GREEN, buf, row+i, col+24);

		/* Only display stat_use if not maximal */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			cnv_stat(p_ptr->stat_use[i], buf, sizeof(buf));
			c_put_str(TERM_YELLOW, buf, row+i, col+31);
		}
	}
}


/*
 * Special display, part 2c
 *
 * How to print out the modifications and sustains.
 * Positive mods with no sustain will be light green.
 * Positive mods with a sustain will be dark green.
 * Sustains (with no modification) will be a dark green 's'.
 * Negative mods (from a curse) will be red.
 * Huge mods (>9), like from MICoMorgoth, will be a '*'
 * No mod, no sustain, will be a slate '.'
 */
static void display_player_sust_info(void)
{
	int i, row, col, stat;

	u32b f[OBJECT_FLAG_STRICT_UB];
	u32b ignore_f[OBJECT_FLAG_STRICT_UB];

	byte a;
	char c;


	/* Row */
	row = 3;

	/* Column */
	col = 26;

	/* low-level dependencies */
	ZAIBAND_STATIC_ASSERT(TR1_STR==(1<<A_STR));
	ZAIBAND_STATIC_ASSERT(TR1_INT==(1<<A_INT));
	ZAIBAND_STATIC_ASSERT(TR1_WIS==(1<<A_WIS));
	ZAIBAND_STATIC_ASSERT(TR1_DEX==(1<<A_DEX));
	ZAIBAND_STATIC_ASSERT(TR1_CON==(1<<A_CON));
	ZAIBAND_STATIC_ASSERT(TR1_CHR==(1<<A_CHR));

	ZAIBAND_STATIC_ASSERT(TR2_SUST_STR==(1<<A_STR));
	ZAIBAND_STATIC_ASSERT(TR2_SUST_INT==(1<<A_INT));
	ZAIBAND_STATIC_ASSERT(TR2_SUST_WIS==(1<<A_WIS));
	ZAIBAND_STATIC_ASSERT(TR2_SUST_DEX==(1<<A_DEX));
	ZAIBAND_STATIC_ASSERT(TR2_SUST_CON==(1<<A_CON));
	ZAIBAND_STATIC_ASSERT(TR2_SUST_CHR==(1<<A_CHR));

	/* Header */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col);

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
	{
		/* Get the object */
		const object_type* const o_ptr = &p_ptr->inventory[i];

		/* Get the "known" flags */
		object_flags_known(o_ptr, f);

		/* Hack -- assume stat modifiers are known */
		object_flags(o_ptr, ignore_f);
		f[0] = ignore_f[0];

		/* Initialize color based of sign of pval. */
		for (stat = 0; stat < A_MAX; stat++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';

			/* Boost */
			if (f[0] & (1<<stat))
			{
				/* Default */
				c = '*';

				/* Good */
				if (o_ptr->pval > 0)
				{
					/* Good */
					a = TERM_L_GREEN;

					/* Label boost */
					if (o_ptr->pval < 10) c = I2D(o_ptr->pval);
				}

				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Bad */
					a = TERM_RED;

					/* Label boost */
					if (o_ptr->pval > -10) c = I2D(-(o_ptr->pval));
				}
			}

			/* Sustain */
			if (f[1] & (1<<stat))
			{
				/* Dark green */
				a = TERM_GREEN;

				/* Convert '.' to 's' */
				if (c == '.') c = 's';
			}

			/* Dump proper character */
			Term_putch(col, row+stat, a, c);
		}

		/* Advance */
		col++;
	}

	/* Player flags */
	p_ptr->flags(f);

	/* Check stats */
	for (stat = 0; stat < A_MAX; ++stat)
	{
		/* Default */
		a = TERM_SLATE;
		c = '.';

		/* Sustain */
		if (f[1] & (1<<stat))
		{
			/* Dark green "s" */
			a = TERM_GREEN;
			c = 's';
		}

		/* Dump */
		Term_putch(col, row+stat, a, c);
	}

	/* Column */
	col = 26;

	/* Footer */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row+6, col);

	/* Equippy */
	display_player_equippy(row+7, col);
}

/* XXX use defines to get runtime constants for static assertion XXX */
#define PANEL1_PAGE_ROW 8
#define PANEL2_PAGE_ROW 8
#define PANEL3_PAGE_ROW 8
#define PANEL4_PAGE_ROW 8
#define PANEL5_PAGE_ROW 5

static const region boundaries [] =
{
	{ 0,	0,		0,		0 },
	{ 1,	2,		30,		PANEL1_PAGE_ROW }, /* Name, Class, ... */
	{ 1,	10,		18,		PANEL2_PAGE_ROW }, /* Cur Exp, Max Exp, ... */
	{ 26,	10,		17,		PANEL3_PAGE_ROW }, /* AC, melee, ... */
	{ 48, 	10,		24,		PANEL4_PAGE_ROW }, /* skills */
	{ 26,	3,		13,		PANEL5_PAGE_ROW }, /* Age, ht, wt, ... */

};


const char* player_type::title() const
{
	if (p_ptr->wizard)
		return "[=-WIZARD-=]";
	else if (p_ptr->total_winner || p_ptr->lev > PY_MAX_LEVEL)
		return "***WINNER***";
	else
		return player_type::c_text + p_ptr->cp_ptr->title[(p_ptr->lev - 1) / 5];
}

static const char *show_adv_exp(void)
{
	if (p_ptr->lev < PY_MAX_LEVEL)
	{
		static char buffer[30];
		s32b advance = (player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L);
		strnfmt(buffer, sizeof(buffer), "%d", advance);
		return buffer;
	}
	else {
		return "********";
	}
}

#if 0
static const char *show_depth(void)
{
	static char buffer[10];
	if (p_ptr->depth == 0) return "Town";
	else if (depth_in_feet)
	{
		strnfmt(buffer, sizeof(buffer), "%d ft", p_ptr->depth * 50);
		return buffer;
	}
	else {
		strnfmt(buffer, sizeof(buffer), "Lev %d", p_ptr->depth);
		return buffer;
	}
}
#endif

static const char *show_max_depth(void)
{
	static char buffer[10];
	if (p_ptr->max_depth == 0) return "Town";
	else if (OPTION(depth_in_feet))
	{
		strnfmt(buffer, sizeof(buffer), "%d ft", p_ptr->max_depth * 50);
		return buffer;
	}
	else {
		strnfmt(buffer, sizeof(buffer), "Lev %d", p_ptr->max_depth);
		return buffer;
	}
}

static const char *show_speed()
{
	static char buffer[10];
	int tmp = p_ptr->speed;
	if (p_ptr->timed[TMD_FAST]) tmp -= 10;
	if (p_ptr->timed[TMD_SLOW]) tmp += 10;
	if (p_ptr->searching) tmp += 10;
	if (tmp == 110) return "Normal";
	strnfmt(buffer, sizeof(buffer), "%d", tmp - 110);
	return buffer;
}

static const char *show_weapon(const object_type *o_ptr)
{
	/* buffer[0] <- melee; buffer[1] <- bow */
	static char buffer[2][12];
	int hit = p_ptr->dis_to_h;
	int dam = p_ptr->dis_to_d;
	const bool is_bow = (o_ptr->obj_id.tval == TV_BOW);
	if (o_ptr->known())
	{
		hit += o_ptr->to_h;
		if (!is_bow) dam += o_ptr->to_d;
	}
	strnfmt(buffer[is_bow], sizeof(buffer), "(%+d,%+d)", hit, dam);
	return buffer[is_bow];
}

static inline byte max_color(int val, int max)
{
	return val < max ? TERM_YELLOW : TERM_L_GREEN;
}


int get_panel(int oid, data_panel *panel, size_t size)
{
  size_t ret = size;
  switch(oid)
 {
  case 1:
  {
	data_panel panel1[] =
	{
		{ TERM_L_BLUE, "Name",	"%y",	 { s2u(op_ptr->full_name) , END  }},
		{ TERM_L_BLUE, "Sex",	"%y",	 { s2u(p_ptr->gender()) , END  }},
		{ TERM_L_BLUE, "Race",	"%y",	 { s2u(p_ptr->racename()), END  }},
		{ TERM_L_BLUE, "Class",	"%y",	 { s2u(p_ptr->classname()), END  }},
		{ TERM_L_BLUE, "Title",	"%y",	 { s2u(p_ptr->title()), END  }},
		{ TERM_L_BLUE, "HP",	"%y/%y", { i2u(p_ptr->chp), i2u(p_ptr->mhp)  }},
		{ TERM_L_BLUE, "SP",	"%y/%y", { i2u(p_ptr->csp), i2u(p_ptr->msp)  }},
		{ TERM_L_BLUE, "Level",	"%y",	 { i2u(p_ptr->lev), END  }}
	};
#ifdef ZAIBAND_STATIC_ASSERT
	ZAIBAND_STATIC_ASSERT(N_ELEMENTS(panel1) == PANEL1_PAGE_ROW);
#else
	assert(N_ELEMENTS(panel1) == boundaries[1].page_rows);
#endif
	if (ret > N_ELEMENTS(panel1)) ret = N_ELEMENTS(panel1);
	C_COPY(panel, panel1, ret);
	return ret;
  }
  case 2:
  {
	data_panel panel2[] =
	{
		{ max_color(p_ptr->lev, p_ptr->max_lev), "Level", "%y",   { i2u(p_ptr->lev), END  }},
		{ max_color(p_ptr->exp, p_ptr->max_exp), "Cur Exp", "%y", { i2u(p_ptr->exp), END  }},
		{ TERM_L_GREEN, "Max Exp",	"%y",		{ i2u(p_ptr->max_exp), END  }},
		{ TERM_L_GREEN, "Adv Exp",	"%y",		{ s2u(show_adv_exp()), END  }},
		{ TERM_L_GREEN,	"MaxDepth",	"%y",		{ s2u(show_max_depth()), END  }},
		{ TERM_L_GREEN, "Gold",		"%y",		{ i2u(p_ptr->au), END  }},
		{ TERM_L_GREEN, "Burden",	"%.1y lbs",	{ f2u(p_ptr->total_weight/10.0), END  }},
		{ TERM_L_GREEN, "Speed",	"%y",		{ s2u(show_speed()), END  }}
	};
#ifdef ZAIBAND_STATIC_ASSERT
	ZAIBAND_STATIC_ASSERT(N_ELEMENTS(panel2) == PANEL2_PAGE_ROW);
#else
	assert(N_ELEMENTS(panel2) == boundaries[2].page_rows);
#endif
	if (ret > N_ELEMENTS(panel2)) ret = N_ELEMENTS(panel2);
	C_COPY(panel, panel2, ret);
	return ret;
  }
  case 3:
  {
	data_panel panel3[] =
	{
		{ TERM_L_BLUE, "Armor", "[%y,%+y]",		{ i2u(p_ptr->dis_ac), i2u(p_ptr->dis_to_a)  }},
		{ TERM_L_BLUE, "Fight", "(%+y,%+y)",	{ i2u(p_ptr->dis_to_h), i2u(p_ptr->dis_to_d)  }},
		{ TERM_L_BLUE, "Melee", "%y",			{ s2u(show_weapon(&p_ptr->inventory[INVEN_WIELD])), END  }},
		{ TERM_L_BLUE, "Shoot", "%y",			{ s2u(show_weapon(&p_ptr->inventory[INVEN_BOW])), END  }},
		{ TERM_L_BLUE, "Blows", "%y/turn",		{ i2u(p_ptr->num_blow), END  }},
		{ TERM_L_BLUE, "Shots", "%y/turn",		{ i2u(p_ptr->num_fire), END  }},
		{ 0, 0, 0, {END} },
		{ TERM_L_BLUE, "Infra", "%y ft",		{ i2u(p_ptr->see_infra * 10), END  }}
	};
#ifdef ZAIBAND_STATIC_ASSERT
	ZAIBAND_STATIC_ASSERT(N_ELEMENTS(panel3) == PANEL3_PAGE_ROW);
#else
	assert(N_ELEMENTS(panel3) == boundaries[3].page_rows);
#endif
	if (ret > N_ELEMENTS(panel3)) ret = N_ELEMENTS(panel3);
	C_COPY(panel, panel3, ret);
	return ret;
  }
  case 4:
  {
	struct {
		const char *name;
		int skill;
		int div;
	} skills[] =
	{
		{ "Saving Throw", p_ptr->skills[SKILL_SAVE], 6 },
		{ "Stealth", p_ptr->skills[SKILL_STEALTH], 1 },
		{ "Fighting", p_ptr->skills[SKILL_TO_HIT_MELEE], 12 },
		{ "Shooting", p_ptr->skills[SKILL_TO_HIT_BOW], 12 },
		{ "Disarming", p_ptr->skills[SKILL_DISARM], 8 },
		{ "Magic Device", p_ptr->skills[SKILL_DEVICE], 6 },
		{ "Perception", p_ptr->skills[SKILL_SEARCH_FREQUENCY], 6 },
		{ "Searching", p_ptr->skills[SKILL_SEARCH], 6 }
	};
	size_t i;
#ifdef ZAIBAND_STATIC_ASSERT
	ZAIBAND_STATIC_ASSERT(N_ELEMENTS(skills) == PANEL4_PAGE_ROW);
#else
	assert(N_ELEMENTS(skills) == boundaries[4].page_rows);
#endif
	ret = N_ELEMENTS(skills);
	if (ret > size) ret = size;
	for (i = 0; i < ret; i++)
	{
		panel[i].color = TERM_L_BLUE;
		panel[i].label = skills[i].name;
		panel[i].fmt = "%y";
		panel[i].value[0] = s2u(likert(skills[i].skill, skills[i].div, &panel[i].color));
	}
	return ret;
  }
  case 5:
  {
	data_panel panel5[] =
	{
		{TERM_L_BLUE, "Age",		"%y",	{ i2u(p_ptr->age), END  }},
		{TERM_L_BLUE, "Height",		"%y",	{ i2u(p_ptr->ht), END  }},
		{TERM_L_BLUE, "Weight",		"%y",	{ i2u(p_ptr->wt), END  }},
		{TERM_L_BLUE, "Status",		"%y",	{ i2u(p_ptr->sc), END  }},
		{TERM_L_BLUE, "Maximize",	"%y",	{ c2u(OPTION(adult_maximize) ? 'Y' : 'N'), END  }}
	};
#ifdef ZAIBAND_STATIC_ASSERT
	ZAIBAND_STATIC_ASSERT(N_ELEMENTS(panel5) == PANEL5_PAGE_ROW);
#else
	assert(N_ELEMENTS(panel5) == boundaries[5].page_rows);
#endif
	if (ret > N_ELEMENTS(panel5)) ret = N_ELEMENTS(panel5);
	C_COPY(panel, panel5, ret);
	return ret;
  }
  default:	/* hopefully not reached */
	DEBUG_DIE_OR_LEAVE("get_panel default invoked",return 0);
 }
}

#undef PANEL1_PAGE_ROW
#undef PANEL2_PAGE_ROW
#undef PANEL3_PAGE_ROW
#undef PANEL4_PAGE_ROW
#undef PANEL5_PAGE_ROW


static void display_player_xtra_info(void)
{
	int i;
	int panels [] = { 1, 2, 3, 4, 5};
	bool left_adj [] = { 1, 0, 0, 0, 0 };
	data_panel data[MAX_PANEL];

	for (i = 0; i < (int)N_ELEMENTS(panels); i++)
	{
		int oid = panels[i];
		int rows = get_panel(oid, data, N_ELEMENTS(data));

		/* Hack:  Don't show 'Level' in the name, class ...  panel */
		if (oid == 1) rows -= 1;

		display_panel(data, rows, left_adj[i], &boundaries[oid]);
	}

	/* Indent output by 1 character, and wrap at column 72 */
	text_out_wrap = 72;
	text_out_indent = 1;

	/* History */
	Term_gotoxy(text_out_indent, 19);
	text_out_to_screen(TERM_WHITE, p_ptr->history);

	/* Reset text_out() vars */
	text_out_wrap = 0;
	text_out_indent = 0;

	return;
}

/*
 * Display the character on the screen (two different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank.
 *
 * Mode 0 = standard display with skills/history
 * Mode 1 = special display with equipment flags
 */
void display_player(int mode)
{
	/* Erase screen */
	clear_from(0);

	/* Stat info */
	display_player_stat_info();

	if (mode)
	{
		data_panel data[MAX_PANEL];
		int rows = get_panel(1, data, N_ELEMENTS(data));

		display_panel(data, rows, 1, &boundaries[1]);

		/* Stat/Sustain flags */
		display_player_sust_info();

		/* Other flags */
		display_player_flag_info();
	}

	/* Standard */
	else
	{
		/* Extra info */
		display_player_xtra_info();
	}
}


/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(const char* name, bool full)
{
	int i, x, y;

	byte a;
	char c;

	int fd;

	FILE *fff = NULL;

	store_type *st_ptr = &store[STORE_HOME];

	char o_name[80];

	char buf[1024];


	/* Unused parameter */
	(void)full;

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

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
		strnfmt(out_val, sizeof(out_val), "Replace existing file %s? ", buf);

		/* Ask */
		if (!get_check(out_val)) fd = -1;
	}

	/* Open the non-existing file */
	if (fd < 0) fff = my_fopen(buf, "w");


	/* Invalid file */
	if (!fff) return (-1);


	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Begin dump */
	fprintf(fff, "  [%s %s Character Dump]\n\n",
	        VERSION_NAME, VERSION_STRING);


	/* Display player */
	display_player(0);

	/* Dump part of the screen */
	for (y = 2; y < 23; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
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

	/* Skip a line */
	fprintf(fff, "\n");

	/* Display player */
	display_player(1);

	/* Dump part of the screen */
	for (y = 11; y < 20; y++)
	{
		/* Dump each row */
		for (x = 0; x < 39; x++)
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

	/* Skip a line */
	fprintf(fff, "\n");

	/* Dump part of the screen */
	for (y = 11; y < 20; y++)
	{
		/* Dump each row */
		for (x = 0; x < 39; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x + 40, y, &a, &c));

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

	/* Skip some lines */
	fprintf(fff, "\n\n");


	/* If dead, dump last messages -- Prfnoff */
	if (p_ptr->is_dead)
	{
		i = messages_num();
		if (i > 15) i = 15;
		fprintf(fff, "  [Last Messages]\n\n");
		while (i-- > 0)
		{
			fprintf(fff, "> %s\n", message_str((s16b)i));
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the equipment */
	if (p_ptr->equip_cnt)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			object_desc(o_name, sizeof(o_name), &p_ptr->inventory[i], TRUE, ODESC_FULL);
			fprintf(fff, "%c) %s\n",
			        index_to_label(i), o_name);

			/* Describe random object attributes */
			identify_random_gen(&p_ptr->inventory[i]);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	fprintf(fff, "  [Character Inventory]\n\n");
	for (i = 0; i < INVEN_PACK; i++)
	{
		if (!p_ptr->inventory[i].k_idx) break;

		object_desc(o_name, sizeof(o_name), &p_ptr->inventory[i], TRUE, ODESC_FULL);
		fprintf(fff, "%c) %s\n",
		        index_to_label(i), o_name);

		/* Describe random object attributes */
		identify_random_gen(&p_ptr->inventory[i]);
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
			object_desc(o_name, sizeof(o_name), &st_ptr->stock[i], TRUE, ODESC_FULL);
			fprintf(fff, "%c) %s\n", I2A(i), o_name);

			/* Describe random object attributes */
			identify_random_gen(&st_ptr->stock[i]);
		}

		/* Add an empty line */
		fprintf(fff, "\n\n");
	}


	/* Dump options */
	fprintf(fff, "  [Options]\n\n");

	/* Dump options */
	for (i = OPT_ADULT; i < OPT_MAX; i++)
	{
		if (options[i].desc)
		{
			fprintf(fff, "%-45s: %s (%s)\n",
			        options[i].desc,
			        op_ptr->opt[i] ? "yes" : "no ",
			        options[i].text);
		}
	}

	/* Skip some lines */
	fprintf(fff, "\n\n");


	/* Close it */
	my_fclose(fff);


	/* Success */
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
 * Return FALSE on "?", otherwise TRUE.
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
bool show_file(const char* name, const char* what, int line, int mode)
{
	int i, k, n;
	char ch;
	int next = 0;	/* Number of "real" lines passed by */
	int size;		/* Number of "real" lines in the file */
	int back = 0;	/* Backup value for "line" */
	bool menu = FALSE;	/* This screen has sub-screens */
	bool case_sensitive = FALSE;	/* Case sensitive search */
	FILE *fff = NULL;	/* Current help file */
	char *find = NULL;	/* Find this string (if any) */
	const char* tag = NULL;	/* Jump to this tag */
	char finder[80];	/* Hold a string to find */
	char shower[80];	/* Hold a string to show */
	char filename[1024];	/* Filename */
	char caption[128];	/* Describe this thing */
	char path[1024];	/* Path buffer */
	char buf[1024];		/* General buffer */
	char lc_buf[1024];	/* Lower case version of the buffer, for searching */
	char hook[26][32];	/* Sub-menu information */
	int wid, hgt;

	/* Wipe finder */
	finder[0] = '\x00';

	/* Wipe shower */
	shower[0] = '\x00';

	/* Wipe caption */
	caption[0] = '\x00';

	/* Wipe the hooks */
	for (i = 0; i < 26; i++) hook[i][0] = '\0';

	/* Get size */
	Term_get_size(&wid, &hgt);

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
			break;
		}
	}

	/* Redirect the name */
	name = filename;


	/* Hack XXX XXX XXX */
	if (what)
	{
		/* Caption */
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
		path_build(path, sizeof(path), ANGBAND_DIR_HELP, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Look in "info" */
	if (!fff)
	{
		/* Caption */
		strnfmt(caption, sizeof(caption), "Info file '%s'", name);

		/* Build the filename */
		path_build(path, sizeof(path), ANGBAND_DIR_INFO, name);

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
			if ((buf[6] == b1) && isalpha((unsigned char)buf[7]) &&
			    (buf[8] == b2) && (buf[9] == ' '))
			{
				/* This is a menu file */
				menu = TRUE;

				/* Extract the menu item */
				k = A2I(buf[7]);

				/* Store the menu item (if valid) */
				if ((k >= 0) && (k < 26))
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


		/* Restrict the visible range */
		if (line > (size - (hgt - 4))) line = size - (hgt - 4);
		if (line < 0) line = 0;


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


		/* Goto the selected line */
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

			/* Dump the line */
			Term_putstr(0, i+2, -1, TERM_WHITE, buf);

			/* Hilite "shower" */
			if (shower[0])
			{
				const char* str = lc_buf;

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


		/* Show a general "title" */
		prt(format("[%s %s, %s, Line %d-%d/%d]", VERSION_NAME, VERSION_STRING,
		           caption, line, line + hgt - 4, size), 0, 0);


		/* Prompt -- menu screen */
		if (menu)
		{
			/* Wait for it */
			prt("[Press a Number, or ESC to exit.]", hgt - 1, 0);
		}

		/* Prompt -- small files */
		else if (size <= hgt - 4)
		{
			/* Wait for it */
			prt("[Press ESC to exit.]", hgt - 1, 0);
		}

		/* Prompt -- large files */
		else
		{
			/* Wait for it */
			prt("[Press Space to advance, or ESC to exit.]", hgt - 1, 0);
		}

		/* Get a keypress */
		ch = inkey();

		/* Exit the help */
		if (ch == '?') break;

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
			(void)askfor_aux(shower, sizeof(shower));

			/* Make the "shower" lowercase */
			if (!case_sensitive) string_lower(shower);
		}

		/* Try finding */
		if (ch == '/')
		{
			/* Get "finder" */
			prt("Find: ", hgt - 1, 0);
			if (askfor_aux(finder, sizeof(finder)))
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
			if (askfor_aux(tmp, sizeof(tmp)))
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
			if (askfor_aux(ftmp, sizeof(ftmp)))
			{
				if (!show_file(ftmp, NULL, 0, mode)) ch = ESCAPE;
			}
		}

		/* Back up one line */
		if ((ch == '8') || (ch == '='))
		{
			line = line - 1;
		}

		/* Back up one half page */
		if (ch == '_')
		{
			line = line - ((hgt - 4) / 2);
		}

		/* Back up one full page */
		if ((ch == '9') || (ch == '-'))
		{
			line = line - (hgt - 4);
		}

		/* Back to the top */
		if (ch == '7')
		{
			line = 0;
		}

		/* Advance one line */
		if ((ch == '2') || (ch == '\n') || (ch == '\r'))
		{
			line = line + 1;
		}

		/* Advance one half page */
		if (ch == '+')
		{
			line = line + ((hgt - 4) / 2);
		}

		/* Advance one full page */
		if ((ch == '3') || (ch == ' '))
		{
			line = line + (hgt - 4);
		}

		/* Advance to the bottom */
		if (ch == '1')
		{
			line = size;
		}

		/* Recurse on letters */
		if (menu && isalpha((unsigned char)ch))
		{
			/* Extract the requested menu item */
			k = A2I(ch);

			/* Verify the menu item */
			if ((k >= 0) && (k <= 25) && hook[k][0])
			{
				/* Recurse on that file */
				if (!show_file(hook[k], NULL, 0, mode)) ch = ESCAPE;
			}
		}

		/* Exit on escape */
		if (ch == ESCAPE) break;
	}

	/* Close the file */
	my_fclose(fff);

	/* Done */
	return (ch != '?');
}


/*
 * Peruse the On-Line-Help
 */
void do_cmd_help(void)
{
	/* Save screen */
	screen_save();

	/* Peruse the main help file */
	(void)show_file("help.hlp", NULL, 0, 0);

	/* Load screen */
	screen_load();
}



/*
 * Process the player name and extract a clean "base name".
 *
 * If "sf" is TRUE, then we initialize "savefile" based on player name.
 *
 * Some platforms (Windows, Macintosh, Amiga) leave the "savefile" empty
 * when a new character is created, and then when the character is done
 * being created, they call this function to choose a new savefile name.
 */
void process_player_name(bool sf)
{
	int i;


	/* Process the player name */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		char c = op_ptr->full_name[i];

		/* No control characters */
		if (iscntrl((unsigned char)c))
		{
			/* Illegal characters */
			quit_fmt("Illegal control char (0x%02X) in player name", c);
		}

		/* Convert all non-alphanumeric symbols */
		if (!isalpha((unsigned char)c) && !isdigit((unsigned char)c)) c = '_';

		/* Build "base_name" */
		op_ptr->base_name[i] = c;
	}

#if defined(WINDOWS) || defined(MSDOS)

	/* Max length */
	if (i > 8) i = 8;

#endif

	/* Terminate */
	op_ptr->base_name[i] = '\0';

	/* Require a "base" name */
	if (!op_ptr->base_name[0])
	{
		strcpy(op_ptr->base_name, "PLAYER");
	}


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
 * Unix machines?  XXX XXX XXX
 *
 * What a horrible name for a global function.  XXX XXX XXX
 */
void get_name(void)
{
	char tmp[32];

	/* Save the player name */
	my_strcpy(tmp, op_ptr->full_name, sizeof(tmp));

	/* Prompt for a new name */
	if (get_string("Enter a name for your character: ", tmp, sizeof(tmp)))
	{
		/* Use the name */
		my_strcpy(op_ptr->full_name, tmp, sizeof(op_ptr->full_name));

		/* Process the player name */
		process_player_name(FALSE);
	}
}



/*
 * Hack -- commit suicide
 */
void do_cmd_suicide(void)
{
	/* Flush input */
	flush();

	/* Verify Retirement */
	if (p_ptr->total_winner)
	{
		/* Verify */
		if (!get_check("Do you want to retire? ")) return;
	}

	/* Verify Suicide */
	else
	{
		char ch;

		/* Verify */
		if (!get_check("Do you really want to commit suicide? ")) return;

		/* Special Verification for suicide */
		prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
		flush();
		ch = inkey();
		prt("", 0, 0);
		if (ch != '@') return;
	}

	/* Commit suicide */
	p_ptr->is_dead = TRUE;

	/* Stop playing */
	p_ptr->playing = FALSE;

	/* Leaving */
	p_ptr->leaving = TRUE;

	/* Cause of death */
	strcpy(p_ptr->died_from, "Quitting");
}



/*
 * Save the game
 */
void do_cmd_save_game(void)
{
	/* Disturb the player */
	disturb(1, 0);

	/* Clear messages */
	message_flush();

	/* Handle stuff */
	handle_stuff();

	/* Message */
	prt("Saving game...", 0, 0);

	/* Refresh */
	Term_fresh();

	/* The player is not dead */
	strcpy(p_ptr->died_from, "(saved)");

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Save the player */
	if (save_player())
	{
		prt("Saving game... done.", 0, 0);
	}

	/* Save failed (oops) */
	else
	{
		prt("Saving game... failed!", 0, 0);
	}

	/* Allow suspend again */
	signals_handle_tstp();

	/* Refresh */
	Term_fresh();

	/* Note that the player is not dead */
	strcpy(p_ptr->died_from, "(alive and well)");
}



/*
 * Hack -- Calculates the total number of points earned
 */
long total_points(void)
{
	return (p_ptr->max_exp + (100 * p_ptr->max_depth));
}



#if 0

/*
 * Save a "bones" file for a dead character
 *
 * Note that we will not use these files until a later version, and
 * then we will only use the name and level on which death occured.
 *
 * Should probably attempt some form of locking...
 */
static void make_bones(void)
{
	FILE *fp;

	char str[1024];


	/* Ignore wizards and borgs */
	if (!(p_ptr->noscore & (NOSCORE_WIZARD | NOSCORE_BORG))
	{
		/* Ignore people who die in town */
		if (p_ptr->depth)
		{
			char tmp[128];

			/* XXX XXX XXX "Bones" name */
			sprintf(tmp, "bone.%03d", p_ptr->depth);

			/* Build the filename */
			path_build(str, sizeof(str), ANGBAND_DIR_BONE, tmp);

			/* Attempt to open the bones file */
			fp = my_fopen(str, "r");

			/* Close it right away */
			if (fp) my_fclose(fp);

			/* Do not over-write a previous ghost */
			if (fp) return;

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

			/* Grab permissions */
			safe_setuid_grab();

			/* Try to write a new "Bones File" */
			fp = my_fopen(str, "w");

			/* Drop permissions */
			safe_setuid_drop();

			/* Not allowed to write it?  Weird. */
			if (!fp) return;

			/* Save the info */
			fprintf(fp, "%s\n", op_ptr->full_name);
			fprintf(fp, "%d\n", p_ptr->mhp);
			fprintf(fp, "%d\n", p_ptr->prace);
			fprintf(fp, "%d\n", p_ptr->pclass);

			/* Close and save the Bones file */
			my_fclose(fp);
		}
	}
}

#endif /* 0 */


/*
 * Hack - Know inventory and home items upon death
 */
static void death_knowledge(void)
{
	int i;

	store_type *st_ptr = &store[STORE_HOME];


	/* Hack -- Know everything in the inven/equip */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type* const o_ptr = &p_ptr->inventory[i];

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
		object_type* const o_ptr = &st_ptr->stock[i];

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
				strnfmt(tmp_val, sizeof(tmp_val), "%c) ", I2A(j));
				prt(tmp_val, j+2, 4);

				/* Get the object description */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

				/* Get the inventory color */
				attr = tval_to_attr[o_ptr->obj_id.tval % N_ELEMENTS(tval_to_attr)];

				/* Display the object */
				c_put_str(attr, o_name, j+2, 7);
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

	const char* q = "Examine which item? ";
	const char* s = "You have nothing to examine.";

	/* Start out in "display" mode */
	p_ptr->command_see = TRUE;

	/* Get an item */
	while (TRUE)
	{
		if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

		/* Get the item */
		o_ptr = &p_ptr->inventory[item];

		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);

		/* Describe */
		object_info_screen(o_ptr);
	}
}



/*
 * Change the player into a Winner
 */
static void kingly(void)
{
	/* Hack -- retire in town */
	p_ptr->depth = 0;

	/* Fake death */
	strcpy(p_ptr->died_from, "Ripe Old Age");

	/* Restore the experience */
	p_ptr->exp = p_ptr->max_exp;

	/* Restore the level */
	p_ptr->lev = p_ptr->max_lev;

	/* Hack -- Instant Gold */
	p_ptr->au += 10000000L;

	/* Clear screen */
	Term_clear();

	/* Display a crown */
	put_str("#", 1, 34);
	put_str("#####", 2, 32);
	put_str("#", 3, 34);
	put_str(",,,  $$$  ,,,", 4, 28);
	put_str(",,=$   \"$$$$$\"   $=,,", 5, 24);
	put_str(",$$        $$$        $$,", 6, 22);
	put_str("*>         <*>         <*", 7, 22);
	put_str("$$         $$$         $$", 8, 22);
	put_str("\"$$        $$$        $$\"", 9, 22);
	put_str("\"$$       $$$       $$\"", 10, 23);
	put_str("*#########*#########*", 11, 24);
	put_str("*#########*#########*", 12, 24);

	/* Display a message */
	put_str("Veni, Vidi, Vici!", 15, 26);
	put_str("I came, I saw, I conquered!", 16, 21);
	put_str(format("All Hail the Mighty %s!", p_ptr->win_rank()), 17, 22);

	/* Flush input */
	flush();

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
	const char* const p = "[(i)nformation, (m)essages, (f)ile dump, (v)iew scores, e(x)amine item, ESC]";


	/* Handle retirement */
	if (p_ptr->total_winner) kingly();

	/* Save dead player */
	if (!save_player())
	{
		msg_print("death save failed!");
		message_flush();
	}

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
				if (get_check("Do you want to quit? "))
					wants_to_quit = TRUE;

				break;
			}

			/* File dump */
			case 'f':
			case 'F':
			{
				char ftmp[80];

				strnfmt(ftmp, sizeof(ftmp), "%s.txt", op_ptr->base_name);

				if (get_string("File name: ", ftmp, sizeof(ftmp)))
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
							msg_print("Character dump successful.");
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

#if 0
	/* Dump bones file */
	make_bones();
#endif
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

	
	/* Open the high score file, for reading/writing */
	open_scoretable(O_RDWR);

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
		do_cmd_save_game();

		if (Term->mapped_flag)
		{
			/* Prompt for scores XXX XXX XXX */
			prt("Press Return (or Escape).", 0, 40);

			/* Predict score (or ESCAPE) */
			if (inkey() != ESCAPE) predict_score();
		}
	}


	/* Shut the high score file */
	close_scoretable();


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


static void write_html_escape_char(FILE *htm, char c)
{
	switch (c)
	{
		case '<':
			fprintf(htm, "&lt;");
			break;
		case '>':
			fprintf(htm, "&gt;");
			break;
		case '&':
			fprintf(htm, "&amp;");
			break;
		default:
			fprintf(htm, "%c", c);
			break;
	}
}


/*
 * Get the default (ASCII) tile for a given screen location
 */
static void get_default_tile(int row, int col, byte *a_def, char *c_def)
{
	byte a;
	char c;

	int wid, hgt;
	int screen_wid, screen_hgt;

	int x;
	int y = row - ROW_MAP + Term->offset_y;

	/* Retrieve current screen size */
	Term_get_size(&wid, &hgt);

	/* Calculate the size of dungeon map area (ignoring bigscreen) */
	screen_wid = wid - (COL_MAP + 1);
	screen_hgt = hgt - (ROW_MAP + 1);

	/* Get the tile from the screen */
	a = Term->scr->a[row][col];
	c = Term->scr->c[row][col];

	/* Skip bigtile placeholders */
	if (use_bigtile && (a == 255) && (c == -1))
	{
		/* Replace with "white space" */
		a = TERM_WHITE;
		c = ' ';
	}
	/* Convert the map display to the default characters */
	else if (!character_icky &&
	    ((col - COL_MAP) >= 0) && ((col - COL_MAP) < screen_wid) &&
	    ((row - ROW_MAP) >= 0) && ((row - ROW_MAP) < screen_hgt))
	{
		/* Bigtile uses double-width tiles */
		if (use_bigtile)
			x = (col - COL_MAP) / 2 + Term->offset_x;
		else
			x = col - COL_MAP + Term->offset_x;

		/* Convert dungeon map into default attr/chars */
		if (in_bounds(y, x))
		{
			/* Retrieve attr/char.  Precondition: reset graphics, no prefs. */
			byte discard_a;
			char discard_c;
			grid_data(y, x).as_text(a, c, discard_a, discard_c);
		}
		else
		{
			/* "Out of bounds" is empty */
			a = TERM_WHITE;
			c = ' ';
		}

		if (c == '\0') c = ' ';
	}

	/* Filter out remaining graphics */
	if (a & 0xf0)
	{
		/* Replace with "white space" */
		a = TERM_WHITE;
		c = ' ';
	}

	/* Return the default tile */
	*a_def = a;
	*c_def = c;
}


/* Take an html screenshot */
void html_screenshot(const char* name)
{
	int y, x;
	int wid, hgt;

	byte a = 0;
	byte oa = TERM_WHITE;
	char c = ' ';

	FILE *htm;

	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	htm = my_fopen(buf, "w");

	/* Oops */
	if (!htm)
	{
		plog_fmt("Cannot write the '%s' file!", buf);
		return;
	}

	/* Retrieve current screen size */
	Term_get_size(&wid, &hgt);

	fprintf(htm, "<HTML>\n");
	fprintf(htm, "<HEAD>\n");
	fprintf(htm, "<META NAME=\"GENERATOR\" Content=\"%s %d.%d.%d\">\n",
	             VERSION_NAME, VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
	fprintf(htm, "<TITLE>%s</TITLE>\n", name);
	fprintf(htm, "</HEAD>\n\n");
	fprintf(htm, "<BODY TEXT=\"#FFFFFF\" BGCOLOR=\"#000000\">\n");
	fprintf(htm, "<PRE><TT>\n");

	/* Dump the screen */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
			/* Get the ASCII tile */
			get_default_tile(y, x, &a, &c);

			/* Color change */
			if (oa != a)
			{
				/* From the default white to another color */
				if (oa == TERM_WHITE)
				{
					fprintf(htm, "<FONT COLOR=\"#%02X%02X%02X\">",
					        angband_color_table[a][1],
					        angband_color_table[a][2],
					        angband_color_table[a][3]);
				}
				/* From another color to the default white */
				else if (a == TERM_WHITE)
				{
					fprintf(htm, "</FONT>");
				}
				/* Change colors */
				else
				{
					fprintf(htm, "</FONT><FONT COLOR=\"#%02X%02X%02X\">",
					        angband_color_table[a][1],
					        angband_color_table[a][2],
					        angband_color_table[a][3]);
				}

				/* Remember the last color */
				oa = a;
			}

			/* Write the character and escape special HTML characters */
			write_html_escape_char(htm, c);
		}

		/* End the row */
		fprintf(htm, "\n");
	}

	/* Close the last <font> tag if necessary */
	if (a != TERM_WHITE) fprintf(htm, "</FONT>");

	fprintf(htm, "</TT></PRE>\n");

	fprintf(htm, "</BODY>\n");
	fprintf(htm, "</HTML>\n");

	/* Close it */
	my_fclose(htm);
}
