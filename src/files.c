/* File: files.c */

/* Code for multiuser machines, Colors for skill descriptions, the char-
 * acter screens (inc. resistance flags for races, etc.), equippy chars,
 * online-help, extraction of base name (for savefiles), saving, death
 * (with inventory, equip, etc. display), calculating and displaying
 * scores, creating tombstones, winners, panic saves, reading a random
 * line from a file, and signal handling.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * You may or may not want to use the following "#undef".
 */
/* #undef _POSIX_SAVED_IDS */


/*
 * Hack -- drop permissions
 *
 void safe_setuid_drop(void)
 {

 #ifdef SET_UID

 # ifdef SAFE_SETUID

 #  ifdef SAFE_SETUID_POSIX

 if (setuid(getuid()) != 0)
 {
 quit("setuid(): cannot set permissions correctly!");
 }
 if (setgid(getgid()) != 0)
 {
 quit("setgid(): cannot set permissions correctly!");
 }

 #  else

 if (setreuid(geteuid(), getuid()) != 0)
 {
 quit("setreuid(): cannot set permissions correctly!");
 }
 if (setregid(getegid(), getgid()) != 0)
 {
 quit("setregid(): cannot set permissions correctly!");
 }

 #  endif

 # endif

 #endif
  
 }
*/

/*
 * Hack -- grab permissions
 *
 void safe_setuid_grab(void)
 {
  
 #ifdef SET_UID

 # ifdef SAFE_SETUID

 #  ifdef SAFE_SETUID_POSIX

 if (setuid(player_euid) != 0)
 {
 quit("setuid(): cannot set permissions correctly!");
 }
 if (setgid(player_egid) != 0)
 {
 quit("setgid(): cannot set permissions correctly!");
 }

 #  else

 if (setreuid(geteuid(), getuid()) != 0)
 {
 quit("setreuid(): cannot set permissions correctly!");
 }
 if (setregid(getegid(), getgid()) != 0)
 {
 quit("setregid(): cannot set permissions correctly!");
 }

 #  endif

 # endif

 #endif

 }
*/


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
 * Specify the attr/char values for "monsters" by race index
 *   R:<num>:<a>/<c>
 *
 * Specify the attr/char values for "objects" by kind index
 *   K:<num>:<a>/<c>
 *
 * Specify the attr/char values for "features" by feature index
 *   F:<num>:<a>/<c>
 *
 * Specify the attr/char values for "flavors" by flavors index.
 *   L:<num>:<a>/<c>
 *
 * Specify the attr/char values for "special" things
 *   S:<num>:<a>/<c>
 *
 * Specify the attr/char values for unaware "objects" by kind tval
 *   U:<tv>:<a>/<c>
 *
 * Specify the attribute values for inventory "objects" by kind tval
 *   E:<tv>:<a>
 *
 * Define a macro action, given an encoded macro action
 *   A:<str>
 *
 * Create a macro, given an encoded macro trigger
 *   P:<str>
 *
 * Create a keymap, given an encoded keymap trigger
 *   C:<num>:<str>
 *
 * Turn an option off, given its name
 *   X:<str>
 *
 * Turn an option on, given its name
 *   Y:<str>
 *
 * Specify visual information, given an index, and some data
 *   V:<num>:<kv>:<rv>:<gv>:<bv>
 */
errr process_pref_file_aux(char *buf)
{
	int i, j, n1, n2, sq;
  
	char *zz[16];
  

	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace(buf[0])) return (0);

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
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= z_info->r_max) return (1);
			r_ptr = &r_info[i];
			if (n1) r_ptr->x_attr = n1;
			if (n2) r_ptr->x_char = n2;
			return (0);
		}
	}
  
  
	/* Process "B:<k_idx>:inscription */
	else if (buf[0] == 'B')
	{
		if (2 == tokenize(buf + 2, 2, zz))
		{
			add_autoinscription(strtol(zz[0], NULL, 0), zz[1]);
			return (0);
		}
	}
  
	/* Process "Q:<idx>:<tval>:<sval>:<y|n>"  -- squelch bits   */
	/* and     "Q:<idx>:<val>"                -- squelch levels */
	/* and     "Q:<val>"                      -- auto_destroy   */
	else if (buf[0] == 'Q')
	{
		i = tokenize(buf+2, 4, zz);
		if (i == 2)
		{
			n1 = strtol(zz[0], NULL, 0);
			n2 = strtol(zz[1], NULL, 0);
			squelch_level[n1] = n2;
			return(0);
		}
		else if (i == 4)
		{
			i = strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			sq = strtol(zz[3], NULL, 0);
			if ((k_info[i].tval == n1) && (k_info[i].sval == n2))
			{
				k_info[i].squelch = sq;
				return(0);
			}
			else
			{
				for (i = 1; i < z_info->k_max; i++)
				{
					if ((k_info[i].tval == n1) && (k_info[i].sval == n2))
					{
						k_info[i].squelch = sq;
						return(0);
					}
				}
			}
		}
	}
  
	/* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
	else if (buf[0] == 'K')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			object_kind *k_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= z_info->k_max) return (1);
			k_ptr = &k_info[i];
			if (n1) k_ptr->x_attr = n1;
			if (n2) k_ptr->x_char = n2;
			return (0);
		}
	}


	/* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
	else if (buf[0] == 'F')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			feature_type *f_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= z_info->f_max) return (1);
			f_ptr = &f_info[i];
			if (n1) f_ptr->x_attr = n1;
			if (n2) f_ptr->x_char = n2;

			return (0);
		}
	}
  
  
	/* Process "L:<num>:<a>/<c>" -- attr/char for flavors */
	else if (buf[0] == 'L')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			flavor_type *flavor_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= z_info->flavor_max)) return (1);
			flavor_ptr = &flavor_info[i];
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
			j = (byte)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			misc_to_attr[j] = n1;
			misc_to_char[j] = n2;
			return (0);
		}
	}


	/* Process "U:<tv>:<a>/<c>" -- attr/char for unaware items */
	else if (buf[0] == 'U')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			j = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			for (i = 1; i < z_info->k_max; i++)
			{
				object_kind *k_ptr = &k_info[i];
				if (k_ptr->tval == j)
				{
					if (n1) k_ptr->d_attr = n1;
					if (n2) k_ptr->d_char = n2;
				}
			}
			return (0);
		}
	}


	/* Process "E:<tv>:<a>" -- attribute for inventory objects */
	else if (buf[0] == 'E')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{
			j = (byte)strtol(zz[0], NULL, 0) % 128;
			n1 = strtol(zz[1], NULL, 0);
			if (n1) tval_to_attr[j] = n1;
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
		int mode;

		char tmp[1024];

		if (tokenize(buf+2, 2, zz) != 2) return (1);

		mode = strtol(zz[0], NULL, 0);
		if ((mode < 0) || (mode >= KEYMAP_MODES)) return (1);
      
		text_to_ascii(tmp, sizeof(tmp), zz[1]);
		if (!tmp[0] || tmp[1]) return (1);
		i = (byte)(tmp[0]);
      
		string_free(keymap_act[mode][i]);

		keymap_act[mode][i] = string_make(macro_buffer);

		/* XXX Mega-Hack - Let system know a keymap changed */
		angband_keymap_flag = TRUE;

		return (0);
	}


	/* Process "V:<num>:<kv>:<rv>:<gv>:<bv>" -- visual info */
	else if (buf[0] == 'V')
	{
		if (tokenize(buf+2, 5, zz) == 5)
		{
			i = (byte)strtol(zz[0], NULL, 0);
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
			cptr s;
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
		for (i = 0; i < OPT_adult_start; i++)
		{
			if (option_text[i] && streq(option_text[i], buf + 2))
			{
				op_ptr->opt[i] = FALSE;
				return (0);
			}
		}
	}

	/* Process "Y:<str>" -- turn option on */
	else if (buf[0] == 'Y')
	{
		for (i = 0; i < OPT_adult_start; i++)
		{
			if (option_text[i] && streq(option_text[i], buf + 2))
			{
				op_ptr->opt[i] = TRUE;
				return (0);
			}
		}
	}

	/* Process "W:<win>:<flag>:<value>" -- window flags */
	else if (buf[0] == 'W')
	{
		int win, flag, value;

		if (tokenize(buf + 2, 3, zz) == 3)
		{
			win = strtol(zz[0], NULL, 0);
			flag = strtol(zz[1], NULL, 0);
			value = strtol(zz[2], NULL, 0);

			/* Ignore illegal windows */
			/* Hack -- Ignore the main window */
			if ((win <= 0) || (win >= 8)) return (1);

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
			u16b type = (u16b)strtol(zz[0], NULL, 0);
			int color = color_char_to_attr(zz[1][0]);

			/* Ignore illegal types */
			if (type >= MSG_MAX) return (1);
          
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
	while (isspace(*s)) s++;

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
		while (isprint(*s) && !strchr(" []", *s)) ++s;

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
				v = p_name + rp_ptr->name;
			}
          
			/* Class */
			else if (streq(b+1, "CLASS"))
			{
				v = c_name + cp_ptr->name;
			}
          
			/* Player */
			else if (streq(b+1, "PLAYER"))
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
 * Process the "user pref file" with the given name
 *
 * See the function above for a list of legal "commands".
 *
 * We also accept the special "?" and "%" directives, which
 * allow conditional evaluation and filename inclusion.
 */
errr process_pref_file(cptr name)
{
	FILE *fp;
  
	char buf[1024];

	char old[1024];

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;
  
  
	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_PREF, name);
  
	/* Open the file */
	fp = my_fopen(buf, "r");
  
	/* Don't stop at non-existing file */
	if (!fp)
	{
		/* Build the filename */
		path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);
      
		/* Open the file */
		fp = my_fopen(buf, "r");
	}

	/* No such file */
	if (!fp) return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Count lines */
		num++;


		/* Skip "empty" lines */
		if (!buf[0]) continue;

		/* Skip "blank" lines */
		if (isspace(buf[0])) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;


		/* Save a copy */
		strcpy(old, buf);


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
		err = process_pref_file_aux(buf);
      
		/* Oops */
		if (err) break;
	}


	/* Error */
	if (err)
	{
		/* Useful error message */
		msg_format("Error %d in line %d of file '%s'.", err, num, name);
		msg_format("Parsing '%s'", old);
		msg_print(NULL);
	}

	/* Close the file */
	my_fclose(fp);

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

#endif


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

#endif

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
	path_build(buf, 1024, ANGBAND_DIR_FILE, "time.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No file, no restrictions */
	if (!fp) return (0);

	/* Assume restrictions */
	check_time_flag = TRUE;

	/* Parse the file */
	while (0 == my_fgets(fp, buf, 80))
	{
		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Chop the buffer */
		buf[29] = '\0';

		/* Extract the info */
		if (prefix(buf, "SUN:")) strcpy(days[0], buf);
		if (prefix(buf, "MON:")) strcpy(days[1], buf);
		if (prefix(buf, "TUE:")) strcpy(days[2], buf);
		if (prefix(buf, "WED:")) strcpy(days[3], buf);
		if (prefix(buf, "THU:")) strcpy(days[4], buf);
		if (prefix(buf, "FRI:")) strcpy(days[5], buf);
		if (prefix(buf, "SAT:")) strcpy(days[6], buf);
	}

	/* Close it */
	my_fclose(fp);

#endif

	/* Success */
	return (0);
}


/*
 * Hack -- pass color info around this file
 */
static byte likert_color = TERM_WHITE;


/*
 * Returns a "rating" of x depending on y
 */
static cptr likert(int x, int y)
{
	/* Paranoia */
	if (y <= 0) y = 1;
  
	/* Negative value */
	if (x < 0)
	{
		likert_color = TERM_L_DARK;
		return ("Awful");
	}
  
	/* Analyze the value */
	switch ((x / y))
	{
	case 0:
	{
		likert_color = TERM_RED;
		return ("Very Bad");
	}
	case 1:
	{
		likert_color = TERM_L_RED;
		return ("Bad");
	}
	case 2:
	{
		likert_color = TERM_ORANGE;
		return ("Poor");
	}
	case 3:
	{
		likert_color = TERM_ORANGE;
		return ("Mediocre");
	}
	case 4:
	{
		likert_color = TERM_YELLOW;
		return ("Fair");
	}
	case 5:
	{
		likert_color = TERM_YELLOW;
		return ("Good");
	}
	case 6:
	case 7:
	{
		likert_color = TERM_YELLOW;
		return ("Very Good");
	}
	case 8:
	case 9:
	case 10:
	{
		likert_color = TERM_L_GREEN;
		return ("Excellent");
	}
	case 11:
	case 12:
	case 13:
	{
		likert_color = TERM_L_GREEN;
		return ("Superb");
	}
	case 14:
	case 15:
	case 16:
	case 17:
	{
		likert_color = TERM_BLUE;
		return ("Heroic");
	}
	default:
	{
		likert_color = TERM_BLUE;
		return ("Legendary");
	}
        }
}


/*
 * Obtain the "flags" for the player as if he was an item.  Currently includes
 * race, class, and shapechange (optionally). -LM-
 *
 * Mega - Hack
 * 'shape' should be set on when calling this function for display purposes,
 * but off when actually applying 'intrinsic flags' in xtra1.c.
 *
 * Shapeshift flags are displayed like race/class flags, but actually
 * applied differently.
 */

void player_flags(u32b *f1, u32b *f2, u32b *f3, bool shape)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Add racial flags */
	(*f1) |= rp_ptr->flags1;
	(*f2) |= rp_ptr->flags2;
	(*f3) |= rp_ptr->flags3;

	/* Warrior. */
	if (check_ability(SP_RELENTLESS))
	{
                if (p_ptr->lev >= 30) (*f2) |= (TR2_RES_FEAR);
		if (p_ptr->lev >= 40) (*f3) |= (TR3_REGEN);
	}
  
	/* Specialty ability Holy Light */
	if (check_ability(SP_HOLY_LIGHT))
	{
		(*f2) |= (TR2_RES_LITE);
	}
  
	/* Specialty ability Unlight */
	if (check_ability(SP_UNLIGHT))
	{
		(*f2) |= (TR2_RES_DARK);
	}

	if (shape)
	{

		/* Shapechange, if any. */
		switch (p_ptr->schange)
		{
		case SHAPE_BEAR:
		case SHAPE_NORMAL:
		{
			break;
		}
		case SHAPE_MOUSE:
		{
			(*f1) |= (TR1_STEALTH);
			(*f1) |= (TR1_INFRA);
			break;
		}
		case SHAPE_FERRET:
		{
			(*f1) |= (TR1_INFRA);
			(*f3) |= (TR3_REGEN);
			(*f1) |= (TR1_SPEED);
			(*f1) |= (TR1_SEARCH);
			break;
		}
		case SHAPE_HOUND:
		{
			(*f1) |= (TR1_INFRA);
			(*f3) |= (TR3_TELEPATHY);
			break;
		}
		case SHAPE_GAZELLE:
		{
			(*f1) |= (TR1_SPEED);
			break;
		}
		case SHAPE_LION:
		{
			(*f1) |= (TR1_INFRA);
			(*f2) |= (TR2_RES_FEAR);
			(*f3) |= (TR3_REGEN);
			(*f1) |= (TR1_SPEED);
			break;
		}
		case SHAPE_ENT:
		{
			(*f2) |= (TR2_RES_COLD);
			(*f2) |= (TR2_RES_POIS);
			(*f1) |= (TR2_RES_FEAR);
			(*f3) |= (TR3_SEE_INVIS);
			(*f3) |= (TR3_FREE_ACT);
			(*f1) |= (TR1_TUNNEL);
			break;
		}
		case SHAPE_BAT:
		{
			(*f1) |= (TR1_INFRA);
			(*f2) |= (TR2_RES_BLIND);
			(*f3) |= (TR3_FEATHER);
			(*f1) |= (TR1_SPEED);
			break;
		}
		case SHAPE_WEREWOLF:
		{
			(*f1) |= (TR1_INFRA);
			(*f3) |= (TR3_REGEN);
			(*f3) |= (TR3_AGGRAVATE);
			break;
		}
		case SHAPE_VAMPIRE:
		{
			(*f1) |= (TR1_INFRA);
			(*f3) |= (TR3_SEE_INVIS);
			(*f3) |= (TR3_HOLD_LIFE);
			(*f2) |= (TR2_RES_COLD);
			(*f3) |= (TR3_REGEN);
			(*f1) |= (TR1_STEALTH);
			(*f1) |= (TR1_MAGIC_MASTERY);
			break;
		}
		case SHAPE_WYRM:
		{
			object_type *o_ptr = &inventory[INVEN_BODY];
			(*f1) |= (TR1_STEALTH);
			(*f1) |= (TR1_MAGIC_MASTERY);

			/* Paranoia */
			if (o_ptr->tval != TV_DRAG_ARMOR) break;

			/* Add 'extra' power if any */
			switch (o_ptr->sval)
			{

			case (SV_DRAGON_BLACK):
			{
				(*f2) |= (TR2_IM_ACID);
				break;
			}
			case (SV_DRAGON_BLUE):
			{
				(*f2) |= (TR2_IM_ELEC);
				break;
			}
			case (SV_DRAGON_WHITE):
			{
				(*f2) |= (TR2_IM_COLD);
				break;
			}
			case (SV_DRAGON_RED):
			{
				(*f2) |= (TR2_IM_FIRE);
				break;
			}
			case (SV_DRAGON_GREEN):
			{
				(*f3) |= (TR3_REGEN);
				break;
			}
			case (SV_DRAGON_SHINING):
			{
				(*f3) |= (TR3_SEE_INVIS);
				break;
			}
			case (SV_DRAGON_LAW):
			case (SV_DRAGON_CHAOS):
			{
				(*f3) |= (TR3_HOLD_LIFE);
				break;
			}
			case (SV_DRAGON_BRONZE):
			case (SV_DRAGON_GOLD):
			{
				(*f3) |= (TR3_FREE_ACT);
				break;
			}

			}
			break;
		}
		}
	}
}

/*
 * Obtain information about player negative mods.
 * Currently includes shapechange and race effects.
 *
 * We do not include AGGRAVATE, which is inherantly bad.  We only use
 * 'reversed' effects.
 *
 * The only effects that we *do* include are those which either totally
 * negate a resist/ability or those which have a negatively effective
 * pval.
 *
 * Based on player_flags, but for display purposes only.
 */
void player_weakness_dis(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* HACK - add weakness of some races */
	if (check_ability(SP_WOODEN))
	{
		(*f3) |= (TR3_FEATHER);
                (*f2) |= (TR2_RES_FIRE);
        }

        if (check_ability(SP_SHADOW))
        {
                (*f2) |= (TR2_RES_LITE);
        }

        /* Shapechange, if any. */
        switch (p_ptr->schange)
        {
	case SHAPE_NORMAL:
	{
		break;
	}
	case SHAPE_MOUSE:
	case SHAPE_FERRET:
	case SHAPE_HOUND:
	case SHAPE_GAZELLE:
	case SHAPE_LION:
	case SHAPE_BAT:
	case SHAPE_WEREWOLF:
	case SHAPE_BEAR:
	{
		(*f1) |= (TR1_MAGIC_MASTERY);
		break;
	}
	case SHAPE_ENT:
	{
		(*f2) |= (TR2_RES_FIRE);
		(*f3) |= (TR3_FEATHER);
		break;
	}
	case SHAPE_VAMPIRE:
	{
		(*f2) |= (TR2_RES_LITE);
		(*f3) |= (TR3_LITE);
		break;
	}
	case SHAPE_WYRM:
	{
		(*f1) |= (TR1_STEALTH);
		break;
	}
	}

}


/*
 * Hack -- see below
 */
static u32b display_player_resists[2][8] =
{
	{
		TR2_RES_ACID,
		TR2_RES_ELEC,
		TR2_RES_FIRE,
		TR2_RES_COLD,
		TR2_RES_POIS,
		TR2_RES_FEAR,
		TR2_RES_LITE,
		TR2_RES_DARK
	},

	{
		TR2_RES_BLIND,
		TR2_RES_CONFU,
		TR2_RES_SOUND,
		TR2_RES_SHARD,
		TR2_RES_NEXUS,
		TR2_RES_NETHR,
		TR2_RES_CHAOS,
		TR2_RES_DISEN
	}
};

/*
 * Hack -- see below
 */
static u32b display_player_flags[2][8] =
{
	{
		TR3_SLOW_DIGEST,
		TR3_FEATHER,
		TR3_LITE,
		TR3_REGEN,
		TR3_TELEPATHY,
		TR3_SEE_INVIS,
		TR3_FREE_ACT,
		TR3_HOLD_LIFE,
	},

	{
		TR1_MAGIC_MASTERY,
		TR1_STEALTH,
		TR1_SEARCH,
		TR1_INFRA,
		TR1_TUNNEL,
		TR1_SPEED,
		TR1_MIGHT2,
		TR1_SHOTS,
	}
};

/*
 * Hack -- see below
 */
static cptr display_player_resist_names[2][8] =
{
	{
                " Acid:",       /* TR2_RES_ACID */
		" Elec:",	/* TR2_RES_ELEC */
                " Fire:",       /* TR2_RES_FIRE */
                " Cold:",       /* TR2_RES_COLD */
                " Pois:",       /* TR2_RES_POIS */
                " Fear:",       /* TR2_RES_FEAR */
                " Lite:",       /* TR2_RES_LITE */
                " Dark:"        /* TR2_RES_DARK */
        },

        {
                "Blind:",       /* TR2_RES_BLIND */
                "Confu:",       /* TR2_RES_CONFU */
                "Sound:",       /* TR2_RES_SOUND */
                "Shard:",       /* TR2_RES_SHARD */
		"Nexus:",	/* TR2_RES_NEXUS */
		"Nethr:",       /* TR2_RES_NETHR */
		"Chaos:",       /* TR2_RES_CHAOS */
		"Disen:"        /* TR2_RES_DISEN */
	}
};


static cptr display_player_flag_names[2][8] =
{
        {
	        "S.Dig:",       /* TR3_SLOW_DIGEST */
		"Feath:",       /* TR3_FEATHER */
		"PLite:",	/* TR3_LITE */
		"Regen:",	/* TR3_REGEN */
		"Telep:",       /* TR3_TELEPATHY */
		"Invis:",       /* TR3_SEE_INVIS */
		"FrAct:",       /* TR3_FREE_ACT */
		"HLife:",       /* TR3_HOLD_LIFE */
	},
	
	{
		"M-Mas:",	/* TR1_MAGIC_MASTERY */
		"Stea.:",	/* TR1_STEALTH */
		"Sear.:",	/* TR1_SEARCH */
		"Infra:",	/* TR1_INFRA */
		"Tunn.:",	/* TR1_TUNNEL */
		"Speed:",       /* TR1_SPEED */
		"Might:",       /* TR1_MIGHT2.  Hack -- also TR1_MIGHT1 */
		"Shots:",       /* TR1_SHOTS */
	}
};


/*
 * Display the character on the screen (various modes)
 *
 * Completely redone for FA 030 to use the new 'C' screen display.
 *
 * Mode 0 = basic display with skills and history
 * Mode 1 = extra display with powers and resistances
 *
 */
void display_player(int mode)
{
	int last_line = 0;

	/* Erase screen */
	clear_from(0);

	/* Make the array of lines */
	last_line = make_dump(((mode == 0) ? pline0 : pline1), mode);

	/* Display the player */
	display_dump(((mode == 0) ? pline0 : pline1), 0, 25, 0);

}

/* 
 * Hack - include a chunk of the old display code to deal with small screen
 * birth
 */


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
 * Print decimal number with header at given row, column
 */
static void prt_deci(cptr header, int num, int deci, int row, int col, 
                     byte color)
{
	int len = strlen(header);
	char out_val[32];
	put_str(header, row, col);
	put_str("   ", row, col + len);
	sprintf(out_val, "%8ld", (long)deci);
	c_put_str(color, out_val, row, col + len);
	sprintf(out_val, "%6ld", (long)num);
	c_put_str(color, out_val, row, col + len);
	sprintf(out_val, ".");
	c_put_str(color, out_val, row, col + len + 6);
}

/*
 * Display the character for small screen birth
 *
 */
void display_player_sml(void)
{
	int i;
  
	int stat;
  
	u32b f1, f2, f3;
	u32b ignore_f2, ignore_f3;
	s16b k_idx;

	byte a;
	char c;
  
	char buf[80];
  
	int show_m_tohit = p_ptr->dis_to_h;
	int show_a_tohit = p_ptr->dis_to_h;
	int show_m_todam = p_ptr->dis_to_d;
	int show_a_todam = p_ptr->dis_to_d;
  
	object_type *o_ptr;
	char tmp[32];
  
	/* Erase screen */
	clear_from(0);

	/* Name, Sex, Race, Class */
	put_str("Name    :", 1, 1);
	put_str("Sex     :", 2, 1);
	put_str("Race    :", 1, 27);
	put_str("Class   :", 2, 27);
  
	c_put_str(TERM_L_BLUE, op_ptr->full_name, 1, 11);
	c_put_str(TERM_L_BLUE, sp_ptr->title, 2, 11);
  
	c_put_str(TERM_L_BLUE, p_name + rp_ptr->name, 1, 37);
	c_put_str(TERM_L_BLUE, c_name + cp_ptr->name, 2, 37);
  
	/* Header and Footer */
	put_str("abcdefghijkl@", 3, 25);
  
	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Assume uppercase stat name */
		put_str(stat_names[i], 4 + i, 0);
      
		/* Indicate natural maximum */
		if (p_ptr->stat_max[i] == 18+100)
		{
			put_str("!", 4 + i, 3);
		}
      
		/* Obtain the current stat (modified) */
		cnv_stat(p_ptr->stat_use[i], buf);
      
		/* Display the current stat (modified) */
		c_put_str(TERM_L_GREEN, buf, 4 + i, 8);
	}

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		/* Access object */
		o_ptr = &inventory[i];

		/* Object kind */
		k_idx = o_ptr->k_idx;

		/* Acquire "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Hack -- assume stat modifiers are known */
		object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3);

		/* Initialize color based of sign of pval. */
		for (stat=0; stat<A_MAX; stat++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';

			/* Boost */
			if (f1 & 1<<stat)
			{
				/* Default */
				c = '*';

				/* Good */
				if (o_ptr->pval > 0)
				{
					/* Good */
					a = TERM_L_GREEN;

					/* Label boost */
					if (o_ptr->pval < 10) c = '0' + o_ptr->pval;
				}

				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Bad */
					a = TERM_RED;

					/* Label boost */
					if (o_ptr->pval < 10) c = '0' - o_ptr->pval;
				}
			}

			/* Sustain */
			if (f2 & 1<<stat)
			{
				/* Dark green, "s" if no stat bonus. */
				a = TERM_GREEN;
				if (c == '.') c = 's';
			}
          
			/* Dump proper character */
			Term_putch(i + 1, 4 + stat, a, c);
		}
      
		/* Player flags */
		player_flags(&f1, &f2, &f3, TRUE);

		/* Check stats */
		for (stat=0; stat<A_MAX; stat++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';
          
			/* Sustain */
			if (f2 & 1<<stat)
			{
				/* Dark green "s" */
				a = TERM_GREEN;
				c = 's';
			}
          
			/* Dump */
			Term_putch(37, 4 + stat, a, c);
		}
	}

	/* Dump the fighting bonuses to hit/dam */
  
	put_str("       (Fighting)    ", 14, 1);
	prt_num("Blows/Round      ", p_ptr->num_blow, 15, 1, TERM_L_BLUE);
	prt_num("+ to Skill       ", show_m_tohit, 16, 1, TERM_L_BLUE);
  
	if (show_m_todam >= 0)
		prt_num("Deadliness (%)   ", deadliness_conversion[show_m_todam], 
			17, 1, TERM_L_BLUE);
	else
		prt_num("Deadliness (%)   ", -deadliness_conversion[-show_m_todam], 
			17, 1, TERM_L_BLUE);
  
	/* Dump the shooting bonuses to hit/dam */
  
	put_str("       (Shooting)    ", 14, 26);

	prt_deci("Shots/Round   ", p_ptr->num_fire/10, p_ptr->num_fire%10, 
		 15, 26, TERM_L_BLUE);

	prt_num("+ to Skill      ", show_a_tohit, 16, 26, TERM_L_BLUE);

	if (show_a_todam > 0)
		prt_num("Deadliness (%)  ", deadliness_conversion[show_a_todam], 
			17, 26, TERM_L_BLUE);
	else
		prt_num("Deadliness (%)  ", -deadliness_conversion[-show_a_todam], 
			17, 26, TERM_L_BLUE);

	/* Dump the base and bonus armor class */
	put_str("AC/+ To AC", 21, 26);
  
	sprintf(tmp, "%3d", p_ptr->dis_ac);
	c_put_str(TERM_L_BLUE, tmp, 21, 41);
  
	put_str("/", 21, 44);
  
	sprintf(tmp, "%3d", p_ptr->dis_to_a);
	c_put_str(TERM_L_BLUE, tmp, 21, 45);
  
	prt_num("Level            ", (int)p_ptr->lev, 19, 1, TERM_L_GREEN);

	prt_lnum("Experience    ", p_ptr->exp, 20, 1, TERM_L_GREEN);
  
	prt_lnum("Max Exp       ", p_ptr->max_exp, 21, 1, TERM_L_GREEN);
  
	prt_lnum("Exp to Adv.  ", (s32b)(player_exp[p_ptr->lev - 1]), 19, 26, 
		 TERM_L_GREEN);
      
	prt_lnum("Gold         ", p_ptr->au, 20, 26, TERM_L_GREEN);

	prt_num("Max Hit Points   ", p_ptr->mhp, 11, 1, TERM_L_GREEN);
  
	prt_num("Cur Hit Points   ", p_ptr->chp, 12, 1, TERM_L_GREEN);
  
  
	prt_num("Max SP (Mana)   ", p_ptr->msp, 11, 26, TERM_L_GREEN);
  
	prt_num("Cur SP (Mana)   ", p_ptr->csp, 12, 26, TERM_L_GREEN);


}

/* 
 * Get the right colour for a given resistance
 */
byte resist_colour(int resist_value)
{
	if (resist_value < RES_LEVEL_BASE) return TERM_RED;
	else if (resist_value == RES_LEVEL_BASE) return TERM_YELLOW;
	else if (resist_value <= RES_LEVEL_BASE + RES_BOOST_GOOD) return TERM_WHITE;
	else return TERM_L_GREEN;
}


/* Max length of note output */
#define LINEWRAP        75

/* 
 * Make a char_attr array of character information for file dump or screen
 * reading.  Mode 0 and 1 show 24 lines of player info for minor windows.
 */
extern int make_dump(char_attr_line *line, int mode)
{
	int i, j, x, y, col;
	bool quiver_empty = TRUE;
  
	cptr paren = ")";
  
	store_type *st_ptr = NULL;
  
	char o_name[120];
  
	char buf[100];
	char buf1[20];
	char buf2[20];

	bool red;

	int show_m_tohit = p_ptr->dis_to_h;
	int show_a_tohit = p_ptr->dis_to_h;
	int show_m_todam = p_ptr->dis_to_d;
	int show_a_todam = p_ptr->dis_to_d;
  
	object_type *o_ptr;
	int value;

	int tmp1;
	int xthn, xthb, xfos, xsrh;
	int xdis, xdev, xsav, xstl;
	cptr desc;

	int n, res_val = RES_LEVEL_BASE, res_num = 0;
  
	u32b flag;
	cptr name1;
  
	u32b f[4];

	int current_line = 0;

	bool dead = FALSE;

	/* Activate the home */
	st_ptr = &store[STORE_HOME];

	dump_ptr = (char_attr *)&line[current_line]; 

	/* Hack - skip all this for mode 1 */
	if (mode != 1)
	{  
		/* Begin dump */
		sprintf(buf, "[Oangband %s Character Dump]", VERSION_STRING);
		dump_put_str(TERM_WHITE, buf, 2);
		current_line++;
      
		/* Start of player screen mode 0 */
		if (mode == 0) 
		{
			current_line = 0;
          
			/* Hack - clear the top line */
			dump_put_str(TERM_WHITE, "", 0);
		}
      
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
      
		/* Name, Sex, Race, Class */
		dump_put_str(TERM_WHITE, "Name    : ", 1);
		dump_put_str(TERM_L_BLUE, op_ptr->full_name, 11);
		dump_put_str(TERM_WHITE, "Age", 27);
		sprintf(buf1, "%10d", (int)p_ptr->age);
		dump_put_str(TERM_L_BLUE, buf1, 42);
		red = (p_ptr->stat_cur[0] < p_ptr->stat_max[0]);
		value = p_ptr->stat_use[0];
		cnv_stat(value, buf1);
		value = p_ptr->stat_top[0];
		cnv_stat(value, buf2);
		dump_put_str(TERM_WHITE, (red ? "Str" : "STR"), 53);
		dump_put_str(TERM_WHITE, ((p_ptr->stat_cur[0] == 18 + 100) ? "!" : " "), 56);
		if (red)
		{
			dump_put_str(TERM_YELLOW, buf1, 61);
			dump_put_str(TERM_L_GREEN, buf2, 70);
		}
		else
			dump_put_str(TERM_L_GREEN, buf1, 61);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "Sex     : ", 1);
		dump_put_str(TERM_L_BLUE, sp_ptr->title, 11);
		dump_put_str(TERM_WHITE, "Height", 27);
		sprintf(buf1, "%10d", 
			(use_metric ? ((int)p_ptr->ht) * 254 / 100 : (int)p_ptr->ht));
		dump_put_str(TERM_L_BLUE, buf1, 42);
		red = (p_ptr->stat_cur[1] < p_ptr->stat_max[1]);
		value = p_ptr->stat_use[1];
		cnv_stat(value, buf1);
		value = p_ptr->stat_top[1];
		cnv_stat(value, buf2);
		dump_put_str(TERM_WHITE, (red ? "Int" : "INT"), 53);
		dump_put_str(TERM_WHITE, ((p_ptr->stat_cur[1] == 18 + 100) ? "!" : " "), 56);
		if (red)
		{
			dump_put_str(TERM_YELLOW, buf1, 61);
			dump_put_str(TERM_L_GREEN, buf2, 70);
		}
		else
			dump_put_str(TERM_L_GREEN, buf1, 61);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "Race    : ", 1);
		dump_put_str(TERM_L_BLUE, p_name + rp_ptr->name, 11);
		dump_put_str(TERM_WHITE, "Weight", 27);
		sprintf(buf1, "%10d", 
			(use_metric ? ((int)p_ptr->wt) * 10 / 22 : (int)p_ptr->wt)); 
		dump_put_str(TERM_L_BLUE, buf1, 42);
		red = (p_ptr->stat_cur[2] < p_ptr->stat_max[2]);
		value = p_ptr->stat_use[2];
		cnv_stat(value, buf1);
		value = p_ptr->stat_top[2];
		cnv_stat(value, buf2);
		dump_put_str(TERM_WHITE, (red ? "Wis" : "WIS"), 53);
		dump_put_str(TERM_WHITE, ((p_ptr->stat_cur[2] == 18 + 100) ? "!" : " "), 56);
		if (red)
		{
			dump_put_str(TERM_YELLOW, buf1, 61);
			dump_put_str(TERM_L_GREEN, buf2, 70);
		}
		else
			dump_put_str(TERM_L_GREEN, buf1, 61);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "Class   : ", 1);
		dump_put_str(TERM_L_BLUE, c_name + cp_ptr->name, 11);
		dump_put_str(TERM_WHITE, "Social Class", 27);
		sprintf(buf1, "%10d", (int)p_ptr->sc);
		dump_put_str(TERM_L_BLUE, buf1, 42);
		red = (p_ptr->stat_cur[3] < p_ptr->stat_max[3]);
		value = p_ptr->stat_use[3];
		cnv_stat(value, buf1);
		value = p_ptr->stat_top[3];
		cnv_stat(value, buf2);
		dump_put_str(TERM_WHITE, (red ? "Dex" : "DEX"), 53);
		dump_put_str(TERM_WHITE, ((p_ptr->stat_cur[3] == 18 + 100) ? "!" : " "), 56);
		if (red)
		{
			dump_put_str(TERM_YELLOW, buf1, 61);
			dump_put_str(TERM_L_GREEN, buf2, 70);
		}
		else
			dump_put_str(TERM_L_GREEN, buf1, 61);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		if (p_ptr->total_winner) 
			dump_put_str(TERM_VIOLET, "***WINNER***", 0);
		red = (p_ptr->stat_cur[4] < p_ptr->stat_max[4]);
		value = p_ptr->stat_use[4];
		cnv_stat(value, buf1);
		value = p_ptr->stat_top[4];
		cnv_stat(value, buf2);
		dump_put_str(TERM_WHITE, (red ? "Con" : "CON"), 53);
		dump_put_str(TERM_WHITE, ((p_ptr->stat_cur[4] == 18 + 100) ? "!" : " "), 56);
		if (red)
		{
			dump_put_str(TERM_YELLOW, buf1, 61);
			dump_put_str(TERM_L_GREEN, buf2, 70);
		}
		else
			dump_put_str(TERM_L_GREEN, buf1, 61);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		red = (p_ptr->stat_cur[5] < p_ptr->stat_max[5]);
		value = p_ptr->stat_use[5];
		cnv_stat(value, buf1);
		value = p_ptr->stat_top[5];
		cnv_stat(value, buf2);
		dump_put_str(TERM_WHITE, (red ? "Chr" : "CHR"), 53);
		dump_put_str(TERM_WHITE, ((p_ptr->stat_cur[5] == 18 + 100) ? "!" : " "), 56);
		if (red)
		{
			dump_put_str(TERM_YELLOW, buf1, 61);
			dump_put_str(TERM_L_GREEN, buf2, 70);
		}
		else
			dump_put_str(TERM_L_GREEN, buf1, 61);
		current_line +=2;
      
		/* Get the bonuses to hit/dam */
      
		o_ptr = &inventory[INVEN_WIELD];
		if (object_known_p(o_ptr)) show_m_tohit += o_ptr->to_h;
		if (object_known_p(o_ptr)) show_m_todam += o_ptr->to_d;
      
		o_ptr = &inventory[INVEN_BOW];
		if (object_known_p(o_ptr)) show_a_tohit += o_ptr->to_h;
		if (object_known_p(o_ptr)) show_a_todam += o_ptr->to_d;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_num("Max Hit Points   ", p_ptr->mhp, 1, TERM_L_GREEN);
		if (p_ptr->lev >= p_ptr->max_lev)
			dump_num("Level            ", (int)p_ptr->lev, 27, TERM_L_GREEN);
		else
			dump_num("Level            ", (int)p_ptr->lev, 27, TERM_YELLOW);
		dump_num("Max SP (Mana)    ", p_ptr->msp, 53, TERM_L_GREEN);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		if (p_ptr->chp >= p_ptr->mhp)
			dump_num("Cur Hit Points   ", p_ptr->chp, 1, TERM_L_GREEN);
		else if (p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
			dump_num("Cur Hit Points   ", p_ptr->chp, 1, TERM_YELLOW);
		else
			dump_num("Cur Hit Points   ", p_ptr->chp, 1, TERM_RED);
		if (p_ptr->exp >= p_ptr->max_exp)
			dump_lnum("Experience    ", p_ptr->exp, 27, TERM_L_GREEN);
		else
			dump_lnum("Experience    ", p_ptr->exp, 27, TERM_YELLOW);
		if (p_ptr->csp >= p_ptr->msp)
			dump_num("Cur SP (Mana)    ", p_ptr->csp, 53, TERM_L_GREEN);
		else if (p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
			dump_num("Cur SP (Mana)    ", p_ptr->csp, 53, TERM_YELLOW);
		else
			dump_num("Cur SP (Mana)    ", p_ptr->csp, 53, TERM_RED);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_lnum("Max Exp       ", p_ptr->max_exp, 27, TERM_L_GREEN);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "(Fighting)", 8);
		if (p_ptr->lev >= PY_MAX_LEVEL)
		{
			dump_put_str(TERM_WHITE, "Exp to Adv.   ", 27);
			sprintf(buf, "       *****");
		}
		else
			dump_lnum("Exp to Adv.   ", (s32b)(player_exp[p_ptr->lev - 1]), 27, 
				  TERM_L_GREEN);
		dump_put_str(TERM_WHITE, "(Shooting)", 60);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_num("Blows/Round      ", p_ptr->num_blow, 1, TERM_L_BLUE);
		dump_lnum("Gold          ", p_ptr->au, 27, TERM_L_GREEN);
		dump_deci("Shots/Round    ", p_ptr->num_fire/10, p_ptr->num_fire%10, 53, 
			  TERM_L_BLUE);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_num("+ to Skill       ", show_m_tohit, 1, TERM_L_BLUE);
		dump_lnum("Max Depth     ", p_ptr->max_depth, 27, TERM_L_GREEN);
		dump_num("+ to Skill       ", show_a_tohit, 53, TERM_L_BLUE);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		/* Show damage per blow if no weapon wielded */
		if (!inventory[INVEN_WIELD].k_idx)
		{
			int sum = 0;

			for (i = 0; i < 12; i++) sum += (int)p_ptr->barehand_dam[i];
			dump_num("Av. Damage/Blow  ", sum/12, 1, TERM_L_BLUE);
		}
		else if (show_m_todam >= 0)
			dump_num("Deadliness (%)   ", deadliness_conversion[show_m_todam], 
				 1, TERM_L_BLUE);
		else
			dump_num("Deadliness (%)   ", -deadliness_conversion[-show_m_todam], 
				 1, TERM_L_BLUE);
		if (show_a_todam > 0)
			dump_num("Deadliness (%)   ", deadliness_conversion[show_a_todam], 
				 53, TERM_L_BLUE);
		else
			dump_num("Deadliness (%)   ", -deadliness_conversion[-show_a_todam], 
				 53, TERM_L_BLUE);
		dump_put_str(TERM_WHITE, "Base AC/+ To AC", 27);
		sprintf(buf1, "%3d", p_ptr->dis_ac);
		dump_put_str(TERM_L_BLUE, buf1, 43);
		dump_put_str(TERM_WHITE, "/", 46);
		sprintf(buf1, "%3d", p_ptr->dis_to_a);
		dump_put_str(TERM_L_BLUE, buf1, 47);

		current_line++;
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "Game Turn", 27);
		sprintf(buf1, "%12d", (int)turn);
		dump_put_str(TERM_L_BLUE, buf1, 38);
		current_line += 2;
      
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "(Character Abilities)", 28);
      
      
		/* Fighting Skill (with current weapon) */
		o_ptr = &inventory[INVEN_WIELD];
		tmp1 = p_ptr->to_h + o_ptr->to_h;
		xthn = p_ptr->skill_thn + (tmp1 * BTH_PLUS_ADJ);
      
		/* Shooting Skill (with current bow and normal missile) */
		o_ptr = &inventory[INVEN_BOW];
		tmp1 = p_ptr->to_h + o_ptr->to_h;
		xthb = p_ptr->skill_thb + (tmp1 * BTH_PLUS_ADJ);
      
		/* Basic abilities */
		xdis = p_ptr->skill_dis;
		xdev = p_ptr->skill_dev;
		xsav = p_ptr->skill_sav;
		xstl = p_ptr->skill_stl;
		xsrh = p_ptr->skill_srh;
		xfos = p_ptr->skill_fos;
      
      
		desc = likert(xthn, 10);
		dump_put_str(TERM_WHITE, "Fighting    :", 1);
		dump_put_str(likert_color, desc, 15);
      
		desc = likert(xstl, 1);
		dump_put_str(TERM_WHITE, "Stealth     :", 27);
		dump_put_str(likert_color, desc, 41);
      
		desc = likert(xdis, 8);
		dump_put_str(TERM_WHITE, "Disarming   :", 53);
		dump_put_str(likert_color, desc, 67);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		desc = likert(xthb, 10);
		dump_put_str(TERM_WHITE, "Bows/Throw  :", 1);
		dump_put_str(likert_color, desc, 15);
      
		desc = likert(xfos, 6);
		dump_put_str(TERM_WHITE, "Perception  :", 27);
		dump_put_str(likert_color, desc, 41);
      
		desc = likert(xdev, 8);
		dump_put_str(TERM_WHITE, "Magic Device:", 53);
		dump_put_str(likert_color, desc, 67);
		current_line++;
      
		dump_ptr = (char_attr *)&line[current_line];
		desc = likert(xsav, 7);
		dump_put_str(TERM_WHITE, "Saving Throw:", 1);
		dump_put_str(likert_color, desc, 15);
      
		desc = likert(xsrh, 6);
		dump_put_str(TERM_WHITE, "Searching   :", 27);
		dump_put_str(likert_color, desc, 41);
      
		if (use_metric)
			sprintf(buf1, "%d meters", p_ptr->see_infra * 3);
		else  
			sprintf(buf1, "%d feet", p_ptr->see_infra * 10);
		dump_put_str(TERM_WHITE, "Infravision :", 53);
		dump_put_str(TERM_WHITE, buf1, 67);
		current_line += 2;
      
      
		/* Display history */
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "(Character Background)", 28);
      
		for (i = 0; i < 4; i++)
		{
			current_line++;
			dump_ptr = (char_attr *)&line[current_line];
			dump_put_str(TERM_WHITE, p_ptr->history[i], 10);
		}
      
		/* End of mode 0 */
		if (mode == 0) return (current_line);
      
		current_line += 2;
      
		/* Heading */
		current_line++;
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "[Resistances and Powers]", 2);
		current_line += 2;
	}
      
	/* Start of mode 1 player screen */
	if (mode == 1)
	{
		current_line = 0;

		/* Hack - clear all the lines */
		for (i = 0; i < 25; i++)
		{
			dump_ptr = (char_attr *)&line[i];
			dump_put_str(TERM_WHITE, "", 0);
		}
	}

	dump_ptr = (char_attr *)&line[current_line];
  
	/* Header */
	dump_put_str(TERM_WHITE, "abcdefghijkl@            abcdefghijkl@", 6);
	current_line++;

	/* Resistances */
	for (y = 0; y < 8; y++)
	{
		dump_ptr = (char_attr *)&line[current_line];
      
		for (x = 0; x < 2; x++)
		{
			bool percentage = TRUE;

			/* Hack - Fear and Blindness */
			if ((y == 5) && (x == 0)) 
			{
				percentage = FALSE;
			}
			else if ((y == 0) && (x == 1)) 
			{
				percentage = FALSE;
			}
			else
			{
				res_num = x * 7 + y;
				if ((x == 0) && (y > 4)) res_num--;
				if (x == 1) res_num--;
				res_val = p_ptr->dis_res_list[res_num];
				percentage = TRUE;
			}

			/* Extract flag */
			flag = display_player_resists[x][y];
      
			/* Extract name */
			name1 = display_player_resist_names[x][y];

			/* Name */
			dump_put_str(TERM_WHITE, name1, 1 + 24 * x);

			/* Check equipment */
			for (n = 6 + 25 * x, i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++, n++)
			{
				object_type *o_ptr;
              
				/* Object */
				o_ptr = &inventory[i];
              
				/* Known flags */
				object_flags_known(o_ptr, &f[1], &f[2], &f[3]);
              
				/* Hack -- Check immunities */
				if ((y < 4) && (x == 0) && (f[2] & ((TR2_IM_ACID) << y)))
				{
					dump_put_str(TERM_WHITE, "*", n);
				}
              
				/* Check flags */
				else if (f[2] & flag)
				{
					dump_put_str(TERM_L_BLUE, "+", n);
				}
	      
				/* Hack -- Check minor resist to low elements */
				else if ((x == 0) && (y < 4) &&
					 (f[2] & TR2_RES_BASE_MINOR))
				{
					dump_put_str(TERM_SLATE, "+", n);
				}
	      
				/* Default */
				else
				{
					dump_put_str((o_ptr->k_idx ? TERM_L_WHITE : TERM_L_DARK), ".", n);
				}

			}
          
			/* Player flags */
			player_flags(&f[1], &f[2], &f[3], TRUE);
          
			/* Check flags */
			if (f[2] & flag) 
			{
				dump_put_str(TERM_L_BLUE, "+", n);
			}
          
			else
			{
				/* Player 'reversed' flags */
				player_weakness_dis(&f[1], &f[2], &f[3]);
              
				/* Check flags */
				if (f[2] & flag) 
				{
					dump_put_str(TERM_RED, "-", n);
				}
              
				else
					/* Default */
					dump_put_str(TERM_L_WHITE, ".", n);
			}
          
			/* Percentage */
			if (percentage)
			{
				sprintf(buf1, "%4d%%", extract_resistance[res_val]);
				dump_put_str(resist_colour(res_val), buf1, n + 1);
			}
		}         
		current_line++;
	}
	/* Skip the gap for mode 1 */
	if (mode != 1)
	{
		current_line ++;
		dump_ptr = (char_attr *)&line[current_line];
  
		/* Header */
		dump_put_str(TERM_WHITE, "abcdefghijkl@            abcdefghijkl@", 6);
		current_line++;
	}

	/* Powers */
	for (y = 0; y < 8; y++)
	{
		dump_ptr = (char_attr *)&line[current_line];
      
		for (x = 0; x < 2; x++)
		{
			/* Extract flag */
			flag = display_player_flags[x][y];
      
			/* Extract name */
			name1 = display_player_flag_names[x][y];

			/* Name */
			dump_put_str(TERM_WHITE, name1, 25 * x);

			/* Check equipment */
			for (n = 6 + 25 * x, i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++, n++)
			{
				object_type *o_ptr;
              
				/* Object */
				o_ptr = &inventory[i];
              
				/* Known flags */
				object_flags_known(o_ptr, &f[1], &f[2], &f[3]);
              
				/* Check flags */
				if (f[3 - 2 * x] & flag)
				{
					dump_put_str(TERM_WHITE, "+", n);
				}
              
				/* Default */
				else
				{
					dump_put_str((o_ptr->k_idx ? TERM_L_WHITE : TERM_SLATE), 
						     ".", n);
				}
			}
          
			/* Player flags */
			player_flags(&f[1], &f[2], &f[3], TRUE);
          
			/* Check flags */
			if (f[3 - 2 * x] & flag) 
			{
				dump_put_str(TERM_WHITE, "+", n);
			}
          
			else
			{
				/* Player 'reversed' flags */
				player_weakness_dis(&f[1], &f[2], &f[3]);
              
				/* Check flags */
				if (f[3 - 2 * x] & flag) 
				{
					dump_put_str(TERM_RED, "-", n);
				}
              
				else
					/* Default */
					dump_put_str(TERM_L_WHITE, ".", n);
			}
		}         
		current_line++;
	}

	/* Skip for mode 1 */
	if (mode != 1)
	{
		/* Skip some lines */
		current_line += 2;
      
		/* Dump specialties if any */
		if (p_ptr->specialty_order[0] != SP_NO_SPECIALTY)
		{
			dump_ptr = (char_attr *)&line[current_line];
			dump_put_str(TERM_WHITE, "[Specialty Abilities]", 2);
			current_line += 2;
          
			for (i = 0; i < 10; i++)
			{
				if (p_ptr->specialty_order[i] != SP_NO_SPECIALTY)
				{
					dump_ptr = (char_attr *)&line[current_line];
					sprintf(buf, "%s %s", 
						specialty_names[p_ptr->specialty_order[i]],
						(i >= p_ptr->specialties_allowed) ? "(forgotten)" : "");
					dump_put_str(TERM_GREEN, buf, 0);
					current_line++; 
				}
			}
			current_line += 2;
		}
  
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "[Stat Breakdown]", 2);
		current_line += 2;
	}
  
	dump_ptr = (char_attr *)&line[current_line];
  
  
	/* Print out the labels for the columns */
	dump_put_str(TERM_WHITE, "Stat", 0);
	dump_put_str(TERM_BLUE, "Intrnl", 5);
	dump_put_str(TERM_L_BLUE, "Rce Cls Oth", 12);
	dump_put_str(TERM_L_GREEN, "Actual", 24);
	dump_put_str(TERM_YELLOW, "Currnt", 31);
	dump_put_str(TERM_WHITE, "abcdefghijkl@", 42);
	current_line++;
  
	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		byte a = TERM_SLATE;
		char c = '.';
		u32b f1, f2, f3;

		dump_ptr = (char_attr *)&line[current_line];

		/* Reduced name of stat */
		dump_put_str(TERM_WHITE, stat_names_reduced[i], 0);
      
		/* Internal "natural" maximum value */
		cnv_stat(p_ptr->stat_max[i], buf);
		dump_put_str(TERM_BLUE, buf, 5);
      
		/* Race, class, and equipment modifiers */
		sprintf(buf, "%3d", rp_ptr->r_adj[i]);
		dump_put_str(TERM_L_BLUE, buf, 12);
		sprintf(buf, "%3d", cp_ptr->c_adj[i]);
		dump_put_str(TERM_L_BLUE, buf, 16);
		sprintf(buf, "%3d", p_ptr->stat_add[i]);
		dump_put_str(TERM_L_BLUE, buf, 20);
      
		/* Resulting "modified" maximum value */
		cnv_stat(p_ptr->stat_top[i], buf);
		dump_put_str(TERM_L_GREEN, buf, 24);
      
		/* Only display stat_use if not maximal */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			cnv_stat(p_ptr->stat_use[i], buf);
			dump_put_str(TERM_YELLOW, buf, 31);
		}

		for (j = INVEN_WIELD; j < INVEN_SUBTOTAL; j++)
		{
			s16b k_idx;
			u32b ignore_f2, ignore_f3;

			col = 42 + j - INVEN_WIELD;
          
			/* Access object */
			o_ptr = &inventory[j];
          
			/* Object kind */
			k_idx = o_ptr->k_idx;
          
			/* Acquire "known" flags */
			object_flags_known(o_ptr, &f1, &f2, &f3);
      
			/* Hack -- assume stat modifiers are known */
			object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3);

			/* Initialize color based of sign of pval. */
			a = TERM_SLATE;
			c = '.';
          
			/* Boost */
			if (f1 & (1 << i))
			{
				/* Default */
				c = '*';
              
				/* Good */
				if (o_ptr->pval > 0)
				{
					/* Good */
					a = TERM_L_GREEN;
                  
					/* Label boost */
					if (o_ptr->pval < 10) c = '0' + o_ptr->pval;
				}
              
				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Bad */
					a = TERM_RED;
                  
					/* Label boost */
					if (o_ptr->pval < 10) c = '0' - o_ptr->pval;
				}
			}
          
			/* Sustain */
			if (f2 & (1 << i))
			{
				/* Dark green, "s" if no stat bonus. */
				a = TERM_GREEN;
				if (c == '.') c = 's';
			}
          
			/* Dump proper character */
			buf[0] = c;
			buf[1] = '\0';
			dump_put_str(a, buf, col);
		}

		/* Specialty stat boosts */
		col++;
		a = TERM_SLATE;
		c = '.';

		/* Player flags */
		player_flags(&f1, &f2, &f3, TRUE);

          
		/* Sustain */
		if (f2 & 1<<i)
		{
			/* Dark green "s" */
			a = TERM_GREEN;
			c = 's';
		}
      

		/* Boost */
		if (check_ability(SP_CLARITY))
		{
			if ((i == A_INT) || (i == A_WIS))
			{
				/* Good */
				a = TERM_L_GREEN;
				c = '2';
			}
		}
      
		if (check_ability(SP_ATHLETICS))
		{
			if ((i == A_DEX) || (i == A_CON))
			{
				/* Good */
				a = TERM_L_GREEN;
				c = '2';
			}
		}
		/* Dump proper character */
		buf[0] = c;
		buf[1] = '\0';
		dump_put_str(a, buf, col);
      
		current_line++;
	}
  
	/* End of mode 1 */
	if (mode == 1) return (current_line);
  
	current_line++;  
  
	/* If dead, dump last messages -- Prfnoff */
	if (p_ptr->is_dead)
	{
		dump_ptr = (char_attr *)&line[current_line];
		i = message_num();
		if (i > 15) i = 15;
		dump_put_str(TERM_WHITE, "[Last Messages]", 2);
		current_line += 2;
		while (i-- > 0)
		{
			dump_ptr = (char_attr *)&line[current_line];
			sprintf(buf, "> %s", message_str((s16b)i));
			dump_put_str(TERM_WHITE, buf, 0);
			current_line++;
		}
		current_line += 2;
	}
  
	/* Dump the equipment */
	if (p_ptr->equip_cnt)
	{
		dump_ptr = (char_attr *)&line[current_line];
		dump_put_str(TERM_WHITE, "[Character Equipment]", 2);
		current_line += 2;
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			if (i == INVEN_BLANK) 
			{
				for (j = 0; j < 10; j++)
				{
					object_desc(o_name, &inventory[i + j + 1], TRUE, 4);
					if (!streq(o_name, "(nothing)")) 
						quiver_empty = FALSE;
				}
				if (!quiver_empty) 
				{
					current_line++;
					dump_ptr = (char_attr *)&line[current_line];
					dump_put_str(TERM_WHITE, "[Quiver]", 9);
					current_line++;
				}
			}         
          
			else
			{
				dump_ptr = (char_attr *)&line[current_line];
				object_desc(o_name, &inventory[i], TRUE, 4);
				if (streq(o_name,"(nothing)")) continue;
				sprintf(buf, "%c%s %s", index_to_label(i), paren, o_name);
				dump_put_str(proc_list_color_hack(&inventory[i]), buf, 0);
				current_line++;
			}
		}
		current_line += 2;
	}
  
	/* Dump the inventory */
	dump_ptr = (char_attr *)&line[current_line];
	dump_put_str(TERM_WHITE, "[Character Inventory]", 2);
	current_line += 2;
	for (i = 0; i < INVEN_PACK; i++)
	{
		if (!inventory[i].k_idx) break;
      
		dump_ptr = (char_attr *)&line[current_line];
		object_desc(o_name, &inventory[i], TRUE, 4);
		sprintf(buf, "%c%s %s", index_to_label(i), paren, o_name);
		dump_put_str(proc_list_color_hack(&inventory[i]), buf, 0);
		current_line++;
    
	}
	current_line += 2;  
  
	/* Dump the Home -- if anything there */
	if (st_ptr->stock_num)
        {
		dump_ptr = (char_attr *)&line[current_line];
          
		/* Header */
		dump_put_str(TERM_WHITE, "[Home Inventory]", 2);
		current_line += 2;
          
		/* Dump all available items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
			dump_ptr = (char_attr *)&line[current_line];
			object_desc(o_name, &st_ptr->stock[i], TRUE, 4);
			sprintf(buf, "%c) %s", I2A(i), o_name);
			dump_put_str(proc_list_color_hack(&st_ptr->stock[i]), buf, 0);
			current_line++;
		}
          
		/* Add an empty line */
		current_line += 2;
        }
  
	/* Add in "character start" information */
	dump_ptr = (char_attr *)&line[current_line];
	sprintf(buf, "%s the %s %s", op_ptr->full_name,
		p_name + rp_ptr->name,
		c_name + cp_ptr->name);
	dump_put_str(TERM_WHITE, buf, 0);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];
	dump_put_str(TERM_WHITE, notes_start, 0);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];
	dump_put_str(TERM_WHITE, "============================================================", 0);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];
	dump_put_str(TERM_WHITE, "CHAR.", 24);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];
	dump_put_str(TERM_WHITE, "|   TURN  |  LOCATION  |LEVEL| EVENT", 
		     0);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];
	dump_put_str(TERM_WHITE, "============================================================", 0);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];

	/* Dump notes */
	i = 0;
	while (notes[i].turn)
	{
		int length, length_info;
		char info_note[43];
		char place[32];

		/* Paranoia */
		if ((notes[i].depth > 127) || (notes[i].level > 50) ||
		    (notes[i].type > 15))
		{
			i++;
			continue;
		}

		/* Divider before death */
		if ((notes[i].type == NOTE_DEATH) && (!dead))
		{
			dead = TRUE;
			dump_put_str(TERM_WHITE, "============================================================", 0);
			current_line++;
			dump_ptr = (char_attr *)&line[current_line];
		}

		/* Get the note */
		sprintf(buf, "%s", notes[i].note);
      
		/* Get the location name */
		if (notes[i].depth)
			strnfmt(place, sizeof(place), "Level%4d ", notes[i].depth);
		else
			strnfmt(place, sizeof(place), "      Town");
      
		/* Make preliminary part of note */
		strnfmt(info_note, sizeof(info_note), "|%9lu| %s | %2d  | ", 
			notes[i].turn, place, notes[i].level);
      
		/* Write the info note*/
		dump_put_str(TERM_WHITE, info_note, 0);
      
		/* Get the length of the notes */
		length_info = strlen(info_note);
		length = strlen(buf);
      
		/* Break up long notes */
		if ((length + length_info) > LINEWRAP)
		{
			bool keep_going = TRUE;
			int startpoint = 0;
			int endpoint, n;
          
			while (keep_going)
			{
				/* Don't print more than the set linewrap amount */
				endpoint = startpoint + LINEWRAP - strlen(info_note) + 1;
              
				/* Find a breaking point */
				while (TRUE)
				{
					/* Are we at the end of the line? */
					if (endpoint >= length)
					{
						/* Print to the end */
						endpoint = length;
						keep_going = FALSE;
						break;
					}
                  
					/* Mark the most recent space or dash in the string */
					else if ((buf[endpoint] == ' ') ||
						 (buf[endpoint] == '-')) break;
              
					/* No spaces in the line, so break in the middle of text */
					else if (endpoint == startpoint)
					{
						endpoint = startpoint + LINEWRAP - strlen(info_note) + 1;
						break;
					}
                  
					/* check previous char */
					endpoint--;
				}
              
				/* Make a continued note if applicable */
				if (startpoint) 
					dump_put_str(TERM_WHITE, 
						     "|  continued...                  |     |  ", 0);
              
				/* Write that line to file */
				for (n = startpoint; n <= endpoint; n++)
				{
					char ch;
                  
					/* Ensure the character is printable */
					ch = (isprint(buf[n]) ? buf[n] : ' ');
                  
					/* Write out the character */
					sprintf(buf1, "%c", ch);
					dump_put_str(notes[i].type, buf1, 
						     strlen(info_note) + n - startpoint);
                  
				}
              
				/* Break the line */
				current_line++;
				dump_ptr = (char_attr *)&line[current_line];
              
				/* Prepare for the next line */
				startpoint = endpoint + 1;
              
			}
		}
  
		/* Add note to buffer */
		else
		{
			/* Print the note */
			dump_put_str(notes[i].type, buf, strlen(info_note));
          
			/* Break the line */
			current_line++;
			dump_ptr = (char_attr *)&line[current_line];
		}

		/* Next note */
		i++;
	}
  

	dump_put_str(TERM_WHITE, "============================================================", 0);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];

	/* Fake note */
	if (!dead)
	{
		char info_note[43];
		char place[32];

		/* Get the location name */
		if (p_ptr->depth)
			strnfmt(place, sizeof(place), "Level%4d ", p_ptr->depth);
		else
			strnfmt(place, sizeof(place), "      Town");
      
		/* Make preliminary part of note */
		strnfmt(info_note, sizeof(info_note), "|%9lu| %s | %2d  | ", 
			turn, place, p_ptr->lev);
      
		/* Write the info note */
		dump_put_str(TERM_WHITE, info_note, 0);
		dump_put_str(NOTE_DEATH, " Still alive", strlen(info_note));

		current_line++;
		dump_ptr = (char_attr *)&line[current_line];
	}
	dump_put_str(TERM_WHITE, "============================================================", 0);
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];
    
  
  
	/* Dump options */
	current_line++;
	dump_ptr = (char_attr *)&line[current_line];
	dump_put_str(TERM_WHITE, "[Options]", 2);
	current_line += 2;

	/* Dump options */
	for (i = OPT_adult_start + 5; i < OPT_adult_end; i++)
	{
		if (option_desc[i])
		{
			dump_ptr = (char_attr *)&line[current_line];
			sprintf(buf, "%-49s: %s (%s)",
				option_desc[i],
				op_ptr->opt[i] ? "yes" : "no ",
				option_text[i]);
			dump_put_str(TERM_WHITE, buf, 0);
			current_line++;
		}
	}

	for (i = OPT_score_start; i < OPT_score_end; i++)
	{
		if (option_desc[i])
		{
			dump_ptr = (char_attr *)&line[current_line];
			sprintf(buf, "%-49s: %s (%s)",
				option_desc[i],
				op_ptr->opt[i] ? "yes" : "no ",
				option_text[i]);
			dump_put_str(TERM_WHITE, buf, 0);
			current_line++;
		}
	}

	/* Success */
	return (current_line);
}

void display_dump(char_attr_line *line, int top_line, int bottom_line, int col)
{
	int i;
	char_attr *shifted_line;

	/* Start at the top */
	dump_row = 0;

	/* Set the hook */
	dump_line_hook = dump_line_screen;

	/* Dump the lines */
	for (i = top_line; i < bottom_line; i++)
	{
		shifted_line = (char_attr *) &line[i];
		shifted_line += col;
		dump_line(shifted_line);
	}
}

errr file_character(cptr name, char_attr_line *line, int last_line)
{
	int i;
	int fd = -1;
	char buf[80];
  
	/* Drop priv's */
	safe_setuid_drop();

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, name);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Check for existing file */
	fd = fd_open(buf, O_RDONLY);
  
	/* Existing file */
#ifdef _WIN32_WCE
	if (fd != -1)
#else
		if (fd >= 0)
#endif
		{
			char out_val[160];
      
			/* Close the file */
			fd_close(fd);

			/* Build query */
			sprintf(out_val, "Replace existing file %s? ", buf);

			/* Ask */
			if (get_check(out_val)) fd = -1;
		}
  
	/* Open the non-existing file */
	if (fd < 0) dump_out_file = my_fopen(buf, "w");
  
	/* Grab priv's */
	safe_setuid_grab();
  
  
	/* Invalid file */
	if (!dump_out_file)
	{
		/* Message */
		msg_format("Character dump failed!");
		msg_print(NULL);

		/* Error */
		return (-1);
	}

	/* Set the hook */
	dump_line_hook = dump_line_file;

	/* Dump the lines */
	for (i = 0; i < last_line; i++)
	{
		dump_ptr = (char_attr *)&line[i];
		dump_line(dump_ptr);
	}
  
	/* Close it */
	my_fclose(dump_out_file);
  
	/* Message */
	msg_print("Character dump successful.");
	msg_print(NULL);

	/* Success */
	return (0);
}

/*
 * Make a string lower case.
 */
static void string_lower(char *buf)
{
	cptr buf_ptr;

	/* No string */
	if (!buf) return;

	/* Lower the string */
	for (buf_ptr = buf; *buf_ptr != 0; buf_ptr++)
	{
		buf[buf_ptr - buf] = tolower(*buf_ptr);
	}
}

/* Keep track of how recursed the file showing is */
static int push_file = 0;

/*
 * Recursive file perusal.
 *
 * Return FALSE on "ESCAPE", otherwise TRUE.
 *
 * Process various special text in the input file, including
 * the "menu" structures used by the "help file" system.
 *
 * XXX XXX XXX Consider using a temporary file.
 *
 * XXX XXX XXX Allow the user to "save" the current file.
 */

#define MAX_BUF   1024

bool show_file(cptr name, cptr what, int line, int mode)
{
	int i, k, n;
	int wid, hgt;
	int ret;
	event_type ke;
  
	/* Number of "real" lines passed by */
	int next = 0;

	/* Number of "real" lines in the file */
	int size = 0;

	/* Backup value for "line" */
	int back = 0;

	/* This screen has sub-screens */
	bool menu = FALSE;

	/* Case sensitive search */
	bool case_sensitive = FALSE;
  
	/* HACK!! -NRM- */
	ang_file *fff = NULL;
  
	/* Find this string (if any) */
	cptr find = NULL;
  
	/* Jump to this tag */
	cptr tag = NULL;

	/* Hold a string to find */
	char finder[81] = "";
  
	/* Hold a string to show */
	char shower[81] = "";
  
	/* Filename */
	char *filename;

	/* Describe this thing */
	char caption[128] = "";

	/* Path buffer */
	char *path;

	/* General buffer */
	char *buf;
  
	/* Small screen back up buffer */
	char *buf2;
  
	/* Lower case version of the buffer, for searching */
	char *lc_buf;
  
	/* Sub-menu information */
	char hook[10][32];

	/* Sub-menu mouse position */
	int mouse[24];
  
	/* Handle second half of screen */
	bool second_half = FALSE;
	bool line_finished = FALSE;

	/* Normal screen ? */
	bool old_normal_screen = FALSE;

	/* mallocs */
	filename = malloc(MAX_BUF);
	path     = malloc(MAX_BUF);
	buf      = malloc(MAX_BUF);
	buf2     = malloc(MAX_BUF);
	lc_buf   = malloc(MAX_BUF);

	/* Show messages first */
	if (easy_more) messages_easy(FALSE);

	/* Record normal screen if it's the first time in */
	if (!push_file) old_normal_screen = normal_screen;
 
	/* Get size */
	Term_get_size(&wid, &hgt);
  
	/* Wipe the hooks */
	for (i = 0; i < 10; i++) 
	{
		hook[i][0] = '\0';
	}

	/* Wipe the mouse menu */
	for (i = 0; i < 24; i++)
	{
		mouse[i] = 0;
	}
  
	/* Copy the filename */
	my_strcpy(filename, name, MAX_BUF);
  
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
		my_strcpy(caption, what, sizeof(caption));
      
		my_strcpy(path, name, MAX_BUF);
		fff = file_open(path, MODE_READ, -1);
	}
  
	/* Look in "help" */
	if (!fff)
	{
		strnfmt(caption, sizeof(caption), "Help file '%s'", name);
      
		path_build(path, MAX_BUF, ANGBAND_DIR_HELP, name);
		fff = file_open(path, MODE_READ, -1);
	}
  
	/* Look in "info" */
	if (!fff)
	{
		strnfmt(caption, sizeof(caption), "Info file '%s'", name);
      
		path_build(path, MAX_BUF, ANGBAND_DIR_INFO, name);
		fff = file_open(path, MODE_READ, -1);
	}
  
	/* Oops */
	if (!fff)
	{
		/* Message */
		msg_format("Cannot open '%s'.", name);
		message_flush();
      
		/* Oops */
		ret = TRUE;
		goto DONE;
	}

	/* Note we're entering the file */
	push_file++;  
  
	/* Pre-Parse the file */
	while (TRUE)
	{
		/* Read a line or stop */
		if (!file_getl(fff, buf, MAX_BUF)) break;
      
		/* Check for a mouseable line (note hex parentheses) */
		if ((buf[4] == 0x28) && (isdigit(buf[5])) && (buf[6] == 0x29))
		{
			mouse[next + 2] = D2I(buf[5]);
		}
      
		/* XXX Parse "menu" items */
		if (prefix(buf, "***** "))
		{
			char b1 = '[', b2 = ']';

			/* Notice "menu" requests */
			if ((buf[6] == b1) && isdigit(buf[7]) &&
			    (buf[8] == b2) && (buf[9] == ' '))
			{
				/* This is a menu file */
				menu = TRUE;

				/* Extract the menu item */
				k = D2I(buf[7]);

				/* Extract the menu item */
				strcpy(hook[k], buf + 10);
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
		char prompt[80];

		/* Clear screen */
		Term_clear();
      

		/* Restart when necessary */
		if (line >= size) line = 0;


		/* Re-open the file if needed */
		if (next > line)
		{
			/* Close it */
			file_close(fff);
          
			/* Hack -- Re-Open the file */
			fff = file_open(path, MODE_READ, -1);
			if (!fff) 
			{
				/* Leaving */
				push_file--;
				ret = TRUE;
				goto DONE;
			}
          
			/* File has been restarted */
			next = 0;
		}
      
		/* Goto the selected line */
		while (next < line)
		{

			/* Get a line */
			if (!file_getl(fff, buf, MAX_BUF)) break;
          
			/* Skip tags/links */
			if (prefix(buf, "***** ")) continue;
          
			/* Count the lines */
			next++;
		}
      
      
      
		/* Dump the next hgt - 4 lines of the file */
		for (i = 0; i < hgt - 4; )
		{
			/* Hack -- track the "first" line */
			if (!i) line = next;
          
			/* Get a line of the file or stop */
			if (!file_getl(fff, buf, MAX_BUF)) break;
          
			/* Hack -- skip "special" lines */
			if (prefix(buf, "***** ")) continue;

			/* Count the "real" lines */
			next++;
          
			/* Make a copy of the current line for searching */
			my_strcpy(lc_buf, buf, sizeof(lc_buf));
          
			/* Make the line lower case */
			if (!case_sensitive)
				string_lower(lc_buf);

			/* Hack -- keep searching */
			if (find && !i && !strstr(lc_buf, find)) continue;

			/* Hack -- stop searching */
			find = NULL;
          
			/* Check if the line is finished */
			for (k = 0; k < 32; k++)
				if (!buf[k]) line_finished = TRUE;

			/* Dump the line */
			if ((small_screen) && (second_half))
			{
				if (!line_finished)
				{
					for (k = 0; k < strlen(buf); k++)
					{
						buf2[k] = buf[k + 32];
					}
					Term_putstr(0, i+2, -1, TERM_WHITE, buf2);
				}
			}
			else
				Term_putstr(0, i+2, -1, TERM_WHITE, buf);
          
			/* Reset line */
			line_finished = FALSE;

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = lc_buf;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);
                  
					/* Display the match */
					Term_putstr(str-lc_buf, i+2, len, TERM_YELLOW, 
						    &buf[str-lc_buf]);
                  
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
		prt(format("[Oangband %s, %s, Line %d/%d]",
			   VERSION_STRING, caption, line, size), 0, 0);
      
      
		/* Buttons */
		if (push_file == 1) backup_buttons();
		kill_all_buttons();
		normal_screen = FALSE;
		add_button("ESC", ESCAPE);
		add_button("?", '?');
      
		/* Prompt -- menu screen */
		if (menu)
		{
			/* Wait for it */
			if (small_screen)
			{
				strncpy(prompt, 
					"[Number, 'h':other half, ESC:previous, ?:exit]", 80);
			}
			else
			{
				strncpy(prompt, 
					"[Press a number, ESC for previous file or ? to exit]", 
					80);
			}
		}
      
		/* Prompt -- small files */
		else if (size <= hgt - 4)
		{
			/* Wait for it */
			if (small_screen)
			{
				strncpy(prompt, "['h' other half, ESC previous file, ? exit]", 
					80);
			}
			else
			{
				strncpy(prompt, "[Press ESC for previous file, ? to exit.]", 80);
			}
		}
      
		/* Prompt -- large files */
		else
		{
			/* Wait for it */
			if (small_screen)
			{
				strncpy(prompt, "['h':other half,Space:advance,ESC:last,?:exit]",
					80); 
			}
			else
			{
				strncpy(prompt, "[Space to advance, ESC for previous file, or ? to exit.]", 
					80);
			}

			/* More buttons */
			add_button("Spc", ' ');
			add_button("-", '-');
		}

		/* Finish the status line */
		prt(prompt, hgt - 1, 0);
		prompt_end = (small_screen ? 0 : strlen(prompt));
		if (small_screen) add_button("h", 'h');
		add_button("/", '/');
		add_button("!", '!');
		add_button("=", '=');
		if (!menu) add_button("#", '#');
		add_button("%", '%');
		update_statusline();

		/* Get a keypress */
		ke = inkey_ex();

		/* Mouse input - menus */
		if ((ke.key == '\xff') && (menu) && (mouse[ke.mousey]))
		{
			/* Recurse on that file */
			if (!show_file(hook[mouse[ke.mousey]], NULL, 0, mode)) ke.key = '?';
		}
      
		/* Hack -- return to last screen on escape */
		if (ke.key == ESCAPE) break;
      
      
		/* Toggle case sensitive on/off */
		if (ke.key == '!')
		{
			case_sensitive = !case_sensitive;
		}
      
		/* Hack -- try showing */
		if (ke.key == '=')
		{
			/* Get "shower" */
			prt("Show: ", hgt - 1, 0);
			(void)askfor_aux(shower, 80, NULL);
          
			/* Make the "shower" lowercase */
			if (!case_sensitive)
				string_lower(shower);
		}
      
		/* Hack -- try finding */
		if (ke.key == '/')
		{
			/* Get "finder" */
			prt("Find: ", hgt - 1, 0);
			if (askfor_aux(finder, 80, NULL))
			{
				/* Find it */
				find = finder;
				back = line;
				line = line + 1;

				/* Make the "finder" lowercase */
				if (!case_sensitive)
					string_lower(finder);

				/* Show it */
				strcpy(shower, finder);
			}
		}
      
		/* Hack -- go to a specific line */
		if (ke.key == '#')
		{
			char tmp[81];
			prt("Goto Line: ", hgt - 1, 0);
			strcpy(tmp, "0");
			if (askfor_aux(tmp, 80, NULL))
			{
				line = atoi(tmp);
			}
		}
      
		/* Hack -- go to a specific file */
		if (ke.key == '%')
		{
			char tmp[81];
			prt("Goto File: ", hgt - 1, 0);
			strcpy(tmp, "help.hlp");
			if (askfor_aux(tmp, 80, NULL))
			{
				if (!show_file(tmp, NULL, 0, mode)) ke.key = '?';
			}
		}
      
		/* Back up one line */
		if (ke.key == ARROW_UP || ke.key == '8')
		{
			line = line - 1;
		}
      
		/* Hack -- Allow backing up */
		if ((ke.key == '-') || (ke.key == '9'))
		{
			line = line - 10;
			if (line < 0) line = 0;
		}
      
		/* Hack -- Advance a single line */
		if ((ke.key == '\n') || (ke.key == '\r') || (ke.key == '2') || 
		    (ke.key == ARROW_DOWN))
		{
			line = line + 1;
		}
      
		/* Switch to other page half */
		if ((small_screen) && (ke.key == 'h'))
		{
			second_half = !second_half;
		}
      
		/* Advance one page */
		if ((ke.key == ' ') || (ke.key == '3'))
		{
			line = line + hgt - 4;
		}
      
		/* Recurse on numbers */
		if (menu && isdigit(ke.key) && hook[D2I(ke.key)][0])
		{
			/* Recurse on that file */
			if (!show_file(hook[D2I(ke.key)], NULL, 0, mode)) ke.key = '?';
		}
      
		/* Exit on '?' */
		if (ke.key == '?') break;
	}

	/* Kill the buttons */
	if (push_file == 1)
	{ 
		restore_buttons();
		normal_screen = old_normal_screen;
		update_statusline();
	}

	/* Close the file */
	file_close(fff);
	push_file--;
  
	/* Normal return */
	ret = TRUE;

	/* Exit on '?' */
	if (ke.key == '?') ret = FALSE;

DONE:
	free(filename);
	free(path);
	free(buf);
	free(buf2);
	free(lc_buf);

	return ret;
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
 * Process the player name.
 * Extract a clean "base name".
 * Build the savefile name if needed.
 */
void process_player_name(bool sf)
{
	int i, k = 0;
  
  
	/* Cannot be too long */
	if (strlen(op_ptr->full_name) > 15)
	{
		/* Silently truncate */
		op_ptr->full_name[15] = '\0';
	}

	/* Cannot contain "icky" characters */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		/* No control characters */
		if (iscntrl(op_ptr->full_name[i]))
		{
                        /* Silently replace */
			op_ptr->full_name[i] = '_';
                }
        }


#ifdef MACINTOSH

	/* Extract "useful" letters */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		char c = op_ptr->full_name[i];

		/* Convert "colon" and "period" */
		if ((c == ':') || (c == '.')) c = '_';

		/* Accept all the letters */
		op_ptr->base_name[k++] = c;
	}

#else

	/* Extract "useful" letters */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		char c = op_ptr->full_name[i];

		/* Accept some letters */
		if (isalpha(c) || isdigit(c)) op_ptr->base_name[k++] = c;

		/* Convert space, dot, and underscore to underscore */
		else if (strchr(". _", c)) op_ptr->base_name[k++] = '_';
	}

#endif


#if defined(WINDOWS) || defined(MSDOS)

	/* Hack -- max length */
	if (k > 8) k = 8;

#endif

	/* Terminate */
	op_ptr->base_name[k] = '\0';

	/* Require a "base" name */
	if (!op_ptr->base_name[0]) strcpy(op_ptr->base_name, "PLAYER");
  
  
#ifdef SAVEFILE_MUTABLE
  
	/* Accept */
	sf = TRUE;
  
#endif
  
	/* Change the savefile name */
	if (sf)
	{
		char temp[128];
      
#ifdef SAVEFILE_USE_UID
		/* Rename the savefile, using the player_uid and base_name */
		sprintf(temp, "%d.%s", player_uid, op_ptr->base_name);
#else
		/* Rename the savefile, using the base name */
		sprintf(temp, "%s", op_ptr->base_name);
#endif
      
#ifdef VM
		/* Hack -- support "flat directory" usage on VM/ESA */
		sprintf(temp, "%s.sv", op_ptr->base_name);
#endif /* VM */
      
		/* Build the filename */
#ifdef _WIN32_WCE
		/* SJG */
		/* Rename the savefile, using the base name + .oa */
		sprintf(temp, "%s.oa", op_ptr->base_name);
      
		// The common open file dialog doesn't like
		// anything being farther up than one directory!
		// For now hard code it. I should probably roll my
		// own open file dailog.
		path_build(savefile, 1024, "\\My Documents\\O", temp);
#else
		path_build(savefile, 1024, ANGBAND_DIR_SAVE, temp);
#endif
	}
}




/*
 * Hack -- commit suicide
 */
void do_cmd_suicide(void)
{
	int i;

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
		/* Verify */
		if (!get_check("Do you really want to suicide? ")) return;

		/* Special Verification for suicide */
		prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
		flush();
		i = inkey();
		prt("", 0, 0);
		if (i != '@') return;
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
	long score = (p_ptr->max_exp + (100 * p_ptr->max_depth));
  
	return (score);
}


/*
 * Gets a personalized string for ghosts.  Code originally from get_name. -LM-
 */
static char *get_personalized_string(byte choice)
{
	static char tmp[80], info[80];
	byte n, i;
  
	/* Clear last line */
	clear_from(15);
  
	clear_from(15);
  
	/* Prompt and ask */
	if (choice == 1)
	{ 
		prt("Enter a message for your character's ghost", 15, 0);
		prt("above, or hit ESCAPE.", 16, 0);
	}
	else if (choice == 2) 
	{
		prt("Enter an addition to your character ghost's", 15, 0);
		prt("description above, or hit ESCAPE.", 16, 0);
	}
	else return NULL;

	sprintf(info, "(%d characters maximum.  Entry will be used as", 
		(small_screen ? 47 : 79));

	prt(info, 17, 0);
	prt("(a) sentence(s).)", 18, 0);
  
	/* Ask until happy */
	while (1)
	{
		move_cursor(14, 0);
      
		/* Get an input */
		(void)askfor_aux(tmp, (small_screen ? 47 : 79), NULL);
	  
		/* All done */
		break;
	}
  
	/* Pad the string (to clear junk and allow room for a ending) */
	if (small_screen)
		sprintf(tmp, "%-47.47s", tmp);
	else
		sprintf(tmp, "%-79.79s", tmp);
  
	/* Ensure that strings end like a sentence, and neatly clip the string. */
	for (n = (small_screen ? 47 : 79); ; n--)
	{
		if ((tmp[n] == ' ') || (tmp[n] == '\0')) continue;
		else
		{
			if ((tmp[n] == '!') || (tmp[n] == '.') || (tmp[n] == '?'))
			{
				tmp[n + 1] = '\0';
				for (i = n + 2; i < (small_screen ? 48 : 80); i++) tmp[i] = '\0';
				break;
			}
			else 
			{
				tmp[n + 1] = '.';
				tmp[n + 2] = '\0';
				for (i = n + 3; i < (small_screen ? 48 : 80); i++) tmp[i] = '\0';
				break;
			}
		}
	}

	/* Start the sentence with a capital letter. */
	if (islower(tmp[0])) tmp[0] = toupper(tmp[0]);

	/* Return the string */
	return tmp;

}

/*
 * Save a "bones" file for a dead character.  Now activated and (slightly)
 * altered.  Allows the inclusion of personalized strings.
 */
static void make_bones(void)
{
	FILE *fp;
  
	char str[1024];
	event_type answer;
	byte choice=0;
  
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
				path_build(str, 1024, ANGBAND_DIR_BONE, tmp);

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
			if (op_ptr->full_name[0] != '\0') 
				fprintf(fp, "%s\n", op_ptr->full_name);
			else fprintf(fp, "Anonymous\n");
          
          
          
			fprintf(fp, "%d\n", p_ptr->psex);
			fprintf(fp, "%d\n", p_ptr->prace);
			fprintf(fp, "%d\n", p_ptr->pclass);

			/* Clear screen */
			Term_clear();

			while(1)
			{
				/* Ask the player if he wants to 
				 * add a personalized string. 
				 */
				prt("Information about your character has been saved", 15, 0);
				prt("in a bones file.  Would you like to give the", 16, 0);
				prt("ghost a special message or description? (yes/no)", 17, 0);
				add_button("Yes", 'y');
				add_button("No", 'n');
				update_statusline();      
      
				answer = inkey_ex();
              
				/* Clear last line */
				clear_from(15);
				clear_from(16);
              
				/* Determine what the personalized string will be used for.  */
				if ((answer.key == 'Y') || (answer.key == 'y'))
				{
					prt("Will you add something for your ghost to say,", 15, 0);
					prt("or add to the monster description?", 16, 0);
					prt("((M)essage/(D)escription)", 17, 0);

					/* Buttons */
					kill_button('y');
					kill_button('n');
					add_button("M", 'M');
					add_button("D", 'D');
					add_button("ESC", ESCAPE);
					update_statusline();
                  
					while(1)
					{
						answer = inkey_ex();
                      
						clear_from(15);
						clear_from(16);
                      
						if ((answer.key == 'M') || (answer.key == 'm'))
						{
							choice = 1;
							break;
						}
						else if ((answer.key == 'D') || (answer.key == 'd'))
						{
							choice = 2;
							break;
						}
						else
						{
							choice = 0;
							break;
						}
					}
				}
				else if ((answer.key == 'N') || (answer.key == 'n') || 
					 (answer.key == ESCAPE)) 
				{
					choice = 0;
					break;
				}
              
				kill_all_buttons();
              
				/* If requested, get the personalized string, and write it and 
				 * info on how it should be used in the bones file.  Otherwise, 
				 * indicate the absence of such a string.
				 */
				if (choice) fprintf(fp, "%d:%s\n",
						    choice, get_personalized_string(choice));
				else fprintf(fp, "0: \n");

				/* Close and save the Bones file */
				my_fclose(fp);

				return;
			}
		}
	}
}


/*
 * Centers a string within a 31 character string
 */
static void center_string(char *buf, cptr str)
{
	int i, j;
  
	/* Total length */
	i = strlen(str);

	/* Necessary border */
	j = 15 - i / 2;

	/* Mega-Hack */
	sprintf(buf, "%*s%s%*s", j, "", str, 31 - i - j, "");
}

/*
 * Hack - save the time of death
 */
static time_t death_time = (time_t)0;


/*
 * Encode the screen colors for the closing screen
 */
static char hack[17] = "dwsorgbuDWvyRGBU";

/*
 * Display a "tomb-stone"
 */
static void print_tomb(void)
{
	cptr p;

	int offset = 12;
  
	char tmp[160];
  
	char buf[1024];
  
	FILE *fp;
  
#ifdef _WIN32_WCE
	time_t ct = fake_time((time_t)0);
#else
	time_t ct = time((time_t)0);
#endif

	/* Clear screen */
	Term_clear();
  
	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, 
		   (small_screen ? "dead_s.txt" : "dead.txt"));
               
	/* Open the News file */
	fp = my_fopen(buf, "r");

	/* Dump */
	if (fp)
	{
		int i, y, x;
      
		byte a = 0;
		char c = ' ';
      
		bool okay = TRUE;
      
		int len;
      
      
		/* Load the screen */
		for (y = 0; okay; y++)
		{
			/* Get a line of data */
			if (my_fgets(fp, buf, 1024)) okay = FALSE;
          
			/* Stop on blank line */
			if (!buf[0]) break;
          
			/* Get the width */
			len = strlen(buf);
          
			/* XXX Restrict to current screen size */
			if (len >= Term->wid) len = Term->wid;
          
			/* Show each row */
			for (x = 0; x < len; x++)
			{
				/* Put the attr/char */
				Term_draw(x, y, TERM_WHITE, buf[x]);
			}
		}
      
		/* Get the blank line */
		/* if (my_fgets(fp, buf, 1024)) okay = FALSE; */
      
      
		/* Load the screen */
		for (y = 0; okay; y++)
		{
			/* Get a line of data */
			if (my_fgets(fp, buf, 1024)) okay = FALSE;
          
			/* Stop on blank line */
			if (!buf[0]) break;
          
			/* Get the width */
			len = strlen(buf);
          
			/* XXX Restrict to current screen size */
			if (len >= Term->wid) len = Term->wid;
          
			/* Show each row */
			for (x = 0; x < len; x++)
			{
				/* Get the attr/char */
				(void)(Term_what(x, y, &a, &c));
              
				/* Look up the attr */
				for (i = 0; i < 16; i++)
				{
					/* Use attr matches */
					if (hack[i] == buf[x]) a = i;
				}
              
				/* Put the attr/char */
				Term_draw(x, y, a, c);
			}

			/* Place the cursor */
			move_cursor(y, x);
          
		}
      
      
		/* Get the blank line */
		/* if (my_fgets(fp, buf, 1024)) okay = FALSE; */
      
      
		/* Close it */
		my_fclose(fp);
	}
  
	/* King or Queen */
	if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "Magnificent";
	}

	/* Normal */
	else
	{
		p = c_text + cp_ptr->title[(p_ptr->lev - 1) / 5];
	}

	/* Set offset */
	offset = 11;
  
	center_string(buf, op_ptr->full_name);
	put_str(buf, 6, offset);
  
	center_string(buf, "the");
	put_str(buf, 7, offset);
  
	center_string(buf, p);
	put_str(buf, 8, offset);
  
  
	center_string(buf, c_name + cp_ptr->name);
	put_str(buf, 10, offset);
  
	sprintf(tmp, "Level: %d", (int)p_ptr->lev);
	center_string(buf, tmp);
	put_str(buf, 11, offset);
  
	sprintf(tmp, "Exp: %ld", (long)p_ptr->exp);
	center_string(buf, tmp);
	put_str(buf, 12, offset);
  
	sprintf(tmp, "AU: %ld", (long)p_ptr->au);
	center_string(buf, tmp);
	put_str(buf, 13, offset);
  
	sprintf(tmp, "Killed on Level %d", p_ptr->depth);
	center_string(buf, tmp);
	put_str(buf, 14, offset);

	sprintf(tmp, "by %s.", p_ptr->died_from);
	center_string(buf, tmp);
	put_str(buf, 15, offset);
  
#ifdef _WIN32_WCE
	{     
		char* fake_ctime(const unsigned long* fake_time_t);
		sprintf(tmp, "%-.24s", fake_ctime(&ct));
	}
#else
	sprintf(tmp, "%-.24s", ctime(&ct));
#endif
	center_string(buf, tmp);
	put_str(buf, 17, offset);
}


/*
 * Hack - Know inventory and home items upon death
 */
static void death_knowledge(void)
{
	int i;
  
	object_type *o_ptr;
  
	store_type *st_ptr = NULL;
  
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
  
	/* Activate the store */
	st_ptr = &store[STORE_HOME];
  
  
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
  
	store_type *st_ptr;

	event_type ke;

	bool done = FALSE;
  
	/* Activate the store */
	st_ptr = &store[STORE_HOME];
  
	/* Display player */
	display_player(0);

	/* Prompt for inventory */
	prt("Hit any key to see more information (ESC to abort): ", 23, 0);
  
	/* Buttons */
	backup_buttons();
	kill_all_buttons();
	add_button("ESC", ESCAPE);
	add_button("Continue", 'q');
	update_statusline();

	/* Allow abort at this point */
	ke = inkey_ex();
	if (ke.key == ESCAPE) done = TRUE;
  
	/* Show equipment and inventory */
  
	/* Equipment -- if any */
	if ((p_ptr->equip_cnt) && !done)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_equip();
		prt("You are using: -more-", 0, 0);
		update_statusline();
		ke = inkey_ex();
		if (ke.key == ESCAPE) done = TRUE;
	}

	/* Inventory -- if any */
	if ((p_ptr->inven_cnt) && !done)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_inven();
		prt("You are carrying: -more-", 0, 0);
		update_statusline();
		ke = inkey_ex();
		if (ke.key == ESCAPE) done = TRUE;
	}
  
  
  
	/* Home -- if anything there */
	if ((st_ptr->stock_num) && !done)
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
				object_desc(o_name, o_ptr, TRUE, 4);
              
				/* Get the inventory color */
				attr = tval_to_attr[o_ptr->tval & 0x7F];

				/* Display the object */
				c_put_str(attr, o_name, j+2, 7);
			}
          
			/* Caption */
			prt(format("Your home contains (page %d): -more-", k+1), 0, 0);
			update_statusline();
          
			/* Wait for it */
			ke = inkey_ex();
			if (ke.key == ESCAPE) done = TRUE;
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

	/* Get an item */
	q = "Examine which item? ";
	s = "You have nothing to examine.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);
  
	/* Save screen */
	screen_save();
  
	/* Examine the item. */
	object_info_screen(o_ptr, FALSE);

	/* Load screen */
	screen_load();
}

/*
 * Seek score 'i' in the highscore file
 */
static int highscore_seek(int i)
{
	/* Seek for the requested record */
	return (fd_seek(highscore_fd, (huge)(i) * sizeof(high_score)));
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
static int highscore_write(high_score *score)
{
	/* Write the record, note failure */
	return (fd_write(highscore_fd, (char*)(score), sizeof(high_score)));
}




/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(high_score *score)
{
	int i;

	high_score the_score;
  
	/* Paranoia -- it may not have opened */
#ifdef _WIN32_WCE
	if (highscore_fd == -1) return (-1);
#else
	if (highscore_fd < 0) return (-1);
#endif

	/* Go to the start of the highscore file */
	if (highscore_seek(0)) return (-1);

	/* Read until we get to a higher score */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score)) return (i);
		if (strcmp(the_score.pts, score->pts) < 0) return (i);
	}

	/* The "last" entry is always usable */
	return (MAX_HISCORES - 1);
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static int highscore_add(high_score *score)
{
	int i, slot;
	bool done = FALSE;

	high_score the_score, tmpscore;
  
  
	/* Paranoia -- it may not have opened */
#ifdef _WIN32_WCE
	if (highscore_fd == -1) return (-1);
#else
	if (highscore_fd < 0) return (-1);
#endif
  
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
void display_scores_aux(int from, int to, int note, high_score *score)
{
	int i, j, k, n, place;
	byte attr;
	int wid, hgt;
	event_type ke;
  
	high_score the_score;
  
	char out_val[256];
	char tmp_val[160];

	int per_screen;

	/* Get size */
	Term_get_size(&wid, &hgt);
  
	per_screen = (hgt - 4) / (small_screen ? 7 : 4);
  
  
	/* Paranoia -- it may not have opened */
#ifdef _WIN32_WCE
	if (highscore_fd == -1) return (-1);
#else
	if (highscore_fd < 0) return;
#endif
  
  
	/* Assume we will show the first 10 */
	if (from < 0) from = 0;
	if (to < 0) to = 10;
	if (to > MAX_HISCORES) to = MAX_HISCORES;


	/* Seek to the beginning */
	if (highscore_seek(0)) return;

	/* Hack -- Count the high scores */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score)) break;
	}

	/* Hack -- allow "fake" entry to be last */
	if ((note == i) && score) i++;

	/* Forget about the last entries */
	if (i > to) i = to;


	/* Show per_screen per page, until "done" */
	for (k = from, place = k+1; k < i; k += per_screen)
	{
		/* Clear screen */
		Term_clear();
      
		/* Title */
		put_str_center("Oangband Hall of Fame", 0);
      
#if 0
		/* Indicate non-top scores */
		if (k > 0)
		{
			sprintf(tmp_val, "(from position %d)", k + 1);
			put_str(tmp_val, 0, 40);
		}
#endif

		/* Dump per_screen entries */
		for (j = k, n = 0; j < i && n < per_screen; place++, j++, n++)
		{
			int pr, pc, clev, mlev;
          
			cptr user, gold, when, aged, cdun;
          
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

			/* Extract the race/class */
			pr = atoi(the_score.p_r);
			pc = atoi(the_score.p_c);

			/* Extract the level info */
			clev = atoi(the_score.cur_lev);
			mlev = atoi(the_score.max_lev);
			cdun = the_score.cur_dun;
          
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
          
			if (small_screen)
			{
				/* Dump some info */
				sprintf(out_val, "%3d.%9s  %s the",
					place, the_score.pts, the_score.who);
              
				/* Dump the first line */
				c_put_str(attr, out_val, n * 7 + 2, 0);
              
				/* More info */
				sprintf(out_val, "               %s %s, Level %d",
					p_name + p_info[pr].name, c_name + c_info[pc].name,
					clev);
              
				/* Dump the next line */
				c_put_str(attr, out_val, n * 7 + 3, 0);
              
				/* Line of info */
				sprintf(out_val, "               Killed by %s", the_score.how);

				/* Dump the info */
				c_put_str(attr, out_val, n * 7 + 4, 0);

				/* Line of info */
				sprintf(out_val, "               on level %s", cdun);

				/* Dump the info */
				c_put_str(attr, out_val, n * 7 + 5, 0);
          
				/* Line of info */
				sprintf(out_val,
					"               (User %s, Date %s,", user, when);

				/* Dump the info */
				c_put_str(attr, out_val, n * 7 + 6, 0);
          
				/* Line of info */
				sprintf(out_val,
					"               Gold %s, Turn %s).", gold, aged);

				/* Dump the info */
				c_put_str(attr, out_val, n * 7 + 7, 0);
			}
			else
			{
				/* Dump some info */
				sprintf(out_val, "%3d.%9s  %s the %s %s, Level %d",
					place, the_score.pts, the_score.who,
					p_name + p_info[pr].name, c_name + c_info[pc].name,
					clev);
          
				/* Dump the first line */
				c_put_str(attr, out_val, n * 4 + 2, 0);
          
				/* Another line of info */
				sprintf(out_val, "               Killed by %s on dungeon level %s",
					the_score.how, cdun);
          
				/* Dump the info */
				c_put_str(attr, out_val, n * 4 + 3, 0);

				/* And still another line of info */
				sprintf(out_val,
					"               (User %s, Date %s, Gold %s, Turn %s).",
					user, when, gold, aged);
				c_put_str(attr, out_val, n * 4 + 4, 0);
			}
		}
      
      
		/* Wait for response */
		prt_center("[Press ESC to quit, any other key to continue.]", hgt - 1);
		ke = inkey_ex();
		prt("", hgt - 1, 0);
      
		/* Hack -- notice Escape */
		if (j == ESCAPE) break;
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

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);
  
	/* Paranoia -- No score file */
#ifdef _WIN32_WCE
	if (highscore_fd == -1) quit("Score file unavailable.");
#else
	if (highscore_fd < 0) quit("Score file unavailable.");
#endif
  
	/* Clear screen */
	Term_clear();

	/* Display the scores */
	display_scores_aux(from, to, -1, NULL);

	/* Shut the high score file */
	fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;

	/* Quit */
	quit(NULL);
}


/*
 * Hack - save index of player's high score
 */
static int score_idx = -1;



/*
 * Enters a players name on a hi-score table, if "legal".
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static errr enter_score(void)
{
	int j;

	high_score the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
		return (0);
	}

#ifndef SCORE_WIZARDS

	/* Wizard-mode pre-empts scoring */
	if (p_ptr->noscore & 0x000F)
	{
		msg_print("Score not registered for wizards.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}

#endif

#ifndef SCORE_BORGS

	/* Borg-mode pre-empts scoring */
	if (p_ptr->noscore & 0x00F0)
	{
		msg_print("Score not registered for borgs.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}
#endif

#ifndef SCORE_CHEATERS

	/* Cheaters are not scored */
	for (j = OPT_cheat_start; j < OPT_cheat_end+1; ++j)
	{
		if (!op_ptr->opt[j]) continue;

		msg_print("Score not registered for cheaters.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}

#endif

	/* Hack -- Interupted */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Interrupting"))
	{
		msg_print("Score not registered due to interruption.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}

	/* Hack -- Quitter */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Quitting"))
	{
		msg_print("Score not registered due to quitting.");
		msg_print(NULL);
		score_idx = -1;
		return (0);
	}


	/* Clear the record */
	(void)WIPE(&the_score, high_score);
  
	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
  
	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);
	the_score.turns[9] = '\0';

#ifdef HIGHSCORE_DATE_HACK
	/* Save the date in a hacked up form (9 chars) */
	sprintf(the_score.day, "%-.6s %-.2s",
		ctime(&death_time) + 4, ctime(&death_time) + 22);
#else
	/* Save the date in standard encoded form (9 chars) */
#ifdef _WIN32_WCE
	{
		char* fake_ctime(const unsigned long* fake_time_t);
		sprintf(the_score.day, "%-.6s %-.2s",
			fake_ctime(&death_time) + 4, fake_ctime(&death_time) + 22);
	}
#else
	strftime(the_score.day, 10, "@%Y%m%d", localtime(&death_time));
#endif /* _WIN32_WCE */
#endif
  
	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->pclass);
  
	/* Save the level and such */
	sprintf(the_score.cur_lev, "%3d", p_ptr->lev);
	sprintf(the_score.cur_dun, "%3d", p_ptr->depth);
	sprintf(the_score.max_lev, "%3d", p_ptr->max_lev);
	sprintf(the_score.max_dun, "%3d", p_ptr->max_depth);
  
	/* Save the cause of death (31 chars) */
	sprintf(the_score.how, "%-.31s", p_ptr->died_from);


	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK)) return (1);

	/* Add a new entry to the score list, see where it went */
	score_idx = highscore_add(&the_score);

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK)) return (1);


	/* Success */
	return (0);
}



/*
 * Enters a players name on a hi-score table, if "legal", and in any
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
		msg_print(NULL);
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
		display_scores_aux(0, (small_screen ? 8 : 15), score_idx, NULL);
	}
  
	/* Display the scores surrounding the player */
	else
	{
		display_scores_aux(0, (small_screen ? 4 : 5), score_idx, NULL);
		display_scores_aux(score_idx - 2, score_idx + (small_screen ? 3 : 7), 
				   score_idx, NULL);
	}
  
  
	/* Success */
	return;
}



/*
 * Predict the players location, and display it.
 */
errr predict_score(void)
{
	int j;

	high_score the_score;
  
  
	/* No score file */
#ifdef _WIN32_WCE
	if (highscore_fd == -1)
#else
		if (highscore_fd < 0)
#endif
		{
			msg_print("Score file unavailable.");
			msg_print(NULL);
			return (0);
		}

  
	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
  
	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long)total_points());

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);

	/* Hack -- no time needed */
	strcpy(the_score.day, "TODAY");

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->pclass);
  
	/* Save the level and such */
	sprintf(the_score.cur_lev, "%3d", p_ptr->lev);
	sprintf(the_score.cur_dun, "%3d", p_ptr->depth);
	sprintf(the_score.max_lev, "%3d", p_ptr->max_lev);
	sprintf(the_score.max_dun, "%3d", p_ptr->max_depth);

	/* Hack -- no cause of death */
	strcpy(the_score.how, "nobody (yet!)");


	/* See where the entry would be placed */
	j = highscore_where(&the_score);


	/* Hack -- Display the top fifteen scores */
	if (j < 10)
	{
		display_scores_aux(0, (small_screen ? 10 : 15), j, &the_score);
	}
  
	/* Display some "useful" scores */
	else
	{
		display_scores_aux(0, (small_screen ? 4 : 5), -1, NULL);
		display_scores_aux(j - 2, j + (small_screen ? 2 : 7), j, &the_score);
	}
  
  
	/* Success */
	return (0);
}



void show_scores(void)
{
	char buf[1024];
  
	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");
  
	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Paranoia -- No score file */
#ifdef _WIN32_WCE
	if (highscore_fd == -1)
#else
		if (highscore_fd < 0)
#endif
		{
			msg_print("Score file unavailable.");
		}
		else
		{
			/* Save Screen */
			screen_save();
      
			/* Clear screen */
			Term_clear();
      
			/* Display the scores */
			if (character_generated)
				predict_score();
			else
				display_scores_aux(0, MAX_HISCORES, -1, NULL);
      
			/* Shut the high score file */
			(void)fd_close(highscore_fd);
      
			/* Forget the high score fd */
			highscore_fd = -1;
      
			/* Load screen */
			screen_load();
      
			/* Hack - Flush it */
			Term_fresh();
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
	put_str(format("All Hail the Mighty %s!", sp_ptr->winner), 17, 22);

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(23);
}

/*
 * Handle character death
 */
static void close_game_aux(void)
{
	int ch, adj = 0;
	event_type ke;
  
	cptr p, q;
  
	/* Flush all input keys */
	flush();

	/* Easy more? */
	if (easy_more) messages_easy(FALSE);
        
	/* Screen no longer normal */
	normal_screen = FALSE;

	/* Dump bones file */
	make_bones();

	if (small_screen)
	{
		q = "['a' add to notes file,'c' chardump,'t' scores]";
		adj = 22;
	}
	else
	{
		q = "['a' to add a notes file comment, 'c' for a character dump, 't' for scores]";
		adj = 41;
	}
	if (small_screen)
		p = "['i' info, 'm' messages, 'x' items, or ESC]";
	else
		p = "['i' for character info, 'm' for messages, 'x' to examine items, or ESC]";

	/* Handle retirement */
	if (p_ptr->total_winner) kingly();

	/* Get time of death */
#ifdef _WIN32_WCE
	{
		unsigned long fake_time(unsigned long* fake_time_t);
		fake_time(&death_time);
	}
#else
	(void)time(&death_time);
#endif

	/* You are dead */
	print_tomb();

	/* Hack - Know everything upon death */
	death_knowledge();

	/* Enter player in high score list */
	enter_score();

	/* Flush all input keys */
	flush();

	/* Flush messages */
	msg_print(NULL);

	/* Forever */
	while (1)
	{
		/* Describe options */
		Term_putstr((small_screen ? 0 : 2), 21, -1, TERM_WHITE, q);
		Term_putstr((small_screen ? 0 : 2), 22, -1, TERM_WHITE, p);
      
		/* Buttons */
		kill_all_buttons();
		add_button("ESC", ESCAPE);
		add_button("x", 'x');
		add_button("m", 'm');
		add_button("i", 'i');
		add_button("t", 't');
		add_button("c", 'c');
		add_button("a", 'a');
		update_statusline();

		/* Query */
		ke = inkey_ex();
		ch = ke.key;
      
      
		/* Exit */
		if (ch == ESCAPE)
		{
			if (get_check("Do you want to quit? ")) break;
		}

		/* File dump */
		else if (ch == 'c')
		{
			/* Show a character screen */
			do_cmd_change_name();
          
			/* Flush messages */
			msg_print(NULL);
          
		}

		/* Show more info */
		else if (ch == 'i')
		{
			/* Save screen */
			screen_save();

			/* Show the character */
			show_info();

			/* Load screen */
			screen_load();
		}

		/* Show top scores */
		else if (ch == 't')
		{
			/* Save screen */
			screen_save();

			/* Show the scores */
			top_twenty();


			/* Load screen */
			screen_load();
		}

		/* Show top scores */
		else if (ch == 'm')
		{
			/* Save screen */
			screen_save();

			/* Show the scores */
			do_cmd_messages();


			/* Load screen */
			screen_load();
		}

		/* Examine an item */
		else if (ch == 'x')
		{
			death_examine();
		}
      
		/* Add last words to notes file */
		else if (ch == 'a')
		{
			do_cmd_note();
          
		}
	}


	/* Save dead player */
	if (!save_player())
	{
		msg_print("death save failed!");
		msg_print(NULL);
	}

}

/*
 * Close up the current game (player may or may not be dead)
 *
 * This function is called only from "main.c" and "signals.c".
 *
 * Note that the savefile is not saved until the tombstone is
 * actually displayed and the player has a chance to examine
 * the inventory and such.  This allows cheating if the game
 * is equipped with a "quit without save" method.  XXX XXX XXX
 */
void close_game(void)
{
	char buf[1024];
	event_type ke;
  
	/* Handle stuff */
	handle_stuff();

	/* Flush the messages */
	msg_print(NULL);

	/* Flush the input */
	flush();


	/* No suspending now */
	signals_ignore_tstp();


	/* Hack -- Character is now "icky" */
	character_icky = TRUE;


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);
  
  
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
      
		/* Prompt for scores XXX XXX XXX */
		prt("Press Return (or Escape).", 0, 40);
      
		/* Predict score (or ESCAPE) */
		ke = inkey_ex();
		if (ke.key != ESCAPE) predict_score();
	}
  
  
	/* Shut the high score file */
	fd_close(highscore_fd);

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


/*  Taken from Zangband.  What a good idea! */
errr get_rnd_line(char *file_name, char *output)
{
	FILE	    *fp;
	char	buf[1024];
	int lines=0, line, counter;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, file_name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp) return (-1);

	/* Parse the file */
	if (0 == my_fgets(fp, buf, 80))
		lines = atoi(buf);
	else return (1);

	/* choose a random line */
	line = randint(lines);

	for (counter = 0; counter <= line; counter++)
	{
		if (!(0 == my_fgets(fp, buf, 80)))
			return (1);
		else if (counter == line)
			break;
	}

	strcpy (output, buf);

	/* Close the file */
	my_fclose(fp);

	return (0);
}



#ifdef HANDLE_SIGNALS


#include <signal.h>


/*
 * Handle signals -- suspend
 *
 * Actually suspend the game, and then resume cleanly
 */
static void handle_signal_suspend(int sig)
{
	/* Disable handler */
	(void)signal(sig, SIG_IGN);

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
	(void)signal(sig, handle_signal_suspend);
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
	/* Disable handler */
	(void)signal(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	/* Count the signals */
	signal_count++;


	/* Terminate dead characters */
	if (p_ptr->is_dead)
	{
		/* Mark the savefile */
		strcpy(p_ptr->died_from, "Abortion");

		/* Close stuff */
		close_game();

		/* Quit */
		quit("interrupt");
	}

	/* Allow suicide (after 5) */
	else if (signal_count >= 5)
	{
		/* Cause of "death" */
		strcpy(p_ptr->died_from, "Interrupting");

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
		Term_erase(0, 0, 255);

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
	(void)signal(sig, handle_signal_simple);
}


/*
 * Handle signal -- abort, kill, etc
 */
static void handle_signal_abort(int sig)
{
	/* Disable handler */
	(void)signal(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	/* Clear the bottom line */
	Term_erase(0, 23, 255);

	/* Give a warning */
	Term_putstr(0, 23, -1, TERM_RED,
		    "A gruesome software bug LEAPS out at you!");

	/* Message */
	Term_putstr(45, 23, -1, TERM_RED, "Panic save...");

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
		Term_putstr(45, 23, -1, TERM_RED, "Panic save succeeded!");
	}

	/* Save failed */
	else
	{
		Term_putstr(45, 23, -1, TERM_RED, "Panic save failed!");
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
	(void)signal(SIGTSTP, SIG_IGN);
#endif

}

/*
 * Handle SIGTSTP signals (keyboard suspend)
 */
void signals_handle_tstp(void)
{

#ifdef SIGTSTP
	(void)signal(SIGTSTP, handle_signal_suspend);
#endif

}


/*
 * Prepare to handle the relevant signals
 */
void signals_init(void)
{

#ifdef SIGHUP
	(void)signal(SIGHUP, SIG_IGN);
#endif


#ifdef SIGTSTP
	(void)signal(SIGTSTP, handle_signal_suspend);
#endif


#ifdef SIGINT
	(void)signal(SIGINT, handle_signal_simple);
#endif

#ifdef SIGQUIT
	(void)signal(SIGQUIT, handle_signal_simple);
#endif


#ifdef SIGFPE
	(void)signal(SIGFPE, handle_signal_abort);
#endif

#ifdef SIGILL
	(void)signal(SIGILL, handle_signal_abort);
#endif

#ifdef SIGTRAP
	(void)signal(SIGTRAP, handle_signal_abort);
#endif

#ifdef SIGIOT
	(void)signal(SIGIOT, handle_signal_abort);
#endif

#ifdef SIGKILL
	(void)signal(SIGKILL, handle_signal_abort);
#endif

#ifdef SIGBUS
	(void)signal(SIGBUS, handle_signal_abort);
#endif

#ifdef SIGSEGV
	(void)signal(SIGSEGV, handle_signal_abort);
#endif

#ifdef SIGTERM
	(void)signal(SIGTERM, handle_signal_abort);
#endif

#ifdef SIGPIPE
	(void)signal(SIGPIPE, handle_signal_abort);
#endif

#ifdef SIGEMT
	(void)signal(SIGEMT, handle_signal_abort);
#endif

#ifdef SIGDANGER
	(void)signal(SIGDANGER, handle_signal_abort);
#endif

#ifdef SIGSYS
	(void)signal(SIGSYS, handle_signal_abort);
#endif

#ifdef SIGXCPU
	(void)signal(SIGXCPU, handle_signal_abort);
#endif

#ifdef SIGPWR
	(void)signal(SIGPWR, handle_signal_abort);
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


#endif  /* HANDLE_SIGNALS */


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

bool write_char(int row, int col)
{
	if (use_trptile && ((row % 3) || (col % 3) || ((col % 6) && use_bigtile))) 
		return (FALSE);
	if (use_dbltile && ((row % 2) || (col % 2) || ((col % 4) && use_bigtile))) 
		return (FALSE);
	if (use_bigtile && (col % 2)) return (FALSE);
	return (TRUE);
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
	int x, y, col_factor, row_factor;

	col_factor = (use_trptile ? (use_bigtile ? 6 : 3) : 
		      (use_dbltile ? (use_bigtile ? 4 : 2) : 
		       (use_bigtile ? 2 : 1)));
	row_factor = (use_trptile ? 3 : (use_dbltile ? 2 : 1));
  
	x = (col - COL_MAP)/col_factor + panel_col_min;
	y = (row - ROW_MAP)/row_factor + panel_row_min;

	/* Retrieve current screen size */
	Term_get_size(&wid, &hgt);
  
	/* Calculate the size of dungeon map area (ignoring bigscreen) */
	screen_wid = wid - (COL_MAP + 1);
	screen_hgt = hgt - (ROW_MAP + 1);
  
	/* Get the tile from the screen */
	a = Term->scr->a[row][col];
	c = Term->scr->c[row][col];
  
	/* Convert the map display to the default characters */
	if (!character_icky &&
	    ((col - COL_MAP) >= 0) && ((col - COL_MAP) < SCREEN_WID * col_factor) &&
	    ((row - ROW_MAP) >= 0) && ((row - ROW_MAP) < SCREEN_HGT * row_factor))
	{
		/* Convert dungeon map into default attr/chars */
		if (in_bounds(y, x) && write_char(row - ROW_MAP, col - COL_MAP))
		{
			/* Retrieve default attr/char */
			map_info_default(y, x, &a, &c);
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
void html_screenshot(cptr name)
{
	int y, x;
	int wid, hgt;
  
	byte a = TERM_WHITE;
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
	fprintf(htm, "<META NAME=\"GENERATOR\" Content=\"FAAngband %s\">\n", 
		VERSION_STRING);
	fprintf(htm, "<TITLE>%s</TITLE>\n", name);
	fprintf(htm, "</HEAD>\n");
	fprintf(htm, "<BODY TEXT=\"#FFFFFF\" BGCOLOR=\"#000000\">");
	fprintf(htm, "<FONT COLOR=\"#%02X%02X%02X\">\n<PRE><TT>",
		angband_color_table[TERM_WHITE][1],
		angband_color_table[TERM_WHITE][2],
		angband_color_table[TERM_WHITE][3]);
  
	/* Dump the screen */
	for (y = 0; y < hgt; y++)
	{
		for (x = 0; x < wid; x++)
		{
          
			/* Get the ASCII tile */
			get_default_tile(y, x, &a, &c);
          
			/* Hack - show base color in dump */
			a &= 0x0F;

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
