/* File: files.c */

/*
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
 */
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


/*
 * Hack -- grab permissions
 */
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
			if ((*t == ':') || (*t == '/'))
				break;

			/* Handle single quotes */
			if (*t == '\'')
			{
				/* Advance */
				t++;

				/* Handle backslash */
				if (*t == '\\')
					t++;

				/* Require a character */
				if (!*t)
					break;

				/* Advance */
				t++;

				/* Hack -- Require a close quote */
				if (*t != '\'')
					*t = '\'';
			}

			/* Handle back-slash */
			if (*t == '\\')
				t++;
		}

		/* Nothing left */
		if (!*t)
			break;

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
 *   R:<num>:<a>:<c>
 *
 * Specify the attr/char values for "objects" by kind index
 *   K:<num>:<a>:<c>
 *
 * Specify the attr/char values for "features" by feature index
 *   F:<num>:<a>:<c>
 *
 * Specify the attr/char values for unaware "objects" by kind tval
 *   U:<tv>:<a>:<c>
 *
 * Specify the attribute values for inventory "objects" by kind tval
 *   E:<tv>:<a>
 *
 * Define a macro action, given an encoded macro action
 *   A:<str>
 *
 * Create a normal macro, given an encoded macro trigger
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
	int i, j, n1, n2;

	char *zz[16];


	/* Skip "empty" lines */
	if (!buf[0])
		return (0);

	/* Skip "blank" lines */
	if (isspace(buf[0]))
		return (0);

	/* Skip comments */
	if (buf[0] == '#')
		return (0);


	/* Require "?:*" format */
	if (buf[1] != ':')
		return (1);


	/* Process "R:<num>:<a>/<c>" -- attr/char for monster races */
	if (buf[0] == 'R')
	{
		if (tokenize(buf + 2, 3, zz) == 3)
		{
			monster_race *r_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= MAX_R_IDX)
				return (1);
			r_ptr = &r_info[i];
			if (n1)
				r_ptr->x_attr = n1;
			if (n2)
				r_ptr->x_char = n2;
			return (0);
		}
	}


	/* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
	else if (buf[0] == 'K')
	{
		if (tokenize(buf + 2, 3, zz) == 3)
		{
			object_kind *k_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= MAX_K_IDX)
				return (1);
			k_ptr = &k_info[i];
			if (n1)
				k_ptr->x_attr = n1;
			if (n2)
				k_ptr->x_char = n2;
			return (0);
		}
	}


	/* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
	else if (buf[0] == 'F')
	{
		if (tokenize(buf + 2, 3, zz) == 3)
		{
			feature_type *f_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= MAX_F_IDX)
				return (1);
			f_ptr = &f_info[i];
			if (n1)
				f_ptr->z_attr = n1;
			if (n2)
				f_ptr->z_char = n2;
			return (0);
		}
	}


	/* Process "U:<tv>:<a>/<c>" -- attr/char for unaware items */
	else if (buf[0] == 'U')
	{
		if (tokenize(buf + 2, 3, zz) == 3)
		{
			j = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			for (i = 1; i < MAX_K_IDX; i++)
			{
				object_kind *k_ptr = &k_info[i];
				if (k_ptr->tval == j)
				{
					if (n1)
						k_ptr->d_attr = n1;
					if (n2)
						k_ptr->d_char = n2;
				}
			}
			return (0);
		}
	}


	/* Process "E:<tv>:<a>" -- attribute for inventory objects */
	else if (buf[0] == 'E')
	{
		if (tokenize(buf + 2, 2, zz) == 2)
		{
			j = (byte) strtol(zz[0], NULL, 0) % 128;
			n1 = strtol(zz[1], NULL, 0);
			if (n1)
				tval_to_attr[j] = n1;
			return (0);
		}
	}


	/* Process "A:<str>" -- save an "action" for later */
	else if (buf[0] == 'A')
	{
		text_to_ascii(macro_buffer, buf + 2);
		return (0);
	}


	/* Process "P:<str>" -- create normal macro */
	else if (buf[0] == 'P')
	{
		char tmp[1024];
		text_to_ascii(tmp, buf + 2);
		macro_add(tmp, macro_buffer);
		return (0);
	}


	/* Process "C:<num>:<str>" -- create keymap */
	else if (buf[0] == 'C')
	{
		int mode;

		char tmp[1024];

		if (tokenize(buf + 2, 2, zz) != 2)
			return (1);

		mode = strtol(zz[0], NULL, 0);
		if ((mode < 0) || (mode >= KEYMAP_MODES))
			return (1);

		text_to_ascii(tmp, zz[1]);
		if (!tmp[0] || tmp[1])
			return (1);
		i = (byte) (tmp[0]);

		string_free(keymap_act[mode][i]);

		keymap_act[mode][i] = string_make(macro_buffer);

		/* XXX Hack -- See main-win.c */
		angband_keymap_flag = TRUE;

		return (0);
	}


	/* Process "V:<num>:<kv>:<rv>:<gv>:<bv>" -- visual info */
	else if (buf[0] == 'V')
	{
		if (tokenize(buf + 2, 5, zz) == 5)
		{
			i = (byte) strtol(zz[0], NULL, 0);
			angband_color_table[i][0] = (byte) strtol(zz[1], NULL, 0);
			angband_color_table[i][1] = (byte) strtol(zz[2], NULL, 0);
			angband_color_table[i][2] = (byte) strtol(zz[3], NULL, 0);
			angband_color_table[i][3] = (byte) strtol(zz[4], NULL, 0);
			return (0);
		}
	}


	/* Process "X:<str>" -- turn option off */
	else if (buf[0] == 'X')
	{
		for (i = 0; i < OPT_MAX; i++)
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
		for (i = 0; i < OPT_MAX; i++)
		{
			if (option_text[i] && streq(option_text[i], buf + 2))
			{
				op_ptr->opt[i] = TRUE;
				return (0);
			}
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
	while (isspace(*s))
		s++;

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
				if (*t && !streq(t, "0"))
					v = "1";
			}
		}

		/* Function: AND */
		else if (streq(t, "AND"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && streq(t, "0"))
					v = "0";
			}
		}

		/* Function: NOT */
		else if (streq(t, "NOT"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
				if (*t && streq(t, "0"))
					v = "0";
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
				if (*t && !streq(p, t))
					v = "0";
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
				if (*t && (strcmp(p, t) >= 0))
					v = "0";
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
				if (*t && (strcmp(p, t) <= 0))
					v = "0";
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
		if (f != b2)
			v = "?x?x?";

		/* Extract final and Terminate */
		if ((f = *s) != '\0')
			*s++ = '\0';
	}

	/* Other */
	else
	{
		/* Scan (accept identifiers and dollar signs) */
		while (isalnum(*s) || (*s == '_') || (*s == '$'))
			s++;

		/* Extract final and Terminate */
		if ((f = *s) != '\0')
			*s++ = '\0';

		/* Variable */
		if (*b == '$')
		{
			/* System */
			if (streq(b + 1, "SYS"))
			{
				v = ANGBAND_SYS;
			}

			/* Race */
			else if (streq(b + 1, "RACE"))
			{
				v = rp_ptr->title;
			}

			/* Class */
			else if (streq(b + 1, "CLASS"))
			{
				v = cp_ptr->title;
			}

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

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_USER, name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No such file */
	if (!fp)
		return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Count lines */
		num++;


		/* Skip "empty" lines */
		if (!buf[0])
			continue;

		/* Skip "blank" lines */
		if (isspace(buf[0]))
			continue;

		/* Skip comments */
		if (buf[0] == '#')
			continue;


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
		if (bypass)
			continue;


		/* Process "%:<file>" */
		if (buf[0] == '%')
		{
			/* Process that file if allowed */
			(void) process_pref_file(buf + 2);

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_pref_file_aux(buf);

		/* Oops */
		if (err)
			break;
	}


	/* Error */
	if (err)
	{
		/* Useful error message */
		msg_format("Error %d in line %d of file '%s'.", err, num, name);
		msg_format("Parsing '%s'", buf);
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
static char days[7][29] = {
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
	if (!check_time_flag)
		return (0);

	/* Check for time violation */
	c = time((time_t *) 0);
	tp = localtime(&c);

	/* Violation */
	if (days[tp->tm_wday][tp->tm_hour + 4] != 'X')
		return (1);

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
	if (!fp)
		return (0);

	/* Assume restrictions */
	check_time_flag = TRUE;

	/* Parse the file */
	while (0 == my_fgets(fp, buf, 80))
	{
		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Chop the buffer */
		buf[29] = '\0';

		/* Extract the info */
		if (prefix(buf, "SUN:"))
			strcpy(days[0], buf);
		if (prefix(buf, "MON:"))
			strcpy(days[1], buf);
		if (prefix(buf, "TUE:"))
			strcpy(days[2], buf);
		if (prefix(buf, "WED:"))
			strcpy(days[3], buf);
		if (prefix(buf, "THU:"))
			strcpy(days[4], buf);
		if (prefix(buf, "FRI:"))
			strcpy(days[5], buf);
		if (prefix(buf, "SAT:"))
			strcpy(days[6], buf);
	}

	/* Close it */
	my_fclose(fp);

#endif

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
	unsigned int v_pgpgin;
	unsigned int v_pgpgout;
	unsigned int v_pswpin;
	unsigned int v_pswpout;
	unsigned int v_intr;
	int if_ipackets;
	int if_ierrors;
	int if_opackets;
	int if_oerrors;
	int if_collisions;
	unsigned int v_swtch;
	long avenrun[3];
	struct timeval boottime;
	struct timeval curtime;
};

/*
 * Maximal load (if any).
 */
static int check_load_value = 0;

#endif


/*
 * Handle CHECK_LOAD
 */
errr check_load(void)
{

#ifdef CHECK_LOAD

	struct statstime st;

	/* Success if not checking */
	if (!check_load_value)
		return (0);

	/* Check the load */
	if (0 == rstat("localhost", &st))
	{
		long val1 = (long) (st.avenrun[2]);
		long val2 = (long) (check_load_value) * FSCALE;

		/* Check for violation */
		if (val1 >= val2)
			return (1);
	}

#endif

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

	char temphost[MAXHOSTNAMELEN + 1];
	char thishost[MAXHOSTNAMELEN + 1];


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, "load.txt");

	/* Open the "load" file */
	fp = my_fopen(buf, "r");

	/* No file, no restrictions */
	if (!fp)
		return (0);

	/* Default load */
	check_load_value = 100;

	/* Get the host name */
	(void) gethostname(thishost, (sizeof thishost) - 1);

	/* Parse it */
	while (0 == my_fgets(fp, buf, 1024))
	{
		int value;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Parse, or ignore */
		if (sscanf(buf, "%s%d", temphost, &value) != 2)
			continue;

		/* Skip other hosts */
		if (!streq(temphost, thishost) && !streq(temphost, "localhost"))
			continue;

		/* Use that value */
		check_load_value = value;

		/* Done */
		break;
	}

	/* Close the file */
	my_fclose(fp);

#endif

	/* Success */
	return (0);
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
	sprintf(out_val, "%9ld", (long) num);
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
	sprintf(out_val, "%6ld", (long) num);
	c_put_str(color, out_val, row, col + len + 3);
}


/*
 * Prints the following information on the screen.
 *
 * For this to look right, the following should be spaced the
 * same as in the prt_lnum code... -CFT
 */
static void display_player_middle(void)
{
	int show_tohit = p_ptr->dis_to_h;
	int show_todam = p_ptr->dis_to_d;

	int sane_p;

	object_type *o_ptr = equipment[EQUIP_WIELD];

	if (p_ptr->msane == 0)
	{
		sane_p = 100;
	}
	else
	{
		sane_p = (100 * p_ptr->csane) / p_ptr->msane;
	}

	/* Hack -- add in weapon info if known */
	if (o_ptr && object_known_p(o_ptr))
		show_tohit += o_ptr->to_h;
	if (o_ptr && object_known_p(o_ptr))
		show_todam += o_ptr->to_d;

	/* Dump the bonuses to hit/dam */
	prt_num("+ To Hit    ", show_tohit, 9, 1, TERM_L_BLUE);
	prt_num("+ To Damage ", show_todam, 10, 1, TERM_L_BLUE);

	/* Dump the armor class bonus */
	prt_num("+ To AC     ", p_ptr->dis_to_a, 11, 1, TERM_L_BLUE);

	/* Dump the total armor class */
	prt_num("  Base AC   ", p_ptr->dis_ac, 12, 1, TERM_L_BLUE);

	prt_num("Level      ", (int) p_ptr->lev, 9, 28, TERM_L_GREEN);

	if (p_ptr->exp >= p_ptr->max_exp)
	{
		prt_lnum("Experience ", p_ptr->exp, 10, 28, TERM_L_GREEN);
	}
	else
	{
		prt_lnum("Experience ", p_ptr->exp, 10, 28, TERM_YELLOW);
	}

	prt_lnum("Max Exp    ", p_ptr->max_exp, 11, 28, TERM_L_GREEN);

	if (p_ptr->lev >= PY_MAX_LEVEL)
	{
		put_str("Exp to Adv.", 12, 28);
		c_put_str(TERM_L_GREEN, "    *****", 12, 28 + 11);
	}
	else
	{
		prt_lnum("Exp to Adv.",
			(s32b) (player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L),
			12, 28, TERM_L_GREEN);
	}

	prt_lnum("Gold       ", p_ptr->au, 13, 28, TERM_L_GREEN);

	prt_num("Max Hit Points ", p_ptr->mhp, 9, 52, TERM_L_GREEN);

	if (p_ptr->chp >= p_ptr->mhp)
	{
		prt_num("Cur Hit Points ", p_ptr->chp, 10, 52, TERM_L_GREEN);
	}
	else if (p_ptr->chp > (p_ptr->mhp * op_ptr->hitpoint_warn) / 10)
	{
		prt_num("Cur Hit Points ", p_ptr->chp, 10, 52, TERM_YELLOW);
	}
	else
	{
		prt_num("Cur Hit Points ", p_ptr->chp, 10, 52, TERM_RED);
	}

	prt_num("Max SP (Mana)  ", p_ptr->msp, 11, 52, TERM_L_GREEN);

	if (p_ptr->csp >= p_ptr->msp)
	{
		prt_num("Cur SP (Mana)  ", p_ptr->csp, 12, 52, TERM_L_GREEN);
	}
	else if (p_ptr->csp > (p_ptr->msp * op_ptr->hitpoint_warn) / 10)
	{
		prt_num("Cur SP (Mana)  ", p_ptr->csp, 12, 52, TERM_YELLOW);
	}
	else
	{
		prt_num("Cur SP (Mana)  ", p_ptr->csp, 12, 52, TERM_RED);
	}

	if (sane_p >= 100)
	{
		prt_num("Sanity         ", p_ptr->csane, 13, 52, TERM_L_GREEN);
	}
	else if (sane_p > (op_ptr->hitpoint_warn * 10))
	{
		prt_num("Sanity         ", p_ptr->csane, 13, 52, TERM_YELLOW);
	}
	else
	{
		prt_num("Sanity         ", p_ptr->csane, 13, 52, TERM_RED);
	}
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
	if (y <= 0)
		y = 1;

	/* Negative value */
	if (x < 0)
	{
		likert_color = TERM_RED;
		return ("Very Bad");
	}

	/* Analyze the value */
	switch ((x / y))
	{
		case 0:
		case 1:
		{
			likert_color = TERM_RED;
			return ("Bad");
		}
		case 2:
		{
			likert_color = TERM_RED;
			return ("Poor");
		}
		case 3:
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
		{
			likert_color = TERM_YELLOW;
			return ("Very Good");
		}
		case 7:
		case 8:
		{
			likert_color = TERM_L_GREEN;
			return ("Excellent");
		}
		case 9:
		case 10:
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
			likert_color = TERM_L_GREEN;
			return ("Heroic");
		}
		default:
		{
			likert_color = TERM_L_GREEN;
			return ("Legendary");
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
	int tmp;
	int xthn, xthb, xfos, xsrh;
	int xdis, xdev, xsav, xstl;
	cptr desc;

	object_type *o_ptr;


	/* Fighting Skill (with current weapon) */
	o_ptr = equipment[EQUIP_WIELD];
	tmp = p_ptr->to_h;

	if (o_ptr)
	{
		tmp += o_ptr->to_h;
	}

	xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

	/* Shooting Skill (with current bow and normal missile) */
	o_ptr = equipment[EQUIP_BOW];
	tmp = p_ptr->to_h;

	if (o_ptr)
	{
		tmp += o_ptr->to_h;
	}

	xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

	/* Basic abilities */
	xdis = p_ptr->skill_dis;
	xdev = p_ptr->skill_dev;
	xsav = p_ptr->skill_sav;
	xstl = p_ptr->skill_stl;
	xsrh = p_ptr->skill_srh;
	xfos = p_ptr->skill_fos;


	put_str("Fighting    :", 16, 1);
	desc = likert(xthn, 12);
	c_put_str(likert_color, desc, 16, 15);

	put_str("Bows/Throw  :", 17, 1);
	desc = likert(xthb, 12);
	c_put_str(likert_color, desc, 17, 15);

	put_str("Saving Throw:", 18, 1);
	desc = likert(xsav, 6);
	c_put_str(likert_color, desc, 18, 15);

	put_str("Stealth     :", 19, 1);
	desc = likert(xstl, 1);
	c_put_str(likert_color, desc, 19, 15);


	put_str("Perception  :", 16, 28);
	desc = likert(xfos, 6);
	c_put_str(likert_color, desc, 16, 42);

	put_str("Searching   :", 17, 28);
	desc = likert(xsrh, 6);
	c_put_str(likert_color, desc, 17, 42);

	put_str("Disarming   :", 18, 28);
	desc = likert(xdis, 8);
	c_put_str(likert_color, desc, 18, 42);

	put_str("Magic Device:", 19, 28);
	desc = likert(xdev, 6);
	c_put_str(likert_color, desc, 19, 42);


	put_str("Blows/Round:", 16, 55);
	put_str(format("%d", p_ptr->num_blow), 16, 69);

	put_str("Shots/Round:", 17, 55);
	put_str(format("%d", p_ptr->num_fire), 17, 69);

	put_str("Infra-Vision:", 19, 55);
	put_str(format("%d feet", p_ptr->see_infra * 10), 19, 69);
}



/*
 * Obtain the "flags" for the player as if he was an item
 */
static void player_flags(u32b * f1, u32b * f2, u32b * f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Elf */
	if (p_ptr->prace == RACE_ELF)
		(*f2) |= (TR2_RES_LITE);

	/* Hobbit */
	if (p_ptr->prace == RACE_HOBBIT)
		(*f2) |= (TR2_SUST_DEX);

	/* Gnome */
	if (p_ptr->prace == RACE_GNOME)
		(*f3) |= (TR3_FREE_ACT);

	/* Dwarf */
	if (p_ptr->prace == RACE_DWARF)
		(*f2) |= (TR2_RES_BLIND);

	/* Half-Orc */
	if (p_ptr->prace == RACE_HALF_ORC)
		(*f2) |= (TR2_RES_DARK);

	/* Half-Troll */
	if (p_ptr->prace == RACE_HALF_TROLL)
		(*f2) |= (TR2_SUST_STR);

	/* Dunadan */
	if (p_ptr->prace == RACE_DUNADAN)
		(*f2) |= (TR2_SUST_CON);

	/* High Elf */
	if (p_ptr->prace == RACE_HIGH_ELF)
		(*f2) |= (TR2_RES_LITE);
	if (p_ptr->prace == RACE_HIGH_ELF)
		(*f3) |= (TR3_SEE_INVIS);

	if (p_ptr->prace == RACE_KOBOLD)
		(*f2) |= TR2_RES_POIS;
	if (p_ptr->prace == RACE_MUTANT)
	{
		(*f2) |= TR2_RES_CHAOS;
		switch (p_ptr->prace_info)
		{
			case 1:
				(*f2) |= TR2_RES_POIS;
				break;
			case 2:
				(*f2) |= TR2_RES_BLIND;
				break;
			case 3:
				(*f2) |= TR2_RES_ACID;
				break;
			case 4:
				(*f2) |= TR2_RES_ELEC;
				break;
			case 5:
				(*f2) |= TR2_RES_FEAR;
				break;
			default:
				break;
		}
	}

	if (p_ptr->prace == RACE_GOLEM)
	{
		(*f2) |= TR2_RES_FEAR;
		(*f2) |= TR2_RES_CONFU;
	}

	if (p_ptr->prace == RACE_LEPRECHAUN)
	{
		(*f2) |= TR2_RES_BLIND;
		(*f2) |= TR2_SUST_DEX;
	}

	if (p_ptr->prace == RACE_MOLD)
	{
		(*f2) |= TR2_RES_NETHR;
		(*f2) |= TR2_RES_NEXUS;
		(*f3) |= TR3_HOLD_LIFE;
	}

	if (p_ptr->prace == RACE_GHOST)
	{
		(*f3) |= TR3_HOLD_LIFE;
		(*f2) |= TR2_RES_NETHR;
	}

	if (p_ptr->prace == RACE_VORTEX)
	{
		(*f2) |= TR2_IM_ACID;
		(*f2) |= TR2_IM_ELEC;
		(*f2) |= TR2_IM_FIRE;
		(*f2) |= TR2_IM_COLD;
	}
}

/*
 * Equippy chars
 */
static void display_player_equippy(int y, int x)
{
	int i;

	byte a;
	char c;

	object_type *o_ptr;


	/* Dump equippy chars */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		/* Object */
		o_ptr = equipment[i];

		/* Skip empty objects */
		if (!o_ptr || !o_ptr->k_idx)
			continue;

		/* Get attr/char for display */
		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Dump */
		Term_putch(x + i - EQUIP_WIELD, y, a, c);
	}
}


/*
 * Hack -- see below
 */
static byte display_player_flag_set[4] = {
	2,
	2,
	3,
	1
};

/*
 * Hack -- see below
 */
static u32b display_player_flag_head[4] = {
	TR2_RES_ACID,
	TR2_RES_BLIND,
	TR3_SLOW_DIGEST,
	TR1_STEALTH
};

/*
 * Hack -- see below
 */
static cptr display_player_flag_names[4][8] = {
	{
			" Acid:", /* TR2_RES_ACID */
			" Elec:", /* TR2_RES_ELEC */
			" Fire:", /* TR2_RES_FIRE */
			" Cold:", /* TR2_RES_COLD */
			" Pois:", /* TR2_RES_POIS */
			" Fear:", /* TR2_RES_FEAR */
			" Lite:", /* TR2_RES_LITE */
			" Dark:" /* TR2_RES_DARK */
		},

	{
			"Blind:", /* TR2_RES_BLIND */
			"Confu:", /* TR2_RES_CONFU */
			"Sound:", /* TR2_RES_SOUND */
			"Shard:", /* TR2_RES_SHARD */
			"Nexus:", /* TR2_RES_NEXUS */
			"Nethr:", /* TR2_RES_NETHR */
			"Chaos:", /* TR2_RES_CHAOS */
			"Disen:" /* TR2_RES_DISEN */
		},

	{
			"S.Dig:", /* TR3_SLOW_DIGEST */
			"Feath:", /* TR3_FEATHER */
			"PLite:", /* TR3_LITE */
			"Regen:", /* TR3_REGEN */
			"Telep:", /* TR3_TELEPATHY */
			"Invis:", /* TR3_SEE_INVIS */
			"FrAct:", /* TR3_FREE_ACT */
			"HLife:" /* TR3_HOLD_LIFE */
		},

	{
			"Stea.:", /* TR1_STEALTH */
			"Sear.:", /* TR1_SEARCH */
			"Infra:", /* TR1_INFRA */
			"Tunn.:", /* TR1_TUNNEL */
			"Speed:", /* TR1_SPEED */
			"Blows:", /* TR1_BLOWS */
			"Shots:", /* TR1_SHOTS */
			"Might:" /* TR1_MIGHT */
		}
};


/*
 * Special display, part 1
 */
static void display_player_flag_info(void)
{
	int x, y, i, n;

	int row, col;

	int set;
	u32b head;
	u32b flag;
	cptr name;

	u32b f[4];


	/* Four columns */
	for (x = 0; x < 4; x++)
	{
		/* Reset */
		row = 10;
		col = 20 * x;

		/* Extract set */
		set = display_player_flag_set[x];

		/* Extract head */
		head = display_player_flag_head[x];

		/* Header */
		display_player_equippy(row++, col + 6);
		c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 6);

		/* Eight rows */
		for (y = 0; y < 8; y++)
		{
			/* Extract flag */
			flag = (head << y);

			/* Extract name */
			name = display_player_flag_names[x][y];

			/* Header */
			c_put_str(TERM_WHITE, name, row, col);

			/* Check equipment */
			for (n = 6, i = 0; i < EQUIP_MAX; i++, n++)
			{
				byte attr = TERM_SLATE;

				object_type *o_ptr = equipment[i];

				/* Known flags */
				if (o_ptr)
					object_flags_known(o_ptr, &f[1], &f[2], &f[3]);

				/* Color columns by parity */
				if (i % 2)
					attr = TERM_L_WHITE;

				/* Non-existant objects */
				if (!o_ptr || !o_ptr->k_idx)
					attr = TERM_L_DARK;

				/* Default */
				c_put_str(attr, ".", row, col + n);

				/* Check flags */
				if (f[set] & flag && o_ptr)
					c_put_str(TERM_WHITE, "+", row, col + n);
			}

			/* Player flags */
			player_flags(&f[1], &f[2], &f[3]);

			/* Default */
			c_put_str(TERM_SLATE, ".", row, col + n);

			/* Check flags */
			if (f[set] & flag)
				c_put_str(TERM_WHITE, "+", row, col + n);

			/* Advance */
			row++;
		}

		/* Footer */
		c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 6);
		display_player_equippy(row++, col + 6);
	}
}


/*
 * Special display, part 2a
 */
static void display_player_misc_info(void)
{
	char buf[80];

	/* Display basics */
	put_str("Name :", 2, 1);
	put_str("Sex  :", 3, 1);
	put_str("Race :", 4, 1);
	put_str("Class:", 5, 1);

	c_put_str(TERM_L_BLUE, op_ptr->full_name, 2, 8);
	c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 8);
	c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 8);
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 8);

	/* Display extras */
	put_str("Level:", 6, 1);
	put_str("HP   :", 7, 1);
	put_str("SP   :", 8, 1);

	sprintf(buf, "%d", p_ptr->lev);
	c_put_str(TERM_L_BLUE, buf, 6, 8);
	sprintf(buf, "%d/%d", p_ptr->chp, p_ptr->mhp);
	c_put_str(TERM_L_BLUE, buf, 7, 8);
	sprintf(buf, "%d/%d", p_ptr->csp, p_ptr->msp);
	c_put_str(TERM_L_BLUE, buf, 8, 8);
}


/*
 * Special display, part 2b
 *
 * How to print out the modifications and sustains.
 * Positive mods with no sustain will be light green.
 * Positive mods with a sustain will be dark green.
 * Sustains (with no modification) will be a dark green 's'.
 * Negative mods (from a curse) will be red.
 * Huge mods (>9), like from MICoMorgoth, will be a '*'
 * No mod, no sustain, will be a slate '.'
 */
static void display_player_stat_info(void)
{
	int i, row, col;
	int stat_col, stat;

	object_type *o_ptr;
	u32b f1, f2, f3;
	u32b ignore_f2, ignore_f3;
	s16b k_idx;

	byte a;
	char c;

	char buf[80];


	/* Column */
	stat_col = 24;

	/* Row */
	row = 3;

	/* Print out the labels for the columns */
	c_put_str(TERM_WHITE, "Stat", row - 1, stat_col);
	c_put_str(TERM_BLUE, "Intrnl", row - 1, stat_col + 5);
	c_put_str(TERM_L_BLUE, "Rce Cls Eqp", row - 1, stat_col + 12);
	c_put_str(TERM_L_GREEN, "Actual", row - 1, stat_col + 24);
	c_put_str(TERM_YELLOW, "Currnt", row - 1, stat_col + 31);

	/* Display the stats */
	for (i = 0; i < 6; i++)
	{
		/* Reduced name of stat */
		c_put_str(TERM_WHITE, stat_names_reduced[i], row + i, stat_col);

		/* Internal "natural" maximum value */
		cnv_stat(p_ptr->stat_max[i], buf);
		c_put_str(TERM_BLUE, buf, row + i, stat_col + 5);

		/* Race, class, and equipment modifiers */
		sprintf(buf, "%3d", rp_ptr->r_adj[i]);
		c_put_str(TERM_L_BLUE, buf, row + i, stat_col + 12);
		sprintf(buf, "%3d", cp_ptr->c_adj[i]);
		c_put_str(TERM_L_BLUE, buf, row + i, stat_col + 16);
		sprintf(buf, "%3d", p_ptr->stat_add[i]);
		c_put_str(TERM_L_BLUE, buf, row + i, stat_col + 20);

		/* Resulting "modified" maximum value */
		cnv_stat(p_ptr->stat_top[i], buf);
		c_put_str(TERM_L_GREEN, buf, row + i, stat_col + 24);

		/* Only display stat_use if not maximal */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			cnv_stat(p_ptr->stat_use[i], buf);
			c_put_str(TERM_YELLOW, buf, row + i, stat_col + 31);
		}
	}

	/* Column */
	col = 60 + 6;

	/* Header and Footer */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row - 1, col);

	/* Process equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		/* Access object */
		o_ptr = equipment[i];

		/* Hack -- nothing in the slot. */
		if (!o_ptr)
		{

			/* Dump proper characters. */
			for (stat = 0; stat < 6; stat++)
			{
				/* Dump proper character */
				Term_putch(col, row + stat, TERM_SLATE, '.');
			}

			/* Advance. */
			col++;
			continue;
		}

		/* Object kind */
		k_idx = o_ptr->k_idx;

		/* Acquire "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Hack -- assume stat modifiers are known */
		object_flags(o_ptr, &f1, &ignore_f2, &ignore_f3);

		/* Initialize color based of sign of pval. */
		for (stat = 0; stat < 6; stat++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';

			/* Sustain */
			if (f2 & 1 << stat)
			{
				/* Dark green "s" */
				a = TERM_GREEN;
				c = 's';
			}

			/* Boost */
			if (f1 & 1 << stat)
			{
				/* Default */
				c = '*';

				/* Good */
				if (o_ptr->pval > 0)
				{
					/* Good */
					if (a != TERM_GREEN)
						a = TERM_L_GREEN;

					/* Label boost */
					if (o_ptr->pval < 10)
						c = '0' + o_ptr->pval;
				}

				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Bad */
					if (a != TERM_GREEN)
						a = TERM_RED;

					/* Label boost */
					if (o_ptr->pval < 10)
						c = '0' - o_ptr->pval;
				}
			}


			/* Dump proper character */
			Term_putch(col, row + stat, a, c);
		}

		/* Advance */
		col++;
	}

	/* Player flags */
	player_flags(&f1, &f2, &f3);

	/* Check stats */
	for (stat = 0; stat < 6; stat++)
	{
		/* Default */
		a = TERM_SLATE;
		c = '.';

		/* Sustain */
		if (f2 & 1 << stat)
		{
			/* Dark green "s" */
			a = TERM_GREEN;
			c = 's';
		}

		/* Dump */
		Term_putch(col, row + stat, a, c);
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
 */
void display_player(int mode)
{
	int i;

	char buf[80];


	/* XXX XXX XXX */
	mode = (mode % 3);


	/* Erase screen */
	clear_from(0);

	/* Standard */
	if ((mode == 0) || (mode == 1))
	{
		/* Name, Sex, Race, Class */
		put_str("Name        :", 2, 1);
		put_str("Sex         :", 3, 1);
		put_str("Race        :", 4, 1);
		put_str("Class       :", 5, 1);

		c_put_str(TERM_L_BLUE, op_ptr->full_name, 2, 15);
		c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 15);
		c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 15);
		c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

		/* Age, Height, Weight, Social */
		prt_num("Age          ", (int) p_ptr->age, 2, 32, TERM_L_BLUE);
		prt_num("Height       ", (int) p_ptr->ht, 3, 32, TERM_L_BLUE);
		prt_num("Weight       ", (int) p_ptr->wt, 4, 32, TERM_L_BLUE);
		prt_num("Social Class ", (int) p_ptr->sc, 5, 32, TERM_L_BLUE);

		/* Display the stats */
		for (i = 0; i < 6; i++)
		{
			/* Special treatment of "injured" stats */
			if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
			{
				int value;

				/* Use lowercase stat name */
				put_str(stat_names_reduced[i], 2 + i, 61);

				/* Get the current stat */
				value = p_ptr->stat_use[i];

				/* Obtain the current stat (modified) */
				cnv_stat(value, buf);

				/* Display the current stat (modified) */
				c_put_str(TERM_YELLOW, buf, 2 + i, 66);

				/* Acquire the max stat */
				value = p_ptr->stat_top[i];

				/* Obtain the maximum stat (modified) */
				cnv_stat(value, buf);

				/* Display the maximum stat (modified) */
				c_put_str(TERM_L_GREEN, buf, 2 + i, 73);
			}

			/* Normal treatment of "normal" stats */
			else
			{
				/* Assume uppercase stat name */
				put_str(stat_names[i], 2 + i, 61);

				/* Obtain the current stat (modified) */
				cnv_stat(p_ptr->stat_use[i], buf);

				/* Display the current stat (modified) */
				c_put_str(TERM_L_GREEN, buf, 2 + i, 66);
			}
		}

		/* Extra info */
		display_player_middle();

		/* Display "history" info */
		if (mode == 1)
		{
			put_str("(Character Background)", 15, 25);

			for (i = 0; i < 4; i++)
			{
				put_str(p_ptr->history[i], i + 16, 10);
			}
		}

		/* Display "various" info */
		else
		{
			put_str("(Miscellaneous Abilities)", 15, 25);

			display_player_various();
		}
	}

	/* Special */
	else if (mode == 2)
	{
		/* See "http://www.cs.berkeley.edu/~davidb/angband.html" */

		/* Dump the info */
		display_player_misc_info();
		display_player_stat_info();
		display_player_flag_info();
	}
}



/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(cptr name)
{
	int i, x, y;

	byte a;
	char c;

#if 0
	cptr other = "(";
#endif

	cptr paren = ")";

	int fd = -1;

	FILE *fff = NULL;

	store_type *st_ptr = &store[7];

	char o_name[80];

	char buf[1024];

	object_type *o_ptr;

	bool equip_p = FALSE;


	/* Drop priv's */
	safe_setuid_drop();

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
		sprintf(out_val, "Replace existing file %s? ", buf);

		/* Ask */
		if (get_check(out_val))
			fd = -1;
	}

	/* Open the non-existing file */
	if (fd < 0)
		fff = my_fopen(buf, "w");

	/* Grab priv's */
	safe_setuid_grab();


	/* Invalid file */
	if (!fff)
	{
		/* Message */
		msg_format("Character dump failed!");
		msg_print(NULL);

		/* Error */
		return (-1);
	}


	/* Begin dump */
	fprintf(fff, "  [Kamband %d.%d Character Dump]\n\n", KAM_VERSION_MAJOR,
		KAM_VERSION_MINOR);


	/* Display player */
	display_player(0);

	/* Dump part of the screen */
	for (y = 2; y < 22; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void) (Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Display history */
	display_player(1);

	/* Dump part of the screen */
	for (y = 15; y < 20; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void) (Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	fprintf(fff, "\n\n");

	/* Display resistances */
	display_player(2);

	/* Dump part of the screen */
	for (y = 2; y < 22; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void) (Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Skip some lines */
	fprintf(fff, "\n\n");


	/* Dump misc. player info. */
	fprintf(fff, "  Preserve mode: %s\n",
		(p_ptr->preserve ? "on" : "off"));
	fprintf(fff, "  Maximise mode: %s\n",
		(p_ptr->maximize ? "on" : "off"));

	switch (p_ptr->inside_special) {
	case SPECIAL_WILD:
	  fprintf(fff, "  Currently on the wilderness level (%d, %d).\n",
		  p_ptr->wild_x, p_ptr->wild_y);
	  break;

	case SPECIAL_QUEST:
	  fprintf(fff, "  Currently questing.\n");
	  break;

	case SPECIAL_STORE:
	  fprintf(fff, "  Currently shopping.\n");
	  break;

	case SPECIAL_ARENA:
	case SPECIAL_MAGIC_ARENA:
	  fprintf(fff, "  Currently in the arena.\n");
	  break;

	default:
	  fprintf(fff, "  Current depth: %d\n", p_ptr->depth);
	  break;
	}

	fprintf(fff, "  Maximal depth: %d\n", p_ptr->max_depth);
	fprintf(fff, "  Player died from: %s\n\n", p_ptr->died_from);

	if (p_ptr->total_winner)
	{
		fprintf(fff, "Player has won the game.\n\n");
	}

	if (p_ptr->wizard)
	{
		fprintf(fff, "Player has used wizard mode.\n\n");
	}

	if (p_ptr->noscore & 0x0008)
	{
		fprintf(fff, "Player has used debug commands.\n\n");
	}

	for (i = 0; i < CHEAT_MAX; i++)
	{
		if (p_ptr->cheat[i])
		{
			fprintf(fff, "Player has cheated.\n\n");
		}
	}


	/* Dump religious info */
	if (p_ptr->pgod)
	{
		int pgod = p_ptr->pgod - 1;
		int badness = interpret_grace();

		deity *d_ptr = &deity_info[pgod];

		fprintf(fff,
			"Player worships %s, the %s God%s of %s, "
			"and is %s by him.\n\n", d_ptr->name,
			deity_rarity[d_ptr->rarity], 
			(d_ptr->female ? "dess" : ""),
			d_ptr->god_of,
			deity_standing[badness]);
	}

	/* Dump shape info */
	if (p_ptr->shape)
	{
		fprintf(fff, "Player is in the form of a%s %s.\n\n",
			(is_a_vowel(shape_info[p_ptr->shape - 1].name[0]) ? "n" : ""),
			shape_info[p_ptr->shape - 1].name);
	}


	/* Dump the mutations */

	if (p_ptr->mutations1 || p_ptr->mutations2 || p_ptr->mutations3)
	{
		fprintf(fff, "  [Character Mutations]\n\n");

		for (i = 0; i < 32; i++)
		{
			if (p_ptr->mutations1 & (1L << i))
			{
				fprintf(fff, "%s\n", mutation_names[i][0]);
			}

			if (p_ptr->mutations2 & (1L << i))
			{
				fprintf(fff, "%s\n", mutation_names[i + 32][0]);
			}

			if (p_ptr->mutations3 & (1L << i))
			{
				fprintf(fff, "%s\n", mutation_names[i + 64][0]);
			}
		}

		fprintf(fff, "\n\n");
	}

	/* Dump the random spells */
	if (spell_num)
	{
		spell *rspell;

		fprintf(fff, "  [Character Spells]\n\n");

		for (i = 0; i < spell_num; i++)
		{
			rspell = &spells[i];

			if (rspell->unknown)
				continue;

			if (rspell->untried)
			{
				fprintf(fff, "%3d%s %-30s (Spell Untried)\n", i + 1, paren,
					rspell->name);

			}
			else
			{
				fprintf(fff, "%3d%s %-30s %4d%% %3d (%s)\n", i + 1, paren,
					rspell->name, spell_chance(rspell), rspell->mana,
					rspell->desc);
			}
		}

		fprintf(fff, "\n\n");
	}

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i])
		{
			equip_p = TRUE;
			break;
		}
	}

	/* Dump the equipment */
	if (equip_p)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = 0; i < EQUIP_MAX; i++)
		{
			object_desc_mode |= ODESC_SLOT;
			object_desc(o_name, equipment[i], TRUE, 3);
			fprintf(fff, "  %-14s: %s\n", mention_use(i), o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	if (inventory)
	{
		fprintf(fff, "  [Character Inventory]\n\n");
		for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
		{
			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "  %s\n", o_name);
		}

		fprintf(fff, "\n\n");

		fprintf(fff, "  Total weight: %d.%1d\n\n",
			p_ptr->total_weight / 10, p_ptr->total_weight % 10);

	}

	/* Dump the Home (page 1) */
	if (st_ptr->stock)
	{
		fprintf(fff, "  [Home Inventory]\n\n");

		for (o_ptr = st_ptr->stock; o_ptr != NULL;
			o_ptr = o_ptr->next_global)
		{

			object_desc(o_name, o_ptr, TRUE, 3);
			fprintf(fff, "  %s\n", o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Close it */
	my_fclose(fff);

	/* Success */
	return (0);
}



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
bool show_file(cptr name, cptr what, int line, int mode)
{
	int i, k;

	/* Number of "real" lines passed by */
	int next = 0;

	/* Number of "real" lines in the file */
	int size = 0;

	/* Backup value for "line" */
	int back = 0;

	/* This screen has sub-screens */
	bool menu = FALSE;

	/* Current help file */
	FILE *fff = NULL;

	/* Find this string (if any) */
	cptr find = NULL;

	/* Hold a string to find */
	char finder[128];

	/* Hold a string to show */
	char shower[128];

	/* Describe this thing */
	char caption[128];

	/* Path buffer */
	char path[1024];

	/* General buffer */
	char buf[1024];

	/* Sub-menu information */
	char hook[10][32];


	/* Wipe finder */
	strcpy(finder, "");

	/* Wipe shower */
	strcpy(shower, "");

	/* Wipe caption */
	strcpy(caption, "");

	/* Wipe the hooks */
	for (i = 0; i < 10; i++)
		hook[i][0] = '\0';


	/* Hack XXX XXX XXX */
	if (what)
	{
		/* Caption */
		strcpy(caption, what);

		/* Access the "file" */
		strcpy(path, name);

		/* Open */
		fff = my_fopen(path, "r");
	}

	/* Look in "help" */
	if (!fff)
	{
		/* Caption */
		sprintf(caption, "Help file '%s'", name);

		/* Build the filename */
		path_build(path, 1024, ANGBAND_DIR_HELP, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Look in "info" */
	if (!fff)
	{
		/* Caption */
		sprintf(caption, "Info file '%s'", name);

		/* Build the filename */
		path_build(path, 1024, ANGBAND_DIR_INFO, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Oops */
	if (!fff)
	{
		/* Message */
		msg_format("Cannot open '%s'.", name);
		msg_print(NULL);

		/* Oops */
		return (TRUE);
	}


	/* Pre-Parse the file */
	while (TRUE)
	{
		/* Read a line or stop */
		if (my_fgets(fff, buf, 1024))
			break;

		/* XXX Parse "menu" items */
		if (prefix(buf, "***** "))
		{
			char b1 = '[', b2 = ']';

			/* Notice "menu" requests */
			if ((buf[6] == b1) && isdigit(buf[7]) && (buf[8] == b2) &&
				(buf[9] == ' '))
			{
				/* This is a menu file */
				menu = TRUE;

				/* Extract the menu item */
				k = buf[7] - '0';

				/* Extract the menu item */
				strcpy(hook[k], buf + 10);
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
		if (line >= size)
			line = 0;


		/* Re-open the file if needed */
		if (next > line)
		{
			/* Close it */
			my_fclose(fff);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");

			/* Oops */
			if (!fff)
				return (FALSE);

			/* File has been restarted */
			next = 0;
		}

		/* Skip lines if needed */
		for (; next < line; next++)
		{
			/* Skip a line */
			if (my_fgets(fff, buf, 1024))
				break;
		}


		/* Dump the next screenful of lines of the file */
		for (i = 0; i < screen_y - 4;)
		{
			/* Hack -- track the "first" line */
			if (!i)
				line = next;

			/* Get a line of the file or stop */
			if (my_fgets(fff, buf, 1024))
				break;

			/* Hack -- skip "special" lines */
			if (prefix(buf, "***** "))
				continue;

			/* Count the "real" lines */
			next++;

			/* Hack -- keep searching */
			if (find && !i && !strstr(buf, find))
				continue;

			/* Hack -- stop searching */
			find = NULL;

			/* Dump the line */
			Term_putstr(0, i + 2, -1, TERM_WHITE, buf);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = buf;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str - buf, i + 2, len, TERM_YELLOW,
						shower);

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
			bell();
			line = back;
			find = NULL;
			continue;
		}


		/* Show a general "title" */
		prt(format("[Kamband %d.%d, %s, Line %d/%d]", KAM_VERSION_MAJOR,
				KAM_VERSION_MINOR, caption, line, size), 0, 0);


		/* Prompt -- menu screen */
		if (menu)
		{
			/* Wait for it */
			prt("[Press a Number, or ESC to exit.]", screen_y - 1, 0);
		}

		/* Prompt -- small files */
		else if (size <= screen_y - 4)
		{
			/* Wait for it */
			prt("[Press ESC to exit.]", screen_y - 1, 0);
		}

		/* Prompt -- large files */
		else
		{
			/* Wait for it */
			prt("[Press Return, Space, -, =, /, or ESC to exit.]",
				screen_y - 1, 0);
		}

		/* Get a keypress */
		k = inkey();

		/* Hack -- return to last screen */
		if (k == '?')
			break;

		/* Hack -- try showing */
		if (k == '=')
		{
			/* Get "shower" */
			prt("Show: ", screen_y - 1, 0);
			(void) askfor_aux(shower, 80, FALSE);
		}

		/* Hack -- try finding */
		if (k == '/')
		{
			/* Get "finder" */
			prt("Find: ", screen_y - 1, 0);
			if (askfor_aux(finder, 80, FALSE))
			{
				/* Find it */
				find = finder;
				back = line;
				line = line + 1;

				/* Show it */
				strcpy(shower, finder);
			}
		}

		/* Hack -- go to a specific line */
		if (k == '#')
		{
			char tmp[80];
			prt("Goto Line: ", screen_y - 1, 0);
			strcpy(tmp, "0");
			if (askfor_aux(tmp, 80, FALSE))
			{
				line = atoi(tmp);
			}
		}

		/* Hack -- go to a specific file */
		if (k == '%')
		{
			char tmp[80];
			prt("Goto File: ", screen_y - 1, 0);
			strcpy(tmp, "help.hlp");
			if (askfor_aux(tmp, 80, FALSE))
			{
				if (!show_file(tmp, NULL, 0, mode))
					k = ESCAPE;
			}
		}

		/* Hack -- Allow backing up */
		if (k == '-')
		{
			line = line - 10;
			if (line < 0)
				line = 0;
		}

		/* Hack -- Advance a single line */
		if ((k == '\n') || (k == '\r'))
		{
			line = line + 1;
		}

		/* Advance one page */
		if (k == ' ')
		{
			line = line + 20;
		}

		/* Recurse on numbers */
		if (menu && isdigit(k) && hook[k - '0'][0])
		{
			/* Recurse on that file */
			if (!show_file(hook[k - '0'], NULL, 0, mode))
				k = ESCAPE;
		}

		/* Exit on escape */
		if (k == ESCAPE)
			break;
	}

	/* Close the file */
	my_fclose(fff);

	/* Escape */
	if (k == ESCAPE)
		return (FALSE);

	/* Normal return */
	return (TRUE);
}


/*
 * Peruse the On-Line-Help
 */
void do_cmd_help(void)
{
	/* Save screen */
	screen_save();

	/* Peruse the main help file */
	(void) show_file("help.hlp", NULL, 0, 0);

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
		/* Name too long */
		quit_fmt("The name '%s' is too long!", op_ptr->full_name);
	}

	/* Cannot contain "icky" characters */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		/* No control characters */
		if (iscntrl(op_ptr->full_name[i]))
		{
			/* Illegal characters */
			quit_fmt("The name '%s' contains control chars!",
				op_ptr->full_name);
		}
	}


#ifdef MACINTOSH

	/* Extract "useful" letters */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		char c = op_ptr->full_name[i];

		/* Convert "colon" and "period" */
		if ((c == ':') || (c == '.'))
			c = '_';

		/* Accept all the letters */
		op_ptr->base_name[k++] = c;
	}

#else

	/* Extract "useful" letters */
	for (i = 0; op_ptr->full_name[i]; i++)
	{
		char c = op_ptr->full_name[i];

		/* Accept some letters */
		if (isalpha(c) || isdigit(c))
			op_ptr->base_name[k++] = c;

		/* Convert space, dot, and underscore to underscore */
		else if (strchr(". _", c))
			op_ptr->base_name[k++] = '_';
	}

#endif


#if defined(WINDOWS) || defined(MSDOS)

	/* Hack -- max length */
	if (k > 8)
		k = 8;

#endif

	/* Terminate */
	op_ptr->base_name[k] = '\0';

	/* Require a "base" name */
	if (!op_ptr->base_name[0])
		strcpy(op_ptr->base_name, "PLAYER");


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
		path_build(savefile, 1024, ANGBAND_DIR_SAVE, temp);
	}
}


/*
 * Gets a name for the character, reacting to name changes.
 *
 * Assumes that "display_player(0)" has just been called
 *
 * Perhaps we should NOT ask for a name (at "birth()") on
 * Unix machines?  XXX XXX
 *
 * What a horrible name for a global function.  XXX XXX XXX
 */
void get_name(void)
{
	char tmp[32];

	/* Clear last line */
	clear_from(22);

	/* Prompt and ask */
	prt("[Enter your player's name above, or hit ESCAPE]", 23, 2);

	/* Ask until happy */
	while (1)
	{
		/* Go to the "name" field */
		move_cursor(2, 15);

		/* Save the player name */
		strcpy(tmp, op_ptr->full_name);

		/* Get an input, ignore "Escape" */
		if (askfor_aux(tmp, 15, FALSE))
			strcpy(op_ptr->full_name, tmp);

		/* Process the player name */
		process_player_name(FALSE);

		/* All done */
		break;
	}

	/* Pad the name (to clear junk) */
	sprintf(tmp, "%-15.15s", op_ptr->full_name);

	/* Re-Draw the name (in light blue) */
	c_put_str(TERM_L_BLUE, tmp, 2, 15);

	/* Erase the prompt, etc */
	clear_from(22);
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
		if (!get_check("Do you want to retire? "))
			return;
	}

	/* Verify Suicide */
	else
	{
		/* Verify */
		if (!get_check("Do you really want to quit? "))
			return;

		/* Special Verification for suicide */
		prt("Please verify QUITTING by typing the '@' sign: ", 0, 0);
		flush();
		i = inkey();
		prt("", 0, 0);
		if (i != '@')
			return;
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
	msg_print(NULL);

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

#if 0
	/* Yuck -- ugly hack for ghosts. */
	if (p_ptr->prace == RACE_GHOST)
	{
		if (p_ptr->prace_info)
			return (p_ptr->max_exp + 2500 + (50 * p_ptr->max_depth));
		else
			return (p_ptr->max_exp + (50 * (p_ptr->max_depth -
						p_ptr->depth)));
	}

	return (p_ptr->max_exp + (100 * p_ptr->max_depth));
#endif

	/* Kamband 1.9 now includes a new score calculation algorithm. 
	 * The reasoning is that it is royally useless to score based 
	 * on experience, that just fills the top slots of the scorefile
	 * with braindamaged "ghost beastmaster" characters.
	 *
	 * We now score mostly on the amount of turns plus max depth and 
	 * a little bit of experience.
	 * 
	 * Note: Ghosts do NOT get extra treatment.
	 * 
	 * Also note: You can "cheat" and get a huge score just by
	 * waiting a long, long time. In any case, this is still less
	 * arbitrary than the old way.
	 */

	int days = (turn / (TOWN_DAWN));

	return (days * 1000 + p_ptr->max_depth * 10 + p_ptr->max_lev * 5);

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



#if 0

/*
 * Save a "bones" file for a dead character
 *
 * Note that we will not use these files until Angband 2.8.0, and
 * then we will only use the name and level on which death occured.
 *
 * Should probably attempt some form of locking...
 */
static void make_bones(void)
{
	FILE *fp;

	char str[1024];


	/* Ignore wizards and borgs */
	if (!(p_ptr->noscore & 0x00FF))
	{
		/* Ignore people who die in town */
		if (p_ptr->depth)
		{
			char tmp[128];

			/* XXX XXX XXX "Bones" name */
			sprintf(tmp, "bone.%03d", p_ptr->depth);

			/* Build the filename */
			path_build(str, 1024, ANGBAND_DIR_BONE, tmp);

			/* Attempt to open the bones file */
			fp = my_fopen(str, "r");

			/* Close it right away */
			if (fp)
				my_fclose(fp);

			/* Do not over-write a previous ghost */
			if (fp)
				return;

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

			/* Try to write a new "Bones File" */
			fp = my_fopen(str, "w");

			/* Not allowed to write it?  Weird. */
			if (!fp)
				return;

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


#endif


/*
 * Draw an ascii file as if it's a raster image.
 *
 * Yes, we've finally got an alpha channel and anti-aliasing! :)
 */

static void print_picture(cptr file) {

  char buf[1024];

  FILE *fp;

  int y, x, xorig, i;

  /* Build the filename */
  path_build(buf, 1024, ANGBAND_DIR_FILE, file);

  /* Open the News file */
  fp = my_fopen(buf, "r");

  /* Dump */
  if (fp) {
    y = (screen_y - 24) / 2;
    xorig = (screen_x - 80) / 2;

    /* Dump the file to the screen */
    while (0 == my_fgets(fp, buf, 1024)) {

      /* Display and advance */
      x = xorig;

      for (i = 0; buf[i] != '\0'; i++) {

	if (buf[i] != ' ') {
	  byte color = TERM_SLATE;

	  if (strchr(",'`.\"", buf[i])) {
	    color = TERM_L_DARK;

	  } else if (strchr("+*", buf[i])) {
	    color = TERM_YELLOW;

	  } else if (strchr("/\\~v^", buf[i])) {
	    color = TERM_L_WHITE;

	  } else if (strchr("|-", buf[i])) {
	    color = TERM_WHITE;
	  }

	  Term_putch(x, y, color, buf[i]);
	}

	x++;
      }

      y++;
    }

    /* Close */
    my_fclose(fp);
  }
}


/*
 * Display a "tomb-stone"
 */
static void print_tomb(void)
{
	cptr p;

	char tmp[160];

	char buf[1024];

	time_t ct = time((time_t) 0);

	int y, x;


	/* Clear screen */
	Term_clear();

	/* King or Queen */
	if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "Magnificent";
	}

	/* Shape-shifted */

	else if (p_ptr->shape)
	{
		p = format("[%^s]", shape_info[p_ptr->shape - 1].name);
	}

	/* Normal */
	else
	{
		p = player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5];
	}

	y = (screen_y - 24) / 2;
	x = (screen_x - 80) / 2;

	center_string(buf, op_ptr->full_name);
	put_str(buf, y + 6, x + 11);

	center_string(buf, "the");
	put_str(buf, y + 7, x + 11);

	center_string(buf, p);
	put_str(buf, y + 8, x + 11);


	center_string(buf, cp_ptr->title);
	put_str(buf, y + 10, x + 11);

	sprintf(tmp, "Level: %d", (int) p_ptr->lev);
	center_string(buf, tmp);
	put_str(buf, y + 11, x + 11);

	sprintf(tmp, "Exp: %ld", (long) p_ptr->exp);
	center_string(buf, tmp);
	put_str(buf, y + 12, x + 11);

	sprintf(tmp, "AU: %ld", (long) p_ptr->au);
	center_string(buf, tmp);
	put_str(buf, y + 13, x + 11);


	if (p_ptr->depth == 0) {
	  sprintf(tmp, "Killed in the town");

	} else if (p_ptr->inside_special == SPECIAL_WILD) {
	  sprintf(tmp, "Killed in the wilderness");
	
	} else {
	  sprintf(tmp, "Killed on level %d", p_ptr->depth);
	}

	center_string(buf, tmp);
	put_str(buf, y + 14, x + 11);

	sprintf(tmp, "by %s.", p_ptr->died_from);
	center_string(buf, tmp);
	put_str(buf, y + 15, x + 11);


	sprintf(tmp, "%-.24s", ctime(&ct));
	center_string(buf, tmp);
	put_str(buf, y + 17, x + 11);

	print_picture("dead.txt");
}


/*
 * Display some character info
 */
static void show_info(void)
{
	object_type *o_ptr;

	store_type *st_ptr = &store[7];

	int i;
	bool equip_p = FALSE;

	/* Hack -- Know everything in the inven/equip */
	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{

		/* Skip non-objects */
		if (!o_ptr->k_idx)
			continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}


	/* Hack -- Know everything in the home */
	for (o_ptr = st_ptr->stock; o_ptr != NULL; o_ptr = o_ptr->next_global)
	{


		/* Skip non-objects */
		if (!o_ptr->k_idx)
			continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}

	/* Hack -- Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Flush all input keys */
	flush();

	/* Flush messages */
	msg_print(NULL);


	/* Describe options */
	prt("You may now dump a character record to one or more files.",
		screen_y - 3, 0);
	prt("Then, hit RETURN to see the character, or ESC to abort.",
		screen_y - 2, 0);

	/* Dump character records as requested */
	while (TRUE)
	{
		char out_val[160];

		/* Prompt */
		put_str("Filename: ", screen_y - 1, 0);

		/* Default */
		strcpy(out_val, "");

		/* Ask for filename (or abort) */
		if (!askfor_aux(out_val, 60, FALSE))
			return;

		/* Return means "show on screen" */
		if (!out_val[0])
			break;

		/* Save screen */
		Term_save();

		/* Dump a character file */
		(void) file_character(out_val);

		/* Load screen */
		Term_load();
	}


	/* Display player */
	display_player(0);

	/* Prompt for inventory */
	prt("Hit any key to see more information (ESC to abort): ",
		screen_y - 1, 0);

	/* Allow abort at this point */
	if (inkey() == ESCAPE)
		return;


	/* Show equipment and inventory */

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i])
		{
			equip_p = TRUE;
			break;
		}
	}

	/* Equipment -- if any */
	if (equip_p)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_equip();
		prt("You are using: -more-", 0, 0);
		if (inkey() == ESCAPE)
			return;
	}

	/* Inventory -- if any */
	if (inventory)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_stack(inventory, FALSE);
		prt("You are carrying: -more-", 0, 0);
		if (inkey() == ESCAPE)
			return;
	}



	/* Home -- if anything there */
	if (st_ptr->stock)
	{

		/* Clear screen */
		Term_clear();

		show_stack(st_ptr->stock, TRUE);

		/* Caption */
		prt("Your home contains: -more-", 0, 0);

		/* Wait for it */
		if (inkey() == ESCAPE)
			return;
	}
}





/*
 * Semi-Portable High Score List Entry (128 bytes)
 *
 * All fields listed below are null terminated ascii strings.
 *
 * In addition, the "number" fields are right justified, and
 * space padded, to the full available length (minus the "null").
 *
 * Note that "string comparisons" are thus valid on "pts".
 */

typedef struct high_score high_score;

struct high_score
{
	char what[8]; /* Version info (string) */

	char pts[10]; /* Total Score (number) */

	char gold[10]; /* Total Gold (number) */

	char turns[10];	/* Turns Taken (number) */

	char day[10]; /* Time stamp (string) */

	char who[16]; /* Player Name (string) */

	char uid[8]; /* Player UID (number) */

	char sex[2]; /* Player Sex (string) */
	char p_r[3]; /* Player Race (number) */
	char p_c[3]; /* Player Class (number) */

	char cur_lev[4]; /* Current Player Level (number) */
	char cur_dun[4]; /* Current Dungeon Level (number) */
	char max_lev[4]; /* Max Player Level (number) */
	char max_dun[4]; /* Max Dungeon Level (number) */

	char arena_number[MAX_ARENAS][4]; /* Arena levels attained -KMW- */
	char inside_special[4];	/* Did the player die in the arena? -KMW- */
	char exit_bldg[4]; /* Can the player exit arena? Goal obtained? -KMW- */

	char how[32]; /* Method of death (string) */
};



/*
 * The "highscore" file descriptor, if available.
 */
static int highscore_fd = -1;


/*
 * Seek score 'i' in the highscore file
 */
static int highscore_seek(int i)
{
	/* Seek for the requested record */
	return (fd_seek(highscore_fd, (huge) (i) * sizeof(high_score)));
}


/*
 * Read one score from the highscore file
 */
static errr highscore_read(high_score * score)
{
	/* Read the record, note failure */
	return (fd_read(highscore_fd, (char *) (score), sizeof(high_score)));
}


/*
 * Write one score to the highscore file
 */
static int highscore_write(high_score * score)
{
	/* Write the record, note failure */
	return (fd_write(highscore_fd, (char *) (score), sizeof(high_score)));
}




/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(high_score * score)
{
	int i;

	high_score the_score;

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0)
		return (-1);

	/* Go to the start of the highscore file */
	if (highscore_seek(0))
		return (-1);

	/* Read until we get to a higher score */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score))
			return (i);
		if (strcmp(the_score.pts, score->pts) < 0)
			return (i);
	}

	/* The "last" entry is always usable */
	return (MAX_HISCORES - 1);
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static int highscore_add(high_score * score)
{
	int i, slot;
	bool done = FALSE;

	high_score the_score, tmpscore;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0)
		return (-1);

	/* Determine where the score should go */
	slot = highscore_where(score);

	/* Hack -- Not on the list */
	if (slot < 0)
		return (-1);

	/* Hack -- prepare to dump the new score */
	the_score = (*score);

	/* Slide all the scores down one */
	for (i = slot; !done && (i < MAX_HISCORES); i++)
	{
		/* Read the old guy, note errors */
		if (highscore_seek(i))
			return (-1);
		if (highscore_read(&tmpscore))
			done = TRUE;

		/* Back up and dump the score we were holding */
		if (highscore_seek(i))
			return (-1);
		if (highscore_write(&the_score))
			return (-1);

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
static void display_scores_aux(int from, int to, int note,
	high_score * score)
{
	int i, j, k, n, attr, place;

	high_score the_score;

	char out_val[256];
/*	char tmp_val[160]; */

	int per_screen = (screen_y - 4) / 4;


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0)
		return;


	/* Assume we will show the first 10 */
	if (from < 0)
		from = 0;
	if (to < 0)
		to = 10;
	if (to > MAX_HISCORES)
		to = MAX_HISCORES;


	/* Seek to the beginning */
	if (highscore_seek(0))
		return;

	/* Hack -- Count the high scores */
	for (i = 0; i < MAX_HISCORES; i++)
	{
		if (highscore_read(&the_score))
			break;
	}

	/* Hack -- allow "fake" entry to be last */
	if ((note == i) && score)
		i++;

	/* Forget about the last entries */
	if (i > to)
		i = to;


	/* Show 5 per page, until "done" */
	for (k = from, place = k + 1; k < i; k += per_screen)
	{
		/* Clear screen */
		Term_clear();

		/* Title */
		put_str_center("Kamband Hall of Fame", 0);

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
			int pr, pc, clev, mlev, cdun, mdun, ia;	/* -KMW- */

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
				if (highscore_seek(j))
					break;
				if (highscore_read(&the_score))
					break;
			}

			/* Extract the race/class */
			pr = atoi(the_score.p_r);
			pc = atoi(the_score.p_c);

			/* Extract the level info */
			clev = atoi(the_score.cur_lev);
			mlev = atoi(the_score.max_lev);
			cdun = atoi(the_score.cur_dun);
			mdun = atoi(the_score.max_dun);

			ia = atoi(the_score.inside_special); /* -KMW- */

			/* Hack -- extract the gold and such */
			for (user = the_score.uid; isspace(*user); user++) /* loop */ ;
			for (when = the_score.day; isspace(*when); when++) /* loop */ ;
			for (gold = the_score.gold; isspace(*gold);
				gold++) /* loop */ ;
			for (aged = the_score.turns; isspace(*aged);
				aged++) /* loop */ ;

			/* Dump some info */
			sprintf(out_val, "%3d.%9s  %s the %s %s, Level %d", place,
				the_score.pts, the_score.who, race_info[pr].title,
				class_info[pc].title, clev);

			/* Append a "maximum level" */
			if (mlev > clev)
				strcat(out_val, format(" (Max %d)", mlev));

			/* Dump the first line */
			c_put_str(attr, out_val, n * 4 + 2, 0);

			/* Another line of info */
			if (ia == 0)
				sprintf(out_val, "               Killed by %s on %s %d",
					the_score.how, "Dungeon Level", cdun);
			else if (ia == SPECIAL_ARENA || ia == SPECIAL_MAGIC_ARENA) /* -KMW- */
				sprintf(out_val,
					"               Killed by %s in the Arena",
					the_score.how);
			else if (ia == SPECIAL_QUEST) /* -KMW- */
				sprintf(out_val,
					"               Killed by %s while questing",
					the_score.how);
			else if (ia == SPECIAL_STORE)
				sprintf(out_val,
					"               Killed by %s while in a store",
					the_score.how);
			else if (ia == SPECIAL_WILD)
				sprintf(out_val,
					"               Killed by %s while exploring",
					the_score.how);

			else
				sprintf(out_val,
					"               Killed by %s while battling software bugs",
					the_score.how);

			/* Hack -- some people die in the town - changed -KMW- */
			if (!cdun)
				sprintf(out_val, "               Killed by %s in the Town",
					the_score.how);

			/* Append a "maximum level" */
			if (mdun > cdun)
				strcat(out_val, format(" (Max %d)", mdun));

			/* Dump the info */
			c_put_str(attr, out_val, n * 4 + 3, 0);

			/* And still another line of info */
			sprintf(out_val,
				"               (User %s, Date %s, Gold %s, Turn %s).",
				user, when, gold, aged);
			c_put_str(attr, out_val, n * 4 + 4, 0);
		}


		/* Wait for response */
		prt("[Press ESC to quit, any other key to continue.]",
			screen_y - 1, 17);
		j = inkey();
		prt("", screen_y - 1, 0);

		/* Hack -- notice Escape */
		if (j == ESCAPE)
			break;
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
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Paranoia -- No score file */
	if (highscore_fd < 0)
		quit("Score file unavailable.");

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
 * Open the highscore file.
 */

void open_highscore(void)
{
	char buf[1024];

	/* Already open */
	if (highscore_fd >= 0)
		return;

	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");
	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_seek(0))
	{
		highscore_fd = -1;
	}

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
	}
}

/*
 * Close the highscore file.
 */

void close_highscore(void)
{
	if (highscore_fd >= 0)
	{
		fd_close(highscore_fd);
		highscore_fd = -1;
	}
}

/*
 * Rewind the highscore file 
 */

void rewind_highscore(void)
{
	if (highscore_seek(0))
	{
		highscore_fd = -1;
	}
}

/*
 * Return a nicely formatted high-score entry
 */

bool next_highscore(score_info * s_info)
{
	high_score score;
	int i;

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return FALSE;
	}

	if (highscore_read(&score))
		return FALSE;

	s_info->points = atoi(score.pts);
	s_info->turns = atoi(score.turns);
	s_info->race = atoi(score.p_r);
	s_info->class = atoi(score.p_c);
	s_info->plev = atoi(score.cur_lev);
	s_info->dlev = atoi(score.cur_dun);

	for (i = 0; i < MAX_ARENAS; i++)
	{
		s_info->arena_number[i] = atoi(score.arena_number[i]);
	}

	strcpy(s_info->name, score.who);

	return TRUE;
}


/*
 * Enters a players name on a hi-score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static errr top_twenty(void)
{
	int j;

	high_score the_score;

	time_t ct = time((time_t *) 0);


	/* Clear screen */
	Term_clear();

	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return (0);
	}

#ifndef SCORE_WIZARDS
	/* Wizard-mode pre-empts scoring */
	if (p_ptr->noscore & 0x000F)
	{
		msg_print("Score not registered for wizards.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}
#endif

#ifndef SCORE_BORGS
	/* Borg-mode pre-empts scoring */
	if (p_ptr->noscore & 0x00F0)
	{
		msg_print("Score not registered for borgs.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}
#endif

#ifndef SCORE_CHEATERS
	/* Cheaters are not scored */
	if (p_ptr->noscore & 0xFF00)
	{
		msg_print("Score not registered for cheaters.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}
#endif

	/* Interupted */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Interrupting"))
	{
		msg_print("Score not registered due to interruption.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}

	/* Quitter */
	if (!p_ptr->total_winner && streq(p_ptr->died_from, "Quitting"))
	{
		msg_print("Score not registered due to quitting.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}


	/* Clear the record */
	WIPE(&the_score, high_score);

	/* Save the version */
	sprintf(the_score.what, "%u.%u", KAM_VERSION_MAJOR, KAM_VERSION_MINOR);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long) total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long) p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long) turn);
	the_score.turns[9] = '\0';

#ifdef HIGHSCORE_DATE_HACK
	/* Save the date in a hacked up form (9 chars) */
	sprintf(the_score.day, "%-.6s %-.2s", ctime(&ct) + 4, ctime(&ct) + 22);
#else
	/* Save the date in standard form (8 chars) */
	strftime(the_score.day, 9, "%m/%d/%y", localtime(&ct));
#endif

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", op_ptr->full_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c",
		(p_ptr->psex ? (p_ptr->psex == 1 ? 'm' : 'n') : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->pclass);

	/* Save the level and such */
	sprintf(the_score.cur_lev, "%3d", p_ptr->lev);
	sprintf(the_score.cur_dun, "%3d", p_ptr->depth);
	sprintf(the_score.max_lev, "%3d", p_ptr->max_lev);
	sprintf(the_score.max_dun, "%3d", p_ptr->max_depth);

	for (j = 0; j < MAX_ARENAS; j++)
	{
		/* -KMW- */
		sprintf(the_score.arena_number[j], "%3d", p_ptr->arena_number[j]);
	}

	sprintf(the_score.inside_special, "%3d", p_ptr->inside_special); /* -KMW- */
	sprintf(the_score.exit_bldg, "%3d", p_ptr->exit_bldg); /* -KMW- */

	/* Save the cause of death (31 chars) */
	sprintf(the_score.how, "%-.31s", p_ptr->died_from);


	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK))
		return (1);

	/* Add a new entry to the score list, see where it went */
	j = highscore_add(&the_score);

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK))
		return (1);


	/* Hack -- Display the top fifteen scores */
	if (j < 10)
	{
		display_scores_aux(0, 15, j, NULL);
	}

	/* Display the scores surrounding the player */
	else
	{
		display_scores_aux(0, 5, j, NULL);
		display_scores_aux(j - 2, j + 7, j, NULL);
	}


	/* Success */
	return (0);
}


/*
 * Predict the players location, and display it.
 */
static errr predict_score(void)
{
	int j;

	high_score the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return (0);
	}


	/* Save the version */
	sprintf(the_score.what, "%u.%u", KAM_VERSION_MAJOR, KAM_VERSION_MINOR);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9lu", (long) total_points());

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long) p_ptr->au);

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long) turn);

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

	for (j = 0; j < MAX_ARENAS; j++)
	{
		/* -KMW- */
		sprintf(the_score.arena_number[j], "%3d", p_ptr->arena_number[j]);
	}

	sprintf(the_score.inside_special, "%3d", p_ptr->inside_special); /* -KMW- */
	sprintf(the_score.exit_bldg, "%3d", p_ptr->exit_bldg); /* -KMW- */

	/* Hack -- no cause of death */
	strcpy(the_score.how, "nobody (yet!)");


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


	/* Handle stuff */
	handle_stuff();

	/* Flush the messages */
	msg_print(NULL);

	/* Flush the input */
	flush();


	/* No suspending now */
	signals_ignore_tstp();


	/* Hack -- Character is now "icky" */
	character_icky = 1;


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);


	/* Handle death */
	if (p_ptr->is_dead)
	{
		/* Handle retirement */
		if (p_ptr->total_winner)
			kingly();

		/* You are dead */
		print_tomb();

		/* Show more info */
		show_info();

		/* Save memories (and mark savefile as "dead") */
		if (!save_player())
			msg_print("death save failed!");

#if 0
		/* Dump bones file */
		make_bones();
#endif

		/* Handle score, show Top scores */
		top_twenty();
	}

	/* Still alive */
	else
	{
		/* Save the game */
		do_cmd_save_game();

		/* Prompt for scores XXX XXX XXX */
		prt("Press Return (or Escape).", 0, 40);

		/* Predict score (or ESCAPE) */
		if (inkey() != ESCAPE)
			predict_score();
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
	if (!character_generated || character_saved)
		quit("panic");

	/* Mega-Hack -- see "msg_print()" */
	msg_flag = FALSE;

	/* Clear the top line */
	prt("", 0, 0);

	/* Hack -- turn off some things */
	disturb(1, 0);

	/* Hack -- Delay death XXX XXX XXX */
	if (p_ptr->chp < 0)
		p_ptr->is_dead = FALSE;

	/* Hardcode panic save */
	p_ptr->panic_save = 1;

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Indicate panic save */
	strcpy(p_ptr->died_from, "(panic save)");

	/* Panic save, or get worried */
	if (!save_player())
		quit("panic save failed!");

	/* Successful panic save */
	quit("panic save succeeded!");
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
	(void) signal(sig, SIG_IGN);

#ifdef SIGSTOP

	/* Flush output */
	Term_fresh();

	/* Suspend the "Term" */
	Term_xtra(TERM_XTRA_ALIVE, 0);

	/* Suspend ourself */
	(void) kill(0, SIGSTOP);

	/* Resume the "Term" */
	Term_xtra(TERM_XTRA_ALIVE, 1);

	/* Redraw the term */
	Term_redraw();

	/* Flush the term */
	Term_fresh();

#endif

	/* Restore handler */
	(void) signal(sig, handle_signal_suspend);
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
	(void) signal(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved)
		quit(NULL);


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
	(void) signal(sig, handle_signal_simple);
}


/*
 * Handle signal -- abort, kill, etc
 */
static void handle_signal_abort(int sig)
{
	/* Disable handler */
	(void) signal(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved)
		quit(NULL);


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
	(void) signal(SIGTSTP, SIG_IGN);
#endif

}

/*
 * Handle SIGTSTP signals (keyboard suspend)
 */
void signals_handle_tstp(void)
{

#ifdef SIGTSTP
	(void) signal(SIGTSTP, handle_signal_suspend);
#endif

}


/*
 * Prepare to handle the relevant signals
 */
void signals_init(void)
{

#ifdef SIGHUP
	(void) signal(SIGHUP, SIG_IGN);
#endif


#ifdef SIGTSTP
	(void) signal(SIGTSTP, handle_signal_suspend);
#endif


#ifdef SIGINT
	(void) signal(SIGINT, handle_signal_simple);
#endif

#ifdef SIGQUIT
	(void) signal(SIGQUIT, handle_signal_simple);
#endif


#ifdef SIGFPE
	(void) signal(SIGFPE, handle_signal_abort);
#endif

#ifdef SIGILL
	(void) signal(SIGILL, handle_signal_abort);
#endif

#ifdef SIGTRAP
	(void) signal(SIGTRAP, handle_signal_abort);
#endif

#ifdef SIGIOT
	(void) signal(SIGIOT, handle_signal_abort);
#endif

#ifdef SIGKILL
	(void) signal(SIGKILL, handle_signal_abort);
#endif

#ifdef SIGBUS
	(void) signal(SIGBUS, handle_signal_abort);
#endif

#ifdef SIGSEGV
	(void) signal(SIGSEGV, handle_signal_abort);
#endif

#ifdef SIGTERM
	(void) signal(SIGTERM, handle_signal_abort);
#endif

#ifdef SIGPIPE
	(void) signal(SIGPIPE, handle_signal_abort);
#endif

#ifdef SIGEMT
	(void) signal(SIGEMT, handle_signal_abort);
#endif

#ifdef SIGDANGER
	(void) signal(SIGDANGER, handle_signal_abort);
#endif

#ifdef SIGSYS
	(void) signal(SIGSYS, handle_signal_abort);
#endif

#ifdef SIGXCPU
	(void) signal(SIGXCPU, handle_signal_abort);
#endif

#ifdef SIGPWR
	(void) signal(SIGPWR, handle_signal_abort);
#endif

}


#else /* HANDLE_SIGNALS */


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


#endif /* HANDLE_SIGNALS */



/********************************************************/

/* Data type for a cache buffer. */

typedef struct cache_type cache_type;

struct cache_type
{
	char *name;
	int num_lines;
	char **lines;
};


/* Hack -- fake buffer for use when there's no cache. */
char *hack_fake_cache = NULL;

/* Maximum number of cache buffers. */
#define MAX_CACHE 20

/* Array of cache buffers. */
cache_type cache[MAX_CACHE];


/* Initialize the cache buffers. */
void init_cache(void)
{
	int i;

	for (i = 0; i < MAX_CACHE; i++)
	{
		cache[i].name = NULL;
	}
}

/*
 * Load the lines in the given file to the given cache entry.
 */
static errr fill_cache(int idx, char *fname)
{
	cache_type *c_ptr;
	int i;
	int num_lines;
	FILE *fp;
	char buf[1024];

	/* Paranoia. */
	if (idx < 0 || idx >= MAX_CACHE)
		return 1;

	c_ptr = &cache[idx];

	/* Save the filename. */
	C_MAKE(c_ptr->name, strlen(fname) + 1, char);
	strcpy(c_ptr->name, fname);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, fname);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp)
		return -1;


	/* Figure out the number of lines. */
	if (my_fgets(fp, buf, 80) == 0)
	{
		num_lines = atoi(buf);

	}
	else
	{
		my_fclose(fp);
		return 1;
	}

	c_ptr->num_lines = num_lines;

	C_MAKE(c_ptr->lines, num_lines, char *);

	/* Load all the lines in the file into the cache. */
	for (i = 0; i < num_lines; i++)
	{

		if (my_fgets(fp, buf, 80) != 0)
		{
			my_fclose(fp);
			return 1;
		}

		C_MAKE(c_ptr->lines[i], strlen(buf) + 1, char);
		strcpy(c_ptr->lines[i], buf);
	}

	my_fclose(fp);

	/* We're done. */
	return 0;
}


/*
 * Find the appropriate cache entry for the given filename.
 */

static cache_type *fetch_cache_entry(char *fname)
{
	int i;

	for (i = 0; i < MAX_CACHE; i++)
	{

		/* Hit the end of the cache buffer. */
		if (cache[i].name == NULL)
		{
			/* Pre-load the file. */
			if (fill_cache(i, fname) != 0)
				return NULL;

			return &cache[i];

			/*  Found an existing cache we can use. */
		}
		else if (strcmp(cache[i].name, fname) == 0)
		{
			return &cache[i];
		}
	}

	return NULL;
}


/*
 * Get the i-th line from the file. If the file is already cached, just return
 * the entry in the cache. Otherwise, load the file into an available cache
 * buffer. If all the allowed cache buffers are filled, then just get the line
 * the old-fashioned dumb way.
 */
cptr get_line(char *fname, int line)
{
	cache_type *c_ptr;

	/* Get the appropriate cache entry. */
	c_ptr = fetch_cache_entry(fname);

	/* Cache buffer is full. */
	if (c_ptr == NULL)
	{
		/* Do it the old-fashioned way. */
		return get_line_old(fname, line);
	}

	/* Simply return the existing line. */
	return c_ptr->lines[line % c_ptr->num_lines];
}

/*
 * Like get_line, but with a random line number.
 */
cptr get_random_line(char *fname)
{
	cache_type *c_ptr;

	/* Get the appropriate cache entry. */
	c_ptr = fetch_cache_entry(fname);

	/* Cache buffer is full. */
	if (c_ptr == NULL)
	{
		/* Do it the old-fashioned way. */
		return get_random_line_old(fname);
	}

	/* Simply return the existing line. */
	return c_ptr->lines[rand_int(c_ptr->num_lines)];
}


/*
 * Return the number of lines in the file.
 */
s16b get_num_lines(char *fname)
{
	cache_type *c_ptr;

	/* Get the appropriate cache entry. */
	c_ptr = fetch_cache_entry(fname);

	/* Cache buffer is full. */
	if (c_ptr == NULL)
	{
		/* Do it the old-fashioned way. */
		return get_num_lines_old(fname);
	}

	/* Simply return the existing line. */
	return c_ptr->num_lines;
}


/* 
 * Fetch a random line from a file.
 * Note that code is stolen from Zangband, but cleaned up somewhat.
 */

cptr get_random_line_old(char *fname)
{
	FILE *fp;
	char buf[1024];
	int line, num_lines, i;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, fname);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp)
		return NULL;

	/* Parse the file */
	if (my_fgets(fp, buf, 80) == 0)
	{
		num_lines = atoi(buf);
	}
	else
	{
		my_fclose(fp);
		return NULL;
	}

	line = randint(num_lines);

	for (i = 1; i <= line; i++)
	{
		if (my_fgets(fp, buf, 80) != 0)
		{
			my_fclose(fp);
			return NULL;
		}
	}

	if (hack_fake_cache != NULL)
	{
		C_KILL(hack_fake_cache, strlen(hack_fake_cache) + 1, char);
	}

	C_MAKE(hack_fake_cache, strlen(buf) + 1, char);
	strcpy(hack_fake_cache, buf);

	my_fclose(fp);
	return hack_fake_cache;
}


/*
 * Return the number of lines in a file.
 */

s16b get_num_lines_old(char *fname)
{
	FILE *fp;
	char buf[1024];

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, fname);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp)
		return -1;

	/* Parse the file */
	if (my_fgets(fp, buf, 80) == 0)
	{
		my_fclose(fp);
		return atoi(buf);
	}
	else
	{
		my_fclose(fp);
		return 0;
	}
}


/*
 * Get a specific line from a file.
 * Note: The first line is 1, not 0, since line 0 is the file size.
 * (see get_random_line)
 */

cptr get_line_old(char *fname, int line)
{
	FILE *fp;
	char buf[1024];
	int i;

	line++;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, fname);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp)
		return NULL;

	for (i = 0; i <= line; i++)
	{
		if (my_fgets(fp, buf, 80) != 0)
		{
			my_fclose(fp);
			return NULL;
		}
	}

	if (hack_fake_cache != NULL)
	{
		C_KILL(hack_fake_cache, strlen(hack_fake_cache) + 1, char);
	}

	C_MAKE(hack_fake_cache, strlen(buf) + 1, char);
	strcpy(hack_fake_cache, buf);

	my_fclose(fp);
	return hack_fake_cache;
}
