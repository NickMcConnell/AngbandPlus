/* File: files.c */

/* Purpose: code dealing with files (and death) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

extern void do_cmd_knowledge_chaos_features();


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
 * Parse another file recursively, see below for details
 *   %:<filename>
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
 * Specify the attr/char values for inventory "objects" by kind tval
 *   E:<tv>:<a>:<c>
 *
 * Define a macro action, given an encoded macro action
 *   A:<str>
 *
 * Create a normal macro, given an encoded macro trigger
 *   P:<str>
 *
 * Create a command macro, given an encoded macro trigger
 *   C:<str>
 *
 * Create a keyset mapping
 *   S:<key>:<key>:<dir>
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
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace(buf[0])) return (0);

	/* Skip comments */
	if (buf[0] == '#') return (0);

	/* Require "?:*" format */
	if (buf[1] != ':') return (1);


	/* Process "%:<fname>" */
	if (buf[0] == '%')
	{
		/* Attempt to Process the given file */
		return (process_pref_file(buf + 2));
	}


	/* Process "R:<num>:<a>/<c>" -- attr/char for monster races */
	if (buf[0] == 'R')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			monster_race *r_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= MAX_R_IDX) return (1);
			r_ptr = &r_info[i];
			if (n1) r_ptr->x_attr = n1;
			if (n2) r_ptr->x_char = n2;
			return (0);
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
			if (i >= MAX_K_IDX) return (1);
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
			if (i >= MAX_F_IDX) return (1);
			f_ptr = &f_info[i];
			if (n1) f_ptr->z_attr = n1;
			if (n2) f_ptr->z_char = n2;
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
			for (i = 1; i < MAX_K_IDX; i++)
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


	/* Process "E:<tv>:<a>/<c>" -- attr/char for equippy chars */
	else if (buf[0] == 'E')
	{
		if (tokenize(buf+2, 3, zz) == 3)
		{
			j = (byte)strtol(zz[0], NULL, 0) % 128;
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (n1) tval_to_attr[j] = n1;
			if (n2) tval_to_char[j] = n2;
			return (0);
		}
	}


	/* Process "A:<str>" -- save an "action" for later */
	else if (buf[0] == 'A')
	{
		text_to_ascii(macro__buf, buf+2);
		return (0);
	}

	/* Process "P:<str>" -- normal macro */
	else if (buf[0] == 'P')
	{
		char tmp[1024];
		text_to_ascii(tmp, buf+2);
		macro_add(tmp, macro__buf);
		return (0);
	}


	/* Process "C:<str>" -- create keymap */
	else if (buf[0] == 'C')
	{
		int mode;

		char tmp[1024];

		if (tokenize(buf+2, 2, zz) != 2) return (1);

		mode = strtol(zz[0], NULL, 0);
		if ((mode < 0) || (mode >= KEYMAP_MODES)) return (1);

		text_to_ascii(tmp, zz[1]);
		if (!tmp[0] || tmp[1]) return (1);
		i = (byte)(tmp[0]);

		string_free(keymap_act[mode][i]);

		keymap_act[mode][i] = string_make(macro__buf);

		/* XXX Hack -- See main-win.c */
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


	/* Process "X:<str>" -- turn option off */
	else if (buf[0] == 'X')
	{
		for (i = 0; option_info[i].o_desc; i++)
		{
			if (option_info[i].o_var &&
			    option_info[i].o_text &&
			    streq(option_info[i].o_text, buf + 2))
			{
				(*option_info[i].o_var) = FALSE;
				return (0);
			}
		}
	}

	/* Process "Y:<str>" -- turn option on */
	else if (buf[0] == 'Y')
	{
		for (i = 0; option_info[i].o_desc; i++)
		{
			if (option_info[i].o_var &&
			    option_info[i].o_text &&
			    streq(option_info[i].o_text, buf + 2))
			{
				(*option_info[i].o_var) = TRUE;
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
				if (*t && streq(t, "0")) v = "0";
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

			/* Race */
			else if (streq(b+1, "RACE"))
			{
				v = rp_ptr->title;
			}

			/* Template */
			else if (streq(b+1, "TEMPLATE"))
			{
				v = cp_ptr->title;
			}

			/* Player */
			else if (streq(b+1, "PLAYER"))
			{
				v = player_base;
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

	time_t              c;
	struct tm		*tp;

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

	FILE        *fp;

	char	buf[1024];


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



#ifdef CHECK_LOAD

#ifndef MAXHOSTNAMELEN
# define MAXHOSTNAMELEN  64
#endif

typedef struct statstime statstime;

struct statstime
{
	int                 cp_time[4];
	int                 dk_xfer[4];
	unsigned int        v_pgpgin;
	unsigned int        v_pgpgout;
	unsigned int        v_pswpin;
	unsigned int        v_pswpout;
	unsigned int        v_intr;
	int                 if_ipackets;
	int                 if_ierrors;
	int                 if_opackets;
	int                 if_oerrors;
	int                 if_collisions;
	unsigned int        v_swtch;
	long                avenrun[3];
	struct timeval      boottime;
	struct timeval      curtime;
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

	FILE        *fp;

	char	buf[1024];

	char	temphost[MAXHOSTNAMELEN+1];
	char	thishost[MAXHOSTNAMELEN+1];


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, "load.txt");

	/* Open the "load" file */
	fp = my_fopen(buf, "r");

	/* No file, no restrictions */
	if (!fp) return (0);

	/* Default load */
	check_load_value = 100;

	/* Get the host name */
	(void)gethostname(thishost, (sizeof thishost) - 1);

	/* Parse it */
	while (0 == my_fgets(fp, buf, 1024))
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
	(void)sprintf(out_val, "%9ld", (long)num);
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
	(void)sprintf(out_val, "%6ld", (long)num);
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

	object_type *o_ptr = &inventory[INVEN_WIELD];

	/* Hack -- add in weapon info if known */
	if (object_known_p(o_ptr)) show_tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) show_todam += o_ptr->to_d;

	/* Dump the bonuses to hit/dam */
	prt_num("+ To Hit    ", show_tohit, 9, 1, TERM_L_BLUE);
	prt_num("+ To Damage ", show_todam, 10, 1, TERM_L_BLUE);

	/* Dump the armor class bonus */
	prt_num("+ To AC     ", p_ptr->dis_to_a, 11, 1, TERM_L_BLUE);

	/* Dump the total armor class */
	prt_num("  Base AC   ", p_ptr->dis_ac, 12, 1, TERM_L_BLUE);

	prt_lnum("Experience ", p_ptr->exp, 10, 28, TERM_L_GREEN);

	prt_lnum("Exp Factor ", p_ptr->expfact, 13, 28, TERM_L_GREEN);


	prt_num("Max Hit Points ", p_ptr->mhp, 9, 52, TERM_L_GREEN);

	if (p_ptr->chp >= p_ptr->mhp)
	{
		prt_num("Cur Hit Points ", p_ptr->chp, 10, 52, TERM_L_GREEN);
	}
	else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
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
	else if (p_ptr->csp > (p_ptr->msp * hitpoint_warn) / 10)
	{
		prt_num("Cur SP (Mana)  ", p_ptr->csp, 12, 52, TERM_YELLOW);
	}
	else
	{
		prt_num("Cur SP (Mana)  ", p_ptr->csp, 12, 52, TERM_RED);
	}

	prt_lnum("Gold           ", p_ptr->au, 13, 52, TERM_L_GREEN);
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
	static char dummy[20] = "";

	/* Paranoia */
	if (y <= 0) y = 1;

	/* Negative value */
	if (x < 0)
	{
		likert_color = TERM_L_DARK;
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
			likert_color = TERM_L_RED;
			return ("Poor");
		}
		case 3:
		case 4:
		{
			likert_color = TERM_ORANGE;
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
			likert_color = TERM_GREEN;
			return ("Superb");
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			likert_color = TERM_BLUE;
			return ("Incredible");
		}
		default:
		{
			likert_color = TERM_VIOLET;
			return ("Stupendous");
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
    int         tmp, damdice, damsides, dambonus, blows;
	int			xthn, xthb, xfos, xsrh;
	int			xdis, xdev, xsav, xstl;
	cptr	  desc;
    int         muta_att = 0;
	object_type		*o_ptr;

	if (p_ptr->muta2 & MUT2_HORNS)     muta_att++;
	if (p_ptr->muta2 & MUT2_SCOR_TAIL) muta_att++;
	if (p_ptr->muta2 & MUT2_BEAK)      muta_att++;
	if (p_ptr->muta2 & MUT2_TRUNK)     muta_att++;
	if (p_ptr->muta2 & MUT2_TENTACLES) muta_att++;

	/* Fighting Skill (with current weapon) */
	o_ptr = &inventory[INVEN_WIELD];
	tmp = p_ptr->to_h + o_ptr->to_h;
	xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

	/* Shooting Skill (with current bow and normal missile) */
	o_ptr = &inventory[INVEN_BOW];
	tmp = p_ptr->to_h + o_ptr->to_h;
	xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);


     /* Average damage per round */
     o_ptr = &inventory[INVEN_WIELD];
     dambonus = p_ptr->dis_to_d;
     if (object_known_p(o_ptr)) dambonus += o_ptr->to_d;
     damdice = o_ptr->dd;
     damsides = o_ptr->ds;   /* dam += (o_ptr->dd * (o_ptr->ds + 1)) >> 1; */
     blows = p_ptr->num_blow;
     /* dam *= p_ptr->num_blow; */

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
    if (!muta_att)
        put_str(format("%d", p_ptr->num_blow), 16, 69);
    else
        put_str(format("%d+%d", p_ptr->num_blow, muta_att), 16, 69);

	put_str("Shots/Round:", 17, 55);
	put_str(format("%d", p_ptr->num_fire), 17, 69);

   put_str("Wpn.dmg/Rnd:", 18, 55);    /* From PsiAngband */
     if ((damdice == 0) || (damsides == 0)) {
   if (dambonus <= 0)
     desc = "nil!";
   else
     desc = format("%d", blows * dambonus);
     } else {
   if (dambonus == 0)
     desc = format("%dd%d", blows * damdice, damsides);
   else
     desc = format("%dd%d%s%d", blows * damdice, damsides,
           (dambonus < 0 ? "":"+"), blows * dambonus);
     }
   put_str(desc, 18, 69);


	put_str("Infra-Vision:", 19, 55);
	put_str(format("%d feet", p_ptr->see_infra * 10), 19, 69);
}



/*
 * Obtain the "flags" for the player as if he was an item
 */
static void player_flags(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Elf */
	if (p_ptr->prace == RACE_ELF) (*f2) |= (TR2_RES_LITE);

	/* Hobbit */
	if (p_ptr->prace == RACE_HOBBIT) (*f2) |= (TR2_SUST_DEX);

	/* Gnome */
    if (p_ptr->prace == RACE_GNOME) (*f2) |= (TR2_FREE_ACT);

	/* Dwarf */
	if (p_ptr->prace == RACE_DWARF) (*f2) |= (TR2_RES_BLIND);

	/* Half-Orc */
	if (p_ptr->prace == RACE_HALF_ORC) (*f2) |= (TR2_RES_DARK);

	/* Half-Troll */
    if (p_ptr->prace == RACE_HALF_TROLL)
        {
            (*f2) |= (TR2_SUST_STR);
            if ((skill_set[SKILL_RACIAL].value/2) > 14)
            {
                (*f3) = (TR3_REGEN);
            }
        }

	/* Dunadan */
    if (p_ptr->prace == RACE_GREAT)

        {
            (*f2) |= (TR2_SUST_CON);
            (*f3) |= (TR3_REGEN); /* Great ones heal fast */
        }

	/* High Elf */
	if (p_ptr->prace == RACE_HIGH_ELF) (*f2) |= (TR2_RES_LITE);
    if (p_ptr->prace == RACE_HIGH_ELF) (*f3) |= (TR3_SEE_INVIS);

    if (p_ptr->prace == RACE_BARBARIAN) (*f2) |= (TR2_RES_FEAR);
    else if (p_ptr->prace == RACE_HALF_OGRE)
    {
            (*f2) |= (TR2_SUST_STR);
            (*f2) |= (TR2_RES_DARK);
        }
    else if (p_ptr->prace == RACE_HALF_GIANT)
    {
        (*f2) |= (TR2_RES_SHARDS);
        (*f2) |= (TR2_SUST_STR);
    }
    else if (p_ptr->prace == RACE_HALF_TITAN)
    {
        (*f2) |= (TR2_RES_CHAOS);
    }
    else if (p_ptr->prace == RACE_CYCLOPS)
    {
        (*f2) |= (TR2_RES_SOUND);
    }
    else if (p_ptr->prace == RACE_YEEK)
    {
        (*f2) |= (TR2_RES_ACID);
        if ((skill_set[SKILL_RACIAL].value/2) > 19)
        {
            (*f2) |= (TR2_IM_ACID);
        }
    }
    else if (p_ptr->prace == RACE_KLACKON)
    {
        if ((skill_set[SKILL_RACIAL].value/2) > 9) (*f1) |= TR1_SPEED;
        (*f2) |= (TR2_RES_CONF);
        (*f2) |= (TR2_RES_ACID);
    }
    else if (p_ptr->prace == RACE_KOBOLD)
    {
        (*f2) |= (TR2_RES_POIS);
    }
    else if (p_ptr->prace == RACE_NIBELUNG)
    {
        (*f2) |= (TR2_RES_DISEN);
        (*f2) |= (TR2_RES_DARK);
    }
    else if (p_ptr->prace == RACE_DARK_ELF)
    {
        (*f2) |= (TR2_RES_DARK);
        if ((skill_set[SKILL_RACIAL].value/2) > 19)
        {
            (*f3) |= (TR3_SEE_INVIS);
        }
    }
    else if (p_ptr->prace == RACE_DRACONIAN)
    {
            (*f3) |= TR3_FEATHER;
        if ((skill_set[SKILL_RACIAL].value/2) > 4)
        {
            (*f2) |= (TR2_RES_FIRE);
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 9)
        {
            (*f2) |= (TR2_RES_COLD);
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 14)
        {
            (*f2) |= (TR2_RES_ACID);
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 19)
        {
            (*f2) |= (TR2_RES_ELEC);
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 34)
        {
            (*f2) |= (TR2_RES_POIS);
        }

    }
    else if (p_ptr->prace == RACE_MIND_FLAYER)
    {
        (*f2) |= (TR2_SUST_INT);
        (*f2) |= (TR2_SUST_WIS);
        if ((skill_set[SKILL_RACIAL].value/2) > 14)
        {
            (*f3) |= (TR3_SEE_INVIS);
        }
        if ((skill_set[SKILL_RACIAL].value/2) > 29)
        {
            (*f3) |= (TR3_TELEPATHY);
        }
    }
    else if (p_ptr->prace == RACE_IMP)
    {
        (*f2) |= (TR2_RES_FIRE);
        if ((skill_set[SKILL_RACIAL].value/2) > 9)
        {
            (*f3) |= (TR3_SEE_INVIS);
        }
    }
    else if (p_ptr->prace == RACE_GOLEM)
    {
        (*f3) |= (TR3_SEE_INVIS);
        (*f2) |= (TR2_FREE_ACT);
        (*f2) |= (TR2_RES_POIS);
        (*f3) |= (TR3_SLOW_DIGEST);
        if ((skill_set[SKILL_RACIAL].value/2) > 34)
        {
            (*f2) |= (TR2_HOLD_LIFE);
        }
    }
    else if (p_ptr->prace == RACE_SKELETON)
    {
        (*f3) |= (TR3_SEE_INVIS);
        (*f2) |= (TR2_RES_SHARDS);
        (*f2) |= (TR2_HOLD_LIFE);
        (*f2) |= (TR2_RES_POIS);
        if ((skill_set[SKILL_RACIAL].value/2) > 9)
        {
            (*f2) |= (TR2_RES_COLD);
        }
    }
    else if (p_ptr->prace == RACE_ZOMBIE)
    {
        (*f3) |= (TR3_SEE_INVIS);
        (*f2) |= (TR2_HOLD_LIFE);
        (*f2) |= (TR2_RES_NETHER);
        (*f2) |= (TR2_RES_POIS);
        (*f3) |= (TR3_SLOW_DIGEST);
        if ((skill_set[SKILL_RACIAL].value/2) > 4)
        {
            (*f2) |= (TR2_RES_COLD);
        }
    }
    else if (p_ptr->prace == RACE_VAMPIRE)
    {
       (*f2) |= (TR2_HOLD_LIFE);
       (*f2) |= (TR2_RES_DARK);
       (*f2) |= (TR2_RES_NETHER);
       (*f3) |= (TR3_LITE);
       (*f2) |= (TR2_RES_POIS);
       (*f2) |= (TR2_RES_COLD);
    }
    else if (p_ptr->prace == RACE_SPECTRE)
    {

        (*f2) |= (TR2_RES_COLD);
        (*f3) |= (TR3_SEE_INVIS);
        (*f2) |= (TR2_HOLD_LIFE);
        (*f2) |= (TR2_RES_NETHER);
        (*f2) |= (TR2_RES_POIS);
        (*f3) |= (TR3_SLOW_DIGEST);
        if ((skill_set[SKILL_RACIAL].value/2) > 34)
        {
            (*f3) |= TR3_TELEPATHY;
        }
    }
    else if (p_ptr->prace == RACE_SPRITE)
    {
        (*f2) |= (TR2_RES_LITE);
        (*f3) |= (TR3_FEATHER);
        if ((skill_set[SKILL_RACIAL].value/2) > 9)
            (*f1) |= (TR1_SPEED);
    }
    else if (p_ptr->prace == RACE_BROO)
    {
        (*f2) |= (TR2_RES_SOUND);
        (*f2) |= (TR2_RES_CONF);
    }
    if (skill_set[SKILL_MINDCRAFTING].value >= 80) (*f3) |= TR3_TELEPATHY;
    if (p_ptr->muta3)
    {
        if (p_ptr->muta3 & MUT3_FLESH_ROT)
        {
            (*f3) &= ~(TR3_REGEN);
        }

        if ((p_ptr->muta3 & MUT3_XTRA_FAT) || 
			(p_ptr->muta3 & MUT3_XTRA_LEGS) ||
            (p_ptr->muta3 & MUT3_SHORT_LEG))
        {
            (*f1) |= TR1_SPEED;
        }

        if (p_ptr->muta3  & MUT3_ELEC_TOUC)
        {
            (*f3) |= TR3_SH_ELEC;
        }

        if (p_ptr->muta3 & MUT3_FIRE_BODY)
        {
            (*f3) |= TR3_SH_FIRE;
            (*f3) |= TR3_LITE;
        }

        if (p_ptr->muta3 & MUT3_WINGS)
        {
            (*f3) |= TR3_FEATHER;
        }

        if (p_ptr->muta3 & MUT3_FEARLESS)
        {
            (*f2) |= (TR2_RES_FEAR);
        }

        if (p_ptr->muta3 & MUT3_REGEN)
        {
            (*f3) |= TR3_REGEN;
        }

        if (p_ptr->muta3 & MUT3_ESP)
        {
            (*f3) |= TR3_TELEPATHY;
        }
		if (p_ptr->muta3 & MUT3_ESP)
		{
			(*f3) |= TR3_TELEPATHY;
		}
		
		if (p_ptr->muta3 & MUT3_MOTION)
		{
			(*f2) |= TR2_FREE_ACT;
		}
		
		if (p_ptr->muta3 & MUT3_SUS_STATS)
		{
			(*f2) |= TR2_SUST_CON;
			if ((skill_set[SKILL_RACIAL].value/2) > 9)
				(*f2) |= TR2_SUST_STR;
			if ((skill_set[SKILL_RACIAL].value/2) > 19)
				(*f2) |= TR2_SUST_DEX;
			if ((skill_set[SKILL_RACIAL].value/2) > 29)
				(*f2) |= TR2_SUST_WIS;
			if ((skill_set[SKILL_RACIAL].value/2) > 39)
				(*f2) |= TR2_SUST_INT;
			if ((skill_set[SKILL_RACIAL].value/2) > 49)
				(*f2) |= TR2_SUST_CHR;
		}
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
	for (i=INVEN_WIELD; i<INVEN_POUCH_1; i++)
	{
		/* Object */
		o_ptr = &inventory[i];

        a = object_attr(o_ptr);
        c = object_char(o_ptr);

		/* No color */
		if (!use_color) a = TERM_WHITE;

        if ((!equippy_chars) ||
            (!o_ptr->k_idx)) /* Clear the part of the screen */
            {
              c = ' ';
              a = TERM_DARK;
            }

		/* Dump */
		Term_putch(x+i-INVEN_WIELD, y, a, c);
	}
}


void print_equippy()
{
    display_player_equippy(ROW_EQUIPPY, COL_EQUIPPY);
}

/*
 * Helper function, see below
 */
static void display_player_flag_aux(int row, int col,
			char *header, int n, u32b flag)
{
	int i;

	u32b f[3];


	/* Header */
	c_put_str(TERM_WHITE, header, row, col);

	/* Advance */
	col += strlen(header) + 1;


	/* Check equipment */
	for (i=INVEN_WIELD; i<INVEN_POUCH_1; i++)
	{
		object_type *o_ptr;

		/* Object */
		o_ptr = &inventory[i];

		/* Known flags */
		object_flags_known(o_ptr, &f[0], &f[1], &f[2]);

		/* Default */
		c_put_str(TERM_SLATE, ".", row, col);

		/* Check flags */
		if (f[n-1] & flag) c_put_str(TERM_WHITE, "+", row, col);

		/* Advance */
		col++;
	}

	/* Player flags */
	player_flags(&f[0], &f[1], &f[2]);

	/* Default */
	c_put_str(TERM_SLATE, ".", row, col);

	/* Check flags */
	if (f[n-1] & flag) c_put_str(TERM_WHITE, "+", row, col);
}


/*
 * Special display, part 1
 */
static void display_player_flag_info(void)
{
	int row;
	int col;


	/*** Set 1 ***/

	row = 13;
	col = 1;

	display_player_equippy(row-2, col+7);

	c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col+7);

	display_player_flag_aux(row+0, col, "Acid :", 2, (TR2_RES_ACID | TR2_IM_ACID));
	display_player_flag_aux(row+1, col, "Elec :", 2, (TR2_RES_ELEC | TR2_IM_ELEC));
	display_player_flag_aux(row+2, col, "Fire :", 2, (TR2_RES_FIRE | TR2_IM_FIRE));
	display_player_flag_aux(row+3, col, "Cold :", 2, (TR2_RES_COLD | TR2_IM_COLD));
	display_player_flag_aux(row+4, col, "Poisn:", 2, TR2_RES_POIS);
	display_player_flag_aux(row+5, col, "Light:", 2, TR2_RES_LITE);
	display_player_flag_aux(row+6, col, "Dark :", 2, TR2_RES_DARK);
	display_player_flag_aux(row+7, col, "Shard:", 2, TR2_RES_SHARDS);
    display_player_flag_aux(row+8, col, "Blind:", 2, TR2_RES_BLIND);
    display_player_flag_aux(row+9, col, "Conf :", 2, TR2_RES_CONF);

	/*** Set 2 ***/

	row = 13;
	col = 24;

	display_player_equippy(row-2, col+8);

	c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col+8);

    display_player_flag_aux(row+0, col, "Sound :", 2, TR2_RES_SOUND);
    display_player_flag_aux(row+1, col, "Nether:", 2, TR2_RES_NETHER);
    display_player_flag_aux(row+2, col, "Nexus :", 2, TR2_RES_NEXUS);
    display_player_flag_aux(row+3, col, "Chaos :", 2, TR2_RES_CHAOS);
    display_player_flag_aux(row+4, col, "Disnch:", 2, TR2_RES_DISEN);
    display_player_flag_aux(row+5, col, "Fear  :", 2, TR2_RES_FEAR);
    display_player_flag_aux(row+6, col, "Reflct:", 2, TR2_REFLECT);
    display_player_flag_aux(row+7, col, "AuFire:", 3, TR3_SH_FIRE);
    display_player_flag_aux(row+8, col, "AuElec:", 3, TR3_SH_ELEC);

	/*** Set 3 ***/

	row = 13;
	col = 48;

	display_player_equippy(row-2, col+15);

    c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col+15);

    display_player_flag_aux(row+0, col, "Speed        :", 1, TR1_SPEED);
    display_player_flag_aux(row+1, col, "Free Action  :", 2, TR2_FREE_ACT);
    display_player_flag_aux(row+2, col, "See Invisible:", 3, TR3_SEE_INVIS);
    display_player_flag_aux(row+3, col, "Hold Life    :", 2, TR2_HOLD_LIFE);
    display_player_flag_aux(row+4, col, "Telepathy    :", 3, TR3_TELEPATHY);
    display_player_flag_aux(row+5, col, "Slow Digest  :", 3, TR3_SLOW_DIGEST);
    display_player_flag_aux(row+6, col, "Regeneration :", 3, TR3_REGEN);
    display_player_flag_aux(row+7, col, "Levitation   :", 3, TR3_FEATHER);
    display_player_flag_aux(row+8, col, "Perm Lite    :", 3, TR3_LITE);
}

/*
 * Return a colour based on the status of a skill
 */
byte skill_colour(int skill_index)
{
	/* The skill has been drained */
	if (skill_set[skill_index].value < skill_set[skill_index].max_value)
	{
		return TERM_L_WHITE;
	}
	return TERM_WHITE;
}

/*
 * Display all the player's base skill levels
 */

static void display_player_skills(void)
{
	char buf[80];

	put_str("Everyman Skills",1,7);
	put_str("===============",2,7);
	put_str("Specialist Skills",1,31);
	put_str("=================",2,31);
	put_str("Hermetic Skills",1,56);
	put_str("===============",2,56);

	/* Everyman Skills */
	sprintf(buf,"Close Combat:    %d%%",skill_set[SKILL_CLOSE].value);
	c_put_str(skill_colour(SKILL_CLOSE),buf,4,5);
	sprintf(buf,"Slashing Weaps:  %d%%",skill_set[SKILL_SLASH].value);
	c_put_str(skill_colour(SKILL_SLASH),buf,5,5);
	sprintf(buf,"Stabbing Weaps:  %d%%",skill_set[SKILL_STAB].value);
	c_put_str(skill_colour(SKILL_STAB),buf,6,5);
	sprintf(buf,"Crushing Weaps:  %d%%",skill_set[SKILL_CRUSH].value);
	c_put_str(skill_colour(SKILL_CRUSH),buf,7,5);
	sprintf(buf,"Missile:         %d%%",skill_set[SKILL_MISSILE].value);
	c_put_str(skill_colour(SKILL_MISSILE),buf,8,5);
	sprintf(buf,"Toughness:       %d%%",skill_set[SKILL_TOUGH].value);
	c_put_str(skill_colour(SKILL_TOUGH),buf,10,5);
	sprintf(buf,"Devices:         %d%%",skill_set[SKILL_DEVICE].value);
	c_put_str(skill_colour(SKILL_DEVICE),buf,12,5);
	sprintf(buf,"Resistance:      %d%%",skill_set[SKILL_SAVE].value);
	c_put_str(skill_colour(SKILL_SAVE),buf,13,5);
	sprintf(buf,"Perception:      %d%%",skill_set[SKILL_PERCEPTION].value);
	c_put_str(skill_colour(SKILL_PERCEPTION),buf,15,5);
	sprintf(buf,"Searching:       %d%%",skill_set[SKILL_SEARCH].value);
	c_put_str(skill_colour(SKILL_SEARCH),buf,16,5);
	sprintf(buf,"Disarming:       %d%%",skill_set[SKILL_DISARM].value);
	c_put_str(skill_colour(SKILL_DISARM),buf,18,5);
	sprintf(buf,"Stealth:         %d%%",skill_set[SKILL_STEALTH].value);
	c_put_str(skill_colour(SKILL_STEALTH),buf,19,5);

	/* Specialist Skills */
	sprintf(buf,"Martial Arts:    %d%%",skill_set[SKILL_MA].value);
	c_put_str(skill_colour(SKILL_MA),buf,4,30);
	sprintf(buf,"Mindcrafting:    %d%%",skill_set[SKILL_MINDCRAFTING].value);
	c_put_str(skill_colour(SKILL_MINDCRAFTING),buf,6,30);
	sprintf(buf,"Chi Balance:     %d%%",skill_set[SKILL_CHI].value);
	c_put_str(skill_colour(SKILL_CHI),buf,7,30);
	sprintf(buf,"Spirit Lore:     %d%%",skill_set[SKILL_SHAMAN].value);
	c_put_str(skill_colour(SKILL_SHAMAN),buf,9,30);
	sprintf(buf,"Hedge Magic:     %d%%",skill_set[SKILL_HEDGE].value);
	c_put_str(skill_colour(SKILL_HEDGE),buf,11,30);
	sprintf(buf,"Innate Racial:   %d%%",skill_set[SKILL_RACIAL].value);
	c_put_str(skill_colour(SKILL_RACIAL),buf,13,30);

	/* Hermetic Skills */
	sprintf(buf,"Thaumaturgy:     %d%%",skill_set[SKILL_THAUMATURGY].value);
	c_put_str(skill_colour(SKILL_THAUMATURGY),buf,4,54);
	sprintf(buf,"Necromancy:      %d%%",skill_set[SKILL_NECROMANCY].value);
	c_put_str(skill_colour(SKILL_NECROMANCY),buf,5,54);
	sprintf(buf,"Conjuration:     %d%%",skill_set[SKILL_CONJURATION].value);
	c_put_str(skill_colour(SKILL_CONJURATION),buf,6,54);
	sprintf(buf,"Sorcery:         %d%%",skill_set[SKILL_SORCERY].value);
	c_put_str(skill_colour(SKILL_SORCERY),buf,7,54);
	sprintf(buf,"Magice Corporis: %d%%",skill_set[SKILL_CORPORIS].value);
	c_put_str(skill_colour(SKILL_CORPORIS),buf,9,54);
	sprintf(buf,"Magice Vis:      %d%%",skill_set[SKILL_VIS].value);
	c_put_str(skill_colour(SKILL_VIS),buf,10,54);
	sprintf(buf,"Magice Naturae:  %d%%",skill_set[SKILL_NATURAE].value);
	c_put_str(skill_colour(SKILL_NATURAE),buf,11,54);
	sprintf(buf,"Magice Animae:   %d%%",skill_set[SKILL_ANIMAE].value);
	c_put_str(skill_colour(SKILL_ANIMAE),buf,12,54);
	sprintf(buf,"Mana Channeling: %d%%",skill_set[SKILL_MANA].value);
	c_put_str(skill_colour(SKILL_MANA),buf,14,54);
}

/*
 * Special display, part 2a
 */
static void display_player_misc_info(void)
{
	char	buf[80];

	/* Display basics */
	put_str("Name      :", 2, 1);
	put_str("Sex       :", 3, 1);
	put_str("Race      :", 4, 1);
	put_str("Template  :", 5, 1);

	c_put_str(TERM_L_BLUE, player_name, 2, 13);
	c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 13);
	c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 13);
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 13);

	/* Display extras */
	put_str("Hits(Max) :", 7, 1);
	put_str("Mana(Max) :", 8, 1);

	(void) sprintf(buf, "%d(%d)", (int) p_ptr->chp, (int) p_ptr->mhp);
    c_put_str(TERM_L_BLUE, buf, 7, 13);
    (void) sprintf(buf, "%d(%d)", (int) p_ptr->csp, (int) p_ptr->msp);
	c_put_str(TERM_L_BLUE, buf, 8, 13);	
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
	int i, e_adj;
	int stat_col, stat;
	int row, col;

	object_type *o_ptr;
	u32b f1, f2, f3;
	s16b k_idx;

	byte a;
	char c;

	char buf[80];


	/* Column */
	stat_col = 24;

	/* Row */
	row = 3;

	/* Print out the labels for the columns */
	c_put_str(TERM_WHITE, "Stat", row-1, stat_col);
	c_put_str(TERM_BLUE, "Intrnl", row-1, stat_col+5);
    c_put_str(TERM_L_BLUE, "Rce Cls Mod", row-1, stat_col+12);
	c_put_str(TERM_L_GREEN, "Actual", row-1, stat_col+24);
	c_put_str(TERM_YELLOW, "Currnt", row-1, stat_col+31);
  
	/* Display the stats */
	for (i = 0; i < 6; i++)
	{
		/* Calculate equipment adjustment */
		e_adj = 0;
      
		/* Icky formula to deal with the 18 barrier */
		if ((p_ptr->stat_max[i]>18) && (p_ptr->stat_top[i]>18))
			e_adj = (p_ptr->stat_top[i] - p_ptr->stat_max[i])/10;
		if ((p_ptr->stat_max[i]<=18) && (p_ptr->stat_top[i]<=18))
			e_adj = p_ptr->stat_top[i] - p_ptr->stat_max[i];
		if ((p_ptr->stat_max[i]<=18) && (p_ptr->stat_top[i]>18))
			e_adj = (p_ptr->stat_top[i] - 18)/10 - p_ptr->stat_max[i] + 18;

        if ((p_ptr->stat_max[i]>18) && (p_ptr->stat_top[i]<=18))
            e_adj = p_ptr->stat_top[i] - (p_ptr->stat_max[i]-18)/10 - 19;
      
		/* Deduct template and race bonuses if in maximize */
		if (maximise_mode)
		{
			e_adj -= rp_ptr->r_adj[i];
			e_adj -= cp_ptr->c_adj[i];
		}

		/* Reduced name of stat */
		c_put_str(TERM_WHITE, stat_names_reduced[i], row+i, stat_col);
      
		/* Internal "natural" max value.  Maxes at 18/100 */
		/* This is useful to see if you are maxed out     */
		cnv_stat(p_ptr->stat_max[i], buf);
		c_put_str(TERM_BLUE, buf, row+i, stat_col+5);
      
		/* Race, template, and equipment modifiers */
		(void) sprintf(buf, "%3d", (int) rp_ptr->r_adj[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, stat_col+12);
		(void) sprintf(buf, "%3d", (int) cp_ptr->c_adj[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, stat_col+16);
		(void) sprintf(buf, "%3d", (int) e_adj);
		c_put_str(TERM_L_BLUE, buf, row+i, stat_col+20);
      
		/* Actual maximal modified value */
		cnv_stat(p_ptr->stat_top[i], buf);
		c_put_str(TERM_L_GREEN, buf, row+i, stat_col+24);
      
		/* Only display stat_use if not maximal */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			cnv_stat(p_ptr->stat_use[i], buf);
			c_put_str(TERM_YELLOW, buf, row+i, stat_col+31);
		}
	}

	/* Column */
	col = stat_col + 39;

	/* Header and Footer */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col);
	c_put_str(TERM_L_GREEN, "Modifications", row+6, col);

	/* Process equipment */
	for (i=INVEN_WIELD; i<INVEN_POUCH_1; i++)
	{
		/* Access object */
		o_ptr = &inventory[i];

		/* Object kind */
		k_idx = o_ptr->k_idx;

		/* Acquire "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Initialize color based of sign of pval. */
		for (stat=0; stat<6; stat++)
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
				/* Dark green "s" */
				a = TERM_GREEN;
				c = 's';
			}

			/* Handle monochrome */
			if (!use_color) a = TERM_WHITE;

			/* Dump proper character */
			Term_putch(col, row+stat, a, c);
		}

		/* Advance */
		col++;
    }

	/* Player flags */
	player_flags(&f1, &f2, &f3);

	/* Check stats */
	for (stat=0; stat<6; stat++)
	{
		/* Default */
		a = TERM_SLATE;
		c = '.';

        /* Chaos Features ... */
        if (p_ptr->muta3)
        {
            int dummy = 0;

            if (stat == A_STR)
            {
                if (p_ptr->muta3 & MUT3_HYPER_STR) dummy += 4;
                if (p_ptr->muta3 & MUT3_PUNY)   dummy -= 4;
            }
            else if (stat == A_WIS || stat == A_INT)
            {
                if (p_ptr->muta3 & MUT3_HYPER_INT) dummy += 4;
                if (p_ptr->muta3 & MUT3_MORONIC) dummy -= 4;
            }
            else if (stat == A_DEX)
            {
				if (p_ptr->muta3 & MUT3_IRON_SKIN) dummy -= 1;
				if (p_ptr->muta3 & MUT3_LIMBER) dummy += 3;
				if (p_ptr->muta3 & MUT3_ARTHRITIS) dummy -= 3;
            }
            else if (stat == A_CON)
            {
                if (p_ptr->muta3 & MUT3_RESILIENT) dummy += 4;
                if (p_ptr->muta3 & MUT3_XTRA_FAT) dummy += 2;
                if (p_ptr->muta3 & MUT3_ALBINO) dummy -= 4;
                if (p_ptr->muta3 & MUT3_FLESH_ROT) dummy -= 2;
            }
            else if (stat == A_CHR)
            {
                if (p_ptr->muta3 & MUT3_SILLY_VOI) dummy -= 4;
                if (p_ptr->muta3 & MUT3_BLANK_FAC) dummy -= 1;
                if (p_ptr->muta3 & MUT3_FLESH_ROT) dummy -= 1;
                if (p_ptr->muta3 & MUT3_SCALES) dummy -= 1;
                if (p_ptr->muta3 & MUT3_WART_SKIN) dummy -= 2;
				if (p_ptr->muta3 & MUT3_ILL_NORM) dummy = 0;
            }


			/* Boost */
            if (dummy)
			{
				/* Default */
				c = '*';

				/* Good */
                if (dummy > 0)
				{
					/* Good */
					a = TERM_L_GREEN;

					/* Label boost */
                    if (dummy < 10) c = '0' + dummy;
				}
				
				/* Bad */
                if (dummy < 0)
				{
					/* Bad */
					a = TERM_RED;

					/* Label boost */
                    if (dummy < 10) c = '0' - dummy;
				}
			}
        }


		/* Sustain */
		if (f2 & 1<<stat)
		{
			/* Dark green "s" */
			a = TERM_GREEN;
			c = 's';
		}


		/* No color */
		if (!use_color) a = TERM_WHITE;

		/* Dump */
		Term_putch(col, row+stat, a, c);
    }
}


/*
 * Object flag names
 */
static cptr object_flag_names[96] =
{
	"Add Str",
	"Add Int",
	"Add Wis",
	"Add Dex",
	"Add Con",
	"Add Chr",
	NULL,
	NULL,
	"Add Stea.",
	"Add Sear.",
	"Add Infra",
	"Add Tun..",
	"Add Speed",
	"Add Blows",
    "Chaotic",
    "Vampiric",
	"Slay Anim.",
	"Slay Evil",
	"Slay Und.",
	"Slay Demon",
	"Slay Orc",
	"Slay Troll",
	"Slay Giant",
	"Slay Drag.",
	"Kill Drag.",
    "Sharpness",
	"Impact",
    "Poison Brd",
	"Acid Brand",
	"Elec Brand",
	"Fire Brand",
	"Cold Brand",

	"Sust Str",
	"Sust Int",
	"Sust Wis",
	"Sust Dex",
	"Sust Con",
	"Sust Chr",
	NULL,
	NULL,
	"Imm Acid",
	"Imm Elec",
	"Imm Fire",
	"Imm Cold",
	NULL,
    "Reflect",
	"Free Act",
	"Hold Life",
	"Res Acid",
	"Res Elec",
	"Res Fire",
	"Res Cold",
	"Res Pois",
    "Res Fear",
	"Res Lite",
	"Res Dark",
	"Res Blind",
	"Res Conf",
	"Res Sound",
	"Res Shard",
	"Res Neth",
	"Res Nexus",
	"Res Chaos",
	"Res Disen",



    "Aura Fire",

    "Aura Elec",
 	NULL,
 	NULL,
    "NoTeleport",
    "AntiMagic",
    "WraithForm",
    "EvilCurse",
 	"Easy Know",
 	"Hide Type",
	"Show Mods",
	"Insta Art",
    "Levitate",
	"Lite",
	"See Invis",
	"Telepathy",
	"Digestion",
	"Regen",
	"Xtra Might",
	"Xtra Shots",
	"Ign Acid",
	"Ign Elec",
	"Ign Fire",
	"Ign Cold",
	"Activate",
	"Drain Exp",
	"Teleport",
	"Aggravate",
	"Blessed",
	"Cursed",
	"Hvy Curse",
	"Prm Curse"
};


/*
 * Summarize resistances
 */
static void display_player_ben(void)
{
	int i, x, y;
	
	object_type *o_ptr;

	u32b f1, f2, f3;

	u16b b[6];


	/* Reset */
	for (i = 0; i < 6; i++) b[i] = 0;
	

	/* Scan equipment */
	for (i = INVEN_WIELD; i < INVEN_POUCH_1; i++)
	{
		/* Object */
		o_ptr = &inventory[i];

		/* Known object flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Incorporate */
		b[0] |= (f1 & 0xFFFF);
		b[1] |= (f1 >> 16);
		b[2] |= (f2 & 0xFFFF);
		b[3] |= (f2 >> 16);
		b[4] |= (f3 & 0xFFFF);
		b[5] |= (f3 >> 16);
	}


	/* Player flags */
	player_flags(&f1, &f2, &f3);
	
	/* Incorporate */
	b[0] |= (f1 & 0xFFFF);
	b[1] |= (f1 >> 16);
	b[2] |= (f2 & 0xFFFF);
	b[3] |= (f2 >> 16);
	b[4] |= (f3 & 0xFFFF);
	b[5] |= (f3 >> 16);


	/* Scan cols */
	for (x = 0; x < 6; x++)
	{
		/* Scan rows */
		for (y = 0; y < 16; y++)
		{
			byte a = TERM_SLATE;
			char c = '.';

			cptr name = object_flag_names[16*x+y];

			/* No name */
			if (!name) continue;

			/* Dump name */
			Term_putstr(x * 13, y + 4, -1, TERM_WHITE, name);

			/* Dump colon */
			Term_putch(x * 13 + 10, y + 4, TERM_WHITE, ':');

			/* Check flag */
			if (b[x] & (1<<y))
			{
				a = TERM_WHITE;
				c = '+';
			}

			/* Monochrome */
			if (!use_color) a = TERM_WHITE;

			/* Dump flag */
			Term_putch(x * 13 + 11, y + 4, a, c);
		}
	}
}


/*
 * Summarize resistances
 */
static void display_player_ben_one(int mode)
{
	int i, n, x, y;
	
	object_type *o_ptr;

	u32b f1, f2, f3;

	u16b b[13][6];


	/* Scan equipment */
	for (i = INVEN_WIELD; i < INVEN_POUCH_1; i++)
	{
		/* Index */
		n = (i - INVEN_WIELD);

		/* Object */
		o_ptr = &inventory[i];

		/* Known object flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Incorporate */
		b[n][0] = (short)(f1 & 0xFFFF); 
		b[n][1] = (short)(f1 >> 16);
		b[n][2] = (short)(f2 & 0xFFFF);
		b[n][3] = (short)(f2 >> 16);
		b[n][4] = (short)(f3 & 0xFFFF);
		b[n][5] = (short)(f3 >> 16);
	}


	/* Index */
	n = 12;

	/* Player flags */
	player_flags(&f1, &f2, &f3);
	
	/* Incorporate */
	b[n][0] = (short)(f1 & 0xFFFF);
	b[n][1] = (short)(f1 >> 16);
	b[n][2] = (short)(f2 & 0xFFFF);
	b[n][3] = (short)(f2 >> 16);
	b[n][4] = (short)(f3 & 0xFFFF);
	b[n][5] = (short)(f3 >> 16);


	/* Scan cols */
	for (x = 0; x < 3; x++)
	{
		/* Equippy */
		display_player_equippy(2, x * 26 + 11);

		/* Label */
		Term_putstr(x * 26 + 11, 3, -1, TERM_WHITE, "abcdefghijkl@");

		/* Scan rows */
		for (y = 0; y < 16; y++)
		{
			cptr name = object_flag_names[48*mode+16*x+y];

			/* No name */
			if (!name) continue;

			/* Dump name */
			Term_putstr(x * 26, y + 4, -1, TERM_WHITE, name);

			/* Dump colon */
			Term_putch(x * 26 + 10, y + 4, TERM_WHITE, ':');

			/* Check flags */
			for (n = 0; n < 13; n++)
			{
				byte a = TERM_SLATE;
				char c = '.';

				/* Check flag */
				if (b[n][3*mode+x] & (1<<y))
				{
					a = TERM_WHITE;
					c = '+';
				}

				/* Monochrome */
				if (!use_color) a = TERM_WHITE;

				/* Dump flag */
				Term_putch(x * 26 + 11 + n, y + 4, a, c);
			}
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
 * Mode 2 = skills display
 * Mode 3 = summary of various things
 * Mode 4 = current flags (combined)
 * Mode 5 = current flags (part 1)
 * Mode 6 = current flags (part 2)
 * Mode 7 = chaos features
 */
void display_player(int mode)
{
	int i;

	char	buf[80];

	/* XXX XXX XXX */
    if ((p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3) && !(skip_chaos_features))
        mode = (mode % 8);
    else
        mode = (mode % 7);

	/* Erase screen */
	clear_from(0);

	/* Standard */
	if ((mode == 0) || (mode == 1))
	{
		/* Name, Sex, Race, template */
		put_str("Name        :", 2, 1);
		put_str("Sex         :", 3, 1);
		put_str("Race        :", 4, 1);
		put_str("Template    :", 5, 1);

		c_put_str(TERM_L_BLUE, player_name, 2, 15);
		c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 15);
		c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 15);
		c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

		/* Age, Height, Weight, Social, Birthday */
		prt_num("Age          ", (int)p_ptr->age, 2, 32, TERM_L_BLUE);
		prt_num("Height       ", (int)p_ptr->ht, 3, 32, TERM_L_BLUE);
		prt_num("Weight       ", (int)p_ptr->wt, 4, 32, TERM_L_BLUE);
		prt_num("Social Class ", (int)p_ptr->sc, 5, 32, TERM_L_BLUE);
		put_str("Birthday",6,32);
		day_to_date(p_ptr->birthday,buf);
		c_put_str(TERM_L_BLUE,buf,6,46);

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
				put_str(history[i], i + 16, 10);
			}
		}

		/* Display "various" info */
		else
		{
			put_str("(Miscellaneous Abilities)", 15, 25);

			display_player_various();
		}
	}

	else if (mode == 2)
	{
		/* Do nothing */
		display_player_skills();
	}
	
	/* Special */
	else if (mode == 3)
	{
		/* See "http://www.cs.berkeley.edu/~davidb/angband.html" */

		/* Dump the info */
		display_player_misc_info();
		display_player_stat_info();
		display_player_flag_info();
	}
	
	/* Special */
	else if (mode == 4)
	{
		display_player_ben();
	}

    else if (mode == 7)
    {
        do_cmd_knowledge_chaos_features();
    }
	
	/* Special */
	else
	{
		display_player_ben_one((mode-1) % 2);
	}
}

void dump_final_messages(FILE * OutFile)
{
	short i;

	if (!OutFile) return;

	/* Reverse order */
	for(i=9;i>=0;i--)
	{
		fprintf(OutFile,"%s\n",message_str(i));
	}
}


/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(cptr name, bool full)
{
	int			i, x, y;

	byte		a;
	char		c;

	cptr		paren = ")";

	int			fd = -1;

	FILE		*fff = NULL;

	char		o_name[80];

	char		buf[1024];


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
		(void)fd_close(fd);

		/* Build query */
		(void)sprintf(out_val, "Replace existing file %s? ", buf);

		/* Ask */
		if (get_check(out_val)) fd = -1;
	}

	/* Open the non-existing file */
	if (fd < 0) fff = my_fopen(buf, "w");

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
	fprintf(fff, "  [Cthangband %d.%d.%d Character Dump]\n\n",
	        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);


	/* Display player */
	display_player(0);

	/* Dump part of the screen */
	for (y = 2; y < 22; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Show Skills */
	display_player(2);

	/* Dump part of the screen */
	for (y = 1; y < 22; y++)
	{
		/* Dump each row */
		for (x = 0; x < 79; x++)
		{
			/* Get the attr/char */
			(void)(Term_what(x, y, &a, &c));

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
			(void)(Term_what(x, y, &a, &c));

			/* Dump it */
			buf[x] = c;
		}

		/* Terminate */
		buf[x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}


        fprintf(fff, "\n\n  [Miscellaneous information]\n");
        if (ironman_shop)
            fprintf(fff, "\n Ironman Shops:     ON");
        else
            fprintf(fff, "\n Ironman Shops:     OFF");

        if (maximise_mode)
            fprintf(fff, "\n Maximize Mode:      ON");
        else
            fprintf(fff, "\n Maximize Mode:      OFF");

        if (preserve_mode)
            fprintf(fff, "\n Preserve Mode:      ON");
        else
            fprintf(fff, "\n Preserve Mode:      OFF");

        if (auto_scum)
            fprintf(fff, "\n Autoscum:           ON");
        else
            fprintf(fff, "\n Autoscum:           OFF");

		if (dungeon_small)
			fprintf(fff, "\n Small Levels:       Always");
		else
		{
			if (small_levels)
				fprintf(fff, "\n Small Levels:       ON");
			else
				fprintf(fff, "\n Small Levels:       OFF");
		}

        if (empty_levels)
            fprintf(fff, "\n Arena Levels:       ON");
        else
            fprintf(fff, "\n Arena Levels:       OFF");

        if (multi_stair)
            fprintf(fff, "\n Long Stairs:       ON");
        else
            fprintf(fff, "\n Long Stairs:       OFF");

            fprintf(fff, "\n Recall Depth:       Level %d (%d')\n", p_ptr->max_dlv[cur_dungeon],
                          50 * (p_ptr->max_dlv[cur_dungeon]));


        if (noscore)
            fprintf(fff, "\n You have done something illegal.");

        if (stupid_monsters)
            fprintf(fff, "\n Your opponents are behaving stupidly.");


    { /* Monsters slain */
        int k;
        s32b Total = 0;

        for (k = 1; k < MAX_R_IDX-1; k++)
        {
            monster_race *r_ptr = &r_info[k];

            if (r_ptr->flags1 & (RF1_UNIQUE))
            {
                bool dead = (r_ptr->max_num == 0);
                if (dead)
                {
                    Total++;
                }
            }
            else
            {
                s16b This = r_ptr->r_pkills;
                if (This > 0)
                {
                    Total += This;
                }
            }
        }

        if (Total < 1)
            fprintf(fff,"\n You have defeated no enemies yet.\n");
        else if (Total == 1)

            fprintf(fff,"\n You have defeated one enemy.\n");
        else
           fprintf(fff,"\n You have defeated %lu enemies.\n", Total);
    }
    

    if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
    {
        fprintf(fff, "\n\n  [Chaos Features]\n\n");
        dump_chaos_features(fff);
    }


	/* Skip some lines */
	fprintf(fff, "\n\n");


	/* Dump the equipment */
	if (equip_cnt)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			object_desc(o_name, &inventory[i], TRUE, 3);
			fprintf(fff, "%c%s %s\n",
			        index_to_label(i), paren, o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	fprintf(fff, "  [Character Inventory]\n\n");
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_desc(o_name, &inventory[i], TRUE, 3);
		fprintf(fff, "%c%s %s\n",
		        index_to_label(i), paren, o_name);
	}
	fprintf(fff, "\n\n");

	fprintf(fff, "  [Last Ten Messages]\n\n");
	dump_final_messages(fff);

	/* Close it */
	my_fclose(fff);


	/* Message */
	msg_print("Character dump successful.");
	msg_print(NULL);

	/* Success */
	return (0);
}



/*
 * Recursive "help file" perusal.  Return FALSE on "ESCAPE".
 *
 * XXX XXX XXX Consider using a temporary file.
 */
static bool do_cmd_help_aux(cptr name, cptr what, int line)
{
	int		i, k;

	/* Number of "real" lines passed by */
	int		next = 0;

	/* Number of "real" lines in the file */
	int		size = 0;

	/* loop counter */
	int cnt;

	/* Backup value for "line" */
	int		back = 0;

	/* This screen has sub-screens */
	bool	menu = FALSE;

	/* Current help file */
	FILE	*fff = NULL;

	/* Find this string (if any) */
	cptr	find = NULL;

	/* Hold a string to find */
	char	finder[128];

	/* Hold a string to show */
	char	shower[128];

	/* Describe this thing */
	char	caption[128];

	/* Path buffer */
	char	path[1024];

	/* General buffer */
	char	buf[1024];

	/* Pointer for making uc_buf uppercase */
	cptr uc_buf_ptr;

	/* Sub-menu information */
	char	hook[26][32];

	/* Upper-cased versions to make searching insensitive */
	char uc_finder[128];
	char uc_shower[128];
	char uc_buf[1024];

	/* Wipe finder */
	strcpy(finder, "");
	strcpy(uc_finder,"");

	/* Wipe shower */
	strcpy(shower, "");
	strcpy(uc_shower,"");

	/* Wipe caption */
	strcpy(caption, "");

	/* Wipe the hooks */
	for (i = 0; i < 10; i++) hook[i][0] = '\0';


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
		if (my_fgets(fff, buf, 1024)) break;

		/* XXX Parse "menu" items */
		if (prefix(buf, "***** "))
		{
			char b1 = '[', b2 = ']';

			/* Notice "menu" requests */
			if ((buf[6] == b1) && islower(buf[7]) &&
			    (buf[8] == b2) && (buf[9] == ' '))
			{
				/* This is a menu file */
				menu = TRUE;

				/* Extract the menu item */
				k = buf[7] - 'a';

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
		if (line >= size) line = 0;


		/* Re-open the file if needed */
		if (next > line)
		{
			/* Close it */
			my_fclose(fff);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");

			/* Oops */
			if (!fff) return (FALSE);

			/* File has been restarted */
			next = 0;
		}

		/* Skip lines if needed */
		for (; next < line; next++)
		{
			/* Skip a line */
			if (my_fgets(fff, buf, 1024)) break;
		}


		/* Dump the next 20 lines of the file */
		for (i = 0; i < 20; )
		{
			/* Hack -- track the "first" line */
			if (!i) line = next;

			/* Get a line of the file or stop */
			if (my_fgets(fff, buf, 1024)) break;

			/* Hack -- skip "special" lines */
			if (prefix(buf, "***** ")) continue;

			/* Count the "real" lines */
			next++;

			/* Make an upper case version of buf for searching */
			strcpy(uc_buf, buf);
			for (uc_buf_ptr = uc_buf; *uc_buf_ptr != 0; uc_buf_ptr++)
			{
				uc_buf[uc_buf_ptr-uc_buf] = toupper(*uc_buf_ptr);
			}		   

			/* Hack -- keep searching */
			if (find && !i && !strstr(uc_buf, find)) continue;

			/* Hack -- stop searching */
			find = NULL;

			/* Dump the line */
			Term_putstr(0, i+2, -1, TERM_WHITE, buf);

			/* Hilite "shower" */
			if (uc_shower[0])
			{
				cptr str = uc_buf;

				/* Display matches */
				while ((str = strstr(str, uc_shower)) != NULL)
				{
					int len = strlen(uc_shower);

					/* Display the match */
					Term_putstr(str-uc_buf, i+2, len, TERM_YELLOW, uc_shower);

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
		prt(format("[Cthangband %d.%d.%d, %s, Line %d/%d]",
		           VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH,
		           caption, line, size), 0, 0);

		/* Prompt -- menu screen */
		if (menu)
		{
			/* Wait for it */
			prt("[Press a letter, or ESC to exit.]", 23, 0);
		}

		/* Prompt -- small files */
		else if (size <= 20)
		{
			/* Wait for it */
			prt("[Press ESC to exit.]", 23, 0);
		}

		/* Prompt -- large files */
		else
		{
			/* Wait for it */
            prt("[Press Return, Space, -, =, /, f, or ESC to exit.]", 23, 0);
		}

		/* Get a keypress */
		k = inkey();

		/* Hack -- return to last screen */
		if (k == '?') break;

		/* Hack -- try showing */
		if (k == '=')
		{
			/* Get "shower" */
			prt("Show: ", 23, 0);
			(void)askfor_aux(shower, 80);

			/* Make finder uppercase */
			strcpy(uc_shower,shower);
			for (cnt = 0; uc_shower[cnt] != 0; cnt++) 
			{
				uc_shower[cnt] = toupper(uc_shower[cnt]);
			}
		}

		/* Hack -- try finding */
		if (k == '/')
		{
			/* Get "finder" */
			prt("Find: ", 23, 0);
			if (askfor_aux(finder, 80))
			{

				/* Make finder uppercase */
				strcpy(uc_finder,finder);
				for (cnt = 0; uc_finder[cnt] != 0; cnt++) 
				{
					uc_finder[cnt] = toupper(uc_finder[cnt]);
				}
				
				/* Find it */
				find = uc_finder;
				back = line;
				line = line + 1;

				/* Show it */
				strcpy(shower,finder);
				strcpy(uc_shower, uc_finder);
			}
		}

		/* Hack -- go to a specific line */
		if (k == '#')
		{
			char tmp[80];
			prt("Goto Line: ", 23, 0);
			strcpy(tmp, "0");
			if (askfor_aux(tmp, 80))
			{
				line = atoi(tmp);
			}
		}

		/* Hack -- go to a specific file */
		if (k == '%')
		{
			char tmp[80];
			prt("Goto File: ", 23, 0);
			strcpy(tmp, syshelpfile);
			if (askfor_aux(tmp, 80))
			{
				if (!do_cmd_help_aux(tmp, NULL, 0)) k = ESCAPE;
			}
		}

		/* Hack -- Allow backing up */
		if (k == '-')
		{
			line = line - 10;
			if (line < 0) line = 0;
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
		if (menu && islower(k) && hook[k-'a'][0])
		{
			/* Recurse on that file */
			if (!do_cmd_help_aux(hook[k-'a'], NULL, 0)) k = ESCAPE;
		}

        /* Hack, dump to file */
        if (k == 'F')
        {

            FILE        *ffp;

            char    buff[1024];

            char xtmp[82];

            strcpy (xtmp, "");

            if (get_string("File name: ", xtmp, 80))
			{
                if (xtmp[0] && (xtmp[0] != ' '))
				{

				}
			}
            else
            {
                continue;
            }

            /* Build the filename */
            path_build(buff, 1024, ANGBAND_DIR_USER, xtmp);

            /* Close it */
			my_fclose(fff);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");

            ffp = my_fopen(buff, "w");

			/* Oops */
            if (!(fff && ffp))
                { msg_print("Failed to open file.");
                  k = ESCAPE;
                  break;
                }

            sprintf(xtmp, "%s: %s", player_name, what);
            my_fputs(ffp, xtmp, 80);
            my_fputs(ffp, "\n", 80);
            while (!my_fgets(fff, buff, 80)) my_fputs(ffp, buff, 80);

            /* Close it */
			my_fclose(fff);
            my_fclose(ffp);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");
            
        }

        if ((k == 'h') && (!menu))  /* Hack, added for character display */
        {
            k = ESCAPE;
        }

		/* Exit on escape */
		if (k == ESCAPE) break;
	}

	/* Close the file */
	my_fclose(fff);

	/* Escape */
	if (k == ESCAPE) return (FALSE);

	/* Normal return */
	return (TRUE);
}


/*
 * Peruse the On-Line-Help, starting at the given file.
 */
void do_cmd_help(cptr name)
{
	/* Hack -- default file */
	if (!name) name = syshelpfile;

	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Peruse the main help file */
	(void)do_cmd_help_aux(name, NULL, 0);

	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
}



/*
 * Hack -- display the contents of a file on the screen
 *
 * XXX XXX XXX Use this function for commands such as the
 * "examine object" command.
 */
errr show_file(cptr name, cptr what)
{
	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Peruse the requested file */
	(void)do_cmd_help_aux(name, what, 0);

	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;

	/* Success */
	return (0);
}




/*
 * Process the player name.
 * Extract a clean "base name".
 * Build the savefile name if needed.
 */
void process_player_name(void)
{
	int i, k = 0;


	/* Cannot be too long */
	if (strlen(player_name) > 15)
	{
		/* Name too long */
		quit_fmt("The name '%s' is too long!", player_name);
	}

	/* Cannot contain "icky" characters */
	for (i = 0; player_name[i]; i++)
	{
		/* No control characters */
		if (iscntrl(player_name[i]))
		{
			/* Illegal characters */
			quit_fmt("The name '%s' contains control chars!", player_name);
		}
	}


#ifdef MACINTOSH

	/* Extract "useful" letters */
	for (i = 0; player_name[i]; i++)
	{
		char c = player_name[i];

		/* Convert "dot" to "underscore" */
		if (c == '.') c = '_';

		/* Accept all the letters */
		player_base[k++] = c;
	}

#else

	/* Extract "useful" letters */
	for (i = 0; player_name[i]; i++)
	{
		char c = player_name[i];

		/* Accept some letters */
		if (isalpha(c) || isdigit(c)) player_base[k++] = c;

		/* Convert space, dot, and underscore to underscore */
		else if (strchr(". _", c)) player_base[k++] = '_';
	}

#endif


#if defined(WINDOWS) || defined(MSDOS)
	/* Hack -- max length */
	if (k > 8) k = 8;
#endif

	/* Terminate */
	player_base[k] = '\0';

	/* Require a "base" name */
	if (!player_base[0]) strcpy(player_base, "PLAYER");


	/* Change the savefile name */
	if (!savefile[0])
	{
		char temp[128];

#ifdef SAVEFILE_USE_UID
		/* Rename the savefile, using the player_uid and player_base */
		(void)sprintf(temp, "%d.%s", player_uid, player_base);
#else
		/* Rename the savefile, using the player_base */
		(void)sprintf(temp, "%s", player_base);
#endif

#ifdef VM
		/* Hack -- support "flat directory" usage on VM/ESA */
		(void)sprintf(temp, "%s.sv", player_base);
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
		strcpy(tmp, player_name);

		/* Get an input, ignore "Escape" */
		if (askfor_aux(tmp, 15)) strcpy(player_name, tmp);

		/* Process the player name */
		process_player_name();

		/* All done */
		break;
	}

	/* Pad the name (to clear junk) */
	sprintf(tmp, "%-15.15s", player_name);

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
	if (total_winner)
	{
		/* Verify */
		if (!get_check("Do you want to retire? ")) return;
	}

	/* Verify Suicide */
	else
	{
		/* Verify */
		if (!get_check("Do you really want to end it all? ")) return;
        if (!noscore)
        {
		/* Special Verification for suicide */
		prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
		flush();
		i = inkey();
		prt("", 0, 0);
		if (i != '@') return;
          }
	}

	/* Stop playing */
	alive = FALSE;

	/* Kill the player */
	death = TRUE;

	/* Cause of death */
	(void)strcpy(died_from, "Suicide");
}



/*
 * Save the game
 */
void do_cmd_save_game(void)
{
    /* Autosaves do not disturb */
    if (!is_autosave)
    {
        /* Disturb the player */
        disturb(1, 0);
    }

	/* Clear messages */
	msg_print(NULL);

	/* Handle stuff */
	handle_stuff();

	/* Message */
	prt("Saving game...", 0, 0);

	/* Refresh */
	Term_fresh();

	/* The player is not dead */
	(void)strcpy(died_from, "(saved)");

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
	(void)strcpy(died_from, "(alive and well)");
}



/*
 * Hack -- Calculates the total number of points earned		-JWT-
 */
long total_points(void)
{
	u32b i,j,best;
	best=0;
	for(i=0;i<MAX_CAVES;i++)
	{
		j=p_ptr->exp + (100 * (p_ptr->max_dlv[i]) + dun_defs[i].offset);
		if(j > best) best = j;
	}
	return (best);
}



/*
 * Centers a string within a 31 character string		-JWT-
 */
static void center_string(char *buf, cptr str)
{
	int i, j;

	/* Total length */
	i = strlen(str);

	/* Necessary border */
	j = 15 - i / 2;

	/* Mega-Hack */
	(void)sprintf(buf, "%*s%s%*s", j, "", str, 31 - i - j, "");
}


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
	FILE                *fp;

	char                str[1024];


	if ((p_ptr->prace == RACE_SKELETON) || (p_ptr->prace == RACE_ZOMBIE) ||
		(p_ptr->prace == RACE_SPECTRE) || (p_ptr->prace == RACE_VAMPIRE))
	{
		return;
	}


	/* Ignore wizards and borgs */
	if (!(noscore & 0x00FF))
	{
		/* Ignore people who die in town */
		if (dun_level)
		{
			char tmp[128];

			/* XXX XXX XXX "Bones" name */
			sprintf(tmp, "bone.%03d", dun_level);

			/* Build the filename */
			path_build(str, 1024, ANGBAND_DIR_BONE, tmp);

			/* Attempt to open the bones file */
			fp = my_fopen(str, "r");

			/* Close it right away */
			if (fp) my_fclose(fp);

			/* Do not over-write a previous ghost */
			if (fp) return;

			/* File type is "TEXT" */
			FILE_TYPE(FILE_TYPE_TEXT);

			/* Try to write a new "Bones File" */
			fp = my_fopen(str, "w");

			/* Not allowed to write it?  Weird. */
			if (!fp) return;

			/* Save the info */
			fprintf(fp, "%s\n", player_name);
			fprintf(fp, "%d\n", p_ptr->mhp);
			fprintf(fp, "%d\n", p_ptr->prace);
			fprintf(fp, "%d\n", p_ptr->ptemplate);

			/* Close and save the Bones file */
			my_fclose(fp);
		}
	}
}


/*
 * Redefinable "print_tombstone" action
 */
bool (*tombstone_aux)(void) = NULL;


/*
 * Display a "tomb-stone"
 */
static void print_tomb(void)
{
	bool done = FALSE;

	/* Do we use a special tombstone ? */
	if (tombstone_aux)
	{
		/* Use tombstone hook */
		done = (*tombstone_aux)();
	}

	/* Print the text-tombstone */
	if (!done)
	{

	char	tmp[160];

	char	buf[1024];
    char    dummy[80];

	FILE        *fp;

	time_t	ct = time((time_t)0);


	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, "dead.txt");

	/* Open the News file */
	fp = my_fopen(buf, "r");

	/* Dump */
	if (fp)
	{
		int i = 0;

		/* Dump the file to the screen */
		while (0 == my_fgets(fp, buf, 1024))
		{
			/* Display and advance */
			put_str(buf, i++, 0);
		}

		/* Close */
		my_fclose(fp);
	}


	center_string(buf, player_name);

	put_str(buf, 6, 11);

	/* King or Queen */
	if (total_winner)
	{
		center_string(buf, "the");
		put_str(buf, 7, 11);
		center_string(buf,"Magnificent");
		put_str(buf,8,11);
	}
	/* Normal */
	else
	{
		center_string(buf, "the");
		put_str(buf, 8, 11);
	}


	center_string(buf, rp_ptr->title);
	put_str(buf, 9, 11);


	center_string(buf, cp_ptr->title);
	put_str(buf, 10, 11);

	(void)sprintf(tmp, "Exp: %ld", (long)p_ptr->exp);
	center_string(buf, tmp);
	put_str(buf, 12, 11);

	(void)sprintf(tmp, "AU: %ld", (long)p_ptr->au);
	center_string(buf, tmp);
	put_str(buf, 13, 11);

	(void)sprintf(tmp, "Killed on Level %d", dun_level);
	center_string(buf, tmp);
	put_str(buf, 14, 11);


    if (strlen(died_from) > 24)
    {
        strncpy(dummy, died_from, 24);
        dummy[24] = '\0';
        (void)sprintf(tmp, "by %s.", dummy);
    }
    else
        (void)sprintf(tmp, "by %s.", died_from);
	center_string(buf, tmp);
	put_str(buf, 15, 11);


	(void)sprintf(tmp, "%-.24s", ctime(&ct));
	center_string(buf, tmp);
	put_str(buf, 17, 11);

	msg_format("Goodbye, %s!", player_name);
	}
}


/*
 * Display some character info
 */
static void show_info(void)
{
	int			i;

	object_type		*o_ptr;

	/* Hack -- Know everything in the inven/equip */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;
		
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
	prt("You may now dump a character record to one or more files.", 21, 0);
	prt("Then, hit RETURN to see the character, or ESC to abort.", 22, 0);

	/* Dump character records as requested */
	while (TRUE)
	{
		char out_val[160];

		/* Prompt */
		put_str("Filename: ", 23, 0);

		/* Default */
		strcpy(out_val, "");

		/* Ask for filename (or abort) */
		if (!askfor_aux(out_val, 60)) return;

		/* Return means "show on screen" */
		if (!out_val[0]) break;

		/* Save screen */
		Term_save();

		/* Dump a character file */
		(void)file_character(out_val, FALSE);

		/* Load screen */
		Term_load();
	}


	/* Display player */
	display_player(0);

	/* Prompt for inventory */
	prt("Hit any key to see more information (ESC to abort): ", 23, 0);

	/* Allow abort at this point */
	if (inkey() == ESCAPE) return;


	/* Show equipment and inventory */

	/* Equipment -- if any */
	if (equip_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_equip();
		prt("You are using: -more-", 0, 0);
		if (inkey() == ESCAPE) return;
	}

	/* Inventory -- if any */
	if (inven_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_inven();
		prt("You are carrying: -more-", 0, 0);
		if (inkey() == ESCAPE) return;
	}
}





/*
 * Semi-Portable High Score List Entry (128 bytes) -- BEN
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
	char what[8];		/* Version info (string) */

	char pts[10];		/* Total Score (number) */

	char gold[10];		/* Total Gold (number) */

	char turns[10];		/* Turns Taken (number) */

	char day[10];		/* Time stamp (string) */

	char who[16];		/* Player Name (string) */

	char uid[8];		/* Player UID (number) */

	char sex[2];		/* Player Sex (string) */
	char p_r[3];		/* Player Race (number) */
	char p_c[3];		/* Player Template (number) */

	char cur_lev[4];		/* Current Player Level (number) */
	char cur_dun[4];		/* Current Dungeon Level (number) */
	char max_lev[4];		/* Max Player Level (number) */
	char max_dun[4];		/* Max Dungeon Level (number) */

	char how[32];		/* Method of death (string) */
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
	int			i;

	high_score		the_score;

	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return (-1);

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
	int			i, slot;
	bool		done = FALSE;

	high_score		the_score, tmpscore;


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
	int		i, j, k, n, attr, place;
	int psc;

	high_score	the_score;

	char	out_val[256];
	char	tmp_val[160];


	/* Paranoia -- it may not have opened */
	if (highscore_fd < 0) return;


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


	/* Show 5 per page, until "done" */
	for (k = from, place = k+1; k < i; k += 5)
	{
		/* Clear screen */
		Term_clear();

		/* Title */
        put_str("               Cthangband Hall of Fame", 0, 0);

		/* Indicate non-top scores */
		if (k > 0)
		{
			sprintf(tmp_val, "(from position %d)", k + 1);
			put_str(tmp_val, 0, 40);
		}

		/* Dump 5 entries */
		for (j = k, n = 0; j < i && n < 5; place++, j++, n++)
		{
			int pr, pc, clev, mlev, cdun, mdun;

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

			/* Extract the race/template */
			pr = atoi(the_score.p_r);
			pc = atoi(the_score.p_c);
			/* Pull the subclass info out of the class entry (archaic score files only) */
			psc=pc/100;
			pc=pc-(100*psc);


			/* Extract the level info */
			clev = atoi(the_score.cur_lev);
			mlev = atoi(the_score.max_lev);
			cdun = atoi(the_score.cur_dun);
			mdun = atoi(the_score.max_dun);

			/* Hack -- extract the gold and such */
			for (user = the_score.uid; isspace(*user); user++) /* loop */;
			for (when = the_score.day; isspace(*when); when++) /* loop */;
			for (gold = the_score.gold; isspace(*gold); gold++) /* loop */;
			for (aged = the_score.turns; isspace(*aged); aged++) /* loop */;

			/* Dump some info */
			sprintf(out_val, "%3d.%9s  %s the %s %s",
			        place, the_score.pts, the_score.who,
			        race_info[pr].title, template_info[pc].title);

			/* Dump the first line */
			c_put_str((byte)attr, out_val, n*4 + 2, 0);

			/* Another line of info */
			sprintf(out_val, "               Killed by %s on %s %d",
			        the_score.how, "Dungeon Level", cdun);

			/* Hack -- some people die in the town */
			if (!cdun)
			{
				sprintf(out_val, "               Killed by %s on the surface",
				        the_score.how);
			}

			/* Append a "maximum level" */
			if (mdun > cdun) strcat(out_val, format(" (Max %d)", mdun));

			/* Dump the info */
			c_put_str((byte)attr, out_val, n*4 + 3, 0);

			/* And still another line of info */
			sprintf(out_val,
			        "               (User %s, Date %s, Gold %s, Turn %s).",
			        user, when, gold, aged);
			c_put_str((byte)attr, out_val, n*4 + 4, 0);
		}


		/* Wait for response */
		prt("[Press ESC to quit, any other key to continue.]", 23, 17);
		j = inkey();
		prt("", 23, 0);

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

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Paranoia -- No score file */
	if (highscore_fd < 0) quit("Score file unavailable.");

	/* Clear screen */
	Term_clear();

	/* Display the scores */
	display_scores_aux(from, to, -1, NULL);

	/* Shut the high score file */
	(void)fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;

	/* Quit */
	quit(NULL);
}

/*
 * templare_score - selectively list highscores based on template
 * -KMW-
 */
void template_score(int ptemplate)
{

	register int i = 0, j, m = 0;
	int pr, pc, clev;
	high_score the_score;
	char buf[1024], out_val[256],tmp_str[80];
	
	sprintf(tmp_str,"The Masters of the %s Profession",template_info[ptemplate].title);
	prt(tmp_str,5,0);
	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return;
	}

	if (highscore_seek(0)) return;

	for (i = 0; i < MAX_HISCORES; i++)
		if (highscore_read(&the_score)) break;

	m=0;
	j=0;
	clev = 0;

	while ((m < 10) && (j < MAX_HISCORES))
	{
		if (highscore_seek(j)) break;
		if (highscore_read(&the_score)) break;
		pr = atoi(the_score.p_r);
		pc = atoi(the_score.p_c);
		clev = atoi(the_score.cur_lev);
		if (pc == ptemplate)
		{
			sprintf(out_val, "%3d) %s the %s (Score %d)",
			    (m + 1), the_score.who,
				race_info[pr].title,atoi(the_score.pts));
			prt(out_val, (m + 7), 0);
			m++;
		}
		j++;
	}

	/* Now, list the active player if they qualify */
	if (p_ptr->ptemplate == ptemplate)
	{
		sprintf(out_val, "You) %s the %s (Score %d)",
		player_name,
		race_info[p_ptr->prace].title, total_points());
		prt(out_val, (m + 8), 0);
	}

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
	msg_print("Hit any key to continue");
	msg_print(NULL);
	for (j=5;j<18;j++)
		prt("",j,0);
}


/*
 * Race Legends
 * -KMW- 
 */
void race_score(int race_num)
{
	register int i = 0, j, m = 0;
	int pr, pc, clev, lastlev,psc;
	high_score the_score;
	char buf[1024], out_val[256], tmp_str[80];
	
	lastlev = 0;
	(void) sprintf(tmp_str,"The %s Heroes of Note",race_info[race_num].title);
	prt(tmp_str, 5, 15);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	highscore_fd = fd_open(buf, O_RDONLY);

	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return;
	}

	if (highscore_seek(0)) return;

	for (i = 0; i < MAX_HISCORES; i++) {
		if (highscore_read(&the_score)) break;
	}

	m=0;
	j=0;

	while ((m < 10) && (j < MAX_HISCORES))
	{
		if (highscore_seek(j)) break;
		if (highscore_read(&the_score)) break;
		pr = atoi(the_score.p_r);
		pc = atoi(the_score.p_c);
		psc=pc/100;
		pc=pc-(psc*100);
		clev = atoi(the_score.cur_lev);
		if (pr == race_num)
		{
			sprintf(out_val, "%3d) %s, the %s (Score %d)",
			    (m + 1), the_score.who,
			template_info[pc].title, atoi(the_score.pts));
			prt(out_val, (m + 7), 0);
			m++;
			lastlev = clev;  
		}
		j++;
	}

	/* add player if qualified */
	if (p_ptr->prace == race_num)
	{
		sprintf(out_val, "You) %s, the %s (Score %d)",
		    player_name, 
			cp_ptr->title,total_points());
		prt(out_val, (m + 8), 0);
	}

	(void)fd_close(highscore_fd);
	highscore_fd = -1;
	msg_print("Hit any key to continue");
	msg_print(NULL);
	for (j=5;j<18;j++)
		prt("",j,0);
}


/*
 * Enters a players name on a hi-score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static errr top_twenty(void)
{
	int          i,j,best;

	high_score   the_score;

	time_t ct = time((time_t*)0);


	/* Clear screen */
	Term_clear();

	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return (0);
	}

#ifndef SCORE_BORGS
	/* Borg-mode pre-empts scoring */
	if (noscore & 0x00F0)
	{
		msg_print("Score not registered for borgs.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}
#endif

#ifndef SCORE_CHEATERS
	/* Cheaters are not scored */
	if (noscore & 0xFF02)
	{
		msg_print("Score not registered for cheaters.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}
#endif

	/* Interupted */
	if (!total_winner && streq(died_from, "Interrupting"))
	{
		msg_print("Score not registered due to interruption.");
		msg_print(NULL);
		display_scores_aux(0, 10, -1, NULL);
		return (0);
	}


	/* Clear the record */
	WIPE(&the_score, high_score);

	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
	        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9ld", (long)total_points());
	the_score.pts[9] = '\0';

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);
	the_score.gold[9] = '\0';

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);
	the_score.turns[9] = '\0';

#ifdef HIGHSCORE_DATE_HACK
	/* Save the date in a hacked up form (9 chars) */
	sprintf(the_score.day, "%-.6s %-.2s", ctime(&ct) + 4, ctime(&ct) + 22);
#else
	/* Save the date in standard form (8 chars) */
	strftime(the_score.day, 9, "%m/%d/%y", localtime(&ct));
#endif

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->ptemplate);

	/* Save the level and such */
	sprintf(the_score.cur_dun, "%3d", dun_level);
	
	/* Work out the deepest level */
	best=0;
	for(i=0;i<MAX_CAVES;i++)
	{
		if (p_ptr->max_dlv[i] > best) best=p_ptr->max_dlv[i];
	}
	sprintf(the_score.max_dun, "%3d", best);

	/* Save the cause of death (31 chars) */
	sprintf(the_score.how, "%-.31s", died_from);


	/* Lock (for writing) the highscore file, or fail */
	if (fd_lock(highscore_fd, F_WRLCK)) return (1);

	/* Add a new entry to the score list, see where it went */
	j = highscore_add(&the_score);

	/* Unlock the highscore file, or fail */
	if (fd_lock(highscore_fd, F_UNLCK)) return (1);


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
	int          i,j,best;

	high_score   the_score;


	/* No score file */
	if (highscore_fd < 0)
	{
		msg_print("Score file unavailable.");
		msg_print(NULL);
		return (0);
	}


	/* Save the version */
	sprintf(the_score.what, "%u.%u.%u",
	        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

	/* Calculate and save the points */
	sprintf(the_score.pts, "%9ld", (long)total_points());

	/* Save the current gold */
	sprintf(the_score.gold, "%9lu", (long)p_ptr->au);

	/* Save the current turn */
	sprintf(the_score.turns, "%9lu", (long)turn);

	/* Hack -- no time needed */
	strcpy(the_score.day, "TODAY");

	/* Save the player name (15 chars) */
	sprintf(the_score.who, "%-.15s", player_name);

	/* Save the player info XXX XXX XXX */
	sprintf(the_score.uid, "%7u", player_uid);
	sprintf(the_score.sex, "%c", (p_ptr->psex ? 'm' : 'f'));
	sprintf(the_score.p_r, "%2d", p_ptr->prace);
	sprintf(the_score.p_c, "%2d", p_ptr->ptemplate);

	/* Save the level and such */
	sprintf(the_score.cur_dun, "%3d", dun_level);

	/* Work out the deepest level */
	best=0;
	for(i=0;i<MAX_CAVES;i++)
	{
		if (p_ptr->max_dlv[i] > best) best=p_ptr->max_dlv[i];
	}
	sprintf(the_score.max_dun, "%3d", best);

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
 * Change the player into a King!			-RAK-
 */
static void kingly(void)
{
	/* Hack -- retire in town */
	dun_level = 0;

	/* Fake death */
	(void)strcpy(died_from, "Ripe Old Age");

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
	character_icky = TRUE;


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);


	/* Handle death */
	if (death)
	{
		/* Handle retirement */
		if (total_winner) kingly();

		/* Save memories */
		if (!save_player()) msg_print("death save failed!");

		/* Dump bones file */
		make_bones();

		/* You are dead */
		print_tomb();

		/* Show more info */
		show_info();

		/* Handle score, show Top scores */
		top_twenty();
	}

	/* Still alive */
	else
	{
        is_autosave = FALSE;
		/* Save the game */
		do_cmd_save_game();

		/* Prompt for scores XXX XXX XXX */
		prt("Press Return (or Escape).", 0, 40);

		/* Predict score (or ESCAPE) */
		if (inkey() != ESCAPE) predict_score();
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

	/* Mega-Hack -- Delay death */
	if (p_ptr->chp < 0) death = FALSE;

	/* Hardcode panic save */
	panic_save = 1;

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Indicate panic save */
	(void)strcpy(died_from, "(panic save)");

	/* Panic save, or get worried */
	if (!save_player()) quit("panic save failed!");

	/* Successful panic save */
	quit("panic save succeeded!");
}


errr get_rnd_line(char * file_name, char * output)
{
	FILE        *fp;

	char	buf[1024];
    int lines=0, line, counter;

    /* test hack */
    if (cheat_wzrd && cheat_xtra) msg_print(file_name);

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
	if (death)
	{
		/* Mark the savefile */
		(void)strcpy(died_from, "Abortion");

		/* Close stuff */
		close_game();

		/* Quit */
		quit("interrupt");
	}

	/* Allow suicide (after 5) */
	else if (signal_count >= 5)
	{
		/* Cause of "death" */
		(void)strcpy(died_from, "Interrupting");

		/* Stop playing */
		alive = FALSE;

		/* Suicide */
		death = TRUE;

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
	panic_save = 1;

	/* Panic save */
	(void)strcpy(died_from, "(panic save)");

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




#endif	/* HANDLE_SIGNALS */


