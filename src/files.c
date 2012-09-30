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


/*
 * Hack -- drop permissions
 */
void safe_setuid_drop(void)
{

#ifdef SET_UID

#ifdef SAFE_SETUID

#ifdef HAVE_SETEGID

	if (setegid(getgid()) != 0)
	{
		quit("setegid(): cannot set permissions correctly!");
	}

#else  /* HAVE_SETEGID */

#ifdef SAFE_SETUID_POSIX

	if (setgid(getgid()) != 0)
	{
		quit("setgid(): cannot set permissions correctly!");
	}

#else  /* SAFE_SETUID_POSIX */

	if (setregid(getegid(), getgid()) != 0)
	{
		quit("setregid(): cannot set permissions correctly!");
	}

#endif /* SAFE_SETUID_POSIX */

#endif /* HAVE_SETEGID */

#endif /* SAFE_SETUID */

#endif /* SET_UID */

}


/*
 * Hack -- grab permissions
 */
void safe_setuid_grab(void)
{

#ifdef SET_UID

#ifdef SAFE_SETUID

#ifdef HAVE_SETEGID

	if (setegid(player_egid) != 0)
	{
		quit("setegid(): cannot set permissions correctly!");
	}

#else  /* HAVE_SETEGID */

#ifdef SAFE_SETUID_POSIX

	if (setgid(player_egid) != 0)
	{
		quit("setgid(): cannot set permissions correctly!");
	}

#else  /* SAFE_SETUID_POSIX */

	if (setregid(getegid(), getgid()) != 0)
	{
		quit("setregid(): cannot set permissions correctly!");
	}

#endif /* SAFE_SETUID_POSIX */

#endif /* HAVE_SETEGID */

#endif /* SAFE_SETUID */

#endif /* SET_UID */

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
s16b tokenize(char *buf, s16b num, char **tokens, int mode)
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
			if ((mode & TOKENIZE_CHECKQUOTE) && (*t == '\''))
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


/* A number with a name */
typedef struct named_num named_num;

struct named_num
{
	cptr name;	/* The name of this thing */
	int num;	/* A number associated with it */
};


/* Index of spell type names */
static const named_num gf_desc[] =
{
	{"GF_ELEC", GF_ELEC},
	{"GF_POIS", GF_POIS},
	{"GF_ACID", GF_ACID},
	{"GF_COLD", GF_COLD},
	{"GF_FIRE", GF_FIRE},
	{"GF_MISSILE", GF_MISSILE},
	{"GF_ARROW", GF_ARROW},
	{"GF_PLASMA", GF_PLASMA},
	{"GF_WATER", GF_WATER},
	{"GF_LITE", GF_LITE},
	{"GF_DARK", GF_DARK},
	{"GF_LITE_WEAK", GF_LITE_WEAK},
	{"GF_DARK_WEAK", GF_DARK_WEAK},
	{"GF_SHARDS", GF_SHARDS},
	{"GF_SOUND", GF_SOUND},
	{"GF_CONFUSION", GF_CONFUSION},
	{"GF_FORCE", GF_FORCE},
	{"GF_INERTIA", GF_INERTIA},
	{"GF_MANA", GF_MANA},
	{"GF_METEOR", GF_METEOR},
	{"GF_ICE", GF_ICE},
	{"GF_CHAOS", GF_CHAOS},
	{"GF_NETHER", GF_NETHER},
	{"GF_DISENCHANT", GF_DISENCHANT},
	{"GF_NEXUS", GF_NEXUS},
	{"GF_TIME", GF_TIME},
	{"GF_GRAVITY", GF_GRAVITY},
	{"GF_KILL_WALL", GF_KILL_WALL},
	{"GF_KILL_DOOR", GF_KILL_DOOR},
	{"GF_KILL_TRAP", GF_KILL_TRAP},
	{"GF_MAKE_WALL", GF_MAKE_WALL},
	{"GF_MAKE_DOOR", GF_MAKE_DOOR},
	{"GF_MAKE_TRAP", GF_MAKE_TRAP},
	{"GF_OLD_CLONE", GF_OLD_CLONE},
	{"GF_OLD_POLY", GF_OLD_POLY},
	{"GF_OLD_HEAL", GF_OLD_HEAL},
	{"GF_OLD_SPEED", GF_OLD_SPEED},
	{"GF_OLD_SLOW", GF_OLD_SLOW},
	{"GF_OLD_CONF", GF_OLD_CONF},
	{"GF_OLD_SLEEP", GF_OLD_SLEEP},
	{"GF_OLD_DRAIN", GF_OLD_DRAIN},
	{"GF_NEW_DRAIN", GF_NEW_DRAIN},
	{"GF_AWAY_UNDEAD", GF_AWAY_UNDEAD},
	{"GF_AWAY_EVIL", GF_AWAY_EVIL},
	{"GF_AWAY_ALL", GF_AWAY_ALL},
	{"GF_TURN_UNDEAD", GF_TURN_UNDEAD},
	{"GF_TURN_EVIL", GF_TURN_EVIL},
	{"GF_TURN_ALL", GF_TURN_ALL},
	{"GF_DISP_UNDEAD", GF_DISP_UNDEAD},
	{"GF_DISP_EVIL", GF_DISP_EVIL},
	{"GF_DISP_ALL", GF_DISP_ALL},
	{"GF_DISP_DEMON", GF_DISP_DEMON},
	{"GF_DISP_LIVING", GF_DISP_LIVING},
	{"GF_ROCKET", GF_ROCKET},
	{"GF_NUKE", GF_NUKE},
	{"GF_MAKE_GLYPH", GF_MAKE_GLYPH},
	{"GF_STASIS", GF_STASIS},
	{"GF_STONE_WALL", GF_STONE_WALL},
	{"GF_DEATH_RAY", GF_DEATH_RAY},
	{"GF_STUN", GF_STUN},
	{"GF_HOLY_FIRE", GF_HOLY_FIRE},
	{"GF_HELL_FIRE", GF_HELL_FIRE},
	{"GF_DISINTEGRATE", GF_DISINTEGRATE},
	{"GF_CHARM", GF_CHARM},
	{"GF_CONTROL_UNDEAD", GF_CONTROL_UNDEAD},
	{"GF_CONTROL_ANIMAL", GF_CONTROL_ANIMAL},
	{"GF_PSI", GF_PSI},
	{"GF_PSI_DRAIN", GF_PSI_DRAIN},
	{"GF_TELEKINESIS", GF_TELEKINESIS},
	{"GF_JAM_DOOR", GF_JAM_DOOR},
	{"GF_DOMINATION", GF_DOMINATION},
	{"GF_DISP_GOOD", GF_DISP_GOOD},
	{NULL, 0}
};


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
 * Parse another file recursively, see below for details.
 *   %:<filename>
 *
 * Specify the attr/char values for "monsters" by race index.
 *   R:<num>:<a>:<c>
 *
 * Specify the attr/char values for "objects" by kind index.
 *   K:<num>:<a>:<c>
 *
 * Specify the attr/char values for "features" by feature index.
 *   F:<num>:<a>:<c>
 *
 * Specify the attr/char values for "fields" by field index.
 *   T:<num>:<a>:<c>
 *
 * Specify the attr/char values for unaware "objects" by kind tval.
 *   U:<tv>:<a>:<c>
 *
 * Specify the attr/char values for inventory "objects" by kind tval.
 *   E:<tv>:<a>:<c>
 *
 * Define a macro action, given an encoded macro action.
 *   A:<str>
 *
 * Create a normal macro, given an encoded macro trigger.
 *   P:<str>
 *
 * Create a command macro, given an encoded macro trigger.
 *   C:<str>
 *
 * Create a keyset mapping.
 *   S:<key>:<key>:<dir>
 *
 * Turn an option off, given its name.
 *   X:<str>
 *
 * Turn an option on, given its name.
 *   Y:<str>
 *
 * Specify visual information, given an index, and some data.
 *   V:<num>:<kv>:<rv>:<gv>:<bv>
 *
 * Specify colors for message-types.
 *   M:<type>:<attr>
 *
 * Specify the set of colors to use when drawing a zapped spell.
 *   Z:<type>:<str>
 */
errr process_pref_file_command(char *buf)
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
		if (tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			monster_race *r_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= z_info->r_max)) return (1);
			r_ptr = &r_info[i];
			if (n1) r_ptr->x_attr = n1;
			if (n2) r_ptr->x_char = n2;
			return (0);
		}
	}

	/* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
	else if (buf[0] == 'K')
	{
		if (tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			object_kind *k_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= z_info->k_max)) return (1);
			k_ptr = &k_info[i];
			if (n1) k_ptr->x_attr = n1;
			if (n2) k_ptr->x_char = n2;
			return (0);
		}
	}

	/* Process "T:<num>:<a>/<c>" -- attr/char for fields */
	else if (buf[0] == 'T')
	{
		if (tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			field_thaum *t_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= z_info->t_max)) return (1);
			t_ptr = &t_info[i];
			if (n1) t_ptr->f_attr = n1;
			if (n2) t_ptr->f_char = n2;
			return (0);
		}
	}

	/* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
	else if (buf[0] == 'F')
	{
		if (tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			feature_type *f_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= z_info->f_max)) return (1);
			f_ptr = &f_info[i];
			if (n1) f_ptr->x_attr = n1;
			if (n2) f_ptr->x_char = n2;
			return (0);
		}
	}

	/* Process "W:<num>:<a>/<c>" -- xtra attr/char for terrain features */
	else if (buf[0] == 'W')
	{
		if (tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			feature_type *f_ptr;
			i = (huge) strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((i < 0) || (i >= z_info->f_max)) return (1);
			f_ptr = &f_info[i];
			if (n1) f_ptr->w_attr = n1;
			if (n2) f_ptr->w_char = n2;
			return (0);
		}
	}

	/* Process "S:<num>:<a>/<c>" -- attr/char for special things */
	else if (buf[0] == 'S')
	{
		if (tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			j = (byte)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if ((j < 0) || (j >= 256)) return (1);
			misc_to_attr[j] = n1;
			misc_to_char[j] = n2;
			return (0);
		}
	}

	/* Process "U:<tv>:<a>/<c>" -- attr/char for unaware items */
	else if (buf[0] == 'U')
	{
		if (tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			j = (huge) strtol(zz[0], NULL, 0);
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
		if (tokenize(buf + 2, 2, zz, TOKENIZE_CHECKQUOTE) == 2)
		{
			j = (byte)strtol(zz[0], NULL, 0) % 128;
			n1 = strtol(zz[1], NULL, 0);
			if ((j < 0) || (j >= 128)) return (1);
			if (n1) tval_to_attr[j] = n1;
			return (0);
		}
	}


	/* Process "A:<str>" -- save an "action" for later */
	else if (buf[0] == 'A')
	{
		text_to_ascii(macro__buf, buf + 2);
		return (0);
	}

	/* Process "P:<str>" -- normal macro */
	else if (buf[0] == 'P')
	{
		char tmp[1024];
		text_to_ascii(tmp, buf + 2);
		macro_add(tmp, macro__buf);
		return (0);
	}


	/* Process "C:<str>" -- create keymap */
	else if (buf[0] == 'C')
	{
		int mode;

		char tmp[1024];

		if (tokenize(buf + 2, 2, zz, TOKENIZE_CHECKQUOTE) != 2) return (1);

		mode = strtol(zz[0], NULL, 0);
		if ((mode < 0) || (mode >= KEYMAP_MODES)) return (1);

		text_to_ascii(tmp, zz[1]);
		if (!tmp[0] || tmp[1]) return (1);
		i = (byte)(tmp[0]);

		string_free(keymap_act[mode][i]);

		keymap_act[mode][i] = string_make(macro__buf);

		return (0);
	}


	/* Process "V:<num>:<kv>:<rv>:<gv>:<bv>" -- visual info */
	else if (buf[0] == 'V')
	{
		if (tokenize(buf + 2, 5, zz, TOKENIZE_CHECKQUOTE) == 5)
		{
			i = (byte)strtol(zz[0], NULL, 0);
			if ((i < 0) || (i >= 256)) return (1);
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
		for (i = 0; i < OPT_MAX; i++)
		{
			if (option_info[i].o_desc &&
				option_info[i].o_text && streq(option_info[i].o_text, buf + 2))
			{
				/* Clear */
				option_info[i].o_val = FALSE;

				/* Save the change */
				if (character_generated)
				{
					init_options(OPT_FLAG_SERVER | OPT_FLAG_PLAYER);
				}
				else
				{
					init_options(OPT_FLAG_BIRTH | OPT_FLAG_SERVER |
								 OPT_FLAG_PLAYER);
				}

				return (0);
			}
		}
		/* XXX XXX XXX - ignore unknown options */
		return (0);
	}

	/* Process "Y:<str>" -- turn option on */
	else if (buf[0] == 'Y')
	{
		for (i = 0; i < OPT_MAX; i++)
		{
			if (option_info[i].o_desc &&
				option_info[i].o_text && streq(option_info[i].o_text, buf + 2))
			{
				/* Set */
				option_info[i].o_val = TRUE;

				/* Save the change */
				if (character_generated)
				{
					init_options(OPT_FLAG_SERVER | OPT_FLAG_PLAYER);
				}
				else
				{
					init_options(OPT_FLAG_BIRTH | OPT_FLAG_SERVER |
								 OPT_FLAG_PLAYER);
				}

				return (0);
			}
		}

		/* XXX XXX XXX - ignore unknown options */
		return (0);
	}

	/* Process "Z:<type>:<str>" -- set spell color */
	else if (buf[0] == 'Z')
	{
		/* Find the colon */
		char *t = strchr(buf + 2, ':');

		/* Oops */
		if (!t) return (1);

		/* Nuke the colon */
		*(t++) = '\0';

		for (i = 0; gf_desc[i].name; i++)
		{
			/* Match this type */
			if (streq(gf_desc[i].name, buf + 2))
			{
				/* Wipe old strings */
				if (gf_color[gf_desc[i].num])
				{
					string_free(gf_color[gf_desc[i].num]);
				}

				/* Remember this color set */
				gf_color[gf_desc[i].num] = string_make(t);

				/* Success */
				return (0);
			}
		}
	}

	/* Process "M:<type>:<attr>" -- colors for message-types */
	else if (buf[0] == 'M')
	{
		if (tokenize(buf + 2, 2, zz, TOKENIZE_CHECKQUOTE) == 2)
		{
			u16b type = (u16b)strtol(zz[0], NULL, 0);
			int color = color_char_to_attr(zz[1][0]);

			/* Ignore illegal color */
			if (color < 0) return (1);

			/* Store the color */
			return (message_color_define(type, (byte)color));
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
			if (streq(b + 1, "SYS"))
			{
				v = ANGBAND_SYS;
			}

			/* Graphics */
			else if (streq(b + 1, "GRAF"))
			{
				switch (use_graphics)
				{
					case GRAPHICS_NONE: v = "none";
						break;
					case GRAPHICS_ORIGINAL: v = "old";
						break;
					case GRAPHICS_ADAM_BOLT: v = "new";
						break;
					case GRAPHICS_ANY: v = "error";
						break;
					case GRAPHICS_HALF_3D: v = "none";
						break;
				}
			}

			/* Monochrome mode */
			else if (streq(b + 1, "MONOCHROME"))
			{
				if (arg_monochrome)
					v = "ON";
				else
					v = "OFF";
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
				v = player_base;
			}

			/* Town */
			else if (streq(b + 1, "TOWN"))
			{
				v = vanilla_town ? "VANILLA" : "WILDERNESS";
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
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Count lines */
		line++;


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
errr process_pref_file(cptr name)
{
	char buf[1024];

	errr err = 0;


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_PREF, name);

	/* Process the pref file */
	err = process_pref_file_aux(buf);

	/* Stop at parser errors, but not at non-existing file */
	if (err < 1)
	{
		/* Build the filename */
		path_build(buf, 1024, ANGBAND_DIR_USER, name);

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
	c = time((time_t *) 0);
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

	FILE *fp;

	char buf[1024];

	char temphost[MAXHOSTNAMELEN + 1];
	char thishost[MAXHOSTNAMELEN + 1];


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
 * Print number with header at given row, column
 */
static void prt_num(cptr header, s32b num, int col, int row, byte color,
                    int wid)
{
	int len = strlen(header);
	char out_val[32];
	put_str(header, col, row);
	put_str("   ", col + len, row);
	(void)sprintf(out_val, "%*ld", wid, (long)num);
	c_put_str(color, out_val, col + len + 3, row);
}


#define COL_SKILLS1		0
#define COL_SKILLS2		29
#define COL_SKILLS3		58
#define WID_SKILLS		14


/*
 * Hack -- pass color info around this file
 */
static byte likert_color = TERM_WHITE;


/*
 * Returns a "rating" of x depending on y
 */
static void likert(int x, int y, char *desc)
{
	/* Make the string empty */
	desc[0] = 0;

	/* Paranoia */
	if (y <= 0) y = 1;

	/* Negative value */
	if (x < 0)
	{
		likert_color = TERM_L_DARK;
		strcpy(desc, "Very Bad");
		return;
	}

	/* Analyze the value */
	switch ((x / y))
	{
		case 0:
		case 1:
		{
			likert_color = TERM_RED;
			strcpy(desc, "Bad");
			return;
		}
		case 2:
		{
			likert_color = TERM_L_RED;
			strcpy(desc, "Poor");
			return;
		}
		case 3:
		case 4:
		{
			likert_color = TERM_ORANGE;
			strcpy(desc, "Fair");
			return;
		}
		case 5:
		{
			likert_color = TERM_YELLOW;
			strcpy(desc, "Good");
			return;
		}
		case 6:
		{
			likert_color = TERM_YELLOW;
			strcpy(desc, "Very Good");
			return;
		}
		case 7:
		case 8:
		{
			likert_color = TERM_L_GREEN;
			strcpy(desc, "Excellent");
			return;
		}
		case 9:
		case 10:
		case 11:
		case 12:
		case 13:
		{
			likert_color = TERM_GREEN;
			strcpy(desc, "Superb");
			return;
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			likert_color = TERM_BLUE;
			strcpy(desc, "Chaos Rank");
			return;
		}
		default:
		{
			likert_color = TERM_VIOLET;
			sprintf(desc, "Amber [%d]", (int)((((x / y) - 17) * 5) / 2));
			return;
		}
	}
}

/* Monk average attack damage - only used here, so not in tables.c */
static int monk_avg_damage[PY_MAX_LEVEL + 1] =
{
	0,
	250, 275, 299, 299, 306, 309, 321, 325, 328, 332,
	347, 353, 375, 450, 463, 507, 523, 537, 551, 575,
	680, 704, 723, 738, 768, 792, 812, 925, 1008, 1032,
	1061, 1074, 1160, 1178, 1303, 1326, 1400, 1435, 1476, 1500,
	1669, 1809, 1836, 1875, 2155, 2190, 2227, 2587, 2769, 2811
};


/*
 * Prints ratings on certain abilities
 *
 * This code is "imitated" elsewhere to "dump" a character sheet.
 */
static void display_player_abilities(void)
{
	int tmp, damdice, damsides, dambonus, blows;
	int xthn, xthb, xfos, xsns;
	int xdis, xdev, xsav, xstl;
	char desc[20];
	int muta_att = 0;
	long avgdam;
	u32b f1, f2, f3;
	int energy_fire;
	int shots, shot_frac;

	object_type *o_ptr;

	if (p_ptr->muta2 & MUT2_HORNS) muta_att++;
	if (p_ptr->muta2 & MUT2_SCOR_TAIL) muta_att++;
	if (p_ptr->muta2 & MUT2_BEAK) muta_att++;
	if (p_ptr->muta2 & MUT2_TRUNK) muta_att++;
	if (p_ptr->muta2 & MUT2_TENTACLES) muta_att++;

	/* Fighting Skill (with current weapon) */
	o_ptr = &inventory[INVEN_WIELD];
	tmp = p_ptr->to_h + o_ptr->to_h;
	xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

	/* Shooting Skill (with current bow and normal missile) */
	o_ptr = &inventory[INVEN_BOW];
	tmp = p_ptr->to_h + o_ptr->to_h;
	xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

	/* Is the player is wielding a shooter? */
	if (o_ptr->k_idx)
	{
		energy_fire = p_ptr->bow_energy;
	}
	else
	{
		energy_fire = 100;
	}

	/* Calculate shots per round  - note "strange" formula. */

	/* The real number of shots per round is (1 + n)/2 */
	shots = (1 + p_ptr->num_fire) * 50;
	shot_frac = (shots * 100 / energy_fire) % 100;
	shots = shots / energy_fire;

	/* Average damage per round */
	o_ptr = &inventory[INVEN_WIELD];
	dambonus = p_ptr->dis_to_d;
	if (object_known_p(o_ptr)) dambonus += o_ptr->to_d;
	damdice = o_ptr->dd;
	damsides = o_ptr->ds;
	blows = p_ptr->num_blow;


	/* Basic abilities */

	xdis = p_ptr->skill_dis;
	xdev = p_ptr->skill_dev;
	xsav = p_ptr->skill_sav;
	xstl = p_ptr->skill_stl;
	xsns = p_ptr->skill_sns;
	xfos = p_ptr->skill_fos;


	put_str("Fighting    :", COL_SKILLS1, 16);
	likert(xthn, 10, desc);
	c_put_str(likert_color, desc, COL_SKILLS1 + WID_SKILLS, 16);

	put_str("Bows/Throw  :", COL_SKILLS1, 17);
	likert(xthb, 10, desc);
	c_put_str(likert_color, desc, COL_SKILLS1 + WID_SKILLS, 17);

	put_str("Saving Throw:", COL_SKILLS1, 18);
	likert(xsav, 6, desc);
	c_put_str(likert_color, desc, COL_SKILLS1 + WID_SKILLS, 18);

	put_str("Stealth     :", COL_SKILLS1, 19);
	likert(xstl, 1, desc);
	c_put_str(likert_color, desc, COL_SKILLS1 + WID_SKILLS, 19);


	put_str("Perception  :", COL_SKILLS2, 16);
	likert(xfos, 6, desc);
	c_put_str(likert_color, desc, COL_SKILLS2 + WID_SKILLS, 16);

	put_str("Sensing     :", COL_SKILLS2, 17);
	likert(xsns, 6, desc);
	c_put_str(likert_color, desc, COL_SKILLS2 + WID_SKILLS, 17);

	put_str("Disarming   :", COL_SKILLS2, 18);
	likert(xdis, 8, desc);
	c_put_str(likert_color, desc, COL_SKILLS2 + WID_SKILLS, 18);

	put_str("Magic Device:", COL_SKILLS2, 19);
	likert(xdev, 6, desc);
	c_put_str(likert_color, desc, COL_SKILLS2 + WID_SKILLS, 19);


	put_str("Blows/Round :", COL_SKILLS3, 16);

	if (!muta_att)
		put_str(format("%d", p_ptr->num_blow), COL_SKILLS3 + WID_SKILLS, 16);
	else
		put_str(format("%d+%d", p_ptr->num_blow, muta_att),
				COL_SKILLS3 + WID_SKILLS, 16);

	put_str("Shots/Round :", COL_SKILLS3, 17);

	/* Calculate shots (rounded) */
	put_str(format("%d.%d", shots, shot_frac), COL_SKILLS3 + WID_SKILLS, 17);

	put_str("Avg.Dam./Rnd:", COL_SKILLS3, 18);

	/* Effect of damage dice x2 */
	avgdam = avg_dam(dambonus, damdice, damsides);

	/* number of blows */
	avgdam *= blows;

	/* Rescale */
	avgdam /= 200;

	/* See if have a weapon with extra power */
	if (o_ptr->k_idx)
	{
		/* Is there a vorpal effect we know about? */
		object_flags(o_ptr, &f1, &f2, &f3);
		if ((o_ptr->ident & IDENT_MENTAL) &&
			(o_ptr->activate - 128 == ART_VORPAL_BLADE))
		{
			/* vorpal blade */
			avgdam *= 786;
			avgdam /= 500;
		}
		else if (object_known_p(o_ptr) && (f1 & TR1_VORPAL))
		{
			/* vorpal flag only */
			avgdam *= 609;
			avgdam /= 500;
		}
	}

	/* normal players get two 1d1 punches */
	if (!o_ptr->k_idx && (p_ptr->pclass != CLASS_MONK)) avgdam = 2;

	if (avgdam == 0)
	{
		if ((p_ptr->pclass == CLASS_MONK) && (!o_ptr->k_idx))
			sprintf(desc, "%d", monk_avg_damage[p_ptr->lev] * blows / 100);
		else
			strcpy(desc, "nil!");
	}
	else
	{
		sprintf(desc, "%d", (int)avgdam);
	}

	put_str(desc, COL_SKILLS3 + WID_SKILLS, 18);

	put_str("Infra-Vision:", COL_SKILLS3, 19);
	put_str(format("%d'", p_ptr->see_infra * 10), COL_SKILLS3 + WID_SKILLS, 19);
}



/*
 * Obtain the "flags" for the player as if he was an item
 */
static void player_flags(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Classes */
	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR:
			if (p_ptr->lev > 29)
				(*f2) |= (TR2_RES_FEAR);
			break;
		case CLASS_PALADIN:
			if (p_ptr->lev > 39)
				(*f2) |= (TR2_RES_FEAR);
			break;
		case CLASS_CHAOS_WARRIOR:
			if (p_ptr->lev > 29)
				(*f2) |= (TR2_RES_CHAOS);
			if (p_ptr->lev > 39)
				(*f2) |= (TR2_RES_FEAR);
			break;
		case CLASS_MONK:
			/* Monks get extra abilities if unencumbered */
			if (!p_ptr->monk_armour_stat)
			{
				if (p_ptr->lev > 9)
					(*f1) |= TR1_SPEED;
				if (p_ptr->lev > 24)
					(*f2) |= (TR2_FREE_ACT);
			}
			break;
		case CLASS_MINDCRAFTER:
			if (p_ptr->lev > 9)
				(*f2) |= (TR2_RES_FEAR);
			if (p_ptr->lev > 19)
				(*f2) |= (TR2_SUST_WIS);
			if (p_ptr->lev > 29)
				(*f2) |= (TR2_RES_CONF);
			if (p_ptr->lev > 39)
				(*f3) |= (TR3_TELEPATHY);
			break;
		default:
			;					/* Do nothing */
	}

	/* Races */
	switch (p_ptr->prace)
	{
		case RACE_ELF:
			(*f2) |= (TR2_RES_LITE);
			break;
		case RACE_HOBBIT:
			(*f2) |= (TR2_SUST_DEX);
			break;
		case RACE_GNOME:
			(*f2) |= (TR2_FREE_ACT);
			break;
		case RACE_DWARF:
			(*f2) |= (TR2_RES_BLIND);
			break;
		case RACE_HALF_ORC:
			(*f2) |= (TR2_RES_DARK);
			break;
		case RACE_HALF_TROLL:
			(*f2) |= (TR2_SUST_STR);
			if (p_ptr->lev > 14)
			{
				(*f3) |= (TR3_REGEN);
				if (p_ptr->pclass == CLASS_WARRIOR)
				{
					(*f3) |= (TR3_SLOW_DIGEST);
					/*
					 * Let's not make Regeneration a disadvantage
					 * for the poor warriors who can never learn
					 * a spell that satisfies hunger (actually
					 * neither can rogues, but half-trolls are not
					 * supposed to play rogues)
					 */
				}
			}
			break;
		case RACE_AMBERITE:
			(*f2) |= (TR2_SUST_CON);
			(*f3) |= (TR3_REGEN);	/* Amberites heal fast */
			break;
		case RACE_HIGH_ELF:
			(*f2) |= (TR2_RES_LITE);
			(*f3) |= (TR3_SEE_INVIS);
			break;
		case RACE_BARBARIAN:
			(*f2) |= (TR2_RES_FEAR);
			break;
		case RACE_HALF_OGRE:
			(*f2) |= (TR2_SUST_STR);
			(*f2) |= (TR2_RES_DARK);
			break;
		case RACE_HALF_GIANT:
			(*f2) |= (TR2_RES_SHARDS);
			(*f2) |= (TR2_SUST_STR);
			break;
		case RACE_HALF_TITAN:
			(*f2) |= (TR2_RES_CHAOS);
			break;
		case RACE_CYCLOPS:
			(*f2) |= (TR2_RES_SOUND);
			break;
		case RACE_YEEK:
			(*f2) |= (TR2_RES_ACID);
			if (p_ptr->lev > 19)
				(*f2) |= (TR2_IM_ACID);
			break;
		case RACE_KLACKON:
			(*f2) |= (TR2_RES_CONF);
			(*f2) |= (TR2_RES_ACID);
			if (p_ptr->lev > 9)
				(*f1) |= TR1_SPEED;
			break;
		case RACE_KOBOLD:
			(*f2) |= (TR2_RES_POIS);
			break;
		case RACE_NIBELUNG:
			(*f2) |= (TR2_RES_DISEN);
			(*f2) |= (TR2_RES_DARK);
			break;
		case RACE_DARK_ELF:
			(*f2) |= (TR2_RES_DARK);
			if (p_ptr->lev > 19)
				(*f3) |= (TR3_SEE_INVIS);
			break;
		case RACE_DRACONIAN:
			(*f3) |= TR3_FEATHER;
			if (p_ptr->lev > 4)
				(*f2) |= (TR2_RES_FIRE);
			if (p_ptr->lev > 9)
				(*f2) |= (TR2_RES_COLD);
			if (p_ptr->lev > 14)
				(*f2) |= (TR2_RES_ACID);
			if (p_ptr->lev > 19)
				(*f2) |= (TR2_RES_ELEC);
			if (p_ptr->lev > 34)
				(*f2) |= (TR2_RES_POIS);
			break;
		case RACE_MIND_FLAYER:
			(*f2) |= (TR2_SUST_INT);
			(*f2) |= (TR2_SUST_WIS);
			if (p_ptr->lev > 14)
				(*f3) |= (TR3_SEE_INVIS);
			if (p_ptr->lev > 29)
				(*f3) |= (TR3_TELEPATHY);
			break;
		case RACE_IMP:
			(*f2) |= (TR2_RES_FIRE);
			if (p_ptr->lev > 9)
				(*f3) |= (TR3_SEE_INVIS);
			break;
		case RACE_GOLEM:
			(*f3) |= (TR3_SEE_INVIS);
			(*f2) |= (TR2_FREE_ACT);
			(*f2) |= (TR2_RES_POIS);
			(*f3) |= (TR3_SLOW_DIGEST);
			if (p_ptr->lev > 34)
				(*f2) |= (TR2_HOLD_LIFE);
			break;
		case RACE_SKELETON:
			(*f3) |= (TR3_SEE_INVIS);
			(*f2) |= (TR2_RES_SHARDS);
			(*f2) |= (TR2_HOLD_LIFE);
			(*f2) |= (TR2_RES_POIS);
			if (p_ptr->lev > 9)
				(*f2) |= (TR2_RES_COLD);
			break;
		case RACE_ZOMBIE:
			(*f3) |= (TR3_SEE_INVIS);
			(*f2) |= (TR2_HOLD_LIFE);
			(*f2) |= (TR2_RES_NETHER);
			(*f2) |= (TR2_RES_POIS);
			(*f3) |= (TR3_SLOW_DIGEST);
			if (p_ptr->lev > 4)
				(*f2) |= (TR2_RES_COLD);
			break;
		case RACE_VAMPIRE:
			(*f2) |= (TR2_HOLD_LIFE);
			(*f2) |= (TR2_RES_DARK);
			(*f2) |= (TR2_RES_NETHER);
			(*f3) |= (TR3_LITE);
			(*f2) |= (TR2_RES_POIS);
			(*f2) |= (TR2_RES_COLD);
			break;
		case RACE_SPECTRE:
			(*f2) |= (TR2_RES_COLD);
			(*f3) |= (TR3_SEE_INVIS);
			(*f2) |= (TR2_HOLD_LIFE);
			(*f2) |= (TR2_RES_NETHER);
			(*f2) |= (TR2_RES_POIS);
			(*f3) |= (TR3_SLOW_DIGEST);
			/* XXX pass_wall */
			if (p_ptr->lev > 34)
				(*f3) |= TR3_TELEPATHY;
			break;
		case RACE_SPRITE:
			(*f2) |= (TR2_RES_LITE);
			(*f3) |= (TR3_FEATHER);
			if (p_ptr->lev > 9)
				(*f1) |= (TR1_SPEED);
			break;
		case RACE_BEASTMAN:
			(*f2) |= (TR2_RES_SOUND);
			(*f2) |= (TR2_RES_CONF);
			break;
		case RACE_GHOUL:
			(*f2) |= (TR2_HOLD_LIFE);
			if (p_ptr->lev > 9) (*f2) |= (TR2_RES_DARK);
			if (p_ptr->lev > 19) (*f2) |= (TR2_RES_NETHER);
			(*f2) |= (TR2_RES_POIS);
			(*f2) |= (TR2_RES_COLD);
			break;
		default:
			;					/* Do nothing */
	}

	/* Mutations */
	if (p_ptr->muta3)
	{
		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
			(*f3) &= ~(TR3_REGEN);
		}

		if ((p_ptr->muta3 & MUT3_XTRA_FAT) ||
			(p_ptr->muta3 & MUT3_XTRA_LEGS) || (p_ptr->muta3 & MUT3_SHORT_LEG))
		{
			(*f1) |= TR1_SPEED;
		}

		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
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

		if (p_ptr->muta3 & MUT3_MOTION)
		{
			(*f2) |= TR2_FREE_ACT;
		}
	}

	/* Remove flags that were not in Moria */
	if (ironman_moria)
	{
		(*f1) &= TR1_MORIA_MASK;
		(*f2) &= TR2_MORIA_MASK;
		(*f3) &= TR3_MORIA_MASK;
	}
}


/*
 * Equippy chars
 */
static void display_player_equippy(int x, int y)
{
	int i;

	byte a;
	char c;

	object_type *o_ptr;


	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Object */
		o_ptr = &inventory[i];

		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* No color */
		if (!use_color || ironman_moria) a = TERM_WHITE;

		/* Clear the part of the screen */
		if (!o_ptr->k_idx)
		{
			c = ' ';
			a = TERM_DARK;
		}

		/* Dump */
		Term_putch(x + i - INVEN_WIELD, y, a, c);
	}
}


void print_equippy(void)
{
	display_player_equippy(COL_EQUIPPY, ROW_EQUIPPY);
}


/*
 * Helper function, see below
 */
static void display_player_flag_aux(int col, int row,
                                    char *header, int n, u32b flag1, u32b flag2)
{
	int i;
	u32b f[3];


	/* Header */
	c_put_str(TERM_WHITE, header, col, row);

	/* Advance */
	col += strlen(header) + 1;


	/* Check equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr;

		/* Object */
		o_ptr = &inventory[i];

		/* Known flags */
		object_flags_known(o_ptr, &f[0], &f[1], &f[2]);

		/* Default */
		c_put_str(TERM_SLATE, ".", col, row);

		/* Check flags */
		if (f[n - 1] & flag1) c_put_str(TERM_WHITE, "+", col, row);
		if (f[n - 1] & flag2) c_put_str(TERM_WHITE, "*", col, row);

		/* Advance */
		col++;
	}

	/* Player flags */
	player_flags(&f[0], &f[1], &f[2]);

	/* Default */
	c_put_str(TERM_SLATE, ".", col, row);

	/* Check flags */
	if (f[n - 1] & flag1) c_put_str(TERM_WHITE, "+", col, row);
	if (f[n - 1] & flag2) c_put_str(TERM_WHITE, "*", col, row);
}


/*
 * Special display, part 1
 */
static void display_player_flag_info(void)
{
	int row;
	int col;


	/*** Set 1 ***/

	row = 5;
	col = 0;

	display_player_equippy(col + 8, row++);

	c_put_str(TERM_WHITE, "abcdefghijkl@", col + 8, row++);

	display_player_flag_aux(col, row++, "Acid  :", 2, TR2_RES_ACID,
							TR2_IM_ACID);
	display_player_flag_aux(col, row++, "Elec  :", 2, TR2_RES_ELEC,
							TR2_IM_ELEC);
	display_player_flag_aux(col, row++, "Fire  :", 2, TR2_RES_FIRE,
							TR2_IM_FIRE);
	display_player_flag_aux(col, row++, "Cold  :", 2, TR2_RES_COLD,
							TR2_IM_COLD);
	display_player_flag_aux(col, row++, "Poison:", 2, TR2_RES_POIS, 0);
	display_player_flag_aux(col, row++, "Fear  :", 2, TR2_RES_FEAR, 0);
	display_player_flag_aux(col, row++, "Light :", 2, TR2_RES_LITE, 0);
	display_player_flag_aux(col, row++, "Dark  :", 2, TR2_RES_DARK, 0);
	display_player_flag_aux(col, row++, "Shard :", 2, TR2_RES_SHARDS, 0);
	display_player_flag_aux(col, row++, "Blind :", 2, TR2_RES_BLIND, 0);
	display_player_flag_aux(col, row++, "Conf  :", 2, TR2_RES_CONF, 0);
	display_player_flag_aux(col, row++, "Sound :", 2, TR2_RES_SOUND, 0);
	display_player_flag_aux(col, row++, "Nether:", 2, TR2_RES_NETHER, 0);
	display_player_flag_aux(col, row++, "Nexus :", 2, TR2_RES_NEXUS, 0);
	display_player_flag_aux(col, row++, "Chaos :", 2, TR2_RES_CHAOS, 0);
	display_player_flag_aux(col, row++, "Disnch:", 2, TR2_RES_DISEN, 0);


	/*** Set 2 ***/

	row = 11;
	col = 25;

	display_player_equippy(col + 10, row++);

	c_put_str(TERM_WHITE, "abcdefghijkl@", col + 10, row++);

	display_player_flag_aux(col, row++, "Speed   :", 1, TR1_SPEED, 0);
	display_player_flag_aux(col, row++, "Reflect :", 2, TR2_REFLECT, 0);
	display_player_flag_aux(col, row++, "AuraFire:", 3, TR3_SH_FIRE, 0);
	display_player_flag_aux(col, row++, "AuraElec:", 3, TR3_SH_ELEC, 0);
	display_player_flag_aux(col, row++, "NoTelprt:", 3, TR3_NO_TELE, 0);
	display_player_flag_aux(col, row++, "No Magic:", 3, TR3_NO_MAGIC, 0);
	display_player_flag_aux(col, row++, "Cursed  :", 3, TR3_HEAVY_CURSE,
							TR3_PERMA_CURSE);
	display_player_flag_aux(col, row++, "DrainExp:", 3, TR3_DRAIN_EXP, 0);
	display_player_flag_aux(col, row++, "Teleport:", 3, TR3_TELEPORT, 0);


	/*** Set 3 ***/

	row = 11;
	col = 52;

	display_player_equippy(col + 11, row++);

	c_put_str(TERM_WHITE, "abcdefghijkl@", col + 11, row++);

	display_player_flag_aux(col, row++, "Free Actn:", 2, TR2_FREE_ACT, 0);
	display_player_flag_aux(col, row++, "SeeInvis.:", 3, TR3_SEE_INVIS, 0);
	display_player_flag_aux(col, row++, "Hold Life:", 2, TR2_HOLD_LIFE, 0);
	display_player_flag_aux(col, row++, "Telepathy:", 3, TR3_TELEPATHY, 0);
	display_player_flag_aux(col, row++, "SlwDigstn:", 3, TR3_SLOW_DIGEST, 0);
	display_player_flag_aux(col, row++, "Regen.   :", 3, TR3_REGEN, 0);
	display_player_flag_aux(col, row++, "Levitate :", 3, TR3_FEATHER, 0);
	display_player_flag_aux(col, row++, "PermLite :", 3, TR3_LITE, 0);
	display_player_flag_aux(col, row++, "Aggravate:", 3, TR3_AGGRAVATE, 0);
	display_player_flag_aux(col, row++, "EvilCurse:", 3, TR3_TY_CURSE, 0);
}

#if 0
/*
 * Special display, part 2a
 */
static void display_player_misc_info(void)
{
	char buf[80];

	/* Display basics */
	put_str("Name      :", 1, 2);
	put_str("Sex       :", 1, 3);
	put_str("Race      :", 1, 4);
	put_str("Class     :", 1, 5);

	c_put_str(TERM_L_BLUE, player_name, 13, 2);
	c_put_str(TERM_L_BLUE, sp_ptr->title, 13, 3);
	c_put_str(TERM_L_BLUE, rp_ptr->title, 13, 4);
	c_put_str(TERM_L_BLUE, cp_ptr->title, 13, 5);

	/* Display extras */
	put_str("Level     :", 1, 6);
	put_str("Hits(Max) :", 1, 7);
	put_str("Mana(Max) :", 1, 8);

	(void)sprintf(buf, "%d", (int)p_ptr->lev);
	c_put_str(TERM_L_BLUE, buf, 13, 6);
	(void)sprintf(buf, "%d(%d)", (int)p_ptr->chp, (int)p_ptr->mhp);
	c_put_str(TERM_L_BLUE, buf, 13, 7);
	(void)sprintf(buf, "%d(%d)", (int)p_ptr->csp, (int)p_ptr->msp);
	c_put_str(TERM_L_BLUE, buf, 13, 8);
}

#endif

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
	c_put_str(TERM_WHITE, "Stat", stat_col, row - 1);
	c_put_str(TERM_BLUE, "Intrnl", stat_col + 5, row - 1);
	c_put_str(TERM_L_BLUE, "Rce Cls Mod", stat_col + 12, row - 1);
	c_put_str(TERM_L_GREEN, "Actual", stat_col + 24, row - 1);
	c_put_str(TERM_YELLOW, "Currnt", stat_col + 31, row - 1);

	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Calculate equipment adjustment */
		e_adj = 0;

		/* Icky formula to deal with the 18 barrier */
		if ((p_ptr->stat_max[i] > 18) && (p_ptr->stat_top[i] > 18))
			e_adj = (p_ptr->stat_top[i] - p_ptr->stat_max[i]) / 10;
		if ((p_ptr->stat_max[i] <= 18) && (p_ptr->stat_top[i] <= 18))
			e_adj = p_ptr->stat_top[i] - p_ptr->stat_max[i];
		if ((p_ptr->stat_max[i] <= 18) && (p_ptr->stat_top[i] > 18))
			e_adj = (p_ptr->stat_top[i] - 18) / 10 - p_ptr->stat_max[i] + 18;

		if ((p_ptr->stat_max[i] > 18) && (p_ptr->stat_top[i] <= 18))
			e_adj = p_ptr->stat_top[i] - (p_ptr->stat_max[i] - 18) / 10 - 19;

		/* Deduct class and race bonuses */
		e_adj -= rp_ptr->r_adj[i];
		e_adj -= cp_ptr->c_adj[i];

		/* Reduced name of stat */
		c_put_str(TERM_WHITE, stat_names_reduced[i], stat_col, row + i);

		/* Internal "natural" max value.  Maxes at 18/100 */
		/* This is useful to see if you are maxed out */
		cnv_stat(p_ptr->stat_max[i], buf);
		c_put_str(TERM_BLUE, buf, stat_col + 5, row + i);

		/* Race, class, and equipment modifiers */
		(void)sprintf(buf, "%3d", (int)rp_ptr->r_adj[i]);
		c_put_str(TERM_L_BLUE, buf, stat_col + 12, row + i);
		(void)sprintf(buf, "%3d", (int)cp_ptr->c_adj[i]);
		c_put_str(TERM_L_BLUE, buf, stat_col + 16, row + i);
		(void)sprintf(buf, "%3d", (int)e_adj);
		c_put_str(TERM_L_BLUE, buf, stat_col + 20, row + i);

		/* Actual maximal modified value */
		cnv_stat(p_ptr->stat_top[i], buf);
		c_put_str(TERM_L_GREEN, buf, stat_col + 24, row + i);

		/* Only display stat_use if not maximal */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			cnv_stat(p_ptr->stat_use[i], buf);
			c_put_str(TERM_YELLOW, buf, stat_col + 31, row + i);
		}
	}

	/* Column */
	col = stat_col + 39;

	/* Header and Footer */
	c_put_str(TERM_WHITE, "abcdefghijkl@", col, row - 1);
	c_put_str(TERM_L_GREEN, "Modifications", col, row + 6);

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Access object */
		o_ptr = &inventory[i];

		/* Object kind */
		k_idx = o_ptr->k_idx;

		/* Acquire "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Initialize color based of sign of pval. */
		for (stat = 0; stat < A_MAX; stat++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';

			/* Boost */
			if (f1 & 1 << stat)
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

				if (f2 & 1 << stat)
				{
					/* Dark green for sustained stats. */
					a = TERM_GREEN;
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
			else if (f2 & 1 << stat)
			{
				/* Dark green "s" */
				a = TERM_GREEN;
				c = 's';
			}

			/* Handle monochrome */
			if (!use_color || ironman_moria) a = TERM_WHITE;

			/* Dump proper character */
			Term_putch(col, row + stat, a, c);
		}

		/* Advance */
		col++;
	}

	/* Player flags */
	player_flags(&f1, &f2, &f3);

	/* Check stats */
	for (stat = 0; stat < A_MAX; stat++)
	{
		/* Default */
		a = TERM_SLATE;
		c = '.';

		/* Mutations ... */
		if (p_ptr->muta3)
		{
			int dummy = 0;

			if (stat == A_STR)
			{
				if (p_ptr->muta3 & MUT3_HYPER_STR) dummy += 4;
				if (p_ptr->muta3 & MUT3_PUNY) dummy -= 4;
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
		if (f2 & 1 << stat)
		{
			/* Dark green "s" */
			a = TERM_GREEN;
			c = 's';
		}


		/* No color */
		if (!use_color || ironman_moria) a = TERM_WHITE;

		/* Dump */
		Term_putch(col, row + stat, a, c);
	}
}

#define COL_NAME			0
#define WID_NAME			11

#define COL_AGE				32
#define COL_STATS			55


/*
 * Display the standard player information
 */
static void display_player_top(void)
{
	int i;
	char buf[80];

	/* Name, Sex, Race, Class */
	put_str("Name     :", COL_NAME, 2);
	put_str("Sex      :", COL_NAME, 3);
	put_str("Race     :", COL_NAME, 4);
	put_str("Class    :", COL_NAME, 5);

	if (p_ptr->realm1 || p_ptr->realm2)
	{
		put_str("Magic    :", COL_NAME, 6);
	}

	if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
	{
		put_str("Patron   :", COL_NAME, 7);
	}

	c_put_str(TERM_L_BLUE, player_name, COL_NAME + WID_NAME, 2);
	c_put_str(TERM_L_BLUE, sp_ptr->title, COL_NAME + WID_NAME, 3);
	c_put_str(TERM_L_BLUE, rp_ptr->title, COL_NAME + WID_NAME, 4);
	c_put_str(TERM_L_BLUE, cp_ptr->title, COL_NAME + WID_NAME, 5);

	if (p_ptr->realm1)
	{
		c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm1], COL_NAME + WID_NAME,
				  6);
	}

	if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
	{
		c_put_str(TERM_L_BLUE, chaos_patrons[p_ptr->chaos_patron],
				  COL_NAME + WID_NAME, 7);
	}

	else if (p_ptr->realm2)
	{
		c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm2], COL_NAME + WID_NAME,
				  7);
	}

	/* Age, Height, Weight, Social */
	prt_num("Age         ", (int)p_ptr->age, COL_AGE, 2, TERM_L_BLUE, 3);
	prt_num("Height      ", (int)p_ptr->ht, COL_AGE, 3, TERM_L_BLUE, 3);
	prt_num("Weight      ", (int)p_ptr->wt, COL_AGE, 4, TERM_L_BLUE, 3);
	prt_num("Social Class", (int)p_ptr->sc, COL_AGE, 5, TERM_L_BLUE, 3);

	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Special treatment of "injured" stats */
		if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
		{
			int value;

			/* Use lowercase stat name */
			put_str(stat_names_reduced[i], COL_STATS, i + 2);

			/* Get the current stat */
			value = p_ptr->stat_use[i];

			/* Obtain the current stat (modified) */
			cnv_stat(value, buf);

			/* Display the current stat (modified) */
			c_put_str(TERM_YELLOW, buf, COL_STATS + 5, i + 2);

			/* Acquire the max stat */
			value = p_ptr->stat_top[i];

			/* Obtain the maximum stat (modified) */
			cnv_stat(value, buf);

			/* Display the maximum stat (modified) */
			c_put_str(TERM_L_GREEN, buf, COL_STATS + 5 + 7, i + 2);
		}

		/* Normal treatment of "normal" stats */
		else
		{
			/* Assume uppercase stat name */
			put_str(stat_names[i], COL_STATS, i + 2);

			/* Obtain the current stat (modified) */
			cnv_stat(p_ptr->stat_use[i], buf);

			/* Display the current stat (modified) */
			c_put_str(TERM_L_GREEN, buf, COL_STATS + 5, i + 2);
		}
	}
}


#define COL_BONUS			0
#define COL_VALUE			23
#define COL_LIFE     	51


/*
 * Display the player attributes.
 */
static void display_player_middle(void)
{
	int percentdam;
	int show_tohit = p_ptr->dis_to_h;
	int show_todam = p_ptr->dis_to_d;
	byte attr;

	object_type *o_ptr = &inventory[INVEN_WIELD];

	/* Hack -- add in weapon info if known */
	if (object_known_p(o_ptr)) show_tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) show_todam += o_ptr->to_d;

	/* convert to oangband "deadliness" */
	percentdam = deadliness_calc(show_todam);

	/*** Bonuses ***/

	prt_num("+ Skill     ", show_tohit, COL_BONUS, 9, TERM_L_BLUE, 3);
	prt_num("% Deadliness", percentdam, COL_BONUS, 10, TERM_L_BLUE, 3);
	prt_num("+ To AC     ", p_ptr->dis_to_a, COL_BONUS, 11, TERM_L_BLUE, 3);
	prt_num("  Base AC   ", p_ptr->dis_ac, COL_BONUS, 12, TERM_L_BLUE, 3);


	/*** Level, experience, gold ***/

	prt_num("Level      ", (int)p_ptr->lev, COL_VALUE, 9, TERM_L_GREEN, 9);

	if (p_ptr->exp >= p_ptr->max_exp)
	{
		attr = TERM_L_GREEN;
	}
	else
	{
		attr = TERM_YELLOW;
	}

	prt_num("Experience ", p_ptr->exp, COL_VALUE, 10, attr, 9);
	prt_num("Max Exp    ", p_ptr->max_exp, COL_VALUE, 11, TERM_L_GREEN, 9);

	if (p_ptr->lev >= PY_MAX_LEVEL)
	{
		put_str("Exp to Adv.", COL_VALUE, 12);
		c_put_str(TERM_L_GREEN, "       *****", COL_VALUE + 11, 12);
	}
	else if (toggle_xp)
	{
		/* Print the amount of xp until next level */
		prt_num("Exp to Adv.",
				(long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L
					   - (long)p_ptr->exp), COL_VALUE, 12, TERM_L_GREEN, 9);
	}
	else
	{
		/* Print the total xp required for next level */
		prt_num("Exp to Adv.",
				(long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L),
				COL_VALUE, 12, TERM_L_GREEN, 9);
	}


	prt_num("Gold       ", p_ptr->au, COL_VALUE, 13, TERM_L_GREEN, 9);


	/*** Hitpoints/spellpoints ***/

	prt_num("Max Hit Points", p_ptr->mhp, COL_LIFE, 9, TERM_L_GREEN, 5);

	if (p_ptr->chp >= p_ptr->mhp)
	{
		attr = TERM_L_GREEN;
	}
	else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
	{
		attr = TERM_YELLOW;
	}
	else
	{
		attr = TERM_RED;
	}

	prt_num("Cur Hit Points", p_ptr->chp, COL_LIFE, 10, attr, 5);

	prt_num("Max SP (Mana) ", p_ptr->msp, COL_LIFE, 11, TERM_L_GREEN, 5);

	if (p_ptr->csp >= p_ptr->msp)
	{
		attr = TERM_L_GREEN;
	}
	else if (p_ptr->csp > (p_ptr->msp * hitpoint_warn) / 10)
	{
		attr = TERM_YELLOW;
	}
	else
	{
		attr = TERM_RED;
	}

	prt_num("Cur SP (Mana) ", p_ptr->csp, COL_LIFE, 12, attr, 5);
}


/*
 * Display the standard player information and abilities
 */
static void display_player_standard(void)
{
	/* Basic info */
	display_player_top();

	/* Extra info */
	display_player_middle();

	put_str("(Miscellaneous Abilities)", 25, 15);

	/* Display the abilities */
	display_player_abilities();
}


/*
 * Display the standard player information and history
 */
static void display_player_history(void)
{
	int i;

	/* Basic info */
	display_player_top();

	/* Extra info */
	display_player_middle();

	put_str("(Character Background)", 25, 15);

	/* Dump the history */
	for (i = 0; i < 4; i++)
	{
		put_str(p_ptr->history[i], 10, i + 16);
	}
}


/*
 * Display a summary of the player's attributes
 *
 * See "http://www.cs.berkeley.edu/~davidb/angband.html"
 */
static void display_player_summary(void)
{
	/* Dump the info */
	display_player_stat_info();
	display_player_flag_info();
}


typedef void (*display_func) (void);


static display_func displays[DISPLAY_PLAYER_MAX] =
{
	/* Standard display with skills */
	display_player_standard,

	/* Standard display with history */
	display_player_history,

	/* Summary of various things */
	display_player_summary,
};


/*
 * Display the character on the screen (various modes)
 *
 * The top two and bottom two lines are left blank.
 */
void display_player(int mode)
{
	mode %= DISPLAY_PLAYER_MAX;

	/* Erase screen */
	clear_from(0);

	/* Display it */
	(*(displays[mode])) ();
}


/*
 * Hack -- change name
 */
void do_cmd_character(void)
{
	char c;

	int mode = DISPLAY_PLAYER_STANDARD;

	char tmp[160];

	/* Save the screen */
	screen_save();

	/* Forever */
	while (1)
	{
		/* Display the player */
		display_player(mode);

		/* Prompt */
		Term_putstr(2, 23, -1, TERM_WHITE,
					"['c' to change name, 'f' to file, 'p' for previous, 'n' for next, or ESC]");

		/* Query */
		c = inkey();

		/* Exit */
		if (c == ESCAPE) break;

		/* Change name */
		if (c == 'c')
		{
			change_player_name();
		}

		/* File dump */
		else if (c == 'f')
		{
			sprintf(tmp, "%s.txt", player_base);
			if (get_string("File name: ", tmp, 80))
			{
				if (tmp[0] && (tmp[0] != ' '))
				{
					(void)file_character(tmp, TRUE);
				}
			}
		}

		/* Decrease mode */
		else if (c == 'p')
		{
			mode = (mode + DISPLAY_PLAYER_MAX - 1) % DISPLAY_PLAYER_MAX;
		}

		/* Increase mode */
		else if (c == 'n')
		{
			mode = (mode + 1) % DISPLAY_PLAYER_MAX;
		}

		/* Oops */
		else
		{
			bell("Illegal option!");
		}

		/* Flush messages */
		message_flush();
	}

	/* Restore the screen */
	screen_load();

	/* Redraw everything */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	handle_stuff();
}


/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(cptr name, bool full)
{
	int i, j, k, x, y;
	byte a;
	char c;
	cptr paren = ")";
	int fd = -1;
	FILE *fff = NULL;
	store_type *st_ptr;
	char o_name[256];
	char buf[1024];

	int msg_max = message_num();


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

	/* Invalid file */
	if (!fff)
	{
		/* Message */
		msg_format("Character dump failed!");
		message_flush();

		/* Error */
		return (-1);
	}


	fprintf(fff, "  [%s %s Character Dump]\n\n", VERSION_NAME, VERSION_STRING);

	/* Display player */
	display_player(DISPLAY_PLAYER_STANDARD);

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

		/* End the string */
		buf[x] = '\0';

		/* Kill trailing spaces */
		while ((x > 0) && (buf[x - 1] == ' ')) buf[--x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Display history */
	display_player(DISPLAY_PLAYER_HISTORY);

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

		/* End the string */
		buf[x] = '\0';

		/* Kill trailing spaces */
		while ((x > 0) && (buf[x - 1] == ' ')) buf[--x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	fprintf(fff, "\n\n  [Miscellaneous information]\n");

	if (preserve_mode)
		fprintf(fff, "\n Preserve Mode:      ON");
	else
		fprintf(fff, "\n Preserve Mode:      OFF");

	if (ironman_autoscum)
		fprintf(fff, "\n Autoscum:           ALWAYS");
	else if (auto_scum)
		fprintf(fff, "\n Autoscum:           ON");
	else
		fprintf(fff, "\n Autoscum:           OFF");

	if (ironman_small_levels)
		fprintf(fff, "\n Small Levels:       ALWAYS");
	else if (small_levels)
		fprintf(fff, "\n Small Levels:       ON");
	else
		fprintf(fff, "\n Small Levels:       OFF");

	if (vanilla_town)
		fprintf(fff, "\n Vanilla Town:       ON");

	if (ironman_shops)
		fprintf(fff, "\n No Shops:           ON");

	if (ironman_downward)
		fprintf(fff, "\n Diving only:        ON");

	if (ironman_empty_levels)
		fprintf(fff, "\n Arena Levels:       ALWAYS");
	else if (empty_levels)
		fprintf(fff, "\n Arena Levels:       ENABLED");
	else
		fprintf(fff, "\n Arena Levels:       OFF");

	if (ironman_hard_quests)
		fprintf(fff, "\n Hard Quests:        ON");
	else
		fprintf(fff, "\n Hard Quests:        OFF");

	fprintf(fff, "\n Number of Quests: %d", number_of_quests());

	if (ironman_nightmare)
		fprintf(fff, "\n Nightmare Mode:     ON");
	else
		fprintf(fff, "\n Nightmare Mode:     OFF");

	fprintf(fff, "\n Recall Depth:       Level %d (%d')\n", p_ptr->max_depth,
			50 * p_ptr->max_depth);

	if (p_ptr->noscore)
		fprintf(fff, "\n You have done something illegal.");

	if (stupid_monsters)
		fprintf(fff, "\n Your opponents are behaving stupidly.");

	if (munchkin_death)
		fprintf(fff, "\n You possess munchkinish power over death.");

	/* Show (known) flags grid */
	if (full)
	{
		fprintf(fff, "\n\n");
		display_player(DISPLAY_PLAYER_SUMMARY);

		/* Dump first column */
		for (y = 6; y < 23; y++)
		{
			for (x = 0; x < 21; x++)
			{
				(void)(Term_what(x, y, &a, &c));
				buf[x] = c;
			}

			buf[x] = '\0';
			fprintf(fff, "%s\n", buf);
		}

		/* New line */
		fprintf(fff, "\n");

		/* Dump second column */
		for (y = 12; y < 22; y++)
		{
			for (x = 0; x < 23; x++)
			{
				(void)(Term_what(x + 25, y, &a, &c));
				buf[x] = c;
			}

			buf[x] = '\0';
			fprintf(fff, "%s\n", buf);
		}

		/* New line */
		fprintf(fff, "\n");

		/* Dump third column */
		for (y = 12; y < 23; y++)
		{
			for (x = 0; x < 24; x++)
			{
				(void)(Term_what(x + 52, y, &a, &c));
				buf[x] = c;
			}

			buf[x] = '\0';
			fprintf(fff, "%s\n", buf);
		}
	}

	/* Monsters slain */
	{
		u32b Total = 0;

		for (k = 1; k < z_info->r_max; k++)
		{
			monster_race *r_ptr = &r_info[k];

			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				if (r_ptr->max_num == 0) Total++;
			}
			else
			{
				Total += r_ptr->r_pkills;
			}
		}

		if (Total < 1)
			fprintf(fff, "\n You have defeated no enemies yet.\n");
		else if (Total == 1)
			fprintf(fff, "\n You have defeated one enemy.\n");
		else
			fprintf(fff, "\n You have defeated %lu enemies.\n", Total);
	}

#if 0
	fprintf(fff, "\n\n  [Virtues]\n\n");
	dump_virtues(fff);
#endif /* 0 */

	if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
	{
		fprintf(fff, "\n\n  [Mutations]\n\n");
		dump_mutations(fff);
	}


	/* Skip some lines */
	fprintf(fff, "\n\n");


	/* Dump the equipment */
	if (p_ptr->equip_cnt)
	{
		fprintf(fff, "  [Character Equipment]\n\n");
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			object_desc(o_name, &inventory[i], TRUE, 3, 256);
			fprintf(fff, "%c%s %s\n", index_to_label(i), paren, o_name);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
	fprintf(fff, "  [Character Inventory]\n\n");
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Don't dump the empty slots */
		if (!inventory[i].k_idx) break;

		/* Dump the inventory slots */
		object_desc(o_name, &inventory[i], TRUE, 3, 256);
		fprintf(fff, "%c%s %s\n", index_to_label(i), paren, o_name);
	}

	/* Add an empty line */
	fprintf(fff, "\n\n");

	/* Print all homes in the different towns */
	for (i = 1; i < z_info->wp_max; i++)
	{
		for (j = 0; j < place[i].numstores; j++)
		{
			st_ptr = &place[i].store[j];

			if (st_ptr->type == BUILD_STORE_HOME)
			{
				/* Home -- if anything there */
				if (st_ptr->stock_num)
				{
					/* Header with name of the town */
					fprintf(fff, "  [Home Inventory - %s]\n\n", place[i].name);

					/* Dump all available items */
					for (k = 0; k < st_ptr->stock_num; k++)
					{
						object_desc(o_name, &st_ptr->stock[k], TRUE, 3, 256);
						fprintf(fff, "%c%s %s\n", I2A(k), paren, o_name);
					}

					/* Add an empty line */
					fprintf(fff, "\n\n");
				}
			}
		}
	}

	fprintf(fff, "  [Message Log (last %d messages)]\n\n", msg_max);

	for (i = msg_max - 1; i >= 0; i--)
	{
		fprintf(fff, "%s\n", message_str((s16b)i));
	}
	fprintf(fff, "\n\n");


	/* Close it */
	my_fclose(fff);


	/* Message */
	msg_print("Character dump successful.");
	message_flush();

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
	int i, n, k;
	int wid, hgt;

	/* Number of "real" lines passed by */
	int next = 0;

	/* Number of "real" lines in the file */
	int size = 0;

	/* Backup value for "line" */
	int back = 0;

	/* Loop counter */
	int cnt;

	/* This screen has sub-screens */
	bool menu = FALSE;

	/* Current help file */
	FILE *fff = NULL;

	/* Find this string (if any) */
	cptr find = NULL;

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

	/* Aux pointer for making lc_buf (and find!) lowercase */
	cptr lc_buf_ptr;

	/* Sub-menu information */
	char hook[62][32];


	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Wipe finder */
	strcpy(finder, "");

	/* Wipe shower */
	strcpy(shower, "");

	/* Wipe caption */
	strcpy(caption, "");

	/* Wipe the hooks */
	for (i = 0; i < 62; i++)
	{
		hook[i][0] = '\0';
	}

	/* Copy the filename */
	strcpy(filename, name);

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
		message_flush();

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
			/* Notice "menu" requests */
			if ((buf[6] == '[') && (isdigit(buf[7]) || isalpha(buf[7])))
			{
				/* This is a menu file */
				menu = TRUE;

				/* Extract the menu item */
				k = isdigit(buf[7]) ? D2I(buf[7]) : buf[7] - 'A' + 10;

				if ((buf[8] == ']') && (buf[9] == ' '))
				{
					/* Extract the menu item */
					strncpy(hook[k], buf + 10, 31);

					/* Make sure it's null-terminated */
					hook[k][31] = '\0';
				}
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
			if (!fff) return (FALSE);

			/* File has been restarted */
			next = 0;
		}

		/* Goto the selected line */
		while (next < line)
		{
			/* Get a line */
			if (my_fgets(fff, buf, 1024)) break;

			/* Skip tags/links */
			if (prefix(buf, "***** ")) continue;

			/* Count the lines */
			next++;
		}

		/* Dump the next hgt - 4 lines of the file */
		for (i = 0; i < hgt - 4;)
		{
			/* Hack -- track the "first" line */
			if (!i) line = next;

			/* Get a line of the file or stop */
			if (my_fgets(fff, buf, 1024)) break;

			/* Hack -- skip "special" lines */
			if (prefix(buf, "***** ")) continue;

			/* Count the "real" lines */
			next++;

			/* Make a lower case version of buf for searching */
			strcpy(lc_buf, buf);

			for (lc_buf_ptr = lc_buf; *lc_buf_ptr != 0; lc_buf_ptr++)
			{
				lc_buf[lc_buf_ptr - lc_buf] = tolower(*lc_buf_ptr);
			}

			/* Hack -- keep searching */
			if (find && !i && !strstr(lc_buf, find)) continue;

			/* Hack -- stop searching */
			find = NULL;

			/* Dump the line */
			Term_putstr(0, i + 2, -1, TERM_WHITE, buf);

			/* Hilite "shower" */
			if (shower[0])
			{
				cptr str = lc_buf;

				/* Display matches */
				while ((str = strstr(str, shower)) != NULL)
				{
					int len = strlen(shower);

					/* Display the match */
					Term_putstr(str - lc_buf, i + 2, len, TERM_YELLOW,
								&buf[str - lc_buf]);

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
		prt(format("[%s %s, %s, Line %d/%d]",
				   VERSION_NAME, VERSION_STRING, caption, line, size), 0, 0);

		/* Prompt -- with menu */
		if (menu)
		{
			/* Prompt -- small files */
			if (size <= hgt - 4)
			{
				/* Wait for it */
				prt("[Press a Number, or ESC to exit.]", 0, hgt - 1);
			}

			/* Prompt -- large files */
			else
			{
				/* Wait for it */
				prt("[Press a Number, Return, Space, -, =, /, or ESC to exit.]",
					0, hgt - 1);
			}
		}
		else
		{
			/* Prompt -- small files */
			if (size <= hgt - 4)
			{
				/* Wait for it */
				prt("[Press ESC to exit.]", 0, hgt - 1);
			}

			/* Prompt -- large files */
			else
			{
				/* Wait for it */
				prt("[Press Return, Space, -, =, /, or ESC to exit.]", 0,
					hgt - 1);
			}
		}

		/* Get a keypress */
		k = inkey();

		/* Hack -- return to last screen */
		if (k == '<') break;

		/* Show the help for the help */
		if (k == '?')
		{
			/* Hack - prevent silly recursion */
			if (strcmp(name, "helpinfo.txt") != 0)
				show_file("helpinfo.txt", NULL, 0, mode);
		}

		/* Hack -- try showing */
		if (k == '=')
		{
			/* Get "shower" */
			prt("Show: ", 0, hgt - 1);
			(void)askfor_aux(shower, 80);
		}

		/* Hack -- try finding */
		if (k == '/')
		{
			/* Get "finder" */
			prt("Find: ", 0, hgt - 1);

			if (askfor_aux(finder, 80))
			{
				/* Find it */
				find = finder;
				back = line;
				line = line + 1;

				/* Make finder lowercase */
				for (cnt = 0; finder[cnt] != 0; cnt++)
				{
					finder[cnt] = tolower(finder[cnt]);
				}

				/* Show it */
				strcpy(shower, finder);
			}
		}

		/* Go to a specific line */
		if (k == '#')
		{
			char tmp[81];
			prt("Goto Line: ", 0, hgt - 1);
			strcpy(tmp, "0");

			if (askfor_aux(tmp, 80))
			{
				line = atoi(tmp);
			}
		}

		/* Go to a specific file */
		if (k == '%')
		{
			char tmp[81];
			prt("Goto File: ", 0, hgt - 1);
			strcpy(tmp, "help.hlp");

			if (askfor_aux(tmp, 80))
			{
				if (!show_file(tmp, NULL, 0, mode)) k = ESCAPE;
			}
		}

		/* Go back half a page */
		if (k == '-')
		{
			line = line - (hgt - 4) / 2;
			if (line < 0) line = 0;
		}

		/* Advance half a page */
		if (k == '+')
		{
			line = line + (hgt - 4) / 2;
		}

		/* Advance a single line */
		if ((k == '\n') || (k == '\r'))
		{
			line = line + 1;
		}

		/* Advance one page */
		if (k == ' ')
		{
			line = line + hgt - 4;
		}

		/* Recurse on numbers */
		if (menu)
		{
			int key = -1;

			if (isdigit(k)) key = D2I(k);
			else if (isalpha(k)) key = k - 'A' + 10;

			if ((key > -1) && hook[key][0])
			{
				/* Recurse on that file */
				if (!show_file(hook[key], NULL, 0, mode))
					k = ESCAPE;
			}
		}

		/* Dump to file */
		if (k == '|')
		{
			FILE *ffp;
			char outfile[1024];
			char xtmp[82];

			/* Start with an empty filename */
			xtmp[0] = '\0';

			/* Get a filename */
			if (!get_string("File name: ", xtmp, 80)) continue;

			/* Check for a "valid" name */
			if (!(xtmp[0] && (xtmp[0] != ' '))) continue;

			/* Build the filename */
			path_build(outfile, 1024, ANGBAND_DIR_USER, xtmp);

			/* Close the input file */
			my_fclose(fff);

			/* Hack -- Re-Open the input file */
			fff = my_fopen(path, "r");

			/* Open the output file */
			ffp = my_fopen(outfile, "w");

			/* Oops */
			if (!(fff && ffp))
			{
				msg_print("Failed to open file.");
				k = ESCAPE;
				break;
			}

			/* Write the file line by line */
			while (!my_fgets(fff, xtmp, 80))
				my_fputs(ffp, xtmp, 80);

			/* Close the files */
			my_fclose(fff);
			my_fclose(ffp);

			/* Hack -- Re-Open the input file */
			fff = my_fopen(path, "r");
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


#ifdef SAVEFILE_MUTABLE

	/* Accept */
	sf = TRUE;

#endif

	/* Change the savefile name */
	if (sf)
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
 */
void change_player_name(void)
{
	char tmp[32];

	/* Clear last line */
	clear_from(22);

	/* Prompt and ask */
	prt("[Enter your player's name above, or hit ESCAPE]", 2, 23);

	/* Ask until happy */
	while (1)
	{
		/* Go to the "name" field */
		Term_gotoxy(COL_NAME + WID_NAME, 2);

		/* Save the player name */
		strcpy(tmp, player_name);

		/* Get an input, ignore "Escape" */
		if (askfor_aux(tmp, 16)) strcpy(player_name, tmp);

		/* Process the player name */
		process_player_name(FALSE);

		/* All done */
		break;
	}

	/* Pad the name (to clear junk) */
	sprintf(tmp, "%-15.15s", player_name);

	/* Re-Draw the name (in light blue) */
	c_put_str(TERM_L_BLUE, tmp, COL_NAME + WID_NAME, 2);

	/* Erase the prompt, etc */
	clear_from(22);
}

/* Gets a name for the character, reacting to name changes.
 * Taken from V 2.9.0.
 */

void get_character_name(void)
{
	char tmp[16];

	/* Save the player name */
	strcpy(tmp, player_name);

	/* Prompt for a new name */
	if (get_string("Enter a name for your character: ", tmp, sizeof(tmp)))
	{
		/* Use the name */
		strcpy(player_name, tmp);

		/* Process the player name */
		process_player_name(FALSE);
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
		if (!get_check("Do you really want to commit suicide? ")) return;

		if (!p_ptr->noscore)
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
	p_ptr->playing = FALSE;

	/* Kill the player */
	p_ptr->is_dead = TRUE;

	/* Leaving */
	p_ptr->leaving = TRUE;

	/* Cause of death */
	(void)strcpy(p_ptr->died_from, "Quitting");
}


/*
 * Save the game
 */
void do_cmd_save_game(int is_autosave)
{
	/* Autosaves do not disturb */
	if (is_autosave)
	{
		msg_print("Autosaving the game...");
	}
	else
	{
		/* Disturb the player */
		disturb(TRUE);
	}

	/* Clear messages */
	message_flush();

	/* Handle stuff */
	handle_stuff();

	/* Message */
	prt("Saving game...", 0, 0);

	/* Refresh */
	Term_fresh();

	/* The player is not dead */
	(void)strcpy(p_ptr->died_from, "(saved)");

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

	/* Clear messages. */
	message_flush();

	/* Hack -- erase the message line. */
	prt("", 0, 0);

	/* Note that the player is not dead */
	(void)strcpy(p_ptr->died_from, "(alive and well)");
}


/*
 * Save the game and exit
 */
void do_cmd_save_and_exit(void)
{
	p_ptr->playing = FALSE;

	/* Leaving */
	p_ptr->leaving = TRUE;
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
			fprintf(fp, "%s\n", player_name);
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
 * Redefinable "print_tombstone" action
 */
bool (*tombstone_aux) (void) = NULL;


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
		done = (*tombstone_aux) ();
	}

	/* Print the text-tombstone */
	if (!done)
	{
		cptr p;

		char tmp[160];

		char buf[1024];
		char dummy[80];

		FILE *fp;

		time_t ct = time((time_t) 0);


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
				put_str(buf, 0, i++);
			}

			/* Close */
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
			p = player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5];
		}

		center_string(buf, player_name);
		put_str(buf, 11, 6);

		center_string(buf, "the");
		put_str(buf, 11, 7);

		center_string(buf, p);
		put_str(buf, 11, 8);


		center_string(buf, cp_ptr->title);
		put_str(buf, 11, 10);

		(void)sprintf(tmp, "Level: %d", (int)p_ptr->lev);
		center_string(buf, tmp);
		put_str(buf, 11, 11);

		(void)sprintf(tmp, "Exp: %ld", (long)p_ptr->exp);
		center_string(buf, tmp);
		put_str(buf, 11, 12);

		(void)sprintf(tmp, "AU: %ld", (long)p_ptr->au);
		center_string(buf, tmp);
		put_str(buf, 11, 13);

		(void)sprintf(tmp, "Killed on Level %d", p_ptr->depth);
		center_string(buf, tmp);
		put_str(buf, 11, 14);


		if (strlen(p_ptr->died_from) > 24)
		{
			strncpy(dummy, p_ptr->died_from, 24);
			dummy[24] = '\0';
			(void)sprintf(tmp, "by %s.", dummy);
		}
		else
			(void)sprintf(tmp, "by %s.", p_ptr->died_from);

		center_string(buf, tmp);
		put_str(buf, 11, 15);


		(void)sprintf(tmp, "%-.24s", ctime(&ct));
		center_string(buf, tmp);
		put_str(buf, 11, 17);
	}
}


/*
 * Display some character info
 */
static void show_info(void)
{
	int i, j, k, l;
	object_type *o_ptr;
	store_type *st_ptr;

	/* Hack -- Know everything in the inven/equip */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
		o_ptr->ident |= IDENT_MENTAL;

		/* Save all the known flags */
		o_ptr->kn_flags1 = o_ptr->flags1;
		o_ptr->kn_flags2 = o_ptr->flags2;
		o_ptr->kn_flags3 = o_ptr->flags3;
	}

	for (i = 1; i < z_info->wp_max; i++)
	{
		for (j = 0; j < place[i].numstores; j++)
		{
			st_ptr = &place[i].store[j];

			if (st_ptr->type == BUILD_STORE_HOME)
			{
				/* Hack -- Know everything in the home */
				for (k = 0; k < st_ptr->stock_num; k++)
				{
					o_ptr = &st_ptr->stock[k];

					/* Skip non-objects */
					if (!o_ptr->k_idx) continue;

					/* Aware and Known */
					object_aware(o_ptr);
					object_known(o_ptr);
					o_ptr->ident |= IDENT_MENTAL;

					/* Save all the known flags */
					o_ptr->kn_flags1 = o_ptr->flags1;
					o_ptr->kn_flags2 = o_ptr->flags2;
					o_ptr->kn_flags3 = o_ptr->flags3;
				}
			}
		}
	}

	/* Hack -- Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Display player */
	display_player(DISPLAY_PLAYER_STANDARD);

	/* Prompt for inventory */
	prt("Hit any key to see more information (ESC to abort): ", 0, 23);

	/* Flush keys */
	flush();

	/* Allow abort at this point */
	if (inkey() == ESCAPE) return;


	/* Show equipment and inventory */
	if (p_ptr->equip_cnt)
	{
		Term_clear();

		/* Equipment -- if any */
		item_tester_full = TRUE;
		show_equip();

		prt("You are using: -more-", 0, 0);

		/* Flush keys */
		flush();

		if (inkey() == ESCAPE) return;
	}


	if (p_ptr->inven_cnt)
	{
		Term_clear();

		/* Inventory -- if any */
		item_tester_full = TRUE;
		show_inven();

		prt("You are carrying: -more-", 0, 0);

		/* Flush keys */
		flush();

		if (inkey() == ESCAPE) return;
	}

	for (i = 1; i < z_info->wp_max; i++)
	{
		for (l = 0; l < place[i].numstores; l++)
		{
			st_ptr = &place[i].store[l];

			if (st_ptr->type == BUILD_STORE_HOME)
			{
				/* Home -- if anything there */
				if (st_ptr->stock_num)
				{
					/* Display contents of the home */
					for (k = 0, i = 0; i < st_ptr->stock_num; k++)
					{
						/* Clear screen */
						Term_clear();

						/* Show 12 items */
						for (j = 0; (j < 12) && (i < st_ptr->stock_num);
							 j++, i++)
						{
							char o_name[256];
							char tmp_val[80];

							/* Acquire item */
							o_ptr = &st_ptr->stock[i];

							/* Print header, clear line */
							sprintf(tmp_val, "%c) ", I2A(j));
							prt(tmp_val, 4, j + 2);

							/* Display object description */
							object_desc(o_name, o_ptr, TRUE, 3, 256);
							c_put_str(tval_to_attr[o_ptr->tval], o_name, 7,
									  j + 2);
						}

						/* Caption */
						prt(format
							("Your home contains (page %d): -more-", k + 1), 0,
							0);

						/* Flush keys */
						flush();

						/* Wait for it */
						if (inkey() == ESCAPE) return;
					}
				}
			}
		}
	}
}


static void close_game_handle_death(void)
{
	char ch;

	/* Handle retirement */
	if (p_ptr->total_winner)
	{
		/* Save winning message to notes file. */
		if (take_notes)
		{
			add_note_type(NOTE_WINNER);
		}

		kingly();
	}

	/* Save memories */
	if (!munchkin_death || get_check("Save death? "))
	{
		if (!save_player()) msg_print("death save failed!");
	}

#if 0
	/* Dump bones file */
	make_bones();
#endif

	/* Inform notes file that you are dead */
	if (take_notes)
	{
		char long_day[30];
		char buf[80];
		time_t ct = time((time_t *) NULL);

		/* Get the date */
		(void)strftime(long_day, 30, "%Y-%m-%d at %H:%M:%S", localtime(&ct));

		/* Create string */
		sprintf(buf, "\n%s was killed by %s on %s\n", player_name,
				p_ptr->died_from, long_day);

		/* Output to the notes file */
		output_note(buf);
	}

	/* Enter player in high score list */
	enter_score();

	/* You are dead */
	print_tomb();

	/* Describe options */
	prt("(D) Dump char record  (C) Show char info  (T) Show top scores  (ESC) Exit", 0, 22);

	/* Flush messages */
	message_flush();

	/* Flush all input keys */
	flush();

	/* Save screen here out of loop to avoid saving more than once */
	Term_save();

	/* Player selection */
	while (TRUE)
	{
		/* Load screen */
		Term_load();

		/* Flush all input keys */
		flush();

		ch = inkey();

		switch (ch)
		{
			case ESCAPE:
			{
				/* Flush the keys */
				flush();

				if (get_check("Do you really want to exit? "))
				{
					/* Save dead player */
					if (!save_player())
					{
						msg_print("Death save failed!");
						message_flush();
					}

#if 0
					/* Dump bones file */
					make_bones();
#endif
					/* Go home, we're done */
					return;
				}
				else
				{
					break;
				}
			}

			case 'd':
			case 'D':
			{
				/* Dump char file */
				char tmp[160] = "";

				/* Prompt */
				put_str("Filename: ", 0, 23);

				/* Ask for filename (or abort) */
				if (!askfor_aux(tmp, 60)) continue;

				/* Ignore Return */
				if (!tmp[0]) continue;

				/* Dump a character file */
				(void)file_character(tmp, FALSE);

				break;
			}

			case 'c':
			case 'C':
			{
				/* Show char info */
				show_info();
				break;
			}

			case 't':
			case 'T':
			{
				/* Show top twenty */
				top_twenty();
				break;
			}
		}
	}
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
	message_flush();

	/* Flush the input */
	flush();

	/* No suspending now */
	signals_ignore_tstp();


	/* Hack -- Character is now "icky" */
	character_icky = TRUE;


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Grab permissions */
	safe_setuid_grab();

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);

	/* Drop permissions */
	safe_setuid_drop();

	if (p_ptr->is_dead)
	{
		/* Handle death */
		close_game_handle_death();
	}

	/* Still alive */
	else
	{
		/* Save the game */
		do_cmd_save_game(FALSE);

		/* If note-taking enabled, write session end to notes file */
		if (take_notes)
		{
			add_note_type(NOTE_SAVE_GAME);
		}

		/* Prompt for scores XXX XXX XXX */
		prt("Press Return (or Escape).", 40, 0);

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
	disturb(TRUE);

	/* Mega-Hack -- Delay death */
	if (p_ptr->chp < 0) p_ptr->is_dead = FALSE;

	/* Hardcode panic save */
	p_ptr->panic_save = 1;

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Indicate panic save */
	(void)strcpy(p_ptr->died_from, "(panic save)");

	/* Panic save, or get worried */
	if (!save_player()) quit("panic save failed!");

	/* Successful panic save */
	quit("panic save succeeded!");
}


/*
 * Get a random line from a file
 * Based on the monster speech patch by Matt Graham,
 */
errr get_rnd_line(cptr file_name, int entry, char *output)
{
	FILE *fp;
	char buf[1024];
	int line, counter, test, numentries;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, file_name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp) return (-1);

	/* Find the entry of the monster */
	while (TRUE)
	{
		/* Get a line from the file */
		if (my_fgets(fp, buf, 1024) == 0)
		{
			/* Look for lines starting with 'N:' */
			if ((buf[0] == 'N') && (buf[1] == ':'))
			{
				/* Allow default lines */
				if (buf[2] == '*') break;

				/* Get the monster number */
				else if (sscanf(&(buf[2]), "%d", &test) != EOF)
				{
					/* Is it the right monster? */
					if (test == entry) break;
				}
				else
				{
					/* Error while converting the monster number */
					msg_print("Error - end of file.");

					my_fclose(fp);
					return (-1);
				}
			}
		}
		else
		{
			/* Reached end of file */
			my_fclose(fp);
			return (-1);
		}

	}

	/* Get the number of entries */
	while (TRUE)
	{
		/* Get the line */
		if (my_fgets(fp, buf, 1024) == 0)
		{
			/* Look for the number of entries */
			if (isdigit(buf[0]))
			{
				/* Get the number of entries */
				numentries = atoi(buf);
				break;
			}
		}
		else
		{
			/* Reached end of file without finding the number */
			msg_print("Error - end of file.");

			my_fclose(fp);
			return (-1);
		}
	}

	if (numentries > 0)
	{
		/* Grab an appropriate line number */
		line = randint1(numentries);

		/* Get the random line */
		for (counter = 0; counter < line; counter++)
		{
			/* Try to read the line, skipping comments */
			while (TRUE)
			{
				test = my_fgets(fp, buf, 1024);
				if (test != 0 || buf[0] != '#') break;
			}

			if (test != 0)
			{
				/* Error - End of file */
				msg_format("Error - end of file.");

				my_fclose(fp);
				return (-1);
			}
		}

		/* Copy the line */
		strcpy(output, buf);
	}
	else
	{
		/* Close the file */
		my_fclose(fp);

		return (-1);
	}

	/* Close the file */
	my_fclose(fp);

	/* Success */
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
		(void)strcpy(p_ptr->died_from, "Abortion");

		/* Close stuff */
		close_game();

		/* Quit */
		quit("interrupt");
	}

	/* Allow suicide (after 5) */
	else if (signal_count >= 5)
	{
		/* Cause of "death" */
		(void)strcpy(p_ptr->died_from, "Interrupting");

		/* Stop playing */
		p_ptr->playing = FALSE;

		/* Suicide */
		p_ptr->is_dead = TRUE;

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
	(void)strcpy(p_ptr->died_from, "(panic save)");

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


#else  /* HANDLE_SIGNALS */


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
