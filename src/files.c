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
#ifdef JP
quit("setuid(): 正しく許可が取れません！");
#else
		quit("setuid(): cannot set permissions correctly!");
#endif

	}
	if (setgid(getgid()) != 0)
	{
#ifdef JP
quit("setgid(): 正しく許可が取れません！");
#else
		quit("setgid(): cannot set permissions correctly!");
#endif

	}

#  else

	if (setreuid(geteuid(), getuid()) != 0)
	{
#ifdef JP
quit("setreuid(): 正しく許可が取れません！");
#else
		quit("setreuid(): cannot set permissions correctly!");
#endif

	}
	if (setregid(getegid(), getgid()) != 0)
	{
#ifdef JP
quit("setregid(): 正しく許可が取れません！");
#else
		quit("setregid(): cannot set permissions correctly!");
#endif

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
#ifdef JP
quit("setuid(): 正しく許可が取れません！");
#else
		quit("setuid(): cannot set permissions correctly!");
#endif

	}
	if (setgid(player_egid) != 0)
	{
#ifdef JP
quit("setgid(): 正しく許可が取れません！");
#else
		quit("setgid(): cannot set permissions correctly!");
#endif

	}

#  else

	if (setreuid(geteuid(), getuid()) != 0)
	{
#ifdef JP
quit("setreuid(): 正しく許可が取れません！");
#else
		quit("setreuid(): cannot set permissions correctly!");
#endif

	}
	if (setregid(getegid(), getgid()) != 0)
	{
#ifdef JP
quit("setregid(): 正しく許可が取れません！");
#else
		quit("setregid(): cannot set permissions correctly!");
#endif

	}

#  endif /* SAFE_SETUID_POSIX */

# endif /* SAFE_SETUID */

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
	cptr name;		/* The name of this thing */
	int num;			/* A number associated with it */
};


/* Index of spell type names */
static named_num gf_desc[] =
{
	{"GF_ELEC", 				GF_ELEC				},
	{"GF_POIS", 				GF_POIS				},
	{"GF_ACID", 				GF_ACID				},
	{"GF_COLD", 				GF_COLD				},
	{"GF_FIRE",		 			GF_FIRE				},
	{"GF_PSY_SPEAR",	 		GF_PSY_SPEAR		},
	{"GF_MISSILE",				GF_MISSILE			},
	{"GF_ARROW",				GF_ARROW			},
	{"GF_PLASMA",				GF_PLASMA			},
	{"GF_WATER",				GF_WATER			},
	{"GF_LITE",					GF_LITE				},
	{"GF_DARK",					GF_DARK				},
	{"GF_LITE_WEAK",			GF_LITE_WEAK		},
	{"GF_DARK_WEAK",			GF_DARK_WEAK		},
	{"GF_SHARDS",				GF_SHARDS			},
	{"GF_SOUND",				GF_SOUND			},
	{"GF_CONFUSION",			GF_CONFUSION		},
	{"GF_FORCE",				GF_FORCE			},
	{"GF_INERTIA",				GF_INERTIA			},
	{"GF_MANA",					GF_MANA				},
	{"GF_METEOR",				GF_METEOR			},
	{"GF_ICE",					GF_ICE				},
	{"GF_CHAOS",				GF_CHAOS			},
	{"GF_NETHER",				GF_NETHER			},
	{"GF_DISENCHANT",			GF_DISENCHANT		},
	{"GF_NEXUS",				GF_NEXUS			},
	{"GF_TIME",					GF_TIME				},
	{"GF_GRAVITY",				GF_GRAVITY			},
	{"GF_KILL_WALL",			GF_KILL_WALL		},
	{"GF_KILL_DOOR",			GF_KILL_DOOR		},
	{"GF_KILL_TRAP",			GF_KILL_TRAP		},
	{"GF_MAKE_WALL",			GF_MAKE_WALL		},
	{"GF_MAKE_DOOR",			GF_MAKE_DOOR		},
	{"GF_MAKE_TRAP",			GF_MAKE_TRAP		},
	{"GF_OLD_CLONE",			GF_OLD_CLONE		},
	{"GF_OLD_POLY",				GF_OLD_POLY			},
	{"GF_OLD_HEAL",				GF_OLD_HEAL			},
	{"GF_OLD_SPEED",			GF_OLD_SPEED		},
	{"GF_OLD_SLOW",				GF_OLD_SLOW			},
	{"GF_OLD_CONF",				GF_OLD_CONF			},
	{"GF_OLD_SLEEP",			GF_OLD_SLEEP		},
	{"GF_OLD_DRAIN",			GF_OLD_DRAIN		},
	{"GF_AWAY_UNDEAD",			GF_AWAY_UNDEAD		},
	{"GF_AWAY_EVIL",			GF_AWAY_EVIL		},
	{"GF_AWAY_ALL",				GF_AWAY_ALL			},
	{"GF_TURN_UNDEAD",			GF_TURN_UNDEAD		},
	{"GF_TURN_EVIL",			GF_TURN_EVIL		},
	{"GF_TURN_ALL",				GF_TURN_ALL			},
	{"GF_DISP_UNDEAD",			GF_DISP_UNDEAD		},
	{"GF_DISP_EVIL",			GF_DISP_EVIL		},
	{"GF_DISP_ALL",				GF_DISP_ALL			},
	{"GF_DISP_DEMON",			GF_DISP_DEMON		},
	{"GF_DISP_LIVING",			GF_DISP_LIVING		},
	{"GF_ROCKET",				GF_ROCKET			},
	{"GF_NUKE",					GF_NUKE				},
	{"GF_MAKE_GLYPH",			GF_MAKE_GLYPH		},
	{"GF_STASIS",				GF_STASIS			},
	{"GF_STONE_WALL",			GF_STONE_WALL		},
	{"GF_DEATH_RAY",			GF_DEATH_RAY		},
	{"GF_STUN",					GF_STUN				},
	{"GF_HOLY_FIRE",			GF_HOLY_FIRE		},
	{"GF_HELL_FIRE",			GF_HELL_FIRE		},
	{"GF_DISINTEGRATE",			GF_DISINTEGRATE		},
	{"GF_CHARM",				GF_CHARM			},
	{"GF_CONTROL_UNDEAD",		GF_CONTROL_UNDEAD	},
	{"GF_CONTROL_ANIMAL",		GF_CONTROL_ANIMAL	},
	{"GF_PSI",					GF_PSI				},
	{"GF_PSI_DRAIN",			GF_PSI_DRAIN		},
	{"GF_TELEKINESIS",			GF_TELEKINESIS		},
	{"GF_JAM_DOOR",				GF_JAM_DOOR			},
	{"GF_DOMINATION",			GF_DOMINATION		},
	{"GF_DISP_GOOD",			GF_DISP_GOOD		},
	{"GF_JUMP",					GF_JUMP				},
	{"GF_JUMP_ATTACK",			GF_JUMP_ATTACK		},
	{"GF_GENOCIDE",				GF_GENOCIDE			},
	{"GF_SOFTEN",				GF_SOFTEN			},
	{NULL, 						0					}
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
 *
 * Specify the set of colors to use when drawing a zapped spell
 *   Z:<type>:<str>
 *
 * Specify a macro trigger template and macro trigger names.
 *   T:<template>:<modifier chr>:<modifier name1>:<modifier name2>:...
 *   T:<trigger>:<keycode>:<shift-keycode>
 *
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


	/* Process "R:<num>:<a>/<c>" -- attr/char for monster races */
	if (buf[0] == 'R')
	{
		if (tokenize(buf+2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			monster_race *r_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= max_r_idx) return (1);
			r_ptr = &r_info[i];
			if (n1 || (!(n2 & 0x80) && n2)) r_ptr->x_attr = n1; /* Allow TERM_DARK text */
			if (n2) r_ptr->x_char = n2;
			return (0);
		}
	}

	/* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
	else if (buf[0] == 'K')
	{
		if (tokenize(buf+2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			object_kind *k_ptr;
			i = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			if (i >= max_k_idx) return (1);
			k_ptr = &k_info[i];
			if (n1 || (!(n2 & 0x80) && n2)) k_ptr->x_attr = n1; /* Allow TERM_DARK text */
			if (n2) k_ptr->x_char = n2;
			return (0);
		}
	}

	/* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
	/* "F:<num>:<a>/<c>" */
	/* "F:<num>:<a>/<c>:LIT" */
	/* "F:<num>:<a>/<c>:<la>/<lc>:<da>/<dc>" */
	else if (buf[0] == 'F')
	{
		feature_type *f_ptr;
		int num = tokenize(buf + 2, F_LIT_MAX * 2 + 1, zz, TOKENIZE_CHECKQUOTE);

		if ((num != 3) && (num != 4) && (num != F_LIT_MAX * 2 + 1)) return 1;
		else if ((num == 4) && !streq(zz[3], "LIT")) return 1;

		i = (huge)strtol(zz[0], NULL, 0);
		if (i >= max_f_idx) return 1;
		f_ptr = &f_info[i];

		n1 = strtol(zz[1], NULL, 0);
		n2 = strtol(zz[2], NULL, 0);
		if (n1 || (!(n2 & 0x80) && n2)) f_ptr->x_attr[F_LIT_STANDARD] = n1; /* Allow TERM_DARK text */
		if (n2) f_ptr->x_char[F_LIT_STANDARD] = n2;

		/* Mega-hack -- feat supports lighting */
		switch (num)
		{
		/* No lighting support */
		case 3:
			n1 = f_ptr->x_attr[F_LIT_STANDARD];
			n2 = f_ptr->x_char[F_LIT_STANDARD];
			for (j = F_LIT_NS_BEGIN; j < F_LIT_MAX; j++)
			{
				f_ptr->x_attr[j] = n1;
				f_ptr->x_char[j] = n2;
			}
			break;

		/* Use default lighting */
		case 4:
			apply_default_feat_lighting(f_ptr->x_attr, f_ptr->x_char);
			break;

		/* Use desired lighting */
		case F_LIT_MAX * 2 + 1:
			for (j = F_LIT_NS_BEGIN; j < F_LIT_MAX; j++)
			{
				n1 = strtol(zz[j * 2 + 1], NULL, 0);
				n2 = strtol(zz[j * 2 + 2], NULL, 0);
				if (n1 || (!(n2 & 0x80) && n2)) f_ptr->x_attr[j] = n1; /* Allow TERM_DARK text */
				if (n2) f_ptr->x_char[j] = n2;
			}
			break;
		}
		return 0;
	}

	/* Process "S:<num>:<a>/<c>" -- attr/char for special things */
	else if (buf[0] == 'S')
	{
		if (tokenize(buf+2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
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
		if (tokenize(buf+2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
		{
			j = (huge)strtol(zz[0], NULL, 0);
			n1 = strtol(zz[1], NULL, 0);
			n2 = strtol(zz[2], NULL, 0);
			for (i = 1; i < max_k_idx; i++)
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
		if (tokenize(buf+2, 2, zz, TOKENIZE_CHECKQUOTE) == 2)
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

		if (tokenize(buf+2, 2, zz, TOKENIZE_CHECKQUOTE) != 2) return (1);

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
		if (tokenize(buf+2, 5, zz, TOKENIZE_CHECKQUOTE) == 5)
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
			int os = option_info[i].o_set;
			int ob = option_info[i].o_bit;

			if ((alive || character_xtra) &&
				(6 == option_info[i].o_page) && !wizard)
			{
#ifdef JP
				msg_format("初期オプションは変更できません! '%s'", buf);
#else
				msg_format("Birth options can not changed! '%s'", buf);
#endif
				msg_print(NULL);
				return 0;
			}

			if (option_info[i].o_var &&
				 option_info[i].o_text &&
				 streq(option_info[i].o_text, buf + 2))
			{
				/* Clear */
				option_flag[os] &= ~(1L << ob);
				(*option_info[i].o_var) = FALSE;
				return (0);
			}
		}
		
		/* XXX XXX XXX - ignore unknown options */
		return (0);
	}

	/* Process "Y:<str>" -- turn option on */
	else if (buf[0] == 'Y')
	{
		for (i = 0; option_info[i].o_desc; i++)
		{
			int os = option_info[i].o_set;
			int ob = option_info[i].o_bit;

			if ((alive || character_xtra) &&
				(6 == option_info[i].o_page) && !wizard)
			{
#ifdef JP
				msg_format("初期オプションは変更できません! '%s'", buf);
#else
				msg_format("Birth options can not changed! '%s'", buf);
#endif
				msg_print(NULL);
				return 0;
			}

			if (option_info[i].o_var &&
				 option_info[i].o_text &&
				 streq(option_info[i].o_text, buf + 2))
			{
				/* Set */
				option_flag[os] |= (1L << ob);
				(*option_info[i].o_var) = TRUE;
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
		char *t = my_strchr(buf + 2, ':');

		/* Oops */
		if (!t) return (1);

		/* Nuke the colon */
		*(t++) = '\0';

		for (i = 0; gf_desc[i].name; i++)
		{
			/* Match this type */
			if (streq(gf_desc[i].name, buf + 2))
			{
				/* Remember this color set */
				gf_color[gf_desc[i].num] = quark_add(t);

				/* Success */
				return (0);
			}
		}
	}

	/* Initialize macro trigger names and a template */
	/* Process "T:<trigger>:<keycode>:<shift-keycode>" */
	/* Process "T:<template>:<modifier chr>:<modifier name>:..." */
	else if (buf[0] == 'T')
	{
		int tok;
		tok = tokenize(buf+2, 2+MAX_MACRO_MOD, zz, 0);

		/* Process "T:<template>:<modifier chr>:<modifier name>:..." */
		if (tok >= 4)
		{
			int i;
			int num;

			if (macro_template != NULL)
			{
				num = strlen(macro_modifier_chr);

				/* Kill the template string */
				string_free(macro_template);
				macro_template = NULL;

				/* Kill flag characters of modifier keys */
				string_free(macro_modifier_chr);

				/* Kill corresponding modifier names */
				for (i = 0; i < num; i++)
				{
					string_free(macro_modifier_name[i]);
				}

				/* Kill trigger name strings */
				for (i = 0; i < max_macrotrigger; i++)
				{
					string_free(macro_trigger_name[i]);
					string_free(macro_trigger_keycode[0][i]);
					string_free(macro_trigger_keycode[1][i]);
				}

				max_macrotrigger = 0;
			}
			
			if (*zz[0] == '\0') return 0; /* clear template */

			/* Number of modifier flags */
			num = strlen(zz[1]);

			/* Limit the number */
			num = MIN(MAX_MACRO_MOD, num);

			/* Stop if number of modifier is not correct */
			if (2 + num != tok) return 1;

			/* Get a template string */
			macro_template = string_make(zz[0]);

			/* Get flag characters of modifier keys */
			macro_modifier_chr = string_make(zz[1]);

			/* Get corresponding modifier names */
			for (i = 0; i < num; i++)
			{
				macro_modifier_name[i] = string_make(zz[2+i]);
			}
		}
		else if (tok >= 2)
		{
			char buf[1024];
			int m;
			char *t, *s;
			if (max_macrotrigger >= MAX_MACRO_TRIG)
			{
#ifdef JP
				msg_print("マクロトリガーの設定が多すぎます!");
#else
				msg_print("Too many macro triggers!");
#endif
				return 1;
			}
			m = max_macrotrigger;
			max_macrotrigger++;

			/* Take into account the escape character  */
			t = buf;
			s = zz[0];
			while (*s)
			{
				if ('\\' == *s) s++;
				*t++ = *s++;
			}
			*t = '\0';

			/* Get a trigger name */
			macro_trigger_name[m] = string_make(buf);

			/* Get the corresponding key code */
			macro_trigger_keycode[0][m] = string_make(zz[1]);

			/* Key code of a combination of it with the shift key */
			if (tok > 2) macro_trigger_keycode[1][m] = string_make(zz[2]);
			else macro_trigger_keycode[1][m] = string_make(zz[1]);
		}

		/* No error */
		return 0;
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
cptr process_pref_file_expr(char **sp, char *fp)
{
	cptr v;

	char *b;
	char *s;

	char b1 = '[';
	char b2 = ']';

	char f = ' ';
	static char tmp[8];

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
			v = "0";
			if (*s && (f != b2))
			{
				t = process_pref_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = process_pref_file_expr(&s, &f);
				if (streq(t, p)) v = "1";
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
				if (*t && (strcmp(p, t) > 0)) v = "0";
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
				if (*t && (strcmp(p, t) < 0)) v = "0";
			}
		}

		/* Oops */
		else
		{
			while (*s && (f != b2))
			{
				(void)process_pref_file_expr(&s, &f);
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
#ifdef JP
		while (iskanji(*s) || (isprint(*s) && !my_strchr(" []", *s)))
		{
			if (iskanji(*s)) s++;
			s++;
		}
#else
		while (isprint(*s) && !my_strchr(" []", *s)) ++s;
#endif

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

			else if (streq(b+1, "KEYBOARD"))
			{
				v = ANGBAND_KEYBOARD;
			}

			/* Graphics */
			else if (streq(b+1, "GRAF"))
			{
				v = ANGBAND_GRAF;
			}

			/* Monochrome mode */
			else if (streq(b+1, "MONOCHROME"))
			{
				if (arg_monochrome)
					v = "ON";
				else
					v = "OFF";
			}

			/* Race */
			else if (streq(b+1, "RACE"))
			{
#ifdef JP
				v = rp_ptr->E_title;
#else
				v = rp_ptr->title;
#endif

			}

			/* Class */
			else if (streq(b+1, "CLASS"))
			{
#ifdef JP
				v = cp_ptr->E_title;
#else
				v = cp_ptr->title;
#endif

			}

			/* First realm */
			else if (streq(b+1, "REALM1"))
			{
#ifdef JP
				v = E_realm_names[p_ptr->realm1];
#else
				v = realm_names[p_ptr->realm1];
#endif
			}

			/* Second realm */
			else if (streq(b+1, "REALM2"))
			{
#ifdef JP
				v = E_realm_names[p_ptr->realm2];
#else
				v = realm_names[p_ptr->realm2];
#endif
			}

			/* Player */
			else if (streq(b+1, "PLAYER"))
			{
				static char tmp_player_name[32];
				char *pn, *tpn;
				for (pn = player_name, tpn = tmp_player_name; *pn; pn++, tpn++)
				{
#ifdef JP
					if (iskanji(*pn))
					{
						*(tpn++) = *(pn++);
						*tpn = *pn;
						continue;
					}
#endif
					*tpn = my_strchr(" []", *pn) ? '_' : *pn;
				}
				*tpn = '\0';
				v = tmp_player_name;
			}

			/* Level */
			else if (streq(b+1, "LEVEL"))
			{
				sprintf(tmp, "%02d", p_ptr->lev);
				v = tmp;
			}

			/* Autopick auto-register is in-use or not? */
			else if (streq(b+1, "AUTOREGISTER"))
			{
				if (p_ptr->autopick_autoregister)
					v = "1";
				else
					v = "0";
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
static errr process_pref_file_aux(cptr name, bool read_pickpref)
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
			static int depth_count = 0;

			/* Ignore if deeper than 20 level */
			if (depth_count > 20) continue;

			/* Count depth level */
			depth_count++;

			/* Process that file if allowed */
			if (read_pickpref) (void)process_autopick_file(buf + 2);
			else (void)process_pref_file(buf + 2);

			/* Set back depth level */
			depth_count--;

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_pref_file_command(buf);

		/* This is not original pref line... */
		if (err)
		{
			if (!read_pickpref) break;
			else err = process_autopick_file_command(buf);
		}
	}


	/* Error */
	if (err)
	{
		/* Print error message */
		/* ToDo: Add better error messages */
#ifdef JP
	      msg_format("ファイル'%s'の%d行でエラー番号%dのエラー.", name, line, err);
	      msg_format("('%s'を解析中)", old);
#else
		msg_format("Error %d in line %d of file '%s'.", err, line, name);
		msg_format("Parsing '%s'", old);
#endif
		msg_print(NULL);
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

	errr err;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_PREF, name);

	/* Process the pref file */
	err = process_pref_file_aux(buf, FALSE);

	/* Stop at parser errors, but not at non-existing file */
	if (err < 1)
	{
		/* Build the filename */
		path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

		/* Process the pref file */
		err = process_pref_file_aux(buf, FALSE);
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

	time_t      c;
	struct tm   *tp;

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
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "time.txt");

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

#endif

	/* Success */
	return (0);
}


#define ENTRY_TOHIT 0
#define ENTRY_TOHIT_R 1
#define ENTRY_TODAM 4
#define ENTRY_SHOOT_TOHIT 7
#define ENTRY_SHOOT_POWER 8
#define ENTRY_SPEED 9
#define ENTRY_BASE_AC 10
#define ENTRY_LEVEL 11
#define ENTRY_CUR_EXP 12
#define ENTRY_MAX_EXP 13
#define ENTRY_EXP_TO_ADV 14
#define ENTRY_GOLD 15
#define ENTRY_DAY 16
#define ENTRY_HP 17
#define ENTRY_SP 18
#define ENTRY_PLAY_TIME 19
#define ENTRY_SKILL_FIGHT 20
#define ENTRY_SKILL_SHOOT 21
#define ENTRY_SKILL_SAVING 22
#define ENTRY_SKILL_STEALTH 23
#define ENTRY_SKILL_PERCEP 24
#define ENTRY_SKILL_SEARCH 25
#define ENTRY_SKILL_DISARM 26
#define ENTRY_SKILL_DEVICE 27
#define ENTRY_BLOWS 28
#define ENTRY_SHOTS 29
#define ENTRY_AVG_DMG 30
#define ENTRY_INFRA 31
#define ENTRY_NAME 32
#define ENTRY_SEX 33
#define ENTRY_RACE 34
#define ENTRY_CLASS 35
#define ENTRY_REALM 36
#define ENTRY_PATRON 37
#define ENTRY_AGE 38
#define ENTRY_HEIGHT 39
#define ENTRY_WEIGHT 40
#define ENTRY_SOCIAL 41
#define ENTRY_ALIGN 42


static struct
{
	int col;
	int row;
	int len;
	char header[20];
} disp_player_line[]
#ifdef JP
= {
	{ 1, 10, 25, "打撃修正"},
	{ 1, 10, 25, "打撃修正(右手)"},
	{ 1, 10, 25, ""},
	{ 1, 10, 25, ""},
	{ 1, 11, 25, "打撃修正(左手)"},
	{ 1, 11, 25, ""},
	{ 1, 11, 25, ""},
	{ 1, 15, 25, "射撃修正"},
	{ 1, 16, 25, "射撃倍率"},
	{01, 20, 25, "加速"},
	{ 1, 19, 25, "ＡＣ"},
	{29, 13, 21, "レベル"},
	{29, 14, 21, "経験値"},
	{29, 15, 21, "最大経験"},
	{29, 16, 21, "次レベル"},
	{29, 17, 21, "所持金"},
	{29, 19, 21, "日付"},
	{29, 10, 21, "ＨＰ"},
	{29, 11, 21, "ＭＰ"},
	{29, 20, 21, "プレイ時間"},
	{54, 10, -1, "打撃攻撃  :"},
	{54, 11, -1, "射撃攻撃  :"},
	{54, 12, -1, "魔法防御  :"},
	{54, 13, -1, "隠密行動  :"},
	{54, 15, -1, "知覚      :"},
	{54, 16, -1, "探索      :"},
	{54, 17, -1, "解除      :"},
	{54, 18, -1, "魔法道具  :"},
	{01, 12, 25, "打撃回数"},
	{01, 17, 25, "射撃回数"},
	{01, 13, 25, "平均ダメージ"},
	{54, 20, -1, "赤外線視力:"},
	{26,  1, -1, "名前  : "},
	{ 1,  3, -1, "性別     : "},
	{ 1,  4, -1, "種族     : "},
	{ 1,  5, -1, "職業     : "},
	{ 1,  6, -1, "魔法     : "},
	{ 1,  7, -1, "守護神   : "},
	{29,  3, 21, "年齢"},
	{29,  4, 21, "身長"},
	{29,  5, 21, "体重"},
	{29,  6, 21, "社会的地位"},
	{29,  7, 21, "属性"},
};
#else
= {
	{ 1, 10, 25, "+ Melee"},
	{ 1, 10, 25, ""},
	{ 1, 10, 25, ""},
	{ 1, 10, 25, ""},
	{ 1, 11, 25, ""},
	{ 1, 11, 25, ""},
	{ 1, 11, 25, ""},
	{ 1, 15, 25, "+ Shooting"},
	{ 1, 16, 25, "Multiplier"},
	{01, 20, 25, "Speed"},
	{ 1, 19, 25, "AC"},
	{29, 13, 21, "Level"},
	{29, 14, 21, "Experience"},
	{29, 15, 21, "Max Exp"},
	{29, 16, 21, "Exp to Adv"},
	{29, 17, 21, "Gold"},
	{29, 19, 21, "Time"},
	{29, 10, 21, "Hit point"},
	{29, 11, 21, "SP (Mana)"},
	{29, 20, 21, "Play time"},
	{54, 10, -1, "Fighting    : "},
	{54, 11, -1, "Bows/Throw  : "},
	{54, 12, -1, "Saving Throw: "},
	{54, 13, -1, "Stealth     : "},
	{54, 15, -1, "Perception  : "},
	{54, 16, -1, "Searching   : "},
	{54, 17, -1, "Disarming   : "},
	{54, 18, -1, "Magic Device: "},
	{01, 12, 25, "Blows/Round"},
	{01, 17, 25, "Shots/Round"},
	{01, 13, 25, "AverageDmg/Rnd"},
	{54, 20, -1, "Infra-Vision: "},
	{26,  1, -1, "Name  : "},
	{ 1,  3, -1, "Sex      : "},
	{ 1,  4, -1, "Race     : "},
	{ 1,  5, -1, "Class    : "},
	{ 1,  6, -1, "Magic    : "},
	{ 1,  7, -1, "Patron   : "},
	{29,  3, 21, "Age"},
	{29,  4, 21, "Height"},
	{29,  5, 21, "Weight"},
	{29,  6, 21, "Social Class"},
	{29,  7, 21, "Align"},
};
#endif

static void display_player_one_line(int entry, cptr val, byte attr)
{
	char buf[40];

	int row = disp_player_line[entry].row;
	int col = disp_player_line[entry].col;
	int len = disp_player_line[entry].len;
	cptr head = disp_player_line[entry].header;

	int head_len = strlen(head);

	Term_putstr(col, row, -1, TERM_WHITE, head);

	if (!val)
		return;

	if (len > 0)
	{
		int val_len = len - head_len;
		sprintf(buf, "%*.*s", val_len, val_len, val);
		Term_putstr(col + head_len, row, -1, attr, buf);
	}
	else
	{
		Term_putstr(col + head_len, row, -1, attr, val);
	}

	return;
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
#ifdef JP
		return ("最低");
#else
		return ("Very Bad");
#endif
	}

	/* Analyze the value */
	switch ((x / y))
	{
		case 0:
		case 1:
		{
			likert_color = TERM_RED;
#ifdef JP
			return ("悪い");
#else
			return ("Bad");
#endif
		}
		case 2:
		{
			likert_color = TERM_L_RED;
#ifdef JP
			return ("劣る");
#else
			return ("Poor");
#endif
		}
		case 3:
		case 4:
		{
			likert_color = TERM_ORANGE;
#ifdef JP
			return ("普通");
#else
			return ("Fair");
#endif
		}
		case 5:
		{
			likert_color = TERM_YELLOW;
#ifdef JP
			return ("良い");
#else
			return ("Good");
#endif
		}
		case 6:
		{
			likert_color = TERM_YELLOW;
#ifdef JP
			return ("大変良い");
#else
			return ("Very Good");
#endif
		}
		case 7:
		case 8:
		{
			likert_color = TERM_L_GREEN;
#ifdef JP
			return ("卓越");
#else
			return ("Excellent");
#endif
		}
		case 9:
		case 10:
		case 11:
		case 12:
		case 13:
		{
			likert_color = TERM_GREEN;
#ifdef JP
			return ("超越");
#else
			return ("Superb");
#endif
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			likert_color = TERM_BLUE;
#ifdef JP
			return ("英雄的");
#else
			return ("Heroic");
#endif
		}
		default:
		{
			likert_color = TERM_L_BLUE;
#ifdef JP
			sprintf(dummy,"伝説的 [%d]", (int) ((((x / y) -17) * 5) / 2));
#else
			sprintf(dummy,"Legendary [%d]", (int)((((x / y) - 17) * 5) / 2));
#endif
			return dummy;
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
	int		i;
	int		tmp, dambonus, blows[2];
	int		xthn, xthb, xfos, xsrh;
	int		xdis, xdev, xsav, xstl;
	cptr	desc;
	long	avgdam[2];
	u32b	f1, f2, f3;
	int		shots, shot_frac;

	object_type		*o_ptr;

	/* Shooting Skill (with current bow and normal missile) */
	o_ptr = &inventory[INVEN_BOW];
	tmp = p_ptr->to_b + o_ptr->to_h;
	xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

	/* If the player is wielding one? */
	if (o_ptr->k_idx)
	{
		int energy_fire = bow_energy(o_ptr->sval);

		/* Calculate shots per round */
		shots = p_ptr->num_fire;
		shot_frac = (shots * 100 / energy_fire) % 100;
		shots = shots / energy_fire;
	}
	else
	{
		shots = 0;
		shot_frac = 0;
	}

	/* Fighting Skill (with current weapon) */
	for (i = 0; i < 2; i++)
	{
		if ((i == 1) && !is_two_handed()) break;

		o_ptr = &inventory[INVEN_WIELD + i];

		tmp = p_ptr->to_h + (o_ptr->to_h + calc_melee_bonus(i));
		xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

		/* Average damage per round */
		blows[i] = p_ptr->num_blow[i];
		dambonus = p_ptr->dis_to_d;
		if (object_known_p(o_ptr)) dambonus += o_ptr->to_d;
		dambonus *= (blows[i] * 100);
		avgdam[i] = blows[i] * (o_ptr->dd * (o_ptr->ds + 1)) * 50;

		/* see if have a weapon with extra power*/
		if (o_ptr->k_idx)
		{
			/* Is there a vorpal effect we know about? */
			object_flags(o_ptr, &f1, &f2, &f3);
			if (object_known_p(o_ptr) && (f1 & TR1_VORPAL))
			{
				/* vorpal flag only */
				avgdam[i] *= 11;
				avgdam[i] /= 9;
			}
		}
		else
			avgdam[i] = 0;

		avgdam[i] += dambonus;
		if (avgdam[i] < 0) avgdam[i] = 0;
	}

	/* Basic abilities */

	xdis = p_ptr->skill_dis;
	xdev = p_ptr->skill_dev;
	xsav = p_ptr->skill_sav;
	xstl = p_ptr->skill_stl;
	xsrh = p_ptr->skill_srh;
	xfos = p_ptr->skill_fos;


	desc = likert(xthn, 10);
	display_player_one_line(ENTRY_SKILL_FIGHT, desc, likert_color);

	desc = likert(xthb, 10);
	display_player_one_line(ENTRY_SKILL_SHOOT, desc, likert_color);

	desc = likert(xsav, 6);
	display_player_one_line(ENTRY_SKILL_SAVING, desc, likert_color);

	desc = likert(xstl, 1);
	display_player_one_line(ENTRY_SKILL_STEALTH, desc, likert_color);

	desc = likert(xfos, 6);
	display_player_one_line(ENTRY_SKILL_PERCEP, desc, likert_color);

	desc = likert(xsrh, 6);
	display_player_one_line(ENTRY_SKILL_SEARCH, desc, likert_color);

	desc = likert(xdis, 8);
	display_player_one_line(ENTRY_SKILL_DISARM, desc, likert_color);

	desc = likert(xdev, 6);
	display_player_one_line(ENTRY_SKILL_DEVICE, desc, likert_color);

	if (!is_two_handed())
		display_player_one_line(ENTRY_BLOWS, format("%d", blows[0]), TERM_L_BLUE);
	else
		display_player_one_line(ENTRY_BLOWS, format("%d+%d", blows[0], blows[1]), TERM_L_BLUE);

	display_player_one_line(ENTRY_SHOTS, format("%d.%02d", shots, shot_frac), TERM_L_BLUE);

	if (!is_two_handed())
		desc = format("%d", (avgdam[0] / 100));
	else
		desc = format("%d+%d", (avgdam[0] / 100), (avgdam[1] / 100));
	display_player_one_line(ENTRY_AVG_DMG, desc, TERM_L_BLUE);

	display_player_one_line(ENTRY_INFRA, format("%d feet", p_ptr->see_infra * 10), TERM_WHITE);
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
		if (p_ptr->lev > 44)
			(*f3) |= (TR3_REGEN);
		break;
	case CLASS_PALADIN:
		if (p_ptr->lev > 39)
			(*f2) |= (TR2_RES_FEAR);
		break;
	default:
		; /* Do nothing */
	}

	/* Races */
	switch (p_ptr->prace)
	{
	case RACE_HOBBIT:
		(*f2) |= (TR2_SUST_DEX);
		break;
	case RACE_DWARF:
		(*f2) |= (TR2_RES_BLIND);
		break;
	case RACE_HALF_ORC:
		(*f2) |= (TR2_RES_DARK);
		break;
	case RACE_DUNADAN:
		(*f2) |= (TR2_SUST_CON);
		(*f3) |= (TR3_REGEN); /* Dunadans heal fast */
		break;
	case RACE_HIGH_ELF:
		(*f3) |= (TR3_SEE_INVIS);
		break;
	case RACE_BARBARIAN:
		(*f2) |= (TR2_RES_FEAR);
		break;
	default:
		; /* Do nothing */
	}

	/* Mutations */
	if (p_ptr->muta)
	{
		if (p_ptr->muta & MUT_FLESH_ROT)
		{
			(*f3) &= ~(TR3_REGEN);
		}

		if ((p_ptr->muta & MUT_XTRA_FAT) ||
			(p_ptr->muta & MUT_XTRA_LEGS) ||
			(p_ptr->muta & MUT_SHORT_LEG))
		{
			(*f1) |= TR1_SPEED;
		}

		if (p_ptr->muta  & MUT_ELEC_TOUC)
		{
			(*f3) |= TR3_SH_ELEC;
			(*f2) |= TR2_RES_ELEC;
		}

		if (p_ptr->muta & MUT_FIRE_BODY)
		{
			(*f3) |= TR3_SH_FIRE;
			(*f2) |= TR2_RES_FIRE;
			(*f3) |= TR3_LITE;
		}

		if (p_ptr->muta & MUT_WINGS)
		{
			(*f3) |= TR3_FEATHER;
		}

		if (p_ptr->muta & MUT_FEARLESS)
		{
			(*f2) |= (TR2_RES_FEAR);
		}

		if (p_ptr->muta & MUT_REGEN)
		{
			(*f3) |= TR3_REGEN;
		}

		if (p_ptr->muta & MUT_ESP)
		{
			(*f3) |= TR3_TELEPATHY;
		}

		if (p_ptr->muta & MUT_MOTION)
		{
			(*f2) |= TR2_FREE_ACT;
		}
	}

	/* Valar */
	switch (p_ptr->valar_patron)
	{
	case VAR_MANWE:
		(*f2) |= TR2_RES_ELEC;
		(*f2) |= TR2_FREE_ACT;
		if (p_ptr->lev >= 30) (*f2) |= TR2_IM_ELEC;
		break;
	case VAR_ULMO:
		(*f2) |= TR2_RES_FIRE;
		if (p_ptr->lev >= 25) (*f2) |= TR2_RES_SOUND;
		break;
	case VAR_AULE:
		/* Nothing */
		break;
	case VAR_OROME:
		(*f1) |= TR1_SPEED;
		break;
	case VAR_MANDOS:
		(*f2) |= TR2_HOLD_LIFE;
		(*f3) |= TR3_SEE_INVIS;
		break;
	case VAR_IRMO:
		/* Nothing */
		break;
	case VAR_TULKAS:
		/* To-Dam and To-Hit */
		break;
	case VAR_VARDA:
		(*f3) |= TR3_LITE;
		break;
	case VAR_YAVANNA:
		(*f2) |= TR2_RES_ACID;
		if (p_ptr->lev >= 30) (*f2) |= TR2_REFLECT;
		break;
	case VAR_NIENNA:
		(*f3) |= TR3_WARNING;
		if (p_ptr->lev >= 25) (*f2) |= TR2_RES_CONF;
		break;
	case VAR_ESTE:
		(*f2) |= TR2_SUST_CON;
		if (p_ptr->lev >= 20) (*f2) |= TR2_RES_BLIND;
		break;
	case VAR_VAIRE:
		(*f1) |= TR1_STEALTH;
		(*f2) |= TR2_RES_COLD;
		break;
	case VAR_VANA:
		(*f3) |= TR3_REGEN;
		(*f3) |= TR3_SLOW_DIGEST;
		break;
	case VAR_NESSA:
		(*f1) |= TR1_STEALTH;
		break;
	}
}


static void tim_player_flags(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	if (p_ptr->hero || p_ptr->shero)
		(*f2) |= TR2_RES_FEAR;
	if (p_ptr->tim_invis)
		(*f3) |= TR3_SEE_INVIS;
	if (p_ptr->tim_regen)
		(*f3) |= TR3_REGEN;
	if (p_ptr->tim_esp)
		(*f3) |= TR3_TELEPATHY;
	if (p_ptr->tim_sh_elec)
		(*f3) |= TR3_SH_ELEC;
	if (p_ptr->tim_sh_fire)
		(*f3) |= TR3_SH_FIRE;
	if (p_ptr->tim_sh_cold)
		(*f3) |= TR3_SH_COLD;
	if (p_ptr->fast || p_ptr->slow)
		(*f1) |= TR1_SPEED;
	if (p_ptr->oppose_acid)
		(*f2) |= TR2_RES_ACID;
	if (p_ptr->oppose_elec)
		(*f2) |= TR2_RES_ELEC;
	if (p_ptr->oppose_fire)
		(*f2) |= TR2_RES_FIRE;
	if (p_ptr->oppose_cold)
		(*f2) |= TR2_RES_COLD;
	if (p_ptr->oppose_pois)
		(*f2) |= TR2_RES_POIS;
	if (p_ptr->wraith_form)
	{
		(*f2) |= TR2_REFLECT;
		(*f3) |= TR3_WRAITH;
	}
	if (p_ptr->magicdef)
	{
		(*f2) |= TR2_RES_BLIND;
		(*f2) |= TR2_RES_CONF;
		(*f2) |= TR2_REFLECT;
		(*f2) |= TR2_FREE_ACT;
		(*f3) |= TR3_FEATHER;
	}
	if (p_ptr->musou)
	{
		(*f2) |= TR2_RES_FEAR;
		(*f2) |= TR2_RES_LITE;
		(*f2) |= TR2_RES_DARK;
		(*f2) |= TR2_RES_BLIND;
		(*f2) |= TR2_RES_CONF;
		(*f2) |= TR2_RES_SOUND;
		(*f2) |= TR2_RES_SHARDS;
		(*f2) |= TR2_RES_NETHER;
		(*f2) |= TR2_RES_NEXUS;
		(*f2) |= TR2_RES_CHAOS;
		(*f2) |= TR2_RES_DISEN;
		(*f2) |= TR2_REFLECT;
		(*f2) |= TR2_HOLD_LIFE;
		(*f2) |= TR2_FREE_ACT;
		(*f3) |= TR3_SH_FIRE;
		(*f3) |= TR3_SH_ELEC;
		(*f3) |= TR3_SH_COLD;
		(*f3) |= TR3_FEATHER;
		(*f3) |= TR3_LITE;
		(*f3) |= TR3_SEE_INVIS;
		(*f3) |= TR3_TELEPATHY;
		(*f3) |= TR3_SLOW_DIGEST;
		(*f3) |= TR3_REGEN;
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
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Object */
		o_ptr = &inventory[i];

#if 0
		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;


		/* Get attr/char for display */
		a = tval_to_attr[o_ptr->tval & 0x7F];
		c = tval_to_char[o_ptr->tval & 0x7F];
#else
		a = object_attr(o_ptr);
		c = object_char(o_ptr);
#endif

		/* No color */
		if (!use_color) a = TERM_WHITE;

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
	display_player_equippy(ROW_EQUIPPY, COL_EQUIPPY);
}


static void known_obj_immunity(u32b *f1, u32b *f2, u32b *f3)
{
	int i;

	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Check equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b    o_f1, o_f2, o_f3;

		object_type *o_ptr;

		/* Object */
		o_ptr = &inventory[i];

		if (!o_ptr->k_idx) continue;

		/* Known flags */
		object_flags_known(o_ptr, &o_f1, &o_f2, &o_f3);

		if (o_f2 & TR2_IM_ACID) (*f2) |= TR2_RES_ACID;
		if (o_f2 & TR2_IM_ELEC) (*f2) |= TR2_RES_ELEC;
		if (o_f2 & TR2_IM_FIRE) (*f2) |= TR2_RES_FIRE;
		if (o_f2 & TR2_IM_COLD) (*f2) |= TR2_RES_COLD;
	}
}

static void player_immunity(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;
#if 0
	if (p_ptr->prace == RACE_SPECTRE)
		(*f2) |= TR2_RES_NETHER;
	if (p_ptr->prace == RACE_VAMPIRE)
		(*f2) |= TR2_RES_DARK;
#endif
}

static void tim_player_immunity(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	if (p_ptr->wraith_form)
		(*f2) |= TR2_RES_DARK;
}

static void player_vuln_flags(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	if (p_ptr->muta & MUT_VULN_ELEM)
	{
		(*f2) |= TR2_RES_ACID;
		(*f2) |= TR2_RES_ELEC;
		(*f2) |= TR2_RES_FIRE;
		(*f2) |= TR2_RES_COLD;
	}
#if 0
	if (p_ptr->prace == RACE_VAMPIRE)
		(*f2) |= TR2_RES_LITE;
#endif
}

/*
 * Helper function, see below
 */
static void display_player_flag_aux(int row, int col, cptr header,
				    int n, u32b flag1, u32b flag2,
				    u32b im_f[], u32b vul_f)
{
	int     i;
	u32b    f[3];
	bool    vuln = FALSE;

	if ((vul_f & flag1) && !((im_f[0] | im_f[1] | im_f[2]) & flag1))
		vuln = TRUE;

	/* Header */
	c_put_str(TERM_WHITE, header, row, col);

	/* Advance */
	col += strlen(header) + 1;

	/* Check equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr;
		f[0] = f[1] = f[2] = 0L;

		/* Object */
		o_ptr = &inventory[i];

		/* Known flags */
		object_flags_known(o_ptr, &f[0], &f[1], &f[2]);

		/* Default */
		c_put_str((byte)(vuln ? TERM_RED : TERM_SLATE), ".", row, col);

		/* Check flags */
		if (f[n - 1] & flag1) c_put_str((byte)(vuln ? TERM_L_RED : TERM_WHITE), "+", row, col);
		if (f[n - 1] & flag2) c_put_str(TERM_WHITE, "*", row, col);

		/* Advance */
		col++;
	}

	/* Player flags */
	player_flags(&f[0], &f[1], &f[2]);

	/* Default */
	c_put_str((byte)(vuln ? TERM_RED : TERM_SLATE), ".", row, col);

	/* Check flags */
	if (f[n-1] & flag1) c_put_str((byte)(vuln ? TERM_L_RED : TERM_WHITE), "+", row, col);

	/* Tim player flags */
	tim_player_flags(&f[0], &f[1], &f[2]);

	/* Check flags */
	if (f[n - 1] & flag1) c_put_str((byte)(vuln ? TERM_ORANGE : TERM_YELLOW), "#", row, col);

	/* Immunity */
	if (im_f[2] & flag1) c_put_str(TERM_YELLOW, "*", row, col);
	if (im_f[1] & flag1) c_put_str(TERM_WHITE, "*", row, col);

	/* Vulnerability */
	if (vuln) c_put_str(TERM_RED, "v", row, col + 1);
}


/*
 * Special display, part 1
 */
static void display_player_flag_info(void)
{
	int row;
	int col;

	u32b im_f[3][3], vul_f[3];

	known_obj_immunity(&im_f[0][0], &im_f[1][0], &im_f[2][0]);
	player_immunity(&im_f[0][1], &im_f[1][1], &im_f[2][1]);
	tim_player_immunity(&im_f[0][2], &im_f[1][2], &im_f[2][2]);

	player_vuln_flags(&vul_f[0], &vul_f[1], &vul_f[2]);

	/*** Set 1 ***/
#ifdef TINYANGBAND
	row = 10;
	col = 1;

	c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 8);

#ifdef JP
	display_player_flag_aux(row++, col, "耐酸  :", 2, TR2_RES_ACID, TR2_IM_ACID, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐電撃:", 2, TR2_RES_ELEC, TR2_IM_ELEC, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐火炎:", 2, TR2_RES_FIRE, TR2_IM_FIRE, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐冷気:", 2, TR2_RES_COLD, TR2_IM_COLD, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐毒  :", 2, TR2_RES_POIS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐恐怖:", 2, TR2_RES_FEAR, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐暗黒:", 2, TR2_RES_DARK, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐盲目:", 2, TR2_RES_BLIND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐混乱:", 2, TR2_RES_CONF, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐轟音:", 2, TR2_RES_SOUND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐地獄:", 2, TR2_RES_NETHER, 0, im_f[1], vul_f[1]);
#else
	display_player_flag_aux(row++, col, "Acid  :", 2, TR2_RES_ACID, TR2_IM_ACID, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Elec  :", 2, TR2_RES_ELEC, TR2_IM_ELEC, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Fire  :", 2, TR2_RES_FIRE, TR2_IM_FIRE, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Cold  :", 2, TR2_RES_COLD, TR2_IM_COLD, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Poison:", 2, TR2_RES_POIS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Fear  :", 2, TR2_RES_FEAR, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Dark  :", 2, TR2_RES_DARK, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Blind :", 2, TR2_RES_BLIND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Conf  :", 2, TR2_RES_CONF, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Sound :", 2, TR2_RES_SOUND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Nether:", 2, TR2_RES_NETHER, 0, im_f[1], vul_f[1]);
#endif
#else
	row = 5;
	col = 1;

	c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 8);

#ifdef JP
	display_player_flag_aux(row++, col, "耐酸  :", 2, TR2_RES_ACID, TR2_IM_ACID, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐電撃:", 2, TR2_RES_ELEC, TR2_IM_ELEC, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐火炎:", 2, TR2_RES_FIRE, TR2_IM_FIRE, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐冷気:", 2, TR2_RES_COLD, TR2_IM_COLD, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐毒  :", 2, TR2_RES_POIS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐恐怖:", 2, TR2_RES_FEAR, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐閃光:", 2, TR2_RES_LITE, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐暗黒:", 2, TR2_RES_DARK, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐破片:", 2, TR2_RES_SHARDS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐盲目:", 2, TR2_RES_BLIND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐混乱:", 2, TR2_RES_CONF, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐轟音:", 2, TR2_RES_SOUND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐地獄:", 2, TR2_RES_NETHER, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐因混:", 2, TR2_RES_NEXUS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐カオ:", 2, TR2_RES_CHAOS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "耐劣化:", 2, TR2_RES_DISEN, 0, im_f[1], vul_f[1]);
#else
	display_player_flag_aux(row++, col, "Acid  :", 2, TR2_RES_ACID, TR2_IM_ACID, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Elec  :", 2, TR2_RES_ELEC, TR2_IM_ELEC, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Fire  :", 2, TR2_RES_FIRE, TR2_IM_FIRE, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Cold  :", 2, TR2_RES_COLD, TR2_IM_COLD, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Poison:", 2, TR2_RES_POIS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Fear  :", 2, TR2_RES_FEAR, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Light :", 2, TR2_RES_LITE, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Dark  :", 2, TR2_RES_DARK, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Shard :", 2, TR2_RES_SHARDS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Blind :", 2, TR2_RES_BLIND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Conf  :", 2, TR2_RES_CONF, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Sound :", 2, TR2_RES_SOUND, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Nether:", 2, TR2_RES_NETHER, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Nexus :", 2, TR2_RES_NEXUS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Chaos :", 2, TR2_RES_CHAOS, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Disnch:", 2, TR2_RES_DISEN, 0, im_f[1], vul_f[1]);
#endif
#endif

	/*** Set 2 ***/

	row = 10;
	col = 26;

	c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 10);

#ifdef JP
	display_player_flag_aux(row++, col, "加速    :", 1, TR1_SPEED, 0, im_f[0], vul_f[0]);
	display_player_flag_aux(row++, col, "反射    :", 2, TR2_REFLECT, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "魔法道具:", 1, TR1_MAGIC_MASTERY, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "火オーラ:", 3, TR3_SH_FIRE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "電オーラ:", 3, TR3_SH_ELEC, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "冷オーラ:", 3, TR3_SH_COLD, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "魔力減少:", 3, TR3_DEC_MANA, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "乱テレポ:", 3, TR3_TELEPORT, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "反テレポ:", 3, TR3_NO_TELE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "反魔法  :", 3, TR3_NO_MAGIC, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "経験吸収:", 3, TR3_DRAIN_EXP, 0, im_f[2], vul_f[2]);
#else
	display_player_flag_aux(row++, col, "Speed   :", 1, TR1_SPEED, 0, im_f[0], vul_f[0]);
	display_player_flag_aux(row++, col, "Reflect :", 2, TR2_REFLECT, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "MagicDev:", 1, TR1_MAGIC_MASTERY, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "AuraFire:", 3, TR3_SH_FIRE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "AuraElec:", 3, TR3_SH_ELEC, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "AuraCold:", 3, TR3_SH_COLD, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "Dec.mana:", 3, TR3_DEC_MANA, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "Teleport:", 3, TR3_TELEPORT, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "NoTelprt:", 3, TR3_NO_TELE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "No Magic:", 3, TR3_NO_MAGIC, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "DrainExp:", 3, TR3_DRAIN_EXP, 0, im_f[2], vul_f[2]);
#endif


	/*** Set 3 ***/

	row = 10;
	col = 53;

	c_put_str(TERM_WHITE, "abcdefghijkl@", row++, col + 11);

#ifdef JP
	display_player_flag_aux(row++, col, "耐麻痺   :", 2, TR2_FREE_ACT, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "透明視認 :", 3, TR3_SEE_INVIS, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "生命保持 :", 2, TR2_HOLD_LIFE, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "テレパシ :", 3, TR3_TELEPATHY, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "警告     :", 3, TR3_WARNING, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "遅消化   :", 3, TR3_SLOW_DIGEST, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "急回復   :", 3, TR3_REGEN, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "浮遊     :", 3, TR3_FEATHER, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "永遠光源 :", 3, TR3_LITE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "反感     :", 3, TR3_AGGRAVATE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "邪悪呪   :", 3,
#else
	display_player_flag_aux(row++, col, "Free Actn:", 2, TR2_FREE_ACT, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "SeeInvis.:", 3, TR3_SEE_INVIS, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "Hold Life:", 2, TR2_HOLD_LIFE, 0, im_f[1], vul_f[1]);
	display_player_flag_aux(row++, col, "Telepathy:", 3, TR3_TELEPATHY, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "Warning  :", 3, TR3_WARNING, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "SlwDigstn:", 3, TR3_SLOW_DIGEST, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "Regen.   :", 3, TR3_REGEN, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "Levitate :", 3, TR3_FEATHER, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "PermLite :", 3, TR3_LITE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "Aggravate:", 3, TR3_AGGRAVATE, 0, im_f[2], vul_f[2]);
	display_player_flag_aux(row++, col, "EvilCurse:", 3,
#endif
		(TR3_CURSED | TR3_HEAVY_CURSE), (TR3_TY_CURSE | TR3_PERMA_CURSE), im_f[2], vul_f[2]);
}

#if 0
/*
 * Special display, part 2a
 */
static void display_player_misc_info(void)
{
	char	buf[80];

	/* Display basics */
#ifdef JP
	put_str("名前      :", 2, 1);
	put_str("性別      :", 3, 1);
	put_str("種族      :", 4, 1);
	put_str("職業      :", 5, 1);
#else
	put_str("Name      :", 2, 1);
	put_str("Sex       :", 3, 1);
	put_str("Race      :", 4, 1);
	put_str("Class     :", 5, 1);
#endif

	c_put_str(TERM_L_BLUE, player_name, 2, 13);
	c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 13);
	c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 13);
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 13);

	/* Display extras */
#ifdef JP
	put_str("レベル    :", 6, 1);
	put_str("ＨＰ(最大):", 7, 1);
	put_str("ＭＰ(最大):", 8, 1);
#else
	put_str("Level     :", 6, 1);
	put_str("Hits(Max) :", 7, 1);
	put_str("Mana(Max) :", 8, 1);
#endif


	(void)sprintf(buf, "%d", (int)p_ptr->lev);
	c_put_str(TERM_L_BLUE, buf, 6, 13);
	(void)sprintf(buf, "%d(%d)", (int)p_ptr->chp, (int)p_ptr->mhp);
	c_put_str(TERM_L_BLUE, buf, 7, 13);
	(void)sprintf(buf, "%d(%d)", (int)p_ptr->csp, (int)p_ptr->msp);
	c_put_str(TERM_L_BLUE, buf, 8, 13);
}
#endif /* 0 */

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

	byte a;
	char c;

	char buf[80];


	/* Column */
	stat_col = 26;

	/* Row */
	row = 3;

	/* Print out the labels for the columns */
#ifdef JP
	c_put_str(TERM_WHITE, "能力", row-1, stat_col);
	c_put_str(TERM_BLUE, "  基本", row-1, stat_col+5);
	c_put_str(TERM_L_BLUE, "種族/職業/装備 ", row-1, stat_col+11);
	c_put_str(TERM_L_GREEN, "合計", row-1, stat_col+26);
	c_put_str(TERM_YELLOW, "現在", row-1, stat_col+31);
#else
	c_put_str(TERM_WHITE, "Stat", row-1, stat_col);
	c_put_str(TERM_BLUE, "Intrnl", row-1, stat_col+5);
	c_put_str(TERM_L_BLUE, "Rce Cls Mod", row-1, stat_col+12);
	c_put_str(TERM_L_GREEN, "Actual", row-1, stat_col+24);
	c_put_str(TERM_YELLOW, "Currnt", row-1, stat_col+31);
#endif


	/* Display the stats */
	for (i = 0; i < 6; i++)
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

		e_adj -= rp_ptr->r_adj[i];
		e_adj -= cp_ptr->c_adj[i];

		/* Reduced name of stat */
#ifdef JP
		c_put_str(TERM_WHITE, stat_names[i], row + i, stat_col);
#else
		c_put_str(TERM_WHITE, stat_names_reduced[i], row + i, stat_col);
#endif

		/* Internal "natural" max value.  Maxes at 18/100 */
		/* This is useful to see if you are maxed out */
		cnv_stat(p_ptr->stat_max[i], buf);
		c_put_str(TERM_BLUE, buf, row + i, stat_col + 5);

		/* Race, class, and equipment modifiers */
		(void)sprintf(buf, "%3d", (int)rp_ptr->r_adj[i]);
		c_put_str(TERM_L_BLUE, buf, row + i, stat_col + 12);

		(void)sprintf(buf, "%3d", (int)cp_ptr->c_adj[i]);

		c_put_str(TERM_L_BLUE, buf, row + i, stat_col + 16);
		(void)sprintf(buf, "%3d", (int)e_adj);
		c_put_str(TERM_L_BLUE, buf, row + i, stat_col + 20);

		/* Actual maximal modified value */
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
	col = stat_col + 38;

	/* Header and Footer */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row - 1, col);
#ifdef JP
	c_put_str(TERM_L_GREEN, "能力修正", row - 2, col);
#else
	c_put_str(TERM_L_GREEN, "Modifications", row - 2, col);
#endif

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Access object */
		o_ptr = &inventory[i];

		/* Acquire "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Initialize color based of sign of pval. */
		for (stat = 0; stat < 6; stat++)
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
					if (o_ptr->pval < 10) c = '0' + (char) o_ptr->pval;
				}

				if (f2 & 1 << stat)
				{
					/* Dark green for sustained stats */
					a = TERM_GREEN;
				}

				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Bad */
					a = TERM_RED;

					/* Label boost */
					if (o_ptr->pval > -10) c = '0' - (char) o_ptr->pval;
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
			if (!use_color) a = TERM_WHITE;

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

		/* Mutations ... */
		if (p_ptr->muta)
		{
			int dummy = 0;

			if (stat == A_STR)
			{
				if (p_ptr->muta & MUT_HYPER_STR) dummy += 4;
				if (p_ptr->muta & MUT_PUNY) dummy -= 4;
			}
			else if (stat == A_WIS || stat == A_INT)
			{
				if (p_ptr->muta & MUT_HYPER_INT) dummy += 4;
				if (p_ptr->muta & MUT_MORONIC) dummy -= 4;
			}
			else if (stat == A_DEX)
			{
				if (p_ptr->muta & MUT_IRON_SKIN) dummy -= 1;
				if (p_ptr->muta & MUT_LIMBER) dummy += 3;
				if (p_ptr->muta & MUT_ARTHRITIS) dummy -= 3;
			}
			else if (stat == A_CON)
			{
				if (p_ptr->muta & MUT_RESILIENT) dummy += 4;
				if (p_ptr->muta & MUT_XTRA_FAT) dummy += 2;
				if (p_ptr->muta & MUT_ALBINO) dummy -= 4;
				if (p_ptr->muta & MUT_FLESH_ROT) dummy -= 2;
			}
			else if (stat == A_CHR)
			{
				if (p_ptr->muta & MUT_SILLY_VOI) dummy -= 4;
				if (p_ptr->muta & MUT_FLESH_ROT) dummy -= 1;
				if (p_ptr->muta & MUT_SCALES) dummy -= 1;
				if (p_ptr->muta & MUT_ILL_NORM) dummy = 0;
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
					if (dummy > -10) c = '0' - dummy;
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
		Term_putch(col, row + stat, a, c);
	}
}

#if 0

/*
 * Object flag names
 */
static cptr object_flag_names[96] =
{
#ifdef JP
	"+腕力",
	"+知能",
	"+賢さ",
	"+器用",
	"+耐久",
	"+魅力",
	"魔法道具",
#else
	"Add Str",
	"Add Int",
	"Add Wis",
	"Add Dex",
	"Add Con",
	"Add Chr",
	"MagicDev",
#endif

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
	"Pois Brand",
	"Acid Brand",
	"Elec Brand",
	"Fire Brand",
	"Cold Brand",

#ifdef JP
	"腕力 保持",
	"知能 保持",
	"賢さ 保持",
	"器用 保持",
	"耐久 保持",
	"魅力 保持",
#else
	"Sust Str",
	"Sust Int",
	"Sust Wis",
	"Sust Dex",
	"Sust Con",
	"Sust Chr",
#endif

	NULL,
	NULL,
#ifdef JP
	"超耐酸  ",
	"超耐電撃",
	"超耐火炎",
	"超耐冷気",
#else
	"Imm Acid",
	"Imm Elec",
	"Imm Fire",
	"Imm Cold",
#endif

#ifdef JP
	"投擲",
#else
	"Throwing",
#endif

#ifdef JP
	"反射",
	"耐麻痺",
	"経験値保持",
#else
	"Reflect",
	"Free Act",
	"Hold Life",
#endif

#ifdef JP
	"耐酸  ",
	"耐電撃",
	"耐火炎",
	"耐冷気",
	"耐毒  ",
	"耐恐怖",
	"耐閃光",
	"耐暗黒",
	"耐盲目",
	"耐混乱",
	"耐轟音",
	"耐破片",
	"耐地獄",
	"耐因混",
	"耐カオ",
	"耐劣化",
#else
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
#endif

#ifdef JP
	"火炎オーラ",

	"電気オーラ",
#else
	"Aura Fire",
	"Aura Elec",
#endif

	NULL,
	NULL,
#ifdef JP
	"防テレポ",
	"反魔法",
	"半物質化",
	"邪悪な怨念",
	"Easy Know",
	"Hide Type",
	"Show Mods",
	"Insta Art",
	"浮遊",
	"光源",
	"透明視認",
	"テレパシー",
	"遅消化",
	"急回復",
	"強力射撃",
	"高速射撃",
	"無傷 酸",
	"無傷 電",
	"無傷 火",
	"無傷 冷",
	"始動",
	"経験吸収",
	"テレポート",
	"反感",
	"祝福",
	"呪い",
	"重い呪い",
	"永遠の呪い"
#else
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
#endif

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
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
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

	/* Player flags */
	tim_player_flags(&f1, &f2, &f3);

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

			cptr name = object_flag_names[16 * x + y];

			/* No name */
			if (!name) continue;

			/* Dump name */
			Term_putstr(x * 13, y + 4, -1, TERM_WHITE, name);

			/* Dump colon */
			Term_putch(x * 13 + 10, y + 4, TERM_WHITE, ':');

			/* Check flag */
			if (b[x] & (1 << y))
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
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Index */
		n = (i - INVEN_WIELD);

		/* Object */
		o_ptr = &inventory[i];

		/* Known object flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Incorporate */
		b[n][0] = (u16b)(f1 & 0xFFFF);
		b[n][1] = (u16b)(f1 >> 16);
		b[n][2] = (u16b)(f2 & 0xFFFF);
		b[n][3] = (u16b)(f2 >> 16);
		b[n][4] = (u16b)(f3 & 0xFFFF);
		b[n][5] = (u16b)(f3 >> 16);
	}


	/* Index */
	n = 12;

	/* Player flags */
	player_flags(&f1, &f2, &f3);

	/* Incorporate */
	b[n][0] = (u16b)(f1 & 0xFFFF);
	b[n][1] = (u16b)(f1 >> 16);
	b[n][2] = (u16b)(f2 & 0xFFFF);
	b[n][3] = (u16b)(f2 >> 16);
	b[n][4] = (u16b)(f3 & 0xFFFF);
	b[n][5] = (u16b)(f3 >> 16);

	/* Player flags */
	tim_player_flags(&f1, &f2, &f3);

	/* Incorporate */
	b[n][0] = (u16b)(f1 & 0xFFFF);
	b[n][1] = (u16b)(f1 >> 16);
	b[n][2] = (u16b)(f2 & 0xFFFF);
	b[n][3] = (u16b)(f2 >> 16);
	b[n][4] = (u16b)(f3 & 0xFFFF);
	b[n][5] = (u16b)(f3 >> 16);


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
				if (b[n][3 * mode + x] & (1 << y))
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
#endif /* 0 */

#define ROW_NAME			1
#define COL_NAME			26
#define WID_NAME			8


/*
 * Display the player attributes.
 */
static void display_player_middle(void)
{
	int show_tohit;
	int show_todam;
	char buf[160];
	object_type *o_ptr;
	int tmul = 0;
	int i;

	for (i = 0; i < 2; i++)
	{
		if ((i == 1) && !is_two_handed()) break;

		o_ptr = &inventory[INVEN_WIELD + i];

		/* Melee wapon */
		show_tohit = p_ptr->dis_to_h + calc_melee_bonus(i);
		show_todam = p_ptr->dis_to_d;

		/* Hack -- add in weapon info if known */
		if (object_known_p(o_ptr)) show_tohit += o_ptr->to_h;
		if (object_known_p(o_ptr)) show_todam += o_ptr->to_d;

		/* Dump the bonuses to hit */
		if (!is_two_handed())
			display_player_one_line(ENTRY_TOHIT, format("(%+d,%+d)", show_tohit, show_todam), TERM_L_BLUE);
		else
			display_player_one_line(((i == 0) ? ENTRY_TOHIT_R : ENTRY_TODAM),
				format("(%+d,%+d)", show_tohit, show_todam), TERM_L_BLUE);
	}

	/* Range weapon */
	o_ptr = &inventory[INVEN_BOW];

	/* Base skill */
	show_tohit = p_ptr->dis_to_b;
	show_todam = 0;

	/* Apply range weapon bonuses */
	if (object_known_p(o_ptr)) show_tohit += o_ptr->to_h;
	if (object_known_p(o_ptr)) show_todam += o_ptr->to_d;

	/* Range attacks */
	display_player_one_line(ENTRY_SHOOT_TOHIT, format("(%+d,%+d)", show_tohit, show_todam), TERM_L_BLUE);

	if (o_ptr->k_idx)
	{
		tmul = bow_tmul(o_ptr->sval);

		/* Get extra "power" from "extra might" */
		if (p_ptr->xtra_might) tmul++;

		tmul = tmul * (100 + (int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	}

	/* shoot power */
	display_player_one_line(ENTRY_SHOOT_POWER, format("x%d.%02d", tmul/100, tmul%100), TERM_L_BLUE);

	/* Dump the armor class */
	display_player_one_line(ENTRY_BASE_AC, format("[%d,%+d]", p_ptr->dis_ac, p_ptr->dis_to_a), TERM_L_BLUE);

	/* Dump speed */
	{
		int tmp_speed = 0;
		byte attr;
		int i;

		i = p_ptr->pspeed - 110;

		/* Hack -- Visually "undo" the Search Mode Slowdown */
		if (p_ptr->searching) i += 10;

		if (i > 0)
		{
			attr = TERM_L_GREEN;
		}
		else if (i == 0)
		{
			attr = TERM_L_BLUE;
		}
		else
		{
			attr = TERM_L_UMBER;
		}

		if (p_ptr->fast) tmp_speed += 10;
		if (p_ptr->slow) tmp_speed -= 10;

		if (tmp_speed)
		{
			sprintf(buf, "(%+d%+d)", i - tmp_speed, tmp_speed);

			if (tmp_speed > 0) attr = TERM_YELLOW;
			else attr = TERM_VIOLET;
		}
		else
		{
			sprintf(buf, "(%+d)", i);
		}

		display_player_one_line(ENTRY_SPEED, buf, attr);
	}

	/* Dump character level */
	display_player_one_line(ENTRY_LEVEL, format("%d", p_ptr->lev), TERM_L_GREEN);

	/* Dump experience */
	if (p_ptr->exp >= p_ptr->max_exp)
		display_player_one_line(ENTRY_CUR_EXP, format("%ld", p_ptr->exp), TERM_L_GREEN);
	else
		display_player_one_line(ENTRY_CUR_EXP, format("%ld", p_ptr->exp), TERM_YELLOW);

	/* Dump max experience */
	display_player_one_line(ENTRY_MAX_EXP, format("%ld", p_ptr->max_exp), TERM_L_GREEN);

	/* Dump exp to advance */
	display_player_one_line(ENTRY_EXP_TO_ADV, format("%ld", (s32b)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L)), TERM_L_GREEN);

	/* Dump gold */
	display_player_one_line(ENTRY_GOLD, format("%ld", p_ptr->au), TERM_L_GREEN);

	/* Dump Day */
	{
		int day, hour, min;
		extract_day_hour_min(&day, &hour, &min);

		sprintf(buf, 
#ifdef JP
			"%d日目 %2d:%02d", 
#else
			"Day %d %2d:%02d", 
#endif
			day, hour, min);
	}
	display_player_one_line(ENTRY_DAY, buf, TERM_L_GREEN);

	/* Dump hit point */
	if (p_ptr->chp >= p_ptr->mhp) 
		display_player_one_line(ENTRY_HP, format("%4d/%4d", p_ptr->chp , p_ptr->mhp), TERM_L_GREEN);
	else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10) 
		display_player_one_line(ENTRY_HP, format("%4d/%4d", p_ptr->chp , p_ptr->mhp), TERM_YELLOW);
	else
		display_player_one_line(ENTRY_HP, format("%4d/%4d", p_ptr->chp , p_ptr->mhp), TERM_RED);

	/* Dump mana power */
	if (p_ptr->csp >= p_ptr->msp) 
		display_player_one_line(ENTRY_SP, format("%4d/%4d", p_ptr->csp , p_ptr->msp), TERM_L_GREEN);
	else if (p_ptr->csp > (p_ptr->msp * spellpoint_warn) / 10) 
		display_player_one_line(ENTRY_SP, format("%4d/%4d", p_ptr->csp , p_ptr->msp), TERM_YELLOW);
	else
		display_player_one_line(ENTRY_SP, format("%4d/%4d", p_ptr->csp , p_ptr->msp), TERM_RED);

	/* Dump play time */
	display_player_one_line(ENTRY_PLAY_TIME, format("%.2lu:%.2lu:%.2lu", playtime/(60*60), (playtime/60)%60, playtime%60), TERM_L_GREEN);
}


/*
 * Display the character on the screen (various modes)
 *
 * The top two and bottom two lines are left blank.
 */
void display_player(int mode)
{
	int i;

	char	buf[80];
	char	tmp[64];

	/* XXX XXX XXX */
	if (p_ptr->muta && !skip_mutations)
		mode = (mode % (DISPLAY_PLAYER_MAX + 1));
	else
		mode = (mode % DISPLAY_PLAYER_MAX);

	/* Erase screen */
	clear_from(0);

	/* Standard */
	if ((mode == DISPLAY_PLAYER_STANDARD) || (mode == DISPLAY_PLAYER_HISTORY))
	{
		/* Name, Sex, Race, Class */
		display_player_one_line(ENTRY_NAME, player_name, TERM_L_BLUE);
		display_player_one_line(ENTRY_SEX, sp_ptr->title, TERM_L_BLUE);
		display_player_one_line(ENTRY_RACE, rp_ptr->title, TERM_L_BLUE);
		display_player_one_line(ENTRY_CLASS, cp_ptr->title, TERM_L_BLUE);

		if (p_ptr->realm1)
		{
			if (p_ptr->realm2)
				sprintf(tmp, "%s, %s", realm_names[p_ptr->realm1], realm_names[p_ptr->realm2]);
			else
				strcpy(tmp, realm_names[p_ptr->realm1]);
			display_player_one_line(ENTRY_REALM, tmp, TERM_L_BLUE);
		}

		display_player_one_line(ENTRY_PATRON, valar_patrons[p_ptr->valar_patron], TERM_L_BLUE);

		/* Age, Height, Weight, Social */
		/* 身長はセンチメートルに、体重はキログラムに変更してあります */
#ifdef JP
		display_player_one_line(ENTRY_AGE, format("%d才", (int)p_ptr->age), TERM_L_BLUE);
		display_player_one_line(ENTRY_HEIGHT, format("%dcm", (int)((p_ptr->ht*254)/100)), TERM_L_BLUE);
		display_player_one_line(ENTRY_WEIGHT, format("%dkg", (int)((p_ptr->wt*4536)/10000)), TERM_L_BLUE);
		display_player_one_line(ENTRY_SOCIAL, format("%d  ", (int)p_ptr->sc), TERM_L_BLUE);
#else
		display_player_one_line(ENTRY_AGE, format("%d", (int)p_ptr->age), TERM_L_BLUE);
		display_player_one_line(ENTRY_HEIGHT, format("%d", (int)p_ptr->ht), TERM_L_BLUE);
		display_player_one_line(ENTRY_WEIGHT, format("%d", (int)p_ptr->wt), TERM_L_BLUE);
		display_player_one_line(ENTRY_SOCIAL, format("%d", (int)p_ptr->sc), TERM_L_BLUE);
#endif
#if 0
		display_player_one_line(ENTRY_ALIGN, format("%s", your_alignment()), TERM_L_BLUE);
#endif

		/* Display the stats */
		for (i = 0; i < 6; i++)
		{
			/* Special treatment of "injured" stats */
			if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
			{
				int value;

				/* Use lowercase stat name */
				put_str(stat_names_reduced[i], 3 + i, 54);

				/* Get the current stat */
				value = p_ptr->stat_use[i];

				/* Obtain the current stat (modified) */
				cnv_stat(value, buf);

				/* Display the current stat (modified) */
				c_put_str(TERM_YELLOW, buf, 3 + i, 61);

				/* Acquire the max stat */
				value = p_ptr->stat_top[i];

				/* Obtain the maximum stat (modified) */
				cnv_stat(value, buf);

				/* Display the maximum stat (modified) */
				c_put_str(TERM_L_GREEN, buf, 3 + i, 68);
			}

			/* Normal treatment of "normal" stats */
			else
			{
				/* Assume uppercase stat name */
				put_str(stat_names[i], 3 + i, 54);

				/* Obtain the current stat (modified) */
				cnv_stat(p_ptr->stat_use[i], buf);

				/* Display the current stat (modified) */
				c_put_str(TERM_L_GREEN, buf, 3 + i, 61);
			}

			if (p_ptr->stat_max[i] == (18 + 100))
			{
#ifdef JP
				c_put_str(TERM_WHITE, "!", 3+i, 59);
#else
				c_put_str(TERM_WHITE, "!", 3+i, 59-2);
#endif
			}
		}

		/* Display "history" info */
		if (mode == DISPLAY_PLAYER_HISTORY)
		{
			char statmsg[1000];

#ifdef JP
			put_str("(キャラクターの生い立ち)", 11, 25);
#else
			put_str("(Character Background)", 11, 25);
#endif

			for (i = 0; i < 4; i++) put_str(history[i], i + 12, 10);

			*statmsg = '\0';

			if (death && total_winner)
			{
#ifdef JP
				strcpy(statmsg, "…あなたは勝利の後引退した。");
#else
				strcpy(statmsg, "...You retired from the adventure after the winning.");
#endif
			}
			else if (death)
			{
				if (!dun_level)
				{
#ifdef JP
					sprintf(statmsg, "…あなたは%sで%sに殺された。", map_name(), died_from);
#else
					sprintf(statmsg, "...You were killed by %s in %s.", died_from, map_name());
#endif
				}
				else
				{
					if (p_ptr->inside_quest && (p_ptr->inside_quest < MIN_RANDOM_QUEST))
					{
						/* Get the quest */
						/* Bewere that INIT_ASSIGN resets the cur_num. */
						init_flags = INIT_ASSIGN;

						process_dungeon_file("q_info.txt", 0, 0, 0, 0);
#ifdef JP
						sprintf(statmsg, "…あなたは、クエスト「%s」で%sに殺された。", quest[p_ptr->inside_quest].name, died_from);
#else
						sprintf(statmsg, "...You were killed by %s in the quest '%s'.", died_from, quest[p_ptr->inside_quest].name);
#endif
					}
					else
					{
#ifdef JP
						sprintf(statmsg, "…あなたは、%d階で%sに殺された。", dun_level, died_from);
#else
						sprintf(statmsg, "...You were killed by %s on level %d.", died_from, dun_level);
#endif
					}
				}
			}
			else if (character_dungeon)
			{
				if (!dun_level)
				{
#ifdef JP
					sprintf(statmsg, "…あなたは現在、%sにいる。", map_name());
#else
					sprintf(statmsg, "...Now, you are in %s.", map_name());
#endif
				}
				else
				{
					if (p_ptr->inside_quest && (p_ptr->inside_quest < MIN_RANDOM_QUEST))
					{
						/* Clear the text */
						/* Must be done before doing INIT_SHOW_TEXT */
						for (i = 0; i < 10; i++)
						{
							quest_text[i][0] = '\0';
						}
						quest_text_line = 0;

						/* Get the quest */
						init_flags = INIT_SHOW_TEXT;

						process_dungeon_file("q_info.txt", 0, 0, 0, 0);
#ifdef JP
						sprintf(statmsg, "…あなたは現在、 クエスト「%s」を遂行中だ。", quest[p_ptr->inside_quest].name);
#else
						sprintf(statmsg, "...Now, you are in the quest '%s'.", quest[p_ptr->inside_quest].name);
#endif
					}
					else
					{
#ifdef JP
						sprintf(statmsg, "…あなたは現在、%d階で探索している。", dun_level);
#else
						sprintf(statmsg, "...Now, you are exploring level %d of.", dun_level);
#endif
					}
				}
			}

			if (*statmsg)
			{
				char temp[64 * 2], *t;
				roff_to_buf(statmsg, 60, temp, sizeof(temp));
				t = temp;
				for (i = 0; i < 2; i++)
				{
					if (!t[0]) break;
					else
					{
						put_str(t, i + 5 + 12, 10);
						t += strlen(t) + 1;
					}
				}
			}
		}

		/* Display "various" info */
		else
		{
			display_player_middle();
			display_player_various();
		}
	}

	/* Special */
	else if (mode == DISPLAY_PLAYER_SUMMARY)
	{
		/* See "http://www.cs.berkeley.edu/~davidb/angband.html" */

		/* Dump the info */
		/* display_player_misc_info(); */
		display_player_stat_info();
		display_player_flag_info();
	}

	else if (mode == DISPLAY_PLAYER_MAX)
	{
		do_cmd_knowledge_mutations();
	}
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
		/* Update playtime */
		update_playtime();

		/* Display the player */
		display_player(mode);

		/* Prompt */
		Term_putstr(2, 23, -1, TERM_WHITE,
#ifdef JP
"['c'で名前変更, 'f'でファイルへ書出, 'p'で前, 'n'で次, ESCで終了]");
#else
			"['c' to change name, 'f' to file, 'p' for previous, 'n' for next, or ESC]");
#endif


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
#ifdef JP
if (get_string("ファイルネーム: ", tmp, 80))
#else
			if (get_string("File name: ", tmp, 80))
#endif

			{
				if (tmp[0] && (tmp[0] != ' '))
				{
					file_character(tmp, TRUE);
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
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
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
	int	  	i, x, y;
	byte		a;
	char		c;
	cptr		paren = ")";
	int			fd;
	FILE		*fff = NULL;
	store_type  *st_ptr;
	char		o_name[MAX_NLEN];
	char		buf[1024];

	bool dmp_msg = FALSE;
	bool dmp_spoil = dump_abilities;
	int msg_max = message_num();

	if (dump_messages)
	{
#ifdef JP
		if (get_check(format("メッセージログ(%d 行)も出力しますか？", msg_max)))
#else
		if (get_check(format("Dump message logs ? (%d lines)", msg_max)))
#endif
			dmp_msg = TRUE;
	}

	/* Drop priv's */
	safe_setuid_drop();

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
		(void)fd_close(fd);

		/* Build query */
#ifdef JP
		(void)sprintf(out_val, "現存するファイルに上書きしますか? (%s)", buf);
#else
		(void)sprintf(out_val, "Replace existing file %s? ", buf);
#endif

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
#ifdef JP
		msg_format("キャラクタ情報のファイルへの書き出しに失敗しました！");
#else
		msg_format("Character dump failed!");
#endif
		msg_print(NULL);

		/* Error */
		return (-1);
	}


#ifndef FAKE_VERSION
	/* Begin dump */
	fprintf(fff, "  [Zangband %d.%d.%d Character Dump]\n\n",
			  VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
#else
#ifdef JP
	fprintf(fff, "  [%s %d.%d.%d キャラクタ情報]\n\n",
		VERSION_NAME, FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH);
#else
	fprintf(fff, "  [%s %d.%d.%d Character Dump]\n\n",
		VERSION_NAME, FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH);
#endif

#endif

	/* Display player */
	display_player(DISPLAY_PLAYER_STANDARD);

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

		/* End the string */
		buf[x] = '\0';

		/* Kill trailing spaces */
		while ((x > 0) && (buf[x-1] == ' ')) buf[--x] = '\0';

		/* End the row */
#ifdef JP
		fprintf(fff, "%s\n", buf);
#else
		fprintf(fff, "%s\n", buf);
#endif
	}

	/* Display history */
	display_player(DISPLAY_PLAYER_HISTORY);

	/* Dump part of the screen */
	for (y = 10; y < 19; y++)
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
		while ((x > 0) && (buf[x-1] == ' ')) buf[--x] = '\0';

		/* End the row */
		fprintf(fff, "%s\n", buf);
	}

	/* Show (known) flags grid */
	if (full)
	{
		display_player(DISPLAY_PLAYER_SUMMARY);

		/* Dump first column */
		for (y = 0; y < 23; y++)
		{
			for (x = 0; x < 79; x++)
			{
				(void)(Term_what(x, y, &a, &c));

				/* Dump it (Ignore equippy tile graphic) */
				if (a < 128)
					buf[x] = c;
				else
					buf[x] = ' ';
			}

			/* End the string */
			buf[x] = '\0';

			/* Kill trailing spaces */
			while ((x > 0) && (buf[x-1] == ' ')) buf[--x] = '\0';

			/* End the row */
			fprintf(fff, "%s\n", buf);
		}
	}

#ifdef JP
	fprintf(fff, "\n  [クエスト情報]\n\n");
#else
	fprintf(fff, "\n  [Quest information]\n\n");
#endif

	if (p_ptr->max_dlv > 0)
	{
#ifdef JP
		fprintf(fff, "《帰還場所》\n");
		fprintf(fff, "  %d 階 (%d フィート)\n", p_ptr->max_dlv, p_ptr->max_dlv * 50);
#else
		fprintf(fff, "< Recall Depth >\n");
		fprintf(fff, "  Level %d (%d')\n", p_ptr->max_dlv, p_ptr->max_dlv * 50);
#endif
	}

	do_cmd_knowledge_quests_aux(fff);

	if (death && !total_winner)
	{
#ifdef JP
		fprintf(fff, "\n  [死ぬ直前のメッセージ]\n\n");
#else
		fprintf(fff, "\n  [Last messages]\n\n");
#endif
		for (i = MIN(message_num(), 30); i >= 0; i--)
		{
			fprintf(fff,"> %s\n",message_str((s16b)i));
		}
		fprintf(fff, "\n");
	}

#ifdef JP
	fprintf(fff, "\n\n  [その他の情報]\n");
#else
	fprintf(fff, "\n\n  [Miscellaneous information]\n");
#endif

	if (preserve_mode)
#ifdef JP
		fprintf(fff, "\n 保存モード:         ON");
#else
		fprintf(fff, "\n Preserve Mode:      ON");
#endif
	else
#ifdef JP
		fprintf(fff, "\n 保存モード:         OFF");
#else
		fprintf(fff, "\n Preserve Mode:      OFF");
#endif

	if (ironman_autoscum)
#ifdef JP
		fprintf(fff, "\n 自動選り好み  :     ALWAYS");
#else
		fprintf(fff, "\n Autoscum:           ALWAYS");
#endif
	else if (auto_scum)
#ifdef JP
		fprintf(fff, "\n 自動選り好み  :     ON");
#else
		fprintf(fff, "\n Autoscum:           ON");
#endif
	else
#ifdef JP
		fprintf(fff, "\n 自動選り好み  :     OFF");
#else
		fprintf(fff, "\n Autoscum:           OFF");
#endif
#ifndef TINYANGBAND
	if (ironman_small_levels)
#ifdef JP
		fprintf(fff, "\n 小さいダンジョン:   ALWAYS");
#else
		fprintf(fff, "\n Small Levels:       ALWAYS");
#endif
	else if (always_small_levels)
#ifdef JP
		fprintf(fff, "\n 小さいダンジョン:   ON");
#else
		fprintf(fff, "\n Small Levels:       ON");
#endif
	else if (small_levels)
#ifdef JP
		fprintf(fff, "\n 小さいダンジョン:   ENABLED");
#else
		fprintf(fff, "\n Small Levels:       ENABLED");
#endif
	else
#ifdef JP
		fprintf(fff, "\n 小さいダンジョン:   OFF");
#else
		fprintf(fff, "\n Small Levels:       OFF");
#endif
#endif /* ifndef TINYANGBAND */
	if (ironman_shops)
#ifdef JP
		fprintf(fff, "\n 店なし:             ON");
#else
		fprintf(fff, "\n No Shops:           ON");
#endif

	if (ironman_downward)
#ifdef JP
		fprintf(fff, "\n 階段を上がれない:   ON");
#else
		fprintf(fff, "\n Diving only:        ON");
#endif
#ifndef TINYANGBAND
	if (ironman_empty_levels)
#ifdef JP
		fprintf(fff, "\n アリーナ:           ALWAYS");
#else
		fprintf(fff, "\n Arena Levels:       ALWAYS");
#endif
	else if (empty_levels)
#ifdef JP
		fprintf(fff, "\n アリーナ:           ON");
#else
		fprintf(fff, "\n Arena Levels:       ENABLED");
#endif
	else
#ifdef JP
		fprintf(fff, "\n アリーナ:           OFF");
#else
		fprintf(fff, "\n Arena Levels:       OFF");
#endif
#endif /* ifndef TINYANGBAND */
	if (ironman_nightmare)
#ifdef JP
		fprintf(fff, "\n 悪夢モード:         ON");
#else
		fprintf(fff, "\n Nightmare Mode:     ON");
#endif
#ifndef TINYANGBAND
	if (ironman_hengband)
#ifdef JP
		fprintf(fff, "\n 変愚蛮怒モード:     ON");
#else
		fprintf(fff, "\n Hengband Mode:      ON");
#endif
#endif /* ifndef TINYANGBAND */
#ifdef JP
	fprintf(fff, "\n ランダムクエスト数: %d", number_of_quests());
#else
	fprintf(fff, "\n Num. Random Quests: %d", number_of_quests());
#endif

	if (noscore)
#ifdef JP
		fprintf(fff, "\n 何か不正なことをしてしまってます。");
#else
		fprintf(fff, "\n You have done something illegal.");
#endif

	if (X_stupid_monsters)
#ifdef JP
		fprintf(fff, "\n 敵は愚かな行動を取ります。");
#else
		fprintf(fff, "\n Your opponents are behaving stupidly.");
#endif

	if (munchkin_death)
#ifdef JP
		fprintf(fff, "\n あなたは死を回避するインチキな力を持っています。");
#else
		fprintf(fff, "\n You possess munchkinish power over death.");
#endif

	/* Monsters slain */
	{
		int k;
		s32b Total = 0;

		for (k = 1; k < max_r_idx; k++)
		{
			monster_race *r_ptr = &r_info[k];

			if (r_ptr->flags1 & RF1_UNIQUE)
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
#ifdef JP
			fprintf(fff,"\n\n まだ敵を倒していません。\n");
		else
			fprintf(fff,"\n\n %lu 体の敵を倒しています。\n", Total);
#else
			fprintf(fff,"\n\n You have defeated no enemies yet.\n");
		else if (Total == 1)
			fprintf(fff,"\n\n You have defeated one enemy.\n");
		else
			fprintf(fff,"\n\n You have defeated %lu enemies.\n", Total);
#endif
	}

	if (p_ptr->muta)
	{
#ifdef JP
		fprintf(fff, "\n\n  [突然変異]\n\n");
#else
		fprintf(fff, "\n\n  [Mutations]\n\n");
#endif
		dump_mutations(fff);
	}


	/* Skip some lines */
	fprintf(fff, "\n\n");


	/* Dump the equipment */
	if (equip_cnt)
	{
#ifdef JP
		fprintf(fff, "  [ キャラクタの装備 ]\n\n");
#else
		fprintf(fff, "  [Character Equipment]\n\n");
#endif
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			object_desc(o_name, &inventory[i], 0);
			fprintf(fff, "%c%s %s\n",
				index_to_label(i), paren, o_name);
			if (dmp_spoil && (inventory[i].ident & IDENT_MENTAL) &&
				(inventory[i].art_name || artifact_p(&inventory[i]) || inventory[i].ego_name))
				dump_object(fff, &inventory[i]);
		}
		fprintf(fff, "\n\n");
	}

	/* Dump the inventory */
#ifdef JP
	fprintf(fff, "  [ キャラクタの持ち物 ]\n\n");
#else
	fprintf(fff, "  [Character Inventory]\n\n");
#endif
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Don't dump the empty slots */
		if (!inventory[i].k_idx) break;

		/* Dump the inventory slots */
		object_desc(o_name, &inventory[i], 0);
		fprintf(fff, "%c%s %s\n", index_to_label(i), paren, o_name);
		if (dmp_spoil && (inventory[i].ident & IDENT_MENTAL) &&
			(inventory[i].art_name || artifact_p(&inventory[i]) || inventory[i].ego_name))
			dump_object(fff, &inventory[i]);
	}

	/* Add an empty line */
	fprintf(fff, "\n\n");

	/* Print all homes in the different towns */
	st_ptr = &town[1].store[STORE_HOME];

	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Header with name of the town */
#ifdef JP
		fprintf(fff, "  [ 我が家のアイテム ]\n");
#else
		fprintf(fff, "  [Home Inventory]\n");
#endif
		x=1;

		/* Dump all available items */
		for (i = 0; i < st_ptr->stock_num; i++)
		{
#ifdef JP
			if ((i % 12) == 0) fprintf(fff, "\n ( %d ページ )\n", x++);
			object_desc(o_name, &st_ptr->stock[i], 0);
			fprintf(fff, "%c%s %s\n", I2A(i%12), paren, o_name);
#else
			if ((i % 12) == 0) fprintf(fff, "\n ( page %d )\n", x++);
			object_desc(o_name, &st_ptr->stock[i], 0);
			fprintf(fff, "%c%s %s\n", I2A(i%12), paren, o_name);
#endif
			if (dmp_spoil && (st_ptr->stock[i].ident & IDENT_MENTAL) &&
				(st_ptr->stock[i].art_name || artifact_p(&st_ptr->stock[i]) || st_ptr->stock[i].ego_name))
			dump_object(fff, &st_ptr->stock[i]);
		}

		/* Add an empty line */
		fprintf(fff, "\n\n");
	}

	if (dmp_msg)
	{
#ifdef JP
		fprintf(fff, "  [メッセージ ログ(直前の %d 行)]\n\n", msg_max);
#else
		fprintf(fff, "  [Message Log (last %d messages)]\n\n", msg_max);
#endif
		for (i = msg_max - 1; i >= 0; i--)
		{
			fprintf(fff, "%s\n", message_str(i));
		}
		fprintf(fff, "\n\n");
	}
		
	/* Close it */
	my_fclose(fff);

	/* Message */
#ifdef JP
	msg_print("キャラクタ情報のファイルへの書き出しに成功しました。");
#else
	msg_print("Character dump successful.");
#endif
	msg_print(NULL);

	/* Success */
	return (0);
}


typedef struct file_tag
{
	char name[32];
	int line_number;
} file_tag;

#define MAX_TAG_NUM 1024

typedef struct file_tags
{
	file_tag tags[MAX_TAG_NUM];
	int index;
} file_tags;


static void add_tag(file_tags *the_tags, cptr name, int line)
{
	if (the_tags->index < MAX_TAG_NUM)
	{
		file_tag *tag = &(the_tags->tags[the_tags->index]);

		/* Set the name and end it with '\0' */
		strncpy(tag->name, name, 31);
		tag->name[31] = '\0';

		/* Set the line-number */
		tag->line_number = line;

		/* Increase the number of tags */
		the_tags->index++;
	}
}


static int get_line(file_tags *the_tags, cptr name)
{
	int i;

	/* Search for the tag */
	for (i = 0; i < the_tags->index; i++)
	{
		if (streq(the_tags->tags[i].name, name))
		{
			return the_tags->tags[i].line_number;
		}
	}

	/* Not found */
	return 0;
}


/*
 * Display single line of on-line help file
 *
 * You can insert some special color tag to change text color.
 * Such as...
 * WHITETEXT [[[[y|SOME TEXT WHICH IS DISPLAYED IN YELLOW| WHITETEXT
 *
 * A colored segment is between "[[[[y|" and the last "|".
 * You can use any single character in place of the "|".
 */
static void show_file_aux_line(cptr str, int cy, cptr shower)
{
	static const char tag_str[] = "[[[[";
	byte color = TERM_WHITE;
	char in_tag = '\0';
	int cx = 0;
	int i;
	char lcstr[1024];

	if (shower)
	{
		/* Make a lower case version of str for searching */
		strcpy(lcstr, str);
		str_tolower(lcstr);
	}

	/* Initial cursor position */
	Term_gotoxy(cx, cy);

	for (i = 0; str[i];)
	{
		int len = strlen(&str[i]);
		int showercol = len + 1;
		int bracketcol = len + 1;
		int endcol = len;
		cptr ptr;

		/* Search for a shower string in the line */
		if (shower)
		{
			ptr = my_strstr(&lcstr[i], shower);
			if (ptr) showercol = ptr - &lcstr[i];
		}

		/* Search for a color segment tag */
		ptr = in_tag ? my_strchr(&str[i], in_tag) : my_strstr(&str[i], tag_str);
		if (ptr) bracketcol = ptr - &str[i];

		/* A color tag is found */
		if (bracketcol < endcol) endcol = bracketcol;

		/* The shower string is found before the color tag */
		if (showercol < endcol) endcol = showercol;

		/* Print a segment of the line */
		Term_addstr(endcol, color, &str[i]);
		cx += endcol;
		i += endcol;

		/* Shower string? */
		if (endcol == showercol)
		{
			int showerlen = strlen(shower);

			/* Print the shower string in yellow */
			Term_addstr(showerlen, TERM_YELLOW, &str[i]);
			cx += showerlen;
			i += showerlen;
		}

		/* Colored segment? */
		else if (endcol == bracketcol)
		{
			if (in_tag)
			{
				/* Found the end of colored segment */
				i++;

				/* Now looking for an another tag_str */
				in_tag = '\0';

				/* Set back to the default color */
				color = TERM_WHITE;
			}
			else
			{
				/* Found a tag_str, and get a tag color */
				i += sizeof(tag_str)-1;

				/* Get tag color */
				color = color_char_to_attr(str[i]);

				/* Illegal color tag */
				if (color == 255 || str[i+1] == '\0')
				{
					/* Illegal color tag */
					color = TERM_WHITE;

					/* Print the broken tag as a string */
					Term_addstr(-1, TERM_WHITE, tag_str);
					cx += sizeof(tag_str)-1;
				}
				else
				{
					/* Skip the color tag */
					i++;

					/* Now looking for a close tag */
					in_tag = str[i];

					/* Skip the close-tag-indicator */
					i++;
				}
			}
		}

	} /* for (i = 0; str[i];) */

	/* Clear rest of line */
	Term_erase(cx, cy, 255);
}


/*
 * Recursive file perusal.
 *
 * Process various special text in the input file, including
 * the "menu" structures used by the "help file" system.
 *
 * Return FALSE on 'q' to exit from a deep, otherwise TRUE.
 */
bool show_file(cptr name, cptr what, int line, int mode)
{
	int i, n, skey;

	/* Number of "real" lines passed by */
	int next = 0;

	/* Number of "real" lines in the file */
	int size;

	/* Backup value for "line" */
	int back = 0;

	/* This screen has sub-screens */
	bool menu = FALSE;

	/* Current help file */
	FILE *fff = NULL;

	/* Find this string (if any) */
	cptr find = NULL;

	/* Jump to this tag */
	cptr tag = NULL;

	/* Hold strings to find/show */
	char finder_str[81];
	char shower_str[81];
	char back_str[81];

	/* String to show */
	cptr shower = NULL;

	/* Filename */
	char filename[1024];

	/* Describe this thing */
	char caption[128];

	/* Path buffer */
	char path[1024];

	/* General buffer */
	char buf[1024];

	/* Sub-menu information */
	char hook[62][32];

	/* Tags for in-file references */
	file_tags tags;

	int wid, hgt, rows;

	Term_get_size(&wid, &hgt);
	rows = hgt - 4;

	/* Wipe finder */
	strcpy(finder_str, "");

	/* Wipe shower */
	strcpy(shower_str, "");

	/* Wipe caption */
	strcpy(caption, "");

	/* Wipe the hooks */
	for (i = 0; i < 62; i++)
	{
		hook[i][0] = '\0';
	}

	/* No tags yet */
	tags.index = 0;

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
#ifdef JP
		sprintf(caption, "ヘルプ・ファイル'%s'", name);
#else
		sprintf(caption, "Help file '%s'", name);
#endif


		/* Build the filename */
		path_build(path, sizeof(path), ANGBAND_DIR_HELP, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Look in "info" */
	if (!fff)
	{
		/* Caption */
#ifdef JP
		sprintf(caption, "スポイラー・ファイル'%s'", name);
#else
		sprintf(caption, "Info file '%s'", name);
#endif


		/* Build the filename */
		path_build(path, sizeof(path), ANGBAND_DIR_INFO, name);

		/* Open the file */
		fff = my_fopen(path, "r");
	}

	/* Oops */
	if (!fff)
	{
		/* Message */
#ifdef JP
		msg_format("'%s'をオープンできません。", name);
#else
		msg_format("Cannot open '%s'.", name);
#endif

		msg_print(NULL);

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
			/* Notice "menu" requests */
			if ((buf[6] == '[') && (isdigit(buf[7]) || isalpha(buf[7])))
			{
				/* Extract the menu item */
				int k = isdigit(buf[7]) ? D2I(buf[7]) : buf[7] - 'A' + 10;

				/* This is a menu file */
				menu = TRUE;

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
				size_t len = strlen(buf);

				if (buf[len - 1] == '>')
				{
					buf[len - 1] = '\0';
					add_tag(&tags, buf + 7, next);
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

	/* Go to the tagged line */
	if (tag) line = get_line(&tags, tag);

	/* Clear screen */
	Term_clear();

	/* Display the file */
	while (TRUE)
	{
		/* Restart when necessary */
		if (line >= size - rows) line = size - rows;
		if (line < 0) line = 0;

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
			if (my_fgets(fff, buf, sizeof(buf))) break;

			/* Skip tags/links */
			if (prefix(buf, "***** ")) continue;

			/* Count the lines */
			next++;
		}

		/* Dump the next rows lines of the file */
		for (i = 0; i < rows; )
		{
			/* Hack -- track the "first" line */
			if (!i) line = next;

			/* Get a line of the file or stop */
			if (my_fgets(fff, buf, sizeof(buf))) break;

			/* Hack -- skip "special" lines */
			if (prefix(buf, "***** ")) continue;

			/* Count the "real" lines */
			next++;

			/* Hack -- keep searching */
			if (find && !i)
			{
				char lc_buf[1024];

				/* Make a lower case version of str for searching */
				strcpy(lc_buf, buf);
				str_tolower(lc_buf);

				if (!my_strstr(lc_buf, find)) continue;
			}

			/* Hack -- stop searching */
			find = NULL;

			/* Dump the line */
			show_file_aux_line(buf, i + 2, shower);

			/* Count the printed lines */
			i++;
		}

		while (i < rows)
		{
			/* Clear rest of line */
			Term_erase(0, i + 2, 255);

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
#ifdef JP
		prt(format("[%s %d.%d.%d, %s, %d/%d]",
#else
		prt(format("[%s %d.%d.%d, %s, Line %d/%d]",
#endif
			   VERSION_NAME, FAKE_VER_MAJOR, FAKE_VER_MINOR, FAKE_VER_PATCH,
			   caption, line, size), 0, 0);

		/* Prompt -- menu screen */
		if (menu)
		{
			/* Wait for it */
#ifdef JP
			prt("[ 番号を入力して下さい( ESCで終了 ) ]", hgt - 1, 0);
#else
			prt("[Press a Number, or ESC to exit.]", hgt - 1, 0);
#endif
		}

		/* Prompt -- small files */
		else if (size <= rows)
		{
			/* Wait for it */
#ifdef JP
			prt("[ ESC で終了 ]", hgt - 1, 0);
#else
			prt("[Press ESC to exit.]", hgt - 1, 0);
#endif
		}

		/* Prompt -- large files */
		else
		{
			/* Wait for it */
#ifdef JP
			prt("[キー:(RET/スペース)↓ (-)↑ (?)ヘルプ使用法 (ESC)戻る]", hgt - 1, 1);
#else
			prt("[Press Return, Space, -, =, /, or ESC to exit.]", hgt - 1, 0);
#endif
		}

		/* Get a special key code */
		skey = inkey_special(TRUE);

		switch (skey)
		{
		/* Show the help for the help */
		case '?':
			/* Hack - prevent silly recursion */
#ifdef JP
			if (strcmp(name, "jhelpinfo.txt") != 0)
				show_file("jhelpinfo.txt", NULL, 0, mode);
#else
			if (strcmp(name, "helpinfo.txt") != 0)
				show_file("helpinfo.txt", NULL, 0, mode);
#endif
			break;

		/* Hack -- try showing */
		case '=':
			/* Get "shower" */
#ifdef JP
			prt("強調: ", hgt - 1, 0);
#else
			prt("Show: ", hgt - 1, 0);
#endif

			strcpy(back_str, shower_str);
			if (askfor(shower_str, 80))
			{
				if (shower_str[0])
				{
					/* Make it lowercase */
					str_tolower(shower_str);

					/* Show it */
					shower = shower_str;
				}
				else shower = NULL; /* Stop showing */
			}
			else strcpy(shower_str, back_str);
			break;

		/* Hack -- try finding */
		case '/':
		case KTRL('s'):
			/* Get "finder" */
#ifdef JP
			prt("検索: ", hgt - 1, 0);
#else
			prt("Find: ", hgt - 1, 0);
#endif

			strcpy(back_str, finder_str);
			if (askfor(finder_str, 80))
			{
				if (finder_str[0])
				{
					/* Find it */
					find = finder_str;
					back = line;
					line = line + 1;

					/* Make finder lowercase */
					str_tolower(finder_str);

					/* Show it */
					shower = finder_str;
				}
				else shower = NULL; /* Stop showing */
			}
			else strcpy(finder_str, back_str);
			break;

		/* Hack -- go to a specific line */
		case '#':
			{
				char tmp[81];
#ifdef JP
				prt("行: ", hgt - 1, 0);
#else
				prt("Goto Line: ", hgt - 1, 0);
#endif
				strcpy(tmp, "0");

				if (askfor(tmp, 80)) line = atoi(tmp);
			}
			break;

		/* Hack -- go to the top line */
		case SKEY_TOP:
			line = 0;
			break;

		/* Hack -- go to the bottom line */
		case SKEY_BOTTOM:
			line = ((size - 1) / rows) * rows;
			break;

		/* Hack -- go to a specific file */
		case '%':
			{
				char tmp[81];
#ifdef JP
				prt("ファイル・ネーム: ", hgt - 1, 0);
				strcpy(tmp, "jhelp.hlp");
#else
				prt("Goto File: ", hgt - 1, 0);
				strcpy(tmp, "help.hlp");
#endif

				if (askfor(tmp, 80))
				{
					if (!show_file(tmp, NULL, 0, mode)) skey = 'q';
				}
			}
			break;

		/* Hack -- Allow backing up */
		case  '-':
			line = line - rows / 2;
			if (line < 0) line = 0;
			break;

		/* One page up */
		case  SKEY_PGUP:
			line = line - rows;
			if (line < 0) line = 0;
			break;

		/* Advance a single line */
		case '\n':
		case '\r':
			line = line + 1;
			break;

		/* Move up / down */
		case '8':
		case SKEY_UP:
			line--;
			if (line < 0) line = 0;
			break;

		case '2':
		case SKEY_DOWN:
			line++;
			break;

		/* Advance one page */
		case ' ':
		case SKEY_PGDOWN:
			line = line + rows;
			break;
		}

		/* Recurse on numbers */
		if (menu)
		{
			int key = -1;

			if (!(skey & SKEY_MASK))
			{
				if (isdigit(skey)) key = D2I(skey);
				else if (isalpha(skey)) key = skey - 'A' + 10;
			}

			if ((key > -1) && hook[key][0])
			{
				/* Recurse on that file */
				if (!show_file(hook[key], NULL, 0, mode))
					skey = 'q';
			}
		}

		/* Hack, dump to file */
		if (skey == '|')
		{
			FILE *ffp;
			char buff[1024];
			char xtmp[82];

			strcpy(xtmp, "");

#ifdef JP
			if (get_string("ファイル名: ", xtmp, 80))
#else
			if (get_string("File name: ", xtmp, 80))
#endif
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
			path_build(buff, sizeof(buff), ANGBAND_DIR_USER, xtmp);

			/* Close it */
			my_fclose(fff);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");

			ffp = my_fopen(buff, "w");

			/* Oops */
			if (!(fff && ffp))
			{
#ifdef JP
				msg_print("ファイルを開けません。");
#else
				msg_print("Failed to open file.");
#endif
				skey = ESCAPE;
				break;
			}

			sprintf(xtmp, "%s: %s", player_name, what ? what : caption);
			my_fputs(ffp, xtmp, 80);
			my_fputs(ffp, "\n", 80);

			while (!my_fgets(fff, buff, 80))
				my_fputs(ffp, buff, 80);

			/* Close it */
			my_fclose(fff);
			my_fclose(ffp);

			/* Hack -- Re-Open the file */
			fff = my_fopen(path, "r");
		}

		/* Return to last screen */
		if (skey == ESCAPE) break;
		if (skey == '<') break;

		/* Exit on the ^q */
		if (skey == KTRL('q')) skey = 'q';

		/* Exit on the q key */
		if (skey == 'q') break;
	}

	/* Close the file */
	my_fclose(fff);

	/* Escape */
	if (skey == 'q') return (FALSE);

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
#ifdef JP
	(void)show_file("jhelp.hlp", NULL, 0, 0);
#else
	(void)show_file("help.hlp", NULL, 0, 0);
#endif

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
	char old_player_base[32] = "";

	if (character_generated) strcpy(old_player_base, player_base);

	/* Cannot be too long */
#if defined(MACINTOSH) || defined(MSDOS) || defined(USE_EMX) || defined(AMIGA) || defined(ACORN) || defined(VM)
#ifdef MSDOS
	if (strlen(player_name) > 8)
#else
	if (strlen(player_name) > 15)
#endif
	{
		/* Name too long */
#ifdef JP
		quit_fmt("'%s'という名前は長すぎます！", player_name);
#else
		quit_fmt("The name '%s' is too long!", player_name);
#endif

	}
#endif

	/* Cannot contain "icky" characters */
	for (i = 0; player_name[i]; i++)
	{
		/* No control characters */
#ifdef JP
		if (iskanji(player_name[i])){i++; continue;}
		if (iscntrl((unsigned char)player_name[i]))
#else
		if (iscntrl(player_name[i]))
#endif
		{
			/* Illegal characters */
#ifdef JP
			quit_fmt("'%s' という名前は不正なコントロールコードを含んでいます。", player_name);
#else
			quit_fmt("The name '%s' contains control chars!", player_name);
#endif
		}
	}

#ifdef MACINTOSH
	/* Extract "useful" letters */
	for (i = 0; player_name[i]; i++)
	{
#ifdef JP
		unsigned char c = player_name[i];
#else
		char c = player_name[i];
#endif

		/* Convert "dot" to "underscore" */
		if (c == '.') c = '_';

		/* Accept all the letters */
		player_base[k++] = c;
	}

#else
	/* Extract "useful" letters */
	for (i = 0; player_name[i]; i++)
	{
#ifdef JP
		unsigned char c = player_name[i];
#else
		char c = player_name[i];
#endif

		/* Accept some letters */
#ifdef JP
		if(iskanji(c))
		{
			if(k + 2 >= sizeof(player_base) || !player_name[i+1]) break;
			player_base[k++] = c;
			i++;
			player_base[k++] = player_name[i];
		}
#ifdef SJIS
		else if (iskana(c)) player_base[k++] = c;
#endif
		else
#endif
		/* Convert path separator to underscore */
		if (!strncmp(PATH_SEP, player_name+i, strlen(PATH_SEP))){
			player_base[k++] = '_';
			i += strlen(PATH_SEP);
		}
		/* Convert some characters to underscore */
#ifdef MSDOS
		else if (my_strchr(" \"*+,./:;<=>?[\\]|", c)) player_base[k++] = '_';
#elif defined(WINDOWS)
		else if (my_strchr("\"*,/:;<>?\\|", c)) player_base[k++] = '_';
#endif
		else if (isprint(c)) player_base[k++] = c;
	}
#endif


#if defined(MSDOS)
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

	if (!savefile_base[0] && savefile[0])
	{
		cptr s;
		s = savefile;
		while (1)
		{
			cptr t;
			t = my_strstr(s, PATH_SEP);
			if (!t)
				break;
			s = t + 1;
		}
		strcpy(savefile_base, s);
	}

	if (!savefile_base[0] || !savefile[0])
		sf = TRUE;

	/* Change the savefile name */
	if (sf)
	{
		char temp[128];

		strcpy(savefile_base, player_base);

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
		path_build(savefile, sizeof(savefile), ANGBAND_DIR_SAVE, temp);
	}

	/* Load an autopick preference file */
	if (character_generated)
	{
		if (!streq(old_player_base, player_base)) autopick_load_pref(FALSE);
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
#ifdef JP
	prt("[上に名前を入力してください。ESCでデフォルト]", 23, 2);
#else
	prt("[Enter your player's name above, or hit ESCAPE]", 23, 2);
#endif

	/* Go to the "name" field */
	move_cursor(ROW_NAME, COL_NAME + WID_NAME);

	/* Save the player name */
	strcpy(tmp, player_name);

	/* Get an input, ignore "Escape" */
	if (askfor(tmp, 15)) strcpy(player_name, tmp);

	/* Process the player name */
	process_player_name(FALSE);

	/* Pad the name (to clear junk) */
	sprintf(tmp, "%-15.15s", player_name);

	/* Re-Draw the name (in light blue) */
	Term_erase(COL_NAME + WID_NAME, ROW_NAME, 255);
	c_put_str(TERM_L_BLUE, tmp, ROW_NAME, COL_NAME + WID_NAME);

	/* Erase the prompt, etc */
	clear_from(22);
}


/*
 * Gets a name for the character, reacting to name changes.
 */
void get_character_name(void)
{
	char tmp[16];

	/* Save the player name */
	strcpy(tmp, player_name);

	/* Prompt for a new name */
#ifdef JP
	if (get_string("キャラクタの名前を入力して下さい:", tmp, 15))
#else
	if (get_string("Enter a name for your character: ", tmp, 15))
#endif
	{
		/* Use the name */
		strcpy(player_name, tmp);
	}

	if (0 == strlen(player_name))
	{
		/* Use default name */
		strcpy(player_name, "PLAYER");
	}

	/* Process the player name */
	process_player_name(FALSE);
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
#ifdef JP
		if (!get_check("引退しますか? ")) return;
#else
		if (!get_check("Do you want to retire? ")) return;
#endif
	}

	/* Verify Suicide */
	else
	{
		/* Verify */
#ifdef JP
		if (!get_check("本当に自殺しますか？")) return;
#else
		if (!get_check("Do you really want to commit suicide? ")) return;
#endif

		if (!noscore)
		{
			/* Special Verification for suicide */
#ifdef JP
			prt("確認のため '@' を押して下さい。", 0, 0);
#else
			prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
#endif
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

	/* Leaving */
	p_ptr->leaving = TRUE;

	/* Cause of death */
#ifdef JP
	(void)strcpy(died_from, "途中終了");
#else
	(void)strcpy(died_from, "Quitting");
#endif
}


/*
 * Save the game
 */
void do_cmd_save_game(int is_autosave)
{
	/* Autosaves do not disturb */
	if (is_autosave)
	{
#ifdef JP
		msg_print("自動セーブ中");
#else
		msg_print("Autosaving the game...");
#endif
	}
	else
	{
		/* Disturb the player */
		disturb(1, 0);
	}

	/* Clear messages */
	msg_print(NULL);

	/* Handle stuff */
	handle_stuff();

	/* Message */
#ifdef JP
	prt("ゲームをセーブしています...", 0, 0);
#else
	prt("Saving game...", 0, 0);
#endif

	/* Refresh */
	Term_fresh();

	/* The player is not dead */
#ifdef JP
	(void)strcpy(died_from, "(セーブ)");
#else
	(void)strcpy(died_from, "(saved)");
#endif

	/* Forbid suspend */
	signals_ignore_tstp();

	/* Save the player */
	if (save_player())
	{
#ifdef JP
		prt("ゲームをセーブしています... 終了", 0, 0);
#else
		prt("Saving game... done.", 0, 0);
#endif
	}

	/* Save failed (oops) */
	else
	{
#ifdef JP
		prt("ゲームをセーブしています... 失敗！", 0, 0);
#else
		prt("Saving game... failed!", 0, 0);
#endif
	}

	/* Allow suspend again */
	signals_handle_tstp();

	/* Update lite/view */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);
	handle_stuff();

	/* Refresh */
	Term_fresh();
	
	/* Clear messages. */
	msg_print(NULL);

	/* Note that the player is not dead */
#ifdef JP
	(void)strcpy(died_from, "(元気に生きている)");
#else
	(void)strcpy(died_from, "(alive and well)");
#endif
}


/*
 * Save the game and exit
 */
void do_cmd_save_and_exit(void)
{
	alive = FALSE;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * How much valuable stuff do we carry. Returns total cost of all found
 * items currently in equipment (ie. those that haven't been storebought
 * or started with)
 */

static long equip_value(void)
{
	object_type *o_ptr;
	long total = 0L;
	int i;

	for (i = 1; i < INVEN_TOTAL; i++)
	{
	    o_ptr = &inventory[i];

		 if (o_ptr->ident & IDENT_STORE) continue;
		 if (!(o_ptr->ident & IDENT_KNOWN)) continue;
	    total += object_value(o_ptr);
	}

	return (total);

}

/*
 * Hack -- Calculates the total number of points earned         -JWT-
 * Now with up to 80% penalty for having mutations & other extra things  -GSN-
 * Fixed this up to be "fairer" -CK-
 */
long total_points(void)
{
	long temp;
	long mult = 100;

	if (preserve_mode) mult -= 10; /* Penalize preserve, maximize modes */
	if (X_stupid_monsters) mult -= 20; /* AI is not that big a deal (yet) */
#ifndef TINYANGBAND
	/* Not too much of a reward since some people like playing with this. */
	if (ironman_small_levels) mult += 5;
	if (ironman_empty_levels) mult += 10;
	if (ironman_rooms) mult +=10;
#endif
	if (ironman_downward) mult +=10;
	if (ironman_nightmare) mult += 20;

	if (mult < 5) mult = 5; /* At least 5% of the original score */

	temp = p_ptr->max_exp + (100 * p_ptr->max_dlv);

	temp = (temp * mult / race_info[p_ptr->prace].r_exp);

	temp += (long)(equip_value() / 10);

	if (ironman_downward) temp *= 2;

	return (temp);
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
	FILE                *fp;

	char                str[1024];


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
			path_build(str, sizeof(str), ANGBAND_DIR_BONE, tmp);

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
		cptr	p;

		char	tmp[160];

		char	buf[1024];
#ifndef JP
		char    dummy[80];
#endif

		FILE        *fp;

		time_t	ct = time((time_t)0);


		/* Clear screen */
		Term_clear();

		/* Build the filename */
#ifdef JP
		path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "dead_j.txt");
#else
		path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "dead.txt");
#endif


		/* Open the News file */
		fp = my_fopen(buf, "r");

		/* Dump */
		if (fp)
		{
			int i = 0;

			/* Dump the file to the screen */
			while (0 == my_fgets(fp, buf, sizeof(buf)))
			{
				/* Display and advance */
				put_str(buf, i++, 0);
			}

			/* Close */
			my_fclose(fp);
		}


		/* King or Queen */
		if (total_winner || (p_ptr->lev > PY_MAX_LEVEL))
		{
#ifdef JP
			p= "偉大なる者";
#else
			p = "Magnificent";
#endif

		}

		/* Normal */
		else
		{
			p =  player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5];
		}

		center_string(buf, player_name);
		put_str(buf, 6, 11);

#ifndef JP
		center_string(buf, "the");
		put_str(buf, 7, 11);
#endif

		center_string(buf, p);
		put_str(buf, 8, 11);


		center_string(buf, cp_ptr->title);

		put_str(buf, 10, 11);

#ifdef JP
(void)sprintf(tmp, "レベル: %d", (int)p_ptr->lev);
#else
		(void)sprintf(tmp, "Level: %d", (int)p_ptr->lev);
#endif

		center_string(buf, tmp);
		put_str(buf, 11, 11);

#ifdef JP
(void)sprintf(tmp, "経験値: %ld", (long)p_ptr->exp);
#else
		(void)sprintf(tmp, "Exp: %ld", (long)p_ptr->exp);
#endif

		center_string(buf, tmp);
		put_str(buf, 12, 11);

#ifdef JP
(void)sprintf(tmp, "所持金: %ld", (long)p_ptr->au);
#else
		(void)sprintf(tmp, "AU: %ld", (long)p_ptr->au);
#endif

		center_string(buf, tmp);
		put_str(buf, 13, 11);

#ifdef JP
	/* 墓に刻む言葉をオリジナルより細かく表示 */
	if (streq(died_from, "途中終了"))
	{
		strcpy(tmp, "<自殺>");
	}
	else
	{
		if (streq(died_from, "ripe"))
		{
			strcpy(tmp, "引退後に天寿を全う");
		}
		else
		{
			strcpy(tmp, died_from);
		}
	}
	center_string(buf, tmp);
	put_str(buf, 14, 11);

	if(!streq(died_from, "ripe"))
	{
		if( dun_level == 0 )
		{
			if(streq(died_from, "途中終了"))
			{
				strcpy(tmp, "町で死んだ");
			}
			else
			{
				strcpy(tmp, "に町で殺された");
			}
		}
		else
		{
			if(streq(died_from, "途中終了"))
			{
				sprintf(tmp, "地下 %d 階で死んだ", dun_level);
			}
			else
			{
				sprintf(tmp, "に地下 %d 階で殺された", dun_level);
			}
		}
		center_string(buf, tmp);
		put_str(buf, 15, 11);
	}
#else
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
#endif



		(void)sprintf(tmp, "%-.24s", ctime(&ct));
		center_string(buf, tmp);
		put_str(buf, 17, 11);

#ifdef JP
msg_format("さようなら、%s!", player_name);
#else
		msg_format("Goodbye, %s!", player_name);
#endif

	}
}


/*
 * Display some character info
 */
static void show_info(void)
{
	int             i, j, k, l;
	object_type		*o_ptr;
	store_type		*st_ptr;

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
	}

	for (i = 1; i < max_towns; i++)
	{
		st_ptr = &town[i].store[STORE_HOME];

		/* Hack -- Know everything in the home */
		for (j = 0; j < st_ptr->stock_num; j++)
		{
			o_ptr = &st_ptr->stock[j];

			/* Skip non-objects */
			if (!o_ptr->k_idx) continue;

			/* Aware and Known */
			object_aware(o_ptr);
			object_known(o_ptr);
			o_ptr->ident |= IDENT_MENTAL;
		}
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
#ifdef JP
prt("キャラクターの記録をファイルに書き出すことができます。", 21, 0);
prt("リターンキーでキャラクターを見ます。ESCで中断します。", 22, 0);
#else
	prt("You may now dump a character record to one or more files.", 21, 0);
	prt("Then, hit RETURN to see the character, or ESC to abort.", 22, 0);
#endif


	/* Dump character records as requested */
	while (TRUE)
	{
		char out_val[160];

		/* Prompt */
#ifdef JP
put_str("ファイルネーム: ", 23, 0);
#else
		put_str("Filename: ", 23, 0);
#endif


		/* Default */
		strcpy(out_val, "");

		/* Ask for filename (or abort) */
		if (!askfor(out_val, 60)) return;

		/* Return means "show on screen" */
		if (!out_val[0]) break;

		/* Save screen */
		screen_save();

		/* Dump a character file */
		(void)file_character(out_val, TRUE);

		/* Load screen */
		screen_load();
	}


	/* Display player */
	display_player(DISPLAY_PLAYER_STANDARD);

	/* Prompt for inventory */
#ifdef JP
prt("何かキーを押すとさらに情報が続きます (ESCで中断): ", 23, 0);
#else
	prt("Hit any key to see more information (ESC to abort): ", 23, 0);
#endif


	/* Allow abort at this point */
	if (inkey() == ESCAPE) return;


	/* Show equipment and inventory */

	/* Equipment -- if any */
	if (equip_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_equip();
#ifdef JP
prt("装備していたアイテム: -続く-", 0, 0);
#else
		prt("You are using: -more-", 0, 0);
#endif

		if (inkey() == ESCAPE) return;
	}

	/* Inventory -- if any */
	if (inven_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_inven();
#ifdef JP
prt("持っていたアイテム: -続く-", 0, 0);
#else
		prt("You are carrying: -more-", 0, 0);
#endif

		if (inkey() == ESCAPE) return;
	}

	/* Homes in the different towns */
	for (l = 1; l < max_towns; l++)
	{
		st_ptr = &town[l].store[STORE_HOME];

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
					char o_name[MAX_NLEN];
					char tmp_val[80];

					/* Acquire item */
					o_ptr = &st_ptr->stock[i];

					/* Print header, clear line */
					sprintf(tmp_val, "%c) ", I2A(j));
					prt(tmp_val, j+2, 4);

					/* Display object description */
					object_desc(o_name, o_ptr, 0);
					c_put_str(tval_to_attr[o_ptr->tval], o_name, j+2, 7);
				}

				/* Caption */
#ifdef JP
prt(format("我が家に置いてあったアイテム ( %d ページ): -続く-", k+1), 0, 0);
#else
				prt(format("Your home contains (page %d): -more-", k+1), 0, 0);
#endif


				/* Wait for it */
				if (inkey() == ESCAPE) return;
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
	msg_print(NULL);

	/* Flush the input */
	flush();


	/* No suspending now */
	signals_ignore_tstp();


	/* Hack -- Character is now "icky" */
	character_icky = TRUE;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Open the high score file, for reading/writing */
	highscore_fd = fd_open(buf, O_RDWR);


	/* Handle death */
	if (death)
	{
		/* Handle retirement */
		if (total_winner)
		{
			/* Save winning message to notes file. */
			if (take_notes)
			{
				add_note_type(NOTE_WINNER);
			}

			kingly();
		}

		/* Save memories */
#ifdef JP
		if (!munchkin_death || get_check("死んだデータをセーブしますか？ "))
#else
		if (!munchkin_death || get_check("Save death? "))
#endif

#ifdef JP
if (!save_player()) msg_print("セーブ失敗！");
#else
			if (!save_player()) msg_print("death save failed!");
#endif


#if 0
		/* Dump bones file */
		make_bones();
#endif

		/* Inform notes file that you are dead */
		if (take_notes) add_note(died_from, 'd');

		/* You are dead */
		print_tomb();

		/* Show more info */
		show_info();

		/* Handle score, show Top scores */
		(void)top_twenty();
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
#ifdef JP
prt("リターンキーか ESC キーを押して下さい。", 0, 40);
#else
		prt("Press Return (or Escape).", 0, 40);
#endif


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
#ifdef JP
if (!character_generated || character_saved) quit("緊急事態");
#else
	if (!character_generated || character_saved) quit("panic");
#endif


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
#ifdef JP
(void)strcpy(died_from, "(緊急セーブ)");
#else
	(void)strcpy(died_from, "(panic save)");
#endif


	/* Panic save, or get worried */
#ifdef JP
if (!save_player()) quit("緊急セーブ失敗！");
#else
	if (!save_player()) quit("panic save failed!");
#endif


	/* Successful panic save */
#ifdef JP
quit("緊急セーブ成功！");
#else
	quit("panic save succeeded!");
#endif

}


/*
 * Get a random line from a file
 * Based on the monster speech patch by Matt Graham,
 */
errr get_rnd_line(cptr file_name, int entry, char *output)
{
	FILE    *fp;
	char    buf[1024];
	int     counter, test;
	int     line_num = 0;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, file_name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Failed */
	if (!fp) return -1;

	/* Find the entry of the monster */
	while (TRUE)
	{
		/* Get a line from the file */
		if (my_fgets(fp, buf, sizeof(buf)) == 0)
		{
			/* Count the lines */
			line_num++;

			/* Look for lines starting with 'N:' */
			if ((buf[0] == 'N') && (buf[1] == ':'))
			{
				/* Allow default lines */
				if (buf[2] == '*')
				{
					/* Default lines */
					break;
				}
				else if (buf[2] == 'M')
				{
					if (r_info[entry].flags1 & RF1_MALE) break;
				}
				else if (buf[2] == 'F')
				{
					if (r_info[entry].flags1 & RF1_FEMALE) break;
				}
				/* Get the monster number */
				else if (sscanf(&(buf[2]), "%d", &test) != EOF)
				{
					/* Is it the right number? */
					if (test == entry) break;
				}
				else
				{
					/* Error while converting the number */
					msg_format("Error in line %d of %s!", line_num, file_name);
					my_fclose(fp);
					return -1;
				}
			}
		}
		else
		{
			/* Reached end of file */
			my_fclose(fp);
			return -1;
		}
	}

	/* Get the random line */
	for (counter = 0; ; counter++)
	{
		while (TRUE)
		{
			test = my_fgets(fp, buf, sizeof(buf));

			/* Count the lines */
			/* line_num++; No more needed */

			if (!test)
			{
				/* Ignore lines starting with 'N:' */
				if ((buf[0] == 'N') && (buf[1] == ':')) continue;

				if (buf[0] != '#') break;
			}
			else break;
		}

		/* Abort */
		if (!buf[0]) break;

		/* Copy the line */
		if (one_in_(counter + 1)) strcpy(output, buf);
	}

	/* Close the file */
	my_fclose(fp);

	/* Success */
	return counter ? 0 : -1;
}


#ifdef JP
errr get_rnd_line_jonly(cptr file_name, int entry, char *output, int count)
{
  int i,j,kanji;
  errr result = 1;
  for (i = 0; i < count; i++){
    result = get_rnd_line(file_name, entry, output);
    if(result) break;
    kanji = 0;
    for(j = 0 ; output[j]; j++) kanji |= iskanji(output[j]);
    if(kanji) break;
  }
  return(result);
}
#endif

/*
 * Process file for auto picker/destroyer.
 */
errr process_autopick_file(cptr name)
{
	char buf[1024];

	errr err;

	/* Drop priv's */
	safe_setuid_drop();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

	err = process_pref_file_aux(buf, TRUE);

	/* Grab priv's */
	safe_setuid_grab();

	/* Result */
	return (err);
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
#ifdef JP
(void)strcpy(died_from, "強制終了");
#else
		(void)strcpy(died_from, "Abortion");
#endif

		forget_lite();
		forget_view();
		clear_mon_lite();

		/* Close stuff */
		close_game();

		/* Quit */
#ifdef JP
quit("強制終了");
#else
		quit("interrupt");
#endif

	}

	/* Allow suicide (after 5) */
	else if (signal_count >= 5)
	{
		/* Cause of "death" */
#ifdef JP
(void)strcpy(died_from, "強制終了中");
#else
		(void)strcpy(died_from, "Interrupting");
#endif

		forget_lite();
		forget_view();
		clear_mon_lite();

		/* Stop playing */
		alive = FALSE;

		/* Suicide */
		death = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Close stuff */
		close_game();

		/* Quit */
#ifdef JP
quit("強制終了");
#else
		quit("interrupt");
#endif

	}

	/* Give warning (after 4) */
	else if (signal_count >= 4)
	{
		/* Make a noise */
		Term_xtra(TERM_XTRA_NOISE, 0);

		/* Clear the top line */
		Term_erase(0, 0, 255);

		/* Display the cause */
#ifdef JP
Term_putstr(0, 0, -1, TERM_WHITE, "熟慮の上の自殺！");
#else
		Term_putstr(0, 0, -1, TERM_WHITE, "Contemplating suicide!");
#endif


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
	int wid, hgt, rows;

	Term_get_size(&wid, &hgt);
	rows = hgt - 4;

	/* Disable handler */
	(void)signal(sig, SIG_IGN);


	/* Nothing to save, just quit */
	if (!character_generated || character_saved) quit(NULL);


	forget_lite();
	forget_view();
	clear_mon_lite();

	/* Clear the bottom line */
	Term_erase(0, hgt - 1, 255);

	/* Give a warning */
#ifdef JP
	Term_putstr(0, hgt - 1, -1, TERM_RED,
"恐ろしいソフトのバグが飛びかかってきた！");
#else
	Term_putstr(0, hgt - 1, -1, TERM_RED,
		    "A gruesome software bug LEAPS out at you!");
#endif


	/* Message */
#ifdef JP
Term_putstr(45, hgt - 1, -1, TERM_RED, "緊急セーブ...");
#else
	Term_putstr(45, hgt - 1, -1, TERM_RED, "Panic save...");
#endif


	/* If note-taking enabled, write session end to notes file */
	if (take_notes) add_note_type(NOTE_ABORT_GAME);

	/* Flush output */
	Term_fresh();

	/* Panic Save */
	panic_save = 1;

	/* Panic save */
#ifdef JP
(void)strcpy(died_from, "(緊急セーブ)");
#else
	(void)strcpy(died_from, "(panic save)");
#endif


	/* Forbid suspend */
	signals_ignore_tstp();

	/* Attempt to save */
	if (save_player())
	{
#ifdef JP
Term_putstr(45, hgt - 1, -1, TERM_RED, "緊急セーブ成功！");
#else
		Term_putstr(45, hgt - 1, -1, TERM_RED, "Panic save succeeded!");
#endif

	}

	/* Save failed */
	else
	{
#ifdef JP
Term_putstr(45, hgt - 1, -1, TERM_RED, "緊急セーブ失敗！");
#else
		Term_putstr(45, hgt - 1, -1, TERM_RED, "Panic save failed!");
#endif

	}

	/* Flush output */
	Term_fresh();

	/* Quit */
#ifdef JP
quit("ソフトのバグ");
#else
	quit("software bug");
#endif

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
