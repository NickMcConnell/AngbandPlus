/* File: files.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: code dealing with files (and death) */

#include "angband.h"
#include "equip.h"
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

/* tokenize, but with user supplied delimiters and no special backslash/quote handling */
int string_split(char *buf, char **tokens, int max, cptr delim)
{
    int i = 0;
    char *s = buf;

    /* inch-worm alogorithm: s marks the start of the current token
       while t scans ahead for the next delimiter. buf is destroyed. */
    while (i < max - 1)
    {
        char *t;

        for (t = s; *t; t++)
        {
            if (strchr(delim, *t)) break;
        }

        if (!*t) break;
        *t++ = '\0';
        tokens[i++] = s;
        s = t;
    }

    tokens[i++] = s;
    return i;
}

void trim_tokens(char **tokens, int ct)
{
    int i;
    for (i = 0; i < ct; i++)
    {
        char *s = tokens[i];
        char *t = s + strlen(s) - 1;

        while (*s && *s == ' ')
            s++;

        while (*t && *t == ' ' && t > s)
            t--;

        t++;
        *t = '\0';
        tokens[i] = s;
    }
}

/* Name(arg1, arg2, arg3) */
int parse_args(char *buf, char **name, char **args, int max)
{
    char *s = buf;
    char *t;
    int   ct = 0;
    
    for (t = s; *t; t++)
    {
        if (*t == '(') break;
    }

    if (!*t)
    {
        *name = s;
        return 0;
    }

    *t++ = '\0';
    *name = s;
    s = t;

    for (t = s; *t; t++)
    {
        if (*t == ')') break;
    }

    if (!*t)
    {
        /* Parse error ... */
        return -1;
    }

    *t++ = '\0';
    ct = string_split(s, args, max, ",");

    trim_tokens(args, ct);
    return ct;
}


/* A number with a name */
typedef struct named_num named_num;

struct named_num
{
    cptr name;        /* The name of this thing */
    int num;            /* A number associated with it */
};


/* Index of spell type names */
static named_num gf_desc[] =
{
    {"GF_ELEC",                 GF_ELEC                },
    {"GF_POIS",                 GF_POIS                },
    {"GF_ACID",                 GF_ACID                },
    {"GF_COLD",                 GF_COLD                },
    {"GF_FIRE",                     GF_FIRE                },
    {"GF_PSY_SPEAR",            GF_PSY_SPEAR            },
    {"GF_MISSILE",                GF_MISSILE            },
    {"GF_ARROW",                GF_ARROW                },
    {"GF_PLASMA",                GF_PLASMA            },
    {"GF_WATER",                GF_WATER                },
    {"GF_LITE",                    GF_LITE                },
    {"GF_DARK",                    GF_DARK                },
    {"GF_LITE_WEAK",            GF_LITE_WEAK        },
    {"GF_DARK_WEAK",            GF_DARK_WEAK        },
    {"GF_SHARDS",                GF_SHARDS            },
    {"GF_SOUND",                GF_SOUND                },
    {"GF_CONFUSION",            GF_CONFUSION        },
    {"GF_FORCE",                GF_FORCE                },
    {"GF_INERTIA",                GF_INERT            },
    {"GF_MANA",                    GF_MANA                },
    {"GF_METEOR",                GF_METEOR            },
    {"GF_ICE",                    GF_ICE                },
    {"GF_CHAOS",                GF_CHAOS                },
    {"GF_NETHER",                GF_NETHER            },
    {"GF_DISENCHANT",            GF_DISENCHANT        },
    {"GF_NEXUS",                GF_NEXUS                },
    {"GF_TIME",                    GF_TIME                },
    {"GF_GRAVITY",                GF_GRAVITY            },
    {"GF_KILL_WALL",            GF_KILL_WALL        },
    {"GF_KILL_DOOR",            GF_KILL_DOOR        },
    {"GF_KILL_TRAP",            GF_KILL_TRAP        },
    {"GF_MAKE_WALL",            GF_MAKE_WALL        },
    {"GF_MAKE_DOOR",            GF_MAKE_DOOR        },
    {"GF_MAKE_TRAP",            GF_MAKE_TRAP        },
    {"GF_MAKE_TREE",            GF_MAKE_TREE        },
    {"GF_OLD_CLONE",            GF_OLD_CLONE        },
    {"GF_OLD_POLY",            GF_OLD_POLY            },
    {"GF_OLD_HEAL",            GF_OLD_HEAL            },
    {"GF_OLD_SPEED",        GF_OLD_SPEED        },
    {"GF_OLD_SLOW",            GF_OLD_SLOW            },
    {"GF_OLD_CONF",            GF_OLD_CONF            },
    {"GF_OLD_SLEEP",        GF_OLD_SLEEP        },
    {"GF_OLD_DRAIN",        GF_OLD_DRAIN        },
    {"GF_AWAY_UNDEAD",        GF_AWAY_UNDEAD        },
    {"GF_AWAY_EVIL",        GF_AWAY_EVIL        },
    {"GF_AWAY_ALL",            GF_AWAY_ALL            },
    {"GF_TURN_UNDEAD",        GF_TURN_UNDEAD        },
    {"GF_TURN_EVIL",        GF_TURN_EVIL        },
    {"GF_TURN_ALL",            GF_TURN_ALL            },
    {"GF_DISP_UNDEAD",        GF_DISP_UNDEAD        },
    {"GF_DISP_EVIL",        GF_DISP_EVIL        },
    {"GF_DISP_ALL",            GF_DISP_ALL            },
    {"GF_DISP_DEMON",        GF_DISP_DEMON        },
    {"GF_DISP_LIVING",        GF_DISP_LIVING        },
    {"GF_ROCKET",            GF_ROCKET            },
    {"GF_NUKE",                GF_NUKE                },
    {"GF_MAKE_GLYPH",        GF_MAKE_GLYPH        },
    {"GF_STASIS",            GF_STASIS            },
    {"GF_STONE_WALL",        GF_STONE_WALL        },
    {"GF_DEATH_RAY",        GF_DEATH_RAY        },
    {"GF_STUN",                GF_STUN                },
    {"GF_HOLY_FIRE",        GF_HOLY_FIRE        },
    {"GF_HELL_FIRE",        GF_HELL_FIRE        },
    {"GF_DISINTEGRATE",        GF_DISINTEGRATE    },
    {"GF_CHARM",            GF_CHARM                },
    {"GF_CONTROL_UNDEAD",    GF_CONTROL_UNDEAD    },
    {"GF_CONTROL_ANIMAL",    GF_CONTROL_ANIMAL    },
    {"GF_PSI",                GF_PSI                },
    {"GF_PSI_DRAIN",        GF_PSI_DRAIN        },
    {"GF_TELEKINESIS",        GF_TELEKINESIS        },
    {"GF_JAM_DOOR",            GF_JAM_DOOR            },
    {"GF_DOMINATION",        GF_DOMINATION        },
    {"GF_DISP_GOOD",        GF_DISP_GOOD        },
    {"GF_DRAIN_MANA",        GF_DRAIN_MANA        },
    {"GF_MIND_BLAST",        GF_MIND_BLAST        },
    {"GF_BRAIN_SMASH",        GF_BRAIN_SMASH        },
    {"GF_CAUSE_1",            GF_CAUSE_1        },
    {"GF_CAUSE_2",            GF_CAUSE_2        },
    {"GF_CAUSE_3",            GF_CAUSE_3        },
    {"GF_CAUSE_4",            GF_CAUSE_4        },
    {"GF_HAND_DOOM",        GF_HAND_DOOM        },
    {"GF_CAPTURE",            GF_CAPTURE        },
    {"GF_ANIM_DEAD",        GF_ANIM_DEAD        },
    {"GF_CONTROL_LIVING",    GF_CONTROL_LIVING    },
    {"GF_IDENTIFY",            GF_IDENTIFY    },
    {"GF_ATTACK",            GF_ATTACK    },
    {"GF_ENGETSU",            GF_ENGETSU    },
    {"GF_GENOCIDE",            GF_GENOCIDE    },
    {"GF_PHOTO",            GF_PHOTO    },
    {"GF_CONTROL_DEMON",    GF_CONTROL_DEMON    },
    {"GF_LAVA_FLOW",        GF_LAVA_FLOW    },
    {"GF_BLOOD_CURSE",        GF_BLOOD_CURSE    },
    {"GF_SEEKER",            GF_SEEKER            },
    {"GF_SUPER_RAY",        GF_SUPER_RAY            },
    {"GF_STAR_HEAL",        GF_STAR_HEAL            },
    {"GF_WATER_FLOW",        GF_WATER_FLOW            },
    {"GF_CRUSADE",            GF_CRUSADE            },
    {"GF_STASIS_EVIL",        GF_STASIS_EVIL        },
    {"GF_WOUNDS",            GF_WOUNDS        },
    {"GF_BLOOD",             GF_BLOOD                },
    {"GF_ELDRITCH",             GF_ELDRITCH                },
    {"GF_ELDRITCH_STUN",    GF_ELDRITCH_STUN        },
    {"GF_ELDRITCH_DRAIN",    GF_ELDRITCH_DRAIN        },
    {"GF_ELDRITCH_DISPEL",    GF_ELDRITCH_DISPEL        },
    {"GF_ELDRITCH_CONFUSE",    GF_ELDRITCH_CONFUSE        },
    {"GF_PHARAOHS_CURSE",    GF_PHARAOHS_CURSE        },
    {NULL,                     0                        }
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


    /* Require "?:*" format */
    if (buf[1] != ':') return 1;


    switch (buf[0])
    {
    /* Process "R:<num>:<a>/<c>" -- attr/char for monster races */
    case 'R':
        if (tokenize(buf+2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
        {
            monster_race *r_ptr;
            i = (huge)strtol(zz[0], NULL, 0);
            n1 = strtol(zz[1], NULL, 0);
            n2 = strtol(zz[2], NULL, 0);
            if (i >= max_r_idx) return 1;
            r_ptr = &r_info[i];
            if (n1 || (!(n2 & 0x80) && n2)) r_ptr->x_attr = n1; /* Allow TERM_DARK text */
            if (n2) r_ptr->x_char = n2;
            return 0;
        }
        break;

    /* Process "K:<num>:<a>/<c>"  -- attr/char for object kinds */
    case 'K':
        if (tokenize(buf+2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
        {
            object_kind *k_ptr;
            i = (huge)strtol(zz[0], NULL, 0);
            n1 = strtol(zz[1], NULL, 0);
            n2 = strtol(zz[2], NULL, 0);
            if (i >= max_k_idx) return 1;
            k_ptr = &k_info[i];
            if (n1 || (!(n2 & 0x80) && n2)) k_ptr->x_attr = n1; /* Allow TERM_DARK text */
            if (n2) k_ptr->x_char = n2;
            return 0;
        }
        break;

    /* Process "F:<num>:<a>/<c>" -- attr/char for terrain features */
    /* "F:<num>:<a>/<c>" */
    /* "F:<num>:<a>/<c>:LIT" */
    /* "F:<num>:<a>/<c>:<la>/<lc>:<da>/<dc>" */
    case 'F':
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
        }
        return 0;

    /* Process "S:<num>:<a>/<c>" -- attr/char for special things */
    case 'S':
        if (tokenize(buf+2, 3, zz, TOKENIZE_CHECKQUOTE) == 3)
        {
            j = (byte)strtol(zz[0], NULL, 0);
            n1 = strtol(zz[1], NULL, 0);
            n2 = strtol(zz[2], NULL, 0);
            misc_to_attr[j] = n1;
            misc_to_char[j] = n2;
            return 0;
        }
        break;

    /* Process "U:<tv>:<a>/<c>" -- attr/char for unaware items */
    case 'U':
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
            return 0;
        }
        break;

    /* Process "E:<tv>:<a>" -- attribute for inventory objects */
    case 'E':
        if (tokenize(buf+2, 2, zz, TOKENIZE_CHECKQUOTE) == 2)
        {
            j = (byte)strtol(zz[0], NULL, 0) % 128;
            n1 = strtol(zz[1], NULL, 0);
            if (n1) tval_to_attr[j] = n1;
            return 0;
        }
        break;

    /* Process "A:<str>" -- save an "action" for later */
    case 'A':
        text_to_ascii(macro__buf, buf+2);
        return 0;

    /* Process "P:<str>" -- normal macro */
    case 'P':
    {
        char tmp[1024];

        text_to_ascii(tmp, buf+2);
        macro_add(tmp, macro__buf);
        return 0;
    }

    /* Process "C:<str>" -- create keymap */
    case 'C':
    {
        int mode;
        char tmp[1024];

        if (tokenize(buf+2, 2, zz, TOKENIZE_CHECKQUOTE) != 2) return 1;

        mode = strtol(zz[0], NULL, 0);
        if ((mode < 0) || (mode >= KEYMAP_MODES)) return 1;

        text_to_ascii(tmp, zz[1]);
        if (!tmp[0] || tmp[1]) return 1;
        i = (byte)(tmp[0]);

        string_free(keymap_act[mode][i]);

        keymap_act[mode][i] = string_make(macro__buf);

        return 0;
    }

    /* Process "V:<num>:<kv>:<rv>:<gv>:<bv>" -- visual info */
    case 'V':
        if (tokenize(buf+2, 5, zz, TOKENIZE_CHECKQUOTE) == 5)
        {
            i = (byte)strtol(zz[0], NULL, 0);
            angband_color_table[i][0] = (byte)strtol(zz[1], NULL, 0);
            angband_color_table[i][1] = (byte)strtol(zz[2], NULL, 0);
            angband_color_table[i][2] = (byte)strtol(zz[3], NULL, 0);
            angband_color_table[i][3] = (byte)strtol(zz[4], NULL, 0);
            return 0;
        }
        break;

    /* Process "X:<str>" -- turn option off */
    /* Process "Y:<str>" -- turn option on */
    case 'X':
    case 'Y':
        for (i = 0; option_info[i].o_desc; i++)
        {
            if (option_info[i].o_var &&
                option_info[i].o_text &&
                streq(option_info[i].o_text, buf + 2))
            {
                int os = option_info[i].o_set;
                int ob = option_info[i].o_bit;

                if ((p_ptr->playing || character_xtra) &&
                    (OPT_PAGE_BIRTH == option_info[i].o_page) && !p_ptr->wizard)
                {
                    msg_format("Birth options can not changed! '%s'", buf);
                    msg_print(NULL);
                    return 0;
                }

                if (buf[0] == 'X')
                {
                    /* Clear */
                    option_flag[os] &= ~(1L << ob);
                    (*option_info[i].o_var) = FALSE;
                }
                else
                {
                    /* Set */
                    option_flag[os] |= (1L << ob);
                    (*option_info[i].o_var) = TRUE;
                }
                return 0;
            }
        }

        /* don't know that option. ignore it.*/
        msg_format("Ignored invalid option: %s", buf);
        msg_print(NULL);
        return 0;

    /* Process "Z:<type>:<str>" -- set spell color */
    case 'Z':
    {
        /* Find the colon */
        char *t = my_strchr(buf + 2, ':');

        /* Oops */
        if (!t) return 1;

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
                return 0;
            }
        }

        break;
    }

    /* Initialize macro trigger names and a template */
    /* Process "T:<trigger>:<keycode>:<shift-keycode>" */
    /* Process "T:<template>:<modifier chr>:<modifier name>:..." */
    case 'T':
    {
        int tok = tokenize(buf+2, 2+MAX_MACRO_MOD, zz, 0);

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

        /* Process "T:<trigger>:<keycode>:<shift-keycode>" */
        else if (tok >= 2)
        {
            char buf[1024];
            int m;
            char *t, *s;
            if (max_macrotrigger >= MAX_MACRO_TRIG)
            {
                msg_print("Too many macro triggers!");
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

            if (tok == 3)
            {
                /* Key code of a combination of it with the shift key */
                macro_trigger_keycode[1][m] = string_make(zz[2]);
            }
            else
            {
                macro_trigger_keycode[1][m] = string_make(zz[1]);
            }
        }

        /* No error */
        return 0;
    }
    }

    /* Failure */
    return 1;
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
    static char tmp[10];

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
        else if (streq(t, "IOR") || streq(t, "OR"))
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
                if (*t && streq(t, "1")) v = "0";
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
                if (*t && atoi(p) > atoi(t)) v = "0";
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

                /* Compare two numbers instead of string */
                if (*t && atoi(p) < atoi(t)) v = "0";
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
        while (isprint(*s) && !my_strchr(" []", *s)) ++s;

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
                if (p_ptr->prace == RACE_DOPPELGANGER) /* Use appropriate visuals for mimicked race */
                    v = get_race_t()->name;
                else
                    v = get_true_race_t()->name;
            }

            /* Class */
            else if (streq(b+1, "CLASS"))
            {
                v = get_class_t()->name;
            }

            /* Player */
            else if (streq(b+1, "PLAYER"))
            {
                static char tmp_player_name[32];
                char *pn, *tpn;
                for (pn = player_name, tpn = tmp_player_name; *pn; pn++, tpn++)
                {
                    *tpn = my_strchr(" []", *pn) ? '_' : *pn;
                }
                *tpn = '\0';
                v = tmp_player_name;
            }

            /* First realm */
            else if (streq(b+1, "REALM1"))
            {
                v = realm_names[p_ptr->realm1];
            }

            /* Second realm */
            else if (streq(b+1, "REALM2"))
            {
                v = realm_names[p_ptr->realm2];
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

            /* Money */
            else if (streq(b+1, "MONEY"))
            {
                sprintf(tmp, "%09d", p_ptr->au);
                v = tmp;
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


#define PREF_TYPE_NORMAL   0
#define PREF_TYPE_AUTOPICK 1
#define PREF_TYPE_HISTPREF 2

/*
 * Open the "user pref file" and parse it.
 */
static errr process_pref_file_aux(cptr name, int preftype)
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
            switch (preftype)
            {
            case PREF_TYPE_AUTOPICK:
                (void)process_autopick_file(buf + 2);
                break;
            case PREF_TYPE_HISTPREF:
                (void)process_histpref_file(buf + 2);
                break;
            default:
                (void)process_pref_file(buf + 2);
                break;
            }

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
            if (preftype != PREF_TYPE_AUTOPICK)
                  break;
            err = process_autopick_file_command(buf);
        }
    }


    /* Error */
    if (err)
    {
        /* Print error message */
        /* ToDo: Add better error messages */
        msg_format("Error %d in line %d of file '%s'.", err, line, name);
        msg_format("Parsing '%s'", old);
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

    errr err1, err2;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_PREF, name);

    /* Process the system pref file */
    err1 = process_pref_file_aux(buf, PREF_TYPE_NORMAL);

    /* Stop at parser errors, but not at non-existing file */
    if (err1 > 0) return err1;


    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);
    
    /* Process the user pref file */
    err2 = process_pref_file_aux(buf, PREF_TYPE_NORMAL);


    /* User file does not exist, but read system pref file */
    if (err2 < 0 && !err1)
        return -2;

    /* Result of user file processing */
    return err2;
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

    char    buf[1024];


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

    char    buf[1024];

    char    temphost[MAXHOSTNAMELEN+1];
    char    thishost[MAXHOSTNAMELEN+1];


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


static void _print_field(int row, int col, cptr header, cptr value, byte attr, int width)
{
    Term_putstr(col, row, -1, TERM_WHITE, header);
    
    if (width)
        Term_putstr(col + width - strlen(value), row, -1, attr, value);
    else
        Term_putstr(col + strlen(header) + 1, row, -1, attr, value);
}

/*
 * Hack -- pass color info around this file
 */
static byte likert_color = TERM_WHITE;

/*
 * Returns a "rating" of x depending on y
 */
static cptr likert_aux(int x, int y)
{
    static char dummy[20] = "";

    /* Paranoia */
    if (y <= 0) y = 1;

    /* Negative value */
    if (x < 0)
    {
        likert_color = TERM_L_DARK;
        return "Very Bad";
    }

    /* Analyze the value */
    switch ((x / y))
    {
    case 0:
    case 1:
        likert_color = TERM_RED;
        return "Bad";

    case 2:
        likert_color = TERM_L_RED;
        return "Poor";

    case 3:
    case 4:
        likert_color = TERM_ORANGE;
        return "Fair";

    case 5:
        likert_color = TERM_YELLOW;
        return "Good";

    case 6:
        likert_color = TERM_YELLOW;
        return "Very Good";

    case 7:
    case 8:
        likert_color = TERM_L_GREEN;
        return "Excellent";

    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
        likert_color = TERM_GREEN;
        return "Superb";

    case 14:
    case 15:
    case 16:
    case 17:
        likert_color = TERM_BLUE;
        return "Heroic";

    default:
        likert_color = TERM_VIOLET;
        sprintf(dummy, "Legendary[%d]", (int)((((x / y) - 17) * 5) / 2));
        return dummy;
    }
}

static cptr likert(int x, int y)
{
    if (p_ptr->wizard) return format("%s (%d)", likert_aux(x, y), x);
    return likert_aux(x, y);
}

/*
 * Obtain the "flags" for the player as if he was an item
 */
static void player_flags(u32b flgs[TR_FLAG_SIZE])
{
    int i;
    class_t *class_ptr = get_class_t();
    race_t *race_ptr = get_race_t();

    /* Clear */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = 0L;

    if (class_ptr != NULL && class_ptr->get_flags)
        class_ptr->get_flags(flgs);

    if (race_ptr != NULL && race_ptr->get_flags)
        race_ptr->get_flags(flgs);

    /* Classes */
    switch (p_ptr->pclass)
    {
    case CLASS_SCOUT:
        if (p_ptr->lev >= 35)
            add_flag(flgs, TR_TELEPATHY);
        break;

    case CLASS_PSION:
        if (psion_blending()) add_flag(flgs, TR_STEALTH);
        if (psion_shielding()) add_flag(flgs, TR_FREE_ACT);
        if (psion_speed()) add_flag(flgs, TR_SPEED);
        break;

    case CLASS_NECROMANCER:
        if (p_ptr->lev >= 5) add_flag(flgs, TR_RES_COLD);
        if (p_ptr->lev >= 15) add_flag(flgs, TR_SEE_INVIS);
        if (p_ptr->lev >= 25) add_flag(flgs, TR_HOLD_LIFE);
        if (p_ptr->lev >= 35) add_flag(flgs, TR_RES_POIS);
        break;
    case CLASS_BLOOD_MAGE:
        add_flag(flgs, TR_REGEN);
        break;
    case CLASS_CHAOS_WARRIOR:
        if (p_ptr->lev > 29)
            add_flag(flgs, TR_RES_CHAOS);
        if (p_ptr->lev > 39)
            add_flag(flgs, TR_RES_FEAR);
        break;
    case CLASS_MINDCRAFTER:
        if (p_ptr->lev > 9)
            add_flag(flgs, TR_RES_FEAR);
        if (p_ptr->lev > 19)
            add_flag(flgs, TR_SUST_WIS);
        if (p_ptr->lev > 29)
            add_flag(flgs, TR_RES_CONF);
        if (p_ptr->lev > 39)
            add_flag(flgs, TR_TELEPATHY);
        break;
    case CLASS_BLOOD_KNIGHT:
        add_flag(flgs, TR_REGEN);
        break;
    case CLASS_ARCHAEOLOGIST:
        if (p_ptr->lev >= 20)
            add_flag(flgs, TR_SEE_INVIS);
        if (p_ptr->lev >= 38)
            add_flag(flgs, TR_RES_DARK);
        break;
    case CLASS_WARLOCK:
        switch(p_ptr->psubclass)
        {
        case PACT_UNDEAD:
            add_flag(flgs, TR_RES_COLD);
            if (p_ptr->lev > 9) add_flag(flgs, TR_STEALTH);
            if (p_ptr->lev > 14) add_flag(flgs, TR_RES_POIS);
            if (p_ptr->lev > 4) add_flag(flgs, TR_CON);
            if (p_ptr->lev > 29) 
            {
                add_flag(flgs, TR_RES_NETHER);
                add_flag(flgs, TR_HOLD_LIFE);
            }
            if (p_ptr->lev > 34) 
            {
                add_flag(flgs, TR_RES_DARK);
                add_flag(flgs, TR_RES_BLIND);
            }
            if (p_ptr->lev > 44) add_flag(flgs, TR_RES_SHARDS);
            break;        
        case PACT_DRAGON:
            add_flag(flgs, TR_RES_FEAR);
        /*    if (p_ptr->lev > 14) add_flag(flgs, TR_LEVITATION); */
        /*    Giving TR_STR flags the player as cursed?  Not sure what might break if I fix that seeming bug ...
            if (p_ptr->lev > 4) add_flag(flgs, TR_STR); */
            if (p_ptr->lev > 29) add_flag(flgs, TR_SUST_CON);
            break;
        case PACT_ANGEL:
            add_flag(flgs, TR_LEVITATION);
            if (p_ptr->lev > 14) add_flag(flgs, TR_SEE_INVIS);
            if (p_ptr->lev > 4) add_flag(flgs, TR_WIS);
            if (p_ptr->lev > 34) add_flag(flgs, TR_REFLECT);
            break;
        case PACT_DEMON:
            add_flag(flgs, TR_RES_FIRE);
            if (p_ptr->lev > 14) add_flag(flgs, TR_HOLD_LIFE);
            if (p_ptr->lev > 4) add_flag(flgs, TR_INT);
            /* This is firing, but not working.  Note, DEMON LORD Mimic does work????? 
               Oh, see player_immunity() */
            if (p_ptr->lev > 49) add_flag(flgs, TR_IM_FIRE);
            break;
        case PACT_ABERRATION:
            add_flag(flgs, TR_RES_CHAOS);
            if (p_ptr->lev > 4) add_flag(flgs, TR_DEX);
            if (p_ptr->lev > 34) add_flag(flgs, TR_TELEPATHY);
            break;
        }
        break;
    case CLASS_WEAPONMASTER:
        if (p_ptr->psubclass == WEAPONMASTER_DAGGERS)
        {
            if (p_ptr->speciality_equip)
            {
                if (p_ptr->lev >= 10) add_flag(flgs, TR_STEALTH);
            }
        }
        else if (p_ptr->psubclass == WEAPONMASTER_SHIELDS)
        {
            if (p_ptr->speciality_equip)
            {
                if (p_ptr->lev >= 45)
                {
                    add_flag(flgs, TR_RES_ACID);
                    add_flag(flgs, TR_RES_COLD);
                    add_flag(flgs, TR_RES_FIRE);
                    add_flag(flgs, TR_RES_ELEC);
                    add_flag(flgs, TR_REFLECT);
                }
            }
        }
        else if (p_ptr->psubclass == WEAPONMASTER_STAVES)
        {
            if (p_ptr->speciality_equip)
            {
                if (p_ptr->lev >= 20) add_flag(flgs, TR_SPEED);
            }
        }
    default:
        break; /* Do nothing */
    }

    /* Mutations */
    if (mut_present(MUT_FLESH_ROT))
    {
        remove_flag(flgs, TR_REGEN);
    }

    if (mut_present(MUT_XTRA_FAT) ||
        mut_present(MUT_XTRA_LEGS) ||
        mut_present(MUT_SHORT_LEG))
    {
        add_flag(flgs, TR_SPEED);
    }

    if (mut_present(MUT_ELEC_AURA))
    {
        add_flag(flgs, TR_SH_ELEC);
    }

    if (mut_present(MUT_FIRE_AURA))
    {
        add_flag(flgs, TR_SH_FIRE);
        add_flag(flgs, TR_LITE);
    }

    if (mut_present(MUT_WINGS))
    {
        add_flag(flgs, TR_LEVITATION);
    }

    if (mut_present(MUT_FEARLESS))
    {
        add_flag(flgs, TR_RES_FEAR);
    }

    if (mut_present(MUT_REGEN))
    {
        add_flag(flgs, TR_REGEN);
    }

    if (mut_present(MUT_ESP))
    {
        add_flag(flgs, TR_TELEPATHY);
    }

    if (mut_present(MUT_MOTION))
    {
        add_flag(flgs, TR_FREE_ACT);
    }

    if (mut_present(MUT_TREAD_SOFTLY))
    {
        add_flag(flgs, TR_STEALTH);
    }

    if (mut_present(MUT_DRACONIAN_SHIELD))
    {
        switch (p_ptr->psubrace)
        {
        case DRACONIAN_RED:
            add_flag(flgs, TR_SH_FIRE);
            break;
        case DRACONIAN_WHITE:
            add_flag(flgs, TR_SH_COLD);
            break;
        case DRACONIAN_BLUE:
            add_flag(flgs, TR_SH_ELEC);
            break;
        case DRACONIAN_CRYSTAL:
            add_flag(flgs, TR_SH_SHARDS);
            break;
        }
    }

    if (mut_present(MUT_DRACONIAN_REGEN))
    {
        add_flag(flgs, TR_REGEN);
    }

    if (p_ptr->personality == PERS_SEXY)
        add_flag(flgs, TR_AGGRAVATE);
    if (p_ptr->personality == PERS_MUNCHKIN)
    {
        add_flag(flgs, TR_RES_BLIND);
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_HOLD_LIFE);
        if (p_ptr->pclass != CLASS_NINJA) 
            add_flag(flgs, TR_LITE);
        if (p_ptr->lev > 9)
            add_flag(flgs, TR_SPEED);
    }
    if (p_ptr->personality == PERS_FEARLESS)
    {
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->personality == PERS_HASTY)
    {
        add_flag(flgs, TR_SPEED);
    }
    if (p_ptr->special_defense & KATA_FUUJIN)
        add_flag(flgs, TR_REFLECT);
    if (p_ptr->special_defense & KAMAE_GENBU)
        add_flag(flgs, TR_REFLECT);
    if (p_ptr->special_defense & KAMAE_SUZAKU)
        add_flag(flgs, TR_LEVITATION);
    if (p_ptr->special_defense & KAMAE_SEIRYU)
    {
        add_flag(flgs, TR_RES_FIRE);
        add_flag(flgs, TR_RES_COLD);
        add_flag(flgs, TR_RES_ACID);
        add_flag(flgs, TR_RES_ELEC);
        add_flag(flgs, TR_RES_POIS);
        add_flag(flgs, TR_LEVITATION);
        add_flag(flgs, TR_SH_FIRE);
        add_flag(flgs, TR_SH_ELEC);
        add_flag(flgs, TR_SH_COLD);
    }
    if (p_ptr->special_defense & KATA_MUSOU)
    {
        add_flag(flgs, TR_RES_FEAR);
        add_flag(flgs, TR_RES_LITE);
        add_flag(flgs, TR_RES_DARK);
        add_flag(flgs, TR_RES_BLIND);
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_SOUND);
        add_flag(flgs, TR_RES_SHARDS);
        add_flag(flgs, TR_RES_NETHER);
        add_flag(flgs, TR_RES_NEXUS);
        add_flag(flgs, TR_RES_CHAOS);
        add_flag(flgs, TR_RES_DISEN);
        add_flag(flgs, TR_REFLECT);
        add_flag(flgs, TR_HOLD_LIFE);
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SH_FIRE);
        add_flag(flgs, TR_SH_ELEC);
        add_flag(flgs, TR_SH_COLD);
        add_flag(flgs, TR_LEVITATION);
        add_flag(flgs, TR_LITE);
        add_flag(flgs, TR_SEE_INVIS);
        add_flag(flgs, TR_TELEPATHY);
        add_flag(flgs, TR_SLOW_DIGEST);
        add_flag(flgs, TR_REGEN);
        add_flag(flgs, TR_SUST_STR);
        add_flag(flgs, TR_SUST_INT);
        add_flag(flgs, TR_SUST_WIS);
        add_flag(flgs, TR_SUST_DEX);
        add_flag(flgs, TR_SUST_CON);
        add_flag(flgs, TR_SUST_CHR);
    }
}


static void tim_player_flags(u32b flgs[TR_FLAG_SIZE])
{
    int i;

    /* Clear */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = 0L;

    if (IS_HERO() || IS_SHERO())
        add_flag(flgs, TR_RES_FEAR);
    if (p_ptr->tim_invis)
        add_flag(flgs, TR_SEE_INVIS);
    if (p_ptr->tim_regen)
        add_flag(flgs, TR_REGEN);
    if (IS_TIM_ESP())
        add_flag(flgs, TR_TELEPATHY);
    if (IS_FAST() || p_ptr->slow)
        add_flag(flgs, TR_SPEED);

    if (IS_OPPOSE_ACID() && !(p_ptr->special_defense & DEFENSE_ACID) && !(prace_is_(RACE_YEEK) && (p_ptr->lev > 19)))
        add_flag(flgs, TR_RES_ACID);
    if (IS_OPPOSE_ELEC() && !(p_ptr->special_defense & DEFENSE_ELEC))
        add_flag(flgs, TR_RES_ELEC);
    if (IS_OPPOSE_FIRE() && !(p_ptr->special_defense & DEFENSE_FIRE))
        add_flag(flgs, TR_RES_FIRE);
    if (IS_OPPOSE_COLD() && !(p_ptr->special_defense & DEFENSE_COLD))
        add_flag(flgs, TR_RES_COLD);
    if (IS_OPPOSE_POIS())
        add_flag(flgs, TR_RES_POIS);

    if (p_ptr->tim_sustain_str) add_flag(flgs, TR_SUST_STR);
    if (p_ptr->tim_sustain_int) add_flag(flgs, TR_SUST_INT);
    if (p_ptr->tim_sustain_wis) add_flag(flgs, TR_SUST_WIS);
    if (p_ptr->tim_sustain_dex) add_flag(flgs, TR_SUST_DEX);
    if (p_ptr->tim_sustain_con) add_flag(flgs, TR_SUST_CON);
    if (p_ptr->tim_sustain_chr) add_flag(flgs, TR_SUST_CHR);
    if (p_ptr->tim_hold_life) add_flag(flgs, TR_HOLD_LIFE);
    if (p_ptr->tim_dark_stalker) add_flag(flgs, TR_STEALTH);

    if (p_ptr->special_attack & ATTACK_ACID)
        add_flag(flgs, TR_BRAND_ACID);
    if (p_ptr->special_attack & ATTACK_ELEC)
        add_flag(flgs, TR_BRAND_ELEC);
    if (p_ptr->special_attack & ATTACK_FIRE)
        add_flag(flgs, TR_BRAND_FIRE);
    if (p_ptr->special_attack & ATTACK_COLD)
        add_flag(flgs, TR_BRAND_COLD);
    if (p_ptr->special_attack & ATTACK_POIS)
        add_flag(flgs, TR_BRAND_POIS);
    if (p_ptr->special_defense & DEFENSE_ACID)
        add_flag(flgs, TR_IM_ACID);
    if (p_ptr->special_defense & DEFENSE_ELEC)
        add_flag(flgs, TR_IM_ELEC);
    if (p_ptr->special_defense & DEFENSE_FIRE)
        add_flag(flgs, TR_IM_FIRE);
    if (p_ptr->special_defense & DEFENSE_COLD)
        add_flag(flgs, TR_IM_COLD);
    if (IS_WRAITH())
        add_flag(flgs, TR_REFLECT);
    /* by henkma */
    if (p_ptr->tim_reflect)
        add_flag(flgs, TR_REFLECT);

    if (p_ptr->tim_blood_shield)
    {
        int amt = 100 * (p_ptr->mhp - p_ptr->chp) / p_ptr->mhp; 
        if (amt > 60)
            add_flag(flgs, TR_REFLECT);
    }

    if (p_ptr->tim_blood_seek)
    {
        /* TODO: Slay Living? */
    }

    if (p_ptr->magicdef)
    {
        add_flag(flgs, TR_RES_BLIND);
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_REFLECT);
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_LEVITATION);
    }
    if (p_ptr->tim_res_nether)
    {
        add_flag(flgs, TR_RES_NETHER);
    }
    if (p_ptr->tim_res_disenchantment)
    {
        add_flag(flgs, TR_RES_DISEN);
    }
    if (p_ptr->tim_sh_fire)
    {
        add_flag(flgs, TR_SH_FIRE);
    }
    if (p_ptr->tim_sh_shards)
    {
        add_flag(flgs, TR_SH_SHARDS);
    }
    if (p_ptr->tim_sh_elements)
    {
        add_flag(flgs, TR_SH_FIRE);
        if (p_ptr->lev >= 25)
            add_flag(flgs, TR_SH_COLD);
        if (p_ptr->lev >= 35)
            add_flag(flgs, TR_SH_ELEC);
    }
    if (p_ptr->ult_res)
    {
        add_flag(flgs, TR_RES_FEAR);
        add_flag(flgs, TR_RES_LITE);
        add_flag(flgs, TR_RES_DARK);
        add_flag(flgs, TR_RES_BLIND);
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_SOUND);
        add_flag(flgs, TR_RES_SHARDS);
        add_flag(flgs, TR_RES_NETHER);
        add_flag(flgs, TR_RES_NEXUS);
        add_flag(flgs, TR_RES_CHAOS);
        add_flag(flgs, TR_RES_DISEN);
        add_flag(flgs, TR_RES_TIME);
        add_flag(flgs, TR_REFLECT);
        add_flag(flgs, TR_HOLD_LIFE);
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SH_FIRE);
        add_flag(flgs, TR_SH_ELEC);
        add_flag(flgs, TR_SH_COLD);
        add_flag(flgs, TR_LEVITATION);
        add_flag(flgs, TR_LITE);
        add_flag(flgs, TR_SEE_INVIS);
        add_flag(flgs, TR_TELEPATHY);
        add_flag(flgs, TR_SLOW_DIGEST);
        add_flag(flgs, TR_REGEN);
        add_flag(flgs, TR_SUST_STR);
        add_flag(flgs, TR_SUST_INT);
        add_flag(flgs, TR_SUST_WIS);
        add_flag(flgs, TR_SUST_DEX);
        add_flag(flgs, TR_SUST_CON);
        add_flag(flgs, TR_SUST_CHR);
    }

    /* Hex bonuses */
    if (p_ptr->realm1 == REALM_HEX)
    {
        if (hex_spelling(HEX_DEMON_AURA))
        {
            add_flag(flgs, TR_SH_FIRE);
            add_flag(flgs, TR_REGEN);
        }
        if (hex_spelling(HEX_ICE_ARMOR)) add_flag(flgs, TR_SH_COLD);
        if (hex_spelling(HEX_SHOCK_CLOAK)) add_flag(flgs, TR_SH_ELEC);
    }
}


/* Mode flags for displaying player flags */
#define DP_CURSE   0x01
#define DP_IMM     0x02
#define DP_VULN    0x04
#define DP_WP      0x08

#define EQUIPPY_MAIN 1

/*
 * Equippy chars
 */
static void display_player_equippy(int y, int x, u16b mode)
{
    int i;

    byte a;
    char c;

    object_type *o_ptr;

    Term_erase(x, y, 12);

    /* Dump equippy chars */
    for (i = 0; i < equip_count(); i++)
    {
        int slot = EQUIP_BEGIN + i;
        o_ptr = equip_obj(slot);

        if (mode == EQUIPPY_MAIN && i >= 12) break; /* Hack: This will overwrite the map display otherwise ... */

        if (o_ptr && equippy_chars)
        {
            a = object_attr(o_ptr);
            c = object_char(o_ptr);
        }
        else
        {
            c = ' ';
            a = TERM_DARK;
        }
        Term_putch(x + i, y, a, c);
    }
}


void print_equippy(void)
{
    display_player_equippy(ROW_EQUIPPY, COL_EQUIPPY, EQUIPPY_MAIN);
}

static void known_obj_immunity(u32b flgs[TR_FLAG_SIZE])
{
    int i;

    /* Clear */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = 0L;

    /* Check equipment */
    for (i = EQUIP_BEGIN; i < EQUIP_BEGIN + equip_count(); i++)
    {
        u32b o_flgs[TR_FLAG_SIZE];
        object_type *o_ptr = equip_obj(i);

        if (!o_ptr) continue;

        object_flags_known(o_ptr, o_flgs);
        if (have_flag(o_flgs, TR_IM_ACID)) add_flag(flgs, TR_RES_ACID);
        if (have_flag(o_flgs, TR_IM_ELEC)) add_flag(flgs, TR_RES_ELEC);
        if (have_flag(o_flgs, TR_IM_FIRE)) add_flag(flgs, TR_RES_FIRE);
        if (have_flag(o_flgs, TR_IM_COLD)) add_flag(flgs, TR_RES_COLD);
    }
}

static void player_immunity(u32b flgs[TR_FLAG_SIZE])
{
    int i;
    race_t  *race_ptr = get_race_t();
    class_t *class_ptr = get_class_t();

    /* Clear */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = 0L;

    if (race_ptr->get_immunities)
        race_ptr->get_immunities(flgs);

    if (class_ptr && class_ptr->get_immunities)
        class_ptr->get_immunities(flgs);

    /* TODO: Move to warlock.c */
    if (p_ptr->pclass == CLASS_WARLOCK && p_ptr->psubclass == PACT_DEMON && p_ptr->lev > 49)
        add_flag(flgs, TR_RES_FIRE);
}

static void tim_player_immunity(u32b flgs[TR_FLAG_SIZE])
{
    int i;

    /* Clear */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = 0L;

    if (p_ptr->special_defense & DEFENSE_ACID)
        add_flag(flgs, TR_RES_ACID);
    if (p_ptr->special_defense & DEFENSE_ELEC)
        add_flag(flgs, TR_RES_ELEC);
    if (p_ptr->special_defense & DEFENSE_FIRE)
        add_flag(flgs, TR_RES_FIRE);
    if (p_ptr->special_defense & DEFENSE_COLD)
        add_flag(flgs, TR_RES_COLD);
    if (IS_WRAITH())
        add_flag(flgs, TR_RES_DARK);
}

static void player_vuln_flags(u32b flgs[TR_FLAG_SIZE])
{
    int i;
    race_t *race_ptr = get_race_t();

    /* Clear */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = 0L;

    if (race_ptr->get_vulnerabilities)
        race_ptr->get_vulnerabilities(flgs);

    if (mut_present(MUT_VULN_ELEM) || (p_ptr->special_defense & KATA_KOUKIJIN))
    {
        add_flag(flgs, TR_RES_ACID);
        add_flag(flgs, TR_RES_ELEC);
        add_flag(flgs, TR_RES_FIRE);
        add_flag(flgs, TR_RES_COLD);
    }        

    if (IS_WRAITH())
        add_flag(flgs, TR_RES_LITE);
}


/*
 * A struct for storing misc. flags
 */
typedef struct {
    u32b player_flags[TR_FLAG_SIZE];
    u32b tim_player_flags[TR_FLAG_SIZE];
    u32b player_imm[TR_FLAG_SIZE];
    u32b tim_player_imm[TR_FLAG_SIZE];
    u32b player_vuln[TR_FLAG_SIZE];
    u32b known_obj_imm[TR_FLAG_SIZE];
} all_player_flags;


/*
 * Helper function, see below
 */
static void display_flag_aux(int row, int col, cptr header,
                    int flag1, all_player_flags *f, u16b mode)
{
    int     i;
    bool    vuln = FALSE;

    if (have_flag(f->player_vuln, flag1) &&
        !(have_flag(f->known_obj_imm, flag1) ||
          have_flag(f->player_imm, flag1) ||
          have_flag(f->tim_player_imm, flag1)))
        vuln = TRUE;

    /* Header */
    if (!(mode & DP_IMM)) c_put_str(TERM_WHITE, header, row, col);

    /* Advance */
    col += strlen(header) + 1;

    /* Check equipment */
    for (i = 0; i < equip_count(); i++)
    {
        int slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);

        /* Default */
        if (!(mode & (DP_IMM | DP_VULN)))
            c_put_str((byte)(vuln ? TERM_RED : TERM_SLATE), ".", row, col);

        if (o_ptr)
        {
            u32b flgs[TR_FLAG_SIZE];
            object_flags_known(o_ptr, flgs);
            if (mode & DP_CURSE)
            {
                if ((mode & DP_CURSE) && (o_ptr->curse_flags & (TRC_CURSED | TRC_HEAVY_CURSE)))
                    c_put_str(TERM_WHITE, "+", row, col);
                if ((mode & DP_CURSE) && (o_ptr->curse_flags & TRC_PERMA_CURSE))
                    c_put_str(TERM_WHITE, "*", row, col);
            }
            else
            {
                if (have_flag(flgs, flag1))
                {
                    c_put_str((byte)(vuln ? TERM_L_RED : TERM_WHITE),
                          (mode & DP_IMM) ? "*" : 
                            (mode & DP_VULN) ? "-" : "+", 
                          row, col);
                }
            }
        }

        /* Advance */
        col++;
    }

    /* Assume that player flag is already written */
    if (mode & (DP_IMM | DP_VULN)) return;

    /* Default */
    c_put_str((byte)(vuln ? TERM_RED : TERM_SLATE), ".", row, col);

    /* Player flags */
    if (have_flag(f->player_flags, flag1)) c_put_str((byte)(vuln ? TERM_L_RED : TERM_WHITE), "+", row, col);

    /* Timed player flags */
    if (have_flag(f->tim_player_flags, flag1)) c_put_str((byte)(vuln ? TERM_ORANGE : TERM_YELLOW), "#", row, col);

    /* Immunity */
    if (have_flag(f->tim_player_imm, flag1)) c_put_str(TERM_YELLOW, "*", row, col);
    if (have_flag(f->player_imm, flag1)) c_put_str(TERM_WHITE, "*", row, col);

    /* Vulnerability */
    if (vuln) c_put_str(TERM_RED, "v", row, col + 1);
}

static int _known_res_pct(int which)
{
    int ct = p_ptr->resist[which];
    int hidden = 0;
    int flg = res_get_object_flag(which);
    int i;

    /* Life is a bit hard at the moment since "player flags"
       may account for multiple resistances. Really, the entire
       flag based approach to resistance is just wrong, but I'm
       too lazy to fix ...
    */
    for (i = 0; i < equip_count(); i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);
        u32b         flgs[TR_FLAG_SIZE];
        u32b         flgs_known[TR_FLAG_SIZE];

        if (!o_ptr) continue;
        object_flags(o_ptr, flgs);
        object_flags_known(o_ptr, flgs_known);

        if (have_flag(flgs, flg) && !have_flag(flgs_known, flg))
            hidden++;
    }

    ct -= hidden;

    return res_pct_aux(which, ct);
}

/*
 * Special display, part 1
 */
static void display_player_flag_info(void)
{
    int row, i;
    int col;

    all_player_flags f;

    /* Extract flags and store */
    player_flags(f.player_flags);
    tim_player_flags(f.tim_player_flags);
    player_immunity(f.player_imm);
    tim_player_immunity(f.tim_player_imm);
    known_obj_immunity(f.known_obj_imm);
    player_vuln_flags(f.player_vuln);

    /*** Resistances ***/
    row = 2;
    col = 1;

    display_player_equippy(row-2, col+13, 0);

    for (i = 0; i < equip_count(); i++)
        Term_putch(col + 13 + i, row - 1, TERM_WHITE, 'a' + i);
    Term_putch(col + 13 + equip_count(), row - 1, TERM_WHITE, '@');

    display_flag_aux(row+ 0, col, "Acid       :", TR_RES_ACID, &f, 0);
    display_flag_aux(row+ 0, col, "Acid       :", TR_IM_ACID, &f, DP_IMM);
    display_flag_aux(row+ 0, col, "Acid       :", TR_VULN_ACID, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_ACID)), row + 0, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 1, col, "Elec       :", TR_RES_ELEC, &f, 0);
    display_flag_aux(row+ 1, col, "Elec       :", TR_IM_ELEC, &f, DP_IMM);
    display_flag_aux(row+ 1, col, "Elec       :", TR_VULN_ELEC, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_ELEC)), row + 1, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 2, col, "Fire       :", TR_RES_FIRE, &f, 0);
    display_flag_aux(row+ 2, col, "Fire       :", TR_IM_FIRE, &f, DP_IMM);
    display_flag_aux(row+ 2, col, "Fire       :", TR_VULN_FIRE, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_FIRE)), row + 2, col + 13 + equip_count() + 1);
    
    display_flag_aux(row+ 3, col, "Cold       :", TR_RES_COLD, &f, 0);
    display_flag_aux(row+ 3, col, "Cold       :", TR_IM_COLD, &f, DP_IMM);
    display_flag_aux(row+ 3, col, "Cold       :", TR_VULN_COLD, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_COLD)), row + 3, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 4, col, "Poison     :", TR_RES_POIS, &f, 0);
    display_flag_aux(row+ 4, col, "Poison     :", TR_VULN_POIS, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_POIS)), row + 4, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 5, col, "Light      :", TR_RES_LITE, &f, 0);
    display_flag_aux(row+ 5, col, "Light      :", TR_VULN_LITE, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_LITE)), row + 5, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 6, col, "Dark       :", TR_RES_DARK, &f, 0);
    display_flag_aux(row+ 6, col, "Dark       :", TR_VULN_DARK, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_DARK)), row + 6, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 7, col, "Confusion  :", TR_RES_CONF, &f, 0);
    display_flag_aux(row+ 7, col, "Confusion  :", TR_VULN_CONF, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_CONF)), row + 7, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 8, col, "Nether     :", TR_RES_NETHER, &f, 0);
    display_flag_aux(row+ 8, col, "Nether     :", TR_VULN_NETHER, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_NETHER)), row + 8, col + 13 + equip_count() + 1);

    display_flag_aux(row+ 9, col, "Nexus      :", TR_RES_NEXUS, &f, 0);
    display_flag_aux(row+ 9, col, "Nexus      :", TR_VULN_NEXUS, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_NEXUS)), row + 9, col + 13 + equip_count() + 1);

    display_flag_aux(row+10, col, "Sound      :", TR_RES_SOUND, &f, 0);
    display_flag_aux(row+10, col, "Sound      :", TR_VULN_SOUND, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_SOUND)), row +10, col + 13 + equip_count() + 1);

    display_flag_aux(row+11, col, "Shards     :", TR_RES_SHARDS, &f, 0);
    display_flag_aux(row+11, col, "Shards     :", TR_VULN_SHARDS, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_SHARDS)), row +11, col + 13 + equip_count() + 1);

    display_flag_aux(row+12, col, "Chaos      :", TR_RES_CHAOS, &f, 0);
    display_flag_aux(row+12, col, "Chaos      :", TR_VULN_CHAOS, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_CHAOS)), row +12, col + 13 + equip_count() + 1);

    display_flag_aux(row+13, col, "Disenchant :", TR_RES_DISEN, &f, 0);
    display_flag_aux(row+13, col, "Disenchant :", TR_VULN_DISEN, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_DISEN)), row +13, col + 13 + equip_count() + 1);

    display_flag_aux(row+14, col, "Time       :", TR_RES_TIME, &f, 0);
    put_str(format(" %3d%%", _known_res_pct(RES_TIME)), row +14, col + 13 + equip_count() + 1);

    display_flag_aux(row+15, col, "Blindness  :", TR_RES_BLIND, &f, 0);
    display_flag_aux(row+15, col, "Blindness  :", TR_VULN_BLIND, &f, DP_VULN);
    put_str(format(" %3d%%", _known_res_pct(RES_BLIND)), row +15, col + 13 + equip_count() + 1);

    display_flag_aux(row+16, col, "Fear       :", TR_RES_FEAR, &f, 0);
    display_flag_aux(row+16, col, "Fear       :", TR_VULN_FEAR, &f, DP_VULN);

    display_flag_aux(row+17, col, "Aura Fire  :", TR_SH_FIRE, &f, 0);
    if (p_ptr->sh_fire)
        put_str(format(" %dd%d+2", 1 + (p_ptr->lev / 10), 2 + (p_ptr->lev / 10)), row +17, col + 13 + equip_count() + 1);

    display_flag_aux(row+18, col, "Aura Elec  :", TR_SH_ELEC, &f, 0);
    if (p_ptr->sh_elec)
        put_str(format(" %dd%d+2", 1 + (p_ptr->lev / 10), 2 + (p_ptr->lev / 10)), row +18, col + 13 + equip_count() + 1);

    display_flag_aux(row+19, col, "Aura Cold  :", TR_SH_COLD, &f, 0);
    if (p_ptr->sh_cold)
        put_str(format(" %dd%d+2", 1 + (p_ptr->lev / 10), 2 + (p_ptr->lev / 10)), row +19, col + 13 + equip_count() + 1);

    /*** Col2 ***/
    row = 2;
    col = 43;

    display_player_equippy(row-2, col+13, 0);

    for (i = 0; i < equip_count(); i++)
        Term_putch(col + 13 + i, row - 1, TERM_WHITE, 'a' + i);
    Term_putch(col + 13 + equip_count(), row - 1, TERM_WHITE, '@');

    display_flag_aux(row+ 0, col, "Speed      :", TR_SPEED, &f, 0);
    display_flag_aux(row+ 1, col, "Free Act   :", TR_FREE_ACT, &f, 0);
    display_flag_aux(row+ 2, col, "See Invis  :", TR_SEE_INVIS, &f, 0);
    display_flag_aux(row+ 3, col, "Warning    :", TR_WARNING, &f, 0);
    display_flag_aux(row+ 4, col, "SlowDigest :", TR_SLOW_DIGEST, &f, 0);
    display_flag_aux(row+ 5, col, "Regenerate :", TR_REGEN, &f, 0);
    display_flag_aux(row+ 6, col, "Levitation :", TR_LEVITATION, &f, 0);
    display_flag_aux(row+ 7, col, "Perm Lite  :", TR_LITE, &f, 0);
    display_flag_aux(row+ 8, col, "Reflection :", TR_REFLECT, &f, 0);
    display_flag_aux(row+ 9, col, "Hold Life  :", TR_HOLD_LIFE, &f, 0);
    display_flag_aux(row+10, col, "Sust Str   :", TR_SUST_STR, &f, 0);
    display_flag_aux(row+11, col, "Sust Int   :", TR_SUST_INT, &f, 0);
    display_flag_aux(row+12, col, "Sust Wis   :", TR_SUST_WIS, &f, 0);
    display_flag_aux(row+13, col, "Sust Dex   :", TR_SUST_DEX, &f, 0);
    display_flag_aux(row+14, col, "Sust Con   :", TR_SUST_CON, &f, 0);
    display_flag_aux(row+15, col, "Sust Chr   :", TR_SUST_CHR, &f, 0);
    display_flag_aux(row+16, col, "Dec Mana   :", TR_DEC_MANA, &f, 0);
    display_flag_aux(row+17, col, "Easy Spell :", TR_EASY_SPELL, &f, 0);
    display_flag_aux(row+18, col, "Anti Magic :", TR_NO_MAGIC, &f, 0);
    display_flag_aux(row+19, col, "Telepathy  :", TR_TELEPATHY, &f, 0);
}


/*
 * Special display, part 2
 */
static void display_player_other_flag_info(void)
{
    int row;
    int col;
    int i;

    all_player_flags f;

    /* Extract flags and store */
    player_flags(f.player_flags);
    tim_player_flags(f.tim_player_flags);
    player_immunity(f.player_imm);
    tim_player_immunity(f.tim_player_imm);
    known_obj_immunity(f.known_obj_imm);
    player_vuln_flags(f.player_vuln);

    /*** Slays ***/
    row = 2;
    col = 1;

    display_player_equippy(row-2, col+13, 0);

    for (i = 0; i < equip_count(); i++)
        Term_putch(col + 13 + i, row - 1, TERM_WHITE, 'a' + i);
    Term_putch(col + 13 + equip_count(), row - 1, TERM_WHITE, '@');

    display_flag_aux(row+ 0, col, "Slay Evil  :", TR_SLAY_EVIL, &f, DP_WP);
    display_flag_aux(row+ 0, col, "Slay Evil  :", TR_KILL_EVIL, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 1, col, "Slay Undead:", TR_SLAY_UNDEAD, &f, DP_WP);
    display_flag_aux(row+ 1, col, "Slay Undead:", TR_KILL_UNDEAD, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 2, col, "Slay Demon :", TR_SLAY_DEMON, &f, DP_WP);
    display_flag_aux(row+ 2, col, "Slay Demon :", TR_KILL_DEMON, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 3, col, "Slay Dragon:", TR_SLAY_DRAGON, &f, DP_WP);
    display_flag_aux(row+ 3, col, "Slay Dragon:", TR_KILL_DRAGON, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 4, col, "Slay Human :", TR_SLAY_HUMAN, &f, DP_WP);
    display_flag_aux(row+ 4, col, "Slay Human :", TR_KILL_HUMAN, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 5, col, "Slay Animal:", TR_SLAY_ANIMAL, &f, DP_WP);
    display_flag_aux(row+ 5, col, "Slay Animal:", TR_KILL_ANIMAL, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 6, col, "Slay Orc   :", TR_SLAY_ORC, &f, DP_WP);
    display_flag_aux(row+ 6, col, "Slay Orc   :", TR_KILL_ORC, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 7, col, "Slay Troll :", TR_SLAY_TROLL, &f, DP_WP);
    display_flag_aux(row+ 7, col, "Slay Troll :", TR_KILL_TROLL, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 8, col, "Slay Giant :", TR_SLAY_GIANT, &f, DP_WP);
    display_flag_aux(row+ 8, col, "Slay Giant :", TR_KILL_GIANT, &f, (DP_WP|DP_IMM));
    display_flag_aux(row+ 9, col, "Slay Good  :", TR_SLAY_GOOD, &f, DP_WP);
    display_flag_aux(row+10, col, "Acid Brand :", TR_BRAND_ACID, &f, DP_WP);
    display_flag_aux(row+11, col, "Elec Brand :", TR_BRAND_ELEC, &f, DP_WP);
    display_flag_aux(row+12, col, "Fire Brand :", TR_BRAND_FIRE, &f, DP_WP);
    display_flag_aux(row+13, col, "Cold Brand :", TR_BRAND_COLD, &f, DP_WP);
    display_flag_aux(row+14, col, "Pois Brand :", TR_BRAND_POIS, &f, DP_WP);
    display_flag_aux(row+15, col, "Mana Brand :", TR_FORCE_WEAPON, &f, DP_WP);
    display_flag_aux(row+16, col, "Sharpness  :", TR_VORPAL, &f, DP_WP);
    display_flag_aux(row+16, col, "Sharpness  :", TR_VORPAL2, &f, DP_WP|DP_IMM);
    display_flag_aux(row+17, col, "Quake      :", TR_IMPACT, &f, DP_WP);
    display_flag_aux(row+18, col, "Vampiric   :", TR_VAMPIRIC, &f, DP_WP);
    display_flag_aux(row+19, col, "Chaotic    :", TR_CHAOTIC, &f, DP_WP);
    display_flag_aux(row+20, col, "Add Blows  :", TR_BLOWS, &f, 0);
    display_flag_aux(row+21, col, "Blessed    :", TR_BLESSED, &f, 0);
    display_flag_aux(row+22, col, "Riding     :", TR_RIDING, &f, 0);
    display_flag_aux(row+23, col, "Tunnel     :", TR_TUNNEL, &f, 0);
    display_flag_aux(row+24, col, "Throw      :", TR_THROW, &f, 0);

    /* ESP and Curses */
    row = 2;
    col = 43;

    display_player_equippy(row-2, col+13, 0);
    for (i = 0; i < equip_count(); i++)
        Term_putch(col + 13 + i, row - 1, TERM_WHITE, 'a' + i);
    Term_putch(col + 132 + equip_count(), row - 1, TERM_WHITE, '@');
    display_flag_aux(row+ 0, col, "Telepathy  :", TR_TELEPATHY, &f, 0);
    display_flag_aux(row+ 1, col, "ESP Evil   :", TR_ESP_EVIL, &f, 0);
    display_flag_aux(row+ 2, col, "ESP Noliv. :", TR_ESP_NONLIVING, &f, 0);
    display_flag_aux(row+ 3, col, "ESP Good   :", TR_ESP_GOOD, &f, 0);
    display_flag_aux(row+ 4, col, "ESP Undead :", TR_ESP_UNDEAD, &f, 0);
    display_flag_aux(row+ 5, col, "ESP Demon  :", TR_ESP_DEMON, &f, 0);
    display_flag_aux(row+ 6, col, "ESP Dragon :", TR_ESP_DRAGON, &f, 0);
    display_flag_aux(row+ 7, col, "ESP Human  :", TR_ESP_HUMAN, &f, 0);
    display_flag_aux(row+ 8, col, "ESP Animal :", TR_ESP_ANIMAL, &f, 0);
    display_flag_aux(row+ 9, col, "ESP Orc    :", TR_ESP_ORC, &f, 0);
    display_flag_aux(row+10, col, "ESP Troll  :", TR_ESP_TROLL, &f, 0);
    display_flag_aux(row+11, col, "ESP Giant  :", TR_ESP_GIANT, &f, 0);
    display_flag_aux(row+12, col, "Magic Skill:", TR_MAGIC_MASTERY, &f, 0);
    if (p_ptr->device_power)
    {
        int tmp = device_power_aux(100, p_ptr->device_power) - 100;
        cptr desc = format("%+3d%%", tmp);
        put_str(desc, row +12, col + 13 + equip_count() + 2);
    }
    display_flag_aux(row+13, col, "Spell Pow  :", TR_SPELL_POWER, &f, 0);
    if (p_ptr->spell_power)
    {
        int  tmp = spell_power_aux(100, p_ptr->spell_power) - 100;
        cptr desc = format("%+3d%%", tmp);
        put_str(desc, row +13, col + 13 + equip_count() + 2);
    }
    display_flag_aux(row+14, col, "Spell Cap  :", TR_SPELL_CAP, &f, 0);
    if (p_ptr->spell_cap)
    {
        int tmp = spell_cap_aux(100, p_ptr->spell_cap) - 100;
        cptr desc = format("%+3d%%", tmp);
        put_str(desc, row +14, col + 13 + equip_count() + 2);
    }
    display_flag_aux(row+15, col, "Magic Res  :", TR_MAGIC_RESISTANCE, &f, 0);
    if (p_ptr->magic_resistance)
    {
        cptr desc = format("%+3d%%", p_ptr->magic_resistance);
        put_str(desc, row +15, col + 13 + equip_count() + 2);
    }

    display_flag_aux(row+16, col, "Infravision:", TR_INFRA, &f, 0);
    if (p_ptr->see_infra)
    {
        cptr desc = format("%3d'", p_ptr->see_infra * 10);
        put_str(desc, row +16, col + 13 + equip_count() + 2);
    }

    display_flag_aux(row+17, col, "Stealth    :", TR_STEALTH, &f, 0);
    display_flag_aux(row+17, col, "Stealth    :", TR_DEC_STEALTH, &f, DP_VULN);
    display_flag_aux(row+18, col, "Searching  :", TR_SEARCH, &f, 0);
    display_flag_aux(row+19, col, "Cursed     :", 0, &f, DP_CURSE);
    display_flag_aux(row+20, col, "Rnd Tele   :", TR_TELEPORT, &f, 0);
    display_flag_aux(row+21, col, "No Tele    :", TR_NO_TELE, &f, 0);
    display_flag_aux(row+22, col, "Drain Exp  :", TR_DRAIN_EXP, &f, 0);
    display_flag_aux(row+23, col, "Aggravate  :", TR_AGGRAVATE, &f, 0);
    display_flag_aux(row+24, col, "TY Curse   :", TR_TY_CURSE, &f, 0);
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
    int i;
    int stat_col, stat;
    int row, col;
    u32b flgs[TR_FLAG_SIZE];
    race_t *race_ptr = get_race_t();
    class_t *class_ptr = get_class_t();

    byte a;
    char c;

    char buf[80];


    /* Column */
    stat_col = 7;

    /* Row */
    row = 1;

    /* Print out the labels for the columns */
    c_put_str(TERM_WHITE, "  Base", row, stat_col+ 8 + equip_count() + 1);
    c_put_str(TERM_WHITE,  "  R  C  P  E", row, stat_col+ 8 + equip_count() + 1 + 6);
    c_put_str(TERM_WHITE, " Total", row, stat_col+ 8 + equip_count() + 1 + 19);

    /* Display the stats */
    for (i = 0; i < 6; i++)
    {
        int r_adj = 0, e_adj = 0, c_adj = 0;

        r_adj = race_ptr->stats[i];

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
            e_adj = p_ptr->stat_top[i] - (p_ptr->stat_max[i] - 19) / 10 - 19;

        c_adj = class_ptr->stats[i];

        e_adj -= r_adj;
        e_adj -= c_adj;
        e_adj -= ap_ptr->a_adj[i];

        if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
            c_put_str(TERM_WHITE, stat_names_reduced[i], row + i+1, stat_col+1);
        else
            c_put_str(TERM_WHITE, stat_names[i], row + i+1, stat_col+1);


        /* Internal "natural" max value.  Maxes at 18/100 */
        /* This is useful to see if you are maxed out */
        cnv_stat(p_ptr->stat_max[i], buf);
        if (p_ptr->stat_max[i] == p_ptr->stat_max_max[i])
        {
            c_put_str(TERM_WHITE, "!", row + i+1, stat_col + 4);
        }
        c_put_str(TERM_L_BLUE, buf, row + i+1, stat_col + 13 + equip_count() + 2 - strlen(buf));

        /* Race, class, and equipment modifiers */
        (void)sprintf(buf, "%3d", r_adj);
        c_put_str(TERM_L_BLUE, buf, row + i+1, stat_col + equip_count() + 13 + 2);
        (void)sprintf(buf, "%3d", c_adj);
        c_put_str(TERM_L_BLUE, buf, row + i+1, stat_col + equip_count() + 16 + 2);
        (void)sprintf(buf, "%3d", (int)ap_ptr->a_adj[i]);
        c_put_str(TERM_L_BLUE, buf, row + i+1, stat_col + equip_count() + 19 + 2);
        (void)sprintf(buf, "%3d", e_adj);
        c_put_str(TERM_L_BLUE, buf, row + i+1, stat_col + equip_count() + 22 + 2);

        /* Actual maximal modified value */
        cnv_stat(p_ptr->stat_top[i], buf);
        c_put_str(TERM_L_GREEN, buf, row + i+1, stat_col + equip_count() + 26 + 2);
    }

    /* Column */
    col = stat_col + 7;

    /* Header and Footer */
    display_player_equippy(row-1, col, 0);
    for (i = 0; i < equip_count(); i++)
        Term_putch(col + i, row, TERM_WHITE, 'a' + i);
    Term_putch(col + equip_count(), row, TERM_WHITE, '@');

    /* Process equipment */
    for (i = 0; i < equip_count(); i++)
    {
        int slot = EQUIP_BEGIN + i;
        int slot_type = equip_slot_type(slot);
        object_type *o_ptr = equip_obj(slot);

        if (o_ptr)
            object_flags_known(o_ptr, flgs);

        /* Initialize color based of sign of pval. */
        for (stat = 0; stat < 6; stat++)
        {
            int adj = 0;

            /* Default */
            a = TERM_SLATE;
            c = '.';

            if (o_ptr)
            {
                if (have_flag(flgs, TR_STR + stat))
                    adj = o_ptr->pval;

                if (have_flag(flgs, TR_DEC_STR + stat))
                    adj = -o_ptr->pval;

                /* Gargantuan hack for runes ... */
                switch (stat)
                {
                case A_STR: case A_CON:
                    if (o_ptr->rune == RUNE_MIGHT)
                        adj += 2;
                    break;
                case A_DEX: 
                    if (o_ptr->rune == RUNE_MIGHT && slot_type == EQUIP_SLOT_BODY_ARMOR)
                        adj += 2;
                    if (o_ptr->rune == RUNE_HASTE && slot_type == EQUIP_SLOT_GLOVES)
                        adj += 2;
                    break;

                case A_INT:
                    if (o_ptr->rune == RUNE_MIND)
                        adj += 2;
                    if (o_ptr->rune == RUNE_UNDERSTANDING)
                    {
                        if (slot_type == EQUIP_SLOT_HELMET)
                            adj += 2;
                        if (slot_type == EQUIP_SLOT_LITE)
                            adj += 1;
                    }
                    break;
                }

                if (adj != 0)
                {
                    /* Default */
                    c = '*';

                    /* Good */
                    if (adj > 0)
                    {
                        /* Good */
                        a = TERM_L_GREEN;

                        /* Label boost */
                        if (adj < 10) c = '0' + adj;
                    }

                    if (have_flag(flgs, stat + TR_SUST_STR))
                    {
                        /* Dark green for sustained stats */
                        a = TERM_GREEN;
                    }

                    /* Bad */
                    if (adj < 0)
                    {
                        /* Bad */
                        a = TERM_RED;

                        /* Label boost */
                        if (adj > -10) c = '0' - adj;
                    }
                }

                /* Sustain */
                else if (have_flag(flgs, stat + TR_SUST_STR))
                {
                    /* Dark green "s" */
                    a = TERM_GREEN;
                    c = 's';
                }
            }
            /* Dump proper character */
            Term_putch(col, row + stat+1, a, c);
        }

        /* Advance */
        col++;
    }

    /* Player flags */
    player_flags(flgs);

    /* Check stats */
    for (stat = 0; stat < 6; stat++)
    {
        /* Default */
        a = TERM_SLATE;
        c = '.';

        /* Mutations ... */
        if (1)
        {
            int dummy = 0;

            if (stat == A_STR)
            {
                if (mut_present(MUT_HYPER_STR)) dummy += 4;
                if (mut_present(MUT_PUNY)) dummy -= 4;
                if (p_ptr->tsuyoshi) dummy += 4;
                if (mut_present(MUT_FELL_SORCERY)) dummy--;
            }
            else if (stat == A_WIS || stat == A_INT)
            {
                if (mut_present(MUT_HYPER_INT)) dummy += 4;
                if (mut_present(MUT_MORONIC)) dummy -= 4;
            }
            else if (stat == A_DEX)
            {
                if (mut_present(MUT_STEEL_SKIN)) dummy -= 1;
                if (mut_present(MUT_LIMBER)) dummy += 3;
                if (mut_present(MUT_ARTHRITIS)) dummy -= 3;
                if (mut_present(MUT_FELL_SORCERY)) dummy--;
            }
            else if (stat == A_CON)
            {
                if (mut_present(MUT_RESILIENT)) dummy += 4;
                if (mut_present(MUT_XTRA_FAT)) dummy += 2;
                if (mut_present(MUT_ALBINO)) dummy -= 4;
                if (mut_present(MUT_FLESH_ROT)) dummy -= 2;
                if (p_ptr->tsuyoshi) dummy += 4;
                if (mut_present(MUT_FELL_SORCERY)) dummy--;
            }
            else if (stat == A_CHR)
            {
                if (mut_present(MUT_SILLY_VOICE)) dummy -= 4;
                if (mut_present(MUT_BLANK_FACE)) dummy -= 1;
                if (mut_present(MUT_FLESH_ROT)) dummy -= 1;
                if (mut_present(MUT_SCALES)) dummy -= 1;
                if (mut_present(MUT_WARTS)) dummy -= 2;
                if (mut_present(MUT_ILL_NORM)) dummy = 0;
            }
            
            if (p_ptr->pclass == CLASS_WARLOCK)
            {
                switch (stat)
                {
                case A_STR:
                    if (p_ptr->psubclass == PACT_DRAGON)
                        dummy += 5 * p_ptr->lev / 50;
                    break;
                case A_INT:
                    if (p_ptr->psubclass == PACT_DEMON)
                        dummy += 5 * p_ptr->lev / 50;
                    break;
                case A_WIS:
                    if (p_ptr->psubclass == PACT_ANGEL)
                        dummy += 5 * p_ptr->lev / 50;
                    break;
                case A_DEX:
                    if (p_ptr->psubclass == PACT_ABERRATION)
                        dummy += 5 * p_ptr->lev / 50;
                    break;
                case A_CON:
                    if (p_ptr->psubclass == PACT_UNDEAD)
                        dummy += 5 * p_ptr->lev / 50;
                    break;
                }
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
        if (have_flag(flgs, stat + TR_SUST_STR))
        {
            /* Dark green "s" */
            a = TERM_GREEN;
            c = 's';
        }


        /* Dump */
        Term_putch(col, row + stat+1, a, c);
    }

    for (i = 0; i < 6; i++)
    {
        if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
        {
            cnv_stat(p_ptr->stat_use[i], buf);
            c_put_str(TERM_YELLOW, buf, row + i+1, stat_col + equip_count() + 26 + 10);
        }
    }
}


/*
 * Display the character on the screen (various modes)
 *
 * The top one and bottom two lines are left blank.
 *
 * Mode 0 = standard display with skills
 * Mode 1 = summary of various things
 * Mode 2 = summary of various things (part 2)
 * Mode 3 = mutations
 */
static int _kills(void)
{
    int i;
    int result = 0;

    for (i = 0; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            if (r_ptr->max_num == 0) result++;
        }
        else
        {
            result += r_ptr->r_pkills;
        }
    }

    return result;
}

static int _uniques(void)
{
    int i;
    int result = 0;

    for (i = 0; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            if (r_ptr->max_num == 0) result++;
        }
    }

    return result;
}

static int _artifacts(void)
{
    int i, y, x;
    int result = 0;
    s16b *skip;

    C_MAKE(skip, max_a_idx, s16b);

    /* This is hard ... don't leak information to the player
       when an artifact is on the current level but not yet found! */
    for (y = 0; y < cur_hgt; y++)
    {
        for (x = 0; x < cur_wid; x++)
        {
            cave_type *c_ptr = &cave[y][x];
            s16b this_o_idx, next_o_idx = 0;

            for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
            {
                object_type *o_ptr = &o_list[this_o_idx];
                next_o_idx = o_ptr->next_o_idx;

                if (!object_is_fixed_artifact(o_ptr)) continue;
                if (object_is_known(o_ptr)) continue;
                skip[o_ptr->name1] = TRUE;
            }
        }
    }

    for (i = 0; i < INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        if (!o_ptr->k_idx) continue;
        if (!object_is_fixed_artifact(o_ptr)) continue;
        if (object_is_known(o_ptr)) continue;

        skip[o_ptr->name1] = TRUE;
    }

    for (i = 0; i < max_a_idx; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        if (!a_ptr->name) continue;
        if (!a_ptr->cur_num) continue;
        if (skip[i]) continue;
        result++;
    }

    C_KILL(skip, max_a_idx, s16b);
    return result;
}

void display_player(int mode)
{
    char    tmp[255], buf[255];
    race_t *race_ptr = get_race_t();
    class_t *class_ptr = get_class_t();


    mode = (mode % 4);

    /* Erase screen */
    clear_from(0);

    /* Page 1 */
    if (mode == 0)
    {
        int col, row, i;
        /* Column 1 */
        _print_field(1, 1, "Name       :", player_name, TERM_L_BLUE, 0);
        _print_field(2, 1, "Sex        :", sp_ptr->title, TERM_L_BLUE, 0);
        _print_field(3, 1, "Personality:", ap_ptr->title, TERM_L_BLUE, 0);
        _print_field(4, 1, "Race       :", race_ptr->mimic ? format("[%s]", race_ptr->name) : race_ptr->name, TERM_L_BLUE, 0);

        if (race_ptr->subname)
        {
            if (p_ptr->prace == RACE_MON_RING)
                _print_field(5, 1, "Controlling:", format("%-27.27s", race_ptr->subname), TERM_L_BLUE, 0);
            else if (p_ptr->prace == RACE_MON_MIMIC)
            {
                if (p_ptr->current_r_idx == MON_MIMIC)
                    _print_field(5, 1, "Mimicking  :", "Nothing", TERM_L_BLUE, 0);
                else
                    _print_field(5, 1, "Mimicking  :", format("%-27.27s", race_ptr->subname), TERM_L_BLUE, 0);
            }
            else
                _print_field(5, 1, "Subrace    :", format("%-27.27s", race_ptr->subname), TERM_L_BLUE, 0);
        }
        else if (p_ptr->prace == RACE_DEMIGOD)
            _print_field(5, 1, "Subrace    :", demigod_info[p_ptr->psubrace].name, TERM_L_BLUE, 0);
        else if (p_ptr->prace == RACE_DRACONIAN)
            _print_field(5, 1, "Subrace    :", race_ptr->subname, TERM_L_BLUE, 0);
        else
            _print_field(5, 1, "Subrace    :", "None", TERM_L_BLUE, 0);

        _print_field(6, 1, "Class      :", class_ptr->name, TERM_L_BLUE, 0);

        /* Assume Subclass and Magic are mutually exclusive ... */
        if (class_ptr->subname)
            _print_field(7, 1, "Subclass   :", format("%-27.27s", class_ptr->subname), TERM_L_BLUE, 0);
        else if (p_ptr->pclass == CLASS_WARLOCK)
            _print_field(7, 1, "Subclass   :", pact_info[p_ptr->psubclass].title, TERM_L_BLUE, 0);
        else if (p_ptr->pclass == CLASS_WEAPONMASTER)
            _print_field(7, 1, "Subclass   :", 
                                format("%-27.27s", weaponmaster_speciality_name(p_ptr->psubclass)), 
                                TERM_L_BLUE, 0);

        if (p_ptr->realm1)
        {
            if (p_ptr->realm2)
                sprintf(tmp, "%s, %s", realm_names[p_ptr->realm1], realm_names[p_ptr->realm2]);
            else
                strcpy(tmp, realm_names[p_ptr->realm1]);
            _print_field(7, 1, "Realm      :", tmp, TERM_L_BLUE, 0);
        }

        if (p_ptr->prace == RACE_MON_DRAGON)
        {
            dragon_realm_ptr realm = dragon_get_realm(p_ptr->dragon_realm);
            _print_field(7, 1, "Realm      :", realm->name, TERM_L_BLUE, 0);
        }

        if ((p_ptr->pclass == CLASS_CHAOS_WARRIOR) || mut_present(MUT_CHAOS_GIFT))
            _print_field(8, 1, "Patron     :", chaos_patrons[p_ptr->chaos_patron], TERM_L_BLUE, 0);


        _print_field( 9, 1, "Level      :", format("%d", p_ptr->lev), TERM_L_GREEN, 21);
        _print_field(10, 1, "Cur Exp    :", 
                                format("%d", p_ptr->exp), 
                                p_ptr->exp >= p_ptr->max_exp ? TERM_L_GREEN : TERM_YELLOW, 21);
        _print_field(11, 1, "Max Exp    :", 
                                p_ptr->prace == RACE_ANDROID ? "N/A" : format("%d", p_ptr->max_exp),
                                TERM_L_GREEN, 21);
        _print_field(12, 1, "Adv Exp    :", 
                                p_ptr->lev >= PY_MAX_LEVEL ? "*****" : format("%d", exp_requirement(p_ptr->lev)),
                                TERM_L_GREEN, 21);

        _print_field(15, 1, "Gold       :", format("%d", p_ptr->au), TERM_L_GREEN, 21);
        _print_field(16, 1, "Kills      :", format("%d", _kills()), TERM_L_GREEN, 21);
        _print_field(17, 1, "Uniques    :", format("%d", _uniques()), TERM_L_GREEN, 21);
        _print_field(18, 1, "Artifacts  :", 
                                no_artifacts ? "N/A" : format("%d+%d" , _artifacts(), stats_rand_art_counts.found),
                                TERM_L_GREEN, 21);

        {
            int day, hour, min;
            extract_day_hour_min(&day, &hour, &min);

            _print_field(20, 1, "Game Day   :", format("%d", day), TERM_L_GREEN, 21);
            _print_field(21, 1, "Game Time  :", format("%d:%02d", hour, min), TERM_L_GREEN, 21);
        }
        _print_field(22, 1, "Play Time  :", 
                                format("%.2lu:%.2lu", playtime/(60*60), (playtime/60)%60), 
                                TERM_L_GREEN, 21);

        /* Column 2 */
        _print_field(1, 43, "========== Stats ==========", "", TERM_L_BLUE, 0);
        col = 49;
        row = 2;
        for (i = 0; i < 6; i++)
        {
            if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
                c_put_str(TERM_WHITE, stat_names_reduced[i], row + i, col);
            else
                c_put_str(TERM_WHITE, stat_names[i], row + i, col);

            if (p_ptr->stat_max[i] == p_ptr->stat_max_max[i])
                c_put_str(TERM_WHITE, "! :", row + i, col + 3);
            else
                c_put_str(TERM_WHITE, "  :", row + i, col + 3);

            cnv_stat(p_ptr->stat_use[i], buf);
            c_put_str(
                p_ptr->stat_use[i] < p_ptr->stat_top[i] ? TERM_YELLOW : TERM_L_GREEN,
                buf, row + i, col + 16 - strlen(buf));
        }

        _print_field(9, 49, "HP   :", 
                            format("%d/%d", p_ptr->chp , p_ptr->mhp),
                            p_ptr->chp >= p_ptr->mhp ? TERM_L_GREEN :
                                p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10 ? TERM_YELLOW : TERM_RED,
                            16);
        
        _print_field(10, 49, "SP   :", 
                             format("%d/%d", p_ptr->csp , p_ptr->msp),
                             p_ptr->csp >= p_ptr->msp ? TERM_L_GREEN :
                                p_ptr->csp > (p_ptr->msp * mana_warn) / 10 ? TERM_YELLOW : TERM_RED,
                             16);
        
        _print_field(11, 49, "AC   :", format("%d", p_ptr->dis_ac + p_ptr->dis_to_a), TERM_L_GREEN, 16);

        /* Dump speed */
        {
            int tmp_speed = 0;
            byte attr;
            int i;

            i = p_ptr->pspeed-110;

            /* Hack -- Visually "undo" the Search Mode Slowdown */
            if (p_ptr->action == ACTION_SEARCH) i += 10;

            if (i > 0)
            {
                if (!p_ptr->riding)
                    attr = TERM_L_GREEN;
                else
                    attr = TERM_GREEN;
            }
            else if (i == 0)
            {
                if (!p_ptr->riding)
                    attr = TERM_L_BLUE;
                else
                    attr = TERM_GREEN;
            }
            else
            {
                if (!p_ptr->riding)
                    attr = TERM_L_UMBER;
                else
                    attr = TERM_RED;
            }

            if (!p_ptr->riding)
            {
                if (IS_FAST()) tmp_speed += 10;
                if (p_ptr->slow) tmp_speed -= 10;
                if (IS_LIGHT_SPEED()) tmp_speed = 99;
            }
            else
            {
                if (MON_FAST(&m_list[p_ptr->riding])) tmp_speed += 10;
                if (MON_SLOW(&m_list[p_ptr->riding])) tmp_speed -= 10;
            }

            if (tmp_speed)
            {
                sprintf(buf, "%+d%+d", i-tmp_speed, tmp_speed);
                if (tmp_speed > 0)
                    attr = TERM_YELLOW;
                else
                    attr = TERM_VIOLET;
            }
            else
            {
                sprintf(buf, "%+d", i);
            }
    
            _print_field(12, 49, "Speed:", buf, attr, 16);
        }

        _print_field(14, 43, "========== Skills =========", "", TERM_L_BLUE, 0);

        {        
            int         tmp = 0;
            int            xthn, xthb, xfos, xsrh;
            int            xdis, xdev, xsav, xstl;
            cptr        desc;
            object_type        *bow = NULL;
            int slot = equip_find_object(TV_BOW, SV_ANY);

            xthn = p_ptr->skills.thn + (p_ptr->to_h_m * BTH_PLUS_ADJ);

            if (slot)
            {
                bow = equip_obj(slot);
                if (bow)
                    tmp = p_ptr->shooter_info.to_h + bow->to_h;
            }
            xthb = p_ptr->skills.thb + (tmp * BTH_PLUS_ADJ);


            /* Basic abilities */
            xdis = p_ptr->skills.dis;
            xdev = p_ptr->skills.dev;
            xsav = p_ptr->skills.sav;
            xstl = p_ptr->skills.stl;
            xsrh = p_ptr->skills.srh;
            xfos = p_ptr->skills.fos;


            desc = likert(xthn, 12);
            _print_field(15, 43, "Melee      :", desc, likert_color, 0);

            desc = likert(xthb, 12);
            _print_field(16, 43, "Ranged     :", desc, likert_color, 0);

            desc = likert(xsav, 7);
            _print_field(17, 43, "SavingThrow:", desc, likert_color, 0);

            /* Hack -- 0 is "minimum stealth value", so print "Very Bad" */
            desc = likert((xstl > 0) ? xstl : -1, 1);
            _print_field(18, 43, "Stealth    :", desc, likert_color, 0);

            desc = likert(xfos, 6);
            _print_field(19, 43, "Perception :", desc, likert_color, 0);

            desc = likert(xsrh, 6);
            _print_field(20, 43, "Searching  :", desc, likert_color, 0);

            desc = likert(xdis, 8);
            _print_field(21, 43, "Disarming  :", desc, likert_color, 0);

            desc = likert(xdev, 6);
            _print_field(22, 43, "Device     :", desc, likert_color, 0);
        }
    }

    /* Special */
    else if (mode == 1)
    {
        display_player_flag_info();
    }
    /* Special */
    else if (mode == 2)
    {
        display_player_other_flag_info();
    }
    else if (mode == 3)
    {
        display_player_stat_info();
    }
}

static void dump_screen(FILE *fff)
{
    bool skip = FALSE;
    int w, h, x, y;
    byte a;
    char c;
    char buf[1024];

    Term_get_size(&w, &h);
    for (y = 0; y < h; y++)
    {
        for (x = 0; x < 79; x++)
        {
            (void)(Term_what(x, y, &a, &c));
            if (a < 128)
                buf[x] = c;
            else
                buf[x] = ' ';
        }
        buf[x] = '\0';
        while ((x > 0) && (buf[x-1] == ' ')) buf[--x] = '\0';
        if (buf[0])
        {
            if (skip)
            {
                fputc('\n', fff);
                skip = FALSE;
            }
            fprintf(fff, "%s\n", buf);
        }
        else
            skip = TRUE;
    }
    fprintf(fff, "\n\n");
}

/*
 *
 */
static void dump_aux_display_player(FILE *fff)
{
    int i;

    /* Display player */
    display_player(0);
    dump_screen(fff);

    if (equip_count_used())
    {
        int slot, i;
        char o_name[MAX_NLEN];
    
        fprintf(fff, "============================= Character Equipment =============================\n\n");
        for (slot = EQUIP_BEGIN, i = 0; slot < EQUIP_BEGIN + equip_count(); slot++, i++)
        {
            object_type *o_ptr = equip_obj(slot);
            if (!o_ptr) continue;

            object_desc(o_name, o_ptr, 0);
            
            fprintf(fff, "%c) %s\n",
                index_to_label(i), o_name);
        }
        fprintf(fff, "\n\n");
    }

    /* Display flags (part 1) */
    display_player(1);
    dump_screen(fff);

    /* Display flags (part 2) */
    display_player(2);
    dump_screen(fff);

    Term_clear();
    display_player_stat_info();
    dump_screen(fff);

    if (p_ptr->prace != RACE_MON_RING)
    {
        fprintf(fff, "==================================== Melee ====================================\n\n");
        for (i = 0; i < MAX_HANDS; i++)
        {
            if (p_ptr->weapon_info[i].wield_how == WIELD_NONE) continue;

            Term_clear();
            if (p_ptr->weapon_info[i].bare_hands)
                monk_display_attack_info(i, 0, 0);
            else
                display_weapon_info(i, 0, 0);

            dump_screen(fff);
        }

        for (i = 0; i < p_ptr->innate_attack_ct; i++)
        {
            Term_clear();
            display_innate_attack_info(i, 0, 0);
            dump_screen(fff);
        }
    }

    if (equip_find_object(TV_BOW, SV_ANY) && !prace_is_(RACE_MON_JELLY))
    {
        fprintf(fff, "=================================== Shooting ==================================\n\n");
        Term_clear();
        display_shooter_info(0, 0);
        dump_screen(fff);
    }

    {
        spell_info spells[MAX_SPELLS];
        int        ct = 0; 
        race_t    *race_ptr = get_race_t();
        class_t   *class_ptr = get_class_t();

        if (race_ptr->get_powers)
            ct += (race_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);

        if (class_ptr->get_powers)
            ct += (class_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);

        ct += mut_get_powers(spells + ct, MAX_SPELLS - ct);

        dump_powers_aux(fff, spells, ct);
        fprintf(fff, "\n");
    }

    {
        spell_info spells[MAX_SPELLS];
        int        ct = 0; 
        race_t    *race_ptr = get_race_t();
        /*class_t   *class_ptr = get_class_t();*/

        if (race_ptr->get_spells)
            ct += (race_ptr->get_spells)(spells + ct, MAX_SPELLS - ct);

        /* TODO: Some classes prompt the user at this point ... 
        if (class_ptr->get_spells)
            ct += (class_ptr->get_spells)(spells + ct, MAX_SPELLS - ct); */

        dump_spells_aux(fff, spells, ct);
    }

    {
        class_t *class_ptr = get_class_t();
        race_t  *race_ptr = get_race_t();
        if (class_ptr->character_dump != NULL)
            (class_ptr->character_dump)(fff);
        if (race_ptr && race_ptr->character_dump)
            race_ptr->character_dump(fff);
    }
}


/*
 *
 */
static void dump_aux_pet(FILE *fff)
{
    int i;
    bool pet = FALSE;
    bool pet_settings = FALSE;
    char pet_name[80];

    for (i = m_max - 1; i >= 1; i--)
    {
        monster_type *m_ptr = &m_list[i];

        if (!m_ptr->r_idx) continue;
        if (!is_pet(m_ptr)) continue;
        pet_settings = TRUE;
        /*if (!m_ptr->nickname && (p_ptr->riding != i)) continue;*/
        if (!pet)
        {
            fprintf(fff, "\n\n==================================== Pets =====================================\n\n");
            fprintf(fff, " Leading Pets\n");
            pet = TRUE;
        }
        monster_desc(pet_name, m_ptr, MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE | MD_NO_PET_ABBREV);
        fprintf(fff, "  %s\n", pet_name);
    }

    if (pet_settings)
    {
        fprintf(fff, "\n Options");
        fprintf(fff, "\n  Pets open doors:                    %s", (p_ptr->pet_extra_flags & PF_OPEN_DOORS) ? "ON" : "OFF");
        fprintf(fff, "\n  Pets pick up items:                 %s", (p_ptr->pet_extra_flags & PF_PICKUP_ITEMS) ? "ON" : "OFF");
        fprintf(fff, "\n  Allow teleport:                     %s", (p_ptr->pet_extra_flags & PF_TELEPORT) ? "ON" : "OFF");
        fprintf(fff, "\n  Allow cast attack spell:            %s", (p_ptr->pet_extra_flags & PF_ATTACK_SPELL) ? "ON" : "OFF");
        fprintf(fff, "\n  Allow cast summon spell:            %s", (p_ptr->pet_extra_flags & PF_SUMMON_SPELL) ? "ON" : "OFF");
        fprintf(fff, "\n  Allow involve player in area spell: %s", (p_ptr->pet_extra_flags & PF_BALL_SPELL) ? "ON" : "OFF");

        fputc('\n', fff);
    }
}


/*
 *
 */
static void dump_aux_class_special(FILE *fff)
{
    if (p_ptr->pclass == CLASS_BLUE_MAGE)
    {
        int i = 0;
        int j = 0;
        int l1 = 0;
        int l2 = 0;
        int num = 0;
        int spellnum[MAX_MONSPELLS];
        s32b f4 = 0, f5 = 0, f6 = 0;
        char p[60][80];
        int col = 0;
        bool pcol = FALSE;

        for (i=0;i<60;i++) { p[i][0] = '\0'; }

        strcat(p[col], "\n\n  [Learned Blue Magic]\n");


        for (j=1;j<6;j++)
        {
            col++;
            set_rf_masks(&f4, &f5, &f6, j);
            switch(j)
            {
                case MONSPELL_TYPE_BOLT:
                    strcat(p[col], "\n     [Bolt  Type]\n");
                    break;

                case MONSPELL_TYPE_BALL:
                    strcat(p[col], "\n     [Ball  Type]\n");
                    break;

                case MONSPELL_TYPE_BREATH:
                    strcat(p[col], "\n     [  Breath  ]\n");
                    break;

                case MONSPELL_TYPE_SUMMON:
                    strcat(p[col], "\n     [Summonning]\n");
                    break;

                case MONSPELL_TYPE_OTHER:
                    strcat(p[col], "\n     [Other Type]\n");
                    break;
            }

            for (i = 0, num = 0; i < 32; i++)
            {
                if ((0x00000001 << i) & f4) spellnum[num++] = i;
            }
            for (; i < 64; i++)
            {
                if ((0x00000001 << (i - 32)) & f5) spellnum[num++] = i;
            }
            for (; i < 96; i++)
            {
                if ((0x00000001 << (i - 64)) & f6) spellnum[num++] = i;
            }

            col++;
            pcol = FALSE;
            strcat(p[col], "       ");

            for (i = 0; i < num; i++)
            {
                if (p_ptr->magic_num2[spellnum[i]])
                {
                    pcol = TRUE;
                    /* Dump blue magic */
                    l1 = strlen(p[col]);
                    l2 = strlen(monster_powers_short[spellnum[i]]);
                    if ((l1 + l2) >= 75)
                    {
                        strcat(p[col], "\n");
                        col++;
                        strcat(p[col], "       ");
                    }
                    strcat(p[col], monster_powers_short[spellnum[i]]);
                    strcat(p[col], ", ");
                }
            }
            
            if (!pcol)
            {
                strcat(p[col], "None");
            }
            else
            {
                if (p[col][strlen(p[col])-2] == ',')
                {
                    p[col][strlen(p[col])-2] = '\0';
                }
                else
                {
                    p[col][strlen(p[col])-10] = '\0';
                }
            }
            
            strcat(p[col], "\n");
        }

        for (i=0;i<=col;i++)
        {
            fprintf(fff, "%s", p[i]);
        }
    }
}


/*
 *
 */
static void dump_aux_quest(FILE *fff)
{
    int i;
    int *quest_num;
    int dummy;


    fprintf(fff, "\n\n==================================== Quests ===================================");

    /* Allocate Memory */
    C_MAKE(quest_num, max_quests, int);

    /* Sort by compete level */
    for (i = 1; i < max_quests; i++) quest_num[i] = i;
    ang_sort_comp = ang_sort_comp_quest_num;
    ang_sort_swap = ang_sort_swap_quest_num;
    ang_sort(quest_num, &dummy, max_quests);

    /* Dump Quest Information */
    fputc('\n', fff);
    do_cmd_knowledge_quests_completed(fff, quest_num);
    fputc('\n', fff);
    do_cmd_knowledge_quests_failed(fff, quest_num);
    fputc('\n', fff);

    /* Free Memory */
    C_KILL(quest_num, max_quests, int);
}

static void dump_aux_object_counts_imp(FILE *fff, int tval, int sval)
{
    int          k_idx = lookup_kind(tval, sval);
    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->counts.found || k_ptr->counts.bought || k_ptr->counts.used || k_ptr->counts.destroyed)
    {
        fprintf(
            fff, 
            "  %-20.20s %5d %6d %5d %5d\n", 
            k_name + k_ptr->name,
            k_ptr->counts.found,
            k_ptr->counts.bought,
            k_ptr->counts.used,
            k_ptr->counts.destroyed
        );
    }
}

typedef bool (*_kind_p)(int k_idx);
bool _kind_is_third_book(int k_idx) { 
    if (k_info[k_idx].tval == TV_ARCANE_BOOK) return FALSE;
    if ( TV_LIFE_BOOK <= k_info[k_idx].tval 
      && k_info[k_idx].tval <= TV_BURGLARY_BOOK
      && k_info[k_idx].sval == 2 )
    {
        return TRUE;
    }
    return FALSE;
}
bool _kind_is_fourth_book(int k_idx) { 
    if (k_info[k_idx].tval == TV_ARCANE_BOOK) return FALSE;
    if ( TV_LIFE_BOOK <= k_info[k_idx].tval 
      && k_info[k_idx].tval <= TV_BURGLARY_BOOK
      && k_info[k_idx].sval == 3 )
    {
        return TRUE;
    }
    return FALSE;
}
bool _kind_is_potion(int k_idx) {
    if (k_info[k_idx].tval == TV_POTION) return TRUE;
    return FALSE;
}
bool _kind_is_scroll(int k_idx) {
    if (k_info[k_idx].tval == TV_SCROLL) return TRUE;
    return FALSE;
}
bool _kind_is_wand(int k_idx) {
    if (k_info[k_idx].tval == TV_WAND) return TRUE;
    return FALSE;
}
bool _kind_is_rod(int k_idx) {
    if (k_info[k_idx].tval == TV_ROD) return TRUE;
    return FALSE;
}
bool _kind_is_staff(int k_idx) {
    if (k_info[k_idx].tval == TV_STAFF) return TRUE;
    return FALSE;
}

static void dump_aux_group_counts_imp(FILE *fff, _kind_p p, cptr text)
{   
    int i;
    counts_t totals = {0};
    for (i = 0; i < max_k_idx; i++)
    {
        if (p(i))
        {
            totals.generated += k_info[i].counts.generated;
            totals.found += k_info[i].counts.found;
            totals.bought += k_info[i].counts.bought;
            totals.used += k_info[i].counts.used;
            totals.destroyed += k_info[i].counts.destroyed;
        }
    }

    if (totals.found || totals.bought || totals.used || totals.destroyed)
    {
        fprintf(
            fff, 
            "  %-20.20s %5d %6d %5d %5d\n", 
            text,
            totals.found,
            totals.bought,
            totals.used,
            totals.destroyed
        );
    }
}

static void dump_aux_ego_counts_imp(FILE *fff, int idx, cptr text)
{
    ego_item_type *e_ptr = &e_info[idx];

    if (e_ptr->counts.found || e_ptr->counts.bought || e_ptr->counts.destroyed)
    {
        fprintf(
            fff, 
            "  %-20.20s %5d %6d %5d\n", 
            text,
            e_ptr->counts.found,
            e_ptr->counts.bought,
            e_ptr->counts.destroyed
        );
    }
}

typedef bool (*_mon_p)(int r_idx);
static bool _mon_drops_good(int r_idx)
{
    if (r_info[r_idx].flags1 & RF1_DROP_GOOD)
        return TRUE;
    return FALSE;
}
static bool _mon_drops_great(int r_idx)
{
    if (r_info[r_idx].flags1 & RF1_DROP_GREAT)
        return TRUE;
    return FALSE;
}
static bool _mon_is_animal(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_ANIMAL)
        return TRUE;
    return FALSE;
}
static bool _mon_is_breeder(int r_idx)
{
    if (r_info[r_idx].flags2 & RF2_MULTIPLY)
        return TRUE;
    return FALSE;
}
static bool _mon_is_demon(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_DEMON)
        return TRUE;
    return FALSE;
}
static bool _mon_is_dragon(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_DRAGON)
        return TRUE;
    return FALSE;
}
static bool _mon_is_evil(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_EVIL)
        return TRUE;
    return FALSE;
}
static bool _mon_is_giant(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_GIANT)
        return TRUE;
    return FALSE;
}
static bool _mon_is_good(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_GOOD)
        return TRUE;
    return FALSE;
}
static bool _mon_is_hound(int r_idx)
{
    if (r_info[r_idx].d_char == 'Z')
        return TRUE;
    return FALSE;
}
static bool _mon_is_human(int r_idx)
{
    if (r_info[r_idx].flags2 & RF2_HUMAN)
        return TRUE;
    return FALSE;
}
static bool _mon_is_neutral(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_GOOD)
        return FALSE;
    if (r_info[r_idx].flags3 & RF3_EVIL)
        return FALSE;
    return TRUE;
}
static bool _mon_is_orc(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_ORC)
        return TRUE;
    return FALSE;
}
static bool _mon_is_troll(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_TROLL)
        return TRUE;
    return FALSE;
}
static bool _mon_is_undead(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_UNDEAD)
        return TRUE;
    return FALSE;
}
static bool _mon_is_unique(int r_idx)
{
    if (r_info[r_idx].flags1 & RF1_UNIQUE)
        return TRUE;
    return FALSE;
}
static void dump_aux_kill_counts_imp(FILE *fff, _mon_p p, cptr text, int total)
{
    int i;
    int kills = 0;
    for (i = 0; i < max_r_idx; i++)
    {
        if (p(i))
        {
            if (_mon_is_unique(i))
            {
                if (r_info[i].max_num == 0)
                    kills++;   /* Perhaps The Cloning Pits is messing up r_akills? */
            }
            else
                kills += r_info[i].r_akills;
        }
    }

    if (kills)
    {
        fprintf(
            fff,
            "  %-20.20s %5d %3d.%1d%%\n",
            text,
            kills,
            kills*100/total,
            (kills*1000/total)%10
        );
    }
}

static void dump_aux_object_counts(FILE *fff)
{
    int i, total_kills = _kills();
    counts_t totals = {0};

    fprintf(fff, "\n================================== Statistics =================================\n\n");

    /* Objects */
    for (i = 0; i < max_k_idx; i++)
    {
        totals.generated += k_info[i].counts.generated;
        totals.found += k_info[i].counts.found;
        totals.bought += k_info[i].counts.bought;
        totals.used += k_info[i].counts.used;
        totals.destroyed += k_info[i].counts.destroyed;
    }

    fprintf(fff, "  Objects Found    : %6d\n", totals.found);
    fprintf(fff, "  Objects Bought   : %6d\n", totals.bought);
    fprintf(fff, "  Objects Destroyed: %6d\n", totals.destroyed);

    fprintf(fff, "\n  Potions              Found Bought  Used  Dest\n");
    fprintf(fff,   "  ---------------------------------------------\n");
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_CURE_CRITICAL);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_CURING);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_SPEED);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_HEALING);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_STAR_HEALING);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_LIFE);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_RESTORE_MANA);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_RESTORE_EXP);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_INC_STR);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_INC_INT);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_INC_WIS);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_INC_DEX);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_INC_CON);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_INC_CHR);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_NEW_LIFE);
    dump_aux_object_counts_imp(fff, TV_POTION, SV_POTION_EXPERIENCE);
    dump_aux_group_counts_imp(fff, _kind_is_potion, "Totals");

    fprintf(fff, "\n  Scrolls              Found Bought  Used  Dest\n");
    fprintf(fff,   "  ---------------------------------------------\n");
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_WORD_OF_RECALL);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_IDENTIFY);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_STAR_IDENTIFY);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_REMOVE_CURSE);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_STAR_REMOVE_CURSE);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_TELEPORT);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_TELEPORT_LEVEL); 
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_STAR_DESTRUCTION);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_GENOCIDE);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_MASS_GENOCIDE);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_FOREST_CREATION);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_ACQUIREMENT);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_STAR_ACQUIREMENT);
    dump_aux_object_counts_imp(fff, TV_SCROLL, SV_SCROLL_ARTIFACT);
    dump_aux_group_counts_imp(fff, _kind_is_scroll, "Totals");

    fprintf(fff, "\n  Wands                Found Bought  Used  Dest\n");
    fprintf(fff,   "  ---------------------------------------------\n");
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_STONE_TO_MUD);
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_TELEPORT_AWAY);
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_DRAGON_COLD);
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_DRAGON_FIRE);
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_DRAGON_BREATH);
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_STRIKING);
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_DISINTEGRATE);
    dump_aux_object_counts_imp(fff, TV_WAND, SV_WAND_ROCKETS);
    dump_aux_group_counts_imp(fff, _kind_is_wand, "Totals");

    fprintf(fff, "\n  Staves               Found Bought  Used  Dest\n");
    fprintf(fff,   "  ---------------------------------------------\n");
    dump_aux_object_counts_imp(fff, TV_STAFF, SV_STAFF_IDENTIFY);
    dump_aux_object_counts_imp(fff, TV_STAFF, SV_STAFF_MAPPING);
    dump_aux_object_counts_imp(fff, TV_STAFF, SV_STAFF_SPEED);
    dump_aux_object_counts_imp(fff, TV_STAFF, SV_STAFF_HEALING);
    dump_aux_object_counts_imp(fff, TV_STAFF, SV_STAFF_DESTRUCTION);
    dump_aux_object_counts_imp(fff, TV_STAFF, SV_STAFF_GENOCIDE);
    dump_aux_object_counts_imp(fff, TV_STAFF, SV_STAFF_MSTORM);
    dump_aux_group_counts_imp(fff, _kind_is_staff, "Totals");

    fprintf(fff, "\n  Rods                 Found Bought  Used  Dest\n");
    fprintf(fff,   "  ---------------------------------------------\n");
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_DETECT_TRAP);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_DETECT_DOOR);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_DETECT_MONSTERS);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_ILLUMINATION);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_RECALL);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_DETECTION);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_MAPPING);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_IDENTIFY);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_TELEPORT_AWAY);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_HEALING);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_RESTORATION);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_SPEED);
    dump_aux_object_counts_imp(fff, TV_ROD, SV_ROD_MANA_BALL);
    dump_aux_group_counts_imp(fff, _kind_is_rod, "Totals");

    fprintf(fff, "\n  Spellbooks           Found Bought  Used  Dest\n");
    fprintf(fff,   "  ---------------------------------------------\n");
    dump_aux_group_counts_imp(fff, _kind_is_third_book, "Third Spellbooks");
    dump_aux_group_counts_imp(fff, _kind_is_fourth_book, "Fourth Spellbooks");
    dump_aux_group_counts_imp(fff, kind_is_book, "Totals");

    /* Egos */
    WIPE(&totals, counts_t);
    for (i = 0; i < max_e_idx; i++)
    {
        totals.generated += e_info[i].counts.generated;
        totals.found += e_info[i].counts.found;
        totals.bought += e_info[i].counts.bought;
        totals.destroyed += e_info[i].counts.destroyed;
    }

    fprintf(fff, "\n\n  Egos Found    : %6d\n", totals.found);
    fprintf(fff,     "  Egos Bought   : %6d\n", totals.bought);
    fprintf(fff,     "  Egos Destroyed: %6d\n", totals.destroyed);

    fprintf(fff, "\n  Egos                 Found Bought  Dest\n");
    fprintf(fff,   "  ---------------------------------------\n");
    dump_aux_ego_counts_imp(fff, EGO_RING_SPEED, "Ring of Speed");
    dump_aux_ego_counts_imp(fff, EGO_RING_DEFENDER, "Ring (Defender)");
    dump_aux_ego_counts_imp(fff, EGO_AMULET_DEFENDER, "Amulet (Defender)");
    dump_aux_ego_counts_imp(fff, EGO_BOOTS_ELVENKIND, "Boots of Elvenkind");
    dump_aux_ego_counts_imp(fff, EGO_BOOTS_SPEED, "Boots of Speed");
    dump_aux_ego_counts_imp(fff, EGO_BOOTS_FEANOR, "Boots of Feanor");

    /* Monsters */
    fprintf(fff, "\n  Monsters             Kills   Pct\n");
    fprintf(fff,   "  --------------------------------\n");
    dump_aux_kill_counts_imp(fff, _mon_is_animal, "Animals", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_breeder, "Breeders", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_demon, "Demons", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_dragon, "Dragons", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_giant, "Giants", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_hound, "Hounds", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_human, "Humans", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_orc, "Orcs", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_troll, "Trolls", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_undead, "Undead", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_unique, "Uniques", total_kills);
    fprintf(fff, "\n");
    dump_aux_kill_counts_imp(fff, _mon_is_evil, "Evil Monsters", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_good, "Good Monsters", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_is_neutral, "Neutral Monsters", total_kills);
    fprintf(fff, "\n");
    dump_aux_kill_counts_imp(fff, _mon_drops_good, "Good Droppers", total_kills);
    dump_aux_kill_counts_imp(fff, _mon_drops_great, "Great Droppers", total_kills);
    fprintf(fff, "  %-20.20s %5d\n", "Totals", total_kills);

    fprintf(fff, "\n");
}

static void dump_aux_last_message(FILE *fff)
{
    /*if (p_ptr->is_dead) */
    {
        if (!p_ptr->total_winner)
        {
            int i;

            fprintf(fff, "\n================================ Last Messages ================================\n\n");
            for (i = MIN(message_num(), 30); i >= 0; i--)
            {
                fprintf(fff,"> %s\n",message_str((s16b)i));
            }
            fputc('\n', fff);
        }
    }
}


static void dump_aux_recall(FILE *fff)
{
    int y;

    fprintf(fff, "\n\n=================================== Dungeons ==================================\n\n");
    for (y = 1; y < max_d_idx; y++)
    {
        bool seiha = FALSE;

        if (!d_info[y].maxdepth) continue;
        if (!max_dlv[y]) continue;
        if (d_info[y].final_guardian)
        {
            if (!r_info[d_info[y].final_guardian].max_num) seiha = TRUE;
        }
        else if (max_dlv[y] == d_info[y].maxdepth) seiha = TRUE;

        fprintf(fff, "%c%-16s: level %3d\n", seiha ? '!' : ' ', d_name+d_info[y].name, max_dlv[y]);
    }

    {
        char statmsg[255];
        int i;
        *statmsg = '\0';
        if (p_ptr->is_dead)
        {
            if (p_ptr->total_winner)
            {
                sprintf(statmsg, "You %s after winning.", streq(p_ptr->died_from, "Seppuku") ? "did Seppuku" : "retired from the adventure");
            }
            else if (!dun_level)
            {
                sprintf(statmsg, "You were killed by %s in %s.", p_ptr->died_from, map_name());
            }
            else if (p_ptr->inside_quest && is_fixed_quest_idx(p_ptr->inside_quest))
            {
                /* Get the quest text */
                /* Bewere that INIT_ASSIGN resets the cur_num. */
                init_flags = INIT_ASSIGN;

                process_dungeon_file("q_info.txt", 0, 0, 0, 0);
                sprintf(statmsg, "You were killed by %s in the quest '%s'.", p_ptr->died_from, quest[p_ptr->inside_quest].name);
            }
            else
            {
                sprintf(statmsg, "You were killed by %s on level %d of %s.", p_ptr->died_from, dun_level, map_name());
            }
        }
        else if (character_dungeon)
        {
            if (!dun_level)
            {
                sprintf(statmsg, "Now, you are in %s.", map_name());
            }
            else if (p_ptr->inside_quest && is_fixed_quest_idx(p_ptr->inside_quest))
            {
                /* Clear the text */
                /* Must be done before doing INIT_SHOW_TEXT */
                for (i = 0; i < 10; i++)
                {
                    quest_text[i][0] = '\0';
                }
                quest_text_line = 0;

                /* Get the quest text */
                init_flags = INIT_SHOW_TEXT;

                process_dungeon_file("q_info.txt", 0, 0, 0, 0);
                sprintf(statmsg, "Now, you are in the quest '%s'.", quest[p_ptr->inside_quest].name);
            }
            else
            {
                sprintf(statmsg, "Now, you are exploring level %d of %s.", dun_level, map_name());
            }
        }

        if (*statmsg)
            fprintf(fff, "\n %s\n", statmsg);

        if (p_ptr->last_message)
        {
            if (p_ptr->is_dead)
                fprintf(fff, "\n Last Message: %s\n\n", p_ptr->last_message);
            else if (p_ptr->total_winner)
                fprintf(fff, "\n *Winning* Message: %s\n\n", p_ptr->last_message);
        }
    }
}

/*
 *
 */
static void dump_aux_options(FILE *fff)
{
    fprintf(fff, "\n=================================== Options ===================================\n");


    if (preserve_mode)
        fprintf(fff, "\n Preserve Mode:      ON");
    else
        fprintf(fff, "\n Preserve Mode:      OFF");


    if (ironman_small_levels)
        fprintf(fff, "\n Small Levels:       ALWAYS");

    else if (always_small_levels)
        fprintf(fff, "\n Small Levels:       ON");

    else if (small_levels)
        fprintf(fff, "\n Small Levels:       ENABLED");

    else
        fprintf(fff, "\n Small Levels:       OFF");


    if (vanilla_town)
        fprintf(fff, "\n Vanilla Town:       ON");

    else if (lite_town)
        fprintf(fff, "\n Lite Town:          ON");


    if (ironman_shops)
        fprintf(fff, "\n No Shops:           ON");


    if (ironman_downward)
        fprintf(fff, "\n Diving Only:        ON");


    if (ironman_rooms)
        fprintf(fff, "\n Unusual Rooms:      ON");


    if (ironman_nightmare)
        fprintf(fff, "\n Nightmare Mode:     ON");


    if (ironman_empty_levels)
        fprintf(fff, "\n Arena Levels:       ALWAYS");

    else if (empty_levels)
        fprintf(fff, "\n Arena Levels:       ENABLED");

    else
        fprintf(fff, "\n Arena Levels:       OFF");


    if (ironman_quests)
        fprintf(fff, "\n Ironman Quests:     ENABLED");

    if (no_artifacts)
        fprintf(fff, "\n No Artifacts:       ENABLED");
    else if (random_artifacts)
        fprintf(fff, "\n Random Artifacts:   ENABLED");

    if (no_egos)
        fprintf(fff, "\n No Egos:            ENABLED");

    if (no_selling)
        fprintf(fff, "\n No Selling:         ENABLED");

    fputc('\n', fff);

    if (p_ptr->noscore)
        fprintf(fff, "\n You have done something illegal.\n");

    fputc('\n', fff);
}


/*
 *
 */
static void dump_aux_arena(FILE *fff)
{
    if (lite_town || vanilla_town) return;

    if (p_ptr->arena_number < 0)
    {
        if (p_ptr->arena_number <= ARENA_DEFEATED_OLD_VER)
        {
            fprintf(fff, "\n Arena: Defeated\n");
        }
        else
        {
            fprintf(fff, "\n Arena: Defeated by %s in the %d%s fight\n",
                r_name + r_info[arena_info[-1 - p_ptr->arena_number].r_idx].name,
                -p_ptr->arena_number, get_ordinal_number_suffix(-p_ptr->arena_number));
        }
    }
    else if (p_ptr->arena_number > MAX_ARENA_MONS + 2)
    {
        fprintf(fff, "\n Arena: True Champion\n");
    }
    else if (p_ptr->arena_number > MAX_ARENA_MONS - 1)
    {
        fprintf(fff, "\n Arena: Champion\n");
    }
    else
    {
        fprintf(fff, "\n Arena: %2d Victor%s\n", (p_ptr->arena_number > MAX_ARENA_MONS ? MAX_ARENA_MONS : p_ptr->arena_number), (p_ptr->arena_number > 1) ? "ies" : "y");
    }

    fprintf(fff, "\n");
}


/*
 *
 */
static void dump_aux_monsters(FILE *fff)
{
    /* Monsters slain */

    int k;
    int uniq_total = 0;
    int norm_total = 0;
    s16b *who;

    /* Sort by monster level */
    u16b why = 2;

    fprintf(fff, "\n============================== Defeated Monsters ==============================\n\n");

    /* Allocate the "who" array */
    C_MAKE(who, max_r_idx, s16b);

    /* Count monster kills */
    for (k = 1; k < max_r_idx; k++)
    {
        monster_race *r_ptr = &r_info[k];

        /* Ignore unused index */
         if (!r_ptr->name) continue;

        /* Unique monsters */
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            bool dead = (r_ptr->max_num == 0);
            if (dead)
            {
                norm_total++;

                /* Add a unique monster to the list */
                who[uniq_total++] = k;
            }
        }

        /* Normal monsters */
        else
        {
            if (r_ptr->r_pkills > 0)
            {
                norm_total += r_ptr->r_pkills;
            }
        }
    }


    /* No monsters is defeated */
    if (norm_total < 1)
    {
        fprintf(fff,"You have defeated no enemies yet.\n");
    }

    /* Defeated more than one normal monsters */
    else if (uniq_total == 0)
    {
        fprintf(fff,"You have defeated %d %s.\n", norm_total, norm_total == 1 ? "enemy" : "enemies");
    }

    /* Defeated more than one unique monsters */
    else /* if (uniq_total > 0) */
    {
        fprintf(fff, "You have defeated %d %s including %d unique monster%s in total.\n", norm_total, norm_total == 1 ? "enemy" : "enemies", uniq_total, (uniq_total == 1 ? "" : "s"));


        /* Select the sort method */
        ang_sort_comp = ang_sort_comp_hook;
        ang_sort_swap = ang_sort_swap_hook;

        /* Sort the array by dungeon depth of monsters */
        ang_sort(who, &why, uniq_total);

        fprintf(fff, "\n< Unique monsters top %d >\n", MIN(uniq_total, 20));

        /* Print top N */
        for (k = uniq_total - 1; k >= 0 && k >= uniq_total - 20; k--)
        {
            monster_race *r_ptr = &r_info[who[k]];

            fprintf(fff, "  %-40s (level %3d)\n", (r_name + r_ptr->name), r_ptr->level); 
        }

    }

    /* Free the "who" array */
    C_KILL(who, max_r_idx, s16b);
}


/*
 *
 */
static void dump_aux_race_history(FILE *fff)
{
    if (p_ptr->old_race1 || p_ptr->old_race2)
    {
        int i;

        fprintf(fff, "\n\n You were born as %s.", get_race_t_aux(p_ptr->start_race, 0)->name);
        for (i = 0; i < MAX_RACES; i++)
        {
            if (p_ptr->start_race == i) continue;
            if (i < 32)
            {
                if (!(p_ptr->old_race1 & 1L << i)) continue;
            }
            else
            {
                if (!(p_ptr->old_race2 & 1L << (i-32))) continue;
            }
            fprintf(fff, "\n You were a %s before.", get_race_t_aux(i, 0)->name);
        }

        fputc('\n', fff);
    }
}


/*
 *
 */
static void dump_aux_virtues(FILE *fff)
{
    fprintf(fff, "\n\n=================================== Virtues ===================================\n\n");
    fprintf(fff, "Your alighnment : %s\n", your_alignment());
    fprintf(fff, "\n");
    virtue_dump(fff);
}


/*
 *
 */
static void dump_aux_mutations(FILE *fff)
{
    if (mut_count(NULL))
    {
        fprintf(fff, "\n\n================================== Mutations ==================================\n\n");
        mut_dump_file(fff);
    }
}


/*
 *
 */
static void dump_aux_equipment_inventory(FILE *fff)
{
    int i;
    char o_name[MAX_NLEN];

    /* Dump the inventory */
    fprintf(fff, "============================= Character Inventory =============================\n\n");

    for (i = 0; i < INVEN_PACK; i++)
    {
        /* Don't dump the empty slots */
        if (!inventory[i].k_idx) break;

        /* Dump the inventory slots */
        object_desc(o_name, &inventory[i], 0);
        fprintf(fff, "%c) %s\n", index_to_label(i), o_name);
    }

    /* Add an empty line */
    fprintf(fff, "\n\n");
}


/*
 *
 */
static void dump_aux_home_museum(FILE *fff)
{
    char o_name[MAX_NLEN];
    store_type  *st_ptr;

    /* Do we need it?? */
    /* process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x); */

    /* Print the home */
    st_ptr = &town[1].store[STORE_HOME];

    /* Home -- if anything there */
    if (st_ptr->stock_num)
    {
        int i;
        int x = 1;

        fprintf(fff, "================================ Home Inventory ===============================\n");

        /* Dump all available items */
        for (i = 0; i < st_ptr->stock_num; i++)
        {
            if ((i % 12) == 0)
                fprintf(fff, "\n ( page %d )\n", x++);
            object_desc(o_name, &st_ptr->stock[i], 0);
            fprintf(fff, "%c) %s\n", I2A(i%12), o_name);
        }

        /* Add an empty line */
        fprintf(fff, "\n\n");
    }


    /* Print the home */
    st_ptr = &town[1].store[STORE_MUSEUM];

    /* Home -- if anything there */
    if (st_ptr->stock_num)
    {
        int i;
        int x = 1;

        fprintf(fff, "==================================== Museum ===================================\n");
        /* Dump all available items */
        for (i = 0; i < st_ptr->stock_num; i++)
        {
        if ((i % 12) == 0) fprintf(fff, "\n ( page %d )\n", x++);
            object_desc(o_name, &st_ptr->stock[i], 0);
            fprintf(fff, "%c) %s\n", I2A(i%12), o_name);
        }

        /* Add an empty line */
        fprintf(fff, "\n\n");
    }
}


/*
 * Output the character dump to a file
 */
errr make_character_dump(FILE *fff)
{
    fprintf(fff, "  [PosChengband %d.%d.%d Character Dump]\n",
        VER_MAJOR, VER_MINOR, VER_PATCH);

    update_playtime();

    if (p_ptr->total_winner)
        fprintf(fff, "              ***WINNER***\n");
    else if (p_ptr->is_dead)
        fprintf(fff, "              ***LOSER***\n");
    else
        fprintf(fff, "\n");

    dump_aux_display_player(fff);
    dump_aux_recall(fff);
    dump_aux_quest(fff);
    dump_aux_arena(fff);
    dump_aux_monsters(fff);
    if (enable_virtues)
        dump_aux_virtues(fff);
    dump_aux_race_history(fff);
    dump_aux_class_special(fff);
    dump_aux_mutations(fff);
    dump_aux_pet(fff);

    fputs("\n\n", fff);
    dump_aux_equipment_inventory(fff);
    dump_aux_home_museum(fff);

    dump_aux_object_counts(fff);
    dump_aux_last_message(fff);
    dump_aux_options(fff);

    fprintf(fff, "  [Check Sum: \"%s\"]\n\n", get_check_sum());
    return 0;
}

/*
 * Hack -- Dump a character description file
 *
 * XXX XXX XXX Allow the "full" flag to dump additional info,
 * and trigger its usage from various places in the code.
 */
errr file_character(cptr name)
{
    int        fd = -1;
    FILE        *fff = NULL;
    char        buf[1024];

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
        (void)sprintf(out_val, "Replace existing file %s? ", buf);


        /* Ask */
        if (get_check_strict(out_val, CHECK_NO_HISTORY)) fd = -1;
    }

    /* Open the non-existing file */
    if (fd < 0) fff = my_fopen(buf, "w");

    /* Invalid file */
    if (!fff)
    {
        /* Message */
        prt("Character dump failed!", 0, 0);

        (void)inkey();

        /* Error */
        return (-1);
    }

    (void)make_character_dump(fff);

    /* Close it */
    my_fclose(fff);


    /* Message */
    msg_print("Character dump successful.");

    msg_print(NULL);

    /* Success */
    return (0);
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
bool show_file(bool show_version, cptr name, cptr what, int line, int mode)
{
    int i, n, skey;

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
    char hook[68][32];

    bool reverse = (line < 0);

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
    for (i = 0; i < 68; i++)
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
        path_build(path, sizeof(path), ANGBAND_DIR_HELP, name);

        /* Open the file */
        fff = my_fopen(path, "r");
    }

    /* Look in "info" */
    if (!fff)
    {
        /* Caption */
        sprintf(caption, "Info file '%s'", name);


        /* Build the filename */
        path_build(path, sizeof(path), ANGBAND_DIR_INFO, name);

        /* Open the file */
        fff = my_fopen(path, "r");
    }

    /* Look in "info" */
    if (!fff)
    {
        /* Build the filename */
        path_build(path, sizeof(path), ANGBAND_DIR, name);

        for (i = 0; path[i]; i++)
            if ('\\' == path[i])
                path[i] = PATH_SEP[0];

        /* Caption */
        sprintf(caption, "Info file '%s'", name);

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
        char *str = buf;

        /* Read a line or stop */
        if (my_fgets(fff, buf, sizeof(buf))) break;

        /* XXX Parse "menu" items */
        if (prefix(str, "***** "))
        {
            /* Notice "menu" requests */
            if ((str[6] == '[') && isalpha(str[7]))
            {
                /* Extract the menu item */
                int k = str[7] - 'A';

                /* This is a menu file */
                menu = TRUE;

                if ((str[8] == ']') && (str[9] == ' '))
                {
                    /* Extract the menu item */
                    strncpy(hook[k], str + 10, 31);

                    /* Make sure it's null-terminated */
                    hook[k][31] = '\0';
                }
            }
            /* Notice "tag" requests */
            else if (str[6] == '<')
            {
                size_t len = strlen(str);

                if (str[len - 1] == '>')
                {
                    str[len - 1] = '\0';
                    if (tag && streq(str + 7, tag)) line = next;
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

    /* start from bottom when reverse mode */
    if (line == -1) line = ((size-1)/rows)*rows;

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

        /* Dump the next 20, or rows, lines of the file */
        for (i = 0; i < rows; )
        {
            cptr str = buf;

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
                strcpy(lc_buf, str);
                str_tolower(lc_buf);

                if (!my_strstr(lc_buf, find)) continue;
            }

            /* Hack -- stop searching */
            find = NULL;

            /* Dump the line */
            show_file_aux_line(str, i + 2, shower);

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
        if (show_version)
        {
            prt(format(
                "[PosChengband %d.%d.%d, %s, Line %d/%d]",
               VER_MAJOR, VER_MINOR, VER_PATCH,
               caption, line, size), 0, 0);
        }
        else
        {
            prt(format(
                "[%s, Line %d/%d]",
                caption, line, size), 0, 0);
        }

        /* Prompt -- small files */
        if (size <= rows)
        {
            /* Wait for it */
            prt("[Press ESC to exit.]", hgt - 1, 0);

        }

        /* Prompt -- large files */
        else
        {
            prt("[Press Return, Space, -, =, /, |, or ESC to exit.]", hgt - 1, 0);
        }

        /* Get a special key code */
        skey = inkey_special(TRUE);

        switch (skey)
        {
        /* Show the help for the help */
        case '?':
            /* Hack - prevent silly recursion */
            if (strcmp(name, "helpinfo.txt") != 0)
                show_file(TRUE, "helpinfo.txt", NULL, 0, mode);
            break;

        /* Hack -- try showing */
        case '=':
            /* Get "shower" */
            prt("Show: ", hgt - 1, 0);

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
            prt("Find: ", hgt - 1, 0);

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
                prt("Goto Line: ", hgt - 1, 0);

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
                prt("Goto File: ", hgt - 1, 0);
                strcpy(tmp, "help.hlp");

                if (askfor(tmp, 80))
                {
                    if (!show_file(TRUE, tmp, NULL, 0, mode)) skey = 'q';
                }
            }
            break;

        /* Allow backing up */
        case '-':
            line = line + (reverse ? rows : -rows);
            if (line < 0) line = 0;
            break;

        /* One page up */
        case SKEY_PGUP:
            line = line - rows;
            if (line < 0) line = 0;
            break;

        /* Advance a single line */
        case '\n':
        case '\r':
            line = line + (reverse ? -1 : 1);
            if (line < 0) line = 0;
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
            line = line + (reverse ? -rows : rows);
            if (line < 0) line = 0;
            break;

        /* One page down */
        case SKEY_PGDOWN:
            line = line + rows;
            break;
        }

        /* Recurse on numbers */
        if (menu)
        {
            int key = -1;

            if (!(skey & SKEY_MASK) && isalpha(skey))
                key = skey - 'A';

            if ((key > -1) && hook[key][0])
            {
                /* Recurse on that file */
                if (!show_file(TRUE, hook[key], NULL, 0, mode))
                    skey = 'q';
            }
        }

        /* Hack, dump to file */
        if (skey == '|')
        {
            FILE *ffp;
            char buff[1024];
            char xtmp[82];

            strcpy (xtmp, "");

            if (!get_string("File name: ", xtmp, 80)) continue;

            /* Close it */
            my_fclose(fff);

            /* Build the filename */
            path_build(buff, sizeof(buff), ANGBAND_DIR_USER, xtmp);

            /* Hack -- Re-Open the file */
            fff = my_fopen(path, "r");

            ffp = my_fopen(buff, "w");

            /* Oops */
            if (!(fff && ffp))
            {
                msg_print("Failed to open file.");
                skey = ESCAPE;
                break;
            }

            sprintf(xtmp, "%s: %s", player_name, what ? what : caption);
            my_fputs(ffp, xtmp, 80);
            my_fputs(ffp, "\n", 80);

            while (!my_fgets(fff, buff, sizeof(buff)))
                my_fputs(ffp, buff, 80);

            /* Close it */
            my_fclose(fff);
            my_fclose(ffp);

            /* Hack -- Re-Open the file */
            fff = my_fopen(path, "r");
        }

        /* Return to last screen */
        if (IS_ESCAPE(skey) || skey == '<') break;

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
    (void)show_file(TRUE, "help.hlp", NULL, 0, 0);


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
        quit_fmt("The name '%s' is too long!", player_name);

    }
#endif

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
            s = t+1;
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
 *
 * What a horrible name for a global function.  XXX XXX XXX
 */
void get_name(void)
{
    char tmp[64];

    /* Save the player name */
    strcpy(tmp, player_name);

    /* Prompt for a new name */
    if (get_string("Enter a name for your character: ", tmp, 15))
    {
        /* Use the name */
        strcpy(player_name, tmp);
    }

    if (0 == strlen(player_name))
    {
        /* Use default name */
        strcpy(player_name, "PLAYER");
    }

    /* Re-Draw the name (in light blue) */
    Term_erase(34, 1, 255);
    c_put_str(TERM_L_BLUE, player_name, 1, 14);

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
        if (!get_check_strict("Do you want to retire? ", CHECK_NO_HISTORY)) return;

    }

    /* Verify Suicide */
    else
    {
        /* Verify */
        if (!get_check("Do you really want to commit suicide? ")) return;
    }


    if (!p_ptr->noscore)
    {
        /* Special Verification for suicide */
        prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);

        flush();
        i = inkey();
        prt("", 0, 0);
        if (i != '@') return;
    }

    /* Initialize "last message" buffer */
    if (p_ptr->last_message) string_free(p_ptr->last_message);
    p_ptr->last_message = NULL;

    /* Hack -- Note *winning* message */
    if (p_ptr->total_winner && last_words)
    {
        char buf[1024] = "";

        do
        {
            while (!get_string("*Winning* message: ", buf, sizeof buf)) ;
        }
        while (!get_check_strict("Are you sure? ", CHECK_NO_HISTORY));

        if (buf[0])
        {
            p_ptr->last_message = string_make(buf);
            msg_print(p_ptr->last_message);
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

    /* Note that the player is not dead */
    (void)strcpy(p_ptr->died_from, "(alive and well)");

    /* HACK -- don't get sanity blast on updating view */
    hack_mind = FALSE;

    /* Update stuff */
    update_stuff();

    /* Initialize monster process */
    mproc_init();

    /* HACK -- reset the hackish flag */
    hack_mind = TRUE;
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
 * Hack -- Calculates the total number of points earned        -JWT-
 */
long total_points(void)
{
    int i, mult = 100;
    s16b max_dl = 0;
    u32b point, point_h, point_l;
    int arena_win = MIN(p_ptr->arena_number, MAX_ARENA_MONS);

    if (!preserve_mode) mult += 10;
    if (!autoroller) mult += 10;
    if (!smart_learn) mult -= 20;
    if (smart_cheat) mult += 30;
    if (ironman_shops) mult += 50;
    if (ironman_small_levels) mult += 10;
    if (ironman_empty_levels) mult += 20;
    if (!powerup_home) mult += 50;
    if (ironman_rooms) mult += 100;
    if (ironman_nightmare) mult += 100;

    if (mult < 5) mult = 5;

    for (i = 0; i < max_d_idx; i++)
        if(max_dlv[i] > max_dl)
            max_dl = max_dlv[i];

    point_l = (p_ptr->max_max_exp + (100 * max_dl));
    point_h = point_l / 0x10000L;
    point_l = point_l % 0x10000L;
    point_h *= mult;
    point_l *= mult;
    point_h += point_l / 0x10000L;
    point_l %= 0x10000L;

    point_l += ((point_h % 100) << 16);
    point_h /= 100;
    point_l /= 100;

    point = (point_h << 16) + (point_l);
    if (p_ptr->arena_number >= 0)
        point += (arena_win * arena_win * (arena_win > 29 ? 1000 : 100));

    if (ironman_downward) point *= 2;
    if (p_ptr->pclass == CLASS_BERSERKER)
    {
        if ( p_ptr->prace == RACE_SPECTRE )
            point = point / 5;
    }

    if ((p_ptr->personality == PERS_MUNCHKIN) && point)
    {
        point = 1;
        if (p_ptr->total_winner) point = 2;
    }
    if (easy_band) point = (0 - point);

    return point;
}


#define GRAVE_LINE_WIDTH 31

/*
 * Centers a string within a GRAVE_LINE_WIDTH character string        -JWT-
 */
static void center_string(char *buf, cptr str)
{
    int i, j;

    /* Total length */
    i = strlen(str);

    /* Necessary border */
    j = GRAVE_LINE_WIDTH / 2 - i / 2;

    /* Mega-Hack */
    (void)sprintf(buf, "%*s%s%*s", j, "", str, GRAVE_LINE_WIDTH - i - j, "");
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
    if (!(p_ptr->noscore & 0x00FF))
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
        cptr   p;
        char   tmp[160];
        char   buf[1024];
        char   dummy[80];
        char   *t;
        FILE   *fp;
        time_t ct = time(0);

        /* Clear screen */
        Term_clear();

        /* Build the filename */
        path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "dead.txt");

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
        if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
        {
            p = "Magnificent";
        }

        /* Normal */
        else
        {
            p =  player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5];
        }

        center_string(buf, player_name);
        put_str(buf, 6, 11);

        center_string(buf, "the");
        put_str(buf, 7, 11);

        center_string(buf, p);
        put_str(buf, 8, 11);

        center_string(buf, get_class_t()->name);
        put_str(buf, 10, 11);

        (void)sprintf(tmp, "Level: %d", (int)p_ptr->lev);
        center_string(buf, tmp);
        put_str(buf, 11, 11);

        (void)sprintf(tmp, "Exp: %d", p_ptr->exp);
        center_string(buf, tmp);
        put_str(buf, 12, 11);

        (void)sprintf(tmp, "AU: %d", p_ptr->au);
        center_string(buf, tmp);
        put_str(buf, 13, 11);

        (void)sprintf(tmp, "Killed on Level %d", dun_level);
        center_string(buf, tmp);
        put_str(buf, 14, 11);

        roff_to_buf(format("by %s.", p_ptr->died_from), GRAVE_LINE_WIDTH + 1, tmp, sizeof tmp);
        center_string(buf, tmp);
        put_str(buf, 15, 11);
        t = tmp + strlen(tmp) + 1;
        if (*t)
        {
            strcpy(dummy, t); /* 2nd line */
            if (*(t + strlen(t) + 1)) /* Does 3rd line exist? */
            {
                int dummy_len = strlen(dummy);
                strcpy(dummy + MIN(dummy_len, GRAVE_LINE_WIDTH - 3), "...");
            }
            center_string(buf, dummy);
            put_str(buf, 16, 11);
        }

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
    int             i, j, k, l;
    object_type        *o_ptr;
    store_type        *st_ptr;

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
        if (!askfor(out_val, 60)) return;

        /* Return means "show on screen" */
        if (!out_val[0]) break;

        /* Save screen */
        screen_save();

        /* Dump a character file */
        (void)file_character(out_val);

        /* Load screen */
        screen_load();
    }

    update_playtime();

    /* Display player */
    display_player(0);

    /* Prompt for inventory */
    prt("Hit any key to see more information (ESC to abort): ", 23, 0);


    /* Allow abort at this point */
    if (inkey() == ESCAPE) return;


    /* Show equipment and inventory */

    /* Equipment -- if any */
    if (equip_count_used())
    {
        Term_clear();
        item_tester_full = TRUE;
        (void)show_equip(0, 0);
        prt("You are using: -more-", 0, 0);

        if (inkey() == ESCAPE) return;
    }

    /* Inventory -- if any */
    if (inven_cnt)
    {
        Term_clear();
        item_tester_full = TRUE;
        (void)show_inven(0, 0);
        prt("You are carrying: -more-", 0, 0);

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
                prt(format("Your home contains (page %d): -more-", k+1), 0, 0);


                /* Wait for it */
                if (inkey() == ESCAPE) return;
            }
        }
    }
}


static bool check_score(void)
{
    /* Clear screen */
    Term_clear();

    /* No score file */
    if (highscore_fd < 0)
    {
        msg_print("Score file unavailable.");

        msg_print(NULL);
        return FALSE;
    }

#ifndef SCORE_WIZARDS
    /* Wizard-mode pre-empts scoring */
    if (p_ptr->noscore & 0x000F)
    {
        msg_print("Score not registered for wizards.");

        msg_print(NULL);
        return FALSE;
    }
#endif

#ifndef SCORE_BORGS
    /* Borg-mode pre-empts scoring */
    if (p_ptr->noscore & 0x00F0)
    {
        msg_print("Score not registered for borgs.");

        msg_print(NULL);
        return FALSE;
    }
#endif

#ifndef SCORE_CHEATERS
    /* Cheaters are not scored */
    if (p_ptr->noscore & 0xFF00)
    {
        msg_print("Score not registered for cheaters.");

        msg_print(NULL);
        return FALSE;
    }
#endif

    /* Interupted */
    if (!p_ptr->total_winner && streq(p_ptr->died_from, "Interrupting"))

    {
        msg_print("Score not registered due to interruption.");

        msg_print(NULL);
        return FALSE;
    }

    /* Quitter */
    if (!p_ptr->total_winner && streq(p_ptr->died_from, "Quitting"))

    {
        msg_print("Score not registered due to quitting.");

        msg_print(NULL);
        return FALSE;
    }
    return TRUE;
}

/*
 * Close up the current game (player may or may not be dead)
 *
 * This function is called only from "main.c" and "signals.c".
 */
void close_game(void)
{
    char buf[1024];
    bool do_send = TRUE;

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

    /* Grab permissions */
    safe_setuid_grab();

    /* Open the high score file, for reading/writing */
    highscore_fd = fd_open(buf, O_RDWR);

    /* Drop permissions */
    safe_setuid_drop();

    /* Handle death */
    if (p_ptr->is_dead)
    {
        /* Handle retirement */
        if (p_ptr->total_winner) kingly();

        /* Save memories */
        if (!cheat_save || get_check("Save death? "))
        {

            if (!save_player()) msg_print("death save failed!");
        }
        else do_send = FALSE;

        /* You are dead */
        print_tomb();

        flush();

        /* Show more info */
        show_info();

        /* Clear screen */
        Term_clear();

        if (check_score())
        {
            if ((!send_world_score(do_send)))
            {
                if (get_check_strict("Stand by for later score registration? ", (CHECK_NO_ESCAPE | CHECK_NO_HISTORY)))
                {
                    p_ptr->wait_report_score = TRUE;
                    p_ptr->is_dead = FALSE;
                    if (!save_player()) msg_print("death save failed!");
                }
            }
            if (!p_ptr->wait_report_score)
                (void)top_twenty();
        }
        else if (highscore_fd >= 0)
        {
            display_scores_aux(0, 10, -1, NULL);
        }
#if 0
        /* Dump bones file */
        make_bones();
#endif
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
    (void)fd_close(highscore_fd);

    /* Forget the high score fd */
    highscore_fd = -1;

    /* Kill all temporal files */
    clear_saved_floor_files();

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
        if (one_in_(counter + 1)) 
            strcpy(output, buf);
    }

    /* Close the file */
    my_fclose(fp);

    /* Success */
    return counter ? 0 : -1;
}



/*
 * Process file for auto picker/destroyer.
 */
errr process_autopick_file(cptr name)
{
    char buf[1024];

    errr err = 0;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

    err = process_pref_file_aux(buf, PREF_TYPE_AUTOPICK);

    /* Result */
    return (err);
}


/*
 * Process file for player's history editor.
 */
errr process_histpref_file(cptr name)
{
    char buf[1024];
    errr err = 0;
    bool old_character_xtra = character_xtra;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);

    /* Hack -- prevent modification birth options in this file */
    character_xtra = TRUE;

    err = process_pref_file_aux(buf, PREF_TYPE_HISTPREF);

    character_xtra = old_character_xtra;

    /* Result */
    return (err);
}


static errr counts_seek(int fd, u32b where, bool flag)
{
    huge seekpoint;
    char temp1[128], temp2[128];
    u32b zero_header[3] = {0L, 0L, 0L};
    int i;

#ifdef SAVEFILE_USE_UID
    (void)sprintf(temp1, "%d.%s.%d%d%d", player_uid, savefile_base, p_ptr->pclass, p_ptr->personality, 0);
#else
    (void)sprintf(temp1, "%s.%d%d%d", savefile_base, p_ptr->pclass, p_ptr->personality, 0);
#endif
    for (i = 0; temp1[i]; i++)
        temp1[i] ^= (i+1) * 63;

    seekpoint = 0;
    while (1)
    {
        if (fd_seek(fd, seekpoint + 3 * sizeof(u32b)))
            return 1;
        if (fd_read(fd, (char*)(temp2), sizeof(temp2)))
        {
            if (!flag)
                return 1;
            /* add new name */
            fd_seek(fd, seekpoint);
            fd_write(fd, (char*)zero_header, 3*sizeof(u32b));
            fd_write(fd, (char*)(temp1), sizeof(temp1));
            break;
        }

        if (strcmp(temp1, temp2) == 0)
            break;

        seekpoint += 128 + 3 * sizeof(u32b);
    }

    return fd_seek(fd, seekpoint + where * sizeof(u32b));
}

u32b counts_read(int where)
{
    int fd;
    u32b count = 0;
    char buf[1024];

    path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, "z_info.raw");
    fd = fd_open(buf, O_RDONLY);

    if (counts_seek(fd, where, FALSE) ||
        fd_read(fd, (char*)(&count), sizeof(u32b)))
        count = 0;

    (void)fd_close(fd);

    return count;
}

errr counts_write(int where, u32b count)
{
    int fd;
    char buf[1024];
    errr err;

    path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, "z_info.raw");

    /* Grab permissions */
    safe_setuid_grab();

    fd = fd_open(buf, O_RDWR);

    /* Drop permissions */
    safe_setuid_drop();

    if (fd < 0)
    {
        /* File type is "DATA" */
        FILE_TYPE(FILE_TYPE_DATA);

        /* Grab permissions */
        safe_setuid_grab();

        /* Create a new high score file */
        fd = fd_make(buf, 0644);

        /* Drop permissions */
        safe_setuid_drop();
    }

    /* Grab permissions */
    safe_setuid_grab();

    err = fd_lock(fd, F_WRLCK);

    /* Drop permissions */
    safe_setuid_drop();

    if (err) return 1;

    counts_seek(fd, where, TRUE);
    fd_write(fd, (char*)(&count), sizeof(u32b));

    /* Grab permissions */
    safe_setuid_grab();

    err = fd_lock(fd, F_UNLCK);

    /* Drop permissions */
    safe_setuid_drop();

    if (err) return 1;

    (void)fd_close(fd);

    return 0;
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

        forget_lite();
        forget_view();
        clear_mon_lite();

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


        forget_lite();
        forget_view();
        clear_mon_lite();

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
    int wid, hgt;

    Term_get_size(&wid, &hgt);

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
    Term_putstr(0, hgt - 1, -1, TERM_RED,
            "A gruesome software bug LEAPS out at you!");


    /* Message */
    Term_putstr(45, hgt - 1, -1, TERM_RED, "Panic save...");



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
        Term_putstr(45, hgt - 1, -1, TERM_RED, "Panic save succeeded!");
    }

    /* Save failed */
    else
    {
        Term_putstr(45, hgt - 1, -1, TERM_RED, "Panic save failed!");
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


#else    /* HANDLE_SIGNALS */


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
#endif    /* HANDLE_SIGNALS */
