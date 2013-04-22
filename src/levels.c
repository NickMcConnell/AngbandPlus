/* File: levels.c */

/* Purpose: Levels functions */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Return the parameter of the given command in the given file
 */
static int start_line = 0;
bool get_command(const char *file, char comm, char *param)
{
	char buf[1024];
        int i = -1;
	FILE *fp;
	char *s;

	/* Build the filename */
        path_build(buf, 1024, ANGBAND_DIR_DNGN, file);

	/* Open the file */
	fp = my_fopen(buf, "r");

        /* The file exists ? */
        /* no ? then command not found */
        if (!fp) return FALSE;

        /* Parse to the end of the file or when the command is found */
        while (0 == my_fgets(fp, buf, 1024))
        {
		/* Advance the line number */
                i++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

                /* Is it the command we are looking for ? */
                if ((i > start_line) && (buf[0] == comm))
		{
			/* Acquire the text */
			s = buf+2;

                        start_line = i;

                        /* Get the parameter */
                        strcpy(param, s);

                        /* Close it */
                        my_fclose(fp);

                        return TRUE;
		}
                
        }

	/* Close it */
	my_fclose(fp);

        /* Assume command not found */
        return FALSE;
}


/*
 * Return the dungeon branch starting form the current dungeon/level
 */
int get_branch()
{
        char file[20], buf[5];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the branch */
        start_line = 0;
        if (get_command(file, 'B', buf)) return (atoi(buf));

        /* No branch ? return 0 */
        else return 0;
}

/*
 * Return the father dungeon branch
 */
int get_fbranch()
{
        char file[20], buf[5];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the branch */
        start_line = 0;
        if (get_command(file, 'A', buf)) return (atoi(buf));

        /* No branch ? return 0 */
        else return 0;
}

/*
 * Return the father dungeon level
 */
int get_flevel()
{
        char file[20], buf[5];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the level */
        start_line = 0;
        if (get_command(file, 'L', buf)) return (atoi(buf));

        /* No level ? return 0 */
        else return 0;
}

/*
 * Return the extension of the savefile for the level
 */
bool get_dungeon_save(char *buf)
{
        char file[20];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the level */
        start_line = 0;
        if (get_command(file, 'S', buf)) return (TRUE);
        else return FALSE;
}

/*
 * Return the special level
 */
bool get_dungeon_special(char *buf)
{
        char file[20];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the level */
        start_line = 0;
        if (get_command(file, 'U', buf)) return (TRUE);
        else return FALSE;
}

/*
 * Return the special level name
 */
bool get_dungeon_name(char *buf)
{
        char file[20];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the level */
        start_line = 0;
        if (get_command(file, 'N', buf)) return (TRUE);
        else return FALSE;
}

/*
 * Level flags
 */
static cptr level_flags1[] =
{
        "NO_TELEPORT",
        "ASK_LEAVE",
        "NO_STAIR",
        "SPECIAL",
        "NO_NEW_MONSTER",
        "DESC",
        "NO_GENO",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
	"XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
};

/*
 * Grab one level flag from a textual string
 */
static errr grab_one_level_flag(cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, level_flags1[i]))
		{
                        dungeon_flags1 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
        msg_format("Unknown level flag '%s'.", what);

	/* Error */
	return (1);
}

/*
 * Return the special level name
 */
void get_level_flags()
{
        char file[20];
        char buf[1024], *s, *t;

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        start_line = 0;

        /* Parse until done */
        while (get_command(file, 'F', buf))
        {
                /* Parse every entry textually */
                for (s = buf; *s; )
                {
                        /* Find the end of this entry */
                        for (t = s; *t && (*t != ' ') && (*t != '|'); ++t)
			  ;

                        /* Nuke and skip any dividers */
                        if (*t)
                        {
                                *t++ = '\0';
                                while (*t == ' ' || *t == '|') t++;
                        }

                        /* Parse this entry */
                        if (0 != grab_one_level_flag(s)) return;

                        /* Start the next entry */
                        s = t;
                }
        }
}

/*
 * Return the special level desc
 */
bool get_level_desc(char *buf)
{
        char file[20];

        sprintf(file, "dun%d.%d", dungeon_type, dun_level - d_info[dungeon_type].mindepth);

        /* Get and return the level */
        start_line = 0;
        if (get_command(file, 'D', buf)) return (TRUE);
        else return FALSE;
}
