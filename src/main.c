/* File: main.c */

/*
 * "main()" function, argument-handling.
 *
 * This file is intended for use by ports that are command line-driven; ports
 * that are not, such as those for Macintosh, Windows, and cross-platform SDL
 * use their own code.
 *
 * Copyright (c) 2007 Ben Harrison, and others
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * Some machines have a "main()" function in their "main-xxx.c" file,
 * all the others use this file for their "main()" function.
 */


#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(USE_SDL)


#include "main.h"


/*
 * List of the available modules in the order they are tried.
 */
static const struct module modules[] =
{
#ifdef USE_GTK
	{ "gtk", help_gtk, init_gtk },
#endif /* USE_GTK */

#ifdef USE_X11
	{ "x11", help_x11, init_x11 },
#endif /* USE_X11 */

#ifdef USE_GCU
	{ "gcu", help_gcu, init_gcu },
#endif /* USE_GCU */

#ifdef USE_DOS
	{ "dos", help_dos, init_dos },
#endif /* USE_DOS */

#ifdef USE_IBM
	{ "ibm", help_ibm, init_ibm },
#endif /* USE_IBM */
};


/*
 * A hook for "quit()".
 *
 * Close down, then fall back into "quit()".
 */
static void quit_hook(cptr s)
{
	int j;

	/* Unused parameter */
	(void)s;

	/* Scan windows */
	for (j = TERM_MAX - 1; j >= 0; j--)
	{
		/* No window, or window is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* Nuke it */
		term_nuke(angband_term[j]);
	}
}


/*
 * Hack -- Display the scores in a given range and quit.
 */
static void display_scores(int from, int to)
{
	char buf[1024];

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* Display the scores */
	display_scores_aux(from, to, -1, NULL);

	/* Shut the high score file */
	(void)fd_close(highscore_fd);

	/* Forget the high score fd */
	highscore_fd = -1;

	/* Wait for response */
	prt("[Press any key to exit.]", Term->rows - 1, 17);
	(void)inkey(FALSE);
	prt("", Term->rows - 1, 0);

	/* Quit */
	quit(NULL);
}

/*
 * Initialize and verify the file paths, and the score file.
 *
 * First, we'll look for the <<VERSION_NAME>>_PATH environment variable,
 * and then look for the files in there.  If that doesn't work, we'll
 * try the DEFAULT_PATH constant.  So be sure that one of  these two
 * things works...
 *
 * We must ensure that the path ends with "PATH_SEP" if needed,
 * since the "init_file_paths()" function will simply append the
 * relevant "sub-directory names" to the given path.
 *
 * Note that the "path" must be "Sangband:" for the Amiga.  XXX XXX
 *
 * Make sure that the path doesn't overflow the buffer.  We have
 * to leave enough space for the path separator, directory, and
 * filenames.
 */
static void init_stuff(void)
{
	char path[1024];

	cptr tail = NULL;

#ifndef FIXED_PATHS

	/* Get the environment variable */
	tail = getenv(format("%s_PATH", VERSION_NAME));

#endif /* FIXED_PATHS */

	/* Use the sangband_path, or a default */
	(void)my_strcpy(path, tail ? tail : DEFAULT_PATH, sizeof(path));

	/* HACK! -- Make sure it's terminated */
	path[768] = '\0';

	/* Hack -- Add a path separator (only if needed) */
	if (!suffix(path, PATH_SEP)) (void)my_strcat(path, PATH_SEP, sizeof(path));

	/* Initialize */
	init_file_paths(path);
}



/*
 * Handle a "-d<what>=<path>" option
 *
 * The "<what>" can be any string starting with the same letter as the
 * name of a subdirectory of the "lib" folder (i.e. "i" or "info").
 *
 * The "<path>" can be any legal path for the given system, and should
 * not end in any special path separator (i.e. "/tmp" or "~/.ang-info").
 */
static void change_path(cptr info)
{
	cptr s;

	/* Find equal sign */
	s = strchr(info, '=');

	/* Verify equal sign */
	if (!s) quit_fmt("Try '-d<what>=<path>' not '-d%s'", info);

	/* Analyze */
	switch (my_tolower((unsigned char)info[0]))
	{
#ifndef FIXED_PATHS
		case 'a':
		{
			(void)string_free(ANGBAND_DIR_APEX);
			ANGBAND_DIR_APEX = string_make(s+1);
			break;
		}

		case 'f':
		{
			(void)string_free(ANGBAND_DIR_FILE);
			ANGBAND_DIR_FILE = string_make(s+1);
			break;
		}

		case 'h':
		{
			(void)string_free(ANGBAND_DIR_HELP);
			ANGBAND_DIR_HELP = string_make(s+1);
			break;
		}

		case 'i':
		{
			(void)string_free(ANGBAND_DIR_INFO);
			ANGBAND_DIR_INFO = string_make(s+1);
			break;
		}

		case 'x':
		{
			(void)string_free(ANGBAND_DIR_XTRA);
			ANGBAND_DIR_XTRA = string_make(s+1);
			break;
		}

		case 'b':
		{
			(void)string_free(ANGBAND_DIR_BONE);
			ANGBAND_DIR_BONE = string_make(s+1);
			break;
		}

		case 'd':
		{
			(void)string_free(ANGBAND_DIR_DATA);
			ANGBAND_DIR_DATA = string_make(s+1);
			break;
		}

		case 'e':
		{
			(void)string_free(ANGBAND_DIR_EDIT);
			ANGBAND_DIR_EDIT = string_make(s+1);
			break;
		}

		case 's':
		{
			(void)string_free(ANGBAND_DIR_SAVE);
			ANGBAND_DIR_SAVE = string_make(s+1);
			break;
		}

#endif /* FIXED_PATHS */

		case 'u':
		{
			(void)string_free(ANGBAND_DIR_USER);
			ANGBAND_DIR_USER = string_make(s+1);
			break;
		}

		default:
		{
			quit_fmt("Unknown path type (no corresponding folder).", info);
		}
	}
}


/*
 * Simple "main" function for multiple platforms.
 *
 * Note the special "--" option which terminates the processing of
 * standard options.  All non-standard options (if any) are passed
 * directly to the "init_xxx()" function.
 */
int main(int argc, char *argv[])
{
	int i;

	bool done = FALSE;

	bool new_game = FALSE;

	int show_score = 0;

	cptr mstr = NULL;

	bool args = TRUE;


	/* Save the "program name" XXX XXX XXX */
	argv0 = argv[0];


#ifdef SET_UID

	/* Default permissions on files */
	(void)umask(022);

#endif /* SET_UID */


	/* Get the file paths */
	init_stuff();


#ifdef SET_UID

	/* Get the user id (?) */
	player_uid = getuid();

	/* Save some info for later */
	player_egid = getegid();

#  if 0	/* XXX XXX XXX */

	/* Redundant setting necessary in case root is running the game */
	/* If not root or game not setuid the following two calls do nothing */

	if (setgid(getegid()) != 0)
	{
		quit("setgid(): cannot set permissions correctly!");
	}

	if (setuid(geteuid()) != 0)
	{
		quit("setuid(): cannot set permissions correctly!");
	}

#  endif /* 0 */

#endif /* SET_UID */


	/* Drop permissions */
	safe_setuid_drop();


#ifdef SET_UID

	/* Get the "user name" as a default player name */
	user_name(op_ptr->full_name, sizeof(op_ptr->full_name), player_uid);

#ifdef PRIVATE_USER_PATH

	/* Create directories for the user's files */
	create_user_dirs();

#endif /* PRIVATE_USER_PATH */

#endif /* SET_UID */


	/* Process the command line arguments */
	for (i = 1; args && (i < argc); i++)
	{
		cptr arg = argv[i];

		/* Require proper options */
		if (*arg++ != '-') goto usage;

		/* Analyze option */
		switch (*arg++)
		{
			case 'N':
			case 'n':
			{
				new_game = TRUE;
				break;
			}

			case 'F':
			case 'f':
			{
				arg_fiddle = TRUE;
				break;
			}

			case 'W':
			case 'w':
			{
				arg_wizard = TRUE;
				break;
			}

			case 'V':
			case 'v':
			{
				arg_sound = TRUE;
				break;
			}

			case 'G':
			case 'g':
			{
				arg_graphics = GRAPHICS_CHAR;
				break;
			}

			case 'R':
			case 'r':
			{
				arg_force_roguelike = TRUE;
				break;
			}

			case 'O':
			case 'o':
			{
				arg_force_original = TRUE;
				break;
			}

			case 'S':
			case 's':
			{
				show_score = atoi(arg);
				if (show_score <= 0) show_score = 10;
				continue;
			}

			case 'u':
			case 'U':
			{
				if (!*arg) goto usage;

				/* Get the savefile name */
				(void)my_strcpy(op_ptr->full_name, arg, sizeof(op_ptr->full_name));
				continue;
			}

			case 'm':
			case 'M':
			{
				if (!*arg) goto usage;
				mstr = arg;
				continue;
			}

			case 'd':
			case 'D':
			{
				change_path(arg);
				continue;
			}

			case '-':
			{
				argv[i] = argv[0];
				argc = argc - i;
				argv = argv + i;
				args = FALSE;
				break;
			}

			default:
			usage:
			{
				/* Dump usage information */
				puts("Usage: sangband [options] [-- subopts]");
				puts("  -n       Start a new character");
				puts("  -f       Request fiddle (verbose) mode");
				puts("  -w       Request wizard mode");
				puts("  -v       Request sound mode");
				puts("  -g       Request graphics mode");
				puts("  -o       Request original keyset (default)");
				puts("  -r       Request rogue-like keyset");
				puts("  -s<num>  Show <num> high scores (default: 10)");
				puts("  -u<who>  Use your <who> savefile");
				puts("  -d<def>  Define a 'lib' dir sub-path");
				puts("  -m<sys>  use Module <sys>, where <sys> can be:");

				/* Print the name and help for each available module */
				for (i = 0; i < N_ELEMENTS(modules); i++)
				{
					printf("     %s   %s\n",
					       modules[i].name, modules[i].help);
				}

				/* Actually abort the process */
				quit(NULL);
			}
		}
		if (*arg) goto usage;
	}

	/* Hack -- Forget standard args */
	if (args)
	{
		argc = 1;
		argv[1] = NULL;
	}

	/* Process the player name (but only if one is specified) */
	if (strlen(op_ptr->full_name)) process_player_name(TRUE);

	/* Install "quit" hook */
	quit_aux = quit_hook;


	/* Try the modules in the order specified by modules[] */
	for (i = 0; i < N_ELEMENTS(modules); i++)
	{
		/* User requested a specific module? */
		if (!mstr || (streq(mstr, modules[i].name)))
		{
			if (0 == modules[i].init(argc, argv))
			{
				ANGBAND_SYS = modules[i].name;
				done = TRUE;
				break;
			}
		}
	}

	/* Make sure we have a display! */
	if (!done) quit("Unable to prepare any 'display module'!");

	/* Catch nasty signals */
	signals_init();

	/* Initialize */
	init_angband();

	/* Hack -- If requested, display scores and quit */
	if (show_score > 0) display_scores(0, show_score);

	/* Wait for response */
	pause_line(Term->rows - 1);

	/* Play the game */
	play_game(new_game);

	/* Free resources */
	cleanup_angband();


	/* Quit */
	quit(NULL);

	/* Exit */
	return (0);
}

#endif /* !defined(MACINTOSH) && !defined(WINDOWS) && !defined(USE_SDL) */

