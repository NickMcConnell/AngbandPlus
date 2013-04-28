/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* main.c: main() */

#include "posband.h"

/*
 * Some machines have a "main()" function in their "main-xxx.c" file,
 * all the others use this file for their "main()" function.
 */


#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(RISCOS)

#include "main.h"


/*
 * List of the available modules in the order they are tried.
 */
static const struct module modules[] =
{
#ifdef USE_GTK
    	{ "gtk", help_gtk, init_gtk },
#endif /* USE_GTK */

#ifdef USE_XAW
	{ "xaw", help_xaw, init_xaw },
#endif /* USE_XAW */

#ifdef USE_LFB
	{ "lfb", help_lfb, init_lfb },
#endif /* USE_LFB */

#ifdef USE_VCS
	{ "vcs", help_vcs, init_vcs },
#endif /* USE_VCS */

#ifdef USE_GCU
	{ "gcu", help_gcu, init_gcu },
#endif /* USE_GCU */

#ifdef USE_SDL
	{ "sdl", help_sdl, init_sdl },
#endif /* USE_SDL */


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
	for (j = ANGBAND_TERM_MAX - 1; j >= 0; j--)
	{
		/* Unused */
		if (!angband_term[j]) continue;

		/* Nuke it */
		term_nuke(angband_term[j]);
	}
}


/*
 * Initialize and verify the file paths, and the score file.
 *
 * Use the ANGBAND_PATH environment var if possible, else use
 * DEFAULT_PATH, and in either case, branch off appropriately.
 *
 * First, we'll look for the ANGBAND_PATH environment variable,
 * and then look for the files in there.  If that doesn't work,
 * we'll try the DEFAULT_PATH constant.  So be sure that one of
 * these two things works...
 *
 * We must ensure that the path ends with "PATH_SEP" if needed,
 * since the "init_file_paths()" function will simply append the
 * relevant "sub-directory names" to the given path.
 *
 * Note that the "path" must be "Angband:" for the Amiga, and it
 * is ignored for "VM/ESA", so I just combined the two.
 *
 * Make sure that the path doesn't overflow the buffer.  We have
 * to leave enough space for the path separator, directory, and
 * filenames.
 */
static void init_stuff(void)
{
	char path[1024];

	/* Use the angband_path, or a default */
	my_strcpy(path, DEFAULT_PATH, sizeof(path));

	/* Make sure it's terminated */
	path[511] = '\0';

	/* Hack -- Add a path separator (only if needed) */
	if (!suffix(path, PATH_SEP)) my_strcat(path, PATH_SEP, sizeof(path));

	/* Initialize */
	init_file_paths(path);
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


#ifdef USE_286
	/* Attempt to use XMS (or EMS) memory for swap space */
	if (_OvrInitExt(0L, 0L))
	{
		_OvrInitEms(0, 0, 64);
	}
#endif /* USE_286 */


#ifdef SET_UID

	/* Default permissions on files */
	(void)umask(022);

#endif /* SET_UID */


	/* Get the file paths */
	init_stuff();


#ifdef SET_UID

	/* Get the user id (?) */
	player_uid = getuid();
	
#ifdef HAVE_SETEGID
	/* Save some info for later */
	player_euid = geteuid();
	player_egid = getegid();
#endif /* HAVE_SETEGID */

#endif /* SET_UID */

	/* Drop permissions */
	safe_setuid_drop();

#ifdef SET_UID

	/* Get the "user name" as a default player name */
	user_name(op_ptr->full_name, sizeof(op_ptr->full_name), player_uid);

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
				/* Default graphics tile */
				arg_graphics = GRAPHICS_ADAM_BOLT;
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
				my_strcpy(op_ptr->full_name, arg, sizeof(op_ptr->full_name));
				continue;
			}

			case 'm':
			case 'M':
			{
				if (!*arg) goto usage;
				mstr = arg;
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
			        puts(PACKAGE_STRING);
				puts("Copyright (c) 2004-2005 Alexander Ulyanov and others");
				puts("");
				puts("Usage: posband [options] [-- subopts]");
				puts("  -n       Start a new character");
				/* puts("  -f       Request fiddle (verbose) mode"); */
				puts("  -w       Request wizard mode");
				puts("  -v       Request sound mode");
				puts("  -g       Request graphics mode");
				puts("  -o       Request original keyset (default)");
				puts("  -r       Request rogue-like keyset");
				puts("  -s<num>  Show <num> high scores (default: 10)");
				puts("  -u<who>  Use your <who> savefile");
				puts("  -m<sys>  use Module <sys>, where <sys> can be:");

				/* Print the name and help for each available module */
				for (i = 0; i < (int)N_ELEMENTS(modules); i++)
				{
					printf("     %s   %s\n",
					       modules[i].name, modules[i].help);
				}

				puts("");
				puts("Report bugs to <" PACKAGE_BUGREPORT ">.");
				
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


	/* Process the player name */
	process_player_name(TRUE);


	/* Install "quit" hook */
	quit_aux = quit_hook;

	/* Try the modules in the order specified by modules[] */
	for (i = 0; i < (int)N_ELEMENTS(modules); i++)
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
	pause_line(Term->hgt - 1);

	/* Play the game */
	play_game(new_game);

	/* Free resources */
	cleanup_angband();

	/* Quit */
	quit(NULL);

	/* Exit */
	return (0);
}

#endif /* !defined(MACINTOSH) && !defined(WINDOWS) && !defined(RISCOS) */
