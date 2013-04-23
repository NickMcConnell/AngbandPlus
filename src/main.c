#define MAIN_C
/* File: main.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"


/*
 * Some machines have a "main()" function in their "main-xxx.c" file,
 * all the others use this file for their "main()" function.
 */


#ifdef USE_MAIN_C



struct module
{
	cptr name;
	cptr help;
	errr (*init)(int argc, char **argv);
};


/*
 * List of the available modules in the order they are tried.
 */
static const struct module modules[] =
{
#ifdef USE_XAW
	{ "xaw", help_xaw, init_xaw },
#endif /* USE_XAW */

#ifdef USE_X11
	{ "x11", help_x11, init_x11 },
#endif /* USE_X11 */

#ifdef USE_GTK
	{ "gtk", help_gtk, init_gtk },
#endif /* USE_GTK */

#ifdef USE_XPJ
	{ "xpj", help_xpj, init_xpj },
#endif /* USE_XPJ */

#ifdef USE_GCU
	{ "gcu", help_gcu, init_gcu },
#endif /* USE_GCU */

#ifdef USE_CAP
	{ "cap", help_cap, init_cap },
#endif /* USE_CAP */

#ifdef USE_DOS
	{ "dos", help_dos, init_dos },
#endif /* USE_DOS */

#ifdef USE_IBM
	{ "ibm", help_ibm, init_ibm },
#endif /* USE_IBM */

#ifdef USE_EMX
	{ "emx", help_emx, init_emx },
#endif /* USE_EMX */

#ifdef USE_SLA
	{ "sla", help_sla, init_sla },
#endif /* USE_SLA */

#ifdef USE_LSL
	{ "lsl", help_lsl, init_lsl },
#endif /* USE_LSL */

#ifdef USE_AMI
	{ "ami", help_ami, init_ami },
#endif /* USE_AMI */

#ifdef USE_VME
	{ "vme", help_vme, init_vme },
#endif /* USE_VME */

#ifdef USE_VCS
	{ "vcs", help_vcs, init_vcs },
#endif /* USE_VCS */
};


/*
 * A hook for "quit()".
 *
 * Close down, then fall back into "quit()".
 */
static void quit_hook(cptr UNUSED s)
{
	int j;

	/* Scan windows */
	for (j = ANGBAND_TERM_MAX - 1; j >= 0; j--)
	{
		/* Unused */
		if (!windows[j].term) continue;

		/* Nuke it */
		term_nuke(windows[j].term);
	}
}



/*
 * Set the stack size (for the Amiga)
 */
#ifdef AMIGA
# include <dos.h>
/* __near long */ __stack = 32768L;
#endif /* AMIGA */


/*
 * Set the stack size and overlay buffer (see main-286.c")
 */
#ifdef USE_286
# include <dos.h>
/* extern unsigned */ _stklen = 32768U;
/* extern unsigned */ _ovrbuffer = 0x1500;
#endif /* USE_286 */

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
 * Make sure that no other environment variables are called for before
 * path is finished with, as it may be overwritten otherwise.
 */
static void init_stuff(void)
{
	cptr path;

#if defined(AMIGA) || defined(VM)

	/* Hack -- prepare "path" */
	path = "Angband:";

#else /* AMIGA / VM */

#ifndef FIXED_PATHS

	cptr tail;

	/* Get the environment variable */
	tail = getenv("ANGBAND_PATH");

	/* Use the angband_path, or a default */
	path = (tail) ? tail : DEFAULT_PATH;

#else /* FIXED_PATHS */

	path = DEFAULT_PATH;
	
#endif /* FIXED_PATHS */

	/* Hack -- Add a path separator (only if needed) */
	if (!suffix(path, PATH_SEP))
	{
		C_TNEW(path_buf, strlen(path)+strlen(PATH_SEP)+1, char);
		sprintf(path_buf, "%s%s", path, PATH_SEP);
		init_file_paths(path_buf);
		TFREE(path_buf);
		return;
	}

#endif /* AMIGA / VM */

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
	switch (TOLOWER(info[0]))
	{
#ifndef FIXED_PATHS
		case 'a':
		{
			FREE(ANGBAND_DIR_APEX);
			ANGBAND_DIR_APEX = string_make(s+1);
			break;
		}

		case 'f':
		{
			FREE(ANGBAND_DIR_FILE);
			ANGBAND_DIR_FILE = string_make(s+1);
			break;
		}

		case 'h':
		{
			FREE(ANGBAND_DIR_HELP);
			ANGBAND_DIR_HELP = string_make(s+1);
			break;
		}

		case 'i':
		{
			FREE(ANGBAND_DIR_INFO);
			ANGBAND_DIR_INFO = string_make(s+1);
			break;
		}

		case 'x':
		{
			FREE(ANGBAND_DIR_XTRA);
			ANGBAND_DIR_XTRA = string_make(s+1);
			break;
		}

#ifdef VERIFY_SAVEFILE

		case 'b':
		case 'd':
		case 'e':
		case 's':
		{
			quit_fmt("Restricted option '-d%s'", info);
		}

#else /* VERIFY_SAVEFILE */

		case 'b':
		{
			FREE(ANGBAND_DIR_BONE);
			ANGBAND_DIR_BONE = string_make(s+1);
			break;
		}

		case 'd':
		{
			FREE(ANGBAND_DIR_DATA);
			ANGBAND_DIR_DATA = string_make(s+1);
			break;
		}

		case 'e':
		{
			FREE(ANGBAND_DIR_EDIT);
			ANGBAND_DIR_EDIT = string_make(s+1);
			break;
		}

		case 's':
		{
			FREE(ANGBAND_DIR_SAVE);
			ANGBAND_DIR_SAVE = string_make(s+1);
			break;
		}

#endif /* VERIFY_SAVEFILE */

#endif /* FIXED_PATHS */

		case 'u':
		{
			FREE(ANGBAND_DIR_USER);
			ANGBAND_DIR_USER = string_make(s+1);
			break;
		}

		default:
		{
			quit_fmt("Bad semantics in '-d%s'", info);
		}
	}
}


/*
 * Dump usage information and quit.
 */
static void show_usage(void)
{
	int i;

	printf("Usage: %s [options] [-- subopts]\n", argv0);
	puts("  -n       Start a new character");
	puts("  -f       Request fiddle (verbose) mode");
	puts("  -w       Request wizard mode");
	puts("  -v       Request sound mode");
	puts("  -g       Request graphics mode");
	puts("  -o       Request original keyset (default) ");
	puts("  -r       Request rogue-like keyset");
	puts("  -s<num>  Show <num> high scores");
	puts("  -u<who>  Use your <who> savefile");
	puts("  -d<def>  Define a 'lib' dir sub-path");
	puts("  -m<sys>  Force 'main-<sys>.c' usage");

	/* Print the name and help for each available module */
	for (i = 0; i < (int)N_ELEMENTS(modules); i++)
	{
		printf("     %s   %s\n",
		       modules[i].name, modules[i].help);
	}

	/* Actually abort the process */
	quit(NULL);
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

# ifdef SECURE
	/* Authenticate */
	Authenticate();
# endif /* SECURE */

#endif /* SET_UID */


	/* Get the file paths */
	init_stuff();


#ifdef SET_UID

	/* Get the user id (?) */
	player_uid = getuid();

#ifdef VMS
	/* Mega-Hack -- Factor group id */
	player_uid += (getgid() * 1000);
#endif /* VMS */

# ifdef SAFE_SETUID

#  if defined(HAVE_SETEGID) || defined(SAFE_SETUID_POSIX)

	/* Save some info for later */
	player_euid = geteuid();
	player_egid = getegid();

#  endif /* defined(HAVE_SETEGID) || defined(SAFE_SETUID_POSIX) */

#  if 0 /* XXX XXX XXX */

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

# endif /* SAFE_SETUID */

#endif /* SET_UID */


	/* Drop permissions */
	safe_setuid_drop();


#ifdef SET_UID

	/* Initialize the "time" checker */
	if (check_time_init() || check_time())
	{
		quit("The Gates of Slumber are closed (bad time).");
	}

	/* Initialize the "load" checker */
	if (check_load_init() || check_load())
	{
		quit("The Gates of Slumber are closed (bad load).");
	}

	/* Acquire the "user name" as a default player name */
	user_name(player_name, player_uid);

#endif /* SET_UID */


	/* Process the command line arguments */
	for (i = 1; args && (i < argc); i++)
	{
		cptr arg = argv[i];

		/* Require proper options */
		if (*arg++ != '-') show_usage();

		/* Check option format */
		switch (FORCELOWER(*arg))
		{
			/* Some never take an argument. */
			case 'n': case 'f': case 'v': case 'g': case 'r': case 'o': case '-':
			{
				if (arg[1] != '\0') show_usage();
				break;
			}
			/* Some always take an argument. */
			case 'u': case 'm': case 'd':
			{
				if (arg[1] == '\0') show_usage();
				break;
			}
			/* 's' can either take or not take an argument. */
			case 's':
			{
				break;
			}
			/* Nothing else is understood by the parser. */
			default:
			{
				show_usage();
			}
		}

		/* Analyze option */
		switch (FORCELOWER(*arg))
		{
			case 'n':
			{
				new_game = TRUE;
				break;
			}
			case 'f':
			{
				arg_fiddle = TRUE;
				break;
			}
			case 'v':
			{
				arg_sound = TRUE;
				break;
			}
			case 'g':
			{
				arg_graphics = TRUE;
				break;
			}
			case 'r':
			{
				arg_force_roguelike = TRUE;
				break;
			}
			case 'o':
			{
				arg_force_original = TRUE;
				break;
			}
			case 's':
			{
				show_score = atoi(arg+1);
				if (show_score <= 0) show_score = 10;
				break;
			}
			case 'u':
			{
				sprintf(player_name, "%.*s", NAME_LEN-1, arg+1);
				process_player_name();
				break;
			}
			case 'm':
			{
				mstr = arg+1;
				break;
			}
			case 'd':
			{
				change_path(arg+1);
				break;
			}
			case '-':
			{
				argv[i] = argv[0];
				argc = argc - i;
				argv = argv + i;
				args = FALSE;
				break;
			}
		}
	}

	/* Hack -- Forget standard args */
	if (args)
	{
		argc = 1;
		argv[1] = NULL;
	}



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

	/* Process the player name, if necessary. */
	process_player_name();

	/* Catch nasty signals */
	signals_init();

	/* Initialize */
	init_angband();

	/* Hack -- If requested, display scores and quit */
	if (show_score > 0) display_scores(0, show_score);

	/* Play the game */
	play_game(new_game);

	/* Free resources */
	cleanup_angband();

	/* Quit */
	quit(NULL);

	/* Exit */
	return (0);
}

#endif /* USE_MAIN_C */
