/* File: init2.c */


/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "ui.h"
#include "ui-menu.h"
#include "init.h"
#include "cmds.h"
#include "game-event.h"
#include "game-cmd.h"

/* We are going to play NPPAngband*/
static void cmd_game_nppangband(void *unused)
{
	game_mode = GAME_NPPANGBAND;
}

/* We are going to play NPPMoria*/
static void cmd_game_nppmoria(void *unused)
{
	game_mode = GAME_NPPMORIA;
}


/* Extra options on the "item options" menu */
struct
{
	char tag;
	const char *name;
	void (*action)(void *unused);
} game_mode_menu[] =
{
	{ '1', "Play NPPAngband", cmd_game_nppangband },
	{ '2', "Play NPPMoria", cmd_game_nppmoria },
};

static char tag_game_mode_item(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(game_mode_menu)) return game_mode_menu[line].tag;

	return 0;
}

static int valid_game_mode_item(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(game_mode_menu))
	{
		return 1;
	}

	return 0;
}

static void display_game_mode_item(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	size_t line = (size_t) oid;

	c_prt(TERM_WHITE, game_mode_menu[line].name, row, col);
}


/*
 * Have the player decide, before the game is started, if they are playing NPP Moria or NPPAngband
 */
static void get_game_mode(void)
{
	int cursor = 0;
	ui_event_data c = EVENT_EMPTY;
	const char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };
	menu_type menu;
	menu_iter menu_f = { tag_game_mode_item , valid_game_mode_item, display_game_mode_item, NULL};

	WIPE(&menu, menu_type);
	menu.cmd_keys = cmd_keys;
	menu.title = "Please select a game to play.";
	menu.count = N_ELEMENTS(game_mode_menu),
	menu_init(&menu, MN_SKIN_SCROLL, &menu_f, &SCREEN_REGION);

	/* Start with a blank screen */
	game_mode = 0;

	while (game_mode == 0)
	{
		clear_from(0);

		c = menu_select(&menu, &cursor, 0);

		if (c.type == EVT_SELECT)
		{
			if ((size_t) cursor < N_ELEMENTS(game_mode_menu))
			{
				game_mode_menu[cursor].action(NULL);
			}
		}
	}

	/* Clear the screen */
	clear_from(0);
}

/* We are going to play NPPAngband*/
static void init_nppangband(void)
{
	z_info->max_level = 50;
	z_info->max_titles = z_info->max_level  / 5;
}

/* We are going to play NPPMoria*/
static void init_nppmoria(void)
{
	z_info->max_level = z_info->max_titles = PY_MAX_LEVEL_MORIA;
}

static void init_game_mode(void)
{
	if (game_mode == GAME_NPPMORIA) init_nppmoria();
	else init_nppangband();
}


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.  Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "lib/edit" directory.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass.  Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 */



/*
 * Find the default paths to all of our important sub-directories.
 *
 * All of the sub-directories should, by default, be located inside
 * the main directory, whose location is very system dependant and is
 * set by the ANGBAND_PATH environment variable, if it exists. (On multi-
 * user systems such as Linux this is not the default - see config.h for
 * more info.)
 *
 * This function takes a writable buffers, initially containing the
 * "path" to the "config", "lib" and "data" directories, for example,
 * "/etc/angband/", "/usr/share/angband" and "/var/games/angband" -
 * or a system dependant string, for example, ":lib:".  The buffer
 * must be large enough to contain at least 32 more characters.
 *
 * Various command line options may allow some of the important
 * directories to be changed to user-specified directories, most
 * importantly, the "apex" and "user" and "save" directories,
 * but this is done after this function, see "main.c".
 *
 * In general, the initial path should end in the appropriate "PATH_SEP"
 * string.  All of the "sub-directory" paths (created below or supplied
 * by the user) will NOT end in the "PATH_SEP" string, see the special
 * "path_build()" function in "util.c" for more information.
 *
 * Hack -- first we free all the strings, since this is known
 * to succeed even if the strings have not been allocated yet,
 * as long as the variables start out as "NULL".  This allows
 * this function to be called multiple times, for example, to
 * try several base "path" values until a good one is found.
 */
void init_file_paths(const char *configpath, const char *libpath, const char *datapath)
{

#ifdef PRIVATE_USER_PATH
	char buf[1024];
#endif /* PRIVATE_USER_PATH */

	/*** Free everything ***/

	/* Free the sub-paths */
	string_free(ANGBAND_DIR_APEX);
	string_free(ANGBAND_DIR_EDIT);
	string_free(ANGBAND_DIR_FILE);
	string_free(ANGBAND_DIR_HELP);
	string_free(ANGBAND_DIR_INFO);
	string_free(ANGBAND_DIR_SAVE);
	string_free(ANGBAND_DIR_PREF);
	string_free(ANGBAND_DIR_USER);
	string_free(ANGBAND_DIR_XTRA);

	/*** Free everything ***/

	/* Free the sub-paths */
	string_free(ANGBAND_DIR_APEX);
	string_free(ANGBAND_DIR_EDIT);
	string_free(ANGBAND_DIR_FILE);
	string_free(ANGBAND_DIR_HELP);
	string_free(ANGBAND_DIR_INFO);
	string_free(ANGBAND_DIR_SAVE);
	string_free(ANGBAND_DIR_PREF);
	string_free(ANGBAND_DIR_USER);
	string_free(ANGBAND_DIR_XTRA);

	string_free(ANGBAND_DIR_XTRA_FONT);
	string_free(ANGBAND_DIR_XTRA_GRAF);
	string_free(ANGBAND_DIR_XTRA_SOUND);
	string_free(ANGBAND_DIR_XTRA_HELP);
	string_free(ANGBAND_DIR_XTRA_ICON);


	/*** Prepare the paths ***/

	/* Build path names */
	ANGBAND_DIR_BONE = string_make(format("%sbone", configpath));
	ANGBAND_DIR_EDIT = string_make(format("%sedit", configpath));
	ANGBAND_DIR_FILE = string_make(format("%sfile", libpath));
	ANGBAND_DIR_HELP = string_make(format("%shelp", libpath));
	ANGBAND_DIR_INFO = string_make(format("%sinfo", libpath));
	ANGBAND_DIR_PREF = string_make(format("%spref", configpath));
	ANGBAND_DIR_XTRA = string_make(format("%sxtra", libpath));

	/* Build xtra/ paths */
	ANGBAND_DIR_XTRA_FONT = string_make(format("%s" PATH_SEP "font", ANGBAND_DIR_XTRA));
	ANGBAND_DIR_XTRA_GRAF = string_make(format("%s" PATH_SEP "graf", ANGBAND_DIR_XTRA));
	ANGBAND_DIR_XTRA_SOUND = string_make(format("%s" PATH_SEP "sound", ANGBAND_DIR_XTRA));
	ANGBAND_DIR_XTRA_HELP = string_make(format("%s" PATH_SEP "help", ANGBAND_DIR_XTRA));
	ANGBAND_DIR_XTRA_ICON = string_make(format("%s" PATH_SEP "icon", ANGBAND_DIR_XTRA));

#ifdef PRIVATE_USER_PATH

	/* Build the path to the user specific directory */
	path_build(buf, sizeof(buf), PRIVATE_USER_PATH, VERSION_NAME);
	ANGBAND_DIR_USER = string_make(buf);

#else /* !PRIVATE_USER_PATH */

        ANGBAND_DIR_USER = string_make(format("%suser", datapath));

#endif /* PRIVATE_USER_PATH */


#ifdef USE_PRIVATE_PATHS

	/* Build the path to the user specific sub-directory */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "scores");
	ANGBAND_DIR_APEX = string_make(buf);

	/* Build the path to the user specific sub-directory */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "save");
	ANGBAND_DIR_SAVE = string_make(buf);

#else /* !USE_PRIVATE_PATHS */

	/* Build pathnames */
	ANGBAND_DIR_APEX = string_make(format("%sapex", datapath));
	ANGBAND_DIR_SAVE = string_make(format("%ssave", datapath));

#endif /* USE_PRIVATE_PATHS */
}


/*
 * Create any missing directories. We create only those dirs which may be
 * empty (user/, save/, apex/, info/, help/). The others are assumed
 * to contain required files and therefore must exist at startup
 * (edit/, pref/, file/, xtra/).
 *
 * ToDo: Only create the directories when actually writing files.
 */
void create_needed_dirs(void)
{
	char dirpath[512];

	path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_USER, "");
	if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

	path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_SAVE, "");
	if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

	path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_APEX, "");
	if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

	path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_INFO, "");
	if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

	path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_HELP, "");
	if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);
}


/*
 * Hack -- help give useful error messages
 */
int error_idx;
int error_line;


/*
 * Standard error message text
 */
static cptr err_str[PARSE_ERROR_MAX] =
{
	NULL,
	"parse error",
	"invalid flag specification",
	"invalid number of items (0-99)",
	"invalid spell frequency",
	"invalid random value",
	"missing colon",
	"missing field",
	"missing record header",
	"non-sequential records",
	"value not a number",
	"obsolete file",
	"value out of bounds",
	"out of memory",
	"too few entries",
	"too many entries",
	"undefined directive",
	"unrecognised blow",
	"unrecognised tval name",
	"unrecognised sval name",
	"vault too big",
	"too many allocations",
	"too many arguments",
	"too many non sequential quests",
	"name too long",
};


/*
 * File headers
 */
header z_head;
header v_head;
header f_head;
header k_head;
header a_head;
header e_head;
header r_head;
header p_head;
header c_head;
header h_head;
header b_head;
header g_head;
header flavor_head;
header q_head;
header n_head;
header t_head;


/*
 * Initialize the header of an *_info array.
 */
static void init_header(header *head, int num, int len)
{
       /* Save the "version" */
       head->v_major = VERSION_MAJOR;
       head->v_minor = VERSION_MINOR;
       head->v_patch = VERSION_PATCH;
       head->v_extra = VERSION_EXTRA;

       /* Save the "record" information */
       head->info_num = num;
       head->info_len = len;

       /* Save the size of "*_head" and "*_info" */
       head->head_size = sizeof(header);
       head->info_size = head->info_num * head->info_len;
}


/*
 * Display a parser error message.
 */
static void display_parse_error(cptr filename, errr err, cptr buf)
{
	cptr oops;

	/* Error string */
	oops = (((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

	/* Oops */
	msg_format("Error at line %d of '%s.txt'.", error_line, filename);
	msg_format("Record %d contains a '%s' error.", error_idx, oops);
	msg_format("Parsing '%s'.", buf);
	message_flush();

	/* Quit */
	quit_fmt("Error in '%s.txt' file.", filename);
}



/*
 * Initialize a "*_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_info(cptr filename, header *head)
{
	ang_file *fh;

	errr err = 1;

	char txt_file[1024];

	char buf[1024];

	void *fake_name;
	void *fake_text;

	/* Build the filename */
	path_build(txt_file, sizeof(txt_file), ANGBAND_DIR_EDIT, format("%s.txt", filename));

	/* Allocate the "*_info" array */
	head->info_ptr = C_ZNEW(head->info_size, char);

	/* MegaHack -- make "fake" arrays */
	if (z_info)
	{
		head->name_ptr = C_ZNEW(z_info->fake_name_size, char);
		head->text_ptr = C_ZNEW(z_info->fake_text_size, char);
	}

	/*** Load the ascii template file ***/

	/* Open the file */
	fh = file_open(txt_file, MODE_READ, -1);
	if (!fh) quit(format("Cannot open '%s.txt' file.", filename));

	/* Parse the file */
	err = init_info_txt(fh, buf, head, head->parse_info_txt);

	file_close(fh);

	/* Errors */
	if (err) display_parse_error(filename, err, buf);

	/* Copy the parsed data into the real array from the fakes */
	fake_name = head->name_ptr;
	head->name_ptr = C_ZNEW(head->name_size, char);
	memcpy(head->name_ptr, fake_name, head->name_size);

	fake_text = head->text_ptr;
	head->text_ptr = C_ZNEW(head->text_size, char);
	memcpy(head->text_ptr, fake_text, head->text_size);

	/* Free the fake arrays */
	if (z_info)
	{
		FREE(fake_name);
		FREE(fake_text);
	}

	/* Success */
	return (0);
}

/*
 * Free the allocated memory for the info-, name-, and text- arrays.
 */
static errr free_info(header *head)
{
	if (head->info_size)
		FREE(head->info_ptr);

	if (head->name_size)
		FREE(head->name_ptr);

	if (head->text_size)
		FREE(head->text_ptr);

	/* Success */
	return (0);
}

/*
 * Initialize the "z_info" array
 */
static errr init_z_info(void)
{
	errr err;

	/* Init the header */
	init_header(&z_head, 1, sizeof(maxima));

	/* Save a pointer to the parsing function */
	z_head.parse_info_txt = parse_z_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("limits", &z_head);
	else err = init_info("m_limits", &z_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	z_info = z_head.info_ptr;

	return (err);
}


/*
 * Initialize the "f_info" array
 */
static errr init_f_info(void)
{
	errr err;

	/* Init the header */
	init_header(&f_head, z_info->f_max, sizeof(feature_type));

	/* Save a pointer to the parsing function */
	f_head.parse_info_txt = parse_f_info;

	err = init_info("terrain", &f_head);

	/* Set the global variables */
	f_info = f_head.info_ptr;
	f_name = f_head.name_ptr;
	f_text = f_head.text_ptr;

	return (err);
}


/*
 * Initialize the "k_info" array
 */
static errr init_k_info(void)
{
	errr err;

	/* Init the header */
	init_header(&k_head, z_info->k_max, sizeof(object_kind));

	/* Save a pointer to the parsing function */
	k_head.parse_info_txt = parse_k_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("object", &k_head);
	else err = init_info("m_object", &k_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	k_info = k_head.info_ptr;
	k_name = k_head.name_ptr;
	k_text = k_head.text_ptr;

	return (err);
}

/*
 * Initialize the "t_info" array
 */
static errr init_t_info(void)
{
	errr err;

	/* Init the header */
	init_header(&t_head, z_info->ghost_template_max, sizeof(ghost_template));

	/* Save a pointer to the parsing function */
	t_head.parse_info_txt = parse_t_info;

	err = init_info("player_ghost", &t_head);

	/* Set the global variables */
	t_info = t_head.info_ptr;
	t_name = t_head.name_ptr;
	t_text = t_head.text_ptr;

	return (err);
}




/*
 * Initialize the "a_info" array
 */
static errr init_a_info(void)
{
	errr err;

	/* Init the header */
	init_header(&a_head, z_info->art_max, sizeof(artifact_type));

	/* Save a pointer to the parsing function */
	a_head.parse_info_txt = parse_a_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("artifact", &a_head);
	else err = init_info("m_artifact", &a_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	a_info = a_head.info_ptr;

	a_text = a_head.text_ptr;

	return (err);
}



/*
 * Initialize the "e_info" array
 */
static errr init_e_info(void)
{
	errr err;

	/* Init the header */
	init_header(&e_head, z_info->e_max, sizeof(ego_item_type));

	/* Save a pointer to the parsing function */
	e_head.parse_info_txt = parse_e_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("ego_item", &e_head);
	else err = init_info("m_ego_item", &e_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	e_info = e_head.info_ptr;
	e_name = e_head.name_ptr;
	e_text = e_head.text_ptr;
	return (err);
}


/*
 * Initialize the "r_info" array
 */
static errr init_r_info(void)
{
	errr err;

	/* Init the header */
	init_header(&r_head, z_info->r_max, sizeof(monster_race));

	/* Save a pointer to the parsing function */
	r_head.parse_info_txt = parse_r_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("monster", &r_head);
	else err = init_info("m_monster", &r_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	r_info = r_head.info_ptr;
	r_text = r_head.text_ptr;

	return (err);
}



/*
 * Initialize the "v_info" array
 */
static errr init_v_info(void)
{
	errr err;

	/* Init the header */
	init_header(&v_head, z_info->v_max, sizeof(vault_type));

	/* Save a pointer to the parsing function */
	v_head.parse_info_txt = parse_v_info;

	err = init_info("vault", &v_head);

	/* Set the global variables */
	v_info = v_head.info_ptr;
	v_name = v_head.name_ptr;
	v_text = v_head.text_ptr;

	return (err);
}



/*
 * Initialize the "p_info" array
 */
static errr init_p_info(void)
{
	errr err;

	/* Init the header */
	init_header(&p_head, z_info->p_max, sizeof(player_race));

	/* Save a pointer to the parsing function */
	p_head.parse_info_txt = parse_p_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("p_race", &p_head);
	else err = init_info("m_p_race", &p_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	p_info = p_head.info_ptr;
	p_name = p_head.name_ptr;
	p_text = p_head.text_ptr;

	return (err);
}


/*
 * Initialize the "c_info" array
 */
static errr init_c_info(void)
{
	errr err;

	/* Init the header */
	init_header(&c_head, z_info->c_max, sizeof(player_class));

	/* Save a pointer to the parsing function */
	c_head.parse_info_txt = parse_c_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("p_class", &c_head);
	else err = init_info("m_p_class", &c_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	c_info = c_head.info_ptr;
	c_name = c_head.name_ptr;
	c_text = c_head.text_ptr;

	return (err);
}



/*
 * Initialize the "h_info" array
 */
static errr init_h_info(void)
{
	errr err;

	/* Init the header */
	init_header(&h_head, z_info->h_max, sizeof(hist_type));

	/* Save a pointer to the parsing function */
	h_head.parse_info_txt = parse_h_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("p_hist", &h_head);
	else err = init_info("m_p_hist", &h_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	h_info = h_head.info_ptr;
	h_text = h_head.text_ptr;

	return (err);
}



/*
 * Initialize the "b_info" array
 */
static errr init_b_info(void)
{
	errr err;

	/* Init the header */
	init_header(&b_head, (u16b)(MAX_STORES * z_info->b_max), sizeof(owner_type));

	/* Save a pointer to the parsing function */
	b_head.parse_info_txt = parse_b_info;


	if (game_mode == GAME_NPPANGBAND) err = init_info("shop_own", &b_head);
	else err = init_info("m_shop_own", &b_head);

	/* Set the global variables */
	b_info = b_head.info_ptr;
	b_name = b_head.name_ptr;
	b_text = b_head.text_ptr;

	return (err);
}


/*
 * Initialize the "q_info" array
 */
static errr init_q_info(void)
{
	errr err;

	/* Init the header */
	init_header(&q_head, z_info->q_max, sizeof(quest_type));

	/* Save a pointer to the parsing function */
	q_head.parse_info_txt = parse_q_info;

	if (game_mode == GAME_NPPANGBAND) err = init_info("quest", &q_head);
	else err = init_info("m_quest", &q_head);  /* game_mode == NPPMORIA */

	/* Set the global variables */
	q_info = q_head.info_ptr;
	q_name = q_head.name_ptr;

	return (err);
}

/*
 * Initialize the "n_info" structure
 */
static errr init_n_info(void)
{
  errr err;

  /* Init the header */
  init_header(&n_head, 1, sizeof(names_type));

  /* Save a pointer to the parsing function */
  n_head.parse_info_txt = parse_n_info;

  err = init_info("names", &n_head);

  n_info = n_head.info_ptr;

  return (err);
}



/*
 * Initialize the "flavor_info" array
 */
static errr init_flavor_info(void)
{
	errr err;

	/* Init the header */
	init_header(&flavor_head, z_info->flavor_max, sizeof(flavor_type));

	/* Save a pointer to the parsing function */
	flavor_head.parse_info_txt = parse_flavor_info;

	err = init_info("flavor", &flavor_head);

	/* Set the global variables */
	flavor_info = flavor_head.info_ptr;
	flavor_name = flavor_head.name_ptr;
	flavor_text = flavor_head.text_ptr;

	return (err);
}


static void autoinscribe_clean(void)
{
	if(inscriptions)
	{
		FREE(inscriptions);
	}

	inscriptions = 0;
	inscriptionsCount = 0;
}

static void autoinscribe_init(void)
{
	/* Paranoia */
	autoinscribe_clean();

	inscriptions = C_ZNEW(AUTOINSCRIPTIONS_MAX, autoinscription);
}


/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
	int i;

	/*** Prepare the various "bizarre" arrays ***/

	/* Initialize the "macro" package */
	(void)macro_init();

	/* Initialize the "quark" package */
	(void)quarks_init();

	/* Initialize squelch things */
	autoinscribe_init();
	init_cmd_know();

	/* Initialize the "message" package */
	(void)messages_init();

	/*** Prepare grid arrays ***/

	/* Array of grids */
	view_g = C_ZNEW(VIEW_MAX, u16b);

	/* Array of grids */
	temp_g = C_ZNEW(TEMP_MAX, u16b);

	/* Hack -- use some memory twice */
	temp_y = ((byte*)(temp_g)) + 0;
	temp_x = ((byte*)(temp_g)) + TEMP_MAX;

	/* Array of grids */
	fire_g = C_ZNEW(VIEW_MAX, u16b);

	/* has_LIGHT patch causes both temp_g and temp_x/y to be used
	   in targetting mode: can't use the same memory any more. */
	temp_y = C_ZNEW(TEMP_MAX, byte);
	temp_x = C_ZNEW(TEMP_MAX, byte);

	/* Array of dynamic grids */
	dyna_g = C_ZNEW(DYNA_MAX, dynamic_grid_type);

	/* Array of stacked monster messages */
	mon_msg = C_ZNEW(MAX_STORED_MON_MSG, monster_race_message);
	mon_message_hist = C_ZNEW(MAX_STORED_MON_CODES, monster_message_history);

	/* Prepare monster movement array*/
	mon_moment_info = C_ZNEW(z_info->m_max, move_moment_type);

	/*** Prepare dungeon arrays ***/

	/* Padded into array */
	cave_info = C_ZNEW(MAX_DUNGEON_HGT, u16b_256);

	/* Feature array */
	cave_feat = C_ZNEW(MAX_DUNGEON_HGT, byte_wid);

	/* Entity arrays */
	cave_o_idx = C_ZNEW(MAX_DUNGEON_HGT, s16b_wid);
	cave_m_idx = C_ZNEW(MAX_DUNGEON_HGT, s16b_wid);
	cave_x_idx = C_ZNEW(MAX_DUNGEON_HGT, s16b_wid);

#ifdef MONSTER_SMELL

	/* Flow arrays */
	cave_when = C_ZNEW(MAX_DUNGEON_HGT, byte_wid);

#endif /* MONSTER_SMELL */

	/*start with cost at center 0*/
	for (i = 0; i < MAX_FLOWS; i++)
	{
		cost_at_center[i] = 0;
	}


	/*** Prepare "vinfo" array ***/

	/* Used by "update_view()" */
	(void)vinfo_init();


	/*** Prepare entity arrays ***/

	/* Objects */
	o_list = C_ZNEW(z_info->o_max, object_type);

	/* Monsters */
	mon_list = C_ZNEW(z_info->m_max, monster_type);

	/* Effects */
	x_list = C_ZNEW(z_info->x_max, effect_type);


	/*** Prepare mosnter lore array ***/

	/* Lore */
	l_list = C_ZNEW(z_info->r_max, monster_lore);

	/*** Prepare terrain lore array ***/

	/* Lore */
	f_l_list = C_ZNEW(z_info->f_max, feature_lore);

	/*** Prepare artifact lore array ***/

    /* Lore */
    a_l_list = C_ZNEW(z_info->art_max, artifact_lore);

	/*** Prepare mouse buttons ***/

	button_init(button_add_text, button_kill_text);

	/*** Prepare the inventory ***/

	/* Allocate it */
	inventory = C_ZNEW(ALL_INVEN_TOTAL, object_type);


	/*** Prepare the stores ***/

	/* Allocate the stores */
	store = C_ZNEW(MAX_STORES, store_type);

	/* Fill in each store */
	for (i = 0; i < MAX_STORES; i++)
	{

		/* Get the store */
		store_type *st_ptr = &store[i];

		/* Assume full stock */
		st_ptr->stock_size = STORE_INVEN_MAX;

		/* Allocate the stock */
		st_ptr->stock = C_ZNEW(st_ptr->stock_size, object_type);

	}


	/*** Prepare the options ***/

	/* Initialize the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		/* Default value */
		op_ptr->opt[i] = options[i].normal;
	}

	/* Initialize the window flags */
	for (i = 0; i < ANGBAND_TERM_MAX; i++)
	{
		/* Assume no flags */
		op_ptr->window_flag[i] = 0L;
	}

	/*Clear the update flags*/
	p_ptr->notice = 0L;
	p_ptr->update = 0L;
	p_ptr->redraw = 0L;


	/*** Pre-allocate space for the "format()" buffer ***/

	/* Hack -- Just call the "format()" function */
	(void)format("%s", MAINTAINER);


	/* Success */
	return (0);
}



/*
 * Initialize some other arrays
 */
static errr init_alloc(void)
{
	int i, j;

	object_kind *k_ptr;

	feature_type *f_ptr;

	monster_race *r_ptr;

	ego_item_type *e_ptr;

	alloc_entry *table;

	s16b num[MAX_DEPTH_ALL];

	s16b aux[MAX_DEPTH_ALL];


	/*** Analyze object allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(aux, MAX_DEPTH_ALL, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(num, MAX_DEPTH_ALL, s16b);

	/* Size of "alloc_kind_table" */
	alloc_kind_size = 0;

	/* Scan the objects */
	for (i = 1; i < z_info->k_max; i++)
	{
		k_ptr = &k_info[i];

		/* Scan allocation pairs */
		for (j = 0; j < 4; j++)
		{
			/* Count the "legal" entries */
			if (k_ptr->chance[j])
			{
				/* Count the entries */
				alloc_kind_size++;

				/* Group by level */
				num[k_ptr->locale[j]]++;
			}
		}
	}

	/* Collect the level indexes */
	for (i = 1; i < MAX_DEPTH_ALL; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/* Paranoia */
	if (!num[0]) quit("No town objects!");


	/*** Initialize object allocation info ***/

	/* Allocate the alloc_kind_table */
	alloc_kind_table = C_ZNEW(alloc_kind_size, alloc_entry);

	/* Get the table entry */
	table = alloc_kind_table;

	/* Scan the objects */
	for (i = 1; i < z_info->k_max; i++)
	{
		k_ptr = &k_info[i];

		/* Scan allocation pairs */
		for (j = 0; j < 4; j++)
		{
			/* Count the "legal" entries */
			if (k_ptr->chance[j])
			{
				int p, x, y, z;

				/* Extract the base level */
				x = k_ptr->locale[j];

				/* Extract the base probability */
				p = (100 / k_ptr->chance[j]);

				/* Skip entries preceding our locale */
				y = (x > 0) ? num[x-1] : 0;

				/* Skip previous entries at this locale */
				z = y + aux[x];

				/* Load the entry */
				table[z].index = i;
				table[z].level = x;
				table[z].prob1 = p;
				table[z].prob2 = p;
				table[z].prob3 = p;

				/* Another entry complete for this locale */
				aux[x]++;
			}
		}
	}

	/*** Analyze feature allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(&aux, MAX_DEPTH_ALL, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(&num, MAX_DEPTH_ALL, s16b);

	/* Size of "alloc_feat_table" */
	alloc_feat_size = 0;

	/* Scan the features */
	for (i = 1; i < z_info->f_max; i++)
	{
		/* Get the i'th race */
		f_ptr = &f_info[i];

		/* Legal features */
		if (f_ptr->f_rarity)
		{
			/* Count the entries */
			alloc_feat_size++;

			/* Group by level */
			num[f_ptr->f_level]++;
		}
	}

	/* Collect the level indexes */
	for (i = 1; i < MAX_DEPTH_ALL; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/* Paranoia - not really necessary */
	if (!num[0]) quit("No town features!");

	/*** Initialize feature allocation info ***/

	/* Allocate the alloc_feat_table */
	alloc_feat_table = C_ZNEW(alloc_feat_size, alloc_entry);

	/* Get the table entry */
	table = alloc_feat_table;

	/* Scan the features */
	for (i = 1; i < z_info->f_max; i++)
	{
		/* Get the i'th feature */
		f_ptr = &f_info[i];

		/* Count valid pairs */
		if (f_ptr->f_rarity)
		{
			int p, x, y, z;

			/* Extract the base level */
			x = f_ptr->f_level;

			/* Extract the base probability */
			p = (100 / f_ptr->f_rarity);

			/* Skip entries preceding our locale */
			y = (x > 0) ? num[x-1] : 0;

			/* Skip previous entries at this locale */
			z = y + aux[x];

			/* Load the entry */
			table[z].index = i;
			table[z].level = x;
			table[z].prob1 = p;
			table[z].prob2 = p;
			table[z].prob3 = p;

			/* Another entry complete for this locale */
			aux[x]++;
		}
	}

	/*** Analyze monster allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(aux, MAX_DEPTH_ALL, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(num, MAX_DEPTH_ALL, s16b);

	/* Size of "alloc_race_table" */
	alloc_race_size = 0;

	/* Scan the monsters*/
	for (i = 1; i < z_info->r_max; i++)
	{
		/* Get the i'th race */
		r_ptr = &r_info[i];

		/* Legal monsters */
		if (r_ptr->rarity)
		{
			/* Count the entries */
			alloc_race_size++;

			/* Group by level */
			num[r_ptr->level]++;
		}
	}

	/* Collect the level indexes */
	for (i = 1; i < MAX_DEPTH_ALL; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/* Paranoia */
	if (!num[0]) quit("No town monsters!");


	/*** Initialize monster allocation info ***/

	/* Allocate the alloc_race_table */
	alloc_race_table = C_ZNEW(alloc_race_size, alloc_entry);

	/* Get the table entry */
	table = alloc_race_table;

	/* Scan the monsters*/
	for (i = 1; i < z_info->r_max; i++)
	{
		/* Get the i'th race */
		r_ptr = &r_info[i];

		/* Count valid pairs */
		if (r_ptr->rarity)
		{
			int p, x, y, z;

			/* Extract the base level */
			x = r_ptr->level;

			/* Extract the base probability */
			p = (100 / r_ptr->rarity);

			/* Skip entries preceding our locale */
			y = (x > 0) ? num[x-1] : 0;

			/* Skip previous entries at this locale */
			z = y + aux[x];

			/* Load the entry */
			table[z].index = i;
			table[z].level = x;
			table[z].prob1 = p;
			table[z].prob2 = p;
			table[z].prob3 = p;

			/* Another entry complete for this locale */
			aux[x]++;
		}
	}

	/*** Analyze ego_item allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(aux, MAX_DEPTH_ALL, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(num, MAX_DEPTH_ALL, s16b);

	/* Size of "alloc_ego_table" */
	alloc_ego_size = 0;

	/* Scan the ego items */
	for (i = 1; i < z_info->e_max; i++)
	{
		/* Get the i'th ego item */
		e_ptr = &e_info[i];

		/* Legal items */
		if (e_ptr->rarity)
		{
			/* Count the entries */
			alloc_ego_size++;

			/* Group by level */
			num[e_ptr->level]++;
		}
	}

	/* Collect the level indexes */
	for (i = 1; i < MAX_DEPTH_ALL; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/*** Initialize ego-item allocation info ***/

	/* Allocate the alloc_ego_table */
	alloc_ego_table = C_ZNEW(alloc_ego_size, alloc_entry);

	/* Get the table entry */
	table = alloc_ego_table;

	/* Scan the ego-items */
	for (i = 1; i < z_info->e_max; i++)
	{
		/* Get the i'th ego item */
		e_ptr = &e_info[i];

		/* Count valid pairs */
		if (e_ptr->rarity)
		{
			int p, x, y, z;

			/* Extract the base level */
			x = e_ptr->level;

			/* Extract the base probability */
			p = (100 / e_ptr->rarity);

			/* Skip entries preceding our locale */
			y = (x > 0) ? num[x-1] : 0;

			/* Skip previous entries at this locale */
			z = y + aux[x];

			/* Load the entry */
			table[z].index = i;
			table[z].level = x;
			table[z].prob1 = p;
			table[z].prob2 = p;
			table[z].prob3 = p;

			/* Another entry complete for this locale */
			aux[x]++;
		}
	}

	/* Success */
	return (0);
}

/*
 * Hack -- main Angband initialization entry point
 *
 * Verify some files, display the "news.txt" file, create
 * the high score file, initialize all internal arrays, and
 * load the basic "user pref files".
 *
 * Be very careful to keep track of the order in which things
 * are initialized, in particular, the only thing *known* to
 * be available when this function is called is the "z-term.c"
 * package, and that may not be fully initialized until the
 * end of this function, when the default "user pref files"
 * are loaded and "Term_xtra(TERM_XTRA_REACT,0)" is called.
 *
 * Note that this function attempts to verify the "news" file,
 * and the game aborts (cleanly) on failure, since without the
 * "news" file, it is likely that the "lib" folder has not been
 * correctly located.  Otherwise, the news file is displayed for
 * the user.
 *
 * Note that this function attempts to verify (or create) the
 * "high score" file, and the game aborts (cleanly) on failure,
 * since one of the most common "extraction" failures involves
 * failing to extract all sub-directories (even empty ones), such
 * as by failing to use the "-d" option of "pkunzip", or failing
 * to use the "save empty directories" option with "Compact Pro".
 * This error will often be caught by the "high score" creation
 * code below, since the "lib/apex" directory, being empty in the
 * standard distributions, is most likely to be "lost", making it
 * impossible to create the high score file.
 *
 * Note that various things are initialized by this function,
 * including everything that was once done by "init_some_arrays".
 *
 * This initialization involves the parsing of special files
 * in the "lib/data" and sometimes the "lib/edit" directories.
 *
 * Note that the "template" files are initialized first, since they
 * often contain errors.  This means that macros and message recall
 * and things like that are not available until after they are done.
 *
 * We load the default "user pref files" here in case any "color"
 * changes are needed before character creation.
 *
 * Note that the "graf-xxx.prf" file must be loaded separately,
 * if needed, in the first (?) pass through "TERM_XTRA_REACT".
 */
bool init_angband(void)
{
	
	/* If we have a savefile, use that for game mode instead */
	if (savefile[0])
	{
		load_gamemode();
	}
		
	/* Which game are we playing? */
	if (game_mode == 0)
	{
		get_game_mode();
	}

	event_signal(EVENT_ENTER_INIT);

	/*** Initialize some arrays ***/

	/* Initialize size info */
	event_signal_string(EVENT_INITSTATUS, "Initializing array sizes...");
	if (init_z_info()) quit("Cannot initialize sizes");

	/* Prepare some things according to the game being played */
	init_game_mode();

	/* Initialize feature info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (features)");
	if (init_f_info()) quit("Cannot initialize features");

	/* Initialize object info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (objects)");
	if (init_k_info()) quit("Cannot initialize objects");

	/* Initialize object info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (ghosts)");
	if (init_t_info()) quit("Cannot initialize ghosts");

	/* Initialize artifact info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (artifacts)");
	if (init_a_info()) quit("Cannot initialize artifacts");

	/* Initialize ego-item info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (ego-items)");
	if (init_e_info()) quit("Cannot initialize ego-items");

	/* Initialize monster info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (monsters)");
	if (init_r_info()) quit("Cannot initialize monsters");

	/* Initialize feature info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (vaults)");
	if (init_v_info()) quit("Cannot initialize vaults");

	/* Initialize history info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (histories)");
	if (init_h_info()) quit("Cannot initialize histories");

	/* Initialize race info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (races)");
	if (init_p_info()) quit("Cannot initialize races");

	/* Initialize class info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (classes)");
	if (init_c_info()) quit("Cannot initialize classes");

	/* Initialize owner info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (owners)");
	if (init_b_info()) quit("Cannot initialize owners");

	/* Initialize flavor info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (flavors)");
	if (init_flavor_info()) quit("Cannot initialize flavors");

	/* Initialize flavor info */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (quests)");
		if (init_q_info()) quit("Cannot initialize quests");

	/* Initialize some other arrays */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (other)");
	if (init_other()) quit("Cannot initialize other stuff");

	/* Initialize some other arrays */
	event_signal_string(EVENT_INITSTATUS, "Initializing arrays... (alloc)");
	if (init_alloc()) quit("Cannot initialize alloc stuff");

	/*** Load default user pref files ***/

	/* Initialize feature info */
	event_signal_string(EVENT_INITSTATUS, "Loading basic user pref file...");

	/* Process that file */
	(void)process_pref_file("pref.prf");

	/* Initialize feature info */
	event_signal_string(EVENT_INITSTATUS, "Initializing Random Artifact Tables...]");
	if (init_n_info()) quit("Cannot initialize random name generator list");

	/*Build the randart probability tables based on the standard Artifact Set*/
	build_randart_tables();

	/* Done */
	event_signal_string(EVENT_INITSTATUS, "Initialization complete");

	/* Sneakily init command list */
	cmd_init();

	/* Ask for a "command" until we get one we like. */
	while (1)
	{
		game_command command_req;

		cmd_get(CMD_INIT, &command_req, TRUE);

		if (command_req.command == CMD_QUIT)
		{
			quit(NULL);
		}
		else if (command_req.command == CMD_NEWGAME)
		{
			event_signal(EVENT_LEAVE_INIT);
			return TRUE;
		}
		else if (command_req.command == CMD_LOADFILE)
		{
			event_signal(EVENT_LEAVE_INIT);
			/* In future we might want to pass back or set the savefile
			   path here. */
			return FALSE;
		}
	}
}


void cleanup_angband(void)
{
	int i;

    delete_notes_file();

	/* Free the macros */
	macro_free();

	/* Free the macro triggers */
	macro_trigger_free();

	/* Free the allocation tables */
	FREE(alloc_ego_table);
	FREE(alloc_feat_table);
	FREE(alloc_race_table);
	FREE(alloc_kind_table);

	if (store)
	{
		/* Free the store inventories */
		for (i = 0; i < MAX_STORES; i++)
		{
			/* Get the store */
			store_type *st_ptr = &store[i];

			/* Free the store inventory */
			FREE(st_ptr->stock);
		}
	}

	/* Free the stores */
	FREE(store);

	event_remove_all_handlers();

	/* free the buttons */
	button_free();

	/* Free the player inventory */
	FREE(inventory);

	/*Clean the Autoinscribe*/
	autoinscribe_clean();

	/* Free the lore, monster, effects, and object lists */
	FREE(l_list);
	FREE(f_l_list);
	FREE(a_l_list);
	FREE(mon_list);
	FREE(o_list);
	FREE(x_list);

#ifdef MONSTER_SMELL

	/* Flow arrays */
	FREE(cave_when);

#endif /* MONSTER_SMELL */

	/* Free the cave */
	FREE(cave_o_idx);
	FREE(cave_m_idx);
	FREE(cave_x_idx);
	FREE(cave_feat);
	FREE(cave_info);

	/* Prepare monster movement array*/
	FREE(mon_moment_info);

	/* Free the "update_view()" array */
	FREE(view_g);

	/* Free the other "update_view()" array */
	FREE(fire_g);

	/* Free the temp array */
	FREE(temp_g);
	FREE(temp_y);
	FREE(temp_x);

	/* Free the dynamic features array */
	FREE(dyna_g);

	/* Free the stacked monster messages */
	FREE(mon_msg);
	FREE(mon_message_hist);

	/* Free the messages */
	messages_free();

	/* Free the "quarks" */
	quarks_free();

	/*free the randart arrays*/
	free_randart_tables();

	/* Free the info, name, and text arrays */
	free_info(&flavor_head);
	free_info(&g_head);
	free_info(&b_head);
	free_info(&c_head);
	free_info(&p_head);
	free_info(&h_head);
	free_info(&v_head);
	free_info(&r_head);
	free_info(&e_head);
	free_info(&a_head);
	free_info(&k_head);
	free_info(&t_head);
	free_info(&f_head);
	free_info(&z_head);
	free_info(&n_head);

	/* Free the format() buffer */
	vformat_kill();

	/* Free the directories */
	string_free(ANGBAND_DIR);
	string_free(ANGBAND_DIR_APEX);
	string_free(ANGBAND_DIR_BONE);
	string_free(ANGBAND_DIR_DATA);
	string_free(ANGBAND_DIR_EDIT);
	string_free(ANGBAND_DIR_FILE);
	string_free(ANGBAND_DIR_HELP);
	string_free(ANGBAND_DIR_INFO);
	string_free(ANGBAND_DIR_SAVE);
	string_free(ANGBAND_DIR_PREF);
	string_free(ANGBAND_DIR_USER);
	string_free(ANGBAND_DIR_XTRA);
}

