/* File: init2.c */

/*
 * Read files in "lib/data" and fill in various arrays.  Initialize and close
 * down the game.  Allocate and deallocate memory for variable-size global
 * arrays.
 *
 * Copyright (c) 2007 Ben Harrison
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"

#include "init.h"


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.  Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "lib/file" directory, from which quick-load binary "image" files
 * are constructed whenever they are not present in the "lib/data"
 * directory, or if those files become obsolete, if we are allowed.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass.  Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 *
 * The "init1.c" file is used only to parse the ascii template files,
 * to create the binary image files.  If you include the binary image
 * files instead of the ascii template files, then you can undefine
 * "ALLOW_TEMPLATES", saving about 20K by removing "init1.c".  Note
 * that the binary image files are extremely system dependant.
 */



/*
 * Find the default paths to all of our important sub-directories.
 *
 * The purpose of each sub-directory is described in "variable.c".
 *
 * All of the sub-directories should, by default, be located inside
 * the main "lib" directory, whose location is very system dependant.
 *
 * This function takes a writable buffer, initially containing the
 * "path" to the "lib" directory, for example, "/pkg/lib/angband/",
 * or a system dependant string, for example, ":lib:".  The buffer
 * must be large enough to contain at least 32 more characters.
 *
 * Various command line options may allow some of the important
 * directories to be changed to user-specified directories, most
 * importantly, the "info" and "user" and "save" directories,
 * but this is done after this function, see "main.c".
 *
 * In general, the initial path should end in the appropriate "PATH_SEP"
 * string.  All of the "sub-directory" paths (created below or supplied
 * by the user) will NOT end in the "PATH_SEP" string, see the special
 * "path_build()" function in "util.c" for more information.
 *
 * We support "fat binaries" by using a string variable as an
 * architecture-dependent suffix for the ANGBAND_DIR_DATA directory.
 * The user (or the main-xxx code, either will do) will have to create
 * this directory themselves.  Note that this variable, fat_data_suffix,
 * is in variables.c.
 *
 * Hack -- first we free all the strings, since this is known
 * to succeed even if the strings have not been allocated yet,
 * as long as the variables start out as "NULL".  This allows
 * this function to be called multiple times, for example, to
 * try several base "path" values until a good one is found.
 */
void init_file_paths(char *path)
{
	char *tail;

#ifdef PRIVATE_USER_PATH
	char buf[1024];
#endif /* PRIVATE_USER_PATH */

	/*** Free everything ***/

	/* Free the main path */
	(void)string_free(ANGBAND_DIR);

	/* Free the sub-paths */
	(void)string_free(ANGBAND_DIR_APEX);
	(void)string_free(ANGBAND_DIR_BONE);
	(void)string_free(ANGBAND_DIR_DATA);
	(void)string_free(ANGBAND_DIR_EDIT);
	(void)string_free(ANGBAND_DIR_FILE);
	(void)string_free(ANGBAND_DIR_HELP);
	(void)string_free(ANGBAND_DIR_INFO);
	(void)string_free(ANGBAND_DIR_SAVE);
	(void)string_free(ANGBAND_DIR_PREF);
	(void)string_free(ANGBAND_DIR_USER);
	(void)string_free(ANGBAND_DIR_XTRA);


	/*** Prepare the "path" ***/

	/* Hack -- save the main directory */
	ANGBAND_DIR = string_make(path);

	/* Prepare to append to the Base Path */
	tail = path + strlen(path);


	/*** Build the sub-directory names ***/

	/* Build a path name */
	strcpy(tail, "edit");
	ANGBAND_DIR_EDIT = string_make(path);

	/* Build a path name */
	strcpy(tail, "file");
	ANGBAND_DIR_FILE = string_make(path);

	/* Build a path name */
	strcpy(tail, "help");
	ANGBAND_DIR_HELP = string_make(path);

	/* Build a path name */
	strcpy(tail, "info");
	ANGBAND_DIR_INFO = string_make(path);

	/* Build a path name */
	strcpy(tail, "pref");
	ANGBAND_DIR_PREF = string_make(path);

#ifdef PRIVATE_USER_PATH

	/* Build the path to the user specific directory */
	(void)path_build(buf, sizeof(buf), PRIVATE_USER_PATH, VERSION_NAME);

	/* Build a relative path name */
	ANGBAND_DIR_USER = string_make(buf);

#else /* PRIVATE_USER_PATH */

	/* Build a path name */
	strcpy(tail, "user");
	ANGBAND_DIR_USER = string_make(path);

#endif /* PRIVATE_USER_PATH */

#ifdef USE_PRIVATE_PATHS

	/* Build the path to the user specific sub-directory */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "scores");

	/* Build a relative path name */
	ANGBAND_DIR_APEX = string_make(buf);

	/* Build the path to the user specific sub-directory */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "bone");

	/* Build a relative path name */
	ANGBAND_DIR_BONE = string_make(buf);

	/* Build the path to the user specific sub-directory */
	if (fat_data_suffix)
	{
		/* Support fat binaries */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER,
		           format("data-%s", fat_data_suffix));
	}
	else
	{
		/* Just build a "simple" path otherwise */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "data");
	}

	/* Build a relative path name */
	ANGBAND_DIR_DATA = string_make(buf);

	/* Build the path to the user specific sub-directory */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "save");

	/* Build a relative path name */
	ANGBAND_DIR_SAVE = string_make(buf);

#else /* USE_PRIVATE_PATHS */

	/* Build a path name */
	strcpy(tail, "apex");
	ANGBAND_DIR_APEX = string_make(path);

	/* Build a path name */
	strcpy(tail, "bone");
	ANGBAND_DIR_BONE = string_make(path);

	/* Build a path name - support fat binaries */
	if (fat_data_suffix)
		(void)strnfmt(tail, sizeof(tail), "data-%s", fat_data_suffix);
	else
		strcpy(tail, "data");
	ANGBAND_DIR_DATA = string_make(path);

	/* Build a path name */
	strcpy(tail, "save");
	ANGBAND_DIR_SAVE = string_make(path);

#endif /* USE_PRIVATE_PATHS */

	/* Build a path name */
	strcpy(tail, "xtra");
	ANGBAND_DIR_XTRA = string_make(path);
}

#ifdef PRIVATE_USER_PATH

/*
 * Create an ".angband/" directory in the users home directory.
 *
 * ToDo: Add error handling.
 * ToDo: Only create the directories when actually writing files.
 */
void create_user_dirs(void)
{
	char dirpath[1024];
	char subdirpath[1024];


	/* Get an absolute path from the filename */
	path_parse(dirpath, sizeof(dirpath), PRIVATE_USER_PATH);

	/* Create the ~/.angband/ directory */
	mkdir(dirpath, 0700);

	/* Build the path to the variant-specific sub-directory */
	(void)path_build(subdirpath, sizeof(subdirpath), dirpath, VERSION_NAME);

	/* Create the directory */
	mkdir(subdirpath, 0700);

#ifdef USE_PRIVATE_PATHS
	/* Build the path to the scores sub-directory */
	(void)path_build(dirpath, sizeof(dirpath), subdirpath, "scores");

	/* Create the directory */
	mkdir(dirpath, 0700);

	/* Build the path to the savefile sub-directory */
	(void)path_build(dirpath, sizeof(dirpath), subdirpath, "bone");

	/* Create the directory */
	mkdir(dirpath, 0700);

	/* Build the path to the savefile sub-directory */
	(void)path_build(dirpath, sizeof(dirpath), subdirpath, "data");

	/* Create the directory */
	mkdir(dirpath, 0700);

	/* Build the path to the savefile sub-directory */
	(void)path_build(dirpath, sizeof(dirpath), subdirpath, "save");

	/* Create the directory */
	mkdir(dirpath, 0700);
#endif /* USE_PRIVATE_PATHS */
}

#endif /* PRIVATE_USER_PATH */



#ifdef ALLOW_TEMPLATES


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
	"obsolete file",
	"missing record header",
	"non-sequential records",
	"invalid flag specification",
	"undefined directive",
	"out of memory",
	"value out of bounds",
	"too few arguments",
	"too many arguments",
	"too many allocation entries",
	"invalid spell frequency",
	"the index number exceeds the limit set in limits.txt",
	"more than one quest per level",
	"too many entries",
	"vault too big",
	"undescribed",
	"undescribed",
	"flags being defined for a pval 1 of zero value",
	"flags being defined for a pval 2 of zero value",
	"flags being defined for a pval 3 of zero value",
	"need to know how many essences are being used",
	"more than four different kinds of essences for the same object",
	"unknown array",
	"no array specified",
	"invalid store (out of bounds)",
	"Color index is not recognized by the game",
	"feature verification failed"
};


#endif /* ALLOW_TEMPLATES */


/*
 * *.txt file headers
 */
header z_head;
header v_head;
header f_head;
header k_head;
header a_head;
header e_head;
header r_head;
header b_head;
header q_head;
header flavor_head;


/*** Initialize from binary image files ***/


/*
 * Initialize a "*_info" array, by parsing a binary "image" file
 */
static errr init_info_raw(int fd, header *head)
{
	header test;


	/* Read and verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major   != head->v_major)   ||
	    (test.v_minor   != head->v_minor)   ||
	    (test.v_patch   != head->v_patch)   ||
	    (test.v_extra   != head->v_extra)   ||
	    (test.info_num  != head->info_num)  ||
	    (test.info_len  != head->info_len)  ||
	    (test.head_size != head->head_size) ||
	    (test.info_size != head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	COPY(head, &test, header);


	/* Allocate the "*_info" array */
	C_MAKE(head->info_ptr, head->info_size, char);

	/* Read the "*_info" array */
	(void)fd_read(fd, (char*) head->info_ptr, head->info_size);

	if (head->name_size)
	{
		/* Allocate the "*_name" array */
		C_MAKE(head->name_ptr, head->name_size, char);

		/* Read the "*_name" array */
		(void)fd_read(fd, head->name_ptr, head->name_size);
	}

	if (head->text_size)
	{
		/* Allocate the "*_text" array */
		C_MAKE(head->text_ptr, head->text_size, char);

		/* Read the "*_text" array */
		(void)fd_read(fd, head->text_ptr, head->text_size);
	}

	/* Success */
	return (0);
}


/*
 * Initialize the header of an *.raw file.
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

#ifdef ALLOW_TEMPLATES

/*
 * Display a parse error message.
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
#endif /* ALLOW_TEMPLATES */


/*
 * Initialize a "*_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_info(cptr filename, header *head)
{
	int fd;

	errr err = 1;

	FILE *fp;

	/* General buffer */
	char buf[1024];


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, format("%s.raw", filename));

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, format("%s.txt", filename));

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
			err = init_info_raw(fd, head);

		/* Close it */
		(void)fd_close(fd);
	}

	/* Do we have to parse the *.txt file? */
	if (err)
	{
		/*** Make the fake arrays ***/

		/* Allocate the "*_info" array */
		C_MAKE(head->info_ptr, head->info_size, char);

		/* MegaHack -- make "fake" arrays */
		if (z_info)
		{
			C_MAKE(head->name_ptr, z_info->fake_name_size, char);
			C_MAKE(head->text_ptr, z_info->fake_text_size, char);
		}


		/*** Load the ascii template file ***/

		/* Build the filename */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_EDIT, format("%s.txt", filename));

		/* Open the file */
		fp = my_fopen(buf, "r");

		/* Parse it */
		if (!fp) quit(format("Cannot open '%s.txt' file.", filename));

		/* Parse the file */
		err = init_info_txt(fp, buf, head, head->parse_info_txt);

		/* Close it */
		(void)my_fclose(fp);

		/* Errors */
		if (err) display_parse_error(filename, err, buf);


		/*** Dump the binary image file ***/

		/* File type is "DATA" */
		FILE_TYPE(FILE_TYPE_DATA);

		/* Build the filename */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, format("%s.raw", filename));


		/* Attempt to open the file */
		fd = fd_open(buf, O_RDONLY);

		/* Failure */
		if (fd < 0)
		{
			int mode = 0644;

			/* Grab permissions */
			safe_setuid_grab();

			/* Create a new file */
			fd = fd_make(buf, mode);

			/* Drop permissions */
			safe_setuid_drop();

			/* Failure */
			if (fd < 0)
			{
				char why[1024];

				/* Message */
				(void)strnfmt(why, sizeof(why), "Cannot create the '%s' file!", buf);

				/* Crash and burn */
				quit(why);
			}
		}

		/* Close it */
		(void)fd_close(fd);

		/* Grab permissions */
		safe_setuid_grab();

		/* Attempt to create the raw file */
		fd = fd_open(buf, O_WRONLY);

		/* Drop permissions */
		safe_setuid_drop();

		/* Dump to the file */
		if (fd >= 0)
		{
			/* Dump it */
			(void)fd_write(fd, (cptr)head, head->head_size);

			/* Dump the "*_info" array */
			(void)fd_write(fd, (char*) head->info_ptr, head->info_size);

			/* Dump the "*_name" array */
			(void)fd_write(fd, head->name_ptr, head->name_size);

			/* Dump the "*_text" array */
			(void)fd_write(fd, head->text_ptr, head->text_size);

			/* Close */
			(void)fd_close(fd);
		}


		/*** Kill the fake arrays ***/

		/* Free the "*_info" array */
		KILL(head->info_ptr, char);

		/* MegaHack -- Free the "fake" arrays */
		if (z_info)
		{
			KILL(head->name_ptr, char);
			KILL(head->text_ptr, char);
		}

#endif /* ALLOW_TEMPLATES */


		/*** Load the binary image file ***/

		/* Build the filename */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, format("%s.raw", filename));

		/* Attempt to open the "raw" file */
		fd = fd_open(buf, O_RDONLY);

		/* Process existing "raw" file */
		if (fd < 0) quit(format("Cannot load '%s.raw' file.", filename));

		/* Attempt to parse the "raw" file */
		err = init_info_raw(fd, head);

		/* Close it */
		(void)fd_close(fd);

		/* Error */
		if (err) quit(format("Cannot parse '%s.raw' file.", filename));

#ifdef ALLOW_TEMPLATES
	}
#endif /* ALLOW_TEMPLATES */

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	z_head.parse_info_txt = parse_z_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("limits", &z_head);

	/* Set the global variables */
	z_info = (maxima*) z_head.info_ptr;

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	f_head.parse_info_txt = parse_f_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("terrain", &f_head);

	/* Set the global variables */
	f_info = (feature_type*) f_head.info_ptr;
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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	k_head.parse_info_txt = parse_k_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("object", &k_head);

	/* Set the global variables */
	k_info = (object_kind*) k_head.info_ptr;
	k_name = k_head.name_ptr;
	k_text = k_head.text_ptr;

	/* Hack -- assign "easy know" flags XXX XXX */
	if (!err) easy_know_init();

	return (err);
}



/*
 * Initialize the "a_info" array
 */
static errr init_a_info(void)
{
	errr err;

	/* Init the header */
	init_header(&a_head, z_info->a_max, sizeof(artifact_type));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	a_head.parse_info_txt = parse_a_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("artifact", &a_head);

	/* Set the global variables */
	a_info = (artifact_type*) a_head.info_ptr;
	a_name = a_head.name_ptr;
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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	e_head.parse_info_txt = parse_e_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("ego_item", &e_head);

	/* Set the global variables */
	e_info = (ego_item_type*) e_head.info_ptr;
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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	r_head.parse_info_txt = parse_r_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("monster", &r_head);

	/* Set the global variables */
	r_info = (monster_race*) r_head.info_ptr;
	r_name = r_head.name_ptr;
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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	v_head.parse_info_txt = parse_v_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("vault", &v_head);

	/* Set the global variables */
	v_info = (vault_type*) v_head.info_ptr;
	v_name = v_head.name_ptr;
	v_text = v_head.text_ptr;

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	q_head.parse_info_txt = parse_q_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("quest", &q_head);

	/* Set the global variables */
	q_info = (quest_type*) q_head.info_ptr;
	q_name = q_head.name_ptr;
	q_text = q_head.text_ptr;

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	flavor_head.parse_info_txt = parse_flavor_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("flavor", &flavor_head);

	/* Set the global variables */
	flavor_info = (flavor_type*) flavor_head.info_ptr;
	flavor_name = flavor_head.name_ptr;
	flavor_text = flavor_head.text_ptr;

	return (err);
}


/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
	int i, j, k, n;


	/*** Prepare the various "bizarre" arrays ***/

	/* Initialize the "macro" package */
	(void)macro_init();

	/* Initialize the "quark" package */
	(void)quarks_init();

	/* Initialize the "message" package */
	(void)messages_init();

	/*** Prepare grid arrays ***/

	/* Array of grids (view) */
	C_MAKE(view_g, VIEW_MAX, u16b);

	/* Array of grids (many temporary purposes) */
	C_MAKE(temp_g, TEMP_MAX, u16b);

	/* Array of grids (glowing light sources) */
	C_MAKE(lite_g, LITE_MAX, u16b);

	/* Hack -- use some memory twice */
	temp_y = ((byte*)(temp_g)) + 0;
	temp_x = ((byte*)(temp_g)) + TEMP_MAX;

	/* Array of effect grids */
	C_MAKE(effect_grid, EFFECT_GRID_MAX, effect_grid_type);


	/*** Prepare dungeon arrays ***/

	/* Padded into array */
	C_MAKE(cave_info, DUNGEON_HGT_MAX, u16b_256);

	/* Feature array */
	C_MAKE(cave_feat, DUNGEON_HGT_MAX, byte_wid);

	/* Entity arrays */
	C_MAKE(cave_o_idx, DUNGEON_HGT_MAX, s16b_wid);
	C_MAKE(cave_m_idx, DUNGEON_HGT_MAX, s16b_wid);

	/* Flow arrays */
	C_MAKE(cave_cost, DUNGEON_HGT_MAX, byte_wid);
	C_MAKE(cave_when, DUNGEON_HGT_MAX, byte_wid);

	/*** Prepare "vinfo" array ***/

	/* Used by "update_view()" */
	(void)vinfo_init();


	/*** Prepare entity arrays ***/

	/* Objects */
	C_MAKE(o_list, z_info->o_max, object_type);

	/* Monsters */
	C_MAKE(m_list, z_info->m_max, monster_type);

	/* Effects */
	C_MAKE(x_list, z_info->x_max, effect_type);

	/* Traps */
	C_MAKE(t_list, z_info->t_max, trap_type);


	/*** Prepare lore array ***/

	/* Lore */
	C_MAKE(l_list, z_info->r_max, monster_lore);


	/*** Prepare the inventory ***/

	/* Allocate it */
	C_MAKE(inventory, INVEN_TOTAL, object_type);


	/* *** Initialize inventory colors to white *** */
	for (i = 0; i < 128; i++)
	{
		/* Default to white */
		tval_to_attr[i] = TERM_WHITE;
	}


	/*** Prepare the stores ***/

	/* Allocate the stores */
	C_MAKE(store, MAX_STORES, store_type);

	/* Read the "store.txt" file (if present), note errors */
	i = parse_store();
	if (i) display_parse_error("store", i, NULL);


	/* Fill in each store */
	for (i = 0; i < MAX_STORES; i++)
	{
		/* Get the store */
		store_type *st_ptr = &store[i];

		/* Assume full stock */
		st_ptr->stock_size = STORE_INVEN_MAX;

		/* Allocate the stock */
		C_MAKE(st_ptr->stock, st_ptr->stock_size, object_type);

		/* Assume no stock */
		st_ptr->stock_start = -1;
		st_ptr->stock_end   = -1;

		/* Scan the stock array */
		for (k = 0; k < STORE_STOCK_SIZE; k++)
		{
			/* Note an array marker */
			if (store_stock[k].k_idx == -1)
			{
				/* Start of this store's list of items */
				if (store_stock[k].prob == i)
				{
					/* List starts immediately after the marker */
					st_ptr->stock_start = k + 1;
				}

				/* Any other marker */
				else
				{
					/* Store has a stock list */
					if (st_ptr->stock_start >= 0)
					{
						/* Stock list ends immediately before the marker */
						st_ptr->stock_end = k - 1;

						/* We're done */
						break;
					}
				}
			}
		}
	}


	/*** Prepare the options ***/

	/* Initialize the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		/* Default value */
		op_ptr->opt[i] = option_norm[i];
	}

	/* Create an array storing the left panel custom displays */
	C_MAKE(custom_display, CUSTOM_DISPLAY_ROWS, byte);


	/* Initialize the window flags */
	for (n = 0; n < TERM_MAX; n++)
	{
		/* Assume no flags */
		op_ptr->window_flag[n] = 0L;
	}

	/*** Pre-allocate space for the "format()" buffer ***/

	/* Hack -- Just call the "format()" function */
	(void)format("%s (%s).", MAINTAINER_NAME, MAINTAINER);


	/* Mark artifacts belonging to sets */
	if (TRUE)
	{
		/* Scan all the sets */
		for (i = 0; i < MAX_S_IDX; i++)
		{
			/* Get this set */
			set_type *s_ptr = &s_info[i];

			/* Scan each item in this set */
			for (j = 0; j < s_ptr->no_of_items; j++)
			{
				/* Point to the correct artifact */
				artifact_type *a_ptr = &a_info[s_ptr->set_items[j].a_idx];

				/* Hack -- mark this artifact */
				a_ptr->set_index = i;
			}
		}
	}

	/* Cancel any special cursor visibility */
	for (i = 0; i < TERM_MAX; i++) inkey_cursor_hack[i] = 0;


	/* Success */
	return (0);
}



/*
 * Initialize some other arrays
 */
static errr init_alloc(void)
{
	int i;

	monster_race *r_ptr;

	ego_item_type *e_ptr;

	quest_type *q_ptr;

	alloc_entry *table;

	s16b num[MAX_DEPTH];

	s16b aux[MAX_DEPTH];


	/*** Initialize object allocation info ***/

	/* Allocate the permit_kind_table */
	C_MAKE(permit_kind_table, z_info->k_max, bool);

	/* Allocate the chance_kind_table */
	C_MAKE(chance_kind_table, z_info->k_max, byte);

	/* Allow all legal objects */
	for (i = 0; i < z_info->k_max; i++) permit_kind_table[i] = TRUE;

	/*** Analyze monster allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(num, MAX_DEPTH, s16b);

	/* Size of "alloc_race_table" */
	alloc_race_size = 0;

	/* Scan the monsters */
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
	for (i = 1; i < MAX_DEPTH; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/* Paranoia */
	if (!num[0]) quit("No town monsters!");


	/*** Initialize monster allocation info ***/

	/* Allocate the alloc_race_table */
	C_MAKE(alloc_race_table, alloc_race_size, alloc_entry);

	/* Get the table entry */
	table = alloc_race_table;

	/* Scan the monsters */
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
	(void)C_WIPE(aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(num, MAX_DEPTH, s16b);

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
	for (i = 1; i < MAX_DEPTH; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/*** Initialize ego-item allocation info ***/

	/* Allocate the alloc_ego_table */
	C_MAKE(alloc_ego_table, alloc_ego_size, alloc_entry);

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

	/*** Initialize the array of movement moments ***/
	C_MAKE(move_moment, z_info->m_max, move_moment_type);

	/*** Initialize the array of spell graphics ***/
	C_MAKE(proj_graphics, 256, proj_graphics_type);


	/*** Initialize quest monsters ***/

	/* Scan the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		/* Get the i'th quest */
		q_ptr = &q_info[i];

		/* Skip non-quests */
		if (q_ptr->active_level)
		{
			/* Get the quest monster */
			r_ptr = &r_info[q_ptr->r_idx];
		}
	}

	/* Success */
	return (0);
}


/*
 * Hack -- take notes on last line
 */
static void init_note(cptr str)
{
	put_str(format("%-60s", str), Term->rows - 1, (Term->cols - 40) / 2);
	(void)Term_fresh();
}



/*
 * Hack -- Explain a broken "lib" folder and quit (see below).
 */
static void init_angband_aux(cptr why)
{
	quit_fmt("%s\n\n%s", why,
	         "The 'lib' directory is probably missing or broken.\n"
	         "Perhaps the archive was not extracted correctly.\n"
	         "See the 'readme.txt' file for more information.");
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
 */
void init_angband(void)
{
	int fd;

	int mode = 0644;

	char buf[1024];


	/*** Verify the "news" file ***/

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "news.txt");

	/* Attempt to open the file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure (use as a quick test of file system integrity */
	if (fd < 0)
	{
		char why[1024];

		/* Message */
		(void)strnfmt(why, sizeof(why), "Cannot access the '%s' file!", buf);

		/* Crash and burn */
		init_angband_aux(why);
	}

	/* Close it */
	(void)fd_close(fd);


	/* Process color definitions */
	(void)process_pref_file("color.prf");

	/* Hack -- React to color changes */
	(void)Term_xtra(TERM_XTRA_REACT, 0);


	/*** Display the "news" file ***/

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "news.txt");

	/* Display the file */
	(void)display_file(buf);



	/*** Verify (or create) the "high score" file ***/

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Attempt to open the high score file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure */
	if (fd < 0)
	{
		/* File type is "DATA" */
		FILE_TYPE(FILE_TYPE_DATA);

		/* Grab permissions */
		safe_setuid_grab();

		/* Create a new high score file */
		fd = fd_make(buf, mode);

		/* Drop permissions */
		safe_setuid_drop();

		/* Failure */
		if (fd < 0)
		{
			char why[1024];

			/* Message */
			(void)strnfmt(why, sizeof(why), "Cannot create the '%s' file!", buf);

			/* Crash and burn */
			init_angband_aux(why);
		}
	}

	/* Close it */
	(void)fd_close(fd);


	/*** Initialize some arrays ***/

	/* Initialize size info */
	init_note("[Initializing array sizes...]");
	if (init_z_info()) quit("Cannot initialize sizes");

	/* Initialize feature info */
	init_note("[Initializing arrays... (features)]");
	if (init_f_info()) quit("Cannot initialize features");

	/* Initialize object info */
	init_note("[Initializing arrays... (objects)]");
	if (init_k_info()) quit("Cannot initialize objects");

	/* Initialize artifact info */
	init_note("[Initializing arrays... (artifacts)]");
	if (init_a_info()) quit("Cannot initialize artifacts");

	/* Initialize ego-item info */
	init_note("[Initializing arrays... (ego-items)]");
	if (init_e_info()) quit("Cannot initialize ego-items");

	/* Initialize monster info */
	init_note("[Initializing arrays... (monsters)]");
	if (init_r_info()) quit("Cannot initialize monsters");

	/* Initialize feature info */
	init_note("[Initializing arrays... (vaults)]");
	if (init_v_info()) quit("Cannot initialize vaults");

	/* Initialize quest info */
	init_note("[Initializing arrays... (quests)]");
	if (init_q_info()) quit("Cannot initialize quests");

	/* Initialize flavor info */
	init_note("[Initializing arrays... (flavors)]");
	if (init_flavor_info()) quit("Cannot initialize flavors");

	/* Initialize some other arrays */
	init_note("[Initializing arrays... (other)]");
	if (init_other()) quit("Cannot initialize other stuff");

	/* Initialize some other arrays */
	init_note("[Initializing arrays... (alloc)]");
	if (init_alloc()) quit("Cannot initialize alloc stuff");



	/*** Load default user pref files ***/

	/* Process the basic pref file */
	init_note("[Loading basic user pref file...]");
	(void)process_pref_file("pref.prf");

	/* Done */
	init_note("[Initialization complete]");
}


/*
 * Free various arrays and structures.
 */
void cleanup_angband(void)
{
	int i;

	/* Free the macros and keymaps */
	(void)macro_free();

	/* Free the allocation tables */
	FREE(alloc_ego_table);
	FREE(alloc_race_table);
	FREE(permit_kind_table);
	FREE(chance_kind_table);

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

	/* Free the player inventory */
	FREE(inventory);

	/* Free the left panel custom display array */
	FREE(custom_display);

	/* Free the lore, monster, and object lists */
	FREE(l_list);
	FREE(m_list);
	FREE(o_list);

	/* Free the effects and the traps */
	FREE(x_list);
	FREE(t_list);

	/* Free the movement moments */
	FREE(move_moment);

	/* Free the projection graphics */
	FREE(proj_graphics);

	/* Flow arrays */
	FREE(cave_when);
	FREE(cave_cost);

	/* Free the cave */
	FREE(cave_o_idx);
	FREE(cave_m_idx);
	FREE(cave_feat);
	FREE(cave_info);

	/* Free the "update_view()" array */
	FREE(view_g);

	/* Free the temp array */
	FREE(temp_g);

	/* Free the effect grids array */
	FREE(effect_grid);

	/* Free the messages */
	messages_free();

	/* Free the "quarks" */
	(void)quarks_free();

	/* Free the info, name, and text arrays */
	(void)free_info(&q_head);
	(void)free_info(&b_head);
	(void)free_info(&v_head);
	(void)free_info(&r_head);
	(void)free_info(&e_head);
	(void)free_info(&a_head);
	(void)free_info(&k_head);
	(void)free_info(&f_head);
	(void)free_info(&z_head);

	/* Free the format() buffer */
	vformat_kill();

	/* Free the directories */
	(void)string_free(ANGBAND_DIR);
	(void)string_free(ANGBAND_DIR_APEX);
	(void)string_free(ANGBAND_DIR_BONE);
	(void)string_free(ANGBAND_DIR_DATA);
	(void)string_free(ANGBAND_DIR_EDIT);
	(void)string_free(ANGBAND_DIR_FILE);
	(void)string_free(ANGBAND_DIR_HELP);
	(void)string_free(ANGBAND_DIR_INFO);
	(void)string_free(ANGBAND_DIR_SAVE);
	(void)string_free(ANGBAND_DIR_PREF);
	(void)string_free(ANGBAND_DIR_USER);
	(void)string_free(ANGBAND_DIR_XTRA);
}

