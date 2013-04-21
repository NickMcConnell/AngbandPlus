/* File: init2.c */


/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
 * Mega-Hack -- support fat raw files under NEXTSTEP, using special
 * "suffixed" directories for the "ANGBAND_DIR_DATA" directory, but
 * requiring the directories to be created by hand by the user.
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
	string_free(ANGBAND_DIR);

	/* Free the sub-paths */
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


	/*** Prepare the "path" ***/

	/* Hack -- save the main directory */
	ANGBAND_DIR = string_make(path);

	/* Prepare to append to the Base Path */
	tail = path + strlen(path);


#ifdef VM


	/*** Use "flat" paths with VM/ESA ***/

	/* Use "blank" path names */
	ANGBAND_DIR_APEX = string_make("");
	ANGBAND_DIR_BONE = string_make("");
	ANGBAND_DIR_DATA = string_make("");
	ANGBAND_DIR_EDIT = string_make("");
	ANGBAND_DIR_FILE = string_make("");
	ANGBAND_DIR_HELP = string_make("");
	ANGBAND_DIR_INFO = string_make("");
	ANGBAND_DIR_SAVE = string_make("");
	ANGBAND_DIR_PREF = string_make("");
	ANGBAND_DIR_USER = string_make("");
	ANGBAND_DIR_XTRA = string_make("");


#else /* VM */


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
	path_build(buf, sizeof(buf), PRIVATE_USER_PATH, VERSION_NAME);

	/* Build a relative path name */
	ANGBAND_DIR_USER = string_make(buf);

#else /* PRIVATE_USER_PATH */

	/* Build a path name */
	strcpy(tail, "user");
	ANGBAND_DIR_USER = string_make(path);

#endif /* PRIVATE_USER_PATH */

#ifdef USE_PRIVATE_PATHS

	/* Build the path to the user specific sub-directory */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "scores");

	/* Build a relative path name */
	ANGBAND_DIR_APEX = string_make(buf);

	/* Build the path to the user specific sub-directory */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "bone");

	/* Build a relative path name */
	ANGBAND_DIR_BONE = string_make(buf);

	/* Build the path to the user specific sub-directory */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "data");

	/* Build a relative path name */
	ANGBAND_DIR_DATA = string_make(buf);

	/* Build the path to the user specific sub-directory */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "save");

	/* Build a relative path name */
	ANGBAND_DIR_SAVE = string_make(buf);

#else /* USE_PRIVATE_PATHS */

	/* Build a path name */
	strcpy(tail, "apex");
	ANGBAND_DIR_APEX = string_make(path);

	/* Build a path name */
	strcpy(tail, "bone");
	ANGBAND_DIR_BONE = string_make(path);

	/* Build a path name */
	strcpy(tail, "data");
	ANGBAND_DIR_DATA = string_make(path);

	/* Build a path name */
	strcpy(tail, "save");
	ANGBAND_DIR_SAVE = string_make(path);

#endif /* USE_PRIVATE_PATHS */

	/* Build a path name */
	strcpy(tail, "xtra");
	ANGBAND_DIR_XTRA = string_make(path);



#endif /* VM */


#ifdef NeXT

	/* Allow "fat binary" usage with NeXT */
	if (TRUE)
	{
		cptr next = NULL;

# if defined(m68k)
		next = "m68k";
# endif

# if defined(i386)
		next = "i386";
# endif

# if defined(sparc)
		next = "sparc";
# endif

# if defined(hppa)
		next = "hppa";
# endif

		/* Use special directory */
		if (next)
		{
			/* Forget the old path name */
			string_free(ANGBAND_DIR_DATA);

			/* Build a new path name */
			sprintf(tail, "data-%s", next);
			ANGBAND_DIR_DATA = string_make(path);
		}
	}

#endif /* NeXT */

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
	path_build(subdirpath, sizeof(subdirpath), dirpath, VERSION_NAME);

	/* Create the directory */
	mkdir(subdirpath, 0700);

#ifdef USE_PRIVATE_PATHS
	/* Build the path to the scores sub-directory */
	path_build(dirpath, sizeof(dirpath), subdirpath, "scores");

	/* Create the directory */
	mkdir(dirpath, 0700);

	/* Build the path to the savefile sub-directory */
	path_build(dirpath, sizeof(dirpath), subdirpath, "bone");

	/* Create the directory */
	mkdir(dirpath, 0700);

	/* Build the path to the savefile sub-directory */
	path_build(dirpath, sizeof(dirpath), subdirpath, "data");

	/* Create the directory */
	mkdir(dirpath, 0700);

	/* Build the path to the savefile sub-directory */
	path_build(dirpath, sizeof(dirpath), subdirpath, "save");

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
	"invalid number of items (0-99)",
	"too many entries",
	"vault too big",
	"non-sequential quest levels",
};


#endif /* ALLOW_TEMPLATES */


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


/*** Initialize from binary image files ***/


/*
 * Initialize a "*_info" array, by parsing a binary "image" file
 */
static errr init_info_raw(int fd, header *head)
{
	header test;


	/* Read and verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != head->v_major) ||
	    (test.v_minor != head->v_minor) ||
	    (test.v_patch != head->v_patch) ||
	    (test.v_extra != head->v_extra) ||
	    (test.info_num != head->info_num) ||
	    (test.info_len != head->info_len) ||
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
	fd_read(fd, head->info_ptr, head->info_size);

	if (head->name_size)
	{
		/* Allocate the "*_name" array */
		C_MAKE(head->name_ptr, head->name_size, char);

		/* Read the "*_name" array */
		fd_read(fd, head->name_ptr, head->name_size);
	}

	if (head->text_size)
	{
		/* Allocate the "*_text" array */
		C_MAKE(head->text_ptr, head->text_size, char);

		/* Read the "*_text" array */
		fd_read(fd, head->text_ptr, head->text_size);
	}

	/* Success */
	return (0);
}


/*
 * Initialize the header of an *_info.raw file.
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
	path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, format("%s.raw", filename));

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
		fd_close(fd);
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
		path_build(buf, sizeof(buf), ANGBAND_DIR_EDIT, format("%s.txt", filename));

		/* Open the file */
		fp = my_fopen(buf, "r");

		/* Parse it */
		if (!fp) quit(format("Cannot open '%s.txt' file.", filename));

		/* Parse the file */
		err = init_info_txt(fp, buf, head, head->parse_info_txt);

		/* Close it */
		my_fclose(fp);

		/* Errors */
		if (err) display_parse_error(filename, err, buf);


		/*** Dump the binary image file ***/

		/* File type is "DATA" */
		FILE_TYPE(FILE_TYPE_DATA);

		/* Build the filename */
		path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, format("%s.raw", filename));


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

				/* Complain */
				plog_fmt("Cannot create the '%s' file!", buf);

				/* Continue */
				return (0);
			}
		}

		/* Close it */
		fd_close(fd);

		/* Grab permissions */
		safe_setuid_grab();

		/* Attempt to create the raw file */
		fd = fd_open(buf, O_WRONLY);

		/* Drop permissions */
		safe_setuid_drop();

		/* Failure */
		if (fd < 0)
		{
			/* Complain */
			plog_fmt("Cannot write the '%s' file!", buf);

			/* Continue */
			return (0);
		}

		/* Dump to the file */
		if (fd >= 0)
		{
			/* Dump it */
			fd_write(fd, (cptr)head, head->head_size);

			/* Dump the "*_info" array */
			if (head->info_size > 0) fd_write(fd, head->info_ptr, head->info_size);

			/* Dump the "*_name" array */
			if (head->name_size > 0) fd_write(fd, head->name_ptr, head->name_size);

			/* Dump the "*_text" array */
			if (head->text_size > 0) fd_write(fd, head->text_ptr, head->text_size);

			/* Close */
			fd_close(fd);
		}


		/*** Kill the fake arrays ***/

		/* Free the "*_info" array */
		KILL(head->info_ptr);

		/* MegaHack -- Free the "fake" arrays */
		if (z_info)
		{
			KILL(head->name_ptr);
			KILL(head->text_ptr);
		}

#endif /* ALLOW_TEMPLATES */


		/*** Load the binary image file ***/

		/* Build the filename */
		path_build(buf, sizeof(buf), ANGBAND_DIR_DATA, format("%s.raw", filename));

		/* Attempt to open the "raw" file */
		fd = fd_open(buf, O_RDONLY);

		/* Process existing "raw" file */
		if (fd < 0) quit(format("Cannot load '%s.raw' file.", filename));

		/* Attempt to parse the "raw" file */
		err = init_info_raw(fd, head);

		/* Close it */
		fd_close(fd);

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	f_head.parse_info_txt = parse_f_info;

#endif /* ALLOW_TEMPLATES */

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	k_head.parse_info_txt = parse_k_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("object", &k_head);

	/* Set the global variables */
	k_info = k_head.info_ptr;
	k_name = k_head.name_ptr;
	k_text = k_head.text_ptr;

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	a_head.parse_info_txt = parse_a_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("artifact", &a_head);

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	e_head.parse_info_txt = parse_e_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("ego_item", &e_head);

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	r_head.parse_info_txt = parse_r_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("monster", &r_head);

	/* Set the global variables */
	r_info = r_head.info_ptr;
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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	p_head.parse_info_txt = parse_p_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("p_race", &p_head);

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	c_head.parse_info_txt = parse_c_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("p_class", &c_head);

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	h_head.parse_info_txt = parse_h_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("p_hist", &h_head);

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	b_head.parse_info_txt = parse_b_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("shop_own", &b_head);

	/* Set the global variables */
	b_info = b_head.info_ptr;
	b_name = b_head.name_ptr;
	b_text = b_head.text_ptr;

	return (err);
}



/*
 * Initialize the "g_info" array
 */
static errr init_g_info(void)
{
	errr err;

	/* Init the header */
	init_header(&g_head, (u16b)(z_info->p_max * z_info->p_max), sizeof(byte));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	g_head.parse_info_txt = parse_g_info;

#endif /* ALLOW_TEMPLATES */

	err = init_info("cost_adj", &g_head);

	/* Set the global variables */
	g_info = g_head.info_ptr;
	g_name = g_head.name_ptr;
	g_text = g_head.text_ptr;

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

#ifdef ALLOW_TEMPLATES

  /* Save a pointer to the parsing function */
  n_head.parse_info_txt = parse_n_info;

#endif /* ALLOW_TEMPLATES */

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

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	flavor_head.parse_info_txt = parse_flavor_info;

#endif /* ALLOW_TEMPLATES */

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

void autoinscribe_init(void)
{
	/* Paranoia */
	autoinscribe_clean();

	C_MAKE(inscriptions, AUTOINSCRIPTIONS_MAX, autoinscription);
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

	/* Initialize autoinscriptions */
	(void)autoinscribe_init();

	/* Initialize the "message" package */
	(void)messages_init();

	/*** Prepare grid arrays ***/

	/* Array of grids */
	C_MAKE(view_g, VIEW_MAX, u16b);

	/* Array of grids */
	C_MAKE(fire_g, VIEW_MAX, u16b);

	/* Array of grids */
	C_MAKE(temp_g, TEMP_MAX, u16b);

	/* has_lite patch causes both temp_g and temp_x/y to be used
	   in targetting mode: can't use the same memory any more. */
	C_MAKE(temp_y, TEMP_MAX, byte);
	C_MAKE(temp_x, TEMP_MAX, byte);

	/* Array of dynamic grids */
	C_MAKE(dyna_g, DYNA_MAX, dynamic_grid_type);

	/* Array of stacked monster messages */
	C_MAKE(mon_msg, MAX_STORED_MON_MSG, monster_race_message);

	/* Prepare monster movement array*/
	C_MAKE(mon_moment_info, z_info->m_max, move_moment_type);

	/*** Prepare dungeon arrays ***/

	/* Padded into array */
	C_MAKE(cave_info, MAX_DUNGEON_HGT, u16b_256);

	/* Feature array */
	C_MAKE(cave_feat, MAX_DUNGEON_HGT, byte_wid);

	/* Entity arrays */
	C_MAKE(cave_o_idx, MAX_DUNGEON_HGT, s16b_wid);
	C_MAKE(cave_m_idx, MAX_DUNGEON_HGT, s16b_wid);
	C_MAKE(cave_x_idx, MAX_DUNGEON_HGT, s16b_wid);

#ifdef MONSTER_SMELL

	/* Flow arrays */
	C_MAKE(cave_when, MAX_DUNGEON_HGT, byte_wid);

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
	C_MAKE(o_list, z_info->o_max, object_type);

	/* Monsters */
	C_MAKE(mon_list, z_info->m_max, monster_type);

	/* Effects */
	C_MAKE(x_list, z_info->x_max, effect_type);


	/*** Prepare mosnter lore array ***/

	/* Lore */
	C_MAKE(l_list, z_info->r_max, monster_lore);

	/*** Prepare terrain lore array ***/

	/* Lore */
	C_MAKE(f_l_list, z_info->f_max, feature_lore);


	/*** Prepare the inventory ***/

	/* Allocate it */
	C_MAKE(inventory, INVEN_TOTAL, object_type);


	/*** Prepare the stores ***/

	/* Allocate the stores */
	C_MAKE(store, MAX_STORES, store_type);

	/* Fill in each store */
	for (i = 0; i < MAX_STORES; i++)
	{

		/* Get the store */
		store_type *st_ptr = &store[i];

		/* Assume full stock */
		st_ptr->stock_size = STORE_INVEN_MAX;

		/* Allocate the stock */
		C_MAKE(st_ptr->stock, st_ptr->stock_size, object_type);

	}


	/*** Prepare the options ***/

	/* Initialize the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		/* Default value */
		op_ptr->opt[i] = options[i].norm;
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
	p_ptr->window = 0L;


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

	quest_type *q_ptr;

	alloc_entry *table;

	s16b num[MAX_DEPTH];

	s16b aux[MAX_DEPTH];


	/*** Analyze object allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(num, MAX_DEPTH, s16b);

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
	for (i = 1; i < MAX_DEPTH; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/* Paranoia */
	if (!num[0]) quit("No town objects!");


	/*** Initialize object allocation info ***/

	/* Allocate the alloc_kind_table */
	C_MAKE(alloc_kind_table, alloc_kind_size, alloc_entry);

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
	(void)C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(&num, MAX_DEPTH, s16b);

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
	for (i = 1; i < MAX_DEPTH; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/* Paranoia - not really necessary */
	if (!num[0]) quit("No town features!");

	/*** Initialize feature allocation info ***/

	/* Allocate the alloc_feat_table */
	C_MAKE(alloc_feat_table, alloc_feat_size, alloc_entry);

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
	(void)C_WIPE(aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(num, MAX_DEPTH, s16b);

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
			r_ptr = &r_info[q_ptr->mon_idx];
		}
	}

	/* Success */
	return (0);
}


/*
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
	Term_erase(0, 23, 255);
	Term_putstr(20, 23, -1, TERM_WHITE, str);
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
 *
 * Note that the "graf-xxx.prf" file must be loaded separately,
 * if needed, in the first (?) pass through "TERM_XTRA_REACT".
 */
void init_angband(void)
{
	int fd;

	int mode = 0644;

	FILE *fp;

	char buf[1024];


	/*** Verify the "news" file ***/

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "news.txt");

	/* Attempt to open the file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure */
	if (fd < 0)
	{
		char why[1024];

		/* Message */
		strnfmt(why, sizeof(why), "Cannot access the '%s' file!", buf);

		/* Crash and burn */
		init_angband_aux(why);
	}

	/* Close it */
	fd_close(fd);


	/*** Display the "news" file ***/

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, "news.txt");

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
			Term_putstr(0, i++, -1, TERM_WHITE, buf);
		}

		/* Close */
		my_fclose(fp);
	}

	/* Flush it */
	(void)Term_fresh();


	/*** Verify (or create) the "high score" file ***/

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

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
			strnfmt(why, sizeof(why), "Cannot create the '%s' file!", buf);

			/* Crash and burn */
			init_angband_aux(why);
		}
	}

	/* Close it */
	fd_close(fd);

	/*** Initialize some arrays ***/

	/* Initialize size info */
	note("[Initializing array sizes...]");
	if (init_z_info()) quit("Cannot initialize sizes");

	/* Initialize feature info */
	note("[Initializing arrays... (features)]");
	if (init_f_info()) quit("Cannot initialize features");

	/* Initialize object info */
	note("[Initializing arrays... (objects)]");
	if (init_k_info()) quit("Cannot initialize objects");

	/* Initialize artifact info */
	note("[Initializing arrays... (artifacts)]");
	if (init_a_info()) quit("Cannot initialize artifacts");

	/* Initialize ego-item info */
	note("[Initializing arrays... (ego-items)]");
	if (init_e_info()) quit("Cannot initialize ego-items");

	/* Initialize monster info */
	note("[Initializing arrays... (monsters)]");
	if (init_r_info()) quit("Cannot initialize monsters");

	/* Initialize feature info */
	note("[Initializing arrays... (vaults)]");
	if (init_v_info()) quit("Cannot initialize vaults");

	/* Initialize history info */
	note("[Initializing arrays... (histories)]");
	if (init_h_info()) quit("Cannot initialize histories");

	/* Initialize race info */
	note("[Initializing arrays... (races)]");
	if (init_p_info()) quit("Cannot initialize races");

	/* Initialize class info */
	note("[Initializing arrays... (classes)]");
	if (init_c_info()) quit("Cannot initialize classes");

	/* Initialize owner info */
	note("[Initializing arrays... (owners)]");
	if (init_b_info()) quit("Cannot initialize owners");

	/* Initialize price info */
	note("[Initializing arrays... (prices)]");
	if (init_g_info()) quit("Cannot initialize prices");

	/* Initialize flavor info */
	note("[Initializing arrays... (flavors)]");
	if (init_flavor_info()) quit("Cannot initialize flavors");

	/* Initialize quest info */
	note("[Initializing arrays... (quests)]");
	if (init_q_info()) quit("Cannot initialize quests");

	/* Initialize some other arrays */
	note("[Initializing arrays... (other)]");
	if (init_other()) quit("Cannot initialize other stuff");

	/* Initialize some other arrays */
	note("[Initializing arrays... (alloc)]");
	if (init_alloc()) quit("Cannot initialize alloc stuff");

	/*** Load default user pref files ***/

	/* Initialize feature info */
	note("[Loading basic user pref file...]");

	/* Process that file */
	(void)process_pref_file("pref.prf");

	/* Initialize feature info */
	note("[Initializing Random Artifact Tables...]");

	/* Initialize the random artifact table */
	if (init_n_info())
	  quit("Cannot initialize random name generator stuff");

	/*Build the randart probability tables based on the standard Artifact Set*/
	build_randart_tables();

	note("[Initializing new spells...]");
	build_new_spells();

	/* Done */
	note("[Initialization complete]");
}


void cleanup_angband(void)
{
	int i;

	static bool once = FALSE;
	if(once) return;
	once = TRUE;

	delete_current_bones_file();

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

	/* Free the player inventory */
	FREE(inventory);

	/*Clean the Autoinscribe*/
	autoinscribe_clean();

	/* Free the lore, monster, effects, and object lists */
	FREE(l_list);
	FREE(f_l_list);
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

	/* Free the dynamic features array */
	FREE(dyna_g);

	/* Free the stacked monster messages */
	FREE(mon_msg);

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

void build_new_spells(void)
{
	C_MAKE(newspells, N_NEWSPELLS, new_spell_info_type);
	{
	newspells[NEWSPELL_BLESS].code =    NEWSPELL_BLESS;
	newspells[NEWSPELL_BLESS].school1 = SCHOOL_BLESSINGS;
	newspells[NEWSPELL_BLESS].school2 = SCHOOL_BUFFS;
	newspells[NEWSPELL_BLESS].level =   5;
	newspells[NEWSPELL_BLESS].cost =    2;
	my_strcpy(newspells[NEWSPELL_BLESS].name,"Bless",80);
	newspells[NEWSPELL_BLESS].book =    0;
	}
	{
	newspells[NEWSPELL_CLW].code =    NEWSPELL_CLW;
	newspells[NEWSPELL_CLW].school1 = SCHOOL_HEALING;
	newspells[NEWSPELL_CLW].school2 = -1;
	newspells[NEWSPELL_CLW].level =   8;
	newspells[NEWSPELL_CLW].cost =    2;
	my_strcpy(newspells[NEWSPELL_CLW].name,"Cure Light Wounds",80);
	newspells[NEWSPELL_CLW].book =    0;
	}
	{
	newspells[NEWSPELL_MAGIC_MISSILE].code =    NEWSPELL_MAGIC_MISSILE;
	newspells[NEWSPELL_MAGIC_MISSILE].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_MAGIC_MISSILE].school2 = SCHOOL_FORCE;
	newspells[NEWSPELL_MAGIC_MISSILE].level =   11;
	newspells[NEWSPELL_MAGIC_MISSILE].cost =    2;
	my_strcpy(newspells[NEWSPELL_MAGIC_MISSILE].name,"Magic Missile",80);
	newspells[NEWSPELL_MAGIC_MISSILE].book =    0;
	}
	{
	newspells[NEWSPELL_LIGHT].code =    NEWSPELL_LIGHT;
	newspells[NEWSPELL_LIGHT].school1 = SCHOOL_FIRE_AND_LIGHT;
	newspells[NEWSPELL_LIGHT].school2 = -1;
	newspells[NEWSPELL_LIGHT].level =   9;
	newspells[NEWSPELL_LIGHT].cost =    2;
	my_strcpy(newspells[NEWSPELL_LIGHT].name,"Light",80);
	newspells[NEWSPELL_LIGHT].book =    0;
	}
	{
	newspells[NEWSPELL_DETECT_ANIMALS].code =    NEWSPELL_DETECT_ANIMALS;
	newspells[NEWSPELL_DETECT_ANIMALS].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_DETECT_ANIMALS].school2 = SCHOOL_NATURE;
	newspells[NEWSPELL_DETECT_ANIMALS].level =   11;
	newspells[NEWSPELL_DETECT_ANIMALS].cost =    2;
	my_strcpy(newspells[NEWSPELL_DETECT_ANIMALS].name,"Detect Animals",80);
	newspells[NEWSPELL_DETECT_ANIMALS].book =    0;
	}
	{
	newspells[NEWSPELL_SLEEP].code =    NEWSPELL_SLEEP;
	newspells[NEWSPELL_SLEEP].school1 = SCHOOL_ENCHANTMENT;
	newspells[NEWSPELL_SLEEP].school2 = -1;
	newspells[NEWSPELL_SLEEP].level =   13;
	newspells[NEWSPELL_SLEEP].cost =    5;
	my_strcpy(newspells[NEWSPELL_SLEEP].name,"Sleep",80);
	newspells[NEWSPELL_SLEEP].book =    0;
	}
	{
	newspells[NEWSPELL_CREATE_FOOD].code =    NEWSPELL_CREATE_FOOD;
	newspells[NEWSPELL_CREATE_FOOD].school1 = SCHOOL_CONJURATIONS;
	newspells[NEWSPELL_CREATE_FOOD].school2 = -1;
	newspells[NEWSPELL_CREATE_FOOD].level =   18;
	newspells[NEWSPELL_CREATE_FOOD].cost =    9;
	my_strcpy(newspells[NEWSPELL_CREATE_FOOD].name,"Create Food",80);
	newspells[NEWSPELL_CREATE_FOOD].book =    1;
	}
	{
	newspells[NEWSPELL_DETECT_EVIL].code =    NEWSPELL_DETECT_EVIL;
	newspells[NEWSPELL_DETECT_EVIL].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_DETECT_EVIL].school2 = -1;
	newspells[NEWSPELL_DETECT_EVIL].level =   12;
	newspells[NEWSPELL_DETECT_EVIL].cost =    2;
	my_strcpy(newspells[NEWSPELL_DETECT_EVIL].name,"Detect Evil",80);
	newspells[NEWSPELL_DETECT_EVIL].book =    1;
	}
	{
	newspells[NEWSPELL_BARKSKIN].code =    NEWSPELL_BARKSKIN;
	newspells[NEWSPELL_BARKSKIN].school1 = SCHOOL_NATURE;
	newspells[NEWSPELL_BARKSKIN].school2 = SCHOOL_BUFFS;
	newspells[NEWSPELL_BARKSKIN].level =   20;
	newspells[NEWSPELL_BARKSKIN].cost =    4;
	my_strcpy(newspells[NEWSPELL_BARKSKIN].name,"Barkskin",80);
	newspells[NEWSPELL_BARKSKIN].book =    1;
	}
	{
	newspells[NEWSPELL_SLOW].code =    NEWSPELL_SLOW;
	newspells[NEWSPELL_SLOW].school1 = SCHOOL_TIME;
	newspells[NEWSPELL_SLOW].school2 = SCHOOL_ENCHANTMENT;
	newspells[NEWSPELL_SLOW].level =   21;
	newspells[NEWSPELL_SLOW].cost =    7;
	my_strcpy(newspells[NEWSPELL_SLOW].name,"Slow",80);
	newspells[NEWSPELL_SLOW].book =    1;
	}
	{
	newspells[NEWSPELL_REMOVE_FEAR].code =    NEWSPELL_REMOVE_FEAR;
	newspells[NEWSPELL_REMOVE_FEAR].school1 = SCHOOL_BLESSINGS;
	newspells[NEWSPELL_REMOVE_FEAR].school2 = -1;
	newspells[NEWSPELL_REMOVE_FEAR].level =   22;
	newspells[NEWSPELL_REMOVE_FEAR].cost =    3;
	my_strcpy(newspells[NEWSPELL_REMOVE_FEAR].name,"Remove Fear",80);
	newspells[NEWSPELL_REMOVE_FEAR].book =    1;
	}
	{
	newspells[NEWSPELL_FIRE_BOLT].code =    NEWSPELL_FIRE_BOLT;
	newspells[NEWSPELL_FIRE_BOLT].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_FIRE_BOLT].school2 = SCHOOL_FIRE_AND_LIGHT;
	newspells[NEWSPELL_FIRE_BOLT].level =   19;
	newspells[NEWSPELL_FIRE_BOLT].cost =    6;
	my_strcpy(newspells[NEWSPELL_FIRE_BOLT].name,"Firebolt",80);
	newspells[NEWSPELL_FIRE_BOLT].book =    1;
	}
	{
	newspells[NEWSPELL_MAGELIGHT].code =    NEWSPELL_MAGELIGHT;
	newspells[NEWSPELL_MAGELIGHT].school1 = SCHOOL_FIRE_AND_LIGHT;
	newspells[NEWSPELL_MAGELIGHT].school2 = SCHOOL_CONJURATIONS;
	newspells[NEWSPELL_MAGELIGHT].level =   22;
	newspells[NEWSPELL_MAGELIGHT].cost =    10;
	my_strcpy(newspells[NEWSPELL_MAGELIGHT].name,"Magelight",80);
	newspells[NEWSPELL_MAGELIGHT].book =    2;
	}
	{
	newspells[NEWSPELL_REMOVE_CURSE].code =    NEWSPELL_REMOVE_CURSE;
	newspells[NEWSPELL_REMOVE_CURSE].school1 = SCHOOL_BLESSINGS;
	newspells[NEWSPELL_REMOVE_CURSE].school2 = -1;
	newspells[NEWSPELL_REMOVE_CURSE].level =   18;
	newspells[NEWSPELL_REMOVE_CURSE].cost =    9;
	my_strcpy(newspells[NEWSPELL_REMOVE_CURSE].name,"Remove Curse",80);
	newspells[NEWSPELL_REMOVE_CURSE].book =    2;
	}
	{
	newspells[NEWSPELL_DETECT_TREASURE].code =    NEWSPELL_DETECT_TREASURE;
	newspells[NEWSPELL_DETECT_TREASURE].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_DETECT_TREASURE].school2 = -1;
	newspells[NEWSPELL_DETECT_TREASURE].level =   14;
	newspells[NEWSPELL_DETECT_TREASURE].cost =    2;
	my_strcpy(newspells[NEWSPELL_DETECT_TREASURE].name,"Detect Treasure",80);
	newspells[NEWSPELL_DETECT_TREASURE].book =    2;
	}
	{
	newspells[NEWSPELL_CSW].code =    NEWSPELL_CSW;
	newspells[NEWSPELL_CSW].school1 = SCHOOL_HEALING;
	newspells[NEWSPELL_CSW].school2 = -1;
	newspells[NEWSPELL_CSW].level =   20;
	newspells[NEWSPELL_CSW].cost =    6;
	my_strcpy(newspells[NEWSPELL_CSW].name,"Cure Serious Wounds",80);
	newspells[NEWSPELL_CSW].book =    2;
	}
	{
	newspells[NEWSPELL_CONFUSION].code =    NEWSPELL_CONFUSION;
	newspells[NEWSPELL_CONFUSION].school1 = SCHOOL_ENCHANTMENT;
	newspells[NEWSPELL_CONFUSION].school2 = -1;
	newspells[NEWSPELL_CONFUSION].level =   16;
	newspells[NEWSPELL_CONFUSION].cost =    6;
	my_strcpy(newspells[NEWSPELL_CONFUSION].name,"Confusion",80);
	newspells[NEWSPELL_CONFUSION].book =    2;
	}
	{
	newspells[NEWSPELL_MANA_BOLT].code =    NEWSPELL_MANA_BOLT;
	newspells[NEWSPELL_MANA_BOLT].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_MANA_BOLT].school2 = SCHOOL_FORCE;
	newspells[NEWSPELL_MANA_BOLT].level =   25;
	newspells[NEWSPELL_MANA_BOLT].cost =    7;
	my_strcpy(newspells[NEWSPELL_MANA_BOLT].name,"Mana Bolt",80);
	newspells[NEWSPELL_MANA_BOLT].book =    2;
	}
	{
	newspells[NEWSPELL_IDENTIFY].code =    NEWSPELL_IDENTIFY;
	newspells[NEWSPELL_IDENTIFY].school1 = SCHOOL_IDENTIFICATION;
	newspells[NEWSPELL_IDENTIFY].school2 = -1;
	newspells[NEWSPELL_IDENTIFY].level =   17;
	newspells[NEWSPELL_IDENTIFY].cost =    7;
	my_strcpy(newspells[NEWSPELL_IDENTIFY].name,"Identify",80);
	newspells[NEWSPELL_IDENTIFY].book =    3;
	}
	{
	newspells[NEWSPELL_DETECT_TRAPS_AND_DOORS].code =    NEWSPELL_DETECT_TRAPS_AND_DOORS;
	newspells[NEWSPELL_DETECT_TRAPS_AND_DOORS].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_DETECT_TRAPS_AND_DOORS].school2 = -1;
	newspells[NEWSPELL_DETECT_TRAPS_AND_DOORS].level =   14;
	newspells[NEWSPELL_DETECT_TRAPS_AND_DOORS].cost =    3;
	my_strcpy(newspells[NEWSPELL_DETECT_TRAPS_AND_DOORS].name,"Detect Traps and Doors",80);
	newspells[NEWSPELL_DETECT_TRAPS_AND_DOORS].book =    3;
	}
	{
	newspells[NEWSPELL_PHASE_DOOR].code =    NEWSPELL_PHASE_DOOR;
	newspells[NEWSPELL_PHASE_DOOR].school1 = SCHOOL_TELEPORTATION;
	newspells[NEWSPELL_PHASE_DOOR].school2 = -1;
	newspells[NEWSPELL_PHASE_DOOR].level =   15;
	newspells[NEWSPELL_PHASE_DOOR].cost =    3;
	my_strcpy(newspells[NEWSPELL_PHASE_DOOR].name,"Phase Door",80);
	newspells[NEWSPELL_PHASE_DOOR].book =    3;
	}
	{
	newspells[NEWSPELL_RESIST_HEAT_AND_COLD].code =    NEWSPELL_RESIST_HEAT_AND_COLD;
	newspells[NEWSPELL_RESIST_HEAT_AND_COLD].school1 = SCHOOL_RESISTANCE;
	newspells[NEWSPELL_RESIST_HEAT_AND_COLD].school2 = SCHOOL_FIRE_AND_LIGHT;
	newspells[NEWSPELL_RESIST_HEAT_AND_COLD].level =   24;
	newspells[NEWSPELL_RESIST_HEAT_AND_COLD].cost =    5;
	my_strcpy(newspells[NEWSPELL_RESIST_HEAT_AND_COLD].name,"Resist Heat and Cold",80);
	newspells[NEWSPELL_RESIST_HEAT_AND_COLD].book =    3;
	}
	{
	newspells[NEWSPELL_FROST_BOLT].code =    NEWSPELL_FROST_BOLT;
	newspells[NEWSPELL_FROST_BOLT].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_FROST_BOLT].school2 = -1;
	newspells[NEWSPELL_FROST_BOLT].level =   22;
	newspells[NEWSPELL_FROST_BOLT].cost =    6;
	my_strcpy(newspells[NEWSPELL_FROST_BOLT].name,"Frost Bolt",80);
	newspells[NEWSPELL_FROST_BOLT].book =    3;
	}
	{
	newspells[NEWSPELL_DETECT_MONSTERS].code =    NEWSPELL_DETECT_MONSTERS;
	newspells[NEWSPELL_DETECT_MONSTERS].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_DETECT_MONSTERS].school2 = -1;
	newspells[NEWSPELL_DETECT_MONSTERS].level =   21;
	newspells[NEWSPELL_DETECT_MONSTERS].cost =    3;
	my_strcpy(newspells[NEWSPELL_DETECT_MONSTERS].name,"Detect Monsters",80);
	newspells[NEWSPELL_DETECT_MONSTERS].book =    3;
	}
	{
	newspells[NEWSPELL_RESTORE_LIFE_LEVELS].code =    NEWSPELL_RESTORE_LIFE_LEVELS;
	newspells[NEWSPELL_RESTORE_LIFE_LEVELS].school1 = SCHOOL_BLESSINGS;
	newspells[NEWSPELL_RESTORE_LIFE_LEVELS].school2 = SCHOOL_HEALING;
	newspells[NEWSPELL_RESTORE_LIFE_LEVELS].level =   31;
	newspells[NEWSPELL_RESTORE_LIFE_LEVELS].cost =    13;
	my_strcpy(newspells[NEWSPELL_RESTORE_LIFE_LEVELS].name,"Restore Life Levels",80);
	newspells[NEWSPELL_RESTORE_LIFE_LEVELS].book =    4;
	}
	{
	newspells[NEWSPELL_TELEPORT_AWAY].code =    NEWSPELL_TELEPORT_AWAY;
	newspells[NEWSPELL_TELEPORT_AWAY].school1 = SCHOOL_TELEPORTATION;
	newspells[NEWSPELL_TELEPORT_AWAY].school2 = SCHOOL_FORCE;
	newspells[NEWSPELL_TELEPORT_AWAY].level =   29;
	newspells[NEWSPELL_TELEPORT_AWAY].cost =    6;
	my_strcpy(newspells[NEWSPELL_TELEPORT_AWAY].name,"Teleport Away",80);
	newspells[NEWSPELL_TELEPORT_AWAY].book =    4;
	}
	{
	newspells[NEWSPELL_MASS_IDENTIFY].code =    NEWSPELL_MASS_IDENTIFY;
	newspells[NEWSPELL_MASS_IDENTIFY].school1 = SCHOOL_IDENTIFICATION;
	newspells[NEWSPELL_MASS_IDENTIFY].school2 = -1;
	newspells[NEWSPELL_MASS_IDENTIFY].level =   38;
	newspells[NEWSPELL_MASS_IDENTIFY].cost =    14;
	my_strcpy(newspells[NEWSPELL_MASS_IDENTIFY].name,"Mass Identify",80);
	newspells[NEWSPELL_MASS_IDENTIFY].book =    4;
	}
	{
	newspells[NEWSPELL_RESIST_POISON].code =    NEWSPELL_RESIST_POISON;
	newspells[NEWSPELL_RESIST_POISON].school1 = SCHOOL_RESISTANCE;
	newspells[NEWSPELL_RESIST_POISON].school2 = SCHOOL_HEALING;
	newspells[NEWSPELL_RESIST_POISON].level =   28;
	newspells[NEWSPELL_RESIST_POISON].cost =    9;
	my_strcpy(newspells[NEWSPELL_RESIST_POISON].name,"Resist Poison",80);
	newspells[NEWSPELL_RESIST_POISON].book =    4;
	}
	{
	newspells[NEWSPELL_CCW].code =    NEWSPELL_CCW;
	newspells[NEWSPELL_CCW].school1 = SCHOOL_HEALING;
	newspells[NEWSPELL_CCW].school2 = -1;
	newspells[NEWSPELL_CCW].level =   26;
	newspells[NEWSPELL_CCW].cost =    10;
	my_strcpy(newspells[NEWSPELL_CCW].name,"Cure Critical Wounds",80);
	newspells[NEWSPELL_CCW].book =    4;
	}
	{
	newspells[NEWSPELL_FIREBALL].code =    NEWSPELL_FIREBALL;
	newspells[NEWSPELL_FIREBALL].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_FIREBALL].school2 = SCHOOL_FIRE_AND_LIGHT;
	newspells[NEWSPELL_FIREBALL].level =   32;
	newspells[NEWSPELL_FIREBALL].cost =    10;
	my_strcpy(newspells[NEWSPELL_FIREBALL].name,"Fireball",80);
	newspells[NEWSPELL_FIREBALL].book =    4;
	}
	{
	newspells[NEWSPELL_STARIDENTIFYSTAR].code =    NEWSPELL_STARIDENTIFYSTAR;
	newspells[NEWSPELL_STARIDENTIFYSTAR].school1 = SCHOOL_IDENTIFICATION;
	newspells[NEWSPELL_STARIDENTIFYSTAR].school2 = -1;
	newspells[NEWSPELL_STARIDENTIFYSTAR].level =   23;
	newspells[NEWSPELL_STARIDENTIFYSTAR].cost =    15;
	my_strcpy(newspells[NEWSPELL_STARIDENTIFYSTAR].name,"*Identify*",80);
	newspells[NEWSPELL_STARIDENTIFYSTAR].book =    5;
	}
	{
	newspells[NEWSPELL_STARREMOVE_CURSESTAR].code =    NEWSPELL_STARREMOVE_CURSESTAR;
	newspells[NEWSPELL_STARREMOVE_CURSESTAR].school1 = SCHOOL_BLESSINGS;
	newspells[NEWSPELL_STARREMOVE_CURSESTAR].school2 = -1;
	newspells[NEWSPELL_STARREMOVE_CURSESTAR].level =   28;
	newspells[NEWSPELL_STARREMOVE_CURSESTAR].cost =    20;
	my_strcpy(newspells[NEWSPELL_STARREMOVE_CURSESTAR].name,"*Remove Curse*",80);
	newspells[NEWSPELL_STARREMOVE_CURSESTAR].book =    5;
	}
	{
	newspells[NEWSPELL_SEE_INVISIBLE].code =    NEWSPELL_SEE_INVISIBLE;
	newspells[NEWSPELL_SEE_INVISIBLE].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_SEE_INVISIBLE].school2 = -1;
	newspells[NEWSPELL_SEE_INVISIBLE].level =   30;
	newspells[NEWSPELL_SEE_INVISIBLE].cost =    8;
	my_strcpy(newspells[NEWSPELL_SEE_INVISIBLE].name,"See Invisible",80);
	newspells[NEWSPELL_SEE_INVISIBLE].book =    5;
	}
	{
	newspells[NEWSPELL_DETECT_OBJECTS].code =    NEWSPELL_DETECT_OBJECTS;
	newspells[NEWSPELL_DETECT_OBJECTS].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_DETECT_OBJECTS].school2 = -1;
	newspells[NEWSPELL_DETECT_OBJECTS].level =   20;
	newspells[NEWSPELL_DETECT_OBJECTS].cost =    3;
	my_strcpy(newspells[NEWSPELL_DETECT_OBJECTS].name,"Detect Objects",80);
	newspells[NEWSPELL_DETECT_OBJECTS].book =    5;
	}
	{
	newspells[NEWSPELL_PORTAL].code =    NEWSPELL_PORTAL;
	newspells[NEWSPELL_PORTAL].school1 = SCHOOL_TELEPORTATION;
	newspells[NEWSPELL_PORTAL].school2 = -1;
	newspells[NEWSPELL_PORTAL].level =   22;
	newspells[NEWSPELL_PORTAL].cost =    7;
	my_strcpy(newspells[NEWSPELL_PORTAL].name,"Portal",80);
	newspells[NEWSPELL_PORTAL].book =    5;
	}
	{
	newspells[NEWSPELL_ACID_BALL].code =    NEWSPELL_ACID_BALL;
	newspells[NEWSPELL_ACID_BALL].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_ACID_BALL].school2 = -1;
	newspells[NEWSPELL_ACID_BALL].level =   31;
	newspells[NEWSPELL_ACID_BALL].cost =    10;
	my_strcpy(newspells[NEWSPELL_ACID_BALL].name,"Acid Ball",80);
	newspells[NEWSPELL_ACID_BALL].book =    5;
	}
	{
	newspells[NEWSPELL_SANDSTORM].code =    NEWSPELL_SANDSTORM;
	newspells[NEWSPELL_SANDSTORM].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_SANDSTORM].school2 = SCHOOL_NATURE;
	newspells[NEWSPELL_SANDSTORM].level =   27;
	newspells[NEWSPELL_SANDSTORM].cost =    7;
	my_strcpy(newspells[NEWSPELL_SANDSTORM].name,"Sandstorm",80);
	newspells[NEWSPELL_SANDSTORM].book =    6;
	}
	{
	newspells[NEWSPELL_MAGIC_MAPPING].code =    NEWSPELL_MAGIC_MAPPING;
	newspells[NEWSPELL_MAGIC_MAPPING].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_MAGIC_MAPPING].school2 = -1;
	newspells[NEWSPELL_MAGIC_MAPPING].level =   33;
	newspells[NEWSPELL_MAGIC_MAPPING].cost =    5;
	my_strcpy(newspells[NEWSPELL_MAGIC_MAPPING].name,"Magic Mapping",80);
	newspells[NEWSPELL_MAGIC_MAPPING].book =    6;
	}
	{
	newspells[NEWSPELL_NEUTRALIZE_POISON].code =    NEWSPELL_NEUTRALIZE_POISON;
	newspells[NEWSPELL_NEUTRALIZE_POISON].school1 = SCHOOL_HEALING;
	newspells[NEWSPELL_NEUTRALIZE_POISON].school2 = -1;
	newspells[NEWSPELL_NEUTRALIZE_POISON].level =   23;
	newspells[NEWSPELL_NEUTRALIZE_POISON].cost =    5;
	my_strcpy(newspells[NEWSPELL_NEUTRALIZE_POISON].name,"Neutralize Poison",80);
	newspells[NEWSPELL_NEUTRALIZE_POISON].book =    6;
	}
	{
	newspells[NEWSPELL_CREATE_MISSILES].code =    NEWSPELL_CREATE_MISSILES;
	newspells[NEWSPELL_CREATE_MISSILES].school1 = SCHOOL_CONJURATIONS;
	newspells[NEWSPELL_CREATE_MISSILES].school2 = -1;
	newspells[NEWSPELL_CREATE_MISSILES].level =   26;
	newspells[NEWSPELL_CREATE_MISSILES].cost =    15;
	my_strcpy(newspells[NEWSPELL_CREATE_MISSILES].name,"Create Missiles",80);
	newspells[NEWSPELL_CREATE_MISSILES].book =    6;
	}
	{
	newspells[NEWSPELL_ENCHANT_ARMOR].code =    NEWSPELL_ENCHANT_ARMOR;
	newspells[NEWSPELL_ENCHANT_ARMOR].school1 = SCHOOL_BUFFS;
	newspells[NEWSPELL_ENCHANT_ARMOR].school2 = SCHOOL_RESISTANCE;
	newspells[NEWSPELL_ENCHANT_ARMOR].level =   25;
	newspells[NEWSPELL_ENCHANT_ARMOR].cost =    12;
	my_strcpy(newspells[NEWSPELL_ENCHANT_ARMOR].name,"Enchant Armor",80);
	newspells[NEWSPELL_ENCHANT_ARMOR].book =    6;
	}
	{
	newspells[NEWSPELL_STONE_TO_MUD].code =    NEWSPELL_STONE_TO_MUD;
	newspells[NEWSPELL_STONE_TO_MUD].school1 = SCHOOL_NATURE;
	newspells[NEWSPELL_STONE_TO_MUD].school2 = -1;
	newspells[NEWSPELL_STONE_TO_MUD].level =   33;
	newspells[NEWSPELL_STONE_TO_MUD].cost =    6;
	my_strcpy(newspells[NEWSPELL_STONE_TO_MUD].name,"Stone to Mud",80);
	newspells[NEWSPELL_STONE_TO_MUD].book =    6;
	}
	{
	newspells[NEWSPELL_DETECTION].code =    NEWSPELL_DETECTION;
	newspells[NEWSPELL_DETECTION].school1 = SCHOOL_DETECTION;
	newspells[NEWSPELL_DETECTION].school2 = -1;
	newspells[NEWSPELL_DETECTION].level =   32;
	newspells[NEWSPELL_DETECTION].cost =    7;
	my_strcpy(newspells[NEWSPELL_DETECTION].name,"Detection",80);
	newspells[NEWSPELL_DETECTION].book =    7;
	}
	{
	newspells[NEWSPELL_TELEPORTATION].code =    NEWSPELL_TELEPORTATION;
	newspells[NEWSPELL_TELEPORTATION].school1 = SCHOOL_TELEPORTATION;
	newspells[NEWSPELL_TELEPORTATION].school2 = -1;
	newspells[NEWSPELL_TELEPORTATION].level =   30;
	newspells[NEWSPELL_TELEPORTATION].cost =    9;
	my_strcpy(newspells[NEWSPELL_TELEPORTATION].name,"Teleportation",80);
	newspells[NEWSPELL_TELEPORTATION].book =    7;
	}
	{
	newspells[NEWSPELL_HEALING].code =    NEWSPELL_HEALING;
	newspells[NEWSPELL_HEALING].school1 = SCHOOL_HEALING;
	newspells[NEWSPELL_HEALING].school2 = -1;
	newspells[NEWSPELL_HEALING].level =   39;
	newspells[NEWSPELL_HEALING].cost =    20;
	my_strcpy(newspells[NEWSPELL_HEALING].name,"Healing",80);
	newspells[NEWSPELL_HEALING].book =    7;
	}
	{
	newspells[NEWSPELL_MANA_STORM].code =    NEWSPELL_MANA_STORM;
	newspells[NEWSPELL_MANA_STORM].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_MANA_STORM].school2 = SCHOOL_FORCE;
	newspells[NEWSPELL_MANA_STORM].level =   45;
	newspells[NEWSPELL_MANA_STORM].cost =    30;
	my_strcpy(newspells[NEWSPELL_MANA_STORM].name,"Mana Storm",80);
	newspells[NEWSPELL_MANA_STORM].book =    7;
	}
	{
	newspells[NEWSPELL_PROTECTION_FROM_EVIL].code =    NEWSPELL_PROTECTION_FROM_EVIL;
	newspells[NEWSPELL_PROTECTION_FROM_EVIL].school1 = SCHOOL_BLESSINGS;
	newspells[NEWSPELL_PROTECTION_FROM_EVIL].school2 = SCHOOL_BUFFS;
	newspells[NEWSPELL_PROTECTION_FROM_EVIL].level =   28;
	newspells[NEWSPELL_PROTECTION_FROM_EVIL].cost =    7;
	my_strcpy(newspells[NEWSPELL_PROTECTION_FROM_EVIL].name,"Protection from Evil",80);
	newspells[NEWSPELL_PROTECTION_FROM_EVIL].book =    7;
	}
	{
	newspells[NEWSPELL_GLOBE_OF_INVULNERABILITY].code =    NEWSPELL_GLOBE_OF_INVULNERABILITY;
	newspells[NEWSPELL_GLOBE_OF_INVULNERABILITY].school1 = SCHOOL_FORCE;
	newspells[NEWSPELL_GLOBE_OF_INVULNERABILITY].school2 = SCHOOL_BUFFS;
	newspells[NEWSPELL_GLOBE_OF_INVULNERABILITY].level =   44;
	newspells[NEWSPELL_GLOBE_OF_INVULNERABILITY].cost =    22;
	my_strcpy(newspells[NEWSPELL_GLOBE_OF_INVULNERABILITY].name,"Globe of Invulnerability",80);
	newspells[NEWSPELL_GLOBE_OF_INVULNERABILITY].book =    7;
	}
	{
	newspells[NEWSPELL_CREATE_STAIRS].code =    NEWSPELL_CREATE_STAIRS;
	newspells[NEWSPELL_CREATE_STAIRS].school1 = SCHOOL_CONJURATIONS;
	newspells[NEWSPELL_CREATE_STAIRS].school2 = SCHOOL_TELEPORTATION;
	newspells[NEWSPELL_CREATE_STAIRS].level =   40;
	newspells[NEWSPELL_CREATE_STAIRS].cost =    28;
	my_strcpy(newspells[NEWSPELL_CREATE_STAIRS].name,"Create Stairs",80);
	newspells[NEWSPELL_CREATE_STAIRS].book =    8;
	}
	{
	newspells[NEWSPELL_RESTORATION].code =    NEWSPELL_RESTORATION;
	newspells[NEWSPELL_RESTORATION].school1 = SCHOOL_BLESSINGS;
	newspells[NEWSPELL_RESTORATION].school2 = SCHOOL_TIME;
	newspells[NEWSPELL_RESTORATION].level =   34;
	newspells[NEWSPELL_RESTORATION].cost =    25;
	my_strcpy(newspells[NEWSPELL_RESTORATION].name,"Restoration",80);
	newspells[NEWSPELL_RESTORATION].book =    8;
	}
	{
	newspells[NEWSPELL_HASTE_SELF].code =    NEWSPELL_HASTE_SELF;
	newspells[NEWSPELL_HASTE_SELF].school1 = SCHOOL_BUFFS;
	newspells[NEWSPELL_HASTE_SELF].school2 = SCHOOL_TIME;
	newspells[NEWSPELL_HASTE_SELF].level =   36;
	newspells[NEWSPELL_HASTE_SELF].cost =    12;
	my_strcpy(newspells[NEWSPELL_HASTE_SELF].name,"Haste Self",80);
	newspells[NEWSPELL_HASTE_SELF].book =    8;
	}
	{
	newspells[NEWSPELL_RIFT].code =    NEWSPELL_RIFT;
	newspells[NEWSPELL_RIFT].school1 = SCHOOL_BATTLE;
	newspells[NEWSPELL_RIFT].school2 = SCHOOL_TELEPORTATION;
	newspells[NEWSPELL_RIFT].level =   40;
	newspells[NEWSPELL_RIFT].cost =    20;
	my_strcpy(newspells[NEWSPELL_RIFT].name,"Rift",80);
	newspells[NEWSPELL_RIFT].book =    8;
	}
	{
	newspells[NEWSPELL_RESISTANCE].code =    NEWSPELL_RESISTANCE;
	newspells[NEWSPELL_RESISTANCE].school1 = SCHOOL_RESISTANCE;
	newspells[NEWSPELL_RESISTANCE].school2 = -1;
	newspells[NEWSPELL_RESISTANCE].level =   39;
	newspells[NEWSPELL_RESISTANCE].cost =    20;
	my_strcpy(newspells[NEWSPELL_RESISTANCE].name,"Resistance",80);
	newspells[NEWSPELL_RESISTANCE].book =    8;
	}
	{
	newspells[NEWSPELL_NATIVITY].code =    NEWSPELL_NATIVITY;
	newspells[NEWSPELL_NATIVITY].school1 = SCHOOL_NATURE;
	newspells[NEWSPELL_NATIVITY].school2 = SCHOOL_RESISTANCE;
	newspells[NEWSPELL_NATIVITY].level =   37;
	newspells[NEWSPELL_NATIVITY].cost =    10;
	my_strcpy(newspells[NEWSPELL_NATIVITY].name,"Nativity",80);
	newspells[NEWSPELL_NATIVITY].book =    8;
	}
}

