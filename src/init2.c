/* File: init2.c */


/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "script.h"

#include "init.h"

#ifdef CHECK_MODIFICATION_TIME
#ifndef RISCOS
#ifdef MACINTOSH
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif /* MACINTOSH */
#endif /* !RISCOS */
#endif /* CHECK_MODIFICATION_TIME */

#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.  Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "lib/file" ory, from which quick-load binary "image" files
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
 * "path_build()" function in "util.c" for more information.  (Note that
 * we call this via the path_make() macro in defines.h)
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
	string_free(ANGBAND_DIR_SCRIPT);
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
	ANGBAND_DIR_SCRIPT = string_make("");
	ANGBAND_DIR_FILE = string_make("");
	ANGBAND_DIR_HELP = string_make("");
	ANGBAND_DIR_INFO = string_make("");
	ANGBAND_DIR_SAVE = string_make("");
	ANGBAND_DIR_PREF = string_make("");
	ANGBAND_DIR_USER = string_make("");
	ANGBAND_DIR_XTRA = string_make("");


#else  /* VM */


	/*** Build the sub-directory names ***/

	/* Build a path name */
	strcpy(tail, "edit");
	ANGBAND_DIR_EDIT = string_make(path);

	/* Build a path name */
	strcpy(tail, "script");
	ANGBAND_DIR_SCRIPT = string_make(path);

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
	path_make(buf, PRIVATE_USER_PATH, VERSION_NAME);

	/* Build a relative path name */
	ANGBAND_DIR_USER = string_make(buf);

#else  /* PRIVATE_USER_PATH */

	/* Build a path name */
	strcpy(tail, "user");
	ANGBAND_DIR_USER = string_make(path);

#endif /* PRIVATE_USER_PATH */

#ifdef USE_PRIVATE_PATHS

	/* Build a path name */
	path_make(buf, ANGBAND_DIR_USER, "scores");
	ANGBAND_DIR_APEX = string_make(buf);

	/* Build a path name */
	path_make(buf, ANGBAND_DIR_USER, "bone");
	ANGBAND_DIR_BONE = string_make(buf);

	/* Build a path name */
	path_make(buf, ANGBAND_DIR_USER, "data");
	ANGBAND_DIR_DATA = string_make(buf);

	/* Build a path name */
	path_make(buf, ANGBAND_DIR_USER, "save");
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
}



#ifdef ALLOW_TEMPLATES


/*
 * Hack -- help give useful error messages
 */
int error_idx;
int error_line;


/*
 * Standard error message text
 */
cptr err_str[PARSE_ERROR_MAX] =
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
};


#endif /* ALLOW_TEMPLATES */


#ifndef RISCOS
#ifdef CHECK_MODIFICATION_TIME

extern errr check_modification_date(int fd, cptr template_file)
{
	char buf[1024];

	struct stat txt_stat, raw_stat;

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_EDIT, template_file);

	/* Access stats on text file */
	if (stat(buf, &txt_stat))
	{
		/* No text file - continue */
	}

	/* Access stats on raw file */
	else if (fstat(fd, &raw_stat))
	{
		/* Error */
		return (-1);
	}

	/* Ensure text file is not newer than raw file */
	else if (txt_stat.st_mtime > raw_stat.st_mtime)
	{
		/* Reprocess text file */
		return (-1);
	}

	return (0);
}

#endif /* CHECK_MODIFICATION_TIME */
#endif /* !RISCOS */

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
header s_head;


/*** Initialize from binary image files ***/


/*
 * Initialize a "*_info" array, by parsing a binary "image" file
 *
 * If possible, just mmap() the image file directory into memory.
 * This is faster than reading it from disk, because it delays
 * the loading until it's actually accessed. It may also save memory.
 */
static errr init_info_raw(int fd, header *head)
{
	header test;
#ifdef HAVE_MMAP
	char *data;
#endif /* HAVE_MMAP */

	/* Read and verify the header */
	if (fd_read(fd, (char *)(&test), sizeof(header)) ||
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

#ifdef HAVE_MMAP
	data = mmap(NULL, sizeof(header) + head->info_size +
		head->name_size + head->text_size,
		PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

	if (data != MAP_FAILED)
	{
		head->mmap_base = data;

		/* Skip the header */
		data += sizeof(header);

		/* Save a pointer to the info */
		head->info_ptr = data;
		data += head->info_size;

		/* Save a pointer to the names */
		head->name_ptr = data;
		data += head->name_size;

		/* Save a pointer to the text */
		head->text_ptr = data;
	}
	else
	{
#endif /* HAVE_MMAP */

		head->mmap_base = NULL;

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

#ifdef HAVE_MMAP
	}
#endif /* HAVE_MMAP */

	/* Success */
	return (0);
}


/*
 * Initialize the header of an *_info.raw file.
 */
static void init_header(header *head, int num, int len)
{
	/* Save the "version" */
	head->v_major = VER_MAJOR;
	head->v_minor = VER_MINOR;
	head->v_patch = VER_PATCH;
	head->v_extra = VER_EXTRA;

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
	msgf("Error at line %d of '%s'.", error_line, filename);
	msgf("Record %d contains a '%s' error.", error_idx, oops);
	msgf("Parsing '%s'.", buf);
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
static errr init_info(cptr filename, header *head,
                      void **info, char **name, char **text)
{
	int fd;

	errr err = 1;

	FILE *fp;

	/* General buffer */
	char buf[1024];

#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_DATA, format("%s.raw", filename));

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

		/* Hack -- make "fake" arrays */
		if (name)
			C_MAKE(head->name_ptr, z_info->fake_name_size, char);

		if (text)
			C_MAKE(head->text_ptr, z_info->fake_text_size, char);

		if (info) (*info) = head->info_ptr;
		if (name) (*name) = head->name_ptr;
		if (text) (*text) = head->text_ptr;

		/*** Load the ascii template file ***/

		/* Build the filename */
		path_make(buf, ANGBAND_DIR_EDIT, format("%s.txt", filename));

		/* Open the file */
		fp = my_fopen(buf, "r");

		/* Parse it */
		if (!fp) quit_fmt("Cannot open '%s.txt' file.", filename);

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
		path_make(buf, ANGBAND_DIR_DATA, format("%s.raw", filename));


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
				/* Crash and burn */
				quit_fmt("Cannot create the '%s' file!", buf);
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

		/* Dump to the file */
		if (fd >= 0)
		{
			/* Dump it */
			fd_write(fd, (cptr)head, head->head_size);

			/* Dump the "*_info" array */
			fd_write(fd, head->info_ptr, head->info_size);

			/* Dump the "*_name" array */
			fd_write(fd, head->name_ptr, head->name_size);

			/* Dump the "*_text" array */
			fd_write(fd, head->text_ptr, head->text_size);

			/* Close */
			fd_close(fd);
		}


		/*** Kill the fake arrays ***/

		/* Free the "*_info" array */
		KILL(head->info_ptr);

		/* Hack -- Free the "fake" arrays */
		if (name)
			KILL(head->name_ptr);

		if (text)
			KILL(head->text_ptr);

#endif /* ALLOW_TEMPLATES */


		/*** Load the binary image file ***/

		/* Build the filename */
		path_make(buf, ANGBAND_DIR_DATA, format("%s.raw", filename));

		/* Attempt to open the "raw" file */
		fd = fd_open(buf, O_RDONLY);

		/* Process existing "raw" file */
		if (fd < 0) quit_fmt("Cannot load '%s.raw' file.", filename);

		/* Attempt to parse the "raw" file */
		err = init_info_raw(fd, head);

		/* Close it */
		fd_close(fd);

		/* Error */
		if (err) quit_fmt("Cannot parse '%s.raw' file.", filename);

#ifdef ALLOW_TEMPLATES
	}
#endif /* ALLOW_TEMPLATES */

	if (info) (*info) = head->info_ptr;
	if (name) (*name) = head->name_ptr;
	if (text) (*text) = head->text_ptr;

	/* Success */
	return (0);
}


/*
 * Free the allocated memory for the info-, name-, and text- arrays.
 */
static errr free_info(header *head)
{
#ifdef HAVE_MMAP
	if (head->mmap_base)
	{
		munmap(head->mmap_base, sizeof(header) + head->info_size +
			head->name_size + head->text_size);

		/* Success */
		return (0);
	}
#endif /* HAVE_MMAP */

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
	/* Init the header */
	init_header(&z_head, 1, sizeof(maxima));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	z_head.parse_info_txt = parse_z_info;

#endif /* ALLOW_TEMPLATES */

	return init_info("misc", &z_head, (void *)&z_info, NULL, NULL);
}


/*
 * Initialize the "f_info" array
 */
static errr init_f_info(void)
{
	/* Init the header */
	init_header(&f_head, z_info->f_max, sizeof(feature_type));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	f_head.parse_info_txt = parse_f_info;

#endif /* ALLOW_TEMPLATES */

	return init_info("f_info", &f_head,
					 (void *)&f_info, (void *)&f_name, (void *)&f_text);
}



/*
 * Initialize the "k_info" array
 */
static errr init_k_info(void)
{
	/* Init the header */
	init_header(&k_head, z_info->k_max, sizeof(object_kind));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	k_head.parse_info_txt = parse_k_info;

#endif /* ALLOW_TEMPLATES */

	return init_info("k_info", &k_head,
					 (void *)&k_info, (void *)&k_name, (void *)&k_text);
}



/*
 * Initialize the "a_info" array
 */
static errr init_a_info(void)
{
	/* Init the header */
	init_header(&a_head, z_info->a_max, sizeof(artifact_type));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	a_head.parse_info_txt = parse_a_info;

#endif /* ALLOW_TEMPLATES */

	return init_info("a_info", &a_head,
					 (void *)&a_info, (void *)&a_name, (void *)&a_text);
}



/*
 * Initialize the "e_info" array
 */
static errr init_e_info(void)
{
	/* Init the header */
	init_header(&e_head, z_info->e_max, sizeof(ego_item_type));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	e_head.parse_info_txt = parse_e_info;

#endif /* ALLOW_TEMPLATES */

	return init_info("e_info", &e_head,
					 (void *)&e_info, (void *)&e_name, (void *)&e_text);
}



/*
 * Initialize the "r_info" array
 */
static errr init_r_info(void)
{
	/* Init the header */
	init_header(&r_head, z_info->r_max, sizeof(monster_race));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	r_head.parse_info_txt = parse_r_info;

#endif /* ALLOW_TEMPLATES */

	/* Create the array for hero race mapping */
	C_MAKE(h_list, z_info->h_max, hero_type);

	return init_info("r_info", &r_head,
					 (void *)&r_info, (void *)&r_name, (void *)&r_text);
}



/*
 * Initialize the "v_info" array
 */
static errr init_v_info(void)
{
	/* Init the header */
	init_header(&v_head, z_info->v_max, sizeof(vault_type));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	v_head.parse_info_txt = parse_v_info;

#endif /* ALLOW_TEMPLATES */

	return init_info("v_info", &v_head,
					 (void *)&v_info, (void *)&v_name, (void *)&v_text);
}



static errr init_s_info(void)
{
	errr rv;
	spell_type *sp_ptr;
	char * s_text;
	int i, j, c;

	/* Init the header */
	init_header(&s_head, NUM_REALMS * NUM_SPELLS, sizeof(spell_type));

#ifdef ALLOW_TEMPLATES

	/* Save a pointer to the parsing function */
	s_head.parse_info_txt = parse_s_info;

#endif /* ALLOW_TEMPLATES */

	rv = init_info("s_info", &s_head, (void *)&s_tmp_info, (void *)&s_name, (void *)&s_text);

	/* If there's an error, return now. */
	if (rv) return (rv);

	for (i = 0; i < NUM_REALMS; i++)
	{
		for (j = 0; j < NUM_SPELLS; j++)
		{
			sp_ptr = &s_tmp_info[(i*NUM_SPELLS)+j];

			s_info[i][j].sval = sp_ptr->sval;
			s_info[i][j].name = sp_ptr->name;
			s_info[i][j].s_idx = sp_ptr->s_idx;
			s_info[i][j].realm = sp_ptr->realm;

			/* Hack: avoid listing bad spells, if any */
			if (!s_info[i][j].name)
				s_info[i][j].sval = 99;

			for (c = 0; c < MAX_CLASS; c++)
			{
				s_info[i][j].info[c].slevel = sp_ptr->info[c].slevel;
				s_info[i][j].info[c].smana = sp_ptr->info[c].smana;
				s_info[i][j].info[c].power = sp_ptr->info[c].power;

				/* Hack: handle default to unlearnable */
				if (!s_info[i][j].info[c].slevel)
					s_info[i][j].info[c].slevel = 99;
			}
		}
	}

	/* Success */
	return (0);
}

/*** Initialize others ***/




/*
 * Initialize the "wild_choice_tree" array and the
 * "wild_gen_data" array.
 *
 */
errr init_w_info(void)
{
	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/* Later must add in raw file support later. */
	C_MAKE(wild_choice_tree, z_info->wn_max, wild_choice_tree_type);
	C_MAKE(wild_gen_data, z_info->wt_max, wild_gen_data_type);

	/*** Load the ascii template file ***/

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_EDIT, "w_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'w_info.txt' file.");

	/* Parse the file */
	err = init_w_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops =
			(((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

		/* Oops */
		msgf("Error %d at line %d of 'w_info.txt'.", err, error_line);
		msgf("Record %d contains a '%s' error.", error_idx, oops);
		msgf("Parsing '%s'.", buf);
		message_flush();

		/* Quit */
		quit("Error in 'w_info.txt' file.");
	}

	/* Success */
	return (0);
}

/*
 * Initialize the field data structures
 */
errr init_t_info(void)
{
	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];

	/* Later must add in python support. */
	C_MAKE(t_info, z_info->t_max, field_thaum);
	C_MAKE(fld_list, z_info->fld_max, field_type);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_EDIT, "t_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 't_info.txt' file.");

	/* Parse the file */
	err = init_t_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops =
			(((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

		/* Oops */
		msgf("Error %d at line %d of 't_info.txt'.", err, error_line);
		msgf("Record %d contains a '%s' error.", error_idx, oops);
		msgf("Parsing '%s'.", buf);
		message_flush();

		/* Quit */
		quit("Error in 't_info.txt' file.");
	}

	/* Success */
	return (0);
}

/*
 * Initialize the field data structures
 */
static errr init_mg_info(void)
{
	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];

	C_MAKE(mg_info, z_info->mg_max, monster_group_type);

	/*** Load the ascii template file ***/

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_EDIT, "mg_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'mg_info.txt' file.");



	/* Parse the file */
	err = init_mg_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops =
			(((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

		/* Oops */
		msgf("Error %d at line %d of 'mg_info.txt'.", err, error_line);
		msgf("Record %d contains a '%s' error.", error_idx, oops);
		msgf("Parsing '%s'.", buf);
		message_flush();

		/* Quit */
		quit("Error in 't_info.txt' file.");
	}

	/* Success */
	return (0);
}

/*
 * The list of available format functions
 *
 * (They should be in order of most-called
 *  through to least-called.)
 */
static vstrnfmt_aux_func my_format_functions[9] =
{
	set_message_type,
	object_fmt,
	object_store_fmt,
	monster_fmt,
	stat_format,
	center_string,
	likert,
	binary_fmt,
	NULL
};


/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
	int i, j, k, n;

	/*** Pre-allocate space for the "format()" buffer ***/

	/* Hack -- Just call the "format()" function */
	(void)format("%s (%s).", "Steven Fuerst", MAINTAINER);

	/* Initialise the "%v" user-defined format function list */
	register_format_funcs(my_format_functions);


	/*** Prepare the various "bizarre" arrays ***/

	/* Macro variables */
	C_MAKE(macro__pat, MACRO_MAX, cptr);
	C_MAKE(macro__act, MACRO_MAX, cptr);
	C_MAKE(macro__cmd, MACRO_MAX, bool);

	/* Macro action buffer */
	C_MAKE(macro__buf, 1024, char);


	/* Clear the spell colour strings */
	(void)C_WIPE(gf_color, MAX_GF, cptr);


	/* Initialize the "quark" package */
	(void)quarks_init();

	/* Initialize the "message" package */
	(void)messages_init();


	/*** Prepare region list ***/
	C_MAKE(rg_list, z_info->rg_max, region_type);
	C_MAKE(ri_list, z_info->rg_max, region_info);


	/*** Hack - Allocate the player information for each grid ***/
	for (i = 0; i < MAX_HGT; i++)
	{
		/* Allocate one row of the cave */
		C_MAKE(p_ptr->pcave[i], MAX_WID, pcave_type);
	}

	/*** Prepare wilderness stuff ***/

	/* Allocate temporary wilderness block */
	for (i = 0; i < WILD_BLOCK_SIZE + 1; i++)
	{
		/* Allocate one row of the temp_block */
		C_MAKE(temp_block[i], WILD_BLOCK_SIZE + 1, u16b);
	}

	/* Make the list of pointers to blocks */
	C_MAKE(wild_cache, WILD_CACHE, blk_ptr);

	/* Allocate each block */
	for (i = 0; i < WILD_CACHE; i++)
	{
		/* Allocate block */
		C_MAKE(wild_cache[i], WILD_BLOCK_SIZE, cave_type *);

		/* Allocate rows of a block */
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			C_MAKE(wild_cache[i][j], WILD_BLOCK_SIZE, cave_type);
		}
	}

	/* Allocate the player information for each grid (wilderness) */

	/* Allocate WILD_VIEW by WILD_VIEW blocks */
	C_MAKE(p_ptr->pwild, WILD_VIEW, pblk_ptr *);

	for (i = 0; i < WILD_VIEW; i++)
	{
		C_MAKE(p_ptr->pwild[i], WILD_VIEW, pblk_ptr);

		/* Allocate each block */
		for (j = 0; j < WILD_VIEW; j++)
		{
			C_MAKE(p_ptr->pwild[i][j], WILD_BLOCK_SIZE, pcave_type *);

			for (k = 0; k < WILD_BLOCK_SIZE; k++)
			{
				C_MAKE(p_ptr->pwild[i][j][k], WILD_BLOCK_SIZE, pcave_type);
			}
		}
	}

	/* Allocate the wilderness itself */
	C_MAKE(wild, WILD_SIZE, wild_type *);
	C_MAKE(wild_grid, WILD_SIZE, blk_ptr *);
	C_MAKE(wild_refcount, WILD_SIZE, int *);

	for (i = 0; i < WILD_SIZE; i++)
	{
		/* Allocate one row of the wilderness */
		C_MAKE(wild[i], WILD_SIZE, wild_type);
		C_MAKE(wild_grid[i], WILD_SIZE, blk_ptr);
		C_MAKE(wild_refcount[i], WILD_SIZE, int);
	}

	/*** Prepare "vinfo" array ***/

	/* Used by "update_view()" */
	(void)vinfo_init();


	/*** Prepare entity arrays ***/

	/* Objects */
	C_MAKE(o_list, z_info->o_max, object_type);

	/* Monsters */
	C_MAKE(m_list, z_info->m_max, monster_type);

	/*** Prepare the options ***/


	init_options(OPT_FLAG_BIRTH | OPT_FLAG_SERVER | OPT_FLAG_PLAYER);

	/* Initialize the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		if (option_info[i].o_text)
		{
			/* Accept */
			option_mask[i / 32] |= (1L << (i % 32));
		}
	}


	/* Initialize the window flags */
	for (n = 0; n < ANGBAND_TERM_MAX; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Accept */
			if (window_flag_desc[i])
			{
				/* Accept */
				window_mask[n] |= (1L << i);
			}
		}
	}


	/*** Make store stock cache ***/

	C_MAKE(store_cache, STORE_CACHE_AMNT, store_type *);

	/* Allocate the towns */
	C_MAKE(place, z_info->wp_max, place_type);

	/* Get size of shop owner name arrays */
	for (i = 0; owner_names[i]; i++)
	{
		/* Do nothing */
	}
	owner_names_max = i;

	for (i = 0; owner_suffix[i]; i++)
	{
		/* Do nothing */
	}
	owner_suffix_max = i;


	/* Success */
	return (0);
}

/*
 * Reinitialize allocation table
 */
void reinit_alloc(void)
{
	int i;
	monster_race *r_ptr;

	alloc_entry *table;
	s16b num[MAX_DEPTH];
	s16b aux[MAX_DEPTH];

	KILL(alloc_race_table);

	/*** Analyze monster allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(&num, MAX_DEPTH, s16b);

	/* Size of "alloc_race_table" */
	alloc_race_size = 0;

	/* Scan the monsters (not the ghost) */
	for (i = 1; i < RACE_MAX; i++)
	{
		/* Get the ith race */
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
		num[i] += num[i - 1];
	}

	/*** Initialize monster allocation info ***/

	/* Allocate the alloc_race_table */
	C_MAKE(alloc_race_table, alloc_race_size, alloc_entry);

	/* Get the table entry */
	table = alloc_race_table;

	/* Scan the monsters */
	for (i = 1; i < RACE_MAX; i++)
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
			y = (x > 0) ? num[x - 1] : 0;

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


/*
 * Initialize some other arrays
 */
static errr init_alloc(void)
{
	int i;
	monster_race *r_ptr;

	alloc_entry *table;
	s16b num[MAX_DEPTH];
	s16b aux[MAX_DEPTH];

	/*** Analyze monster allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(&num, MAX_DEPTH, s16b);

	/* Size of "alloc_race_table" */
	alloc_race_size = 0;

	/* Scan the monsters (not the ghost) */
	for (i = 1; i < z_info->r_max - 1; i++)
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
		num[i] += num[i - 1];
	}

	/* Paranoia */
	if (!num[0]) quit("No town monsters!");

	/*** Initialize monster allocation info ***/

	/* Allocate the alloc_race_table */
	C_MAKE(alloc_race_table, alloc_race_size, alloc_entry);

	/* Get the table entry */
	table = alloc_race_table;

	/* Scan the monsters */
	for (i = 1; i < HERO_MIN; i++)
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
			y = (x > 0) ? num[x - 1] : 0;

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

	/* Init "alloc_kind_table" and "alloc_ego_table" */
	(void)init_object_alloc();

	/* Success */
	return (0);
}


/*
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
	clear_row(23);
	put_fstr(20, 23, str);
	Term_fresh();
}



/*
 * Hack -- Explain a broken "lib" folder and quit (see below).
 *
 * XXX XXX XXX This function is "messy" because various things
 * may or may not be initialized, but the "plog()" and "quit()"
 * functions are "supposed" to work under any conditions.
 */
static void init_angband_fail(void)
{
	/* Explain */
	plog("The 'lib' directory is probably missing or broken.");

	/* More details */
	plog("Perhaps the archive was not extracted correctly.");

	/* Explain */
	plog("See the 'README' file for more information.");

	/* Quit with error */
	quit("Fatal Error.");
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
	int fd = -1;

	int mode = 0644;

	FILE *fp;

	char buf[1024];


	/*** Verify the "news" file ***/

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_FILE, "news.txt");

	/* Attempt to open the file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure */
	if (fd < 0)
	{
		/* Message */
		plog_fmt("Cannot access the '%s' file!", buf);

		/* Crash and burn */
		init_angband_fail();
	}

	/* Close it */
	(void)fd_close(fd);


	/*** Display the "news" file ***/

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_FILE, "news.txt");

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
			put_fstr(0, i++, buf);
		}

		/* Close */
		my_fclose(fp);
	}

	/* Display version number */
	put_fstr(42, 3, "%d.%d.%d", VER_MAJOR, VER_MINOR, VER_PATCH);

	/* Flush it */
	Term_fresh();


	/*** Verify (or create) the "high score" file ***/

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_APEX, "scores.raw");

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
			/* Message */
			plog_fmt("Cannot create the '%s' file!", buf);

			/* Crash and burn */
			init_angband_fail();
		}
	}

	/* Close it */
	(void)fd_close(fd);


	/*** Initialize some arrays ***/

	/* Init the interface callbacks */
	init_term_callbacks();

	/* Initialize size info */
	note("[Initializing array sizes...]");
	if (init_z_info()) quit("Cannot initialize sizes");

	/* Initialize scripting */
	note("[Initializing scripts... (scripts)]");
	if (script_init()) quit("Cannot initialize scripts");

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

	/* Initialize quest array */
	note("[Initializing arrays... (quests)]");
	if (init_quests()) quit("Cannot initialize quests");

	/* Initialize some other arrays */
	note("[Initializing arrays... (other)]");
	if (init_other()) quit("Cannot initialize other stuff");

	/* Initialize some other arrays */
	note("[Initializing arrays... (alloc)]");
	if (init_alloc()) quit("Cannot initialize alloc stuff");

	/* Initialize monster group array */
	note("[Initializing arrays... (groups)]");
	if (init_mg_info()) quit("Cannot initialize monster groups");

	/* Initialize spell info */
	note("[Initializing arrays... (spells)]");
	if (init_s_info()) quit("Cannot initialize spells");

	/*** Load default user pref files ***/

	/* Initialize feature info */
	note("[Initializing user pref files...]");

	/* Access the "basic" pref file */
	(void)process_pref_file("pref.prf");

	/* Access the "user" pref file */
	(void)process_pref_file("user.prf");

	/* Initialise the fake monochrome flag */
	fake_monochrome = (!use_graphics
					   || streq(ANGBAND_SYS, "ibm")) ? TRUE : FALSE;

	/* Initialise the overhead map */
	init_overhead_map();

	/* Done */
	note("[Initialization complete]");
}




void cleanup_angband(void)
{
	int i, j;

	/* Free the macros */
	for (i = 0; i < macro__num; ++i)
	{
		string_free(macro__pat[i]);
		string_free(macro__act[i]);
	}

	FREE((void *)macro__pat);
	FREE((void *)macro__act);

	/* Free the keymaps */
	for (i = 0; i < KEYMAP_MODES; ++i)
	{
		for (j = 0; j < 256; ++j)
		{
			string_free(keymap_act[i][j]);
		}
	}

	/* Free the allocation tables */
	FREE(alloc_ego_table);
	FREE(alloc_race_table);
	FREE(alloc_kind_table);

	/* Free the towns */
	FREE(place);

	/* Free the stores */
	FREE(store_cache);

	/* Free the quest list */
	FREE(quest);

	/* Free the lore, monster, and object lists */
	FREE(m_list);
	FREE(o_list);

#ifdef MONSTER_FLOW

	/* Flow arrays */
	FREE(cave_when);
	FREE(cave_cost);

#endif /* MONSTER_FLOW */

	/* Delete the overhead map */
	del_overhead_map();

/*
 * Note that this causes problems if Zangband exits due to an error
 * parsing the info files since at that point the wilderness is not
 * initiated. It works fine thereafter.
 */
#if 0

	This code is wrong - the wilderness works differently now. - SF -
		/* Free the wilderness */
	for (i = 0; i < WILD_SIZE; i++)
	{
		/* Free one row of the wilderness */
		FREE(wild[i]);
	}

	/* Free the wilderness itself */
	FREE(wild);


	/* Free cache of wilderness blocks */
	for (i = 0; i < WILD_BLOCKS; i++)
	{
		/* Free rows of a block */
		for (j = 0; j < WILD_BLOCK_SIZE; j++)
		{
			FREE(wild_cache[i][j]);
		}

		/* Free block */
		FREE(wild_cache[i]);
	}

	/* Free temporary wilderness block */
	for (i = 0; i < WILD_BLOCK_SIZE + 1; i++)
	{
		/* Allocate one row of the temp_block */
		FREE(temp_block[i]);
	}

	/* Free the cave */
	for (i = 0; i < MAX_HGT; i++)
	{
		/* Allocate one row of the cave */
		FREE(cave[i]);
	}
#endif /* 0 */

	/* Free the messages */
	messages_free();

	/* Free the "quarks" */
	quarks_free();

	/* Free the info, name, and text arrays */
	free_info(&v_head);
	free_info(&r_head);
	free_info(&e_head);
	free_info(&a_head);
	free_info(&k_head);
	free_info(&f_head);
	free_info(&z_head);

	/* Free the interface callbacks */
	free_term_callbacks();

	/* Free the directories */
	string_free(ANGBAND_DIR);
	string_free(ANGBAND_DIR_APEX);
	string_free(ANGBAND_DIR_BONE);
	string_free(ANGBAND_DIR_DATA);
	string_free(ANGBAND_DIR_EDIT);
	string_free(ANGBAND_DIR_SCRIPT);
	string_free(ANGBAND_DIR_FILE);
	string_free(ANGBAND_DIR_HELP);
	string_free(ANGBAND_DIR_INFO);
	string_free(ANGBAND_DIR_SAVE);
	string_free(ANGBAND_DIR_PREF);
	string_free(ANGBAND_DIR_USER);
	string_free(ANGBAND_DIR_XTRA);
}
