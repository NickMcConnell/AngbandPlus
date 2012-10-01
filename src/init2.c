/* File: init2.c */

/* Paths, initializiation of *_info arrays from the binary files, control
 * of what items are sold in the stores, prepare stores, inventory, and
 * many other things, some error text, startup initializations.
 *
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


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

	/* Build a path name */
	strcpy(tail, "xtra");
	ANGBAND_DIR_XTRA = string_make(path);

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



#ifdef ALLOW_TEMPLATES


/*
 * Hack -- help give useful error messages
 */
s16b error_idx;
s16b error_line;

/*
 * Standard error message text
 */
static cptr err_str[8] =
{
	NULL,
	"parse error",
	"obsolete file",
	"missing record header",
	"non-sequential records",
	"invalid flag specification",
	"undefined directive",
	"out of memory"
};


#endif

/*** Initialize from binary image files ***/


/*
 * Initialize the "f_info" array, by parsing a binary "image" file
 */
static errr init_f_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != f_head->v_major) ||
	    (test.v_minor != f_head->v_minor) ||
	    (test.v_patch != f_head->v_patch) ||
	    (test.v_extra != f_head->v_extra) ||
	    (test.info_num != f_head->info_num) ||
	    (test.info_len != f_head->info_len) ||
	    (test.head_size != f_head->head_size) ||
	    (test.info_size != f_head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	(*f_head) = test;


	/* Allocate the "f_info" array */
	C_MAKE(f_info, f_head->info_num, feature_type);

	/* Read the "f_info" array */
	fd_read(fd, (char*)(f_info), f_head->info_size);


	/* Allocate the "f_name" array */
	C_MAKE(f_name, f_head->name_size, char);

	/* Read the "f_name" array */
	fd_read(fd, (char*)(f_name), f_head->name_size);


#ifndef DELAY_LOAD_F_TEXT

	/* Allocate the "f_text" array */
	C_MAKE(f_text, f_head->text_size, char);

	/* Read the "f_text" array */
	fd_read(fd, (char*)(f_text), f_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "f_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_f_info(void)
{
	int fd;

	int mode = 0644;

	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(f_head, header);

	/* Save the "version" */
	f_head->v_major = O_VERSION_MAJOR;
	f_head->v_minor = O_VERSION_MINOR;
	f_head->v_patch = O_VERSION_PATCH;
	f_head->v_extra = 0;

	/* Save the "record" information */
	f_head->info_num = MAX_F_IDX;
	f_head->info_len = sizeof(feature_type);

	/* Save the size of "f_head" and "f_info" */
	f_head->head_size = sizeof(header);
	f_head->info_size = f_head->info_num * f_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "terrain.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, "terrain.txt");

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
		       err = init_f_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'terrain.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "f_info" array */
	C_MAKE(f_info, f_head->info_num, feature_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(f_name, FAKE_NAME_SIZE, char);
	C_MAKE(f_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "terrain.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'terrain.txt' file.");

	/* Parse the file */
	err = init_f_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d at line %d of 'terrain.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'terrain.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "terrain.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
		/* Dump it */
		fd_write(fd, (char*)(f_head), f_head->head_size);

		/* Dump the "f_info" array */
		fd_write(fd, (char*)(f_info), f_head->info_size);

		/* Dump the "f_name" array */
		fd_write(fd, (char*)(f_name), f_head->name_size);

		/* Dump the "f_text" array */
		fd_write(fd, (char*)(f_text), f_head->text_size);

		/* Close */
		fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "f_info" array */
	C_KILL(f_info, f_head->info_num, feature_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(f_name, FAKE_NAME_SIZE, char);
	C_KILL(f_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "terrain.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'terrain.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_f_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'terrain.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "k_info" array, by parsing a binary "image" file
 */
static errr init_k_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != k_head->v_major) ||
	    (test.v_minor != k_head->v_minor) ||
	    (test.v_patch != k_head->v_patch) ||
	    (test.v_extra != k_head->v_extra) ||
	    (test.info_num != k_head->info_num) ||
	    (test.info_len != k_head->info_len) ||
	    (test.head_size != k_head->head_size) ||
	    (test.info_size != k_head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	(*k_head) = test;


	/* Allocate the "k_info" array */
	C_MAKE(k_info, k_head->info_num, object_kind);

	/* Read the "k_info" array */
	fd_read(fd, (char*)(k_info), k_head->info_size);


	/* Allocate the "k_name" array */
	C_MAKE(k_name, k_head->name_size, char);

	/* Read the "k_name" array */
	fd_read(fd, (char*)(k_name), k_head->name_size);


#ifndef DELAY_LOAD_K_TEXT

	/* Allocate the "k_text" array */
	C_MAKE(k_text, k_head->text_size, char);

	/* Read the "k_text" array */
	fd_read(fd, (char*)(k_text), k_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "k_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_k_info(void)
{
	int fd;

	int mode = 0644;

	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(k_head, header);

	/* Save the "version" */
	k_head->v_major = O_VERSION_MAJOR;
	k_head->v_minor = O_VERSION_MINOR;
	k_head->v_patch = O_VERSION_PATCH;
	k_head->v_extra = 0;

	/* Save the "record" information */
	k_head->info_num = MAX_K_IDX;
	k_head->info_len = sizeof(object_kind);

	/* Save the size of "k_head" and "k_info" */
	k_head->head_size = sizeof(header);
	k_head->info_size = k_head->info_num * k_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "object.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, "object.txt");

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
			err = init_k_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'object.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "k_info" array */
	C_MAKE(k_info, k_head->info_num, object_kind);

	/* Hack -- make "fake" arrays */
	C_MAKE(k_name, FAKE_NAME_SIZE, char);
	C_MAKE(k_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "object.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'object.txt' file.");

	/* Parse the file */
	err = init_k_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d at line %d of 'object.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'object.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "object.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
		/* Dump it */
		fd_write(fd, (char*)(k_head), k_head->head_size);

		/* Dump the "k_info" array */
		fd_write(fd, (char*)(k_info), k_head->info_size);

		/* Dump the "k_name" array */
		fd_write(fd, (char*)(k_name), k_head->name_size);

		/* Dump the "k_text" array */
		fd_write(fd, (char*)(k_text), k_head->text_size);

		/* Close */
		fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "k_info" array */
	C_KILL(k_info, k_head->info_num, object_kind);

	/* Hack -- Free the "fake" arrays */
	C_KILL(k_name, FAKE_NAME_SIZE, char);
	C_KILL(k_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "object.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'object.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_k_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'object.raw' file.");

	/* Success */
	return (0);
}

/*
* Initialize the "h_info" array, by parsing a binary "image" file
*/
static errr init_h_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	   (test.v_major != h_head->v_major) ||
	   (test.v_minor != h_head->v_minor) ||
	   (test.v_patch != h_head->v_patch) ||
	   (test.v_extra != h_head->v_extra) ||
	   (test.info_num != h_head->info_num) ||
	   (test.info_len != h_head->info_len) ||
	   (test.head_size != h_head->head_size) ||
	   (test.info_size != h_head->info_size))
	{
	      /* Error */
	      return (-1);
	}


	/* Accept the header */
	(*h_head) = test;


	/* Allocate the "h_info" array */
	C_MAKE(h_info, h_head->info_num, hist_type);

	/* Read the "h_info" array */
	fd_read(fd, (char*)(h_info), h_head->info_size);


	/* Allocate the "h_text" array */
	C_MAKE(h_text, h_head->text_size, char);

	/* Read the "h_text" array */
	fd_read(fd, (char*)(h_text), h_head->text_size);


	/* Success */
	return (0);
}



/*
 * Initialize the "h_info" array
 */
static errr init_h_info(void)
{
	int fd;

	int mode = 0644;

	errr err = 0;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(h_head, header);

	/* Save the "version" */
	h_head->v_major = O_VERSION_MAJOR;
	h_head->v_minor = O_VERSION_MINOR;
	h_head->v_patch = O_VERSION_PATCH;
	h_head->v_extra = 0;

	/* Save the "record" information */
	h_head->info_num = MAX_H_IDX;
	h_head->info_len = sizeof(hist_type);

	/* Save the size of "h_head" and "h_info" */
	h_head->head_size = sizeof(header);
	h_head->info_size = h_head->info_num * h_head->info_len;

#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_hist.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

	      err = check_modification_date(fd, "p_hist.txt");

#endif /* CHECK_MODIFICATION_TIME */

	      /* Attempt to parse the "raw" file */
	      if (!err)
		     err = init_h_info_raw(fd);

	      /* Close it */
	      fd_close(fd);

	      /* Success */
	      if (!err) return (0);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "h_info" array */
	C_MAKE(h_info, h_head->info_num, hist_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(h_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "p_hist.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'p_hist.txt' file.");

	/* Parse the file */
	err = init_h_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
	      cptr oops;

	      /* Error string */
	      oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

	      /* Oops */
	      msg_format("Error %d at line %d of 'p_hist.txt'.", err, error_line);
	      msg_format("Record %d contains a '%s' error.", error_idx, oops);
	      msg_format("Parsing '%s'.", buf);
	      msg_print(NULL);

	      /* Quit */
	      quit("Error in 'p_hist.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_hist.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
	      /* Dump it */
	      fd_write(fd, (char*)(h_head), h_head->head_size);

	      /* Dump the "h_info" array */
	      fd_write(fd, (char*)(h_info), h_head->info_size);

	      /* Dump the "h_text" array */
	      fd_write(fd, (char*)(h_text), h_head->text_size);

	      /* Close */
	      fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "h_info" array */
	C_KILL(h_info, h_head->info_num, hist_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(h_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_hist.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'p_hist.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_h_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'p_hist.raw' file.");

	/* Success */
	return (0);
}


/*
 * Initialize the "b_info" array, by parsing a binary "image" file
 */
static errr init_b_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	   (test.v_major != b_head->v_major) ||
	   (test.v_minor != b_head->v_minor) ||
	   (test.v_patch != b_head->v_patch) ||
	   (test.v_extra != b_head->v_extra) ||
	   (test.info_num != b_head->info_num) ||
	   (test.info_len != b_head->info_len) ||
	   (test.head_size != b_head->head_size) ||
	   (test.info_size != b_head->info_size))
	{
	      /* Error */
	      return (-1);
	}


	/* Accept the header */
	(*b_head) = test;


	/* Allocate the "b_info" array */
	C_MAKE(b_info, b_head->info_num, owner_type);

	/* Read the "b_info" array */
	fd_read(fd, (char*)(b_info), b_head->info_size);


	/* Allocate the "b_name" array */
	C_MAKE(b_name, b_head->name_size, char);

	/* Read the "b_name" array */
	fd_read(fd, (char*)(b_name), b_head->name_size);


	/* Success */
	return (0);
}



/*
 * Initialize the "b_info" array
 */
static errr init_b_info(void)
{
	int fd;

	int mode = 0644;

	errr err = 0;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(b_head, header);

	/* Save the "version" */
	b_head->v_major = O_VERSION_MAJOR;
	b_head->v_minor = O_VERSION_MINOR;
	b_head->v_patch = O_VERSION_PATCH;
	b_head->v_extra = O_VERSION_EXTRA;

	/* Save the "record" information */
	b_head->info_num = MAX_STORES * MAX_B_IDX;
	b_head->info_len = sizeof(owner_type);

	/* Save the size of "b_head" and "b_info" */
	b_head->head_size = sizeof(header);
	b_head->info_size = b_head->info_num * b_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "cost_adj.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

	      err = check_modification_date(fd, "shop_own.txt");

#endif /* CHECK_MODIFICATION_TIME */

	      /* Attempt to parse the "raw" file */
	      if (!err)
		     err = init_b_info_raw(fd);

	      /* Close it */
	      fd_close(fd);

	      /* Success */
	      if (!err) return (0);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "b_info" array */
	C_MAKE(b_info, b_head->info_num, owner_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(b_name, FAKE_NAME_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "shop_own.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'shop_own.txt' file.");

	/* Parse the file */
	err = init_b_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
	      cptr oops;

	      /* Error string */
	      oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

	      /* Oops */
	      msg_format("Error %d at line %d of 'shop_own.txt'.", err, error_line);
	      msg_format("Record %d contains a '%s' error.", error_idx, oops);
	      msg_format("Parsing '%s'.", buf);
	      msg_print(NULL);

	      /* Quit */
	      quit("Error in 'shop_own.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "cost_adj.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
	      /* Dump it */
	      fd_write(fd, (char*)(b_head), b_head->head_size);

	      /* Dump the "b_info" array */
	      fd_write(fd, (char*)(b_info), b_head->info_size);

	      /* Dump the "b_name" array */
	      fd_write(fd, (char*)(b_name), b_head->name_size);

	      /* Close */
	      fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "b_info" array */
	C_KILL(b_info, b_head->info_num, owner_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(b_name, FAKE_NAME_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "cost_adj.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'cost_adj.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_b_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'cost_adj.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "g_info" array, by parsing a binary "image" file
 */
static errr init_g_info_raw(int fd)
{
	header test;

	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	   (test.v_major != g_head->v_major) ||
	   (test.v_minor != g_head->v_minor) ||
	   (test.v_patch != g_head->v_patch) ||
	   (test.v_extra != g_head->v_extra) ||
	   (test.info_num != g_head->info_num) ||
	   (test.info_len != g_head->info_len) ||
	   (test.head_size != g_head->head_size) ||
	   (test.info_size != g_head->info_size))
	{
	      /* Error */
	      return (-1);
	}


	/* Accept the header */
	(*g_head) = test;


	/* Allocate the "g_info" array */
	C_MAKE(g_info, g_head->info_num, byte);

	/* Read the "g_info" array */
	fd_read(fd, (char*)(g_info), g_head->info_size);


	/* Success */
	return (0);
}



/*
 * Initialize the "g_info" array
 */
static errr init_g_info(void)
{
	int fd;

	int mode = 0644;

	errr err = 0;

	FILE *fp;


	/* General buffer */
	char buf[1024];


	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(g_head, header);

	/* Save the "version" */
	g_head->v_major = O_VERSION_MAJOR;
	g_head->v_minor = O_VERSION_MINOR;
	g_head->v_patch = O_VERSION_PATCH;
	g_head->v_extra = O_VERSION_EXTRA;

	/* Save the "record" information */
	g_head->info_num = (MAX_P_IDX * MAX_P_IDX);
	g_head->info_len = sizeof(byte);

	/* Save the size of "g_head" and "g_info" */
	g_head->head_size = sizeof(header);
	g_head->info_size = g_head->info_num * g_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "shop_own.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

	      err = check_modification_date(fd, "cost_adj.txt");

#endif /* CHECK_MODIFICATION_TIME */

	      /* Attempt to parse the "raw" file */
	      if (!err)
		      err = init_g_info_raw(fd);

	      /* Close it */
	      fd_close(fd);

	      /* Success */
	      if (!err) return (0);
	}


	/* Allocate the "g_info" array */
	C_MAKE(g_info, g_head->info_num, byte);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "cost_adj.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'cost_adj.txt' file.");

	/* Parse the file */
	err = init_g_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
	      cptr oops;

	      /* Error string */
	      oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

	      /* Oops */
	      msg_format("Error %d at line %d of 'cost_adj.txt'.", err, error_line);
	      msg_format("Record %d contains a '%s' error.", error_idx, oops);
	      msg_format("Parsing '%s'.", buf);
	      msg_print(NULL);

	      /* Quit */
	      quit("Error in 'cost_adj.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "shop_own.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
	      /* Dump it */
	      fd_write(fd, (char*)(g_head), g_head->head_size);

	      /* Dump the "g_info" array */
	      fd_write(fd, (char*)(g_info), g_head->info_size);

	      /* Close */
	      fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "g_info" array */
	C_KILL(g_info, g_head->info_num, byte);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "shop_own.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'shop_own.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_g_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'shop_own.raw' file.");

	/* Success */
	return (0);
}







/*
 * In Angband, all of the "special" artifacts are first in the
 * artifact list. The first non-special artifact is at ART_MIN_NORMAL.
 * OAngband adds several new special artifacts greater than ART_MIN_NORMAL.
 * As a further catch, Morgoth's Crown and Hammer are special artifacts
 * greater than ART_MIN_NORMAL which should not be created until Morgoth
 * is defeated. -TNB-
 */

/* Lists of normal and special a_info[] indexes */
int *artifact_normal, *artifact_special;
int artifact_normal_cnt, artifact_special_cnt;

/*
 * This routine separates all the normal and special artifacts into
 * separate lists for easy allocation later.
 */
void init_artifacts(void)
{
	int loop;

	/* First: count. Second: build lists */
	for (loop = 0; loop <= 1; loop++)
	{
		int a_idx;

		artifact_normal_cnt = 0;
		artifact_special_cnt = 0;

		/* Check every artifact (including randoms) */
		for (a_idx = 1; a_idx < MAX_A_IDX; a_idx++)
		{
			/* Access this artifact */
			artifact_type *a_ptr = &a_info[a_idx];

			/* Require "real" artifacts */
			if (!a_ptr->name) continue;

			/* This is a "special" artifact */
			if ((a_idx < ART_MIN_NORMAL) ||
				(a_ptr->tval == TV_ROD) ||
				(a_ptr->tval == TV_STAFF) ||
				(a_ptr->tval == TV_WAND))
			{
				if (loop == 1)
				{
					artifact_special[artifact_special_cnt] = a_idx;
				}

				/* Count the special artifacts */
				artifact_special_cnt++;
			}

			/*
			 * This is a "normal" artifact. Notice we must skip
			 * Morgoth's Crown and Hammer.
			 */
			else if (!(a_ptr->flags3 & TR3_INSTA_ART))
			{
				if (loop == 1)
				{
					artifact_normal[artifact_normal_cnt] = a_idx;
				}

				/* Count the normal artifacts */
				artifact_normal_cnt++;
			}
		}

		/* Allocate the lists the first time through */
		if (loop == 0)
		{
			C_MAKE(artifact_normal, artifact_normal_cnt, int);
			C_MAKE(artifact_special, artifact_special_cnt, int);
		}
	}
}


/*
 * Initialize the "a_info" array, by parsing a binary "image" file
 */
static errr init_a_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != a_head->v_major) ||
	    (test.v_minor != a_head->v_minor) ||
	    (test.v_patch != a_head->v_patch) ||
	    (test.v_extra != a_head->v_extra) ||
	    (test.info_num != a_head->info_num) ||
	    (test.info_len != a_head->info_len) ||
	    (test.head_size != a_head->head_size) ||
	    (test.info_size != a_head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	(*a_head) = test;


	/* Allocate the "a_info" array */
	C_MAKE(a_info, a_head->info_num, artifact_type);

	/* Read the "a_info" array */
	fd_read(fd, (char*)(a_info), a_head->info_size);


	/* Allocate the "a_name" array */
	C_MAKE(a_name, a_head->name_size, char);

	/* Read the "a_name" array */
	fd_read(fd, (char*)(a_name), a_head->name_size);


#ifndef DELAY_LOAD_A_TEXT

	/* Allocate the "a_text" array */
	C_MAKE(a_text, a_head->text_size, char);

	/* Read the "a_text" array */
	fd_read(fd, (char*)(a_text), a_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "a_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_a_info(void)
{
	int fd;

	int mode = 0644;

	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the "header" ***/

	/* Allocate the "header" */
	MAKE(a_head, header);

	/* Save the "version" */
	a_head->v_major = O_VERSION_MAJOR;
	a_head->v_minor = O_VERSION_MINOR;
	a_head->v_patch = O_VERSION_PATCH;
	a_head->v_extra = 0;

	/* Save the "record" information */
	a_head->info_num = MAX_A_IDX;
	a_head->info_len = sizeof(artifact_type);

	/* Save the size of "a_head" and "a_info" */
	a_head->head_size = sizeof(header);
	a_head->info_size = a_head->info_num * a_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "artifact.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, "artifact.txt");

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
			err = init_a_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'artifact.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "a_info" array */
	C_MAKE(a_info, a_head->info_num, artifact_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(a_name, FAKE_NAME_SIZE, char);
	C_MAKE(a_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "artifact.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'artifact.txt' file.");

	/* Parse the file */
	err = init_a_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d at line %d of 'artifact.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'artifact.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "artifact.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
		/* Dump it */
		fd_write(fd, (char*)(a_head), a_head->head_size);

		/* Dump the "a_info" array */
		fd_write(fd, (char*)(a_info), a_head->info_size);

		/* Dump the "a_name" array */
		fd_write(fd, (char*)(a_name), a_head->name_size);

		/* Dump the "a_text" array */
		fd_write(fd, (char*)(a_text), a_head->text_size);

		/* Close */
		fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "a_info" array */
	C_KILL(a_info, a_head->info_num, artifact_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(a_name, FAKE_NAME_SIZE, char);
	C_KILL(a_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "artifact.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot open 'artifact.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_a_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'artifact.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "s_info" array, by parsing a binary "image" file
 */
static errr init_s_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != s_head->v_major) ||
	    (test.v_minor != s_head->v_minor) ||
	    (test.v_patch != s_head->v_patch) ||
	    (test.v_extra != s_head->v_extra) ||
	    (test.info_num != s_head->info_num) ||
	    (test.info_len != s_head->info_len) ||
	    (test.head_size != s_head->head_size) ||
	    (test.info_size != s_head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	(*s_head) = test;


	/* Allocate the "s_info" array */
	C_MAKE(s_info, s_head->info_num, set_type);

	/* Read the "s_info" array */
	fd_read(fd, (char*)(s_info), s_head->info_size);


	/* Allocate the "s_name" array */
	C_MAKE(s_name, s_head->name_size, char);

	/* Read the "s_name" array */
	fd_read(fd, (char*)(s_name), s_head->name_size);


#ifndef DELAY_LOAD_S_TEXT

	/* Allocate the "a_text" array */
	C_MAKE(s_text, s_head->text_size, char);

	/* Read the "s_text" array */
	fd_read(fd, (char*)(s_text), s_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "s_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_s_info(void)
{
	int fd;

	int mode = 0644;

	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the "header" ***/

	/* Allocate the "header" */
	MAKE(s_head, header);

	/* Save the "version" */
	s_head->v_major = O_VERSION_MAJOR;
	s_head->v_minor = O_VERSION_MINOR;
	s_head->v_patch = O_VERSION_PATCH;
	s_head->v_extra = 0;

	/* Save the "record" information */
	s_head->info_num = MAX_S_IDX;
	s_head->info_len = sizeof(set_type);

	/* Save the size of "a_head" and "a_info" */
	s_head->head_size = sizeof(header);
	s_head->info_size = s_head->info_num * s_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "set_item.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, "set_item.txt");

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
			err = init_s_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'set_item.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "a_info" array */
	C_MAKE(s_info, s_head->info_num, set_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(s_name, FAKE_NAME_SIZE, char);
	C_MAKE(s_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "set_item.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'set_item.txt' file.");

	/* Parse the file */
	err = init_s_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d at line %d of 'set_item.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'set_item.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "set_item.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
		/* Dump it */
		fd_write(fd, (char*)(s_head), s_head->head_size);

		/* Dump the "s_info" array */
		fd_write(fd, (char*)(s_info), s_head->info_size);

		/* Dump the "s_name" array */
		fd_write(fd, (char*)(s_name), s_head->name_size);

		/* Dump the "s_text" array */
		fd_write(fd, (char*)(s_text), s_head->text_size);

		/* Close */
		fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "s_info" array */
	C_KILL(s_info, s_head->info_num, set_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(s_name, FAKE_NAME_SIZE, char);
	C_KILL(s_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "set_item.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot open 'set_item.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_s_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'set_item.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "e_info" array, by parsing a binary "image" file
 */
static errr init_e_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != e_head->v_major) ||
	    (test.v_minor != e_head->v_minor) ||
	    (test.v_patch != e_head->v_patch) ||
	    (test.v_extra != e_head->v_extra) ||
	    (test.info_num != e_head->info_num) ||
	    (test.info_len != e_head->info_len) ||
	    (test.head_size != e_head->head_size) ||
	    (test.info_size != e_head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	(*e_head) = test;


	/* Allocate the "e_info" array */
	C_MAKE(e_info, e_head->info_num, ego_item_type);

	/* Read the "e_info" array */
	fd_read(fd, (char*)(e_info), e_head->info_size);


	/* Allocate the "e_name" array */
	C_MAKE(e_name, e_head->name_size, char);

	/* Read the "e_name" array */
	fd_read(fd, (char*)(e_name), e_head->name_size);


#ifndef DELAY_LOAD_E_TEXT

	/* Allocate the "e_text" array */
	C_MAKE(e_text, e_head->text_size, char);

	/* Read the "e_text" array */
	fd_read(fd, (char*)(e_text), e_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "e_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_e_info(void)
{
	int fd;

	int mode = 0644;

	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the "header" ***/

	/* Allocate the "header" */
	MAKE(e_head, header);

	/* Save the "version" */
	e_head->v_major = O_VERSION_MAJOR;
	e_head->v_minor = O_VERSION_MINOR;
	e_head->v_patch = O_VERSION_PATCH;
	e_head->v_extra = 0;

	/* Save the "record" information */
	e_head->info_num = MAX_E_IDX;
	e_head->info_len = sizeof(ego_item_type);

	/* Save the size of "e_head" and "e_info" */
	e_head->head_size = sizeof(header);
	e_head->info_size = e_head->info_num * e_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "ego_item.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, "ego_item.txt");

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
			err = init_e_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'ego_item.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "e_info" array */
	C_MAKE(e_info, e_head->info_num, ego_item_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(e_name, FAKE_NAME_SIZE, char);
	C_MAKE(e_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "ego_item.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'ego_item.txt' file.");

	/* Parse the file */
	err = init_e_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d at line %d of 'ego_item.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'ego_item.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "ego_item.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
		/* Dump it */
		fd_write(fd, (char*)(e_head), e_head->head_size);

		/* Dump the "e_info" array */
		fd_write(fd, (char*)(e_info), e_head->info_size);

		/* Dump the "e_name" array */
		fd_write(fd, (char*)(e_name), e_head->name_size);

		/* Dump the "e_text" array */
		fd_write(fd, (char*)(e_text), e_head->text_size);

		/* Close */
		fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "e_info" array */
	C_KILL(e_info, e_head->info_num, ego_item_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(e_name, FAKE_NAME_SIZE, char);
	C_KILL(e_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "ego_item.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'ego_item.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_e_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'ego_item.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "r_info" array, by parsing a binary "image" file
 */
static errr init_r_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != r_head->v_major) ||
	    (test.v_minor != r_head->v_minor) ||
	    (test.v_patch != r_head->v_patch) ||
	    (test.v_extra != r_head->v_extra) ||
	    (test.info_num != r_head->info_num) ||
	    (test.info_len != r_head->info_len) ||
	    (test.head_size != r_head->head_size) ||
	    (test.info_size != r_head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	(*r_head) = test;


	/* Allocate the "r_info" array */
	C_MAKE(r_info, r_head->info_num, monster_race);

	/* Read the "r_info" array */
	fd_read(fd, (char*)(r_info), r_head->info_size);


	/* Allocate the "r_name" array */
	C_MAKE(r_name, r_head->name_size, char);

	/* Read the "r_name" array */
	fd_read(fd, (char*)(r_name), r_head->name_size);


#ifndef DELAY_LOAD_R_TEXT

	/* Allocate the "r_text" array */
	C_MAKE(r_text, r_head->text_size, char);

	/* Read the "r_text" array */
	fd_read(fd, (char*)(r_text), r_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "r_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_r_info(void)
{
	int fd;

	int mode = 0644;

	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(r_head, header);

	/* Save the "version" */
	r_head->v_major = O_VERSION_MAJOR;
	r_head->v_minor = O_VERSION_MINOR;
	r_head->v_patch = O_VERSION_PATCH;
	r_head->v_extra = 0;

	/* Save the "record" information */
	r_head->info_num = MAX_R_IDX;
	r_head->info_len = sizeof(monster_race);

	/* Save the size of "r_head" and "r_info" */
	r_head->head_size = sizeof(header);
	r_head->info_size = r_head->info_num * r_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "monster.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, "monster.txt");

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
			err = init_r_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'monster.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "r_info" array */
	C_MAKE(r_info, r_head->info_num, monster_race);

	/* Hack -- make "fake" arrays */
	C_MAKE(r_name, FAKE_NAME_SIZE, char);
	C_MAKE(r_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "monster.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'monster.txt' file.");

	/* Parse the file (all monsters) */
	err = init_r_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d at line %d of 'monster.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'monster.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "monster.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
		/* Dump it */
		fd_write(fd, (char*)(r_head), r_head->head_size);

		/* Dump the "r_info" array */
		fd_write(fd, (char*)(r_info), r_head->info_size);

		/* Dump the "r_name" array */
		fd_write(fd, (char*)(r_name), r_head->name_size);

		/* Dump the "r_text" array */
		fd_write(fd, (char*)(r_text), r_head->text_size);

		/* Close */
		fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "r_info" array */
	C_KILL(r_info, r_head->info_num, monster_race);

	/* Hack -- Free the "fake" arrays */
	C_KILL(r_name, FAKE_NAME_SIZE, char);
	C_KILL(r_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "monster.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'monster.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_r_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'monster.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "v_info" array, by parsing a binary "image" file
 */
static errr init_v_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	    (test.v_major != v_head->v_major) ||
	    (test.v_minor != v_head->v_minor) ||
	    (test.v_patch != v_head->v_patch) ||
	    (test.v_extra != v_head->v_extra) ||
	    (test.info_num != v_head->info_num) ||
	    (test.info_len != v_head->info_len) ||
	    (test.head_size != v_head->head_size) ||
	    (test.info_size != v_head->info_size))
	{
		/* Error */
		return (-1);
	}


	/* Accept the header */
	(*v_head) = test;


	/* Allocate the "v_info" array */
	C_MAKE(v_info, v_head->info_num, vault_type);

	/* Read the "v_info" array */
	fd_read(fd, (char*)(v_info), v_head->info_size);


	/* Allocate the "v_name" array */
	C_MAKE(v_name, v_head->name_size, char);

	/* Read the "v_name" array */
	fd_read(fd, (char*)(v_name), v_head->name_size);


#ifndef DELAY_LOAD_V_TEXT

	/* Allocate the "v_text" array */
	C_MAKE(v_text, v_head->text_size, char);

	/* Read the "v_text" array */
	fd_read(fd, (char*)(v_text), v_head->text_size);

#endif

	/* Success */
	return (0);
}



/*
 * Initialize the "v_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_v_info(void)
{
	int fd;

	int mode = 0644;

	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];


	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(v_head, header);

	/* Save the "version" */
	v_head->v_major = O_VERSION_MAJOR;
	v_head->v_minor = O_VERSION_MINOR;
	v_head->v_patch = O_VERSION_PATCH;
	v_head->v_extra = 0;

	/* Save the "record" information */
	v_head->info_num = MAX_V_IDX;
	v_head->info_len = sizeof(vault_type);

	/* Save the size of "v_head" and "v_info" */
	v_head->head_size = sizeof(header);
	v_head->info_size = v_head->info_num * v_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "vault.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

		err = check_modification_date(fd, "vault.txt");

#endif /* CHECK_MODIFICATION_TIME */

		/* Attempt to parse the "raw" file */
		if (!err)
			err = init_v_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'vault.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "v_info" array */
	C_MAKE(v_info, v_head->info_num, vault_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(v_name, FAKE_NAME_SIZE, char);
	C_MAKE(v_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "vault.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'vault.txt' file.");

	/* Parse the file */
	err = init_v_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d at line %d of 'vault.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'vault.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "vault.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
		/* Dump it */
		fd_write(fd, (char*)(v_head), v_head->head_size);

		/* Dump the "v_info" array */
		fd_write(fd, (char*)(v_info), v_head->info_size);

		/* Dump the "v_name" array */
		fd_write(fd, (char*)(v_name), v_head->name_size);

		/* Dump the "v_text" array */
		fd_write(fd, (char*)(v_text), v_head->text_size);

		/* Close */
		fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "v_info" array */
	C_KILL(v_info, v_head->info_num, vault_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(v_name, FAKE_NAME_SIZE, char);
	C_KILL(v_text, FAKE_TEXT_SIZE, char);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "vault.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'vault.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_v_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'vault.raw' file.");

	/* Success */
	return (0);
}


/*
 * Initialize the "rp_info" array, by parsing a binary "image" file
 */
static errr init_rp_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	   (test.v_major != rp_head->v_major) ||
	   (test.v_minor != rp_head->v_minor) ||
	   (test.v_patch != rp_head->v_patch) ||
	   (test.v_extra != rp_head->v_extra) ||
	   (test.info_num != rp_head->info_num) ||
	   (test.info_len != rp_head->info_len) ||
	   (test.head_size != rp_head->head_size) ||
	   (test.info_size != rp_head->info_size))
	{
	      /* Error */
	      return (-1);
	}


	/* Accept the header */
	(*rp_head) = test;


	/* Allocate the "rp_info" array */
	C_MAKE(rp_info, rp_head->info_num, player_race);

	/* Read the "rp_info" array */
	fd_read(fd, (char*)(rp_info), rp_head->info_size);


	/* Allocate the "rp_name" array */
	C_MAKE(rp_name, rp_head->name_size, char);

	/* Read the "rp_name" array */
	fd_read(fd, (char*)(rp_name), rp_head->name_size);


#ifndef DELAY_LOAD_P_TEXT

	/* Allocate the "p_text" array */
	C_MAKE(rp_text, rp_head->text_size, char);

	/* Read the "p_text" array */
	fd_read(fd, (char*)(rp_text), rp_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "rp_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_rp_info(void)
{
	int fd;

	int mode = 0644;

	errr err = 0;

	FILE *fp;

	/* General buffer */
	char buf[1024];

	/*** Make the "header" ***/

	/* Allocate the "header" */
	MAKE(rp_head, header);

	/* Save the "version" */
	rp_head->v_major = O_VERSION_MAJOR;
	rp_head->v_minor = O_VERSION_MINOR;
	rp_head->v_patch = O_VERSION_PATCH;
	rp_head->v_extra = O_VERSION_EXTRA;

	/* Save the "record" information */
	rp_head->info_num = MAX_P_IDX;
	rp_head->info_len = sizeof(player_race);

	/* Save the size of "p_head" and "rp_info" */
	rp_head->head_size = sizeof(header);
	rp_head->info_size = rp_head->info_num * rp_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_race.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

	      err = check_modification_date(fd, "p_race.txt");

#endif /* CHECK_MODIFICATION_TIME */

	      /* Attempt to parse the "raw" file */
	      if (!err)
		     err = init_rp_info_raw(fd);

	      /* Close it */
	      fd_close(fd);

	      /* Success */
	      if (!err) return (0);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "rp_info" array */
	C_MAKE(rp_info, rp_head->info_num, player_race);

	/* Hack -- make "fake" arrays */
	C_MAKE(rp_name, FAKE_NAME_SIZE, char);
	C_MAKE(rp_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "p_race.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'p_race.txt' file.");

	/* Parse the file */
	err = init_rp_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
	      cptr oops;

	      /* Error string */
	      oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

	      /* Oops */
	      msg_format("Error %d at line %d of 'p_race.txt'.", err, error_line);
	      msg_format("Record %d contains a '%s' error.", error_idx, oops);
	      msg_format("Parsing '%s'.", buf);
	      msg_print(NULL);

	      /* Quit */
	      quit("Error in 'p_race.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_race.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
	      /* Dump it */
	      fd_write(fd, (char*)(rp_head), rp_head->head_size);

	      /* Dump the "rp_info" array */
	      fd_write(fd, (char*)(rp_info), rp_head->info_size);

	      /* Dump the "rp_name" array */
	      fd_write(fd, (char*)(rp_name), rp_head->name_size);

	      /* Dump the "rp_text" array */
	      fd_write(fd, (char*)(rp_text), rp_head->text_size);

	      /* Close */
	      fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "rp_info" array */
	C_KILL(rp_info, rp_head->info_num, player_race);

	/* Hack -- Free the "fake" arrays */
	C_KILL(rp_name, FAKE_NAME_SIZE, char);
	C_KILL(rp_text, FAKE_TEXT_SIZE, char);


#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_race.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot open 'p_race.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_rp_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'p_race.raw' file.");

	/* Success */
	return (0);
}


/*
 * Initialize the "cp_info" array, by parsing a binary "image" file
 */
static errr init_cp_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	   (test.v_major != cp_head->v_major) ||
	   (test.v_minor != cp_head->v_minor) ||
	   (test.v_patch != cp_head->v_patch) ||
	   (test.v_extra != cp_head->v_extra) ||
	   (test.info_num != cp_head->info_num) ||
	   (test.info_len != cp_head->info_len) ||
	   (test.head_size != cp_head->head_size) ||
	   (test.info_size != cp_head->info_size))
	{
	      /* Error */
	      return (-1);
	}


	/* Accept the header */
	(*cp_head) = test;


	/* Allocate the "cp_info" array */
	C_MAKE(cp_info, cp_head->info_num, player_class);

	/* Read the "cp_info" array */
	fd_read(fd, (char*)(cp_info), cp_head->info_size);


	/* Allocate the "cp_name" array */
	C_MAKE(cp_name, cp_head->name_size, char);

	/* Read the "cp_name" array */
	fd_read(fd, (char*)(cp_name), cp_head->name_size);


#ifndef DELAY_LOAD_CP_TEXT

	/* Allocate the "p_text" array */
	C_MAKE(cp_text, cp_head->text_size, char);

	/* Read the "p_text" array */
	fd_read(fd, (char*)(cp_text), cp_head->text_size);

#endif


	/* Success */
	return (0);
}



/*
 * Initialize the "cp_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_cp_info(void)
{
	int fd;

	int mode = 0644;

	errr err = 0;

	FILE *fp;

	/* General buffer */
	char buf[1024];

	/*** Make the "header" ***/

	/* Allocate the "header" */
	MAKE(cp_head, header);

	/* Save the "version" */
	cp_head->v_major = O_VERSION_MAJOR;
	cp_head->v_minor = O_VERSION_MINOR;
	cp_head->v_patch = O_VERSION_PATCH;
	cp_head->v_extra = O_VERSION_EXTRA;

	/* Save the "record" information */
	cp_head->info_num = MAX_CP_IDX;
	cp_head->info_len = sizeof(player_class);

	/* Save the size of "p_head" and "cp_info" */
	cp_head->head_size = sizeof(header);
	cp_head->info_size = cp_head->info_num * cp_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_class.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

	      err = check_modification_date(fd, "p_class.txt");

#endif /* CHECK_MODIFICATION_TIME */

	      /* Attempt to parse the "raw" file */
	      if (!err)
		     err = init_cp_info_raw(fd);

	      /* Close it */
	      fd_close(fd);

	      /* Success */
	      if (!err) return (0);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "cp_info" array */
	C_MAKE(cp_info, cp_head->info_num, player_class);

	/* Hack -- make "fake" arrays */
	C_MAKE(cp_name, FAKE_NAME_SIZE, char);
	C_MAKE(cp_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "p_class.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'p_class.txt' file.");

	/* Parse the file */
	err = init_cp_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
	      cptr oops;

	      /* Error string */
	      oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

	      /* Oops */
	      msg_format("Error %d at line %d of 'p_class.txt'.", err, error_line);
	      msg_format("Record %d contains a '%s' error.", error_idx, oops);
	      msg_format("Parsing '%s'.", buf);
	      msg_print(NULL);

	      /* Quit */
	      quit("Error in 'p_class.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_class.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
	      /* Dump it */
	      fd_write(fd, (char*)(cp_head), cp_head->head_size);

	      /* Dump the "cp_info" array */
	      fd_write(fd, (char*)(cp_info), cp_head->info_size);

	      /* Dump the "cp_name" array */
	      fd_write(fd, (char*)(cp_name), cp_head->name_size);

	      /* Dump the "cp_text" array */
	      fd_write(fd, (char*)(cp_text), cp_head->text_size);

	      /* Close */
	      fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "cp_info" array */
	C_KILL(cp_info, cp_head->info_num, player_class);

	/* Hack -- Free the "fake" arrays */
	C_KILL(cp_name, FAKE_NAME_SIZE, char);
	C_KILL(cp_text, FAKE_TEXT_SIZE, char);


#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_class.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot open 'p_class.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_cp_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'p_class.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "ch_info" array, by parsing a binary "image" file
 */
static errr init_ch_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	   (test.v_major != ch_head->v_major) ||
	   (test.v_minor != ch_head->v_minor) ||
	   (test.v_patch != ch_head->v_patch) ||
	   (test.v_extra != ch_head->v_extra) ||
	   (test.info_num != ch_head->info_num) ||
	   (test.info_len != ch_head->info_len) ||
	   (test.head_size != ch_head->head_size) ||
	   (test.info_size != ch_head->info_size))
	{
	      /* Error */
	      return (-1);
	}


	/* Accept the header */
	(*ch_head) = test;


	/* Allocate the "ch_info" array */
	C_MAKE(ch_info, ch_head->info_num, chest_drops);

	/* Read the "ch_info" array */
	fd_read(fd, (char*)(ch_info), ch_head->info_size);

	/* Success */
	return (0);
}



/*
 * Initialize the "ch_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_ch_info(void)
{
	int fd;

	int mode = 0644;

	errr err = 0;

	FILE *fp;

	/* General buffer */
	char buf[1024];

	/*** Make the "header" ***/

	/* Allocate the "header" */
	MAKE(ch_head, header);

	/* Save the "version" */
	ch_head->v_major = O_VERSION_MAJOR;
	ch_head->v_minor = O_VERSION_MINOR;
	ch_head->v_patch = O_VERSION_PATCH;
	ch_head->v_extra = O_VERSION_EXTRA;

	/* Save the "record" information */
	ch_head->info_num = MAX_CP_IDX;
	ch_head->info_len = sizeof(chest_drops);

	/* Save the size of "p_head" and "ch_info" */
	ch_head->head_size = sizeof(header);
	ch_head->info_size = ch_head->info_num * ch_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_chest.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

	      err = check_modification_date(fd, "p_chest.txt");

#endif /* CHECK_MODIFICATION_TIME */

	      /* Attempt to parse the "raw" file */
	      if (!err)
		     err = init_ch_info_raw(fd);

	      /* Close it */
	      fd_close(fd);

	      /* Success */
	      if (!err) return (0);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "ch_info" array */
	C_MAKE(ch_info, ch_head->info_num, chest_drops);

	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "p_chest.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'p_chest.txt' file.");

	/* Parse the file */
	err = init_ch_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
	      cptr oops;

	      /* Error string */
	      oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

	      /* Oops */
	      msg_format("Error %d at line %d of 'p_chest.txt'.", err, error_line);
	      msg_format("Record %d contains a '%s' error.", error_idx, oops);
	      msg_format("Parsing '%s'.", buf);
	      msg_print(NULL);

	      /* Quit */
	      quit("Error in 'p_chest.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_chest.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
	      /* Dump it */
	      fd_write(fd, (char*)(ch_head), ch_head->head_size);

	      /* Dump the "ch_info" array */
	      fd_write(fd, (char*)(ch_info), ch_head->info_size);

	      /* Close */
	      fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "ch_info" array */
	C_KILL(ch_info, ch_head->info_num, chest_drops);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_chest.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot open 'p_chest.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_ch_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'p_chest.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "mp_info" array, by parsing a binary "image" file
 */
static errr init_mp_info_raw(int fd)
{
	header test;


	/* Read and Verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
	   (test.v_major != mp_head->v_major) ||
	   (test.v_minor != mp_head->v_minor) ||
	   (test.v_patch != mp_head->v_patch) ||
	   (test.v_extra != mp_head->v_extra) ||
	   (test.info_num != mp_head->info_num) ||
	   (test.info_len != mp_head->info_len) ||
	   (test.head_size != mp_head->head_size) ||
	   (test.info_size != mp_head->info_size))
	{
	      /* Error */
	      return (-1);
	}


	/* Accept the header */
	(*mp_head) = test;


	/* Allocate the "mp_info" array */
	C_MAKE(mp_info, mp_head->info_num, player_magic);

	/* Read the "mp_info" array */
	fd_read(fd, (char*)(mp_info), mp_head->info_size);

	/* Success */
	return (0);
}



/*
 * Initialize the "mp_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_mp_info(void)
{
	int fd;

	int mode = 0644;

	errr err = 0;

	FILE *fp;

	/* General buffer */
	char buf[1024];

	/*** Make the "header" ***/

	/* Allocate the "header" */
	MAKE(mp_head, header);

	/* Save the "version" */
	mp_head->v_major = O_VERSION_MAJOR;
	mp_head->v_minor = O_VERSION_MINOR;
	mp_head->v_patch = O_VERSION_PATCH;
	mp_head->v_extra = O_VERSION_EXTRA;

	/* Save the "record" information */
	mp_head->info_num = MAX_CP_IDX;
	mp_head->info_len = sizeof(player_magic);

	/* Save the size of "p_head" and "mp_info" */
	mp_head->head_size = sizeof(header);
	mp_head->info_size = mp_head->info_num * mp_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_magic.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
#ifdef CHECK_MODIFICATION_TIME

	      err = check_modification_date(fd, "p_magic.txt");

#endif /* CHECK_MODIFICATION_TIME */

	      /* Attempt to parse the "raw" file */
	      if (!err)
		     err = init_mp_info_raw(fd);

	      /* Close it */
	      fd_close(fd);

	      /* Success */
	      if (!err) return (0);
	}


	/*** Make the fake arrays ***/

	/* Allocate the "mp_info" array */
	C_MAKE(mp_info, mp_head->info_num, player_magic);

	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "p_magic.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'p_magic.txt' file.");

	/* Parse the file */
	err = init_mp_info_txt(fp, buf);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
	      cptr oops;

	      /* Error string */
	      oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

	      /* Oops */
	      msg_format("Error %d at line %d of 'p_magic.txt'.", err, error_line);
	      msg_format("Record %d contains a '%s' error.", error_idx, oops);
	      msg_format("Parsing '%s'.", buf);
	      msg_print(NULL);

	      /* Quit */
	      quit("Error in 'p_magic.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_magic.raw");

	/* Kill the old file */
	fd_kill(buf);

	/* Attempt to create the raw file */
	fd = fd_make(buf, mode);

	/* Dump to the file */
	if (fd >= 0)
	{
	      /* Dump it */
	      fd_write(fd, (char*)(mp_head), mp_head->head_size);

	      /* Dump the "mp_info" array */
	      fd_write(fd, (char*)(mp_info), mp_head->info_size);

	      /* Close */
	      fd_close(fd);
	}


	/*** Kill the fake arrays ***/

	/* Free the "mp_info" array */
	C_KILL(mp_info, mp_head->info_num, player_magic);

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "p_magic.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot open 'p_magic.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_mp_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'p_magic.raw' file.");

	/* Success */
	return (0);
}



/*
 * Initialize the "t_info" array.  Code from "init_v_info". -LM-
 * Unlike other, similar arrays, that for themed levels is not written to a
 * binary file, since themed levels are neither important enough to warrant
 * the use of extra space, nor used often enough to worry excessively about
 * speed.
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
errr init_t_info(byte chosen_level)
{
	errr err;

	FILE *fp;

	/* General buffer */
	char buf[1024];

#ifndef NO_THEMED_LEVELS	/* Themed levels and old machines don't mix. */

	/*** Make the header ***/

	/* Allocate the "header" */
	MAKE(t_head, header);

	/* Save the "version" */
	t_head->v_major = O_VERSION_MAJOR;
	t_head->v_minor = O_VERSION_MINOR;
	t_head->v_patch = O_VERSION_PATCH;
	t_head->v_extra = 0;

	/* Save the "record" information */
	t_head->info_num = MAX_V_IDX;
	t_head->info_len = sizeof(vault_type);

	/* Save the size of "v_head" and "v_info" */
	t_head->head_size = sizeof(header);
	t_head->info_size = t_head->info_num * t_head->info_len;


	/*** Make the fake arrays ***/

	/* Allocate the "t_info" array */
	C_MAKE(t_info, t_head->info_num, vault_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(t_name, FAKE_NAME_SIZE, char);
	C_MAKE(t_text, FAKE_TEXT_SIZE, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "themed.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it, canceling on failure. */
	if (!fp) return(TRUE);

	/* Parse the file */
	err = init_t_info_txt(fp, buf, chosen_level);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < 8)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Non-critical error: themed.txt is unusable.");
		msg_format("Error %d at line %d of 'themed.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Failure */
		return(TRUE);
	}




	/* Success */
	return (0);

#else

	/* No themed levels allowed if compiler option forbids them. */
	return(1);

#endif
}

/*
 * Release memory used to store information about a themed level. -LM-
 */
void kill_t_info(void)
{

	/*** Kill the fake arrays ***/

	/* Free the "t_info" array */
	C_KILL(t_info, t_head->info_num, vault_type);

	/* Hack -- Free the "fake" arrays */
	C_KILL(t_name, FAKE_NAME_SIZE, char);
	C_KILL(t_text, FAKE_TEXT_SIZE, char);
}







/*** Initialize others ***/


/*
 * Hack -- Objects sold in the stores -- by tval/sval pair.
 * Note code for initializing the stores, below.
 */
static byte store_table[MAX_STORES][STORE_CHOICES][2] =
{
	{
		/* General Store. */

		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_BISCUIT },
		{ TV_FOOD, SV_FOOD_BISCUIT },
		{ TV_FOOD, SV_FOOD_JERKY },
		{ TV_FOOD, SV_FOOD_JERKY },

		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },
		{ TV_LITE, SV_LITE_TORCH },
		{ TV_LITE, SV_LITE_TORCH },
		{ TV_LITE, SV_LITE_TORCH },
		{ TV_LITE, SV_LITE_TORCH },
		{ TV_LITE, SV_LITE_LANTERN },
		{ TV_LITE, SV_LITE_LANTERN },

		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },
		{ TV_SPIKE, 0 },
		{ TV_SPIKE, 0 },

		{ TV_SHOT, SV_AMMO_NORMAL },
		{ TV_ARROW, SV_AMMO_NORMAL },
		{ TV_BOLT, SV_AMMO_NORMAL },
		{ TV_DIGGING, SV_SHOVEL },
		{ TV_DIGGING, SV_PICK },
		{ TV_CLOAK, SV_CLOAK },
		{ TV_CLOAK, SV_CLOAK },
		{ TV_CLOAK, SV_CLOAK }
	},

	{
		/* Armoury */

		{ TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS },
		{ TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS },
		{ TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
		{ TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
		{ TV_HELM, SV_HARD_LEATHER_CAP },
		{ TV_HELM, SV_HARD_LEATHER_CAP },
		{ TV_HELM, SV_METAL_CAP },
		{ TV_HELM, SV_IRON_HELM },

		{ TV_SOFT_ARMOR, SV_ROBE },
		{ TV_SOFT_ARMOR, SV_ROBE },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
		{ TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR },
		{ TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR },
		{ TV_SOFT_ARMOR, SV_HARD_STUDDED_LEATHER },
		{ TV_SOFT_ARMOR, SV_HARD_STUDDED_LEATHER },

		{ TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL },
		{ TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL },
		{ TV_HARD_ARMOR, SV_METAL_SCALE_MAIL },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_DOUBLE_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_AUGMENTED_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_BAR_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_DOUBLE_CHAIN_MAIL },

		{ TV_HARD_ARMOR, SV_METAL_BRIGANDINE_ARMOUR },
		{ TV_GLOVES, SV_SET_OF_LEATHER_GLOVES },
		{ TV_GLOVES, SV_SET_OF_LEATHER_GLOVES },
		{ TV_GLOVES, SV_SET_OF_MAIL_GAUNTLETS },
		{ TV_SHIELD, SV_WICKER_SHIELD },
		{ TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
		{ TV_SHIELD, SV_LARGE_LEATHER_SHIELD },
		{ TV_SHIELD, SV_SMALL_METAL_SHIELD }
	},

	{
		/* Weaponsmith */

		{ TV_SWORD, SV_DAGGER },
		{ TV_SWORD, SV_DAGGER },
		{ TV_SWORD, SV_MAIN_GAUCHE },
		{ TV_SWORD, SV_RAPIER },
		{ TV_SWORD, SV_SMALL_SWORD },
		{ TV_SWORD, SV_SHORT_SWORD },
		{ TV_SWORD, SV_SABRE },
		{ TV_SWORD, SV_CUTLASS },

		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_SWORD, SV_LONG_SWORD },
		{ TV_SWORD, SV_SCIMITAR },
		{ TV_SWORD, SV_KATANA },
		{ TV_SWORD, SV_BASTARD_SWORD },
		{ TV_SWORD, SV_TWO_HANDED_SWORD },
		{ TV_POLEARM, SV_SPEAR },
		{ TV_POLEARM, SV_TRIDENT },

		{ TV_POLEARM, SV_PIKE },
		{ TV_POLEARM, SV_BEAKED_AXE },
		{ TV_POLEARM, SV_BROAD_AXE },
		{ TV_POLEARM, SV_DART },
		{ TV_POLEARM, SV_BATTLE_AXE },
		{ TV_BOW, SV_SLING },
		{ TV_BOW, SV_SLING },
		{ TV_BOW, SV_SHORT_BOW },

		{ TV_BOW, SV_LONG_BOW },
		{ TV_BOW, SV_LIGHT_XBOW },
		{ TV_SHOT, SV_AMMO_NORMAL },
		{ TV_SHOT, SV_AMMO_NORMAL },
		{ TV_ARROW, SV_AMMO_NORMAL },
		{ TV_ARROW, SV_AMMO_NORMAL },
		{ TV_BOLT, SV_AMMO_NORMAL },
		{ TV_BOLT, SV_AMMO_NORMAL },
	},

	{
		/* Temple. */

		{ TV_HAFTED, SV_WHIP },
		{ TV_HAFTED, SV_QUARTERSTAFF },
		{ TV_HAFTED, SV_MACE },
		{ TV_HAFTED, SV_MACE },
		{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL },
		{ TV_HAFTED, SV_WAR_HAMMER },
		{ TV_HAFTED, SV_WAR_HAMMER },
		{ TV_HAFTED, SV_MORNING_STAR },

		{ TV_HAFTED, SV_FLAIL },
		{ TV_HAFTED, SV_FLAIL },
		{ TV_HAFTED, SV_LEAD_FILLED_MACE },
		{ TV_SCROLL, SV_SCROLL_REMOVE_CURSE },
		{ TV_SCROLL, SV_SCROLL_BLESSING },
		{ TV_SCROLL, SV_SCROLL_HOLY_CHANT },
		{ TV_POTION, SV_POTION_BOLDNESS },
		{ TV_POTION, SV_POTION_HEROISM },

		{ TV_POTION, SV_POTION_CURE_LIGHT },
		{ TV_SCROLL, SV_SCROLL_RECHARGING },
		{ TV_POTION, SV_POTION_RESIST_ACID_ELEC },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },
		{ TV_POTION, SV_POTION_CURE_POISON },
		{ TV_POTION, SV_POTION_SLOW_POISON },
		{ TV_POTION, SV_POTION_RESIST_HEAT_COLD },
		{ TV_POTION, SV_POTION_RESTORE_EXP },

		{ TV_POTION, SV_POTION_CURE_LIGHT },
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP }
	},

	{
		/* Alchemy shop.  All the general-purpose scrolls and potions. */

		{ TV_SCROLL, SV_SCROLL_ENCHANT_WEAPON_TO_HIT },
		{ TV_SCROLL, SV_SCROLL_ENCHANT_WEAPON_TO_DAM },
		{ TV_SCROLL, SV_SCROLL_ENCHANT_ARMOR },
		{ TV_SCROLL, SV_SCROLL_TELEPORT },
		{ TV_SCROLL, SV_SCROLL_TELEPORT_LEVEL },
		{ TV_SCROLL, SV_SCROLL_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_LIGHT },

		{ TV_SCROLL, SV_SCROLL_PHASE_DOOR },
		{ TV_SCROLL, SV_SCROLL_PHASE_DOOR },
		{ TV_SCROLL, SV_SCROLL_PHASE_DOOR },
		{ TV_SCROLL, SV_SCROLL_MONSTER_CONFUSION },
		{ TV_SCROLL, SV_SCROLL_MAPPING },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_DETECT_TRAP },

		{ TV_SCROLL, SV_SCROLL_DETECT_DOOR },
		{ TV_SCROLL, SV_SCROLL_DETECT_INVIS },
		{ TV_SCROLL, SV_SCROLL_RECHARGING },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },

		{ TV_SCROLL, SV_SCROLL_DISPEL_UNDEAD },
		{ TV_POTION, SV_POTION_HEROISM },
		{ TV_POTION, SV_POTION_RES_STR },
		{ TV_POTION, SV_POTION_RES_INT },
		{ TV_POTION, SV_POTION_RES_WIS },
		{ TV_POTION, SV_POTION_RES_DEX },
		{ TV_POTION, SV_POTION_RES_CON },
		{ TV_POTION, SV_POTION_RES_CHR }
	},

	{
		/* Magic-User store. */

		{ TV_RING, SV_RING_SEARCHING },
		{ TV_RING, SV_RING_FEATHER_FALL },
		{ TV_RING, SV_RING_PROTECTION },
		{ TV_AMULET, SV_AMULET_CHARISMA },
		{ TV_AMULET, SV_AMULET_SLOW_DIGEST },
		{ TV_AMULET, SV_AMULET_RESIST_ACID },
		{ TV_WAND, SV_WAND_SLOW_MONSTER },
		{ TV_WAND, SV_WAND_CONFUSE_MONSTER },

		{ TV_WAND, SV_WAND_SLEEP_MONSTER },
		{ TV_WAND, SV_WAND_MAGIC_MISSILE },
		{ TV_WAND, SV_WAND_STINKING_CLOUD },
		{ TV_WAND, SV_RING_RESIST_COLD },
		{ TV_STAFF, SV_RING_RESIST_FIRE },
		{ TV_STAFF, SV_RING_SLOW_DIGESTION },
		{ TV_STAFF, SV_STAFF_DETECT_TRAP },
		{ TV_STAFF, SV_STAFF_DETECT_DOOR },

		{ TV_STAFF, SV_STAFF_DETECT_GOLD },
		{ TV_STAFF, SV_STAFF_DETECT_ITEM },
		{ TV_STAFF, SV_STAFF_DETECT_INVIS },
		{ TV_STAFF, SV_STAFF_DETECT_EVIL },
		{ TV_STAFF, SV_STAFF_TELEPORTATION },
		{ TV_STAFF, SV_STAFF_TELEPORTATION },
		{ TV_STAFF, SV_STAFF_IDENTIFY },
		{ TV_STAFF, SV_STAFF_IDENTIFY },

		{ TV_WAND, SV_WAND_DOOR_DEST },
		{ TV_WAND, SV_WAND_STONE_TO_MUD },
		{ TV_WAND, SV_WAND_STINKING_CLOUD },
		{ TV_WAND, SV_WAND_POLYMORPH },
		{ TV_STAFF, SV_STAFF_LITE },
		{ TV_STAFF, SV_STAFF_MAPPING },
		{ TV_STAFF, SV_ROD_DETECT_TRAP },
		{ TV_STAFF, SV_ROD_DETECT_DOOR }
	},

	{
		/* Black Market (unused) */
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 }
	},

	{
		/* Home (unused) */
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 },
		{ 0, 0 }
	},

	{
		/* Bookseller. */

		{ TV_MAGIC_BOOK, 0 },
		{ TV_MAGIC_BOOK, 0 },
		{ TV_MAGIC_BOOK, 0 },
		{ TV_MAGIC_BOOK, 1 },
		{ TV_MAGIC_BOOK, 1 },
		{ TV_MAGIC_BOOK, 2 },
		{ TV_MAGIC_BOOK, 2 },
		{ TV_MAGIC_BOOK, 3 },

		{ TV_PRAYER_BOOK, 0 },
		{ TV_PRAYER_BOOK, 0 },
		{ TV_PRAYER_BOOK, 0 },
		{ TV_PRAYER_BOOK, 1 },
		{ TV_PRAYER_BOOK, 1 },
		{ TV_PRAYER_BOOK, 2 },
		{ TV_PRAYER_BOOK, 2 },
		{ TV_PRAYER_BOOK, 3 },

		{ TV_DRUID_BOOK, 0 },
		{ TV_DRUID_BOOK, 0 },
		{ TV_DRUID_BOOK, 0 },
		{ TV_DRUID_BOOK, 1 },
		{ TV_DRUID_BOOK, 1 },
		{ TV_DRUID_BOOK, 2 },
		{ TV_DRUID_BOOK, 2 },
		{ TV_DRUID_BOOK, 3 },

		{ TV_NECRO_BOOK, 0 },
		{ TV_NECRO_BOOK, 0 },
		{ TV_NECRO_BOOK, 0 },
		{ TV_NECRO_BOOK, 1 },
		{ TV_NECRO_BOOK, 1 },
		{ TV_NECRO_BOOK, 2 },
		{ TV_NECRO_BOOK, 2 },
		{ TV_NECRO_BOOK, 3 }
	}
};



/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
	int i, k, n;


	/*** Prepare the various "bizarre" arrays ***/

	/* Initialize the "macro" package */
	(void)macro_init();

	/* Initialize the "quark" package */
	(void)quark_init();

	/* Initialize the "message" package */
	(void)message_init();


	/*** Prepare grid arrays ***/

	/* Array of grids */
	C_MAKE(view_g, VIEW_MAX, u16b);

	/* Array of grids */
	C_MAKE(temp_g, TEMP_MAX, u16b);

	/* Hack -- use some memory twice */
	temp_y = ((byte*)(temp_g)) + 0;
	temp_x = ((byte*)(temp_g)) + TEMP_MAX;


	/*** Prepare dungeon arrays ***/

	/* Padded into array */
	C_MAKE(cave_info, DUNGEON_HGT, byte_256);

	/* Feature array */
	C_MAKE(cave_feat, DUNGEON_HGT, byte_wid);

	/* Entity arrays */
	C_MAKE(cave_o_idx, DUNGEON_HGT, s16b_wid);
	C_MAKE(cave_m_idx, DUNGEON_HGT, s16b_wid);

	/* Lore */
	C_MAKE(l_list, MAX_R_IDX, monster_lore);

	/* Flow arrays */
	C_MAKE(cave_cost, DUNGEON_HGT, byte_wid);
	C_MAKE(cave_when, DUNGEON_HGT, byte_wid);


	/*** Prepare "vinfo" array ***/

	/* Used by "update_view()" */
	(void)vinfo_init();


	/*** Prepare entity arrays ***/

	/* Objects */
	C_MAKE(o_list, MAX_O_IDX, object_type);

	/* Monsters */
	C_MAKE(m_list, MAX_M_IDX, monster_type);

	/*** Prepare quest array ***/

	/* Quests */
	C_MAKE(q_list, MAX_Q_IDX, quest);


	/*** Prepare the inventory ***/

	/* Allocate it */
	C_MAKE(inventory, INVEN_TOTAL, object_type);


	/*** Prepare the stores ***/

	/* Allocate the stores */
	C_MAKE(store, MAX_STORES, store_type);

	/* Fill in each store */
	for (i = 0; i < MAX_STORES; i++)
	{
		/* Access the store */
		store_type *st_ptr = &store[i];

		/* Assume full stock */
		st_ptr->stock_size = STORE_INVEN_MAX;

		/* Allocate the stock */
		C_MAKE(st_ptr->stock, st_ptr->stock_size, object_type);

		/* No table for the black market or home */
		if ((i == 6) || (i == 7)) continue;

		/* Assume full table */
		st_ptr->table_size = STORE_CHOICES;

		/* Allocate the stock */
		C_MAKE(st_ptr->table, st_ptr->table_size, s16b);

		/* Scan the choices */
		for (k = 0; k < STORE_CHOICES; k++)
		{
			int k_idx;

			/* Extract the tval/sval codes */
			int tv = store_table[i][k][0];
			int sv = store_table[i][k][1];

			/* Look for it */
			for (k_idx = 1; k_idx < MAX_K_IDX; k_idx++)
			{
				object_kind *k_ptr = &k_info[k_idx];

				/* Found a match */
				if ((k_ptr->tval == tv) && (k_ptr->sval == sv)) break;
			}

			/* Catch errors */
			if (k_idx == MAX_K_IDX) continue;

			/* Add that item index to the table */
			st_ptr->table[st_ptr->table_num++] = k_idx;
		}
	}


	/*** Pre-allocate the basic "auto-inscriptions" ***/

	/* Some extra strings */
	(void)quark_add("on sale");


	/*** Prepare the options ***/

	/* Initialize the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		/* Default value */
		op_ptr->opt[i] = option_norm[i];
	}

	/* Initialize the window flags */
	for (n = 0; n < 8; n++)
	{
		/* Assume no flags */
		op_ptr->window_flag[n] = 0L;
	}

	/* Initialize the delay_factor */
	op_ptr->delay_factor = 1;

	/*** Pre-allocate space for the "format()" buffer ***/

	/* Hack -- Just call the "format()" function */
	(void)format("%s (%s).", "Bahman Rabii", MAINTAINER);


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

	monster_race *r_ptr;

	ego_item_type *e_ptr;

	alloc_entry *table;

	s16b num[MAX_DEPTH];

	s16b aux[MAX_DEPTH];


	/*** Analyze object allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(&num, MAX_DEPTH, s16b);

	/* Size of "alloc_kind_table" */
	alloc_kind_size = 0;

	/* Scan the objects */
	for (i = 1; i < MAX_K_IDX; i++)
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

	/* Access the table entry */
	table = alloc_kind_table;

	/* Scan the objects */
	for (i = 1; i < MAX_K_IDX; i++)
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


	/*** Analyze monster allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(&num, MAX_DEPTH, s16b);

	/* Size of "alloc_race_table" */
	alloc_race_size = 0;

	/* Scan the monsters */
	for (i = 1; i < MAX_R_IDX; i++)
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

	/* Access the table entry */
	table = alloc_race_table;

	/* Scan the monsters */
	for (i = 1; i < MAX_R_IDX; i++)
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
	for (i = 1; i < MAX_E_IDX; i++)
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
	for (i = 1; i < MAX_E_IDX; i++)
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
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
	Term_erase(0, 23, 255);
	Term_putstr(20, 23, -1, TERM_WHITE, str);
	Term_fresh();
}

/*
 * Hack -- Explain a broken "lib" folder and quit (see below).
 *
 * XXX XXX XXX This function is "messy" because various things
 * may or may not be initialized, but the "plog()" and "quit()"
 * functions are "supposed" to work under any conditions.
 */
static void init_angband_aux(cptr why)
{
	/* Why */
	plog(why);

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
 * Hack - identify set item artifacts.
 *
 * Go through the list of Set Items and identify all artifacts in each set
 * as belonging to that set. By GS
 */
void update_artifact_sets()
{
 	byte i;
	byte j;
	set_type *s_ptr;
	set_element *se_ptr;
	artifact_type *a_ptr;

	for (i=0;i<MAX_S_IDX;i++)
	{

		s_ptr = &s_info[i];
		for (j=0;j<s_ptr->no_of_items;j++)
		{
			se_ptr= &s_ptr->set_items[j];
			a_ptr = &a_info[se_ptr->a_idx];
			a_ptr->set_no = i;
		}
	}
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
	path_build(buf, 1024, ANGBAND_DIR_FILE, "news.txt");

	/* Attempt to open the file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure */
	if (fd < 0)
	{
		char why[1024];

		/* Message */
		sprintf(why, "Cannot access the '%s' file!", buf);

		/* Crash and burn */
		init_angband_aux(why);
	}

	/* Close it */
	fd_close(fd);


	/*** Display the "news" file ***/

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, "news.txt");

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
			Term_putstr(0, i++, -1, TERM_WHITE, buf);
		}

		/* Close */
		my_fclose(fp);
	}

	/* Flush it */
	Term_fresh();


	/*** Verify (or create) the "high score" file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_APEX, "scores.raw");

	/* Attempt to open the high score file */
	fd = fd_open(buf, O_RDONLY);

	/* Failure */
	if (fd < 0)
	{
		/* File type is "DATA" */
		FILE_TYPE(FILE_TYPE_DATA);

		/* Create a new high score file */
		fd = fd_make(buf, mode);

		/* Failure */
		if (fd < 0)
		{
			char why[1024];

			/* Message */
			sprintf(why, "Cannot create the '%s' file!", buf);

			/* Crash and burn */
			init_angband_aux(why);
		}
	}

	/* Close it */
	fd_close(fd);


	/*** Initialize some arrays ***/

	/* Initialize feature info */
	note("[Initializing arrays... (features)]");
	if (init_f_info()) quit("Cannot initialize features");

	/* Initialize object info */
	note("[Initializing arrays... (objects)]");
	if (init_k_info()) quit("Cannot initialize objects");

	/* Initialize artifact info */
	note("[Initializing arrays... (artifacts)]");
	if (init_a_info()) quit("Cannot initialize artifacts");

	/* Initialize set item info */
	note("[Initializing arrays... (set items)]");
	if (init_s_info()) quit("Cannot initialize set items");
	update_artifact_sets();

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
	if (init_rp_info()) quit("Cannot initialize races");

	/* Initialize class info */
	note("[Initializing arrays... (classes)]");
	if (init_cp_info()) quit("Cannot initialize classes");

	/* Initialize chest info */
	note("[Initializing arrays... (chests)]");
	if (init_ch_info()) quit("Cannot initialize chests");

	/* Initialize magic info */
	note("[Initializing arrays... (magic)]");
	if (init_mp_info()) quit("Cannot initialize magic");

	/* Initialize price info */
	note("[Initializing arrays... (prices)]");
	if (init_g_info()) quit("Cannot initialize prices");

	/* Initialize owner info */
	note("[Initializing arrays... (owners)]");
	if (init_b_info()) quit("Cannot initialize owners");

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

	/* Done */
	note("[Initialization complete]");
}



